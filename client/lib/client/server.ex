defmodule Client.Server do
  require Logger
  use GenServer

  @ip {127, 0, 0, 1}

  def start_link(state) do
    GenServer.start_link(__MODULE__, state)
  end

  def init(state) do
    send(self(), :connect)
    {:ok, state}
  end

  def handle_info(:connect, state=%{socket: sock, port: port}) do
    Logger.info "Connecting to #{:inet.ntoa(@ip)}:#{port}"

    case :gen_tcp.connect(@ip, port, [:binary, active: true]) do
      {:ok, socket} ->
        {:noreply, %{state | socket: socket}}
      {:error, reason} ->
        disconnect(state, reason)
    end
  end

  def handle_info({:tcp, _, data}, state) do
    case String.split(data) do
      ["PUT", file_size, filename] ->
        send(self(), {:response, {:metadata, file_size, filename}})
      _ ->
        send(self(), {:response, {:download, data}})
    end
    {:noreply, state}
  end

  def handle_info({:tcp_closed, _}, state), do: {:stop, :normal, state}
  def handle_info({:tcp_error, _}, state), do: {:stop, :normal, state}

  def handle_info({:request, {:upload, path}}, %{socket: socket, port: port} = state) do
    File.stream!(path, [], 2048)
    |> Enum.each(fn chunk ->
      :gen_tcp.send(socket, chunk)
    end)
    {:noreply, state}
  end

  def handle_cast({:request, {:store_file, path}}, %{socket: socket, port: port} = state) do
    filename =
      path
      |> Path.split
      |> Enum.at(-1)
    {tmp, _} = System.cmd("wc", ["-c", path])
    file_size = String.split(tmp) |> Enum.at(0)
    :gen_tcp.send(socket, "PUT #{file_size} #{filename}")
    send(self(), {:request, {:upload, path}})
    {:noreply, state}
  end

  def handle_cast({:request, {:get_file, filename}}, %{socket: socket} = state) do
    :gen_tcp.send(socket, "GET #{filename}")
    {:noreply, state}
  end

  def handle_info({:response, {:metadata, file_size, filename}}, state) do
    {
      :noreply, %{
        state | filename: filename,
        total_size: String.to_integer(file_size)}
    }
  end

  def handle_info({:response, {:download, data}}, state= %{
    socket: socket, filename: filename,
    check_sum: check_sum, total_size: total_size}) do
      Logger.info("Transfering... #{check_sum}/#{total_size}")
      File.write(filename, data, [:append])
      if byte_size(data) + check_sum == total_size do
        :gen_tcp.close(socket)
      end
      {:noreply, %{state | check_sum: check_sum + byte_size(data)}}
  end

  def handle_cast({:message, message}, %{socket: socket, port: port} = state) do
    Logger.info "Sending #{message}"

    :ok = :gen_tcp.send(socket, message)
    {:noreply, state}
  end


  def disconnect(state, reason) do
    Logger.info "Disconnected: #{reason}"
    {:stop, :normal, state}
  end
end
