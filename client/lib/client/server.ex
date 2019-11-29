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

    case :gen_tcp.connect(@ip, port, [active: true]) do
      {:ok, socket} ->
        {:noreply, %{state | socket: socket}}
      {:error, reason} ->
        disconnect(state, reason)
    end
  end

  def handle_info({:tcp, _, data}, state) do
    Logger.info "Received #{data}"

    {:noreply, state}
  end

  def handle_info({:tcp_closed, _}, state), do: {:stop, :normal, state}
  def handle_info({:tcp_error, _}, state), do: {:stop, :normal, state}

  def handle_info({:request, {:upload, path}}, %{socket: socket, port: port} = state) do
    File.stream!(path, [], 1028)
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

  def handle_cast({:request, {:get_file, filename}}, %{socket: socket, port: port} = state) do
    data = Poison.encode!(%{
      "method" => "GET",
      "filename" => filename}
    )
    :ok = :gen_tcp.send(socket, data)

    {:noreply, state}
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
