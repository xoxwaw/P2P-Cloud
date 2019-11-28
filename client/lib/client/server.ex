defmodule Client.Server do
  require Logger
  use GenServer

  @ip {127, 0, 0, 1}
  @port 5555

  def start_link() do
    GenServer.start_link(__MODULE__, %{socket: nil})
  end

  def init(state) do
    send(self(), :connect)
    {:ok, state}
  end

  def handle_info(:connect, state) do
    Logger.info "Connecting to #{:inet.ntoa(@ip)}:#{@port}"

    case :gen_tcp.connect(@ip, @port, [active: true]) do
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

  def handle_info({:request, {:upload, path}}, %{socket: socket} = state) do
    File.stream!(path, [], 1028)
    |> Enum.each(fn chunk ->
      :gen_tcp.send(socket, chunk)
    end)

    Logger.info("SENT")
    {:noreply, state}
  end

  def handle_cast({:request, {:store_file, path}}, %{socket: socket} = state) do
    filename =
      path
      |> Path.split
      |> Enum.at(-1)
    :gen_tcp.send(socket, "PUT #{filename}")
    send(self(), {:request, {:upload, path}})
    {:noreply, state}
  end

  def handle_cast({:request, {:get_file, filename}}, %{socket: socket} = state) do
    data = Poison.encode!(%{
      "method" => "GET",
      "filename" => filename}
    )
    :ok = :gen_tcp.send(socket, data)

    {:noreply, state}
  end

  def handle_cast({:message, message}, %{socket: socket} = state) do
    Logger.info "Sending #{message}"

    :ok = :gen_tcp.send(socket, message)
    {:noreply, state}
  end


  def disconnect(state, reason) do
    Logger.info "Disconnected: #{reason}"
    {:stop, :normal, state}
  end
end
