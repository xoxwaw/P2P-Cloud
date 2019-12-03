defmodule Peer.Server do
  require Logger
  @ip {127,0,0,1}

  def start_link do
    GenServer.start_link(__MODULE__, %{socket: nil, list_peers: []})
  end

  def init(state) do
    {:ok, state}
  end

  def handle_cast({:connect, port}, state) do
    Logger.info "Connecting to #{:inet.ntoa(@ip)}:#{port}"
    case :gen_tcp.connect(@ip, port, [:binary, active: true]) do
      {:ok, socket} ->
        {:noreply, %{state | socket: socket}}
      {:error, reason} ->
        disconnect(state, reason)
    end
  end

  def handle_cast({:ping, port},  state=%{socket: socket}) do
    :gen_tcp.send(socket, "PING")
    {:noreply, state}
  end

  def handle_cast({:join, port},  state=%{socket: socket}) do
    my_port = System.get_env("PORT")
    :gen_tcp.send(socket, "JOIN #{my_port}")
    {:noreply, state}
  end

  def handle_info({:tcp, _, data}, state=%{socket: socket}) do
    case String.split(data) do
      ["PEERS" | lst] ->
        IO.inspect(lst)
        {:noreply, %{state | list_peers: lst}}
      _ ->
        {:noreply, state}
    end

  end

  def handle_call({:list_peers}, _from, state=%{list_peers: list_peers}) do
    {:reply, list_peers, state}
  end

  def disconnect(state, reason) do
    Logger.info "Disconnected: #{reason}"
    {:stop, :normal, state}
  end

  def handle_cast({:store_file, port, path}, state) do
    pid = Client.start(port)
    Client.store_file(pid, path)
    {:noreply, state}
  end

  def handle_cast({:get_file, port, path}, state) do
    pid = Client.start(port)
    Client.get_file(pid, path)
    {:noreply, state}
  end

end
