defmodule Swarm.Server do
  use GenServer
  alias Swarm.{Node, AVL, State}

  def start_link(ref, socket, transport, opts) do
    sid = Keyword.fetch!(opts, :sid)
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport, sid])
    {:ok, pid}
  end

  def init(ref, socket, transport, sid) do
    IO.puts "Starting connection"
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [:binary, {:active, true}])
    :gen_server.enter_loop(__MODULE__, [], %{
      socket: socket,
      transport: transport,
      nodes: nil,
      state_id: sid,
    })
  end

  def handle_info({:tcp, socket, data}, state) do
    case String.split(data) do
      ["JOIN", address] ->
        send(self(), {:insert, address})
      ["PING"] ->
        send(self(), {:nodes})
      _ ->
        IO.inspect(data)
    end
    {:noreply, state}
  end

  def handle_info({:insert, node}, state=%{state_id: sid}) do
    GenServer.cast(sid, {:insert, node})
    {:noreply, state}
  end

  def handle_info({:nodes}, state=%{socket: socket, transport: transport, state_id: sid}) do
    lst = GenServer.call(sid, {:nodes})
    nodes = Enum.join(lst, " ")
    transport.send(socket, "PEERS #{nodes}")
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    IO.puts "Closing"
    transport.close(socket)
    {:stop, :normal, state}
  end

end
