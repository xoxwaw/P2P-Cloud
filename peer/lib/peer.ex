defmodule Peer do
  alias Peer.Server
  def start do
    {:ok,pid} = Server.start_link
    join(pid, 8080)
    pid
  end

  def store_file(pid, port, path) do
    GenServer.cast(pid, {:store_file, port, path})
  end

  def get_file(pid, port, filename) do
    GenServer.cast(pid, {:get_file, port, filename})
  end

  def ping(pid, port) do
    GenServer.cast(pid, {:ping, port})
  end

  def peers(pid) do
    GenServer.call(pid, {:list_peers})
  end

  def join(pid, port) do
    connect(pid, port)
    GenServer.cast(pid, {:join, port})
  end

  def connect(pid, port) do
    GenServer.cast(pid, {:connect, port})
  end
end
