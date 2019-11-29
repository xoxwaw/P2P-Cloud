defmodule Peer do
  alias Peer.Server
  def start do
    {:ok,pid} = Server.start_link
    pid
  end

  def store_file(pid, port, path) do
    GenServer.cast(pid, {:store_file, port, path})
  end

  def get_file(pid, port, filename) do
    GenServer.cast(pid, {:get_file, port, filename})
  end
end
