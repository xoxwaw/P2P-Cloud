defmodule Swarm do
  alias Swarm.Server
  def start do
    {:ok, pid} = Server.start_link
    pid
  end

  def add_node(pid, node) do
    GenServer.cast(pid, {:insert, node})
  end

  def remove_node(pid, node) do
    GenServer.cast(pid, {:remove, node})
  end

  def get_all(pid) do
    GenServer.cast(pid, {:all_nodes})
  end
end
