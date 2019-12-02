defmodule Swarm.Server do
  use GenServer
  alias Swarm.{Node, AVL}

  def start_link() do
    {:ok, pid} = GenServer.start_link(__MODULE__, %Node{})
    pid
  end

  def init(state) do
    {:ok, state}
  end

  def handle_cast({:insert, node}, state) do
    new_state = AVL.insert(state, node)
    {:ok, new_state}
  end

  def handle_call({:all_nodes}, _from, state) do
    lst = AVL.to_list([state], [])
    {:ok, lst}
  end

end
