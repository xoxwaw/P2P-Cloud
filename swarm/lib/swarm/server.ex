defmodule Swarm.Server do
  use GenServer
  alias Swarm.{Node, AVL}

  def start_link() do
    GenServer.start_link(__MODULE__, %Node{})
  end

  def init(state) do
    {:ok, state}
  end

  def handle_cast({:insert, node}, state) do
    new_state = AVL.insert(state, node)
    new_state
  end

end
