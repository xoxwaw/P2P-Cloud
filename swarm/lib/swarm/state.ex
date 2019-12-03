defmodule Swarm.State do
  use GenServer
  alias Swarm.{Node, AVL}

  def start_link do
    {:ok, pid} = GenServer.start_link(__MODULE__, nil)
    pid
  end

  def handle_cast({:insert, node}, state) do
    new_state = AVL.insert(state, node)
    {:noreply, new_state}
  end

  def handle_call({:nodes}, _from, state) do
    lst = AVL.to_list([state])
    res =
      case lst == [:nil] do
        true ->
          []
        false ->
          lst
      end
    {:reply, lst, state}
  end
end
