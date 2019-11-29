defmodule Peer.Server do
  def start_link do
    GenServer.start_link(__MODULE__, %{})
  end

  def init(state) do
    {:ok, state}
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
