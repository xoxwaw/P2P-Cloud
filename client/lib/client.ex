defmodule Client do
  alias Client.Server

  def start(port) do
    {:ok, pid} = Server.start_link(%{
      socket: nil,
      port: port,
      check_sum: 0,
      filename: "",
      total_size: 0,
    })
    pid
  end

  def store_file(pid, path) do
    GenServer.cast(pid, {:request, {:store_file, path}})
  end

  def get_file(pid, filename) do
    GenServer.cast(pid, {:request, {:get_file, filename}})
  end

end
