defmodule Client do
  alias Client.Server

  def start(port) do
    {:ok, pid} = Server.start_link(%{socket: nil, port: port})
    pid
  end

  def store_file(pid, filename) do
    path =
      "../assets/" <> filename
      |> Path.expand(__DIR__)
    GenServer.cast(pid, {:request, {:store_file, path}})
  end

  def get_file(pid, filename) do
    GenServer.cast(pid, {:request, {:get_file, filename}})
  end



end
