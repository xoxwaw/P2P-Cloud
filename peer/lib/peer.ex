defmodule Peer do
  alias Peer.Server
  @root "../assets/" |> Path.expand(__DIR__)

  def start do
    {:ok,pid} = Server.start_link
    join(pid, 8080)
    pid
  end

  def store_file(pid, filename) do
    port = System.get_env("PORT")
    case File.ls!(@root) |> Enum.member?(port) do
      false ->
        System.cmd("mkdir", [@root <> "/#{port}"])
      true ->
        IO.inspect("EXISTS")
    end
    path = @root <> "/#{port}/#{filename}"
    GenServer.cast(pid, {:store_file, path})
  end

  def get_file(pid, filename) do
    GenServer.cast(pid, {:get_file, filename})
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
