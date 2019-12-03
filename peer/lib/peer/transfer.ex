defmodule Peer.Transfer do
  def store(path, peers) do
    IO.inspect(peers) 
    split(path, peers)
    prefix = get_prefix(path)
    chunks = prefix |> File.ls!
    path_to_chunks = Enum.map(chunks, fn chunk -> prefix <> chunk end)
    send_to_peers(path_to_chunks, peers)
  end

  def send_to_peers([], []), do: []
  def send_to_peers([chunk | t], [peer | p]) do
    pid = Client.start(peer |> String.to_integer )
    Client.store_file(pid, chunk)
    send_to_peers(t, p)
  end

  def split(path, peers) do
    len = length(peers)
    {res, _} = System.cmd("wc", ["-l", path])
    lines =  String.split(res) |> Enum.at(0) |> String.to_integer
    chunk_size = div(lines, len)
    filename = get_file_name(path)
    System.cmd("mkdir", [get_prefix(path)])
    System.cmd("gsplit", ["-l", "#{chunk_size}", path, get_prefix(path)])
  end

  def get_file_name(path) do
    path
    |> Path.split
    |> Enum.at(-1)
  end

  def get_prefix(path) do
    get_enclosing_folder(path) <> "/_#{get_file_name(path)}/"
  end

  def get_enclosing_folder(path) do
    path
    |> Path.split
    |> Enum.drop(-1)
    |> Path.join
  end
end
