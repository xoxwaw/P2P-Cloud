defmodule Server.GenProtocol do
  use GenServer
  require Poison

  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(ref, socket, transport) do
    IO.puts "Starting protocol"

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, true}])
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport, filename: "output.txt"})
  end


  def handle_info({:tcp, socket, data}, state = %{socket: socket, transport: transport, filename: filename}) do
    case String.split(data) do
      ["PUT", filename] ->
        send(self(), {:store_file, filename})
      ["GET", filename] ->
        send(self(), {:get_file, filename})
      _ ->
        send(self(), {:download, data})
    end
    {:noreply, state}
  end

  def handle_info({:download, data}, state= %{socket: socket, transport: transport, filename: filename}) do
    File.write(filename, data, [:append])
    {:noreply, state}
  end

  def handle_info({:get_file, filename}, state = %{socket: socket, transport: transport, filename: filename}) do
    content =
      "../../assets/" <> filename
      |> Path.expand(__DIR__)
      |> File.read!
    data =
      Poison.encode!(%{
        "status" => "success",
        "filename" => filename,

        "content" => content,
      })
    transport.send(socket, data)
    {:noreply, state}
  end

  def handle_info({:store_file, filename}, state = %{socket: socket, transport: transport, filename: file}) do
    {:noreply, %{socket: socket, transport: transport, filename: filename}}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport, filename: filename}) do
    IO.puts "Closing"
    transport.close(socket)
    {:stop, :normal, state}
  end

end
