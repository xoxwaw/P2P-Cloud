defmodule Server.GenProtocol do
  use GenServer
  require Poison
  require Logger

  @behaviour :ranch_protocol

  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(ref, socket, transport) do
    IO.puts "Starting protocol"

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [{:active, true}])
    :gen_server.enter_loop(__MODULE__, [], %{
      socket: socket,
      transport: transport,
      filename: "output.txt",
      check_sum: 0,
      total_size: 0,
    })
  end


  def handle_info({:tcp, socket, data}, state) do
    case String.split(data) do
      ["PUT", file_size, filename] ->
        send(self(), {:store_file, file_size, filename})
      ["GET", filename] ->
        send(self(), {:get_file, filename})
      _ ->
        send(self(), {:download, data})
    end
    {:noreply, state}
  end

  def handle_info({:store_file, file_size, filename}, state) do
    {:noreply, %{state | filename:  filename, total_size: String.to_integer(file_size)}}
  end

  def handle_info({:download, data}, state= %{
    socket: socket, transport: transport, filename: filename,
    check_sum: check_sum, total_size: total_size}) do
      File.write(filename, data, [:append])
      if byte_size(data) + check_sum == total_size do
        transport.close(socket)
      end
      {:noreply, %{state | check_sum: check_sum + byte_size(data)}}
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



  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    IO.puts "Closing"
    transport.close(socket)
    {:stop, :normal, state}
  end

end
