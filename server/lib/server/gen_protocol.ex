defmodule Server.GenProtocol do
  use GenServer
  require Poison
  require Logger

  @behaviour :ranch_protocol
  @root "../../assets/" <> System.get_env("PORT") <> "/"

  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  def init(ref, socket, transport) do
    IO.puts "Starting protocol"

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, [:binary, {:active, true}])
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
    {:noreply, %{state | filename: @root <> filename, total_size: String.to_integer(file_size)}}
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

  def handle_info({:get_file, filename}, state = %{socket: socket, transport: transport}) do
    path = @root <> filename |> Path.expand(__DIR__)
    {tmp, _} = System.cmd("wc", ["-c", path])
    file_size = String.split(tmp) |> Enum.at(0)
    transport.send(socket, "PUT #{file_size} #{filename}")
    send(self(), {:transfer, path})
    {:noreply, %{state | filename: filename }}
  end

  def handle_info({:transfer, filename}, state= %{socket: socket, transport: transport}) do
    File.stream!(filename, [], 1024)
    |> Enum.each(fn chunk ->
      transport.send(socket, chunk)
    end)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state = %{socket: socket, transport: transport}) do
    IO.puts "Closing"
    transport.close(socket)
    {:stop, :normal, state}
  end

end
