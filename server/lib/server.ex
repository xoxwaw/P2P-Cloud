defmodule Server do
  def start_link() do
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, 5555}], Server.GenProtocol, [])
  end
end
