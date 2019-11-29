defmodule Server do
  require Logger

  def start_link() do
    port = String.to_integer( System.get_env("PORT"))
    Logger.info("LISTEN TO PORT #{port}")
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], Server.GenProtocol, [])
  end
end
