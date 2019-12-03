defmodule Swarm do
  alias Swarm.{Server, State}
  require Logger

  def start_link() do
    port = String.to_integer( System.get_env("PORT"))
    Logger.info("LISTEN TO PORT #{port}")
    sid = State.start_link
    :ranch.start_listener(make_ref(), :ranch_tcp, [{:port, port}], Server, [{:sid, sid}])
  end

end
