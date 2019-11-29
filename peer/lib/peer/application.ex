defmodule Peer.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  import Supervisor.Spec
  
  def start(_type, _args) do
    children = [
      worker(Peer.Server, [])
    ]
    opts = [strategy: :simple_one_for_one, name: Peer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
