defmodule Swarm.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec
    children = [
      worker(Swarm,[]),
    ]
    opts = [strategy: :one_for_one, name: Swarm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
