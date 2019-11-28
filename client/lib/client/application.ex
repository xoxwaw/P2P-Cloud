defmodule Client.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      worker(Client.Server, [])
    ]

    opts = [strategy: :simple_one_for_one]
    Supervisor.start_link(children, opts)
  end
end
