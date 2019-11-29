defmodule Peer.MixProject do
  use Mix.Project

  def project do
    [
      app: :peer,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      applications: [:server],
      included_applications: [:client],
      extra_applications: [:logger],
      mod: {Peer.Application, []},
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:client, [ path: "../client" ]},
      {:server, [ path: "../server" ]},
    ]
  end
end
