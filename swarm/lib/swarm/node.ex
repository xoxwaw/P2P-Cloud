defmodule Swarm.Node do
  defstruct [:address, :left, :right, :height]

  @type t :: %__MODULE__{
    address: charlist(),
    left: Swarm.Node.t(),
    right: Swarm.Node.t(),
    height: integer(),
  }
end
