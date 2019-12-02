defmodule SwarmTest do
  use ExUnit.Case
  doctest Swarm

  test "greets the world" do
    assert Swarm.hello() == :world
  end
end
