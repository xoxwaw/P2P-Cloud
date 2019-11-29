defmodule PeerTest do
  use ExUnit.Case
  doctest Peer

  test "greets the world" do
    assert Peer.hello() == :world
  end
end
