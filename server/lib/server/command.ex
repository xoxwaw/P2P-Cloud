defmodule Server.Command do
  require Logger
  def parse(data) do
    case Poison.decode!(data) do
      %{"method" => "PUT", "filename" => filename, "content" => content} ->
        {:ok, {:put, filename, content}}
      %{"method" => "GET", "filename" => filename} ->
        {:ok, {:get, filename}}
      %{"method" => "DELETE", "filename" => filename} ->
        {:ok, {:delete, filename}}
    end
  end

  def run({:put, filename, content}) do
    path =
      "../../assets/" <> filename
      |> Path.expand(__DIR__)
    File.write(path, content)
    {:ok, "OK\r\n"}
  end

  def run({:get, filename}) do
    content =
      "../../assets/" <> filename
      |> File.read!
    {:ok, Poison.encode!(%{"filename" => filename, "content" => content})}
  end
end
