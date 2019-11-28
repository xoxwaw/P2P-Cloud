defmodule Server.Storage do
  def get_file(file_name) do
    "../../assets/" <> file_name
    |> Path.expand(__DIR__)
    |> File.read!
  end

  def store_file(file_name, content) do
    path = "../../assets/" <> file_name
    |> Path.expand(__DIR__)
    File.write(file_name, content)
  end
end
