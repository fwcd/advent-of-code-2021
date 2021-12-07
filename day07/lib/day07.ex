defmodule Day07 do
  def main do
    input = File.read!("resources/demo.txt")
          |> String.split(",")
          |> Enum.map(fn raw -> String.to_integer(String.trim(raw)) end)
    input
  end
end
