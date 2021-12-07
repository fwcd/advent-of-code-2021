defmodule Day07 do
  def main do
    input = File.read!("resources/input.txt")
          |> String.split(",")
          |> Enum.map(fn raw -> String.to_integer(String.trim(raw)) end)

    part1 = input
          |> Enum.map(fn pos -> input
            |> Enum.map(fn x -> abs(x - pos) end)
            |> Enum.sum end)
          |> Enum.min
    IO.puts "Part 1: #{part1}"

    part2 = (Enum.min(input)..Enum.max(input))
          |> Enum.map(fn pos -> input
            |> Enum.map(fn x -> abs(x - pos) end)
            |> Enum.map(fn dx -> div(dx * (dx + 1), 2) end) # Gauss formula
            |> Enum.sum end)
          |> Enum.min
    IO.puts "Part 2: #{part2}"
  end
end
