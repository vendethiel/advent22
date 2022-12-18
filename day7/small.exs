File.read!("small-data.txt")
|> String.trim
|> String.split("\n")
|> Parser.parse
|> Evaluator.evaluate
|> Tree.from_flat
|> Calculator.calculate
|> Tree.flatten
|> Enum.filter(fn {_, v} -> v > 0 and v <= 100_000 end)
|> Enum.map(fn {_, v} -> v end)
|> Enum.sum
|> IO.puts
