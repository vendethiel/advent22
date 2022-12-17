res = File.read!("data.txt")
      |> String.trim
      |> String.split("\n")
      |> Parser.parse
      |> Evaluator.evaluate
      |> IO.inspect(label: "a")
      |> Tree.from_flat
      |> IO.inspect(label: "b")
      |> Calculator.calculate
      |> IO.inspect(label: "c")
      |> Tree.flatten
      |> IO.inspect(label: "d")
      |> IO.inspect(label: "e")
      |> Enum.filter(fn {k, v} -> v > 0 and v <= 100_000 end)
      |> IO.inspect(label: "f")
      |> Enum.map(fn {k, v} -> v end)
      |> IO.inspect(label: "L")
      |> Enum.sum
      |> IO.inspect(label: "R")
