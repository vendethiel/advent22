res = File.read!("small-data.txt")
      |> String.trim
      |> String.split("\n")
      |> Parser.parse
      |> Evaluator.evaluate
      |> IO.inspect
      |> Tree.from_flat
      |> IO.inspect
      |> Calculator.calculate
      |> IO.inspect
      |> Tree.to_flat
      |> IO.inspect

