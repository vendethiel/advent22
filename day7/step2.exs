tree = File.read!("data.txt")
       |> String.trim
       |> String.split("\n")
       |> Parser.parse
       |> Evaluator.evaluate
       |> Tree.from_flat
%AST.CalculatedDir{size: total_size, contents: dirs} = Calculator.calculate(tree)
size_needed = 30000000 - (70000000 - total_size)

{_, size} = dirs
            |> Tree.flatten
            |> Enum.filter(fn {_, v} -> v >= size_needed end)
            |> Enum.sort(fn {_, v1}, {_, v2} -> v1 < v2 end)
            |> Enum.at(0, {})
IO.puts(size)
