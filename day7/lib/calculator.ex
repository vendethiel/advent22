defmodule Calculator do
  def calculate(tree) when is_map(tree) do
    Map.new(tree, fn
      {k, %AST.Dir{}} -> {k, %AST.CalculatedDir{size: 0, contents: %{}}}
      {k, v} when is_struct(v) -> {k, v}
      {k, v} when is_map(v) ->
        updated = calculate(v)
        sum = updated
              |> Map.values
              |> Enum.map(fn
                %AST.CalculatedDir{size: size} -> size
                %AST.File{size: size} -> size
              end)
              |> Enum.sum
        {k, %AST.CalculatedDir{size: sum, contents: updated}}
    end)
  end
end
