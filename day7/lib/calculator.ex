defmodule Calculator do
  def calculate(tree) when is_map(tree) do
    contents = Map.new(tree, fn
      {k, %AST.File{size: size, name: name}} ->
        {k, %AST.File{size: size, name: name}}
      {k, v} when is_map(v) and not is_struct(v) ->
        {k, calculate(v)}
    end)
    size = contents
          |> Map.values
          |> Enum.map(fn
            %AST.CalculatedDir{size: size} -> size
            %AST.File{size: size} -> size
          end)
          |> Enum.sum
    %AST.CalculatedDir{size: size, contents: contents}
  end
end
