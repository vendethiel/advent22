defmodule Tree do
  def from_flat(map) do
    map
    |> Map.to_list
    |> Enum.reduce(%{}, fn {key, val}, paths ->
      enter(paths, key, val)
    end)
  end

  defp enter(paths, [key], %AST.Dir{}), do: Map.put(paths, key, %{})
  defp enter(paths, [key], el), do: Map.put(paths, key, el)
  defp enter(paths, [key|rem], el) do
    updated = enter(%{}, rem, el)
    Map.update(paths, key, updated, fn
      %AST.Dir{} -> updated # clear out the `dir`
      map -> enter(map, rem, el)
    end)
  end

  def flatten(map), do: flatten([], map, %{})

  defp flatten(prefix, %AST.CalculatedDir{size: size, contents: contents}, map) do
    Map.put(flatten(prefix, contents, map), prefix, size)
  end
  defp flatten(prefix, o, map) when is_map(o) and not is_struct(o) do
    o
    |> Enum.reduce(map, fn {k, v}, updated ->
      flatten(prefix ++ [k], v, updated)
    end)
  end
  defp flatten(_, _, map), do: map
end
