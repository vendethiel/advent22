defmodule Tree do
  def from_flat(map) do
    map
    |> Map.to_list
    |> Enum.reduce(%{}, fn {key, val}, paths ->
      enter(paths, key, val)
    end)
  end

  defp enter(paths, [key], el) do
    Map.put_new(paths, key, el) # TODO write test case where `put` is wrong here
  end
  defp enter(paths, [key|rem], el) do
    sub = enter(%{}, rem, el)
    Map.update(paths, key, sub, fn map -> enter(map, rem, el) end)
  end

  def flatten(map), do: flatten([], map, %{})

  defp flatten(prefix, %AST.CalculatedDir{size: size, contents: contents}, map) do
    Map.put_new(flatten(prefix, contents, map), prefix, size)
  end
  defp flatten(prefix, o, map) when is_map(o) and not is_struct(o) do
    Enum.reduce(o, map, fn {k, v}, updated ->
      flatten(prefix ++ [k], v, updated)
    end)
  end
  defp flatten(_, _, map), do: map
end
