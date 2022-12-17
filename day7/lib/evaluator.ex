defmodule Evaluator do
  @moduledoc """
  Evaluator for Day7.
  """

  def evaluate(commands), do: run(commands, %{}, [])

  defp run([], paths, _), do: paths

  # pwd is reversed because it's much easier to manipulate in Elixir that way.
  # /usr/local/bin/ => ["bin", "local", "usr"]

  defp run([%AST.Ls{lines: lines}|xs], paths, rev_pwd) do
    updated = Enum.reduce(lines, paths, fn line, paths ->
      name = line.name
      Map.put(paths, Enum.reverse([name|rev_pwd]), line)
    end)
    run(xs, updated, rev_pwd)
  end

  defp run([%AST.Cd{dir: "/"}|xs], paths, _) do
    run(xs, paths, [])
  end

  defp run([%AST.Cd{dir: ".."}|xs], paths, rev_pwd) do
    [_|rem] = rev_pwd
    run(xs, paths, rem)
  end

  defp run([%AST.Cd{dir: dir}|xs], paths, rev_pwd) do
    run(xs, paths, [dir|rev_pwd])
  end
end
