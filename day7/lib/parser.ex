defmodule Parser do
  @moduledoc """
  Parser for Day7.
  """

  @doc """
  Parse the lines

  ## Examples

      iex> Parser.parse([])
      []

  """
  def parse(lines), do: parse_commands(lines, [])

  defp parse_commands([], parsed), do: Enum.reverse(parsed)
  defp parse_commands([line|xs], parsed) do
    if not is_command?(line), do: raise "It's not a command!"

    output = xs
             |> Enum.take_while(fn l -> !is_command?(l) end)
             |> Enum.map(&parse_output_line/1)
    rest = Enum.drop(xs, Enum.count(output))
    command = String.slice(line, 2, String.length(line)) # remove "$ "
    parse_commands(rest, [parse_command(command, output)|parsed])
  end

  defp parse_command("ls", lines), do: %AST.Ls{lines: lines}
  defp parse_command(cd, []) do
    ["cd", dir] = String.split(cd, " ", parts: 2)
    %AST.Cd{dir: dir}
  end

  defp is_command?(str), do: String.starts_with?(str, "$ ")

  defp parse_output_line(str) do
    if String.starts_with?(str, "dir ") do
      dirname = String.slice(str, 4, String.length(str))
      %AST.Dir{name: dirname}
    else
      [sizestring, name] = String.split(str, " ", parts: 2)
      {size, ""} = Integer.parse(sizestring)
      %AST.File{name: name, size: size}
    end
  end
end
