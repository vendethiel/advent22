defmodule AST do
  # Commands
  defmodule Ls do
    defstruct lines: []
  end

  defmodule Cd do
    defstruct dir: ""
  end

  # Output
  defmodule File do
    defstruct name: "", size: 0
  end

  defmodule Dir do
    defstruct name: ""
  end

  # Calc
  defmodule CalculatedDir do
    defstruct size: 0, contents: []
  end

  def ls(lines), do: %Ls{lines: lines}
  def cd(dir), do: %Cd{dir: dir}
  def dir(name), do: %Dir{name: name}
  def file(name, size), do: %File{name: name, size: size}
end

