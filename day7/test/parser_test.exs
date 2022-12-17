defmodule ParserTest do
  use ExUnit.Case
  doctest Parser
  import AST

  test "accepts an empty prompt" do
    assert Parser.parse([]) == []
  end

  test "parses a simple ls command" do
    assert Parser.parse(["$ ls"]) == [
      ls([]),
    ]
  end

  test "parses several commands in a row" do
    assert Parser.parse(["$ ls", "$ ls"]) == [
      ls([]),
      ls([]),
    ]
  end

  test "parses dir output for a command" do
    assert Parser.parse(["$ ls", "dir bar"]) == [
      ls([dir("bar")]),
    ]
  end

  test "parses dir output for the first command" do
    assert Parser.parse(["$ ls", "dir bar", "$ ls"]) == [
      ls([dir("bar")]),
      ls([]),
    ]
  end

  test "parses dir output for the last command" do
    assert Parser.parse(["$ ls", "$ ls", "dir baz"]) == [
      ls([]),
      ls([dir("baz")]),
    ]
  end

  test "parses dir output for several commands" do
    assert Parser.parse(["$ ls", "dir b", "dir c", "$ ls", "$ ls", "dir f"]) == [
      ls([dir("b"), dir("c")]),
      ls([]),
      ls([dir("f")]),
    ]
  end

  test "parses files" do
    assert Parser.parse(["$ ls", "10 b", "20 c"]) == [
      ls([file("b", 10), file("c", 20)])
    ]
  end

  test "parses mixed dirs/files" do
    assert Parser.parse(["$ ls", "10 b", "20 c", "dir d", "30 e", "dir f", "dir g"]) == [
      ls([
        file("b", 10),
        file("c", 20),
        dir("d"),
        file("e", 30),
        dir("f"),
        dir("g"),
      ])
    ]
  end

  test "parses cd commands" do
    assert Parser.parse(["$ ls", "$ cd a", "$ cd b"]) == [
      ls([]),
      cd("a"),
      cd("b")
    ]
  end

  test "fails if a different command" do
    catch_error(Parser.parse(["$ foo"]))
  end

  test "fails if cd has output" do
    catch_error(Parser.parse(["$ cd a", "dir b"]))
  end

  test "fails if it starts with some output" do
    assert catch_error(Parser.parse(["a"])) == %RuntimeError{message: "It's not a command!"}
  end
end
