defmodule EvaluatorTest do
  use ExUnit.Case
  doctest Evaluator

  import AST

  test "should do nothing for no instructions" do
    assert Evaluator.evaluate([]) == %{}
  end

  test "should not fill paths on cd" do
    assert Evaluator.evaluate([cd("/")]) == %{}
    assert Evaluator.evaluate([cd("a")]) == %{}
  end

  test "should allow as many cds as one wants" do
    assert Evaluator.evaluate([cd("a"), cd("b"), cd("c")]) == %{}
  end

  test "should allow to go back to root at any point" do
    assert Evaluator.evaluate([cd("a"), cd("/"), cd("c")]) == %{}
  end

  test "should allow to go back levels" do
    assert Evaluator.evaluate([cd("a"), cd("b"), cd(".."), cd("c"), cd(".."), cd("..")]) == %{}
  end

  test "should mark a path" do
    assert Evaluator.evaluate([
      ls([dir("a")])
    ]) == %{["a"] => dir("a")}
  end

  test "evaluate" do
    assert Evaluator.evaluate([
      ls([dir("a"), dir("b")]),
      cd("a"),
      ls([dir("x"), dir("y")]),
      cd("x"),
      ls([dir("1")]),
      cd(".."),
      cd("y"),
      ls([dir("I"), file("II", 2), file("III", 3)]),
      cd("/"),
      cd("b"),
      ls([dir("|")])
    ]) == %{
      ["a"] => dir("a"),
      ["a", "x"] => dir("x"),
      ["a", "x", "1"] => dir("1"),
      ["a", "y"] => dir("y"),
      ["a", "y", "I"] => dir("I"),
      ["a", "y", "II"] => file("II", 2),
      ["a", "y", "III"] => file("III", 3),
      ["b"] => dir("b"),
      ["b", "|"] => dir("|"),
    }
  end

  test "should not allow you to go back before the root" do
    catch_error(Evaluator.evaluate([cd("..")]))
    catch_error(Evaluator.evaluate([cd("a"), cd(".."), cd("..")]))
  end
end
