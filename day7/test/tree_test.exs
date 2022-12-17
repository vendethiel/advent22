defmodule TreeTest do
  use ExUnit.Case
  doctest Tree

  import AST

  test "should not do anything to an empty map" do
    assert Tree.from_flat(%{}) == %{}
  end

  test "should keep a normal dir" do
    assert Tree.from_flat(%{["a"] => %{}}) == %{"a" => %{}}
  end

  test "parse" do
    assert Tree.from_flat(%{
      ["a"] => %{},
      ["a", "x"] => %{},
      ["a", "x", "1"] => %{},
      ["a", "y"] => %{},
      ["a", "y", "I"] => %{},
      ["a", "y", "II"] => file("II", 2),
      ["a", "y", "III"] => file("III", 3),
      ["b"] => %{},
      ["b", "|"] => %{},
    }) == %{
      "a" => %{
        "x" => %{
          "1" => %{},
        },
        "y" => %{
          "I" => %{},
          "II" => file("II", 2),
          "III" => file("III", 3),
        },
      },
      "b" => %{
        "|" => %{},
      },
    }
  end

  test "flatten" do
    assert Tree.to_flat(%{"a" => %{"b" => %{"c" => "d"}}}) == %{["a", "b", "c"] => "d"}
  end
end
