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

  test "should not override a node with a parent empty directory" do
    assert Tree.from_flat(%{
      ["b", "lpzgcrd", "ztwbpvbq", "wdhjpzp"] => %{},
      ["b", "ppf", "mbbmmf.plr"] => file("mbbmmf.plr", 176398),
      ["b", "lpzgcrd"] => %{},
    }) == %{
      "b" => %{
        "lpzgcrd" => %{
          "ztwbpvbq" => %{
            "wdhjpzp" => %{}
          }
        },
        "ppf" => %{
          "mbbmmf.plr" => file("mbbmmf.plr", 176398),
        }
      }
    }
  end

  test "flatten" do
    assert Tree.flatten(%{
      "x" => %AST.CalculatedDir{
        size: 0,
        contents: %{
          "1" => %AST.CalculatedDir{
            size: 0,
            contents: %{}
          }
        }
      }
    }) == %{["x"] => 0, ["x", "1"] => 0}
  end
end
