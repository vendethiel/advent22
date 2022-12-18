defmodule CalculatorTest do
  use ExUnit.Case
  doctest Parser

  import AST

  test "calculate empty trees" do
    assert Calculator.calculate(%{}) == %AST.CalculatedDir{size: 0, contents: %{}}
  end

  test "calculate top-level files" do
    assert Calculator.calculate(%{"a" => file("a", 500)}) == %AST.CalculatedDir{
      size: 500,
      contents: %{
        "a" => file("a", 500)
      }
    }
  end

  test "calculate a big tree" do
    assert Calculator.calculate(%{
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
    }) == %AST.CalculatedDir{
      size: 5,
      contents: %{
        "a" => %AST.CalculatedDir{
          size: 5,
          contents: %{
            "x" => %AST.CalculatedDir{
              size: 0,
              contents: %{
                "1" => %AST.CalculatedDir{
                  size: 0,
                  contents: %{}
                }
              }
            },
            "y" => %AST.CalculatedDir{
              size: 5,
              contents: %{
                "I" => %AST.CalculatedDir{
                  size: 0,
                  contents: %{}
                },
                "II" => file("II", 2),
                "III" => file("III", 3),
              }
            }
          }
        },
        "b" => %AST.CalculatedDir{
          size: 0,
          contents: %{
            "|" => %AST.CalculatedDir{
              size: 0,
              contents: %{}
            }
          }
        },
        }
      }
  end
end
