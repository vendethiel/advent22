defmodule CalculatorTest do
  use ExUnit.Case
  doctest Parser

  import AST

  test "Calculate empty trees" do
    assert Calculator.calculate(%{}) == %{}
  end

  test "Calculate a big tree" do
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
    }) == %{
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
  end
end
