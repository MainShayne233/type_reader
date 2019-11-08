defmodule TypeReaderTest do
  use ExUnit.Case
  use TypeReader.TestUtil

  alias TypeReader.{CyclicalType, RemoteType, TerminalType, TestClient}
  doctest TypeReader

  describe "type_chain_from_quoted/1" do
    test "should resolve basic types" do
      for {type_name, _arity, quoted_type} <- TypeReader.__basic_types__() do
        quoted_type_with_args = apply_args(quoted_type)

        assert_type_chain_match(
          quoted_type_with_args,
          [%TerminalType{name: ^type_name, bindings: _}]
        )
      end
    end

    test "should resolve built-in types" do
      for {type_name, _arity, quoted_type} <- TypeReader.__built_in_types__() do
        quoted_type_with_args = apply_args(quoted_type)

        assert_type_chain_match(
          quoted_type_with_args,
          [%TerminalType{name: ^type_name, bindings: _}]
        )
      end
    end

    test "should resolve basic types that usually have args but are not being passed in" do
      quoted_type = quote(do: list())

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :list, bindings: []}]
      )
    end

    test "should resolve unions" do
      quoted_type = quote(do: integer() | float())

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :union,
            bindings: [
              elem_types: [
                %TerminalType{name: :integer},
                %TerminalType{name: :float}
              ]
            ]
          }
        ]
      )
    end

    test "should resolve literal types" do
      quoted_type = quote(do: :an_atom)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :literal, bindings: [value: :an_atom]}]
      )

      quoted_type = quote(do: true)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :literal, bindings: [value: true]}]
      )

      quoted_type = quote(do: nil)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :literal, bindings: [value: nil]}]
      )
    end

    test "should resolve an empty bitstring type" do
      quoted_type = quote(do: <<>>)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :bitstring, bindings: [size: 0, unit: nil]}]
      )
    end

    test "should resolve a bitstring spec with a size specified" do
      quoted_type = quote(do: <<_::5>>)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :bitstring, bindings: [size: 5, unit: nil]}]
      )
    end

    test "should resolve a bitstring spec with a unit specified" do
      quoted_type = quote(do: <<_::_*6>>)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :bitstring, bindings: [size: nil, unit: 6]}]
      )
    end

    test "should resolve a bitstring spec with both a size and unit specified" do
      quoted_type = quote(do: <<_::7, _::_*8>>)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :bitstring, bindings: [size: 7, unit: 8]}]
      )
    end

    test "should resolve 0-arity anonymous functions" do
      quoted_type = quote(do: (() -> atom()))

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :function,
            bindings: [
              params: [],
              return: %TerminalType{name: :atom}
            ]
          }
        ]
      )
    end

    test "should resolve 1+ arity anonymous functions" do
      quoted_type = quote(do: (atom(), integer() -> atom()))

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :function,
            bindings: [
              params: [
                %TerminalType{name: :atom},
                %TerminalType{name: :integer}
              ],
              return: %TerminalType{name: :atom}
            ]
          }
        ]
      )
    end

    test "should resolve any-arity anonymous functions" do
      quoted_type = quote(do: (... -> atom()))

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :function,
            bindings: [
              params: :any,
              return: %TerminalType{name: :atom}
            ]
          }
        ]
      )
    end

    test "should resolve literal integers" do
      quoted_type = quote(do: 5)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :literal, bindings: [value: 5]}]
      )
    end

    test "should resolve literal ranges" do
      quoted_type = quote(do: 1..10)

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :literal, bindings: [value: 1..10]}]
      )
    end

    test "should resolve literal lists of a single type" do
      quoted_type = quote(do: [integer()])

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :list,
            bindings: [
              type: %TerminalType{name: :integer}
            ]
          }
        ]
      )
    end

    test "should resolve empty lists" do
      quoted_type = quote(do: [])

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :empty_list, bindings: []}]
      )
    end

    test "should resolve non-empty lists of any type" do
      quoted_type = quote(do: [...])

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :non_empty_list,
            bindings: [
              type: %TerminalType{name: :any}
            ]
          }
        ]
      )
    end

    test "should resolve non-empty lists of a specific type" do
      quoted_type = quote(do: [integer(), ...])

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :non_empty_list,
            bindings: [
              type: %TerminalType{name: :integer}
            ]
          }
        ]
      )
    end

    test "should resolve keyword lists" do
      quoted_type = quote(do: [some_key: integer(), another_key: float()])

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :keyword,
            bindings: [
              type:
                {:required_keys,
                 [
                   some_key: %TerminalType{name: :integer},
                   another_key: %TerminalType{name: :float}
                 ]}
            ]
          }
        ]
      )
    end

    test "should resolve literal empty maps" do
      quoted_type = quote(do: %{})

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :empty_map, bindings: []}]
      )
    end

    test "should resolve literal maps with literal keys" do
      quoted_type = quote(do: %{a: integer(), b: float()})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :map,
            bindings: [
              required: %{
                %TerminalType{name: :literal, bindings: [value: :a]} => %TerminalType{
                  name: :integer
                },
                %TerminalType{name: :literal, bindings: [value: :b]} => %TerminalType{
                  name: :float
                }
              },
              optional: %{}
            ]
          }
        ]
      )
    end

    test "should resolve literal maps with required/optional kvs" do
      quoted_type =
        quote(
          do: %{
            required(atom()) => integer(),
            required(integer()) => float(),
            optional(atom()) => atom()
          }
        )

      assert_type_chain_match(
        quoted_type,
        [
          %TypeReader.TerminalType{
            name: :map,
            bindings: [
              required: %{
                %TypeReader.TerminalType{bindings: [], name: :atom} => %TypeReader.TerminalType{
                  bindings: [],
                  name: :integer
                },
                %TypeReader.TerminalType{bindings: [], name: :integer} =>
                  %TypeReader.TerminalType{bindings: [], name: :float}
              },
              optional: %{
                %TypeReader.TerminalType{bindings: [], name: :atom} => %TypeReader.TerminalType{
                  bindings: [],
                  name: :atom
                }
              }
            ]
          }
        ]
      )
    end

    test "should resolve undefined structs" do
      quoted_type = quote(do: %NobodyHasDefinedThis{})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :struct,
            bindings: [
              module: NobodyHasDefinedThis,
              fields: %{}
            ]
          }
        ]
      )
    end

    test "should resolve defined structs" do
      defmodule SomebodyHasDefinedThis, do: defstruct([])
      quoted_type = quote(do: %SomebodyHasDefinedThis{})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :struct,
            bindings: [
              module: SomebodyHasDefinedThis,
              fields: %{}
            ]
          }
        ]
      )
    end

    test "should resolve defined structs that have been aliases" do
      defmodule StructA, do: defstruct([])
      alias StructA, as: StructB
      quoted_type = quote(do: %StructB{})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :struct,
            bindings: [
              module: StructA,
              fields: %{}
            ]
          }
        ]
      )
    end

    test "should resolve defined structs with defined kvs" do
      quoted_type =
        quote(
          do: %StructC{
            some_key: atom(),
            another_key: integer()
          }
        )

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :struct,
            bindings: [
              module: StructC,
              fields: %{
                some_key: %TerminalType{
                  name: :atom
                },
                another_key: %TerminalType{
                  name: :integer
                }
              }
            ]
          }
        ]
      )
    end

    test "should resolve literal empty tuples" do
      quoted_type = quote(do: {})

      assert_type_chain_match(
        quoted_type,
        [%TerminalType{name: :tuple, bindings: [elem_types: []]}]
      )
    end

    test "should resolve literal tuples with 1 element" do
      quoted_type = quote(do: {integer()})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :tuple,
            bindings: [
              elem_types: [
                %TerminalType{name: :integer}
              ]
            ]
          }
        ]
      )
    end

    test "should resolve literal tuples with 2 elements" do
      quoted_type = quote(do: {integer(), atom()})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :tuple,
            bindings: [
              elem_types: [
                %TerminalType{name: :integer},
                %TerminalType{name: :atom}
              ]
            ]
          }
        ]
      )
    end

    test "should resolve literal tuples with 3 elements" do
      quoted_type = quote(do: {integer(), atom(), float()})

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :tuple,
            bindings: [
              elem_types: [
                %TerminalType{name: :integer},
                %TerminalType{name: :atom},
                %TerminalType{name: :float}
              ]
            ]
          }
        ]
      )
    end

    ## MISC

    test "parse remote type that utilizies a tuple" do
      quoted_type = quote(do: :io.server_no_data())

      assert_type_chain_match(quoted_type, [
        %TypeReader.TerminalType{
          bindings: [
            elem_types: [
              %TypeReader.TerminalType{
                bindings: [
                  elem_types: [
                    %TypeReader.TerminalType{bindings: [value: :error], name: :literal},
                    %TypeReader.TerminalType{bindings: [], name: :term}
                  ]
                ],
                name: :tuple
              },
              %TypeReader.TerminalType{bindings: [value: :eof], name: :literal}
            ]
          ]
        },
        %TypeReader.RemoteType{bindings: [], module: :io, name: :server_no_data}
      ])
    end

    test "should properly resolve a built-in remote type with multiple alias jumps" do
      quoted_type = quote do: Enum.t()

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{name: :term},
          %RemoteType{name: :t, module: Enumerable},
          %RemoteType{name: :t, module: Enum}
        ]
      )
    end

    test "should handle a built in remote type that points to a union" do
      quoted_type = quote do: Access.container()

      assert_type_chain_match(
        quoted_type,
        [
          %TerminalType{
            name: :union,
            bindings: [
              elem_types: [
                %TerminalType{name: :list},
                %TerminalType{name: :struct},
                %TerminalType{name: :map}
              ]
            ]
          },
          %RemoteType{name: :container, module: Access}
        ]
      )
    end

    test "should handle cyclical types" do
      quoted_type = quote do: TypeReader.TestClient.A.rec_a(atom())

      assert_type_chain_match(
        quoted_type,
        [
          %CyclicalType{
            cycle_start_type: %RemoteType{
              module: TestClient.A,
              name: :rec_b,
              bindings: [_]
            },
            type_chain: [
              %RemoteType{module: TestClient.A, name: :rec_d},
              %RemoteType{module: TestClient.A, name: :rec_c},
              %RemoteType{module: TestClient.A, name: :rec_b},
              %RemoteType{module: TestClient.A, name: :rec_a}
            ]
          }
          | _
        ]
      )
    end
  end

  defp apply_args({quoted_type_name, options, params}) do
    args = get_args(length(params))

    {quoted_type_name, options, args}
  end

  defp get_args(count) when count >= 0 and count <= 3 do
    Enum.take(
      [
        quote(do: integer()),
        quote(do: float()),
        quote(do: atom())
      ],
      count
    )
  end

  ##
  # Use this to brute force check all remote types.
  #
  # defp all_compiled_types do
  #   :code.all_loaded()
  #   |> Enum.map(&elem(&1, 0))
  #   |> Enum.flat_map(fn module ->
  #     case Code.Typespec.fetch_types(module) do
  #       {:ok, types} ->
  #         Enum.map(types, fn {_, {name, _, params}} ->
  #           {module, name, length(params)}
  #         end)

  #       :error ->
  #         []
  #     end
  #   end)
  # end
end
