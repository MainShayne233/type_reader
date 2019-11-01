defmodule TypeReaderTest do
  use ExUnit.Case
  use TypeReader.TestUtil

  alias TypeReader.{CyclicalType, RemoteType, TerminalType, TestClient, UnionType}
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

    test "should resolve 1+-arity anonymous functions" do
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

    ## MISC

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
          %UnionType{
            types: [
              %TerminalType{name: :list},
              %TerminalType{name: :struct},
              %TerminalType{name: :map}
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
    args =
      Enum.take(
        [
          quote(do: integer()),
          quote(do: float()),
          quote(do: atom())
        ],
        length(params)
      )

    {quoted_type_name, options, args}
  end
end
