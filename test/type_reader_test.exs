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
