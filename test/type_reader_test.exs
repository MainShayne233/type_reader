defmodule TypeReaderTest do
  use ExUnit.Case
  alias TypeReader.{CyclicalType, RemoteType, TerminalType, TestClient}
  doctest TypeReader

  describe "type_chain_from_quoted/1" do
    test "should resolve basic types" do
      for {type_name, _arity, quoted_type} <- TypeReader.__basic_types__() do
        quoted_type_with_args = apply_args(quoted_type)

        assert match?(
                 {:ok, [%TerminalType{name: ^type_name, bindings: _}]},
                 TypeReader.type_chain_from_quoted(quoted_type_with_args)
               )
      end
    end

    test "should handle cyclical types" do
      quoted_type = quote do: TypeReader.TestClient.A.rec_a(atom())

      {:ok, type_chain} = TypeReader.type_chain_from_quoted(quoted_type)

      assert match?(
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
               ],
               type_chain
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
