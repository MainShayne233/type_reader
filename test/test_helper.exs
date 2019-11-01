defmodule TypeReader.TestUtil do
  defmacro __using__([]) do
    quote do
      import TypeReader.TestUtil, only: [assert_type_chain_match: 2]
    end
  end

  defmacro assert_type_chain_match(quoted_type, expected_type_chain_matcher) do
    quote do
      {:ok, type_chain} = TypeReader.type_chain_from_quoted(unquote(quoted_type))

      assert match?(
               unquote(expected_type_chain_matcher),
               type_chain
             )
    end
  end
end

ExUnit.start()
