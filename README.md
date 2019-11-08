# TypeReader

Like a typewriter, except instead of writing, it reads! Also, it's nothing like a typewriter.

## Goal

For a given type, we want to resolve it to it's most terminal value.

## Examples

```elixir
# simple type
iex(1)> TypeReader.type_chain_from_quoted(quote do: binary())
{:ok, [%TypeReader.TerminalType{name: :binary, bindings: []}]}
```
