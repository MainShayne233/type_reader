# TypeReader

Like a typewriter, except instead of writing, it reads! Also, it's nothing like a typewriter.

## Goal

For a given type, we want to resolve it to it's most terminal value.

## Examples

```elixir
# simple type
iex(1)> TypeReader.type_chain_from_quoted(quote do: binary())
{:ok, [%TypeReader.TerminalType{name: :binary, bindings: []}]}

# remote type
iex(2)> TypeReader.type_chain_from_quoted(quote do: String.t())
{:ok,
 [
   %TypeReader.TerminalType{bindings: [], name: :binary},
   %TypeReader.RemoteType{bindings: [], module: String, name: :t}
 ]}

# union type
iex(1)> TypeReader.type_from_quoted(quote do: integer() | float())
{:ok,
 %TypeReader.TerminalType{
   bindings: [
     elem_types: [
       %TypeReader.TerminalType{bindings: [], name: :integer},
       %TypeReader.TerminalType{bindings: [], name: :float}
     ]
   ],
   name: :union
 }}

# literal
 iex(2)> TypeReader.type_from_quoted(quote do: :cool)
{:ok, %TypeReader.TerminalType{bindings: [value: :cool], name: :literal}}

# complex types
iex(3)> TypeReader.type_from_quoted(quote do: {:a, [String.t()]})
{:ok,
 %TypeReader.TerminalType{
   bindings: [
     elem_types: [
       %TypeReader.TerminalType{bindings: [value: :a], name: :literal},
       %TypeReader.TerminalType{
         bindings: [type: %TypeReader.TerminalType{bindings: [], name: :binary}],
         name: :list
       }
     ]
   ],
   name: :tuple
 }}
```

## Type Chain

You might not want the final resolved type, like in the case of `Enum.t()` which as a result chain of `Enum.t() -> Enumerable.t() -> term()`.

In this case, you can use the `type_chain_from_quoted/1` function:

```elixir
iex(4)> TypeReader.type_chain_from_quoted(quote do: Enum.t())
{:ok,
 [
   %TypeReader.TerminalType{bindings: [], name: :term},
   %TypeReader.RemoteType{bindings: [], module: Enumerable, name: :t},
   %TypeReader.RemoteType{bindings: [], module: Enum, name: :t}
 ]}
```

The head of the list will be the most resolved type.
