run = fn ->
  TypeReader.type_from_quoted(quote do: Client.A.rec_a(atom()))
end

# run = fn ->
#   TypeReader.type_from_quoted(quote do: Client.A.jump(atom()))
#   TypeReader.type_from_quoted(quote do: Enum.t())
# end

# run = fn ->
#   TypeReader.type_from_quoted(quote do: Client.A.simple_union())
# end
#
# run = fn ->
#   TypeReader.type_from_quoted(quote do: Client.A.keyword_wrap(number()))
# end
