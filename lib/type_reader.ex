defmodule TypeReader do
  defmodule RemoteType do
    defstruct [:module, :name, :bindings]
  end

  defmodule TerminalType do
    defstruct [:name, :bindings]
  end

  defmodule CyclicalType do
    defstruct [:type_chain, :cycle_start_type]
  end

  defmodule UnionType do
    defstruct [:types]
  end

  defmodule Context do
    defstruct type_chain: []

    def prepend_to_type_chain(context, type) do
      %Context{context | type_chain: [type | context.type_chain]}
    end

    def in_type_chain?(context, type) do
      Enum.any?(context.type_chain, &(&1 == type))
    end
  end

  @basic_types [
    {:any, 0, quote(do: any())},
    {:none, 0, quote(do: none())},
    {:atom, 0, quote(do: atom())},
    {:map, 0, quote(do: map())},
    {:pid, 0, quote(do: pid())},
    {:port, 0, quote(do: port())},
    {:reference, 0, quote(do: reference())},
    {:struct, 0, quote(do: struct())},
    {:tuple, 0, quote(do: tuple())},
    {:float, 0, quote(do: float())},
    {:integer, 0, quote(do: integer())},
    {:neg_integer, 0, quote(do: neg_integer())},
    {:non_neg_integer, 0, quote(do: non_neg_integer())},
    {:pos_integer, 0, quote(do: pos_integer())},
    {:list, 1, quote(do: list(type))},
    {:nonempty_list, 1, quote(do: nonempty_list(type))},
    {:maybe_improper_list, 2, quote(do: maybe_improper_list(type1, type2))},
    {:nonempty_improper_list, 2, quote(do: nonempty_improper_list(type1, type2))},
    {:nonempty_maybe_improper_list, 2, quote(do: nonempty_maybe_improper_list(type1, type2))}
  ]

  @built_in_types [
    {:term, 0, quote(do: term())},
    {:arity, 0, quote(do: arity())},
    {:as_boolean, 1, quote(do: as_boolean(t))},
    {:binary, 0, quote(do: binary())},
    {:bitstring, 0, quote(do: bitstring())},
    {:boolean, 0, quote(do: boolean())},
    {:byte, 0, quote(do: byte())},
    {:char, 0, quote(do: char())},
    {:charlist, 0, quote(do: charlist())},
    {:nonempty_charlist, 0, quote(do: nonempty_charlist())},
    {:fun, 0, quote(do: fun())},
    {:function, 0, quote(do: function())},
    {:identifier, 0, quote(do: identifier())},
    {:iodata, 0, quote(do: iodata())},
    {:iolist, 0, quote(do: iolist())},
    {:keyword, 1, quote(do: keyword(t))},
    {:list, 0, quote(do: list(term()))},
    {:nonempty_list, 0, quote(do: nonempty_list())},
    {:maybe_improper_list, 0, quote(do: maybe_improper_list())},
    {:nonempty_maybe_improper_list, 0, quote(do: nonempty_maybe_improper_list())},
    {:mfa, 0, quote(do: mfa())},
    {:module, 0, quote(do: module())},
    {:no_return, 0, quote(do: no_return())},
    {:node, 0, quote(do: node())},
    {:number, 0, quote(do: number())},
    {:struct, 0, quote(do: struct())},
    {:timeout, 0, quote(do: timeout())}
  ]

  @standard_types @basic_types ++ @built_in_types

  defmacro gen_bindings_from_args(quoted_params) do
    params = Enum.map(quoted_params, &elem(&1, 0))

    quote do
      &Enum.zip(
        unquote(params),
        &1
      )
    end
  end

  def type_from_quoted(quoted_type) do
    do_type_from_quoted(quoted_type, %Context{})
  end

  for {name, arity, {_name, _, quoted_params}} <- @standard_types do
    defp do_type_from_quoted({unquote(name), _, quoted_args}, context)
         when length(quoted_args) == unquote(arity) do
      with {:ok, args} <- maybe_map(quoted_args, &do_type_from_quoted(&1, context)) do
        type = %TerminalType{
          name: unquote(name),
          bindings: gen_bindings_from_args(unquote(quoted_params)).(args)
        }

        {:ok, type}
      end
    end
  end

  defp do_type_from_quoted({{:., _, type_name_info}, _, quoted_args}, context) do
    {module, name} = fetch_remote_type_module_and_name!(type_name_info)

    with {:ok, {type, definition}} <-
           fetch_remote_type_from_definition(module, name, quoted_args, context) do
      from_type_and_definition(type, definition, Context.prepend_to_type_chain(context, type))
    end
  end

  defp from_type_and_definition(type, {:var, _, binding_name}, _context) do
    Keyword.fetch(type.bindings, binding_name)
  end

  defp from_type_and_definition(type, {_typ, _, :union, defined_types}, context) do
    with {:ok, types} <- maybe_map(defined_types, &from_type_and_definition(type, &1, context)) do
      type = %UnionType{
        types: types
      }

      {:ok, type}
    end
  end

  for {name, arity, {_name, _, quoted_params}} <- @standard_types do
    params = Enum.map(quoted_params, &elem(&1, 0))

    defp from_type_and_definition(
           type,
           {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, unquote(name)}, defined_params]},
           _context
         )
         when length(defined_params) == unquote(arity) do
      bindings =
        Enum.zip(
          unquote(params),
          Keyword.values(type.bindings)
        )

      type = %TerminalType{
        name: unquote(name),
        bindings: bindings
      }

      {:ok, type}
    end
  end

  defp from_type_and_definition(
         type,
         {:remote_type, _, [{:atom, _, module}, {:atom, _, name}, defined_params]},
         context
       ) do
    with {:ok, args} <- maybe_map(defined_params, &from_type_and_definition(type, &1, context)) do
      bindings =
        Enum.zip(
          Enum.map(defined_params, &elem(&1, 2)),
          args
        )

      new_type = %RemoteType{
        module: module,
        name: name,
        bindings: bindings
      }

      if Context.in_type_chain?(context, new_type) do
        {:ok,
         %CyclicalType{
           type_chain: context.type_chain,
           cycle_start_type: new_type
         }}
      else
        with {:ok, {^name, definition, _}} <-
               fetch_remote_type_definition(new_type.module, new_type.name, length(bindings)) do
          from_type_and_definition(
            new_type,
            definition,
            Context.prepend_to_type_chain(context, new_type)
          )
        end
      end
    end
  end

  defp from_type_and_definition(type, {:user_type, _, name, defined_params}, context) do
    from_type_and_definition(
      type,
      {:remote_type, [], [{:atom, [], type.module}, {:atom, [], name}, defined_params]},
      context
    )
  end

  defp fetch_remote_type_module_and_name!([{:__aliases__, _, module_path}, name])
       when is_list(module_path) and is_atom(name) do
    {Module.concat(module_path), name}
  end

  defp maybe_map(enum, map) do
    Enum.reduce_while(enum, [], fn value, acc ->
      case map.(value) do
        {:ok, mapped_value} -> {:cont, [mapped_value | acc]}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      acc when is_list(acc) -> {:ok, Enum.reverse(acc)}
      :error -> :error
    end
  end

  defp fetch_remote_type_from_definition(module, name, quoted_args, context) do
    with {:ok, {^name, definition, quoted_params}} <-
           fetch_remote_type_definition(module, name, length(quoted_args)),
         {:ok, args} <- maybe_map(quoted_args, &do_type_from_quoted(&1, context)) do
      type = %RemoteType{
        module: module,
        name: name,
        bindings:
          Enum.zip(
            Enum.map(quoted_params, &elem(&1, 2)),
            args
          )
      }

      {:ok, {type, definition}}
    end
  end

  defp fetch_remote_type_definition(module, type_name, arity) do
    with {:ok, types} <- Code.Typespec.fetch_types(module) do
      Enum.find_value(types, :error, fn
        {_, {^type_name, _, type_params} = compiled_type}
        when length(type_params) == arity ->
          {:ok, compiled_type}

        _other ->
          false
      end)
    end
  end
end
