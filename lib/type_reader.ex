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
    {:keyword, 0, quote(do: keyword())},
    {:keyword, 1, quote(do: keyword(type))},
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

  @aliased_types [
    {:string, :binary}
  ]

  @standard_types @basic_types ++ @built_in_types

  @typ_types [:type, :typep, :opaque]

  @doc """
  Will return the resolved type, if possible.

  ## Examples

      iex> TypeReader.type_from_quoted(quote do: binary())
      {:ok, %TerminalType{name: :binary, bindings: []}}

      iex> TypeReader.type_from_quoted(quote do: String.t())
      {:ok, %TerminalType{name: :binary, bindings: []}}

      iex> TypeReader.type_from_quoted(quote do: integer() | float())
      {:ok,
        %TerminalType{
          name: :union,
          bindings: [
            elem_types: [
              %TerminalType{name: :integer, bindings: []},
              %TerminalType{name: :float, bindings: []}
          ]
        ]
      }}

      iex> TypeReader.type_from_quoted(quote do: {:a, [String.t()]})
      {:ok,
        %TypeReader.TerminalType{
          bindings: [
            elem_types: [
              %TypeReader.TerminalType{bindings: [value: :a], name: :literal},
              %TypeReader.TerminalType{
                name: :list,
                bindings: [
                  type: %TypeReader.TerminalType{bindings: [], name: :binary}
                ],
              }
            ]
          ],
          name: :tuple
        }}
  """
  def type_from_quoted(quoted_type) do
    with {:ok, [type | _]} <- type_chain_from_quoted(quoted_type) do
      {:ok, type}
    end
  end

  @doc """
  Will return the entire type chain for the resolved type, if possible.

  ## Examples

      iex> TypeReader.type_chain_from_quoted(quote do: Enum.t())
      {:ok, [
        %TerminalType{name: :term, bindings: []},
        %RemoteType{module: Enumerable, name: :t, bindings: []},
        %RemoteType{module: Enum, name: :t, bindings: []},
      ]}
  """
  def type_chain_from_quoted(quoted_type) do
    with {:ok, %Context{type_chain: type_chain}} <-
           do_type_chain_from_quoted(quoted_type, %Context{}) do
      {:ok, type_chain}
    end
  end

  defmacrop gen_bindings_from_args(quoted_params) do
    params = Enum.map(quoted_params, &elem(&1, 0))

    quote do
      &Enum.zip(
        unquote(params),
        &1
      )
    end
  end

  defp do_type_from_quoted(quoted_type, context) do
    with {:ok, %Context{type_chain: [resolved_type | _]}} <-
           do_type_chain_from_quoted(quoted_type, context) do
      {:ok, resolved_type}
    end
  end

  defguardp is_literal(value) when is_atom(value) or is_integer(value)

  defp do_type_chain_from_quoted(literal, context) when is_literal(literal) do
    type = %TerminalType{
      name: :literal,
      bindings: [value: literal]
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted({:%{}, _, []}, context) do
    type = %TerminalType{
      name: :empty_map,
      bindings: []
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted({:__aliases__, _, _} = module_alias, context) do
    type = %TerminalType{
      name: :literal,
      bindings: [value: module_alias]
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted({:%{}, _, [_ | _] = quoted_map_contents}, context) do
    with {:ok, {required_kv_types, optional_kv_types}} <-
           resolve_required_and_optional_keys(quoted_map_contents, context) do
      type = %TerminalType{
        name: :map,
        bindings: [
          required: required_kv_types,
          optional: optional_kv_types
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted({:|, _, quoted_elem_types}, context) do
    with {:ok, elem_types} <- maybe_map(quoted_elem_types, &do_type_from_quoted(&1, context)) do
      type = %TerminalType{
        name: :union,
        bindings: [
          elem_types: squash_nested_unions_if_needed(elem_types)
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted({:<<>>, [], args}, context) do
    {size, unit} =
      case args do
        [] ->
          {0, nil}

        [{:"::", _, [{:_, _, _}, size]}] when is_integer(size) ->
          {size, nil}

        [{:"::", _, [{:_, _, _}, {:*, _, [{:_, _, _}, unit]}]}] when is_integer(unit) ->
          {nil, unit}

        [{:"::", _, [{:_, _, _}, size]}, {:"::", _, [{:_, _, _}, {:*, _, [{:_, _, _}, unit]}]}]
        when is_integer(size) and is_integer(unit) ->
          {size, unit}
      end

    type = %TerminalType{
      name: :bitstring,
      bindings: [
        size: size,
        unit: unit
      ]
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted([{:->, _, [[{:..., _, _}], quoted_return_type]}], context) do
    with {:ok, return_type} <- do_type_from_quoted(quoted_return_type, context) do
      type = %TerminalType{
        name: :function,
        bindings: [
          params: :any,
          return: return_type
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted([{:->, _, [param_specs, return_spec]}], context) do
    with {:ok, param_types} <- maybe_map(param_specs, &do_type_from_quoted(&1, context)),
         {:ok, return_type} <- do_type_from_quoted(return_spec, context) do
      type = %TerminalType{
        name: :function,
        bindings: [
          params: param_types,
          return: return_type
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted({:.., _, [min, max]}, context)
       when is_integer(min) and is_integer(max) do
    type = %TerminalType{
      name: :literal,
      bindings: [
        value: min..max
      ]
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted([{key, _quoted_item_type} | _] = quoted_keyword_type, context)
       when is_atom(key) do
    quoted_keyword_type
    |> maybe_map(fn {key, quoted_type} ->
      with {:ok, type} <- do_type_from_quoted(quoted_type, context), do: {:ok, {key, type}}
    end)
    |> case do
      {:ok, keys_and_types} ->
        type = %TerminalType{
          name: :keyword,
          bindings: [
            type: {:required_keys, keys_and_types}
          ]
        }

        prepend_type_and_wrap(context, type)

      :error ->
        {:error, context}
    end
  end

  defp do_type_chain_from_quoted([], context) do
    type = %TerminalType{
      name: :empty_list,
      bindings: []
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted([quoted_elem_type, {:..., _, _}], context) do
    with {:ok, elem_type} <- do_type_from_quoted(quoted_elem_type, context) do
      type = %TerminalType{
        name: :nonempty_list,
        bindings: [
          type: elem_type
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted([{:..., _, _}], context) do
    type = %TerminalType{
      name: :nonempty_list,
      bindings: [
        type: %TerminalType{name: :any, bindings: []}
      ]
    }

    prepend_type_and_wrap(context, type)
  end

  defp do_type_chain_from_quoted([quoted_elem_type], context) do
    with {:ok, elem_type} <- do_type_from_quoted(quoted_elem_type, context) do
      type = %TerminalType{
        name: :list,
        bindings: [
          type: elem_type
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted({:%, _, [aliases, {:%{}, _, quoted_map_contents}]}, context) do
    with {:ok, {required_kv_types, %{}}} <-
           resolve_required_and_optional_keys(quoted_map_contents, context) do
      module =
        case aliases do
          {:__aliases__, [alias: false], module_path} ->
            Module.concat(module_path)

          {:__aliases__, [alias: module], _} ->
            module

          {:__aliases__, _, module_path} ->
            Module.concat(module_path)
        end

      fields =
        for {%TerminalType{name: :literal, bindings: [value: key]}, value_type} <-
              required_kv_types,
            into: %{} do
          {key, value_type}
        end

      type = %TerminalType{
        name: :struct,
        bindings: [
          module: module,
          fields: fields
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted({:{}, _, quoted_elem_specs}, context) do
    with {:ok, elem_types} <-
           maybe_map(quoted_elem_specs, &do_type_from_quoted(&1, context)) do
      type = %TerminalType{
        name: :tuple,
        bindings: [
          elem_types: elem_types
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp do_type_chain_from_quoted({lhs_quoted_spec, rhs_quoted_spec}, context) do
    with {:ok, lhs_type} <- do_type_from_quoted(lhs_quoted_spec, context),
         {:ok, rhs_type} <- do_type_from_quoted(rhs_quoted_spec, context) do
      type = %TerminalType{
        name: :tuple,
        bindings: [
          elem_types: [lhs_type, rhs_type]
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  for {name, arity, {_name, _, quoted_params}} <- @standard_types do
    defp do_type_chain_from_quoted({unquote(name), _, quoted_args}, context)
         when length(quoted_args) == unquote(arity) do
      with {:ok, args} <- maybe_map(quoted_args, &do_type_from_quoted(&1, context)) do
        type = %TerminalType{
          name: unquote(name),
          bindings: gen_bindings_from_args(unquote(quoted_params)).(args)
        }

        prepend_type_and_wrap(context, type)
      end
    end
  end

  defp do_type_chain_from_quoted({{:., _, type_name_info}, _, quoted_args}, context) do
    {module, name} = fetch_remote_type_module_and_name!(type_name_info)

    fetch_remote_type(module, name, quoted_args, context)
  end

  defp fetch_remote_type(module, name, quoted_args, context) do
    with :error <- fetch_remote_type_from_beam(module, name, quoted_args, context) do
      fetch_remote_type_from_source(module, name, quoted_args, context)
    end
  end

  defp fetch_remote_type_from_beam(module, name, quoted_args, context) do
    with {:ok, {type, definition}} <-
           fetch_remote_type_from_definition(module, name, quoted_args, context) do
      from_type_and_definition(type, definition, Context.prepend_to_type_chain(context, type))
    end
  end

  defp from_type_and_definition(_type, {_typ, _, nil, []}, context) do
    type = %TerminalType{
      name: :literal,
      bindings: [
        value: nil
      ]
    }

    prepend_type_and_wrap(context, type)
  end

  defp from_type_and_definition(type, {:var, _, binding_name}, context) do
    case Keyword.fetch(type.bindings, binding_name) do
      {:ok, type} ->
        prepend_type_and_wrap(context, type)

      :error ->
        type = hd(type.bindings)
        prepend_type_and_wrap(context, type)
    end
  end

  defp from_type_and_definition(type, {_typ, _, :union, defined_types}, context) do
    with {:ok, elem_types} <- get_types_from_type_and_definition(type, defined_types, context) do
      type = %TerminalType{
        name: :union,
        bindings: [
          elem_types: elem_types
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp from_type_and_definition(
         type,
         {_typ, _, :fun, [{:type, _, :product, defined_params}, defined_return_type]},
         context
       ) do
    with {:ok, param_types} <-
           maybe_map(defined_params, &from_type_and_definition(type, &1, context)),
         {:ok, return_type} <- from_type_and_definition(type, defined_return_type, context) do
      type = %TerminalType{
        name: :function,
        bindings: [
          params: param_types,
          return: return_type
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  defp from_type_and_definition(type, {_typ, _, :tuple, defined_types}, context) do
    with {:ok, elem_types} <- get_types_from_type_and_definition(type, defined_types, context) do
      type = %TerminalType{
        name: :tuple,
        bindings: [
          elem_types: elem_types
        ]
      }

      prepend_type_and_wrap(context, type)
    end
  end

  for {name, arity, {_name, _, quoted_params}} <- @standard_types do
    params = Enum.map(quoted_params, &elem(&1, 0))

    defp from_type_and_definition(
           type,
           {:remote_type, _, [{:atom, _, :elixir}, {:atom, _, unquote(name)}, defined_params]},
           context
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

      prepend_type_and_wrap(context, type)
    end
  end

  for {type_alias, aliased_type} <- @aliased_types do
    defp from_type_and_definition(type, {typ, i, unquote(type_alias), params}, context) do
      from_type_and_definition(type, {typ, i, unquote(aliased_type), params}, context)
    end
  end

  defp from_type_and_definition(type, {:user_type, _, name, defined_params}, context) do
    from_type_and_definition(
      type,
      {:remote_type, [], [{:atom, [], type.module}, {:atom, [], name}, defined_params]},
      context
    )
  end

  defp from_type_and_definition(
         type,
         {:remote_type, _, [{:atom, _, module}, {:atom, _, name}, defined_params]},
         context
       ) do
    with {:ok, args} <- get_types_from_type_and_definition(type, defined_params, context) do
      bindings =
        case defined_params do
          :any ->
            []

          defined_params when is_list(defined_params) ->
            Enum.zip(
              Enum.map(defined_params, &elem(&1, 2)),
              args
            )
        end

      new_type = %RemoteType{
        module: module,
        name: name,
        bindings: bindings
      }

      if Context.in_type_chain?(context, new_type) do
        type = %CyclicalType{
          type_chain: context.type_chain,
          cycle_start_type: new_type
        }

        prepend_type_and_wrap(context, type)
      else
        with {:ok, {^name, definition, _}} <-
               fetch_type_definition_from_beam(new_type.module, new_type.name, length(bindings)) do
          from_type_and_definition(
            new_type,
            definition,
            Context.prepend_to_type_chain(context, new_type)
          )
        end
      end
    end
  end

  defp from_type_and_definition(type, {typ, opts, type_name, :any}, context) do
    from_type_and_definition(type, {typ, opts, type_name, []}, context)
  end

  defp from_type_and_definition(type, {typ, _, type_name, args}, context)
       when typ in @typ_types and is_list(args) do
    from_type_and_definition(
      type,
      {:remote_type, [], [{:atom, [], :elixir}, {:atom, [], type_name}, args]},
      context
    )
  end

  defp from_type_and_definition(_type, {:atom, _, literal_atom}, context)
       when is_atom(literal_atom) do
    type = %TerminalType{
      name: :literal,
      bindings: [
        value: literal_atom
      ]
    }

    prepend_type_and_wrap(context, type)
  end

  defp from_type_and_definition(type, {:ann_type, _, [_var, aliased_type]}, context) do
    from_type_and_definition(type, aliased_type, context)
  end

  defp fetch_remote_type_module_and_name!([{:__aliases__, _, module_path}, name])
       when is_list(module_path) and is_atom(name) do
    {Module.concat(module_path), name}
  end

  defp fetch_remote_type_module_and_name!([module, name])
       when is_atom(module) and is_atom(name) do
    {module, name}
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
           fetch_type_definition_from_beam(module, name, length(quoted_args)),
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

  def fetch_type_definition_from_beam(module, type_name, arity) do
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

  defp fetch_remote_type_from_source(module, type_name, quoted_args, context) do
    arity = length(quoted_args)

    with {:ok, source_path} <- module.__info__(:compile) |> Keyword.fetch(:source),
         {:ok, source} <- File.read(to_string(source_path)),
         {:ok, code} <- Code.string_to_quoted(source),
         {_, {:ok, params, type}} <-
           Macro.prewalk(code, :error, fn
             {:type, _,
              [
                {:"::", _,
                 [
                   {^type_name, _, params},
                   quoted_type
                 ]}
              ]} = ast,
             _
             when (arity == 0 and is_nil(params)) or arity == length(params) ->
               {ast, {:ok, params, quoted_type}}

             ast, acc ->
               {ast, acc}
           end) do
      args_lookup =
        (params || [])
        |> Enum.zip(quoted_args)
        |> Enum.into(%{})

      type
      |> Macro.prewalk(&Map.get(args_lookup, &1, &1))
      |> do_type_chain_from_quoted(context)
    end
  end

  defp get_types_from_type_and_definition(_type, :any, _context) do
    {:ok, []}
  end

  defp get_types_from_type_and_definition(type, defined_params, context) do
    with {:ok, contexts} <-
           maybe_map(defined_params, &from_type_and_definition(type, &1, context)) do
      Enum.map(contexts, &hd(&1.type_chain))
      |> wrap()
    end
  end

  defp resolve_required_and_optional_keys(quoted_map_contents, context) do
    Enum.reduce(quoted_map_contents, %{required: [], optional: []}, fn
      {{key_type, _, [quoted_key_type]}, quoted_value_type}, result
      when key_type in [:required, :optional] ->
        Map.update!(result, key_type, &[{quoted_key_type, quoted_value_type} | &1])

      {quoted_key_type, quoted_value_type}, result
      when is_atom(quoted_key_type) ->
        Map.update!(result, :required, &[{quoted_key_type, quoted_value_type} | &1])
    end)
    |> maybe_map(fn {kv_type, kvs} ->
      with {:ok, resolved_kvs} <-
             maybe_map(kvs, fn {quoted_key_type, quoted_value_type} ->
               with {:ok, key_type} <- do_type_from_quoted(quoted_key_type, context),
                    {:ok, value_type} <- do_type_from_quoted(quoted_value_type, context) do
                 {:ok, {key_type, value_type}}
               end
             end) do
        {:ok, {kv_type, Enum.into(resolved_kvs, %{})}}
      end
    end)
    |> case do
      {:ok, bindings} ->
        {:ok, {Keyword.fetch!(bindings, :required), Keyword.fetch!(bindings, :optional)}}

      :error ->
        {:error, context}
    end
  end

  defp squash_nested_unions_if_needed(elem_types) do
    Enum.flat_map(elem_types, fn
      %TerminalType{name: :union, bindings: [elem_types: elem_types]} ->
        squash_nested_unions_if_needed(elem_types)

      type ->
        [type]
    end)
  end

  defp prepend_type_and_wrap(context, type) do
    context
    |> Context.prepend_to_type_chain(type)
    |> wrap()
  end

  defp wrap(value), do: {:ok, value}

  if Mix.env() == :test do
    def __basic_types__, do: @basic_types
    def __built_in_types__, do: @built_in_types

    defmodule TestClient do
      defmodule A do
        alias Client.B, as: Bee
        @type i :: integer()
        @type t :: binary()
        @type c(lhs, rhs) :: C.t(lhs, rhs)
        @type identity(val) :: val
        @type rec_a(val) :: rec_b(val)
        @type rec_b(val) :: rec_c(val)
        @type rec_c(val) :: rec_d(val)
        @type rec_d(val) :: rec_b(val)

        @type jump(val) :: Bee.jump(val)

        @type simple_union :: integer() | number()

        @type keyword_wrap(val) :: keyword(val)
        @type map_wrap() :: map()
      end

      defmodule B do
        alias A, as: Ayy

        @type t :: atom() | Ayy.t()
        @type c(lhs, rhs) :: A.c(lhs, rhs)
        @type jump(val) :: val
      end

      defmodule C do
        @type t(lhs, rhs) :: B.c(lhs, rhs)
      end

      @type t(lhs, rhs) :: B.t() | C.t(lhs, rhs) | lhs | [rhs]
    end
  end
end
