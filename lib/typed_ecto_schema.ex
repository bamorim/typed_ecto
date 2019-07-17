defmodule TypedEctoSchema do
  @doc false
  defmacro __using__(_) do
    quote do
      import TypedEctoSchema,
        only: [typed_embedded_schema: 1, typed_embedded_schema: 2]

      use Ecto.Schema
    end
  end

  defmacro typed_embedded_schema(opts \\ [], do: block) do
    calls =
      case block do
        {:__block__, _, calls} ->
          calls

        call ->
          [call]
      end

    new_calls = Enum.map(calls, &apply_syntax_sugar/1)

    new_block = {:__block__, [], new_calls}

    quote do
      Module.register_attribute(__MODULE__, :fields, accumulate: true)
      Module.register_attribute(__MODULE__, :types, accumulate: true)
      Module.register_attribute(__MODULE__, :keys_to_enforce, accumulate: true)
      Module.put_attribute(__MODULE__, :enforce?, unquote(!!opts[:enforce]))

      Ecto.Schema.embedded_schema do
        unquote(new_block)
        @enforce_keys @keys_to_enforce
      end

      TypedEctoSchema.__type__(@types, unquote(opts))

      def __typed_schema__(:keys), do: Enum.reverse(@fields)
      def __typed_schema__(:types), do: Enum.reverse(@types)
    end
  end

  @macro_names [:field, :embeds_one, :embeds_many]

  defp apply_syntax_sugar({macro, _, [name, type, opts]})
       when macro in @macro_names do
    ecto_opts = Keyword.drop(opts, [:__typed_ecto_type__, :enforce])

    quote do
      unquote(macro)(unquote(name), unquote(type), unquote(ecto_opts))

      TypedEctoSchema.__add_field__(
        __MODULE__,
        unquote(macro),
        unquote(name),
        unquote(type),
        unquote(opts)
      )
    end
  end

  defp apply_syntax_sugar({macro, _, [name, type]})
       when macro in @macro_names do
    quote do
      unquote(macro)(unquote(name), unquote(type))

      TypedEctoSchema.__add_field__(
        __MODULE__,
        unquote(macro),
        unquote(name),
        unquote(type),
        []
      )
    end
  end

  defp apply_syntax_sugar({:::, _, [{macro, _, [name, ecto_type, opts]}, type]})
       when macro in @macro_names do
    apply_syntax_sugar(
      {macro, [],
       [name, ecto_type, [{:__typed_ecto_type__, Macro.escape(type)} | opts]]}
    )
  end

  defp apply_syntax_sugar({:::, _, [{macro, _, [name, ecto_type]}, type]})
       when macro in @macro_names do
    apply_syntax_sugar(
      {macro, [], [name, ecto_type, [__typed_ecto_type__: Macro.escape(type)]]}
    )
  end

  defp apply_syntax_sugar(other), do: other

  ##
  ## Callbacks
  ##

  @doc false
  def __add_field__(mod, macro, name, ecto_type, opts) when is_atom(name) do
    base_type = type_for(ecto_type)

    default_type =
      if macro == :embeds_many do
        quote do
          list(unquote(base_type))
        end
      else
        base_type
      end

    type = Keyword.get(opts, :__typed_ecto_type__, default_type)

    if mod |> Module.get_attribute(:fields) |> Enum.member?(name) do
      raise ArgumentError, "the field #{inspect(name)} is already set"
    end

    default = opts[:default]

    enforce? =
      if is_nil(opts[:enforce]),
        do: Module.get_attribute(mod, :enforce?) && is_nil(default),
        else: !!opts[:enforce]

    nullable? = macro != :embeds_many && !default && !enforce?

    Module.put_attribute(mod, :fields, name)

    Module.put_attribute(
      mod,
      :types,
      {name, add_nil_if_nullable(type, nullable?)}
    )

    if enforce?, do: Module.put_attribute(mod, :keys_to_enforce, name)
  end

  def __add_field__(_mod, _macro, name, _type, _opts) do
    raise ArgumentError, "a field name must be an atom, got #{inspect(name)}"
  end

  @doc false
  defmacro __type__(types, opts) do
    if Keyword.get(opts, :opaque, false) do
      quote bind_quoted: [types: types] do
        @opaque t() :: %__MODULE__{unquote_splicing(types)}
      end
    else
      quote bind_quoted: [types: types] do
        @type t() :: %__MODULE__{unquote_splicing(types)}
      end
    end
  end

  ##
  ## Helpers
  ##

  # Gets the type for a given Ecto.Type.t()
  @type_t_module_map %{
    string: String,
    decimal: Decimal,
    date: Date,
    time: Time,
    time_usec: Time,
    naive_datetime: NaiveDateTime,
    naive_datetime_usec: NaiveDateTime,
    utc_datetime: DateTime,
    utc_datetime_usec: DateTime
  }
  @type_t_module_keys Map.keys(@type_t_module_map)
  @direct_types [:integer, :float, :boolean, :map, :binary]

  defp type_for(atom) when atom in @type_t_module_keys do
    quote do
      unquote(Map.get(@type_t_module_map, atom)).t()
    end
  end

  defp type_for(atom) when atom in @direct_types do
    quote do
      unquote(atom)()
    end
  end

  defp type_for(:binary_id) do
    quote do
      binary()
    end
  end

  defp type_for({:array, type}) do
    quote do
      list(unquote(type_for(type)))
    end
  end

  defp type_for({:map, type}) do
    quote do
      %{optional(any()) => unquote(type_for(type))}
    end
  end

  defp type_for(atom) when is_atom(atom) do
    case to_string(atom) do
      "Elixir." <> _ ->
        quote do
          unquote(atom).t()
        end

      _ ->
        quote do
          any()
        end
    end
  end

  defp type_for(_) do
    quote do
      any()
    end
  end

  # Makes the type nullable if the key is not enforced.
  defp add_nil_if_nullable(type, false), do: type
  defp add_nil_if_nullable(type, _), do: quote(do: unquote(type) | nil)
end
