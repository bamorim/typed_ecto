defmodule TypedEctoSchema do
  @schema_macros [
    :field,
    :embeds_one,
    :embeds_many,
    :has_one,
    :has_many,
    :belongs_to
  ]

  @schema_many_macros [:embeds_many, :has_many]

  @schema_assoc_macros [
    :has_many,
    :has_one,
    :belongs_to
  ]

  @module_for_ecto_type %{
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

  @module_for_ecto_type_keys Map.keys(@module_for_ecto_type)
  @direct_types [:integer, :float, :boolean, :map, :binary]

  @doc false
  defmacro __using__(_) do
    quote do
      import TypedEctoSchema,
        only: [
          typed_embedded_schema: 1,
          typed_embedded_schema: 2,
          typed_schema: 2,
          typed_schema: 3
        ]

      use Ecto.Schema
    end
  end

  defmacro typed_embedded_schema(opts \\ [], do: block) do
    __typed_schema__(
      opts,
      block,
      fn inner ->
        quote do
          Ecto.Schema.embedded_schema do
            unquote(inner)
          end
        end
      end
    )
  end

  defmacro typed_schema(table_name, opts \\ [], do: block) do
    __typed_schema__(
      opts,
      block,
      fn inner ->
        quote do
          Ecto.Schema.schema unquote(table_name) do
            unquote(inner)
          end
        end
      end
    )
  end

  defp __typed_schema__(opts, block, wrapper) do
    calls =
      case block do
        {:__block__, _, calls} ->
          calls

        call ->
          [call]
      end

    new_calls = Enum.map(calls, &apply_syntax_sugar/1)

    new_block = {:__block__, [], new_calls}

    wrapped_block =
      wrapper.(
        quote do
          TypedEctoSchema.__add_primary_key__(__MODULE__)
          unquote(new_block)
          @enforce_keys @keys_to_enforce
        end
      )

    enforce? = Keyword.get(opts, :enforce, false)
    null? = Keyword.get(opts, :null, true)

    quote do
      Module.register_attribute(__MODULE__, :fields, accumulate: true)
      Module.register_attribute(__MODULE__, :types, accumulate: true)
      Module.register_attribute(__MODULE__, :keys_to_enforce, accumulate: true)
      Module.put_attribute(__MODULE__, :enforce?, unquote(enforce?))
      Module.put_attribute(__MODULE__, :null?, unquote(null?))

      unquote(wrapped_block)

      TypedEctoSchema.__type__(@types, unquote(opts))

      def __typed_schema__(:keys), do: Enum.reverse(@fields)
      def __typed_schema__(:types), do: Enum.reverse(@types)
    end
  end

  defp apply_syntax_sugar({macro, _, [name, type, opts]})
       when macro in @schema_macros do
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
       when macro in @schema_macros do
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

  defp apply_syntax_sugar({:field, _, [name]}) do
    quote do
      field(unquote(name))

      TypedEctoSchema.__add_field__(
        __MODULE__,
        :field,
        unquote(name),
        :string,
        []
      )
    end
  end

  defp apply_syntax_sugar({:::, _, [{macro, _, [name, ecto_type, opts]}, type]})
       when macro in @schema_macros do
    apply_syntax_sugar(
      {macro, [],
       [name, ecto_type, [{:__typed_ecto_type__, Macro.escape(type)} | opts]]}
    )
  end

  defp apply_syntax_sugar({:::, _, [{macro, _, [name, ecto_type]}, type]})
       when macro in @schema_macros do
    apply_syntax_sugar(
      {macro, [], [name, ecto_type, [__typed_ecto_type__: Macro.escape(type)]]}
    )
  end

  defp apply_syntax_sugar({:::, _, [{:field, _, [name]}, type]}) do
    apply_syntax_sugar(
      {:field, [], [name, :string, [__typed_ecto_type__: Macro.escape(type)]]}
    )
  end

  defp apply_syntax_sugar(other), do: other

  ##
  ## Callbacks
  ##

  @doc false
  def __add_primary_key__(mod) do
    case Module.get_attribute(mod, :primary_key) do
      {name, type, opts} ->
        __add_field__(mod, nil, name, type, opts)

      _ ->
        :ok
    end
  end

  @doc false
  # | default | many | has_default |
  # | true
  def __add_field__(mod, macro, name, ecto_type, opts) when is_atom(name) do
    type =
      ecto_type
      |> type_for()
      |> wrap_in_list_if_many(macro)
      |> add_not_loaded_if_assoc(macro)
      |> add_nil_if_nullable(field_is_nullable?(mod, macro, opts))
      |> override_type(opts)

    Module.put_attribute(mod, :fields, name)
    Module.put_attribute(mod, :types, {name, type})

    if field_is_enforced?(mod, opts),
      do: Module.put_attribute(mod, :keys_to_enforce, name)
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
  defp type_for(atom) when atom in @module_for_ecto_type_keys do
    quote do
      unquote(Map.get(@module_for_ecto_type, atom)).t()
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

  defp type_for(:id) do
    quote do
      integer()
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

  ##
  ## Type Transformations Helpers
  ##

  defp wrap_in_list_if_many(type, macro) when macro in @schema_many_macros do
    quote do
      list(unquote(type))
    end
  end

  defp wrap_in_list_if_many(type, _), do: type

  defp add_not_loaded_if_assoc(type, macro)
       when macro in @schema_assoc_macros do
    quote(do: unquote(type) | Ecto.Association.NotLoaded.t())
  end

  defp add_not_loaded_if_assoc(type, _), do: type

  defp add_nil_if_nullable(type, false), do: type
  defp add_nil_if_nullable(type, true), do: quote(do: unquote(type) | nil)

  defp override_type(type, opts),
    do: Keyword.get(opts, :__typed_ecto_type__, type)

  ##
  ## Field Information Helpers
  ##

  defp field_is_nullable?(_mod, macro, _opts) when macro in @schema_many_macros,
    do: false

  defp field_is_nullable?(_mod, macro, _args)
       when macro in @schema_assoc_macros,
       do: true

  defp field_is_nullable?(mod, _macro, opts) do
    Keyword.get(opts, :null, Module.get_attribute(mod, :null?))
  end

  defp field_is_enforced?(mod, opts) do
    global_enforce = Module.get_attribute(mod, :enforce?)
    default_enforce = global_enforce && is_nil(opts[:default])
    Keyword.get(opts, :enforce, default_enforce)
  end
end
