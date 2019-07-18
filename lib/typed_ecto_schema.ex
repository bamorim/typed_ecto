defmodule TypedEctoSchema do
  @schema_macros [
    :field,
    :embeds_one,
    :embeds_many,
    :has_one,
    :has_many,
    :belongs_to
  ]

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
  def __add_field__(mod, macro, name, ecto_type, opts) when is_atom(name) do
    nullable_default = Module.get_attribute(mod, :null?)

    type =
      TypedEctoSchema.EctoTypeMapper.type_for(
        ecto_type,
        macro,
        nullable_default,
        opts
      )

    TypedEctoSchema.TypeBuilder.add_field(mod, name, type)

    if field_is_enforced?(mod, opts),
      do: Module.put_attribute(mod, :keys_to_enforce, name)

    if macro == :belongs_to and Keyword.get(opts, :define_field, true) do
      __add_field__(
        mod,
        :field,
        Keyword.get(opts, :foreign_key, :"#{name}_id"),
        Keyword.get(opts, :type, :integer),
        opts
      )
    end
  end

  def __add_field__(_mod, _macro, name, _type, _opts) do
    raise ArgumentError, "a field name must be an atom, got #{inspect(name)}"
  end

  defp __typed_schema__(opts, block, wrapper) do
    wrapped_block =
      wrapper.(
        quote do
          TypedEctoSchema.__add_primary_key__(__MODULE__)
          unquote(apply_syntax_sugar_to_block(block))
          @enforce_keys @keys_to_enforce
        end
      )

    quote do
      unquote(prelude(opts))
      unquote(wrapped_block)

      TypedEctoSchema.TypeBuilder.define_type(unquote(opts))

      def __typed_schema__(:types), do: TypedEctoSchema.TypeBuilder.types()
    end
  end

  defp prelude(opts) do
    enforce? = Keyword.get(opts, :enforce, false)
    null? = Keyword.get(opts, :null, true)

    quote do
      require TypedEctoSchema.TypeBuilder
      TypedEctoSchema.TypeBuilder.init()
      Module.register_attribute(__MODULE__, :keys_to_enforce, accumulate: true)
      Module.put_attribute(__MODULE__, :enforce?, unquote(enforce?))
      Module.put_attribute(__MODULE__, :null?, unquote(null?))
    end
  end

  defp apply_syntax_sugar_to_block(block) do
    calls =
      case block do
        {:__block__, _, calls} ->
          calls

        call ->
          [call]
      end

    new_calls = Enum.map(calls, &apply_syntax_sugar/1)

    {:__block__, [], new_calls}
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
  ## Field Information Helpers
  ##

  defp field_is_enforced?(mod, opts) do
    global_enforce = Module.get_attribute(mod, :enforce?)
    default_enforce = global_enforce && is_nil(opts[:default])
    Keyword.get(opts, :enforce, default_enforce)
  end
end
