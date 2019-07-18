defmodule TypedEctoSchema do
  alias TypedEctoSchema.SyntaxSugar
  alias TypedEctoSchema.TypeBuilder

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
    quote do
      unquote(prelude(opts))

      Ecto.Schema.embedded_schema do
        unquote(inner(block))
      end

      unquote(postlude(opts))
    end
  end

  defmacro typed_schema(table_name, opts \\ [], do: block) do
    quote do
      unquote(prelude(opts))

      Ecto.Schema.schema unquote(table_name) do
        unquote(inner(block))
      end

      unquote(postlude(opts))
    end
  end

  defp prelude(opts) do
    quote do
      require unquote(TypeBuilder)
      unquote(TypeBuilder).init(unquote(opts))
    end
  end

  defp inner(block) do
    quote do
      unquote(TypeBuilder).add_primary_key(__MODULE__)
      unquote(SyntaxSugar.apply_to_block(block))
      unquote(TypeBuilder).enforce_keys()
    end
  end

  defp postlude(opts) do
    quote do
      unquote(TypeBuilder).define_type(unquote(opts))

      def __typed_schema__(:types),
        do: Enum.reverse(@__typed_ecto_schema_types__)
    end
  end
end
