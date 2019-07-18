defmodule TypedEctoSchema.TypeBuilder do
  @moduledoc false

  defmacro init do
    quote do
      unquote(__MODULE__).__init__(__MODULE__)
    end
  end

  defmacro define_type(opts) do
    quote do
      unquote(__MODULE__).__define_type__(
        @__typed_ecto_schema_types__,
        unquote(opts)
      )
    end
  end

  def __init__(mod) do
    Module.register_attribute(mod, :__typed_ecto_schema_types__,
      accumulate: true
    )
  end

  def add_field(mod, name, type) do
    Module.put_attribute(mod, :__typed_ecto_schema_types__, {name, type})
  end

  defmacro types do
    quote do
      Enum.reverse(@__typed_ecto_schema_types__)
    end
  end

  @doc false
  defmacro __define_type__(types, opts) do
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
end
