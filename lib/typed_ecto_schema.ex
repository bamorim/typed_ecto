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
    quote do
      Module.register_attribute(__MODULE__, :fields, accumulate: true)
      Module.register_attribute(__MODULE__, :types, accumulate: true)
      Module.register_attribute(__MODULE__, :keys_to_enforce, accumulate: true)
      Module.put_attribute(__MODULE__, :enforce?, unquote(!!opts[:enforce]))

      import TypedEctoSchema

      Ecto.Schema.embedded_schema do
        unquote(block)
        @enforce_keys @keys_to_enforce
      end

      TypedEctoSchema.__type__(@types, unquote(opts))

      def __keys__, do: @fields |> Keyword.keys() |> Enum.reverse()
      def __defaults__, do: Enum.reverse(@fields)
      def __types__, do: Enum.reverse(@types)
    end
  end

  defmacro typed_field(name, type, opts \\ []) do
    ecto_opts = Keyword.drop(opts, [:enforce])
    our_opts = Keyword.take(opts, [:enforce, :default])

    quote do
      TypedEctoSchema.__typed_field__(
        __MODULE__,
        unquote(name),
        unquote(type),
        unquote(our_opts)
      )

      Ecto.Schema.field(unquote(name), unquote(type), unquote(ecto_opts))
    end
  end

  ##
  ## Callbacks
  ##

  @doc false
  def __typed_field__(mod, name, ecto_type, opts) when is_atom(name) do
    type =
      case ecto_type do
        :string ->
          quote do
            String.t()
          end

        :integer ->
          quote do
            integer()
          end

        :boolean ->
          quote do
            boolean()
          end
      end

    if mod |> Module.get_attribute(:fields) |> Keyword.has_key?(name) do
      raise ArgumentError, "the field #{inspect(name)} is already set"
    end

    default = opts[:default]

    enforce? =
      if is_nil(opts[:enforce]),
        do: Module.get_attribute(mod, :enforce?) && is_nil(default),
        else: !!opts[:enforce]

    nullable? = !default && !enforce?

    Module.put_attribute(mod, :fields, {name, default})
    Module.put_attribute(mod, :types, {name, type_for(type, nullable?)})
    if enforce?, do: Module.put_attribute(mod, :keys_to_enforce, name)
  end

  def __typed_field__(_mod, name, _type, _opts) do
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

  # Makes the type nullable if the key is not enforced.
  defp type_for(type, false), do: type
  defp type_for(type, _), do: quote(do: unquote(type) | nil)
end
