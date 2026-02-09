defmodule ElephantPatrol.Elephant do
  @moduledoc """
  A gen_statem representing an elephant that can either stay calm or destroy crops.
  When destroying crops, it periodically logs amusing quotes.
  """
  @behaviour :gen_statem
  require Logger

  # Green color for elephant
  @color IO.ANSI.green()
  @reset IO.ANSI.reset()

  # Cycle through these quotes when destroying crops
  @destruction_quotes [
    "*STOMP* *STOMP* Mmm, delicious crops!",
    "*TRUMPET* This farmland is now elephant territory!",
    "*CRUNCH* *MUNCH* Why do they plant such tasty things?",
    "*RUMBLE* Making my own garden art here...",
    "*STOMP* Landscaping, elephant style!"
  ]

  # How often to log when destroying (in milliseconds)
  @destruction_interval 3000

  defstruct [:name, quote_index: 0]

  # Client API

  @doc """
  Starts an Elephant process.
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name)
    :gen_statem.start_link(name, __MODULE__, opts, [])
  end

  @doc """
  Returns the current state of the elephant (:calm or :destroying).
  """
  def get_state(elephant) do
    :gen_statem.call(elephant, :get_state, 20_000)
  end

  @doc """
  Makes the elephant stay calm.
  """
  def stay_calm(elephant) do
    :gen_statem.cast(elephant, :stay_calm)
  end

  @doc """
  Makes the elephant destroy crops.
  """
  def destroy_crops(elephant) do
    :gen_statem.cast(elephant, :destroy_crops)
  end

  @doc """
  Scares the elephant, making it calm down.
  """
  def scare(elephant) do
    :gen_statem.cast(elephant, :scare)
  end

  # gen_statem Callbacks

  @impl :gen_statem
  def callback_mode(), do: :state_functions

  @impl :gen_statem
  def init(opts) do
    name = Keyword.get(opts, :name, self())
    data = %__MODULE__{name: format_name(name)}
    Logger.debug("#{@color}[#{data.name}] 🐘 Elephant initialized#{@reset}")
    {:ok, :calm, data}
  end

  # State: calm
  def calm({:call, from}, :get_state, _data) do
    {:keep_state_and_data, [{:reply, from, :calm}]}
  end

  def calm(:cast, :stay_calm, data) do
    Logger.debug("#{@color}[#{data.name}] 🐘 Staying calm#{@reset}")
    :keep_state_and_data
  end

  def calm(:cast, :destroy_crops, data) do
    Logger.info("#{@color}[#{data.name}] 🐘 DESTROYING CROPS!#{@reset}")
    # Start destroying and set a timeout to periodically log
    {:next_state, :destroying, data, [{:state_timeout, @destruction_interval, :log_destruction}]}
  end

  def calm(:cast, :scare, _data) do
    # Already calm, ignore
    :keep_state_and_data
  end

  # State: destroying
  def destroying({:call, from}, :get_state, _data) do
    {:keep_state_and_data, [{:reply, from, :destroying_crops}]}
  end

  def destroying(:cast, :stay_calm, data) do
    Logger.debug("#{@color}[#{data.name}] 🐘 Calming down#{@reset}")
    {:next_state, :calm, data}
  end

  def destroying(:cast, :destroy_crops, _data) do
    # Already destroying, ignore
    :keep_state_and_data
  end

  def destroying(:cast, :scare, data) do
    Logger.info("#{@color}[#{data.name}] 🐘 Got scared! Running away#{@reset}")
    {:next_state, :calm, data}
  end

  def destroying(:state_timeout, :log_destruction, data) do
    # Log a quote from the cycle
    quote = Enum.at(@destruction_quotes, data.quote_index)
    Logger.info("#{@color}[#{data.name}] 🐘 #{quote}#{@reset}")
    
    # Update quote index (cycle through quotes)
    next_index = rem(data.quote_index + 1, length(@destruction_quotes))
    new_data = %{data | quote_index: next_index}
    
    # Schedule next log
    {:keep_state, new_data, [{:state_timeout, @destruction_interval, :log_destruction}]}
  end

  # Private Functions

  defp format_name(name) when is_atom(name), do: "Elephant:#{name}"
  defp format_name(pid) when is_pid(pid), do: "Elephant:#{inspect(pid)}"
  defp format_name({:via, _, name}), do: "Elephant:#{inspect(name)}"
  defp format_name({:global, name}), do: "Elephant:#{inspect(name)}"
  defp format_name(other), do: "Elephant:#{inspect(other)}"
end
