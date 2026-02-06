defmodule ElephantPatrol.Elephant do
  @moduledoc """
  A GenServer representing an elephant that can either stay calm or destroy crops.
  """
  use GenServer
  require Logger

  # Green color for elephant
  @color IO.ANSI.green()
  @reset IO.ANSI.reset()

  defstruct [:name, :state]

  # Client API

  @doc """
  Starts an Elephant process.
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Returns the current state of the elephant (:calm or :destroying_crops).
  """
  def get_state(elephant) do
    GenServer.call(elephant, :get_state, 20_000)
  end

  @doc """
  Makes the elephant stay calm.
  """
  def stay_calm(elephant) do
    GenServer.cast(elephant, :stay_calm)
  end

  @doc """
  Makes the elephant destroy crops.
  """
  def destroy_crops(elephant) do
    GenServer.cast(elephant, :destroy_crops)
  end

  @doc """
  Scares the elephant, making it calm down.
  """
  def scare(elephant) do
    GenServer.cast(elephant, :scare)
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    name = Keyword.get(opts, :name, self())
    state = %__MODULE__{name: format_name(name), state: :calm}
    Logger.debug("#{@color}[#{state.name}] 🐘 Elephant initialized#{@reset}")
    {:ok, state}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state.state, state}
  end

  @impl true
  def handle_cast(:stay_calm, state) do
    Logger.debug("#{@color}[#{state.name}] 🐘 Staying calm#{@reset}")
    {:noreply, %{state | state: :calm}}
  end

  @impl true
  def handle_cast(:destroy_crops, state) do
    Logger.info("#{@color}[#{state.name}] 🐘 DESTROYING CROPS!#{@reset}")
    {:noreply, %{state | state: :destroying_crops}}
  end

  @impl true
  def handle_cast(:scare, state) do
    Logger.info("#{@color}[#{state.name}] 🐘 Got scared! Running away#{@reset}")
    {:noreply, %{state | state: :calm}}
  end

  # Private Functions

  defp format_name(name) when is_atom(name), do: "Elephant:#{name}"
  defp format_name(pid) when is_pid(pid), do: "Elephant:#{inspect(pid)}"
  defp format_name({:via, _, name}), do: "Elephant:#{inspect(name)}"
  defp format_name({:global, name}), do: "Elephant:#{inspect(name)}"
  defp format_name(other), do: "Elephant:#{inspect(other)}"
end
