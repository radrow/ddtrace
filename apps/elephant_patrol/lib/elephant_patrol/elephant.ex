defmodule ElephantPatrol.Elephant do
  @moduledoc """
  A GenServer representing an elephant that can either stay calm or destroy crops.
  """
  use GenServer

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
    GenServer.call(elephant, :get_state)
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
  def init(_opts) do
    {:ok, :calm}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast(:stay_calm, _state) do
    {:noreply, :calm}
  end

  @impl true
  def handle_cast(:destroy_crops, _state) do
    {:noreply, :destroying_crops}
  end

  @impl true
  def handle_cast(:scare, _state) do
    {:noreply, :calm}
  end
end
