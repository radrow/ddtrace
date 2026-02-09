defmodule ElephantPatrol.PatrolSupervisor do
  @moduledoc """
  Supervisor for a patrol unit (one drone + one controller).

  Uses `:one_for_all` strategy — if either the drone or controller
  crashes, both are restarted to maintain a consistent pair.
  """
  use Supervisor

  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    Supervisor.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Starts the supervisor without linking to the calling process.
  Used when the caller is an ephemeral process (e.g. a script's spawn block).
  """
  def start(opts) do
    name = Keyword.fetch!(opts, :name)
    # Supervisor.start/3 doesn't exist, so we start_link and unlink
    result = Supervisor.start_link(__MODULE__, opts, name: name)

    case result do
      {:ok, pid} ->
        Process.unlink(pid)
        {:ok, pid}

      other ->
        other
    end
  end

  @doc """
  Gracefully restarts all children (drone + controller).
  Used after deadlock detection to unblock stuck gen_server calls.

  Terminates all running children then restarts any stopped ones.
  Safe to call even if some children have already been restarted
  by the :one_for_all strategy.
  """
  def restart_children(supervisor) do
    children = Supervisor.which_children(supervisor)

    # Terminate all running children (sends :shutdown — a clean exit)
    for {id, pid, _type, _modules} <- children, is_pid(pid) do
      Supervisor.terminate_child(supervisor, id)
    end

    # Restart any stopped children, in start order
    for {id, pid, _type, _modules} <- Enum.reverse(children), !is_pid(pid) do
      Supervisor.restart_child(supervisor, id)
    end

    :ok
  end

  @impl true
  def init(opts) do
    drone_opts = Keyword.fetch!(opts, :drone)
    controller_opts = Keyword.fetch!(opts, :controller)

    children = [
      {ElephantPatrol.Drone, drone_opts},
      {ElephantPatrol.Controller, controller_opts}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
