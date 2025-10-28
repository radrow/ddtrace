defmodule Ddmon.TestServer do
  use GenServer

  def start_link(id, kind, opts) do
    GenServer.start_link(__MODULE__, {id, kind, opts}, opts)
  end


  def init({id, {:router, n}, genserver_opts}) do
    pids = for _ <- 1..n do
      {:ok, pid} = GenServer.start_link(__MODULE__, {id, :worker, genserver_opts}, genserver_opts)
      pid
    end

    {:ok, {:route, pids, []}}
  end

  def init({id, :worker, _}) do
    {:ok, {:work, id}}
  end


  def handle_call(:'$get_child', _from, state) do
    # This is to make the scenario script less confused when monitoring is
    # switched off
    {:reply, self(), state}
  end

  def handle_call(:'$get_workers', _from, state = {:route, free, busy}) do
    {:reply, free ++ busy, state}
  end

  def handle_call(:'$get_workers', _from, state = {:work, _}) do
    {:reply, [], state}
  end


  def handle_call(_request, _from, {:route, [worker|free], busy}) do
    {:reply, {:route, worker}, {:route, free, busy ++ [worker]}}
  end

  def handle_call({session_id, _request}, _from, {:route, [], [next_worker|busy]}) do
    # Wait for it to free in the Round Robin way. If we want to be smarter, we need the OR model.
    GenServer.call(next_worker, {session_id, []}, :infinity)
    {:reply, {:route, next_worker}, {:route, [], busy ++ [next_worker]}}
  end

  def handle_call({session_id, session}, _from, state = {:work, _id}) do
    # Session evaluation
    reply = eval_session(session_id, session)

    :erlang.yield()
    {:reply, {session_id, :reply, reply}, state}
  end


  def handle_cast({:release, worker}, {:route, free, busy}) do
    {:noreply, {:route, [worker|free], List.delete(busy, worker)}}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end


  defp eval_session(session_id, session) do
    eval_session(session_id, %{}, session)
  end

  defp eval_session(session_id, vars, session) do
    case session do
      [] ->
        self()

      [pid | next] when is_pid(pid) ->
        :erlang.yield()
        try do # The call may throw when the party is terminated
          call(pid, {session_id, next})
        rescue e ->
            e
        catch e, m ->
            {e, m}
        end
      [var | next] when is_atom(var) ->
        case Map.get(vars, var, :none) do
          :none -> :erlang.error({:undef, self(), var})
          val -> eval_session(session_id, vars, [val | next])
        end

      {:wait, time} ->
        wait(time)
        self()

      :spawn ->
        {:ok, pid} = GenServer.start_link(__MODULE__, {session_id, :worker}, [])
        pid

      t when is_tuple(t) ->
        {val, _vars} = List.foldl(
        Tuple.to_list(t),
        {self(), vars},
        fn
          ({:let, var, expr}, {_val, vars1}) ->
            val = eval_session(session_id, vars1, expr)
            vars2 = Map.put(vars1, var, val)
            {val, vars2}
          (stmt, {_val, vars1}) ->
            val = eval_session(session_id, vars1, stmt)
            {val, vars1}
        end
      )
      val
    end
  end


  def call(pid, data) do
    case GenServer.call(pid, data, :infinity) do
      {:route, pid1} ->
        res = GenServer.call(pid1, data, :infinity)
        GenServer.cast(pid, {:release, pid1})
        res
      res ->
        res
    end
  end


  def wait(time) do
    # This function exists mainly for the tracer's pleasure
    :timer.sleep(time)
    :ok
  end
end
