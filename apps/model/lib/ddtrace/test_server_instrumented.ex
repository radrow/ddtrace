defmodule DDTrace.TestServerInstrumented do
  use GenServer
  alias :ddmon, as: GenServer

  def start_link(id, kind, opts) do
    GenServer.start_link(__MODULE__, {id, kind, opts}, opts)
  end

  def init({id, {:router, n}, genserver_opts}) do
    pids =
      for _ <- 1..n do
        {:ok, pid} = GenServer.start_link(__MODULE__, {id, :worker, genserver_opts}, genserver_opts)
        pid
      end

    {:ok, {:route, pids, []}}
  end

  def init({id, :worker, _}) do
    {:ok, {:work, id}}
  end

  def handle_call(:'$get_child', _from, state) do
    # Keep the scenario runner happy when monitoring is disabled
    {:reply, self(), state}
  end

  def handle_call(:'$get_workers', _from, {:route, free, busy} = state) do
    {:reply, free ++ busy, state}
  end

  def handle_call(:'$get_workers', _from, {:work, _} = state) do
    {:reply, [], state}
  end

  def handle_call(_request, _from, {:route, [worker | free], busy}) do
    {:reply, {:route, worker}, {:route, free, busy ++ [worker]}}
  end

  def handle_call({session_id, _request}, _from, {:route, [], [next_worker | busy]}) do
    GenServer.call(next_worker, {session_id, []}, :infinity)
    {:reply, {:route, next_worker}, {:route, [], busy ++ [next_worker]}}
  end

  def handle_call({session_id, session}, _from, {:work, _id} = state) do
    reply = eval_session(session_id, session)
    :erlang.yield()
    {:reply, {session_id, :reply, reply}, state}
  end

  def handle_cast({:release, worker}, {:route, free, busy}) do
    {:noreply, {:route, [worker | free], List.delete(busy, worker)}}
  end

  def handle_cast(_msg, state), do: {:noreply, state}

  defp eval_session(session_id, session), do: eval_session(session_id, %{}, session)

  defp eval_session(session_id, vars, session) do
    case session do
      [] ->
        self()

      [pid | next] when is_pid(pid) ->
        :erlang.yield()

        try do
          call(pid, {session_id, next})
        rescue
          e -> e
        catch
          :exit, {:shutdown, _info} -> :shutdown
          :exit, :shutdown -> :shutdown
          :exit, {:noproc, _} -> :noproc
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

      tuple when is_tuple(tuple) ->
        {val, _vars} =
          Tuple.to_list(tuple)
          |> List.foldl({self(), vars}, fn
            {:let, var, expr}, {_val, vars_acc} ->
              value = eval_session(session_id, vars_acc, expr)
              {value, Map.put(vars_acc, var, value)}

            stmt, {_val, vars_acc} ->
              value = eval_session(session_id, vars_acc, stmt)
              {value, vars_acc}
          end)

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
    :timer.sleep(time)
    :ok
  end
end
