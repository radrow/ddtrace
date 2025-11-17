defmodule MicrochipFactory do
  @moduledoc false

  ### ==========================================================================
  ### Example interactions
  ### ==========================================================================

  @doc """
  Simple example with two services
  """
  def start_two(monitored \\ false) do
    Registry.start_link(keys: :unique, name: :factory)

    {:ok, prod1} = MicrochipFactory.Producer.start_link({:via, Registry, {:factory, :prod1}}, 3, [])
    {:ok, prod2} = MicrochipFactory.Producer.start_link({:via, Registry, {:factory, :prod2}}, 5, [])

    {:ok, insp1} =
      MicrochipFactory.Inspector.start_link(
        {:via, Registry, {:factory, :insp1}},
        {:via, Registry, {:factory, :prod1}}
      )

    {:ok, insp2} =
      MicrochipFactory.Inspector.start_link(
        {:via, Registry, {:factory, :insp2}},
        {:via, Registry, {:factory, :prod2}}
      )

    ctx = setup_monitors(monitored, [prod1, prod2, insp1, insp2])

    calls = [
      {{:via, Registry, {:factory, :prod1}}, {:via, Registry, {:factory, :insp2}}},
      {{:via, Registry, {:factory, :prod2}}, {:via, Registry, {:factory, :insp1}}}
    ]

    result = do_calls(calls, timeout: 1_000, monitor_ctx: ctx)
    print_result(result)
    cleanup_monitors(ctx)

    result
  end

  @doc """
  Complex example with many producers and a few inspectors
  """
  def start_many(monitored \\ false) do
    session_size = 30
    session_cut = 23

    Registry.start_link(keys: :unique, name: :factory)

    producer_pids =
      for idx <- 0..session_size, sess <- [:a, :b, :c] do
        name = {:via, Registry, {:factory, {:prod, sess, idx}}}

        components =
          if idx == session_size do
            []
          else
            [{:via, Registry, {:factory, {:prod, sess, idx + 1}}}]
          end

        {:ok, pid} = MicrochipFactory.Producer.start_link(name, 21, components)
        pid
      end

    inspector_pids =
      for sess <- [:a, :b, :c] do
        name = {:via, Registry, {:factory, {:insp, sess}}}

        cut_target = :rand.uniform(session_size - session_cut + 1) + session_cut - 1

        next_sess =
          case sess do
            :a -> :b
            :b -> :c
            :c -> :a
          end

        prod_ref = {:via, Registry, {:factory, {:prod, next_sess, cut_target}}}

        {:ok, pid} = MicrochipFactory.Inspector.start_link(name, prod_ref)
        pid
      end

    ctx = setup_monitors(monitored, producer_pids ++ inspector_pids)

    calls =
      for sess <- [:a, :b, :c] do
        {{:via, Registry, {:factory, {:prod, sess, 0}}},
         {:via, Registry, {:factory, {:insp, sess}}}}
      end

    result = do_calls(calls, timeout: 4_000, monitor_ctx: ctx)
    print_result(result)
    cleanup_monitors(ctx)

    result
  end

  ### ==========================================================================
  ### Printing and initiating
  ### ==========================================================================

  defp do_calls(calls, opts) do
    timeout = Keyword.fetch!(opts, :timeout)
    ctx = Keyword.get(opts, :monitor_ctx)

    delay_max = max(length(calls) * 80, 1)

    reqs =
      Enum.map(calls, fn {prod, insp} ->
        :timer.sleep(:rand.uniform(delay_max))
        prepare_request(prod, insp, ctx)
      end)

    results = Enum.map(reqs, &await_response(&1, timeout))

    case Enum.find(results, &match?({:deadlock, _}, &1)) do
      {:deadlock, dl} -> {:deadlock, dl}
      nil ->
        cond do
          Enum.any?(results, &(&1 == :timeout)) -> :timeout
          true ->
            replies = for {:ok, reply} <- results, do: reply
            {:success, replies}
        end
    end
  end

  defp prepare_request(prod, insp, nil) do
    call_req = :gen_server.send_request(prod, {:produce_microchip, insp})
    %{call_req: call_req}
  end

  defp prepare_request(prod, insp, %{monitors: monitors}) do
    prod_pid = GenServer.whereis(prod) || raise "unknown producer #{inspect(prod)}"
    monitor = Map.fetch!(monitors, prod_pid)

    call_req = :gen_server.send_request(prod, {:produce_microchip, insp})
    deadlock_req = :ddtrace.subscribe_deadlocks(monitor)

    %{call_req: call_req, deadlock_req: deadlock_req, monitor: monitor}
  end

  defp await_response(req_info, timeout) do
    reqs0 = :gen_server.reqids_new()
    reqs1 = :gen_server.reqids_add(req_info.call_req, {:call, req_info}, reqs0)

    reqs =
      case Map.get(req_info, :deadlock_req) do
        nil -> reqs1
        deadlock_req -> :gen_server.reqids_add(deadlock_req, {:deadlock, req_info}, reqs1)
      end

    wait_for_response(reqs, req_info, timeout)
  end

  defp wait_for_response(reqs, info, timeout) do
    case :gen_server.wait_response(reqs, timeout, true) do
      :timeout ->
        maybe_unsubscribe(info)
        :timeout

      {{:reply, {:deadlock, dl}}, {:deadlock, _}, _next_reqs} ->
        maybe_unsubscribe(info)
        {:deadlock, dl}

      {{:reply, payload}, {:call, _}, _next_reqs} ->
        maybe_unsubscribe(info)
        {:ok, payload}

      {{:error, _}, {:call, _}, _next_reqs} ->
        maybe_unsubscribe(info)
        :timeout

      {_, _, next_reqs} ->
        wait_for_response(next_reqs, info, timeout)

      :no_request ->
        maybe_unsubscribe(info)
        :timeout
    end
  end

  defp setup_monitors(false, _pids), do: nil

  defp setup_monitors(true, pids) do
    {:ok, mon_reg} = :mon_reg.start_link()

    monitors =
      pids
      |> Enum.uniq()
      |> Enum.reduce(%{}, fn pid, acc ->
        {:ok, monitor} = :ddtrace.start_link(pid, mon_reg, [])
        Map.put(acc, pid, monitor)
      end)

    %{mon_reg: mon_reg, monitors: monitors}
  end

  defp cleanup_monitors(nil), do: :ok

  defp cleanup_monitors(%{mon_reg: mon_reg, monitors: monitors}) do
    Enum.each(monitors, fn {_pid, monitor} ->
      :ddtrace.stop_tracer(monitor)
    end)

    :gen_server.stop(mon_reg)
  end

  defp maybe_unsubscribe(%{monitor: monitor}) do
    :ddtrace.unsubscribe_deadlocks(monitor)
    :ok
  end

  defp maybe_unsubscribe(_), do: :ok

  defp print_result(result) do
    case result do
      {:deadlock, dl} ->
        IO.puts("\e[31;1mDeadlock\e[0m:")

        dl =
          for p <- dl do
            case Registry.keys(:factory, p) do
              [] -> p
              [name | _] -> name
            end
          end

        duplicates =
          dl
          |> Enum.group_by(& &1)
          |> Enum.filter(fn {_k, v} -> length(v) > 1 end)
          |> Enum.map(fn {k, _v} -> k end)

        for p <- dl do
          if Enum.member?(duplicates, p) do
            IO.puts("- \e[31;1m#{inspect(p)} <==\e[0m")
          else
            IO.puts("- #{inspect(p)}")
          end
        end

      {:success, resps} ->
        IO.puts("\e[32;1mSuccess\e[0m: got #{inspect(resps)}")

      :timeout ->
        IO.puts("\e[33;1mTimeout\e[0m")
    end
  end
end
