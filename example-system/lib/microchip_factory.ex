defmodule MicrochipFactory do

  ### ==========================================================================
  ### Example interactions
  ### ==========================================================================


  @doc"""
  Simple example with two services
  """
  def start_two(monitored \\ false) do
    # Starting namespace registry
    Registry.start_link(keys: :unique, name: :factory)

    # Starting producers
    {:ok, _prod1} = MicrochipFactory.Producer.start_link({:via, Registry, {:factory, :prod1}}, 3, [])
    {:ok, _prod2} = MicrochipFactory.Producer.start_link({:via, Registry, {:factory, :prod2}}, 5, [])

    # Starting inspectors
    {:ok, _insp1} = MicrochipFactory.Inspector.start_link(
      {:via, Registry, {:factory, :insp1}},
      {:via, Registry, {:factory, :prod1}}
    )
    {:ok, _insp2} = MicrochipFactory.Inspector.start_link(
      {:via, Registry, {:factory, :insp2}},
      {:via, Registry, {:factory, :prod2}}
    )

    # Initial calls
    calls = [{{:via, Registry, {:factory, :prod1}}, {:via, Registry, {:factory, :insp2}}},
             {{:via, Registry, {:factory, :prod2}}, {:via, Registry, {:factory, :insp1}}}
            ]
    do_calls(calls, monitored, 1000)
  end


  @doc """
  Complex example with many producers and a few inspectors
  """
  def start_many(monitored \\ false) do
    # How long a single cascade of calls should be
    session_size = 30
    # At what point sessions should clash with others
    session_cut = 23

    # Starting namespace registry
    Registry.start_link(keys: :unique, name: :factory)

    # Starting producers in three sessions
    _prods = for idx <- 0..session_size, sess <- [:a, :b, :c], into: %{} do
      name = {:via, Registry, {:factory, {:prod, sess, idx}}}

      components = if idx == session_size do
        []
      else
        [{:via, Registry, {:factory, {:prod, sess, idx + 1}}}]
      end

      prod = MicrochipFactory.Producer.start_link(name, 21, components)
      {{sess, idx}, prod}
    end

    # Starting inspectors in three sessions
    _insps = for sess <- [:a, :b, :c] do
      name = {:via, Registry, {:factory, {:insp, sess}}}

      # Sample the actual clash point
      cut_target = :rand.uniform(session_size - session_cut + 1) + session_cut - 1

      # The session to clash with
      next_sess = case sess do
                    :a -> :b
                    :b -> :c
                    :c -> :a
                  end

      # Producer to query for metadata
      prod_ref = {:via, Registry, {:factory, {:prod, next_sess, cut_target}}}

      MicrochipFactory.Inspector.start_link(name, prod_ref)
    end

    # All producers in each session share the inspector
    calls = for sess <- [:a, :b, :c] do
      {{:via, Registry, {:factory, {:prod, sess, 0}}},
       {:via, Registry, {:factory, {:insp, sess}}}
      }
    end
    do_calls(calls, monitored, 4000)
  end


  ### ==========================================================================
  ### Printing and initiating
  ### ==========================================================================

  defp do_calls(calls, monitored, timeout) do
    # Performs a sequence of initial calls to the system. Calls are sent in
    # parallel. If any results in a timeout or a deadlock, those are reported;
    # otherwise a success message is displayed.

    # Subscribe to deadlocks if requested
    if monitored, do: (for {prod, _} <- calls, do: :ddmon.subscribe_deadlocks(prod))

    # Perform the calls
    reqids = for {prod, insp} <- calls do
      :timer.sleep(:rand.uniform(80 * length(calls)))
      :ddmon.send_request(prod, {:produce_microchip, insp})
    end

    # Wait for responses
    resps = for reqid <- reqids do
      :ddmon.wait_response_report(reqid, timeout)
    end

    # Check the responses:
    # - If any call resulted in a deadlock, report a deadlock
    # - If any call resulted in a timeout, report a timeout
    # - Otherwise, report results from all calls
    result =
      case Enum.find(resps, fn
            {:"$ddmon_deadlock_spread", _} -> true
            _ -> false
          end)
      do
      {:"$ddmon_deadlock_spread", dl} ->
        {:deadlock, dl}
      _ ->
        if Enum.member?(resps, :timeout) do
          :timeout
        else
          resps = for {:reply, resp} <- resps, do: resp
          {:success, resps}
        end
    end

    # Make it pretty
    print_result(result)
  end

  defp print_result(result) do
    case result do
      {:deadlock, dl} ->
        IO.puts("\e[31;1mDeadlock\e[0m:")
        dl = for p <- dl do
          case Registry.keys(:factory, p) do
            [] -> p
            [name|_] -> name
          end
        end

        duplicates = dl
        |> Enum.group_by(& &1)
        |> Enum.filter(fn {_k, v} -> length(v) > 1 end)
        |> Enum.map(fn {k, _v} -> k end)

        for p <- dl do
          case Enum.member?(duplicates, p) do
            true -> IO.puts "- \e[31;1m#{inspect p} <==\e[0m"
            false -> IO.puts "- #{inspect p}"
          end
        end
      {:success, resps} ->
        IO.puts("\e[32;1mSuccess\e[0m: got #{inspect resps}")
      :timeout ->
        IO.puts("\e[33;1mTimeout\e[0m")
    end
  end
end
