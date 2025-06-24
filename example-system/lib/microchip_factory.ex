defmodule MicrochipFactory do

  ### ==========================================================================
  ### Example interactions
  ### ==========================================================================


  @doc"""
  Simple example with two services
  """
  def start_two(monitored \\ false) do
    Registry.start_link(keys: :unique, name: :factory)

    {:ok, _prod1} = MicrochipFactory.Producer.start_link({:via, Registry, {:factory, :prod1}}, 3, [])
    {:ok, _prod2} = MicrochipFactory.Producer.start_link({:via, Registry, {:factory, :prod2}}, 5, [])

    {:ok, _insp1} = MicrochipFactory.Inspector.start_link(
      {:via, Registry, {:factory, :insp1}},
      {:via, Registry, {:factory, :prod1}}
    )
    {:ok, _insp2} = MicrochipFactory.Inspector.start_link(
      {:via, Registry, {:factory, :insp2}},
      {:via, Registry, {:factory, :prod2}}
    )

    calls = [{{:via, Registry, {:factory, :prod1}}, {:via, Registry, {:factory, :insp2}}},
             {{:via, Registry, {:factory, :prod2}}, {:via, Registry, {:factory, :insp1}}}
            ]
    do_calls(calls, monitored, 1000)
  end


  @doc """
  Complex example with many producers and a few inspectors
  """
  def start_many(monitored \\ false) do
    Registry.start_link(keys: :unique, name: :factory)

    # How long a single cascade of calls should be
    session_size = 30
    # At what point a session should clash with another
    session_cut = 26

    # Create Producers
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

    # Create inspectors
    _insps = for sess <- [:a, :b, :c] do
      name = {:via, Registry, {:factory, {:insp, sess}}}

      next_sess = case sess do
                    :a -> :b
                    :b -> :c
                    :c -> :a
                  end

      prod_ref = {:via, Registry, {:factory, {:prod, next_sess, session_cut}}}
      MicrochipFactory.Inspector.start_link(name, prod_ref)
    end

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
    reqids = for {prod, insp} <- calls do
      :timer.sleep(:rand.uniform(80 * length(calls)))

      case monitored do
        :monitored -> :ddmon.send_request_report(prod, {:produce_microchip, insp})
        false -> :ddmon.send_request(prod, {:produce_microchip, insp})
      end
    end

    resps = for reqid <- reqids do
      :gen_server.receive_response(reqid, timeout)
    end

    result = case Enum.find(resps, fn
                   {:reply, {:"$ddmon_deadlock_spread", _}} -> true
                   _ -> false
                 end) do
               {:reply, {:"$ddmon_deadlock_spread", dl}} -> {:deadlock, dl}
               _ ->
                 if Enum.member?(resps, :timeout) do
                   :timeout
                 else
                   resps = for {:reply, resp} <- resps do
                     resp
                   end
                   {:success, resps}
                 end
             end

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
