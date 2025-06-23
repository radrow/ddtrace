defmodule MicrochipFactory do

  def start_two do
    {:ok, _prod1} = MicrochipFactory.Producer.start_link(3, [], :prod1)
    {:ok, _prod2} = MicrochipFactory.Producer.start_link(5, [], :prod2)

    {:ok, _insp1} = MicrochipFactory.Inspector.start_link(:prod1, :insp1)
    {:ok, _insp2} = MicrochipFactory.Inspector.start_link(:prod2, :insp2)

    reqid1 = :gen_server.send_request(:prod1, {:produce_microchip, :insp2})
    :timer.sleep(:rand.uniform(500))
    reqid2 = :gen_server.send_request(:prod2, {:produce_microchip, :insp1})

    resp1 = :gen_server.receive_response(reqid1, 1000)
    resp2 = :gen_server.receive_response(reqid2, 1000)

    result = case {resp1, resp2} do
               {{:reply, {:"$ddmon_deadlock_spread", dl}}, _} -> {:deadlock, dl}
               {_, {:reply, {:"$ddmon_deadlock_spread", dl}}} -> {:deadlock, dl}
               {{:reply, resp1}, {:reply, resp2}} -> {:success, resp1, resp2}
               {:timeout, _} -> :timeout
               {_, :timeout} -> :timeout
             end

    case result do
      {:deadlock, dl} ->
        IO.puts("\e[31;1mDeadlock\e[0m: #{inspect dl}")
      {:success, resp1, resp2} ->
        IO.puts("\e[32;1mSuccess\e[0m: got #{inspect resp1}, #{inspect resp2}")
      :timeout ->
        IO.puts("\e[33;1mTimeout\e[0m")
    end
  end

  def start_many do
    Registry.start_link(keys: :unique, name: :factory)

    # How long a single cascade of calls should be
    session_size = 30
    # At what point a session should clash with another
    session_cut = 25

    # Create Producers
    prods = for idx <- 0..session_len, sess <- [:a, :b, :c], into: %{} do
      name = {:via, Registry, {:factory, {:prod, sess, idx}}}
      prod = MicrochipFactory.Producer.start_link(21, name)
      {{sess, idx}, prod}
    end

    # Create inspectors
    insps = for idx <- 0..session_len, sess <- [:a, :b, :c], into: %{} do
      {next_sess, next_idx} =
        case idx == session_len do
          true ->
            # If we reach the end of a stream, attack
            next_sess = case sess do
                          :a -> :b
                          :b -> :c
                          :c -> :a
                        end
            {next_sess, session_cut}
          false ->
            {sess, idx + 1}
        end
      prod = {:via, Registry, {:factory, {:prod, next_sess, next_idx}}}
      name = {:via, Registry, {:factory, {:insp, sess, idx}}}
      insp = MicrochipFactory.Inspector.start_link(prod, name)
    end
  end

end
