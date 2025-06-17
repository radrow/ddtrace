defmodule TurnipFactory do
  def start_all do

    {:ok, prod1} = TurnipFactory.Producer.start_link(3)
    {:ok, prod2} = TurnipFactory.Producer.start_link(5)

    {:ok, insp1} = TurnipFactory.Inspector.start_link(prod1)
    {:ok, insp2} = TurnipFactory.Inspector.start_link(prod2)

    reqid1 = :gen_server.send_request(prod1, {:produce_turnip, insp2})
    :timer.sleep(:rand.uniform(500))
    reqid2 = :gen_server.send_request(prod2, {:produce_turnip, insp1})

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
end
