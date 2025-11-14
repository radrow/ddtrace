defmodule DDTrace.Main do
  defp usage() do
    """
    Usage: #{Path.basename(__ENV__.file)} FILE [OPTIONS]
    Options:
    \t--timeout\tSets timeout for the simulation. If not provided or set to 0, it is estimated by the script.
    \t--trace-proc\tTrace events in monitored services
    \t--trace-mon\tTrace events in monitored services (default)
    \t--trace-int\tTrace internal events (default)
    \t--live-log\tPrint logs from the tracer instead of retroactively
    \t--log-timestamp\tInclude time in the printed log
    \t--indent\tHow many tabs should log cells be separated with; 0 means no tabulation (default 4)
    \t--csv\tOutput log file in CSV format
    \t--stats-csv\tOutput stats file in CSV format
    \t--seed\tRNG seed (default unset)
    \t--silent\tDo not print the trace (default unset)
    \t--probe-delay\tWhether to send probes with a delay. 0 means no delay, but asynchronous. -1 (default) means no delay.
    \t--unmonitored\tRuns services as regular generic servers
    \t--ddmon\tUses ddmon for cross-check
    Prefix options with "no-" to disable them, eg. "--no-trace-mon"
    """
  end

  defp err(msg, return_code \\ 1) do
    IO.puts(:stderr, msg)
    IO.puts(:stderr, "")
    IO.puts(:stderr, usage())
    System.halt(return_code)
  end

  defp parse_args() do
    args = System.argv()
    switches = [trace_proc: :boolean,
                trace_mon: :boolean,
                trace_int: :boolean,
                live_log: :boolean,
                log_timestamp: :boolean,
                help: :boolean,
                timeout: :integer,
                indent: :integer,
                csv: :string,
                stats_csv: :string,
                seed: :integer,
                probe_delay: :integer,
                silent: :boolean,
                unmonitored: :boolean,
                ddmon: :boolean
               ]
    {opts, args, bad} = OptionParser.parse(args, strict: switches)

    if Keyword.get(opts, :help, false) do
      IO.puts(usage())
      System.halt(0)
    end

    case {bad, args} do
      {[], [file]} -> {file, opts}
      {[], _} ->
        err("Error: scenario file not provided.")
      {[{opt, :nil}|_], _} ->
        err("Unrecognized option: #{opt}")
      {[{opt, val}|_], _} ->
        err("Invalid value #{opt}=#{val}")
    end
  end

  def main(_args) do
    {file, opts} = parse_args()
    :scenario.run(file, opts)
  end
end
