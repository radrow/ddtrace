# DDMon

## Prerequisites (local execution)

- [OTP/Erlang](https://github.com/asdf-vm/asdf-erlang), at least version `26`
- [Elixir](https://github.com/asdf-vm/asdf-elixir), at least version `1.14`
- Python 3 with numpy, pandas and matplotlib (for plotting benchmark results)

### Build prerequisites (Linux/Unix/Mac only)

Build dependencies for Erlang and Elixir:

- `autoconf`
- `make`
- `libssl-dev`
- `openssl`
- `ncurses`
- `wxWidgets`

To automatically obtain the right versions of Erlang and Elixir, **source** (not
run!) the following script to setup Erlang and Elixir via `asdf`:

```
source install-otp.sh
```

If you do not have `asdf` on your system, everything should be installed in the
current directory. You may need to run this script in every shell session in
order to set up `PATH` correctly.


## OOPSLA benchmark reproduction

To reproduce the results from the paper (*Figures 15* and *16*), simply run the
`./bench.sh` script without parameters (UNIX). Alternatively, you can use
`docker`:

```bash
mkdir -p output
docker build -t ddmon .
docker run --rm -v "$(pwd)/output:/app/output" ddmon
```

The scripts take time to execute all experiments --- on our machines it takes
around an hour to run the full benchmark. The plots shall be found in the
`output` directory as PDF files named accordingly to the figures they represent
(e.g. `output/fig_15_a.pdf` for *Figure 15a*).

Note that the produced plots may look differently than what is in the paper.
This is an inevitable consequence of the non-deterministic nature of distributed
systems, which are subject to data races. Different variants of *Figure 16* might
be previewed by running


```bash
python python/trace_log.py -t output/$DIR/ts_p$DELAY/$FILE.csv
```

Where `$DIR` is the timestamp of the experiment, `$DELAY` is the probe delay
setting (`-1` for no delay) and `$FILE` is any file in that directory. For
example (probe delay set to 5000ms):

```bash
python python/trace_log.py -t output/17-06-2025--12:54:05/ts_p5000/conditional__1000__19150746.csv
```

## `ddmon` as library

`ddmon` serves as a drop-in replacement for generic servers. It is applied by
replacing all references to `gen_server` or `GenServer` modules with `ddmon`. An
example can be found in `lib/test_server.ex`, where this is achieved in a single
line via an `alias`:

```elixir
defmodule Ddmon.TestServer do
  use GenServer
  alias :ddmon, as: GenServer  # Comment this line to switch off monitoring
```

Once every generic server in the system is modified this way, the system is
monitored. Deadlocks will be signaled by responses of form
`{'$ddmon_deadlock_spread', L}` where `L` is a list of mutually dependent
processes involved in a deadlock.

### Limitations

The tool supports only standard features of generic servers, i.e. the `call` and
`cast` callbacks. Timeouts, deferred responses (`no_reply`) and pooled calls
through `reqids` are not covered by the prototype.

## Running experiments and testing

`ddmon` comes with a testing platform that allows simulating scenarios of nested
RPC sessions. After each run, an elaborate and readable log is produced. This is
especially useful in replicating and analysing known deadlocks, as well as
investigating potential data races on which said deadlocks depend.

### Build and run

Build:

```bash
make
```

Run:

```bash
./ddmon SCENARIO_FILE [OPTIONS]
```

Run `./ddmon --help` for details on option. Prefix options with `no-` to disable
them, eg. `--no-trace-mon`.

#### Examples

The following executes the scenario pictured in *Figure 7* and discussed in
*Appendix A.1* (*Listings 1* and *2*). Note that due to development of the tool,
the format of the log may differ slightly in formatting. Moreover, since the
deadlock is contingent on data races, it might take a few retries to reproduce
it.

```bash
./ddmon scenarios/envelope.conf
```

The log from *Figure 19* has been obtained from the following:

```bash
./ddmon scenarios/envelope-small.conf
```

### Scenario file format


A scenario is specified as an Erlang `conf` file that schedules communication
between services in an experiment. Each scenario describes a number of parallel
sessions of RPC calls that result in calls to further services, potentially
provoking deadlocks. See *Appendix A.1* of the paper for additional information.

The file roughly follows the following format.

```erlang
%%% The scenario file is consists of a number of entries.
-type config() :: list(entry()).

%%% Top level configuration. For scenario, see below. For all others, see command line options.
-type entry()
   :: scenario()
    | {timeout, non_neg_integer()}
    | {trace_proc, boolean()}
    | {trace_mon, boolean()}
    | {live_log, boolean()}
    | {indent, non_neg_integer()}.

%%% Mandatory. Describes all sessions that are to be executed in parallel.
-type scenario() :: {sessions, list(session())}.

%%% Single session identified as `[Name]`. `[Initial]` is the process id that is
%%% called first when the session is initialized. `[Schedule]` describes further
%%% logic of the session, which shall be executed by `[Initial]`.
-type session() :: {Name :: atom(), {Initial :: proc_id(), Schedule :: schedule()}}.

%%% Description of the computation shape.
%%% - `{E0, E1, ..}` indicates sequential execution within the process, i.e.
%%%   execute `E0`, then `E1` and so on.
%%% - `[I :: proc_id() | S]` makes a blocking call to `I` and orders it to execute `S`
%%% - `{wait, X}` waits for `X` miliseconds
%%% - `{wait, X, Y}` waits randomly between `X` and `Y` miliseconds
%%% - `[]` does nothing
%%% After all actions are executed, the service sends a reply.
-type schedule()
    :: tuple(schedule())
    |  [proc_id() | schedule()]
    |  {wait, non_neg_integer()}
    |  {wait, non_neg_integer(), non_neg_integer()}
    |  []
    |  schedule_sugar()
    .

%%% - `42` as a `schedule()` acts as `[42]`
%%% - `[{wait, X} | S]` (and its variants) act as `{{wait, X}, S}`
-type schedule_sugar()
    :: proc_id()
    |  [{wait, non_neg_integer()} | schedule()]
    |  [{wait, non_neg_integer(), non_neg_integer()} | schedule()]
    .

%%% Services are identified by integers. Can be selected at random from a list.
-type proc_id() :: non_neg_integer() | {random, list(proc_id())}.
```

#### Example

```erlang
{sessions,
 [ {left,
    { 0
    , { 1
      , {wait, 50}
      , 1
      }
    }
   }
 , {rite,
    { 3
    , [2, {wait, 0, 100}, 1, 0]
    }
  }
 ]
}.
```

This starts two independent sessions: `left` and `rite` with 4 services. `left`
begins with a call to the service `0`, which will call `1` for an immediate
reply, wait for 50 milliseconds and then call `1` again. `rite` starts in
parallel with a call to `3` which immediately calls `2`. Upon receiving a
request, `2` will first wait for some time randomly between 0 and 100
milliseconds before calling `1`, which should then call `0`.

A deadlock may occur depending on how long `2` will wait. If it waits long
enough for `left` to terminate, `1` will not be blocked and everything will
resolve by `0` replying to `1`, `1` to `2`, `2` to `3` and `3` finishing the
session. If `2` does not wait, it may block `1` expecting a reply from `0` while
session `left` blocks `0` until `1` is unblocked, implying a deadlock.
