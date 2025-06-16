# Ddmon demo

## Usage

Requirements:

- non-ancient OTP/Erlang, ideally `26`
- non-ancient Elixir, ideally `1.14`


### Build prerequisites (optional, Linux/Unix/Mac only)

First, make sure your system has all dependencies for
[Erlang](https://github.com/asdf-vm/asdf-erlang) and
[Elixir](https://github.com/asdf-vm/asdf-elixir) installed. Then, run the
following script to install Erlang and Elixir:

```
source install-otp.sh
```

If you do not have `asdf` in your system, everything should be installed in the
currently visited directory.


### Build and run

Build:

```
make
```

Run:

```
./ddmon SCENARIO_FILE [OPTIONS]
```

### Scenario file format

Erlang conf file with following entries:

```erlang
%%% File entries
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

### Options

See `./ddmon --help` for details. Prefix options with `no-` to disable them,
eg. `--no-trace-mon`.
