# DDMon evaluation

This file describes instructions on how to evaluate the artifact.

## Preparation

The project can be built and run locally or in a
[docker](https://www.docker.com/) container.

### Docker

Run the following commands to build the docker image:

```bash
mkdir -p output
docker build -t ddmon .
```

### Local build

Here are instructions on how to set up the project on your host machine. We
however recommend using docker for reproducing results.

Prerequisites:

- [OTP/Erlang](https://www.erlang.org/), at least version `26`
- [Elixir](https://elixir-lang.org/), at least version `1.14`
- [Mix](https://hexdocs.pm/mix/Mix.html)
- [Python 3](https://www.python.org/) with [numpy](https://numpy.org/),
  [pandas](https://pandas.pydata.org/) and [matplotlib](https://matplotlib.org/)
  (for plotting benchmark results)

To build the project, run

```bash
make
```

If you do not have `make`, run the following instead

```bash
mix deps.get
mix escript.build
```

#### Local installation of Erlang and Elixir (optional, Linux/Unix/maybe Mac)

If you do not have Erlang or Elixir installed, we provide a script that
automatically downloads and installs the correct versions locally. Make sure you
have the following build dependencies installed on your system:

- `autoconf`
- `make`
- `libssl-dev`
- `openssl`
- `ncurses`
- `wxWidgets`

To automatically obtain the right versions of Erlang and Elixir, **source** (not
run!) the provided script:

```bash
source install-otp.sh
```

This will use [asdf](https://github.com/asdf-vm/asdf) version manager to install
Erlang and Elixir. If you do not have `asdf` on your system, it shall be
installed in the currently visited directory. You may need to run this script in
every shell session in order to set up `PATH` correctly.

## "Kick the tire"

The following instructions assume that you have built the docker image as
described above.

### Plot reproduction

Run the following command:

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh small
```

The program should terminate after about a minute and print `Done` at the end.
No stacktraces (red walls of gibberish text) should be printed --- if that is
the case, please send us the entire output.

After the program has finished, the `output` folder should contain PDF files,
(e.g. `figure_16_a.pdf`) which should contain line plots (their shape does not
matter yet).

### Scenario experiment

Run the following command:

```bash
docker run --rm ddmon ./ddmon scenarios/supersimple.conf
```

The output should look more-or-less like

```
Node: ddmon@32063e08d1f7
Timeout: 2010ms
Sessions: s
00:000:030	| M0:	I0 ? Q(s)
00:000:058	| M0:	%[ Q(s) ]
00:000:079	| M0:	P0 ! Q(s)
00:010:567	| M0:	 ? R(s)
00:010:590	| M0:	%[ ?! R(s) ]
00:010:601	| M0:	I0 ! R(s)
Registered 6 events:
	2	messages sent
	1	queries
	1	replies
	0	probes
	2	mque picks
Directions:
	1	involving Init
	0	Mon -> Mon
	0	Mon -> Proc
	1	Proc -> Mon
	0	Proc -> Proc
State changes:
	0	unlocks
	0	locks
	0	deadlocks
Time: 10570973

### TERMINATED ###
```

## Reproducing figures

### Plots

To reproduce *Figures 15 and 16* run the following command:

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh
```

**Note:** it may take about an hour to finish.

After that, the following figures should be generated:

- *Figure 15a*: `output/figure_15_a.pdf`
- *Figure 15b*: `output/figure_15_b.pdf`
- *Figure 15c*: `output/figure_15_c.pdf`
- *Figure 16a*: `output/figure_16_a.pdf`
- *Figure 16b*: `output/figure_16_b.pdf`
- *Figure 16c*: `output/figure_16_c.pdf`

Note that the produced plots may look differently than what is in the paper.
This is an inevitable consequence of the non-deterministic nature of distributed
systems, which are subject to data races. This is especially likely for *Figure
16*, where we manually selected runs that resulted in deadlocks to show
communication overhead in case of a deadlock. If you want to see plots for
*Figure 16* from other runs

All variants of *Figure 16*  might be previewed by running

```bash
DELAY=5000
ls -1dt output/*/ | head -n1 | xargs -I DIR find DIR/ts_p$DELAY/ -type f | xargs -n1 python python/trace_
log.py -t
```

Where `DELAY` can be set to `-1` (no probe delay), `1000` or `5000` (1000ms and
5000ms of probe delay respectively).

### Simulation logs

The following command executes the scenario pictured in *Figure 7* and discussed
in *Appendix A.1* (*Listings 1* and *2*). Note that due to development of the
tool, the format of the log may differ slightly in formatting. Moreover, since
the deadlock is contingent on data races, it might take a few retries to
reproduce it.

```bash
docker run --rm ddmon ./ddmon scenarios/envelope.conf
```

The log from *Figure 19* has been obtained from the following:

```bash
docker run --rm ddmon ./ddmon scenarios/envelope-small.conf
```

Again, several runs might be needed to obtain each of the two possible outcomes
("terminated" and "deadlock").

If the output is distorted, make sure you run it in a maximised terminal window
with small font. If that does not suffice, you can make the output flat by
adding `--indent=0` at the very end of each of the commands above.


## Running simulations

`ddmon` comes with a testing platform for reproducing various (dead)lock
scenarios in gen_server-based systems. Such scenarios are specified in Erlang
`.conf` files and describe schedules of exchanged calls. We provide a bunch of
scenarios in the `scenarios` directory. To evaluate a scenario `scenarios/example.conf`, run

```bash
docker run --rm ddmon ./ddmon scenarios/example.conf
```

### Overview of provided scenarios

#### `supersimple`

Mock-test with only one service that just replies to the initial call.

```erlang
{sessions, [ {s, {0, [{wait, 10}]}} ] }.
```

#### `deadlock`

Certain deadlock where service `0` calls service `1`, `1` calls `2` and `2` calls `0` making a loop.

```erlang
{sessions,
 [ {s, { 0
       , [ 1, 2, 0]
       }
   }
 ]
}.
```

#### `nodeadlock`

Certain termination (i.e. no deadlock), where two parallel sessions finish
successfully:

- Service `0` calls `1`, `1` calls `2`, `2` replies to `1`, `1` replies to `0`
- Service `4` calls `3`, `3` calls `0` (optionally waiting for it to receive reply from `1`), then `0` replies to `3`, and `3` replies to `4`

```erlang
{sessions,
 [ {left, { 0
          , [1, 2]
          }
   }
 , {rite, { 4
          , [3, 0]
          }
   }
 ]
}.
```

#### `random`

Approximately 50% chances to deadlock or not.

This starts two independent sessions: `left` and `rite` with 3 services. `left`
begins with a call to the service `0`, which will wait for 50 milliseconds and
then call `1`, which then should reply immediately. `rite` starts in parallel
with a call to `2` which waits for some time randomly between 0 and 100
milliseconds before calling `1`, which should then call `0`.

A deadlock may occur depending on how long `2` will wait. If it waits long
enough for `left` to terminate, `1` will not be blocked and everything will
resolve by `0` replying to `1`, `1` to `2`, and `2` finishing the session. If
`2` does not wait long enough, it may block `1` expecting a reply from `0` while
session `left` blocks `0` until `1` is unblocked, implying a deadlock.

```erlang
{sessions,
 [ {left, { 0
          , [{wait, 50}, 1]
          }
   }
 , {rite, { 2
          , [{wait, 0, 100}, 1, 0]
          }
   }
 ]
}.
```

#### `example`

A more comprehensive example, similar to `random`.

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

#### `routing`

An example of a replicated service implemented as in *Appendix B*. Here, service
`0` has two workers (`{router, {0, 2}}`). The session begins with service `1`
calling `0`, which then calls service `2`, which calls `0` again. Normally, this
would cause a dependency cycle, but because `0` schedules tasks between two
workers, deadlock is avoided.

```erlang
{router, {0, 2}}.

{sessions,
 [ {left,
    { 1
    , [0, 2, 0]
    }
   }
 ]
}.
```

## Applying `ddmon` to `gen_server`-based systems

In Erlang applications, `ddmon` is applied by replacing all references to the
`gen_server` module with `ddmon`. In Elixir, it suffices to add the following
line right after `use GenServer`:

```elixir
alias :ddmon, as: GenServer
```

The tool supports only standard features of generic servers, i.e. the `call` and
`cast` callbacks. Timeouts, deferred responses (`no_reply`) and pooled calls
through `reqids` are not covered by the prototype.

Below is an example of how `ddmon` is applied to a simple distributed system. We
provide it not just for its sole evaluation, but also as a simple reference for
custom experiments, which we encourage.

### Example: turnip factory

We provide an example Elixir application in the `example-system` directory. The
application implements a simple distributed system where two Producers (tagged 1
and 2) produce turnip and ask Inspectors (tagged 1 and 2 respectively) to
"validate" their produce before it is returned to the caller. Inspectors 1 and 2
validate turnip by comparing it to metadata provided by Producers 2 and 1
respectively (note the difference in order).

There are two scenarios which may occur in this setup:

#### No deadlock

1. Producer 1 receives a call
2. Producer 1 computes a result asks Inspector 1 for audit
3. Inspector 1 asks Producer 2 for metadata
4. Producer 2 replies to Inspector 1
5. Inspector 1 replies to Producer 1 with its audit
6. Producer 1 replies to the caller
7. Producer 2 receives a call
8. ...Same story but flip 1 and 2

Both calls result with a value returned to the caller.

#### Deadlock

1. Producers 1 and 2 receive calls
2. Producer 1 asks Inspector 1 for audit
3. Producer 2 asks Inspector 2 for audit
4. Inspector 1 asks Producer 2 for metadata
5. Inspector 2 asks Producer 1 for metadata

Now, Producer 1 cannot reply to Inspector 2, because it is waiting for a reply
from Inspector 1. Similarly, Producer 2 cannot reply to Inspector 1, because it
is waiting for a reply from Inspector 2. Therefore deadlock has occurred.

#### Running the example

To execute the presented setup, run the following command:

```bash
docker run --rm ddmon bash -c 'cd example-system; mix run -e "TurnipFactory.start_all"'
```

If both calls terminate successfully, you should see a green **Success**
message. If a deadlock occurs, the message shall say **Timeout** in yellow. Note
that the system is *not* monitored yet, thus the deadlock is not detected. Since
the deadlock is subject to a data race (via randomised waits), you may need to
try several times before you reproduce both results.

#### Applying `ddmon`

To apply `ddmon` edit the following files:

- `example-system/lib/turnip_factory/producer.ex`
- `example-system/lib/turnip_factory/inspector.ex`

In each uncomment the *line 3*. For example, `producer.ex` should begin as
follows:

```elixir
defmodule TurnipFactory.Producer do
  use GenServer
  alias :ddmon, as: GenServer

  def start_link(turnip_metadata) do
    GenServer.start_link(__MODULE__, turnip_metadata, [])
  end

...
```

Now, rebuild the docker image:

```bash
docker build -t ddmon .
```

Rerun the experiment several times:

```bash
docker run --rm ddmon bash -c 'cd example-system; mix run -e "TurnipFactory.start_all"'
```

The **Success** output should look exactly as before. However, if the system
deadlocks, you should see a red **Deadlock** message (instead of "Timeout"),
followed by a list of PIDs of the processes involved in the deadlock.
