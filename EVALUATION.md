# DDMon evaluation

This file describes instructions on how to evaluate the artifact.

## Preparation

We recommend building and running the project in a
[docker](https://www.docker.com/) container. To build the docker image, run the
following commands:

```bash
mkdir -p output
docker build -t ddmon .
```

## "Kick the tires"

The following instructions test the overall setup. Please let us know if you
encounter any issues.

### Plot reproduction

Run the following command:

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh small
```

The program should terminate after about a minute and print `Done` at the end.
No stacktrace (wall of red text) should be printed --- if that is the case,
please send us the entire output. If you run into permission issues, delete the
`output` folder and try again.

After the program has finished, the `output` folder should contain PDF files
(e.g. `figure_16_a.pdf`) with line plots (their shape does not matter yet).

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

## Reproducing the results from the paper

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
*Figure 16* from other executions, run

```bash
DELAY=5000
ls -1dt output/*/ | head -n1 | xargs -I DIR find DIR/ts_p$DELAY/ -type f | xargs -n1 python python/trace_log.py -t
```

Where `DELAY` can be set to `-1` (no probe delay), `1000` or `5000` (1000ms and
5000ms of probe delay respectively).

### Simulation logs

The following command executes the scenario pictured in *Figure 7* and discussed
in *Appendix A.1* (*Listings 1* and *2*). Note that due to development of the
tool, the format of the log may **slightly differ** in formatting. Moreover,
since the deadlock is contingent on data races, it might **take a few retries**
to reproduce it.

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

DDMon comes with a testing platform for reproducing various (dead)lock scenarios
in gen_server-based systems. Such scenarios are specified in Erlang `.conf`
files and schedule calls exchanged by services in a simulated network. We
provide a bunch of scenarios in the `scenarios` directory. For more information
about this feature, see [SCENARIOS.md](SCENARIOS.md) and *Appendix 1* of the
paper.

To evaluate a scenario `scenarios/example.conf`, run

```bash
docker run --rm ddmon ./ddmon scenarios/example.conf
```


## Applying DDMon to `gen_server`-based systems

In Erlang applications, DDMon is applied by replacing all references to the
`gen_server` module with `ddmon`. In Elixir, it suffices to add the following
line right after `use GenServer` at the top of the file:

```elixir
alias :ddmon, as: GenServer
```

The tool supports only standard features of generic servers, i.e. the `call` and
`cast` callbacks. Timeouts, deferred responses (`no_reply`) and pooled calls
through `reqids` are not covered by the prototype yet.

Below is an example of how DDMon is applied to a simple distributed system. We
provide it not just for its sole evaluation, but also as a simple reference for
custom experiments, which we encourage to try.

### Example: turnip factory

We provide an example Elixir application in the `example-system` directory (see
the [README](example-system/README.md)). The application implements a simple
distributed system where two Producers (tagged 1 and 2) construct values and ask
Inspectors (tagged 1 and 2 respectively) to "validate" their produce before it
is returned to the caller. Inspectors 1 and 2 validate these values by comparing
it to metadata provided by Producers 2 and 1 respectively (note that the id
numbers are flipped).

There are two scenarios which may occur in this setup:

##### No deadlock

1. Producer 1 receives a call
2. Producer 1 computes a result asks Inspector 1 for audit
3. Inspector 1 asks Producer 2 for metadata
4. Producer 2 replies to Inspector 1
5. Inspector 1 replies to Producer 1 with its audit
6. Producer 1 replies to the caller
7. Producer 2 receives a call
8. ...Same story but flip 1 and 2

Both calls result with a value returned to the caller.

##### Deadlock

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

#### Applying DDMon

To apply DDMon to the project, edit the following files:

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
