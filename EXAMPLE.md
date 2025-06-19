

# Using DDMon to monitor a `gen_server`-based application

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
