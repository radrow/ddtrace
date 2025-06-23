# Using DDMon to monitor a `gen_server`-based application

DDMon can monitor applications consisting of processes (written in Erlang or
Elixir) based on the generic server (`gen_server`) behaviour. Intuitively, DDMon
acts as a drop-in replacement for the `gen_server` behaviour of the OTP standard
library. At this stage, DDMon supports only the most commonly used features of
generic servers, i.e. the `call` and `cast` callbacks. Timeouts, deferred
responses (`no_reply`) and pooled calls through `reqids` are not covered by the
prototype yet.

The current version of DDMon is instrumented as follows, depending on the
language used to write each `gen_server` instance:

- In the case of Elixir files,  it suffices to add the following line at the top
  of the file, immediately after `use GenServer`:

  ```elixir
  alias :ddmon, as: GenServer
  ```

- In the case of Erlang files, DDMon is instrumented by replacing all references
  to the `gen_server` module with `ddmon`. (This is necessary because Erlang
  lacks the `alias` directive provided by Elixir.)

Below is an example of how DDMon is applied to a simple distributed system. We
provide it not just for its sole evaluation, but also as a simple reference for
custom experiments, which we encourage to try.

## Example: microchip factory

We provide an example Elixir application in the `example-system` directory (see
the [README](example-system/README.md)). The application implements a simple
distributed system where Producers construct values and ask Inspectors to
"validate" their produce before it is returned to the caller. Producers may call
other Producers in order to construct return values to received calls.

We implement two example interactions with this application.

#### Small case

This case includes two Producers (tagged 1 and 2) and two Inspectors (tagged 1
and 2 as well). Inspectors 1 and 2 validate values produced by Producers 1 and 2
respectively by comparing them to metadata provided by Producers 2 and 1
respectively (note that the id numbers are flipped).

There are two scenarios which may nondeterministically occur when the example
application runs, depending on how the calls and responses between `gen_servers`
are scheduled: the run may complete successfully, or it may deadlock.

- **Execution without deadlock:**

  1. Producer 1 receives a call
  2. Producer 1 computes a result asks Inspector 1 for audit
  3. Inspector 1 asks Producer 2 for metadata
  4. Producer 2 replies to Inspector 1
  5. Inspector 1 replies to Producer 1 with its audit
  6. Producer 1 replies to the caller
  7. Producer 2 receives a call
  8. ... (Same as above but flip 1 and 2)

  After the steps above, both calls emitted by the Producers are successfully
  completed, as they both receive a response. When this happens, the application
  will end with a **"Success"** message.

- **Execution with a deadlock:**

  1. Producers 1 and 2 receive calls
  2. Producer 1 asks Inspector 1 for audit
  3. Producer 2 asks Inspector 2 for audit
  4. Inspector 1 asks Producer 2 for metadata
  5. Inspector 2 asks Producer 1 for metadata

  Now, Producer 1 cannot reply to Inspector 2, because Producer 1 is waiting for
  a reply from Inspector 1. Similarly, Producer 2 cannot reply to Inspector 1,
  because Producer 2 is waiting for a reply from Inspector 2. Therefore, a
  deadlock has occurred. When this happens, the application will end with a
  **"Timeout"** message.

To execute the application, run the following command:

```bash
docker run --rm ddmon bash -c 'cd example-system; mix run -e "MicrochipFactory.start_two"'
```

If both Producers' calls receive a response, you should see a green **Success**
message. If a deadlock occurs, you should see a yellow **Timeout** message. You
can repeat the command above multiple times to observe both possible outcomes.

**NOTE:** at this stage the application is *not* monitored yet, and therefore,
the deadlock is not detected. Moreover, since the deadlock is nondeterministic
(due to nondeterministic scheduling by the Erlang VM, and some randomised waits
in the code), you may need to try several times before you can observe both
outcomes described above.

#### Large case

This case illustrates a larger setup with 93 Producers and 3 Inspectors. Here,
deadlocks may involve different services across different executions. To evaluate
it, run the following command:


```bash
docker run --rm ddmon bash -c 'cd example-system; mix run -e "MicrochipFactory.start_many"'
```

Again, several tries may be needed to reproduce deadlocks and successful executions.

### Instrumenting the example `gen_server`s with DDMon

To instrument the example application with DDMon, edit the following files:

- `example-system/lib/microchip_factory/producer.ex`
- `example-system/lib/microchip_factory/inspector.ex`

In each uncomment the *line 3*. For example, `producer.ex` should begin as
follows:

```elixir
defmodule MicrochipFactory.Producer do
  use GenServer
  alias :ddmon, as: GenServer

  def start_link(microchip_metadata) do
    GenServer.start_link(__MODULE__, microchip_metadata, [])
  end

  ...
```

Now, you should rebuild the docker image:

```bash
docker build -t ddmon .
```

Now you can try rerunning the experiment several times (replace `start_two` with
`start_many` to run the large experiment):

```bash
docker run --rm ddmon bash -c 'cd example-system; mix run -e "MicrochipFactory.start_two"'
```

The **Success** output should look exactly as before. However, if the system
deadlocks, you should see a red **Deadlock** message (instead of "Timeout"),
followed by a list of PIDs: those are the PIDs of the processes involved in the
deadlock. Symbol `<==` marks a process that reported the deadlock.
