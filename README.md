# DDMon

DDMon is a deadlock monitoring tool for Erlang and Elixir programs based on the
`gen_server` behaviour.

This document contains prerequisites and instructions for building and running
DDMon.

**NOTE:** To evaluate the OOPSLA'25 artifact, you can use a Docker-based setup
for DDMon, instead of performing a local build. In this case, you can skip this
file and move directly to the instructions in the following files:

- [EVALUATION.md](EVALUATION.md) for setting up the Docker image and reproducing
  the plots and listings in the companion paper.
- [EXAMPLE.md](EXAMPLE.md) to see how DDMon can be applied to examples beyond
  those in the companion paper.
- [SCENARIOS.md](SCENARIOS.md) for the documentation of the testing DSL used in
  the paper to benchmark DDMon to various randomised scenarios of varying size.
- [IMPLEMENTATION.md](IMPLEMENTATION.md) for details about the DDMon
  implementation and how it connects to the theory and results in the companion
  paper.

**NOTE:** The following instructions are tested on GNU/Linux (Ubuntu 24.04 and
Fedora 42).


## Prerequisites

- [Erlang/OTP](https://www.erlang.org/), version `26` or higher
- [Elixir](https://elixir-lang.org/), version `1.14` or higher
- [Mix](https://hexdocs.pm/mix/Mix.html)
- [Python 3](https://www.python.org/) with [numpy](https://numpy.org/),
  [pandas](https://pandas.pydata.org/) and [matplotlib](https://matplotlib.org/)
  (for plotting benchmark results)


### Optional: script for a fresh local installation of Erlang and Elixir

If you do not have Erlang or Elixir installed, we provide a script that
automatically downloads and installs the correct versions. Make sure you have
the following build dependencies installed on your system:

- `autoconf`
- `make`
- `libssl-dev`
- `openssl`
- `ncurses`
- `wxWidgets`

To automatically obtain the right versions of Erlang and Elixir, **source** (not
run!) the provided script in `bash`:

```bash
source install-otp.sh
```

This will use [asdf](https://github.com/asdf-vm/asdf) version manager to install
Erlang and Elixir. If you do not have `asdf` on your system, it shall be
installed in the currently visited directory. You may need to run this script in
every shell session in order to set up `PATH` correctly.


## Building DDMon

To build DDMon, run

```bash
make
```

If you do not have `make`, you can run the following instead

```bash
mix deps.get
mix escript.build
```


## Usage

DDMon serves as a drop-in replacement for Erlang and Elixir's generic server
behaviour (`gen_server`). To use it, include the contents of the [src/](src/)
directory in your project and replace all references to `gen_server` or
`GenServer` with `ddmon`.

For more details, please see [EXAMPLE.md](EXAMPLE.md).


## Testing

After building the project, you can run:

```bash
./ddmon SCENARIO_FILE
```

Where `SCENARIO_FILE` is a file describing the test scenario. For tweaking
information, see `./ddmon --help`.

See [SCENARIOS.md](SCENARIOS.md) for the documentation on how the DDMon scenario
tests work, and instructions on how to define new test scenarios.

## Evaluating the artifact

Now, you can move to [EVALUATION.md](EVALUATION.md) which contains instructions
on how to evaluate the artifact. Note that the instructions are for docker
build. To run the provided commands using a local build, remove the docker part:
for example

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh small
```

should become

```bash
./bench.sh small
```

