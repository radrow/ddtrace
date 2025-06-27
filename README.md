# DDMon

DDMon is a deadlock monitoring tool for Erlang and Elixir programs based on the
`gen_server` behaviour.

This document contains prerequisites and instructions for building, using, and
evaluating DDMon.

**NOTE:** To evaluate the OOPSLA'25 artifact, you can use a Docker-based setup
for DDMon. In this case, you can just follow the instructions in the following
files, and skip the rest of this file.

- [EVALUATION.md](EVALUATION.md) for setting up the Docker image, "kicking the
  tires," and reproducing the plots and listings in the companion paper.
- [EXAMPLE.md](EXAMPLE.md) to see how DDMon can be used for monitoring
  applications based on the `gen_server` behaviour.
- [SCENARIOS.md](SCENARIOS.md) for the documentation of the testing DSL used in
  the paper to benchmark DDMon to various randomised scenarios of varying size.
- [IMPLEMENTATION.md](IMPLEMENTATION.md) for details about the DDMon
  implementation and how it connects to the theory and results in the companion
  paper.
- [BADGING.md](BADGING.md) contains overview on how we address requirements for
each ACM artifact badge.


**NOTE:** The following build instructions are tested on GNU/Linux (Ubuntu 24.04
and 25.04, and Fedora 42) and macOS.

## Build prerequisites

- [Erlang/OTP](https://www.erlang.org/), version `26` or higher
- [Elixir](https://elixir-lang.org/), version `1.14` or higher
- [Mix](https://hexdocs.pm/mix/Mix.html)
- [Python 3](https://www.python.org/) with [numpy](https://numpy.org/) (at least
  2.2), [pandas](https://pandas.pydata.org/) (at least 2.2) and
  [matplotlib](https://matplotlib.org/) (at least 3.10) --- for plotting
  benchmark results


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

To create a local build of DDMon, run:

```bash
make
```

If you do not have `make`, you can run the following instead:

```bash
mix deps.get
mix escript.build
```


## Evaluating the OOPSLA'25 artifact

After building DDMon, you can follow the instructions and documentation in the
files listed at the beginning of this document, starting with
[EVALUATION.md](EVALUATION.md) --- except that you should **remove the
Docker-related part of each command**. For example, if
[EVALUATION.md](EVALUATION.md) asks you to run:

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh small
```

then you should run instead:

```bash
./bench.sh small
```
