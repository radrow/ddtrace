# DDMon

## Evaluation

__To evaluate the OOPSLA artifact, please move to [EVALUATION.md](EVALUATION.md).__

## Local build

Here are instructions for building and running the project locally. You should
not need to do it to evaluate the artifact — instead, follow the instructions in
[EVALUATION.md](EVALUATION.md) which guide through a docker setup.

---

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

### Local installation of Erlang and Elixir (Linux/Unix/maybe Mac)

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

## Application

DDMon serves as a drop-in replacement for generic servers. To use it, include
the contents of the `src` directory in your project and replace all references
to `gen_server` or `GenServer` with `ddmon`.

## Testing

After building the project, run

```bash
./ddmon FILE
```

Where `FILE` is a file describing the test scenario. For tweaking information,
see `./ddmon --help`.

See [SCENARIOS.md](SCENARIOS.md) for instructions on how to program custom tests.
