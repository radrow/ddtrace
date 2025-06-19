# DDMon artifact evaluation

This document contains instructions on how to evaluate the DDMon artifact
running within a [docker](https://www.docker.com/) container.

As an alternative, similar commands can be executed locally, after performing a
local installation as described in [README.md](README.md).


## Preparation

For this evaluation, we recommend building and running DDMon within a
[docker](https://www.docker.com/) container. To build the docker image, run the
following commands:

```bash
mkdir -p output         # Creates a directory for the plots and other data
docker build -t ddmon . # Creates a Docker image called 'ddmon'
```

(Note: the creation of the Docker image might take several minutes.)

## "Kick the tires"

The following instructions assess whether DDMon is working as intended within
the Docker container created above. They check two features: the generation of
PDF files containing plots, and the execution of test scenarios.


### Plot generation

Run the following command:

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh small
```

While the program runs, you should see several legends and animated diagnostics
looking as follows:

```none
Experiment status legend:
 . Waiting | o Preparing | O Working | @ Success | D Deadlock | ! Crash | T Timeout
[ @ @ D @ @ @ @ @ D D D @ D D D @ D D D @ ]
```

The command should terminate after about a minute and print `Done` at the end.

No stack trace (i.e., a wall of red text) should be printed --- otherwise,
please send us the entire output. If you run into permission issues, please
delete the `output` folder and try again.

After the program has finished, the `output` folder should contain PDF files
with various plots (e.g. `figure_16_a.pdf`).

Please try visualising such PDF files with a PDF reader: if the files contain
some sort of plot (the plotted values are not important), then this step of the
"Kick the tires" assessment is completed.


### Test scenarios

Run the following command:

```bash
docker run --rm ddmon ./ddmon scenarios/supersimple.conf
```

The output should resemble the following:

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

- It should end with the line `### TERMINATED ###`.
- It should not contain stack traces nor error messages --- otherwise, please
  send us the entire output.
- Time measurements (e.g. `00:010:601` or `Time: 10570973`) may be different.


## Reproducing the results from the paper

### Reproducing the plots

To reproduce *Figures 15 and 16* in the paper, please run the following command.


```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh
```

**Note:** the command above may take about one hour to finish (tested on Fedora
42 running on 11th Gen Intel Core i7-1185G7 with **32GB of RAM**). For a less
resource-intensive variant, run the command below instead (this one takes about
10 minutes on the same machine). Note that we used the command above to obtain
our results, thus the outputs of the following command might be less similar to
the figures in the paper.

```bash
docker run --rm -v "$(pwd)/output:/app/output" ddmon ./bench.sh medium
```

After that, the following PDF files should be generated:

- *Figure 15a*: `output/figure_15_a.pdf`
- *Figure 15b*: `output/figure_15_b.pdf`
- *Figure 15c*: `output/figure_15_c.pdf`
- *Figure 16a*: `output/figure_16_a/*.pdf` (many files)
- *Figure 16b*: `output/figure_16_b/*.pdf` (many files)
- *Figure 16c*: `output/figure_16_c/*.pdf` (many files)

Note that the produced plots may look different w.r.t. those in the paper. This
is because the benchmarks perform multiple executions of concurrent systems, and
each execution may or may not deadlock at a certain time, depending on (1)
intrinsic nondeterminism in their behaviour, and (2) further nondeterminism
introduced by scheduling (similarly to the non-deterministic deadlock
illustrated in Example 3.10 in the paper). More specifically:

#### Figure 15

The produced plot may be slightly different w.r.t. the paper, but the overall
trends should be the same:

- In *Figures 15a, 15b and 15c*, the orange line should show values greater than
  blue and green, while red should be above orange.
- In *Figures 15a and 15b*, the blue line should present roughly the same values
  as the green one.
- In *Figure 15b*, the blue and green lines should be close to zero.
- In *Figure 15c*, the blue line should show values approximately 3 times lower
  than other lines.

#### Figure 16

The figure in the paper visualises one specific execution per probe emission
delay (none, 1000ms, or 5000ms). To produce the figure we manually selected 3
executions that clearly show how many queries, responses, and probes may be
emitted, and when, in case of a deadlock. The benchmark script runs numerous
experiments for each delay, thus yields many candidates for Figures *16a*, *16b*
and *16c*. The following trends should be observed:

- In variants of *Figure 16a*, the red solid line rises early and often above
  the dashed cyan one.
- In variants of *Figure 16b* and *Figure 16c*, the orange and green solid lines
  are generally below both dashed cyan and dotted blue lines.
- In variants of *Figure 16c*, the green solid line should be close to zero
  unless a red dashed vertical line (indicating a deadlock) is present.

In each plot, there may be any number of deadlocks marked by red dashed vertical
lines. Specifically, it is not necessary that *Figure 16a and 16b* do not have
such a line, while *Figure 16c* has (as in the figures in the paper).

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

# Using DDMon to monitor a `gen_server`-based application

Please see the instructions in [EXAMPLE.md](EXAMPLE.md).
