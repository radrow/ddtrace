# ACM Artifact Review and Badging

In this file, we outline how we address requirements of each ACM artifact badge
in this project.

## Artifacts Evaluated

### Functional

#### Documented

- [EXAMPLE.md](EXAMPLE.md) documents how to use DDMon to monitor a distributed
  application based on the `gen_server` behaviour. The example can be used as a
  blueprint to monitor other `gen_server`-based applications.
- [SCENARIOS.md](SCENARIOS.md) documents the testing DSL used to test and
  benchmark DDMon.
- [IMPLEMENTATION.md](IMPLEMENTATION.md) describes the implementation details of
  DDMon.

#### Consistent

The results in *Figures 15 and 16* as well as *Listings 2, 3 and 4* in the paper
have been produced according to the instructions in
[EVALUATION.md](EVALUATION.md).

#### Complete

The provided artifact includes all components necessary to reproduce the results
in the paper:

- [EVALUATION.md](EVALUATION.md) shows how to generate plots for *Figures 15 and
  16*, as well as *Listings 2, 3 and 4*.
- [EXAMPLE.md](EXAMPLE.md) shows how DDMon can be used to monitor
  `gen_server`-based applications for deadlocks.

#### Exercisable

Please follow the instructions in [EVALUATION.md](EVALUATION.md) which shows how
to generate plots for *Figures 15 and 16*, as well as *Listings 2, 3 and 4*.

Additionally, note that the experiments produce CSV files (in the `output`
folder) with detailed execution statistics.

### Reusable

- [EXAMPLE.md](EXAMPLE.md) shows how to use DDMon to monitor an example
  distributed application based on the `gen_server` behaviour (see
  [example-system/README.md](example-system/README.md) for further details on
  that application). The example can be used as a blueprint to monitor other
  `gen_server`-based applications. We encourage experimenting with it and trying
  to apply DDMon to other applications.
- [IMPLEMENTATION.md](IMPLEMENTATION.md) describes the design of the DDMon
  internals. DDMon follows standard patterns of OTP application design, using
  standard behaviours in its foundation.

## Artifacts Available

We will release this artifact under the MIT license. We have not attached the
license yet to preserve anonymity.

## Results Validated

### Results Reproduced

Please follow the instructions in [EVALUATION.md](EVALUATION.md).
