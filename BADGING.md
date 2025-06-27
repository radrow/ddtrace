# ACM Artifact Review and Badging

In this file, we outline how we address requirements of each ACM artifact badge
in this project.

## Artifacts Evaluated

### Functional

#### Documented

- [SCENARIOS.md](SCENARIOS.md) describes how to build and run the project
- [IMPLEMENTATION.md](IMPLEMENTATION.md) guides through the implementation details
- [EXAMPLE.md](EXAMPLE.md) shows how to apply DDMon to a distributed system

#### Consistent

The results produced in [EVALUATION.md](EVALUATION.md) have been directly used
as *Figures 15 and 16* as well as *Listings 2, 3 and 4*. The outputs should
not differ significantly between the various experiments.

Note that certain results cannot be replicated exactly, or may require a number
of retries. This is due to arbitrary scheduling, which is an inherent feature of
distributed systems. In fact, some of our tests produce variable outputs on
purpose, to induce a wider range of potential program behaviours, taking
non-determinism into account.

#### Complete

The provided artifact is a standalone device.

- [EXAMPLE.md](EXAMPLE.md) shows how it can be used to monitor
  `gen_server`s for deadlocks.
- [EVALUATION.md](EVALUATION.md) shows how to generate plots for *Figures 15 and
  16*, as well as *Listings 2, 3 and 4*.

#### Exercisable

Please follow the instructions in [EVALUATION.md](EVALUATION.md) which shows how
to generate plots for *Figures 15 and 16*, as well as *Listings 2, 3 and 4*.

Additionally, note that the experiments produce CSV files (in the `output`
folder) with detailed execution statistics.

### Reusable

- [EXAMPLE.md](EXAMPLE.md) shows how to use DDMon to monitor an example
  distributed system (see [example-system/README.md](example-system/README.md)).
  The example is not tailored to be used with DDMon --- the compatibility comes
  just from the fact that it is a `gen_server`-based system. We encourage
  experimenting with it and trying to apply DDMon to custom-made networks of
  generic servers.
- [IMPLEMENTATION.md](IMPLEMENTATION.md) describes the design of the monitor.
  DDMon follows standard patterns of OTP application design, using standard
  behaviours in its foundation.

## Artifacts Available

We will release the artifact under MIT license and host it on our facility's
public repository. We have not attached the license yet to preserve anonymity.

## Results Validated

### Reproduced

Please follow the instructions in [EVALUATION.md](EVALUATION.md).

### Replicated

We encourage implementing the algorithm. A minimal implementation can be rather
simple, especially if it does not aim to preserve wide compatibility with
`gen_server` API and telemetry.
