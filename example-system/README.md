# The Microchip Factory

This is an example enterprise gen_server-based application that simulates
well-audited microchip production.

## Project structure

- `lib/`
  - `microchip_factory.ex` — script executing the experiment
  - `microchip_factory/`
    - `application.ex` — Elixir app and supervisor
    - `producer.ex` — the producer generic server
    - `inspector` — the inspector generic server
- `src/` — DDMon library

## Services

Both services are implemented as simple generic servers.

### Producer

`Producer` produces objects (representing real microchip) according to some
metadata which it was provided with during initialisation. Before a microchip is
returned to the caller, it must be first inspected by an `Inspector` --- only if
it passes its meticulous audit, it can be returned to the caller.

Aside from producing microchip, `Producer` may also provide the metadata of its
produce.

### Inspector

`Inspector` checks compliance of microchips against metadata of a reference microchip
producer. In order to perform the audit, the inspector asks its reference
producer for metadata and replies `ok` when certain conditions are met (in our
scenario they are always met). Different inspectors may use different producers
for their references.

## Test case

The file `lib/microchip_factory.ex` describes a case of two producers and two
inspectors. `Producer` 1 asks `Inspector` 1 for audits; similarly, `Producer` 2
asks `Inspector` 2. However, `Inspector` 1 uses `Producer` **2** as its
reference; analogously, `Inspector` 2 refers to `Producer` 1 in its audits.

The test begins by requesting both producers to produce microchip. This, depending
on the order of events, may result in either successful creation of two
vegetables, or a deadlock.
