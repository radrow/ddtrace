# The Turnip Factory

This is an example enterprise gen_server-based application that simulates
well-audited turnip production.

## Project structure

- `lib/`
  - `turnip_factory.ex` — script executing the experiment
  - `turnip_factory/`
    - `application.ex` — Elixir app and supervisor
    - `producer.ex` — the producer generic server
    - `inspector` — the inspector generic server
- `src/` — DDMon library

## Services

### Producer

`Producer` produces objects (representing real turnip) according to some
metadata which it was provided with during initialisation. Before a turnip is
returned to the caller, it must be first inspected by an `Inspector` --- only if
it passes its opaque audit, it can be returned to the caller.

Aside from producing turnip, `Producer` may also provide the metadata of its
produce.

### Inspector

`Inspector` checks compliance of turnips against metadata of a reference turnip
`Producer`. In order to perform the audit, the inspector asks its reference
producer for metadata and replies `ok` when certain conditions are met (in our
scenario they are always met). Different inspectors may use different producers
for their references.

## Test case

The file `lib/turnip_factory.ex` describes a case of two producers and two
inspectors. `Producer` 1 asks `Inspector` 1 for audits; similarly, `Producer` 2
asks `Inspector` 2. However, `Inspector` 1 uses `Producer` **2** as its
reference; analogously, `Inspector` 2 refers to `Producer` 1 in its audits.

The test begins by requesting both producers to produce turnip. This, depending
on the order of events, may result in either successful creation of two
vegetables, or a deadlock.
