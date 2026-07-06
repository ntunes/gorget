# `spectests/run/` — execution fixtures

`mode: run` fixtures: a program that runs to a `Value` (or a defined
`Trap`/`IllFormed`/`FuelExhausted`) outcome. The runner executes the program and
compares **stdout + exit code** against the `expect:` block, which is
`ggdef`-generated (`adjudicator: ggdef`) via `ggdef -- gen`.

## Phase-0 seeds

| Fixture | Ratifies | expect stdout |
|---|---|---|
| `smith_dead_branch_alias_bind.gg` | smith B1 / C1 — dead-branch alias bind (RFC §6(c) adj. #1) | `9` |
| `smith_move_param_concat.gg` | smith B2 — `String !p` move + concat (RFC §6(c) adj. #2) | `ablog` |
| `emove_lazy_bind_witness.gg` | EMove witness — pre-mutation value under D1 (RFC §6(c) adj. #3) | `hello` |
| `cow_bare_assign_sever.gg` | C2 — bare-assign copy severs on first write | `1` / `99` |
| `cow_bare_param_materialize.gg` | D2 / C4 — bare-param write materialises; caller untouched | `3` |

Regenerate any `expect:` block with `cargo run -p ggdef -- gen spectests/run/<f>.gg`.

The full `run/` tier fills in phase 1 (migration of the ~1,218 literal harness
expectation pairs).
