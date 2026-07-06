# Implicit-copy positions (the closed set) and the fresh-temp Move

> **STUB (phase 0).** Rule stated; full prose is phase 1.

**Rule (RFC §2.2, bullet 1).** There is a **closed set** of *implicit-copy
positions*: bare-assign binds, constructor / struct / enum field init, collection
put, `return`, closure capture, and **materialize-on-write** (see
[`02`](02-borrow-and-materialize-on-write.md)). An *implicit copy* is **a read of
a LIVE PLACE** at one of these positions: the value is conceptually copied
as-of that point. A **fresh expression temp** (a constructor or call result) has
no continuing owner, so elaboration tags it `Move` — a **STRUCTURAL** fact, not
the retired clone-vs-move liveness optimization. `Res r = Res(1)`,
`return make()`, and `with Res(1) as r:` are moves, never copies. Clone-vs-move
liveness optimization does not exist in `ggdef` (that is production's D1
refinement obligation, not the meaning).

A copy emits a `BindCopy` trace event; a fresh-temp move emits a structural
`Move` event (RFC §2.7).

<!-- cites: eval.rs::eval_source_to_slot -->
<!-- cites: eval.rs::eval_source_to_value -->
<!-- cites: eval.rs::emit_fresh_temp_move -->
<!-- cites: elaborate::bind_source -->
<!-- cites: elaborate::owning_source_from_expr -->

**Related:** decisions.md D1 (value semantics), C2/C8 (bare-assign copy + sever).
