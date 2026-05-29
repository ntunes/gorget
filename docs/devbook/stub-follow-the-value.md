# Follow the value: a construct from source to machine code

> **Status: STUB** — deepened after reference coverage is complete (per [the plan](../plans/devbook_plan.md), reference-first).

This chapter will be the book's narrative spine: it picks 2–3 concrete constructs and traces each one through *every* layer — token → AST → resolved → typed → GIR → LIR → BIR → emitted C — returning to it from each part. It is deliberately written last, once the per-subsystem reference chapters exist to link into.

Planned traced constructs:
- `Vector.push(x)` — exercises the copy-on-write consuming-position contract end to end.
- An f-string with interpolation — exercises the cross-cutting lexer→parser→resolve→typecheck→lowering path.
- A `match` on an enum — exercises EnumInit/EnumCheck/EnumExtract lowering and BIR expansion.

Each trace doubles as a self-checking artifact: if it stops matching the code, a chapter is stale.
