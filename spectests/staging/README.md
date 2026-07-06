# `spectests/staging/` — the low-bar tier (NOT gated)

An incubator for fixtures that are not yet ratified conformance tests: newly
minted smith repros awaiting triage, candidate expectations pending human review,
shapes whose spec disposition is still open. Nothing here gates any
implementation (RFC §3).

A fixture graduates out of `staging/` into `run/` (or an error tier) once its
expectation is `ggdef`-generated and human-review-diffed and its governing rule
is settled.

**Empty in phase 0.**
