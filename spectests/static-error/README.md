# `spectests/static-error/` — static-rejection fixtures

`mode: static-error` fixtures: programs a conforming implementation MUST reject
before execution (typecheck / elaboration diagnostics). The `expect:` block
carries the diagnostic code, not stdout.

Adjudication (RFC §4):
- `adjudicator: production-v1` — expectation derived from the production
  typecheck in v1 (a tracked inversion, retired at v1.5 when the executable
  well-formedness checker lands).
- `adjudicator: prose` — expectation derived from spec prose for a rejection that
  NEITHER `ggdef`-elaboration NOR production implements yet. These are the §2.3
  invariant-#8 findings (e.g. the D4/D5/D6 rejections, the IllFormed-implies-
  reject cases): every implementation counts as MISMATCH until the rejection
  ships.

**Empty in phase 0.** The D4/D5/D6 negative fixtures + the diagnostic-code
registry are phase 1 (RFC §6).
