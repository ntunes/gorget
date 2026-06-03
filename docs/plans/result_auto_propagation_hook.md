# Brief — Result→T auto-propagation hook (consumer-targeted port)

Self-host-dir only, **`tests/fixtures/self_host_lowerer/lower.gg` + `lir_lower.gg`** (NO other dir/src).
FULLY PROTOTYPED + END-TO-END verified by a scout (2026-06-03, tip `cf6d5f89`): built the proto,
ran a full-corpus differential = **0 REGRESSED / 14 IMPROVED**, 272/272 snapshots green, fixed point
preserved. ⚠ Needs ≥3 fresh sequential reviews before the executor. Reference proto (if still on the
box): `/tmp/sh_proto_13343/sh/{lower.gg,lir_lower.gg}` — cross-check only; implement from THIS spec.

## The gap
A `throws E` function call that returns `Result__<ok>__E` is NOT auto-unwrapped at consumer positions
(`for x in might()`, `if might()`, `while might()`, `v[might()]`, `f(might())`, `T x = might()`). The
self-host has NONE of Rust's `Result→T` auto-propagation (`grep should_auto_propagate lower.gg` → 0).
Today these consumer sites either `lower_fail`-DROP the construct (for-iter `lower.gg:8473`) or use the
`Result` struct as a scalar → CC-FAIL / WRONG-OUTPUT. Rust auto-unwraps: on Ok → continue with the
payload; on Error → early-return the error re-wrapped in the caller's `Result[_,E]` (+ on-error
cleanups + drops).

## ⚠ KEY DESIGN DECISION — CONSUMER-TARGETED, not Rust's centralized hook ([[feedback-rust-not-sacrosanct]])
Rust fires `maybe_auto_propagate` from `lower_expr` on EVERY Call/MethodCall (`exprs/mod.rs:73-86`),
then needs a `suppress_auto_prop` one-shot (a `LowerCtx` field, set at every Ok/Error match arm) to
STOP auto-unwrapping a `match might():` scrutinee. **This port deliberately does NOT mirror that.**
Instead it calls a `maybe_auto_propagate(val)` helper explicitly at the 6 CONSUMER sites — match
scrutinees never call it, so they stay raw Results (correct) with ZERO suppress machinery. This is the
single most important risk-reduction; reviewers: validate this is sound (a Result reaching a consumer
site that ISN'T a match-scrutinee always wants unwrapping when the enclosing fn is `throws`).

## (1) `lir_lower.gg` PREREQ (6 lines) — confirmed STILL needed after the V/E/Err fix
`Result__<ok>__E` / `Option__<inner>` mono'd-generic type names are NOT `type_infos` keys (Pass 3
synthesizes their fields from `m.structs`; comment `lir_lower.gg:969`), so the V/E/Err guard (`and not
type_infos.contains`) does NOT rescue them: `Result__int__E` ends in `__E` → `is_generic_placeholder_name`
true → skipped → typedef dropped → `error: 'Result' undeclared` (reproduced: snag49a-d baseline). FIX:
at the TOP of `is_generic_placeholder_name` (`lir_lower.gg:469`), before the single-letter check:
```
if name.starts_with("Result__") or name.starts_with("Option__"):
    return false
```
Mono'd prelude generics are concrete by construction. (snag49a-d are `throws E` → `Result__*__E`, need
this; `throws String` fixtures don't.) ⚠ Confirm this doesn't un-skip a genuine `Result`/`Option`
TEMPLATE name (an unsubstituted `Result[T,E]`) — the throws-wrap only builds `Result__`/`Option__`
from RESOLVED return/throws types, so a `Result__`/`Option__` prefix is always concrete; reviewers
verify no bare-`Result`/`Option` (no `__`) path regresses.

## (2) `lower.gg` — the helper `maybe_auto_propagate` (near-clone of the EXISTING `lower_rethrow_expr`)
Insertion: a new `int maybe_auto_propagate(LowerCtx &ctx, int val, GirModule &gmod)` right after
`lower_rethrow_expr` ends (~`lower.gg:8032`, before `lower_match_stmt`). It is `lower_rethrow_expr`
(`:7982`) MINUS the user `transform` (forward the raw Error payload) — the same tag-read / branch /
Ok-extract / Error-early-return skeleton. Reuse the EXISTING primitives (all present):
- `result_payload_types(result_tn, &gmod)` (`:7866`) → `(ok_tid, err_tid)` (⚠ see §4 smart-split fix).
- `match_scrutinee_ptr` (`:7509`), `emit_tag_read` (`:7331`), `emit_payload_read_mode(..., false)` (`:7419`, clone-extract — the mode catch/rethrow use).
- `new_block`/`switch_to`/`set_terminator(GTBranch/GTJump/GTReturn)`/`GICmp`/`add_local`/`emit`.
- Error re-wrap: `GICallExtern(err_dst, "Error", [op_consume(err_val, CkCallArgOwning())])` (bare
  `"Error"` variant routes via LIR `try_lower_prelude_variant`, as `lower_throw_stmt:7917` does), with
  `err_dst` typed to local-0's type; then `GIAssign(0, op_consume(err_dst, CkReturn()))`.
- On-error + drops: `emit_on_error_cleanups(&ctx, &gmod)` (`:7883`) + `emit_drops_for_early_exit(&ctx, DSK_FUNCTION, Some(0))` (`:1299`) + `GTReturn`.
**Two gates (idempotent no-op when not applicable):**
1. `val`'s type name (peel `GtPtr`) `starts_with("Result__")` — else `return val` unchanged.
2. The enclosing fn can propagate: local-0 (return place) type name `starts_with("Result__")` (the
   `throws` lowering makes local 0 the mono'd Result) — else `return val` (a non-throws caller keeps
   the raw Result; this subsumes Rust's `should_auto_propagate`/`expected_type` check).

## (3) `lower.gg` — 6 firing sites (one line each: `x = maybe_auto_propagate(&ctx, x, &gmod)`)
| # | Site | line | for | extra gate |
|---|------|------|-----|-----------|
| 1 | `lower_for` `coll_local` | `:8258` | snag49a | none (helper self-gates) |
| 2 | `lower_if` (stmt) `cond` | `:7091` | snag49b | none |
| 3 | `lower_while` `cond` | `:8642` | snag49d | none |
| 4 | `lower_if_chain_expr` `cond` | `:4196` | completeness | none |
| 5 | `SVarDecl` init `val` | `:6412` | throws_call_capture | **skip if the declared type name `starts_with("Result__")`** (`int v = might()` unwraps; `Result[int,E] r = might()` keeps raw) |
| 6 | call-arg `arg_local` | `:6220` | throws_call_capture (argprop) | **skip if `callee_param_types[ai]` name `starts_with("Result__")`** (ctor args flow through this same loop → covered for free) |
⚠ Sites 1-4 need NO gate (a Result-typed iterable/bool-cond is never a "destination wants Result").
Sites 5/6 need the expected-type gate (Rust's skip-if-destination-is-Result). VERIFY each line number
+ the exact local var against current source before editing (they may drift a few lines).
⚠ **Match scrutinees are deliberately NOT wired** (the consumer-targeted design — they must stay raw
Results for Ok/Error discrimination). EIndex value-read (`:5537`) is a separate STUB returning 0, so
`snag49c` stays WRONG regardless — do NOT wire index / do NOT chase snag49c here (log it).

## (4) `lower.gg` — `result_payload_types` smart-split fix (REQUIRED for snag49a)
`result_payload_types` (`:7866`) uses a leftmost-`__` split (documented limit `:7857`); for
`Result__Vector__int64_t__E` it mis-splits Ok=`Vector` → `__gg_Vector` vs the real `GorgetArray` →
`incompatible types`. FIX: port lir_lower Pass 3's smart split (`lir_lower.gg:1026-1041`) — try each
`__` boundary L→R, accept the first where BOTH sides name a concrete type (a new
`is_concrete_payload_name` helper: scalar OR `type_infos.contains` OR a mono'd-collection prefix).
SHARED helper (catch/rethrow use `result_payload_types` too) — strictly better (only changes
nested-generic payloads that were mis-split anyway); reviewers confirm catch/rethrow fixtures stay green.

## (5) Resource/drop wiring
Mirror the SHIPPED catch/rethrow path: clone-extract the payload (`emit_payload_read_mode(...,false)`),
re-wrap Error via `op_consume(CkCallArgOwning())`, return via `op_consume(CkReturn())`, then
`emit_on_error_cleanups` + `emit_drops_for_early_exit(DSK_FUNCTION, Some(0))`. The self-host's drop
discipline here is IDENTICAL to the already-shipped catch/rethrow — inherits their correctness. Proto
ran clean on a Result carrying `Vector[int]` (snag49a) — no double-drop/leak.

## Validation gate (self-host-dir only; FORCE-REBUILD driver before each comparison/diff run)
1. `cargo build` + `cargo build --release` + `cargo test --lib` (~1066/0).
2. Force-rebuild driver (`rm -f tests/fixtures/self_host_lowerer/driver{,.c}`; `GG_BUILD_TIMEOUT_SECS=600`).
3. Per-fixture END-TO-END (emit→cc→run, diff FULL stdout vs `cargo run -- run`): **snag49a, snag49b,
   snag49d, throws_call_capture_and_propagate** must MATCH; bonus expected (verify, snapshot if MATCH):
   `encoding_basic`, `error_handling`, `error_propagation_chain`,
   `result_propagation`, `test_error_handling`, `on_error_basic`, `on_error_inline`. ⚠ TWO that do
   NOT flip (do NOT snapshot, do NOT chase — PRE-EXISTING, not regressions): `snag49c` (orthogonal
   EIndex value-read stub `lower.gg:5537` returns const 0) and `option_result_nested` (its
   `Result[Option[int],_]` Ok payload `Option__int64_t` defeats `is_concrete_payload_name` →
   leftmost-split fallback; a known Option-payload smart-split gap — log as follow-up). Snapshot EVERY
   fixture that reaches byte-identical MATCH (expected ~+9 to +12); do NOT cap.
4. `self_host_runtime` ≥ **272/0** + the new snapshots (0 regressed).
5. `lowerer_comparison` ≥ **954**, `c_emit_comparison` ≥ **882** (re-confirm from `--nocapture`).
6. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → MATCH ≥ **282** (272 + ~10), NO fixture MATCH→worse.
   ⚠ The proto measured a full-corpus C-differential = 0 REGRESSED / 14 IMPROVED — the executor should
   spot-confirm no MATCH→worse on the error-handling families.
7. `bootstrap_fixed_point` GREEN (the driver self-emits throws/Result code — this is a REAL signal here,
   not just a regression guard; confirm stage2==3==4 byte-identical). ⚠ a pre-existing `__gg_R`/`__gg_W`
   stage1-vs-stage2 driver-source diff exists in the BASELINE too (not from this change) — don't be
   alarmed; the gate is stage2==3==4.

## Files (stage by name only)
`tests/fixtures/self_host_lowerer/lower.gg` + `tests/fixtures/self_host_lowerer/lir_lower.gg` + new
`tests/fixtures/runtime_snapshots/*.out` (all verified MATCHes). Do NOT touch other dirs/src/TODO/DONE.

## Follow-ups to LOG (orchestrator logs to TODO post-integration; execution agent does NOT touch TODO)
- `snag49c` EIndex value-read stub (`lower.gg:5537` returns const 0) — separate index-read chain.
- **Option-payloaded Result smart-split gap:** `is_concrete_payload_name` returns false for an
  `Option__<inner>` Ok payload (not scalar / not in type_infos / not a collection), so
  `Result[Option[T],_]` mis-splits (leftmost) → blocks `option_result_nested` etc. Extend the helper
  to accept `Option__`/`Result__`-prefixed payloads as concrete (recursively) — separate follow-up.
- If the centralized variant is ever wanted (to also auto-prop in more positions), it needs the
  `suppress_auto_prop` one-shot — out of scope; the consumer-targeted variant is the chosen design.
