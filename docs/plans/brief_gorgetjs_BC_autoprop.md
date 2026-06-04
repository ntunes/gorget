# Brief — Chain 1: Rust `gg` auto-prop at binop operands + match-arm tails (gorget-js B/C)

> **⚠ SUPERSEDED 2026-06-04 (owner decision):** approach changed from (A) expand the
> per-position carve-out to **(B) CENTRALIZE the peel at `infer_expr`** (mirror the lowering
> hook `90d09414`: peel a throws/Result call to `T` by default in a propagating context,
> suppress only where a raw Result is wanted — mirroring lowering's suppress set). Brief-review
> pass 1 also found the gap is ~6 positions, not 2 (if-expr branches, list-literal elements,
> method-call args, struct-ctor field args — beyond binop + match-arm), which (B) fixes
> wholesale. The centralized design is being RUN-verified by a scout; this (A) brief is kept
> for the root-cause / file:line map only. The == caveat (B2): skip the peel for `==`/`!=` when
> both operands are Result (preserve today's behavior).

> **Status (HISTORICAL — approach A, superseded):** brief for review. RUN-verified
> fix direction from the architecture scout (`a24c2e75`); culprit pinned by bisect
> (`a53109a9`). This is a **Rust `src/` (oracle) change** — lands on gorget-1, owner
> promotes. SCOPE = Rust only; the self-host twin is Chain 2 (do NOT bundle).

## Problem (bisect-confirmed regression)
Culprit `2a045ebf` "throws-call-into-bare-T is a compile error (Snag #35)" (2026-05-12)
made a call to a `throws E` fn infer as `Result[T,E]` (so `int n = might()` correctly
errors) and added an auto-prop "skip-unify" carve-out
(`is_auto_propagation_compatible || is_result_capture_compatible`) at FOUR consumer
positions only: `Stmt::VarDecl`, positional call-args, named call-args, `Expr::Catch`.
It was never wired into the two positions that unify operands **against each other**
(not against a known destination):
- **Binary-operator operands** (`src/semantic/typecheck.rs:1205-1304`): `left_type =
  infer_expr(left)` (~:1206), `right_type` (~:1264-1272), then `unify(left_type,
  right_type)` (~:1282/:1296/:1301). `to_s(x) + ""` → left is `Result[String,E]`, unify
  vs `String` fails → `expected Result[String,<error>], found String`. (`if g(x) > 0` is
  the same — the binop errors before the if-cond gate at :3104.)
- **Match-arm tail expressions** (`src/semantic/typecheck.rs:2310-2319`): each arm tail
  `infer_expr(&arm.body)` (~:2312), unified into `result_type` (~:2313); else at :2317-2318.
  A `Result[int,E]` arm unified with an `int` arm fails.

Lowering already handles both (centralized `maybe_auto_propagate` at `lower_expr`'s tail,
`src/ir/lowering/exprs/mod.rs:73-86/2891`, commit `90d09414`) — so once type-check passes,
lowering Just Works. The fix is purely type-check-side.

## The fix (additive, type-check-side only)
Add a helper that yields the unwrapped Ok type for the two unify-against-each-other
positions, reusing the EXISTING eligibility logic (do NOT invent new propagation rules):

```rust
// Returns Ok(T) when `value` is Result[T,E] AND the current fn can propagate
// (mirror the eligibility predicate already inside is_auto_propagation_compatible,
// typecheck.rs:4027 — current_function_throws OR the fn's return type is Result[..]).
// Otherwise returns `value` unchanged.
fn autoprop_peel(&self, value: TypeId) -> TypeId
```
Borrow note: `infer_expr` takes `&mut self`, so sequence as two statements:
`let t = self.infer_expr(x); let t = self.autoprop_peel(t);`.

**Insertion points** (RUN-verified by the scout; re-confirm line numbers — they drift):
1. **Binop operands — NO gate.** Peel `left_type` immediately after its inference (~:1206)
   and `right_type` after its inference block (~:1272). Safe unconditionally: an arithmetic
   / comparison / string-eq operand always wants `T`; a raw `Result` is never wanted there.
2. **Match-arm tails — GATED on the match's destination.** Peel `arm_type` (after ~:2312)
   and `else_type` (after ~:2317) ONLY WHEN the match expression's surrounding expected /
   destination type is NOT itself a `Result`. When the destination DOES want a `Result`
   (`Result[int,PErr] cv = match sel: case 0: to_n(sel); else: Ok(sel)` — capture the whole
   Result), do NOT peel. Mirror the existing `is_result_capture_compatible`
   (`typecheck.rs:4174`) + how the lowering hook suppresses on `expected_type == Result`
   (`exprs/mod.rs:2897`). ⚠ This gate is the one non-mechanical part — the reviewer must
   confirm the match's expected/destination type is actually available at the arm-unify site
   (thread/consult it) and that the gate matches the lowering-side suppress exactly.

## Test/fixture changes
- **Negative test `variant_mixed_arm_match_error`** (`tests/fixtures/...` + asserted in
  `tests/integration.rs:~3889`): with the match-arm peel, the program STILL fails to compile
  (its arms are `int` but the fn returns `bool`) — correctly — but with a DIFFERENT, better
  message (e.g. `expected bool, found int` at the return, instead of `expected int, found
  Result[int,`). Update ONLY the asserted error substring; the test's intent (this is an
  error) is preserved. Verify the new message against the actual build output.
- **New positive fixtures** (regression-protect the two positions; the 1187/0 suite had
  ZERO coverage — that's why gorget-js caught this): add small fixtures in `tests/fixtures/`
  + wire into `tests/integration.rs`, each in a `throws E` fn with deterministic stdout:
  (a) a throwing call as a **binop operand** (e.g. `int n = to_n(x) + 5` → asserts the
      success path prints the sum AND the error path propagates — mirror the scout's
      `to_n(3)+5 → 35`, error path → `err neg`);
  (b) a throwing call as a **match-arm tail** (e.g. `int d = match sel: case 0: to_n(sel);
      else: sel` → asserts `d0 ...`);
  (c) optionally a **negative** fixture confirming `return Error(X)` in a `throws` fn is
      still rejected (Family A — guards the deliberate `150ce7d5` behavior).
  Use the `/tmp/snagB.gg` / `/tmp/snagC.gg` repros as the starting shapes.

## Constraints / non-goals
- ADDITIVE only — do NOT touch the existing 4 carve-out positions (they work).
- Rust `src/` ONLY. Do NOT touch the self-host (`tests/fixtures/self_host_lowerer/`) — that
  is Chain 2.
- No name-matching; reuse the typed helpers (`is_auto_propagation_compatible`,
  `is_result_capture_compatible`). Layering-clean.

## Gate (RUN everything; tee logs)
1. `cargo build`.
2. The 3 repros: `cargo run -- run /tmp/snagB.gg` + `/tmp/snagC.gg` compile AND run (match the
   scout's expected output); `cargo run -- check /tmp/snagA.gg` STILL errors (A stays rejected).
3. `cargo test --lib` (baseline 1072/0; + the updated negative test still passes with the new
   asserted string).
4. FULL `cargo test --test integration -- --test-threads=4` (baseline 1187/0; + the new
   positive fixtures pass). Tee to `/tmp/bc-gate-$RANDOM.log`.
5. Spot-check `GG_BACKEND=llvm` is unaffected (type-check is backend-agnostic, but the new
   fixtures should pass under both — quick check on the new fixtures only).
⚠ The architecture scout's prototype passed 1072 lib + error-handling + 111 enum/result
fixtures with an UNCONDITIONAL match-arm peel; the GATED version (this brief) is stricter and
must also pass `risk_match_result`-shaped cases (a match feeding a `Result` destination keeps
the whole Result). Build `/tmp/risk_match_result.gg` (the scout left it) and confirm it still
type-checks as a Result-capture.

## Report back
The diff (autoprop_peel + 4 insertion points + the gate logic + fixtures + the negative-test
string update), the 3-repro results, the `risk_match_result` result, and all gate numbers.
Cite the final line numbers actually used (they drift from the ~estimates above).
