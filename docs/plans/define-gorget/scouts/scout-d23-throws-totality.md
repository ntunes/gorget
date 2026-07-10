# SCOUT REPORT — D23 (Throws Totality Invariant) Enforcement — Track T3

Read-only scout. Compiler built in worktree (`cargo build` OK, `target/debug/gg`).
All measurements below run against that binary. Every `.gg` in `/tmp/d23_meas_6005/`.

TL;DR: D23 is REAL and violated today in **three distinct failure modes**, not one:
(1) free-fn throws call in a consumer position **LEAKS `Result[`** via `E_TypeMismatch`;
(2) free-fn throws call as a **match scrutinee / bare statement SILENTLY SWALLOWS** the
error (miscompile, no diagnostic); (3) **throws METHOD calls** in unhandled positions
**SILENTLY MISCOMPILE to garbage** (typecheck loses throws-ness entirely). The prior
scout mapped only (1) and located the fix at the wrong layer (intercept `E_TypeMismatch`),
which would miss (2) and (3). The correct fix is a **single producer-site emit** shared
by the free-fn AND method call paths. Premise #2 (expr-body asymmetry) is CONFIRMED but
in the **capture** direction, and is a genuinely small, separable widening.

---

## SECTION 1 — PREMISE VERIFICATION

### Premise 1 — "the leak is real, near typecheck.rs:5378, 5414-5444" → CORRECTED (right bug, wrong site)
- Lines 5378/5414-5444 are `auto_prop_skips_unify` / `auto_prop_error_gate` — the Snag#11
  error-TYPE gate (caller-E vs callee-E). That is NOT where the `Result[` leak is emitted.
- **The leak is emitted by the generic `unify` catch-all** at `typecheck.rs:1132-1143`
  (`SemanticErrorKind::TypeMismatch { expected, found }`, rendered by
  `errors.rs:753-755` as `type mismatch: expected \`{expected}\`, found \`{found}\``).
  It fires from ~15 call sites, each shaped `if !self.auto_prop_skips_unify(dest, val, span) { ... unify(dest, val, span) }`.
- **The real root cause is one layer up**, at the throws-fn producer-peel:
  `typecheck.rs:1973-2038` (free-fn `Expr::Call`). In a *non-propagating* context the
  `else` branch (line 2035) returns `raw_result` = `Result[T,E]`, which then hits `unify`
  against a non-Result expected type → the leak. The peel condition is exactly:
  ```
  if !suppress_auto_prop && !dest_is_result && current_fn_can_propagate()  → peel to T (line 2034)
  else                                                                      → raw_result (line 2035–2036)
  ```
  The `else` covers three disjoint cases: `suppress_auto_prop` (legit: scrutinee-w/Result-arms,
  catch/rethrow inner), `dest_is_result` (legit: `Result[T,E] r = f()` capture per §10.3), and
  `!current_fn_can_propagate()` — the **unhandled-throws** case. Only the third is the D23 violation.

### Premise 1 (positions) — enumerated (see Section 2 table)
- LEAK (`Result[` in message): binary operand, match-arm tail (expr-match), plain bind,
  function argument, `return` in non-throws fn, expr-body tail in non-throws fn.
- SILENT SWALLOW (no diagnostic, miscompile): **match scrutinee with non-Result arms**,
  **bare-statement discard** — the throws call's `raw_result` never meets a `unify`, so no
  error is raised and lowering runs on a Result the checker typed as unhandled.
- SILENT MISCOMPILE (garbage): **any throws METHOD call** in an unhandled position (below).
- Already handled correctly: propagating-context consumer positions (peel fires), `catch`,
  `rethrow`, and `Result[T,E]`-typed capture bindings (§10.3).

### Premise 2 — expr-body throws asymmetry → CONFIRMED (in the CAPTURE direction; small)
- Doc basis: reference **§5.1:619** "Equivalent to a block body with `return`", **:628**
  "Expression-bodied functions always return their expression's value."
- Measured: block-body `return risky()` and expr-body `: risky()` behave IDENTICALLY for
  **auto-prop** (p1/p2 both OK) and both **leak** in non-throws fns (p3/p4). The asymmetry
  is **capture**: `Result[int,String] wrap(): risky()` (p5) → `E_TypeMismatch expected Result[int,String] found int`
  (peeled!), while block-body `Result[int,String] wrap(): return risky()` (p6) → OK (captured).
- Root cause located: **`Stmt::Return` (block-body) at `typecheck.rs:3893-3927`** sets
  `decl_type_hint = current_return_type` (3895) and skips `unify` via the auto-prop/capture
  guards (`is_collection_assignment` / `auto_prop_skips_unify` / `is_result_capture_compatible`,
  3922-3924). **`FunctionBody::Expression` at `typecheck.rs:6999-7001`** does NEITHER — it
  calls `infer_expr(expr)` with no hint set, then an **unconditional** `unify(return_type, expr_type)`.
  Classic sibling-site drift: the expr-body arm never got the Snag#36 treatment.
- Scope: SMALL (~8 lines, mirror the Return arm) — cleanest as a shared
  `check_return_value(return_type, expr, span)` helper called by both sites. Entangled? No.

### Premise 3 — enforcement infrastructure → CONFIRMED with specifics
- `SemanticErrorKind` lives in `src/semantic/errors.rs`; `code()` at :641 (exhaustive match,
  no `_`, mirrors `Display`) — new variant `E_UnhandledThrows` goes there + a `Display` arm
  (template near the throws siblings at :881-900) + the `code()` arm. The `code()` doc
  comment (:637-640) makes the missing-arm a hard build error — the ratchet is free.
- Existing throws-family teaching errors to mirror in tone: `ThrowInNonThrowingFunction`
  (:881), `UnconvertibleErrorPropagation` (:893, the closest — it already teaches
  "add … or handle the error").
- Diagnostics ratchet home: `tests/lints.rs` (4875 lines) — but its ratchets are
  **source-scans** (`count_*_sites` + no-growth). The no-`Result[` guard is behavioral, so
  the right home is a **corpus of negative fixtures** run through the existing
  `check_gg_fails(fixture, substr)` harness (`tests/integration.rs:7049`), which already
  asserts `gg check` fails AND stderr contains a substring. Extend it (or add a sibling
  `check_gg_fails_no_desugar(fixture, must, must_not)`) to also assert stderr does NOT contain
  the desugar leak. There is already a `check_gg_warns` used by `lint:suggest_throws`.
- smith: `tests/smith/generator.rs:789` `generate(seed, tier)` — `assert_eq!(tier, 0, …)`,
  tier knob plumbed but only tier-0 grammar exists. Verdict lane `tests/smith/main.rs:8-48`:
  step **1b** — "ggdef `IllFormed` where `gg check` accepted → immediate **SPEC-DIVERGE**";
  a check-accepted program whose value/trap differs from the C oracle is also SPEC-DIVERGE.
  So the throws tier catches the ACCEPT-but-should-reject holes (method miscompile, swallow)
  **iff ggdef models D23's virality** (a soft T1/ggdef dependency — see slicing).
- `FunctionSig` (`src/semantic/traits.rs:14-19`) has **no `throws` field**; `build_function_sig`
  (:1683-1733) computes `return_type` from the declared type and handles `async` (Future wrap)
  but **ignores `throws`** — this is the method-path root cause (below).

### NEW FINDING (correction/deepening of the prior scout) — throws METHOD calls silently miscompile
The prior scout measured only free functions. Throws METHODS are a **deeper, more dangerous** hole:
- `int risky(self) throws String` → `s.risky()` **types as `int`** (measured g6:
  `String x = s.risky()` → `found \`int\``), because `build_function_sig` drops the throws
  clause. Free-fn contrast g7: `String x = risky()` → `found \`Result[int, String]\``.
- Consequence: `int x = 1 + s.risky()` **passes `gg check`** and at runtime yields **garbage**
  (`x=281474674991985` — a Result read as int). `int x = s.risky()` (g5) same: OK-then-garbage.
  This is a **silent miscompile** (core-invariant #8), strictly worse than the free-fn leak.
- Handled contexts still work (g2 auto-prop in a throws fn → r=6; g3 `catch` → x=-99; g4
  `Result` capture → "err boom") because lowering desugars the method to return Result and the
  catch/capture typecheck paths special-case it — but the **unhandled** path has no gate.
- Fix is available at the SAME layer: methods DO carry `throws_type_id` in `function_info`
  (`resolve.rs:745`, the equip-method registration), and the MethodCall arm already reads
  `self.function_info.get(&stored_def_id)` at `typecheck.rs:2383`. It just returns
  `sig.return_type` (2422) without the throws peel. → shared helper fixes both paths.

---

## SECTION 2 — END-TO-END MEASUREMENT TABLE

Non-throwing context, user fn `int risky() throws String`, unless noted. Commands:
`./target/debug/gg check <f>.gg` (leak cases) and `gg run` (swallow/miscompile cases).

| # | position | current diagnostic | leaks `Result[`? | verdict |
|---|----------|--------------------|:---:|---------|
| a | binary operand `1 + risky()` | `E_TypeMismatch: expected int, found Result[int, String]` (1 err) | **Y** | LEAK |
|   | LHS operand `risky() + 1` | E_TypeMismatch (**2** errs — cascade) | **Y** | LEAK |
|   | `risky() + risky()` | E_TypeMismatch (1 err) | **Y** | LEAK |
| b | match-arm tail `case 0: risky()` (expr-match) | 2× E_TypeMismatch: arm `else:7` `expected Result[int,String] found int` (cascade) + whole-match `found Result[int,String]` | **Y** | LEAK (noisy) |
| c | match **scrutinee** `match risky():` non-Result arms | **OK — no diagnostic**; runs → prints `hit-else`, `throw "boom"` DISCARDED (C warns `__v2==__v3` ptr-vs-int) | n/a | **SILENT SWALLOW / miscompile** |
| e | plain bind `int x = risky()` | `E_TypeMismatch: found Result[int, String]` | **Y** | LEAK |
| f | fn argument `id(risky())` | `E_TypeMismatch: found Result[int, String]` | **Y** | LEAK |
| h | bare statement `risky()` (discarded) | **OK — no diagnostic**; runs → `throw "boom"` DISCARDED, prints `done` | n/a | **SILENT SWALLOW / miscompile** |
| — | `return risky()` in non-throws fn (p4) | `E_TypeMismatch: found Result[int, String]` | **Y** | LEAK |
| — | expr-body tail `int wrap(): risky()` non-throws (p3) | `E_TypeMismatch: found Result[int, String]` | **Y** | LEAK |
| g | **throws method** `1 + s.risky()` | **OK — no diagnostic**; runs → `x=281474674991985` (GARBAGE) | n/a | **SILENT MISCOMPILE** |
|   | throws method `int x = s.risky()` (g5) | **OK**; runs → garbage | n/a | **SILENT MISCOMPILE** |
|   | throws method `String x = s.risky()` (g6) | `E_TypeMismatch: expected String, found int` (throws-ness LOST) | no (leaks `int`) | wrong type |

Expr-body vs block-body asymmetry (the concrete pair):

| probe | shape | result |
|-------|-------|--------|
| p1 | `int wrap() throws String: risky()` | OK (peels, auto-prop) |
| p2 | `int wrap() throws String: return risky()` | OK — **symmetric** |
| p5 | `Result[int,String] wrap(): risky()` | **E_TypeMismatch expected Result[int,String] found int** (peeled, should capture) |
| p6 | `Result[int,String] wrap(): return risky()` | **OK** (captures) — **ASYMMETRIC vs p5** |

Handled-correctly controls (no change wanted): g2 (method auto-prop in throws fn) OK/r=6;
g3 (method `catch`) OK/x=-99; g4 (method `Result` capture) OK/"err boom".

---

## SECTION 3 — DESIGN PROPOSAL

### 3a. `E_UnhandledThrows` — emit at the PRODUCER, not by intercepting `unify`
Rationale (CLAUDE.md "fix at the write site" + "fix the class not the instance"): intercepting
the `E_TypeMismatch` at `unify` (prior-scout plan) only catches cases that *reach* a unify —
it structurally CANNOT catch the silent-swallow scrutinee (c) or bare statement (h) (no unify
happens) nor the method miscompile (g) (types as `int`, unifies fine). The producer-peel is
the one chokepoint that sees every unhandled throws regardless of downstream position.

**New variant** (`errors.rs`): `UnhandledThrows { throws_type: String }`, `code()` →
`"E_UnhandledThrows"`, `Display`:
```
this call throws `{throws_type}` but the error is not handled here; declare the
enclosing function `throws {throws_type}`, or handle it with `catch`, `rethrow`,
or by binding the result to a `Result[T, {throws_type}]`
```
`{throws_type}` = `describe_resolved_type(err_ty)`, the callee's `E`, already in hand at the peel.
(See Q2 on the `Result[T, E]` token in the *suggestion* text vs the ratchet.)

**Free-fn emit site** — `typecheck.rs:2011-2038`, split the `else`:
```rust
if suppress_auto_prop || dest_is_result {
    raw_result                       // legit whole-Result positions (scrutinee/catch/rethrow/capture)
} else if self.current_fn_can_propagate() {
    self.auto_prop_error_gate(err_ty, expr.span);   // Route A peel (unchanged)
    return_type
} else {
    self.error(SemanticErrorKind::UnhandledThrows {
        throws_type: self.describe_resolved_type(err_ty),
    }, expr.span);
    self.types.error_id              // error_id unifies with anything → collapses the cascade
}
```
Returning `error_id` (not `raw_result`) makes the multi-error cascades (rows a-LHS, b)
collapse to ONE clean diagnostic (via `unify`'s `error_id` short-circuit at :855).

**Interaction with §10.3 capture:** unchanged. `dest_is_result` (line 2017-2019, reads
`decl_type_hint`) still short-circuits to `raw_result` BEFORE the emit, so
`Result[T,E] r = f()` is never flagged. Match-scrutinee-with-Result-arms and catch/rethrow
inner still set `suppress_auto_prop` → also short-circuit. Legitimate captures are safe.

**Method emit site — REQUIRED for correctness (not optional):** extract the block above into
a shared helper, e.g.
`fn resolve_throws_call_type(&mut self, return_type, err_ty, suppress, span) -> TypeId`,
and call it from the MethodCall arm at `typecheck.rs:2419-2423` using
`self.function_info.get(&stored_def_id).and_then(|fi| fi.throws_type_id)`:
```rust
let ret = match self.function_info.get(&stored_def_id).and_then(|fi| fi.throws_type_id) {
    Some(err_ty) => self.resolve_throws_call_type(sig.return_type, err_ty, suppress_auto_prop, expr.span),
    None => sig.return_type,
};
self.expr_types.insert(expr.span, ret);
ret
```
This makes `s.risky()` type as `Result[T,E]` in the non-propagating case (→ E_UnhandledThrows
instead of garbage), peel to `T` when propagating, and keep working for catch/capture.
NOTE the method arm has multiple return points (2422/2465/2523 and the resolve_method_by_name
fallbacks) — the helper must be applied at each throws-carrying one, and an **arm-count lint**
(3b) should force new method-return sites through it.

### 3b. The no-desugar ratchet — behavioral corpus, not source-scan
Cleanest shape: a set of negative fixtures `tests/fixtures/d23_unhandled_*.gg` (one per
position: binop, arg, bind, scrutinee, statement, match-arm, method) driven by an extended
`check_gg_fails`. Add a helper:
```rust
fn check_gg_fails_no_desugar(fixture: &str, expect: &str) {
    // gg check fails, stderr contains `expect` (e.g. "throws"),
    // AND stderr does NOT contain the desugar-leak substring "found `Result["
}
```
This is BEHAVIORAL — it guards every leak path (free-fn, method, future positions), not just
the one we patch, and it doubles as the negative-fixture set D23 requires. A pure source-scan
(`grep TypeMismatch` in the throws region) is inferior: the leak is emitted by the generic
`unify`, shared with hundreds of unrelated mismatches, so it can't be scanned statically.

### 3c. Reference §10.1 totality sentence
Insert after `docs/language-reference.md:2416` (the auto-propagate paragraph):
> A `throws` call is an expression of type `T` in **every** position — its `Result[T, E]`
> desugar is never observable. In a `throws` (or `Result`-returning) function the error
> auto-propagates; anywhere else you must handle it with `catch`, `rethrow`, or by binding
> to a `Result[T, E]` (§10.3). A `throws` call whose error is neither propagated nor handled
> is a compile-time error (`E_UnhandledThrows`) — never a silently-typed `Result`.

(The A33 "faults never enter a `throws` type" rider belongs in §10.9, out of T3's edit zone.)

### 3d. Expr-body asymmetry fix (the widening) — small, mirror `Stmt::Return`
Replace `typecheck.rs:6999-7001` with the `Stmt::Return` logic (3893-3927):
```rust
FunctionBody::Expression(expr) => {
    let prev_hint = self.decl_type_hint;
    self.decl_type_hint = Some(return_type);
    let expr_type = self.infer_expr(expr);
    self.decl_type_hint = prev_hint;
    if !self.is_collection_assignment(return_type, expr_type)
        && !self.auto_prop_skips_unify(return_type, expr_type, expr.span)
        && !self.is_result_capture_compatible(return_type, expr_type) {
        self.unify(return_type, expr_type, expr.span);
    }
    // ...existing noreturn check unchanged...
}
```
Best done as a shared `check_return_value(return_type, expr, span)` helper called by BOTH
the expr-body arm and `Stmt::Return` (kills the sibling-drift class permanently). Composes
cleanly with 3a: p3/p5 route through the same peel → capture (p5) or E_UnhandledThrows (p3).

### 3e. smith throws tier
- `generator.rs:789`: replace the `assert_eq!(tier,0,…)` with a dispatch; add
  `Gen::program_throws_positions(seed)` that emits a `throws` helper and places a call to it
  in each expression position (binop operand, arg, bind, scrutinee, statement, match-arm,
  method-receiver) with NO handling. Keep tier-0 as the default.
- Detection is mechanical via the existing verdict lane (`main.rs` step 1b): where `gg check`
  ACCEPTS but the program is unhandled-throws, ggdef must return `IllFormed` → **SPEC-DIVERGE**;
  or the value diverges from the C oracle (garbage) → SPEC-DIVERGE. **Dependency:** ggdef must
  model D23 virality (reject unhandled throws) for the accept-but-wrong cases to fire —
  otherwise they fall through as MATCH. Flag as a T1/ggdef coordination item.

---

## SECTION 4 — RECOMMENDED SLICING

T3 is bigger than the prior scout scoped (it did not measure the method miscompile or the
capture-direction asymmetry). But it is still cohesive. Recommended split into
**T3a (must ship together)** and **T3b (parallel/after)**:

| slice | scope | files | size | risk |
|-------|-------|-------|:---:|------|
| **T3a — diagnostic + method + expr-body + fixtures + ref** | `E_UnhandledThrows` variant; shared `resolve_throws_call_type` helper wired into BOTH free-fn (2011-2038) and MethodCall (2419-2523) return sites; expr-body widening (shared `check_return_value`); negative fixtures + `check_gg_fails_no_desugar`; §10.1 sentence | `src/semantic/{errors,typecheck}.rs`, `tests/integration.rs`, `tests/fixtures/d23_*.gg`, `docs/language-reference.md §10.1` | **M–L** | Med — method arm has several return points; must apply helper to each + arm-count lint |
| **T3b — smith throws tier** | new generator tier + tier dispatch; relies on ggdef modeling D23 | `tests/smith/{generator,main}.rs` | **M** | Med — soft dep on T1/ggdef D23 modeling; if ggdef not ready, tier still lands but only catches leak/reject cases, not accept-but-garbage |

**Dependency order:** T3a first (defines the behavior + the negative corpus). T3b after (or
parallel), with a coordination note that its accept-but-should-reject detection needs ggdef's
D23 model (T1). T3a is **disjoint** from the D11 trap tracks (different files) and can run in
parallel with them; the ONLY shared file is `docs/language-reference.md` (§10.1 here vs §10.9
in T1 — different sections; sequence or merge-coordinate the one file).

**Why not split the diagnostic from the method fix?** They share the producer-peel helper and
the method fix is the more dangerous half (silent garbage vs a loud leak). Splitting risks
shipping T3a-lite that greens the free-fn leak while the method miscompile persists — the exact
"benign because rare / matches the reference" trap invariant #8 forbids.

---

## SECTION 5 — OWNER DESIGN QUESTIONS (with recommendations)

**Q1 — Is the throws-METHOD silent miscompile in T3's scope, or its own track?**
Recommend: **IN T3a (mandatory).** Same layer (producer-peel), same `throws_type_id` metadata
(already in `function_info`), same invariant — and it is the WORST of the three modes (silent
garbage, not a loud leak). Fixing free-fns only would be fixing the instance, not the class.
(One-line: the method hole is exactly why "intercept E_TypeMismatch" was the wrong plan — it
can't even see this case.)

**Q2 — Message wording for `E_UnhandledThrows`.**
Recommend the 3a template. Note the capture suggestion names `Result[T, E]`, which would trip a
naive `!contains("Result[")` ratchet. Two options: (a) phrase the suggestion without the bracket
("bind the whole `Result`"); or (b) scope the ratchet to the *leak* substring `found \`Result[`
(the desugar leak is always the `found` type). Recommend **(b)** — the ban is on surfacing the
desugar as the found type, not on ever naming `Result` in teaching prose.

**Q3 — Does `E_UnhandledThrows` REPLACE or SUPPLEMENT `E_TypeMismatch` at the site?**
Recommend: **REPLACE at the producer** (emit E_UnhandledThrows, return `error_id`) so the
downstream `unify` sees `error_id` and stays silent. Supplementing double-reports; replacing
also collapses the current 1-2 error cascades to one clean diagnostic.

**Q4 — Same diagnostic for the swallow (c/h) and method garbage (g) as for the leak?**
Recommend: **same `E_UnhandledThrows`** — all are "a throws call whose error goes nowhere"; the
producer emit unifies them by construction. (Whether Gorget also wants a general must-use on
Result even inside a throws fn is OUT of D23 scope — note as a follow-up, do not expand T3.)

**Q5 — Is the expr-body widening in T3 or its own track?**
Recommend: **IN T3a.** ~8 lines, same "route through the return path" theme, and the D23 LOG
entry explicitly names "the expr-body asymmetry." Landing it separately would leave §5.1's
stated equivalence violated between two T3 commits.

**Q6 — How do negative fixtures assert the message shape?**
Recommend: `check_gg_fails_no_desugar(fixture, "throws")` — assert (i) check fails, (ii) stderr
contains `throws`, (iii) stderr does NOT contain `found \`Result[`. Fixtures in
`tests/fixtures/d23_unhandled_{binop,arg,bind,scrutinee,statement,matcharm,method}.gg`. The
scrutinee/statement/method fixtures are load-bearing — they assert the swallow/garbage now
FAILS (was accepted). This is the invariant-#8 gate made executable.

**Q7 — smith throws tier: block on ggdef D23 model, or ship detecting only leaks/rejects?**
Recommend: **ship T3b's generator regardless; flag the ggdef dependency.** Even without ggdef
modeling D23, the tier exercises the fixed diagnostic (all positions now reject); once ggdef
models virality (T1) the accept-but-garbage detection lights up automatically via SPEC-DIVERGE.
Do not block T3b on T1.
