# Extension Scout Report — self-host liveness/UAM gauntlet, Parts A–D

Extends `scout-liveness.md` (the CORE liveness pass, proven blast-radius 0). This pass verifies the
core proto applies onto CURRENT main (post-B2 + owner's 4 folds) and MEASURES the three folded parts
the original scout never saw. Agent `ad184bc9993c0c307`, 2026-07-15, off main `1b478631`.
Prototype: `scouts/patches/liveness-ext-proto.patch` (buildable core+A+B, +462 net; **applies CLEAN to
pristine main** — verified `git apply --check`). Raw findings: `liveness-ext-FINDINGS.txt`,
re-measure harness `liveness-ext-REMEASURE.sh`. (Persisted by the orchestrator — the harness blocks
agents writing `.md`.)

## VERDICT: all four parts FEASIBLE, infra EXISTS, NO scope-expanding blocker.

Core proto re-verification: applies onto current main with only a **1-line `diagnostic.gg` conflict**
(B2 added `DkBorrowConflict` at the line the proto adds `DkUseAfterMove` — resolved by keeping BOTH).
`typecheck.gg` applies clean. **Re-measured whole-source blast = 0** (exit 0, 37,459,663 bytes C, 134 s).

## PART A — B2 ↔ liveness pass-order CONVERGENCE (MEASURED, clean)
The convergence is a **single syntactic line** in `check_call_aliasing`'s pairwise loop (`i<j`, places in
arg order): `if places[i].ownership == OWN_MOVE: continue`. This *subsumes* the old mover-mover arm AND
defers every move-before-use to liveness (a move at the earlier arg dominates all later uses of its place —
liveness's job). Purely syntactic — needs no liveness state, works in either walk arrangement.

**d10b fixture table (MEASURED on the prototype `driver_ab`):**

| fixture | source | post-unification (MEASURED) | axis | flip? |
|---|---|---|---|---|
| `writer_writer_reject` | `f(&n,&n)` | REJECT "their places overlap" | overlap | no |
| `writer_subfield_reject` | `h(&n,&n.data)` | REJECT "their places overlap" | overlap | no |
| `read_move_reject` | `g(n,!n)` (non-Copy read-before-move) | REJECT "their places overlap" | overlap | no |
| `move_noncopyread_reject` | `f(!n,n.data)` (move-before-read) | REJECT **"use of `n` after it was moved"** | **liveness UAM** | **FLIP** |
| `double_move_reject` | `f(!n,!n)` | REJECT **"`n` moved more than once (double move)"** | **liveness DM** | **FLIP** |
| `disjoint_siblings_accept` | `f(&m.a,&m.b)` | ACCEPT (4660 C bytes) | accept | no |
| `writer_copyread_accept` | `f(&s,s.tag)` | ACCEPT (3935 C bytes) | accept | no |

Both flips emit **exactly ONE** diagnostic (stderr contains the UAM/DM message and **zero** "their places
overlap"). **The self-host is CLEANER than production**, which the scout MEASURED double-fires on
`f(!n,n.data)` (E_BorrowConflict AND E_UseAfterMove — redundant second diagnostic). ggdef (the acceptance
oracle) models the single IllFormed → self-host matches the DEFINITION and *improves* on the reference.
Production probe matrix measured: P1 `f(!n,n.data)`→2 errors (wart); P2 `g(n,!n)`→1 overlap; P3 `f(!n,!n)`→1
DoubleMove; P4 `f(!s,s.copy_field)`→1 UAM. Self-host matches P2/P3/P4, improves P1.

## PART B — `!self`-consuming receivers + ConsumeCallable — infra EXISTS (MEASURED)
- **Self-sigil resolution: YES.** `reject_amp_self_mutator` (`typecheck.gg:931-940`) already calls
  `resolve_method_full(recv_type, method_name, scopes, &types) → MethodResolution` and reads `mr.found` +
  `mr.sig.self_ownership` (0=self, 1=`&self`, **2=`!self`==OWN_MOVE**), imported from `traits`
  (`typecheck.gg:37`). Mirrors production `check_expr.rs:387-403`.
- **ConsumeCallable typing: YES.** `types.gg` has `RTConsumeCallableTrait(int)` + `RTBoxedCallable("ConsumeCallable", id)`;
  `infer_expr_type`/`get_rtype_at` resolve it. Mirrors production `is_consume_callable_var` (`helpers.rs:423`).

Prototype: a `!self` method call marks the receiver root moved (bare-place receiver only, mirroring
production's conservatism); a call to a ConsumeCallable-typed var marks it moved. **MEASURED vs production:**

| probe | self-host | production |
|---|---|---|
| `consuming_self_use_after_move_error.gg` | REJECT UAM | E_UseAfterMove ✓ |
| self-consume accept-variant (no post-use) | ACCEPT | ACCEPT ✓ |
| `consume_callable_once_error.gg` (`f(5);f(10)`) | REJECT DoubleMove | E_DoubleMove ✓ |
| `consume_callable_once.gg` | ACCEPT | ACCEPT ✓ |

**Whole-source blast (CORE+A+B) = 0** (exit 0, 37,474,916 bytes C, 134 s). Zero false positives. Closes
`consuming_self_use_after_move_error.gg`.

## PART C — loop-local precision (design mirrors production; FP bound = 0 by audit)
Production oracle (measured `gg check`): `c1` in-loop move of OUTER var → **E_MoveInLoop**; `c2` loop-local
move → accept; `c3` same-stmt rebind fold → accept. The clone-and-discard proto measured **ACCEPT on c1**
(under-detects cross-iteration MoveInLoop), accept c2/c3. **Precise design** (mirror `origins.rs:495-503` +
`check_stmt.rs:988-1022`): walk loop body IN-PLACE, `loop_depth+1`, fresh innermost `loop_locals` set (add
for-pattern binding + body `SVarDecl` names), track same-stmt `rebind` in the `SAssign` arm; in
`live_mark_move`: `loop_depth>0 ∧ root∉loop_locals ∧ root≠rebind → MoveInLoop`; after body
`merge(before, after_body)`.

**FP bound (static + oracle, NOT a full precise-build whole-source blast):** production compiles the
self-host WITH MoveInLoop enforcement ⇒ zero in-loop-outer-var moves exist in the source. Audited all 20 real
(non-comment) `!move` sites; every loop-adjacent one (`scope.gg:231,278`; `traits.gg:309,315`) is the same
straight-line idiom (local temp declared then moved in an if/else branch — not in a loop). **Expected precise
blast = 0.** **CAVEAT (honest):** the ~90 call-site threading for the `SafetyState` refactor was too much to
land reliably in a standalone scout build, so the scout did NOT run a full precise-build whole-source blast.
**The executor implements precise loops inside the Part D unified walk and MUST re-measure blast = 0.**
Owner-accepted floor = clone-and-discard if any residual FP surfaces.

## PART D — unified walk design (merge is MECHANICAL)
```
struct SafetyState:
    Dict[String, int] moved       # branched flow-state (cloned per branch, unioned moved-in-any)
    int loop_depth                # structural loop context (saved/restored, NOT branch-cloned)
    Dict[String, int] loop_locals
    String rebind
```
`scope_id` STAYS an ambient `int` param (constant per function body, never branches — bundling it would
clone a constant). Extensible: a new axis adds a field.

**Merge is mechanical** — the two walks already mirror the same Stmt/Expr enums arm-for-arm; each arm runs the
stateless carrier-ops rejects AND the stateful liveness update in ONE traversal. Branch arms clone `moved` per
branch (via `live_branch`/`live_commit`); loop arms depth++/fresh loop_locals/MoveInLoop then
`merge(before, after_body)`. **Call arms exact order:** (1) callee liveness + ConsumeCallable-consume;
(2) per-arg loop: liveness move/use (left-to-right) + carrier-ops ctor/ingest reject; (3) `check_call_aliasing`
(skip-move-first). ⇒ liveness precedes aliasing (rider satisfied). **Non-mechanical arm: NONE.**

## Scope-expanding surprises / decisions
- **EXECUTOR MUST rewire the d10b integration reject-lane (MEASURED).** `integration.rs
  self_host_driver_rejects_d10b_place_overlap` asserts `stderr.contains("their places overlap")` for all 5
  reject fixtures. After the flip, `move_noncopyread_reject` + `double_move_reject` emit UAM/DM messages and NO
  overlap message → those two must move to a UAM/DM-message assertion group, and their `.gg` header comments
  updated (`double_move_reject.gg` already anticipates the flip; `move_noncopyread_reject.gg` says "overlap
  diagnostic" → change to UAM). **Part of the fix, not a follow-up.**
- **Self-host becomes CLEANER than Rust gg** on `f(!x, x.noncopy_field)` (single UAM vs Rust's UAM+BorrowConflict
  double). Intended by the rider, reference-grade-better. File Rust gg's redundant double as a LOW diagnostic
  wart (program is correctly rejected either way — NOT a correctness bug, NOT blocking).
- **Carryover (owner Q1, UNCHANGED):** the discarded-`Dict.remove` `Option[int]` coalesce miscompile is still
  worked around by the isolated `live_reinit` helper (preserved). Own Rust-gg track.
- No new codegen bug found this pass.

## Size estimate
Measured buildable prototype (core+A+B) = **+462 over main**. Part C precise loops ≈ **+40–60**. Folded into the
Part D unified `check_safety_stmts` (each arm does both; deletes the separate carrier-ops driver invocation) →
full unified+folded pass ≈ **+450–520 net LOC over current main**. Proto's standalone liveness was +403.

## Re-measure commands
Apply patch → `cargo build --release` → `gg build tests/fixtures/self_host_lowerer/driver.gg -o /tmp/PRIV/driver`
→ `driver tests/fixtures/self_host_lowerer/driver.gg lib --lir-c 2>err` (blast = `wc -c err`, must be 0) → run
each `d10b_place_overlap/*.gg`. Full harness: `scouts/patches/liveness-ext-REMEASURE.sh`.
