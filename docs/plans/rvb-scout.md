# RV-B Scout — DotShorthand enum-init consume position

## Status: IN PROGRESS (Rust lane fixed+verified; self-host next)

## Premises re-verified (RUN, not source-read) — 2026-07-17

### Rust production lane (BUG CONFIRMED)
- `src/semantic/safety/check_expr.rs` DotShorthand arm (was ~1205) did only
  `for arg in args { check_expr(&arg.node.value) }` — NO pos-2 hook, NO move check,
  NO aliasing check. Longhand `Expr::Call` ctor arm ran the full ownership loop.
- Repro (built `target/debug/gg`, C + LLVM):
  - longhand `E.Wrap(r)` (r drop-tainted struct) → REJECT `error[E_MoveWithoutOperator]` (DropTaint reason). CORRECT.
  - dot-shorthand `.Wrap(r)` bare → **ACCEPTED (BUG)**.
  - live-r `.Wrap(r); use r` runtime → **`drop R` TWICE** on BOTH C and LLVM (identical — shared front-end gap).
  - `.Wrap(!r)` then `use r` → **ACCEPTED (2nd BUG)** where longhand rejects `E_UseAfterMove` (Move sigil ignored by the arm).
- NUANCE on "double-drop": with heap-carrying R (String) under `--sanitize`, ASan does NOT
  report a double-FREE — the lowering does CoW clone-if-live / move-if-dead, so it is a
  SILENT AUTO-CLONE/MOVE of a drop-tainted resource (drop side-effect runs twice), NOT
  memory-corruption. Still a real HIGH correctness bug: violates D12's explicit-intent
  contract (longhand forces `!`/`.clone()`); duplicates a resource's Drop side-effect
  (double file-close / refcount-decrement / FFI-free for a real resource). Report states this precisely.

## Rust fix (PROTOTYPED + VERIFIED) — fix-the-class (Core #4)
Extracted the Call arm's per-arg ownership loop into shared
`check_call_arg_ownership(&mut self, args, is_constructor)`. Call arm now calls it;
DotShorthand arm calls `check_call_aliasing(args)` + `check_call_arg_ownership(args, true)`
(dot-shorthand ALWAYS resolves to enum-variant ctor per typecheck.rs:3681 → is_constructor=true).
Post-fix (all PASS):
1. bare `.Wrap(r)` tainted → REJECT E_MoveWithoutOperator ✓
2. bare live-r → REJECT ✓
3. `.Wrap(!r)` dead-r → ACCEPT, runs, ONE drop ✓
4. `.Wrap(!r)` then use r → REJECT E_UseAfterMove ✓
5. legal bare String `.Wrap(s)` (both lanes) → ACCEPT (no over-reject) ✓
6. legal `.Wrap(r.clone())` → ACCEPT ✓

## Self-host lane (Option A PROTOTYPED + VERIFIED)
BASELINE over-reject CONFIRMED (self_host_typechecker/driver): `.Wrap(!r)` →
`DIAG error move-without-operator ... write !r to move` (self-contradicting misfire);
`.Wrap(!callable)` → single-owner misfire. Both over-reject the LEGAL move.

FIX = Option A (reference-grade, completes the CallArg normalization the ECall/
EMethodCall paths already did; EDotShorthand was the lone holdout on bare
Vector[SpannedExpr]). Changed `EDotShorthand(String, Vector[SpannedExpr])` →
`EDotShorthand(String, Vector[CallArg])` and mirrored the ECall ctor arm. Sites:
- ast.gg:114 (enum def)  [MY ZONE]
- parser.gg:2547 (adapter: push dsa not dsa.value)  [MY ZONE]
- typecheck.gg:2015 (safety walk: gate on a.ownership; add check_call_aliasing)  [MY ZONE]
- typecheck.gg:1209 (collect_idents: a.value)  [MY ZONE]
- resolve.gg:905 (a.value)  [MY ZONE]
- format.gg:263 (a.value)  [MY ZONE]
- infer.gg:232 — NO CHANGE (ignores args)
- meta.gg/derive.gg — none
- ⚠ lower_expr.gg:5701 (callarg_values/callarg_names adapter) — **OTHER SCOUT'S ZONE**
  (1 mechanical hunk, far from method-receiver lowering; flag for coordination)

Post-fix driver (typechecker) verified:
- `.Wrap(!r)` move → NO diag (over-reject CLEARED) ✓
- `.Wrap(r)` bare tainted → 1 diag "cannot copy r" (correct reject preserved) ✓
- `.Wrap(!callable)` → NO diag (single-owner misfire CLEARED) ✓
- `.Wrap(callable)` bare → 1 diag single-owner (correct reject) ✓
- legal bare String `.Wrap(s)` → NO diag ✓
Self-host now EXACTLY mirrors production.

## Validation results
- cargo test --lib: 1107 passed, 0 failed ✓
- Rust integration `dot_shorthand d12_`: 20 passed (Rust-side) ✓
- self_host_driver (26 tests, incl. rejects_d12_drop_purity + accepts_d12_legal): 0 FAILED ✓
  (both were RED before the lowerer arm fixes; GREEN after)
- Corpus blast radius: ZERO at-risk cases — every existing dot-shorthand-with-args
  uses a LITERAL arg (`.Blue(42)`, `.Email("x")`, `.Rectangle(3.0,4.0)`), never a
  tainted place. Self-host source uses ZERO dot-shorthand-with-args.
- ggdef lane: value-position dot-shorthand is OUT OF ggdef subset (only
  `expr_kind`→"dot-shorthand"/"unsupported"; ggdef handles DotShorthand PATTERNS
  only). No ggdef change; note as explicit subset gap (Core #9).
- self_host_bootstrap_fixed_point: 1 passed, 0 failed (592s) ✓ — stage0→4 fixed point HOLDS with the AST-shape change.

## Fixtures (WRITTEN + verified, in tests/fixtures/d12_drop_purity/, saved to /tmp/rvb_fixtures/)
- `dotshorthand_tainted_bare_reject.gg` (NEG): C+LLVM reject E_MoveWithoutOperator;
  self-host driver exit=1, "cannot copy `r`", no C. Wire: check_gg_fails(D12_MOVE_CODE)
  + add to self_host_driver_rejects_d12_drop_purity reject_fixtures list.
- `dotshorthand_move_ok.gg` (POS): C+LLVM+ASan all `built\ndrop 1` (single drop, ASan
  clean — pins the double-drop regression); self-host driver exit=0 emits C.
  Wire: run_gg(..., "built\ndrop 1") + add to self_host_driver_accepts_d12_legal.
- `dotshorthand_callable_move_ok.gg` (POS, single-owner): C+LLVM+self-host CHECK
  accept (clears the flip-track misfire → retires the LOW TODO). ⚠ RUNTIME blocked
  by a PRE-EXISTING, ORTHOGONAL Callable-in-enum-payload lowering panic
  (`src/ir/lowering/mod.rs:2105` — the LONGHAND `E.Wrap(!f)` panics IDENTICALLY).
  Wire: check_gg_ok + add to self_host_driver_accepts_d12_legal. FILE the panic (TODO).

## Orthogonal pre-existing gaps discovered (FILE, not RV-B)
1. Callable-in-enum-payload lowering PANIC (`src/ir/lowering/mod.rs:2105`) — `E.Wrap(!f)`
   both long/shorthand. Check accepts, build panics.
2. Match-bound enum payload drop MISS — `match e: case .Wrap(inner): print(inner.id)`
   drops ZERO times (longhand identical); bare construct (`E.Wrap(!r); print`) drops
   correctly. A match-payload drop-registration gap.

## GO. Rust lane = ship-ready. Self-host lane = Option A prototyped green, needs
## other-scout-zone coordination (7 lowerer arms).
