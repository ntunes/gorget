# Scout Report — ggdef liveness transition-table (definition-integrity)

Agent `ae7dc3c6`, 2026-07-15. Prototype `scouts/patches/ggdef-liveness-fix-proto.patch` (applies CLEAN
to main; 4 files under `spec/ggdef/src/`, +175/-8). Test log `ggdef_test.log`. (Persisted by the
orchestrator — harness blocks agents writing `.md`.)

## VERDICT: 2 confirmed bugs fixed with a transition-complete model. NO third fixable bug. Contained to `spec/ggdef/` (NOT bootstrap-gated). Ready to brief.

## The complete transition table (every cell measured via `ggdef run`)
ggdef models liveness DYNAMICALLY — one `Slot` per binding (`Owned|BorrowView|WriteThrough|Moved`, `eval.rs:203`).

| Cell | ggdef site | pre-fix | correct? | fix |
|---|---|---|---|---|
| Move `!x` (Live→Moved) | `kill_place` (`eval.rs:633,666`) | kills; later read→IllFormed | ✅ | none |
| **Consume-call `f()` ConsumeCallable (Live→Moved)** | `CallValue` (`eval.rs:1028`) | reads callee, does NOT kill → 2nd call runs | ❌ **BUG 2** | kill callee slot when `consumes_callee` |
| Plain call `f()` Callable/MutCallable | same | stays Live (reusable) | ✅ | preserved (flag=false) |
| **Whole-local reassign `x=…` after move (Moved→Live)** | `resolve_write` Moved arm (`eval.rs:768`) | IllFormed "write to moved" | ❌ **BUG 1** | revive → `Owned(newval)` |
| Projected write `x.f=…` on moved root | same arm | IllFormed | ✅ | preserved (proj non-empty) |
| Read of moved / double-move | `resolve_read` (`eval.rs:745`) | IllFormed | ✅ | none |
| **Branch-merge** (moved in one/both/diverging arm) | dynamic per-taken-path | see below | ✅ correct-as-dynamic | **NO fixable cell** |
| Loop back-edge (multi-iter re-move) | dynamic | IllFormed on iter 2 | ✅ | none |

## The branch-merge cell — NO third bug (the key finding)
ggdef is a big-step interpreter: ONE path per run (per the concrete condition). Bugs 1&2 are WITHIN-model
completeness holes — after the fix, ggdef's dynamic verdict on STRAIGHT-LINE code coincides with the static
gate on all inputs. The branch disagreements (`if c(=false): sink(!x); use x` → ggdef Value, static REJECT)
are the **INHERENT static-vs-dynamic gap**: those programs are memory-safe on their actual execution (x
never moved), so ggdef's `Value` is the CORRECT DYNAMIC semantics; the static checker rejects them
CONSERVATIVELY. "Fixing" it would require all-paths exploration, contradicting the fuel-bounded
deterministic-execution design (a category error: dynamic semantics ≠ static gate). Does NOT affect any
gauntlet fixture — reject fixtures pin the condition so the offending op is on the sampled path (verified:
all 9 reject → IllFormed, all 6 accept → Value; `move_in_loop_reject` uses a 2-elem iterable → genuine
iter-2 re-move). **⇒ ggdef's oracle-boundary: ggdef defines DYNAMIC/runtime semantics (and straight-line
liveness where dynamic=static); STATIC over-rejection of conditionally-moved vars is owned by the
production + self-host static borrow-check + their negative fixtures, NOT ggdef. This must be DOCUMENTED
(one paragraph in the ggdef/RFC docs) so a future session doesn't "discover" the c=false case and mistake
it for a bug.**

## The fix (coherent whole, not 2 point patches)
- `ggc.rs`: `Expr::CallValue` gains typed `consumes_callee: bool`.
- `elaborate/mod.rs`: new `Ty::Callable{consuming}`; RESOLVE-ONCE in `ty_of_type` (`ConsumeCallable`→true,
  `Callable`/`MutCallable`→false); `CallValue` reads the typed classification (no surface-name match AT
  THE CALL SITE — layering rule 2/4: name matched once at type-resolution, written to a typed field, read
  downstream). ⚠ reviewer eye: the `ty_of_type` classification keys off the surface type NAME
  ("ConsumeCallable") — same pattern ggdef uses for Vector/Dict/Option (name IS the surface identity,
  resolve-once into a typed field) — acceptable but confirm it's the good kind, not re-derivation.
- `eval.rs`: `resolve_write` Moved arm → `Action::Revive` (whole-local) vs stays-IllFormed (projected);
  `CallValue` kills callee slot before args when `consumes_callee`.
- `tests.rs`: 6 tests (revive, revive-then-move-again, projected-write-stays-illformed, consume-double→
  IllFormed, consume-once-legal, plain-Callable-reusable = the carve-out boundary).

## Measured (foreground)
- `reinit_accept.gg` → Value "new" (was IllFormed 102); `consume_callable_double_reject.gg` → IllFormed 102
  (was Value). Matches production (`consume_callable_once_error.gg` → E_DoubleMove; `move_and_reinit.gg` ACCEPT).
- **ggdef suite: 127 passed / 0 failed** (was 121, +6) + 8 integration binaries green incl.
  **`spec_conformance_ggdef` floor=195 MATCH=195**. `move_then_read_is_illformed` + all `d10b_*` still pass.
- All 15 `tests/fixtures/liveness/*.gg`: only the 2 target fixtures flipped; other 13 UNCHANGED.

## Write-through (design)
- **Re-init: NO write-through needed — docs ALREADY correct** (`language-reference.md:1118` "Reassigning a
  moved variable revives it"; rule 9 `:2284`; `book/11-ownership.md:143`+table `:480`). ggdef lagged its OWN
  ratified prose; the fix brings ggdef INTO conformance (the stronger position).
- **ConsumeCallable: ADD one sentence** (§4.2 Callable Trait Types, after `:483`): *"A `ConsumeCallable` is
  single-owner: calling it consumes the callable (its captured environment is moved out), so it can be
  called at most once. A second call — or any use after the call — is a compile-time use-after-move
  (`error[E_DoubleMove]`). `Callable`/`MutCallable` are reusable."* Optional mirror to `book/11-ownership.md`.
- **NEW: document the ggdef dynamic-oracle boundary** (the branch-merge finding above) — one paragraph.

## Fixture migration (once fixed)
1. Strip `KNOWN-ORACLE-BUG` headers from `reinit_accept.gg` (:3-5) + `consume_callable_double_reject.gg`
   (:4-6); rewrite `integration.rs:18855-18877` from "beyond ggdef… filed" to "now AGREE with ggdef". KEEP
   the `self_host_driver_{accepts,rejects}_liveness` assertions.
2. Add 2 hand-authored `spectests/run/` seeds (`liveness_reinit_accept.gg`, `liveness_consume_callable_double.gg`)
   with `#!spectest`/`# mode: run`/`# adjudicator: ggdef` frontmatter; `cargo run -p ggdef -- gen …` fills
   `expect:` (exit 0 "new\n"; exit 102). **The IllFormed reject seed MUST be hand-authored** — auto-migrate
   only takes the Value/Agree bucket (`classify.rs`).
3. Bump `GGDEF_MATCH_FLOOR` 195→197 in `spec_conformance_ggdef.rs` in the SAME commit.

## Consult-history: CLEAN (no STOP)
`decisions.md` pins NOTHING for write-to-moved/re-init/ConsumeCallable-call. In fact the ledger MANDATES the
direction: D10(a) (`:485-504`) "a move-bind kills the source — one live name after" ⇒ revive on rebind;
`:457` "Acceptance = ggdef fixture-for-fixture"; D5 (`:128`) says the kind-classification is a separate
(unpinned) axis. Fixing an unpinned early-dev guard, overturning nothing.

## Size: ~175 LOC, 4 files, ALL under `spec/ggdef/src/` (pure Rust). NOT bootstrap-gated. Docs +1-3 sentences + the dynamic-oracle note. Migration +2 spectests + floor bump.
