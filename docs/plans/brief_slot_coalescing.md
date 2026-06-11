# BRIEF — Slot-coalescing in the SSA→C emitter (the frame fix; macOS-path keystone)

Status: v1 (orchestrator draft from scout `agent-ae36233658b60182e`, GO, proven
prototype `docs/plans/coalesce_slots_prototype.diff`, 2026-06-11). Owner chose
the codegen route ("2 then 3"); the scout found Option 2 unsound for the SSA
backend → **this is Option 3, the rustc-equivalent**, the only viable codegen
fix. NEEDS ≥3 fresh brief-reviews before launch. Unblocks the plain-main revert
(macOS fix for gorget-arena).

## Mission
Add liveness-based stack-slot coalescing to gg's SSA→C emitter so the self-host
compiler's fat frames shrink (`lower_expr_inner` 182,640 B → ~92,000 B at -O0)
and it self-compiles on a plain ~8 MB stack — eliminating the need for the
forced 64 MB-pthread main. **Both compilers, SYMMETRIC** (Rust
`src/backend/c_lir/mod.rs` + self-host
`tests/fixtures/self_host_lowerer/lir_codegen.gg`). This is a behavior-preserving
codegen optimization — the ROOT fix for "gg doesn't coalesce slots; rustc does"
(only the self-host overflows because it's gg-compiled at -O0; the Rust binary
is rustc-compiled and already coalesces, so the Rust-side change is for
symmetry, not its own stack).

## Why (measured root cause)
gg emits each function as ONE flat C scope: all `__v{N}` SSA value-locals
declared up front (`c_lir/mod.rs` value-decl loop ~`:1924`, slot-decl ~`:1898`),
goto-based `__bb{N}` blocks. There's dead-decl elimination (Fix A,
`mark_used_value_ids` ~`:2029`) but NO coalescing — every referenced value gets
its own C local even when live ranges are disjoint. At -O0 the C compiler gives
each its own stack slot → the frame is the SUM of all (mutually-exclusive) match
arms' locals.

## Proven prototype (`docs/plans/coalesce_slots_prototype.diff`, behind `GG_COALESCE=1`)
Standard SSA value liveness (backward dataflow over the block CFG; block-args as
phi: arg live at end of pred, param at start of succ — reuse the existing
`Inst::uses()`/`Term::uses()`/`successors()` in `src/lir/mod.rs`), then greedy
interval-coloring per C-decl-type → one C local per coalesced slot, emitted as
`#define __vN __coalK` aliases (ZERO body rewrite) + `#undef` after the close.
**Measured:** `lower_expr_inner` 182,640 → 91,680 B (−50%); 7,833 used locals →
187 coalesced; plain-8 MB EMPIRICALLY clears (`ulimit -s 8192`: baseline
SIGSEGV on the 51-term `+` chain, coalesced exit 0, budget ~doubles, ~3 MB
headroom; flipped-CoW fits); 100/100 fixtures `gg run`-identical, 60/60 self-host
driver emits byte-identical C, ASan/UBSan clean (one pre-existing UBSan finding,
unrelated). NO slot-aliasing (greedy shares a slot only between values whose
block-live-sets are provably disjoint).

## THE TWO PRODUCTIONIZING REQUIREMENTS (the prototype's known gaps — DO NOT SHIP WITHOUT)
1. **DETERMINISM (load-bearing for `fixed_point`).** The prototype groups via
   `HashMap` → non-deterministic slot numbering across runs. This does NOT
   affect correctness but WOULD break `fixed_point` byte-identity (the self-host
   must self-reproduce stage-2==stage-3==stage-4). The real impl MUST use a
   deterministic ordered grouping: sort the C-decl-type keys, iterate value ids
   ASCENDING, assign slots in that fixed order. Verify by emitting the same
   fixture twice → byte-identical.
2. **EXACT-TYPE keying.** Coalesce only values whose EXACT C-decl-type string
   matches (incl. the `CStr → const char*` and `void → void*` special cases at
   `c_lir/mod.rs:~1936-1953`), so two slot-sharing values declare identically.
   The prototype's `coalesce_assign_exact` does this — keep it.

## Both-emitter symmetry
- Rust: `c_lir/mod.rs` (prototype lives here). Determinism recommended (Rust
  emission is already run-to-run nondeterministic in value-decl typing — a
  pre-existing wart, not introduced here — but don't make it worse).
- Self-host: `lir_codegen.gg` value-decl loop ~`:4917`. The self-host has the
  CFG infra (`lir_ssa.gg:~46/127` `compute_predecessors`/`term_successors`) but
  needs an inst-`uses()` enumerator ADDED (it currently only has `inst_dst()`
  ~`lir_codegen.gg:2830`) — mechanical, ~1 helper. The self-host coalescing MUST
  be deterministic (it's what `fixed_point` exercises) and should run the SAME
  algorithm as Rust (clean symmetric port).
- ⚠ `c_emit_comparison` is BLIND to this change (it counts only `user_fn_count`
  = function-body `) {` openers, `tests/integration.rs:~13995`, NOT local
  declarations) → it stays matched trivially. **The real symmetry/correctness
  gate is `fixed_point` byte-identity** (the self-host emitter's determinism +
  self-consistency) — NOT c_emit_comparison.

## Correctness validation (emit-diff alone is INSUFFICIENT — the scout was explicit)
A slot-aliasing bug (two simultaneously-live values → one slot) would NOT be
caught by an emit byte-diff. The gate must RUN:
- Full-corpus `gg run` output IDENTICAL coalesced-vs-baseline (the scout's
  100/100 — extend to the full corpus).
- ASan + UBSan build of the coalesced self-host C runs clean (no
  coalescing-introduced read-of-uninit / clobber).
- `self_host_bootstrap_fixed_point` GREEN — the load-bearing net (self-host
  self-reproduces byte-identically with the deterministic coalescing).

## Gates (executor; parent re-runs the battery)
- `cargo build`; `cargo test --lib`; `cargo test --test lints`.
- DETERMINISM check: emit the same fixture twice (both compilers) → byte-identical.
- Full-corpus `gg run` coalesced-vs-baseline → IDENTICAL output; ASan/UBSan clean.
- `self_host_bootstrap_fixed_point` GREEN (`GG_BUILD_TIMEOUT_SECS=600`) — both
  the Rust-emitter coalescing (builds the self-host) AND the self-host-emitter
  coalescing (self-reproduces) must hold.
- `c_emit_comparison` + `lowerer_comparison` no regression (read counts).
- `self_host_runtime_diff` no regression (parity-neutral — it's an optimization).
- MEASURE: re-confirm `lower_expr_inner` `-fstack-usage` frame drop (~−50%) and
  that the self-host driver self-compiles under `ulimit -s 8192` (the empirical
  plain-8 MB proof, with the deterministic version).

## Constraints
- Worktree preamble; explicit-file `git add`; no push; STOP on a contradicted
  premise (esp. a non-deterministic emit, a slot-aliasing run-diff, or
  `fixed_point` regressing).
- Zone: `src/backend/c_lir/mod.rs`, `tests/fixtures/self_host_lowerer/lir_codegen.gg`
  (+ the new `inst_uses` helper there), possibly `src/lir/mod.rs` (if a `uses()`
  accessor is missing), `tests/lints.rs`, TODO/DONE. Disjoint from the snag #11
  Block-2 zone (self-host `lower.gg`/`lower_closures.gg`) — but BOTH gate on
  `fixed_point`, so serialize execution (this runs AFTER Block 2 integrates).
- AFTER this lands, the plain-main revert (`docs/plans/lean_runtime_prototype.diff`
  plain-main hunk only — a CLEAN revert, not a paste) becomes viable; that step
  carries the owner sign-off on the deep-user-recursion `stack_guard` trade
  (TCO #11 / opt-in / expect-fail).
- Commit cites this brief + the scout; Co-Authored-By trailer.
