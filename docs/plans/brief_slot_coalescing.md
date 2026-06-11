# BRIEF — Slot-coalescing in the SSA→C emitter (the frame fix; macOS-path keystone)

Status: v3 (pass-1+pass-2 folds; orchestrator draft from scout `agent-ae36233658b60182e`, GO, proven
prototype `docs/plans/coalesce_slots_prototype.diff`, 2026-06-11). Owner chose
the codegen route ("2 then 3"); the scout found Option 2 unsound for the SSA
backend → **this is Option 3, the rustc-equivalent**, the only viable codegen
fix. Unblocks the plain-main revert (macOS fix for gorget-arena). Pass-1 fold
(fresh reviewer, core claims PASS, 3 reservations): EXACT-TYPE keying is
PER-EMITTER (the self-host `decl_ctype` is a simpler subset, no CStr branch —
don't key it on Rust's shape); `inst_uses` is a 57-arm enumerator = the SINGLE
HIGHEST-RISK site (a missing operand → an uncatchable slot-aliasing clobber),
port arm-for-arm vs Rust `Inst::uses()` + a 1:1 arm-count gate; line numbers
drift (anchor by name). Pass-2 fold (fresh reviewer, all 3 v1 folds verified
PASS): added the FORGOTTEN `term_uses` over `LirTerm` (liveness reads terminator
uses too — block-arg/phi liveness; same uncatchable-clobber class as `inst_uses`)
+ `lir_ssa.gg` to the zone + the cross-check gate.
✅ REVIEW-CLEAN: pass-3 (fresh) SIGN OFF — verified the `term_uses` fold against
source (the `LirTerm` operand set vs `Term::uses()`) AND confirmed there is NO
THIRD missing operand-accessor (`compute_live_blocks` reads exactly block-params
/`inst.uses()`/`inst.dst()`/`terminator.uses()` → `inst_uses`+`term_uses` is the
COMPLETE missing surface). 3 sequential fresh passes (3 → missing-term_uses →
clean). READY for the executor (run AFTER Block-2's battery clears — both gate
`fixed_point`; the executor wants solo CPU for byte-identity).

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
2. **EXACT-TYPE keying — PER-EMITTER, NOT symmetric.** Coalesce only values
   whose EXACT C-decl-type string matches — but **each emitter must key on ITS
   OWN `decl_ctype`** (the exact string it emits at its own value-decl site):
   Rust `c_lir/mod.rs` value-decl loop (~`:1915-1947`, incl. the `CStr → const
   char*` ~`:1927` and `void → void*` ~`:1934` specials); the self-host
   `lir_codegen.gg` value-decl loop (~`:4917-4936`) keys on `c_type_name(...)`
   (with `void → void*`). Do NOT key the self-host coalescing on the Rust
   decl-ctype shape; key on the self-host's own.
   ✅ **RESOLVED 2026-06-11 (`e2859be3`) — the REAL root cause was ADDRESS-ESCAPE,
   not cstr-keying (an earlier cstr "correction" here was a symptom-level
   misread).** The base coalescing broke `fixed_point` with a slot-aliasing
   CLOBBER that surfaced DOWNSTREAM as a spurious `gorget_str_from_cstr`
   double-wrap (stage-2 cc error). Root cause: the self-host block-param copy
   `emit_block_param_copies` (`lir_codegen.gg:~4914`, the `bp_ptr and arg_agg`
   arm) takes the ADDRESS of an aggregate block-arg (`__bp{param} = &__v{arg}`),
   so that value's storage outlives its SSA value-liveness across the block
   boundary (dereferenced in the SUCCESSOR) — an aliasing the liveness can't see.
   Coalescing such an address-escaped value reuses its storage → clobber. The
   Rust emitter does NOT have this (its `emit_jump_args` is always a plain by-VALUE
   `__bp = __v`, `mod.rs:~3402`), so the Rust side needed no change. FIX:
   `coal_addr_escaped_args` excludes address-escaped block-args from coalescing
   (private `__vN`), mirroring the `bp_ptr and arg_agg` predicate 1:1, guarded by
   the `coal_term_arg_lists_arms_count` lint. The synchronous call-arg `&__v`
   sites (`:4776`/`:4810`) need NO exclusion (the value is an `inst_uses` operand
   at the call → live there → per-block granularity already prevents the merge).
   Longer-term: converge the self-host block-arg ABI onto Rust's by-value shape so
   the asymmetry + the exclusion disappear (TODO codegen note).

## Both-emitter symmetry
- Rust: `c_lir/mod.rs` (prototype lives here). Determinism recommended (Rust
  emission is already run-to-run nondeterministic in value-decl typing — a
  pre-existing wart, not introduced here — but don't make it worse).
- Self-host: `lir_codegen.gg` value-decl loop ~`:4917`. The self-host has the
  CFG infra (`lir_ssa.gg:~46/127` `compute_predecessors`/`term_successors`) but
  needs an inst-`uses()` enumerator ADDED (it has `inst_dst()` ~`:2830` but no
  uses). ⚠ **This is the SINGLE HIGHEST-RISK site in the task — NOT "~1
  mechanical helper":** `inst_dst` is a 57-variant match, and the parallel
  `inst_uses` must enumerate EVERY operand value-id in EVERY arm — a SINGLE
  missing operand → an under-live range → a slot-aliasing CLOBBER that
  `c_emit_comparison` AND the emit-diff CANNOT catch (only the RUN gate would).
  The self-host `LirInst` is POSITIONAL (`ICall(dst, fid, args)`) vs Rust's
  named-field structs, so it CANNOT be copied blindly — port it ARM-FOR-ARM
  against the Rust gold reference `Inst::uses()` (`src/lir/mod.rs:~1099`),
  reading each self-host variant's operand POSITIONS from the `lir.gg` decl (NOT
  Rust field names). GATE: cross-check the `inst_uses` ↔ `Inst::uses()` arm set
  1:1 (add an arm-count lint, per "fix the class").
- ⚠ **ALSO needs `term_uses` over `LirTerm` — the brief's first cut FORGOT it
  (pass-2 catch):** liveness reads BOTH `inst.uses()` AND `terminator.uses()`
  (Rust `Term::uses()` `src/lir/mod.rs:~1238`). The self-host has `term_successors`
  (CFG edges only, `lir_ssa.gg:~127`) but NO terminator-operand accessor — and
  the TERMINATOR is exactly where the block-arg/phi liveness lives (`TBranch`
  cond + then_args + else_args, `TJump` args, `TRet` value, `TSwitch` value +
  case-args = the "arg live at end of pred" semantics). A missing terminator arg
  is the SAME uncatchable-by-emit-diff clobber class as a missing `inst_uses`
  operand. Add `term_uses` (~5-arm `LirTerm` match, near `term_successors` in
  `lir_ssa.gg`) ported arm-for-arm vs `Term::uses()`, with the SAME 1:1
  cross-check / arm-count gate.
- The self-host coalescing MUST be deterministic (it's what `fixed_point`
  exercises) and runs the SAME algorithm as Rust (clean symmetric port).
- ⚠ `c_emit_comparison` is BLIND to this change (it counts only `user_fn_count`
  = function-body `) {` openers, `tests/integration.rs` ~`:14030` — re-grep by
  name, all cited line numbers drift, NOT local
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
  (+ the new `inst_uses` helper there), `tests/fixtures/self_host_lowerer/lir_ssa.gg`
  (the new `term_uses` helper, near `term_successors`), possibly `src/lir/mod.rs`
  (only if a `uses()` accessor is missing — `Inst::uses()`/`Term::uses()` already
  exist), `tests/lints.rs`, TODO/DONE. Disjoint from the snag #11
  Block-2 zone (self-host `lower.gg`/`lower_closures.gg`) — but BOTH gate on
  `fixed_point`, so serialize execution (this runs AFTER Block 2 integrates).
- AFTER this lands, the plain-main revert (`docs/plans/lean_runtime_prototype.diff`
  plain-main hunk only — a CLEAN revert, not a paste) becomes viable; that step
  carries the owner sign-off on the deep-user-recursion `stack_guard` trade
  (TCO #11 / opt-in / expect-fail).
- Commit cites this brief + the scout; Co-Authored-By trailer.
