# T2a-rust — PRODUCTION trap emit, Rust backends (C + LLVM) (executor brief)

> **Wave position:** the production side of trap-normalization (D11). T1 (definition) LANDED. T2a
> makes production emit the ratified `trap[T_<Code>]: <detail> at file:line:col` + exit **101**
> that T1's fixtures expect. Per the T2a scout, T2a SPLITS: **T2a-rust (THIS brief — the Rust gg
> C+LLVM backends + the C runtime + registry + parity lint)** and **T2a-selfhost (sibling, the
> self-host `.gg` lowering — separate brief, disjoint file zone, parallelizable)**. T2b folds the
> runtime-library bounds path + `abort()` sites afterward.
>
> **Grounded in:** `docs/plans/define-gorget/scouts/scout-t2a-production-emit.md` (measured, with a
> working prototype), the LANDED T1 registry (`spec/ggdef/src/eval.rs` `TrapKind` + `spec/prose/trap-codes.md`),
> `decisions.md` D11, CLAUDE.md "No name matching" / layering rule 2.

## The scout's headline correction (internalize)
The prior whole-track scout was WRONG that overflow/divzero route through `gorget_panic`. **They are
emitted INLINE in both backends** (byte-identical stderr hid it): C at `src/backend/c_lir/mod.rs:2456-2542`
(`Inst::Add/Sub/Mul` + `Div/Rem/Mod`), LLVM at `src/backend/llvm/mod.rs` `emit_overflow_check`
(~:7250-7323) + div checks (~:3768/3803/3849/6899). So T2a-rust reroutes the INLINE arithmetic emit
in BOTH backends, plus the `gorget_panic`-based sites (assert/panic/unwrap). The scout PROVED the
reroute end-to-end: a prototype `gorget_trap_at` on the C `Inst::Add` site produced
`trap[T_Overflow]: integer overflow at f.gg:4:17` + exit 101 and flipped `trap_overflow` MISMATCH→MATCH.

## Owner rulings that constrain this brief
- **Shift-out-of-range → `T_Overflow`** (2026-07-10). The C backend already traps out-of-range shifts;
  LLVM does NOT check them (poison/UB — a C/LLVM parity bug). This brief: reroute the C shift trap →
  `gorget_trap_at(T_Overflow, …)` AND **add the missing LLVM shift-range check** → same, so both
  backends agree and both normalize. Keep the closed registry unchanged (no new variant).
- **Conformance compares T_ code + exit 101 ONLY** — detail is impl-defined (keep production's
  existing detail strings, Q-B). Unwrap detail keeps the variant word (Q-C).
- **assert gets a REAL span** now (single emit site, cheap — Q-D). (Bounds real-locations is T2b, per
  the earlier "thread locations now" ruling — NOT this brief; bounds stays MISMATCH here.)

## Scope fences (do NOT cross)
- Touch ONLY: NEW `src/trap.rs`; `src/ir/lowering/*` (the compiler-emit sites); `src/lir/lower/insts.rs`
  (unwrap); `src/backend/c_lir/*` (inline arith + the `gorget_panic`→ rewrite); `src/backend/llvm/mod.rs`
  (inline arith + shift check + the rewrite + decls); `src/backend/c/runtime/panic_normal.c` +
  `panic_test.c` (the new `gorget_trap`/`gorget_trap_at` entries); `tests/lints.rs` (parity lint +
  the arm-count lint); `tests/spec_conformance.rs` (floor bumps ONLY); targeted `tests/fixtures/*.gg`
  / `tests/integration.rs` for the shift test. Cargo.toml if a `[dev-dependency]` on ggdef is needed
  for the parity lint (the scout says ggdef is ALREADY a dev-dep — verify).
- Do NOT touch: the self-host `.gg` lowering (T2a-selfhost), the runtime-library `fprintf...exit(1)`
  bounds/OOM/channel sites + `runtime_array.c` (T2b), the c_lir `abort()`/134 sites (T2b),
  `spec/ggdef/*`, `spectests/*`, reference §10.9/§10.1. Do NOT change the 1-arg `gorget_panic`
  branch (runtime-internal callers stay old-path — that IS the clean T2a/T2b seam).

## Work items

### W1 — `src/trap.rs`: the production `TrapKind` registry (mirror ggdef EXACTLY)
- New crate-root module `src/trap.rs` (readable by `src/ir/lowering` + `src/backend/*` — no layering
  inversion). A closed `TrapKind` with the SAME 8 variants as ggdef (`spec/ggdef/src/eval.rs`,
  the landed `TrapKind`): `Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError, UnwrapErrorOnOk,
  AssertFailed, Panic`. `code(&self) -> &'static str` = an exhaustive no-catch-all `match` →
  `"T_<Variant>"` (mirror `SemanticErrorKind::code()`, `src/semantic/errors.rs:641` — rustc
  exhaustiveness IS the ratchet). `is_catchable()` = `true` for `Overflow | DivByZero | Bounds` only.
  Optionally `detail()` for production's default per-class strings (Q-B — keep existing wording).
- This is a DUPLICATE of ggdef's registry by design (Q3 — the import ratchet forbids ggdef importing
  `src/`, and definitional independence is the point). The parity lint (W6) pins the correspondence.

### W2 — the runtime entries (`panic_normal.c` + `panic_test.c`)
- Add `gorget_trap_at(const char* code, const char* detail, const char* file, int line, int col)` →
  `fprintf(stderr, "trap[%s]: %s at %s:%d:%d\n", code, detail, file, line, col); exit(101);`. The `T_`
  name is DATA passed from `TrapKind::code()` — NO C-side name table (layering rule 2; the only spelled
  symbol is the fixed `gorget_trap_at`).
- Add a 2-arg `gorget_trap(const char* code, const char* detail)` extern (the `<unknown>:0:0` form) for
  emit sites that lack a span, symmetric to `gorget_panic`/`gorget_panic_at`. The C/LLVM boundary
  rewrite (W3) rewrites `gorget_trap`→`gorget_trap_at` threading the span, exactly as it does
  `gorget_panic`→`gorget_panic_at` today (`emit_call_extern.rs:45-68`, `llvm/mod.rs:4507-4530`).
- Mirror both in `panic_test.c` (the setjmp `#[test]` capture path) so tests that trigger a trap under
  the test harness capture identically.
- LEAVE the existing 1-arg `gorget_panic`/`gorget_panic_at` (exit 1) untouched.

### W3 — reroute the compiler-emit trap sites (the core)
Two mechanisms (per the scout §3):
- **(A) Inline arithmetic — both backends.** C `c_lir/mod.rs:2456-2542` (Add/Sub/Mul → `T_Overflow`;
  Div/Rem/Mod div-by-zero → `T_DivByZero`, div-overflow `INT_MIN/-1` → `T_Overflow`) and LLVM
  `emit_overflow_check` (~:7250-7323) + div checks: replace the inline `fprintf(...);exit(1)` with a
  call to `gorget_trap_at(<T_code>, <detail>, <span>)`. The span is available at these emit sites
  (they already print `file:line:col` today). Detail keeps the current wording.
- **Shift-out-of-range (owner ruling → `T_Overflow`).** Reroute the C shift trap → `gorget_trap_at(T_Overflow,…)`.
  **ADD the missing LLVM shift-range check** (mirror the C check + the existing LLVM overflow/div check
  pattern) → `gorget_trap_at(T_Overflow,…)`, so `x << 64` traps identically on both backends. Add a
  `tests/integration.rs` test (or a `tests/fixtures` + `run_gg`) asserting BOTH backends emit
  `trap[T_Overflow]` + exit 101 for an out-of-range shift. (A `spectests/run/trap_shift.gg` conformance
  fixture is BLOCKED on ggdef modeling shift — FILE that as a T1-zone follow-up, do NOT add it here.)
- **(B) `gorget_panic`-based sites — via the new `gorget_trap`/`gorget_trap_at` extern.** Reroute:
  unwrap `src/lir/lower/insts.rs:3603-3638` (None → `T_UnwrapNone`, Error → `T_UnwrapError`, unwrap_error
  on Ok → `T_UnwrapErrorOnOk`; the variant words are at `:4195-4247` — keep them in detail, Q-C);
  assert `src/ir/lowering/stmts/mod.rs:2550-2561` → `T_AssertFailed` AND thread the real span (Q-D —
  it currently emits `<unknown>:0:0`); user `panic()` `src/ir/lowering/exprs/calls.rs:573-577` → `T_Panic`.
  These call the new `gorget_trap`(code, detail) extern; the C/LLVM rewrite (extend
  `emit_call_extern.rs:45-68` + `llvm/mod.rs:4507-4530` + the `@gorget_trap_at` decl near
  `llvm/mod.rs:1506`) rewrites it to `gorget_trap_at` threading the span — the SAME machinery the
  `gorget_panic` rewrite already uses.
- **NOTE the `functions.rs:88-107` `gorget_panic` blocks are the fault-scope participating-callee path
  (DCE'd for `main`, comment `functions.rs:80-83`)** — the scout flagged these are NOT the reachable
  top-level arithmetic path. Reroute them for consistency IF they carry a TrapKind cleanly, but they
  are not what flips the fixtures; do not get stuck here.

### W4 — the parity lint + arm-count lint (`tests/lints.rs`)
- A lint pinning **(a) production `TrapKind` ↔ ggdef `TrapKind`**: import both (ggdef is a `[dev-dependency]`
  — verify `Cargo.toml`) and assert the `code()` string SETS are identical and `is_catchable()` agrees
  variant-for-variant. **(b) the §10.9 `Fault` LANGUAGE prelude enum ↔ `is_catchable()` subset**: the
  prelude fault set is `builtin_fault_enum()` (`src/semantic/substitute.rs:323-347` = {Overflow,DivByZero,Bounds});
  assert it equals exactly the `is_catchable()`-true variants. NO message-text matching — compare typed
  code sets.
- An **arm-count lint** pinning the compiler-emit reroute sites through the registry (so the next new
  trap emit can't silently reintroduce a raw `fprintf;exit(1)` or a bare `gorget_panic`). Model on
  `container_literal_arms_count`.

### W5 — floor bumps (`tests/spec_conformance.rs`)
- The 7 non-bounds trap fixtures (overflow, divzero, unwrap_none, unwrap_error, unwrap_error_on_ok,
  assert, panic) now MATCH on the C + LLVM lanes. Bump `C_MATCH_FLOOR` and `LLVM_MATCH_FLOOR` 187 → **194**.
  **`SELFHOST_MATCH_FLOOR` STAYS 187** (self-host still emits old-format — T2a-selfhost bumps it).
  `trap_bounds` STAYS MISMATCH on all lanes (T2b). Verify: exactly the 7 flip on C/LLVM, bounds stays
  MISMATCH, and NO baseline regression.

## Gate battery (run FOREGROUND, generous timeouts; PASTE actual output)
```
cargo build
cargo test --lib 2>&1 | tee /tmp/t2ar_lib_$$.log
cargo test --test lints 2>&1 | tee /tmp/t2ar_lints_$$.log   # parity + arm-count lints green
GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1 --nocapture 2>&1 | tee /tmp/t2ar_conf_$$.log
# both backends agree on the trap format — spot-run a few by hand + the shift test:
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration trap -- --nocapture 2>&1 | tee /tmp/t2ar_trap_$$.log
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration self_host_bootstrap_fixed_point -- --nocapture 2>&1 | tee /tmp/t2ar_boot_$$.log
```
Acceptance: builds (both backends); `--lib` + lints green (incl. the new parity + arm-count lints);
`spec_conformance` shows **C 194 MATCH / LLVM 194 MATCH** (the 7 trap fixtures flipped, `trap_bounds`
still MISMATCH) + **self-host still 187 MATCH + 8 MISMATCH** + 0 BUILD-FAIL, floors green; the C and
LLVM backends emit BYTE-IDENTICAL `trap[T_<Code>]: <detail> at file:line:col` + exit 101 for each class
(verify by hand-running one program per class on each backend); `x << 64` traps `T_Overflow` on BOTH
backends; bootstrap fixed-point green (the C runtime gains `gorget_trap_at` additively; the self-host
emit is unchanged so the bootstrap is unaffected). Do NOT run the full `cargo test --test integration`
(parent's job).

## Worktree & agent discipline (NON-NEGOTIABLE)
Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm BOTH inside your worktree (under
`/workspace/gorget/.claude/worktrees/`); STOP if either is `/workspace/gorget` or `/workspace/gorget-1`.
Paths RELATIVE to your worktree; on Edit desync re-Read + retry the Edit tool (NEVER a heredoc with an
absolute path); after any non-Edit write run `git -C /workspace/gorget status` and STOP if it shows
changes. Entry: `git merge --ff-only gorget-1 2>/dev/null || true`. **Checkpoint to `/tmp/t2ar_report_$$.md`
after each work item.** Stage ONLY exact files by name; NEVER `git add -a`/`.`/`commit -a`; NEVER `git
stash`. Commit on your worktree branch, message ending with the two trailers:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01YWwxrY4NAvQ5uv43X4VjHL
```

## Deliverable
Per work item: files+file:line and one-line what. PASTED gate output — the `spec_conformance` lane
summary (C/LLVM 194 MATCH, self-host 187, bounds still MISMATCH), the parity-lint result, and the
by-hand C-vs-LLVM byte-identical trap output for each class + the `x << 64` both-backend agreement.
Note the filed follow-up (ggdef model shift + `trap_shift.gg`). Any brief premise that differed from
reality (corrected). Branch + commit hash. Honest reporting: red gates shown, not hidden.
