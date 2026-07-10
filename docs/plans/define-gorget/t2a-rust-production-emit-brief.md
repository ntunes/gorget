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
- Touch ONLY: NEW `src/trap.rs`; `src/ir/lowering/*` (the compiler-emit sites — incl. registering
  `gorget_trap`'s ABI `[CStr, CStr]` in `fn_extern_abi_kinds` at `src/ir/lowering/mod.rs:~991`, mirroring
  the `gorget_panic` `[CStr]` registration; without it the new 2-`Str`-arg extern gets default ABIs);
  `src/lir/lower/insts.rs` (unwrap); **`src/lir/runtime.rs`** (only if W3 approach (a) — the 4-arg
  `AssertFailValues` sig); `src/backend/c_lir/*` (inline arith + the `gorget_panic`→ rewrite); `src/backend/llvm/mod.rs`
  (inline arith + shift check + the rewrite + decls); `src/backend/c/runtime/panic_normal.c` +
  `panic_test.c` (the new `gorget_trap`/`gorget_trap_at` entries); **`runtime_string.c`** (add the
  `gorget_trap`/`gorget_trap_at` forward-decls next to the existing `gorget_panic`/`gorget_panic_at`
  ones at `runtime_string.c:~457-459`, since runtime files concatenate with `runtime_string.c` BEFORE
  `panic_normal.c`); **`runtime_tostr.c`** (the message-less comparison-assert reroute, W3);
  `tests/lints.rs` (parity + source-scan ratchet lints); `tests/spec_conformance.rs` (floor bumps
  ONLY); targeted `tests/fixtures/*.gg` / `tests/integration.rs` for the shift + message-less-assert
  tests. `Cargo.toml` if a `[dev-dependency]` on ggdef is needed for the parity lint (the scout says
  ggdef is ALREADY a dev-dep — verify).
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
- Mirror both in `panic_test.c` (the setjmp `#[test]` capture path). **⚠ Capture the BARE `detail`
  argument** into `__gorget_test_fail_msg` (exactly as `gorget_panic_at` captures `msg`) and take the
  `__gorget_in_test` longjmp branch — do NOT `exit(101)` and do NOT capture the `trap[...]`-wrapped
  string. If you capture the wrapped string, `test_failure` (`tests/integration.rs:~9713`, which asserts
  `FAIL: assertion failed: left == right` on a message-less `assert 1 == 2`, the exact
  `gorget_assert_fail_values` path W3 reroutes) BREAKS — and the base gate battery does NOT run it, so a
  wrong capture ships green and only the parent's full sweep catches it. (`helpers.rs:~1286` prints
  `FAIL: %s` from the captured pointer; `helpers.rs:~1261` `@should_panic` mid-string `strstr` survives
  either way, but `test_failure`'s anchored substring does not.)
- **⚠ LLVM arg-order footgun:** the LLVM decl must match the C signature — `declare void
  @gorget_trap_at(ptr, ptr, ptr, i32, i32)` = **(code, detail, file, line, col)**. Do NOT copy the
  ADJACENT `gorget_panic_at`'s order, which is `(ptr file, i32 line, i32 col, ptr msg)` — file-first,
  a DIFFERENT order. The inline-arith LLVM reroute that calls `gorget_trap_at` directly must pass args
  in the (code, detail, file, line, col) order.
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
  assert-with-message `src/ir/lowering/stmts/mod.rs:2550-2561` → `T_AssertFailed` AND thread the real
  span (Q-D — it currently emits `<unknown>:0:0`); user `panic()` `src/ir/lowering/exprs/calls.rs:573-577` → `T_Panic`.
- **⚠ ALSO reroute the MESSAGE-LESS comparison-assert path — do NOT leave it (invariant #8).** A
  message-less `assert a == b` takes a DIFFERENT route: `src/ir/lowering/stmts/mod.rs:2524-2534` →
  `gorget_assert_fail_values` (`src/backend/c/runtime/runtime_tostr.c:~125`), which internally calls
  the 1-arg `gorget_panic` → `<unknown>:0:0: assertion failed…` + exit 1. This is a USER-FACING assert,
  semantically identical to the message form — NOT a runtime-internal caller, so the T2a/T2b seam does
  NOT excuse it. Reroute it too, so BOTH assert forms → `trap[T_AssertFailed]` + exit 101.
  **⚠ `gorget_assert_fail_values` is a REGISTERED runtime fn** — `src/lir/runtime.rs:~563` gives it a
  canonical 3-arg sig `(CStr, GorgetString, GorgetString)` that `resolve_lir_sig` OVERWRITES the LIR
  extern's `params`/`param_abis` from (`src/lir/lower/operands.rs:~211-217`). So you can NOT just
  prepend a `code` arg at the GIR emit site — a 4-arg GIR call against the 3-arg canonical sig fails LIR
  validation or mis-tags the ABIs. Two acceptable approaches; **prefer (a) for layering consistency**:
  **(a)** make `AssertFailValues`' sig 4-arg — prepend `(T::Ptr, A::CStr)` at `runtime.rs:~563`, update
  the C signature at `runtime_tostr.c:~125`, and pass `TrapKind::AssertFailed.code()` from the emit site
  (`:2524`) so the code is typed data uniform with every other reroute (parity-lint-covered). **This
  requires `src/lir/runtime.rs` in the scope fence (added below).** **(b) fallback ONLY if (a)
  entangles:** keep the 3-arg sig and let `gorget_assert_fail_values` pass a `"T_AssertFailed"` literal
  to `gorget_trap` internally — a single-caller single-meaning constant, BUT it's a hardcoded C string
  NOT sourced from `TrapKind::code()` and NOT pinned by the W4 parity lint (drift risk on a variant
  rename), so the W4 source-scan ratchet must explicitly cover it. Add a `tests/integration.rs` test
  asserting `assert 1 == 2` (message-less) traps `T_AssertFailed` + exit 101 on BOTH backends. **FILE a T1-zone
  follow-up:** a ggdef-generated `spectests/run/trap_assert_cmp.gg` conformance fixture for the
  message-less form (ggdef already models `Stmt::Assert`, so it's generatable — but adding a spectests
  fixture crosses into T1's zone + shifts MIN_FIXTURES/floors, so do it as a separate small follow-up,
  NOT in this brief).
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
  prelude fault set is `builtin_fault_enum()` (**`src/ir/lowering/generics/substitute.rs:323-347`** =
  {Overflow,DivByZero,Bounds} — NOT `src/semantic/`, a corrected citation); assert it equals exactly
  the `is_catchable()`-true variants. **NO message-text matching — the lint compares the typed `code()`
  SETS ONLY, never `detail()`/message wording** (production's `"integer overflow"` vs ggdef's
  `"arithmetic overflow"` is a sanctioned, conformance-ignored divergence — do NOT feed `detail()` in).
- A **source-scan ratchet lint** (NOT a match-arm count — the emit sites are `write!`-based inline
  strings across two backends, so `container_literal_arms_count`'s arm-counting shape does NOT apply).
  Assert the exact remaining count of raw trap-`exit(1)`/`fprintf`-trap occurrences in `c_lir/mod.rs` +
  `llvm/mod.rs` (post-reroute), so a NEW raw trap emit (a bare `fprintf;exit(1)` that bypasses the
  registry) forces review by tripping the count. Adjust the count deliberately when a site is
  legitimately added/removed. **Include the legacy `Inst::InlineC` fatal `call void @exit(i32 1)` at
  `llvm/mod.rs:~6975-6977` in the baseline COUNT DELIBERATELY** — it is NOT a rerouted trap site (it's
  the InlineC-fallback abort), so the baseline must expect it, not read it as "one trap left to reroute."

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
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration test_failure test_should_panic assert_fails -- --nocapture 2>&1 | tee /tmp/t2ar_assert_$$.log  # the #[test]-harness assert-capture path (panic_test.c) — NOT in the base gates, catches a wrong `detail` capture
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
