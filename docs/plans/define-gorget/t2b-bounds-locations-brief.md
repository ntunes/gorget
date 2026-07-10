# T2b — bounds trap normalization + REAL location (the LAST slice of D11) (executor brief)

> **Wave position:** the final D11 slice. T1 + T2a-rust + T2a-selfhost + T3a + T3b all LANDED; all 4
> conformance lanes emit `trap[T_<Code>]` + exit 101 EXCEPT the bounds trap (each lane shows 194
> MATCH / 1 MISMATCH = `trap_bounds.gg`). T2b flips `trap_bounds` on all lanes AND threads a REAL
> source location into the flagship `v[i]` bounds trap (owner ruling) → all floors 194→195 =
> MIN_FIXTURES, **completing D11**. After T2b: the round is DONE — the parent hands off (do NOT start D12).
>
> **Grounded in:** `docs/plans/define-gorget/scouts/scout-t2b-bounds-locations.md` (measured, with a
> working prototype that flipped `spec_conformance_c` 194→195 with a real location), `src/trap.rs`,
> `src/backend/c/runtime/panic_normal.c` (`gorget_trap`/`gorget_trap_at`).

## The scout's de-risking headlines (internalize)
1. **All 3 lanes emit the bounds trap through the SAME shared runtime helper `gorget_array_get`**
   (`src/backend/c/runtime/runtime_array.c:31-37` — `index out of bounds` + `exit(1)`). Rust C, Rust
   LLVM, AND the self-host all link `src/backend/c/runtime`. **So the self-host lane flips for FREE
   from the runtime change — NO self-host `.gg` edit, NO driver rebuild** (scout-measured). This is
   structurally unlike T2a (whose overflow was inline per-backend).
2. **The owner's real-location ruling is PROVEN reachable** — `block.span_map` already resolves `loc`
   for the `v[i]` `CallExtern`. The scout prototyped `gorget_array_get_at(arr, idx, code, file, line,
   col)` + a C-emit-boundary rewrite → `trap[T_Bounds]: index out of bounds: index 5, length 3 at
   trap_bounds.gg:18:13` + exit 101, and `spec_conformance_c` went 194→195 (0 regressions). NO
   per-call-site ABI fan-out.
3. **The Fault RE-PANIC gap is DESCOPED from T2b** (was W3; pass-1 review R1 reshaped it). The
   `gorget_panic("<category>")` re-panic emits are NOT in the conformance corpus (the 7 trap fixtures
   hit the DIRECT paths), so T2b flips `trap_bounds` + reaches 195/195/195 WITHOUT touching them. And
   normalizing them RIGHT is a both-compiler change: the brief's original sites (`functions.rs:88/95/102`)
   are DEAD (DCE'd, per their own `functions.rs:79-83` comment); the LIVE cross-frame re-panic is
   `fill_fault_return_block` at `functions.rs:190` (called for the 3 categories at ~:1083/1178/1196),
   and the live fault-scope re-panic is `exprs/mod.rs:3786/3793/3800`. Rerouting ONLY the Rust side
   would REINTRODUCE the self-host↔Rust divergence T2a-selfhost deliberately avoided (the self-host has
   the twin cross-frame re-panic at `lower_closures.gg:93` + `emit_fault_repanic_block`). So the WHOLE
   fault-re-panic normalization (Rust live fault-scope + live cross-frame + self-host twins, all 3
   categories, category-correct) is ONE both-compiler follow-up — see the FILE item in W3.
4. **Fault-catch is UNTOUCHED** — the catchable path uses `gorget_array_safe_get` (returns NULL,
   `src/lir/lower/insts.rs:1249`), a separate helper. Do not touch it.
5. **The `abort()`/134 `Inst::*Check`/`Inst::Trap` sites are DEAD** (never constructed — grep-verified;
   the prior T2a-selfhost scout's "self-host bounds = inline abort 134" was WRONG). Only the
   `emit_hof.rs:156`/`emit_types.rs:246` unwrap-on-Ok `abort()` is a reachable latent 134 defect.

## Owner-ruling / decided (do NOT re-open)
- **Real location for the flagship `v[i]` READ path** (Layer B). The rarer bounds-class sites
  (string index/slice, `shared` array, `bytes` offset) stay SPAN-LESS in T2b (Layer A) — FILE per-site
  span plumbing as a follow-up. (Delivers the owner's flagship-debuggability intent; the read path is
  THE common bounds error.)
- **Q-A: hardcode `"T_Bounds"` in the span-less bounds helper** is acceptable — it is a single-purpose
  bounds helper, not a routing table, and the `trap_bounds.gg` conformance fixture is the executable
  guard (a `Bounds` variant rename → `TrapKind::code()` changes → ggdef regenerates the fixture →
  production's stale `"T_Bounds"` MISMATCHes → caught). The flagship read path (Layer B) threads
  `TrapKind::Bounds.code()` as data anyway.
- **Q-C: fix the overflow/divzero re-panic siblings in T2b** (one-fix-all-siblings — same 6 adjacent
  emit lines; closes the T2a gap).

## Work items

### W1 — Layer B: the flagship `v[i]` READ gets a REAL location (Rust C + LLVM)
- Add `gorget_trap`-family runtime entry `gorget_array_get_at(GorgetArray* arr, int64_t idx, const
  char* code, const char* file, int line, int col)` (mirror `gorget_array_get` `runtime_array.c:31-37`
  but on OOB call `gorget_trap_at(code, "index out of bounds: index N, length M", file, line, col)` +
  exit 101; on in-bounds return the element `void*` — identical fast path).
- **⚠ R2 — the in-bounds element DEREF is name-INDEPENDENT on BOTH backends; do NOT touch
  `is_collection_void_return`.** The `v[i]` read's `gorget_array_get` CallExtern dst is ALWAYS `Ptr`
  (the extern returns `Ptr` — `src/lir/lower/insts.rs:1171`, `src/lir/runtime.rs:380`), and the element
  deref is the SHARED LIR `Inst::Load` emitted AFTER the call by `materialize_collection_element`
  (`src/lir/lower/insts.rs:1958-1966`; C emits it at `c_lir/mod.rs:2734`, LLVM as a Load). Because the
  dst is `Ptr`, `dst_needs_deref` is FALSE, so `is_collection_void_return` (`helpers.rs:638`) NEVER
  fires for this read — registering the new name there is inert, and reproducing a `dst = *(Type*)…`
  deref in the reroute branch would DOUBLE-DEREF (the downstream `Load` derefs again) → garbage.
  **So on BOTH backends the reroute is a single early-return branch** mirroring the existing
  `gorget_trap`→`gorget_trap_at` rewrite (C `emit_call_extern.rs:74-91`, LLVM `llvm/mod.rs:4597`): when
  emitting a `gorget_array_get` CallExtern that HAS a `span_map` entry, emit `<dst> =
  gorget_array_get_at(arr, idx, code, file, line, col)` into `ptr_val` (Ptr) and `return` — do NOT
  deref in that branch; the following `Inst::Load` handles it. (`mod.rs:3097/5465` match only
  guard/shared helpers — untouched.)
- Reroute the COMPILER-EMITTED `v[i]` read from `gorget_array_get` → `gorget_array_get_at`, threading
  `TrapKind::Bounds.code()` + the `loc` (file/line/col from `block.span_map`) at BOTH emit boundaries
  (C: `src/backend/c_lir/*` / the emit site the scout used; LLVM: the existing `gorget_trap` rewrite at
  `src/backend/llvm/mod.rs:~4597` region). **⚠ R3 — the scout proved the C `_at` path end-to-end but
  the LLVM `_at` path is UNPROVEN (its LLVM measurement was the span-less normalization). VERIFY the
  LLVM `_at` reroute, do NOT assume symmetry** — actually diff the C vs LLVM trap line for
  `trap_bounds.gg`. Confirm C and LLVM emit BYTE-IDENTICAL `trap[T_Bounds]: … at file:line:col` + 101.

### W2 — Layer A: span-less bounds normalization (flips the fixture on all lanes; fix-the-class)
- In `src/backend/c/runtime/*.c`, change the INDEX/BOUNDS-family OOB from `fprintf(…"index out of
  bounds"…); exit(1)` / `gorget_panic("index out of bounds")` → `gorget_trap("T_Bounds", "<detail>")`
  (span-less, exit 101). The span-less `gorget_array_get` itself (internal callers) + the siblings:
  `gorget_array_set/remove/insert/swap/slice`, `gorget_str_index/slice/byte`, `gorget_shared_array_get`,
  the `bytes` offset checks (~30 sites — the scout's Layer-A census). **LEAVE OFF `T_Bounds`:**
  capacity-overflow / alloc-failed / pop-from-empty (those are resource/empty, → `T_Panic` or stay as
  the runtime-lib class — NOT index bounds).
- **The self-host lane flips for FREE here** (it links the same runtime) — do NOT edit any `.gg`.
- **Blast-radius check:** Layer A rewrites str/shared/bytes OOB stderr `gorget: panic: …` → `trap[T_Bounds]`.
  Before committing, grep for any exact-`gorget: panic:`-prefix or exact-`exit==1` assertion on those
  ~30 sites OUTSIDE the `run_gg_panics`/`security_traps` families (which assert nonzero+substring, safe);
  fix any exact-format assertion. The parent's full integration run is the backstop, but one grep saves a cycle.
- **R5 — use a single shared constant for the Layer-A code** (e.g. `#define GG_T_BOUNDS "T_Bounds"` in
  the runtime header) rather than ~30 separate `"T_Bounds"` literals, so a future `Bounds` rename can't
  PARTIALLY drift (only `gorget_array_get`-via-`trap_bounds.gg` is fixture-guarded; str/shared/bytes
  are not). Q-A accepted the hardcode; this just keeps it single-sourced.

### W3 — FILE the fault-re-panic normalization as a both-compiler follow-up (do NOT do it here)
The uncaught-fault RE-PANIC still emits old-format `gorget_panic("<category>")` + exit 1. Do NOT touch
it in T2b (descoped per headline #3 — it's not in the corpus, and a Rust-only fix reintroduces a
self-host divergence). Instead, REPLACE the existing `TODO.md` "both-compiler cross-frame-fault-repanic"
follow-up (filed by T2a-selfhost) with a COMPREHENSIVE version that names ALL the live sites and the
both-compiler requirement:
- **Rust:** the live fault-scope re-panic `src/ir/lowering/exprs/mod.rs:3786/3793/3800` AND the live
  cross-frame re-panic `fill_fault_return_block` `src/ir/lowering/functions.rs:190` (invoked per-category
  at ~:1083/1178/1196). The `functions.rs:88/95/102` scope-level blocks are DEAD (DCE'd, `:79-83`) —
  reroute for hygiene or note as dead.
- **self-host:** the twin cross-frame re-panic `lower_closures.gg:93` + `lower_expr.gg` `emit_fault_repanic_block`.
- **All at once, category-correct** (overflow→`T_Overflow`, divzero→`T_DivByZero`, bounds→`T_Bounds`,
  NEVER `T_Panic`) via a shared producer helper (`emit_uncaught_fault_trap(builder, TrapKind)`), so an
  uncaught cross-frame/fault-scope fault surfaces the same `trap[T_X]` + exit 101 as a direct one — on
  BOTH compilers, keeping them in parity. Closing sibling-grep: `grep -rn '"integer overflow"\|"division
  by zero"\|"index out of bounds"' src/ir/lowering/ tests/fixtures/self_host_lowerer/`. Own scout→brief→gauntlet.

### W4 — the reachable abort()/134 fold (Q-D, bounded)
- Fold `src/backend/c_lir/emit_hof.rs:156` + `emit_types.rs:246` (unwrap-on-Ok → `abort()` exit 134 —
  a reachable latent defect) to `gorget_trap` with the right code. Verify reachability first; if
  genuinely dead, note it and skip. The `Inst::BoundsCheck/DivCheck/Trap` variants are DEAD (never
  constructed) — do NOT touch them.

### W5 — floors + ratchet + doc-comments
- `tests/spec_conformance.rs` `C/LLVM/SELFHOST_MATCH_FLOOR` all 194 → **195** (= MIN_FIXTURES) — after
  the reroute, `trap_bounds` MATCHes on all lanes. Verify total=195 · MATCH=195 · MISMATCH=0 per lane.
- **R4 — do NOT lower the `raw_trap_exit_sites_ratchet` baseline.** The ratchet (`tests/lints.rs:~5042`)
  counts `abort()` in `c_lir/mod.rs` ONLY (`c_abort=3` = the 3 DEAD `Inst::*Check` arms at
  `c_lir/mod.rs:3123/3132/3138`). W4 folds `emit_hof.rs`/`emit_types.rs` — DIFFERENT files — so
  `c_abort` stays 3; leave the baseline. Instead, update the STALE ratchet text — BOTH the doc-comment
  (`lints.rs:~5029-5031`, "When T2b normalizes them, lower this baseline") AND its TWIN in the ratchet's
  assert MESSAGE (`lints.rs:~5062-5064`) — to note the 3 dead `Inst::*Check` arms are intentionally
  retained (not touched by T2b).
- Update stale `exit(1)`/`abort` doc-comments in the touched runtime `.c` + any bounds fixture that
  cites the old format.

## Gate battery (run FOREGROUND, generous timeouts; PASTE output)
```
cargo build
cargo test --lib 2>&1 | tee /tmp/t2b_lib_$$.log
cargo test --test lints 2>&1 | tee /tmp/t2b_lints_$$.log                       # parity + ratchet (adjust count if W4)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test spec_conformance -- --test-threads=1 --nocapture 2>&1 | tee /tmp/t2b_conf_$$.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration trap -- --nocapture 2>&1 | tee /tmp/t2b_trap_$$.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration fault -- --nocapture 2>&1 | tee /tmp/t2b_fault_$$.log   # regression: T2b does NOT touch the fault re-panic — confirm the fault_* fixtures stay GREEN (unchanged)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration self_host_bootstrap_fixed_point -- --nocapture 2>&1 | tee /tmp/t2b_boot_$$.log
```
Acceptance: builds (both backends); `--lib` + lints green (ratchet count updated if W4 folded abort);
`spec_conformance` shows **C 195 / LLVM 195 / self-host 195 MATCH, 0 MISMATCH, 0 BUILD-FAIL** (all
floors 195 = MIN_FIXTURES — `trap_bounds` flipped on ALL lanes, including the self-host FOR FREE);
the flagship `v[i]` bounds trap carries a REAL `file:line:col` (byte-identical C vs LLVM); the
`fault_*` fixtures stay GREEN unchanged (T2b does NOT touch the fault re-panic — descoped to the W3
follow-up); the `run_gg_panics`/`security_traps` families stay green (they assert nonzero-exit +
substring, not exit==1); bootstrap green (the runtime change is additive; the self-host emits the same
`gorget_array_get` call, now normalized in the shared runtime). Run the ~3-min bootstrap FOREGROUND
(rule 9). Do NOT run the full `cargo test --test integration` (parent's job).

## Scope fences
- Touch: `src/backend/c/runtime/*.c` (Layer A + `gorget_array_get_at`), `src/backend/c_lir/*` +
  `src/backend/llvm/mod.rs` (Layer B rewrite + W4), `tests/spec_conformance.rs` (floors),
  `tests/lints.rs` (the stale ratchet comment), touched runtime `.c` doc-comments, **touched
  `tests/fixtures/*.gg` stale-comment edits** (e.g. `fault_bounds_panic_default.gg`, whose comment
  still says "panics … exit(1)" — becomes `trap[T_Bounds]` + 101), `TODO.md` (file the rarer-bounds-sites
  span follow-up + REPLACE the cross-frame-repanic follow-up with the comprehensive W3 version).
- Do NOT touch: **`src/ir/lowering/*` (the fault re-panic is DESCOPED to the W3 follow-up — do NOT
  reroute it in T2b)**, the self-host `.gg` lowering (flips FOR FREE), `gorget_array_safe_get` (the
  fault-catch path), the dead `Inst::*Check` variants, `spec/ggdef/*` (ggdef already models
  `Trap(Bounds)`), `spectests/*`, the reference §10.9.

## Worktree & agent discipline (NON-NEGOTIABLE)
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm BOTH inside your worktree; STOP if either is
`/workspace/gorget` or `/workspace/gorget-1`. Paths RELATIVE to your worktree; on Edit desync re-Read +
retry the Edit tool (NEVER a heredoc with an absolute path); after any non-Edit write `git -C
/workspace/gorget status` and STOP if it shows changes. Entry: `git merge --ff-only gorget-1
2>/dev/null || true`. **Checkpoint to `/tmp/t2b_report_$$.md` after each work item.** Run the FINAL
bootstrap gate FOREGROUND with `GG_BUILD_TIMEOUT_SECS=600` — do NOT background then end (rule 9). Stage
ONLY exact files by name; NEVER `git add -a`/`.`/`commit -a`; NEVER `git stash`. Commit on your
worktree branch, message ending:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01YWwxrY4NAvQ5uv43X4VjHL
```

## Deliverable
Per work item: files+file:line + one-line what. PASTED gate output — the `spec_conformance` summary
(C/LLVM/self-host ALL 195 MATCH / 0 MISMATCH), the flagship `v[i]` real-location trap line (C AND
LLVM, byte-identical), the `fault_*` fixtures still green (unchanged), bootstrap green. The TWO
`TODO.md` follow-ups (rarer-bounds-sites span-plumbing + the REPLACED comprehensive both-compiler
fault-re-panic item). Whether W4's abort() sites were reachable (folded) or dead (skipped). Any scout
premise that differed. Branch + commit hash.
