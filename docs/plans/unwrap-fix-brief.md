# EXECUTOR BRIEF: unwrap/expect panic-by-default (🔥 both-backend + self-host, 4 zones)

> **STATUS: v1 DRAFT — review passes: (none yet; ≥3 sequential fresh passes required before launch).**
> Scout: full report + measured prototype at `/tmp/recover_unwrap/` (proto_FINAL.patch,
> probes/ with emitted C + ASan bins both backends, integ_full_c.log, clean_stage0.c).
> Scout measured: fix green on BOTH backends, ASan clean, lib 1105/0, full C sweep 1539/4
> with all 4 failures diagnosed (3 = the exposed self-host latent bug, 1 = a fragile
> textual assertion). ggdef message-text parity confirmed.

## The bug (verified on main 485ef0bd)

`unwrap()`/`expect()`/`unwrap_error()` emit NO tag check on either backend —
`Option[int] o = None; o.unwrap()` prints `0` at exit 0; String payloads print empty;
`unwrap_error()` on Ok prints empty at exit 0 (the emit_hof `abort()` path is DEAD — the
LIR path intercepts first). Reference §15.2 says "panics". ggdef traps (101). Two prior
premises are STALE: no 134-abort behavior, and no SIGSEGV for String payloads (zeroed
struct prints empty) — do not cite them.

## The fix (scout-ratified, prototype measured green)

**Zone 1 — `src/lir/lower/insts.rs` (the shared write site, both backends inherit):**
Tier 2a of `emit_extern_call` handles `__option_unwrap`/`__result_unwrap`/
`__result_unwrap_error` (GIR emits bare externs from `ir/lowering/exprs/methods.rs:975/
1046`). Add ONE helper `emit_unwrap_panic_guard` called from BOTH extraction branches
(the with-StructId branch, was :4127, and the no-StructId raw-pointer fallback, was
:4153): `Load` tag (I32 field 0) → `Cmp Eq` valid tag → `Term::Branch(ok_bb, panic_bb)`;
panic_bb = `CallExtern{"gorget_panic", [StrLit], [AbiKind::CStr]}` + `Term::Unreachable`
(C-emit auto-rewrites to `gorget_panic_at(file,line,col,msg)`; LLVM → `unreachable`).
Valid tag: **0 = Some/Ok** for unwrap/expect (NOTE: `EnumKind::Option`'s doc comment
claiming "Tag 0 = None" is WRONG — filed; cite the real convention in the helper doc),
**1 = Error** for unwrap_error. Message TEXT matches ggdef exactly: `` called `unwrap()`
on a `None` value `` / `` …`Error` value `` / `` …`Ok` value ``. The class = {unwrap,
expect, unwrap_error/unwrap_err} — all three flow through the same Tier-2a gate; no
other panicking extractors exist (verified).
**Exit code stays production's current `1`.** D11's ratified 101 lands with the
trap-normalization TRACK (all trap sites swept uniformly in one commit) — do NOT
piecemeal it here.

**Zone 2 — `tests/fixtures/self_host_typechecker/types.gg` (the exposed latent bug):**
the tag-check makes the self-host trap while compiling ITSELF: `get_rtype_at(v, i)`
(types.gg:33 `return v.get(i).unwrap()`) is called 7× with `i = -1` (the self-host's
unresolved-type sentinel; Rust's TypeId is u32 + `ResolvedType::Error`, so it can't hit
this). Guard: `if i < 0: return RTError()` (mirrors "unresolved ⇒ Error"). Measured:
stage-0 self-compile rc=0, 34MB C, no trap. The deeper -1-inflow question is FILED as
its own investigation (TODO) — the executor applies the guard + a comment pointing at
that entry, nothing more.

**Zone 3 — `tests/fixtures/self_host_lowerer/lower_expr.gg:3280` (NEW work — the
self-host as a COMPILER must emit the same check):** the self-host's plain-unwrap emit
path (its own comment admits it does not emit the None panic) must emit the tag-check +
`gorget_panic_at` branch for user `unwrap()`/`expect()`/`unwrap_error()`, mirroring
Zone 1's shape and message text. (NOT `lir_codegen.gg`'s `__option_unwrap` case — that's
a secondary path; direct `o.unwrap()` emits inline.) Add a guard test: a user program
compiled BY the self-host driver must trap on unwrap-None with the same stderr (the
spec_conformance selfhost lane pattern or a targeted integration test — executor's
choice, cite precedent).

**Zone 4 — `tests/integration.rs` + `tests/fixtures/`:**
- Relax `witness_never_emitted_c_clone_shape`'s TEXTUAL-ORDER assertion (the new panic
  block lands at a high block-ID, reordering `borrow_view` vs `clone_to_owned` in the
  emitted text). Scout verified the SEMANTIC content is intact (exactly 1 clone + 1
  borrow_view, output correct) — keep those asserts, drop only the position proxy.
  A reviewer must confirm the order is genuinely not load-bearing.
- New `run_gg_panics` pairs (exit≠0 — these CANNOT be spectests run-tier until D11's
  `trap:` field lands; the helper at integration.rs:6125 asserts non-zero + stderr-
  contains, which fits): `unwrap_none_traps` (None literal + `v.get(i).unwrap()` on
  empty), `unwrap_error_result_traps`, `expect_none_traps` (NOTE: currently prints the
  generic unwrap message — the dropped-expect-message bug is FILED separately; assert
  the generic text and cite the TODO), `unwrap_error_on_ok_traps`, `get_unwrap_empty_traps`.

## Zone summary + hazards

Zones: `src/lir/lower/insts.rs` · `tests/fixtures/self_host_typechecker/types.gg` ·
`tests/fixtures/self_host_lowerer/lower_expr.gg` · `tests/integration.rs` +
`tests/fixtures/` (new panics fixtures). **NO TODO.md/DONE.md** (parent-only). Do NOT
touch the dead `abort()` fossil paths (filed for later retirement). CONCURRENT TRACK:
strmove touches `src/ir/lowering/exprs/operators.rs` + a different region of
integration.rs — disjoint files except integration.rs (different regions; merge-
reconcilable, stay away from the concat/witness areas it edits... correction: witness
is YOURS; strmove touches move_param_concat's `#[ignore]` — different tests, fine).

## Gates (foreground, generous timeouts, tee to /tmp/unwrap_*)

- `cargo build` · `cargo test --lib` (1105/0).
- The probe matrix on BOTH backends (panic + happy paths) + ASan on the emitted C.
- `run_gg_panics` slices + the new fixtures.
- **`self_host_bootstrap` AND `self_host_bootstrap_fixed_point`** (the scout ran only
  stage-0; the FULL fixed-point must be green — Zones 2+3 change self-host source, this
  is the bootstrap-gated path; `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`,
  `--test-threads=1` for the bootstrap tests).
- The self-host emit-check guard test (Zone 3) green.
- ggdef cross-check on the probes (`cargo run -p ggdef -- run` — Trap verdict + message).
- Check `c_emit_comparison` + `runtime_diff` floors are UNAFFECTED or IMPROVED (Zone 3
  adds emitted-C branches; the comparison counts function definitions so should hold —
  verify, don't assume; if a floor moves DOWN, STOP and report).
- Parent-driven after merge: full sweeps both backends.

## Non-goals

The expect-message threading (filed M) · the -1-inflow investigation (filed) · the
abort() fossils (filed L) · exit-code 101 unification (the D11 trap-normalization track)
· any spectests/ggdef changes.
