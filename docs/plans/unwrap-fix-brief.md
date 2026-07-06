# EXECUTOR BRIEF: unwrap/expect panic-by-default (🔥 both-backend + self-host, 4 zones)

> **STATUS: v3 FINAL — pass 3 (launch-gate) = CLEAN SIGN OFF 2026-07-06 (all anchors re-verified; both fresh-eyes seeds dismissed: Term::Unreachable+noreturn pattern already live+tested both backends via panic_builtin; the witness position-proxy is the ONLY textual assertion in the blast zone). ✅ LANDED 2026-07-06 (executor 0cad419a+9382cab0+5024e538 → output-review SIGN OFF, reference-grade gate passed → merged; parent battery running: floors reconcile on the combined tip). Was: EXECUTOR LAUNCHED w/ pass-3 notes: Zone-3 insertion = lower_expr.gg:3502-3504 disjoint from the lir_codegen combinator routes; Zone-1 guard wrapped in `!is_unwrap_or` at BOTH :4126 and :4153 (prototype lacks it).**
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
Zone 1's shape and message text. (⚡ STRUCK by pass-1 R1: lir_codegen.gg IS in scope — see the FOLDS section; the
combinator cases there are the ONLY unwrap_error path and are also unguarded.) Add a guard test: a user program
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
  empty), `unwrap_error_result_traps`, `expect_none_traps` (⚡ per pass-1 R3: assert ONLY
  bug-agnostic substrings — non-zero exit + a fragment like `` `None` value `` — NEVER the
  full generic text; cite the filed expect-message TODO in the fixture comment),
  `unwrap_error_on_ok_traps`, `get_unwrap_empty_traps`.

## ⚡ PASS-1 FOLDS (2026-07-06) — these OVERRIDE conflicting text above/below

- **R1 (HIGH — Zone 3 corrected: the self-host class is SPLIT across two files).** The
  `lower_expr.gg:3280` intercept handles only `unwrap`/`unwrap_or`/`expect` — `unwrap_error`
  is NOT there; its SOLE self-host emit path is **`lir_codegen.gg:5135`**
  (`case "__result_unwrap_error"`), which reads the Error payload with NO tag check. The
  adjacent `__result_unwrap` (:5127) / `__result_expect` (:5163) / `__option_unwrap`
  combinator cases are also unguarded (the fallback path behind the inline route). Zone 3
  therefore = **BOTH** the `lower_expr.gg` inline path AND the `lir_codegen.gg`
  combinator cases (the draft's "NOT lir_codegen.gg" is STRUCK). Guard-test coverage must
  include an `unwrap_error`-on-Ok program compiled by the self-host driver, not just
  unwrap-None.
- **R2 (Zone 2 is a BEHAVIOR CHANGE, flagged):** `RTError` is variant index 13; the
  pre-fix `get_rtype_at(v,-1)` extracted a zeroed payload = tag 0 = `RTPrimitive("")`
  garbage. The guard changes that garbage to `RTError` — the CORRECT direction (aligns
  with Rust's reserved `error_id` → `ResolvedType::Error`, types.rs:156-157), but it may
  move `c_emit_comparison`/`runtime_diff` (UP = expected improvement; DOWN = STOP).
  **Guard shape: `if i < 0` ONLY** — the prototype's extra `or i >= v.len()` upper bound
  is DROPPED for Rust-parity (Rust's `get` is a bare index that panics on positive OOB;
  masking positive OOB would hide real bugs). Re-measure stage-0 + bootstrap with the
  narrowed guard.
- **R3 (expect fixture must not cement the dropped-message bug):** `expect_none_traps`
  asserts only bug-agnostic substrings (non-zero exit + a stderr fragment like
  `` `None` value `` that stays true once the message-threading bug is fixed) — NEVER the
  full generic unwrap text as the expected canonical output. Cite the filed expect-message
  TODO in the fixture comment.
- **R4 (ggdef gate scoped):** ggdef's phase-0 Method surface is `Unwrap`/`UnwrapOr` only —
  the ggdef cross-check applies to unwrap-None/unwrap-Error probes ONLY; `unwrap_error`/
  `expect` yield ggdef elaboration errors (outside subset) and that is NOT a gate failure.
  The `` `Ok` value `` message text is compiler-chosen (reference-§15.2-consistent), not
  ggdef-ratified.
- **R5 (Zone 3 idiom + decoupling trigger):** Zone 3 was NOT in the scout prototype — it
  is the highest-risk zone (bootstrap-critical emit path). The self-host GIR has **no
  GTUnreachable**: follow the ASSERT-LOWERING idiom (`lower_stmt.gg:1407-1445` — noreturn
  `gorget_panic` + `GTJump` dead-edge), NOT a literal mirror of Zone 1's
  `Term::Unreachable`. If `self_host_bootstrap_fixed_point`/`runtime_diff` fails on
  Zone 3, DECOUPLE: ship proven Zones 1+2+4, report Zone 3 with the failure evidence for
  its own track — do not block the round.
- **R6 (fallback guard gating):** the no-StructId fallback branch (insts.rs:4154) also
  serves `unwrap_or` — gate the panic guard on `!is_unwrap_or` (a defaulting extractor
  must never panic), or add a debug assertion that unwrap_or never reaches the fallback.

## ⚡ PASS-2 FOLDS (2026-07-06) — same override precedence as PASS-1 FOLDS

- **R-A (Zone-2 contingency — mirror R5's decoupling for the OTHER unproven leg):** the
  narrowed `i < 0` guard + the FULL fixed-point combination is UNMEASURED (the prototype
  measured stage-0 only, WITH the upper bound). If the fixed-point traps in Zone-2
  territory — the narrowed `get_rtype_at`, positive OOB, or a SIBLING typed accessor
  (`get_int_at`/`get_stype_at` at types.gg:35/38 share the identical `v.get(i).unwrap()`
  shape, called throughout infer.gg) — the playbook is: investigate the specific inflow,
  guard the CLASS (all three accessors, each with its natural error/default sentinel —
  RTError for the ResolvedType one), FILE the root cause, and do NOT silently restore the
  `i >= v.len()` upper bound (it masks positive-OOB bugs Rust would panic on). Fix the
  class, never redesign around the gap.
- **R-B (gate BOTH guard call sites on `!is_unwrap_or`):** the prototype calls the guard
  at the with-StructId branch (insts.rs:4126) AND the fallback (:4153); `unwrap_or` can
  reach the with-StructId branch when `lir_args.len() <= 1` (:4063 else-arm). Apply the
  `!is_unwrap_or` gate at BOTH sites (or centralize the decision) — a defaulting
  extractor must never panic, at either site.
- **R-C (the `unwrap_error` message names `unwrap_error()`):** the message for
  unwrap_error-on-Ok is `` called `unwrap_error()` on a `Ok` value `` (per the measured
  prototype) — NOT `unwrap()`. This path is outside ggdef's subset (R4) and the fixtures
  assert substrings (R3), so no gate catches a wrong method name — get it right at the
  write site. The draft's "matches ggdef exactly" applies to the unwrap-None/Error
  messages only.
- **Citation corrections (cosmetic):** lir_codegen.gg has NO `case "__result_unwrap"` —
  :5127 is a section comment; bare `Result.unwrap` routes through the INLINE path (the
  live combinator routes are `__option_unwrap|__option_expect` :5129, `__result_expect`
  :5162, `__result_unwrap_error` :5135). The assert-idiom GTJump dead-edge is at
  lower_stmt.gg:1446. `move_param_concat` (integration.rs:5044) is a live run_gg test
  (the strmove track un-gaps its fixture) — still a disjoint region from this track's
  integration.rs edits.
- **Optional (cheap, recommended):** diff the narrowed-guard stage-0 emitted C against
  the scout's `clean_stage0.c` to localize the RTError-vs-`RTPrimitive("")` behavioral
  delta before running the heavy fixed-point.

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
