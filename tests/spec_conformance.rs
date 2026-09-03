//! The **per-implementation conformance lanes** (RFC §4) — the three PRODUCTION
//! implementations adjudicated against the same executable definition the ggdef
//! lane uses.
//!
//! For every `spectests/run/*.gg` fixture the ggdef lane
//! (`spec/ggdef/tests/spec_conformance_ggdef.rs`) diffs the DEFINITIONAL
//! interpreter against the committed `#!spectest` `expect:` block. These three
//! lanes close the same loop for the shipping compiler, once per backend:
//!
//!   * `spec_conformance_c`        — `gg build` (default C backend) → run.
//!   * `spec_conformance_llvm`     — `gg build --backend=llvm` → run. Each lane
//!                                   PINS its backend on the build command; it
//!                                   deliberately does NOT read `GG_BACKEND`, so
//!                                   a single `cargo test --test spec_conformance`
//!                                   exercises all three regardless of the env.
//!   * `spec_conformance_selfhost` — the self-host driver `--emit-c` → `cc` →
//!                                   run. The driver builds ONCE per process via
//!                                   a `OnceLock` (mirrors `driver_paths()`,
//!                                   tests/smith/main.rs:223).
//!
//! Verdicts, per fixture, per lane:
//!   * **MATCH**       — the impl reproduced the committed `expect:` (exit +
//!                       stdout, per the comparison rule in `adjudicate`).
//!   * **MISMATCH**    — the impl BUILT and RAN but the observed `(exit, stdout)`
//!                       disagrees with the committed `expect:`.
//!   * **BUILD-FAIL**  — the impl failed to PRODUCE a runnable binary (a gg /
//!                       cc / driver / llc stage error). A distinct verdict from
//!                       MISMATCH: a fixture that never ran did not "disagree",
//!                       it failed to compile. This is a first-class outcome, NOT
//!                       an error — a check-accepted program that fails at
//!                       cc/llc is a both-backend defect the lane surfaces
//!                       (core invariant #8), and the floor treats it as a
//!                       non-MATCH until it is fixed.
//!
//! The per-fixture table is ALWAYS printed (a diagnostic). Then each lane
//! enforces an INLINE, monotone MATCH-count floor with a fixtures-count guard,
//! mirroring `parity_floor_active` (tests/integration.rs:99) MINUS its
//! GG_BACKEND carve-out — that carve-out exists there because a set `GG_BACKEND`
//! silently flips the WHOLE run's backend; here each lane pins its own, so the
//! env cannot change what a lane exercises.
//!
//! ## Floors (regenerate IN-WORKTREE; never a dated number)
//!
//!   cargo test --test spec_conformance -- --test-threads=1 --nocapture
//!
//! The three PRODUCTION floors (C/LLVM/self-host) are the count of committed
//! fixtures each production impl reproduces today. `MIN_FIXTURES` is the TOTAL
//! committed corpus count (the glob-emptiness guard). The C and LLVM floors
//! equal it; the SELF-HOST floor sits ONE BELOW, on `d22_slice_clamp.gg` — see
//! `SELFHOST_MATCH_FLOOR`, which names the gap and its cause. (An earlier
//! revision of this paragraph claimed all three lanes reached the whole corpus,
//! contradicting the constant ten lines below it.)
//! The former self-host staging — the floor once held FIVE below the corpus on
//! two KNOWN, FILED gaps: the four single-owner-Callable init rejects
//! (E_MoveWithoutOperator) the self-host typechecker did not yet enforce (it
//! ACCEPTED them) plus one Copy-axis struct-field ACCEPT
//! (`copy_struct_field_borrow_ok.gg`) its scalar-only Copy axis wrongly REJECTED
//! — is CLOSED: the self-host now enforces the single-owner INIT class at the
//! init boundaries and computes the struct/enum Copy axis (`compute_is_copy`), so
//! those five fixtures MATCH the self-host lane too. Every fixture MATCHes all
//! three lanes.
//!
//! (History: the C and LLVM lanes once floored one below self-host because
//! `smith_move_param_concat.gg` was a both-backend BUILD-FAIL — the C backend
//! emitted an invalid pointer-add, the LLVM backend an invalid `add ptr`, while
//! the self-host lowerer was already correct. That defect is FIXED
//! (`src/ir/lowering/exprs/operators.rs` `cow_deref_if_ptr`). Later the self-host
//! held one below on a reject-diagnostic-rendering gap — its reject headline
//! lacked the `error[E_<code>]` bracket — which is ALSO now closed: the self-host
//! renders the ratified `error[E_<code>]` family off its typed `DiagKind`, so all
//! three lanes are level at the whole corpus.)

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

use ggdef::{parse_frontmatter, Expect};

// ── Floors — regenerated in-worktree (see the module-doc command). ──────────
// T2a-rust (D11 production trap emit, Rust C+LLVM backends), T2a-selfhost (the
// self-host `.gg` lowerer emit), AND T2b (bounds trap normalization) all landed:
// ALL 8 trap fixtures (overflow, divbyzero, bounds, unwrap_none, unwrap_error,
// unwrap_error_on_ok, assert, panic) now emit `trap[T_X]` + exit 101 and MATCH on
// ALL THREE lanes (C, LLVM, self-host). T2b normalized the shared runtime bounds
// path (`gorget_array_get` → `gorget_trap` + the whole index/bounds class), so
// `trap_bounds` flipped on every lane — the self-host FOR FREE (it links the same
// runtime), no `.gg` edit — reaching 187 + 8 = 195 exit-0/trap fixtures.
//
// REJECT fixtures (`expect.reject`, exit 1, empty stdout, an `error[E_X]` marker
// — the ratified verdict-triple for a may-move static rejection): a reject
// fixture MATCHes a lane iff that lane REFUSES it at check/build time with the
// declared `E_` code (`adjudicate_lane`). The first reject seed,
// `reject_use_after_move.gg`, MATCHes the C and LLVM lanes (both emit
// `error[E_UseAfterMove]` from the shared semantic checker — backend-independent,
// so the +1 is on BOTH). Its ACCEPT complement `reinit_accept.gg` (a moved slot
// revived by a whole-local reassign → `Value "new"`, exit 0) MATCHes ALL THREE
// lanes, so the corpus + every lane's MATCH rise by that fixture together.
//
// The reject seed now MATCHes the SELF-HOST lane too (the tracked gap is CLOSED):
// `render_diagnostic` renders the ratified `error[E_<code>]:` headline off the
// TYPED `DiagKind` via `diag_kind_code` (the `[E_<code>]` segment rides inside the
// colored severity run so `error[E_UseAfterMove]` stays a contiguous substring for
// `extract_reject_code`). `DkUseAfterMove` was split into `DkUseAfterMove`
// (E_UseAfterMove) + a new `DkDoubleMove` (E_DoubleMove) so each reject carries its
// own registry code — `tests/fixtures/self_host_typechecker/diagnostic.gg`
// (`diag_kind_code` + the headline) and typecheck.gg's double-move push site. With
// the render aligned, ALL THREE lanes reproduce the whole corpus.
//
// The render alignment then let the four remaining driver-only liveness/move
// rejects migrate into four-lane conformance fixtures (each was previously only a
// `self_host_driver_rejects_liveness` message assertion in tests/integration.rs):
// `reject_move_in_loop.gg` (E_MoveInLoop), `reject_use_after_move_branch.gg`
// (E_UseAfterMove, the branch-join union), `reject_consuming_self_use_after_move.gg`
// (E_UseAfterMove via a `!self`-consuming method), and `reject_consume_callable_double.gg`
// (E_DoubleMove via a single-owner ConsumeCallable). Together with the patch's
// `reject_double_move.gg` that is FIVE new coded rejects — all four-lane MATCH — so
// every floor rose 197 → 202 in lockstep with the corpus.
//
// RV-F added 12 fixtures (Copy axis, loop revive-seeding, for-var MoveInLoop, the
// single-owner Callable init class). All TWELVE MATCH on ALL THREE production
// lanes. Five are ACCEPT seeds (`copy_field_borrow_ok`,
// `loop_reassign_revive_move_ok`, `loop_body_local_move_ok`,
// `callable_move_bind_return_ok`, `callable_param_rebind_ok`); the other seven are
// rejects — `reject_borrow_conflict_noncopy_field.gg` (E_BorrowConflict),
// `reject_for_var_move_in_loop.gg` (E_MoveInLoop), the four single-owner-Callable
// init rejects (`reject_callable_bind_bare` / `_ctor_bare` / `_enum_variant_bare`
// / `_for_var_bind`, all E_MoveWithoutOperator), and the Copy-axis struct PIN
// `copy_struct_field_borrow_ok.gg` (an all-int struct-field bare read under `&h`
// that ACCEPTs + runs). C and LLVM reject/run via the shared backend-independent
// semantic checker; the self-host renders each reject code off its typed
// `DiagKind`. Every floor rose by all TWELVE (202 → 214).
//
// The self-host lane now reaches all twelve. The two former gaps are CLOSED:
// (a) it enforces the single-owner INIT class at the bind / reassign / ctor /
// struct-literal / enum-variant boundaries (`reject_single_owner_init`, params
// exempt only at bind/reassign — mirroring production's
// `require_explicit_move_for_single_owner_init` + `check_value_needs_move`), so
// the four bare-init programs REJECT E_MoveWithoutOperator instead of building +
// running; and (b) it computes the struct/enum Copy axis (`compute_is_copy`, the
// DUAL of `compute_drop_taint`), so `copy_struct_field_borrow_ok.gg`'s all-int
// struct is Copy — the D10(b) place-overlap check ACCEPTs the bare field read
// under `&h` (was a scalar-only BUILD-FAIL). The self-host floor now equals the
// corpus (202 → 214), level with C and LLVM.
//
// D29 (visible error propagation) added SIX gate-8 seeds — all four-lane MATCH:
// two ACCEPT+run (`d29_unmarked_capture_accept`, `d29_tvariant_marked_match_accept`
// — the snag48 T-variant-marked-match) and four rejects, all
// `error[E_MissingFallibleMark]` (`d29_bare_throws_discard_reject` [kind-1 bare],
// `d29_kind2_bare_discard_reject` [kind-2 bare], `d29_mark_capture_reject`
// [redundant mark on capture], `d29_marked_match_result_arms_reject`
// [`match f()!: case Ok/Error` peeled arms]). C/LLVM reject/run via the shared
// frontend; the self-host renders the reject off its typed `DkMissingFallibleMark`
// + runs the accepts through the D29-enforced `check_safety_*` walk. Every floor
// rose by all SIX (214 → 220). (The D29 chain also migrated 8 pre-existing
// `throws` spectests the earlier corpus migration had missed — behavior-
// preserving marks, so the count is unchanged, only their build-verdict restored.)
// R47 Track D1 (+2): `reject_no_method_on_float.gg` and
// `reject_no_method_on_string.gg` — the primitive-receiver E_NoMethodFound
// class. ⚠ RATCHETING THESE IS PART OF ADDING A FIXTURE, not bookkeeping. (At
// the time of this entry the floors were `matched >= FLOOR` and the glob guard
// was `len() >= MIN_FIXTURES`; the glob guard is an EXACT PIN as of R48 close —
// see below.) Under those `>=` guards a new fixture never counted in left every
// assert passing —
// including after the fix it pins is reverted. Verified for these two: with the
// self-host reject reverted, the SH lane MISMATCHes both and drops to 221 < 222.
// R48 Track A (+1): `combinator_callable_param.gg` — the corpus's first
// combinator coverage (Callable-param class). Main's floors at that integrate
// were 224/224/223 + `MIN_FIXTURES` 224.
// R48 Track β (+11): the DIRECTIVE-VALIDATION class, `todo/t0825`. Ten rejects
// — `reject_directive_{scheduler_unknown_value,scheduler_case_mismatch,
// scheduler_missing_value,unknown_name,unknown_name_with_value,retired_overflow,
// strip_asserts_valued,trace_valued,hot_reload_valued,
// implicit_clones_unimplemented}.gg`, all `error[E_UnknownDirective]` — plus one
// ACCEPT seed, `directive_scheduler_pool_ok.gg` (the `pool` mode, the one
// admitted scheduler value with no coverage anywhere: green by coincidence,
// since `pool` is also the lowerer's default).
//
// All ELEVEN MATCH on ALL THREE production lanes. C and LLVM reject through the
// shared frontend; the self-host renders `E_UnknownDirective` off the new typed
// `DkUnknownDirective` emitted by `validate_directives`
// (`self_host_typechecker/resolve.gg`). Before this round the self-host
// validated NO directive at all — it COMPILED, LINKED and RAN every one of the
// ten reject programs — so all ten are RED-verified on the SH lane by
// construction. Merged floors name both inflows: Track A's +1 combinator seed
// and Track β's +11 directive seeds (main 224/224/223/`MIN` 224 + 11 →
// 235/235/234/`MIN` 235). Taking tip's 234 drops A's fixture from the glob
// guard (silently loosen); taking main's 224 drops the eleven.
//
// ⚠ The GRAMMAR half of the same class (`directive trace=1`,
// `directive scheduler single`, `directive 42`, `directive trace single`) is a
// PARSE error on both lanes and carries NO registry code, so `adjudicate_lane`
// — which reads the `error[E_..]` marker — cannot own it. Those five cells are
// pinned two-lane in tests/integration.rs instead; do not "complete" this list
// by adding them here without a code.
// R48 close (+2, RESEEDED 2026-09-03): `reject_partial_move_field.gg` and
// `reject_partial_move_self.gg` — the D10 no-partial-moves class (`todo/t0437`,
// `todo/t0438`). ⚠ THESE TWO SAT UNRATCHETED, AND THE REASON IS THE LESSON: the
// D10 work was owner-directed and landed DIRECTLY ON MAIN rather than through a
// gauntlet track, so no output-review ever asked "did you ratchet the floors?".
// The paragraph above says ratcheting is PART OF ADDING A FIXTURE; that duty
// does not attach to the track pipeline, it attaches to the fixture.
// Nothing detected the drift for two fixtures, exactly as that paragraph warns:
// the floors WERE `matched >= FLOOR` and the glob guard WAS `len() >= MIN_FIXTURES`,
// both `>=`, so a corpus that grew without a ratchet left every assert GREEN.
// ⇒ THE GLOB GUARD IS NOW AN EXACT PIN (`assert_eq!`, below); the three lane
// floors remain `>=`, which is correct — their consumer genuinely is
// `matched >= floor`. Past tense here is deliberate: this paragraph records why
// the drift happened, not how the guard behaves today.
// Measured at final HEAD `d7d899c9b` (`--test-threads=1 --nocapture`, rc=0):
// C 237/237 · LLVM 237/237 · self-host 236/237, so both new fixtures MATCH on
// all three lanes and the sole SH mismatch remains `d22_slice_clamp.gg` below.
// 235/235/234/`MIN` 235 → 237/237/236/`MIN` 237.
const C_MATCH_FLOOR: usize = 237;
const LLVM_MATCH_FLOOR: usize = 237;
const SELFHOST_MATCH_FLOOR: usize = 236;
// SH lane doesn't yet reproduce d22_slice_clamp.gg — SH lowerer needs the
// Range-in-index lowering wired (parser mirror lands the syntax, but the
// lowerer's SIndex arm at self_host_lowerer/lower_expr.gg doesn't yet
// dispatch to the range-slice runtime call). Filed for R40 as part of the
// D22 C-3b (SH sites migration) track, which is hard-blocked on A1 anyway.

/// The glob-emptiness guard: `spectests/run` must contain at least this many
/// `.gg` seeds or a shrunken corpus would make a lane vacuously green. This is
/// the TOTAL seed COUNT — regenerate with `ls spectests/run/*.gg | wc -l`.
///
/// It equals the C and LLVM MATCH floors. The SELF-HOST floor sits ONE BELOW,
/// on `d22_slice_clamp.gg` (see `SELFHOST_MATCH_FLOOR`); adding a fixture
/// raises all four constants together.
const MIN_FIXTURES: usize = 237;

// ── THE RELATION ABOVE IS NOW ENFORCED, NOT ASSERTED IN PROSE (Core #14) ──
// The doc comment on MIN_FIXTURES claims "It equals the C and LLVM MATCH
// floors … adding a fixture raises all four constants together". That was an
// invariant-asserting comment with NOTHING behind it, sitting on the exact
// four constants that then drifted for two fixtures at R48. Core #14 gives two
// options — enforce it or delete it — and it is worth enforcing: C and LLVM are
// the reference lanes, and a spectest they cannot reproduce is a reference-grade
// defect (Core #8), not a floor to quietly lower.
//
// These are CONST asserts on purpose: they fail at COMPILE time, so unlike a
// `#[test]` they cannot be skipped by a name filter, cannot be `#[ignore]`d,
// and cannot false-RED under load.
//
// ⚠ IF YOU LAND A SPECTEST THE C OR LLVM LANE CANNOT MATCH, this breaks the
// build — deliberately. Fix the lane, or make the exemption an explicit,
// commented decision here. Do not "fix" it by lowering a floor silently; that
// is the drift this guard exists to stop.
const _: () = assert!(
    C_MATCH_FLOOR == MIN_FIXTURES,
    "C_MATCH_FLOOR must equal MIN_FIXTURES: the C lane is a reference lane and      must reproduce every committed spectest. Fix the C lane or amend this      guard deliberately."
);
const _: () = assert!(
    LLVM_MATCH_FLOOR == MIN_FIXTURES,
    "LLVM_MATCH_FLOOR must equal MIN_FIXTURES: the LLVM lane is a reference lane      and must reproduce every committed spectest. Fix the LLVM lane or amend      this guard deliberately."
);
// The self-host is the lane still catching up, so its floor is `<=` rather than
// `==` — but it may never EXCEED the corpus, which would mean a floor seeded
// from a stale, larger corpus.
const _: () = assert!(
    SELFHOST_MATCH_FLOOR <= MIN_FIXTURES,
    "SELFHOST_MATCH_FLOOR exceeds MIN_FIXTURES — a floor cannot require more      matches than the corpus has fixtures."
);

// ─────────────────────────── infrastructure ────────────────────────────
// tests/spec_conformance.rs is a SEPARATE test target from tests/integration.rs
// (and there is no tests/common/ in this repo), so integration's helpers
// (gg_binary, run_with_timeout, self_host_emit_cc_run, …) are not importable.
// The minimal subset is re-derived here from the same sources, mirroring the
// precedent tests/smith/main.rs:122-254 already sets.

/// Workspace root. This is a root-package test target, so `CARGO_MANIFEST_DIR`
/// is the repo root directly (same as tests/smith/main.rs).
fn ws_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Path to the pre-built `gg` binary (Cargo guarantees it is current before
/// this test process starts).
fn gg_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_gg"))
}

/// `GG_BUILD_TIMEOUT_SECS`, else a generous 600s default. The C/LLVM fixture
/// builds are ~1s each, but the self-host DRIVER build is a ~1-2 min release
/// (longer under debug), so the default is set to cover the slow lane rather
/// than the fast ones — one knob, honored everywhere.
fn build_timeout() -> Duration {
    Duration::from_secs(
        std::env::var("GG_BUILD_TIMEOUT_SECS").ok().and_then(|s| s.parse().ok()).unwrap_or(600),
    )
}

/// `GG_TEST_TIMEOUT_SECS`, else 30s — deadline for a produced test binary.
fn run_timeout() -> Duration {
    Duration::from_secs(
        std::env::var("GG_TEST_TIMEOUT_SECS").ok().and_then(|s| s.parse().ok()).unwrap_or(30),
    )
}

/// A child overran its deadline (killed).
struct TimedOut;

/// Run a command with a deadline, through the SHARED runner
/// (`gorget::proc_guard`).
///
/// ⚠ This was a hand-rolled copy, and it had the defect the correct copy's own
/// doc comment described one file away: a plain `child.kill()` reaps the direct
/// child and leaves every grandchild alive, spinning at ~100% CPU and poisoning
/// every later load-adjusted measurement on the box. It also drained with an
/// UNCAPPED `read_to_end` (the OOM class the capture cap exists to prevent) and
/// joined the drain threads AFTER the kill, so a grandchild holding the pipe
/// write end hung the timeout handler itself. All three are gone with the copy.
///
/// ⚠ The post-kill `stdout_thread.join()` this replaces was a CONFIRMED
/// DEADLOCK, and not only on the timeout path — the SUCCESS path joined
/// unconditionally too. A grandchild inheriting the pipe write end keeps it
/// open, `read_to_end` never returns, and the 3-lane adjudicator hangs with no
/// deadline above it.
///
/// Does NOT panic on timeout: a timeout is a lane classification here, not a
/// harness failure. An OVERFLOW is a harness failure and still panics — runaway
/// output is a miscompile signal, and silently returning truncated bytes as a
/// lane's answer would launder it into a MATCH.
fn run_cmd(cmd: &mut Command, timeout: Duration) -> Result<Output, TimedOut> {
    match gorget::proc_guard::run_with_deadline(cmd, timeout) {
        Ok(out) => Ok(out),
        Err(gorget::proc_guard::RunFailure::Deadline { .. }) => Err(TimedOut),
        Err(gorget::proc_guard::RunFailure::Overflow { cap }) => {
            panic!("{cmd:?} produced runaway output (>{cap} bytes) — killed")
        }
    }
}

/// Build the self-host driver ONCE per test process (mirrors
/// tests/smith/main.rs:223 `driver_paths`). Artifacts land at the shared,
/// gitignored `tests/fixtures/self_host_lowerer/driver{,.c}` paths (how the
/// existing self-host lane works — do not delete them). Returns
/// `(driver_exe, lib_dir, runtime_dir)` with `runtime_dir` ABSOLUTE (a relative
/// `--runtime-dir` only works by cwd luck). The driver build never reads
/// GG_BACKEND: the self-host lane's reference is pinned to the default C backend.
fn driver_paths() -> &'static (PathBuf, PathBuf, PathBuf) {
    static DRIVER: OnceLock<(PathBuf, PathBuf, PathBuf)> = OnceLock::new();
    DRIVER.get_or_init(|| {
        let manifest = ws_root();
        let main_path = manifest.join("tests/fixtures/self_host_lowerer/driver.gg");
        assert!(main_path.exists(), "driver source not found: {}", main_path.display());
        eprintln!("[spec_conformance] building self-host driver (once per process, ~1-2 min)…");
        let t0 = Instant::now();
        let out = run_cmd(Command::new(gg_binary()).arg("build").arg(&main_path), build_timeout())
            .unwrap_or_else(|_| {
                panic!(
                    "self-host driver build timed out after {}s (raise GG_BUILD_TIMEOUT_SECS)",
                    build_timeout().as_secs()
                )
            });
        assert!(
            out.status.success(),
            "self-host driver build failed:\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr),
        );
        eprintln!("[spec_conformance] driver built in {:.1}s", t0.elapsed().as_secs_f64());
        (
            manifest.join("tests/fixtures/self_host_lowerer/driver"),
            manifest.join("lib"),
            manifest.join("src/backend/c/runtime"),
        )
    })
}

// ─────────────────────────── verdict + adjudication ─────────────────────

/// One fixture's outcome on one lane.
enum Verdict {
    Match,
    Mismatch(String),
    BuildFail(String),
}

/// Compare a produced binary's `(exit, stdout)` against the committed `expect:`.
///
/// Comparison rule (mirrors the ggdef lane's EXACT compare — `expect.stdout`
/// already carries its exact trailing newlines; we capture RAW stdout and do
/// NOT inherit `self_host_emit_cc_run`'s `trim_end`):
///   * `expect.exit == 0` → require process exit 0 AND byte-exact stdout.
///   * a TRAP fixture (`expect.trap` present ⟺ `expect.exit == 101`, D11) →
///     require process exit **101**, the `trap[<code>]` marker on stderr (the
///     trailing ` at file:line:col` and the human detail are IGNORED — never
///     compared, Q1), AND the pre-trap `expect.stdout` as a PREFIX of observed
///     stdout. **This is the POST-T2 contract**: T1 DEFINES it (and the ggdef
///     lane meets it by construction); production meets it in T2 (until then a
///     trap fixture MISMATCHes the C/LLVM/self-host lanes — they still exit 1
///     with no `trap[` line — which is expected and holds each production floor
///     at the 187 exit-0 baseline).
///   * any other nonzero, NON-trap exit (102 IllFormed / 103 FuelExhausted; no
///     production fixture today) → the defensive "nonzero exit + stdout prefix".
fn adjudicate(expect: &Expect, out: &Output) -> Verdict {
    let stdout = String::from_utf8_lossy(&out.stdout);
    if expect.exit == 0 {
        if out.status.success() && stdout == expect.stdout {
            Verdict::Match
        } else {
            Verdict::Mismatch(format!(
                "exit {} vs 0 · stdout {:?} vs {:?}",
                exit_str(out),
                stdout,
                expect.stdout
            ))
        }
    } else if let Some(code) = expect.trap.as_deref() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        let marker = format!("trap[{code}]");
        if out.status.code() == Some(101)
            && stderr.contains(&marker)
            && stdout.starts_with(expect.stdout.as_str())
        {
            Verdict::Match
        } else {
            Verdict::Mismatch(format!(
                "trap: want exit 101 + stderr⊇{marker:?} + stdout prefix {:?} · got exit {} · \
                 stdout {:?} · stderr {:?}",
                expect.stdout,
                exit_str(out),
                stdout,
                stderr,
            ))
        }
    } else if !out.status.success() && stdout.starts_with(expect.stdout.as_str()) {
        Verdict::Match
    } else {
        Verdict::Mismatch(format!(
            "exit {} (want nonzero) · stdout {:?} vs prefix {:?}",
            exit_str(out),
            stdout,
            expect.stdout
        ))
    }
}

/// Adjudicate one lane's raw step result against the committed expectation,
/// dispatching on whether the fixture is a static REJECTION.
///
/// A reject fixture (`expect.reject` is `Some`) inverts the build contract: a
/// correct production impl REFUSES it at check/build time (exit nonzero, no
/// runnable binary) AND its build stderr carries the `error[E_<code>]` marker
/// whose code equals `expect.reject`. This mirrors the `trap:` arm exactly — the
/// CODE is the conformance axis; the prose message and the codespan rendering
/// stay impl-defined (pin 3). Verdicts:
///   * build failed + code == `expect.reject`  → Match (the reject is AFFIRMED)
///   * build failed + wrong/absent code        → Mismatch (on the code axis)
///   * build SUCCEEDED (a binary ran)          → Mismatch (should have rejected)
///
/// A NON-reject fixture keeps the prior contract byte-for-byte: `Ok(out)` →
/// `adjudicate`; a step `Err(v)` (BuildFail / run-timeout Mismatch) passes
/// through unchanged, so a plain build failure still counts as BuildFail.
fn adjudicate_lane(expect: &Expect, raw: Result<Output, Verdict>) -> Verdict {
    let Some(want) = expect.reject.as_deref() else {
        return match raw {
            Ok(out) => adjudicate(expect, &out),
            Err(v) => v,
        };
    };
    match raw {
        // The EXPECTED path: a check/build-stage rejection. Its marker line is in
        // the BuildFail detail (`first_error_line` keeps the first `error`-bearing
        // line, and `error[E_..]:` sits at its head, inside the 200-char window).
        Err(Verdict::BuildFail(stderr)) => match extract_reject_code(&stderr) {
            Some(got) if got == want => Verdict::Match,
            Some(got) => Verdict::Mismatch(format!(
                "reject: want error[{want}] · got error[{got}] · build stderr {stderr:?}"
            )),
            None => Verdict::Mismatch(format!(
                "reject: want error[{want}] · build failed with NO error[E_..] marker · \
                 stderr {stderr:?}"
            )),
        },
        // Built + ran when it should have been rejected at check time.
        Ok(out) => Verdict::Mismatch(format!(
            "reject: want build rejection error[{want}] · but the build SUCCEEDED and the \
             binary ran (exit {}, stdout {:?})",
            exit_str(&out),
            String::from_utf8_lossy(&out.stdout),
        )),
        // A non-BuildFail step error (e.g. an emit/run timeout) — surface as-is.
        Err(v) => v,
    }
}

/// Human-readable exit for a diagnostic line (`signal` when killed by a signal,
/// which surfaces as `code() == None` on Unix).
fn exit_str(out: &Output) -> String {
    match out.status.code() {
        Some(c) => c.to_string(),
        None => "signal".to_string(),
    }
}

/// First meaningful stderr line of a failed build/run, truncated for the table.
fn first_error_line(out: &Output) -> String {
    let stderr = String::from_utf8_lossy(&out.stderr);
    stderr
        .lines()
        .find(|l| l.contains("error") || l.contains("undefined"))
        .or_else(|| stderr.lines().find(|l| !l.trim().is_empty()))
        .unwrap_or("(no stderr)")
        .chars()
        .take(200)
        .collect()
}

/// Extract the `E_<code>` from the FIRST `error[E_..]` marker in a build/check
/// stderr — the production diagnostic family (`error[E_UseAfterMove]: …`), the
/// static-rejection analogue of the `trap[T_..]` marker `adjudicate` already
/// extracts. Returns the bare registry code (`E_UseAfterMove`), or `None` when no
/// such marker is present. Only the CODE is read; the message + the codespan
/// rendering (`┌─ file:line:col`) are impl-defined and NEVER compared (pin 3 —
/// the exact analogue of ignoring the trap's ` at file:line:col` detail). The
/// `E_` guard ensures a stray `error[...]` bracket is not mistaken for a code.
fn extract_reject_code(stderr: &str) -> Option<String> {
    let after = stderr.find("error[").map(|i| i + "error[".len())?;
    let rest = &stderr[after..];
    let end = rest.find(']')?;
    let code = &rest[..end];
    code.starts_with("E_").then(|| code.to_string())
}

// ─────────────────────────── per-lane build+run steps ───────────────────

/// C / LLVM lane step: copy the fixture into the lane's own scratch (so `gg
/// build`'s next-to-source binary does NOT pollute `spectests/run/` and the two
/// backends never collide), build with the pinned backend, then run the binary.
/// A gg/cc stage failure is BUILD-FAIL; a run timeout is a MISMATCH (the binary
/// existed, it just did not terminate).
fn gg_build_step(scratch: &Path, fixture: &Path, backend: Option<&str>) -> Result<Output, Verdict> {
    let stem = fixture.file_stem().unwrap().to_string_lossy().to_string();
    let prog = scratch.join(format!("{stem}.gg"));
    fs::copy(fixture, &prog).expect("copy fixture into scratch");

    let mut cmd = Command::new(gg_binary());
    cmd.arg("build");
    if let Some(b) = backend {
        cmd.arg(format!("--backend={b}"));
    }
    cmd.arg(&prog);
    match run_cmd(&mut cmd, build_timeout()) {
        Err(TimedOut) => {
            return Err(Verdict::BuildFail(format!(
                "gg build timed out after {}s",
                build_timeout().as_secs()
            )));
        }
        Ok(o) if !o.status.success() => return Err(Verdict::BuildFail(first_error_line(&o))),
        Ok(_) => {}
    }

    let bin = scratch.join(&stem);
    match run_cmd(&mut Command::new(&bin), run_timeout()) {
        Err(TimedOut) => {
            Err(Verdict::Mismatch(format!("run timed out after {}s", run_timeout().as_secs())))
        }
        Ok(o) => Ok(o),
    }
}

fn c_step(scratch: &Path, fixture: &Path) -> Result<Output, Verdict> {
    gg_build_step(scratch, fixture, None)
}

fn llvm_step(scratch: &Path, fixture: &Path) -> Result<Output, Verdict> {
    gg_build_step(scratch, fixture, Some("llvm"))
}

/// Self-host lane step: driver `--emit-c` → `cc` → run. Mirrors
/// `self_host_emit_cc_run` (integration.rs:19737) EXCEPT it captures RAW stdout
/// (no `trim_end`) so the exact-newline compare in `adjudicate` is honest. A
/// driver or cc failure is BUILD-FAIL; a run timeout is a MISMATCH.
fn selfhost_step(scratch: &Path, fixture: &Path) -> Result<Output, Verdict> {
    let (driver_exe, lib_dir, runtime_dir) = driver_paths();
    let stem = fixture.file_stem().unwrap().to_string_lossy().to_string();
    let c_path = scratch.join(format!("{stem}.c"));
    let bin = scratch.join(&stem);

    // Driver reads the ORIGINAL fixture and emits C to stdout (no next-to-source
    // binary), so no copy is needed.
    let emit = run_cmd(
        Command::new(driver_exe)
            .arg(fixture)
            .arg(lib_dir)
            .arg("--emit-c")
            .arg(format!("--runtime-dir={}", runtime_dir.display())),
        build_timeout(),
    );
    let emit = match emit {
        Err(TimedOut) => return Err(Verdict::BuildFail("self-host driver emit timed out".into())),
        Ok(o) if !o.status.success() => {
            return Err(Verdict::BuildFail(format!("driver: {}", first_error_line(&o))));
        }
        Ok(o) => o,
    };
    fs::write(&c_path, &emit.stdout).expect("write self-host C");

    let cc = run_cmd(
        Command::new("cc")
            .arg("-O0")
            .arg("-w")
            .arg("-o")
            .arg(&bin)
            .arg(&c_path)
            .arg("-lm")
            .arg("-lpthread"),
        build_timeout(),
    );
    match cc {
        Err(TimedOut) => return Err(Verdict::BuildFail("cc (self-host) timed out".into())),
        Ok(o) if !o.status.success() => {
            return Err(Verdict::BuildFail(format!("cc: {}", first_error_line(&o))));
        }
        Ok(_) => {}
    }

    match run_cmd(&mut Command::new(&bin), run_timeout()) {
        Err(TimedOut) => Err(Verdict::Mismatch(format!(
            "self-host run timed out after {}s",
            run_timeout().as_secs()
        ))),
        Ok(o) => Ok(o),
    }
}

// ─────────────────────────── the lane driver ────────────────────────────

/// Local MATCH-count floor gate. Mirrors `parity_floor_active`
/// (tests/integration.rs:99) MINUS the GG_BACKEND carve-out — each lane here
/// PINS its own backend on the build command, so a set GG_BACKEND does not
/// change what the lane exercises. Every carve-out prints a non-silent notice.
/// Returns true when the floor assert should fire.
fn floor_active(lane: &str) -> bool {
    if std::env::var("GG_PARITY_FLOOR_OFF").as_deref() == Ok("1") {
        eprintln!(
            "WARNING [{lane}]: MATCH-count floor DISABLED via GG_PARITY_FLOOR_OFF=1 — \
             conformance regressions will NOT fail this run. Unset it for gate-honest results."
        );
        return false;
    }
    if !cfg!(target_os = "linux") {
        eprintln!(
            "NOTE [{lane}]: MATCH-count floor skipped (non-linux host — the self-host cc step \
             CC-FAILs en masse under Apple clang; see the TODO.md macOS shim note). The floor is \
             enforced on linux (CI and linux dev boxes)."
        );
        return false;
    }
    true
}

/// Enumerate `spectests/run/*.gg`, adjudicate each through `step`, print the
/// always-on table, then enforce the fixtures-count guard, the frontmatter
/// fatal, and the monotone MATCH-count floor.
fn run_lane(
    lane: &str,
    floor: usize,
    step: impl Fn(&Path, &Path) -> Result<Output, Verdict>,
) {
    let run_dir = ws_root().join("spectests/run");
    let mut fixtures: Vec<PathBuf> = fs::read_dir(&run_dir)
        .expect("read spectests/run")
        .filter_map(|e| {
            let p = e.unwrap().path();
            (p.extension().and_then(|x| x.to_str()) == Some("gg")).then_some(p)
        })
        .collect();
    fixtures.sort();

    // Guard the glob. ⚠ THIS IS AN EXACT PIN, NOT A FLOOR, AND THE `>=` IT
    // REPLACED WAS BLIND IN THE DIRECTION THE CORPUS ACTUALLY MOVES.
    // A `>=` guard catches only a SHRUNKEN corpus; adding a fixture makes its
    // left side bigger, so the assert gets MORE true. R48's close measured that
    // exactly: `reject_partial_move_field.gg` and `reject_partial_move_self.gg`
    // landed with no ratchet and all four constants stayed green for two
    // fixtures. Corpus SIZE is deterministic (a file count, not a timing-
    // sensitive measurement), so `==` is safe here where it would not be for
    // the parity MATCH counts — and it catches growth AND shrink.
    assert_eq!(
        fixtures.len(),
        MIN_FIXTURES,
        "spectests/run has {} `.gg` seeds but MIN_FIXTURES is {MIN_FIXTURES}.\n\n\
         ADDING OR REMOVING A SPECTEST RATCHETS ALL FOUR CONSTANTS TOGETHER \
         (tests/spec_conformance.rs):\n  \
         MIN_FIXTURES, C_MATCH_FLOOR, LLVM_MATCH_FLOOR, SELFHOST_MATCH_FLOOR\n\n\
         Regenerate the count with `ls spectests/run/*.gg | wc -l`, then run\n  \
         cargo test --test spec_conformance -- --test-threads=1 --nocapture\n\
         and seed each lane floor from the `total=… MATCH=…` line it prints.\n\
         This duty attaches to the FIXTURE, not to any track pipeline.",
        fixtures.len()
    );

    let scratch =
        std::env::temp_dir().join(format!("gg_spec_conf_{lane}_{}", std::process::id()));
    fs::create_dir_all(&scratch).expect("create scratch dir");

    let mut table = format!("\n══ {lane} ══ (production impl vs committed expect:)\n\n");
    let mut matched = 0usize;
    let mut mismatched = 0usize;
    let mut build_failed = 0usize;
    let mut details: Vec<String> = Vec::new();
    let mut frontmatter_errs: Vec<String> = Vec::new();

    for path in &fixtures {
        let name = path.file_name().unwrap().to_string_lossy().to_string();
        let src = fs::read_to_string(path).unwrap();

        // A malformed committed seed corrupts every verdict — collect and fail
        // hard below, independent of the floor escape hatch (ggdef-lane policy).
        let fm = match parse_frontmatter(&src) {
            Ok(f) => f,
            Err(e) => {
                table.push_str(&format!("  FRONTMATTER-ERR  {name}  ({e})\n"));
                frontmatter_errs.push(format!("  {name}: {e}"));
                continue;
            }
        };

        let verdict = adjudicate_lane(&fm.expect, step(&scratch, path));
        match verdict {
            Verdict::Match => {
                matched += 1;
                table.push_str(&format!("  MATCH        {name}\n"));
            }
            Verdict::Mismatch(d) => {
                mismatched += 1;
                table.push_str(&format!("  MISMATCH     {name}  ({d})\n"));
                details.push(format!("  MISMATCH   {name}: {d}"));
            }
            Verdict::BuildFail(d) => {
                build_failed += 1;
                table.push_str(&format!("  BUILD-FAIL   {name}  ({d})\n"));
                details.push(format!("  BUILD-FAIL {name}: {d}"));
            }
        }
    }

    table.push_str(&format!(
        "\n  total={} · MATCH={matched} · MISMATCH={mismatched} · BUILD-FAIL={build_failed}\n",
        fixtures.len()
    ));
    eprintln!("{table}");

    fs::remove_dir_all(&scratch).ok();

    assert!(
        frontmatter_errs.is_empty(),
        "{lane}: {} committed seed(s) have malformed frontmatter — the conformance reader \
         cannot adjudicate them:\n{}",
        frontmatter_errs.len(),
        frontmatter_errs.join("\n")
    );

    if floor_active(lane) {
        assert!(
            matched >= floor,
            "{lane} MATCH-count floor regression: MATCH {matched} < floor {floor}.\n\n\
             A change lowered the count of fixtures this production impl reproduces from the \
             committed `expect:`. The table above names them (MISMATCH = built+ran but disagreed; \
             BUILD-FAIL = never produced a runnable binary):\n{}\n\n\
             Fix the regression rather than lowering the floor. If MATCH went UP (a known defect \
             was fixed, or a new seed landed), raise the floor const in tests/spec_conformance.rs \
             in the SAME commit to lock in the gain.\n\
             Regenerate: cargo test --test spec_conformance -- --test-threads=1 --nocapture\n\
             Emergency escape hatch (loud, temporary): GG_PARITY_FLOOR_OFF=1.",
            details.join("\n")
        );
    }
}

// ─────────────────────────── the three lanes ────────────────────────────

/// C backend: `gg build` (default) → run, vs the committed `expect:`.
#[test]
fn spec_conformance_c() {
    run_lane("spec_conformance_c", C_MATCH_FLOOR, c_step);
}

/// LLVM backend: `gg build --backend=llvm` → run, vs the committed `expect:`.
#[test]
fn spec_conformance_llvm() {
    run_lane("spec_conformance_llvm", LLVM_MATCH_FLOOR, llvm_step);
}

/// Self-host: driver `--emit-c` → `cc` → run, vs the committed `expect:`. Pays
/// one driver build (OnceLock) for the whole lane.
#[test]
fn spec_conformance_selfhost() {
    run_lane("spec_conformance_selfhost", SELFHOST_MATCH_FLOOR, selfhost_step);
}
