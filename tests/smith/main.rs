//! # gorget-smith — differential fuzzer for the Gorget compiler
//!
//! Generates seeded, well-typed, deterministic Gorget programs (see
//! `generator.rs`) and cross-checks every backend the project ships against
//! each other, per seed, in cost order:
//!
//!   1. `gg check`               — front-end accepts the program
//!   1b. ggdef (the definition)  — evaluate in-process (RFC §4): an `IllFormed`
//!                                 outcome where `gg check` accepted is an
//!                                 immediate SPEC-DIVERGE; else the value/trap
//!                                 outcome is held as an oracle for step 3
//!   2. `gg build` (C backend)   — reference build must succeed
//!   3. run the C binary         — must exit 0; stdout becomes the oracle, and
//!                                 the held ggdef value/trap oracle is checked
//!                                 against it here (c_out now exists)
//!   4. self-host lane           — `driver --emit-c` → `cc` → run, diff vs C
//!   5. LLVM lane                — `gg build --backend=llvm` → run, diff vs C
//!
//! The per-seed flow is SHORT-CIRCUIT: the first failing oracle assigns the
//! seed's single classification and all later lanes are skipped.
//!
//! ## Taxonomy (mirrors `RuntimeParityOutcome`, tests/integration.rs)
//!
//! | class            | meaning                                              |
//! |------------------|------------------------------------------------------|
//! | MATCH            | all enabled lanes agree (stdout + exit)              |
//! | GEN-INVALID      | `gg check` rejected — generator bug OR compiler      |
//! |                  | false-reject; triage, not automatically ours         |
//! | BUILD-FAIL(C)    | check-accepted but `gg build` (default C backend)    |
//! |                  | failed at ANY stage, gg or cc — a COMPILER bug (the  |
//! |                  | front and back ends disagree about validity)         |
//! | CRASH            | the C binary exited nonzero / signalled              |
//! | DRIVER-FAIL      | the self-host driver rejected the program            |
//! | CC-FAIL          | cc rejected the SELF-HOST-emitted C — a self-host    |
//! |                  | emit defect. Distinct from BUILD-FAIL(C): that one   |
//! |                  | is the Rust gg pipeline, this one is the self-host's |
//! | DIVERGE(C,SH)    | self-host binary's stdout/exit differs from C        |
//! | BUILD-FAIL(llvm) | C built but `--backend=llvm` didn't                  |
//! | DIVERGE(C,llvm)  | LLVM binary's stdout/exit differs from C             |
//! | TIMEOUT          | some lane hit its deadline                           |
//! | SPEC-DIVERGE     | ggdef (the definition) and an impl disagree: a       |
//! |                  | check-accepted program ggdef calls IllFormed, or a   |
//! |                  | value/trap outcome differing from the C oracle. A    |
//! |                  | both-compiler finding (invariant #8); tri-state      |
//! |                  | triage: impl-bug / spec-bug / spec-silent            |
//! | GGDEF-SKIP       | ggdef could not adjudicate (out of GGC surface or    |
//! |                  | non-terminating under fuel); the cross-impl lanes    |
//! |                  | still ran and agreed. Benign — recorded so ggdef     |
//! |                  | coverage gaps stay visible                           |
//!
//! ## How to run
//!
//! ```text
//! # always-on (plain `cargo test --test smith`): generator determinism only
//! cargo test --test smith
//!
//! # batch run (always-pass DIAGNOSTIC — prints a verdict table, never
//! # asserts counts). Seed range is INCLUSIVE on both ends: 1..200 = 200 seeds.
//! GG_SMITH_SEEDS=1..200 cargo test --test smith -- --nocapture
//!
//! # skip the LLVM lane (ON by default)
//! GG_SMITH_SEEDS=1..200 GG_SMITH_SKIP_LLVM=1 cargo test --test smith -- --nocapture
//!
//! # minimize one seed's program while its classification is preserved
//! GG_SMITH_MINIMIZE=36 cargo test --test smith -- --nocapture
//! ```
//!
//! Honors `GG_BUILD_TIMEOUT_SECS` / `GG_TEST_TIMEOUT_SECS` (same semantics as
//! tests/integration.rs). **`GG_BACKEND` is deliberately IGNORED**: smith
//! selects backends per-lane, explicitly — a stray `GG_BACKEND=llvm` must not
//! silently flip the C lane (and the driver build) to LLVM and collapse the
//! differential.
//!
//! ## Tier roadmap
//!
//! - tier 0 (default): scalars/strings/vectors, aliases, views, moves,
//!   helper fns, control flow — the CoW/ownership core (the DIFFERENTIAL shape).
//! - tier 1 = `TIER_THROWS` (this round, T3b): the throws-position REJECTION
//!   tier — one unhandled `throws` call per program, in a fuzzed position, that
//!   production must REJECT with `E_UnhandledThrows`. A DIFFERENT ORACLE than
//!   the differential (see the "throws-position rejection tier" section below);
//!   CHECK-ONLY, no ggdef / build / self-host / LLVM lane.
//! - future: widen the differential grammar — the mutation-route class table of
//!   devbook/11 (§ "materialization points"); traits/equip, closures, generics.
//!
//! Tier is selected with `GG_SMITH_TIER` (default 0).
//!
//! ## throws-position rejection tier (T3b, `GG_SMITH_TIER=1`)
//!
//! Tier 1 inverts the differential: each seed is a program well-formed EXCEPT
//! for ONE unhandled `throws` call (`generator::program_throws_positions`), and
//! `classify` runs an INVERTED oracle (`classify_throws`) — production is
//! REQUIRED to reject with the EXACT diagnostic code `error[E_UnhandledThrows]`.
//! Verdicts: `UNHANDLED-THROWS-REJECTED` is the benign PASS (the exact code,
//! never a loose "mentions throws" match); `UNHANDLED-THROWS-SLIP` (check
//! accepted an unhandled throws) is a real T3a enforcement hole;
//! `THROWS-DESUGAR-LEAK` (`found `Result[`) is a D23 diagnostic regression; any
//! other rejection is `GEN-INVALID` (a generator bug or a mis-coded rejection).
//! All three non-benign verdicts gate the green report.
//!
//! ## ggdef verdict lane (RFC §4)
//!
//! The definitional interpreter `ggdef` runs IN-PROCESS as a fifth oracle (see
//! the flow above). For tier 0 this is provably safe: tier-0 output is shallow
//! (a fixed helper set), and over 200 seeds ggdef hits 0 `FuelExhausted` at
//! `GGDEF_FUEL` = 2,000,000 — no seed drives ggdef deep enough to overflow its
//! native stack. That safety is a property of tier 0, NOT of ggdef: ggdef
//! bounds STEP count, not eval/elaboration recursion DEPTH, so a deep seed
//! would SIGABRT the whole (in-process) smith run uncatchably. Widening the
//! generator past tier 0 is therefore GATED on the ggdef native-recursion-guard
//! fix (filed HIGH in TODO.md) — do NOT enable a deeper tier before it lands.
//! That gate is on TIER WIDENING; running ggdef in-process at tier 0 is safe
//! now, so this lane adds no depth-bound of its own.
//!
//! ## Round-1 findings (both FIXED and promoted into `tests/fixtures/`)
//!
//! Round 1 surfaced two check-accepted programs that both production backends
//! miscompiled while the self-host lowerer was reference-correct. Both were
//! filed TODO.md HIGH, both were fixed, and both fixtures were promoted out of
//! `known_gaps/` and un-ignored (they now MATCH on both backends):
//!
//! - alias bind in a never-taken branch → a later source mutation SIGSEGVed in
//!   `gorget_array_clone` on BOTH backends. Fixed at the write site
//!   (`restore_locals` resets a branch-local `Alias(_)` to unowned at scope
//!   exit) — test `cow_dead_branch_alias_bind` (`tests/fixtures/cow_dead_branch_alias_bind.gg`).
//! - `String !p` move-param + concat in the callee: check-accepted, the C
//!   backend emitted `(void*)a + (void*)b` (cc rejected), the LLVM backend an
//!   invalid `add ptr` (llc rejected). Fixed at the binop consume site (the
//!   `!`-move `MutPtr(String)` operand now derefs in `cow_deref_if_ptr`,
//!   `src/ir/lowering/exprs/operators.rs`) — test `move_param_concat`
//!   (`tests/fixtures/move_param_concat.gg`).
//!
//! Why this tool exists: ASan is NOT the safety net for the CoW/ownership
//! bug class (devbook/11) — the fixture corpus only covers shapes someone
//! thought to write. Smith walks the shape space mechanically.

mod generator;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

// ─────────────────────────── infrastructure ────────────────────────────
// tests/smith is a SEPARATE crate from tests/integration.rs, so the helpers
// there (gg_binary, run_with_timeout, self_host_emit_cc_run, …) are not
// importable. The small subset needed is re-derived here from the same
// sources (integration.rs:20-50 timeouts, :134-136 binary, :157-219 runner,
// :19388-19476 self-host lane).

/// Path to the pre-built `gg` binary (Cargo guarantees it is up to date
/// before this test process starts).
fn gg_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_gg"))
}

/// `GG_BUILD_TIMEOUT_SECS`, else 120s scaled by host load pressure
/// (`loadavg(1m) / cpus`, floor 1.0) — same policy as integration.rs.
fn build_timeout() -> Duration {
    if let Some(secs) =
        std::env::var("GG_BUILD_TIMEOUT_SECS").ok().and_then(|s| s.parse::<u64>().ok())
    {
        return Duration::from_secs(secs);
    }
    let load = std::fs::read_to_string("/proc/loadavg")
        .ok()
        .and_then(|s| s.split_whitespace().next()?.parse::<f64>().ok())
        .unwrap_or(0.0);
    let cpus = std::thread::available_parallelism().map(|n| n.get() as f64).unwrap_or(1.0);
    let ratio = (load / cpus).max(1.0);
    Duration::from_secs((120.0 * ratio).ceil() as u64)
}

/// `GG_TEST_TIMEOUT_SECS`, else 30s — deadline for generated test binaries.
fn run_timeout() -> Duration {
    Duration::from_secs(
        std::env::var("GG_TEST_TIMEOUT_SECS").ok().and_then(|s| s.parse().ok()).unwrap_or(30),
    )
}

/// A child overran its deadline (killed; classified TIMEOUT by the caller).
struct TimedOut;

/// Run a command with nulled stdin and a deadline. Unlike integration.rs's
/// `run_with_timeout` this does NOT panic on timeout — TIMEOUT is a smith
/// classification, not a harness failure. stdout/stderr are drained on
/// background threads to avoid the >64KB pipe-buffer deadlock.
fn run_cmd(cmd: &mut Command, timeout: Duration) -> Result<Output, TimedOut> {
    let mut child = cmd
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| panic!("failed to spawn {cmd:?}: {e}"));

    let stdout_handle = child.stdout.take().unwrap();
    let stderr_handle = child.stderr.take().unwrap();
    let stdout_thread = std::thread::spawn(move || {
        use std::io::Read;
        let mut buf = Vec::new();
        let mut reader = stdout_handle;
        reader.read_to_end(&mut buf).ok();
        buf
    });
    let stderr_thread = std::thread::spawn(move || {
        use std::io::Read;
        let mut buf = Vec::new();
        let mut reader = stderr_handle;
        reader.read_to_end(&mut buf).ok();
        buf
    });

    let deadline = Instant::now() + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if Instant::now() >= deadline {
                    child.kill().ok();
                    child.wait().ok();
                    // Join the drain threads so their pipes close cleanly.
                    stdout_thread.join().ok();
                    stderr_thread.join().ok();
                    return Err(TimedOut);
                }
                std::thread::sleep(Duration::from_millis(20));
            }
            Err(e) => panic!("failed to wait on child: {e}"),
        }
    };
    let stdout = stdout_thread.join().unwrap_or_default();
    let stderr = stderr_thread.join().unwrap_or_default();
    Ok(Output { status, stdout, stderr })
}

/// Build the self-host driver ONCE per smith process (≈57s — see the
/// `build_gg_dir_cached` doc comment in integration.rs). The artifacts land
/// at the shared `tests/fixtures/self_host_lowerer/driver{,.c}` paths (that
/// is how the existing self-host lane works — deliberate carve-out; they are
/// gitignored and must not be deleted here). Returns
/// `(driver_exe, lib_dir, runtime_dir)`; `runtime_dir` is ABSOLUTE (derived
/// from CARGO_MANIFEST_DIR — a relative `--runtime-dir` only works by cwd
/// luck). The driver build never reads GG_BACKEND: the differential's
/// reference lane is pinned to the default C backend.
fn driver_paths() -> &'static (PathBuf, PathBuf, PathBuf) {
    static DRIVER: OnceLock<(PathBuf, PathBuf, PathBuf)> = OnceLock::new();
    DRIVER.get_or_init(|| {
        let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
        let main_path = manifest.join("tests/fixtures/self_host_lowerer/driver.gg");
        assert!(main_path.exists(), "driver source not found: {}", main_path.display());
        eprintln!("[smith] building self-host driver (once per process, ~1 min)…");
        let t0 = Instant::now();
        let out = run_cmd(
            Command::new(gg_binary()).arg("build").arg(&main_path),
            build_timeout(),
        )
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
        eprintln!("[smith] driver built in {:.1}s", t0.elapsed().as_secs_f64());
        (
            manifest.join("tests/fixtures/self_host_lowerer/driver"),
            manifest.join("lib"),
            manifest.join("src/backend/c/runtime"),
        )
    })
}

// ───────────────────────────── classification ──────────────────────────

/// One seed's verdict. Mirrors `RuntimeParityOutcome`
/// (tests/integration.rs:19304-19317), extended with the differential's
/// per-lane build classes. See the module docs for the taxonomy table.
enum Verdict {
    Match,
    GenInvalid { detail: String },
    BuildFailC { detail: String },
    Crash { detail: String },
    Timeout { lane: &'static str },
    DriverFail { detail: String },
    CcFail { detail: String },
    DivergeSh { detail: String },
    BuildFailLlvm { detail: String },
    DivergeLlvm { detail: String },
    /// ggdef (the definition, RFC §4) and an implementation disagree: ggdef
    /// flags a program `gg check` ACCEPTED as `IllFormed`, or ggdef's value/
    /// trap outcome differs from the C oracle. A both-compiler finding
    /// (invariant #8) — triage impl-bug vs spec-bug vs spec-silent, never
    /// silently accept.
    SpecDiverge { detail: String },
    /// ggdef could not adjudicate this seed (out of the GGC surface, or
    /// non-terminating under fuel); the cross-impl differential still ran and
    /// agreed. Benign, recorded distinctly so ggdef-coverage gaps stay visible.
    GgdefSkip { detail: String },
    // ── throws-position rejection tier (T3b, `TIER_THROWS`) verdicts ──
    /// The BENIGN PASS: production correctly REJECTED the one unhandled `throws`
    /// call with the EXACT code `error[E_UnhandledThrows]` (and no `found
    /// `Result[` desugar leak). A rejection without that code routes to
    /// `GenInvalid`, never here (`classify_throws_routing` pins this).
    /// The whole point of the tier — treated like `Match` for cleanup/reporting.
    UnhandledThrowsRejected,
    /// NON-BENIGN: `gg check` ACCEPTED a program with an unhandled `throws`
    /// call — a real T3a enforcement hole (silent swallow / miscompile). Must
    /// surface distinctly from a generator bug; blocks the green gate.
    UnhandledThrowsSlip { detail: String },
    /// NON-BENIGN: `gg check` rejected but the diagnostic LEAKED the `Result[T,
    /// E]` desugar (`found `Result[`) instead of the clean `E_UnhandledThrows`
    /// — the pre-D23 desugar surfacing as the found type. A regression; blocks.
    ThrowsDesugarLeak { detail: String },
}

impl Verdict {
    fn label(&self) -> &'static str {
        match self {
            Verdict::Match => "MATCH",
            Verdict::GenInvalid { .. } => "GEN-INVALID",
            Verdict::BuildFailC { .. } => "BUILD-FAIL(C)",
            Verdict::Crash { .. } => "CRASH",
            Verdict::Timeout { .. } => "TIMEOUT",
            Verdict::DriverFail { .. } => "DRIVER-FAIL",
            Verdict::CcFail { .. } => "CC-FAIL",
            Verdict::DivergeSh { .. } => "DIVERGE(C,SH)",
            Verdict::BuildFailLlvm { .. } => "BUILD-FAIL(llvm)",
            Verdict::DivergeLlvm { .. } => "DIVERGE(C,llvm)",
            Verdict::SpecDiverge { .. } => "SPEC-DIVERGE",
            Verdict::GgdefSkip { .. } => "GGDEF-SKIP",
            Verdict::UnhandledThrowsRejected => "UNHANDLED-THROWS-REJECTED",
            Verdict::UnhandledThrowsSlip { .. } => "UNHANDLED-THROWS-SLIP",
            Verdict::ThrowsDesugarLeak { .. } => "THROWS-DESUGAR-LEAK",
        }
    }

    /// A benign verdict: the seed exhibits no cross-impl or spec divergence.
    /// `GgdefSkip` is benign (the differential agreed; ggdef merely abstained),
    /// so it is treated like `Match` for scratch-dir cleanup and progressive
    /// reporting, and is NOT counted as suspicious.
    fn is_benign(&self) -> bool {
        matches!(
            self,
            Verdict::Match | Verdict::GgdefSkip { .. } | Verdict::UnhandledThrowsRejected
        )
    }

    /// First error/diff line for the report (empty for MATCH).
    fn detail(&self) -> String {
        match self {
            Verdict::Match | Verdict::UnhandledThrowsRejected => String::new(),
            Verdict::GenInvalid { detail }
            | Verdict::BuildFailC { detail }
            | Verdict::Crash { detail }
            | Verdict::DriverFail { detail }
            | Verdict::CcFail { detail }
            | Verdict::DivergeSh { detail }
            | Verdict::BuildFailLlvm { detail }
            | Verdict::DivergeLlvm { detail }
            | Verdict::SpecDiverge { detail }
            | Verdict::GgdefSkip { detail }
            | Verdict::UnhandledThrowsSlip { detail }
            | Verdict::ThrowsDesugarLeak { detail } => detail.clone(),
            Verdict::Timeout { lane } => format!("lane: {lane}"),
        }
    }
}

#[derive(Default, Clone, Copy)]
struct Timings {
    generate: Duration,
    check: Duration,
    ggdef: Duration,
    cbuild: Duration,
    crun: Duration,
    driver: Duration,
    cc: Duration,
    shrun: Duration,
    lbuild: Duration,
    lrun: Duration,
}

impl Timings {
    fn add(&mut self, o: &Timings) {
        self.generate += o.generate;
        self.check += o.check;
        self.ggdef += o.ggdef;
        self.cbuild += o.cbuild;
        self.crun += o.crun;
        self.driver += o.driver;
        self.cc += o.cc;
        self.shrun += o.shrun;
        self.lbuild += o.lbuild;
        self.lrun += o.lrun;
    }

    fn total(&self) -> Duration {
        self.generate
            + self.check
            + self.ggdef
            + self.cbuild
            + self.crun
            + self.driver
            + self.cc
            + self.shrun
            + self.lbuild
            + self.lrun
    }
}

struct Config {
    tier: u32,
    skip_llvm: bool,
}

impl Config {
    fn from_env() -> Config {
        // NOTE: GG_BACKEND is deliberately NOT read anywhere in this crate —
        // smith pins its backends per-lane (see module docs).
        Config {
            tier: std::env::var("GG_SMITH_TIER")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(0),
            skip_llvm: std::env::var("GG_SMITH_SKIP_LLVM").as_deref() == Ok("1"),
        }
    }
}

/// Strip ANSI escape sequences (`ESC [ … <alpha>`), so diagnostic lines from
/// colored `gg` output stay grep-able in the teed log.
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\u{1b}' {
            for t in chars.by_ref() {
                if t.is_ascii_alphabetic() {
                    break;
                }
            }
            continue;
        }
        out.push(c);
    }
    out
}

fn truncate_chars(s: &str, n: usize) -> String {
    if s.chars().count() <= n { s.to_string() } else { s.chars().take(n).collect() }
}

/// Up to 3 `error`/`undefined` lines from a build's combined output (else
/// its first non-empty line), joined and truncated — the "first error line"
/// of the report.
fn error_lines(o: &Output) -> String {
    let text = format!(
        "{}\n{}",
        String::from_utf8_lossy(&o.stderr),
        String::from_utf8_lossy(&o.stdout)
    );
    let text = strip_ansi(&text);
    let hits: Vec<&str> = text
        .lines()
        .filter(|l| l.contains("error") || l.contains("undefined"))
        .take(3)
        .collect();
    let joined = if hits.is_empty() {
        text.lines().find(|l| !l.trim().is_empty()).unwrap_or("(no output)").to_string()
    } else {
        hits.join(" | ")
    };
    truncate_chars(joined.trim(), 240)
}

/// `exit=…/signal=…` + first stderr line, for CRASH-class details.
fn exit_detail(o: &Output) -> String {
    #[cfg(unix)]
    let sig = {
        use std::os::unix::process::ExitStatusExt;
        o.status.signal()
    };
    #[cfg(not(unix))]
    let sig: Option<i32> = None;
    let first = String::from_utf8_lossy(&o.stderr);
    let first = strip_ansi(first.lines().find(|l| !l.trim().is_empty()).unwrap_or(""));
    truncate_chars(
        format!("exit={:?} signal={:?} {}", o.status.code(), sig, first).trim(),
        240,
    )
}

/// First differing stdout line between two lanes, or a line-count/exit
/// summary when one output is a prefix of the other.
fn first_diff(a_name: &str, a: &str, b_name: &str, b: &str, a_exit: i32, b_exit: String) -> String {
    let d = a
        .lines()
        .zip(b.lines())
        .enumerate()
        .find(|(_, (x, y))| x != y)
        .map(|(i, (x, y))| format!("L{i}: {a_name}={x:?} {b_name}={y:?}"))
        .unwrap_or_else(|| {
            format!(
                "line-count {a_name}={} {b_name}={} exit {a_name}={a_exit} {b_name}={b_exit}",
                a.lines().count(),
                b.lines().count(),
            )
        });
    truncate_chars(&d, 240)
}

// ───────────────────────────── per-seed flow ───────────────────────────

/// Fuel bound for the in-process ggdef verdict-lane evaluation. Tier-0 output
/// is provably shallow: over 200 seeds, 0 `FuelExhausted` at this bound (scout
/// measurement, phase1-infra P1-E). This bounds ggdef's STEP count only — the
/// native-recursion DEPTH is bounded by the tier-0 grammar, and widening past
/// tier 0 is gated on the ggdef native-recursion-guard fix (see the module
/// docs "ggdef verdict lane"), NOT on this constant.
const GGDEF_FUEL: u64 = 2_000_000;

/// The ggdef oracle for one seed, captured right after `gg check` accepts.
/// `IllFormed` never reaches this enum — it is an immediate SPEC-DIVERGE (the
/// definition rejects a program production accepted). `Value`/`Trap` are
/// checked against the C oracle once `c_out` exists (step 3); `Skip` means
/// ggdef abstained and the seed falls back to the cross-impl differential.
enum GgdefOracle {
    /// ggdef ran to a value; its stdout must match the C oracle's.
    Value(String),
    /// ggdef faulted (a defined Trap); the C run must NOT exit 0 cleanly. The
    /// held string is the fault message, for the SPEC-DIVERGE detail line.
    Trap { fault: String },
    /// ggdef could not adjudicate (FuelExhausted / ElabError / ParseError). The
    /// held string is the reason, surfaced in the GGDEF-SKIP detail line.
    Skip { reason: String },
}

/// Classify one program source through the full differential. `work` is a
/// scratch dir owned by the caller (created here; cleaned by the caller).
/// SHORT-CIRCUIT: the first failing oracle is the verdict — cross-lane
/// attribution of a failure happens at triage/minimization, not in the
/// batch (generated programs are well-formed by construction, so a failure
/// on ANY lane is already a filed-quality bug).
fn classify(source: &str, work: &Path, cfg: &Config, t: &mut Timings) -> Verdict {
    let cdir = work.join("c");
    let ldir = work.join("l");
    std::fs::create_dir_all(&cdir).expect("failed to create scratch dir");
    std::fs::create_dir_all(&ldir).expect("failed to create scratch dir");
    let cprog = cdir.join("prog.gg");
    let lprog = ldir.join("prog.gg");
    std::fs::write(&cprog, source).expect("failed to write program");
    std::fs::write(&lprog, source).expect("failed to write program");

    // (1) gg check — a rejection here is GEN-INVALID: a generator bug OR a
    // compiler false-reject. Triage; not automatically a generator bug.
    let t0 = Instant::now();
    let check = run_cmd(Command::new(gg_binary()).arg("check").arg(&cprog), build_timeout());
    t.check += t0.elapsed();

    // Throws-position rejection tier (TIER_THROWS, T3b): the generated program
    // is well-formed EXCEPT for one unhandled `throws` call, so production MUST
    // reject it with `E_UnhandledThrows`. This is an INVERTED oracle and a
    // CHECK-ONLY tier — none of the ggdef / build / self-host / LLVM lanes below
    // run for it (the program never compiles). `classify_throws` returns before
    // the tier-0 `match check` (which would mislabel the expected rejection as
    // GEN-INVALID); the move of `check` is on a diverging branch, so the tier-0
    // path below still owns it.
    if cfg.tier == generator::TIER_THROWS {
        return classify_throws(check);
    }

    match check {
        Err(TimedOut) => return Verdict::Timeout { lane: "gg check" },
        Ok(o) if !o.status.success() => {
            return Verdict::GenInvalid { detail: error_lines(&o) };
        }
        Ok(_) => {}
    }

    // (1b) ggdef verdict lane — the definitional interpreter (RFC §4) as an
    // in-process oracle, evaluated right after `gg check` accepts. `gg check`
    // passing means production considers the program well-formed, so a ggdef
    // `IllFormed` outcome here is a SPEC-DIVERGE reported IMMEDIATELY (the
    // flagship class — e.g. the A29 same-call-aliasing hole where production
    // accepts a program the definition rejects). Value/Trap outcomes are HELD
    // and reconciled against the C oracle after step 3 (c_out). Anything ggdef
    // cannot adjudicate is a GGDEF-SKIP: the seed falls back to the cross-impl
    // differential unchanged — ggdef ADDS an oracle, it never removes a lane.
    // In-process is safe at tier 0 (module docs "ggdef verdict lane").
    let t0 = Instant::now();
    let ggdef_res = ggdef::run_source(source, GGDEF_FUEL);
    t.ggdef += t0.elapsed();
    let ggdef_oracle = match ggdef_res {
        Ok(run) => match run.outcome {
            ggdef::Outcome::IllFormed(msg) => {
                return Verdict::SpecDiverge {
                    detail: truncate_chars(
                        &format!("gg check ACCEPTED but ggdef IllFormed: {msg}"),
                        240,
                    ),
                };
            }
            ggdef::Outcome::Value(_) => GgdefOracle::Value(run.stdout),
            ggdef::Outcome::Trap(f) => GgdefOracle::Trap { fault: f.message().to_string() },
            ggdef::Outcome::FuelExhausted => {
                GgdefOracle::Skip { reason: "ggdef FuelExhausted".to_string() }
            }
        },
        Err(ggdef::FrontendError::Parse(errs)) => GgdefOracle::Skip {
            reason: truncate_chars(&format!("ggdef ParseError ({} error/s)", errs.len()), 240),
        },
        Err(ggdef::FrontendError::Elaborate(e)) => GgdefOracle::Skip {
            reason: truncate_chars(&format!("ggdef ElabError: {}", e.message), 240),
        },
    };

    // (2) C build (default backend — never --backend, never GG_BACKEND).
    // Check-accepted + ANY build failure (gg stage or cc stage) is a
    // compiler bug: BUILD-FAIL(C).
    let t0 = Instant::now();
    let cbuild = run_cmd(Command::new(gg_binary()).arg("build").arg(&cprog), build_timeout());
    t.cbuild += t0.elapsed();
    match cbuild {
        Err(TimedOut) => return Verdict::Timeout { lane: "gg build (C)" },
        Ok(o) if !o.status.success() => {
            return Verdict::BuildFailC { detail: error_lines(&o) };
        }
        Ok(_) => {}
    }

    // (3) C run — the reference lane's stdout becomes the oracle.
    let t0 = Instant::now();
    let crun = run_cmd(&mut Command::new(cdir.join("prog")), run_timeout());
    t.crun += t0.elapsed();
    let c_out = match crun {
        Err(TimedOut) => return Verdict::Timeout { lane: "C run" },
        Ok(o) if !o.status.success() => {
            return Verdict::Crash { detail: exit_detail(&o) };
        }
        Ok(o) => String::from_utf8_lossy(&o.stdout).to_string(),
    };

    // (3b) ggdef oracle reconciliation — the C oracle (a clean exit 0 + stdout)
    // now exists. A held ggdef `Value` whose stdout disagrees, or a held `Trap`
    // the C run did NOT reproduce (it exited 0 cleanly above), is a
    // SPEC-DIVERGE. A C-side fault would already have short-circuited to CRASH
    // above and that verdict stands — ggdef ADDS an oracle, it never softens a
    // real crash. tier-0 emits no floats (generator.rs, integer arithmetic
    // only), so a Value stdout diff is never spurious float-rendering skew.
    match &ggdef_oracle {
        GgdefOracle::Value(g_out) => {
            if g_out.trim_end() != c_out.trim_end() {
                return Verdict::SpecDiverge {
                    detail: first_diff("ggdef", g_out, "c", &c_out, 0, "0".to_string()),
                };
            }
        }
        GgdefOracle::Trap { fault } => {
            return Verdict::SpecDiverge {
                detail: truncate_chars(
                    &format!("ggdef Trap ({fault}) but C ran to a clean exit 0"),
                    240,
                ),
            };
        }
        GgdefOracle::Skip { .. } => {}
    }

    // (4) self-host lane: driver --emit-c → cc → run, diff vs C (mirrors
    // self_host_emit_cc_run, integration.rs:19388-19476).
    let (driver_exe, lib_dir, runtime_dir) = driver_paths();
    let t0 = Instant::now();
    let emit = run_cmd(
        Command::new(driver_exe)
            .arg(&cprog)
            .arg(lib_dir)
            .arg("--emit-c")
            .arg(format!("--runtime-dir={}", runtime_dir.display())),
        build_timeout(),
    );
    t.driver += t0.elapsed();
    let emit = match emit {
        Err(TimedOut) => return Verdict::Timeout { lane: "self-host driver emit" },
        Ok(o) if !o.status.success() => {
            return Verdict::DriverFail { detail: error_lines(&o) };
        }
        Ok(o) => o,
    };
    let sh_c = work.join("sh.c");
    let sh_bin = work.join("sh");
    std::fs::write(&sh_c, &emit.stdout).expect("failed to write self-host C");
    let t0 = Instant::now();
    let cc = run_cmd(
        Command::new("cc")
            .arg("-O0")
            .arg("-w")
            .arg("-o")
            .arg(&sh_bin)
            .arg(&sh_c)
            .arg("-lm")
            .arg("-lpthread"),
        build_timeout(),
    );
    t.cc += t0.elapsed();
    match cc {
        Err(TimedOut) => return Verdict::Timeout { lane: "cc (self-host)" },
        Ok(o) if !o.status.success() => {
            return Verdict::CcFail { detail: error_lines(&o) };
        }
        Ok(_) => {}
    }
    let t0 = Instant::now();
    let shrun = run_cmd(&mut Command::new(&sh_bin), run_timeout());
    t.shrun += t0.elapsed();
    match shrun {
        Err(TimedOut) => return Verdict::Timeout { lane: "self-host run" },
        Ok(o) if !o.status.success() => {
            // C exited 0 by this point, so a nonzero self-host exit is an
            // exit-code divergence (includes self-host-only crashes).
            return Verdict::DivergeSh {
                detail: format!("self-host {} vs C exit=0", exit_detail(&o)),
            };
        }
        Ok(o) => {
            let sh_out = String::from_utf8_lossy(&o.stdout).to_string();
            if sh_out.trim_end() != c_out.trim_end() {
                return Verdict::DivergeSh {
                    detail: first_diff("c", &c_out, "self", &sh_out, 0, "0".to_string()),
                };
            }
        }
    }

    // (5) LLVM lane (ON by default; GG_SMITH_SKIP_LLVM=1 skips).
    if !cfg.skip_llvm {
        let t0 = Instant::now();
        let lbuild = run_cmd(
            Command::new(gg_binary()).arg("build").arg("--backend=llvm").arg(&lprog),
            build_timeout(),
        );
        t.lbuild += t0.elapsed();
        match lbuild {
            Err(TimedOut) => return Verdict::Timeout { lane: "gg build (llvm)" },
            Ok(o) if !o.status.success() => {
                return Verdict::BuildFailLlvm { detail: error_lines(&o) };
            }
            Ok(_) => {}
        }
        let t0 = Instant::now();
        let lrun = run_cmd(&mut Command::new(ldir.join("prog")), run_timeout());
        t.lrun += t0.elapsed();
        match lrun {
            Err(TimedOut) => return Verdict::Timeout { lane: "llvm run" },
            Ok(o) => {
                let l_out = String::from_utf8_lossy(&o.stdout).to_string();
                if !o.status.success() || l_out != c_out {
                    let l_exit = if o.status.success() {
                        "0".to_string()
                    } else {
                        exit_detail(&o)
                    };
                    return Verdict::DivergeLlvm {
                        detail: first_diff("c", &c_out, "llvm", &l_out, 0, l_exit),
                    };
                }
            }
        }
    }

    // All lanes agreed. If ggdef adjudicated (Value stdout matched, or a Trap
    // that C reproduced — neither reached here as a divergence), that is a full
    // MATCH; if ggdef ABSTAINED (Skip), record GGDEF-SKIP so the ggdef-coverage
    // gap for this shape stays visible even though the cross-impl diff passed.
    match ggdef_oracle {
        GgdefOracle::Skip { reason } => Verdict::GgdefSkip { detail: reason },
        GgdefOracle::Value(_) | GgdefOracle::Trap { .. } => Verdict::Match,
    }
}

/// Inverted oracle for the throws-position rejection tier (`TIER_THROWS`, T3b).
/// The generated program is well-formed EXCEPT for one unhandled `throws` call,
/// so production is REQUIRED to reject it with `E_UnhandledThrows`. Four
/// verdicts via an ORDERED decision tree (mirrors `check_gg_fails_no_desugar`,
/// tests/integration.rs):
///
///   check SUCCEEDS                                → UnhandledThrowsSlip   (a real T3a hole)
///   check FAILS, stderr `found `Result[`          → ThrowsDesugarLeak    (CHECK THIS FIRST)
///   check FAILS, code `error[E_UnhandledThrows]`  → UnhandledThrowsRejected (the BENIGN PASS)
///   check FAILS, any other diagnostic             → GenInvalid  (generator bug OR mis-coded rejection)
///
/// The desugar-LEAK arm MUST precede the PASS / GEN-INVALID arms: a leak is a
/// plain type-mismatch diagnostic (`expected `int`, found `Result[int, String]`)
/// that could in principle co-occur with the exact code; ordering leak-first
/// routes a real compiler regression to the non-benign `ThrowsDesugarLeak`
/// instead of greening it as the PASS.
///
/// The PASS arm requires the EXACT diagnostic code, never a loose
/// `contains("throws")`: any rejection whose text merely mentions "throws" (a
/// parse error quoting the generated `int risky() throws String:` helper line,
/// a throws-adjacent diagnostic like `E_ThrowInNonThrowingFunction`) would
/// otherwise be silently misrouted into the benign PASS — an inverted oracle
/// that loses precision without failing. Routing pinned by
/// `classify_throws_routing` below.
fn classify_throws(check: Result<Output, TimedOut>) -> Verdict {
    let o = match check {
        Err(TimedOut) => return Verdict::Timeout { lane: "gg check" },
        Ok(o) => o,
    };
    // check SUCCEEDS → production wrongly accepted an unhandled `throws`: a real
    // T3a enforcement hole (silent swallow / miscompile). NOT a generator bug —
    // must surface distinctly, never folded into GEN-INVALID.
    if o.status.success() {
        return Verdict::UnhandledThrowsSlip {
            detail: "gg check ACCEPTED an unhandled `throws` call (T3a enforcement hole)"
                .to_string(),
        };
    }
    // check FAILS → inspect the diagnostic. Strip ANSI first: `gg` colorizes
    // even to a pipe, so a raw substring test would miss the signal.
    let stderr = strip_ansi(&String::from_utf8_lossy(&o.stderr));
    // (1) desugar leak — CHECKED FIRST (its predicate overlaps GEN-INVALID's).
    if stderr.contains("found `Result[") {
        return Verdict::ThrowsDesugarLeak { detail: error_lines(&o) };
    }
    // (2) the D23 signal → the BENIGN PASS. The EXACT diagnostic code is
    // required (`error[E_UnhandledThrows]` — `report_semantic_error` renders
    // `.with_code(kind.code())`, src/errors.rs, inside ONE color span, so the
    // substring is contiguous after `strip_ansi`). A rejection that merely
    // MENTIONS "throws" — a parse error whose snippet quotes the
    // `int risky() throws String:` helper line, an
    // `E_ThrowInNonThrowingFunction`, any throws-adjacent diagnostic — is NOT
    // the tier's pass and falls through to GEN-INVALID.
    if stderr.contains("error[E_UnhandledThrows]") {
        return Verdict::UnhandledThrowsRejected;
    }
    // (3) rejected WITHOUT the exact code → a generator bug (a malformed
    // program) OR a compiler regression that re-coded the rejection. Both
    // block the green gate; triage from the detail line, same bucket as
    // tier 0's check-reject.
    Verdict::GenInvalid { detail: error_lines(&o) }
}

/// Generate + classify one seed. The seed's scratch dir is removed when the
/// verdict is benign (MATCH or GGDEF-SKIP); repro dirs are kept only for
/// divergent/suspicious seeds.
fn run_seed(seed: u64, cfg: &Config, tmp_root: &Path) -> (Verdict, Timings, PathBuf) {
    let mut t = Timings::default();
    let t0 = Instant::now();
    let src = generator::generate(seed, cfg.tier);
    t.generate += t0.elapsed();
    let dir = tmp_root.join(format!("s{seed}"));
    let v = classify(&src, &dir, cfg, &mut t);
    if v.is_benign() {
        let _ = std::fs::remove_dir_all(&dir);
    }
    (v, t, dir)
}

/// Chunked scoped-thread map over the seed range, preserving order.
/// Worker count mirrors `parallel_map_fixtures` (integration.rs:261-300).
fn parallel_map_seeds(
    seeds: &[u64],
    cfg: &Config,
    tmp_root: &Path,
) -> Vec<(u64, Verdict, Timings, PathBuf)> {
    let cpus = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
    let n_workers = (cpus / 2).clamp(2, 8).min(seeds.len().max(1));
    let chunk_size = seeds.len().div_ceil(n_workers);
    std::thread::scope(|s| {
        let handles: Vec<_> = seeds
            .chunks(chunk_size)
            .map(|chunk| {
                s.spawn(move || {
                    chunk
                        .iter()
                        .map(|&seed| {
                            let (v, t, dir) = run_seed(seed, cfg, tmp_root);
                            if !v.is_benign() {
                                // Progressive report (one print! call so the
                                // two lines never interleave across workers).
                                // Benign verdicts (MATCH / GGDEF-SKIP) are
                                // quiet — their scratch dir is already gone.
                                print!(
                                    "seed {seed}: {} [{}]\n    repro: {}\n",
                                    v.label(),
                                    v.detail(),
                                    dir.display(),
                                );
                            }
                            (seed, v, t, dir)
                        })
                        .collect::<Vec<_>>()
                })
            })
            .collect();
        handles.into_iter().flat_map(|h| h.join().expect("smith worker panicked")).collect()
    })
}

/// Parse `GG_SMITH_SEEDS` — `A..B` (inclusive on BOTH ends; `A..=B` also
/// accepted) or a single seed `N`.
fn parse_seed_range(spec: &str) -> (u64, u64) {
    let spec = spec.trim();
    let parsed = if let Some((lo, hi)) = spec.split_once("..") {
        let hi = hi.strip_prefix('=').unwrap_or(hi);
        match (lo.trim().parse::<u64>(), hi.trim().parse::<u64>()) {
            (Ok(a), Ok(b)) if a <= b => Some((a, b)),
            _ => None,
        }
    } else {
        spec.parse::<u64>().ok().map(|n| (n, n))
    };
    parsed.unwrap_or_else(|| {
        panic!("GG_SMITH_SEEDS must be `A..B` (inclusive) or a single seed, got {spec:?}")
    })
}

fn mk_tmp_root(prefix: &str) -> PathBuf {
    let root = std::env::temp_dir().join(format!(
        "{prefix}_{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis())
            .unwrap_or(0),
    ));
    std::fs::create_dir_all(&root).expect("failed to create smith tmp root");
    root
}

// ─────────────────────────────── tests ─────────────────────────────────

/// Always-on: the generator is byte-reproducible from
/// `(GENERATOR_VERSION, seed)` — two independent generations of the same
/// seed must be identical, and the program skeleton must be present.
#[test]
fn generator_determinism() {
    for seed in [0u64, 1, 7, 36, 42, 99, 1000, 123_456] {
        let a = generator::generate(seed, 0);
        let b = generator::generate(seed, 0);
        assert_eq!(
            a, b,
            "generator is not deterministic for (v{}, seed {seed})",
            generator::GENERATOR_VERSION
        );
        assert!(a.contains("void main():"), "seed {seed}: program lost its main()");
        assert!(
            a.trim_end().ends_with("print(\"end\")"),
            "seed {seed}: program lost its end sentinel"
        );
    }
}

/// Always-on: the throws-position rejection tier (`TIER_THROWS`, T3b) is
/// byte-reproducible per seed, and every generated program actually carries a
/// `throws` helper + an unhandled `risky()` call (so the inverted oracle has
/// something to reject). Cheap insurance the batch diagnostic wouldn't surface.
#[test]
fn generator_determinism_throws() {
    for seed in [0u64, 1, 7, 36, 42, 99, 1000, 123_456] {
        let a = generator::generate(seed, generator::TIER_THROWS);
        let b = generator::generate(seed, generator::TIER_THROWS);
        assert_eq!(
            a, b,
            "throws-tier generator is not deterministic for (v{}, seed {seed})",
            generator::GENERATOR_VERSION
        );
        assert!(a.contains("void main():"), "seed {seed}: throws program lost its main()");
        assert!(
            a.contains("throws String:"),
            "seed {seed}: throws program lost its `throws` helper"
        );
        assert!(a.contains("risky("), "seed {seed}: throws program lost its risky() call");
        assert!(
            a.trim_end().ends_with("print(\"end\")"),
            "seed {seed}: throws program lost its end sentinel"
        );
    }
}

/// Always-on: `classify_throws` routing (T3b's inverted oracle). The benign
/// PASS requires the EXACT diagnostic code — `error[E_UnhandledThrows]` — not
/// any mention of "throws": a parse error whose snippet quotes the
/// `int risky() throws String:` helper line, or a throws-ADJACENT diagnostic
/// (`E_ThrowInNonThrowingFunction`), must route to GEN-INVALID, never silently
/// green. Also pins slip-on-accept, the leak-FIRST arm ordering, and that the
/// code substring survives `gg`'s ANSI-colored output (via `strip_ansi`).
#[cfg(unix)]
#[test]
fn classify_throws_routing() {
    use std::os::unix::process::ExitStatusExt;
    fn checked(exit_code: i32, stderr: &str) -> Result<Output, TimedOut> {
        Ok(Output {
            status: std::process::ExitStatus::from_raw(exit_code << 8),
            stdout: Vec::new(),
            stderr: stderr.as_bytes().to_vec(),
        })
    }
    // check ACCEPTED → a T3a enforcement hole, never a pass.
    assert_eq!(classify_throws(checked(0, "")).label(), "UNHANDLED-THROWS-SLIP");
    // Leak-FIRST ordering: when the desugar leak and the exact code are BOTH
    // present, the leak (a D23 regression) must win over the benign PASS.
    assert_eq!(
        classify_throws(checked(
            1,
            "error[E_UnhandledThrows]: this call throws `String`\n\
             error[E_TypeMismatch]: type mismatch: expected `int`, found `Result[int, String]`\n",
        ))
        .label(),
        "THROWS-DESUGAR-LEAK",
    );
    // The benign PASS: the exact code, ANSI-colored exactly as `gg` renders it
    // (one color span wraps the whole `error[E_...]` header — src/errors.rs
    // `report_semantic_error` + codespan-reporting).
    assert_eq!(
        classify_throws(checked(
            1,
            "\x1b[0m\x1b[1m\x1b[38;5;9merror[E_UnhandledThrows]\x1b[0m\x1b[1m: this call throws \
             `String` but the error is not handled here\x1b[0m\n",
        ))
        .label(),
        "UNHANDLED-THROWS-REJECTED",
    );
    // A parse error whose SNIPPET quotes the `throws` helper line mentions
    // "throws" but carries NO diagnostic code → a generator bug (GEN-INVALID),
    // NOT the tier's pass. (The pre-tightening `contains("throws")` predicate
    // misrouted this to the benign PASS.)
    assert_eq!(
        classify_throws(checked(
            1,
            "error: expected identifier, found ')'\n  \u{250c}\u{2500} prog.gg:1:14\n\
             1 \u{2502} int risky(int) throws String:\n",
        ))
        .label(),
        "GEN-INVALID",
    );
    // A throws-ADJACENT diagnostic with a DIFFERENT code → GEN-INVALID.
    assert_eq!(
        classify_throws(checked(
            1,
            "error[E_ThrowInNonThrowingFunction]: throw in function that doesn't declare \
             `throws`\n",
        ))
        .label(),
        "GEN-INVALID",
    );
    assert_eq!(classify_throws(Err(TimedOut)).label(), "TIMEOUT");
}

/// Env-gated batch differential — an always-pass DIAGNOSTIC (prints a
/// verdict table, a divergent-seed list, repro paths, and the first
/// error/diff line per non-MATCH seed; never asserts counts).
///
///   GG_SMITH_SEEDS=1..200 cargo test --test smith -- --nocapture
///
/// No-op skip without the env var.
#[test]
fn smith_batch() {
    let Ok(spec) = std::env::var("GG_SMITH_SEEDS") else {
        return; // diagnostic-only: opt in via GG_SMITH_SEEDS=A..B
    };
    if spec.is_empty() {
        return;
    }
    let (lo, hi) = parse_seed_range(&spec);
    let cfg = Config::from_env();
    let tmp_root = mk_tmp_root("gg_smith");
    let seeds: Vec<u64> = (lo..=hi).collect();

    println!(
        "=== gorget-smith batch: generator v{}, tier {}, seeds {lo}..={hi} ({} seeds), \
         llvm lane {} ===",
        generator::GENERATOR_VERSION,
        cfg.tier,
        seeds.len(),
        if cfg.skip_llvm { "OFF (GG_SMITH_SKIP_LLVM=1)" } else { "ON" },
    );

    // One-time driver build BEFORE the parallel fan-out (OnceLock would
    // serialize the stampede anyway, but this keeps per-seed timing honest).
    // SKIPPED for the throws-position rejection tier: it is CHECK-ONLY, never
    // reaches the self-host lane, and the ~57s build would otherwise be paid for
    // nothing (and would spuriously PANIC if the driver were broken unrelatedly).
    if cfg.tier != generator::TIER_THROWS {
        let _ = driver_paths();
    }

    let results = parallel_map_seeds(&seeds, &cfg, &tmp_root);

    let mut counts: BTreeMap<&'static str, usize> = BTreeMap::new();
    let mut totals = Timings::default();
    let mut suspicious: Vec<u64> = Vec::new();
    let mut gen_invalid: Vec<u64> = Vec::new();
    let mut ggdef_skip: Vec<u64> = Vec::new();
    // throws-position tier (TIER_THROWS) — kept in DISTINCT lists so a slip and
    // a leak read apart (the acceptance criterion counts them separately), and
    // BOTH gate the green report below (a slip could otherwise silently green).
    let mut throws_slip: Vec<u64> = Vec::new();
    let mut throws_leak: Vec<u64> = Vec::new();
    for (seed, v, t, _dir) in &results {
        *counts.entry(v.label()).or_insert(0) += 1;
        totals.add(t);
        match v {
            Verdict::Match => {}
            // Benign: cross-impl lanes agreed; ggdef merely abstained. Tracked
            // separately so a ggdef-coverage gap is visible without polluting
            // the suspicious list (a GGDEF-SKIP is NOT a divergence).
            Verdict::GgdefSkip { .. } => ggdef_skip.push(*seed),
            // Benign PASS of the throws-position tier — no-op arm (mirror the
            // `Match` arm) so the `_ =>` catch-all does NOT sweep it into
            // `suspicious`; without this the report never greens.
            Verdict::UnhandledThrowsRejected => {}
            Verdict::GenInvalid { .. } => gen_invalid.push(*seed),
            // NON-BENIGN throws-tier verdicts — surfaced distinctly AND gated.
            Verdict::UnhandledThrowsSlip { .. } => throws_slip.push(*seed),
            Verdict::ThrowsDesugarLeak { .. } => throws_leak.push(*seed),
            _ => suspicious.push(*seed),
        }
    }

    let n = seeds.len() as f64;
    println!("\n=== gorget-smith verdict table ===");
    println!(
        "seeds: {}   {}",
        seeds.len(),
        counts.iter().map(|(k, v)| format!("{k}: {v}")).collect::<Vec<_>>().join("  "),
    );
    println!(
        "avg per seed: gen={:.3}s check={:.3}s ggdef={:.4}s cbuild={:.3}s crun={:.3}s \
         driver={:.3}s cc={:.3}s shrun={:.3}s lbuild={:.3}s lrun={:.3}s total={:.3}s",
        totals.generate.as_secs_f64() / n,
        totals.check.as_secs_f64() / n,
        totals.ggdef.as_secs_f64() / n,
        totals.cbuild.as_secs_f64() / n,
        totals.crun.as_secs_f64() / n,
        totals.driver.as_secs_f64() / n,
        totals.cc.as_secs_f64() / n,
        totals.shrun.as_secs_f64() / n,
        totals.lbuild.as_secs_f64() / n,
        totals.lrun.as_secs_f64() / n,
        totals.total().as_secs_f64() / n,
    );
    println!("divergent/suspicious seeds: {suspicious:?}");
    println!("gen-invalid seeds: {gen_invalid:?}");
    println!("ggdef-skip (out-of-surface) seeds: {ggdef_skip:?}");
    if cfg.tier == generator::TIER_THROWS {
        // Both are REAL findings for the throws tier: a slip is a T3a
        // enforcement hole; a leak is a D23 diagnostic regression.
        println!("throws-SLIP (production ACCEPTED an unhandled throws) seeds: {throws_slip:?}");
        println!("throws-desugar-LEAK (`found `Result[`) seeds: {throws_leak:?}");
    }
    if suspicious.is_empty()
        && gen_invalid.is_empty()
        && throws_slip.is_empty()
        && throws_leak.is_empty()
    {
        let _ = std::fs::remove_dir_all(&tmp_root);
        println!("all seeds benign — repro root removed");
    } else {
        println!("repro root (divergent/suspicious seed dirs kept): {}", tmp_root.display());
        println!(
            "minimize a seed with: GG_SMITH_MINIMIZE=<seed> cargo test --test smith -- --nocapture"
        );
    }
}

/// End of the removable region starting at line `i`: the line itself, plus —
/// when it opens a block (ends with `:`) — every following line that is
/// blank or more-indented. Port of the prototype `ddmin.py`.
fn block_end(lines: &[String], i: usize) -> usize {
    fn indent_of(l: &str) -> usize {
        l.len() - l.trim_start().len()
    }
    let mut j = i + 1;
    if lines[i].trim_end().ends_with(':') {
        let base = indent_of(&lines[i]);
        while j < lines.len()
            && (lines[j].trim().is_empty() || indent_of(&lines[j]) > base)
        {
            j += 1;
        }
    }
    j
}

/// Env-gated minimizer: block-aware line removal with the predicate "same
/// classification as the original".
///
///   GG_SMITH_MINIMIZE=36 cargo test --test smith -- --nocapture
///
/// No-op skip without the env var.
#[test]
fn smith_minimize() {
    let Ok(spec) = std::env::var("GG_SMITH_MINIMIZE") else {
        return; // opt in via GG_SMITH_MINIMIZE=<seed>
    };
    if spec.is_empty() {
        return;
    }
    let seed: u64 = spec
        .trim()
        .parse()
        .unwrap_or_else(|_| panic!("GG_SMITH_MINIMIZE must be a seed number, got {spec:?}"));
    let cfg = Config::from_env();
    let tmp_root = mk_tmp_root("gg_smith_min");
    let scratch = tmp_root.join("work");

    let src = generator::generate(seed, cfg.tier);
    let mut t = Timings::default();
    let baseline = classify(&src, &scratch, &cfg, &mut t);
    println!(
        "seed {seed} baseline: {} [{}] ({} lines)",
        baseline.label(),
        baseline.detail(),
        src.lines().count(),
    );
    if baseline.is_benign() {
        println!("seed {seed} is {} (benign) — nothing to minimize", baseline.label());
        let _ = std::fs::remove_dir_all(&tmp_root);
        return;
    }
    let target = baseline.label();

    let mut lines: Vec<String> = src.lines().map(str::to_string).collect();
    let mut changed = true;
    while changed {
        changed = false;
        let mut i = 0;
        while i < lines.len() {
            let j = block_end(&lines, i);
            let mut cand: Vec<String> = lines[..i].to_vec();
            cand.extend_from_slice(&lines[j..]);
            if !cand.is_empty() {
                let cand_src = cand.join("\n") + "\n";
                let _ = std::fs::remove_dir_all(&scratch);
                let mut tt = Timings::default();
                let v = classify(&cand_src, &scratch, &cfg, &mut tt);
                if v.label() == target {
                    lines = cand;
                    changed = true;
                    continue; // same slot again — the next line shifted into i
                }
            }
            i += 1;
        }
    }

    let min_src = lines.join("\n") + "\n";
    let out_path = tmp_root.join(format!("min_s{seed}.gg"));
    std::fs::write(&out_path, &min_src).expect("failed to write minimized program");
    let _ = std::fs::remove_dir_all(&scratch);
    println!(
        "=== minimized seed {seed} (classification preserved: {target}, {} lines) ===\n{min_src}\
         saved: {}",
        min_src.lines().count(),
        out_path.display(),
    );
}
