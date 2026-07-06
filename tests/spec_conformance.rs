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
//! All three lanes floor at MIN_FIXTURES — every committed seed MATCHes on
//! every impl. The C and LLVM lanes previously floored one below the self-host
//! lane because `smith_move_param_concat.gg` (`String !p` move-param + concat)
//! was a both-backend BUILD-FAIL: the C backend emitted an invalid pointer-add
//! `(void*)a + (void*)b` (cc rejected), the LLVM backend an invalid `add ptr`
//! (llc rejected), while the self-host lowerer was already reference-correct.
//! That defect is FIXED (the `!`-move String binop operand now derefs its
//! `MutPtr` slot at the consume site, `src/ir/lowering/exprs/operators.rs`
//! `cow_deref_if_ptr`), so the SAME commit ratcheted `C_MATCH_FLOOR` /
//! `LLVM_MATCH_FLOOR` up to equal the self-host floor. All lanes now floor at
//! MIN_FIXTURES.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

use ggdef::{parse_frontmatter, Expect};

// ── Floors — regenerated in-worktree (see the module-doc command). ──────────
const C_MATCH_FLOOR: usize = 187;
const LLVM_MATCH_FLOOR: usize = 187;
const SELFHOST_MATCH_FLOOR: usize = 187;

/// The glob-emptiness guard: `spectests/run` must contain at least this many
/// `.gg` seeds or a shrunken corpus would make a lane vacuously green. This is
/// the seed COUNT; all three lane MATCH floors currently equal it (every seed
/// MATCHes on every impl).
const MIN_FIXTURES: usize = 187;

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

/// Run a command with nulled stdin and a deadline. Does NOT panic on timeout —
/// a timeout is a lane classification, not a harness failure. stdout/stderr are
/// drained on background threads to avoid the >64KB pipe-buffer deadlock
/// (integration.rs:171-190 / smith:166-212).
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
///   * `expect.exit != 0` → require a NON-ZERO exit and that the expected
///     (pre-trap) stdout is a PREFIX of the observed stdout. The exact code is
///     deliberately NOT compared: ggdef's 101/102/103 are PROVISIONAL
///     (spec/ggdef/src/eval.rs:40) and the production C runtime always exits 1
///     on a trap via `gorget_panic`, so the codes can never be equal. All 5
///     current seeds are exit-0, so this branch is wired DEFENSIVELY and is
///     currently unexercised; revisit once a nonzero-exit seed lands and the
///     production trap-exit convention is pinned.
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

    // Guard the glob: an empty (or shrunken) corpus must not make a lane
    // vacuously green.
    assert!(
        fixtures.len() >= MIN_FIXTURES,
        "expected >= {MIN_FIXTURES} run fixtures in spectests/run, found {}",
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

        let verdict = match step(&scratch, path) {
            Ok(out) => adjudicate(&fm.expect, &out),
            Err(v) => v,
        };
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
