//! Security test harness for Gorget.
//!
//! Each fixture under `tests/fixtures/security/` is one adversarial program
//! designed to probe a specific memory-safety or correctness guarantee.
//! Fixtures are classified by helper:
//!
//! - [`security_safe`]       — a well-typed program that must build under
//!                             `--sanitize` and run exit-0 with expected stdout.
//! - [`security_safe_no_leak`] — as above, but run under `detect_leaks=1` so a
//!                             LEAK is a hard failure. For bug classes that are
//!                             stdout-invisible, which a `security_safe` run
//!                             (`detect_leaks=0`) cannot see.
//! - [`security_traps`]      — a program that intentionally performs a
//!                             runtime-defined trap (e.g. division by zero)
//!                             and must panic with the Gorget-level message,
//!                             not raw C UB.
//! - [`security_rejected`]   — a program the compiler must reject at `gg check`,
//!                             with expected stderr pattern.
//! - [`security_known_unsafe`] — a program that currently reveals a real bug.
//!                             The test asserts the bug is *still* present
//!                             (so regressions can't make it worse silently
//!                             and so fixes force a reclassification).
//! - [`security_safe_except_on`] — `security_safe` everywhere except ONE named
//!                             backend, where a filed, cited compiler defect
//!                             makes it trip. NOT a skip: on that backend it
//!                             asserts the trip STILL happens, so fixing the
//!                             cited item turns it red and forces the
//!                             annotation out.
//!
//! When a known-unsafe bug is fixed, the test will start failing — that's
//! the signal to reclassify as `security_safe` / `security_rejected` /
//! `security_traps` and relabel the fixture.
//!
//! To run just the security suite:
//!     cargo test --test security
//!
//! ⚠ THE SUITE RUNS ON WHICHEVER BACKEND `GG_BACKEND` SELECTS. Unset means the
//! compiler's default; `GG_BACKEND=llvm` makes this a genuinely second lane
//! rather than a re-run of the first, which is what it was for as long as this
//! file ignored the variable. See [`gg_command`] for which subcommands carry
//! the flag and why, and [`backend_flag_selection_is_wired`] for the guard that
//! keeps the wiring honest. ASan serialises on global state, so the LLVM lane
//! wants `-- --test-threads=1`:
//!     GG_BACKEND=llvm cargo test --test security -- --test-threads=1
//!
//! ⚠ `cargo test --test security -- --ignored` fails EVERYTHING it runs, by
//! design — 25 of 25 at the time of writing. Those tests assert INTENDED states
//! that do not hold yet, so failing is what they are for; a bare `--ignored`
//! sweep is not a health check. One of the 25 is [`backend_flag_wiring_inner_probe`],
//! a child-process probe that requires `GG_BACKEND` to be set and whose parent
//! asserts it FAILS when the variable is unset — do not file it; it is exercised
//! through [`backend_flag_selection_is_wired`].
//!
//! The full build includes `-fsanitize=address,undefined` via the
//! compiler's own `--sanitize` flag. ⚠ On `--backend=llvm` that instruments the
//! runtime only, not generated user code (`todo/t0727`): leaks and
//! runtime-side faults are caught there, user-code faults are not.

use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

/// Sanitize builds are slower than the integration suite's, so the default
/// is higher (180 vs 120). Override with GG_BUILD_TIMEOUT_SECS for full
/// manual control. When unset, auto-scales by /proc/loadavg /
/// available_parallelism so the gate doesn't spuriously trip on shared /
/// loaded hosts.
fn build_timeout() -> Duration {
    Duration::from_secs(env_or_load_adjusted_secs("GG_BUILD_TIMEOUT_SECS", 180))
}

/// Same shape as integration.rs's helper. Linux only — falls back to
/// `base` when `/proc/loadavg` isn't readable.
fn env_or_load_adjusted_secs(var: &str, base: u64) -> u64 {
    if let Some(secs) = std::env::var(var).ok().and_then(|s| s.parse::<u64>().ok()) {
        return secs;
    }
    let load = std::fs::read_to_string("/proc/loadavg")
        .ok()
        .and_then(|s| s.split_whitespace().next()?.parse::<f64>().ok())
        .unwrap_or(0.0);
    let cpus = std::thread::available_parallelism()
        .map(|n| n.get() as f64)
        .unwrap_or(1.0);
    let ratio = (load / cpus).max(1.0);
    ((base as f64) * ratio).ceil() as u64
}

fn test_binary_timeout() -> Duration {
    Duration::from_secs(
        std::env::var("GG_TEST_TIMEOUT_SECS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(30),
    )
}

/// The backend selector, mirroring `tests/integration.rs`'s helper of the same
/// name. `None` (or an empty value) means the compiler's default, `c-lir`.
fn gg_backend() -> Option<String> {
    std::env::var("GG_BACKEND").ok().filter(|s| !s.is_empty())
}

/// Invoke the compiler under test.
///
/// When `GG_BACKEND` is set, append `--backend=<b>` to `gg build` — and ONLY
/// to `build`. Until this existed, `.github/workflows/ci.yml`'s
/// "Security tests (LLVM + ASan + UBSan)" job set `GG_BACKEND: llvm` and this
/// file read it nowhere, so that job was a verbatim re-run of the C suite: a
/// whole lane of this project's sanitizer evidence was a copy of the other
/// lane's. (`--sanitize` being a silent no-op on that backend, t0723, is the
/// same vacuity one layer down — a suite that DID select the backend would
/// still have asserted nothing.)
///
/// ⚠ `build` only, and the reason is NOT that the CLI rejects the flag
/// elsewhere — it does not; `gg check --backend=llvm` exits 0 and ignores it.
/// The reason is that a backend is only meaningful where code is GENERATED.
/// `security_rejected` uses `gg check`, which stops at semantic analysis, so
/// its 32 fixtures are backend-independent by construction; appending the flag
/// there would advertise a lane distinction that does not exist.
fn gg_command(subcommand: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO"));
    cmd.args(["run", "--quiet", "--", subcommand]);
    if let Some(flag) = backend_flag_for(subcommand, gg_backend().as_deref()) {
        cmd.arg(flag);
    }
    cmd
}

/// The backend-flag DECISION, split out of [`gg_command`] so its branches can
/// be enumerated without an environment.
///
/// WARNING: testing this function alone is NOT a guard on the mechanism, and
/// mistaking it for one is how the original defect comes back. The bug this
/// replaces was not a wrong decision — it was a decision nothing ever
/// CONSULTED: the LLVM CI job read as coverage for an unknown period while
/// `GG_BACKEND` went unread here. A guard that only checks this function stays
/// green while `gg_command` stops calling it. See
/// [`backend_flag_selection_is_wired`], which asserts on the command
/// `gg_command` actually builds.
fn backend_flag_for(subcommand: &str, backend: Option<&str>) -> Option<String> {
    match backend {
        // `build` is the only subcommand this file uses that GENERATES code,
        // and a backend is meaningless anywhere else. See `gg_command`.
        Some(b) if subcommand == "build" => Some(format!("--backend={b}")),
        _ => None,
    }
}

/// Collect a `Command`'s arguments as plain strings, so a guard can assert on
/// what was actually constructed rather than on what a helper would return.
fn args_of(cmd: &Command) -> Vec<String> {
    cmd.get_args().map(|a| a.to_string_lossy().into_owned()).collect()
}

/// The inner half of [`backend_flag_selection_is_wired`], re-executed as a
/// CHILD process with `GG_BACKEND` set.
///
/// `#[ignore]`d because it asserts a positive that holds only when the variable
/// is set; it is meant to be reached by its parent, not by a sweep. It is
/// nonetheless a real assertion on the real `gg_command`.
///
/// Why a child process rather than `std::env::set_var`: this suite runs its
/// tests in parallel and each one SPAWNS `cargo run`, which inherits the
/// process environment. Mutating `GG_BACKEND` in-process — even briefly, even
/// under `#[serial]`, which does not exclude non-serial tests — could hand a
/// concurrently-spawning fixture the wrong backend and produce a wrong-lane
/// result. Setting the variable on a child is race-free by construction.
#[test]
#[ignore = "inner probe: re-executed by backend_flag_selection_is_wired with GG_BACKEND set"]
fn backend_flag_wiring_inner_probe() {
    let backend = gg_backend().expect(
        "inner probe requires GG_BACKEND to be set; it is re-executed by \
         backend_flag_selection_is_wired, not run directly",
    );
    let build = args_of(&gg_command("build"));
    assert!(
        build.contains(&format!("--backend={backend}")),
        "`gg_command(build)` did not carry `--backend={backend}` with GG_BACKEND={backend} \
         set. THE SELECTOR IS NOT WIRED: the LLVM security job is running the C suite under \
         a different name, and ~180 fixtures of a whole lane's memory-safety evidence are a \
         copy of the other lane's.\nargs: {build:?}"
    );
    let check = args_of(&gg_command("check"));
    assert!(
        !check.iter().any(|a| a.starts_with("--backend")),
        "`gg_command(check)` carried a backend flag. `check` does no codegen, so this \
         advertises a lane distinction that does not exist.\nargs: {check:?}"
    );
}

/// Guard for the selector itself: `--test security` must actually run on the
/// backend it was asked for.
///
/// It pins the WIRING that `.github/workflows/ci.yml`'s "Security tests (LLVM +
/// ASan + UBSan)" job silently depends on. That job sets `GG_BACKEND: llvm`
/// and, for as long as this file ignored it, ran the C suite a second time
/// under an LLVM name — ~180 fixtures' worth of a whole lane's memory-safety
/// evidence that was a copy of the other lane's. Nothing was red; there was
/// simply nothing there.
///
/// WARNING: it asserts on the command `gg_command` ACTUALLY BUILDS, not on
/// [`backend_flag_for`]'s return value. That distinction is the entire point.
/// An earlier version of this guard checked only the pure decision function and
/// stayed GREEN when `gg_command` was edited to stop consulting the environment
/// — i.e. when the exact original defect was reintroduced. A guard that cannot
/// catch its own class is worse than none, because it reads as coverage.
///
/// The environment is supplied to a CHILD process
/// ([`backend_flag_wiring_inner_probe`]) rather than mutated in this one; see
/// that function for why.
#[test]
fn backend_flag_selection_is_wired() {
    let exe = std::env::current_exe().expect("test binary path");
    let probe = |set: bool| -> std::process::Output {
        let mut c = Command::new(&exe);
        c.args(["--exact", "backend_flag_wiring_inner_probe", "--ignored", "--nocapture"]);
        if set { c.env("GG_BACKEND", "llvm"); } else { c.env_remove("GG_BACKEND"); }
        c.output().expect("re-exec the test binary")
    };

    // 1. THE WIRING, under an environment we control.
    let out = probe(true);
    assert!(
        out.status.success(),
        "the wiring probe FAILED under GG_BACKEND=llvm — `gg_command` is not consulting \
         the environment.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );

    // 2. ...and the negative, so an unconditionally-appended flag is caught too.
    let out = probe(false);
    assert!(
        !out.status.success(),
        "the wiring probe PASSED with GG_BACKEND unset. It requires the variable, so either \
         `gg_backend()` is inventing a value or `gg_command` appends a backend \
         unconditionally FOR EVERY SUBCOMMAND. (A build-only unconditional append is \
         caught by the ambient-environment branch below, not here.)"
    );

    // 3. The decision's branches: through the real command for the ambient
    //    environment, through the pure function for values it cannot be.
    let ambient = gg_backend();
    let build = args_of(&gg_command("build"));
    match ambient.as_deref() {
        Some(b) => assert!(
            build.contains(&format!("--backend={b}")),
            "ambient GG_BACKEND={b} is not reaching `gg build`.\nargs: {build:?}"
        ),
        None => assert!(
            !build.iter().any(|a| a.starts_with("--backend")),
            "no GG_BACKEND is set, yet `gg build` carried a backend flag.\nargs: {build:?}"
        ),
    }
    assert_eq!(
        backend_flag_for("build", Some("c-lir")).as_deref(),
        Some("--backend=c-lir"),
        "the selector must pass through whatever it is given, not just `llvm`."
    );
    assert_eq!(backend_flag_for("build", None), None);
    // `check` does no codegen, so a backend there would be noise advertising a
    // lane distinction that does not exist (`security_rejected`'s 32 fixtures).
    assert_eq!(backend_flag_for("check", Some("llvm")), None);
    assert_eq!(backend_flag_for("check", None), None);
}

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
/// This is the ASAN target, so the uncapped drain mattered doubly here: a
/// sanitizer report on a runaway fixture is exactly when the capture is largest.
fn run_with_deadline(cmd: &mut Command, fixture: &str, timeout: Duration) -> std::process::Output {
    match gorget::proc_guard::run_with_deadline(cmd, timeout) {
        Ok(out) => out,
        Err(gorget::proc_guard::RunFailure::Deadline { secs }) => {
            panic!("Process for {fixture} timed out after {secs}s")
        }
        Err(gorget::proc_guard::RunFailure::Overflow { cap }) => {
            panic!("Process for {fixture} produced runaway output (>{cap} bytes) — killed")
        }
    }
}

fn fixture_path(name: &str) -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = manifest_dir
        .join("tests/fixtures/security")
        .join(format!("{name}.gg"));
    assert!(path.exists(), "Fixture not found: {}", path.display());
    path
}

fn cleanup(fixture: &PathBuf) {
    let stem = fixture.file_stem().unwrap().to_str().unwrap();
    let dir = fixture.parent().unwrap();
    let _ = std::fs::remove_file(dir.join(format!("{stem}.c")));
    let _ = std::fs::remove_file(dir.join(stem));
}

/// Result of a sanitize build + run. Records exactly what happened so the
/// known-unsafe helper can assert against it.
struct SanitizeOutcome {
    build_ok: bool,
    build_stderr: String,
    ran: bool,
    exit_code: Option<i32>,
    stdout: String,
    stderr: String,
}

/// Default run-time `ASAN_OPTIONS`. Leak detection is OFF here — most
/// security fixtures probe UAF/overflow/traps, not leaks, and LSan noise from
/// intentionally-abandoned allocations in trap-fixtures would be spurious.
const ASAN_OPTS_NO_LEAK: &str =
    "detect_leaks=0:halt_on_error=1:abort_on_error=0:print_summary=1:allocator_may_return_null=1";

/// Leak-detecting run-time `ASAN_OPTIONS`. `detect_leaks=1` makes
/// LeakSanitizer a hard failure and `exitcode=99` turns any ASan/LSan report
/// into a nonzero exit — so a leak is caught by BOTH the report scrape and the
/// exit code. Used by [`security_safe_no_leak`] to guard leak-class bugs that a
/// `detect_leaks=0` run (or a plain stdout check) cannot see.
const ASAN_OPTS_LEAK_CHECK: &str =
    "detect_leaks=1:halt_on_error=1:abort_on_error=0:print_summary=1:allocator_may_return_null=1:exitcode=99";

fn sanitize_build_and_run(fixture_name: &str) -> (SanitizeOutcome, PathBuf) {
    sanitize_build_and_run_with_opts(fixture_name, ASAN_OPTS_NO_LEAK)
}

/// Build the fixture with the Rust `gg` under `--sanitize`, then run it with
/// the caller-supplied `ASAN_OPTIONS`. Factored so the leak-checking guard
/// ([`security_safe_no_leak`]) shares the exact same build path but flips on
/// `detect_leaks=1` — the build MUST be the Rust compiler (the class of bug the
/// leak guard covers is a Rust-side lowering bug, not a self-host one).
fn sanitize_build_and_run_with_opts(
    fixture_name: &str,
    asan_options: &str,
) -> (SanitizeOutcome, PathBuf) {
    let fp = fixture_path(fixture_name);
    let stem = fp.file_stem().unwrap().to_str().unwrap();
    let dir = fp.parent().unwrap();
    let exe_path = dir.join(stem);

    let build = run_with_deadline(
        gg_command("build").arg("--sanitize").arg(&fp),
        fixture_name,
        build_timeout(),
    );
    let build_ok = build.status.success();
    let build_stderr = String::from_utf8_lossy(&build.stderr).into_owned();

    if !build_ok || !exe_path.exists() {
        return (
            SanitizeOutcome {
                build_ok,
                build_stderr,
                ran: false,
                exit_code: None,
                stdout: String::new(),
                stderr: String::new(),
            },
            fp,
        );
    }

    let mut run_cmd = Command::new(&exe_path);
    // Halt on first sanitizer error so a trip produces nonzero exit.
    // `allocator_may_return_null=1` lets the runtime's own null-check-after-
    // alloc paths run — otherwise ASan pre-aborts on oversized allocations
    // and masks Gorget's cleaner runtime trap.
    run_cmd.env("ASAN_OPTIONS", asan_options);
    run_cmd.env("UBSAN_OPTIONS", "halt_on_error=1:print_stacktrace=1");

    let run = run_with_deadline(&mut run_cmd, fixture_name, test_binary_timeout());
    (
        SanitizeOutcome {
            build_ok,
            build_stderr,
            ran: true,
            exit_code: run.status.code(),
            stdout: String::from_utf8_lossy(&run.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&run.stderr).into_owned(),
        },
        fp,
    )
}

/// A well-typed program that must build under `--sanitize` and run exit-0
/// with the expected stdout. Any deviation (build failure, sanitizer trip,
/// wrong output, nonzero exit) fails the test.
fn security_safe(name: &str, expected_stdout: &str) {
    let (out, fp) = sanitize_build_and_run(name);
    assert!(
        out.build_ok,
        "security_safe({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_safe({name}): binary did not run");
    assert_eq!(
        out.stdout.trim(),
        expected_stdout.trim(),
        "security_safe({name}): stdout mismatch\nExpected:\n{expected_stdout}\nGot:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    assert_eq!(
        out.exit_code,
        Some(0),
        "security_safe({name}): nonzero exit {:?}\nstderr:\n{}",
        out.exit_code,
        out.stderr
    );
    cleanup(&fp);
}

/// A [`security_safe`] fixture that a filed, cited **backend-specific compiler
/// defect** currently makes trip on ONE lane.
///
/// ⚠ THIS IS NOT A SKIP, AND IT MUST NEVER BECOME ONE. On every other backend
/// the fixture is held to the full `security_safe` contract. On the named
/// backend it is held to the INVERSE: the sanitizer trip must **still happen**.
/// So the moment the cited defect is fixed, this test goes RED and forces the
/// annotation to be removed — the same self-retiring contract
/// [`security_known_unsafe`] uses, and the reason a lane exemption here cannot
/// quietly outlive its cause. A plain skip would rot into a permanent waiver
/// and re-create, one lane down, exactly the vacuum this file's positive
/// control exists to prevent.
///
/// `item` is the `todo/` id, and it is not decoration: the defect must be
/// filed with a durable `known_gaps` repro asserting the INTENDED behaviour,
/// so what the language should do is pinned by an artifact rather than by this
/// comment.
///
/// `trip_marker` names the SPECIFIC sanitizer class expected — e.g.
/// `"memcpy-param-overlap"`. Asserting merely "nonzero exit" or "some sanitizer
/// output" would also be satisfied by an unrelated segfault, so the exemption
/// would silently widen to cover a defect nobody adjudicated. Name the class
/// the filed item describes, and nothing else.
#[expect(
    dead_code,
    reason = "No lane exemption is live. The last two — attack_64 and attack_70, \
              both citing todo/t0729 — were removed when the LLVM backend stopped \
              allocating a nested Option's inner `None()` temp with the OUTER \
              Option's type, which is what made the payload copy overrun by 8 \
              bytes. The CONTRACT this helper encodes is the reusable artifact, so \
              it stays. `expect` rather than `allow`: it fires \
              `unfulfilled_lint_expectation` the moment a caller returns, so the \
              annotation retires itself instead of rotting."
)]
fn security_safe_except_on(
    name: &str,
    expected_stdout: &str,
    backend: &str,
    item: &str,
    trip_marker: &str,
    reason: &str,
) {
    if gg_backend().as_deref() != Some(backend) {
        security_safe(name, expected_stdout);
        return;
    }
    let (out, fp) = sanitize_build_and_run(name);
    assert!(
        out.build_ok,
        "security_safe_except_on({name}) [{backend}]: the fixture must still BUILD — \
         {item} is a codegen defect, not a build failure.\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_safe_except_on({name}) [{backend}]: binary did not run");
    // Whatever ran before the trip must still be CORRECT. Under
    // `halt_on_error=1` the process aborts partway, so the full expected stdout
    // cannot be asserted — but what was printed must be a prefix of it. Without
    // this the exempted lane checks no output at all and a value regression
    // there would be invisible.
    assert!(
        expected_stdout.trim().starts_with(out.stdout.trim()),
        "security_safe_except_on({name}) [{backend}]: the output produced before the \
         sanitizer trip is not a prefix of the expected output, so this fixture has a \
         VALUE regression on top of {item}.\nexpected (full): {expected_stdout:?}\ngot: {}",
        out.stdout
    );
    let tripped = out.stderr.contains(trip_marker);
    assert!(
        tripped,
        "security_safe_except_on({name}) [{backend}]: this fixture is recorded as tripping \
         the sanitizer on this backend with `{trip_marker}` because `{reason}` ({item}), \
         and it NO LONGER DOES. \
         If {item} was fixed, that is good news: delete this annotation, restore the plain \
         `security_safe` call, and graduate the `known_gaps` repro {item} cites into a live \
         fixture. Do NOT leave the annotation in place — an exemption whose cause is gone is \
         a lane of coverage silently switched off.\nexit: {:?}\nstdout: {}\nstderr: {}",
        out.exit_code,
        out.stdout,
        out.stderr
    );
    cleanup(&fp);
}

/// Like [`security_safe`], but runs under `detect_leaks=1` so a memory LEAK is
/// a hard failure — not just a UAF/overflow/trap. Use for fixtures whose bug
/// class is stdout-INVISIBLE (a leaked heap buffer produces the correct output
/// yet leaks): a plain stdout check or a `detect_leaks=0` run passes it, so
/// only a LeakSanitizer-armed run guards it.
///
/// Each fixture MUST have been verified to FAIL (exit 99 / LSan report) at the
/// pre-fix baseline and PASS post-fix — else it guards nothing (Core #6).
fn security_safe_no_leak(name: &str, expected_stdout: &str) {
    let (out, fp) = sanitize_build_and_run_with_opts(name, ASAN_OPTS_LEAK_CHECK);
    assert!(
        out.build_ok,
        "security_safe_no_leak({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_safe_no_leak({name}): binary did not run");
    // A leak trips LeakSanitizer → `exitcode=99` + a report on stderr. Guard on
    // BOTH so neither a stray exit code nor a scrape miss lets a leak through.
    let has_leak_report = out.stderr.contains("LeakSanitizer")
        || out.stderr.contains("AddressSanitizer")
        || out.stderr.contains("detected memory leaks")
        || out.stderr.contains("ERROR:")
        || out.stderr.contains("SUMMARY:");
    assert!(
        !has_leak_report,
        "security_safe_no_leak({name}): sanitizer report (leak/UAF) under detect_leaks=1:\n{}",
        out.stderr
    );
    assert_eq!(
        out.exit_code,
        Some(0),
        "security_safe_no_leak({name}): nonzero exit {:?} under detect_leaks=1 \
         (a leak trips exitcode=99). If this regressed, the CoW bare-assign \
         owned-String path is re-leaking.\nstderr:\n{}",
        out.exit_code,
        out.stderr
    );
    assert_eq!(
        out.stdout.trim(),
        expected_stdout.trim(),
        "security_safe_no_leak({name}): stdout mismatch\nExpected:\n{expected_stdout}\nGot:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    cleanup(&fp);
}

/// Like [`security_safe`], but a UBSan diagnostic is a hard failure.
///
/// `--sanitize` already builds with `-fsanitize=address,undefined`, and UBSan
/// findings are RECOVERABLE by default: the program prints
/// `runtime error: ...` to stderr and CARRIES ON, exit 0, with correct stdout.
/// So [`security_safe`] — which checks stdout and the exit code and nothing
/// else — passes straight through undefined behaviour, and did: the
/// `qsort(NULL, 0, ...)` class (`t0780`, closed at R47 close — DONE.md) sat in
/// the corpus for months producing exactly the right answers.
///
/// Use this for a defect whose only observable is a UBSan line. Core #13: pick
/// an instrument that can SEE the failure class.
///
/// Each fixture MUST have been verified to emit the diagnostic at the pre-fix
/// baseline and to be silent post-fix — else it guards nothing (Core #12).
fn security_safe_no_ubsan(name: &str, expected_stdout: &str) {
    let (out, fp) = sanitize_build_and_run(name);
    assert!(
        out.build_ok,
        "security_safe_no_ubsan({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_safe_no_ubsan({name}): binary did not run");
    assert!(
        !out.stderr.contains("runtime error:"),
        "security_safe_no_ubsan({name}): UndefinedBehaviorSanitizer reported \
         undefined behaviour. stdout can be entirely correct and this still be \
         a real defect — \"nothing is read, so it is benign\" is not a defence \
         (Core #8).\nstderr:\n{}",
        out.stderr
    );
    assert_eq!(
        out.stdout.trim(),
        expected_stdout.trim(),
        "security_safe_no_ubsan({name}): stdout mismatch\nExpected:\n{expected_stdout}\nGot:\n{}\nstderr:\n{}",
        out.stdout,
        out.stderr
    );
    assert_eq!(
        out.exit_code,
        Some(0),
        "security_safe_no_ubsan({name}): nonzero exit {:?}\nstderr:\n{}",
        out.exit_code,
        out.stderr
    );
    cleanup(&fp);
}

/// A program that intentionally performs a runtime-defined trap (e.g.
/// division by zero, integer overflow). Must build and run, then exit
/// nonzero with the Gorget-level trap message in stderr — NOT raw C UB.
fn security_traps(name: &str, stderr_pattern: &str) {
    let (out, fp) = sanitize_build_and_run(name);
    assert!(
        out.build_ok,
        "security_traps({name}): sanitize build failed\nstderr: {}",
        out.build_stderr
    );
    assert!(out.ran, "security_traps({name}): binary did not run");
    assert_ne!(
        out.exit_code,
        Some(0),
        "security_traps({name}): expected nonzero exit (trap), got 0\nstdout:\n{}",
        out.stdout
    );
    let combined = format!("{}\n{}", out.stdout, out.stderr);
    assert!(
        combined.contains(stderr_pattern),
        "security_traps({name}): expected stderr to contain `{stderr_pattern}`, got:\nstdout:{}\nstderr:{}",
        out.stdout,
        out.stderr
    );
    cleanup(&fp);
}

/// A program the compiler must reject at `gg check` with the expected
/// stderr pattern.
fn security_rejected(name: &str, stderr_pattern: &str) {
    let fp = fixture_path(name);
    let output = run_with_deadline(gg_command("check").arg(&fp), name, build_timeout());
    assert!(
        !output.status.success(),
        "security_rejected({name}): expected `gg check` to fail, but it succeeded.\nstdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(stderr_pattern),
        "security_rejected({name}): expected stderr to contain `{stderr_pattern}`, got:\n{stderr}"
    );
}

/// Kinds of currently-present bugs we track with `security_known_unsafe`.
/// When a bug is fixed, the assertion for its kind will stop holding, the
/// test will fail, and someone must reclassify the fixture.
#[allow(dead_code)]
enum KnownBug {
    /// Compiler panics or emits C that fails to build.
    BuildFails,
    /// Binary builds under --sanitize, but the sanitizer trips at runtime.
    SanitizerTrips,
    /// Program silently produces this (wrong) output, with no trap. The most
    /// insidious class — use this for cases where the compiler "accepts" a
    /// bad program and runs it to completion.
    SilentlyProduces(&'static str),
}

/// A program that currently reveals a real bug. Asserts the bug is still
/// present. When the bug is fixed, this test will start failing — the
/// forcing function to move the fixture into `security_safe`/`_rejected`/
/// `_traps`.
fn security_known_unsafe(name: &str, bug: KnownBug, reason: &str) {
    let (out, _fp) = sanitize_build_and_run(name);
    match bug {
        KnownBug::BuildFails => {
            assert!(
                !out.build_ok,
                "security_known_unsafe({name}): expected BuildFails because `{reason}`, \
                 but build succeeded. If you fixed it, reclassify this fixture."
            );
        }
        KnownBug::SanitizerTrips => {
            assert!(
                out.build_ok,
                "security_known_unsafe({name}): expected SanitizerTrips because `{reason}`, \
                 but build failed. Did the codegen regress?\nstderr: {}",
                out.build_stderr
            );
            assert!(out.ran, "security_known_unsafe({name}): binary didn't run");
            assert_ne!(
                out.exit_code,
                Some(0),
                "security_known_unsafe({name}): expected sanitizer trip (nonzero exit) \
                 because `{reason}`, got exit 0. If you fixed it, reclassify this fixture.\n\
                 stdout: {}\nstderr: {}",
                out.stdout,
                out.stderr
            );
            let sanitizer_msg = out.stderr.contains("Sanitizer")
                || out.stderr.contains("runtime error")
                || out.stderr.contains("SEGV");
            assert!(
                sanitizer_msg,
                "security_known_unsafe({name}): expected sanitizer output in stderr \
                 because `{reason}`, got:\n{}",
                out.stderr
            );
        }
        KnownBug::SilentlyProduces(expected_stdout) => {
            assert!(
                out.build_ok,
                "security_known_unsafe({name}): expected SilentlyProduces because `{reason}`, \
                 but build failed.\nstderr: {}",
                out.build_stderr
            );
            assert!(out.ran, "security_known_unsafe({name}): binary didn't run");
            assert_eq!(
                out.exit_code,
                Some(0),
                "security_known_unsafe({name}): expected silent exit 0 because `{reason}`, \
                 got exit {:?}. Did it finally trap?\nstderr: {}",
                out.exit_code,
                out.stderr
            );
            assert_eq!(
                out.stdout.trim(),
                expected_stdout.trim(),
                "security_known_unsafe({name}): stdout changed. If you fixed the bug, \
                 reclassify this fixture. Reason: `{reason}`"
            );
        }
    }
}

// ════════════════════════════════════════════════════════════════════════════
// POSITIVE CONTROL FOR THE SANITIZER ITSELF
// ════════════════════════════════════════════════════════════════════════════

/// Does `gg build --sanitize` actually produce an instrumented binary — on
/// EVERY backend?
///
/// Everything else in this file assumes the answer is yes and then asserts
/// something about the compiler. This test asserts nothing about the compiler;
/// it checks the INSTRUMENT, so that the ~200 assertions below mean what they
/// say. A sanitizer gate that has never been seen to go red on the lane it
/// claims to cover is not evidence, and this one had not: `--sanitize` was a
/// silent no-op under `--backend=llvm` for an unknown period (t0723), so every
/// "ASan-clean on LLVM" claim in this project's history was free.
///
/// ── WHY TWO CELLS ──
///
/// The two halves of a sanitized build fail independently, and the obvious
/// control only sees one of them:
///
/// | cell | what it proves | probe |
/// |---|---|---|
/// | 1. LINKING | the ASan runtime is in the process | a leak is reported |
/// | 2. INSTRUMENTATION | the compiler emitted shadow checks | `__asan_report_*` is referenced by the artifact |
///
/// Cell 1 alone is not enough, and this is measured, not assumed:
/// LeakSanitizer is INTERCEPTOR-based, so it works on any binary that merely
/// LINKS libasan, whether or not a single line was instrumented. Building with
/// the sanitize flags on the link command and NOT on the runtime compile
/// (i.e. reverting half the t0723 fix) leaves cell 1 fully green — verified:
/// the leak was still reported, `ldd` still listed `libasan.so.8` — while
/// every shadow-memory check silently disappeared. Cell 2 catches exactly that
/// revert: `__asan_report_*` references went 10 -> 0.
///
/// So cell 2's probe is deliberately NOT `ldd`, and NOT the presence of
/// `__asan_*` symbols generally — both of those are satisfied by linking.
/// `__asan_report_load8` and friends are the failure calls that INSTRUMENTED
/// code emits inline; nothing but instrumentation puts them in the artifact.
/// The probe is a raw byte scan for the name rather than an `nm` subprocess,
/// which keeps it toolchain-free and identical on ELF and Mach-O.
///
/// ── WHY IT LOOPS THE BACKENDS INSTEAD OF READING `GG_BACKEND` ──
///
/// A control that only covers the lane it happens to be run on cannot tell you
/// the other lane went vacuous. Both backends are built here on every run, so
/// this test fails on a `cargo test --test security` with no environment set
/// at all. That is the point: the LLVM lane's vacuity survived for as long as
/// it did precisely because observing it required opting in.
///
/// The fixture leaks by construction and can never be fixed into silence —
/// see the header of `sanitizer_positive_control_leak.gg`.
#[test]
fn sanitizer_gate_is_real_on_both_backends() {
    let fp = fixture_path("sanitizer_positive_control_leak");
    let src = std::fs::read_to_string(&fp).expect("control fixture unreadable");

    for backend in ["c-lir", "llvm"] {
        // Private per-backend directory: the two builds share a stem, and the
        // suite is not always run single-threaded.
        let dir = std::env::temp_dir().join(format!(
            "gg_sanitizer_control_{backend}_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let gg_path = dir.join("sanitizer_positive_control_leak.gg");
        std::fs::write(&gg_path, &src).expect("failed to stage control fixture");
        let exe_path = dir.join("sanitizer_positive_control_leak");

        let build = run_with_deadline(
            Command::new(env!("CARGO"))
                .args(["run", "--quiet", "--", "build", "--sanitize"])
                .arg(format!("--backend={backend}"))
                .arg(&gg_path),
            "sanitizer_positive_control_leak",
            build_timeout(),
        );
        assert!(
            build.status.success() && exe_path.exists(),
            "positive control [{backend}]: `gg build --sanitize --backend={backend}` failed.\n\
             stderr:\n{}",
            String::from_utf8_lossy(&build.stderr)
        );

        // ── CELL 2: INSTRUMENTATION ──
        // Read first: if this fails the binary is not worth running, and the
        // diagnosis ("linked but not instrumented") is the one a reader is
        // least likely to reach on their own.
        let bytes = std::fs::read(&exe_path).expect("control binary unreadable");
        let instrumented = bytes
            .windows(b"__asan_report".len())
            .any(|w| w == b"__asan_report");
        assert!(
            instrumented,
            "positive control [{backend}]: the binary references no `__asan_report_*`, \
             so NO CODE IN IT IS INSTRUMENTED — the sanitizer flags reached the link \
             step but not the compile step, or not at all. Note the leak check below \
             would still PASS in this state (LeakSanitizer is interceptor-based), \
             which is why this cell exists. Look at `add_sanitize_flags` in \
             `src/main.rs` and at every command it is called from."
        );

        // ── CELL 1: LINKING ──
        let mut run_cmd = Command::new(&exe_path);
        run_cmd.env("ASAN_OPTIONS", ASAN_OPTS_LEAK_CHECK);
        run_cmd.env("UBSAN_OPTIONS", "halt_on_error=1:print_stacktrace=1");
        let run = run_with_deadline(
            &mut run_cmd,
            "sanitizer_positive_control_leak",
            test_binary_timeout(),
        );
        let stderr = String::from_utf8_lossy(&run.stderr);
        assert!(
            stderr.contains("LeakSanitizer: detected memory leaks"),
            "positive control [{backend}]: a 64-byte `malloc` that is never freed was NOT \
             reported by LeakSanitizer. `--sanitize` is not reaching the link step on this \
             backend — the binary looks sanitized and is not, so every sanitizer assertion \
             in this file is vacuous on this lane.\nexit: {:?}\nstdout: {}\nstderr: {stderr}",
            run.status.code(),
            String::from_utf8_lossy(&run.stdout)
        );
        assert_ne!(
            run.status.code(),
            Some(0),
            "positive control [{backend}]: LeakSanitizer reported a leak but the process still \
             exited 0, so a sanitizer trip would not fail a test that checks the exit code. \
             Is `exitcode=99` still in ASAN_OPTS_LEAK_CHECK?\nstderr: {stderr}"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }
}

// ════════════════════════════════════════════════════════════════════════════
// Classification of the 29 adversarial fixtures (audit 2026-04-23)
// ════════════════════════════════════════════════════════════════════════════

// ── Compiler correctly rejects (the borrow checker's happy path) ──

#[test]
fn sec_01_multi_move_string() {
    security_rejected("attack_01_multi_move_string", "move");
}

#[test]
fn sec_07_return_borrow_of_local() {
    security_rejected("attack_07_return_borrow_of_local", "");
}

#[test]
fn sec_09_match_binding_escape() {
    security_rejected("attack_09_match_binding_escape", "");
}

#[test]
fn sec_16_closure_captures_string() {
    security_rejected("attack_16_closure_captures_string", "");
}

#[test]
fn sec_19_field_borrow_escape() {
    security_rejected("attack_19_field_borrow_escape", "");
}

#[test]
fn sec_93_arena_borrow_escape_push() {
    // N2 sibling of sec_19: the SAME arena borrow-escape UAF at a
    // collection-MUTATION consume position (`outer.push(arenaVec.get(0).unwrap())`).
    // RUN-confirmed UAF under ASan before the check existed.
    security_rejected("attack_93_arena_borrow_escape_push", "cannot assign arena-scoped value");
}

#[test]
fn sec_94_arena_borrow_escape_channel_send() {
    // N2 subset (a): Channel `send` ingests an arena-borrowed non-Copy element
    // into the channel's buffer; the channel OUTLIVES the arena, so the element
    // dangles at `gorget_arena_destroy`. RUN-confirmed UAF under ASan before the
    // typed `owns_buffered_elements` gate. Channel `collection_kind: None` —
    // gated on the typed flag, not a name-match of `send`.
    security_rejected("attack_94_arena_borrow_escape_channel_send", "cannot assign arena-scoped value");
}

#[test]
fn sec_95_arena_borrow_escape_heap_push() {
    // N2 subset (a) sibling: Heap `push` is the SAME element-owning-sink class as
    // Channel `send` (also `owns_buffered_elements: true`, also
    // `collection_kind: None`). RUN-confirmed UAF under ASan. Proves the gate is
    // the CLASS, not a Channel-only instance ("fix the class, not the instance").
    security_rejected("attack_95_arena_borrow_escape_heap_push", "cannot assign arena-scoped value");
}

#[test]
fn sec_96_arena_borrow_escape_ctor_arg() {
    // N2 subset (b): constructor/wrapper arg (`outer = Some(arenaVec.get(0).unwrap())`)
    // assigned to an outer binding. The ctor copies the arena-borrowed element
    // into the built value under the in-scope arena allocator, so it dangles at
    // arena destruction. RUN-confirmed UAF under ASan. Also covers positional
    // struct constructors (desugared to `Expr::StructLiteral`).
    security_rejected("attack_96_arena_borrow_escape_ctor_arg", "cannot assign arena-scoped value");
}

#[test]
fn sec_98_arena_borrow_escape_ctor_bareid() {
    // N2 subset (b) sibling: the ctor arg is a BARE arena-scoped non-Copy
    // identifier (`Some(arenaStr)` / `!arenaStr`), not a `.get()` borrow-read —
    // the same UAF class, now caught by the one producer `arena_backed_source`
    // (the arena-scoped-identifier arm). RUN-confirmed UAF under ASan.
    security_rejected("attack_98_arena_borrow_escape_ctor_bareid", "cannot assign arena-scoped value");
}

// ── Accepted well-typed programs that must run safely under --sanitize ──

#[test]
fn sec_97_arena_channel_send_inner_ok() {
    // Negative control for sec_94: the arena-escape gate must NOT over-fire when
    // the buffer-owning handle does NOT outlive the arena (Channel declared
    // INSIDE the `with`). Must build, run clean under --sanitize, and print the
    // payload. Guards against a false positive on a Copy-typed handle that the
    // arena-scoped-binding tracker must still recognize via is_buffer_owning_type.
    security_safe("attack_97_arena_channel_send_inner_ok", "payload");
}

#[test]
fn sec_03_cow_mutate_while_borrowed() {
    security_safe("attack_03_cow_mutate_while_borrowed", "alpha\ndelta");
}

/// D10(a) (decisions.md, ratified 2026-07-06): the fixture's `int &borrowed =
/// ...` decl-sigil bind is now a parse-time rejection — which matches the
/// fixture's ORIGINAL intent ("must reject"). Pre-D10 the parser silently
/// discarded the sigil, `borrowed` was a value copy, and the fixture was
/// (mis)classified safe with output "10".
#[test]
fn sec_04_cow_mutate_ref_borrow() {
    security_rejected(
        "attack_04_cow_mutate_ref_borrow",
        "local `&`-bindings are not supported",
    );
}

#[test]
fn sec_05_nested_vector_drop() {
    security_safe("attack_05_nested_vector_drop", "3\n99");
}

#[test]
fn sec_06_inline_none() {
    security_safe("attack_06_inline_none", "42\n-2\nfalse\n42");
}

#[test]
fn sec_08_closure_captures_local_ref() {
    security_safe("attack_08_closure_captures_local_ref", "123");
}

#[test]
fn sec_10_spawn_stale_shared() {
    security_safe("attack_10_spawn_stale_shared", "20000");
}

#[test]
fn sec_11_uninit_return() {
    security_safe("attack_11_uninit_return", "10\n7\n0\n1\n-1\n0");
}

#[test]
fn sec_13_vector_grow_pointer_invalidation() {
    security_safe("attack_13_vector_grow_pointer_invalidation", "alpha\nalpha");
}

#[test]
fn sec_14_dict_borrow_mutate() {
    security_safe("attack_14_dict_borrow_mutate", "v1");
}

#[test]
fn sec_15_channel_close_race() {
    security_safe("attack_15_channel_close_race", "0");
}

#[test]
fn sec_18_self_referential_mutation() {
    security_safe("attack_18_self_referential_mutation", "third\nthird");
}

#[test]
fn sec_21_vector_negative_index() {
    security_safe("attack_21_vector_negative_index", "-1\n-2\n30");
}

#[test]
fn sec_24_move_in_loop_iteration() {
    security_safe("attack_24_move_in_loop_iteration", "one\ntwo\nthree");
}

#[test]
fn sec_29_unwrap_pattern() {
    security_safe("attack_29_unwrap_pattern", "alpha");
}

#[test]
fn sec_92_static_collection_reassign_uaf() {
    // Conformance Bug 2. Storing an owned local collection into a `static`
    // (`CACHE = d`) is a consuming position; before the fix it shallow-aliased
    // the local's heap buffer, which the local's scope-exit drop freed → a
    // heap-use-after-free on the next read of the static (silent in release:
    // garbage value, exit 0). Fixed in src/ir/lowering/stmts/assigns.rs — the
    // store-to-static path now clones-or-moves the RHS, MoveZeros the moved
    // source, and drops the static's prior value. Covers Dict/Vector/Set.
    security_safe(
        "attack_92_static_collection_reassign_uaf",
        "42\n42\n10\n20\ntrue\ntrue\nfalse",
    );
}

// D39 Phase A.3 R6 axis: interior-pointer UAF fixtures on the dense-index-map
// layout. Decision-tree BRANCH 1: ASan CLEAN under `--sanitize
// detect_leaks=1`. The fixtures ship as POSITIVE CONTROLS verifying the
// transient-view discipline — the `.clone()` before a mutating call
// materialises before swap_remove/remove runs the swap-out or shift, so the
// interior pointer is dropped and no UAF can occur. The runtime's swap-out
// (last-entry-into-hole memcpy + swapped indices rewrite) and the order-
// preserving remove's O(n) memmove both stay memory-safe under this
// discipline. The safety pass's `is_mutating_builtin_method` covers
// swap_remove (populated from the `is_mutating: true` flag on the SET/DICT
// BuiltinMethodDecls added same-round in src/ir/lowering/builtins.rs).
#[test]
fn sec_r6_dict_dense_swap_remove_interior_ptr() {
    security_safe(
        "dict_dense_swap_remove_interior_ptr_uaf",
        "v1_padding_padding_padding_padding_padding\n\
         v2_padding_padding_padding_padding_padding",
    );
}

#[test]
fn sec_r6_dict_dense_remove_interior_ptr() {
    security_safe(
        "dict_dense_remove_interior_ptr_uaf",
        "v1_padding_padding_padding_padding_padding\n\
         v2_padding_padding_padding_padding_padding",
    );
}

#[test]
fn sec_r6_set_dense_swap_remove_interior_ptr() {
    security_safe(
        "set_dense_swap_remove_interior_ptr_uaf",
        "present=1\nabsent=0\nlen=3",
    );
}

#[test]
fn sec_r6_dict_dense_transient_view_positive_control() {
    security_safe(
        "dict_dense_transient_view_positive_control",
        "v1_padding_padding_padding_padding\n\
         v1_padding_padding_padding_padding\n\
         len=1",
    );
}

// ── Runtime traps that must use the Gorget panic path, not C UB ──

#[test]
fn sec_25_div_zero_direct() {
    security_traps("attack_25_div_zero_direct", "division by zero");
}

#[test]
fn sec_27_signed_overflow() {
    security_traps("attack_27_signed_overflow", "overflow");
}

// ── Known-present bugs. Fixing any of these makes its test fail. ──

#[test]
fn sec_02_box_new_consuming_position() {
    // Was BuildFails. Fixed 2026-04-23 in src/ir/lowering/exprs/methods.rs —
    // Box.new now follows the push/set consuming-position rule (clone
    // multi-use, MoveZero only on last-use).
    security_safe("attack_02_box_new_double_move", "5");
}

#[test]
fn sec_17_vector_borrow_invalidate() {
    // Was SanitizerTrips. Fixed 2026-04-23 in src/ir/lowering/stmts/mod.rs —
    // VarDecl now lifts Option[Ref[T]] → Option[T] via tag-branch + clone
    // instead of emitting a wrong-sized memcpy. `v.push(...)` materializes
    // the snapshot via CoW, so `s` keeps v[0]'s value as it was at get-time.
    security_safe("attack_17_vector_borrow_invalidate", "aaa");
}

#[test]
fn sec_20_shift_out_of_range() {
    security_traps(
        "attack_20_div_by_zero",
        "shift out of range",
    );
}

#[test]
fn sec_22_weak_after_shared_dropped() {
    // Fixed 2026-04-23. Two things: (1) the original `Shared.new(v)` surface
    // syntax isn't supported in Gorget — rewrote the fixture to the idiomatic
    // `Shared[T](v)` constructor. That alone turned the link error into a
    // heap-use-after-free. (2) Root fix in src/ir/lowering/exprs/calls.rs —
    // `!x` on a refcounted type (Shared/Weak/Channel) now MoveZeros the
    // caller's slot after the call, matching the push/enum-init consuming-
    // position rule. Without (2), `drop_all(!s)` left `s` live in the caller
    // and its scope-exit drop double-freed the control block.
    security_safe("attack_22_weak_after_shared_dropped", "-1");
}

#[test]
fn sec_23_dead_div_zero_now_traps() {
    // Was SilentlyProduces("10"). Fixed 2026-04-23 in src/lir/optimize.rs —
    // integer Div/Rem/Mod and all shifts are now treated as side-effecting
    // by the LIR DCE pass, so the trap fires even when the result is dead.
    security_traps("attack_23_panic_during_init", "division by zero");
}

#[test]
fn sec_26_mod_zero_traps() {
    security_traps("attack_26_mod_zero", "division by zero");
}

#[test]
fn sec_12_vector_iter_resource() {
    // Was BuildFails. Fixed 2026-04-23 in src/lir/lower/operands.rs —
    // `resolve_field_type` and `resolve_place_type` now consult the LIR
    // struct registry for enums (flat-layout Option/Result), so Field(1)
    // on Option__Ref_T resolves as LIR::Ptr instead of falling back to I64.
    // Eliminates the Ptr-ABI debug_assert trip and the cosmetic `*(int64_t*)`
    // cast in generated C.
    security_safe("attack_12_vector_iter_resource", "a\nb\nc");
}

// ── Regression fixtures for closed findings — keep these passing. ──

#[test]
fn sec_30_option_ref_match_arm() {
    // Parallel to sec_28 (VarDecl) — this one exercises `case Some(s):` arm
    // destructuring of Option[Ref[T]]. Pre-fix: memcpy of 40 bytes from a
    // 16-byte source via &Some_0 treated as Str*. Fixed by Option[Ref[T]]
    // pattern extraction now Loading the Ref value instead of taking its
    // address (src/lir/lower/insts.rs EnumFieldLoad handler).
    security_safe("attack_30_option_ref_match_arm", "beta");
}

#[test]
fn sec_31_option_ref_if_is() {
    // Parallel to sec_30 — `if ... is Some(s):` sugar.
    security_safe("attack_31_option_ref_if_is", "alpha");
}

#[test]
fn sec_32_option_ref_resource_struct() {
    // Parallel to sec_30 — Option[Ref[S]] where S is a user struct with a
    // resource field. Exercises the Ref→struct extraction path.
    security_safe("attack_32_option_ref_resource_struct", "4");
}

#[test]
fn sec_33_integer_shift_all_counts() {
    // Regression for the shift guards: `1 << 63` must produce INT64_MIN
    // (defined via unsigned intermediate), `-8 >> 2` must produce -2
    // (arithmetic right shift), and `1 << 64` must trap with the Gorget
    // message. The program traps on the third line.
    security_traps("attack_33_integer_shift_all_counts", "shift out of range");
}

#[test]
fn sec_34_div_zero_in_expression() {
    // Regression for the DCE side-effect fix: `(x / 0) * 0 + x` has a dead
    // division (multiplied by zero then replaced by x), and DCE must NOT
    // eliminate it.
    security_traps("attack_34_div_zero_in_expression", "division by zero");
}

#[test]
fn sec_35_shared_move_chain_trivial_payload() {
    // Fixed 2026-04-23. Previously thought to be a refcount-metadata bug; the
    // real cause was that `Weak.upgrade()` registered `Option[Shared[T]]`
    // as a Named TypeId without its enum TypeDef, so match dispatch on
    // `w.upgrade()` fell through to const_bool(true) and always fired the
    // first arm. Fix at src/ir/lowering/exprs/methods.rs upgrade handler.
    security_safe("attack_35_shared_move_chain", "0");
}

#[test]
fn sec_36_match_weak_upgrade_rvalue() {
    // Regression — rvalue match on w.upgrade() must dispatch correctly.
    security_safe("attack_36_match_weak_upgrade_rvalue", "dead");
}

#[test]
fn sec_37_upgrade_still_alive() {
    // Complementary to sec_36 — Some arm must fire when Shared is alive.
    security_safe("attack_37_upgrade_still_alive", "alive");
}

// ── Round 3 attacks: broader soundness probes ────────────────────────────

#[test]
fn sec_38_fstring_no_format_injection() {
    // User-controlled %s etc. in interpolated values must be treated as
    // opaque data, not format directives. f-string lowers to
    // gorget_string_format(template, %.*s, user_data).
    security_safe(
        "attack_38_fstring_format_injection",
        "hello: %s %s %s %s %s %n",
    );
}

#[test]
fn sec_39_closure_captures_resource() {
    // Regression — closures that capture an owned resource-typed local at
    // last-use must MOVE the source into the env (MoveZero after struct init),
    // not leave a shallow alias that the source's scope-exit drop frees.
    // Fix in src/ir/lowering/closures.rs.
    security_safe("attack_39_closure_captures_resource", "3\n10");
}

#[test]
fn sec_40_integer_narrowing() {
    // `x as int8` truncates silently (300 → 44, 9999999999 → 1410065407).
    // Document the current behavior; a `checked_as` would be an upgrade.
    security_safe("attack_40_integer_narrowing", "44\n1410065407");
}

#[test]
fn sec_41_panic_during_init() {
    // Division-by-zero mid-execution traps cleanly; the partially-built
    // Vector is handled by process exit(1). No UAF under ASan.
    security_traps("attack_41_panic_during_drop", "division by zero");
}

#[test]
fn sec_42_vector_subscript_oob() {
    security_traps("attack_42_vector_subscript_oob", "index out of bounds");
}

#[test]
fn sec_43_string_slice_oob() {
    security_traps("attack_43_string_slice_negative", "byte_slice out of bounds");
}

#[test]
fn sec_45_deep_recursion_stack_overflow() {
    // Accepted as-is (2026-04-24, matches Rust / C++ / Zig): unbounded
    // recursion hits the OS guard page and SIGSEGVs. ASan surfaces this as
    // `stack-overflow`. A per-call depth counter would trap cleanly at
    // ~1-2 ns/call overhead; for now we prefer Gorget's "thin C ABI, no
    // hidden per-call cost" promise. Keeping the fixture as known_unsafe
    // documents the behavior so any future stack-guard work flips it.
    security_known_unsafe(
        "attack_45_deep_recursion",
        KnownBug::SanitizerTrips,
        "By design (matches Rust / C++ / Zig): unbounded recursion hits the \
         OS guard page → SIGSEGV. ASan surfaces `stack-overflow`. Closing \
         this means adding a per-call depth counter, which isn't free.",
    );
}

#[test]
fn sec_46_nan_equality_ieee754() {
    // Regression — simplify_cmp used to fold `x == x` as `true` even on
    // floats, which breaks IEEE-754 (NaN != NaN). Fix in
    // src/ir/transforms/optimize.rs:simplify_cmp — skip the reflexive
    // identity when type is F32/F64.
    security_safe("attack_46_float_nan", "false\ntrue\nfalse\nfalse");
}

#[test]
fn sec_47_dict_overwrite_drops_old() {
    // Dict.put on an existing key must drop the old value before storing
    // the new one. Exercises elem_drop on the overwritten cell.
    security_safe("attack_47_dict_overwrite", "1");
}

#[test]
fn sec_48_vector_remove_while_iterating() {
    // v.remove(i) inside a `while i < v.len()` loop — the classic
    // iterator-invalidation shape. Gorget's CoW system must either clone
    // the view or reflow through the mutated collection. No UAF expected.
    security_safe(
        "attack_48_vector_remove_while_iterating",
        "4\n1\n2\n4\n5",
    );
}

#[test]
fn sec_44_match_partial_option() {
    // Fixed 2026-04-24 at src/ir/lowering/exprs/mod.rs — inline `None()` in
    // a call argument now constructs Option[T]::None from expected_type
    // context instead of emitting a raw NULL constant.
    security_safe("attack_44_match_partial_option", "42\n-1");
}

#[test]
fn sec_28_cow_option_extraction_minimal() {
    // Was SanitizerTrips. Fixed 2026-04-23 together with sec_17 — the
    // minimal 4-line repro of Option[Ref[T]] → Option[T] now builds and
    // runs cleanly, printing the actual element.
    security_safe("attack_28_cow_option_extraction", "alpha");
}

// ── Round 4 attacks: arithmetic edges, collections, generics ─────────────

#[test]
fn sec_49_nested_closure() {
    // Closure capturing an enclosing function's scalar locals. Safe.
    security_safe("attack_49_nested_closure", "105");
}

#[test]
fn sec_50_int_min_div_neg_one() {
    // Regression — INT64_MIN / -1 is C UB (the result isn't representable).
    // Pre-fix: wrapping_div silently folded to INT_MIN, bypassing the runtime
    // guard. Post-fix: constant folder uses checked_div (returns None on
    // overflow so the runtime guard handles it), and the C backend's Div
    // instruction adds an explicit INT_MIN/-1 trap.
    security_traps("attack_50_int_min_div_neg_one", "integer overflow");
}

#[test]
fn sec_51_null_byte_string_equality() {
    // Strings are length-prefixed; embedded \0 doesn't terminate. Equality
    // returns correct result, len reports full byte count.
    security_safe("attack_51_null_byte_in_string", "false\n5\n5");
}

#[test]
fn sec_52_channel_send_after_close() {
    security_traps("attack_52_channel_send_after_close", "closed channel");
}

#[test]
fn sec_53_empty_collection_ops() {
    security_safe(
        "attack_53_empty_collection_ops",
        "empty-get\nempty-pop\nmissing-key",
    );
}

#[test]
fn sec_54_parse_int_overflow() {
    security_safe("attack_54_parse_int_overflow", "42\nerr-2\nerr-3");
}

#[test]
fn sec_55_dict_float_nan_key() {
    // Gorget's Dict uses bitwise equality for float keys — NaN stored with
    // `zero / zero` can be retrieved by recomputing `zero / zero` because
    // both have the same bit pattern. Semantic choice, not a memory bug.
    security_safe("attack_55_dict_float_nan_key", "real\nunreachable");
}

#[test]
fn sec_56_mutex_double_lock() {
    // Mutex.lock() on a Mutex that already has a live Guard in the
    // current scope is a deadlock waiting to happen — non-reentrant.
    // The borrow checker rejects it at compile time so it never reaches
    // the runtime.
    security_rejected("attack_56_mutex_double_lock", "already locked");
}

#[test]
fn sec_57_box_cycle_via_shared() {
    security_safe("attack_57_box_cycle_via_shared", "done");
}

#[test]
fn sec_58_string_concat_loop() {
    security_safe("attack_58_string_concat_loop", "100000");
}

#[test]
fn sec_59_generic_resource_monomorphization() {
    security_safe("attack_59_generic_resource_collision", "10\nhello");
}

// ── Round 5 attacks: sort, UTF-8, allocator edges, nested types ──────────

#[test]
fn sec_60_sort_non_transitive_comparator() {
    security_safe("attack_60_sort_non_transitive", "11");
}

#[test]
fn sec_61_dict_mutate_during_iter() {
    // `.keys()` materializes — iteration is over a snapshot. No UAF even
    // if the Dict is rehashed mid-loop.
    security_safe("attack_61_dict_mutate_during_iter", "3");
}

#[test]
fn sec_62_utf8_invalid_bytes() {
    security_safe("attack_62_utf8_invalid_bytes", "invalid");
}

#[test]
fn sec_63_huge_vector_capacity_traps() {
    // Regression — `v.reserve(huge)` used to silently fail (realloc → NULL),
    // leaving the vector with cap > 0 and data = NULL; the next push
    // segfaulted. Fix in src/backend/c/c_runtime.rs: gorget_array_reserve /
    // ensure_capacity / with_capacity now check __builtin_mul_overflow on
    // the size computation AND null-check the allocator's return, trapping
    // with `gorget: panic: allocation failed` or `array capacity overflow`.
    security_traps("attack_63_huge_vector_capacity", "allocation failed");
}

#[test]
fn sec_64_deep_option_unwrap() {
    security_safe("attack_64_deep_option_unwrap", "42\ninner-none");
}

#[test]
fn sec_65_tuple_resource_destructure() {
    security_safe("attack_65_tuple_resource_destructure", "hello\n3");
}

#[test]
fn sec_66_chained_field_mutation() {
    security_safe("attack_66_chained_field_mutation", "100");
}

#[test]
fn sec_67_tuple_return_auto_destructure() {
    security_safe("attack_67_pattern_guard_side_effects", "big\n1");
}

#[test]
fn sec_68_range_edge_cases() {
    security_safe("attack_68_range_edge_cases", "0\n0\n5");
}

#[test]
fn sec_69_negative_range() {
    security_safe("attack_69_negative_range", "-3");
}

#[test]
fn sec_70_inline_none_nested() {
    // Regression — cover several inline-None shapes so the fix doesn't
    // regress only at the top level. `level_1(None())`, `level_2(None())`,
    // `level_2(Some(None()))` all used to emit NULL-ptr-deref; now each
    // constructs the correct Option[T]::None struct from expected_type.
    security_safe("attack_70_inline_none_nested", "-1\n42\n-2\n-1\n7");
}

// ── Round 6 attacks: float casts, async panics, struct ABI, enum dispatch ─

#[test]
fn sec_71_float_to_int_saturation() {
    // Matches Rust `as i64` semantics (defined since 1.45): NaN → 0,
    // +Inf / huge → INT64_MAX. Not C UB on this platform; our cast
    // sequence is deterministic.
    security_safe(
        "attack_71_float_to_int_edge",
        "0\n9223372036854775807\n9223372036854775807\n3",
    );
}

#[test]
fn sec_72_spawn_task_panic_propagates() {
    // A panic inside a spawned task exits the whole process with a
    // clean Gorget-level message — the main task doesn't silently
    // survive while the worker dies.
    security_traps("attack_72_spawn_task_panic", "division by zero");
}

#[test]
fn sec_73_moved_self_reuse_rejected() {
    // Compiler correctly rejects `bag` use after `consume(!bag)`.
    security_rejected("attack_73_moved_self_reuse", "use of moved value");
}

#[test]
fn sec_74_hashmap_1k_keys() {
    // 1000 inserts + lookup — baseline for HashMap at moderate scale.
    // A pathological collision test would need attacker-crafted keys
    // that defeat the hash function; without knowing the hash, this
    // just confirms correctness at scale.
    security_safe("attack_74_hashmap_collision_dos", "1000\n500");
}

#[test]
fn sec_75_enum_variant_dispatch() {
    security_safe("attack_75_enum_256_variants", "0\n9\n5");
}

#[test]
fn sec_76_big_struct_return() {
    // Returning a 10-field struct by value — exercises the sret ABI.
    security_safe("attack_76_big_struct_return", "1\n10\n55");
}

#[test]
fn sec_77_dict_remove_during_iter() {
    // Collect keys first, then remove in a second pass. Safe pattern.
    security_safe("attack_77_dict_remove_during_iter", "3");
}

#[test]
fn sec_78_deep_box_drop() {
    // 1000-node Box chain. Each node's Drop recurses into next's Drop.
    // Passes — C stack handles 1000 fine. A 1M-node chain would SIGSEGV
    // via the same path as attack_45.
    security_safe("attack_78_deep_box_drop", "1000");
}

#[test]
fn sec_79_weak_clone_sanity() {
    security_safe("attack_79_weak_clone_sanity", "1\ndone");
}

#[test]
fn sec_80_char_construction_edges() {
    // `chr(0)` → 1-byte NUL String (len 1 codepoint). Gorget String is
    // length-prefixed and carries interior NULs (NOT NUL-terminated); the
    // empty-String behavior was deliberately removed in commit 11e3abb0
    // (gorget-js snag #6, codepoint_to_str(0)). `chr(1114111)` → U+10FFFF,
    // 1 codepoint (4 UTF-8 bytes). `chr(1200000)` (invalid) → U+FFFD
    // replacement, 1 codepoint. All deterministic; no invalid UTF-8 is produced.
    security_safe("attack_80_char_edge_cases", "1\n1\n1");
}

// ── Round 7 attacks: self-ref, closures-in-vector, atomic races, etc. ────

#[test]
fn sec_81_match_expr_some_arm_dropped() {
    // Fixed 2026-04-30. `lower_match_expr` was always switching to the
    // last arm's `next_test_bb` (which equals `merge_bb` when there's
    // no else), then writing `Constant::Unit` into the merge block —
    // overwriting whichever arm value the arms had just stored. The
    // sibling `lower_match_stmt_as_expr` already had the right shape:
    // gate the `switch_to` on `next_test_bb != merge_bb`, and only
    // emit a fallback assignment when there's an actual else arm.
    security_safe("attack_81_self_referential_struct", "6");
}

#[test]
fn sec_82_vector_of_closures() {
    // Fixed 2026-04-28. Three independent bugs all had to land for
    // `Vector[Callable]` to work:
    //  1. `infer_collection_element_type` resolves `Vector__Callable__…`
    //     to `FnPtr` so `gorget_array_new` is created with elem_size = 16
    //     (sizeof(GorgetClosure)) instead of 8.
    //  2. The LIR `Callable__…` Named type maps to `GorgetClosure`
    //     (16-byte struct), so `LoadRef` through the borrow returns the
    //     full closure value, not just the first 8 bytes.
    //  3. The CallExtern dispatch (used for `Vector__Callable__push`)
    //     runs `wrap_closure_call_args`, packing each closure literal
    //     into a `GorgetClosure` with a heap-alloc'd env before the
    //     runtime memcpys 16 bytes into the array slot.
    // Without (1) the array slot is too small; without (2) `.clone()`
    // on `Ref[Callable]` reinterprets fn_ptr as a memory address;
    // without (3) the slot's env is uninitialized after push.
    security_safe(
        "attack_82_vector_of_closures",
        "11\n102\n1000",
    );
}

#[test]
fn sec_83_parse_format_roundtrip() {
    security_safe(
        "attack_83_parse_format_roundtrip",
        "-9223372036854775808\n0.100000\n-0.000000",
    );
}

#[test]
fn sec_84_negative_subscript_traps() {
    // `v.get(-1)` → None (safe Option path). `v[-1]` traps via unsigned
    // bounds check (index UINT64_MAX > length). Error message prints
    // the unsigned value, which is misleading but not wrong.
    security_traps(
        "attack_84_negative_subscript",
        "index out of bounds",
    );
}

#[test]
fn sec_85_dict_struct_key_codegen() {
    // `@derive(Hashable, Equatable) struct Point` as a Dict key — the
    // key-equality wrapper now derefs and passes-by-value to match
    // `Point__eq(const void*, Point)`'s ABI, lookups by structurally-
    // equal keys hit, and the runtime drop-callback dispatch through
    // `__gorget_drop_fn` is well-typed under UBSan.
    security_safe(
        "attack_85_dict_struct_key",
        "origin-ish\norigin-ish",
    );
}

#[test]
fn sec_86_multi_arg_print() {
    // `print(evil, evil)` — the second positional arg is silently
    // ignored (print only uses the first positional; extra args would
    // need to be kwargs like terminator=/file=). User data is treated
    // as opaque %.*s payload — no format injection.
    security_safe(
        "attack_86_multi_arg_print",
        "header\n%s %n %d %%\n%s %n %d %%",
    );
}

#[test]
fn sec_87_atomic_counter_four_spawners() {
    // 4 threads × 10000 increments = 40000. No lost updates.
    security_safe("attack_87_atomic_counter_race", "40000");
}

#[test]
fn sec_88_iterator_fusion_take_after_filter_map() {
    // filter(x%7==0).map(*2).take(3) → [0, 14, 28]. Lazy — stops
    // early instead of materializing all 100 elements.
    security_safe("attack_88_iterator_fusion", "3\n0\n7000\n14000");
}

#[test]
fn sec_89_large_dict_drop() {
    // 1000-entry Dict[String, Vector[int]] drops cleanly.
    security_safe("attack_89_large_dict_drop", "1000");
}

#[test]
fn sec_90_vector_zero_capacity_then_push() {
    // reserve(0) is a no-op; push allocates on demand.
    security_safe("attack_90_vector_zero_capacity", "2\n42\n43");
}

#[test]
fn sec_91_callable_clone_outlives_source() {
    // Deep-clone path on Ref[Callable].clone(). Before the fix, .clone()
    // shallow-memcpy'd the GorgetClosure, so the cloned f shared its env
    // pointer with the Vector's slot. Vector elem_drop then UAF'd /
    // double-free'd at scope exit. The fix routes Callable through
    // gorget_closure_clone_to_owned + size-prefixed env malloc + emits
    // gorget_closure_free on FnPtr-typed locals. Each loop iteration's f
    // owns its own env; the Vector owns the slot's env independently.
    security_safe(
        "attack_91_callable_clone_outlives_source",
        "101\n101\n101",
    );
}

#[test]
fn sec_92_static_set_runtime() {
    // Gap (c) — static `Set`/`HashSet` populated at runtime. Before the GIR
    // Set arm existed, the static slot stayed a null header (`GlobalInit::
    // Zeroed`) → the first `.add` dereferenced null → SIGSEGV (exit 139) on
    // BOTH backends. This is the silent-crash class CLAUDE.md #7 flags: the
    // always-pass `*_comparison` diagnostics never see it. Under ASan the
    // fixed program must build clean and run exit-0; a regression to the
    // null-header state trips a SEGV report here instead of passing silently.
    security_safe(
        "attack_92_static_set_runtime",
        "true\nfalse\ntrue\nfalse\n4",
    );
}

// ════════════════════════════════════════════════════════════════════════════
// Round-30 Fix C — CoW bare-assign owned-String leak guards (Core #6).
//
// The removed Branch A of `lower_var_decl_assign_mode` (src/ir/lowering/stmts/
// mod.rs) `set_shared_heap`'d a `String v = <owned live heap source>` and
// `unregister`ed the source's drop, while the backend DEEP-copied the source
// into a second buffer via `gorget_string_copy_cow` — so the source's heap
// allocation LEAKED. The leaks are stdout-INVISIBLE (the program prints the
// right bytes and leaks), so only a `detect_leaks=1` run guards them.
//
// Each fixture was verified to FAIL at the pre-fix baseline (`a9b034f1`, arm
// present) under `--sanitize` + `detect_leaks=1` and to PASS post-fix:
//   owned_string   → LeakSanitizer 23B / 1 alloc  → clean
//   return_alias   → LeakSanitizer 14B / 1 alloc  → clean
//   struct_escape  → LeakSanitizer 19B / 1 alloc  → clean
//   view_source    → LeakSanitizer 32B / 2 allocs → clean
//   alias_chain    → LeakSanitizer 51B / 3 allocs → clean
// A future regression that re-adds the SharedHeap+unregister shape re-leaks and
// trips `exitcode=99` here.
// ════════════════════════════════════════════════════════════════════════════

/// SECURITY KNOWN GAP — a `Box`ed trait object minted inside a CLOSURE and
/// returned leaks its allocation: 32 bytes direct through
/// `__gorget_box_alloc_Robot <- __Closure_0__call <- main`, at pristine HEAD.
///
/// The fixture is wired as a VALUE test only (`box_trait_closure_return` in
/// tests/integration.rs), which is why this has been green while leaking —
/// a stdout comparison structurally cannot see a leak (Core #13: pick an
/// instrument that can see the failure class).
///
/// DISCRIMINATED from `t0526` by ALLOCATION SOURCE: that one leaks the trait
/// pack from a discarded `Box.new`, this one leaks a box minted inside a
/// closure body and returned through the closure's call thunk. Also distinct
/// from the 3-byte thunk residual in the same report, which is a separate
/// known residual.
/// GRADUATED (R47 Track B) from an `#[ignore]`d known gap to a live guard.
/// Measured 35 B / 2 allocations at pristine `f3feea79`, 3/3 runs; CLEAN 3/3
/// once the closure call thunk's result is registered at its birth. Both
/// allocations in the report — the boxed trait object and the vtable thunk's
/// String return — were the same defect at two arms.
#[test]
fn box_trait_closure_return_no_leak() {
    security_safe_no_leak("box_trait_closure_return_leak", "R2");
}

/// `sort()` / `sorted()` / `unique()` on an EMPTY collection reached `qsort`
/// with a NULL base pointer, which is UNDEFINED BEHAVIOUR by the C standard
/// (`qsort`'s first parameter is declared `nonnull`) even at length zero.
///
/// RED-VERIFIED at `ce54bdc1`, the pre-fix compiler: this program printed the
/// expected 27 lines AND emitted **9** `runtime error: null pointer passed as
/// argument 1, which is declared to never be null` lines — one per
/// (element type × operation) cell. Post-fix stderr is empty and stdout is
/// byte-identical, which is the whole reason a stdout-comparing fixture could
/// not see this: `test_vector_sort_methods` exercised the same defect and had
/// been green in `cargo test --test integration` since it was written.
///
/// The class was 15 emitted call sites in Rust gg and 12 in the self-host
/// lowerer; both now build the call through one `qsort_guarded` producer, and
/// `emitted_qsort_is_guarded` in tests/lints.rs is the arm-count guard.
/// Retires `t0780` (closed at R47 close — DONE.md).
#[test]
fn sort_empty_collection_no_ub() {
    security_safe_no_ubsan(
        "sort_empty_collection_no_ub",
        "0\n0\n0\n0\n0\n0\n0\n0\n0\n42\n42\n1\n42\nsolo\nsolo\n1\n2\n3\n\
         0.250000\n0.500000\n0.750000\napple\nbanana\ncherry\n2\n1\n2",
    );
}

#[test]
#[ignore = "SECURITY KNOWN GAP (found + verified 2026-08-17 by the for-in idiom \
scout, orchestrator-reproduced): `for s in &d: s = \"zz\"` over a Vector[String] \
DOUBLE FREES and SIGABRTs on BOTH backends, while `gg check` reports \"OK: no \
semantic errors\". The syntax is safe and spec-documented (language-reference.md:1374 \
teaches `for x in &coll` as the in-place mutation form) — no unsafe, no ownership \
operator. Distinct from the filed `&`-write-through gap, which silently LOSES a \
write; this one corrupts the heap. Un-ignore when the assignment either writes \
through or is rejected at check time."]
fn security_amp_for_in_element_assign_double_free() {
    // INTENDED: the documented in-place mutation writes through. Today the
    // program is accepted and then double-frees, so this asserts the SPEC.
    security_safe(
        "attack_99_amp_for_in_element_assign_double_free",
        "zz\nzz",
    );
}

/// GRADUATED 2026-08-23 (R44 Track G census). The `Set[String]` byte-vs-str
/// ctor defect is fixed: `lower_set_literal_from_array`
/// (`src/ir/lowering/exprs/collections.rs`) now writes the element type through,
/// so the literal selects `gorget_ordered_set_new_str` and the set can own its
/// heap keys. The fix landed as a side effect of R43's container-literal
/// element-type work, under a different headline, which is why the entry stayed
/// open for four days after the bug stopped existing — and why the census that
/// found it is now a CI gate.
///
/// The cell still needs TWO read loops and HEAP elements: one loop is
/// accidentally correct and literal elements are a false negative, which is how
/// this shape was twice reported as not crashing (Core #15e Q6). Do not
/// simplify the fixture.
///
/// RED-VERIFY — break `gorget_ordered_set_new_str` to return the byte-keyed
/// `gorget_dict_new(sizeof(GorgetString), 0)` (same arity, so the break is
/// exactly `key_drop=NULL` and nothing else), then this test fails with
/// `AddressSanitizer: attempting double-free`. Verified 2026-08-23. Either
/// route works, but ⚠ **the C runtime is EMBEDDED IN THE COMPILER BINARY**, so:
///   * source route — edit `src/backend/c/runtime/runtime_set.c` **and then
///     `cargo build`**. Rebuilding only the fixture is a NO-OP; the break
///     silently does not apply and you get rc 0 with correct output, which
///     reads as "the bug is gone". Two people hit this.
///   * emitted-C route — `gg build --sanitize <fixture>`, edit the
///     `gorget_ordered_set_new_str` body in the generated `.c`, then
///     `cc -O0 -g -o broken broken.c -lm -lpthread` and run it. No compiler
///     rebuild needed.
///
/// ⚠ `security_safe` runs with `detect_leaks=0`, so it guards the double free,
/// not the leak. The leak half was checked separately and is also gone (rc 0
/// under `detect_leaks=1`), and the live-source cell below is the one wired to
/// `security_safe_no_leak`.
#[test]
fn security_set_string_heap_elem_double_free() {
    // INTENDED: reading a set of Strings twice prints them twice and exits 0.
    security_safe(
        "attack_101_set_string_heap_elem_double_free",
        "aa\nbb\naa\nbb",
    );
}

/// LIVE-SOURCE cell of the set-literal element-ownership axis — the sibling of
/// `security_set_string_heap_elem_double_free` (whose elements are dead
/// temporaries). Building a `Set[String]` from live named locals must CLONE
/// them, and the sources are read afterwards.
///
/// ⚠ Those reads are a FORWARD guard, not a demonstrated one: forcing the
/// literal to move a live local was measured NOT to change this output, because
/// `key_materialize` deep-copies the key and the `MoveZero` is elided. What the
/// cell actually pins is the source-liveness AXIS (its other value is the
/// sibling) and a leaked clone on this path.
///
/// This cell was BLOCKED, not merely uncovered: the clone-if-live picker for set
/// literals had been reverted because a live element's clone leaked into the
/// byte-variant set, leaving `Set[String] s = {x, y}` as a validator panic. The
/// same ctor-selection fix unblocked it, so the axis is covered on both values
/// for the first time.
///
/// Wired to `security_safe_no_leak` (`detect_leaks=1`) rather than
/// `security_safe`, because the failure mode this cell risks is a LEAKED CLONE,
/// which is stdout-invisible and which a `detect_leaks=0` run passes.
///
/// ⚠ THIS ONE COULD NOT BE MADE RED — four breaks of the set-literal ownership
/// path all left it green, because `_new_str`'s `key_materialize` deep-copies
/// the key inside `gorget_set_add` and hides the caller-side decision. The
/// fixture header enumerates all four and states what the cell pins instead (it
/// was a validator PANIC before the fix, and the sources must survive). It is a
/// boundary/control cell, not a mechanism guard — do not cite it as one.
#[test]
fn set_string_live_source_elements_safe() {
    security_safe_no_leak(
        "set_string_live_source_elements",
        "aa\nbb\naa\nbb",
    );
}

#[test]
#[ignore = "SECURITY KNOWN GAP (found 2026-08-17 by the Track-E brief-review, \
orchestrator-reproduced and axis-corrected): `unwrap()` on an Option payload \
BORROWED out of a collection frees what the collection still owns — DOUBLE FREE, \
SIGABRT, BOTH backends, while `gg check` reports \"OK: no semantic errors\". NO \
assignment and NO loop are involved — this is a READ — so it is a DISTINCT \
mechanism from the for-in element-rebind double free, not the same bug in \
another costume. The discriminator is `unwrap()`, NOT the container or the \
element type: a `match o: case Some(s)` readback of the very same value is rc 0, \
binding without unwrapping is rc 0, and a struct element whose Option is a FIELD \
crashes identically (cell 2). Scalar payloads, container-free Options and \
LITERAL payloads are all false negatives. Un-ignore when unwrap stops freeing a \
payload it does not own."]
fn security_unwrap_borrowed_payload_double_free() {
    // INTENDED: an ordinary read prints both payloads and exits 0. There is
    // nothing to reject here — every construct is safe, documented Gorget.
    security_safe(
        "attack_100_unwrap_borrowed_payload_double_free",
        "aa\nbb",
    );
}

#[test]
fn cow_bareassign_owned_string_no_leak() {
    // `String v = sb` with `sb` a heap-owned String (`a + b`), both live to
    // scope exit. Baseline leaked sb's 23-byte buffer.
    security_safe_no_leak(
        "cow_bareassign_owned_string_leak",
        "hello, cow-owned world\nhello, cow-owned world",
    );
}

/// `??` on an OWNING (`!`) param carrier — the deref-copy cell of the
/// carrier-ownership axis.
///
/// `lower_expr`'s identifier arm peels a non-Copy param's pointer into a temp
/// that is a SHALLOW copy of the pointee, so the temp does NOT own the payload
/// even though its type is a plain `Option[T]`. The `??` lowering asked the
/// post-deref temp for Ptr-ness, got "no", and took the source-owns path — the
/// Move-extract zeroed only the copy's payload field while the param's slot
/// kept the same buffer and stayed registered for drop. Both dropped.
///
/// RED-verified against the pre-fix compiler: `RUN_RC=134`, ASan
/// `attempting double-free`. The payload is heap-forced (a `Vector` built by
/// push) — a literal payload is a measured false negative for this class.
///
/// ⚠ Lives in `security/` rather than top-level `tests/fixtures/` because the
/// self-host lane rejects `??` with a `Vector` payload
/// (`E_DefaultOpRhsTypeMismatch`); a top-level placement would add a non-MATCH
/// parity row for the round's own inflow (Core #9 ⊕). The SH over-rejection is
/// filed separately.
#[test]
fn security_default_op_owning_param_double_free() {
    // INTENDED: the unwrap prints the payload's length and the program exits 0.
    security_safe_no_leak(
        "attack_102_default_op_owning_param_double_free",
        "2\ndone",
    );
}

#[test]
fn cow_bareassign_return_alias_no_leak() {
    // Heap-owned String bare-assigned, then RETURNED (escapes the frame).
    // Baseline leaked 14 bytes.
    security_safe_no_leak("cow_bareassign_return_alias_leak", "aaa-bbb-owned");
}

#[test]
fn cow_bareassign_struct_escape_no_leak() {
    // Heap-owned String bare-assigned, then stored into an owned struct field.
    // Baseline leaked 19 bytes.
    security_safe_no_leak(
        "cow_bareassign_struct_escape_leak",
        "field-escape-owned\nfield-escape-owned",
    );
}

#[test]
fn cow_bareassign_view_source_no_leak() {
    // Heap-owned String bare-assigned, a view (`.substring`) taken of the alias,
    // then materialized. Baseline leaked 32 bytes across 2 allocations.
    security_safe_no_leak(
        "cow_bareassign_view_source_leak",
        "view-\nview-source-owned-payload\nview-source-owned-payload",
    );
}

#[test]
fn cow_bareassign_alias_chain_no_leak() {
    // Transitive alias chain off one heap-owned String, all live to scope exit.
    // Baseline leaked 51 bytes across 3 allocations (one per hop).
    security_safe_no_leak(
        "cow_bareassign_alias_chain_leak",
        "chain-owned-tail\nchain-owned-tail\nchain-owned-tail\nchain-owned-tail",
    );
}

#[test]
fn cow_element_borrow_no_uaf() {
    // Element borrows held across a reallocating growth of the collection —
    // spelled through an alias, through the collection's own name, and via
    // `v[i]` with no alias in the program at all. Baseline: rc 139 on both
    // backends, and under `--sanitize` a heap-use-after-free in
    // `gorget_string_clone_to_owned` / `gorget_string_copy_cow`.
    //
    // stdout is a weak instrument here: freed memory routinely still holds
    // the right bytes, so a stdout-only net can go green over a live UAF.
    // ASan is what adjudicates memory validity (Core #13), and it is C-lane
    // only — the LLVM `--sanitize` path emits a binary with no ASan in it.
    security_safe_no_leak(
        "cow_element_borrow_uaf",
        "hello\nhello\nhello\nhello",
    );
}

#[test]
fn box_resource_struct_no_leak() {
    // `Box[Money]` local, Money owning a heap `String` (Recursive drop). The
    // box local's scope-exit drop must run `Money__drop` before the box free.
    // Baseline leaked the inner heap String (22 bytes/box) — `drops.rs`'s
    // Box-inner-drop match had no `DropStrategy::Recursive` arm.
    security_safe_no_leak("box_resource_struct_leak", "box-struct-ok");
}

#[test]
fn box_resource_enum_no_leak() {
    // `Box[Msg]` local, Msg an enum with a resource-typed variant payload
    // (Recursive drop). Same Box-inner-drop hole as the struct case; the box
    // local's scope-exit drop must run the enum-dispatch `Msg__drop`.
    security_safe_no_leak("box_resource_enum_leak", "box-enum-ok");
}

// KNOWN GAP — box sibling #2 (filed 2026-07-24): `Box[user-Drop struct with a
// droppable field]` leaks the field. The user `drop` runs ("dropping") but the
// field frees (in `__gorget_dtor`) are skipped — a wrong-fn-selection shared
// with `box_inner_drop_fn`. Un-ignore + promote when the CLASS is fixed.
#[test]
#[ignore = "KNOWN GAP box sibling #2: Box[user-Drop-struct-with-field] leaks the field; \
TODO.md. Un-ignore when box_inner_drop_fn/inline both target __gorget_dtor_R."]
fn box_user_drop_struct_field_no_leak() {
    security_safe_no_leak("box_user_drop_struct_field_leak", "done\ndropping");
}

// KNOWN GAP — R1 (filed 2026-07-24, MEMORY-SAFETY): moving a `Box[T]` via `!bx`
// into a consuming `!`-param fn BAD-FREES a stack address on ALL Box[T] (accepted
// by `gg check`; ASan "attempting free on address which was not malloc()-ed",
// both backends). The `!`-param Box-consumption ABI passes `&slot` and the callee
// frees it. Un-ignore when the consumption ABI is fixed. See TODO.md.
#[test]
#[ignore = "KNOWN GAP R1: !-param Box move bad-frees a stack address (accepted \
program, ASan abort, both backends); TODO.md. Un-ignore when the !-Box consume ABI is fixed."]
fn box_move_param_bad_free_safe() {
    security_safe("box_move_param_bad_free", "consumed\ndone");
}

// ════════════════════════════════════════════════════════════════════════════
// `&`-param at an ownership boundary — return / bind / re-assign
// ════════════════════════════════════════════════════════════════════════════
//
// A `&`-param is a BORROW: the caller keeps ownership. Crossing an ownership
// boundary owes exactly ONE clone; a typed binding owes ZERO (borrow now,
// materialize on mutation). Every fixture below reported a double-free (or, for
// the String arm, an LSan leak) before the statement `return` was routed through
// the shared materialize chokepoint and the auto-deref temp started carrying its
// borrow provenance. The bug class is stdout-INVISIBLE, so these run under
// LeakSanitizer via `security_safe_no_leak`, not a plain stdout check.

#[test]
fn retborrow_return_amp_param_vector_no_leak() {
    // Arm 2: `return v` of a whole `Vector[int] &`-param, non-throws.
    security_safe_no_leak("retborrow_return_amp_param_vector", "4\n4");
}

#[test]
fn retborrow_bind_amp_param_vector_no_leak() {
    // Arm 4: bind then return. The bind is a CoW Ptr alias of the param.
    security_safe_no_leak("retborrow_bind_amp_param_vector", "4\n4");
}

#[test]
fn retborrow_reassign_amp_param_vector_no_leak() {
    // Arm 5: re-assign into an existing owned slot, then return.
    security_safe_no_leak("retborrow_reassign_amp_param_vector", "4\n4");
}

#[test]
fn retborrow_return_amp_param_string_no_leak() {
    // Arm 6: the String `&`-param return — two clones and an orphaned buffer.
    security_safe_no_leak("retborrow_return_amp_param_string", "5\n5");
}

#[test]
fn retborrow_bind_amp_param_struct_no_leak() {
    // Receiver-shape width: a user struct with a resource field.
    security_safe_no_leak("retborrow_bind_amp_param_struct", "4\n4\ntag");
}

#[test]
fn retborrow_return_amp_param_dict_no_leak() {
    // Receiver-shape width: a Dict with heap-allocated keys.
    security_safe_no_leak("retborrow_return_amp_param_dict", "3\n3");
}

#[test]
fn retborrow_bind_then_mutate_no_leak() {
    // The alias-at-bind's correctness obligation: materialize on mutation, so
    // the CALLER's value is unchanged. Two receiver shapes.
    security_safe_no_leak("retborrow_bind_then_mutate", "5\n4\n4\n4\n3\n3");
}

#[test]
fn retborrow_mutate_through_control_no_leak() {
    // CONTROL: mutating the `&`-param itself must still WRITE THROUGH — so the
    // aliasing fix cannot degenerate into "always clone".
    security_safe_no_leak("retborrow_mutate_through_control", "5\n999");
}

#[test]
fn retborrow_throws_bare_return_no_leak() {
    // CONTROL: the throws bare-`return v` leg was already clean and must stay so.
    security_safe_no_leak("retborrow_throws_bare_return", "4\n4");
}

#[test]
fn retborrow_no_over_clone_controls_no_leak() {
    // ANTI-OVER-CLONE controls: owned-local return · bare-param return ·
    // `!`-param return · bind-then-read-only.
    security_safe_no_leak("retborrow_no_over_clone_controls", "4\n4\n4\n3\n42\n4");
}

// ── DESCOPED arms: `return &v` is a VALUE-POSITION `&` ──────────────────────
// ⚖ Owner ruling: a value-position `&` must be REJECTED at check time, and the
// ruling explicitly covers `return &v`. Both fixtures below are therefore
// `security_known_unsafe` pins on a program the compiler should not accept —
// NOT accept-polarity fixtures, which would wire the wrong answer into a
// committed artifact. They fail loudly when the reject lands.

#[test]
fn retborrow_valuepos_amp_return_rejected() {
    // Round XXIII Track β (2026-08-01) landed `E_AmpInOperandPosition`; the
    // value-position `&`-at-`return` case is now correctly REJECTED at check
    // time. Reclassified from `security_known_unsafe(SilentlyProduces)` to
    // `security_rejected` per the prior entry's own instruction.
    security_rejected("retborrow_valuepos_amp_return", "E_AmpInOperandPosition");
}

/// KNOWN GAP (filed 2026-07-25 by the Track-B1 output-review). Re-assigning
/// THROUGH a `&`-param never drops the param slot's OLD pointee, so the caller's
/// original buffer leaks 64 bytes.
///
/// PRE-EXISTING and independent of the return boundary — it reproduces
/// identically before and after Track B1. It was MASKED in the combined costume
/// (`local = v; v = build(2); return local`), where the bind's double-free
/// happened to cancel the missing drop; fixing the bind correctly unmasks it.
///
/// The write-through VALUES are already correct (4 then 2) — only the drop of the
/// old value is missing, which is why this asserts the INTENDED no-leak state and
/// is `#[ignore]`d rather than pinning today's behaviour. `security_known_unsafe`
/// is the wrong harness here: it runs with `detect_leaks=0`, so a pure leak does
/// not trip it. UN-IGNORE when the write-through re-assign drops the old pointee.
#[test]
#[ignore = "known gap: write-through re-assign leaks the old pointee; asserts the INTENDED no-leak state"]
fn reassign_amp_param_leaks_old_value() {
    security_safe_no_leak("reassign_amp_param_leaks_old_value", "4\n2\n");
}

#[test]
fn retborrow_valuepos_amp_return_throws_rejected() {
    // Round XXIII Track β (2026-08-01) landed `E_AmpInOperandPosition`; the
    // value-position `&`-at-`return` case in a `throws` fn is also correctly
    // REJECTED at check time (the reject fires before the throws-leg materialize
    // ever runs). Reclassified per the prior entry's own instruction.
    security_rejected("retborrow_valuepos_amp_return_throws", "E_AmpInOperandPosition");
}

// ════════════════════════════════════════════════════════════════════════════
// CoW aliasing-soundness durable repros (filed 2026-07-25).
//
// Every fixture below was measured at HEAD BEFORE being committed, and the
// observed pre-fix failure is recorded in its `.gg` header (Core #12: a fixture
// is not coverage until it has been seen to FAIL). Each `#[ignore]` reason names
// exactly what un-ignores it.
//
// The intent polarity is deliberate throughout: these assert the INTENDED state,
// never today's behaviour, so none of them can make a defect canonical.
// `security_known_unsafe` is NOT used here for that reason — it pins the bug as
// present, which is the opposite of what a durable repro for an unfixed defect
// should encode when the intended state is already known.
//
// LIVE (not ignored) members of the batch are the anti-regression CONTROLS: they
// are green today and go RED if a fix over-reaches. They are the other half of
// Core #12 — the axis is only covered when both the broken and the
// accidentally-working cells are pinned.
// ════════════════════════════════════════════════════════════════════════════

// ── A1: Callable-typed local bound to an `&`-param function ─────────────────

/// REGRESSION — A1 cell of the callable-indirection axis: a `Callable`-typed
/// LOCAL bound to an `&`-param function writes through on indirect call.
/// (`f = bumpint; f(&x)` — plain-local int root × LOCAL callable × `&`-arg.)
/// Pre-Track-B1: `gg check` clean, build clean, binary exit 139 (SIGSEGV);
/// under ASan, `SEGV on unknown address 0x29 ... in bumpint`. Fix wires the
/// callable's declared `param_ownerships` into `lower_call_arg` at both
/// indirect-call arms in `src/ir/lowering/exprs/calls.rs`.
#[test]
fn sound_callable_amp_param_indirect_call_safe() {
    security_safe("sound_callable_amp_param_indirect_call", "42");
}

/// KNOWN GAP — MEMORY-UNSAFE. An escaping closure that captured a local by
/// reference reads a dropped stack slot. `gg check` accepts; both backends
/// print a raw address where 1 then 2 belong.
///
/// The discriminator is why it slips through: `E_DanglingReturn` DOES fire when
/// the closure returns the captured value, but not when it merely reads it — so
/// the check appears keyed on the closure's return value rather than on what it
/// captures.
#[test]
#[ignore = "KNOWN GAP: an escaping closure reads its captured local from freed stack on both \
backends while `gg check` accepts. Asserts the INTENDED 1/2 (or a check-time reject); TODO.md."]
fn sound_closure_escaping_capture_dangles_safe() {
    security_safe("sound_closure_escaping_capture_dangles", "1\n2");
}

/// LIVE CONTROLS — the CALLEE-DISPOSITION axis. Green today and must stay so.
///
/// They guard a measured hazard: routing `exprs/calls.rs:371` through
/// `ensure_owned_at_consuming_arg` (whose Case 1 registers the clone for drops)
/// adds a caller-side drop with no compensating `MoveZero`. Under ASan that is
/// a double-free — but ONLY when the callee moves its `!` param ONWARD, because
/// `drop_if_alive` is self-nulling and makes the extra drop a no-op when the
/// callee merely drops it. That is why the pre-existing fixture over this site
/// stayed green under the same change, and why this axis needed its own net.
#[test]
fn consume_callee_moves_on_return_safe() {
    security_safe("consume_callee_moves_on_return", "4\ndone");
}

#[test]
fn consume_callee_moves_on_stash_safe() {
    security_safe("consume_callee_moves_on_stash", "1\n3\ndone");
}

/// GRADUATED Round XXIII Track β — `&`-of-a-projection in an OPERAND position
/// (tainted-twin double-Drop facet) is now REJECTED at `gg check` with
/// `E_AmpInOperandPosition` via the one-producer chokepoint at the
/// `check_expr::Expr::MutableBorrow` arm. This graduation re-purposes the
/// pre-existing DURABLE security repro (no new file — Core #12 durable-repro
/// rule) as a check-fails assertion. Same underlying root as the
/// silent-wrong-output twin (`known_gaps/sound_amp_operand_position_scrutinee.gg`);
/// the class reject retires BOTH facets in one arm.
#[test]
fn sound_amp_operand_position_duplicate_drop_rejected() {
    security_rejected(
        "sound_amp_operand_position_duplicate_drop",
        "error[E_AmpInOperandPosition]",
    );
}

/// Round XXIII Track β — TAINTED-TWIN class-breadth pin (Core #11 wide
/// coverage): match scrutinee sibling of the `if`-cond twin, same class
/// reject collapses both. Scout confirmed EVERY operand costume duplicates
/// the user Drop (`0\nclose 9\nclose 9`), not just `if`.
#[test]
fn sound_amp_operand_drop_scrutinee_rejected() {
    security_rejected(
        "sound_amp_operand_drop_scrutinee",
        "error[E_AmpInOperandPosition]",
    );
}

/// Round XXIII Track β — TAINTED-TWIN binop-RHS sibling: same class reject.
/// Pre-fix: prints `<address>\nclose 9\nclose 9`.
#[test]
fn sound_amp_operand_drop_binop_rev_rejected() {
    security_rejected(
        "sound_amp_operand_drop_binop_rev",
        "error[E_AmpInOperandPosition]",
    );
}

/// Round XXIII Track β — TAINTED-TWIN augassign sibling. Third of three
/// representative shape samples (Core #12: three-cell axis coverage rather
/// than 15 near-duplicates).
#[test]
fn sound_amp_operand_drop_augassign_rejected() {
    security_rejected(
        "sound_amp_operand_drop_augassign",
        "error[E_AmpInOperandPosition]",
    );
}

/// REGRESSION — A2 cell of the callable-indirection axis: a PROJECTION arg
/// through a `Callable`-typed PARAMETER writes through on indirect call.
/// (`struct-`&`-param projection root × PARAM callable × `&`-arg`.) Pre-Track-B1:
/// SEGV on both backends at runtime; the UNIT_TYPE arm in
/// `src/ir/lowering/exprs/calls.rs` (Callable-PARAMETER, `__callable_N`) chose
/// pointer-vs-value from the argument's provenance rather than the callee's
/// declared param types. Fix populates `callable_param_types` +
/// `callable_param_ownerships` for the parameter and routes each user arg
/// through `lower_call_arg`.
#[test]
fn sound_callable_amp_param_projection_safe() {
    security_safe("sound_callable_amp_param_projection", "11\n11");
}

// ── D10 exclusivity holes: accepted programs that heap-use-after-free ────────
//
// All eleven assert the INTENDED REJECT (`E_BorrowConflict`). Measured at HEAD:
// every one of them passes `gg check` and then trips AddressSanitizer with a
// heap-use-after-free — the per-fixture stack frame is recorded in each header.
// ggdef ACCEPTS these (it is a value-semantics interpreter, so the class is
// unobservable in that lane) — see the oracle note in
// `sound_excl_getchain_local.gg`; the intended reject is the ratified owner
// ruling, not ggdef's verdict.

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #1, get-chain view arg): accepted program, ASan \
heap-use-after-free in gorget_string_copy_cow. Asserts the INTENDED E_BorrowConflict reject; \
TODO.md. Un-ignore when the semantic place-overlap chokepoint lands."]
fn sound_excl_getchain_local_rejected() {
    security_rejected("sound_excl_getchain_local", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #3, Static root): accepted program, ASan \
heap-use-after-free; the byte-identical LOCAL spelling already rejects. Asserts the INTENDED \
E_BorrowConflict reject; TODO.md. Un-ignore when the chokepoint admits Static roots."]
fn sound_excl_getchain_static_rejected() {
    security_rejected("sound_excl_getchain_static", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #1, field-path costume): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the \
chokepoint's place resolver descends field paths through view chains."]
fn sound_excl_getchain_fieldpath_rejected() {
    security_rejected("sound_excl_getchain_fieldpath", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, alias-provenance costume): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the \
chokepoint resolves a CoW bind alias to its root."]
fn sound_excl_getchain_alias_provenance_rejected() {
    security_rejected("sound_excl_getchain_alias_provenance", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #2, method receiver): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the \
receiver becomes a participant (&self = writer, bare self = reader)."]
fn sound_excl_receiver_writer_rejected() {
    security_rejected("sound_excl_receiver_writer", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #4, for-body &-arg writer): accepted program, ASan \
heap-use-after-free in gorget_string_borrow; the direct `v.push` spelling already rejects. \
Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when the iterable guard sees \
through a call boundary."]
fn sound_excl_forbody_amp_writer_rejected() {
    security_rejected("sound_excl_forbody_amp_writer", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, nested writer one call deep): accepted program, ASan \
heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when a \
writer inside a sibling arg's expression tree becomes a participant."]
fn sound_excl_nested_writer_call_rejected() {
    security_rejected("sound_excl_nested_writer_call", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, nested writer at a CLOSURE call): accepted program, \
ASan heap-use-after-free in __Closure_0__call. Asserts the INTENDED E_BorrowConflict reject; \
TODO.md. Un-ignore when closure calls route through the chokepoint."]
fn sound_excl_nested_writer_closure_rejected() {
    security_rejected("sound_excl_nested_writer_closure", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, nested writer in an INDEX STORE): accepted program, \
ASan heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when \
index-store value operands route through the chokepoint."]
fn sound_excl_nested_writer_index_store_rejected() {
    security_rejected("sound_excl_nested_writer_index_store", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10(b) ADDENDUM 3 amendment, ctor arg vs sibling WRITER): accepted \
program, ASan heap-use-after-free in gorget_string_clone_to_owned — the boundary clone is \
emitted AFTER sibling evaluation. Asserts the INTENDED E_BorrowConflict reject; TODO.md + \
decisions.md. Un-ignore when aggregate init participates on the writer axis."]
fn sound_excl_nested_writer_ctor_rejected() {
    security_rejected("sound_excl_nested_writer_ctor", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10(b) ADDENDUM 3 amendment, TUPLE twin of the ctor row): accepted \
program, ASan heap-use-after-free. Asserts the INTENDED E_BorrowConflict reject; TODO.md + \
decisions.md. Un-ignore when tuple_init participates on the writer axis."]
fn sound_excl_nested_writer_tuple_rejected() {
    security_rejected("sound_excl_nested_writer_tuple", "E_BorrowConflict");
}

#[test]
#[ignore = "KNOWN GAP (D10 exclusivity, operator overload + bind alias): accepted program, ASan \
heap-use-after-free in Acc__add. NOTE `a + a` runs clean and is false confidence — the ALIAS \
twin is the real shape. Asserts the INTENDED E_BorrowConflict reject; TODO.md. Un-ignore when \
operator-overload calls route through the chokepoint with receiver participation."]
fn sound_excl_overload_alias_twin_rejected() {
    security_rejected("sound_excl_overload_alias_twin", "E_BorrowConflict");
}

// ── D10 hole #5: the one with the OPPOSITE polarity ─────────────────────────

/// Owner-ruled MATERIALIZE, not reject (TODO.md): the mutation site is visible,
/// so the clone is placed lazily at the binding. Measured at HEAD: `gg check`
/// clean, ASan heap-use-after-free in `gorget_string_copy_cow`.
#[test]
#[ignore = "KNOWN GAP (D10 exclusivity hole #5, match-scrutinee element): accepted program, \
ASan heap-use-after-free. Owner-ruled MATERIALIZE (NOT reject) — asserts the INTENDED \
pre-mutation value; TODO.md. Un-ignore when the match-binding materialize lands. A fix that \
REJECTS this program has implemented the wrong ruling."]
fn sound_match_scrutinee_elem_materialize_safe() {
    security_safe("sound_match_scrutinee_elem_materialize", "alpha-heap-forced-payload");
}

// ── The `&`-in-an-owning-position class: the DROP facet ─────────────────────

/// Measured at HEAD, BOTH backends: `1 / close 9 / close 9` — the owner's user
/// `Drop` runs TWICE. ggdef REJECTS the literal form. The intended state is the
/// reject; this fixture additionally pins that if it were ever accepted, the
/// drop must run exactly ONCE (a single-owner `Drop` violation is not an
/// acceptable outcome under any reading).
#[test]
#[ignore = "KNOWN GAP (&-in-an-owning-position, DROP facet): `[&p.fd]` duplicates the owner's \
user Drop on C AND LLVM; ggdef rejects. Asserts the INTENDED single drop; TODO.md. Un-ignore \
when the owning-position reject (or the drop-taint gate at container-literal element positions) \
lands."]
fn sound_amp_array_literal_duplicate_drop_safe() {
    security_safe("sound_amp_array_literal_duplicate_drop", "1\nclose 9");
}

/// LIVE CONTROL — the unsigilled twin. Green today; goes RED if a fix changes
/// how a bare container-literal element takes ownership.
#[test]
fn sound_amp_array_literal_duplicate_drop_control_safe() {
    security_safe("sound_amp_array_literal_duplicate_drop_control", "1\nclose 9");
}

// ════════════════════════════════════════════════════════════════════════════
// Round XXII β — Option/Result combinator adapter payload-registration class
// ════════════════════════════════════════════════════════════════════════════
//
// Pre-fix (scout `/tmp/round_xxii_trackBeta_scout_79000.md`), the combinator
// adapter's extracted payload (`try_lower_option_result_combinator` at
// `src/ir/lowering/exprs/methods.rs:3795-3922`) was `set_owned` but never
// `drops.register_local(...)`. Arms whose closure only BORROWED the payload
// (map · flat_map · and_then · map_err · unwrap_or_else-Result-Error with a
// mapped-away return type) leaked 24-56B of the payload's heap bytes per
// call — no owner registered, no scope-exit `DropIfAlive` fired. Class-fix
// (Track β, 2026-08-01): route all 5 extraction sites through the shared
// `extract_enum_payload_owned` helper (Core #3 birth-registration + Core #4
// producer); pair the 4 currently-clean aliasing wrap sites with
// `mov(payload) + move_zero(payload)` to avoid a new double-free (Core #10;
// the LIR does not zero the source slot on a Move operand).
//
// Every fixture below RED-verified against the pre-fix compiler (Track β
// executor, 2026-08-01, HEAD 0aa3f5a6 with methods.rs helper reverted):
//   combinator_leak_map_money_param                → 24B / 1 alloc  → clean
//   combinator_leak_flat_map_money_param           → 32B / 1 alloc  → clean
//   combinator_leak_and_then_money_param           → 40B / 1 alloc  → clean
//   combinator_leak_map_err_money_param            → 48B / 1 alloc  → clean
//   combinator_leak_unwrap_or_else_result_money_param → 56B / 1 alloc → clean
// A future regression that removes the birth-registration OR the b1 pairing
// re-leaks the payload and trips `exitcode=99` here.

#[test]
fn combinator_leak_map_money_param_no_leak() {
    // AXIS: Option[Money].map((Money m): int) — mapped-away return type;
    // closure only borrows payload. Pre-fix: `Direct leak of 24 byte(s)
    // in 1 object(s)` via `Option__Money__clone → Money__clone →
    // gorget_array_clone` on the extracted payload.
    security_safe_no_leak("combinator_leak_map_money_param", "3");
}

#[test]
fn combinator_leak_flat_map_money_param_no_leak() {
    // AXIS: Option[Money].flat_map((Money m): Option[int]) — sibling arm
    // (closure returns Option[U]). Pre-fix: `32 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_flat_map_money_param", "4");
}

#[test]
fn combinator_leak_and_then_money_param_no_leak() {
    // AXIS: Option[Money].and_then((Money m): Option[int]) — cross-type
    // sibling. Pre-fix: `40 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_and_then_money_param", "5");
}

#[test]
fn combinator_leak_map_err_money_param_no_leak() {
    // AXIS: Result[int, Money].map_err((Money m): int) — Error-branch
    // sibling; err_val extraction had the same missing-registration hole.
    // Pre-fix: `48 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_map_err_money_param", "6");
}

#[test]
fn combinator_leak_unwrap_or_else_result_money_param_no_leak() {
    // AXIS: Result[int, Money].unwrap_or_else((Money m): int) — Error
    // branch err_val borrowed into closure; Ok branch keeps original int.
    // Pre-fix: `56 byte(s) in 1 object(s)`.
    security_safe_no_leak("combinator_leak_unwrap_or_else_result_money_param", "7");
}

/// Round XXIII Track α — SBO guard for cross-type Result.or_else with an
/// Error-axis-cross closure return. Pre-fix (scout
/// `/tmp/round_xxiii_trackAlpha_scout_1.md`): AddressSanitizer:
/// stack-buffer-overflow READ of size 80 at the merge memcpy in
/// `try_lower_option_result_combinator`; `result_local` was mis-sized to
/// recv_type (24B) while the closure wrote 80B. Fixed by extending the
/// `and_then | flat_map` arm at `src/ir/lowering/exprs/methods.rs:3779` to
/// include `"or_else"` (sizes `result_local` from the closure's declared
/// return). The typecheck class-guard `unify_closure_ret_axis` in
/// `src/semantic/typecheck.rs` pins the Ok-payload unify (T'==T; E'≠E is
/// the recovery axis).
#[test]
fn combinator_sbo_or_else_result_error_cross_type_asan_clean() {
    security_safe_no_leak("combinator_sbo_or_else_result_error_cross_type", "5");
}

// ── B5: the Option/Result combinator skips the payload's user Drop ──────────

/// Measured at HEAD under `--sanitize` + `detect_leaks=1`:
/// `512 / control-end / dropping / 512 / map-end` — the trailing `dropping` is
/// MISSING — plus `LeakSanitizer: 8192 bytes in 2 allocations`. The `is Some`
/// control leg in the same program is clean, which is what makes the map leg's
/// silence a defect rather than a design choice.
#[test]
#[ignore = "KNOWN GAP (combinator adapter): Option.map skips the payload's user Drop and leaks \
8192B; the `if o is Some` spelling of the same program is clean. Asserts the INTENDED \
drop-runs/no-leak state; TODO.md. Un-ignore when the combinator adapter owns its payload."]
fn sound_option_map_user_drop_no_leak() {
    security_safe_no_leak(
        "sound_option_map_user_drop_leak",
        "512\ncontrol-end\ndropping\n512\nmap-end\ndropping",
    );
}

// ── Anti-regression CONTROLS for the aggregate-init mover exemption ─────────

/// LIVE — D10(b) ADDENDUM 3's mover exemption. Green today; goes RED if the
/// chokepoint rebuild blanket-rejects aggregate init (a new over-rejection).
#[test]
fn sound_ctor_mover_sibling_accept_safe() {
    security_safe(
        "sound_ctor_mover_sibling_accept",
        "alpha-heap-forced-payload\nbeta-heap-forced-payload\ngamma-heap-forced-payload",
    );
}

/// The one row of the batch whose intended state is MORE permissive than
/// today's. Measured at HEAD: `gg check` REJECTS with `E_BorrowConflict` while
/// the struct and tuple spellings of the identical program accept.
#[test]
#[ignore = "KNOWN GAP (over-rejection): enum-variant construction routes through the \
CALL-shaped aliasing check, so `E.Two(v[0], !v)` rejects while `Pair(v[0], !v)` accepts. \
D10(b) ADDENDUM 3 ratifies the WIDENING; asserts the INTENDED accept. Un-ignore when enum \
ctors use the aggregate-init predicate."]
fn sound_enum_ctor_mover_sibling_accept_safe() {
    security_safe("sound_enum_ctor_mover_sibling_accept", "alpha-heap-forced-payload");
}

/// LIVE AXIS PIN — the mover-FIRST ordering must reject on the LIVENESS axis
/// (`E_UseAfterMove`), not the aliasing axis. The mover exemption above is only
/// sound because this case is already caught here; if a refactor makes this
/// fail with `E_BorrowConflict` instead, the move-tracker silently lost a case.
#[test]
fn sound_ctor_mover_first_rejected() {
    security_rejected("sound_ctor_mover_first_reject", "E_UseAfterMove");
}

/// LIVE — MEMORY-VALIDITY pin for the class `t0699` closed: renaming a user
/// `&self` mutator must not change whether the program is memory-safe.
///
/// This cell belongs HERE and not only in the runtime corpus because neither
/// of the other adjudicators can see it: ggdef adjudicates VALUE semantics and
/// accepts live heap-UAFs, and stdout cannot distinguish "correct" from
/// "crashed before flush". ASan on the real backend can.
///
/// RED-verified at `f3feea79` (2026-08-29, `ASAN_OPTIONS=halt_on_error=1:\
/// detect_leaks=0:allocator_may_return_null=1`, single-threaded so
/// `use_stacks` is not in play): `heap-use-after-free` READ of size 8 in
/// `gorget_string_clone_to_owned` <- `Unlisted__probe`, and NOTHING for the
/// byte-identical `Listed` half whose mutator happened to be named `resize`.
#[test]
fn sound_user_mutator_name_invariant_uaf() {
    security_safe(
        "sound_user_mutator_name_invariant_uaf",
        "helloworld\nhelloworld",
    );
}

/// LIVE — the intra-function sever. Green today; goes RED if a chokepoint
/// rejects on "a view of `v` exists in scope" instead of "a view of `v` is live
/// at this call".
#[test]
fn sound_getchain_sever_accept_safe() {
    security_safe("sound_getchain_sever_accept", "alpha-heap-forced-payload");
}

/// LIVE — the thin-pointer half of the `&`-of-field write-through axis, i.e.
/// the two cells that work by accident (Core #12's type case). Green today;
/// goes RED if the by-value fix "unifies" the halves by breaking this one.
#[test]
fn sound_amp_field_thinptr_control_safe() {
    security_safe("sound_amp_field_thinptr_control", "base-grown\n2\n99");
}

/// LEAK GUARD — `f(&(*box).field)` must NOT clone the whole struct to take an
/// address of. Before the Family-1 chokepoint it did, leaking the clone's
/// deep-copied `String` field (`gorget_string_clone_to_owned` <- `Holder__clone`
/// <- `main`) while ALSO discarding the callee's write.
///
/// RED-VERIFIED against the pre-fix compiler: `SUMMARY: AddressSanitizer:
/// 5 byte(s) leaked in 1 allocation(s)`, exit 99. ggdef is structurally blind to
/// this class, so ASan is the instrument (Core #13). See the fixture header for
/// why the field is literal-seeded — a heap seed makes this RED post-fix for the
/// separately-filed "re-assign through a `&`-param drops nothing" leak.
#[test]
fn sound_amp_deref_box_field_leak_safe() {
    security_safe_no_leak("sound_amp_deref_box_field_leak", "gg-payload\n9");
}

/// KNOWN GAP — assigning to a `String` element inside `for … in &coll`
/// DOUBLE-FREES. `gg check` passes, `gg build` succeeds, the binary aborts with
/// "free(): double free detected in tcache 2".
///
/// The memory-unsafe cell of a type axis that is otherwise merely wrong or
/// correct, measured on the same loop shape: `int` element -> write LOST;
/// struct and `Vector[int]` elements -> write through CORRECTLY; `String`
/// element -> double free. So the reference's former blanket claim that
/// "element write-through through a loop iterable is lost" was wrong twice —
/// false for struct/Vector, and understating heap corruption as a lost write
/// for String (Core #8 on top of Core #12).
///
/// Asserts the INTENDED write-through (`x!`) running clean under ASan. If the
/// language instead rules whole-element assignment under `&`-iteration
/// inexpressible, replace this with the check-time reject — an accepted
/// program that double-frees is wrong under either reading.
#[test]
#[ignore = "KNOWN GAP: assigning a String element inside `for e in &coll` double-frees \
(gg check passes, binary aborts). Asserts the INTENDED write-through under ASan; TODO.md. \
Un-ignore when the element-assign releases the old buffer, or replace with the reject."]
fn sound_loop_string_elem_assign_double_free() {
    security_safe("sound_loop_string_elem_assign_double_free", "x!");
}

// ── Round MEMORY SAFETY / ONE OWNERSHIP BOUNDARY · Track B ──────────────
//
// View-tagged TEMP crossing a container mutator: the writer-side fix restores
// the missing borrow-detection predicate to `ensure_owned_at_consuming_arg`'s
// else arm (`src/ir/lowering/context.rs:2680`) so a View (Guard.get and
// family) temp is CLONED at the boundary, not memcpy'd as a shallow alias.
// The fixture set covers producer × destination-consumer × payload cells;
// pre-fix each was measured RED (exit 134 ASan double-free, or check-time
// PANIC at src/ir/lowering/mod.rs:2143 for cells whose consumer was
// Tier 2a Move-classified).

/// GRADUATED from known_gaps/ — Guard[String].get() → Dict.put · TEMP form.
/// Root of the view-into-consumer class; pre-fix exit 134.
#[test]
fn guard_get_into_dict_put_double_free_fixed() {
    security_safe("guard_get_into_dict_put_double_free", "1");
}

/// Guard[Vector[int]].get() → Dict.put · TEMP form · payload-type axis.
/// Pre-fix exit 134 (double-free on the 64-byte GorgetArray region).
#[test]
fn guard_get_vector_int_into_dict_put_temp_fixed() {
    security_safe("guard_get_vector_int_into_dict_put_temp", "1");
}

/// Guard[String].get() → Vector.push · TEMP form ·
/// destination-consumer axis. Pre-fix: `gg build` panics at
/// mod.rs:2143 (Vector.push is Tier 2a Move-classified so the
/// borrow-consumed shape is caught at check time).
#[test]
fn guard_get_into_vector_push_temp_fixed() {
    security_safe("guard_get_into_vector_push_temp_fixed", "1");
}

/// ReadGuard[String].get() → Dict.put · TEMP form · guard-family axis.
/// Pre-fix exit 134.
#[test]
fn read_guard_get_into_dict_put_double_free_fixed() {
    security_safe("read_guard_get_into_dict_put_double_free", "1");
}

/// WriteGuard[String].get() → Dict.put · TEMP form · guard-family axis.
/// Pre-fix exit 134.
#[test]
fn write_guard_get_into_dict_put_double_free_fixed() {
    security_safe("write_guard_get_into_dict_put_double_free", "1");
}

/// Guard[String].get() → Set.add · TEMP form · destination-consumer axis.
/// Pre-fix: `gg build` panics at mod.rs:2143 (Set.add is Tier 2a
/// Move-classified).
#[test]
fn guard_get_into_set_add_temp_fixed() {
    security_safe("guard_get_into_set_add_temp", "1");
}

/// Guard[String].get() → Channel.send · TEMP form ·
/// destination-consumer axis. Pre-fix exit 134.
#[test]
fn guard_get_into_channel_send_temp_fixed() {
    security_safe("guard_get_into_channel_send_temp", "1");
}

/// Guard[String].get() → index-assign `d[k] = v` · TEMP form ·
/// destination-consumer axis (index-assign sugar). Pre-fix exit 134.
#[test]
fn guard_get_into_index_set_temp_fixed() {
    security_safe("guard_get_into_index_set_temp", "1");
}

/// CONTROL PIN: `String s = g.get(); d.put("k", s)` NAMED-LOCAL form —
/// green before AND after Track B's fix. Pins the var-decl clone path
/// (`ensure_owned_at_boundary` at context.rs:2516 — the sibling helper
/// whose predicate has always been unconditional). Discriminates the
/// temp form (which needed the fix) from the named-local form (which
/// was correct all along).
#[test]
fn guard_get_named_local_into_dict_put_pin() {
    security_safe("guard_get_named_local_into_dict_put", "1");
}

// ── R47 Track B: indirect-dispatch call results are registered at their birth.
//
// Nine lowering arms minted a freshly-owned, droppable call result with a raw
// `builder.call` and registered it nowhere, so every such call leaked its
// result — once per call, i.e. UNBOUNDED inside a loop. All nine now route
// through `LoweringContext::call_indirect_tracked`.
//
// THE NET HAS TWO DIRECTIONS, because the fix could fail either way.
//
//   * `*_call_result_leak` — the UNDER-registration direction. Each is
//     RED-verified at pristine `f3feea79`, byte counts in the fixture headers.
//   * `*_transferred_no_double_free` — the OVER-registration direction: the
//     result is copied ONWARD into a second live slot, because both runtime
//     release paths self-null BY DESIGN (`gorget_string_free` zeroes the `Str`
//     after the dealloc, explicitly "so double-free is safe") and a second
//     release through the SAME slot is therefore a silent no-op. The three
//     `combinator_*` controls are RED-verified against the conversion WITHOUT
//     its compensating ownership transfer (ASan `attempting double-free`,
//     3/3 runs each); see each header for what the other six pin.
//
// Every one of these is C-lane only: `--sanitize` adds no instrumentation on
// the LLVM backend, so an "ASan-clean on both lanes" claim would be vacuous.

/// S1 — closure-STRUCT local (`__Closure_N__call`). RED at HEAD: 6 B / 1.
#[test]
fn indirect_closure_struct_call_result_no_leak() {
    security_safe_no_leak("indirect_closure_struct_call_result_leak", "hello");
}

/// S2 — `Callable[T]` PARAMETER (`__callable_N`). RED at HEAD: 6 B / 1.
#[test]
fn indirect_callable_param_call_result_no_leak() {
    security_safe_no_leak("indirect_callable_param_call_result_leak", "hello");
}

/// S3 — ESCAPED closure in an `FnPtr` local (`__gorget_closure_call_N`).
/// RED at HEAD: 6 B / 1.
#[test]
fn indirect_escaped_closure_call_result_no_leak() {
    security_safe_no_leak("indirect_escaped_closure_call_result_leak", "hello");
}

/// S4 — IIFE. RED at HEAD: 6 B / 1.
#[test]
fn indirect_iife_call_result_no_leak() {
    security_safe_no_leak("indirect_iife_call_result_leak", "hello");
}

/// S5 — `Box[Trait]` vtable dispatch. RED at HEAD: 4 B / 1.
#[test]
fn indirect_boxtrait_vtable_call_result_no_leak() {
    security_safe_no_leak("indirect_boxtrait_vtable_call_result_leak", "R2!");
}

/// S9 — EXPRESSION callee (`make()()`), the ninth arm, missing from the filed
/// set until this round. RED at HEAD: 6 B / 1.
#[test]
fn indirect_expr_callee_call_result_no_leak() {
    security_safe_no_leak("indirect_expr_callee_call_result_leak", "hello");
}

/// REPETITION axis — vtable dispatch in a loop. RED at HEAD: 20 B / **5**
/// allocations for 5 iterations. The unboundedness is the point.
#[test]
fn indirect_boxtrait_vtable_loop_unbounded_no_leak() {
    security_safe_no_leak("indirect_boxtrait_vtable_loop_unbounded_leak", "15");
}

/// REPETITION axis — escaped closure in a loop. RED at HEAD: 30 B / 5.
#[test]
fn indirect_escaped_closure_loop_unbounded_no_leak() {
    security_safe_no_leak("indirect_escaped_closure_loop_unbounded_leak", "25");
}

/// PAYLOAD-TYPE axis — the result is a `Vector[int]`, not a String. A fix that
/// registered only owned Strings would leave this red and every other cell
/// green. RED at HEAD: 64 B / 1.
#[test]
fn indirect_closure_vector_payload_no_leak() {
    security_safe_no_leak("indirect_closure_vector_payload_leak", "2");
}

/// RECEIVER-ROOT axis — the trait object lives in a struct field, not a bare
/// local. RED at HEAD: 4 B / 1.
#[test]
fn indirect_boxtrait_struct_field_root_no_leak() {
    security_safe_no_leak("indirect_boxtrait_struct_field_root_leak", "R2!");
}

/// NEGATIVE CONTROL — three statically-dispatched shapes, including the SAME
/// trait method on a CONCRETE receiver. Proves the discriminator is
/// indirection, not traits, and pins the conversion's blast radius.
#[test]
fn direct_dispatch_call_result_stays_clean() {
    security_safe_no_leak("direct_dispatch_call_result_no_leak", "hello\nt?\nR2!");
}

/// LIVE REGRESSION FIXTURE (graduated from `t0772`) — the MIRROR of the class
/// this block fixes. The combinator adapter CLONES its receiver and then used
/// to unregister the ORIGINAL from drop tracking anyway, so `o.map(...)` leaked
/// `o`'s payload: 6 B / 1 allocation, RED-verified against the pre-fix compiler
/// blob. The prologue now gates the receiver's unregister on the adapter's own
/// `receiver_was_cloned` decision — the authoritative fact, not a second
/// opinion from `LocalOwnership` that could disagree with the emitted code.
#[test]
fn option_map_clones_receiver_then_unregisters_it() {
    security_safe_no_leak(
        "option_map_clones_receiver_then_unregisters_it_leak",
        "hello!",
    );
}

/// The WIDE net for that same class: every combinator arm on a NAMED receiver
/// with a heap-forced payload, across the receiver-liveness axis (live after /
/// dead after), the shape axis (named local, chained, in a loop, borrowed
/// param, Vector payload) and both receivers. RED pre-fix at 334 bytes leaked
/// in 18 allocations; clean after, with no double-free and no use-after-free —
/// a combinator receiver can never carry a move sigil, so restoring its drop
/// registration cannot race a consumer.
///
/// ⚠ It does NOT cover the INLINE-CONSTRUCTOR receiver (`Some(mk(..)).map(f)`),
/// which leaks 6 B before AND after with byte-identical generated C: that temp
/// was never registered at all (`t0872`, a different layer). That residual's
/// durable repro is `inline_ctor_temp_not_registered_leak.gg`, wired ignored
/// below as `known_gap_inline_ctor_temp_not_registered`.
#[test]
fn combinator_named_receiver_stays_leak_free() {
    security_safe_no_leak(
        "combinator_named_receiver_no_leak",
        "\
hello!
hello
dead!
lit*
and?
flat#
orelse
filt
uoe
res!
bad!^
err^
chain!!
loop!
loop!
loop!
param!
2",
    );
}

/// KNOWN GAP `t0872` — an inline-constructor combinator receiver
/// (`Some(mk(..)).map(f)`) and a plain struct-ctor field read
/// (`Node(mk(..)).name`) each leak 6 B: the temp is never registered for drop.
/// The `.unwrap()` control is clean, so ctor temps ARE normally registered;
/// the gap is on the adapter / field-read path. Heap-forced payload. Not
/// fixed here — this test asserts the INTENDED ASan-clean state.
#[test]
#[ignore = "KNOWN GAP t0872: inline-ctor combinator receiver and struct-ctor \
field read leak 6 B each; temp never registered for drop"]
fn known_gap_inline_ctor_temp_not_registered() {
    security_safe_no_leak(
        "inline_ctor_temp_not_registered_leak",
        "\
hello!
hello",
    );
}

/// KNOWN GAP `t0880` — `Result[T, E].unwrap_or(<heap temp>)` leaks the temp
/// (5 B / 1, from `gorget_string_copy_cow`) while the `Option[T]` sibling of
/// the SAME method is clean. Same method name, same argument shape, two
/// receivers, opposite results — so the temp IS normally registered at this
/// position and the Result arm specifically drops the registration.
/// Pre-existing: measured identical before and after the
/// `receiver_was_cloned` gate (`t0772`), which is about the RECEIVER's
/// registration, not an ARGUMENT's. Found while widening that fix's ASan net.
#[test]
#[ignore = "KNOWN GAP t0880: Result.unwrap_or leaks a heap default argument \
(5 B / 1); the Option sibling is clean"]
fn known_gap_result_unwrap_or_heap_default_leak() {
    security_safe_no_leak("result_unwrap_or_heap_default_leak", "res");
}

/// The ASan-armed twin of `tests/fixtures/print_trait_object.gg`, which had no
/// gap test to graduate: it was wired as a stdout comparison only, so it stayed
/// green for rounds while leaking. RED at HEAD: 5 B / 1 — the figure filed
/// against it (61 B / 2) had decayed and is refuted.
#[test]
fn print_trait_object_stays_leak_free() {
    security_safe_no_leak(
        "print_trait_object_no_leak",
        "gear\n7\n3.140000\ntrue",
    );
}

/// POSITIVE CONTROL, adapter closure-struct branch. RED (ASan double-free,
/// 3/3) against the conversion with its ownership transfer removed.
#[test]
fn combinator_closure_result_transferred_stays_clean() {
    security_safe_no_leak("combinator_closure_result_transferred_no_double_free", "hello");
}

/// POSITIVE CONTROL, adapter `Callable`-parameter branch. RED (ASan
/// double-free, 3/3) against the conversion with its transfer removed.
#[test]
fn combinator_callable_param_result_transferred_stays_clean() {
    security_safe_no_leak(
        "combinator_callable_param_result_transferred_no_double_free",
        "hello",
    );
}

/// POSITIVE CONTROL, adapter `FuncRef` branch — a STATICALLY NAMED callee that
/// is nonetheless in the class. RED (ASan double-free, 3/3) against the
/// conversion with its transfer removed.
#[test]
fn combinator_funcref_result_transferred_stays_clean() {
    security_safe_no_leak("combinator_funcref_result_transferred_no_double_free", "hello");
}

/// POSITIVE CONTROL, S1 — result transferred into a container that outlives it.
#[test]
fn indirect_closure_struct_result_transferred_stays_clean() {
    security_safe_no_leak(
        "indirect_closure_struct_result_transferred_no_double_free",
        "2\nhello\nhello",
    );
}

/// POSITIVE CONTROL, S2 — result transferred into a container that outlives it.
#[test]
fn indirect_callable_param_result_transferred_stays_clean() {
    security_safe_no_leak(
        "indirect_callable_param_result_transferred_no_double_free",
        "2\nhello\nhello",
    );
}

/// POSITIVE CONTROL, S3 — result transferred into a container that outlives it.
#[test]
fn indirect_escaped_closure_result_transferred_stays_clean() {
    security_safe_no_leak(
        "indirect_escaped_closure_result_transferred_no_double_free",
        "2\nhello\nhello",
    );
}

/// POSITIVE CONTROL, S4 — result transferred into a container that outlives it.
#[test]
fn indirect_iife_result_transferred_stays_clean() {
    security_safe_no_leak(
        "indirect_iife_result_transferred_no_double_free",
        "2\nhello\nworld",
    );
}

/// POSITIVE CONTROL, S5 — result transferred into a container that outlives it.
#[test]
fn indirect_boxtrait_vtable_result_transferred_stays_clean() {
    security_safe_no_leak(
        "indirect_boxtrait_vtable_result_transferred_no_double_free",
        "2\nR2!\nR2!",
    );
}

/// POSITIVE CONTROL, S9 — result transferred into a container that outlives it.
#[test]
fn indirect_expr_callee_result_transferred_stays_clean() {
    security_safe_no_leak(
        "indirect_expr_callee_result_transferred_no_double_free",
        "2\nhello\nhello",
    );
}


// ── R47 Track E2 — for-loop producers owe BOTH ownership axes ──────────────
//
// `src/ir/lowering/stmts/for_loops.rs` mints values in a dozen arms, and each
// owes an ownership TAG and a drop REGISTRATION. Three arms had one axis or
// neither, and each miss has its own signature:
//
//   * neither decided → the value leaks, and a leak has NO stdout signature,
//     so `iterable.gg` / `vector_iter_userdef.gg` / `iterator_direct.gg` were
//     green the whole time the arm leaked both its iterator and its element.
//     Only a `detect_leaks=1` run can see this class, which is why every cell
//     below is judged on the sanitized run.
//   * drop registered, ownership NOT tagged → the local stays `Untracked`,
//     the Tier 2a consume-site validator refuses to pick move-vs-clone, and
//     `for s in some_set: dst.add(s)` ABORTED the compiler.
//   * the direct-Iterator branch iterated a shallow COPY of the source, so
//     `next()`'s mutations were lost and the original's drop ran against
//     pre-iteration state — a double-free the moment the body moved an
//     element out.
//
// Axes covered: producer cell {iterator object (owned branch) · iterator
// source (borrow branch) · element binding · Set/Dict out-param arms} ×
// element ownership {Copy(int) · String · resource struct} × iterator source
// {Iterable-with-iter() · direct-Iterator · Set · Dict} × body disposition
// {read-only · move-out · early break}.
//
// OMITTED CELLS, named: (a) Copy element × move-out — degenerate, an `int`
// has no drop to get wrong; (b) direct-Iterator × resource-struct element —
// the element binding is ONE site (`enum_field_load_move`) shared by every
// iterator source, so the element-type axis is exercised where it varies, on
// the Iterable source; (c) LLVM lane — ASan evidence is C-lane only by
// construction, and the stdout halves of these fixtures are backend-agnostic
// GIR-level behaviour.
//
// Every fixture below was RED-verified against the pre-fix compiler.

/// Cell 1, ISOLATED: the iterator object `iter(&collection)` mints. `int`
/// elements hold the element cell inert, so the only leak this can report is
/// the iterator itself. Pre-fix: 146 bytes in 3 allocations, 3/3 runs.
#[test]
fn foriter_iterobj_int_elem_no_leak() {
    security_safe_no_leak("foriter_iterobj_int_elem_leak", "3");
}

/// Cell 1 with an early `break` — pins the iterator's drop SCOPE. The
/// iterator is minted before the loop and read by `next()` every iteration,
/// so it belongs to the ENCLOSING scope; registering it into the loop-body
/// scope frees it after iteration 1 and every later `next()` is a UAF, which
/// one iteration cannot show. Pre-fix: 146 bytes in 3 allocations, 3/3 runs.
#[test]
fn foriter_iterobj_break_no_leak() {
    security_safe_no_leak("foriter_iterobj_break_leak", "3");
}

/// Cell 2, ISOLATED: the loop binding moved out of the `Option` `next()`
/// returns, with a scalar-only iterator so cell 1 is inert. Pre-fix: 39 bytes
/// in 3 allocations (one per iteration), 3/3 runs.
#[test]
fn foriter_elem_string_no_leak() {
    security_safe_no_leak("foriter_elem_string_leak", "36");
}

/// Cell 2 on the third value of the element-type axis — a resource STRUCT
/// element, so the drop that must run is the generated destructor rather than
/// a plain string free. Pre-fix: 45 bytes in 3 allocations, 3/3 runs.
#[test]
fn foriter_elem_struct_no_leak() {
    security_safe_no_leak("foriter_elem_struct_leak", "42");
}

/// Both cells at once with a MOVE-OUT body — the bidirectional cell.
/// Registering the element is what flips the consume site from clone to move,
/// so this is red pre-fix (196 bytes in 7 allocations, 3/3 runs) AND red
/// against an over-eager fix that frees the element the set now owns.
#[test]
fn foriter_move_out_to_set_no_leak() {
    security_safe_no_leak("foriter_move_out_to_set", "3");
}

/// Cell 1's BORROW branch — direct-Iterator, move-out body, early break.
/// Pre-fix this was LSan-clean and silently WRONG: the loop popped from a
/// shallow copy, so the source still reported all 4 elements (`2\n4`) while
/// the set held elements the source also believed it owned. That is the
/// double-free `stdlib_iter_drain` hit the moment the element cell was
/// registered — see the C-lane control in the integration suite.
#[test]
fn foriter_direct_drain_move_out_no_leak() {
    security_safe_no_leak("foriter_direct_drain_move_out", "2\n2");
}

/// The same borrow-branch defect through its observable side: after two
/// elements and a `break`, the iterator's own `idx` must read 2. Pre-fix it
/// read 0 — the loop had advanced a copy. `lib/std/iter.gg`'s `VectorDrain`
/// `Drop` ("reverses the remaining buffer back if the caller breaks early")
/// is only meaningful if the advanced object is the dropped one.
#[test]
fn foriter_direct_advances_source() {
    security_safe_no_leak("foriter_direct_advances_source", "0\n1\n2");
}

/// Set arm sibling: `for s in src: dst.add(s)`. The out-param accessor hands
/// back an independent owned clone, and the arm registered its drop without
/// deciding ownership — so pre-fix this ABORTED the compiler with the Tier 2a
/// "untracked source consumed (ownership not decided)" violation.
#[test]
fn forset_move_out_elem_no_leak() {
    security_safe_no_leak("forset_move_out_elem", "3\n3");
}

/// Dict arm siblings — the `for k, v in d` destructure and the key-only
/// `for k in d` form are separate arms with separate bindings, and both had
/// the same missing ownership decision. Pre-fix: compiler abort, 2 violations.
#[test]
fn fordict_move_out_kv_no_leak() {
    security_safe_no_leak("fordict_move_out_kv", "2\n2\n2");
}

/// REGRESSION (was `todo/t0840`, R48 Track D2) on the SANITIZER lane. The
/// value lanes saw only a SEGV; this lane sees the memory story — the push
/// stored the handle VALUE where an ADDRESS was required, so the runtime
/// memcpy'd out of the refcount control block, and the read then handed the
/// slot's ADDRESS to a by-VALUE accessor. A value lane is structurally blind
/// to that distinction (Core #13), which is why the class owes a fixture on
/// BOTH.
///
/// What only this lane can pin: the refcount arithmetic BALANCES — three
/// `.clone()` increfs against three element drops from `gorget_array_free`
/// plus the original's — so a repair that wrote the slot correctly but leaked
/// or over-released a reference would still redden here.
#[test]
fn shared_vector_of_clones_push_marshalling_asan() {
    security_safe_no_leak("shared_vector_of_clones_push_marshalling", "3\n42\n4");
}

// R48 Track D2 - the sanitizer half of the handle net. Six of this track's
// findings came out of the sanitize sweep and NONE was visible to a
// stdout-comparing fixture: an over-release that still printed the right
// answer, a 32-byte control block nobody owned, a double free that only
// appeared once the element slot held a real handle. One ASan pin on the
// headline cell was too thin a net for that.

#[test]
fn mutex_vector_single_owner_move_asan() {
    security_rejected("mutex_vector_single_owner_move", "E_MoveWithoutOperator");
}

#[test]
fn rwlock_vector_single_owner_move_asan() {
    security_rejected("rwlock_vector_single_owner_move", "E_MoveWithoutOperator");
}

#[test]
fn mutex_vector_live_source_single_owner_asan() {
    security_rejected("mutex_vector_live_source_single_owner", "E_MoveWithoutOperator");
}

#[test]
fn rwlock_vector_live_source_single_owner_asan() {
    security_rejected("rwlock_vector_live_source_single_owner", "E_MoveWithoutOperator");
}

#[test]
fn mutex_vector_two_slots_named_push_reject_asan() {
    security_rejected("mutex_vector_two_slots_double_free", "E_MoveWithoutOperator");
}

#[test]
fn rwlock_vector_nslot_named_push_reject_asan() {
    security_rejected("rwlock_nslot_named_push_reject", "E_MoveWithoutOperator");
}

#[test]
fn mutex_vector_d53_class_pin_guard_push_reject_asan() {
    security_rejected("guard_named_push_reject", "E_MoveWithoutOperator");
}

#[test]
fn mutex_vector_d53_pos_temp_caret_nonconsuming_asan() {
    // `mutex_temp_push` is the legal D53 spelling; its 136B Mutex-ctor leak is
    // the pre-existing `todo/t0623` lock-object leak (`mutex_basic` same size),
    // not a consume-position defect — pin it with `security_safe` (UAF-visible)
    // rather than `no_leak`.
    security_safe("mutex_temp_push", "1\n0");
    security_safe_no_leak("rwlock_temp_push", "1\n0");
    security_safe_no_leak("mutex_caret_push", "1\n5");
    security_safe_no_leak("rwlock_caret_push", "1\n5");
    security_safe_no_leak("mutex_nonconsuming_call", "5\n5");
    security_safe_no_leak("rwlock_nonconsuming_call", "5\n5");
}

/// Refcount arithmetic: five releases against four retains printed the right
/// answer on both value lanes and hung or tripped a pthread assertion instead
/// of reporting itself. Only this lane names it.
#[test]
fn channel_clone_by_value_incref_asan() {
    security_safe_no_leak("channel_clone_by_value_incref", "7\n1\n9");
}

#[test]
fn channel_clone_consuming_positions_asan() {
    security_safe_no_leak("channel_clone_consuming_positions", "1\n2\n3\n4\ndone");
}

/// The cell that surfaced the index-read leak: a refcount element read as a
/// VALUE took the element-clone path and nobody owned the retain. Stdout was
/// correct throughout.
#[test]
fn shared_vector_element_read_shapes_asan() {
    security_safe_no_leak("shared_vector_element_read_shapes", "4\n12\n4\n2");
}

#[test]
fn shared_vector_producing_arms_asan() {
    security_safe_no_leak("shared_vector_producing_arms", "2\n1\n2\n2\n1");
}

/// KNOWN GAP (`todo/t0907`) on the SANITIZER lane, and ONLY this lane can see
/// it: `for (k, v) in d` over a `Dict[String, Shared[int]]` prints the RIGHT
/// answer on both value lanes while over-releasing the value bind, so
/// `gorget_map_free` walks its second entry into an already-freed control
/// block. The identical loop over `String` values is clean, and so is
/// `for h in v` over a `Vector[Shared[int]]` — the Vector element bind borrows
/// and never copies. The ROOT is shared with the nested-clone cell:
/// `gorget_map_iter_value` calls the same `val_clone` in-place hook that
/// `gorget_array_clone` calls as `elem_clone`, and it is NULL for a refcount
/// element, so ONE fix closes both faces.
#[test]
#[ignore = "known gap (todo/t0907): Dict value-bind iteration over refcount values over-releases — heap-use-after-free in gorget_map_free"]
fn known_gap_shared_dict_iteration_value_over_release_asan() {
    security_safe_no_leak("shared_dict_iteration_value_over_release", "4");
}

/// KNOWN GAP (`todo/t0907`) on the SANITIZER lane. Cloning a
/// `Vector[Shared[int]]` does not incref its elements: `gorget_array_clone`
/// consults `elem_clone`, a refcount element type carries `clone_fn` but no
/// `clone_inplace_fn`, so the two arrays alias the same handles with the
/// refcount never raised and both drop. The `String`-element twin is clean.
#[test]
#[ignore = "known gap (todo/t0907): cloning a Vector[Shared[T]] does not incref its elements — heap-use-after-free under two nested gorget_array_free frames"]
fn known_gap_shared_nested_vector_clone_over_release_asan() {
    security_safe_no_leak("shared_nested_vector_clone_over_release", "1\n6");
}

/// KNOWN GAP (`todo/t0108`) on the SANITIZER lane, in its `Channel` costume.
/// `needs_param_drop` keys on TYPE alone, so a BARE refcount-handle parameter —
/// a borrow under CoW-default-borrow — is dropped by the CALLEE at scope exit
/// and the caller's handle goes to zero underneath it. There is no `.clone()`
/// anywhere in the program, which is what discriminates it from the R48
/// Track D2 clone-convention class.
#[test]
#[ignore = "known gap (todo/t0108): a bare refcount-handle PARAM is dropped by the callee — heap-use-after-free in gorget_channel_release"]
fn known_gap_channel_bare_param_callee_drop_uaf_asan() {
    security_safe_no_leak("channel_bare_param_callee_drop_uaf", "in\n1");
}

/// REGRESSION (`todo/t0841`, R48 Track D1) on the SANITIZER lane. RED at
/// pre-fix HEAD: `AddressSanitizer: attempting double-free` in
/// `__gorget_global_dealloc_fn` ← `gorget_string_free` ← `gorget_array_free` ←
/// `main` — three aliases of one heap buffer, freed once each by the vector.
///
/// A comprehension IS a loop, and its three liveness arms were single-pass
/// while the statement-loop arms did the back-edge two-pass dance, so the
/// loop-invariant owned name was MOVED on every iteration instead of cloned.
/// All seven loop-shaped arms now share one helper (Core #4).
///
/// BOTH LANES ARE NEEDED: the value twin is
/// `cow_comprehension_invariant_owned_name` in `tests/integration.rs`, and it
/// could not have caught this alone — the program printed the right answer
/// before aborting.
#[test]
fn cow_comprehension_invariant_owned_name_asan() {
    security_safe_no_leak("cow_comprehension_invariant_owned_name", "ababab");
}

/// KNOWN GAP (`todo/t0108`) on the SANITIZER lane: `heap-use-after-free` in
/// `gorget_shared_get_ptr`, freed by `gorget_shared_drop` ← `Shared__int64_t__drop`
/// ← the CALLEE. The free chain naming the callee is the whole diagnosis, and it
/// exists only on this lane.
#[test]
#[ignore = "known gap (t0108): a bare Shared param is a borrow but the callee decrefs it at scope exit"]
fn known_gap_shared_plain_call_param_uaf_asan() {
    security_safe_no_leak("known_gap_shared_plain_call_param_uaf", "42\n42");
}

// ══════════════════════════════════════════════════════════════════════════
// THE PER-FUNCTION-BODY PRESCAN NET — the SANITIZE axis
// ══════════════════════════════════════════════════════════════════════════
//
// The value lanes (tests/integration.rs) see rc 139; this lane sees WHY —
// `AddressSanitizer: heap-use-after-free`, with the freeing `gorget_array_push`
// realloc naming the mechanism. Core #13: pick an instrument that can SEE the
// failure class. A CoW element view is a pointer INTO the collection's buffer,
// so a reallocating push frees it; only ASan distinguishes "read freed memory"
// from "read garbage that happened to look right".
//
// One cell per function-body-lowering PATH, mirroring the integration net.
// Each was RED-verified against the pre-fix compiler: rc 139 on C AND LLVM,
// ASan heap-use-after-free under `--sanitize`.

/// PATH CELL — `lower_equip_method_with_subs` (a `&self` mutator on a GENERIC
/// equip). Graduated from `tests/fixtures/known_gaps/`.
#[test]
fn cow_generic_equip_mutator_view_uaf_safe() {
    security_safe("cow_generic_equip_mutator_view_uaf", "helloworld");
}

/// PATH CELL — `lower_generic_function` (a plain generic FREE function).
#[test]
fn cow_generic_fn_view_survives_realloc_safe() {
    security_safe("cow_generic_fn_view_survives_realloc", "helloworld");
}

/// PATH CELL — `lower_trait_method_body` (a trait DEFAULT method).
#[test]
fn cow_trait_default_view_survives_realloc_safe() {
    security_safe("cow_trait_default_view_survives_realloc", "helloworld");
}

/// PATH CELL — `lower_static_trait_method` (a static trait method, no `self`).
#[test]
fn cow_static_trait_method_view_survives_realloc_safe() {
    security_safe("cow_static_trait_method_view_survives_realloc", "helloworld");
}

/// PATH CELL — `emit_closure_call_function` (the closure body, whose AST is a
/// bare `Spanned<Expr>`). The vector is LOCAL to the closure: the CAPTURED
/// sibling is `todo/t0704`, a different defect at the capture boundary.
#[test]
fn cow_closure_body_view_survives_realloc_safe() {
    security_safe("cow_closure_body_view_survives_realloc", "helloworld");
}

/// LANE CELL — the generic-equip `&self` mutator on a NAMED bare-value-param
/// receiver. The value lanes assert `Y / A`; this lane asserts nobody wrote
/// through a freed or aliased buffer to get there.
#[test]
fn cow_generic_equip_named_recv_safe() {
    security_safe("cow_generic_equip_named_recv", "Y\nA");
}

/// CLASS GUARD — a closure with heap-forced resource captures, called TWICE,
/// each capture forwarded into a CONSUMING position inside the body. The env
/// owns the captured data ACROSS calls, so a consuming position must COPY. If
/// a future change to capture ownership lets it MOVE instead, the second call
/// reads a move-zeroed slot and the env's drop double-frees — and BOTH can
/// still print the right bytes, which is precisely why the value lanes are not
/// enough here. Run under `detect_leaks=1` so the clone-per-call is also held
/// to not leaking. See the fixture header for the four mechanisms measured to
/// hold this up today, and why no single-line RED stub exists at HEAD.
#[test]
fn closure_capture_called_twice_no_leak() {
    security_safe_no_leak(
        "closure_capture_called_twice",
        "helloworld/alphabeta\nhelloworld/alphabeta\nhelloworld\nalphabeta",
    );
}
