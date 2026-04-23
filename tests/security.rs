//! Security test harness for Gorget.
//!
//! Each fixture under `tests/fixtures/security/` is one adversarial program
//! designed to probe a specific memory-safety or correctness guarantee.
//! Fixtures are classified by helper:
//!
//! - [`security_safe`]       — a well-typed program that must build under
//!                             `--sanitize` and run exit-0 with expected stdout.
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
//!
//! When a known-unsafe bug is fixed, the test will start failing — that's
//! the signal to reclassify as `security_safe` / `security_rejected` /
//! `security_traps` and relabel the fixture.
//!
//! To run just the security suite:
//!     cargo test --test security
//!
//! The full build includes `-fsanitize=address,undefined` via the
//! compiler's own `--sanitize` flag.

use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;

const BUILD_TIMEOUT: Duration = Duration::from_secs(180);

fn test_binary_timeout() -> Duration {
    Duration::from_secs(
        std::env::var("GG_TEST_TIMEOUT_SECS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(30),
    )
}

fn gg_command(subcommand: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO"));
    cmd.args(["run", "--quiet", "--", subcommand]);
    cmd
}

fn run_with_deadline(cmd: &mut Command, fixture: &str, timeout: Duration) -> std::process::Output {
    let mut child = cmd
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute compiled binary");

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

    let deadline = std::time::Instant::now() + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {
                if std::time::Instant::now() >= deadline {
                    child.kill().ok();
                    child.wait().ok();
                    panic!("Process for {fixture} timed out after {}s", timeout.as_secs());
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(e) => panic!("Failed to wait on child for {fixture}: {e}"),
        }
    };

    let stdout = stdout_thread.join().unwrap_or_default();
    let stderr = stderr_thread.join().unwrap_or_default();

    std::process::Output { status, stdout, stderr }
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

fn sanitize_build_and_run(fixture_name: &str) -> (SanitizeOutcome, PathBuf) {
    let fp = fixture_path(fixture_name);
    let stem = fp.file_stem().unwrap().to_str().unwrap();
    let dir = fp.parent().unwrap();
    let exe_path = dir.join(stem);

    let build = run_with_deadline(
        gg_command("build").arg("--sanitize").arg(&fp),
        fixture_name,
        BUILD_TIMEOUT,
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
    run_cmd.env(
        "ASAN_OPTIONS",
        "detect_leaks=0:halt_on_error=1:abort_on_error=0:print_summary=1",
    );
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
    let output = run_with_deadline(gg_command("check").arg(&fp), name, BUILD_TIMEOUT);
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

// ── Accepted well-typed programs that must run safely under --sanitize ──

#[test]
fn sec_03_cow_mutate_while_borrowed() {
    security_safe("attack_03_cow_mutate_while_borrowed", "alpha\ndelta");
}

#[test]
fn sec_04_cow_mutate_ref_borrow() {
    security_safe("attack_04_cow_mutate_ref_borrow", "10");
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
fn sec_22_option_shared_drop_mistype() {
    security_known_unsafe(
        "attack_22_weak_after_shared_dropped",
        KnownBug::BuildFails,
        "Monomorphized Option[Shared[T]]::drop has a void**/GorgetShared** pointer-type \
         mismatch plus a dangling `int64_t__new` reference. C won't link.",
    );
}

#[test]
fn sec_23_dead_div_zero_dce() {
    security_known_unsafe(
        "attack_23_panic_during_init",
        KnownBug::SilentlyProduces("10"),
        "`int boom = 10 / z` (z==0) gets DCE'd when boom is unused; runtime never traps. \
         Hides real bugs from users.",
    );
}

#[test]
fn sec_26_mod_zero_traps() {
    security_traps("attack_26_mod_zero", "division by zero");
}

#[test]
fn sec_12_vector_iter_resource_panic() {
    security_known_unsafe(
        "attack_12_vector_iter_resource",
        KnownBug::BuildFails,
        "`for s in v.iter():` over Vector[String] (resource-typed T) crashes the C backend \
         at emit_types.rs:965 with `Ptr ABI received scalar value`. Compiler DoS on valid \
         source code. (TODO Low: VectorIter[T] Ptr-ABI codegen panic in for-loops)",
    );
}

#[test]
fn sec_28_cow_option_extraction_minimal() {
    // Was SanitizerTrips. Fixed 2026-04-23 together with sec_17 — the
    // minimal 4-line repro of Option[Ref[T]] → Option[T] now builds and
    // runs cleanly, printing the actual element.
    security_safe("attack_28_cow_option_extraction", "alpha");
}
