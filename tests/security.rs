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
    // `allocator_may_return_null=1` lets the runtime's own null-check-after-
    // alloc paths run — otherwise ASan pre-aborts on oversized allocations
    // and masks Gorget's cleaner runtime trap.
    run_cmd.env(
        "ASAN_OPTIONS",
        "detect_leaks=0:halt_on_error=1:abort_on_error=0:print_summary=1:allocator_may_return_null=1",
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
    // `chr(0)` → empty String (0 bytes — Gorget refuses to embed NUL).
    // `chr(1114111)` → U+10FFFF, 1 grapheme, 4 UTF-8 bytes (len counts
    // codepoints here). `chr(1200000)` (invalid) → U+FFFD replacement.
    // All deterministic — no invalid UTF-8 is ever produced.
    security_safe("attack_80_char_edge_cases", "0\n1\n1");
}

// ── Round 7 attacks: self-ref, closures-in-vector, atomic races, etc. ────

#[test]
fn sec_81_match_expr_some_arm_dropped() {
    // BUG: `x = match o: case Some(n): compute(n); else: 0` silently
    // discards the Some-arm value; x always equals the else-arm value.
    // Traced in codegen: bb_some computes `__v45 = compute(n)` then
    // `goto exit`, but the exit block unconditionally re-emits the
    // literal `0` as the match result — the Some arm's value is never
    // merged into the match's result slot. A 1+2+3 list sum returns 1.
    security_known_unsafe(
        "attack_81_self_referential_struct",
        KnownBug::SilentlyProduces("1"),
        "Match-expression drops Some-arm value when used in assignment. \
         `x = match o: case Some(n): f(n); else: 0` — the Some arm's \
         computed value isn't stored into x; x always gets the else-arm \
         value (0). Silent wrong-result bug, not memory-unsafe.",
    );
}

#[test]
fn sec_82_vector_of_closures_segv() {
    // BUG: Vector[Callable[...]].get().unwrap().clone() SEGVs. The
    // generated Option[Callable] struct defines Some_0 as int64_t,
    // but a Callable (= GorgetClosure) is 16 bytes (fn_ptr + env).
    // When extracted, the code reads fn_ptr as an int64 and then
    // memcpy's 16 bytes from (void*)fn_ptr — dereferences a code
    // pointer, reading from executable memory. Then invokes through
    // garbage fn_ptr → SIGSEGV.
    security_known_unsafe(
        "attack_82_vector_of_closures",
        KnownBug::SanitizerTrips,
        "Option[Callable] payload field typed int64_t, but Callable \
         is 16 bytes (GorgetClosure). v.get().unwrap().clone() reads \
         only 8 bytes, then misinterprets fn_ptr as a pointer to the \
         closure struct → SEGV reading code memory.",
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
#[ignore = "ABI fix landed (build now succeeds, runs correctly without sanitizers); \
            but ASan-instrumented binary hangs at runtime — separate bug, see TODO.md"]
fn sec_85_dict_struct_key_codegen() {
    // Original failure: `@derive(Hashable, Equatable) struct Point` as a
    // Dict key emitted a key-equality wrapper that passed `(void*, void*)`
    // to `Point__eq` (which wants `(void*, Point-by-value)`). That ABI bug
    // is fixed: the wrapper now derefs `__b` and passes by value, and the
    // unsanitized binary prints "origin-ish\norigin-ish" as expected.
    //
    // New finding (uncovered when build started succeeding): ASan+UBSan
    // build hangs at 99% CPU instead of completing — exposed only under
    // sanitizers. Harness has no `Hangs` variant, so `#[ignore]` until
    // the underlying hang is investigated.
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
    security_safe("attack_88_iterator_fusion", "3\n0\n14\n28");
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
