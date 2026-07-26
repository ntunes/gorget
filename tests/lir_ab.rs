//! LIR A/B test: compile fixtures through both the GIR C backend and the
//! LIR→C backend, then compare their stdout.
//!
//! Only tests fixtures known to produce matching output through both pipelines.
//! As the LIR backend matures, more fixtures are added to this list.
//!
//! Both builders invoke the PRE-BUILT `gg` via `CARGO_BIN_EXE_gg`, never
//! `cargo run`. `cargo run` serialises every one of these ~700 tests on the
//! cargo build lock, which under `--test-threads=4` surfaced as a build that
//! "failed" for no reason the fixture could explain (`lir_ab_coroutine_struct
//! _methods`, which passes in isolation and builds fine by hand). This is the
//! same fix `integration.rs` already carries — see its `CARGO_BIN_EXE_gg` note.

use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;

/// Build + run through the GIR C backend (normal path).
/// Uses a `_gir` suffix to avoid colliding with `integration.rs` which
/// builds to the bare stem path.
fn run_gir(fixture_path: &std::path::Path) -> Option<String> {
    let stem = fixture_path.file_stem()?.to_str()?;
    let dir = fixture_path.parent()?;
    let exe_path = dir.join(format!("{stem}_gir"));
    let c_path = dir.join(format!("{stem}_gir.c"));

    let build = Command::new(env!("CARGO_BIN_EXE_gg"))
        .args(["build", "-o"])
        .arg(&exe_path)
        .arg(fixture_path)
        .output()
        .ok()?;
    if !build.status.success() {
        return None;
    }

    let run = run_with_timeout(&exe_path, Duration::from_secs(30))?;
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();

    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&c_path);

    Some(stdout)
}

/// Run an executable with a timeout. Returns None if it times out.
fn run_with_timeout(exe: &std::path::Path, timeout: Duration) -> Option<std::process::Output> {
    let mut child = Command::new(exe)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;
    let _id = child.id();
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        std::thread::sleep(timeout);
        let _ = tx.send(());
    });
    // Poll: try wait in a loop with small sleeps
    loop {
        match child.try_wait() {
            Ok(Some(_status)) => {
                return child.wait_with_output().ok();
            }
            Ok(None) => {
                if rx.try_recv().is_ok() {
                    // Timeout reached — kill the process
                    let _ = child.kill();
                    let _ = child.wait();
                    return None;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(_) => return None,
        }
    }
}

/// Build through LIR→C backend (full pipeline including runtime), run, return stdout.
fn run_lir(fixture_path: &std::path::Path) -> Option<String> {
    let stem = fixture_path.file_stem()?.to_str()?;
    let dir = fixture_path.parent()?;
    let exe_path = dir.join(format!("{stem}_lir"));
    let c_path = dir.join(format!("{stem}_lir.c"));

    let build = Command::new(env!("CARGO_BIN_EXE_gg"))
        .args(["build", "--backend=lir", "-o"])
        .arg(&exe_path)
        .arg(fixture_path)
        .output()
        .ok()?;
    if !build.status.success() {
        return None;
    }

    let run = run_with_timeout(&exe_path, Duration::from_secs(30))?;
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();

    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&c_path);

    Some(stdout)
}

/// Build + run through the GIR C backend, passing args to the binary.
fn run_gir_with_args(fixture_path: &std::path::Path, args: &[&str]) -> Option<String> {
    let stem = fixture_path.file_stem()?.to_str()?;
    let dir = fixture_path.parent()?;
    let exe_path = dir.join(format!("{stem}_gir"));
    let c_path = dir.join(format!("{stem}_gir.c"));

    let build = Command::new(env!("CARGO_BIN_EXE_gg"))
        .args(["build", "-o"])
        .arg(&exe_path)
        .arg(fixture_path)
        .output()
        .ok()?;
    if !build.status.success() {
        return None;
    }

    let child = Command::new(&exe_path)
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;
    let run = child.wait_with_output().ok()?;
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();

    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&c_path);

    Some(stdout)
}

/// Build through LIR→C backend, passing args to the binary.
fn run_lir_with_args(fixture_path: &std::path::Path, args: &[&str]) -> Option<String> {
    let stem = fixture_path.file_stem()?.to_str()?;
    let dir = fixture_path.parent()?;
    let exe_path = dir.join(format!("{stem}_lir"));
    let c_path = dir.join(format!("{stem}_lir.c"));

    let build = Command::new(env!("CARGO_BIN_EXE_gg"))
        .args(["build", "--backend=lir", "-o"])
        .arg(&exe_path)
        .arg(fixture_path)
        .output()
        .ok()?;
    if !build.status.success() {
        return None;
    }

    let child = Command::new(&exe_path)
        .args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .ok()?;
    let run = child.wait_with_output().ok()?;
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();

    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&c_path);

    Some(stdout)
}

fn ab_test_with_args(fixture: &str, args: &[&str]) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);
    assert!(fixture_path.exists(), "Fixture not found: {}", fixture_path.display());

    let gir_out = run_gir_with_args(&fixture_path, args)
        .unwrap_or_else(|| panic!("GIR backend failed for {fixture}"));
    let lir_out = run_lir_with_args(&fixture_path, args)
        .unwrap_or_else(|| panic!("LIR backend failed for {fixture}"));

    assert_eq!(
        gir_out.trim(),
        lir_out.trim(),
        "A/B mismatch for {fixture}:\n  GIR: {:?}\n  LIR: {:?}",
        gir_out.trim(),
        lir_out.trim(),
    );
}

fn ab_test(fixture: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);
    assert!(fixture_path.exists(), "Fixture not found: {}", fixture_path.display());

    let gir_out = run_gir(&fixture_path)
        .unwrap_or_else(|| panic!("GIR backend failed for {fixture}"));
    let lir_out = run_lir(&fixture_path)
        .unwrap_or_else(|| panic!("LIR backend failed for {fixture}"));

    assert_eq!(
        gir_out.trim(),
        lir_out.trim(),
        "A/B mismatch for {fixture}:\n  GIR: {:?}\n  LIR: {:?}",
        gir_out.trim(),
        lir_out.trim(),
    );
}

/// A/B smoke test that only asserts BOTH backends BUILD + RUN successfully,
/// WITHOUT comparing their stdouts. For fixtures that are intentionally racy
/// (e.g. the §3.5 check-then-act warning demo: a `with`-guarded branch yields,
/// so a spawned worker may mutate the shared state mid-branch), each backend's
/// run can win the race differently — pinning GIR stdout == LIR stdout is a
/// latent flake of the same class the integration test was fixed for. We still
/// want the cross-backend compile-and-run smoke coverage, so we assert both
/// `run_gir`/`run_lir` return `Some(_)` (they return `None` on build/run failure
/// or timeout) and drop the stdout equality check.
fn ab_test_build_only(fixture: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);
    assert!(fixture_path.exists(), "Fixture not found: {}", fixture_path.display());

    assert!(
        run_gir(&fixture_path).is_some(),
        "GIR backend failed to build/run {fixture}",
    );
    assert!(
        run_lir(&fixture_path).is_some(),
        "LIR backend failed to build/run {fixture}",
    );
}

// ── Fixtures known to match between GIR and LIR backends ─────────────────

#[test] fn lir_ab_hello() { ab_test("hello.gg"); }
#[test] fn lir_ab_expressions() { ab_test("expressions.gg"); }
#[test] fn lir_ab_int_range() { ab_test("int_range.gg"); }
#[test] fn lir_ab_mod_rem() { ab_test("mod_rem.gg"); }
#[test] fn lir_ab_wrapping_ops() { ab_test("wrapping_ops.gg"); }
#[test] fn lir_ab_match_patterns() { ab_test("match_patterns.gg"); }
#[test] fn lir_ab_traits() { ab_test("traits.gg"); }
#[test] fn lir_ab_type_alias() { ab_test("type_alias.gg"); }
#[test] fn lir_ab_embed_file() { ab_test("embed_file.gg"); }
#[test] fn lir_ab_assert_basic() { ab_test("assert_basic.gg"); }
#[test] fn lir_ab_use_strip_asserts() { ab_test("use_strip_asserts.gg"); }
#[test] fn lir_ab_meta_fn_basic() { ab_test("meta_fn_basic.gg"); }
#[test] fn lir_ab_meta_fn_loops() { ab_test("meta_fn_loops.gg"); }
#[test] fn lir_ab_meta_fn_recursive() { ab_test("meta_fn_recursive.gg"); }
#[test] fn lir_ab_meta_sizeof() { ab_test("meta_sizeof.gg"); }
#[test] fn lir_ab_meta_type_func() { ab_test("meta_type_func.gg"); }
#[test] fn lir_ab_meta_while() { ab_test("meta_while.gg"); }
#[test] fn lir_ab_meta_enum_ordinal() { ab_test("meta_enum_ordinal.gg"); }
#[test] fn lir_ab_meta_fields() { ab_test("meta_fields.gg"); }
#[test] fn lir_ab_meta_implements() { ab_test("meta_implements.gg"); }
#[test] fn lir_ab_bitwise_ops() { ab_test("bitwise_ops.gg"); }
#[test] fn lir_ab_block_expr() { ab_test("block_expr.gg"); }
#[test] fn lir_ab_break_nested() { ab_test("break_nested.gg"); }
#[test] fn lir_ab_control_flow() { ab_test("control_flow.gg"); }
#[test] fn lir_ab_else_if() { ab_test("else_if.gg"); }
#[test] fn lir_ab_extern_ffi() { ab_test("extern_ffi.gg"); }
#[test] fn lir_ab_for_else() { ab_test("for_else.gg"); }
#[test] fn lir_ab_named_scope_basic() { ab_test("named_scope_basic.gg"); }
#[test] fn lir_ab_strings() { ab_test("strings.gg"); }
#[test] fn lir_ab_type_alias_usage() { ab_test("type_alias_usage.gg"); }
#[test] fn lir_ab_type_casts() { ab_test("type_casts.gg"); }
#[test] fn lir_ab_enums() { ab_test("enums.gg"); }
#[test] fn lir_ab_functions() { ab_test("functions.gg"); }
#[test] fn lir_ab_generics() { ab_test("generics.gg"); }
#[test] fn lir_ab_generic_trait_equip() { ab_test("generic_trait_equip.gg"); }
#[test] fn lir_ab_match_advanced() { ab_test("match_advanced.gg"); }
#[test] fn lir_ab_ownership() { ab_test("ownership.gg"); }
#[test] fn lir_ab_pattern_is() { ab_test("pattern_is.gg"); }
#[test] fn lir_ab_test_coexist() { ab_test("test_coexist.gg"); }
#[test] fn lir_ab_trace_test() { ab_test("trace_test.gg"); }
#[test] fn lir_ab_trait_inherit_defaults() { ab_test("trait_inherit_defaults.gg"); }
#[test] fn lir_ab_type_alias_fn_sig() { ab_test("type_alias_fn_sig.gg"); }
#[test] fn lir_ab_core_traits() { ab_test("core_traits.gg"); }
#[test] fn lir_ab_default_trait() { ab_test("default_trait.gg"); }
#[test] fn lir_ab_dot_shorthand() { ab_test("dot_shorthand.gg"); }
#[test] fn lir_ab_error_handling() { ab_test("error_handling.gg"); }
#[test] fn lir_ab_from_trait() { ab_test("from_trait.gg"); }
#[test] fn lir_ab_from_trait_multi() { ab_test("from_trait_multi.gg"); }
#[test] fn lir_ab_fstring_basic() { ab_test("fstring_basic.gg"); }
#[test] fn lir_ab_generic_op_smoke() { ab_test("generic_op_smoke.gg"); }
#[test] fn lir_ab_meta_basic() { ab_test("meta_basic.gg"); }
#[test] fn lir_ab_meta_builtins() { ab_test("meta_builtins.gg"); }
#[test] fn lir_ab_meta_delayed_match() { ab_test("meta_delayed_match.gg"); }
#[test] fn lir_ab_meta_log() { ab_test("meta_log.gg"); }
#[test] fn lir_ab_meta_numeric_meta() { ab_test("meta_numeric_meta.gg"); }
#[test] fn lir_ab_meta_reflection() { ab_test("meta_reflection.gg"); }
#[test] fn lir_ab_meta_type_is() { ab_test("meta_type_is.gg"); }
#[test] fn lir_ab_meta_variant_payloads() { ab_test("meta_variant_payloads.gg"); }
#[test] fn lir_ab_multiline_strings() { ab_test("multiline_strings.gg"); }
#[test] fn lir_ab_mutable_borrow_params() { ab_test("mutable_borrow_params.gg"); }
#[test] fn lir_ab_operator_overload() { ab_test("operator_overload.gg"); }
#[test] fn lir_ab_operators() { ab_test("operators.gg"); }
#[test] fn lir_ab_raw_strings() { ab_test("raw_strings.gg"); }
#[test] fn lir_ab_str_fat_ptr() { ab_test("str_fat_ptr.gg"); }
#[test] fn lir_ab_structs() { ab_test("structs.gg"); }
#[test] fn lir_ab_variables() { ab_test("variables.gg"); }
#[test] fn lir_ab_cstr_basic() { ab_test("cstr_basic.gg"); }
#[test] fn lir_ab_generic_functions() { ab_test("generic_functions.gg"); }
#[test] fn lir_ab_lifetime_basic() { ab_test("lifetime_basic.gg"); }
#[test] fn lir_ab_meta_delayed_basic() { ab_test("meta_delayed_basic.gg"); }
#[test] fn lir_ab_meta_delayed_for() { ab_test("meta_delayed_for.gg"); }
#[test] fn lir_ab_meta_delayed_nested() { ab_test("meta_delayed_nested.gg"); }
#[test] fn lir_ab_trait_bounds() { ab_test("trait_bounds.gg"); }
#[test] fn lir_ab_char_str_coerce() { ab_test("char_str_coerce.gg"); }
#[test] fn lir_ab_assert_return_basic() { ab_test("assert_return_basic.gg"); }
#[test] fn lir_ab_assert_return_fail() { ab_test("assert_return_fail.gg"); }
#[test] fn lir_ab_assert_rich_enum() { ab_test("assert_rich_enum.gg"); }
#[test] fn lir_ab_assert_rich_enum_fail() { ab_test("assert_rich_enum_fail.gg"); }
#[test] fn lir_ab_assert_rich_string_fail() { ab_test("assert_rich_string_fail.gg"); }
#[test] fn lir_ab_assert_rich_strings() { ab_test("assert_rich_strings.gg"); }
#[test] fn lir_ab_assert_rich_struct() { ab_test("assert_rich_struct.gg"); }
#[test] fn lir_ab_assert_rich_struct_fail() { ab_test("assert_rich_struct_fail.gg"); }
#[test] fn lir_ab_catch_basic() { ab_test("catch_basic.gg"); }
#[test] fn lir_ab_chars() { ab_test("chars.gg"); }
#[test] fn lir_ab_enum_nullary_bare() { ab_test("enum_nullary_bare.gg"); }
#[test] fn lir_ab_option_assign() { ab_test("option_assign.gg"); }
#[test] fn lir_ab_pattern_destructure() { ab_test("pattern_destructure.gg"); }
#[test] fn lir_ab_result_propagation() { ab_test("result_propagation.gg"); }
#[test] fn lir_ab_result_str_concat() { ab_test("result_str_concat.gg"); }
#[test] fn lir_ab_string_concat() { ab_test("string_concat.gg"); }
#[test] fn lir_ab_string_methods() { ab_test("string_methods.gg"); }
#[test] fn lir_ab_trait_defaults() { ab_test("trait_defaults.gg"); }
#[test] fn lir_ab_trait_inheritance() { ab_test("trait_inheritance.gg"); }
#[test] fn lir_ab_try_from_trait() { ab_test("try_from_trait.gg"); }
#[test] fn lir_ab_tuples() { ab_test("tuples.gg"); }
#[test] fn lir_ab_via_delegation() { ab_test("via_delegation.gg"); }
// Async fixtures — some pass but timing-sensitive output can cause false A/B mismatches.
#[test] fn lir_ab_async_basic() { ab_test("async_basic.gg"); }
#[test] fn lir_ab_async_expr_await() { ab_test("async_expr_await.gg"); }
#[test] fn lir_ab_async_for_else() { ab_test("async_for_else.gg"); }
#[test] fn lir_ab_async_param_across_await() { ab_test("async_param_across_await.gg"); }
#[test] fn lir_ab_async_prefix_await() { ab_test("async_prefix_await.gg"); }
#[test] fn lir_ab_async_condition_await() { ab_test("async_condition_await.gg"); }
#[test] fn lir_ab_async_range_await() { ab_test("async_range_await.gg"); }
#[test] fn lir_ab_loops_advanced() { ab_test("loops_advanced.gg"); }
#[test] fn lir_ab_named_args() { ab_test("named_args.gg"); }
#[test] fn lir_ab_assert_fails() { ab_test("assert_fails.gg"); }
#[test] fn lir_ab_drop_move_zero() { ab_test("drop_move_zero.gg"); }
#[test] fn lir_ab_lifetime_method() { ab_test("lifetime_method.gg"); }
#[test] fn lir_ab_lifetime_struct() { ab_test("lifetime_struct.gg"); }
#[test] fn lir_ab_math_constants() { ab_test("math_constants.gg"); }
#[test] fn lir_ab_math_trig() { ab_test("math_trig.gg"); }
#[test] fn lir_ab_on_error_basic() { ab_test("on_error_basic.gg"); }
#[test] fn lir_ab_on_error_inline() { ab_test("on_error_inline.gg"); }
#[test] fn lir_ab_ownership_showcase() { ab_test("ownership_showcase.gg"); }
#[test] fn lir_ab_print_builtin() { ab_test("print_builtin.gg"); }
#[test] fn lir_ab_static_mutation() { ab_test("static_mutation.gg"); }
#[test] fn lir_ab_struct_string_coerce() { ab_test("struct_string_coerce.gg"); }
#[test] fn lir_ab_assert_return_msg() { ab_test("assert_return_msg.gg"); }
#[test] fn lir_ab_char_methods2() { ab_test("char_methods2.gg"); }
#[test] fn lir_ab_str_byte_slice() { ab_test("str_byte_slice.gg"); }
#[test] fn lir_ab_str_codepoint_len() { ab_test("str_codepoint_len.gg"); }
#[test] fn lir_ab_string_methods3() { ab_test("string_methods3.gg"); }
#[test] fn lir_ab_string_stdlib() { ab_test("string_stdlib.gg"); }
#[test] fn lir_ab_generic_method_chain() { ab_test("generic_method_chain.gg"); }
#[test] fn lir_ab_generic_struct_methods() { ab_test("generic_struct_methods.gg"); }
#[test] fn lir_ab_main_throws() { ab_test("main_throws.gg"); }
#[test] fn lir_ab_field_access() { ab_test("field_access.gg"); }
#[test] fn lir_ab_on_error_rethrow() { ab_test("on_error_rethrow.gg"); }
#[test] fn lir_ab_rethrow_bare() { ab_test("rethrow_bare.gg"); }
#[test] fn lir_ab_rethrow_basic() { ab_test("rethrow_basic.gg"); }
#[test] fn lir_ab_string_format() { ab_test("string_format.gg"); }
#[test] fn lir_ab_lifetime_reassign() { ab_test("lifetime_reassign.gg"); }
#[test] fn lir_ab_ownership_calls() { ab_test("ownership_calls.gg"); }
#[test] fn lir_ab_ownership_keywords() { ab_test("ownership_keywords.gg"); }
#[test] fn lir_ab_is_bindings() { ab_test("is_bindings.gg"); }
#[test] fn lir_ab_match_option_result() { ab_test("match_option_result.gg"); }
#[test] fn lir_ab_trait_default_meta() { ab_test("trait_default_meta.gg"); }
#[test] fn lir_ab_derive() { ab_test("derive.gg"); }
#[test] fn lir_ab_alloc_keyword() { ab_test("alloc_keyword.gg"); }
#[test] fn lir_ab_alloc_nested_mixed() { ab_test("alloc_nested_mixed.gg"); }
#[test] fn lir_ab_async_blocking_coroutine() { ab_test("async_blocking_coroutine.gg"); }
#[test] fn lir_ab_async_blocking_io() { ab_test("async_blocking_io.gg"); }
#[test] fn lir_ab_async_channel() { ab_test("async_channel.gg"); }
#[test] fn lir_ab_async_channel_multi() { ab_test("async_channel_multi.gg"); }
#[test] fn lir_ab_async_channel_poll() { ab_test("async_channel_poll.gg"); }
#[test] fn lir_ab_async_channel_unbuffered() { ab_test("async_channel_unbuffered.gg"); }
#[test] fn lir_ab_async_channel_waker() { ab_test("async_channel_waker.gg"); }
#[test] fn lir_ab_async_control_flow() { ab_test("async_control_flow.gg"); }
#[test] fn lir_ab_async_drop() { ab_test("async_drop.gg"); }
#[test] fn lir_ab_async_for_loop() { ab_test("async_for_loop.gg"); }
#[test] fn lir_ab_async_for_loop_collections() { ab_test("async_for_loop_collections.gg"); }
#[test] fn lir_ab_async_match() { ab_test("async_match.gg"); }
#[test] fn lir_ab_async_mutex_lock() { ab_test("async_mutex_lock.gg"); }
#[test] fn lir_ab_async_reactor_sleep() { ab_test("async_reactor_sleep.gg"); }
#[test] fn lir_ab_async_rwlock() { ab_test("async_rwlock.gg"); }
#[test] fn lir_ab_async_select() { ab_test("async_select.gg"); }
#[test] fn lir_ab_async_sleep() { ab_test("async_sleep.gg"); }
#[test] fn lir_ab_async_sleep_spawn() { ab_test("async_sleep_spawn.gg"); }
#[test] fn lir_ab_async_sleep_yield() { ab_test("async_sleep_yield.gg"); }
#[test] fn lir_ab_async_socket_echo() { ab_test("async_socket_echo.gg"); }
#[test] fn lir_ab_async_spawn() { ab_test("async_spawn.gg"); }
#[test] fn lir_ab_async_task_expr_await() { ab_test("async_task_expr_await.gg"); }
#[test] fn lir_ab_async_task_group() { ab_test("async_task_group.gg"); }
#[test] fn lir_ab_async_task_group_fire() { ab_test("async_task_group_fire.gg"); }
#[test] fn lir_ab_async_timer_loop() { ab_test("async_timer_loop.gg"); }
#[test] fn lir_ab_auto_types() { ab_test("auto_types.gg"); }
#[test] fn lir_ab_bare_tuples() { ab_test("bare_tuples.gg"); }
#[test] fn lir_ab_bounds_check() { ab_test("bounds_check.gg"); }
#[test] fn lir_ab_box_callable() { ab_test("box_callable.gg"); }
#[test] fn lir_ab_box_heap() { ab_test("box_heap.gg"); }
#[test] fn lir_ab_builtins_interactive() { ab_test("builtins_interactive.gg"); }
#[test] fn lir_ab_bytes_ops() { ab_test("bytes_ops.gg"); }
#[test] fn lir_ab_callable_ref_param() { ab_test("callable_ref_param.gg"); }
#[test] fn lir_ab_char_method_on_index() { ab_test("char_method_on_index.gg"); }
#[test] fn lir_ab_char_methods() { ab_test("char_methods.gg"); }
#[test] fn lir_ab_cli_advanced() { ab_test("cli_advanced.gg"); }
#[test] fn lir_ab_cli_basic() { ab_test("cli_basic.gg"); }
#[test] fn lir_ab_cli_help() { ab_test("cli_help.gg"); }
#[test] fn lir_ab_closure_escape() { ab_test("closure_escape.gg"); }
#[test] fn lir_ab_closure_float_ret() { ab_test("closure_float_ret.gg"); }
#[test] fn lir_ab_closures() { ab_test("closures.gg"); }
#[test] fn lir_ab_collection_types() { ab_test("collection_types.gg"); }
#[test] fn lir_ab_collections_construct() { ab_test("collections_construct.gg"); }
#[test] fn lir_ab_comprehensions() { ab_test("comprehensions.gg"); }
#[test] fn lir_ab_concurrency_params() { ab_test("concurrency_params.gg"); }
#[test] fn lir_ab_consume_callable_once() { ab_test("consume_callable_once.gg"); }
#[test] fn lir_ab_conv_stdlib() { ab_test("conv_stdlib.gg"); }
#[test] fn lir_ab_crypto_hash() { ab_test("crypto_hash.gg"); }
#[test] fn lir_ab_crypto_x25519() { ab_test("crypto_x25519.gg"); }
#[test] fn lir_ab_csv_basic() { ab_test("csv_basic.gg"); }
#[test] fn lir_ab_csv_delimiters() { ab_test("csv_delimiters.gg"); }
#[test] fn lir_ab_csv_edge() { ab_test("csv_edge.gg"); }
#[test] fn lir_ab_csv_stringify() { ab_test("csv_stringify.gg"); }
#[test] fn lir_ab_dataframe_agg() { ab_test("dataframe_agg.gg"); }
#[test] fn lir_ab_dataframe_basic() { ab_test("dataframe_basic.gg"); }
#[test] fn lir_ab_dataframe_cast() { ab_test("dataframe_cast.gg"); }
#[test] fn lir_ab_dataframe_clip() { ab_test("dataframe_clip.gg"); }
#[test] fn lir_ab_dataframe_csv() { ab_test("dataframe_csv.gg"); }
#[test] fn lir_ab_dataframe_extra() { ab_test("dataframe_extra.gg"); }
#[test] fn lir_ab_dataframe_filter() { ab_test("dataframe_filter.gg"); }
#[test] fn lir_ab_dataframe_filter_sort() { ab_test("dataframe_filter_sort.gg"); }
#[test] fn lir_ab_dataframe_groupby() { ab_test("dataframe_groupby.gg"); }
#[test] fn lir_ab_dataframe_nulls() { ab_test("dataframe_nulls.gg"); }
#[test] fn lir_ab_dataframe_ops() { ab_test("dataframe_ops.gg"); }
#[test] fn lir_ab_dataframe_tier2_basic() { ab_test("dataframe_tier2_basic.gg"); }
#[test] fn lir_ab_dataframe_tier2_groupby() { ab_test("dataframe_tier2_groupby.gg"); }
#[test] fn lir_ab_dataframe_tier2_joins() { ab_test("dataframe_tier2_joins.gg"); }
#[test] fn lir_ab_dataframe_tier2_sort_arith() { ab_test("dataframe_tier2_sort_arith.gg"); }
#[test] fn lir_ab_dataframe_transform() { ab_test("dataframe_transform.gg"); }
#[test] fn lir_ab_datetime_basic() { ab_test("datetime_basic.gg"); }
#[test] fn lir_ab_datetime_extended() { ab_test("datetime_extended.gg"); }
#[test] fn lir_ab_datetime_format() { ab_test("datetime_format.gg"); }
#[test] fn lir_ab_datetime_gaps() { ab_test("datetime_gaps.gg"); }
#[test] fn lir_ab_derive_generic() { ab_test("derive_generic.gg"); }
#[test] fn lir_ab_derive_hashable() { ab_test("derive_hashable.gg"); }
#[test] fn lir_ab_deserializable() { ab_test("deserializable.gg"); }
#[test] fn lir_ab_dict_box_callable() { ab_test("dict_box_callable.gg"); }
#[test] fn lir_ab_dict_get_or_put() { ab_test("dict_get_or_put.gg"); }
#[test] fn lir_ab_dict_higher_order() { ab_test("dict_higher_order.gg"); }
#[test] fn lir_ab_dict_items() { ab_test("dict_items.gg"); }
#[test] fn lir_ab_dict_iter() { ab_test("dict_iter.gg"); }
#[test] fn lir_ab_dict_keys_values() { ab_test("dict_keys_values.gg"); }
#[test] fn lir_ab_dict_literal() { ab_test("dict_literal.gg"); }
#[test] fn lir_ab_dict_order_remove() { ab_test("dict_order_remove.gg"); }
#[test] fn lir_ab_dict_remove() { ab_test("dict_remove.gg"); }
#[test] fn lir_ab_dict_struct_field() { ab_test("dict_struct_field.gg"); }
#[test] fn lir_ab_dict_subscript() { ab_test("dict_subscript.gg"); }
#[test] fn lir_ab_dict_tombstone_stress() { ab_test("dict_tombstone_stress.gg"); }
#[test] fn lir_ab_dict_update() { ab_test("dict_update.gg"); }
#[test] fn lir_ab_drop_block_scope() { ab_test("drop_block_scope.gg"); }
#[test] fn lir_ab_drop_collections() { ab_test("drop_collections.gg"); }
#[test] fn lir_ab_drop_field_move_zero() { ab_test("drop_field_move_zero.gg"); }
#[test] fn lir_ab_drop_fn_return_collection() { ab_test("drop_fn_return_collection.gg"); }
#[test] fn lir_ab_drop_raii() { ab_test("drop_raii.gg"); }
#[test] fn lir_ab_drop_reassign() { ab_test("drop_reassign.gg"); }
#[test] fn lir_ab_drop_struct_collection_fields() { ab_test("drop_struct_collection_fields.gg"); }
#[test] fn lir_ab_drop_struct_fields() { ab_test("drop_struct_fields.gg"); }
#[test] fn lir_ab_dynamic_dispatch() { ab_test("dynamic_dispatch.gg"); }
#[test] fn lir_ab_ecs_advanced() { ab_test("ecs_advanced.gg"); }
#[test] fn lir_ab_ecs_basics() { ab_test("ecs_basics.gg"); }
#[test] fn lir_ab_ecs_query2() { ab_test("ecs_query2.gg"); }
#[test] fn lir_ab_encoding_basic() { ab_test("encoding_basic.gg"); }
#[test] fn lir_ab_encoding_edge() { ab_test("encoding_edge.gg"); }
#[test] fn lir_ab_enumerate() { ab_test("enumerate.gg"); }
#[test] fn lir_ab_exec_builtin() { ab_test("exec_builtin.gg"); }
#[test] fn lir_ab_fallback_basic() { ab_test("fallback_basic.gg"); }
#[test] fn lir_ab_fallback_composable() { ab_test("fallback_composable.gg"); }
#[test] fn lir_ab_fba_basic() { ab_test("fba_basic.gg"); }
#[test] fn lir_ab_fba_composable() { ab_test("fba_composable.gg"); }
#[test] fn lir_ab_fmt_basic() { ab_test("fmt_basic.gg"); }
#[test] fn lir_ab_fmt_edge() { ab_test("fmt_edge.gg"); }
#[test] fn lir_ab_fmt_edges() { ab_test("fmt_edges.gg"); }
#[test] fn lir_ab_fn_mut_once() { ab_test("fn_mut_once.gg"); }
#[test] fn lir_ab_fn_trait() { ab_test("fn_trait.gg"); }
#[test] fn lir_ab_fs_ops() { ab_test("fs_ops.gg"); }
#[test] fn lir_ab_generic_callable() { ab_test("generic_callable.gg"); }
#[test] fn lir_ab_generic_callable_ref() { ab_test("generic_callable_ref.gg"); }
#[test] fn lir_ab_global_float() { ab_test("global_float.gg"); }
#[test] fn lir_ab_guard_rwlock_field() { ab_test("guard_rwlock_field.gg"); }
#[test] fn lir_ab_guard_struct_field() { ab_test("guard_struct_field.gg"); }
#[test] fn lir_ab_hashmap_methods() { ab_test("hashmap_methods.gg"); }
#[test] fn lir_ab_hashmap_string_keys() { ab_test("hashmap_string_keys.gg"); }
#[test] fn lir_ab_hashmap_unordered() { ab_test("hashmap_unordered.gg"); }
#[test] fn lir_ab_hashset_methods() { ab_test("hashset_methods.gg"); }
#[test] fn lir_ab_heap_advanced() { ab_test("heap_advanced.gg"); }
#[test] fn lir_ab_heap_basic() { ab_test("heap_basic.gg"); }
#[test] fn lir_ab_heap_edges() { ab_test("heap_edges.gg"); }
#[test] fn lir_ab_httpserver_basic() { ab_test("httpserver_basic.gg"); }
#[test] fn lir_ab_httpserver_before() { ab_test("httpserver_before.gg"); }
#[test] fn lir_ab_httpserver_body_parsers() { ab_test("httpserver_body_parsers.gg"); }
#[test] fn lir_ab_httpserver_chunked() { ab_test("httpserver_chunked.gg"); }
#[test] fn lir_ab_httpserver_concurrent() { ab_test("httpserver_concurrent.gg"); }
#[test] fn lir_ab_httpserver_e2e() { ab_test("httpserver_e2e.gg"); }
#[test] fn lir_ab_httpserver_e2e_extended() { ab_test("httpserver_e2e_extended.gg"); }
#[test] fn lir_ab_httpserver_json() { ab_test("httpserver_json.gg"); }
#[test] fn lir_ab_httpserver_keepalive() { ab_test("httpserver_keepalive.gg"); }
#[test] fn lir_ab_httpserver_large_body() { ab_test("httpserver_large_body.gg"); }
#[test] fn lir_ab_httpserver_lifecycle() { ab_test("httpserver_lifecycle.gg"); }
#[test] fn lir_ab_httpserver_methods() { ab_test("httpserver_methods.gg"); }
#[test] fn lir_ab_httpserver_middleware() { ab_test("httpserver_middleware.gg"); }
#[test] fn lir_ab_httpserver_parse_request() { ab_test("httpserver_parse_request.gg"); }
#[test] fn lir_ab_httpserver_protocol() { ab_test("httpserver_protocol.gg"); }
#[test] fn lir_ab_httpserver_query_string() { ab_test("httpserver_query_string.gg"); }
#[test] fn lir_ab_httpserver_response() { ab_test("httpserver_response.gg"); }
#[test] fn lir_ab_httpserver_router() { ab_test("httpserver_router.gg"); }
#[test] fn lir_ab_httpserver_router_extended() { ab_test("httpserver_router_extended.gg"); }
#[test] fn lir_ab_httpserver_routing() { ab_test("httpserver_routing.gg"); }
#[test] fn lir_ab_httpserver_static() { ab_test("httpserver_static.gg"); }
#[test] fn lir_ab_httpserver_static_enhanced() { ab_test("httpserver_static_enhanced.gg"); }
#[test] fn lir_ab_httpserver_tls() { ab_test("httpserver_tls.gg"); }
#[test] fn lir_ab_implicit_it() { ab_test("implicit_it.gg"); }
#[test] fn lir_ab_in_operator() { ab_test("in_operator.gg"); }
#[test] fn lir_ab_interp_method_call() { ab_test("interp_method_call.gg"); }
#[test] fn lir_ab_iter_for_else() { ab_test("iter_for_else.gg"); }
#[test] fn lir_ab_iterable() { ab_test("iterable.gg"); }
#[test] fn lir_ab_iterator() { ab_test("iterator.gg"); }
#[test] fn lir_ab_iterator_adapters() { ab_test("iterator_adapters.gg"); }
#[test] fn lir_ab_json_edge_cases() { ab_test("json_edge_cases.gg"); }
#[test] fn lir_ab_json_parse() { ab_test("json_parse.gg"); }
#[test] fn lir_ab_json_pretty() { ab_test("json_pretty.gg"); }
#[test] fn lir_ab_linked_list() { ab_test("linked_list.gg"); }
#[test] fn lir_ab_log_basic() { ab_test("log_basic.gg"); }
#[test] fn lir_ab_log_levels() { ab_test("log_levels.gg"); }
#[test] fn lir_ab_log_set_level() { ab_test("log_set_level.gg"); }
#[test] fn lir_ab_match_generic_methods() { ab_test("match_generic_methods.gg"); }
#[test] fn lir_ab_math_stdlib() { ab_test("math_stdlib.gg"); }
#[test] fn lir_ab_measurable_trait() { ab_test("measurable_trait.gg"); }
#[test] fn lir_ab_meta_conditional_types() { ab_test("meta_conditional_types.gg"); }
#[test] fn lir_ab_method_mut_borrow_arg() { ab_test("method_mut_borrow_arg.gg"); }
#[test] fn lir_ab_move_fn_arg_last_use() { ab_test("move_fn_arg_last_use.gg"); }
#[test] fn lir_ab_move_fn_arg_not_last_use() { ab_test("move_fn_arg_not_last_use.gg"); }
#[test] fn lir_ab_move_type_fn_arg() { ab_test("move_type_fn_arg.gg"); }
#[test] fn lir_ab_move_type_unwrap() { ab_test("move_type_unwrap.gg"); }
#[test] fn lir_ab_mutex_async_contention() { ab_test("mutex_async_contention.gg"); }
#[test] fn lir_ab_mutex_basic() { ab_test("mutex_basic.gg"); }
#[test] fn lir_ab_namespace_basic() { ab_test("namespace_basic.gg"); }
#[test] fn lir_ab_nested_generics() { ab_test("nested_generics.gg"); }
#[test] fn lir_ab_newtype() { ab_test("newtype.gg"); }
#[test] fn lir_ab_newtype_field_access() { ab_test("newtype_field_access.gg"); }
#[test] fn lir_ab_newtype_fn_sig() { ab_test("newtype_fn_sig.gg"); }
#[test] fn lir_ab_numeric_trait() { ab_test("numeric_trait.gg"); }
#[test] fn lir_ab_numeric_trait_ops() { ab_test("numeric_trait_ops.gg"); }
#[test] fn lir_ab_onceflag_basic() { ab_test("onceflag_basic.gg"); }
#[test] fn lir_ab_option_box_enum() { ab_test("option_box_enum.gg"); }
#[test] fn lir_ab_option_expect() { ab_test("option_expect.gg"); }
#[test] fn lir_ab_option_map() { ab_test("option_map.gg"); }
#[test] fn lir_ab_option_methods() { ab_test("option_methods.gg"); }
#[test] fn lir_ab_option_result_combinators() { ab_test("option_result_combinators.gg"); }
#[test] fn lir_ab_option_struct_field() { ab_test("option_struct_field.gg"); }
#[test] fn lir_ab_option_struct_field_ordering() { ab_test("option_struct_field_ordering.gg"); }
#[test] fn lir_ab_option_task() { ab_test("option_task.gg"); }
#[test] fn lir_ab_os_stdlib() { ab_test("os_stdlib.gg"); }
#[test] fn lir_ab_p2p_basic() { ab_test("p2p_basic.gg"); }
#[test] fn lir_ab_p2p_dht() { ab_test("p2p_dht.gg"); }
#[test] fn lir_ab_p2p_discovery() { ab_test("p2p_discovery.gg"); }
#[test] fn lir_ab_p2p_encrypted() { ab_test("p2p_encrypted.gg"); }
#[test] fn lir_ab_p2p_encrypted_large() { ab_test("p2p_encrypted_large.gg"); }
#[test] fn lir_ab_p2p_gossip() { ab_test("p2p_gossip.gg"); }
#[test] fn lir_ab_p2p_multiplex() { ab_test("p2p_multiplex.gg"); }
#[test] fn lir_ab_p2p_nat() { ab_test("p2p_nat.gg"); }
#[test] fn lir_ab_p2p_protocol_rpc() { ab_test("p2p_protocol_rpc.gg"); }
#[test] fn lir_ab_p2p_reliable_basic() { ab_test("p2p_reliable_basic.gg"); }
#[test] fn lir_ab_p2p_reliable_bidir() { ab_test("p2p_reliable_bidir.gg"); }
#[test] fn lir_ab_p2p_reliable_large() { ab_test("p2p_reliable_large.gg"); }
#[test] fn lir_ab_p2p_stream_robust() { ab_test("p2p_stream_robust.gg"); }
#[test] fn lir_ab_path_funcs() { ab_test("path_funcs.gg"); }
#[test] fn lir_ab_path_normalize() { ab_test("path_normalize.gg"); }
#[test] fn lir_ab_pattern_destructure_loop() { ab_test("pattern_destructure_loop.gg"); }
#[test] fn lir_ab_pool_basic() { ab_test("pool_basic.gg"); }
#[test] fn lir_ab_pool_composable() { ab_test("pool_composable.gg"); }
#[test] fn lir_ab_pool_free_blocks() { ab_test("pool_free_blocks.gg"); }
#[test] fn lir_ab_pool_overflow() { ab_test("pool_overflow.gg"); }
#[test] fn lir_ab_print_trait_object() { ab_test("print_trait_object.gg"); }
#[test] fn lir_ab_query_basic() { ab_test("query_basic.gg"); }
#[test] fn lir_ab_random_stdlib() { ab_test("random_stdlib.gg"); }
#[test] fn lir_ab_readdir() { ab_test("readdir.gg"); }
#[test] fn lir_ab_recursive_enum() { ab_test("recursive_enum.gg"); }
#[test] fn lir_ab_regex_basic() { ab_test("regex_basic.gg"); }
#[test] fn lir_ab_regex_extended() { ab_test("regex_extended.gg"); }
#[test] fn lir_ab_result_map() { ab_test("result_map.gg"); }
#[test] fn lir_ab_result_methods() { ab_test("result_methods.gg"); }
#[test] fn lir_ab_return_in_if_in_match() { ab_test("return_in_if_in_match.gg"); }
#[test] fn lir_ab_scheduler_inline() { ab_test("scheduler_inline.gg"); }
#[test] fn lir_ab_scheduler_single() { ab_test("scheduler_single.gg"); }
#[test] fn lir_ab_scheduler_thread() { ab_test("scheduler_thread.gg"); }
#[test] fn lir_ab_semaphore_basic() { ab_test("semaphore_basic.gg"); }
#[test] fn lir_ab_serializable() { ab_test("serializable.gg"); }
#[test] fn lir_ab_serialize_collections() { ab_test("serialize_collections.gg"); }
#[test] fn lir_ab_set_arena() { ab_test("set_arena.gg"); }
#[test] fn lir_ab_set_higher_order() { ab_test("set_higher_order.gg"); }
#[test] fn lir_ab_set_iter() { ab_test("set_iter.gg"); }
#[test] fn lir_ab_set_operations() { ab_test("set_operations.gg"); }
#[test] fn lir_ab_set_subset() { ab_test("set_subset.gg"); }
#[test] fn lir_ab_shared_arc_only() { ab_test("shared_arc_only.gg"); }
#[test] fn lir_ab_shared_atomic() { ab_test("shared_atomic.gg"); }
#[test] fn lir_ab_shared_atomic_bool() { ab_test("shared_atomic_bool.gg"); }
#[test] fn lir_ab_shared_await_release() { ab_test("shared_await_release.gg"); }
#[test] fn lir_ab_shared_basic() { ab_test("shared_basic.gg"); }
#[test] fn lir_ab_shared_early_return() { ab_test("shared_early_return.gg"); }
#[test] fn lir_ab_shared_float() { ab_test("shared_float.gg"); }
#[test] fn lir_ab_shared_iterator_invalidation() { ab_test("shared_iterator_invalidation.gg"); }
#[test] fn lir_ab_shared_keyword_local() { ab_test("shared_keyword_local.gg"); }
#[test] fn lir_ab_shared_multi_spawn() { ab_test("shared_multi_spawn.gg"); }
#[test] fn lir_ab_shared_multi_token() { ab_test("shared_multi_token.gg"); }
#[test] fn lir_ab_shared_nested_spawn() { ab_test("shared_nested_spawn.gg"); }
#[test] fn lir_ab_shared_refcount() { ab_test("shared_refcount.gg"); }
#[test] fn lir_ab_shared_rwlock() { ab_test("shared_rwlock.gg"); }
#[test] fn lir_ab_shared_sleep_loop() { ab_test("shared_sleep_loop.gg"); }
#[test] fn lir_ab_shared_spawn_mutex() { ab_test("shared_spawn_mutex.gg"); }
#[test] fn lir_ab_shared_spawn_readonly() { ab_test("shared_spawn_readonly.gg"); }
#[test] fn lir_ab_shared_spawn_with_tracked() { ab_test("shared_spawn_with_tracked.gg"); }
#[test] fn lir_ab_shared_stale_blocking() { ab_test("shared_stale_blocking.gg"); }
#[test] fn lir_ab_shared_stale_call() { ab_test("shared_stale_call.gg"); }
#[test] fn lir_ab_shared_stale_match() { ab_test("shared_stale_match.gg"); }
#[test] fn lir_ab_shared_stale_refreshed() { ab_test("shared_stale_refreshed.gg"); }
#[test] fn lir_ab_shared_stale_transitive() { ab_test("shared_stale_transitive.gg"); }
#[test] fn lir_ab_shared_stale_tuple() { ab_test("shared_stale_tuple.gg"); }
#[test] fn lir_ab_shared_stale_warning() { ab_test("shared_stale_warning.gg"); }
#[test] fn lir_ab_shared_stale_while() { ab_test("shared_stale_while.gg"); }
#[test] fn lir_ab_shared_stale_writeback() { ab_test("shared_stale_writeback.gg"); }
#[test] fn lir_ab_shared_stress() { ab_test("shared_stress.gg"); }
#[test] fn lir_ab_shared_stress_yield() { ab_test("shared_stress_yield.gg"); }
#[test] fn lir_ab_shared_string() { ab_test("shared_string.gg"); }
#[test] fn lir_ab_shared_struct() { ab_test("shared_struct.gg"); }
#[test] fn lir_ab_shared_transparent() { ab_test("shared_transparent.gg"); }
#[test] fn lir_ab_shared_vector_elem() { ab_test("shared_vector_elem.gg"); }
#[test] fn lir_ab_shared_weak() { ab_test("shared_weak.gg"); }
#[test] fn lir_ab_shared_with_blocking_refresh() { ab_test("shared_with_blocking_refresh.gg"); }
// Intentionally racy (§3.5 check-then-act warning demo) — each backend's run can
// win the spawn→check race differently, so we only assert both BUILD + RUN, not
// that their stdouts match. See `ab_test_build_only`.
#[test] fn lir_ab_shared_with_check_then_act() { ab_test_build_only("shared_with_check_then_act.gg"); }
#[test] fn lir_ab_shared_with_refresh() { ab_test("shared_with_refresh.gg"); }
#[test] fn lir_ab_shared_with_spawned_refresh() { ab_test("shared_with_spawned_refresh.gg"); }
#[test] fn lir_ab_signal_basic() { ab_test("signal_basic.gg"); }
#[test] fn lir_ab_snapshot_basic() { ab_test("snapshot_basic.gg"); }
#[test] fn lir_ab_spawn_blocking_basic() { ab_test("spawn_blocking_basic.gg"); }
#[test] fn lir_ab_spawn_blocking_multi() { ab_test("spawn_blocking_multi.gg"); }
#[test] fn lir_ab_spawn_closure_copy() { ab_test("spawn_closure_copy.gg"); }
#[test] fn lir_ab_spawn_closure_inline() { ab_test("spawn_closure_inline.gg"); }
#[test] fn lir_ab_spawn_closure_shared() { ab_test("spawn_closure_shared.gg"); }
#[test] fn lir_ab_spawn_closure_void() { ab_test("spawn_closure_void.gg"); }
#[test] fn lir_ab_spawn_coroutine_drops() { ab_test("spawn_coroutine_drops.gg"); }
#[test] fn lir_ab_spawn_coroutine_str_args() { ab_test("spawn_coroutine_str_args.gg"); }
#[test] fn lir_ab_spawn_coroutine_string() { ab_test("spawn_coroutine_string.gg"); }
#[test] fn lir_ab_spawn_drop_void() { ab_test("spawn_drop_void.gg"); }
#[test] fn lir_ab_spawn_join_on_drop() { ab_test("spawn_join_on_drop.gg"); }
#[test] fn lir_ab_spawn_many() { ab_test("spawn_many.gg"); }
#[test] fn lir_ab_spawn_method_basic() { ab_test("spawn_method_basic.gg"); }
#[test] fn lir_ab_spawn_method_void() { ab_test("spawn_method_void.gg"); }
#[test] fn lir_ab_spawn_multi_await() { ab_test("spawn_multi_await.gg"); }
#[test] fn lir_ab_spawn_nested_await() { ab_test("spawn_nested_await.gg"); }
#[test] fn lir_ab_spawn_vector_await() { ab_test("spawn_vector_await.gg"); }
#[test] fn lir_ab_static_collection() { ab_test("static_collection.gg"); }
#[test] fn lir_ab_static_ref_param() { ab_test("static_ref_param.gg"); }
#[test] fn lir_ab_str_codepoint_index() { ab_test("str_codepoint_index.gg"); }
#[test] fn lir_ab_stress_atomic_hammer() { ab_test("stress_atomic_hammer.gg"); }
#[test] fn lir_ab_stress_channel_mpsc() { ab_test("stress_channel_mpsc.gg"); }
#[test] fn lir_ab_stress_channel_select() { ab_test("stress_channel_select.gg"); }
#[test] fn lir_ab_stress_mutex_hammer() { ab_test("stress_mutex_hammer.gg"); }
#[test] fn lir_ab_stress_nested_return() { ab_test("stress_nested_return.gg"); }
#[test] fn lir_ab_stress_nested_spawn() { ab_test("stress_nested_spawn.gg"); }
#[test] fn lir_ab_stress_pipeline() { ab_test("stress_pipeline.gg"); }
#[test] fn lir_ab_stress_rwlock_writers() { ab_test("stress_rwlock_writers.gg"); }
#[test] fn lir_ab_stress_shared_channel_notify() { ab_test("stress_shared_channel_notify.gg"); }
#[test] fn lir_ab_stress_shared_channel_pipeline() { ab_test("stress_shared_channel_pipeline.gg"); }
#[test] fn lir_ab_stress_shared_channel_scatter() { ab_test("stress_shared_channel_scatter.gg"); }
#[test] fn lir_ab_stress_shared_channel_select() { ab_test("stress_shared_channel_select.gg"); }
#[test] fn lir_ab_stress_shared_channel_semaphore() { ab_test("stress_shared_channel_semaphore.gg"); }
#[test] fn lir_ab_stress_shared_channel_workqueue() { ab_test("stress_shared_channel_workqueue.gg"); }
#[test] fn lir_ab_stress_shared_comprehensive() { ab_test("stress_shared_comprehensive.gg"); }
#[test] fn lir_ab_stress_shared_multi_token() { ab_test("stress_shared_multi_token.gg"); }
#[test] fn lir_ab_stress_spawn_fan_out() { ab_test("stress_spawn_fan_out.gg"); }
#[test] fn lir_ab_stress_taskgroup_fan() { ab_test("stress_taskgroup_fan.gg"); }
#[test] fn lir_ab_string_builder() { ab_test("string_builder.gg"); }
#[test] fn lir_ab_string_coerce_args() { ab_test("string_coerce_args.gg"); }
#[test] fn lir_ab_string_indexing() { ab_test("string_indexing.gg"); }
#[test] fn lir_ab_string_iterators() { ab_test("string_iterators.gg"); }
#[test] fn lir_ab_string_methods2() { ab_test("string_methods2.gg"); }
#[test] fn lir_ab_string_owned() { ab_test("string_owned.gg"); }
#[test] fn lir_ab_string_strip() { ab_test("string_strip.gg"); }
#[test] fn lir_ab_struct_field_methods() { ab_test("struct_field_methods.gg"); }
#[test] fn lir_ab_sync_atomics() { ab_test("sync_atomics.gg"); }
#[test] fn lir_ab_sync_barrier() { ab_test("sync_barrier.gg"); }
#[test] fn lir_ab_sync_condvar() { ab_test("sync_condvar.gg"); }
#[test] fn lir_ab_sync_rwlock() { ab_test("sync_rwlock.gg"); }
#[test] fn lir_ab_tensor_arithmetic() { ab_test("tensor_arithmetic.gg"); }
#[test] fn lir_ab_tensor_basic() { ab_test("tensor_basic.gg"); }
#[test] fn lir_ab_tensor_broadcast() { ab_test("tensor_broadcast.gg"); }
#[test] fn lir_ab_tensor_extra() { ab_test("tensor_extra.gg"); }
#[test] fn lir_ab_tensor_float_frac() { ab_test("tensor_float_frac.gg"); }
#[test] fn lir_ab_tensor_linalg() { ab_test("tensor_linalg.gg"); }
#[test] fn lir_ab_tensor_reduce() { ab_test("tensor_reduce.gg"); }
#[test] fn lir_ab_tensor_reshape() { ab_test("tensor_reshape.gg"); }
#[test] fn lir_ab_term_basic() { ab_test("term_basic.gg"); }
#[test] fn lir_ab_test_basic() { ab_test("test_basic.gg"); }
#[test] fn lir_ab_test_collections_nested() { ab_test("test_collections_nested.gg"); }
#[test] fn lir_ab_test_collections_nested_advanced() { ab_test("test_collections_nested_advanced.gg"); }
#[test] fn lir_ab_test_dict_all() { ab_test("test_dict_all.gg"); }
#[test] fn lir_ab_test_dict_edge_cases() { ab_test("test_dict_edge_cases.gg"); }
#[test] fn lir_ab_test_hashmap_all() { ab_test("test_hashmap_all.gg"); }
#[test] fn lir_ab_test_hashset_all() { ab_test("test_hashset_all.gg"); }
#[test] fn lir_ab_test_higher_order_named_fn() { ab_test("test_higher_order_named_fn.gg"); }
#[test] fn lir_ab_test_method_chaining() { ab_test("test_method_chaining.gg"); }
#[test] fn lir_ab_test_option_all() { ab_test("test_option_all.gg"); }
// timing-dependent output — excluded from A/B
// #[test] fn lir_ab_test_process() { ab_test("test_process.gg"); }
#[test] fn lir_ab_test_result_advanced() { ab_test("test_result_advanced.gg"); }
#[test] fn lir_ab_test_result_all() { ab_test("test_result_all.gg"); }
#[test] fn lir_ab_test_result_int_int() { ab_test("test_result_int_int.gg"); }
#[test] fn lir_ab_test_set_all() { ab_test("test_set_all.gg"); }
#[test] fn lir_ab_test_set_string() { ab_test("test_set_string.gg"); }
#[test] fn lir_ab_test_should_panic() { ab_test("test_should_panic.gg"); }
#[test] fn lir_ab_test_skip() { ab_test("test_skip.gg"); }
#[test] fn lir_ab_test_str_all() { ab_test("test_str_all.gg"); }
#[test] fn lir_ab_test_string_owned_all() { ab_test("test_string_owned_all.gg"); }
#[test] fn lir_ab_test_suite() { ab_test("test_suite.gg"); }
#[test] fn lir_ab_test_tags() { ab_test("test_tags.gg"); }
#[test] fn lir_ab_test_traced() { ab_test("test_traced.gg"); }
#[test] fn lir_ab_test_vector_advanced() { ab_test("test_vector_advanced.gg"); }
#[test] fn lir_ab_test_vector_all() { ab_test("test_vector_all.gg"); }
#[test] fn lir_ab_test_vector_edge_cases() { ab_test("test_vector_edge_cases.gg"); }
#[test] fn lir_ab_test_vector_float_higher_order() { ab_test("test_vector_float_higher_order.gg"); }
#[test] fn lir_ab_test_vector_zip() { ab_test("test_vector_zip.gg"); }
#[test] fn lir_ab_thread_atomic() { ab_test("thread_atomic.gg"); }
#[test] fn lir_ab_thread_barrier() { ab_test("thread_barrier.gg"); }
#[test] fn lir_ab_thread_basic() { ab_test("thread_basic.gg"); }
#[test] fn lir_ab_thread_mutex() { ab_test("thread_mutex.gg"); }
#[test] fn lir_ab_time_format() { ab_test("time_format.gg"); }
#[test] fn lir_ab_time_stdlib() { ab_test("time_stdlib.gg"); }
#[test] fn lir_ab_tlsf_basic() { ab_test("tlsf_basic.gg"); }
#[test] fn lir_ab_tlsf_composable() { ab_test("tlsf_composable.gg"); }
#[test] fn lir_ab_tlsf_peak_bytes() { ab_test("tlsf_peak_bytes.gg"); }
#[test] fn lir_ab_toml_datetime() { ab_test("toml_datetime.gg"); }
#[test] fn lir_ab_toml_edge() { ab_test("toml_edge.gg"); }
#[test] fn lir_ab_toml_parse() { ab_test("toml_parse.gg"); }
#[test] fn lir_ab_toml_stringify() { ab_test("toml_stringify.gg"); }
#[test] fn lir_ab_tracking_basic() { ab_test("tracking_basic.gg"); }
#[test] fn lir_ab_tracking_composable() { ab_test("tracking_composable.gg"); }
#[test] fn lir_ab_tracking_full_stats() { ab_test("tracking_full_stats.gg"); }
#[test] fn lir_ab_tracking_report() { ab_test("tracking_report.gg"); }
#[test] fn lir_ab_tracking_wraps_arena() { ab_test("tracking_wraps_arena.gg"); }
#[test] fn lir_ab_tracking_wraps_pool() { ab_test("tracking_wraps_pool.gg"); }
#[test] fn lir_ab_type_alias_callback() { ab_test("type_alias_callback.gg"); }
#[test] fn lir_ab_type_alias_complex() { ab_test("type_alias_complex.gg"); }
#[test] fn lir_ab_udp_echo() { ab_test("udp_echo.gg"); }
#[test] fn lir_ab_unicode_strings() { ab_test("unicode_strings.gg"); }
#[test] fn lir_ab_utf8_validation() { ab_test("utf8_validation.gg"); }
#[test] fn lir_ab_uuid_basic() { ab_test("uuid_basic.gg"); }
#[test] fn lir_ab_uuid_props() { ab_test("uuid_props.gg"); }
#[test] fn lir_ab_vector_capacity() { ab_test("vector_capacity.gg"); }
#[test] fn lir_ab_vector_concat() { ab_test("vector_concat.gg"); }
#[test] fn lir_ab_vector_first_last() { ab_test("vector_first_last.gg"); }
#[test] fn lir_ab_vector_higher_order() { ab_test("vector_higher_order.gg"); }
#[test] fn lir_ab_vector_literal() { ab_test("vector_literal.gg"); }
#[test] fn lir_ab_vector_methods() { ab_test("vector_methods.gg"); }
#[test] fn lir_ab_vector_methods2() { ab_test("vector_methods2.gg"); }
#[test] fn lir_ab_vector_sort() { ab_test("vector_sort.gg"); }
#[test] fn lir_ab_vector_task_get() { ab_test("vector_task_get.gg"); }
#[test] fn lir_ab_waitgroup_basic() { ab_test("waitgroup_basic.gg"); }
#[test] fn lir_ab_xml_parse() { ab_test("xml_parse.gg"); }
#[test] fn lir_ab_xml_query() { ab_test("xml_query.gg"); }
#[test] fn lir_ab_xml_roundtrip() { ab_test("xml_roundtrip.gg"); }
#[test] fn lir_ab_yaml_multi() { ab_test("yaml_multi.gg"); }
#[test] fn lir_ab_yaml_parse() { ab_test("yaml_parse.gg"); }
#[test] fn lir_ab_arena_basic() { ab_test("arena_basic.gg"); }
#[test] fn lir_ab_arena_checkpoint() { ab_test("arena_checkpoint.gg"); }
#[test] fn lir_ab_arena_dict() { ab_test("arena_dict.gg"); }
#[test] fn lir_ab_arena_multi_collection() { ab_test("arena_multi_collection.gg"); }
#[test] fn lir_ab_arena_reset_reuse() { ab_test("arena_reset_reuse.gg"); }
#[test] fn lir_ab_channel_raii() { ab_test("channel_raii.gg"); }
#[test] fn lir_ab_channel_recv_timeout() { ab_test("channel_recv_timeout.gg"); }
#[test] fn lir_ab_coroutine_collections() { ab_test("coroutine_collections.gg"); }
#[test] fn lir_ab_coroutine_collections_advanced() { ab_test("coroutine_collections_advanced.gg"); }
#[test] fn lir_ab_coroutine_dict_set() { ab_test("coroutine_dict_set.gg"); }
#[test] fn lir_ab_coroutine_option_combinators() { ab_test("coroutine_option_combinators.gg"); }
#[test] fn lir_ab_coroutine_option_result() { ab_test("coroutine_option_result.gg"); }
#[test] fn lir_ab_coroutine_result_combinators() { ab_test("coroutine_result_combinators.gg"); }
#[test] fn lir_ab_div_by_zero() { ab_test("div_by_zero.gg"); }
#[test] fn lir_ab_overflow_add() { ab_test("overflow_add.gg"); }
#[test] fn lir_ab_overflow_mul() { ab_test("overflow_mul.gg"); }
#[test] fn lir_ab_overflow_sub() { ab_test("overflow_sub.gg"); }
#[test] fn lir_ab_process_pipe() { ab_test("process_pipe.gg"); }
#[test] fn lir_ab_process_spawn() { ab_test("process_spawn.gg"); }
#[test] fn lir_ab_string_index_oob() { ab_test("string_index_oob.gg"); }
#[test] fn lir_ab_test_cleanup() { ab_test("test_cleanup.gg"); }
#[test] fn lir_ab_test_failure() { ab_test("test_failure.gg"); }
#[test] fn lir_ab_test_with_clause() { ab_test("test_with_clause.gg"); }
#[test] fn lir_ab_closure_iife() { ab_test("closure_iife.gg"); }
#[test] fn lir_ab_control_early_return() { ab_test("control_early_return.gg"); }
#[test] fn lir_ab_control_match_in_loop() { ab_test("control_match_in_loop.gg"); }
#[test] fn lir_ab_control_nested_loops() { ab_test("control_nested_loops.gg"); }
#[test] fn lir_ab_control_nested_match() { ab_test("control_nested_match.gg"); }
#[test] fn lir_ab_control_while_complex() { ab_test("control_while_complex.gg"); }
#[test] fn lir_ab_default_params_basic() { ab_test("default_params_basic.gg"); }
#[test] fn lir_ab_derive_equatable_enum() { ab_test("derive_equatable_enum.gg"); }
#[test] fn lir_ab_dict_operations_edge() { ab_test("dict_operations_edge.gg"); }
#[test] fn lir_ab_enum_single_variant() { ab_test("enum_single_variant.gg"); }
#[test] fn lir_ab_enum_with_data_variants() { ab_test("enum_with_data_variants.gg"); }
#[test] fn lir_ab_equip_multiple_traits() { ab_test("equip_multiple_traits.gg"); }
#[test] fn lir_ab_expression_body_functions() { ab_test("expression_body_functions.gg"); }
#[test] fn lir_ab_fstring_expressions() { ab_test("fstring_expressions.gg"); }
#[test] fn lir_ab_generic_equip_method() { ab_test("generic_equip_method.gg"); }
#[test] fn lir_ab_generic_trait_bound() { ab_test("generic_trait_bound.gg"); }
#[test] fn lir_ab_numeric_division() { ab_test("numeric_division.gg"); }
#[test] fn lir_ab_numeric_overflow_wrap() { ab_test("numeric_overflow_wrap.gg"); }
#[test] fn lir_ab_option_result_nested() { ab_test("option_result_nested.gg"); }
#[test] fn lir_ab_recursion_fibonacci() { ab_test("recursion_fibonacci.gg"); }
#[test] fn lir_ab_recursion_mutual() { ab_test("recursion_mutual.gg"); }
#[test] fn lir_ab_recursive_enum_tree() { ab_test("recursive_enum_tree.gg"); }
#[test] fn lir_ab_shadowing_nested() { ab_test("shadowing_nested.gg"); }
#[test] fn lir_ab_string_escape_sequences() { ab_test("string_escape_sequences.gg"); }
#[test] fn lir_ab_string_methods_edge() { ab_test("string_methods_edge.gg"); }
#[test] fn lir_ab_struct_nested_access() { ab_test("struct_nested_access.gg"); }
#[test] fn lir_ab_struct_single_field() { ab_test("struct_single_field.gg"); }
#[test] fn lir_ab_two_traits_basic() { ab_test("two_traits_basic.gg"); }
#[test] fn lir_ab_type_alias_generic() { ab_test("type_alias_generic.gg"); }
#[test] fn lir_ab_type_cast_numeric() { ab_test("type_cast_numeric.gg"); }
#[test] fn lir_ab_vector_operations_edge() { ab_test("vector_operations_edge.gg"); }

// ── Batch 2: additional fixtures enrolled 2026-03-16 ─────────────────
#[test] fn lir_ab_bare_param_immutable() { ab_test("bare_param_immutable.gg"); }
#[test] fn lir_ab_borrow_after_method_mut() { ab_test("borrow_after_method_mut.gg"); }
#[test] fn lir_ab_borrow_method_chain() { ab_test("borrow_method_chain.gg"); }
#[test] fn lir_ab_borrow_multiple_fields() { ab_test("borrow_multiple_fields.gg"); }
#[test] fn lir_ab_borrow_struct_field_access() { ab_test("borrow_struct_field_access.gg"); }
#[test] fn lir_ab_closure_as_callback() { ab_test("closure_as_callback.gg"); }
#[test] fn lir_ab_closure_capture_loop_var() { ab_test("closure_capture_loop_var.gg"); }
#[test] fn lir_ab_closure_higher_order_chain() { ab_test("closure_higher_order_chain.gg"); }
#[test] fn lir_ab_closure_mutable_capture() { ab_test("closure_mutable_capture.gg"); }
#[test] fn lir_ab_closure_returning_closure() { ab_test("closure_returning_closure.gg"); }
#[test] fn lir_ab_compound_index_assign() { ab_test("compound_index_assign.gg"); }
#[test] fn lir_ab_copy_enum_primitive_payload() { ab_test("copy_enum_primitive_payload.gg"); }
#[test] fn lir_ab_copy_mixed_struct_fields() { ab_test("copy_mixed_struct_fields.gg"); }
#[test] fn lir_ab_copy_struct_closure_capture() { ab_test("copy_struct_closure_capture.gg"); }
#[test] fn lir_ab_copy_struct_in_loop() { ab_test("copy_struct_in_loop.gg"); }
#[test] fn lir_ab_copy_struct_match() { ab_test("copy_struct_match.gg"); }
#[test] fn lir_ab_copy_struct_multiple_args() { ab_test("copy_struct_multiple_args.gg"); }
#[test] fn lir_ab_copy_struct_nested() { ab_test("copy_struct_nested.gg"); }
#[test] fn lir_ab_copy_struct_return() { ab_test("copy_struct_return.gg"); }
#[test] fn lir_ab_coroutine_chained_methods() { ab_test("coroutine_chained_methods.gg"); }
#[test] fn lir_ab_coroutine_dict_higher_order() { ab_test("coroutine_dict_higher_order.gg"); }
#[test] fn lir_ab_coroutine_struct_methods() { ab_test("coroutine_struct_methods.gg"); }
#[test] fn lir_ab_coroutine_vector_ops() { ab_test("coroutine_vector_ops.gg"); }
#[test] fn lir_ab_derive_ordinal() { ab_test("derive_ordinal.gg"); }
#[test] fn lir_ab_error_catch_chain() { ab_test("error_catch_chain.gg"); }
#[test] fn lir_ab_error_catch_in_expression() { ab_test("error_catch_in_expression.gg"); }
#[test] fn lir_ab_error_catch_with_value() { ab_test("error_catch_with_value.gg"); }
#[test] fn lir_ab_error_conditional_throw() { ab_test("error_conditional_throw.gg"); }
#[test] fn lir_ab_error_on_error_ordering() { ab_test("error_on_error_ordering.gg"); }
#[test] fn lir_ab_error_propagation_chain() { ab_test("error_propagation_chain.gg"); }
#[test] fn lir_ab_error_raw_nested() { ab_test("error_raw_nested.gg"); }
#[test] fn lir_ab_error_result_methods() { ab_test("error_result_methods.gg"); }
#[test] fn lir_ab_error_rethrow_transform() { ab_test("error_rethrow_transform.gg"); }
#[test] fn lir_ab_fstring_format() { ab_test("fstring_format.gg"); }
#[test] fn lir_ab_generic_identity() { ab_test("generic_identity.gg"); }
#[test] fn lir_ab_generic_option_chain() { ab_test("generic_option_chain.gg"); }
#[test] fn lir_ab_generic_pair_swap() { ab_test("generic_pair_swap.gg"); }
#[test] fn lir_ab_generic_vector_of_structs() { ab_test("generic_vector_of_structs.gg"); }
#[test] fn lir_ab_match_enum_guard() { ab_test("match_enum_guard.gg"); }
#[test] fn lir_ab_match_enum_multiple_variants() { ab_test("match_enum_multiple_variants.gg"); }
#[test] fn lir_ab_match_int_ranges() { ab_test("match_int_ranges.gg"); }
#[test] fn lir_ab_match_nested_option() { ab_test("match_nested_option.gg"); }
#[test] fn lir_ab_match_result_chain() { ab_test("match_result_chain.gg"); }
#[test] fn lir_ab_match_tuple_destructure() { ab_test("match_tuple_destructure.gg"); }
#[test] fn lir_ab_method_chaining() { ab_test("method_chaining.gg"); }
#[test] fn lir_ab_move_in_branch() { ab_test("move_in_branch.gg"); }
#[test] fn lir_ab_move_reassign_after() { ab_test("move_reassign_after.gg"); }
#[test] fn lir_ab_struct_copy_pass_twice() { ab_test("struct_copy_pass_twice.gg"); }
#[test] fn lir_ab_struct_vector_bare_param() { ab_test("struct_vector_bare_param.gg"); }
#[test] fn lir_ab_struct_vector_bare_param2() { ab_test("struct_vector_bare_param2.gg"); }
#[test] fn lir_ab_test_assert() { ab_test("test_assert.gg"); }
#[test] fn lir_ab_test_bitwise_ops() { ab_test("test_bitwise_ops.gg"); }
#[test] fn lir_ab_test_comprehensions() { ab_test("test_comprehensions.gg"); }
#[test] fn lir_ab_test_coroutine_basic() { ab_test("test_coroutine_basic.gg"); }
#[test] fn lir_ab_test_deque() { ab_test("test_deque.gg"); }
#[test] fn lir_ab_test_dict_int_keys() { ab_test("test_dict_int_keys.gg"); }
#[test] fn lir_ab_test_dict_str_values() { ab_test("test_dict_str_values.gg"); }
#[test] fn lir_ab_test_do_block_expr() { ab_test("test_do_block_expr.gg"); }
#[test] fn lir_ab_test_early_return() { ab_test("test_early_return.gg"); }
#[test] fn lir_ab_test_enum_match() { ab_test("test_enum_match.gg"); }
#[test] fn lir_ab_test_enum_user_defined() { ab_test("test_enum_user_defined.gg"); }
#[test] fn lir_ab_test_error_handling() { ab_test("test_error_handling.gg"); }
#[test] fn lir_ab_test_for_loops() { ab_test("test_for_loops.gg"); }
#[test] fn lir_ab_test_fstrings() { ab_test("test_fstrings.gg"); }
#[test] fn lir_ab_test_generic_functions() { ab_test("test_generic_functions.gg"); }
#[test] fn lir_ab_test_generic_struct() { ab_test("test_generic_struct.gg"); }
#[test] fn lir_ab_test_if_expressions() { ab_test("test_if_expressions.gg"); }
#[test] fn lir_ab_test_linked_list() { ab_test("test_linked_list.gg"); }
#[test] fn lir_ab_test_match_advanced() { ab_test("test_match_advanced.gg"); }
#[test] fn lir_ab_test_math3d() { ab_test("test_math3d.gg"); }
#[test] fn lir_ab_test_multiline_closures() { ab_test("test_multiline_closures.gg"); }
#[test] fn lir_ab_test_named_scope() { ab_test("test_named_scope.gg"); }
#[test] fn lir_ab_test_nested_loops() { ab_test("test_nested_loops.gg"); }
#[test] fn lir_ab_test_numeric_types() { ab_test("test_numeric_types.gg"); }
#[test] fn lir_ab_test_option_chaining() { ab_test("test_option_chaining.gg"); }
#[test] fn lir_ab_test_recursion() { ab_test("test_recursion.gg"); }
#[test] fn lir_ab_test_result_chaining() { ab_test("test_result_chaining.gg"); }
#[test] fn lir_ab_test_scope_blocks() { ab_test("test_scope_blocks.gg"); }
#[test] fn lir_ab_test_static_vars() { ab_test("test_static_vars.gg"); }
#[test] fn lir_ab_test_str_predicates() { ab_test("test_str_predicates.gg"); }
#[test] fn lir_ab_test_string_interpolation() { ab_test("test_string_interpolation.gg"); }
#[test] fn lir_ab_test_string_methods() { ab_test("test_string_methods.gg"); }
#[test] fn lir_ab_test_string_owned() { ab_test("test_string_owned.gg"); }
#[test] fn lir_ab_test_struct_methods() { ab_test("test_struct_methods.gg"); }
#[test] fn lir_ab_test_timeout() { ab_test("test_timeout.gg"); }
#[test] fn lir_ab_test_traits_equip() { ab_test("test_traits_equip.gg"); }
#[test] fn lir_ab_test_tuples() { ab_test("test_tuples.gg"); }
#[test] fn lir_ab_test_type_casting() { ab_test("test_type_casting.gg"); }
#[test] fn lir_ab_test_vector_bool() { ab_test("test_vector_bool.gg"); }
#[test] fn lir_ab_test_vector_sort_methods() { ab_test("test_vector_sort_methods.gg"); }
#[test] fn lir_ab_test_vector_of_structs() { ab_test("test_vector_of_structs.gg"); }
#[test] fn lir_ab_test_vector_str_higher_order() { ab_test("test_vector_str_higher_order.gg"); }
#[test] fn lir_ab_test_while_loops() { ab_test("test_while_loops.gg"); }
#[test] fn lir_ab_file_io() { ab_test("file_io.gg"); }
#[test] fn lir_ab_set_ordering() { ab_test("set_ordering.gg"); }
#[test] fn lir_ab_test_closures_advanced() { ab_test("test_closures_advanced.gg"); }
#[test] fn lir_ab_test_closures_edge_cases() { ab_test("test_closures_edge_cases.gg"); }
#[test] fn lir_ab_generic_nested_collections() { ab_test("generic_nested_collections.gg"); }
#[test] fn lir_ab_test_struct_derive() { ab_test("test_struct_derive.gg"); }
#[test] fn lir_ab_cli_args() { ab_test_with_args("cli_args.gg", &["hello", "world"]); }

// ── Allocation stress tests ─────────────────────────────────────────
#[test] fn lir_ab_stress_alloc_strings() { ab_test("stress_alloc_strings.gg"); }
#[test] fn lir_ab_stress_alloc_vectors() { ab_test("stress_alloc_vectors.gg"); }
#[test] fn lir_ab_stress_alloc_dicts() { ab_test("stress_alloc_dicts.gg"); }
#[test] fn lir_ab_stress_alloc_structs() { ab_test("stress_alloc_structs.gg"); }
#[test] fn lir_ab_stress_alloc_closures() { ab_test("stress_alloc_closures.gg"); }
#[test] fn lir_ab_stress_alloc_mixed() { ab_test("stress_alloc_mixed.gg"); }
