//! LIR A/B test: compile fixtures through both the GIR C backend and the
//! LIR→C backend, then compare their stdout.
//!
//! Only tests fixtures known to produce matching output through both pipelines.
//! As the LIR backend matures, more fixtures are added to this list.

use std::path::PathBuf;
use std::process::Command;

/// Build + run through the GIR C backend (normal path).
fn run_gir(fixture_path: &std::path::Path) -> Option<String> {
    let stem = fixture_path.file_stem()?.to_str()?;
    let dir = fixture_path.parent()?;
    let exe_path = dir.join(stem);
    let c_path = dir.join(format!("{stem}.c"));

    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build"])
        .arg(fixture_path)
        .output()
        .ok()?;
    if !build.status.success() {
        return None;
    }

    let run = Command::new(&exe_path).output().ok()?;
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();

    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&c_path);

    Some(stdout)
}

/// Build through LIR→C, compile with cc, run, return stdout.
fn run_lir(fixture_path: &std::path::Path) -> Option<String> {
    // Step 1: emit C from LIR
    let emit = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build", "--emit-c-lir"])
        .arg(fixture_path)
        .output()
        .ok()?;
    if !emit.status.success() {
        return None;
    }

    let c_code = String::from_utf8_lossy(&emit.stdout);
    if c_code.is_empty() {
        return None;
    }

    // Step 2: write C to temp file and compile with cc
    // Use fixture stem in temp name to avoid collisions during parallel test runs.
    let stem = fixture_path.file_stem()?.to_str()?;
    let tmp_c = std::env::temp_dir().join(format!("gorget_lir_ab_{stem}.c"));
    let tmp_exe = std::env::temp_dir().join(format!("gorget_lir_ab_{stem}"));
    std::fs::write(&tmp_c, c_code.as_bytes()).ok()?;

    let cc = Command::new("cc")
        .args(["-w", "-o"])
        .arg(&tmp_exe)
        .arg(&tmp_c)
        .arg("-lm")
        .output()
        .ok()?;
    if !cc.status.success() {
        let _ = std::fs::remove_file(&tmp_c);
        return None;
    }

    // Step 3: run the binary
    let run = Command::new(&tmp_exe).output().ok()?;
    let stdout = String::from_utf8_lossy(&run.stdout).to_string();

    let _ = std::fs::remove_file(&tmp_c);
    let _ = std::fs::remove_file(&tmp_exe);

    Some(stdout)
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
#[test] fn lir_ab_extern_ffi() { ab_test("extern_ffi.gg"); }
#[test] fn lir_ab_for_else() { ab_test("for_else.gg"); }
#[test] fn lir_ab_named_scope_basic() { ab_test("named_scope_basic.gg"); }
#[test] fn lir_ab_strings() { ab_test("strings.gg"); }
#[test] fn lir_ab_type_alias_usage() { ab_test("type_alias_usage.gg"); }
#[test] fn lir_ab_type_casts() { ab_test("type_casts.gg"); }
#[test] fn lir_ab_use_overflow_wrap() { ab_test("use_overflow_wrap.gg"); }
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
#[test] fn lir_ab_async_basic() { ab_test("async_basic.gg"); }
#[test] fn lir_ab_async_condition_await() { ab_test("async_condition_await.gg"); }
#[test] fn lir_ab_async_expr_await() { ab_test("async_expr_await.gg"); }
#[test] fn lir_ab_async_for_else() { ab_test("async_for_else.gg"); }
#[test] fn lir_ab_async_param_across_await() { ab_test("async_param_across_await.gg"); }
#[test] fn lir_ab_async_prefix_await() { ab_test("async_prefix_await.gg"); }
// async_range_await: timing-sensitive
#[test] fn lir_ab_catch_basic() { ab_test("catch_basic.gg"); }
#[test] fn lir_ab_math_constants() { ab_test("math_constants.gg"); }
#[test] fn lir_ab_math_trig() { ab_test("math_trig.gg"); }
#[test] fn lir_ab_enum_nullary_bare() { ab_test("enum_nullary_bare.gg"); }
#[test] fn lir_ab_on_error_basic() { ab_test("on_error_basic.gg"); }
#[test] fn lir_ab_on_error_inline() { ab_test("on_error_inline.gg"); }
#[test] fn lir_ab_result_propagation() { ab_test("result_propagation.gg"); }
#[test] fn lir_ab_string_concat() { ab_test("string_concat.gg"); }
#[test] fn lir_ab_string_methods_all() { ab_test("string_methods.gg"); }
