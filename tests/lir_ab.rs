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
    let tmp_c = std::env::temp_dir().join("gorget_lir_ab_test.c");
    let tmp_exe = std::env::temp_dir().join("gorget_lir_ab_test");
    std::fs::write(&tmp_c, c_code.as_bytes()).ok()?;

    let cc = Command::new("cc")
        .args(["-w", "-o"])
        .arg(&tmp_exe)
        .arg(&tmp_c)
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
#[test] fn lir_ab_immutable_by_default() { ab_test("immutable_by_default.gg"); }
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
