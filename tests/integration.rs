use std::path::{Path, PathBuf};
use std::process::Command;

/// Build and run a `.gg` fixture, asserting its stdout matches `expected`.
fn run_gg(fixture: &str, expected: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);

    assert!(
        fixture_path.exists(),
        "Fixture not found: {}",
        fixture_path.display()
    );

    let stem = fixture_path.file_stem().unwrap().to_str().unwrap();
    let dir = fixture_path.parent().unwrap();
    let c_path = dir.join(format!("{stem}.c"));
    let exe_path = dir.join(stem);

    // 1. Build: cargo run -- build <fixture>
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed for {fixture}:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr),
    );

    // 2. Execute the compiled binary
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    let stdout = String::from_utf8_lossy(&run.stdout);

    // 3. Assert stdout
    assert_eq!(
        stdout.trim(),
        expected.trim(),
        "Output mismatch for {fixture}:\nExpected:\n{expected}\nGot:\n{stdout}",
    );

    assert!(
        run.status.success(),
        "Binary exited with error for {fixture}: {:?}\nstderr: {}",
        run.status.code(),
        String::from_utf8_lossy(&run.stderr),
    );

    // 4. Clean up generated files
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn hello() {
    run_gg("hello.gg", "Hello, World!");
}

#[test]
fn variables() {
    run_gg(
        "variables.gg",
        "\
30
20
gorget",
    );
}

#[test]
fn functions() {
    run_gg(
        "functions.gg",
        "\
7
10
120",
    );
}

#[test]
fn control_flow() {
    run_gg(
        "control_flow.gg",
        "\
positive
0
1
2
0
1
2
3
4",
    );
}

#[test]
fn structs() {
    run_gg(
        "structs.gg",
        "\
point created
rectangle created
sum called
origin called
param sum called
direct sum called
fn sum called",
    );
}

#[test]
fn enums() {
    run_gg("enums.gg", "red");
}

#[test]
fn match_patterns() {
    run_gg(
        "match_patterns.gg",
        "\
the answer
big",
    );
}

#[test]
fn strings() {
    run_gg(
        "strings.gg",
        "\
hello world
x is 42
sum is 52
escape: \\n is newline",
    );
}

#[test]
fn expressions() {
    run_gg(
        "expressions.gg",
        "\
15
-5
50",
    );
}

#[test]
fn for_else() {
    run_gg(
        "for_else.gg",
        "\
0
1
2
3
4
completed
0
1
2
while done
end",
    );
}

#[test]
fn error_handling() {
    run_gg(
        "error_handling.gg",
        "\
10
0
0
11
0
done",
    );
}

#[test]
fn generics() {
    run_gg(
        "generics.gg",
        "\
30
99",
    );
}

#[test]
fn type_alias() {
    run_gg(
        "type_alias.gg",
        "\
42
type alias works",
    );
}

#[test]
fn traits() {
    run_gg("traits.gg", "circle created");
}

#[test]
fn comprehensions() {
    run_gg(
        "comprehensions.gg",
        "\
list done
set done
dict done",
    );
}

#[test]
fn ownership() {
    run_gg(
        "ownership.gg",
        "\
42
42
1
2",
    );
}

#[test]
fn closures() {
    run_gg("closures.gg", "\
15
30
20
10
111
closures");
}

#[test]
fn dynamic_dispatch() {
    run_gg("dynamic_dispatch.gg", "dispatch");
}

#[test]
fn auto_types() {
    run_gg(
        "auto_types.gg",
        "\
42
hello
3.140000
true
A
50
hello world
100
auto struct
15
auto closure",
    );
}

#[test]
fn break_nested() {
    run_gg(
        "break_nested.gg",
        "\
test1 done
test2 done
test3 done
completed normally
test4 done
test5 done
while completed
test6 done
test7 done",
    );
}

#[test]
fn newtype() {
    run_gg("newtype.gg", "newtype works");
}

/// Test that `gg run` works (compile + execute in one step).
#[test]
fn gg_run_command() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/hello.gg");

    let output = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "run"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        output.status.success(),
        "`gg run` failed:\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.trim(), "Hello, World!");

    // Clean up artifacts from `gg run`
    let dir = fixture_path.parent().unwrap();
    let _ = std::fs::remove_file(dir.join("hello.c"));
    let _ = std::fs::remove_file(dir.join("hello"));
}

#[test]
fn operators() {
    run_gg(
        "operators.gg",
        "\
3
1
-5
-5
false
true
-4
-21
-10
7
-10
false
true
3
2",
    );
}

#[test]
fn chars() {
    run_gg(
        "chars.gg",
        "\
A
A < B
equal
\\
true",
    );
}

#[test]
fn loops_advanced() {
    run_gg(
        "loops_advanced.gg",
        "\
0
1
2
1
3
5
1
2
4
5
0
1
2
done",
    );
}

#[test]
fn tuples() {
    run_gg(
        "tuples.gg",
        "\
10
20
42
99
1
2
3
10
20
1
2
3
true
7",
    );
}

#[test]
fn type_casts() {
    run_gg(
        "type_casts.gg",
        "\
42.000000
3
2.500000
-7",
    );
}

#[test]
fn match_advanced() {
    run_gg(
        "match_advanced.gg",
        "\
5
3 4
positive
point",
    );
}

#[test]
fn pattern_is() {
    run_gg(
        "pattern_is.gg",
        "\
is red
not blue
not red
is green",
    );
}

#[test]
fn block_expr() {
    run_gg(
        "block_expr.gg",
        "\
15
9
30
11
20",
    );
}

#[test]
fn ownership_calls() {
    run_gg(
        "ownership_calls.gg",
        "\
42
moved
borrowed
done",
    );
}

#[test]
fn ownership_keywords() {
    run_gg(
        "ownership_keywords.gg",
        "\
42
42
1
2
moved
borrowed
99
done",
    );
}

// ══════════════════════════════════════════════════════════════
// Module / import tests
// ══════════════════════════════════════════════════════════════

/// Build and run a multi-file `.gg` fixture from a directory.
fn run_gg_dir(dir_name: &str, main_file: &str, expected: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let dir_path = manifest_dir.join("tests/fixtures").join(dir_name);
    let main_path = dir_path.join(main_file);

    assert!(
        main_path.exists(),
        "Fixture not found: {}",
        main_path.display()
    );

    let stem = Path::new(main_file)
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();
    let c_path = dir_path.join(format!("{stem}.c"));
    let exe_path = dir_path.join(stem);

    // 1. Build: cargo run -- build <dir/main.gg>
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build"])
        .arg(&main_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed for {dir_name}/{main_file}:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr),
    );

    // 2. Execute the compiled binary
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    let stdout = String::from_utf8_lossy(&run.stdout);

    // 3. Assert stdout
    assert_eq!(
        stdout.trim(),
        expected.trim(),
        "Output mismatch for {dir_name}/{main_file}:\nExpected:\n{expected}\nGot:\n{stdout}",
    );

    assert!(
        run.status.success(),
        "Binary exited with error for {dir_name}/{main_file}: {:?}\nstderr: {}",
        run.status.code(),
        String::from_utf8_lossy(&run.stderr),
    );

    // 4. Clean up generated files
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn modules_basic() {
    run_gg_dir("modules_basic", "main.gg", "5");
}

#[test]
fn modules_nested() {
    run_gg_dir("modules_nested", "main.gg", "hello world");
}

#[test]
fn modules_from_import() {
    run_gg_dir("modules_from", "main.gg", "42");
}

#[test]
fn modules_chain() {
    run_gg_dir("modules_chain", "main.gg", "99");
}

#[test]
fn vector_methods() {
    run_gg(
        "vector_methods.gg",
        "\
4
1
4
3
99
2
2
0
empty",
    );
}

#[test]
fn collections_construct() {
    run_gg(
        "collections_construct.gg",
        "\
2
10",
    );
}

#[test]
fn hashmap_methods() {
    run_gg(
        "hashmap_methods.gg",
        "\
3
20
has 1
no 99
2
removed 2
0
empty",
    );
}

#[test]
fn hashset_methods() {
    run_gg(
        "hashset_methods.gg",
        "\
2
has 10
no 99
1
removed 10
0
empty",
    );
}

#[test]
fn collection_types() {
    run_gg(
        "collection_types.gg",
        "\
alice
bob
30",
    );
}

#[test]
fn string_methods() {
    run_gg(
        "string_methods.gg",
        "\
5
0",
    );
}

#[test]
fn interp_method_call() {
    run_gg(
        "interp_method_call.gg",
        "\
2
10
5",
    );
}

#[test]
fn option_methods() {
    run_gg(
        "option_methods.gg",
        "\
42
42
99
some is some
none is none",
    );
}

#[test]
fn result_methods() {
    run_gg(
        "result_methods.gg",
        "\
10
10
99
ok is ok
err is err",
    );
}

#[test]
fn option_map() {
    run_gg(
        "option_map.gg",
        "\
84
0
43
99",
    );
}

#[test]
fn result_map() {
    run_gg(
        "result_map.gg",
        "\
20
0
11",
    );
}

#[test]
fn dict_iter() {
    run_gg(
        "dict_iter.gg",
        "\
90
3",
    );
}

#[test]
fn set_iter() {
    run_gg("set_iter.gg", "60");
}

#[test]
fn core_traits() {
    run_gg(
        "core_traits.gg",
        "\
equal
not equal
Point",
    );
}

#[test]
fn iter_for_else() {
    run_gg(
        "iter_for_else.gg",
        "\
empty set
done",
    );
}

#[test]
fn implicit_it() {
    run_gg(
        "implicit_it.gg",
        "\
84
0
43",
    );
}

#[test]
fn box_heap() {
    run_gg(
        "box_heap.gg",
        "\
42
42
100
hello",
    );
}

#[test]
fn drop_raii() {
    run_gg(
        "drop_raii.gg",
        "\
value: 42
done
dropping alpha",
    );
}

#[test]
fn trait_defaults() {
    run_gg(
        "trait_defaults.gg",
        "\
hello Alice
bonjour Bob",
    );
}

#[test]
fn trait_inheritance() {
    run_gg(
        "trait_inheritance.gg",
        "\
Alice
hi",
    );
}

#[test]
fn trait_inherit_defaults() {
    run_gg(
        "trait_inherit_defaults.gg",
        "\
5
10
105",
    );
}

#[test]
fn file_io() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_dir = manifest_dir.join("tests/fixtures");

    run_gg(
        "file_io.gg",
        "\
true
hello world
hello world
second line
from File struct
from File struct
false",
    );

    // Clean up test files created by the fixture
    let _ = std::fs::remove_file(fixture_dir.join("_test_output.txt"));
    let _ = std::fs::remove_file(fixture_dir.join("_test_output2.txt"));
}

// ══════════════════════════════════════════════════════════════
// Formatter idempotency tests
// ══════════════════════════════════════════════════════════════

/// Format a .gg fixture twice and assert the second pass produces the same
/// output as the first (idempotency). Uses the library API directly.
fn assert_fmt_idempotent(fixture: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);

    let source = std::fs::read_to_string(&fixture_path)
        .unwrap_or_else(|e| panic!("Cannot read {}: {e}", fixture_path.display()));

    let first = gorget::formatter::format_source(&source);
    let second = gorget::formatter::format_source(&first);

    assert_eq!(
        first, second,
        "Formatter is NOT idempotent for {fixture}.\n\
         === First pass ===\n{first}\n\
         === Second pass ===\n{second}"
    );
}

#[test]
fn fmt_idempotent() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = manifest_dir.join("tests/fixtures");

    for entry in std::fs::read_dir(&fixtures_dir).expect("cannot read fixtures dir") {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("gg") {
            let name = path.file_name().unwrap().to_str().unwrap();
            assert_fmt_idempotent(name);
        }
    }
}
