use std::path::PathBuf;
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
sum called",
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
before
after",
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
    run_gg("closures.gg", "closures");
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
100",
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
