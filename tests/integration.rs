use std::path::{Path, PathBuf};
use std::process::Command;

use gorget::lexer::Lexer;
use gorget::lexer::token::{StringKind, StringLiteral, StringSegment, Token};
use gorget::parser::ast::*;
use gorget::span::Spanned;

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
fn dot_shorthand() {
    run_gg(
        "dot_shorthand.gg",
        "\
red
blue 42
fn green
made 7
done",
    );
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
fn cstr_basic() {
    run_gg(
        "cstr_basic.gg",
        "\
Hello from C
via function
coerced to str
str to cstr",
    );
}

#[test]
fn str_fat_ptr() {
    run_gg(
        "str_fat_ptr.gg",
        "\
hello
world",
    );
}

#[test]
fn str_codepoint_len() {
    run_gg(
        "str_codepoint_len.gg",
        "\
5
5
4
5
2
6
0
0",
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
fn type_alias_usage() {
    run_gg(
        "type_alias_usage.gg",
        "\
42
50",
    );
}

#[test]
fn type_alias_complex() {
    run_gg(
        "type_alias_complex.gg",
        "\
3
1
3",
    );
}

#[test]
fn type_alias_fn_sig() {
    run_gg("type_alias_fn_sig.gg", "10");
}

#[test]
fn type_alias_callback() {
    run_gg(
        "type_alias_callback.gg",
        "\
7
12",
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
7
-42
3
60
203
closures");
}

#[test]
fn closure_escape() {
    run_gg("closure_escape.gg", "\
15
8
21
12");
}

#[test]
fn fn_trait() {
    run_gg("fn_trait.gg", "\
10
21
12
done");
}

#[test]
fn fn_mut_once() {
    run_gg("fn_mut_once.gg", "\
10
21
12
15
107
36
16
18
done");
}

#[test]
fn closure_kind_error() {
    check_gg_fails(
        "closure_kind_error.gg",
        "closure kind mismatch: expected `Callable`, found `MutCallable`",
    );
}

#[test]
fn closure_move_kind_error() {
    check_gg_fails(
        "closure_move_kind_error.gg",
        "closure kind mismatch: expected `MutCallable`, found `ConsumeCallable`",
    );
}

#[test]
fn consume_callable_once() {
    run_gg("consume_callable_once.gg", "10\n101\ndone");
}

#[test]
fn consume_callable_once_error() {
    check_gg_fails("consume_callable_once_error.gg", "moved more than once");
}

#[test]
fn consume_callable_loop_error() {
    check_gg_fails("consume_callable_loop_error.gg", "cannot move");
}

#[test]
fn dynamic_dispatch() {
    run_gg("dynamic_dispatch.gg", "hello\nhola\nhello\nhola");
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

#[test]
fn newtype_field_access() {
    run_gg(
        "newtype_field_access.gg",
        "\
3.140000
42",
    );
}

#[test]
fn newtype_fn_sig() {
    run_gg("newtype_fn_sig.gg", "150");
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
fn bare_tuples() {
    run_gg(
        "bare_tuples.gg",
        "\
10
20
hello
42
true
10
20
1
10
2
20
3
30
99",
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
fn int_range() {
    run_gg(
        "int_range.gg",
        "\
255
0
-128
127
65535
-32768",
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
fn match_option_result() {
    run_gg(
        "match_option_result.gg",
        "\
42
none
100
fail
is some
is none",
    );
}

#[test]
fn option_assign() {
    run_gg("option_assign.gg", "hello");
}

#[test]
fn match_generic_methods() {
    run_gg(
        "match_generic_methods.gg",
        "\
2
10",
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
fn is_bindings() {
    run_gg(
        "is_bindings.gg",
        "\
42
oops
not failure
100
10
11
12
done
compound_guard:42
guard_failed
none_compound
multi_is:5:10
elif_else:bad
multi_elif:err
mixed_chain:mixed
fallthrough_else",
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

#[test]
fn ownership_showcase() {
    run_gg(
        "ownership_showcase.gg",
        "\
3
3
Alice Hello 1
alive
Alice Hello 5
sent Hello by Alice
Bob Reply 2
done",
    );
}

// ══════════════════════════════════════════════════════════════
// Module / import tests
// ══════════════════════════════════════════════════════════════

/// Build and run a `.gg` fixture, passing extra args to the compiled binary.
fn run_gg_with_args(fixture: &str, binary_args: &[&str], expected: &str) {
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

    // 1. Build
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

    // 2. Execute with args
    let run = Command::new(&exe_path)
        .args(binary_args)
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

    // 4. Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

/// Build and run a `.gg` fixture, piping `stdin_data` to the binary.
fn run_gg_with_stdin(fixture: &str, stdin_data: &str, expected: &str) {
    use std::io::Write;
    use std::process::Stdio;

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

    // 1. Build
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

    // 2. Execute with stdin
    let mut child = Command::new(&exe_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute compiled binary");

    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin_data.as_bytes())
        .unwrap();

    let output = child.wait_with_output().expect("failed to wait on child");
    let stdout = String::from_utf8_lossy(&output.stdout);

    // 3. Assert stdout
    assert_eq!(
        stdout.trim(),
        expected.trim(),
        "Output mismatch for {fixture}:\nExpected:\n{expected}\nGot:\n{stdout}",
    );

    assert!(
        output.status.success(),
        "Binary exited with error for {fixture}: {:?}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr),
    );

    // 4. Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

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
fn modules_struct() {
    run_gg_dir("modules_struct", "main.gg", "5");
}

#[test]
fn modules_enum() {
    run_gg_dir("modules_enum", "main.gg", "red");
}

#[test]
fn modules_struct_return() {
    run_gg_dir("modules_struct_return", "main.gg", "0\n0");
}

#[test]
fn modules_trait() {
    run_gg_dir("modules_trait", "main.gg", "woof");
}

#[test]
fn modules_auto() {
    run_gg_dir("modules_auto", "main.gg", "49");
}

#[test]
fn modules_pkg() {
    run_gg_dir("modules_pkg", "main.gg", "hello world");
}

#[test]
fn self_host_lexer() {
    run_gg_dir(
        "self_host_lexer",
        "main.gg",
        "\
kw:int ident:x = int:42 NL EOF
kw:int ident:add ( kw:int ident:a , kw:int ident:b ) : NL INDENT kw:return ident:a + ident:b NL DEDENT EOF
ident:x == ident:y != ident:z <= ident:w >= ident:v NL EOF
int:255 int:63 int:10 float:3.14 NL EOF
ident:print ( str:hello {name} ) NL EOF
kw:char ident:c = str:a NL EOF
comment:this is a comment ident:x = int:1 NL EOF
ident:f ( ident:a , ident:b ) NL EOF
ident:a += int:1 NL ident:b -= int:2 NL ident:c ..= ident:d NL EOF
kw:if kw:true kw:and kw:not kw:false : NL INDENT kw:return kw:None NL DEDENT EOF",
    );
}

#[test]
fn self_host_parser() {
    run_gg_dir(
        "self_host_parser",
        "main.gg",
        "\
=== function ===
int add(int a, int b): return (a + b);
=== struct ===
struct Point: int x; int y;
=== enum ===
enum Color: Red; Green; Blue(int);
=== import ===
from std.collections import Vector
=== expr_body ===
int double(int x) = (x * 2)
=== vardecl ===
void f(): int x = 42;
=== if_else ===
void f(): if (x > 0): print(x); else: print(0);;
=== for_loop ===
void f(): for i in range(10): print(i);;
=== match ===
void f(): match x: case 1: print(1); case _: print(0);;
=== method_call ===
void f(): v.push(42);
=== assign ===
void f(): x = 10;",
    );
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
fn hashmap_string_keys() {
    run_gg(
        "hashmap_string_keys.gg",
        "\
42
found
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
fn nested_generics() {
    run_gg(
        "nested_generics.gg",
        "\
2
1
2
3
4
10
2
2
2
4",
    );
}

#[test]
fn generic_struct_methods() {
    run_gg(
        "generic_struct_methods.gg",
        "\
42
hello
42
10
world
99",
    );
}

#[test]
fn generic_method_chain() {
    run_gg(
        "generic_method_chain.gg",
        "\
42
hello
42
equal
not equal
10",
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
99
val",
    );
}

#[test]
fn result_question_operator() {
    run_gg("result_question.gg", "84\n-1\nis error\n52\ndone");
}

#[test]
fn result_str_concat() {
    run_gg("result_str_concat.gg", "file not found: test.txt");
}

#[test]
fn result_map() {
    run_gg(
        "result_map.gg",
        "\
20
0
11
yes
4",
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
fn derive() {
    run_gg(
        "derive.gg",
        "\
equal
not equal
Point(x=1.000000, y=2.000000)
Point(x=1.000000, y=2.000000)
colors equal
colors differ
Red()
Blue(42)
Red()",
    );
}

#[test]
fn derive_hashable() {
    run_gg(
        "derive_hashable.gg",
        "\
int hash consistent
str hash nonzero
same fields same hash
diff fields diff hash
red != green
red != blue",
    );
}

#[test]
fn derive_generic() {
    run_gg(
        "derive_generic.gg",
        "\
pair equal
Pair(first=10, second=20)
Pair(first=10, second=20)
wrapper equal
wrapper not equal
Value(42)
Empty()
hash ok",
    );
}

#[test]
fn default_trait() {
    run_gg(
        "default_trait.gg",
        "\
0.000000
0.000000
0
0
false

0.000000
0",
    );
}

#[test]
fn from_trait() {
    run_gg(
        "from_trait.gg",
        "\
98.600000
42
5
5",
    );
}

#[test]
fn try_from_trait() {
    run_gg(
        "try_from_trait.gg",
        "\
98.600000
50
over 100
negative",
    );
}

#[test]
fn from_trait_multi() {
    run_gg(
        "from_trait_multi.gg",
        "\
42
99",
    );
}

#[test]
fn serializable() {
    run_gg(
        "serializable.gg",
        "\
{\"name\":\"Alice\",\"age\":30,\"active\":true}
\"Red\"
{\"Custom\":[255,128,0]}
{\"label\":\"admin\",\"user\":{\"name\":\"Bob\",\"age\":25,\"active\":false}}",
    );
}

#[test]
fn deserializable() {
    run_gg(
        "deserializable.gg",
        "\
{\"name\":\"Alice\",\"age\":30,\"active\":true}
\"Red\"
{\"Custom\":[255,128,0]}
{\"label\":\"admin\",\"user\":{\"name\":\"Bob\",\"age\":25,\"active\":false}}",
    );
}

#[test]
fn serialize_collections() {
    run_gg(
        "serialize_collections.gg",
        "\
{\"name\":\"Hawks\",\"scores\":[10,20]}
{\"name\":\"Hawks\",\"scores\":[10,20]}
{\"env\":\"prod\",\"settings\":{\"timeout\":30,\"retries\":3}}
{\"env\":\"prod\",\"settings\":{\"timeout\":30,\"retries\":3}}
{\"tags\":[\"bug\",\"urgent\"],\"metadata\":{\"author\":\"alice\"}}
{\"tags\":[\"bug\",\"urgent\"],\"metadata\":{\"author\":\"alice\"}}",
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
fn drop_reassign() {
    run_gg(
        "drop_reassign.gg",
        "\
drop first
alive: second
drop second",
    );
}

#[test]
fn drop_move_zero() {
    run_gg(
        "drop_move_zero.gg",
        "\
hello
after move",
    );
}

#[test]
fn drop_block_scope() {
    run_gg(
        "drop_block_scope.gg",
        "\
drop if-var
after if
drop branch-2
after elif
drop case-1
after match
conditional string
done",
    );
}

#[test]
fn drop_struct_fields() {
    run_gg(
        "drop_struct_fields.gg",
        "\
created wrapper
created container
created config
drop container box
drop inner nested
drop inner auto",
    );
}

#[test]
fn drop_collections() {
    run_gg(
        "drop_collections.gg",
        "\
done
drop boxed
drop elem-a
drop elem-b",
    );
}

#[test]
fn drop_struct_collection_fields() {
    run_gg(
        "drop_struct_collection_fields.gg",
        "\
len 3
got first len=2
drop old len=2
after set: new
wrapper id=1
nested len=2
drop old-inner len=0
after nested container set
done
drop new-inner len=0
drop wrapped len=1
drop wrapped len=1
drop new len=1
drop new len=1
drop first len=2
drop first len=2
drop second len=1
drop third len=1",
    );
}

#[test]
fn drop_field_move_zero() {
    run_gg(
        "drop_field_move_zero.gg",
        "\
pushed 2
extracted 3
taken 3
chained 1
done",
    );
}

#[test]
fn drop_fn_return_collection() {
    run_gg(
        "drop_fn_return_collection.gg",
        "\
2
1
1
30
done",
    );
}

#[test]
fn move_type_fn_arg() {
    run_gg(
        "move_type_fn_arg.gg",
        "60",
    );
}

#[test]
fn move_fn_arg_last_use() {
    run_gg(
        "move_fn_arg_last_use.gg",
        "60",
    );
}

#[test]
fn move_fn_arg_not_last_use() {
    run_gg(
        "move_fn_arg_not_last_use.gg",
        "\
3
3",
    );
}

#[test]
fn move_type_unwrap() {
    run_gg(
        "move_type_unwrap.gg",
        "\
1
2
2",
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
fn generic_trait_equip() {
    run_gg(
        "generic_trait_equip.gg",
        "\
42
42
7
70
7
70",
    );
}

#[test]
fn file_io() {
    run_gg(
        "file_io.gg",
        "\
true
hello world
hello world
second line
from File struct
from File struct
false
false
false",
    );
}

#[test]
fn generic_functions() {
    run_gg("generic_functions.gg", "42\n3.140000\nhello\n10\n7");
}

#[test]
fn trait_bounds() {
    run_gg("trait_bounds.gg", "num");
}

#[test]
fn vector_capacity() {
    run_gg(
        "vector_capacity.gg",
        "\
0
2
1
2
2
20
30",
    );
}

#[test]
fn vector_higher_order() {
    run_gg(
        "vector_higher_order.gg",
        "\
2
2
4
5
2
10
15
15",
    );
}

#[test]
fn struct_field_methods() {
    run_gg(
        "struct_field_methods.gg",
        "\
3
10
20
30",
    );
}

#[test]
fn dict_higher_order() {
    run_gg(
        "dict_higher_order.gg",
        "\
90
2
65",
    );
}

#[test]
fn set_higher_order() {
    run_gg(
        "set_higher_order.gg",
        "\
100
3
90",
    );
}

#[test]
fn named_args() {
    run_gg(
        "named_args.gg",
        "\
3
13
12
alice is 25
bob is 30
carol is 40
9
1024",
    );
}

#[test]
fn raw_strings() {
    run_gg(
        "raw_strings.gg",
        "\
C:\\Users\\test
\\d+\\.\\d+
no {interp} here",
    );
}

#[test]
fn multiline_strings() {
    run_gg(
        "multiline_strings.gg",
        "\
hello
world
one line",
    );
}

#[test]
fn string_stdlib() {
    run_gg(
        "string_stdlib.gg",
        "\
true
true
false
true
false
true
hi
HELLO, WORLD!
hello, world!
Hello, Gorget!
3",
    );
}

#[test]
fn string_strip() {
    run_gg(
        "string_strip.gg",
        "hello\nhello  \n  hello\nhello\nhelloxyy\nxxyhello\n\n[]\nhello",
    );
}

#[test]
fn str_byte_slice() {
    run_gg(
        "str_byte_slice.gg",
        "\
hello
world
caf
é",
    );
}

#[test]
fn string_indexing() {
    run_gg(
        "string_indexing.gg",
        "\
h
o
el
he
a
b
c
é
4
caf
o",
    );
}

#[test]
fn str_codepoint_index() {
    run_gg(
        "str_codepoint_index.gg",
        "\
c
a
f
é
4
ca
fé
你
好
2
é
f
a
é
b",
    );
}

#[test]
fn string_concat() {
    run_gg(
        "string_concat.gg",
        "\
hello world
hi there
foobar
abc
hello world",
    );
}

#[test]
fn string_owned() {
    run_gg(
        "string_owned.gg",
        "\
hello
0
hello world
hello world
abcdef
abcdef!
0
foobar
hi there
HELLO
hi
42
3.14
A
11",
    );
}

#[test]
fn string_coerce_args() {
    run_gg(
        "string_coerce_args.gg",
        "\
contains: yes
starts_with: no
ends_with: yes
in: yes
6
hello wow",
    );
}

#[test]
fn struct_string_coerce() {
    run_gg(
        "struct_string_coerce.gg",
        "\
hello
5
label
value
5",
    );
}

#[test]
fn in_operator() {
    run_gg(
        "in_operator.gg",
        "\
true
false
true
false
true
false
true
false",
    );
}

// ══════════════════════════════════════════════════════════════
// Runtime safety tests (expected panics)
// ══════════════════════════════════════════════════════════════

/// Build and run a `.gg` fixture, asserting the binary panics with the expected stderr message.
fn run_gg_panics(fixture: &str, expected_stderr: &str) {
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

    // 2. Execute the compiled binary — expect it to fail
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    assert!(
        !run.status.success(),
        "Expected panic but binary succeeded for {fixture}",
    );

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains(expected_stderr),
        "Stderr mismatch for {fixture}:\nExpected to contain: {expected_stderr}\nGot: {stderr}",
    );

    // 3. Clean up generated files
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn assert_basic() {
    run_gg("assert_basic.gg", "all asserts passed");
}

#[test]
fn assert_fails() {
    run_gg_panics("assert_fails.gg", "this should fail");
}

#[test]
fn bounds_check() {
    run_gg("bounds_check.gg", "true\n20\ntrue\ntrue\n");
}

#[test]
fn string_index_oob() {
    run_gg_panics("string_index_oob.gg", "str index out of bounds");
}

#[test]
fn div_by_zero() {
    run_gg_panics("div_by_zero.gg", "division by zero");
}

#[test]
fn iterator_trait() {
    run_gg("iterator.gg", "\
0
1
2
3
4
10
11
12");
}

#[test]
fn iterable_trait() {
    run_gg("iterable.gg", "\
1
2
3
1
2
3
empty");
}

#[test]
fn iterator_adapters() {
    run_gg("iterator_adapters.gg", "\
5
3
0
0
4
0
4
10
106");
}

#[test]
fn linked_list() {
    run_gg(
        "linked_list.gg",
        "\
3
10
10
20
30
60
20
40
60",
    );
}

/// Build and run a `.gg` fixture with extra CLI flags, asserting its stdout matches `expected`.
fn run_gg_with_flags(fixture: &str, flags: &[&str], expected: &str) {
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

    // 1. Build: cargo run -- build <flags> <fixture>
    let mut build_args = vec!["run", "--quiet", "--", "build"];
    build_args.extend(flags.iter());
    let build = Command::new(env!("CARGO"))
        .args(&build_args)
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
fn overflow_add() {
    run_gg_panics("overflow_add.gg", "integer overflow");
}

#[test]
fn overflow_sub() {
    run_gg_panics("overflow_sub.gg", "integer overflow");
}

#[test]
fn overflow_mul() {
    run_gg_panics("overflow_mul.gg", "integer overflow");
}

#[test]
fn overflow_wrap() {
    run_gg_with_flags("overflow_wrap.gg", &["--overflow=wrap"], "-9223372036854775808");
}

#[test]
fn string_format() {
    run_gg(
        "string_format.gg",
        "\
hello world
no interp
42
10 + 20 = 30
HELLO
value is 99
hello from gorget
hi world
no interp here
coerced 42",
    );
}

#[test]
fn wrapping_ops() {
    run_gg("wrapping_ops.gg", "-9223372036854775808\n9223372036854775807\n-2\n-9223372036854775808");
}

#[test]
fn bitwise_ops() {
    run_gg("bitwise_ops.gg", "1\n7\n6\n-1\n16\n4\n15\n63\n30\n120\n15\n7\n16");
}

// ══════════════════════════════════════════════════════════════
// Directive tests
// ══════════════════════════════════════════════════════════════

/// Build and run a `.gg` fixture with extra CLI flags, asserting it panics with expected stderr.
fn run_gg_panics_with_flags(fixture: &str, flags: &[&str], expected_stderr: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);

    assert!(
        fixture_path.exists(),
        "Fixture not found: {}",
        fixture_path.display()
    );

    let stem = fixture_path.file_stem().unwrap().to_str().unwrap();
    let dir = fixture_path.parent().unwrap();
    let _c_path = dir.join(format!("{stem}.c"));
    let exe_path = dir.join(stem);

    // 1. Build with flags
    let mut build_args = vec!["run", "--quiet", "--", "build"];
    build_args.extend(flags.iter());
    let build = Command::new(env!("CARGO"))
        .args(&build_args)
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed for {fixture}:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr),
    );

    // 2. Execute — expect panic
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    assert!(
        !run.status.success(),
        "Expected panic but binary succeeded for {fixture}",
    );

    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains(expected_stderr),
        "Expected stderr to contain '{expected_stderr}' for {fixture}, got: {stderr}",
    );
}

#[test]
fn directive_strip_asserts() {
    run_gg("use_strip_asserts.gg", "directives work");
}

#[test]
fn directive_overflow_wrap() {
    run_gg("use_overflow_wrap.gg", "-9223372036854775808");
}

#[test]
fn directive_cli_override_no_strip_asserts() {
    // Source says `directive strip-asserts` but CLI says `--no-strip-asserts` → asserts kept → panic
    run_gg_panics_with_flags("use_strip_asserts.gg", &["--no-strip-asserts"], "this would fail without strip-asserts");
}

#[test]
fn directive_cli_override_overflow_checked() {
    // Source says `directive overflow=wrap` but CLI says `--overflow=checked` → checked → panic
    run_gg_panics_with_flags("use_overflow_wrap.gg", &["--overflow=checked"], "integer overflow");
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

// ══════════════════════════════════════════════════════════════
// Semantic error tests (expected check failures)
// ══════════════════════════════════════════════════════════════

/// Run `gg check` on a fixture and assert it fails with a specific error message.
fn check_gg_fails(fixture: &str, expected_stderr: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures").join(fixture);

    assert!(
        fixture_path.exists(),
        "Fixture not found: {}",
        fixture_path.display()
    );

    let output = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "check"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        !output.status.success(),
        "Expected `gg check` to fail for {fixture}, but it succeeded.\nstdout: {}",
        String::from_utf8_lossy(&output.stdout),
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(expected_stderr),
        "Expected stderr to contain '{expected_stderr}' for {fixture}, got:\n{stderr}",
    );
}

#[test]
fn immutable_by_default() {
    run_gg(
        "immutable_by_default.gg",
        "\
20
40
50",
    );
}

#[test]
fn immutable_by_default_error() {
    check_gg_fails(
        "immutable_by_default_error.gg",
        "cannot assign to immutable variable `x`",
    );
}

#[test]
fn const_assign_error() {
    check_gg_fails(
        "const_assign_error.gg",
        "cannot assign to constant `x`",
    );
}

#[test]
fn mutable_borrow_params() {
    run_gg(
        "mutable_borrow_params.gg",
        "\
5
8
0
0
20
10",
    );
}

#[test]
fn recursive_enum() {
    run_gg(
        "recursive_enum.gg",
        "object with 4 keys\n\
         name = Alice\n\
         tags has 2 items\n\
         first = a\n\
         active = true\n\
         null ok\n\
         done",
    );
}

#[test]
fn option_box_enum() {
    run_gg(
        "option_box_enum.gg",
        "\
42
0
7",
    );
}

#[test]
fn toml_parse() {
    run_gg(
        "toml_parse.gg",
        "\
TOML Example
42
3.140000
true
localhost
8080
server.pem
3
80
8080
prod
2
apple
banana
255
63
10
1000
Tom
Preston
true
true
true
true
true
true
false
true
true
false
false
error caught
99
A
B
done",
    );
}

#[test]
fn xml_parse() {
    run_gg(
        "xml_parse.gg",
        "\
greeting
Hello
Alice
30
2
two
br
0
a & b < c
x&y
data
2
a
b
<root><child/></root>
hello world
val
AB
CD
<b>
true
done",
    );
}

#[test]
fn yaml_parse() {
    run_gg(
        "yaml_parse.gg",
        "\
Alice
30
true
apple
banana
cherry
3
localhost
8080
one
two
1
2
10
20
30
hello world
it's fine
true
true
true
false
42
-7
3.140000
just a string
value
Hello
[10, 20, 30]
name: Alice
age: 30
active: true
true
false
true
false
true
true
true
true
true
3
name
true
literal:
line 1
line 2
line 3

folded:
This is a paragraph.

Another one.

strip:
no trailing
end-strip
keep:
keep trailing



end-keep
This is a multi-line plain scalar.
val
first item
multi line

second
folded-indent:
paragraph
  indented line
back to normal

pretty-block:
desc: |
  multi
  line

name: test
2
1
2
2
a
b
1
value
--- {first: 1}
--- {second: 2}
---
first: 1
---
second: 2
Alice
Alice
first
second
first
localhost
3000
dev_db
99
2
1
2
3
42
42
255
15
10
1000000
-255
150.000000
42
true
true
true
true
_100
true
line1
line2
a\\b\"c
true
true
true
true
true
true
ok
value1
value2
true
0
true
0
value#not-comment
value
second
true
true
true
true
true
true
false
false
1
2
3
deep
5
null
true
42
done",
    );
}

#[test]
fn json_parse() {
    run_gg(
        "json_parse.gg",
        "\
Alice
30
true
true
false
10
20
30
3
true
[10,20,30]
true
0
true
false
true
false
false
false
false
true
A
Hi
Abc
done",
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

// ── Examples (programs under examples/) ─────────────────────────

/// Build and run an example, asserting its stdout matches `expected`.
/// Handles both single-file (`examples/foo.gg`) and multi-file
/// (`examples/foo/main.gg`) layouts.
fn run_example(name: &str, expected: &str) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let examples_dir = manifest_dir.join("examples");

    // Determine source path: directory with main.gg, or standalone .gg file
    let (source_path, c_path, exe_path) = {
        let dir_path = examples_dir.join(name);
        if dir_path.is_dir() {
            let main = dir_path.join("main.gg");
            let c = dir_path.join("main.c");
            let exe = dir_path.join("main");
            (main, c, exe)
        } else {
            let gg = examples_dir.join(format!("{name}.gg"));
            let c = examples_dir.join(format!("{name}.c"));
            let exe = examples_dir.join(name);
            (gg, c, exe)
        }
    };

    assert!(
        source_path.exists(),
        "Example not found: {}",
        source_path.display()
    );

    // 1. Build
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build"])
        .arg(&source_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed for examples/{name}:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr),
    );

    // 2. Execute
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    let stdout = String::from_utf8_lossy(&run.stdout);

    // 3. Assert stdout
    assert_eq!(
        stdout.trim(),
        expected.trim(),
        "Output mismatch for examples/{name}:\nExpected:\n{expected}\nGot:\n{stdout}",
    );

    assert!(
        run.status.success(),
        "Binary exited with error for examples/{name}: {:?}\nstderr: {}",
        run.status.code(),
        String::from_utf8_lossy(&run.stderr),
    );

    // 4. Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn example_hello() {
    run_example("hello", "Hello, World!");
}

#[test]
fn example_basics() {
    run_example("basics", "\
positive
0
1
2
3
4
5
6
7
8
9
add(3, 4) = 7
double(5) = 10
name = gorget");
}

#[test]
fn example_fibonacci() {
    run_example("fibonacci", "\
Fibonacci sequence:
  fib(0) = 0
  fib(1) = 1
  fib(2) = 1
  fib(3) = 2
  fib(4) = 3
  fib(5) = 5
  fib(6) = 8
  fib(7) = 13
  fib(8) = 21
  fib(9) = 34
  fib(10) = 55
  fib(11) = 89
  fib(12) = 144
  fib(13) = 233
  fib(14) = 377
  fib(15) = 610
  fib(16) = 987
  fib(17) = 1597
  fib(18) = 2584
  fib(19) = 4181
Recursive check:
  fib(0) = 0
  fib(1) = 1
  fib(2) = 1
  fib(3) = 2
  fib(4) = 3
  fib(5) = 5
  fib(6) = 8
  fib(7) = 13
  fib(8) = 21
  fib(9) = 34
  fib(10) = 55
  fib(11) = 89
  fib(12) = 144
  fib(13) = 233
  fib(14) = 377
All checks passed.");
}

#[test]
fn example_fizzbuzz() {
    let mut lines = Vec::new();
    for i in 1..=100 {
        if i % 15 == 0 {
            lines.push("FizzBuzz".to_string());
        } else if i % 3 == 0 {
            lines.push("Fizz".to_string());
        } else if i % 5 == 0 {
            lines.push("Buzz".to_string());
        } else {
            lines.push(i.to_string());
        }
    }
    run_example("fizzbuzz", &lines.join("\n"));
}

#[test]
fn example_inference() {
    run_example("inference", "\
yes
0
1
2
3
4
other");
}

#[test]
fn example_comprehensive() {
    run_example("comprehensive", "\
positive
0
1
2
3
4
5
6
7
8
9
result: 7
two
done");
}

#[test]
fn example_ownership() {
    run_example("ownership", "\
Priority: 3, backup: 3
[Preview] sender=Alice subject=Meeting tomorrow priority=1
[Preview] sender=Alice subject=Meeting tomorrow priority=5
[Sent] Meeting tomorrow by Alice
[Preview] sender=Bob subject=Re: Meeting tomorrow priority=2
done");
}

#[test]
fn example_sieve() {
    run_example("sieve", "\
Primes up to 100 (25 found):
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29
  31
  37
  41
  43
  47
  53
  59
  61
  67
  71
  73
  79
  83
  89
  97");
}

#[test]
fn example_iterator_demo() {
    run_example("iterator_demo", "\
Counting by 3s:
  0
  3
  6
  9
  12
  15
  18
Even numbers 1..20:
  2
  4
  6
  8
  10
  12
  14
  16
  18
Fibonacci (first 10):
  0
  1
  1
  2
  3
  5
  8
  13
  21
  34
Sum of first 20 Fibonacci numbers: 10945
Squares of 1..5:
  1
  4
  9
  16
  25");
}

#[test]
fn example_linked_list() {
    run_example("linked_list", "\
3
10
10
20
30
60
20
40
60");
}

#[test]
fn example_shapes() {
    run_example("shapes", "\
circle(r=5) area=75
rect(3x4) area=12
circumference=30
square
s1 area=300
s2 area=42
circle wins");
}

#[test]
fn example_calculator() {
    run_example("calculator", "\
2 + 3 = 5
(2 + 3) * 4 = 20
-7 = -7
1 + 2 + 3 = 6
(3 + 4) * (2 + 5) = 49");
}

#[test]
fn example_todo_app() {
    run_example("todo_app", "\
All tasks:
[x] Write parser
[ ] Implement codegen
[ ] Add error messages
[ ] Write docs
[x] Release v1.0
total: 5
done: 2
pending: 3
high priority: 2
[x] Implement codegen");
}

#[test]
fn example_ecs() {
    run_example("ecs", "\
=== Turn 1 ===
Knight attacks Orc for 30 damage (50 HP left)
Archer attacks Orc for 20 damage (30 HP left)
Orc attacks Knight for 25 damage (75 HP left)
Goblin attacks Knight for 15 damage (60 HP left)
=== Turn 2 ===
Knight attacks Goblin for 30 damage (20 HP left)
Archer attacks Orc for 20 damage (10 HP left)
Orc attacks Knight for 25 damage (35 HP left)
Goblin attacks Archer for 15 damage (45 HP left)
=== Turn 3 ===
Knight attacks Goblin for 30 damage (defeated)
Archer attacks Orc for 20 damage (defeated)
Heroes win!
Knight: 35/100 HP
Archer: 45/60 HP");
}

#[test]
fn example_pipeline() {
    run_example("pipeline", "\
Class roster:
  Alice (95*)
  Bob (67)
  Carol (82*)
  Dave (45)
  Eve (91*)
  Frank (73)
  Grace (88*)
  Hank (56)
count: 8
sum: 597
max: 95
min: 45
honors: 4
passing: 6
passing avg: 82
above 80: 4
top: Alice
top: Eve
top: Grace");
}

// ══════════════════════════════════════════════════════════════
// Builtin function tests
// ══════════════════════════════════════════════════════════════

#[test]
fn path_funcs() {
    run_gg(
        "path_funcs.gg",
        "\
/usr/local
/usr/local
.
/
bin
bin
file.txt
gz


jpg
archive.tar
README
.hidden
photo
usr/local/bin
/usr/local
a/b",
    );
}

#[test]
fn path_normalize() {
    run_gg(
        "path_normalize.gg",
        "\
/a/c/d
c
/a/b
.
.
/",
    );
}

#[test]
fn readdir() {
    run_gg("readdir.gg", "2");
}

#[test]
fn cli_args() {
    run_gg_with_args("cli_args.gg", &["hello", "world"], "\
3
hello
world");
}

#[test]
fn exec_builtin() {
    run_gg("exec_builtin.gg", "\
0
has_path
42");
}

#[test]
fn print_builtin() {
    run_gg("print_builtin.gg", "hello world");
}

#[test]
fn char_methods() {
    run_gg("char_methods.gg", "\
true
false
false
true
true
false
true
false
65
A
42
-7");
}

#[test]
fn builtins_interactive() {
    run_gg("builtins_interactive.gg", "\
5
91
time ok
done");
}

#[test]
fn math_stdlib() {
    run_gg("math_stdlib.gg", "\
42
10
3
7
2.000000
1024.000000
3.000000
4.000000
4.000000
3.000000
0.000000
1.000000
3.000000
3.000000
2.500000
1.500000
2.500000");
}

#[test]
fn io_input() {
    run_gg_with_stdin("io_input.gg", "world\nAlice\n", "\
got: world
name? hello Alice");
}

#[test]
fn conv_stdlib() {
    run_gg("conv_stdlib.gg", "\
42
int_err
3.140000
-0.500000
float_err
42
-100
2.5
1000
true
false
A
42
none
3.140000
none
0
empty_none
overflow_none
127
65535
neg0_ok
150.000000
99
77
0.000000
false
str_default::");
}

#[test]
fn random_stdlib() {
    run_gg("random_stdlib.gg", "\
a_ok
b_ok
5");
}

#[test]
fn os_stdlib() {
    run_gg("os_stdlib.gg", "\
cwd_ok
platform_ok
hello123");
}

#[test]
fn fs_ops() {
    run_gg("fs_ops.gg", "\
true
true
false
5
-1
true
true
true
true
true");
}

#[test]
fn time_stdlib() {
    run_gg("time_stdlib.gg", "\
time_ms_ok
ms_reasonable");
}

#[test]
fn via_delegation() {
    run_gg("via_delegation.gg", "\
inner
custom
7");
}

// ══════════════════════════════════════════════════════════════
// Trace tests
// ══════════════════════════════════════════════════════════════

#[test]
fn trace_directive() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/trace_test.gg");
    let dir = fixture_path.parent().unwrap();
    let c_path = dir.join("trace_test.c");
    let exe_path = dir.join("trace_test");
    let trace_path = dir.join("trace_test.trace.jsonl");

    // 1. Build
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed for trace_test.gg:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr),
    );

    // 2. Execute the compiled binary
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout.trim(), "6", "factorial(3) should print 6");

    assert!(
        run.status.success(),
        "Binary exited with error:\nstderr: {}",
        String::from_utf8_lossy(&run.stderr),
    );

    // 3. Verify trace file exists and contains expected entries
    assert!(trace_path.exists(), "Trace file should be created");

    let trace_content = std::fs::read_to_string(&trace_path)
        .expect("Failed to read trace file");
    let lines: Vec<&str> = trace_content.lines().collect();

    // Count event types rather than checking by line index
    let calls: Vec<_> = lines.iter().filter(|l| l.contains(r#""type":"call""#)).collect();
    let returns: Vec<_> = lines.iter().filter(|l| l.contains(r#""type":"return""#)).collect();
    let branches: Vec<_> = lines.iter().filter(|l| l.contains(r#""type":"branch""#)).collect();
    let stmt_starts: Vec<_> = lines.iter().filter(|l| l.contains(r#""type":"stmt_start""#)).collect();
    let stmt_ends: Vec<_> = lines.iter().filter(|l| l.contains(r#""type":"stmt_end""#)).collect();

    assert_eq!(calls.len(), 3, "Should have 3 calls (factorial(3), factorial(2), factorial(1))");
    assert_eq!(returns.len(), 3, "Should have 3 structural returns");
    assert_eq!(branches.len(), 1, "Should have 1 branch (if n <= 1 taken for n=1)");
    assert_eq!(stmt_starts.len(), stmt_ends.len(), "stmt_start/stmt_end should be balanced");
    assert!(stmt_starts.len() >= 4, "Should have stmt_start events for return stmts + let");

    // Verify first event is stmt_start for `auto result = factorial(3)`
    assert!(lines[0].contains(r#""type":"stmt_start""#), "First line should be stmt_start");
    assert!(lines[0].contains(r#""depth":0"#), "First stmt_start at depth 0");

    // Verify calls use Gorget names (not C-mangled)
    assert!(calls[0].contains(r#""fn":"factorial""#), "Should use Gorget name");
    assert!(calls[0].contains(r#""n":3"#), "First call should have n=3");

    // 4. Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&trace_path);
}

#[test]
fn trace_cli_flag() {
    // Test --trace flag on a file WITHOUT the directive
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/functions.gg");
    let dir = fixture_path.parent().unwrap();
    let c_path = dir.join("functions.c");
    let exe_path = dir.join("functions");
    let trace_path = dir.join("functions.trace.jsonl");

    // Build with --trace
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build", "--trace"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed:\nstderr: {}",
        String::from_utf8_lossy(&build.stderr),
    );

    // Execute
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    assert!(run.status.success());

    // Trace file should exist
    assert!(trace_path.exists(), "Trace file should be created with --trace flag");

    let trace_content = std::fs::read_to_string(&trace_path)
        .expect("Failed to read trace file");
    assert!(!trace_content.is_empty(), "Trace file should not be empty");
    // Should contain function calls with Gorget names (not gg_ prefixed)
    assert!(
        !trace_content.contains("gg_"),
        "Trace should use Gorget names, not C-mangled names"
    );

    // Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&trace_path);
}

#[test]
fn trace_no_trace_flag() {
    // Test --no-trace overrides directive trace
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/trace_test.gg");
    let dir = fixture_path.parent().unwrap();
    let c_path = dir.join("trace_test.c");
    let exe_path = dir.join("trace_test");
    let trace_path = dir.join("trace_test.trace.jsonl");

    // Build with --no-trace (overrides directive)
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build", "--no-trace"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Build failed:\nstderr: {}",
        String::from_utf8_lossy(&build.stderr),
    );

    // Execute
    let run = Command::new(&exe_path)
        .output()
        .expect("failed to execute compiled binary");

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout.trim(), "6", "factorial(3) should still print 6");

    // Trace file should NOT exist
    assert!(
        !trace_path.exists(),
        "--no-trace should prevent trace file creation"
    );

    // Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(&trace_path);
}

#[test]
fn trace_in_test_mode() {
    // Test --trace flag works with gg test
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/test_basic.gg");
    let dir = fixture_path.parent().unwrap();
    let trace_path = dir.join("test_basic.trace.jsonl");

    // Run with gg test --trace
    let run = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "test", "--trace"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        run.status.success(),
        "gg test --trace failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr),
    );

    // Trace file should exist
    assert!(trace_path.exists(), "Trace file should be created with --trace in test mode");

    let trace_content = std::fs::read_to_string(&trace_path)
        .expect("Failed to read trace file");
    let lines: Vec<&str> = trace_content.lines().collect();

    // Should contain test_start and test_end events for each test
    let test_starts: Vec<&&str> = lines.iter().filter(|l| l.contains(r#""type":"test_start""#)).collect();
    let test_ends: Vec<&&str> = lines.iter().filter(|l| l.contains(r#""type":"test_end""#)).collect();

    assert_eq!(test_starts.len(), 3, "Should have 3 test_start events (one per test)");
    assert_eq!(test_ends.len(), 3, "Should have 3 test_end events (one per test)");

    // Verify test names appear in events
    assert!(test_starts[0].contains(r#""name":"addition works""#), "First test_start should be 'addition works'");
    assert!(test_starts[1].contains(r#""name":"string equality""#), "Second test_start should be 'string equality'");
    assert!(test_starts[2].contains(r#""name":"boolean logic""#), "Third test_start should be 'boolean logic'");

    // Verify test_end events have pass status and duration
    for end_line in &test_ends {
        assert!(end_line.contains(r#""status":"pass""#), "All tests should pass: {end_line}");
        assert!(end_line.contains(r#""duration_ms":"#), "test_end should include duration: {end_line}");
    }

    // Verify ordering: each test_start is followed by its test_end
    assert!(test_ends[0].contains(r#""name":"addition works""#), "First test_end should be 'addition works'");
    assert!(test_ends[1].contains(r#""name":"string equality""#), "Second test_end should be 'string equality'");
    assert!(test_ends[2].contains(r#""name":"boolean logic""#), "Third test_end should be 'boolean logic'");

    // Clean up
    let _ = std::fs::remove_file(&trace_path);
}

#[test]
fn vector_first_last() {
    run_gg(
        "vector_first_last.gg",
        "\
30
20
true
true
20
10
30
30
1
-1
3
1
2
3
done",
    );
}

#[test]
fn vector_sort() {
    run_gg(
        "vector_sort.gg",
        "\
1
1
3
4
5
3
2
1
2
5
8
5",
    );
}

#[test]
fn vector_methods2() {
    run_gg(
        "vector_methods2.gg",
        "\
1
true
15
5
7
60
3
10
20
true
false
true
false
true
false",
    );
}

#[test]
fn vector_literal() {
    run_gg(
        "vector_literal.gg",
        "\
4
10
40
99
179
found",
    );
}

#[test]
fn vector_concat() {
    run_gg(
        "vector_concat.gg",
        "\
5
1
2
3
4
5
3
2",
    );
}

#[test]
fn dict_keys_values() {
    run_gg(
        "dict_keys_values.gg",
        "\
alice
bob
carol
30
25
35
90
3",
    );
}

#[test]
fn dict_items() {
    run_gg(
        "dict_items.gg",
        "\
3
60",
    );
}

#[test]
fn dict_order_remove() {
    run_gg(
        "dict_order_remove.gg",
        "\
1
3
2
10
30
99",
    );
}

#[test]
fn hashmap_unordered() {
    run_gg(
        "hashmap_unordered.gg",
        "\
3
100
200
300
true
false
2
400",
    );
}

#[test]
fn dict_tombstone_stress() {
    run_gg(
        "dict_tombstone_stress.gg",
        "\
21
110
105",
    );
}

#[test]
fn dict_literal() {
    run_gg(
        "dict_literal.gg",
        "\
3
30
25
35
0
200
3",
    );
}

#[test]
fn dict_subscript() {
    run_gg(
        "dict_subscript.gg",
        "\
10
20
99
30
3
100
300
3",
    );
}

#[test]
fn dict_get_or_put() {
    run_gg(
        "dict_get_or_put.gg",
        "\
1
1
42
42
3
2
2
1",
    );
}

#[test]
fn set_operations() {
    run_gg(
        "set_operations.gg",
        "\
6
2
has 3
has 4
2
has 1
has 2",
    );
}

#[test]
fn string_methods2() {
    run_gg(
        "string_methods2.gg",
        "\
hello
world
h
6
true
3
0
hahaha

a, b, c
a-b-c",
    );
}

#[test]
fn string_methods3() {
    run_gg(
        "string_methods3.gg",
        "\
world
hello world
config
config.toml
00042
42
42
hi...
hi",
    );
}

#[test]
fn char_methods2() {
    run_gg(
        "char_methods2.gg",
        "\
A
z
false
true
true
false
false
false
true
false
true
false",
    );
}

#[test]
fn char_method_on_index() {
    run_gg(
        "char_method_on_index.gg",
        "\
true
false
true
true
true
1
h
true
true",
    );
}

#[test]
fn option_expect() {
    run_gg(
        "option_expect.gg",
        "\
42
100",
    );
}

#[test]
fn dict_update() {
    run_gg(
        "dict_update.gg",
        "\
3
1
20
30
1
99",
    );
}

#[test]
fn set_subset() {
    run_gg(
        "set_subset.gg",
        "\
true
false
true
true
false
true",
    );
}

#[test]
fn dict_struct_field() {
    run_gg(
        "dict_struct_field.gg",
        "\
3
30
has bob
no nobody
35
3
2
100
has y",
    );
}

// ─── Test Framework Integration Tests ────────────────────────

/// Run `gg test` on a fixture, assert stdout contains expected, and check exit code.
fn run_gg_test(fixture: &str, expected_fragments: &[&str], expect_success: bool) {
    run_gg_test_with_tags(fixture, &[], expected_fragments, expect_success);
}

/// Run `gg test` with optional `--tag` flags.
fn run_gg_test_with_tags(
    fixture: &str,
    tags: &[&str],
    expected_fragments: &[&str],
    expect_success: bool,
) {
    run_gg_test_with_flags(fixture, tags, &[], None, expected_fragments, expect_success);
}

/// Run `gg test` with optional `--tag`, `--exclude-tag`, and `--filter` flags.
fn run_gg_test_with_flags(
    fixture: &str,
    tags: &[&str],
    exclude_tags: &[&str],
    filter: Option<&str>,
    expected_fragments: &[&str],
    expect_success: bool,
) {
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

    // Build args: cargo run -- test <fixture> [--tag <tag>]... [--exclude-tag <tag>]... [--filter <substr>]
    let mut args: Vec<&str> = vec!["run", "--quiet", "--", "test"];
    args.push(fixture_path.to_str().unwrap());
    for tag in tags {
        args.push("--tag");
        args.push(tag);
    }
    for tag in exclude_tags {
        args.push("--exclude-tag");
        args.push(tag);
    }
    if let Some(f) = filter {
        args.push("--filter");
        args.push(f);
    }

    let output = Command::new(env!("CARGO"))
        .args(&args)
        .output()
        .expect("failed to run cargo");

    let stdout = String::from_utf8_lossy(&output.stdout);

    for fragment in expected_fragments {
        assert!(
            stdout.contains(fragment),
            "Expected fragment {fragment:?} not found in output:\n{stdout}",
        );
    }

    if expect_success {
        assert!(
            output.status.success(),
            "Expected success for {fixture} but got {:?}\nstdout: {stdout}\nstderr: {}",
            output.status.code(),
            String::from_utf8_lossy(&output.stderr),
        );
    } else {
        assert!(
            !output.status.success(),
            "Expected failure for {fixture} but got success\nstdout: {stdout}",
        );
    }

    // Clean up
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn test_basic() {
    run_gg_test(
        "test_basic.gg",
        &["3 passed, 0 failed", "PASS"],
        true,
    );
}

#[test]
fn test_failure() {
    run_gg_test(
        "test_failure.gg",
        &["1 passed, 1 failed", "FAIL: assertion failed: left == right", "left:  1", "right: 2"],
        false,
    );
}

#[test]
fn test_suite_setup_teardown() {
    run_gg_test(
        "test_suite.gg",
        &["SETUP", "TEARDOWN", "2 passed, 0 failed"],
        true,
    );
}

#[test]
fn test_tag_filtering() {
    run_gg_test_with_tags(
        "test_tags.gg",
        &["smoke"],
        &["1 passed, 0 failed", "smoke test"],
        true,
    );
}

#[test]
fn test_process() {
    run_gg_test(
        "test_process.gg",
        &["2 passed, 0 failed"],
        true,
    );
}

#[test]
fn test_cleanup() {
    run_gg_test(
        "test_cleanup.gg",
        &["dropping alpha", "dropping beta", "1 passed, 1 failed"],
        false,
    );
}

#[test]
fn test_with_clause() {
    run_gg_test(
        "test_with_clause.gg",
        &[
            "dropping alpha",
            "dropping beta",
            "dropping delta",
            "dropping gamma",
            "2 passed, 1 failed",
        ],
        false,
    );
}

#[test]
fn test_coexist_build_mode() {
    // gg build/run should use main(), ignore test blocks
    run_gg("test_coexist.gg", "42");
}

#[test]
fn test_coexist_test_mode() {
    // gg test should run tests, ignore main()
    run_gg_test(
        "test_coexist.gg",
        &["2 passed, 0 failed"],
        true,
    );
}

#[test]
fn test_filter_by_name() {
    // --filter should only run tests whose name contains the substring
    run_gg_test_with_flags(
        "test_coexist.gg",
        &[], &[], Some("double works"),
        &["Running 1 tests", "1 passed, 0 failed", "double works"],
        true,
    );
}

#[test]
fn test_exclude_tag() {
    // --exclude-tag should skip tests with the excluded tag
    run_gg_test_with_flags(
        "test_tags.gg",
        &[], &["slow"], None,
        &["Running 2 tests", "2 passed, 0 failed", "smoke test", "untagged test"],
        true,
    );
}

#[test]
fn test_exclude_tag_wins_over_include() {
    // --exclude-tag wins: if a tag is both included and excluded, test is skipped
    run_gg_test_with_flags(
        "test_tags.gg",
        &["smoke"], &["smoke"], None,
        &["Running 0 tests", "0 passed, 0 failed"],
        true,
    );
}

#[test]
fn test_should_panic() {
    run_gg_test(
        "test_should_panic.gg",
        &["Running 3 tests", "3 passed, 0 failed", "PASS"],
        true,
    );
}

#[test]
fn test_running_count_header() {
    // All test outputs should include "Running N tests..." header
    run_gg_test(
        "test_basic.gg",
        &["Running 3 tests", "3 passed, 0 failed"],
        true,
    );
}

#[test]
fn test_timing_in_output() {
    // Test output should include timing in ms
    run_gg_test(
        "test_basic.gg",
        &["PASS (", "ms)"],
        true,
    );
}

// ── Report tests ─────────────────────────────────────────────

#[test]
fn test_report_subcommand() {
    // 1. Run `gg test --trace` to produce a trace file
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/test_basic.gg");
    let dir = fixture_path.parent().unwrap();
    let trace_path = dir.join("test_basic.trace.jsonl");
    let report_path = dir.join("test_basic.report.html");
    let c_path = dir.join("test_basic.c");
    let exe_path = dir.join("test_basic");

    // Clean up any leftover files
    let _ = std::fs::remove_file(&trace_path);
    let _ = std::fs::remove_file(&report_path);

    let run = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "test", "--trace"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run gg test --trace");

    assert!(
        run.status.success(),
        "gg test --trace failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr),
    );
    assert!(trace_path.exists(), "Trace file should exist after gg test --trace");

    // 2. Run `gg report` on the trace file
    let report_run = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "report"])
        .arg(&trace_path)
        .output()
        .expect("failed to run gg report");

    assert!(
        report_run.status.success(),
        "gg report failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&report_run.stdout),
        String::from_utf8_lossy(&report_run.stderr),
    );

    // 3. Verify report exists and contains expected content
    assert!(report_path.exists(), "report.html should exist after gg report");

    let html = std::fs::read_to_string(&report_path).expect("Failed to read report");
    assert!(html.contains("Test Report"), "Report should contain title");
    assert!(html.contains("addition works"), "Report should contain test name");
    assert!(html.contains("string equality"), "Report should contain test name");
    assert!(html.contains("boolean logic"), "Report should contain test name");
    assert!(html.contains("PASS"), "Report should contain PASS status");
    assert!(html.contains("3 passed"), "Report should show 3 passed");
    assert!(html.contains("0 failed"), "Report should show 0 failed");

    let stdout = String::from_utf8_lossy(&report_run.stdout);
    assert!(stdout.contains("Report:"), "Should print report path");

    // Clean up
    let _ = std::fs::remove_file(&trace_path);
    let _ = std::fs::remove_file(&report_path);
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn test_report_flag_on_test() {
    // Run `gg test --report html` — should auto-enable trace and produce both files
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/test_basic.gg");
    let dir = fixture_path.parent().unwrap();
    let trace_path = dir.join("test_basic.trace.jsonl");
    let report_path = dir.join("test_basic.report.html");
    let c_path = dir.join("test_basic.c");
    let exe_path = dir.join("test_basic");

    // Clean up any leftover files
    let _ = std::fs::remove_file(&trace_path);
    let _ = std::fs::remove_file(&report_path);

    let run = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "test", "--report", "html"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run gg test --report html");

    assert!(
        run.status.success(),
        "gg test --report html failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr),
    );

    // Both trace and report should exist
    assert!(trace_path.exists(), "Trace file should be auto-created by --report html");
    assert!(report_path.exists(), "Report file should be created by --report html");

    let html = std::fs::read_to_string(&report_path).expect("Failed to read report");
    assert!(html.contains("Test Report"), "Report should contain title");
    assert!(html.contains("PASS"), "Report should contain PASS status");
    assert!(html.contains("3 passed"), "Report should show 3 passed");

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(stdout.contains("Report:"), "Should print report path");

    // Clean up
    let _ = std::fs::remove_file(&trace_path);
    let _ = std::fs::remove_file(&report_path);
    let _ = std::fs::remove_file(&c_path);
    let _ = std::fs::remove_file(&exe_path);
}

#[test]
fn crypto_hash() {
    run_gg(
        "crypto_hash.gg",
        "\
2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d
32
aes roundtrip ok
32
aes error ok
hmac error ok
done",
    );
}

#[test]
fn crypto_x25519() {
    run_gg(
        "crypto_x25519.gg",
        "\
keys generated
shared secret matches
hkdf produced 32 bytes
ciphertext size correct
hello encrypted world",
    );
}

#[test]
fn socket_connect() {
    run_gg(
        "socket_connect.gg",
        "\
error handled
done",
    );
}

#[test]
fn httpserver_basic() {
    run_gg(
        "httpserver_basic.gg",
        "\
true
done",
    );
}

#[test]
fn httpserver_concurrent() {
    run_gg("httpserver_concurrent.gg", "2");
}

#[test]
fn httpserver_keepalive() {
    run_gg("httpserver_keepalive.gg", "true\ntrue");
}

#[test]
fn httpserver_router() {
    run_gg(
        "httpserver_router.gg",
        "200\nhello\n200\nposted\n404\n404\n200\nuser:42\n200\npost:7:comment:99\nhello\n2\n*\n302\n/new-location",
    );
}

#[test]
fn httpserver_middleware() {
    run_gg(
        "httpserver_middleware.gg",
        "200\nhello!\ngorget\nget\n404\ngorget\n200\npong",
    );
}

#[test]
fn httpserver_static() {
    run_gg(
        "httpserver_static.gg",
        "text/html\napplication/javascript\ntext/css\napplication/json\nimage/png\napplication/octet-stream\n200\ntext/html\n<h1>hello</h1>\n404\n400\n200\nindex",
    );
}

#[test]
fn httpserver_tls() {
    run_gg(
        "httpserver_tls.gg",
        "0.0.0.0\n8443\n/tmp/cert.pem\n/tmp/key.pem\n127.0.0.1\n8080",
    );
}

#[test]
fn httpserver_e2e() {
    // 6 checks × 2 lines each (status + body/header), all printing "ok"
    run_gg(
        "httpserver_e2e.gg",
        "ok\nok\nok\nok\nok\nok\nok\nok\nok\nok\nok",
    );
}

#[test]
fn httpserver_router_extended() {
    run_gg(
        "httpserver_router_extended.gg",
        "405\n404\n404\n404\npost:7\nbase-mw1-mw2-mw3",
    );
}

#[test]
fn httpserver_methods() {
    run_gg(
        "httpserver_methods.gg",
        "200\nput\n200\ndeleted\n200\npatched\n200\n\n200\nGET\n404\n404",
    );
}

#[test]
fn httpserver_large_body() {
    run_gg(
        "httpserver_large_body.gg",
        "ok\nok",
    );
}

#[test]
fn http_patch() {
    run_gg(
        "http_patch.gg",
        "ok\nok",
    );
}

#[test]
fn httpserver_protocol() {
    run_gg(
        "httpserver_protocol.gg",
        "ok\nok\nok\nok",
    );
}

#[test]
fn httpserver_chunked() {
    run_gg(
        "httpserver_chunked.gg",
        "ok\nok",
    );
}

#[test]
fn httpserver_before() {
    run_gg(
        "httpserver_before.gg",
        "ok\nok\nok\nok\nok",
    );
}

#[test]
fn httpserver_routing() {
    run_gg(
        "httpserver_routing.gg",
        "ok\nok\nok\nok\nok",
    );
}

#[test]
fn httpserver_body_parsers() {
    run_gg(
        "httpserver_body_parsers.gg",
        "ok\nok\nok\nok\nok",
    );
}

#[test]
fn httpserver_static_enhanced() {
    run_gg(
        "httpserver_static_enhanced.gg",
        "ok\nok\nok\nok\nok",
    );
}

#[test]
fn httpserver_lifecycle() {
    run_gg(
        "httpserver_lifecycle.gg",
        "ok\nok\nok",
    );
}

#[test]
fn httpserver_json() {
    run_gg(
        "httpserver_json.gg",
        "ok\nok\nok\nok\nok",
    );
}

#[test]
fn fstring_basic() {
    run_gg(
        "fstring_basic.gg",
        "{name}\nHello, Alice!\n\\n stays\nhello world\nHi, Bob!",
    );
}

#[test]
fn char_str_coerce() {
    run_gg("char_str_coerce.gg", "A\ntrue\nA");
}

#[test]
fn httpserver_response() {
    run_gg(
        "httpserver_response.gg",
        "\
200
text/plain
body
text/html
application/json
404
400
500
gorget
42
302
/target
OK
Not Found
Bad Request
Internal Server Error
Found
Unknown",
    );
}

#[test]
fn httpserver_query_string() {
    run_gg(
        "httpserver_query_string.gg",
        "0\nempty\nvalue\n1\n2\n3\na=b\nempty\nhello\n2",
    );
}

#[test]
fn httpserver_parse_request() {
    run_gg(
        "httpserver_parse_request.gg",
        "ok\nok\nok\nok\nok",
    );
}

#[test]
fn http_urls_extended() {
    run_gg(
        "http_urls_extended.gg",
        "\
host.com
80
/search?q=test&page=2
example.com
443
/path
host
80
/
host.com
80
/
done",
    );
}

#[test]
fn httpserver_e2e_extended() {
    run_gg(
        "httpserver_e2e_extended.gg",
        "ok\nok\nok\nok\nok\nok",
    );
}

#[test]
fn udp_echo() {
    run_gg(
        "udp_echo.gg",
        "hello p2p",
    );
}

#[test]
fn p2p_basic() {
    run_gg(
        "p2p_basic.gg",
        "\
node2 discovered peer
node1 discovered peer
hello p2p",
    );
}

#[test]
fn p2p_discovery() {
    run_gg(
        "p2p_discovery.gg",
        "\
both discovered
discovery works",
    );
}

#[test]
fn p2p_dht() {
    run_gg(
        "p2p_dht.gg",
        "\
peers connected
hello from DHT
hello from DHT",
    );
}

#[test]
fn p2p_nat() {
    run_gg(
        "p2p_nat.gg",
        "\
addr discovered
C discovered A via hole punch
A discovered C via hole punch
relayed msg",
    );
}

#[test]
fn p2p_gossip() {
    run_gg(
        "p2p_gossip.gg",
        "\
chat
hello gossip
hello gossip
second msg",
    );
}

#[test]
fn p2p_reliable_basic() {
    run_gg(
        "p2p_reliable_basic.gg",
        "\
peers connected
syn received
stream connected
hello reliable
stream closed
fin acked",
    );
}

#[test]
fn p2p_reliable_large() {
    run_gg(
        "p2p_reliable_large.gg",
        "\
received 12000 bytes
content verified
stream closed",
    );
}

#[test]
fn p2p_reliable_bidir() {
    run_gg(
        "p2p_reliable_bidir.gg",
        "\
A->B connected
B->A connected
hello from A
hello from B
both streams closed",
    );
}

#[test]
fn p2p_encrypted() {
    run_gg(
        "p2p_encrypted.gg",
        "\
peers connected
syn received
stream connected
encrypted: true
authenticated: true
hello encrypted
stream closed
fin acked",
    );
}

#[test]
fn p2p_encrypted_large() {
    run_gg(
        "p2p_encrypted_large.gg",
        "\
received 12000 bytes
content verified
stream closed",
    );
}

#[test]
fn p2p_multiplex() {
    run_gg(
        "p2p_multiplex.gg",
        "\
stream 1 connected
stream 2 connected
stream1: data channel
stream2: control channel
both closed",
    );
}

#[test]
fn p2p_stream_robust() {
    run_gg(
        "p2p_stream_robust.gg",
        "\
hello
world
!
graceful close ok",
    );
}

#[test]
fn p2p_protocol_rpc() {
    run_gg(
        "p2p_protocol_rpc.gg",
        "\
protocol: echo/1.0
query: echo/1.0
connected
request: ping
response: pong
error: not found
rpc done",
    );
}

#[test]
fn name_first() {
    run_gg(
        "name_first.gg",
        "\
7
10
3
gorget
1
done",
    );
}

#[test]
fn bytes_ops() {
    run_gg(
        "bytes_ops.gg",
        "\
5
Hello
Hello
48656c6c6f
305419896
2864434397
4660
305419896
7856341200000000
4660
34120000
Hello World
Hello
16
SGVsbG8=
Hello

caught error
done",
    );
}

#[test]
fn utf8_validation() {
    run_gg(
        "utf8_validation.gg",
        "\
hi
invalid UTF-8 in byte buffer
café
invalid UTF-8 in byte buffer",
    );
}

#[test]
fn unicode_strings() {
    run_gg(
        "unicode_strings.gg",
        "\
CAF\u{c9}
\u{03b5}\u{03bb}\u{03bb}\u{03b7}\u{03bd}\u{03b9}\u{03ba}\u{03ac}
\u{043c}\u{043e}\u{0441}\u{043a}\u{0432}\u{0430}
hello
hello
3
true
caf\u{e9}
hi
HELLO
cafe
true
true
3
3
\u{e9}",
    );
}

#[test]
fn string_iterators() {
    run_gg(
        "string_iterators.gg",
        "\
3
97
98
99
5
169
3
97
98
99
4
233
3
a
b
c
4
\u{e9}
c-a-f-\u{e9}-",
    );
}

#[test]
fn hot_reload_basic() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixture_path = manifest_dir.join("tests/fixtures/hot_reload_basic.gg");
    let dir = fixture_path.parent().unwrap();
    let stem = "hot_reload_basic";

    assert!(fixture_path.exists(), "Fixture not found: {}", fixture_path.display());

    // 1. Build with --hot-reload
    let build = Command::new(env!("CARGO"))
        .args(["run", "--quiet", "--", "build", "--hot-reload"])
        .arg(&fixture_path)
        .output()
        .expect("failed to run cargo");

    assert!(
        build.status.success(),
        "Hot-reload build failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr),
    );

    // 2. Run the host binary (it dlopen's the guest and runs init/tick)
    let exe_path = dir.join(stem);
    let run = Command::new(&exe_path)
        .current_dir(dir)
        .output()
        .expect("failed to execute hot-reload binary");

    let stdout = String::from_utf8_lossy(&run.stdout);

    // 3. Assert: init() creates State(0), tick() increments 3 times then exits
    // The recompile step prints "Built shared library: ..." to stdout, followed by "1\n2\n3"
    assert!(
        stdout.contains("1\n2\n3"),
        "Hot-reload output mismatch.\nExpected stdout to contain '1\\n2\\n3'.\nGot:\n{stdout}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stderr),
    );

    assert!(
        run.status.success(),
        "Hot-reload binary exited with error: {:?}\nstderr: {}",
        run.status.code(),
        String::from_utf8_lossy(&run.stderr),
    );

    // 4. Clean up generated files
    let _ = std::fs::remove_file(dir.join(format!("{stem}_host.c")));
    let _ = std::fs::remove_file(dir.join(format!("{stem}_guest.c")));
    let _ = std::fs::remove_file(dir.join(format!("{stem}_guest.dylib")));
    let _ = std::fs::remove_file(dir.join(format!("{stem}_guest.so")));
    let _ = std::fs::remove_file(&exe_path);
    let _ = std::fs::remove_file(dir.join(format!("{stem}.c")));
}

#[test]
fn box_callable() {
    run_gg("box_callable.gg", "\
10
21
101
done");
}

#[test]
fn generic_callable() {
    run_gg("generic_callable.gg", "\
10
val
14
done");
}

#[test]
fn callable_ref_param() {
    run_gg("callable_ref_param.gg", "\
1
2
2
done");
}

#[test]
fn generic_callable_ref() {
    run_gg("generic_callable_ref.gg", "\
1
restored
1
done");
}

#[test]
fn extern_ffi() {
    run_gg(
        "extern_ffi.gg",
        "\
42
5
0",
    );
}

#[test]
fn operator_overload() {
    run_gg(
        "operator_overload.gg",
        "\
4 6
-2 -2
-1 -2
11 22
small < big
big > small
small <= small
big >= small
10 20 30 40
99",
    );
}

#[test]
fn lifetime_basic() {
    run_gg(
        "lifetime_basic.gg",
        "\
hello
world
a
chained
hello
world
forwarded
live_a
all lifetime checks passed",
    );
}

#[test]
fn lifetime_dangling_error() {
    check_gg_fails(
        "lifetime_dangling_error.gg",
        "borrows from local variable",
    );
}

#[test]
fn lifetime_use_after_move_error() {
    check_gg_fails(
        "lifetime_use_after_move_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_struct() {
    run_gg(
        "lifetime_struct.gg",
        "\
literal
from param
mixed
literal
from param
mixed
struct lifetime ok",
    );
}

#[test]
fn lifetime_struct_error() {
    check_gg_fails(
        "lifetime_struct_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_branch_error() {
    check_gg_fails(
        "lifetime_branch_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_pattern_error() {
    check_gg_fails(
        "lifetime_pattern_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_temporary_error() {
    check_gg_fails(
        "lifetime_temporary_error.gg",
        "temporary",
    );
}

#[test]
fn lifetime_groups() {
    run_gg(
        "lifetime_groups.gg",
        "\
hello
world
done
",
    );
}

#[test]
fn lifetime_groups_error() {
    check_gg_fails(
        "lifetime_groups_error.gg",
        "must outlive",
    );
}

#[test]
fn lifetime_method() {
    run_gg(
        "lifetime_method.gg",
        "hello",
    );
}

#[test]
fn lifetime_method_error() {
    check_gg_fails(
        "lifetime_method_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_method_temp_error() {
    check_gg_fails(
        "lifetime_method_temp_error.gg",
        "temporary",
    );
}

#[test]
fn lifetime_loop_error() {
    check_gg_fails(
        "lifetime_loop_error.gg",
        "cannot move",
    );
}

#[test]
fn lifetime_closure_error() {
    check_gg_fails(
        "lifetime_closure_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_closure_return_error() {
    check_gg_fails(
        "lifetime_closure_return_error.gg",
        "borrows from local variable",
    );
}

#[test]
fn lifetime_closure_cross_fn_error() {
    check_gg_fails(
        "lifetime_closure_cross_fn_error.gg",
        "after source",
    );
}

#[test]
fn lifetime_reassign() {
    run_gg(
        "lifetime_reassign.gg",
        "world",
    );
}

#[test]
fn lifetime_reassign_error() {
    check_gg_fails(
        "lifetime_reassign_error.gg",
        "after source",
    );
}

#[test]
fn measurable_trait() {
    run_gg(
        "measurable_trait.gg",
        "\
3
5
42
3
5
42",
    );
}

#[test]
fn print_trait_object() {
    run_gg(
        "print_trait_object.gg",
        "\
gear
7
3.140000
true",
    );
}

#[test]
fn time_format() {
    run_gg(
        "time_format.gg",
        "\
2026-01-15 12:30:00
true
-1
2026-01-15
12:30:00",
    );
}

#[test]
fn math_constants() {
    run_gg(
        "math_constants.gg",
        "\
true
true
true
true
true
true
true
true",
    );
}

#[test]
fn string_builder() {
    run_gg(
        "string_builder.gg",
        "\
hello world
11
false
true
first
second
42!
3.14
256
true false
100
2.72
true
z
",
    );
}

#[test]
fn option_result_combinators() {
    run_gg(
        "option_result_combinators.gg",
        "\
42
99
42
true
42
77
84
43
100
true
true
10
99
fail
mapped
10
77
20",
    );
}

#[test]
fn enumerate() {
    run_gg(
        "enumerate.gg",
        "\
0: hello
1: world
2: foo
0: a
1: b
2: c
done",
    );
}

#[test]
fn regex_basic() {
    run_gg(
        "regex_basic.gg",
        "\
true
false
user@example.com
8
24
3
user
example
com
3
hello
world
foo
abc NUM def 456
abc NUM def NUM
4
a
b
c
d
2025
01
15
3
3
hello\\.world\\[0\\]
true
42
abc NUM def
123
no fullmatch
compile error caught
true
hello
3
a
b
c,d,e
done",
    );
}

#[test]
fn regex_extended() {
    run_gg(
        "regex_extended.gg",
        "\
99
11
2
item
42
done",
    );
}

#[test]
fn encoding_basic() {
    run_gg(
        "encoding_basic.gg",
        "\
hello%20world
a-b_c.d~e
a%3D1%26b%3D2
hello world
caught
hello world!
hello+world
hello world
&lt;b&gt;A &amp; B&lt;/b&gt;
<b>hi</b>
AB
CD
169
<div>
5
true
72
-1
2
72
105
2
65
66
Hi
done",
    );
}

#[test]
fn encoding_edge() {
    run_gg(
        "encoding_edge.gg",
        "\
true


%2Fpath%3Fq%3D1%26x%3D2
a+b%2Bc
caught
caught
8211
8212
8230
8364
163
&bogus;
trail&
true
0
-1
caught
4
done",
    );
}

#[test]
fn option_struct_field() {
    run_gg(
        "option_struct_field.gg",
        "\
hello (1)
no message (no priority)
world (no priority)",
    );
}

#[test]
fn option_struct_field_ordering() {
    run_gg(
        "option_struct_field_ordering.gg",
        "\
255
no color",
    );
}

#[test]
fn pattern_destructure() {
    run_gg(
        "pattern_destructure.gg",
        "\
10
20
42
hello
1
2
3",
    );
}

#[test]
fn pattern_destructure_loop() {
    run_gg(
        "pattern_destructure_loop.gg",
        "\
30
0 hello
1 world",
    );
}

#[test]
fn csv_basic() {
    run_gg(
        "csv_basic.gg",
        "\
2
a
b
c
1
2
3
a,b
c
he said \"hi\"
line1
line2
2
a
c
3
true
2
3
Alice
30
LA
true
false
2
-1
true
true
true
0
unterminated quoted field
2
1
done",
    );
}

#[test]
fn ecs_basics() {
    run_gg(
        "ecs_basics.gg",
        "\
0
1
2
2
0
2
2
2
true
true
100
false
50
2
75
1
false
10:100
20:200
30:300
10
200
true
false
1
1
50
none
0:50
5:99",
    );
}

// ══════════════════════════════════════════════════════════════
// New stdlib modules — Batch 2
// ══════════════════════════════════════════════════════════════

#[test]
fn uuid_basic() {
    run_gg(
        "uuid_basic.gg",
        "\
550e8400-e29b-41d4-a716-446655440000
36
4
-
-
-
-
true
false
4
4
true
done",
    );
}

#[test]
fn log_levels() {
    run_gg(
        "log_levels.gg",
        "\
[ERROR] visible error
[DEBUG] debug now visible
[INFO] info now visible
[WARN] warn now visible
[ERROR] error still visible
[INFO] info still visible
[ERROR] error still visible 2
done",
    );
}

#[test]
fn log_basic() {
    run_gg(
        "log_basic.gg",
        "\
[INFO] server started
[WARN] disk space low
[ERROR] connection refused
[DEBUG] trace point
[INFO] hello
[WARN] caution
[ERROR] boom
[DEBUG] debug visible
[INFO] info visible
[ERROR] only error shows
[INFO] app: started
done",
    );
}

#[test]
fn namespace_basic() {
    run_gg(
        "namespace_basic.gg",
        "\
info
42
[ERROR] boom
done",
    );
}

#[test]
fn term_basic() {
    run_gg(
        "term_basic.gg",
        "\
hello
world
important
abc
plain text
faint
link
warn
info
false
done",
    );
}

#[test]
fn cli_help() {
    run_gg(
        "cli_help.gg",
        "\
Usage: myapp
A test CLI application
Arguments:
  input  Input file to process
Options:
  --verbose, -v  Enable verbose output
  --output, -o  Output file path (default: out.txt)
---
Usage: tool
A utility tool
Options:
  --dry-run, -n  Simulate without changes
  --config, -c  Config file (default: config.toml)
  --timeout, -t  Timeout in seconds (default: 30)",
    );
}

#[test]
fn cli_basic() {
    run_gg(
        "cli_basic.gg",
        "\
true
true
result.txt
1
input.txt
default.txt
false
true
done",
    );
}

#[test]
fn heap_edges() {
    run_gg(
        "heap_edges.gg",
        "\
true
true
1
3
3
5
10
5
20
true
false
true
done",
    );
}

#[test]
fn heap_basic() {
    run_gg(
        "heap_basic.gg",
        "\
5
5
false
5
10
15
20
30
true
true
42
1.000000
done",
    );
}

#[test]
fn datetime_format() {
    run_gg(
        "datetime_format.gg",
        "\
2000-01-01
00:00:00
2024/07/15 14:30
Thursday
done",
    );
}

#[test]
fn datetime_basic() {
    run_gg(
        "datetime_basic.gg",
        "\
946684800
5
1
1
0
1
0
1
0
3
2000-01-01T00:00:00Z
86400
61
60
2000-01-02T00:00:00Z
2000-01-02T01:00:00Z
2000-01-01T00:01:30Z
1999-12-31T00:00:00Z
2000-01-01T01:30:00Z
done",
    );
}

// ══════════════════════════════════════════════════════════════
// New stdlib modules — Batch 3
// ══════════════════════════════════════════════════════════════

#[test]
fn math_trig() {
    run_gg(
        "math_trig.gg",
        "\
true
true
true
true
true
true
true
true
false
false
done",
    );
}

#[test]
fn log_set_level() {
    run_gg(
        "log_set_level.gg",
        "\
[ERROR] visible1
[INFO] visible2
[DEBUG] visible3
done",
    );
}

#[test]
fn cli_advanced() {
    run_gg(
        "cli_advanced.gg",
        "\
true
true
true
2
file1.txt
file2.txt
Usage: mytool
A sample tool
Arguments:
  input  Input file
Options:
  --verbose, -v  Be verbose
  --output, -o  Output path (default: out.txt)
done",
    );
}

#[test]
fn ecs_query2() {
    run_gg(
        "ecs_query2.gg",
        "\
1
0
0
2
done",
    );
}

#[test]
fn heap_advanced() {
    run_gg(
        "heap_advanced.gg",
        "\
1
2
3
apple
banana
cherry
3
3
5
5
5
1
2
3
4
5
6
7
8
9
10
done",
    );
}

#[test]
fn datetime_extended() {
    run_gg(
        "datetime_extended.gg",
        "\
31
29
31
30
31
30
31
31
30
31
30
31
28
0
1
365
10957
-1
-365
-366
-731
-86400
1969
12
31
1970
1
1
0
1970
1
2
2026
3
3
12
30
45
done",
    );
}

// ══════════════════════════════════════════════════════════════
// Lexer comparison: Rust vs self-hosting Gorget lexer
// ══════════════════════════════════════════════════════════════

/// Build a multi-file `.gg` fixture from a directory.
/// Returns (exe_path, c_path) — caller is responsible for cleanup.
fn build_gg_dir(dir_name: &str, main_file: &str) -> (PathBuf, PathBuf) {
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

    (exe_path, c_path)
}

/// Canonical Rust-side string literal formatter matching the Gorget describe_string_canonical.
fn escape_canonical_rust(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            '\\' => result.push_str("\\\\"),
            _ => result.push(c),
        }
    }
    result
}

fn describe_string_canonical_rust(slit: &StringLiteral) -> String {
    let prefix = match slit.kind {
        StringKind::Normal => "str:",
        StringKind::Format => "fstr:",
        StringKind::Raw => "rstr:",
        StringKind::MultiLine => "mstr:",
        StringKind::Byte => "bstr:",
        StringKind::CStr => "cstr:",
    };
    let mut result = prefix.to_string();
    for seg in &slit.segments {
        match seg {
            StringSegment::Literal(text) => result.push_str(&escape_canonical_rust(text)),
            StringSegment::Interpolation(expr) => {
                result.push('{');
                result.push_str(expr);
                result.push('}');
            }
        }
    }
    result
}

/// Canonical Rust-side token formatter matching the Gorget describe_token_canonical.
fn describe_token_canonical_rust(token: &Token) -> String {
    match token {
        Token::Keyword(kw) => format!("kw:{}", kw.as_name()),
        Token::Identifier(name) => format!("ident:{name}"),
        Token::IntLiteral(n) => format!("int:{n}"),
        Token::FloatLiteral(n) => format!("float:{n}"),
        Token::StringLiteral(slit) => describe_string_canonical_rust(slit),
        Token::BoolLiteral(b) => format!("bool:{b}"),
        Token::Plus => "+".into(),
        Token::Minus => "-".into(),
        Token::Star => "*".into(),
        Token::Slash => "/".into(),
        Token::Percent => "%".into(),
        Token::Eq => "=".into(),
        Token::Lt => "<".into(),
        Token::Gt => ">".into(),
        Token::Bang => "!".into(),
        Token::Ampersand => "&".into(),
        Token::Pipe => "|".into(),
        Token::Caret => "^".into(),
        Token::Tilde => "~".into(),
        Token::Dot => ".".into(),
        Token::Question => "?".into(),
        Token::At => "@".into(),
        Token::Underscore => "_".into(),
        Token::EqEq => "==".into(),
        Token::BangEq => "!=".into(),
        Token::LtEq => "<=".into(),
        Token::GtEq => ">=".into(),
        Token::LtLt => "<<".into(),
        Token::GtGt => ">>".into(),
        Token::LtLtEq => "<<=".into(),
        Token::GtGtEq => ">>=".into(),
        Token::AmpersandEq => "&=".into(),
        Token::PipeEq => "|=".into(),
        Token::CaretEq => "^=".into(),
        Token::PlusEq => "+=".into(),
        Token::Arrow => "->".into(),
        Token::MinusEq => "-=".into(),
        Token::StarEq => "*=".into(),
        Token::SlashEq => "/=".into(),
        Token::PercentEq => "%=".into(),
        Token::PlusPercent => "+%".into(),
        Token::MinusPercent => "-%".into(),
        Token::StarPercent => "*%".into(),
        Token::PlusPercentEq => "+%=".into(),
        Token::MinusPercentEq => "-%=".into(),
        Token::StarPercentEq => "*%=".into(),
        Token::DotDot => "..".into(),
        Token::DotDotEq => "..=".into(),
        Token::QuestionDot => "?.".into(),
        Token::DoubleQuestion => "??".into(),
        Token::LParen => "(".into(),
        Token::RParen => ")".into(),
        Token::LBracket => "[".into(),
        Token::RBracket => "]".into(),
        Token::LBrace => "lbrace".into(),
        Token::RBrace => "rbrace".into(),
        Token::Colon => ":".into(),
        Token::Comma => ",".into(),
        Token::Indent => "INDENT".into(),
        Token::Dedent => "DEDENT".into(),
        Token::Newline => "NL".into(),
        Token::DocComment(text) => format!("doc:{text}"),
        Token::Comment(text) => format!("comment:{text}"),
        Token::Eof => "EOF".into(),
        Token::Error(msg) => format!("error:{msg}"),
    }
}

/// Compare two canonical token strings, with float tolerance.
fn canonical_token_eq(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    // Float tolerance: parse both values and compare with relative epsilon.
    // C's %g uses 6 significant digits, so values may round differently from Rust's Display.
    if a.starts_with("float:") && b.starts_with("float:") {
        if let (Ok(va), Ok(vb)) = (a[6..].parse::<f64>(), b[6..].parse::<f64>()) {
            if va == vb {
                return true;
            }
            let max = va.abs().max(vb.abs());
            if max == 0.0 {
                return true;
            }
            return (va - vb).abs() / max < 1e-6;
        }
    }
    false
}

/// Returns true if a canonical token string is a comment or doc comment.
fn is_comment_token(s: &str) -> bool {
    s.starts_with("comment:") || s.starts_with("doc:")
}

#[test]
fn lexer_comparison() {
    // 1. Build the Gorget lexer driver
    let (driver_exe, driver_c) = build_gg_dir("self_host_lexer", "driver.gg");

    // 2. Discover all top-level .gg fixture files
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = manifest_dir.join("tests/fixtures");
    let mut fixtures: Vec<PathBuf> = std::fs::read_dir(&fixtures_dir)
        .expect("failed to read fixtures dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_file() && p.extension().map_or(false, |ext| ext == "gg"))
        .collect();
    fixtures.sort();

    assert!(
        !fixtures.is_empty(),
        "No .gg fixtures found in {}",
        fixtures_dir.display()
    );

    struct Mismatch {
        fixture: String,
        first_diff: usize,
        rust_len: usize,
        gorget_len: usize,
        rust_context: Vec<String>,
        gorget_context: Vec<String>,
    }

    let mut mismatches: Vec<Mismatch> = Vec::new();
    let mut crashes: Vec<(String, String)> = Vec::new();
    let mut compared = 0;

    // 3. For each fixture, compare Rust vs Gorget lexer output
    for fixture in &fixtures {
        let source = match std::fs::read_to_string(fixture) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "  SKIP {}: read error: {e}",
                    fixture.file_name().unwrap().to_string_lossy()
                );
                continue;
            }
        };

        // Rust side: lex with Gorget's Rust lexer
        let rust_tokens: Vec<String> = Lexer::new(&source)
            .map(|spanned| describe_token_canonical_rust(&spanned.node))
            .filter(|s| !is_comment_token(s))
            .collect();

        // Gorget side: run the driver binary
        let output = Command::new(&driver_exe).arg(fixture).output();

        match output {
            Ok(out) if out.status.success() => {
                let gorget_tokens: Vec<String> = String::from_utf8_lossy(&out.stdout)
                    .lines()
                    .filter(|s| !is_comment_token(s))
                    .map(|s| s.to_string())
                    .collect();

                // Find first divergence
                let mut first_diff = None;
                let max_len = rust_tokens.len().max(gorget_tokens.len());
                for i in 0..max_len {
                    let r = rust_tokens.get(i).map(|s| s.as_str()).unwrap_or("<missing>");
                    let g = gorget_tokens
                        .get(i)
                        .map(|s| s.as_str())
                        .unwrap_or("<missing>");
                    if !canonical_token_eq(r, g) {
                        first_diff = Some(i);
                        break;
                    }
                }

                if let Some(diff_idx) = first_diff {
                    // Collect context: 2 tokens before and 3 after the divergence
                    let start = diff_idx.saturating_sub(2);
                    let end = (diff_idx + 3).min(max_len);
                    let rust_context: Vec<String> = (start..end)
                        .map(|i| {
                            let prefix = if i == diff_idx { ">>  " } else { "    " };
                            format!(
                                "{prefix}[{i}] {}",
                                rust_tokens
                                    .get(i)
                                    .map(|s| s.as_str())
                                    .unwrap_or("<missing>")
                            )
                        })
                        .collect();
                    let gorget_context: Vec<String> = (start..end)
                        .map(|i| {
                            let prefix = if i == diff_idx { ">>  " } else { "    " };
                            format!(
                                "{prefix}[{i}] {}",
                                gorget_tokens
                                    .get(i)
                                    .map(|s| s.as_str())
                                    .unwrap_or("<missing>")
                            )
                        })
                        .collect();

                    mismatches.push(Mismatch {
                        fixture: fixture
                            .file_name()
                            .unwrap()
                            .to_string_lossy()
                            .to_string(),
                        first_diff: diff_idx,
                        rust_len: rust_tokens.len(),
                        gorget_len: gorget_tokens.len(),
                        rust_context,
                        gorget_context,
                    });
                }
            }
            Ok(out) => {
                let name = fixture
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                crashes.push((name, stderr));
            }
            Err(e) => {
                let name = fixture
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                crashes.push((name, format!("exec error: {e}")));
            }
        }
        compared += 1;
    }

    // 4. Cleanup
    let _ = std::fs::remove_file(&driver_c);
    let _ = std::fs::remove_file(&driver_exe);

    // 5. Report
    eprintln!("\n=== Lexer Comparison Results ===");
    eprintln!("Fixtures compared: {compared}");
    eprintln!("Crashes: {}", crashes.len());
    eprintln!("Mismatches: {}", mismatches.len());

    if !crashes.is_empty() {
        eprintln!("\n--- Crashes ---");
        for (name, err) in &crashes {
            let first_line = err.lines().next().unwrap_or("(no stderr)");
            eprintln!("  {name}: {first_line}");
        }
    }

    if !mismatches.is_empty() {
        eprintln!("\n--- Mismatches ---");
        for m in &mismatches {
            eprintln!(
                "\n  {} (first diff at token {}, rust={} gorget={} tokens)",
                m.fixture, m.first_diff, m.rust_len, m.gorget_len
            );
            eprintln!("  Rust:");
            for line in &m.rust_context {
                eprintln!("  {line}");
            }
            eprintln!("  Gorget:");
            for line in &m.gorget_context {
                eprintln!("  {line}");
            }
        }
    }

    // The test passes even with mismatches — this is a diagnostic/tracking test.
    // Mismatches are expected during development and guide Gorget lexer improvements.
    // Crashes indicate the Gorget driver can't handle a fixture at all.
    eprintln!("\n================================\n");
}

// ═══════════════════════════════════════════════════════════════
// Canonical AST Formatter (Rust side)
// Produces the same format as format.gg in the self-hosting parser.
// ═══════════════════════════════════════════════════════════════

fn format_primitive_canonical(p: &PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::Int => "int",
        PrimitiveType::Int8 => "int8",
        PrimitiveType::Int16 => "int16",
        PrimitiveType::Int32 => "int32",
        PrimitiveType::Int64 => "int64",
        PrimitiveType::Uint => "uint",
        PrimitiveType::Uint8 => "uint8",
        PrimitiveType::Uint16 => "uint16",
        PrimitiveType::Uint32 => "uint32",
        PrimitiveType::Uint64 => "uint64",
        PrimitiveType::Float => "float",
        PrimitiveType::Float32 => "float32",
        PrimitiveType::Float64 => "float64",
        PrimitiveType::Bool => "bool",
        PrimitiveType::Str => "str",
        PrimitiveType::CStr => "cstr",
        PrimitiveType::StringType => "String",
        PrimitiveType::Void => "void",
    }
}

fn format_type_canonical(ty: &Type) -> String {
    match ty {
        Type::Primitive(p) => format_primitive_canonical(p).to_string(),
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                name.node.clone()
            } else {
                let args: Vec<String> = generic_args
                    .iter()
                    .map(|a| format_type_canonical(&a.node))
                    .collect();
                format!("{}[{}]", name.node, args.join(", "))
            }
        }
        Type::Array { element, size } => {
            let elem = format_type_canonical(&element.node);
            let sz = format_expr_canonical(&size.node);
            format!("[{elem}; {sz}]")
        }
        Type::Slice { element } => {
            let elem = format_type_canonical(&element.node);
            format!("[{elem}]")
        }
        Type::Tuple(elems) => {
            let parts: Vec<String> = elems.iter().map(|e| format_type_canonical(&e.node)).collect();
            format!("({})", parts.join(", "))
        }
        Type::Function {
            return_type,
            params,
            ..
        } => {
            let ret = format_type_canonical(&return_type.node);
            let ps: Vec<String> = params.iter().map(|p| format_type_canonical(&p.node)).collect();
            format!("{ret}({})", ps.join(", "))
        }
        Type::SelfType => "Self".to_string(),
        Type::Inferred => "auto".to_string(),
    }
}

fn format_pattern_canonical(pat: &Pattern) -> String {
    match pat {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Binding(name) => name.clone(),
        Pattern::Literal(expr) => format_expr_canonical(&expr.node),
        Pattern::Constructor { path, fields } => {
            let name = path
                .iter()
                .map(|s| s.node.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if fields.is_empty() {
                name
            } else {
                let args: Vec<String> = fields
                    .iter()
                    .map(|f| format_pattern_canonical(&f.node))
                    .collect();
                format!("{name}({})", args.join(", "))
            }
        }
        Pattern::Tuple(elems) => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| format_pattern_canonical(&e.node))
                .collect();
            format!("({})", parts.join(", "))
        }
        Pattern::Or(alts) => {
            let parts: Vec<String> = alts
                .iter()
                .map(|a| format_pattern_canonical(&a.node))
                .collect();
            parts.join(" | ")
        }
        Pattern::Rest => "..".to_string(),
        Pattern::DotShorthand { variant, fields } => {
            if fields.is_empty() {
                format!(".{}", variant.node)
            } else {
                let args: Vec<String> = fields
                    .iter()
                    .map(|f| format_pattern_canonical(&f.node))
                    .collect();
                format!(".{}({})", variant.node, args.join(", "))
            }
        }
    }
}

fn format_binop_canonical(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Mod => "mod",
        BinaryOp::AddWrap => "+%",
        BinaryOp::SubWrap => "-%",
        BinaryOp::MulWrap => "*%",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::Eq => "==",
        BinaryOp::Neq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Gt => ">",
        BinaryOp::LtEq => "<=",
        BinaryOp::GtEq => ">=",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        BinaryOp::In => "in",
    }
}

fn format_compound_assign_canonical(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+=",
        BinaryOp::Sub => "-=",
        BinaryOp::Mul => "*=",
        BinaryOp::Div => "/=",
        BinaryOp::Rem => "%=",
        BinaryOp::AddWrap => "+%=",
        BinaryOp::SubWrap => "-%=",
        BinaryOp::MulWrap => "*%=",
        BinaryOp::BitAnd => "&=",
        BinaryOp::BitOr => "|=",
        BinaryOp::BitXor => "^=",
        BinaryOp::Shl => "<<=",
        BinaryOp::Shr => ">>=",
        _ => "?=",
    }
}

fn format_unaryop_canonical(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "not",
        UnaryOp::BitNot => "~",
        UnaryOp::Deref => "*",
    }
}

/// Flatten a StringLiteral to a plain string for canonical output.
fn flatten_string_literal(slit: &StringLiteral) -> String {
    let mut result = String::new();
    for seg in &slit.segments {
        match seg {
            StringSegment::Literal(text) => result.push_str(text),
            StringSegment::Interpolation(expr) => {
                result.push('{');
                result.push_str(expr);
                result.push('}');
            }
        }
    }
    result
}

fn format_expr_canonical(expr: &Expr) -> String {
    match expr {
        Expr::IntLiteral(n) => n.to_string(),
        Expr::FloatLiteral(f) => {
            // Match Gorget's float_to_str: use %g-style formatting
            let s = format!("{f}");
            // Ensure there's a decimal point for whole numbers
            if !s.contains('.') && !s.contains('e') && !s.contains('E') {
                format!("{s}.0")
            } else {
                s
            }
        }
        Expr::BoolLiteral(b) => if *b { "true" } else { "false" }.to_string(),
        Expr::StringLiteral(slit) => {
            let text = flatten_string_literal(slit);
            format!("\"{text}\"")
        }
        Expr::NoneLiteral => "None".to_string(),
        Expr::Identifier(name) => name.clone(),
        Expr::SelfExpr => "self".to_string(),
        Expr::It => "it".to_string(),
        Expr::Path { segments } => segments
            .iter()
            .map(|s| s.node.as_str())
            .collect::<Vec<_>>()
            .join("."),
        Expr::BinaryOp { left, op, right } => {
            let ls = format_expr_canonical(&left.node);
            let ops = format_binop_canonical(op);
            let rs = format_expr_canonical(&right.node);
            format!("({ls} {ops} {rs})")
        }
        Expr::UnaryOp { op, operand } => {
            let ops = format_unaryop_canonical(op);
            let os = format_expr_canonical(&operand.node);
            format!("({ops} {os})")
        }
        Expr::Call { callee, args, .. } => {
            let cs = format_expr_canonical(&callee.node);
            let arg_strs: Vec<String> = args.iter().map(|a| format_callarg_canonical(a)).collect();
            format!("{cs}({})", arg_strs.join(", "))
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let os = format_expr_canonical(&receiver.node);
            let arg_strs: Vec<String> = args.iter().map(|a| format_callarg_canonical(a)).collect();
            format!("{os}.{}({})", method.node, arg_strs.join(", "))
        }
        Expr::FieldAccess { object, field } => {
            let os = format_expr_canonical(&object.node);
            format!("{os}.{}", field.node)
        }
        Expr::TupleFieldAccess { object, index } => {
            let os = format_expr_canonical(&object.node);
            format!("{os}.{index}")
        }
        Expr::Index { object, index } => {
            let os = format_expr_canonical(&object.node);
            let is = format_expr_canonical(&index.node);
            format!("{os}[{is}]")
        }
        Expr::Range {
            start, end, ..
        } => {
            let mut result = String::new();
            if let Some(s) = start {
                result.push_str(&format_expr_canonical(&s.node));
            }
            result.push_str("..");
            if let Some(e) = end {
                result.push_str(&format_expr_canonical(&e.node));
            }
            result
        }
        Expr::NilCoalescing { lhs, rhs } => {
            let ls = format_expr_canonical(&lhs.node);
            let rs = format_expr_canonical(&rhs.node);
            format!("({ls} ?? {rs})")
        }
        Expr::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
        } => {
            // Expr::If has then_branch as Box<Spanned<Expr>> — wrap in a pseudo-block
            let cond = format_expr_canonical(&condition.node);
            let then_body = format!(" {};", format_expr_canonical(&then_branch.node));
            let mut result = format!("if {cond}:{then_body}");
            for (elif_cond, elif_body) in elif_branches {
                let ec = format_expr_canonical(&elif_cond.node);
                let eb = format!(" {};", format_expr_canonical(&elif_body.node));
                result.push_str(&format!(" elif {ec}:{eb}"));
            }
            if let Some(else_body) = else_branch {
                let eb = format!(" {};", format_expr_canonical(&else_body.node));
                result.push_str(&format!(" else:{eb}"));
            }
            result
        }
        Expr::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            let subj = format_expr_canonical(&scrutinee.node);
            let mut result = format!("match {subj}:");
            for arm in arms {
                let pat = format_pattern_canonical(&arm.pattern.node);
                let body = match &arm.body.node {
                    Expr::Block(block) => format_block_canonical(&block.stmts),
                    _ => format!(" {};", format_expr_canonical(&arm.body.node)),
                };
                result.push_str(&format!(" case {pat}:{body}"));
            }
            if let Some(else_body) = else_arm {
                let eb = match &else_body.node {
                    Expr::Block(block) => format_block_canonical(&block.stmts),
                    _ => format!(" {};", format_expr_canonical(&else_body.node)),
                };
                result.push_str(&format!(" case _:{eb}"));
            }
            result
        }
        Expr::Closure {
            params, body, ..
        } => {
            let param_strs: Vec<String> = params
                .iter()
                .map(|p| {
                    let cp = &p.node;
                    if let Some(ty) = &cp.type_ {
                        format!("{} {}", format_type_canonical(&ty.node), cp.name.node)
                    } else {
                        format!("auto {}", cp.name.node)
                    }
                })
                .collect();
            // If body is Block, unwrap to match Gorget's Vector[Stmt] representation
            let body_str = match &body.node {
                Expr::Block(block) => format_block_canonical(&block.stmts),
                _ => format!(" {};", format_expr_canonical(&body.node)),
            };
            format!("({}):{body_str}", param_strs.join(", "))
        }
        Expr::ImplicitClosure { body } => {
            // Gorget parser doesn't wrap implicit-it in closures, so just emit the body expression
            format_expr_canonical(&body.node)
        }
        Expr::Block(block) => {
            let body = format_block_canonical(&block.stmts);
            format!("block:{body}")
        }
        Expr::Do { body } => {
            let body_str = format_block_canonical(&body.stmts);
            format!("do:{body_str}")
        }
        Expr::ArrayLiteral(elems) => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| format_expr_canonical(&e.node))
                .collect();
            format!("[{}]", parts.join(", "))
        }
        Expr::TupleLiteral(elems) => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| format_expr_canonical(&e.node))
                .collect();
            format!("({})", parts.join(", "))
        }
        Expr::DictLiteral(pairs) => {
            let parts: Vec<String> = pairs
                .iter()
                .map(|(k, v)| {
                    format!(
                        "{}: {}",
                        format_expr_canonical(&k.node),
                        format_expr_canonical(&v.node)
                    )
                })
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        Expr::StructLiteral { name, args, .. } => {
            let arg_strs: Vec<String> = args
                .iter()
                .map(|a| format_expr_canonical(&a.node))
                .collect();
            format!("{}({})", name.node, arg_strs.join(", "))
        }
        Expr::Try { expr } => {
            format!("{}?", format_expr_canonical(&expr.node))
        }
        Expr::TryCapture { expr } => {
            format!("try {}", format_expr_canonical(&expr.node))
        }
        Expr::Move { expr } => {
            format!("!{}", format_expr_canonical(&expr.node))
        }
        Expr::MutableBorrow { expr } => {
            format!("&{}", format_expr_canonical(&expr.node))
        }
        Expr::Deref { expr } => {
            format!("*{}", format_expr_canonical(&expr.node))
        }
        Expr::As { expr, type_ } => {
            format!(
                "{} as {}",
                format_expr_canonical(&expr.node),
                format_type_canonical(&type_.node)
            )
        }
        Expr::ListComprehension {
            expr,
            variable,
            iterable,
            condition,
            ..
        } => {
            let e = format_expr_canonical(&expr.node);
            let var = format_pattern_canonical(&variable.node);
            let iter = format_expr_canonical(&iterable.node);
            let mut result = format!("[{e} for {var} in {iter}");
            if let Some(cond) = condition {
                result.push_str(&format!(" if {}", format_expr_canonical(&cond.node)));
            }
            result.push(']');
            result
        }
        Expr::Is {
            expr,
            negated,
            pattern,
        } => {
            let e = format_expr_canonical(&expr.node);
            let p = format_pattern_canonical(&pattern.node);
            if *negated {
                format!("({e} is not {p})")
            } else {
                format!("({e} is {p})")
            }
        }
        Expr::Await { expr } => {
            format!("{}.await()", format_expr_canonical(&expr.node))
        }
        Expr::Spawn { expr } => {
            format!("spawn {}", format_expr_canonical(&expr.node))
        }
        Expr::OptionalChain { object, field } => {
            let os = format_expr_canonical(&object.node);
            format!("{os}?.{}", field.node)
        }
        Expr::SetComprehension {
            expr,
            variable,
            iterable,
            condition,
        } => {
            let e = format_expr_canonical(&expr.node);
            let var = &variable.node;
            let iter = format_expr_canonical(&iterable.node);
            let mut result = format!("{{{e} for {var} in {iter}");
            if let Some(cond) = condition {
                result.push_str(&format!(" if {}", format_expr_canonical(&cond.node)));
            }
            result.push('}');
            result
        }
        Expr::DictComprehension {
            key,
            value,
            variables,
            iterable,
            condition,
        } => {
            let k = format_expr_canonical(&key.node);
            let v = format_expr_canonical(&value.node);
            let vars: Vec<&str> = variables.iter().map(|s| s.node.as_str()).collect();
            let iter = format_expr_canonical(&iterable.node);
            let mut result = format!("{{{k}: {v} for {} in {iter}", vars.join(", "));
            if let Some(cond) = condition {
                result.push_str(&format!(" if {}", format_expr_canonical(&cond.node)));
            }
            result.push('}');
            result
        }
        Expr::DotShorthand { variant, args } => {
            if args.is_empty() {
                format!(".{}", variant.node)
            } else {
                let arg_strs: Vec<String> = args
                    .iter()
                    .map(format_callarg_canonical)
                    .collect();
                format!(".{}({})", variant.node, arg_strs.join(", "))
            }
        }
        Expr::MetaOpInfix { left, op_name, right } => {
            format!(
                "{} meta[{}] {}",
                format_expr_canonical(&left.node),
                op_name,
                format_expr_canonical(&right.node)
            )
        }
        Expr::MetaOpToken(op) => format!("meta {:?}", op),
    }
}

fn format_callarg_canonical(arg: &Spanned<CallArg>) -> String {
    format_expr_canonical(&arg.node.value.node)
}

fn format_stmt_canonical(stmt: &Stmt) -> String {
    match stmt {
        Stmt::VarDecl {
            is_const,
            type_,
            pattern,
            value,
            ..
        } => {
            let ts = format_type_canonical(&type_.node);
            let name = format_pattern_canonical(&pattern.node);
            let vs = format_expr_canonical(&value.node);
            if *is_const {
                format!("const {ts} {name} = {vs}")
            } else {
                format!("{ts} {name} = {vs}")
            }
        }
        Stmt::Assign { target, value } => {
            format!(
                "{} = {}",
                format_expr_canonical(&target.node),
                format_expr_canonical(&value.node)
            )
        }
        Stmt::CompoundAssign { target, op, value } => {
            format!(
                "{} {} {}",
                format_expr_canonical(&target.node),
                format_compound_assign_canonical(op),
                format_expr_canonical(&value.node)
            )
        }
        Stmt::Expr(expr) => format_expr_canonical(&expr.node),
        Stmt::Return(Some(expr)) => format!("return {}", format_expr_canonical(&expr.node)),
        Stmt::Return(None) => "return".to_string(),
        Stmt::Throw(expr) => format!("throw {}", format_expr_canonical(&expr.node)),
        Stmt::Break(Some(expr)) => format!("break {}", format_expr_canonical(&expr.node)),
        Stmt::Break(None) => "break".to_string(),
        Stmt::Continue => "continue".to_string(),
        Stmt::Pass => "pass".to_string(),
        Stmt::For {
            pattern,
            iterable,
            body,
            ..
        } => {
            let pat = format_pattern_canonical(&pattern.node);
            let iter = format_expr_canonical(&iterable.node);
            let b = format_block_canonical(&body.stmts);
            format!("for {pat} in {iter}:{b}")
        }
        Stmt::While {
            condition, body, ..
        } => {
            let cond = format_expr_canonical(&condition.node);
            let b = format_block_canonical(&body.stmts);
            format!("while {cond}:{b}")
        }
        Stmt::Loop { body } => {
            let b = format_block_canonical(&body.stmts);
            format!("loop:{b}")
        }
        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => {
            let cond = format_expr_canonical(&condition.node);
            let b = format_block_canonical(&then_body.stmts);
            let mut result = format!("if {cond}:{b}");
            for (elif_cond, elif_body) in elif_branches {
                let ec = format_expr_canonical(&elif_cond.node);
                let eb = format_block_canonical(&elif_body.stmts);
                result.push_str(&format!(" elif {ec}:{eb}"));
            }
            if let Some(else_body) = else_body {
                let eb = format_block_canonical(&else_body.stmts);
                result.push_str(&format!(" else:{eb}"));
            }
            result
        }
        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            let subj = format_expr_canonical(&scrutinee.node);
            let mut result = format!("match {subj}:");
            for arm in arms.iter().filter_map(|i| i.arm()) {
                let pat = format_pattern_canonical(&arm.pattern.node);
                // Unwrap Block bodies to match Gorget's representation
                let body = match &arm.body.node {
                    Expr::Block(block) => format_block_canonical(&block.stmts),
                    _ => format!(" {};", format_expr_canonical(&arm.body.node)),
                };
                result.push_str(&format!(" case {pat}:{body}"));
            }
            if let Some(else_body) = else_arm {
                let eb = format_block_canonical(&else_body.stmts);
                result.push_str(&format!(" case _:{eb}"));
            }
            result
        }
        Stmt::With { bindings, body } => {
            // Use first binding (Gorget AST only supports one)
            if let Some(b) = bindings.first() {
                let res = format_expr_canonical(&b.expr.node);
                let name = &b.name.node;
                let body_str = format_block_canonical(&body.stmts);
                format!("with {res} as {name}:{body_str}")
            } else {
                "with ?".to_string()
            }
        }
        Stmt::Unsafe { body } => {
            let b = format_block_canonical(&body.stmts);
            format!("unsafe:{b}")
        }
        Stmt::Assert { condition, message } => {
            let cond = format_expr_canonical(&condition.node);
            if let Some(msg) = message {
                // Extract string text from message expr
                let msg_text = match &msg.node {
                    Expr::StringLiteral(slit) => flatten_string_literal(slit),
                    other => format_expr_canonical(other),
                };
                format!("assert {cond}, \"{msg_text}\"")
            } else {
                format!("assert {cond}")
            }
        }
        Stmt::Select { arms, else_arm } => {
            let mut s = "select:".to_string();
            for arm in arms {
                let op_str = match &arm.op {
                    SelectOp::Recv { type_: _, name, channel } => {
                        let ch = format_expr_canonical(&channel.node);
                        format!("case {} = {ch}.recv()", name.node)
                    }
                    SelectOp::Send { channel, value } => {
                        let ch = format_expr_canonical(&channel.node);
                        let val = format_expr_canonical(&value.node);
                        format!("case {ch}.send({val})")
                    }
                };
                let body = format_block_canonical(&arm.body.stmts);
                s.push_str(&format!(" {op_str}:{body}"));
            }
            if let Some(eb) = else_arm {
                let body = format_block_canonical(&eb.stmts);
                s.push_str(&format!(" else:{body}"));
            }
            s
        }
        Stmt::Item(item) => format_item_canonical(item),
        Stmt::MetaIf { .. } | Stmt::MetaFor { .. } | Stmt::MetaMatch { .. }
        | Stmt::MetaWhile { .. } | Stmt::MetaConst { .. } | Stmt::MetaLog { .. } => "meta".to_string(),
        Stmt::NamedScope { name, body } => {
            let body = format_block_canonical(&body.stmts);
            format!("{}:{}", name.node, body)
        }
    }
}

fn format_block_canonical(stmts: &[Spanned<Stmt>]) -> String {
    if stmts.is_empty() {
        return " pass".to_string();
    }
    let mut result = String::new();
    for s in stmts {
        result.push(' ');
        result.push_str(&format_stmt_canonical(&s.node));
        result.push(';');
    }
    result
}

fn format_generic_params_canonical(gp: &Option<Spanned<GenericParams>>) -> String {
    match gp {
        Some(gp) => {
            let params: Vec<String> = gp
                .node
                .params
                .iter()
                .map(|p| match &p.node {
                    GenericParam::Type { name, .. } => name.node.clone(),
                    GenericParam::Lifetime(name) => name.node.clone(),
                    GenericParam::Const { name, .. } => name.node.clone(),
                })
                .collect();
            if params.is_empty() {
                String::new()
            } else {
                format!("[{}]", params.join(", "))
            }
        }
        None => String::new(),
    }
}

fn format_param_canonical(p: &Param) -> String {
    if p.name.node == "self" {
        match p.ownership {
            Ownership::MutableBorrow => "&self".to_string(),
            Ownership::Move => "!self".to_string(),
            _ => "self".to_string(),
        }
    } else {
        format!("{} {}", format_type_canonical(&p.type_.node), p.name.node)
    }
}

fn format_function_canonical(fd: &FunctionDef) -> String {
    let ret = format_type_canonical(&fd.return_type.node);
    let gp = format_generic_params_canonical(&fd.generic_params);
    let params: Vec<String> = fd.params.iter().map(|p| format_param_canonical(&p.node)).collect();
    let mut result = format!("{ret} {}{gp}({})", fd.name.node, params.join(", "));

    match &fd.body {
        FunctionBody::Expression(expr) => {
            result.push_str(&format!(" = {}", format_expr_canonical(&expr.node)));
        }
        FunctionBody::Block(block) => {
            result.push_str(&format!(":{}", format_block_canonical(&block.stmts)));
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            result.push_str(": pass");
        }
    }
    result
}

fn format_struct_canonical(sd: &StructDef) -> String {
    let gp = format_generic_params_canonical(&sd.generic_params);
    let mut result = format!("struct {}{gp}:", sd.name.node);
    for f in &sd.fields {
        let fd = &f.node;
        result.push_str(&format!(
            " {} {};",
            format_type_canonical(&fd.type_.node),
            fd.name.node
        ));
    }
    result
}

fn format_enum_canonical(ed: &EnumDef) -> String {
    let gp = format_generic_params_canonical(&ed.generic_params);
    let mut result = format!("enum {}{gp}:", ed.name.node);
    for v in &ed.variants {
        let var = &v.node;
        result.push_str(&format!(" {}", var.name.node));
        match &var.fields {
            VariantFields::Tuple(fields) if !fields.is_empty() => {
                let parts: Vec<String> =
                    fields.iter().map(|f| format_type_canonical(&f.node)).collect();
                result.push_str(&format!("({})", parts.join(", ")));
            }
            _ => {}
        }
        result.push(';');
    }
    result
}

fn format_trait_canonical(td: &TraitDef) -> String {
    let gp = format_generic_params_canonical(&td.generic_params);
    let mut result = format!("trait {}{gp}", td.name.node);
    if !td.extends.is_empty() {
        let parents: Vec<String> = td.extends.iter().map(|e| e.node.name.node.clone()).collect();
        result.push_str(&format!(" extends {}", parents.join(", ")));
    }
    result.push(':');
    for item in &td.items {
        match &item.node {
            TraitItem::Method(fd) => {
                result.push_str(&format!(" {};", format_function_canonical(fd)));
            }
            TraitItem::AssociatedType(_) => {
                result.push_str(" ?;");
            }
        }
    }
    result
}

fn format_equip_canonical(eq: &EquipBlock) -> String {
    let target = format_type_canonical(&eq.type_.node);
    let mut result = format!("equip {target}");
    if let Some(trait_) = &eq.trait_ {
        let tn = format_type_canonical(&trait_.trait_name.node);
        result.push_str(&format!(" via {tn}"));
    }
    result.push(':');
    for item in &eq.items {
        result.push_str(&format!(" {};", format_function_canonical(&item.node)));
    }
    result
}

fn format_import_canonical(imp: &ImportStmt) -> String {
    match imp {
        ImportStmt::From { path, names, .. } => {
            let module_path = path
                .iter()
                .map(|s| s.node.as_str())
                .collect::<Vec<_>>()
                .join(".");
            let name_list = names
                .iter()
                .map(|s| s.node.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            format!("from {module_path} import {name_list}")
        }
        ImportStmt::Simple { path, .. } => {
            let module_path = path
                .iter()
                .map(|s| s.node.as_str())
                .collect::<Vec<_>>()
                .join(".");
            format!("import {module_path}")
        }
        ImportStmt::Grouped { path, names, .. } => {
            let module_path = path
                .iter()
                .map(|s| s.node.as_str())
                .collect::<Vec<_>>()
                .join(".");
            let name_list = names
                .iter()
                .map(|s| s.node.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            format!("from {module_path} import {name_list}")
        }
    }
}

fn format_item_canonical(item: &Item) -> String {
    match item {
        Item::Function(fd) => format_function_canonical(fd),
        Item::Struct(sd) => format_struct_canonical(sd),
        Item::Enum(ed) => format_enum_canonical(ed),
        Item::Trait(td) => format_trait_canonical(td),
        Item::Equip(eq) => format_equip_canonical(eq),
        Item::Import(imp) => format_import_canonical(imp),
        Item::Directive(d) => {
            if let Some(val) = &d.value {
                format!("directive {} {val}", d.name)
            } else {
                format!("directive {}", d.name)
            }
        }
        Item::TypeAlias(ta) => {
            let gp = format_generic_params_canonical(&ta.generic_params);
            format!(
                "type {}{gp} = {}",
                ta.name.node,
                format_type_canonical(&ta.type_.node)
            )
        }
        Item::Newtype(nt) => {
            format!(
                "newtype {}({})",
                nt.name.node,
                format_type_canonical(&nt.inner_type.node)
            )
        }
        Item::ConstDecl(cd) => {
            format!(
                "const {} {} = {}",
                format_type_canonical(&cd.type_.node),
                cd.name.node,
                format_expr_canonical(&cd.value.node)
            )
        }
        Item::StaticDecl(sd) => {
            format!(
                "static {} {} = {}",
                format_type_canonical(&sd.type_.node),
                sd.name.node,
                format_expr_canonical(&sd.value.node)
            )
        }
        Item::ExternBlock(eb) => {
            let mut result = "extern:".to_string();
            for f in &eb.items {
                result.push_str(&format!(" {};", format_function_canonical(&f.node)));
            }
            result
        }
        Item::Test(td) => {
            let body = format_block_canonical(&td.body.stmts);
            format!("test \"{}\":{body}", td.name.node)
        }
        Item::SuiteSetup(ss) => {
            let body = format_block_canonical(&ss.body.stmts);
            format!("suite_setup:{body}")
        }
        Item::SuiteTeardown(st) => {
            let body = format_block_canonical(&st.body.stmts);
            format!("suite_teardown:{body}")
        }
        Item::MetaConst(mc) => format!("meta {} {} = {}", format_type_canonical(&mc.type_.node), mc.name.node, format_expr_canonical(&mc.value.node)),
        Item::MetaType(mt) => format!("meta type {} = <rhs>", mt.name.node),
        Item::MetaTypeFunc(mtf) => format!("meta type {}(...)", mtf.name.node),
        Item::MetaAssert(_) => "meta assert ...".to_string(),
        Item::MetaIf(_) => "meta if ...".to_string(),
        Item::MetaLog(_) => "meta log ...".to_string(),
        Item::Module { path, items } => {
            let path_str = path.join(".");
            let inner = items.iter().map(|si| format_item_canonical(&si.node)).collect::<Vec<_>>().join("|");
            format!("module({path_str})[{inner}]")
        }
    }
}

fn format_module_canonical(m: &Module) -> String {
    m.items
        .iter()
        .map(|item| format_item_canonical(&item.node))
        .collect::<Vec<_>>()
        .join("\n")
}

// ═══════════════════════════════════════════════════════════════
#[test]
fn async_basic() {
    run_gg("async_basic.gg", "14");
}

#[test]
fn async_spawn() {
    run_gg("async_spawn.gg", "25");
}

#[test]
fn spawn_join_on_drop() {
    run_gg("spawn_join_on_drop.gg", "50\ndone");
}

#[test]
fn spawn_drop_void() {
    run_gg("spawn_drop_void.gg", "ok");
}

#[test]
fn channel_raii() {
    run_gg("channel_raii.gg", "60");
}

#[test]
fn async_channel() {
    run_gg("async_channel.gg", "21");
}

#[test]
fn async_channel_waker() {
    run_gg("async_channel_waker.gg", "10");
}

#[test]
fn async_channel_unbuffered() {
    run_gg("async_channel_unbuffered.gg", "10\n20\n30");
}

#[test]
fn async_select() {
    run_gg("async_select.gg", "36");
}

#[test]
fn async_control_flow() {
    run_gg("async_control_flow.gg", "20\n6\n14\n12");
}

#[test]
fn async_drop() {
    run_gg("async_drop.gg", "drop compute-local\n42\ndone\ndrop main-local");
}

#[test]
fn async_for_loop() {
    run_gg("async_for_loop.gg", "20\n6\n18");
}

#[test]
fn async_match() {
    run_gg("async_match.gg", "30\n60\n15\n16\n12\n9");
}

#[test]
fn async_for_loop_collections() {
    run_gg("async_for_loop_collections.gg", "60\n300\n30\n100");
}

#[test]
fn async_expr_await() {
    run_gg("async_expr_await.gg", "11\n14\n10\n-10\n20");
}

#[test]
fn async_sleep() {
    run_gg("async_sleep.gg", "sleep works\n10");
}

#[test]
fn async_sleep_spawn() {
    run_gg("async_sleep_spawn.gg", "25");
}

#[test]
fn async_sleep_yield() {
    run_gg("async_sleep_yield.gg", "4\n9\n16\n25\n54");
}

#[test]
fn async_blocking_io() {
    run_gg("async_blocking_io.gg", "hello from blocking io");
}

#[test]
fn async_blocking_coroutine() {
    run_gg("async_blocking_coroutine.gg", "coroutine+blocking");
}

#[test]
fn async_mutex_lock() {
    run_gg("async_mutex_lock.gg", "2000");
}

#[test]
fn async_condition_await() {
    run_gg("async_condition_await.gg", "20\nyes");
}

#[test]
fn async_range_await() {
    run_gg("async_range_await.gg", "10");
}

#[test]
fn async_task_expr_await() {
    run_gg("async_task_expr_await.gg", "25");
}

#[test]
fn async_for_else() {
    run_gg("async_for_else.gg", "6\n-1");
}

#[test]
fn async_prefix_await() {
    run_gg("async_prefix_await.gg", "14");
}

// Arena Allocator
// ═══════════════════════════════════════════════════════════════

#[test]
fn arena_basic() {
    run_gg(
        "arena_basic.gg",
        "\
bytes used > 0: true
len: 3
after reset: 99
inner: 20
outer: 10
done",
    );
}

#[test]
fn arena_escape_return() {
    check_gg_fails(
        "arena_escape_return.gg",
        "cannot return arena-scoped value",
    );
}

#[test]
fn arena_escape_assign() {
    check_gg_fails(
        "arena_escape_assign.gg",
        "cannot assign arena-scoped value",
    );
}

#[test]
fn alloc_keyword() {
    run_gg(
        "alloc_keyword.gg",
        "\
len: 2
used > 0: true
done",
    );
}

#[test]
fn tracking_basic() {
    run_gg(
        "tracking_basic.gg",
        "\
bytes > 0: true
current > 0: true
done",
    );
}

#[test]
fn tracking_report() {
    run_gg(
        "tracking_report.gg",
        "\
realloc_count > 0: true
done",
    );
}

#[test]
fn tracking_composable() {
    run_gg(
        "tracking_composable.gg",
        "\
bytes > 0: true
done",
    );
}

#[test]
fn alloc_keyword_escape() {
    check_gg_fails(
        "alloc_keyword_escape.gg",
        "cannot assign arena-scoped value",
    );
}

#[test]
fn pool_basic() {
    run_gg(
        "pool_basic.gg",
        "\
used > 0: true
block_size: 64
total >= 256: true
done",
    );
}

#[test]
fn pool_composable() {
    run_gg(
        "pool_composable.gg",
        "\
alloc= works: true
done",
    );
}

#[test]
fn tlsf_basic() {
    run_gg(
        "tlsf_basic.gg",
        "\
bytes_used > 0: true
pool_size: 65536
after reset: 0
done",
    );
}

#[test]
fn tlsf_composable() {
    run_gg(
        "tlsf_composable.gg",
        "\
alloc= works: true
done",
    );
}

#[test]
fn tlsf_escape() {
    check_gg_fails(
        "tlsf_escape.gg",
        "cannot assign arena-scoped value",
    );
}

#[test]
fn fba_basic() {
    run_gg(
        "fba_basic.gg",
        "\
bytes_used > 0: true
capacity: 4096
after reset: 0
done",
    );
}

#[test]
fn fba_composable() {
    run_gg(
        "fba_composable.gg",
        "\
alloc= works: true
done",
    );
}

#[test]
fn fallback_basic() {
    run_gg(
        "fallback_basic.gg",
        "\
total_count > 0: true
fallback_count > 0: true
done",
    );
}

#[test]
fn fallback_composable() {
    run_gg(
        "fallback_composable.gg",
        "\
alloc= works: true
done",
    );
}

// Allocator escape tests (error fixtures)

#[test]
fn fba_escape() {
    check_gg_fails(
        "fba_escape.gg",
        "cannot assign arena-scoped value",
    );
}

#[test]
fn fba_escape_return() {
    check_gg_fails(
        "fba_escape_return.gg",
        "cannot return arena-scoped value",
    );
}

#[test]
fn fallback_escape() {
    check_gg_fails(
        "fallback_escape.gg",
        "cannot assign arena-scoped value",
    );
}

#[test]
fn pool_escape() {
    check_gg_fails(
        "pool_escape.gg",
        "cannot assign arena-scoped value",
    );
}

#[test]
fn tracking_escape() {
    check_gg_fails(
        "tracking_escape.gg",
        "cannot return arena-scoped value",
    );
}

// Mixed nested allocator tests

#[test]
fn alloc_nested_mixed() {
    run_gg(
        "alloc_nested_mixed.gg",
        "\
inner len: 2
outer len: 2
outer[0]: 10
done",
    );
}

#[test]
fn tracking_wraps_arena() {
    run_gg(
        "tracking_wraps_arena.gg",
        "\
len: 3
alloc_count > 0: true
bytes > 0: true
done",
    );
}

// Dict and multi-collection allocator tests

#[test]
fn arena_dict() {
    run_gg(
        "arena_dict.gg",
        "\
len: 3
alice: 30
bytes_used > 0: true
done",
    );
}

#[test]
fn arena_multi_collection() {
    run_gg(
        "arena_multi_collection.gg",
        "\
nums: 3
names: 2
scores: 2
alice: 100
bytes_used > 0: true
done",
    );
}

// Additional allocator coverage tests

#[test]
fn tracking_full_stats() {
    run_gg(
        "tracking_full_stats.gg",
        "\
peak >= current: true
bytes_freed > 0: true
free_count > 0: true
done",
    );
}

#[test]
fn pool_free_blocks() {
    run_gg(
        "pool_free_blocks.gg",
        "\
initial free: 16
free decreased: true
invariant: true
done",
    );
}

#[test]
fn tlsf_peak_bytes() {
    run_gg(
        "tlsf_peak_bytes.gg",
        "\
peak >= used: true
peak > 0: true
peak after reset: 0
done",
    );
}

#[test]
fn set_arena() {
    run_gg(
        "set_arena.gg",
        "\
len: 3
has 20: true
bytes_used > 0: true
done",
    );
}

#[test]
fn tracking_wraps_pool() {
    run_gg(
        "tracking_wraps_pool.gg",
        "\
allocs > 0: true
bytes > 0: true
pool used > 0: true
done",
    );
}

#[test]
fn arena_reset_reuse() {
    run_gg(
        "arena_reset_reuse.gg",
        "\
cycle 0 len: 10
cycle 0 first: 0
cycle 0 last: 9
cycle 1 len: 10
cycle 1 first: 0
cycle 1 last: 18
cycle 2 len: 10
cycle 2 first: 0
cycle 2 last: 27
done",
    );
}

#[test]
fn arena_checkpoint() {
    run_gg("arena_checkpoint.gg", "true\ntrue\ndone");
}

#[test]
fn pool_overflow() {
    run_gg(
        "pool_overflow.gg",
        "\
len: 50
first: 0
last: 49
tracker allocs > 0: true
done",
    );
}

// Meta (Compile-Time) Tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn meta_basic() {
    run_gg("meta_basic.gg", "1024\n512\n1.0\ntrue\n70\n99\n100");
}

#[test]
fn meta_builtins() {
    // arch_word_bits() returns 64 on all 64-bit targets; feature() and debug() return false when no --feature flags are passed
    run_gg("meta_builtins.gg", "64\ntrue\nfalse\nfalse\nfeature disabled");
}

#[test]
fn meta_conditional_types() {
    run_gg("meta_conditional_types.gg", "1");
}

#[test]
fn meta_type_func() {
    run_gg("meta_type_func.gg", "7\n1000\n42");
}

#[test]
fn meta_sizeof() {
    // sizeof/alignof/typename built-in meta functions (M8)
    // Sizes: int=8, bool=1, str=16, cstr=8, int8=1, int16=2, int32=4, float32=4
    // Alignments: int=8, bool=1, str=8
    // typename: "int", "bool", "Vector[int]", "str"
    run_gg("meta_sizeof.gg", "8\n1\n16\n8\n1\n2\n4\n4\n8\n1\n8\nint\nbool\nVector[int]\nstr");
}

#[test]
fn meta_fn_basic() {
    // M7: user-defined pure functions called in meta initializers
    // square(2)=4, add(3,7)=10, add(square(2), square(3))=4+9=13
    run_gg("meta_fn_basic.gg", "4\n10\n13");
}

#[test]
fn meta_fn_recursive() {
    // M7: recursive meta functions — factorial(10) and fib(10)
    run_gg("meta_fn_recursive.gg", "3628800\n55");
}

#[test]
fn meta_fn_loops() {
    // M7: meta functions with while loops — sum_to(100)=5050, count_digits(123456)=6
    run_gg("meta_fn_loops.gg", "5050\n6");
}

// Concurrency Primitives Tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn shared_basic() {
    run_gg("shared_basic.gg", "42\n10\n5000\n42");
}

#[test]
fn shared_refcount() {
    run_gg("shared_refcount.gg", "1\n3\n3\n100");
}

#[test]
fn shared_weak() {
    run_gg("shared_weak.gg", "42\nok");
}

#[test]
fn shared_struct() {
    run_gg("shared_struct.gg", "3\n10\n5000\n10\n5000");
}

#[test]
fn shared_vector_elem() {
    run_gg("shared_vector_elem.gg", "3\n10\n30\n99\n42\n2");
}

#[test]
fn generic_op_smoke() {
    run_gg("generic_op_smoke.gg", "13\n7\n30\n15");
}

#[test]
fn async_reactor_sleep() {
    run_gg("async_reactor_sleep.gg", "sleep works\ndone");
}

#[test]
fn async_timer_loop() {
    run_gg("async_timer_loop.gg", "3");
}

#[test]
fn mutex_basic() {
    run_gg("mutex_basic.gg", "0\n42");
}

#[test]
fn guard_struct_field() {
    run_gg("guard_struct_field.gg", "10\n20\n42");
}

#[test]
fn guard_rwlock_field() {
    run_gg("guard_rwlock_field.gg", "8080\nserver\n9090");
}

#[test]
fn mutex_async_contention() {
    run_gg("mutex_async_contention.gg", "400");
}

#[test]
fn shared_multi_token() {
    run_gg("shared_multi_token.gg", "21\n11\n12\n22");
}

#[test]
fn shared_await_release() {
    run_gg("shared_await_release.gg", "10");
}

#[test]
fn shared_spawn_mutex() {
    run_gg("shared_spawn_mutex.gg", "2");
}

#[test]
fn shared_spawn_readonly() {
    run_gg("shared_spawn_readonly.gg", "42");
}

#[test]
fn shared_arc_only() {
    run_gg("shared_arc_only.gg", "99\n99\n99");
}

#[test]
fn shared_atomic() {
    run_gg("shared_atomic.gg", "0\n10\n15\n12\n212");
}

#[test]
fn shared_atomic_bool() {
    run_gg("shared_atomic_bool.gg", "false\ntrue\ntrue");
}

#[test]
fn shared_rwlock() {
    run_gg("shared_rwlock.gg", "10\n20\n25\n25\n99");
}

#[test]
fn shared_keyword_local() {
    run_gg("shared_keyword_local.gg", "10\n42\n50");
}

#[test]
fn shared_transparent() {
    run_gg("shared_transparent.gg", "5\n100\n150\n160");
}

#[test]
fn shared_stale_warning() {
    run_gg("shared_stale_warning.gg", "was zero before await\n1");
}

#[test]
fn shared_stale_refreshed() {
    run_gg("shared_stale_refreshed.gg", "refreshed\n1");
}

#[test]
fn shared_float() {
    run_gg("shared_float.gg", "2.500000");
}

#[test]
fn shared_string() {
    run_gg("shared_string.gg", "5\n5");
}

#[test]
fn shared_atomic_error() {
    check_gg_fails("shared_atomic_error.gg", "int or bool");
}

#[test]
fn shared_stale_while() {
    run_gg("shared_stale_while.gg", "stale while fired\n1");
}

#[test]
fn shared_stale_match() {
    run_gg("shared_stale_match.gg", "stale match fired\n1");
}

#[test]
fn shared_multi_spawn() {
    run_gg("shared_multi_spawn.gg", "2\n11\n20");
}

#[test]
fn shared_early_return() {
    run_gg("shared_early_return.gg", "10\n11\n11");
}

#[test]
fn shared_stale_transitive() {
    run_gg("shared_stale_transitive.gg", "stale transitive fired\n1");
}

#[test]
fn shared_stale_call() {
    run_gg("shared_stale_call.gg", "stale call fired\n1");
}

#[test]
fn shared_stale_tuple() {
    run_gg("shared_stale_tuple.gg", "stale tuple fired\n1");
}

#[test]
fn shared_closure_capture_error() {
    check_gg_fails("shared_closure_capture_error.gg", "cannot capture shared variable");
}

#[test]
fn shared_closure_inline_error() {
    check_gg_fails("shared_closure_inline_error.gg", "cannot capture shared variable");
}

#[test]
fn async_task_group() {
    run_gg("async_task_group.gg", "2");
}

#[test]
fn async_task_group_fire() {
    run_gg("async_task_group_fire.gg", "42\ndone");
}

#[test]
fn concurrency_params() {
    // Verifies that Mutex[T], Guard[T], Channel[T] work as function parameters —
    // the map_ast_type_mut pre-registration fix ensures these types resolve to
    // correct TypeIds before function bodies are lowered, not UNIT_TYPE.
    run_gg("concurrency_params.gg", "10\n11\n99");
}

#[test]
fn sync_atomics() {
    run_gg("sync_atomics.gg", "0\n42\n42\n50\n50\n40\ntrue\n100\nfalse\ntrue\ntrue\nfalse\ntrue\ntrue");
}

#[test]
fn thread_basic() {
    run_gg("thread_basic.gg", "42\nhello from thread\ntrue");
}

#[test]
fn sync_barrier() {
    run_gg("sync_barrier.gg", "barrier passed\nbarrier passed again");
}

#[test]
fn sync_rwlock() {
    run_gg("sync_rwlock.gg", "42\n100");
}

#[test]
fn thread_atomic() {
    run_gg("thread_atomic.gg", "2");
}

#[test]
fn thread_mutex() {
    run_gg("thread_mutex.gg", "2");
}

#[test]
fn thread_barrier() {
    run_gg("thread_barrier.gg", "2");
}

#[test]
fn sync_condvar() {
    run_gg("sync_condvar.gg", "true");
}

#[test]
fn fmt_basic() {
    run_gg("fmt_basic.gg", "    42\nhello\nhi...\ntoolong\n--ab--\n**abc**\nhahaha\n\nhello...\nshort\nhi\none, two, three\none-two-three");
}

#[test]
fn fmt_edges() {
    run_gg(
        "fmt_edges.gg",
        "\
...

x
ab
..
0
0
only
done",
    );
}

#[test]
fn fmt_edge() {
    // "y   " has 3 trailing spaces from pad_right; using \n escapes to preserve them
    run_gg(
        "fmt_edge.gg",
        "   x\ny   \n4\n-a--\nabcdef\ntrue\n..\nabc\ntrue\n\nonly\nabc\nAAAx\ndone",
    );
}

#[test]
fn process_spawn() {
    // echo appends \n, print(out) adds another \n → blank line before exit code
    run_gg("process_spawn.gg", "hello world\n\n0\ntrue");
}

#[test]
fn process_pipe() {
    // cat echoes stdin back; write_stdin adds \n, print(out) adds another \n
    run_gg("process_pipe.gg", "hello from gorget\n\n0");
}

// ═══════════════════════════════════════════════════════════════
// gg.tensor integration tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn tensor_basic() {
    run_gg(
        "tensor_basic.gg",
        "\
Tensor(shape=[6], [0, 1, 2, 3, 4, 5])
Tensor(shape=[2,3], [0, 1, 2, 3, 4, 5])
2
Tensor(shape=[3], [0, 0, 0])
Tensor(shape=[3], [1, 1, 1])
6
2
5
1
99",
    );
}

#[test]
fn tensor_arithmetic() {
    run_gg(
        "tensor_arithmetic.gg",
        "\
Tensor(shape=[4], [1, 3, 5, 7])
Tensor(shape=[4], [1, 1, 1, 1])
Tensor(shape=[4], [0, 2, 6, 12])
Tensor(shape=[4], [0, -1, -2, -3])
Tensor(shape=[4], [10, 11, 12, 13])
Tensor(shape=[4], [3, 6, 9, 12])
Tensor(shape=[4], [3, 5, 7, 9])
Tensor(shape=[4], [2, 6, 12, 20])
Tensor(shape=[4], [11, 12, 13, 14])
Tensor(shape=[4], [2, 4, 6, 8])",
    );
}

#[test]
fn tensor_broadcast() {
    run_gg(
        "tensor_broadcast.gg",
        "\
Tensor(shape=[4], [1, 2, 3, 4])
Tensor(shape=[4], [10, 12, 14, 16])
3
4
Tensor(shape=[4], [10, 10, 10, 10])
Tensor(shape=[4], [0, 11, 24, 39])
Tensor(shape=[4], [2, 3, 4, 5])
Tensor(shape=[4], [0, 1, 2, 3])",
    );
}

#[test]
fn tensor_reshape() {
    run_gg(
        "tensor_reshape.gg",
        "\
Tensor(shape=[2,3], [0, 1, 2, 3, 4, 5])
5
2
3
2
3
2
1
3
42
Tensor(shape=[2,3], [0, 1, 2, 3, 4, 5])",
    );
}

#[test]
fn tensor_reduce() {
    run_gg(
        "tensor_reduce.gg",
        "\
10
0
4
15.000000
1.000000
5.000000
3.000000
Tensor(shape=[3], [3, 5, 7])
Tensor(shape=[2], [3, 12])
Tensor(shape=[2], [3, 12])",
    );
}

#[test]
fn tensor_linalg() {
    run_gg(
        "tensor_linalg.gg",
        "\
8
Tensor(shape=[2,2], [10, 13, 28, 40])
14.000000
true
true
true
true",
    );
}

#[test]
fn tensor_extra() {
    run_gg(
        "tensor_extra.gg",
        "\
6
15.000000
2.000000
-15.000000
28
9.000000
1
3
2
1.414214
3
4
true
true
true
true
true
true
true
true
true
true
4.000000
1.000000",
    );
}

#[test]
fn tensor_float_frac() {
    run_gg(
        "tensor_float_frac.gg",
        "\
5.000000
0.500000
1.500000",
    );
}

// ═══════════════════════════════════════════════════════════════
// gg.dataframe integration tests
// ═══════════════════════════════════════════════════════════════

#[test]
fn dataframe_basic() {
    run_gg(
        "dataframe_basic.gg",
        "\
3
3
true
false
25
35
int
Bob
2
1
Charlie
1
2",
    );
}

#[test]
fn dataframe_filter() {
    run_gg(
        "dataframe_filter.gg",
        "\
2
4
2
2
Bob
Charlie
3
Bob
1
25
3
2",
    );
}

#[test]
fn dataframe_agg() {
    run_gg(
        "dataframe_agg.gg",
        "\
100
25
10
40
4
10
2.5
2
11.1803
6
3
2",
    );
}

#[test]
fn dataframe_transform() {
    run_gg(
        "dataframe_transform.gg",
        "\
20
25
30
30
3
60
3",
    );
}

#[test]
fn closure_float_ret() {
    run_gg(
        "closure_float_ret.gg",
        "\
2.5
5
10",
    );
}

#[test]
fn dataframe_groupby() {
    run_gg(
        "dataframe_groupby.gg",
        "\
2
10
5
3
3
30
10
50",
    );
}

#[test]
fn dataframe_csv() {
    run_gg(
        "dataframe_csv.gg",
        "\
3
3
int
25
float
9
Charlie
3
3",
    );
}

#[test]
fn dataframe_nulls() {
    run_gg(
        "dataframe_nulls.gg",
        "\
bool
true
true
1
true
false
0
2
3
x
3
1
hello
true
1.5
9.9
hello
default",
    );
}

#[test]
fn toml_stringify() {
    run_gg(
        "toml_stringify.gg",
        "\
Alice
42
true
localhost
8080
2
first
second
\"hello\"
99
true
false
true
3
true
false
true
2024-01-15T09:30:00Z
done",
    );
}

#[test]
fn toml_edge() {
    run_gg(
        "toml_edge.gg",
        "\
hello
world
C:\\Users\\me
line1
line2
1
2
-42
-3.500000
true
true
true
0
true
false
3
3
done",
    );
}

#[test]
fn json_edge_cases() {
    run_gg(
        "json_edge_cases.gg",
        "\
true
true
true
true
false
false
true
false
true
false
[
  1,
  2,
  3
]
0
0
-42
11
error caught
error caught
[1,2,3]
done",
    );
}

#[test]
fn dataframe_ops() {
    run_gg(
        "dataframe_ops.gg",
        "\
3
2
true
false
2
name
age
Alice
30
true
2
2
10
20
3
2
true
true
done",
    );
}

#[test]
fn csv_delimiters() {
    run_gg(
        "csv_delimiters.gg",
        "\
2
3
Alice
LA
true
true
true
2
10
40
2
1
alpha
0
2
done",
    );
}

#[test]
fn dataframe_filter_sort() {
    run_gg(
        "dataframe_filter_sort.gg",
        "\
Alice
Eve
Alice
Eve
3
3
1
Alice
2
Alice
1
Eve
2
3
2.3
9.0
10.5
done",
    );
}

#[test]
fn csv_stringify() {
    run_gg(
        "csv_stringify.gg",
        "\
3
name
age
Alice
30
2
x
y
2
2
Paris
Tokyo
1
2
done",
    );
}

#[test]
fn csv_edge() {
    run_gg(
        "csv_edge.gg",
        "true\ntrue\nfalse\n2\n-1\n2\nhello, world\nplain\nsay \"hi\"\nend\n0\ndone",
    );
}

#[test]
fn json_pretty() {
    run_gg(
        "json_pretty.gg",
        "\
{
  \"name\": \"Alice\"
}
[
  1,
  2,
  3
]
done",
    );
}

#[test]
fn xml_query() {
    run_gg(
        "xml_query.gg",
        "\
item
1
a
0
2
1
2
0
<item id=\"1\">a</item>
done",
    );
}

#[test]
fn xml_roundtrip() {
    run_gg(
        "xml_roundtrip.gg",
        "\
a < b & c
a & b
true
42
true
3 > 2
he said \"hi\"
done",
    );
}

#[test]
fn http_urls() {
    run_gg(
        "http_urls.gg",
        "\
example.com
443
/api/v1
true
localhost
8080
/health
false
host.example.com
80
/
false
api.service
3000
/
false
done",
    );
}

#[test]
fn yaml_multi() {
    run_gg(
        "yaml_multi.gg",
        "\
2
Alice
Bob
true
true
done",
    );
}

#[test]
fn uuid_props() {
    run_gg(
        "uuid_props.gg",
        "\
4
true
false
done",
    );
}

#[test]
fn toml_datetime() {
    run_gg(
        "toml_datetime.gg",
        "\
true
false
true
false
done",
    );
}

#[test]
fn datetime_gaps() {
    run_gg(
        "datetime_gaps.gg",
        "\
1970-01-01T00:00:00Z
2000-01-01T00:00:00Z
2000-01-01T00:00:00Z
1
61
366
2000-01-01T01:30:00Z
1999-12-31T23:30:00Z
done",
    );
}

#[test]
fn ecs_advanced() {
    run_gg(
        "ecs_advanced.gg",
        "\
3
100
none
100
50
done",
    );
}

#[test]
fn dataframe_extra() {
    run_gg(
        "dataframe_extra.gg",
        "\
2
2
Alice
90
2
true
true
done",
    );
}

#[test]
fn dataframe_tier2_basic() {
    run_gg(
        "dataframe_tier2_basic.gg",
        "\
true
false
3
3
str
int
float
false
true
false
false
3
3
false
true
27.5
8.75
17.1875
done",
    );
}

#[test]
fn dataframe_tier2_sort_arith() {
    run_gg(
        "dataframe_tier2_sort_arith.gg",
        "\
4
Alice
Charlie
2
35
2
25
105
115
35
65
2450
2
4
done",
    );
}

#[test]
fn dataframe_tier2_joins() {
    run_gg(
        "dataframe_tier2_joins.gg",
        "\
2
int
3
100
true
100
2
4
6
4
done",
    );
}

#[test]
fn dataframe_tier2_groupby() {
    run_gg(
        "dataframe_tier2_groupby.gg",
        "\
4
3
1
2
int
float
str
bool
done",
    );
}

#[test]
fn return_in_if_in_match() {
    run_gg(
        "return_in_if_in_match.gg",
        "\
big circle
small circle
big rect
small rect
big triangle
tall triangle
small triangle
10
false
0
A
B
C
D
F
zero
done",
    );
}

#[test]
fn dataframe_cast() {
    run_gg(
        "dataframe_cast.gg",
        "\
true
float
1
3
true
3
-2
0
true
str
1
true
42
null
7
true
1
0
true
true
true
false
false
true
10
null
20
done",
    );
}

#[test]
fn dataframe_clip() {
    run_gg(
        "dataframe_clip.gg",
        "\
0
0
50
100
0
5.5
10
hello
world
4
0
null
100
0
100
50
done",
    );
}

// Parser Comparison Test
// ═══════════════════════════════════════════════════════════════

#[test]
fn parser_comparison() {
    use gorget::parser::Parser;

    // 1. Build the Gorget parser driver
    let (driver_exe, driver_c) = build_gg_dir("self_host_parser", "driver.gg");

    // 2. Discover all top-level .gg fixture files
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = manifest_dir.join("tests/fixtures");
    let mut fixtures: Vec<PathBuf> = std::fs::read_dir(&fixtures_dir)
        .expect("failed to read fixtures dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_file() && p.extension().map_or(false, |ext| ext == "gg"))
        .collect();
    fixtures.sort();

    assert!(
        !fixtures.is_empty(),
        "No .gg fixtures found in {}",
        fixtures_dir.display()
    );

    struct Mismatch {
        fixture: String,
        first_diff_line: usize,
        rust_line: String,
        gorget_line: String,
        rust_total: usize,
        gorget_total: usize,
    }

    let mut matched = 0;
    let mut mismatches: Vec<Mismatch> = Vec::new();
    let mut crashes: Vec<(String, String)> = Vec::new();
    let mut compared = 0;

    // 3. For each fixture, compare Rust vs Gorget parser output
    for fixture in &fixtures {
        let source = match std::fs::read_to_string(fixture) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "  SKIP {}: read error: {e}",
                    fixture.file_name().unwrap().to_string_lossy()
                );
                continue;
            }
        };

        // Rust side: parse and format canonically
        let mut parser = Parser::new(&source);
        let module = parser.parse_module();
        let rust_output = format_module_canonical(&module);

        // Gorget side: run the driver binary
        let output = Command::new(&driver_exe).arg(fixture).output();

        match output {
            Ok(out) if out.status.success() => {
                let gorget_output = String::from_utf8_lossy(&out.stdout)
                    .trim_end()
                    .to_string();

                let rust_lines: Vec<&str> = rust_output.lines().collect();
                let gorget_lines: Vec<&str> = gorget_output.lines().collect();

                // Find first line divergence
                let mut first_diff = None;
                let max_lines = rust_lines.len().max(gorget_lines.len());
                for i in 0..max_lines {
                    let r = rust_lines.get(i).unwrap_or(&"<missing>");
                    let g = gorget_lines.get(i).unwrap_or(&"<missing>");
                    if r != g {
                        first_diff = Some(i);
                        break;
                    }
                }

                if let Some(diff_line) = first_diff {
                    mismatches.push(Mismatch {
                        fixture: fixture
                            .file_name()
                            .unwrap()
                            .to_string_lossy()
                            .to_string(),
                        first_diff_line: diff_line,
                        rust_line: rust_lines
                            .get(diff_line)
                            .unwrap_or(&"<missing>")
                            .to_string(),
                        gorget_line: gorget_lines
                            .get(diff_line)
                            .unwrap_or(&"<missing>")
                            .to_string(),
                        rust_total: rust_lines.len(),
                        gorget_total: gorget_lines.len(),
                    });
                } else {
                    matched += 1;
                }
            }
            Ok(out) => {
                let name = fixture
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                crashes.push((name, stderr));
            }
            Err(e) => {
                let name = fixture
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                crashes.push((name, format!("exec error: {e}")));
            }
        }
        compared += 1;
    }

    // 4. Cleanup
    let _ = std::fs::remove_file(&driver_c);
    let _ = std::fs::remove_file(&driver_exe);

    // 5. Report
    eprintln!("\n=== Parser Comparison Results ===");
    eprintln!(
        "Fixtures compared: {compared}, matched: {matched}, mismatched: {}, crashed: {}",
        mismatches.len(),
        crashes.len()
    );

    if !crashes.is_empty() {
        eprintln!("\n--- Crashes ({}) ---", crashes.len());
        for (name, err) in &crashes {
            let first_line = err.lines().next().unwrap_or("(no stderr)");
            eprintln!("  {name}: {first_line}");
        }
    }

    if !mismatches.is_empty() {
        eprintln!("\n--- Mismatches ({}) ---", mismatches.len());
        for m in mismatches.iter().take(200) {
            eprintln!(
                "\n  {} (line {}, rust={} gorget={} lines)",
                m.fixture, m.first_diff_line, m.rust_total, m.gorget_total
            );
            eprintln!("    Rust:   {}", m.rust_line);
            eprintln!("    Gorget: {}", m.gorget_line);
        }
        if mismatches.len() > 30 {
            eprintln!("\n  ... and {} more", mismatches.len() - 30);
        }
    }

    // Diagnostic test — always passes. Mismatches guide development.
    eprintln!("\n================================\n");
}

// ═══════════════════════════════════════════════════════════════
// Resolver Canonical Formatter (Rust side)
// ═══════════════════════════════════════════════════════════════

fn format_def_kind_canonical(kind: &gorget::semantic::scope::DefKind) -> &'static str {
    use gorget::semantic::scope::DefKind::*;
    match kind {
        Function => "Function",
        Struct => "Struct",
        Enum => "Enum",
        Variant => "Variant",
        Trait => "Trait",
        TypeAlias => "TypeAlias",
        Newtype => "Newtype",
        Variable => "Variable",
        Const => "Const",
        Static => "Static",
        GenericParam => "GenericParam",
        Import => "Import",
    }
}

fn format_scope_kind_canonical(kind: &gorget::semantic::scope::ScopeKind) -> &'static str {
    use gorget::semantic::scope::ScopeKind::*;
    match kind {
        Module => "Module",
        FileModule { .. } => "FileModule",
        Function => "Function",
        Block => "Block",
        EquipBlock { .. } => "EquipBlock",
        TraitDef => "TraitDef",
        ForLoop => "ForLoop",
    }
}

fn format_resolution_canonical(
    scopes: &gorget::semantic::scope::ScopeTable,
    resolution_map: &gorget::semantic::resolve::ResolutionMap,
) -> String {
    use gorget::semantic::ids::{DefId, ScopeId};

    let mut lines = Vec::new();

    // DEF lines — sorted by DefId (natural order)
    for i in 0..scopes.def_count() {
        let def = scopes.get_def(DefId(i as u32));
        let kind = format_def_kind_canonical(&def.kind);
        lines.push(format!(
            "DEF {} {} \"{}\" {}:{}",
            i, kind, def.name, def.span.start, def.span.end
        ));
    }

    // SCOPE lines — sorted by ScopeId (natural order)
    for i in 0..scopes.scope_count() {
        let sid = ScopeId(i as u32);
        let kind = format_scope_kind_canonical(scopes.scope_kind(sid));
        let parent = match scopes.scope_parent(sid) {
            Some(p) => p.0 as i32,
            None => -1,
        };
        lines.push(format!("SCOPE {} {} parent:{}", i, kind, parent));
    }

    // RES lines — sorted by span_start
    let mut res_entries: Vec<(usize, u32)> = resolution_map
        .iter()
        .map(|(&span, &def_id)| (span, def_id.0))
        .collect();
    res_entries.sort_by_key(|&(span, _)| span);
    for (span_start, def_id) in res_entries {
        lines.push(format!("RES {} -> {}", span_start, def_id));
    }

    lines.join("\n")
}

/// Normalize resolver canonical output for comparison.
///
/// Differences between Rust and Gorget AST representations mean certain
/// lines can't be compared verbatim:
/// - DEF spans: Gorget AST doesn't store name spans → strip `start:end` from DEF lines
/// - SCOPE lines: Rust `Expr::Block` creates extra scopes absent in Gorget AST → skip SCOPE lines
/// - RES lines: compared exactly (this is the core correctness check)
///
/// Returns (def_lines, res_lines) — SCOPE lines are excluded.
fn normalize_resolver_output(output: &str) -> (Vec<String>, Vec<String>) {
    let mut defs = Vec::new();
    let mut res = Vec::new();
    for line in output.lines() {
        if line.starts_with("DEF ") {
            // Strip the trailing ` start:end` span from DEF lines.
            if let Some(last_quote) = line.rfind('"') {
                defs.push(line[..=last_quote].to_string());
            } else {
                defs.push(line.to_string());
            }
        } else if line.starts_with("RES ") {
            res.push(line.to_string());
        }
        // SCOPE lines are skipped — structural differences between ASTs
    }
    (defs, res)
}

// ═══════════════════════════════════════════════════════════════
// Resolver Comparison Test
// ═══════════════════════════════════════════════════════════════

#[test]
fn resolver_comparison() {
    use gorget::parser::Parser;
    use gorget::semantic::resolve;
    use gorget::semantic::scope::ScopeTable;
    use gorget::semantic::types::TypeTable;

    // 1. Build the Gorget resolver driver
    let (driver_exe, driver_c) = build_gg_dir("self_host_resolver", "driver.gg");

    // 2. Discover all top-level .gg fixture files
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = manifest_dir.join("tests/fixtures");
    let mut fixtures: Vec<PathBuf> = std::fs::read_dir(&fixtures_dir)
        .expect("failed to read fixtures dir")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_file() && p.extension().map_or(false, |ext| ext == "gg"))
        .collect();
    fixtures.sort();

    assert!(
        !fixtures.is_empty(),
        "No .gg fixtures found in {}",
        fixtures_dir.display()
    );

    struct Mismatch {
        fixture: String,
        first_diff_line: usize,
        rust_line: String,
        gorget_line: String,
        rust_total: usize,
        gorget_total: usize,
    }

    let mut matched = 0;
    let mut mismatches: Vec<Mismatch> = Vec::new();
    let mut crashes: Vec<(String, String)> = Vec::new();
    let mut compared = 0;

    // 3. For each fixture, compare Rust vs Gorget resolver output
    for fixture in &fixtures {
        let source = match std::fs::read_to_string(fixture) {
            Ok(s) => s,
            Err(e) => {
                eprintln!(
                    "  SKIP {}: read error: {e}",
                    fixture.file_name().unwrap().to_string_lossy()
                );
                continue;
            }
        };

        // Rust side: parse, resolve, format canonically
        let mut parser = Parser::new(&source);
        let module = parser.parse_module();
        let mut scopes = ScopeTable::new();
        let mut types = TypeTable::new();
        let mut errors = Vec::new();
        let mut resolve_ctx =
            resolve::collect_top_level(&module, &mut scopes, &mut types, &mut errors);
        let mut resolution_map = resolve::resolve_bodies(
            &module,
            &mut scopes,
            &mut types,
            &mut errors,
            &mut resolve_ctx.function_info,
            &mut resolve_ctx.function_body_scopes,
            &resolve_ctx.file_module_scopes,
        );
        resolution_map.extend(resolve_ctx.resolution_map);
        let rust_output = format_resolution_canonical(&scopes, &resolution_map);
        let fname = fixture.file_name().unwrap().to_string_lossy().to_string();

        // Gorget side: run the driver binary
        let output = Command::new(&driver_exe).arg(fixture).output();

        match output {
            Ok(out) if out.status.success() => {
                let gorget_output = String::from_utf8_lossy(&out.stdout)
                    .trim_end()
                    .to_string();

                // Normalize: extract DEF + RES lines (skip SCOPE — structural AST diffs)
                let (rust_defs, rust_res) = normalize_resolver_output(&rust_output);
                let (gorget_defs, gorget_res) = normalize_resolver_output(&gorget_output);

                // Combine DEF + RES for comparison
                let mut rust_lines = rust_defs;
                rust_lines.extend(rust_res);
                let mut gorget_lines = gorget_defs;
                gorget_lines.extend(gorget_res);

                // Find first line divergence
                let mut first_diff = None;
                let max_lines = rust_lines.len().max(gorget_lines.len());
                for i in 0..max_lines {
                    let r = rust_lines.get(i).map(|s| s.as_str()).unwrap_or("<missing>");
                    let g = gorget_lines.get(i).map(|s| s.as_str()).unwrap_or("<missing>");
                    if r != g {
                        first_diff = Some(i);
                        break;
                    }
                }

                if let Some(diff_line) = first_diff {
                    mismatches.push(Mismatch {
                        fixture: fname.clone(),
                        first_diff_line: diff_line,
                        rust_line: rust_lines
                            .get(diff_line)
                            .cloned()
                            .unwrap_or_else(|| "<missing>".to_string()),
                        gorget_line: gorget_lines
                            .get(diff_line)
                            .cloned()
                            .unwrap_or_else(|| "<missing>".to_string()),
                        rust_total: rust_lines.len(),
                        gorget_total: gorget_lines.len(),
                    });
                } else {
                    matched += 1;
                }
            }
            Ok(out) => {
                let name = fixture
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                let stderr = String::from_utf8_lossy(&out.stderr).to_string();
                crashes.push((name, stderr));
            }
            Err(e) => {
                let name = fixture
                    .file_name()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                crashes.push((name, format!("exec error: {e}")));
            }
        }
        compared += 1;
    }

    // 4. Cleanup
    let _ = std::fs::remove_file(&driver_c);
    let _ = std::fs::remove_file(&driver_exe);

    // 5. Report
    eprintln!("\n=== Resolver Comparison Results ===");
    eprintln!(
        "Fixtures compared: {compared}, matched: {matched}, mismatched: {}, crashed: {}",
        mismatches.len(),
        crashes.len()
    );

    if !crashes.is_empty() {
        eprintln!("\n--- Crashes ({}) ---", crashes.len());
        for (name, err) in &crashes {
            let first_line = err.lines().next().unwrap_or("(no stderr)");
            eprintln!("  {name}: {first_line}");
        }
    }

    if !mismatches.is_empty() {
        eprintln!("\n--- Mismatches ({}) ---", mismatches.len());
        for m in mismatches.iter().take(200) {
            eprintln!(
                "\n  {} (line {}, rust={} gorget={} lines)",
                m.fixture, m.first_diff_line, m.rust_total, m.gorget_total
            );
            eprintln!("    Rust:   {}", m.rust_line);
            eprintln!("    Gorget: {}", m.gorget_line);
        }
        if mismatches.len() > 30 {
            eprintln!("\n  ... and {} more", mismatches.len() - 30);
        }
    }

    // Diagnostic test — always passes. Mismatches guide development.
    eprintln!("\n================================\n");
}

// Numeric trait integration tests

#[test]
fn numeric_trait() {
    run_gg(
        "numeric_trait.gg",
        "\
7
4.000000
0
1
1.000000",
    );
}

#[test]
fn numeric_trait_ops() {
    run_gg(
        "numeric_trait_ops.gg",
        "\
7
3.500000
20
7.000000
-42
-1.500000
true
false",
    );
}

#[test]
fn mod_rem() {
    run_gg(
        "mod_rem.gg",
        "\
1
-1
1
-1
2
-2
-1
1
2
2.000000
1
0
0",
    );
}

#[test]
fn meta_delayed_basic() {
    run_gg(
        "meta_delayed_basic.gg",
        "\
integer
float
string
other
14
2.500000",
    );
}

#[test]
fn meta_delayed_for() {
    run_gg(
        "meta_delayed_for.gg",
        "\
first
second
third
first
second
third
int
float
unknown",
    );
}

#[test]
fn meta_delayed_nested() {
    run_gg(
        "meta_delayed_nested.gg",
        "\
int64
float64
other
is int
8 bytes
is str
16 bytes
other type",
    );
}

#[test]
fn meta_delayed_match() {
    run_gg(
        "meta_delayed_match.gg",
        "\
integer
float
string
other
0
0
-1",
    );
}

#[test]
fn meta_numeric_meta() {
    run_gg(
        "meta_numeric_meta.gg",
        "\
8
8
16
16
32
64
signed
unsigned
signed
unsigned
int8-max
uint8-max
int16-max
int8
uint8
int16
other
done",
    );
}

#[test]
fn meta_implements() {
    run_gg(
        "meta_implements.gg",
        "\
numeric
numeric
numeric
numeric
not-numeric
not-numeric
comparable
comparable
printable
not-printable
done",
    );
}

#[test]
fn meta_while() {
    run_gg(
        "meta_while.gg",
        "\
normal
normal
done
done
8-bit
16-bit
32-bit
64-bit
done",
    );
}

#[test]
fn meta_fields() {
    run_gg(
        "meta_fields.gg",
        "\
x:float
y:float
name:str
health:int
alive:bool
2
1
done",
    );
}

#[test]
fn meta_type_is() {
    run_gg(
        "meta_type_is.gg",
        "\
float
float
signed
signed
unsigned
unsigned
bool
other
numeric
numeric
not-numeric
not-signed
signed
float32-exact
float-category
handles-signed-math
handles-signed-math
other
done",
    );
}

#[test]
fn meta_enum_ordinal() {
    run_gg(
        "meta_enum_ordinal.gg",
        "\
North=0
East=1
South=2
West=3
Red=0
Green=1
Blue=2
Red
Green
Blue
done",
    );
}

#[test]
fn meta_reflection() {
    run_gg(
        "meta_reflection.gg",
        "\
2
3
false
true
3-variants
x
y
x
y
z
Red
Green
Blue
x:int
y:float
done",
    );
}

#[test]
fn meta_variant_payloads() {
    run_gg(
        "meta_variant_payloads.gg",
        "\
Circle
Square
Tag
Circle
Square
Tag
done",
    );
}

#[test]
fn field_access() {
    run_gg(
        "field_access.gg",
        "\
10
20
x=3,y=7
name=alice,health=100,alive=true
10
done",
    );
}

#[test]
fn embed_file() {
    run_gg(
        "embed_file.gg",
        "\
SELECT id, name FROM users WHERE active = 1;
hello world
done",
    );
}

#[test]
fn meta_log() {
    run_gg(
        "meta_log.gg",
        "\
integer
string
boolean
done",
    );
}

#[test]
fn trait_default_meta() {
    run_gg(
        "trait_default_meta.gg",
        "\
found Red
found Blue
not found
found South
done",
    );
}

#[test]
fn sqlite_basic() {
    run_gg(
        "sqlite_basic.gg",
        "\
3
alice
30
alice
30
bob
25
1
2
done",
    );
}

#[test]
fn named_scope_basic() {
    run_gg(
        "named_scope_basic.gg",
        "\
15
30
15
10",
    );
}

#[test]
fn async_param_across_await() {
    run_gg(
        "async_param_across_await.gg",
        "\
world
84",
    );
}

#[test]
fn spawn_closure_copy() {
    run_gg(
        "spawn_closure_copy.gg",
        "\
42
420",
    );
}

#[test]
fn spawn_closure_shared() {
    run_gg(
        "spawn_closure_shared.gg",
        "\
5
15",
    );
}

#[test]
fn spawn_closure_inline() {
    run_gg(
        "spawn_closure_inline.gg",
        "\
7
done",
    );
}

#[test]
fn spawn_closure_void() {
    run_gg(
        "spawn_closure_void.gg",
        "\
hello from thread
main done",
    );
}

#[test]
fn spawn_method_basic() {
    run_gg(
        "spawn_method_basic.gg",
        "\
50",
    );
}

#[test]
fn spawn_method_void() {
    run_gg(
        "spawn_method_void.gg",
        "\
hello from thread
done",
    );
}

#[test]
fn spawn_nested_await() {
    // Spawned function internally spawns+awaits another task (Phase 4: cooperative yield).
    run_gg("spawn_nested_await.gg", "11\n21");
}

#[test]
fn spawn_many() {
    // 10,000 spawns bounded by thread pool (no thread exhaustion).
    run_gg("spawn_many.gg", "49995000");
}

#[test]
fn spawn_coroutine_drops() {
    // Coroutines with String/Vector locals — verifies drops emit in poll functions.
    run_gg("spawn_coroutine_drops.gg", "Hello, Alice!\nHello, Bob!\n60");
}

#[test]
fn spawn_coroutine_string() {
    // Coroutine with multiple internal awaits — verifies Move-type Task drops in poll fn.
    run_gg("spawn_coroutine_string.gg", "45");
}

#[test]
fn spawn_multi_await() {
    // Coroutine with multiple awaits per basic block — verifies multi-state machine.
    run_gg("spawn_multi_await.gg", "40");
}

#[test]
fn spawn_coroutine_str_args() {
    // String literal args in coroutine Call context — verifies gorget_str_from_literal wrapping.
    run_gg("spawn_coroutine_str_args.gg", "hello world\n11");
}

#[test]
fn spawn_vector_await() {
    // Spawn tasks into a vector and await them by index — type-based await dispatch.
    run_gg("spawn_vector_await.gg", "30");
}

#[test]
fn method_mut_borrow_arg() {
    // MutableBorrow non-self param in equip method — callee can mutate the original.
    run_gg("method_mut_borrow_arg.gg", "60\n6");
}

// ── Concurrency stress tests ────────────────────────────────────────────

#[test]
fn stress_spawn_fan_out() {
    // 200 tasks in parallel via TaskGroup + atomic counter.
    run_gg("stress_spawn_fan_out.gg", "19900");
}

#[test]
fn stress_mutex_hammer() {
    // 8 tasks x 1000 increments on shared(mutex) counter.
    run_gg("stress_mutex_hammer.gg", "8000");
}

#[test]
fn stress_atomic_hammer() {
    // 8 tasks x 1000 atomic increments.
    run_gg("stress_atomic_hammer.gg", "8000");
}

#[test]
fn stress_channel_mpsc() {
    // 4 producers x 500 values into bounded channel, single consumer sums.
    run_gg("stress_channel_mpsc.gg", "501000");
}

#[test]
fn stress_shared_multi_token() {
    // 3 shared vars, 6 tasks touching pairs — deadlock freedom.
    run_gg("stress_shared_multi_token.gg", "2000\n2000\n2000");
}

#[test]
fn stress_taskgroup_fan() {
    // TaskGroup with 100 tasks, each atomic-incrementing a counter.
    run_gg("stress_taskgroup_fan.gg", "100");
}

#[test]
fn stress_channel_select() {
    // 4 channels, 4 producers, consumer select-drains all.
    run_gg("stress_channel_select.gg", "20200");
}

#[test]
fn stress_rwlock_writers() {
    // 4 writer tasks x 500 increments on shared(rwlock).
    run_gg("stress_rwlock_writers.gg", "2000");
}

#[test]
fn stress_nested_spawn() {
    // 10 tasks each spawn 10 sub-tasks = 100 leaves, sum 0..99.
    run_gg("stress_nested_spawn.gg", "4950");
}

#[test]
fn stress_pipeline() {
    // 3-stage pipeline: produce -> double -> consume via 2 channels.
    run_gg("stress_pipeline.gg", "250500");
}

#[test]
fn stress_nested_return() {
    // Nested spawn with return values — 5 batches x 5 tasks, each returns x*2.
    // sum(0..24) * 2 = 2*(24*25/2) = 600
    run_gg("stress_nested_return.gg", "600");
}

#[test]
fn scheduler_thread() {
    // 1:1 OS thread per spawn: double(10)=20 + double(21)=42 = 62
    run_gg("scheduler_thread.gg", "62");
}

#[test]
fn scheduler_inline() {
    // Synchronous inline: triple(5)=15 + triple(10)=30 = 45
    run_gg("scheduler_inline.gg", "45");
}

#[test]
fn scheduler_single() {
    // Cooperative single-threaded: add(10,20)=30 + add(30,40)=70 = 100
    run_gg("scheduler_single.gg", "100");
}
