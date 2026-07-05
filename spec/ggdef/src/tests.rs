//! Unit tests for the evaluator — at minimum one per §2.2 bullet (the Gates-A
//! requirement), plus the four-outcome coverage (Value/Trap/IllFormed/
//! FuelExhausted) and the desugarings the elaborator performs.

use crate::trace::TraceEvent;
use crate::{run_source, Fault, Outcome, Run};

const FUEL: u64 = 10_000_000;

/// Run a program, asserting it produced a Value outcome, and return trimmed
/// stdout.
fn out(src: &str) -> String {
    let run = go(src);
    match run.outcome {
        Outcome::Value(_) => run.stdout.trim_end().to_string(),
        other => panic!("expected Value, got {other:?}\nstdout so far:\n{}", run.stdout),
    }
}

fn go(src: &str) -> Run {
    run_source(src, FUEL).unwrap_or_else(|e| panic!("frontend error: {e}"))
}

fn kinds(run: &Run) -> Vec<&'static str> {
    run.trace.iter().map(TraceEvent::kind).collect()
}

// ── §2.2 bullet 1: implicit-copy at bare-assign; live-place copy ───────────

#[test]
fn bare_assign_copy_is_independent() {
    // `S b = a` copies as-of the bind; mutating `a` leaves `b` at its old value.
    let src = r#"
struct S:
    String text
void main():
    S a = S("hello")
    S b = a
    a.text = "world"
    print(a.text)
    print(b.text)
"#;
    assert_eq!(out(src), "world\nhello");
}

#[test]
fn fresh_temp_bind_is_a_move_not_a_copy() {
    // A constructor result has no continuing owner ⇒ tagged Move, so binding it
    // emits NO BindCopy (it is not a live-place copy).
    let src = r#"
struct S:
    String text
void main():
    S a = S("hi")
    print(a.text)
"#;
    let run = go(src);
    assert!(!kinds(&run).contains(&"bind_copy"), "fresh-temp bind must not BindCopy");
}

#[test]
fn place_bind_emits_bind_copy() {
    let src = r#"
struct S:
    String text
void main():
    S a = S("hi")
    S b = a
    print(b.text)
"#;
    let run = go(src);
    assert!(kinds(&run).contains(&"bind_copy"), "place bind must BindCopy");
}

// ── §2.2 bullet 2: Borrow view; materialize-on-first-write; owner untouched ─

#[test]
fn materialize_on_write_owner_untouched_and_binding_sees_copy() {
    let src = r#"
struct Bag:
    Vector[int] items
void by_val(Bag s):
    s.items.push(99)
    print(s.items.len())
void main():
    Bag a = Bag(Vector[int]())
    a.items.push(1)
    by_val(a)
    print(a.items.len())
"#;
    let run = go(src);
    // Binding sees the copy (2); the owner is untouched (1).
    assert_eq!(run.stdout.trim_end(), "2\n1");
    assert!(kinds(&run).contains(&"materialize"), "write through a Borrow must materialize");
}

#[test]
fn borrow_view_read_needs_no_copy() {
    let src = r#"
int getlen(Vector[int] v):
    return v.len()
void main():
    Vector[int] a = [1, 2, 3]
    print(getlen(a))
"#;
    assert_eq!(out(src), "3");
}

#[test]
fn self_like_bare_param_field_assign_materializes() {
    // Nested field-path assign through a bare param materialises the root; the
    // caller is untouched (the D2 shape at a plain binding).
    let src = r#"
struct Inner:
    int val
struct Outer:
    Inner inner
void sneak(Outer o):
    o.inner.val = 99
void main():
    Outer x = Outer(Inner(1))
    sneak(x)
    print(x.inner.val)
"#;
    assert_eq!(out(src), "1");
}

// ── §2.2 bullet 3: WriteThrough aliases; Move kills the source ─────────────

#[test]
fn writethrough_alias_reaches_owner_without_materialize() {
    let src = r#"
struct Bag:
    Vector[int] items
void by_ref(Bag &s):
    s.items.push(99)
void main():
    Bag a = Bag(Vector[int]())
    a.items.push(1)
    by_ref(&a)
    print(a.items.len())
"#;
    let run = go(src);
    assert_eq!(run.stdout.trim_end(), "2");
    assert!(!kinds(&run).contains(&"materialize"), "`&` must not materialize");
}

#[test]
fn amp_bind_alias_writes_through() {
    // `auto r = &b` is a write-through alias binding.
    let src = r#"
void main():
    Vector[int] a = [1, 2, 3]
    Vector[int] b = a
    auto r = &b
    r.push(9)
    print(a.len())
    print(b.len())
"#;
    assert_eq!(out(src), "3\n4");
}

#[test]
fn move_then_read_is_illformed() {
    let src = r#"
struct S:
    String text
void consume(S !s):
    print(s.text)
void main():
    S a = S("hi")
    consume(!a)
    print(a.text)
"#;
    let run = go(src);
    assert!(matches!(run.outcome, Outcome::IllFormed(_)), "read of moved = IllFormed, got {:?}", run.outcome);
    assert_eq!(run.stdout, "hi\n", "output up to the ill-formed read is preserved");
}

// ── §2.2 bullet 4 (drop) + §2.1 scope-exit reverse order ───────────────────

#[test]
fn scope_exit_drops_in_reverse_declaration_order() {
    let src = r#"
struct S:
    String text
void main():
    S a = S("a")
    if true:
        S b = S("b")
        S c = S("c")
    print("done")
"#;
    let run = go(src);
    let drops: Vec<&str> = run
        .trace
        .iter()
        .filter_map(|e| if e.kind() == "drop" { Some(e.place()) } else { None })
        .collect();
    // Inner scope drops c then b; the function scope then drops a.
    assert_eq!(drops, vec!["c", "b", "a"]);
}

// ── Explicit clone ─────────────────────────────────────────────────────────

#[test]
fn explicit_clone_is_independent_and_traced() {
    let src = r#"
void main():
    Vector[int] a = [1, 2, 3]
    Vector[int] b = a.clone()
    b.push(9)
    print(a.len())
    print(b.len())
"#;
    let run = go(src);
    assert_eq!(run.stdout.trim_end(), "3\n4");
    assert!(kinds(&run).contains(&"explicit_clone"), "`.clone()` must emit ExplicitClone");
}

// ── Checked arithmetic ⇒ Trap(Fault) ───────────────────────────────────────

#[test]
fn integer_overflow_traps() {
    let src = r#"
void main():
    int m = 9223372036854775807
    int r = m + 1
    print(r)
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(Fault::Overflow));
}

#[test]
fn division_by_zero_traps() {
    let src = r#"
void main():
    int z = 0
    int r = 10 / z
    print(r)
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(Fault::DivByZero));
}

#[test]
fn index_out_of_bounds_traps() {
    let src = r#"
void main():
    Vector[int] a = [1]
    print(a[5])
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(Fault::Bounds));
}

// ── FuelExhausted is a distinct outcome ────────────────────────────────────

#[test]
fn nontermination_is_fuel_exhausted() {
    let src = r#"
void main():
    loop:
        pass
"#;
    assert_eq!(run_source(src, 1_000).unwrap().outcome, Outcome::FuelExhausted);
}

// ── Desugarings + scalars: float, tuple, for, while, f-string, concat ──────

#[test]
fn float_arithmetic() {
    let src = r#"
void main():
    float x = 1.5
    float y = 2.25
    print(x + y)
"#;
    assert_eq!(out(src), "3.75");
}

#[test]
fn tuple_literal_and_field_access() {
    let src = r#"
void main():
    (int, String) t = (1, "hi")
    print(t.0)
    print(t.1)
"#;
    assert_eq!(out(src), "1\nhi");
}

#[test]
fn for_loop_sums_and_reads_are_views() {
    let src = r#"
int sum(Vector[int] v):
    int total = 0
    for n in v:
        total = total + n
    return total
void main():
    Vector[int] a = [10, 20, 30]
    print(sum(a))
"#;
    assert_eq!(out(src), "60");
}

#[test]
fn while_loop_counts() {
    let src = r#"
void main():
    int i = 0
    int acc = 0
    while i < 4:
        acc = acc + i
        i = i + 1
    print(acc)
"#;
    assert_eq!(out(src), "6");
}

#[test]
fn fstring_interpolates_int_and_string() {
    let src = r#"
void main():
    int n = 5
    String s = "hi"
    print(f"n={n} s={s}")
"#;
    assert_eq!(out(src), "n=5 s=hi");
}

#[test]
fn string_concat() {
    let src = r#"
void main():
    String a = "foo"
    String b = a + "bar"
    print(b)
    print(a)
"#;
    assert_eq!(out(src), "foobar\nfoo");
}

// ── The parse-and-discard import no-op ─────────────────────────────────────

#[test]
fn import_is_discarded() {
    let src = r#"
from std.collections import Vector
void main():
    Vector[int] a = Vector[int]()
    a.push(7)
    print(a.len())
"#;
    assert_eq!(out(src), "1");
}
