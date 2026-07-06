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

// ══════════════════════════════════════════════════════════════════════════
// Increment B1 — the non-equip surface
// ══════════════════════════════════════════════════════════════════════════

// ── Option / Result + .get()/.unwrap()/.unwrap_or() ────────────────────────

#[test]
fn option_some_none_unwrap_or() {
    let src = r#"
void main():
    Option[int] a = Some(5)
    Option[int] b = None
    print(a.unwrap())
    print(a.unwrap_or(0))
    print(b.unwrap_or(99))
"#;
    assert_eq!(out(src), "5\n5\n99");
}

#[test]
fn unwrap_none_traps() {
    let src = r#"
void main():
    Option[int] b = None
    print(b.unwrap())
"#;
    assert!(matches!(go(src).outcome, Outcome::Trap(Fault::Panic(_))), "unwrap-None must Trap(Panic)");
}

#[test]
fn result_ok_error_unwrap() {
    // Result is an ordinary enum too (unit-tested — no phase-0 corpus fixture).
    let src = r#"
void main():
    Result[int, String] a = Ok(7)
    print(a.unwrap())
    print(a.unwrap_or(0))
    Result[int, String] e = Error("boom")
    print(e.unwrap_or(42))
"#;
    assert_eq!(out(src), "7\n7\n42");
}

#[test]
fn vector_get_out_of_bounds_is_none() {
    let src = r#"
void main():
    Vector[int] v = [10, 20]
    print(v.get(0).unwrap())
    print(v.get(5).unwrap_or(-1))
"#;
    assert_eq!(out(src), "10\n-1");
}

#[test]
fn vector_pop_clear_fill() {
    let src = r#"
void main():
    Vector[int] v = [1, 2, 3]
    print(v.pop().unwrap())
    print(v.len())
    v.fill(4, 7)
    print(v.len())
    print(v.get(0).unwrap())
    v.clear()
    print(v.len())
"#;
    assert_eq!(out(src), "3\n2\n4\n7\n0");
}

// ── Dict / Set (insertion-ordered) ─────────────────────────────────────────

#[test]
fn dict_set_get_len_and_update_preserves_order() {
    let src = r#"
void main():
    Dict[String, int] d = Dict[String, int]()
    d.set("a", 1)
    d.set("b", 2)
    d.set("a", 9)
    print(d.len())
    print(d.get("a").unwrap())
    print(d.get("b").unwrap())
    print(d.get("z").unwrap_or(-1))
"#;
    assert_eq!(out(src), "2\n9\n2\n-1");
}

#[test]
fn set_add_dedups_and_clones_independently() {
    let src = r#"
void main():
    Set[String] s1 = Set[String]()
    s1.add("x")
    s1.add("x")
    s1.add("y")
    print(s1.len())
    Set[String] s2 = s1.clone()
    s2.add("z")
    print(s1.len())
    print(s2.len())
"#;
    assert_eq!(out(src), "2\n2\n3");
}

// ── match + user payload enums + pattern bindings ──────────────────────────

#[test]
fn match_expression_binds_payload() {
    let src = r#"
enum Token:
    Ident(String)
    Number(int)
String describe(Token t):
    return match t:
        case Token.Ident(s): s
        case Token.Number(_): "num"
void main():
    print(describe(Token.Ident("hi")))
    print(describe(Token.Number(42)))
"#;
    assert_eq!(out(src), "hi\nnum");
}

#[test]
fn match_statement_literal_and_else() {
    let src = r#"
void main():
    int sel = 2
    match sel:
        case 1:
            print("one")
        case 2:
            print("two")
        else:
            print("other")
"#;
    assert_eq!(out(src), "two");
}

#[test]
fn match_binding_reads_payload_view() {
    // A pattern binding is a Borrow view of the scrutinee's payload (reached
    // via `Proj::Payload`); reading it copies the payload out at the return.
    let src = r#"
enum Wrap:
    W(String)
    Empty()
String peek(Wrap w):
    return match w:
        case Wrap.W(inner): inner
        else: "none"
void main():
    Wrap a = Wrap.W("hello")
    print(peek(a))
    Wrap b = Wrap.Empty()
    print(peek(b))
"#;
    assert_eq!(out(src), "hello\nnone");
}

// ── ranges + string slices / indexing ──────────────────────────────────────

#[test]
fn string_index_and_slice() {
    let src = r#"
void main():
    String s = "hello"
    print(s[1])
    print(s[0..3])
    print(s.substring(1, 4))
    print(s.trim())
"#;
    assert_eq!(out(src), "e\nhel\nell\nhello");
}

#[test]
fn for_over_range_and_string() {
    let src = r#"
void main():
    int total = 0
    for i in 0..4:
        total = total + i
    print(total)
    String out = ""
    for c in "abc":
        out = out + c
    print(out)
"#;
    assert_eq!(out(src), "6\nabc");
}

// ── named-arg construction ─────────────────────────────────────────────────

#[test]
fn named_arg_construction_reorders() {
    let src = r#"
struct Point:
    int x
    int y
void main():
    Point p = Point(y=2, x=1)
    print(p.x)
    print(p.y)
"#;
    assert_eq!(out(src), "1\n2");
}

// ── by-value closures ──────────────────────────────────────────────────────

#[test]
fn closure_captures_by_value_at_creation() {
    // The closure snapshots `v` at creation; a later mutation of the outer `v`
    // is invisible to the closure, and the closure's own push does not leak
    // out (per-call private copy). Mirrors cow_closure_deferred_mutate.
    let src = r#"
void main():
    Vector[int] v = [1, 2]
    auto grow = (): v.push(9)
    String snap = "x"
    grow()
    grow()
    print(v.len())
    print(snap)
"#;
    assert_eq!(out(src), "2\nx");
}

#[test]
fn closure_reads_captured_snapshot() {
    let src = r#"
void main():
    String s = "hello"
    auto f = (): print(s)
    f()
    print(s)
"#;
    assert_eq!(out(src), "hello\nhello");
}

// ── std.conv.int_to_str shim ───────────────────────────────────────────────

#[test]
fn int_to_str_shim() {
    let src = r#"
from std.conv import int_to_str
void main():
    String s = int_to_str(42)
    print(s + "!")
"#;
    assert_eq!(out(src), "42!");
}

// ── sized ints + `as`-cast saturation (unit-tested only — no corpus) ────────

#[test]
fn cast_float_to_int_saturates() {
    // float→int SATURATES (the ratified 2026-04-24 both-backend rule), not
    // wraps: 5e9 clamps to int8's max, and a negative float clamps to 0 for
    // an unsigned target.
    let src = r#"
void main():
    float big = 5000000000.0
    int8 a = big as int8
    print(a)
    float small = 3.9
    int b = small as int
    print(b)
    float neg = 0.0 - 10.0
    uint8 c = neg as uint8
    print(c)
"#;
    assert_eq!(out(src), "127\n3\n0");
}

#[test]
fn cast_int_narrowing_wraps() {
    // int→int narrowing truncates/wraps (two's-complement, Rust `as`): 300 as
    // int8 = 44 (300 - 256), and 200 as int8 = -56.
    let src = r#"
void main():
    int x = 300
    int8 a = x as int8
    print(a)
    int y = 200
    int8 b = y as int8
    print(b)
"#;
    assert_eq!(out(src), "44\n-56");
}
