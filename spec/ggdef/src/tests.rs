//! Unit tests for the evaluator — at minimum one per §2.2 bullet (the Gates-A
//! requirement), plus the four-outcome coverage (Value/Trap/IllFormed/
//! FuelExhausted) and the desugarings the elaborator performs.

use crate::trace::TraceEvent;
use crate::{run_source, Outcome, Run, TrapKind};

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
    // emits NO BindCopy (it is not a live-place copy) and DOES emit a structural
    // `Move` provenance event (F2) — a fresh-temp bind is a move, not a copy.
    let src = r#"
struct S:
    String text
void main():
    S a = S("hi")
    print(a.text)
"#;
    let run = go(src);
    let k = kinds(&run);
    assert!(!k.contains(&"bind_copy"), "fresh-temp bind must not BindCopy");
    assert!(k.contains(&"move"), "fresh-temp bind of a droppable value emits a structural Move (F2)");
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
fn amp_bind_local_alias_is_rejected() {
    // D10(a) (decisions.md, ratified 2026-07-06): a WriteThrough alias reaches
    // the owner at a CALL argument (the test above), but binding one to a LOCAL
    // NAME (`auto r = &b`) is REJECTED — a place has one exclusive writer, and a
    // named `&`-binding would alias a second writable path. Pre-D10 this
    // write-through-aliased `b` (printed `3\n4`); the definition now models the
    // ratified rejection (see the `d10a_*` cluster). Mirrors production
    // `expr_is_borrow_bind` (src/semantic/typecheck.rs, landed by 414e652a).
    let src = r#"
void main():
    Vector[int] a = [1, 2, 3]
    Vector[int] b = a
    auto r = &b
    r.push(9)
    print(a.len())
    print(b.len())
"#;
    elab_rejects(src, "error[E_LocalBorrowBind]", "local `auto r = &b` bind");
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

// ── Checked arithmetic ⇒ Trap(TrapKind) ─────────────────────────────────────

#[test]
fn integer_overflow_traps() {
    let src = r#"
void main():
    int m = 9223372036854775807
    int r = m + 1
    print(r)
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(TrapKind::Overflow));
}

#[test]
fn division_by_zero_traps() {
    let src = r#"
void main():
    int z = 0
    int r = 10 / z
    print(r)
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(TrapKind::DivByZero));
}

#[test]
fn index_out_of_bounds_traps() {
    let src = r#"
void main():
    Vector[int] a = [1]
    print(a[5])
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(TrapKind::Bounds));
}

// ── The trap registry: codes + the §10.9 catchable subset ──────────────────

#[test]
fn catchable_subset_is_exactly_overflow_divbyzero_bounds() {
    // The §10.9 `Fault` subset (W1): a fault `catch` may recover EXACTLY these.
    assert!(TrapKind::Overflow.is_catchable());
    assert!(TrapKind::DivByZero.is_catchable());
    assert!(TrapKind::Bounds.is_catchable());
    // The uncatchable five (unwrap / assert / panic).
    assert!(!TrapKind::UnwrapNone.is_catchable());
    assert!(!TrapKind::UnwrapError.is_catchable());
    assert!(!TrapKind::UnwrapErrorOnOk.is_catchable());
    assert!(!TrapKind::AssertFailed(String::new()).is_catchable());
    assert!(!TrapKind::Panic(String::new()).is_catchable());
}

#[test]
fn trap_codes_are_mechanical_t_variant() {
    // The `T_<VariantName>` code derives from variant identity, never detail.
    assert_eq!(TrapKind::Overflow.code(), "T_Overflow");
    assert_eq!(TrapKind::DivByZero.code(), "T_DivByZero");
    assert_eq!(TrapKind::Bounds.code(), "T_Bounds");
    assert_eq!(TrapKind::UnwrapNone.code(), "T_UnwrapNone");
    assert_eq!(TrapKind::UnwrapError.code(), "T_UnwrapError");
    assert_eq!(TrapKind::UnwrapErrorOnOk.code(), "T_UnwrapErrorOnOk");
    assert_eq!(TrapKind::AssertFailed("x".into()).code(), "T_AssertFailed");
    assert_eq!(TrapKind::Panic("x".into()).code(), "T_Panic");
}

// ── W2: the three implemented trap classes (assert / panic / unwrap_error) ──

#[test]
fn assert_false_traps_assert_failed() {
    let src = r#"
void main():
    print("before")
    assert 1 > 2, "math broke"
    print("after")
"#;
    let run = go(src);
    assert_eq!(run.outcome, Outcome::Trap(TrapKind::AssertFailed("math broke".to_string())));
    assert_eq!(run.outcome.exit_code(), 101);
    assert_eq!(run.stdout, "before\n", "output before the assert is preserved; `after` is not reached");
}

#[test]
fn assert_true_continues() {
    let src = r#"
void main():
    assert 2 > 1
    print("ok")
"#;
    assert_eq!(out(src), "ok");
}

#[test]
fn assert_false_no_message_uses_default_detail() {
    let src = r#"
void main():
    assert 1 > 2
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(TrapKind::AssertFailed("assertion failed".to_string())));
}

#[test]
fn panic_traps_panic() {
    let src = r#"
void main():
    print("start")
    panic("boom")
"#;
    let run = go(src);
    assert_eq!(run.outcome, Outcome::Trap(TrapKind::Panic("boom".to_string())));
    assert_eq!(run.outcome.exit_code(), 101);
    assert_eq!(run.stdout, "start\n");
}

#[test]
fn unwrap_error_on_ok_traps() {
    let src = r#"
void main():
    Result[int, String] a = Ok(7)
    print(a.unwrap_error())
"#;
    assert_eq!(go(src).outcome, Outcome::Trap(TrapKind::UnwrapErrorOnOk));
}

#[test]
fn unwrap_error_on_error_extracts_payload() {
    let src = r#"
void main():
    Result[int, String] a = Error("nope")
    print(a.unwrap_error())
"#;
    assert_eq!(out(src), "nope");
}

#[test]
fn unwrap_error_traps_uncatchable() {
    // A Trap outcome is exit 101 and NOT in the catchable subset.
    let src = r#"
void main():
    Result[int, String] a = Ok(1)
    print(a.unwrap_error())
"#;
    match go(src).outcome {
        Outcome::Trap(k) => assert!(!k.is_catchable()),
        other => panic!("expected Trap, got {other:?}"),
    }
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
    assert_eq!(go(src).outcome, Outcome::Trap(TrapKind::UnwrapNone), "unwrap-None must Trap(UnwrapNone)");
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

// ══════════════════════════════════════════════════════════════════════════
// Increment B2 — equip, Drop, D4 rejections, with-resource, call reorder
// ══════════════════════════════════════════════════════════════════════════

// ── (B1 output-review R2) call-side named-arg REORDER, both ways ────────────

#[test]
fn named_args_reordered_at_function_calls() {
    // B2 replaces the B1-interim rejection: named call args are REORDERED to the
    // callee's param order via the pass-1 signature registry. `sub(b=3, a=10)`
    // must evaluate as `sub(10, 3)` = 7, NOT the mis-bound `sub(3, 10)` = -7.
    let src = r#"
int sub(int a, int b):
    return a - b

void main():
    print(sub(b=3, a=10))
    print(sub(a=10, b=3))
    print(sub(10, 3))
"#;
    assert_eq!(out(src), "7\n7\n7");
}

#[test]
fn named_args_still_rejected_at_enum_ctor() {
    // Enum-variant/collection-ctor positions keep the B1 rejection (positional
    // binding there would silently mis-bind).
    let src = r#"
enum Pair:
    P(int, int)
void main():
    Pair p = Pair.P(b=1, a=2)
    print("x")
"#;
    match run_source(src, FUEL) {
        Err(e) => assert!(e.to_string().contains("named argument"), "got: {e}"),
        Ok(_) => panic!("named args at an enum ctor must be rejected"),
    }
}

// ── equip method dispatch + D2 (plain self = materialize-on-write) ─────────

#[test]
fn equip_amp_self_mutator_on_bare_param_materializes() {
    // A `&self` mutator called on a bare-value param materializes a private
    // copy (the caller is untouched) — the R38 named-receiver shape.
    let src = r#"
struct Rec:
    String name
equip Rec:
    void set_name(&self, String n):
        self.name = n
String touch(Rec r):
    r.set_name("Y")
    return r.name
void main():
    Rec orig = Rec("A")
    print(touch(orig))
    print(orig.name)
"#;
    assert_eq!(out(src), "Y\nA");
}

#[test]
fn equip_plain_self_write_materializes_d2() {
    // D2: a write through PLAIN `self` materializes a private copy — the caller
    // is untouched, exactly like a bare param.
    let src = r#"
struct Box:
    int n
equip Box:
    void bump(self):
        self.n = self.n + 1
void main():
    Box b = Box(1)
    b.bump()
    print(b.n)
"#;
    assert_eq!(out(src), "1");
}

#[test]
fn equip_user_method_name_collision_with_builtin_get() {
    // A user `get(&self)` mutator COLLIDES with the builtin `.get()`; receiver-
    // type inference (`c: Holder`) must dispatch the USER method, not the
    // builtin. Mirrors cow_named_recv_gate_name_collision.
    let src = r#"
struct Holder:
    String name
equip Holder:
    void get(&self):
        self.name = "Y"
String touch(Holder c):
    c.get()
    return c.name
void main():
    Holder orig = Holder("A")
    print(touch(orig))
    print(orig.name)
"#;
    assert_eq!(out(src), "Y\nA");
}

// ── equip T with Drop — custom-drop EXECUTION (side-effecting + trapping) ───

#[test]
fn custom_drop_side_effect_runs_at_scope_exit() {
    // The corpus's Drop bodies are all `pass`; a side-effecting body proves the
    // custom drop actually EXECUTES at scope exit (after the body prints).
    let src = r#"
struct Loud:
    int id
equip Loud with Drop:
    void drop(!self):
        print("dropped")
void main():
    Loud a = Loud(1)
    print("body")
"#;
    assert_eq!(out(src), "body\ndropped");
}

#[test]
fn custom_drops_run_in_reverse_declaration_order() {
    let src = r#"
struct Loud:
    String tag
equip Loud with Drop:
    void drop(!self):
        print(self.tag)
void main():
    Loud a = Loud("a")
    Loud b = Loud("b")
    print("body")
"#;
    // Reverse declaration order: b drops before a.
    assert_eq!(out(src), "body\nb\na");
}

#[test]
fn custom_drop_that_traps_propagates_the_trap() {
    // A custom drop is arbitrary code — a Trap inside it must escape (drop_scope
    // threads the Halt), overriding the otherwise-Value outcome.
    let src = r#"
struct Bomb:
    int id
equip Bomb with Drop:
    void drop(!self):
        int z = 0
        int r = 1 / z
        print(r)
void main():
    print("before")
    Bomb b = Bomb(1)
"#;
    let run = go(src);
    assert_eq!(run.outcome, Outcome::Trap(TrapKind::DivByZero));
    assert_eq!(run.stdout, "before\n", "output up to the trapping drop is preserved");
}

#[test]
fn custom_drop_does_not_recurse_on_itself() {
    // `drop(!self)` moves self in; self must NOT re-trigger its own custom drop
    // at the drop body's scope exit (else infinite recursion → FuelExhausted).
    let src = r#"
struct Res:
    int id
equip Res with Drop:
    void drop(!self):
        pass
void main():
    Res r = Res(1)
    print("ok")
"#;
    assert_eq!(out(src), "ok");
}

// ── with <expr> as name: — scoped bind + drop-at-block-exit ────────────────

#[test]
fn with_resource_drops_at_block_exit_not_function_exit() {
    // The resource drops at the END of the with-block (before the trailing
    // print), not at function exit — that timing is why `with` is a scoped
    // statement, not an inlined bind.
    let src = r#"
struct Guard:
    int id
equip Guard with Drop:
    void drop(!self):
        print("released")
void main():
    print("open")
    with Guard(1) as g:
        print("inside")
    print("after")
"#;
    assert_eq!(out(src), "open\ninside\nreleased\nafter");
}

#[test]
fn with_fresh_temp_resource_is_a_move_not_a_copy() {
    // A `with Res(1) as r` resource is a fresh-temp Move — a drop-tainted type is
    // single-owner, but a fresh temp is never a live-place copy, so it is NOT a
    // D4 rejection (mirrors cow_element_borrow_source_mutate_with). It must run.
    let src = r#"
struct Res:
    int id
equip Res with Drop:
    void drop(!self):
        pass
void main():
    Vector[String] coll = ["alpha"]
    String s = coll.get(0).unwrap()
    with Res(1) as r:
        coll.push("beta")
    print(s)
    print(coll.len())
"#;
    assert_eq!(out(src), "alpha\n2");
}

// ── D4 rejections — ONE unit test per implicit-copy position (six) ─────────
//
// A drop-tainted type (custom `Drop`) is single-owner: an implicit copy of a
// LIVE PLACE at any of the six positions is `E_MoveWithoutOperator`. Fresh
// temps move and are never rejected.

/// A drop-tainted `struct R` used across the six D4 tests.
const D4_PRELUDE: &str = r#"
struct R:
    int id
equip R with Drop:
    void drop(!self):
        pass
"#;

fn d4_rejects(body: &str, position_hint: &str) {
    let src = format!("{D4_PRELUDE}\n{body}");
    match run_source(&src, FUEL) {
        Err(e) => {
            let s = e.to_string();
            assert!(s.contains("E_MoveWithoutOperator"), "{position_hint}: got: {s}");
        }
        Ok(_) => panic!("{position_hint}: a live-place copy of a drop-tainted value must be rejected"),
    }
}

#[test]
fn d4_position_1_bind() {
    d4_rejects("void main():\n    R a = R(1)\n    R b = a\n", "bind");
}

#[test]
fn d4_position_2_ctor_init() {
    d4_rejects(
        "struct W:\n    R inner\nvoid main():\n    R a = R(1)\n    W w = W(a)\n",
        "ctor-init",
    );
}

#[test]
fn d4_position_3_collection_put() {
    d4_rejects(
        "void main():\n    R a = R(1)\n    Vector[R] v = Vector[R]()\n    v.push(a)\n",
        "collection-put",
    );
}

#[test]
fn d4_position_4_return() {
    d4_rejects("R make():\n    R a = R(1)\n    return a\nvoid main():\n    print(\"x\")\n", "return");
}

#[test]
fn d4_position_5_capture() {
    d4_rejects(
        "void main():\n    R a = R(1)\n    auto f = (): use(a)\n    f()\nvoid use(R x):\n    print(x.id)\n",
        "capture",
    );
}

#[test]
fn d4_position_6_materialize_on_write() {
    // A write through a bare-param BORROW binding of a tainted type would
    // materialize (privatise) it — rejected.
    d4_rejects(
        "void f(R s):\n    s.id = 9\nvoid main():\n    R a = R(1)\n    f(a)\n",
        "materialize-on-write",
    );
}

#[test]
fn d4_allows_fresh_temp_move_and_explicit_move() {
    // The counterpart: fresh temps move (never rejected), and an explicit `!`
    // move at a bind is allowed. Neither is a live-place implicit copy.
    let src = r#"
struct R:
    int id
equip R with Drop:
    void drop(!self):
        pass
void main():
    R a = R(1)
    R b = !a
    print(b.id)
"#;
    assert_eq!(out(src), "1");
}

#[test]
fn d4_position_6_user_amp_self_mutator_on_tainted_borrow_rejected() {
    // B2 output-review R2: a user `&self` mutator through a tainted
    // Borrow-rooted receiver is the user-method sibling of position 6.
    let src = r#"
struct R:
    int id

equip R with Drop:
    void drop(!self):
        print("bye")

equip R:
    void bump(&self):
        self.id = 9

void poke(R s):
    s.bump()

void main():
    R r = R(1)
    poke(r)
"#;
    match run_source(src, FUEL) {
        Err(e) => assert!(e.to_string().contains("materialize-on-write"), "got: {e}"),
        Ok(_) => panic!("tainted &self mutator on a bare param must be rejected (D4 pos 6)"),
    }
}

#[test]
fn d4_user_amp_self_mutator_on_owned_tainted_local_allowed() {
    // Counterpart: `&self` on an OWNED tainted local writes through the owner
    // — no materialize, no implicit copy, legal.
    let src = r#"
struct R:
    int id

equip R with Drop:
    void drop(!self):
        print(f"bye {self.id}")

equip R:
    void bump(&self):
        self.id = 9

void main():
    R r = R(1)
    r.bump()
    print(r.id)
"#;
    assert_eq!(out(src), "9
bye 9");
}

// ── D4 taint parity extensions (A2-R1): prelude-enum payloads, closure-tail,
//    field-place (REJECTION-ONLY) — each closes a production/ggdef divergence ──

#[test]
fn d4_position_1_bind_option_payload_tainted() {
    // The 10th D4 test. Parity with production `is_drop_tainted_type`: an
    // `Option[R]` (R custom `Drop`) is drop-tainted — the prelude enum carries
    // R, so a bare bind is an implicit copy. ggdef phase-0 formerly ERASED
    // Option's payload (Named("Option") → untainted) and ACCEPTED this where
    // production rejects; the `Ty::Option` carrier + `ty_tainted` arm close it.
    // The `Some(R(1))` initializer is a fresh temp (a Call, not a place) and is
    // NOT rejected — only the live-place bind `b = a` is.
    d4_rejects(
        "void main():\n    Option[R] a = Some(R(1))\n    Option[R] b = a\n",
        "bind-option-payload",
    );
}

#[test]
fn d4_bind_result_payload_tainted_both_arms() {
    // Parity: production taints `Result[R,_]` AND `Result[_,R]` (recursion over
    // both generic args). The `Ty::Result` carrier taints if EITHER arm is
    // tainted. Rejection-only — a bare bind of a live Result carrying R copies.
    d4_rejects(
        "void main():\n    Result[R, int] a = Ok(R(1))\n    Result[R, int] b = a\n",
        "bind-result-ok-payload",
    );
    d4_rejects(
        "void main():\n    Result[int, R] a = Error(R(1))\n    Result[int, R] b = a\n",
        "bind-result-error-payload",
    );
}

#[test]
fn d4_closure_tail_param_place_rejected() {
    // Closure-tail (position 4): a closure whose body IS a bare tainted PARAM
    // place (`(R x): x`) returns a copy at the closure return boundary. The
    // rejection fires at closure ELABORATION, so the closure need not be
    // called. Mirrors production's `Expr::Closure` tail arm.
    d4_rejects(
        "void main():\n    auto f = (R x): x\n    print(\"x\")\n",
        "closure-tail-param-place",
    );
}

#[test]
fn d4_closure_tail_fresh_temp_allowed() {
    // The closure-tail counterpart: a FRESH-TEMP tail (`(): R(7)`) moves the
    // freshly-materialized value — never a live-place copy — so it is legal
    // (elaboration succeeds). The tail arm gates on the body being a PLACE;
    // `R(7)` is a ctor call, not a place.
    let src = format!(
        "{D4_PRELUDE}\nvoid main():\n    auto f = (): R(7)\n    print(\"ok\")\n"
    );
    if let Err(e) = run_source(&src, FUEL) {
        panic!("closure fresh-temp tail must be legal, got: {e}");
    }
}

#[test]
fn d4_field_place_bind_rejected() {
    // Field-place (position 1) REJECTION-ONLY: a live tainted FIELD place
    // (`hh.r`) bound bare is an implicit copy — resolved via `infer_ast_ty`'s
    // projection (`field_ty`), mirroring production's structural
    // `lvalue_value_type`. No `!`-move legal counterpart: `!hh.r` is a PARTIAL
    // MOVE (production `E_UseAfterMove`), so `.clone()` is the only fix.
    d4_rejects(
        "struct HH:\n    R r\nvoid main():\n    HH hh = HH(R(1))\n    R c = hh.r\n",
        "field-place-bind",
    );
}

#[test]
fn d4_field_place_return_rejected() {
    // Field-place (position 4) REJECTION-ONLY: returning a live tainted FIELD
    // place (`return h.r`) is an implicit copy at the return boundary. Same
    // structural projection; same rejection-only rationale as the bind shape.
    d4_rejects(
        "struct HH:\n    R r\nR get_r(HH h):\n    return h.r\nvoid main():\n    HH hh = HH(R(1))\n    get_r(hh)\n",
        "field-place-return",
    );
}

// ── `ggdef -- gen` — frontmatter expectation generation (Increment C) ──────

#[test]
fn gen_inserts_expect_block_when_missing() {
    // No `# expect:` yet → gen inserts one before the closing fence, with the
    // observed exit code + JSON-escaped stdout.
    let src = "#!spectest\n# mode: run\n#!end\n\nvoid main():\n    print(9)\n";
    let got = crate::gen_frontmatter(src, FUEL).unwrap();
    assert!(got.contains("# expect:\n#   exit: 0\n#   stdout: \"9\\n\"\n"), "got:\n{got}");
    // And the program still runs after gen (frontmatter is comments).
    assert_eq!(out(&got), "9");
}

#[test]
fn gen_is_idempotent_and_replaces_stale_expect() {
    // A STALE expect block is overwritten, and a second gen is a byte no-op.
    let stale = "#!spectest\n# mode: run\n# expect:\n#   exit: 0\n#   stdout: \"WRONG\"\n#!end\n\nvoid main():\n    print(9)\n";
    let once = crate::gen_frontmatter(stale, FUEL).unwrap();
    assert!(once.contains("#   stdout: \"9\\n\""), "stale not replaced:\n{once}");
    assert!(!once.contains("WRONG"), "stale value survived:\n{once}");
    let twice = crate::gen_frontmatter(&once, FUEL).unwrap();
    assert_eq!(once, twice, "gen is not idempotent");
}

#[test]
fn gen_without_fence_is_an_error() {
    let src = "void main():\n    print(9)\n";
    assert!(matches!(crate::gen_frontmatter(src, FUEL), Err(crate::GenError::NoFrontmatter)));
}

#[test]
fn gen_escapes_multiline_stdout() {
    // Multi-line stdout round-trips through JSON escaping.
    let src = "#!spectest\n# mode: run\n#!end\n\nvoid main():\n    print(1)\n    print(2)\n";
    let got = crate::gen_frontmatter(src, FUEL).unwrap();
    assert!(got.contains("#   stdout: \"1\\n2\\n\""), "got:\n{got}");
}

// ── §10.3 Type-Directed Result Capture (language-reference.md) ─────────────
//
// A throws-call at a destination DECLARED `Result[_,_]` captures the full
// `Result` instead of auto-propagating (§10.1). Every expectation below is
// the GROUND-TRUTHED production output (`gg run`, probed 2026-07-06) — never
// invented. The `RISKY` prelude is the shared throws helper.

const RISKY: &str = r#"
int risky(int x) throws String:
    if x < 0:
        throw "negative"
    return x * 2
"#;

/// Assert the frontend rejects `src` with an ElabError containing `needle`.
fn elab_rejects(src: &str, needle: &str, hint: &str) {
    match run_source(src, FUEL) {
        Err(e) => {
            let s = e.to_string();
            assert!(s.contains(needle), "{hint}: expected `{needle}` in: {s}");
        }
        Ok(r) => panic!("{hint}: must be a LOUD frontend error, got {:?}", r.outcome),
    }
}

#[test]
fn s103_vardecl_captures_in_throws_fn() {
    // The R1 falsifying repro: `Result[int, String] r = risky(x)` inside a
    // throws fn CAPTURES (error path recovers locally; ok path is a Value —
    // both were wrong before: silent propagate / false IllFormed).
    let src = format!(
        "{RISKY}
int compute(int x) throws String:
    Result[int, String] r = risky(x)
    match r:
        case Ok(v):
            return v
        case Error(e):
            print(f\"recovered from {{e}}\")
            return -1

void main():
    match compute(-5):
        case Ok(v):
            print(f\"got {{v}}\")
        case Error(e):
            print(f\"propagated {{e}}\")
    match compute(5):
        case Ok(v):
            print(f\"got {{v}}\")
        case Error(e):
            print(f\"propagated {{e}}\")
"
    );
    assert_eq!(out(&src), "recovered from negative\ngot -1\ngot 10");
}

#[test]
fn s103_vardecl_captures_in_non_throws_fn() {
    // §10.3 is type-directed, not context-directed: capture also fires in a
    // NON-throws fn (where propagate would be ill-formed).
    let src = format!(
        "{RISKY}
void main():
    Result[int, String] r = risky(-5)
    match r:
        case Ok(v):
            print(f\"ok {{v}}\")
        case Error(e):
            print(f\"err {{e}}\")
    Result[int, String] r2 = risky(5)
    match r2:
        case Ok(v):
            print(f\"ok {{v}}\")
        case Error(e):
            print(f\"err {{e}}\")
"
    );
    assert_eq!(out(&src), "err negative\nok 10");
}

#[test]
fn s103_assign_target_result_captures() {
    let src = format!(
        "{RISKY}
int compute(int x) throws String:
    Result[int, String] r = Ok(0)
    r = risky(x)
    match r:
        case Ok(v):
            return v
        case Error(e):
            print(f\"recovered from {{e}}\")
            return -1

void main():
    match compute(-5):
        case Ok(v):
            print(f\"got {{v}}\")
        case Error(e):
            print(f\"propagated {{e}}\")
"
    );
    assert_eq!(out(&src), "recovered from negative\ngot -1");
}

#[test]
fn s103_return_captures_in_non_throws_result_fn() {
    // `return risky(x)` in a NON-throws fn declared `Result[int, String]`:
    // the return slot is Result-typed → capture (production runs this).
    let src = format!(
        "{RISKY}
Result[int, String] wrap(int x):
    return risky(x)

void main():
    match wrap(-5):
        case Ok(v):
            print(f\"ok {{v}}\")
        case Error(e):
            print(f\"err {{e}}\")
    match wrap(5):
        case Ok(v):
            print(f\"ok {{v}}\")
        case Error(e):
            print(f\"err {{e}}\")
"
    );
    assert_eq!(out(&src), "err negative\nok 10");
}

#[test]
fn s103_param_arg_result_captures() {
    // A throws-call arg into a param DECLARED `Result[int, String]` captures —
    // from a non-throws caller AND from inside a throws fn.
    let src = format!(
        "{RISKY}
void show(Result[int, String] r):
    match r:
        case Ok(v):
            print(f\"ok {{v}}\")
        case Error(e):
            print(f\"err {{e}}\")

int go() throws String:
    show(risky(-5))
    show(risky(5))
    return 0

void main():
    show(risky(-7))
    match go():
        case Ok(_):
            print(\"go done\")
        case Error(e):
            print(f\"go propagated {{e}}\")
"
    );
    assert_eq!(out(&src), "err negative\nerr negative\nok 10\ngo done");
}

#[test]
fn s103_struct_field_result_captures() {
    // A throws-call into a struct-ctor field DECLARED `Result[int, String]`
    // captures (production ground truth: the field init is a typed dest).
    let src = format!(
        "{RISKY}
struct Holder:
    Result[int, String] r

int go() throws String:
    Holder h = Holder(risky(-5))
    match h.r:
        case Ok(v):
            print(f\"ok {{v}}\")
        case Error(e):
            print(f\"err {{e}}\")
    return 0

void main():
    match go():
        case Ok(_):
            print(\"go done\")
        case Error(e):
            print(f\"go propagated {{e}}\")
"
    );
    assert_eq!(out(&src), "err negative\ngo done");
}

#[test]
fn s103_return_in_throws_fn_with_result_t_captures() {
    // A throws fn whose declared T is ITSELF `Result[int, String]`, returning
    // a throws-callee whose own T is NOT Result: the callee's full Result is
    // captured as the T value, then Ok-wrapped → `Ok(Error("negative"))`.
    let src = format!(
        "{RISKY}
Result[int, String] outer(int x) throws String:
    return risky(x)

void main():
    match outer(-5):
        case Ok(inner):
            match inner:
                case Ok(v):
                    print(f\"inner ok {{v}}\")
                case Error(e):
                    print(f\"inner err {{e}}\")
        case Error(e):
            print(f\"outer err {{e}}\")
    match outer(5):
        case Ok(inner):
            match inner:
                case Ok(v):
                    print(f\"inner ok {{v}}\")
                case Error(e):
                    print(f\"inner err {{e}}\")
        case Error(e):
            print(f\"outer err {{e}}\")
"
    );
    assert_eq!(out(&src), "inner err negative\ninner ok 10");
}

#[test]
fn s103_return_in_throws_fn_callee_t_result_propagates() {
    // The complement: the throws-callee's own declared T IS `Result` — its
    // outer Result auto-propagates (peel) and the INNER Result is the T value
    // (re-wrapped once). `f(0)` throws → outer Error; `f(-3)`/`f(3)` →
    // Ok(inner).
    let src = "
Result[int, String] g(int x) throws String:
    if x == 0:
        throw \"zero\"
    if x < 0:
        return Error(\"inner-neg\")
    return Ok(x * 2)

Result[int, String] f(int x) throws String:
    return g(x)

void main():
    match f(0):
        case Ok(inner):
            match inner:
                case Ok(v):
                    print(f\"inner ok {v}\")
                case Error(e):
                    print(f\"inner err {e}\")
        case Error(e):
            print(f\"outer err {e}\")
    match f(-3):
        case Ok(inner):
            match inner:
                case Ok(v):
                    print(f\"inner ok {v}\")
                case Error(e):
                    print(f\"inner err {e}\")
        case Error(e):
            print(f\"outer err {e}\")
    match f(3):
        case Ok(inner):
            match inner:
                case Ok(v):
                    print(f\"inner ok {v}\")
                case Error(e):
                    print(f\"inner err {e}\")
        case Error(e):
            print(f\"outer err {e}\")
";
    assert_eq!(out(src), "outer err zero\ninner err inner-neg\ninner ok 6");
}

#[test]
fn s103_nested_arg_still_propagates_inside_capture() {
    // The capture flag binds to the OUTERMOST call only: a nested throws-call
    // in the captured call's args still auto-propagates (§10.1).
    let src = format!(
        "{RISKY}
int go(int x) throws String:
    Result[int, String] r = risky(risky(x))
    match r:
        case Ok(v):
            return v
        case Error(e):
            print(f\"captured {{e}}\")
            return -1

void main():
    match go(3):
        case Ok(v):
            print(f\"got {{v}}\")
        case Error(e):
            print(f\"propagated {{e}}\")
    match go(-3):
        case Ok(v):
            print(f\"got {{v}}\")
        case Error(e):
            print(f\"propagated {{e}}\")
"
    );
    assert_eq!(out(&src), "got 12\npropagated negative");
}

#[test]
fn s103_callee_t_result_at_typed_dest_is_loud() {
    // A throws-callee whose declared T is ITSELF Result, feeding a Result-
    // typed VarDecl: name-level types cannot tell capture (outer) from
    // propagate (inner) apart — and production MISCOMPILES this shape
    // (garbage payloads, probed 2026-07-06). LOUD, never modeled.
    let src = "
Result[int, String] g(int x) throws String:
    if x == 0:
        throw \"zero\"
    return Ok(x * 2)

int go(int x) throws String:
    Result[int, String] r = g(x)
    return 0

void main():
    match go(3):
        case Ok(_):
            pass
        case Error(_):
            pass
";
    elab_rejects(src, "undecidable", "callee-T-Result at a typed dest");
}

#[test]
fn s103_expr_body_tail_throws_call_into_result_ret_is_loud() {
    // Production REJECTS a throws-call expression-body tail where the declared
    // return is Result (capture does not apply at expr-body tails; probed:
    // "type mismatch: expected Result[int, String], found int"). Mirrored as
    // a loud ElabError, not a silently differently-shaped value.
    let src = format!(
        "{RISKY}
Result[int, String] outer(int x) throws String: risky(x)

void main():
    match outer(5):
        case Ok(_):
            pass
        case Error(_):
            pass
"
    );
    elab_rejects(&src, "expression-body tail", "expr-body tail capture");
}

// ── D10(a): local `&`-binds are rejected in the definition ──────────────────
//
// The ratified exclusivity package (decisions.md, D10 2026-07-06 + the D10(a)
// move-bind addendum 2026-07-11): a named `&`-binding aliases a second live
// writable path to a place, so the definition REJECTS it — mirroring production
// `expr_is_borrow_bind` / `block_tail_is_borrow_bind` (src/semantic/typecheck.rs,
// landed by 414e652a). The diagnostic carries `error[E_LocalBorrowBind]`, the
// code the corpus expectation greps. Move-binds (`R b = !a`) stay LEGAL — they
// kill the source, so no aliasing exists (D10(a) addendum).

#[test]
fn d10a_bare_amp_bind_rejected() {
    // `cow_amp_bind_ref` shape: `auto r = &b` — the standalone bare local
    // `&`-bind. Pre-D10 this write-through-aliased `b`; now rejected.
    let src = r#"
void main():
    Vector[int] a = [1, 2, 3]
    Vector[int] b = a
    auto r = &b
    r.push(9)
    print(b.len())
"#;
    elab_rejects(src, "error[E_LocalBorrowBind]", "bare `auto r = &b`");
}

#[test]
fn d10a_projected_amp_bind_rejected() {
    // `cow_amp_bind_ref_field` shape: `auto r = &b.data` — a projected local
    // `&`-bind. Same class as the bare form (one exclusive writer per place).
    let src = r#"
struct Holder:
    Vector[int] data
void main():
    Vector[int] init = [1, 2, 3]
    Holder a = Holder(init)
    Holder b = a
    auto r = &b.data
    r.push(9)
    print(b.data.len())
"#;
    elab_rejects(src, "error[E_LocalBorrowBind]", "projected `auto r = &b.data`");
}

#[test]
fn d10a_do_tail_amp_bind_rejected() {
    // The `do:`-tail dodge (`amp_bind_doexpr_error` shape): the block's value IS
    // its tail statement, so a `&a` tail is the same named-`&`-bind. The check
    // recurses the block tail and rejects BEFORE the subset guard would fire on
    // `do:` — proving the recursion, not just the top-level `&expr` arm.
    let src = r#"
void main():
    Vector[int] a = [1, 2, 3]
    auto r = do:
        &a
    r.push(9)
    print(r.len())
"#;
    elab_rejects(src, "error[E_LocalBorrowBind]", "`do:`-tail `&a`");
}

#[test]
fn d10a_amp_arg_at_call_is_legal_control() {
    // The legal control: `&a` at a CALL argument is a frame-scoped borrow, NOT
    // a named bind — accepted and write-through (the caller sees the push). The
    // rejection helper is deliberately NOT a deep walk, so `f(&a)` is never
    // visited. Confirms the check does not over-fire on the legal `&`-arg form.
    let src = r#"
void grow(Vector[int] &v):
    v.push(9)
void main():
    Vector[int] a = [1, 2, 3]
    grow(&a)
    print(a.len())
"#;
    assert_eq!(out(src), "4");
}

// ── the `render_expect_block_from` seam round-trips json_escape (D2 prep) ─────
#[test]
fn render_expect_block_from_round_trips_json_escape() {
    use crate::frontmatter::parse_json_string;
    use crate::render_expect_block_from;
    // Cover the five escapes json_escape emits plus passthrough + the empty
    // string. The stdout line's quoted payload, decoded by the reader's inverse
    // (parse_json_string), must equal the original — so a value serialized by
    // render_expect_block_from always parses back byte-exact.
    for s in ["", "9\n", "plain", "a\"b\\c\nd\te\r", "line1\nline2\n", "tab\tend"] {
        // No-trap outcome: a 3-line block (no `trap:` line).
        let block = render_expect_block_from(0, s, None);
        assert_eq!(block.len(), 3);
        assert_eq!(block[0], "# expect:");
        assert_eq!(block[1], "#   exit: 0");
        let quoted = &block[2][block[2].find('"').expect("stdout line has a quote")..];
        assert_eq!(parse_json_string(quoted).unwrap(), s, "round-trip failed for {s:?}");
    }
    // The exit code is threaded verbatim (not hardcoded to 0).
    assert_eq!(render_expect_block_from(101, "", None)[1], "#   exit: 101");
    // A Trap outcome appends the `trap:` line at index 3, keeping exit@1/stdout@2.
    let trap_block = render_expect_block_from(101, "pre\n", Some("T_Overflow"));
    assert_eq!(trap_block.len(), 4);
    assert_eq!(trap_block[1], "#   exit: 101");
    assert_eq!(trap_block[3], "#   trap: T_Overflow");
}
