    use crate::parser::Parser;
    use crate::semantic;
    use super::*;

    fn check(source: &str) -> Vec<SemanticError> {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);
        let result = semantic::analyze(&mut module, &[]);
        result.errors
    }

    fn has_error(errors: &[SemanticError], pred: impl Fn(&SemanticErrorKind) -> bool) -> bool {
        errors.iter().any(|e| pred(&e.kind))
    }

    #[test]
    fn use_after_move() {
        let source = "\
void main():
    String s1 = \"hello\"
    String s2 = !s1
    print(s1)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s1")),
            "expected UseAfterMove for s1, got: {:?}", errors
        );
    }

    #[test]
    fn double_move() {
        let source = "\
void main():
    String s = \"hello\"
    String a = !s
    String b = !s
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { name, .. } if name == "s")),
            "expected DoubleMove for s, got: {:?}", errors
        );
    }

    #[test]
    fn move_in_loop() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    for i in 0..3:
        consume(!s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "s")),
            "expected MoveInLoop for s, got: {:?}", errors
        );
    }

    #[test]
    fn copy_types_ok() {
        let source = "\
void main():
    int a = 5
    int b = a
    int c = a
    print(\"{b}\")
    print(\"{c}\")
";
        let errors = check(source);
        // int is Copy — no errors expected from borrow checker
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::MoveWithoutOperator { .. }
                    | SemanticErrorKind::UseAfterMove { .. }
            )),
            "unexpected borrow errors for Copy types: {:?}", errors
        );
    }

    #[test]
    fn move_then_new_decl_ok() {
        // After moving s, declaring a new s in the same scope is fine
        // (The old s is gone, but the new one is a fresh variable)
        let source = "\
void main():
    String s1 = \"hello\"
    String s2 = !s1
    int x = 5
    int y = x
";
        let errors = check(source);
        // No borrow errors: s1 moved once (valid), x is Copy
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
                    | SemanticErrorKind::DoubleMove { .. }
            )),
            "unexpected borrow errors: {:?}", errors
        );
    }

    #[test]
    fn reassignment_revives() {
        let source = "\
void main():
    String s = \"hello\"
    String t = !s
    s = \"world\"
    print(s)
";
        let errors = check(source);
        // After moving s and reassigning it, s is live again — no errors
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
            )),
            "unexpected UseAfterMove after reassignment: {:?}", errors
        );
    }

    // ── Ownership mismatch tests ──

    #[test]
    fn ownership_mismatch_move_param_bare_call() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    consume(s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { param_name, .. } if param_name == "s")),
            "expected OwnershipMismatch, got: {:?}", errors
        );
    }

    #[test]
    fn ownership_mismatch_borrow_param_move_call() {
        let source = "\
void read_it(String &s):
    pass

void main():
    String s = \"hello\"
    read_it(!s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { param_name, .. } if param_name == "s")),
            "expected OwnershipMismatch, got: {:?}", errors
        );
    }

    #[test]
    fn ownership_mismatch_bare_param_mut_call() {
        let source = "\
void look(int x):
    pass

void main():
    int x = 5
    look(&x)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { param_name, .. } if param_name == "x")),
            "expected OwnershipMismatch, got: {:?}", errors
        );
    }

    #[test]
    fn ownership_match_move_ok() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    consume(!s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { .. })),
            "unexpected OwnershipMismatch: {:?}", errors
        );
    }

    #[test]
    fn ownership_match_borrow_ok() {
        let source = "\
void read_it(String &s):
    pass

void main():
    String s = \"hello\"
    read_it(&s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::OwnershipMismatch { .. })),
            "unexpected OwnershipMismatch: {:?}", errors
        );
    }

    // ── Aliasing conflict tests ──

    #[test]
    fn aliasing_double_mut_borrow() {
        let source = "\
void both(String &a, String &b):
    pass

void main():
    String s = \"hello\"
    both(&s, &s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { name, .. } if name == "s")),
            "expected BorrowConflict for double &, got: {:?}", errors
        );
    }

    #[test]
    fn aliasing_borrow_and_mut_borrow() {
        let source = "\
void mixed(String a, String &b):
    pass

void main():
    String s = \"hello\"
    mixed(s, &s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { name, .. } if name == "s")),
            "expected BorrowConflict for bare + &, got: {:?}", errors
        );
    }

    #[test]
    fn aliasing_mut_borrow_and_move() {
        let source = "\
void danger(String &a, String !b):
    pass

void main():
    String s = \"hello\"
    danger(&s, !s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { name, .. } if name == "s")),
            "expected BorrowConflict for & + !, got: {:?}", errors
        );
    }

    #[test]
    fn aliasing_double_bare_ok() {
        let source = "\
void both(int a, int b):
    pass

void main():
    int x = 5
    both(x, x)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowConflict { .. })),
            "unexpected BorrowConflict for double bare: {:?}", errors
        );
    }

    #[test]
    fn consume_callable_double_call() {
        let source = "\
int apply_once(ConsumeCallable[int(int)] f, int x):
    return f(x)

void main():
    ConsumeCallable[int(int)] f = !(n): n * 2
    int r1 = f(5)
    int r2 = f(10)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { name, .. } if name == "f")),
            "expected DoubleMove for f, got: {:?}", errors
        );
    }

    #[test]
    fn consume_callable_single_call_ok() {
        let source = "\
void main():
    ConsumeCallable[int(int)] f = !(n): n * 2
    int r = f(5)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DoubleMove { .. }
                    | SemanticErrorKind::MoveInLoop { .. }
                    | SemanticErrorKind::UseAfterMove { .. }
            )),
            "unexpected borrow errors for single ConsumeCallable call: {:?}", errors
        );
    }

    #[test]
    fn consume_callable_loop_error() {
        let source = "\
void main():
    ConsumeCallable[int(int)] f = !(n): n * 2
    for i in 0..3:
        int r = f(i)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "f")),
            "expected MoveInLoop for f, got: {:?}", errors
        );
    }

    #[test]
    fn if_else_branch_merging() {
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    if true:
        consume(!s)
    else:
        pass
    print(s)
";
        let errors = check(source);
        // s is moved in one branch but not the other — conservative: treat as moved
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s")),
            "expected UseAfterMove after conditional move, got: {:?}", errors
        );
    }

    #[test]
    fn move_in_divergent_branch_ok() {
        // Moving in a branch that always returns should not mark the variable as moved
        // at the join point — the divergent branch never reaches the join.
        let source = "\
void consume(String !s):
    pass

int main():
    String s = \"hello\"
    if true:
        consume(!s)
        return 1
    print(s)
    return 0
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { name, .. } if name == "s")),
            "should NOT get UseAfterMove when move is in a divergent branch, got: {:?}", errors
        );
    }

    // ── Lifetime inference tests ──

    #[test]
    fn return_str_literal_ok() {
        let source = "\
String f(): \"hello\"
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for string literal: {:?}", errors
        );
    }

    #[test]
    fn return_str_from_param_ok() {
        let source = "\
String f(String s): s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for param forwarding: {:?}", errors
        );
    }

    #[test]
    fn return_str_from_local_string() {
        // With str→String unification, `String s = "hi"` where "hi" is a view
        // makes s owned via provenance. Returning an owned string transfers
        // ownership safely — no DanglingReturn.
        let source = "\
String f():
    String s = \"hi\"
    return s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn — owned string return transfers ownership safely: {:?}", errors
        );
    }

    #[test]
    fn use_str_after_string_moved() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so moving s doesn't invalidate v.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hi\" + \"\"
    String v = s
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn cross_function_borrow_ok() {
        let source = "\
String id(String s): s

void main():
    print(id(\"hi\"))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for cross-function borrow: {:?}", errors
        );
    }

    #[test]
    fn cross_function_chain() {
        let source = "\
String f(String s): s

String g(String s): f(s)

void main():
    print(g(\"hello\"))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for chained cross-function borrow: {:?}", errors
        );
    }

    #[test]
    fn cross_function_dangling() {
        // User-defined functions always return owned strings (IR clones on return),
        // so the value is independent — no dangling.
        let source = "\
String id(String s): s

void consume(String !s):
    pass

void main():
    String s = \"hi\" + \"\"
    String v = id(s)
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "unexpected UseAfterSourceMoved — id() returns owned string, v is independent of s: {:?}", errors
        );
    }

    #[test]
    fn return_str_from_expression_body_local() {
        // With str→String unification, returning a local owned string transfers
        // ownership safely — no DanglingReturn.
        let source = "\
String bad():
    String s = \"hello\"
    return s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn — owned string return transfers ownership safely: {:?}", errors
        );
    }

    #[test]
    fn str_from_param_through_local_ok() {
        let source = "\
String f(String s):
    String local = s
    return local
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for param forwarded through local: {:?}", errors
        );
    }

    #[test]
    fn str_view_reassigned_ok() {
        let source = "\
void main():
    String s = \"hello\"
    String v = s
    v = \"world\"
    String t = !s
    print(v)
";
        let errors = check(source);
        // v was reassigned to a literal before s was moved — no error
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved after reassignment: {:?}", errors
        );
    }

    // ── Struct borrowing tests (Phase 4) ──

    #[test]
    fn struct_str_field_auto() {
        // Struct with a str field assigned from param — no error
        let source = "\
struct View:
    String name

void main():
    String s = \"hello\"
    View v = View(s)
    print(v.name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for struct with str field: {:?}", errors
        );
    }

    #[test]
    fn struct_outlives_source() {
        // CoW rule (b): `View(s)` does NOT consume a live `s` — the lowering
        // clones at the struct-init boundary because `s` is still used by the
        // following `consume(!s)`. So `consume(!s)` is the FIRST and only move
        // of the original `s`; there is no double move. `v.name` reads the
        // independent clone. (ASan-verified clean: clone-if-live, no UAF.)
        let source = "\
struct View:
    String name

void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    View v = View(s)
    consume(!s)
    print(v.name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { .. }
                | SemanticErrorKind::UseAfterMove { .. })),
            "rule (b): View(s) clones a live s, so consume(!s) is sound; got: {:?}", errors
        );
    }

    #[test]
    fn struct_from_literal_ok() {
        // Struct with str field from string literal → no error (Static origin)
        let source = "\
struct View:
    String name
    String name

void main():
    View v = View(\"hello\")
    print(v.name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for struct with literal str field: {:?}", errors
        );
    }

    #[test]
    fn struct_no_ref_fields_unaffected() {
        // Struct without reference-type fields — no borrow tracking
        let source = "\
struct Point:
    float x
    float y

void main():
    Point p = Point(1.0, 2.0)
    print(\"{p.x}\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for non-ref struct: {:?}", errors
        );
    }

    #[test]
    fn struct_transitive_borrow() {
        // CoW rule (b): `Inner(s)` does NOT consume a live `s` — it clones at
        // the struct-init boundary because `s` is still used by `consume(!s)`.
        // So `consume(!s)` is the first and only move of the original; no
        // double move. `o.inner.name` reads the independent clone.
        let source = "\
struct Inner:
    String name

struct Outer:
    Inner inner

void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    Inner i = Inner(s)
    Outer o = Outer(i)
    consume(!s)
    print(o.inner.name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { .. }
                | SemanticErrorKind::UseAfterMove { .. })),
            "rule (b): Inner(s) clones a live s, so consume(!s) is sound; got: {:?}", errors
        );
    }

    #[test]
    fn struct_mixed_fields() {
        // CoW rule (b): `Tagged(s, 42)` does NOT consume a live `s` — it clones
        // the String field at the boundary because `s` is still used by
        // `consume(!s)`. The int field is Copy. So `consume(!s)` is the first
        // and only move of the original; no double move. `t.label` reads the
        // independent clone.
        let source = "\
struct Tagged:
    String label
    int count

void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    Tagged t = Tagged(s, 42)
    consume(!s)
    print(t.label)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { .. }
                | SemanticErrorKind::UseAfterMove { .. })),
            "rule (b): Tagged(s, 42) clones a live s, so consume(!s) is sound; got: {:?}", errors
        );
    }

    // ── Phase 6: Branch origin merging ──

    #[test]
    fn branch_origin_merging_if_one_moves() {
        // With StringView removed, `String v = s` is an owned copy (CoW), not a view.
        // v is independent of s, so moving s doesn't invalidate v.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    if true:
        consume(!s)
    else:
        pass
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn branch_origin_merging_both_move() {
        // With StringView removed, `String v = s` is an owned copy (CoW), not a view.
        // v is independent of s, so moving s doesn't invalidate v.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    if true:
        consume(!s)
    else:
        consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn branch_origin_merging_neither_moves() {
        // No moves in any branch → no error
        let source = "\
void main():
    String s = \"hello\"
    String v = s
    if true:
        pass
    else:
        pass
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved when neither branch moves: {:?}", errors
        );
    }

    // ── Phase 6: Pattern binding origins ──

    // ── Phase 6: Closure capture origin tracking ──

    #[test]
    fn closure_capture_source_moved() {
        // With StringView removed, `String v = s` is an owned copy (CoW), not a view.
        // v is independent of s, so moving s doesn't invalidate closure f.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    auto f = (): print(v)
    consume(!s)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn closure_return_captures_local() {
        // With StringView removed, `String v = local` is an owned copy (CoW).
        // The closure captures an owned value, so returning it is fine.
        let source = "\
Callable[void()] bad():
    String local = \"hello\" + \"\"
    String v = local
    return (): print(v)

void main():
    pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn closure_return_captures_param_ok() {
        // Returning a closure that captures a param → no error
        let source = "\
Callable[void()] ok(String v):
    return (): print(v)

void main():
    pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for closure capturing param: {:?}", errors
        );
    }

    #[test]
    fn closure_return_literal_ok() {
        // Returning a closure that captures a literal str → no error
        let source = "\
Callable[void()] ok():
    String v = \"hello\"
    return (): print(v)

void main():
    pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "unexpected DanglingReturn for closure capturing literal: {:?}", errors
        );
    }

    #[test]
    fn closure_no_ref_captures_ok() {
        // Closure with no ref-type captures → no false positive
        let source = "\
void main():
    int x = 42
    auto f = (): print(x)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for closure with no ref captures: {:?}", errors
        );
    }

    #[test]
    fn closure_capture_literal_ok() {
        // Closure capturing str from literal → Static origin
        let source = "\
void main():
    String v = \"hello\"
    auto f = (): print(v)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                    | SemanticErrorKind::DanglingReturn { .. }
            )),
            "unexpected error for closure capturing literal: {:?}", errors
        );
    }

    // ── Closure body scope isolation ──

    #[test]
    fn closure_body_move_does_not_leak_to_enclosing_scope() {
        // A move inside a closure body should not mark the variable as Moved
        // in the enclosing scope — the closure body executes on call, not at
        // definition.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    auto f = (): consume(!s)
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { .. })),
            "closure body move should not leak to enclosing scope: {:?}", errors
        );
    }

    #[test]
    fn closure_body_move_still_detected_inside_body() {
        // A double-move inside the closure body should still be caught.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    auto f = ():
        consume(!s)
        consume(!s)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { .. }
                | SemanticErrorKind::DoubleMove { .. })),
            "double move inside closure body should be caught: {:?}", errors
        );
    }

    #[test]
    fn closure_in_loop_body_no_false_move_in_loop() {
        // A move inside a closure body should not trigger MoveInLoop even if
        // the closure is defined inside a loop.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    for i in 0..3:
        auto f = (): consume(!s)
        print(\"ok\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "move inside closure body should not trigger MoveInLoop: {:?}", errors
        );
    }

    #[test]
    fn closure_definition_preserves_enclosing_origins() {
        // A closure that captures a ref-type variable should not change the
        // variable's origin in the enclosing scope.
        let source = "\
void main():
    String owner = \"hello\"
    String v = owner
    auto f = (): print(v)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterMove { .. }
                | SemanticErrorKind::UseAfterSourceMoved { .. })),
            "closure definition should not alter enclosing origins: {:?}", errors
        );
    }

    // ── Phase 6: Temporary borrow detection ──

    #[test]
    fn temporary_borrow_str_from_string_call() {
        // With str→String unification, `str v = make_string()` where make_string
        // returns owned → `v` becomes owned → no TemporaryBorrow.
        let source = "\
String make_string():
    String name = \"world\"
    return f\"hello {name}\"

void main():
    String v = make_string()
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected TemporaryBorrow — str v receives owned value safely: {:?}", errors
        );
    }

    #[test]
    fn no_temporary_borrow_str_from_str_call() {
        // str v = get_str() where get_str returns str → no error (returns ref type)
        let source = "\
String get_str(): \"hello\"

void main():
    String v = get_str()
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected TemporaryBorrow for str from str call: {:?}", errors
        );
    }

    #[test]
    fn no_temporary_borrow_owning_to_owning() {
        // String s = make_string() → no error (owning to owning)
        let source = "\
String make_string():
    return \"hello\"

void main():
    String s = make_string()
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected TemporaryBorrow for owning-to-owning: {:?}", errors
        );
    }

    #[test]
    fn match_pattern_binding_source_moved() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so moving s doesn't invalidate v.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    consume(!s)
    int x = 1
    match x:
        case 1:
            print(v)
        case 2:
            pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn match_pattern_binding_literal_ok() {
        // Scrutinee from literal → no error
        let source = "\
void main():
    String v = \"hello\"
    int x = 1
    match x:
        case 1:
            print(v)
        case 2:
            pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved for literal pattern binding: {:?}", errors
        );
    }

    #[test]
    fn branch_origin_merging_match() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so moving s doesn't invalidate v.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    int x = 1
    match x:
        case 1:
            consume(!s)
        case 2:
            pass
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn method_origin_borrows_from_self() {
        // Method returns str borrowing from self.field — no error when receiver is alive
        let source = "\
struct Holder:
    String name

equip Holder:
    String get_name(self):
        return self.name

void main():
    Holder h = Holder(\"hello\")
    String v = h.get_name()
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                | SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected error for method borrowing from alive receiver: {:?}", errors
        );
    }

    #[test]
    fn method_origin_use_after_source_moved() {
        // User-defined functions always return owned strings (IR clones on return),
        // so the value is independent — no dangling.
        let source = "\
struct Holder:
    String name

equip Holder:
    String get_name(self):
        return self.name

void consume(Holder !h):
    pass

void main():
    Holder h = Holder(\"hello\")
    String v = h.get_name()
    consume(!h)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — get_name() returns owned string, v is independent of h: {:?}", errors
        );
    }

    #[test]
    fn method_temporary_borrow() {
        // With str→String unification, `str v = b.build()` where build() returns
        // owned String → `v` becomes owned → no TemporaryBorrow.
        let source = "\
struct Builder:
    String data

equip Builder:
    String build(self):
        return !self.data

void main():
    Builder b = Builder(\"hello\")
    String v = b.build()
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::TemporaryBorrow { .. })),
            "unexpected TemporaryBorrow — str v receives owned value safely: {:?}", errors
        );
    }

    #[test]
    fn loop_origin_merging_use_after_move() {
        // Move source inside while body, use ref after loop → UseAfterSourceMoved
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\"
    String v = s
    int i = 0
    while i < 3:
        consume(!s)
        i = i + 1
    print(v)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                    if name == "v" && source_name == "s"))
            || has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { name } if name == "s")),
            "expected UseAfterSourceMoved or MoveInLoop after loop origin merge, got: {:?}", errors
        );
    }

    #[test]
    fn loop_origin_no_move_ok() {
        // No move in loop → no error
        let source = "\
void main():
    String s = \"hello\"
    String v = s
    int i = 0
    while i < 3:
        print(v)
        i = i + 1
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::UseAfterSourceMoved { .. }
                | SemanticErrorKind::MoveInLoop { .. })),
            "unexpected error for loop with no move: {:?}", errors
        );
    }

    // ── Phase 8: Origin completeness sweep ──

    #[test]
    fn match_expr_origin_use_after_move() {
        // With StringView removed, match expression returning s gives v an owned copy.
        // v is independent of s, so moving s doesn't invalidate v.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = match 1:
        case 1: s
        case 2: s
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy from match, not a view: {:?}", errors
        );
    }

    #[test]
    fn try_expr_origin_propagation() {
        // User-defined functions always return owned strings (IR clones on return),
        // so the value is independent — no dangling.
        let source = "\
String get_view(String s):
    return s

void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = get_view(s)
    consume(!s)
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "v" && source_name == "s")),
            "unexpected UseAfterSourceMoved — get_view() returns owned string, v is independent of s: {:?}", errors
        );
    }

    // ── Phase 10: Cross-function callable lifetime tracking ──

    #[test]
    fn cross_function_closure_source_moved() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so moving s doesn't invalidate closure f.
        let source = "\
Callable[void()] make_printer(String v):
    return (): print(v)

void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    auto f = make_printer(v)
    consume(!s)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn cross_function_closure_ok() {
        // Closure from function call — source not moved → no error
        let source = "\
Callable[void()] make_printer(String v):
    return (): print(v)

void main():
    String s = \"hello\"
    String v = s
    auto f = make_printer(v)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "expected no UseAfterSourceMoved, got: {:?}", errors
        );
    }

    #[test]
    fn closure_reassignment_tracks_origin() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so moving s doesn't invalidate closure f.
        let source = "\
void consume(String !s):
    pass

void main():
    String s = \"hello\" + \"\"
    String v = s
    auto f = (): print(\"\")
    f = (): print(v)
    consume(!s)
    f()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn closure_pattern_binding_origin() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so moving s doesn't invalidate closure c.
        let source = "\
void consume(String !s):
    pass

Callable[void()] make_printer(String v):
    return (): print(v)

void main():
    String s = \"hello\" + \"\"
    String v = s
    auto c = make_printer(v)
    int x = 1
    match x:
        case n:
            consume(!s)
            c()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    // ── Phase 11: Reassignment invalidation ──

    #[test]
    fn reassignment_invalidates_borrow() {
        // With StringView removed, `String v = s` is an owned copy (CoW).
        // v is independent of s, so reassigning s doesn't invalidate v.
        let source = "\
void main():
    String s = \"hello\" + \"\"
    String v = s
    s = \"world\" + \"\"
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved — v is an owned copy, not a view: {:?}", errors
        );
    }

    #[test]
    fn reassignment_new_borrow_ok() {
        // After reassignment, new borrows from the variable are fine
        let source = "\
void main():
    String s = \"hello\"
    s = \"world\"
    String v = s
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved error, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_reborrow_ok() {
        // If the dependent variable is itself reassigned, the stale entry is cleared
        let source = "\
void main():
    String s = \"hello\"
    String v = s
    s = \"world\"
    v = s
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved error after reborrow, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_copy_type_ok() {
        // Copy types (str) don't destroy the old value on reassignment
        let source = "\
void main():
    String s = \"hello\"
    String v = s
    s = \"world\"
    print(v)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected UseAfterSourceMoved for Copy type, got: {:?}", errors
        );
    }

    #[test]
    fn reassignment_transitive() {
        // User-defined functions always return owned strings (IR clones on return),
        // so the value is independent — no dangling.
        let source = "\
String identity(String x):
    return x

void main():
    String s = \"hello\" + \"\"
    String v = s
    String w = identity(v)
    s = \"world\" + \"\"
    print(w)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. }
                if name == "w" && source_name == "s")),
            "unexpected UseAfterSourceMoved — identity() returns owned string, w is independent of s: {:?}", errors
        );
    }

    // ── Constructor implicit move tests ──

    #[test]
    fn struct_constructor_implicit_move() {
        // CoW rule (b): `Wrapper(s)` followed by `print(s)` is sound — `s` is
        // live past the construction, so the lowering CLONES into the struct
        // field (it does not move). The `print(s)` reads the still-valid
        // source. (ASan-verified clean: clone emitted at the init site.)
        let source = "\
struct Wrapper:
    String value

void main():
    String s = \"hello\" + \"\"
    Wrapper w = Wrapper(s)
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { .. })),
            "rule (b): Wrapper(s) clones a live s; print(s) is sound, got: {:?}", errors
        );
    }

    #[test]
    fn struct_constructor_copy_args_ok() {
        let source = "\
struct Point:
    int x
    int y

void main():
    int a = 1
    int b = 2
    Point p = Point(a, b)
    print(\"{a}\")
    print(\"{b}\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
                    | SemanticErrorKind::DoubleMove { .. }
            )),
            "unexpected move errors for Copy-type constructor args: {:?}", errors
        );
    }

    #[test]
    fn constructor_single_owner_requires_explicit_move() {
        // CoW rule (b) CARVE-OUT: single-owner types (closures/Callable, Box,
        // Owned, Task/...) are NOT CoW-eligible — there is no clone-if-live path
        // in the lowering. Passing one BARE into a constructor must be REJECTED
        // with MoveWithoutOperator (forcing explicit `!`), NOT accepted and then
        // panic the IR lowering as an untracked consumed source. Regression test
        // for the (b) relaxation's over-relaxation (a closure was un-guarded).
        let source = "\
struct Holder:
    int(int) f

void main():
    int(int) g = (int x): x * 2
    Holder h = Holder(g)
    print(\"{h.f(3)}\")
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveWithoutOperator { name, .. }
                if name == "g")),
            "expected MoveWithoutOperator for a bare closure into a constructor, got: {:?}", errors
        );
    }

    #[test]
    fn constructor_single_owner_explicit_move_ok() {
        // The explicit `!` form of the carve-out is accepted by the checker (the
        // guard only fires on a bare `Expr::Identifier`, never on `!g`).
        let source = "\
struct Holder:
    int(int) f

void main():
    int(int) g = (int x): x * 2
    Holder h = Holder(!g)
    print(\"{h.f(3)}\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveWithoutOperator { .. })),
            "explicit !g into a constructor should be accepted by the checker, got: {:?}", errors
        );
    }

    #[test]
    fn struct_constructor_double_move() {
        // CoW rule (b): `Pair(s, s)` is sound, NOT a double move. The first `s`
        // is live (used again as the second arg), so the lowering CLONES it
        // into field `a`; the second `s` is the last use, so it MOVES into
        // field `b`. Two independent values, no double-free. (ASan-verified
        // clean with a `Pair(s, s)` + print(p.a)/print(p.b) fixture: 1 clone,
        // correct distinct output.)
        let source = "\
struct Pair:
    String a
    String b

void main():
    String s = \"hello\" + \"\"
    Pair p = Pair(s, s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DoubleMove { .. }
                | SemanticErrorKind::UseAfterMove { .. })),
            "rule (b): Pair(s, s) clones the live first s and moves the last-use second s; sound, got: {:?}", errors
        );
    }

    #[test]
    fn variant_constructor_implicit_move() {
        // With CoW, enum constructors borrow string arguments (no move).
        // The IR creates a Ptr alias, not a consume. s is still usable after.
        let source = "\
enum Container:
    Holding(String)
    Empty

void main():
    String s = \"hello\" + \"\"
    Container c = Container.Holding(s)
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterMove { .. })),
            "string args to enum constructors are borrowed (CoW), not moved: {:?}", errors
        );
    }

    #[test]
    fn struct_constructor_param_not_moved() {
        // CoW rule (b): `Wrapper(s)` where `s` is a by-value param, followed by
        // `print(s)`, is sound — `s` is live past the construction, so the
        // lowering CLONES it into the struct field (a by-value param is a
        // borrow at the IR level and is always cloned at the owning boundary).
        // `print(s)` reads the still-valid param.
        let source = "\
struct Wrapper:
    String value

void wrap(String s):
    Wrapper w = Wrapper(s)
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UseAfterMove { .. }
            )),
            "rule (b): Wrapper(s) clones the live param s; print(s) is sound, got: {:?}", errors
        );
    }

    #[test]
    fn constructor_move_in_loop_not_return() {
        // With CoW, string args to enum constructors are borrowed, not consumed.
        // Each loop iteration borrows s — no MoveInLoop.
        let source = "\
enum Wrapper:
    Value(String)

void main():
    String s = \"hello\" + \"\"
    for i in 0..3:
        auto w = Wrapper.Value(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "string args to enum constructors are borrowed (CoW), not moved: {:?}", errors
        );
    }

    #[test]
    fn constructor_move_in_loop_return() {
        // `return Wrapper.Value(label)` in a loop should be fine — return exits the function
        let source = "\
enum Wrapper:
    Value(String)

Wrapper find(Vector[String] items):
    for item in items:
        String label = item
        return Wrapper.Value(label)
    return Wrapper.Value(\"default\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "unexpected MoveInLoop for return-position constructor: {:?}", errors
        );
    }

    #[test]
    fn struct_literal_move_in_loop_not_return() {
        // CoW rule (b): `Container(s)` in a loop, where `s` is declared OUTSIDE
        // the loop, is sound — `s` is live across iterations (each iteration
        // reads the same source), so the lowering CLONES it into the struct on
        // every iteration (`clone_multi_use_resource_args` treats a non-loop-
        // local named source as multi-use). No MoveInLoop: the source is never
        // consumed by the construction.
        let source = "\
struct Container:
    String value

void main():
    String s = \"hello\" + \"\"
    for i in 0..3:
        auto b = Container(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "rule (b): Container(s) clones the loop-carried s each iteration; no MoveInLoop, got: {:?}", errors
        );
    }

    #[test]
    fn nested_constructor_in_return() {
        // `return Outer.Wrap(Inner.Val(s))` — nested constructors in return should be fine
        let source = "\
enum Inner:
    Val(String)

enum Outer:
    Wrap(Inner)

Outer find(Vector[String] items):
    for item in items:
        String s = item
        return Outer.Wrap(Inner.Val(s))
    return Outer.Wrap(Inner.Val(\"default\"))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::MoveInLoop { .. })),
            "unexpected MoveInLoop for nested return-position constructor: {:?}", errors
        );
    }

    // ─── Pass 5a: Local Variable Alias Tracing ───────────────

    #[test]
    fn return_through_local_two_ref_params() {
        // `str pick(String a, String b)` with local alias — Pass 5a should trace `result` back to `a`
        let source = "\
String pick(String a, String b):
    String result = a
    return result

void main():
    String x = \"hello\"
    String y = \"world\"
    String r = pick(x, y)
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for return through local alias: {:?}", errors
        );
    }

    #[test]
    fn return_through_local_use_after_move() {
        // User-defined functions always return owned strings (IR clones on return),
        // so the value is independent — no dangling.
        let source = "\
String id(String x):
    return x

String pick(String a, String b):
    String result = a
    return result

void main():
    String s = \"hello\" + \"\"
    String r = pick(s, \"world\")
    String s2 = !s
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { name, .. } if name == "r")),
            "unexpected UseAfterSourceMoved — pick() returns owned string, r is independent of s: {:?}", errors
        );
    }

    #[test]
    fn return_through_local_branch_union() {
        // result assigned from a or b depending on branch — both should flow
        let source = "\
String pick(String a, String b, bool flag):
    String result = a
    if flag:
        result = b
    return result

void main():
    String x = \"hello\"
    String y = \"world\"
    String r = pick(x, y, true)
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for branch union alias: {:?}", errors
        );
    }

    #[test]
    fn return_through_transitive_alias() {
        // `str x = a; str y = x; return y` — transitive chain should resolve
        let source = "\
String chain(String a, String b):
    String x = a
    String y = x
    return y

void main():
    String s = \"hello\"
    String r = chain(s, \"world\")
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for transitive alias chain: {:?}", errors
        );
    }

    #[test]
    fn return_through_call() {
        // `return id(a)` where id has return_borrows_from = [0] — trace through call
        let source = "\
String id(String x):
    return x

String wrapper(String a, String b):
    return id(a)

void main():
    String s = \"hello\"
    String r = wrapper(s, \"world\")
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for return through call: {:?}", errors
        );
    }

    #[test]
    fn return_local_assigned_from_call() {
        // `str result = id(a); return result` — alias from call result
        let source = "\
String id(String x):
    return x

String wrapper(String a, String b):
    String result = id(a)
    return result

void main():
    String s = \"hello\"
    String r = wrapper(s, \"world\")
    print(r)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UseAfterSourceMoved { .. })),
            "unexpected error for local assigned from call: {:?}", errors
        );
    }

    #[test]
    fn unknown_origin_return_rejected() {
        // Bodyless function returning str with multiple ref params.
        // After provenance fix: bodyless functions are conservatively treated as
        // returning owned (not view), since we can't analyze the body. This avoids
        // incorrect view downgrades for stdlib functions like regex_escape/path_join
        // that take borrowed params but return owned strings.
        let source = "\
String get_data(String a, String b)

String wrapper(String x, String y):
    String s = get_data(x, y)
    return s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::UnresolvedBorrowOrigin { .. })),
            "bodyless functions return owned (conservative default), got: {:?}", errors
        );
    }

    #[test]
    fn static_origin_return_ok() {
        // Function with body returning a string literal — origin is Static.
        let source = "\
String greet():
    return \"hello\"

String wrapper():
    String s = greet()
    return s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DanglingReturn { .. }
                    | SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "unexpected error for static return: {:?}", errors
        );
    }

    #[test]
    fn unknown_closure_capture_rejected() {
        // Bodyless function returning str → conservatively treated as owned.
        // Closure captures owned result — no borrow origin issue.
        let source = "\
String get_data(String a, String b)

Callable[String()] wrapper(String x, String y):
    String s = get_data(x, y)
    return (): s
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DanglingReturn { .. }
                    | SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "bodyless functions return owned (conservative default), got: {:?}", errors
        );
    }

    #[test]
    fn merge_unknown_with_static() {
        // If/else with Static and bodyless-function-call branches.
        // Bodyless function returns owned (conservative) → both branches are safe.
        let source = "\
String get_data(String a, String b)

String pick(bool cond, String x, String y):
    if cond:
        return \"hello\"
    return get_data(x, y)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "bodyless functions return owned (conservative default), got: {:?}", errors
        );
    }

    #[test]
    fn owned_return_from_bodyless_fn_ok() {
        // Bodyless function returning an owned type — caller should be fine
        // returning coerced result, since owned data is always Static.
        let source = "\
String make_string()

String wrapper():
    return make_string()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::DanglingReturn { .. }
                    | SemanticErrorKind::UnresolvedBorrowOrigin { .. }
            )),
            "unexpected error for owned return coercion: {:?}", errors
        );
    }

    // ─── Async/Await Borrow-Across-Await Tests ──────────────

    #[test]
    fn param_str_across_await_ok() {
        // str param used after await → now OK: caller is blocked, param stays alive.
        // The soundness argument USED to lean on spawn enforcement
        // (SpawnWithBorrowedRef) keeping borrowed refs out of fire-and-forget
        // spawns. That guard has no positive control — no test asserts it FIRES
        // and no ordinary borrow shape has been observed to trip it — so do not
        // rely on it here. Filed in TODO.md (safety / spawn).
        let source = "\
async int do_work():
    return 1

async void process(String name):
    do_work().await()
    print(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for param str: {:?}", errors
        );
    }

    #[test]
    fn local_str_from_param_across_await_ok() {
        // Local str derived from a str param (via function call) is also safe across await:
        // its origin traces back to the param, which stays alive while caller is blocked.
        let source = "\
String get_slice(String input):
    return input

async int do_work():
    return 1

async void process(String data):
    String s = get_slice(data)
    do_work().await()
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for local from param: {:?}", errors
        );
    }

    #[test]
    fn local_str_across_await_still_rejected() {
        // With StringView removed, .as_str() returns an owned String.
        // Owned values are safe across await points — no borrow issue.
        let source = "\
String get_slice(String input):
    return input

async int do_work():
    return 1

async void process():
    String owned = String.from(\"hello\")
    String s = owned.as_str()
    do_work().await()
    print(s)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait — s is owned String, not a view: {:?}", errors
        );
    }

    #[test]
    fn owned_across_await_ok() {
        // int (Copy) used after await → no error
        let source = "\
async int do_work():
    return 1

async int compute():
    int x = 42
    do_work().await()
    return x
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for Copy type: {:?}", errors
        );
    }

    #[test]
    fn static_str_across_await_ok() {
        // str from literal used after await → no error (Static origin)
        let source = "\
async int do_work():
    return 1

async void greet():
    String msg = \"hello\"
    do_work().await()
    print(msg)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for static str: {:?}", errors
        );
    }

    #[test]
    fn borrow_used_before_await_only_ok() {
        // str param used before await, not after → no error
        let source = "\
async int do_work():
    return 1

async void process(String name):
    print(name)
    do_work().await()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for use-before-await: {:?}", errors
        );
    }

    #[test]
    fn reassigned_after_await_ok() {
        // str param reassigned after await, then used → no error
        let source = "\
async int do_work():
    return 1

async void process(String name):
    do_work().await()
    name = \"fresh\"
    print(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for reassigned-after-await: {:?}", errors
        );
    }

    #[test]
    fn borrow_across_await_in_branch() {
        // await in one if branch, param used after merge → now OK (param-only origin)
        let source = "\
async int do_work():
    return 1

async void process(String name, bool cond):
    if cond:
        do_work().await()
    print(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for param-origin branch-await, got: {:?}", errors
        );
    }

    #[test]
    fn spawn_is_not_suspension_point() {
        // str param used after spawn → no error (spawn doesn't suspend)
        let source = "\
async int do_work():
    return 1

async void process(String name):
    auto task = spawn do_work()
    print(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::BorrowAcrossAwait { .. })),
            "unexpected BorrowAcrossAwait for spawn: {:?}", errors
        );
    }

    // ─── Spawn-site borrow enforcement ──────────────────────

    #[test]
    fn spawn_with_owned_string_param_accepted() {
        // With StringView removed, String params are owned, not borrowed.
        // Passing an owned String to a spawned task is fine.
        let source = "\
async void worker(String name):
    print(name)

void launch(String name):
    auto t = spawn worker(name)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::SpawnWithBorrowedRef { .. })),
            "unexpected SpawnWithBorrowedRef — String is owned, not borrowed: {:?}", errors
        );
    }

    #[test]
    fn spawn_with_static_str_ok() {
        // passing a string literal to a spawned task → OK (Static origin)
        let source = "\
async void worker(String name):
    print(name)

void launch():
    auto t = spawn worker(\"hello\")
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::SpawnWithBorrowedRef { .. })),
            "unexpected SpawnWithBorrowedRef for static str: {:?}", errors
        );
    }

    #[test]
    fn spawn_with_call_returning_owned_str_accepted() {
        // User-defined functions always return owned strings (IR clones on return),
        // so the value is independent — no dangling.
        let source = "\
String get_slice(String s):
    return s

async void worker(String name):
    print(name)

void launch(String data):
    auto t = spawn worker(get_slice(data))
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::SpawnWithBorrowedRef { .. })),
            "unexpected SpawnWithBorrowedRef — get_slice() returns owned string, safe for spawn: {:?}", errors
        );
    }

    #[test]
    fn spawn_method_call_ok() {
        // spawn obj.method() is now supported
        let source = "\
struct Runner:
    int id

equip Runner:
    void run(self):
        print(self.id)

void launch():
    Runner r = Runner(1)
    auto t = spawn r.run()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::SpawnRequiresDirectCall)),
            "expected no SpawnRequiresDirectCall for method spawn: {:?}", errors
        );
    }

    #[test]
    fn spawn_closure_call_no_captures_ok() {
        // spawn closure() with no captures → allowed (no borrowed state)
        let source = "\
void launch():
    auto c = (): print(42)
    auto t = spawn c()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnRequiresDirectCall
                | SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "expected no spawn error for capture-free closure: {:?}", errors
        );
    }

    // ─── Closure capture set + spawn tests ────────────────────

    #[test]
    fn spawn_closure_inline_no_captures_ok() {
        // spawn ((): body)() with no captures — allowed
        let source = "\
void launch():
    auto t = spawn ((): print(42))()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnRequiresDirectCall
                | SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "expected no spawn error for inline closure with no captures: {:?}", errors
        );
    }

    #[test]
    fn spawn_closure_copy_capture_ok() {
        // Closure captures an int (Copy type) — allowed
        let source = "\
void launch():
    int x = 5
    auto t = spawn ((): print(x))()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnRequiresDirectCall
                | SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "expected no spawn error for closure capturing Copy int: {:?}", errors
        );
    }

    #[test]
    fn spawn_closure_str_capture_rejected() {
        // With StringView removed, String params are owned, not borrowed.
        // Capturing an owned String in a spawned closure is fine.
        let source = "\
void launch(String name):
    auto t = spawn ((): print(name))()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "unexpected SpawnClosureCaptureBorrowed — String is owned, not borrowed: {:?}", errors
        );
    }

    #[test]
    fn spawn_closure_var_copy_ok() {
        // Closure variable capturing int — allowed
        let source = "\
void launch():
    int x = 5
    auto c = (): print(x)
    auto t = spawn c()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnRequiresDirectCall
                | SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "expected no spawn error for closure var with Copy capture: {:?}", errors
        );
    }

    #[test]
    fn spawn_closure_var_str_rejected() {
        // With StringView removed, String params are owned, not borrowed.
        // Capturing an owned String in a spawned closure variable is fine.
        let source = "\
void launch(String name):
    auto c = (): print(name)
    auto t = spawn c()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "unexpected SpawnClosureCaptureBorrowed — String is owned, not borrowed: {:?}", errors
        );
    }

    #[test]
    fn spawn_direct_fn_still_works() {
        // Direct function call — the existing path must still work
        let source = "\
async void work(int n):
    print(n)
void launch():
    auto t = spawn work(42)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnRequiresDirectCall
                | SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
                | SemanticErrorKind::SpawnWithBorrowedRef { .. }
            )),
            "expected no spawn error for direct function call: {:?}", errors
        );
    }

    #[test]
    fn spawn_method_with_async_ok() {
        // Method calls (including async) are now supported for spawn
        let source = "\
struct Worker:
    int id
equip Worker:
    async void run(self):
        print(self.id)
void launch():
    Worker w = Worker(1)
    auto t = spawn w.run()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::SpawnRequiresDirectCall)),
            "expected no SpawnRequiresDirectCall for method spawn: {:?}", errors
        );
    }

    #[test]
    fn capture_set_read_vs_mutable() {
        // Verify a mutated capture is classified as Mutable, read-only as Read
        let source = "\
void launch():
    int x = 1
    int y = 2
    auto c = ():
        x = x + 1
        print(y)
    c()
";
        let errors = check(source);
        // This test just verifies no crash — the classification is internal.
        // Mutable captures of Copy types should not cause spawn errors.
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "Copy captures (even mutable) should not be rejected: {:?}", errors
        );
    }

    // ─── Named scope borrow checker tests ───────────────────

    #[test]
    fn named_scope_outer_borrow_ok() {
        // variable declared outside the named scope, borrowed inside → fine
        let source = "\
void process(String data):
    workers:
        print(data)
";
        let errors = check(source);
        assert!(
            errors.is_empty(),
            "unexpected errors for outer borrow in named scope: {:?}", errors
        );
    }

    #[test]
    fn named_scope_basic_parse_ok() {
        // named scope parses and type-checks without error
        let source = "\
void main():
    int x = 5
    section:
        int y = x + 1
        print(y)
    print(x)
";
        let errors = check(source);
        assert!(
            errors.is_empty(),
            "unexpected errors for basic named scope: {:?}", errors
        );
    }

    // ─── MutCallable aliasing enforcement (Feature B) ─────────────────────

    #[test]
    fn read_while_mut_captured_rejected() {
        // Reading x while it is mutably captured by live closure c → error.
        // Block-bodied closure: ():↵    x = 1
        let source = "void main():\n    int x = 0\n    auto c = ():\n        x = 1\n    print(x)\n";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::ReadWhileMutCaptured { .. })),
            "expected ReadWhileMutCaptured: {:?}", errors
        );
    }

    #[test]
    fn write_while_mut_captured_rejected() {
        // Writing x while it is mutably captured by live closure c → error.
        let source = "void main():\n    int x = 0\n    auto c = ():\n        x = 1\n    x = 2\n";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::WriteWhileMutCaptured { .. })),
            "expected WriteWhileMutCaptured: {:?}", errors
        );
    }

    #[test]
    fn read_only_capture_ok() {
        // Closure reads x but doesn't mutate it → no aliasing error.
        let source = "\
void main():
    int x = 0
    auto c = (): print(x)
    print(x)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::ReadWhileMutCaptured { .. }
                | SemanticErrorKind::WriteWhileMutCaptured { .. }
            )),
            "expected no aliasing error for read-only capture: {:?}", errors
        );
    }

    #[test]
    fn mut_capture_released_after_move() {
        // After the closure is moved (!c), the captured variable can be read again.
        // We define a sink function to accept the moved closure.
        let source = "void sink(auto f):\n    print(\"done\")\nvoid main():\n    int x = 0\n    auto c = ():\n        x = 1\n    sink(!c)\n    print(x)\n";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::ReadWhileMutCaptured { .. }
                | SemanticErrorKind::WriteWhileMutCaptured { .. }
            )),
            "expected no aliasing error after closure moved: {:?}", errors
        );
    }

    #[test]
    fn mut_capture_in_named_scope_released_at_block_exit() {
        // Closure declared inside a named scope block goes out of scope at block exit;
        // the captured variable should be readable after the block.
        let source = "void main():\n    int x = 0\n    inner:\n        auto c = ():\n            x = 1\n        c()\n    print(x)\n";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::ReadWhileMutCaptured { .. }
                | SemanticErrorKind::WriteWhileMutCaptured { .. }
            )),
            "expected no aliasing error after block exit: {:?}", errors
        );
    }

    // ─── Soundness fixes: spawn mutable capture rejection ─────────────────

    #[test]
    fn spawn_closure_with_mutable_capture_rejected() {
        // A closure that mutably captures x cannot be spawned — it stores a
        // pointer to the parent stack frame.
        let source = "\
async void main():
    int x = 0
    auto c = ():
        x = 1
    Task[void] t = spawn c()
    t.await()
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::SpawnClosureCaptureMutable { .. })),
            "expected SpawnClosureCaptureMutable: {:?}", errors
        );
    }

    #[test]
    fn spawn_closure_with_readonly_capture_ok() {
        // A closure that only reads x can be safely spawned (ByValue copy).
        let source = "\
async void main():
    int x = 42
    auto c = (): print(x)
    Task[void] t = spawn c()
    t.await()
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::SpawnClosureCaptureMutable { .. }
                | SemanticErrorKind::SpawnClosureCaptureBorrowed { .. }
            )),
            "expected no spawn capture errors: {:?}", errors
        );
    }

    // ─── Soundness fixes: multiple closures on same variable ──────────────

    #[test]
    fn two_mut_closures_same_var_rejected() {
        // Two closures both capturing x mutably is a conflict — the second
        // closure definition (inside its body walk) triggers WriteWhileMutCaptured
        // because c1 already holds x.
        let source = "\
void main():
    int x = 0
    auto c1 = ():
        x = 1
    auto c2 = ():
        x = 2
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k, SemanticErrorKind::WriteWhileMutCaptured { .. })),
            "expected WriteWhileMutCaptured for second closure: {:?}", errors
        );
    }

    #[test]
    fn sequential_mut_closures_in_scopes_ok() {
        // After c1 goes out of scope, defining c2 with mutable capture of x is fine.
        let source = "\
void main():
    int x = 0
    s1:
        auto c1 = ():
            x = 1
        c1()
    s2:
        auto c2 = ():
            x = 2
        c2()
    print(x)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::ReadWhileMutCaptured { .. }
                | SemanticErrorKind::WriteWhileMutCaptured { .. }
            )),
            "expected no aliasing error for sequential closures in scopes: {:?}", errors
        );
    }

    // ─── Soundness fixes: closure reassignment ───────────────────────────

    #[test]
    fn closure_reassignment_releases_old_locks() {
        // Reassigning a closure variable to a non-mutating closure should
        // release the old mutable capture lock.
        let source = "\
void main():
    int x = 0
    auto c = ():
        x = 1
    c = (): print(x)
    print(x)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::ReadWhileMutCaptured { .. }
                | SemanticErrorKind::WriteWhileMutCaptured { .. }
            )),
            "expected no aliasing error after closure reassignment: {:?}", errors
        );
    }

    fn check_warnings(source: &str) -> Vec<crate::semantic::errors::SemanticWarning> {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);
        let result = semantic::analyze(&mut module, &[]);
        result.warnings
    }

    fn check_warnings_with_const(source: &str) -> Vec<crate::semantic::errors::SemanticWarning> {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "parse errors: {:?}", parser.errors);
        let result = semantic::analyze_with_source_dir(&mut module, &[], None, true);
        result.warnings
    }

    fn has_warning(warnings: &[crate::semantic::errors::SemanticWarning], pred: impl Fn(&crate::semantic::errors::SemanticWarningKind) -> bool) -> bool {
        warnings.iter().any(|w| pred(&w.kind))
    }

    #[test]
    fn stale_shared_condition_warns() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    int val = x
    t.await()
    if val > 0:
        print(val)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::StaleSharedCondition { local_name, shared_name, .. }
                if local_name == "val" && shared_name == "x"
            )),
            "expected StaleSharedCondition warning, got: {:?}", warnings
        );
    }

    #[test]
    fn stale_shared_condition_refreshed_no_warn() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    int val = x
    t.await()
    val = x
    if val > 0:
        print(val)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::StaleSharedCondition { .. }
            )),
            "expected no StaleSharedCondition after refresh, got: {:?}", warnings
        );
    }

    #[test]
    fn with_check_then_act_warns_if_yield_inside_guarded_branch() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        if x > 0:
            sleep(100)
            print(x)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { shared_names, .. }
                if shared_names.contains(&"x".to_string())
            )),
            "expected WithCheckThenAct warning, got: {:?}", warnings
        );
    }

    #[test]
    fn with_check_then_act_no_warn_without_yield() {
        // No yield inside the branch — no check-then-act race
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        if x > 0:
            print(x)
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { .. }
            )),
            "expected no WithCheckThenAct warning, got: {:?}", warnings
        );
    }

    #[test]
    fn with_check_then_act_warns_blocking_call() {
        // Blocking call (read_file) inside a with-guarded branch
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        if x > 0:
            String data = read_file(\"test.txt\")
            print(data)
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { shared_names, .. }
                if shared_names.contains(&"x".to_string())
            )),
            "expected WithCheckThenAct warning for blocking call, got: {:?}", warnings
        );
    }

    #[test]
    fn with_check_then_act_warns_while_loop() {
        // Yield inside a while loop guarded by with-tracked variable
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        while x > 0:
            sleep(50)
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { shared_names, .. }
                if shared_names.contains(&"x".to_string())
            )),
            "expected WithCheckThenAct warning for while loop, got: {:?}", warnings
        );
    }

    // ─── §3.5b Multi-Variable Invariant Tests ─────────────────

    #[test]
    fn with_check_then_act_names_both_shared_vars() {
        let source = "\
async void worker(int &a, int &b):
    a = a + 1
    b = b + 1

async void main():
    shared int x = 0
    shared int y = 0
    Task[void] t = spawn worker(&x, &y)
    with x, y:
        if x > 0 and y < 100:
            sleep(50)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { shared_names, .. }
                if shared_names.contains(&"x".to_string()) && shared_names.contains(&"y".to_string())
            )),
            "expected WithCheckThenAct naming both x and y, got: {:?}", warnings
        );
    }

    // ─── §3.8 Compound Yield Race Tests ─────────────────────────

    #[test]
    fn compound_yield_race_warns() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        x = x + sleep(1)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CompoundYieldRace { shared_name, .. }
                if shared_name == "x"
            )),
            "expected CompoundYieldRace warning, got: {:?}", warnings
        );
    }

    #[test]
    fn compound_yield_race_no_warn_without_yield() {
        // No yield in RHS — no race
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        x = x + 1
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CompoundYieldRace { .. }
            )),
            "expected no CompoundYieldRace warning, got: {:?}", warnings
        );
    }

    // ─── §3.9 Closure Captures With Binding Tests ───────────────

    #[test]
    fn closure_captures_with_binding_warns() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        auto f = ((): x > 0)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::ClosureCapturesWithBinding { var_name, .. }
                if var_name == "x"
            )),
            "expected ClosureCapturesWithBinding warning, got: {:?}", warnings
        );
    }

    #[test]
    fn closure_captures_with_binding_no_warn_outside_with() {
        // Closure captures shared var but NOT inside a `with` block
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    int local = 42
    auto f = ((): local > 0)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::ClosureCapturesWithBinding { .. }
            )),
            "expected no ClosureCapturesWithBinding warning, got: {:?}", warnings
        );
    }

    // ─── Phase 2: Generic Container Origin Tracking ───────────

    #[test]
    fn result_str_param_no_dangling_return() {
        // Result[String, String] param: unwrap() returns str with Param origin — should be safe
        let source = "\
enum Result[T, E]:
    Ok(T)
    Error(E)

String unwrap_ok(Result[String, String] r):
    match r:
        case Ok(s):
            return s
        else:
            return \"error\"
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "expected no DanglingReturn for Result[String, String] param, got: {:?}", errors
        );
    }

    #[test]
    fn option_str_param_no_dangling_return() {
        // Option[String] param: unwrap yields str with Param origin — should be safe
        let source = "\
enum Option[T]:
    Some(T)
    None

String unwrap_opt(Option[String] o):
    match o:
        case Some(s):
            return s
        else:
            return \"none\"
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::DanglingReturn { .. })),
            "expected no DanglingReturn for Option[String] param, got: {:?}", errors
        );
    }

    // ─── §3.6 Stale Write-Back Tests ─────────────────────────

    #[test]
    fn stale_writeback_assign_warns() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    int val = x
    t.await()
    x = val
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::StaleSharedWriteBack { local_name, target_shared_name, .. }
                if local_name == "val" && target_shared_name == "x"
            )),
            "expected StaleSharedWriteBack warning for assign, got: {:?}", warnings
        );
    }

    #[test]
    fn stale_writeback_compound_assign_warns() {
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    int delta = x
    t.await()
    x += delta
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::StaleSharedWriteBack { local_name, target_shared_name, .. }
                if local_name == "delta" && target_shared_name == "x"
            )),
            "expected StaleSharedWriteBack warning for compound assign, got: {:?}", warnings
        );
    }

    #[test]
    fn stale_writeback_no_yield_no_warn() {
        // No yield between derivation and write-back — no warning
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    int val = x
    x = val + 1
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::StaleSharedWriteBack { .. }
            )),
            "expected no StaleSharedWriteBack without yield, got: {:?}", warnings
        );
    }

    // ─── §3.7 Iterator Invalidation Tests ────────────────────

    #[test]
    fn shared_iteration_with_yield_warns() {
        let source = "\
async void worker(Vector[int] &items):
    items.push(99)

async void main():
    shared Vector[int] items = Vector[int]()
    Task[void] t = spawn worker(&items)
    with items:
        for item in items:
            sleep(50)
            print(item)
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::SharedIteratorInvalidation { shared_name, .. }
                if shared_name == "items"
            )),
            "expected SharedIteratorInvalidation warning, got: {:?}", warnings
        );
    }

    #[test]
    fn shared_iteration_without_yield_no_warn() {
        let source = "\
async void worker(Vector[int] &items):
    items.push(99)

async void main():
    shared Vector[int] items = Vector[int]()
    Task[void] t = spawn worker(&items)
    with items:
        for item in items:
            print(item)
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::SharedIteratorInvalidation { .. }
            )),
            "expected no SharedIteratorInvalidation without yield, got: {:?}", warnings
        );
    }

    // ─── §3.8 Spawn Inside With Tests ────────────────────────

    #[test]
    fn spawn_with_tracked_binding_warns() {
        let source = "\
async void other(int val):
    print(val)

async void main():
    shared int x = 0
    with x:
        spawn other(x)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::SpawnWithTrackedBinding { shared_name, .. }
                if shared_name == "x"
            )),
            "expected SpawnWithTrackedBinding warning, got: {:?}", warnings
        );
    }

    #[test]
    fn spawn_outside_with_no_warn() {
        let source = "\
async void other(int val):
    print(val)

async void main():
    shared int x = 0
    int copy = x
    spawn other(copy)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::SpawnWithTrackedBinding { .. }
            )),
            "expected no SpawnWithTrackedBinding outside with, got: {:?}", warnings
        );
    }

    // ─── Phase 1: Purity Integration Tests ───────────────────

    #[test]
    fn pure_call_in_with_block_no_warning() {
        // A pure function call inside a `with` block should NOT trigger a warning
        let source = "\
int double(int x): x * 2

async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        if x > 0:
            int y = double(x)
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { .. }
            )),
            "expected no WithCheckThenAct warning for pure call, got: {:?}", warnings
        );
    }

    #[test]
    fn blocking_call_in_with_block_warns() {
        // A blocking call inside a `with` block SHOULD trigger a warning
        let source = "\
async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        if x > 0:
            String data = read_file(\"test.txt\")
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { .. }
            )),
            "expected WithCheckThenAct warning for blocking call, got: {:?}", warnings
        );
    }

    #[test]
    fn impure_user_fn_in_with_block_warns() {
        // A user-defined function with side effects inside a `with` block
        // should trigger a warning (it calls write_file which is HasSideEffects)
        let source = "\
void save(String data):
    write_file(\"out.txt\", data)

async void worker(int &counter):
    counter = counter + 1

async void main():
    shared int x = 0
    Task[void] t = spawn worker(&x)
    with x:
        if x > 0:
            save(\"data\")
    t.await()
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::WithCheckThenAct { .. }
            )),
            "expected WithCheckThenAct warning for impure user function, got: {:?}", warnings
        );
    }

    // ─── Phase 2: Unreachable Code Tests ─────────────────────

    #[test]
    fn unreachable_after_return() {
        let source = "\
void main():
    return
    int x = 5
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnreachableCode
            )),
            "expected UnreachableCode warning after return, got: {:?}", warnings
        );
    }

    #[test]
    fn unreachable_after_break() {
        let source = "\
void main():
    for i in 0..10:
        break
        int x = 5
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnreachableCode
            )),
            "expected UnreachableCode warning after break, got: {:?}", warnings
        );
    }

    #[test]
    fn unreachable_after_continue() {
        let source = "\
void main():
    for i in 0..10:
        continue
        int x = 5
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnreachableCode
            )),
            "expected UnreachableCode warning after continue, got: {:?}", warnings
        );
    }

    #[test]
    fn no_unreachable_after_conditional_return() {
        let source = "\
void main():
    int x = 5
    if x > 0:
        return
    int y = 10
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnreachableCode
            )),
            "expected no UnreachableCode after conditional return, got: {:?}", warnings
        );
    }

    #[test]
    fn no_unreachable_in_test_after_function_with_return() {
        // Regression for the cosmetic bug reported 2026-05-05: `Item::Test` /
        // `Item::Bench` / `Item::SuiteSetup` / `Item::SuiteTeardown` reset
        // most BorrowChecker fields between items but forgot `diverged`. After
        // a previous function ended with `return`, the test body's first
        // statement was flagged unreachable. Fix in `mod.rs` routes the
        // resets through `reset_per_function_state` which clears `diverged`.
        let source = "\
int compute(): 42

test \"non-diverging\":
    int x = compute()
    assert x == 42
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnreachableCode
            )),
            "expected no UnreachableCode for the test body's first stmt, got: {:?}",
            warnings
        );
    }

    // ─── Phase 3: Unused Variable Tests ──────────────────────

    #[test]
    fn unused_local_warns() {
        let source = "\
void main():
    int x = 5
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnusedVariable { name, .. }
                if name == "x"
            )),
            "expected UnusedVariable warning for x, got: {:?}", warnings
        );
    }

    #[test]
    fn used_local_no_warning() {
        let source = "\
void main():
    int x = 5
    print(x)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnusedVariable { .. }
            )),
            "expected no UnusedVariable warning, got: {:?}", warnings
        );
    }

    #[test]
    fn fstring_complex_expr_no_unused_warn() {
        // Variables used in f-string complex expressions (e.g., f"{obj.method()}")
        // should be marked as used even though the interpolation text is "obj.method()".
        let source = "\
Vector[int] get_items(): [1, 2, 3]

void main():
    Vector[int] items = get_items()
    print(f\"{items.len()} items\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnusedVariable { name, .. }
                if name == "items"
            )),
            "expected no UnusedVariable for items used in f-string, got: {:?}", warnings
        );
    }

    #[test]
    fn underscore_prefix_suppressed() {
        let source = "\
void main():
    int _x = 5
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnusedVariable { .. }
            )),
            "expected no UnusedVariable for _x prefix, got: {:?}", warnings
        );
    }

    #[test]
    fn param_not_warned_unused() {
        // Parameters should not trigger unused variable warnings
        let source = "\
void process(int x):
    pass
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnusedVariable { name, .. }
                if name == "x"
            )),
            "expected no UnusedVariable for parameter, got: {:?}", warnings
        );
    }

    #[test]
    fn used_in_fstring_no_warning() {
        let source = "\
void main():
    int x = 42
    String s = f\"{x}\"
    print(s)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UnusedVariable { name, .. }
                if name == "x"
            )),
            "expected no UnusedVariable for variable used in f-string, got: {:?}", warnings
        );
    }

    // ─── Phase 5: Fallible Unwrap Tests ───────────────────────

    #[test]
    fn unchecked_unwrap_warns() {
        let source = "\
Option[int] find_value(): Some(42)

void main():
    Option[int] x = find_value()
    int v = x.unwrap()
    print(f\"{v}\")
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected UncheckedUnwrap for x, got: {:?}", warnings
        );
    }

    #[test]
    fn checked_via_is_some_no_warn() {
        let source = "\
Option[int] find_value(): Some(42)

void main():
    Option[int] x = find_value()
    if x.is_some():
        int v = x.unwrap()
        print(f\"{v}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected no UncheckedUnwrap for x after is_some() guard, got: {:?}", warnings
        );
    }

    #[test]
    fn checked_via_match_no_warn() {
        let source = "\
Option[int] find_value(): Some(42)

void main():
    Option[int] x = find_value()
    match x:
        case Some(v):
            print(f\"{v}\")
        else:
            print(\"none\")
    int val = x.unwrap()
    print(f\"{val}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected no UncheckedUnwrap for x after match, got: {:?}", warnings
        );
    }

    #[test]
    fn unwrap_or_no_warn() {
        let source = "\
Option[int] find_value(): Some(42)

void main():
    Option[int] x = find_value()
    int v = x.unwrap_or(0)
    print(f\"{v}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected no UncheckedUnwrap for unwrap_or, got: {:?}", warnings
        );
    }

    #[test]
    fn reassignment_resets_to_unchecked() {
        let source = "\
Option[int] find_value(): Some(42)

void main():
    Option[int] x = find_value()
    if x.is_some():
        pass
    x = find_value()
    int v = x.unwrap()
    print(f\"{v}\")
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected UncheckedUnwrap after reassignment, got: {:?}", warnings
        );
    }

    #[test]
    fn option_param_starts_unchecked() {
        let source = "\
void process(Option[int] x):
    int v = x.unwrap()
    print(f\"{v}\")

void main():
    process(Some(42))
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected UncheckedUnwrap for param, got: {:?}", warnings
        );
    }

    #[test]
    fn result_is_ok_no_warn() {
        let source = "\
Result[int, String] parse_num(): Ok(42)

void main():
    Result[int, String] r = parse_num()
    if r.is_ok():
        int v = r.unwrap()
        print(f\"{v}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "r"
            )),
            "expected no UncheckedUnwrap for Result after is_ok() guard, got: {:?}", warnings
        );
    }

    #[test]
    fn checked_one_branch_only_still_warns() {
        // Only checked in one branch — after merge, should remain checked
        // (since calling is_some at all proves awareness)
        let source = "\
Option[int] find_value(): Some(42)

void main():
    Option[int] x = find_value()
    if x.is_some():
        print(\"found\")
    int v = x.unwrap()
    print(f\"{v}\")
";
        let warnings = check_warnings(source);
        // After calling is_some(), the variable is marked Checked — unwrap is fine
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::UncheckedUnwrap { name, .. }
                if name == "x"
            )),
            "expected no UncheckedUnwrap after is_some() call, got: {:?}", warnings
        );
    }

    // ─── Phase 8: Private-in-Public Tests ─────────────────────

    #[test]
    fn private_return_type_errors() {
        let source = "\
private struct InternalState:
    int value

InternalState create():
    return InternalState(value=42)

void main():
    pass
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k,
                SemanticErrorKind::PrivateTypeInPublicSignature { type_name, fn_name, position }
                if type_name == "InternalState" && fn_name == "create" && position == "return type"
            )),
            "expected PrivateTypeInPublicSignature for return type, got: {:?}", errors
        );
    }

    #[test]
    fn private_param_type_errors() {
        let source = "\
private struct InternalState:
    int value

void process(InternalState state):
    print(f\"{state.value}\")

void main():
    pass
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k,
                SemanticErrorKind::PrivateTypeInPublicSignature { type_name, fn_name, position }
                if type_name == "InternalState" && fn_name == "process" && position == "parameter"
            )),
            "expected PrivateTypeInPublicSignature for parameter, got: {:?}", errors
        );
    }

    #[test]
    fn public_type_in_public_fn_ok() {
        let source = "\
struct PublicState:
    int value

PublicState create():
    return PublicState(value=42)

void main():
    pass
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k,
                SemanticErrorKind::PrivateTypeInPublicSignature { .. }
            )),
            "expected no PrivateTypeInPublicSignature for public type, got: {:?}", errors
        );
    }

    #[test]
    fn generic_private_type_caught() {
        let source = "\
private struct Secret:
    int value

Vector[Secret] get_secrets():
    return []

void main():
    pass
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(k,
                SemanticErrorKind::PrivateTypeInPublicSignature { type_name, fn_name, .. }
                if type_name == "Secret" && fn_name == "get_secrets"
            )),
            "expected PrivateTypeInPublicSignature for Vector[Secret], got: {:?}", errors
        );
    }

    // ─── Phase 7: Needless Mutable Borrow Tests ───────────────

    #[test]
    fn needless_mut_param_warns() {
        // Use a Copy type (int) — `&` on Copy types is purely about mutability,
        // so the warning should fire when the param is never mutated.
        let source = "\
void process(int &x):
    print(f\"{x}\")

void main():
    int v = 42
    process(&v)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "x"
            )),
            "expected NeedlessMutableBorrow for x, got: {:?}", warnings
        );
    }

    #[test]
    fn needless_mut_param_resource_read_only_warns() {
        // Un-suppressed (Class-C): a Resource `&`-param that is only READ is
        // needless. Under CoW-default-borrow a bare Resource param is a
        // read-only borrow, so dropping the `&` does NOT move the value — it
        // makes the intent explicit and elides the write-through clone. (This
        // used to be `needless_mut_param_move_type_no_warn`, whose "removing
        // `&` would move the value" rationale was a pre-CoW fossil.)
        let source = "\
void process(Vector[int] &items):
    for item in items:
        print(f\"{item}\")

void main():
    Vector[int] v = [1, 2, 3]
    process(&v)
";
        let warnings = check_warnings(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "items"
            )),
            "expected NeedlessMutableBorrow for read-only Resource items, got: {:?}", warnings
        );
    }

    #[test]
    fn mut_param_with_field_assign_no_warn() {
        let source = "\
struct Point:
    int x
    int y

void set_x(Point &p, int val):
    p.x = val

void main():
    Point pt = Point(x=0, y=0)
    set_x(&pt, 42)
    print(f\"{pt.x}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "p"
            )),
            "expected no NeedlessMutableBorrow for p with field assignment, got: {:?}", warnings
        );
    }

    #[test]
    fn mut_param_passed_as_mut_no_warn() {
        let source = "\
void inner(Vector[int] &v):
    v.append(1)

void outer(Vector[int] &items):
    inner(&items)

void main():
    Vector[int] v = [1, 2, 3]
    outer(&v)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "items"
            )),
            "expected no NeedlessMutableBorrow for items passed with &, got: {:?}", warnings
        );
    }

    #[test]
    fn mut_param_compound_assign_no_warn() {
        let source = "\
struct Counter:
    int count

void increment(Counter &c):
    c.count += 1

void main():
    Counter c = Counter(count=0)
    increment(&c)
    print(f\"{c.count}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "c"
            )),
            "expected no NeedlessMutableBorrow for c with compound assign, got: {:?}", warnings
        );
    }

    #[test]
    fn mut_param_underscore_prefix_no_warn() {
        let source = "\
void process(Vector[int] &_items):
    pass

void main():
    Vector[int] v = [1, 2, 3]
    process(&v)
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "_items"
            )),
            "expected no NeedlessMutableBorrow for _items, got: {:?}", warnings
        );
    }

    // ─── Class-C marking-hole probe matrix (a–g) + positive control ──
    // Every `&`-param that IS genuinely mutated must be marked (no false
    // NeedlessMutableBorrow), across every mutation channel; the read-only
    // control must warn. These are the acceptance contract for D1-pre-A (the
    // marking block consulting the unified `receiver_is_mutating`
    // classification, closing the builtin-mutator hole).

    // (a) direct field store, (b) index store, (d) pass-through `&`-arg,
    // (e) `&self` user method on a `&`-param — plus the read-only positive
    // control that MUST warn. One source exercises the whole surface.
    #[test]
    fn needless_mut_matrix_field_index_passthrough_selfmethod() {
        let source = "\
struct Bag:
    Vector[int] items

equip Bag:
    void grow(&self):
        self.items.push(7)

void a_field(Bag &b):
    b.items = [1, 2]

void b_index(Vector[int] &v):
    v[0] = 5

void d_inner(Vector[int] &v):
    v[0] = 9

void d_outer(Vector[int] &v):
    d_inner(&v)

void e_selfm(Bag &b):
    b.grow()

void main():
    Bag b = Bag(items=[0])
    a_field(&b)
    e_selfm(&b)
    Vector[int] v = [1]
    b_index(&v)
    d_outer(&v)
    print(f\"{v.len()} {b.items.len()}\")
";
        let warnings = check_warnings(source);
        // b (a_field / e_selfm) and v (b_index / d_outer) are all mutated —
        // none may warn.
        for name in ["b", "v"] {
            assert!(
                !has_warning(&warnings, |k| matches!(k,
                    crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name: n }
                    if n == name)),
                "unexpected NeedlessMutableBorrow for mutating param {name}, got: {:?}", warnings
            );
        }
        // The read-only positive control MUST warn — asserted with a distinct
        // param name so it can't collide with the mutating params above.
        let control = "\
void only_reads(Vector[int] &ro):
    print(f\"{ro.len()}\")

void main():
    Vector[int] v = [1]
    only_reads(&v)
";
        let cw = check_warnings(control);
        assert!(
            has_warning(&cw, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "ro")),
            "expected NeedlessMutableBorrow for read-only control ro, got: {:?}", cw
        );
    }

    // (c1) `&`-param mutated ONLY via a BUILTIN mutating method — the hole the
    // D1-pre-A fix closes. Must NOT warn.
    #[test]
    fn needless_mut_builtin_mutator_no_warn() {
        let source = "\
void f(Vector[int] &v):
    v.push(1)

void main():
    Vector[int] v = []
    f(&v)
    print(f\"{v.len()}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "v")),
            "expected no NeedlessMutableBorrow for builtin-mutated v, got: {:?}", warnings
        );
    }

    // (f) `&`-param mutated via a closure that captures it. Must NOT warn.
    #[test]
    fn needless_mut_closure_capture_no_warn() {
        let source = "\
void f(Vector[int] &v):
    void() g = (): v.push(1)
    g()

void main():
    Vector[int] v = []
    f(&v)
    print(f\"{v.len()}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "v")),
            "expected no NeedlessMutableBorrow for closure-captured v, got: {:?}", warnings
        );
    }

    // (g) `&self` mutated via a BUILTIN on one of its fields. Must NOT warn.
    #[test]
    fn needless_mut_self_field_builtin_no_warn() {
        let source = "\
struct Bag:
    Vector[int] items

equip Bag:
    void grow(&self):
        self.items.push(7)

void main():
    Bag b = Bag(items=[0])
    b.grow()
    print(f\"{b.items.len()}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "self")),
            "expected no NeedlessMutableBorrow for &self mutated via field builtin, got: {:?}", warnings
        );
    }

    // (h) `&`-param mutated ONLY through a `.get().unwrap()` borrow-view
    // chain — the marking hole behind the Class-C stage-1 bootstrap
    // regression (lir_push_inst / lir_set_term / emit / refine_local_type
    // were wrongly bared on the lint's advice, and the SH lowerer then
    // dropped the write-through, emitting empty basic blocks). All three
    // shapes must mark:
    //   * builtin mutator through the chain  (lir_push_inst shape)
    //   * field assignment through the chain (lir_set_term shape)
    //   * builtin mutator DIRECTLY on the unwrapped view — no field hop
    //     (the `emit` shape: `nested.get(i).unwrap().push(v)`; the receiver
    //     types as `Ref[Vector[T]]`, so the buffer-owning classification
    //     must peel the view)
    //   * `&`-pass-through to a chain mutator (the transitive caller)
    // The read-only chain control MUST still warn.
    #[test]
    fn needless_mut_get_chain_mutation_no_warn() {
        let source = "\
struct Block:
    Vector[int] insts
    int term

struct Func:
    Vector[Block] blocks
    Vector[Vector[int]] nested

void push_inst(Func &f, int bb, int inst):
    f.blocks.get(bb).unwrap().insts.push(inst)

void set_term(Func &f, int bb, int t):
    f.blocks.get(bb).unwrap().term = t

void push_nested(Func &f, int bb, int v):
    f.nested.get(bb).unwrap().push(v)

void outer(Func &f):
    push_inst(&f, 0, 42)

void main():
    Vector[int] e = []
    Vector[Block] blks = [Block(insts=e, term=0)]
    Vector[int] e2 = []
    Vector[Vector[int]] nst = [e2]
    Func fn = Func(blocks=blks, nested=nst)
    push_inst(&fn, 0, 1)
    set_term(&fn, 0, 2)
    push_nested(&fn, 0, 3)
    outer(&fn)
    print(f\"{fn.blocks.get(0).unwrap().insts.len()}\")
";
        let warnings = check_warnings(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "f")),
            "expected no NeedlessMutableBorrow for get-chain-mutating f, got: {:?}", warnings
        );
        // Read-only chain control: `.get().unwrap()` READ (no mutation) on a
        // `&`-param is still needless `&` — the chain routing must not
        // over-mark reads.
        let control = "\
struct Block:
    Vector[int] insts

struct Func:
    Vector[Block] blocks

void peek(Func &ro, int bb):
    print(f\"{ro.blocks.get(bb).unwrap().insts.len()}\")

void main():
    Vector[int] e = []
    Vector[Block] blks = [Block(insts=e)]
    Func fn = Func(blocks=blks)
    peek(&fn, 0)
";
        let cw = check_warnings(control);
        assert!(
            has_warning(&cw, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::NeedlessMutableBorrow { name }
                if name == "ro")),
            "expected NeedlessMutableBorrow for read-only get-chain control ro, got: {:?}", cw
        );
    }

    // ─── DeadBareParamWrite (dead write on a bare CoW param) ──

    fn has_deadwrite(
        warnings: &[crate::semantic::errors::SemanticWarning],
        param: &str,
    ) -> bool {
        has_warning(warnings, |k| matches!(k,
            crate::semantic::errors::SemanticWarningKind::DeadBareParamWrite { name, .. }
            if name == param
        ))
    }

    #[test]
    fn dead_bare_param_index_assign_warns() {
        let source = "\
void relabel(Vector[int] xs):
    xs[0] = 99

void main():
    Vector[int] a = [1, 2, 3]
    relabel(a)
    print(a[0])
";
        let warnings = check_warnings(source);
        assert!(
            has_deadwrite(&warnings, "xs"),
            "expected DeadBareParamWrite for xs, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_push_warns() {
        let source = "\
void add_item(Vector[int] xs):
    xs.push(42)

void main():
    Vector[int] a = [1]
    add_item(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            has_deadwrite(&warnings, "xs"),
            "expected DeadBareParamWrite for xs push, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_string_push_warns() {
        let source = "\
void shout(String s):
    s.push(\"!\")

void main():
    String m = \"hi\"
    shout(m)
    print(m)
";
        let warnings = check_warnings(source);
        assert!(
            has_deadwrite(&warnings, "s"),
            "expected DeadBareParamWrite for String s, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_user_mut_method_warns() {
        let source = "\
struct Counter:
    Vector[int] hits

equip Counter:
    void bump(&self):
        self.hits.push(1)

void tally(Counter c):
    c.bump()

void main():
    Counter c = Counter(hits=[0])
    tally(c)
    print(c.hits.len())
";
        let warnings = check_warnings(source);
        assert!(
            has_deadwrite(&warnings, "c"),
            "expected DeadBareParamWrite for &self method on c, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_chained_stmt_mutator_warns() {
        // `xs.pop().unwrap()` as a statement: the whole chain's result is
        // discarded, caller unchanged — warns by design (the span.start
        // statement-position classification).
        let source = "\
void trim(Vector[int] xs):
    xs.pop().unwrap()

void main():
    Vector[int] a = [1, 2]
    trim(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            has_deadwrite(&warnings, "xs"),
            "expected DeadBareParamWrite for chained stmt-position pop, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_write_param_span_is_declaration() {
        // The secondary label must point at the parameter declaration, which
        // precedes the mutation site in the source.
        let source = "\
void relabel(Vector[int] xs):
    xs[0] = 99

void main():
    Vector[int] a = [1]
    relabel(a)
    print(a[0])
";
        let warnings = check_warnings(source);
        let dw = warnings.iter().find(|w| matches!(&w.kind,
            crate::semantic::errors::SemanticWarningKind::DeadBareParamWrite { name, .. }
            if name == "xs"
        )).expect("expected DeadBareParamWrite for xs");
        if let crate::semantic::errors::SemanticWarningKind::DeadBareParamWrite { param_span, .. } = &dw.kind {
            assert!(
                param_span.start < dw.span.start,
                "param declaration span {:?} should precede the mutation span {:?}",
                param_span, dw.span
            );
        }
    }

    #[test]
    fn dead_bare_param_read_after_write_no_warn() {
        let source = "\
int peek_mod(Vector[int] xs):
    xs[0] = 99
    return xs[0]

void main():
    Vector[int] a = [1, 2]
    print(peek_mod(a))
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for scratch-copy read, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_mut_borrow_no_warn() {
        let source = "\
void add_item(Vector[int] &xs):
    xs.push(42)

void main():
    Vector[int] a = [1]
    add_item(&a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for `&` param, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_while_drain_no_warn() {
        // The while condition re-evaluates every iteration — its read is
        // loop-carried with the body's pop.
        let source = "\
void drain(Vector[int] xs):
    while xs.len() > 2:
        xs.pop()

void main():
    Vector[int] a = [1, 2, 3, 4]
    drain(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for while-cond drain, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_loop_read_before_write_no_warn() {
        let source = "\
void grow(Vector[int] xs):
    for i in 0..3:
        print(xs.len())
        xs.push(i)

void main():
    Vector[int] a = [1]
    grow(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for loop-carried read, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_rebind_no_warn() {
        let source = "\
void rebind(Vector[int] xs):
    xs = [9, 9]
    xs.push(1)
    print(xs.len())

void main():
    Vector[int] a = [1]
    rebind(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite after full rebind, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_value_position_pop_no_warn() {
        // Value-position mutating call is the peek idiom — a read of the copy.
        let source = "\
Option[int] take_last(Vector[int] xs):
    return xs.pop()

void main():
    Vector[int] a = [1, 2]
    print(take_last(a).unwrap_or(0))
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for value-position pop, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_underscore_no_warn() {
        let source = "\
void ignore_it(Vector[int] _xs):
    _xs.push(1)

void main():
    Vector[int] a = [1]
    ignore_it(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "_xs"),
            "expected no DeadBareParamWrite for _-prefixed param, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_copy_type_no_warn() {
        // Copy-struct param: mutation of a by-value copy is the
        // Python-identical model, not tracked.
        let source = "\
struct Point:
    int x
    int y

void set_x(Point p):
    p.x = 42

void main():
    Point pt = Point(x=1, y=2)
    set_x(pt)
    print(pt.x)
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "p"),
            "expected no DeadBareParamWrite for Copy struct, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_branch_sibling_read_no_warn() {
        // Deliberate false-negative pin: write in branch A, read in branch B.
        // Walk-order union semantics (no BranchState threading) suppress.
        let source = "\
void touch(Vector[int] xs, bool c):
    if c:
        xs.push(1)
    else:
        print(xs[0])

void main():
    Vector[int] a = [2]
    touch(a, true)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for sibling-branch read, got: {:?}", warnings
        );
    }

    #[test]
    fn dead_bare_param_fstring_read_no_warn() {
        // f-string interpolation reads go through synthetic-span paths — they
        // must still count as reads of the copy.
        let source = "\
void log_push(Vector[int] xs):
    xs.push(9)
    print(f\"len={xs.len()}\")

void main():
    Vector[int] a = [1]
    log_push(a)
    print(a.len())
";
        let warnings = check_warnings(source);
        assert!(
            !has_deadwrite(&warnings, "xs"),
            "expected no DeadBareParamWrite for f-string read after write, got: {:?}", warnings
        );
    }

    // ─── Phase 6: Const Promotion Tests ───────────────────────

    #[test]
    fn never_reassigned_int_warns() {
        let source = "\
void main():
    int x = 42
    print(f\"{x}\")
";
        let warnings = check_warnings_with_const(source);
        assert!(
            has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { name }
                if name == "x"
            )),
            "expected CouldBeConst for x, got: {:?}", warnings
        );
        // Default (warn_const=false) should NOT emit CouldBeConst
        let warnings_default = check_warnings(source);
        assert!(
            !has_warning(&warnings_default, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { .. }
            )),
            "expected no CouldBeConst by default, got: {:?}", warnings_default
        );
    }

    #[test]
    fn reassigned_int_no_warn() {
        let source = "\
void main():
    int x = 0
    x = 42
    print(f\"{x}\")
";
        let warnings = check_warnings_with_const(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { name }
                if name == "x"
            )),
            "expected no CouldBeConst for reassigned x, got: {:?}", warnings
        );
    }

    #[test]
    fn compound_assigned_no_warn() {
        let source = "\
void main():
    int x = 0
    x += 1
    print(f\"{x}\")
";
        let warnings = check_warnings_with_const(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { name }
                if name == "x"
            )),
            "expected no CouldBeConst for compound-assigned x, got: {:?}", warnings
        );
    }

    #[test]
    fn non_copy_type_no_const_warn() {
        let source = "\
void main():
    String s = \"hello\" + \"\"
    print(s)
";
        let warnings = check_warnings_with_const(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { name }
                if name == "s"
            )),
            "expected no CouldBeConst for non-Copy String, got: {:?}", warnings
        );
    }

    #[test]
    fn underscore_prefix_no_const_warn() {
        let source = "\
void main():
    int _x = 42
    pass
";
        let warnings = check_warnings_with_const(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { name }
                if name == "_x"
            )),
            "expected no CouldBeConst for _x, got: {:?}", warnings
        );
    }

    #[test]
    fn param_no_const_warn() {
        let source = "\
void process(int x):
    print(f\"{x}\")

void main():
    process(42)
";
        let warnings = check_warnings_with_const(source);
        assert!(
            !has_warning(&warnings, |k| matches!(k,
                crate::semantic::errors::SemanticWarningKind::CouldBeConst { name }
                if name == "x"
            )),
            "expected no CouldBeConst for parameter, got: {:?}", warnings
        );
    }

    // ─── Phase 4: Unused Import Tests ────────────────────────

    #[test]
    fn unused_import_warns() {
        // Note: this test uses a from-import of a function that's defined in the source
        // but never called. Since `helper` is defined, the resolver will find it.
        let _source = "\
int helper(): 42

void main():
    pass
";
        // For unused import, we need actual import statements. Since test fixtures
        // don't have multi-file imports, we verify the warning kind Display works.
        let kind = crate::semantic::errors::SemanticWarningKind::UnusedImport { name: "helper".to_string() };
        let warning = crate::semantic::errors::SemanticWarning {
            kind,
            span: crate::span::Span::new(0, 6),
        };
        assert!(warning.to_string().contains("unused import `helper`"));
    }

    // ─── Arena borrow-escape (non-Copy `.get()` aliasing into arena buffer) ───

    #[test]
    fn arena_borrow_escape_assign_outer_rejected() {
        // Binding a non-Copy `.get().unwrap()` borrow of an arena-scoped Vector
        // to an OUTER variable aliases into the arena buffer → UAF at scope exit.
        let source = "\
struct Arena:
    int cap

void main():
    String peek = \"init\"
    with Arena(4096) as pool:
        Vector[String] v = [\"payload\"]
        peek = v.get(0).unwrap()
    print(peek)
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::ArenaEscape {
                    kind: crate::semantic::errors::ArenaEscapeKind::AssignOuter { .. },
                    ..
                }
            )),
            "expected ArenaEscape::AssignOuter for arena borrow-escape, got: {:?}", errors
        );
    }

    #[test]
    fn arena_borrow_escape_return_rejected() {
        // Returning a non-Copy `.get().unwrap()` borrow of an arena-scoped Vector
        // escapes the arena scope → the returned borrow dangles after destroy.
        let source = "\
struct Arena:
    int cap

String first():
    with Arena(4096) as pool:
        Vector[String] v = [\"payload\"]
        return v.get(0).unwrap()
    return \"fallback\"

void main():
    print(first())
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::ArenaEscape {
                    kind: crate::semantic::errors::ArenaEscapeKind::Return,
                    ..
                }
            )),
            "expected ArenaEscape::Return for arena borrow-escape, got: {:?}", errors
        );
    }

    #[test]
    fn arena_copy_element_escape_ok() {
        // A Copy element (`int`) is value-copied out — no aliasing into the arena.
        // Both the AssignOuter and Return shapes must stay ACCEPTED.
        let source = "\
struct Arena:
    int cap

int first():
    with Arena(4096) as pool:
        Vector[int] v = [42]
        return v.get(0).unwrap()
    return 0

void main():
    int peek = 0
    with Arena(4096) as pool:
        Vector[int] v = [42]
        peek = v.get(0).unwrap()
    print(peek)
    print(first())
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::ArenaEscape { .. })),
            "unexpected ArenaEscape for Copy-element arena `.get()`, got: {:?}", errors
        );
    }

    #[test]
    fn arena_borrow_inner_binding_ok() {
        // Binding the `.get()` borrow to an INNER (in-scope) variable is safe:
        // the borrow does not outlive the arena. Must stay ACCEPTED.
        let source = "\
struct Arena:
    int cap

void main():
    with Arena(4096) as pool:
        Vector[String] v = [\"payload\"]
        String inner = v.get(0).unwrap()
        print(inner)
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::ArenaEscape { .. })),
            "unexpected ArenaEscape for in-scope arena borrow binding, got: {:?}", errors
        );
    }

    // ─── Arena borrow-escape sibling N2: collection-MUTATION consume positions ───

    #[test]
    fn arena_borrow_escape_push_outer_rejected() {
        // Pushing a non-Copy `.get().unwrap()` borrow of an arena-scoped Vector
        // into an OUTER collection aliases into the arena buffer → UAF at exit.
        let source = "\
struct Arena:
    int cap

void main():
    Vector[String] outer = Vector[String]()
    with Arena(4096) as pool:
        Vector[String] v = [\"payload\"]
        outer.push(v.get(0).unwrap())
    print(outer.get(0).unwrap())
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::ArenaEscape {
                    kind: crate::semantic::errors::ArenaEscapeKind::AssignOuter { .. },
                    ..
                }
            )),
            "expected ArenaEscape for arena borrow pushed into outer collection, got: {:?}", errors
        );
    }

    #[test]
    fn arena_borrow_escape_dict_insert_outer_rejected() {
        // Inserting a non-Copy arena-Vector element as a Dict VALUE into an
        // OUTER Dict aliases into the arena buffer → UAF at exit.
        let source = "\
struct Arena:
    int cap

void main():
    Dict[String, String] outerDict = Dict[String, String]()
    with Arena(4096) as pool:
        Vector[String] v = [\"payload\"]
        outerDict.insert(\"k\", v.get(0).unwrap())
    print(outerDict.get(\"k\").unwrap())
";
        let errors = check(source);
        assert!(
            has_error(&errors, |k| matches!(
                k,
                SemanticErrorKind::ArenaEscape {
                    kind: crate::semantic::errors::ArenaEscapeKind::AssignOuter { .. },
                    ..
                }
            )),
            "expected ArenaEscape for arena borrow inserted into outer dict, got: {:?}", errors
        );
    }

    #[test]
    fn arena_push_copy_element_outer_ok() {
        // A Copy element (`int`) is value-copied out — no aliasing into the
        // arena. Pushing into an outer collection must stay ACCEPTED.
        let source = "\
struct Arena:
    int cap

void main():
    Vector[int] outer = Vector[int]()
    with Arena(4096) as pool:
        Vector[int] v = [42]
        outer.push(v.get(0).unwrap())
    print(outer.get(0).unwrap())
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::ArenaEscape { .. })),
            "unexpected ArenaEscape for Copy-element push into outer collection, got: {:?}", errors
        );
    }

    #[test]
    fn arena_push_into_inner_collection_ok() {
        // Pushing into an INNER (arena-scoped) collection is safe: both the
        // source borrow and the destination die with the arena. Must stay
        // ACCEPTED (the destination does NOT outlive the arena).
        let source = "\
struct Arena:
    int cap

void main():
    with Arena(4096) as pool:
        Vector[String] v = [\"payload\"]
        Vector[String] inner = Vector[String]()
        inner.push(v.get(0).unwrap())
        print(inner.get(0).unwrap())
";
        let errors = check(source);
        assert!(
            !has_error(&errors, |k| matches!(k, SemanticErrorKind::ArenaEscape { .. })),
            "unexpected ArenaEscape for push into in-scope arena collection, got: {:?}", errors
        );
    }

