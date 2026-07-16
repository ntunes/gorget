# RV-F SCOUT REPORT — four ggdef ORACLE divergences (liveness/Copy/Callable)

Worktree: /workspace/gorget/.claude/worktrees/agent-a3dd94c65c87279cc
Binaries: target/debug/gg (production), target/debug/ggdef (oracle).
Prototype patch: /tmp/recover_rvf_proto.patch (257 lines; spec/ggdef/src/elaborate/{mod.rs,liveness.rs}).

## VERDICT: all four divergences CONFIRMED + fixed end-to-end. Prototype: full ggdef suite GREEN
(lib 140/0, corpus_a/b/b1 + converter_agreement AGREE 198 [floor 182, UNCHANGED from baseline]
+ coverage_histogram + gen_idempotent + spec_conformance_ggdef all pass). Main-repo spec_conformance
3/3 (C/LLVM/selfhost) unaffected (diff is ggdef-only). 32/33 repros agree; the 1 exception is a
pre-existing subset gap (`.cb()` field-call), NOT RV-F.

## Premise table (file:line verified against CURRENT source + MEASURED)

| # | claim | prod ref | measured gg vs ggdef (pre-fix) | status |
|---|-------|----------|-------------------------------|--------|
| 11 | is_copy = `matches!(.., Ty::Prim)` too narrow | mod.rs:1110 / call_arg 1168; prod is_copy_type type_utils.rs:17 | `f(&x,x.p)` all-int struct: gg ACCEPT, ggdef REJECT E_BorrowConflict | CONFIRMED |
| 13 | assign-revive doesn't seed loop_locals | liveness.rs:454-456; prod mark_live origins.rs:14-21 | reassign-then-move-in-loop: gg ACCEPT+run, ggdef REJECT E_MoveInLoop | CONFIRMED |
| 14 | desugar_for elem = body-local Bind → seeded loop-local | mod.rs:945-952; prod check_stmt.rs:968 vs :992 | `for x in v: sink(!x)`: gg REJECT E_MoveInLoop, ggdef ACCEPT+run | CONFIRMED |
| 15 | Ty::Callable no bind-side reject | mod.rs:1428/2335, ty_tainted:524; prod needs_explicit_move type_utils.rs:102 | callable bare bind (all 3 kinds): gg REJECT E_MoveWithoutOperator, ggdef ACCEPT | CONFIRMED |

## Calibration measurements (what shaped each fix — all end-to-end gg-vs-ggdef)

### #11 Copy extent — ty_is_copy mirrors production is_copy_type EXACTLY:
- all-int struct field `x.p`: gg ACCEPT → ggdef must accept (was reject). FIX.
- tuple `(int,int)` field: gg ACCEPT → include Tuple-of-Copy. FIX.
- nested all-scalar struct: gg ACCEPT → recurse Named structs. FIX.
- all-scalar user ENUM field: gg ACCEPT → recurse Named enums (enum_payload_types). FIX.
- `Option[int]` field: gg REJECT (Option is Generic, NOT in prod Copy whitelist) → Option/Result stay NON-Copy.
- struct with a String field: BOTH reject (correct — non-Copy).
- drop-tainted struct: NEVER Copy (D4/D12 mutual exclusion).

### #14 for-var vs user borrow-bind (THE critical distinction):
- `for x in v: sink(!x)`: gg REJECT (for-var is NOT loop-local).
- user `String s = coll[i]; sink(!s)` INSIDE a loop: gg ACCEPT (s IS loop-local). **← rules out keying on
  BorrowView-source shape naively.** BUT: bind_source NEVER produces Source::BorrowView (only the
  for-desugar mod.rs:947 does; user bare-place binds → Source::Copy). So a `Stmt::Bind` with
  `Source::BorrowView` is UNIQUELY the for-element view → safe+precise to key on (typed metadata, not
  a string heuristic). The user case is Source::Copy → still loop-local → still accepted.

### #15 position matrix (measured — production rejects at a SUBSET of consume positions):
- REJECT (bare callable): bind `R g=f`, whole reassign `g=f`, struct-ctor `Holder(f)`, enum-variant
  `Wrap.Fn(f)`/`Some(f)`, struct-literal. (all 3 kinds Callable/MutCallable/ConsumeCallable)
- ACCEPT (last-use move / borrow): `return f`, `v.push(f)`, `[f]` collection literal, `apply(f,5)` plain
  arg, `(): inner(9)` capture-for-call.
- **Drop-taint axis DIFFERS at return**: gg REJECTS bare drop-tainted `return r` (dt_return_bare) but
  ACCEPTS bare callable `return f` (c15_return_bare). So the callable broadening must NOT touch the
  return position — it stays drop-taint-only (matches prod tainted_place_name at check_stmt.rs:833;
  ggdef reject_if_tainted_live_place("return") already correct). This is the position matrix mirror of
  prod's `require_explicit_move_for_single_owner_init` (check_expr.rs:24) call-site set + bare-assign
  (check_stmt.rs:1490), NOT a blanket single-owner reject.
- Reject is IDENTIFIER-gated + Borrow-ownership-gated (prod needs_explicit_move identifier branch;
  `!f` is an explicit move → accept).

## Prototype (all four; see /tmp/recover_rvf_proto.patch)
- #11: new `ty_is_copy(&self, ty)` (mod.rs, after ty_tainted). Replaces the two `matches!(.., Ty::Prim)`
  in check_arg_place_overlap AND call_arg_source (the Copy-snapshot decision — same axis, kept uniform).
- #13: liveness.rs Assign whole-local revive ALSO seeds `loop_locals.last_mut()` (mirrors mark_live).
- #14: liveness.rs `declare` split into declare / declare_non_loop_local / declare_impl(loop_local:bool);
  Bind arm routes `Source::BorrowView` binds → declare_non_loop_local.
- #15: two guards `reject_if_single_owner_callable_init` (Identifier-gated) + `_arg` (Borrow-gated),
  wired at bind_source, whole-local Assign, StructLiteral, struct_ctor_args (pos+named), prelude-enum
  ctor, user-enum bare ctor, method-form enum-variant. NOT at return/push-set/collection-ctor/
  array-tuple-literal/capture.

## Blast radius on committed spectests: NONE flipped.
spec_conformance_ggdef (ASSERTS ggdef-vs-committed-expect) PASSES → no committed fixture re-adjudicated.
gen_idempotent PASSES → no generated expectation changed. converter_agreement AGREE 198 identical
pre/post patch (measured both). #14 re-check: no committed reject fixture was passing for the wrong
reason on the for-var-move shape; drop_reassign_after_move.gg (for+reassign, no for-var move) unaffected.

## RV-H adjacency (NOT touched): the while-condition gate hole is liveness.rs Stmt::While arm (~line 539,
`check_expr(cond)` before check_loop_body increments loop_depth). My liveness.rs edits are all in
declare/Bind/Assign (lines <490) — the While arm is untouched. Executor: carve the zone away from it.

## NEW pre-existing finding (file-don't-fix): calling a struct's callable FIELD (`h.cb(5)`) is outside
the ggdef phase-0 subset (elaborate error "method .cb() outside phase-0 subset"). Honest subset gap
(not a soundness divergence). Blocks testing the struct-callable-field-CALL shape but NOT the ctor-init
reject (c15_ctor_nocall tests it without the call). Note for the axis-extension track, don't fix here.
