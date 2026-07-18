//! Pattern matching and match-statement lowering.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Block, Expr, Pattern};
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::super::drops::DropScopeKind;
use super::super::exprs::{lower_expr, maybe_auto_propagate, infer_operand_type_full, resolve_none_tag};
use super::lower_block;

/// Stage a match scrutinee into a fresh temp with the right `AssignMode` and
/// transfer ownership/borrow tracking to the temp.
///
/// Phase C: `_scrut = copy _src` for a resource-typed scrut is a shallow
/// alias of `_src`'s heap data. The validator (correctly) flags it. This
/// helper picks the mode by source shape and runs the matching ownership
/// transfer so the GIR carries a well-formed Move/Borrow at the boundary.
///
/// Three sites use it: `lower_match_stmt` (statement form),
/// `lower_match_stmt_as_expr` (last-stmt-in-block form — what nested
/// `match b:` inside an arm body lowers as), and `lower_match_expr`
/// (Expr::Match). All three must do the same thing — they were drifting,
/// which is what produced the @DataFrame__col_* cluster (the inner match
/// went through the second site, which lacked C2.9's fix).
/// Collect all `Binding(name)` identifiers introduced by a pattern,
/// recursively. Used by the Snag #41 detection to limit the
/// `arms_consume_payload` trigger to moves of pattern-bound names —
/// avoiding the Snag #42 regression where a *scrutinee* move (`!c`
/// inside `match c: case _: ...`) misclassified as a payload-consume
/// and the direct-source staging zeroed the scrutinee's payload via
/// `emit_pattern_bindings` before the arm body could read it.
fn collect_pattern_bindings(pattern: &Spanned<Pattern>, out: &mut Vec<String>) {
    match &pattern.node {
        Pattern::Binding(name) => out.push(name.clone()),
        Pattern::Constructor { fields, .. } => {
            for f in fields { collect_pattern_bindings(f, out); }
        }
        Pattern::Tuple(elems) => {
            for e in elems { collect_pattern_bindings(e, out); }
        }
        Pattern::Or(alts) => {
            for a in alts { collect_pattern_bindings(a, out); }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest
        | Pattern::DotShorthand { .. } => {}
    }
}

/// Walks an expression looking for `Expr::Move(Expr::Identifier(name))`
/// where `name` is one of the arm's pattern bindings. Returns true on
/// the first such match.
///
/// **Why filter by `bindings`.** The Snag #41 fix uses an
/// `arms_consume_payload` flag to switch to direct-source staging that
/// zeros the scrutinee's payload field via `emit_pattern_bindings`.
/// That zero is only correct when the move is of a pattern binding
/// (whose data aliases the scrutinee's payload). Moves of the
/// scrutinee itself (`!c` inside `match c:`) need the *opposite* —
/// the existing shallow-copy path that leaves the source intact, so
/// the wholesale move sees the original value. Filtering by bindings
/// makes the detection precise: `!v` where v is bound triggers
/// direct-source; `!c` where c is the scrutinee does not. (Snag #42,
/// 2026-05-12.)
fn body_has_binding_move(expr: &Spanned<Expr>, bindings: &[String]) -> bool {
    use crate::parser::visitor::{walk_expr, ExprVisitor};
    struct Finder<'a> { found: bool, bindings: &'a [String] }
    impl<'a> ExprVisitor for Finder<'a> {
        fn visit_expr(&mut self, e: &Spanned<Expr>) {
            if self.found { return; }
            if let Expr::Move { expr: inner } = &e.node {
                if let Expr::Identifier(name) = &inner.node {
                    if self.bindings.iter().any(|b| b == name) {
                        self.found = true;
                        return;
                    }
                }
            }
            walk_expr(self, e);
        }
        fn visit_stmt(&mut self, s: &Spanned<crate::parser::ast::Stmt>) {
            if self.found { return; }
            crate::parser::visitor::walk_stmt(self, s);
        }
        fn visit_block(&mut self, b: &Block) {
            if self.found { return; }
            crate::parser::visitor::walk_block(self, b);
        }
    }
    let mut f = Finder { found: false, bindings };
    f.visit_expr(expr);
    f.found
}

/// Returns true if any match arm has a pattern binding that's moved
/// (via `!name`) inside that arm's body. Drives the Snag #41
/// direct-source staging path. Else-arm contributes nothing since it
/// has no pattern → no bindings → no qualifying moves (a `!scrutinee`
/// inside else is the Snag #42 shape, which the existing shallow-copy
/// path handles correctly).
///
/// Two arm-shape helpers because the three callers of
/// `stage_match_scrutinee` pass arms in two different shapes
/// (`&[MatchItem]` for stmt forms, `&[MatchArm]` for the expr form).
pub fn arms_have_move_extract_items(
    arms: &[ast::MatchItem],
    _else_arm: Option<&Block>,
) -> bool {
    for item in arms {
        if let Some(a) = item.arm() {
            let mut bindings = Vec::new();
            collect_pattern_bindings(&a.pattern, &mut bindings);
            if !bindings.is_empty() && body_has_binding_move(&a.body, &bindings) {
                return true;
            }
        }
    }
    false
}

pub fn arms_have_move_extract_exprs(
    arms: &[ast::MatchArm],
    _else_arm: Option<&Spanned<Expr>>,
) -> bool {
    for a in arms {
        let mut bindings = Vec::new();
        collect_pattern_bindings(&a.pattern, &mut bindings);
        if !bindings.is_empty() && body_has_binding_move(&a.body, &bindings) {
            return true;
        }
    }
    false
}

/// Snag #48 follow-up: detect whether the user is explicitly matching on
/// Result / Option variants (`case Ok(x):`, `case Error(e):`, `case Some(v):`,
/// `case None:`). In that case the scrutinee must STAY as `Result[T, E]` /
/// `Option[T]` — auto-propagating it would discard the very Ok/Error split
/// the user wrote arms for.
///
/// The naive auto-prop-on-every-Result-scrutinee approach broke 3 fixtures
/// (snag31 / snag41 / nested_match_return_from_inner_arm) — all of the
/// `Completion c = match risky_call(): case Ok(x): … case Error(e): …`
/// shape, which is canonical Result-discrimination, NOT throws-sugar.
///
/// The check is name-based on the prelude variants because the typechecker's
/// resolved `expr_types` map isn't plumbed to IR-lowering. The prelude
/// names are stable contract; user-defined enums shouldn't shadow them.
pub fn arms_match_result_or_option_arm(arms: &[ast::MatchArm]) -> bool {
    arms.iter().any(|arm| pattern_is_result_or_option(&arm.pattern))
}

pub fn arms_match_result_or_option_item(arms: &[ast::MatchItem]) -> bool {
    arms.iter().any(|item| {
        item.arm().map_or(false, |arm| pattern_is_result_or_option(&arm.pattern))
    })
}

fn pattern_is_result_or_option(pat: &Spanned<ast::Pattern>) -> bool {
    match &pat.node {
        ast::Pattern::Constructor { path, .. } => {
            let head = path.first().map(|s| s.node.as_str());
            let second = path.get(1).map(|s| s.node.as_str());
            matches!(head, Some("Ok" | "Error" | "Some" | "None"))
                || (matches!(head, Some("Result" | "Option"))
                    && matches!(second, Some("Ok" | "Error" | "Some" | "None")))
        }
        _ => false,
    }
}

pub fn stage_match_scrutinee(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrut_op: &Operand,
    scrut_type: TypeId,
    source_at_last_use: bool,
    arms_consume_payload: bool,
) -> (LocalId, TypeId) {
    use crate::ir::instructions::AssignMode;
    // Snag #41 (2026-05-12): non-Copy user types (struct/enum containing
    // resources) under Borrow mode used to emit `[Bw] scrut_local =
    // copy source` into a fresh value-typed slot, which at LIR/C
    // produces a struct memcpy — a SHALLOW copy of all inner resource
    // fields.
    //
    // Two scope-exit paths through that shallow copy:
    //
    // (a) Read-only arms (`case C.V(x): use(x)`): extracted `x` is a
    //     non-owning view (no drop registered, no zero step). The
    //     scrut_local copy is not registered for drop either (the
    //     existing Borrow-stage path leaves the drop on source). At
    //     scope exit, only the source's drop fires — frees the data
    //     once. Safe.
    // (b) Consuming arms (`case C.V(x): !x` or anything that moves x
    //     to a new owner): the moved-out destination registers for
    //     drop. The source still aliases the same heap (no zero ever
    //     fired against it). At scope exit BOTH the new owner AND
    //     source try to free the same heap — double-free.
    //
    // The `arms_consume_payload` flag (true iff any arm body contains
    // an `Expr::Move`) distinguishes (a) from (b). Only (b) needs the
    // direct-source staging that zeros the source's payload at
    // extraction — for (a) the existing shallow-copy + view path is
    // correct and cheaper (no zero, source readable post-match).
    //
    // Collections (Vector/Dict/Set/HashMap/HashSet/Deque) and `String`
    // are safe under value-typed Borrow regardless because they carry
    // a runtime `cap == 0 ↔ view` discriminator — the borrowed copy
    // is marked as a view and drops elide. User aggregates have no
    // such discriminator, so case (b) above bites.
    let needs_direct_source = arms_consume_payload
        && ctx.type_registry.is_resource_type(scrut_type)
        && !ctx.type_registry.is_collection_type(scrut_type)
        && scrut_type != ctx.type_mapper.owned_string_type
        && !matches!(
            ctx.type_registry.get(scrut_type),
            Some(GirType::Ptr(_) | GirType::MutPtr(_))
        );
    // For Move-eligible non-Copy scrutinees (source at last-use, owned),
    // keep the existing staging path so ownership transfers cleanly into
    // a fresh scrut_local and the source is zeroed.
    let move_eligible = if let Operand::Copy(p) | Operand::Move(p) = scrut_op {
        p.projections.is_empty()
            && ctx.is_owned_local(builder, p.local)
            && source_at_last_use
    } else { false };

    if needs_direct_source && !move_eligible {
        if let Operand::Copy(place) | Operand::Move(place) = scrut_op {
            if place.projections.is_empty() {
                // Use the source local directly — no staging assign,
                // no shallow copy. The match's `enum_field_load_move`
                // zeros the source's payload field in-place at LIR
                // (`Inst::Store(NullPtr) through FieldPtr` — see
                // `src/lir/lower/insts.rs:1335`), correctly partial-
                // moving the source. The source's existing drop
                // registration handles the now-zeroed value (a
                // resource drop on a zeroed slot is a no-op via the
                // cap=0 / NULL-pointer path). Any subsequent reads of
                // source (e.g. a later `return !source` from a sister
                // arm) see the partially-moved state — by construction
                // a sister arm wouldn't have executed if this arm did,
                // and same-arm post-match reads of source are now well-
                // defined: the moved field is zero, the others intact.
                return (place.local, scrut_type);
            }
        }
    }

    let scrut_local = builder.add_local(scrut_type, None);
    // Phase C: prefer Borrow for resource-typed scrutinees by default —
    // a match doesn't consume its scrutinee, so the staged temp is a
    // non-owning view. This makes the GIR mode unconditionally non-Copy
    // for resources, regardless of whether the source's ownership state
    // is currently tracked as Owned (loop-reassigned named locals etc.).
    // The earlier per-source-shape picker depended on is_owned_local
    // which is brittle across loop iterations + reassignments. Borrow
    // is correct under the match-doesn't-consume invariant.
    //
    // Owned + dead-after sources still benefit from Move (transfers
    // ownership), but only when we can prove the source is owned at
    // this site — which the predicate below handles.
    //
    // Reached only for: (a) Copy-eligible types (primitives, value
    // structs without resources), (b) types with view-discriminators
    // (collections / String), (c) types already Ptr-shaped, (d)
    // Move-eligible cases (last-use + owned). The Snag #41 path above
    // handles all other non-Copy aggregates via Ptr alias.
    let mode = if !ctx.type_registry.is_resource_type(scrut_type) {
        AssignMode::Copy
    } else if let Operand::Copy(p) | Operand::Move(p) = scrut_op {
        if p.projections.is_empty()
            && ctx.is_owned_local(builder, p.local)
            && source_at_last_use
        {
            AssignMode::Move
        } else {
            AssignMode::Borrow
        }
    } else {
        AssignMode::Copy
    };
    builder.assign_mode(mode, Place::local(scrut_local), scrut_op.clone());

    if let Operand::Copy(place) | Operand::Move(place) = scrut_op {
        if place.projections.is_empty() && ctx.is_owned_local(builder, place.local) {
            ctx.set_owned(builder, scrut_local);
            let src_type = builder.local_type(place.local);
            if ctx.type_registry.needs_drop(src_type) {
                if !ctx.is_named_local(place.local) {
                    // Unnamed temp source: dead by construction at this site,
                    // safe to zero. Existing transfer + zero pattern.
                    ctx.drops.unregister(place.local);
                    ctx.move_zero_and_mark(builder, place.local);
                    ctx.drops.register_local(scrut_local, scrut_type, &ctx.type_registry);
                } else if source_at_last_use {
                    // Named source at last-use: source is dead after the
                    // match, so transferring the drop to the scrutinee is
                    // sound — without this, an arm's `unwrap()` zeros only
                    // the scrutinee's variant tag (memcpy aliased the source
                    // and scrut at the resource fields), and the source's
                    // scope-exit drop double-frees what unwrap extracted.
                    // No move_zero: `tag_of source` reads the source after
                    // the staging assign and needs the data intact.
                    ctx.drops.unregister(place.local);
                    ctx.drops.register_local(scrut_local, scrut_type, &ctx.type_registry);
                }
                // Named source NOT at last-use: leave drop on source. The
                // user's later use of the source expects its data to live;
                // transferring would leak. Aliasing-double-free risk on
                // arm consumption stays open (Snag #25d).
            }
        }
        // Ref propagation: see comment in lower_match_stmt below.
        if !place.projections.is_empty() && ctx.is_ref_local(builder, place.local) {
            ctx.set_ref(builder, scrut_local);
        }
    }
    (scrut_local, scrut_type)
}

/// Lower a match statement to GIR using Branch chains.
pub(super) fn lower_match_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchItem],
    else_arm: &Option<Block>,
) {
    // Lower scrutinee to a temp local.
    // For & params (MutPtr), lower_expr auto-derefs to a VALUE copy — creating
    // a shallow alias. For match, we want the original MutPtr so scrut_is_ptr
    // detects it and pattern extraction produces borrows, not copies.
    //
    // Snag #48: when the scrutinee is a `throws`-fn call inside a
    // `throws E` context, the operand is `Result[T, E]` at the IR
    // layer. The match patterns are written against T, so without
    // auto-propagation the pattern condition / extraction reads
    // Result's layout as if it were T — variant payloads come out
    // as zero/discriminant garbage. Apply `maybe_auto_propagate` on
    // the non-identifier scrutinee path (the identifier paths bind
    // an already-named local, which is the `Tagged t = throws_fn();
    // match t:` workaround shape that always worked — auto-prop
    // already fired at the VarDecl site).
    //
    // GATE: when the arm patterns explicitly match `Ok(x)` / `Error(e)`
    // / `Some(v)` / `None`, the user wants to discriminate the Result
    // (or Option) directly — auto-propagating would discard the very
    // split they wrote arms for. Skip auto-prop in that case. This is
    // the snag31 / snag41 / nested_match_return_from_inner_arm shape.
    // `lower_expr`'s centralized auto-prop hook (producer-side, Call /
    // MethodCall only) handles the throws-sugar unwrap when the scrutinee
    // is a call. For Identifier scrutinees (the `Result[T,E] fr = ...;
    // match fr:` shape) the hook does not fire because Identifier isn't a
    // producer — fall back to the explicit unwrap step for the non-user-
    // matches-Result/Option case. The one-shot `suppress_auto_prop` flag
    // covers the Call/MethodCall case for `case Ok/Error/Some/None` arms.
    let user_matches_result_option = arms_match_result_or_option_item(arms);
    let (scrut_op, scrut_type) = if let Expr::Identifier(name) = &scrutinee.node {
        if let Some((local_id, type_id)) = ctx.lookup_local(name) {
            if ctx.is_param_borrow_unique(builder, local_id) {
                // & or ! param — use the MutPtr local directly, skip auto-deref
                (Operand::Copy(Place::local(local_id)), type_id)
            } else {
                let op = lower_expr(ctx, builder, scrutinee);
                let op = if user_matches_result_option {
                    op
                } else {
                    let saved_expected = ctx.func_state.expected_type.take();
                    let op = maybe_auto_propagate(ctx, builder, op, scrutinee.span);
                    ctx.func_state.expected_type = saved_expected;
                    op
                };
                let ty = infer_operand_type_full(ctx, &op, builder);
                (op, ty)
            }
        } else {
            let op = lower_expr(ctx, builder, scrutinee);
            let ty = infer_operand_type_full(ctx, &op, builder);
            (op, ty)
        }
    } else {
        // Call / MethodCall / other expressions. Take `expected_type` so a
        // Result-typed surrounding context doesn't block the hook (which
        // would otherwise see the match's destination type and skip).
        let saved_expected = ctx.func_state.expected_type.take();
        ctx.func_state.suppress_auto_prop = user_matches_result_option;
        let op = lower_expr(ctx, builder, scrutinee);
        ctx.func_state.expected_type = saved_expected;
        let ty = infer_operand_type_full(ctx, &op, builder);
        (op, ty)
    };

    // Check if scrutinee is dead after the match (last use at match site) AND
    // the operand is a simple local we can MoveZero. If both, we can skip the
    // pattern extraction clone for string fields — the extracted field takes
    // ownership, and both the scrutinee copy AND original are zeroed.
    let scrutinee_dead_original = if let Expr::Identifier(name) = &scrutinee.node {
        if ctx.is_last_use_at(name, scrutinee.span) {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = scrut_op {
                if place.projections.is_empty() {
                    // Exclude Ptr originals — they're borrowed from the caller,
                    // and the existing scrut_is_ptr check already skips cloning.
                    let orig_type = builder.local_type(place.local);
                    let is_ptr = matches!(ctx.type_registry.get(orig_type),
                        Some(GirType::Ptr(_) | GirType::MutPtr(_)));
                    if !is_ptr { Some(place.local) } else { None }
                } else { None }
            } else { None }
        } else { None }
    } else { None };

    // Phase C: stage the scrutinee with the right AssignMode and ownership
    // transfer. See `stage_match_scrutinee` for the full rationale.
    // Snag #25d: pass last-use to enable safe drop transfer for named
    // sources at end-of-life — without this, an arm's `unwrap()` zeros only
    // the scrutinee copy and the source's drop double-frees the payload.
    let source_at_last_use = scrutinee_dead_original.is_some();
    let arms_consume_payload = arms_have_move_extract_items(arms, else_arm.as_ref());
    let (scrut_local, scrut_type) = stage_match_scrutinee(ctx, builder, &scrut_op, scrut_type, source_at_last_use, arms_consume_payload);

    let merge_bb = builder.new_block();

    // Snapshot the pre-branch `maybe_moved` flags so each arm sees the same
    // starting view — match arms are mutually exclusive at the language level,
    // so a `mark_moved(_x)` in one arm must not leak into the next arm's
    // lowering. Mirrors `lower_if`'s fix for snag #8 (2026-05-05). Without
    // this, a sequence of arms that each move the same local into a different
    // field would only emit `move_zero` on the first arm — leaving the
    // others with a heap double-drop at scope exit.
    let pre_branch_moved = ctx.drops.snapshot_moved();
    let mut post_branch_snapshots: Vec<Vec<(usize, usize, bool)>> = Vec::new();

    // Process each arm as a test-body chain (MetaFor items are always expanded before lowering)
    let concrete_arms: Vec<&ast::MatchArm> = arms.iter().filter_map(|i| i.arm()).collect();
    for (i, arm) in concrete_arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < concrete_arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        // Emit pattern condition check
        let cond = lower_pattern_condition(ctx, builder, &arm.pattern, scrut_local, scrut_type);

        if arm.guard.is_some() {
            // Pattern match → check guard → arm body
            let guard_bb = builder.new_block();
            builder.branch(cond, guard_bb, next_test_bb);

            builder.switch_to(guard_bb);
            let saved_arm = ctx.save_locals(builder);
            ctx.drops.push_scope(DropScopeKind::Block);
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            let guard_cond = lower_expr(ctx, builder, arm.guard.as_ref().unwrap());
            builder.branch(guard_cond, arm_body_bb, next_test_bb);

            builder.switch_to(arm_body_bb);
            // Re-emit bindings in the body block — the guard block's SSA values
            // aren't visible here (different basic block).
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
            ctx.restore_locals(builder, saved_arm);
        } else {
            builder.branch(cond, arm_body_bb, next_test_bb);

            // Arm body (non-guarded — safe to elide pattern clone if scrutinee is dead)
            builder.switch_to(arm_body_bb);
            let saved_arm = ctx.save_locals(builder);
            ctx.drops.push_scope(DropScopeKind::Block);
            ctx.func_state.scrutinee_clone_elision = scrutinee_dead_original.is_some();
            emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
            ctx.func_state.scrutinee_clone_elision = false;
            if let Some(original_local) = scrutinee_dead_original {
                // Move-if-dead: scrutinee is dead after this match.
                // Unregister from drops — the extracted payload takes ownership.
                // MoveZero still fires to prevent stale reads through the pointer.
                ctx.drops.unregister(original_local);
                ctx.move_zero_and_mark(builder, original_local);
            }
            lower_expr(ctx, builder, &arm.body);
            if builder.is_terminated() {
                // Return/break/continue already emitted early-exit drops — don't double-drop.
                ctx.drops.pop_scope_no_emit();
            } else {
                ctx.drops.pop_scope(builder, &ctx.type_registry);
                builder.jump(merge_bb);
            }
            ctx.restore_locals(builder, saved_arm);
        }

        post_branch_snapshots.push(ctx.drops.snapshot_moved());
        ctx.drops.restore_moved(&pre_branch_moved);

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_body) = else_arm {
        let saved_else = ctx.save_locals(builder);
        ctx.drops.push_scope(DropScopeKind::Block);
        lower_block(ctx, builder, else_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }
        ctx.restore_locals(builder, saved_else);
        post_branch_snapshots.push(ctx.drops.snapshot_moved());
        ctx.drops.restore_moved(&pre_branch_moved);
    }

    // Conservative join: union each arm's moves into the post-match state.
    for snap in &post_branch_snapshots {
        ctx.drops.union_moved(snap);
    }

    builder.switch_to(merge_bb);

    // MoveZero the scrutinee copy at the merge point. Each arm may have
    // extracted variant data from scrut_local; the scope-exit drop would
    // otherwise double-free it. This runs after ALL arms have jumped here.
    if scrutinee_dead_original.is_some() && ctx.type_registry.needs_drop(scrut_type) {
        ctx.move_zero_and_mark(builder, scrut_local);
    }
}

/// Lower a pattern condition to a boolean Operand.
pub fn lower_pattern_condition(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) -> Operand {
    match &pattern.node {
        Pattern::Wildcard => FunctionBuilder::const_bool(true),

        Pattern::Literal(expr) => {
            // None literal: compare enum tag instead of struct == NULL
            if matches!(expr.node, Expr::NoneLiteral) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                let none_tag = resolve_none_tag(ctx, scrut_type);
                let cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(none_tag)),
                );
                return FunctionBuilder::copy(cmp);
            }
            let lit_op = lower_expr(ctx, builder, expr);
            let cmp = builder.cmp(
                CmpOp::Eq,
                scrut_type,
                FunctionBuilder::copy(scrut_local),
                lit_op,
            );
            FunctionBuilder::copy(cmp)
        }

        Pattern::Binding(name) => {
            // Constant-pattern: resolver marked this `case CONST_NAME:`
            // as a value comparison (Snag 2026-05-13). Emit equality
            // compare against the constant's folded value.
            if let Some(&def_id) = ctx.analysis.resolution_map.get(&pattern.span.start) {
                let kind = ctx.analysis.scopes.get_def(def_id).kind;
                if matches!(kind, crate::semantic::scope::DefKind::Const | crate::semantic::scope::DefKind::Static) {
                    if let Some(const_value) = ctx.module_constants.get(name).cloned() {
                        let cmp = builder.cmp(
                            CmpOp::Eq,
                            scrut_type,
                            FunctionBuilder::copy(scrut_local),
                            Operand::Constant(const_value),
                        );
                        return FunctionBuilder::copy(cmp);
                    }
                }
            }
            // Check if this is an enum variant name (unit variant match)
            if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            // Plain variable binding — always matches
            FunctionBuilder::const_bool(true)
        }

        Pattern::Constructor { path, fields } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return FunctionBuilder::const_bool(true);
            };
            // Qualified path (Color.Red): use first segment as enum name.
            // Bare variant (Some, None, Ok, Error): prefer scrutinee type name
            // to avoid ambiguity when multiple monomorphizations exist.
            let (enum_name, variant_name) = if path.len() >= 2 {
                (path[0].node.clone(), variant_name)
            } else {
                let en = ctx.type_registry.type_name(scrut_type)
                    .or_else(|| {
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                            ctx.type_registry.type_name(inner)
                        } else {
                            None
                        }
                    })
                    .or_else(|| ctx.resolve_enum_variant(&variant_name).map(|(en, _)| en));
                match en {
                    Some(en) => (en, variant_name),
                    None => return FunctionBuilder::const_bool(true),
                }
            };
            let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
            if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                let tag_cmp = builder.cmp(
                    CmpOp::Eq,
                    I32_TYPE,
                    FunctionBuilder::copy(tag),
                    Operand::Constant(Constant::I32(variant_tag as i32)),
                );

                // Check whether any sub-pattern needs a nested condition check (e.g.,
                // Outer.Wrap(Inner.A(n)) must also check Inner's discriminant).
                let has_nested = fields.iter().any(|fp|
                    !matches!(fp.node, Pattern::Binding(_) | Pattern::Wildcard | Pattern::Rest)
                );

                if !has_nested {
                    return FunctionBuilder::copy(tag_cmp);
                }

                // Short-circuit: only check inner patterns when the outer tag matches.
                // This avoids extracting fields from the wrong variant.
                let result_id = builder.add_local(BOOL_TYPE, None);
                let inner_bb = builder.new_block();
                let merge_bb = builder.new_block();
                let false_bb = builder.new_block();

                builder.branch(FunctionBuilder::copy(tag_cmp), inner_bb, false_bb);

                // false_bb: outer tag didn't match → result = false
                builder.switch_to(false_bb);
                builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));
                builder.jump(merge_bb);

                // inner_bb: outer tag matched → check nested sub-patterns
                builder.switch_to(inner_bb);
                let mut inner_result: Option<LocalId> = None;
                for (i, field_pat) in fields.iter().enumerate() {
                    if matches!(field_pat.node, Pattern::Binding(_) | Pattern::Wildcard | Pattern::Rest) {
                        continue;
                    }
                    let field_type = ctx.type_registry.get_type_def(&enum_name)
                        .and_then(|td| {
                            if let TypeDefKind::Enum(ref e) = td.kind {
                                e.variants.iter()
                                    .find(|v| v.name == variant_name)
                                    .and_then(|v| v.fields.get(i))
                                    .map(|f| f.type_id)
                            } else {
                                None
                            }
                        })
                        .unwrap_or(I64_TYPE);

                    // Snag #34: Borrow mode — the condition test reads the
                    // payload to inspect a nested constructor's tag but
                    // must NOT zero the source field, because
                    // `emit_pattern_bindings` re-reads from the same source
                    // for the actual binding. Without Borrow, the test's
                    // destructive read zeros the payload and the binding
                    // sees zeros (silent wrong-value bug — Dict[K, V]
                    // with non-Copy V returning zeros after put).
                    let field_local = builder.enum_field_load_borrow(
                        Place::local(scrut_local),
                        variant_name.clone(),
                        i as u32,
                        field_type,
                    );
                    let sub_cond = lower_pattern_condition(
                        ctx, builder, field_pat, field_local, field_type,
                    );
                    inner_result = Some(match inner_result {
                        None => {
                            let tmp = builder.add_local(BOOL_TYPE, None);
                            builder.assign(Place::local(tmp), sub_cond);
                            tmp
                        }
                        Some(prev) => builder.bin_op(
                            BinOp::BitAnd,
                            BOOL_TYPE,
                            FunctionBuilder::copy(prev),
                            sub_cond,
                        ),
                    });
                }
                let final_inner = inner_result.map_or_else(
                    || FunctionBuilder::const_bool(true),
                    FunctionBuilder::copy,
                );
                builder.assign(Place::local(result_id), final_inner);
                builder.jump(merge_bb);

                builder.switch_to(merge_bb);
                return FunctionBuilder::copy(result_id);
            }
            FunctionBuilder::const_bool(true)
        }

        Pattern::Or(alts) => {
            // Short-circuit OR: if any alternative matches, return true
            let result_id = builder.add_local(BOOL_TYPE, None);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));

            let merge_bb = builder.new_block();

            for (i, alt) in alts.iter().enumerate() {
                let cond = lower_pattern_condition(ctx, builder, alt, scrut_local, scrut_type);
                let next_bb = if i + 1 < alts.len() {
                    builder.new_block()
                } else {
                    merge_bb
                };
                let true_bb = builder.new_block();
                builder.branch(cond, true_bb, next_bb);

                builder.switch_to(true_bb);
                builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
                builder.jump(merge_bb);

                if i + 1 < alts.len() {
                    builder.switch_to(next_bb);
                }
            }

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_id)
        }

        Pattern::Tuple(_) | Pattern::Rest => {
            // Structural match — always matches if types match
            FunctionBuilder::const_bool(true)
        }

        Pattern::DotShorthand { variant, .. } => {
            // Use scrutinee type to look up the enum name, then compare tag
            let enum_name = ctx.type_registry.type_name(scrut_type)
                .or_else(|| {
                    if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                        ctx.type_registry.type_name(inner)
                    } else {
                        None
                    }
                })
                .or_else(|| ctx.resolve_enum_variant(&variant.node).map(|(en, _)| en));
            if let Some(ref en) = enum_name {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(en, &variant.node) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            FunctionBuilder::const_bool(true)
        }
    }
}

/// Emit pattern bindings — assign destructured values to local variables.
pub fn emit_pattern_bindings(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) {
    match &pattern.node {
        Pattern::Binding(name) => {
            // If not an enum variant, bind the scrutinee value
            if ctx.resolve_enum_variant(name).is_none() {
                ctx.register_local(name, scrut_local, scrut_type);
            }
        }

        Pattern::Constructor { path, fields } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return;
            };

            // Use scrutinee type to find the enum name (avoids ambiguous variant lookups
            // when multiple monomorphized enums share variant names like "Some"/"None"/"Ok"/"Err")
            // For qualified paths (Color.Red), path[0] gives us the explicit enum name.
            let enum_name = if path.len() >= 2 {
                Some(path[0].node.clone())
            } else {
                ctx.type_registry.type_name(scrut_type)
                    .or_else(|| {
                        // Fallback: pointer type → dereference to find pointee name
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                            ctx.type_registry.type_name(inner)
                        } else {
                            None
                        }
                    })
                    .or_else(|| {
                        // Last resort: use variant name lookup (may be ambiguous for generics)
                        ctx.resolve_enum_variant(&variant_name).map(|(en, _)| en)
                    })
            };
            let enum_name = if let Some(en) = enum_name {
                en
            } else {
                return;
            };

            // Is the scrutinee a Ptr (borrowed)? Also true for borrow-derived
            // scrutinees (set by lower_match_stmt when scrut_op chained through
            // a ref-typed local — see comment on `set_ref(scrut_local)` there).
            let scrut_is_ptr = matches!(
                ctx.type_registry.get(scrut_type),
                Some(GirType::Ptr(_) | GirType::MutPtr(_))
            ) || ctx.is_ref_local(builder, scrut_local);

            for (i, field_pat) in fields.iter().enumerate() {
                // Skip extraction for wildcard sub-patterns — they bind
                // nothing, so the only side effect of `enum_field_load_move`
                // would be the source-payload-zero step (a leak for resource
                // fields, ill-typed for void/Unit fields). Surfaced by
                // gorget-js critique 2026-05-13: `void X() throws E`
                // produces `Result[void, E]` whose Ok variant has a
                // void/uint8_t payload; `case Ok(_)` previously emitted a
                // `void` load (`*(void*)Ok_0`) and the C backend rejected
                // it. The Rust `Pattern::Wildcard` handler below is a
                // no-op anyway, so the prior extraction's discarded `dst`
                // was pure waste.
                if matches!(field_pat.node, Pattern::Wildcard) {
                    continue;
                }
                // Determine the field type from the enum variant OR struct
                // definition. Struct constructor patterns (`case Point(x,y):`)
                // share this AST shape; using enum payload layout (+tag offset)
                // on a struct mis-binds by one field (Core #8).
                let is_struct_ctor = ctx
                    .type_registry
                    .get_type_def(&enum_name)
                    .map(|td| matches!(td.kind, TypeDefKind::Struct(_)))
                    .unwrap_or(false);
                let mut field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    match &type_def.kind {
                        TypeDefKind::Enum(e) => {
                            if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                                if let Some(f) = v.fields.get(i) {
                                    f.type_id
                                } else {
                                    I64_TYPE
                                }
                            } else {
                                I64_TYPE
                            }
                        }
                        TypeDefKind::Struct(s) => {
                            if let Some(f) = s.fields.get(i) {
                                f.type_id
                            } else {
                                I64_TYPE
                            }
                        }
                        _ => I64_TYPE,
                    }
                } else {
                    I64_TYPE
                };

                // When scrutinee is Ptr (borrowed param), resource-type variant
                // fields should be references into the enum's storage, not
                // shallow copies. This ensures borrows derived from the field
                // (e.g., d.get(key) on a borrowed Dict) remain valid for the
                // lifetime of the borrowed scrutinee.
                // Box types are excluded — user code explicitly dereferences
                // them with `*a`, which requires a Box value, not a Ptr.
                if scrut_is_ptr && ctx.type_registry.is_resource_type(field_type) {
                    // Read the typed `metadata.is_box` flag at every Box-TypeDef
                    // registration path (replaces a name-prefix probe).
                    if !ctx.type_registry.is_box(field_type) {
                        field_type = ctx.type_registry.insert(GirType::Ptr(field_type));
                    }
                }

                let dst = if is_struct_ctor {
                    // Struct fields are 0-based on the value layout (no tag).
                    builder.field_load(
                        Place::local(scrut_local),
                        i as u32,
                        field_type,
                    )
                } else {
                    builder.enum_field_load_move(
                        Place::local(scrut_local),
                        variant_name.clone(),
                        i as u32,
                        field_type,
                    )
                };

                // Mark Ptr-extracted locals as ref_locals (no auto-deref, no drop).
                // Phase D: origin is Field { base: scrut_local, field: i }.
                if matches!(ctx.type_registry.get(field_type), Some(GirType::Ptr(_))) {
                    ctx.set_field_borrow(builder, dst, scrut_local, i as u32);
                }
                // Value scrutinee + droppable field (string, collection, user
                // struct with resource fields): register for drop at scope exit.
                // Pattern extraction is a shallow memcpy — the binding and the
                // scrutinee share the same heap buffer.
                // When scrutinee_clone_elision is set, the scrutinee is dead and
                // both the scrutinee copy AND the original variable will be zeroed
                // after extraction — the shallow copy takes ownership directly.
                // For non-elided cases, the scrutinee is MoveZeroed after
                // extraction (line ~538), so the binding still takes ownership.
                // Strings/collections clone to get an independent buffer;
                // user structs take ownership directly (no clone needed).
                else if !scrut_is_ptr
                    && ctx.type_registry.needs_drop(field_type)
                {
                    let is_string_or_collection =
                        field_type == ctx.type_mapper.owned_string_type
                        || ctx.type_registry.is_collection_type(field_type);

                    if is_string_or_collection {
                        // String/collection: original behavior. Clone elision
                        // registers directly; otherwise clone for independence.
                        if ctx.func_state.scrutinee_clone_elision {
                            ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                            ctx.set_owned(builder, dst);
                        } else if let Some(clone_fn) = ctx.clone_fn_for_ptr(field_type) {
                            ctx.warn_clone_and_hit(builder, pattern.span, field_type, crate::ir::ImplicitCloneReason::PatternExtraction);
                            let ptr_type = ctx.register_ptr_type(field_type);
                            let ptr = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr, Place::local(dst));
                            let cloned = builder.call_clone(
                                &clone_fn,
                                vec![FunctionBuilder::copy(ptr)],
                                field_type,
                                crate::ir::ImplicitCloneReason::PatternExtraction,
                            );
                            // Phase C: cloned is a fresh owned local; this assign
                            // transfers ownership into dst (the binding). Move
                            // mode matches the runtime intent — the cloned temp
                            // is dead after this single use.
                            builder.assign_mode(
                                crate::ir::instructions::AssignMode::Move,
                                Place::local(dst),
                                FunctionBuilder::copy(cloned),
                            );
                            ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                            ctx.set_owned(builder, dst);
                        }
                    } else if ctx.is_owned_local(builder, scrut_local)
                        && ctx.func_state.scrutinee_clone_elision
                    {
                        // Clone elision (scrutinee is last-use): take ownership
                        // directly.  Both the scrutinee copy AND the original
                        // will be MoveZero'd — the extracted field owns the data.
                        ctx.drops.register_local(dst, field_type, &ctx.type_registry);
                        ctx.set_owned(builder, dst);
                    }
                    // Non-last-use: extracted field is a VIEW into the scrutinee
                    // copy (no registration, no drop).  The copy is dropped at
                    // the merge point, freeing the data.  This avoids both
                    // cloning and double-free for match-in-loop patterns.
                }

                // Recurse on sub-pattern
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
            // Move semantics: zero the scrutinee after extracting all variant fields.
            // Prevents double-free when both extracted values and the scrutinee are dropped.
            // Match arms are exclusive — only one arm executes, so zeroing is safe.
            // Enum *and* struct resource fields: last-use clone-elision must
            // zero the scrutinee after extract so bindings own without double-free.
            // Pre-Track-A this was enum-only, so `match s: case S(r):` on a
            // last-use resource-struct could double-free / skip zero (Core #8).
            let has_resource_field = fields.iter().enumerate().any(|(i, _)| {
                if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    match &type_def.kind {
                        TypeDefKind::Enum(e) => {
                            if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                                if let Some(f) = v.fields.get(i) {
                                    return ctx.type_registry.is_resource_type(f.type_id);
                                }
                            }
                        }
                        TypeDefKind::Struct(s) => {
                            if let Some(f) = s.fields.get(i) {
                                return ctx.type_registry.is_resource_type(f.type_id);
                            }
                        }
                        _ => {}
                    }
                }
                false
            });
            if has_resource_field && ctx.func_state.scrutinee_clone_elision {
                // Last-use (clone elision active): extracted fields took
                // ownership — zero the copy to prevent double-free.
                builder.move_zero(Place::local(scrut_local));
                ctx.drops.mark_moved(scrut_local);
            }
            // Non-last-use: the copy stays alive — extracted fields are views.
            // The copy is dropped at scope exit via the normal drop tracker.
        }

        Pattern::Tuple(elems) => {
            // Phase C FieldLoad migration (2026-05-06): two shapes for
            // resource elements:
            //
            // (a) scrutinee owns the data (clone elision: scrutinee is
            //     last-use): emit `field_load + move_zero` so the
            //     extracted bindings take ownership. The scrutinee's
            //     drop won't double-free (its slots are zeroed).
            //
            // (b) scrutinee borrows (default — for-loop iteration over a
            //     vector returns a value-typed Tuple that aliases the
            //     vector's storage; nested-pattern matches against a
            //     non-last-use scrutinee): wrap the resource field as
            //     Ptr(elem_type), tag it as a field borrow, and let the
            //     auto-clone path materialise ownership at boundaries.
            //
            // Non-resource fields stay value-typed (bit-copy is sound).
            let move_resource_fields = ctx.func_state.scrutinee_clone_elision;
            for (i, elem_pat) in elems.iter().enumerate() {
                let elem_type = super::super::exprs::resolve_tuple_field_type(ctx, scrut_type, i);
                let is_resource = ctx.type_registry.is_resource_type(elem_type);
                let (load_type, wrap_as_borrow) = if is_resource && !move_resource_fields {
                    (ctx.type_registry.insert(GirType::Ptr(elem_type)), true)
                } else {
                    (elem_type, false)
                };
                let dst = builder.field_load(Place::local(scrut_local), i as u32, load_type);
                if move_resource_fields && is_resource {
                    builder.move_zero(Place {
                        local: scrut_local,
                        projections: vec![Projection::Field(i as u32)],
                    });
                }
                if wrap_as_borrow {
                    ctx.set_field_borrow(builder, dst, scrut_local, i as u32);
                }
                emit_pattern_bindings(ctx, builder, elem_pat, dst, load_type);
            }
        }

        Pattern::DotShorthand { variant, fields } => {
            // Look up enum name from scrutinee type (same as Constructor)
            let enum_name = ctx.type_registry.type_name(scrut_type)
                .or_else(|| {
                    if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(scrut_type).cloned() {
                        ctx.type_registry.type_name(inner)
                    } else {
                        None
                    }
                })
                .or_else(|| ctx.resolve_enum_variant(&variant.node).map(|(en, _)| en));
            let enum_name = if let Some(en) = enum_name { en } else { return; };
            let variant_name = variant.node.clone();

            // Mirror Constructor handler: when scrutinee is a Ptr (borrowed),
            // resource-type variant fields should bind as Ptr<T> references
            // into the enum's storage rather than shallow-copy values.
            // Without this, `enum_field_load_move`'s post-extract zero would
            // write through the borrow back into the original (e.g.
            // `menu.items[i].variant.Button.label` cleared after the first
            // match in gorget-arena's draw_menu, surfaced 2026-04-28).
            // `is_ref_local` covers borrow-derived scrutinees whose type is
            // not itself a Ptr (e.g. `match item.item_type` — see
            // `lower_match_stmt`'s ref-propagation block).
            let scrut_is_ptr = matches!(
                ctx.type_registry.get(scrut_type),
                Some(GirType::Ptr(_) | GirType::MutPtr(_))
            ) || ctx.is_ref_local(builder, scrut_local);

            for (i, field_pat) in fields.iter().enumerate() {
                let mut field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            v.fields.get(i).map(|f| f.type_id).unwrap_or(I64_TYPE)
                        } else { I64_TYPE }
                    } else { I64_TYPE }
                } else { I64_TYPE };

                if scrut_is_ptr && ctx.type_registry.is_resource_type(field_type) {
                    // Read the typed `metadata.is_box` flag at every Box-TypeDef
                    // registration path (replaces a name-prefix probe).
                    if !ctx.type_registry.is_box(field_type) {
                        field_type = ctx.type_registry.insert(GirType::Ptr(field_type));
                    }
                }

                let dst = builder.enum_field_load_move(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );

                // Phase D: origin is Field { base: scrut_local, field: i }.
                if matches!(ctx.type_registry.get(field_type), Some(GirType::Ptr(_))) {
                    ctx.set_field_borrow(builder, dst, scrut_local, i as u32);
                }

                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
        }

        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Or(_) | Pattern::Rest => {
            // No bindings
        }
    }
}
