//! Last-use analysis for auto-move optimization (Phase 1f).
//!
//! Full-function reverse walk that tags each variable use as "last use" or not.
//! A use is "last" if the variable is not used on any future execution path.
//!
//! Algorithm: reverse AST walk with branch union and two-pass loops.
//! Result: a set of span positions where the identifier is the last use.
//!
//! Perf note (2026-05-18): the live set was `FxHashSet<String>` and was
//! cloned at every branch (if/elif/else, match arm, while body, for body,
//! Expr::If, Expr::Match). For 695 functions in the self-host lowerer
//! workload that scaled to ~9 ms by itself. Switched to
//! `FxHashSet<&'a str>` where `'a` is the lifetime of the AST function
//! body — the `&str` references point at the `String` fields of
//! `Expr::Identifier`. Cloning is now hash-rebuild of small `&str`
//! headers (16 bytes), no per-entry `String` allocation. Same applies
//! to the `cow_reassigned_after` analysis in `functions.rs`.

use crate::parser::ast::*;
use crate::span::Spanned;
use rustc_hash::{FxHashMap, FxHashSet};

/// Result of full-function liveness analysis.
/// Contains the span start positions of identifier uses that are the last use
/// of that variable on all reachable paths, each mapped to the NAME it is the
/// last use OF.
///
/// The name is carried rather than discarded because the query
/// (`LoweringContext::is_last_use_at`) is asked *"is this use of `name` its last
/// use"*, and a position-only set can only answer *"is some variable's last use
/// here"*. The walker has the name in hand at the point it records the position
/// (`Expr::Identifier(name)`), so dropping it forced the read side to trust that
/// every caller pairs a name with that name's own span — an invariant nothing
/// enforced (Core #14). Keeping it makes the query answer the question asked.
#[derive(Default)]
pub struct LivenessResult {
    /// `span.start` of each last-use `Expr::Identifier` → the name used there.
    pub last_use_spans: FxHashMap<usize, String>,
}

/// Compute last-use information for an entire function body.
pub fn compute_function_liveness(body: &[Spanned<Stmt>]) -> LivenessResult {
    let mut live: FxHashSet<&str> = FxHashSet::default();
    let mut last_use_spans = FxHashMap::default();
    // Function-wide, before the reverse walk: see `seed_on_error_uses`.
    seed_on_error_uses(body, &mut live);
    walk_block(body, &mut live, &mut last_use_spans);
    LivenessResult { last_use_spans }
}

fn walk_block<'a>(
    stmts: &'a [Spanned<Stmt>],
    live: &mut FxHashSet<&'a str>,
    last_uses: &mut FxHashMap<usize, String>,
) {
    // `on error:` handlers are live from their REGISTRATION to function exit,
    // not at the registration point. The handler runs on the error path, which
    // leaves from any statement AFTER the registration -- so a name the handler
    // reads is live across all of them.
    //
    // A reverse walk visits those later statements BEFORE it reaches the
    // registration, so injecting the handler's uses when the registration is
    // reached is too late: everything after it was already walked blind. The
    // uses must be seeded before the reverse walk of this block begins.
    //
    // This over-approximates -- the handler's names are treated as live for the
    // whole block, including before the registration. That is the SAFE
    // direction (an extra clone), and the precise window would need a forward
    // pass this walker does not have.
    //
    // Measured: without the seed, a consume AFTER the registration printed
    // garbage at rc 0 on both backends while the same program with the handler
    // below the consume was correct. Unioning at the registration (the first
    // attempt) fixed only the second shape.
    for stmt in stmts.iter().rev() {
        walk_stmt(&stmt.node, live, last_uses);
    }
}

/// Seed the uses of every `on error:` handler in the function.
///
/// TWO properties, each of which was a live defect when absent:
///
/// * **USES ONLY, NEVER KILLS.** The handler body is walked into a FRESH set
///   and only the survivors are unioned in. Passing the caller's `live`
///   straight to `walk_block` leaks the handler's KILLS into the normal path,
///   deleting a name the normal path still reads -- measured as
///   `local _N read after MoveZero` one block down.
/// * **FUNCTION-WIDE, NOT PER-BLOCK.** `on_error_blocks` is function-scoped
///   and never popped (`context.rs:251`, `stmts/mod.rs:460`), so a handler
///   registered inside an `if`/`while` is live to function exit just like a
///   top-level one. Seeding only the current block left every statement after
///   the ENCLOSING block walked blind -- measured rc 0 GARBAGE on both lanes.
///
/// Last-use decisions are discarded: a handler use is never the last use of a
/// name on the normal path, and recording it would mark a span the normal path
/// also reads.
fn seed_on_error_uses<'a>(stmts: &'a [Spanned<Stmt>], live: &mut FxHashSet<&'a str>) {
    for s in stmts {
        for handler in on_error_bodies_in(&s.node) {
            let mut handler_live: FxHashSet<&'a str> = FxHashSet::default();
            let mut discard: FxHashMap<usize, String> = FxHashMap::default();
            walk_block(handler, &mut handler_live, &mut discard);
            live.extend(handler_live);
        }
    }
}

/// Every `on error:` body reachable from `stmt`, including nested ones.
///
/// EXHAUSTIVE over `Stmt`, and it must ALSO descend through EXPRESSIONS:
/// `Expr::Do` and `Expr::Block` carry statement blocks, so an `on error:`
/// inside a `do:` (as a statement, or as a `VarDecl` value) is reachable only
/// through the expression side. A first version of this function was
/// `Stmt`-exhaustive and expression-blind, and those two cells printed GARBAGE
/// at rc 0 on both backends -- the seed was provably INERT on them (ablating
/// it left their GIR byte-identical while a control's moved 85 lines).
///
/// Expression descent routes through `parser::visitor::visit_expr_children`,
/// the one exhaustive child enumeration, rather than being hand-rolled again.
fn on_error_bodies_in<'a>(stmt: &'a Stmt) -> Vec<&'a [Spanned<Stmt>]> {
    let mut out: Vec<&[Spanned<Stmt>]> = Vec::new();
    fn push_block<'b>(out: &mut Vec<&'b [Spanned<Stmt>]>, b: &'b Block) {
        for s in &b.stmts {
            out.extend(on_error_bodies_in(&s.node));
        }
    }
    match stmt {
        Stmt::OnError { body } => {
            out.push(&body.stmts);
            push_block(&mut out, body);
        }
        Stmt::If { then_body, elif_branches, else_body, .. } => {
            push_block(&mut out, then_body);
            for (_c, b) in elif_branches { push_block(&mut out, b); }
            if let Some(b) = else_body { push_block(&mut out, b); }
        }
        Stmt::While { body, else_body, .. } | Stmt::For { body, else_body, .. } => {
            push_block(&mut out, body);
            if let Some(b) = else_body { push_block(&mut out, b); }
        }
        Stmt::Loop { body }
        | Stmt::Unsafe { body }
        | Stmt::NamedScope { body, .. }
        | Stmt::With { body, .. }
        | Stmt::MetaFor { body, .. }
        | Stmt::MetaWhile { body, .. } => push_block(&mut out, body),
        Stmt::Match { arms, else_arm, .. } => {
            for item in arms {
                let arm = match item {
                    MatchItem::Arm(a) => a,
                    MatchItem::MetaFor { arm_template, .. } => arm_template,
                };
                if let Expr::Block(b) = &arm.body.node { push_block(&mut out, b); }
            }
            if let Some(b) = else_arm { push_block(&mut out, b); }
        }
        Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
            push_block(&mut out, then_body);
            for (_c, b) in elif_branches { push_block(&mut out, b); }
            if let Some(b) = else_body { push_block(&mut out, b); }
        }
        Stmt::MetaMatch { arms, else_arm, .. } => {
            for (_c, b) in arms { push_block(&mut out, b); }
            if let Some(b) = else_arm { push_block(&mut out, b); }
        }
        Stmt::Select { arms, else_arm } => {
            for a in arms { push_block(&mut out, &a.body); }
            if let Some(b) = else_arm { push_block(&mut out, b); }
        }
        // These carry EXPRESSIONS, and an expression can carry a block
        // (`do:` / a block expression), so they are not leaves for this walk.
        Stmt::VarDecl { value, .. } | Stmt::Expr(value) | Stmt::Throw(value)
        | Stmt::Snapshot { value, .. } | Stmt::MetaConst { value, .. } => {
            out.extend(on_error_bodies_in_expr(&value.node));
        }
        Stmt::Return(Some(e)) => out.extend(on_error_bodies_in_expr(&e.node)),
        Stmt::Assign { target, value, .. } | Stmt::CompoundAssign { target, value, .. } => {
            out.extend(on_error_bodies_in_expr(&target.node));
            out.extend(on_error_bodies_in_expr(&value.node));
        }
        Stmt::Assert { condition, message, .. }
        | Stmt::AssertReturn { condition, message } => {
            out.extend(on_error_bodies_in_expr(&condition.node));
            if let Some(m) = message { out.extend(on_error_bodies_in_expr(&m.node)); }
        }
        Stmt::MetaLog { args, .. } => {
            for a in args { out.extend(on_error_bodies_in_expr(&a.node)); }
        }
        // Genuinely nothing to walk.
        Stmt::Return(None) | Stmt::Break | Stmt::Continue | Stmt::Pass | Stmt::Item(_) => {}
    }
    out
}

fn walk_stmt<'a>(
    stmt: &'a Stmt,
    live: &mut FxHashSet<&'a str>,
    lu: &mut FxHashMap<usize, String>,
) {
    match stmt {
        Stmt::VarDecl { pattern, value, .. } => {
            kill_pattern(pattern, live);
            uses_expr(&value.node, value.span.start, live, lu);
        }
        Stmt::Assign { target, value, .. } => {
            if let Expr::Identifier(name) = &target.node {
                live.remove(name.as_str());
            }
            uses_expr(&value.node, value.span.start, live, lu);
            uses_target_sub(&target.node, live, lu);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            uses_expr(&value.node, value.span.start, live, lu);
            uses_expr(&target.node, target.span.start, live, lu);
        }
        Stmt::Return(Some(expr)) | Stmt::Expr(expr) => {
            uses_expr(&expr.node, expr.span.start, live, lu);
        }
        Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
            let saved = live.clone();
            let mut live_else = saved.clone();
            if let Some(b) = else_body { walk_block(&b.stmts, &mut live_else, lu); }
            let mut live_then = saved.clone();
            walk_block(&then_body.stmts, &mut live_then, lu);
            *live = live_then;
            live.extend(live_else);
            for (cond, body) in elif_branches.iter().rev() {
                let mut live_elif = saved.clone();
                walk_block(&body.stmts, &mut live_elif, lu);
                uses_expr(&cond.node, cond.span.start, &mut live_elif, lu);
                live.extend(live_elif);
            }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Stmt::While { condition, body, else_body, .. } => {
            // Pass 1: collect live-at-exit set.  Discard last-use decisions —
            // they don't account for the loop back-edge and produce false
            // positives (e.g., match scrutinee incorrectly marked last-use).
            let mut live_body = live.clone();
            let mut lu_discard: FxHashMap<usize, String> = FxHashMap::default();
            if let Some(eb) = else_body { walk_block(&eb.stmts, &mut live_body, &mut lu_discard); }
            walk_block(&body.stmts, &mut live_body, &mut lu_discard);
            uses_expr(&condition.node, condition.span.start, &mut live_body, &mut lu_discard);
            live.extend(live_body);
            // Pass 2: walk with loop-propagated live set.  Only this pass
            // records last-use decisions.
            //
            // UNION, never overwrite: the body may run ZERO times, so a KILL
            // inside it must not delete a name that is live before the loop.
            // Measured -- overwriting printed garbage at rc 0 on both backends
            // for a break-skips-kill shape and for a zero-iteration `for`.
            let mut live_body2 = live.clone();
            walk_block(&body.stmts, &mut live_body2, lu);
            live.extend(live_body2);
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Stmt::For { pattern, iterable, body, else_body, .. } => {
            // Pass 1: collect live set (discard last-use decisions).
            let mut live_body = live.clone();
            let mut lu_discard: FxHashMap<usize, String> = FxHashMap::default();
            if let Some(eb) = else_body { walk_block(&eb.stmts, &mut live_body, &mut lu_discard); }
            walk_block(&body.stmts, &mut live_body, &mut lu_discard);
            kill_pattern(pattern, &mut live_body);
            live.extend(live_body);
            // Pass 2: record last-use decisions. UNION, never overwrite -- see
            // the `While` arm; a zero-iteration `for` must not let a kill in
            // its body delete a name live before the loop.
            let mut live_body2 = live.clone();
            walk_block(&body.stmts, &mut live_body2, lu);
            live.extend(live_body2);
            kill_pattern(pattern, live);
            uses_expr(&iterable.node, iterable.span.start, live, lu);
        }
        Stmt::Match { scrutinee, arms, else_arm, .. } => {
            let saved = live.clone();
            let mut union: FxHashSet<&'a str> = FxHashSet::default();
            for item in arms {
                // `MatchItem::MetaFor` carries an `arm_template` whose body is real code that
                // the meta expansion will emit. Liveness/prescan run on the UNEXPANDED AST, so
                // dropping this item walks that body blind -- measured rc 0 GARBAGE on both
                // backends against a control with the arms written out. Walk the template.
                let arm = match item {
                    MatchItem::Arm(a) => a,
                    MatchItem::MetaFor { arm_template, .. } => arm_template,
                };
                {
                    let mut a = saved.clone();
                    uses_expr(&arm.body.node, arm.body.span.start, &mut a, lu);
                    if let Some(g) = &arm.guard { uses_expr(&g.node, g.span.start, &mut a, lu); }
                    kill_pattern(&arm.pattern, &mut a);
                    union.extend(a);
                }
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                walk_block(&b.stmts, &mut a, lu);
                union.extend(a);
            }
            *live = union;
            uses_expr(&scrutinee.node, scrutinee.span.start, live, lu);
        }
        Stmt::Assert { condition, message, .. } => {
            if let Some(m) = message { uses_expr(&m.node, m.span.start, live, lu); }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }

        // ── Forms below were `_ => {}` until 2026-08-27 ──────────────────────
        // Under-approximating `live` is the DANGEROUS direction: a use this
        // walker cannot see makes the variable look dead at an earlier
        // consuming position, so the value is MOVED instead of cloned and the
        // later read returns garbage. Measured at pristine HEAD: `loop:` and
        // `throw` printed garbage at rc 0 on BOTH backends while ggdef printed
        // the right answer, and `unsafe:` / a named scope / `with` /
        // `assert return` ICE'd with `read after MoveZero`. `while true:` and
        // `loop:` differ by one keyword and differed in correctness.
        // Over-approximating is merely conservative (an extra clone), so every
        // arm here walks everything it carries.

        // An infinite loop is `While` without the condition: the body's uses
        // must survive the back-edge, so the same two-pass treatment applies —
        // pass 1 collects live-at-exit with last-use decisions DISCARDED
        // (they cannot account for the back-edge), pass 2 records them.
        Stmt::Loop { body } => {
            let mut live_body = live.clone();
            let mut lu_discard: FxHashMap<usize, String> = FxHashMap::default();
            walk_block(&body.stmts, &mut live_body, &mut lu_discard);
            live.extend(live_body);
            // UNION, never overwrite -- a `break` can skip a kill in the body,
            // so the kill must not delete a name live before the loop. This arm
            // shipped with the overwrite bug and is fixed here with its
            // siblings (Core #4).
            let mut live_body2 = live.clone();
            walk_block(&body.stmts, &mut live_body2, lu);
            live.extend(live_body2);
        }
        // Straight-line scopes: the block runs in sequence with its neighbours.
        Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
            walk_block(&body.stmts, live, lu);
        }
        // P14 CANDIDATE FIX: `on error:` is an ALTERNATIVE path, not a
        // straight-line scope -- it runs ONLY when the function exits via an
        // error and never on normal return (language-reference S10.7). Walking
        // it into `live` directly lets a KILL inside the handler (an
        // assignment, or a shadowing VarDecl) delete a name that is still live
        // on the normal path -- an UNDER-approximation, the dangerous
        // direction. Union it the way `If` unions a branch.
        // Seeded at block entry by `seed_on_error_uses` -- see `walk_block`.
        // Walking it again here would record last-use spans inside a handler
        // that the normal path also reads.
        Stmt::OnError { .. } => {}
        // Reverse walk: the body runs AFTER the binding exprs are evaluated.
        Stmt::With { bindings, body } => {
            walk_block(&body.stmts, live, lu);
            for b in bindings.iter().rev() {
                uses_expr(&b.expr.node, b.expr.span.start, live, lu);
            }
        }
        Stmt::Throw(e) | Stmt::Snapshot { value: e, .. } => {
            uses_expr(&e.node, e.span.start, live, lu);
        }
        Stmt::AssertReturn { condition, message } => {
            if let Some(m) = message { uses_expr(&m.node, m.span.start, live, lu); }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        // Arms are alternatives: union their live sets, as `Match` does.
        Stmt::Select { arms, else_arm } => {
            let saved = live.clone();
            let mut union: FxHashSet<&'a str> = FxHashSet::default();
            for arm in arms {
                let mut a = saved.clone();
                walk_block(&arm.body.stmts, &mut a, lu);
                union.extend(a);
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                walk_block(&b.stmts, &mut a, lu);
                union.extend(a);
            }
            *live = union;
            for arm in arms {
                match &arm.op {
                    crate::parser::ast::SelectOp::Send { channel, value, .. } => {
                        uses_expr(&value.node, value.span.start, live, lu);
                        uses_expr(&channel.node, channel.span.start, live, lu);
                    }
                    crate::parser::ast::SelectOp::Recv { channel, .. } => {
                        uses_expr(&channel.node, channel.span.start, live, lu);
                    }
                }
            }
        }
        // Compile-time forms. They are evaluated/eliminated before lowering, so
        // they cannot themselves consume a runtime local — but they can MENTION
        // one, and over-approximation is the safe direction, so walk them.
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            let saved = live.clone();
            if let Some(b) = else_body { walk_block(&b.stmts, live, lu); }
            let mut lt = saved.clone();
            walk_block(&then_body.stmts, &mut lt, lu);
            live.extend(lt);
            for (cond, body) in elif_branches.iter().rev() {
                let mut le = saved.clone();
                walk_block(&body.stmts, &mut le, lu);
                uses_expr(&cond.node, cond.span.start, &mut le, lu);
                live.extend(le);
            }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Stmt::MetaFor { range: e, body, .. } | Stmt::MetaWhile { condition: e, body, .. } => {
            let mut live_body = live.clone();
            let mut lu_discard: FxHashMap<usize, String> = FxHashMap::default();
            walk_block(&body.stmts, &mut live_body, &mut lu_discard);
            live.extend(live_body);
            walk_block(&body.stmts, live, lu);
            uses_expr(&e.node, e.span.start, live, lu);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            let saved = live.clone();
            let mut union: FxHashSet<&'a str> = FxHashSet::default();
            for (_case, body) in arms {
                let mut a = saved.clone();
                walk_block(&body.stmts, &mut a, lu);
                union.extend(a);
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                walk_block(&b.stmts, &mut a, lu);
                union.extend(a);
            }
            *live = union;
            uses_expr(&scrutinee.node, scrutinee.span.start, live, lu);
        }
        Stmt::MetaConst { value, .. } => uses_expr(&value.node, value.span.start, live, lu),
        Stmt::MetaLog { args, .. } => {
            for a in args.iter().rev() {
                uses_expr(&a.node, a.span.start, live, lu);
            }
        }

        // ── Genuinely nothing to walk ───────────────────────────────────────
        // Listed EXPLICITLY rather than swept into `_ => {}` so that a new
        // `Stmt` variant is a COMPILE ERROR here instead of a silent
        // under-approximation — the same arm-count guard the `Expr` walker got.
        // `Item` is a NESTED DEFINITION with its own scope and its own liveness
        // run; it cannot reference an enclosing function's locals.
        Stmt::Break | Stmt::Continue | Stmt::Pass | Stmt::Item(_) | Stmt::Return(None) => {}
    }
}

/// Process an expression: for each Identifier, check if it's a last use.
fn uses_expr<'a>(
    expr: &'a Expr,
    span_start: usize,
    live: &mut FxHashSet<&'a str>,
    lu: &mut FxHashMap<usize, String>,
) {
    match expr {
        Expr::Identifier(name) => {
            if !live.contains(name.as_str()) {
                // Not in live set → this is the last use → record span AND name
                lu.insert(span_start, name.clone());
            }
            live.insert(name.as_str());
        }
        Expr::Call { callee, args, .. } => {
            // Process args right-to-left (reverse of evaluation order)
            for a in args.iter().rev() {
                uses_expr(&a.node.value.node, a.node.value.span.start, live, lu);
            }
            uses_expr(&callee.node, callee.span.start, live, lu);
        }
        Expr::MethodCall { receiver, args, .. } => {
            for a in args.iter().rev() {
                uses_expr(&a.node.value.node, a.node.value.span.start, live, lu);
            }
            uses_expr(&receiver.node, receiver.span.start, live, lu);
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::Index { object, index, .. } => {
            uses_expr(&index.node, index.span.start, live, lu);
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::BinaryOp { left, right, .. } => {
            uses_expr(&right.node, right.span.start, live, lu);
            uses_expr(&left.node, left.span.start, live, lu);
        }
        Expr::UnaryOp { operand, .. }
        | Expr::Move { expr: operand }
        | Expr::Propagate { expr: operand } => {
            uses_expr(&operand.node, operand.span.start, live, lu);
        }
        Expr::Block(block) => {
            walk_block(&block.stmts, live, lu);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            let saved = live.clone();
            let mut live_else = saved.clone();
            if let Some(b) = else_branch { uses_expr(&b.node, b.span.start, &mut live_else, lu); }
            let mut live_then = saved;
            uses_expr(&then_branch.node, then_branch.span.start, &mut live_then, lu);
            *live = live_then;
            live.extend(live_else);
            for (c, b) in elif_branches.iter().rev() {
                uses_expr(&b.node, b.span.start, live, lu);
                uses_expr(&c.node, c.span.start, live, lu);
            }
            uses_expr(&condition.node, condition.span.start, live, lu);
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            let saved = live.clone();
            let mut union: FxHashSet<&'a str> = FxHashSet::default();
            for arm in arms {
                let mut a = saved.clone();
                uses_expr(&arm.body.node, arm.body.span.start, &mut a, lu);
                kill_pattern(&arm.pattern, &mut a);
                union.extend(a);
            }
            if let Some(b) = else_arm {
                let mut a = saved;
                uses_expr(&b.node, b.span.start, &mut a, lu);
                union.extend(a);
            }
            *live = union;
            uses_expr(&scrutinee.node, scrutinee.span.start, live, lu);
        }
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            // Closures capture — treat as uses at the closure creation site.
            uses_expr(&body.node, span_start, live, lu);
        }
        Expr::Is { expr, .. }
        | Expr::Rethrow { expr, .. }
        | Expr::Await { expr, .. }
        | Expr::MutableBorrow { expr, .. }
        | Expr::Deref { expr, .. }
        | Expr::As { expr, .. } => {
            uses_expr(&expr.node, expr.span.start, live, lu);
        }
        Expr::Catch { expr, recovery, .. } => {
            uses_expr(&recovery.node, recovery.span.start, live, lu);
            uses_expr(&expr.node, expr.span.start, live, lu);
        }
        Expr::OptionalChain { object, .. } => {
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::Spawn { expr, .. } | Expr::SpawnBlocking { expr, .. } => {
            uses_expr(&expr.node, expr.span.start, live, lu);
        }
        Expr::ListComprehension { expr, variable, iterable, condition, .. } => {
            if let Some(c) = condition { uses_expr(&c.node, c.span.start, live, lu); }
            uses_expr(&expr.node, expr.span.start, live, lu);
            kill_pattern(variable, live);
            uses_expr(&iterable.node, iterable.span.start, live, lu);
        }
        Expr::SetComprehension { expr, variable, iterable, condition, .. } => {
            if let Some(c) = condition { uses_expr(&c.node, c.span.start, live, lu); }
            uses_expr(&expr.node, expr.span.start, live, lu);
            live.remove(variable.node.as_str());
            uses_expr(&iterable.node, iterable.span.start, live, lu);
        }
        Expr::StringLiteral(_, interp_exprs) => {
            // F-string interpolations are real uses of any locals they reference
            // (`f"x={items.get(0).unwrap()}"`). Without walking them, liveness
            // misses the use, and a preceding consuming-arg / field-store would
            // emit `move_zero` on a still-live source — the read inside the
            // interpolation then becomes a `local _N read after MoveZero` GIR
            // validation panic. Surfaced 2026-05-05 wiring field-assign through
            // `ensure_owned_at_consuming_arg`.
            for interp in interp_exprs {
                uses_expr(&interp.node, interp.span.start, live, lu);
            }
        }
        // Container literals — every element is a real use.
        Expr::ArrayLiteral(elems, _) | Expr::TupleLiteral(elems) => {
            for e in elems.iter().rev() {
                uses_expr(&e.node, e.span.start, live, lu);
            }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs.iter().rev() {
                uses_expr(&v.node, v.span.start, live, lu);
                uses_expr(&k.node, k.span.start, live, lu);
            }
        }
        Expr::DictComprehension { key, value, variables, iterable, condition } => {
            if let Some(c) = condition { uses_expr(&c.node, c.span.start, live, lu); }
            uses_expr(&value.node, value.span.start, live, lu);
            uses_expr(&key.node, key.span.start, live, lu);
            for v in variables { live.remove(v.node.as_str()); }
            uses_expr(&iterable.node, iterable.span.start, live, lu);
        }
        Expr::Range { start, end, .. } => {
            if let Some(e) = end { uses_expr(&e.node, e.span.start, live, lu); }
            if let Some(s) = start { uses_expr(&s.node, s.span.start, live, lu); }
        }
        Expr::DefaultOp { lhs, rhs } => {
            uses_expr(&rhs.node, rhs.span.start, live, lu);
            uses_expr(&lhs.node, lhs.span.start, live, lu);
        }
        Expr::Do { body, .. } => {
            walk_block(&body.stmts, live, lu);
        }
        // `Foo.bar(args)` shorthand — args are real uses. Variant tag is
        // a name path, not an identifier use.
        Expr::DotShorthand { args, .. } => {
            for a in args.iter().rev() {
                uses_expr(&a.node.value.node, a.node.value.span.start, live, lu);
            }
        }
        // Struct constructors `Foo(args)` — every arg is a real use.
        Expr::StructLiteral { args, .. } => {
            for a in args.iter().rev() {
                uses_expr(&a.node, a.span.start, live, lu);
            }
        }
        // `meta[op]` carries two real operand expressions.
        Expr::MetaOpInfix { left, right, .. } => {
            uses_expr(&right.node, right.span.start, live, lu);
            uses_expr(&left.node, left.span.start, live, lu);
        }

        // Leaves. Listed EXPLICITLY, not swept by `_ => {}`: this walker is
        // under-approximation-critical (a use it cannot see makes a variable
        // look dead, so the value is moved instead of cloned), so a new `Expr`
        // variant must be a COMPILE ERROR here rather than silently ignored.
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::SelfExpr
        | Expr::ReturnValue
        | Expr::Path { .. }
        | Expr::It
        | Expr::MetaOpToken(_) => {} // Literals, type names, Path, MetaOp*, It, etc.
    }
}

fn uses_target_sub<'a>(expr: &'a Expr, live: &mut FxHashSet<&'a str>, lu: &mut FxHashMap<usize, String>) {
    match expr {
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            uses_expr(&object.node, object.span.start, live, lu);
        }
        Expr::Index { object, index, .. } => {
            uses_expr(&index.node, index.span.start, live, lu);
            uses_expr(&object.node, object.span.start, live, lu);
        }
        _ => {}
    }
}

fn kill_pattern<'a>(pat: &'a Spanned<Pattern>, live: &mut FxHashSet<&'a str>) {
    match &pat.node {
        Pattern::Binding(name) => { live.remove(name.as_str()); }
        Pattern::Constructor { fields, .. } => { for f in fields { kill_pattern(f, live); } }
        Pattern::Tuple(elems) => { for e in elems { kill_pattern(e, live); } }
        _ => {}
    }
}

/// Every `on error:` body reachable from an EXPRESSION.
///
/// Routed through `parser::visitor::visit_expr_children` -- the one exhaustive
/// child enumeration -- so a new `Expr` variant that carries a block cannot
/// silently escape this walk the way `Expr::Do` did.
fn on_error_bodies_in_expr<'a>(e: &'a Expr) -> Vec<&'a [Spanned<Stmt>]> {
    let mut children: Vec<&'a Spanned<Expr>> = Vec::new();
    let mut blocks: Vec<&'a Block> = Vec::new();
    crate::parser::visitor::visit_expr_children(
        e,
        &mut |child: &'a Spanned<Expr>| children.push(child),
        &mut |b: &'a Block| blocks.push(b),
    );
    let mut out: Vec<&'a [Spanned<Stmt>]> = Vec::new();
    for b in blocks {
        for s in &b.stmts {
            out.extend(on_error_bodies_in(&s.node));
        }
    }
    for c in children {
        out.extend(on_error_bodies_in_expr(&c.node));
    }
    out
}
