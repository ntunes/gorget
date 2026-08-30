use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr, FunctionBody, FunctionDef, GenericParam, Ownership, Stmt, Type};
use crate::semantic::meta::{self as meta, DelayedMetaContext, MetaValue};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::lower_expr;
use super::generics;
use super::stmts::lower_block;

/// Assign an expression-body operand into the return slot _0, picking the
/// AssignMode that matches Phase C semantics: Move for resource-typed
/// bare-place sources (transfer ownership; the source is dead at function
/// exit anyway), Copy otherwise. Mirrors the use_move logic in
/// `lower_return` (stmts/mod.rs line ~1090).
fn assign_to_return_slot(
    ctx: &LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
) {
    use crate::ir::instructions::AssignMode;
    let mode = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
        let local_ty = builder.local_type(p.local);
        // Cluster 5 probe (2026-05-10): the disjunction
        // `needs_drop || is_resource_type` is NOT redundant. `needs_drop`
        // depends on TypeDef metadata set by `upgrade_types_from_fields`,
        // which may not have run at the point this code executes (or for
        // late-registered types). `is_resource_type` does a per-call
        // transitive struct-field scan that doesn't depend on upgrade scan.
        // For VectorIter[T] (struct containing Vector[T]), `is_resource_type`
        // returns true via the transitive scan, while `needs_drop` returns
        // false until the upgrade scan sets DropStrategy::Recursive.
        // Collapse to either alone regresses ~22 stdlib_iter / tensor /
        // vector_userspace_hofs fixtures with `[resource-moves]` violations
        // (shallow copy of resource _0 : VectorIter__int64_t).
        if p.projections.is_empty()
            && (ctx.type_registry.needs_drop(local_ty)
                || ctx.type_registry.is_resource_type(local_ty))
        {
            AssignMode::Move
        } else {
            AssignMode::Copy
        }
    } else {
        AssignMode::Copy
    };
    builder.assign_mode(mode, Place::local(LocalId(0)), operand);
}

/// Expression-body `throws` fn: wrap the tail value in `Ok(...)` so it matches
/// the function's `Result[T, E]` return slot.
///
/// The declared return type of an expr-body `throws` fn is the *unwrapped* `T`
/// (the typechecker views the tail against `T`, rejecting an explicit
/// `Ok(...)`/`Error(...)` tail), but the return slot `_0` is the `Result[T, E]`
/// (`ret_type`). Without this wrap the bare `T` is assigned straight into the
/// `Result` slot → ill-typed C. The block-body path wraps in `lower_return`,
/// and the `is_main && throws` arm in `lower_function` does the same; this
/// centralizes the wrap so every expr-body arm (`lower_function`,
/// `lower_equip_method`) routes through one site.
///
/// Auto-prop has already run inside `lower_expr` (the centralized hook), so a
/// tail `throws`-call operand arrives here as the unwrapped `T` — this wraps it
/// exactly once. The defensive `op_ty == ret_type` guard skips the wrap if the
/// operand is somehow already the function's `Result` type (no double-`Ok`).
fn wrap_expr_tail_in_ok(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
    ret_type: TypeId,
    throws: bool,
) -> Operand {
    if !throws {
        return operand;
    }
    let op_ty = super::exprs::infer_operand_type_full(ctx, &operand, builder);
    if op_ty == ret_type {
        return operand;
    }
    let type_name = ctx.type_registry.type_name(ret_type)
        .unwrap_or_else(|| "Result".to_string());
    // Core #3 — route the outer-Ok wrap through the CoW chokepoint.
    // `emit_enum_init_owned` clone-or-moves the payload per the CoW table and
    // `drops.unregister`s the consumed source, so an inner resource (e.g. a
    // `Result[String, …]` tail whose Ok holds a heap String) is NOT double-freed:
    // dropped once inside this fn AND again at the call site. Raw `builder.enum_init`
    // (shallow memcpy, no ownership transfer) was the sibling that forgot it
    // (the block-body path in `lower_return` does the transfer via
    // `move_zero_and_mark`). `drops.unregister` emits NO instruction, so the
    // `stmts/mod.rs:1812` move-zero drop-flag-corruption class on rethrow shapes
    // is structurally out of reach here. The operand is a fresh owned temp, so
    // `None` arg_spans is fine — `clone_resource_args_for_init` moves it regardless.
    let ok_val = ctx.emit_enum_init_owned(builder, &type_name, "Ok", ret_type, vec![operand], None);
    FunctionBuilder::copy(ok_val)
}

/// Public wrapper around `all_return_nominals_registered` for
/// cross-module callers (non-generic trait default emission in
/// `traits.rs` reuses the same demand-gate heuristic).
pub fn all_return_nominals_registered_pub(ctx: &LoweringContext, ty: &Type) -> bool {
    all_return_nominals_registered(ctx, ty)
}

/// Walk an AST type and return true only if every nominal (named struct /
/// enum) reachable through it has already been registered in the type
/// mapper. Used to demand-gate bulk default-method emission: if a
/// lifted-default adapter constructor like `TakeIter[Self, T] take(self,
/// int n)` substitutes to `TakeIter[X, int]` for some X where
/// `TakeIter__<mangled X>__int64_t` was never registered from a user
/// call site, skip the emission — otherwise we'd cascade into emitting
/// every adapter's `.take() / .skip() / …` for every Iterator
/// implementor forever.
fn all_return_nominals_registered(ctx: &LoweringContext, ty: &Type) -> bool {
    match ty {
        Type::Named { name, generic_args } => {
            // Primitive aliases / unused bare names pass through (they
            // mangle directly to int64_t / etc. and don't need a named-
            // type registration).
            if generic_args.is_empty() {
                return true;
            }
            // Mangled name for this instance — has to be registered on the type mapper.
            let mangled = super::types::mangle_generic_name(&name.node, generic_args);
            if ctx.type_mapper.lookup_named(&mangled).is_none() {
                return false;
            }
            // Recurse into generic args.
            generic_args.iter().all(|a| all_return_nominals_registered(ctx, &a.node))
        }
        Type::Tuple(elems) => elems.iter().all(|e| all_return_nominals_registered(ctx, &e.node)),
        Type::Ref(inner) | Type::Owned(inner) | Type::Pointer(inner) => {
            all_return_nominals_registered(ctx, &inner.node)
        }
        Type::Function { return_type, params, .. } => {
            all_return_nominals_registered(ctx, &return_type.node)
                && params.iter().all(|p| all_return_nominals_registered(ctx, &p.node))
        }
        Type::Array { element, .. } | Type::Slice { element } => {
            all_return_nominals_registered(ctx, &element.node)
        }
        Type::Primitive(_) | Type::SelfType | Type::Inferred => true,
    }
}

/// Pre-scan a function body to find variable names unsafe for CoW aliasing:
/// reassigned, !-moved, or used as RHS for Move-type VarDecls.
/// CoW skips aliasing for these because the LIR can't change local types
/// mid-function (static ref_locals), and MoveZero on a source invalidates aliases.
fn prescan_cow_unsafe_names(body: &[Spanned<Stmt>]) -> rustc_hash::FxHashSet<String> {
    let mut declared: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
    let mut unsafe_names: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
    prescan_block(body, &mut declared, &mut unsafe_names);
    unsafe_names
}

/// Pre-scan a function body for identifier names that are the TARGET of an
/// assignment (`x = …` / `x += …`) textually INSIDE a loop body (`for`/`while`/
/// `loop`, recursively — including nested scopes within a loop). Populates
/// `FunctionState::loop_reassigned_names`, read by `lower_call_arg`'s owning-`!`-
/// param fast-path to detect a loop-carried accumulator (`x = f(!x)`) and route
/// it through the temp-materialize path instead of the pointer-forward + whole-
/// slot MoveZero (which would trip the GIR "read after MoveZero" validator on the
/// back-edge reassignment). Bare param names only — projected targets (`p.f = …`,
/// `xs[i] = …`) don't rebind the slot the fast-path zeroes, so they're irrelevant.
fn prescan_loop_reassigned_names(body: &[Spanned<Stmt>]) -> rustc_hash::FxHashSet<String> {
    let mut names: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
    collect_loop_reassigned(body, false, &mut names);
    names
}

fn collect_loop_reassigned(
    stmts: &[Spanned<Stmt>],
    in_loop: bool,
    names: &mut rustc_hash::FxHashSet<String>,
) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Assign { target, .. } | Stmt::CompoundAssign { target, .. } => {
                if in_loop {
                    if let Expr::Identifier(name) = &target.node {
                        names.insert(name.clone());
                    }
                }
            }
            // Loop forms: everything nested becomes loop-carried.
            Stmt::While { body, else_body, .. } => {
                collect_loop_reassigned(&body.stmts, true, names);
                if let Some(eb) = else_body {
                    collect_loop_reassigned(&eb.stmts, in_loop, names);
                }
            }
            Stmt::For { body, else_body, .. } => {
                collect_loop_reassigned(&body.stmts, true, names);
                if let Some(eb) = else_body {
                    collect_loop_reassigned(&eb.stmts, in_loop, names);
                }
            }
            Stmt::Loop { body } => {
                collect_loop_reassigned(&body.stmts, true, names);
            }
            // Non-loop scope forms: propagate the current `in_loop` flag inward.
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                collect_loop_reassigned(&then_body.stmts, in_loop, names);
                for (_, b) in elif_branches {
                    collect_loop_reassigned(&b.stmts, in_loop, names);
                }
                if let Some(eb) = else_body {
                    collect_loop_reassigned(&eb.stmts, in_loop, names);
                }
            }
            Stmt::With { body, .. }
            | Stmt::NamedScope { body, .. }
            | Stmt::OnError { body } => {
                collect_loop_reassigned(&body.stmts, in_loop, names);
            }
            Stmt::Match { arms, else_arm, .. } => {
                for item in arms {
                    // MIRROR of the `liveness.rs` hole, same class, fixed with it:
                    // `item.arm()` drops `MatchItem::MetaFor` by construction, and
                    // narrowing the body to `Expr::Do` drops `Expr::Block`. Measured
                    // rc 101 `read after MoveZero` on both lanes when the accumulator
                    // is an owning `!`-param (a local does not fire this path).
                    let arm = match item {
                        crate::parser::ast::MatchItem::Arm(a) => a,
                        crate::parser::ast::MatchItem::MetaFor { arm_template, .. } => arm_template,
                    };
                    // Route through the one exhaustive child enumeration rather
                    // than narrowing to a variant list: `visit_expr_children`
                    // hands back every BLOCK an expression carries, at any depth,
                    // so a new block-carrying `Expr` cannot slip past this walk
                    // the way `Expr::Block` did when this narrowed to `Expr::Do`.
                    let mut blocks: Vec<&crate::parser::ast::Block> = Vec::new();
                    collect_blocks_in_expr(&arm.body.node, &mut blocks);
                    for b in blocks {
                        collect_loop_reassigned(&b.stmts, in_loop, names);
                    }
                }
                if let Some(eb) = else_arm {
                    collect_loop_reassigned(&eb.stmts, in_loop, names);
                }
            }
            Stmt::Select { arms, else_arm } => {
                for arm in arms {
                    collect_loop_reassigned(&arm.body.stmts, in_loop, names);
                }
                if let Some(eb) = else_arm {
                    collect_loop_reassigned(&eb.stmts, in_loop, names);
                }
            }

            // ── Previously swept by `_ => {}` (12 of 29 forms handled) ──────
            // A reassignment this walker cannot see is one the loop-carried
            // materializer will not hoist. Same failure class as the other
            // walkers made exhaustive this round.
            Stmt::MetaIf { then_body, elif_branches, else_body, .. } => {
                collect_loop_reassigned(&then_body.stmts, in_loop, names);
                for (_c, b) in elif_branches {
                    collect_loop_reassigned(&b.stmts, in_loop, names);
                }
                if let Some(b) = else_body { collect_loop_reassigned(&b.stmts, in_loop, names); }
            }
            Stmt::MetaFor { body, .. } | Stmt::MetaWhile { body, .. } => {
                collect_loop_reassigned(&body.stmts, in_loop, names);
            }
            Stmt::MetaMatch { arms, else_arm, .. } => {
                for (_c, b) in arms { collect_loop_reassigned(&b.stmts, in_loop, names); }
                if let Some(b) = else_arm { collect_loop_reassigned(&b.stmts, in_loop, names); }
            }

            // Carry no nested statement block, so nothing to collect. Listed
            // explicitly so a new statement kind is a compile error here.
            Stmt::VarDecl { .. } | Stmt::Expr(..) | Stmt::Return(..)
            | Stmt::Assert { .. } | Stmt::AssertReturn { .. } | Stmt::Throw(..)
            | Stmt::Snapshot { .. } | Stmt::MetaConst { .. } | Stmt::MetaLog { .. }
            | Stmt::Break | Stmt::Continue | Stmt::Pass | Stmt::Item(_) => {}
        }
    }
}

fn prescan_block(
    stmts: &[Spanned<Stmt>],
    declared: &mut rustc_hash::FxHashSet<String>,
    unsafe_names: &mut rustc_hash::FxHashSet<String>,
) {
    use crate::parser::ast::Pattern;
    for stmt in stmts {
        match &stmt.node {
            Stmt::VarDecl { pattern, value, .. } => {
                if let Pattern::Binding(name) = &pattern.node {
                    declared.insert(name.clone());
                }
                // Scan for !-moved args in the value expression
                prescan_expr_moves(&value.node, declared, unsafe_names);
            }
            Stmt::Assign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if declared.contains(name.as_str()) {
                        unsafe_names.insert(name.clone());
                    }
                }
                prescan_expr_moves(&value.node, declared, unsafe_names);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                if let Expr::Identifier(name) = &target.node {
                    if declared.contains(name.as_str()) {
                        unsafe_names.insert(name.clone());
                    }
                }
                prescan_expr_moves(&value.node, declared, unsafe_names);
            }
            Stmt::Expr(expr) => {
                prescan_expr_moves(&expr.node, declared, unsafe_names);
            }
            Stmt::Return(Some(expr)) => {
                prescan_expr_moves(&expr.node, declared, unsafe_names);
            }
            Stmt::While { body, else_body, .. } => {
                prescan_block(&body.stmts, declared, unsafe_names);
                if let Some(eb) = else_body {
                    prescan_block(&eb.stmts, declared, unsafe_names);
                }
            }
            Stmt::For { body, .. } => {
                prescan_block(&body.stmts, declared, unsafe_names);
            }
            Stmt::If { then_body, elif_branches, else_body, .. } => {
                prescan_block(&then_body.stmts, declared, unsafe_names);
                for (_, branch_body) in elif_branches {
                    prescan_block(&branch_body.stmts, declared, unsafe_names);
                }
                if let Some(eb) = else_body {
                    prescan_block(&eb.stmts, declared, unsafe_names);
                }
            }
            _ => {}
        }
    }
}

/// Scan an expression for `!name` (Move) arguments in function/method calls.
fn prescan_expr_moves(
    expr: &Expr,
    declared: &rustc_hash::FxHashSet<String>,
    unsafe_names: &mut rustc_hash::FxHashSet<String>,
) {
    match expr {
        Expr::Call { args, callee, .. } => {
            prescan_expr_moves(&callee.node, declared, unsafe_names);
            for arg in args {
                if matches!(arg.node.ownership, Ownership::Move) {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        if declared.contains(name.as_str()) {
                            unsafe_names.insert(name.clone());
                        }
                    }
                }
                prescan_expr_moves(&arg.node.value.node, declared, unsafe_names);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            prescan_expr_moves(&receiver.node, declared, unsafe_names);
            for arg in args {
                if matches!(arg.node.ownership, Ownership::Move) {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        if declared.contains(name.as_str()) {
                            unsafe_names.insert(name.clone());
                        }
                    }
                }
                prescan_expr_moves(&arg.node.value.node, declared, unsafe_names);
            }
        }
        Expr::BinaryOp { left, right, .. } => {
            prescan_expr_moves(&left.node, declared, unsafe_names);
            prescan_expr_moves(&right.node, declared, unsafe_names);
        }
        _ => {}
    }
}

/// Flow-sensitive CoW analysis: for each statement, compute the set of variable names
/// (and collection-path mutation markers) that render any CoW borrow sourced from
/// them unsafe on a forward execution path.
///
/// The set contains two shapes of entries:
///   - `"name"` — the variable `name` is reassigned or `!`-moved later. Any CoW
///     borrow of `name` would dangle if we kept it through the reassignment.
///   - `"@mut:path"` — the collection at `path` (a local name like `"x"` or a
///     field path like `"self.data"`) is mutated later. A mutating method call
///     (push/pop/set/insert/remove/clear/sort/reverse/swap/extend/append/truncate/
///     retain/dedup/resize/reserve/replace/drain/fill) on the receiver, a direct
///     field reassignment, or a call site that passes `&path` as a mut-borrow arg
///     all contribute. A CoW borrow of an *element* from this collection must be
///     eagerly materialized at its var_decl site if this marker is present.
///
/// Algorithm: reverse walk of the AST (same shape as liveness analysis). At each
/// statement, the set is the accumulation of all mutations/reassignments that
/// appear textually after it. At branches (if/else), the sets are unioned
/// (any-path: if a name is mutated in any branch, it's unsafe for CoW).
/// At loops, the loop body's mutations are included (conservative but correct).
fn compute_cow_reassigned_after(
    body: &[Spanned<Stmt>],
    info: &CowPrescan<'_>,
) -> rustc_hash::FxHashMap<usize, rustc_hash::FxHashSet<std::rc::Rc<str>>> {
    let mut result = rustc_hash::FxHashMap::default();
    let mut future: rustc_hash::FxHashSet<std::rc::Rc<str>> = rustc_hash::FxHashSet::default();
    let mut interner: rustc_hash::FxHashMap<String, std::rc::Rc<str>> = rustc_hash::FxHashMap::default();
    cow_after_block(body, &mut future, &mut result, info, &mut interner);
    result
}

/// CoW 2G — loop-carried bare-param mutation detection. Runs the SHARED prescan
/// collectors (`cow_after_block` + `cow_after_expr_moves`) FRESH over a single
/// loop's own statements (and, for `while`, its condition expr, which
/// re-executes every iteration), returning the UNION mutation-marker set.
///
/// This is deliberately NOT the recorded per-position `cow_reassigned_after` map
/// (`is_source_mut_unsafe_at`): that map is exclusive-of-self and suffix-scoped
/// (the reverse walk records each position's set BEFORE processing that stmt), so
/// a single-statement loop body returns EMPTY. A fresh union over the loop's own
/// statements is the only sound query for "does THIS loop mutate param P".
///
/// One source of truth (devbook/24): the answer comes from the exact same
/// collectors that drive the in-body materialize decision — no parallel AST
/// walker to drift.
pub(crate) fn cow_mutations_in_loop(
    body: &[Spanned<Stmt>],
    condition: Option<&Expr>,
    else_body: Option<&[Spanned<Stmt>]>,
    info: &CowPrescan<'_>,
) -> rustc_hash::FxHashSet<std::rc::Rc<str>> {
    let mut result = rustc_hash::FxHashMap::default();
    let mut future: rustc_hash::FxHashSet<std::rc::Rc<str>> = rustc_hash::FxHashSet::default();
    let mut interner: rustc_hash::FxHashMap<String, std::rc::Rc<str>> = rustc_hash::FxHashMap::default();
    cow_after_block(body, &mut future, &mut result, info, &mut interner);
    if let Some(cond) = condition {
        cow_after_expr_moves(cond, &mut future, info, &mut interner);
    }
    // Planner consumer #1 (loop-else): a bare-param mutation in a `for … else:` /
    // `while … else:` body has the IDENTICAL save/restore hole — the else body
    // lowers via `lower_block_scoped`, whose `restore_locals` reverts the in-body
    // materialize rebind. The pre-LOOP hoist (`materialize_loop_carried_bare_params`)
    // dominates the else exit, so folding the else body into the loop's pre-header
    // scan lets that single hoist cover it too (keeping `LoopPreHeaderMaterialize`).
    if let Some(eb) = else_body {
        cow_after_block(eb, &mut future, &mut result, info, &mut interner);
    }
    future
}

/// Planner consumer #1 (scope pre-header) — the drift-free scope analog of
/// `cow_mutations_in_loop`. Returns the UNION mutation-marker set that a whole
/// non-loop SCOPE statement (`if`/`with`/named-scope/`match`/`select`)
/// contributes, so its pre-scope hoist can materialize every bare param the scope
/// mutates on any path — before the scope's `save_locals`, so the post-scope read
/// sees the private copy on every path without a phi (the same write-site-hoist
/// logic as the loop pre-header, devbook/11 2G).
///
/// One source of truth (devbook/24): this IS the shared collector `cow_after_stmt`
/// run over the whole scope statement — NOT a hand-mirror of its per-form arms.
/// That is what keeps it drift-free: the collector's own `Stmt::If` / `Stmt::With`
/// / `Stmt::Match` / `Stmt::Select` arms already fold in the `if`/elif CONDITIONS,
/// the match GUARDS, the `with` BINDINGS, the arm-Expr-vs-Block bodies, the
/// `else` bodies, and any NESTED scopes — so a new sub-form the collector learns
/// to scan is covered here automatically, with no parallel walker to fall behind.
/// (The proto's earlier `cow_mutations_in_branches` hand-mirrored the branch-body
/// union and had already drifted by missing the elif conditions — the exact
/// Core #2/#3 failure this replacement retires.)
///
/// Including the `if` condition (which runs on the dominating path) is harmless:
/// the pre-scope rebind makes the at-site condition materialize a no-op — no
/// double clone. Over-approximation costs one extra clone; under-approximation
/// revives the thrown-away private copy.
pub(crate) fn cow_mutations_in_stmt(
    stmt: &Stmt,
    info: &CowPrescan<'_>,
) -> rustc_hash::FxHashSet<std::rc::Rc<str>> {
    let mut result = rustc_hash::FxHashMap::default();
    let mut future: rustc_hash::FxHashSet<std::rc::Rc<str>> = rustc_hash::FxHashSet::default();
    let mut interner: rustc_hash::FxHashMap<String, std::rc::Rc<str>> = rustc_hash::FxHashMap::default();
    cow_after_stmt(stmt, &mut future, &mut result, info, &mut interner);
    future
}

/// CoW 2G — does the loop-mutation set produced by `cow_mutations_in_loop` mark
/// the bare-param root `name` as mutated? Matches a bare name (reassignment /
/// `!`-move / dotless index-or-tuple target), an exact `@mut:name` receiver
/// mutation, OR any `@mut:name.…` projection mutation (`self.items.push()` marks
/// `@mut:self.items`, which the pre-header materialize of `self` must catch).
pub(crate) fn loop_set_mutates(
    set: &rustc_hash::FxHashSet<std::rc::Rc<str>>,
    name: &str,
) -> bool {
    if set.contains(name) {
        return true;
    }
    let direct = format!("@mut:{}", name);
    if set.contains(direct.as_str()) {
        return true;
    }
    let prefix = format!("@mut:{}.", name);
    set.iter().any(|e| e.starts_with(prefix.as_str()))
}

/// Intern a `&str` as `Rc<str>`. Subsequent inserts of the same name reuse the
/// existing `Rc` cell, which lets `future.clone()` propagate refcount bumps
/// instead of allocating fresh `String`s per entry.
#[inline]
fn intern_rc(s: &str, interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>) -> std::rc::Rc<str> {
    if let Some(rc) = interner.get(s) {
        return rc.clone();
    }
    let rc: std::rc::Rc<str> = std::rc::Rc::from(s);
    interner.insert(s.to_string(), rc.clone());
    rc
}

/// The typed side tables the CoW mutation prescan reads. Bundled so the
/// collectors below take ONE parameter instead of a growing tuple — and, more
/// to the point, so that "does this call mutate its receiver" has exactly one
/// entry point (`Self::call_mutates_receiver`) rather than an open-coded test
/// at each of the collector's arms.
pub(crate) struct CowPrescan<'a> {
    /// Free-function name -> declared per-param ownership. Drives implicit
    /// mut-borrow ARGUMENT detection (`f(x)` where `f` declares `T &x`).
    pub(crate) fn_param_ownerships: &'a rustc_hash::FxHashMap<String, Vec<Ownership>>,
    /// Method-call span -> does this call write through its RECEIVER, resolved
    /// once by the semantic pass. See
    /// [`crate::semantic::safety::ReceiverMutations`].
    pub(crate) receiver_mutations: &'a crate::semantic::safety::ReceiverMutations,
}

impl CowPrescan<'_> {
    /// Does `receiver.method(...)` mutate its receiver — and therefore
    /// contribute `@mut:{receiver_path}` to the prescan set?
    ///
    /// The answer is READ, not re-derived: the semantic pass already resolved
    /// this call site's receiver to a type and its method to a `DefId`, and
    /// classified it from the receiver's typed builtin protocol plus the
    /// method's declared `&self` (`safety/check_expr.rs`, the
    /// `receiver_is_mutating` binding). Layering rule 4.
    ///
    /// This function must never consult `method.node`'s TEXT to decide whether
    /// a USER method mutates. That is what it used to do — a 25-entry
    /// `MUTATING_METHODS` hand list — and it made a user `&self` mutator's
    /// NAME decide memory safety: `void grow(&self)` segfaulted where a
    /// byte-identical `void resize(&self)` was correct, because `resize` was on
    /// the list and `grow` was not (`todo/t0699`, both backends). The
    /// regression net is `user_mutator_method_name_decides_memory_safety.gg`
    /// and `cow_user_mutator_two_types_same_name.gg`.
    ///
    /// ── The UNCLASSIFIED case ───────────────────────────────────────────
    ///
    /// `None` means the semantic pass never reached this call. Two classes do
    /// that, both structural, both measurable with `GG_REPORT_RECV_MUT=1`
    /// (which prints one `[recv-mut-miss] <name>` per unclassified query):
    ///
    ///   1. **Nodes the lowering itself synthesized** — iterator-pipeline
    ///      spines, f-string `display`/`debug` calls, derive bodies. They did
    ///      not exist when the semantic pass ran.
    ///   2. **`meta`-expanded bodies** — most sharply a `meta for` generating
    ///      MATCH ARMS, whose pattern binding has no resolved type until
    ///      expansion, which happens here in the lowering.
    ///
    /// The answer for those is the SELF-HOST's, and the self-host is explicit
    /// that the other direction is the unsound one (`lower.gg`,
    /// `method_mutates_receiver` step 3: *"a name that is NEITHER a registered
    /// user method on the resolved receiver type NOR in the builtin table is
    /// treated as MUTATING. Over-clones, never UAFs. (The `fill` bug direction
    /// — absent ⇒ read-only — is the unsound one and is NOT used.)"*).
    ///
    /// This is not theory. Measured on the pre-fix compiler, a user `&self`
    /// mutator called from a `meta for`-generated match arm was rescued only
    /// by its NAME being on the retired hand list: `resize` returned 0 and
    /// `grow` SIGSEGV'd, the same rename-decides-memory-safety defect
    /// (`todo/t0699`) one layer in. Answering `false` here would have kept
    /// that hole open for both names. Fixture:
    /// `cow_user_mutator_meta_generated_arm.gg`.
    ///
    /// Over-approximating costs an extra private copy of a bare-param
    /// receiver; under-approximating frees a buffer a live bind points at.
    /// This collector's own contract three screens up says the same thing in
    /// its own words — *"under-approximation is the bug — over-approx is
    /// safe"*.
    fn call_mutates_receiver(&self, method: &Spanned<String>) -> bool {
        match self
            .receiver_mutations
            .call_mutates_receiver(method.span.start)
        {
            Some(mutates) => mutates,
            None => {
                if std::env::var_os("GG_REPORT_RECV_MUT").is_some() {
                    eprintln!("[recv-mut-miss] {}", method.node);
                }
                true
            }
        }
    }
}

/// Extract the dotted path from a place expression (field/tuple-field/index
/// chain or identifier). Returns `"self.data"` for `self.data`, `"x"` for `x`,
/// `"xs.0"` for `xs.0`, and — for an index — the dotted-path prefix ABOVE the
/// index (`self.data[i]` → `"self.data"`, `xs[i]` → `"xs"`), since mutating one
/// element invalidates borrows of the whole collection. None for complex roots.
///
/// The caller decides how to record the result: `record_path_mutation` (mutating
/// RECEIVERS) emits `@mut:{path}` unconditionally, so a dotless root like `xs`
/// becomes `@mut:xs`; the Assign/CompoundAssign TARGET path (`record_place_target`)
/// splits on the dot so a dotless bare-param index/tuple target inserts the bare
/// name (the DOTLESS-ROOT insert — the Assign arm otherwise drops it).
fn extract_path_for_mut(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Identifier(name) => Some(name.clone()),
        Expr::SelfExpr => Some("self".to_string()),
        Expr::FieldAccess { object, field } => {
            let prefix = extract_path_for_mut(&object.node)?;
            Some(format!("{}.{}", prefix, field.node))
        }
        Expr::TupleFieldAccess { object, index } => {
            let prefix = extract_path_for_mut(&object.node)?;
            Some(format!("{}.{}", prefix, index))
        }
        // Peel to the deepest dotted-path prefix ABOVE the index. `resolve_
        // projection_root_local` (exprs/mod.rs) peels Index the same way at the
        // in-body materialize sites (`lower_index_assign`, the projected mutating
        // receiver), so recording the prefix here keeps the prescan in step.
        Expr::Index { object, .. } => extract_path_for_mut(&object.node),
        // Peel a method-call CHAIN to its root the same way `Index` is peeled.
        // `outer.get(0).unwrap().push(x)` mutates storage reachable from
        // `outer`, but the receiver is an unnamed temp, so without this the
        // whole mutation was DROPPED (`_ => None` -> `record_path_mutation`
        // records nothing) and a live view into it was never rescued: rc 139 on
        // both backends while the equivalent `first.push(x)` and
        // `outer[0].push(x)` spellings were clean. Recording the root is the
        // write-site fix (Core #1); the read side needed no per-shape rule.
        Expr::MethodCall { receiver, .. } => extract_path_for_mut(&receiver.node),

        // ── No path to record ───────────────────────────────────────────────
        // Listed EXPLICITLY rather than swept into `_ => None`. The catch-all
        // here was a live defect: a chain-spelled mutation receiver returned
        // None, so `record_path_mutation` recorded NOTHING and a live view was
        // never rescued (rc 139 both backends). A new `Expr` variant must now
        // force a decision at this site rather than silently defaulting to
        // "not a mutable path" — the dangerous direction.
        Expr::ArrayLiteral(..)
        | Expr::As { .. }
        | Expr::Await { .. }
        | Expr::BinaryOp { .. }
        | Expr::Block(..)
        | Expr::BoolLiteral(..)
        | Expr::Call { .. }
        | Expr::Catch { .. }
        | Expr::Closure { .. }
        | Expr::DefaultOp { .. }
        | Expr::Deref { .. }
        | Expr::DictComprehension { .. }
        | Expr::DictLiteral(..)
        | Expr::Do { .. }
        | Expr::DotShorthand { .. }
        | Expr::FloatLiteral(..)
        | Expr::If { .. }
        | Expr::ImplicitClosure { .. }
        | Expr::IntLiteral(..)
        | Expr::Is { .. }
        | Expr::It
        | Expr::ListComprehension { .. }
        | Expr::Match { .. }
        | Expr::MetaOpInfix { .. }
        | Expr::MetaOpToken(..)
        | Expr::Move { .. }
        | Expr::MutableBorrow { .. }
        | Expr::NoneLiteral
        | Expr::OptionalChain { .. }
        | Expr::Path { .. }
        | Expr::Propagate { .. }
        | Expr::Range { .. }
        | Expr::Rethrow { .. }
        | Expr::ReturnValue
        | Expr::SetComprehension { .. }
        | Expr::Spawn { .. }
        | Expr::SpawnBlocking { .. }
        | Expr::StringLiteral(..)
        | Expr::StructLiteral { .. }
        | Expr::TupleLiteral(..)
        | Expr::UnaryOp { .. } => None,
    }
}

/// Record the mutation marker(s) for an assignment / compound-assignment TARGET
/// place expression (`x = …`, `xs[i] = …`, `self.data = …`, `self.data[i] = …`,
/// `xs.0 = …`). Splits on whether the peeled place path has a dot:
/// - dotted (`self.data`) → `@mut:self.data` (invalidates borrows of the field
///   path and, via `is_source_mut_unsafe_at`'s ancestor walk, its projections);
/// - dotless (`x`, `xs` from `xs[i]`) → the bare name (the DOTLESS-ROOT insert:
///   a bare-param index/tuple target peels to a dotless root the `@mut:` path
///   would otherwise drop, leaving the loop-carried materialize blind).
fn record_place_target(
    target: &Expr,
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    if let Some(path) = extract_path_for_mut(target) {
        if path.contains('.') {
            let marker = format!("@mut:{}", path);
            future.insert(intern_rc(&marker, interner));
        } else {
            future.insert(intern_rc(&path, interner));
        }
    }
}

/// Helper: given an `expr` that appears at a mutating-position (the receiver of a
/// mutating method, or the inside of `&arg`), insert the right marker into the
/// prescan set.
fn record_path_mutation(
    expr: &Expr,
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    if let Some(path) = extract_path_for_mut(expr) {
        let marker = format!("@mut:{}", path);
        future.insert(intern_rc(&marker, interner));
    }
}

fn cow_after_block(
    stmts: &[Spanned<Stmt>],
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    result: &mut rustc_hash::FxHashMap<usize, rustc_hash::FxHashSet<std::rc::Rc<str>>>,
    info: &CowPrescan<'_>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    for stmt in stmts.iter().rev() {
        // Record the "reassigned-after" set at this statement's position.
        // Cloning `FxHashSet<Rc<str>>` is a per-entry refcount bump — cheap
        // compared to the prior `FxHashSet<String>` clone which reallocated
        // every entry. This is the dominant per-statement cost.
        result.insert(stmt.span.start, future.clone());
        // Collect reassignments from this statement
        cow_after_stmt(&stmt.node, future, result, info, interner);
    }
}

fn cow_after_stmt(
    stmt: &Stmt,
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    result: &mut rustc_hash::FxHashMap<usize, rustc_hash::FxHashSet<std::rc::Rc<str>>>,
    info: &CowPrescan<'_>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    match stmt {
        Stmt::VarDecl { value, .. } => {
            cow_after_expr_moves(&value.node, future, info, interner);
        }
        Stmt::Assign { target, value, .. } => {
            // Target place reassignment: `x = rhs` marks `x`; `self.data = rhs`
            // emits `@mut:self.data`; `xs[i] = rhs` / `xs.0 = rhs` mark the
            // dotless bare root `xs` (see `record_place_target`).
            record_place_target(&target.node, future, interner);
            // A mutating call can also hide in the TARGET's index/receiver
            // sub-expr (`ys[xs.pop()] = 0` mutates `xs` inside the target).
            cow_after_expr_moves(&target.node, future, info, interner);
            cow_after_expr_moves(&value.node, future, info, interner);
        }
        Stmt::CompoundAssign { target, value, .. } => {
            record_place_target(&target.node, future, interner);
            cow_after_expr_moves(&target.node, future, info, interner);
            cow_after_expr_moves(&value.node, future, info, interner);
        }
        Stmt::Expr(expr) => {
            cow_after_expr_moves(&expr.node, future, info, interner);
        }
        Stmt::Return(Some(expr)) => {
            cow_after_expr_moves(&expr.node, future, info, interner);
        }
        // `throw expr` can carry a mutating call (`throw make(xs.pop())`).
        Stmt::Throw(expr) => {
            cow_after_expr_moves(&expr.node, future, info, interner);
        }
        Stmt::While { condition, body, else_body } => {
            // The condition re-executes every iteration — a mutation there
            // (`while shrink(&xs):`) is loop-carried with the body.
            cow_after_expr_moves(&condition.node, future, info, interner);
            // Loop body reassignments are visible to statements before the loop
            cow_after_block(&body.stmts, future, result, info, interner);
            if let Some(eb) = else_body {
                cow_after_block(&eb.stmts, future, result, info, interner);
            }
        }
        Stmt::For { iterable, body, else_body, .. } => {
            // The iterable expr is evaluated once, but a mutating call inside it
            // (`for x in drain(&xs):`) is still a forward mutation w.r.t. earlier
            // statements. The `else_body` was previously dropped (unlike While).
            cow_after_expr_moves(&iterable.node, future, info, interner);
            cow_after_block(&body.stmts, future, result, info, interner);
            if let Some(eb) = else_body {
                cow_after_block(&eb.stmts, future, result, info, interner);
            }
        }
        Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
            // The if/elif conditions execute unconditionally on entry — scan them
            // for mutating calls (previously only the branch BODIES were scanned).
            cow_after_expr_moves(&condition.node, future, info, interner);
            for (elif_cond, _) in elif_branches {
                cow_after_expr_moves(&elif_cond.node, future, info, interner);
            }
            // Union all branches: if a name is reassigned in any branch, it's unsafe
            let saved = future.clone();
            cow_after_block(&then_body.stmts, future, result, info, interner);
            let then_set = future.clone();
            *future = saved.clone();
            for (_, branch_body) in elif_branches {
                cow_after_block(&branch_body.stmts, future, result, info, interner);
                // Accumulate into future (union)
            }
            if let Some(eb) = else_body {
                cow_after_block(&eb.stmts, future, result, info, interner);
            }
            // Union: include then-branch reassignments too
            future.extend(then_set);
        }
        Stmt::Match { scrutinee, arms, else_arm } => {
            // The scrutinee evaluates on entry (`match xs.pop():`).
            cow_after_expr_moves(&scrutinee.node, future, info, interner);
            let saved = future.clone();
            for item in arms {
                // A `MetaFor` item carries an `arm_template` whose body IS emitted by the
                // meta expansion, and these walkers run on the UNEXPANDED AST. Dropping
                // it walks that body blind (Core #4: the liveness sibling of this shape
                // was measured rc 0 GARBAGE on both backends).
                let arm = match item {
                    crate::parser::ast::MatchItem::Arm(a) => a,
                    crate::parser::ast::MatchItem::MetaFor { arm_template, .. } => arm_template,
                };
                {
                    let mut branch = saved.clone();
                    // A guard runs before the arm body (`case p if xs.pop() > 0:`).
                    if let Some(guard) = &arm.guard {
                        cow_after_expr_moves(&guard.node, &mut branch, info, interner);
                    }
                    // Arm bodies may be a Block OR a bare expression — the latter
                    // was previously skipped (`case p: xs.pop()`).
                    if let Expr::Block(block) = &arm.body.node {
                        cow_after_block(&block.stmts, &mut branch, result, info, interner);
                    } else {
                        cow_after_expr_moves(&arm.body.node, &mut branch, info, interner);
                    }
                    future.extend(branch);
                }
            }
            if let Some(b) = else_arm {
                let mut branch = saved;
                cow_after_block(&b.stmts, &mut branch, result, info, interner);
                future.extend(branch);
            }
        }
        Stmt::Loop { body } => {
            cow_after_block(&body.stmts, future, result, info, interner);
        }
        // Block-bearing scope forms: a mutating method / reassignment inside a
        // `with` / named-scope / bare block body is still a forward
        // mutation w.r.t. statements BEFORE the block. Omitting these left every
        // such mutation invisible to the prescan — a CoW element borrow taken
        // before a `with`-block that mutates its source collection would dangle
        // (the in-block `cow_before_mutation` materialise doesn't survive the
        // block's save/restore boundary, so the eager-clone at var-decl is the
        // correct severance — and it only fires when `is_source_mut_unsafe_at`
        // sees the `@mut:` marker recorded here).
        Stmt::With { bindings, body } => {
            cow_after_block(&body.stmts, future, result, info, interner);
            for b in bindings {
                cow_after_expr_moves(&b.expr.node, future, info, interner);
            }
        }
        Stmt::NamedScope { body, .. } | Stmt::OnError { body } => {
            cow_after_block(&body.stmts, future, result, info, interner);
        }
        // `select` multiplexes over channel ops; each arm body (and the optional
        // `else` arm) can mutate a source collection just like a `match` arm.
        // Treating select as a union of its arm bodies keeps the prescan honest
        // (mirrors the `Match` arm above; `SelectArm { op, body: Block, .. }`).
        Stmt::Select { arms, else_arm } => {
            use crate::parser::ast::SelectOp;
            // Each arm's channel OP runs on entry: `send` mutates the channel
            // (typed-mutating), and either op's channel/value expr can carry a
            // nested mutating call. These execute unconditionally w.r.t. the arm
            // bodies, so record them on the outer `future`, not a per-arm branch.
            for arm in arms {
                match &arm.op {
                    SelectOp::Send { channel, value } => {
                        record_path_mutation(&channel.node, future, interner);
                        cow_after_expr_moves(&channel.node, future, info, interner);
                        cow_after_expr_moves(&value.node, future, info, interner);
                    }
                    SelectOp::Recv { channel, .. } => {
                        cow_after_expr_moves(&channel.node, future, info, interner);
                    }
                }
            }
            let saved = future.clone();
            for arm in arms {
                let mut branch = saved.clone();
                cow_after_block(&arm.body.stmts, &mut branch, result, info, interner);
                future.extend(branch);
            }
            if let Some(eb) = else_arm {
                let mut branch = saved;
                cow_after_block(&eb.stmts, &mut branch, result, info, interner);
                future.extend(branch);
            }
        }

        // ── Forms below were `_ => {}` until 2026-08-27 ──────────────────────
        // The prescan under-approximated: a mutation spelled inside one of
        // these statements was never recorded, so a live view into the mutated
        // collection was not rescued. `assert grow(&v) == 1` in a realloc loop
        // was rc 139 on BOTH backends against an rc-0 control differing only in
        // statement form. Over-approximation here is merely a clone.
        //
        // ⚠ The lint `cow_after_stmt_covers_block_bearing_variants` was GREEN
        // throughout, because its subject is BLOCK-bearing variants and
        // `Assert` is EXPRESSION-bearing. No widening of its list could have
        // caught this — the subject was the wrong set (Core #15e Q4). The
        // exhaustive match below is what actually retires the class: a new
        // `Stmt` variant is now a COMPILE ERROR here.
        Stmt::Assert { condition, message, .. }
        | Stmt::AssertReturn { condition, message } => {
            cow_after_expr_moves(&condition.node, future, info, interner);
            if let Some(m) = message {
                cow_after_expr_moves(&m.node, future, info, interner);
            }
        }
        Stmt::Snapshot { value, .. } | Stmt::MetaConst { value, .. } => {
            cow_after_expr_moves(&value.node, future, info, interner);
        }
        Stmt::MetaLog { args, .. } => {
            for a in args {
                cow_after_expr_moves(&a.node, future, info, interner);
            }
        }
        Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
            cow_after_expr_moves(&condition.node, future, info, interner);
            let saved = future.clone();
            let mut branch = saved.clone();
            cow_after_block(&then_body.stmts, &mut branch, result, info, interner);
            future.extend(branch);
            for (cond, body) in elif_branches {
                cow_after_expr_moves(&cond.node, future, info, interner);
                let mut b = saved.clone();
                cow_after_block(&body.stmts, &mut b, result, info, interner);
                future.extend(b);
            }
            if let Some(eb) = else_body {
                let mut b = saved;
                cow_after_block(&eb.stmts, &mut b, result, info, interner);
                future.extend(b);
            }
        }
        Stmt::MetaFor { range: e, body, .. } | Stmt::MetaWhile { condition: e, body, .. } => {
            cow_after_expr_moves(&e.node, future, info, interner);
            cow_after_block(&body.stmts, future, result, info, interner);
        }
        Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
            cow_after_expr_moves(&scrutinee.node, future, info, interner);
            let saved = future.clone();
            for (_case, body) in arms {
                let mut b = saved.clone();
                cow_after_block(&body.stmts, &mut b, result, info, interner);
                future.extend(b);
            }
            if let Some(eb) = else_arm {
                let mut b = saved;
                cow_after_block(&eb.stmts, &mut b, result, info, interner);
                future.extend(b);
            }
        }

        // Nothing to walk. Listed explicitly so a new variant cannot be swept
        // up silently. `Item` is a nested definition with its own scope.
        Stmt::Break | Stmt::Continue | Stmt::Pass | Stmt::Item(_) | Stmt::Return(None) => {}
    }
}

/// The shared CALL-ARGUMENT walk, used by every call-shaped node (`Call`,
/// `MethodCall`, `DotShorthand`). One copy, so a new call-shaped variant cannot
/// pick up three-quarters of the rules — the drift this file already suffered
/// once (Core #4: fix the class, and centralize at the producer).
///
/// `callee_sig` is the callee's per-parameter ownership list where it is known
/// (only an `Expr::Identifier` callee maps cleanly to a signature); pass `None`
/// when it is not, which disables implicit-mut-borrow detection but nothing else.
fn cow_after_call_args(
    args: &[Spanned<crate::parser::ast::CallArg>],
    callee_sig: Option<&Vec<Ownership>>,
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    info: &CowPrescan<'_>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    for (i, arg) in args.iter().enumerate() {
        // `!arg` — the name is moved away.
        if matches!(arg.node.ownership, Ownership::Move) {
            if let Expr::Identifier(name) = &arg.node.value.node {
                future.insert(intern_rc(name.as_str(), interner));
            }
        }
        // Explicit `&arg` at the call site -> mutating.
        if matches!(arg.node.ownership, Ownership::MutableBorrow) {
            record_path_mutation(&arg.node.value.node, future, interner);
        }
        // Implicit mut-borrow: the callee's parameter is MutableBorrow per sig.
        if let Some(ownerships) = callee_sig {
            if let Some(Ownership::MutableBorrow) = ownerships.get(i) {
                record_path_mutation(&arg.node.value.node, future, interner);
            }
        }
        cow_after_expr_moves(&arg.node.value.node, future, info, interner);
    }
}

/// Collect !-moved names, mutating-method receivers, field reassignments, and
/// indirect-mutation (&arg / mut-borrow sig) arg targets from expressions.
fn cow_after_expr_moves(
    expr: &Expr,
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    info: &CowPrescan<'_>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    match expr {
        Expr::Call { args, callee, .. } => {
            cow_after_expr_moves(&callee.node, future, info, interner);
            // Look up the callee's per-param ownership for implicit-mut-borrow
            // detection. Only Expr::Identifier callees map cleanly to signatures.
            let callee_sig = if let Expr::Identifier(name) = &callee.node {
                info.fn_param_ownerships.get(name)
            } else {
                None
            };
            cow_after_call_args(args, callee_sig, future, info, interner);
        }
        Expr::MethodCall { receiver, args, method, .. } => {
            cow_after_expr_moves(&receiver.node, future, info, interner);
            // Mutating method on a receiver -> the receiver path is mutated.
            // TYPED, per receiver, resolved once by the semantic pass — the
            // method's NAME is not consulted (`todo/t0699`).
            if info.call_mutates_receiver(method) {
                record_path_mutation(&receiver.node, future, interner);
            }
            cow_after_call_args(args, None, future, info, interner);
        }
        Expr::BinaryOp { left, right, .. } => {
            cow_after_expr_moves(&left.node, future, info, interner);
            cow_after_expr_moves(&right.node, future, info, interner);
        }
        // Recurse into place-projection sub-exprs so a mutating call nested in an
        // index / field object (`total + arr[xs.pop()]`, `pair.0.f(xs.pop())`) is
        // not missed (under-approximation is the bug — over-approx is safe).
        Expr::Index { object, index } => {
            cow_after_expr_moves(&object.node, future, info, interner);
            cow_after_expr_moves(&index.node, future, info, interner);
        }
        Expr::FieldAccess { object, .. }
        | Expr::TupleFieldAccess { object, .. } => {
            cow_after_expr_moves(&object.node, future, info, interner);
        }

        // ── Sub-expression carriers ─────────────────────────────────────────
        // Every arm below existed only as `_ => {}` until 2026-08-27. This
        // function's contract is stated three lines up — "under-approximation
        // is the bug, over-approx is safe" — and the catch-all under-
        // approximated 41 of the 47 `Expr` variants, so a mutation spelled
        // anywhere but a call / binop / index / field was invisible to the CoW
        // prescan and its rescue was never emitted.
        Expr::UnaryOp { operand: e, .. } => {
            cow_after_expr_moves(&e.node, future, info, interner);
        }
        Expr::Move { expr: e }
        | Expr::Propagate { expr: e }
        | Expr::MutableBorrow { expr: e }
        | Expr::Deref { expr: e }
        | Expr::As { expr: e, .. }
        | Expr::Await { expr: e, .. }
        | Expr::Spawn { expr: e, .. }
        | Expr::SpawnBlocking { expr: e, .. }
        | Expr::Is { expr: e, .. }
        | Expr::OptionalChain { object: e, .. }
        | Expr::ImplicitClosure { body: e } => {
            cow_after_expr_moves(&e.node, future, info, interner);
        }
        Expr::DefaultOp { lhs, rhs } | Expr::MetaOpInfix { left: lhs, right: rhs, .. } => {
            cow_after_expr_moves(&lhs.node, future, info, interner);
            cow_after_expr_moves(&rhs.node, future, info, interner);
        }
        Expr::Range { start, end, .. } => {
            if let Some(e) = start {
                cow_after_expr_moves(&e.node, future, info, interner);
            }
            if let Some(e) = end {
                cow_after_expr_moves(&e.node, future, info, interner);
            }
        }
        Expr::Rethrow { expr, transform, .. } => {
            cow_after_expr_moves(&expr.node, future, info, interner);
            cow_after_expr_moves(&transform.node, future, info, interner);
        }
        Expr::Catch { expr, recovery, .. } => {
            cow_after_expr_moves(&expr.node, future, info, interner);
            cow_after_expr_moves(&recovery.node, future, info, interner);
        }
        Expr::If { condition, then_branch, elif_branches, else_branch } => {
            cow_after_expr_moves(&condition.node, future, info, interner);
            cow_after_expr_moves(&then_branch.node, future, info, interner);
            for (c, b) in elif_branches {
                cow_after_expr_moves(&c.node, future, info, interner);
                cow_after_expr_moves(&b.node, future, info, interner);
            }
            if let Some(b) = else_branch {
                cow_after_expr_moves(&b.node, future, info, interner);
            }
        }
        Expr::Match { scrutinee, arms, else_arm } => {
            cow_after_expr_moves(&scrutinee.node, future, info, interner);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    cow_after_expr_moves(&g.node, future, info, interner);
                }
                cow_after_expr_moves(&arm.body.node, future, info, interner);
            }
            if let Some(e) = else_arm {
                cow_after_expr_moves(&e.node, future, info, interner);
            }
        }
        // A closure BODY mutating a captured collection is a mutation of that
        // collection, wherever the closure later runs — the capture is what
        // makes it reachable. `t0704` is this arm's absence.
        Expr::Closure { body, .. } => {
            cow_after_expr_moves(&body.node, future, info, interner);
        }
        Expr::Block(block) => {
            cow_after_nested_block(&block.stmts, future, info, interner);
        }
        Expr::Do { body, .. } => {
            cow_after_nested_block(&body.stmts, future, info, interner);
        }
        Expr::ListComprehension { expr, iterable, condition, .. } => {
            cow_after_expr_moves(&expr.node, future, info, interner);
            cow_after_expr_moves(&iterable.node, future, info, interner);
            if let Some(c) = condition {
                cow_after_expr_moves(&c.node, future, info, interner);
            }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            cow_after_expr_moves(&key.node, future, info, interner);
            cow_after_expr_moves(&value.node, future, info, interner);
            cow_after_expr_moves(&iterable.node, future, info, interner);
            if let Some(c) = condition {
                cow_after_expr_moves(&c.node, future, info, interner);
            }
        }
        Expr::SetComprehension { expr, iterable, condition, .. } => {
            cow_after_expr_moves(&expr.node, future, info, interner);
            cow_after_expr_moves(&iterable.node, future, info, interner);
            if let Some(c) = condition {
                cow_after_expr_moves(&c.node, future, info, interner);
            }
        }
        Expr::ArrayLiteral(items, _) | Expr::TupleLiteral(items) => {
            for e in items {
                cow_after_expr_moves(&e.node, future, info, interner);
            }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                cow_after_expr_moves(&k.node, future, info, interner);
                cow_after_expr_moves(&v.node, future, info, interner);
            }
        }
        Expr::StructLiteral { args, .. } => {
            for e in args {
                cow_after_expr_moves(&e.node, future, info, interner);
            }
        }
        // Call-shaped args, so the same ownership handling as `Expr::Call`.
        Expr::DotShorthand { args, .. } => {
            cow_after_call_args(args, None, future, info, interner);
        }
        // An f-string's interpolations are real expressions: `f"{v.pop()}"`
        // mutates `v`.
        Expr::StringLiteral(_, interpolations) => {
            for e in interpolations {
                cow_after_expr_moves(&e.node, future, info, interner);
            }
        }

        // ── Leaves ──────────────────────────────────────────────────────────
        // Listed EXPLICITLY rather than swept up by `_ => {}` so that adding an
        // `Expr` variant is a COMPILE ERROR here instead of a silent
        // under-approximation. This is the arm-count guard the Layering doc asks
        // for, enforced by the type system rather than by a lint (Core #4/#10).
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::Identifier(_)
        | Expr::SelfExpr
        | Expr::ReturnValue
        | Expr::Path { .. }
        | Expr::It
        | Expr::MetaOpToken(_) => {}
    }
}

/// Collect a nested block's mutation markers into `future`, discarding the
/// per-position map.
///
/// The position map is keyed by STATEMENT INDEX within the enclosing function
/// body; a block nested inside an EXPRESSION has no such index, so there is
/// nothing meaningful to record. `cow_mutations_in_loop` uses the same
/// throwaway-`result` shape for the same reason — one set of collectors, never a
/// parallel AST walker to drift (devbook/24).
fn cow_after_nested_block(
    stmts: &[Spanned<Stmt>],
    future: &mut rustc_hash::FxHashSet<std::rc::Rc<str>>,
    info: &CowPrescan<'_>,
    interner: &mut rustc_hash::FxHashMap<String, std::rc::Rc<str>>,
) {
    let mut discarded = rustc_hash::FxHashMap::default();
    cow_after_block(stmts, future, &mut discarded, info, interner);
}

/// Pre-scan: count how many times each declared name is USED (read) in the function body.
/// Names with exactly 1 use-site (beyond their declaration) are "single-use" — they can
/// be auto-moved at push/constructor sites instead of cloned.
fn prescan_name_use_counts(body: &[Spanned<Stmt>]) -> rustc_hash::FxHashMap<String, u32> {
    let mut counts: rustc_hash::FxHashMap<String, u32> = rustc_hash::FxHashMap::default();
    count_uses_in_block(body, &mut counts);
    counts
}

fn count_uses_in_block(stmts: &[Spanned<Stmt>], counts: &mut rustc_hash::FxHashMap<String, u32>) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::VarDecl { value, .. } => {
                count_uses_in_expr(&value.node, counts);
            }
            Stmt::Assign { target, value, .. } => {
                count_uses_in_expr(&target.node, counts);
                count_uses_in_expr(&value.node, counts);
            }
            Stmt::Return(Some(expr)) => count_uses_in_expr(&expr.node, counts),
            Stmt::If { condition, then_body, elif_branches, else_body, .. } => {
                count_uses_in_expr(&condition.node, counts);
                count_uses_in_block(&then_body.stmts, counts);
                for (cond, body) in elif_branches {
                    count_uses_in_expr(&cond.node, counts);
                    count_uses_in_block(&body.stmts, counts);
                }
                if let Some(body) = else_body {
                    count_uses_in_block(&body.stmts, counts);
                }
            }
            Stmt::While { condition, body, .. } => {
                count_uses_in_expr(&condition.node, counts);
                count_uses_in_block(&body.stmts, counts);
            }
            Stmt::For { iterable, body, .. } => {
                count_uses_in_expr(&iterable.node, counts);
                count_uses_in_block(&body.stmts, counts);
            }
            Stmt::Match { scrutinee, arms, else_arm, .. } => {
                count_uses_in_expr(&scrutinee.node, counts);
                for item in arms {
                    // MetaFor's template body is real code -- see the sibling in
                    // `cow_after_stmt`. Counting it is conservative and correct.
                    let arm = match item {
                        crate::parser::ast::MatchItem::Arm(a) => a,
                        crate::parser::ast::MatchItem::MetaFor { arm_template, .. } => arm_template,
                    };
                    count_uses_in_expr(&arm.body.node, counts);
                }
                if let Some(body) = else_arm {
                    count_uses_in_block(&body.stmts, counts);
                }
            }
            Stmt::Expr(expr) => count_uses_in_expr(&expr.node, counts),

            // ── Previously swept by `_ => {}` (8 of 29 forms handled) ───────
            // UNDER-counting is the dangerous direction: a name with two real
            // uses counted as one reads as SINGLE-USE, is auto-moved at a
            // push/ctor site instead of cloned, and the second use then reads
            // moved-from memory.
            Stmt::CompoundAssign { target, value, .. } => {
                count_uses_in_expr(&target.node, counts);
                count_uses_in_expr(&value.node, counts);
            }
            Stmt::Loop { body }
            | Stmt::NamedScope { body, .. }
            | Stmt::OnError { body }
            | Stmt::MetaFor { body, .. }
            | Stmt::MetaWhile { body, .. } => count_uses_in_block(&body.stmts, counts),
            Stmt::With { bindings, body } => {
                for b in bindings { count_uses_in_expr(&b.expr.node, counts); }
                count_uses_in_block(&body.stmts, counts);
            }
            Stmt::Throw(e) | Stmt::Snapshot { value: e, .. } | Stmt::MetaConst { value: e, .. } => {
                count_uses_in_expr(&e.node, counts)
            }
            Stmt::Assert { condition, message, .. }
            | Stmt::AssertReturn { condition, message } => {
                count_uses_in_expr(&condition.node, counts);
                if let Some(m) = message { count_uses_in_expr(&m.node, counts); }
            }
            Stmt::MetaLog { args, .. } => {
                for a in args { count_uses_in_expr(&a.node, counts); }
            }
            Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
                count_uses_in_expr(&condition.node, counts);
                count_uses_in_block(&then_body.stmts, counts);
                for (c, b) in elif_branches {
                    count_uses_in_expr(&c.node, counts);
                    count_uses_in_block(&b.stmts, counts);
                }
                if let Some(b) = else_body { count_uses_in_block(&b.stmts, counts); }
            }
            Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
                count_uses_in_expr(&scrutinee.node, counts);
                for (_c, b) in arms { count_uses_in_block(&b.stmts, counts); }
                if let Some(b) = else_arm { count_uses_in_block(&b.stmts, counts); }
            }
            Stmt::Select { arms, else_arm } => {
                for a in arms {
                    match &a.op {
                        crate::parser::ast::SelectOp::Send { channel, value, .. } => {
                            count_uses_in_expr(&channel.node, counts);
                            count_uses_in_expr(&value.node, counts);
                        }
                        crate::parser::ast::SelectOp::Recv { channel, .. } => {
                            count_uses_in_expr(&channel.node, counts);
                        }
                    }
                    count_uses_in_block(&a.body.stmts, counts);
                }
                if let Some(b) = else_arm { count_uses_in_block(&b.stmts, counts); }
            }

            // Nothing to count. Explicit so a new variant is a compile error.
            Stmt::Return(None)
            | Stmt::Break
            | Stmt::Continue
            | Stmt::Pass
            | Stmt::Item(_) => {}
        }
    }
}

fn count_uses_in_expr(expr: &Expr, counts: &mut rustc_hash::FxHashMap<String, u32>) {
    match expr {
        Expr::Identifier(name) => {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
        Expr::Call { callee, args, .. } => {
            count_uses_in_expr(&callee.node, counts);
            for arg in args { count_uses_in_expr(&arg.node.value.node, counts); }
        }
        Expr::MethodCall { receiver, args, .. } => {
            count_uses_in_expr(&receiver.node, counts);
            for arg in args { count_uses_in_expr(&arg.node.value.node, counts); }
        }
        Expr::FieldAccess { object, .. } | Expr::TupleFieldAccess { object, .. } => {
            count_uses_in_expr(&object.node, counts);
        }
        Expr::Index { object, index, .. } => {
            count_uses_in_expr(&object.node, counts);
            count_uses_in_expr(&index.node, counts);
        }
        Expr::BinaryOp { left, right, .. } => {
            count_uses_in_expr(&left.node, counts);
            count_uses_in_expr(&right.node, counts);
        }
        Expr::UnaryOp { operand, .. }
        | Expr::Move { expr: operand }
        | Expr::Propagate { expr: operand } => {
            count_uses_in_expr(&operand.node, counts);
        }
        // Struct / container literals also USE their argument identifiers — the
        // prescan previously omitted these, so a name appearing only inside a
        // `Wrapper(item)` / `[item]` / `(a, b)` / `{k: v}` was undercounted
        // (`is_single_use` mis-reported false). Traverse them like Call args.
        // This ENABLES the container-literal move (T-A) and improves
        // `is_single_use` precision for literals; it is NOT a general fix for
        // the remaining `_ => {}` identifier-bearing kinds (that residual
        // undercount can only make `is_single_use` spuriously true, which the
        // `is_last_use_at` co-gate + the GIR "read after MoveZero" validator
        // still block — cost is a missed move-opt, never a wrong result).
        Expr::StructLiteral { args, .. } => {
            for arg in args { count_uses_in_expr(&arg.node, counts); }
        }
        Expr::ArrayLiteral(elems, _) | Expr::TupleLiteral(elems) => {
            for e in elems { count_uses_in_expr(&e.node, counts); }
        }
        Expr::DictLiteral(pairs) => {
            for (k, v) in pairs {
                count_uses_in_expr(&k.node, counts);
                count_uses_in_expr(&v.node, counts);
            }
        }
        Expr::Block(block) => count_uses_in_block(&block.stmts, counts),
        Expr::If { condition, then_branch, elif_branches, else_branch, .. } => {
            count_uses_in_expr(&condition.node, counts);
            count_uses_in_expr(&then_branch.node, counts);
            for (cond, body) in elif_branches {
                count_uses_in_expr(&cond.node, counts);
                count_uses_in_expr(&body.node, counts);
            }
            if let Some(body) = else_branch {
                count_uses_in_expr(&body.node, counts);
            }
        }
        Expr::StringLiteral(_, _) => {} // f-string interpolations don't affect move analysis
        Expr::Closure { body, .. } | Expr::ImplicitClosure { body, .. } => {
            count_uses_in_expr(&body.node, counts);
        }
        Expr::Match { scrutinee, arms, else_arm, .. } => {
            count_uses_in_expr(&scrutinee.node, counts);
            for arm in arms { count_uses_in_expr(&arm.body.node, counts); }
            if let Some(body) = else_arm { count_uses_in_expr(&body.node, counts); }
        }

        // ── Sub-expression carriers previously swept by `_ => {}` ───────────
        // UNDER-counting is the dangerous direction here: a name with two real
        // uses counted as one reads as SINGLE-USE, gets auto-moved at a
        // push/ctor site instead of cloned, and the second use then reads
        // moved-from memory. Same failure class as the four prescan/liveness
        // walkers made exhaustive this round.
        Expr::MutableBorrow { expr: e }
        | Expr::Deref { expr: e }
        | Expr::As { expr: e, .. }
        | Expr::Await { expr: e, .. }
        | Expr::Spawn { expr: e, .. }
        | Expr::SpawnBlocking { expr: e, .. }
        | Expr::Is { expr: e, .. }
        | Expr::OptionalChain { object: e, .. } => count_uses_in_expr(&e.node, counts),
        Expr::DefaultOp { lhs, rhs } | Expr::MetaOpInfix { left: lhs, right: rhs, .. } => {
            count_uses_in_expr(&lhs.node, counts);
            count_uses_in_expr(&rhs.node, counts);
        }
        Expr::Range { start, end, .. } => {
            if let Some(e) = start { count_uses_in_expr(&e.node, counts); }
            if let Some(e) = end { count_uses_in_expr(&e.node, counts); }
        }
        Expr::Rethrow { expr, transform, .. } => {
            count_uses_in_expr(&expr.node, counts);
            count_uses_in_expr(&transform.node, counts);
        }
        Expr::Catch { expr, recovery, .. } => {
            count_uses_in_expr(&expr.node, counts);
            count_uses_in_expr(&recovery.node, counts);
        }
        Expr::Do { body, .. } => count_uses_in_block(&body.stmts, counts),
        Expr::ListComprehension { expr, iterable, condition, .. }
        | Expr::SetComprehension { expr, iterable, condition, .. } => {
            count_uses_in_expr(&expr.node, counts);
            count_uses_in_expr(&iterable.node, counts);
            if let Some(c) = condition { count_uses_in_expr(&c.node, counts); }
        }
        Expr::DictComprehension { key, value, iterable, condition, .. } => {
            count_uses_in_expr(&key.node, counts);
            count_uses_in_expr(&value.node, counts);
            count_uses_in_expr(&iterable.node, counts);
            if let Some(c) = condition { count_uses_in_expr(&c.node, counts); }
        }
        Expr::DotShorthand { args, .. } => {
            for a in args { count_uses_in_expr(&a.node.value.node, counts); }
        }

        // Leaves, listed explicitly so a new variant is a compile error here.
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral
        | Expr::SelfExpr
        | Expr::ReturnValue
        | Expr::Path { .. }
        | Expr::It
        | Expr::MetaOpToken(_) => {}
    }
}

/// Lower a single function definition into the GIR module.
///
/// `name_override` — when `Some`, use this as the GIR/C function name instead of
/// `func.name.node`.  Used by module-scoped name mangling (Phase 5) so that functions
/// from non-entry modules get their module-path prefix in the emitted C symbol while
/// the rest of the lowering logic (body, params, drops) remains unchanged.
pub fn lower_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    func: &FunctionDef,
    name_override: Option<&str>,
) {
    // Sub-pass timing — sums per-function setup/prescan/body/finalize
    // contributions across all calls to `lower_function`. Surfaces in
    // `gg profile` as `lower_function::<sub>` entries under the
    // `gir_lower` pass-times map. See `LoweringContext::lower_fn_sub_times`.
    let __setup_t0 = std::time::Instant::now();

    let func_span = func.span;
    let name: &str = name_override.unwrap_or(func.name.node.as_str());
    let is_main = name == "main";

    // Map return type — use fn_sigs if available (handles `throws` → Result)
    let return_type = if is_main && !func.throws.declares_throws() {
        I32_TYPE
    } else if let Some((_, ret_ty)) = ctx.fn_sigs.get(name) {
        *ret_ty
    } else {
        ctx.type_mapper.map_ast_type(&func.return_type.node)
    };

    // Map parameters — MutableBorrow params become MutPtr types;
    // Resource-type params are passed by pointer (const Ptr for bare, MutPtr for &)
    let params: Vec<(TypeId, Option<&str>)> = func
        .params
        .iter()
        .map(|p| {
            let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            let param_name = p.node.name.node.as_str();
            (gir_type, Some(param_name))
        })
        .collect();

    // Register standalone function signature in fn_sigs with BASE types
    // (before pointer wrapping) so callers can detect resource-type parameters
    // and forward pointers instead of copying structs.
    if !ctx.fn_sigs.contains_key(name) {
        let base_param_types: Vec<TypeId> = func
            .params
            .iter()
            .map(|p| ctx.type_mapper.map_ast_type(&p.node.type_.node))
            .collect();
        ctx.fn_sigs.insert(name.to_string(), (base_param_types, return_type));
    }

    let mut builder = FunctionBuilder::new(name.to_string(), return_type, &params);

    // Clear and register locals for this function
    ctx.clear_locals();
    ctx.func_state.current_fn_name = name.to_string();

    // Register parameters as locals
    ctx.callable_return_types_clear();
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32); // _1, _2, ...
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            // Bare-borrow resource param: Ptr, no auto-deref, clone-on-mutation
            ctx.set_bare_param(&mut builder, local_id);
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            // ! resource params and & trivial params: MutPtr, auto-deref + write-through.
            // Per §6.2: typed shape is Borrowed { Param(self), Unique }.
            ctx.set_param_borrow_unique(&mut builder, local_id);
            // `!` resource params: callee owns the pointee. Tag the Local so
            // the LIR drop lowering knows to dereference through the pointer
            // for the exit drop. Distinct from `&` (MutableBorrow) which
            // shares the same borrow shape but does not own.
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.set_owning_param(&mut builder, local_id);
            }
        }
        // ! string params: caller transfers ownership — mark as owned
        // so clone_resource_args_for_init skips the clone.
        if ctx.type_mapper.is_string_type(base_type)
            && matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
        {
            ctx.set_owned(&mut builder, local_id);
        }
        // Track callable parameter return types + argument types + ownerships
        // (Track B1 write site). Fused via `set_callable_sig` — layering
        // chokepoint (Core #4/#6) so the three sidecars cannot be populated in
        // parallel. Read by the indirect-call arg-emit loops so a
        // `Callable[void(&int)]` value forwards a pointer for a plain-local
        // arg — the write-site fix for the SIGSEGV class Track B1 closes.
        let ret = extract_callable_return_type(&p.node.type_.node, &[], ctx);
        let sig = extract_callable_param_types(&p.node.type_.node, &[], ctx);
        match (ret, sig) {
            (Some(ret_type), Some((param_types, param_owns))) => {
                ctx.set_callable_sig(local_id, ret_type, param_types, param_owns);
            }
            (Some(ret_type), None) => ctx.set_callable_return_type(local_id, ret_type),
            (None, Some((param_types, param_owns))) => {
                ctx.set_callable_param_types(local_id, param_types, param_owns);
            }
            (None, None) => {}
        }
    }

    // Track throws context for Result wrapping in return/throw
    ctx.func_state.current_throws_result_type = if func.throws.declares_throws() {
        Some(return_type)
    } else {
        None
    };

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator so that ref-counted
    // types (Channel, Shared, Weak) passed by value are released at scope exit.
    // For `!` resource params, also register an owning-param drop on the
    // pointee — the callee accepted ownership and must drop it at exit unless
    // the body transfers it onward (which emits a MoveZero on the param slot
    // and flips the LIR drop flag, suppressing the exit drop).
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32);
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.drops.register_param(local_id, gir_type, &ctx.type_registry);
        if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
            && ctx.type_registry.is_resource_type(base_type)
        {
            ctx.drops.register_owning_param(local_id, base_type, &ctx.type_registry);
        }
    }

    // End of `setup` sub-pass — accumulate timing before prescan.
    *ctx.lower_fn_sub_times.entry("lower_function::setup").or_default() += __setup_t0.elapsed();

    let __prescan_t0 = std::time::Instant::now();

    // Pre-scan: find variables unsafe for CoW (reassigned, !-moved).
    // Also count name uses and compute liveness for auto-move (Phase 1f).
    if let FunctionBody::Block(block) = &func.body {
        let __t = std::time::Instant::now();
        ctx.func_state.cow_reassigned_names = prescan_cow_unsafe_names(&block.stmts);
        ctx.func_state.loop_reassigned_names = prescan_loop_reassigned_names(&block.stmts);
        *ctx.lower_fn_sub_times.entry("lower_function::prescan::cow_unsafe").or_default() += __t.elapsed();
        let __t = std::time::Instant::now();
        ctx.func_state.cow_reassigned_after = compute_cow_reassigned_after(
            &block.stmts,
            &CowPrescan {
                fn_param_ownerships: &ctx.fn_param_ownerships,
                receiver_mutations: &ctx.analysis.receiver_mutations,
            },
        );
        *ctx.lower_fn_sub_times.entry("lower_function::prescan::cow_after").or_default() += __t.elapsed();
        let __t = std::time::Instant::now();
        ctx.func_state.name_use_counts = prescan_name_use_counts(&block.stmts);
        *ctx.lower_fn_sub_times.entry("lower_function::prescan::name_use_counts").or_default() += __t.elapsed();
        let __t = std::time::Instant::now();
        ctx.func_state.liveness = super::liveness::compute_function_liveness(&block.stmts);
        *ctx.lower_fn_sub_times.entry("lower_function::prescan::liveness").or_default() += __t.elapsed();
    }

    *ctx.lower_fn_sub_times.entry("lower_function::prescan").or_default() += __prescan_t0.elapsed();

    let __body_t0 = std::time::Instant::now();

    // NOTE: move_override_params is NOT used for non-generic functions.
    // The callee-side move-through-Ptr optimization is only safe when the
    // CALLER's argument is also last-use, which the callee cannot verify.
    // This optimization exists in lower_generic_function for historical reasons
    // but is a known soundness concern — it zeroes the caller's slot through
    // the Ptr, corrupting the caller's data if used after the call.
    // The safe approach (caller-side drop suppression) is deferred to Phase 2.

    // Lower the body
    match &func.body {
        FunctionBody::Block(block) => {
            // Run delayed meta expansion (e.g. `meta for` inside match arms using
            // variant_payloads(T)) for non-generic functions.  For generic functions
            // this is done inside lower_generic_function with type substitutions.
            //
            // Cheap-fix follow-up to the prescan instrumentation (`91af0eb1`):
            // for non-generic functions `meta_env` is empty and the only work
            // the meta-eval pass does is splice out Meta* stmts. Pre-scan the
            // AST in read-only mode; if no Meta* node is reachable from this
            // block, skip the `block.clone()` + walk entirely and lower the
            // original `&block` directly. Typical non-generic functions
            // contain zero meta nodes — saves a deep AST clone per fn.
            let __meta_t0 = std::time::Instant::now();
            let expanded_block;
            let block: &ast::Block = if meta::block_has_delayed_meta(block) {
                let empty_subs: Vec<(String, crate::parser::ast::Type)> = vec![];
                let empty_env = rustc_hash::FxHashMap::default();
                let delayed_ctx = DelayedMetaContext {
                    type_subs:      &empty_subs,
                    features:       &[],
                    meta_env:       &empty_env,
                    items:          &[],
                    trait_registry: &ctx.analysis.traits,
                    type_registry:  &ctx.type_registry,
                };
                let mut meta_errors = Vec::new();
                let mut cloned = block.clone();
                meta::evaluate_delayed_meta_block(&mut cloned, &delayed_ctx, &mut meta_errors);
                for e in &meta_errors {
                    eprintln!("[delayed-meta fn] {e:?}");
                }
                expanded_block = cloned;
                &expanded_block
            } else {
                block
            };
            *ctx.lower_fn_sub_times.entry("lower_function::body::meta_expand").or_default() += __meta_t0.elapsed();
            let __lower_block_t0 = std::time::Instant::now();
            lower_block(ctx, &mut builder, block);
            *ctx.lower_fn_sub_times.entry("lower_function::body::lower_block").or_default() += __lower_block_t0.elapsed();

            // Add implicit return if the last block has no terminator
            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                // P2.6: Emit scope drops before implicit return
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if is_main && !func.throws.declares_throws() {
                    builder.assign(
                        Place::local(LocalId(0)),
                        FunctionBuilder::const_i32(0),
                    );
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                } else if is_main && func.throws.declares_throws() {
                    // throws main: implicit success → Ok(unit) wrapped in Result
                    let type_name = ctx.type_registry.type_name(return_type)
                        .unwrap_or_else(|| "Result".to_string());
                    let ok_val = builder.enum_init(
                        type_name,
                        "Ok",
                        return_type,
                        vec![FunctionBuilder::const_unit()],
                    );
                    assign_to_return_slot(ctx, &mut builder, FunctionBuilder::copy(ok_val));
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                } else if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    // Non-void function without explicit return — emit return _0
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                // Explicit return already handled drops via emit_early_exit_drops.
                // Just pop the scope tracking without emitting more drops.
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            // Mirror the binding form / `lower_return`: set `expected_type` to the
            // *declared success type* `T` while lowering the tail, so the
            // centralized auto-prop hook (and `Ok(...)`/`Error(...)` constructor
            // resolution) sees the user-level type, not the synthesized
            // `Result[T, E]` return slot. For a `throws` fn `T` is the Ok-payload
            // of the slot; when `T` is itself a `Result`/`Option` the gate then
            // fires and the tail is kept un-unwrapped (the B1 silent-miscompile:
            // without this the gate didn't fire, auto-prop over-unwrapped the
            // inner `Result` to a bare value, and the Ok-wrap below re-wrapped it
            // at the wrong layer). For a non-throws fn `expected_type` stays the
            // slot type (unchanged behavior).
            let slot_type = builder.locals[0].type_id;
            let declared_success_type = if func.throws.declares_throws() {
                super::exprs::result_ok_payload_type(ctx, slot_type)
            } else {
                slot_type
            };
            let prev_expected = ctx.func_state.expected_type;
            ctx.func_state.expected_type = Some(declared_success_type);
            let mut operand = lower_expr(ctx, &mut builder, expr);
            ctx.func_state.expected_type = prev_expected;
            // A `return`/`throw` used as the expr-body tail diverges: the inner
            // statement already assigned the return slot, emitted scope-exit
            // drops, and terminated the block. The outer `assign_to_return_slot`
            // (an unguarded `emit`) would clobber the real value with the
            // divergent tail's Unit operand; the trailing `ret` is already a
            // no-op on the terminated block. Skip the trailing assign/drops/ret
            // (the inner return owns them) while still balancing the drop scope.
            // Mirrors the closure-body terminator guard (`closures.rs:520`).
            if !builder.is_terminated() {
                // Clone borrowed operands at the return boundary (BareParam, CowBorrow, etc.).
                // Skip when return type is Ptr — the caller expects a borrow, not an owned clone.
                let ret_type = builder.locals[0].type_id;
                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                    operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                    operand = ctx.auto_deref_at_return(&mut builder, operand, ret_type);
                }
                operand = wrap_expr_tail_in_ok(ctx, &mut builder, operand, ret_type, func.throws.declares_throws());
                let returned_local = match &operand {
                    Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                        Some(place.local)
                    }
                    _ => None,
                };
                assign_to_return_slot(ctx, &mut builder, operand);
                ctx.drops.emit_early_exit_drops(
                    &mut builder, &ctx.type_registry,
                    DropScopeKind::Function, returned_local,
                );
                ctx.drops.pop_scope_no_emit();
                builder.ret(FunctionBuilder::copy(LocalId(0)));
            } else {
                // Tail already terminated (e.g. `: return x`). Balance the drop scope.
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // Not handled in lowering — skip
            // Pop the Function scope we pushed
            ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            *ctx.lower_fn_sub_times.entry("lower_function::body").or_default() += __body_t0.elapsed();
            return;
        }
    }

    *ctx.lower_fn_sub_times.entry("lower_function::body").or_default() += __body_t0.elapsed();

    let __finalize_t0 = std::time::Instant::now();
    ctx.flush_ownership_to_locals(&mut builder);
    let mut func = builder.build();
    func.display_name = Some(name.to_string());
    func.def_span = Some(func_span);
    module.functions.push(func);
    *ctx.lower_fn_sub_times.entry("lower_function::finalize").or_default() += __finalize_t0.elapsed();
}

/// Lower an equip method into a standalone GIR function with mangled name.
pub fn lower_equip_method(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    method: &FunctionDef,
    type_name: &str,
    equipped_type: &Type,
) {
    let method_name = &method.name.node;
    let mangled_name = format!("{type_name}__{method_name}");

    let return_type = if let Some(throws) = method.throws.explicit_type() {
        // `int parse(self, String input) throws String` → Result[int, String].
        // One source of truth (devbook-24 rule 3): synthesize via the shared
        // helper — same path as the free-fn and equip-method pre-scans in
        // `mod.rs`.
        crate::ir::lowering::types::synthesize_throws_result_type(
            &mut ctx.type_mapper,
            &mut ctx.type_registry,
            &method.return_type.node,
            &throws.node,
        )
    } else {
        ctx.type_mapper.map_ast_type(&method.return_type.node)
    };

    // Trivial getter clone elision: return Ptr(T) instead of T.
    // The normal body lowering produces Ptr(T) from IndexLoad/FieldLoad on borrowed self;
    // by matching the return type, we skip the return-boundary materialization clone.
    let is_trivial_getter = ctx.trivial_getter_methods.contains(&mangled_name);
    let return_type = if is_trivial_getter {
        ctx.register_ptr_type(return_type)
    } else {
        return_type
    };

    // Check if method has a self parameter (static methods don't)
    let has_self = method.params.first()
        .map(|p| p.node.name.node == "self")
        .unwrap_or(false);

    // Build parameters: optional self pointer + explicit params
    let mut params: Vec<(TypeId, Option<&str>)> = Vec::new();
    let self_ptr_type = if has_self {
        let self_type_id = ctx.type_mapper.map_ast_type(equipped_type);
        let self_needs_mut_ptr = method.params.first()
            .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow | Ownership::Move))
            .unwrap_or(false);
        // Scalar primitive receivers (int, float, bool, uint8, …) pass
        // by value to match the call-site dispatch (see methods.rs
        // `recv_type_id.0 < PRIMITIVE_TYPE_COUNT` branch). Only the
        // mutable-borrow form goes through a pointer so the callee can
        // write back.
        let is_scalar = self_type_id.0 < crate::ir::types::PRIMITIVE_TYPE_COUNT;
        let spt = if self_needs_mut_ptr {
            ctx.register_mut_ptr_type(self_type_id)
        } else if is_scalar {
            self_type_id
        } else {
            ctx.register_ptr_type(self_type_id)
        };
        params.push((spt, Some("self")));
        Some(spt)
    } else {
        None
    };
    // Track consuming self (!self) for field load optimization
    let self_is_consuming = has_self && method.params.first()
        .map(|p| matches!(p.node.ownership, Ownership::Move))
        .unwrap_or(false);
    for p in &method.params {
        if p.node.name.node == "self" {
            continue; // self handled above
        }
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        params.push((gir_type, Some(p.node.name.node.as_str())));
    }

    let mut builder = FunctionBuilder::new(mangled_name.clone(), return_type, &params);

    // Clear and register locals
    ctx.clear_locals();
    ctx.func_state.current_fn_name = mangled_name.clone();
    ctx.callable_return_types_clear();
    ctx.func_state.consuming_self = self_is_consuming;

    // Register self as local _1 (only if method has self)
    let mut param_idx = if let Some(spt) = self_ptr_type {
        ctx.register_local("self", LocalId(1), spt);
        // Mark immutable self as BareParam so CoW materializes on mutation.
        if !self_is_consuming && matches!(ctx.type_registry.get(spt), Some(GirType::Ptr(_))) {
            ctx.set_bare_param(&mut builder, LocalId(1));
        }
        2u32
    } else {
        1u32
    };

    // Register other params
    for p in &method.params {
        if p.node.name.node == "self" {
            continue;
        }
        let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            ctx.set_bare_param(&mut builder, LocalId(param_idx));
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            // & or ! MutPtr param. Per §6.2: typed shape Borrowed { Param(self), Unique }.
            ctx.set_param_borrow_unique(&mut builder, LocalId(param_idx));
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.set_owning_param(&mut builder, LocalId(param_idx));
            }
        }
        // Track callable parameter sidecars for indirect-call lowering (all
        // three fused via `set_callable_sig` — the layering chokepoint).
        let ret = extract_callable_return_type(&p.node.type_.node, &[], ctx);
        let sig = extract_callable_param_types(&p.node.type_.node, &[], ctx);
        match (ret, sig) {
            (Some(ret_type), Some((param_types, param_owns))) => {
                ctx.set_callable_sig(LocalId(param_idx), ret_type, param_types, param_owns);
            }
            (Some(ret_type), None) => ctx.set_callable_return_type(LocalId(param_idx), ret_type),
            (None, Some((param_types, param_owns))) => {
                ctx.set_callable_param_types(LocalId(param_idx), param_types, param_owns);
            }
            (None, None) => {}
        }
        param_idx += 1;
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator
    {
        let mut pidx = if self_ptr_type.is_some() { 2u32 } else { 1u32 };
        for p in &method.params {
            if p.node.name.node == "self" {
                continue;
            }
            let base_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            ctx.drops.register_param(LocalId(pidx), gir_type, &ctx.type_registry);
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.drops.register_owning_param(LocalId(pidx), base_type, &ctx.type_registry);
            }
            pidx += 1;
        }
    }

    // Track throws context for Result wrapping in return/throw statements
    ctx.func_state.current_throws_result_type = if method.throws.declares_throws() {
        Some(return_type)
    } else {
        None
    };

    // Register equip method signature in fn_sigs so callers see the Result return type
    if !ctx.fn_sigs.contains_key(&mangled_name) {
        let base_param_types: Vec<TypeId> = method
            .params
            .iter()
            .filter(|p| p.node.name.node != "self")
            .map(|p| ctx.type_mapper.map_ast_type(&p.node.type_.node))
            .collect();
        ctx.fn_sigs.insert(mangled_name.clone(), (base_param_types, return_type));
    }

    // Pre-scan: find variables unsafe for CoW + count name uses + liveness for auto-move.
    if let FunctionBody::Block(block) = &method.body {
        ctx.func_state.cow_reassigned_names = prescan_cow_unsafe_names(&block.stmts);
        ctx.func_state.loop_reassigned_names = prescan_loop_reassigned_names(&block.stmts);
        ctx.func_state.cow_reassigned_after = compute_cow_reassigned_after(
            &block.stmts,
            &CowPrescan {
                fn_param_ownerships: &ctx.fn_param_ownerships,
                receiver_mutations: &ctx.analysis.receiver_mutations,
            },
        );
        ctx.func_state.name_use_counts = prescan_name_use_counts(&block.stmts);
        ctx.func_state.liveness = super::liveness::compute_function_liveness(&block.stmts);
    }

    // Lower the body
    match &method.body {
        FunctionBody::Block(block) => {
            // Evaluate delayed meta blocks (meta if/for) with Self bound to
            // the equipped type. Elide the clone+walk when there are no
            // delayed-meta nodes anywhere in the AST — same pattern as
            // `lower_function` at the non-generic site above.
            let expanded_block;
            let block_ref: &ast::Block = if meta::block_has_delayed_meta(block) {
                let mut block = block.clone();
                let self_subs = vec![("Self".to_string(), equipped_type.clone())];
                let empty_env = rustc_hash::FxHashMap::default();
                let delayed_ctx = DelayedMetaContext {
                    type_subs:      &self_subs,
                    features:       &[],
                    meta_env:       &empty_env,
                    items:          &[],
                    trait_registry: &ctx.analysis.traits,
                    type_registry:  &ctx.type_registry,
                };
                let mut meta_errors = Vec::new();
                meta::evaluate_delayed_meta_block(&mut block, &delayed_ctx, &mut meta_errors);
                for e in &meta_errors {
                    eprintln!("[delayed-meta equip] {e:?}");
                }
                expanded_block = block;
                &expanded_block
            } else {
                block
            };
            lower_block(ctx, &mut builder, block_ref);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            // See `lower_function`'s expr-body arm: set `expected_type` to the
            // declared success type `T` (Ok-payload of the `Result[T, E]` slot
            // for a `throws` method) so auto-prop / `Ok(...)` resolution sees the
            // user-level type, not the slot. Fixes the `T = Result` silent
            // miscompile on the method path too.
            let slot_type = builder.locals[0].type_id;
            let declared_success_type = if method.throws.declares_throws() {
                super::exprs::result_ok_payload_type(ctx, slot_type)
            } else {
                slot_type
            };
            let prev_expected = ctx.func_state.expected_type;
            ctx.func_state.expected_type = Some(declared_success_type);
            let mut operand = lower_expr(ctx, &mut builder, expr);
            ctx.func_state.expected_type = prev_expected;
            // See `lower_function`'s expr-body arm: a `return`/`throw` tail
            // already terminated the block; the outer assign would clobber the
            // slot. Skip the trailing assign/drops/ret, just balance the scope.
            if !builder.is_terminated() {
                // Clone borrowed operands at the return boundary.
                // Skip when return type is Ptr — the caller expects a borrow.
                let ret_type = builder.locals[0].type_id;
                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                    operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                    operand = ctx.auto_deref_at_return(&mut builder, operand, ret_type);
                }
                operand = wrap_expr_tail_in_ok(ctx, &mut builder, operand, ret_type, method.throws.declares_throws());
                let returned_local = match &operand {
                    Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                        Some(place.local)
                    }
                    _ => None,
                };
                assign_to_return_slot(ctx, &mut builder, operand);
                ctx.drops.emit_early_exit_drops(
                    &mut builder, &ctx.type_registry,
                    DropScopeKind::Function, returned_local,
                );
            }
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            return;
        }
    }

    ctx.flush_ownership_to_locals(&mut builder);
    let mut func = builder.build();
    func.display_name = Some(format!("{type_name}.{method_name}"));
    module.functions.push(func);
}

/// Lower a monomorphized instance of a generic function.
///
/// `type_args` are the concrete type arguments (e.g., `[int]` for `identity[int]`).
/// `mangled_name` is the fully mangled name (e.g., `identity__int64_t`).
///
/// The function body is lowered with type parameter substitutions active,
/// so references to `T` in the template resolve to the concrete type.
pub fn lower_generic_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    template: &FunctionDef,
    type_args: &[Spanned<Type>],
    mangled_name: &str,
    meta_op_bindings: &[(String, ast::BinaryOp)],
) {
    let subs = build_subs(template.generic_params.as_ref(), type_args);

    // Build a meta env pre-populated with any compile-time operator bindings.
    // Used by evaluate_delayed_meta_block so MetaOpInfix nodes get substituted
    // to real BinaryOp expressions during monomorphization.
    let mut meta_env_map: rustc_hash::FxHashMap<String, MetaValue> =
        rustc_hash::FxHashMap::default();
    for (param_name, op) in meta_op_bindings {
        meta_env_map.insert(param_name.clone(), MetaValue::Op(*op));
    }

    // Evaluate delayed meta blocks (meta if/for inside generic bodies) with
    // the concrete type substitutions.  Modifies a local clone of the template
    // so the original template is left intact for subsequent instantiations.
    let template_with_meta_evaluated;
    let template = if subs.is_empty() && meta_env_map.is_empty() {
        template
    } else {
        let mut cloned = template.clone();
        let delayed_ctx = DelayedMetaContext {
            type_subs:      &subs,
            features:       &[],
            meta_env:       &meta_env_map,
            items:          &[],
            trait_registry: &ctx.analysis.traits,
            type_registry:  &ctx.type_registry,
        };
        match cloned.body {
            FunctionBody::Block(ref mut block) => {
                let mut errors = Vec::new();
                meta::evaluate_delayed_meta_block(block, &delayed_ctx, &mut errors);
                // Errors are non-fatal here (will surface as missing symbols); log if any.
                if !errors.is_empty() {
                    for e in &errors {
                        eprintln!("[delayed-meta] {e:?}");
                    }
                }
            }
            // Expression bodies get the same meta-op substitution sweep (Core #4:
            // same MetaOpInfix-substitution class as the block arm). Without this
            // an expr-body `meta[op]` infix survives to GIR lowering and panics.
            FunctionBody::Expression(ref mut expr) => {
                meta::evaluate_delayed_meta_expr(expr, &delayed_ctx);
            }
            // Enumerated (not `_`) so a future FunctionBody variant that can
            // carry a `meta[op]` fails to compile here and forces the author to
            // decide whether it needs the sweep — the meta-substitution
            // completeness gate (Core #10 / #4). Declaration has no body; Extern
            // is a C-symbol string — neither carries a sweepable meta op.
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
        template_with_meta_evaluated = cloned;
        &template_with_meta_evaluated
    };

    // Build type name substitutions for struct init/method calls in the body
    build_type_name_subs(ctx, &subs);

    // Build generic type parameter → concrete TypeId substitutions
    build_generic_type_params(ctx, &subs);

    // Map return type with substitutions
    let return_type = substitute_and_map_type(ctx, &template.return_type.node, &subs);

    // Pre-scan: detect bare Move-type params that are directly returned.
    // These need Move (not Borrow) semantics to avoid double-free.
    // In expression bodies `T f[T](T x) = x`, or block bodies with `return x`,
    // a borrow param would create a shallow copy without transferring ownership.
    let mut move_override_params: std::collections::HashSet<String> = std::collections::HashSet::new();
    if ctx.type_registry.is_resource_type(return_type) {
        let returned_param_name = match &template.body {
            FunctionBody::Expression(expr) => {
                if let Expr::Identifier(name) = &expr.node {
                    Some(name.as_str())
                } else { None }
            }
            FunctionBody::Block(block) => {
                // Check if the last statement is `return x` where x is a param.
                // The param must be the last use — earlier statements may read
                // through it but must not consume or reassign it.
                block.stmts.last().and_then(|stmt| {
                    if let Stmt::Return(Some(expr)) = &stmt.node {
                        if let Expr::Identifier(name) = &expr.node {
                            // Verify this is the param's last use via liveness
                            Some(name.as_str())
                        } else { None }
                    } else { None }
                })
            }
            _ => None,
        };
        if let Some(name) = returned_param_name {
            for p in &template.params {
                if !p.node.is_meta_op
                    && p.node.name.node == name
                    && p.node.ownership == Ownership::Borrow
                {
                    let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
                    if ctx.type_registry.is_resource_type(base_type) {
                        move_override_params.insert(name.to_string());
                    }
                }
            }
        }
    }

    // Map parameters with substitutions — skip meta op params (no runtime representation),
    // MutableBorrow params become MutPtr; Borrow params of Move types also become MutPtr
    let params: Vec<(TypeId, Option<String>)> = template
        .params
        .iter()
        .filter(|p| !p.node.is_meta_op)
        .map(|p| {
            let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
            let ownership = if move_override_params.contains(&p.node.name.node) {
                Ownership::Move
            } else {
                p.node.ownership
            };
            let gir_type = ctx.resolve_param_type(base_type, ownership);
            (gir_type, Some(p.node.name.node.clone()))
        })
        .collect();

    let param_refs: Vec<(TypeId, Option<&str>)> = params
        .iter()
        .map(|(tid, name)| (*tid, name.as_deref()))
        .collect();

    let mut builder = FunctionBuilder::new(mangled_name, return_type, &param_refs);

    // Clear and register locals — assign sequential LocalIds to runtime params only
    // (meta op params carry no runtime value and are skipped).
    ctx.clear_locals();
    ctx.func_state.current_fn_name = mangled_name.to_string();
    ctx.callable_return_types_clear();

    let mut local_idx: u32 = 0;
    for p in template.params.iter() {
        if p.node.is_meta_op {
            continue;
        }
        local_idx += 1;
        let local_id = LocalId(local_idx);
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        let is_move_override = move_override_params.contains(&p.node.name.node);
        let ownership = if is_move_override {
            Ownership::Move
        } else {
            p.node.ownership
        };
        let gir_type = ctx.resolve_param_type(base_type, ownership);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
        if is_move_override {
            ctx.func_state.move_override_params.insert(local_id);
        }
        if ctx.is_ref_param(base_type, ownership) {
            ctx.set_bare_param(&mut builder, local_id);
        } else if ctx.is_mut_ref_param(base_type, ownership) {
            // & or ! MutPtr param. Per §6.2: typed shape Borrowed { Param(self), Unique }.
            ctx.set_param_borrow_unique(&mut builder, local_id);
            if matches!(ownership, Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.set_owning_param(&mut builder, local_id);
            }
        }
        // Track callable parameter sidecars for indirect-call lowering (fused
        // via `set_callable_sig`).
        let ret = extract_callable_return_type(&p.node.type_.node, &subs, ctx);
        let sig = extract_callable_param_types(&p.node.type_.node, &subs, ctx);
        match (ret, sig) {
            (Some(ret_type), Some((param_types, param_owns))) => {
                ctx.set_callable_sig(local_id, ret_type, param_types, param_owns);
            }
            (Some(ret_type), None) => ctx.set_callable_return_type(local_id, ret_type),
            (None, Some((param_types, param_owns))) => {
                ctx.set_callable_param_types(local_id, param_types, param_owns);
            }
            (None, None) => {}
        }
        // Phase D4: store move-override flag as a typed LocalId set so the
        // return path can zero the source through the pointer without a
        // name lookup. Replaces the legacy `HashSet<String>` sidecar.
        if move_override_params.contains(&p.node.name.node) {
            ctx.func_state.move_override_params.insert(local_id);
        }
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator.
    // Skip meta op params — they have no runtime local slot.
    let mut drop_idx: u32 = 0;
    for p in template.params.iter() {
        if p.node.is_meta_op {
            continue;
        }
        drop_idx += 1;
        let local_id = LocalId(drop_idx);
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, &subs);
        let ownership = if move_override_params.contains(&p.node.name.node) {
            Ownership::Move
        } else {
            p.node.ownership
        };
        let gir_type = ctx.resolve_param_type(base_type, ownership);
        ctx.drops.register_param(local_id, gir_type, &ctx.type_registry);
        if matches!(ownership, Ownership::Move)
            && ctx.type_registry.is_resource_type(base_type)
        {
            ctx.drops.register_owning_param(local_id, base_type, &ctx.type_registry);
        }
    }

    // Lower the body
    match &template.body {
        FunctionBody::Block(block) => {
            lower_block(ctx, &mut builder, block);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                // For move-overridden params in block bodies with implicit return:
                // zero the source through the pointer to prevent caller double-free.
                if !move_override_params.is_empty() {
                    for name in &move_override_params {
                        if let Some((local_id, _)) = ctx.lookup_local(name) {
                            builder.move_zero(Place {
                                local: local_id,
                                projections: vec![Projection::Deref],
                            });
                        }
                    }
                }
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }

        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            let mut operand = lower_expr(ctx, &mut builder, expr);
            // See `lower_function`'s expr-body arm: a `return`/`throw` tail
            // already terminated the block; the outer assign would clobber the
            // slot. Skip the trailing assign/drops/ret, just balance the scope.
            if !builder.is_terminated() {
                // Clone borrowed operands at the return boundary.
                // Skip when return type is Ptr — the caller expects a borrow.
                let ret_type = builder.locals[0].type_id;
                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                    operand = ctx.auto_deref_at_return(&mut builder, operand, ret_type);
                    operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                }
                let returned_local = match &operand {
                    Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                        Some(place.local)
                    }
                    _ => None,
                };
                assign_to_return_slot(ctx, &mut builder, operand);
                // For move-overridden params: zero the source through the pointer
                // to prevent the caller from double-freeing.
                if !move_override_params.is_empty() {
                    if let Expr::Identifier(name) = &expr.node {
                        if let Some((local_id, _)) = ctx.lookup_local(name) {
                            builder.move_zero(Place {
                                local: local_id,
                                projections: vec![Projection::Deref],
                            });
                        }
                    }
                }
                ctx.drops.emit_early_exit_drops(
                    &mut builder, &ctx.type_registry,
                    DropScopeKind::Function, returned_local,
                );
            }
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            ctx.generics.type_name_subs.clear();
            ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
            return;
        }
    }

    ctx.generics.type_name_subs.clear();
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
    ctx.flush_ownership_to_locals(&mut builder);
    module.functions.push(builder.build());
}

/// Lower monomorphized equip methods for a generic type instantiation.
///
/// For each method in the equip block, creates a GIR function named
/// `{mangled_type_name}__{method_name}` with substituted types.
pub fn lower_generic_equip_methods(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
    mangled_type_name: &str,
) {
    lower_generic_equip_methods_with_defaults(ctx, module, equip, type_args, mangled_type_name, None);
}

/// Lower monomorphized equip methods for a generic type instantiation,
/// with optional AST module for default trait method emission.
pub fn lower_generic_equip_methods_with_defaults(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
    mangled_type_name: &str,
    ast_module: Option<&ast::Module>,
) {
    let subs = build_equip_subs(equip, type_args);

    // Substituted equipped type — used for Self binding in delayed meta evaluation.
    let substituted_equipped_type = generics::substitute_type_pub(&equip.type_.node, &subs);

    // Build type name substitutions for struct init/method calls in the body
    build_type_name_subs(ctx, &subs);

    // Add substitution for the equipped type itself (e.g., Pair__T → Pair__int64_t)
    // This handles cases where the method body references the struct/enum being equipped
    if let Type::Named { name, generic_args } = &equip.type_.node {
        let base_name = &name.node;
        if !generic_args.is_empty() {
            // Mangle the template name (with generic params as wildcards)
            // For Pair[T], we want "Pair__T"
            let template_mangled = super::types::mangle_generic_name(base_name, generic_args);
            // mangled_type_name is already the concrete name (e.g., "Pair__int64_t")
            if template_mangled != mangled_type_name {
                ctx.generics.type_name_subs.insert(template_mangled, mangled_type_name.to_string());
            }
        }
    }

    // Build generic type parameter → concrete TypeId substitutions
    build_generic_type_params(ctx, &subs);

    for method in &equip.items {
        let method_def = &method.node;
        // Methods with their own generic params (e.g. `map[U, F]` inside
        // `equip [T] VectorIter[T]:`) can't be lowered here — the equip-level
        // subs only cover T, so the return type `MapIter[T, U, F]` would
        // substitute to `MapIter[int, U, F]` which map_ast_type_mut resolves
        // to UNIT_TYPE. Per-call-site mono via `lower_method_instance`
        // handles these with merged equip + method substitutions.
        if method_def.generic_params.is_some() {
            continue;
        }
        let method_mangled = format!("{mangled_type_name}__{}", method_def.name.node);
        lower_equip_method_with_subs(
            ctx, module, method_def, &subs,
            mangled_type_name, &method_mangled,
            &substituted_equipped_type,
        );
    }

    // Emit default trait methods that aren't overridden in the equip block
    if let (Some(ast_mod), Some(trait_ref)) = (ast_module, &equip.trait_) {
        use crate::parser::ast::{Item, TraitItem};
        let trait_name = super::traits::extract_trait_name(&trait_ref.trait_name.node);
        if !trait_name.is_empty() {
            let implemented: Vec<String> = equip.items.iter()
                .map(|m| m.node.name.node.clone())
                .collect();
            // Substituted equipped type already computed above; re-borrow for this scope.
            let substituted_type = substituted_equipped_type.clone();
            // Find trait def and emit defaults
            for item in &ast_mod.items {
                if let Item::Trait(trait_def) = &item.node {
                    if trait_def.name.node == trait_name {
                        for trait_item in &trait_def.items {
                            if let TraitItem::Method(default_method) = &trait_item.node {
                                let method_name = &default_method.name.node;
                                if implemented.contains(method_name) {
                                    continue;
                                }
                                match &default_method.body {
                                    FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                                    FunctionBody::Block(_) | FunctionBody::Expression(_) => {}
                                }
                                // Method-level-generic defaults (e.g.
                                // `bool any[F](&self, F pred)`) follow the
                                // same per-call-site mono path as inherent
                                // method-generic equip methods — skip the
                                // bulk lowering and let lower_method_instance
                                // handle them at each call site.
                                if default_method.generic_params.is_some() {
                                    continue;
                                }
                                // Emit as {mangled_type_name}__{method_name}.
                                // Threading the equip-level subs lets the
                                // signature substitute `T → int64_t` (etc.)
                                // before map_ast_type — without this,
                                // `Option[T] last(&self)` resolves to
                                // `Option[UNIT]` and the call site sees a
                                // void return. Defaults additionally bind
                                // `Self` so adapter constructors like
                                // `TakeIter[Self, T] take(self, int n)`
                                // resolve to the equipping type. The
                                // trait's OWN generic params (e.g. `T`
                                // in `Vector[T] collect(&self)` on
                                // `Iterator[T]`) bind to the substituted
                                // trait arg, pushed BEFORE the impl subs
                                // so name collisions (impl-local `T` vs
                                // trait's `T` mapped to `U`) resolve to
                                // the trait binding for trait-body refs.
                                // Matches `register_equip_sigs_with_defaults`
                                // in generics/mod.rs.
                                let trait_args_ast: Vec<ast::Type> = if let ast::Type::Named { generic_args, .. } = &trait_ref.trait_name.node {
                                    generic_args.iter()
                                        .map(|a| generics::substitute_type_pub(&a.node, &subs))
                                        .collect()
                                } else {
                                    Vec::new()
                                };
                                let trait_generic_names: Vec<String> = trait_def.generic_params.as_ref()
                                    .map(|gp| gp.node.params.iter().filter_map(|p| match &p.node {
                                        ast::GenericParam::Type { name, .. } => Some(name.node.clone()),
                                        ast::GenericParam::Const { .. } => None,
                                    }).collect())
                                    .unwrap_or_default();
                                let mut default_subs: Vec<(String, ast::Type)> = Vec::new();
                                default_subs.push(("Self".to_string(), substituted_type.clone()));
                                for (name, concrete) in trait_generic_names.iter().zip(trait_args_ast.iter()) {
                                    default_subs.push((name.clone(), concrete.clone()));
                                }
                                default_subs.extend(subs.iter().cloned());
                                // Demand-gate adapter-returning defaults:
                                // if the substituted return type references
                                // a generic struct (e.g. `TakeIter[...]`)
                                // that hasn't been registered as an instance,
                                // skip emission. `discover_method_instances`
                                // registers these demand-driven from user
                                // call sites; anything missing here is dead
                                // code that would otherwise cascade forever
                                // through every Iterator implementor.
                                let substituted_ret = generics::substitute_type_pub(
                                    &default_method.return_type.node, &default_subs,
                                );
                                if !all_return_nominals_registered(ctx, &substituted_ret) {
                                    continue;
                                }
                                let method_mangled = format!("{mangled_type_name}__{method_name}");
                                // Refresh generic_type_params so the body
                                // lowering sees `Self → equipping_type`
                                // when it resolves type-arg lists like
                                // `TakeIter[Self, T](self, n)`.
                                build_generic_type_params(ctx, &default_subs);
                                // Pre-substitute the body so struct-constructor
                                // type-arg lists resolve before mangling. Without
                                // this `TakeIter[Self, T]` mangles to
                                // `TakeIter__unknown__int64_t` because
                                // `mangle_type_for_name(Self)` returns "unknown".
                                let substituted_method = generics::substitute_function_body_pub(
                                    default_method, &default_subs,
                                );
                                lower_equip_method_with_subs(
                                    ctx, module, &substituted_method, &default_subs,
                                    mangled_type_name, &method_mangled,
                                    &substituted_type,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    ctx.generics.type_name_subs.clear();
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
}

/// Lower a single equip method body with the given substitutions. Shared
/// between the equip-block bulk path (`lower_generic_equip_methods_with_defaults`)
/// and the per-call-site method-instance path (`lower_method_instance`). The
/// two differ only in how they build `subs` and `method_mangled`.
fn lower_equip_method_with_subs(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    method_def: &ast::FunctionDef,
    subs: &[(String, Type)],
    mangled_type_name: &str,
    method_mangled: &str,
    substituted_equipped_type: &Type,
) {
    // Use map_ast_type_mut so that generic return types like Option[T] get
    // registered (not silently resolved to UNIT_TYPE) after substitution.
    let substituted_ret = generics::substitute_type_pub(&method_def.return_type.node, subs);
    let return_type = ctx.type_mapper.map_ast_type_mut(&substituted_ret, &mut ctx.type_registry);

    // Self pointer type — only for methods with a self parameter
    let has_self = method_def.params.first()
        .map(|p| p.node.name.node == "self")
        .unwrap_or(false);

    let self_type_id = ctx.type_mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
    let self_is_mutable = method_def.params.first()
        .map(|p| {
            p.node.name.node == "self" &&
            matches!(p.node.ownership, Ownership::MutableBorrow)
        })
        .unwrap_or(false);

    let self_ptr_type = if self_is_mutable {
        ctx.register_mut_ptr_type(self_type_id)
    } else {
        ctx.register_ptr_type(self_type_id)
    };

    let mut params: Vec<(TypeId, Option<&str>)> = if has_self {
        vec![(self_ptr_type, Some("self"))]
    } else {
        vec![]
    };
    for p in &method_def.params {
        if p.node.name.node == "self" {
            continue;
        }
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, subs);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        params.push((gir_type, Some(p.node.name.node.as_str())));
    }

    let mut builder = FunctionBuilder::new(method_mangled, return_type, &params);

    ctx.clear_locals();
    ctx.callable_return_types_clear();
    let mut param_idx = if has_self {
        ctx.register_local("self", LocalId(1), self_ptr_type);
        // 2E scout (sibling of the non-generic path's mark at ~:1320): mark
        // immutable plain `self` as BareParam so CoW materializes on
        // mutation — the generic-equip lowering forgot the mark, so
        // plain-`self` writes in generic methods wrote through (probe p13).
        let self_is_consuming = method_def.params.first()
            .map(|p| matches!(p.node.ownership, Ownership::Move))
            .unwrap_or(false);
        if !self_is_mutable
            && !self_is_consuming
            && matches!(ctx.type_registry.get(self_ptr_type), Some(GirType::Ptr(_)))
        {
            ctx.set_bare_param(&mut builder, LocalId(1));
        }
        2u32
    } else {
        1u32
    };
    for p in &method_def.params {
        if p.node.name.node == "self" {
            continue;
        }
        let base_type = substitute_and_map_type(ctx, &p.node.type_.node, subs);
        let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
        ctx.register_local(&p.node.name.node, LocalId(param_idx), gir_type);
        if ctx.is_ref_param(base_type, p.node.ownership) {
            ctx.set_bare_param(&mut builder, LocalId(param_idx));
        } else if ctx.is_mut_ref_param(base_type, p.node.ownership) {
            // & or ! MutPtr param. Per §6.2: typed shape Borrowed { Param(self), Unique }.
            ctx.set_param_borrow_unique(&mut builder, LocalId(param_idx));
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.set_owning_param(&mut builder, LocalId(param_idx));
            }
        }
        // Track callable parameter sidecars for indirect-call lowering (fused
        // via `set_callable_sig`).
        let ret = extract_callable_return_type(&p.node.type_.node, subs, ctx);
        let sig = extract_callable_param_types(&p.node.type_.node, subs, ctx);
        match (ret, sig) {
            (Some(ret_type), Some((param_types, param_owns))) => {
                ctx.set_callable_sig(LocalId(param_idx), ret_type, param_types, param_owns);
            }
            (Some(ret_type), None) => ctx.set_callable_return_type(LocalId(param_idx), ret_type),
            (None, Some((param_types, param_owns))) => {
                ctx.set_callable_param_types(LocalId(param_idx), param_types, param_owns);
            }
            (None, None) => {}
        }
        param_idx += 1;
    }

    // P2.6: Push Function drop scope
    ctx.drops.push_scope(DropScopeKind::Function);

    // Register function parameters with the drop elaborator
    {
        let mut pidx = if has_self { 2u32 } else { 1u32 };
        for p in &method_def.params {
            if p.node.name.node == "self" {
                continue;
            }
            let base_type = substitute_and_map_type(ctx, &p.node.type_.node, subs);
            let gir_type = ctx.resolve_param_type(base_type, p.node.ownership);
            ctx.drops.register_param(LocalId(pidx), gir_type, &ctx.type_registry);
            if matches!(p.node.ownership, crate::parser::ast::Ownership::Move)
                && ctx.type_registry.is_resource_type(base_type)
            {
                ctx.drops.register_owning_param(LocalId(pidx), base_type, &ctx.type_registry);
            }
            pidx += 1;
        }
    }

    match &method_def.body {
        FunctionBody::Block(block) => {
            // Evaluate delayed meta blocks (meta if/for) with Self bound to
            // the equipped type. Generic-equip path — usually has meta nodes
            // (this is monomorphization-time work), but a generic equip
            // method with no `meta if`/`meta for` still hits this branch
            // and would clone+walk for nothing. Same elision as the
            // non-generic / inherent-equip paths above.
            let expanded_block;
            let block_ref: &ast::Block = if meta::block_has_delayed_meta(block) {
                let mut block = block.clone();
                let self_subs = vec![("Self".to_string(), substituted_equipped_type.clone())];
                let empty_env = rustc_hash::FxHashMap::default();
                let delayed_ctx = DelayedMetaContext {
                    type_subs:      &self_subs,
                    features:       &[],
                    meta_env:       &empty_env,
                    items:          &[],
                    trait_registry: &ctx.analysis.traits,
                    type_registry:  &ctx.type_registry,
                };
                let mut meta_errors = Vec::new();
                meta::evaluate_delayed_meta_block(&mut block, &delayed_ctx, &mut meta_errors);
                for e in &meta_errors {
                    eprintln!("[delayed-meta generic-equip] {e:?}");
                }
                expanded_block = block;
                &expanded_block
            } else {
                block
            };
            lower_block(ctx, &mut builder, block_ref);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            } else {
                ctx.drops.pop_scope_no_emit();
            }
        }
        FunctionBody::Expression(expr) => {
            let expr_span = expr.span;
            let mut operand = lower_expr(ctx, &mut builder, expr);
            // See `lower_function`'s expr-body arm: a `return`/`throw` tail
            // already terminated the block; the outer assign would clobber the
            // slot. Skip the trailing assign/drops/ret, just balance the scope.
            if !builder.is_terminated() {
                // Clone borrowed operands at the return boundary.
                // Skip when return type is Ptr — the caller expects a borrow.
                let ret_type = builder.locals[0].type_id;
                operand = ctx.auto_deref_at_return(&mut builder, operand, ret_type);
                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                    operand = ctx.ensure_owned_at_boundary(&mut builder, operand, expr_span, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                }
                let returned_local = match &operand {
                    Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                        Some(place.local)
                    }
                    _ => None,
                };
                assign_to_return_slot(ctx, &mut builder, operand);
                ctx.drops.emit_early_exit_drops(
                    &mut builder, &ctx.type_registry,
                    DropScopeKind::Function, returned_local,
                );
            }
            ctx.drops.pop_scope_no_emit();
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }
        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            ctx.drops.pop_scope_no_emit();
            return;
        }
    }

    ctx.flush_ownership_to_locals(&mut builder);
    module.functions.push(builder.build());
}

/// Lower a per-call-site monomorphisation of a method-level-generic equip
/// method into a free-function-shaped symbol. Used for calls like
/// `v.iter().map[int, int(int)](f)` that the bulk equip-lowering path skips
/// because the equip-level subs don't cover the method-level generics.
///
/// Merges equip-level substitutions (e.g. `T→int` from `VectorIter[int]`)
/// with method-level substitutions (e.g. `U→int`, `F→int(int)` from the
/// call site's generic args). The resulting body has all type params
/// substituted; the emitted function is named by `mangled_symbol` and is
/// called directly by the MethodCall dispatch path (see
/// `src/ir/lowering/exprs/methods.rs`).
pub fn lower_method_instance(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    equip: &ast::EquipBlock,
    method: &ast::FunctionDef,
    equip_type_args: &[Spanned<Type>],
    method_type_args: &[Spanned<Type>],
    mangled_type_name: &str,
    mangled_symbol: &str,
) {
    // Equip-level subs (T → concrete from receiver).
    let mut subs = build_equip_subs(equip, equip_type_args);
    // Equipped type uses ONLY equip-level params (the receiver struct's
    // own params, e.g. FilterIter's `Iter, T, F`); compute it before the
    // method-level params are merged so method-scope shadowing (below)
    // can't rewrite the receiver's own closure param `F`.
    let substituted_equipped_type = generics::substitute_type_pub(&equip.type_.node, &subs);
    // Method-level subs (U, F → concrete from call-site targs). Method-level
    // generics are the innermost scope and SHADOW an equip-level param of the
    // same name (see `merge_method_subs`) — otherwise a struct+method sharing a
    // param letter (e.g. FilterIter's predicate `F` and the trait-default
    // `map[U, F]`'s map closure `F`) collides and `substitute_type`'s
    // first-match picks the wrong (equip) binding, mis-typing the map result.
    generics::merge_method_subs(&mut subs, method.generic_params.as_ref(), method_type_args);

    // Bind trait's own generic params first so trait-scope references
    // in the default body (e.g. `Vector[T]` / `Option[T]` where `T` is
    // the trait's `T`) resolve to the concrete trait arg supplied by
    // the impl. Pushed BEFORE Self + impl subs so trait bindings win
    // when names collide with impl-scope names (e.g. `equip [Iter, T,
    // U, F] MapIter[...] with Iterator[U]` has a local `T` distinct
    // from trait's `T` which maps to `U`).
    let mut trait_subs: Vec<(String, Type)> = Vec::new();
    if let Some(ref trait_ref) = equip.trait_ {
        if let Type::Named { generic_args, .. } = &trait_ref.trait_name.node {
            // Trait generic param names are discovered by walking the
            // ast module for the matching trait def. The lowering
            // context doesn't carry them directly; look them up via
            // the analysis traits registry.
            let trait_name = super::traits::extract_trait_name(&trait_ref.trait_name.node);
            if let Some(trait_info) = ctx.analysis.traits.traits.values()
                .find(|t| t.name == trait_name)
            {
                let trait_args: Vec<Type> = generic_args.iter()
                    .map(|a| generics::substitute_type_pub(&a.node, &subs))
                    .collect();
                for (name, concrete) in trait_info.trait_generic_params.iter().zip(trait_args.iter()) {
                    trait_subs.push((name.clone(), concrete.clone()));
                }
            }
        }
    }

    // `Self` → substituted equipped type, so trait-default method bodies
    // lifted to `Iterator[T]` (like
    // `MapIter[Self, T, U, F] map[U, F](self, F f)`) pre-substitute to
    // `MapIter[VectorIter[int], T, U, F]` before mangling. The bulk
    // emission path in `lower_generic_equip_methods_with_defaults` adds
    // the same binding; method-level-generic defaults reach this path
    // via `find_default_trait_method` in the per-call-site dispatch and
    // without the Self entry the body's `MapIter[Self, T, U, F](self, f)`
    // mangles to `MapIter__unknown__...` and the constructor undefined-
    // references at link time.
    let mut merged: Vec<(String, Type)> = Vec::new();
    merged.extend(trait_subs);
    merged.push(("Self".to_string(), substituted_equipped_type.clone()));
    merged.extend(subs);
    let subs = merged;

    // Shared context setup: type-name subs + generic type param TypeId map.
    // Both drive method-body lowering decisions (struct init, method dispatch).
    build_type_name_subs(ctx, &subs);
    if let Type::Named { name, generic_args } = &equip.type_.node {
        let base_name = &name.node;
        if !generic_args.is_empty() {
            let template_mangled = super::types::mangle_generic_name(base_name, generic_args);
            if template_mangled != mangled_type_name {
                ctx.generics.type_name_subs.insert(template_mangled, mangled_type_name.to_string());
            }
        }
    }
    build_generic_type_params(ctx, &subs);

    // Pre-substitute the body so nested generic type-arg lists like
    // `MapIter[VectorIter[T], T, U, F](self, f)` mangle directly to
    // `MapIter__VectorIter__int64_t__int64_t__int64_t__GorgetClosure`.
    // The string-replacement substitution that fires later can't handle
    // adjacent `__T__T__` patterns (Rust's `replace` consumes underscores
    // non-overlappingly, leaving the trailing T orphaned).
    let substituted_method = generics::substitute_function_body_pub(method, &subs);
    lower_equip_method_with_subs(
        ctx, module, &substituted_method, &subs,
        mangled_type_name, mangled_symbol,
        &substituted_equipped_type,
    );

    ctx.generics.type_name_subs.clear();
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
}

/// Build type parameter substitutions from generic params + concrete type args.
fn build_subs(
    generic_params: Option<&Spanned<ast::GenericParams>>,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    let mut subs = Vec::new();
    if let Some(params) = generic_params {
        for (param, arg) in params.node.params.iter().zip(type_args.iter()) {
            let name = match &param.node {
                GenericParam::Type { name: s, .. } => s.node.clone(),
                GenericParam::Const { name, .. } => name.node.clone(),
            };
            subs.push((name, arg.node.clone()));
        }
    }
    subs
}

/// Build type parameter substitutions for an equip block.
fn build_equip_subs(
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    if let Some(ref gp) = equip.generic_params {
        return build_subs(Some(gp), type_args);
    }
    // Fall back: extract params from the equipped type's generic args
    if let Type::Named { generic_args, .. } = &equip.type_.node {
        let mut subs = Vec::new();
        for (param_type, arg) in generic_args.iter().zip(type_args.iter()) {
            if let Type::Named { name, generic_args: inner } = &param_type.node {
                if inner.is_empty() {
                    subs.push((name.node.clone(), arg.node.clone()));
                }
            }
        }
        return subs;
    }
    Vec::new()
}

/// Substitute type parameters in an AST type and map to GIR TypeId.
fn substitute_and_map_type(
    ctx: &LoweringContext,
    ty: &Type,
    subs: &[(String, Type)],
) -> TypeId {
    let substituted = generics::substitute_type_pub(ty, subs);
    ctx.type_mapper.map_ast_type(&substituted)
}

/// Extract the return type of a callable/function parameter type.
///
/// For parameters like `Callable[T(int)]` or `int(int)`, extracts the return type
/// after applying generic substitutions. Returns None if the type isn't a callable.
fn extract_callable_return_type(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
) -> Option<TypeId> {
    extract_callable_return_type_bounded(ty, subs, ctx, 8)
}

fn extract_callable_return_type_bounded(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
    depth: u32,
) -> Option<TypeId> {
    if depth == 0 {
        return None;
    }
    match ty {
        // Callable[RetType(Params...)] or MutCallable[...] or ConsumeCallable[...]
        Type::Named { name, generic_args } => {
            let name_str = name.node.as_str();
            if name_str == "Callable" || name_str == "MutCallable" || name_str == "ConsumeCallable" {
                // The generic_args should contain a single Function type
                if let Some(func_type) = generic_args.first() {
                    if let Type::Function { return_type, .. } = &func_type.node {
                        let ret_type = substitute_and_map_type(ctx, &return_type.node, subs);
                        return Some(ret_type);
                    }
                }
                return None;
            }
            // Generic type param bound to a Function or Callable at the call
            // site — recurse after substitution so method-level-generic params
            // like `F f` (F → int(int)) get their return type picked up.
            if generic_args.is_empty() {
                for (param, concrete) in subs {
                    if param == name_str {
                        // Guard against degenerate self-loops: `T → Named{T, []}`
                        // (shouldn't happen, but cheap to check).
                        if let Type::Named { name: cn, generic_args: cgs } = concrete {
                            if cgs.is_empty() && cn.node == *name_str {
                                return None;
                            }
                        }
                        return extract_callable_return_type_bounded(concrete, subs, ctx, depth - 1);
                    }
                }
            }
            None
        }
        // Direct function type: RetType(Params...)
        Type::Function { return_type, .. } => {
            let ret_type = substitute_and_map_type(ctx, &return_type.node, subs);
            Some(ret_type)
        }
        _ => None,
    }
}

/// Extract the ARGUMENT types AND ownerships of a callable/function parameter
/// type. Parallels `extract_callable_return_type` for the sidecars
/// `callable_param_types` + `callable_param_ownerships` set at every site
/// that already sets `callable_return_type`. Types are PLAIN inner
/// (no MutPtr wrap) — matches the direct-call `fn_sigs` shape — and the
/// ownership per index feeds `lower_call_arg` via the same
/// `fn_param_ownerships` axis it uses on direct calls.
///
/// Returns `None` when the type isn't a callable; empty vecs mean "callable
/// with zero params". This is the WRITE site the Track-B1 SIGSEGV class
/// needs: the Function-type ARM discards `param_ownerships` elsewhere, so
/// the two indirect-call arg loops in `exprs/calls.rs` can't tell whether
/// the callee expected `int` or `*mut int` and pass the value bits either
/// way — segfault on the callee's pointer deref.
fn extract_callable_param_types(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
) -> Option<(Vec<TypeId>, Vec<Ownership>)> {
    extract_callable_param_types_bounded(ty, subs, ctx, 8)
}

fn extract_callable_param_types_bounded(
    ty: &Type,
    subs: &[(String, Type)],
    ctx: &LoweringContext,
    depth: u32,
) -> Option<(Vec<TypeId>, Vec<Ownership>)> {
    if depth == 0 {
        return None;
    }
    match ty {
        Type::Named { name, generic_args } => {
            let name_str = name.node.as_str();
            if name_str == "Callable" || name_str == "MutCallable" || name_str == "ConsumeCallable" {
                if let Some(func_type) = generic_args.first() {
                    if let Type::Function { params, param_ownerships, .. } = &func_type.node {
                        let mut types = Vec::with_capacity(params.len());
                        let mut owns = Vec::with_capacity(params.len());
                        for (i, p) in params.iter().enumerate() {
                            types.push(substitute_and_map_type(ctx, &p.node, subs));
                            owns.push(param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow));
                        }
                        return Some((types, owns));
                    }
                }
                return None;
            }
            // Generic type param bound to a Callable at the call site — same
            // recursion pattern as extract_callable_return_type.
            if generic_args.is_empty() {
                for (param, concrete) in subs {
                    if param == name_str {
                        if let Type::Named { name: cn, generic_args: cgs } = concrete {
                            if cgs.is_empty() && cn.node == *name_str {
                                return None;
                            }
                        }
                        return extract_callable_param_types_bounded(concrete, subs, ctx, depth - 1);
                    }
                }
            }
            None
        }
        Type::Function { params, param_ownerships, .. } => {
            let mut types = Vec::with_capacity(params.len());
            let mut owns = Vec::with_capacity(params.len());
            for (i, p) in params.iter().enumerate() {
                types.push(substitute_and_map_type(ctx, &p.node, subs));
                owns.push(param_ownerships.get(i).copied().unwrap_or(Ownership::Borrow));
            }
            Some((types, owns))
        }
        _ => None,
    }
}

/// Build generic type parameter → concrete TypeId substitutions.
///
/// For each type parameter (e.g., T), maps it to the concrete TypeId
/// (e.g., I64_TYPE for int). This enables `map_type_with_subs` to resolve
/// bare type parameters in variable declarations inside generic bodies.
pub(super) fn build_generic_type_params(ctx: &mut LoweringContext, subs: &[(String, Type)]) {
    ctx.generics.generic_type_params.clear();
    ctx.generics.generic_param_ast_types.clear();
    for (param_name, concrete_ty) in subs {
        let type_id = ctx.type_mapper.map_ast_type(concrete_ty);
        ctx.generics.generic_type_params.insert(param_name.clone(), type_id);
        ctx.generics.generic_param_ast_types.insert(param_name.clone(), concrete_ty.clone());
    }
}

/// Build type name substitution map for generic body lowering.
///
/// For each registered type name that contains a type parameter placeholder
/// (e.g., `Container__T`), computes the concrete mangled name (e.g.,
/// `Container__int64_t`) and stores the mapping in ctx.generics.type_name_subs.
pub(super) fn build_type_name_subs(ctx: &mut LoweringContext, subs: &[(String, Type)]) {
    ctx.generics.type_name_subs.clear();

    // Build a map of param-mangled-fragment → concrete-mangled-fragment.
    // E.g., for sub T → int:  "T" → "int64_t"
    // For sub T → str:  "T" → "Str"
    let fragment_subs: Vec<(String, String)> = subs.iter().map(|(param, concrete_ty)| {
        let concrete_name = super::types::mangle_type_for_name(concrete_ty);
        (param.clone(), concrete_name)
    }).collect();

    // Store fragment subs for on-the-fly resolution of names not in the pre-computed map
    ctx.generics.generic_fragment_subs = fragment_subs.clone();

    // Scan all known type names in the registry for template patterns.
    // For each name like "Container__T", substitute "T" → "int64_t" to get "Container__int64_t".
    let type_names: Vec<String> = ctx.type_registry.type_defs().iter()
        .map(|def| def.name.clone())
        .collect();
    for name in type_names {
        let mut substituted = name.clone();
        let mut changed = false;
        for (param, concrete) in &fragment_subs {
            // Match `__T` at end of name AND `__T__` anywhere in the middle —
            // both can fire on the same name (e.g. `A__T__B__T`), so we don't
            // short-circuit between them.
            let pattern_mid = format!("__{param}__");
            if substituted.contains(&pattern_mid) {
                substituted = substituted.replace(&pattern_mid, &format!("__{concrete}__"));
                changed = true;
            }
            let pattern_end = format!("__{param}");
            if substituted.ends_with(&pattern_end) {
                let prefix = &substituted[..substituted.len() - pattern_end.len()];
                substituted = format!("{prefix}__{concrete}");
                changed = true;
            }
        }
        if changed && name != substituted {
            ctx.generics.type_name_subs.insert(name, substituted);
        }
    }
}

/// Every `Block` an expression carries, at any depth, via the one exhaustive
/// child enumeration (`parser::visitor::visit_expr_children`).
fn collect_blocks_in_expr<'a>(e: &'a Expr, out: &mut Vec<&'a crate::parser::ast::Block>) {
    let mut children: Vec<&'a Spanned<Expr>> = Vec::new();
    let mut blocks: Vec<&'a crate::parser::ast::Block> = Vec::new();
    crate::parser::visitor::visit_expr_children(
        e,
        &mut |c: &'a Spanned<Expr>| children.push(c),
        &mut |b: &'a crate::parser::ast::Block| blocks.push(b),
    );
    out.extend(blocks);
    for c in children {
        collect_blocks_in_expr(&c.node, out);
    }
}
