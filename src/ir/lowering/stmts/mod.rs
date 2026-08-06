// Visible so exprs can share the get-chain field-place fallback with assign
// faces (Family-3 / Core #4).
pub(in crate::ir::lowering) mod assigns;
// Visible inside `ir::lowering` so the string-comprehension lowering in
// `exprs/collections.rs` can reuse `lower_for_string` (Chain C item 7).
pub(in crate::ir::lowering) mod for_loops;
mod patterns;
use assigns::*;
use for_loops::*;
pub use patterns::*;

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, BinaryOp, Block, Expr, Pattern, SelectOp, Stmt};
use crate::span::Spanned;

use super::context::{
    LoweringContext, MaterializeDirective, MaterializePosition, SharedLocalInfo, SharedLocalKind,
};
use super::drops::DropScopeKind;
use super::exprs::{
    lower_expr, infer_operand_type_full, maybe_auto_propagate, emit_operator_overload_call,
};

/// If `operand` is `Constant::GlobalRef(name)` referencing a module-level
/// global whose type needs drop (`String`, collections, …), emit a clone
/// and return the cloned operand. Otherwise pass through.
///
/// Rationale: `String DT_LOCAL = "literal"` lowers to a heap-allocated
/// `GorgetString` initialised at program start. Reading it by name
/// produces `GlobalAddr+Load` in LIR — a shallow byte-copy of the
/// global's struct that aliases its heap buffer. If the consumer (var
/// binding, return slot, struct field init, …) treats the value as
/// owned, scope-exit drop frees the global's buffer; the next read of
/// the global re-frees the same buffer → double-free. Cloning at the
/// boundary gives the consumer a fresh independent allocation.
///
/// Sites that need a borrow (`&GLOBAL`, call args by &/bare pointer)
/// rewrite `GlobalRef → GlobalRefPtr` in their own paths and never
/// reach this helper.
pub(super) fn clone_resource_global_ref(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
    span: crate::span::Span,
) -> Operand {
    let name = match &operand {
        Operand::Constant(Constant::GlobalRef(n)) => n.clone(),
        _ => return operand,
    };
    // `String FOO = "literal"` — backed by a cap=0 rodata view in the C/LLVM
    // emit. The shallow byte-copy is safe (the buffer is immortal `.rodata`),
    // and any drop the consumer emits is a runtime no-op (cap=0 fast-path in
    // `gorget_string_free`). No clone needed, no implicit-clone warning.
    // See `lower_static_decl` for the writer of `string_literal_view_globals`.
    if ctx.string_literal_view_globals.contains(&name) {
        return operand;
    }
    let type_name = match ctx.global_type_names.get(&name) {
        Some(t) => t.clone(),
        None => return operand,
    };
    let global_ty = match super::exprs::lookup_global_type(ctx, &type_name) {
        Some(t) => t,
        None => return operand,
    };
    if !ctx.type_registry.is_resource_type(global_ty) {
        return operand;
    }
    let clone_fn = match ctx.clone_fn_for_ptr(global_ty) {
        Some(f) => f,
        None => return operand,
    };
    // Pass &GLOBAL (GlobalRefPtr) to the clone fn. Clone ABIs are
    // `gorget_string_clone_to_owned(const GorgetString*)`,
    // `gorget_array_clone(const GorgetArray*)`, etc.
    let cloned = ctx.emit_clone(
        builder,
        &clone_fn,
        vec![Operand::Constant(Constant::GlobalRefPtr(name))],
        span,
        global_ty,
        crate::ir::ImplicitCloneReason::ReturnFromBorrow,
    );
    ctx.drops.register_local(cloned, global_ty, &ctx.type_registry);
    ctx.set_owned_fresh(builder, cloned);
    FunctionBuilder::copy(cloned)
}

/// If the operand came from a resource-type field load, emit a MoveZero for the source field
/// to prevent double-free. Call this after assigning the operand to its destination.
/// If the local's declared AST type is (or resolves to) a `Callable` or bare
/// function type, return its mapped GIR return type. Used to thread
/// `callable_return_types` entries for closure-typed locals — otherwise a
/// call like `cb(x)` (where `cb` was bound via `F cb = self.f`) has no
/// return-type info and falls back to I64.
fn callable_local_return_type(ctx: &LoweringContext, ty: &ast::Type) -> Option<TypeId> {
    callable_local_return_type_bounded(ctx, ty, 8)
}

fn callable_local_return_type_bounded(
    ctx: &LoweringContext,
    ty: &ast::Type,
    depth: u32,
) -> Option<TypeId> {
    if depth == 0 {
        return None;
    }
    match ty {
        ast::Type::Named { name, generic_args } => {
            let name_str = name.node.as_str();
            if matches!(name_str, "Callable" | "MutCallable" | "ConsumeCallable") {
                if let Some(func_type) = generic_args.first() {
                    if let ast::Type::Function { return_type, .. } = &func_type.node {
                        return Some(ctx.map_type_with_subs(&return_type.node));
                    }
                }
                return None;
            }
            // Bare type param bound to a Function or Callable at the call
            // site — look up the concrete substituted AST type and recurse.
            // Essential for method-level-generic params like `F` inside
            // `Option[U] next(&self): F cb = self.f; ...` where F resolves
            // to `Option[int](int)` at this call site.
            if generic_args.is_empty() {
                if let Some(concrete) = ctx.generics.generic_param_ast_types.get(name_str).cloned() {
                    // Degenerate self-loop guard: `T → Named{T, []}`.
                    if let ast::Type::Named { name: cn, generic_args: cgs } = &concrete {
                        if cgs.is_empty() && cn.node == *name_str {
                            return None;
                        }
                    }
                    return callable_local_return_type_bounded(ctx, &concrete, depth - 1);
                }
            }
            None
        }
        ast::Type::Function { return_type, .. } => {
            Some(ctx.map_type_with_subs(&return_type.node))
        }
        _ => None,
    }
}

/// If the local's declared AST type is (or resolves to) a `Callable` or bare
/// function type, return the mapped GIR TypeIds of its params (plain inner —
/// no MutPtr wrap) and the parallel `Ownership` per index. Parallels
/// `callable_local_return_type` for the sidecars the indirect-call
/// arg-emit loops in `exprs/calls.rs` read to route pointer-expecting args
/// through `lower_call_arg` (Track B1 write-site fix). Returns `None` when
/// `ty` isn't a callable; empty vecs mean "callable with zero params".
fn callable_local_param_types(
    ctx: &LoweringContext,
    ty: &ast::Type,
) -> Option<(Vec<TypeId>, Vec<crate::parser::ast::Ownership>)> {
    callable_local_param_types_bounded(ctx, ty, 8)
}

fn callable_local_param_types_bounded(
    ctx: &LoweringContext,
    ty: &ast::Type,
    depth: u32,
) -> Option<(Vec<TypeId>, Vec<crate::parser::ast::Ownership>)> {
    if depth == 0 {
        return None;
    }
    match ty {
        ast::Type::Named { name, generic_args } => {
            let name_str = name.node.as_str();
            if matches!(name_str, "Callable" | "MutCallable" | "ConsumeCallable") {
                if let Some(func_type) = generic_args.first() {
                    if let ast::Type::Function { params, param_ownerships, .. } = &func_type.node {
                        let mut types = Vec::with_capacity(params.len());
                        let mut owns = Vec::with_capacity(params.len());
                        for (i, p) in params.iter().enumerate() {
                            types.push(ctx.map_type_with_subs(&p.node));
                            owns.push(param_ownerships.get(i).copied()
                                .unwrap_or(crate::parser::ast::Ownership::Borrow));
                        }
                        return Some((types, owns));
                    }
                }
                return None;
            }
            if generic_args.is_empty() {
                if let Some(concrete) = ctx.generics.generic_param_ast_types.get(name_str).cloned() {
                    if let ast::Type::Named { name: cn, generic_args: cgs } = &concrete {
                        if cgs.is_empty() && cn.node == *name_str {
                            return None;
                        }
                    }
                    return callable_local_param_types_bounded(ctx, &concrete, depth - 1);
                }
            }
            None
        }
        ast::Type::Function { params, param_ownerships, .. } => {
            let mut types = Vec::with_capacity(params.len());
            let mut owns = Vec::with_capacity(params.len());
            for (i, p) in params.iter().enumerate() {
                types.push(ctx.map_type_with_subs(&p.node));
                owns.push(param_ownerships.get(i).copied()
                    .unwrap_or(crate::parser::ast::Ownership::Borrow));
            }
            Some((types, owns))
        }
        _ => None,
    }
}

/// Lower a block of statements.
pub fn lower_block(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &Block,
) {
    for stmt in &block.stmts {
        lower_stmt(ctx, builder, stmt);
    }
}

/// Lower a block of statements in a new lexical scope (saves/restores locals).
pub fn lower_block_scoped(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &Block,
) {
    let saved = ctx.save_locals(builder);
    lower_block(ctx, builder, block);
    ctx.restore_locals(builder, saved);
}

/// Lower a single statement.
///
/// Per-statement-kind timing is recorded as EXCLUSIVE (self) time into
/// `ctx.lower_fn_sub_times` under keys `lower_function::body::lower_block::stmt::<kind>`,
/// computed as `elapsed - (ctx.stmt_nested_dur delta during this call)`.
/// Each call adds its own `elapsed` to `ctx.stmt_nested_dur` so the parent
/// `lower_stmt` invocation (if any) subtracts our wall time back out — this
/// makes the per-kind buckets sum to `body::lower_block` total, no double
/// counting from `Stmt::If`/`Stmt::Match`/`Stmt::For` recursing into nested
/// stmts. The pattern mirrors how lower_function's body timer subtracts
/// nested meta_expand cost from itself.
pub fn lower_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    stmt: &Spanned<Stmt>,
) {
    let __stmt_t0 = std::time::Instant::now();
    let __nested_at_entry = ctx.stmt_nested_dur;
    let __kind_key: &'static str;

    builder.set_span(stmt.span);
    match &stmt.node {
        Stmt::VarDecl {
            type_,
            pattern,
            value,
            shared,
            ..
        } => {
            __kind_key = if *shared != ast::SharedKind::None {
                "lower_function::body::lower_block::stmt::shared_var_decl"
            } else {
                "lower_function::body::lower_block::stmt::var_decl"
            };
            if *shared != ast::SharedKind::None {
                lower_shared_var_decl(ctx, builder, type_, pattern, value, shared);
            } else {
                lower_var_decl(ctx, builder, type_, pattern, value, stmt.span);
            }
        }

        Stmt::Assign { target, value } => {
            __kind_key = "lower_function::body::lower_block::stmt::assign";
            lower_assign(ctx, builder, target, value);
        }

        Stmt::CompoundAssign { target, op, value } => {
            __kind_key = "lower_function::body::lower_block::stmt::compound_assign";
            lower_compound_assign(ctx, builder, target, *op, value);
        }

        Stmt::Return(expr) => {
            __kind_key = "lower_function::body::lower_block::stmt::return";
            lower_return(ctx, builder, expr.as_ref());
        }

        Stmt::Expr(expr) => {
            __kind_key = "lower_function::body::lower_block::stmt::expr";
            // Statement-end GuardKind drop (Round XIX Track Y): Mutex/RWLock
            // guard temps minted under an expression statement release at
            // statement end so sequential acquires do not self-deadlock.
            // Non-Guard droppables re-register into the parent scope.
            // Named binds / VarDecl / Assign / `with` do NOT push Statement.
            ctx.drops.push_scope(DropScopeKind::Statement);
            let val = lower_expr(ctx, builder, expr);
            // Auto-propagate: if the expression returns Result in a propagation
            // context, unwrap it so errors aren't silently swallowed.
            // may emit_early_exit_drops + terminate on the error path.
            let _ = maybe_auto_propagate(ctx, builder, val, expr.span);
            if builder.is_terminated() {
                // Drops already emitted on early exit — same contract as Block.
                ctx.drops.pop_scope_no_emit();
            } else {
                // Split-borrow drops / type_mapper / type_registry so the
                // GuardKind predicate can read typed metadata while we pop.
                let type_mapper = &ctx.type_mapper;
                let type_registry = &ctx.type_registry;
                ctx.drops.pop_statement_guard_temps(builder, type_registry, |tid| {
                    type_mapper.guard_kind(tid).is_some()
                });
            }
        }

        Stmt::Pass => {
            __kind_key = "lower_function::body::lower_block::stmt::pass";
            builder.nop();
        }

        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => {
            __kind_key = "lower_function::body::lower_block::stmt::if";
            // Planner consumer #1: pre-scope bare-param materialize (one shared
            // hoist entry for every non-loop scope form; loops ride
            // `materialize_loop_carried_bare_params` instead). Runs in THIS
            // pre-scope block before the scope fn's first `save_locals`.
            materialize_scope_carried_bare_params(ctx, builder, &stmt.node, stmt.span);
            lower_if(ctx, builder, condition, then_body, elif_branches, else_body);
        }

        Stmt::While {
            condition,
            body,
            else_body,
            ..
        } => {
            __kind_key = "lower_function::body::lower_block::stmt::while";
            lower_while(ctx, builder, condition, body, else_body.as_ref());
        }

        Stmt::For {
            pattern,
            ownership,
            iterable,
            body,
            else_body,
        } => {
            __kind_key = "lower_function::body::lower_block::stmt::for";
            lower_for(ctx, builder, pattern, *ownership, iterable, body, else_body.as_ref());
        }

        Stmt::Loop { body } => {
            __kind_key = "lower_function::body::lower_block::stmt::loop";
            lower_loop(ctx, builder, body);
        }

        Stmt::Break => {
            __kind_key = "lower_function::body::lower_block::stmt::break_continue";
            lower_break(ctx, builder);
        }

        Stmt::Continue => {
            __kind_key = "lower_function::body::lower_block::stmt::break_continue";
            lower_continue(ctx, builder);
        }

        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => {
            __kind_key = "lower_function::body::lower_block::stmt::match";
            // Planner consumer #1: pre-scrutinee bare-param materialize (arm
            // bodies + guards, via the shared collector). Dominates every arm's
            // save/restore. See `materialize_scope_carried_bare_params`.
            materialize_scope_carried_bare_params(ctx, builder, &stmt.node, stmt.span);
            lower_match_stmt(ctx, builder, scrutinee, arms, else_arm);
        }

        Stmt::Throw(expr) => {
            __kind_key = "lower_function::body::lower_block::stmt::throw";
            lower_throw(ctx, builder, expr);
        }

        Stmt::Assert { condition, message } => {
            __kind_key = "lower_function::body::lower_block::stmt::assert";
            lower_assert(ctx, builder, condition, message.as_ref());
        }

        Stmt::AssertReturn { condition, message } => {
            __kind_key = "lower_function::body::lower_block::stmt::assert_return";
            if !ctx.strip_asserts {
                ctx.func_state.postconditions.push((condition.clone(), message.clone()));
            }
        }

        Stmt::Snapshot { name, value } => {
            __kind_key = "lower_function::body::lower_block::stmt::snapshot";
            if ctx.snapshot_mode {
                lower_snapshot(ctx, builder, name, value);
            }
        }

        Stmt::With { bindings, body } => {
            __kind_key = "lower_function::body::lower_block::stmt::with";
            // Planner consumer #1: pre-scope materialize. Straight-line scope
            // (single predecessor) — entry hoist dominates. Covers the `with`
            // BINDINGS too (they lower inside the save/restore). See the shared fn.
            materialize_scope_carried_bare_params(ctx, builder, &stmt.node, stmt.span);
            lower_with(ctx, builder, bindings, body);
        }

        Stmt::Unsafe { body } => {
            __kind_key = "lower_function::body::lower_block::stmt::unsafe";
            // Planner consumer #1: pre-scope materialize (straight-line scope).
            materialize_scope_carried_bare_params(ctx, builder, &stmt.node, stmt.span);
            lower_block_scoped(ctx, builder, body);
        }

        Stmt::NamedScope { body, .. } => {
            __kind_key = "lower_function::body::lower_block::stmt::named_scope";
            // Planner consumer #1: pre-scope materialize (straight-line scope).
            materialize_scope_carried_bare_params(ctx, builder, &stmt.node, stmt.span);
            lower_named_scope(ctx, builder, body);
        }

        Stmt::Item(_) => {
            __kind_key = "lower_function::body::lower_block::stmt::item_or_meta";
            /* Nested items are hoisted — no-op in GIR */
        }

        Stmt::Select { arms, else_arm: _ } => {
            __kind_key = "lower_function::body::lower_block::stmt::select";
            // Planner consumer #1: pre-scope materialize before the spin-loop jump
            // (dominates every recv-arm body). The collector's `Stmt::Select` arm
            // also scans the else body — a harmless unobserved pre-materialize,
            // since the dispatcher discards `else_arm` (a select-else body is never
            // lowered). See the shared fn.
            materialize_scope_carried_bare_params(ctx, builder, &stmt.node, stmt.span);
            lower_select(ctx, builder, arms);
        }

        // meta if/for/match/while should have been evaluated and removed before GIR lowering.
        // If they appear here it means they were in a non-generic context (a semantic
        // error should have been emitted) — emit nothing.
        Stmt::MetaIf { .. } | Stmt::MetaFor { .. } | Stmt::MetaMatch { .. }
        | Stmt::MetaWhile { .. } | Stmt::MetaConst { .. } | Stmt::MetaLog { .. } => {
            __kind_key = "lower_function::body::lower_block::stmt::item_or_meta";
        }

        Stmt::OnError { body } => {
            __kind_key = "lower_function::body::lower_block::stmt::on_error";
            // Register the cleanup block — it will be emitted on error paths
            ctx.func_state.on_error_blocks.push(body.clone());
        }
    }

    let __elapsed = __stmt_t0.elapsed();
    let __nested_during = ctx.stmt_nested_dur - __nested_at_entry;
    let __exclusive = __elapsed.saturating_sub(__nested_during);
    *ctx.lower_fn_sub_times.entry(__kind_key).or_default() += __exclusive;
    // Report our wall time back to any parent `lower_stmt` so it subtracts
    // us out and only counts its own self-time.
    ctx.stmt_nested_dur += __elapsed;
}

/// Emit accumulated `on error` cleanup blocks in LIFO order.
/// Called on error exit paths (throw, try-error, rethrow-error) BEFORE drop elaboration.
pub fn emit_on_error_cleanups(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if ctx.func_state.on_error_blocks.is_empty() {
        return;
    }
    // Clone the blocks to avoid borrow conflicts (lowering each block borrows ctx mutably)
    let blocks: Vec<_> = ctx.func_state.on_error_blocks.iter().rev().cloned().collect();
    for block in &blocks {
        lower_block(ctx, builder, block);
    }
}

/// Lower a variable declaration.
fn lower_var_decl(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_: &Spanned<ast::Type>,
    pattern: &Spanned<Pattern>,
    value: &Spanned<Expr>,
    stmt_span: crate::span::Span,
) {
    match &pattern.node {
        Pattern::Binding(name) => {
            let gir_type = ctx.resolve_var_type(type_, value);
            // resolve_var_type → map_type_with_subs → map_ast_type (immutable) returns UNIT_TYPE for
            // unregistered generic types. Fall back to map_ast_type_mut which can auto-register them
            // (Callable, ReadGuard, WriteGuard, RWLock, etc.).
            let gir_type = if gir_type == crate::ir::types::UNIT_TYPE {
                match type_.node {
                    ast::Type::Named { name: _, ref generic_args } if !generic_args.is_empty() => {
                        ctx.type_mapper.map_ast_type_mut(&type_.node, &mut ctx.type_registry)
                    }
                    // A non-empty tuple type (`(int, int) t = (5, 7)`)
                    // maps to UNIT_TYPE on the immutable path when its
                    // `Tuple__…` TypeDef hasn't been registered yet —
                    // `try_map_ast_type` only LOOKS UP tuples, it doesn't
                    // create them. Without this fallback the declared
                    // local stays `unit`-typed and the subsequent
                    // `_dst = copy <tuple temp>` memcpy reads garbage (the
                    // all-scalar / scalar-first case; resource-element
                    // tuples happened to be registered elsewhere). Use the
                    // mut path to register-on-the-fly, same as the
                    // `auto`/`return` paths' `register_tuple_type`. Empty
                    // tuples correctly stay UNIT.
                    ast::Type::Tuple(ref elems) if !elems.is_empty() => {
                        ctx.type_mapper.map_ast_type_mut(&type_.node, &mut ctx.type_registry)
                    }
                    _ => gir_type,
                }
            } else {
                gir_type
            };
            // Box[Callable[...]] variables pre-register with a "Box__Callable__unknown" type from the
            // generic collector. We need to reinfer from the actual RHS to get the real closure type.
            // Read the typed `metadata.is_box` flag + inner-type's `c_runtime_alias == "GorgetClosure"`
            // rather than probing the compound name prefix. Callable variants
            // (Callable / MutCallable / ConsumeCallable) all carry the
            // `c_runtime_alias = "GorgetClosure"` typed flag at registration.
            let gir_type_is_box_callable = if ctx.type_registry.is_box(gir_type) {
                if let Some(tn) = ctx.type_name_for_id(gir_type) {
                    if let Some(inner_name) = tn.strip_prefix("Box__") {
                        ctx.type_registry
                            .get_type_def(inner_name)
                            .and_then(|td| td.metadata.c_runtime_alias.as_deref())
                            == Some("GorgetClosure")
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };
            let local_id = builder.add_local(gir_type, Some(name));
            ctx.register_local(name, local_id, gir_type);
            // Track callable sidecars for locals declared with a Callable or
            // bare-function type. Enables `cb(...)` call-site return-type
            // inference AND indirect-call arg lowering when `F cb = self.f`
            // binds a closure field and F is a method-level-generic param that
            // resolves to a Function type. Fused via `set_callable_sig` — the
            // layering chokepoint (Core #4/#6) shared with the four
            // function-registration paths in `src/ir/lowering/functions.rs`.
            let ret = callable_local_return_type(ctx, &type_.node);
            let sig = callable_local_param_types(ctx, &type_.node);
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
            // P2.6: Register Move-type locals for drop at scope exit
            ctx.drops.register_local(local_id, gir_type, &ctx.type_registry);
            // Force-register Option/Result with resource payloads (needs_drop
            // returns false because the type upgrade scan hasn't run yet).
            if !ctx.drops.is_registered(local_id) {
                if let Some(crate::ir::types::GirType::Named(tn)) = ctx.type_registry.get(gir_type).cloned() {
                    // Read typed `enum_category` (Option/Result discriminator)
                    // instead of name-prefix matching the type name. Coalesce
                    // the two `get_type_def(&tn)` lookups (one for the
                    // is_opt_or_result gate, one for the Enum-kind walk) into
                    // a single fetch — the original shape did the same
                    // FxHashMap-by-name lookup twice. The borrow released
                    // before any `&ctx.type_registry` use deeper in the loop.
                    if let Some(td) = ctx.type_registry.get_type_def(&tn) {
                        let is_opt_or_result = td.metadata.enum_category.is_some();
                        if is_opt_or_result {
                            if let crate::ir::types::TypeDefKind::Enum(ref edef) = td.kind {
                                let droppable = edef.variants.iter().any(|v| v.fields.iter().any(|f| {
                                    ctx.type_registry.needs_drop(f.type_id)
                                    || ctx.type_registry.is_resource_type(f.type_id)
                                    || matches!(ctx.type_registry.get(f.type_id),
                                        Some(crate::ir::types::GirType::Named(n))
                                        if n == "GorgetString" || ctx.type_registry.is_collection_type_name(n))
                                }));
                                if droppable {
                                    ctx.drops.register_local_unconditional(local_id, gir_type);
                                }
                            }
                        }
                    }
                }
            }
            // Register borrow dependencies for drop ordering.
            // If this local borrows from other locals, the drop elaborator
            // will ensure this local is dropped before its sources.
            if let Some(def_id) = ctx.analysis.scopes.lookup_def_by_span(name, pattern.span) {
                if let Some(source_def_ids) = ctx.analysis.borrow_deps.get(&def_id) {
                    for &source_def_id in source_def_ids {
                        let source_name = ctx.analysis.scopes.get_def(source_def_id).name.clone();
                        if let Some((source_local, _)) = ctx.lookup_local(&source_name) {
                            ctx.drops.add_borrow_dep(local_id, source_local);
                        }
                    }
                }
            }
            // Set expected type hint so enum variant constructors (Some, None, Ok, Error)
            // can pick the correctly-monomorphized type
            let prev_expected = ctx.func_state.expected_type;
            ctx.func_state.expected_type = Some(gir_type);
            let operand = lower_expr(ctx, builder, value);
            // `T x = MODULE_GLOBAL_RESOURCE`: clone the global so the new
            // binding owns its own allocation. See `clone_resource_global_ref`.
            let operand = clone_resource_global_ref(ctx, builder, operand, value.span);
            // Auto-propagate: if operand is Result-typed but the declared type is not Result,
            // unwrap it (propagating errors) so the binding gets the Ok value.
            // NOTE: must run before restoring expected_type so the guard sees gir_type.
            let mut operand = maybe_auto_propagate(ctx, builder, operand, value.span);
            ctx.func_state.expected_type = prev_expected;
            // If this was a Spawn expression, register the task local → spawned fn mapping
            if let Some(fn_name) = ctx.spawn.pending_fn.take() {
                ctx.spawn.result_locals.insert(local_id, fn_name);
            }
            // For auto/inferred types, closure values, and Box[Callable[...]] variables,
            // re-infer from the lowered operand to pick up the actual concrete type.
            // Also handle Shared[T]/Mutex[T]/Channel[T] and the non-generic TaskGroup,
            // whose TypeIds are registered lazily inside lower_call — at declaration time
            // map_ast_type returns UNIT_TYPE, but after the RHS is lowered the TypeId is
            // registered and the operand carries it.
            let gir_type_is_lazy_generic = gir_type == crate::ir::types::UNIT_TYPE && {
                if let ast::Type::Named { ref name, ref generic_args, .. } = type_.node {
                    // TaskGroup has no generic args; the others require at least one.
                    name.node.as_str() == "TaskGroup"
                        || (!generic_args.is_empty()
                            && matches!(name.node.as_str(),
                                "Shared" | "Weak" | "Mutex" | "Guard" | "Channel" | "Task"))
                } else {
                    false
                }
            };
            // Don't reinfer when gir_type is FnPtr (explicit Callable[T] declaration):
            // the Assign handler will pack closures/FuncRefs into GorgetClosure form.
            let gir_type_is_fnptr = matches!(ctx.type_registry.get(gir_type), Some(GirType::FnPtr { .. }));
            let needs_reinfer = !gir_type_is_fnptr && (
                matches!(type_.node, ast::Type::Inferred)
                || matches!(value.node, ast::Expr::Closure { .. } | ast::Expr::ImplicitClosure { .. })
                || gir_type_is_box_callable
                || gir_type_is_lazy_generic
            );
            // Set by the lazy loop-carried CoW branch when it has fully
            // emitted the bind (borrow_view into `s` + flag). The
            // unconditional trailing `builder.assign_mode(.., local_id,
            // operand)` MUST be skipped, else it re-clobbers `s`'s value slot
            // with the raw element struct (a double-free).
            let mut lazy_handled = false;
            if needs_reinfer {
                let inferred = infer_operand_type_with_builder(ctx, &operand, builder);
                if inferred != gir_type {
                    builder.locals[local_id.0 as usize].type_id = inferred;
                    ctx.register_local(name, local_id, inferred);
                    // Register for drop at CURRENT scope (not function scope).
                    // For `auto` VarDecl in a loop, the variable must be dropped at
                    // each iteration end. update_or_register_type registers at function
                    // scope (for CoW materializations), which leaks loop-body locals.
                    if !ctx.drops.is_registered(local_id) {
                        ctx.drops.register_local(local_id, inferred, &ctx.type_registry);
                    }
                    // Also update existing registrations (type changed from I64 to real type).
                    ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                }
            }
            // CoW: Ptr(T) bindings from BORROWED sources stay as borrows.
            // Only propagate Ptr for operands whose source local is currently
            // tracked as `LocalOwnership::Borrowed` (e.g. `set_bare_param`,
            // `set_ref`, `set_collection_ref`), not from owned function
            // returns or fresh allocations.
            if !needs_reinfer {
                let inferred = infer_operand_type_with_builder(ctx, &operand, builder);
                // Option[Ref[T]] → Option[T] conversion. `v.get(i)` returns
                // Option[Ref[T]] (16 bytes: {tag, void*}); the LHS Option[T]
                // is 40 bytes (tag + Str). Without conversion, the C backend
                // emits `memcpy(dst_40, src_16, sizeof(dst))` — an OOB read
                // that leaks 24 bytes of adjacent stack into the Str header.
                // With ASan this traps; without it, the stack-junk `len` makes
                // `print(s)` an arbitrary-memory info-leak primitive.
                //
                // Fix: emit a tag branch. On Some, load the Ref as a Ptr(T),
                // clone to an owned T, wrap in Option[T]. On None, construct
                // Option[T]::None. Merge and reassign operand so downstream
                // VarDecl stores the converted value into local_id.
                //
                // Mirrors the return-statement conversion ~line 1010.
                if let Some(converted) = try_lift_option_ref(
                    ctx, builder, &operand, inferred, gir_type, value.span,
                ) {
                    operand = converted;
                }
                // Type mismatch: declared type (e.g. int) vs RHS resource type (e.g. String).
                // This happens for `int ch = text.char_at(0)` where char_at returns String.
                // Reinfer to the RHS type so the variable's slot matches the value, preventing
                // the Move assign from storing a pointer-as-int into a mismatched slot.
                let inferred = infer_operand_type_with_builder(ctx, &operand, builder);
                // Skip the corrective when the declared type and the inferred
                // type both ultimately land on the same C runtime struct.
                // Concretely: a `Callable[int()]` local lowers to GirType::FnPtr,
                // but Callable values inside collections come back as
                // GirType::Named("Callable__T_args") with `c_runtime_alias =
                // "GorgetClosure"`. Both are 16-byte GorgetClosure handles —
                // overwriting the FnPtr type with the Named form breaks the
                // closure-call dispatch (`call @f1`-style codegen) at calls.rs.
                // Layering-discipline: the metadata field is on the typed
                // TypeDef, NOT a name match. Phase A residual #1.
                let gir_is_fnptr = matches!(ctx.type_registry.get(gir_type), Some(GirType::FnPtr { .. }));
                let inferred_is_alias_of_closure = ctx.type_registry.is_closure_runtime_type(inferred);
                let same_runtime_alias = gir_is_fnptr && inferred_is_alias_of_closure;
                if !same_runtime_alias
                    && inferred != gir_type
                    && !matches!(ctx.type_registry.get(inferred), Some(GirType::Ptr(_)))
                    && ctx.type_registry.is_resource_type(inferred)
                    && !ctx.type_registry.is_resource_type(gir_type)
                {
                    builder.locals[local_id.0 as usize].type_id = inferred;
                    ctx.register_local(name, local_id, inferred);
                    ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                }
                // Closure (Callable) var-decl auto-clone. `Callable[...] h = hook`
                // pre-fix lowered to a shallow Copy of the 16-byte GorgetClosure,
                // aliasing the source's env pointer with the destination.
                // Subsequent `push(h)` into a Vector triggered the Tier 2a
                // consume-site violation (untracked source consumed).
                //
                // The destination's `gir_type` is the load-bearing signal:
                // the user declared `Callable[...]`, which `map_ast_type_mut`
                // resolves to `GirType::FnPtr` (primitive sig) or Named with
                // `c_runtime_alias = "GorgetClosure"` (user-typed sig).
                // The source's `inferred` is unreliable here — Callable
                // parameters resolve to `UNIT_TYPE` at the immutable
                // `map_ast_type` path (intentional design for the void*
                // __callable_N ABI; see types.rs:60-110).
                //
                // Auto-clone unless the source is already Owned/FreshOwned
                // (e.g., fresh from a function-call return). Added 2026-05-12
                // as the Tier 2a consume_externs burn-down (see TODO).
                let gir_is_closure = matches!(ctx.type_registry.get(gir_type), Some(GirType::FnPtr { .. }))
                    || ctx.type_registry.is_closure_runtime_type(gir_type);
                if gir_is_closure {
                    // Narrow gate: only auto-clone when the source local's
                    // type is ALSO a closure-handle shape (FnPtr or
                    // c_runtime_alias = GorgetClosure) AND the source
                    // local is named (i.e., a callable parameter or a
                    // previously-bound Callable local), NOT an unnamed
                    // closure-literal temp.
                    //
                    // Closure literals lower to the user's __Closure_N
                    // struct type — passing that to gorget_closure_clone_to_owned
                    // fails the ABI (expects const GorgetClosure*, gets
                    // user-struct-by-value). Closure literals are fresh
                    // by construction and don't need clone.
                    //
                    // Callable params (Router::use's hook) and prior
                    // Callable bindings ARE closure-handle-shaped at the
                    // IR level (the param's local has Unit type for the
                    // void* __callable_N ABI, but the underlying value at
                    // runtime is a GorgetClosure handle).
                    let should_clone = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                        if !p.projections.is_empty() {
                            false
                        } else {
                            let src_local = builder.locals.get(p.local.0 as usize);
                            let src_owned = matches!(
                                src_local.map(|l| &l.ownership),
                                Some(crate::ir::LocalOwnership::Owned)
                                    | Some(crate::ir::LocalOwnership::FreshOwned)
                            );
                            let src_named = ctx.is_named_local(p.local);
                            // Source type: is it a `Ptr<GorgetClosure>` from a
                            // `coll.get(k).unwrap()` on a Callable-element
                            // collection? (`Option__Ref__Callable__T.unwrap()`
                            // produces a Ptr-typed temp marked `CowBorrow`.)
                            // The default `!src_owned && src_named` rule
                            // excludes temps — but THIS temp's pointee is a
                            // closure handle, and unlike Vector/Dict/String
                            // pointees, the downstream `g(args)` invocation
                            // path doesn't auto-deref. Force the clone here so
                            // `g` binds to a fresh `GorgetClosure` value and
                            // direct-call works.
                            let src_is_ptr_to_closure = src_local
                                .map(|l| l.type_id)
                                .and_then(|t| ctx.pointee_type(t))
                                .map(|inner| ctx.type_registry.is_closure_runtime_type(inner))
                                .unwrap_or(false);
                            // Already Owned/FreshOwned (fresh local from
                            // call result) → no clone needed.
                            // Not named (closure literal temp) → ABI-
                            // incompatible source struct, skip clone.
                            // Ptr-to-closure → clone unconditionally (the
                            // declared `Callable[T] g = …` demands a value
                            // binding; CowBorrow propagation would retype to
                            // Ptr and break direct invocation).
                            (!src_owned && src_named) || src_is_ptr_to_closure
                        }
                    } else { false };
                    if should_clone {
                        // G3 EXEMPT: this closure clone emits via `call_extern`
                        // (`Instruction::CallExtern`), not `Instruction::Call`, so
                        // `call_clone` cannot wrap it and the Call-only clone-reason
                        // validator cannot see it. Left warn+call_extern as-is; the
                        // `CallExtern.reason` field is a filed follow-up (TODO).
                        ctx.warn_clone_and_hit(builder, value.span, gir_type, crate::ir::ImplicitCloneReason::ClosureCapture);
                        let cloned = builder.call_extern(
                            "gorget_closure_clone_to_owned",
                            vec![operand.clone()],
                            gir_type,
                        );
                        ctx.drops.register_local(cloned, gir_type, &ctx.type_registry);
                        ctx.set_owned_fresh(builder, cloned);
                        operand = FunctionBuilder::copy(cloned);
                    }
                }
                if let Some(GirType::Ptr(_inner)) = ctx.type_registry.get(inferred).cloned() {
                    if !matches!(ctx.type_registry.get(gir_type), Some(GirType::Ptr(_))) {
                        // Check: is the source a Ptr borrow safe to propagate?
                        // - BareParam: borrows from caller, lifetime = function scope.
                        // - CowBorrow with provenance: borrows from a collection via
                        //   .get().unwrap(). Tracked as CollectionRef so
                        //   cow_before_mutation materializes when the collection is mutated.
                        let (source_is_bare_param, source_is_cow_borrow, source_field_origin) = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                            if p.projections.is_empty() {
                                let cow = ctx.is_cow_borrow(builder, p.local) && ctx.cow_borrow_source(p.local).is_some();
                                (ctx.is_bare_param(builder, p.local), cow, ctx.field_borrow_origin(builder, p.local))
                            } else {
                                (false, false, None)
                            }
                        } else { (false, false, None) };

                        let in_loop = ctx.current_loop().is_some();
                        // Allow borrow propagation in loops when the variable is
                        // not reassigned on any forward path from this statement.
                        // Flow-sensitive: only blocks propagation when the name is
                        // reassigned AFTER this VarDecl, not globally in the function.
                        let safe_in_loop = !ctx.is_cow_unsafe_at(name, stmt_span.start);
                        // **Tier 2a Phase 3 (2026-05-10):** the historical
                        // bare-param branch silently changed `local_id`'s
                        // type from `gir_type` (value resource) to
                        // `inferred` (Ptr<resource>) when the user wrote
                        // `String x = some_param` shapes. Downstream
                        // consumers then auto-deref-and-memcpy the
                        // borrowed pointee into owned slots — the
                        // dominant Snag #28-class bug pattern (412 of
                        // 764 AssignIntoOwnedSlot violations pre-fix; -47%
                        // of the Borrowed class after this branch was
                        // removed). We're in this subtree because
                        // `gir_type` is NOT a Ptr (line 426 gate), so
                        // the user explicitly declared a value type and
                        // expects an owned binding. Flow now falls
                        // through to the `clone_fn_for_ptr` clone branch
                        // (`:494`) which emits the sound clone-then-Move
                        // shape.
                        let _ = source_is_bare_param;
                        // Tier 2a Phase 3 (residual): require strict
                        // `safe_in_loop` for cow-borrow propagation —
                        // even outside a loop, a later reassign of
                        // `name` forces the assigns.rs writeback to
                        // materialise (clone) the Ptr into the value
                        // slot. The FIRST `String x =
                        // lines.get(0).unwrap()` shape with `x`
                        // reassigned later showed up as a borrowed-
                        // source-into-owned-slot consume in
                        // `string_reassign_loop`, `cow_borrow_basic`,
                        // and self-host's `format_*_lines` / `join` /
                        // `join_lines` defaults — the validator
                        // (correctly) flagged the alias-before-clone
                        // shape. The previous permissive
                        // `(!in_loop || safe_in_loop)` allowed the
                        // alias outside loops; tightening to strict
                        // `safe_in_loop` routes those cases to the
                        // `clone_fn_for_ptr` clone branch (`:494`)
                        // which emits the sound clone-then-Move IR up
                        // front and skips the reassign-time
                        // materialise.
                        // Source-mutation check (2026-05-13 follow-up to the
                        // `expand_derives` for-loop investigation): if the
                        // source collection is mutated later on any forward
                        // path from this var-decl, the CowBorrow propagation
                        // would dangle. cow_before_mutation IS triggered at
                        // the mutation site and emits a clone, but the
                        // resulting owned local's rebinding doesn't survive
                        // the enclosing loop's `save_locals`/`restore_locals`
                        // boundary — instructions emitted in the loop-exit
                        // block (or after the loop) still reference the
                        // original Ptr-typed local, which points into the
                        // now-freed buffer. Falling through to the eager-
                        // clone branch below produces an owned local at the
                        // var-decl site, safely outliving any subsequent
                        // mutation. Mirrors the self-host `loader.gg`
                        // `String mod_path = ... .clone()` defensive pattern.
                        let source_mut_unsafe = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                            if let Some(coll) = ctx.cow_borrow_source(p.local).cloned() {
                                let path: Option<String> = match coll {
                                    crate::ir::lowering::context::CollectionId::Local(loc) => {
                                        builder.local_name(loc).map(|s| s.to_string())
                                    }
                                    crate::ir::lowering::context::CollectionId::FieldPath(p) => Some(p),
                                };
                                path.map_or(false, |p| ctx.is_source_mut_unsafe_at(&p, stmt_span.start))
                            } else { false }
                        } else { false };
                        // FULL LAZY materialization (devbook/11 "Lazy
                        // loop-carried materialization"). The source IS
                        // mutated on a forward path (`source_mut_unsafe`), so
                        // the pre-lazy lowering EAGER-cloned at this bind (the
                        // clone callsite lands in the bind block before the
                        // loop header). Instead, emit the lazy loop-carried
                        // shape: keep `s` as a pre-loop String VALUE slot
                        // holding a shallow borrow + a pre-loop `s_mat=false`
                        // flag; defer the deep clone to a flag-guarded
                        // IN-PLACE materialize at the mutation site (clone
                        // once, from the still-valid borrow). Dead mutation
                        // path → 0 clones. Both slots are pre-loop locals
                        // (lid < the loop's save_locals boundary) so the
                        // materialize survives restore_locals AND becomes
                        // loop-carried (LIR-SSA phis them at the header). This
                        // is the centerpiece the a12333a0 attempt got wrong
                        // (it cloned every iteration from a stale ptr with no
                        // flag).
                        //
                        // Eligibility:
                        // - `borrow_view_fn_for` (typed metadata axis): the
                        //   pointee's runtime must support drop-safe cap=0
                        //   views. Phase 1: String only — collection frees are
                        //   not view-aware (`gorget_array_free` runs
                        //   `elem_drop` regardless of cap).
                        // - `CollectionId::Local` sources only: FieldPath
                        //   sources are EXCLUDED because
                        //   `cow_before_field_mutation` has no lazy routing
                        //   and `lower_field_assign` does not walk descendant
                        //   FieldPath refs on root-struct mutation (the
                        //   empty_literal_struct_field UAF shape). FieldPath
                        //   lazy = Phase 1b.
                        let lazy_collection = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                            ctx.cow_borrow_source(p.local)
                                .filter(|c| matches!(c, crate::ir::lowering::context::CollectionId::Local(_)))
                                .cloned()
                        } else { None };
                        if source_is_cow_borrow && source_mut_unsafe
                            && ctx.type_registry.is_resource_type(_inner)
                            && ctx.borrow_view_fn_for(_inner).is_some()
                            && lazy_collection.is_some()
                        {
                            let collection = lazy_collection.unwrap();
                            ctx.emit_lazy_loopcarried_borrow(
                                builder, name, local_id, _inner, inferred,
                                operand.clone(), collection, value.span,
                            );
                            lazy_handled = true;
                        } else if source_is_cow_borrow && safe_in_loop && !source_mut_unsafe
                            && ctx.type_registry.is_resource_type(_inner)
                        {
                            // Propagate CowBorrow as CollectionRef — typed binding
                            // behaves identically to `auto`. cow_before_mutation on the
                            // collection materializes this local before collection mutation.
                            //
                            // Only apply to resource pointees: deferring cloning is the
                            // entire point. For primitive / value-struct pointees, fall
                            // through to the deref branch — captures a snapshot value at
                            // the binding site, matches the user's `int x = …` intent,
                            // and avoids leaking Ref types into phi/SSA back-edges that
                            // were typed for the original value.
                            let collection = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                                ctx.cow_borrow_source(p.local).cloned().unwrap()
                            } else { unreachable!() };
                            builder.locals[local_id.0 as usize].type_id = inferred;
                            ctx.register_local(name, local_id, inferred);
                            ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                            ctx.set_collection_ref(builder, local_id, collection);
                            ctx.drops.unregister(local_id);
                        } else if let Some((base, field)) = source_field_origin
                            .filter(|_| !in_loop || safe_in_loop)
                            .filter(|_| ctx.type_registry.is_resource_type(_inner))
                            .filter(|_| false) // PROBE: field_origin off (cow_borrow restored)
                        {
                            // PROBE (Site #1 Field-borrow propagation): the
                            // source is a Ptr-typed field-load (e.g.
                            // `String x = obj.field`). Instead of eagerly
                            // cloning the field's data (the legacy branch
                            // below), propagate the Field origin onto the
                            // typed binding so cow_before_mutation severs
                            // when the parent struct is mutated. Mirrors
                            // the bare-param / cow-borrow propagations
                            // above. CoW severance walks NAMED Field
                            // borrows (commit 86d4fef7) handle the
                            // materialisation side.
                            builder.locals[local_id.0 as usize].type_id = inferred;
                            ctx.register_local(name, local_id, inferred);
                            ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                            ctx.set_field_borrow(builder, local_id, base, field);
                            ctx.drops.unregister(local_id);
                        } else if let Some(clone_fn) = ctx.clone_fn_for_ptr(_inner) {
                            // Owned Ptr source (function return, etc.) → auto-clone
                            let cloned = ctx.emit_clone(builder, &clone_fn, vec![operand.clone()], value.span, _inner, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);
                            // Tier 2a Phase 2A: clone temp owns a fresh
                            // heap allocation. Tag FreshOwned so the
                            // consume-site validator sees a sound state
                            // at the downstream consumer.
                            ctx.drops.register_local(cloned, _inner, &ctx.type_registry);
                            ctx.set_owned_fresh(builder, cloned);
                            operand = FunctionBuilder::copy(cloned);
                        } else if !ctx.type_registry.is_resource_type(_inner) {
                            // Non-resource pointee (primitives / value structs) — deref
                            // the pointer to load the pointee value into the T-typed slot.
                            if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                                if p.projections.is_empty() {
                                    let tmp = builder.add_local(_inner, None);
                                    builder.assign(
                                        Place::local(tmp),
                                        Operand::Copy(Place {
                                            local: p.local,
                                            projections: vec![Projection::Deref],
                                        }),
                                    );
                                    operand = FunctionBuilder::copy(tmp);
                                }
                            }
                        } else {
                            // Resource without clone fn — propagate as Ptr
                            builder.locals[local_id.0 as usize].type_id = inferred;
                            ctx.register_local(name, local_id, inferred);
                            ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                            ctx.set_ref(builder, local_id);
                        }
                    }
                }
            }
            // Also mark auto-reinferred locals that got Ptr type.
            // Propagate CollectionRef from the source so cow_before_mutation
            // and cow_before_field_mutation can find and materialize this local
            // when the underlying collection is mutated (e.g., c.items.push(x)
            // after auto elem = c.items[0]).
            if needs_reinfer {
                let actual = builder.local_type(local_id);
                if let Some(GirType::Ptr(_)) = ctx.type_registry.get(actual) {
                    if !matches!(ctx.type_registry.get(gir_type), Some(GirType::Ptr(_))) {
                        // Propagate CollectionRef from the source operand.
                        let propagated = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                            if p.projections.is_empty() {
                                if let Some(collection) = ctx.collection_ref_source(builder, p.local) {
                                    ctx.set_collection_ref(builder, local_id, collection);
                                    ctx.drops.unregister(local_id);
                                    true
                                } else { false }
                            } else { false }
                        } else { false };
                        if !propagated {
                            ctx.set_ref(builder, local_id);
                        }
                    }
                }
            }
            // Determine assignment mode and emit with explicit ownership semantics.
            use crate::ir::instructions::AssignMode;
            let actual_var_type = builder.local_type(local_id);

            // The lazy loop-carried CoW branch already emitted the bind
            // (borrow_view → `s`, flag init) and set `s`'s
            // ownership/drop-tracking. Skip the trailing assign +
            // ownership-propagation + move-zero, which would re-clobber `s`'s
            // value slot with the raw element struct (a double-free).
            if !lazy_handled {
            // Lazy loop-carried CoW, hook W3a (bind from a lazy-view SOURCE):
            // `String x = s` where `s` is a lazy view would otherwise capture
            // the PRE-materialize slot version (Branch C Ptr alias) or steal
            // the view bytes (Branches F/G) — provenance to the collection is
            // lost and the later mutation-site materialize cannot fix `x`.
            // Materialize `s` in place FIRST; the bind below then sees the
            // eager-world owned+live source states (Branch A / F). Defined by
            // SOURCE, not by lowering branch — a single site upstream of
            // Branches A-G and the trailing assign path. Rejected
            // alternative: propagating CollectionElement provenance to the
            // alias preserves more laziness but multiplies loop-placement and
            // alias-chain cases (Phase 1b if profiles justify).
            ctx.materialize_lazy_source_if_needed(builder, &operand, value.span);
            // Phase D4 typed signals — see TODO entry "Phase D4 —
            // lower_var_decl decision tree refactor" and
            // `docs/devbook/13-ownership-in-ir.md` (Phase D, §6.7). The decision tree below is
            // expressed as a typed match on (target_resource, source_live,
            // source_own). Three of the seven branches are fully typed
            // (E, F-extension, G); the remaining four (A, B, C, D) keep
            // their `is_named_local` guard documented as genuine gating
            // (probe history: 10 / 16 / 50+ / 50+ regressions on naive
            // removal, see TODO).
            let target_resource = ctx.type_registry.is_resource_type(actual_var_type);
            let source_live = ctx.source_live_past(&operand, stmt_span, builder);
            let source_own = ctx.source_ownership(&operand, builder);
            // Branch-C-SPECIFIC suppress flag (same mechanism as the
            // `lazy_handled` skip above): Branch C fully establishes the
            // bind itself — it retypes the dst to Ptr(rhs) and emits the
            // Borrow. The trailing assign below would then store the enum
            // VALUE into the Ptr slot; for Vector the LIR coerces that to
            // a benign slot_addr re-store, but for Result/Option the LIR
            // `try_enum_payload_extract` intercepted it and emitted a
            // payload extraction into the pointer slot — deref of the
            // payload as a pointer, SIGSEGV. Keyed on the BRANCH, never on
            // `AssignMode::Borrow` generally: Branch A (SharedHeap string
            // aliasing) also returns Borrow and its trailing assign IS
            // load-bearing.
            let mut branch_c_bound = false;
            let assign_mode = lower_var_decl_assign_mode(
                ctx,
                builder,
                local_id,
                name,
                value.span,
                stmt_span,
                actual_var_type,
                target_resource,
                source_live,
                source_own,
                &mut operand,
                &mut branch_c_bound,
            );


            if !branch_c_bound {
                builder.assign_mode(assign_mode, Place::local(local_id), operand.clone());
            }

            // Propagate ownership to the destination.
            //
            // Tier 2a Phase 2A (writer-site tagging): when the assign mode
            // is Move and the destination is droppable, the destination
            // becomes Owned regardless of how the source was tagged
            // upstream — Move-mode IS ownership transfer at the IR
            // semantic level. Without this, the destination's ownership
            // stays at the default `Untracked` whenever upstream lowering
            // produced a fresh resource via a raw `builder.call*` (which
            // doesn't tag) — leaving every downstream consume-site
            // validator with no signal.
            //
            // The decision tree in `lower_var_decl_assign_mode` only
            // selects Move for sources that are sound to consume; Branch
            // A/C/D's borrow / clone / shared-heap shapes pre-set the
            // destination's typed ownership to Borrowed{Alias},
            // SharedHeap, or similar BEFORE this block runs and we must
            // not clobber those. The skip below preserves their state.
            //
            // For Copy-mode of resource-typed assigns (Branch G safety
            // net + the rare resource-typed Copy that slipped past F),
            // still propagate from the source — preserves the legacy
            // "owned source flows to owned destination" shape.
            if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                let local_idx = local_id.0 as usize;
                let dst_already_typed = local_idx < builder.locals.len()
                    && matches!(
                        &builder.locals[local_idx].ownership,
                        crate::ir::LocalOwnership::SharedHeap { .. }
                            | crate::ir::LocalOwnership::Borrowed { .. }
                            | crate::ir::LocalOwnership::View { .. }
                    );
                if !dst_already_typed {
                    let move_assign = assign_mode == AssignMode::Move;
                    let target_needs_drop = ctx.type_registry.needs_drop(actual_var_type);
                    if move_assign && target_needs_drop {
                        // Move-mode + droppable destination: ownership
                        // transfer is the IR semantic. Tag destination as
                        // Owned so downstream consume-site validators see
                        // a sound (Owned, dead, _) tuple.
                        ctx.set_owned(builder, local_id);
                    } else if ctx.is_owned_local(builder, p.local) {
                        // Copy-mode of an owned source — preserve legacy
                        // propagation-from-call-result shape.
                        ctx.set_owned(builder, local_id);
                    }
                }
            }

            // Mark source as moved + emit GIR-level move-zero.
            // The LIR also reads AssignMode::Move for zeroing, but the GIR
            // move-zero is still needed for the drop elaborator's DropIfAlive.
            if assign_mode == AssignMode::Move {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty()
                        && place.local != local_id
                        && !ctx.drops.is_moved(place.local)
                    {
                        ctx.move_zero_and_mark(builder, place.local);
                    }
                }
            }
            } // end `if !lazy_handled`
        }

        Pattern::Tuple(parts) => {
            // Lower the RHS expression first — it should produce a tuple (struct) value
            let operand = lower_expr(ctx, builder, value);
            let tuple_type = infer_operand_type_with_builder(ctx, &operand, builder);

            // Store the tuple in a temp local. Phase C: pick mode by source —
            // call results (owned + dead) Move; named-local sources Borrow;
            // primitives Copy.
            let tuple_local = builder.add_local(tuple_type, None);
            let tuple_assign_mode = {
                use crate::ir::instructions::AssignMode;
                if !ctx.type_registry.is_resource_type(tuple_type) {
                    AssignMode::Copy
                } else if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    if p.projections.is_empty() && ctx.is_owned_local(builder, p.local) {
                        AssignMode::Move
                    } else {
                        AssignMode::Copy
                    }
                } else {
                    AssignMode::Copy
                }
            };
            builder.assign_mode(tuple_assign_mode, Place::local(tuple_local), operand.clone());

            // Move-mode follow-through. Mirror the Pattern::Binding path
            // (lines 675-684 above): a Move-mode assign of a drop-registered
            // source must MoveZero the source so its scope-exit drop doesn't
            // re-free the buffers now owned by `tuple_local`. With the Tier
            // 1c tuple migration (`map_ast_type_mut::Type::Tuple` /
            // `register_tuple_type`), tuples holding resource-typed fields
            // are now `(Recursive, Resource)`, so the destructure source
            // needs the same follow-through every other Move-mode assign
            // site does.
            if matches!(tuple_assign_mode, crate::ir::instructions::AssignMode::Move) {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty()
                        && place.local != tuple_local
                        && !ctx.drops.is_moved(place.local)
                    {
                        ctx.move_zero_and_mark(builder, place.local);
                    }
                }
            }

            // Extract each field and bind it to the corresponding pattern variable.
            //
            // Phase C FieldLoad migration (2026-05-06): when the tuple was
            // moved into the temp (Move mode), the temp is dead at this site
            // and resource-typed fields must be moved out — emit `field_load
            // + move_zero` so the temp's drop doesn't free buffers the new
            // bindings now own. The validator's next-inst peek encodes this
            // as ReadMode::Move (sound).
            //
            // For Copy-mode tuples (named-local source / primitive tuple)
            // resource fields stay shallow-copy because the source is still
            // alive; that's the same shape the lower_field_access path
            // handles. Today no fixture exercises tuple-destructure of a
            // borrowed tuple with resource fields — if one appears, the
            // validator will flag it for migration.
            let move_resource_fields = matches!(
                tuple_assign_mode,
                crate::ir::instructions::AssignMode::Move
            );
            for (i, part) in parts.iter().enumerate() {
                let field_type = super::exprs::resolve_tuple_field_type(ctx, tuple_type, i);
                let field_local = builder.field_load(Place::local(tuple_local), i as u32, field_type);
                if move_resource_fields && ctx.type_registry.is_resource_type(field_type) {
                    builder.move_zero(Place {
                        local: tuple_local,
                        projections: vec![Projection::Field(i as u32)],
                    });
                }

                if let Pattern::Binding(name) = &part.node {
                    ctx.register_local(name, field_local, field_type);
                    ctx.drops.register_local(field_local, field_type, &ctx.type_registry);
                } else {
                    // Nested destructuring — recurse via emit_pattern_bindings
                    emit_pattern_bindings(ctx, builder, part, field_local, field_type);
                }
            }
        }

        _ => {
            // Other pattern forms not yet supported in VarDecl
        }
    }
}

/// Phase D4: typed-shape decision tree for the VarDecl assign mode.
///
/// Implements the §6.7 contract from `docs/devbook/13-ownership-in-ir.md` (Phase D).
/// Reads three signals from the surrounding lowering state:
/// - `target_resource`: does the destination type own heap data?
/// - `source_live`: is the source's underlying local live AFTER `stmt_span`?
/// - `source_own`: the source local's typed `LocalOwnership`, if any.
///
/// All branches read typed predicates — `is_named_local`
/// fully retired from this function (D 2026-05-10, F 2026-05-10):
/// - **A** (REMOVED, round-30 Fix C) — the old Owned + live
///   GorgetString same-type arm (`String b = a` → `Borrow` +
///   `set_shared_heap`) leaked the source buffer; owned-String
///   same-type sources now route through the principled Branch C
///   CoW Ptr-alias borrow, which subsumes it.
/// - **B** (Owned + live non-resource source with cross-type
///   `clone_fn`, e.g. Str → GorgetString) → emit clone, `Move`.
/// - **C** (live resource source, CoW-safe; transitive aliases
///   permitted) → CoW alias via `Borrow` + Ptr retype + `set_ref`.
/// - **D** (live resource source, CoW-unsafe + non-Borrowed) →
///   emit clone, `Move`. Typed via `source_live && !Borrowed`.
/// - **E** (View source, GorgetString same-type) → emit
///   clone-to-owned, `Move`. Typed via `LocalOwnership::View`.
/// - **F** (dead droppable source OR Owned + dead with droppable
///   target) → `Move`. Fully typed via `!source_live` + ownership
///   probes; legacy `drops.is_registered` and `is_named_local`
///   proxies both retired.
/// - **G** (safety net: target_resource fell through to Copy) →
///   `Move`.
fn lower_var_decl_assign_mode(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    local_id: LocalId,
    name: &str,
    value_span: crate::span::Span,
    stmt_span: crate::span::Span,
    actual_var_type: TypeId,
    target_resource: bool,
    source_live: bool,
    source_own: Option<crate::ir::LocalOwnership>,
    operand: &mut Operand,
    branch_c_bound: &mut bool,
) -> AssignMode {
    use crate::ir::LocalOwnership;
    let mut assign_mode = AssignMode::Copy;

    // Snapshot the source place / type once; arms read from these.
    let (source_place, rhs_type) = match operand {
        Operand::Copy(p) | Operand::Move(p)
            if p.projections.is_empty() && p.local != local_id =>
        {
            (p.clone(), builder.local_type(p.local))
        }
        _ => return assign_mode, // not a place operand → stays Copy
    };

    // Helper: emit `T__clone(&place)` and rewrite `*operand` to the
    // cloned local. Used by branches B, D, E (all "Move with clone").
    // Returns AssignMode::Move on success, AssignMode::Copy if no
    // clone fn is registered for `clone_src_type`.
    let emit_clone_to_owned =
        |ctx: &mut LoweringContext,
         builder: &mut FunctionBuilder,
         operand: &mut Operand,
         clone_src_type: TypeId,
         clone_ret_type: TypeId|
         -> AssignMode {
            let Some(clone_fn) = ctx.clone_fn_for_ptr(clone_src_type) else {
                return AssignMode::Copy;
            };
            ctx.warn_clone_and_hit(
                builder,
                value_span,
                clone_src_type,
                crate::ir::ImplicitCloneReason::VarDeclFromBorrow,
            );
            let ptr_type = ctx.register_ptr_type(clone_src_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow(ptr_local, source_place.clone());
            let cloned = builder.call_clone(
                &clone_fn,
                vec![FunctionBuilder::copy(ptr_local)],
                clone_ret_type,
                crate::ir::ImplicitCloneReason::VarDeclFromBorrow,
            );
            ctx.set_owned(builder, cloned); // clone result owns its data
            *operand = FunctionBuilder::copy(cloned);
            AssignMode::Move
        };

    let owned_string = ctx.type_mapper.owned_string_type;
    let same_type_string =
        rhs_type == owned_string && actual_var_type == owned_string;

    // Branch P — the `&`-param deref temp. `Vector[int] local = v` over a
    // `Vector[int] &v` lowers its RHS to an unnamed temp holding a SHALLOW
    // copy of `*v` (`exprs/mod.rs`, the auto-deref site), which now carries
    // `Borrowed { Param(v) }`. A typed binding defaults to BORROW
    // (docs/devbook/11 — "`var_decl` is deliberately not in either list …
    // clones only on later mutation"), so bind the destination as a CoW Ptr
    // alias of the PARAM rather than of the dying temp.
    //
    // Aliasing the param (not the temp) is what makes this safe: the temp is
    // an unnamed stack slot that dies at end-of-statement — the documented
    // "50+ fixtures SIGSEGV" class Branch C's own comment records — whereas
    // `v` outlives the frame. And the emitted instruction is a pointer COPY,
    // not an `emit_borrow`: `v` ALREADY holds the caller's pointer, so
    // `borrow_mut v` would produce `Ptr(MutPtr(T))` and `cow_materialize_alias`
    // would then clone the POINTER instead of the buffer.
    //
    // Without the Ptr retype the `Alias` tag is inert: a later `local.push(…)`
    // reaches `cow_before_mutation` Case 1, which `unset_ownership`s the alias
    // and then early-returns from `cow_materialize_alias` because `pointee_type`
    // is `None` on a non-Ptr local — destroying the tag and emitting NO clone,
    // silently writing through into the caller's buffer.
    let deref_of_borrowed_param: Option<LocalId> = match &source_own {
        Some(LocalOwnership::Borrowed {
            origin: crate::ir::BorrowOrigin::Param(p),
            ..
        }) if *p != source_place.local => Some(*p),
        _ => None,
    };
    if let Some(param) = deref_of_borrowed_param {
        if ctx.type_registry.is_resource_type(rhs_type)
            && actual_var_type == rhs_type
            && !ctx.is_cow_unsafe_at(name, stmt_span.start)
        {
            let ptr_type = ctx.register_ptr_type(rhs_type);
            builder.locals[local_id.0 as usize].type_id = ptr_type;
            // Pointer COPY of the param slot — see the comment above.
            builder.assign_mode(
                AssignMode::Borrow,
                Place::local(local_id),
                FunctionBuilder::copy(param),
            );
            // The dst tag must be `Alias(param)`, NOT `Borrowed { Param(param) }`:
            // `cow_before_mutation` dispatches on five shapes and a non-self
            // `Param(p)` matches NONE of them, so a later `local.push(…)` would
            // find nothing to materialize and write through the Ptr into the
            // caller's buffer — trading a double-free for a silent aliasing
            // miscompile. `Alias(p)` is what Branch C uses and what makes
            // `cow_materialize_alias` fire.
            ctx.cow_register_alias(builder, local_id, param);
            // A Ptr doesn't own data — don't register it for drop.
            ctx.drops.unregister(local_id);
            if let Some(hint) = builder.local_name(local_id).map(|s| s.to_string()) {
                ctx.register_local(&hint, local_id, ptr_type);
            }
            // Borrow (not Copy) so the Branch-G safety net stays inert: G flips
            // `Copy && target_resource` to Move, which here would be a Move of
            // the temp into a Ptr-typed slot.
            // `branch_c_bound` suppresses the caller's trailing value-into-Ptr
            // store, which the LIR enum payload-extract path mis-classifies.
            *branch_c_bound = true;
            return AssignMode::Borrow;
        }
    }

    // Branch A (REMOVED, round-30 Fix C) — the old Owned + live
    // GorgetString same-type arm (value-aliasing `String b = a`)
    // emitted `set_shared_heap` + `Borrow` + `drops.unregister(source)`.
    // That shape was internally inconsistent: the SharedHeap tag made
    // the destination alias the source's buffer AND unregistered the
    // source's drop as if the two locals shared one buffer, but the
    // backend's `gorget_string_copy_cow` DEEP-copied a cap>0 owned
    // source into a SECOND independent buffer — so the source's buffer
    // leaked (`String v = sb`, both live to scope exit, leaked sb's
    // heap allocation under ASan). The `set_shared_heap` was an
    // optimization crutch for return-clone-elision (commit 3e4379ea),
    // NOT a correctness primitive. An owned-String same-type source now
    // falls through to Branch C, the principled CoW Ptr-alias borrow
    // (zero-cost, severed on mutation via `cow_before_mutation`), which
    // fully subsumes it and also fixes the pre-existing return-alias,
    // struct-field-escape, and View-source memory bugs the SharedHeap
    // model mishandled. Verified ASan-clean, both backends, no
    // bootstrap/hot-path regression.
    //
    // Branch B — Owned + live source, non-resource type, cross-type
    // clone_fn (e.g. Str → GorgetString). Migrated 2026-05-06: the
    // legacy `is_named_local` proxy was replaced with the typed
    // `source_live && source_own.is_owned()` predicate. Probe history
    // (2026-05-04) regressed 16 fixtures because unnamed
    // Result/Option temps with recursive drop have
    // `clone_fn_for_ptr.is_some()` true and were wrongly routed
    // here; the typed predicate excludes them (`source_live = false`)
    // and they correctly fall through to F's Move path.
    if source_live
        && source_own.as_ref().map_or(false, |s| s.is_owned())
        && !ctx.type_registry.is_resource_type(rhs_type)
        && ctx.clone_fn_for_ptr(rhs_type).is_some()
    {
        // Use TARGET type for clone return (cross-type case).
        assign_mode = emit_clone_to_owned(
            ctx, builder, operand, rhs_type, actual_var_type,
        );
    }
    // Branch C — live resource source, CoW-safe. Create a Ptr
    // alias instead of cloning. Migrated 2026-05-06: legacy
    // `is_named_local` proxy replaced with `source_live`. Probe
    // history (2026-05-04) regressed 50+ fixtures with naive
    // `is_named_local` removal — root cause: unnamed temps die at
    // end-of-stmt → dangling Ptr alias → SIGSEGV. The typed
    // predicate excludes them (`source_live = false` for unnamed
    // temps). Note: source need NOT be Owned — transitive alias
    // chains (`String b = a` where `a` is itself a Borrowed CoW
    // alias) must also propagate through this branch.
    else if source_live
        && ctx.type_registry.is_resource_type(rhs_type)
        && !ctx.is_cow_unsafe_at(name, stmt_span.start)
        && !builder
            .local_name(source_place.local)
            .map_or(false, |n| ctx.is_cow_unsafe_at(n, stmt_span.start))
    {
        let ptr_type = ctx.register_ptr_type(rhs_type);
        builder.locals[local_id.0 as usize].type_id = ptr_type;
        ctx.set_ref(builder, local_id);
        builder.emit_borrow(local_id, source_place.clone());
        ctx.cow_register_alias(builder, local_id, source_place.local);
        // Ptr doesn't own data — don't register for drop.
        ctx.drops.unregister(local_id);
        // Update local type in context lookup
        if let Some(hint) = builder.local_name(local_id).map(|s| s.to_string()) {
            ctx.register_local(&hint, local_id, ptr_type);
        }
        assign_mode = AssignMode::Borrow;
        // The bind is fully established here (Ptr retype + emit_borrow).
        // Tell the caller to suppress its trailing assign — a value-into-
        // Ptr-slot store that the LIR enum payload-extract path
        // mis-classified for Result/Option sources (SIGSEGV). Branch-C-
        // specific BY DESIGN: Branch A also returns Borrow and its
        // trailing assign is load-bearing.
        *branch_c_bound = true;
    }
    // Branch D — live resource source, CoW-unsafe → clone fallback.
    // Migrated 2026-05-10 (D-PROBE-OPT-B+RETIRE): legacy `is_named_local`
    // proxy replaced with the typed `source_live && !Borrowed`
    // predicate. The 2026-05-06 D-probe substituted `source_live` alone
    // and regressed self-host bootstrap (`is_cstr_returning_call` →
    // "local _19 read after MoveZero in bb5"): when D's `clone_fn_for_ptr`
    // lookup failed, assign_mode stayed Copy and the safety-net G's
    // Move zeroed a Borrowed transitive alias's heap data. The added
    // `!Borrowed` bail (option (b) per docs/devbook/13-ownership-in-ir.md
    // Phase D §6.7) routes Borrowed sources to E/F/G with correct Borrowed-aware
    // behavior instead of triggering the clone-failure → G-Move chain.
    // Owned-named-at-last-use sources correctly fall through to F's Move
    // path now (instead of the redundant clone D was emitting under the
    // legacy predicate). Phase D4 — last `is_named_local` site retired.
    else if source_live
        && !matches!(source_own, Some(LocalOwnership::Borrowed { .. }))
        && ctx.type_registry.is_resource_type(rhs_type)
    {
        // Same-type clone (rhs_type → rhs_type).
        assign_mode = emit_clone_to_owned(
            ctx, builder, operand, rhs_type, rhs_type,
        );
    }
    // Branch E — `View` source, GorgetString same-type → clone-to-owned.
    // Fully typed via LocalOwnership::View.
    else if matches!(source_own, Some(LocalOwnership::View { .. }))
        && same_type_string
    {
        assign_mode = emit_clone_to_owned(
            ctx, builder, operand, rhs_type, rhs_type,
        );
    }
    // Branch F — dead source + droppable type → Move. Two typed
    // clauses (probed 2026-05-06 + 2026-05-10, retired both legacy
    // proxies — `drops.is_registered` and `is_named_local`):
    // - **rhs-droppable**: `!source_live && needs_drop(rhs_type)`
    //   covers unnamed temps (which always die at end-of-stmt) AND
    //   named locals at last use whose own type needs dropping. The
    //   former was the original `!source_is_named` shape; the latter
    //   widening is safe because Borrowed-source rhs_type is Ptr
    //   (not droppable) and Owned-named-at-last-use is what the
    //   second clause already moved.
    // - **target-droppable**: `source_own.is_owned() && !source_live
    //   && needs_drop(actual_var_type)` covers Option/Result wrapper
    //   targets where `is_resource_type(rhs_type)` returns false but
    //   the variant payload still requires ownership transfer.
    else if (!source_live && ctx.type_registry.needs_drop(rhs_type))
        || (source_own.as_ref().map_or(false, |s| s.is_owned())
            && !source_live
            && ctx.type_registry.needs_drop(actual_var_type))
    {
        assign_mode = AssignMode::Move;
    }

    // Branch G (safety net) — if still Copy and the TARGET is a
    // resource type, switch to Move. Catches edge cases not covered
    // by A–F (e.g. clone_fn lookup failed in D). Target-keyed: Move
    // applies to the destination's type, not the source's.
    if assign_mode == AssignMode::Copy && target_resource {
        assign_mode = AssignMode::Move;
    }

    assign_mode
}

/// Lower a shared VarDecl with transparent access.
///
/// Creates a hidden Mutex local and registers the user-visible name with the
/// inner type T. Reads/writes of the variable are transparently rewritten to
/// lock+get/set through the mutex local by `lower_expr` and `lower_assign`.
fn lower_shared_var_decl(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_: &Spanned<ast::Type>,
    pattern: &Spanned<Pattern>,
    value: &Spanned<Expr>,
    shared: &ast::SharedKind,
) {
    use crate::ir::types::GirType;
    use super::exprs::{ensure_mutex_type_def, ensure_shared_type_def};
    use crate::semantic::SharedStrategy;

    // Phase C: pick AssignMode for resource-typed assigns within
    // lower_shared_var_decl. Sources here are either fresh call results
    // (wrapped/shared_val/init_val) or user-provided expression results
    // (val_operand). For resource types, Move transfers ownership;
    // primitives stay Copy (bit-copy is correct).
    let resource_assign_mode = |ctx: &LoweringContext, ty: TypeId| {
        use crate::ir::instructions::AssignMode;
        if ctx.type_registry.is_resource_type(ty)
            || ctx.type_registry.needs_drop(ty)
        {
            AssignMode::Move
        } else {
            AssignMode::Copy
        }
    };

    let name = match &pattern.node {
        Pattern::Binding(n) => n,
        _ => {
            lower_var_decl(ctx, builder, type_, pattern, value, pattern.span);
            return;
        }
    };

    // Look up CFA strategy via DefId
    let strategy = ctx.analysis.resolution_map
        .get(&pattern.span.start)
        .and_then(|&def_id| ctx.analysis.shared_bindings.get(&def_id))
        .copied()
        .unwrap_or(SharedStrategy::ArcMutex); // default to Mutex for safety

    // Resolve inner type and lower init value
    let inner_type = ctx.resolve_var_type(type_, value);
    let inner_c = ctx.c_type_name_for_id(inner_type);

    let prev_expected = ctx.func_state.expected_type;
    ctx.func_state.expected_type = Some(inner_type);
    let val_operand = lower_expr(ctx, builder, value);
    ctx.func_state.expected_type = prev_expected;

    match strategy {
        SharedStrategy::ArcAtomic => {
            // Atomic: use AtomicInt or AtomicBool — lock-free ops
            let atomic_name = super::exprs::atomic_type_name_for(inner_type);
            let wrapper_type = if let Some(tid) = ctx.type_mapper.lookup_named(&atomic_name) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(atomic_name.clone()));
                ctx.type_mapper.register_named(atomic_name.clone(), tid);
                // AtomicInt/AtomicBool are built-in runtime types — no TypeDef needed,
                // but register a trivial drop for RAII cleanup
                use crate::ir::types::{TypeDef, TypeDefKind, StructDef, StructField, TypeMetadata, CopySemantics, DropStrategy};
                let drop_fn = format!("gorget_atomic_{}_free", if inner_type == BOOL_TYPE { "bool" } else { "int" });
                let type_def = TypeDef {
                    name: atomic_name.clone(),
                    kind: TypeDefKind::Struct(StructDef {
                        fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
                    }),
                    metadata: TypeMetadata {
                        size: None,
                        align: None,
                        copy_semantics: CopySemantics::Trivial, // pointer type, cheap to copy
                        drop_strategy: DropStrategy::Trivial(drop_fn),
                        ..Default::default()
                    },
                };
                ctx.type_registry.add_type_def(type_def);
                tid
            };

            let new_fn = format!("{atomic_name}__new");
            let wrapped = builder.call(&new_fn, vec![val_operand], wrapper_type);

            let hidden_local = builder.add_local(wrapper_type, None);
            ctx.drops.register_local(hidden_local, wrapper_type, &ctx.type_registry);
            builder.assign_mode(crate::ir::instructions::AssignMode::Move, Place::local(hidden_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type, kind: SharedLocalKind::Atomic, ast_shared: *shared });

            // Initialize facade with atomic load
            let init_val = super::exprs::emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name);
            let mode_fi = resource_assign_mode(ctx, inner_type);
            ctx.assign_with_move_follow_through(builder, facade_local, init_val, mode_fi);
        }

        SharedStrategy::ArcOnly => {
            // ArcOnly: use Shared[T] — no locking needed
            let mangled = format!("Shared__{inner_c}");
            let shared_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                ctx.type_mapper.register_named(mangled.clone(), tid);
                ensure_shared_type_def(ctx, &mangled, inner_type);
                tid
            };

            let new_fn = format!("{mangled}__new");
            let mode = resource_assign_mode(ctx, inner_type);
            let tmp = ctx.materialize_addressable(builder, val_operand, inner_type, mode);
            let wrapped = builder.call(&new_fn, vec![FunctionBuilder::copy(tmp)], shared_type);

            let hidden_local = builder.add_local(shared_type, None);
            ctx.drops.register_local(hidden_local, shared_type, &ctx.type_registry);
            builder.assign_mode(crate::ir::instructions::AssignMode::Move, Place::local(hidden_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type: shared_type, kind: SharedLocalKind::SharedArc, ast_shared: *shared });

            let init_val = super::exprs::emit_shared_get(ctx, builder, hidden_local, inner_type);
            let mode_fi = resource_assign_mode(ctx, inner_type);
            ctx.assign_with_move_follow_through(builder, facade_local, init_val, mode_fi);
        }

        SharedStrategy::ArcRwLock => {
            // ArcRwLock: use RWLock[T] — reader-writer lock (concurrent reads, exclusive writes)
            let mangled = format!("RWLock__{inner_c}");
            let rwlock_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                ctx.type_mapper.register_named(mangled.clone(), tid);
                super::exprs::ensure_rwlock_type_def(ctx, &mangled, inner_type);
                tid
            };

            // Ensure ReadGuard and WriteGuard types exist WITH a TypeDef. The
            // facade read/write (`emit_rwlock_read_get`/`emit_rwlock_write_set`)
            // mints a `ReadGuard__T`/`WriteGuard__T` value slot and calls the
            // `gorget_rwlock_read_to`/`gorget_rwlock_write_to`, each writing a
            // 16-byte `gorget_read_guard_t`/`gorget_write_guard_t` into the slot
            // (via `*out = gorget_rwlock_read(rw)`).
            // Registering only a
            // bare `GirType::Named` (no TypeDef) left the name out of
            // `module.structs`, so the C backend emitted NO `typedef
            // gorget_read_guard_t ReadGuard__T;` and the slot fell back to
            // `void*` (8 bytes) → the 16-byte runtime write stack-buffer-
            // overflows (silent UB without ASan; fatal once the RWLock __drop
            // perturbs the stack — Core #8 Inc-B). Mint the full TypeDef here
            // (same shape as the `monomorphize_struct` ReadGuard/WriteGuard arm
            // and `ensure_guard_type_def`) so BOTH guard typedefs are emitted
            // whenever `shared(rwlock)` is used, regardless of whether user code
            // names a `ReadGuard[T]`/`WriteGuard[T]` local. The typedef BODY is
            // still driven by the typed resources table (`emit_wrapper_typedef`),
            // not a name match — this only ensures the name reaches `module.structs`.
            let read_guard_mangled = format!("ReadGuard__{inner_c}");
            let _ = super::exprs::get_or_register_type(ctx, &read_guard_mangled, Some(&|c| super::exprs::ensure_rwlock_guard_type_def(c, &read_guard_mangled, inner_type)));
            let write_guard_mangled = format!("WriteGuard__{inner_c}");
            let _ = super::exprs::get_or_register_type(ctx, &write_guard_mangled, Some(&|c| super::exprs::ensure_rwlock_guard_type_def(c, &write_guard_mangled, inner_type)));

            let new_fn = format!("{mangled}__new");
            let mode = resource_assign_mode(ctx, inner_type);
            let tmp = ctx.materialize_addressable(builder, val_operand, inner_type, mode);
            let wrapped = builder.call(&new_fn, vec![FunctionBuilder::copy(tmp)], rwlock_type);

            let rwlock_local = builder.add_local(rwlock_type, None);
            ctx.drops.register_local(rwlock_local, rwlock_type, &ctx.type_registry);
            builder.assign_mode(crate::ir::instructions::AssignMode::Move, Place::local(rwlock_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local: rwlock_local, inner_type, wrapper_type: rwlock_type, kind: SharedLocalKind::RwLock, ast_shared: *shared });

            let init_val = super::exprs::emit_rwlock_read_get(ctx, builder, rwlock_local, inner_type);
            let mode_fi = resource_assign_mode(ctx, inner_type);
            ctx.assign_with_move_follow_through(builder, facade_local, init_val, mode_fi);
        }

        SharedStrategy::ArcMutex => {
            // ArcMutex: use Shared[Mutex[T]] — ARC for lifetime, Mutex for sync
            let mutex_mangled = format!("Mutex__{inner_c}");
            let mutex_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mutex_mangled) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(mutex_mangled.clone()));
                ctx.type_mapper.register_named(mutex_mangled.clone(), tid);
                ensure_mutex_type_def(ctx, &mutex_mangled, inner_type);
                tid
            };

            let guard_mangled = format!("Guard__{inner_c}");
            if ctx.type_mapper.lookup_named(&guard_mangled).is_none() {
                let guard_tid = ctx.type_registry.insert(GirType::Named(guard_mangled.clone()));
                ctx.type_mapper.register_named(guard_mangled, guard_tid);
            }

            // Wrap Mutex in Shared for ARC lifetime control
            let shared_mutex_mangled = format!("Shared__{mutex_mangled}");
            let shared_mutex_type = if let Some(tid) = ctx.type_mapper.lookup_named(&shared_mutex_mangled) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(shared_mutex_mangled.clone()));
                ctx.type_mapper.register_named(shared_mutex_mangled.clone(), tid);
                ensure_shared_type_def(ctx, &shared_mutex_mangled, mutex_type);
                tid
            };

            // Create Mutex, then wrap in Shared
            let mutex_new_fn = format!("{mutex_mangled}__new");
            let mode = resource_assign_mode(ctx, inner_type);
            let tmp = ctx.materialize_addressable(builder, val_operand, inner_type, mode);
            let mutex_val = builder.call(&mutex_new_fn, vec![FunctionBuilder::copy(tmp)], mutex_type);

            let shared_new_fn = format!("{shared_mutex_mangled}__new");
            let shared_val = builder.call(&shared_new_fn, vec![FunctionBuilder::copy(mutex_val)], shared_mutex_type);

            let hidden_local = builder.add_local(shared_mutex_type, None);
            ctx.drops.register_local(hidden_local, shared_mutex_type, &ctx.type_registry);
            builder.assign_mode(crate::ir::instructions::AssignMode::Move, Place::local(hidden_local), FunctionBuilder::copy(shared_val));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type: shared_mutex_type, kind: SharedLocalKind::Mutex, ast_shared: *shared });

            // Init facade: Shared.get() → Mutex, then lock → get → release
            let init_val = super::exprs::emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type);
            let mode_fi = resource_assign_mode(ctx, inner_type);
            ctx.assign_with_move_follow_through(builder, facade_local, init_val, mode_fi);
        }
    }
}
/// Lower a return statement.
fn lower_return(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: Option<&Spanned<Expr>>,
) {
    if let Some(expr) = expr {
        // A `throws` fn whose declared success type `T` is *itself* a
        // `Result`/`Option`: the synthesized return slot is `Result[T, E]`
        // (double-Result). The user's `return Ok(...)` / `return <value>` then
        // produces a value of the inner type `T`, which still needs ONE outer
        // Ok-wrap into the slot. The default path below treats `Ok(...)` as the
        // slot-typed variant (the `is_explicit_result_variant` shortcut) and
        // sets `expected_type` to the slot — both correct only when `T` is a
        // non-Result (then slot == Result[T, E] and `Ok(value)` *is* the slot).
        // When `T` is a Result we instead lower the value against `T` and force
        // it through the Ok-wrap branch. (Was a silent miscompile: the inner
        // value's bytes were written straight into the outer Ok slot, dropping
        // the middle Result layer.)
        let ret_type = builder.locals[0].type_id;
        let throws_declared_success_type = ctx.func_state.current_throws_result_type
            .map(|slot| super::exprs::result_ok_payload_type(ctx, slot));
        // True when the declared success type `T` is itself an enum-wrapper
        // (`Result[..]` OR `Option[..]`). In both cases the synthesized return
        // slot is `Result[T, E]` (one extra outer layer), so an explicit
        // `Ok`/`Error`/`Some`/`None` from the user builds the *inner* `T` and
        // still needs ONE outer Ok-wrap. (Option must be included too: without
        // it, a `Some(...)`/`None` tail kept the shortcut on and the 16-byte
        // inner `Option` was direct-assigned into the larger outer `Result`
        // slot — a stack-buffer overflow + silently dropped value.)
        let declared_t_is_enum_wrapper = throws_declared_success_type
            .map(|t| matches!(ctx.type_registry.enum_category(t),
                Some(EnumCategory::Result) | Some(EnumCategory::Option)))
            .unwrap_or(false);

        // Check if the return expression is already an explicit Ok/Error variant
        // (used in throws functions). If so, skip the automatic Result wrapping —
        // the expression itself already produces a Result.
        //
        // When the declared `T` is itself a Result/Option, an explicit
        // `Ok(...)` / `Some(...)` builds the *inner* `T` and still needs the
        // outer wrap, so force the shortcut off and route through the Ok-wrap
        // branch below.
        let mut is_explicit_result_variant = !declared_t_is_enum_wrapper && matches!(&expr.node,
            Expr::Call { callee, .. } if matches!(&callee.node,
                Expr::Identifier(name) if name == "Ok" || name == "Error" || name == "Some" || name == "None"
            )
        );
        // Set expected type so variant constructors / auto-prop resolve against
        // the user-level type. For a `throws` fn with a Result/Option `T` that is
        // the inner `T` (so `Ok(...)` / `Some(...)` builds `T`, not the slot);
        // otherwise the slot.
        let prev_expected = ctx.func_state.expected_type;
        let expected_for_value = if declared_t_is_enum_wrapper {
            throws_declared_success_type.unwrap_or(ret_type)
        } else {
            ret_type
        };
        ctx.func_state.expected_type = Some(expected_for_value);

        // `return v` where `v` is a `!`-sigil resource parameter: the body is
        // transferring its owned pointee onward through the return value.
        // Track the source local so we can MoveZero it after the return-slot
        // assignment — without this, the function-exit `DropIfAlive { *v }`
        // would fire and free the data that the return value still aliases
        // (the standard Identifier Move-Deref-into-temp path doesn't zero
        // the source slot, only the temp). The MoveZero flips the LIR
        // drop flag to false, suppressing the exit drop.
        let owning_param_returned: Option<LocalId> = if let Expr::Identifier(name) = &expr.node {
            ctx.lookup_local(name).and_then(|(local_id, _)| {
                let idx = local_id.0 as usize;
                if idx < builder.locals.len() && builder.locals[idx].is_owning_param {
                    Some(local_id)
                } else {
                    None
                }
            })
        } else {
            None
        };

        let operand = lower_expr(ctx, builder, expr);
        // `return MODULE_GLOBAL_STRING`: clone the global so the caller
        // gets an independent allocation. See `clone_resource_global_ref`.
        let operand = clone_resource_global_ref(ctx, builder, operand, expr.span);
        // Snag #36: `return throws_fn(...)` from a throws function. The
        // operand here is already a `Result[T, E]` (the call's typed
        // return per Snag #35) matching the function's own return type.
        // Treat it as if the user had written an explicit Ok/Error
        // variant — direct-assign into the return slot. Without this
        // the Ok-wrap path below would re-wrap the Result in another
        // `Ok(...)`, producing `Result[Result[T, E], E]` bytes in the
        // `Result[T, E]` return slot at the C layer.
        if !is_explicit_result_variant {
            if let Some(throws_result_ty) = ctx.func_state.current_throws_result_type {
                let op_type = super::exprs::infer_operand_type_full(ctx, &operand, builder);
                if op_type == throws_result_ty {
                    is_explicit_result_variant = true;
                }
            }
        }
        // Auto-propagate: if returning a Result value from a throws function,
        // unwrap so the Ok-wrapping below works on the inner value.
        // NOTE: must run before restoring expected_type so the guard sees ret_type.
        let mut operand = if !is_explicit_result_variant {
            maybe_auto_propagate(ctx, builder, operand, expr.span)
        } else {
            operand
        };
        ctx.func_state.expected_type = prev_expected;
        // Identify the local being returned (to exclude from drops — it's being moved out)
        let mut returned_local = match &operand {
            Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                Some(place.local)
            }
            _ => None,
        };
        if let Some(result_type) = ctx.func_state.current_throws_result_type {
            if is_explicit_result_variant {
                // Expression already produced a Result — assign directly.
                //
                // Tier 1c: when the operand is a freshly-built Result
                // (place operand on a bare local), use Move semantics so
                // the source doesn't shallow-alias the return slot now
                // that Option/Result are Resource. The variant
                // constructor (`Ok(...)` / `Error(...)`) builds an Owned
                // Result temp that's dead immediately after the assign.
                let src_local = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    if p.projections.is_empty() { Some(p.local) } else { None }
                } else { None };
                if src_local.is_some() && ctx.type_registry.is_resource_type(result_type) {
                    builder.assign_mode(
                        crate::ir::instructions::AssignMode::Move,
                        Place::local(LocalId(0)),
                        operand,
                    );
                    if let Some(local) = src_local {
                        if !ctx.drops.is_moved(local) {
                            ctx.move_zero_and_mark(builder, local);
                        }
                    }
                } else {
                    builder.assign(Place::local(LocalId(0)), operand);
                }
            } else {
                // Ensure the return value is owned before wrapping in Result.Ok.
                // Bare parameters are Ptr(T) — storing the raw pointer into the
                // Result creates a dangling alias. Resolve through Ptr and clone.
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty() {
                        let src_type = builder.local_type(place.local);
                        // Ptr(T) → clone the inner T.
                        // Cannot move through Ptr here — the callee doesn't know
                        // if the caller still needs the argument after the call.
                        if let Some(crate::ir::types::GirType::Ptr(inner)) = ctx.type_registry.get(src_type).cloned() {
                            if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                                let cloned = ctx.emit_clone(
                                    builder,
                                    &clone_fn,
                                    vec![operand.clone()],
                                    expr.span,
                                    inner,
                                    crate::ir::ImplicitCloneReason::ReturnFromBorrow,
                                );
                                operand = FunctionBuilder::copy(cloned);
                                returned_local = Some(cloned);
                            }
                        } else if ctx.type_registry.needs_drop(src_type) {
                            // Owned resource — clone if borrowed/shared
                            let can_skip_clone = ctx.is_fresh_string(builder, place.local)
                                || (ctx.is_owned_local(builder, place.local)
                                    && !ctx.has_string_borrowers(builder, place.local));
                            if !can_skip_clone {
                                if let Some(clone_fn) = ctx.clone_fn_for_ptr(src_type) {
                                    let cloned = ctx.emit_clone(
                                        builder,
                                        &clone_fn,
                                        vec![operand.clone()],
                                        expr.span,
                                        src_type,
                                        crate::ir::ImplicitCloneReason::ReturnFromBorrow,
                                    );
                                    operand = FunctionBuilder::copy(cloned);
                                    returned_local = Some(cloned);
                                }
                            }
                        }
                    }
                }
                // Wrap value in Result.Ok — the operand's local is consumed (moved into Result)
                let ok_dst = {
                    let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
                    builder.enum_init(type_name, "Ok", result_type, vec![operand])
                };
                // Tier 1c: Move-mode (LIR zeros ok_dst). No explicit
                // GIR-level `move_zero_and_mark` — that adds a second
                // zero that corrupts the drop flag tracking on rethrow
                // shapes where the source local of the rethrow's
                // intermediate is read again via tag_of.
                if ctx.type_registry.is_resource_type(result_type) {
                    builder.assign_mode(
                        crate::ir::instructions::AssignMode::Move,
                        Place::local(LocalId(0)),
                        FunctionBuilder::copy(ok_dst),
                    );
                } else {
                    builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(ok_dst));
                }
                // Zero out the original local (its value is now owned by the Result)
                if let Some(local) = returned_local {
                    ctx.move_zero_and_mark(builder, local);
                }
            }
        } else {
            let ret_type = builder.locals[0].type_id;
            // Clone string returns unless the source can be proven independent:
            // 1. Fresh string temps (user function call results) — fresh allocation
            // 2. Named owned locals with no string borrowers — sole data holder
            // All other cases (field loads, pattern extracts, locals with borrowers)
            // may share heap data with other variables and MUST be cloned.
            let mut did_clone_return = false;
            if ret_type == ctx.type_mapper.owned_string_type {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty() {
                        let rhs_type = builder.local_type(place.local);
                        let can_skip_clone = ctx.is_fresh_string(builder, place.local)
                            || (ctx.is_owned_local(builder, place.local)
                                && ctx.is_named_local(place.local)
                                && !ctx.has_string_borrowers(builder, place.local));
                        if rhs_type == ctx.type_mapper.owned_string_type
                            && !can_skip_clone
                        {
                            ctx.warn_clone_and_hit(builder, expr.span, rhs_type, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                            let clone_fn = ctx.clone_fn_for_ptr(rhs_type)
                                .unwrap_or_else(|| "gorget_string_from_str".to_string());
                            let clone_result = builder.call_clone(
                                &clone_fn,
                                vec![operand.clone()],
                                ret_type,
                                crate::ir::ImplicitCloneReason::ReturnFromBorrow,
                            );
                            builder.assign_mode(
                                crate::ir::instructions::AssignMode::Move,
                                Place::local(LocalId(0)),
                                FunctionBuilder::copy(clone_result),
                            );
                            ctx.move_zero_and_mark(builder, clone_result);
                            did_clone_return = true;
                        }
                    }
                }
            }
            if !did_clone_return {
                // ── The return boundary's ONE materialize decision ──────────
                // A `return` is an unconditional leave-behind ownership
                // boundary, so it routes through the SHARED chokepoint
                // (`ensure_owned_at_boundary`) — byte-for-byte the same guard
                // the EXPRESSION-BODY return already uses
                // (`functions.rs:1453,1764,2045,2456`). That is why `f(&v): v`
                // was always clean while `f(&v): return v` double-freed: the
                // statement return had a hand-rolled `GirType::Ptr`-only
                // sibling that is blind to the `MutPtr` an `&`-param actually
                // is, and blind to a by-value borrow temp entirely.
                // The chokepoint keys on `pointee_type` (Ptr AND MutPtr) plus
                // the by-value borrow/untracked-resource predicate, so it
                // subsumes the resource-clone leg below (Core #4: retire the
                // sibling, don't patch it).
                //
                // ⚠ The OWNING `!`-param return stays IN FRONT of this
                // chokepoint. `return v` for a `!`-param is a transfer, not a
                // borrow: the caller already gave the callee ownership, and the
                // trailing `move_zero(owning_param_returned)` below hands it
                // onward. The chokepoint cannot see that — its `!`-param escape
                // (`maybe_move_owning_param_ctor_temp`) additionally requires
                // `is_single_use`, so a `!` param that is reassigned in a loop
                // and then returned would pick up a wasteful clone. Memory-safe,
                // charter-breaking; measured as 2 new `ReturnFromBorrow` sites
                // on the self-host self-compile before this guard.
                if owning_param_returned.is_none()
                    && !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_)))
                {
                    let src_before = match &operand {
                        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                        _ => None,
                    };
                    operand = ctx.ensure_owned_at_boundary(
                        builder,
                        operand,
                        expr.span,
                        crate::ir::ImplicitCloneReason::ReturnFromBorrow,
                    );
                    let src_after = match &operand {
                        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                        _ => None,
                    };
                    if src_after != src_before {
                        // The chokepoint materialized: the fresh owned local is
                        // what leaves the frame. The old source was a borrow (or
                        // an untracked alias) — it never owned the buffer, so it
                        // is deliberately NOT move-zeroed here; the trailing
                        // move-zero block below now targets the clone.
                        returned_local = src_after;
                    }
                }
                // Ptr(T) → T auto-deref for return values: if the operand
                // is Ptr(T) but the return type is T and the pointee is a
                // non-resource (primitives, value structs), deref the pointer.
                // The resource-clone leg that used to live here is retired —
                // the chokepoint above owns that decision now. The Ptr-
                // propagation fallback (resource pointee with no clone fn)
                // stays: it retypes `_0` and `set_ref`s it, which no
                // chokepoint models.
                if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    if p.projections.is_empty() {
                        let src_idx = p.local.0 as usize;
                        if src_idx < builder.locals.len() {
                            let src_type = builder.locals[src_idx].type_id;
                            if let Some(GirType::Ptr(inner)) = ctx.type_registry.get(src_type).cloned() {
                                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                                    if !ctx.type_registry.is_resource_type(inner) {
                                        // Non-resource pointee — deref to load the value.
                                        let tmp = builder.add_local(inner, None);
                                        builder.assign(
                                            Place::local(tmp),
                                            Operand::Copy(Place {
                                                local: p.local,
                                                projections: vec![Projection::Deref],
                                            }),
                                        );
                                        operand = FunctionBuilder::copy(tmp);
                                    } else {
                                        // Resource without clone fn — fall back to Ptr propagation
                                        builder.locals[0].type_id = src_type;
                                        builder.return_type = src_type;
                                        ctx.set_ref(builder, LocalId(0));
                                    }
                                }
                            }
                        }
                    }
                }
                // Option[Ref[T]] → Option[T] return conversion: when a function
                // returns Option[T] but the operand is Option[Ref[T]] (from a
                // collection .get() on a borrowed field), extract the payload:
                // clone for resource types, deref for primitives/value types.
                // Without this, memcpy would read sizeof(Option[T]) bytes from a
                // sizeof(Option[Ref[T]]) source — buffer overflow.
                let src_type = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    if p.projections.is_empty() && (p.local.0 as usize) < builder.locals.len() {
                        Some(builder.local_type(p.local))
                    } else { None }
                } else { None };
                if let Some(src_ty) = src_type {
                    if let Some(converted) = try_lift_option_ref(ctx, builder, &operand, src_ty, ret_type, expr.span) {
                        // Tier 1c: the converted operand is the lift's
                        // `merge` local — Owned, fresh, dead immediately
                        // after this return-slot assign. Use Move so the
                        // return slot doesn't shallow-alias.
                        let converted_local = if let Operand::Copy(ref p) | Operand::Move(ref p) = converted {
                            if p.projections.is_empty() { Some(p.local) } else { None }
                        } else { None };
                        if converted_local.is_some() && ctx.type_registry.is_resource_type(ret_type) {
                            builder.assign_mode(
                                crate::ir::instructions::AssignMode::Move,
                                Place::local(LocalId(0)),
                                converted,
                            );
                            if let Some(l) = converted_local {
                                if !ctx.drops.is_moved(l) {
                                    ctx.move_zero_and_mark(builder, l);
                                }
                            }
                        } else {
                            builder.assign(Place::local(LocalId(0)), converted);
                        }
                        builder.ret(FunctionBuilder::copy(LocalId(0)));
                        ctx.func_state.expected_type = prev_expected;
                        return;
                    }
                }
                // Use Move for locals that own their data (call results, constructors).
                // Move (memcpy) avoids a C backend clone that leaks the original
                // data when MoveZero zeros the source without freeing.
                // Locals from field/pattern extracts may be shallow copies — Clone
                // is needed to produce an independent return value.
                //
                // Phase C: the condition is widened to "any bare-place return of
                // a needs_drop local" because the post-assign block at line 1135
                // unconditionally emits move_zero on the source for these cases.
                // GIR mode follows runtime intent — Copy + move_zero at the source
                // IS Move semantics, and the validator (correctly) flagged the
                // original Copy mode as a shallow alias. Named non-owned locals
                // (rare on this path — see lower_var_decl ownership propagation)
                // would benefit from Clone instead, but that's a C3 audit
                // refinement and the current move_zero behavior is unchanged.
                // Cluster 5 probe (2026-05-10): the disjunction
                // `needs_drop || is_resource_type` is NOT redundant. See
                // `lowering/functions.rs:28` for the full reasoning.
                let use_move = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    let local_ty = builder.local_type(p.local);
                    p.projections.is_empty()
                        && (ctx.type_registry.needs_drop(local_ty)
                            || ctx.type_registry.is_resource_type(local_ty))
                } else { false };
                if use_move {
                    builder.assign_mode(
                        crate::ir::instructions::AssignMode::Move,
                        Place::local(LocalId(0)),
                        operand.clone(),
                    );
                    if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                        ctx.move_zero_and_mark(builder, p.local);
                    }
                } else {
                    builder.assign(Place::local(LocalId(0)), operand.clone());
                }
            }
            // When did_clone_return, the original source still has its own
            // allocation (the clone produced an independent copy). Let scope-exit
            // drops free it — don't unregister or move-zero it.
            if did_clone_return {
                returned_local = None; // original is NOT the returned local
            }
            if !did_clone_return {
                // If the return local is str-typed and the operand is a GorgetString temp,
                // unregister the temp to prevent use-after-free: the str view in the return
                // local may be accessed after the temp's scope exit frees it.
                if ret_type == ctx.type_mapper.owned_string_type {
                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        if place.projections.is_empty() {
                            let rhs_type = builder.local_type(place.local);
                            if rhs_type == ctx.type_mapper.owned_string_type {
                                ctx.drops.unregister(place.local);
                            }
                        }
                    }
                }
                // Move-zero source locals on return to prevent double-free.
                // The return assigns into slot 0 (shallow copy). Without zeroing the
                // source, both the source and the return slot share heap data — the
                // source gets Recursive/Trivial drop at scope exit, and the caller
                // drops the return value later → double-free.
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty() && place.local != LocalId(0) {
                        let rhs_type = builder.local_type(place.local);
                        if ctx.type_registry.needs_drop(rhs_type) {
                            ctx.move_zero_and_mark(builder, place.local);
                        }
                        // Tuple return: also MoveZero the individual element locals.
                        // TupleInit copies element values into the tuple struct, then
                        // the tuple is copied into the return slot. Without zeroing
                        // the element locals, both the return tuple and the locals
                        // own the same heap data → double-free at scope exit.
                        //
                        // Phase D §6.3: typed walk over
                        // `Borrowed { TupleElement { tuple, .. } }`. The
                        // sidecar `tuple_element_locals` is retired; the
                        // writer at `Inst::TupleInit` perf-gates on
                        // `needs_drop(elem_ty)` so this walk only finds
                        // droppable element sources (which is exactly
                        // what the MoveZero loop below filters for).
                        let elem_locals: Vec<LocalId> =
                            ctx.tuple_element_sources(builder, place.local);
                        for elem_local in elem_locals {
                            if elem_local != LocalId(0)
                                && !ctx.drops.is_moved(elem_local)
                            {
                                let elem_type = builder.local_type(elem_local);
                                if ctx.type_registry.needs_drop(elem_type) {
                                    builder.move_zero(Place::local(elem_local));
                                    ctx.drops.mark_moved(elem_local);
                                }
                            }
                        }
                    }
                }
            }
        }
        // For move-overridden generic params: zero the source through the pointer
        // to transfer ownership to the caller and prevent double-free.
        // Phase D4: typed LocalId-keyed set replaces the legacy
        // name-based HashSet<String>.
        if !ctx.func_state.move_override_params.is_empty() {
            if let Expr::Identifier(name) = &expr.node {
                if let Some((local_id, _)) = ctx.lookup_local(name.as_str()) {
                    if ctx.func_state.move_override_params.contains(&local_id) {
                        builder.move_zero(crate::ir::instructions::Place {
                            local: local_id,
                            projections: vec![crate::ir::instructions::Projection::Deref],
                        });
                    }
                }
            }
        }
        // Ownership boundary: materialize view elements in returned collections.
        // The return value in _0 may contain string views (cap=0) from for-loop
        // borrows or split results. These must be materialized to owned copies
        // before the function's locals (including view sources) are dropped.
        {
            let ret_type = builder.locals[0].type_id;
            let ret_name = ctx.type_registry.type_name(ret_type).unwrap_or_default();
            // Read typed `collection_kind` (Phase A) instead of name-prefix
            // matching. Both runtime singletons and monomorphized aliases
            // (Vector__T/Dict__K__V/...) carry the kind in their TypeDef.
            let kind = ctx.type_registry.get_type_def(&ret_name)
                .and_then(|td| td.metadata.collection_kind);
            let is_array_type = kind == Some(crate::ir::types::CollectionKind::Array);
            let is_dict_type = matches!(kind,
                Some(crate::ir::types::CollectionKind::OrderedMap)
                | Some(crate::ir::types::CollectionKind::Map));
            // Skip materialization when the function has no string borrows —
            // no views could have been pushed into the returned collection.
            if (is_array_type || is_dict_type) && ctx.func_state.has_string_borrows {
                let ptr_type = ctx.register_ptr_type(ret_type);
                let ptr = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(ptr, Place::local(LocalId(0)));
                let fn_name = if is_array_type {
                    "gorget_array_materialize_all"
                } else {
                    "gorget_map_materialize_keys"
                };
                builder.call_void(fn_name, vec![FunctionBuilder::copy(ptr)]);
            }
        }

        // Postcondition checks: `assert return <expr>` — check before returning
        emit_postcondition_checks(ctx, builder);

        // `return v` where `v` is a `!`-sigil resource parameter: invalidate the
        // param slot now so the function-exit owning-param drop guard's flag
        // flips to false. The return slot already holds the data (memcpy'd
        // from `*v` via the lower_expr Move/Copy path).
        if let Some(owning_local) = owning_param_returned {
            ctx.move_zero_and_mark(builder, owning_local);
        }

        // P2.6: Emit cleanup drops for all scopes being exited
        // Exclude the local being returned (it's moved into _0, not consumed)
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, returned_local);
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    } else {
        // Bare `return` in a throws-widened fn: ret_type is Result__V__E, not unit.
        // Return the zero-inited _0 (tag 0 = Ok), not const_unit() (an int32 → C type
        // mismatch). Typed metadata (enum_category), not name-matching. Mirrors the
        // self-host fix 167cb1b6 and the fall-off path (which returns copy(_0)).
        let ret_type = builder.locals[0].type_id;
        if ctx.type_registry.enum_category(ret_type) == Some(EnumCategory::Result) {
            ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, Some(LocalId(0)));
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        } else {
            // P2.6: Emit cleanup drops for all scopes being exited
            ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
            builder.ret(FunctionBuilder::const_unit());
        }
    }
}

/// Emit postcondition checks (`assert return`) at a return site.
/// Temporarily registers `__return__` as a local alias for `LocalId(0)` (the return slot),
/// then lowers each accumulated postcondition as a regular assert.
fn emit_postcondition_checks(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if ctx.func_state.postconditions.is_empty() {
        return;
    }
    // Register __return__ → _0 so the postcondition expression can reference the return value
    let ret_type = builder.locals[0].type_id;
    ctx.register_local("__return__", LocalId(0), ret_type);

    let postconditions = ctx.func_state.postconditions.clone();
    for (condition, message) in &postconditions {
        lower_assert(ctx, builder, condition, message.as_ref());
    }
}

/// Lower an if/elif/else statement.
fn lower_if(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_body: &Block,
    elif_branches: &[(Spanned<Expr>, Block)],
    else_body: &Option<Block>,
) {
    // Sub-sub-phase instrumentation: each phase records its exclusive (self)
    // time into `ctx.lower_fn_sub_times` under
    // `lower_function::body::lower_block::stmt::if::<sub>`, computed as
    // `elapsed - (stmt_nested_dur delta during this phase)`. The parent
    // `lower_stmt` Stmt::If arm already subtracts the wall time of
    // `lower_if` as a whole from its own bucket — so the per-sub-phase
    // buckets here sum to the `stmt::if` exclusive total with no double
    // counting from nested `lower_block`/`lower_stmt` calls.
    let __if_phase = |ctx: &mut LoweringContext, key: &'static str, t0: std::time::Instant, nested_entry: std::time::Duration| {
        let elapsed = t0.elapsed();
        let nested_during = ctx.stmt_nested_dur - nested_entry;
        let exclusive = elapsed.saturating_sub(nested_during);
        *ctx.lower_fn_sub_times.entry(key).or_default() += exclusive;
    };

    // Planner consumer #1: the pre-branch bare-param materialize is hoisted at
    // the `lower_stmt` DISPATCH ARM (`materialize_scope_carried_bare_params`),
    // one shared entry for every non-loop scope form — so `lower_if` itself no
    // longer carries it (the dispatch arm runs in the SAME pre-scope block, no
    // block is created between there and here). See that fn's doc.

    // ── cond_eval: condition expression + branch setup ─────────────────
    let __cond_t0 = std::time::Instant::now();
    let __cond_nested_entry = ctx.stmt_nested_dur;
    let merge_bb = builder.new_block();

    // Lower the condition. Auto-deref Ref[bool] → bool — `if v.get(i).unwrap():`
    // returns Ptr(bool) post-1.7b, but `branch` needs a bool value, not a
    // pointer (whose non-null bit is always true once safe_get checks pass).
    let cond = lower_expr(ctx, builder, condition);
    let cond = deref_bool_if_ptr(ctx, builder, cond);

    let then_bb = builder.new_block();
    let first_else_bb = if !elif_branches.is_empty() || else_body.is_some() {
        builder.new_block()
    } else {
        merge_bb
    };

    builder.branch(cond, then_bb, first_else_bb);

    // Snapshot the pre-branch `maybe_moved` flags so each alternative branch
    // sees the same starting view of which locals have been moved. Without
    // this, `mark_moved` calls in the then-branch leak into the elif/else
    // lowering and cause the elif's field-store/move logic to skip the
    // required move_zero — leading to a heap double-drop at scope exit (snag #8,
    // 2026-05-05). After all branches finish we union their post-branch
    // snapshots into the current state — conservative join, matching the
    // borrow-checker's branch-merging semantics.
    let pre_branch_moved = ctx.drops.snapshot_moved();
    let mut post_branch_snapshots: Vec<Vec<(usize, usize, bool)>> = Vec::new();
    __if_phase(ctx, "lower_function::body::lower_block::stmt::if::cond_eval", __cond_t0, __cond_nested_entry);

    // ── then_branch ────────────────────────────────────────────────────
    let __then_t0 = std::time::Instant::now();
    let __then_nested_entry = ctx.stmt_nested_dur;
    builder.switch_to(then_bb);
    let saved_then = ctx.save_locals(builder);
    ctx.drops.push_scope(DropScopeKind::Block);
    emit_is_bindings(ctx, builder, condition);
    lower_block(ctx, builder, then_body);
    // Use is_terminated() rather than block_always_returns(): handles any early exit
    // (return/break/continue, including nested match/while) and prevents double-drops.
    if builder.is_terminated() {
        ctx.drops.pop_scope_no_emit();
    } else {
        ctx.drops.pop_scope(builder, &ctx.type_registry);
        builder.jump(merge_bb);
    }
    ctx.restore_locals(builder, saved_then);
    post_branch_snapshots.push(ctx.drops.snapshot_moved());
    ctx.drops.restore_moved(&pre_branch_moved);
    __if_phase(ctx, "lower_function::body::lower_block::stmt::if::then_branch", __then_t0, __then_nested_entry);

    // ── elif_branches ──────────────────────────────────────────────────
    let __elif_t0 = std::time::Instant::now();
    let __elif_nested_entry = ctx.stmt_nested_dur;
    let mut current_else_bb = first_else_bb;
    for (i, (elif_cond, elif_body)) in elif_branches.iter().enumerate() {
        builder.switch_to(current_else_bb);
        let elif_cond_op = lower_expr(ctx, builder, elif_cond);

        let elif_then_bb = builder.new_block();
        let next_else_bb = if i + 1 < elif_branches.len() || else_body.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        builder.branch(elif_cond_op, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        let saved_elif = ctx.save_locals(builder);
        ctx.drops.push_scope(DropScopeKind::Block);
        emit_is_bindings(ctx, builder, elif_cond);
        lower_block(ctx, builder, elif_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }
        ctx.restore_locals(builder, saved_elif);
        post_branch_snapshots.push(ctx.drops.snapshot_moved());
        ctx.drops.restore_moved(&pre_branch_moved);

        current_else_bb = next_else_bb;
    }
    __if_phase(ctx, "lower_function::body::lower_block::stmt::if::elif_branches", __elif_t0, __elif_nested_entry);

    // ── else_branch ────────────────────────────────────────────────────
    let __else_t0 = std::time::Instant::now();
    let __else_nested_entry = ctx.stmt_nested_dur;
    if let Some(else_body) = else_body {
        builder.switch_to(current_else_bb);
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
    __if_phase(ctx, "lower_function::body::lower_block::stmt::if::else_branch", __else_t0, __else_nested_entry);

    // ── phi_merge: union post-branch snapshots + switch to merge bb ───
    let __merge_t0 = std::time::Instant::now();
    let __merge_nested_entry = ctx.stmt_nested_dur;
    // Conservative join: union each branch's moves into the post-if state.
    for snap in &post_branch_snapshots {
        ctx.drops.union_moved(snap);
    }

    builder.switch_to(merge_bb);
    __if_phase(ctx, "lower_function::body::lower_block::stmt::if::phi_merge", __merge_t0, __merge_nested_entry);
}

/// CoW 2G — loop-carried bare-param materialize. Called at every loop
/// PRE-HEADER (before the header/condition is lowered and before the body's
/// `save_locals`). For each in-scope bare (borrow) param the loop's own
/// statements (+ the `while` condition, which re-executes each iteration)
/// mutate, eagerly materialize a private owned copy HERE via the EXISTING
/// `cow_before_mutation` and rebind the name — so the loop condition, body, and
/// exit all read the persistent private copy.
///
/// Why the pre-header (not the in-body write site): the in-body materialize
/// (`cow_before_mutation`) rebinds the name inside `lower_block(body)`, but the
/// loop's `restore_locals` reverts that rebind every iteration AND the
/// condition/exit blocks resolve the name to the pre-loop param-borrow slot — so
/// the private copy is thrown away each iteration (per-iteration throwaway
/// clone; infinite loop when the condition reads the param). Hoisting the
/// materialize to the pre-header makes the fresh owned local a pre-loop slot
/// that LIR-SSA phis at the header (the same loop-carried-slot substrate
/// `emit_lazy_loopcarried_borrow` relies on), and the rebind is captured by
/// `save_locals` so it survives `restore_locals`. Devbook/11 2G: fix at the
/// WRITE site, never phi-repair at the loop head.
///
/// Detection routes through the SHARED CoW prescan collectors
/// (`functions::cow_mutations_in_loop`) — never a parallel AST walker
/// (devbook/24 one-source-of-truth). OVER-approximation is safe (a private copy
/// for a param the body does not actually mutate is observationally identical —
/// bare-param private-copy semantics start == the caller's bytes, so a pre-loop
/// clone equals lazy-at-first-write; just an extra clone). UNDER-approximation
/// re-creates the per-iteration throwaway, so the prescan errs toward MORE
/// mutation markers.
fn materialize_loop_carried_bare_params(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    body: &Block,
    condition: Option<&Expr>,
    else_body: Option<&Block>,
    span: crate::span::Span,
) {
    let mut_set = crate::ir::lowering::functions::cow_mutations_in_loop(
        &body.stmts,
        condition,
        else_body.map(|b| b.stmts.as_slice()),
        &ctx.fn_param_ownerships,
    );
    if mut_set.is_empty() {
        return;
    }
    // Snapshot the (name, local) candidates first to avoid borrowing
    // `ctx.func_state.locals` across the `&mut ctx` materialize calls.
    let candidates: Vec<(String, LocalId)> = ctx
        .func_state
        .locals
        .iter()
        .filter(|(_, (lid, _))| ctx.is_bare_param(builder, *lid))
        .map(|(n, (lid, _))| (n.clone(), *lid))
        .collect();
    for (name, local) in candidates {
        // Re-check is_bare_param defensively (an earlier candidate's materialize
        // rebinds only its own name, but guard against shadows) and query the
        // shared prescan set.
        if ctx.is_bare_param(builder, local)
            && crate::ir::lowering::functions::loop_set_mutates(&mut_set, &name)
        {
            ctx.cow_before_mutation_loop_preheader(builder, local, span);
        }
    }
}

/// Planner consumer #1 — scope-carried bare-param materialize. Called at the
/// `lower_stmt` DISPATCH ARM of every NON-LOOP scope form (`if`/elif/else,
/// `with`, `unsafe`, named-scope, `match` arms, `select` arms) — i.e. in the
/// PRE-SCOPE block, before the scope fn creates any block or runs its
/// `save_locals`. For each in-scope bare (borrow) param the scope statement
/// mutates on ANY path, eagerly materialize a private owned copy HERE through the
/// plan (`apply_materialize_directive` with a `BranchPreHeader` directive, stamped
/// `BranchPreHeaderMaterialize`) and rebind the name — so every branch/arm and
/// the post-scope merge read the persistent private copy.
///
/// Why the pre-scope hoist (not the in-body write site): the in-body materialize
/// (`cow_before_mutation`) rebinds the name inside `lower_block(scope-body)`, but
/// the scope's per-branch/per-arm `restore_locals` reverts that rebind, and the
/// merge block resolves the name to the stale pre-scope param-borrow slot — so
/// the private copy is thrown away (the `cow_loop_bare_param_if_branch` gap:
/// prints 4 instead of 3). Hoisting the materialize BEFORE the scope dispatch
/// makes the fresh owned local dominate the merge on every path, so the merge
/// needs no phi. Devbook/11 2G: fix at the WRITE site, never phi-repair at the
/// merge. (Conditional scopes therefore hoist to the dominating pre-scope point;
/// the identical treatment is sound for the straight-line scopes too — `with` /
/// `unsafe` / named-scope have a single predecessor, so the entry hoist trivially
/// dominates.)
///
/// Eager-here-is-observationally-lazy: a bare param's private copy starts == the
/// caller's bytes, so a pre-scope clone for a param a not-taken branch would have
/// mutated is indistinguishable from clone-at-first-write; it only fires when the
/// scope statically mutates the param SOMEWHERE. Over-approximation costs one
/// extra clone; under-approximation revives the thrown-away private copy.
///
/// Detection routes through the SHARED prescan collector
/// (`functions::cow_mutations_in_stmt`, which IS `cow_after_stmt` over the whole
/// scope statement) — never a parallel AST walker (devbook/24 one-source-of-truth).
/// `stmt` MUST be one of the non-loop scope forms; `Stmt::While`/`Stmt::For` are
/// handled by `materialize_loop_carried_bare_params` instead (which keeps
/// `LoopPreHeaderMaterialize`, so per-position costing stays honest).
fn materialize_scope_carried_bare_params(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    stmt: &Stmt,
    span: crate::span::Span,
) {
    let mut_set = crate::ir::lowering::functions::cow_mutations_in_stmt(
        stmt,
        &ctx.fn_param_ownerships,
    );
    if mut_set.is_empty() {
        return;
    }
    let candidates: Vec<(String, LocalId)> = ctx
        .func_state
        .locals
        .iter()
        .filter(|(_, (lid, _))| ctx.is_bare_param(builder, *lid))
        .map(|(n, (lid, _))| (n.clone(), *lid))
        .collect();
    for (name, local) in candidates {
        if ctx.is_bare_param(builder, local)
            && crate::ir::lowering::functions::loop_set_mutates(&mut_set, &name)
        {
            ctx.apply_materialize_directive(
                builder,
                MaterializeDirective {
                    root: local,
                    reason: crate::ir::ImplicitCloneReason::BranchPreHeaderMaterialize,
                    position: MaterializePosition::BranchPreHeader { anchor: span },
                },
            );
        }
    }
}

/// Lower a while loop.
fn lower_while(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    // CoW 2G: materialize loop-carried bare-param mutations in the PRE-HEADER
    // (the current block), BEFORE the condition is lowered into the header and
    // before the body's `save_locals`. The condition re-executes every
    // iteration, so it is scanned for mutations too.
    materialize_loop_carried_bare_params(ctx, builder, body, Some(&condition.node), else_arm, condition.span);

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // For while-else: use a break flag to skip else on break
    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    // Jump from current block to header
    builder.jump(header_bb);

    // Header: evaluate condition, branch. Auto-deref Ref[bool] → bool just
    // like `if`.
    builder.switch_to(header_bb);
    let cond = lower_expr(ctx, builder, condition);
    let cond = deref_bool_if_ptr(ctx, builder, cond);
    builder.branch(cond, body_bb, else_exit_bb);

    // Body: execute, jump back to header (wrapped in Loop scope for drop cleanup)
    builder.switch_to(body_bb);
    let saved_while = ctx.save_locals(builder);
    emit_is_bindings(ctx, builder, condition);
    ctx.push_loop(header_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_while);
    builder.jump(header_bb);

    // Else block: executed when loop completes naturally (no break)
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);

        // Break exit goes directly to exit (skipping else)
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    // Continue from exit
    builder.switch_to(exit_bb);
}
/// Lower an infinite `loop: body` statement.
fn lower_loop(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    body: &Block,
) {
    // CoW 2G: materialize loop-carried bare-param mutations in the PRE-HEADER.
    // No condition to scan for a bare `loop`; body-only detection.
    if let Some(first) = body.stmts.first() {
        materialize_loop_carried_bare_params(ctx, builder, body, None, None, first.span);
    }

    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // Jump into the loop body
    builder.jump(body_bb);

    // Body: execute, jump back to body (infinite loop)
    builder.switch_to(body_bb);
    let saved_loop = ctx.save_locals(builder);
    ctx.push_loop(body_bb, exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_loop);
    builder.jump(body_bb);

    // Exit (reached via break)
    builder.switch_to(exit_bb);
}

/// Lower a `break` statement.
fn lower_break(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if let Some(loop_info) = ctx.current_loop() {
        let exit_bb = loop_info.exit_bb;
        // Emit cleanup drops up to the Loop scope
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Loop, None);
        builder.jump(exit_bb);
        // Create unreachable block to absorb dead code after break
        let dead_bb = builder.new_block();
        builder.switch_to(dead_bb);
    }
}

/// Lower a `continue` statement.
fn lower_continue(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if let Some(loop_info) = ctx.current_loop() {
        let header_bb = loop_info.header_bb;
        // Emit cleanup drops up to the Loop scope
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Loop, None);
        builder.jump(header_bb);
        // Create unreachable block to absorb dead code after continue
        let dead_bb = builder.new_block();
        builder.switch_to(dead_bb);
    }
}

// ---- P3.1: Match Statements ----
// ---- P3.3: Error Handling ----

/// Lower a `throw expr` statement.
fn lower_throw(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) {
    let val = lower_expr(ctx, builder, expr);
    if let Some(result_type) = ctx.func_state.current_throws_result_type {
        // Wrap error in Result.Error and return
        let val_local = if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
            if place.projections.is_empty() { Some(place.local) } else { None }
        } else { None };
        let err_dst = {
                let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
                builder.enum_init(type_name, "Error", result_type, vec![val])
            };
        // Tier 1c: Move-mode (LIR zeros err_dst at the consume site).
        if ctx.type_registry.is_resource_type(result_type) {
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Move,
                Place::local(LocalId(0)),
                FunctionBuilder::copy(err_dst),
            );
        } else {
            builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
        }
        // Mark consumed operand as moved to prevent double-free during early-exit drops
        if let Some(local) = val_local {
            ctx.move_zero_and_mark(builder, local);
        }
        emit_on_error_cleanups(ctx, builder);
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    } else {
        let code = Operand::Constant(Constant::I32(1));
        builder.call_extern("gorget_throw", vec![val, code], UNIT_TYPE);
        builder.unreachable();
    }
    // Create unreachable block for dead code after throw
    let dead_bb = builder.new_block();
    builder.switch_to(dead_bb);
}

/// Lower an `assert condition [, message]` statement.
fn lower_assert(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    message: Option<&Spanned<Expr>>,
) {
    // If strip-asserts directive is active, emit nothing (skip assert entirely)
    if ctx.strip_asserts {
        return;
    }

    // For binary comparison conditions without a custom message, emit a rich diagnostic
    // that includes the actual left/right values. Works for all types: primitives, strings,
    // and any named type with an eq/display method.
    if message.is_none() {
        if let Expr::BinaryOp { left, op, right } = &condition.node {
            if let Some((op_str, _)) = comparison_op_info(*op) {
                let lhs_op = lower_expr(ctx, builder, left);
                let rhs_op = lower_expr(ctx, builder, right);
                let lhs_type = infer_operand_type_full(ctx, &lhs_op, builder);
                let rhs_type = infer_operand_type_full(ctx, &rhs_op, builder);

                // Store in locals so values survive across basic blocks.
                // Phase C: pick Move for resource-typed temps so the GIR
                // mode matches the move-to-temp semantic. Primitives
                // stay Copy.
                use crate::ir::instructions::AssignMode;
                // Cluster 5 probe (2026-05-10): the disjunction
                // `is_resource_type || needs_drop` is NOT redundant.
                // See `lowering/functions.rs:28` for the full reasoning.
                let lhs_local = builder.add_local(lhs_type, None);
                let lhs_mode = if ctx.type_registry.is_resource_type(lhs_type)
                    || ctx.type_registry.needs_drop(lhs_type)
                { AssignMode::Move } else { AssignMode::Copy };
                builder.assign_mode(lhs_mode, Place::local(lhs_local), lhs_op);
                let rhs_local = builder.add_local(rhs_type, None);
                let rhs_mode = if ctx.type_registry.is_resource_type(rhs_type)
                    || ctx.type_registry.needs_drop(rhs_type)
                { AssignMode::Move } else { AssignMode::Copy };
                builder.assign_mode(rhs_mode, Place::local(rhs_local), rhs_op);

                // Emit type-appropriate comparison
                let cond_local = emit_assert_comparison(
                    ctx, builder, lhs_local, lhs_type, rhs_local, rhs_type, *op,
                );

                let pass_bb = builder.new_block();
                let fail_bb = builder.new_block();
                builder.branch(Operand::Copy(Place::local(cond_local)), pass_bb, fail_bb);
                builder.switch_to(fail_bb);

                // Convert both values to strings for the diagnostic
                let lhs_str = assert_value_to_string(ctx, builder, lhs_local, lhs_type);
                let rhs_str = assert_value_to_string(ctx, builder, rhs_local, rhs_type);

                // Call gorget_assert_fail_values(code, op, left_str, right_str)
                // (D11: the message-less comparison-assert is a USER-FACING
                // assert, semantically identical to the message form — so it
                // routes to trap[T_AssertFailed]+exit 101 too. The T_ code is
                // typed data from the production TrapKind registry, prepended
                // as the first arg — the 4-arg canonical sig is in
                // src/lir/runtime.rs, approach (a).)
                builder.call_extern_void(
                    "gorget_assert_fail_values",
                    vec![
                        FunctionBuilder::const_str(crate::trap::TrapKind::AssertFailed.code()),
                        FunctionBuilder::const_str(op_str),
                        FunctionBuilder::copy(lhs_str),
                        FunctionBuilder::copy(rhs_str),
                    ],
                );
                builder.unreachable();
                builder.switch_to(pass_bb);
                return;
            }
        }
    }

    let cond = lower_expr(ctx, builder, condition);

    let pass_bb = builder.new_block();
    let fail_bb = builder.new_block();

    builder.branch(cond, pass_bb, fail_bb);

    // Fail path: panic with message (allows test-mode setjmp to catch it).
    builder.switch_to(fail_bb);
    if let Some(msg) = message {
        // Custom message provided — lower it and pass to gorget_trap (D11:
        // T_AssertFailed + exit 101). Was gorget_panic. NOTE(Q-D): the trap
        // LOCATION renders `<unknown>:0:0` — a PRE-EXISTING GIR→LIR span-
        // propagation gap for these branch-target trap blocks (the old
        // gorget_panic assert emitted `<unknown>:0:0` too). Not conformance-
        // compared (location is impl-defined) and byte-identical across
        // backends; filed as a follow-up.
        let msg_op = lower_expr(ctx, builder, msg);
        let code_op = Operand::Constant(Constant::Str(
            crate::trap::TrapKind::AssertFailed.code().to_string()));
        builder.call_extern("gorget_trap", vec![code_op, msg_op], UNIT_TYPE);
        builder.unreachable();
        builder.switch_to(pass_bb);
        return;
    }
    // No custom message: generate a static message based on the expression shape.
    let panic_msg = generate_assert_static_msg(condition);
    builder.call_extern(
        "gorget_trap",
        vec![
            Operand::Constant(Constant::Str(
                crate::trap::TrapKind::AssertFailed.code().to_string())),
            Operand::Constant(Constant::Str(panic_msg)),
        ],
        UNIT_TYPE,
    );
    builder.unreachable();

    // Pass path: continue
    builder.switch_to(pass_bb);
}

/// Emit a comparison appropriate for the operand types. Returns a LocalId holding the bool result.
/// Handles primitives (IR cmp), strings (gorget_str_eq/cmp), and named types (Type__eq/compare).
fn emit_assert_comparison(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    lhs_local: LocalId,
    lhs_type: TypeId,
    rhs_local: LocalId,
    _rhs_type: TypeId,
    op: BinaryOp,
) -> LocalId {
    let is_string = lhs_type == ctx.type_mapper.owned_string_type;

    // String comparison via runtime functions
    if is_string {
        if matches!(op, BinaryOp::Eq | BinaryOp::Neq) {
            let result = builder.call_extern(
                "gorget_str_eq",
                vec![FunctionBuilder::copy(lhs_local), FunctionBuilder::copy(rhs_local)],
                BOOL_TYPE,
            );
            if op == BinaryOp::Neq {
                return builder.un_op(UnOp::Not, BOOL_TYPE, FunctionBuilder::copy(result));
            }
            return result;
        }
        // Ordering comparisons: gorget_str_cmp returns int, compare with 0
        let cmp_result = builder.call_extern(
            "gorget_str_cmp",
            vec![FunctionBuilder::copy(lhs_local), FunctionBuilder::copy(rhs_local)],
            I64_TYPE,
        );
        let cmp_op = match op {
            BinaryOp::Lt => CmpOp::Lt,
            BinaryOp::Gt => CmpOp::Gt,
            BinaryOp::LtEq => CmpOp::Le,
            BinaryOp::GtEq => CmpOp::Ge,
            _ => unreachable!(),
        };
        return builder.cmp(
            cmp_op, I64_TYPE,
            FunctionBuilder::copy(cmp_result),
            Operand::Constant(Constant::I64(0)),
        );
    }

    // Named types: dispatch to Type__eq / Type__compare if available.
    // Route through emit_operator_overload_call (Core #4 — same ByPtr prep
    // + result tracking as binary/compound overload sites).
    if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(lhs_type).cloned() {
        if matches!(op, BinaryOp::Eq | BinaryOp::Neq) {
            let eq_method = format!("{type_name}__eq");
            if ctx.fn_sigs.contains_key(&eq_method) {
                let self_ptr_type = ctx.register_ptr_type(lhs_type);
                let self_ptr = builder.add_local(self_ptr_type, None);
                builder.emit_borrow(self_ptr, Place::local(lhs_local));
                let result = emit_operator_overload_call(
                    ctx,
                    builder,
                    eq_method,
                    vec![FunctionBuilder::copy(self_ptr), FunctionBuilder::copy(rhs_local)],
                    BOOL_TYPE,
                );
                if op == BinaryOp::Neq {
                    return builder.un_op(UnOp::Not, BOOL_TYPE, FunctionBuilder::copy(result));
                }
                return result;
            }
        }
        if matches!(op, BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq) {
            let compare_method = format!("{type_name}__compare");
            let has_compare = ctx.fn_sigs.contains_key(&compare_method)
                || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__compare")));
            if has_compare {
                let effective_name = if ctx.fn_sigs.contains_key(&compare_method) {
                    compare_method
                } else {
                    ctx.fn_sigs.keys()
                        .find(|k| k.ends_with(&format!("_for_{type_name}__compare")))
                        .cloned()
                        .unwrap_or(compare_method)
                };
                let self_ptr_type = ctx.register_ptr_type(lhs_type);
                let self_ptr = builder.add_local(self_ptr_type, None);
                builder.emit_borrow(self_ptr, Place::local(lhs_local));
                let cmp_result = emit_operator_overload_call(
                    ctx,
                    builder,
                    effective_name,
                    vec![FunctionBuilder::copy(self_ptr), FunctionBuilder::copy(rhs_local)],
                    I64_TYPE,
                );
                let cmp_op = match op {
                    BinaryOp::Lt => CmpOp::Lt,
                    BinaryOp::Gt => CmpOp::Gt,
                    BinaryOp::LtEq => CmpOp::Le,
                    BinaryOp::GtEq => CmpOp::Ge,
                    _ => unreachable!(),
                };
                return builder.cmp(
                    cmp_op, I64_TYPE,
                    FunctionBuilder::copy(cmp_result),
                    Operand::Constant(Constant::I64(0)),
                );
            }
        }
    }

    // Fallback: primitive IR comparison
    let cmp_op = comparison_op_info(op).map(|(_, c)| c).unwrap_or(CmpOp::Eq);
    builder.cmp(cmp_op, lhs_type, FunctionBuilder::copy(lhs_local), FunctionBuilder::copy(rhs_local))
}

/// Convert a local value to a GorgetString representation for assert diagnostics.
/// Returns a LocalId holding a GorgetString/Str value.
fn assert_value_to_string(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    local: LocalId,
    type_id: TypeId,
) -> LocalId {
    let owned_string_type = ctx.type_mapper.owned_string_type;

    // Integer types: call gorget_int_to_str
    if type_id == I64_TYPE || type_id == I32_TYPE || type_id == I16_TYPE || type_id == I8_TYPE
        || type_id == U64_TYPE || type_id == U32_TYPE || type_id == U16_TYPE || type_id == U8_TYPE
    {
        return builder.call_extern(
            "gorget_int_to_str",
            vec![FunctionBuilder::copy(local)],
            owned_string_type,
        );
    }

    // Float types: call gorget_float_to_str
    if type_id == F64_TYPE || type_id == F32_TYPE {
        return builder.call_extern(
            "gorget_float_to_str",
            vec![FunctionBuilder::copy(local)],
            owned_string_type,
        );
    }

    // Bool: call gorget_bool_to_str
    if type_id == BOOL_TYPE {
        return builder.call_extern(
            "gorget_bool_to_str",
            vec![FunctionBuilder::copy(local)],
            owned_string_type,
        );
    }

    // String types: use the value directly
    if ctx.type_mapper.is_string_type(type_id) {
        return local;
    }

    // Named types: call display method if available
    if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(type_id).cloned() {
        let display_method = format!("{type_name}__display");
        let has_display = ctx.fn_sigs.contains_key(&display_method)
            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__display")));
        if has_display {
            let effective_method = if ctx.fn_sigs.contains_key(&display_method) {
                display_method
            } else {
                ctx.fn_sigs.keys()
                    .find(|k| k.ends_with(&format!("_for_{type_name}__display")))
                    .cloned()
                    .unwrap_or(display_method)
            };
            let self_type = ctx.register_ptr_type(type_id);
            let self_ptr = builder.add_local(self_type, None);
            builder.emit_borrow(self_ptr, Place::local(local));
            return builder.call(
                effective_method,
                vec![FunctionBuilder::copy(self_ptr)],
                owned_string_type,
            );
        }
        // Named type without display — static placeholder
        let placeholder = builder.add_local(owned_string_type, None);
        builder.assign(Place::local(placeholder), FunctionBuilder::const_str(format!("<{type_name}>")));
        return placeholder;
    }

    // Opaque fallback
    let placeholder = builder.add_local(owned_string_type, None);
    builder.assign(Place::local(placeholder), FunctionBuilder::const_str("<opaque>"));
    placeholder
}

/// Return `(op_str, CmpOp)` for a comparison BinaryOp, or None for non-comparison ops.
fn comparison_op_info(op: BinaryOp) -> Option<(&'static str, CmpOp)> {
    match op {
        BinaryOp::Eq    => Some(("==", CmpOp::Eq)),
        BinaryOp::Neq   => Some(("!=", CmpOp::Ne)),
        BinaryOp::Lt    => Some(("<",  CmpOp::Lt)),
        BinaryOp::Gt    => Some((">",  CmpOp::Gt)),
        BinaryOp::LtEq  => Some(("<=", CmpOp::Le)),
        BinaryOp::GtEq  => Some((">=", CmpOp::Ge)),
        _ => None,
    }
}

/// Generate a static assertion failure message for an assertion condition.
/// For binary comparisons, includes the operator name (e.g., "left == right").
fn generate_assert_static_msg(condition: &Spanned<Expr>) -> String {
    if let Expr::BinaryOp { op, .. } = &condition.node {
        let op_str = match op {
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::LtEq => "<=",
            BinaryOp::GtEq => ">=",
            _ => return "assertion failed".to_string(),
        };
        format!("assertion failed: left {op_str} right")
    } else {
        "assertion failed".to_string()
    }
}

// ---- P3.5: With statement ----

/// Check if an expression is an allocator constructor (Arena, TrackingAllocator, PoolAllocator, TlsfAllocator, FixedBufferAllocator, FallbackAllocator).
fn is_allocator_constructor(expr: &Expr) -> bool {
    if let Expr::Call { callee, .. } = expr {
        if let Expr::Identifier(name) = &callee.node {
            return matches!(name.as_str(), "Arena" | "TrackingAllocator" | "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator" | "FallbackAllocator");
        }
    }
    false
}

/// Lower a named scope block: `identifier:\n    body`.
/// Opens a new drop scope so variables declared inside are dropped at block exit.
/// Lower a `snapshot "name" expr` statement — serialize value and write to snapshot file.
fn lower_snapshot(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    name: &Spanned<String>,
    value: &Spanned<Expr>,
) {
    let val_op = lower_expr(ctx, builder, value);
    let val_type = infer_operand_type_full(ctx, &val_op, builder);
    // §6.8 / Phase C: for resource-typed values, the prior `assign(local, op)`
    // was a shallow Copy. Use AssignMode::Borrow + skip drop registration —
    // the snapshot runtime fn reads via &Str without taking ownership; the
    // source's drop is the only one. For non-resource types, plain assign
    // is correct (Copy = bitwise read of a value).
    let val_local = builder.add_local(val_type, None);
    if ctx.type_registry.is_resource_type(val_type) {
        builder.assign_mode(
            crate::ir::instructions::AssignMode::Borrow,
            Place::local(val_local),
            val_op,
        );
    } else {
        builder.assign(Place::local(val_local), val_op);
    }

    let point_name = name.node.replace('\\', "\\\\").replace('"', "\\\"");
    let point_arg = FunctionBuilder::const_str(&point_name);

    // Emit the appropriate snapshot write call based on the value's type.
    // The runtime functions use __gorget_current_test internally.
    if val_type == I64_TYPE || val_type == I32_TYPE || val_type == I16_TYPE || val_type == I8_TYPE
        || val_type == U64_TYPE || val_type == U32_TYPE || val_type == U16_TYPE || val_type == U8_TYPE
    {
        builder.call_extern_void(
            "__gorget_snapshot_write_int",
            vec![point_arg, FunctionBuilder::copy(val_local)],
        );
    } else if val_type == F64_TYPE || val_type == F32_TYPE {
        builder.call_extern_void(
            "__gorget_snapshot_write_float",
            vec![point_arg, FunctionBuilder::copy(val_local)],
        );
    } else if val_type == BOOL_TYPE {
        builder.call_extern_void(
            "__gorget_snapshot_write_bool",
            vec![point_arg, FunctionBuilder::copy(val_local)],
        );
    } else if ctx.type_mapper.is_string_type(val_type) {
        builder.call_extern_void(
            "__gorget_snapshot_write_str",
            vec![point_arg, FunctionBuilder::copy(val_local)],
        );
    } else if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(val_type).cloned() {
        // Named types with display: call display, then write the result as a string
        let display_method = format!("{type_name}__display");
        let has_display = ctx.fn_sigs.contains_key(&display_method)
            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__display")));
        if has_display {
            let effective_method = if ctx.fn_sigs.contains_key(&display_method) {
                display_method
            } else {
                ctx.fn_sigs.keys()
                    .find(|k| k.ends_with(&format!("_for_{type_name}__display")))
                    .cloned()
                    .unwrap_or(display_method)
            };
            let self_type = ctx.register_ptr_type(val_type);
            let self_ptr = builder.add_local(self_type, None);
            builder.emit_borrow(self_ptr, Place::local(val_local));
            let owned_string_type = ctx.type_mapper.owned_string_type;
            let result = builder.call(
                effective_method,
                vec![FunctionBuilder::copy(self_ptr)],
                owned_string_type,
            );
            builder.call_extern_void(
                "__gorget_snapshot_write_str",
                vec![point_arg, FunctionBuilder::copy(result)],
            );
        } else {
            // No display method — write null
            builder.call_extern_void(
                "__gorget_snapshot_write_null",
                vec![point_arg],
            );
        }
    } else {
        // Unknown type — write null
        builder.call_extern_void(
            "__gorget_snapshot_write_null",
            vec![point_arg],
        );
    }
}

fn lower_named_scope(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    body: &Block,
) {
    let saved = ctx.save_locals(builder);
    ctx.drops.push_scope(DropScopeKind::Block);
    lower_block(ctx, builder, body);
    if builder.is_terminated() {
        ctx.drops.pop_scope_no_emit();
    } else {
        ctx.drops.pop_scope(builder, &ctx.type_registry);
    }
    ctx.restore_locals(builder, saved);
}

/// Lower a `with bindings: body` statement.
fn lower_with(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    bindings: &[ast::WithBinding],
    body: &Block,
) {
    let saved_with = ctx.save_locals(builder);
    ctx.drops.push_scope(DropScopeKind::Block);

    let mut allocator_locals = Vec::new();
    let mut shared_refresh_entries = Vec::new();

    for binding in bindings {
        // Detect shared variable bindings before lowering (lowering consumes the name mapping)
        let shared_facade = if let Expr::Identifier(ref name) = binding.expr.node {
            ctx.lookup_local(name).and_then(|(local_id, _)| {
                if ctx.shared.locals.contains_key(&local_id) {
                    Some(local_id)
                } else {
                    None
                }
            })
        } else {
            None
        };

        // Also detect param-based with bindings for spawned function refresh.
        // If the target is a param (not a declared shared), record it so the
        // shared_async GIR transform can emit refresh assignments after reacquire.
        let param_source = if shared_facade.is_none() {
            if let Expr::Identifier(ref name) = binding.expr.node {
                ctx.lookup_local(name).and_then(|(local_id, _)| {
                    // Params are locals _1.._N (local 0 is the return place)
                    let idx = local_id.0 as usize;
                    if idx >= 1 && idx <= builder.params.len() {
                        Some(local_id)
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        } else {
            None
        };

        let is_alloc = is_allocator_constructor(&binding.expr.node);
        let val = lower_expr(ctx, builder, &binding.expr);
        let type_id = super::exprs::infer_operand_type_full(ctx, &val, builder);
        let local_id = builder.add_local(type_id, Some(&binding.name.node));
        ctx.register_local(&binding.name.node, local_id, type_id);
        ctx.drops.register_local(local_id, type_id, &ctx.type_registry);
        // Phase C: pick Move for resource types (with-binding takes ownership
        // of the result; e.g. `with Resource(...) as r:` constructs a fresh
        // owned local). Primitives stay Copy.
        // Cluster 5 probe (2026-05-10): the disjunction
        // `is_resource_type || needs_drop` is NOT redundant. See
        // `lowering/functions.rs:28` for the full reasoning.
        let with_mode = if ctx.type_registry.is_resource_type(type_id)
            || ctx.type_registry.needs_drop(type_id)
        {
            crate::ir::instructions::AssignMode::Move
        } else {
            crate::ir::instructions::AssignMode::Copy
        };
        builder.assign_mode(with_mode, Place::local(local_id), val);

        // If this binding mirrors a shared variable, register for auto-refresh after await
        if let Some(facade_local) = shared_facade {
            shared_refresh_entries.push((local_id, facade_local));
        }

        // Record param-based with binding for shared_async transform
        if let Some(param_local) = param_source {
            builder.with_refresh_pairs.push((local_id, param_local));
        }

        // If this is an allocator, push it as the active thread-local allocator
        if is_alloc {
            builder.push_allocator(FunctionBuilder::copy(local_id));
            allocator_locals.push(local_id);
        }
    }

    // Push shared-refresh entries for the duration of the body
    let prev_refresh_len = ctx.func_state.with_shared_refresh.len();
    ctx.func_state.with_shared_refresh.extend(shared_refresh_entries);

    lower_block(ctx, builder, body);

    // Pop shared-refresh entries
    ctx.func_state.with_shared_refresh.truncate(prev_refresh_len);

    // Drop all non-allocator locals FIRST (while the allocator is still alive),
    // then pop + destroy allocators. This avoids use-after-free when collections
    // allocated within the `with` scope try to dealloc via the active allocator.
    ctx.drops.pop_scope(builder, &ctx.type_registry);

    for &local_id in allocator_locals.iter().rev() {
        builder.pop_allocator();
        let type_id = builder.local_type(local_id);
        let type_name = ctx.type_name_for_id(type_id);
        let destroy_fn = match type_name.as_deref() {
            Some("PoolAllocator") => Some("gorget_pool_destroy"),
            Some("TlsfAllocator") => Some("gorget_tlsf_destroy"),
            Some("TrackingAllocator") => Some("gorget_tracking_destroy"),
            Some("Arena") => Some("gorget_arena_destroy"),
            Some("FixedBufferAllocator") => Some("gorget_fba_destroy"),
            Some("FallbackAllocator") => Some("gorget_fallback_destroy"),
            _ => None,
        };
        if let Some(fn_name) = destroy_fn {
            builder.call_void(fn_name, vec![FunctionBuilder::copy(local_id)]);
        }
    }
    ctx.restore_locals(builder, saved_with);
}

/// If the condition is an `Expr::Is { expr, pattern, .. }`, emit pattern bindings
/// so that variables bound by the pattern (e.g., `if x is Some(v):`) become usable
/// in the then-branch.
pub fn emit_is_bindings(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
) {
    if let Expr::Is { expr: inner, pattern, negated } = &condition.node {
        if *negated {
            return; // `is not` — no bindings in the then branch
        }
        // Bug-1 fix: reuse the scrutinee local created when this same `Expr::Is`
        // node was lowered as a boolean value (the tag test in the enclosing
        // condition), keyed by the node's span start. RE-lowering `inner` here
        // would re-evaluate the scrutinee — a second call for a side-effecting
        // scrutinee (a mutating `&self` method returning Option), binding the
        // payload from the wrong evaluation. The value-lowering runs first and
        // always memoizes non-negated Is nodes, so the entry is present; the
        // re-lower path below is a defensive fallback only.
        //
        // The entry is READ (not removed): an `and`-chain binds its LEFT operand
        // in two dominated blocks — `lower_short_circuit`'s rhs block (so the
        // binding is in scope while evaluating the right operand) AND the outer
        // then/body block (for the branch body). Both must reuse the SINGLE
        // scrutinee evaluation; removing on first read would force the second
        // site to re-lower (re-invoking the scrutinee). The value-lowering block
        // dominates every binding site, so the local is always valid to read;
        // stale entries are harmless (unique spans, cleared per-function).
        if let Some((scrut_local, scrut_type)) =
            ctx.func_state.is_scrut_memo.get(&condition.span.start).copied()
        {
            emit_pattern_bindings(ctx, builder, pattern, scrut_local, scrut_type);
            return;
        }
        // Fallback: no memo (scrutinee was never value-lowered on this path) —
        // re-lower to get the scrutinee local. Safe for side-effect-free
        // scrutinees; the memo above covers every if/while/expr-if condition.
        let val = lower_expr(ctx, builder, inner);
        let scrut_type = super::exprs::infer_operand_type_full(ctx, &val, builder);
        let scrut_local = if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
            place.local
        } else {
            let tmp = builder.add_local(scrut_type, None);
            builder.assign(Place::local(tmp), val);
            tmp
        };
        emit_pattern_bindings(ctx, builder, pattern, scrut_local, scrut_type);
    }
    // Also handle `condition and is_expr` compound conditions
    if let Expr::BinaryOp { left, op: ast::BinaryOp::And, right } = &condition.node {
        emit_is_bindings(ctx, builder, left);
        emit_is_bindings(ctx, builder, right);
    }
}

/// Auto-deref `Ref[bool] → bool` for branch conditions. Applies to plain
/// `Copy(local)` / `Move(local)` operands whose local type is `Ptr(bool)`
/// (or `Ptr(T)` whose pointee is BOOL). Other operands pass through.
/// Used by `if` / `while` so `if v.get(i).unwrap():` evaluates the bool
/// value, not the pointer's non-null bit.
fn deref_bool_if_ptr(
    ctx: &LoweringContext,
    builder: &mut FunctionBuilder,
    operand: Operand,
) -> Operand {
    if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
        if p.projections.is_empty() {
            let local_type = builder.local_type(p.local);
            if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(local_type).cloned() {
                if inner == BOOL_TYPE {
                    let tmp = builder.add_local(BOOL_TYPE, None);
                    builder.assign(
                        Place::local(tmp),
                        Operand::Copy(Place {
                            local: p.local,
                            projections: vec![Projection::Deref],
                        }),
                    );
                    return FunctionBuilder::copy(tmp);
                }
            }
        }
    }
    operand
}

/// Resolve a mangled type-name fragment (`int64_t`, `GorgetString`, user
/// struct names, etc.) to the corresponding GIR `TypeId`. Mirrors the
/// `mangle_type_for_name` table: primitives come first, user types fall
/// through to `lookup_named`.
fn resolve_mangled_type(ctx: &LoweringContext, name: &str) -> Option<TypeId> {
    match name {
        "int64_t" | "int" => Some(I64_TYPE),
        "int32_t" => Some(I32_TYPE),
        "int16_t" => Some(I16_TYPE),
        "int8_t" => Some(I8_TYPE),
        "uint64_t" | "uint" => Some(U64_TYPE),
        "uint32_t" => Some(U32_TYPE),
        "uint16_t" => Some(U16_TYPE),
        "uint8_t" => Some(U8_TYPE),
        "double" | "float64" => Some(F64_TYPE),
        "float" | "float32" => Some(F32_TYPE),
        "bool" => Some(BOOL_TYPE),
        _ => ctx.type_mapper.lookup_named(name),
    }
}

/// Convert `Option[Ref[T]] → Option[T]` by tag branching and cloning the Some
/// payload. The input operand must be a bare `Copy`/`Move` of a local with
/// `Option__Ref__T` type; the target must be `Option__T` (non-Ref). Returns the
/// converted operand — a `Copy` of a freshly-filled merge local.
///
/// Returns `None` if the shapes don't match or the inner type has no clone
/// function (callers pass the operand through untouched).
///
/// This is the soundness fix for the `Option[Ref[T]] → Option[T]` info leak:
/// without it, the C backend emits a `memcpy(dst, src, sizeof(dst))` where
/// `sizeof(dst)` is larger than `src`, reading adjacent stack into the dst's
/// header fields and leaking them via subsequent use of the value.
fn try_lift_option_ref(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    operand: &Operand,
    src_type: TypeId,
    dst_type: TypeId,
    span: crate::span::Span,
) -> Option<Operand> {
    // Must be a bare Copy/Move of a whole local (no projections).
    let src_place = match operand {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p,
        _ => return None,
    };

    // Names must be `Option__Ref__T` (src) and `Option__T` (dst, non-Ref).
    let src_name = ctx.type_registry.type_name(src_type).unwrap_or_default();
    let dst_name = ctx.type_registry.type_name(dst_type).unwrap_or_default();
    if !src_name.starts_with("Option__Ref__") {
        return None;
    }
    if !dst_name.starts_with("Option__") || dst_name.starts_with("Option__Ref__") {
        return None;
    }

    // Extract inner type: `Option__Ref__GorgetString` → `GorgetString`.
    // Resolves primitives (int64_t, double, …) and user-named types alike.
    let inner_name = src_name.strip_prefix("Option__Ref__")?;
    let inner_type = resolve_mangled_type(ctx, inner_name)?;
    let clone_fn = ctx.clone_fn_for_ptr(inner_type);

    // CONDITIONAL clone site: bare `warn_implicit_clone` (not the
    // `warn_clone_and_hit` helper) because the clone only executes on the
    // Some arm's resource path — the hit is emitted there, below.
    // Allowlisted in tests/lints.rs::clone_warn_hit_pairing.
    let cid = ctx.warn_implicit_clone(span, inner_type, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);

    // Build: branch on tag; Some → extract+wrap; None → construct None.
    let tag_place = Place {
        local: src_place.local,
        projections: vec![Projection::Field(0)],
    };
    let tag = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(tag), Operand::Copy(tag_place));
    let is_some = builder.cmp(
        CmpOp::Eq, I64_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I64(0)),
    );
    let some_bb = builder.new_block();
    let none_bb = builder.new_block();
    let merge_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(is_some), some_bb, none_bb);

    // Merge local: holds the resulting Option[T].
    let merge = builder.add_local(dst_type, None);
    ctx.drops.register_local(merge, dst_type, &ctx.type_registry);
    // Tier 2a: tag merge as Owned. Both branches build the Option
    // (Some via enum_init from a cloned-or-dereffed payload, None via
    // enum_init []), so merge owns its data. Without this tag, the
    // caller's `assign(LocalId(0), copy(merge))` is flagged
    // `AssignIntoOwnedSlot(... — untracked source)`.
    ctx.set_owned(builder, merge);

    // Some branch: extract the Ref payload (void*). Assigning the whole
    // Option[Ref[T]] source to a Ptr(T) local triggers the LIR's
    // `try_enum_payload_extract`, which emits FieldPtr+Load with the correct
    // LIR::Ptr type tag (avoids the scalar/Ptr ABI tag mismatch that a raw
    // field projection hits). Then:
    //   - resource pointee → call clone_fn(ptr) → owned T
    //   - non-resource pointee (primitives, value structs) → *ptr (deref)
    builder.switch_to(some_bb);
    let ptr_type = ctx.register_ptr_type(inner_type);
    let ptr_local = builder.add_local(ptr_type, None);
    builder.assign(
        Place::local(ptr_local),
        Operand::Copy(Place::local(src_place.local)),
    );
    let owned_payload = if let Some(ref fn_name) = clone_fn {
        // Attribution: the clone only executes on the Some arm's resource path.
        ctx.emit_clone_site_hit(builder, cid);
        builder.call_clone(fn_name, vec![FunctionBuilder::copy(ptr_local)], inner_type, crate::ir::ImplicitCloneReason::VarDeclFromBorrow)
    } else {
        let tmp = builder.add_local(inner_type, None);
        builder.assign(
            Place::local(tmp),
            Operand::Copy(Place {
                local: ptr_local,
                projections: vec![Projection::Deref],
            }),
        );
        tmp
    };
    let some_result = builder.enum_init(
        &dst_name, "Some", dst_type,
        vec![FunctionBuilder::copy(owned_payload)],
    );
    // Tier 1c: Move freshly-built enum_init into merge slot; Copy
    // would shallow-alias and double-free now that Option is Resource.
    if ctx.type_registry.is_resource_type(dst_type) {
        builder.assign_mode(
            crate::ir::instructions::AssignMode::Move,
            Place::local(merge),
            FunctionBuilder::copy(some_result),
        );
    } else {
        builder.assign(Place::local(merge), FunctionBuilder::copy(some_result));
    }
    builder.jump(merge_bb);

    // None branch: construct Option[T]::None directly.
    builder.switch_to(none_bb);
    let none_result = builder.enum_init(&dst_name, "None", dst_type, vec![]);
    if ctx.type_registry.is_resource_type(dst_type) {
        builder.assign_mode(
            crate::ir::instructions::AssignMode::Move,
            Place::local(merge),
            FunctionBuilder::copy(none_result),
        );
    } else {
        builder.assign(Place::local(merge), FunctionBuilder::copy(none_result));
    }
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    Some(FunctionBuilder::copy(merge))
}

/// Infer operand type using both ctx locals and builder locals (for intermediates like tuples).
pub fn infer_operand_type_with_builder(
    ctx: &LoweringContext,
    operand: &Operand,
    builder: &FunctionBuilder,
) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // First check ctx locals (user-named variables)
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            // Fall back to builder locals (compiler temporaries)
            let idx = place.local.0 as usize;
            if idx < builder.locals.len() {
                return builder.locals[idx].type_id;
            }
            I64_TYPE
        }
        other => super::exprs::infer_operand_type(ctx, other),
    }
}

/// Check if a block always ends with a return statement.
/// Stub for `select` statement lowering in synchronous GIR mode.
/// The async backend handles select via its own codegen path; in the synchronous
/// GIR path we emit a no-op (the C backend for async will never see this path).
/// Lower a `select` statement using a spin-wait loop over channel arms.
///
/// ```text
/// loop_header → try_arm_0 → (ready) → body_arm_0 → exit_bb
///                         → (not ready) → try_arm_1 → (ready) → body_arm_1 → exit_bb
///                                       → (not ready) → loop_header
/// ```
/// Lower a `select` statement using a spin-wait loop over channel arms.
///
/// ```text
/// loop_header → try_arm_0 → (ready) → body_arm_0 → exit_bb
///                         → (not ready) → try_arm_1 → (ready) → body_arm_1 → exit_bb
///                                       → (not ready) → loop_header
/// ```
fn lower_select(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    arms: &[ast::SelectArm],
) {
    let num_arms = arms.len();
    if num_arms == 0 {
        return;
    }

    let loop_header = builder.new_block();
    let exit_bb = builder.new_block();

    // Allocate try and body blocks for each arm
    let try_blocks: Vec<_> = (0..num_arms).map(|_| builder.new_block()).collect();
    let body_blocks: Vec<_> = (0..num_arms).map(|_| builder.new_block()).collect();

    // Entry: jump to spin loop header
    builder.jump(loop_header);

    // Loop header: when all select arms are pending, help the executor make progress
    // by trying to run a queued task inline (work-stealing), then retry arms.
    // Without this, the tight spin-loop can starve producer fibers.
    builder.switch_to(loop_header);
    builder.call("__gorget_select_yield", vec![], I32_TYPE);
    builder.jump(try_blocks[0]);

    for (i, arm) in arms.iter().enumerate() {
        let next_block = if i + 1 < num_arms { try_blocks[i + 1] } else { loop_header };

        match &arm.op {
            SelectOp::Recv { channel, name, .. } => {
                // Try block: poll the channel; if ready, jump to body; else try next arm
                builder.switch_to(try_blocks[i]);

                // Lower the channel expression
                let ch_op = lower_expr(ctx, builder, channel);
                let ch_type = infer_operand_type_full(ctx, &ch_op, builder);

                // Get a mutable pointer to the channel
                let ch_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = ch_op {
                    let ptr_type = ctx.register_mut_ptr_type(ch_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, place.clone());
                    Operand::Copy(Place::local(ptr_local))
                } else {
                    let temp = builder.add_local(ch_type, None);
                    builder.assign(Place::local(temp), ch_op.clone());
                    let ptr_type = ctx.register_mut_ptr_type(ch_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, Place::local(temp));
                    Operand::Copy(Place::local(ptr_local))
                };

                // Determine element type from Channel__T name
                let ch_type_name = ctx.type_name_for_id(ch_type)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| "Channel__int64_t".to_string());
                let elem_suffix = ch_type_name.strip_prefix("Channel__").unwrap_or("int64_t");
                let elem_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);

                // Allocate output slot and get a mutable pointer to it
                let out_local = builder.add_local(elem_type, None);
                let out_ptr_type = ctx.register_mut_ptr_type(elem_type);
                let out_ptr_local = builder.add_local(out_ptr_type, None);
                builder.emit_borrow_mut(out_ptr_local, Place::local(out_local));
                let out_ptr_op = Operand::Copy(Place::local(out_ptr_local));

                // Call poll_recv(&ch, &out, NULL) → bool
                let poll_fn = format!("{ch_type_name}__poll_recv");
                let result_local = builder.call(
                    &poll_fn,
                    vec![ch_ptr, out_ptr_op, Operand::Constant(Constant::Null)],
                    BOOL_TYPE,
                );
                let result_op = Operand::Copy(Place::local(result_local));

                // Branch: if ready → body block, else → next arm (or loop header)
                builder.branch(result_op, body_blocks[i], next_block);

                // Body block: bind variable, lower body, jump to exit
                builder.switch_to(body_blocks[i]);
                let saved_select = ctx.save_locals(builder);
                let var_name = &name.node;
                ctx.register_local(var_name, out_local, elem_type);
                lower_block(ctx, builder, &arm.body);
                ctx.restore_locals(builder, saved_select);
                builder.jump(exit_bb);
            }
            SelectOp::Send { .. } => {
                // Send arms not yet implemented — treat as always-not-ready
                builder.switch_to(try_blocks[i]);
                builder.jump(next_block);
                builder.switch_to(body_blocks[i]);
                builder.jump(exit_bb);
            }
        }
    }

    builder.switch_to(exit_bb);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::TypeRegistry;
    use crate::parser::ast::SharedKind;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn make_test_ctx() -> LoweringContext<'static> {
        let analysis = Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = super::super::types::TypeMapper::new(&mut reg);
        LoweringContext::new(analysis, mapper, reg)
    }

    #[test]
    fn lower_var_decl_test() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::VarDecl {
            is_const: false,
            is_mutable: false,
            shared: SharedKind::None,
            type_: spanned(ast::Type::Primitive(ast::PrimitiveType::Int)),
            pattern: spanned(Pattern::Binding("x".into())),
            value: spanned(Expr::IntLiteral(42)),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have created a local and an assign
        assert!(ctx.lookup_local("x").is_some());
        assert!(!builder.blocks[0].instructions.is_empty());
        assert!(matches!(
            builder.blocks[0].instructions.last().unwrap(),
            Instruction::Assign { .. }
        ));
    }

    #[test]
    fn lower_if_else_test() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::If {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("x".into()))),
                op: ast::BinaryOp::Gt,
                right: Box::new(spanned(Expr::IntLiteral(0))),
            }),
            then_body: Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            },
            elif_branches: vec![],
            else_body: Some(Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            }),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have created multiple blocks (entry + merge + then + else)
        assert!(builder.blocks.len() >= 3);
        // Entry block should end with a Branch terminator
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
    }

    #[test]
    fn lower_while_loop_test() {
        let mut ctx = make_test_ctx();
        let i_id = LocalId(1);
        ctx.register_local("i", i_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("i"))]);

        let stmt = spanned(Stmt::While {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("i".into()))),
                op: ast::BinaryOp::Lt,
                right: Box::new(spanned(Expr::IntLiteral(10))),
            }),
            body: Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            },
            else_body: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have header, body, exit blocks
        assert!(builder.blocks.len() >= 4); // entry + header + body + exit
        // Entry block should jump to header
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Jump(_))
        ));
    }

    // ---- P3.0: Break, Continue, Loop tests ----

    #[test]
    fn lower_loop_basic() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break)],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have: entry(bb0) → body(bb1), exit(bb2), dead(bb3)
        assert!(builder.blocks.len() >= 3);
        // Entry block should jump to body
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Jump(BlockId(1)))
        ));
        // Body block back-edge (body_bb → body_bb) won't be present since break overrides it;
        // the break inside the body should jump to exit_bb
    }

    #[test]
    fn lower_break_in_loop() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // loop: break
        let stmt = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break)],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // The body block (bb1) should contain a Jump to the exit block (bb2)
        // break emits: jump to exit_bb, then creates dead block
        let body_block = &builder.blocks[1];
        if let Some(Terminator::Jump(target)) = &body_block.terminator {
            // Break should jump to exit_bb (bb2)
            assert_eq!(*target, BlockId(2), "break should jump to exit block");
        } else {
            panic!("Body block should have Jump terminator from break");
        }
    }

    #[test]
    fn lower_continue_in_while() {
        let mut ctx = make_test_ctx();
        let i_id = LocalId(1);
        ctx.register_local("i", i_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("i"))]);

        // while i < 10: continue
        let stmt = spanned(Stmt::While {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("i".into()))),
                op: ast::BinaryOp::Lt,
                right: Box::new(spanned(Expr::IntLiteral(10))),
            }),
            body: Block {
                stmts: vec![spanned(Stmt::Continue)],
                span: Span { start: 0, end: 0 },
            },
            else_body: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // bb0=entry, bb1=header, bb2=body, bb3=exit, bb4=dead(from continue)
        // Body block (bb2) should jump back to header (bb1)
        let body_block = &builder.blocks[2];
        if let Some(Terminator::Jump(target)) = &body_block.terminator {
            assert_eq!(*target, BlockId(1), "continue should jump to header block");
        } else {
            panic!("Body block should have Jump terminator from continue");
        }
    }

    #[test]
    fn lower_nested_break() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // loop:
        //   loop:
        //     break   <- should break inner loop only
        let inner_loop = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break)],
                span: Span { start: 0, end: 0 },
            },
        });
        let outer_loop = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![inner_loop],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &outer_loop);

        // After lowering, the inner break should target inner exit, not outer exit.
        // The structure is:
        // bb0: entry → jump to outer_body (bb1)
        // bb1: outer body → inner stuff starts here
        //   bb3: inner body → break jumps to inner exit (bb4)
        //   bb4: inner exit → falls through
        // bb2: outer exit
        // The key assertion: inner break doesn't reach outer exit

        // Verify we have enough blocks
        assert!(builder.blocks.len() >= 5, "Should have at least 5 blocks for nested loops");
    }

    // ---- P3.1: Match statement tests ----

    #[test]
    fn lower_match_literal() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![
                ast::MatchItem::Arm(ast::MatchArm {
                    pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                    guard: None,
                    body: spanned(Expr::IntLiteral(10)),
                    span: Span { start: 0, end: 0 },
                }),
                ast::MatchItem::Arm(ast::MatchArm {
                    pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                    guard: None,
                    body: spanned(Expr::IntLiteral(20)),
                    span: Span { start: 0, end: 0 },
                }),
            ],
            else_arm: Some(Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            }),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should create blocks for scrutinee + each arm + else + merge
        assert!(builder.blocks.len() >= 5);
        // Entry block: assign scrutinee, then branch on first pattern
        // There should be Cmp instructions for literal matching
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { .. }))
        });
        assert!(has_cmp, "Should have Cmp instructions for literal pattern matching");
    }

    #[test]
    fn lower_match_binding() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![ast::MatchItem::Arm(ast::MatchArm {
                pattern: spanned(Pattern::Binding("val".into())),
                guard: None,
                body: spanned(Expr::IntLiteral(42)),
                span: Span { start: 0, end: 0 },
            })],
            else_arm: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // After match, arm-scoped bindings should NOT leak into the outer scope
        assert!(ctx.lookup_local("val").is_none(), "Pattern binding 'val' should be scoped to the match arm");
    }

    #[test]
    fn lower_match_or_pattern() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        // match x: case 1 | 2 | 3: pass
        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![ast::MatchItem::Arm(ast::MatchArm {
                pattern: spanned(Pattern::Or(vec![
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(3))))),
                ])),
                guard: None,
                body: spanned(Expr::IntLiteral(0)),
                span: Span { start: 0, end: 0 },
            })],
            else_arm: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have multiple Cmp instructions (one per alternative) and Branch terminators
        let cmp_count: usize = builder.blocks.iter()
            .map(|bb| bb.instructions.iter().filter(|inst| matches!(inst, Instruction::Cmp { .. })).count())
            .sum();
        assert!(cmp_count >= 3, "Or pattern should have at least 3 Cmp instructions, got {cmp_count}");
    }

    // ---- P3.3: Error handling tests ----

    #[test]
    fn lower_assert_true() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Assert {
            condition: spanned(Expr::BoolLiteral(true)),
            message: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have Branch terminator in entry block
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
        // Fail block should have Unreachable terminator
        let has_unreachable = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Unreachable))
        });
        assert!(has_unreachable, "Assert fail path should have Unreachable terminator");
    }

    #[test]
    fn lower_throw_stmt() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Throw(spanned(Expr::StringLiteral(
            crate::lexer::token::StringLiteral {
                kind: crate::lexer::token::StringKind::Normal,
                segments: vec![crate::lexer::token::StringSegment::Literal("error".into())],
            },
            Vec::new(),
        ))));

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have a CallExtern to gorget_throw + Unreachable
        let has_throw = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_throw")
        });
        assert!(has_throw, "Should call gorget_throw");
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Unreachable)
        ));
    }
}
