mod assigns;
mod for_loops;
mod patterns;
use assigns::*;
use for_loops::*;
pub use patterns::*;

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, BinaryOp, Block, Expr, Pattern, SelectOp, Stmt};
use crate::span::Spanned;

use super::context::{LoweringContext, SharedLocalInfo, SharedLocalKind};
use super::drops::DropScopeKind;
use super::exprs::{lower_expr, infer_operand_type_full, maybe_auto_propagate};

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
pub fn lower_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    stmt: &Spanned<Stmt>,
) {
    builder.set_span(stmt.span);
    match &stmt.node {
        Stmt::VarDecl {
            type_,
            pattern,
            value,
            shared,
            ..
        } => {
            if *shared != ast::SharedKind::None {
                lower_shared_var_decl(ctx, builder, type_, pattern, value, shared);
            } else {
                lower_var_decl(ctx, builder, type_, pattern, value, stmt.span);
            }
        }

        Stmt::Assign { target, value } => lower_assign(ctx, builder, target, value),

        Stmt::CompoundAssign { target, op, value } => {
            lower_compound_assign(ctx, builder, target, *op, value)
        }

        Stmt::Return(expr) => lower_return(ctx, builder, expr.as_ref()),

        Stmt::Expr(expr) => {
            let val = lower_expr(ctx, builder, expr);
            // Auto-propagate: if the expression returns Result in a propagation
            // context, unwrap it so errors aren't silently swallowed.
            let _ = maybe_auto_propagate(ctx, builder, val);
        }

        Stmt::Pass => {
            builder.nop();
        }

        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => lower_if(ctx, builder, condition, then_body, elif_branches, else_body),

        Stmt::While {
            condition,
            body,
            else_body,
            ..
        } => lower_while(ctx, builder, condition, body, else_body.as_ref()),

        Stmt::For {
            pattern,
            iterable,
            body,
            else_body,
            ..
        } => lower_for(ctx, builder, pattern, iterable, body, else_body.as_ref()),

        Stmt::Loop { body } => lower_loop(ctx, builder, body),

        Stmt::Break(_) => lower_break(ctx, builder),

        Stmt::Continue => lower_continue(ctx, builder),

        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => lower_match_stmt(ctx, builder, scrutinee, arms, else_arm),

        Stmt::Throw(expr) => lower_throw(ctx, builder, expr),

        Stmt::Assert { condition, message } => lower_assert(ctx, builder, condition, message.as_ref()),

        Stmt::AssertReturn { condition, message } => {
            if !ctx.strip_asserts {
                ctx.func_state.postconditions.push((condition.clone(), message.clone()));
            }
        }

        Stmt::Snapshot { name, value } => {
            if ctx.snapshot_mode {
                lower_snapshot(ctx, builder, name, value);
            }
        }

        Stmt::With { bindings, body } => lower_with(ctx, builder, bindings, body),

        Stmt::Unsafe { body } => lower_block_scoped(ctx, builder, body),

        Stmt::NamedScope { body, .. } => lower_named_scope(ctx, builder, body),

        Stmt::Item(_) => { /* Nested items are hoisted — no-op in GIR */ }

        Stmt::Select { arms, else_arm: _ } => lower_select(ctx, builder, arms),

        // meta if/for/match/while should have been evaluated and removed before GIR lowering.
        // If they appear here it means they were in a non-generic context (a semantic
        // error should have been emitted) — emit nothing.
        Stmt::MetaIf { .. } | Stmt::MetaFor { .. } | Stmt::MetaMatch { .. }
        | Stmt::MetaWhile { .. } | Stmt::MetaConst { .. } | Stmt::MetaLog { .. } => {}

        Stmt::OnError { body } => {
            // Register the cleanup block — it will be emitted on error paths
            ctx.func_state.on_error_blocks.push(body.clone());
        }
    }
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
                if let ast::Type::Named { name: _, ref generic_args } = type_.node {
                    if !generic_args.is_empty() {
                        ctx.type_mapper.map_ast_type_mut(&type_.node, &mut ctx.type_registry)
                    } else {
                        gir_type
                    }
                } else {
                    gir_type
                }
            } else {
                gir_type
            };
            // Box[Callable[...]] variables pre-register with a "Box__Callable__unknown" type from the
            // generic collector. We need to reinfer from the actual RHS to get the real closure type.
            let gir_type_is_box_callable = ctx.type_name_for_id(gir_type)
                .map(|n| n.starts_with("Box__Callable__") || n.starts_with("Box__MutCallable__") || n.starts_with("Box__ConsumeCallable__"))
                .unwrap_or(false);
            let local_id = builder.add_local(gir_type, Some(name));
            ctx.register_local(name, local_id, gir_type);
            // Track callable return types for locals declared with a Callable
            // or bare-function type. Enables `cb(...)` call-site return-type
            // inference when `F cb = self.f` binds a closure field and F is a
            // method-level-generic param that resolves to a Function type.
            if let Some(ret_type) = callable_local_return_type(ctx, &type_.node) {
                ctx.set_callable_return_type(local_id, ret_type);
            }
            // P2.6: Register Move-type locals for drop at scope exit
            ctx.drops.register_local(local_id, gir_type, &ctx.type_registry);
            // Force-register Option/Result with resource payloads (needs_drop
            // returns false because the type upgrade scan hasn't run yet).
            if !ctx.drops.is_registered(local_id) {
                if let Some(crate::ir::types::GirType::Named(tn)) = ctx.type_registry.get(gir_type).cloned() {
                    if tn.starts_with("Option__") || tn.starts_with("Result__") {
                        if let Some(td) = ctx.type_registry.get_type_def(&tn) {
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
            // Auto-propagate: if operand is Result-typed but the declared type is not Result,
            // unwrap it (propagating errors) so the binding gets the Ok value.
            // NOTE: must run before restoring expected_type so the guard sees gir_type.
            let mut operand = maybe_auto_propagate(ctx, builder, operand);
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
            // Only propagate Ptr for operands that are actually borrowed
            // (from cow_ptr_params or ref_locals), not from owned function returns.
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
                if inferred != gir_type
                    && !matches!(ctx.type_registry.get(inferred), Some(GirType::Ptr(_)))
                    && ctx.type_registry.is_resource_type(inferred)
                    && !ctx.type_registry.is_resource_type(gir_type)
                {
                    builder.locals[local_id.0 as usize].type_id = inferred;
                    ctx.register_local(name, local_id, inferred);
                    ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                }
                if let Some(GirType::Ptr(_inner)) = ctx.type_registry.get(inferred).cloned() {
                    if !matches!(ctx.type_registry.get(gir_type), Some(GirType::Ptr(_))) {
                        // Check: is the source a Ptr borrow safe to propagate?
                        // - BareParam: borrows from caller, lifetime = function scope.
                        // - CowBorrow with provenance: borrows from a collection via
                        //   .get().unwrap(). Tracked as CollectionRef so
                        //   cow_before_mutation materializes when the collection is mutated.
                        let (source_is_bare_param, source_is_cow_borrow) = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                            if p.projections.is_empty() {
                                let cow = ctx.is_cow_borrow(p.local) && ctx.cow_borrow_source(p.local).is_some();
                                (ctx.is_bare_param(p.local), cow)
                            } else {
                                (false, false)
                            }
                        } else { (false, false) };

                        let in_loop = ctx.current_loop().is_some();
                        // Allow borrow propagation in loops when the variable is
                        // not reassigned on any forward path from this statement.
                        // Flow-sensitive: only blocks propagation when the name is
                        // reassigned AFTER this VarDecl, not globally in the function.
                        let safe_in_loop = !ctx.is_cow_unsafe_at(name, stmt_span.start);
                        if source_is_bare_param && (!in_loop || safe_in_loop) {
                            // Propagate bare param borrow
                            builder.locals[local_id.0 as usize].type_id = inferred;
                            ctx.register_local(name, local_id, inferred);
                            ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                            ctx.set_bare_param(local_id);
                        } else if source_is_cow_borrow && (!in_loop || safe_in_loop)
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
                            ctx.set_collection_ref(local_id, collection);
                            ctx.drops.unregister(local_id);
                        } else if let Some(clone_fn) = ctx.clone_fn_for_ptr(_inner) {
                            // Owned Ptr source (function return, etc.) → auto-clone
                            ctx.warn_implicit_clone(value.span, _inner, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);
                            let cloned = builder.call(&clone_fn, vec![operand.clone()], _inner);
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
                            ctx.set_ref(local_id);
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
                                if let Some(collection) = ctx.collection_ref_source(p.local) {
                                    ctx.set_collection_ref(local_id, collection);
                                    ctx.drops.unregister(local_id);
                                    true
                                } else { false }
                            } else { false }
                        } else { false };
                        if !propagated {
                            ctx.set_ref(local_id);
                        }
                    }
                }
            }
            // Determine assignment mode and emit with explicit ownership semantics.
            use crate::ir::instructions::AssignMode;
            let actual_var_type = builder.local_type(local_id);
            let mut assign_mode = AssignMode::Copy; // default for trivial types

            // Phase D4 typed signals (for incremental decision-tree migration —
            // see TODO entry "Phase D4 — lower_var_decl decision tree refactor").
            // Branches below progressively read these instead of the legacy
            // sidecar predicates (`named_local`, `cow_unsafe_at`,
            // `drops.is_registered`, `needs_drop`) until every arm is expressed
            // as `(target_resource, source_live, source_own)`.
            let target_resource = ctx.type_registry.is_resource_type(actual_var_type);
            let source_live = ctx.source_live_past(&operand, stmt_span, builder);
            let source_own = ctx.source_ownership(&operand, builder);

            if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                if place.projections.is_empty() && place.local != local_id {
                    let rhs_type = builder.local_type(place.local);

                    // GorgetString → str view: if the source is a NAMED local, unregister
                    // it so the view can borrow its data safely. Unnamed temps (from
                    // function calls) should NOT be unregistered — they may hold
                    // owned data that needs freeing.
                    //
                    // Phase D4 probe (2026-05-04): removing the
                    // is_named_local guard regressed 10 fixtures across
                    // the leak_*, stress_alloc_strings/closures, and
                    // string_builder/string_builder_loop families. An
                    // unnamed temp from a function call returning
                    // GorgetString owns its data; this branch
                    // unregisters its drop and treats it as a borrow
                    // source — leaks the heap allocation. Genuine
                    // gating (Outcome 2). Retiring requires moving the
                    // borrow-source bookkeeping to consult typed
                    // ownership instead — a function-call temp is
                    // Owned, only named-and-still-live locals are
                    // legitimate borrow sources.
                    if rhs_type == ctx.type_mapper.owned_string_type
                        && actual_var_type == ctx.type_mapper.owned_string_type
                        && ctx.is_named_local(place.local)
                    {
                        ctx.drops.unregister(place.local);
                        // Track that the source has been borrowed-from.
                        // If we later `return source`, the clone is needed because
                        // the target shares the source's heap data.
                        //
                        // Cannot use the typed `set_view_of(local_id, place.local)`
                        // channel here: ViewOf flushes to OwnershipState::MaybeBorrowed
                        // which the LIR backend treats as Ptr ABI (SlotLoad → void*).
                        // The LHS here is a value-type GorgetString slot (32-byte
                        // shallow copy that aliases source's heap data) — NOT a Ptr.
                        // Tagging it ViewOf produces a slot/local-type mismatch in
                        // C codegen ("incompatible types when assigning to type
                        // 'void *' from type 'Str'"). ViewOf models cap=0 string
                        // views (true byte-pointer slices into another buffer),
                        // which are a different structural shape than this
                        // value-aliasing case.
                        ctx.mark_string_borrow_source(place.local);
                        assign_mode = AssignMode::Borrow;
                    }
                    // Named non-resource local with clone_fn (e.g., Str → GorgetString conversion):
                    // still clone, not CoW alias (different types, not an alias relationship).
                    //
                    // Phase D4 probe (2026-05-04): removing the
                    // is_named_local guard regressed 16 fixtures —
                    // unnamed temps of types like Result[Config,
                    // String] hit `clone_fn_for_ptr.is_some()` true
                    // (the type-upgrade scan sets a clone fn on
                    // recursively-droppable enums) and were routed
                    // through this cross-type-clone path when they
                    // should fall through to F's Move path. Unlike
                    // the view_returning_temps case, this guard is
                    // not hiding a downstream consumer bug — it's a
                    // genuine "this branch only applies to named
                    // sources" gate. Keeping the guard until either
                    // (a) is_resource_type is widened to include
                    // enum-with-resource-payload (so the !is_resource
                    // check filters those), or (b) the cross-type
                    // axis is split into a separate explicit arm.
                    else if ctx.is_named_local(place.local)
                        && !ctx.type_registry.is_resource_type(rhs_type)
                        && ctx.clone_fn_for_ptr(rhs_type).is_some()
                    {
                        ctx.warn_implicit_clone(value.span, rhs_type, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);
                        let clone_fn = ctx.clone_fn_for_ptr(rhs_type).expect("BUG: clone_fn_for_ptr returned None after is_some check");
                        let ptr_type = ctx.register_ptr_type(rhs_type);
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow(ptr_local, place.clone());
                        // Use the TARGET type (actual_var_type) as clone return type,
                        // not the source type. The clone function may return a
                        // different (owned) type than the source (view) type.
                        let clone_ret_type = actual_var_type;
                        let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], clone_ret_type);
                        ctx.set_owned(cloned); // clone result owns its data
                        operand = FunctionBuilder::copy(cloned);
                        assign_mode = AssignMode::Move;
                    }
                    // Named resource variable → CoW alias OR clone (if unsafe for CoW).
                    // Flow-sensitive: only skip aliasing if the name is reassigned
                    // on a forward path from THIS statement (not globally).
                    //
                    // Phase D4 probe (2026-05-04): removing the
                    // is_named_local guard regressed 50+ fixtures
                    // across arena/async/borrow/box/bytes/closure/
                    // collection/cow/csv/dataframe/derive/dict/...
                    // families before the sweep was halted (only
                    // reached letter "d"). An unnamed temp from a
                    // function call returning a resource type owns its
                    // data; this branch creates a Ptr alias into it
                    // (`builder.emit_borrow` + `cow_register_alias` +
                    // `set_ref` + Ptr-typing the destination), but the
                    // temp dies at end-of-stmt — the alias is then a
                    // dangling pointer. SIGSEGV is the typical
                    // signature. Genuine gating (Outcome 2). Retiring
                    // requires teaching CoW alias creation to consume
                    // (move-from) the source temp instead of borrowing
                    // it — equivalent to switching the unnamed-temp
                    // case to Branch F's Move path while still
                    // recognising the alias relationship for downstream
                    // CoW materialisation.
                    else if ctx.is_named_local(place.local)
                        && ctx.type_registry.is_resource_type(rhs_type)
                        && !ctx.is_cow_unsafe_at(name, stmt_span.start)
                        && !builder.local_name(place.local)
                            .map_or(false, |n| ctx.is_cow_unsafe_at(n, stmt_span.start))
                    {
                        // Create Ptr(T) alias instead of cloning
                        let ptr_type = ctx.register_ptr_type(rhs_type);
                        builder.locals[local_id.0 as usize].type_id = ptr_type;
                        ctx.set_ref(local_id);
                        builder.emit_borrow(local_id, place.clone());
                        ctx.cow_register_alias(local_id, place.local);
                        // Ptr doesn't own data — don't register for drop.
                        // Unregister if already registered (from line 226).
                        ctx.drops.unregister(local_id);
                        // Update local type in context lookup
                        if let Some(ref hint) = builder.local_name(local_id).map(|s| s.to_string()) {
                            let name = hint.clone();
                            ctx.register_local(&name, local_id, ptr_type);
                        }
                        // Skip normal assign_mode logic — borrow already emitted
                        assign_mode = AssignMode::Borrow;
                    }
                    // Named resource local unsafe for CoW (reassigned/moved) → clone.
                    //
                    // Phase D4 probe (2026-05-04): removing the
                    // is_named_local guard was bundled with Branch C's
                    // probe; that combined sweep regressed 50+
                    // fixtures and was halted at letter "d". Most of
                    // those failures attribute to Branch C (which sees
                    // the same source first when the destination is
                    // CoW-safe — Branch D only fires when the
                    // destination is CoW-unsafe). Branch D's
                    // independent contribution is a redundant clone
                    // of an already-Move-eligible unnamed temp:
                    // structurally a leak, not a use-after-free, since
                    // the Move-zero in F won't fire (D ran first) and
                    // the source temp's drop registration is left in
                    // place. Retiring requires the same shape as C —
                    // route unnamed-temp sources to Branch F's Move
                    // path. Keeping the guard until the unified typed
                    // arm lands.
                    else if ctx.is_named_local(place.local)
                        && ctx.type_registry.is_resource_type(rhs_type)
                    {
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(rhs_type) {
                            ctx.warn_implicit_clone(value.span, rhs_type, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);
                            let ptr_type = ctx.register_ptr_type(rhs_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], rhs_type);
                            ctx.set_owned(cloned); // clone result owns its data
                            operand = FunctionBuilder::copy(cloned);
                            assign_mode = AssignMode::Move;
                        }
                    }
                    // CoW chain materialization: source is a view of some
                    // upstream container (LocalOwnership::View — set on
                    // every result of a view-returning string method:
                    // trim / slice / strip / substring / str / as_str)
                    // feeding a value-typed String destination. The view's
                    // bytes are a cap=0 borrow; without a clone here, x's
                    // view goes dangling once the chain's upstream
                    // mutates. Emit gorget_string_clone_to_owned. Mirrors
                    // the auto-clone path at line ~463 (Ptr-typed source)
                    // for the value-typed-but-View case.
                    //
                    // Phase D4 (2026-05-04): typed read of LocalOwnership
                    // replaces the legacy view_returning_temps sidecar —
                    // sidecar deleted after cow_materialize_view's
                    // shallow-copy bug was fixed (Move mode at the
                    // clone-to-owned assign).
                    else if matches!(source_own, Some(crate::ir::LocalOwnership::View { .. }))
                        && rhs_type == ctx.type_mapper.owned_string_type
                        && actual_var_type == ctx.type_mapper.owned_string_type
                    {
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(rhs_type) {
                            ctx.warn_implicit_clone(value.span, rhs_type, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);
                            let ptr_type = ctx.register_ptr_type(rhs_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr_local, place.clone());
                            let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], rhs_type);
                            ctx.set_owned(cloned);
                            operand = FunctionBuilder::copy(cloned);
                            assign_mode = AssignMode::Move;
                        }
                    }
                    // Drop-registered temp OR unregistered droppable temp → move.
                    // Temps (not named vars) that need drop should be moved to transfer
                    // ownership, preventing shallow-copy double-free on scope exit.
                    //
                    // Phase D4 migration in progress: the legacy predicate
                    // `drops.is_registered(place.local)` doubles as a
                    // liveness proxy for "this local will get scope-exit
                    // dropped." The typed-match arm `(needs_drop_target,
                    // source_dead, source_owned) => Move` is the principled
                    // shape — `needs_drop(actual_var_type)` covers
                    // Option/Result wrapper types where `is_resource_type`
                    // returns false but the variant payload still requires
                    // ownership transfer. Both predicates retained today;
                    // the typed predicate strictly extends the legacy one
                    // for the cases the legacy predicate misses.
                    else if ctx.drops.is_registered(place.local)
                        || (!ctx.is_named_local(place.local) && ctx.type_registry.needs_drop(rhs_type))
                        || (matches!(source_own, Some(crate::ir::LocalOwnership::Owned))
                            && !source_live
                            && ctx.type_registry.needs_drop(actual_var_type))
                    {
                        assign_mode = AssignMode::Move;
                    }
                    // Safety net: if still Copy and the TARGET is a resource
                    // type, switch to Move. Catches edge cases not covered
                    // by the specific guards above (e.g., named resource
                    // structs where clone_fn lookup failed in branch D).
                    //
                    // Phase D4: typed `target_resource` replaces the legacy
                    // `is_resource_type(rhs_type)` source-keyed read. The
                    // correct axis is the destination's type — Move applies
                    // to where the value lands, not where it came from.
                    // Cross-type resource→non-resource assigns are caught
                    // by earlier branches (B handles Str→GorgetString); the
                    // remaining cases reach G with rhs_type==actual_var_type.
                    if assign_mode == AssignMode::Copy && target_resource {
                        assign_mode = AssignMode::Move;
                    }
                }
            }


            builder.assign_mode(assign_mode, Place::local(local_id), operand.clone());

            // Propagate ownership: if RHS local owned its data (call result),
            // the new local also owns the data (via move or clone).
            if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                if ctx.is_owned_local(p.local) {
                    ctx.set_owned(local_id);
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
                    if p.projections.is_empty() && ctx.is_owned_local(p.local) {
                        AssignMode::Move
                    } else {
                        AssignMode::Copy
                    }
                } else {
                    AssignMode::Copy
                }
            };
            builder.assign_mode(tuple_assign_mode, Place::local(tuple_local), operand);

            // Extract each field and bind it to the corresponding pattern variable
            for (i, part) in parts.iter().enumerate() {
                let field_type = super::exprs::resolve_tuple_field_type(ctx, tuple_type, i);
                let field_local = builder.field_load(Place::local(tuple_local), i as u32, field_type);

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
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(facade_local), init_val);
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
            let tmp = builder.add_local(inner_type, None);
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(tmp), val_operand);
            let wrapped = builder.call(&new_fn, vec![FunctionBuilder::copy(tmp)], shared_type);

            let hidden_local = builder.add_local(shared_type, None);
            ctx.drops.register_local(hidden_local, shared_type, &ctx.type_registry);
            builder.assign_mode(crate::ir::instructions::AssignMode::Move, Place::local(hidden_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type: shared_type, kind: SharedLocalKind::SharedArc, ast_shared: *shared });

            let init_val = super::exprs::emit_shared_get(ctx, builder, hidden_local, inner_type);
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(facade_local), init_val);
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

            // Ensure ReadGuard and WriteGuard types exist
            let read_guard_mangled = format!("ReadGuard__{inner_c}");
            if ctx.type_mapper.lookup_named(&read_guard_mangled).is_none() {
                let tid = ctx.type_registry.insert(GirType::Named(read_guard_mangled.clone()));
                ctx.type_mapper.register_named(read_guard_mangled, tid);
            }
            let write_guard_mangled = format!("WriteGuard__{inner_c}");
            if ctx.type_mapper.lookup_named(&write_guard_mangled).is_none() {
                let tid = ctx.type_registry.insert(GirType::Named(write_guard_mangled.clone()));
                ctx.type_mapper.register_named(write_guard_mangled, tid);
            }

            let new_fn = format!("{mangled}__new");
            let tmp = builder.add_local(inner_type, None);
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(tmp), val_operand);
            let wrapped = builder.call(&new_fn, vec![FunctionBuilder::copy(tmp)], rwlock_type);

            let rwlock_local = builder.add_local(rwlock_type, None);
            ctx.drops.register_local(rwlock_local, rwlock_type, &ctx.type_registry);
            builder.assign_mode(crate::ir::instructions::AssignMode::Move, Place::local(rwlock_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local: rwlock_local, inner_type, wrapper_type: rwlock_type, kind: SharedLocalKind::RwLock, ast_shared: *shared });

            let init_val = super::exprs::emit_rwlock_read_get(ctx, builder, rwlock_local, inner_type);
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(facade_local), init_val);
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
            let tmp = builder.add_local(inner_type, None);
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(tmp), val_operand);
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
            builder.assign_mode(resource_assign_mode(ctx, inner_type), Place::local(facade_local), init_val);
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
        // Check if the return expression is already an explicit Ok/Error variant
        // (used in throws functions). If so, skip the automatic Result wrapping —
        // the expression itself already produces a Result.
        let is_explicit_result_variant = matches!(&expr.node,
            Expr::Call { callee, .. } if matches!(&callee.node,
                Expr::Identifier(name) if name == "Ok" || name == "Error" || name == "Some" || name == "None"
            )
        );
        // Set expected type from function return type so variant constructors resolve correctly
        let prev_expected = ctx.func_state.expected_type;
        let ret_type = builder.locals[0].type_id;
        ctx.func_state.expected_type = Some(ret_type);

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
        // Auto-propagate: if returning a Result value from a throws function,
        // unwrap so the Ok-wrapping below works on the inner value.
        // NOTE: must run before restoring expected_type so the guard sees ret_type.
        let mut operand = if !is_explicit_result_variant {
            maybe_auto_propagate(ctx, builder, operand)
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
                // Expression already produced a Result — assign directly, no wrapping
                builder.assign(Place::local(LocalId(0)), operand);
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
                                ctx.record_param_cloned(builder, place.local);
                                ctx.warn_implicit_clone(expr.span, inner, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                                let cloned = builder.call(
                                    &clone_fn,
                                    vec![operand.clone()],
                                    inner,
                                );
                                operand = FunctionBuilder::copy(cloned);
                                returned_local = Some(cloned);
                            }
                        } else if ctx.type_registry.needs_drop(src_type) {
                            // Owned resource — clone if borrowed/shared
                            let can_skip_clone = ctx.is_fresh_string(place.local)
                                || (ctx.is_owned_local(place.local)
                                    && !ctx.has_string_borrowers(place.local));
                            if !can_skip_clone {
                                if let Some(clone_fn) = ctx.clone_fn_for_ptr(src_type) {
                                    ctx.warn_implicit_clone(expr.span, src_type, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                                    let cloned = builder.call(
                                        &clone_fn,
                                        vec![operand.clone()],
                                        src_type,
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
                builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(ok_dst));
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
                        let can_skip_clone = ctx.is_fresh_string(place.local)
                            || (ctx.is_owned_local(place.local)
                                && ctx.is_named_local(place.local)
                                && !ctx.has_string_borrowers(place.local));
                        if rhs_type == ctx.type_mapper.owned_string_type
                            && !can_skip_clone
                        {
                            ctx.warn_implicit_clone(expr.span, rhs_type, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                            let clone_fn = ctx.clone_fn_for_ptr(rhs_type)
                                .unwrap_or_else(|| "gorget_string_from_str".to_string());
                            let clone_result = builder.call(
                                &clone_fn,
                                vec![operand.clone()],
                                ret_type,
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
                // Ptr(T) → T auto-clone/deref for return values: if the operand
                // is Ptr(T) but the return type is T, resolve the borrow:
                //   - resource T → clone to owned T
                //   - non-resource T (primitives, value structs) → deref the pointer
                if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    if p.projections.is_empty() {
                        let src_idx = p.local.0 as usize;
                        if src_idx < builder.locals.len() {
                            let src_type = builder.locals[src_idx].type_id;
                            if let Some(GirType::Ptr(inner)) = ctx.type_registry.get(src_type).cloned() {
                                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                                    if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                                        ctx.record_param_cloned(builder, p.local);
                                        ctx.warn_implicit_clone(expr.span, inner, crate::ir::ImplicitCloneReason::ReturnFromBorrow);
                                        let cloned = builder.call(&clone_fn, vec![operand.clone()], inner);
                                        operand = FunctionBuilder::copy(cloned);
                                    } else if !ctx.type_registry.is_resource_type(inner) {
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
                                        ctx.set_ref(LocalId(0));
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
                        builder.assign(Place::local(LocalId(0)), converted);
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
                        // Phase D migration: union the typed walk over
                        // `LocalOwnership::Borrowed { TupleElement { tuple, .. } }`
                        // with the legacy sidecar. The typed side covers anonymous
                        // temps tagged at TupleInit; the sidecar still covers named
                        // locals that retained their pre-existing ownership state.
                        let mut elem_locals: Vec<LocalId> =
                            ctx.tuple_element_sources(place.local);
                        if let Some(side) = ctx.func_state.tuple_element_locals.get(&place.local) {
                            for &el in side {
                                if !elem_locals.contains(&el) {
                                    elem_locals.push(el);
                                }
                            }
                        }
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
            let is_array_type = ret_name.starts_with("Vector__")
                || ret_name.starts_with("Deque__")
                || ret_name == "GorgetArray";
            let is_dict_type = ret_name.starts_with("Dict__")
                || ret_name.starts_with("HashMap__")
                || ret_name == "GorgetMap";
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
        // P2.6: Emit cleanup drops for all scopes being exited
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
        builder.ret(FunctionBuilder::const_unit());
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

    // Then branch
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

    // Elif branches
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

        current_else_bb = next_else_bb;
    }

    // Else branch
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
    }

    builder.switch_to(merge_bb);
}

/// Lower a while loop.
fn lower_while(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
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
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
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

                // Call gorget_assert_fail_values(op, left_str, right_str)
                builder.call_extern_void(
                    "gorget_assert_fail_values",
                    vec![
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
        // Custom message provided — lower it and pass to gorget_panic.
        let msg_op = lower_expr(ctx, builder, msg);
        builder.call_extern("gorget_panic", vec![msg_op], UNIT_TYPE);
        builder.unreachable();
        builder.switch_to(pass_bb);
        return;
    }
    // No custom message: generate a static message based on the expression shape.
    let panic_msg = generate_assert_static_msg(condition);
    builder.call_extern(
        "gorget_panic",
        vec![Operand::Constant(Constant::Str(panic_msg))],
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

    // Named types: dispatch to Type__eq / Type__compare if available
    if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(lhs_type).cloned() {
        if matches!(op, BinaryOp::Eq | BinaryOp::Neq) {
            let eq_method = format!("{type_name}__eq");
            if ctx.fn_sigs.contains_key(&eq_method) {
                let self_ptr_type = ctx.register_ptr_type(lhs_type);
                let self_ptr = builder.add_local(self_ptr_type, None);
                builder.emit_borrow(self_ptr, Place::local(lhs_local));
                let result = builder.call(
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
                let cmp_result = builder.call(
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
        // Re-lower the inner expression to get the scrutinee local
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

    ctx.warn_implicit_clone(span, inner_type, crate::ir::ImplicitCloneReason::VarDeclFromBorrow);

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
        builder.call(fn_name, vec![FunctionBuilder::copy(ptr_local)], inner_type)
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
    builder.assign(Place::local(merge), FunctionBuilder::copy(some_result));
    builder.jump(merge_bb);

    // None branch: construct Option[T]::None directly.
    builder.switch_to(none_bb);
    let none_result = builder.enum_init(&dst_name, "None", dst_type, vec![]);
    builder.assign(Place::local(merge), FunctionBuilder::copy(none_result));
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
                stmts: vec![spanned(Stmt::Break(None))],
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
                stmts: vec![spanned(Stmt::Break(None))],
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
                stmts: vec![spanned(Stmt::Break(None))],
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
