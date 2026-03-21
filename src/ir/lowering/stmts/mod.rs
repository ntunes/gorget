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
fn maybe_emit_field_move_zero(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    operand: &Operand,
) {
    let source_local = match operand {
        Operand::Copy(place) | Operand::Move(place) => place.local,
        _ => return,
    };
    ctx.emit_field_origin_zero(builder, source_local);
}

/// Lower a block of statements.
pub fn lower_block(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &Block,
) {
    for stmt in &block.stmts {
        lower_stmt(ctx, builder, stmt);
        // Clear unconsumed field_load_origins. Only VarDecl/Assign consume them
        // via maybe_emit_field_move_zero. Field loads used as method receivers
        // (e.g., h.data.push(x)) create dead temps that should not trigger zeroing.
        ctx.field_load_origins.clear();
    }
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
                lower_var_decl(ctx, builder, type_, pattern, value);
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
                ctx.postconditions.push((condition.clone(), message.clone()));
            }
        }

        Stmt::Snapshot { name, value } => {
            if ctx.snapshot_mode {
                lower_snapshot(ctx, builder, name, value);
            }
        }

        Stmt::With { bindings, body } => lower_with(ctx, builder, bindings, body),

        Stmt::Unsafe { body } => lower_block(ctx, builder, body),

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
            ctx.on_error_blocks.push(body.clone());
        }
    }
}

/// Emit accumulated `on error` cleanup blocks in LIFO order.
/// Called on error exit paths (throw, try-error, rethrow-error) BEFORE drop elaboration.
pub fn emit_on_error_cleanups(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if ctx.on_error_blocks.is_empty() {
        return;
    }
    // Clone the blocks to avoid borrow conflicts (lowering each block borrows ctx mutably)
    let blocks: Vec<_> = ctx.on_error_blocks.iter().rev().cloned().collect();
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
            // After str→StringType unification, provenance may have downgraded this
            // binding's semantic type_id to view. Adjust the GIR type accordingly.
            let gir_type = ctx.provenance_adjusted_string_type(gir_type, name, pattern.span);
            // Box[Callable[...]] variables pre-register with a "Box__Callable__unknown" type from the
            // generic collector. We need to reinfer from the actual RHS to get the real closure type.
            let gir_type_is_box_callable = ctx.type_name_for_id(gir_type)
                .map(|n| n.starts_with("Box__Callable__") || n.starts_with("Box__MutCallable__") || n.starts_with("Box__ConsumeCallable__"))
                .unwrap_or(false);
            let local_id = builder.add_local(gir_type, Some(name));
            ctx.register_local(name, local_id, gir_type);
            // P2.6: Register Move-type locals for drop at scope exit
            ctx.drops.register_local(local_id, gir_type, &ctx.type_registry);
            // Set expected type hint so enum variant constructors (Some, None, Ok, Error)
            // can pick the correctly-monomorphized type
            let prev_expected = ctx.expected_type;
            ctx.expected_type = Some(gir_type);
            let operand = lower_expr(ctx, builder, value);
            // Auto-propagate: if operand is Result-typed but the declared type is not Result,
            // unwrap it (propagating errors) so the binding gets the Ok value.
            // NOTE: must run before restoring expected_type so the guard sees gir_type.
            let operand = maybe_auto_propagate(ctx, builder, operand);
            ctx.expected_type = prev_expected;
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
                    // Also update the drop elaborator with the correct type — the initial
                    // registration used `gir_type` which may have been I64_TYPE (no-drop),
                    // but the real type (e.g., Wrapper, Container) does need dropping.
                    ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                }
            }
            // T & reference propagation: if the RHS operand is Ptr(T) (from a
            // collection borrowing read) but the local has a non-Ptr type (from
            // explicit annotation), override the local's type to Ptr(T).
            // This ensures: no drop emitted, field/method access uses pointee_type().
            if !needs_reinfer {
                let inferred = infer_operand_type_with_builder(ctx, &operand, builder);
                if let Some(GirType::Ptr(_)) = ctx.type_registry.get(inferred) {
                    if !matches!(ctx.type_registry.get(gir_type), Some(GirType::Ptr(_))) {
                        builder.locals[local_id.0 as usize].type_id = inferred;
                        ctx.register_local(name, local_id, inferred);
                        ctx.drops.update_or_register_type(local_id, inferred, &ctx.type_registry);
                        ctx.collection_ref_locals.insert(local_id);
                    }
                }
            }
            // Also mark auto-reinferred locals that got Ptr type
            if needs_reinfer {
                let actual = builder.locals[local_id.0 as usize].type_id;
                if let Some(GirType::Ptr(_)) = ctx.type_registry.get(actual) {
                    if !matches!(ctx.type_registry.get(gir_type), Some(GirType::Ptr(_))) {
                        ctx.collection_ref_locals.insert(local_id);
                    }
                }
            }
            builder.assign(Place::local(local_id), operand.clone());
            // Track GorgetString temps assigned to Str or String variables.
            // Use the ACTUAL variable type (after reinference) not the original gir_type.
            let actual_var_type = builder.locals[local_id.0 as usize].type_id;
            if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                if place.projections.is_empty() {
                    let rhs_type = builder.locals[place.local.0 as usize].type_id;
                    if rhs_type == ctx.type_mapper.owned_string_type
                        && place.local != local_id
                    {
                        if actual_var_type == ctx.type_mapper.owned_string_type {
                            // String variable consuming a GorgetString temp: mark temp as moved
                            // to prevent double-free (VarDecl already registers the variable for drop)
                            builder.move_zero(Place::local(place.local));
                            ctx.drops.mark_moved(place.local);
                        } else if actual_var_type == ctx.type_mapper.str_type {
                            // Str variable taking a view of a GorgetString temp: completely
                            // unregister the temp from drop tracking. The str view may escape
                            // the scope, and freeing the GorgetString would invalidate the view.
                            // The GorgetString will leak — same as pre-drop-registration behavior.
                            ctx.drops.unregister(place.local);
                        }
                    }
                }
            }
            // Emit MoveZero for resource-type field loads to prevent double-free
            maybe_emit_field_move_zero(ctx, builder, &operand);
        }

        Pattern::Tuple(parts) => {
            // Lower the RHS expression first — it should produce a tuple (struct) value
            let operand = lower_expr(ctx, builder, value);
            let tuple_type = infer_operand_type_with_builder(ctx, &operand, builder);

            // Store the tuple in a temp local
            let tuple_local = builder.add_local(tuple_type, None);
            builder.assign(Place::local(tuple_local), operand);

            // Extract each field and bind it to the corresponding pattern variable
            for (i, part) in parts.iter().enumerate() {
                let field_type = super::exprs::resolve_tuple_field_type(ctx, tuple_type, i);
                let field_local = builder.field_load(Place::local(tuple_local), i as u32, field_type);

                if let Pattern::Binding(name) = &part.node {
                    // Apply provenance adjustment: if the type checker downgraded
                    // this binding from String to str (view), use the view type.
                    let field_type = ctx.provenance_adjusted_string_type(
                        field_type, name, part.span,
                    );
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

    let name = match &pattern.node {
        Pattern::Binding(n) => n,
        _ => {
            lower_var_decl(ctx, builder, type_, pattern, value);
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

    let prev_expected = ctx.expected_type;
    ctx.expected_type = Some(inner_type);
    let val_operand = lower_expr(ctx, builder, value);
    ctx.expected_type = prev_expected;

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
                    },
                };
                ctx.type_registry.add_type_def(type_def);
                tid
            };

            let new_fn = format!("{atomic_name}__new");
            let wrapped = builder.call(&new_fn, vec![val_operand], wrapper_type);

            let hidden_local = builder.add_local(wrapper_type, None);
            ctx.drops.register_local(hidden_local, wrapper_type, &ctx.type_registry);
            builder.assign(Place::local(hidden_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type, kind: SharedLocalKind::Atomic, ast_shared: *shared });

            // Initialize facade with atomic load
            let init_val = super::exprs::emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name);
            builder.assign(Place::local(facade_local), init_val);
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
            builder.assign(Place::local(tmp), val_operand);
            let wrapped = builder.call(&new_fn, vec![FunctionBuilder::copy(tmp)], shared_type);

            let hidden_local = builder.add_local(shared_type, None);
            ctx.drops.register_local(hidden_local, shared_type, &ctx.type_registry);
            builder.assign(Place::local(hidden_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type: shared_type, kind: SharedLocalKind::SharedArc, ast_shared: *shared });

            let init_val = super::exprs::emit_shared_get(ctx, builder, hidden_local, inner_type);
            builder.assign(Place::local(facade_local), init_val);
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
            builder.assign(Place::local(tmp), val_operand);
            let wrapped = builder.call(&new_fn, vec![FunctionBuilder::copy(tmp)], rwlock_type);

            let rwlock_local = builder.add_local(rwlock_type, None);
            ctx.drops.register_local(rwlock_local, rwlock_type, &ctx.type_registry);
            builder.assign(Place::local(rwlock_local), FunctionBuilder::copy(wrapped));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local: rwlock_local, inner_type, wrapper_type: rwlock_type, kind: SharedLocalKind::RwLock, ast_shared: *shared });

            let init_val = super::exprs::emit_rwlock_read_get(ctx, builder, rwlock_local, inner_type);
            builder.assign(Place::local(facade_local), init_val);
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
            builder.assign(Place::local(tmp), val_operand);
            let mutex_val = builder.call(&mutex_new_fn, vec![FunctionBuilder::copy(tmp)], mutex_type);

            let shared_new_fn = format!("{shared_mutex_mangled}__new");
            let shared_val = builder.call(&shared_new_fn, vec![FunctionBuilder::copy(mutex_val)], shared_mutex_type);

            let hidden_local = builder.add_local(shared_mutex_type, None);
            ctx.drops.register_local(hidden_local, shared_mutex_type, &ctx.type_registry);
            builder.assign(Place::local(hidden_local), FunctionBuilder::copy(shared_val));

            let facade_local = builder.add_local(inner_type, Some(name));
            ctx.register_local(name, facade_local, inner_type);
            ctx.drops.register_local(facade_local, inner_type, &ctx.type_registry);

            ctx.shared.locals.insert(facade_local, SharedLocalInfo { hidden_local, inner_type, wrapper_type: shared_mutex_type, kind: SharedLocalKind::Mutex, ast_shared: *shared });

            // Init facade: Shared.get() → Mutex, then lock → get → release
            let init_val = super::exprs::emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type);
            builder.assign(Place::local(facade_local), init_val);
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
        let prev_expected = ctx.expected_type;
        let ret_type = builder.locals[0].type_id;
        ctx.expected_type = Some(ret_type);
        let operand = lower_expr(ctx, builder, expr);
        // Auto-propagate: if returning a Result value from a throws function,
        // unwrap so the Ok-wrapping below works on the inner value.
        // NOTE: must run before restoring expected_type so the guard sees ret_type.
        let operand = if !is_explicit_result_variant {
            maybe_auto_propagate(ctx, builder, operand)
        } else {
            operand
        };
        ctx.expected_type = prev_expected;
        // Identify the local being returned (to exclude from drops — it's being moved out)
        let returned_local = match &operand {
            Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                Some(place.local)
            }
            _ => None,
        };
        if let Some(result_type) = ctx.current_throws_result_type {
            if is_explicit_result_variant {
                // Expression already produced a Result — assign directly, no wrapping
                builder.assign(Place::local(LocalId(0)), operand);
            } else {
                // Wrap value in Result.Ok — the operand's local is consumed (moved into Result)
                let ok_dst = {
                    let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
                    builder.enum_init(type_name, "Ok", result_type, vec![operand])
                };
                builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(ok_dst));
                // Zero out the original local (its value is now owned by the Result)
                if let Some(local) = returned_local {
                    builder.move_zero(Place::local(local));
                    ctx.drops.mark_moved(local);
                }
            }
        } else {
            let ret_type = builder.locals[0].type_id;
            // If returning a str_type (Copy/view) value through an owned_string_type
            // return slot, clone it so the caller gets an independent allocation.
            // Without this, the caller frees a pointer still owned by the source
            // (e.g., an enum field loaded via match destructuring).
            let mut did_clone_return = false;
            if ret_type == ctx.type_mapper.owned_string_type {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty() {
                        let rhs_type = builder.locals[place.local.0 as usize].type_id;
                        if rhs_type == ctx.type_mapper.str_type {
                            let clone_result = builder.call(
                                "gorget_string_from_str",
                                vec![operand.clone()],
                                ret_type,
                            );
                            builder.assign(
                                Place::local(LocalId(0)),
                                FunctionBuilder::copy(clone_result),
                            );
                            did_clone_return = true;
                        }
                    }
                }
            }
            if !did_clone_return {
                // T & reference propagation for return values: if the operand
                // is Ptr(T) (from a collection borrowing read) but the return
                // local _0 has a non-Ptr type, override _0's type to Ptr(T).
                if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                    if p.projections.is_empty() {
                        let src_idx = p.local.0 as usize;
                        if src_idx < builder.locals.len() {
                            let src_type = builder.locals[src_idx].type_id;
                            if let Some(GirType::Ptr(_)) = ctx.type_registry.get(src_type) {
                                if !matches!(ctx.type_registry.get(ret_type), Some(GirType::Ptr(_))) {
                                    builder.locals[0].type_id = src_type;
                                    builder.return_type = src_type;
                                    ctx.collection_ref_locals.insert(LocalId(0));
                                }
                            }
                        }
                    }
                }
                builder.assign(Place::local(LocalId(0)), operand.clone());
            }
            // If the return local is str-typed and the operand is a GorgetString temp,
            // unregister the temp to prevent use-after-free: the str view in the return
            // local may be accessed after the temp's scope exit frees it.
            if ret_type == ctx.type_mapper.str_type {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty() {
                        let rhs_type = builder.locals[place.local.0 as usize].type_id;
                        if rhs_type == ctx.type_mapper.owned_string_type {
                            ctx.drops.unregister(place.local);
                        }
                    }
                }
            }
            // If the return local is owned_string_type and the operand is also
            // owned_string_type, the source has been moved into the return slot.
            // Mark it as moved so drop elaboration doesn't free it (double-free).
            if ret_type == ctx.type_mapper.owned_string_type {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                    if place.projections.is_empty() {
                        let rhs_type = builder.locals[place.local.0 as usize].type_id;
                        if rhs_type == ctx.type_mapper.owned_string_type {
                            builder.move_zero(Place::local(place.local));
                            ctx.drops.mark_moved(place.local);
                        }
                    }
                }
            }
        }
        // For move-overridden generic params: zero the source through the pointer
        // to transfer ownership to the caller and prevent double-free.
        if !ctx.move_override_params.is_empty() {
            if let Expr::Identifier(name) = &expr.node {
                if ctx.move_override_params.contains(name.as_str()) {
                    if let Some((local_id, _)) = ctx.lookup_local(name.as_str()) {
                        builder.move_zero(crate::ir::instructions::Place {
                            local: local_id,
                            projections: vec![crate::ir::instructions::Projection::Deref],
                        });
                    }
                }
            }
        }
        // Postcondition checks: `assert return <expr>` — check before returning
        emit_postcondition_checks(ctx, builder);

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
    if ctx.postconditions.is_empty() {
        return;
    }
    // Register __return__ → _0 so the postcondition expression can reference the return value
    let ret_type = builder.locals[0].type_id;
    ctx.register_local("__return__", LocalId(0), ret_type);

    let postconditions = ctx.postconditions.clone();
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

    // Lower the condition
    let cond = lower_expr(ctx, builder, condition);

    let then_bb = builder.new_block();
    let first_else_bb = if !elif_branches.is_empty() || else_body.is_some() {
        builder.new_block()
    } else {
        merge_bb
    };

    builder.branch(cond, then_bb, first_else_bb);

    // Then branch
    builder.switch_to(then_bb);
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
        ctx.drops.push_scope(DropScopeKind::Block);
        emit_is_bindings(ctx, builder, elif_cond);
        lower_block(ctx, builder, elif_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }

        current_else_bb = next_else_bb;
    }

    // Else branch
    if let Some(else_body) = else_body {
        builder.switch_to(current_else_bb);
        ctx.drops.push_scope(DropScopeKind::Block);
        lower_block(ctx, builder, else_body);
        if builder.is_terminated() {
            ctx.drops.pop_scope_no_emit();
        } else {
            ctx.drops.pop_scope(builder, &ctx.type_registry);
            builder.jump(merge_bb);
        }
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
        let break_bb = builder.new_block();
        let else_bb = builder.new_block();
        (break_bb, Some(else_bb))
    } else {
        (exit_bb, None)
    };

    // Jump from current block to header
    builder.jump(header_bb);

    // Header: evaluate condition, branch
    builder.switch_to(header_bb);
    let cond = lower_expr(ctx, builder, condition);
    let natural_exit = if else_arm.is_some() {
        else_exit_bb.unwrap()
    } else {
        exit_bb
    };
    builder.branch(cond, body_bb, natural_exit);

    // Body: execute, jump back to header (wrapped in Loop scope for drop cleanup)
    builder.switch_to(body_bb);
    emit_is_bindings(ctx, builder, condition);
    ctx.push_loop(header_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(header_bb);

    // Else block: executed when loop completes naturally (no break)
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb.unwrap());
        lower_block(ctx, builder, else_body);
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
    ctx.push_loop(body_bb, exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
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
    if let Some(result_type) = ctx.current_throws_result_type {
        // Wrap error in Result.Error and return
        let err_dst = {
                let type_name = ctx.type_registry.type_name(result_type).unwrap_or_else(|| "Result".to_string());
                builder.enum_init(type_name, "Error", result_type, vec![val])
            };
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_dst));
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

                // Store in locals so values survive across basic blocks
                let lhs_local = builder.add_local(lhs_type, None);
                builder.assign(Place::local(lhs_local), lhs_op);
                let rhs_local = builder.add_local(rhs_type, None);
                builder.assign(Place::local(rhs_local), rhs_op);

                // Emit type-appropriate comparison
                let cond_local = emit_assert_comparison(
                    ctx, builder, lhs_local, lhs_type, rhs_local, rhs_type, *op,
                );

                let pass_bb = builder.new_block();
                let fail_bb = builder.new_block();
                builder.branch(Operand::Copy(Place::local(cond_local)), pass_bb, fail_bb);
                builder.switch_to(fail_bb);

                // Format both values for the diagnostic message
                let (lhs_fmt, lhs_arg) = assert_format_info_rich(ctx, builder, lhs_local, lhs_type);
                let (rhs_fmt, rhs_arg) = assert_format_info_rich(ctx, builder, rhs_local, rhs_type);
                builder.inline_c(format!(
                    "gorget_panic(gorget_format(\"assertion failed: left {op_str} right\\n  left:  {lhs_fmt}\\n  right: {rhs_fmt}\", {lhs_arg}, {rhs_arg}));"
                ));
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
    let is_string = lhs_type == ctx.type_mapper.str_type
        || lhs_type == ctx.type_mapper.owned_string_type;

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

/// Return (printf_format_spec, c_args_expression) for an assert diagnostic value.
/// Handles all types: primitives, strings, and named types with display methods.
fn assert_format_info_rich(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    local: LocalId,
    type_id: TypeId,
) -> (String, String) {
    let c_expr = format!("_{}", local.0);

    // Primitive types: direct printf formatting
    if is_primitive_type_for_assert(type_id) {
        if type_id == F64_TYPE || type_id == F32_TYPE {
            return ("%g".to_string(), format!("(double){c_expr}"));
        } else if type_id == BOOL_TYPE {
            return ("%s".to_string(), format!("({c_expr}) ? \"true\" : \"false\""));
        } else {
            return ("%lld".to_string(), format!("(long long)({c_expr})"));
        }
    }

    // String types: show the string value via %.*s
    if type_id == ctx.type_mapper.str_type || type_id == ctx.type_mapper.owned_string_type {
        return ("%.*s".to_string(), format!("(int){c_expr}.len, {c_expr}.data"));
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
            // Call Type__display(&val) → Str, then format via %.*s
            let self_type = ctx.register_ptr_type(type_id);
            let self_ptr = builder.add_local(self_type, None);
            builder.emit_borrow(self_ptr, Place::local(local));
            let str_type = ctx.type_mapper.str_type;
            let result = builder.call(
                effective_method,
                vec![FunctionBuilder::copy(self_ptr)],
                str_type,
            );
            let result_c = format!("_{}", result.0);
            return ("%.*s".to_string(), format!("(int){result_c}.len, {result_c}.data"));
        }
        // Named type without display — show type name
        return ("%s".to_string(), format!("\"<{type_name}>\""));
    }

    // Opaque fallback
    ("%s".to_string(), "\"<opaque>\"".to_string())
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

/// Return true if type_id is a primitive numeric/bool type suitable for assert rich diagnostics.
/// Strings and named types need special comparison logic and are excluded.
fn is_primitive_type_for_assert(type_id: TypeId) -> bool {
    matches!(type_id,
        I64_TYPE | I32_TYPE | I16_TYPE | I8_TYPE |
        U64_TYPE | U32_TYPE | U16_TYPE | U8_TYPE |
        F64_TYPE | F32_TYPE | BOOL_TYPE
    )
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
    let val_local = builder.add_local(val_type, None);
    builder.assign(Place::local(val_local), val_op);

    let point_name = name.node.replace('\\', "\\\\").replace('"', "\\\"");
    let c_expr = format!("_{}", val_local.0);

    // Emit the appropriate direct-write call based on the value's type
    if val_type == I64_TYPE || val_type == I32_TYPE || val_type == I16_TYPE || val_type == I8_TYPE
        || val_type == U64_TYPE || val_type == U32_TYPE || val_type == U16_TYPE || val_type == U8_TYPE
    {
        builder.inline_c(format!(
            "__gorget_snapshot_write_int(__gorget_current_test, \"{point_name}\", (long long)({c_expr}));"
        ));
    } else if val_type == F64_TYPE || val_type == F32_TYPE {
        builder.inline_c(format!(
            "__gorget_snapshot_write_float(__gorget_current_test, \"{point_name}\", (double){c_expr});"
        ));
    } else if val_type == BOOL_TYPE {
        builder.inline_c(format!(
            "__gorget_snapshot_write_bool(__gorget_current_test, \"{point_name}\", {c_expr});"
        ));
    } else if val_type == ctx.type_mapper.str_type || val_type == ctx.type_mapper.owned_string_type {
        builder.inline_c(format!(
            "__gorget_snapshot_write_str(__gorget_current_test, \"{point_name}\", {c_expr});"
        ));
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
            let str_type = ctx.type_mapper.str_type;
            let result = builder.call(
                effective_method,
                vec![FunctionBuilder::copy(self_ptr)],
                str_type,
            );
            builder.inline_c(format!(
                "__gorget_snapshot_write_str(__gorget_current_test, \"{point_name}\", _{});",
                result.0
            ));
        } else {
            // No display method — write null
            builder.inline_c(format!(
                "__gorget_snapshot_write_null(__gorget_current_test, \"{point_name}\");"
            ));
        }
    } else {
        // Unknown type — write null
        builder.inline_c(format!(
            "__gorget_snapshot_write_null(__gorget_current_test, \"{point_name}\");"
        ));
    }
}

fn lower_named_scope(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    body: &Block,
) {
    ctx.drops.push_scope(DropScopeKind::Block);
    lower_block(ctx, builder, body);
    if builder.is_terminated() {
        ctx.drops.pop_scope_no_emit();
    } else {
        ctx.drops.pop_scope(builder, &ctx.type_registry);
    }
}

/// Lower a `with bindings: body` statement.
fn lower_with(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    bindings: &[ast::WithBinding],
    body: &Block,
) {
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
        builder.assign(Place::local(local_id), val);

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
    let prev_refresh_len = ctx.with_shared_refresh.len();
    ctx.with_shared_refresh.extend(shared_refresh_entries);

    lower_block(ctx, builder, body);

    // Pop shared-refresh entries
    ctx.with_shared_refresh.truncate(prev_refresh_len);

    // Drop all non-allocator locals FIRST (while the allocator is still alive),
    // then pop + destroy allocators. This avoids use-after-free when collections
    // allocated within the `with` scope try to dealloc via the active allocator.
    ctx.drops.pop_scope(builder, &ctx.type_registry);

    for &local_id in allocator_locals.iter().rev() {
        builder.pop_allocator();
        let type_id = builder.locals[local_id.0 as usize].type_id;
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
                let var_name = &name.node;
                ctx.register_local(var_name, out_local, elem_type);
                lower_block(ctx, builder, &arm.body);
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

        // The binding pattern should register "val" as a local alias
        assert!(ctx.lookup_local("val").is_some(), "Pattern binding should register 'val'");
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
