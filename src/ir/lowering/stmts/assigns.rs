//! Assignment lowering: simple assign, field assign, index assign, compound assign.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr};
use crate::span::Spanned;

use super::super::context::{LoweringContext, SharedLocalKind};
use super::super::exprs::{
    lower_expr, infer_operand_type_full, maybe_auto_propagate,
    guard_inner_suffix, emit_guard_get_ptr,
    emit_shared_mutex_lock_get, emit_shared_mutex_lock_set,
    atomic_type_name_for, emit_atomic_load, emit_atomic_store,
    emit_rwlock_write_get, emit_rwlock_write_set, emit_rwlock_write_finish,
    try_resolve_field_place, extract_field_path_string,
    infer_collection_element_type,
};

/// Local helper mirroring `exprs::type_reg::is_resource_type_local` —
/// the crate-private visibility on the original prevented direct use here.
fn is_resource_type_local(
    local: LocalId,
    builder: &FunctionBuilder,
    registry: &TypeRegistry,
) -> bool {
    registry.is_resource_type(builder.local_type(local))
}

/// Lower an assignment.
pub(super) fn lower_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    match &target.node {
        Expr::Identifier(name) => {
            if let Some((local_id, _type_id)) = ctx.lookup_local(name) {
                // Shared variable: dispatch based on wrapper kind
                if let Some(info) = ctx.shared.locals.get(&local_id) {
                    let (hidden_local, inner_type, kind) = (info.hidden_local, info.inner_type, info.kind);
                    match kind {
                        SharedLocalKind::Mutex => {
                            let operand = lower_expr(ctx, builder, value);
                            let inner_c = ctx.c_type_name_for_id(inner_type);
                            let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                            emit_shared_mutex_lock_set(ctx, builder, hidden_local, mutex_type, inner_type, operand);
                            return;
                        }
                        SharedLocalKind::Atomic => {
                            let operand = lower_expr(ctx, builder, value);
                            let atomic_name = atomic_type_name_for(inner_type);
                            emit_atomic_store(ctx, builder, hidden_local, operand, &atomic_name);
                            return;
                        }
                        SharedLocalKind::RwLock => {
                            let operand = lower_expr(ctx, builder, value);
                            emit_rwlock_write_set(ctx, builder, hidden_local, inner_type, operand);
                            return;
                        }
                        SharedLocalKind::SharedArc => {
                            // ArcOnly: assignment shouldn't happen (CFA upgrades to ArcMutex)
                        }
                    }
                }
                let type_id = _type_id;
                // CoW: if this local is a source with aliases, sever them first.
                // Aliases keep the old value; this local gets the new value.
                if ctx.cow_has_aliases(local_id) {
                    ctx.cow_sever_all_aliases_from(builder, local_id, target.span);
                }
                // CoW: if this local is an alias, just remove from alias maps.
                // The reassignment naturally replaces the binding value.
                if ctx.cow_is_alias(local_id) {
                    ctx.unset_ownership(local_id);
                }
                // CoW: if this collection has element refs, clone them out.
                // Reassignment replaces the buffer; outstanding refs would dangle.
                if ctx.cow_has_collection_refs(local_id) {
                    ctx.cow_before_mutation(builder, local_id, target.span);
                }
                // CoW clone-on-mutate: if LHS is an immutable borrow (Ptr, not MutPtr),
                // materialize to owned before computing RHS. This is the CoW clone.
                // MutPtr (& params) and unique-borrow locals pass through without cloning.
                {
                    use crate::ir::types::GirType;
                    let is_mut = ctx.is_param_borrow_unique(local_id)
                        || matches!(ctx.type_registry.get(type_id), Some(GirType::MutPtr(_)));
                    if !is_mut {
                        if let Some(GirType::Ptr(inner)) = ctx.type_registry.get(type_id).cloned() {
                            if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                                ctx.warn_implicit_clone(value.span, inner, crate::ir::ImplicitCloneReason::NamedToNamed);
                                let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(local_id)], inner);
                                // Phase C: cloned is a fresh owned local, dead at this single
                                // use — Move transfers ownership into local_id.
                                builder.assign_mode(
                                    crate::ir::instructions::AssignMode::Move,
                                    Place::local(local_id),
                                    FunctionBuilder::copy(cloned),
                                );
                                builder.locals[local_id.0 as usize].type_id = inner;
                                ctx.register_local(name, local_id, inner);
                                ctx.drops.update_or_register_type(local_id, inner, &ctx.type_registry);
                                ctx.set_owned(local_id);
                            }
                        }
                    }
                }
                let type_id = builder.local_type(local_id); // re-read after possible CoW upgrade
                // Check if old value needs dropping before reassignment.
                // All droppable types — including collections — drop the old value
                // before the new one is assigned. The RHS is fully computed (and
                // cloned for named locals) before the Drop fires, so even
                // self-referential RHS like `v = v.slice(...)` is safe.
                let needs_drop = ctx.type_registry.needs_drop(type_id);
                // Compute new value (now operating on owned copy if CoW upgraded)
                let prev_expected = ctx.func_state.expected_type;
                ctx.func_state.expected_type = Some(type_id);
                let operand = lower_expr(ctx, builder, value);
                // Auto-propagate: if RHS is Result-typed but target is not, unwrap
                let mut operand = maybe_auto_propagate(ctx, builder, operand);
                ctx.func_state.expected_type = prev_expected;
                // Auto-deref Ref[T] → T at reassignment when target is bare T.
                // `int ai = 1; ai = a.get(da).unwrap()` — declared int but RHS
                // is Ptr(i64) post-1.7b. Mirrors VarDecl's deref branch so the
                // stored value is the int, not the pointer bits.
                {
                    use crate::ir::types::GirType;
                    let rhs_type = if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                        if p.projections.is_empty() && (p.local.0 as usize) < builder.locals.len() {
                            Some(builder.local_type(p.local))
                        } else { None }
                    } else { None };
                    if let Some(rhs) = rhs_type {
                        if let Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) = ctx.type_registry.get(rhs).cloned() {
                            if !matches!(ctx.type_registry.get(type_id), Some(GirType::Ptr(_) | GirType::MutPtr(_)))
                                && !ctx.type_registry.is_resource_type(inner)
                                && ctx.clone_fn_for_ptr(inner).is_none()
                            {
                                if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                                    let tmp = builder.add_local(inner, None);
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
                        }
                    }
                }
                // String self-referential reassignment fix: if the RHS might be a
                // view into the old LHS (e.g., `s = s.trim()` or `s = s[0..1]`),
                // clone-to-owned before dropping. gorget_string_clone_to_owned
                // allocates a new buffer even for owned strings, so skip when the
                // RHS is a fresh allocation (user function call result — independent
                // buffer, cannot be a view into the old LHS).
                if needs_drop && ctx.type_mapper.is_string_type(type_id) {
                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        let src_type = builder.local_type(place.local);
                        if ctx.type_mapper.is_string_type(src_type)
                            && !ctx.is_fresh_string(place.local)
                        {
                            let owned = ctx.type_mapper.owned_string_type;
                            let cloned = builder.call(
                                "gorget_string_clone_to_owned",
                                vec![FunctionBuilder::copy(place.local)],
                                owned,
                            );
                            ctx.drops.register_local(cloned, owned, &ctx.type_registry);
                            ctx.set_owned(cloned);
                            operand = FunctionBuilder::copy(cloned);
                        }
                    }
                }
                // P2.6: Drop old value AFTER computing new value, BEFORE assigning
                if needs_drop {
                    // For enums with resource payloads but no DropStrategy,
                    // emit an explicit call to {Name}__drop instead of a GIR Drop
                    // instruction (which would lower to Nop for DropStrategy::None).
                    let used_explicit_drop = if let Some(crate::ir::types::GirType::Named(tn)) =
                        ctx.type_registry.get(type_id).cloned()
                    {
                        let tn = tn.clone();
                        if let Some(td) = ctx.type_registry.get_type_def(&tn) {
                            if td.metadata.drop_strategy == DropStrategy::None {
                                if let crate::ir::types::TypeDefKind::Enum(_) = &td.kind {
                                    let drop_fn = format!("{tn}__drop");
                                    let ptr_type = ctx.register_ptr_type(type_id);
                                    let ptr_local = builder.add_local(ptr_type, None);
                                    builder.emit_borrow_mut(ptr_local, Place::local(local_id));
                                    builder.call_void(&drop_fn, vec![FunctionBuilder::copy(ptr_local)]);
                                    true
                                } else { false }
                            } else { false }
                        } else { false }
                    } else { false };
                    if !used_explicit_drop {
                        // Use DropIfAlive if the variable may have been moved
                        // (e.g., pushed into a collection earlier).  MoveSlot
                        // marks the slot as dead; DropIfAlive lets the drop
                        // elaboration pass skip the drop when the slot is
                        // provably uninitialized.
                        if ctx.drops.is_moved(local_id) {
                            builder.drop_if_alive(Place::local(local_id));
                        } else {
                            builder.drop(Place::local(local_id));
                        }
                    }
                }
                // If this is a mutable capture pointer, write through the pointer
                if ctx.is_param_borrow_unique(local_id) {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    builder.assign(deref_place, operand.clone());
                    // Unregister RHS temp — the deref store took ownership.
                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        if place.projections.is_empty() && !ctx.is_named_local(place.local) {
                            ctx.drops.unregister(place.local);
                        }
                    }
                } else {
                    // Determine assignment mode (same decision tree as VarDecl).
                    use crate::ir::instructions::AssignMode;
                    let mut assign_mode = AssignMode::Copy;

                    // Phase D4 typed signals — same lift as `lower_var_decl`.
                    // Branches below progressively migrate from sidecar predicates
                    // (`is_named_local`, `drops.is_registered`, `needs_drop`) to
                    // `(target_resource, source_live, source_own)`.
                    let _target_resource = ctx.type_registry.is_resource_type(type_id);
                    let _source_live = ctx.source_live_past(&operand, value.span, builder);
                    let _source_own = ctx.source_ownership(&operand, builder);

                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        if place.projections.is_empty() && place.local != local_id {
                            let rhs_type = builder.local_type(place.local);

                            if rhs_type == ctx.type_mapper.owned_string_type
                                && type_id == ctx.type_mapper.owned_string_type
                            {
                                ctx.drops.unregister(place.local);
                                assign_mode = AssignMode::Move;
                            } else if ctx.is_named_local(place.local) {
                                // Last-use optimization: if the RHS is dead after this
                                // assignment, move instead of cloning. The source is
                                // MoveZeroed after the assignment (see line ~207).
                                let rhs_name = builder.local_name(place.local).map(|s| s.to_string());
                                let is_last_use = rhs_name.as_ref()
                                    .map(|n| ctx.is_last_use_at(n, value.span))
                                    .unwrap_or(false);
                                if is_last_use && ctx.type_registry.is_resource_type(rhs_type) {
                                    // Move: unregister source from drops, assign, MoveZero
                                    ctx.drops.unregister(place.local);
                                    assign_mode = AssignMode::Move;
                                } else if let Some(clone_fn) = ctx.clone_fn_for_ptr(rhs_type) {
                                    ctx.warn_implicit_clone(value.span, rhs_type, crate::ir::ImplicitCloneReason::NamedToNamed);
                                    let ptr_type = ctx.register_ptr_type(rhs_type);
                                    let ptr_local = builder.add_local(ptr_type, None);
                                    builder.emit_borrow(ptr_local, place.clone());
                                    let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], rhs_type);
                                    ctx.set_owned(cloned);
                                    operand = FunctionBuilder::copy(cloned);
                                    assign_mode = AssignMode::Move;
                                }
                            } else if ctx.drops.is_registered(place.local) {
                                assign_mode = AssignMode::Move;
                            }
                            // Branch D (rhs==string && type==string) was a
                            // duplicate of branch A's predicate — unreachable
                            // today; deleted in the D4 cleanup. The
                            // string-to-string Move case is handled by branch A
                            // (with the additional drops.unregister side effect).
                            // Safety net: no Copy for resource RHS types.
                            //
                            // Phase D4 probe (2026-05-04): switching to
                            // target-keyed `target_resource` regressed 7
                            // fixtures (dataframe_*, self_host_bootstrap*).
                            // Unlike `lower_var_decl`'s branch G, this site
                            // sees cross-type cases where rhs is non-resource
                            // and target is resource — Move'ing the source
                            // would zero a non-resource (e.g. primitive)
                            // local that's still alive elsewhere. The legacy
                            // RHS-keyed read is the genuinely correct axis
                            // here. Kept as-is; the mirror with branch G is
                            // not appropriate at this site.
                            if assign_mode == AssignMode::Copy && ctx.type_registry.is_resource_type(rhs_type) {
                                assign_mode = AssignMode::Move;
                            }
                        }
                    }

                    builder.assign_mode(assign_mode, Place::local(local_id), operand.clone());

                    // Mark source as moved + emit GIR-level move-zero.
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
                    // Propagate ownership: if RHS local owned its data (call result),
                    // the target local now owns the data after move/clone.
                    if let Operand::Copy(ref p) | Operand::Move(ref p) = operand {
                        if ctx.is_owned_local(p.local) {
                            ctx.set_owned(local_id);
                        }
                    }
                }
            } else if ctx.global_names.contains(name.as_str()) {
                // Module-level static variable — emit GlobalAssign
                let operand = lower_expr(ctx, builder, value);
                builder.global_assign(name.clone(), operand);
            }
        }
        Expr::FieldAccess { object, field } => {
            lower_field_assign(ctx, builder, object, &field.node, value);
        }
        Expr::Index { object, index } => {
            lower_index_assign(ctx, builder, object, index, value);
        }
        _ => {
            // Other target types not yet supported
        }
    }
}

/// Emit a drop for a field place if its type is droppable (has a non-None DropStrategy).
fn emit_field_drop_if_needed(
    ctx: &LoweringContext,
    builder: &mut FunctionBuilder,
    place: &Place,
    field_type: TypeId,
) {
    use crate::ir::types::GirType;
    // Check if the field type has a drop strategy
    let needs_drop = if let Some(GirType::Named(type_name)) = ctx.type_registry.get(field_type) {
        if ctx.type_registry.is_collection_type_name(type_name) {
            true
        } else if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
            type_def.metadata.drop_strategy != DropStrategy::None
        } else {
            type_name == "GorgetString"
        }
    } else {
        false
    };
    if needs_drop {
        builder.drop(place.clone());
    }
}

/// Lower a field assignment: `obj.field = value`
pub(super) fn lower_field_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
    value: &Spanned<Expr>,
) {
    use crate::ir::types::TypeDefKind;

    // CoW: field write mutates the object. Sever aliases before proceeding.
    if let Expr::Identifier(obj_name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(obj_name) {
            ctx.cow_before_mutation(builder, local_id, object.span);
        }
    } else if let Some(field_path) = extract_field_path_string(&object.node) {
        ctx.cow_before_field_mutation(builder, &field_path, object.span);
    }

    // Try to resolve the full field projection chain without materializing
    // intermediate struct values. This handles nested field writes like
    // `gs.current_weapon.ammo = x` by building Place { local: gs, projections: [Deref, Field(5), Field(2)] }
    // instead of copying the intermediate struct to a temp.
    if let Some((target_place, field_type)) = try_resolve_field_place(ctx, builder, object, field_name) {
        let mut rhs = lower_expr(ctx, builder, value);
        clone_ptr_rhs_if_needed(ctx, builder, &mut rhs, value.span);
        emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
        return;
    }

    // Fallback: lower_expr on object (may copy intermediate structs)
    // For unique-borrow params (& or !), use the pointer local directly
    // instead of lower_expr which would copy the deref'd value to a temp
    let obj = if let Expr::Identifier(name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.is_param_borrow_unique(local_id) {
                // Return the raw pointer local (not deref'd)
                Operand::Copy(Place::local(local_id))
            } else {
                lower_expr(ctx, builder, object)
            }
        } else {
            lower_expr(ctx, builder, object)
        }
    } else {
        lower_expr(ctx, builder, object)
    };
    let mut rhs = lower_expr(ctx, builder, value);
    clone_ptr_rhs_if_needed(ctx, builder, &mut rhs, value.span);

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;

            // Guard[T] auto-deref for writes: guard.field = val → (*get_ptr(&guard)).field = val
            if let Some(type_name) = ctx.type_name_for_id(local_type_id) {
                let type_name = type_name.to_string();
                if let Some((inner_suffix, is_read_only)) = guard_inner_suffix(&type_name) {
                    if is_read_only {
                        // ReadGuard: writes are forbidden — skip (type checker should catch in future)
                        return;
                    }
                    let (inner_ptr_local, inner_type) = emit_guard_get_ptr(
                        ctx, builder, place, local_type_id, &type_name, inner_suffix,
                    );
                    let deref_place = Place {
                        local: inner_ptr_local,
                        projections: vec![Projection::Deref],
                    };
                    if let Some(inner_type_name) = ctx.type_name_for_id(inner_type) {
                        let inner_type_name = inner_type_name.to_string();
                        if let Some((field_idx, field_type)) = ctx.lookup_field(&inner_type_name, field_name) {
                            let mut target_place = deref_place;
                            target_place.projections.push(Projection::Field(field_idx));
                            emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
                            return;
                        }
                        let inner_field: Option<(u32, TypeId)> = ctx.type_registry.get_type_def(&inner_type_name)
                            .and_then(|td| {
                                if let TypeDefKind::Struct(ref s) = td.kind {
                                    s.fields.iter().enumerate().find(|(_, f)| f.name == field_name)
                                        .map(|(i, f)| (i as u32, f.type_id))
                                } else {
                                    None
                                }
                            });
                        if let Some((field_idx, field_type)) = inner_field {
                            let mut target_place = deref_place;
                            target_place.projections.push(Projection::Field(field_idx));
                            emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
                            return;
                        }
                    }
                }
            }

            // If the local is a pointer, dereference to get the struct type
            let (effective_type_id, base_place) =
                if let Some(pointee) = ctx.pointee_type(local_type_id) {
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (local_type_id, place.clone())
                };

            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                let type_name = type_name.to_string();
                if let Some((field_idx, field_type)) = ctx.lookup_field(&type_name, field_name) {
                    let mut target_place = base_place;
                    target_place.projections.push(Projection::Field(field_idx));
                    emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
                    return;
                }
                // Fallback: look up from TypeDef
                let field_match: Option<(u32, TypeId)> = ctx.type_registry.get_type_def(&type_name)
                    .and_then(|td| {
                        if let TypeDefKind::Struct(ref s) = td.kind {
                            s.fields.iter().enumerate().find(|(_, f)| f.name == field_name)
                                .map(|(i, f)| (i as u32, f.type_id))
                        } else {
                            None
                        }
                    });
                if let Some((field_idx, field_type)) = field_match {
                    let mut target_place = base_place;
                    target_place.projections.push(Projection::Field(field_idx));
                    emit_field_store_with_cleanup(ctx, builder, &target_place, field_type, &rhs);
                    return;
                }
            }
        }
    }
}

/// Clone a Ptr-typed RHS (borrowed reference) to produce an independently owned copy.
/// Without this, storing a borrowed param into a struct field creates aliased ownership —
/// when the caller drops its copy, the field becomes a dangling pointer.
// Thin wrapper over `ensure_owned_at_boundary` for field-store paths that want
// to clone a Ptr(T) RHS to an owned T before writing into a struct field.
// Kept as a standalone helper only so the call sites can pass `&mut rhs`
// instead of passing/returning the operand.
fn clone_ptr_rhs_if_needed(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    rhs: &mut Operand,
    span: crate::span::Span,
) {
    let taken = std::mem::replace(rhs, Operand::Constant(Constant::Unit));
    *rhs = ctx.ensure_owned_at_boundary(
        builder, taken, span, crate::ir::ImplicitCloneReason::StructFieldFromBorrow,
    );
}

/// Emit a field store with full cleanup: drop old value, unregister string temps,
/// assign, move-zero the RHS temp to prevent scope-exit double-free.
fn emit_field_store_with_cleanup(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target_place: &Place,
    field_type: TypeId,
    rhs: &Operand,
) {
    emit_field_drop_if_needed(ctx, builder, target_place, field_type);
    maybe_unregister_string_temp(ctx, builder, rhs, field_type);
    maybe_unregister_owned_string_temp(ctx, builder, rhs, field_type);
    builder.assign(target_place.clone(), rhs.clone());
    if let Operand::Copy(ref p) | Operand::Move(ref p) = *rhs {
        if p.projections.is_empty()
            && !ctx.drops.is_moved(p.local)
            && ctx.drops.is_registered(p.local)
        {
            ctx.move_zero_and_mark(builder, p.local);
        }
    }
}

/// If the RHS is a bare GorgetString local being assigned to a GorgetString field,
/// unregister it from drop tracking to prevent double-free. The field now owns the
/// data; the temp should not be freed when it goes out of scope.
fn maybe_unregister_owned_string_temp(
    ctx: &mut LoweringContext,
    builder: &FunctionBuilder,
    rhs: &Operand,
    target_type: TypeId,
) {
    if target_type != ctx.type_mapper.owned_string_type {
        return;
    }
    let place = match rhs {
        Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => place,
        _ => return,
    };
    if builder.local_type(place.local) == ctx.type_mapper.owned_string_type {
        ctx.drops.unregister(place.local);
    }
}

/// If a GorgetString temp is being assigned to another string local,
/// unregister the temp from drop tracking. The CoW alias shares the
/// underlying buffer — the new owner is responsible for freeing it.
fn maybe_unregister_string_temp(
    ctx: &mut LoweringContext,
    builder: &FunctionBuilder,
    rhs: &Operand,
    target_type: TypeId,
) {
    if target_type != ctx.type_mapper.owned_string_type {
        return;
    }
    let place = match rhs {
        Operand::Copy(place) | Operand::Move(place) => Some(place),
        _ => None,
    };
    if let Some(place) = place {
        if place.projections.is_empty() {
            let rhs_type = builder.local_type(place.local);
            if rhs_type == ctx.type_mapper.owned_string_type {
                ctx.drops.unregister(place.local);
            }
        }
    }
}

/// Lower an index assignment: `obj[index] = value`
pub(super) fn lower_index_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    // CoW: index write mutates the object. Sever aliases before proceeding.
    if let Expr::Identifier(obj_name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(obj_name) {
            ctx.cow_before_mutation(builder, local_id, object.span);
        }
    } else if let Some(field_path) = extract_field_path_string(&object.node) {
        ctx.cow_before_field_mutation(builder, &field_path, object.span);
    }

    // When the object is a struct field access (e.g. self.dict_field[key] = val),
    // resolve the field to a Place in-place to avoid copying the Dict struct.
    // This ensures hash table resizes and metadata updates propagate to the original.
    let (obj, resolved_field_type) = if let Expr::FieldAccess { object: inner_obj, field } = &object.node {
        if let Some((field_place, field_type)) = try_resolve_field_place(ctx, builder, inner_obj, &field.node) {
            (Operand::Copy(field_place), Some(field_type))
        } else {
            (lower_expr(ctx, builder, object), None)
        }
    } else {
        (lower_expr(ctx, builder, object), None)
    };
    let idx = lower_expr(ctx, builder, index);
    let val = lower_expr(ctx, builder, value);

    // Determine the receiver type to dispatch correctly.
    // Use the resolved field type if we resolved through a field access,
    // since infer_operand_type_full doesn't walk projections.
    let obj_type = resolved_field_type.unwrap_or_else(|| infer_operand_type_full(ctx, &obj, builder));
    let obj_type = ctx.pointee_type(obj_type).unwrap_or(obj_type);
    let type_name = ctx.type_name_for_id(obj_type).unwrap_or("").to_string();
    let is_vector = type_name.starts_with("Vector__") || type_name == "GorgetArray";
    let is_dict = type_name.starts_with("Dict__") || type_name.starts_with("HashMap__")
        || type_name == "GorgetMap";

    // Auto-clone borrow sources for consuming args at `Dict[k]=v` / `Vec[i]=v` sites.
    // The runtime is symmetric with gorget_array_push: gorget_map_put / _set /
    // _insert take ownership via raw memcpy without an internal clone, so the
    // compiler must guarantee independence at the call site. Delegate the
    // decision to the shared `ensure_owned_at_consuming_arg` helper — same rule
    // used by `lower_method_call` for push/put/set method calls.
    //
    // Helper: emit MoveZero on an operand's local after the call to transfer
    // ownership to the collection. For resource-typed locals that are the
    // source of a consuming arg, the slot's bytes are memcpy'd into the
    // collection — zeroing the source prevents the scope-exit drop from
    // double-freeing. Skips Ptr-wrapped locals (already materialized) and
    // primitives.
    let maybe_move_zero = |ctx: &mut LoweringContext,
                           builder: &mut FunctionBuilder,
                           op: &Operand| {
        if let Operand::Copy(place) | Operand::Move(place) = op {
            if place.projections.is_empty()
                && is_resource_type_local(place.local, builder, &ctx.type_registry)
            {
                let ty = builder.local_type(place.local);
                let is_ptr = matches!(
                    ctx.type_registry.get(ty),
                    Some(crate::ir::types::GirType::Ptr(_)) | Some(crate::ir::types::GirType::MutPtr(_))
                );
                if !is_ptr {
                    builder.move_zero(Place::local(place.local));
                    ctx.drops.mark_moved(place.local);
                }
            }
        }
    };

    if is_vector {
        // Vector[i] = val → Vector__T__set(&arr, index, val)
        let val = ctx.ensure_owned_at_consuming_arg(
            builder, val, value, crate::ir::ImplicitCloneReason::ConsumingArg);
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let ptr_type = ctx.register_mut_ptr_type(obj_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(ptr_local, place.clone());
            let mangled = format!("{type_name}__set");
            builder.call_void(
                mangled,
                vec![FunctionBuilder::copy(ptr_local), idx, val.clone()],
            );
            maybe_move_zero(ctx, builder, &val);
        }
    } else if is_dict {
        // Dict[key] = val → Dict__K__V__put(&dict, key, val)
        let idx = ctx.ensure_owned_at_consuming_arg(
            builder, idx, index, crate::ir::ImplicitCloneReason::ConsumingArg);
        let val = ctx.ensure_owned_at_consuming_arg(
            builder, val, value, crate::ir::ImplicitCloneReason::ConsumingArg);
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let ptr_type = ctx.register_mut_ptr_type(obj_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(ptr_local, place.clone());
            let mangled = format!("{type_name}__put");
            builder.call_void(
                mangled,
                vec![FunctionBuilder::copy(ptr_local), idx.clone(), val.clone()],
            );
            maybe_move_zero(ctx, builder, &idx);
            maybe_move_zero(ctx, builder, &val);
        }
    } else {
        // Check for IndexMut / set equip method (operator overload)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let candidates = [
                format!("{type_name}__set"),
                format!("IndexMut_for_{type_name}__set"),
                format!("{type_name}____setitem__"),
            ];
            for set_name in &candidates {
                if ctx.fn_sigs.contains_key(set_name.as_str()) {
                    let ptr_type = ctx.register_mut_ptr_type(obj_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, place.clone());
                    builder.call_void(
                        set_name.clone(),
                        vec![FunctionBuilder::copy(ptr_local), idx, val],
                    );
                    return;
                }
            }
        }
    }
    // String index assignment not supported (strings are immutable views)
}

/// Lower a compound assignment (e.g., `x += 1`).
pub(super) fn lower_compound_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    op: ast::BinaryOp,
    value: &Spanned<Expr>,
) {
    if let Expr::Identifier(name) = &target.node {
        if let Some((local_id, type_id)) = ctx.lookup_local(name) {
            // Shared variable: dispatch based on wrapper kind
            if let Some(info) = ctx.shared.locals.get(&local_id) {
                let (hidden_local, inner_type, kind) = (info.hidden_local, info.inner_type, info.kind);
                match kind {
                    SharedLocalKind::Mutex => {
                        let inner_c = ctx.c_type_name_for_id(inner_type);
                        let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                        let cur_val = emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type);
                        let rhs = lower_expr(ctx, builder, value);
                        let gir_op = match op {
                            ast::BinaryOp::Add => BinOp::Add,
                            ast::BinaryOp::Sub => BinOp::Sub,
                            ast::BinaryOp::Mul => BinOp::Mul,
                            ast::BinaryOp::Div => BinOp::Div,
                            ast::BinaryOp::Rem => BinOp::Rem,
                            ast::BinaryOp::Mod => BinOp::Mod,
                            ast::BinaryOp::BitAnd => BinOp::BitAnd,
                            ast::BinaryOp::BitOr => BinOp::BitOr,
                            ast::BinaryOp::BitXor => BinOp::BitXor,
                            ast::BinaryOp::Shl => BinOp::Shl,
                            ast::BinaryOp::Shr => BinOp::Shr,
                            _ => BinOp::Add,
                        };
                        let new_val = builder.bin_op(gir_op, inner_type, cur_val, rhs);
                        emit_shared_mutex_lock_set(ctx, builder, hidden_local, mutex_type, inner_type, FunctionBuilder::copy(new_val));
                        return;
                    }
                    SharedLocalKind::Atomic => {
                        // For += and -=, use native atomic add/sub (lock-free)
                        // For other ops, fall back to load → compute → CAS loop
                        let rhs = lower_expr(ctx, builder, value);
                        let atomic_name = atomic_type_name_for(inner_type);
                        match op {
                            ast::BinaryOp::Add => {
                                let add_fn = format!("{atomic_name}__add");
                                builder.call(&add_fn, vec![FunctionBuilder::copy(hidden_local), rhs], inner_type);
                                return;
                            }
                            ast::BinaryOp::Sub => {
                                let sub_fn = format!("{atomic_name}__sub");
                                builder.call(&sub_fn, vec![FunctionBuilder::copy(hidden_local), rhs], inner_type);
                                return;
                            }
                            _ => {
                                // Fallback: atomic load → compute → atomic store (NOT atomic, but functional)
                                let cur_val = emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name);
                                let gir_op = match op {
                                    ast::BinaryOp::Mul => BinOp::Mul,
                                    ast::BinaryOp::Div => BinOp::Div,
                                    ast::BinaryOp::Rem => BinOp::Rem,
                                    ast::BinaryOp::Mod => BinOp::Mod,
                                    ast::BinaryOp::BitAnd => BinOp::BitAnd,
                                    ast::BinaryOp::BitOr => BinOp::BitOr,
                                    ast::BinaryOp::BitXor => BinOp::BitXor,
                                    ast::BinaryOp::Shl => BinOp::Shl,
                                    ast::BinaryOp::Shr => BinOp::Shr,
                                    _ => BinOp::Add,
                                };
                                let new_val = builder.bin_op(gir_op, inner_type, cur_val, rhs);
                                emit_atomic_store(ctx, builder, hidden_local, FunctionBuilder::copy(new_val), &atomic_name);
                                return;
                            }
                        }
                    }
                    SharedLocalKind::RwLock => {
                        // Write-lock, get current value, compute, set, release — all under one lock
                        let (guard_ptr, cur_val) = emit_rwlock_write_get(ctx, builder, hidden_local, inner_type);
                        let rhs = lower_expr(ctx, builder, value);
                        let gir_op = match op {
                            ast::BinaryOp::Add => BinOp::Add,
                            ast::BinaryOp::Sub => BinOp::Sub,
                            ast::BinaryOp::Mul => BinOp::Mul,
                            ast::BinaryOp::Div => BinOp::Div,
                            ast::BinaryOp::Rem => BinOp::Rem,
                            ast::BinaryOp::Mod => BinOp::Mod,
                            ast::BinaryOp::BitAnd => BinOp::BitAnd,
                            ast::BinaryOp::BitOr => BinOp::BitOr,
                            ast::BinaryOp::BitXor => BinOp::BitXor,
                            ast::BinaryOp::Shl => BinOp::Shl,
                            ast::BinaryOp::Shr => BinOp::Shr,
                            _ => BinOp::Add,
                        };
                        let new_val = builder.bin_op(gir_op, inner_type, cur_val, rhs);
                        emit_rwlock_write_finish(ctx, builder, guard_ptr, inner_type, FunctionBuilder::copy(new_val));
                        return;
                    }
                    SharedLocalKind::SharedArc => {
                        // ArcOnly: compound-assign shouldn't happen (CFA upgrades to ArcMutex)
                    }
                }
            }

            let is_mut_capture = ctx.is_param_borrow_unique(local_id);
            let value_type = if is_mut_capture {
                // The local's GIR type is MutPtr(T); pointee_type returns T.
                ctx.pointee_type(builder.local_type(local_id)).unwrap_or(type_id)
            } else {
                type_id
            };
            // Read current value (deref if mutable capture)
            let cur_val = if is_mut_capture {
                let deref_place = Place {
                    local: local_id,
                    projections: vec![Projection::Deref],
                };
                let tmp = builder.add_local(value_type, None);
                builder.assign(Place::local(tmp), Operand::Copy(deref_place));
                FunctionBuilder::copy(tmp)
            } else {
                FunctionBuilder::copy(local_id)
            };

            let rhs = lower_expr(ctx, builder, value);
            let is_string = value_type == ctx.type_mapper.owned_string_type;

            // String concatenation via += → gorget_str_cat (returns GorgetString)
            if is_string && matches!(op, ast::BinaryOp::Add) {
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = ctx.call_extern_tracked(builder, "gorget_str_cat", vec![cur_val, rhs], owned_type);
                let dst = if is_mut_capture {
                    Place { local: local_id, projections: vec![Projection::Deref] }
                } else {
                    Place::local(local_id)
                };
                // Phase C: tmp is fresh from gorget_str_cat, dead after this
                // single assign — Move transfers ownership.
                builder.assign_mode(
                    crate::ir::instructions::AssignMode::Move,
                    dst,
                    FunctionBuilder::copy(tmp),
                );
                // Mark the temp as moved so the drop elaborator doesn't free it
                // (the destination variable now owns the GorgetString)
                ctx.move_zero_and_mark(builder, tmp);
                return;
            }

            // Check for operator overload on Named types
            let overload_method = match op {
                ast::BinaryOp::Add => Some("add"),
                ast::BinaryOp::Sub => Some("sub"),
                ast::BinaryOp::Mul => Some("mul"),
                ast::BinaryOp::Div => Some("div"),
                ast::BinaryOp::Rem => Some("rem"),
                ast::BinaryOp::Mod => Some("mod"),
                _ => None,
            }.and_then(|method| {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(value_type).cloned() {
                    let mangled = format!("{type_name}__{method}");
                    let has_method = ctx.fn_sigs.contains_key(&mangled)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__{method}")));
                    if has_method {
                        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                            mangled
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__{method}")))
                                .cloned()
                                .unwrap_or(mangled)
                        };
                        Some(effective_name)
                    } else { None }
                } else { None }
            });

            let tmp = if let Some(effective_name) = overload_method {
                // Borrow lhs for self parameter
                let self_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = cur_val {
                    let ptr_type = ctx.register_ptr_type(value_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, place.clone());
                    FunctionBuilder::copy(ptr_local)
                } else {
                    cur_val
                };
                builder.call(effective_name, vec![self_ptr, rhs], value_type)
            } else {
                let gir_op = match op {
                    ast::BinaryOp::Add => BinOp::Add,
                    ast::BinaryOp::Sub => BinOp::Sub,
                    ast::BinaryOp::Mul => BinOp::Mul,
                    ast::BinaryOp::Div => BinOp::Div,
                    ast::BinaryOp::Rem => BinOp::Rem,
                    ast::BinaryOp::Mod => BinOp::Mod,
                    ast::BinaryOp::AddWrap => BinOp::AddWrap,
                    ast::BinaryOp::SubWrap => BinOp::SubWrap,
                    ast::BinaryOp::MulWrap => BinOp::MulWrap,
                    ast::BinaryOp::BitAnd => BinOp::BitAnd,
                    ast::BinaryOp::BitOr => BinOp::BitOr,
                    ast::BinaryOp::BitXor => BinOp::BitXor,
                    ast::BinaryOp::Shl => BinOp::Shl,
                    ast::BinaryOp::Shr => BinOp::Shr,
                    _ => BinOp::Add, // fallback
                };
                builder.bin_op(gir_op, value_type, cur_val, rhs)
            };
            let dst = if is_mut_capture {
                Place { local: local_id, projections: vec![Projection::Deref] }
            } else {
                Place::local(local_id)
            };
            // Phase C: tmp is fresh op-result (binop or overload call), dead
            // after this assign. Move for resource types, Copy for primitives.
            let cmp_mode = if ctx.type_registry.is_resource_type(value_type)
                || ctx.type_registry.needs_drop(value_type)
            {
                crate::ir::instructions::AssignMode::Move
            } else {
                crate::ir::instructions::AssignMode::Copy
            };
            builder.assign_mode(cmp_mode, dst, FunctionBuilder::copy(tmp));
        } else if ctx.global_names.contains(name.as_str()) {
            // Module-level static variable — read via GlobalRef, compute, write via GlobalAssign
            let cur_val = Operand::Constant(Constant::GlobalRef(name.clone()));
            let rhs = lower_expr(ctx, builder, value);
            // Determine a type for the binop — look up from global_type_names
            let value_type = ctx.global_type_names.get(name)
                .and_then(|tn| ctx.type_mapper.lookup_named(tn))
                .unwrap_or(I64_TYPE);
            let gir_op = match op {
                ast::BinaryOp::Add => BinOp::Add,
                ast::BinaryOp::Sub => BinOp::Sub,
                ast::BinaryOp::Mul => BinOp::Mul,
                ast::BinaryOp::Div => BinOp::Div,
                ast::BinaryOp::Rem => BinOp::Rem,
                ast::BinaryOp::Mod => BinOp::Mod,
                ast::BinaryOp::AddWrap => BinOp::AddWrap,
                ast::BinaryOp::SubWrap => BinOp::SubWrap,
                ast::BinaryOp::MulWrap => BinOp::MulWrap,
                ast::BinaryOp::BitAnd => BinOp::BitAnd,
                ast::BinaryOp::BitOr => BinOp::BitOr,
                ast::BinaryOp::BitXor => BinOp::BitXor,
                ast::BinaryOp::Shl => BinOp::Shl,
                ast::BinaryOp::Shr => BinOp::Shr,
                _ => BinOp::Add,
            };
            let tmp = builder.bin_op(gir_op, value_type, cur_val, rhs);
            builder.global_assign(name.clone(), FunctionBuilder::copy(tmp));
        }
    } else if let Expr::FieldAccess { object, field } = &target.node {
        // Compound assign on struct field: obj.field OP= val
        // Desugar to: read field → compute → write field back
        if let Some((field_place, field_type)) = try_resolve_field_place(ctx, builder, object, &field.node) {
            // Read current field value
            let cur = builder.add_local(field_type, None);
            builder.assign(Place::local(cur), Operand::Copy(field_place.clone()));

            let rhs = lower_expr(ctx, builder, value);

            // String concatenation: field += str → gorget_str_cat
            let is_string = field_type == ctx.type_mapper.owned_string_type;
            if is_string && matches!(op, ast::BinaryOp::Add) {
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = builder.call_extern(
                    "gorget_str_cat",
                    vec![FunctionBuilder::copy(cur), rhs],
                    owned_type,
                );
                emit_field_drop_if_needed(ctx, builder, &field_place, field_type);
                builder.assign(field_place, FunctionBuilder::copy(tmp));
                return;
            }

            // Check for operator overload on Named types
            let overload_method = match op {
                ast::BinaryOp::Add => Some("add"),
                ast::BinaryOp::Sub => Some("sub"),
                ast::BinaryOp::Mul => Some("mul"),
                ast::BinaryOp::Div => Some("div"),
                ast::BinaryOp::Rem => Some("rem"),
                ast::BinaryOp::Mod => Some("mod"),
                _ => None,
            }.and_then(|method| {
                if let Some(GirType::Named(type_name)) = ctx.type_registry.get(field_type).cloned() {
                    let mangled = format!("{type_name}__{method}");
                    let has_method = ctx.fn_sigs.contains_key(&mangled)
                        || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__{method}")));
                    if has_method {
                        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                            mangled
                        } else {
                            ctx.fn_sigs.keys()
                                .find(|k| k.ends_with(&format!("_for_{type_name}__{method}")))
                                .cloned()
                                .unwrap_or(mangled)
                        };
                        Some(effective_name)
                    } else { None }
                } else { None }
            });

            let result = if let Some(effective_name) = overload_method {
                // Borrow lhs for self parameter
                let ptr_type = ctx.register_ptr_type(field_type);
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow(ptr_local, Place::local(cur));
                builder.call(effective_name, vec![FunctionBuilder::copy(ptr_local), rhs], field_type)
            } else {
                let gir_op = compound_op_to_gir(op);
                builder.bin_op(gir_op, field_type, FunctionBuilder::copy(cur), rhs)
            };
            builder.assign(field_place, FunctionBuilder::copy(result));
        }
    } else if let Expr::Index { object, index } = &target.node {
        // Compound assign on index: obj[i] OP= val
        // Desugar to: current = obj[i]; result = current OP val; obj[i] = result

        // Resolve the object — handle field access (self.vec) by resolving in-place
        let (obj, resolved_field_type) = if let Expr::FieldAccess { object: inner_obj, field } = &object.node {
            if let Some((field_place, field_type)) = try_resolve_field_place(ctx, builder, inner_obj, &field.node) {
                (Operand::Copy(field_place), Some(field_type))
            } else {
                (lower_expr(ctx, builder, object), None)
            }
        } else {
            (lower_expr(ctx, builder, object), None)
        };

        let idx_raw = lower_expr(ctx, builder, index);
        let obj_type = resolved_field_type.unwrap_or_else(|| infer_operand_type_full(ctx, &obj, builder));
        let obj_type = ctx.pointee_type(obj_type).unwrap_or(obj_type);
        let type_name = ctx.type_name_for_id(obj_type).unwrap_or("").to_string();
        let is_vector = type_name.starts_with("Vector__") || type_name == "GorgetArray";
        let is_dict = type_name.starts_with("Dict__") || type_name.starts_with("HashMap__")
            || type_name == "GorgetMap";

        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            // Save index into a local so it can be reused for both read and write.
            // Phase C: for resource-typed indices (e.g. Dict[String, V] keys),
            // Move from the source (key local) — the idx_local is consumed by
            // both the read and the eventual put/set, but cap=0 propagation
            // makes the shallow shape correct for literal sources today.
            let idx_type = infer_operand_type_full(ctx, &idx_raw, builder);
            let idx_local = builder.add_local(idx_type, None);
            let idx_mode = if let Operand::Copy(ref p) | Operand::Move(ref p) = idx_raw {
                let src_ty = builder.local_type(p.local);
                if p.projections.is_empty()
                    && (ctx.type_registry.is_resource_type(src_ty)
                        || ctx.type_registry.needs_drop(src_ty))
                {
                    crate::ir::instructions::AssignMode::Borrow
                } else {
                    crate::ir::instructions::AssignMode::Copy
                }
            } else {
                crate::ir::instructions::AssignMode::Copy
            };
            builder.assign_mode(idx_mode, Place::local(idx_local), idx_raw);

            // For field-accessed collections (e.g. self.scores[i]), copy to a temp local
            // so the C backend can determine the collection type from the local's TypeId.
            // index_load doesn't handle Places with Field projections correctly.
            // Phase C: temp is a non-owning view of the field — Borrow.
            let read_place = if resolved_field_type.is_some() {
                let temp = builder.add_local(obj_type, None);
                let temp_mode = if ctx.type_registry.is_resource_type(obj_type) {
                    crate::ir::instructions::AssignMode::Borrow
                } else {
                    crate::ir::instructions::AssignMode::Copy
                };
                builder.assign_mode(temp_mode, Place::local(temp), Operand::Copy(place.clone()));
                Place::local(temp)
            } else {
                place.clone()
            };

            // Step 1: Read current value at index
            let (cur_val, elem_type) = if is_vector || is_dict {
                let elem_type = infer_collection_element_type(ctx, obj_type);
                let dst = builder.index_load(read_place, FunctionBuilder::copy(idx_local), elem_type);
                (FunctionBuilder::copy(dst), elem_type)
            } else {
                // Custom type: try Type__get / Index_for_Type__get
                let candidates = [
                    format!("{type_name}__get"),
                    format!("Index_for_{type_name}__get"),
                    format!("{type_name}____getitem__"),
                ];
                let mut found = None;
                for get_name in &candidates {
                    if ctx.fn_sigs.contains_key(get_name.as_str()) {
                        let ret_type = ctx.fn_sigs.get(get_name.as_str())
                            .map(|(_, ret)| *ret)
                            .unwrap_or(I64_TYPE);
                        let pt = ctx.register_ptr_type(obj_type);
                        let pl = builder.add_local(pt, None);
                        builder.emit_borrow(pl, place.clone());
                        let dst = builder.call(
                            get_name.clone(),
                            vec![FunctionBuilder::copy(pl), FunctionBuilder::copy(idx_local)],
                            ret_type,
                        );
                        found = Some((FunctionBuilder::copy(dst), ret_type));
                        break;
                    }
                }
                if let Some(pair) = found {
                    pair
                } else {
                    // Fallback: string indexing or unknown type
                    let elem_type = if obj_type == ctx.type_mapper.owned_string_type {
                        ctx.type_mapper.owned_string_type
                    } else {
                        I64_TYPE
                    };
                    let dst = builder.index_load(read_place, FunctionBuilder::copy(idx_local), elem_type);
                    (FunctionBuilder::copy(dst), elem_type)
                }
            };

            // Step 2: Lower RHS
            let rhs = lower_expr(ctx, builder, value);

            // Step 3: Compute result
            let is_string = elem_type == ctx.type_mapper.owned_string_type;

            let result = if is_string && matches!(op, ast::BinaryOp::Add) {
                // String concatenation via gorget_str_cat
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = builder.call_extern(
                    "gorget_str_cat",
                    vec![cur_val, rhs],
                    owned_type,
                );
                FunctionBuilder::copy(tmp)
            } else {
                // Check for operator overload on Named types
                let overload_method = match op {
                    ast::BinaryOp::Add => Some("add"),
                    ast::BinaryOp::Sub => Some("sub"),
                    ast::BinaryOp::Mul => Some("mul"),
                    ast::BinaryOp::Div => Some("div"),
                    ast::BinaryOp::Rem => Some("rem"),
                    ast::BinaryOp::Mod => Some("mod"),
                    _ => None,
                }.and_then(|method| {
                    if let Some(GirType::Named(tn)) = ctx.type_registry.get(elem_type).cloned() {
                        let mangled = format!("{tn}__{method}");
                        let has_method = ctx.fn_sigs.contains_key(&mangled)
                            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{tn}__{method}")));
                        if has_method {
                            let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
                                mangled
                            } else {
                                ctx.fn_sigs.keys()
                                    .find(|k| k.ends_with(&format!("_for_{tn}__{method}")))
                                    .cloned()
                                    .unwrap_or(mangled)
                            };
                            Some(effective_name)
                        } else { None }
                    } else { None }
                });

                if let Some(effective_name) = overload_method {
                    // Borrow lhs for self parameter
                    let cur_local = builder.add_local(elem_type, None);
                    builder.assign(Place::local(cur_local), cur_val);
                    let ptr_type = ctx.register_ptr_type(elem_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, Place::local(cur_local));
                    let dst = builder.call(effective_name, vec![FunctionBuilder::copy(ptr_local), rhs], elem_type);
                    FunctionBuilder::copy(dst)
                } else {
                    let gir_op = compound_op_to_gir(op);
                    let tmp = builder.bin_op(gir_op, elem_type, cur_val, rhs);
                    FunctionBuilder::copy(tmp)
                }
            };

            // Step 4: Write back via collection set method
            if is_vector {
                let ptr_type = ctx.register_mut_ptr_type(obj_type);
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(ptr_local, place.clone());
                let mangled = format!("{type_name}__set");
                builder.call_void(
                    mangled,
                    vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local), result],
                );
            } else if is_dict {
                let ptr_type = ctx.register_mut_ptr_type(obj_type);
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(ptr_local, place.clone());
                let mangled = format!("{type_name}__put");
                builder.call_void(
                    mangled,
                    vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local), result],
                );
            } else {
                // Custom type: try Type__set / IndexMut_for_Type__set
                let set_candidates = [
                    format!("{type_name}__set"),
                    format!("IndexMut_for_{type_name}__set"),
                    format!("{type_name}____setitem__"),
                ];
                for set_name in &set_candidates {
                    if ctx.fn_sigs.contains_key(set_name.as_str()) {
                        let ptr_type = ctx.register_mut_ptr_type(obj_type);
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow_mut(ptr_local, place.clone());
                        builder.call_void(
                            set_name.clone(),
                            vec![FunctionBuilder::copy(ptr_local), FunctionBuilder::copy(idx_local), result],
                        );
                        break;
                    }
                }
            }
        }
    }
}

/// Map compound assignment operator to GIR binary operator.
fn compound_op_to_gir(op: ast::BinaryOp) -> BinOp {
    match op {
        ast::BinaryOp::Add => BinOp::Add,
        ast::BinaryOp::Sub => BinOp::Sub,
        ast::BinaryOp::Mul => BinOp::Mul,
        ast::BinaryOp::Div => BinOp::Div,
        ast::BinaryOp::Rem => BinOp::Rem,
        ast::BinaryOp::Mod => BinOp::Mod,
        ast::BinaryOp::AddWrap => BinOp::AddWrap,
        ast::BinaryOp::SubWrap => BinOp::SubWrap,
        ast::BinaryOp::MulWrap => BinOp::MulWrap,
        ast::BinaryOp::BitAnd => BinOp::BitAnd,
        ast::BinaryOp::BitOr => BinOp::BitOr,
        ast::BinaryOp::BitXor => BinOp::BitXor,
        ast::BinaryOp::Shl => BinOp::Shl,
        ast::BinaryOp::Shr => BinOp::Shr,
        _ => BinOp::Add,
    }
}
