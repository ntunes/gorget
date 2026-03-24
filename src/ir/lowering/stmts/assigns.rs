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
    try_resolve_field_place,
    infer_collection_element_type,
};

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
                // Check if old value needs dropping before reassignment.
                // For collection types (no TypeDef), only drop if the local is
                // confirmed sole owner (was move-zero'd at VarDecl). Shallow copies
                // from struct field reads share data and must not be freed here.
                let needs_drop = {
                    use crate::ir::types::GirType;
                    if let Some(GirType::Named(type_name)) = ctx.type_registry.get(type_id) {
                        let type_name = type_name.clone();
                        if ctx.type_registry.is_collection_type_name(&type_name) {
                            ctx.drops.is_moved(local_id)
                        } else if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                            type_def.metadata.drop_strategy != DropStrategy::None
                        } else { false }
                    } else { false }
                };
                // Compute new value FIRST (it may reference the old value, e.g. s = s + x)
                let prev_expected = ctx.expected_type;
                ctx.expected_type = Some(type_id);
                let operand = lower_expr(ctx, builder, value);
                // Auto-propagate: if RHS is Result-typed but target is not, unwrap
                // NOTE: must run before restoring expected_type so the guard sees type_id.
                let mut operand = maybe_auto_propagate(ctx, builder, operand);
                ctx.expected_type = prev_expected;
                // P2.6: Drop old value AFTER computing new value, BEFORE assigning
                if needs_drop {
                    builder.drop(Place::local(local_id));
                }
                // If this is a mutable capture pointer, write through the pointer
                if ctx.mut_capture_locals.contains_key(&local_id) {
                    let deref_place = Place {
                        local: local_id,
                        projections: vec![Projection::Deref],
                    };
                    builder.assign(deref_place, operand.clone());
                    super::maybe_emit_field_move_zero(ctx, builder, &operand);
                } else {
                    // Determine assignment mode (same decision tree as VarDecl).
                    use crate::ir::instructions::AssignMode;
                    let mut assign_mode = AssignMode::Copy;

                    if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                        if place.projections.is_empty() && place.local != local_id {
                            let rhs_type = builder.locals[place.local.0 as usize].type_id;

                            if rhs_type == ctx.type_mapper.owned_string_type
                                && type_id == ctx.type_mapper.string_view_type
                            {
                                ctx.drops.unregister(place.local);
                                assign_mode = AssignMode::Borrow;
                            } else if ctx.is_named_local(place.local) {
                                if let Some(clone_fn) = ctx.clone_fn_for_ptr(rhs_type) {
                                    let ptr_type = ctx.register_ptr_type(rhs_type);
                                    let ptr_local = builder.add_local(ptr_type, None);
                                    builder.emit_borrow(ptr_local, place.clone());
                                    let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(ptr_local)], rhs_type);
                                    operand = FunctionBuilder::copy(cloned);
                                    assign_mode = AssignMode::Move;
                                }
                            } else if ctx.drops.is_registered(place.local) {
                                assign_mode = AssignMode::Move;
                            } else if rhs_type == ctx.type_mapper.owned_string_type
                                && type_id == ctx.type_mapper.owned_string_type
                            {
                                assign_mode = AssignMode::Move;
                            }
                            // Safety net: no Copy for resource types.
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
                                builder.move_zero(Place::local(place.local));
                                ctx.drops.mark_moved(place.local);
                            }
                        }
                    }
                    super::maybe_emit_field_move_zero(ctx, builder, &operand);
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

    // Try to resolve the full field projection chain without materializing
    // intermediate struct values. This handles nested field writes like
    // `gs.current_weapon.ammo = x` by building Place { local: gs, projections: [Deref, Field(5), Field(2)] }
    // instead of copying the intermediate struct to a temp.
    if let Some((target_place, field_type)) = try_resolve_field_place(ctx, builder, object, field_name) {
        let rhs = lower_expr(ctx, builder, value);
        emit_field_drop_if_needed(ctx, builder, &target_place, field_type);
        maybe_unregister_str_view_temp(ctx, builder, &rhs, field_type);
        maybe_unregister_owned_string_temp(ctx, builder, &rhs, field_type);
        builder.assign(target_place, rhs.clone());
        // Move-zero drop-registered temps after field assignment
        // to prevent scope-exit double-free.
        if let Operand::Copy(ref p) | Operand::Move(ref p) = rhs {
            if p.projections.is_empty()
                && !ctx.drops.is_moved(p.local)
                && ctx.drops.is_registered(p.local)
            {
                builder.move_zero(Place::local(p.local));
                ctx.drops.mark_moved(p.local);
            }
        }
        super::maybe_emit_field_move_zero(ctx, builder, &rhs);
        return;
    }

    // Fallback: lower_expr on object (may copy intermediate structs)
    // For mut_capture_locals (mutable borrow params), use the pointer local directly
    // instead of lower_expr which would copy the deref'd value to a temp
    let obj = if let Expr::Identifier(name) = &object.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.mut_capture_locals.contains_key(&local_id) {
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
    let rhs = lower_expr(ctx, builder, value);

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
                            emit_field_drop_if_needed(ctx, builder, &target_place, field_type);
                            maybe_unregister_str_view_temp(ctx, builder, &rhs, field_type);
                            maybe_unregister_owned_string_temp(ctx, builder, &rhs, field_type);
                            builder.assign(target_place, rhs);
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
                            emit_field_drop_if_needed(ctx, builder, &target_place, field_type);
                            maybe_unregister_str_view_temp(ctx, builder, &rhs, field_type);
                            maybe_unregister_owned_string_temp(ctx, builder, &rhs, field_type);
                            builder.assign(target_place, rhs);
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
                    // Drop old field value before reassignment if it's droppable
                    emit_field_drop_if_needed(ctx, builder, &target_place, field_type);
                    maybe_unregister_str_view_temp(ctx, builder, &rhs, field_type);
                    maybe_unregister_owned_string_temp(ctx, builder, &rhs, field_type);
                    builder.assign(target_place.clone(), rhs.clone());
                    // Move-zero drop-registered temps after field assignment
                    // to prevent scope-exit double-free.
                    if let Operand::Copy(ref p) | Operand::Move(ref p) = rhs {
                        if p.projections.is_empty()
                            && !ctx.drops.is_moved(p.local)
                            && ctx.drops.is_registered(p.local)
                        {
                            builder.move_zero(Place::local(p.local));
                            ctx.drops.mark_moved(p.local);
                        }
                    }
                    super::maybe_emit_field_move_zero(ctx, builder, &rhs);
                    return;
                }
                // Fallback: look up from TypeDef
                // Look up field index and type from TypeDef (separate borrow scope)
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
                    emit_field_drop_if_needed(ctx, builder, &target_place, field_type);
                    maybe_unregister_str_view_temp(ctx, builder, &rhs, field_type);
                    maybe_unregister_owned_string_temp(ctx, builder, &rhs, field_type);
                    builder.assign(target_place.clone(), rhs.clone());
                    // Move-zero drop-registered temps after field assignment
                    if let Operand::Copy(ref p) | Operand::Move(ref p) = rhs {
                        if p.projections.is_empty()
                            && !ctx.drops.is_moved(p.local)
                            && ctx.drops.is_registered(p.local)
                        {
                            builder.move_zero(Place::local(p.local));
                            ctx.drops.mark_moved(p.local);
                        }
                    }
                    super::maybe_emit_field_move_zero(ctx, builder, &rhs);
                    return;
                }
            }
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
    if builder.locals[place.local.0 as usize].type_id == ctx.type_mapper.owned_string_type {
        ctx.drops.unregister(place.local);
    }
}

/// If a GorgetString temp is being assigned to a str-typed target (field, variable, etc.),
/// unregister the temp from drop tracking. The str view may escape the scope, and freeing
/// the GorgetString would create a use-after-free. The GorgetString will leak.
fn maybe_unregister_str_view_temp(
    ctx: &mut LoweringContext,
    builder: &FunctionBuilder,
    rhs: &Operand,
    target_type: TypeId,
) {
    if target_type != ctx.type_mapper.string_view_type {
        return;
    }
    let place = match rhs {
        Operand::Copy(place) | Operand::Move(place) => Some(place),
        _ => None,
    };
    if let Some(place) = place {
        if place.projections.is_empty() {
            let rhs_type = builder.locals[place.local.0 as usize].type_id;
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

    if is_vector {
        // Vector[i] = val → Vector__T__set(&arr, index, val)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let ptr_type = ctx.register_mut_ptr_type(obj_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(ptr_local, place.clone());
            let mangled = format!("{type_name}__set");
            builder.call_void(
                mangled,
                vec![FunctionBuilder::copy(ptr_local), idx, val],
            );
        }
    } else if is_dict {
        // Dict[key] = val → Dict__K__V__put(&dict, key, val)
        if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
            let ptr_type = ctx.register_mut_ptr_type(obj_type);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow_mut(ptr_local, place.clone());
            let mangled = format!("{type_name}__put");
            builder.call_void(
                mangled,
                vec![FunctionBuilder::copy(ptr_local), idx, val],
            );
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

            let is_mut_capture = ctx.mut_capture_locals.contains_key(&local_id);
            let value_type = if is_mut_capture {
                ctx.mut_capture_locals[&local_id]
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
            let is_string = value_type == ctx.type_mapper.string_view_type
                || value_type == ctx.type_mapper.owned_string_type;

            // String concatenation via += → gorget_str_cat (returns GorgetString)
            if is_string && matches!(op, ast::BinaryOp::Add) {
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = ctx.call_extern_tracked(builder, "gorget_str_cat", vec![cur_val, rhs], owned_type);
                let dst = if is_mut_capture {
                    Place { local: local_id, projections: vec![Projection::Deref] }
                } else {
                    Place::local(local_id)
                };
                builder.assign(dst, FunctionBuilder::copy(tmp));
                // Mark the temp as moved so the drop elaborator doesn't free it
                // (the destination variable now owns the GorgetString)
                builder.move_zero(Place::local(tmp));
                ctx.drops.mark_moved(tmp);
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
            builder.assign(dst, FunctionBuilder::copy(tmp));
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
            let is_string = field_type == ctx.type_mapper.string_view_type
                || field_type == ctx.type_mapper.owned_string_type;
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
            // Save index into a local so it can be reused for both read and write
            let idx_type = infer_operand_type_full(ctx, &idx_raw, builder);
            let idx_local = builder.add_local(idx_type, None);
            builder.assign(Place::local(idx_local), idx_raw);

            // For field-accessed collections (e.g. self.scores[i]), copy to a temp local
            // so the C backend can determine the collection type from the local's TypeId.
            // index_load doesn't handle Places with Field projections correctly.
            let read_place = if resolved_field_type.is_some() {
                let temp = builder.add_local(obj_type, None);
                builder.assign(Place::local(temp), Operand::Copy(place.clone()));
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
                    let elem_type = if obj_type == ctx.type_mapper.string_view_type
                        || obj_type == ctx.type_mapper.owned_string_type {
                        ctx.type_mapper.string_view_type
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
            let is_string = elem_type == ctx.type_mapper.string_view_type
                || elem_type == ctx.type_mapper.owned_string_type;

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
