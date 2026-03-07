//! Assignment lowering: simple assign, field assign, index assign, compound assign.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr};
use crate::span::Spanned;

use super::super::context::{LoweringContext, SharedLocalKind};
use super::super::exprs::{
    lower_expr, infer_operand_type_full, guard_inner_suffix, emit_guard_get_ptr,
    emit_shared_mutex_lock_get, emit_shared_mutex_lock_set,
    atomic_type_name_for, emit_atomic_load, emit_atomic_store,
    emit_rwlock_write_get, emit_rwlock_write_set, emit_rwlock_write_finish,
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
                // Check if old value needs dropping
                let needs_drop = {
                    use crate::ir::types::GirType;
                    if let Some(GirType::Named(type_name)) = ctx.type_registry.get(type_id) {
                        let type_name = type_name.clone();
                        if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                            type_def.metadata.drop_strategy != DropStrategy::None
                        } else { false }
                    } else { false }
                };
                // Compute new value FIRST (it may reference the old value, e.g. s = s + x)
                let prev_expected = ctx.expected_type;
                ctx.expected_type = Some(type_id);
                let operand = lower_expr(ctx, builder, value);
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
                    builder.assign(deref_place, operand);
                } else {
                    builder.assign(Place::local(local_id), operand);
                }
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
        if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
            type_def.metadata.drop_strategy != DropStrategy::None
        } else {
            // Collections (GorgetArray, GorgetDict, etc.) are always droppable
            type_name.starts_with("GorgetArray")
                || type_name.starts_with("GorgetDict")
                || type_name.starts_with("GorgetMap")
                || type_name.starts_with("GorgetSet")
                || type_name.starts_with("GorgetString")
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
                            builder.assign(target_place, rhs);
                            return;
                        }
                        if let Some(type_def) = ctx.type_registry.get_type_def(&inner_type_name) {
                            if let TypeDefKind::Struct(ref s) = type_def.kind {
                                for (i, f) in s.fields.iter().enumerate() {
                                    if f.name == field_name {
                                        let mut target_place = deref_place;
                                        target_place.projections.push(Projection::Field(i as u32));
                                        emit_field_drop_if_needed(ctx, builder, &target_place, f.type_id);
                                        builder.assign(target_place, rhs);
                                        return;
                                    }
                                }
                            }
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
                    builder.assign(target_place, rhs);
                    return;
                }
                // Fallback: look up from TypeDef
                if let Some(type_def) = ctx.type_registry.get_type_def(&type_name) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        for (i, f) in s.fields.iter().enumerate() {
                            if f.name == field_name {
                                let mut target_place = base_place;
                                target_place.projections.push(Projection::Field(i as u32));
                                // Drop old field value before reassignment if it's droppable
                                emit_field_drop_if_needed(ctx, builder, &target_place, f.type_id);
                                builder.assign(target_place, rhs);
                                return;
                            }
                        }
                    }
                }
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
    let obj = lower_expr(ctx, builder, object);
    let idx = lower_expr(ctx, builder, index);
    let val = lower_expr(ctx, builder, value);

    // Determine the receiver type to dispatch correctly
    let obj_type = infer_operand_type_full(ctx, &obj, builder);
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
            let is_string = value_type == ctx.type_mapper.str_type
                || value_type == ctx.type_mapper.owned_string_type;

            // String concatenation via += → gorget_str_cat (returns GorgetString)
            if is_string && matches!(op, ast::BinaryOp::Add) {
                let owned_type = ctx.type_mapper.owned_string_type;
                let tmp = builder.call_extern(
                    "gorget_str_cat",
                    vec![cur_val, rhs],
                    owned_type,
                );
                let dst = if is_mut_capture {
                    Place { local: local_id, projections: vec![Projection::Deref] }
                } else {
                    Place::local(local_id)
                };
                builder.assign(dst, FunctionBuilder::copy(tmp));
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
        }
    }
}
