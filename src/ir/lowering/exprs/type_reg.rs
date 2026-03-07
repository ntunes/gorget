//! Type registration helpers for generic container types (Box, Shared, Mutex, etc.),
//! tuple types, and operand type inference.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;

use super::super::context::LoweringContext;

pub(super) fn register_tuple_type(ctx: &mut LoweringContext, elem_types: &[TypeId]) -> TypeId {
    use crate::ir::types::{format_type_for_mangle, GirType, StructDef, StructField, TypeDef, TypeDefKind, TypeMetadata, CopySemantics};

    // Build mangled name: Tuple__T1__T2__...
    let mut name = "Tuple".to_string();
    for &tid in elem_types {
        name.push_str("__");
        name.push_str(&format_type_for_mangle(tid, &ctx.type_registry));
    }

    // Reuse existing TypeDef if already registered
    if let Some(existing) = ctx.type_mapper.lookup_named(&name) {
        // Ensure struct_fields is populated even if the type was pre-registered
        // (e.g., by map_ast_type_mut during fn_sigs pre-scan, which doesn't
        // have access to struct_fields)
        if !ctx.struct_fields.contains_key(&(name.clone(), "_0".to_string())) {
            if let Some(type_def) = ctx.type_registry.get_type_def(&name) {
                if let TypeDefKind::Struct(ref s) = type_def.kind {
                    for (i, field) in s.fields.iter().enumerate() {
                        ctx.struct_fields.insert(
                            (name.clone(), field.name.clone()),
                            (i as u32, field.type_id),
                        );
                    }
                }
            }
        }
        return existing;
    }

    // Create struct fields: _0, _1, _2, ...
    let fields: Vec<StructField> = elem_types.iter().enumerate()
        .map(|(i, &tid)| StructField { name: format!("_{i}"), type_id: tid })
        .collect();

    ctx.type_registry.add_type_def(TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields: fields.clone() }),
        metadata: TypeMetadata {
            copy_semantics: CopySemantics::Copy,
            ..TypeMetadata::default()
        },
    });

    // Also populate struct_fields cache so lookup_field() works
    // (populate_struct_fields() runs once before function lowering,
    // so dynamically-created tuple types need manual insertion)
    for (i, field) in fields.iter().enumerate() {
        ctx.struct_fields.insert(
            (name.clone(), field.name.clone()),
            (i as u32, field.type_id),
        );
    }

    let type_id = ctx.type_registry.insert(GirType::Named(name.clone()));
    ctx.type_mapper.register_named(name, type_id);
    type_id
}

/// Resolve the element type at a given index from a tuple TypeDef.
pub fn resolve_tuple_field_type(ctx: &LoweringContext, tuple_type_id: TypeId, index: usize) -> TypeId {
    if let Some(type_name) = ctx.type_name_for_id(tuple_type_id) {
        if let Some(type_def) = ctx.type_registry.get_type_def(type_name) {
            if let TypeDefKind::Struct(ref s) = type_def.kind {
                if let Some(field) = s.fields.get(index) {
                    return field.type_id;
                }
            }
        }
    }
    I64_TYPE // fallback
}

/// Ensure a Box type has a TypeDef in the registry so the C backend can emit its typedef.
pub fn ensure_box_type_def(ctx: &mut LoweringContext, box_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(box_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(box_type_name, inner_type, CopySemantics::Move, DropStrategy::Trivial("free".to_string()))
    );
}

/// Ensure a Shared[T] type has a TypeDef in the registry (Copy pointer, drop decrements refcount).
pub fn ensure_shared_type_def(ctx: &mut LoweringContext, shared_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(shared_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(shared_type_name, inner_type, CopySemantics::Copy, DropStrategy::Trivial(format!("{shared_type_name}__drop")))
    );
}

/// Ensure a Weak[T] type has a TypeDef in the registry (Copy pointer, drop decrements weak count).
pub fn ensure_weak_type_def(ctx: &mut LoweringContext, weak_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(weak_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(weak_type_name, inner_type, CopySemantics::Copy, DropStrategy::Trivial(format!("{weak_type_name}__drop")))
    );
}

/// Ensure a Mutex[T] type has a TypeDef in the registry (Copy pointer, no drop).
pub fn ensure_mutex_type_def(ctx: &mut LoweringContext, mutex_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(mutex_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(mutex_type_name, inner_type, CopySemantics::Copy, DropStrategy::None)
    );
}

/// Ensure a Guard[T] type has a TypeDef in the registry (Move value struct, drop releases mutex).
pub fn ensure_guard_type_def(ctx: &mut LoweringContext, guard_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(guard_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(guard_type_name, inner_type, CopySemantics::Move, DropStrategy::Trivial(format!("{guard_type_name}__drop")))
    );
}

/// Ensure a RWLock[T] type has a TypeDef in the registry (Copy pointer, no drop).
pub fn ensure_rwlock_type_def(ctx: &mut LoweringContext, rwlock_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(rwlock_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(rwlock_type_name, inner_type, CopySemantics::Copy, DropStrategy::None)
    );
}

/// Ensure a Channel[T] type has a TypeDef in the registry (Copy pointer, no drop).
pub fn ensure_channel_type_def(ctx: &mut LoweringContext, channel_type_name: &str) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_opaque_type_def;
    if ctx.type_registry.get_type_def(channel_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_opaque_type_def(channel_type_name, CopySemantics::Copy, DropStrategy::None)
    );
}

/// Ensure TaskGroup has a TypeDef in the registry (Move pointer, drop waits for all children).
pub fn ensure_task_group_type_def(ctx: &mut LoweringContext, tg_type_name: &str) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_opaque_type_def;
    if ctx.type_registry.get_type_def(tg_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_opaque_type_def(tg_type_name, CopySemantics::Move, DropStrategy::Trivial("gorget_task_group_free".to_string()))
    );
}

/// If `type_name` is a Guard/ReadGuard/WriteGuard type, return (inner_c_suffix, is_read_only).

pub fn infer_operand_type_full(ctx: &LoweringContext, operand: &Operand, builder: &FunctionBuilder) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // First check ctx locals
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            // Fall back to builder locals
            let idx = place.local.0 as usize;
            if idx < builder.locals.len() {
                return builder.locals[idx].type_id;
            }
            I64_TYPE
        }
        other => infer_operand_type(ctx, other),
    }
}

pub fn infer_operand_type(ctx: &LoweringContext, operand: &Operand) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // Look up the local's type
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            I64_TYPE // fallback
        }
        Operand::Constant(c) => match c {
            Constant::Bool(_) => BOOL_TYPE,
            Constant::I8(_) => I8_TYPE,
            Constant::I16(_) => I16_TYPE,
            Constant::I32(_) => I32_TYPE,
            Constant::I64(_) => I64_TYPE,
            Constant::U8(_) => U8_TYPE,
            Constant::U16(_) => U16_TYPE,
            Constant::U32(_) => U32_TYPE,
            Constant::U64(_) => U64_TYPE,
            Constant::F32(_) => F32_TYPE,
            Constant::F64(_) => F64_TYPE,
            Constant::Str(_) => ctx.type_mapper.str_type,
            Constant::Null => UNIT_TYPE,
            Constant::Unit => UNIT_TYPE,
            Constant::SizeOf(_) => U64_TYPE,
            Constant::FuncRef(_) => UNIT_TYPE, // treated as void* at call site
            Constant::GlobalRef(_) => UNIT_TYPE, // type looked up from global table at call site
        },
    }
}

/// Returns true if `local` has ANY Move-semantics type (Vector, Dict, Set,
/// GorgetString, Box, user Move structs). Used to emit MoveZero + mark_moved
/// after ownership transfer (function call args, unwrap, struct-init fields)
/// to prevent double-free of shared heap buffers.
pub(super) fn is_move_type_local(
    local: LocalId,
    builder: &FunctionBuilder,
    registry: &TypeRegistry,
) -> bool {
    registry.is_move_type(builder.locals[local.0 as usize].type_id)
}
