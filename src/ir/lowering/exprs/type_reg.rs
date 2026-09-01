//! Type registration helpers for generic container types (Box, Shared, Mutex, etc.),
//! tuple types, and operand type inference.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;

use super::super::context::LoweringContext;

pub(super) fn register_tuple_type(ctx: &mut LoweringContext, elem_types: &[TypeId]) -> TypeId {
    use crate::ir::types::{format_type_for_mangle, GirType, StructDef, StructField, TypeDef, TypeDefKind, TypeMetadata};

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

    // Tier 1c: coherence-at-construction. A tuple holding a resource-typed
    // element is itself a resource (its drop must recurse). Mirrors the
    // matching path in `map_ast_type_mut` for `Type::Tuple`.
    let (drop_strategy, copy_semantics) = ctx.type_registry.compute_drop_strategy_for_struct(&fields);
    ctx.type_registry.add_type_def(TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields: fields.clone() }),
        metadata: TypeMetadata {
            drop_strategy,
            copy_semantics,
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
    // Peel Ptr/MutPtr — closure tuple-destructure params arrive as
    // `*Tuple__T__U` (the closure's `Tuple` arg is passed by Ptr ABI).
    // Without this peel, the lookup falls through to the I64 fallback
    // and the field-load result is mis-typed, producing a Tier 2a
    // `AssignIntoOwnedSlot` violation at the desugared `T name =
    // __dp_0._0` VarDecl. The mismatch is also a structural type bug
    // — the GIR slot was sized for i64 but received a memcpy of a
    // GorgetString-shape struct.
    use crate::ir::types::GirType;
    let resolved = match ctx.type_registry.get(tuple_type_id) {
        Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
        _ => tuple_type_id,
    };
    if let Some(type_name) = ctx.type_name_for_id(resolved) {
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

/// Look up a named type by its mangled name, or register it if absent.
/// The optional `ensure_fn` callback runs only on first registration (to add TypeDefs).
pub(crate) fn get_or_register_type(
    ctx: &mut LoweringContext,
    name: &str,
    ensure_fn: Option<&dyn Fn(&mut LoweringContext)>,
) -> TypeId {
    if let Some(tid) = ctx.type_mapper.lookup_named(name) {
        return tid;
    }
    let tid = ctx.type_registry.insert(GirType::Named(name.to_string()));
    ctx.type_mapper.register_named(name.to_string(), tid);
    if let Some(f) = ensure_fn {
        f(ctx);
    }
    tid
}

/// Ensure a Box type has a TypeDef in the registry so the C backend can emit its typedef.
///
/// Coherence-at-construction: tag `is_box: true` here so downstream
/// consumers reading `TypeRegistry::is_box(type_id)` see the truth at
/// every Box-TypeDef registration path. This is one of three Box-registration
/// entry points (alongside `register_collection_alias`'s Box arm in
/// `lowering/types.rs:789` and `monomorphize_struct`'s Box arm in
/// `lowering/generics/mod.rs:2334`). All three must populate `is_box`
/// uniformly — see `docs/devbook/24-layering-discipline.md` rule 3
/// (one source of truth per axis).
pub fn ensure_box_type_def(ctx: &mut LoweringContext, box_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy, StructDef, StructField, TypeDef, TypeDefKind, TypeMetadata};
    if ctx.type_registry.get_type_def(box_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(TypeDef {
        name: box_type_name.to_string(),
        kind: TypeDefKind::Struct(StructDef {
            fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
        }),
        metadata: TypeMetadata {
            copy_semantics: CopySemantics::Resource,
            drop_strategy: DropStrategy::Trivial("free".to_string()),
            is_box: true,
            ..Default::default()
        },
    });
}

/// Ensure a Shared[T] type has a TypeDef in the registry (Copy pointer, drop decrements refcount).
pub fn ensure_shared_type_def(ctx: &mut LoweringContext, shared_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(shared_type_name).is_some() { return; }
    // Shared is a REFCOUNTED handle — its "clone" is a by-VALUE incref
    // (`{name}__clone` → gorget_shared_clone), not a deep copy. Registering the
    // clone_fn here lets `clone_fn_for_ptr` return the retain so consuming
    // positions (ctor field-init, container literal, push, return, capture)
    // auto-incref a LIVE source instead of shallow-aliasing it (the double-free
    // / under-incref class). Routed through the SINGLE writer so this ctor-path
    // def-mint stays byte-identical to the annotated-type path (map_ast_type_mut).
    // Single-owner wrappers (Mutex/RWLock/Guard) keep clone_fn=None.
    let mut td = make_wrapper_type_def(shared_type_name, inner_type, CopySemantics::Trivial, DropStrategy::Trivial(format!("{shared_type_name}__drop")));
    td.metadata.set_refcount_clone_fn(shared_type_name);
    ctx.type_registry.add_type_def(td);
}

/// Ensure a Weak[T] type has a TypeDef in the registry (Copy pointer, drop decrements weak count).
pub fn ensure_weak_type_def(ctx: &mut LoweringContext, weak_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(weak_type_name).is_some() { return; }
    // Weak is a REFCOUNT handle — its "clone" (`{name}__clone` →
    // gorget_weak_clone) is a by-VALUE weak-count incref. Routed through the
    // SINGLE writer so this ctor-path def-mint carries the same clone_fn the
    // annotated-type path (map_ast_type_mut) sets, and the consuming-position
    // gates auto-incref a LIVE Weak source instead of shallow-aliasing it.
    let mut td = make_wrapper_type_def(weak_type_name, inner_type, CopySemantics::Trivial, DropStrategy::Trivial(format!("{weak_type_name}__drop")));
    td.metadata.set_refcount_clone_fn(weak_type_name);
    ctx.type_registry.add_type_def(td);
}

/// Ensure a Mutex[T] type has a TypeDef in the registry (Copy pointer, single-owner
/// drop frees the mutex via `{name}__drop` -> `gorget_mutex_free`). Mutex keeps
/// `clone_fn = None` (single-owner, not refcounted) so `needs_param_drop` excludes
/// its borrow-param — only the owner frees.
pub fn ensure_mutex_type_def(ctx: &mut LoweringContext, mutex_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(mutex_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(mutex_type_name, inner_type, CopySemantics::Trivial, DropStrategy::Trivial(format!("{mutex_type_name}__drop")))
    );
}

/// Ensure a Guard[T] type has a TypeDef in the registry (Move value struct, drop releases mutex).
pub fn ensure_guard_type_def(ctx: &mut LoweringContext, guard_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(guard_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(guard_type_name, inner_type, CopySemantics::Resource, DropStrategy::Trivial(format!("{guard_type_name}__drop")))
    );
}

/// Ensure a RWLock[T] type has a TypeDef in the registry (Copy pointer, single-owner
/// drop frees the rwlock via `{name}__drop` -> `gorget_rwlock_free`). RWLock keeps
/// `clone_fn = None` (single-owner, not refcounted) so `needs_param_drop` excludes
/// its borrow-param — only the owner frees.
pub fn ensure_rwlock_type_def(ctx: &mut LoweringContext, rwlock_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(rwlock_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(rwlock_type_name, inner_type, CopySemantics::Trivial, DropStrategy::Trivial(format!("{rwlock_type_name}__drop")))
    );
}

/// Ensure a ReadGuard[T] / WriteGuard[T] type has a TypeDef in the registry
/// (Move value struct, drop releases the rwlock read/write lock via
/// `{name}__drop`). Identical shape to `ensure_guard_type_def` and to the
/// `monomorphize_struct` ReadGuard/WriteGuard arm (`generics/mod.rs:2384`) —
/// the value the C backend reads is the **presence of a TypeDef** (which puts
/// the name into `module.structs` → `emit_monomorphized_typedefs` emits the
/// `typedef gorget_read_guard_t {name};` from the resources table). Without a
/// TypeDef the rwlock guard slot resolves to `void*` (8 bytes) and the 16-byte
/// `gorget_rwlock_read_to` write (`*out = gorget_rwlock_read(rw)`, returning a
/// by-value `gorget_read_guard_t`; likewise `gorget_rwlock_write_to`) stack-buffer-overflows.
pub fn ensure_rwlock_guard_type_def(ctx: &mut LoweringContext, guard_type_name: &str, inner_type: TypeId) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_wrapper_type_def;
    if ctx.type_registry.get_type_def(guard_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_wrapper_type_def(guard_type_name, inner_type, CopySemantics::Resource, DropStrategy::Trivial(format!("{guard_type_name}__drop")))
    );
}

/// Ensure a Channel[T] type has a TypeDef in the registry (Copy pointer, no drop).
pub fn ensure_channel_type_def(ctx: &mut LoweringContext, channel_type_name: &str) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_opaque_type_def;
    if ctx.type_registry.get_type_def(channel_type_name).is_some() { return; }
    // Channel is a REFCOUNT handle — its "clone" (`{name}__clone` →
    // gorget_channel_retain) is a by-VALUE refcount incref. Routed through the
    // SINGLE writer so this ctor-path def-mint matches the annotated-type path
    // (map_ast_type_mut) and consuming positions auto-retain a LIVE Channel
    // source. NOTE Channel is `DropStrategy::None`, so `needs_param_drop` stays
    // false for it (that predicate's third clause excludes it) — only the
    // consuming-position axis (`is_refcount_clone_type`) admits Channel.
    let mut td = make_opaque_type_def(channel_type_name, CopySemantics::Trivial, DropStrategy::None);
    td.metadata.set_refcount_clone_fn(channel_type_name);
    ctx.type_registry.add_type_def(td);
}

/// Ensure TaskGroup has a TypeDef in the registry (Move pointer, drop waits for all children).
pub fn ensure_task_group_type_def(ctx: &mut LoweringContext, tg_type_name: &str) {
    use crate::ir::types::{CopySemantics, DropStrategy};
    use super::super::types::make_opaque_type_def;
    if ctx.type_registry.get_type_def(tg_type_name).is_some() { return; }
    ctx.type_registry.add_type_def(
        make_opaque_type_def(tg_type_name, CopySemantics::Resource, DropStrategy::Trivial("gorget_task_group_free".to_string()))
    );
}

/// If `type_name` is a Guard/ReadGuard/WriteGuard type, return (inner_c_suffix, is_read_only).

pub fn infer_operand_type_full(ctx: &LoweringContext, operand: &Operand, builder: &FunctionBuilder) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // Builder index is O(1) and authoritative for in-range LocalIds.
            // Fallback ctx scan handles closure-param sentinel IDs
            // (`LocalId(u32::MAX - i)` from closures.rs:203). See
            // `infer_operand_type_with_builder` for the rationale.
            let idx = place.local.0 as usize;
            if idx < builder.locals.len() {
                return builder.locals[idx].type_id;
            }
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
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
            Constant::Str(_) => ctx.type_mapper.owned_string_type,
            Constant::Null => UNIT_TYPE,
            Constant::Unit => UNIT_TYPE,
            Constant::SizeOf(_) => U64_TYPE,
            Constant::FuncRef(_) => UNIT_TYPE, // treated as void* at call site
            Constant::GlobalRef(name) => {
                // Look up type from global_type_names → type_mapper
                ctx.global_type_names.get(name)
                    .and_then(|tn| lookup_global_type(ctx, tn))
                    .unwrap_or(I64_TYPE)
            }
            Constant::GlobalRefPtr(name) => {
                // Pointer to global: return the base type (the C backend emits `&name`
                // so the pointer semantics are handled at the backend level).
                ctx.global_type_names.get(name)
                    .and_then(|tn| lookup_global_type(ctx, tn))
                    .unwrap_or(I64_TYPE)
            }
        },
    }
}

/// Look up a global variable's type by its type name string.
/// Tries named types first (structs/enums), then primitive type names.
pub(in crate::ir::lowering) fn lookup_global_type(ctx: &LoweringContext, type_name: &str) -> Option<TypeId> {
    ctx.type_mapper.lookup_named(type_name).or_else(|| {
        match type_name {
            "int" | "i64" => Some(I64_TYPE),
            "float" | "f64" => Some(F64_TYPE),
            "bool" => Some(BOOL_TYPE),
            "str" | "String" => Some(ctx.type_mapper.owned_string_type),
            "i8" => Some(I8_TYPE),
            "i16" => Some(I16_TYPE),
            "i32" => Some(I32_TYPE),
            "u8" => Some(U8_TYPE),
            "u16" => Some(U16_TYPE),
            "u32" => Some(U32_TYPE),
            "u64" => Some(U64_TYPE),
            "f32" => Some(F32_TYPE),
            _ => None,
        }
    })
}

/// Returns true if `local` has ANY Move-semantics type (Vector, Dict, Set,
/// GorgetString, Box, user Move structs). Used to emit MoveZero + mark_moved
/// after ownership transfer (function call args, unwrap, struct-init fields)
/// to prevent double-free of shared heap buffers.
pub(super) fn is_resource_type_local(
    local: LocalId,
    builder: &FunctionBuilder,
    registry: &TypeRegistry,
) -> bool {
    registry.is_resource_type(builder.local_type(local))
}

/// Returns true if `local` holds a SINGLE-OWNER OPAQUE HANDLE: a
/// pointer-typedef handle that HAS a drop but NO clone path.
///
/// The membership is DERIVED, never listed — but so that a reader knows the
/// shape of it, the three clauses below select exactly `Mutex[T]` and
/// `RWLock[T]` at present. Every other by-value handle is excluded for a
/// stated reason: `Shared`/`Weak`/`Channel` have a refcount clone path;
/// `Thread` and `TaskGroup` carry `Resource` copy-semantics, so
/// `is_resource_type` already covered them; and `AtomicInt`/`AtomicBool`/
/// `Barrier`/`WaitGroup`/`Semaphore`/`OnceFlag` have no drop at all, so
/// nothing can double-free.
///
/// `is_resource_type` answers `false` for these, because the handle itself is
/// a bitwise-copyable pointer (`copy_semantics: Trivial`). At a CONSUMING
/// position that answer is wrong: the collection takes THE one owner, so the
/// source must be move-zeroed or both the local and the collection slot drop
/// the same handle (measured: `Vector[Mutex[int]].push(m)` → `free(): double
/// free detected in tcache 2`, both backends).
///
/// The two predicates read are the typed ones already governing the axis:
/// `is_by_value_receiver` (the protocol table's `SelfConvention::ByValue` —
/// "this type IS a runtime handle pointer") and `is_refcount_clone_type`
/// (`TypeMetadata::set_refcount_clone_fn`'s single-writer axis). REFCOUNT
/// handles — {Shared, Weak, Channel} — are deliberately EXCLUDED: they have a
/// real clone path, the consuming position increfs, and both owners drop
/// legitimately.
pub(super) fn is_single_owner_handle_local(
    local: LocalId,
    builder: &FunctionBuilder,
    registry: &TypeRegistry,
) -> bool {
    let tid = builder.local_type(local);
    if registry.is_resource_type(tid) || registry.is_refcount_clone_type(tid) {
        return false;
    }
    if !registry.needs_drop(tid) {
        return false;
    }
    match registry.get(tid) {
        Some(crate::ir::types::GirType::Named(name)) => {
            crate::ir::lowering::builtins::is_by_value_receiver(name)
        }
        _ => false,
    }
}
