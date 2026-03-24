//! Shared variable, atomic, mutex, and rwlock IR emission helpers.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;

use super::super::context::LoweringContext;

/// Emit Shared.get() → Mutex, then lock+get+release on a Shared[Mutex[T]] hidden local.
/// Returns an operand holding the inner value T.
pub fn emit_shared_mutex_lock_get(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    shared_mutex_local: LocalId,
    mutex_type: TypeId,
    inner_type: TypeId,
) -> Operand {
    let mutex_c = ctx.type_name_for_id(mutex_type)
        .unwrap_or("Mutex__int64_t")
        .to_string();
    let shared_mangled = format!("Shared__{mutex_c}");
    let get_fn = format!("{shared_mangled}__get");
    let mutex_val = builder.call(&get_fn, vec![FunctionBuilder::copy(shared_mutex_local)], mutex_type);
    // Now lock+get+release the extracted Mutex
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let guard_mangled = format!("Guard__{inner_c}");
    let guard_type = ctx.type_mapper.lookup_named(&guard_mangled)
        .unwrap_or(inner_type);
    let lock_fn = format!("{mutex_c}__lock");
    let guard = builder.call(&lock_fn, vec![FunctionBuilder::copy(mutex_val)], guard_type);
    let guard_ptr_type = ctx.register_ptr_type(guard_type);
    let guard_ptr = builder.add_local(guard_ptr_type, None);
    builder.emit_borrow(guard_ptr, Place::local(guard));
    let get_val_fn = format!("{guard_mangled}__get");
    let val = builder.call(&get_val_fn, vec![FunctionBuilder::copy(guard_ptr)], inner_type);
    let release_fn = format!("{guard_mangled}__drop");
    builder.call(&release_fn, vec![FunctionBuilder::copy(guard_ptr)], UNIT_TYPE);
    FunctionBuilder::copy(val)
}

/// Emit Shared.get() → Mutex, then lock+set+release on a Shared[Mutex[T]] hidden local.
pub fn emit_shared_mutex_lock_set(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    shared_mutex_local: LocalId,
    mutex_type: TypeId,
    inner_type: TypeId,
    value: Operand,
) {
    let mutex_c = ctx.type_name_for_id(mutex_type)
        .unwrap_or("Mutex__int64_t")
        .to_string();
    let shared_mangled = format!("Shared__{mutex_c}");
    let get_fn = format!("{shared_mangled}__get");
    let mutex_val = builder.call(&get_fn, vec![FunctionBuilder::copy(shared_mutex_local)], mutex_type);
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let guard_mangled = format!("Guard__{inner_c}");
    let guard_type = ctx.type_mapper.lookup_named(&guard_mangled)
        .unwrap_or(inner_type);
    let lock_fn = format!("{mutex_c}__lock");
    let guard = builder.call(&lock_fn, vec![FunctionBuilder::copy(mutex_val)], guard_type);
    let guard_ptr_type = ctx.register_ptr_type(guard_type);
    let guard_ptr = builder.add_local(guard_ptr_type, None);
    builder.emit_borrow(guard_ptr, Place::local(guard));
    let set_fn = format!("{guard_mangled}__set");
    builder.call(&set_fn, vec![FunctionBuilder::copy(guard_ptr), value], UNIT_TYPE);
    let release_fn = format!("{guard_mangled}__drop");
    builder.call(&release_fn, vec![FunctionBuilder::copy(guard_ptr)], UNIT_TYPE);
}

/// Emit a simple `.get()` on a Shared[T] local (no locking needed — ARC-only).
pub fn emit_shared_get(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    shared_local: LocalId,
    inner_type: TypeId,
) -> Operand {
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let shared_mangled = format!("Shared__{inner_c}");
    let get_fn = format!("{shared_mangled}__get");
    let val = builder.call(&get_fn, vec![FunctionBuilder::copy(shared_local)], inner_type);
    FunctionBuilder::copy(val)
}

/// Emit an atomic load on a shared(atomic) variable's hidden AtomicInt/AtomicBool local.
pub fn emit_atomic_load(
    _ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    atomic_local: LocalId,
    inner_type: TypeId,
    atomic_type_name: &str,
) -> Operand {
    let load_fn = format!("{atomic_type_name}__load");
    let val = builder.call(&load_fn, vec![FunctionBuilder::copy(atomic_local)], inner_type);
    FunctionBuilder::copy(val)
}

/// Emit an atomic store on a shared(atomic) variable's hidden AtomicInt/AtomicBool local.
pub fn emit_atomic_store(
    _ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    atomic_local: LocalId,
    value: Operand,
    atomic_type_name: &str,
) {
    let store_fn = format!("{atomic_type_name}__store");
    builder.call(&store_fn, vec![FunctionBuilder::copy(atomic_local), value], UNIT_TYPE);
}

/// Emit read-lock+get+release on a shared(rwlock) variable's hidden RWLock local.
/// Returns an operand holding the inner value. The read lock is released immediately.
pub fn emit_rwlock_read_get(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    rwlock_local: LocalId,
    inner_type: TypeId,
) -> Operand {
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let rwlock_mangled = format!("RWLock__{inner_c}");
    let read_guard_mangled = format!("ReadGuard__{inner_c}");
    let read_guard_type = ctx.type_mapper.lookup_named(&read_guard_mangled)
        .unwrap_or(inner_type);
    let read_fn = format!("{rwlock_mangled}__read");
    let get_fn = format!("{read_guard_mangled}__get");
    let release_fn = format!("{read_guard_mangled}__drop");
    let guard = builder.call(&read_fn, vec![FunctionBuilder::copy(rwlock_local)], read_guard_type);
    let guard_ptr_type = ctx.register_ptr_type(read_guard_type);
    let guard_ptr = builder.add_local(guard_ptr_type, None);
    builder.emit_borrow(guard_ptr, Place::local(guard));
    let val = builder.call(&get_fn, vec![FunctionBuilder::copy(guard_ptr)], inner_type);
    builder.call(&release_fn, vec![FunctionBuilder::copy(guard_ptr)], UNIT_TYPE);
    FunctionBuilder::copy(val)
}

/// Emit write-lock+set+release on a shared(rwlock) variable's hidden RWLock local.
pub fn emit_rwlock_write_set(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    rwlock_local: LocalId,
    inner_type: TypeId,
    value: Operand,
) {
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let rwlock_mangled = format!("RWLock__{inner_c}");
    let write_guard_mangled = format!("WriteGuard__{inner_c}");
    let write_guard_type = ctx.type_mapper.lookup_named(&write_guard_mangled)
        .unwrap_or(inner_type);
    let write_fn = format!("{rwlock_mangled}__write");
    let set_fn = format!("{write_guard_mangled}__set");
    let release_fn = format!("{write_guard_mangled}__drop");
    let guard = builder.call(&write_fn, vec![FunctionBuilder::copy(rwlock_local)], write_guard_type);
    let guard_ptr_type = ctx.register_ptr_type(write_guard_type);
    let guard_ptr = builder.add_local(guard_ptr_type, None);
    builder.emit_borrow(guard_ptr, Place::local(guard));
    builder.call(&set_fn, vec![FunctionBuilder::copy(guard_ptr), value], UNIT_TYPE);
    builder.call(&release_fn, vec![FunctionBuilder::copy(guard_ptr)], UNIT_TYPE);
}

/// Emit write-lock+get on a shared(rwlock) variable's hidden RWLock local.
/// Returns (guard_ptr local, current value operand). Caller must call emit_rwlock_write_finish
/// after computing the new value and setting it.
pub fn emit_rwlock_write_get(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    rwlock_local: LocalId,
    inner_type: TypeId,
) -> (LocalId, Operand) {
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let rwlock_mangled = format!("RWLock__{inner_c}");
    let write_guard_mangled = format!("WriteGuard__{inner_c}");
    let write_guard_type = ctx.type_mapper.lookup_named(&write_guard_mangled)
        .unwrap_or(inner_type);
    let write_fn = format!("{rwlock_mangled}__write");
    let get_fn = format!("{write_guard_mangled}__get");
    let guard = builder.call(&write_fn, vec![FunctionBuilder::copy(rwlock_local)], write_guard_type);
    let guard_ptr_type = ctx.register_ptr_type(write_guard_type);
    let guard_ptr = builder.add_local(guard_ptr_type, None);
    builder.emit_borrow(guard_ptr, Place::local(guard));
    let val = builder.call(&get_fn, vec![FunctionBuilder::copy(guard_ptr)], inner_type);
    (guard_ptr, FunctionBuilder::copy(val))
}

/// Finish a write-lock compound assign: set the new value and release the write guard.
pub fn emit_rwlock_write_finish(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    guard_ptr: LocalId,
    inner_type: TypeId,
    new_value: Operand,
) {
    let inner_c = ctx.c_type_name_for_id(inner_type);
    let write_guard_mangled = format!("WriteGuard__{inner_c}");
    let set_fn = format!("{write_guard_mangled}__set");
    let release_fn = format!("{write_guard_mangled}__drop");
    builder.call(&set_fn, vec![FunctionBuilder::copy(guard_ptr), new_value], UNIT_TYPE);
    builder.call(&release_fn, vec![FunctionBuilder::copy(guard_ptr)], UNIT_TYPE);
}

/// Map inner GIR type to the atomic wrapper type name.
pub fn atomic_type_name_for(inner_type: TypeId) -> String {
    if inner_type == BOOL_TYPE {
        "AtomicBool".to_string()
    } else {
        "AtomicInt".to_string()
    }
}

pub fn guard_inner_suffix(type_name: &str) -> Option<(&str, bool)> {
    if let Some(suffix) = type_name.strip_prefix("Guard__") {
        Some((suffix, false))
    } else if let Some(suffix) = type_name.strip_prefix("ReadGuard__") {
        Some((suffix, true))
    } else if let Some(suffix) = type_name.strip_prefix("WriteGuard__") {
        Some((suffix, false))
    } else {
        None
    }
}

/// Emit a call to `Guard__T__get_ptr` (or ReadGuard/WriteGuard variant).
/// Returns `(inner_ptr_local, inner_type_id)` where the local has type `MutPtr(inner_type)`.
pub fn emit_guard_get_ptr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    guard_place: &Place,
    guard_type_id: TypeId,
    guard_type_name: &str,
    inner_suffix: &str,
) -> (LocalId, TypeId) {
    let inner_type = c_suffix_to_type_id(inner_suffix, ctx);
    // Borrow guard mutably for the get_ptr call
    let guard_ptr_type = ctx.register_mut_ptr_type(guard_type_id);
    let guard_ptr = builder.add_local(guard_ptr_type, None);
    builder.emit_borrow_mut(guard_ptr, guard_place.clone());
    // Call get_ptr → returns T* (MutPtr(inner_type))
    let inner_ptr_type = ctx.register_mut_ptr_type(inner_type);
    let get_ptr_fn = format!("{guard_type_name}__get_ptr");
    let inner_ptr_local = builder.call(
        &get_ptr_fn,
        vec![Operand::Copy(Place::local(guard_ptr))],
        inner_ptr_type,
    );
    (inner_ptr_local, inner_type)
}

/// Map a C type suffix (e.g. "bool", "int64_t", "double") to a GIR TypeId.
/// Used when the elem type of a generic container is stored as a C name rather than a Gorget name.
pub(super) fn c_suffix_to_type_id(suffix: &str, ctx: &LoweringContext) -> TypeId {
    match suffix {
        "bool"    => BOOL_TYPE,
        "double" | "float64_t" => F64_TYPE,
        "float"  => F64_TYPE,
        "int64_t" | "int" | "long long" => I64_TYPE,
        "int32_t" => I32_TYPE,
        "int8_t"  => I8_TYPE,
        "GorgetStringView" => ctx.type_mapper.str_type,
        other     => ctx.type_mapper.lookup_named(other).unwrap_or(I64_TYPE),
    }
}

/// Auto-refresh all active `with shared_var:` bindings after an await point.
/// For each (binding_local, shared_facade_local) pair, re-reads the shared variable
/// and assigns the fresh value to the binding local.
pub fn emit_with_shared_refresh(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    use super::super::context::SharedLocalKind;

    if ctx.with_shared_refresh.is_empty() {
        return;
    }

    // Snapshot to avoid borrow issues
    let refresh_pairs: Vec<_> = ctx.with_shared_refresh.clone();
    for (binding_local, facade_local) in refresh_pairs {
        let info = match ctx.shared.locals.get(&facade_local) {
            Some(info) => (info.hidden_local, info.inner_type, info.kind),
            None => continue,
        };
        let (hidden_local, inner_type, kind) = info;

        let fresh = match kind {
            SharedLocalKind::SharedArc => emit_shared_get(ctx, builder, hidden_local, inner_type),
            SharedLocalKind::Atomic => {
                let atomic_name = atomic_type_name_for(inner_type);
                emit_atomic_load(ctx, builder, hidden_local, inner_type, &atomic_name)
            }
            SharedLocalKind::Mutex => {
                let inner_c = ctx.c_type_name_for_id(inner_type);
                let mutex_type = ctx.type_mapper.lookup_named(&format!("Mutex__{inner_c}")).unwrap_or(inner_type);
                emit_shared_mutex_lock_get(ctx, builder, hidden_local, mutex_type, inner_type)
            }
            SharedLocalKind::RwLock => emit_rwlock_read_get(ctx, builder, hidden_local, inner_type),
        };
        builder.assign(Place::local(binding_local), fresh);
    }
}
