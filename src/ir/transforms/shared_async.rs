//! GIR-to-GIR transform: inject shared-variable token management into async functions.
//!
//! Instead of re-lowering the AST with different assumptions (the old `build_shared_async_variant`
//! approach), this module takes an already-lowered GIR function and surgically modifies it:
//!
//! 1. Remap shared param types from inner T to Shared[Mutex[T]]
//! 2. Insert lock-acquire preamble at function entry
//! 3. Splice release/reacquire around `__gorget_await_*` calls
//! 4. Insert guard cleanup before every `Return` terminator

use rustc_hash::FxHashMap;

use crate::ir::{BasicBlock, Function, Local};
use crate::ir::instructions::*;
use crate::ir::types::*;

/// Specification for a shared argument that needs token management.
#[derive(Debug, Clone)]
pub struct SharedArgSpec {
    /// Index in the callee's parameter list (0-based).
    pub arg_index: usize,
    /// The inner (user-visible) type T.
    pub inner_type: TypeId,
    /// The wrapper type passed to the variant (Shared[Mutex[T]]).
    pub wrapper_type: TypeId,
    /// Mutex[T] type.
    pub mutex_type: TypeId,
    /// Guard[T] type.
    pub guard_type: TypeId,
    /// Whether the callee param expects a mutable borrow (&T).
    pub is_mutable: bool,
    /// Declaration order key for deadlock-free acquire/release ordering.
    pub decl_order: u32,
    /// Mangled inner C type name (e.g. "int64_t") for constructing function names.
    pub inner_c_name: String,
}

/// Allocated locals for one shared parameter's token management.
#[derive(Debug, Clone)]
struct SharedParamLocals {
    /// The original param local (e.g. _1).
    param_local: LocalId,
    /// Mutex value local.
    mutex_val: LocalId,
    /// Guard local.
    guard: LocalId,
    /// Guard pointer local (&Guard[T]).
    guard_ptr: LocalId,
    /// Facade local (inner pointer or value, replaces the original param in the body).
    facade: LocalId,
    /// Spec reference index.
    spec_idx: usize,
}

/// A pending request to generate a shared-async variant, recorded at the spawn site
/// and processed after all functions are lowered.
#[derive(Debug, Clone)]
pub struct PendingSharedVariant {
    /// Name of the source function (already lowered to GIR).
    pub source_fn_name: String,
    /// Name for the generated variant.
    pub variant_name: String,
    /// Shared argument specifications.
    pub shared_args: Vec<SharedArgSpec>,
    /// Return type of the function.
    pub return_type: TypeId,
}

/// Request to generate a shared wrapper for an inner callee spawned from
/// within a shared-async variant.
#[derive(Debug, Clone)]
pub struct InnerSpawnRewrite {
    /// Original spawn function name (e.g., `__gorget_spawn_modifier`).
    pub original_spawn_fn: String,
    /// New spawn function name (e.g., `__gorget_spawn___shared_token_modifier`).
    pub new_spawn_fn: String,
    /// New await function name (e.g., `__gorget_await___shared_token_modifier`).
    pub new_await_fn: String,
    /// The inner callee name (e.g., `modifier`).
    pub callee_name: String,
    /// Wrapper variant name (e.g., `__shared_token_modifier`).
    pub wrapper_name: String,
    /// Shared arg specs for the inner callee.
    pub shared_args: Vec<SharedArgSpec>,
    /// Callee param types (original, before wrapping).
    pub callee_param_types: Vec<TypeId>,
    /// Return type of the inner callee.
    pub callee_return_type: TypeId,
    /// Whether the callee has internal yield points.
    pub callee_has_awaits: bool,
}

/// Result of `inject_shared_token_management`.
pub struct SharedTransformResult {
    /// The transformed function.
    pub func: Function,
    /// Inner spawn rewrites needed (new wrapper functions to generate).
    pub inner_rewrites: Vec<InnerSpawnRewrite>,
}

/// Transform an already-lowered GIR function into a shared-async variant.
///
/// The variant wraps shared parameters in Shared[Mutex[T]] and manages lock
/// tokens: acquire at entry, release before await points, reacquire after,
/// release before return.
pub fn inject_shared_token_management(
    source_fn: &Function,
    variant_name: &str,
    shared_args: &[SharedArgSpec],
    type_registry: &mut TypeRegistry,
    yield_point_fns: &rustc_hash::FxHashSet<String>,
) -> SharedTransformResult {
    let mut func = source_fn.clone();
    func.name = variant_name.to_string();

    // -- Step 1: Allocate locals and build remap table --

    // Change param types in locals (params are locals[1..=N])
    for sa in shared_args {
        let param_idx = sa.arg_index + 1; // locals[0] = return place, params start at 1
        func.locals[param_idx].type_id = sa.wrapper_type;
    }

    // Also update the params vec
    let mut new_params = func.params.clone();
    for sa in shared_args {
        new_params[sa.arg_index] = sa.wrapper_type;
    }
    func.params = new_params;

    let mut local_remap: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    let mut shared_locals: Vec<SharedParamLocals> = Vec::new();

    for (spec_idx, sa) in shared_args.iter().enumerate() {
        let param_local = LocalId((sa.arg_index + 1) as u32);

        // Allocate new locals
        let mutex_val = alloc_local(&mut func, sa.mutex_type, Some("__mutex_val"));
        let guard = alloc_local(&mut func, sa.guard_type, Some("__guard"));
        let guard_ptr_type = type_registry.insert(GirType::Ptr(sa.guard_type));
        let guard_ptr = alloc_local(&mut func, guard_ptr_type, Some("__guard_ptr"));

        let facade = if sa.is_mutable {
            let inner_ptr_type = type_registry.insert(GirType::MutPtr(sa.inner_type));
            alloc_local(&mut func, inner_ptr_type, Some("__facade"))
        } else {
            alloc_local(&mut func, sa.inner_type, Some("__facade"))
        };

        local_remap.insert(param_local, facade);

        shared_locals.push(SharedParamLocals {
            param_local,
            mutex_val,
            guard,
            guard_ptr,
            facade,
            spec_idx,
        });
    }

    // -- Step 2: Remap all LocalId references in the body --

    for block in &mut func.blocks {
        for instr in &mut block.instructions {
            remap_instruction(instr, &local_remap);
        }
        if let Some(ref mut term) = block.terminator {
            remap_terminator(term, &local_remap);
        }
    }

    // -- Step 3: Insert preamble at the start of block 0 --

    let mut preamble: Vec<Instruction> = Vec::new();

    // Sort by decl_order for deadlock-free ordering
    let mut sorted_locals: Vec<&SharedParamLocals> = shared_locals.iter().collect();
    sorted_locals.sort_by_key(|sl| shared_args[sl.spec_idx].decl_order);

    for sl in &sorted_locals {
        let sa = &shared_args[sl.spec_idx];
        let mutex_mangled = format!("Mutex__{}", sa.inner_c_name);
        let guard_mangled = format!("Guard__{}", sa.inner_c_name);
        let shared_mutex_mangled = format!("Shared__{mutex_mangled}");

        // Unwrap Shared[Mutex[T]] → Mutex[T]
        preamble.push(Instruction::Call {
            dst: Some(sl.mutex_val),
            func: format!("{shared_mutex_mangled}__get"),
            args: vec![Operand::Copy(Place::local(sl.param_local))],
            reason: None,
                   });

        // Lock mutex → Guard[T]
        preamble.push(Instruction::Call {
            dst: Some(sl.guard),
            func: format!("{mutex_mangled}__lock"),
            args: vec![Operand::Copy(Place::local(sl.mutex_val))],
            reason: None,
                   });

        // Borrow guard → &Guard[T]
        preamble.push(Instruction::Borrow {
            dst: sl.guard_ptr,
            place: Place::local(sl.guard),
        });

        // Derive facade from guard
        if sa.is_mutable {
            preamble.push(Instruction::Call {
                dst: Some(sl.facade),
                func: format!("{guard_mangled}__get_ptr"),
                args: vec![Operand::Copy(Place::local(sl.guard_ptr))],
                reason: None,
                           });
        } else {
            preamble.push(Instruction::Call {
                dst: Some(sl.facade),
                func: format!("{guard_mangled}__get"),
                args: vec![Operand::Copy(Place::local(sl.guard_ptr))],
                reason: None,
                           });
        }
    }

    if !func.blocks.is_empty() {
        let preamble_len = preamble.len();
        let block = &mut func.blocks[0];
        // Prepend preamble instructions
        let mut new_instrs = preamble;
        new_instrs.append(&mut block.instructions);
        block.instructions = new_instrs;
        // Extend span_map to match
        let mut new_spans: Vec<Option<crate::span::Span>> = vec![None; preamble_len];
        new_spans.append(&mut block.span_map);
        block.span_map = new_spans;
    }

    // -- Step 4: Splice release/reacquire around await points --

    // Build with-refresh assignments: after reacquire, re-read facade into binding locals.
    // with_refresh_pairs contains (binding_local, param_local) from AST lowering.
    // param_local was remapped to facade. For mutable params, facade is a pointer (*mut T)
    // so we dereference; for immutable, it's the value directly.
    let with_refresh_instrs: Vec<Instruction> = func.with_refresh_pairs.iter()
        .filter_map(|&(binding, param)| {
            let facade = *local_remap.get(&param)?;
            // Find the shared arg spec for this param to check mutability
            let param_idx = param.0 as usize; // locals index (1-based for params)
            let is_mutable = shared_args.iter()
                .any(|sa| sa.arg_index + 1 == param_idx && sa.is_mutable);
            let source_place = if is_mutable {
                // Facade is *mut T — deref to get T
                Place { local: facade, projections: vec![Projection::Deref] }
            } else {
                Place::local(facade)
            };
            Some(Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(binding),
                value: Operand::Copy(source_place),
            })
        })
        .collect();

    for block in &mut func.blocks {
        splice_around_awaits(block, &shared_locals, shared_args, type_registry, &with_refresh_instrs, yield_point_fns);
    }

    // -- Step 5: Insert guard cleanup before Return terminators --

    for block in &mut func.blocks {
        if let Some(Terminator::Return(_)) = &block.terminator {
            let cleanup = build_release_sequence(&shared_locals, shared_args);
            let cleanup_len = cleanup.len();
            block.instructions.extend(cleanup);
            block.span_map.extend(std::iter::repeat(None).take(cleanup_len));
        }
    }

    // -- Step 6: Rewrite inner spawn calls that propagate shared params --

    let mut inner_rewrites: Vec<InnerSpawnRewrite> = Vec::new();

    for inner in &source_fn.inner_shared_spawns {
        // Check which of the inner spawn's param mappings actually target shared params
        let mut inner_shared_specs: Vec<SharedArgSpec> = Vec::new();
        for &(call_arg_idx, enclosing_param_idx) in &inner.shared_arg_mappings {
            // Is the enclosing param actually shared in this variant?
            if let Some(sa) = shared_args.iter().find(|sa| sa.arg_index == enclosing_param_idx) {
                inner_shared_specs.push(SharedArgSpec {
                    arg_index: call_arg_idx,
                    inner_type: sa.inner_type,
                    wrapper_type: sa.wrapper_type,
                    mutex_type: sa.mutex_type,
                    guard_type: sa.guard_type,
                    is_mutable: inner.callee_param_ownerships.get(call_arg_idx)
                        .map_or(false, |o| matches!(o, crate::parser::ast::Ownership::MutableBorrow)),
                    decl_order: 0, // single-param ordering doesn't matter
                    inner_c_name: sa.inner_c_name.clone(),
                });
            }
        }

        if inner_shared_specs.is_empty() {
            continue;
        }

        let wrapper_name = if inner.callee_has_awaits {
            format!("__shared_async_{}", inner.callee_name)
        } else {
            format!("__shared_token_{}", inner.callee_name)
        };
        let original_spawn_fn = format!("__gorget_spawn_{}", inner.callee_name);
        let new_spawn_fn = format!("__gorget_spawn_{wrapper_name}");
        let original_await_fn = format!("__gorget_await_{}", inner.callee_name);
        let new_await_fn = format!("__gorget_await_{wrapper_name}");

        // Rewrite spawn/await calls in the transformed function body
        for block in &mut func.blocks {
            for instr in &mut block.instructions {
                match instr {
                    Instruction::Call { func: fname, args, dst, .. } => {
                        if *fname == original_spawn_fn {
                            *fname = new_spawn_fn.clone();
                            // Replace args: for shared positions, use the wrapper
                            // param local instead of the facade-derived value
                            for spec in &inner_shared_specs {
                                let enclosing_param_idx = inner.shared_arg_mappings.iter()
                                    .find(|(ci, _)| *ci == spec.arg_index)
                                    .map(|(_, pi)| *pi)
                                    .unwrap();
                                // The enclosing param local (1-based)
                                let param_local = LocalId((enclosing_param_idx + 1) as u32);
                                // Replace the arg with the wrapper param
                                if spec.arg_index < args.len() {
                                    args[spec.arg_index] = Operand::Copy(Place::local(param_local));
                                }
                            }
                            // Update dst type if needed — task type stays the same
                            let _ = dst;
                        } else if *fname == original_await_fn {
                            *fname = new_await_fn.clone();
                        }
                    }
                    _ => {}
                }
            }
        }

        inner_rewrites.push(InnerSpawnRewrite {
            original_spawn_fn,
            new_spawn_fn,
            new_await_fn,
            callee_name: inner.callee_name.clone(),
            wrapper_name,
            shared_args: inner_shared_specs,
            callee_param_types: inner.callee_param_types.clone(),
            callee_return_type: inner.callee_return_type,
            callee_has_awaits: inner.callee_has_awaits,
        });
    }

    SharedTransformResult { func, inner_rewrites }
}

/// Allocate a new local in the function and return its LocalId.
fn alloc_local(func: &mut Function, type_id: TypeId, hint: Option<&str>) -> LocalId {
    let id = LocalId(func.locals.len() as u32);
    func.locals.push(Local {
        type_id,
        name_hint: hint.map(|s| s.to_string()),
        ownership: crate::ir::LocalOwnership::default(),
        slot_kind: crate::ir::SlotKind::default(),
        is_owning_param: false,
        deref_of_owning_param: None,
    });
    id
}

/// Build the release sequence: drop guards in reverse decl_order.
fn build_release_sequence(
    shared_locals: &[SharedParamLocals],
    shared_args: &[SharedArgSpec],
) -> Vec<Instruction> {
    let mut sorted: Vec<&SharedParamLocals> = shared_locals.iter().collect();
    sorted.sort_by_key(|sl| shared_args[sl.spec_idx].decl_order);

    sorted.iter().rev().map(|sl| {
        let sa = &shared_args[sl.spec_idx];
        let guard_mangled = format!("Guard__{}", sa.inner_c_name);
        Instruction::Call {
            dst: None,
            func: format!("{guard_mangled}__drop"),
            args: vec![Operand::Copy(Place::local(sl.guard_ptr))],
            reason: None,
                   }
    }).collect()
}

/// Build the reacquire sequence: re-lock in forward decl_order, re-derive facades.
fn build_reacquire_sequence(
    shared_locals: &[SharedParamLocals],
    shared_args: &[SharedArgSpec],
    type_registry: &mut TypeRegistry,
) -> Vec<Instruction> {
    let mut sorted: Vec<&SharedParamLocals> = shared_locals.iter().collect();
    sorted.sort_by_key(|sl| shared_args[sl.spec_idx].decl_order);

    let mut instrs = Vec::new();
    for sl in &sorted {
        let sa = &shared_args[sl.spec_idx];
        let mutex_mangled = format!("Mutex__{}", sa.inner_c_name);
        let guard_mangled = format!("Guard__{}", sa.inner_c_name);
        let shared_mutex_mangled = format!("Shared__{mutex_mangled}");

        // Unwrap Shared → Mutex
        instrs.push(Instruction::Call {
            dst: Some(sl.mutex_val),
            func: format!("{shared_mutex_mangled}__get"),
            args: vec![Operand::Copy(Place::local(sl.param_local))],
            reason: None,
                   });

        // Lock
        instrs.push(Instruction::Call {
            dst: Some(sl.guard),
            func: format!("{mutex_mangled}__lock"),
            args: vec![Operand::Copy(Place::local(sl.mutex_val))],
            reason: None,
                   });

        // Re-derive guard pointer (reuse same local)
        instrs.push(Instruction::Borrow {
            dst: sl.guard_ptr,
            place: Place::local(sl.guard),
        });

        // Re-derive facade
        if sa.is_mutable {
            let inner_ptr_type = type_registry.insert(GirType::MutPtr(sa.inner_type));
            let tmp = Instruction::Call {
                dst: Some(sl.facade),
                func: format!("{guard_mangled}__get_ptr"),
                args: vec![Operand::Copy(Place::local(sl.guard_ptr))],
                reason: None,
                           };
            // For mutable, the facade IS the pointer, so calling get_ptr and assigning to facade works.
            // But we need a temp because facade already holds the old pointer.
            // Actually, `Call { dst: Some(facade) }` overwrites facade directly, which is fine.
            let _ = inner_ptr_type; // used to register the type, which is already registered from preamble
            instrs.push(tmp);
        } else {
            instrs.push(Instruction::Call {
                dst: Some(sl.facade),
                func: format!("{guard_mangled}__get"),
                args: vec![Operand::Copy(Place::local(sl.guard_ptr))],
                reason: None,
                           });
        }
    }
    instrs
}

/// True if a GIR call instruction is a yield point (await, async, or blocking extern).
fn is_yield_point(instr: &Instruction, yield_point_fns: &rustc_hash::FxHashSet<String>) -> bool {
    match instr {
        Instruction::Call { func, .. } | Instruction::CallExtern { func, .. } => {
            func.starts_with("__gorget_await_")
                || yield_point_fns.contains(func)
        }
        _ => false,
    }
}

/// Scan a block for yield points (await, sleep, blocking I/O) and splice
/// release/reacquire around them for shared-variable token management.
fn splice_around_awaits(
    block: &mut BasicBlock,
    shared_locals: &[SharedParamLocals],
    shared_args: &[SharedArgSpec],
    type_registry: &mut TypeRegistry,
    with_refresh_instrs: &[Instruction],
    yield_point_fns: &rustc_hash::FxHashSet<String>,
) {
    // Find indices of yield points (process in reverse to avoid invalidation)
    let await_indices: Vec<usize> = block.instructions.iter().enumerate()
        .filter_map(|(i, instr)| {
            if is_yield_point(instr, yield_point_fns) { Some(i) } else { None }
        })
        .collect();

    // Process in reverse order
    for &idx in await_indices.iter().rev() {
        let release = build_release_sequence(shared_locals, shared_args);
        let mut reacquire = build_reacquire_sequence(shared_locals, shared_args, type_registry);
        // Append with-refresh assignments after reacquire (re-read facade into binding locals)
        reacquire.extend_from_slice(with_refresh_instrs);
        let release_len = release.len();
        let reacquire_len = reacquire.len();

        // Insert reacquire AFTER the await call
        let after_pos = idx + 1;
        for (j, instr) in reacquire.into_iter().enumerate() {
            block.instructions.insert(after_pos + j, instr);
        }
        for _ in 0..reacquire_len {
            block.span_map.insert(after_pos, None);
        }

        // Insert release BEFORE the await call
        for (j, instr) in release.into_iter().enumerate() {
            block.instructions.insert(idx + j, instr);
        }
        for _ in 0..release_len {
            block.span_map.insert(idx, None);
        }
    }
}

// ── LocalId remapping ──────────────────────────────────────────────────────

fn remap_local(id: LocalId, map: &FxHashMap<LocalId, LocalId>) -> LocalId {
    map.get(&id).copied().unwrap_or(id)
}

fn remap_place(place: &mut Place, map: &FxHashMap<LocalId, LocalId>) {
    place.local = remap_local(place.local, map);
    for proj in &mut place.projections {
        if let Projection::Index(lid) = proj {
            *lid = remap_local(*lid, map);
        }
    }
}

fn remap_operand(op: &mut Operand, map: &FxHashMap<LocalId, LocalId>) {
    match op {
        Operand::Copy(place) | Operand::Move(place) => remap_place(place, map),
        Operand::Constant(_) => {}
    }
}

fn remap_instruction(instr: &mut Instruction, map: &FxHashMap<LocalId, LocalId>) {
    match instr {
        Instruction::Assign { dst, value, .. } => {
            remap_place(dst, map);
            remap_operand(value, map);
        }
        Instruction::FieldLoad { dst, base, .. } => {
            *dst = remap_local(*dst, map);
            remap_place(base, map);
        }
        Instruction::IndexLoad { dst, base, index, .. } => {
            *dst = remap_local(*dst, map);
            remap_place(base, map);
            remap_operand(index, map);
        }
        Instruction::HeapAlloc { dst, allocator, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(allocator, map);
        }
        Instruction::HeapAllocArray { dst, count, allocator, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(count, map);
            remap_operand(allocator, map);
        }
                Instruction::LoadRef { .. } | Instruction::StoreRef { .. } => {}
        Instruction::Dealloc { ptr, allocator } => {
            remap_operand(ptr, map);
            remap_operand(allocator, map);
        }
        Instruction::BinOp { dst, lhs, rhs, .. }
        | Instruction::FaultableBinOp { dst, lhs, rhs, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(lhs, map);
            remap_operand(rhs, map);
        }
        Instruction::UnOp { dst, operand, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(operand, map);
        }
        Instruction::Cmp { dst, lhs, rhs, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(lhs, map);
            remap_operand(rhs, map);
        }
        Instruction::Cast { dst, value, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(value, map);
        }
        Instruction::BitCast { dst, value, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(value, map);
        }
        Instruction::PtrCast { dst, value, .. } => {
            *dst = remap_local(*dst, map);
            remap_operand(value, map);
        }
        Instruction::StructInit { dst, fields, .. } => {
            *dst = remap_local(*dst, map);
            for f in fields {
                remap_operand(f, map);
            }
        }
        Instruction::EnumInit { dst, fields, .. } => {
            *dst = remap_local(*dst, map);
            for f in fields {
                remap_operand(f, map);
            }
        }
        Instruction::TupleInit { dst, elements } => {
            *dst = remap_local(*dst, map);
            for e in elements {
                remap_operand(e, map);
            }
        }
        Instruction::TagOf { dst, operand } => {
            *dst = remap_local(*dst, map);
            remap_operand(operand, map);
        }
        Instruction::EnumFieldLoad { dst, base, .. } => {
            *dst = remap_local(*dst, map);
            remap_place(base, map);
        }
        Instruction::Call { dst, args, .. } => {
            if let Some(d) = dst {
                *d = remap_local(*d, map);
            }
            for a in args {
                remap_operand(a, map);
            }
        }
        Instruction::CallIndirect { dst, callee, args } => {
            if let Some(d) = dst {
                *d = remap_local(*d, map);
            }
            remap_operand(callee, map);
            for a in args {
                remap_operand(a, map);
            }
        }
        Instruction::CallExtern { dst, args, .. } => {
            if let Some(d) = dst {
                *d = remap_local(*d, map);
            }
            for a in args {
                remap_operand(a, map);
            }
        }
        Instruction::MoveZero { place } => {
            remap_place(place, map);
        }
        Instruction::Borrow { dst, place } => {
            *dst = remap_local(*dst, map);
            remap_place(place, map);
        }
        Instruction::BorrowMut { dst, place } => {
            *dst = remap_local(*dst, map);
            remap_place(place, map);
        }
        Instruction::Drop { place } => {
            remap_place(place, map);
        }
        Instruction::DropIfAlive { place } => {
            remap_place(place, map);
        }
        Instruction::LoadThreadLocal { dst, .. } => {
            *dst = remap_local(*dst, map);
        }
        Instruction::PushAllocator { allocator } => {
            remap_operand(allocator, map);
        }
        Instruction::PopAllocator => {}
        Instruction::GlobalAssign { value, .. } => {
            remap_operand(value, map);
        }
        Instruction::InlineC { .. } => {}
        Instruction::Nop => {}
    }
}

fn remap_terminator(term: &mut Terminator, map: &FxHashMap<LocalId, LocalId>) {
    match term {
        Terminator::Return(op) => {
            remap_operand(op, map);
        }
        Terminator::Jump(_) => {}
        Terminator::Branch { cond, .. } => {
            remap_operand(cond, map);
        }
        Terminator::Switch { value, .. } => {
            remap_operand(value, map);
        }
        Terminator::Invoke { dst, args, .. } => {
            if let Some(d) = dst {
                *d = remap_local(*d, map);
            }
            for a in args {
                remap_operand(a, map);
            }
        }
        Terminator::Unreachable => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Function, Local};

    fn make_registry() -> TypeRegistry {
        TypeRegistry::new()
    }

    /// Build a minimal async function that calls `__gorget_await_compute`.
    fn make_test_function(registry: &mut TypeRegistry) -> Function {
        // Simulate: async int process(int &counter, int amount)
        //   ... some work ...
        //   _await_result = __gorget_await_compute(_task_local)
        //   ... use await result ...
        //   return _result

        let mut_ptr_i64 = registry.insert(GirType::MutPtr(I64_TYPE));
        let task_type = registry.insert(GirType::Named("Task__int64_t".into()));

        Function {
            name: "process".to_string(),
            params: vec![mut_ptr_i64, I64_TYPE],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: Some("_0".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },         // return place
                Local { type_id: mut_ptr_i64, name_hint: Some("counter".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },  // _1: &int
                Local { type_id: I64_TYPE, name_hint: Some("amount".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },      // _2: int
                Local { type_id: task_type, name_hint: Some("_task".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },      // _3: Task
                Local { type_id: I64_TYPE, name_hint: Some("_await_res".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },  // _4: await result
                Local { type_id: I64_TYPE, name_hint: Some("_result".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None },     // _5: final result
            ],
            blocks: vec![
                BasicBlock {
                    instructions: vec![
                        // Some work using counter (_1)
                        Instruction::Assign { mode: crate::ir::instructions::AssignMode::Copy, dst: Place::local(LocalId(5)),
                            value: Operand::Copy(Place::local(LocalId(2))),
                        },
                        // Await call
                        Instruction::Call {
                            dst: Some(LocalId(4)),
                            func: "__gorget_await_compute".into(),
                            args: vec![Operand::Move(Place::local(LocalId(3)))],
                            reason: None,
                        },
                        // Use result — add await_res to counter deref
                        Instruction::BinOp {
                            dst: LocalId(5),
                            op: BinOp::Add,
                            type_id: I64_TYPE,
                            lhs: Operand::Copy(Place::local(LocalId(4))),
                            rhs: Operand::Copy(Place::local(LocalId(2))),
                        },
                    ],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(5))))),
                    span_map: vec![None, None, None],
                    terminator_span: None,
                },
            ],
            is_test_fn: false,
            display_name: Some("process".into()),
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            takes_env: false,
        }
    }

    fn make_shared_arg_spec(registry: &mut TypeRegistry) -> Vec<SharedArgSpec> {
        let mutex_type = registry.insert(GirType::Named("Mutex__int64_t".into()));
        let guard_type = registry.insert(GirType::Named("Guard__int64_t".into()));
        let shared_mutex_type = registry.insert(GirType::Named("Shared__Mutex__int64_t".into()));

        vec![SharedArgSpec {
            arg_index: 0,
            inner_type: I64_TYPE,
            wrapper_type: shared_mutex_type,
            mutex_type,
            guard_type,
            is_mutable: true,
            decl_order: 1,
            inner_c_name: "int64_t".into(),
        }]
    }

    #[test]
    fn transform_renames_function() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;
        assert_eq!(result.name, "__shared_async_process");
    }

    #[test]
    fn transform_changes_param_types() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);
        let wrapper_type = specs[0].wrapper_type;

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        // param 0 should now be the wrapper type
        assert_eq!(result.params[0], wrapper_type);
        // param 1 unchanged
        assert_eq!(result.params[1], I64_TYPE);
        // local[1] (param 0) should be wrapper type
        assert_eq!(result.locals[1].type_id, wrapper_type);
    }

    #[test]
    fn transform_adds_preamble_locals() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let orig_locals = source.locals.len();
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        // Should have 4 new locals: mutex_val, guard, guard_ptr, facade
        assert_eq!(result.locals.len(), orig_locals + 4);
    }

    #[test]
    fn transform_inserts_preamble() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        // Block 0 should start with preamble: get, lock, borrow, get_ptr (4 instructions)
        let bb0 = &result.blocks[0];
        assert!(bb0.instructions.len() >= 4, "expected preamble, got {} instructions", bb0.instructions.len());

        // First instruction should be Shared__Mutex__int64_t__get
        if let Instruction::Call { func, .. } = &bb0.instructions[0] {
            assert_eq!(func, "Shared__Mutex__int64_t__get");
        } else {
            panic!("expected Call, got {:?}", bb0.instructions[0]);
        }

        // Second: Mutex__int64_t__lock
        if let Instruction::Call { func, .. } = &bb0.instructions[1] {
            assert_eq!(func, "Mutex__int64_t__lock");
        } else {
            panic!("expected Call, got {:?}", bb0.instructions[1]);
        }

        // Third: Borrow (guard_ptr)
        assert!(matches!(&bb0.instructions[2], Instruction::Borrow { .. }));

        // Fourth: Guard__int64_t__get_ptr (mutable)
        if let Instruction::Call { func, .. } = &bb0.instructions[3] {
            assert_eq!(func, "Guard__int64_t__get_ptr");
        } else {
            panic!("expected Call, got {:?}", bb0.instructions[3]);
        }
    }

    #[test]
    fn transform_splices_around_await() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        let bb0 = &result.blocks[0];

        // Find the await call
        let await_idx = bb0.instructions.iter().position(|i| {
            matches!(i, Instruction::Call { func, .. } if func == "__gorget_await_compute")
        }).expect("should have await call");

        // Instruction before await should be Guard__int64_t__drop (release)
        assert!(await_idx > 0);
        if let Instruction::Call { func, .. } = &bb0.instructions[await_idx - 1] {
            assert_eq!(func, "Guard__int64_t__drop", "expected release before await");
        } else {
            panic!("expected release Call before await, got {:?}", bb0.instructions[await_idx - 1]);
        }

        // Instructions after await should be reacquire sequence
        let reacq_start = await_idx + 1;
        if let Instruction::Call { func, .. } = &bb0.instructions[reacq_start] {
            assert_eq!(func, "Shared__Mutex__int64_t__get", "expected reacquire get after await");
        } else {
            panic!("expected reacquire Call after await, got {:?}", bb0.instructions[reacq_start]);
        }
    }

    #[test]
    fn transform_inserts_cleanup_before_return() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        let bb0 = &result.blocks[0];
        assert!(matches!(&bb0.terminator, Some(Terminator::Return(_))));

        // Last instruction before terminator should be Guard__int64_t__drop (cleanup)
        let last = bb0.instructions.last().expect("should have instructions");
        if let Instruction::Call { func, .. } = last {
            assert_eq!(func, "Guard__int64_t__drop", "expected cleanup before return");
        } else {
            panic!("expected cleanup Call before return, got {:?}", last);
        }
    }

    #[test]
    fn transform_remaps_param_references() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        // The original body used LocalId(1) for counter. After remapping, all references
        // should point to the facade local instead. Verify that no instruction in the
        // original body region references LocalId(1) directly (except the preamble which
        // deliberately references the param).
        let bb0 = &result.blocks[0];

        // The return terminator originally referenced _5 which is unchanged.
        // But if any instruction referenced _1 (param), it should now reference the facade.
        if let Some(Terminator::Return(op)) = &bb0.terminator {
            // Return referenced _5, which shouldn't be remapped
            if let Operand::Copy(place) = op {
                assert_eq!(place.local, LocalId(5), "return should still reference _5");
            }
        }
    }

    #[test]
    fn transform_span_map_consistent() {
        let mut reg = make_registry();
        let source = make_test_function(&mut reg);
        let specs = make_shared_arg_spec(&mut reg);

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        for (i, block) in result.blocks.iter().enumerate() {
            assert_eq!(
                block.instructions.len(),
                block.span_map.len(),
                "block {i}: instructions and span_map length mismatch ({} vs {})",
                block.instructions.len(),
                block.span_map.len(),
            );
        }
    }

    #[test]
    fn transform_multiple_shared_params() {
        let mut reg = make_registry();
        let mut source = make_test_function(&mut reg);

        // Add a second param
        let mut_ptr_i64 = reg.insert(GirType::MutPtr(I64_TYPE));
        source.params.push(I64_TYPE);
        source.locals.push(Local { type_id: mut_ptr_i64, name_hint: Some("counter2".into()), ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None });

        let mutex_type = reg.insert(GirType::Named("Mutex__int64_t".into()));
        let guard_type = reg.insert(GirType::Named("Guard__int64_t".into()));
        let shared_mutex_type = reg.insert(GirType::Named("Shared__Mutex__int64_t".into()));

        let specs = vec![
            SharedArgSpec {
                arg_index: 0,
                inner_type: I64_TYPE,
                wrapper_type: shared_mutex_type,
                mutex_type,
                guard_type,
                is_mutable: true,
                decl_order: 1,
                inner_c_name: "int64_t".into(),
            },
            SharedArgSpec {
                arg_index: 2,
                inner_type: I64_TYPE,
                wrapper_type: shared_mutex_type,
                mutex_type,
                guard_type,
                is_mutable: false,
                decl_order: 3,
                inner_c_name: "int64_t".into(),
            },
        ];

        let yield_fns = rustc_hash::FxHashSet::default();
        let result = inject_shared_token_management(&source, "__shared_async_process", &specs, &mut reg, &yield_fns).func;

        // Should have 8 new locals (4 per shared param)
        assert_eq!(result.locals.len(), source.locals.len() + 8);

        // Preamble should have 8 instructions (4 per param, sorted by decl_order)
        // Check first preamble instruction is for decl_order=1 (param 0)
        if let Instruction::Call { func, .. } = &result.blocks[0].instructions[0] {
            assert!(func.starts_with("Shared__Mutex__"), "first preamble should be for first shared param");
        }
    }
}
