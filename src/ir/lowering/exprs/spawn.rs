//! Spawn and concurrency lowering helpers — method spawn wrappers,
//! closure spawn, blocking method call wrappers, shared token wrappers.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::Expr;
use crate::span::Spanned;

use super::super::context::{LoweringContext, ParamABI};
use super::{lower_expr, infer_operand_type_full, infer_type_name_from_operand_full, atomic_type_name_for};

fn build_method_spawn_wrapper(
    ctx: &mut LoweringContext,
    wrapper_name: &str,
    method_c_name: &str,
    recv_value_type: TypeId,
    needs_mut_borrow: bool,
    arg_types: &[TypeId],    // explicit args, excluding self
    return_type: TypeId,
) -> crate::ir::Function {
    // Params: receiver value, then explicit call args
    let all_param_types: Vec<TypeId> = std::iter::once(recv_value_type)
        .chain(arg_types.iter().copied())
        .collect();
    let params: Vec<(TypeId, Option<&str>)> = all_param_types.iter()
        .enumerate()
        .map(|(i, &tid)| (tid, if i == 0 { Some("__self") } else { None }))
        .collect();

    let mut builder = FunctionBuilder::new(wrapper_name, return_type, &params);

    // _0 = return slot, _1 = receiver value, _2.. = explicit args
    // Borrow _1 to get the self pointer
    let ptr_type = if needs_mut_borrow {
        ctx.type_registry.insert(GirType::MutPtr(recv_value_type))
    } else {
        ctx.type_registry.insert(GirType::Ptr(recv_value_type))
    };
    let ptr_local = builder.add_local(ptr_type, None);
    if needs_mut_borrow {
        builder.emit_borrow_mut(ptr_local, Place::local(LocalId(1)));
    } else {
        builder.emit_borrow(ptr_local, Place::local(LocalId(1)));
    }

    // Build call args: self pointer + explicit params
    let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
    for i in 0..arg_types.len() {
        call_args.push(FunctionBuilder::copy(LocalId((i + 2) as u32)));
    }

    // Call the method and return the result
    if return_type == UNIT_TYPE {
        builder.call_void(method_c_name, call_args);
        builder.ret(FunctionBuilder::const_unit());
    } else {
        let result = builder.call(method_c_name, call_args, return_type);
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(result));
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    }

    builder.build()
}

/// Lower `spawn receiver.method(args)` by generating a thin wrapper function
/// and dispatching through the spawn infrastructure.
pub(super) fn lower_method_spawn(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    receiver: &Spanned<Expr>,
    method_name: &str,
    args: &[Spanned<crate::parser::ast::CallArg>],
) -> Operand {
    // 1. Lower the receiver to get the value operand
    let recv = lower_expr(ctx, builder, receiver);
    let recv_type = infer_operand_type_full(ctx, &recv, builder);

    // 2. Infer the type name for method resolution
    let type_name = match infer_type_name_from_operand_full(ctx, &recv, builder) {
        Some(name) => name,
        None => {
            // Can't resolve type — fall back to blocking call
            return lower_method_call_blocking(ctx, builder, recv, method_name, args);
        }
    };

    // 3. Resolve the mangled method name (same logic as lower_method_call)
    let mangled = format!("{type_name}__{method_name}");
    let effective_name = if ctx.fn_sigs.contains_key(mangled.as_str()) {
        mangled.clone()
    } else {
        let suffix = format!("_for_{type_name}__{method_name}");
        let candidates: Vec<&String> = ctx.fn_sigs.keys()
            .filter(|k| k.ends_with(&suffix))
            .collect();
        if candidates.len() == 1 {
            candidates[0].clone()
        } else {
            mangled.clone()
        }
    };

    // 4. Get method signature from fn_sigs
    let (param_types, fn_ret_type) = match ctx.fn_sigs.get(effective_name.as_str()) {
        Some((params, ret)) => (params.clone(), *ret),
        None => {
            // Method not in fn_sigs (synthetic stdlib method) — fall back to blocking call
            return lower_method_call_blocking(ctx, builder, recv, method_name, args);
        }
    };

    // param_types[0] is the self pointer type (Ptr(T) or MutPtr(T))
    if param_types.is_empty() {
        return lower_method_call_blocking(ctx, builder, recv, method_name, args);
    }

    // 5. Determine self borrow mode
    let needs_mut = matches!(ctx.type_registry.get(param_types[0]), Some(GirType::MutPtr(_)));

    // 6. Get the receiver value type (strip pointer wrapper)
    let recv_value_type = match ctx.type_registry.get(param_types[0]) {
        Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
        _ => recv_type, // self param is not a pointer — use recv type directly
    };

    // Explicit arg types (everything after self)
    let explicit_arg_types: Vec<TypeId> = param_types[1..].to_vec();

    // 7. Build or reuse wrapper (idempotent, keyed by wrapper name)
    let wrapper_name = format!("__spawn_method_wrap_{effective_name}");

    if !ctx.spawn.fn_names.contains_key(&wrapper_name) {
        let wrapper_fn = build_method_spawn_wrapper(
            ctx,
            &wrapper_name,
            &effective_name,
            recv_value_type,
            needs_mut,
            &explicit_arg_types,
            fn_ret_type,
        );
        ctx.spawn.wrapper_fns.push(wrapper_fn);

        // Register wrapper signature: [recv_value_type, explicit_arg_types...] → fn_ret_type
        let mut sig_params = vec![recv_value_type];
        sig_params.extend_from_slice(&explicit_arg_types);
        ctx.fn_sigs.insert(wrapper_name.clone(), (sig_params, fn_ret_type));

        // Register param names
        let mut param_names = vec!["__self".to_string()];
        param_names.extend((0..explicit_arg_types.len()).map(|i| format!("__arg{i}")));
        ctx.fn_param_names.insert(wrapper_name.clone(), param_names);
    }

    // 8. Register for spawn
    ctx.spawn.pending_fn = Some(wrapper_name.clone());
    ctx.spawn.fn_names.insert(wrapper_name.clone(), true);

    // 9. Register Task type (same boilerplate as direct-fn spawn).
    let ret_c = ctx.type_name_for_id(fn_ret_type)
        .unwrap_or("int64_t")
        .to_string();
    let task_name = if fn_ret_type == UNIT_TYPE {
        "Task__void".to_string()
    } else {
        format!("Task__{ret_c}")
    };
    let task_type = if let Some(tid) = ctx.type_mapper.lookup_named(&task_name) {
        tid
    } else {
        ctx.type_registry.add_type_def(TypeDef {
            name: task_name.clone(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial(format!("{task_name}__drop")),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let tid = ctx.type_registry.insert(GirType::Named(task_name.clone()));
        ctx.type_mapper.register_named(task_name.clone(), tid);
        tid
    };

    // Register task type → fn mapping for await dispatch on indexed tasks
    ctx.spawn.register_task_type_fn(task_type, wrapper_name.clone());

    // 10. Build spawn args: receiver value + explicit args
    let recv_local = match &recv {
        Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
            Some(place.local)
        }
        _ => None,
    };
    let mut spawn_args = vec![recv];
    for arg in args {
        spawn_args.push(lower_expr(ctx, builder, &arg.node.value));
    }

    // 11. Emit spawn call
    let spawn_fn = format!("__gorget_spawn_{wrapper_name}");
    let dst = builder.call(&spawn_fn, spawn_args, task_type);

    // 12. Zero receiver to transfer ownership (prevents double-free for Move types)
    if let Some(local_id) = recv_local {
        builder.move_zero(Place::local(local_id));
        ctx.drops.mark_moved(local_id);
    }

    FunctionBuilder::copy(dst)
}

/// Fallback: lower a method call as a blocking (non-spawned) call.
/// Used when method resolution fails in the spawn path.
fn lower_method_call_blocking(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    recv: Operand,
    method_name: &str,
    args: &[Spanned<crate::parser::ast::CallArg>],
) -> Operand {
    // Re-create the MethodCall expression and lower it normally.
    // We already have the lowered receiver, so we build the self ptr + call directly.
    let recv_type = infer_operand_type_full(ctx, &recv, builder);
    let type_name = infer_type_name_from_operand_full(ctx, &recv, builder)
        .unwrap_or_else(|| "unknown".to_string());
    let mangled = format!("{type_name}__{method_name}");

    // Borrow receiver for self
    let mut call_args = Vec::new();
    if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
        let pt = ctx.register_ptr_type(recv_type);
        let pl = builder.add_local(pt, None);
        builder.emit_borrow(pl, place.clone());
        call_args.push(FunctionBuilder::copy(pl));
    } else {
        call_args.push(recv);
    }
    for arg in args {
        call_args.push(lower_expr(ctx, builder, &arg.node.value));
    }

    let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
        mangled.clone()
    } else {
        let suffix = format!("_for_{type_name}__{method_name}");
        ctx.fn_sigs.keys()
            .find(|k| k.ends_with(&suffix))
            .cloned()
            .unwrap_or(mangled.clone())
    };

    let ret_type = ctx.fn_sigs.get(effective_name.as_str())
        .map(|(_, r)| *r)
        .unwrap_or(I64_TYPE);

    if ret_type == UNIT_TYPE {
        builder.call_void(effective_name, call_args);
        Operand::Constant(Constant::Unit)
    } else {
        let dst = builder.call(effective_name, call_args, ret_type);
        FunctionBuilder::copy(dst)
    }
}

// ─── Closure Spawn Codegen ─────────────────────────────────────────────────

/// Build a thin wrapper function `__spawn_wrap_<struct_name>` that accepts
/// flat capture arguments + call arguments, reconstructs the closure struct,
/// and calls `__Closure_N__call(&struct, args...)`.
///
/// This wrapper is treated as a regular spawned function by the C backend,
/// so `emit_spawn_helpers` generates the context struct, thread function,
/// and spawn/await helpers automatically.
fn build_spawn_wrapper(
    ctx: &mut LoweringContext,
    wrapper_name: &str,
    call_fn_name: &str,
    captures: &[(String, TypeId, u32)],
    call_param_types: &[TypeId],
    return_type: TypeId,
    struct_name: &str,
    struct_type_id: TypeId,
) -> crate::ir::Function {
    // Build parameter slice: capture types first, then call arg types.
    // We use None for names — they're internal implementation details.
    let all_param_types: Vec<TypeId> = captures.iter()
        .map(|(_, tid, _)| *tid)
        .chain(call_param_types.iter().copied())
        .collect();
    let params: Vec<(TypeId, Option<&str>)> = all_param_types.iter()
        .map(|&tid| (tid, None))
        .collect();

    let mut builder = FunctionBuilder::new(wrapper_name, return_type, &params);

    // _0 = return slot, _1.._n_cap = capture params, _n_cap+1.. = call arg params.
    let n_cap = captures.len();

    // Tag each capture param as Owned: the caller transfers ownership of each
    // capture into the spawn wrapper (the spawn mechanism passes these by value).
    // Without this tagging, params start as Untracked, and the validator fires
    // `StructInit … untracked source consumed` at the struct_init below.
    for i in 0..n_cap {
        let local = LocalId((i + 1) as u32);
        let idx = local.0 as usize;
        if idx < builder.locals.len() {
            builder.locals[idx].ownership = crate::ir::LocalOwnership::Owned;
        }
    }

    // Build field operands from capture params (_1.._n_cap).
    let field_operands: Vec<Operand> = (0..n_cap)
        .map(|i| FunctionBuilder::copy(LocalId((i + 1) as u32)))
        .collect();

    // Reconstruct the closure struct.
    let struct_local = builder.struct_init(struct_name, struct_type_id, field_operands);

    // Take an immutable reference to the struct (env pointer for __call).
    let env_ptr_type = ctx.type_registry.insert(GirType::Ptr(struct_type_id));
    let env_ptr_local = builder.add_local(env_ptr_type, None);
    builder.emit_borrow(env_ptr_local, Place::local(struct_local));

    // Build call args: env pointer + explicit call params.
    // Wrap resource-type args in Ptr borrows where the __call function expects Ptr ABI.
    let mut call_args = vec![FunctionBuilder::copy(env_ptr_local)];
    let call_arg_start = (n_cap + 1) as u32; // skip _0 (return) + n_cap capture params
    let abis = ctx.fn_param_abis.get(call_fn_name).cloned();
    for i in 0..call_param_types.len() {
        let arg_local = LocalId(call_arg_start + i as u32);
        let needs_ptr = abis.as_ref()
            .and_then(|a| a.get(i + 1)) // +1 for env ptr
            .map(|a| matches!(a, ParamABI::ByPtr | ParamABI::ByMutPtr))
            .unwrap_or(false);
        if needs_ptr {
            let ptr_type = ctx.register_ptr_type(call_param_types[i]);
            let ptr_local = builder.add_local(ptr_type, None);
            builder.emit_borrow(ptr_local, Place::local(arg_local));
            call_args.push(FunctionBuilder::copy(ptr_local));
        } else {
            call_args.push(FunctionBuilder::copy(arg_local));
        }
    }

    // Emit the call to __Closure_N__call and return its result.
    if return_type == UNIT_TYPE {
        builder.call_void(call_fn_name, call_args);
        builder.ret(FunctionBuilder::const_unit());
    } else {
        let result = builder.call(call_fn_name, call_args, return_type);
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(result));
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    }

    builder.build()
}

/// Lower `spawn c(args)` or `spawn ((): body)(args)` where `c` is a closure variable.
///
/// Generates (or reuses) a thin wrapper function that reconstructs the closure
/// struct from flat parameters, then emits a call to `__gorget_spawn_<wrapper>`.
/// The Task local is registered in `ctx.spawn.result_locals` for await dispatch.
pub(super) fn lower_closure_spawn(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    closure_local: LocalId,
    struct_name: &str,
    call_fn_name: &str,
    // The closure ENVIRONMENT struct's own `TypeId` — the key CARRIER #1 is
    // read under, so it is also the local's type. Was passed twice (once
    // unused) before the sidecar was retired.
    struct_type_id: TypeId,
    captures: &[(String, TypeId, u32)],
    call_args: &[Spanned<crate::parser::ast::CallArg>],
) -> Operand {
    // Get the closure call function's return type and param types.
    let (call_param_types, fn_ret_type) = ctx.fn_sigs.get(call_fn_name)
        .map(|(params, ret)| {
            // Skip the first param (env pointer).
            let call_params: Vec<TypeId> = params.iter().skip(1).copied().collect();
            (call_params, *ret)
        })
        .unwrap_or_else(|| (vec![], I64_TYPE));

    let wrapper_name = format!("__spawn_wrap_{struct_name}");

    // Emit the wrapper function once (idempotent — skip if already registered).
    if !ctx.spawn.fn_names.contains_key(&wrapper_name) {
        let wrapper_fn = build_spawn_wrapper(
            ctx,
            &wrapper_name,
            call_fn_name,
            captures,
            &call_param_types,
            fn_ret_type,
            struct_name,
            struct_type_id,
        );
        ctx.spawn.wrapper_fns.push(wrapper_fn);

        // Register the wrapper's signature so the module assembly loop can
        // populate module.spawned_fns for the C backend.
        let mut sig_params = captures.iter().map(|(_, tid, _)| *tid).collect::<Vec<_>>();
        sig_params.extend_from_slice(&call_param_types);
        ctx.fn_sigs.insert(wrapper_name.clone(), (sig_params.clone(), fn_ret_type));
        let param_names: Vec<String> = captures.iter().map(|(n, _, _)| n.clone())
            .chain((0..call_param_types.len()).map(|i| format!("__arg{i}")))
            .collect();
        ctx.fn_param_names.insert(wrapper_name.clone(), param_names);
    }

    ctx.spawn.pending_fn = Some(wrapper_name.clone());
    ctx.spawn.fn_names.insert(wrapper_name.clone(), true);

    // Register the Task type (same logic as direct-fn spawn).
    let ret_c = ctx.type_name_for_id(fn_ret_type)
        .unwrap_or("int64_t")
        .to_string();
    let task_name = if fn_ret_type == UNIT_TYPE {
        "Task__void".to_string()
    } else {
        format!("Task__{ret_c}")
    };
    let task_type = if let Some(tid) = ctx.type_mapper.lookup_named(&task_name) {
        tid
    } else {
        ctx.type_registry.add_type_def(TypeDef {
            name: task_name.clone(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial(format!("{task_name}__drop")),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let tid = ctx.type_registry.insert(GirType::Named(task_name.clone()));
        ctx.type_mapper.register_named(task_name.clone(), tid);
        tid
    };

    // Register task type → fn mapping for await dispatch on indexed tasks
    ctx.spawn.register_task_type_fn(task_type, wrapper_name.clone());

    // Extract capture field values from the closure struct using actual field indices.
    // The field_index is the position in the struct (which may differ from the
    // position in this captures list when ByMutRef captures are filtered out).
    //
    // Phase C FieldLoad migration (2026-05-06): the spawned task takes
    // ownership of the captures (the closure_local is dead at this site —
    // last use, immediately consumed by the spawn). For drop-tracked
    // captures, emit a `field_load + move_zero` pair so the GIR-level
    // Move semantic is explicit (the validator's next-inst peek
    // recognises this pattern and skips the shallow-copy warning).
    // Non-droppable captures (primitives) bit-copy is sound.
    //
    // Cluster 3 widening (2026-05-07): the gate is `needs_drop` (via
    // is_resource_or_contains_resource), not the narrow is_resource_type.
    // `Option[String]` / `Result[Vector, _]` / struct-with-resource-field
    // captures carry heap pointers transitively — without MoveZero, the
    // closure_local's drop at scope-exit and the spawn-arg's drop in the
    // task body race on the same heap allocation. Cluster 1's
    // ensure_option_type_registered upgrade fix made `needs_drop` reliable
    // on late-registered Option/Result types. (No fixture currently
    // exercises an Option/Result-of-resource capture flowing through
    // spawn — the widening is correctness-driven for future code.)
    let mut spawn_args: Vec<Operand> = Vec::new();
    for (_, cap_type, field_index) in captures {
        let field_val = builder.field_load(Place::local(closure_local), *field_index, *cap_type);
        if ctx.type_registry.is_resource_or_contains_resource(*cap_type) {
            builder.move_zero(Place {
                local: closure_local,
                projections: vec![Projection::Field(*field_index)],
            });
        }
        spawn_args.push(FunctionBuilder::copy(field_val));
    }

    // Lower and append explicit call arguments.
    for arg in call_args {
        spawn_args.push(lower_expr(ctx, builder, &arg.node.value));
    }

    let spawn_fn = format!("__gorget_spawn_{wrapper_name}");
    let dst = builder.call(&spawn_fn, spawn_args, task_type);
    FunctionBuilder::copy(dst)
}

// ─── Token Semantics: Shared Spawn Wrappers ────────────────────────────────

/// Information about a shared argument at a spawn site.
/// Collected during spawn lowering, used to build the token wrapper.
pub struct SharedSpawnArg {
    /// Index of this arg in the callee's parameter list.
    pub arg_index: usize,
    /// The kind of sync primitive wrapping the shared variable.
    pub kind: super::super::context::SharedLocalKind,
    /// The inner (user-visible) type of the shared variable.
    pub inner_type: TypeId,
    /// The wrapper type (Mutex__T, Shared__T, AtomicInt, RWLock__T).
    pub wrapper_type: TypeId,
    /// Whether the callee's parameter expects a mutable borrow (&T).
    pub is_mutable: bool,
    /// Declaration order key (facade LocalId) for deadlock prevention.
    /// Tokens are acquired in ascending decl_order and released in reverse.
    pub decl_order: u32,
}

/// Build a token wrapper function for spawning with shared variables.
///
/// Token semantics: the wrapper acquires tokens (locks) for all shared args,
/// calls the original function with plain types, then releases all tokens.
/// The lock is held for the entire function call — the synchronous execution
/// region is the spawned function body.
///
/// For mutable borrow params: the wrapper holds the lock and passes a pointer
/// to the guarded value (so the callee can mutate through &T).
/// For immutable params: the wrapper gets a copy under the lock, then calls
/// the callee with the copy (lock still held for consistency).
pub fn build_shared_token_wrapper(
    ctx: &mut LoweringContext,
    wrapper_name: &str,
    callee_name: &str,
    callee_param_types: &[TypeId],
    shared_args: &[SharedSpawnArg],
    return_type: TypeId,
) -> crate::ir::Function {
    use super::super::context::SharedLocalKind;

    // Wrapper params: for shared args, use the wrapper type; for non-shared, use the callee's type.
    let wrapper_param_types: Vec<TypeId> = callee_param_types.iter().enumerate()
        .map(|(i, &callee_type)| {
            if let Some(sa) = shared_args.iter().find(|sa| sa.arg_index == i) {
                sa.wrapper_type
            } else {
                callee_type
            }
        })
        .collect();

    let params: Vec<(TypeId, Option<&str>)> = wrapper_param_types.iter()
        .map(|&tid| (tid, None))
        .collect();

    let mut builder = FunctionBuilder::new(wrapper_name, return_type, &params);

    // _0 = return slot, _1.. = params
    //
    // Deadlock prevention (§3.3): acquire tokens in ascending declaration order
    // (by decl_order / facade LocalId), release in reverse. This ensures all
    // tasks acquire the same set of locks in the same order.

    // Phase 1: Acquire tokens in declaration order.
    // Sort shared args by decl_order for acquisition ordering.
    let mut sorted_shared: Vec<&SharedSpawnArg> = shared_args.iter().collect();
    sorted_shared.sort_by_key(|sa| sa.decl_order);

    // Maps arg_index → call operand (filled during acquisition, consumed during call_args assembly)
    let mut shared_call_operands: std::collections::HashMap<usize, Operand> = std::collections::HashMap::new();
    // Guard pointers with their decl_order for ordered release
    let mut guard_ptrs_ordered: Vec<(u32, LocalId)> = Vec::new();

    for sa in &sorted_shared {
        let param_local = LocalId((sa.arg_index + 1) as u32);

        match sa.kind {
            SharedLocalKind::Mutex => {
                let inner_c = ctx.c_type_name_for_id(sa.inner_type);
                let guard_mangled = format!("Guard__{inner_c}");
                let mutex_mangled = format!("Mutex__{inner_c}");
                let mutex_type = ctx.type_mapper.lookup_named(&mutex_mangled)
                    .unwrap_or(sa.inner_type);
                let guard_type = ctx.type_mapper.lookup_named(&guard_mangled)
                    .unwrap_or(sa.inner_type);

                // Unwrap Shared[Mutex[T]] → Mutex[T]
                let shared_mutex_mangled = format!("Shared__{mutex_mangled}");
                let shared_get_fn = format!("{shared_mutex_mangled}__get");
                let mutex_val = builder.call(
                    &shared_get_fn,
                    vec![FunctionBuilder::copy(param_local)],
                    mutex_type,
                );

                // Acquire token: lock the mutex
                let guard = builder.call(
                    &format!("{mutex_mangled}__lock"),
                    vec![FunctionBuilder::copy(mutex_val)],
                    guard_type,
                );
                let guard_ptr_type = ctx.register_ptr_type(guard_type);
                let guard_ptr = builder.add_local(guard_ptr_type, None);
                builder.emit_borrow(guard_ptr, Place::local(guard));
                guard_ptrs_ordered.push((sa.decl_order, guard_ptr));

                if sa.is_mutable {
                    let inner_ptr_type = ctx.register_mut_ptr_type(sa.inner_type);
                    let inner_ptr = builder.call(
                        &format!("{guard_mangled}__get_ptr"),
                        vec![FunctionBuilder::copy(guard_ptr)],
                        inner_ptr_type,
                    );
                    shared_call_operands.insert(sa.arg_index, FunctionBuilder::copy(inner_ptr));
                } else {
                    let val = builder.call(
                        &format!("{guard_mangled}__get"),
                        vec![FunctionBuilder::copy(guard_ptr)],
                        sa.inner_type,
                    );
                    shared_call_operands.insert(sa.arg_index, FunctionBuilder::copy(val));
                }
            }
            SharedLocalKind::SharedArc => {
                // ARC-only: get a copy (no locking — no ordering constraint)
                let inner_c = ctx.c_type_name_for_id(sa.inner_type);
                let shared_mangled = format!("Shared__{inner_c}");
                let val = builder.call(
                    &format!("{shared_mangled}__get"),
                    vec![FunctionBuilder::copy(param_local)],
                    sa.inner_type,
                );
                shared_call_operands.insert(sa.arg_index, FunctionBuilder::copy(val));
            }
            SharedLocalKind::Atomic => {
                // Atomic: load value (lock-free — no ordering constraint)
                let atomic_name = atomic_type_name_for(sa.inner_type);
                let val = builder.call(
                    &format!("{atomic_name}__load"),
                    vec![FunctionBuilder::copy(param_local)],
                    sa.inner_type,
                );
                shared_call_operands.insert(sa.arg_index, FunctionBuilder::copy(val));
            }
            SharedLocalKind::RwLock => {
                let inner_c = ctx.c_type_name_for_id(sa.inner_type);
                let rwlock_mangled = format!("RWLock__{inner_c}");

                if sa.is_mutable {
                    let write_guard_mangled = format!("WriteGuard__{inner_c}");
                    let write_guard_type = ctx.type_mapper.lookup_named(&write_guard_mangled)
                        .unwrap_or(sa.inner_type);
                    let guard = builder.call(
                        &format!("{rwlock_mangled}__write"),
                        vec![FunctionBuilder::copy(param_local)],
                        write_guard_type,
                    );
                    let guard_ptr_type = ctx.register_ptr_type(write_guard_type);
                    let guard_ptr = builder.add_local(guard_ptr_type, None);
                    builder.emit_borrow(guard_ptr, Place::local(guard));
                    guard_ptrs_ordered.push((sa.decl_order, guard_ptr));

                    let inner_ptr_type = ctx.register_mut_ptr_type(sa.inner_type);
                    let inner_ptr = builder.call(
                        &format!("{write_guard_mangled}__get_ptr"),
                        vec![FunctionBuilder::copy(guard_ptr)],
                        inner_ptr_type,
                    );
                    shared_call_operands.insert(sa.arg_index, FunctionBuilder::copy(inner_ptr));
                } else {
                    let read_guard_mangled = format!("ReadGuard__{inner_c}");
                    let read_guard_type = ctx.type_mapper.lookup_named(&read_guard_mangled)
                        .unwrap_or(sa.inner_type);
                    let guard = builder.call(
                        &format!("{rwlock_mangled}__read"),
                        vec![FunctionBuilder::copy(param_local)],
                        read_guard_type,
                    );
                    let guard_ptr_type = ctx.register_ptr_type(read_guard_type);
                    let guard_ptr = builder.add_local(guard_ptr_type, None);
                    builder.emit_borrow(guard_ptr, Place::local(guard));
                    guard_ptrs_ordered.push((sa.decl_order, guard_ptr));

                    let val = builder.call(
                        &format!("{read_guard_mangled}__get"),
                        vec![FunctionBuilder::copy(guard_ptr)],
                        sa.inner_type,
                    );
                    shared_call_operands.insert(sa.arg_index, FunctionBuilder::copy(val));
                }
            }
        }
    }

    // Phase 2: Build call args in parameter order
    let mut call_args: Vec<Operand> = Vec::new();
    for (i, &_callee_type) in callee_param_types.iter().enumerate() {
        if let Some(operand) = shared_call_operands.remove(&i) {
            call_args.push(operand);
        } else {
            // Non-shared arg: pass through
            let param_local = LocalId((i + 1) as u32);
            call_args.push(FunctionBuilder::copy(param_local));
        }
    }

    // Call the original function (token held for the entire call — synchronous region)
    if return_type == UNIT_TYPE {
        builder.call_void(callee_name, call_args);
    } else {
        let result = builder.call(callee_name, call_args, return_type);
        builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(result));
    }

    // Phase 3: Release tokens in reverse declaration order
    guard_ptrs_ordered.sort_by_key(|(order, _)| *order);
    for &(_, guard_ptr) in guard_ptrs_ordered.iter().rev() {
        let guard_type_id = match ctx.type_registry.get(builder.local_type(guard_ptr)) {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
            _ => UNIT_TYPE,
        };
        let guard_type_name = ctx.type_name_for_id(guard_type_id)
            .unwrap_or("Guard__int64_t")
            .to_string();
        let release_fn = format!("{guard_type_name}__drop");
        builder.call(&release_fn, vec![FunctionBuilder::copy(guard_ptr)], UNIT_TYPE);
    }

    // Return
    if return_type == UNIT_TYPE {
        builder.ret(FunctionBuilder::const_unit());
    } else {
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    }

    builder.build()
}

