//! Function call lowering, argument resolution, and print/string interpolation.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr, Ownership};
use crate::parser::Parser;
use crate::span::Spanned;

use super::super::context::{LoweringContext, ParamABI};
use super::{lower_expr, infer_operand_type_full, is_resource_type_local,
            ensure_box_type_def, ensure_mutex_type_def, ensure_shared_type_def,
            ensure_task_group_type_def,
            resolve_option_result_variant, lower_string_interpolation};

pub(super) fn lower_call_arg(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    arg: &Spanned<ast::CallArg>,
    callee_param_type: Option<TypeId>,
    callee_name: &str,
    arg_idx: usize,
) -> Operand {
    // Look up the unified ParamABI (single source of truth when available).
    let abi = ctx.fn_param_abis.get(callee_name)
        .and_then(|abis| abis.get(arg_idx))
        .copied();

    // Whether the callee's parameter is a Move type (passed by pointer).
    // Use ParamABI when available, fall back to type-based derivation for extern/runtime fns.
    let callee_is_move_param = match abi {
        Some(abi) => matches!(abi, ParamABI::ByPtr | ParamABI::ByMutPtr),
        None => callee_param_type.map(|pt| ctx.type_registry.is_resource_type(pt)).unwrap_or(false),
    };

    // The callee expects a pointer for this param.
    let callee_passes_by_ptr = match abi {
        Some(abi) => abi != ParamABI::ByValue,
        None => {
            let callee_param_ownership = ctx.fn_param_ownerships.get(callee_name)
                .and_then(|ownerships| ownerships.get(arg_idx))
                .copied();
            let callee_param_is_mut_borrow = callee_param_ownership
                .map(|o| matches!(o, Ownership::MutableBorrow))
                .unwrap_or(false);
            callee_is_move_param || callee_param_is_mut_borrow
        }
    };

    // Special case: &name where name is already a pass-by-pointer param.
    // Skip the auto-deref that Identifier would do — just forward the pointer.
    // Only forward when the call site explicitly has & — bare args must not
    // silently forward a MutPtr.
    if matches!(arg.node.ownership, Ownership::MutableBorrow) {
        if let Expr::Identifier(name) = &arg.node.value.node {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                let is_already_ptr = {
                    let lid = local_id.0 as usize;
                    lid < builder.locals.len() && matches!(
                        ctx.type_registry.get(builder.locals[lid].type_id),
                        Some(GirType::MutPtr(_)) | Some(GirType::Ptr(_))
                    )
                };
                if ctx.ref_locals.contains(&local_id)
                    || ctx.mut_capture_locals.contains_key(&local_id)
                    || is_already_ptr
                {
                    return FunctionBuilder::copy(local_id);
                }
            }
        }
    }
    let val = lower_expr(ctx, builder, &arg.node.value);
    match arg.node.ownership {
        Ownership::MutableBorrow => {
            // GlobalRef → GlobalRefPtr: emit &global_name directly.
            if let Operand::Constant(Constant::GlobalRef(name)) = &val {
                return Operand::Constant(Constant::GlobalRefPtr(name.clone()));
            }
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let local_type = if (place.local.0 as usize) < builder.locals.len() {
                    builder.locals[place.local.0 as usize].type_id
                } else {
                    UNIT_TYPE
                };
                let ptr_type = ctx.register_mut_ptr_type(local_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
            val
        }
        Ownership::Borrow if callee_passes_by_ptr => {
            // Bare call-site: emit const Ptr by default.
            // Exception: when the callee's param ownership is Move (e.g., generic functions
            // that return a Move-type parameter directly), use MutPtr to transfer ownership.
            let callee_param_ownership = ctx.fn_param_ownerships.get(callee_name)
                .and_then(|ownerships| ownerships.get(arg_idx))
                .copied();
            let use_mut_ptr = matches!(callee_param_ownership, Some(Ownership::Move));
            // GlobalRef → GlobalRefPtr: emit &global_name directly.
            if let Operand::Constant(Constant::GlobalRef(name)) = &val {
                return Operand::Constant(Constant::GlobalRefPtr(name.clone()));
            }
            // For Copy/Move operands of plain locals, borrow in place.
            // For constants or complex expressions, materialize into a temp first.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.locals[place.local.0 as usize].type_id;
                    // Already a Ptr (borrowed resource param) — forward directly,
                    // don't wrap in another Ptr layer.
                    if matches!(ctx.type_registry.get(local_type), Some(GirType::Ptr(_))) {
                        return FunctionBuilder::copy(place.local);
                    }
                    if use_mut_ptr {
                        let ptr_type = ctx.register_mut_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow_mut(dst, place.clone());
                        // Mark source as moved — callee takes ownership
                        ctx.drops.mark_moved(place.local);
                        return FunctionBuilder::copy(dst);
                    } else {
                        let ptr_type = ctx.register_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow(dst, place.clone());
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // Materialize non-place values (constants, call results) into a temp local
            if let Some(pt) = callee_param_type {
                let tmp = builder.add_local(pt, None);
                builder.assign(Place::local(tmp), val);
                if use_mut_ptr {
                    let ptr_type = ctx.register_mut_ptr_type(pt);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(dst, Place::local(tmp));
                    return FunctionBuilder::copy(dst);
                } else {
                    let ptr_type = ctx.register_ptr_type(pt);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow(dst, Place::local(tmp));
                    return FunctionBuilder::copy(dst);
                }
            }
            Operand::Constant(Constant::Unit) // unreachable: callee_passes_by_ptr implies callee_param_type.is_some()
        }
        Ownership::Move if callee_is_move_param => {
            // If the operand is Ptr(T) (borrowed ref), auto-clone to create
            // an owned value before moving to the callee.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.locals[place.local.0 as usize].type_id;
                    if let Some(inner) = ctx.pointee_type(local_type) {
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                            let cloned = builder.call(&clone_fn, vec![FunctionBuilder::copy(place.local)], inner);
                            let ptr_type = ctx.register_mut_ptr_type(inner);
                            let dst = builder.add_local(ptr_type, None);
                            builder.emit_borrow_mut(dst, Place::local(cloned));
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }
            }
            // Move of a Move-type value: callee expects MutPtr. Emit borrow_mut.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.locals[place.local.0 as usize].type_id;
                    let ptr_type = ctx.register_mut_ptr_type(local_type);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(dst, place.clone());
                    // Mark the source as moved in the caller
                    ctx.drops.mark_moved(place.local);
                    return FunctionBuilder::copy(dst);
                }
            }
            // Materialize non-place values into a temp
            if let Some(pt) = callee_param_type {
                let tmp = builder.add_local(pt, None);
                builder.assign(Place::local(tmp), val);
                let ptr_type = ctx.register_mut_ptr_type(pt);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow_mut(dst, Place::local(tmp));
                return FunctionBuilder::copy(dst);
            }
            val
        }
        _ => val, // Borrow-of-Copy or Move-of-Copy: pass by value
    }
}

/// Resolve named arguments and default parameter values for a function call.
/// Reorders named args to match parameter order and fills in defaults for missing params.
fn resolve_call_args<'a>(
    ctx: &LoweringContext,
    fn_name: &str,
    args: &'a [Spanned<ast::CallArg>],
) -> Vec<Spanned<ast::CallArg>> {
    let param_names = match ctx.fn_param_names.get(fn_name) {
        Some(names) => names,
        None => return args.to_vec(), // no param info → pass through unchanged
    };

    let has_named = args.iter().any(|a| a.node.name.is_some());
    let has_defaults = ctx.fn_defaults.contains_key(fn_name);

    if !has_named && !has_defaults {
        return args.to_vec();
    }
    if !has_named && args.len() >= param_names.len() {
        return args.to_vec(); // all params supplied positionally, no reorder needed
    }

    // Build a slot array matching parameter order
    let mut slots: Vec<Option<Spanned<ast::CallArg>>> = vec![None; param_names.len()];

    // Place positional args first
    let mut positional_idx = 0;
    for arg in args {
        if arg.node.name.is_some() {
            // Named arg — place by name
            let arg_name = arg.node.name.as_ref().unwrap().node.as_str();
            if let Some(pos) = param_names.iter().position(|p| p == arg_name) {
                slots[pos] = Some(arg.clone());
            }
        } else {
            // Positional — skip past already-filled slots from named args
            while positional_idx < slots.len() && slots[positional_idx].is_some() {
                positional_idx += 1;
            }
            if positional_idx < slots.len() {
                slots[positional_idx] = Some(arg.clone());
                positional_idx += 1;
            }
        }
    }

    // Fill in defaults for any remaining empty slots
    if let Some(defaults) = ctx.fn_defaults.get(fn_name) {
        for (param_idx, default_expr) in defaults {
            if *param_idx < slots.len() && slots[*param_idx].is_none() {
                slots[*param_idx] = Some(Spanned::dummy(ast::CallArg {
                    name: None,
                    ownership: ast::Ownership::Borrow,
                    value: Spanned::dummy(default_expr.clone()),
                }));
            }
        }
    }

    // Collect filled slots (skip any remaining None — shouldn't happen for valid code)
    slots.into_iter().flatten().collect()
}

/// Lower a function call.
pub(super) fn lower_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    callee: &Spanned<Expr>,
    args: &[Spanned<ast::CallArg>],
    generic_args: Option<&[Spanned<ast::Type>]>,
) -> Operand {
    if let Expr::Identifier(name) = &callee.node {
        if name == "print" {
            lower_print_call(ctx, builder, args);
            return Operand::Constant(Constant::Unit);
        }

        // Box(value) constructor → heap allocation via __gorget_box_alloc
        if (name == "Box" || name.starts_with("Box__")) && args.len() == 1 {
            let val_op = lower_expr(ctx, builder, &args[0].node.value);
            let val_type = infer_operand_type_full(ctx, &val_op, builder);
            let inner_c = if let Some(rest) = name.strip_prefix("Box__") {
                rest.to_string()
            } else {
                ctx.type_name_for_id(val_type)
                    .unwrap_or("int64_t")
                    .to_string()
            };
            let box_mangled = format!("Box__{inner_c}");
            let box_type = if let Some(tid) = ctx.type_mapper.lookup_named(&box_mangled) {
                tid
            } else {
                let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(box_mangled.clone()));
                ctx.type_mapper.register_named(box_mangled.clone(), tid);
                ensure_box_type_def(ctx, &box_mangled, val_type);
                tid
            };
            let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
            let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
            return FunctionBuilder::copy(dst);
        }

        // String("hello") constructor → gorget_string_from_str
        // String(capacity) constructor → gorget_string_with_capacity
        if name == "String" {
            if args.len() == 1 {
                let arg_op = lower_expr(ctx, builder, &args[0].node.value);
                let owned_type = ctx.type_mapper.owned_string_type;
                let arg_type = infer_operand_type_full(ctx, &arg_op, builder);
                let fn_name = if arg_type == I64_TYPE || arg_type == I32_TYPE {
                    "gorget_string_with_capacity"
                } else {
                    "gorget_string_from_str"
                };
                let dst = builder.call_extern(fn_name, vec![arg_op], owned_type);
                return FunctionBuilder::copy(dst);
            } else if args.is_empty() {
                let owned_type = ctx.type_mapper.owned_string_type;
                let dst = builder.call_extern(
                    "gorget_string_from_str",
                    vec![Operand::Constant(Constant::Str(String::new()))],
                    owned_type,
                );
                return FunctionBuilder::copy(dst);
            }
        }

        // format("...") → string interpolation or gorget_string_from_str
        if name == "format" && args.len() == 1 {
            if let Expr::StringLiteral(lit) = &args[0].node.value.node {
                if lit.segments.iter().any(|s| matches!(s, StringSegment::Interpolation(_, _))) {
                    return lower_string_interpolation(ctx, builder, lit);
                } else {
                    // Plain string literal → gorget_string_from_str(str_literal)
                    let str_op = lower_expr(ctx, builder, &args[0].node.value);
                    let owned_type = ctx.type_mapper.owned_string_type;
                    let dst = builder.call_extern("gorget_string_from_str", vec![str_op], owned_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Allocator constructors → runtime functions
        if name == "Arena" && args.len() == 1 {
            let cap_op = lower_expr(ctx, builder, &args[0].node.value);
            let arena_type = ctx.type_mapper.lookup_named("Arena").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_arena_new", vec![cap_op], arena_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "TrackingAllocator" && args.is_empty() {
            let ta_type = ctx.type_mapper.lookup_named("TrackingAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_tracking_new", vec![], ta_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "PoolAllocator" && args.len() == 2 {
            let a1 = lower_expr(ctx, builder, &args[0].node.value);
            let a2 = lower_expr(ctx, builder, &args[1].node.value);
            let pool_type = ctx.type_mapper.lookup_named("PoolAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_pool_new", vec![a1, a2], pool_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "TlsfAllocator" && args.len() == 1 {
            let a1 = lower_expr(ctx, builder, &args[0].node.value);
            let tlsf_type = ctx.type_mapper.lookup_named("TlsfAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_tlsf_new", vec![a1], tlsf_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "FixedBufferAllocator" && args.len() == 1 {
            let a1 = lower_expr(ctx, builder, &args[0].node.value);
            let fba_type = ctx.type_mapper.lookup_named("FixedBufferAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_fba_new", vec![a1], fba_type);
            return FunctionBuilder::copy(dst);
        }
        if name == "FallbackAllocator" && args.len() == 2 {
            let a1 = lower_expr(ctx, builder, &args[0].node.value);
            let a2 = lower_expr(ctx, builder, &args[1].node.value);
            let fb_type = ctx.type_mapper.lookup_named("FallbackAllocator").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_fallback_new", vec![a1, a2], fb_type);
            return FunctionBuilder::copy(dst);
        }

        // Channel[T](capacity) constructor → Channel__T__new(capacity)
        if name == "Channel" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let chan_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        tid
                    } else {
                        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                        ctx.type_mapper.register_named(mangled.clone(), tid);
                        tid
                    };
                    let cap_op = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![cap_op], chan_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Shared[T](value) constructor → Shared__T__new(value)
        if name == "Shared" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let val_op = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let val_type = infer_operand_type_full(ctx, &val_op, builder);
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let shared_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        tid
                    } else {
                        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                        ctx.type_mapper.register_named(mangled.clone(), tid);
                        ensure_shared_type_def(ctx, &mangled, val_type);
                        tid
                    };
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op.clone()], shared_type);
                    // Shared[T](v) takes ownership of v's data. Mark Move-type locals
                    // as moved so the drop elaborator skips them (avoids dangling ptr).
                    if let Operand::Copy(place) = &val_op {
                        if place.projections.is_empty()
                            && is_resource_type_local(place.local, builder, &ctx.type_registry)
                        {
                            builder.move_zero(place.clone());
                            ctx.emit_field_origin_zero(builder, place.local);
                            ctx.drops.mark_moved(place.local);
                        }
                    }
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Mutex[T](value) constructor → Mutex__T__new(value)
        if name == "Mutex" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let val_op = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let val_type = infer_operand_type_full(ctx, &val_op, builder);
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let mutex_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        tid
                    } else {
                        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                        ctx.type_mapper.register_named(mangled.clone(), tid);
                        ensure_mutex_type_def(ctx, &mangled, val_type);
                        tid
                    };
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op], mutex_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // TaskGroup.new() static constructor
        if name == "TaskGroup" && args.is_empty() {
            let tg_name = "TaskGroup";
            let tg_type = if let Some(tid) = ctx.type_mapper.lookup_named(tg_name) {
                tid
            } else {
                let tid = ctx.type_registry.insert(GirType::Named(tg_name.to_string()));
                ctx.type_mapper.register_named(tg_name.to_string(), tid);
                ensure_task_group_type_def(ctx, tg_name);
                tid
            };
            let dst = builder.call("gorget_task_group_new", vec![], tg_type);
            return FunctionBuilder::copy(dst);
        }

        // AtomicInt(initial_value) → gorget_atomic_int_new(val)
        if name == "AtomicInt" && args.len() == 1 {
            let val_op = lower_expr(ctx, builder, &args[0].node.value);
            let at_type = ctx.type_mapper.lookup_named("AtomicInt").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_atomic_int_new", vec![val_op], at_type);
            return FunctionBuilder::copy(dst);
        }

        // AtomicBool(initial_value) → gorget_atomic_bool_new(val)
        if name == "AtomicBool" && args.len() == 1 {
            let val_op = lower_expr(ctx, builder, &args[0].node.value);
            let at_type = ctx.type_mapper.lookup_named("AtomicBool").unwrap_or(BOOL_TYPE);
            let dst = builder.call_extern("gorget_atomic_bool_new", vec![val_op], at_type);
            return FunctionBuilder::copy(dst);
        }

        // Barrier(n) → gorget_barrier_new(n)
        if name == "Barrier" && args.len() == 1 {
            let n_op = lower_expr(ctx, builder, &args[0].node.value);
            let b_type = ctx.type_mapper.lookup_named("Barrier").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_barrier_new", vec![n_op], b_type);
            return FunctionBuilder::copy(dst);
        }

        // WaitGroup() → gorget_waitgroup_new()
        if name == "WaitGroup" && args.is_empty() {
            let wg_type = ctx.type_mapper.lookup_named("WaitGroup").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_waitgroup_new", vec![], wg_type);
            return FunctionBuilder::copy(dst);
        }

        // Semaphore(n) → gorget_semaphore_new(n)
        if name == "Semaphore" && args.len() == 1 {
            let n_op = lower_expr(ctx, builder, &args[0].node.value);
            let s_type = ctx.type_mapper.lookup_named("Semaphore").unwrap_or(I64_TYPE);
            let dst = builder.call_extern("gorget_semaphore_new", vec![n_op], s_type);
            return FunctionBuilder::copy(dst);
        }

        // RWLock[T](initial_value) → RWLock__T__new(value)
        if name == "RWLock" {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() && !args.is_empty() {
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    let rw_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        tid
                    } else {
                        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                        ctx.type_mapper.register_named(mangled.clone(), tid);
                        tid
                    };
                    let val_op = lower_expr(ctx, builder, &args[0].node.value);
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op], rw_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // thread_spawn(fn_name) → __gorget_thread_spawn_fn_name()
        // V1: only bare function references supported. Closures are a follow-up (see TODO.md).
        if name == "thread_spawn" && args.len() == 1 {
            if let ast::Expr::Identifier(fn_name) = &args[0].node.value.node {
                let fn_name = fn_name.clone();
                let fn_ret_type = ctx.fn_sigs.get(fn_name.as_str())
                    .map(|(_, r)| *r)
                    .unwrap_or(I64_TYPE);
                let ret_c = ctx.type_name_for_id(fn_ret_type)
                    .unwrap_or("int64_t")
                    .to_string();
                let thread_name = if fn_ret_type == UNIT_TYPE {
                    "Thread__void".to_string()
                } else {
                    format!("Thread__{ret_c}")
                };
                let thread_type = if let Some(tid) = ctx.type_mapper.lookup_named(&thread_name) {
                    tid
                } else {
                    let tid = ctx.type_registry.insert(GirType::Named(thread_name.clone()));
                    ctx.type_mapper.register_named(thread_name.clone(), tid);
                    tid
                };
                ctx.spawn.thread_fns.entry(fn_name.clone()).or_insert(fn_ret_type);
                let spawn_fn = format!("__gorget_thread_spawn_{fn_name}");
                let dst = builder.call(&spawn_fn, vec![], thread_type);
                return FunctionBuilder::copy(dst);
            }
        }

        // current_thread_id() → gorget_current_thread_id()
        if name == "current_thread_id" && args.is_empty() {
            let dst = builder.call_extern("gorget_current_thread_id", vec![], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }

        // getpid() → gorget_getpid()
        if name == "getpid" && args.is_empty() {
            let dst = builder.call_extern("gorget_getpid", vec![], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }

        // Collection constructors: Dict[K,V](), HashMap[K,V](), Set[K](), HashSet[K](), Vector[T]()
        if matches!(name.as_str(), "Dict" | "HashMap" | "Set" | "HashSet" | "Vector") {
            if let Some(type_args) = generic_args {
                if !type_args.is_empty() {
                    let mangled = super::super::types::mangle_generic_name(name, type_args);
                    let mangled = ctx.resolve_type_name(&mangled);
                    // Register the collection type if not present
                    let coll_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
                        tid
                    } else {
                        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
                        ctx.type_mapper.register_named(mangled.clone(), tid);
                        tid
                    };
                    // Check for alloc= named argument
                    let alloc_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
                    });
                    let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                        .filter(|a| !a.node.name.as_ref().map_or(false, |n| n.node == "alloc"))
                        .collect();

                    if positional_args.is_empty() {
                        let new_fn = format!("{mangled}__new");
                        if let Some(alloc_a) = alloc_arg {
                            // alloc= present: push allocator, construct, pop allocator
                            let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                            builder.push_allocator(alloc_op);
                            let coll_local = builder.call_extern(&new_fn, vec![], coll_type);
                            builder.pop_allocator();
                            return FunctionBuilder::copy(coll_local);
                        } else {
                            let coll_local = builder.call_extern(&new_fn, vec![], coll_type);
                            return FunctionBuilder::copy(coll_local);
                        }
                    }
                    // Fall through for positional args — type is registered, regular call will use correct return type
                }
            }
        }

        // Determine effective function name (mangled if generic call).
        // For meta op calls, also append per-op suffixes so the name matches
        // the mangled name produced by GenericCollector::register_instance_with_ops.
        let effective_name = if let Some(type_args) = generic_args {
            if !type_args.is_empty() {
                let mut mangled = super::super::types::mangle_generic_name(name, type_args);
                // Append __<op_suffix> for each MetaOpToken arg (same order as params)
                for arg in args.iter() {
                    if let Expr::MetaOpToken(op) = &arg.node.value.node {
                        mangled.push_str("__");
                        mangled.push_str(super::super::types::op_mangle_suffix(*op));
                    }
                }
                // Apply type name substitutions for generic monomorphization
                ctx.resolve_type_name(&mangled)
            } else {
                name.clone()
            }
        } else {
            name.clone()
        };

        // Check if this is an Option/Result variant constructor — resolve with type-aware logic
        {
            let call_arg_values: Vec<Spanned<Expr>> = args.iter()
                .map(|a| a.node.value.clone())
                .collect();
            if let Some(result) = resolve_option_result_variant(ctx, builder, name, &call_arg_values) {
                return result;
            }
        }

        // Check if this is an enum variant constructor
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(&effective_name) {
            let field_operands: Vec<Operand> = args.iter()
                .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                .collect();

            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
            return FunctionBuilder::copy(dst);
        }
        // Also check base name for non-generic enum variants
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
            let field_operands: Vec<Operand> = args.iter()
                .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                .collect();

            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
            return FunctionBuilder::copy(dst);
        }

        // Check if this is a closure variable call (e.g., `add_x(5)` where `add_x` is a closure)
        if let Some((local_id, local_type_id)) = ctx.lookup_local(&effective_name) {
            let type_name = ctx.type_name_for_id(local_type_id);
            if let Some(type_name) = type_name {
                let type_name = type_name.to_string();
                if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                    let call_fn = call_fn.to_string();
                    // Closure call: __Closure_N__call(&closure_var, args...)
                    // The __call function expects a pointer to the closure struct
                    let ptr_type = ctx.type_registry.insert(GirType::Ptr(local_type_id));
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr_local, Place::local(local_id));
                    let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
                    for arg in args {
                        call_args.push(lower_expr(ctx, builder, &arg.node.value));
                    }
                    let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(call_fn.as_str()) {
                        *ret
                    } else {
                        I64_TYPE
                    };
                    if ret_type == UNIT_TYPE {
                        builder.call_void(call_fn, call_args);
                        return Operand::Constant(Constant::Unit);
                    } else {
                        let dst = builder.call(call_fn, call_args, ret_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
            // Callable parameter call: local exists with void* type (UNIT_TYPE)
            // Emit as __callable_N where N is the local ID, which the C backend
            // will recognize and emit as an indirect function pointer call.
            if local_type_id == UNIT_TYPE {
                let mut call_args = vec![FunctionBuilder::copy(local_id)];
                for arg in args {
                    // For borrow params passed to callable, preserve the pointer
                    // (don't auto-deref). The adapter function expects the pointer type.
                    if let Expr::Identifier(arg_name) = &arg.node.value.node {
                        if let Some((arg_local, _)) = ctx.lookup_local(arg_name) {
                            if ctx.mut_capture_locals.contains_key(&arg_local) {
                                call_args.push(FunctionBuilder::copy(arg_local));
                                continue;
                            }
                        }
                    }
                    call_args.push(lower_expr(ctx, builder, &arg.node.value));
                }
                let callable_name = format!("__callable_{}", local_id.0);
                // Look up tracked callable return type, fall back to I64_TYPE
                let ret_type = ctx.callable_return_types.get(&local_id)
                    .copied()
                    .unwrap_or(I64_TYPE);
                if ret_type == UNIT_TYPE {
                    builder.call_void(callable_name, call_args);
                    return Operand::Constant(Constant::Unit);
                }
                let dst = builder.call(callable_name, call_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
            // FnPtr-typed local: escaped closure returned from a function, stored as GorgetClosure.
            // Emit __gorget_closure_call_N; the C backend expands it to fn_ptr+env dispatch.
            if let Some(GirType::FnPtr { return_type: fn_ret, .. }) = ctx.type_registry.get(local_type_id).cloned() {
                let mut call_args = vec![FunctionBuilder::copy(local_id)];
                for arg in args {
                    call_args.push(lower_expr(ctx, builder, &arg.node.value));
                }
                let callable_name = format!("__gorget_closure_call_{}", local_id.0);
                if fn_ret == UNIT_TYPE {
                    builder.call_void(callable_name, call_args);
                    return Operand::Constant(Constant::Unit);
                } else {
                    let dst = builder.call(callable_name, call_args, fn_ret);
                    return FunctionBuilder::copy(dst);
                }
            }
        }

        // Regular function call (use effective name for generic functions)
        // Filter out MetaOpToken args — they are compile-time only and have no
        // runtime representation in the lowered GIR call.
        let runtime_args_buf: Vec<Spanned<ast::CallArg>>;
        let runtime_args: &[Spanned<ast::CallArg>] =
            if args.iter().any(|a| matches!(a.node.value.node, Expr::MetaOpToken(_))) {
                runtime_args_buf = args
                    .iter()
                    .filter(|a| !matches!(a.node.value.node, Expr::MetaOpToken(_)))
                    .cloned()
                    .collect();
                &runtime_args_buf
            } else {
                args
            };
        // Resolve named args + default params before lowering
        let resolved_args = resolve_call_args(ctx, &effective_name, runtime_args);
        // Extract parameter types to thread expected_type for dot-shorthand args
        let param_types: Vec<TypeId> = ctx.fn_sigs.get(effective_name.as_str())
            .map(|(params, _)| params.clone())
            .unwrap_or_default();
        let lowered_args: Vec<Operand> = resolved_args
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let prev_expected = ctx.expected_type;
                let callee_pt = param_types.get(i).copied();
                if let Some(pt) = callee_pt {
                    ctx.expected_type = Some(pt);
                }
                let op = lower_call_arg(ctx, builder, arg, callee_pt, &effective_name, i);
                ctx.expected_type = prev_expected;
                op
            })
            .collect();

        // Collect Move-ownership Move-type arg locals for post-call MoveZero.
        // Resolve the original source local from the arg expression (not the
        // lowered MutPtr, which is_resource_type_local doesn't recognize).
        let move_zero_locals: Vec<Place> = resolved_args.iter()
            .filter_map(|arg| {
                if !matches!(arg.node.ownership, Ownership::Move) { return None; }
                if let Expr::Identifier(name) = &arg.node.value.node {
                    if let Some((local_id, _)) = ctx.lookup_local(name) {
                        if is_resource_type_local(local_id, builder, &ctx.type_registry) {
                            return Some(Place::local(local_id));
                        }
                    }
                }
                None
            })
            .collect();

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
            *ret
        } else {
            I64_TYPE // fallback
        };

        // Save callee name for ABI lookup before effective_name is moved
        let sig_name = effective_name.clone();

        // Resolve extern bindings: use the C symbol name instead of the Gorget name
        let call_name = if let Some(c_symbol) = ctx.extern_bindings.get(effective_name.as_str()) {
            c_symbol.clone()
        } else {
            effective_name
        };

        // Unregister GorgetString temps when the callee might store str views.
        // For void-returning calls with no mutable-ref params, keep temps in
        // drop tracking — the str view dies on the callee's stack.
        if super::should_unregister_string_args(ctx, &sig_name, ret_type) {
            super::unregister_gorget_string_args(ctx, builder, &lowered_args);
        }

        // Collect drop-registered collection TEMPS (not named variables) passed
        // as args. These need move-zero after the call to prevent double-free:
        // the callee received a shallow copy of the buffer, so the caller must
        // relinquish ownership of the anonymous temp.
        // Named variables (e.g., `len(nums)`) must NOT be zeroed — caller still owns them.
        let collection_arg_locals: Vec<LocalId> = lowered_args.iter()
            .filter_map(|op| {
                if let Operand::Copy(place) | Operand::Move(place) = op {
                    if place.projections.is_empty()
                        && !ctx.is_named_local(place.local)
                        && ctx.drops.is_registered(place.local)
                        && !ctx.drops.is_moved(place.local)
                    {
                        let ty = builder.locals[place.local.0 as usize].type_id;
                        if ctx.type_registry.is_collection_type(ty) {
                            return Some(place.local);
                        }
                    }
                }
                None
            })
            .collect();

        let result = if ret_type == UNIT_TYPE {
            builder.call_void(&call_name, lowered_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = ctx.call_tracked(builder, &call_name, lowered_args, ret_type);
            FunctionBuilder::copy(dst)
        };

        // MoveZero Move-ownership args to transfer ownership (prevent double-free)
        for place in &move_zero_locals {
            builder.move_zero(place.clone());
            ctx.emit_field_origin_zero(builder, place.local);
            ctx.drops.mark_moved(place.local);
        }

        // MoveZero collection temps passed as args — callee received a
        // shallow copy, so the caller relinquishes the original.
        for local in &collection_arg_locals {
            builder.move_zero(Place::local(*local));
            ctx.drops.mark_moved(*local);
        }

        result
    } else if let Expr::Closure { params, body, is_move, .. } = &callee.node {
        // IIFE: ((int x): x * x)(5) — inline closure called immediately
        let mut cl = std::mem::take(&mut ctx.closures);
        let closure_op = cl.lower_closure(ctx, builder, params, body, *is_move);
        ctx.closures = cl;

        if let Operand::Copy(ref place) | Operand::Move(ref place) = closure_op {
            if place.projections.is_empty() {
                let closure_local = place.local;
                let closure_type_id = builder.locals[closure_local.0 as usize].type_id;
                if let Some(type_name) = ctx.type_name_for_id(closure_type_id).map(|s| s.to_string()) {
                    if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                        let call_fn = call_fn.to_string();
                        // Build args: pointer to closure struct + call arguments
                        let ptr_type = ctx.type_registry.insert(GirType::Ptr(closure_type_id));
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow(ptr_local, Place::local(closure_local));
                        let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
                        for arg in args {
                            call_args.push(lower_expr(ctx, builder, &arg.node.value));
                        }
                        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(call_fn.as_str()) {
                            *ret
                        } else {
                            I64_TYPE
                        };
                        if ret_type == UNIT_TYPE {
                            builder.call_void(&call_fn, call_args);
                            return Operand::Constant(Constant::Unit);
                        } else {
                            let dst = builder.call(&call_fn, call_args, ret_type);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }
            }
        }
        // Fallback if closure info not found
        Operand::Constant(Constant::Unit)
    } else {
        // Non-identifier, non-closure callee — not handled
        Operand::Constant(Constant::Unit)
    }
}

/// Lower a `print(...)` call to a `printf` extern call.
pub fn lower_print_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    args: &[Spanned<ast::CallArg>],
) {
    if args.is_empty() {
        // print() with no args → printf("\n")
        let fmt = Operand::Constant(Constant::Str("\n".to_string()));
        builder.call_extern("printf", vec![fmt], I32_TYPE);
        return;
    }

    // Check for named arguments: newline=false, file=stderr
    let mut add_newline = true;
    let mut use_stderr = false;
    for arg in args.iter().skip(1) {
        if let Some(ref name) = arg.node.name {
            match name.node.as_str() {
                "newline" => {
                    if let Expr::BoolLiteral(false) = &arg.node.value.node {
                        add_newline = false;
                    }
                }
                "file" => {
                    if let Expr::Identifier(id) = &arg.node.value.node {
                        if id == "stderr" {
                            use_stderr = true;
                        }
                    }
                }
                _ => {}
            }
        }
    }

    let arg_expr = &args[0].node.value;

    match &arg_expr.node {
        Expr::StringLiteral(lit) => {
            let mut format_str = String::new();
            let mut printf_args: Vec<Operand> = Vec::new();

            for segment in &lit.segments {
                match segment {
                    StringSegment::Literal(text) => {
                        format_str.push_str(text);
                    }
                    StringSegment::Interpolation(var_name, fmt_spec) => {
                        lower_interp_segment(ctx, builder, var_name,
                            &mut format_str, &mut printf_args, fmt_spec.as_deref());
                    }
                }
            }

            if add_newline {
                format_str.push('\n');
            }

            let mut all_args = Vec::new();
            if use_stderr {
                all_args.push(Operand::Constant(Constant::Null)); // stderr placeholder
                all_args.push(Operand::Constant(Constant::Str(format_str)));
                all_args.extend(printf_args);
                builder.call_extern("fprintf_stderr", all_args, I32_TYPE);
            } else {
                all_args.push(Operand::Constant(Constant::Str(format_str)));
                all_args.extend(printf_args);
                builder.call_extern("printf", all_args, I32_TYPE);
            }
        }
        _ => {
            // General expression (identifier, method call, etc.) — lower and infer type
            let val = lower_expr(ctx, builder, arg_expr);
            let type_id = infer_operand_type_full(ctx, &val, builder);
            let (spec, extra_args) = format_for_printf(ctx, builder, type_id, val, None);
            let nl = if add_newline { "\n" } else { "" };
            let fmt = format!("{spec}{nl}");
            let fmt_op = Operand::Constant(Constant::Str(fmt));
            let mut all_args = Vec::new();
            if use_stderr {
                all_args.push(Operand::Constant(Constant::Null)); // stderr placeholder
                all_args.push(fmt_op);
                all_args.extend(extra_args);
                builder.call_extern("fprintf_stderr", all_args, I32_TYPE);
            } else {
                all_args.push(fmt_op);
                all_args.extend(extra_args);
                builder.call_extern("printf", all_args, I32_TYPE);
            }
        }
    }
}

/// Lower a single interpolation segment in a print/format context.
/// Handles simple variable lookups and re-parses complex expressions (method calls, field access, etc.).
/// `fmt_spec` is an optional format specifier like ".2f", "x", "08d", etc.
pub(super) fn lower_interp_segment(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    format_str: &mut String,
    printf_args: &mut Vec<Operand>,
    fmt_spec: Option<&str>,
) {
    // 1. Try simple variable lookup first
    if let Some((local_id, type_id)) = ctx.lookup_local(var_name) {
        // If this is a pointer param, deref to get the value for formatting.
        // Covers &/! params (mut_capture_locals) and borrowed resource params (ref_locals).
        let ptr_value_type = ctx.mut_capture_locals.get(&local_id).copied()
            .or_else(|| {
                if ctx.ref_locals.contains(&local_id) {
                    ctx.pointee_type(builder.locals[local_id.0 as usize].type_id)
                } else {
                    None
                }
            });
        if let Some(value_type) = ptr_value_type {
            let deref_place = Place {
                local: local_id,
                projections: vec![Projection::Deref],
            };
            let tmp = builder.add_local(value_type, None);
            builder.assign(Place::local(tmp), Operand::Copy(deref_place));
            let (spec, args) = format_for_printf(ctx, builder, value_type, FunctionBuilder::copy(tmp), fmt_spec);
            format_str.push_str(&spec);
            printf_args.extend(args);
        } else {
            let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(local_id), fmt_spec);
            format_str.push_str(&spec);
            printf_args.extend(args);
        }
        return;
    }

    // 2. Try re-parsing as a full expression (handles method calls, field access, operators)
    if let Ok(parsed_expr) = Parser::new(var_name).parse_expr() {
        let val = lower_expr(ctx, builder, &parsed_expr);
        let type_id = infer_operand_type_full(ctx, &val, builder);
        // Store result in a temp local so we can take its address / reuse
        let tmp = builder.add_local(type_id, None);
        builder.assign(Place::local(tmp), val);
        let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(tmp), fmt_spec);
        format_str.push_str(&spec);
        printf_args.extend(args);
        return;
    }

    // 3. Fallback — insert literal text
    format_str.push_str(var_name);
}

/// Given a type and an operand, return the printf format specifier and the
/// argument list. For Str types, returns `%.*s` with two args (len, data).
/// For bool, returns `%s` with ternary. For other types, returns the standard specifier.
/// When `fmt_spec` is provided (e.g., ".2f", "x", "08d"), it overrides the default format.
fn format_for_printf(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_id: TypeId,
    operand: Operand,
    fmt_spec: Option<&str>,
) -> (String, Vec<Operand>) {
    // If a format spec is provided, try to generate a custom printf format
    if let Some(spec) = fmt_spec {
        if let Some(result) = apply_format_spec(ctx, builder, type_id, operand.clone(), spec) {
            return result;
        }
        // If apply_format_spec returns None, fall through to default
    }

    if type_id == ctx.type_mapper.string_view_type || type_id == ctx.type_mapper.owned_string_type {
        // Str/GorgetString → %.*s with (int)expr.len, expr.data
        ("%.*s".to_string(), vec![operand])
    } else if type_id == BOOL_TYPE {
        ("%s".to_string(), vec![operand])
    } else if let Some(GirType::Named(ref type_name)) = ctx.type_registry.get(type_id).cloned() {
        // Struct type — check if it has a Displayable `display` method
        let display_method = format!("{type_name}__display");
        let has_display = ctx.fn_sigs.contains_key(&display_method)
            || ctx.fn_sigs.keys().any(|k| k.ends_with(&format!("_for_{type_name}__display")));
        if has_display {
            // Call Type__display(&val) → Str, then use %.*s
            let effective_method = if ctx.fn_sigs.contains_key(&display_method) {
                display_method
            } else {
                ctx.fn_sigs.keys()
                    .find(|k| k.ends_with(&format!("_for_{type_name}__display")))
                    .cloned()
                    .unwrap_or(display_method)
            };
            // Create borrow of the operand for self parameter
            let self_type = ctx.register_ptr_type(type_id);
            let self_ptr = builder.add_local(self_type, None);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = operand {
                builder.emit_borrow(self_ptr, place.clone());
            }
            let string_view_type = ctx.type_mapper.string_view_type;
            let result = builder.call(effective_method, vec![FunctionBuilder::copy(self_ptr)], string_view_type);
            ("%.*s".to_string(), vec![FunctionBuilder::copy(result)])
        } else {
            // No display method — fall through to default formatting
            let spec = ctx.type_mapper.format_specifier(type_id);
            (spec.to_string(), vec![operand])
        }
    } else {
        // For narrow integer types, cast to int64_t/uint64_t to match %lld/%llu format
        let needs_widen = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE;
        let needs_unsigned_widen = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE;
        let effective_op = if needs_widen {
            let tmp = builder.cast(I64_TYPE, operand);
            FunctionBuilder::copy(tmp)
        } else if needs_unsigned_widen {
            let tmp = builder.cast(U64_TYPE, operand);
            FunctionBuilder::copy(tmp)
        } else {
            operand
        };
        let spec = ctx.type_mapper.format_specifier(type_id);
        (spec.to_string(), vec![effective_op])
    }
}

/// Apply a user-provided format spec (e.g., ".2f", "x", "08d") to produce a
/// printf format string. Returns None if the spec is not recognized.
///
/// Supported specs:
///   Integer: d, x, X, o, b, #x, #X, #o, #b, with optional width/zero-pad (e.g., "08x", "5d")
///   Float: f, e, E, with optional precision (e.g., ".2f", ".4e")
///   String: s, with optional width (e.g., "10s")
fn apply_format_spec(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_id: TypeId,
    operand: Operand,
    spec: &str,
) -> Option<(String, Vec<Operand>)> {
    if spec.is_empty() {
        return None;
    }

    let is_signed_int = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE
        || type_id == I64_TYPE;
    let is_unsigned_int = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE
        || type_id == U64_TYPE;
    let is_any_int = is_signed_int || is_unsigned_int;
    let is_float = type_id == F32_TYPE || type_id == F64_TYPE;
    let is_str = type_id == ctx.type_mapper.string_view_type || type_id == ctx.type_mapper.owned_string_type;

    // Parse the spec: [#][0][width][.precision][type_char]
    let bytes = spec.as_bytes();
    let mut pos = 0;

    // Check for '#' (alternate form: 0x, 0o, 0b prefix)
    let alt = if pos < bytes.len() && bytes[pos] == b'#' {
        pos += 1;
        true
    } else {
        false
    };

    // Check for '0' (zero-pad)
    let zero_pad = if pos < bytes.len() && bytes[pos] == b'0'
        && pos + 1 < bytes.len() && bytes[pos + 1].is_ascii_digit()
    {
        pos += 1;
        true
    } else {
        false
    };

    // Parse width digits
    let width_start = pos;
    while pos < bytes.len() && bytes[pos].is_ascii_digit() {
        pos += 1;
    }
    let width: Option<&str> = if pos > width_start {
        Some(&spec[width_start..pos])
    } else {
        None
    };

    // Parse precision: .N
    let precision: Option<&str> = if pos < bytes.len() && bytes[pos] == b'.' {
        pos += 1;
        let prec_start = pos;
        while pos < bytes.len() && bytes[pos].is_ascii_digit() {
            pos += 1;
        }
        Some(&spec[prec_start..pos])
    } else {
        None
    };

    // Parse type character
    if pos >= bytes.len() {
        // No type char — just width/precision with default type
        if is_any_int && (width.is_some() || zero_pad) {
            let w = width.unwrap_or("0");
            let z = if zero_pad { "0" } else { "" };
            let len_mod = if is_unsigned_int { "llu" } else { "lld" };
            let op = widen_int(builder, type_id, operand);
            return Some((format!("%{z}{w}{len_mod}"), vec![op]));
        }
        if is_float && precision.is_some() {
            let p = precision.unwrap();
            return Some((format!("%.{p}f"), vec![operand]));
        }
        return None;
    }

    let type_char = bytes[pos] as char;

    match type_char {
        // ── Integer formats ──
        'd' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let len_mod = if is_unsigned_int { "llu" } else { "lld" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{z}{w}{len_mod}"), vec![op]))
        }
        'x' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let prefix = if alt { "#" } else { "" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{prefix}{z}{w}llx"), vec![op]))
        }
        'X' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let prefix = if alt { "#" } else { "" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{prefix}{z}{w}llX"), vec![op]))
        }
        'o' if is_any_int => {
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            let prefix = if alt { "#" } else { "" };
            let op = widen_int(builder, type_id, operand);
            Some((format!("%{prefix}{z}{w}llo"), vec![op]))
        }
        'b' if is_any_int => {
            // Binary has no printf equivalent — call runtime helper returning const char*
            let op = widen_int(builder, type_id, operand);
            let string_view_type = ctx.type_mapper.string_view_type;
            let alt_arg = Operand::Constant(Constant::I64(if alt { 1 } else { 0 }));
            let result = builder.call_extern(
                "gorget_int_to_binary",
                vec![op, alt_arg],
                string_view_type,
            );
            Some(("%.*s".to_string(), vec![FunctionBuilder::copy(result)]))
        }

        // ── Float formats ──
        'f' if is_float => {
            let p = precision.unwrap_or("6");
            let w = width.unwrap_or("");
            let z = if zero_pad { "0" } else { "" };
            Some((format!("%{z}{w}.{p}f"), vec![operand]))
        }
        'e' if is_float => {
            let p = precision.unwrap_or("6");
            let w = width.unwrap_or("");
            Some((format!("%{w}.{p}e"), vec![operand]))
        }
        'E' if is_float => {
            let p = precision.unwrap_or("6");
            let w = width.unwrap_or("");
            Some((format!("%{w}.{p}E"), vec![operand]))
        }

        // ── String format ──
        's' if is_str => {
            if let Some(w) = width {
                Some((format!("%-{w}.*s"), vec![operand]))
            } else {
                None // no spec effect, use default
            }
        }

        _ => None, // unrecognized spec — fall through to default
    }
}

/// Widen narrow integer types to 64-bit for printf length modifiers.
fn widen_int(builder: &mut FunctionBuilder, type_id: TypeId, operand: Operand) -> Operand {
    let needs_widen = type_id == I8_TYPE || type_id == I16_TYPE || type_id == I32_TYPE;
    let needs_unsigned_widen = type_id == U8_TYPE || type_id == U16_TYPE || type_id == U32_TYPE;
    if needs_widen {
        let tmp = builder.cast(I64_TYPE, operand);
        FunctionBuilder::copy(tmp)
    } else if needs_unsigned_widen {
        let tmp = builder.cast(U64_TYPE, operand);
        FunctionBuilder::copy(tmp)
    } else {
        operand
    }
}
