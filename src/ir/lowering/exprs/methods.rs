//! Method call lowering, collection method dispatch, and iterator adapter lowering.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr, Ownership};
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::{lower_expr, lower_call_arg, infer_operand_type_full, register_tuple_type,
            c_suffix_to_type_id, is_resource_type_local,
            ensure_box_type_def, ensure_guard_type_def, ensure_shared_type_def, ensure_weak_type_def,
            index_expr_to_mangle_fragment, try_resolve_field_place};

fn gorget_name_for_type_id(ctx: &LoweringContext, type_id: TypeId) -> String {
    if type_id == ctx.type_mapper.string_view_type {
        return "str".to_string();
    }
    match type_id {
        I64_TYPE => "int".to_string(),
        F64_TYPE => "float".to_string(),
        BOOL_TYPE => "bool".to_string(),
        I8_TYPE => "int8".to_string(),
        I16_TYPE => "int16".to_string(),
        I32_TYPE => "int32".to_string(),
        U8_TYPE => "uint8".to_string(),
        U16_TYPE => "uint16".to_string(),
        U32_TYPE => "uint32".to_string(),
        U64_TYPE => "uint64".to_string(),
        F32_TYPE => "float32".to_string(),
        _ => {
            // For named types, look up in type_mapper
            ctx.type_mapper.name_for_type_id(type_id)
                .unwrap_or_else(|| "int".to_string())
        }
    }
}

pub(super) fn lower_method_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    receiver: &Spanned<Expr>,
    method_name: &str,
    args: &[Spanned<ast::CallArg>],
) -> Operand {
    // Static method call: Type.method(args) where receiver is a type name, not a value
    if let Expr::Identifier(name) = &receiver.node {
        // Resolve generic type params to concrete type names (e.g., T → "int" when T=int).
        // This enables T.default(), T.one(), T.parse() etc. inside monomorphized generic bodies.
        let resolved_name: String = if let Some(&type_id) = ctx.generics.generic_type_params.get(name.as_str()) {
            gorget_name_for_type_id(ctx, type_id)
        } else {
            name.clone()
        };
        let name = &resolved_name;
        if ctx.lookup_local(name).is_none() && !ctx.module_constants.contains_key(name) {
            // Box.new(value) → heap allocation
            if name == "Box" && method_name == "new" && !args.is_empty() {
                let mut val = lower_expr(ctx, builder, &args[0].node.value);
                let raw_type = infer_operand_type_full(ctx, &val, builder);
                // Unwrap Ptr(T) → T: when the argument is a bare-borrowed resource
                // type (passed by pointer), Box should box the value, not the pointer.
                // Emit a LoadRef to dereference the pointer and get the value.
                let inner_type = match ctx.type_registry.get(raw_type) {
                    Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                        let pointee = *inner;
                        if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                            let derefed = builder.load_ref(place.clone(), pointee);
                            val = FunctionBuilder::copy(derefed);
                        }
                        pointee
                    }
                    _ => raw_type,
                };
                let inner_c = ctx.c_type_name_for_id(inner_type);

                // For Box.new(closure): return the closure struct directly in the GIR path.
                // Box[Callable[...]] variables use needs_reinfer to pick up the __Closure_N type,
                // and dispatch via lookup_closure_info at call sites. This avoids the complexity
                // of tracking heap-allocated callable boxes through the GIR type system.
                if inner_c.starts_with("__Closure_") {
                    return val;
                }

                let box_type_name = format!("Box__{inner_c}");
                let box_type = if let Some(tid) = ctx.type_mapper.lookup_named(&box_type_name) {
                    tid
                } else {
                    let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(box_type_name.clone()));
                    ctx.type_mapper.register_named(box_type_name.clone(), tid);
                    // Also create TypeDef so C backend emits typedef
                    ensure_box_type_def(ctx, &box_type_name, inner_type);
                    tid
                };
                let alloc_fn = format!("__gorget_box_alloc_{inner_c}");
                let dst = builder.call(alloc_fn, vec![val], box_type);
                return FunctionBuilder::copy(dst);
            }

            // Check if this is a known type name (including primitives like int, float, bool)
            let is_primitive_type = matches!(name.as_str(), "int" | "float" | "bool" | "uint8" | "uint16" | "uint32" | "uint64"
                | "int8" | "int16" | "int32" | "str" | "String" | "char" | "byte");
            if is_primitive_type || ctx.type_mapper.lookup_named(name).is_some() || ctx.resolve_enum_variant(name).is_some() {
                let lowered_args: Vec<Operand> = args.iter()
                    .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                    .collect();

                // Map Gorget primitive names to C type names for method lookup
                let c_type_name = match name.as_str() {
                    "int" => "int64_t",
                    "float" => "double",
                    "bool" => "bool",
                    "str" | "String" => "GorgetStringView",
                    "char" => "char",
                    "byte" | "uint8" => "uint8_t",
                    "uint16" => "uint16_t",
                    "uint32" => "uint32_t",
                    "uint64" => "uint64_t",
                    "int8" => "int8_t",
                    "int16" => "int16_t",
                    "int32" => "int32_t",
                    _ => name.as_str(),
                };
                // Check if this is a qualified enum variant constructor: Color.Red()
                if let Some(type_def) = ctx.type_registry.get_type_def(c_type_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if e.variants.iter().any(|v| v.name == method_name) {
                            let type_id = ctx.type_mapper.lookup_named(name).unwrap_or(UNIT_TYPE);
                            let dst = builder.enum_init(name, method_name, type_id, lowered_args.clone());
                            super::move_zero_resource_args(ctx, builder, &lowered_args);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                }

                let mangled = format!("{c_type_name}__{method_name}");
                // Check fn_sigs for direct name or trait-mangled name (Trait_for_Type__method)
                let effective_name = if ctx.fn_sigs.contains_key(mangled.as_str()) {
                    mangled.clone()
                } else {
                    // Search for *_for_Type__method pattern — may have multiple overloads
                    let suffix = format!("_for_{name}__{method_name}");
                    let candidates: Vec<&String> = ctx.fn_sigs.keys()
                        .filter(|k| k.ends_with(&suffix))
                        .collect();

                    if candidates.len() == 1 {
                        candidates[0].clone()
                    } else if candidates.len() > 1 {
                        // Multiple overloads — match by arg types
                        let arg_types: Vec<TypeId> = lowered_args.iter()
                            .map(|a| infer_operand_type_full(ctx, a, builder))
                            .collect();
                        candidates.iter()
                            .find(|k| {
                                if let Some((params, _)) = ctx.fn_sigs.get(k.as_str()) {
                                    params.len() == arg_types.len()
                                        && params.iter().zip(arg_types.iter()).all(|(p, a)| *p == *a)
                                } else {
                                    false
                                }
                            })
                            .map(|k| (*k).clone())
                            .unwrap_or(mangled.clone())
                    } else {
                        mangled.clone()
                    }
                };

                let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
                    *ret
                } else {
                    // Try to infer return type — for .from(), .default() etc., return the type itself
                    ctx.type_mapper.lookup_named(name).unwrap_or(I64_TYPE)
                };
                if super::should_unregister_string_args(ctx, &effective_name, ret_type) {
                    super::unregister_gorget_string_args(ctx, builder, &lowered_args);
                }
                if ret_type == UNIT_TYPE {
                    builder.call_void(effective_name, lowered_args);
                    return Operand::Constant(Constant::Unit);
                }
                let dst = ctx.call_tracked(builder, effective_name, lowered_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
        }
    }

    // Static method call on a generic type: SparseSet[Health].new()
    // Parsed as MethodCall { receiver: Index { object: Identifier("SparseSet"), index: Identifier("Health") }, ... }
    if let Expr::Index { object, index } = &receiver.node {
        if let Expr::Identifier(type_name) = &object.node {
            if let Some(mangled_type) = index_expr_to_mangle_fragment(&index.node)
                .map(|frag| format!("{type_name}__{frag}"))
            {
                if ctx.type_mapper.lookup_named(&mangled_type).is_some() {
                    let lowered_args: Vec<Operand> = args.iter()
                        .map(|arg| lower_expr(ctx, builder, &arg.node.value))
                        .collect();
                    let mangled_fn = format!("{mangled_type}__{method_name}");
                    let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(mangled_fn.as_str()) {
                        *ret
                    } else {
                        ctx.type_mapper.lookup_named(&mangled_type).unwrap_or(I64_TYPE)
                    };
                    if ret_type == UNIT_TYPE {
                        builder.call_void(mangled_fn, lowered_args);
                        return Operand::Constant(Constant::Unit);
                    }
                    let dst = builder.call(mangled_fn, lowered_args, ret_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // Before lowering the receiver expression, check if it's a field access that
    // we may need to borrow in-place (e.g., self.values.push(x) should not copy values).
    let field_place_info = if let Expr::FieldAccess { object, field } = &receiver.node {
        try_resolve_field_place(ctx, builder, object, &field.node)
    } else {
        None
    };

    // For pointer params used as method receivers, pass the raw pointer directly.
    // Auto-deref would copy the struct, and mutations to the copy wouldn't propagate back.
    let borrow_param_local = if let Expr::Identifier(name) = &receiver.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.ref_locals.contains(&local_id)
                || ctx.mut_capture_locals.contains_key(&local_id)
            {
                Some(local_id)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    let recv = if borrow_param_local.is_some() {
        // Skip auto-deref — use the raw pointer
        let local_id = borrow_param_local.unwrap();
        Operand::Copy(Place::local(local_id))
    } else {
        lower_expr(ctx, builder, receiver)
    };

    // .await() on Task → dispatch through __gorget_await_<fn> (joins pthread, returns result).
    // Check spawn_result_locals FIRST, before type check, since the declared type may be I64_TYPE
    // (lower_type returns I64_TYPE for unknown Task[T] types) even when the local is a spawn result.
    if method_name == "await" {
        // Direct local lookup (simple `task.await()` case)
        let task_local = if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            if place.projections.is_empty() {
                ctx.spawn.result_locals.get(&place.local).cloned()
                    .map(|fn_name| (Some(place.local), fn_name))
            } else {
                None
            }
        } else {
            None
        };
        // Fallback: type-based lookup for indexed tasks (e.g., `tasks[j].await()`)
        let resolved = task_local.or_else(|| {
            let type_id = if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                Some(builder.locals[place.local.0 as usize].type_id)
            } else {
                None
            };
            type_id.and_then(|tid| {
                ctx.spawn.task_type_fns.get(&tid).and_then(|fns| {
                    if fns.len() == 1 { Some((None, fns[0].clone())) } else { None }
                })
            })
        });
        if let Some((maybe_local_id, fn_name)) = resolved {
            let ret_type = ctx.fn_sigs.get(fn_name.as_str())
                .map(|(_, r)| *r)
                .unwrap_or(UNIT_TYPE);
            let await_fn = format!("__gorget_await_{fn_name}");
            // Extract receiver local before recv is consumed by the call.
            let recv_local = match &recv {
                Operand::Copy(place) | Operand::Move(place)
                    if place.projections.is_empty() => Some(place.local),
                _ => None,
            };
            let result = if ret_type == UNIT_TYPE {
                builder.call_void(&await_fn, vec![recv]);
                Operand::Constant(Constant::Unit)
            } else {
                let dst = builder.call(&await_fn, vec![recv], ret_type);
                FunctionBuilder::copy(dst)
            };
            // Zero out the Task local after await to prevent double-join in drop.
            // For direct spawn locals, maybe_local_id is Some. For type-based
            // fallback (e.g. task from Vector.remove().unwrap()), use the
            // receiver local extracted above.
            let zero_local = maybe_local_id.or(recv_local);
            if let Some(local_id) = zero_local {
                builder.move_zero(Place::local(local_id));
                ctx.drops.mark_moved(local_id);
            }
            return result;
        }
        return recv; // fallback pass-through (no known spawn source)
    }

    // Primitive .hash() → runtime hash functions
    if method_name == "hash" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if recv_type == I64_TYPE || recv_type == I32_TYPE {
            let dst = builder.call_extern("__gorget_hash_int", vec![recv], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == BOOL_TYPE {
            let cast = builder.cast(I64_TYPE, recv);
            let dst = builder.call_extern("__gorget_hash_int", vec![FunctionBuilder::copy(cast)], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }
        // Str.hash() is handled by the normal method dispatch path below
    }

    // .unwrap() / .expect() / .unwrap_or() on Option/Result → inline extraction
    // On non-Option/Result types → pass-through (unwrap is a no-op)
    if matches!(method_name, "unwrap" | "expect" | "unwrap_or") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_option_or_result = type_name.as_ref()
            .map(|n| n.starts_with("Option") || n.starts_with("Result"))
            .unwrap_or(false);
        if !is_option_or_result {
            // Not an Option/Result — unwrap is a no-op
            return recv;
        }
        // For Option/Result, extract the inner value via extern call that C backend handles
        if let Some(ref tn) = type_name {
            let is_result = tn.starts_with("Result__");
            let inner_type = if tn.starts_with("Option__") {
                let inner_name = &tn["Option__".len()..];
                // Option__Ref_T → Ptr(T) (borrowed reference from collection)
                if let Some(pointee_name) = inner_name.strip_prefix("Ref_") {
                    let pointee_type = resolve_inner_type(ctx, pointee_name);
                    ctx.type_registry.insert(GirType::Ptr(pointee_type))
                } else {
                    resolve_inner_type(ctx, inner_name)
                }
            } else if is_result {
                // Result__Ok__Err → extract Ok type (strip error type from end)
                let rest = &tn["Result__".len()..];
                // Try stripping common error suffixes
                let ok_name = ["__Str", "__int64_t", "__bool", "__double"].iter()
                    .find_map(|suffix| rest.strip_suffix(suffix))
                    .unwrap_or_else(|| {
                        // Fallback: split at last __ separator
                        rest.rfind("__").map(|pos| &rest[..pos]).unwrap_or(rest)
                    });
                resolve_inner_type(ctx, ok_name)
            } else {
                I64_TYPE
            };

            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                let ptr_type = ctx.register_ptr_type(
                    infer_operand_type_full(ctx, &recv, builder),
                );
                let borrow = builder.add_local(ptr_type, None);
                builder.emit_borrow(borrow, place.clone());

                if method_name == "unwrap_or" {
                    // unwrap_or(default) → (tag == 0) ? data.Variant._0 : default
                    let default_val = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    let extern_name = if is_result { "__result_unwrap_or" } else { "__option_unwrap_or" };
                    let dst = ctx.call_extern_tracked(builder,
                        extern_name,
                        vec![FunctionBuilder::copy(borrow), default_val],
                        inner_type,
                    );
                    // If the extracted value is a Move type, zero the Option/Result
                    // to prevent its drop from freeing the inner value's buffer.
                    if is_resource_type_local(dst, builder, &ctx.type_registry) {
                        builder.move_zero(place.clone());
                        ctx.drops.mark_moved(place.local);
                    }
                    return FunctionBuilder::copy(dst);
                } else {
                    // unwrap() / expect() → direct extraction
                    let extern_name = if is_result { "__result_unwrap" } else { "__option_unwrap" };
                    let dst = ctx.call_extern_tracked(builder,
                        extern_name,
                        vec![FunctionBuilder::copy(borrow)],
                        inner_type,
                    );
                    // If the extracted value is a Move type, zero the Option/Result
                    // to prevent its drop from freeing the inner value's buffer.
                    if is_resource_type_local(dst, builder, &ctx.type_registry) {
                        builder.move_zero(place.clone());
                        ctx.drops.mark_moved(place.local);
                    }
                    // Note: Ptr(T) results for Recursive types are NOT cloned here.
                    // The VarDecl/assign site handles cloning via the Ptr→T auto-clone
                    // path when the caller uses an explicit type (String x = ...).
                    // auto x = ... keeps the Ptr (zero-cost borrow).
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // Channel methods: send, recv, close, poll_recv — dispatch via C wrapper functions.
    // Channel is emitted as a pointer typedef; methods take &self (Channel__T*).
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref chan_tn) = recv_type_name {
            if chan_tn.starts_with("Channel__") {
                let elem_suffix = chan_tn.strip_prefix("Channel__").unwrap_or("int64_t");
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                // Get a mutable pointer to the channel local (Channel__T*)
                let ch_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let ptr_type = ctx.register_mut_ptr_type(recv_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, place.clone());
                    Operand::Copy(Place::local(ptr_local))
                } else {
                    // Temp local needed
                    let temp = builder.add_local(recv_type, None);
                    builder.assign(Place::local(temp), recv.clone());
                    let ptr_type = ctx.register_mut_ptr_type(recv_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, Place::local(temp));
                    Operand::Copy(Place::local(ptr_local))
                };
                match method_name {
                    "send" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        let send_fn = format!("{chan_tn}__send");
                        builder.call_void(&send_fn, vec![ch_ptr, val]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "recv" => {
                        let elem_type = ctx.type_mapper.lookup_named(elem_suffix)
                            .unwrap_or(I64_TYPE);
                        let recv_fn = format!("{chan_tn}__recv");
                        let dst = builder.call(&recv_fn, vec![ch_ptr], elem_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "close" => {
                        let close_fn = format!("{chan_tn}__close");
                        builder.call_void(&close_fn, vec![ch_ptr]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "poll_recv" if args.len() >= 2 => {
                        // poll_recv(&self, &mut out, waker)
                        let out_ptr = lower_expr(ctx, builder, &args[0].node.value);
                        let waker = lower_expr(ctx, builder, &args[1].node.value);
                        let poll_fn = format!("{chan_tn}__poll_recv");
                        let dst = builder.call(&poll_fn, vec![ch_ptr, out_ptr, waker], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "recv_timeout" if !args.is_empty() => {
                        let ms = lower_expr(ctx, builder, &args[0].node.value);
                        let option_name = format!("Option__{elem_suffix}");
                        let option_type = ctx.lookup_type_by_name(&option_name)
                            .unwrap_or(I64_TYPE);
                        let recv_fn = format!("{chan_tn}__recv_timeout");
                        let dst = builder.call(&recv_fn, vec![ch_ptr, ms], option_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "len" => {
                        let len_fn = format!("{chan_tn}__len");
                        let dst = builder.call(&len_fn, vec![ch_ptr], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "capacity" => {
                        let cap_fn = format!("{chan_tn}__capacity");
                        let dst = builder.call(&cap_fn, vec![ch_ptr], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "is_closed" => {
                        let closed_fn = format!("{chan_tn}__is_closed");
                        let dst = builder.call(&closed_fn, vec![ch_ptr], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // Shared[T] methods: clone, get, strong_count, downgrade — via C wrapper functions.
    // Shared[T] is a Copy pointer typedef (GorgetShared*); methods pass value directly.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref stn) = recv_type_name {
            if stn.starts_with("Shared__") {
                let elem_suffix = stn.strip_prefix("Shared__").unwrap_or("int64_t");
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                match method_name {
                    "clone" => {
                        let clone_fn = format!("{stn}__clone");
                        let dst = builder.call(&clone_fn, vec![recv], recv_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "get" => {
                        let elem_type = ctx.type_mapper.lookup_named(elem_suffix)
                            .unwrap_or(I64_TYPE);
                        let get_fn = format!("{stn}__get");
                        let dst = builder.call(&get_fn, vec![recv], elem_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "strong_count" => {
                        let count_fn = format!("{stn}__strong_count");
                        let dst = builder.call(&count_fn, vec![recv], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "downgrade" => {
                        let weak_name = format!("Weak__{elem_suffix}");
                        let weak_type = if let Some(tid) = ctx.type_mapper.lookup_named(&weak_name) {
                            tid
                        } else {
                            let inner_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);
                            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(weak_name.clone()));
                            ctx.type_mapper.register_named(weak_name.clone(), tid);
                            ensure_weak_type_def(ctx, &weak_name, inner_type);
                            tid
                        };
                        let downgrade_fn = format!("{stn}__downgrade");
                        let dst = builder.call(&downgrade_fn, vec![recv], weak_type);
                        return FunctionBuilder::copy(dst);
                    }
                    // Shared[Vector[T]] element access — at/set_at/slen
                    "at" if elem_suffix.starts_with("Vector__") => {
                        let inner_elem = elem_suffix.strip_prefix("Vector__").unwrap_or("int64_t");
                        let elem_type = ctx.type_mapper.lookup_named(inner_elem).unwrap_or(I64_TYPE);
                        let idx = lower_expr(ctx, builder, &args[0].node.value);
                        let at_fn = format!("{stn}__at");
                        let dst = builder.call(&at_fn, vec![recv, idx], elem_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "set_at" if elem_suffix.starts_with("Vector__") => {
                        let idx = lower_expr(ctx, builder, &args[0].node.value);
                        let val = lower_expr(ctx, builder, &args[1].node.value);
                        let set_fn = format!("{stn}__set_at");
                        builder.call_void(&set_fn, vec![recv, idx, val]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "slen" if elem_suffix.starts_with("Vector__") => {
                        let slen_fn = format!("{stn}__slen");
                        let dst = builder.call(&slen_fn, vec![recv], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
            // Weak[T] methods: clone, upgrade — via C wrapper functions.
            if stn.starts_with("Weak__") {
                let elem_suffix = stn.strip_prefix("Weak__").unwrap_or("int64_t");
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                match method_name {
                    "clone" => {
                        let clone_fn = format!("{stn}__clone");
                        let dst = builder.call(&clone_fn, vec![recv], recv_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "upgrade" => {
                        // Returns Option[Shared[T]] — need to build the Option type
                        let shared_name = format!("Shared__{elem_suffix}");
                        let _shared_type = if let Some(tid) = ctx.type_mapper.lookup_named(&shared_name) {
                            tid
                        } else {
                            let inner_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);
                            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(shared_name.clone()));
                            ctx.type_mapper.register_named(shared_name.clone(), tid);
                            ensure_shared_type_def(ctx, &shared_name, inner_type);
                            tid
                        };
                        let option_name = format!("Option__{shared_name}");
                        let option_type = if let Some(tid) = ctx.type_mapper.lookup_named(&option_name) {
                            tid
                        } else {
                            let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(option_name.clone()));
                            ctx.type_mapper.register_named(option_name.clone(), tid);
                            tid
                        };
                        let upgrade_fn = format!("{stn}__upgrade");
                        let dst = builder.call(&upgrade_fn, vec![recv], option_type);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // Mutex[T] methods: lock — dispatch via C wrapper functions.
    // Mutex[T] is a Copy pointer typedef (GorgetMutex*).
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref mtn) = recv_type_name {
            if mtn.starts_with("Mutex__") {
                let elem_suffix = mtn.strip_prefix("Mutex__").unwrap_or("int64_t");
                let guard_name = format!("Guard__{elem_suffix}");
                let guard_type = if let Some(tid) = ctx.type_mapper.lookup_named(&guard_name) {
                    tid
                } else {
                    let inner_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);
                    let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(guard_name.clone()));
                    ctx.type_mapper.register_named(guard_name.clone(), tid);
                    ensure_guard_type_def(ctx, &guard_name, inner_type);
                    tid
                };
                if method_name == "lock" {
                    let lock_fn = format!("{mtn}__lock");
                    let dst = builder.call(&lock_fn, vec![recv], guard_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // AtomicInt methods — pass the GorgetAtomicInt* receiver directly by value.
    // fn_name_map maps AtomicInt__method → gorget_atomic_int_method in the C backend.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref atn) = recv_type_name {
            if atn == "AtomicInt" {
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                match method_name {
                    "load" => {
                        let dst = builder.call("AtomicInt__load", vec![recv], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "store" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        builder.call_void("AtomicInt__store", vec![recv, val]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "add" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        let dst = builder.call("AtomicInt__add", vec![recv, val], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "sub" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        let dst = builder.call("AtomicInt__sub", vec![recv, val], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "compare_exchange" if args.len() == 2 => {
                        let expected = lower_expr(ctx, builder, &args[0].node.value);
                        let desired  = lower_expr(ctx, builder, &args[1].node.value);
                        let dst = builder.call("AtomicInt__compare_exchange", vec![recv, expected, desired], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => { let _ = recv_type; }
                }
            }
        }
    }

    // AtomicBool methods — pass the GorgetAtomicBool* receiver directly by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref atn) = recv_type_name {
            if atn == "AtomicBool" {
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                match method_name {
                    "load" => {
                        let dst = builder.call("AtomicBool__load", vec![recv], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "store" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        builder.call_void("AtomicBool__store", vec![recv, val]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "swap" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        let dst = builder.call("AtomicBool__swap", vec![recv, val], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "compare_exchange" if args.len() == 2 => {
                        let expected = lower_expr(ctx, builder, &args[0].node.value);
                        let desired  = lower_expr(ctx, builder, &args[1].node.value);
                        let dst = builder.call("AtomicBool__compare_exchange", vec![recv, expected, desired], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => { let _ = recv_type; }
                }
            }
        }
    }

    // Barrier methods — pass the GorgetBarrier* receiver directly by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref btn) = recv_type_name {
            if btn == "Barrier" {
                if method_name == "wait" {
                    builder.call_void("Barrier__wait", vec![recv]);
                    return Operand::Constant(Constant::Unit);
                }
            }
        }
    }

    // CondVar methods — receiver is GorgetCondVar* (pointer), passed by value.
    // CondVar.wait(g) passes a mutable pointer to the Guard so the C bridge can access
    // g->mutex->lock for pthread_cond_wait (gorget_condvar_wait_guard in MUTEX_RUNTIME).
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref ctn) = recv_type_name {
            if ctn == "CondVar" {
                match method_name {
                    "notify_one" => {
                        builder.call_void("CondVar__notify_one", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "notify_all" => {
                        builder.call_void("CondVar__notify_all", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "wait" if !args.is_empty() => {
                        // Lower the guard argument and pass a mutable pointer to it,
                        // so the C bridge can reach g->mutex->lock.
                        let guard_val = lower_expr(ctx, builder, &args[0].node.value);
                        let guard_type = infer_operand_type_full(ctx, &guard_val, builder);
                        let guard_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = guard_val {
                            let ptr_type = ctx.register_mut_ptr_type(guard_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow_mut(ptr_local, place.clone());
                            Operand::Copy(Place::local(ptr_local))
                        } else {
                            let temp = builder.add_local(guard_type, None);
                            builder.assign(Place::local(temp), guard_val);
                            let ptr_type = ctx.register_mut_ptr_type(guard_type);
                            let ptr_local = builder.add_local(ptr_type, None);
                            builder.emit_borrow_mut(ptr_local, Place::local(temp));
                            Operand::Copy(Place::local(ptr_local))
                        };
                        builder.call_void("CondVar__wait", vec![recv, guard_ptr]);
                        return Operand::Constant(Constant::Unit);
                    }
                    _ => {}
                }
            }
        }
    }

    // WaitGroup methods — receiver is GorgetWaitGroup* (pointer), passed by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref wtn) = recv_type_name {
            if wtn == "WaitGroup" {
                match method_name {
                    "add" if !args.is_empty() => {
                        let n = lower_expr(ctx, builder, &args[0].node.value);
                        builder.call_void("WaitGroup__add", vec![recv, n]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "done" => {
                        builder.call_void("WaitGroup__done", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "wait" => {
                        builder.call_void("WaitGroup__wait", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    _ => {}
                }
            }
        }
    }

    // Semaphore methods — receiver is GorgetSemaphore* (pointer), passed by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref stn) = recv_type_name {
            if stn == "Semaphore" {
                match method_name {
                    "acquire" => {
                        builder.call_void("Semaphore__acquire", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "release" => {
                        builder.call_void("Semaphore__release", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "try_acquire" => {
                        let dst = builder.call("Semaphore__try_acquire", vec![recv], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // OnceFlag methods — receiver is GorgetOnceFlag* (pointer), passed by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref otn) = recv_type_name {
            if otn == "OnceFlag" {
                match method_name {
                    "do_once" => {
                        let dst = builder.call("OnceFlag__do_once", vec![recv], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    "is_done" => {
                        let dst = builder.call("OnceFlag__is_done", vec![recv], BOOL_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // RWLock[T] methods: read, write — pass the GorgetRWLock* receiver directly by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref rtn) = recv_type_name {
            if rtn.starts_with("RWLock__") {
                let elem_suffix = rtn.strip_prefix("RWLock__").unwrap_or("int64_t");
                match method_name {
                    "read" => {
                        let rg_name = format!("ReadGuard__{elem_suffix}");
                        let rg_type = ctx.type_mapper.lookup_named(&rg_name).unwrap_or(UNIT_TYPE);
                        let read_fn = format!("{rtn}__read");
                        let dst = builder.call(&read_fn, vec![recv], rg_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "write" => {
                        let wg_name = format!("WriteGuard__{elem_suffix}");
                        let wg_type = ctx.type_mapper.lookup_named(&wg_name).unwrap_or(UNIT_TYPE);
                        let write_fn = format!("{rtn}__write");
                        let dst = builder.call(&write_fn, vec![recv], wg_type);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // ReadGuard[T] / WriteGuard[T] methods: get, set — pass by mutable reference (like Guard).
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref gtn) = recv_type_name {
            if gtn.starts_with("ReadGuard__") || gtn.starts_with("WriteGuard__") {
                let elem_suffix = if let Some(s) = gtn.strip_prefix("ReadGuard__") { s }
                    else { gtn.strip_prefix("WriteGuard__").unwrap_or("int64_t") };
                let elem_type = c_suffix_to_type_id(elem_suffix, ctx);
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                let guard_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let pt = ctx.register_mut_ptr_type(recv_type);
                    let pl = builder.add_local(pt, None);
                    builder.emit_borrow_mut(pl, place.clone());
                    Operand::Copy(Place::local(pl))
                } else {
                    let tmp = builder.add_local(recv_type, None);
                    builder.assign(Place::local(tmp), recv.clone());
                    let pt = ctx.register_mut_ptr_type(recv_type);
                    let pl = builder.add_local(pt, None);
                    builder.emit_borrow_mut(pl, Place::local(tmp));
                    Operand::Copy(Place::local(pl))
                };
                match method_name {
                    "get" => {
                        let get_fn = format!("{gtn}__get");
                        let dst = builder.call(&get_fn, vec![guard_ptr], elem_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "set" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        let set_fn = format!("{gtn}__set");
                        builder.call_void(&set_fn, vec![guard_ptr, val]);
                        return Operand::Constant(Constant::Unit);
                    }
                    _ => {}
                }
            }
        }
    }

    // Thread[T] methods: join (Move, pass by value), id (pass by value — pointer).
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref ttn) = recv_type_name {
            if ttn.starts_with("Thread__") {
                let elem_suffix = ttn.strip_prefix("Thread__").unwrap_or("int64_t");
                let is_void = elem_suffix == "void";
                match method_name {
                    "join" => {
                        let join_fn = format!("{ttn}__join");
                        if is_void {
                            builder.call_void(&join_fn, vec![recv]);
                            return Operand::Constant(Constant::Unit);
                        } else {
                            // Map C type name back to TypeId (primitives first, then registry)
                            let ret_type = match elem_suffix {
                                "int64_t" => I64_TYPE,
                                "double"  => F64_TYPE,
                                "bool"    => BOOL_TYPE,
                                "int32_t" => I32_TYPE,
                                _ => ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE),
                            };
                            let dst = builder.call(&join_fn, vec![recv], ret_type);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                    "id" => {
                        let id_fn = format!("{ttn}__id");
                        let dst = builder.call(&id_fn, vec![recv], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // Process methods — receiver is already GorgetProcess* (pointer), pass directly by value.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if recv_type_name.as_deref() == Some("Process") {
            let gs_type = ctx.type_mapper.owned_string_type;
            match method_name {
                "wait" => {
                    let dst = builder.call("Process__wait", vec![recv], I64_TYPE);
                    return FunctionBuilder::copy(dst);
                }
                "kill" => {
                    builder.call_void("Process__kill", vec![recv]);
                    return Operand::Constant(Constant::Unit);
                }
                "pid" => {
                    let dst = builder.call("Process__pid", vec![recv], I64_TYPE);
                    return FunctionBuilder::copy(dst);
                }
                "write_stdin" if !args.is_empty() => {
                    let data = lower_expr(ctx, builder, &args[0].node.value);
                    builder.call_void("Process__write_stdin", vec![recv, data]);
                    return Operand::Constant(Constant::Unit);
                }
                "close_stdin" => {
                    builder.call_void("Process__close_stdin", vec![recv]);
                    return Operand::Constant(Constant::Unit);
                }
                "read_stdout" => {
                    let dst = builder.call("Process__read_stdout", vec![recv], gs_type);
                    return FunctionBuilder::copy(dst);
                }
                "read_stderr" => {
                    let dst = builder.call("Process__read_stderr", vec![recv], gs_type);
                    return FunctionBuilder::copy(dst);
                }
                _ => {}
            }
        }
    }

    // Guard[T] methods: get, set — dispatch via C wrapper functions.
    // Guard[T] is a Move struct (gorget_guard_t value type).
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref gtn) = recv_type_name {
            if gtn.starts_with("Guard__") {
                let elem_suffix = gtn.strip_prefix("Guard__").unwrap_or("int64_t");
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                // Get a pointer to the guard local so we can pass by reference
                let guard_ptr = if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let ptr_type = ctx.register_mut_ptr_type(recv_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, place.clone());
                    Operand::Copy(Place::local(ptr_local))
                } else {
                    let temp = builder.add_local(recv_type, None);
                    builder.assign(Place::local(temp), recv.clone());
                    let ptr_type = ctx.register_mut_ptr_type(recv_type);
                    let ptr_local = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(ptr_local, Place::local(temp));
                    Operand::Copy(Place::local(ptr_local))
                };
                match method_name {
                    "get" => {
                        let elem_type = c_suffix_to_type_id(elem_suffix, ctx);
                        let get_fn = format!("{gtn}__get");
                        let dst = builder.call(&get_fn, vec![guard_ptr], elem_type);
                        return FunctionBuilder::copy(dst);
                    }
                    "set" if !args.is_empty() => {
                        let val = lower_expr(ctx, builder, &args[0].node.value);
                        let set_fn = format!("{gtn}__set");
                        builder.call_void(&set_fn, vec![guard_ptr, val]);
                        return Operand::Constant(Constant::Unit);
                    }
                    _ => {}
                }
            }
        }
    }

    // TaskGroup methods: spawn, join — dispatch via C runtime functions.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref ttn) = recv_type_name {
            if ttn == "TaskGroup" {
                match method_name {
                    "spawn" if !args.is_empty() => {
                        let task_op = lower_expr(ctx, builder, &args[0].node.value);
                        builder.call_void("gorget_task_group_submit", vec![recv, task_op]);
                        return Operand::Constant(Constant::Unit);
                    }
                    "join" => {
                        builder.call_void("gorget_task_group_join", vec![recv]);
                        return Operand::Constant(Constant::Unit);
                    }
                    _ => {}
                }
            }
        }
    }

    // .is_some() / .is_none() / .is_ok() / .is_error() on Option/Result → tag check
    // On non-Option/Result types → pass-through (return false)
    if matches!(method_name, "is_some" | "is_none" | "is_ok" | "is_error") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_option_or_result = type_name.as_ref()
            .map(|n| n.starts_with("Option") || n.starts_with("Result"))
            .unwrap_or(false);
        if !is_option_or_result {
            // Not an Option/Result — return false
            return Operand::Constant(Constant::Bool(false));
        }
        // Tag check: is_some/is_ok → tag == 0; is_none/is_err → tag != 0
        if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            let ptr_type = ctx.register_ptr_type(
                infer_operand_type_full(ctx, &recv, builder),
            );
            let borrow = builder.add_local(ptr_type, None);
            builder.emit_borrow(borrow, place.clone());
            let extern_name = match method_name {
                "is_some" | "is_ok" => "__option_is_some",
                _ => "__option_is_none",
            };
            let dst = builder.call_extern(
                extern_name,
                vec![FunctionBuilder::copy(borrow)],
                BOOL_TYPE,
            );
            return FunctionBuilder::copy(dst);
        }
    }

    // Handle .len() for strings and collections
    if method_name == "len" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if recv_type == ctx.type_mapper.string_view_type || recv_type == ctx.type_mapper.owned_string_type {
            let dst = builder.call_extern(
                "gorget_str_codepoint_count",
                vec![recv],
                I64_TYPE,
            );
            return FunctionBuilder::copy(dst);
        }
        // GorgetArray: .len is field 1 (element count, no function call needed)
        // Resolve through Ptr for field-load refs (Ptr(Vector__T) → Vector__T)
        let resolved_type = ctx.pointee_type(recv_type).unwrap_or(recv_type);
        let is_ptr_recv = resolved_type != recv_type;
        if let Some(GirType::Named(name)) = ctx.type_registry.get(resolved_type) {
            if name.starts_with("GorgetArray") || name.starts_with("Vector__") {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let mut len_place = place.clone();
                    if is_ptr_recv {
                        len_place.projections.push(Projection::Deref);
                    }
                    len_place.projections.push(Projection::Field(1));
                    let tmp = builder.add_local(I64_TYPE, None);
                    builder.assign(Place::local(tmp), Operand::Copy(len_place));
                    return FunctionBuilder::copy(tmp);
                }
            }
        }
    }
    // Handle .byte_len() for strings → direct field access
    if method_name == "byte_len" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if recv_type == ctx.type_mapper.string_view_type || recv_type == ctx.type_mapper.owned_string_type {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                let mut len_place = place.clone();
                len_place.projections.push(Projection::Field(1));
                let tmp = builder.add_local(I64_TYPE, None);
                builder.assign(Place::local(tmp), Operand::Copy(len_place));
                return FunctionBuilder::copy(tmp);
            }
        }
    }

    // Determine the receiver type and mangle the method name
    let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);

    // After String/str unification, owned strings (GorgetString) use the same
    // methods as view strings (Str). Normalize type name for method mangling.
    let type_name = type_name.map(|n| if n == "GorgetString" { "GorgetStringView".to_string() } else { n });

    if let Some(type_name) = type_name {
        // Box[T].get() → call Box__T__get(b) passing value directly (not borrow)
        if type_name.starts_with("Box__") && method_name == "get" {
            let inner_type_name = &type_name["Box__".len()..];
            let inner_type = ctx.type_mapper.lookup_named(inner_type_name).unwrap_or(I64_TYPE);
            let mangled = format!("{type_name}__get");
            let dst = builder.call(mangled, vec![recv], inner_type);
            return FunctionBuilder::copy(dst);
        }

        // Box[T].set(val) → call Box__T__set(&b, val) passing borrow of box + value
        if type_name.starts_with("Box__") && method_name == "set" && !args.is_empty() {
            let val = lower_expr(ctx, builder, &args[0].node.value);
            let mangled = format!("{type_name}__set");
            // set takes (&box, val) — pass pointer to the box local
            let recv_place = match &recv {
                Operand::Copy(p) | Operand::Move(p) => p.clone(),
                _ => {
                    // If recv is not a place, store it in a temp
                    let box_type = ctx.type_mapper.lookup_named(&type_name).unwrap_or(I64_TYPE);
                    let tmp = builder.add_local(box_type, None);
                    builder.assign(Place::local(tmp), recv);
                    Place::local(tmp)
                }
            };
            let ptr_type = ctx.type_mapper.lookup_named(&type_name).unwrap_or(I64_TYPE);
            let recv_ref = builder.borrow_mut(recv_place, ptr_type);
            builder.call_void(mangled, vec![FunctionBuilder::copy(recv_ref), val]);
            return Operand::Constant(Constant::Unit);
        }

        // Box[Trait] method dispatch — look up return type from VTable TypeDef
        if type_name.starts_with("Box__") {
            let inner = &type_name["Box__".len()..];
            let vtable_name = format!("{inner}_VTable");
            if let Some(vtable_def) = ctx.type_registry.get_type_def(&vtable_name) {
                if let crate::ir::types::TypeDefKind::Struct(ref s) = vtable_def.kind {
                    for field in &s.fields {
                        if field.name == method_name {
                            // Found the method in the VTable — extract return type from FnPtr
                            if let Some(crate::ir::types::GirType::FnPtr { return_type, .. }) = ctx.type_registry.get(field.type_id) {
                                let ret_type = *return_type;
                                let mangled = format!("{type_name}__{method_name}");
                                // Pass borrow of the Box local (or forward pointer if already a pointer)
                                let recv_self = if let Operand::Copy(ref p) | Operand::Move(ref p) = recv {
                                    let local_type = builder.locals[p.local.0 as usize].type_id;
                                    if matches!(ctx.type_registry.get(local_type), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))) {
                                        // Already a pointer (pass-by-pointer param) — forward directly
                                        FunctionBuilder::copy(p.local)
                                    } else {
                                        let ptr_type = ctx.register_ptr_type(
                                            ctx.type_mapper.lookup_named(&type_name).unwrap_or(I64_TYPE)
                                        );
                                        let recv_ref = builder.borrow(p.clone(), ptr_type);
                                        FunctionBuilder::copy(recv_ref)
                                    }
                                } else {
                                    let box_type = ctx.type_mapper.lookup_named(&type_name).unwrap_or(I64_TYPE);
                                    let tmp = builder.add_local(box_type, None);
                                    builder.assign(Place::local(tmp), recv);
                                    let ptr_type = ctx.register_ptr_type(box_type);
                                    let recv_ref = builder.borrow(Place::local(tmp), ptr_type);
                                    FunctionBuilder::copy(recv_ref)
                                };
                                let mut call_args = vec![recv_self];
                                for arg in args {
                                    call_args.push(lower_expr(ctx, builder, &arg.node.value));
                                }
                                if ret_type == UNIT_TYPE {
                                    builder.call_void(mangled, call_args);
                                    return Operand::Constant(Constant::Unit);
                                } else {
                                    let dst = builder.call(mangled, call_args, ret_type);
                                    return FunctionBuilder::copy(dst);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Dict/HashMap .items() → register tuple type for (K, V) and return Vector[tuple]
        if method_name == "items" && (type_name.starts_with("Dict__") || type_name.starts_with("HashMap__")) {
            // Extract key and value type names from Dict__K__V
            let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
            let rest = &type_name[prefix.len()..];
            // Split at first __ to get key type
            if let Some(sep_pos) = rest.find("__") {
                let key_name = &rest[..sep_pos];
                let val_name = &rest[sep_pos + 2..];
                let key_type = ctx.type_mapper.lookup_named(key_name).unwrap_or(I64_TYPE);
                let val_type = ctx.type_mapper.lookup_named(val_name).unwrap_or(I64_TYPE);
                let tuple_type_id = register_tuple_type(ctx, &[key_type, val_type]);
                let tuple_name = ctx.type_name_for_id(tuple_type_id).unwrap_or("int64_t").to_string();
                // Register Vector[tuple] type name
                let vec_name = format!("Vector__{tuple_name}");
                if ctx.lookup_type_by_name(&vec_name).is_none() {
                    let vec_type = ctx.type_registry.insert(crate::ir::types::GirType::Named(vec_name.clone()));
                    ctx.type_mapper.register_named(vec_name.clone(), vec_type);
                }
                // Also register Option[tuple] for .get() calls
                let option_name = format!("Option__{tuple_name}");
                if ctx.lookup_type_by_name(&option_name).is_none() {
                    ctx.ensure_option_type_registered(&option_name, tuple_type_id);
                }
            }
        }

        // Iterator adapter expansion: fold/map/filter/collect on Iterator types
        if matches!(method_name, "fold" | "map" | "filter" | "collect") {
            if let Some(result) = try_lower_iterator_adapter(
                ctx, builder, &type_name, method_name, recv.clone(), args,
            ) {
                return result;
            }
        }

        // .clone() on resource types: route to the generated {Name}__clone function.
        // The clone function takes a const pointer and returns an owned copy.
        if method_name == "clone" && args.is_empty() {
            // Resolve the receiver's concrete type
            let recv_type_id = match &recv {
                Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                    let lid = p.local.0 as usize;
                    if lid < builder.locals.len() {
                        let tid = builder.locals[lid].type_id;
                        // Unwrap Ptr(T) → T for borrowed receivers
                        match ctx.type_registry.get(tid) {
                            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
                            _ => tid,
                        }
                    } else { UNIT_TYPE }
                }
                _ => UNIT_TYPE,
            };
            if let Some(clone_fn) = ctx.clone_fn_for_ptr(recv_type_id) {
                // Build the call: clone_fn(&receiver) → owned T
                let ptr_arg = match &recv {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                        let lid = p.local.0 as usize;
                        let tid = builder.locals[lid].type_id;
                        if matches!(ctx.type_registry.get(tid), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))) {
                            // Already a pointer — pass directly
                            FunctionBuilder::copy(p.local)
                        } else {
                            // Value — take a borrow
                            let pt = ctx.register_ptr_type(recv_type_id);
                            let pl = builder.add_local(pt, None);
                            builder.emit_borrow(pl, p.clone());
                            FunctionBuilder::copy(pl)
                        }
                    }
                    _ => recv.clone(),
                };
                let dst = ctx.call_tracked(builder, clone_fn, vec![ptr_arg], recv_type_id);
                return FunctionBuilder::copy(dst);
            }
            // String .clone(): use gorget_string_clone_to_owned to produce an
            // independent owned copy (gorget_string_clone preserves views).
            let is_string = recv_type_id == ctx.type_mapper.owned_string_type
                || recv_type_id == ctx.type_mapper.string_view_type;
            if is_string {
                let owned_type = ctx.type_mapper.owned_string_type;
                let ptr_arg = match &recv {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
                        let lid = p.local.0 as usize;
                        let tid = builder.locals[lid].type_id;
                        if matches!(ctx.type_registry.get(tid), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))) {
                            FunctionBuilder::copy(p.local)
                        } else {
                            let pt = ctx.register_ptr_type(recv_type_id);
                            let pl = builder.add_local(pt, None);
                            builder.emit_borrow(pl, p.clone());
                            FunctionBuilder::copy(pl)
                        }
                    }
                    _ => recv.clone(),
                };
                let dst = ctx.call_tracked(builder, "gorget_string_clone_to_owned", vec![ptr_arg], owned_type);
                return FunctionBuilder::copy(dst);
            }
            // Non-resource type: .clone() is a trivial copy (no deep clone needed)
            return recv;
        }

        let mangled = format!("{type_name}__{method_name}");

        // Build args: &receiver + explicit args
        let mut call_args = Vec::new();

        // Create a borrow of the receiver for the self parameter.
        // Mutating methods need a mutable borrow (&self → MutPtr).
        let is_mutating = matches!(method_name,
            "push" | "pop" | "put" | "remove" | "insert" | "extend"
            | "clear" | "sort" | "reverse" | "append" | "set"
            | "push_back" | "push_front" | "pop_back" | "pop_front"
            | "add" | "push_line" | "push_str" | "push_char"
        );

        // Determine if we need a mutable borrow (from explicit list or fn_sigs)
        let needs_mut = is_mutating || ctx.fn_sigs.get(&mangled)
            .and_then(|(params, _)| params.first())
            .map(|&p| matches!(ctx.type_registry.get(p), Some(GirType::MutPtr(_))))
            .unwrap_or(false);

        // If receiver is a field access, borrow the field in-place instead of
        // borrowing a copy (which would mutate the copy, not the original).
        if let Some((field_place, field_type_id)) = &field_place_info {
            if needs_mut {
                let pt = ctx.register_mut_ptr_type(*field_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow_mut(pl, field_place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            } else {
                let pt = ctx.register_ptr_type(*field_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow(pl, field_place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            }
        } else if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            let recv_type_id = if (place.local.0 as usize) < builder.locals.len() {
                builder.locals[place.local.0 as usize].type_id
            } else {
                UNIT_TYPE
            };
            // If receiver is already a pointer (e.g., self in equip methods), pass directly
            let is_ptr = matches!(
                ctx.type_registry.get(recv_type_id),
                Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
            );
            if is_ptr {
                call_args.push(FunctionBuilder::copy(place.local));
            } else if recv_type_id.0 < crate::ir::types::PRIMITIVE_TYPE_COUNT {
                // Scalar types (int, float, bool, uint8, etc.) — pass by value, not by reference.
                // Equip methods on scalars (e.g. uint8.is_alpha()) expect the value directly.
                call_args.push(recv.clone());
            } else if needs_mut {
                let pt = ctx.register_mut_ptr_type(recv_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow_mut(pl, place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            } else {
                let pt = ctx.register_ptr_type(recv_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow(pl, place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            }
        } else if matches!(&recv, Operand::Constant(Constant::Str(_))) {
            // String constant receiver: materialize into a local for borrow
            let string_view_type = ctx.type_mapper.string_view_type;
            let tmp_local = builder.add_local(string_view_type, None);
            builder.assign(Place::local(tmp_local), recv);
            let pt = ctx.register_ptr_type(string_view_type);
            let pl = builder.add_local(pt, None);
            builder.emit_borrow(pl, Place::local(tmp_local));
            call_args.push(FunctionBuilder::copy(pl));
        } else {
            call_args.push(recv);
        }

        // Resolve function name: try Type__method first, fallback to Trait_for_Type__method
        let effective_name = if ctx.fn_sigs.contains_key(&mangled) {
            mangled.clone()
        } else {
            let suffix = format!("_for_{type_name}__{method_name}");
            ctx.fn_sigs.keys()
                .find(|k| k.ends_with(&suffix))
                .cloned()
                .unwrap_or(mangled.clone())
        };

        // Pass-by-pointer non-self method args via lower_call_arg.
        // Only pass callee param types for GIR-lowered equip methods (which pass
        // resource-type params by pointer). C runtime methods take values by value, so
        // passing None prevents pass-by-pointer for those.
        let is_gir_method = ctx.gir_equip_methods.contains(&effective_name);
        let method_param_types: Vec<TypeId> = if is_gir_method {
            ctx.fn_sigs.get(effective_name.as_str())
                .map(|(params, _)| params.iter().skip(1).copied().collect())  // skip self ptr
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        // For higher-order methods (filter/map/fold/etc.), set closure parameter type hints
        // so untyped closure params get the correct element type instead of defaulting to I64.
        let prev_hints = std::mem::take(&mut ctx.closure_param_type_hints);
        if matches!(method_name, "filter" | "map" | "flat_map" | "any" | "all" | "each" | "for_each" | "find" | "count" | "reduce" | "enumerate") {
            if let Some(elem_type_id) = extract_elem_type_id_from_type_name(ctx, &type_name) {
                ctx.closure_param_type_hints = vec![elem_type_id];
            }
        } else if method_name == "fold" {
            // fold closure has (accumulator, element) — use element type for both params
            // as a reasonable default. Explicitly-typed params override the hint.
            if let Some(elem_type_id) = extract_elem_type_id_from_type_name(ctx, &type_name) {
                ctx.closure_param_type_hints = vec![elem_type_id, elem_type_id];
            }
        }

        // For and_then/or_else, the closure should return the same Option/Result type
        // as the receiver. Set expected_type so Ok()/Error()/Some()/None() constructors
        // inside the closure body get the correct type.
        let prev_expected = ctx.expected_type;
        if matches!(method_name, "and_then" | "or_else") {
            if let Some(type_id) = ctx.lookup_type_by_name(&type_name) {
                ctx.expected_type = Some(type_id);
            }
        }

        let lowered_method_args: Vec<Operand> = args.iter()
            .enumerate()
            .map(|(i, arg)| {
                let callee_pt = method_param_types.get(i).copied();
                // Method args: i is 0-based for non-self args, but fn_param_ownerships
                // includes self at index 0, so offset by 1.
                lower_call_arg(ctx, builder, arg, callee_pt, &effective_name, i + 1)
            })
            .collect();
        call_args.extend(lowered_method_args.iter().cloned());

        // Restore previous hints and expected type
        ctx.closure_param_type_hints = prev_hints;
        ctx.expected_type = prev_expected;

        // For Vector.zip(other_vec), register tuple and result vector types
        if method_name == "zip" && type_name.starts_with("Vector__") {
            let self_elem = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            // Get the other vector's element type from the first explicit arg
            let other_elem_name = if let Some(arg_op) = lowered_method_args.first() {
                if let Operand::Copy(p) | Operand::Move(p) = arg_op {
                    let type_id = builder.locals[p.local.0 as usize].type_id;
                    let type_str = crate::ir::types::format_type_for_mangle(type_id, &ctx.type_registry);
                    type_str.strip_prefix("Vector__").unwrap_or(&type_str).to_string()
                } else { "int64_t".to_string() }
            } else { "int64_t".to_string() };
            // Register the tuple type
            let self_type = resolve_inner_type(ctx, self_elem);
            let other_type = resolve_inner_type(ctx, &other_elem_name);
            let tuple_type_id = register_tuple_type(ctx, &[self_type, other_type]);
            let tuple_name = ctx.type_name_for_id(tuple_type_id).unwrap_or("int64_t").to_string();
            // Register Vector[Tuple] type
            let vec_name = format!("Vector__{tuple_name}");
            if ctx.lookup_type_by_name(&vec_name).is_none() {
                let vec_type = ctx.type_registry.insert(crate::ir::types::GirType::Named(vec_name.clone()));
                ctx.type_mapper.register_named(vec_name, vec_type);
            }
        }

        // Borrowing methods (get/first/last) on resource-type elements: Option__Ref_T (Ptr payload).
        // Consuming methods (pop/remove) and primitive elements: Option__T (value payload).
        let fn_sig_ret = ctx.fn_sigs.get(&effective_name).map(|(_, ret)| *ret);
        if matches!(method_name, "get" | "first" | "last" | "remove" | "pop")
            && (type_name.starts_with("Vector__") || type_name == "GorgetArray")
        {
            let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            let inner_type = resolve_inner_type(ctx, elem_type_name);
            let is_borrowing = matches!(method_name, "get" | "first" | "last");
            let is_resource_elem = ctx.type_registry.is_resource_type(inner_type);
            if is_borrowing && is_resource_elem {
                let option_name = format!("Option__Ref_{elem_type_name}");
                if ctx.lookup_type_by_name(&option_name).is_none() {
                    let ptr_type = ctx.type_registry.insert(GirType::Ptr(inner_type));
                    ctx.ensure_option_type_registered(&option_name, ptr_type);
                }
            } else {
                let option_name = format!("Option__{elem_type_name}");
                if ctx.lookup_type_by_name(&option_name).is_none() {
                    ctx.ensure_option_type_registered(&option_name, inner_type);
                }
            }
        }
        // For Dict/HashMap.get(), auto-register Option[V] so get returns Option.
        // Dict.get() uses value payload (not Ptr) because the GIR→LIR struct pipeline
        // doesn't yet propagate Ptr payload types to the C backend's Option wrapping.
        // dict[key] (IndexLoad) already returns Ptr for resource values via a separate path.
        if method_name == "get"
            && (type_name.starts_with("Dict__") || type_name.starts_with("HashMap__"))
        {
            let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
            if let Some(rest) = type_name.strip_prefix(prefix) {
                if let Some(pos) = rest.find("__") {
                    let val_name = &rest[pos + 2..];
                    let option_name = format!("Option__{val_name}");
                    if ctx.lookup_type_by_name(&option_name).is_none() {
                        let inner_type = resolve_inner_type(ctx, val_name);
                        ctx.ensure_option_type_registered(&option_name, inner_type);
                    }
                }
            }
        }
        // For index_of/find on strings/collections (NOT Regex or user-defined types), register Option[int] return type
        let sentinel_method_key = format!("{type_name}__{method_name}");
        let is_sentinel_wrapped = ctx.sentinel_to_option_methods.contains(&sentinel_method_key);
        if is_sentinel_wrapped {
            let option_name = "Option__int64_t";
            if ctx.lookup_type_by_name(option_name).is_none() {
                ctx.ensure_option_type_registered(option_name, I64_TYPE);
            }
        }
        let ret_type = if let Some(ret) = fn_sig_ret {
            if matches!(method_name, "get" | "first" | "last" | "remove" | "pop")
                && (type_name.starts_with("Vector__") || type_name == "GorgetArray")
            {
                let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
                let inner_type = resolve_inner_type(ctx, elem_type_name);
                let is_borrowing = matches!(method_name, "get" | "first" | "last");
                let is_resource_elem = ctx.type_registry.is_resource_type(inner_type);
                let option_name = if is_borrowing && is_resource_elem {
                    format!("Option__Ref_{elem_type_name}")
                } else {
                    format!("Option__{elem_type_name}")
                };
                ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
            } else if method_name == "get"
                && (type_name.starts_with("Dict__") || type_name.starts_with("HashMap__"))
            {
                // Dict/HashMap.get() returns Option[V]
                let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                if let Some(rest) = type_name.strip_prefix(prefix) {
                    if let Some(pos) = rest.find("__") {
                        let val_name = &rest[pos + 2..];
                        let option_name = format!("Option__{val_name}");
                        ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
                    } else { ret }
                } else { ret }
            } else if method_name == "remove"
                && (type_name.starts_with("Dict__") || type_name.starts_with("HashMap__")
                    || type_name.starts_with("Set__") || type_name.starts_with("HashSet__"))
            {
                BOOL_TYPE
            } else if is_sentinel_wrapped {
                // Stdlib sentinel-to-Option wrapping for find/index_of
                ctx.lookup_type_by_name("Option__int64_t").unwrap_or(ret)
            } else {
                ret
            }
        } else if method_name == "zip" && type_name.starts_with("Vector__") {
            // zip return type: look up the Vector__Tuple__A__B type we just registered
            let self_elem = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            let other_elem_name = if let Some(arg_op) = lowered_method_args.first() {
                if let Operand::Copy(p) | Operand::Move(p) = arg_op {
                    let type_id = builder.locals[p.local.0 as usize].type_id;
                    let type_str = crate::ir::types::format_type_for_mangle(type_id, &ctx.type_registry);
                    type_str.strip_prefix("Vector__").unwrap_or(&type_str).to_string()
                } else { "int64_t".to_string() }
            } else { "int64_t".to_string() };
            let tuple_name = format!("Tuple__{self_elem}__{other_elem_name}");
            let vec_name = format!("Vector__{tuple_name}");
            ctx.lookup_type_by_name(&vec_name)
                .or_else(|| ctx.lookup_type_by_name("GorgetArray"))
                .unwrap_or(I64_TYPE)
        } else {
            // Resolve from the BuiltinTypeProtocol table (lazy substitution + cache).
            // This covers all builtin methods on late-registered types.
            ctx.resolve_builtin_method_return_type(&type_name, method_name)
                .unwrap_or(UNIT_TYPE)
        };

        // Resolve extern bindings: use the C symbol name instead of the Gorget mangled name
        let sig_name = effective_name.clone(); // keep for fn_param_ownerships lookup
        let call_name = if let Some(c_symbol) = ctx.extern_bindings.get(effective_name.as_str()) {
            c_symbol.clone()
        } else {
            effective_name
        };

        // Collect Move-ownership Move-type arg locals for post-call MoveZero.
        // Includes: (a) explicit !arg at call site, (b) bare args whose callee
        // param is declared Move, (c) resource-type args to consuming methods
        // (push, put, set, send) — these transfer ownership to the collection.
        let consuming_method = matches!(method_name,
            "push" | "put" | "set" | "push_back" | "push_front" | "send" | "add");
        let move_zero_locals: Vec<Place> = args.iter()
            .enumerate()
            .filter_map(|(i, arg)| {
                // Check call-site explicit Move
                let call_site_move = matches!(arg.node.ownership, Ownership::Move);
                // Check callee param Move (i+1 because index 0 is self)
                let callee_move = ctx.fn_param_ownerships.get(&sig_name)
                    .and_then(|ownerships| ownerships.get(i + 1))
                    .map(|o| matches!(o, Ownership::Move))
                    .unwrap_or(false);
                if !call_site_move && !callee_move { return None; }
                // Resolve the original local from the arg expression
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

        // For fold, refine ret_type from the init value's type (call_args[1])
        // so the destination local gets the correct type (e.g., double for float fold).
        // Note: fold has call_args = [receiver, init, closure], reduce has [receiver, closure].
        let ret_type = if method_name == "fold" && call_args.len() > 2 {
            match &call_args[1] {
                Operand::Constant(Constant::F64(_)) => F64_TYPE,
                Operand::Constant(Constant::Str(_)) => ctx.type_mapper.string_view_type,
                Operand::Copy(p) | Operand::Move(p) => {
                    let init_type = builder.locals[p.local.0 as usize].type_id;
                    if init_type != I64_TYPE { init_type } else { ret_type }
                }
                _ => ret_type,
            }
        } else {
            ret_type
        };

        // Unregister GorgetString temps when the callee might store str views.
        if super::should_unregister_string_args(ctx, &sig_name, ret_type) {
            super::unregister_gorget_string_args(ctx, builder, &call_args);
        }

        let result = if ret_type == UNIT_TYPE {
            builder.call_void(call_name, call_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = ctx.call_tracked(builder, call_name, call_args, ret_type);
            FunctionBuilder::copy(dst)
        };

        // MoveZero Move-ownership args to transfer ownership (prevent double-free)
        for place in &move_zero_locals {
            builder.move_zero(place.clone());
            ctx.emit_field_origin_zero(builder, place.local);
            ctx.drops.mark_moved(place.local);
        }

        // Zero source fields for resource-type args that came from field loads.
        // This handles e.g. items.push(h.data) where the C backend zeros the temp
        // but not the source field h.data — the struct's scope-end drop would
        // double-free the field without this.
        if is_mutating {
            for op in &lowered_method_args {
                if let Operand::Copy(place) | Operand::Move(place) = op {
                    if place.projections.is_empty()
                        && is_resource_type_local(place.local, builder, &ctx.type_registry)
                    {
                        ctx.emit_field_origin_zero(builder, place.local);
                    }
                }
            }
        }

        result
    } else {
        // Can't determine receiver type — fallback
        Operand::Constant(Constant::Unit)
    }
}


// infer_collection_method_return_type() DELETED — 262 lines of name-based dispatch.
// Replaced by BuiltinTypeProtocol in builtins.rs with lazy substitution + caching
// via ctx.resolve_builtin_method_return_type().

// ---- Iterator Adapter Inline Expansion ----

/// Try to expand fold/map/filter/collect as inline GIR loops for Iterator types.
/// Returns Some(result) if the receiver type has a __next method (implements Iterator),
/// None otherwise (falls through to regular method dispatch).
fn try_lower_iterator_adapter(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_name: &str,
    method_name: &str,
    recv: Operand,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    // Find __next method for this type (same lookup as lower_for_iterable)
    let next_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__next") && k.contains(&format!("_for_{type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{type_name}__next");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        })?;

    // Get Option return type from next()
    let (_, option_ret_type) = ctx.fn_sigs.get(&next_fn_name)?.clone();

    // Extract element type from Option__T
    let option_type_name = ctx.type_registry.type_name(option_ret_type)
        .unwrap_or_else(|| "Option__int64_t".to_string());
    let elem_c_type = option_type_name.strip_prefix("Option__")
        .unwrap_or("int64_t");
    let elem_type = ctx.type_mapper.lookup_named(elem_c_type).unwrap_or(I64_TYPE);

    // Store the iterator for repeated borrow_mut
    let iter_type = infer_operand_type_full(ctx, &recv, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), recv);

    match method_name {
        "fold" => lower_iter_fold(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type, args),
        "map" => lower_iter_map(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type, args),
        "filter" => lower_iter_filter(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type, args),
        "collect" => lower_iter_collect(ctx, builder, iter_local, iter_type, &next_fn_name,
            option_ret_type, elem_type),
        _ => None,
    }
}

/// Build the common iterator loop pattern: call next(), check Option tag, branch.
/// Returns (header_bb, exit_bb, elem_local) with builder positioned at the body block.
fn build_iter_next_loop(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
) -> (BlockId, BlockId, LocalId) {
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();
    builder.jump(header_bb);

    // Header: call next(&mut iter) → Option
    builder.switch_to(header_bb);
    let iter_ptr_type = ctx.register_mut_ptr_type(iter_type);
    let iter_ref = builder.borrow_mut(Place::local(iter_local), iter_ptr_type);
    let opt_result = builder.call_extern(
        next_fn_name,
        vec![FunctionBuilder::copy(iter_ref)],
        option_ret_type,
    );

    // Check tag: if tag != 0 (None), exit
    let tag_val = builder.tag_of(FunctionBuilder::copy(opt_result));
    let is_none = builder.cmp(
        CmpOp::Ne,
        I32_TYPE,
        FunctionBuilder::copy(tag_val),
        Operand::Constant(Constant::I32(0)),
    );
    builder.branch(FunctionBuilder::copy(is_none), exit_bb, body_bb);

    // Body: extract element from Some._0
    builder.switch_to(body_bb);
    let elem_local = builder.enum_field_load(
        Place::local(opt_result),
        "Some",
        0,
        elem_type,
    );

    (header_bb, exit_bb, elem_local)
}

/// Call a closure operand with the given arguments.
/// Handles: known closure struct types, callable parameters, and function refs.
fn call_closure_in_adapter(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    closure_op: &Operand,
    call_args: Vec<Operand>,
    fallback_ret_type: TypeId,
) -> Operand {
    if let Operand::Copy(place) | Operand::Move(place) = closure_op {
        let local_idx = place.local.0 as usize;
        let local_type_id = if local_idx < builder.locals.len() {
            builder.locals[local_idx].type_id
        } else {
            UNIT_TYPE
        };
        if let Some(type_name) = ctx.type_name_for_id(local_type_id) {
            let type_name = type_name.to_string();
            if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                let call_fn = call_fn.to_string();
                // Closure call: __Closure_N__call(&closure, args...)
                let ptr_type = ctx.type_registry.insert(GirType::Ptr(local_type_id));
                let ptr_local = builder.add_local(ptr_type, None);
                builder.emit_borrow(ptr_local, place.clone());
                let mut final_args = vec![FunctionBuilder::copy(ptr_local)];
                final_args.extend(call_args);

                let ret_type = ctx.fn_sigs.get(call_fn.as_str())
                    .map(|(_, ret)| *ret)
                    .unwrap_or(fallback_ret_type);

                if ret_type == UNIT_TYPE {
                    builder.call_void(&call_fn, final_args);
                    return Operand::Constant(Constant::Unit);
                }
                let dst = builder.call(&call_fn, final_args, ret_type);
                return FunctionBuilder::copy(dst);
            }
        }
        // Fallback: __callable_N for callable parameters
        let callable_name = format!("__callable_{}", place.local.0);
        let mut final_args = vec![closure_op.clone()];
        final_args.extend(call_args);
        let dst = builder.call(callable_name, final_args, fallback_ret_type);
        return FunctionBuilder::copy(dst);
    }
    if let Operand::Constant(Constant::FuncRef(name)) = closure_op {
        let ret_type = ctx.fn_sigs.get(name.as_str())
            .map(|(_, ret)| *ret)
            .unwrap_or(fallback_ret_type);
        let dst = builder.call(name.clone(), call_args, ret_type);
        return FunctionBuilder::copy(dst);
    }
    Operand::Constant(Constant::Unit)
}

/// Inline expansion of iter.fold(initial, closure)
fn lower_iter_fold(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    if args.len() < 2 { return None; }

    let initial = lower_expr(ctx, builder, &args[0].node.value);
    let closure_op = lower_expr(ctx, builder, &args[1].node.value);

    let acc_type = infer_operand_type_full(ctx, &initial, builder);
    let acc_local = builder.add_local(acc_type, None);
    builder.assign(Place::local(acc_local), initial);

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: acc = closure(acc, elem)
    let new_acc = call_closure_in_adapter(
        ctx, builder, &closure_op,
        vec![FunctionBuilder::copy(acc_local), FunctionBuilder::copy(elem_local)],
        acc_type,
    );
    builder.assign(Place::local(acc_local), new_acc);
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(acc_local))
}

/// Inline expansion of iter.map(closure) → GorgetArray
fn lower_iter_map(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    if args.is_empty() { return None; }

    let closure_op = lower_expr(ctx, builder, &args[0].node.value);

    // Create result array
    let array_type = ctx.lookup_type_by_name("GorgetArray")
        .or_else(|| ctx.lookup_type_by_name("Vector__int64_t"))
        .unwrap_or(I64_TYPE);
    let result = builder.call_extern(
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        array_type,
    );

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: mapped = closure(elem), push to result
    let mapped = call_closure_in_adapter(
        ctx, builder, &closure_op,
        vec![FunctionBuilder::copy(elem_local)],
        elem_type,
    );
    let mapped_local = builder.add_local(elem_type, None);
    builder.assign(Place::local(mapped_local), mapped);

    let arr_ref = builder.borrow_mut(Place::local(result), ctx.register_mut_ptr_type(array_type));
    let el_ref = builder.borrow(Place::local(mapped_local), ctx.register_ptr_type(elem_type));
    builder.call_extern(
        "gorget_array_push",
        vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
        UNIT_TYPE,
    );
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(result))
}

/// Inline expansion of iter.filter(predicate) → GorgetArray
fn lower_iter_filter(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    if args.is_empty() { return None; }

    let predicate_op = lower_expr(ctx, builder, &args[0].node.value);

    // Create result array
    let array_type = ctx.lookup_type_by_name("GorgetArray")
        .or_else(|| ctx.lookup_type_by_name("Vector__int64_t"))
        .unwrap_or(I64_TYPE);
    let result = builder.call_extern(
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        array_type,
    );

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: if predicate(elem), push to result
    let keep = call_closure_in_adapter(
        ctx, builder, &predicate_op,
        vec![FunctionBuilder::copy(elem_local)],
        BOOL_TYPE,
    );
    let push_bb = builder.new_block();
    builder.branch(keep, push_bb, header_bb);

    builder.switch_to(push_bb);
    let el_local = builder.add_local(elem_type, None);
    builder.assign(Place::local(el_local), FunctionBuilder::copy(elem_local));
    let arr_ref = builder.borrow_mut(Place::local(result), ctx.register_mut_ptr_type(array_type));
    let el_ref = builder.borrow(Place::local(el_local), ctx.register_ptr_type(elem_type));
    builder.call_extern(
        "gorget_array_push",
        vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
        UNIT_TYPE,
    );
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(result))
}

/// Inline expansion of iter.collect() → GorgetArray
fn lower_iter_collect(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_local: LocalId,
    iter_type: TypeId,
    next_fn_name: &str,
    option_ret_type: TypeId,
    elem_type: TypeId,
) -> Option<Operand> {
    // Create result array
    let array_type = ctx.lookup_type_by_name("GorgetArray")
        .or_else(|| ctx.lookup_type_by_name("Vector__int64_t"))
        .unwrap_or(I64_TYPE);
    let result = builder.call_extern(
        "gorget_array_new",
        vec![Operand::Constant(Constant::SizeOf(elem_type))],
        array_type,
    );

    let (header_bb, exit_bb, elem_local) = build_iter_next_loop(
        ctx, builder, iter_local, iter_type, next_fn_name, option_ret_type, elem_type,
    );

    // Body: push elem to result
    let el_local = builder.add_local(elem_type, None);
    builder.assign(Place::local(el_local), FunctionBuilder::copy(elem_local));
    let arr_ref = builder.borrow_mut(Place::local(result), ctx.register_mut_ptr_type(array_type));
    let el_ref = builder.borrow(Place::local(el_local), ctx.register_ptr_type(elem_type));
    builder.call_extern(
        "gorget_array_push",
        vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
        UNIT_TYPE,
    );
    builder.jump(header_bb);

    builder.switch_to(exit_bb);
    Some(FunctionBuilder::copy(result))
}

/// Lower an index access expression.
pub(super) fn lower_index_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);
    let idx = lower_expr(ctx, builder, index);

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        // Infer element type from the base collection type
        let base_type = infer_operand_type_full(ctx, &obj, builder);

        // Check if the type has a get() equip method (Index trait / operator overload)
        // Skip for built-in collection types — use direct index_load instead
        // (Vector.get() returns Option[T] but v[i] returns T directly)
        if let Some(type_name) = infer_type_name_from_operand_full(ctx, &obj, builder) {
            let is_builtin_collection = type_name.starts_with("Vector__")
                || type_name == "GorgetArray"
                || type_name.starts_with("Dict__")
                || type_name.starts_with("HashMap__")
                || type_name.starts_with("Set__");
            if !is_builtin_collection {
                // Try Type__get (from equip Type with Index) or __getitem__
                let candidates = [
                    format!("{type_name}__get"),
                    format!("Index_for_{type_name}__get"),
                    format!("{type_name}____getitem__"),
                ];
                for get_name in &candidates {
                    if ctx.fn_sigs.contains_key(get_name.as_str()) {
                        let ret_type = ctx.fn_sigs.get(get_name.as_str())
                            .map(|(_, ret)| *ret)
                            .unwrap_or(I64_TYPE);
                        // Only dispatch if the first param is a pointer to our type (it's a method)
                        let is_method = ctx.fn_sigs.get(get_name.as_str())
                            .map(|(params, _)| {
                                params.first().map(|&p| {
                                    matches!(ctx.type_registry.get(p), Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)))
                                }).unwrap_or(false)
                            })
                            .unwrap_or(false);
                        if !is_method { continue; }
                        let pt = ctx.register_ptr_type(base_type);
                        let pl = builder.add_local(pt, None);
                        builder.emit_borrow(pl, place.clone());
                        let dst = builder.call(get_name.clone(), vec![FunctionBuilder::copy(pl), idx], ret_type);
                        return FunctionBuilder::copy(dst);
                    }
                }
            }
        }

        let elem_type = if base_type == ctx.type_mapper.string_view_type
            || base_type == ctx.type_mapper.owned_string_type
        {
            ctx.type_mapper.string_view_type // indexing a string returns Str
        } else {
            // Try to infer element type from collection type name
            infer_collection_element_type(ctx, base_type)
        };
        // Resource-type elements: return Ptr(T) reference for direct collections
        // (zero-cost borrow — LIR reads through pointer without clone).
        // For Recursive/Custom-drop types (Yaml, user structs), the LIR clones
        // unconditionally, so return owned type and register for drop.
        // Exception: Task elements are consumed on await, not borrowed.
        let is_task = matches!(ctx.type_registry.get(elem_type),
            Some(GirType::Named(n)) if n.starts_with("Task__"));
        let lir_will_clone = if let Some(GirType::Named(n)) = ctx.type_registry.get(elem_type) {
            // LIR clones Recursive-drop types that aren't direct collections
            !ctx.type_registry.is_collection_type(elem_type) && {
                ctx.type_registry.get_type_def(n)
                    .map(|td| matches!(td.metadata.drop_strategy,
                        crate::ir::types::DropStrategy::Recursive | crate::ir::types::DropStrategy::Custom(_)))
                    .unwrap_or(false)
            }
        } else { false };
        let result_type = if is_task {
            elem_type
        } else if lir_will_clone {
            // LIR will clone → result is owned, needs drop tracking
            elem_type
        } else if ctx.type_registry.is_resource_type(elem_type) {
            // Direct collection → Ptr (zero-cost borrow)
            ctx.type_registry.insert(GirType::Ptr(elem_type))
        } else {
            elem_type
        };
        let dst = builder.index_load(place.clone(), idx, result_type);
        // Register owned clones for drop
        if lir_will_clone {
            ctx.drops.register_local(dst, result_type, &ctx.type_registry);
        }
        return FunctionBuilder::copy(dst);
    }

    Operand::Constant(Constant::Unit)
}

/// Infer the element type of a collection from its TypeId.
/// Returns the element TypeId, or I64_TYPE if unknown.
pub(in crate::ir::lowering) fn infer_collection_element_type(ctx: &mut LoweringContext, collection_type: TypeId) -> TypeId {
    let collection_type = ctx.pointee_type(collection_type).unwrap_or(collection_type);
    if let Some(GirType::Named(name)) = ctx.type_registry.get(collection_type).cloned() {
        // Vector__T → look up T as a type
        if let Some(elem_name) = name.strip_prefix("Vector__") {
            let elem_name = elem_name.to_string();
            return resolve_type_name_to_id(ctx, &elem_name);
        }
        // Dict__K__V → V is the value type (for indexing)
        if let Some(rest) = name.strip_prefix("Dict__").or_else(|| name.strip_prefix("Map__")) {
            if let Some(pos) = rest.find("__") {
                let val_name = &rest[pos + 2..];
                // Callable value types → FnPtr TypeId so the local is declared as GorgetClosure
                if val_name.starts_with("Callable__") || val_name.starts_with("MutCallable__") || val_name.starts_with("ConsumeCallable__") {
                    return ctx.type_registry.insert(GirType::FnPtr { params: vec![], return_type: I64_TYPE });
                }
                let val_name = val_name.to_string();
                return resolve_type_name_to_id(ctx, &val_name);
            }
        }
    }
    I64_TYPE
}

/// Resolve a mangled C type name back to a TypeId.

fn resolve_type_name_to_id(ctx: &LoweringContext, name: &str) -> TypeId {
    match name {
        "int64_t" => I64_TYPE,
        "int32_t" => I32_TYPE,
        "int16_t" => I16_TYPE,
        "int8_t" => I8_TYPE,
        "uint64_t" => U64_TYPE,
        "uint32_t" => U32_TYPE,
        "uint16_t" => U16_TYPE,
        "uint8_t" => U8_TYPE,
        "double" => F64_TYPE,
        "float" => F32_TYPE,
        "bool" => BOOL_TYPE,
        "GorgetStringView" => ctx.type_mapper.string_view_type,
        "GorgetString" => ctx.type_mapper.owned_string_type,
        _ => {
            // Try named type lookup
            if let Some(tid) = ctx.type_mapper.lookup_named(name) {
                return tid;
            }
            // Insert as a new Named type
            I64_TYPE
        }
    }
}

/// Lower an if expression (used as ternary).

pub(super) fn infer_type_name_from_operand_full(
    ctx: &LoweringContext,
    operand: &Operand,
    builder: &FunctionBuilder,
) -> Option<String> {
    let type_id = match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // First check ctx locals (named variables)
            let mut tid = None;
            for (_, (lid, local_tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    tid = Some(*local_tid);
                    break;
                }
            }
            // Fall back to builder locals (temporaries)
            if tid.is_none() {
                let idx = place.local.0 as usize;
                if idx < builder.locals.len() {
                    tid = Some(builder.locals[idx].type_id);
                }
            }
            tid?
        }
        Operand::Constant(c) => match c {
            Constant::Str(_) => return Some("GorgetStringView".to_string()),
            Constant::Bool(_) => return Some("bool".to_string()),
            Constant::I64(_) => return Some("int64_t".to_string()),
            Constant::F64(_) => return Some("double".to_string()),
            Constant::GlobalRef(name) => return ctx.global_type_names.get(name).cloned(),
            _ => return None,
        },
    };

    // Resolve through pointer types
    let effective_tid = ctx.pointee_type(type_id).unwrap_or(type_id);

    // Check primitive types
    if effective_tid == ctx.type_mapper.string_view_type {
        return Some("GorgetStringView".to_string());
    }
    if effective_tid == ctx.type_mapper.owned_string_type {
        return Some("GorgetString".to_string());
    }
    if effective_tid == U8_TYPE {
        return Some("uint8_t".to_string());
    }
    // Check named types (match both the original type_id and the dereferenced effective_tid,
    // since opaque pointer types like PoolAllocator are registered as Ptr(Named(...)))
    ctx.type_mapper.named_types.iter()
        .find_map(|(name, &id)| if id == effective_tid || id == type_id { Some(name.clone()) } else { None })
}

/// Resolve the inner TypeId from a type name (e.g., "int64_t" → I64_TYPE).

fn resolve_inner_type(ctx: &mut LoweringContext, inner_name: &str) -> TypeId {
    match inner_name {
        "int64_t" => I64_TYPE,
        "int32_t" => I32_TYPE,
        "double" => F64_TYPE,
        "float" => F32_TYPE,
        "bool" => BOOL_TYPE,
        "uint8_t" => U8_TYPE,
        "uint16_t" => U16_TYPE,
        "uint32_t" => U32_TYPE,
        "uint64_t" => U64_TYPE,
        "int8_t" => I8_TYPE,
        "int16_t" => I16_TYPE,
        "GorgetStringView" => ctx.type_mapper.string_view_type,
        "GorgetString" => ctx.type_mapper.owned_string_type,
        name => {
            if let Some(id) = ctx.type_mapper.lookup_named(name) {
                return id;
            }
            // Collection/compound types might not be registered yet —
            // register them on-the-fly as Named types so the C backend can emit the right typedef.
            if name.starts_with("Vector__") || name.starts_with("Dict__")
                || name.starts_with("HashMap__") || name.starts_with("Set__")
                || name.starts_with("HashSet__")
                || name.starts_with("Task__") || name.starts_with("Tuple__")
                || name.starts_with("Channel__")
            {
                let type_id = ctx.type_registry.insert(GirType::Named(name.to_string()));
                ctx.type_mapper.register_named(name.to_string(), type_id);
                return type_id;
            }
            I64_TYPE
        }
    }
}

/// Extract the element TypeId from a collection type name like "Vector__Str", "Set__int64_t".
/// Returns None if the type name doesn't match a known collection pattern.
fn extract_elem_type_id_from_type_name(ctx: &LoweringContext, type_name: &str) -> Option<TypeId> {
    let elem_str = if let Some(rest) = type_name.strip_prefix("Vector__") {
        Some(rest)
    } else if let Some(rest) = type_name.strip_prefix("Set__") {
        Some(rest)
    } else if let Some(rest) = type_name.strip_prefix("HashSet__") {
        Some(rest)
    } else {
        // For Dict/HashMap, element type depends on the method context.
        // Dict filter closure takes (key, value), which is more complex.
        // Skip for now — Dict closures with explicit types work fine.
        None
    };
    elem_str.and_then(|elem| {
        // Check primitive types first
        match elem {
            "int64_t" => Some(I64_TYPE),
            "double" => Some(F64_TYPE),
            "bool" => Some(BOOL_TYPE),
            "GorgetStringView" => Some(ctx.type_mapper.string_view_type),
            _ => ctx.lookup_type_by_name(elem)
                .or_else(|| ctx.type_mapper.lookup_named(elem)),
        }
    })
}

