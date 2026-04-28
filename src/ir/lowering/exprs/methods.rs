//! Method call lowering, collection method dispatch, and iterator adapter lowering.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr, Ownership};
use crate::span::Spanned;

use super::super::context::{LoweringContext, CollectionId, ParamABI};
use super::{lower_expr, lower_call_arg, infer_operand_type_full, register_tuple_type,
            is_resource_type_local, get_or_register_type,
            ensure_box_type_def, ensure_guard_type_def, ensure_shared_type_def, ensure_weak_type_def,
            index_expr_to_mangle_fragment, try_resolve_field_place, extract_field_path_string};

fn gorget_name_for_type_id(ctx: &LoweringContext, type_id: TypeId) -> String {
    if type_id == ctx.type_mapper.owned_string_type {
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
    method_generic_args: Option<&[Spanned<ast::Type>]>,
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

                // Box.new is a consuming position (like Vector.push / enum variant init).
                // The pre-existing behavior MoveZero's the source's named-local slot
                // unconditionally, which is correct for bare params (the load_ref
                // already produced an owned temp that alloc_fn will consume) but
                // UNSOUND for owned named locals whose identifier is used downstream —
                // the borrow checker never sees the consumption, lowering MoveZeros,
                // and the GIR validator then either catches a use-after-MoveZero
                // (compiler panic on valid source) or, if the validator is weakened,
                // the binary reads zeroed heap-backed collections at runtime.
                //
                // Rule: if the source is an owned named local AND this use is not
                // its last-use (i.e. the identifier is read downstream), insert a
                // clone of the *derefed* value before boxing. Leave the bare-param
                // path alone.
                let needs_clone_guard = if let Expr::Identifier(arg_name) = &args[0].node.value.node {
                    if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                        let is_owned_named_resource = ctx.is_named_local(local_id)
                            && is_resource_type_local(local_id, builder, &ctx.type_registry)
                            && !ctx.is_bare_param(local_id);
                        let not_last_use = !ctx.is_last_use_at(arg_name, args[0].node.value.span);
                        is_owned_named_resource && not_last_use
                    } else {
                        false
                    }
                } else {
                    false
                };
                let clone_inserted = if needs_clone_guard {
                    if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner_type) {
                        ctx.warn_implicit_clone(
                            args[0].node.value.span,
                            inner_type,
                            crate::ir::ImplicitCloneReason::ConsumingArg,
                        );
                        let clone_src = if let Operand::Copy(ref place) = val {
                            let ptr_type = ctx.register_ptr_type(inner_type);
                            let ptr = builder.add_local(ptr_type, None);
                            builder.emit_borrow(ptr, place.clone());
                            FunctionBuilder::copy(ptr)
                        } else {
                            val.clone()
                        };
                        let cloned = builder.call(&clone_fn, vec![clone_src], inner_type);
                        ctx.drops.register_local(cloned, inner_type, &ctx.type_registry);
                        val = FunctionBuilder::copy(cloned);
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

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
                // MoveZero the source slot only when no clone was inserted — otherwise
                // the identifier is still live and MoveZero'ing it would UAF the
                // downstream read that triggered the clone.
                if !clone_inserted {
                    if let Expr::Identifier(arg_name) = &args[0].node.value.node {
                        if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                            if ctx.is_named_local(local_id)
                                && is_resource_type_local(local_id, builder, &ctx.type_registry)
                            {
                                ctx.move_zero_and_mark(builder, local_id);
                            }
                        }
                    }
                }
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
                    "str" | "String" => "GorgetString",
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
                            let mut lowered_args = lowered_args;
                            let ast_args: Vec<_> = args.iter().map(|a| a.node.value.clone()).collect();
                            super::clone_multi_use_resource_args(ctx, builder, &mut lowered_args, &ast_args);
                            let type_id = ctx.type_mapper.lookup_named(name).unwrap_or(UNIT_TYPE);
                            let dst = ctx.emit_enum_init_owned(builder, name, method_name, type_id, lowered_args);
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

    // Extract field path string for CowBorrow provenance on field-access receivers.
    let field_path_for_cow: Option<String> = extract_field_path_string(&receiver.node);

    // For pointer params used as method receivers, pass the raw pointer directly.
    // Auto-deref would copy the struct, and mutations to the copy wouldn't propagate back.
    let borrow_param_local = if let Expr::Identifier(name) = &receiver.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.is_ref_local(local_id)
                || ctx.func_state.mut_capture_locals.contains_key(&local_id)
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

    let mut recv = if borrow_param_local.is_some() {
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
                Some(builder.local_type(place.local))
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
                ctx.move_zero_and_mark(builder, local_id);
            }
            return result;
        }
        return recv; // fallback pass-through (no known spawn source)
    }

    // Primitive .hash() → runtime hash functions
    // .mod(divisor) → Euclidean modulo (BinOp::Mod)
    if method_name == "mod" && !args.is_empty() {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        let divisor = lower_expr(ctx, builder, &args[0].node.value);
        let dst = builder.bin_op(crate::ir::instructions::BinOp::Mod, recv_type, recv, divisor);
        return FunctionBuilder::copy(dst);
    }

    // Hashable.hash(self, FxHasher &h) — state-based hashing.
    //
    // Primitives don't carry a user-written `equip T with Hashable`
    // block (the C backend chokes on it, see history), so the IR
    // lowers `x.hash(&h)` on scalar/bool/String/byte receivers
    // directly to the matching `FxHasher.write_*` method. The one-arg
    // form is the trait method; the no-arg legacy form is gone —
    // callers use `hash_of(x)` (in `std.hash`) for the one-shot.
    if method_name == "hash" && args.len() == 1 {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        // Unwrap Ptr(T) to T — inside a monomorphized generic body the
        // receiver arrives as Ptr(GorgetString) / Ptr(int64_t) because
        // the formerly-generic T param is passed by pointer ABI. The
        // primitive-hash path still applies to the pointee type.
        let recv_type_unwrapped = match ctx.type_registry.get(recv_type) {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
            _ => recv_type,
        };
        let is_int_like = recv_type_unwrapped == I64_TYPE || recv_type_unwrapped == I32_TYPE
            || recv_type_unwrapped == I16_TYPE || recv_type_unwrapped == I8_TYPE
            || recv_type_unwrapped == U64_TYPE || recv_type_unwrapped == U32_TYPE
            || recv_type_unwrapped == U16_TYPE || recv_type_unwrapped == U8_TYPE;
        let is_string = ctx.type_mapper.is_string_type(recv_type_unwrapped);
        if is_int_like || recv_type_unwrapped == BOOL_TYPE || is_string {
            // The hasher arg's type drives which `Hasher` impl's `write_int` /
            // `write_string` to call. Static dispatch — H is concrete at mono
            // time. Try the inherent form `H__write_int` first, then fall
            // back to the trait forwarder `Hasher_for_H__write_int` for
            // user-defined Hashers that only provide the `equip H with
            // Hasher:` block (no inherent equip).
            let h = lower_call_arg(ctx, builder, &args[0], None, "FxHasher__write_int", 0);
            let hasher_type_name = infer_type_name_from_operand_full(ctx, &h, builder)
                .unwrap_or_else(|| "FxHasher".to_string());
            let resolve_fn = |op: &str| -> String {
                let inherent = format!("{hasher_type_name}__{op}");
                if ctx.fn_sigs.contains_key(&inherent) {
                    inherent
                } else {
                    let suffix = format!("_for_{hasher_type_name}__{op}");
                    ctx.fn_sigs.keys()
                        .find(|k| k.ends_with(&suffix))
                        .cloned()
                        .unwrap_or(inherent)
                }
            };
            let write_int_fn = resolve_fn("write_int");
            let write_string_fn = resolve_fn("write_string");
            // If the receiver arrived as Ptr(T) (generic-param pointer ABI),
            // dereference it before passing to the Hasher method.
            let recv_was_ptr = matches!(
                ctx.type_registry.get(recv_type),
                Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
            );
            let deref_recv = if recv_was_ptr {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let loaded = builder.load_ref(place.clone(), recv_type_unwrapped);
                    FunctionBuilder::copy(loaded)
                } else {
                    recv
                }
            } else {
                recv
            };
            if is_string {
                builder.call_void(&write_string_fn, vec![h, deref_recv]);
            } else if recv_type_unwrapped == BOOL_TYPE {
                let cast = builder.cast(I64_TYPE, deref_recv);
                builder.call_void(&write_int_fn, vec![h, FunctionBuilder::copy(cast)]);
            } else if recv_type_unwrapped == I64_TYPE {
                builder.call_void(&write_int_fn, vec![h, deref_recv]);
            } else {
                let cast = builder.cast(I64_TYPE, deref_recv);
                builder.call_void(&write_int_fn, vec![h, FunctionBuilder::copy(cast)]);
            }
            return Operand::Constant(Constant::Unit);
        }
        // Fall through to normal dispatch for user-defined types.
    }

    // Primitive .clone() is a no-op — POD types copy by assignment, so cloning
    // just returns the value. Generic functions bounded by `T: Cloneable` can
    // therefore call `.clone()` on primitive arguments without hitting a missing
    // method. Resource-backed types (String, Vector) fall through to the normal
    // dispatch path which invokes the runtime clone helpers.
    if method_name == "clone" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        let is_pod = recv_type == I64_TYPE || recv_type == I32_TYPE || recv_type == I16_TYPE || recv_type == I8_TYPE
            || recv_type == U64_TYPE || recv_type == U32_TYPE || recv_type == U16_TYPE || recv_type == U8_TYPE
            || recv_type == F64_TYPE || recv_type == F32_TYPE
            || recv_type == BOOL_TYPE;
        if is_pod {
            return recv;
        }
    }

    // Primitive .debug() / .display() → runtime stringification.
    // debug() on String quotes and escapes; display() on String is identity.
    // For numeric/bool types, both are identical (reuse gorget_{int,float,bool}_to_str).
    if method_name == "debug" || method_name == "display" {
        let raw_type = infer_operand_type_full(ctx, &recv, builder);
        let (recv_type, maybe_deref_recv): (TypeId, Option<Operand>) =
            if let Some(inner) = ctx.pointee_type(raw_type) {
                if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let derefed = builder.load_ref(place.clone(), inner);
                    (inner, Some(FunctionBuilder::copy(derefed)))
                } else {
                    (raw_type, None)
                }
            } else {
                (raw_type, None)
            };
        let owned_string_type = ctx.type_mapper.owned_string_type;
        let use_recv = || maybe_deref_recv.clone().unwrap_or_else(|| recv.clone());
        if recv_type == I64_TYPE {
            let dst = builder.call_extern("gorget_int_to_str", vec![use_recv()], owned_string_type);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == I8_TYPE || recv_type == I16_TYPE || recv_type == I32_TYPE {
            let widened = builder.cast(I64_TYPE, use_recv());
            let dst = builder.call_extern("gorget_int_to_str", vec![FunctionBuilder::copy(widened)], owned_string_type);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == U64_TYPE || recv_type == U8_TYPE || recv_type == U16_TYPE || recv_type == U32_TYPE {
            let arg = if recv_type == U64_TYPE {
                use_recv()
            } else {
                let w = builder.cast(I64_TYPE, use_recv());
                FunctionBuilder::copy(w)
            };
            let dst = builder.call_extern("gorget_int_to_str", vec![arg], owned_string_type);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == F64_TYPE {
            let dst = builder.call_extern("gorget_float_to_str", vec![use_recv()], owned_string_type);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == F32_TYPE {
            let widened = builder.cast(F64_TYPE, use_recv());
            let dst = builder.call_extern("gorget_float_to_str", vec![FunctionBuilder::copy(widened)], owned_string_type);
            return FunctionBuilder::copy(dst);
        }
        if recv_type == BOOL_TYPE {
            let dst = builder.call_extern("gorget_bool_to_str", vec![use_recv()], owned_string_type);
            return FunctionBuilder::copy(dst);
        }
        if ctx.type_mapper.is_string_type(recv_type) {
            if method_name == "debug" {
                let dst = builder.call_extern("gorget_string_debug", vec![use_recv()], owned_string_type);
                return FunctionBuilder::copy(dst);
            } else {
                return use_recv();
            }
        }
        // Named types fall through to the normal dispatch path (user-defined
        // display/debug via equip blocks).
    }

    // .unwrap() / .expect() / .unwrap_or() on Option/Result → inline extraction
    // On non-Option/Result types → pass-through (unwrap is a no-op)
    if matches!(method_name, "unwrap" | "expect" | "unwrap_or") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_option_or_result = type_name.as_ref()
            .map(|n| ctx.type_registry.is_option_or_result(n)
                || n.starts_with("Option") || n.starts_with("Result"))
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
                // Option__Ref__T → Ptr(T) (borrowed reference from collection)
                if let Some(pointee_name) = inner_name.strip_prefix("Ref__") {
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
                    // Move-if-dead: unwrap consumes the Option/Result.
                    // Unregister + MoveZero to transfer ownership.
                    if is_resource_type_local(dst, builder, &ctx.type_registry) {
                        ctx.drops.unregister(place.local);
                        ctx.move_zero_and_mark(builder, place.local);
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
                    // Move-if-dead: unwrap consumes the Option/Result.
                    // Unregister + MoveZero to transfer ownership.
                    if is_resource_type_local(dst, builder, &ctx.type_registry) {
                        ctx.drops.unregister(place.local);
                        ctx.move_zero_and_mark(builder, place.local);
                    }
                    // Ptr(T) from Option__Ref_ (collection .get().unwrap()):
                    // Mark as CowBorrow so typed bindings defer the clone to
                    // ownership boundaries instead of cloning at VarDecl.
                    // Uses insert to override Owned from call_extern_tracked.
                    // Propagate collection provenance from the Option local.
                    if matches!(ctx.type_registry.get(inner_type), Some(GirType::Ptr(_))) {
                        ctx.set_cow_borrow(dst);
                        if let Some(collection) = ctx.cow_borrow_source(place.local).cloned() {
                            ctx.set_cow_borrow_source(dst, collection);
                        }
                    }
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // .unwrap_error() / .unwrap_err() on Result → extract Error payload with MoveZero
    if matches!(method_name, "unwrap_error" | "unwrap_err") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_result = type_name.as_ref()
            .map(|n| n.starts_with("Result__"))
            .unwrap_or(false);
        if is_result {
            if let Some(ref tn) = type_name {
                // Result__Ok__Err → extract Err type (last component after last __)
                let rest = &tn["Result__".len()..];
                let err_name = ["__Str", "__int64_t", "__bool", "__double"].iter()
                    .find_map(|suffix| rest.strip_suffix(suffix).map(|_| &suffix[2..]))
                    .unwrap_or_else(|| {
                        rest.rfind("__").map(|pos| &rest[pos + 2..]).unwrap_or(rest)
                    });
                let err_type = resolve_inner_type(ctx, err_name);

                if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                    let ptr_type = ctx.register_ptr_type(
                        infer_operand_type_full(ctx, &recv, builder),
                    );
                    let borrow = builder.add_local(ptr_type, None);
                    builder.emit_borrow(borrow, place.clone());
                    let dst = ctx.call_extern_tracked(builder,
                        "__result_unwrap_error",
                        vec![FunctionBuilder::copy(borrow)],
                        err_type,
                    );
                    // Move-if-dead: unwrap_error consumes the Result.
                    // Unregister from drops. MoveZero only for temps (named
                    // locals may be read again — unregister alone suffices).
                    if is_resource_type_local(dst, builder, &ctx.type_registry) {
                        ctx.drops.unregister(place.local);
                        if !ctx.is_named_local(place.local) {
                            ctx.move_zero_and_mark(builder, place.local);
                        }
                    }
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // Channel[T] — handled by generic dispatch via BuiltinTypeProtocol (MutBorrow self_conv)

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
                        let inner_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);
                        let weak_type = get_or_register_type(ctx, &weak_name, Some(&|c| ensure_weak_type_def(c, &weak_name, inner_type)));
                        let downgrade_fn = format!("{stn}__downgrade");
                        let dst = builder.call(&downgrade_fn, vec![recv], weak_type);
                        return FunctionBuilder::copy(dst);
                    }
                    // Shared[Vector[T]] element access — at/set_at/slen
                    "at" if elem_suffix.starts_with("Vector__") => {
                        let inner_elem = elem_suffix.strip_prefix("Vector__").unwrap_or("int64_t");
                        // Must use correct primitive types — lookup_named misses "double"/"float"
                        let elem_type = match inner_elem {
                            "double" => F64_TYPE,
                            "float"  => F32_TYPE,
                            "bool"   => BOOL_TYPE,
                            "int32_t" | "uint32_t" => I32_TYPE,
                            "int16_t" | "uint16_t" => I16_TYPE,
                            "int8_t"  | "uint8_t"  => I8_TYPE,
                            _ => ctx.type_mapper.lookup_named(inner_elem).unwrap_or(I64_TYPE),
                        };
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
                        // Returns Option[Shared[T]] — need to build the Option type.
                        // Register the Option's enum TypeDef too, not just the Named
                        // TypeId: without the TypeDef, `resolve_variant_tag` returns
                        // None for "Some"/"None" at `match w.upgrade():` sites, which
                        // makes `lower_pattern_condition` fall through to
                        // `const_bool(true)` — every arm matches and only the first
                        // arm body fires. Symptom: `w.upgrade()` cases appeared to
                        // keep the Shared alive (print "alive" / 1) even when strong
                        // count had reached zero.
                        let shared_name = format!("Shared__{elem_suffix}");
                        let inner_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);
                        let shared_type = get_or_register_type(ctx, &shared_name, Some(&|c| ensure_shared_type_def(c, &shared_name, inner_type)));
                        let option_name = format!("Option__{shared_name}");
                        // Register the enum TypeDef FIRST — `ensure_option_type_registered`
                        // short-circuits if `option_name` is already in `named_types`, so
                        // calling it before the first `get_or_register_type` ensures the
                        // TypeDef is actually inserted.
                        ctx.ensure_option_type_registered(&option_name, shared_type);
                        let option_type = get_or_register_type(ctx, &option_name, None);
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
                let inner_type = ctx.type_mapper.lookup_named(elem_suffix).unwrap_or(I64_TYPE);
                let guard_type = get_or_register_type(ctx, &guard_name, Some(&|c| ensure_guard_type_def(c, &guard_name, inner_type)));
                if method_name == "lock" {
                    let lock_fn = format!("{mtn}__lock");
                    let dst = builder.call(&lock_fn, vec![recv], guard_type);
                    return FunctionBuilder::copy(dst);
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

    // ReadGuard[T] / WriteGuard[T] — handled by generic dispatch (MutBorrow self_conv)

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
                "wait_timeout" if !args.is_empty() => {
                    let ms = lower_expr(ctx, builder, &args[0].node.value);
                    let dst = builder.call("Process__wait_timeout", vec![recv, ms], I64_TYPE);
                    return FunctionBuilder::copy(dst);
                }
                "read_all" | "read_all_timeout" => {
                    let str_type = ctx.type_mapper.owned_string_type;
                    let exec_result_tid = ctx.type_mapper.get_or_register(
                        "ExecResult", &mut ctx.type_registry, |n| {
                            use crate::ir::types::*;
                            TypeDef {
                                name: n.to_string(),
                                kind: TypeDefKind::Struct(StructDef {
                                    fields: vec![
                                        StructField { name: "output".into(), type_id: str_type },
                                        StructField { name: "errors".into(), type_id: str_type },
                                        StructField { name: "exit_code".into(), type_id: I64_TYPE },
                                    ],
                                }),
                                metadata: TypeMetadata {
                                    copy_semantics: CopySemantics::Resource,
                                    drop_strategy: DropStrategy::Recursive,
                                    ..Default::default()
                                },
                            }
                        },
                    );
                    if method_name == "read_all" {
                        let dst = builder.call("Process__read_all", vec![recv], exec_result_tid);
                        return FunctionBuilder::copy(dst);
                    } else {
                        let ms = lower_expr(ctx, builder, &args[0].node.value);
                        let dst = builder.call("Process__read_all_timeout", vec![recv, ms], exec_result_tid);
                        return FunctionBuilder::copy(dst);
                    }
                }
                _ => {}
            }
        }
    }

    // Guard[T] — handled by generic dispatch via BuiltinTypeProtocol (MutBorrow self_conv)

    // TaskGroup, AtomicInt, AtomicBool, Barrier, WaitGroup, Semaphore, OnceFlag
    // — handled by generic dispatch via BuiltinTypeProtocol (builtins.rs)

    // .is_some() / .is_none() / .is_ok() / .is_error() on Option/Result → tag check
    // On non-Option/Result types → pass-through (return false)
    if matches!(method_name, "is_some" | "is_none" | "is_ok" | "is_error") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        let is_option_or_result = type_name.as_ref()
            .map(|n| ctx.type_registry.is_option_or_result(n)
                || n.starts_with("Option") || n.starts_with("Result"))
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
        if ctx.type_mapper.is_string_type(recv_type) {
            let dst = builder.call_extern(
                "gorget_str_codepoint_count",
                vec![recv],
                I64_TYPE,
            );
            return FunctionBuilder::copy(dst);
        }
        // GorgetArray: .len is field 2 (element count, no function call needed)
        // Under uniform layout {data, cap, len, elem_size}, .len is at offset +16.
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
                    len_place.projections.push(Projection::Field(2));
                    let tmp = builder.add_local(I64_TYPE, None);
                    builder.assign(Place::local(tmp), Operand::Copy(len_place));
                    return FunctionBuilder::copy(tmp);
                }
            }
        }
    }
    // Handle .byte_len() for strings → direct field access
    // Under 32-byte Str layout {data, cap, len, alloc}, len is at field index 2.
    if method_name == "byte_len" {
        let recv_type = infer_operand_type_full(ctx, &recv, builder);
        if ctx.type_mapper.is_string_type(recv_type) {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                let mut len_place = place.clone();
                len_place.projections.push(Projection::Field(2));
                let tmp = builder.add_local(I64_TYPE, None);
                builder.assign(Place::local(tmp), Operand::Copy(len_place));
                return FunctionBuilder::copy(tmp);
            }
        }
    }

    // Determine the receiver type and mangle the method name
    let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);

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
                                    let local_type = builder.local_type(p.local);
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
                                    let val = lower_expr(ctx, builder, &arg.node.value);
                                    // Auto-deref Ptr(T) → T for non-resource pointees.
                                    // Trait-object methods (e.g. `Box[Serializer].write_int`)
                                    // expect by-value primitives / Copy structs, but the
                                    // caller may pass a Ref[T] from `v.get(i).unwrap()`.
                                    let val = ctx.auto_clone_if_ptr(builder, val, arg.span);
                                    call_args.push(val);
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

        // GIR-level desugaring for Option/Result combinators.
        // Replaces C backend inline functions with explicit tag check + closure call,
        // giving the compiler full ownership visibility.
        // GIR-level desugaring for Option/Result combinators on primitive-payload types.
        // For resource-payload types (String, Vector, etc.), the C inline path handles
        // implicit type coercions (GorgetString ↔ Str) that GIR can't express.
        if (type_name.starts_with("Option__") || type_name.starts_with("Result__"))
            && matches!(method_name, "map" | "and_then" | "or_else" | "filter"
                | "unwrap_or_else" | "flat_map" | "map_err")
        {
            if let Some(result) = try_lower_option_result_combinator(
                ctx, builder, &type_name, method_name, recv.clone(), args,
            ) {
                return result;
            }
        }

        // Iterator adapter expansion: fold/map/filter/collect on Iterator types.
        // A legacy `try_lower_iterator_adapter` shortcut used to
        // intercept here for `.fold()` / `.map()` / `.filter()` /
        // `.collect()` on any Iterator implementor and eagerly emit a
        // `GorgetArray` loop. Retired 2026-04-23 along with its
        // helpers (`build_iter_next_loop` + `lower_iter_fold|map|filter|collect`)
        // and the `has_method_instance` guard that gated its firing —
        // auto-import std.iter + non-generic-impl per-call-site mono
        // now provide proper concrete functions through the regular
        // dispatch below.

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
            let is_string = recv_type_id == ctx.type_mapper.owned_string_type;
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
            // Non-resource type: .clone() is a trivial copy (no deep clone needed).
            // But if recv is a borrow (Ptr/MutPtr) — e.g. `Ref[Callable].clone()`
            // after `Vector[Callable].get(i).unwrap()` — we must dereference to
            // materialize the value into a fresh local of the pointee type.
            // Otherwise downstream codegen sees a Ptr-typed value flowing into a
            // wider value-typed slot (e.g. 16-byte GorgetClosure) and emits a
            // broken double-deref: it reads 8 bytes through the pointer and
            // uses *those bytes* as a memcpy source address — for a closure
            // that means dereffing fn_ptr as memory → SEGV
            // (attack_82_vector_of_closures.gg).
            if let Operand::Copy(p) | Operand::Move(p) = &recv {
                if p.projections.is_empty() {
                    let lid = p.local.0 as usize;
                    if lid < builder.locals.len() {
                        let tid = builder.locals[lid].type_id;
                        if matches!(
                            ctx.type_registry.get(tid),
                            Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
                        ) {
                            let derefed = builder.load_ref(p.clone(), recv_type_id);
                            return FunctionBuilder::copy(derefed);
                        }
                    }
                }
            }
            return recv;
        }

        // Route overloaded methods to distinct runtime functions based on arg count.
        // split(sep) → GorgetString__split (2-arg), split(sep, limit) → GorgetString__splitn (3-arg)
        // replace(old, new) → GorgetString__replace, replace(old, new, limit) → GorgetString__replacen
        // sort()/sorted() → default compare, sort(by)/sorted(by) → closure compare
        let effective_method = match (type_name.as_str(), method_name, args.len()) {
            ("GorgetString", "split", 2) => "splitn",
            ("GorgetString", "replace", 3) => "replacen",
            ("GorgetString", "find", 2) => "find_from",
            ("GorgetString", "find", 3) => "find_ext",
            (tn, "sort", 1) if tn.starts_with("Vector__") || tn.starts_with("Deque__") => "sort_by",
            (tn, "sorted", 1) if tn.starts_with("Vector__") || tn.starts_with("Deque__") => "sorted_by",
            _ => method_name,
        };
        // Per-call-site method instance for method-level-generic equip methods:
        // `v.iter().map[int, int(int)](f)` targets a dedicated mangled symbol
        // whose body was produced by `lower_method_instance`. The fully-qualified
        // mangled symbol appends each method-level type arg's mangled form.
        //
        // When the call site lives inside a generic body whose own type params
        // are substituted (e.g. `apply_hash[Hashable T, Hasher H]` → mono'd with
        // `T=Point, H=MyHasher`), the targ AST nodes still spell out "H"; we
        // substitute through `generic_param_ast_types` before mangling so the
        // dispatch finds `Point__hash__MyHasher` rather than `Point__hash__H`.
        let mangled = if let Some(targs) = method_generic_args {
            if !targs.is_empty() {
                let mut sym = format!("{type_name}__{effective_method}");
                for t in targs {
                    let resolved = if let crate::parser::ast::Type::Named { name, generic_args } = &t.node {
                        if generic_args.is_empty() {
                            ctx.generics.generic_param_ast_types.get(&name.node).cloned()
                                .unwrap_or_else(|| t.node.clone())
                        } else {
                            t.node.clone()
                        }
                    } else {
                        t.node.clone()
                    };
                    sym.push_str("__");
                    sym.push_str(&crate::ir::lowering::types::mangle_type_for_name(&resolved));
                }
                if ctx.fn_sigs.contains_key(&sym) {
                    sym
                } else {
                    format!("{type_name}__{effective_method}")
                }
            } else {
                format!("{type_name}__{effective_method}")
            }
        } else {
            format!("{type_name}__{effective_method}")
        };

        // Save receiver local for !self post-call MoveZero (before recv is consumed)
        let recv_local_for_move_zero = if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            if place.projections.is_empty() { Some(place.local) } else { None }
        } else { None };

        // Build args: &receiver + explicit args
        let mut call_args = Vec::new();

        // Create a borrow of the receiver for the self parameter.
        // Mutating methods need a mutable borrow (&self → MutPtr).
        let is_mutating =
            crate::ir::lowering::builtins::is_mutating_builtin_method(method_name);

        // Determine if we need a mutable borrow (from explicit list, protocol, or fn_sigs)
        let needs_mut = is_mutating
            || crate::ir::lowering::builtins::is_mut_borrow_method(&type_name, method_name)
            || ctx.fn_sigs.get(&mangled)
            .and_then(|(params, _)| params.first())
            .map(|&p| matches!(ctx.type_registry.get(p), Some(GirType::MutPtr(_))))
            .unwrap_or(false);

        // CoW: if receiver is being mutated, sever any alias relationships first.
        // This may materialize a Ptr param → new owned local (Phase 1c),
        // so re-resolve the receiver afterwards.
        if needs_mut {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                if place.projections.is_empty() {
                    ctx.cow_before_mutation(builder, place.local, receiver.span);
                    // Re-resolve: cow_before_mutation may have redirected the variable
                    // name to a new owned local (Phase 1c param materialization).
                    if let Some(hint) = builder.local_name(place.local).map(|s| s.to_string()) {
                        if let Some((new_local, _)) = ctx.lookup_local(&hint) {
                            if new_local != place.local {
                                recv = FunctionBuilder::copy(new_local);
                            }
                        }
                    }
                }
            }
        }

        // CoW: if the receiver came from an IndexLoad on a collection (tracked via
        // CollectionRef or cow_borrow_source), also sever the SOURCE collection's
        // aliases. This handles `d2["key"].push(x)` where d2 is a CoW alias — the
        // push mutates through a pointer into d2's shared storage, so d2 must be
        // severed before the mutation.
        if needs_mut {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                if place.projections.is_empty() {
                    // Check CollectionRef (from index_load) and CowBorrow (from .get().unwrap())
                    let source = match ctx.func_state.local_ownership.get(&place.local) {
                        Some(crate::ir::lowering::context::LocalOwnershipState::CollectionRef { collection: CollectionId::Local(src) }) => Some(*src),
                        _ => ctx.cow_borrow_source(place.local)
                            .and_then(|c| if let CollectionId::Local(src) = c { Some(*src) } else { None }),
                    };
                    if let Some(source_local) = source {
                        ctx.cow_before_mutation(builder, source_local, receiver.span);
                        // Re-resolve: the source collection may have been replaced
                        // by cow_before_mutation (alias severed → new owned local).
                        // The receiver must be re-lowered to point into the new copy.
                        if let Some(hint) = builder.local_name(source_local).map(|s| s.to_string()) {
                            if let Some((new_local, _)) = ctx.lookup_local(&hint) {
                                if new_local != source_local {
                                    recv = lower_expr(ctx, builder, receiver);
                                }
                            }
                        }
                    }
                }
            }
        }

        // CoW: field-access receiver mutation — materialize any collection refs
        // that borrow from this field path (e.g., self.data.push(x) severs refs
        // created by self.data.get(i).unwrap()).
        if needs_mut {
            if let Some(ref field_path) = field_path_for_cow {
                ctx.cow_before_field_mutation(builder, field_path, receiver.span);
            }
        }

        // If receiver is a field access, borrow the field in-place instead of
        // borrowing a copy (which would mutate the copy, not the original).
        // Exception: if the field's type is already `Ptr(T)` / `MutPtr(T)` —
        // user-written `Ref[T]` / `MutRef[T]` borrow field — its STORED VALUE
        // is already the receiver pointer; borrowing the field place would
        // produce `**T`, which the method's `*T self` ABI rejects. Fall
        // through to the `recv` (Copy/Move) path which already handles the
        // existing `is_ptr` check correctly.
        let field_is_borrow_ptr = field_place_info.as_ref()
            .map(|(_, fty)| matches!(
                ctx.type_registry.get(*fty),
                Some(GirType::Ptr(_) | GirType::MutPtr(_))
            ))
            .unwrap_or(false);
        if let Some((field_place, field_type_id)) = field_place_info.clone()
            .filter(|_| !field_is_borrow_ptr)
        {
            if needs_mut {
                let pt = ctx.register_mut_ptr_type(field_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow_mut(pl, field_place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            } else {
                let pt = ctx.register_ptr_type(field_type_id);
                let pl = builder.add_local(pt, None);
                builder.emit_borrow(pl, field_place.clone());
                call_args.push(FunctionBuilder::copy(pl));
            }
        } else if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            let recv_type_id = if (place.local.0 as usize) < builder.locals.len() {
                builder.local_type(place.local)
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
            } else if crate::ir::lowering::builtins::is_by_value_receiver(&type_name) {
                // Copy-semantics pointer handles (AtomicInt, Barrier, Semaphore, etc.)
                // — pass by value, not by reference.
                call_args.push(recv.clone());
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
            let owned_string_type = ctx.type_mapper.owned_string_type;
            let tmp_local = builder.add_local(owned_string_type, None);
            builder.assign(Place::local(tmp_local), recv);
            let pt = ctx.register_ptr_type(owned_string_type);
            let pl = builder.add_local(pt, None);
            builder.emit_borrow(pl, Place::local(tmp_local));
            call_args.push(FunctionBuilder::copy(pl));
        } else if let Operand::Constant(Constant::GlobalRef(gname)) = &recv {
            // Static-global receiver. The default `Operand::Constant(GlobalRef)`
            // emission loads the global's value, so a struct-typed `&self`
            // method would get the value by copy and the C-side pointer arg
            // would be wrong. Convert to `GlobalRefPtr(name)` (emitted as
            // `&global_name`) for types that the callee receives by pointer.
            //
            // Primitives and by-value handle types (AtomicInt / Barrier /
            // Semaphore / …) keep the `GlobalRef` form — those callees
            // expect the value directly.
            let is_by_value = ctx.global_type_names.get(gname).cloned()
                .map(|t| {
                    crate::ir::lowering::builtins::is_by_value_receiver(&t)
                        || matches!(t.as_str(),
                            "int" | "i8" | "i16" | "i32" | "i64"
                            | "uint" | "u8" | "u16" | "u32" | "u64"
                            | "float" | "f32" | "f64"
                            | "bool" | "char" | "str")
                })
                .unwrap_or(false);
            if is_by_value {
                call_args.push(recv);
            } else {
                call_args.push(Operand::Constant(Constant::GlobalRefPtr(gname.clone())));
            }
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

        // Detect !self (consuming self) methods for post-call receiver MoveZero
        let has_consuming_self = ctx.fn_param_ownerships.get(effective_name.as_str())
            .and_then(|ownerships| ownerships.first())
            .map(|o| matches!(o, crate::parser::ast::Ownership::Move))
            .unwrap_or(false);

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
        let prev_hints = std::mem::take(&mut ctx.func_state.closure_param_type_hints);
        if matches!(method_name, "filter" | "map" | "flat_map" | "any" | "all" | "each" | "for_each" | "find" | "count" | "reduce" | "enumerate") {
            if let Some(elem_type_id) = extract_elem_type_id_from_type_name(ctx, &type_name) {
                ctx.func_state.closure_param_type_hints = vec![elem_type_id];
            }
        } else if method_name == "fold" {
            // fold closure has (accumulator, element) — use element type for both params
            // as a reasonable default. Explicitly-typed params override the hint.
            if let Some(elem_type_id) = extract_elem_type_id_from_type_name(ctx, &type_name) {
                ctx.func_state.closure_param_type_hints = vec![elem_type_id, elem_type_id];
            }
        }

        // For and_then/or_else, the closure should return the same Option/Result type
        // as the receiver. Set expected_type so Ok()/Error()/Some()/None() constructors
        // inside the closure body get the correct type.
        let prev_expected = ctx.func_state.expected_type;
        if matches!(method_name, "and_then" | "or_else") {
            if let Some(type_id) = ctx.lookup_type_by_name(&type_name) {
                ctx.func_state.expected_type = Some(type_id);
            }
        }

        // Save pending_move_zeros baseline so we only drain entries added
        // by THIS method call's argument lowering (not from nested/prior calls).
        let move_zero_baseline = ctx.func_state.pending_move_zeros.len();

        let mut lowered_method_args: Vec<Operand> = args.iter()
            .enumerate()
            .map(|(i, arg)| {
                let callee_pt = method_param_types.get(i).copied();
                // Method args: i is 0-based for non-self args, but fn_param_ownerships
                // includes self at index 0, so offset by 1.
                lower_call_arg(ctx, builder, arg, callee_pt, &effective_name, i + 1)
            })
            .collect();
        // Positions that semantically consume (take ownership of) their arg.
        // `GorgetString.push/push_line/push_char` are StringBuilder appends — they
        // READ the arg and copy its bytes, they do NOT take ownership. Collection
        // mutating methods (push/add/extend/send/push_back/push_front) consume
        // arg 0; (put/set/insert) consume the value at arg 1 (dict) or arg 1 (vec).
        let is_string_builder_method = type_name == "GorgetString";
        let consuming_positions: Vec<usize> = match method_name {
            "push" | "add" | "extend" | "send" | "push_back" | "push_front"
                if !is_string_builder_method => vec![0],
            "put" | "set" | "insert" => {
                let mut p = vec![];
                if lowered_method_args.len() >= 1 { p.push(0); }
                if lowered_method_args.len() >= 2 { p.push(1); }
                p
            }
            _ => vec![],
        };

        // Pre-call ownership materialization at consuming arg positions.
        // Two cases handled here:
        //   (a) Ptr(inner) args — always clone to materialize the borrow.
        //   (b) By-value resource args that are NON-last-use named locals —
        //       clone so the source retains its value. Last-use + temps are
        //       handled via post-call MoveZero (built below into consuming_arg_move_zeros).
        //
        // Why a single section: the thin-pointer String design makes String args
        // pass by-value (ParamABI::ByValue on collection methods). Without a
        // pre-call clone, a non-last-use `s` in `v.push(s); use(s)` would alias
        // the collection element — and reverting materialize_inplace's safety-net
        // clone requires the compiler to guarantee independence at the push site.
        //
        // Cloned temps are tracked in `pre_call_clone_temps` so the scope-exit
        // drop pass doesn't double-free: we MoveZero each clone right after the
        // call (same idiom as `consuming_clone_temps` below). Delegates the
        // clone-vs-move decision to `ensure_owned_at_consuming_arg` — the same
        // helper `lower_index_assign` uses for `Dict[k]=v` / `Vec[i]=v`.
        let mut pre_call_clone_temps: Vec<LocalId> = Vec::new();
        {
            for &idx in &consuming_positions {
                let Some(ast_arg) = args.get(idx) else { continue; };
                // Explicit `!` → caller wants a move; handled by post-call move_zero.
                if matches!(ast_arg.node.ownership, Ownership::Move) { continue; }
                let orig = lowered_method_args[idx].clone();
                let new_op = ctx.ensure_owned_at_consuming_arg(
                    builder,
                    orig.clone(),
                    &ast_arg.node.value,
                    crate::ir::ImplicitCloneReason::ConsumingArg,
                );
                // Detect whether a clone was emitted (distinct local) — if so, it's
                // a fresh owned temp whose data was just consumed by the call, so we
                // track it for post-call MoveZero.
                if let (
                    Operand::Copy(orig_place) | Operand::Move(orig_place),
                    Operand::Copy(new_place) | Operand::Move(new_place),
                ) = (&orig, &new_op) {
                    if orig_place.local != new_place.local {
                        pre_call_clone_temps.push(new_place.local);
                    }
                }
                lowered_method_args[idx] = new_op;
            }
        }
        call_args.extend(lowered_method_args.iter().cloned());

        // Restore previous hints and expected type
        ctx.func_state.closure_param_type_hints = prev_hints;
        ctx.func_state.expected_type = prev_expected;

        // For Vector.zip(other_vec), register tuple and result vector types
        if method_name == "zip" && type_name.starts_with("Vector__") {
            let self_elem = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            // Get the other vector's element type from the first explicit arg
            let other_elem_name = if let Some(arg_op) = lowered_method_args.first() {
                if let Operand::Copy(p) | Operand::Move(p) = arg_op {
                    let type_id = builder.local_type(p.local);
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

        // Borrowing methods (get/first/last) always return `Option__Ref__T` with a
        // Ptr(T) payload, regardless of whether T is a resource type. This keeps
        // the IR's return type identical to the user-declared `Option[Ref[T]]`
        // and avoids aliasing an int-value as a pointer when the two forms
        // used to diverge (IR said Option[T], typechecker said Option[Ref[T]]).
        // Consuming methods (pop/remove) keep `Option__T` with a value payload.
        let fn_sig_ret = ctx.fn_sigs.get(&effective_name).map(|(_, ret)| *ret);
        if matches!(method_name, "get" | "first" | "last" | "remove" | "pop")
            && (type_name.starts_with("Vector__") || type_name == "GorgetArray")
        {
            let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            let inner_type = resolve_inner_type(ctx, elem_type_name);
            let is_borrowing = matches!(method_name, "get" | "first" | "last");
            if is_borrowing {
                let option_name = format!("Option__Ref__{elem_type_name}");
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
                let _inner_type = resolve_inner_type(ctx, elem_type_name);
                let is_borrowing = matches!(method_name, "get" | "first" | "last");
                let option_name = if is_borrowing {
                    format!("Option__Ref__{elem_type_name}")
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
                && (type_name.starts_with("Dict__") || type_name.starts_with("HashMap__"))
            {
                // Dict/HashMap.remove(key) → Option[V !]
                let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                if let Some(rest) = type_name.strip_prefix(prefix) {
                    if let Some(pos) = rest.find("__") {
                        let val_name = &rest[pos + 2..];
                        let option_name = format!("Option__{val_name}");
                        ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
                    } else { ret }
                } else { ret }
            } else if method_name == "remove"
                && (type_name.starts_with("Set__") || type_name.starts_with("HashSet__"))
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
                    let type_id = builder.local_type(p.local);
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

        // Collect locals to MoveZero after the call — transfers ownership to callee.
        // Coordinates with the pre-call clone section above:
        //   - Explicit `!arg`  → always move_zero (caller wants move).
        //   - Bare arg, identifier, named, last-use → move_zero (zero-cost transfer).
        //   - Bare arg, identifier, named, NON-last-use → already cloned pre-call;
        //     the source must NOT be zeroed (it's still live).
        //   - Bare arg, expression temp (non-identifier) → move_zero the lowered
        //     temp local (always effectively "last use"). Its scope-exit drop
        //     becomes a no-op on the NULL'd resource.
        //   - Bare arg, bare param → already cloned pre-call; caller's data stays.
        //
        // Builtin consuming methods (push/put/set/insert/...) don't always have
        // fn_param_ownerships entries, so we fall back to the method-name
        // `consuming_positions` whitelist computed once above.
        let move_zero_locals: Vec<Place> = args.iter()
            .enumerate()
            .filter_map(|(i, arg)| {
                let call_site_move = matches!(arg.node.ownership, Ownership::Move);
                let callee_move = ctx.fn_param_ownerships.get(&sig_name)
                    .and_then(|ownerships| ownerships.get(i + 1))
                    .map(|o| matches!(o, Ownership::Move))
                    .unwrap_or(false)
                    || consuming_positions.contains(&i);
                if !call_site_move && !callee_move { return None; }

                // For explicit `!`: always move_zero the identifier source.
                if call_site_move {
                    if let Expr::Identifier(name) = &arg.node.value.node {
                        if let Some((local_id, _)) = ctx.lookup_local(name) {
                            if is_resource_type_local(local_id, builder, &ctx.type_registry) {
                                return Some(Place::local(local_id));
                            }
                        }
                    }
                    return None;
                }

                // Bare arg with callee_move: two sub-cases.
                if let Expr::Identifier(name) = &arg.node.value.node {
                    // Identifier: only move_zero if last use (non-last-use was cloned pre-call).
                    let (local_id, _) = ctx.lookup_local(name)?;
                    if !is_resource_type_local(local_id, builder, &ctx.type_registry) {
                        return None;
                    }
                    // Skip non-drop-tracked locals — they're borrows (for-loop string
                    // vars aliasing the outer collection, bare params, etc.). The
                    // pre-call clone section already produced an owned copy for them.
                    if !ctx.drops.is_registered(local_id) { return None; }
                    // Skip bare params / ref locals / CoW borrows (same reasoning).
                    if ctx.is_bare_param(local_id) { return None; }
                    if ctx.is_ref_local(local_id) { return None; }
                    if ctx.is_cow_borrow(local_id) { return None; }
                    // Skip non-named locals (should be rare — falls through via temp path).
                    if !ctx.is_named_local(local_id) { return None; }
                    // Only zero on last use.
                    if !ctx.is_last_use_at(name, arg.node.value.span) { return None; }
                    return Some(Place::local(local_id));
                }

                // Non-identifier (expression temp): zero the lowered temp local.
                // Expression temps are always "last use" by construction.
                let op = lowered_method_args.get(i)?;
                let place = match op {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => p.clone(),
                    _ => return None,
                };
                if !is_resource_type_local(place.local, builder, &ctx.type_registry) {
                    return None;
                }
                // Skip Ptr wrappers (CoW clone already replaced them with a temp that
                // the callee owns; the original Ptr source doesn't need zeroing).
                let local_type = builder.local_type(place.local);
                if matches!(
                    ctx.type_registry.get(local_type),
                    Some(crate::ir::types::GirType::Ptr(_)) | Some(crate::ir::types::GirType::MutPtr(_))
                ) {
                    return None;
                }
                Some(place)
            })
            .collect();

        // For fold, refine ret_type from the init value's type (call_args[1])
        // so the destination local gets the correct type (e.g., double for float fold).
        // Note: fold has call_args = [receiver, init, closure], reduce has [receiver, closure].
        let ret_type = if method_name == "fold" && call_args.len() > 2 {
            match &call_args[1] {
                Operand::Constant(Constant::F64(_)) => F64_TYPE,
                Operand::Constant(Constant::Str(_)) => ctx.type_mapper.owned_string_type,
                Operand::Copy(p) | Operand::Move(p) => {
                    let init_type = builder.local_type(p.local);
                    if init_type != I64_TYPE { init_type } else { ret_type }
                }
                _ => ret_type,
            }
        } else {
            ret_type
        };

        // Auto-clone Ptr(resource) args at consuming method positions — the
        // Ptr(Ptr(resource)) fallback for cases the pre-call section above
        // missed (e.g. call arg is wrapped in an extra Ptr layer by
        // `lower_call_arg`'s borrow materialization).
        //
        // Uses the last consuming position (value position for put/set/insert,
        // element for push/add/...) — same `consuming_positions` list computed
        // at the top of the function.
        let mut consuming_clone_temps: Vec<LocalId> = Vec::new();
        if let Some(&value_idx) = consuming_positions.last() {
            let call_idx = 1 + value_idx;
            // Check call_args first (ptr-wrapped). For Ptr(resource) field
            // accesses the call arg is Ptr(Ptr(resource)) — pointee_type
            // gives Ptr(resource) which is_resource_type misses. Fall back
            // to lowered_method_args (pre-wrapping) which has Ptr(resource)
            // → pointee_type gives resource → is_resource_type matches.
            let needs_clone = call_args.get(call_idx).and_then(|op| {
                if let Operand::Copy(place) | Operand::Move(place) = op {
                    if place.projections.is_empty() {
                        let local_type = builder.local_type(place.local);
                        if let Some(inner) = ctx.pointee_type(local_type) {
                            if ctx.type_registry.is_resource_type(inner) {
                                return Some((place.local, inner));
                            }
                        }
                    }
                }
                None
            }).or_else(|| {
                // Fallback: check pre-wrapped arg (handles Ptr(resource) from field access)
                lowered_method_args.get(value_idx).and_then(|op| {
                    if let Operand::Copy(place) | Operand::Move(place) = op {
                        if place.projections.is_empty() {
                            let local_type = builder.local_type(place.local);
                            if let Some(inner) = ctx.pointee_type(local_type) {
                                if ctx.type_registry.is_resource_type(inner) {
                                    return Some((place.local, inner));
                                }
                            }
                        }
                    }
                    None
                })
            });
            if let Some((ptr_local, inner_type)) = needs_clone {
                if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner_type) {
                    let span = args.get(value_idx).map(|a| a.span).unwrap_or(receiver.span);
                    ctx.warn_implicit_clone(span, inner_type, crate::ir::ImplicitCloneReason::ConsumingArg);
                    let cloned = builder.call(&clone_fn,
                        vec![FunctionBuilder::copy(ptr_local)], inner_type);
                    ctx.drops.register_local(cloned, inner_type, &ctx.type_registry);
                    ctx.set_owned(cloned);
                    consuming_clone_temps.push(cloned);
                    let ptr_type = ctx.register_ptr_type(inner_type);
                    let ptr = builder.add_local(ptr_type, None);
                    builder.emit_borrow(ptr, Place::local(cloned));
                    call_args[call_idx] = FunctionBuilder::copy(ptr);
                }
            }
        }

        // Upgrade consuming call args from Copy to Move (Rust-style ownership on
        // operand).  This enables the LIR lowering to emit generic post-call
        // zeroing without hardcoded function-name matching.  The move_zero_locals
        // and pre_call_clone_temps identify which locals are consumed; we match
        // them to call_args by local id.
        for arg in call_args.iter_mut() {
            if let Operand::Copy(place) = arg {
                if place.projections.is_empty() {
                    let dominated = move_zero_locals.iter().any(|mz| mz.local == place.local)
                        || pre_call_clone_temps.contains(&place.local);
                    if dominated {
                        *arg = Operand::Move(place.clone());
                    }
                }
            }
        }

        // Option-returning Vector methods where the C runtime returns void*.
        // Generate null-check + Option.Some/None construction at GIR level so both
        // C and LLVM backends see truthful IR (extern returns Ptr, Option is explicit).
        let is_option_void_ptr_vector = matches!(method_name, "get" | "first" | "last" | "pop" | "remove")
            && (type_name.starts_with("Vector__") || type_name == "GorgetArray")
            && ret_type != UNIT_TYPE
            && ctx.type_name_for_id(ret_type).map_or(false, |n| n.starts_with("Option__"));

        let result = if is_option_void_ptr_vector {
            let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            let inner_type = resolve_inner_type(ctx, elem_type_name);
            let is_borrowing = matches!(method_name, "get" | "first" | "last");
            // Borrowing methods always produce Option__Ref__T with a Ptr(T) payload —
            // the raw pointer from the runtime `gorget_array_safe_get` IS the payload.
            // Consuming methods (pop/remove) deref to take ownership of the value.
            let payload_is_ptr = is_borrowing;

            // Call with Ptr return type (truthful void* ABI)
            let ptr_type = ctx.register_ptr_type(inner_type);
            let raw_ptr = builder.call(call_name, call_args, ptr_type);

            // Null check: raw_ptr != null
            let is_not_null = builder.cmp(
                CmpOp::Ne, ptr_type,
                FunctionBuilder::copy(raw_ptr),
                FunctionBuilder::const_null(),
            );

            let result_id = builder.add_local(ret_type, None);
            let some_bb = builder.new_block();
            let none_bb = builder.new_block();
            let merge_bb = builder.new_block();
            builder.branch(FunctionBuilder::copy(is_not_null), some_bb, none_bb);

            // === Some block: construct Option.Some(payload) ===
            builder.switch_to(some_bb);
            let option_name = ctx.type_name_for_id(ret_type)
                .unwrap_or("Option__int64_t").to_string();
            let payload = if payload_is_ptr {
                // Option__Ref_T: the raw pointer IS the payload (borrowed reference)
                FunctionBuilder::copy(raw_ptr)
            } else {
                // Dereference void* to get the element value.
                // Uses Deref projection — LIR loads from the pointer address.
                Operand::Copy(Place {
                    local: raw_ptr,
                    projections: vec![Projection::Deref],
                })
            };
            let some_val = builder.enum_init(&option_name, "Some", ret_type, vec![payload]);
            builder.assign(Place::local(result_id), FunctionBuilder::copy(some_val));
            builder.jump(merge_bb);

            // === None block: construct Option.None() ===
            builder.switch_to(none_bb);
            let none_val = builder.enum_init(&option_name, "None", ret_type, vec![]);
            builder.assign(Place::local(result_id), FunctionBuilder::copy(none_val));
            builder.jump(merge_bb);

            // === Merge ===
            builder.switch_to(merge_bb);
            if ctx.type_registry.needs_drop(ret_type) {
                ctx.drops.register_local(result_id, ret_type, &ctx.type_registry);
            }
            ctx.set_owned(result_id);

            // Track collection provenance for Option__Ref_ results.
            // Case A: named-local receiver → `Local(recv)`.
            // Case B: field-access receiver with NO recv temp → `FieldPath(...)`.
            // Case C: anon recv temp + field_path — ACTIVATED. Safe now that
            //   (a) save/restore covers local_ownership, (b) restore clears
            //   branch-local CollectionRef/CowBorrow entries, (c) f-string deref
            //   emits a deep clone for resource-containing struct types instead
            //   of a shallow memcpy, (d) prescan walks every path ancestor.
            if let Some(ret_name) = ctx.type_name_for_id(ret_type) {
                if ret_name.starts_with("Option__Ref__") {
                    if let Some(recv_local) = recv_local_for_move_zero {
                        if ctx.is_named_local(recv_local) {
                            ctx.set_cow_borrow_source(result_id, CollectionId::Local(recv_local));
                        } else if let Some(ref field_path) = field_path_for_cow {
                            if !ctx.is_source_mut_unsafe_at(field_path, receiver.span.start) {
                                ctx.set_cow_borrow_source(result_id, CollectionId::FieldPath(field_path.clone()));
                            }
                        }
                    } else if let Some(ref field_path) = field_path_for_cow {
                        if !ctx.is_source_mut_unsafe_at(field_path, receiver.span.start) {
                            ctx.set_cow_borrow_source(result_id, CollectionId::FieldPath(field_path.clone()));
                        }
                    }
                }
            }
            FunctionBuilder::copy(result_id)
        } else if ret_type == UNIT_TYPE {
            builder.call_void(call_name, call_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = ctx.call_tracked(builder, call_name, call_args, ret_type);
            // Trivial getter clone elision: result is Ptr(T) — mark as CowBorrow
            // so the caller sees a zero-cost borrow with collection provenance.
            if ctx.trivial_getter_methods.contains(sig_name.as_str()) {
                ctx.set_cow_borrow(dst);
                if let Some(recv_local) = recv_local_for_move_zero {
                    if ctx.is_named_local(recv_local) {
                        ctx.set_cow_borrow_source(dst, CollectionId::Local(recv_local));
                    } else if let Some(ref field_path) = field_path_for_cow {
                        if !ctx.is_source_mut_unsafe_at(field_path, receiver.span.start) {
                            ctx.set_cow_borrow_source(dst, CollectionId::FieldPath(field_path.clone()));
                        }
                    }
                } else if let Some(ref field_path) = field_path_for_cow {
                    if !ctx.is_source_mut_unsafe_at(field_path, receiver.span.start) {
                        ctx.set_cow_borrow_source(dst, CollectionId::FieldPath(field_path.clone()));
                    }
                }
            }
            // Track collection provenance for Option__Ref__ results (from .get(), .first(), etc.).
            if let Some(ret_name) = ctx.type_name_for_id(ret_type) {
                if ret_name.starts_with("Option__Ref__") {
                    if let Some(recv_local) = recv_local_for_move_zero {
                        if ctx.is_named_local(recv_local) {
                            ctx.set_cow_borrow_source(dst, CollectionId::Local(recv_local));
                        } else if let Some(ref field_path) = field_path_for_cow {
                            if !ctx.is_source_mut_unsafe_at(field_path, receiver.span.start) {
                                ctx.set_cow_borrow_source(dst, CollectionId::FieldPath(field_path.clone()));
                            }
                        }
                    } else if let Some(ref field_path) = field_path_for_cow {
                        if !ctx.is_source_mut_unsafe_at(field_path, receiver.span.start) {
                            ctx.set_cow_borrow_source(dst, CollectionId::FieldPath(field_path.clone()));
                        }
                    }
                }
            }
            FunctionBuilder::copy(dst)
        };

        // MoveZero Move-ownership args to transfer ownership (prevent double-free).
        // The LIR's emit_post_call_zeros handles args that are directly in
        // call_args as Operand::Move.  The GIR MoveZero is still needed for
        // args wrapped in borrow ptrs (field loads, MutPtr params) whose
        // source local is not the call arg's local.
        for place in &move_zero_locals {
            builder.move_zero(place.clone());
            ctx.emit_field_origin_zero(builder, place.local);
            ctx.drops.mark_moved(place.local);
        }

        // !self consuming methods: MoveZero the receiver after the call.
        // The callee consumed self's resource fields via MoveZeroSource;
        // zeroing the receiver prevents double-free at scope exit.
        if has_consuming_self {
            if let Some(recv_local) = recv_local_for_move_zero {
                if !ctx.drops.is_moved(recv_local) {
                    ctx.move_zero_and_mark(builder, recv_local);
                }
            }
        }

        // MoveZero Option/Result receiver after combinator calls to prevent
        // the scope-exit destructor from double-freeing the consumed payload.
        // The combinator's inline C code shallow-copies the payload for the closure;
        // MoveZero transfers ownership to the returned value.
        if !has_consuming_self {
            if let Some(recv_local) = recv_local_for_move_zero {
                let is_option_result = type_name.starts_with("Option__")
                    || type_name.starts_with("Result__");
                let is_combinator = matches!(method_name,
                    "map" | "and_then" | "or_else" | "filter" | "unwrap_or_else"
                    | "flat_map" | "or" | "flatten" | "map_err");
                if is_option_result && is_combinator
                    && ctx.type_registry.is_resource_type(builder.local_type(recv_local))
                    && !ctx.drops.is_moved(recv_local)
                {
                    // Move-if-dead: combinator consumes the receiver.
                    ctx.drops.unregister(recv_local);
                    ctx.move_zero_and_mark(builder, recv_local);
                }
            }
        }

        // MoveZero clone temps from consuming-position auto-clone.
        // The push/put/set memcpy'd the clone into the collection buffer;
        // the temp must be zeroed so its DropIfAlive guard doesn't
        // double-free the data the collection now owns.
        for local in &consuming_clone_temps {
            ctx.move_zero_and_mark(builder, *local);
        }
        // MoveZero pre-call clone temps.  Some are in call_args as
        // Operand::Move (LIR handles zeroing); others are behind borrow
        // ptrs and need GIR MoveZero.
        for local in &pre_call_clone_temps {
            ctx.move_zero_and_mark(builder, *local);
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
        // Drain pending_move_zeros from lower_call_arg. These were borrowed
        // (borrow_mut) for the callee; now that the call has returned, zero
        // the source to prevent double-free at scope exit.
        let pending: Vec<LocalId> = ctx.func_state.pending_move_zeros.drain(move_zero_baseline..).collect();
        for local in pending {
            builder.move_zero(Place::local(local));
            ctx.drops.mark_moved(local);
        }

        // Track ViewOf provenance for view-returning builtin methods (slice, trim, etc.).
        // The result is a cap=0 Str borrowing from the receiver's buffer.
        // Keep it drop-registered (gorget_string_free short-circuits on cap==0).
        // Mark ViewOf(receiver) so cow_before_mutation materializes if source mutates.
        if ctx.builtin_returns_view(&type_name, method_name)
            && ctx.type_mapper.is_string_type(ret_type)
        {
            if let Operand::Copy(ref result_place) | Operand::Move(ref result_place) = result {
                if result_place.projections.is_empty() {
                    let result_local = result_place.local;
                    // Track provenance: result borrows from receiver.
                    // Only for named locals — expression temps in chains should
                    // NOT be marked as refs (it changes receiver borrow semantics).
                    if ctx.is_named_local(result_local) {
                        if let Some(recv_local) = recv_local_for_move_zero {
                            ctx.set_view_of(result_local, recv_local);
                        }
                        ctx.func_state.has_string_borrows = true;
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
                // Wrap pre-lowered args in Ptr borrows where the closure expects Ptr ABI
                for (i, arg) in call_args.into_iter().enumerate() {
                    let abi = ctx.fn_param_abis.get(call_fn.as_str())
                        .and_then(|abis| abis.get(i + 1)) // +1 for env ptr
                        .copied();
                    if matches!(abi, Some(ParamABI::ByPtr) | Some(ParamABI::ByMutPtr)) {
                        if let Operand::Copy(ref place) | Operand::Move(ref place) = arg {
                            if place.projections.is_empty() {
                                let local_type = builder.local_type(place.local);
                                if !matches!(ctx.type_registry.get(local_type),
                                    Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_)))
                                {
                                    let ptr_type = ctx.register_ptr_type(local_type);
                                    let dst = builder.add_local(ptr_type, None);
                                    builder.emit_borrow(dst, place.clone());
                                    final_args.push(FunctionBuilder::copy(dst));
                                    continue;
                                }
                            }
                        }
                    }
                    final_args.push(arg);
                }

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

/// GIR-level desugaring of Option/Result combinators.
/// Replaces C backend inline functions with explicit tag check, field extraction,
/// closure call, and enum construction — giving the compiler full ownership visibility.
fn try_lower_option_result_combinator(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_name: &str,
    method_name: &str,
    recv: Operand,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    let is_option = type_name.starts_with("Option__");
    let is_result = type_name.starts_with("Result__");
    if !is_option && !is_result { return None; }
    if args.is_empty() { return None; }

    // Resolve the receiver's TypeId and the result type for the combinator.
    let recv_type = infer_operand_type_full(ctx, &recv, builder);

    // Resolve inner types from the TypeDef (needed for bail check below)
    let (some_ok_type, none_err_type) = if is_option {
        let inner = ctx.type_registry.get_type_def(type_name)
            .and_then(|td| if let TypeDefKind::Enum(ref e) = td.kind {
                e.variants.iter().find(|v| v.name == "Some")
                    .and_then(|v| v.fields.first().map(|f| f.type_id))
            } else { None })
            .unwrap_or(I64_TYPE);
        (inner, UNIT_TYPE)
    } else {
        let td = ctx.type_registry.get_type_def(type_name);
        let ok_ty = td.as_ref().and_then(|td| if let TypeDefKind::Enum(ref e) = td.kind {
            e.variants.iter().find(|v| v.name == "Ok")
                .and_then(|v| v.fields.first().map(|f| f.type_id))
        } else { None }).unwrap_or(I64_TYPE);
        let err_ty = td.as_ref().and_then(|td| if let TypeDefKind::Enum(ref e) = td.kind {
            e.variants.iter().find(|v| v.name == "Error")
                .and_then(|v| v.fields.first().map(|f| f.type_id))
        } else { None }).unwrap_or(I64_TYPE);
        (ok_ty, err_ty)
    };

    // Bail to C inline path for types needing GorgetString→Str coercion.
    // The C inline combinator handles this implicitly; GIR would need explicit coercion.
    let has_string_coercion = |ty: TypeId| -> bool {
        matches!(ctx.type_registry.get(ty), Some(GirType::Named(n)) if n == "GorgetString")
    };
    match method_name {
        "map" | "filter" | "and_then" | "flat_map" | "unwrap_or_else" if has_string_coercion(some_ok_type) => return None,
        "map_err" | "or_else" if has_string_coercion(none_err_type) => return None,
        _ => {}
    }

    // Store receiver in a local for field extraction (after bail checks)
    let scrut_local = builder.add_local(recv_type, None);
    builder.assign(Place::local(scrut_local), recv.clone());

    // Set closure param type hints so untyped closure params get correct types
    let prev_hints = std::mem::take(&mut ctx.func_state.closure_param_type_hints);
    match method_name {
        "map" | "filter" | "flat_map" | "and_then" => {
            ctx.func_state.closure_param_type_hints = vec![some_ok_type];
        }
        "map_err" | "or_else" => {
            ctx.func_state.closure_param_type_hints = vec![none_err_type];
        }
        "unwrap_or_else" => {
            // closure takes no args
        }
        _ => {}
    }

    // Set expected_type for and_then/or_else closures that return Option/Result
    let prev_expected = ctx.func_state.expected_type;
    if matches!(method_name, "and_then" | "or_else" | "flat_map") {
        if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
            ctx.func_state.expected_type = Some(type_id);
        }
    }

    // Lower the closure argument
    let closure_op = lower_expr(ctx, builder, &args[0].node.value);

    // Restore hints
    ctx.func_state.closure_param_type_hints = prev_hints;
    ctx.func_state.expected_type = prev_expected;

    // Check tag: 0 = Some/Ok, 1 = None/Error
    let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
    let is_some_ok = builder.cmp(
        CmpOp::Eq, I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let some_bb = builder.new_block();
    let none_bb = builder.new_block();
    let merge_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(is_some_ok), some_bb, none_bb);

    // Move-if-dead: unregister scrutinee from drops before branching
    ctx.drops.unregister(scrut_local);
    if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
        if place.projections.is_empty() {
            ctx.drops.unregister(place.local);
        }
    }

    // Determine the result type (may differ from recv_type for cross-type map)
    let result_type = match method_name {
        "unwrap_or_else" => some_ok_type,
        "map" => {
            // Closure returns U → result is Option[U] or Result[U, E]
            let mapped_ret = infer_closure_return_type(ctx, &closure_op, builder);
            if mapped_ret != some_ok_type && mapped_ret != UNIT_TYPE {
                let mapped_name = crate::ir::types::format_type_for_mangle(mapped_ret, &ctx.type_registry);
                if is_option {
                    let option_name = format!("Option__{mapped_name}");
                    ctx.ensure_option_type_registered(&option_name, mapped_ret);
                    ctx.lookup_type_by_name(&option_name).unwrap_or(recv_type)
                } else {
                    // Result[Ok, Err].map(fn → U) → Result[U, Err]
                    let err_name = crate::ir::types::format_type_for_mangle(none_err_type, &ctx.type_registry);
                    let result_name = format!("Result__{mapped_name}__{err_name}");
                    if ctx.lookup_type_by_name(&result_name).is_none() {
                        use super::super::types::make_result_type_def;
                        ctx.type_mapper.get_or_register(&result_name, &mut ctx.type_registry, |n| {
                            make_result_type_def(n, mapped_ret, none_err_type)
                        });
                    }
                    ctx.lookup_type_by_name(&result_name).unwrap_or(recv_type)
                }
            } else {
                recv_type
            }
        }
        "map_err" if is_result => {
            // Closure returns U → Result[Ok, U]
            let mapped_ret = infer_closure_return_type(ctx, &closure_op, builder);
            if mapped_ret != none_err_type && mapped_ret != UNIT_TYPE {
                let ok_name = crate::ir::types::format_type_for_mangle(some_ok_type, &ctx.type_registry);
                let mapped_name = crate::ir::types::format_type_for_mangle(mapped_ret, &ctx.type_registry);
                let result_name = format!("Result__{ok_name}__{mapped_name}");
                if ctx.lookup_type_by_name(&result_name).is_none() {
                    use super::super::types::make_result_type_def;
                    ctx.type_mapper.get_or_register(&result_name, &mut ctx.type_registry, |n| {
                        make_result_type_def(n, some_ok_type, mapped_ret)
                    });
                }
                ctx.lookup_type_by_name(&result_name).unwrap_or(recv_type)
            } else {
                recv_type
            }
        }
        _ => recv_type,
    };

    let result_local = builder.add_local(result_type, None);
    let result_type_name = ctx.type_name_for_id(result_type).unwrap_or(type_name).to_string();

    // === Some/Ok branch ===
    builder.switch_to(some_bb);
    let payload = builder.enum_field_load_move(Place::local(scrut_local), if is_option { "Some" } else { "Ok" }, 0, some_ok_type);

    match method_name {
        "map" => {
            // map(fn) → Some/Ok(fn(payload))
            let mapped = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(payload)], some_ok_type);
            let wrapped = builder.enum_init(&result_type_name, if is_option { "Some" } else { "Ok" }, result_type, vec![mapped]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(wrapped));
        }
        "and_then" | "flat_map" => {
            // and_then(fn) → fn(payload) (fn returns Option/Result)
            let result = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(payload)], result_type);
            builder.assign(Place::local(result_local), result);
        }
        "or_else" => {
            // or_else: Some/Ok path → keep original
            let wrapped = builder.enum_init(&result_type_name, if is_option { "Some" } else { "Ok" }, result_type, vec![FunctionBuilder::copy(payload)]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(wrapped));
        }
        "filter" if is_option => {
            // filter(fn) → if fn(payload): Some(payload) else: None
            let pred = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(payload)], BOOL_TYPE);
            let filter_then = builder.new_block();
            let filter_else = builder.new_block();
            builder.branch(pred, filter_then, filter_else);
            builder.switch_to(filter_then);
            let some_val = builder.enum_init(&result_type_name, "Some", result_type, vec![FunctionBuilder::copy(payload)]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(some_val));
            builder.jump(merge_bb);
            builder.switch_to(filter_else);
            let none_val = builder.enum_init(&result_type_name, "None", result_type, vec![]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(none_val));
            // Don't jump to merge — fall through to the common jump below.
            // Actually, we need to jump to merge since the None branch below is separate.
            builder.jump(merge_bb);
            // Switch to a dummy block so the common `builder.jump(merge_bb)` below
            // doesn't add a duplicate jump from this block.
            let dummy = builder.new_block();
            builder.switch_to(dummy);
        }
        "unwrap_or_else" => {
            // unwrap_or_else(fn) → payload
            builder.assign(Place::local(result_local), FunctionBuilder::copy(payload));
        }
        "map_err" if is_result => {
            // map_err: Ok path → keep original Ok
            let wrapped = builder.enum_init(&result_type_name, "Ok", result_type, vec![FunctionBuilder::copy(payload)]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(wrapped));
        }
        _ => return None,
    }

    builder.jump(merge_bb);

    // === None/Error branch ===
    builder.switch_to(none_bb);
    match method_name {
        "map" | "filter" | "and_then" | "flat_map" if is_option => {
            // None → None
            let none_val = builder.enum_init(&result_type_name, "None", result_type, vec![]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(none_val));
        }
        "or_else" if is_option => {
            // or_else: None → fn()
            let result = call_closure_in_adapter(ctx, builder, &closure_op, vec![], result_type);
            builder.assign(Place::local(result_local), result);
        }
        "unwrap_or_else" if is_option => {
            // unwrap_or_else: None → fn()
            let result = call_closure_in_adapter(ctx, builder, &closure_op, vec![], some_ok_type);
            builder.assign(Place::local(result_local), result);
        }
        "map" | "and_then" | "flat_map" if is_result => {
            // Error → Error(err)
            let err_val = builder.enum_field_load_move(Place::local(scrut_local), "Error", 0, none_err_type);
            let wrapped = builder.enum_init(&result_type_name, "Error", result_type, vec![FunctionBuilder::copy(err_val)]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(wrapped));
        }
        "or_else" if is_result => {
            // or_else: Error → fn(err)
            let err_val = builder.enum_field_load_move(Place::local(scrut_local), "Error", 0, none_err_type);
            let result = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(err_val)], result_type);
            builder.assign(Place::local(result_local), result);
        }
        "unwrap_or_else" if is_result => {
            // unwrap_or_else: Error → fn(err)
            let err_val = builder.enum_field_load_move(Place::local(scrut_local), "Error", 0, none_err_type);
            let result = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(err_val)], some_ok_type);
            builder.assign(Place::local(result_local), result);
        }
        "map_err" if is_result => {
            // map_err: Error → Error(fn(err))
            let err_val = builder.enum_field_load_move(Place::local(scrut_local), "Error", 0, none_err_type);
            let mapped = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(err_val)], none_err_type);
            let wrapped = builder.enum_init(&result_type_name, "Error", result_type, vec![mapped]);
            builder.assign(Place::local(result_local), FunctionBuilder::copy(wrapped));
        }
        _ => return None,
    }
    builder.jump(merge_bb);

    // === Merge ===
    builder.switch_to(merge_bb);
    if ctx.type_registry.needs_drop(result_type)
        || ctx.type_registry.is_resource_type(result_type) {
        ctx.drops.register_local(result_local, result_type, &ctx.type_registry);
    }
    ctx.set_owned(result_local);

    Some(FunctionBuilder::copy(result_local))
}

/// Infer the return type of a closure operand from its __call function signature.
fn infer_closure_return_type(
    ctx: &LoweringContext,
    closure_op: &Operand,
    builder: &FunctionBuilder,
) -> TypeId {
    if let Operand::Copy(place) | Operand::Move(place) = closure_op {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;
            if let Some(type_name) = ctx.type_name_for_id(local_type_id) {
                let type_name = type_name.to_string();
                if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                    if let Some((_, ret)) = ctx.fn_sigs.get(call_fn) {
                        return *ret;
                    }
                }
            }
        }
    }
    if let Operand::Constant(Constant::FuncRef(name)) = closure_op {
        if let Some((_, ret)) = ctx.fn_sigs.get(name.as_str()) {
            return *ret;
        }
    }
    I64_TYPE
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

        // Resolve through Ptr — string params are now Ptr(GorgetString)
        let resolved_base = ctx.pointee_type(base_type).unwrap_or(base_type);
        let elem_type = if ctx.type_mapper.is_string_type(resolved_base) {
            ctx.type_mapper.owned_string_type // indexing a string returns Str
        } else {
            // Try to infer element type from collection type name
            infer_collection_element_type(ctx, base_type)
        };
        // CoW: all resource-type elements return Ptr(T) — zero-cost borrow.
        // Auto-clone happens at Ptr→T boundaries (call args, VarDecl, return, etc.).
        let is_task = matches!(ctx.type_registry.get(elem_type),
            Some(GirType::Named(n)) if n.starts_with("Task__"));
        // String character indexing: returns a new Str value (not a borrow).
        let is_string_base = ctx.type_mapper.is_string_type(resolved_base);
        let result_type = if is_task || is_string_base {
            elem_type
        } else if ctx.type_registry.is_resource_type(elem_type) {
            ctx.register_ptr_type(elem_type)
        } else {
            elem_type
        };
        let dst = builder.index_load(place.clone(), idx, result_type);
        if ctx.type_registry.is_resource_type(elem_type) && !is_task && !is_string_base {
            // Use FieldPath provenance when the base is a field access (e.g., s.v[0]).
            // This ensures cow_before_field_mutation("s.v") finds the ref when
            // s.v.push(x) is called later. Without this, the ref is keyed on the
            // FieldLoad temp LocalId, which cow_before_field_mutation can't find.
            let collection_id = extract_field_path_string(&object.node)
                .map(CollectionId::FieldPath)
                .unwrap_or_else(|| CollectionId::Local(place.local));
            ctx.set_collection_ref(dst, collection_id);
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
            // Callable element types → FnPtr TypeId so sizeof(elem) lowers to
            // sizeof(GorgetClosure) = 16. Otherwise the unrecognized
            // `Callable__…` name falls through to I64 (8 bytes) and
            // gorget_array_new is created with the wrong elem_size — push
            // writes only the first 8 bytes of the closure into the slot,
            // leaving env uninitialized and causing SIGSEGV/SIGBUS when the
            // closure is read back and called (attack_82).
            if elem_name.starts_with("Callable__")
                || elem_name.starts_with("MutCallable__")
                || elem_name.starts_with("ConsumeCallable__")
            {
                return ctx.type_registry.insert(GirType::FnPtr {
                    params: vec![],
                    return_type: I64_TYPE,
                });
            }
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
            Constant::Str(_) => return Some("GorgetString".to_string()),
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
    if effective_tid == ctx.type_mapper.owned_string_type {
        return Some("GorgetString".to_string());
    }
    // Scalar primitives — return their C mangled names so method
    // dispatch on `equip int:` / `equip bool:` / etc. resolves to the
    // same `int64_t__method` / `bool__method` registration used by
    // `equip_target_name` in types.rs. Without this, a receiver with
    // type I64_TYPE gets type_name=None and the call site falls
    // through to `Constant::Unit`.
    if effective_tid == I64_TYPE  { return Some("int64_t".to_string()); }
    if effective_tid == I32_TYPE  { return Some("int32_t".to_string()); }
    if effective_tid == I16_TYPE  { return Some("int16_t".to_string()); }
    if effective_tid == I8_TYPE   { return Some("int8_t".to_string()); }
    if effective_tid == U64_TYPE  { return Some("uint64_t".to_string()); }
    if effective_tid == U32_TYPE  { return Some("uint32_t".to_string()); }
    if effective_tid == U16_TYPE  { return Some("uint16_t".to_string()); }
    if effective_tid == U8_TYPE   { return Some("uint8_t".to_string()); }
    if effective_tid == F64_TYPE  { return Some("double".to_string()); }
    if effective_tid == F32_TYPE  { return Some("float".to_string()); }
    if effective_tid == BOOL_TYPE { return Some("bool".to_string()); }
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
                || name.starts_with("Callable__")
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
            "GorgetString" => Some(ctx.type_mapper.owned_string_type),
            _ => ctx.lookup_type_by_name(elem)
                .or_else(|| ctx.type_mapper.lookup_named(elem)),
        }
    })
}

