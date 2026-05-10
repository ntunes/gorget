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
            ensure_task_group_type_def, get_or_register_type,
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
                // CoW: mutable borrow mutates the local. Sever aliases first.
                ctx.cow_before_mutation(builder, local_id, arg.span);
                let is_already_ptr = {
                    let lid = local_id.0 as usize;
                    lid < builder.locals.len() && matches!(
                        ctx.type_registry.get(builder.locals[lid].type_id),
                        Some(GirType::MutPtr(_)) | Some(GirType::Ptr(_))
                    )
                };
                if ctx.is_ref_local(builder, local_id)
                    || ctx.is_param_borrow_unique(builder, local_id)
                    || is_already_ptr
                {
                    return FunctionBuilder::copy(local_id);
                }
            }
        }
    }

    // Special case: !name where name is a `!`-sigil resource parameter (the
    // local already holds a MutPtr to caller-owned data). Forward the pointer
    // directly and emit MoveZero on the param slot, bypassing the
    // Identifier-path's deref-into-temp + memcpy. Without this, the temp
    // and the caller's R buffer would alias the same heap data; both the
    // inner callee's exit drop (on the temp's transferred ownership) and
    // this function's exit drop (on its own `!`-param) would fire,
    // double-freeing the resource.
    //
    // Detection: `is_owning_param` is the typed bit set at param
    // registration for `Ownership::Move` resource params. The flag drives
    // both this fast-path and the `lower_drop` deref-aware emission, so
    // there's no name-matching or shape inference downstream.
    if matches!(arg.node.ownership, Ownership::Move) {
        if let Expr::Identifier(name) = &arg.node.value.node {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                let is_owning_param = (local_id.0 as usize) < builder.locals.len()
                    && builder.locals[local_id.0 as usize].is_owning_param;
                if is_owning_param {
                    // Sever any CoW aliases of the source slot before transfer.
                    ctx.cow_before_mutation(builder, local_id, arg.span);
                    // Forward the pointer (the local holds a MutPtr already).
                    // Schedule a post-call MoveZero on the param slot so the
                    // exit drop's flag flips to false — this function no
                    // longer owns the pointee.
                    ctx.drops.mark_moved(local_id);
                    ctx.func_state.pending_move_zeros.push(local_id);
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
                    builder.local_type(place.local)
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
                    let local_type = builder.local_type(place.local);
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
                        // Schedule MoveZero AFTER the call — the callee reads from
                        // this address, so we can't zero before. Matches the
                        // Ownership::Move path at line ~212.
                        ctx.func_state.pending_move_zeros.push(place.local);
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
                // String constants need a Str-typed temp (32 bytes), not a Ptr-typed
                // temp (8 bytes), even when callee_param_type is Ptr(Str).
                let mat_type = if matches!(val, Operand::Constant(Constant::Str(_)))
                    && ctx.pointee_type(pt).map_or(false, |inner| ctx.type_mapper.is_string_type(inner))
                {
                    ctx.pointee_type(pt).unwrap_or(pt)
                } else {
                    pt
                };
                let tmp = builder.add_local(mat_type, None);
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
            // Fallback: materialize as GorgetString if the value is a string constant.
            if let Operand::Constant(Constant::Str(_)) = &val {
                let sv_type = ctx.type_mapper.owned_string_type;
                let tmp = builder.add_local(sv_type, None);
                builder.assign(Place::local(tmp), val);
                let ptr_type = ctx.register_ptr_type(sv_type);
                let dst = builder.add_local(ptr_type, None);
                builder.emit_borrow(dst, Place::local(tmp));
                return FunctionBuilder::copy(dst);
            }
            val // pass through for non-string constants
        }
        Ownership::Move if callee_is_move_param => {
            // If the operand is Ptr(T) (borrowed ref), auto-clone to create
            // an owned value before moving to the callee.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.local_type(place.local);
                    if let Some(inner) = ctx.pointee_type(local_type) {
                        if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
                            ctx.warn_implicit_clone(arg.span, inner, crate::ir::ImplicitCloneReason::MoveParamFromBorrow);
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
                    let local_type = builder.local_type(place.local);
                    // Pragmatic: skip Move for named string locals (! is no-op for strings).
                    // TODO: borrow checker should reject multi-use ! on strings.
                    let inner = ctx.pointee_type(local_type).unwrap_or(local_type);
                    if ctx.type_mapper.is_string_type(inner) && ctx.is_named_local(place.local) {
                        // Pass as const Ptr (borrow), no MoveZero
                        let ptr_type = ctx.register_ptr_type(local_type);
                        let dst = builder.add_local(ptr_type, None);
                        builder.emit_borrow(dst, place.clone());
                        return FunctionBuilder::copy(dst);
                    }
                    // CoW: move transfers ownership. Sever aliases first.
                    ctx.cow_before_mutation(builder, place.local, arg.span);
                    let ptr_type = ctx.register_mut_ptr_type(local_type);
                    let dst = builder.add_local(ptr_type, None);
                    builder.emit_borrow_mut(dst, place.clone());
                    // Mark the source as moved in the caller
                    ctx.drops.mark_moved(place.local);
                    // Schedule MoveZero AFTER the call — the callee reads from this
                    // address, so we can't zero before. The post-call MoveZero
                    // prevents the scope-exit drop from double-freeing.
                    ctx.func_state.pending_move_zeros.push(place.local);
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
        Ownership::Move => {
            // Refcounted types (Shared / Weak / Channel — Trivial copy semantics
            // but needing a drop at scope exit). `!x` at the call site means the
            // callee takes ownership of the refcount; the caller's slot must be
            // zeroed so its scope-exit drop doesn't fire a second time.
            // Without this, `drop_all(!s)` compiles to a plain by-value pass,
            // callee drops s, then caller ALSO drops s — heap-use-after-free
            // inside gorget_shared_drop the second time around.
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                if place.projections.is_empty() {
                    let local_type = builder.local_type(place.local);
                    if ctx.type_registry.needs_param_drop(local_type) {
                        ctx.drops.mark_moved(place.local);
                        ctx.func_state.pending_move_zeros.push(place.local);
                        return val;
                    }
                }
            }
            ctx.auto_clone_if_ptr(builder, val, arg.span)
        }
        _ => ctx.auto_clone_if_ptr(builder, val, arg.span), // Auto-clone Ptr(T) → T at boundary
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

/// Smart-pointer constructors (`Shared[Callable[T]](closure)`,
/// `Mutex[Callable[T]](closure)`, etc.) lower to a static-inline C wrapper
/// `XXX__T__new(T val)` that takes the inner type by value. When the inner
/// is a Callable family alias (`c_runtime_alias = "GorgetClosure"`), the GIR
/// arg is a `__Closure_N` env struct — but the C wrapper expects a packed
/// 16-byte `GorgetClosure` (fn_ptr + env_ptr).
///
/// The LIR's `try_closure_pack` (in `operands.rs`) already handles the
/// packing, but it fires only on `Assign` instructions where the destination
/// slot type is `GorgetClosure`. Direct `Call` arguments bypass it.
///
/// This helper bridges the two: it allocates an intermediate local typed as
/// the Callable alias (which lowers to `Struct(GorgetClosure)`), assigns the
/// closure into it (triggering `try_closure_pack`), and returns an operand
/// pointing at the now-packed local. The constructor then sees a proper
/// `GorgetClosure` value, identical to what `Box.new(closure)` synthesises
/// via its own special-case path at `methods.rs:78-84`.
///
/// Decision driven by typed metadata (`c_runtime_alias`), not by name —
/// per CLAUDE.md "no name matching" + "layering discipline §3 (one source
/// of truth per axis)". Same shape as `is_callable_alias_name` in
/// `methods.rs:2728` and `infer_drop_strategy` in `lir/lower/drops.rs:698`.
pub(super) fn pack_closure_for_smart_ptr_ctor(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    val_op: Operand,
    inner_c: &str,
) -> Operand {
    // Read the typed signal: is `inner_c` a Callable family alias of
    // GorgetClosure? `register_callable_alias` and the eager-walk path in
    // `types.rs:257` install `c_runtime_alias = "GorgetClosure"` on every
    // such TypeDef.
    let is_callable_alias = ctx.type_registry.get_type_def(inner_c)
        .and_then(|td| td.metadata.c_runtime_alias.as_deref())
        == Some("GorgetClosure");
    if !is_callable_alias { return val_op; }

    // Look up the alias TypeId. If the alias hasn't been registered yet
    // (e.g. the smart-pointer path bypassed `register_callable_inner_if_any`),
    // fall through — the C compile would fail anyway, but better not to
    // guess at a TypeId out of thin air.
    let alias_tid = match ctx.type_mapper.lookup_named(inner_c) {
        Some(tid) => tid,
        None => return val_op,
    };

    // Materialise into a typed temp. The slot's LIR type resolves to
    // `Struct(GorgetClosure)` (via the `c_runtime_alias` path in
    // `lir/lower/mod.rs:700+`), so the SlotStore here triggers
    // `try_closure_pack` which packs the env into a real GorgetClosure.
    let tmp = builder.add_local(alias_tid, None);
    builder.assign(Place::local(tmp), val_op);
    FunctionBuilder::copy(tmp)
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

        // len(x) free function → dispatch to the correct runtime function
        // based on the argument type (string, vector, dict, set).
        if name == "len" && args.len() == 1 {
            let recv = lower_expr(ctx, builder, &args[0].node.value);
            let recv_type = infer_operand_type_full(ctx, &recv, builder);
            let resolved = ctx.pointee_type(recv_type).unwrap_or(recv_type);
            let runtime_fn = if ctx.type_mapper.is_string_type(resolved) {
                "gorget_str_codepoint_count"
            } else if ctx.type_registry.is_collection_type(resolved) {
                // Read typed `collection_kind` (Phase A) — covers OrderedMap/Map
                // (Dict/HashMap), OrderedSet/Set (Set/HashSet), and Array
                // (default fall-through).
                use crate::ir::types::CollectionKind;
                match ctx.type_registry.collection_kind(resolved) {
                    Some(CollectionKind::OrderedMap) | Some(CollectionKind::Map) => "gorget_map_len",
                    Some(CollectionKind::OrderedSet) | Some(CollectionKind::Set) => "gorget_set_len",
                    _ => "gorget_array_len",
                }
            } else {
                // Check for user-defined Measurable.len() before falling back
                if let Some(crate::ir::types::GirType::Named(n)) = ctx.type_registry.get(resolved) {
                    let method_name = format!("{n}__len");
                    if ctx.fn_sigs.contains_key(&method_name) {
                        let ptr_type = ctx.register_ptr_type(resolved);
                        let borrow = match &recv {
                            Operand::Copy(p) | Operand::Move(p) => builder.borrow(p.clone(), ptr_type),
                            _ => {
                                let l = builder.add_local(resolved, None);
                                builder.assign(Place::local(l), recv.clone());
                                builder.borrow(Place::local(l), ptr_type)
                            }
                        };
                        let dst = builder.call(&method_name, vec![FunctionBuilder::copy(borrow)], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                }
                "gorget_array_len"
            };
            let dst = builder.call_extern(runtime_fn, vec![recv], I64_TYPE);
            return FunctionBuilder::copy(dst);
        }

        // Box(value) constructor → heap allocation via __gorget_box_alloc
        if (name == "Box" || name.starts_with("Box__")) && args.len() == 1 {
            let mut val_op = lower_expr(ctx, builder, &args[0].node.value);
            let raw_type = infer_operand_type_full(ctx, &val_op, builder);
            // Unwrap Ptr(T) → T: bare-borrowed resource params are passed by pointer.
            // Box should box the value, not the pointer.
            let val_type = match ctx.type_registry.get(raw_type) {
                Some(crate::ir::types::GirType::Ptr(inner)) | Some(crate::ir::types::GirType::MutPtr(inner)) => {
                    let pointee = *inner;
                    if let Operand::Copy(ref place) | Operand::Move(ref place) = val_op {
                        let derefed = builder.load_ref(place.clone(), pointee);
                        val_op = FunctionBuilder::copy(derefed);
                    }
                    pointee
                }
                _ => raw_type,
            };
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
            // Box() is a consuming position: the runtime's `*p = val` shallow-
            // copies the value into the heap, so resource-typed values whose
            // source is borrowed (bare param, non-last-use local, Ref, etc.)
            // would alias the box's interior with the caller's data —
            // identical pattern to push/put/insert. Apply the standard
            // consuming-arg ownership shim before the alloc call so a clone
            // is inserted when the source can't be moved. Without this, code
            // like `parse_function_type(named_ty, ...)` ends up with the
            // returned TFunction's box and the caller's named_ty sharing the
            // same Vector data — a use-after-free that, on this specific
            // shape, manifests as infinite Type__clone recursion.
            val_op = ctx.ensure_owned_at_consuming_arg(
                builder,
                val_op,
                &args[0].node.value,
                crate::ir::ImplicitCloneReason::ConsumingArg,
            );
            // Box takes ownership: after the alloc shallow-copies the value
            // into the heap, the source's slot still holds the same interior
            // pointers (Box children, String data, Vector handles). If we
            // only `unregister` from the drop tracker, the slot stays alive
            // for any subsequent INSTRUCTION-LEVEL drop — notably the
            // pre-rebind `drop x` that `lower_assign` emits when the
            // assignment target itself owns a resource. That drop frees
            // the interior pointers the new Box now owns, leaving the
            // freshly-allocated Box with dangling children. Zero the source
            // slot and mark it moved so both scope-exit drops AND
            // pre-rebind drops see it as already-dead.
            //
            // This matters specifically for the left-fold-into-self pattern
            //   `lhs = Node.Op(..., Box.new(!lhs), Box.new(!rhs))`
            // where `lhs` is being read for the `Box.new` AND rebound by the
            // surrounding assignment. Without the zero+mark, iteration 2's
            // pre-rebind drop frees iteration 1's heap-copied interior, and
            // iteration 3 segfaults reading dangling pointers from the box.
            let consumed_source: Option<LocalId> = match &val_op {
                Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                _ => None,
            };
            if let Some(src) = consumed_source {
                ctx.drops.unregister(src);
            }
            let dst = builder.call_extern(&alloc_fn, vec![val_op], box_type);
            // Tier 2a Phase 2A: Box allocation returns a fresh heap
            // allocation that doesn't alias any other slot. Tag the
            // result FreshOwned so the consume-site validator sees a
            // sound `(FreshOwned, dead, _)` tuple at the EnumInit /
            // StructInit consumer instead of `Untracked`. Mirrors
            // `call_extern_tracked` for the Box-alloc shape.
            if !ctx.drops.is_registered(dst) {
                ctx.drops.register_local(dst, box_type, &ctx.type_registry);
            }
            ctx.set_owned_fresh(builder, dst);
            if let Some(src) = consumed_source {
                ctx.move_zero_and_mark(builder, src);
            }
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
                let dst = ctx.call_extern_tracked(builder, fn_name, vec![arg_op], owned_type);
                return FunctionBuilder::copy(dst);
            } else if args.is_empty() {
                let owned_type = ctx.type_mapper.owned_string_type;
                let dst = ctx.call_extern_tracked(
                    builder,
                    "gorget_string_from_str",
                    vec![Operand::Constant(Constant::Str(String::new()))],
                    owned_type,
                );
                return FunctionBuilder::copy(dst);
            }
        }

        // format("...") → string interpolation or gorget_string_from_str
        if name == "format" && args.len() == 1 {
            if let Expr::StringLiteral(lit, interp_exprs) = &args[0].node.value.node {
                if lit.segments.iter().any(|s| matches!(s, StringSegment::Interpolation(_, _))) {
                    return lower_string_interpolation(ctx, builder, lit, interp_exprs);
                } else {
                    // Plain string literal → gorget_string_from_str(str_literal)
                    let str_op = lower_expr(ctx, builder, &args[0].node.value);
                    let owned_type = ctx.type_mapper.owned_string_type;
                    let dst = ctx.call_extern_tracked(builder, "gorget_string_from_str", vec![str_op], owned_type);
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
                    let chan_type = get_or_register_type(ctx, &mangled, None);
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
                    let vt = val_type;
                    let shared_type = get_or_register_type(ctx, &mangled, Some(&|c| ensure_shared_type_def(c, &mangled, vt)));
                    // Pack closure → GorgetClosure when the inner is a Callable
                    // alias. See `pack_closure_for_smart_ptr_ctor` for rationale.
                    let inner_c = mangled.strip_prefix("Shared__").unwrap_or("");
                    let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
                    let new_fn = format!("{mangled}__new");
                    let dst = builder.call(&new_fn, vec![val_op.clone()], shared_type);
                    // Shared[T](v) takes ownership of v's data. Mark Move-type locals
                    // as moved so the drop elaborator skips them (avoids dangling ptr).
                    if let Operand::Copy(place) = &val_op {
                        if place.projections.is_empty()
                            && is_resource_type_local(place.local, builder, &ctx.type_registry)
                        {
                            builder.move_zero(place.clone());
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
                    let vt = val_type;
                    let mutex_type = get_or_register_type(ctx, &mangled, Some(&|c| ensure_mutex_type_def(c, &mangled, vt)));
                    let inner_c = mangled.strip_prefix("Mutex__").unwrap_or("");
                    let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
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
                    let rw_type = get_or_register_type(ctx, &mangled, None);
                    let val_op = lower_expr(ctx, builder, &args[0].node.value);
                    let inner_c = mangled.strip_prefix("RWLock__").unwrap_or("");
                    let val_op = pack_closure_for_smart_ptr_ctor(ctx, builder, val_op, inner_c);
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
                let thread_type = get_or_register_type(ctx, &thread_name, None);
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
                    let coll_type = get_or_register_type(ctx, &mangled, None);
                    // Check for alloc= and cap= named arguments
                    let alloc_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "alloc")
                    });
                    let cap_arg = args.iter().find(|a| {
                        a.node.name.as_ref().map_or(false, |n| n.node == "cap")
                    });
                    let positional_args: Vec<&Spanned<ast::CallArg>> = args.iter()
                        .filter(|a| !a.node.name.as_ref().map_or(false, |n| n.node == "alloc" || n.node == "cap"))
                        .collect();

                    if positional_args.is_empty() {
                        let new_fn = format!("{mangled}__new");
                        // Tier 2a Phase 2A: collection constructors return
                        // a fresh heap allocation. Use `call_extern_tracked`
                        // which registers the result for drop AND tags
                        // ownership as Owned (consume-site validator
                        // accepts `(Owned, dead, _)` at downstream
                        // EnumInit/StructInit sites). Bumping to FreshOwned
                        // afterward signals the strictly-stronger
                        // "independent heap, no aliasing" axis.
                        if let Some(alloc_a) = alloc_arg {
                            let alloc_op = lower_expr(ctx, builder, &alloc_a.node.value);
                            builder.push_allocator(alloc_op);
                            let coll_local = ctx.call_extern_tracked(builder, &new_fn, vec![], coll_type);
                            ctx.set_owned_fresh(builder, coll_local);
                            builder.pop_allocator();
                            if let Some(cap_a) = cap_arg {
                                let cap_op = lower_expr(ctx, builder, &cap_a.node.value);
                                let ptr_type = ctx.type_registry.insert(crate::ir::types::GirType::MutPtr(coll_type));
                                let ptr = builder.borrow_mut(Place::local(coll_local), ptr_type);
                                let reserve_fn = format!("{mangled}__reserve");
                                builder.call_extern_void(&reserve_fn, vec![FunctionBuilder::copy(ptr), cap_op]);
                            }
                            return FunctionBuilder::copy(coll_local);
                        } else {
                            let coll_local = ctx.call_extern_tracked(builder, &new_fn, vec![], coll_type);
                            ctx.set_owned_fresh(builder, coll_local);
                            if let Some(cap_a) = cap_arg {
                                let cap_op = lower_expr(ctx, builder, &cap_a.node.value);
                                let ptr_type = ctx.type_registry.insert(crate::ir::types::GirType::MutPtr(coll_type));
                                let ptr = builder.borrow_mut(Place::local(coll_local), ptr_type);
                                let reserve_fn = format!("{mangled}__reserve");
                                builder.call_extern_void(&reserve_fn, vec![FunctionBuilder::copy(ptr), cap_op]);
                            }
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

        // Cross-module resolution: when the semantic resolver mapped this call site
        // to a specific module-qualified function, use the mangled name.  This prevents
        // bare-name collisions when multiple modules define the same function name
        // (e.g., `parse_float` in both std.conv and game.entity_parser).
        let effective_name = if let Some(resolved) = ctx.call_resolved_names.get(&callee.span.start) {
            resolved.clone()
        } else {
            effective_name
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
            let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
                .map(|a| Some(a.node.value.span))
                .collect();
            let ast_args: Vec<_> = args.iter().map(|a| a.node.value.clone()).collect();
            // Look up variant field types so that nested constructors like
            // `R2.A(Some(s))` see the expected `Option[GorgetString]` payload
            // type when lowering the inner `Some(s)`. Without this, `Some(s)`
            // infers from the operand type of `s` (a `*GorgetString` borrow)
            // and produces `Option[*GorgetString]` (Option__T<n>) — the
            // resulting struct is 16 bytes vs the variant's 40-byte slot,
            // and the memcpy into the variant payload reads past the source.
            let field_types: Vec<Option<TypeId>> = ctx.type_registry
                .get_type_def(&enum_name)
                .and_then(|td| match &td.kind {
                    crate::ir::types::TypeDefKind::Enum(ed) => Some(ed),
                    _ => None,
                })
                .and_then(|ed| ed.variants.iter().find(|v| v.name == variant_name))
                .map(|v| v.fields.iter().map(|f| Some(f.type_id)).collect())
                .unwrap_or_else(|| vec![None; args.len()]);
            let mut field_operands: Vec<Operand> = args.iter()
                .enumerate()
                .map(|(i, arg)| {
                    let prev = ctx.func_state.expected_type;
                    if let Some(ft) = field_types.get(i).and_then(|f| *f) {
                        ctx.func_state.expected_type = Some(ft);
                    }
                    let op = lower_expr(ctx, builder, &arg.node.value);
                    ctx.func_state.expected_type = prev;
                    op
                })
                .collect();
            // Clone multi-use resource args that can't be safely moved into the enum variant.
            super::clone_multi_use_resource_args(ctx, builder, &mut field_operands, &ast_args);
            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands, Some(arg_spans));
            return FunctionBuilder::copy(dst);
        }
        // Also check base name for non-generic enum variants
        if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
            let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
                .map(|a| Some(a.node.value.span))
                .collect();
            let ast_args: Vec<_> = args.iter().map(|a| a.node.value.clone()).collect();
            let field_types: Vec<Option<TypeId>> = ctx.type_registry
                .get_type_def(&enum_name)
                .and_then(|td| match &td.kind {
                    crate::ir::types::TypeDefKind::Enum(ed) => Some(ed),
                    _ => None,
                })
                .and_then(|ed| ed.variants.iter().find(|v| v.name == variant_name))
                .map(|v| v.fields.iter().map(|f| Some(f.type_id)).collect())
                .unwrap_or_else(|| vec![None; args.len()]);
            let mut field_operands: Vec<Operand> = args.iter()
                .enumerate()
                .map(|(i, arg)| {
                    let prev = ctx.func_state.expected_type;
                    if let Some(ft) = field_types.get(i).and_then(|f| *f) {
                        ctx.func_state.expected_type = Some(ft);
                    }
                    let op = lower_expr(ctx, builder, &arg.node.value);
                    ctx.func_state.expected_type = prev;
                    op
                })
                .collect();
            // Clone multi-use resource args for enum variant init.
            super::clone_multi_use_resource_args(ctx, builder, &mut field_operands, &ast_args);
            let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
            let dst = ctx.emit_enum_init_owned(builder, &enum_name, &variant_name, type_id, field_operands, Some(arg_spans));
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
                    // Route closure args through lower_call_arg for unified Ptr ABI
                    let sig_params = ctx.fn_sigs.get(call_fn.as_str()).map(|(p, _)| p.clone());
                    for (i, arg) in args.iter().enumerate() {
                        let param_type = sig_params.as_ref().and_then(|p| p.get(i + 1).copied());
                        call_args.push(lower_call_arg(ctx, builder, arg, param_type, &call_fn, i + 1));
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
                            if ctx.is_param_borrow_unique(builder, arg_local) {
                                call_args.push(FunctionBuilder::copy(arg_local));
                                continue;
                            }
                        }
                    }
                    let val = lower_expr(ctx, builder, &arg.node.value);
                    // Auto-deref Ptr(T) → T for non-resource value types. A closure
                    // declared `(Entity e): ...` expects an Entity by value, but
                    // the caller's local may hold a Ref[Entity] (from a collection
                    // `.get().unwrap()` or a `Ref[T]` field). Resource types stay
                    // as Ptr since their adapter expects the pointer form.
                    let val = ctx.auto_clone_if_ptr(builder, val, arg.span);
                    call_args.push(val);
                }
                let callable_name = format!("__callable_{}", local_id.0);
                // Look up tracked callable return type, fall back to I64_TYPE
                let ret_type = ctx.callable_return_type(local_id).unwrap_or(I64_TYPE);
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
        // Save pending_move_zeros baseline so we only drain entries added
        // by THIS call's argument lowering (not from nested/prior calls).
        let move_zero_baseline = ctx.func_state.pending_move_zeros.len();
        let mut lowered_args: Vec<Operand> = resolved_args
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let prev_expected = ctx.func_state.expected_type;
                let callee_pt = param_types.get(i).copied();
                if let Some(pt) = callee_pt {
                    ctx.func_state.expected_type = Some(pt);
                }
                let op = lower_call_arg(ctx, builder, arg, callee_pt, &effective_name, i);
                ctx.func_state.expected_type = prev_expected;
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

        // Suggest `!arg` for last-use resource-type arguments where the callee
        // clones the param at an ownership boundary. Both conditions must hold:
        // (1) the callee actually clones this param (recorded in fn_consumed_params)
        // (2) the caller's argument is the last use of a named resource-type local
        // Resolve the callee name through extern_bindings (same as the call emission path).
        let resolved_callee = ctx.extern_bindings.get(effective_name.as_str())
            .cloned()
            .unwrap_or_else(|| effective_name.clone());
        if let Some(consumed) = ctx.fn_consumed_params.get(resolved_callee.as_str()).cloned() {
            let param_names = ctx.fn_param_names.get(effective_name.as_str())
                .or_else(|| ctx.fn_param_names.get(resolved_callee.as_str()))
                .cloned();
            if let Some(param_names) = param_names {
                for (i, arg) in resolved_args.iter().enumerate() {
                    if matches!(arg.node.ownership, Ownership::Move) { continue; } // already !
                    if let Expr::Identifier(ref arg_name) = arg.node.value.node {
                        if let Some(pname) = param_names.get(i) {
                            if consumed.contains(pname) {
                                if let Some((local_id, _)) = ctx.lookup_local(arg_name) {
                                    let local_type = builder.local_type(local_id);
                                    let is_resource = ctx.type_registry.is_resource_type(local_type)
                                        || ctx.pointee_type(local_type)
                                            .map_or(false, |inner| ctx.type_registry.is_resource_type(inner));
                                    if is_resource && ctx.is_last_use_at(arg_name, arg.span) {
                                        let type_name = ctx.type_registry.type_name(local_type)
                                            .map(|n| crate::ir::lowering::context::demangle_type_name(&n))
                                            .unwrap_or_else(|| "resource".to_string());
                                        ctx.move_suggestions.push(crate::ir::MoveSuggestion {
                                            span: arg.node.value.span,
                                            name: arg_name.clone(),
                                            type_name,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
            *ret
        } else {
            I64_TYPE // fallback
        };

        // Resolve extern bindings: use the C symbol name instead of the Gorget name
        let call_name = if let Some(c_symbol) = ctx.extern_bindings.get(effective_name.as_str()) {
            c_symbol.clone()
        } else {
            effective_name
        };

        // Unregister GorgetString temps when the callee might store str views.

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
                        let ty = builder.local_type(place.local);
                        if ctx.type_registry.is_collection_type(ty) {
                            return Some(place.local);
                        }
                    }
                }
                None
            })
            .collect();

        // Upgrade consuming call args from Copy to Move (Rust-style ownership
        // on operand).  Enables generic LIR post-call zeroing.
        for arg in lowered_args.iter_mut() {
            if let Operand::Copy(place) = arg {
                if place.projections.is_empty() {
                    let dominated = move_zero_locals.iter().any(|mz| mz.local == place.local)
                        || collection_arg_locals.contains(&place.local);
                    if dominated {
                        *arg = Operand::Move(place.clone());
                    }
                }
            }
        }

        let result = if ret_type == UNIT_TYPE {
            builder.call_void(&call_name, lowered_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = ctx.call_tracked(builder, &call_name, lowered_args, ret_type);
            FunctionBuilder::copy(dst)
        };

        // MoveZero Move-ownership args.  The LIR's emit_post_call_zeros handles
        // args directly in lowered_args as Operand::Move; the GIR MoveZero
        // covers args wrapped in borrow ptrs.
        for place in &move_zero_locals {
            builder.move_zero(place.clone());
            ctx.drops.mark_moved(place.local);
        }

        // MoveZero collection temps passed as args.
        for local in &collection_arg_locals {
            ctx.move_zero_and_mark(builder, *local);
        }

        // MoveZero locals from Move-argument lowering (e.g., !expr.clone()).
        // These were borrowed (borrow_mut) for the callee; now that the call
        // has returned, zero the source to prevent double-free at scope exit.
        // Only drain entries added during THIS call's arg lowering.
        let pending: Vec<LocalId> = ctx.func_state.pending_move_zeros.drain(move_zero_baseline..).collect();
        for local in pending {
            builder.move_zero(Place::local(local));
            ctx.drops.mark_moved(local);
        }

        // `noreturn` extern calls (exit, abort, …) never return to the caller.
        // Terminate the basic block with `unreachable` so divergent uses
        // (e.g. an Error-arm `exit(2)` in a `T x = match …` expression) compose
        // with the surrounding result type — the match-expr lowerer's
        // `is_terminated()` check then correctly skips the arm-value assign.
        if ctx.noreturn_fns.contains(call_name.as_str()) {
            builder.unreachable();
        }

        result
    } else if let Expr::Closure { params, body, is_move, .. } = &callee.node {
        // IIFE: ((int x): x * x)(5) — inline closure called immediately
        let mut cl = std::mem::take(&mut ctx.closures);
        let closure_op = cl.lower_closure(ctx, builder, params, body, *is_move, callee.span);
        ctx.closures = cl;

        if let Operand::Copy(ref place) | Operand::Move(ref place) = closure_op {
            if place.projections.is_empty() {
                let closure_local = place.local;
                let closure_type_id = builder.local_type(closure_local);
                if let Some(type_name) = ctx.type_name_for_id(closure_type_id).map(|s| s.to_string()) {
                    if let Some((call_fn, _, _)) = ctx.lookup_closure_info(&type_name) {
                        let call_fn = call_fn.to_string();
                        // Build args: pointer to closure struct + call arguments
                        let ptr_type = ctx.type_registry.insert(GirType::Ptr(closure_type_id));
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow(ptr_local, Place::local(closure_local));
                        let mut call_args = vec![FunctionBuilder::copy(ptr_local)];
                        // Route IIFE args through lower_call_arg for unified Ptr ABI
                        let sig_params = ctx.fn_sigs.get(call_fn.as_str()).map(|(p, _)| p.clone());
                        for (i, arg) in args.iter().enumerate() {
                            let param_type = sig_params.as_ref().and_then(|p| p.get(i + 1).copied());
                            call_args.push(lower_call_arg(ctx, builder, arg, param_type, &call_fn, i + 1));
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
        // Non-identifier, non-closure callee — typically an expression that
        // produces a `Callable` value (e.g. `shared_callable.get()()`,
        // `make_adder(3)(5)`, `arr[0](x)`). Lower the callee to a value, then
        // dispatch via the LIR `__gorget_closure_call_N` shape — `insts.rs`
        // promotes that name to `Inst::CallClosure` regardless of whether the
        // GIR type is `FnPtr` or a Callable family alias (typed via
        // `c_runtime_alias = "GorgetClosure"`).
        let callee_op = lower_expr(ctx, builder, callee);
        let callee_local = match &callee_op {
            Operand::Copy(place) | Operand::Move(place) if place.projections.is_empty() => {
                place.local
            }
            _ => {
                // Materialise into a local so we have a stable ValueId to
                // pass through __gorget_closure_call_N.
                let ty = infer_operand_type_full(ctx, &callee_op, builder);
                let tmp = builder.add_local(ty, None);
                builder.assign(Place::local(tmp), callee_op);
                tmp
            }
        };
        let callee_type_id = builder.local_type(callee_local);

        // Resolve the return type. For a `FnPtr` GIR type, pick `return_type`
        // directly. For a Callable family alias (`Named("Callable__…")`), the
        // alias TypeDef doesn't carry the function signature, so fall back to
        // I64 — the C backend honours the call's `ret_ty` from the LIR
        // instruction, which we reconstruct from `builder.call`'s ret_type
        // argument. Without a recorded sig we can't be more precise here; the
        // typical `Callable[int(...)]` case lands on int64_t anyway.
        let ret_type = match ctx.type_registry.get(callee_type_id).cloned() {
            Some(GirType::FnPtr { return_type, .. }) => return_type,
            _ => I64_TYPE,
        };

        let mut call_args = vec![FunctionBuilder::copy(callee_local)];
        for arg in args {
            call_args.push(lower_expr(ctx, builder, &arg.node.value));
        }
        let callable_name = format!("__gorget_closure_call_{}", callee_local.0);
        if ret_type == UNIT_TYPE {
            builder.call_void(callable_name, call_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(callable_name, call_args, ret_type);
            FunctionBuilder::copy(dst)
        }
    }
}

/// Lower a `print(...)` call to a `printf` extern call.
///
/// Kwargs:
///  - `terminator: String` (default `"\n"`) — string appended after the
///    printed value. Use `""` to suppress the newline; `"\t"` or `", "`
///    for tabular / CSV-style output.
///  - `file: stderr` — route to stderr instead of stdout.
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

    // Check for named arguments: terminator="…", file=stderr
    let mut terminator: String = "\n".to_string();
    let mut use_stderr = false;
    for arg in args.iter().skip(1) {
        if let Some(ref name) = arg.node.name {
            match name.node.as_str() {
                "terminator" => {
                    // Accept a plain (non-interpolated) string literal — the
                    // terminator has to be known at compile time to splice
                    // into the printf format string. Empty string (""),
                    // single-segment literals, and escapes all flow through
                    // `as_plain_text`. Interpolation segments are silently
                    // dropped; a user passing `terminator=f"{x}"` would only
                    // see the literal chunks, but that's not a real use case.
                    if let Expr::StringLiteral(lit, _) = &arg.node.value.node {
                        let has_interp = lit.segments.iter().any(|s| matches!(s, StringSegment::Interpolation(_, _)));
                        if !has_interp {
                            terminator = lit.as_plain_text();
                        }
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
        Expr::StringLiteral(lit, interp_exprs) => {
            let mut format_str = String::new();
            let mut printf_args: Vec<Operand> = Vec::new();

            let mut interp_idx = 0usize;
            for segment in &lit.segments {
                match segment {
                    StringSegment::Literal(text) => {
                        format_str.push_str(text);
                    }
                    StringSegment::Interpolation(var_name, fmt_spec) => {
                        let pre_parsed = interp_exprs.get(interp_idx);
                        interp_idx += 1;
                        lower_interp_segment(ctx, builder, var_name, pre_parsed,
                            &mut format_str, &mut printf_args, fmt_spec.as_deref());
                    }
                }
            }

            format_str.push_str(&terminator);

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
            let fmt = format!("{spec}{terminator}");
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
/// `pre_parsed` is the parser-supplied AST for the segment text (populated for
/// every f-string segment so the expression participates in resolution and
/// typecheck/method-mangling rewrites). When `Some`, lowering uses it
/// directly; when `None` (constructed-during-lowering literals or parse
/// failures during early f-string sub-parse), falls back to re-parsing the
/// raw text — that path bypasses the rewriter and may emit un-mangled
/// symbols, but is preserved as a backstop so synthesised f-strings still
/// work.
/// `fmt_spec` is an optional format specifier like ".2f", "x", "08d", etc.
/// Pick an `AssignMode` for the f-string interp temp `tmp = lower(expr)`.
/// The temp is single-use (consumed by the format call). For resource
/// types, the right semantic is Move (transfer ownership from the
/// expression's owned result) when the source is a place, or Clone when
/// the source can't be moved. For non-resource types, Copy (bit-copy of
/// a primitive) is correct.
fn interp_temp_mode(ctx: &LoweringContext, val: &Operand, type_id: crate::ir::types::TypeId)
    -> crate::ir::instructions::AssignMode
{
    use crate::ir::instructions::AssignMode;
    if !ctx.type_registry.is_resource_type(type_id) {
        return AssignMode::Copy;
    }
    match val {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => AssignMode::Move,
        // Source has projections (field/index/deref) or is a constant/computation:
        // can't safely move. Clone gives us an owned independent copy.
        _ => AssignMode::Clone,
    }
}

pub(super) fn lower_interp_segment(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    pre_parsed: Option<&Spanned<Expr>>,
    format_str: &mut String,
    printf_args: &mut Vec<Operand>,
    fmt_spec: Option<&str>,
) {
    // 1. Try simple variable lookup first
    if let Some((local_id, type_id)) = ctx.lookup_local(var_name) {
        // If this is a pointer param, deref to get the value for formatting.
        // Covers &/! params (Borrowed/Unique) and borrowed resource params (ref_locals).
        let ptr_value_type = if ctx.is_param_borrow_unique(builder, local_id) || ctx.is_ref_local(builder, local_id) {
            ctx.pointee_type(builder.local_type(local_id))
        } else {
            None
        };
        if let Some(value_type) = ptr_value_type {
            let deref_place = Place {
                local: local_id,
                projections: vec![Projection::Deref],
            };
            // For resource-containing struct types (e.g. `struct { String name }`),
            // a plain deref+memcpy aliases the borrowed struct's interior resources
            // (String buffers, nested collections). Registering the resulting temp
            // for drop would double-free them. Use the type's clone function when
            // available so the temp owns independent resources.
            //
            // For primitives / Str / GorgetString — the existing Assign path is
            // already correct (C backend emits a deep clone for Ptr→String loads
            // and a by-value load for primitives).
            let needs_deep_clone = !ctx.type_mapper.is_string_type(value_type)
                && ctx.type_registry.is_resource_type(value_type);
            let tmp = if needs_deep_clone {
                if let Some(clone_fn) = ctx.clone_fn_for_ptr(value_type) {
                    ctx.call_tracked(builder, &clone_fn, vec![FunctionBuilder::copy(local_id)], value_type)
                } else {
                    let t = builder.add_local(value_type, None);
                    builder.assign(Place::local(t), Operand::Copy(deref_place));
                    ctx.drops.register_local(t, value_type, &ctx.type_registry);
                    t
                }
            } else {
                // String / primitive deref: emit the typed AssignMode for the
                // type. Strings need Clone (the SlotStore handler emits a deep
                // copy via gorget_string_copy_cow); primitives stay Copy
                // (bit-copy is correct). Phase C: explicit modes replace the
                // C-backend's "deep clone for Ptr→String loads" magic, so the
                // GIR layer carries the typed contract.
                use crate::ir::instructions::AssignMode;
                let mode = if ctx.type_mapper.is_string_type(value_type) {
                    AssignMode::Clone
                } else {
                    AssignMode::Copy
                };
                let t = builder.add_local(value_type, None);
                builder.assign_mode(mode, Place::local(t), Operand::Copy(deref_place));
                ctx.drops.register_local(t, value_type, &ctx.type_registry);
                t
            };
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

    // 2. Lower the parser-supplied Expr if available — it has been through
    //    resolution and typecheck so method calls dispatch to mangled symbols.
    if let Some(expr) = pre_parsed {
        let val = lower_expr(ctx, builder, expr);
        let type_id = infer_operand_type_full(ctx, &val, builder);
        let tmp = builder.add_local(type_id, None);
        let mode = interp_temp_mode(ctx, &val, type_id);
        // Tier 1b Move follow-through: when the temp is staged with Move,
        // the source's ownership transfers to the temp. If the source is
        // a drop-registered bare local, retire its drop registration with
        // `move_zero_and_mark` so the scope-exit drop doesn't double-free
        // the heap allocation that `tmp` (and the `format_for_printf`
        // expansion) now owns. Mirrors the snag #19 / #23 fixes
        // (commits `952b403f`, `4ebefe44`).
        let move_source: Option<LocalId> = if mode == AssignMode::Move {
            match &val {
                Operand::Copy(p) | Operand::Move(p)
                    if p.projections.is_empty() && ctx.drops.is_registered(p.local) =>
                    Some(p.local),
                _ => None,
            }
        } else { None };
        builder.assign_mode(mode, Place::local(tmp), val);
        if let Some(src) = move_source {
            ctx.move_zero_and_mark(builder, src);
        }
        let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(tmp), fmt_spec);
        format_str.push_str(&spec);
        printf_args.extend(args);
        return;
    }

    // 3. Backstop — re-parse the raw text. Reached only for synthesised f-strings
    //    constructed during lowering (no parse-time AST attached) or when the
    //    parser's sub-expression parse failed. Bypasses semantic passes; complex
    //    expressions here may produce un-mangled symbols.
    if let Ok(parsed_expr) = Parser::new(var_name).parse_expr() {
        let val = lower_expr(ctx, builder, &parsed_expr);
        let type_id = infer_operand_type_full(ctx, &val, builder);
        let tmp = builder.add_local(type_id, None);
        let mode = interp_temp_mode(ctx, &val, type_id);
        // Tier 1b Move follow-through: see branch (2) above.
        let move_source: Option<LocalId> = if mode == AssignMode::Move {
            match &val {
                Operand::Copy(p) | Operand::Move(p)
                    if p.projections.is_empty() && ctx.drops.is_registered(p.local) =>
                    Some(p.local),
                _ => None,
            }
        } else { None };
        builder.assign_mode(mode, Place::local(tmp), val);
        if let Some(src) = move_source {
            ctx.move_zero_and_mark(builder, src);
        }
        let (spec, args) = format_for_printf(ctx, builder, type_id, FunctionBuilder::copy(tmp), fmt_spec);
        format_str.push_str(&spec);
        printf_args.extend(args);
        return;
    }

    // 4. Last-resort — insert literal text
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

    if ctx.type_mapper.is_string_type(type_id) {
        // Str/GorgetString → %.*s with (int)expr.len, expr.data
        ("%.*s".to_string(), vec![operand])
    } else if ctx.pointee_type(type_id).map_or(false, |inner| ctx.type_mapper.is_string_type(inner)) {
        // Ptr(String) — dereference to get the String, then format as %.*s.
        // Assign from the pointer into a String-typed local so the Printf expansion
        // can extract .len and .data fields via SlotAddr + FieldPtr.
        let str_ty = ctx.type_mapper.owned_string_type;
        let tmp = builder.add_local(str_ty, None);
        builder.assign(builder.local(tmp), operand);
        ("%.*s".to_string(), vec![FunctionBuilder::copy(tmp)])
    } else if let Some(pointee) = ctx.pointee_type(type_id) {
        // Ptr(T) / MutPtr(T) for primitives or user types — auto-deref to the
        // pointee value. Covers user-written Ref[T]/MutRef[T] field loads and
        // field accesses on borrow-param receivers. Recurse so the pointee can
        // pick up its own formatting (narrow-int widening, Displayable, etc.).
        let deref_place = match &operand {
            Operand::Copy(place) | Operand::Move(place) => {
                let mut p = place.clone();
                p.projections.push(Projection::Deref);
                p
            }
            // Non-place operand (constant, literal) — can't add a Deref
            // projection; fall through to default int formatting.
            _ => return (
                ctx.type_mapper.format_specifier(type_id).to_string(),
                vec![operand],
            ),
        };
        let tmp = builder.add_local(pointee, None);
        builder.assign(Place::local(tmp), Operand::Copy(deref_place));
        return format_for_printf(ctx, builder, pointee, FunctionBuilder::copy(tmp), fmt_spec);
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
            let owned_string_type = ctx.type_mapper.owned_string_type;
            let result = builder.call(effective_method, vec![FunctionBuilder::copy(self_ptr)], owned_string_type);
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
    let is_str = ctx.type_mapper.is_string_type(type_id);

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
            let owned_string_type = ctx.type_mapper.owned_string_type;
            let alt_arg = Operand::Constant(Constant::I64(if alt { 1 } else { 0 }));
            let result = builder.call_extern(
                "gorget_int_to_binary",
                vec![op, alt_arg],
                owned_string_type,
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
