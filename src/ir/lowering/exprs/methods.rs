//! Method call lowering, collection method dispatch, and iterator adapter lowering.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Expr, Ownership};
use crate::span::Spanned;

use super::super::context::{LoweringContext, CollectionId, ParamABI};
use super::{lower_expr, lower_call_arg, maybe_auto_propagate, infer_operand_type_full, register_tuple_type,
            is_resource_type_local, get_or_register_type,
            ensure_box_type_def, ensure_guard_type_def, ensure_shared_type_def, ensure_weak_type_def,
            index_expr_to_mangle_fragment, try_resolve_field_place, try_resolve_index_element_ptr,
            extract_field_path_string,
            resolve_projection_root_local, expr_projection_contains_index};
use super::shared::{guard_of, emit_guard_get_ptr};

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

/// Resolve named arguments and fill default parameter values for an
/// equip-METHOD call. Mirrors the free-function `resolve_call_args`
/// (`calls.rs`), offset by the implicit `self`.
///
/// `ctx.fn_param_names` / `ctx.fn_defaults` for an equip method are keyed by
/// the mangled `Type__method` name and SURFACE-indexed with `self` at idx 0
/// (the parser injects a synthetic self param). The `args` passed here are the
/// NON-self surface args, so method-arg position `N` ↔ surface param `N + 1`.
/// We build a non-self slot array (length = #non-self params), place
/// positional/named args into it, fill any empty slot that has a default, and
/// flatten back to a positional arg list.
fn resolve_method_call_args(
    ctx: &LoweringContext,
    mangled_name: &str,
    args: &[Spanned<ast::CallArg>],
) -> Vec<Spanned<ast::CallArg>> {
    // Surface param names INCLUDE self at idx 0 for instance methods. The
    // non-self params (which the method-call args correspond to) are
    // `param_names[1..]`. A static equip method has no `self`, so its
    // FunctionInfo already excludes it — don't strip in that case.
    let param_names = match ctx.fn_param_names.get(mangled_name) {
        Some(names) if !names.is_empty() => names,
        _ => return args.to_vec(), // no param info → pass through unchanged
    };
    let strip_self = param_names.first().map(|n| n == "self").unwrap_or(false);
    let non_self_param_names: &[String] = if strip_self { &param_names[1..] } else { &param_names[..] };

    let has_named = args.iter().any(|a| a.node.name.is_some());
    let has_defaults = ctx.fn_defaults.contains_key(mangled_name);

    if !has_named && !has_defaults {
        return args.to_vec();
    }
    if !has_named && args.len() >= non_self_param_names.len() {
        return args.to_vec(); // all params supplied positionally, no reorder needed
    }

    // Build a slot array matching non-self parameter order.
    let mut slots: Vec<Option<Spanned<ast::CallArg>>> = vec![None; non_self_param_names.len()];

    let mut positional_idx = 0;
    for arg in args {
        if let Some(name) = arg.node.name.as_ref() {
            if let Some(pos) = non_self_param_names.iter().position(|p| p == &name.node) {
                slots[pos] = Some(arg.clone());
            }
        } else {
            while positional_idx < slots.len() && slots[positional_idx].is_some() {
                positional_idx += 1;
            }
            if positional_idx < slots.len() {
                slots[positional_idx] = Some(arg.clone());
                positional_idx += 1;
            }
        }
    }

    // Fill defaults for any remaining empty slot. `fn_defaults` is surface-
    // indexed, matching `fn_param_names`: a default at surface index `param_idx`
    // fills non-self slot `param_idx - 1` when `self` is present, else `param_idx`.
    let self_offset: usize = if strip_self { 1 } else { 0 };
    if let Some(defaults) = ctx.fn_defaults.get(mangled_name) {
        for (param_idx, default_expr) in defaults {
            if *param_idx < self_offset { continue; } // the self slot has no default
            let slot = *param_idx - self_offset;
            if slot < slots.len() && slots[slot].is_none() {
                slots[slot] = Some(Spanned::dummy(ast::CallArg {
                    name: None,
                    ownership: ast::Ownership::Borrow,
                    value: Spanned::dummy(default_expr.clone()),
                }));
            }
        }
    }

    slots.into_iter().flatten().collect()
}

/// Build the `Ptr(enum)` argument for an Option/Result builtin extern
/// (`__option_is_some`, `__option_unwrap`, `__result_unwrap_error`, …).
///
/// The enum builtins take the Option/Result BY POINTER. The default path
/// `emit_borrow`s the receiver place to take its address — this is correct
/// for value receivers AND for `Field`-origin field-borrows (`&w.name`),
/// whose underlying struct is owned and WILL drop, so the unwrap must
/// invalidate the source.
///
/// THE CARVE-OUT: a for-loop element bound as a *collection-element borrow
/// alias* by `lower_for_array` (origin `CowBorrowPending` /
/// `CollectionElement`, i.e. `is_cow_borrow`) is a `Ptr(enum)` aimed into a
/// collection THIS scope does not own. Two things must change for it:
///   1. Pass the pointer THROUGH (it already IS the `Ptr(enum)` arg) rather
///      than re-borrowing to `Ptr(Ptr(enum))`.
///   2. NEVER invalidate / MoveZero the source — the collection owns the
///      element; zeroing it would corrupt the collection.
/// The returned `is_collection_borrow` flag tells the unwrap callers to skip
/// the `Move` signal and the `move_zero_and_mark`.
///
/// `Field`-origin borrows are deliberately EXCLUDED from the carve-out so
/// the long-standing `w.name.unwrap()` (struct-field-Option) path keeps its
/// source-invalidating behavior — that's load-bearing for the Snag #25d
/// double-free guard (`test_option_resource_field`).
fn build_enum_recv_ptr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    recv: &Operand,
    place: &Place,
) -> (Operand, bool) {
    let recv_type = infer_operand_type_full(ctx, recv, builder);
    let recv_is_ptr = matches!(
        ctx.type_registry.get(recv_type),
        Some(GirType::Ptr(_) | GirType::MutPtr(_))
    );
    // Only the genuine collection-element borrow alias (for-loop element)
    // gets the pass-through + no-invalidate treatment. A bare-local Ptr that
    // is a collection borrow has projections=[] and is_cow_borrow=true.
    let is_collection_borrow = recv_is_ptr
        && place.projections.is_empty()
        && ctx.is_cow_borrow(builder, place.local);
    if is_collection_borrow {
        // Receiver slot already holds the enum pointer — pass it directly.
        (FunctionBuilder::copy(place.local), true)
    } else {
        let ptr_type = ctx.register_ptr_type(recv_type);
        let borrow = builder.add_local(ptr_type, None);
        builder.emit_borrow(borrow, place.clone());
        (FunctionBuilder::copy(borrow), false)
    }
}

pub(super) fn lower_method_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    receiver: &Spanned<Expr>,
    method_name: &str,
    method_span_start: usize,
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
        // Snag #29: in qualified-path position (`E.method(...)`), prefer the
        // TYPE interpretation when `name` resolves to a registered type or
        // enum-variant — but locals still take priority over types (Rust-
        // style: `let MyType = ...; MyType.x()` calls method on the local).
        // The parallel `module_constants` lookup was masking user-defined
        // types that collide with stdlib constants — e.g., `enum E` (user)
        // vs `const float E` (math.gg, hardcoded into every module's
        // constants at `mod.rs:486-493`). The collision turned `E.A` into
        // a method call on the float `e = 2.718…`, generating
        // `call @double__A()` and assigning the void result to an `__gg_E`
        // struct slot — type mismatch in C codegen. Type-position priority
        // matches Rust's separate type/value namespace convention.
        let is_type_name = matches!(name.as_str(),
                "int" | "float" | "bool" | "uint8" | "uint16" | "uint32" | "uint64"
                | "int8" | "int16" | "int32" | "str" | "String" | "char" | "byte")
            || ctx.type_mapper.lookup_named(name).is_some()
            || ctx.resolve_enum_variant(name).is_some();
        let is_local = ctx.lookup_local(name).is_some();
        if !is_local && (is_type_name || !ctx.module_constants.contains_key(name)) {
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

                // Box.new(value) is a consuming position (like Vector.push,
                // enum variant init, and the bare-name `Box(value)` ctor).
                // Apply the standard consuming-arg ownership shim, then
                // unregister the source from drops — the box's heap region
                // shallow-copies `val` and now owns the bytes; any scope-exit
                // drop on the source would double-free with the box's own
                // recursive drop chain (case-c of TODO Box[T] item).
                //
                // This mirrors the bare-name `Box(value)` ctor at
                // `src/ir/lowering/exprs/calls.rs:419-430` exactly. Both
                // entry points should produce identical IR for the same
                // semantic operation.
                val = ctx.ensure_owned_at_consuming_arg(
                    builder,
                    val,
                    &args[0].node.value,
                    crate::ir::ImplicitCloneReason::ConsumingArg,
                );
                // Box takes ownership: after the alloc shallow-copies the
                // value into the heap, the source slot still holds the
                // same interior pointers (Box children, String data,
                // Vector handles). `unregister` alone only removes the
                // scope-exit drop entry — an INSTRUCTION-LEVEL pre-rebind
                // drop (the `drop x` that lower_assign emits when `x` is
                // being reassigned to a resource type) still fires on the
                // just-consumed slot and frees the box's interior.
                //
                // Repro: snag #23 follow-up segfault at iteration 3 of
                //   while … : lhs = Node.Op(…, Box.new(!lhs), Box.new(!rhs))
                // Iteration 1 free is harmless (lhs is a Lit with no
                // interior). Iteration 2 free of lhs (now a Node.Op
                // holding two Boxes) cuts off iteration 1's box-tree.
                // Iteration 3 reads dangling pointers → SEGV.
                //
                // The fix mirrors the bare-name `Box(value)` ctor in
                // `src/ir/lowering/exprs/calls.rs`: zero the source slot
                // and mark it moved so both scope-exit AND pre-rebind
                // drops see it as already-dead.
                let consumed_source: Option<LocalId> = match &val {
                    Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => Some(p.local),
                    _ => None,
                };
                if let Some(src) = consumed_source {
                    ctx.drops.unregister(src);
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
                // Tier 2c (snag #23 class): register this alloc fn as a
                // shallow-copy heap-allocating consumer so
                // `validate_drop_pre_rebind` recognises it via typed
                // metadata rather than name matching. See
                // `Module::heap_alloc_consumer_externs`.
                ctx.heap_alloc_consumer_externs.insert(alloc_fn.clone());
                let dst = builder.call(alloc_fn, vec![val], box_type);
                if let Some(src) = consumed_source {
                    ctx.move_zero_and_mark(builder, src);
                }
                return FunctionBuilder::copy(dst);
            }

            // Check if this is a known type name (including primitives like int, float, bool)
            let is_primitive_type = matches!(name.as_str(), "int" | "float" | "bool" | "uint8" | "uint16" | "uint32" | "uint64"
                | "int8" | "int16" | "int32" | "str" | "String" | "char" | "byte");
            if is_primitive_type || ctx.type_mapper.lookup_named(name).is_some() || ctx.resolve_enum_variant(name).is_some() {
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
                // For qualified enum variant constructors (Color.Red(), R2.A(...)),
                // pre-resolve the variant's field types so each arg sees the
                // correct expected_type during lowering. Without this, a nested
                // constructor like `Some(s)` inside `R2.A(Some(s))` infers its
                // type from the operand (a *GorgetString borrow) and produces
                // `Option[*GorgetString]` (Option__T<n>) — a 16-byte struct
                // instead of the variant slot's 40-byte `Option__GorgetString`.
                let variant_field_types: Vec<Option<TypeId>> = ctx.type_registry
                    .get_type_def(c_type_name)
                    .and_then(|td| match &td.kind {
                        TypeDefKind::Enum(ed) => Some(ed),
                        _ => None,
                    })
                    .and_then(|ed| ed.variants.iter().find(|v| v.name == method_name))
                    .map(|v| v.fields.iter().map(|f| Some(f.type_id)).collect())
                    .unwrap_or_else(|| vec![None; args.len()]);
                // STATIC equip-method default-fill / named-arg reorder. A
                // STATIC method (no `self`) reached via `Type.method(...)`
                // dispatches HERE rather than through the instance path's
                // `resolve_method_call_args`, so without this a `Maker.make(5)`
                // call to `Maker make(int a, int b = 7)` would lower as "too
                // few arguments to function 'Maker__make'". `fn_defaults` /
                // `fn_param_names` are keyed by `equip_target_name` (`mod.rs`),
                // which is the C-MANGLED name for PRIMITIVE equips (`int64_t`,
                // not `int`) and the surface name for named/struct types — i.e.
                // `c_type_name`, the SAME mangling the emitted call uses below
                // (`{c_type_name}__{method_name}`). Keying on the surface `name`
                // here missed primitive-equip statics (`int.combine(5)` → fill
                // skipped → broken C). They carry NO self slot for a static
                // method, so the helper fills/reorders correctly (`strip_self`
                // is false). Enum-variant ctors carry no fn_defaults entry → no-op.
                let static_defaults_key = format!("{c_type_name}__{method_name}");
                let filled_static_args = resolve_method_call_args(ctx, &static_defaults_key, args);
                let args: &[Spanned<ast::CallArg>] = &filled_static_args;
                let variant_field_types: Vec<Option<TypeId>> = if variant_field_types.len() == args.len() {
                    variant_field_types
                } else {
                    // Default-fill widened the arg list; pad field-type hints
                    // (variants don't get here with defaults, so None is fine).
                    let mut v = variant_field_types;
                    v.resize(args.len(), None);
                    v
                };
                let lowered_args: Vec<Operand> = args.iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        let prev = ctx.func_state.expected_type;
                        if let Some(ft) = variant_field_types.get(i).and_then(|f| *f) {
                            ctx.func_state.expected_type = Some(ft);
                        }
                        let op = lower_expr(ctx, builder, &arg.node.value);
                        // Snag #46: a throws-fn call result here is `Result[T, E]`,
                        // but the variant field expects `T`. Mirror `lower_call_arg`
                        // (and `calls.rs:151`) — auto-propagate Result → T at the
                        // boundary so the variant slot receives the unwrapped value
                        // rather than a memcpy of the Result struct (which read as
                        // the type's zero-init default).
                        let op = maybe_auto_propagate(ctx, builder, op, arg.node.value.span);
                        ctx.func_state.expected_type = prev;
                        op
                    })
                    .collect();
                // Check if this is a qualified enum variant constructor: Color.Red()
                if let Some(type_def) = ctx.type_registry.get_type_def(c_type_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if e.variants.iter().any(|v| v.name == method_name) {
                            let arg_spans: Vec<Option<crate::span::Span>> = args.iter()
                                .map(|a| Some(a.node.value.span))
                                .collect();
                            let mut lowered_args = lowered_args;
                            let ast_args: Vec<_> = args.iter().map(|a| a.node.value.clone()).collect();
                            super::clone_multi_use_resource_args(ctx, builder, &mut lowered_args, &ast_args);
                            let type_id = ctx.type_mapper.lookup_named(name).unwrap_or(UNIT_TYPE);
                            let dst = ctx.emit_enum_init_owned(builder, name, method_name, type_id, lowered_args, Some(arg_spans));
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

    // `field_place_info` (the in-place field-borrow for `self.values.push(x)`)
    // is computed AFTER the CoW mutation blocks below — a projected-root
    // materialize (`h.nums.push()` on a bare `h`) rebinds `h`'s name to a fresh
    // owned local, and the field borrow must re-resolve against THAT rebound
    // local (mirrors stmts/assigns.rs:lower_field_assign, which calls
    // try_resolve_field_place after cow_before_field_mutation). See the
    // relocated computation just before `field_is_borrow_ptr`.

    // Extract field path string for CowBorrow provenance on field-access receivers.
    let field_path_for_cow: Option<String> = extract_field_path_string(&receiver.node);

    // CoW UAF fix (round-33, class fix — the 3rd G1 root-materialize site, after
    // lower_field_assign / lower_index_assign): snapshot the local range for the
    // WHOLE method-call statement (receiver chain + args). If ANY of the three
    // receiver-root materialize blocks below actually rebinds the root into a
    // private owned copy — the projected-root block (`v[0].method()`), the
    // bare-param NAMED-receiver block (`v.push(v[0])`), or the index-source block
    // — an ARG that is an element of the SAME collection (`v[0].set_from(v[1])`,
    // `m[0].push(m[1][0])`, `v.push(v[0])`) mints a transient CollectionElement/
    // FieldPath ref into that copy; it dangles on a later same-collection push
    // (cow_before_mutation Case 3 clones freed memory → heap-UAF). Each of those
    // blocks sets `did_g1_materialize` on its `before != after` rebind; the
    // untrack runs at the END (after `ensure_owned_at_consuming_arg` has cloned
    // the consumed args), guarded by that flag so NON-materializing / `&`-correct
    // method calls stay byte-identical. Mirrors the assign gate.
    let stmt_locals_start = builder.locals.len();
    let mut did_g1_materialize = false;

    // For pointer params used as method receivers, pass the raw pointer directly.
    // Auto-deref would copy the struct, and mutations to the copy wouldn't propagate back.
    let borrow_param_local = if let Expr::Identifier(name) = &receiver.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            if ctx.is_ref_local(builder, local_id)
                || ctx.is_param_borrow_unique(builder, local_id)
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

    // ─── D36 auto-deref for method-call receivers ────────────────────
    // TRACK E2 SCOUT PROTOTYPE — when the typechecker resolved the method
    // through the wrapper's INNER type (docs §9.3/§9.4), project the receiver
    // through `emit_guard_get_ptr` (GuardAccept) or the inner cast (DerefTarget,
    // Box), then let the normal dispatch flow re-mangle from the INNER type
    // name. `guard_of` peels `Ptr`/`MutPtr` from a `&`/`!` param so the pattern
    // is uniform.
    if let Some(wrapper_kind) = ctx.analysis.method_resolutions.get(&method_span_start).and_then(|r| r.auto_deref) {
        use crate::semantic::scope::DerefWrapperKind;
        match wrapper_kind {
            DerefWrapperKind::GuardAccept => {
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                if let Some(info) = guard_of(ctx, recv_type) {
                    // Extract the place; for a projected recv we materialise
                    // first (matches the guard field-access read path pattern).
                    let guard_place = match &recv {
                        Operand::Copy(p) | Operand::Move(p) => p.clone(),
                        _ => {
                            let tmp = builder.add_local(recv_type, None);
                            builder.assign(Place::local(tmp), recv.clone());
                            Place::local(tmp)
                        }
                    };
                    let (inner_ptr_local, _inner_type) =
                        emit_guard_get_ptr(ctx, builder, &guard_place, &info);
                    // Replace recv with the inner-pointer local. Downstream
                    // dispatch reads `type_name` from the inner-pointer's
                    // pointee via `infer_type_name_from_operand_full`, mangles
                    // `Inner__method`, and dispatches through the equipped
                    // method's fn_sigs entry.
                    recv = Operand::Copy(Place::local(inner_ptr_local));
                }
            }
            DerefWrapperKind::DerefTarget => {
                // Box[T] auto-deref (D36): project through `Box__T__get_ptr`
                // — mirrors the Guard branch. `emit_box_wrapper` emits the
                // helper; the C emitter's call-name scan pulls it in from
                // the emitted call automatically.
                let recv_type = infer_operand_type_full(ctx, &recv, builder);
                let box_type_name: Option<String> = ctx
                    .type_name_for_id(recv_type)
                    .map(|s| s.to_string())
                    .or_else(|| {
                        // If recv is a pointer to Box (e.g. `&`/`!` param),
                        // peel one Ptr layer to find the Box name.
                        ctx.pointee_type(recv_type)
                            .and_then(|inner| ctx.type_name_for_id(inner))
                            .map(|s| s.to_string())
                    });
                if let Some(box_name) = box_type_name {
                    if ctx.type_registry.is_box_name(&box_name) {
                        let inner_suffix = &box_name["Box__".len()..];
                        let inner_type = ctx.type_mapper
                            .lookup_named(inner_suffix)
                            .unwrap_or(I64_TYPE);
                        // Ensure recv is a place we can pass by value.
                        let _place = match &recv {
                            Operand::Copy(p) | Operand::Move(p) => p.clone(),
                            _ => {
                                let box_ty = ctx.type_mapper
                                    .lookup_named(&box_name)
                                    .unwrap_or(I64_TYPE);
                                let tmp = builder.add_local(box_ty, None);
                                builder.assign(Place::local(tmp), recv.clone());
                                Place::local(tmp)
                            }
                        };
                        // If recv is currently a pointer-to-Box (`&`/`!` param),
                        // load the box handle first so the helper sees the
                        // Box (which is itself `void*`).
                        let box_operand = if ctx.pointee_type(recv_type).is_some() {
                            // recv is Ptr(Box) — Box__T__get_ptr expects Box
                            // (a `void*`) by value, so dereference through
                            // the pointer. LIR: load through the ptr local.
                            recv.clone()
                        } else {
                            recv.clone()
                        };
                        let inner_ptr_type = ctx.register_mut_ptr_type(inner_type);
                        let get_ptr_fn = format!("{box_name}__get_ptr");
                        let inner_ptr_local = builder.call(
                            &get_ptr_fn,
                            vec![box_operand],
                            inner_ptr_type,
                        );
                        recv = Operand::Copy(Place::local(inner_ptr_local));
                    }
                }
            }
            DerefWrapperKind::NonDerefContainer => {}
        }
    }

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
        // Value-routed fallback: a collection-sourced Task[void] whose static
        // TypeId maps to >1 DISTINCT producer fn (so no single
        // __gorget_await_<fn> name is resolvable). For a VOID task,
        // await == join+destroy+free == the per-instance __drop the value
        // already carries, so we dispatch through the value via
        // Task__void__await (per-value provenance, NOT name matching).
        // The named path's `recv_local` is scoped inside the resolved block
        // above, so RE-EXTRACT the receiver local here (mirror of :447-451).
        let recv_local = match &recv {
            Operand::Copy(place) | Operand::Move(place)
                if place.projections.is_empty() => Some(place.local),
            _ => None,
        };
        if let Some(local_id) = recv_local {
            let tid = builder.local_type(local_id);
            // Equality against the registered type name the producer wrote at
            // spawn (read through the typed accessor) — the documented
            // C-emit-symbol-boundary idiom (docs/devbook/24 exception), NOT a
            // substring/prefix heuristic. There is no typed "is Task[void]"
            // discriminator (Task is a plain Named struct-def).
            if ctx.type_name_for_id(tid) == Some("Task__void") {
                builder.call_void("Task__void__await", vec![recv]);
                // Zero the slot so scope-exit Task__void__drop (null-guarded)
                // is a no-op — prevents double-join. See emit_types.rs.
                ctx.move_zero_and_mark(builder, local_id);
                return Operand::Constant(Constant::Unit);
            }
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

    // .unwrap() / .expect() / .unwrap_or() on Option/Result → inline extraction.
    //
    // The type checker (`is_option_or_result_receiver`, Brief A Phase 1) now
    // REJECTS these methods on a non-Option/Result *source* expression at
    // `gg check`, so the old blanket `if !is_option_or_result { return recv }`
    // no-op (which masked the real bug — unwrap on an `int`/struct silently
    // returned the receiver) is gone. What REMAINS is a narrower, legitimate
    // no-op: when the LOWERED receiver operand is no longer an Option/Result
    // mangled type, the value was already destructured upstream (e.g. the GIR
    // lowering of `parse_float(val)` produces a bare `double` temp, so
    // `parse_float(val).unwrap()` arrives with a `double` receiver). In that
    // case `unwrap` is a genuine no-op on an already-extracted payload — return
    // it unchanged rather than falling through to the `I64_TYPE` mis-typing
    // branch. This is keyed off the lowered TYPE, not the source — the checker
    // gate already guaranteed the source was optional.
    if matches!(method_name, "unwrap" | "expect" | "unwrap_or") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        // For Option/Result, extract the inner value via extern call that C backend handles
        if let Some(ref tn) = type_name {
            // Read typed `enum_category` (Phase A) for Option vs Result
            // discrimination. The downstream inner-name slicing
            // (`Option__T`/`Result__Ok__Err` → T or Ok) is the C-mangling
            // boundary and stays — only the discriminator is migrated.
            //
            // FALLBACK on the mangled name prefix when `enum_category` is
            // absent: some Result instantiations with a USER error enum (e.g.
            // `Result[float, ParseError]`) reach here without their
            // `enum_category` registered (a separate upstream gap — see
            // TODO.md "Result enum_category not set for user-error-typed
            // Results"). Before Brief A's checker gate the silent no-op masked
            // this; now that `unwrap` is guaranteed-Option/Result by the
            // checker, we must NOT fall through to the `I64_TYPE` garbage
            // branch — that mis-typed the Ok payload (a `double` got read as
            // `int64_t` + bogus `+8` ptr arithmetic). The name prefix is the
            // same C-mangling boundary the inner-type slicing already trusts.
            use crate::ir::types::EnumCategory;
            let cat = ctx.type_registry.get_type_def(tn)
                .and_then(|td| td.metadata.enum_category)
                .or_else(|| {
                    // Fall back on the mangled name prefix when `enum_category`
                    // is absent — covers Result instantiations with a USER error
                    // enum (e.g. `Result[float, ParseError]`) that reach here
                    // without their category registered (a separate upstream gap).
                    if tn.starts_with("Option__") {
                        Some(EnumCategory::Option)
                    } else if tn.starts_with("Result__") {
                        Some(EnumCategory::Result)
                    } else {
                        None
                    }
                });
            // If the lowered receiver is NOT an Option/Result mangled type, the
            // Result/Option was ALREADY destructured upstream (e.g. the GIR
            // lowering of `parse_float(val)` yields a bare `double` temp, so
            // `parse_float(val).unwrap()` arrives here with a `double` receiver).
            // `unwrap` on an already-extracted payload is a genuine no-op —
            // return it unchanged. The Brief A checker gate guarantees the
            // SOURCE expression was Option/Result, so this is never the
            // unwrap-on-non-optional bug (that's rejected at `gg check`); it is
            // only the already-unwrapped fast path. Do NOT fall through to the
            // `I64_TYPE` branch below, which would mis-type the payload.
            if cat.is_none() {
                return recv;
            }
            let is_result = cat == Some(EnumCategory::Result);
            let inner_type = if cat == Some(EnumCategory::Option) {
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
                // Ptr-receiver guard: a borrowed enum element (for-loop
                // collection-element borrow alias) is already `Ptr(enum)` — pass
                // it through and skip source-invalidation (the collection owns
                // it). All other receivers (values, `&self` params, field
                // borrows) keep the original `emit_borrow` + invalidate path.
                let (borrow_arg, recv_is_collection_borrow) = build_enum_recv_ptr(ctx, builder, &recv, place);

                // Decide once at the GIR layer: resource payloads require the
                // source Option/Result to be invalidated after extraction to
                // prevent a double-free when the source is later dropped.
                // Signal this to the LIR by passing the borrow as Operand::Move;
                // the LIR __option_unwrap special-case reads the operand kind
                // instead of re-deriving the fact from the drop registry.
                //
                // EXCEPTION: when the receiver is a Ptr borrow alias (the data
                // is owned by the collection / caller, not this slot), NEVER
                // signal Move — invalidating the source would zero a collection
                // element. Pass Copy and skip the MoveZero/unregister below.
                let inner_is_resource = ctx.type_registry.is_resource_type(inner_type);
                let borrow_op = if inner_is_resource && !recv_is_collection_borrow {
                    if let Operand::Copy(p) = &borrow_arg { FunctionBuilder::mov(p.local) } else { borrow_arg.clone() }
                } else {
                    borrow_arg.clone()
                };

                if method_name == "unwrap_or" {
                    // unwrap_or(default) → (tag == 0) ? data.Variant._0 : default
                    let mut default_val = if !args.is_empty() {
                        lower_expr(ctx, builder, &args[0].node.value)
                    } else {
                        Operand::Constant(Constant::I64(0))
                    };
                    // Option__Ref__T parity: when the Option's payload is `Ptr(T)`
                    // (collection borrow), the user-written default is the bare
                    // pointee `T`. The LIR __option_unwrap_or stores both
                    // payload and default into the same result slot — typed by
                    // the struct field, which is `Ptr(T)` — so the bare-`T`
                    // default would byte-clash with a `Ptr(T)` slot.
                    //
                    // Spill the default to a fresh slot and pass its address as
                    // the default. Both branches now flow `Ptr(T)`. The result
                    // is a `Ptr(T)` borrow that downstream sites (var-decl
                    // eager-clone via `clone_fn_for_ptr`, return-value
                    // auto-deref) handle the same way as bare `unwrap()`.
                    if matches!(ctx.type_registry.get(inner_type), Some(GirType::Ptr(pointee)) if !matches!(default_val, Operand::Constant(Constant::Unit))
                        && {
                            let _ = pointee;
                            true
                        }
                    ) {
                        if let Some(GirType::Ptr(pointee)) = ctx.type_registry.get(inner_type).cloned() {
                            let dv_type = infer_operand_type_full(ctx, &default_val, builder);
                            // Only spill if the default isn't already a Ptr<pointee>.
                            // (Some callsites — e.g. `unwrap_or(other_get_call)` —
                            // already produce a Ptr<T>.)
                            let already_ptr = matches!(ctx.type_registry.get(dv_type), Some(GirType::Ptr(_)));
                            if !already_ptr {
                                let tmp_slot = builder.add_local(pointee, None);
                                builder.assign(Place::local(tmp_slot), default_val);
                                let ptr_t = ctx.register_ptr_type(pointee);
                                let borrow = builder.add_local(ptr_t, None);
                                builder.emit_borrow(borrow, Place::local(tmp_slot));
                                default_val = FunctionBuilder::copy(borrow);
                            }
                        }
                    }
                    let extern_name = if is_result { "__result_unwrap_or" } else { "__option_unwrap_or" };
                    let dst = ctx.call_extern_tracked(builder,
                        extern_name,
                        vec![borrow_op, default_val],
                        inner_type,
                    );
                    // Move-if-dead: unwrap consumes the Option/Result.
                    // Unregister + MoveZero to transfer ownership.
                    // Skip for Ptr borrow aliases — the slot is a non-owning view;
                    // zeroing it would corrupt the collection element / caller value.
                    if inner_is_resource && !recv_is_collection_borrow {
                        ctx.drops.unregister(place.local);
                        ctx.move_zero_and_mark(builder, place.local);
                    }
                    // For Option__Ref__T.unwrap_or: result is Ptr<T>. Mark as
                    // CowBorrow with the receiver's collection provenance so a
                    // downstream var-decl to a typed local eager-clones (the
                    // None branch's stack-slot Ptr is short-lived but the
                    // var-decl handler's `clone_fn_for_ptr` branch is what
                    // actually fires for resource pointees — both paths
                    // produce an owned fresh value).
                    if matches!(ctx.type_registry.get(inner_type), Some(GirType::Ptr(_))) {
                        ctx.set_cow_borrow(builder, dst);
                        if let Some(collection) = ctx.cow_borrow_source(place.local).cloned() {
                            ctx.set_cow_borrow_source(dst, collection);
                        }
                    }
                    return FunctionBuilder::copy(dst);
                } else {
                    // unwrap() / expect() → direct extraction
                    let extern_name = if is_result { "__result_unwrap" } else { "__option_unwrap" };
                    let dst = ctx.call_extern_tracked(builder,
                        extern_name,
                        vec![borrow_op],
                        inner_type,
                    );
                    // Move-if-dead: unwrap consumes the Option/Result.
                    // Unregister + MoveZero to transfer ownership.
                    // Skip for Ptr borrow aliases — the slot is a non-owning view;
                    // zeroing it would corrupt the collection element / caller value.
                    if inner_is_resource && !recv_is_collection_borrow {
                        ctx.drops.unregister(place.local);
                        ctx.move_zero_and_mark(builder, place.local);
                    }
                    // Ptr(T) from Option__Ref_ (collection .get().unwrap()):
                    // Mark as CowBorrow so typed bindings defer the clone to
                    // ownership boundaries instead of cloning at VarDecl.
                    // Uses insert to override Owned from call_extern_tracked.
                    // Propagate collection provenance from the Option local.
                    if matches!(ctx.type_registry.get(inner_type), Some(GirType::Ptr(_))) {
                        ctx.set_cow_borrow(builder, dst);
                        if let Some(collection) = ctx.cow_borrow_source(place.local).cloned() {
                            ctx.set_cow_borrow_source(dst, collection);
                        }
                    }
                    return FunctionBuilder::copy(dst);
                }
            }
        }
        // Fell through without extracting: either `type_name` couldn't be
        // inferred, or the receiver operand isn't a Copy/Move place (e.g. a
        // constant). The checker already guaranteed the source was
        // Option/Result, so the value is effectively already in hand — `unwrap`
        // is a no-op here. Return the receiver rather than dropping into the
        // generic method-dispatch path below (which would mangle a runtime
        // symbol the C backend can't link).
        return recv;
    }

    // .unwrap_error() / .unwrap_err() on Result → extract Error payload with MoveZero
    if matches!(method_name, "unwrap_error" | "unwrap_err") {
        let type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        // Read typed `enum_category` (Phase A) instead of name-prefix matching.
        let is_result = type_name.as_ref()
            .and_then(|n| ctx.type_registry.get_type_def(n))
            .and_then(|td| td.metadata.enum_category)
            == Some(crate::ir::types::EnumCategory::Result);
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
                    // Ptr-receiver guard: a borrowed enum element (for-loop
                    // collection-element borrow alias) is already `Ptr(enum)` —
                    // pass it through and skip source-invalidation. Other
                    // receivers keep the original emit_borrow + invalidate path.
                    let (borrow_arg, recv_is_collection_borrow) = build_enum_recv_ptr(ctx, builder, &recv, place);
                    let err_is_resource = ctx.type_registry.is_resource_type(err_type);
                    let borrow_op = if err_is_resource && !recv_is_collection_borrow {
                        if let Operand::Copy(p) = &borrow_arg { FunctionBuilder::mov(p.local) } else { borrow_arg.clone() }
                    } else {
                        borrow_arg.clone()
                    };
                    let dst = ctx.call_extern_tracked(builder,
                        "__result_unwrap_error",
                        vec![borrow_op],
                        err_type,
                    );
                    // Move-if-dead: unwrap_error consumes the Result.
                    // Unregister from drops. MoveZero only for temps (named
                    // locals may be read again — unregister alone suffices).
                    // Reviewed 2026-05-04 (Phase D4): keep the guard; the
                    // asymmetry is intentional. For named locals, leaving the
                    // slot non-zeroed preserves observable contents for later
                    // reads while drop-tracking takes ownership; for temps
                    // there are no later reads but MoveZero costs nothing and
                    // closes any aliased-read window.
                    if err_is_resource && !recv_is_collection_borrow {
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
                        let dst = builder.call_clone(&clone_fn, vec![recv], recv_type, crate::ir::ImplicitCloneReason::ExplicitUserClone);
                        // Core #3 (register at birth): the by-value incref result
                        // is a FRESH owned handle. Tag it FreshOwned so a
                        // consuming position (ctor field-init, container literal,
                        // push) MOVES it in rather than re-cloning it via the
                        // Untracked-temp conservative-clone branch (that would
                        // double-incref: `Cell(s.clone(), ..)` → strong_count 3
                        // instead of 2). Sibling of the Shared ctor FreshOwned tag.
                        ctx.set_owned_fresh(builder, dst);
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
                        // Core #3: the downgrade result is a FRESH owned Weak
                        // handle (weak-count incref) — tag FreshOwned so a
                        // consuming position moves it rather than re-cloning the
                        // Untracked temp.
                        ctx.set_owned_fresh(builder, dst);
                        return FunctionBuilder::copy(dst);
                    }
                    // Shared[Vector[T]] element access — at/set_at/slen
                    "at" if elem_suffix.starts_with("Vector__") => {
                        // vector-only-by-design: the outer arm gate matches
                        // `elem_suffix.starts_with("Vector__")`, so this is
                        // reached ONLY for `Shared[Vector[T]]` receivers.
                        // Deque is not currently a receiver kind at this
                        // Shared-wrapped element-access path (no
                        // `Shared[Deque[T]].at/set_at` API surface today);
                        // if that surface is added, add a sibling
                        // `Shared__Deque__` branch alongside this block.
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
                        // vector-only-by-design: symmetric sibling of the
                        // Shared[Vector[T]].at arm above — same Shared-wrapped
                        // Vector-only surface. No `strip_prefix("Vector__")`
                        // on this line but the Vector-only condition IS the
                        // one that gates the sibling `unwrap_or` at :1218,
                        // so keeping the guard-comment style here for parity.
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
                        let dst = builder.call_clone(&clone_fn, vec![recv], recv_type, crate::ir::ImplicitCloneReason::ExplicitUserClone);
                        // Core #3: fresh owned Weak handle — see the Shared clone
                        // arm above; tag FreshOwned so consuming positions move it.
                        ctx.set_owned_fresh(builder, dst);
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
    //
    // The fresh Guard temp is REGISTERED FOR DROP so `{Guard__T}__drop` fires
    // and releases the pthread mutex. Without this the mutex leaks (a chained
    // `m.lock().get()` would hold the mutex forever, deadlocking any follow-up
    // `m.lock()` on the same thread). Sibling of the RWLock read/write arm
    // below (Round XIII Track Y; Core #4 "one fix, all siblings").
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
                    ctx.drops.register_local(dst, guard_type, &ctx.type_registry);
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
    //
    // The returned ReadGuard[T] / WriteGuard[T] carries a TypeDef + guard-kind
    // metadata so a chained `.get()` / `.set()` on the fresh temp resolves
    // through the guard intercept above (methods.rs:1628 -> `guard_of` ->
    // `emit_guard_get_ptr`). Without registration the return type falls back
    // to UNIT_TYPE, the guard channel misses, and the `.get()`/`.set()` call
    // is silently dropped in the emitted C — see
    // `tests/fixtures/known_gaps/rwlock_chained_{read,write}_*.gg`
    // (Round XII Track N3 filing; Round XIII Track Y fix). Registration path
    // mirrors the `shared(rwlock)` `SharedStrategy::ArcRwLock` arm at
    // `stmts/mod.rs:1737-1739`.
    {
        let recv_type_name = infer_type_name_from_operand_full(ctx, &recv, builder);
        if let Some(ref rtn) = recv_type_name {
            if rtn.starts_with("RWLock__") {
                let elem_suffix = rtn.strip_prefix("RWLock__").unwrap_or("int64_t");
                match method_name {
                    "read" => {
                        let rg_name = format!("ReadGuard__{elem_suffix}");
                        let inner_type = super::shared::c_suffix_to_type_id(elem_suffix, ctx);
                        let rg_type = super::type_reg::get_or_register_type(
                            ctx,
                            &rg_name,
                            Some(&|c| super::type_reg::ensure_rwlock_guard_type_def(c, &rg_name, inner_type)),
                        );
                        let read_fn = format!("{rtn}__read");
                        let dst = builder.call(&read_fn, vec![recv], rg_type);
                        // Register the fresh guard temp for scope-exit drop so
                        // `{ReadGuard__T}__drop` fires and releases the pthread
                        // read lock. Without this the read lock leaks (the
                        // annotated `ReadGuard[T] g = r.read()` path gets the
                        // drop via the named-local `stmt::let` registration; a
                        // fresh temp minted here does not).
                        ctx.drops.register_local(dst, rg_type, &ctx.type_registry);
                        return FunctionBuilder::copy(dst);
                    }
                    "write" => {
                        let wg_name = format!("WriteGuard__{elem_suffix}");
                        let inner_type = super::shared::c_suffix_to_type_id(elem_suffix, ctx);
                        let wg_type = super::type_reg::get_or_register_type(
                            ctx,
                            &wg_name,
                            Some(&|c| super::type_reg::ensure_rwlock_guard_type_def(c, &wg_name, inner_type)),
                        );
                        let write_fn = format!("{rtn}__write");
                        let dst = builder.call(&write_fn, vec![recv], wg_type);
                        // Register the fresh guard temp for scope-exit drop —
                        // see the read arm above; without this the write lock
                        // leaks and a follow-up `r.read()`/`r.write()` on the
                        // same thread can deadlock.
                        ctx.drops.register_local(dst, wg_type, &ctx.type_registry);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => {}
                }
            }
        }
    }

    // ReadGuard[T] / WriteGuard[T] — handled by generic dispatch (MutBorrow self_conv)

    // Thread[T] methods: join (Move, pass by value), id (pass by value — pointer).
    //
    // Typed dispatch: the receiver is a Thread handle iff its (pointer-peeled)
    // TypeId is in `type_mapper.thread_payload_types` — written at every
    // Thread-handle mint site (the TypeMapper protocol branch for annotated
    // types, the `thread_spawn` intrinsic for unannotated spawns). The join
    // payload TypeId comes from that same map — never re-derived by slicing
    // the payload name out of the `Thread__` prefix (layering rule 2). The
    // receiver's registered type NAME is used only to spell the
    // `Thread__{T}__join` / `__id` helper symbols: that is the symbol axis,
    // where the name IS the contract with `emit_thread_helpers`.
    {
        let recv_tid = infer_operand_type_full(ctx, &recv, builder);
        let eff_tid = ctx.pointee_type(recv_tid).unwrap_or(recv_tid);
        if let Some(&payload_tid) = ctx.type_mapper.thread_payload_types.get(&eff_tid) {
            if matches!(method_name, "join" | "id") {
                let ttn = ctx.type_name_for_id(eff_tid)
                    .expect("Thread handle TypeId in thread_payload_types must be a registered Named type")
                    .to_string();
                match method_name {
                    "join" => {
                        let join_fn = format!("{ttn}__join");
                        if payload_tid == UNIT_TYPE {
                            builder.call_void(&join_fn, vec![recv]);
                            return Operand::Constant(Constant::Unit);
                        } else {
                            let dst = builder.call(&join_fn, vec![recv], payload_tid);
                            return FunctionBuilder::copy(dst);
                        }
                    }
                    "id" => {
                        let id_fn = format!("{ttn}__id");
                        let dst = builder.call(&id_fn, vec![recv], I64_TYPE);
                        return FunctionBuilder::copy(dst);
                    }
                    _ => unreachable!(),
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
            // Ptr-receiver guard: a borrowed enum element (for-loop collection-
            // element borrow alias) is already `Ptr(enum)` — pass it through
            // instead of re-borrowing it to `Ptr(Ptr(enum))`. Other receivers
            // (values, `&self` params, field borrows) keep the emit_borrow path.
            let (arg, _recv_is_collection_borrow) = build_enum_recv_ptr(ctx, builder, &recv, place);
            let extern_name = match method_name {
                "is_some" | "is_ok" => "__option_is_some",
                _ => "__option_is_none",
            };
            let dst = builder.call_extern(
                extern_name,
                vec![arg],
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
        if let Some(GirType::Named(name)) = ctx.type_registry.get(resolved_type).cloned() {
            // Read typed `collection_kind` (Phase A) — Vector/Deque/GorgetArray
            // all carry `Array` from the protocol registration.
            let is_array = ctx.type_registry.get_type_def(&name)
                .and_then(|td| td.metadata.collection_kind)
                == Some(crate::ir::types::CollectionKind::Array);
            if is_array {
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
        // ─── Guard/ReadGuard/WriteGuard `.get()` — Track J drop-suppression ──
        //
        // Route through the same borrow-projection helper the WRITE path uses
        // (`emit_guard_get_ptr` at `stmts/assigns.rs:859`,
        // `exprs/mod.rs:2283`). Placed at the TOP of the `type_name` block, so
        // this fires ahead of `materialize_lazy_source_if_needed` (L2114) and
        // every other side-effecting arm — the intercept is the sole handler
        // for the read.
        //
        // Root cause (Core #1 — fix at the WRITE site): the declaration in
        // `builtins.rs` said `return_type: ret_elem`, so the typechecker saw
        // an OWNED T flowing back. The runtime returns a pointer INTO the
        // Mutex-owned buffer; the LIR inline arm loaded the T-sized header
        // into a drop-tracked local (`call_tracked` registers it), which
        // aliased the guard's buffer → scope-exit dropped both, hitting
        // `gorget_array_free` twice (Vector[int] double-free) or reading
        // through a freed vector backing (Vector[String] / Dict[..]
        // heap-UAF). The read-site LIR arm is faithful; the LIE was the
        // owned-T ownership tag at the write site.
        //
        // The fix: emit `Guard__T__get_ptr(&g) → MutPtr(T)`, load through the
        // pointer into a fresh local of type T, and tag its ownership as
        // View { RuntimeView(guard_local) }. Consequences:
        //   * drop-insertion emits NO drop for a View local (View is a no-op
        //     drop by design — see `LocalOwnership::View` in `ir/mod.rs`);
        //   * `ensure_owned_at_boundary` (`is_ref_local` returns true for
        //     View) clones-if-borrow when the value flows into an owned
        //     position (return, ctor field, `push`/`put`, etc.);
        //   * `views_of_source(guard_local)` picks it up so a subsequent
        //     `g.set(...)` invalidates the alias via `cow_before_mutation`.
        //
        // Symmetric with `emit_guard_get_ptr` at the write path — one shared
        // helper, one class fix (Core #4). The LIR inline arm at
        // `src/lir/lower/insts.rs:3729` for `gorget_guard_get` becomes
        // unreachable through this path; a `debug_assert!(false, ...)` there
        // catches any regression that would re-route through it.
        if method_name == "get" && args.is_empty() {
            let recv_type = infer_operand_type_full(ctx, &recv, builder);
            if let Some(info) = guard_of(ctx, recv_type) {
                let guard_place = match &recv {
                    Operand::Copy(p) | Operand::Move(p) => p.clone(),
                    _ => {
                        let tmp = builder.add_local(recv_type, None);
                        builder.assign(Place::local(tmp), recv.clone());
                        Place::local(tmp)
                    }
                };
                // Guard's own local is the source axis for the view provenance
                // tag — mutating the guard (e.g. `g.set(...)`) invalidates the
                // view; `views_of_source(source_local)` reads this.
                let source_local = guard_place.local;
                let (inner_ptr_local, inner_type) =
                    emit_guard_get_ptr(ctx, builder, &guard_place, &info);
                // Load through the MutPtr(inner) into a fresh local of type
                // `inner_type`. `LoadRef` on a bare MutPtr local emits the
                // two-step "load pointer bits, load pointee" sequence at LIR
                // (`insts.rs` `Instruction::LoadRef` arm's needs_two_step
                // branch), matching the raw-Load pair the retired inline LIR
                // arm at `insts.rs:3729` used to emit — the value memcpy
                // that materialises the inner T's struct-header into the
                // destination local. The View tag below suppresses drop.
                let dst_local = builder.load_ref(Place::local(inner_ptr_local), inner_type);
                ctx.set_view_of(builder, dst_local, source_local);
                return FunctionBuilder::copy(dst_local);
            }
        }

        // Box[T] methods — read the typed `metadata.is_box` flag rather than
        // name-prefix-probing. `is_box_name` checks the TypeDef metadata at the
        // registry, which every Box registration path now writes uniformly.
        let type_is_box = ctx.type_registry.is_box_name(&type_name);

        // Box[T].get() → call Box__T__get(b) passing value directly (not borrow)
        if type_is_box && method_name == "get" {
            let inner_type_name = &type_name["Box__".len()..];
            let inner_type = ctx.type_mapper.lookup_named(inner_type_name).unwrap_or(I64_TYPE);
            let mangled = format!("{type_name}__get");
            let dst = builder.call(mangled, vec![recv], inner_type);
            return FunctionBuilder::copy(dst);
        }

        // Box[T].set(val) → call Box__T__set(&b, val) passing borrow of box + value
        if type_is_box && method_name == "set" && !args.is_empty() {
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
        if type_is_box {
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
        let items_kind = ctx.type_registry.get_type_def(&type_name)
            .and_then(|td| td.metadata.collection_kind);
        let items_is_map = matches!(items_kind,
            Some(crate::ir::types::CollectionKind::OrderedMap)
            | Some(crate::ir::types::CollectionKind::Map));
        if method_name == "items" && items_is_map {
            // Extract key and value type names from Dict__K__V or HashMap__K__V.
            // The strip-prefix here is at the C-mangling boundary — the K/V slice
            // names are how the runtime address the elements.
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
                // Register Vector[tuple] type name with full Phase A metadata
                // so downstream consumers can read collection_kind etc.
                let vec_name = format!("Vector__{tuple_name}");
                ctx.ensure_collection_type(&vec_name);
                // Also register Option[tuple] for .get() calls
                let option_name = format!("Option__{tuple_name}");
                if ctx.lookup_type_by_name(&option_name).is_none() {
                    ctx.ensure_option_type_registered(&option_name, tuple_type_id);
                }
            }
        }

        // GIR-level desugaring for Option/Result combinators.
        // Replaces C backend inline functions with explicit tag check + closure call,
        // giving the compiler full ownership visibility — including String
        // payloads (Round XV Track B retired the has_string_coercion bail so
        // the adapter owns GorgetString Some/Ok the same way it owns Money;
        // Tier 1c already did this for map_err).
        let is_opt_or_result = ctx.type_registry.get_type_def(&type_name)
            .and_then(|td| td.metadata.enum_category)
            .is_some();
        // Typed combinator_kind (Round XV Track D) — not a method-name match.
        if is_opt_or_result {
            if let Some(kind) = ctx.builtin_combinator_kind(&type_name, method_name) {
                if kind.is_gir_adapter() {
                    if let Some(result) = try_lower_option_result_combinator(
                        ctx, builder, &type_name, method_name, recv.clone(), args,
                    ) {
                        return result;
                    }
                }
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
                let dst = ctx.call_tracked_clone(builder, clone_fn, vec![ptr_arg], recv_type_id, crate::ir::ImplicitCloneReason::ExplicitUserClone);
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
                let dst = ctx.call_tracked_clone(builder, "gorget_string_clone_to_owned", vec![ptr_arg], owned_type, crate::ir::ImplicitCloneReason::ExplicitUserClone);
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
        // Read typed `collection_kind` once for the array discriminator below
        // (Vector/Deque/GorgetArray all map to Array via Phase A protocol).
        let recv_is_array = ctx.type_registry.get_type_def(&type_name)
            .and_then(|td| td.metadata.collection_kind)
            == Some(crate::ir::types::CollectionKind::Array);
        let effective_method = match (type_name.as_str(), method_name, args.len()) {
            ("GorgetString", "split", 2) => "splitn",
            ("GorgetString", "replace", 3) => "replacen",
            ("GorgetString", "find", 2) => "find_from",
            ("GorgetString", "find", 3) => "find_ext",
            (_, "sort", 1) if recv_is_array => "sort_by",
            (_, "sorted", 1) if recv_is_array => "sorted_by",
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

        // Tier 1c pre-call clone for non-adapter Option/Result combinators
        // when recv is live past this call. The C inline combinator's
        // return value shallow-copies recv's payload (e.g. `or` returns
        // recv on the Ok branch by memcpy); if recv stays alive, both
        // recv and the result drop the same heap data at scope exit.
        // Cloning recv before the call gives the runtime an independent
        // copy to consume, leaving the original intact. Matches the
        // adapter's scrut_local Clone (see
        // `try_lower_option_result_combinator` above).
        //
        // Gated to non-adapter paths because the adapter early-returns;
        // the adapter path already Clones internally and doesn't reach
        // this site.
        if let Some(recv_local) = recv_local_for_move_zero {
            let is_option_result = ctx.type_registry.get_type_def(&type_name)
                .and_then(|td| td.metadata.enum_category)
                .is_some();
            // Typed combinator_kind (Round XV Track D) — D2 pre-call clone gate.
            let is_combinator = ctx.builtin_combinator_kind(&type_name, method_name).is_some();
            if is_option_result && is_combinator
                && ctx.type_registry.is_resource_type(builder.local_type(recv_local))
            {
                let is_last_use = builder.local_name(recv_local)
                    .map(|n| ctx.is_last_use_at(n, receiver.span))
                    .unwrap_or(true);
                if !is_last_use {
                    let recv_type = builder.local_type(recv_local);
                    if let Some(clone_fn) = ctx.clone_fn_for_ptr(recv_type) {
                        ctx.warn_clone_and_hit(builder, receiver.span, recv_type, crate::ir::ImplicitCloneReason::CallArg);
                        let ptr_type = ctx.register_ptr_type(recv_type);
                        let ptr_local = builder.add_local(ptr_type, None);
                        builder.emit_borrow(ptr_local, Place::local(recv_local));
                        let cloned = builder.call_clone(
                            &clone_fn,
                            vec![FunctionBuilder::copy(ptr_local)],
                            recv_type,
                            crate::ir::ImplicitCloneReason::CallArg,
                        );
                        ctx.set_owned(builder, cloned);
                        // Route the call through the clone. The original
                        // recv stays alive past this call.
                        recv = FunctionBuilder::copy(cloned);
                    }
                }
            }
        }

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

        // CoW G1 — full lazy copy-on-write, decide-at-root: a PROJECTED
        // mutating receiver (`v[i].method()`, `obj.field.method()`,
        // `v[i].inner.method()`) writes through a pointer into the projection
        // ROOT. Materialize the immutable-in-context root before the call so
        // the mutation lands on an owned copy; the receiver is then rebuilt
        // against the rebound owned local (index case: re-lower `recv`;
        // field-path case: the relocated `field_place_info` below re-resolves
        // by NAME). cow_before_mutation is a no-op on unique/owned roots
        // (`&self`/`&` chain, already-owned), so write-through along an
        // unbroken `&` chain is preserved.
        //
        // This fires for BOTH index-projected AND pure field-path receivers
        // (Core #1/#4 — one uniform trigger on the root's immutability, not a
        // per-shape provenance tag). The OLD framing — "field-access chains
        // are already handled by cow_before_field_mutation, materializing the
        // root here is redundant" — was FALSE: `cow_before_field_mutation`
        // only materializes collection refs INTO the field path
        // (context.rs:cow_before_field_mutation), never the ROOT struct, so
        // the field-path case wrote through the caller's buffer until this
        // gate was removed. `field_place_info` is recomputed AFTER these CoW
        // blocks so the field borrow re-resolves against the rebound root.
        if needs_mut
            && !matches!(&receiver.node, Expr::Identifier(_))
        {
            if let Some(root_local) = resolve_projection_root_local(ctx, &receiver.node) {
                // Snapshot the root's name→local binding; cow_before_mutation
                // rebinds it to a fresh owned local ONLY when it actually
                // materializes (bare-param / alias / element root). A
                // unique-borrow (`&`) or already-owned root is a no-op → no
                // rebind → we must NOT re-lower (that would gratuitously
                // re-emit the element read and let Case 3 clone stale temps).
                let root_name = builder.local_name(root_local).map(|s| s.to_string());
                let before = root_name.as_deref()
                    .and_then(|n| ctx.lookup_local(n).map(|(l, _)| l));
                ctx.cow_before_mutation(builder, root_local, receiver.span);
                let after = root_name.as_deref()
                    .and_then(|n| ctx.lookup_local(n).map(|(l, _)| l));
                // Record whether the root actually materialized into a private
                // owned copy — the end-of-statement untrack gate reads this so it
                // only fires when there IS a private copy for the receiver/arg
                // element refs to dangle into (non-materializing calls stay
                // byte-identical).
                if before != after {
                    did_g1_materialize = true;
                }
                // Re-lower `recv` ONLY for an INDEX-projected receiver
                // (`v[i].method()`) — there `recv` IS the self-arg the call
                // uses, and it must re-read the element out of the rebound
                // owned root. A pure FIELD-PATH receiver (`h.f.method()`,
                // field_path_for_cow=Some) instead builds its self-arg from the
                // relocated `field_place_info` below (which re-resolves against
                // the rebound root by name); re-lowering `recv` here would be a
                // DEAD field read that still CLONES the resource field — the
                // spurious +120K self-host clone the index-only prototype
                // avoided by gating field-path out of this block entirely.
                if before != after && field_path_for_cow.is_none() {
                    recv = lower_expr(ctx, builder, receiver);
                }
            }
        }

        // CoW: if receiver is being mutated, sever any alias relationships first.
        // This may materialize a Ptr param → new owned local (Phase 1c),
        // so re-resolve the receiver afterwards.
        // Gated to NAMED locals: an anonymous element temp (`v[0]` inline)
        // is handled by the projected-root block above; running
        // cow_before_mutation on it would emit a wasted element clone whose
        // rebind can't take (the temp has no name), leaving the call on the
        // original pointer (the pre-prototype G1-method leak shape).
        if needs_mut {
            if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
                if place.projections.is_empty()
                    && builder.local_name(place.local).is_some() {
                    ctx.cow_before_mutation(builder, place.local, receiver.span);
                    // Re-resolve: cow_before_mutation may have redirected the variable
                    // name to a new owned local (Phase 1c param materialization).
                    if let Some(hint) = builder.local_name(place.local).map(|s| s.to_string()) {
                        if let Some((new_local, _)) = ctx.lookup_local(&hint) {
                            if new_local != place.local {
                                recv = FunctionBuilder::copy(new_local);
                                // A bare-param NAMED receiver (`v.push(v[0])`)
                                // materialized its root into a private copy here —
                                // arm the end-of-statement untrack (see the
                                // projected-root block) so a same-collection element
                                // ARG ref into that copy can't dangle on the next push.
                                did_g1_materialize = true;
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
                    // Check CollectionRef (from index_load) and CowBorrow (from .get().unwrap()).
                    // Phase D: read CollectionRef via the typed accessor; its
                    // `CollectionId::FieldPath` arm is excluded by the
                    // Local match below (only direct-collection sources are
                    // severed at this site).
                    let source = match ctx.collection_ref_source(builder, place.local) {
                        Some(CollectionId::Local(src)) => Some(src),
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
                                    // The index-source collection materialized into a
                                    // private copy — arm the end-of-statement untrack
                                    // (symmetric with the named-receiver block above).
                                    did_g1_materialize = true;
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

        // Lazy loop-carried CoW, hook W3b (`returns_view` RECEIVER): a
        // view-returning method on a lazy-view receiver copies the receiver's
        // header AT CALL TIME — the produced view aliases the element buffer,
        // not the receiver's slot, so the post-call View tag (the
        // `set_view_of` site at the result handling below) is too late and
        // the later materialize of `s` cannot fix the temp. Materialize the
        // receiver in place BEFORE the receiver borrow is built (upstream of
        // `emit_borrow` and ALL call-emission arms). Before-args placement
        // also covers args that mutate the source (`s.substring(0, poke(&v))`).
        if ctx.builtin_returns_view(&type_name, method_name) {
            ctx.materialize_lazy_source_if_needed(builder, &recv, receiver.span);
        }

        // Relocated field-borrow resolution (from the top of this fn). Computed
        // HERE, after the CoW mutation blocks, so that when a projected-root
        // materialize above rebound the receiver's root local (`h.nums.push()`
        // on a bare `h` → `h` is now a fresh owned copy), the field borrow
        // re-resolves against THAT rebound local via ctx.lookup_local inside
        // try_resolve_field_place — the field-store lands on the copy, not the
        // un-materialized original (which would both write through the caller's
        // buffer AND waste the materialize clone). On an `&`/owned root the
        // materialize was a no-op, so this resolves to the same place as before
        // — write-through preserved. Mirrors stmts/assigns.rs:lower_field_assign,
        // which resolves the field place AFTER cow_before_field_mutation.
        let field_place_info = if let Expr::FieldAccess { object, field } = &receiver.node {
            let info = try_resolve_field_place(ctx, builder, object, &field.node);
            // CoW UAF (Core #4 sibling): `try_resolve_field_place`'s `Expr::Index`
            // arm resolves a value-field of an index element (`v[i].vf.method()`)
            // to a write-through element pointer, and for a MULTILEVEL base
            // (`m[i][j].vf.method()`) its inner `lower_expr(m[i])` mints a transient
            // CollectionElement handle. Left CoW-tracked, that handle dangles when a
            // later same-collection `push` reallocs and a Case-3 clone reads the
            // freed buffer (ASan heap-UAF). Arm the end-of-statement untrack (the
            // `did_g1_materialize`-gated `untrack_transient_element_refs_in_range`
            // below) so the transient is cleared — mirrors lower_field_assign's
            // hoisted untrack. IR-instruction-neutral for the single-level shape
            // (base is a bare local → no transient minted → untrack is a no-op).
            if info.is_some() && expr_projection_contains_index(&object.node) {
                did_g1_materialize = true;
            }
            info
        } else {
            None
        };

        // Sibling of `field_place_info` (Core #4): bare Index receiver with a
        // mutating method (`v[i].bump()`). Computed AFTER the CoW G1 rebind /
        // re-lower blocks so the index_load re-resolves against any rebound
        // root. `lower_index_access` returns a value COPY for VALUE-type
        // elements — borrowing that throwaway and calling a mut method would
        // silently drop the write (the field-of-index shape `hs[0].c.bump()`
        // already wrote through via `try_resolve_field_place`). Shared producer
        // `try_resolve_index_element_ptr` forces `Ptr(elem)` for value AND
        // resource elements. Read-only `v[i].get_n()` stays on the value-read
        // path (clone elision / 1B lesson). Arm `did_g1_materialize` so the
        // end-of-stmt untrack clears any transient element handle the producer
        // minted (multilevel base; single-level bare-local is a no-op untrack).
        let index_elem_place_info = if field_place_info.is_none() && needs_mut {
            if let Expr::Index { object: coll, index } = &receiver.node {
                let info = try_resolve_index_element_ptr(ctx, builder, coll, index);
                if info.is_some() {
                    did_g1_materialize = true;
                }
                info
            } else {
                None
            }
        } else {
            None
        };

        // If receiver is a field access, borrow the field in-place instead of
        // borrowing a copy (which would mutate the copy, not the original).
        // Exception: if the field's type is already `Ptr(T)` / `MutPtr(T)` —
        // user-written `Ref[T]` / `MutRef[T]` borrow field — its STORED VALUE
        // is already the receiver pointer; borrowing the field place would
        // produce `**T`, which the method's `*T self` ABI rejects. Fall
        // through to the `recv` (Copy/Move) path which already handles the
        // existing `is_ptr` check correctly.
        //
        // Bare Index mut receiver (`v[i].bump()`): borrow `*elem_ptr` in place
        // so the method writes through into the collection buffer.
        let field_is_borrow_ptr = field_place_info.as_ref()
            .map(|(_, fty)| matches!(
                ctx.type_registry.get(*fty),
                Some(GirType::Ptr(_) | GirType::MutPtr(_))
            ))
            .unwrap_or(false);
        if let Some((elem_ptr_place, elem_type_id)) = index_elem_place_info {
            let mut elem_place = elem_ptr_place;
            elem_place.projections.push(Projection::Deref);
            let pt = ctx.register_mut_ptr_type(elem_type_id);
            let pl = builder.add_local(pt, None);
            builder.emit_borrow_mut(pl, elem_place);
            call_args.push(FunctionBuilder::copy(pl));
        } else if let Some((field_place, field_type_id)) = field_place_info.clone()
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

        // Resolve named args + fill trailing defaults for equip-method calls —
        // mirrors the free-fn `resolve_call_args` (calls.rs), offset by the
        // implicit `self`. Equip methods register `fn_param_names` /
        // `fn_defaults` keyed by the mangled `Type__method` name, SURFACE-
        // indexed with `self` at idx 0; the method-call `args` here are the
        // NON-self surface args (arg position N ↔ surface param N+1). Without
        // this fill, `p.add(5)` to a method with a trailing default would
        // lower as "too few arguments to function 'P__add'".
        let filled_args: Vec<Spanned<ast::CallArg>> =
            resolve_method_call_args(ctx, effective_name.as_str(), args);
        let args: &[Spanned<ast::CallArg>] = &filled_args;

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

        // Save/restore frame for expected_type around method-arg lowering.
        // Combinator dual-rule forces live only inside
        // `try_lower_option_result_combinator` (or_else/flat_map always;
        // and_then conditional on outer expected). The pre-adapter always-
        // force for and_then|or_else was deleted (Round XVI F3 / Core #14):
        // it double-set the same field and contradicted the dual rule.
        let prev_expected = ctx.func_state.expected_type;

        // Save pending_move_zeros baseline so we only drain entries added
        // by THIS method call's argument lowering (not from nested/prior calls).
        let move_zero_baseline = ctx.func_state.pending_move_zeros.len();
        let temp_drop_baseline = ctx.func_state.pending_temp_drops.len();

        // Snag #43 (2026-05-13) prep: set `expected_type` per consuming
        // arg position so the auto-propagation step inside
        // `lower_call_arg` knows whether to unwrap a throws-call result
        // (param type is T → unwrap) or keep it (param type is
        // `Result[T,E]` → leave alone, e.g.
        // `Vector[Result[int, String]].push(Ok(1))`). For
        // GIR-lowered equip methods, `method_param_types` already
        // carries the typed signature; for builtin collection runtime
        // methods, derive the element/value type from the receiver's
        // mangled name (see the per-family hint below — Dict/HashMap value
        // typing IS handled, via `infer_collection_element_type`).
        //
        // Owning-VALUE-arg expected-type hint. The value position and the
        // element/value type it should carry differ per method family:
        //
        //   push/add/extend/send/push_back/push_front → value is arg 0; its
        //     type is the collection ELEMENT type (`Vector__T`/`Set__T` →  T),
        //     recovered by `extract_elem_type_id_from_type_name`.
        //   put/set/insert/fill/get_or_put → value is the LAST arg (Dict
        //     `put(k, v)` / `get_or_put(k, v)`, Vector `set(i, v)` /
        //     `insert(i, v)` / `fill(n, v)`); its type is the collection VALUE
        //     type (Dict → V, Vector → T), recovered by
        //     `infer_collection_element_type` (the SAME source of truth the
        //     `d[k]` / `v[i]` index-read path uses, so the Dict key/value
        //     split heuristic is consistent across both). The KEY / INDEX /
        //     COUNT arg (position 0) is left un-hinted — its type is unrelated
        //     to V.
        //
        // Without this hint the value's `expected_type` is unset, so a bare
        // `None` (or `Some`/`Ok`/`Error`) in the value position materialises as
        // `Constant::Null` and is either copied into the slot as zeros == bogus
        // `Some(0)` (Dict `put`/`get_or_put`, whose runtime store does NOT
        // rewrite Null → tagged struct — `Dict[_, Option[T]].put(k, None)` read
        // back as `Some(0)` on both backends) or memcpy'd FROM the null pointer
        // (Vector `fill`, which SEGV'd on `gorget_array_fill(..., NULL)`).
        // `Vector.push`/`set`/`insert` avoided it — `push` via this same hint,
        // `set`/`insert` via the store-site Null rewrite — an inconsistency this
        // unifies. Mirrors the self-host `lower_expr.gg` owning-value
        // element-type hint (round-35 T3).
        //
        // NOTE: this is a HINT ONLY. `fill`/`get_or_put` are deliberately NOT
        // added to `consuming_positions_by_name` below — the hint-vs-consume
        // separation is load-bearing: `fill` clones its value per element
        // internally and `get_or_put` borrows the default, so consuming (clone +
        // move-zero) the value here would double-free a live source
        // (`fill(2, live_string)` / `get_or_put(k, live_default)`).
        let elem_type_hint = extract_elem_type_id_from_type_name(ctx, &type_name);
        let (value_arg_idx_for_method, value_arg_type_hint): (Option<usize>, Option<TypeId>) =
            match method_name {
                "push" | "add" | "extend" | "send" | "push_back" | "push_front" =>
                    (Some(0), elem_type_hint),
                "put" | "set" | "insert" | "fill" | "get_or_put" if args.len() >= 2 => {
                    // `recv` has already been moved into `call_args` above; recover
                    // the receiver TypeId from the mangled `type_name` instead.
                    let val_hint = ctx.type_mapper.lookup_named(&type_name)
                        .map(|recv_tid| infer_collection_element_type(ctx, recv_tid));
                    (Some(args.len() - 1), val_hint)
                }
                _ => (None, None),
            };
        let mut lowered_method_args: Vec<Operand> = args.iter()
            .enumerate()
            .map(|(i, arg)| {
                let prev_expected = ctx.func_state.expected_type;
                let callee_pt = method_param_types.get(i).copied();
                if let Some(pt) = callee_pt {
                    ctx.func_state.expected_type = Some(pt);
                } else if Some(i) == value_arg_idx_for_method {
                    if let Some(et) = value_arg_type_hint {
                        ctx.func_state.expected_type = Some(et);
                    }
                }
                // Method args: i is 0-based for non-self args, but fn_param_ownerships
                // includes self at index 0, so offset by 1.
                let op = lower_call_arg(ctx, builder, arg, callee_pt, &effective_name, i + 1);
                ctx.func_state.expected_type = prev_expected;
                op
            })
            .collect();
        // Positions that semantically consume (take ownership of) their arg.
        // `GorgetString.push/push_line/push_char` are StringBuilder appends — they
        // READ the arg and copy its bytes, they do NOT take ownership. Collection
        // mutating methods (push/add/extend/send/push_back/push_front) consume
        // arg 0; (put/set/insert) consume the value at arg 1 (dict) or arg 1 (vec).
        let is_string_builder_method = type_name == "GorgetString";
        // The consuming-position NAME match below applies ONLY to builtin
        // collection RUNTIME methods, which carry no typed param signature —
        // `is_gir_method` is false, so `method_param_types` is empty and the
        // `lower_call_arg` pass above could NOT decide arg ownership from the
        // callee's param type, hence this name-based fallback. A USER equip
        // method that merely SHARES a name with a builtin collection mutator
        // (`equip Q: void push(&self, Ev !event)`) already had its arg ownership
        // resolved by `lower_call_arg` from its typed `method_param_types`;
        // re-running the consuming-position materialization here spuriously
        // CLONES a fresh temp at the call site (gorget-arena snag #2 — a user
        // `push`/`add`/`insert`/`set`/`send`/`put` was routed to the consume
        // path purely because of its name). Route the decision off the
        // resolved-callee typed identity (`gir_equip_methods` — "did this call
        // resolve to a user GIR equip method?"), NOT the method NAME. The prior
        // `fn_param_abis`/`ByPtr` filter below only caught borrow-param user
        // methods and MISSED `!`-move-param ones. (CLAUDE.md "No name matching",
        // Core invariant #2: put the flag on the typed decl, read via accessor.)
        let consuming_positions_by_name: Vec<usize> = if is_gir_method {
            vec![]
        } else {
            match method_name {
                "push" | "add" | "extend" | "send" | "push_back" | "push_front"
                    if !is_string_builder_method => vec![0],
                "put" | "set" | "insert" => {
                    let mut p = vec![];
                    if lowered_method_args.len() >= 1 { p.push(0); }
                    if lowered_method_args.len() >= 2 { p.push(1); }
                    p
                }
                _ => vec![],
            }
        };
        // The name match above only identifies CANDIDATE value positions. A
        // position genuinely *consumes* its arg — and therefore needs the
        // own-the-value materialization (clone non-last-use / move-zero
        // last-use) below — only when the callee takes it by value or by move.
        // A const-borrow param (`ParamABI::ByPtr`, e.g. a user method
        // `SlotKey insert(&self, T value)` whose `value` is a borrow) does NOT
        // take ownership: it reads through the pointer and clones internally if
        // it stores. Cloning + move-zeroing such an arg here would zero a fresh
        // clone that nobody takes ownership of → leak (the clone temps are
        // move-zeroed, never dropped). `lower_call_arg`'s borrow path (incl. the
        // owning-temp re-home + post-call drop) already handles those correctly.
        // Name-matching the method alone (pre-2026-05-25) wrongly treated
        // same-named user methods as builtin-collection consumers. ABI unknown
        // (extern/runtime without a registered ABI) → keep, preserving prior
        // behavior. (CLAUDE.md "no name matching": route on the typed ABI.)
        let consuming_positions: Vec<usize> = consuming_positions_by_name
            .into_iter()
            .filter(|&idx| {
                !matches!(
                    ctx.fn_param_abis
                        .get(&effective_name)
                        .and_then(|abis| abis.get(idx + 1))
                        .copied(),
                    Some(ParamABI::ByPtr)
                )
            })
            .collect();

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

        // CoW UAF fix (round-33, class fix — 3rd G1 root-materialize site): if the
        // receiver root-materialized above, untrack EVERY transient element/field-
        // path handle minted across this whole method-call statement (receiver
        // chain + args). Runs HERE, after `ensure_owned_at_consuming_arg` cloned
        // the consumed args, so every handle left in range is a dead READ ref
        // (the call below uses the cloned/owned operands, not the tag). An arg
        // that is an element of the SAME collection the receiver materialized
        // (`v[0].set_from(v[1])`, `m[0].push(m[1][0])`) would otherwise dangle on
        // a later same-collection push (Case 3 clones freed memory). Guarded by
        // `did_g1_materialize` so `&`-correct / non-materializing calls emit
        // identical IR (byte-identical self-host). `local_name().is_none()` spares
        // live named borrows. Mirrors lower_field_assign / lower_index_assign.
        if did_g1_materialize {
            ctx.untrack_transient_element_refs_in_range(builder, stmt_locals_start, builder.locals.len());
        }

        // Restore previous hints and expected type
        ctx.func_state.closure_param_type_hints = prev_hints;
        ctx.func_state.expected_type = prev_expected;

        // For Vector.zip(other_vec), register tuple and result vector types
        if method_name == "zip" && recv_is_array {
            // vector-only-by-design: `.zip()` is currently a Vector-only
            // method surface (no `Deque.zip()` user-facing API today). The
            // `recv_is_array` gate is broader (includes Deque per
            // CollectionKind::Array — see src/ir/types.rs) but no Deque zip
            // fixture reaches this arm. When Deque.zip is added, mirror
            // this whole block with a `Deque__` strip alongside the
            // Vector__ one and drop this comment.
            let self_elem = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            // Get the other vector's element type from the first explicit arg
            let other_elem_name = if let Some(arg_op) = lowered_method_args.first() {
                if let Operand::Copy(p) | Operand::Move(p) = arg_op {
                    let type_id = builder.local_type(p.local);
                    let type_str = crate::ir::types::format_type_for_mangle(type_id, &ctx.type_registry);
                    // vector-only-by-design: sibling of the self_elem strip
                    // just above; same Vector.zip-specific surface.
                    type_str.strip_prefix("Vector__").unwrap_or(&type_str).to_string()
                } else { "int64_t".to_string() }
            } else { "int64_t".to_string() };
            // Register the tuple type
            let self_type = resolve_inner_type(ctx, self_elem);
            let other_type = resolve_inner_type(ctx, &other_elem_name);
            let tuple_type_id = register_tuple_type(ctx, &[self_type, other_type]);
            let tuple_name = ctx.type_name_for_id(tuple_type_id).unwrap_or("int64_t").to_string();
            // Register Vector[Tuple] type with full Phase A metadata.
            let vec_name = format!("Vector__{tuple_name}");
            ctx.ensure_collection_type(&vec_name);
        }

        // Borrowing methods (get/first/last) always return `Option__Ref__T` with a
        // Ptr(T) payload, regardless of whether T is a resource type. This keeps
        // the IR's return type identical to the user-declared `Option[Ref[T]]`
        // and avoids aliasing an int-value as a pointer when the two forms
        // used to diverge (IR said Option[T], typechecker said Option[Ref[T]]).
        // Consuming methods (pop/remove) keep `Option__T` with a value payload.
        let fn_sig_ret = ctx.fn_sigs.get(&effective_name).map(|(_, ret)| *ret);
        // Read typed `collection_kind` once (Phase A) and reuse the
        // discriminator for every dispatch arm in this block. Vector/Deque/
        // GorgetArray → Array; Dict → OrderedMap; HashMap/GorgetMap → Map;
        // Set → OrderedSet; HashSet/GorgetSet → Set.
        let recv_collection_kind = ctx.type_registry.get_type_def(&type_name)
            .and_then(|td| td.metadata.collection_kind);
        let recv_is_array = recv_collection_kind == Some(crate::ir::types::CollectionKind::Array);
        let recv_is_map = matches!(recv_collection_kind,
            Some(crate::ir::types::CollectionKind::OrderedMap)
            | Some(crate::ir::types::CollectionKind::Map));
        let recv_is_set = matches!(recv_collection_kind,
            Some(crate::ir::types::CollectionKind::OrderedSet)
            | Some(crate::ir::types::CollectionKind::Set));
        if matches!(method_name, "get" | "first" | "last" | "remove" | "pop") && recv_is_array
        {
            // vector-only-by-design: Deque's `get/first/last/remove/pop` do
            // not currently exist as user-facing methods (only Vector uses
            // this Option[Ref[T]] return-type registration path). The
            // `recv_is_array` gate is broader (includes Deque) but no
            // Deque get/first/... fixture reaches this arm. When those
            // methods are added to Deque, mirror this block with a
            // `Deque__` strip.
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
        // For Dict/HashMap.get(), auto-register `Option[Ref[V]]` with a `Ptr(V)`
        // payload — symmetric with Vector.get() above. `gorget_map_get` returns
        // `void*` into the bucket's value slot, so the Option's Some payload IS
        // the borrow. Keeps the IR's return type identical to the typechecker's
        // `Option[Ref[V]]` (typecheck.rs:4565-4574) and makes
        // `d.get(k).unwrap().push(x)` mutate the stored element instead of a
        // byte-copy.
        if method_name == "get" && recv_is_map {
            let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
            if let Some(rest) = type_name.strip_prefix(prefix) {
                if let Some(pos) = rest.find("__") {
                    let val_name = &rest[pos + 2..];
                    let option_name = format!("Option__Ref__{val_name}");
                    if ctx.lookup_type_by_name(&option_name).is_none() {
                        let inner_type = resolve_inner_type(ctx, val_name);
                        let ptr_type = ctx.type_registry.insert(GirType::Ptr(inner_type));
                        ctx.ensure_option_type_registered(&option_name, ptr_type);
                    }
                }
            }
        }
        // For Dict/HashMap.remove(), auto-register `Option[V]` with a value
        // payload — consuming counterpart to Dict.get above. `gorget_map_remove_opt`
        // returns `void*` into the removed value's slot (ownership transferred to
        // caller). Without this register, the fn_sigs entry for the method falls
        // back to a placeholder (i64) and chained `.remove(k).unwrap()` collapses
        // the intermediate Option slot — the LIR lift's `slot_kind == Option`
        // guard never matches and the raw void* assignment hits the unwrapped
        // V slot. Mirrors the Vector.pop/remove branch above.
        if method_name == "remove" && recv_is_map {
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
            if matches!(method_name, "get" | "first" | "last" | "remove" | "pop") && recv_is_array {
                // vector-only-by-design: return-type resolver sibling of the
                // Option registration at :2741 above. Same Vector-only
                // get/first/last/remove/pop surface — see :2741 for the
                // full note.
                let elem_type_name = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
                let _inner_type = resolve_inner_type(ctx, elem_type_name);
                let is_borrowing = matches!(method_name, "get" | "first" | "last");
                let option_name = if is_borrowing {
                    format!("Option__Ref__{elem_type_name}")
                } else {
                    format!("Option__{elem_type_name}")
                };
                ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
            } else if method_name == "get" && recv_is_map {
                // Dict/HashMap.get() returns Option[Ref[V]] with Ptr(V) payload
                // (symmetric with Vector.get() — see auto-register block above).
                let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                if let Some(rest) = type_name.strip_prefix(prefix) {
                    if let Some(pos) = rest.find("__") {
                        let val_name = &rest[pos + 2..];
                        let option_name = format!("Option__Ref__{val_name}");
                        ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
                    } else { ret }
                } else { ret }
            } else if method_name == "remove" && recv_is_map {
                // Dict/HashMap.remove(key) → Option[V !]
                let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                if let Some(rest) = type_name.strip_prefix(prefix) {
                    if let Some(pos) = rest.find("__") {
                        let val_name = &rest[pos + 2..];
                        let option_name = format!("Option__{val_name}");
                        ctx.lookup_type_by_name(&option_name).unwrap_or(ret)
                    } else { ret }
                } else { ret }
            } else if method_name == "remove" && recv_is_set {
                BOOL_TYPE
            } else if is_sentinel_wrapped {
                // Stdlib sentinel-to-Option wrapping for find/index_of
                ctx.lookup_type_by_name("Option__int64_t").unwrap_or(ret)
            } else {
                ret
            }
        } else if method_name == "zip" && recv_is_array {
            // vector-only-by-design: zip return-type-lookup sibling of the
            // zip type-registration block at :2690 above; same Vector.zip-
            // only surface.
            // zip return type: look up the Vector__Tuple__A__B type we just registered
            let self_elem = type_name.strip_prefix("Vector__").unwrap_or("int64_t");
            let other_elem_name = if let Some(arg_op) = lowered_method_args.first() {
                if let Operand::Copy(p) | Operand::Move(p) = arg_op {
                    let type_id = builder.local_type(p.local);
                    let type_str = crate::ir::types::format_type_for_mangle(type_id, &ctx.type_registry);
                    // vector-only-by-design: nested sibling of self_elem
                    // strip just above; same Vector.zip-only surface.
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
                    if ctx.is_bare_param(builder, local_id) { return None; }
                    if ctx.is_ref_local(builder, local_id) { return None; }
                    if ctx.is_cow_borrow(builder, local_id) { return None; }
                    // Skip non-named locals (should be rare — falls through via temp path).
                    // Reviewed 2026-05-04 (Phase D4): structurally defensive. The
                    // outer branch is Expr::Identifier resolved through
                    // lookup_local, which under normal invariants only returns
                    // named locals; the temp-path branch below handles
                    // expression-shape args. The guard would be redundant
                    // under stricter typing but is cheap insurance.
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
            // An owning `!` resource param forwarded AS ITS POINTER
            // (`out.push(!item)` where `item: MutPtr(S)` is `is_owning_param`)
            // is a MOVE, not a borrow — `lower_call_arg` already forwarded the
            // pointer + move-zeroed the slot, so re-cloning here defeats the
            // move. Strings are excluded (they clone via a different path).
            let is_owning_param_ptr = |local: LocalId, builder: &FunctionBuilder, ctx: &LoweringContext| -> bool {
                (local.0 as usize) < builder.locals.len()
                    && builder.locals[local.0 as usize].is_owning_param
                    && !ctx.is_string_type(builder.local_type(local))
            };
            let needs_clone = call_args.get(call_idx).and_then(|op| {
                if let Operand::Copy(place) | Operand::Move(place) = op {
                    if place.projections.is_empty() && !is_owning_param_ptr(place.local, builder, ctx) {
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
                        if place.projections.is_empty() && !is_owning_param_ptr(place.local, builder, ctx) {
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
                    let cloned = ctx.emit_clone(builder, &clone_fn,
                        vec![FunctionBuilder::copy(ptr_local)], span, inner_type, crate::ir::ImplicitCloneReason::ConsumingArg);
                    ctx.drops.register_local(cloned, inner_type, &ctx.type_registry);
                    ctx.set_owned(builder, cloned);
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
        // Read typed `collection_kind` (recv must be an Array) and
        // `enum_category` (return must be an Option) — Phase A typed
        // dispatch instead of name-prefix probes.
        let recv_is_array = ctx.type_registry.get_type_def(&type_name)
            .and_then(|td| td.metadata.collection_kind)
            == Some(crate::ir::types::CollectionKind::Array);
        let ret_is_option = ctx.type_registry.enum_category(ret_type)
            == Some(crate::ir::types::EnumCategory::Option);
        let is_option_void_ptr_vector = matches!(method_name, "get" | "first" | "last" | "pop" | "remove")
            && recv_is_array
            && ret_type != UNIT_TYPE
            && ret_is_option;
        // Dict/HashMap.remove(key) shares the void*-returning shape with Vector.pop/
        // remove: `gorget_map_remove_opt` returns a pointer to the removed value's
        // bucket-slot (NULL = not found). Build the Option explicitly with a
        // null-check + EnumInit at the GIR layer so the IR is type-truthful
        // regardless of downstream `.unwrap()` fusion. Without this, a chained
        // `dp.remove(k).unwrap()` collapses the intermediate Option-typed temp,
        // the LIR lift's `slot_kind == EnumKind::Option` guard never matches,
        // and the raw `void*` lands directly in the unwrapped V slot — broken
        // for struct/enum V, silent garbage for primitive V.
        let is_option_void_ptr_dict_remove = method_name == "remove"
            && recv_is_map
            && ret_type != UNIT_TYPE
            && ret_is_option;

        let result = if is_option_void_ptr_vector || is_option_void_ptr_dict_remove {
            // Resolve the value-name (V for Dict__K__V, T for Vector__T) for inner-type
            // lookup and Option name resolution.
            let elem_type_name: String = if is_option_void_ptr_dict_remove {
                let prefix = if type_name.starts_with("Dict__") { "Dict__" } else { "HashMap__" };
                type_name.strip_prefix(prefix)
                    .and_then(|rest| rest.find("__").map(|pos| rest[pos + 2..].to_string()))
                    .unwrap_or_else(|| "int64_t".to_string())
            } else {
                // vector-only-by-design: this arm is reached only when
                // `is_option_void_ptr_vector`, which is set at :3097 gated
                // on `recv_is_array && method_name ∈ {get, first, last,
                // remove, pop}` — the SAME Vector-only get/first/... surface
                // allowlisted at :2741 / :2821 above.
                type_name.strip_prefix("Vector__").unwrap_or("int64_t").to_string()
            };
            let inner_type = resolve_inner_type(ctx, &elem_type_name);
            let is_borrowing = matches!(method_name, "get" | "first" | "last")
                && is_option_void_ptr_vector;
            // Borrowing methods always produce Option__Ref__T with a Ptr(T) payload —
            // the raw pointer from the runtime `gorget_array_safe_get` IS the payload.
            // Consuming methods (Vector.pop/remove, Dict.remove) deref to take
            // ownership of the value.
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
            // Move from the fresh enum_init temp into the merge slot. The
            // some_val/none_val temp is dead immediately after this assign;
            // Copy mode would shallow-alias the heap payload of a resource
            // Option, tripping Phase C's resource-moves validator. See
            // `docs/devbook/13-ownership-in-ir.md` (Phase A, type axis).
            builder.assign_mode(AssignMode::Move, Place::local(result_id), FunctionBuilder::copy(some_val));
            builder.jump(merge_bb);

            // === None block: construct Option.None() ===
            builder.switch_to(none_bb);
            let none_val = builder.enum_init(&option_name, "None", ret_type, vec![]);
            builder.assign_mode(AssignMode::Move, Place::local(result_id), FunctionBuilder::copy(none_val));
            builder.jump(merge_bb);

            // === Merge ===
            builder.switch_to(merge_bb);
            if ctx.type_registry.needs_drop(ret_type) {
                ctx.drops.register_local(result_id, ret_type, &ctx.type_registry);
            }
            ctx.set_owned(builder, result_id);

            // Track collection provenance for Option__Ref_ results.
            // Case A: named-local receiver → `Local(recv)`.
            // Case B: field-access receiver with NO recv temp → `FieldPath(...)`.
            // Case C: anon recv temp + field_path — ACTIVATED. Safe now that
            //   (a) save/restore covers local_ownership, (b) restore clears
            //   branch-local CollectionRef/CowBorrow entries, (c) f-string deref
            //   emits a deep clone for resource-containing struct types instead
            //   of a shallow memcpy, (d) prescan walks every path ancestor.
            //
            // The `is_named_local` guard on `recv_local` is GENUINELY required.
            // Probed 2026-05-04: dropping it (recording Local(unnamed_temp) for
            // anonymous recv temps) breaks `self.field.get(i).unwrap()` chains
            // — the unnamed temp aliases the field-load result and downstream
            // mutation tracking has no way to map back to the actual collection.
            // Fixtures regressed: heap_advanced (String heap pop returns same
            // value 3x), vector_task_get, and others. The FieldPath fallback at
            // the `else if` arm is the correct path for unnamed receivers
            // because it carries the structural identity of the collection.
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
            // G3: collection `.clone()` (gorget_array_clone/map_clone/set_clone)
            // dispatches here through the generic builtin path — tag the emitted
            // clone Call `ExplicitUserClone` so the clone-reason validator sees
            // it, keyed on the typed `is_clone` decl flag (not the resolved name).
            let dst = if ctx.builtin_method_is_clone(&type_name, method_name) {
                ctx.call_tracked_clone(builder, call_name, call_args, ret_type, crate::ir::ImplicitCloneReason::ExplicitUserClone)
            } else {
                ctx.call_tracked(builder, call_name, call_args, ret_type)
            };
            // Trivial getter clone elision: result is Ptr(T) — mark as CowBorrow
            // so the caller sees a zero-cost borrow with collection provenance.
            // Trivial-getter / Option__Ref__ provenance tracking. The
            // `is_named_local` guard on `recv_local` is GENUINELY required —
            // see the parallel block above for rationale and the 2026-05-04
            // probe outcome. The FieldPath fallback handles unnamed receivers
            // (e.g. `self.field.get(i)`); recording Local(unnamed_temp) breaks
            // downstream cow_before_mutation routing because the temp carries
            // no live notion of the collection's identity.
            if ctx.trivial_getter_methods.contains(sig_name.as_str()) {
                ctx.set_cow_borrow(builder, dst);
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
        // The combinator's inline C code shallow-copies the payload for the
        // result; MoveZero transfers ownership to the returned value.
        //
        // Tier 1c: gated on last-use. With Option/Result now Resource, the
        // pre-Tier-1c default of "always MoveZero" trips when user code
        // reuses the receiver (e.g. `ok.or(alt)` followed by `ok.map(...)`)
        // — the second `.map(...)` reads a zeroed slot. When recv is NOT
        // at last use, we skip the MoveZero AND emit a Clone before the
        // call so the original `recv` and the result don't alias the same
        // heap data (which would double-free at scope exit).
        if !has_consuming_self {
            if let Some(recv_local) = recv_local_for_move_zero {
                // Read typed `enum_category` (Phase A) — Option/Result detection.
                let is_option_result = ctx.type_registry.get_type_def(&type_name)
                    .and_then(|td| td.metadata.enum_category)
                    .is_some();
                // Typed combinator_kind (Round XV Track D) — D3 post-call MoveZero gate.
                let is_combinator = ctx.builtin_combinator_kind(&type_name, method_name).is_some();
                if is_option_result && is_combinator
                    && ctx.type_registry.is_resource_type(builder.local_type(recv_local))
                    && !ctx.drops.is_moved(recv_local)
                {
                    let is_last_use = builder.local_name(recv_local)
                        .map(|n| ctx.is_last_use_at(n, receiver.span))
                        .unwrap_or(true); // unnamed temps: always last-use
                    if is_last_use {
                        // Move-if-dead: combinator consumes the receiver.
                        ctx.drops.unregister(recv_local);
                        ctx.move_zero_and_mark(builder, recv_local);
                    }
                    // else: recv is live past this call. Skip MoveZero.
                    // The C inline combinator returns a Result that
                    // aliases recv's payload; to keep recv valid AND
                    // avoid double-free, the GIR should have emitted a
                    // pre-call Clone of recv. This is the
                    // clone-before-non-adapter-combinator pattern; see
                    // the matching pre-call clone emission below in the
                    // arg setup.
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

        // Drain pending_move_zeros from lower_call_arg. These were borrowed
        // (borrow_mut) for the callee; now that the call has returned, zero
        // the source to prevent double-free at scope exit.
        let pending: Vec<LocalId> = ctx.func_state.pending_move_zeros.drain(move_zero_baseline..).collect();
        for local in pending {
            builder.move_zero(Place::local(local));
            ctx.drops.mark_moved(local);
        }

        // Drop owning temporaries materialized as borrow-arguments for THIS
        // method call (temporary lifetime ends after the call).
        let temp_drops: Vec<LocalId> = ctx.func_state.pending_temp_drops.drain(temp_drop_baseline..).collect();
        for local in temp_drops {
            // Unconditional: a bare-borrow callee never moves the temp.
            builder.drop(Place::local(local));
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
                    // Tag every view-returning method result as
                    // LocalOwnership::View, regardless of whether the
                    // local is named. The unnamed-temp path used to
                    // require a separate `view_returning_temps` sidecar
                    // because cow_materialize_view's clone-to-owned step
                    // shallow-copied the cloned local — fixed in
                    // cow_materialize_view by switching to AssignMode::Move
                    // (matches its sibling cow_materialize_alias).
                    if let Some(recv_local) = recv_local_for_move_zero {
                        ctx.set_view_of(builder, result_local, recv_local);
                    }
                    ctx.func_state.has_string_borrows = true;
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



/// Core #6 Edit-A pin: destructive enum extract must never target a Ptr-typed
/// `scrut_local`. The adapter unwraps `Ptr(Option/Result)` → value type before
/// allocating the slot; a regression re-opens receiver-emptying / wrong-layout
/// extraction (Round XIV Edit A). Centralized so every `enum_field_load_move`
/// site shares one assertion (Core #4 sibling discipline).
fn assert_scrut_is_value_enum(
    ctx: &LoweringContext,
    builder: &FunctionBuilder,
    scrut_local: LocalId,
) {
    let scrut_ty = builder.local_type(scrut_local);
    debug_assert!(
        !matches!(
            ctx.type_registry.get(scrut_ty),
            Some(GirType::Ptr(_)) | Some(GirType::MutPtr(_))
        ),
        "try_lower_option_result_combinator: enum_field_load_move on Ptr-typed scrut_local (Edit A regression) — unwrap Ptr(Option/Result) before allocating scrut_local"
    );
}

/// Round XXII β chokepoint (Core #3 birth-registration + Core #4 producer):
/// destructively extract an owned payload from `scrut_local`'s enum variant.
///
/// Every combinator adapter branch that pulls out a Some/Ok/Error payload
/// routes through here so the payload's ownership is registered at its
/// BIRTH — not at its first Move (Core #3). Pre-fix, the 5 open-coded
/// duplications of this 4-line pattern (`assert + enum_field_load_move +
/// set_owned + move_zero(scrut)`) all skipped `drops.register_local`, so
/// arms whose closure only BORROWED the payload (`map` / `flat_map` /
/// `and_then` / `map_err` / `unwrap_or_else`-Result-Error with a
/// mapped-away return type) leaked the payload's heap bytes on the
/// happy path — the payload had no owner registered, no scope-exit
/// `DropIfAlive` fired, ASan flagged 40B (headline: 3B on
/// `combinator_map_string_to_int_param.gg`, 608B/6 objs on
/// `combinator_map_money_param_and_field.gg`; scout
/// `/tmp/round_xxii_trackBeta_scout_79000.md`).
///
/// Sibling arms that ALIAS payload into `result_local` (or_else Some,
/// filter Some, unwrap_or_else Some, map_err Ok) get paired at their
/// wrap site (Core #4): the wrap Move-consumes the payload and emits
/// `move_zero(payload)` afterwards so scope-exit `DropIfAlive` sees a
/// zeroed slot and skips — the LIR does NOT auto-zero the source slot
/// on a Move operand (verified against `src/lir/lower/operands.rs:157,
/// 175`; both operand variants share the same load path), so the
/// explicit `move_zero` is load-bearing (skipping it ships a double-free).
///
/// `register_local` self-gates on `needs_drop(field_type, registry)`, so
/// this is a no-op for trivial-copy payload types (int, bool, …).
fn extract_enum_payload_owned(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrut_local: LocalId,
    variant: &str,
    field_type: TypeId,
) -> LocalId {
    assert_scrut_is_value_enum(ctx, builder, scrut_local);
    let payload = builder.enum_field_load_move(Place::local(scrut_local), variant, 0, field_type);
    // Tier 2a Phase 2B: tag Owned before zeroing scrut so tag_ownership
    // infers Owned for payload (was: open-coded at every extraction site).
    ctx.set_owned(builder, payload);
    builder.move_zero(Place::local(scrut_local));
    // Core #3: register at birth. Downstream aliasing wraps pair with
    // `mov(payload) + move_zero(payload)` to avoid double-free; borrow-only
    // arms (map/flat_map/and_then closures) rely on this scope-exit drop
    // to free the extracted resource.
    ctx.drops.register_local(payload, field_type, &ctx.type_registry);
    payload
}

fn try_lower_option_result_combinator(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_name: &str,
    method_name: &str,
    recv: Operand,
    args: &[Spanned<ast::CallArg>],
) -> Option<Operand> {
    // Read typed `enum_category` (Phase A) — Option/Result discriminator.
    use crate::ir::types::EnumCategory;
    let cat = ctx.type_registry.get_type_def(type_name)
        .and_then(|td| td.metadata.enum_category);
    let is_option = cat == Some(EnumCategory::Option);
    let is_result = cat == Some(EnumCategory::Result);
    if !is_option && !is_result { return None; }
    if args.is_empty() { return None; }

    // Resolve the receiver's TypeId and the result type for the combinator.
    // If recv is Ptr(T) (a bare-borrow parameter), unwrap to the VALUE type —
    // the scrut_local + all extraction ops need an owned T, not a T*.
    let raw_recv_type = infer_operand_type_full(ctx, &recv, builder);
    let recv_type = match ctx.type_registry.get(raw_recv_type) {
        Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
        _ => raw_recv_type,
    };
    let recv_is_ptr = recv_type != raw_recv_type;

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

    // String payloads use the GIR adapter (Tier 1c map_err precedent;
    // Round XV Track B retired the remaining `has_string_coercion` bail for
    // map/filter/and_then/flat_map/unwrap_or_else/or_else). The old bail
    // forced the C-inline path while GIR still typed the result local as the
    // *receiver* type (Option[GorgetString] over Option[int]), causing
    // size-mismatched memcpy + free-panic. Adapter owns String now.

    // Store receiver in a local for field extraction.
    //
    // Tier 1c: now that Option/Result are Resource, the Copy default
    // would create a shallow alias with the receiver — both recv and
    // scrut_local would drop the same heap data. Two options:
    // (a) Move + MoveZero recv — only safe when recv is at last use
    //     (`ok.or(...)` followed by `ok.map(...)` reuses `ok`, so
    //     Move would trip the "read after MoveZero" validator);
    // (b) Clone — emit a deep-clone fn call producing an independently
    //     owned copy. Always safe regardless of liveness; costs one
    //     extra allocation per adapter call when recv is at last use.
    // We use (b) for now. Last-use liveness analysis can later refine
    // this to pick Move at last-use sites.
    let scrut_local = builder.add_local(recv_type, None);
    // Edit B: materialize place receivers via a Ptr into the clone path.
    // Live cases at this adapter are bare places and already-Ptr params —
    // projected places are materialised to empty-proj temps by lower_expr
    // / field_load before the adapter (Round XVI F2 scout-proved).
    // Previously only a bare, non-projected place with a registered
    // clone_fn was cloned; Ptr recvs fell through to plain Copy, creating
    // a shallow alias that enum_field_load_move then emptied — corrupting
    // caller storage on map/filter/or_else (Core #1: fix at the write site).
    //
    // Build a Ptr(recv_type) that points at the receiver's storage; then
    // either clone through the ptr (deep, safe) or load-then-copy
    // (fallback when no clone fn exists).
    let ptr_local_opt: Option<LocalId> = if let Operand::Copy(ref p) | Operand::Move(ref p) = recv {
        debug_assert!(
            p.projections.is_empty(),
            "combinator adapter: projected place recv unreachable (lower_expr materializes); \
             if this fires, restore emit_borrow(projected) or fix the producer"
        );
        let ptr_type = ctx.register_ptr_type(recv_type);
        let ptr_local = builder.add_local(ptr_type, None);
        if recv_is_ptr {
            // recv is already a Ptr(T) value — pass the pointer through.
            builder.assign(Place::local(ptr_local), FunctionBuilder::copy(p.local));
        } else {
            // Bare place — borrow into a Ptr(T).
            builder.emit_borrow(ptr_local, Place::local(p.local));
        }
        Some(ptr_local)
    } else {
        None
    };
    if let (Some(ptr_local), Some(clone_fn)) = (ptr_local_opt, ctx.clone_fn_for_ptr(recv_type)) {
        // `args` is non-empty here (guarded above); use the closure arg's
        // span as the diagnostic site for this combinator-receiver clone.
        ctx.warn_clone_and_hit(builder, args[0].span, recv_type, crate::ir::ImplicitCloneReason::CallArg);
        let cloned = builder.call_clone(
            &clone_fn,
            vec![FunctionBuilder::copy(ptr_local)],
            recv_type,
            crate::ir::ImplicitCloneReason::CallArg,
        );
        ctx.set_owned(builder, cloned);
        builder.assign_mode(
            crate::ir::instructions::AssignMode::Move,
            Place::local(scrut_local),
            FunctionBuilder::copy(cloned),
        );
        builder.move_zero(Place::local(cloned));
    } else if let Some(ptr_local) = ptr_local_opt {
        // No clone fn — load through the ptr into scrut_local. Downstream may
        // still be unsound for a resource type, but this is no worse than the
        // previous plain-Copy fallback and preserves the pre-fix behavior for
        // trivial-copy receivers.
        //
        // Core #14 guard (REV-P1 RSV-5): this fallback is only sound when
        // recv_type is trivial-copy. If a resource type reaches here, upstream
        // has failed to register a clone_fn — the class-fix's invariant is
        // that any destructive receiver-extraction happens on an OWNED
        // scrut_local, and load_ref of a resource is not that. debug_assert!
        // so the class-retirement guard (Core #6 follow-up) has a concrete
        // enforcement point to graduate.
        debug_assert!(
            !ctx.type_registry.is_resource_type(recv_type),
            "combinator fallback path (load_ref, no clone_fn) reached for resource type {:?} — clone_fn registration missing at upstream write site",
            recv_type
        );
        let loaded = builder.load_ref(Place::local(ptr_local), recv_type);
        builder.assign(Place::local(scrut_local), FunctionBuilder::copy(loaded));
    } else {
        // Non-place operand (constants) — Copy is safe.
        builder.assign(Place::local(scrut_local), recv.clone());
    }

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

    // Set expected_type for the closure body so bare Ok/Error/Some/None
    // constructors resolve to a full wrapper (not int64_t fallback).
    //
    // and_then and or_else are CROSS-TYPE capable
    //   (Option[T]→Option[U] / Result[T,E]→Result[U,E] for and_then;
    //    Option[T]→Option[T] / Result[T,E]→Result[T,F] for or_else).
    // Always-forcing recv made Result Ok(u) build Result[T,E] with a size-
    // mismatched Ok payload (Track B silent-wrong; Round XXIII Track α:
    // same class for cross-type or_else where a call-body closure returned
    // a differently-sized Result). Never-forcing broke bare `None()` in
    // same-type Option chains (`test_option_chaining` → mangled
    // `int64_t__and_then`). Dual rule (mirrors SH-3 RSV-1):
    //   and_then / or_else: if outer expected is Option/Result → keep outer
    //                       (annotated cross-type); else → recv (bare
    //                       None/Ok same-type chains).
    //   flat_map: force recv (Option-only; the type is Option[U] which is
    //             legitimately cross-type but the expected-type hint here
    //             only affects bare-literal-body coercion, and same-type
    //             chains without an outer annotation still need the recv
    //             hint for `Some`/`None` resolution).
    let prev_expected = ctx.func_state.expected_type;
    if method_name == "flat_map" {
        if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
            ctx.func_state.expected_type = Some(type_id);
        }
    } else if matches!(method_name, "and_then" | "or_else") {
        let outer_is_opt_res = prev_expected
            .and_then(|tid| ctx.type_registry.enum_category(tid))
            .is_some();
        if !outer_is_opt_res {
            if let Some(type_id) = ctx.lookup_type_by_name(type_name) {
                ctx.func_state.expected_type = Some(type_id);
            }
        }
        // outer_is_opt_res: leave prev_expected (annotated Result[T,F]/Option[T])
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
                        // Coherence-at-construction (Tier 1c): pass the registry
                        // so payload drop-strategy propagates into the wrapper's
                        // metadata at registration. Bypass get_or_register since
                        // its `FnOnce(&str)` closure shape can't carry the
                        // additional borrow.
                        let td = make_result_type_def(&result_name, mapped_ret, none_err_type, &ctx.type_registry);
                        ctx.type_registry.add_type_def(td);
                        let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(result_name.clone()));
                        ctx.type_mapper.register_named(result_name.clone(), tid);
                    }
                    ctx.lookup_type_by_name(&result_name).unwrap_or(recv_type)
                }
            } else {
                recv_type
            }
        }
        "and_then" | "flat_map" | "or_else" => {
            // Closure returns Option[U] or Result[U, E] — that IS the result type.
            // Previously (before Round XXIII Track α, `or_else` was in this arm's
            // fall-through `_ => recv_type` branch) Option[Money]→Option[int]
            // produced an Option[Money]-typed result_local that the closure's
            // Option[int] return got Move-assigned into — LLVM verifier caught
            // the i64-vs-ptr phi; the C backend silently emitted the memcpy and
            // dereffed an int as a Vector[int] handle → SIGSEGV.
            // (Core #1 write-site fix: the wrong type was chosen when result_local
            // was allocated, not at the assign.)
            //
            // Track α extended this arm to `or_else`: cross-type `.or_else`
            // (Result[T,E]→Result[T,E'] and Option[T]→Option[T]) previously
            // SBOd on both branches at the merge memcpy — the pre-Track-α
            // `_ => recv_type` fall-through mis-sized `result_local` at birth
            // and downstream memcpys read/wrote the wrong number of bytes.
            // Under the new sigil semantics or_else is the same closure-return
            // shape as and_then/flat_map: the closure DECLARES the result type;
            // the typechecker (`unify_closure_ret_axis`) enforces the axis rule
            // (Ok-unify for Result.or_else, Error-unify for Result.and_then,
            // Some-unify for Option.or_else) so the ill-typed shapes that used
            // to reach here get rejected at check time.
            //
            // Class registry — every closure-returning-Result/Option combinator
            // arm belongs here. `tests/lints.rs::unify_closure_ret_axis_class_enumeration`
            // ratchets the exhaustive list (axis-unify cell count in
            // `ClosureCombinatorCell` + call-site count + a superset
            // `EXPECTED_BUILTIN_REGISTRATIONS` scan of `combinator_kind: Some(...)`
            // entries in `src/ir/lowering/builtins.rs`) so the next combinator
            // addition cannot silently escape.
            let closure_ret = infer_closure_return_type(ctx, &closure_op, builder);
            if closure_ret != UNIT_TYPE {
                closure_ret
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
                    // Coherence-at-construction (Tier 1c): see twin site above.
                    let td = make_result_type_def(&result_name, some_ok_type, mapped_ret, &ctx.type_registry);
                    ctx.type_registry.add_type_def(td);
                    let tid = ctx.type_registry.insert(crate::ir::types::GirType::Named(result_name.clone()));
                    ctx.type_mapper.register_named(result_name.clone(), tid);
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

    // Helper: Move-mode assign + MoveZero on the source local.
    // Every site in the per-branch switch below assigns a freshly-built
    // Owned local (`wrapped`, `some_val`, `none_val`, or a closure call
    // `result`) into `result_local`. The Copy default would create a
    // shallow alias — both source and result_local would drop the
    // same heap data once Option/Result became Resource via Tier 1c.
    //
    // We use Move mode WITHOUT an explicit MoveZero: the LIR's Move
    // semantic at consume sites already zeros the source slot
    // (consume-site contract). An additional GIR-level `move_zero`
    // after the Move-mode assign would zero the source TWICE, which
    // for `enum_init`-built locals containing String/Vector payloads
    // corrupts the payload data (visible as `"BAD"` → `"B"` in
    // coroutine_result_combinators where the closure returns a 3-byte
    // String through a map_err's Error wrap).
    fn assign_result_local_move(builder: &mut FunctionBuilder, result_local: LocalId, src: Operand) {
        use crate::ir::instructions::AssignMode;
        builder.assign_mode(AssignMode::Move, Place::local(result_local), src);
    }

    // === Some/Ok branch ===
    builder.switch_to(some_bb);
    // Round XXII β: route extraction through the shared helper (Core #4
    // producer) so payload is registered at birth (Core #3). Aliasing wraps
    // below (or_else / filter / unwrap_or_else Some, map_err Ok) pair with
    // `mov(payload) + move_zero(payload)` at the wrap site to avoid
    // double-free; borrow-only arms (map / flat_map / and_then) rely on
    // this scope-exit drop to free the extracted resource.
    let payload = extract_enum_payload_owned(
        ctx,
        builder,
        scrut_local,
        if is_option { "Some" } else { "Ok" },
        some_ok_type,
    );

    match method_name {
        "map" => {
            // map(fn) → Some/Ok(fn(payload))
            let mapped = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(payload)], some_ok_type);
            let wrapped = builder.enum_init(&result_type_name, if is_option { "Some" } else { "Ok" }, result_type, vec![mapped]);
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(wrapped));
        }
        "and_then" | "flat_map" => {
            // and_then(fn) → fn(payload) (fn returns Option/Result)
            let result = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(payload)], result_type);
            assign_result_local_move(builder, result_local, result);
        }
        "or_else" => {
            // or_else: Some/Ok path → keep original.
            // Round XXII β pairing (b1): Move-consume payload into the wrap
            // and zero its slot so scope-exit `DropIfAlive(payload)` sees a
            // zeroed slot and skips — otherwise result_local (which now owns
            // payload's bytes via the alias) AND payload's registered drop
            // would both free the same allocation. The LIR does NOT zero the
            // source slot on a Move operand (operands.rs:157,175 handle Copy
            // and Move identically at the value layer), so the explicit
            // `move_zero(payload)` is load-bearing.
            let wrapped = builder.enum_init(&result_type_name, if is_option { "Some" } else { "Ok" }, result_type, vec![FunctionBuilder::mov(payload)]);
            builder.move_zero(Place::local(payload));
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(wrapped));
        }
        "filter" if is_option => {
            // filter(fn) → if fn(payload): Some(payload) else: None
            let pred = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(payload)], BOOL_TYPE);
            let filter_then = builder.new_block();
            let filter_else = builder.new_block();
            builder.branch(pred, filter_then, filter_else);
            builder.switch_to(filter_then);
            // Round XXII β pairing (b1, MANDATED at this CFG-conditional
            // site): Move-consume payload into Some(...) and zero its slot.
            // b2 (drops.unregister(payload)) is UNSAFE here — `unregister`
            // (drops.rs:300-307) is not CFG-aware and would remove the entry
            // globally, leaving the filter_else path with payload's
            // registered-at-birth bytes NEVER freed → LEAK on the else path.
            // b1 keeps the registration; only the consuming filter_then
            // path zeros the slot (DropIfAlive is a no-op on zero), while
            // the else path's untouched-payload slot drops normally.
            let some_val = builder.enum_init(&result_type_name, "Some", result_type, vec![FunctionBuilder::mov(payload)]);
            builder.move_zero(Place::local(payload));
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(some_val));
            builder.jump(merge_bb);
            builder.switch_to(filter_else);
            let none_val = builder.enum_init(&result_type_name, "None", result_type, vec![]);
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(none_val));
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
            // Round XXII β pairing (b1-form, adapted to the no-enum_init
            // shape): direct assign uses `mov(payload)` + `move_zero(payload)`
            // — same rationale as :3817 or_else, without the enum_init wrap.
            assign_result_local_move(builder, result_local, FunctionBuilder::mov(payload));
            builder.move_zero(Place::local(payload));
        }
        "map_err" if is_result => {
            // map_err: Ok path → keep original Ok.
            // Round XXII β pairing (b1): same shape as :3817 or_else.
            let wrapped = builder.enum_init(&result_type_name, "Ok", result_type, vec![FunctionBuilder::mov(payload)]);
            builder.move_zero(Place::local(payload));
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(wrapped));
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
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(none_val));
        }
        "or_else" if is_option => {
            // or_else: None → fn()
            let result = call_closure_in_adapter(ctx, builder, &closure_op, vec![], result_type);
            assign_result_local_move(builder, result_local, result);
        }
        "unwrap_or_else" if is_option => {
            // unwrap_or_else: None → fn()
            // SCOUT: for a resource payload, the closure's return value must be
            // tagged Owned before it flows into result_local (Tier 2a). Without
            // this, the closure-call result dst is Untracked and the AssignInto-
            // OwnedSlot validator panics (`ICE at mod.rs:2127`).
            let result = call_closure_in_adapter(ctx, builder, &closure_op, vec![], some_ok_type);
            if let Operand::Copy(ref p) | Operand::Move(ref p) = result {
                if p.projections.is_empty() {
                    ctx.set_owned(builder, p.local);
                }
            }
            assign_result_local_move(builder, result_local, result);
        }
        "map" | "and_then" | "flat_map" if is_result => {
            // Error → Error(err). err_val extraction routes through the
            // shared helper (Core #4 producer) — same shape as the Some/Ok
            // extraction above. `emit_enum_init_owned` auto-transfers
            // err_val's registration via its post-init unregister loop
            // (`context.rs:1786-1803`; err_val is Owned+unnamed so
            // `clone_resource_args_for_init` skips clone at :1848-1852 via
            // the `is_owned_local && !is_named_local` continue), so no
            // extra pairing at this err_val site.
            let err_val = extract_enum_payload_owned(ctx, builder, scrut_local, "Error", none_err_type);
            let wrapped = ctx.emit_enum_init_owned(builder, &result_type_name, "Error", result_type, vec![FunctionBuilder::copy(err_val)], None);
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(wrapped));
        }
        "or_else" if is_result => {
            // or_else: Error → fn(err). err_val is only BORROWED into the
            // closure via `FunctionBuilder::copy(err_val)` — the slot stays
            // registered and scope-exit `DropIfAlive(err_val)` frees it.
            // No extra pairing at this err_val site.
            let err_val = extract_enum_payload_owned(ctx, builder, scrut_local, "Error", none_err_type);
            let result = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(err_val)], result_type);
            assign_result_local_move(builder, result_local, result);
        }
        "unwrap_or_else" if is_result => {
            // unwrap_or_else: Error → fn(err). Same err_val borrow-into-
            // closure shape as or_else Error; no extra pairing.
            let err_val = extract_enum_payload_owned(ctx, builder, scrut_local, "Error", none_err_type);
            let result = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(err_val)], some_ok_type);
            assign_result_local_move(builder, result_local, result);
        }
        "map_err" if is_result => {
            // map_err: Error → Error(fn(err)). err_val is borrowed into the
            // closure; the closure's return `mapped` flows into
            // `emit_enum_init_owned` (which handles ownership). err_val's
            // slot stays registered and scope-exit `DropIfAlive` frees it.
            let err_val = extract_enum_payload_owned(ctx, builder, scrut_local, "Error", none_err_type);
            let mapped = call_closure_in_adapter(ctx, builder, &closure_op,
                vec![FunctionBuilder::copy(err_val)], none_err_type);
            let wrapped = ctx.emit_enum_init_owned(builder, &result_type_name, "Error", result_type, vec![mapped], None);
            assign_result_local_move(builder, result_local, FunctionBuilder::copy(wrapped));
        }
        _ => return None,
    }
    builder.jump(merge_bb);

    // === Merge ===
    builder.switch_to(merge_bb);
    // Cluster 2 stylistic cleanup (2026-05-07): the original disjunction
    // `needs_drop || is_resource_type` was redundant — `needs_drop` already
    // returns true for every type that owns heap (Resource copy semantics
    // OR non-None drop strategy), which subsumes the narrow `is_resource_type`
    // predicate. See `src/ir/types.rs` Phase 1 audit notes.
    if ctx.type_registry.needs_drop(result_type) {
        ctx.drops.register_local(result_local, result_type, &ctx.type_registry);
    }
    ctx.set_owned(builder, result_local);

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
    // Clear `expected_type` for the object and index sub-expressions — the
    // surrounding destination type (e.g. the function's return slot when
    // this index expression appears in `return v[i]`) describes the index
    // *result*, not the object or the index. Without this, `lower_expr`'s
    // centralized Result→T auto-prop hook would see a leaked `Result[_,_]`
    // destination and skip the unwrap on a throws-fn-returning index like
    // `v[throws_fn()]` — leaving the index slot holding the bytes of the
    // Result struct rather than an int.
    let saved_expected = ctx.func_state.expected_type.take();
    let mut obj = lower_expr(ctx, builder, object);
    let idx = lower_expr(ctx, builder, index);
    ctx.func_state.expected_type = saved_expected;

    // A module-level `static` lowers to Operand::Constant(GlobalRef) (see
    // exprs/mod.rs), which the place-guard below would not match — falling
    // through to Constant::Unit and silently dropping the read (the whole
    // `TABLE[i].field` index-read const-folded to 0). Materialize the
    // GlobalRef into a local so the place path emits the real index_load.
    // Resource collections (Vector/Dict) Borrow (zero-cost; the global
    // retains ownership, freed once at static teardown → no double-free);
    // value types Copy. Mirrors `init_borrow_iter_local` (for_loops.rs).
    if let Operand::Constant(Constant::GlobalRef(_)) = obj {
        let base_type = infer_operand_type_full(ctx, &obj, builder);
        let local = builder.add_local(base_type, None);
        let mode = if ctx.type_registry.is_resource_type(base_type) {
            AssignMode::Borrow
        } else {
            AssignMode::Copy
        };
        builder.assign_mode(mode, Place::local(local), obj);
        obj = Operand::Copy(Place::local(local));
    }

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        // Lazy loop-carried CoW, hook W3c (INDEX/SLICE base): `s[i]` /
        // `s[a..b]` lower to gorget_str_index / gorget_str_slice — cap=0
        // views into the base's buffer. This route never consults
        // `returns_view`, so the result carries NO View tag and even a NAMED
        // bind (`String t = s[0..5]`) would dangle once the source
        // collection mutates. If the base is a lazy-tagged local,
        // materialize it in place BEFORE the read captures the buffer.
        ctx.materialize_lazy_source_if_needed(builder, &obj, object.span);
        // Infer element type from the base collection type
        let base_type = infer_operand_type_full(ctx, &obj, builder);

        // Check if the type has a get() equip method (Index trait / operator overload)
        // Skip for built-in collection types — use direct index_load instead
        // (Vector.get() returns Option[T] but v[i] returns T directly)
        if let Some(type_name) = infer_type_name_from_operand_full(ctx, &obj, builder) {
            // Read typed `collection_kind` (Phase A) — covers Array (Vector/
            // Deque/GorgetArray), OrderedMap/Map (Dict/HashMap), OrderedSet
            // (Set). Skipping HashSet here matches the original arm set.
            let is_builtin_collection = matches!(
                ctx.type_registry.get_type_def(&type_name)
                    .and_then(|td| td.metadata.collection_kind),
                Some(crate::ir::types::CollectionKind::Array)
                | Some(crate::ir::types::CollectionKind::OrderedMap)
                | Some(crate::ir::types::CollectionKind::Map)
                | Some(crate::ir::types::CollectionKind::OrderedSet)
            );
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
        // String character indexing: returns a cap=0 VIEW REGION into the
        // base's buffer (gorget_str_index → gorget_str_view_region), not an
        // owned copy — collection-put materialize hooks / boundary clones own
        // it when it escapes; the W3c hook above handles lazy-view bases.
        let is_string_base = ctx.type_mapper.is_string_type(resolved_base);
        // Range slicing (`v[a..b]`, `s[a..b]`) returns a fresh container
        // of the same type as the base, NOT an element. Detect via the
        // index's runtime type (`GorgetRange`) and use the base type so
        // the GIR slot is sized correctly. Without this, the dst was
        // typed `elem_type` (e.g. `i64` for a `Vector[int]` slice),
        // causing a Tier 2a `AssignIntoOwnedSlot` violation at the
        // downstream `[Mv] _result = copy _slice_dst` and a
        // structural type mismatch (the GIR slot was sized for one
        // element, but the LIR rewrites the call to
        // `gorget_array_slice` which fills a full container).
        let idx_type = infer_operand_type_full(ctx, &idx, builder);
        let is_range_index = matches!(
            ctx.type_name_for_id(idx_type),
            Some(n) if n == "GorgetRange"
        );
        let result_type = if is_range_index && (is_string_base || ctx.type_registry.is_collection_type(resolved_base)) {
            // Slice returns the same container type as the base.
            resolved_base
        } else if is_task || is_string_base {
            elem_type
        } else if ctx.type_registry.is_resource_type(elem_type) {
            ctx.register_ptr_type(elem_type)
        } else {
            elem_type
        };
        // Fault-`catch` routing for `Fault.Bounds` (error-model.md §11,
        // Increment 2): inside an active fault scope whose `bounds_handler` is
        // set, an out-of-bounds ARRAY element read BRANCHES to the handler
        // instead of panicking. Gated to array element READS only — the sole
        // path with a runtime bounds check (`gorget_array_safe_get`); range
        // slices, string indexing, and dict-get are OUT (different runtime fns,
        // some with no `safe_*` variant). Typed gate (collection_kind + not a
        // range/string base), never a name check.
        let bounds_handler = bounds_handler_for(ctx);
        let base_type_name = ctx.type_name_for_id(resolved_base).unwrap_or_default().to_string();
        let base_is_array = ctx.type_registry.get_type_def(&base_type_name)
            .and_then(|td| td.metadata.collection_kind)
            == Some(crate::ir::types::CollectionKind::Array);
        let mut is_faultable_clone = false;
        let dst = if let Some(handler) = bounds_handler {
            if base_is_array && !is_range_index && !is_string_base {
                let read = crate::ir::instructions::ReadMode::Clone;
                is_faultable_clone = true;
                builder.index_load_faultable(place.clone(), idx, result_type, read, handler)
            } else {
                builder.index_load(place.clone(), idx, result_type)
            }
        } else {
            builder.index_load(place.clone(), idx, result_type)
        };
        // A CollectionRef tags `dst` as a LIVE borrow into the base collection so
        // a later `base.push(...)` triggers `cow_before_mutation` to materialize
        // the borrow before the collection reallocates. That is correct for a
        // plain `ReadMode::Borrow` index (the dst genuinely aliases the element).
        //
        // It is WRONG for the faultable `Fault.Bounds` read (`ReadMode::Clone`):
        //   (a) the no-fault path already materializes the element to an OWNED
        //       value, and the enclosing fault-catch's `ensure_owned_at_boundary`
        //       ensures the catch result escapes owned — nothing holds a live
        //       borrow into the collection past the catch; and
        //   (b) on the out-of-bounds path the dst is NULL (the safe_get returned
        //       NULL and control branched to the handler). A stale CollectionRef
        //       on that NULL dst makes `cow_before_mutation` at a later
        //       `base.push(...)` clone NULL → NULL-deref crash in
        //       `gorget_string_clone_to_owned` (Core #8, both backends).
        // So skip the CollectionRef registration for the faultable-clone dst.
        if ctx.type_registry.is_resource_type(elem_type) && !is_task && !is_string_base
            && !is_faultable_clone
        {
            // Use FieldPath provenance when the base is a field access (e.g., s.v[0]).
            // This ensures cow_before_field_mutation("s.v") finds the ref when
            // s.v.push(x) is called later. Without this, the ref is keyed on the
            // FieldLoad temp LocalId, which cow_before_field_mutation can't find.
            let collection_id = extract_field_path_string(&object.node)
                .map(CollectionId::FieldPath)
                .unwrap_or_else(|| CollectionId::Local(place.local));
            ctx.set_collection_ref(builder, dst, collection_id);
        }
        return FunctionBuilder::copy(dst);
    }

    Operand::Constant(Constant::Unit)
}

/// The active fault scope's `bounds_handler`, if any — the GIR block an
/// out-of-bounds array index read branches to instead of panicking
/// (`Fault.Bounds`, error-model.md §11). `None` when no fault scope is active
/// or the scope doesn't catch `Bounds`. Typed read off `FaultScope`, never a
/// name check (mirrors `operators::fault_handler_for`).
fn bounds_handler_for(ctx: &LoweringContext) -> Option<crate::ir::types::BlockId> {
    ctx.func_state.fault_scope?.bounds_handler
}

/// Phase A residual #1: a mangled type-name fragment is a Callable family
/// alias of the runtime GorgetClosure layout. Reads `c_runtime_alias`
/// from the GIR TypeDef when present; falls back to a name-shape check for
/// the bootstrap path where no TypeDef has been registered yet (the
/// `register_callable_alias` helper hasn't run for this collection-elem
/// name). Returning `true` here means "treat the elem as a 16-byte
/// GorgetClosure handle, NOT a fresh user struct".
fn is_callable_alias_name(ctx: &LoweringContext, name: &str) -> bool {
    if let Some(td) = ctx.type_registry.get_type_def(name) {
        return td.metadata.c_runtime_alias.as_deref() == Some("GorgetClosure");
    }
    // Pre-registration fallback: Callable__T_args is mangled by
    // `mangle_generic_name`/`mangle_type_for_name` and the registration
    // happens lazily — at this site we may be the first to see the name.
    name == "GorgetClosure"
        || name.starts_with("Callable__")
        || name.starts_with("MutCallable__")
        || name.starts_with("ConsumeCallable__")
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
            if is_callable_alias_name(ctx, elem_name) {
                // TRACK K: previously constructed an EMPTY-params, I64-return
                // FnPtr — right size (16-byte GorgetClosure), wrong sig.
                // The read side at `calls.rs`'s non-identifier arm (line
                // ~2213) could not tell that `arr[0](&a)` had an `&int`
                // param, so it lowered `&a` as a plain value and the
                // callee's write-through segfaulted on both backends.
                // Recover the sig from the `callable_alias_sigs`
                // side-table (populated by `register_callable_alias`).
                if let Some((params, owns, ret)) = ctx.type_mapper.callable_alias_sigs.get(elem_name).cloned() {
                    return ctx.type_registry.insert(GirType::FnPtr {
                        params,
                        return_type: ret,
                        param_ownerships: owns,
                    });
                }
                return ctx.type_registry.insert(GirType::FnPtr {
                    params: vec![],
                    return_type: I64_TYPE,
                    param_ownerships: vec![],
                });
            }
            let elem_name = elem_name.to_string();
            return resolve_type_name_to_id(ctx, &elem_name);
        }
        // Deque__T → T is the element type (sibling of Vector__T; the LIR
        // layer at `insts.rs:1905` already strips `Deque__` alongside
        // `Vector__` — this is the missing IR-layer sibling that was
        // dropping `Deque[S]` element types to `I64_TYPE`, producing a
        // `gg check`-clean SIGSEGV (C) / llc-reject (LLVM) on `&d[i].fd`
        // and a `for s in d:` silent `0/0` miscompile.
        //
        // Grows the TODO.md:1063 rule-2 debt (`infer_collection_element_type`
        // name-strip): 3 prefixes → 5. Does NOT close the debt; the typed-
        // field-on-TypeDef fix remains queued as filed.
        if let Some(elem_name) = name.strip_prefix("Deque__") {
            let elem_name = elem_name.to_string();
            return resolve_type_name_to_id(ctx, &elem_name);
        }
        // Set__T / HashSet__T → T is the element type. Covers the
        // SIZE-DERIVATION face — the sole caller today is the empty-literal
        // path at `collections.rs:277` (`Set[T] s = {}` computes
        // `elem_size = infer_collection_element_type(ctx, Set__T)` and
        // hands it to `gorget_set_new(sizeof(elem))`). Without this arm the
        // fall-through to `I64_TYPE` produced a `gg check`-clean C-emit
        // failure ("incompatible types when assigning to type 'GorgetSet'
        // from type 'int32_t'") on `Set[P] s = {}` for struct P.
        //
        // Set/HashSet do NOT open the POSITIONAL-INDEX face — the kind-gate
        // at `mod.rs`'s `try_resolve_index_element_ptr` still excludes them
        // (Set has no positional index; `set_index_returns_garbage.gg` pins
        // the unsound check-time accept as a separate filed defect pending a
        // ggdef DESIGN DECISION). Also no Callable-alias signature-recovery
        // hoist (closures are not Hashable+Equatable, so they cannot be
        // Set/HashSet elements).
        if let Some(elem_name) = name.strip_prefix("Set__")
            .or_else(|| name.strip_prefix("HashSet__"))
        {
            let elem_name = elem_name.to_string();
            return resolve_type_name_to_id(ctx, &elem_name);
        }
        // Dict__K__V or HashMap__K__V → V is the value type (for indexing).
        // HashMap is the sibling missed at the IR layer; the LIR layer at
        // `insts.rs:1905`/method dispatchers all split `HashMap__` correctly.
        // Adding this arm ALSO admits `&hm[k]` at the kind-gate below
        // (change (b)) and retires the TODO.md:1064 `HashMap[int,Point].x`
        // silent-`0` read (same root, one edit).
        if let Some(rest) = name
            .strip_prefix("Dict__")
            .or_else(|| name.strip_prefix("Map__"))
            .or_else(|| name.strip_prefix("HashMap__"))
        {
            if let Some(pos) = rest.find("__") {
                let val_name = &rest[pos + 2..];
                // Callable value types → FnPtr TypeId so the local is declared as GorgetClosure
                if is_callable_alias_name(ctx, val_name) {
                    // TRACK K: same sig-recovery as the Vector arm above.
                    if let Some((params, owns, ret)) = ctx.type_mapper.callable_alias_sigs.get(val_name).cloned() {
                        return ctx.type_registry.insert(GirType::FnPtr {
                            params,
                            return_type: ret,
                            param_ownerships: owns,
                        });
                    }
                    return ctx.type_registry.insert(GirType::FnPtr { params: vec![], return_type: I64_TYPE, param_ownerships: vec![] });
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
            // Builder index is O(1) and authoritative for in-range LocalIds
            // (see `infer_operand_type_with_builder` for the rationale).
            // Fallback ctx scan handles closure-param sentinel IDs.
            let idx = place.local.0 as usize;
            if idx < builder.locals.len() {
                builder.locals[idx].type_id
            } else {
                let mut tid = None;
                for (_, (lid, local_tid)) in ctx.locals_iter() {
                    if *lid == place.local {
                        tid = Some(*local_tid);
                        break;
                    }
                }
                tid?
            }
        }
        Operand::Constant(c) => match c {
            Constant::Str(_) => return Some("GorgetString".to_string()),
            Constant::Bool(_) => return Some("bool".to_string()),
            Constant::I64(_) => return Some("int64_t".to_string()),
            Constant::F64(_) => return Some("double".to_string()),
            // Defense (snag #56): normalize user/legacy String type names so
            // method mangle hits `GorgetString__*` in the runtime map. Primary
            // fix is registering static StringType as "GorgetString" at the
            // write site in mod.rs; this catches any residual "str"/"String".
            Constant::GlobalRef(name) => {
                return ctx.global_type_names.get(name).map(|tn| match tn.as_str() {
                    "str" | "String" => "GorgetString".to_string(),
                    _ => tn.clone(),
                });
            }
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
    ctx.type_mapper.iter_named()
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
                // Even if the Named TypeId was inserted earlier, it may lack a
                // TypeDef. For Callable types that's the only place where
                // metadata-driven consumers (clone_fn_for_ptr, infer_drop_strategy,
                // …) can pick up drop / clone / c_runtime_alias instead of
                // falling back to name-prefix matching. Idempotent.
                if name == "GorgetClosure"
                    || name.starts_with("Callable__")
                    || name.starts_with("MutCallable__")
                    || name.starts_with("ConsumeCallable__")
                {
                    super::super::types::register_callable_alias(
                        &mut ctx.type_mapper, &mut ctx.type_registry, name,
                    );
                }
                return id;
            }
            // Callable / MutCallable / ConsumeCallable — register both Named TypeId
            // AND a TypeDef carrying protocol metadata (drop_fn, clone_fn,
            // c_runtime_alias = "GorgetClosure", …). Routes through the same
            // helper as `register_collection_alias` so consumers read TypeDef
            // metadata uniformly.
            if name.starts_with("Callable__")
                || name.starts_with("MutCallable__")
                || name.starts_with("ConsumeCallable__")
                || name == "GorgetClosure"
            {
                return super::super::types::register_callable_alias(
                    &mut ctx.type_mapper, &mut ctx.type_registry, name,
                );
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
///
/// Deque shares Vector's element-type-in-suffix mangling and its
/// element-typed HOF (each/map/filter/fold/reduce/…) + push_back/
/// push_front value-arg hints. Round XXVII Track B added the `Deque__`
/// arm (Core #4 sibling arm-add): pre-fix, `Deque[T].each((x): ...)` with
/// an UNTYPED closure param fell through to `None`, so the closure-
/// param type hint (see caller at methods.rs:2438) defaulted to
/// `I64_TYPE` and non-int Deque elements executed the body with a
/// wrong-typed parameter (runtime type-mismatch / garbage prints).
fn extract_elem_type_id_from_type_name(ctx: &LoweringContext, type_name: &str) -> Option<TypeId> {
    let elem_str = if let Some(rest) = type_name.strip_prefix("Vector__") {
        Some(rest)
    } else if let Some(rest) = type_name.strip_prefix("Deque__") {
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

