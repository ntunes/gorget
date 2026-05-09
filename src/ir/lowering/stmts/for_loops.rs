//! For-loop lowering: range, string, array, dict, set, iterable, enumerate variants.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{Block, Expr, Pattern};
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::super::drops::DropScopeKind;
use super::super::exprs::{lower_expr, infer_operand_type_full};
use super::{lower_block, lower_block_scoped, emit_pattern_bindings};

/// Lower a for loop over a range (`for i in start..end`).
pub(super) fn lower_for(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    iterable: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    if let Pattern::Binding(var_name) = &pattern.node {
        if let Expr::Range {
            start: Some(start),
            end: Some(end),
            inclusive,
        } = &iterable.node
        {
            lower_for_range(ctx, builder, var_name, start, end, *inclusive, body, else_arm);
            return;
        }
    }

    // Detect `for (i, elem) in collection.enumerate():` and lower as index-tracked loop
    if let Pattern::Tuple(parts) = &pattern.node {
        if parts.len() == 2 {
            if let Expr::MethodCall { receiver, method, args: call_args, .. } = &iterable.node {
                if method.node == "enumerate" && call_args.is_empty() {
                    lower_for_enumerate(ctx, builder, parts, receiver, body, else_arm);
                    return;
                }
            }
        }
    }

    // Lower the iterable and check its type for string/collection iteration
    let iter_op = lower_expr(ctx, builder, iterable);
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);

    // §6.8 Stage 5: when iterating a string AND the iterable is Ptr-typed
    // (e.g. a borrowed resource param), preserve iter_op as the Ptr — so
    // lower_for_string's source-aware picker can route through
    // slot_kind=BorrowedPtr without going through a value-typed shallow
    // alias. For collections, keep the legacy auto-deref path: their
    // lower_for_* helpers haven't been migrated to consume Ptr-typed
    // iter_ops yet (separate audit).
    let pointee = ctx.pointee_type(iter_type);
    let pointee_is_string = pointee.map_or(false, |p| ctx.type_mapper.is_string_type(p));

    let (iter_op, iter_type) = if pointee.is_some() && !pointee_is_string {
        // Auto-deref for non-string Ptr iterables (legacy collection paths).
        if let (Operand::Copy(p) | Operand::Move(p), Some(inner)) = (&iter_op, pointee) {
            let deref_place = Place {
                local: p.local,
                projections: vec![Projection::Deref],
            };
            let tmp = builder.add_local(inner, None);
            builder.assign_mode(
                crate::ir::instructions::AssignMode::Borrow,
                Place::local(tmp),
                Operand::Copy(deref_place),
            );
            (Operand::Copy(Place::local(tmp)), inner)
        } else {
            (iter_op, iter_type)
        }
    } else {
        // String case OR non-Ptr iterable — keep iter_op as-is. For
        // strings: lower_for_string detects Ptr-typed source. For
        // non-Ptr: nothing to deref.
        (iter_op, pointee.unwrap_or(iter_type))
    };

    // Extract the binding name (or use a temp for pattern destructuring)
    let var_name = if let Pattern::Binding(name) = &pattern.node {
        name.clone()
    } else {
        "__for_elem".to_string()
    };

    if ctx.type_mapper.is_string_type(iter_type) {
        lower_for_string(ctx, builder, &var_name, iter_op, iterable, body, else_arm);
    } else {
        // Determine collection kind from type metadata (set by BuiltinTypeProtocol).
        // Falls back to name-based detection, then Iterable/Iterator trait dispatch.
        use crate::ir::types::CollectionKind;
        let collection_kind = if let Operand::Copy(ref p) | Operand::Move(ref p) = iter_op {
            let local_idx = p.local.0 as usize;
            if local_idx < builder.locals.len() {
                let tid = builder.locals[local_idx].type_id;
                let tid = ctx.pointee_type(tid).unwrap_or(tid);
                if let Some(GirType::Named(name)) = ctx.type_registry.get(tid) {
                    // Metadata-based: read `collection_kind` from the TypeDef.
                    // Both runtime singletons (GorgetArray/GorgetMap/GorgetSet —
                    // registered in `lowering/mod.rs`) and monomorphized aliases
                    // (Vector__T/Dict__K__V/... — registered via
                    // `register_collection_alias` in `lowering/types.rs:766`)
                    // carry `collection_kind` in their TypeMetadata. The earlier
                    // name-prefix fallback was dead — Phase A made it so.
                    ctx.type_registry.get_type_def(name)
                        .and_then(|td| td.metadata.collection_kind)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        match collection_kind {
            Some(CollectionKind::Array) =>
                lower_for_array(ctx, builder, &var_name, iter_op, body, else_arm, pattern),
            Some(CollectionKind::OrderedMap | CollectionKind::Map) =>
                lower_for_dict(ctx, builder, iter_op, body, else_arm, pattern),
            Some(CollectionKind::OrderedSet | CollectionKind::Set) =>
                lower_for_set(ctx, builder, &var_name, iter_op, body, else_arm),
            None => {
                // Try Iterable/Iterator trait dispatch for user-defined types
                if let Some(type_name) = ctx.type_registry.get(iter_type)
                    .and_then(|gt| if let GirType::Named(n) = gt { Some(n.clone()) } else { None })
                {
                    lower_for_iterable(ctx, builder, &var_name, iter_op, &type_name, body, else_arm, pattern);
                }
            }
        }
    }
}

/// Lower `for ch in str_value: body` — iterate UTF-8 codepoints.
fn lower_for_string(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    _iterable_expr: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let owned_string_type = ctx.type_mapper.owned_string_type;

    // §6.8 Stage 5 — source-aware picker (option (c) from the prior
    // comment). Three iter_op shapes existed:
    //   1. literal cap=0 view — iter_local can be a bit-copy; drop is no-op.
    //   2. owned clone from CoW upstream — iter_local takes ownership; drop frees.
    //   3. shared borrow — iter_local must NOT drop (source frees).
    //
    // With the SlotKind axis in place (§6.8 Stages 2-4), shape #3 can
    // now be expressed honestly: make iter_local a Ptr(String) and tag
    // its slot as BorrowedPtr. SlotLoad routing gives the pointer; the
    // .len field projection auto-decodes through the pointer at the LIR
    // layer; the source's drop is the only one. Shapes #1 and #2 keep
    // the old Copy + drop_register path — the bit-copy is harmless when
    // the source isn't drop-tracked.
    //
    // Detection: iter_op's source is "owning" if it's a bare-place Copy
    // or Move on a drop-registered local. That's the source of truth
    // for whether the source will free the heap independently.
    // Two flavors of "source has an owner besides this loop":
    //   (a) named local that's drop-registered (e.g. `String s = f"..."`)
    //   (b) Ptr-typed source (e.g. `String input` param after set_bare_param,
    //       or any local already tagged BorrowedPtr by an earlier setter)
    // Both let iter_local borrow without taking ownership. Different inits
    // (`emit_borrow` to take &source vs `assign` to copy the pointer
    // value), same end state: iter_local is Ptr(String) with
    // slot_kind=BorrowedPtr, the source's drop is the only one.
    let (source_is_owning_named, source_is_ptr_typed) = match &iter_op {
        Operand::Copy(p) | Operand::Move(p) if p.projections.is_empty() => {
            let owning = ctx.drops.is_registered(p.local);
            let ptr_typed = builder.locals.get(p.local.0 as usize).map_or(false, |l| {
                matches!(ctx.type_registry.get(l.type_id),
                    Some(GirType::Ptr(_) | GirType::MutPtr(_)))
            });
            (owning && !ptr_typed, ptr_typed)
        }
        _ => (false, false),
    };
    let iter_local = if source_is_owning_named {
        let ptr_type = ctx.register_ptr_type(owned_string_type);
        let local = builder.add_local(ptr_type, None);
        if let Operand::Copy(p) | Operand::Move(p) = &iter_op {
            builder.emit_borrow(local, p.clone());
        }
        ctx.set_ref(builder, local);
        local
    } else if source_is_ptr_typed {
        // Source is already a pointer to String; copy the pointer value.
        let ptr_type = ctx.register_ptr_type(owned_string_type);
        let local = builder.add_local(ptr_type, None);
        builder.assign(Place::local(local), iter_op);
        ctx.set_ref(builder, local);
        local
    } else {
        let local = builder.add_local(owned_string_type, None);
        builder.assign(Place::local(local), iter_op);
        ctx.drops.register_local(local, owned_string_type, &ctx.type_registry);
        local
    };

    // byte_pos = 0
    let byte_pos = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(byte_pos), Operand::Constant(Constant::I64(0)));

    // len = iter.len (byte length)
    let len_local = builder.add_local(I64_TYPE, None);
    // Access .len field (field index 2 for 32-byte Str: {data, cap, len, alloc})
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(2)],
    };
    builder.assign(Place::local(len_local), Operand::Copy(len_place));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // break_exit_bb: where `break` jumps. If else arm exists, it's a separate block
    // (break skips else); otherwise it's exit_bb directly.
    // else_exit_bb: where the loop's natural exit goes. If else arm exists, it's the
    // else block; otherwise it's exit_bb directly. No Option needed.
    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    builder.jump(header_bb);

    // Header: byte_pos < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(len_local));
    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_str = ctx.save_locals(builder);
    ctx.push_loop(header_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // cplen = gorget_utf8_codepoint_len((unsigned char)data[byte_pos])
    // We'll call this as an extern that takes a Str and byte offset → returns int
    let cplen = builder.call_extern(
        "gorget_utf8_codepoint_len_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        I64_TYPE,
    );

    // ch = (Str){ .data = iter.data + byte_pos, .len = cplen }
    // We'll construct this as a StructInit with computed fields via extern.
    // `gorget_str_codepoint_at` returns an owned Str (allocates via
    // `gorget_str_own_region`), so drop-register the local for the loop body
    // scope — without this, every iteration leaked one codepoint copy.
    let ch_local = builder.call_extern(
        "gorget_str_codepoint_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        owned_string_type,
    );
    ctx.register_local(var_name, ch_local, owned_string_type);
    ctx.drops.register_local(ch_local, owned_string_type, &ctx.type_registry);

    // Lower the body
    lower_block(ctx, builder, body);

    // byte_pos += cplen
    let new_pos = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(cplen));
    builder.assign(Place::local(byte_pos), FunctionBuilder::copy(new_pos));

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_str);
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for elem in array: body` — iterate array elements by index.
fn lower_for_array(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // Store the iterable in a local. Phase C: emit Borrow mode for
    // resource-typed iters — iter_local is a non-owning view (no drop
    // registration), the original local still owns the data. Same shape
    // as the C2.6 fix in lower_for_iterable.
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    let iter_assign_mode = if ctx.type_registry.is_resource_type(iter_type) {
        crate::ir::instructions::AssignMode::Borrow
    } else {
        crate::ir::instructions::AssignMode::Copy
    };
    builder.assign_mode(iter_assign_mode, Place::local(iter_local), iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 2 of GorgetArray: {data, cap, len, elem_size})
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(2)],
    };
    builder.assign(Place::local(len), Operand::Copy(len_place));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    builder.jump(header_bb);

    // Header: idx < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_arr = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // elem = iter[idx] — borrow element from array (view for strings, clone for collections).
    let elem_type = super::super::exprs::infer_collection_element_type(ctx, iter_type);
    let elem = builder.index_load_borrow(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    ctx.register_local(var_name, elem, elem_type);
    // String borrows are pointer copies — do NOT register for drops.
    // The collection still owns the data; the borrow just reads through the pointer.
    // Non-string resource types still need drops (they have separate allocations).
    if !ctx.type_mapper.is_string_type(elem_type) {
        ctx.drops.register_local(elem, elem_type, &ctx.type_registry);
    }
    if ctx.type_mapper.is_string_type(elem_type) {
        ctx.func_state.has_string_borrows = true;
    }

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        emit_pattern_bindings(ctx, builder, pattern, elem, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_arr);

    // Increment idx
    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_idx = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for (i, elem) in collection.enumerate(): body` — iterate with index tracking.
/// Instead of materializing an enumerate array, we emit a regular for-loop with a counter.
fn lower_for_enumerate(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    parts: &[Spanned<Pattern>],
    receiver: &Spanned<Expr>,
    body: &Block,
    else_arm: Option<&Block>,
) {
    

    // Lower the receiver collection
    let iter_op = lower_expr(ctx, builder, receiver);
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);

    // Store the iterable in a local. Phase C: Borrow mode for resource
    // iters — non-owning view; original local owns the data.
    let iter_local = builder.add_local(iter_type, None);
    let iter_assign_mode = if ctx.type_registry.is_resource_type(iter_type) {
        crate::ir::instructions::AssignMode::Borrow
    } else {
        crate::ir::instructions::AssignMode::Copy
    };
    builder.assign_mode(iter_assign_mode, Place::local(iter_local), iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 2 for both Str and GorgetArray under uniform layout)
    // Str: {data, cap, len, alloc}. GorgetArray: {data, cap, len, elem_size}.
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(2)],
    };
    builder.assign(Place::local(len), Operand::Copy(len_place));

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    builder.jump(header_bb);

    // Header: idx < len
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_enum = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Bind index variable (first tuple element)
    if let Pattern::Binding(idx_name) = &parts[0].node {
        let idx_local = builder.add_local(I64_TYPE, Some(idx_name));
        builder.assign(Place::local(idx_local), FunctionBuilder::copy(idx));
        ctx.register_local(idx_name, idx_local, I64_TYPE);
    }

    // Bind element variable (second tuple element) — load from array
    let elem_type = super::super::exprs::infer_collection_element_type(ctx, iter_type);
    let elem = builder.index_load_borrow(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    if let Pattern::Binding(elem_name) = &parts[1].node {
        ctx.register_local(elem_name, elem, elem_type);
    }
    // String borrows: don't register for drops (pointer copy, collection owns the data).
    if !ctx.type_mapper.is_string_type(elem_type) {
        ctx.drops.register_local(elem, elem_type, &ctx.type_registry);
    }
    if ctx.type_mapper.is_string_type(elem_type) {
        ctx.func_state.has_string_borrows = true;
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_enum);

    // Increment idx
    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_idx = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(idx), FunctionBuilder::copy(new_idx));
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for k, v in dict: body` — iterate Dict or HashMap entries.
fn lower_for_dict(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // Store the iterable in a local. Phase C: emit Borrow mode for
    // resource-typed iters — iter_local is a non-owning view (no drop
    // registration), the original local still owns the data. Same shape
    // as the C2.6 fix in lower_for_iterable.
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    let iter_assign_mode = if ctx.type_registry.is_resource_type(iter_type) {
        crate::ir::instructions::AssignMode::Borrow
    } else {
        crate::ir::instructions::AssignMode::Copy
    };
    builder.assign_mode(iter_assign_mode, Place::local(iter_local), iter_op);

    // Create a pointer to the dict for iterator accessor calls.
    let dict_ptr_type = ctx.register_ptr_type(iter_type);
    let dict_ptr = builder.add_local(dict_ptr_type, None);
    builder.emit_borrow(dict_ptr, Place::local(iter_local));

    // Determine key/value type names from the collection type
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    let (key_gir_type, val_gir_type) = parse_dict_kv_types(&type_name);
    let key_type = ctx.lookup_type_by_name(&key_gir_type).unwrap_or(I64_TYPE);
    let val_type = ctx.lookup_type_by_name(&val_gir_type).unwrap_or(I64_TYPE);

    // oi = 0 (outer iteration index)
    let oi = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(oi), Operand::Constant(Constant::I64(0)));

    // limit = cap (iterate over all slots, check states for USED)
    let limit = builder.call_extern(
        "gorget_map_iter_cap",
        vec![FunctionBuilder::copy(dict_ptr)],
        I64_TYPE,
    );

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    builder.jump(header_bb);

    // Header: oi < limit
    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(oi), FunctionBuilder::copy(limit));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body
    builder.switch_to(body_bb);
    let saved_dict = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // idx = oi (direct index into capacity)
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), FunctionBuilder::copy(oi));

    // state check: if states[idx] != 1, skip to incr
    let state = builder.call_extern(
        "gorget_map_iter_state",
        vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx)],
        I64_TYPE,
    );
    let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

    let elem_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
    builder.switch_to(elem_bb);

    // Extract key/value bindings via runtime output-parameter functions.
    match &pattern.node {
        Pattern::Tuple(parts) if parts.len() == 2 => {
            let k_name = if let Pattern::Binding(n) = &parts[0].node { n.clone() } else { "__k".to_string() };
            let v_name = if let Pattern::Binding(n) = &parts[1].node { n.clone() } else { "__v".to_string() };

            let k_local = builder.add_local(key_type, Some(&k_name));
            let k_ptr_type = ctx.register_ptr_type(key_type);
            let k_ptr = builder.borrow_mut(Place::local(k_local), k_ptr_type);
            builder.call_extern_void(
                "gorget_map_iter_key",
                vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx), FunctionBuilder::copy(k_ptr)],
            );
            ctx.register_local(&k_name, k_local, key_type);

            let v_local = builder.add_local(val_type, Some(&v_name));
            let v_ptr_type = ctx.register_ptr_type(val_type);
            let v_ptr = builder.borrow_mut(Place::local(v_local), v_ptr_type);
            builder.call_extern_void(
                "gorget_map_iter_value",
                vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx), FunctionBuilder::copy(v_ptr)],
            );
            ctx.register_local(&v_name, v_local, val_type);
        }
        Pattern::Binding(name) => {
            let k_local = builder.add_local(key_type, Some(name));
            let k_ptr_type = ctx.register_ptr_type(key_type);
            let k_ptr = builder.borrow_mut(Place::local(k_local), k_ptr_type);
            builder.call_extern_void(
                "gorget_map_iter_key",
                vec![FunctionBuilder::copy(dict_ptr), FunctionBuilder::copy(idx), FunctionBuilder::copy(k_ptr)],
            );
            ctx.register_local(name, k_local, key_type);
        }
        _ => {}
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_dict);

    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_oi = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(oi), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(oi), FunctionBuilder::copy(new_oi));
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for elem in set: body` — iterate Set elements.
fn lower_for_set(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    body: &Block,
    else_arm: Option<&Block>,
) {
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    // Phase C: Borrow mode — iter_local is non-owning view; the original
    // local still owns the Set's heap data.
    let iter_assign_mode = if ctx.type_registry.is_resource_type(iter_type) {
        crate::ir::instructions::AssignMode::Borrow
    } else {
        crate::ir::instructions::AssignMode::Copy
    };
    builder.assign_mode(iter_assign_mode, Place::local(iter_local), iter_op);
    // Create pointer for iterator accessor calls
    let set_ptr_type = ctx.register_ptr_type(iter_type);
    let set_ptr = builder.add_local(set_ptr_type, None);
    builder.emit_borrow(set_ptr, Place::local(iter_local));

    // Parse element type from Set__T
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    // Read typed `collection_kind` to discriminate ordered Set vs unordered
    // HashSet (the iteration shape differs: ordered walks `order[]`, unordered
    // walks `states[]`). Replaces a name-prefix probe on the receiver type.
    let is_ordered = ctx.type_registry.get_type_def(&type_name)
        .and_then(|td| td.metadata.collection_kind)
        == Some(CollectionKind::OrderedSet);
    let elem_gir_type = parse_set_elem_type(&type_name);
    let elem_type = ctx.lookup_type_by_name(&elem_gir_type).unwrap_or(I64_TYPE);

    // i = 0  (for ordered: index into order array; for unordered: index into states)
    let i_local = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(i_local), Operand::Constant(Constant::I64(0)));

    // limit: ordered uses order_len, unordered uses cap
    let limit = if is_ordered {
        builder.call_extern(
            "gorget_map_iter_order_len",
            vec![FunctionBuilder::copy(set_ptr)],
            I64_TYPE,
        )
    } else {
        builder.call_extern(
            "gorget_map_iter_cap",
            vec![FunctionBuilder::copy(set_ptr)],
            I64_TYPE,
        )
    };

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    builder.jump(header_bb);

    builder.switch_to(header_bb);
    let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(i_local), FunctionBuilder::copy(limit));

    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    builder.switch_to(body_bb);
    let saved_set = ctx.save_locals(builder);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    if is_ordered {
        // For ordered sets: dereference order array to get the real bucket index
        let real_i = builder.call_extern(
            "gorget_map_iter_order",
            vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(i_local)],
            I64_TYPE,
        );

        // state check (still needed — deleted entries may leave stale order slots)
        let state = builder.call_extern(
            "gorget_map_iter_state",
            vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(real_i)],
            I64_TYPE,
        );
        let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

        let elem_bb = builder.new_block();
        builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
        builder.switch_to(elem_bb);

        // Bind element using the real bucket index
        let elem_local = builder.add_local(elem_type, Some(var_name));
        let elem_ptr_type = ctx.register_ptr_type(elem_type);
        let elem_ptr = builder.borrow_mut(Place::local(elem_local), elem_ptr_type);
        builder.call_extern_void(
            "gorget_map_iter_key",
            vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(real_i), FunctionBuilder::copy(elem_ptr)],
        );
        ctx.register_local(var_name, elem_local, elem_type);
    } else {
        // state check
        let state = builder.call_extern(
            "gorget_map_iter_state",
            vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(i_local)],
            I64_TYPE,
        );
        let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

        let elem_bb = builder.new_block();
        builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
        builder.switch_to(elem_bb);

        // Bind element
        let elem_local = builder.add_local(elem_type, Some(var_name));
        let elem_ptr_type = ctx.register_ptr_type(elem_type);
        let elem_ptr = builder.borrow_mut(Place::local(elem_local), elem_ptr_type);
        builder.call_extern_void(
            "gorget_map_iter_key",
            vec![FunctionBuilder::copy(set_ptr), FunctionBuilder::copy(i_local), FunctionBuilder::copy(elem_ptr)],
        );
        ctx.register_local(var_name, elem_local, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_set);

    builder.jump(incr_bb);
    builder.switch_to(incr_bb);
    let new_i = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(i_local), Operand::Constant(Constant::I64(1)));
    builder.assign(Place::local(i_local), FunctionBuilder::copy(new_i));
    builder.jump(header_bb);

    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Lower `for var in iterable: body` for user-defined Iterable[T] types.
/// Generates: iter = Type__iter(&collection); loop { opt = Iter__next(&iter); if None: break; var = opt.Some._0; body }
fn lower_for_iterable(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    iter_op: Operand,
    type_name: &str,
    body: &Block,
    else_arm: Option<&Block>,
    pattern: &Spanned<Pattern>,
) {
    // 1. Find the iter() method for this type, or — if the type implements
    // Iterator directly (has `next()` but no `iter()`) — treat `self` as the
    // iterator by skipping the iter() call.
    // Search fn_sigs for patterns like: Iterable__T_for_TypeName__iter or TypeName__iter
    let iter_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__iter") && k.contains(&format!("_for_{type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{type_name}__iter");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    // Detect types that directly implement Iterator (have next() but no iter()).
    let has_next_direct = {
        let direct_next = format!("{type_name}__next");
        ctx.fn_sigs.contains_key(&direct_next)
            || ctx.fn_sigs.keys().any(|k| k.ends_with("__next") && k.contains(&format!("_for_{type_name}__")))
    };
    let self_as_iterator = iter_fn_name.is_none() && has_next_direct;

    // 2. Determine the iterator return type. If there's an iter() method, use
    // its return type; otherwise the iterator IS the collection itself.
    let iter_type_name;
    let iter_ret_type;
    if let Some(ref iter_fn) = iter_fn_name {
        let (_, ret) = match ctx.fn_sigs.get(iter_fn) {
            Some(sig) => sig.clone(),
            None => return,
        };
        iter_ret_type = ret;
        iter_type_name = match ctx.type_registry.type_name(iter_ret_type) {
            Some(name) => name,
            None => return,
        };
    } else if self_as_iterator {
        // Self-as-iterator: iter type = collection type.
        iter_ret_type = infer_operand_type_full(ctx, &iter_op, builder);
        iter_type_name = type_name.to_string();
    } else {
        return;
    }

    // 3. Find the next() method for the iterator type
    let next_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__next") && k.contains(&format!("_for_{iter_type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{iter_type_name}__next");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    let next_fn_name = match next_fn_name {
        Some(name) => name,
        None => return,
    };

    // 4. Get the Option return type from next()
    let (_, option_ret_type) = match ctx.fn_sigs.get(&next_fn_name) {
        Some(sig) => sig.clone(),
        None => return,
    };

    // Determine the element type from the Option type name (Option__T → T)
    let option_type_name = ctx.type_registry.type_name(option_ret_type)
        .unwrap_or_else(|| "Option__int64_t".to_string());
    let elem_c_type = option_type_name.strip_prefix("Option__")
        .unwrap_or("int64_t");
    let elem_type = ctx.type_mapper.lookup_named(elem_c_type).unwrap_or(I64_TYPE);

    // 5. Store the iterable and (optionally) call iter().
    // The collection_local is a non-owning view into the caller's data
    // — same shape as the deref-Ptr case at line 54. Phase C: emit
    // Borrow mode so the typed contract matches (no drop, no clone).
    let iter_type_full = infer_operand_type_full(ctx, &iter_op, builder);
    let collection_local = builder.add_local(iter_type_full, None);
    let collection_assign_mode = if ctx.type_registry.is_resource_type(iter_type_full) {
        crate::ir::instructions::AssignMode::Borrow
    } else {
        crate::ir::instructions::AssignMode::Copy
    };
    builder.assign_mode(collection_assign_mode, Place::local(collection_local), iter_op);

    let iterator_local = if let Some(ref iter_fn) = iter_fn_name {
        // Iterable path: Call iter(&collection) → iterator
        let self_ptr_type = ctx.register_ptr_type(iter_type_full);
        let self_ref = builder.borrow(Place::local(collection_local), self_ptr_type);
        builder.call_extern(
            iter_fn,
            vec![FunctionBuilder::copy(self_ref)],
            iter_ret_type,
        )
    } else {
        // Direct-Iterator path: the collection IS the iterator.
        collection_local
    };

    // 6. Build the loop structure
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    builder.jump(header_bb);

    // Header: call next(&iterator) → Option
    builder.switch_to(header_bb);
    let iter_ptr_type = ctx.register_mut_ptr_type(iter_ret_type);
    let iter_ref = builder.borrow_mut(Place::local(iterator_local), iter_ptr_type);
    let opt_result = builder.call_extern(
        &next_fn_name,
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
    
    builder.branch(FunctionBuilder::copy(is_none), else_exit_bb, body_bb);

    // Body: extract value from Some variant
    builder.switch_to(body_bb);
    let saved_iter = ctx.save_locals(builder);
    ctx.push_loop(header_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Extract: elem = opt_result.data.Some._0
    let elem_local = builder.enum_field_load_move(
        Place::local(opt_result),
        "Some",
        0,
        elem_type,
    );
    ctx.register_local(var_name, elem_local, elem_type);

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        emit_pattern_bindings(ctx, builder, pattern, elem_local, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(builder, saved_iter);
    builder.jump(header_bb);

    // Else block
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    builder.switch_to(exit_bb);
}

/// Parse Dict/HashMap type name to extract key/value C type strings.
fn parse_dict_kv_types(type_name: &str) -> (String, String) {
    // Dict__Str__int64_t → ("Str", "int64_t")
    // HashMap__int64_t__Str → ("int64_t", "Str")
    let stripped = type_name
        .strip_prefix("Dict__")
        .or_else(|| type_name.strip_prefix("HashMap__"))
        .or_else(|| type_name.strip_prefix("GorgetDict__"))
        .or_else(|| type_name.strip_prefix("GorgetMap__"))
        .unwrap_or(type_name);
    // Split on __ to get key and value types
    // But type names themselves can contain __ (e.g. Option__int64_t)
    // Simple heuristic: first known type boundary
    if let Some(pos) = find_kv_split(stripped) {
        (stripped[..pos].to_string(), stripped[pos + 2..].to_string())
    } else {
        ("int64_t".to_string(), "int64_t".to_string())
    }
}

/// Parse Set type name to extract element C type string.
fn parse_set_elem_type(type_name: &str) -> String {
    type_name
        .strip_prefix("Set__")
        .or_else(|| type_name.strip_prefix("HashSet__"))
        .or_else(|| type_name.strip_prefix("GorgetSet__"))
        .unwrap_or("int64_t")
        .to_string()
}

/// Find the __ separator between key and value types.
fn find_kv_split(s: &str) -> Option<usize> {
    // Known primitive suffixes to try splitting on
    let primitives = ["int64_t", "int32_t", "int16_t", "int8_t",
                      "uint64_t", "uint32_t", "uint16_t", "uint8_t",
                      "double", "float", "bool", "GorgetString"];
    for prim in &primitives {
        if s.starts_with(prim) && s[prim.len()..].starts_with("__") {
            return Some(prim.len());
        }
    }
    // Try splitting at each __ position
    let mut i = 0;
    while let Some(pos) = s[i..].find("__") {
        let abs_pos = i + pos;
        if abs_pos > 0 && abs_pos + 2 < s.len() {
            return Some(abs_pos);
        }
        i = abs_pos + 2;
    }
    None
}


/// Lower `for var in start..end: body` or `for var in start..=end: body`.
fn lower_for_range(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    var_name: &str,
    start: &Spanned<Expr>,
    end: &Spanned<Expr>,
    inclusive: bool,
    body: &Block,
    else_arm: Option<&Block>,
) {
    // Create loop variable — type inferred from the start expression.
    // For literal bounds (e.g. `0..n`) this gives I64_TYPE; for typed variables
    // (e.g. `start..end` where start: uint8) it preserves the narrower type.
    let saved_range = ctx.save_locals(builder);
    let start_val = lower_expr(ctx, builder, start);
    let loop_type = infer_operand_type_full(ctx, &start_val, builder);
    let loop_var = builder.add_local(loop_type, Some(var_name));
    builder.assign(Place::local(loop_var), start_val);
    ctx.register_local(var_name, loop_var, loop_type);

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // For for-else: separate break target from natural exit
    let (break_exit_bb, else_exit_bb) = if else_arm.is_some() {
        (builder.new_block(), builder.new_block())
    } else {
        (exit_bb, exit_bb)
    };

    // Jump to header
    builder.jump(header_bb);

    // Header: compare loop var with end
    builder.switch_to(header_bb);
    let end_val = lower_expr(ctx, builder, end);
    let cmp_op = if inclusive { CmpOp::Le } else { CmpOp::Lt };
    let cond = builder.cmp(cmp_op, loop_type, FunctionBuilder::copy(loop_var), end_val);
    builder.branch(FunctionBuilder::copy(cond), body_bb, else_exit_bb);

    // Body (wrapped in Loop scope for drop cleanup)
    // Continue target is incr_bb (not header_bb) so the loop variable gets incremented
    builder.switch_to(body_bb);
    ctx.push_loop(incr_bb, break_exit_bb, builder.locals.len() as u32);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(incr_bb);

    // Increment: loop_var = loop_var + 1
    builder.switch_to(incr_bb);
    let one = Operand::Constant(Constant::I64(1));
    let incremented = builder.bin_op(BinOp::Add, loop_type, FunctionBuilder::copy(loop_var), one);
    builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
    builder.jump(header_bb);

    ctx.restore_locals(builder, saved_range);

    // Else block: executed when loop completes naturally (no break)
    if let Some(else_body) = else_arm {
        builder.switch_to(else_exit_bb);
        lower_block_scoped(ctx, builder, else_body);
        builder.jump(exit_bb);

        // Break exit goes directly to exit (skipping else)
        builder.switch_to(break_exit_bb);
        builder.jump(exit_bb);
    }

    // Exit
    builder.switch_to(exit_bb);
}
