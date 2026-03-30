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

    // If the iterable is a Ptr (borrowed resource param), deref to get the value
    // for iteration. The iter_local is not drop-registered (read-only view), so
    // the shallow copy is safe — the caller still owns the heap data.
    let (iter_op, iter_type) = if let Some(inner) = ctx.pointee_type(iter_type) {
        if let Operand::Copy(ref p) | Operand::Move(ref p) = iter_op {
            let deref_place = Place {
                local: p.local,
                projections: vec![Projection::Deref],
            };
            let tmp = builder.add_local(inner, None);
            builder.assign(Place::local(tmp), Operand::Copy(deref_place));
            (Operand::Copy(Place::local(tmp)), inner)
        } else {
            (iter_op, iter_type)
        }
    } else {
        (iter_op, iter_type)
    };

    // Extract the binding name (or use a temp for pattern destructuring)
    let var_name = if let Pattern::Binding(name) = &pattern.node {
        name.clone()
    } else {
        "__for_elem".to_string()
    };

    if ctx.type_mapper.is_string_type(iter_type) {
        lower_for_string(ctx, builder, &var_name, iter_op, body, else_arm);
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
                    // Metadata-based: check collection_kind on the TypeDef
                    ctx.type_registry.get_type_def(name)
                        .and_then(|td| td.metadata.collection_kind)
                        .or_else(|| {
                            // Fallback for types without TypeDefs (registered via register_collection_alias)
                            if name.starts_with("Vector__") || name.starts_with("GorgetArray") || name.starts_with("Deque__") {
                                Some(CollectionKind::Array)
                            } else if name.starts_with("Dict__") {
                                Some(CollectionKind::OrderedMap)
                            } else if name.starts_with("HashMap__") || name.starts_with("GorgetMap") || name.starts_with("GorgetDict") {
                                Some(CollectionKind::Map)
                            } else if name.starts_with("Set__") {
                                Some(CollectionKind::OrderedSet)
                            } else if name.starts_with("HashSet__") || name.starts_with("GorgetSet") {
                                Some(CollectionKind::Set)
                            } else {
                                None
                            }
                        })
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
    body: &Block,
    else_arm: Option<&Block>,
) {
    let string_view_type = ctx.type_mapper.string_view_type;

    // Store the iterable in a local
    let iter_local = builder.add_local(string_view_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    // byte_pos = 0
    let byte_pos = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(byte_pos), Operand::Constant(Constant::I64(0)));

    // len = iter.len (byte length)
    let len_local = builder.add_local(I64_TYPE, None);
    // Access .len field (field index 1 for Str: {data, len})
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(1)],
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
    let saved_str = ctx.save_locals();
    ctx.push_loop(header_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // cplen = gorget_utf8_codepoint_len((unsigned char)data[byte_pos])
    // We'll call this as an extern that takes a Str and byte offset → returns int
    let cplen = builder.call_extern(
        "gorget_utf8_codepoint_len_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        I64_TYPE,
    );

    // ch = (Str){ .data = iter.data + byte_pos, .len = cplen }
    // We'll construct this as a StructInit with computed fields via extern
    let ch_local = builder.call_extern(
        "gorget_str_codepoint_at",
        vec![FunctionBuilder::copy(iter_local), FunctionBuilder::copy(byte_pos)],
        string_view_type,
    );
    ctx.register_local(var_name, ch_local, string_view_type);

    // Lower the body
    lower_block(ctx, builder, body);

    // byte_pos += cplen
    let new_pos = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(byte_pos), FunctionBuilder::copy(cplen));
    builder.assign(Place::local(byte_pos), FunctionBuilder::copy(new_pos));

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(saved_str);
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
    // Store the iterable in a local
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 1 of GorgetArray: {data, len, elem_size, cap})
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(1)],
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
    let saved_arr = ctx.save_locals();
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // elem = iter[idx] — load element from array
    let elem_type = super::super::exprs::infer_collection_element_type(ctx, iter_type);
    let elem = builder.index_load(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    ctx.register_local(var_name, elem, elem_type);

    // If pattern is a destructuring tuple, emit bindings
    if !matches!(pattern.node, Pattern::Binding(_)) {
        emit_pattern_bindings(ctx, builder, pattern, elem, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(saved_arr);

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

    // Store the iterable in a local
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    // idx = 0
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), Operand::Constant(Constant::I64(0)));

    // len = iter.len (field index 1 of GorgetArray)
    let len = builder.add_local(I64_TYPE, None);
    let len_place = Place {
        local: iter_local,
        projections: vec![Projection::Field(1)],
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
    let saved_enum = ctx.save_locals();
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Bind index variable (first tuple element)
    if let Pattern::Binding(idx_name) = &parts[0].node {
        let idx_local = builder.add_local(I64_TYPE, Some(idx_name));
        builder.assign(Place::local(idx_local), FunctionBuilder::copy(idx));
        ctx.register_local(idx_name, idx_local, I64_TYPE);
    }

    // Bind element variable (second tuple element) — load from array
    let elem_type = super::super::exprs::infer_collection_element_type(ctx, iter_type);
    let elem = builder.index_load(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
    if let Pattern::Binding(elem_name) = &parts[1].node {
        ctx.register_local(elem_name, elem, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(saved_enum);

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
    // Store the iterable in a local
    let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
    let iter_local = builder.add_local(iter_type, None);
    builder.assign(Place::local(iter_local), iter_op);

    let dict_id = iter_local.0;

    // Determine key/value type names from the collection type
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    // Parse: Dict__KeyType__ValueType or HashMap__KeyType__ValueType
    let (key_gir_type, val_gir_type) = parse_dict_kv_types(&type_name);
    // Convert GIR names → C type names for inline C codegen
    let key_c_type = to_c_type_name(&key_gir_type);
    let val_c_type = to_c_type_name(&val_gir_type);
    // Look up the TypeIds for key/value types (use GIR names for type registry)
    let key_type = ctx.lookup_type_by_name(&key_gir_type).unwrap_or(I64_TYPE);
    let val_type = ctx.lookup_type_by_name(&val_gir_type).unwrap_or(I64_TYPE);

    // oi = 0 (outer iteration index)
    let oi = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(oi), Operand::Constant(Constant::I64(0)));

    // limit = cap (iterate over all slots, check states for USED)
    let limit = builder.add_local(I64_TYPE, None);
    builder.inline_c(format!("_{} = (int64_t)_{}.cap;", limit.0, dict_id));

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
    let saved_dict = ctx.save_locals();
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // idx = oi (direct index into capacity)
    let idx = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(idx), FunctionBuilder::copy(oi));

    // state check: if states[idx] != 1, skip to incr
    let state = builder.add_local(I64_TYPE, None);
    builder.inline_c(format!("_{s} = (int64_t)_{dict}.states[(size_t)_{idx}];",
        s = state.0, dict = dict_id, idx = idx.0));
    let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

    let elem_bb = builder.new_block();
    builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
    builder.switch_to(elem_bb);

    // Extract key/value bindings
    match &pattern.node {
        Pattern::Tuple(parts) if parts.len() == 2 => {
            let k_name = if let Pattern::Binding(n) = &parts[0].node { n.clone() } else { "__k".to_string() };
            let v_name = if let Pattern::Binding(n) = &parts[1].node { n.clone() } else { "__v".to_string() };

            let k_local = builder.add_local(key_type, Some(&k_name));
            builder.inline_c(format!("_{k} = (({key_c_type}*)_{dict}.keys)[(size_t)_{idx}];",
                k = k_local.0, dict = dict_id, idx = idx.0));
            ctx.register_local(&k_name, k_local, key_type);

            let v_local = builder.add_local(val_type, Some(&v_name));
            builder.inline_c(format!("_{v} = (({val_c_type}*)_{dict}.values)[(size_t)_{idx}];",
                v = v_local.0, dict = dict_id, idx = idx.0));
            ctx.register_local(&v_name, v_local, val_type);
        }
        Pattern::Binding(name) => {
            // Single binding: bind the key
            let k_local = builder.add_local(key_type, Some(name));
            builder.inline_c(format!("_{k} = (({key_c_type}*)_{dict}.keys)[(size_t)_{idx}];",
                k = k_local.0, dict = dict_id, idx = idx.0));
            ctx.register_local(name, k_local, key_type);
        }
        _ => {}
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(saved_dict);

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
    builder.assign(Place::local(iter_local), iter_op);
    let set_id = iter_local.0;

    // Parse element type from Set__T
    let type_name = ctx.type_name_for_id(iter_type)
        .map(|s| s.to_string())
        .unwrap_or_default();
    let is_ordered = type_name.starts_with("Set__");
    let elem_gir_type = parse_set_elem_type(&type_name);
    let elem_c_type = to_c_type_name(&elem_gir_type);
    let elem_type = ctx.lookup_type_by_name(&elem_gir_type).unwrap_or(I64_TYPE);

    // i = 0  (for ordered: index into order array; for unordered: index into states)
    let i_local = builder.add_local(I64_TYPE, None);
    builder.assign(Place::local(i_local), Operand::Constant(Constant::I64(0)));

    // limit: ordered uses order_len, unordered uses cap
    let limit = builder.add_local(I64_TYPE, None);
    if is_ordered {
        builder.inline_c(format!("_{} = (int64_t)_{}.order_len;", limit.0, set_id));
    } else {
        builder.inline_c(format!("_{} = (int64_t)_{}.cap;", limit.0, set_id));
    }

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
    let saved_set = ctx.save_locals();
    ctx.push_loop(incr_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    if is_ordered {
        // For ordered sets: dereference order array to get the real bucket index
        let real_i = builder.add_local(I64_TYPE, None);
        builder.inline_c(format!("_{ri} = (int64_t)_{set}.order[(size_t)_{i}];",
            ri = real_i.0, set = set_id, i = i_local.0));

        // state check (still needed — deleted entries may leave stale order slots)
        let state = builder.add_local(I64_TYPE, None);
        builder.inline_c(format!("_{s} = (int64_t)_{set}.states[(size_t)_{ri}];",
            s = state.0, set = set_id, ri = real_i.0));
        let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

        let elem_bb = builder.new_block();
        builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
        builder.switch_to(elem_bb);

        // Bind element using the real bucket index
        let elem_local = builder.add_local(elem_type, Some(var_name));
        builder.inline_c(format!("_{e} = (({elem_c_type}*)_{set}.keys)[(size_t)_{ri}];",
            e = elem_local.0, set = set_id, ri = real_i.0));
        ctx.register_local(var_name, elem_local, elem_type);
    } else {
        // state check (assignment form)
        let state = builder.add_local(I64_TYPE, None);
        builder.inline_c(format!("_{s} = (int64_t)_{set}.states[(size_t)_{i}];",
            s = state.0, set = set_id, i = i_local.0));
        let state_ok = builder.cmp(CmpOp::Eq, I64_TYPE, FunctionBuilder::copy(state), Operand::Constant(Constant::I64(1)));

        let elem_bb = builder.new_block();
        builder.branch(FunctionBuilder::copy(state_ok), elem_bb, incr_bb);
        builder.switch_to(elem_bb);

        // Bind element (assignment form with correct type, cast void* keys)
        let elem_local = builder.add_local(elem_type, Some(var_name));
        builder.inline_c(format!("_{e} = (({elem_c_type}*)_{set}.keys)[(size_t)_{i}];",
            e = elem_local.0, set = set_id, i = i_local.0));
        ctx.register_local(var_name, elem_local, elem_type);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    ctx.restore_locals(saved_set);

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
    // 1. Find the iter() method for this type
    // Search fn_sigs for patterns like: Iterable__T_for_TypeName__iter or TypeName__iter
    let iter_fn_name = ctx.fn_sigs.keys()
        .find(|k| k.ends_with("__iter") && k.contains(&format!("_for_{type_name}__")))
        .cloned()
        .or_else(|| {
            let direct = format!("{type_name}__iter");
            if ctx.fn_sigs.contains_key(&direct) { Some(direct) } else { None }
        });

    let iter_fn_name = match iter_fn_name {
        Some(name) => name,
        None => return, // No iter() method found — silently skip
    };

    // 2. Get the iterator return type from fn_sigs
    let (_, iter_ret_type) = match ctx.fn_sigs.get(&iter_fn_name) {
        Some(sig) => sig.clone(),
        None => return,
    };

    // Get the iterator type name
    let iter_type_name = match ctx.type_registry.type_name(iter_ret_type) {
        Some(name) => name,
        None => return,
    };

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

    // 5. Store the iterable and call iter()
    let iter_type_full = infer_operand_type_full(ctx, &iter_op, builder);
    let collection_local = builder.add_local(iter_type_full, None);
    builder.assign(Place::local(collection_local), iter_op);

    // Call iter(&collection) → iterator
    let self_ptr_type = ctx.register_ptr_type(iter_type_full);
    let self_ref = builder.borrow(Place::local(collection_local), self_ptr_type);
    let iterator_local = builder.call_extern(
        &iter_fn_name,
        vec![FunctionBuilder::copy(self_ref)],
        iter_ret_type,
    );

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
    let saved_iter = ctx.save_locals();
    ctx.push_loop(header_bb, break_exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);

    // Extract: elem = opt_result.data.Some._0
    let elem_local = builder.enum_field_load(
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
    ctx.restore_locals(saved_iter);
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
                      "double", "float", "bool", "GorgetStringView", "GorgetString"];
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

/// Map a GIR/mangled type name to its C runtime type name for inline C codegen.
/// GorgetStringView is an internal name; the C runtime uses "Str".
fn to_c_type_name(gir_name: &str) -> String {
    match gir_name {
        "GorgetStringView" => "Str".to_string(),
        _ => gir_name.to_string(),
    }
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
    let saved_range = ctx.save_locals();
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
    ctx.push_loop(incr_bb, break_exit_bb);
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

    ctx.restore_locals(saved_range);

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
