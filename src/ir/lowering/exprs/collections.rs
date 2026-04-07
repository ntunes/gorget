//! Array/dict/set literal lowering, comprehensions, optional chaining, and range expressions.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{Expr, Pattern};
use crate::span::Spanned;

use super::super::context::LoweringContext;
use super::{lower_expr, infer_operand_type_full, infer_collection_element_type};

/// Lower `[e1, e2, ...]` to `gorget_array_new(sizeof(elem))` + N `gorget_array_push` calls.
pub(super) fn lower_array_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    elems: &[Spanned<Expr>],
) -> Operand {
    let array_type = ctx.type_mapper.lookup_named("GorgetArray").unwrap_or(UNIT_TYPE);

    // Infer element type from first element
    let elem_type = if !elems.is_empty() {
        let first = lower_expr(ctx, builder, &elems[0]);
        let etype = infer_operand_type_full(ctx, &first, builder);
        // Create the array
        let arr_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(etype))],
            array_type,
        );
        // Push first element
        let elem_local = builder.add_local(etype, None);
        builder.assign(Place::local(elem_local), first);
        let ref_local = builder.borrow(Place::local(elem_local), ctx.register_ptr_type(etype));
        let arr_ref = builder.borrow_mut(Place::local(arr_local), ctx.register_mut_ptr_type(array_type));
        builder.call_extern(
            "gorget_array_push",
            vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(ref_local)],
            UNIT_TYPE,
        );
        // Push remaining elements
        for elem_expr in &elems[1..] {
            let elem_val = lower_expr(ctx, builder, elem_expr);
            let el = builder.add_local(etype, None);
            builder.assign(Place::local(el), elem_val);
            let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(etype));
            let ar_ref = builder.borrow_mut(Place::local(arr_local), ctx.register_mut_ptr_type(array_type));
            builder.call_extern(
                "gorget_array_push",
                vec![FunctionBuilder::copy(ar_ref), FunctionBuilder::copy(el_ref)],
                UNIT_TYPE,
            );
        }
        FunctionBuilder::copy(arr_local)
    } else {
        // Empty array — infer element size from expected type if available.
        // Without this, Vector[LargeStruct] initialized as [] gets elem_size=8
        // instead of sizeof(LargeStruct), causing buffer overflows on push.
        let elem_size_type = ctx.func_state.expected_type
            .map(|et| infer_collection_element_type(ctx, et))
            .unwrap_or(I64_TYPE);
        let arr_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(elem_size_type))],
            array_type,
        );
        FunctionBuilder::copy(arr_local)
    };
    elem_type
}

// ---- Dict Literals ----

/// Lower `{"a": 1, "b": 2}` to `Dict__K__V__new()` + N `Dict__K__V__put()` calls.
pub(super) fn lower_dict_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pairs: &[(Spanned<Expr>, Spanned<Expr>)],
) -> Operand {
    if pairs.is_empty() {
        // Use expected type from VarDecl context to determine dict type
        if let Some(expected_type) = ctx.func_state.expected_type {
            if let Some(type_name) = ctx.type_registry.type_name(expected_type) {
                if type_name.starts_with("Dict__") || type_name.starts_with("HashMap__") {
                    let new_fn = format!("{type_name}__new");
                    let dict_local = builder.call_extern(&new_fn, vec![], expected_type);
                    return FunctionBuilder::copy(dict_local);
                }
            }
        }
        return Operand::Constant(Constant::Unit);
    }

    // Lower first pair to infer key/value types
    let first_key = lower_expr(ctx, builder, &pairs[0].0);
    let first_val = lower_expr(ctx, builder, &pairs[0].1);
    let key_type = infer_operand_type_full(ctx, &first_key, builder);
    let val_type = infer_operand_type_full(ctx, &first_val, builder);

    // Compute mangled dict type name
    let key_c = type_id_to_mangle_name(ctx, key_type);
    let val_c = type_id_to_mangle_name(ctx, val_type);
    let mangled = format!("Dict__{key_c}__{val_c}");

    // Register dict type if not present
    let dict_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
        tid
    } else {
        let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
        ctx.type_mapper.register_named(mangled.clone(), tid);
        tid
    };

    let new_fn = format!("{mangled}__new");
    let put_fn = format!("{mangled}__put");

    // Create the dict
    let dict_local = builder.call_extern(&new_fn, vec![], dict_type);

    // Insert first pair
    let dict_ref = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
    builder.call_extern(
        &put_fn,
        vec![FunctionBuilder::copy(dict_ref), first_key, first_val],
        UNIT_TYPE,
    );

    // Insert remaining pairs
    for (key_expr, val_expr) in &pairs[1..] {
        let k = lower_expr(ctx, builder, key_expr);
        let v = lower_expr(ctx, builder, val_expr);
        let dr = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
        builder.call_extern(
            &put_fn,
            vec![FunctionBuilder::copy(dr), k, v],
            UNIT_TYPE,
        );
    }

    FunctionBuilder::copy(dict_local)
}

/// Map a TypeId to a C-compatible mangle fragment for dict/set type names.
fn type_id_to_mangle_name(ctx: &LoweringContext, type_id: TypeId) -> String {
    if type_id == I64_TYPE { return "int64_t".to_string(); }
    if type_id == I32_TYPE { return "int32_t".to_string(); }
    if type_id == I16_TYPE { return "int16_t".to_string(); }
    if type_id == I8_TYPE { return "int8_t".to_string(); }
    if type_id == U64_TYPE { return "uint64_t".to_string(); }
    if type_id == U32_TYPE { return "uint32_t".to_string(); }
    if type_id == U16_TYPE { return "uint16_t".to_string(); }
    if type_id == U8_TYPE { return "uint8_t".to_string(); }
    if type_id == F64_TYPE { return "double".to_string(); }
    if type_id == F32_TYPE { return "float".to_string(); }
    if type_id == BOOL_TYPE { return "bool".to_string(); }
    if type_id == ctx.type_mapper.owned_string_type { return "GorgetString".to_string(); }
    if type_id == ctx.type_mapper.owned_string_type { return "GorgetString".to_string(); }
    // Named types
    if let Some(name) = ctx.type_name_for_id(type_id) {
        return name.to_string();
    }
    "int64_t".to_string() // fallback
}

// ---- List Comprehensions ----

/// Lower `[expr for var in iterable if condition]` to a loop that builds an array.
pub(super) fn lower_list_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    comp_expr: &Spanned<Expr>,
    variable: &Spanned<Pattern>,
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let array_type = ctx.type_mapper.lookup_named("GorgetArray").unwrap_or(UNIT_TYPE);

    // Only handle range iterables for now
    if let Expr::Range { start: Some(start), end: Some(end), inclusive } = &iterable.node {
        // Create accumulator array (use I64 as default element size)
        let acc_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(I64_TYPE))],
            array_type,
        );

        // Create loop variable
        let var_name = match &variable.node {
            Pattern::Binding(name) => name.clone(),
            _ => "_comp_var".to_string(),
        };
        let loop_var = builder.add_local(I64_TYPE, Some(&var_name));
        let start_val = lower_expr(ctx, builder, start);
        builder.assign(Place::local(loop_var), start_val);
        ctx.register_local(&var_name, loop_var, I64_TYPE);

        let header_bb = builder.new_block();
        let body_bb = builder.new_block();
        let push_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        // Header: compare loop var with end
        builder.switch_to(header_bb);
        let end_val = lower_expr(ctx, builder, end);
        let cmp_op = if *inclusive { CmpOp::Le } else { CmpOp::Lt };
        let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        // Body: optionally check condition
        builder.switch_to(body_bb);
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, push_bb.unwrap(), incr_bb);
            builder.switch_to(push_bb.unwrap());
        }

        // Push element
        let elem_val = lower_expr(ctx, builder, comp_expr);
        let elem_type = infer_operand_type_full(ctx, &elem_val, builder);
        let el = builder.add_local(elem_type, None);
        builder.assign(Place::local(el), elem_val);
        let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(elem_type));
        let arr_ref = builder.borrow_mut(Place::local(acc_local), ctx.register_mut_ptr_type(array_type));
        builder.call_extern(
            "gorget_array_push",
            vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
            UNIT_TYPE,
        );
        builder.jump(incr_bb);

        // Increment
        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
        builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        // Exit
        builder.switch_to(exit_bb);
        FunctionBuilder::copy(acc_local)
    } else {
        // Non-range iterables (e.g. vector variables): iterate by index
        let iter_op = lower_expr(ctx, builder, iterable);
        let iter_type = infer_operand_type_full(ctx, &iter_op, builder);
        let iter_local = builder.add_local(iter_type, None);
        builder.assign(Place::local(iter_local), iter_op);

        // Get element type from collection
        let elem_type = infer_collection_element_type(ctx, iter_type);

        // Create accumulator array with correct element size
        let acc_local = builder.call_extern(
            "gorget_array_new",
            vec![Operand::Constant(Constant::SizeOf(elem_type))],
            array_type,
        );

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
        let push_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        // Header: idx < len
        builder.switch_to(header_bb);
        let cond = builder.cmp(CmpOp::Lt, I64_TYPE, FunctionBuilder::copy(idx), FunctionBuilder::copy(len));
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        // Body
        builder.switch_to(body_bb);

        // Register comprehension variable: elem = iter[idx]
        let var_name = match &variable.node {
            Pattern::Binding(name) => name.clone(),
            _ => "_comp_var".to_string(),
        };
        let elem = builder.index_load(Place::local(iter_local), FunctionBuilder::copy(idx), elem_type);
        ctx.register_local(&var_name, elem, elem_type);

        // Optionally check condition (filter)
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, push_bb.unwrap(), incr_bb);
            builder.switch_to(push_bb.unwrap());
        }

        // Push element
        let elem_val = lower_expr(ctx, builder, comp_expr);
        let pushed_elem_type = infer_operand_type_full(ctx, &elem_val, builder);
        let el = builder.add_local(pushed_elem_type, None);
        builder.assign(Place::local(el), elem_val);
        let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(pushed_elem_type));
        let arr_ref = builder.borrow_mut(Place::local(acc_local), ctx.register_mut_ptr_type(array_type));
        builder.call_extern(
            "gorget_array_push",
            vec![FunctionBuilder::copy(arr_ref), FunctionBuilder::copy(el_ref)],
            UNIT_TYPE,
        );
        builder.jump(incr_bb);

        // Increment
        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(idx), one);
        builder.assign(Place::local(idx), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        // Exit
        builder.switch_to(exit_bb);
        FunctionBuilder::copy(acc_local)
    }
}

// ---- Dict and Set Comprehensions ----

/// Lower `{key: value for var in iterable if condition}`.
pub(super) fn lower_dict_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    key_expr: &Spanned<Expr>,
    val_expr: &Spanned<Expr>,
    variables: &[Spanned<String>],
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    // Only handle range iterables for now
    if let Expr::Range { start: Some(start), end: Some(end), inclusive } = &iterable.node {
        let var_name = if let Some(first) = variables.first() {
            first.node.clone()
        } else {
            "_dict_comp_var".to_string()
        };

        // We need to infer dict type — use I64 placeholders
        let mangled = "Dict__int64_t__int64_t".to_string();
        let dict_type = if let Some(tid) = ctx.type_mapper.lookup_named(&mangled) {
            tid
        } else {
            let tid = ctx.type_registry.insert(GirType::Named(mangled.clone()));
            ctx.type_mapper.register_named(mangled.clone(), tid);
            tid
        };

        let new_fn = format!("{mangled}__new");
        let put_fn = format!("{mangled}__put");

        let dict_local = builder.call_extern(&new_fn, vec![], dict_type);

        // Create loop variable
        let loop_var = builder.add_local(I64_TYPE, Some(&var_name));
        let start_val = lower_expr(ctx, builder, start);
        builder.assign(Place::local(loop_var), start_val);
        ctx.register_local(&var_name, loop_var, I64_TYPE);

        let header_bb = builder.new_block();
        let body_bb = builder.new_block();
        let put_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        builder.switch_to(header_bb);
        let end_val = lower_expr(ctx, builder, end);
        let cmp_op = if *inclusive { CmpOp::Le } else { CmpOp::Lt };
        let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        builder.switch_to(body_bb);
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, put_bb.unwrap(), incr_bb);
            builder.switch_to(put_bb.unwrap());
        }

        let k = lower_expr(ctx, builder, key_expr);
        let v = lower_expr(ctx, builder, val_expr);
        let dr = builder.borrow_mut(Place::local(dict_local), ctx.register_mut_ptr_type(dict_type));
        builder.call_extern(&put_fn, vec![FunctionBuilder::copy(dr), k, v], UNIT_TYPE);
        builder.jump(incr_bb);

        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
        builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        builder.switch_to(exit_bb);
        FunctionBuilder::copy(dict_local)
    } else {
        builder.nop();
        Operand::Constant(Constant::Unit)
    }
}

/// Lower `{expr for var in iterable if condition}` (set comprehension).
pub(super) fn lower_set_comprehension(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    comp_expr: &Spanned<Expr>,
    variable: &Spanned<String>,
    iterable: &Spanned<Expr>,
    condition: Option<&Spanned<Expr>>,
) -> Operand {
    let set_type = ctx.type_mapper.lookup_named("GorgetSet")
        .or_else(|| ctx.type_mapper.lookup_named("GorgetArray"))
        .unwrap_or(UNIT_TYPE);

    // Only handle range iterables for now
    if let Expr::Range { start: Some(start), end: Some(end), inclusive } = &iterable.node {
        let acc_local = builder.call_extern(
            "gorget_set_new",
            vec![Operand::Constant(Constant::SizeOf(I64_TYPE))],
            set_type,
        );

        let var_name = &variable.node;
        let loop_var = builder.add_local(I64_TYPE, Some(var_name));
        let start_val = lower_expr(ctx, builder, start);
        builder.assign(Place::local(loop_var), start_val);
        ctx.register_local(var_name, loop_var, I64_TYPE);

        let header_bb = builder.new_block();
        let body_bb = builder.new_block();
        let push_bb = if condition.is_some() { Some(builder.new_block()) } else { None };
        let incr_bb = builder.new_block();
        let exit_bb = builder.new_block();

        builder.jump(header_bb);

        builder.switch_to(header_bb);
        let end_val = lower_expr(ctx, builder, end);
        let cmp_op = if *inclusive { CmpOp::Le } else { CmpOp::Lt };
        let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
        builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

        builder.switch_to(body_bb);
        if let Some(cond_expr) = condition {
            let filter = lower_expr(ctx, builder, cond_expr);
            builder.branch(filter, push_bb.unwrap(), incr_bb);
            builder.switch_to(push_bb.unwrap());
        }

        let elem_val = lower_expr(ctx, builder, comp_expr);
        let elem_type = infer_operand_type_full(ctx, &elem_val, builder);
        let el = builder.add_local(elem_type, None);
        builder.assign(Place::local(el), elem_val);
        let el_ref = builder.borrow(Place::local(el), ctx.register_ptr_type(elem_type));
        let set_ref = builder.borrow_mut(Place::local(acc_local), ctx.register_mut_ptr_type(set_type));
        builder.call_extern(
            "gorget_set_add",
            vec![FunctionBuilder::copy(set_ref), FunctionBuilder::copy(el_ref)],
            UNIT_TYPE,
        );
        builder.jump(incr_bb);

        builder.switch_to(incr_bb);
        let one = Operand::Constant(Constant::I64(1));
        let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
        builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
        builder.jump(header_bb);

        builder.switch_to(exit_bb);
        FunctionBuilder::copy(acc_local)
    } else {
        builder.nop();
        Operand::Constant(Constant::Unit)
    }
}

// ---- Optional Chaining ----

/// Lower `obj?.field` to a null-check + conditional field access.
pub(super) fn lower_optional_chain(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field: &Spanned<String>,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);
    let obj_type = infer_operand_type_full(ctx, &obj, builder);
    let obj_local = builder.add_local(obj_type, None);
    builder.assign(Place::local(obj_local), obj);

    // Check if not null
    let not_null = builder.cmp(
        CmpOp::Ne,
        obj_type,
        FunctionBuilder::copy(obj_local),
        Operand::Constant(Constant::Null),
    );

    let result_local = builder.add_local(I64_TYPE, None); // placeholder result type
    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(not_null), then_bb, else_bb);

    // then: access the field
    builder.switch_to(then_bb);
    // Try to resolve field via struct field cache
    let field_val = if let Some(type_name) = ctx.type_name_for_id(obj_type) {
        if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, &field.node) {
            let dst = builder.field_load(Place::local(obj_local), field_idx, field_type);
            FunctionBuilder::copy(dst)
        } else {
            Operand::Constant(Constant::Null)
        }
    } else {
        // Try through pointer dereference
        if let Some(pointee) = ctx.pointee_type(obj_type) {
            if let Some(type_name) = ctx.type_name_for_id(pointee) {
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, &field.node) {
                    let mut deref_place = Place::local(obj_local);
                    deref_place.projections.push(Projection::Deref);
                    let dst = builder.field_load(deref_place, field_idx, field_type);
                    FunctionBuilder::copy(dst)
                } else {
                    Operand::Constant(Constant::Null)
                }
            } else {
                Operand::Constant(Constant::Null)
            }
        } else {
            Operand::Constant(Constant::Null)
        }
    };
    builder.assign(Place::local(result_local), field_val);
    builder.jump(merge_bb);

    // else: assign null
    builder.switch_to(else_bb);
    builder.assign(Place::local(result_local), Operand::Constant(Constant::Null));
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

// ---- Range Expressions ----

/// Lower `start..end` or `start..=end` to a `GorgetRange` struct init.
pub(super) fn lower_range_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    start: Option<&Spanned<Expr>>,
    end: Option<&Spanned<Expr>>,
    inclusive: bool,
) -> Operand {
    let start_val = if let Some(s) = start {
        lower_expr(ctx, builder, s)
    } else {
        Operand::Constant(Constant::I64(0))
    };
    let end_val = if let Some(e) = end {
        lower_expr(ctx, builder, e)
    } else {
        Operand::Constant(Constant::I64(0))
    };
    let inclusive_val = Operand::Constant(Constant::Bool(inclusive));

    let range_type = ctx.type_mapper.lookup_named("GorgetRange").unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(
        "GorgetRange",
        range_type,
        vec![start_val, end_val, inclusive_val],
    );
    FunctionBuilder::copy(dst)
}
