use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr};
use crate::span::Spanned;

use super::context::LoweringContext;

/// Lower an expression to GIR instructions, returning the result `Operand`.
pub fn lower_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) -> Operand {
    lower_expr_inner(ctx, builder, expr, None)
}

/// Lower an expression with optional type registry access for mutable operations.
fn lower_expr_inner(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
    _registry: Option<&mut TypeRegistry>,
) -> Operand {
    match &expr.node {
        Expr::IntLiteral(n) => Operand::Constant(Constant::I64(*n)),

        Expr::FloatLiteral(n) => Operand::Constant(Constant::F64(*n)),

        Expr::BoolLiteral(b) => Operand::Constant(Constant::Bool(*b)),

        Expr::StringLiteral(lit) => {
            if !lit.has_interpolation() {
                let text = lit.as_plain_text();
                Operand::Constant(Constant::Str(text))
            } else {
                // Interpolated string literals that aren't print calls —
                // for Phase 1 we just concat literal segments.
                let text = lit.as_plain_text();
                Operand::Constant(Constant::Str(text))
            }
        }

        Expr::Identifier(name) => {
            if let Some((local_id, _)) = ctx.lookup_local(name) {
                Operand::Copy(Place::local(local_id))
            } else {
                // Could be a function name or unknown — produce a constant placeholder
                Operand::Constant(Constant::I64(0))
            }
        }

        Expr::BinaryOp { left, op, right } => {
            lower_binary_op(ctx, builder, left, *op, right)
        }

        Expr::UnaryOp { op, operand } => {
            lower_unary_op(ctx, builder, *op, operand)
        }

        Expr::Call { callee, args, generic_args } => {
            lower_call(ctx, builder, callee, args, generic_args.as_deref())
        }

        // -- P2.1: Struct operations --
        Expr::StructLiteral { name, args, generic_args } => {
            lower_struct_literal(ctx, builder, &name.node, args, generic_args.as_deref())
        }

        Expr::FieldAccess { object, field } => {
            lower_field_access(ctx, builder, object, &field.node)
        }

        // -- P2.2: Method calls --
        Expr::MethodCall { receiver, method, args, .. } => {
            lower_method_call(ctx, builder, receiver, &method.node, args)
        }

        // -- Index access --
        Expr::Index { object, index } => {
            lower_index_access(ctx, builder, object, index)
        }

        // -- P2.6: Move/Borrow --
        Expr::Move { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            // After extracting the value, zero the source and mark as maybe-moved
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                builder.move_zero(place.clone());
                ctx.drops.mark_moved(place.local);
            }
            val
        }

        Expr::MutableBorrow { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let local_type = if (place.local.0 as usize) < builder.locals.len() {
                    builder.locals[place.local.0 as usize].type_id
                } else {
                    UNIT_TYPE
                };
                // Create a mutable pointer — we don't have registry here,
                // so use UNIT_TYPE as placeholder for the pointer type
                let _ = local_type;
                let dst = builder.add_local(UNIT_TYPE, None);
                builder.emit_borrow_mut(dst, place.clone());
                return FunctionBuilder::copy(dst);
            }
            val
        }

        // -- P2.4: Closures --
        Expr::Closure { params, body, is_move, .. } => {
            let mut cl = std::mem::take(&mut ctx.closures);
            let result = cl.lower_closure(ctx, builder, params, body, *is_move);
            ctx.closures = cl;
            result
        }

        // -- If expression (ternary) --
        Expr::If { condition, then_branch, else_branch, .. } => {
            lower_if_expr(ctx, builder, condition, then_branch, else_branch.as_deref())
        }

        // -- P3.2: Match expression --
        Expr::Match { scrutinee, arms, else_arm } => {
            lower_match_expr(ctx, builder, scrutinee, arms, else_arm.as_deref())
        }

        // -- P3.3: Try/error handling --
        Expr::Try { expr: inner } => {
            lower_try_expr(ctx, builder, inner)
        }

        Expr::TryCapture { expr: inner } => {
            // try expr → evaluate, return zero-default on error
            let val = lower_expr(ctx, builder, inner);
            val // simplified: just return the value
        }

        // -- P3.4: Miscellaneous expressions --
        Expr::CharLiteral(ch) => {
            Operand::Constant(Constant::I64(*ch as i64))
        }

        Expr::NoneLiteral => {
            Operand::Constant(Constant::Null)
        }

        Expr::SelfExpr => {
            if let Some((local_id, _)) = ctx.lookup_local("self") {
                Operand::Copy(Place::local(local_id))
            } else {
                Operand::Constant(Constant::Unit)
            }
        }

        Expr::It => {
            if let Some((local_id, _)) = ctx.lookup_local("it") {
                Operand::Copy(Place::local(local_id))
            } else {
                Operand::Constant(Constant::Unit)
            }
        }

        Expr::Block(block) => {
            lower_block_expr(ctx, builder, block)
        }

        Expr::Do { body } => {
            lower_block_expr(ctx, builder, body)
        }

        Expr::As { expr: inner, type_ } => {
            let val = lower_expr(ctx, builder, inner);
            let target_type = ctx.type_mapper.map_ast_type(&type_.node);
            let dst = builder.cast(target_type, val);
            FunctionBuilder::copy(dst)
        }

        Expr::TupleLiteral(elems) => {
            let operands: Vec<Operand> = elems.iter()
                .map(|e| lower_expr(ctx, builder, e))
                .collect();
            let type_id = UNIT_TYPE; // placeholder tuple type
            let dst = builder.tuple_init(operands, type_id);
            FunctionBuilder::copy(dst)
        }

        Expr::ArrayLiteral(elems) => {
            // Lower each element — for now, emit as a series of extern calls
            // to build an array through the runtime
            let _operands: Vec<Operand> = elems.iter()
                .map(|e| lower_expr(ctx, builder, e))
                .collect();
            Operand::Constant(Constant::Unit) // array construction deferred
        }

        Expr::Deref { expr: inner } => {
            let val = lower_expr(ctx, builder, inner);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = val {
                let mut deref_place = place.clone();
                deref_place.projections.push(Projection::Deref);
                // Need to determine the dereferenced type
                let local_idx = place.local.0 as usize;
                let deref_type = if local_idx < builder.locals.len() {
                    let ptr_type = builder.locals[local_idx].type_id;
                    ctx.pointee_type(ptr_type).unwrap_or(I64_TYPE)
                } else {
                    I64_TYPE
                };
                let dst = builder.add_local(deref_type, None);
                builder.assign(Place::local(dst), Operand::Copy(deref_place));
                return FunctionBuilder::copy(dst);
            }
            val
        }

        Expr::TupleFieldAccess { object, index } => {
            let obj = lower_expr(ctx, builder, object);
            if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
                let elem_type = I64_TYPE; // placeholder
                let dst = builder.field_load(place.clone(), *index as u32, elem_type);
                return FunctionBuilder::copy(dst);
            }
            Operand::Constant(Constant::Unit)
        }

        Expr::Is { expr: inner, negated, pattern } => {
            let val = lower_expr(ctx, builder, inner);
            let scrut_type = infer_operand_type(ctx, &val);
            let scrut_local = builder.add_local(scrut_type, None);
            builder.assign(Place::local(scrut_local), val);

            let cond = super::stmts::lower_pattern_condition(
                ctx, builder, pattern, scrut_local, scrut_type,
            );
            if *negated {
                let neg = builder.un_op(UnOp::Not, BOOL_TYPE, cond);
                FunctionBuilder::copy(neg)
            } else {
                cond
            }
        }

        Expr::NilCoalescing { lhs, rhs } => {
            // lhs ?? rhs: check if lhs is None, if so evaluate rhs
            let lhs_val = lower_expr(ctx, builder, lhs);
            let lhs_type = infer_operand_type(ctx, &lhs_val);
            let lhs_local = builder.add_local(lhs_type, None);
            builder.assign(Place::local(lhs_local), lhs_val);

            // Check tag: if tag != None_tag, use lhs; else evaluate rhs
            let tag = builder.tag_of(FunctionBuilder::copy(lhs_local));
            // None is conventionally the last variant (tag = num_variants - 1)
            // Use 0 as "has value" heuristic: tag == 0 means first variant (Some/Ok)
            let is_some = builder.cmp(
                CmpOp::Eq,
                I32_TYPE,
                FunctionBuilder::copy(tag),
                Operand::Constant(Constant::I32(0)),
            );

            let result_id = builder.add_local(lhs_type, None);
            let then_bb = builder.new_block();
            let else_bb = builder.new_block();
            let merge_bb = builder.new_block();

            builder.branch(FunctionBuilder::copy(is_some), then_bb, else_bb);

            // Has value: extract it (field 0 of variant 0)
            builder.switch_to(then_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::copy(lhs_local));
            builder.jump(merge_bb);

            // None: evaluate rhs
            builder.switch_to(else_bb);
            let rhs_val = lower_expr(ctx, builder, rhs);
            builder.assign(Place::local(result_id), rhs_val);
            builder.jump(merge_bb);

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_id)
        }

        Expr::Path { segments } => {
            // Qualified path — try to resolve as enum variant
            if let Some(last) = segments.last() {
                if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(&last.node) {
                    let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
                    let dst = builder.enum_init(&enum_name, &variant_name, type_id, vec![]);
                    return FunctionBuilder::copy(dst);
                }
                // Try as identifier
                if let Some((local_id, _)) = ctx.lookup_local(&last.node) {
                    return Operand::Copy(Place::local(local_id));
                }
            }
            Operand::Constant(Constant::Unit)
        }

        // Deferred: Spawn, Await, Select, Comprehensions
        _ => Operand::Constant(Constant::Unit),
    }
}

/// Lower a struct literal (constructor call).
fn lower_struct_literal(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    name: &str,
    args: &[Spanned<Expr>],
    generic_args: Option<&[Spanned<ast::Type>]>,
) -> Operand {
    // Determine the effective type name (mangled if generic)
    let effective_name = if let Some(type_args) = generic_args {
        if !type_args.is_empty() {
            super::types::mangle_generic_name(name, type_args)
        } else {
            name.to_string()
        }
    } else {
        name.to_string()
    };

    // Check if this is an enum variant constructor
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(&effective_name) {
        let field_operands: Vec<Operand> = args.iter()
            .map(|arg| lower_expr(ctx, builder, arg))
            .collect();

        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
        return FunctionBuilder::copy(dst);
    }
    // Also check the base name for non-generic enum variants
    if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
        let field_operands: Vec<Operand> = args.iter()
            .map(|arg| lower_expr(ctx, builder, arg))
            .collect();

        let type_id = ctx.type_mapper.lookup_named(&enum_name).unwrap_or(UNIT_TYPE);
        let dst = builder.enum_init(&enum_name, &variant_name, type_id, field_operands);
        return FunctionBuilder::copy(dst);
    }

    // Regular struct literal
    let field_operands: Vec<Operand> = args.iter()
        .map(|arg| lower_expr(ctx, builder, arg))
        .collect();

    let type_id = ctx.type_mapper.lookup_named(&effective_name).unwrap_or(UNIT_TYPE);
    let dst = builder.struct_init(&effective_name, type_id, field_operands);
    FunctionBuilder::copy(dst)
}

/// Lower a field access expression.
fn lower_field_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    field_name: &str,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let local_idx = place.local.0 as usize;
        if local_idx < builder.locals.len() {
            let local_type_id = builder.locals[local_idx].type_id;

            // If the local is a pointer (e.g., self in equip methods), dereference it
            // to get the underlying struct type for field access.
            let (effective_type_id, base_place) =
                if let Some(pointee) = ctx.pointee_type(local_type_id) {
                    // Pointer type: add Deref projection → (*_N).field
                    let mut deref_place = place.clone();
                    deref_place.projections.push(Projection::Deref);
                    (pointee, deref_place)
                } else {
                    (local_type_id, place.clone())
                };

            // Look up the type name, then the field info
            if let Some(type_name) = ctx.type_name_for_id(effective_type_id) {
                if let Some((field_idx, field_type)) = ctx.lookup_field(type_name, field_name) {
                    let dst = builder.field_load(base_place, field_idx, field_type);
                    return FunctionBuilder::copy(dst);
                }
            }
        }
    }

    // Fallback: can't resolve field
    Operand::Constant(Constant::Unit)
}

/// Lower a method call on a concrete (non-trait-object) type.
fn lower_method_call(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    receiver: &Spanned<Expr>,
    method_name: &str,
    args: &[Spanned<ast::CallArg>],
) -> Operand {
    let recv = lower_expr(ctx, builder, receiver);

    // Determine the receiver type and mangle the method name
    let type_name = infer_type_name_from_operand(ctx, &recv);

    if let Some(type_name) = type_name {
        let mangled = format!("{type_name}__{method_name}");

        // Build args: &receiver + explicit args
        let mut call_args = Vec::new();

        // Create a borrow of the receiver for the self parameter
        if let Operand::Copy(ref place) | Operand::Move(ref place) = recv {
            let recv_type_id = if (place.local.0 as usize) < builder.locals.len() {
                builder.locals[place.local.0 as usize].type_id
            } else {
                UNIT_TYPE
            };
            let ptr_local = builder.add_local(recv_type_id, None); // placeholder type
            builder.emit_borrow(ptr_local, place.clone());
            call_args.push(FunctionBuilder::copy(ptr_local));
        } else {
            call_args.push(recv);
        }

        for arg in args {
            call_args.push(lower_expr(ctx, builder, &arg.node.value));
        }

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(&mangled) {
            *ret
        } else {
            UNIT_TYPE
        };

        if ret_type == UNIT_TYPE {
            builder.call_void(mangled, call_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(mangled, call_args, ret_type);
            FunctionBuilder::copy(dst)
        }
    } else {
        // Can't determine receiver type — fallback
        Operand::Constant(Constant::Unit)
    }
}

/// Lower an index access expression.
fn lower_index_access(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    object: &Spanned<Expr>,
    index: &Spanned<Expr>,
) -> Operand {
    let obj = lower_expr(ctx, builder, object);
    let idx = lower_expr(ctx, builder, index);

    if let Operand::Copy(ref place) | Operand::Move(ref place) = obj {
        let elem_type = I64_TYPE; // placeholder — real type needs registry
        let dst = builder.index_load(place.clone(), idx, elem_type);
        return FunctionBuilder::copy(dst);
    }

    Operand::Constant(Constant::Unit)
}

/// Lower an if expression (used as ternary).
fn lower_if_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_branch: &Spanned<Expr>,
    else_branch: Option<&Spanned<Expr>>,
) -> Operand {
    let cond = lower_expr(ctx, builder, condition);

    // Allocate a result local
    let result_type = infer_operand_type(ctx, &cond);
    let _ = result_type;
    let result_id = builder.add_local(I64_TYPE, None); // placeholder type

    let then_bb = builder.new_block();
    let else_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(cond, then_bb, else_bb);

    builder.switch_to(then_bb);
    let then_val = lower_expr(ctx, builder, then_branch);
    builder.assign(Place::local(result_id), then_val);
    builder.jump(merge_bb);

    builder.switch_to(else_bb);
    if let Some(else_expr) = else_branch {
        let else_val = lower_expr(ctx, builder, else_expr);
        builder.assign(Place::local(result_id), else_val);
    } else {
        builder.assign(Place::local(result_id), Operand::Constant(Constant::Unit));
    }
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}

/// Infer the type name of an operand (for method dispatch).
fn infer_type_name_from_operand(ctx: &LoweringContext, operand: &Operand) -> Option<String> {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    // Resolve through pointer types (e.g., self in equip methods is Ptr(T))
                    let effective_tid = ctx.pointee_type(*tid).unwrap_or(*tid);

                    // Check if this is a Named type
                    if let Some(name) = ctx.type_mapper.named_types.iter()
                        .find_map(|(name, &id)| if id == effective_tid { Some(name.clone()) } else { None })
                    {
                        return Some(name);
                    }
                    return None;
                }
            }
            None
        }
        _ => None,
    }
}

/// Lower a binary operation.
fn lower_binary_op(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    left: &Spanned<Expr>,
    op: ast::BinaryOp,
    right: &Spanned<Expr>,
) -> Operand {
    use ast::BinaryOp as AstOp;

    // Short-circuit: `and` / `or`
    if op == AstOp::And || op == AstOp::Or {
        return lower_short_circuit(ctx, builder, left, op, right);
    }

    let lhs = lower_expr(ctx, builder, left);
    let rhs = lower_expr(ctx, builder, right);

    // Determine result type from lhs operand type
    let operand_type = infer_operand_type(ctx, &lhs);

    match op {
        // Comparison operators → bool result
        AstOp::Eq | AstOp::Neq | AstOp::Lt | AstOp::Gt | AstOp::LtEq | AstOp::GtEq => {
            let cmp_op = match op {
                AstOp::Eq => CmpOp::Eq,
                AstOp::Neq => CmpOp::Ne,
                AstOp::Lt => CmpOp::Lt,
                AstOp::Gt => CmpOp::Gt,
                AstOp::LtEq => CmpOp::Le,
                AstOp::GtEq => CmpOp::Ge,
                _ => unreachable!(),
            };
            let dst = builder.cmp(cmp_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }

        // Arithmetic operators → same type as operands
        _ => {
            let bin_op = match op {
                AstOp::Add => BinOp::Add,
                AstOp::Sub => BinOp::Sub,
                AstOp::Mul => BinOp::Mul,
                AstOp::Div => BinOp::Div,
                AstOp::Mod => BinOp::Rem,
                AstOp::BitAnd => BinOp::BitAnd,
                AstOp::BitOr => BinOp::BitOr,
                AstOp::BitXor => BinOp::BitXor,
                AstOp::Shl => BinOp::Shl,
                AstOp::Shr => BinOp::Shr,
                _ => BinOp::Add, // fallback
            };
            let dst = builder.bin_op(bin_op, operand_type, lhs, rhs);
            FunctionBuilder::copy(dst)
        }
    }
}

/// Lower short-circuit `and`/`or` via basic block branching.
fn lower_short_circuit(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    left: &Spanned<Expr>,
    op: ast::BinaryOp,
    right: &Spanned<Expr>,
) -> Operand {
    let lhs = lower_expr(ctx, builder, left);

    // Allocate a result local
    let result_id = builder.add_local(BOOL_TYPE, None);

    let rhs_bb = builder.new_block();
    let merge_bb = builder.new_block();

    match op {
        ast::BinaryOp::And => {
            let false_bb = builder.new_block();
            builder.branch(lhs, rhs_bb, false_bb);

            // false_bb: assign false, jump merge
            builder.switch_to(false_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));
            builder.jump(merge_bb);

            // rhs_bb: evaluate rhs, assign to result, jump merge
            builder.switch_to(rhs_bb);
            let rhs = lower_expr(ctx, builder, right);
            builder.assign(Place::local(result_id), rhs);
            builder.jump(merge_bb);
        }
        ast::BinaryOp::Or => {
            let true_bb = builder.new_block();
            builder.branch(lhs, true_bb, rhs_bb);

            // true_bb: assign true, jump merge
            builder.switch_to(true_bb);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
            builder.jump(merge_bb);

            // rhs_bb: evaluate rhs, assign to result, jump merge
            builder.switch_to(rhs_bb);
            let rhs = lower_expr(ctx, builder, right);
            builder.assign(Place::local(result_id), rhs);
            builder.jump(merge_bb);
        }
        _ => unreachable!(),
    }

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_id)
}

/// Lower a unary operation.
fn lower_unary_op(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    op: ast::UnaryOp,
    operand: &Spanned<Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, operand);
    let operand_type = infer_operand_type(ctx, &val);

    let gir_op = match op {
        ast::UnaryOp::Neg => UnOp::Neg,
        ast::UnaryOp::Not => UnOp::Not,
        ast::UnaryOp::BitNot => UnOp::BitNot,
        ast::UnaryOp::Deref => return val, // Phase 1: no pointer derefs
    };

    let result_type = if gir_op == UnOp::Not { BOOL_TYPE } else { operand_type };
    let dst = builder.un_op(gir_op, result_type, val);
    FunctionBuilder::copy(dst)
}

/// Lower a function call.
fn lower_call(
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

        // Determine effective function name (mangled if generic call)
        let effective_name = if let Some(type_args) = generic_args {
            if !type_args.is_empty() {
                super::types::mangle_generic_name(name, type_args)
            } else {
                name.clone()
            }
        } else {
            name.clone()
        };

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

        // Regular function call (use effective name for generic functions)
        let lowered_args: Vec<Operand> = args
            .iter()
            .map(|arg| lower_expr(ctx, builder, &arg.node.value))
            .collect();

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(effective_name.as_str()) {
            *ret
        } else {
            I64_TYPE // fallback
        };

        if ret_type == UNIT_TYPE {
            builder.call_void(effective_name, lowered_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(effective_name, lowered_args, ret_type);
            FunctionBuilder::copy(dst)
        }
    } else {
        // Non-identifier callee — not handled in Phase 1
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
                    StringSegment::Interpolation(var_name) => {
                        if let Some((local_id, type_id)) = ctx.lookup_local(var_name) {
                            let spec = ctx.type_mapper.format_specifier(type_id);
                            format_str.push_str(spec);
                            printf_args.push(FunctionBuilder::copy(local_id));
                        } else {
                            // Variable not found — insert literal placeholder
                            format_str.push_str(var_name);
                        }
                    }
                }
            }

            format_str.push('\n');

            let mut all_args = vec![Operand::Constant(Constant::Str(format_str))];
            all_args.extend(printf_args);

            builder.call_extern("printf", all_args, I32_TYPE);
        }
        Expr::Identifier(name) => {
            // print(variable) — determine format from variable type
            if let Some((local_id, type_id)) = ctx.lookup_local(name) {
                let spec = ctx.type_mapper.format_specifier(type_id);
                let fmt = format!("{spec}\n");
                let fmt_op = Operand::Constant(Constant::Str(fmt));
                builder.call_extern(
                    "printf",
                    vec![fmt_op, FunctionBuilder::copy(local_id)],
                    I32_TYPE,
                );
            } else {
                // Unknown variable — print as string
                let fmt = Operand::Constant(Constant::Str("%s\n".to_string()));
                let val = lower_expr(ctx, builder, arg_expr);
                builder.call_extern("printf", vec![fmt, val], I32_TYPE);
            }
        }
        _ => {
            // General expression — lower it and print as int
            let val = lower_expr(ctx, builder, arg_expr);
            let fmt = Operand::Constant(Constant::Str("%lld\n".to_string()));
            builder.call_extern("printf", vec![fmt, val], I32_TYPE);
        }
    }
}

/// Lower a match expression (value-producing).
fn lower_match_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchArm],
    else_arm: Option<&Spanned<Expr>>,
) -> Operand {
    // Lower scrutinee to a temp local
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = infer_operand_type(ctx, &scrut_op);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

    // Allocate result local (placeholder type — will be overwritten)
    let result_local = builder.add_local(I64_TYPE, None);
    let merge_bb = builder.new_block();

    for (i, arm) in arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        let cond = super::stmts::lower_pattern_condition(
            ctx, builder, &arm.pattern, scrut_local, scrut_type,
        );
        builder.branch(cond, arm_body_bb, next_test_bb);

        builder.switch_to(arm_body_bb);
        super::stmts::emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        let arm_val = lower_expr(ctx, builder, &arm.body);
        builder.assign(Place::local(result_local), arm_val);
        builder.jump(merge_bb);

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_expr) = else_arm {
        let else_val = lower_expr(ctx, builder, else_expr);
        builder.assign(Place::local(result_local), else_val);
    } else {
        builder.assign(Place::local(result_local), Operand::Constant(Constant::Unit));
    }
    builder.jump(merge_bb);

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(result_local)
}

/// Lower a try expression (`expr?`) on a Result type.
fn lower_try_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    inner: &Spanned<Expr>,
) -> Operand {
    let val = lower_expr(ctx, builder, inner);
    let val_type = infer_operand_type(ctx, &val);
    let val_local = builder.add_local(val_type, None);
    builder.assign(Place::local(val_local), val);

    // Check tag: 0 = Ok, 1 = Error
    let tag = builder.tag_of(FunctionBuilder::copy(val_local));
    let is_ok = builder.cmp(
        CmpOp::Eq,
        I32_TYPE,
        FunctionBuilder::copy(tag),
        Operand::Constant(Constant::I32(0)),
    );

    let ok_bb = builder.new_block();
    let err_bb = builder.new_block();
    let merge_bb = builder.new_block();

    builder.branch(FunctionBuilder::copy(is_ok), ok_bb, err_bb);

    // Ok path: extract Ok value (field 0 of variant 0)
    builder.switch_to(ok_bb);
    let ok_val = builder.enum_field_load(
        Place::local(val_local),
        "Ok",
        0,
        I64_TYPE, // placeholder Ok field type
    );
    builder.jump(merge_bb);

    // Error path: propagate error via early return
    builder.switch_to(err_bb);
    let err_val = builder.enum_field_load(
        Place::local(val_local),
        "Error",
        0,
        I64_TYPE, // placeholder Error field type
    );
    // Build a Result::Error to return
    // For now, assign error to _0 and return
    builder.assign(Place::local(LocalId(0)), FunctionBuilder::copy(err_val));
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, super::drops::DropScopeKind::Function);
    builder.ret(FunctionBuilder::copy(LocalId(0)));

    builder.switch_to(merge_bb);
    FunctionBuilder::copy(ok_val)
}

/// Lower a block expression — the last expression in the block is the value.
fn lower_block_expr(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &ast::Block,
) -> Operand {
    if block.stmts.is_empty() {
        return Operand::Constant(Constant::Unit);
    }

    // Lower all but the last statement normally
    for stmt in &block.stmts[..block.stmts.len() - 1] {
        super::stmts::lower_stmt(ctx, builder, stmt);
    }

    // If the last statement is an expression, lower it and return as value
    let last = &block.stmts[block.stmts.len() - 1];
    match &last.node {
        ast::Stmt::Expr(expr) => lower_expr(ctx, builder, expr),
        _ => {
            super::stmts::lower_stmt(ctx, builder, last);
            Operand::Constant(Constant::Unit)
        }
    }
}

/// Infer the GIR type of an operand by examining its structure.
pub fn infer_operand_type(ctx: &LoweringContext, operand: &Operand) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // Look up the local's type
            for (_, (lid, tid)) in ctx.locals_iter() {
                if *lid == place.local {
                    return *tid;
                }
            }
            I64_TYPE // fallback
        }
        Operand::Constant(c) => match c {
            Constant::Bool(_) => BOOL_TYPE,
            Constant::I8(_) => I8_TYPE,
            Constant::I16(_) => I16_TYPE,
            Constant::I32(_) => I32_TYPE,
            Constant::I64(_) => I64_TYPE,
            Constant::U8(_) => U8_TYPE,
            Constant::U16(_) => U16_TYPE,
            Constant::U32(_) => U32_TYPE,
            Constant::U64(_) => U64_TYPE,
            Constant::F32(_) => F32_TYPE,
            Constant::F64(_) => F64_TYPE,
            Constant::Str(_) => ctx.type_mapper.str_type,
            Constant::Null => UNIT_TYPE,
            Constant::Unit => UNIT_TYPE,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::TypeRegistry;
    use crate::lexer::token::{StringKind, StringLiteral};
    use crate::parser::ast::CallArg;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn make_test_ctx() -> (crate::semantic::AnalysisResult, LoweringContext<'static>) {
        // We need a 'static AnalysisResult to satisfy lifetime requirements.
        // Use a leaked box for tests only.
        let analysis = Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = super::super::types::TypeMapper::new(&mut reg);
        let ctx = LoweringContext::new(analysis, mapper, reg);
        // Return a dummy analysis (not used) and the context
        (crate::ir::lowering::empty_analysis_for_test(), ctx)
    }

    #[test]
    fn lower_literals() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let int_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::IntLiteral(42)));
        assert!(matches!(int_op, Operand::Constant(Constant::I64(42))));

        let float_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::FloatLiteral(3.14)));
        assert!(matches!(float_op, Operand::Constant(Constant::F64(f)) if (f - 3.14).abs() < 1e-10));

        let bool_op = lower_expr(&mut ctx, &mut builder, &spanned(Expr::BoolLiteral(true)));
        assert!(matches!(bool_op, Operand::Constant(Constant::Bool(true))));

        let str_op = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::StringLiteral(StringLiteral {
                kind: StringKind::Normal,
                segments: vec![StringSegment::Literal("hello".into())],
            })),
        );
        assert!(matches!(str_op, Operand::Constant(Constant::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn lower_binary_op_test() {
        let (_analysis, mut ctx) = make_test_ctx();
        let a_id = LocalId(1);
        let b_id = LocalId(2);
        ctx.register_local("a", a_id, I64_TYPE);
        ctx.register_local("b", b_id, I64_TYPE);

        let mut builder = FunctionBuilder::new(
            "test",
            I64_TYPE,
            &[(I64_TYPE, Some("a")), (I64_TYPE, Some("b"))],
        );

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("a".into()))),
                op: ast::BinaryOp::Add,
                right: Box::new(spanned(Expr::Identifier("b".into()))),
            }),
        );

        assert!(matches!(result, Operand::Copy(_)));
        assert_eq!(builder.blocks[0].instructions.len(), 1);
        assert!(matches!(
            builder.blocks[0].instructions[0],
            Instruction::BinOp { op: BinOp::Add, .. }
        ));
    }

    #[test]
    fn lower_print_interpolation() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let lit = StringLiteral {
            kind: StringKind::Normal,
            segments: vec![StringSegment::Interpolation("x".into())],
        };
        let args = vec![spanned(CallArg {
            name: None,
            ownership: ast::Ownership::Borrow,
            value: spanned(Expr::StringLiteral(lit)),
        })];

        lower_print_call(&mut ctx, &mut builder, &args);

        assert!(!builder.blocks[0].instructions.is_empty());
        assert!(matches!(
            builder.blocks[0].instructions.last().unwrap(),
            Instruction::CallExtern { func, .. } if func == "printf"
        ));
    }

    // ---- P3.2: Match expression tests ----

    #[test]
    fn lower_match_expr_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", I64_TYPE, &[(I64_TYPE, Some("x"))]);

        use crate::parser::ast::{MatchArm, Pattern};

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Match {
                scrutinee: Box::new(spanned(Expr::Identifier("x".into()))),
                arms: vec![
                    MatchArm {
                        pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                        guard: None,
                        body: spanned(Expr::IntLiteral(10)),
                        span: Span { start: 0, end: 0 },
                    },
                    MatchArm {
                        pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                        guard: None,
                        body: spanned(Expr::IntLiteral(20)),
                        span: Span { start: 0, end: 0 },
                    },
                ],
                else_arm: Some(Box::new(spanned(Expr::IntLiteral(0)))),
            }),
        );

        // Result should be a Copy of the result local
        assert!(matches!(result, Operand::Copy(_)));

        // Should have Branch terminators for pattern checks
        let has_branch = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Branch { .. }))
        });
        assert!(has_branch, "Match expr should have Branch terminators");

        // Should have Assign to result local in arm bodies
        let assign_count: usize = builder.blocks.iter()
            .map(|bb| bb.instructions.iter()
                .filter(|inst| matches!(inst, Instruction::Assign { .. }))
                .count())
            .sum();
        assert!(assign_count >= 3, "Should have assigns for scrutinee + arms");
    }

    // ---- P3.4: Miscellaneous expression tests ----

    #[test]
    fn lower_char_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::CharLiteral('A')));
        assert!(matches!(result, Operand::Constant(Constant::I64(65))));
    }

    #[test]
    fn lower_self_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let self_id = LocalId(1);
        ctx.register_local("self", self_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("self"))]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::SelfExpr));
        assert!(matches!(result, Operand::Copy(ref p) if p.local == LocalId(1)));
    }

    #[test]
    fn lower_block_expr_test() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", I64_TYPE, &[]);

        use crate::parser::ast::{Block, Stmt};

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Block(Block {
                stmts: vec![spanned(Stmt::Expr(spanned(Expr::IntLiteral(42))))],
                span: Span { start: 0, end: 0 },
            })),
        );

        // The block's last expression (42) should be the value
        assert!(matches!(result, Operand::Constant(Constant::I64(42))));
    }

    #[test]
    fn lower_cast_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::As {
                expr: Box::new(spanned(Expr::IntLiteral(42))),
                type_: spanned(ast::Type::Primitive(ast::PrimitiveType::Float)),
            }),
        );

        // Should produce a Copy of the cast result local
        assert!(matches!(result, Operand::Copy(_)));
        // Should have a Cast instruction
        let has_cast = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::Cast { .. })
        });
        assert!(has_cast, "Should have Cast instruction for 'as' expression");
    }

    #[test]
    fn lower_tuple_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::TupleLiteral(vec![
                spanned(Expr::IntLiteral(1)),
                spanned(Expr::IntLiteral(2)),
                spanned(Expr::IntLiteral(3)),
            ])),
        );

        assert!(matches!(result, Operand::Copy(_)));
        // Should have a TupleInit instruction
        let has_tuple_init = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::TupleInit { .. })
        });
        assert!(has_tuple_init, "Should have TupleInit instruction");
    }

    #[test]
    fn lower_is_expr() {
        let (_analysis, mut ctx) = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        use crate::parser::ast::Pattern;

        let result = lower_expr(
            &mut ctx,
            &mut builder,
            &spanned(Expr::Is {
                expr: Box::new(spanned(Expr::Identifier("x".into()))),
                negated: false,
                pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(5))))),
            }),
        );

        // Should produce a boolean condition (Copy of Cmp result)
        assert!(matches!(result, Operand::Copy(_)));
    }

    #[test]
    fn lower_none_literal() {
        let (_analysis, mut ctx) = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let result = lower_expr(&mut ctx, &mut builder, &spanned(Expr::NoneLiteral));
        assert!(matches!(result, Operand::Constant(Constant::Null)));
    }
}
