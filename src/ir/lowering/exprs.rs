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

        Expr::Call { callee, args, .. } => {
            lower_call(ctx, builder, callee, args)
        }

        // Fallback for unsupported expressions in Phase 1
        _ => Operand::Constant(Constant::Unit),
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
            // `a and b`: if a is false, result is false; else evaluate b
            builder.branch(lhs.clone(), rhs_bb, merge_bb);

            // Short-circuit block (lhs was false for `and`)
            // Actually for `and`: if lhs true → evaluate rhs; if lhs false → result = false
            // Current block ended with branch. merge_bb gets false.
            // But we need to assign before merge.
            // Use a "false" block:
            let false_bb = builder.new_block();

            // Rewrite: lhs true → rhs_bb, lhs false → false_bb
            // We already emitted the branch — need to fix it.
            // Since we can't rewrite, restructure:
            // Actually, let's restart properly.

            // Remove the branch we just set (replace current block terminator)
            let current = builder.current_block;
            builder.blocks[current.0 as usize].terminator = None;

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
            // `a or b`: if a is true, result is true; else evaluate b
            let true_bb = builder.new_block();

            let current = builder.current_block;
            builder.blocks[current.0 as usize].terminator = None;

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
) -> Operand {
    if let Expr::Identifier(name) = &callee.node {
        if name == "print" {
            lower_print_call(ctx, builder, args);
            return Operand::Constant(Constant::Unit);
        }

        // Regular function call
        let lowered_args: Vec<Operand> = args
            .iter()
            .map(|arg| lower_expr(ctx, builder, &arg.node.value))
            .collect();

        let ret_type = if let Some((_, ret)) = ctx.fn_sigs.get(name.as_str()) {
            *ret
        } else {
            I64_TYPE // fallback
        };

        if ret_type == UNIT_TYPE {
            builder.call_void(name.clone(), lowered_args);
            Operand::Constant(Constant::Unit)
        } else {
            let dst = builder.call(name.clone(), lowered_args, ret_type);
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
                            if type_id == BOOL_TYPE {
                                // Bool needs ternary: cond ? "true" : "false"
                                // Emit as two separate printf for simplicity? No, better:
                                // Use format specifier %s and emit a ternary expression.
                                // Since GIR is low-level, we handle this in the C backend.
                                // For now, just pass the bool and use %s — the C backend
                                // will need to know to convert. Actually, let's emit a select:
                                format_str.push_str(spec);
                                // We'll need the C backend to handle bool→"true"/"false"
                                // For now, pass an extra operand with the conversion done at C level
                                printf_args.push(FunctionBuilder::copy(local_id));
                            } else {
                                format_str.push_str(spec);
                                printf_args.push(FunctionBuilder::copy(local_id));
                            }
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
                if type_id == BOOL_TYPE {
                    builder.call_extern(
                        "printf",
                        vec![fmt_op, FunctionBuilder::copy(local_id)],
                        I32_TYPE,
                    );
                } else {
                    builder.call_extern(
                        "printf",
                        vec![fmt_op, FunctionBuilder::copy(local_id)],
                        I32_TYPE,
                    );
                }
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

/// Infer the GIR type of an operand by examining its structure.
fn infer_operand_type(ctx: &LoweringContext, operand: &Operand) -> TypeId {
    match operand {
        Operand::Copy(place) | Operand::Move(place) => {
            // Look up the local's type
            // Find by iterating ctx locals
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
        let ctx = LoweringContext::new(analysis, mapper);
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
}
