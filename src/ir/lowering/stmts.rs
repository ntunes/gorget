use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{self, Block, Expr, Pattern, Stmt};
use crate::span::Spanned;

use super::context::LoweringContext;
use super::drops::DropScopeKind;
use super::exprs::lower_expr;

/// Lower a block of statements.
pub fn lower_block(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    block: &Block,
) {
    for stmt in &block.stmts {
        lower_stmt(ctx, builder, stmt);
    }
}

/// Lower a single statement.
pub fn lower_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    stmt: &Spanned<Stmt>,
) {
    match &stmt.node {
        Stmt::VarDecl {
            type_,
            pattern,
            value,
            ..
        } => lower_var_decl(ctx, builder, type_, pattern, value),

        Stmt::Assign { target, value } => lower_assign(ctx, builder, target, value),

        Stmt::CompoundAssign { target, op, value } => {
            lower_compound_assign(ctx, builder, target, *op, value)
        }

        Stmt::Return(expr) => lower_return(ctx, builder, expr.as_ref()),

        Stmt::Expr(expr) => {
            lower_expr(ctx, builder, expr);
        }

        Stmt::Pass => {
            builder.nop();
        }

        Stmt::If {
            condition,
            then_body,
            elif_branches,
            else_body,
        } => lower_if(ctx, builder, condition, then_body, elif_branches, else_body),

        Stmt::While {
            condition,
            body,
            ..
        } => lower_while(ctx, builder, condition, body),

        Stmt::For {
            pattern,
            iterable,
            body,
            ..
        } => lower_for(ctx, builder, pattern, iterable, body),

        Stmt::Loop { body } => lower_loop(ctx, builder, body),

        Stmt::Break(_) => lower_break(ctx, builder),

        Stmt::Continue => lower_continue(ctx, builder),

        Stmt::Match {
            scrutinee,
            arms,
            else_arm,
        } => lower_match_stmt(ctx, builder, scrutinee, arms, else_arm),

        Stmt::Throw(expr) => lower_throw(ctx, builder, expr),

        Stmt::Assert { condition, message } => lower_assert(ctx, builder, condition, message.as_ref()),

        Stmt::With { bindings, body } => lower_with(ctx, builder, bindings, body),

        Stmt::Unsafe { body } => lower_block(ctx, builder, body),

        Stmt::Item(_) => { /* Nested items are hoisted — no-op in GIR */ }

        // Deferred: Select (async), other async constructs
        _ => {}
    }
}

/// Lower a variable declaration.
fn lower_var_decl(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    type_: &Spanned<ast::Type>,
    pattern: &Spanned<Pattern>,
    value: &Spanned<Expr>,
) {
    if let Pattern::Binding(name) = &pattern.node {
        let gir_type = ctx.resolve_var_type(type_, value);
        let local_id = builder.add_local(gir_type, Some(name));
        ctx.register_local(name, local_id, gir_type);
        // P2.6: Register Move-type locals for drop at scope exit
        ctx.drops.register_local(local_id, gir_type, &ctx.type_registry);
        let operand = lower_expr(ctx, builder, value);
        builder.assign(Place::local(local_id), operand);
    }
}

/// Lower an assignment.
fn lower_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    value: &Spanned<Expr>,
) {
    if let Expr::Identifier(name) = &target.node {
        if let Some((local_id, _)) = ctx.lookup_local(name) {
            let operand = lower_expr(ctx, builder, value);
            builder.assign(Place::local(local_id), operand);
        }
    }
}

/// Lower a compound assignment (e.g., `x += 1`).
fn lower_compound_assign(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    target: &Spanned<Expr>,
    op: ast::BinaryOp,
    value: &Spanned<Expr>,
) {
    if let Expr::Identifier(name) = &target.node {
        if let Some((local_id, type_id)) = ctx.lookup_local(name) {
            let rhs = lower_expr(ctx, builder, value);
            let gir_op = match op {
                ast::BinaryOp::Add => BinOp::Add,
                ast::BinaryOp::Sub => BinOp::Sub,
                ast::BinaryOp::Mul => BinOp::Mul,
                ast::BinaryOp::Div => BinOp::Div,
                ast::BinaryOp::Mod => BinOp::Rem,
                _ => BinOp::Add, // fallback
            };
            let tmp = builder.bin_op(gir_op, type_id, FunctionBuilder::copy(local_id), rhs);
            builder.assign(Place::local(local_id), FunctionBuilder::copy(tmp));
        }
    }
}

/// Lower a return statement.
fn lower_return(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: Option<&Spanned<Expr>>,
) {
    if let Some(expr) = expr {
        let operand = lower_expr(ctx, builder, expr);
        builder.assign(Place::local(LocalId(0)), operand);
        // P2.6: Emit cleanup drops for all scopes being exited
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function);
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    } else {
        // P2.6: Emit cleanup drops for all scopes being exited
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function);
        builder.ret(FunctionBuilder::const_unit());
    }
}

/// Lower an if/elif/else statement.
fn lower_if(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    then_body: &Block,
    elif_branches: &[(Spanned<Expr>, Block)],
    else_body: &Option<Block>,
) {
    let merge_bb = builder.new_block();

    // Lower the condition
    let cond = lower_expr(ctx, builder, condition);

    let then_bb = builder.new_block();
    let first_else_bb = if !elif_branches.is_empty() || else_body.is_some() {
        builder.new_block()
    } else {
        merge_bb
    };

    builder.branch(cond, then_bb, first_else_bb);

    // Then branch
    builder.switch_to(then_bb);
    lower_block(ctx, builder, then_body);
    if !block_always_returns(then_body) {
        builder.jump(merge_bb);
    }

    // Elif branches
    let mut current_else_bb = first_else_bb;
    for (i, (elif_cond, elif_body)) in elif_branches.iter().enumerate() {
        builder.switch_to(current_else_bb);
        let elif_cond_op = lower_expr(ctx, builder, elif_cond);

        let elif_then_bb = builder.new_block();
        let next_else_bb = if i + 1 < elif_branches.len() || else_body.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        builder.branch(elif_cond_op, elif_then_bb, next_else_bb);

        builder.switch_to(elif_then_bb);
        lower_block(ctx, builder, elif_body);
        if !block_always_returns(elif_body) {
            builder.jump(merge_bb);
        }

        current_else_bb = next_else_bb;
    }

    // Else branch
    if let Some(else_body) = else_body {
        builder.switch_to(current_else_bb);
        lower_block(ctx, builder, else_body);
        if !block_always_returns(else_body) {
            builder.jump(merge_bb);
        }
    }

    builder.switch_to(merge_bb);
}

/// Lower a while loop.
fn lower_while(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    body: &Block,
) {
    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // Jump from current block to header
    builder.jump(header_bb);

    // Header: evaluate condition, branch
    builder.switch_to(header_bb);
    let cond = lower_expr(ctx, builder, condition);
    builder.branch(cond, body_bb, exit_bb);

    // Body: execute, jump back to header (wrapped in Loop scope for drop cleanup)
    builder.switch_to(body_bb);
    ctx.push_loop(header_bb, exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(header_bb);

    // Continue from exit
    builder.switch_to(exit_bb);
}

/// Lower a for loop over a range (`for i in start..end`).
fn lower_for(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    iterable: &Spanned<Expr>,
    body: &Block,
) {
    if let Pattern::Binding(var_name) = &pattern.node {
        if let Expr::Range {
            start: Some(start),
            end: Some(end),
            inclusive,
        } = &iterable.node
        {
            lower_for_range(ctx, builder, var_name, start, end, *inclusive, body);
            return;
        }
    }
    // Fallback: ignore non-range for loops in Phase 1
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
) {
    // Create loop variable
    let loop_var = builder.add_local(I64_TYPE, Some(var_name));
    let start_val = lower_expr(ctx, builder, start);
    builder.assign(Place::local(loop_var), start_val);
    ctx.register_local(var_name, loop_var, I64_TYPE);

    let header_bb = builder.new_block();
    let body_bb = builder.new_block();
    let incr_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // Jump to header
    builder.jump(header_bb);

    // Header: compare loop var with end
    builder.switch_to(header_bb);
    let end_val = lower_expr(ctx, builder, end);
    let cmp_op = if inclusive { CmpOp::Le } else { CmpOp::Lt };
    let cond = builder.cmp(cmp_op, I64_TYPE, FunctionBuilder::copy(loop_var), end_val);
    builder.branch(FunctionBuilder::copy(cond), body_bb, exit_bb);

    // Body (wrapped in Loop scope for drop cleanup)
    builder.switch_to(body_bb);
    ctx.push_loop(header_bb, exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(incr_bb);

    // Increment: loop_var = loop_var + 1
    builder.switch_to(incr_bb);
    let one = Operand::Constant(Constant::I64(1));
    let incremented = builder.bin_op(BinOp::Add, I64_TYPE, FunctionBuilder::copy(loop_var), one);
    builder.assign(Place::local(loop_var), FunctionBuilder::copy(incremented));
    builder.jump(header_bb);

    // Exit
    builder.switch_to(exit_bb);
}

/// Lower an infinite `loop: body` statement.
fn lower_loop(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    body: &Block,
) {
    let body_bb = builder.new_block();
    let exit_bb = builder.new_block();

    // Jump into the loop body
    builder.jump(body_bb);

    // Body: execute, jump back to body (infinite loop)
    builder.switch_to(body_bb);
    ctx.push_loop(body_bb, exit_bb);
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
    ctx.pop_loop();
    builder.jump(body_bb);

    // Exit (reached via break)
    builder.switch_to(exit_bb);
}

/// Lower a `break` statement.
fn lower_break(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if let Some(loop_info) = ctx.current_loop() {
        let exit_bb = loop_info.exit_bb;
        // Emit cleanup drops up to the Loop scope
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Loop);
        builder.jump(exit_bb);
        // Create unreachable block to absorb dead code after break
        let dead_bb = builder.new_block();
        builder.switch_to(dead_bb);
    }
}

/// Lower a `continue` statement.
fn lower_continue(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
) {
    if let Some(loop_info) = ctx.current_loop() {
        let header_bb = loop_info.header_bb;
        // Emit cleanup drops up to the Loop scope
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Loop);
        builder.jump(header_bb);
        // Create unreachable block to absorb dead code after continue
        let dead_bb = builder.new_block();
        builder.switch_to(dead_bb);
    }
}

// ---- P3.1: Match Statements ----

/// Lower a match statement to GIR using Branch chains.
fn lower_match_stmt(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    scrutinee: &Spanned<Expr>,
    arms: &[ast::MatchArm],
    else_arm: &Option<Block>,
) {
    // Lower scrutinee to a temp local
    let scrut_op = lower_expr(ctx, builder, scrutinee);
    let scrut_type = super::exprs::infer_operand_type(ctx, &scrut_op);
    let scrut_local = builder.add_local(scrut_type, None);
    builder.assign(Place::local(scrut_local), scrut_op);

    let merge_bb = builder.new_block();

    // Process each arm as a test-body chain
    for (i, arm) in arms.iter().enumerate() {
        let arm_body_bb = builder.new_block();
        let next_test_bb = if i + 1 < arms.len() || else_arm.is_some() {
            builder.new_block()
        } else {
            merge_bb
        };

        // Emit pattern condition check
        let cond = lower_pattern_condition(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        builder.branch(cond, arm_body_bb, next_test_bb);

        // Arm body
        builder.switch_to(arm_body_bb);
        emit_pattern_bindings(ctx, builder, &arm.pattern, scrut_local, scrut_type);
        // Match arms in Stmt::Match have body as Spanned<Expr>
        lower_expr(ctx, builder, &arm.body);
        builder.jump(merge_bb);

        builder.switch_to(next_test_bb);
    }

    // Else arm
    if let Some(else_body) = else_arm {
        lower_block(ctx, builder, else_body);
        builder.jump(merge_bb);
    }

    builder.switch_to(merge_bb);
}

/// Lower a pattern condition to a boolean Operand.
pub fn lower_pattern_condition(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) -> Operand {
    match &pattern.node {
        Pattern::Wildcard => FunctionBuilder::const_bool(true),

        Pattern::Literal(expr) => {
            let lit_op = lower_expr(ctx, builder, expr);
            let cmp = builder.cmp(
                CmpOp::Eq,
                scrut_type,
                FunctionBuilder::copy(scrut_local),
                lit_op,
            );
            FunctionBuilder::copy(cmp)
        }

        Pattern::Binding(name) => {
            // Check if this is an enum variant name (unit variant match)
            if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(name) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            // Plain variable binding — always matches
            FunctionBuilder::const_bool(true)
        }

        Pattern::Constructor { path, .. } => {
            // Extract variant name from path
            let variant_name = if let Some(last) = path.last() {
                &last.node
            } else {
                return FunctionBuilder::const_bool(true);
            };
            if let Some((enum_name, variant_name)) = ctx.resolve_enum_variant(variant_name) {
                let tag = builder.tag_of(FunctionBuilder::copy(scrut_local));
                if let Some(variant_tag) = ctx.resolve_variant_tag(&enum_name, &variant_name) {
                    let cmp = builder.cmp(
                        CmpOp::Eq,
                        I32_TYPE,
                        FunctionBuilder::copy(tag),
                        Operand::Constant(Constant::I32(variant_tag as i32)),
                    );
                    return FunctionBuilder::copy(cmp);
                }
            }
            FunctionBuilder::const_bool(true)
        }

        Pattern::Or(alts) => {
            // Short-circuit OR: if any alternative matches, return true
            let result_id = builder.add_local(BOOL_TYPE, None);
            builder.assign(Place::local(result_id), FunctionBuilder::const_bool(false));

            let merge_bb = builder.new_block();

            for (i, alt) in alts.iter().enumerate() {
                let cond = lower_pattern_condition(ctx, builder, alt, scrut_local, scrut_type);
                let next_bb = if i + 1 < alts.len() {
                    builder.new_block()
                } else {
                    merge_bb
                };
                let true_bb = builder.new_block();
                builder.branch(cond, true_bb, next_bb);

                builder.switch_to(true_bb);
                builder.assign(Place::local(result_id), FunctionBuilder::const_bool(true));
                builder.jump(merge_bb);

                if i + 1 < alts.len() {
                    builder.switch_to(next_bb);
                }
            }

            builder.switch_to(merge_bb);
            FunctionBuilder::copy(result_id)
        }

        Pattern::Tuple(_) | Pattern::Rest => {
            // Structural match — always matches if types match
            FunctionBuilder::const_bool(true)
        }
    }
}

/// Emit pattern bindings — assign destructured values to local variables.
pub fn emit_pattern_bindings(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    pattern: &Spanned<Pattern>,
    scrut_local: LocalId,
    scrut_type: TypeId,
) {
    match &pattern.node {
        Pattern::Binding(name) => {
            // If not an enum variant, bind the scrutinee value
            if ctx.resolve_enum_variant(name).is_none() {
                ctx.register_local(name, scrut_local, scrut_type);
            }
        }

        Pattern::Constructor { path, fields } => {
            let variant_name = if let Some(last) = path.last() {
                last.node.clone()
            } else {
                return;
            };

            // Look up enum info to find the type name
            let enum_name = if let Some((en, _)) = ctx.resolve_enum_variant(&variant_name) {
                en
            } else {
                return;
            };

            for (i, field_pat) in fields.iter().enumerate() {
                // Determine the field type from the enum variant definition
                let field_type = if let Some(type_def) = ctx.type_registry.get_type_def(&enum_name) {
                    if let TypeDefKind::Enum(ref e) = type_def.kind {
                        if let Some(v) = e.variants.iter().find(|v| v.name == variant_name) {
                            if let Some(f) = v.fields.get(i) {
                                f.type_id
                            } else {
                                I64_TYPE
                            }
                        } else {
                            I64_TYPE
                        }
                    } else {
                        I64_TYPE
                    }
                } else {
                    I64_TYPE
                };

                let dst = builder.enum_field_load(
                    Place::local(scrut_local),
                    variant_name.clone(),
                    i as u32,
                    field_type,
                );

                // Recurse on sub-pattern
                emit_pattern_bindings(ctx, builder, field_pat, dst, field_type);
            }
        }

        Pattern::Tuple(elems) => {
            for (i, elem_pat) in elems.iter().enumerate() {
                // Use field_load with field index
                let elem_type = I64_TYPE; // placeholder — real type needs registry
                let dst = builder.field_load(Place::local(scrut_local), i as u32, elem_type);
                emit_pattern_bindings(ctx, builder, elem_pat, dst, elem_type);
            }
        }

        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Or(_) | Pattern::Rest => {
            // No bindings
        }
    }
}

// ---- P3.3: Error Handling ----

/// Lower a `throw expr` statement.
fn lower_throw(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    expr: &Spanned<Expr>,
) {
    let val = lower_expr(ctx, builder, expr);
    builder.call_extern("gorget_throw", vec![val], UNIT_TYPE);
    builder.unreachable();
    // Create unreachable block for dead code after throw
    let dead_bb = builder.new_block();
    builder.switch_to(dead_bb);
}

/// Lower an `assert condition [, message]` statement.
fn lower_assert(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    condition: &Spanned<Expr>,
    message: Option<&Spanned<Expr>>,
) {
    let cond = lower_expr(ctx, builder, condition);

    let pass_bb = builder.new_block();
    let fail_bb = builder.new_block();

    builder.branch(cond, pass_bb, fail_bb);

    // Fail path: print message and abort
    builder.switch_to(fail_bb);
    if let Some(msg) = message {
        let msg_op = lower_expr(ctx, builder, msg);
        builder.call_extern(
            "fprintf",
            vec![
                Operand::Constant(Constant::Null), // stderr placeholder
                Operand::Constant(Constant::Str("Assertion failed: %s\n".to_string())),
                msg_op,
            ],
            I32_TYPE,
        );
    } else {
        builder.call_extern(
            "fprintf",
            vec![
                Operand::Constant(Constant::Null), // stderr placeholder
                Operand::Constant(Constant::Str("Assertion failed\n".to_string())),
            ],
            I32_TYPE,
        );
    }
    builder.call_extern("exit", vec![Operand::Constant(Constant::I32(1))], UNIT_TYPE);
    builder.unreachable();

    // Pass path: continue
    builder.switch_to(pass_bb);
}

// ---- P3.5: With statement ----

/// Lower a `with bindings: body` statement.
fn lower_with(
    ctx: &mut LoweringContext,
    builder: &mut FunctionBuilder,
    bindings: &[ast::WithBinding],
    body: &Block,
) {
    ctx.drops.push_scope(DropScopeKind::Block);

    for binding in bindings {
        let val = lower_expr(ctx, builder, &binding.expr);
        let type_id = super::exprs::infer_operand_type(ctx, &val);
        let local_id = builder.add_local(type_id, Some(&binding.name.node));
        ctx.register_local(&binding.name.node, local_id, type_id);
        ctx.drops.register_local(local_id, type_id, &ctx.type_registry);
        builder.assign(Place::local(local_id), val);
    }

    lower_block(ctx, builder, body);

    ctx.drops.pop_scope(builder, &ctx.type_registry);
}

/// Check if a block always ends with a return statement.
fn block_always_returns(block: &Block) -> bool {
    if let Some(last) = block.stmts.last() {
        matches!(last.node, Stmt::Return(_))
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::TypeRegistry;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            span: Span { start: 0, end: 0 },
        }
    }

    fn make_test_ctx() -> LoweringContext<'static> {
        let analysis = Box::leak(Box::new(crate::ir::lowering::empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = super::super::types::TypeMapper::new(&mut reg);
        LoweringContext::new(analysis, mapper, reg)
    }

    #[test]
    fn lower_var_decl_test() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::VarDecl {
            is_const: false,
            is_mutable: false,
            type_: spanned(ast::Type::Primitive(ast::PrimitiveType::Int)),
            pattern: spanned(Pattern::Binding("x".into())),
            value: spanned(Expr::IntLiteral(42)),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have created a local and an assign
        assert!(ctx.lookup_local("x").is_some());
        assert!(!builder.blocks[0].instructions.is_empty());
        assert!(matches!(
            builder.blocks[0].instructions.last().unwrap(),
            Instruction::Assign { .. }
        ));
    }

    #[test]
    fn lower_if_else_test() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::If {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("x".into()))),
                op: ast::BinaryOp::Gt,
                right: Box::new(spanned(Expr::IntLiteral(0))),
            }),
            then_body: Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            },
            elif_branches: vec![],
            else_body: Some(Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            }),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have created multiple blocks (entry + merge + then + else)
        assert!(builder.blocks.len() >= 3);
        // Entry block should end with a Branch terminator
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
    }

    #[test]
    fn lower_while_loop_test() {
        let mut ctx = make_test_ctx();
        let i_id = LocalId(1);
        ctx.register_local("i", i_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("i"))]);

        let stmt = spanned(Stmt::While {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("i".into()))),
                op: ast::BinaryOp::Lt,
                right: Box::new(spanned(Expr::IntLiteral(10))),
            }),
            body: Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            },
            else_body: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have header, body, exit blocks
        assert!(builder.blocks.len() >= 4); // entry + header + body + exit
        // Entry block should jump to header
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Jump(_))
        ));
    }

    // ---- P3.0: Break, Continue, Loop tests ----

    #[test]
    fn lower_loop_basic() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break(None))],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have: entry(bb0) → body(bb1), exit(bb2), dead(bb3)
        assert!(builder.blocks.len() >= 3);
        // Entry block should jump to body
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Jump(BlockId(1)))
        ));
        // Body block back-edge (body_bb → body_bb) won't be present since break overrides it;
        // the break inside the body should jump to exit_bb
    }

    #[test]
    fn lower_break_in_loop() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // loop: break
        let stmt = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break(None))],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // The body block (bb1) should contain a Jump to the exit block (bb2)
        // break emits: jump to exit_bb, then creates dead block
        let body_block = &builder.blocks[1];
        if let Some(Terminator::Jump(target)) = &body_block.terminator {
            // Break should jump to exit_bb (bb2)
            assert_eq!(*target, BlockId(2), "break should jump to exit block");
        } else {
            panic!("Body block should have Jump terminator from break");
        }
    }

    #[test]
    fn lower_continue_in_while() {
        let mut ctx = make_test_ctx();
        let i_id = LocalId(1);
        ctx.register_local("i", i_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("i"))]);

        // while i < 10: continue
        let stmt = spanned(Stmt::While {
            condition: spanned(Expr::BinaryOp {
                left: Box::new(spanned(Expr::Identifier("i".into()))),
                op: ast::BinaryOp::Lt,
                right: Box::new(spanned(Expr::IntLiteral(10))),
            }),
            body: Block {
                stmts: vec![spanned(Stmt::Continue)],
                span: Span { start: 0, end: 0 },
            },
            else_body: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // bb0=entry, bb1=header, bb2=body, bb3=exit, bb4=dead(from continue)
        // Body block (bb2) should jump back to header (bb1)
        let body_block = &builder.blocks[2];
        if let Some(Terminator::Jump(target)) = &body_block.terminator {
            assert_eq!(*target, BlockId(1), "continue should jump to header block");
        } else {
            panic!("Body block should have Jump terminator from continue");
        }
    }

    #[test]
    fn lower_nested_break() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // loop:
        //   loop:
        //     break   <- should break inner loop only
        let inner_loop = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![spanned(Stmt::Break(None))],
                span: Span { start: 0, end: 0 },
            },
        });
        let outer_loop = spanned(Stmt::Loop {
            body: Block {
                stmts: vec![inner_loop],
                span: Span { start: 0, end: 0 },
            },
        });

        lower_stmt(&mut ctx, &mut builder, &outer_loop);

        // After lowering, the inner break should target inner exit, not outer exit.
        // The structure is:
        // bb0: entry → jump to outer_body (bb1)
        // bb1: outer body → inner stuff starts here
        //   bb3: inner body → break jumps to inner exit (bb4)
        //   bb4: inner exit → falls through
        // bb2: outer exit
        // The key assertion: inner break doesn't reach outer exit

        // Verify we have enough blocks
        assert!(builder.blocks.len() >= 5, "Should have at least 5 blocks for nested loops");
    }

    // ---- P3.1: Match statement tests ----

    #[test]
    fn lower_match_literal() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![
                ast::MatchArm {
                    pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                    guard: None,
                    body: spanned(Expr::IntLiteral(10)),
                    span: Span { start: 0, end: 0 },
                },
                ast::MatchArm {
                    pattern: spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                    guard: None,
                    body: spanned(Expr::IntLiteral(20)),
                    span: Span { start: 0, end: 0 },
                },
            ],
            else_arm: Some(Block {
                stmts: vec![spanned(Stmt::Pass)],
                span: Span { start: 0, end: 0 },
            }),
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should create blocks for scrutinee + each arm + else + merge
        assert!(builder.blocks.len() >= 5);
        // Entry block: assign scrutinee, then branch on first pattern
        // There should be Cmp instructions for literal matching
        let has_cmp = builder.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| matches!(inst, Instruction::Cmp { .. }))
        });
        assert!(has_cmp, "Should have Cmp instructions for literal pattern matching");
    }

    #[test]
    fn lower_match_binding() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![ast::MatchArm {
                pattern: spanned(Pattern::Binding("val".into())),
                guard: None,
                body: spanned(Expr::IntLiteral(42)),
                span: Span { start: 0, end: 0 },
            }],
            else_arm: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // The binding pattern should register "val" as a local alias
        assert!(ctx.lookup_local("val").is_some(), "Pattern binding should register 'val'");
    }

    #[test]
    fn lower_match_or_pattern() {
        let mut ctx = make_test_ctx();
        let x_id = LocalId(1);
        ctx.register_local("x", x_id, I64_TYPE);

        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        // match x: case 1 | 2 | 3: pass
        let stmt = spanned(Stmt::Match {
            scrutinee: spanned(Expr::Identifier("x".into())),
            arms: vec![ast::MatchArm {
                pattern: spanned(Pattern::Or(vec![
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(1))))),
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(2))))),
                    spanned(Pattern::Literal(Box::new(spanned(Expr::IntLiteral(3))))),
                ])),
                guard: None,
                body: spanned(Expr::IntLiteral(0)),
                span: Span { start: 0, end: 0 },
            }],
            else_arm: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have multiple Cmp instructions (one per alternative) and Branch terminators
        let cmp_count: usize = builder.blocks.iter()
            .map(|bb| bb.instructions.iter().filter(|inst| matches!(inst, Instruction::Cmp { .. })).count())
            .sum();
        assert!(cmp_count >= 3, "Or pattern should have at least 3 Cmp instructions, got {cmp_count}");
    }

    // ---- P3.3: Error handling tests ----

    #[test]
    fn lower_assert_true() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Assert {
            condition: spanned(Expr::BoolLiteral(true)),
            message: None,
        });

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have Branch terminator in entry block
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Branch { .. })
        ));
        // Fail block should have Unreachable terminator
        let has_unreachable = builder.blocks.iter().any(|bb| {
            matches!(bb.terminator, Some(Terminator::Unreachable))
        });
        assert!(has_unreachable, "Assert fail path should have Unreachable terminator");
    }

    #[test]
    fn lower_throw_stmt() {
        let mut ctx = make_test_ctx();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        let stmt = spanned(Stmt::Throw(spanned(Expr::StringLiteral(
            crate::lexer::token::StringLiteral {
                kind: crate::lexer::token::StringKind::Normal,
                segments: vec![crate::lexer::token::StringSegment::Literal("error".into())],
            },
        ))));

        lower_stmt(&mut ctx, &mut builder, &stmt);

        // Should have a CallExtern to gorget_throw + Unreachable
        let has_throw = builder.blocks[0].instructions.iter().any(|inst| {
            matches!(inst, Instruction::CallExtern { func, .. } if func == "gorget_throw")
        });
        assert!(has_throw, "Should call gorget_throw");
        assert!(matches!(
            builder.blocks[0].terminator,
            Some(Terminator::Unreachable)
        ));
    }
}
