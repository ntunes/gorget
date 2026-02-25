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

        // Phase 1: ignore other statement types
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
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
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
    ctx.drops.push_scope(DropScopeKind::Loop);
    lower_block(ctx, builder, body);
    ctx.drops.pop_scope(builder, &ctx.type_registry);
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
}
