use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::*;
use crate::ir::types::*;
use crate::parser::ast::{FunctionBody, FunctionDef};

use super::context::LoweringContext;
use super::exprs::lower_expr;
use super::stmts::lower_block;

/// Lower a single function definition into the GIR module.
pub fn lower_function(
    ctx: &mut LoweringContext,
    module: &mut crate::ir::Module,
    func: &FunctionDef,
) {
    let name = &func.name.node;
    let is_main = name == "main";

    // Map return type
    let return_type = if is_main {
        I32_TYPE
    } else {
        ctx.type_mapper.map_ast_type(&func.return_type.node)
    };

    // Map parameters
    let params: Vec<(TypeId, Option<&str>)> = func
        .params
        .iter()
        .map(|p| {
            let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
            let param_name = p.node.name.node.as_str();
            (gir_type, Some(param_name))
        })
        .collect();

    let mut builder = FunctionBuilder::new(name.clone(), return_type, &params);

    // Clear and register locals for this function
    ctx.clear_locals();

    // Register parameters as locals
    for (i, p) in func.params.iter().enumerate() {
        let local_id = LocalId((i + 1) as u32); // _1, _2, ...
        let gir_type = ctx.type_mapper.map_ast_type(&p.node.type_.node);
        ctx.register_local(&p.node.name.node, local_id, gir_type);
    }

    // Lower the body
    match &func.body {
        FunctionBody::Block(block) => {
            lower_block(ctx, &mut builder, block);

            // Add implicit return if the last block has no terminator
            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                if is_main {
                    builder.assign(
                        Place::local(LocalId(0)),
                        FunctionBuilder::const_i32(0),
                    );
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                } else if return_type == UNIT_TYPE {
                    builder.ret(FunctionBuilder::const_unit());
                } else {
                    // Non-void function without explicit return — emit return _0
                    builder.ret(FunctionBuilder::copy(LocalId(0)));
                }
            }
        }

        FunctionBody::Expression(expr) => {
            let operand = lower_expr(ctx, &mut builder, expr);
            builder.assign(Place::local(LocalId(0)), operand);
            builder.ret(FunctionBuilder::copy(LocalId(0)));
        }

        FunctionBody::Declaration | FunctionBody::Extern(_) => {
            // Not handled in lowering — skip
            return;
        }
    }

    module.functions.push(builder.build());
}
