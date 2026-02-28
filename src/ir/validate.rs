use rustc_hash::FxHashSet;

use super::instructions::*;
use super::types::*;
use super::Module;

/// A validation error found in a GIR module.
#[derive(Debug)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub context: String,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.context, self.kind)
    }
}

#[derive(Debug)]
pub enum ValidationErrorKind {
    MissingTerminator { block: BlockId },
    LocalOutOfRange { local: LocalId, max: u32 },
    BlockOutOfRange { block: BlockId, max: u32 },
    EmptyFunction,
    NoReturnPlace,
    UndefinedFunction(String),
    UndefinedType(String),
    DuplicateFunctionName(String),
    DuplicateTypeName(String),
    DuplicateGlobalName(String),
}

impl std::fmt::Display for ValidationErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingTerminator { block } => {
                write!(f, "basic block bb{} has no terminator", block.0)
            }
            Self::LocalOutOfRange { local, max } => {
                write!(f, "local _{} out of range (max: _{})", local.0, max)
            }
            Self::BlockOutOfRange { block, max } => {
                write!(f, "block bb{} out of range (max: bb{})", block.0, max)
            }
            Self::EmptyFunction => write!(f, "function has no basic blocks"),
            Self::NoReturnPlace => write!(f, "function has no locals (missing return place _0)"),
            Self::UndefinedFunction(name) => write!(f, "call to undefined function @{}", name),
            Self::UndefinedType(name) => write!(f, "reference to undefined type '{}'", name),
            Self::DuplicateFunctionName(name) => {
                write!(f, "duplicate function name @{}", name)
            }
            Self::DuplicateTypeName(name) => write!(f, "duplicate type name '{}'", name),
            Self::DuplicateGlobalName(name) => {
                write!(f, "duplicate global name @{}", name)
            }
        }
    }
}

/// Validate a GIR module for structural well-formedness.
pub fn validate(module: &Module) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    check_duplicate_functions(module, &mut errors);
    check_duplicate_type_names(module, &mut errors);
    check_duplicate_globals(module, &mut errors);

    // Collect all known callable names for call target validation.
    let mut callables: FxHashSet<&str> = FxHashSet::default();
    for f in &module.functions {
        callables.insert(&f.name);
    }
    for e in &module.externs {
        callables.insert(&e.name);
    }

    for func in &module.functions {
        let ctx = format!("function @{}", func.name);
        check_function(func, &ctx, &callables, &module.type_registry, &mut errors);
    }

    errors
}

fn check_duplicate_functions(module: &Module, errors: &mut Vec<ValidationError>) {
    let mut seen = FxHashSet::default();
    for f in &module.functions {
        if !seen.insert(&f.name) {
            errors.push(ValidationError {
                kind: ValidationErrorKind::DuplicateFunctionName(f.name.clone()),
                context: "module".into(),
            });
        }
    }
}

fn check_duplicate_type_names(module: &Module, errors: &mut Vec<ValidationError>) {
    let mut seen = FxHashSet::default();
    for def in module.type_registry.type_defs() {
        if !seen.insert(&def.name) {
            errors.push(ValidationError {
                kind: ValidationErrorKind::DuplicateTypeName(def.name.clone()),
                context: "module".into(),
            });
        }
    }
}

fn check_duplicate_globals(module: &Module, errors: &mut Vec<ValidationError>) {
    let mut seen = FxHashSet::default();
    for g in &module.globals {
        if !seen.insert(&g.name) {
            errors.push(ValidationError {
                kind: ValidationErrorKind::DuplicateGlobalName(g.name.clone()),
                context: "module".into(),
            });
        }
    }
}

fn check_function(
    func: &super::Function,
    ctx: &str,
    callables: &FxHashSet<&str>,
    type_registry: &TypeRegistry,
    errors: &mut Vec<ValidationError>,
) {
    let max_local = func.locals.len() as u32;
    let max_block = func.blocks.len() as u32;

    // Must have at least one local (return place).
    if func.locals.is_empty() {
        errors.push(ValidationError {
            kind: ValidationErrorKind::NoReturnPlace,
            context: ctx.into(),
        });
    }

    // Must have at least one block.
    if func.blocks.is_empty() {
        errors.push(ValidationError {
            kind: ValidationErrorKind::EmptyFunction,
            context: ctx.into(),
        });
        return;
    }

    for (i, block) in func.blocks.iter().enumerate() {
        let block_ctx = format!("{}, bb{}", ctx, i);

        // Every block must have a terminator.
        if block.terminator.is_none() {
            errors.push(ValidationError {
                kind: ValidationErrorKind::MissingTerminator {
                    block: BlockId(i as u32),
                },
                context: block_ctx.clone(),
            });
        }

        // Check locals in instructions.
        for inst in &block.instructions {
            check_instruction_locals(inst, max_local, &block_ctx, errors);
            check_instruction_calls(inst, callables, &block_ctx, errors);
            check_instruction_types(inst, type_registry, &block_ctx, errors);
        }

        // Check terminator references.
        if let Some(ref term) = block.terminator {
            check_terminator_blocks(term, max_block, &block_ctx, errors);
            check_terminator_locals(term, max_local, &block_ctx, errors);
            check_terminator_calls(term, callables, &block_ctx, errors);
        }
    }
}

fn check_local_id(id: LocalId, max: u32, ctx: &str, errors: &mut Vec<ValidationError>) {
    if id.0 >= max {
        errors.push(ValidationError {
            kind: ValidationErrorKind::LocalOutOfRange {
                local: id,
                max: max.saturating_sub(1),
            },
            context: ctx.into(),
        });
    }
}

fn check_block_id(id: BlockId, max: u32, ctx: &str, errors: &mut Vec<ValidationError>) {
    if id.0 >= max {
        errors.push(ValidationError {
            kind: ValidationErrorKind::BlockOutOfRange {
                block: id,
                max: max.saturating_sub(1),
            },
            context: ctx.into(),
        });
    }
}

fn check_place_locals(place: &Place, max: u32, ctx: &str, errors: &mut Vec<ValidationError>) {
    check_local_id(place.local, max, ctx, errors);
    for proj in &place.projections {
        if let Projection::Index(local) = proj {
            check_local_id(*local, max, ctx, errors);
        }
    }
}

fn check_operand_locals(op: &Operand, max: u32, ctx: &str, errors: &mut Vec<ValidationError>) {
    match op {
        Operand::Copy(place) | Operand::Move(place) => {
            check_place_locals(place, max, ctx, errors);
        }
        Operand::Constant(_) => {}
    }
}

fn check_instruction_locals(
    inst: &Instruction,
    max: u32,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    match inst {
        Instruction::Assign { dst, value } => {
            check_place_locals(dst, max, ctx, errors);
            check_operand_locals(value, max, ctx, errors);
        }
        Instruction::FieldLoad { dst, base, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_place_locals(base, max, ctx, errors);
        }
        Instruction::IndexLoad { dst, base, index } => {
            check_local_id(*dst, max, ctx, errors);
            check_place_locals(base, max, ctx, errors);
            check_operand_locals(index, max, ctx, errors);
        }
        Instruction::HeapAlloc { dst, allocator, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(allocator, max, ctx, errors);
        }
        Instruction::HeapAllocArray {
            dst,
            count,
            allocator,
            ..
        } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(count, max, ctx, errors);
            check_operand_locals(allocator, max, ctx, errors);
        }
        Instruction::Dealloc { ptr, allocator } => {
            check_operand_locals(ptr, max, ctx, errors);
            check_operand_locals(allocator, max, ctx, errors);
        }
        Instruction::BinOp { dst, lhs, rhs, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(lhs, max, ctx, errors);
            check_operand_locals(rhs, max, ctx, errors);
        }
        Instruction::UnOp { dst, operand, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(operand, max, ctx, errors);
        }
        Instruction::Cmp { dst, lhs, rhs, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(lhs, max, ctx, errors);
            check_operand_locals(rhs, max, ctx, errors);
        }
        Instruction::Cast { dst, value, .. }
        | Instruction::BitCast { dst, value, .. }
        | Instruction::PtrCast { dst, value, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(value, max, ctx, errors);
        }
        Instruction::StructInit { dst, fields, .. } => {
            check_local_id(*dst, max, ctx, errors);
            for f in fields {
                check_operand_locals(f, max, ctx, errors);
            }
        }
        Instruction::EnumInit { dst, fields, .. } => {
            check_local_id(*dst, max, ctx, errors);
            for f in fields {
                check_operand_locals(f, max, ctx, errors);
            }
        }
        Instruction::TupleInit { dst, elements } => {
            check_local_id(*dst, max, ctx, errors);
            for e in elements {
                check_operand_locals(e, max, ctx, errors);
            }
        }
        Instruction::TagOf { dst, operand } => {
            check_local_id(*dst, max, ctx, errors);
            check_operand_locals(operand, max, ctx, errors);
        }
        Instruction::EnumFieldLoad { dst, base, .. } => {
            check_local_id(*dst, max, ctx, errors);
            check_place_locals(base, max, ctx, errors);
        }
        Instruction::Call { dst, args, .. } | Instruction::CallExtern { dst, args, .. } => {
            if let Some(d) = dst {
                check_local_id(*d, max, ctx, errors);
            }
            for a in args {
                check_operand_locals(a, max, ctx, errors);
            }
        }
        Instruction::CallIndirect {
            dst, callee, args, ..
        } => {
            if let Some(d) = dst {
                check_local_id(*d, max, ctx, errors);
            }
            check_operand_locals(callee, max, ctx, errors);
            for a in args {
                check_operand_locals(a, max, ctx, errors);
            }
        }
        Instruction::MoveZero { place }
        | Instruction::Drop { place }
        | Instruction::DropIfAlive { place } => {
            check_place_locals(place, max, ctx, errors);
        }
        Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
            check_local_id(*dst, max, ctx, errors);
            check_place_locals(place, max, ctx, errors);
        }
        Instruction::LoadThreadLocal { dst, .. } => {
            check_local_id(*dst, max, ctx, errors);
        }
        Instruction::PushAllocator { allocator } => {
            check_operand_locals(allocator, max, ctx, errors);
        }
        Instruction::PopAllocator | Instruction::Nop | Instruction::InlineC { .. } => {}
    }
}

fn check_instruction_calls(
    inst: &Instruction,
    callables: &FxHashSet<&str>,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    match inst {
        Instruction::Call { func, .. } | Instruction::CallExtern { func, .. } => {
            if !callables.contains(func.as_str()) && !func.starts_with("__callable_") {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedFunction(func.clone()),
                    context: ctx.into(),
                });
            }
        }
        _ => {}
    }
}

fn check_instruction_types(
    inst: &Instruction,
    type_registry: &TypeRegistry,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    match inst {
        Instruction::StructInit { type_name, .. } | Instruction::EnumInit { type_name, .. } => {
            if !type_registry.has_type_def(type_name) {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedType(type_name.clone()),
                    context: ctx.into(),
                });
            }
        }
        _ => {}
    }
}

fn check_terminator_blocks(
    term: &Terminator,
    max: u32,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    match term {
        Terminator::Jump(target) => check_block_id(*target, max, ctx, errors),
        Terminator::Branch {
            then_block,
            else_block,
            ..
        } => {
            check_block_id(*then_block, max, ctx, errors);
            check_block_id(*else_block, max, ctx, errors);
        }
        Terminator::Switch {
            cases, default, ..
        } => {
            for (_, block) in cases {
                check_block_id(*block, max, ctx, errors);
            }
            check_block_id(*default, max, ctx, errors);
        }
        Terminator::Invoke { normal, error, .. } => {
            check_block_id(*normal, max, ctx, errors);
            check_block_id(*error, max, ctx, errors);
        }
        Terminator::Return(_) | Terminator::Unreachable => {}
    }
}

fn check_terminator_locals(
    term: &Terminator,
    max: u32,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    match term {
        Terminator::Return(value) => check_operand_locals(value, max, ctx, errors),
        Terminator::Branch { cond, .. } => check_operand_locals(cond, max, ctx, errors),
        Terminator::Switch { value, .. } => check_operand_locals(value, max, ctx, errors),
        Terminator::Invoke { args, dst, .. } => {
            if let Some(d) = dst {
                check_local_id(*d, max, ctx, errors);
            }
            for a in args {
                check_operand_locals(a, max, ctx, errors);
            }
        }
        Terminator::Jump(_) | Terminator::Unreachable => {}
    }
}

fn check_terminator_calls(
    term: &Terminator,
    callables: &FxHashSet<&str>,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    if let Terminator::Invoke { func, .. } = term {
        if !callables.contains(func.as_str()) {
            errors.push(ValidationError {
                kind: ValidationErrorKind::UndefinedFunction(func.clone()),
                context: ctx.into(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::{BasicBlock, ExternDecl, Function, Local};

    #[test]
    fn valid_module_passes() {
        let mut module = Module::new();

        module.externs.push(ExternDecl {
            name: "puts".into(),
            params: vec![],
            return_type: I32_TYPE,
            is_variadic: false,
        });

        let mut b = FunctionBuilder::new("main", I32_TYPE, &[]);
        b.call_extern("puts", vec![FunctionBuilder::const_str("hello")], I32_TYPE);
        b.ret(FunctionBuilder::const_i32(0));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
    }

    #[test]
    fn missing_terminator() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "f".into(),
            params: vec![],
            return_type: UNIT_TYPE,
            locals: vec![Local {
                type_id: UNIT_TYPE,
                name_hint: None,
            }],
            blocks: vec![BasicBlock::new()], // no terminator
        });

        let errors = validate(&module);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            errors[0].kind,
            ValidationErrorKind::MissingTerminator { block: BlockId(0) }
        ));
    }

    #[test]
    fn local_out_of_range() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("f", I32_TYPE, &[]);
        // Manually emit an instruction referencing a non-existent local.
        b.assign(
            Place::local(LocalId(99)),
            FunctionBuilder::const_i32(0),
        );
        b.ret(FunctionBuilder::const_i32(0));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            e.kind,
            ValidationErrorKind::LocalOutOfRange {
                local: LocalId(99),
                ..
            }
        )));
    }

    #[test]
    fn block_out_of_range() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        // Jump to non-existent block.
        b.jump(BlockId(99));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            e.kind,
            ValidationErrorKind::BlockOutOfRange {
                block: BlockId(99),
                ..
            }
        )));
    }

    #[test]
    fn duplicate_function_name() {
        let mut module = Module::new();

        let mut b1 = FunctionBuilder::new("dup", I32_TYPE, &[]);
        b1.ret(FunctionBuilder::const_i32(0));
        module.functions.push(b1.build());

        let mut b2 = FunctionBuilder::new("dup", I32_TYPE, &[]);
        b2.ret(FunctionBuilder::const_i32(1));
        module.functions.push(b2.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::DuplicateFunctionName(name) if name == "dup"
        )));
    }
}
