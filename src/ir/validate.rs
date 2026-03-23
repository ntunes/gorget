use rustc_hash::FxHashSet;

use super::instructions::*;
use super::types::*;
use super::{Function, Module};

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
    /// StructInit field count doesn't match the TypeDef.
    StructFieldCountMismatch { type_name: String, expected: usize, got: usize },
    /// EnumInit field count doesn't match the variant's definition.
    EnumFieldCountMismatch { type_name: String, variant: String, expected: usize, got: usize },
    /// EnumInit references a variant that doesn't exist in the TypeDef.
    EnumVariantNotFound { type_name: String, variant: String },
    /// Drop/DropIfAlive on a local whose type doesn't need dropping.
    DropOnNonDroppable { local: LocalId, type_id: TypeId },
    /// Local references a TypeId beyond the registry's range.
    InvalidTypeId { local: LocalId, type_id: TypeId },
    /// Type metadata inconsistency: Copy semantics with a non-None drop strategy
    /// (except for ref-counted types which are intentionally Copy+Drop).
    InconsistentDropMetadata { type_name: String, copy_semantics: CopySemantics, drop_strategy: String },
    /// Return place _0 has a type that doesn't match the function's declared return_type.
    ReturnTypeMismatch { return_type: TypeId, local_0_type: TypeId },
    /// A local was read after being MoveZero'd within the same basic block.
    UseAfterMove { local: LocalId, block: BlockId },
    /// span_map length doesn't match instruction count.
    SpanMapMismatch { block: BlockId, insts: usize, spans: usize },
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
            Self::StructFieldCountMismatch { type_name, expected, got } => {
                write!(f, "StructInit '{}' has {} fields, TypeDef has {}", type_name, got, expected)
            }
            Self::EnumFieldCountMismatch { type_name, variant, expected, got } => {
                write!(f, "EnumInit '{}::{}' has {} fields, variant has {}", type_name, variant, got, expected)
            }
            Self::EnumVariantNotFound { type_name, variant } => {
                write!(f, "EnumInit '{}::{}' variant not found in TypeDef", type_name, variant)
            }
            Self::DropOnNonDroppable { local, type_id } => {
                write!(f, "Drop on _{} (type {}) which doesn't need dropping", local.0, type_id)
            }
            Self::InvalidTypeId { local, type_id } => {
                write!(f, "local _{} has invalid type {}", local.0, type_id)
            }
            Self::InconsistentDropMetadata { type_name, copy_semantics, drop_strategy } => {
                write!(f, "type '{}' has {:?} semantics but {} drop strategy", type_name, copy_semantics, drop_strategy)
            }
            Self::ReturnTypeMismatch { return_type, local_0_type } => {
                write!(f, "return place _0 has type {} but function declares return type {}", local_0_type, return_type)
            }
            Self::UseAfterMove { local, block } => {
                write!(f, "local _{} read after MoveZero in bb{}", local.0, block.0)
            }
            Self::SpanMapMismatch { block, insts, spans } => {
                write!(f, "bb{}: span_map has {} entries but {} instructions", block.0, spans, insts)
            }
        }
    }
}

/// Validate a GIR module for structural well-formedness and semantic consistency.
pub fn validate(module: &Module) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    check_duplicate_functions(module, &mut errors);
    check_duplicate_type_names(module, &mut errors);
    check_duplicate_globals(module, &mut errors);
    check_drop_metadata_consistency(module, &mut errors);

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

    // Semantic checks on function-level properties
    check_return_type_consistency(func, ctx, errors);
    check_local_type_ids(func, ctx, type_registry, errors);
    check_drop_targets(func, ctx, type_registry, errors);
    check_use_after_move(func, ctx, errors);

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

        // Span map must match instruction count.
        if block.span_map.len() != block.instructions.len() {
            errors.push(ValidationError {
                kind: ValidationErrorKind::SpanMapMismatch {
                    block: BlockId(i as u32),
                    insts: block.instructions.len(),
                    spans: block.span_map.len(),
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
        Instruction::Assign { dst, value, .. } => {
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
        Instruction::GlobalAssign { value, .. } => {
            check_operand_locals(value, max, ctx, errors);
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
            if !callables.contains(func.as_str()) && !func.starts_with("__callable_") && !func.starts_with("__gorget_closure_call_") {
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
        Instruction::StructInit { type_name, fields, .. } => {
            if let Some(type_def) = type_registry.get_type_def(type_name) {
                if let TypeDefKind::Struct(ref sdef) = type_def.kind {
                    if sdef.fields.len() != fields.len() {
                        errors.push(ValidationError {
                            kind: ValidationErrorKind::StructFieldCountMismatch {
                                type_name: type_name.clone(),
                                expected: sdef.fields.len(),
                                got: fields.len(),
                            },
                            context: ctx.into(),
                        });
                    }
                }
            } else {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedType(type_name.clone()),
                    context: ctx.into(),
                });
            }
        }
        Instruction::EnumInit { type_name, variant, fields, .. } => {
            if let Some(type_def) = type_registry.get_type_def(type_name) {
                if let TypeDefKind::Enum(ref edef) = type_def.kind {
                    if let Some(vdef) = edef.variants.iter().find(|v| v.name == *variant) {
                        if vdef.fields.len() != fields.len() {
                            errors.push(ValidationError {
                                kind: ValidationErrorKind::EnumFieldCountMismatch {
                                    type_name: type_name.clone(),
                                    variant: variant.clone(),
                                    expected: vdef.fields.len(),
                                    got: fields.len(),
                                },
                                context: ctx.into(),
                            });
                        }
                    } else {
                        errors.push(ValidationError {
                            kind: ValidationErrorKind::EnumVariantNotFound {
                                type_name: type_name.clone(),
                                variant: variant.clone(),
                            },
                            context: ctx.into(),
                        });
                    }
                }
            } else {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedType(type_name.clone()),
                    context: ctx.into(),
                });
            }
        }
        _ => {}
    }
}

/// Check that the return place `_0` has a type matching `func.return_type`.
///
/// The return place (local 0) is where the function's return value is stored.
/// Its type should always agree with the declared return type. A mismatch indicates
/// a bug in the lowering — e.g., the return type was fixed up but `_0` was not.
fn check_return_type_consistency(
    func: &Function,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    if func.locals.is_empty() {
        return; // Already caught by NoReturnPlace check
    }
    let local_0_type = func.locals[0].type_id;
    if local_0_type != func.return_type {
        errors.push(ValidationError {
            kind: ValidationErrorKind::ReturnTypeMismatch {
                return_type: func.return_type,
                local_0_type,
            },
            context: ctx.into(),
        });
    }
}

/// Check that Drop/DropIfAlive instructions target locals whose types actually need dropping.
fn check_drop_targets(
    func: &Function,
    ctx: &str,
    registry: &TypeRegistry,
    errors: &mut Vec<ValidationError>,
) {
    for block in &func.blocks {
        for inst in &block.instructions {
            let place = match inst {
                Instruction::Drop { place } | Instruction::DropIfAlive { place } => place,
                _ => continue,
            };
            // Only check simple locals (no projections — field drops are structural)
            if !place.projections.is_empty() {
                continue;
            }
            let local_idx = place.local.0 as usize;
            if local_idx >= func.locals.len() {
                continue; // Already caught by local-out-of-range check
            }
            let type_id = func.locals[local_idx].type_id;
            if !type_needs_drop(type_id, registry) {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::DropOnNonDroppable {
                        local: place.local,
                        type_id,
                    },
                    context: ctx.into(),
                });
            }
        }
    }
}

/// Check that all local type IDs reference valid types in the registry.
fn check_local_type_ids(
    func: &Function,
    ctx: &str,
    registry: &TypeRegistry,
    errors: &mut Vec<ValidationError>,
) {
    for (i, local) in func.locals.iter().enumerate() {
        if registry.get(local.type_id).is_none() {
            errors.push(ValidationError {
                kind: ValidationErrorKind::InvalidTypeId {
                    local: LocalId(i as u32),
                    type_id: local.type_id,
                },
                context: ctx.into(),
            });
        }
    }
}

/// Check that type metadata is internally consistent.
///
/// Flags types where CopySemantics and DropStrategy conflict in unexpected ways.
/// Known valid combinations:
/// - Copy + None: plain value types (primitives, simple structs)
/// - Copy + Trivial: ref-counted types (Shared, Weak, Channel) — Copy at GIR level
///   but need ref-count decrement at drop
/// - Move + None: ownership-tracking only (no heap to free, e.g. Thread)
/// - Move + Trivial/Custom/Recursive: standard owned types with cleanup
///
/// The only flagged case: Copy + Recursive or Copy + Custom, which would mean
/// the type can be freely copied but also runs complex cleanup — a likely bug.
fn check_drop_metadata_consistency(module: &Module, errors: &mut Vec<ValidationError>) {
    for type_def in module.type_registry.type_defs() {
        let is_suspicious = match (&type_def.metadata.copy_semantics, &type_def.metadata.drop_strategy) {
            // Copy + None: fine (plain value types)
            (CopySemantics::Trivial, DropStrategy::None) => false,
            // Copy + Trivial: fine (ref-counted types)
            (CopySemantics::Trivial, DropStrategy::Trivial(_)) => false,
            // Copy + Recursive or Copy + Custom: suspicious
            (CopySemantics::Trivial, DropStrategy::Recursive) => true,
            (CopySemantics::Trivial, DropStrategy::Custom(_)) => true,
            // Move + anything: fine
            (CopySemantics::Resource, _) => false,
        };
        if is_suspicious {
            errors.push(ValidationError {
                kind: ValidationErrorKind::InconsistentDropMetadata {
                    type_name: type_def.name.clone(),
                    copy_semantics: type_def.metadata.copy_semantics,
                    drop_strategy: format!("{:?}", type_def.metadata.drop_strategy),
                },
                context: "module".into(),
            });
        }
    }
}

/// Intra-block use-after-move detection.
///
/// Within each basic block, track which locals have been MoveZero'd.
/// If any subsequent instruction reads a moved local (before it's reassigned),
/// flag it as a potential use-after-move bug.
///
/// This is a conservative intra-block analysis — it won't catch cross-block
/// use-after-move patterns, which would require a full dataflow framework.
fn check_use_after_move(
    func: &Function,
    ctx: &str,
    errors: &mut Vec<ValidationError>,
) {
    for (block_idx, block) in func.blocks.iter().enumerate() {
        let mut moved: FxHashSet<u32> = FxHashSet::default();

        for inst in &block.instructions {
            // First: check if this instruction READS any moved locals
            let reads = collect_read_locals_for_validate(inst);
            for r in &reads {
                if moved.contains(r) {
                    errors.push(ValidationError {
                        kind: ValidationErrorKind::UseAfterMove {
                            local: LocalId(*r),
                            block: BlockId(block_idx as u32),
                        },
                        context: ctx.into(),
                    });
                }
            }

            // Then: update moved set based on writes
            match inst {
                Instruction::MoveZero { place } if place.projections.is_empty() => {
                    moved.insert(place.local.0);
                }
                // Any assignment to a simple local restores it (un-moves it)
                Instruction::Assign { dst, .. } if dst.projections.is_empty() => {
                    moved.remove(&dst.local.0);
                }
                // Other instructions that write to a local destination
                _ => {
                    if let Some(written) = instruction_write_local(inst) {
                        moved.remove(&written);
                    }
                }
            }
        }
    }
}

/// Get the destination local of an instruction (for un-moving tracking).
fn instruction_write_local(inst: &Instruction) -> Option<u32> {
    match inst {
        Instruction::BinOp { dst, .. }
        | Instruction::UnOp { dst, .. }
        | Instruction::Cmp { dst, .. }
        | Instruction::Cast { dst, .. }
        | Instruction::BitCast { dst, .. }
        | Instruction::PtrCast { dst, .. }
        | Instruction::FieldLoad { dst, .. }
        | Instruction::IndexLoad { dst, .. }
        | Instruction::HeapAlloc { dst, .. }
        | Instruction::HeapAllocArray { dst, .. }
        | Instruction::StructInit { dst, .. }
        | Instruction::EnumInit { dst, .. }
        | Instruction::TupleInit { dst, .. }
        | Instruction::TagOf { dst, .. }
        | Instruction::EnumFieldLoad { dst, .. }
        | Instruction::Borrow { dst, .. }
        | Instruction::BorrowMut { dst, .. }
        | Instruction::LoadThreadLocal { dst, .. } => Some(dst.0),
        Instruction::Call { dst: Some(d), .. }
        | Instruction::CallIndirect { dst: Some(d), .. }
        | Instruction::CallExtern { dst: Some(d), .. } => Some(d.0),
        _ => None,
    }
}

/// Collect all locals READ by an instruction (for use-after-move checking).
fn collect_read_locals_for_validate(inst: &Instruction) -> Vec<u32> {
    let mut reads = Vec::new();

    let push_op = |reads: &mut Vec<u32>, op: &Operand| {
        if let Operand::Copy(p) | Operand::Move(p) = op {
            reads.push(p.local.0);
            for proj in &p.projections {
                if let Projection::Index(id) = proj {
                    reads.push(id.0);
                }
            }
        }
    };
    let push_place = |reads: &mut Vec<u32>, p: &Place| {
        reads.push(p.local.0);
        for proj in &p.projections {
            if let Projection::Index(id) = proj {
                reads.push(id.0);
            }
        }
    };

    match inst {
        Instruction::Assign { dst, value, .. } => {
            if !dst.projections.is_empty() {
                push_place(&mut reads, dst);
            }
            push_op(&mut reads, value);
        }
        Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
            push_op(&mut reads, lhs);
            push_op(&mut reads, rhs);
        }
        Instruction::UnOp { operand, .. }
        | Instruction::Cast { value: operand, .. }
        | Instruction::BitCast { value: operand, .. }
        | Instruction::PtrCast { value: operand, .. }
        | Instruction::TagOf { operand, .. } => {
            push_op(&mut reads, operand);
        }
        Instruction::FieldLoad { base, .. } | Instruction::EnumFieldLoad { base, .. } => {
            push_place(&mut reads, base);
        }
        Instruction::IndexLoad { base, index, .. } => {
            push_place(&mut reads, base);
            push_op(&mut reads, index);
        }
        Instruction::Call { args, .. } | Instruction::CallExtern { args, .. } => {
            for a in args { push_op(&mut reads, a); }
        }
        Instruction::CallIndirect { callee, args, .. } => {
            push_op(&mut reads, callee);
            for a in args { push_op(&mut reads, a); }
        }
        Instruction::StructInit { fields, .. } | Instruction::EnumInit { fields, .. } => {
            for f in fields { push_op(&mut reads, f); }
        }
        Instruction::TupleInit { elements, .. } => {
            for e in elements { push_op(&mut reads, e); }
        }
        // Drop/DropIfAlive of a moved local is OK — that's the normal pattern
        // (MoveZero + DropIfAlive). Don't flag these as reads.
        Instruction::Drop { .. } | Instruction::DropIfAlive { .. } => {}
        // MoveZero reads the place to move from
        // But we handle MoveZero specially in the caller, so don't add reads here
        Instruction::MoveZero { .. } => {}
        Instruction::Borrow { place, .. } | Instruction::BorrowMut { place, .. } => {
            push_place(&mut reads, place);
        }
        Instruction::HeapAlloc { allocator, .. } => { push_op(&mut reads, allocator); }
        Instruction::HeapAllocArray { count, allocator, .. } => {
            push_op(&mut reads, count);
            push_op(&mut reads, allocator);
        }
        Instruction::Dealloc { ptr, allocator } => {
            push_op(&mut reads, ptr);
            push_op(&mut reads, allocator);
        }
        Instruction::PushAllocator { allocator } => { push_op(&mut reads, allocator); }
        Instruction::GlobalAssign { value, .. } => {
            push_op(&mut reads, value);
        }
        Instruction::PopAllocator | Instruction::Nop
        | Instruction::InlineC { .. } | Instruction::LoadThreadLocal { .. } => {}
    }
    reads
}

/// Check whether a type needs dropping. Delegates to `TypeRegistry::needs_drop()`.
fn type_needs_drop(type_id: TypeId, registry: &TypeRegistry) -> bool {
    registry.needs_drop(type_id)
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
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            ref_locals: rustc_hash::FxHashSet::default(),
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

    #[test]
    fn struct_init_field_count_mismatch() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata::default(),
        });
        module.type_registry.insert(GirType::Named("Point".into()));

        let point_id = module.type_registry.insert(GirType::Named("Point".into()));

        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        // StructInit with wrong field count (1 instead of 2)
        b.struct_init("Point", point_id, vec![Operand::Constant(Constant::F64(1.0))]);
        b.ret(Operand::Constant(Constant::Unit));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::StructFieldCountMismatch { expected: 2, got: 1, .. }
        )));
    }

    #[test]
    fn enum_variant_not_found() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "Color".into(),
            kind: TypeDefKind::Enum(EnumDef {
                variants: vec![
                    EnumVariant { name: "Red".into(), fields: vec![] },
                    EnumVariant { name: "Blue".into(), fields: vec![] },
                ],
            }),
            metadata: TypeMetadata::default(),
        });
        module.type_registry.insert(GirType::Named("Color".into()));

        let color_id = module.type_registry.insert(GirType::Named("Color".into()));

        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        b.enum_init("Color", "Green", color_id, vec![]);
        b.ret(Operand::Constant(Constant::Unit));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::EnumVariantNotFound { type_name, variant }
                if type_name == "Color" && variant == "Green"
        )));
    }

    #[test]
    fn drop_on_non_droppable_type() {
        let mut module = Module::new();
        // I64_TYPE is a primitive — doesn't need dropping
        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        let x = b.add_local(I64_TYPE, Some("x"));
        b.drop(Place::local(x));
        b.ret(Operand::Constant(Constant::Unit));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::DropOnNonDroppable { .. }
        )));
    }

    #[test]
    fn drop_on_droppable_type_ok() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("OwnedBuf".into()));

        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        let x = b.add_local(buf_id, Some("x"));
        b.drop(Place::local(x));
        b.ret(Operand::Constant(Constant::Unit));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::DropOnNonDroppable { .. })),
            "Should not flag Drop on a Move type with Trivial drop"
        );
    }

    #[test]
    fn invalid_local_type_id() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        // Add a local with a TypeId that doesn't exist in the registry
        b.add_local(TypeId(9999), Some("bad"));
        b.ret(Operand::Constant(Constant::Unit));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::InvalidTypeId { type_id: TypeId(9999), .. }
        )));
    }

    #[test]
    fn copy_recursive_drop_flagged() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "BadType".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Recursive,
                copy_semantics: CopySemantics::Trivial,
            },
        });

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::InconsistentDropMetadata { type_name, .. }
                if type_name == "BadType"
        )));
    }

    #[test]
    fn return_type_mismatch_detected() {
        let mut module = Module::new();
        // Create function where return_type is I64 but _0 is F64
        let func = Function {
            name: "bad_ret".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![Local {
                type_id: F64_TYPE, // _0 has wrong type
                name_hint: None,
            }],
            blocks: vec![{
                let mut bb = BasicBlock::new();
                bb.terminator = Some(Terminator::Return(Operand::Constant(Constant::I64(0))));
                bb
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            ref_locals: rustc_hash::FxHashSet::default(),
        };
        module.functions.push(func);

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::ReturnTypeMismatch { .. }
        )), "Should detect return type mismatch. Errors: {:?}", errors);
    }

    #[test]
    fn return_type_consistency_ok() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("ok_ret", I64_TYPE, &[]);
        b.ret(FunctionBuilder::const_i64(42));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::ReturnTypeMismatch { .. })),
            "Should not flag matching return type"
        );
    }

    #[test]
    fn use_after_move_detected() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("f", I64_TYPE, &[]);
        let x = b.add_local(I64_TYPE, Some("x"));
        b.assign(Place::local(x), FunctionBuilder::const_i64(42));
        // MoveZero _1 — local is now moved
        b.move_zero(Place::local(x));
        // Read _1 after move — use-after-move
        b.assign(
            Place::local(LocalId(0)),
            Operand::Copy(Place::local(x)),
        );
        b.ret(Operand::Copy(Place::local(LocalId(0))));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            ValidationErrorKind::UseAfterMove { local: LocalId(1), .. }
        )), "Should detect use-after-move. Errors: {:?}", errors);
    }

    #[test]
    fn no_use_after_move_with_reassign() {
        let mut module = Module::new();
        let mut b = FunctionBuilder::new("f", I64_TYPE, &[]);
        let x = b.add_local(I64_TYPE, Some("x"));
        b.assign(Place::local(x), FunctionBuilder::const_i64(42));
        // MoveZero _1
        b.move_zero(Place::local(x));
        // Reassign _1 — restores it
        b.assign(Place::local(x), FunctionBuilder::const_i64(99));
        // Read _1 — should be OK since it was reassigned
        b.assign(
            Place::local(LocalId(0)),
            Operand::Copy(Place::local(x)),
        );
        b.ret(Operand::Copy(Place::local(LocalId(0))));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::UseAfterMove { .. })),
            "Should not flag use after reassign. Errors: {:?}", errors
        );
    }

    #[test]
    fn drop_after_move_not_flagged() {
        // MoveZero + DropIfAlive is the standard pattern — don't flag it
        let mut module = Module::new();
        // Need a droppable type
        module.type_registry.add_type_def(TypeDef {
            name: "Buf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("Buf".into()));

        let mut b = FunctionBuilder::new("f", UNIT_TYPE, &[]);
        let x = b.add_local(buf_id, Some("x"));
        b.move_zero(Place::local(x));
        b.drop_if_alive(Place::local(x));
        b.ret(Operand::Constant(Constant::Unit));
        module.functions.push(b.build());

        let errors = validate(&module);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::UseAfterMove { .. })),
            "DropIfAlive after MoveZero is normal — should not flag. Errors: {:?}", errors
        );
    }

    #[test]
    fn copy_trivial_drop_ok() {
        let mut module = Module::new();
        // Ref-counted types are Copy + Trivial — this is intentional, not a bug
        module.type_registry.add_type_def(TypeDef {
            name: "SharedRef".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("shared_decref".into()),
                copy_semantics: CopySemantics::Trivial,
            },
        });

        let errors = validate(&module);
        assert!(
            !errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::InconsistentDropMetadata { .. })),
            "Copy + Trivial should be allowed for ref-counted types"
        );
    }

    #[test]
    fn span_map_mismatch_detected() {
        let mut module = Module::new();
        let f = Function {
            name: "test".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![Local { type_id: I64_TYPE, name_hint: None }],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Nop, Instruction::Nop],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::I64(0)))),
                span_map: vec![None], // 1 entry for 2 instructions — mismatch!
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            ref_locals: rustc_hash::FxHashSet::default(),
        };
        module.functions.push(f);
        let errors = validate(&module);
        assert!(errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::SpanMapMismatch { .. })));
    }

    #[test]
    fn span_map_correct_no_error() {
        let mut module = Module::new();
        let f = Function {
            name: "test".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![Local { type_id: I64_TYPE, name_hint: None }],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Nop],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::I64(0)))),
                span_map: vec![None], // 1 entry for 1 instruction — correct
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
            ref_locals: rustc_hash::FxHashSet::default(),
        };
        module.functions.push(f);
        let errors = validate(&module);
        assert!(!errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::SpanMapMismatch { .. })));
    }
}
