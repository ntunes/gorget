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
        Instruction::IndexLoad { dst, base, index, .. } => {
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
                Instruction::LoadRef { .. } | Instruction::StoreRef { .. } => {}
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
        Instruction::Call { func, .. } => {
            if !callables.contains(func.as_str()) && !func.starts_with("__callable_") && !func.starts_with("__gorget_closure_call_") {
                errors.push(ValidationError {
                    kind: ValidationErrorKind::UndefinedFunction(func.clone()),
                    context: ctx.into(),
                });
            }
        }
        Instruction::CallExtern { func, .. } => {
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
                // Allow drops on Option/Result enums with resource-type payloads
                // (force-registered at VarDecl).
                let is_force_droppable = if let Some(crate::ir::types::GirType::Named(name)) = registry.get(type_id) {
                    (name.starts_with("Option__") || name.starts_with("Result__"))
                    && registry.get_type_def(name).map_or(false, |td| {
                        if let crate::ir::types::TypeDefKind::Enum(ref edef) = td.kind {
                            edef.variants.iter().any(|v| v.fields.iter().any(|f|
                                type_needs_drop(f.type_id, registry)
                                || registry.is_resource_type(f.type_id)
                                || matches!(registry.get(f.type_id),
                                    Some(crate::ir::types::GirType::Named(n))
                                    if n == "GorgetString" || registry.is_collection_type_name(n))))
                        } else { false }
                    })
                } else { false };
                if !is_force_droppable {
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
                Instruction::LoadRef { .. } | Instruction::StoreRef { .. } => {}
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

// ── Phase C: resource-move validation ────────────────────────────────
// See docs/internals/unified-resource-model.md §5.
//
// The CoW contract: every read of a resource-typed value resolves to
// Move / Clone / Borrow. AssignMode::Copy of a resource source is a
// shallow alias of an owned resource — a latent double-free or
// aliased-mutable-state bug.
//
// Stage C1 (this commit): emitted as a warning. Gated behind the
// `GG_VALIDATE_RESOURCE_MOVES` env var so default builds (and CI) are
// unaffected; manual sweeps print the violation set so we can fix
// patterns by frequency. Stages C2-C4 fix the upstream lowering and
// then promote to a fail-fast error in `validate`.

/// A resource-move validation finding. Phase C, Stage C1: warning only.
#[derive(Debug, Clone)]
pub struct ResourceMoveWarning {
    pub function: String,
    pub block: BlockId,
    pub inst_index: usize,
    pub kind: ResourceMoveWarningKind,
}

#[derive(Debug, Clone)]
pub enum ResourceMoveWarningKind {
    /// `Assign { mode: Copy, value: Copy(place)|Move(place) }` where dst
    /// is resource-typed: a bit-copy of a resource value, aliasing the
    /// source's heap data without registering ownership transfer or a
    /// clone. The CoW spec mandates Move / Clone / Borrow at this site.
    ShallowCopyOfResource {
        local: LocalId,
        type_name: String,
    },
    /// `FieldLoad { dst, base, field }` where the field type is
    /// resource and the base is value-typed: produces a shallow copy of
    /// resource data into dst. Phase C: must be a Borrow-mode bind or a
    /// Clone at the boundary.
    ShallowCopyOfResourceField {
        dst: LocalId,
        field_type_name: String,
    },
    /// `IndexLoad { read: !Borrow }` where the element type is resource:
    /// produces a shallow alias of a collection element. Phase C: must
    /// use `read: ReadMode::Borrow` (LIR will emit a zero-copy view) or
    /// the upstream lowering must explicitly request `ReadMode::Clone`
    /// against an element-clone fn.
    ShallowReadOfResourceElement {
        dst: LocalId,
        elem_type_name: String,
    },
    /// `EnumFieldLoad { dst, base, variant, field }` where the
    /// variant's payload field is resource and not GorgetString (which
    /// auto-zeros on extract): produces a shallow copy of the payload
    /// without zeroing the source.
    ShallowCopyOfEnumPayload {
        dst: LocalId,
        variant: String,
        payload_type_name: String,
    },
    /// `Call { args }` or `CallExtern { args }` where an arg is
    /// `Operand::Copy(place)` of a resource-typed local AND the
    /// callee's parameter is `ByValue` (no borrow shape): the resource
    /// flows by shallow-copy across the call boundary. Phase C: must
    /// be Move (with MoveZero on the source slot) or Clone.
    ShallowCopyOfResourceArg {
        callee: String,
        arg_index: usize,
        arg_type_name: String,
    },
}

impl std::fmt::Display for ResourceMoveWarningKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ShallowCopyOfResource { local, type_name } => {
                write!(f, "shallow copy of resource _{} : {}", local.0, type_name)
            }
            Self::ShallowCopyOfResourceField { dst, field_type_name } => {
                write!(f, "shallow copy of resource field into _{} : {}", dst.0, field_type_name)
            }
            Self::ShallowReadOfResourceElement { dst, elem_type_name } => {
                write!(f, "shallow read of resource element into _{} : {} (borrow=false)", dst.0, elem_type_name)
            }
            Self::ShallowCopyOfEnumPayload { dst, variant, payload_type_name } => {
                write!(f, "shallow copy of enum payload {} into _{} : {}", variant, dst.0, payload_type_name)
            }
            Self::ShallowCopyOfResourceArg { callee, arg_index, arg_type_name } => {
                write!(f, "shallow copy of resource arg #{} : {} into call to {}", arg_index, arg_type_name, callee)
            }
        }
    }
}

impl std::fmt::Display for ResourceMoveWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}::bb{}::i{} — {}",
            self.function, self.block.0, self.inst_index, self.kind)
    }
}

/// Walk every Assign in the module and flag resource-typed shallow
/// copies. Stage C1: callers print these as warnings; the IR is not
/// rejected.
pub fn validate_resource_moves(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        check_resource_moves(func, &module.type_registry, &mut warnings);
    }
    warnings
}

fn check_resource_moves(
    func: &Function,
    registry: &TypeRegistry,
    warnings: &mut Vec<ResourceMoveWarning>,
) {
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            let Instruction::Assign { mode, dst, value } = inst else { continue };
            if *mode != AssignMode::Copy { continue; }
            // Only flag whole-local destinations. Projected stores
            // (`_x.field = ...`) are FieldStore semantics — the dst's
            // field type, not the whole struct, is what gets the copy.
            // The struct-typed base is incidental, not an alias source.
            if !dst.projections.is_empty() { continue; }
            // Only resource-typed destinations.
            let local_idx = dst.local.0 as usize;
            if local_idx >= func.locals.len() { continue; }
            let dst_ty = func.locals[local_idx].type_id;
            if !registry.is_resource_type(dst_ty) { continue; }
            // Only place-sourced operands. `Assign { mode: Copy, value:
            // Constant(_) }` is fine — that's a literal, not a shallow
            // alias of an owned resource.
            let src_place = match value {
                Operand::Copy(p) | Operand::Move(p) => p,
                _ => continue,
            };
            // Self-assignments aren't shallow copies in the bug sense.
            if src_place.local == dst.local && src_place.projections.is_empty() {
                continue;
            }
            // Auto-deref: two recognised shapes that codegen handles
            // correctly today (LoadRef-equivalent reads through a
            // pointer into a value slot). They're flagged duplicates,
            // not runtime risks; upstream lowering will be migrated to
            // explicit LoadRef / Borrow modes.
            //
            //   1. `dst:T = copy src:Ptr<T>`  — bare-place auto-deref
            //      (printer renders this as `dst = copy src`; the type
            //      mismatch tells codegen to deref).
            //   2. `dst:T = copy src.*`       — explicit Deref projection
            //      where src is `Ptr<T>`. Same semantic, different shape.
            {
                let src_idx = src_place.local.0 as usize;
                if src_idx < func.locals.len() {
                    let src_ty = func.locals[src_idx].type_id;
                    use crate::ir::types::GirType;
                    use crate::ir::instructions::Projection;
                    let pointee = match registry.get(src_ty) {
                        Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) => Some(*inner),
                        _ => None,
                    };
                    if pointee == Some(dst_ty) {
                        let is_bare = src_place.projections.is_empty();
                        let is_single_deref = src_place.projections.len() == 1
                            && matches!(src_place.projections[0], Projection::Deref);
                        if is_bare || is_single_deref {
                            continue;
                        }
                    }
                    // Type-mismatched assigns (`dst:Vector = copy src:i64`) are
                    // generic-monomorphization bugs, not shallow-resource-alias
                    // bugs. The Phase C validator is scoped to "shallow copy of
                    // owned resource"; flagging type-mismatched assigns is out
                    // of scope (and the runtime is producing wrong results
                    // there independently of the GIR mode label). Skip when
                    // src isn't the same resource type as dst.
                    //
                    // We allow same-type assigns (the genuine shallow-alias
                    // case) and assigns where src is non-place (constants,
                    // already excluded above). Other cross-type assigns fall
                    // out of validator scope.
                    if src_ty != dst_ty && src_place.projections.is_empty() {
                        continue;
                    }
                }
            }
            let type_name = registry.type_name(dst_ty)
                .unwrap_or_else(|| format!("ty{}", dst_ty.0));
            warnings.push(ResourceMoveWarning {
                function: func.name.clone(),
                block: BlockId(b as u32),
                inst_index: i,
                kind: ResourceMoveWarningKind::ShallowCopyOfResource {
                    local: dst.local,
                    type_name,
                },
            });
        }
    }
}

// ── Phase C extension: read-site validators ──────────────────────────
// Four additional read sites whose silent shallow-copy semantics are
// the structural cousins of `Assign { mode: Copy }`. Phase C's
// guarantee — no shallow alias of an owned resource — applies equally
// here. Stage 1: gated behind `GG_VALIDATE_RESOURCE_READS=1`, callers
// print warnings, do not panic. Once a class shows zero violations on
// the integration sweep, it can be promoted to fatal alongside the
// existing `validate_resource_moves` panic site.

/// Run the four extension validators for the read-side classes:
/// FieldLoad, IndexLoad, EnumFieldLoad, Call/CallExtern args. Returns a
/// flat `Vec<ResourceMoveWarning>`. Caller groups by kind.
pub fn validate_resource_reads(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        check_field_load_reads(func, &module.type_registry, &mut warnings);
        check_index_load_reads(func, &module.type_registry, &mut warnings);
        check_enum_field_load_reads(func, &module.type_registry, &mut warnings);
        check_call_arg_reads(func, module, &mut warnings);
    }
    warnings
}

/// Just the Call/CallExtern args check — promoted to fatal at the
/// `validate_resource_moves` site after the 2026-05-04 sweep showed
/// 0 violations across 1056 fixtures. Splitting it out so it can run
/// unconditionally while the other three classes (field/index/enum)
/// still surface warnings only.
pub fn validate_resource_call_args(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        check_call_arg_reads(func, module, &mut warnings);
    }
    warnings
}

/// Resolve the type at the end of a place's projection chain. Walks
/// Field projections through Struct/Enum TypeDefs and follows Deref
/// through Ptr/MutPtr. Index projections are traversed through Vector
/// element types when discoverable. Returns `None` when any projection
/// step can't be resolved (the validator simply skips those sites).
fn resolve_place_type(
    place: &Place,
    func: &Function,
    registry: &TypeRegistry,
) -> Option<TypeId> {
    let local_idx = place.local.0 as usize;
    if local_idx >= func.locals.len() { return None; }
    let mut cur = func.locals[local_idx].type_id;
    for proj in &place.projections {
        match proj {
            Projection::Deref => {
                match registry.get(cur)? {
                    GirType::Ptr(inner) | GirType::MutPtr(inner) => cur = *inner,
                    _ => return None,
                }
            }
            Projection::Field(f) => {
                let name = match registry.get(cur)? {
                    GirType::Named(n) => n.clone(),
                    GirType::Ptr(inner) | GirType::MutPtr(inner) => {
                        match registry.get(*inner)? {
                            GirType::Named(n) => n.clone(),
                            _ => return None,
                        }
                    }
                    _ => return None,
                };
                let td = registry.get_type_def(&name)?;
                if let TypeDefKind::Struct(ref sd) = td.kind {
                    cur = sd.fields.get(*f as usize)?.type_id;
                } else {
                    return None;
                }
            }
            Projection::Index(_) => {
                // Index projections in GIR Place are rare; we don't
                // need element-type resolution for the validator's
                // base-type checks.
                return None;
            }
        }
    }
    Some(cur)
}

/// Field type at index `field` for a value-typed struct base, or
/// `None` if the base type isn't a struct or the field is OOB. Looks
/// through Ptr/MutPtr wrapping (the field is on the pointee struct).
fn resolve_field_type_id(
    base_ty: TypeId,
    field: u32,
    registry: &TypeRegistry,
) -> Option<TypeId> {
    let pointee = match registry.get(base_ty)? {
        GirType::Ptr(inner) | GirType::MutPtr(inner) => *inner,
        _ => base_ty,
    };
    let name = match registry.get(pointee)? {
        GirType::Named(n) => n.clone(),
        _ => return None,
    };
    let td = registry.get_type_def(&name)?;
    if let TypeDefKind::Struct(ref sd) = td.kind {
        sd.fields.get(field as usize).map(|f| f.type_id)
    } else {
        None
    }
}

/// Variant payload field type, or None when the type isn't an enum or
/// variant/field is OOB.
fn resolve_enum_field_type_id(
    base_ty: TypeId,
    variant: &str,
    field: u32,
    registry: &TypeRegistry,
) -> Option<TypeId> {
    let pointee = match registry.get(base_ty)? {
        GirType::Ptr(inner) | GirType::MutPtr(inner) => *inner,
        _ => base_ty,
    };
    let name = match registry.get(pointee)? {
        GirType::Named(n) => n.clone(),
        _ => return None,
    };
    let td = registry.get_type_def(&name)?;
    if let TypeDefKind::Enum(ref ed) = td.kind {
        let v = ed.variants.iter().find(|v| v.name == variant)?;
        v.fields.get(field as usize).map(|f| f.type_id)
    } else {
        None
    }
}

/// Quick predicate: is `ty` a Ptr/MutPtr at the top level? Used to
/// skip dst-is-Ptr cases where the LIR materializes a borrow rather
/// than a value copy.
fn type_is_ptr(ty: TypeId, registry: &TypeRegistry) -> bool {
    matches!(registry.get(ty), Some(GirType::Ptr(_) | GirType::MutPtr(_)))
}

fn check_field_load_reads(
    func: &Function,
    registry: &TypeRegistry,
    warnings: &mut Vec<ResourceMoveWarning>,
) {
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            let Instruction::FieldLoad { dst, base, field } = inst else { continue };
            // dst-is-Ptr branch is borrow-shaped at LIR level (returns
            // field address, not field value). Not a shallow copy.
            let dst_idx = dst.0 as usize;
            if dst_idx >= func.locals.len() { continue; }
            let dst_ty = func.locals[dst_idx].type_id;
            if type_is_ptr(dst_ty, registry) { continue; }
            // Resolve base place type (after projections), look up
            // field type, check if it's resource.
            let base_ty = match resolve_place_type(base, func, registry) {
                Some(t) => t,
                None => continue,
            };
            let field_ty = match resolve_field_type_id(base_ty, *field, registry) {
                Some(t) => t,
                None => continue,
            };
            if !registry.is_resource_type(field_ty) { continue; }
            // The dst type and field type should match for genuine
            // shallow-copy (cross-type would be a generic-mono bug,
            // out of scope).
            if dst_ty != field_ty { continue; }
            let field_type_name = registry.type_name(field_ty)
                .unwrap_or_else(|| format!("ty{}", field_ty.0));
            warnings.push(ResourceMoveWarning {
                function: func.name.clone(),
                block: BlockId(b as u32),
                inst_index: i,
                kind: ResourceMoveWarningKind::ShallowCopyOfResourceField {
                    dst: *dst,
                    field_type_name,
                },
            });
        }
    }
}

fn check_index_load_reads(
    func: &Function,
    registry: &TypeRegistry,
    warnings: &mut Vec<ResourceMoveWarning>,
) {
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            let Instruction::IndexLoad { dst, base: _, index: _, read } = inst else { continue };
            // Phase D5: ReadMode::Borrow == legacy `borrow: true` (zero-copy view);
            // any other mode (Clone today; Copy/Move reserved) is value-shaped and
            // therefore the shallow-read class.
            if matches!(read, ReadMode::Borrow) { continue; }
            let dst_idx = dst.0 as usize;
            if dst_idx >= func.locals.len() { continue; }
            let dst_ty = func.locals[dst_idx].type_id;
            // Ptr-typed dst: LIR returns raw element pointer (borrow shape).
            if type_is_ptr(dst_ty, registry) { continue; }
            if !registry.is_resource_type(dst_ty) { continue; }
            let elem_type_name = registry.type_name(dst_ty)
                .unwrap_or_else(|| format!("ty{}", dst_ty.0));
            warnings.push(ResourceMoveWarning {
                function: func.name.clone(),
                block: BlockId(b as u32),
                inst_index: i,
                kind: ResourceMoveWarningKind::ShallowReadOfResourceElement {
                    dst: *dst,
                    elem_type_name,
                },
            });
        }
    }
}

fn check_enum_field_load_reads(
    func: &Function,
    registry: &TypeRegistry,
    warnings: &mut Vec<ResourceMoveWarning>,
) {
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            let Instruction::EnumFieldLoad { dst, base, variant, field } = inst else { continue };
            let dst_idx = dst.0 as usize;
            if dst_idx >= func.locals.len() { continue; }
            let dst_ty = func.locals[dst_idx].type_id;
            // Ptr-typed dst: LIR returns field address (borrow shape).
            if type_is_ptr(dst_ty, registry) { continue; }
            let base_ty = match resolve_place_type(base, func, registry) {
                Some(t) => t,
                None => continue,
            };
            let payload_ty = match resolve_enum_field_type_id(base_ty, variant, *field, registry) {
                Some(t) => t,
                None => continue,
            };
            if !registry.is_resource_type(payload_ty) { continue; }
            // GorgetString payloads are auto-zeroed by the LIR lowering
            // (see lir/lower/insts.rs is_str_field path). Skip — that's
            // a Move-semantic at LIR even though GIR shape is identical.
            if registry.type_name(payload_ty).as_deref() == Some("GorgetString") { continue; }
            let payload_type_name = registry.type_name(payload_ty)
                .unwrap_or_else(|| format!("ty{}", payload_ty.0));
            warnings.push(ResourceMoveWarning {
                function: func.name.clone(),
                block: BlockId(b as u32),
                inst_index: i,
                kind: ResourceMoveWarningKind::ShallowCopyOfEnumPayload {
                    dst: *dst,
                    variant: variant.clone(),
                    payload_type_name,
                },
            });
        }
    }
}

fn check_call_arg_reads(
    func: &Function,
    module: &Module,
    warnings: &mut Vec<ResourceMoveWarning>,
) {
    use crate::ir::lowering::context::ParamABI;
    let registry = &module.type_registry;
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            // Internal calls: ParamABI per arg.
            if let Instruction::Call { func: callee, args, .. } = inst {
                let Some(abis) = module.fn_param_abis.get(callee) else { continue };
                for (idx, arg) in args.iter().enumerate() {
                    let abi = abis.get(idx).copied().unwrap_or(ParamABI::ByValue);
                    if !matches!(abi, ParamABI::ByValue) { continue; }
                    let Operand::Copy(p) = arg else { continue };
                    let Some(src_ty) = resolve_place_type(p, func, registry) else { continue };
                    if !registry.is_resource_type(src_ty) { continue; }
                    let arg_type_name = registry.type_name(src_ty)
                        .unwrap_or_else(|| format!("ty{}", src_ty.0));
                    warnings.push(ResourceMoveWarning {
                        function: func.name.clone(),
                        block: BlockId(b as u32),
                        inst_index: i,
                        kind: ResourceMoveWarningKind::ShallowCopyOfResourceArg {
                            callee: callee.clone(),
                            arg_index: idx,
                            arg_type_name,
                        },
                    });
                }
            }
            // External calls: AbiKind per arg. Treat ByValue /
            // GorgetString / Auto as "by-value" shallow positions; Ptr
            // / VoidElem / CStr / BytePtr / Opaque / Scalar are not
            // resource-shallow-copy shapes.
            if let Instruction::CallExtern { func: callee, args, .. } = inst {
                let Some(extern_decl) = module.find_extern(callee) else { continue };
                for (idx, arg) in args.iter().enumerate() {
                    use crate::ir::abi::AbiKind;
                    let abi = extern_decl.param_abis.get(idx).copied().unwrap_or(AbiKind::Auto);
                    let by_value = matches!(abi, AbiKind::ByValue | AbiKind::GorgetString);
                    if !by_value { continue; }
                    let Operand::Copy(p) = arg else { continue };
                    let Some(src_ty) = resolve_place_type(p, func, registry) else { continue };
                    if !registry.is_resource_type(src_ty) { continue; }
                    let arg_type_name = registry.type_name(src_ty)
                        .unwrap_or_else(|| format!("ty{}", src_ty.0));
                    warnings.push(ResourceMoveWarning {
                        function: func.name.clone(),
                        block: BlockId(b as u32),
                        inst_index: i,
                        kind: ResourceMoveWarningKind::ShallowCopyOfResourceArg {
                            callee: callee.clone(),
                            arg_index: idx,
                            arg_type_name,
                        },
                    });
                }
            }
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
            param_abis: vec![],
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
                ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false,
            }],
            blocks: vec![BasicBlock::new()], // no terminator
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
            inner_shared_spawns: Vec::new(),
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
                ..Default::default()
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
                ..Default::default()
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
                ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false,
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
                ..Default::default()
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
                ..Default::default()
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
            locals: vec![Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false }],
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
            locals: vec![Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false }],
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
        };
        module.functions.push(f);
        let errors = validate(&module);
        assert!(!errors.iter().any(|e| matches!(e.kind, ValidationErrorKind::SpanMapMismatch { .. })));
    }
}
