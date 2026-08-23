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
        Instruction::BinOp { dst, lhs, rhs, .. }
        | Instruction::FaultableBinOp { dst, lhs, rhs, .. } => {
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
                //
                // Cluster 2 stylistic cleanup (2026-05-07): the original
                // payload-droppable check was a three-way disjunction
                // (type_needs_drop || is_resource_type || (Named-name-match
                // for GorgetString / collection_type)). All three clauses
                // covered the same set after the upgrade scan: GorgetString
                // has copy_semantics=Resource (so `needs_drop` returns true),
                // collection types carry collection_kind + Resource metadata,
                // and `needs_drop` is the wider predicate that subsumes
                // `is_resource_type`. The single `type_needs_drop` call
                // suffices.
                // Read typed `enum_category` (Phase A) — Option/Result detection.
                let is_force_droppable = if let Some(crate::ir::types::GirType::Named(name)) = registry.get(type_id) {
                    registry.get_type_def(name).map_or(false, |td| {
                        td.metadata.enum_category.is_some()
                        && if let crate::ir::types::TypeDefKind::Enum(ref edef) = td.kind {
                            edef.variants.iter().any(|v| v.fields.iter().any(|f|
                                type_needs_drop(f.type_id, registry)))
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
        | Instruction::FaultableBinOp { dst, .. }
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
        Instruction::BinOp { lhs, rhs, .. }
        | Instruction::FaultableBinOp { lhs, rhs, .. }
        | Instruction::Cmp { lhs, rhs, .. } => {
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
// See docs/devbook/25-structural-guards.md (Phase C).
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
/// copies. Phase D5 (this file's collapse) routes through the unified
/// [`validate_read`] predicate — the per-Assign upstream skips
/// (auto-deref shapes, cross-type generic-mono noise, self-assign,
/// constant-source) live in [`assign_read_site`] and flag the site as
/// ReadMode-Borrow / out-of-scope so the unified rule never sees them.
pub fn validate_resource_moves(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, inst) in bb.instructions.iter().enumerate() {
                let Some(site) = assign_read_site(func, &module.type_registry, inst, b, i) else { continue };
                if let Some(w) = validate_read(site, &module.type_registry) {
                    warnings.push(w);
                }
            }
        }
    }
    warnings
}

/// Findings of [`validate_resource_sites_all`], partitioned by class so
/// `lower_module` can emit class-specific diagnostics with identical
/// labels to the legacy split-walk path. Each bucket carries the same
/// `ResourceMoveWarning` values that the per-class entry points would
/// have returned individually.
///
/// Caller is expected to inspect each bucket independently — the buckets
/// were historically five separate fatal gates (`resource-moves`,
/// `resource-call-args`, `resource-index-reads`, `resource-enum-reads`,
/// `resource-field-reads`) and we preserve that fan-out.
#[derive(Debug, Default, Clone)]
pub struct ResourceSiteFindings {
    /// `Assign { mode: Copy }` shallow-resource sites
    /// (`ResourceMoveWarningKind::ShallowCopyOfResource`).
    pub assign: Vec<ResourceMoveWarning>,
    /// `Call` / `CallExtern` resource args at `ByValue`-shaped positions
    /// (`ShallowCopyOfResourceArg`).
    pub call_args: Vec<ResourceMoveWarning>,
    /// `IndexLoad` with non-Borrow mode on a resource element
    /// (`ShallowReadOfResourceElement`).
    pub index_reads: Vec<ResourceMoveWarning>,
    /// `EnumFieldLoad` of a resource payload without auto-zero
    /// (`ShallowCopyOfEnumPayload`).
    pub enum_reads: Vec<ResourceMoveWarning>,
    /// `FieldLoad` of a resource field without the consuming-self idiom
    /// (`ShallowCopyOfResourceField`).
    pub field_reads: Vec<ResourceMoveWarning>,
}

impl ResourceSiteFindings {
    /// `true` iff every bucket is empty.
    pub fn is_empty(&self) -> bool {
        self.assign.is_empty()
            && self.call_args.is_empty()
            && self.index_reads.is_empty()
            && self.enum_reads.is_empty()
            && self.field_reads.is_empty()
    }
}

/// Unified resource-site validator. Performs a SINGLE walk per function
/// covering the five legacy entry points:
///   * [`validate_resource_moves`] (Assign-class)
///   * [`validate_resource_call_args`]
///   * [`validate_resource_index_reads`]
///   * [`validate_resource_enum_reads`]
///   * [`validate_resource_field_reads`]
///
/// All five share the unified [`validate_read`] rule and produce
/// `ResourceMoveWarning` values; the only thing that differs is the
/// per-instruction *extractor*. Pre-D5 collapse `lower_module` ran each
/// validator as its own `for func in module.functions { ... }` loop —
/// five back-to-back walks doing essentially identical work, ~30% of
/// the `gir_lower` phase on the LW workload. This collapse drops it
/// to one walk and partitions the warnings into the same five buckets
/// so the existing class-specific fatal diagnostics stay byte-identical.
pub fn validate_resource_sites_all(module: &Module) -> ResourceSiteFindings {
    let mut findings = ResourceSiteFindings::default();
    let registry = &module.type_registry;
    for func in &module.functions {
        // Assign-class extractor (own loop because `assign_read_site`'s
        // shape doesn't fit `for_each_read_site`'s match arms — it
        // peeks at the dst's projections and the source's ownership tag
        // rather than the instruction discriminant alone). Sharing the
        // outer per-function loop is the win; the body is identical to
        // [`validate_resource_moves`].
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, inst) in bb.instructions.iter().enumerate() {
                if let Some(site) = assign_read_site(func, registry, inst, b, i) {
                    if let Some(w) = validate_read(site, registry) {
                        findings.assign.push(w);
                    }
                }
            }
        }
        // Read-site classes (FieldLoad / IndexLoad / EnumFieldLoad /
        // Call*-ByValue arg). One walk; per-class dispatch on the
        // warning's discriminant — cheaper than re-walking four times
        // with class filters.
        for_each_read_site(func, module, |site| {
            let class_bucket = match &site.class {
                ReadSiteClass::Assign { .. } => return, // emitted above
                ReadSiteClass::FieldLoad { .. } => &mut findings.field_reads,
                ReadSiteClass::IndexLoad { .. } => &mut findings.index_reads,
                ReadSiteClass::EnumFieldLoad { .. } => &mut findings.enum_reads,
                ReadSiteClass::CallArg { .. } => &mut findings.call_args,
            };
            if let Some(w) = validate_read(site, registry) {
                class_bucket.push(w);
            }
        });
    }
    findings
}

/// Extract the conceptual ReadSite for an `Assign { mode: Copy }`
/// instruction, applying the per-Assign skips before returning. Returns
/// `None` when the assign is out-of-scope for the validator (any of the
/// skip predicates fires) — letting the unified [`validate_read`] rule
/// see ONLY the same shallow-copy-of-resource shape it sees for the
/// other classes.
///
/// Skips folded into the extractor:
/// * non-Copy modes (Move/Clone/Borrow are sound by construction);
/// * projected destinations (FieldStore semantics — handled by FieldLoad);
/// * constant-source operands (`x = 42` isn't a shallow alias);
/// * self-assignments (`x = x`);
/// * auto-deref bare-place / single-Deref shapes (`dst:T = copy src:Ptr<T>`);
/// * cross-type bare-place assigns (generic-mono bugs, out of scope).
fn assign_read_site<'a>(
    func: &'a Function,
    registry: &TypeRegistry,
    inst: &'a Instruction,
    b: usize,
    i: usize,
) -> Option<ReadSite<'a>> {
    let Instruction::Assign { mode, dst, value } = inst else { return None };
    if *mode != AssignMode::Copy { return None; }
    // FieldStore semantics — the dst's field type drives the check, not
    // the whole-local case. Out of Assign scope.
    if !dst.projections.is_empty() { return None; }
    let local_idx = dst.local.0 as usize;
    if local_idx >= func.locals.len() { return None; }
    let dst_ty = func.locals[local_idx].type_id;
    // Constants (`x = 42`) aren't shallow aliases of owned resources.
    let src_place = match value {
        Operand::Copy(p) | Operand::Move(p) => p,
        _ => return None,
    };
    // Self-assignments — not the shallow-alias bug shape.
    if src_place.local == dst.local && src_place.projections.is_empty() {
        return None;
    }
    // Auto-deref + cross-type skips: see the original commentary in
    // c6fedd4c. Both flag dst as Borrow-shaped for the unified rule
    // (sound), so we don't need a separate path.
    let src_idx = src_place.local.0 as usize;
    if src_idx < func.locals.len() {
        let src_ty = func.locals[src_idx].type_id;
        let pointee = match registry.get(src_ty) {
            Some(GirType::Ptr(inner) | GirType::MutPtr(inner)) => Some(*inner),
            _ => None,
        };
        // `dst:T = copy src:Ptr<T>` (bare-place) or `dst:T = copy src.*`
        // (single-Deref) — codegen materialises this as a LoadRef-equivalent
        // borrow, not a shallow copy.
        if pointee == Some(dst_ty) {
            let is_bare = src_place.projections.is_empty();
            let is_single_deref = src_place.projections.len() == 1
                && matches!(src_place.projections[0], Projection::Deref);
            if is_bare || is_single_deref {
                return None;
            }
        }
        // Cross-type bare-place assigns — generic-mono bugs, not shallow-alias.
        if src_ty != dst_ty && src_place.projections.is_empty() {
            return None;
        }
        // View-awareness probe (2026-05-11): a Copy of a `Borrowed` or
        // `View` source is runtime-safe — both source and copy are
        // non-owning aliases (cap=0 GorgetString views, Ptr-typed
        // borrows), so the resulting byte-copy creates another
        // non-owning alias whose drop is a no-op. Mirrors the
        // `validate_consume_sites` rule at L2507. This skip is the
        // basic-Assign-class equivalent.
        use crate::ir::LocalOwnership;
        if src_place.projections.is_empty()
            && matches!(
                func.locals[src_idx].ownership,
                LocalOwnership::Borrowed { .. } | LocalOwnership::View { .. }
            )
        {
            return None;
        }
    }
    Some(ReadSite {
        func_name: &func.name,
        block: BlockId(b as u32),
        inst_index: i,
        mode: ReadMode::Copy,
        source_ty: dst_ty,
        class: ReadSiteClass::Assign { dst_local: dst.local },
    })
}

// ── Phase C extension: read-site validators ──────────────────────────
// Phase D5 collapse (`docs/devbook/13-ownership-in-ir.md`):
// the four read-site classes (FieldLoad, IndexLoad, EnumFieldLoad,
// Call/CallExtern args) and the original Assign-Copy class share one
// underlying rule:
//
//   "is this read of a resource-typed value shaped as a shallow copy?"
//
// Each per-instruction walker now extracts a typed [`ReadSite`]
// describing the conceptual read (mode + source type + per-class
// metadata) and routes through the single [`validate_read`] predicate.
// Adding a future read class only requires extending [`ReadSite`] and
// registering one extractor — the validation rule itself is one match.

/// Run the four extension validators for the read-side classes:
/// FieldLoad, IndexLoad, EnumFieldLoad, Call/CallExtern args. Returns a
/// flat `Vec<ResourceMoveWarning>`. Caller groups by kind.
pub fn validate_resource_reads(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for_each_read_site(func, module, |site| {
            if let Some(w) = validate_read(site, &module.type_registry) {
                warnings.push(w);
            }
        });
    }
    warnings
}

/// Just the Call/CallExtern args class — promoted to fatal at the
/// `validate_resource_moves` site after the 2026-05-04 sweep showed
/// 0 violations across 1056 fixtures. Splitting it out so it can run
/// unconditionally while the other three classes (field/index/enum)
/// still surface warnings only.
pub fn validate_resource_call_args(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for_each_read_site(func, module, |site| {
            if !matches!(site.class, ReadSiteClass::CallArg { .. }) { return; }
            if let Some(w) = validate_read(site, &module.type_registry) {
                warnings.push(w);
            }
        });
    }
    warnings
}

/// Just the IndexLoad class — promoted to fatal at the
/// `validate_resource_moves` site after the 2026-05-06 sweep showed
/// 0 violations across 1066 fixtures. The CoW lowering already routes
/// resource-typed elements through `Ptr(T)` (zero-copy borrow shape) at
/// the resource-element index_access path; for-loop iteration emits
/// `index_load_borrow` for resource elements; the remaining `index_load`
/// (Clone) callers either operate on non-resource elements or wrap with
/// proper element-clone routing in the LIR. Splitting this out lets it
/// run unconditionally alongside the call-arg fatal.
pub fn validate_resource_index_reads(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for_each_read_site(func, module, |site| {
            if !matches!(site.class, ReadSiteClass::IndexLoad { .. }) { return; }
            if let Some(w) = validate_read(site, &module.type_registry) {
                warnings.push(w);
            }
        });
    }
    warnings
}

/// Just the EnumFieldLoad class — promoted to fatal at the
/// `validate_resource_moves` site after the 2026-05-06 LIR-side migration
/// (lir/lower/insts.rs `payload_is_resource` widening) drove the count
/// from 3750 to 0 across 1068 fixtures. Auto-zero of the source field
/// after extraction is now unconditional for all resource payloads
/// (was previously gated to GorgetString only); the validator's
/// `for_each_read_site` walker now reports Move for every resource
/// payload to mirror the lowering. Any future lowering that emits
/// `EnumFieldLoad` of a resource payload through a non-Ptr dst without
/// the LIR auto-zero halts the build instead of leaking past the
/// validator.
pub fn validate_resource_enum_reads(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for_each_read_site(func, module, |site| {
            if !matches!(site.class, ReadSiteClass::EnumFieldLoad { .. }) { return; }
            if let Some(w) = validate_read(site, &module.type_registry) {
                warnings.push(w);
            }
        });
    }
    warnings
}

/// Just the FieldLoad class — promoted to fatal at the
/// `validate_resource_moves` site after the 2026-05-06 FieldLoad
/// migration drove the integration sweep to 0 violations. Migration
/// covered: `lower_field_access` (drop the `base_is_ptr &&` guard);
/// closure ByValue capture loads (`Ptr(cap_type)` shape); spawn
/// closure-arg extraction (`field_load + move_zero` across the spawn
/// boundary); `Pattern::Tuple` destructure (move-out vs Ptr-wrap by
/// ownership); `Expr::TupleFieldAccess` (same Ptr-wrap shape); plus
/// the validator's FieldLoad-then-MoveZero peek encoding the `!self`
/// consuming-self idiom.
pub fn validate_resource_field_reads(module: &Module) -> Vec<ResourceMoveWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for_each_read_site(func, module, |site| {
            if !matches!(site.class, ReadSiteClass::FieldLoad { .. }) { return; }
            if let Some(w) = validate_read(site, &module.type_registry) {
                warnings.push(w);
            }
        });
    }
    warnings
}

/// A typed read-site descriptor: enough to (a) route through the unified
/// validator and (b) build the per-class warning when a violation fires.
///
/// Each case in [`ReadSiteClass`] corresponds to one of the read-site
/// classes from §6.4. Adding a new class is a one-shot extension — the
/// validate rule is shared and the location plumbing is shared.
struct ReadSite<'a> {
    /// Function whose body owns this site (for the warning's display).
    func_name: &'a str,
    /// Block + instruction index that emitted the site.
    block: BlockId,
    inst_index: usize,
    /// How the value is read at this site.
    mode: ReadMode,
    /// Type that flows out of the read (the dst's effective type, or
    /// the field/element type).
    source_ty: TypeId,
    /// Shape-specific data needed to build the warning kind.
    class: ReadSiteClass<'a>,
}

enum ReadSiteClass<'a> {
    /// `Assign { mode, dst, value }` — D5: AssignMode IS ReadMode.
    /// The Phase C-original class. `dst_local` carries the dst place's
    /// LocalId for the warning; `is_auto_deref_skip` and friends are
    /// resolved upstream by the extractor (the predicate here stays
    /// shape-agnostic).
    Assign { dst_local: LocalId },
    /// `FieldLoad { dst, base, field }`. ReadMode is implicit at this
    /// site (FieldLoad has no mode field — it always copies bytes), so
    /// the extractor synthesises `ReadMode::Copy` for shallow-copy
    /// extraction and `ReadMode::Borrow` when the dst is Ptr-typed.
    FieldLoad { dst_local: LocalId },
    /// `IndexLoad { dst, base, index, read }` — D5: `read` is the
    /// authoritative ReadMode for the site.
    IndexLoad { dst_local: LocalId },
    /// `EnumFieldLoad { dst, base, variant, field }`. ReadMode is
    /// synthesised: the extractor knows the LIR auto-zeroes
    /// GorgetString payloads (effectively Move) and that other resource
    /// payloads stay shallow-copy unless upstream lowering opts in.
    EnumFieldLoad { dst_local: LocalId, variant: &'a str },
    /// Call / CallExtern arg position. ReadMode synthesised from ABI:
    /// ByValue/GorgetString/Auto positions are shallow-copy candidates;
    /// Ptr/borrow positions are `ReadMode::Borrow`.
    CallArg { callee: &'a str, arg_index: usize },
}

/// The unified rule. Returns `Some(warning)` when the site is a shallow
/// alias of an owned resource, `None` when sound or out-of-scope.
///
/// Per `docs/devbook/13-ownership-in-ir.md` the rule is:
/// * If `source_ty` is non-resource → sound (any mode is fine).
/// * `Borrow` → sound (destination is a reference / view).
/// * `Move`   → sound (ownership transfer; source becomes dead).
/// * `Clone`  → sound (deep clone via the type's clone fn).
/// * `Copy`   → **violation** — shallow alias of an owned resource.
fn validate_read(site: ReadSite<'_>, registry: &TypeRegistry) -> Option<ResourceMoveWarning> {
    if !registry.is_resource_type(site.source_ty) { return None; }
    match site.mode {
        ReadMode::Borrow | ReadMode::Move | ReadMode::Clone => None,
        ReadMode::Copy => Some(ResourceMoveWarning {
            function: site.func_name.to_string(),
            block: site.block,
            inst_index: site.inst_index,
            kind: warning_kind_for(&site, registry),
        }),
    }
}

/// Build the per-class warning kind from the site descriptor. The
/// fan-out lives here (one match) instead of being duplicated across
/// per-class checkers.
fn warning_kind_for(site: &ReadSite<'_>, registry: &TypeRegistry) -> ResourceMoveWarningKind {
    let type_name = registry.type_name(site.source_ty)
        .unwrap_or_else(|| format!("ty{}", site.source_ty.0));
    match &site.class {
        ReadSiteClass::Assign { dst_local } => ResourceMoveWarningKind::ShallowCopyOfResource {
            local: *dst_local,
            type_name,
        },
        ReadSiteClass::FieldLoad { dst_local } => ResourceMoveWarningKind::ShallowCopyOfResourceField {
            dst: *dst_local,
            field_type_name: type_name,
        },
        ReadSiteClass::IndexLoad { dst_local } => ResourceMoveWarningKind::ShallowReadOfResourceElement {
            dst: *dst_local,
            elem_type_name: type_name,
        },
        ReadSiteClass::EnumFieldLoad { dst_local, variant } => ResourceMoveWarningKind::ShallowCopyOfEnumPayload {
            dst: *dst_local,
            variant: variant.to_string(),
            payload_type_name: type_name,
        },
        ReadSiteClass::CallArg { callee, arg_index } => ResourceMoveWarningKind::ShallowCopyOfResourceArg {
            callee: callee.to_string(),
            arg_index: *arg_index,
            arg_type_name: type_name,
        },
    }
}

/// Walk every instruction in `func`, extract the conceptual read sites
/// of resource values, and call `visit` on each. Per-instruction shape
/// extraction lives here — the validation rule itself is one match in
/// [`validate_read`]. The Phase C `Assign { mode: Copy }` class lives
/// in [`check_resource_moves`] (which uses its own auto-deref skip
/// logic that doesn't fit the simple ReadMode rule); the four extension
/// classes flow through this walker.
fn for_each_read_site<'a, F: FnMut(ReadSite<'a>)>(
    func: &'a Function,
    module: &'a Module,
    mut visit: F,
) {
    let registry = &module.type_registry;
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            let block = BlockId(b as u32);
            match inst {
                Instruction::FieldLoad { dst, base, field } => {
                    let Some(dst_ty) = func.locals.get(dst.0 as usize).map(|l| l.type_id) else { continue };
                    // Ptr-typed dst at LIR level is borrow-shaped (field address, not value).
                    // FieldLoad-followed-by-MoveZero on the same source field is the
                    // !self consuming-self idiom (and the closure-env-by-value capture
                    // load via field_load + move_zero in StructInit move pending list):
                    // the GIR contract is Move, but FieldLoad has no mode field. The
                    // peek-next-inst lookahead encodes that idiom so the validator
                    // doesn't flag it as shallow-copy. Phase C FieldLoad migration
                    // 2026-05-06: every other FieldLoad of a resource field flows
                    // through the Ptr(T) borrow path (lower_field_access), so this
                    // peek is the only remaining shape that legitimately produces a
                    // value-typed FieldLoad of a resource field.
                    let mode = if type_is_ptr(dst_ty, registry) {
                        ReadMode::Borrow
                    } else if next_inst_zeroes_field(&bb.instructions, i, base, *field) {
                        ReadMode::Move
                    } else {
                        ReadMode::Copy
                    };
                    let Some(base_ty) = resolve_place_type(base, func, registry) else { continue };
                    let Some(field_ty) = resolve_field_type_id(base_ty, *field, registry) else { continue };
                    // Cross-type assigns (`dst:Vector = field of unrelated`) are
                    // generic-mono bugs, out of validator scope.
                    if dst_ty != field_ty { continue; }
                    visit(ReadSite {
                        func_name: &func.name,
                        block,
                        inst_index: i,
                        mode,
                        source_ty: field_ty,
                        class: ReadSiteClass::FieldLoad { dst_local: *dst },
                    });
                }
                Instruction::IndexLoad { dst, base: _, index: _, read } => {
                    let Some(dst_ty) = func.locals.get(dst.0 as usize).map(|l| l.type_id) else { continue };
                    // Ptr-typed dst: raw element pointer == borrow.
                    let mode = if type_is_ptr(dst_ty, registry) { ReadMode::Borrow } else { *read };
                    visit(ReadSite {
                        func_name: &func.name,
                        block,
                        inst_index: i,
                        mode,
                        source_ty: dst_ty,
                        class: ReadSiteClass::IndexLoad { dst_local: *dst },
                    });
                }
                Instruction::EnumFieldLoad { dst, base, variant, field, .. } => {
                    let Some(dst_ty) = func.locals.get(dst.0 as usize).map(|l| l.type_id) else { continue };
                    // Ptr-typed dst: LIR returns field address (borrow).
                    if type_is_ptr(dst_ty, registry) {
                        // No need to resolve — borrow is sound regardless of payload type.
                        visit(ReadSite {
                            func_name: &func.name,
                            block,
                            inst_index: i,
                            mode: ReadMode::Borrow,
                            source_ty: dst_ty,
                            class: ReadSiteClass::EnumFieldLoad { dst_local: *dst, variant },
                        });
                        continue;
                    }
                    let Some(base_ty) = resolve_place_type(base, func, registry) else { continue };
                    let Some(payload_ty) = resolve_enum_field_type_id(base_ty, variant, *field, registry) else { continue };
                    // Resource payloads are auto-zeroed by the LIR lowering
                    // (see lir/lower/insts.rs `payload_is_resource` path) —
                    // the GIR shape is identical (`EnumFieldLoad` with no mode
                    // field) but the lowering emits a post-extract field zero
                    // for every resource type, so the read is Move-semantic at
                    // LIR. Non-resource payloads are bytes-copy and sound. The
                    // 2026-05-06 widening covered all resource types (was
                    // previously gated to GorgetString); validator mode mirrors
                    // the lowering's predicate.
                    let mode = if registry.is_resource_type(payload_ty) {
                        ReadMode::Move
                    } else {
                        ReadMode::Copy
                    };
                    visit(ReadSite {
                        func_name: &func.name,
                        block,
                        inst_index: i,
                        mode,
                        source_ty: payload_ty,
                        class: ReadSiteClass::EnumFieldLoad { dst_local: *dst, variant },
                    });
                }
                Instruction::Call { func: callee, args, .. } => {
                    use crate::ir::lowering::context::ParamABI;
                    let Some(abis) = module.fn_param_abis.get(callee) else { continue };
                    for (idx, arg) in args.iter().enumerate() {
                        let abi = abis.get(idx).copied().unwrap_or(ParamABI::ByValue);
                        // Internal calls: only ByValue is shallow-copy-shaped.
                        let mode = if matches!(abi, ParamABI::ByValue) { ReadMode::Copy } else { ReadMode::Borrow };
                        let Operand::Copy(p) = arg else { continue };
                        let Some(src_ty) = resolve_place_type(p, func, registry) else { continue };
                        visit(ReadSite {
                            func_name: &func.name,
                            block,
                            inst_index: i,
                            mode,
                            source_ty: src_ty,
                            class: ReadSiteClass::CallArg { callee, arg_index: idx },
                        });
                    }
                }
                Instruction::CallExtern { func: callee, args, .. } => {
                    use crate::ir::abi::AbiKind;
                    let Some(extern_decl) = module.find_extern(callee) else { continue };
                    for (idx, arg) in args.iter().enumerate() {
                        let abi = extern_decl.param_abis.get(idx).copied().unwrap_or(AbiKind::Auto);
                        // Externs: ByValue/GorgetString/Auto are shallow-copy positions;
                        // Ptr/VoidElem/CStr/BytePtr/Opaque/Scalar are borrow-shaped.
                        let by_value = matches!(abi, AbiKind::ByValue | AbiKind::GorgetString);
                        let mode = if by_value { ReadMode::Copy } else { ReadMode::Borrow };
                        let Operand::Copy(p) = arg else { continue };
                        let Some(src_ty) = resolve_place_type(p, func, registry) else { continue };
                        visit(ReadSite {
                            func_name: &func.name,
                            block,
                            inst_index: i,
                            mode,
                            source_ty: src_ty,
                            class: ReadSiteClass::CallArg { callee, arg_index: idx },
                        });
                    }
                }
                _ => {}
            }
        }
    }
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

/// Peek the instruction at `i + 1`: does it `MoveZero` the source field of
/// the FieldLoad at index `i`? This is the !self consuming-self idiom emitted
/// by `lower_field_access` (and the closure-env-StructInit move-pending list):
/// the GIR shape is `_dst = field_load _base.<base_proj>.field; move_zero
/// _base.<base_proj>.field`. Together they encode a Move semantic that
/// FieldLoad's instruction shape can't otherwise express. Returns false at
/// the end of the block — the idiom always emits MoveZero in the same bb.
fn next_inst_zeroes_field(insts: &[Instruction], i: usize, base: &Place, field: u32) -> bool {
    let Some(next) = insts.get(i + 1) else { return false };
    let Instruction::MoveZero { place: zp } = next else { return false };
    if zp.local != base.local { return false; }
    // The MoveZero place is `base.projections + Field(field)`.
    if zp.projections.len() != base.projections.len() + 1 { return false; }
    if zp.projections[..base.projections.len()] != base.projections[..] { return false; }
    matches!(zp.projections.last(), Some(Projection::Field(f)) if *f == field)
}

// ── Tier 1b: Move follow-through validator ───────────────────────────
// `docs/devbook/25-structural-guards.md` §Tier 1b.
//
// Invariant. Every `Inst::Assign { mode: Move, value: Copy(p) | Move(p) }`
// whose source `p` is drop-registered (i.e. some `Drop` / `DropIfAlive`
// in the same function targets `p.local` bare) must be followed by a
// `MoveZero` of `p` — in the same basic block — before any subsequent
// `Drop` / `DropIfAlive` of `p` is emitted. Move means transfer of
// ownership; declaring it without zeroing the source is the snag #19 /
// #23 bug shape (shallow-copy aliasing the heap pointer that scope-exit
// drops then double-frees).
//
// Phase C catches the symmetric Copy-direction class (shallow copy of a
// resource); this validator closes the Move-direction. Both shapes
// produce the same use-after-free at runtime — Phase C from the source
// side, this from the destination's perspective on the source.

/// A Move-follow-through validation finding. Tier 1b: warning during
/// the env-gated stage; once the initial sweep is clean, the validator
/// is promoted to fatal in `lowering/mod.rs`.
#[derive(Debug, Clone)]
pub struct MoveFollowThroughWarning {
    pub function: String,
    pub block: BlockId,
    pub inst_index: usize,
    pub kind: MoveFollowThroughWarningKind,
}

#[derive(Debug, Clone)]
pub enum MoveFollowThroughWarningKind {
    /// `Assign { mode: Move, value: Copy(p) | Move(p) }` where `p` is a
    /// drop-registered local (some `Drop`/`DropIfAlive` later targets
    /// `p.local`) but the same basic block does NOT emit a `MoveZero(p)`
    /// before the next drop site references `p`. The source slot stays
    /// alive after the move; both source and destination then drop the
    /// same heap allocation. See snag #19 / #23.
    MoveWithoutZero {
        source_local: LocalId,
        dst_local: LocalId,
    },
}

impl std::fmt::Display for MoveFollowThroughWarningKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MoveWithoutZero { source_local, dst_local } => write!(
                f,
                "Move-mode assign _{} = move _{}: source is drop-registered but is not MoveZero'd before the next drop site",
                dst_local.0, source_local.0
            ),
        }
    }
}

impl std::fmt::Display for MoveFollowThroughWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@{}::bb{}::i{} — {}",
            self.function, self.block.0, self.inst_index, self.kind
        )
    }
}

/// Tier 1b validator. Walk every function's basic blocks; for each
/// `Inst::Assign { mode: Move, value: Copy(p) | Move(p) }` where `p` is
/// bare (no projections) and `p.local` is drop-registered in this
/// function, scan the same block forward for a bare `MoveZero(p)` (or
/// any subsequent `Drop`/`DropIfAlive` of `p` — that's the violation).
///
/// "Drop-registered" is detected structurally: a local is drop-registered
/// if any `Drop`/`DropIfAlive { place }` in the function targets
/// `place.local == p.local && place.projections.is_empty()`. The drop
/// accountant's lowering-time `is_registered` state isn't preserved past
/// lowering; the IR's drop instructions ARE that ground truth at the
/// validator stage.
pub fn validate_move_follow_through(module: &Module) -> Vec<MoveFollowThroughWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        // Pre-compute: which locals receive a bare-place Drop or DropIfAlive
        // anywhere in this function? Those are the drop-registered ones.
        let drop_registered = collect_drop_registered_locals(func);
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, inst) in bb.instructions.iter().enumerate() {
                let Instruction::Assign { mode, dst, value } = inst else { continue };
                if *mode != AssignMode::Move { continue }
                // Self-assigns aren't move-follow-through sites.
                let src_place = match value {
                    Operand::Copy(p) | Operand::Move(p) => p,
                    _ => continue,
                };
                if !src_place.projections.is_empty() { continue }
                let src_local = src_place.local;
                if !drop_registered.contains(&src_local) { continue }
                // The dst can also be projected (struct-field move-into);
                // for the warning's display we still report the base dst.
                let dst_local = dst.local;
                // Walk forward in the block. The first event for src_local:
                // - bare MoveZero(src_local) → followed-through, OK
                // - bare Drop / DropIfAlive(src_local) → violation
                // - end of block → conservatively NOT a violation (the
                //   drop, if any, must be in a subsequent block, and the
                //   move-zero must precede it; but the validator can't see
                //   inter-block flow without a full dataflow pass. The
                //   doc says "in the same basic block" — we honour that
                //   bound and the writer-site fix is to MoveZero locally).
                let mut found_violation = false;
                let mut zeroed = false;
                for follow in bb.instructions.iter().skip(i + 1) {
                    match follow {
                        Instruction::MoveZero { place } if place.local == src_local && place.projections.is_empty() => {
                            zeroed = true;
                            break;
                        }
                        Instruction::Drop { place } | Instruction::DropIfAlive { place }
                            if place.local == src_local && place.projections.is_empty() =>
                        {
                            found_violation = true;
                            break;
                        }
                        _ => {}
                    }
                }
                if !zeroed && found_violation {
                    warnings.push(MoveFollowThroughWarning {
                        function: func.name.clone(),
                        block: BlockId(b as u32),
                        inst_index: i,
                        kind: MoveFollowThroughWarningKind::MoveWithoutZero {
                            source_local: src_local,
                            dst_local,
                        },
                    });
                }
            }
        }
    }
    warnings
}

// ── Tier 1c: TypeDef metadata coherence at registration ──────────────
// See `docs/devbook/25-structural-guards.md` §1c for the invariant.
//
// Every registered TypeDef whose fields/variant-payloads contain a
// droppable type must itself have a non-None drop_strategy and Resource
// copy_semantics. Today this is enforced post-hoc by
// `upgrade_types_from_fields` — but that creates a timing class where
// late-registered Options/tuples/structs carry stale metadata between
// registration and the next upgrade scan.
//
// The validator walks every TypeDef and compares the recorded metadata
// to what `compute_drop_strategy_for_struct/_for_enum` would compute
// now. Any mismatch indicates a registration site that didn't write
// coherent metadata at construction — those are the migration targets.

/// A single Tier 1c coherence violation.
#[derive(Debug, Clone)]
pub struct TypeMetadataCoherenceWarning {
    pub type_name: String,
    /// What `compute_drop_strategy_for_struct/_for_enum` returned NOW.
    pub expected_drop: DropStrategy,
    pub expected_copy: CopySemantics,
    /// What's recorded on the TypeDef.
    pub actual_drop: DropStrategy,
    pub actual_copy: CopySemantics,
    /// Whether the TypeDef is a struct or enum (for grouping).
    pub kind: TypeMetadataCoherenceKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeMetadataCoherenceKind {
    Struct,
    Enum,
}

impl std::fmt::Display for TypeMetadataCoherenceWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = match self.kind {
            TypeMetadataCoherenceKind::Struct => "struct",
            TypeMetadataCoherenceKind::Enum => "enum",
        };
        write!(f,
            "{} {}: expected ({:?}, {:?}), actual ({:?}, {:?})",
            kind, self.type_name,
            self.expected_drop, self.expected_copy,
            self.actual_drop, self.actual_copy)
    }
}

/// Run the Tier 1c TypeDef metadata coherence validator.
///
/// Walks every TypeDef and returns a warning for any whose metadata is
/// less restrictive than `compute_drop_strategy_for_struct/_for_enum`
/// would yield NOW. False positives are impossible at the
/// `(DropStrategy, CopySemantics)` axis: the helper only "upgrades"
/// from `(None, Trivial)` to `(Recursive, Resource)`. A site that
/// recorded `Trivial("fn")` / `Custom("fn")` / explicit `Recursive` is
/// considered coherent — the helper would have upgraded to Recursive,
/// but the recorded strategy is already non-None, so the validator
/// treats it as already correct (the writer was explicit and we trust
/// it).
///
/// **Smart-pointer-wrapper carve-out.** A single-field struct
/// `{ _0: T }` registered with `(Trivial, None)` and no
/// `enum_category` / `collection_kind` is the signature of the
/// Mutex/RWLock-style "permanent singleton" wrapper. The writer
/// chose Trivial+None EXPLICITLY (these handles are never freed at
/// the GIR level; the inner T's lifecycle is managed at the runtime
/// level via `mutex_destroy` etc.). The validator skips this case to
/// avoid flagging the writer's deliberate design. The signature is
/// structural: number of fields + recorded copy/drop + absence of
/// enum_category / collection_kind. No name-matching.
///
/// Returns the violations sorted by type name (stable output).
pub fn validate_type_metadata_coherence(
    module: &Module,
) -> Vec<TypeMetadataCoherenceWarning> {
    let mut warnings = Vec::new();
    for td in module.type_registry.type_defs() {
        let (expected_drop, expected_copy) = match &td.kind {
            TypeDefKind::Struct(sdef) => {
                module.type_registry.compute_drop_strategy_for_struct(&sdef.fields)
            }
            TypeDefKind::Enum(edef) => {
                module.type_registry.compute_drop_strategy_for_enum(&edef.variants)
            }
            // Aliases don't have their own drop metadata — they defer to
            // the aliased TypeId.
            TypeDefKind::Alias(_) => continue,
        };
        // Coherence rule: if the helper says "must be Resource+Recursive"
        // but the TypeDef recorded (None, Trivial), that's a violation.
        // Any explicit non-None strategy is accepted (the writer was
        // explicit and chose Trivial("free") / Custom / Recursive).
        let actual_drop = td.metadata.drop_strategy.clone();
        let actual_copy = td.metadata.copy_semantics;

        // Smart-pointer wrapper carve-out (see doc above).
        if let TypeDefKind::Struct(sdef) = &td.kind {
            let is_single_field_wrapper = sdef.fields.len() == 1
                && sdef.fields[0].name == "_0"
                && td.metadata.enum_category.is_none()
                && td.metadata.collection_kind.is_none()
                && actual_drop == DropStrategy::None
                && actual_copy == CopySemantics::Trivial;
            if is_single_field_wrapper {
                continue;
            }
        }

        // Closure-env carve-out: structs tagged `is_closure_env: true`
        // (`closures.rs:140`) capture outer-scope locals at non-last-use as
        // lifetime-tied aliases — the closure does NOT independently own
        // those captured values. Outer-scope drops handle cleanup; the
        // env struct itself stays `(None, Trivial)` so scope-exit doesn't
        // double-free. The consume-site validator already skips StructInit
        // fields for closure-env destinations; the coherence validator
        // skips them here for the same reason. See
        // `docs/devbook/12-gir-lowering.md` (closure lowering and capture).
        if td.metadata.is_closure_env {
            continue;
        }

        if expected_drop == DropStrategy::Recursive
            && actual_drop == DropStrategy::None
        {
            let kind = match &td.kind {
                TypeDefKind::Struct(_) => TypeMetadataCoherenceKind::Struct,
                TypeDefKind::Enum(_) => TypeMetadataCoherenceKind::Enum,
                TypeDefKind::Alias(_) => unreachable!(),
            };
            warnings.push(TypeMetadataCoherenceWarning {
                type_name: td.name.clone(),
                expected_drop,
                expected_copy,
                actual_drop,
                actual_copy,
                kind,
            });
        } else if expected_drop == DropStrategy::Recursive
            && actual_copy != CopySemantics::Resource
        {
            // drop_strategy is set, but copy_semantics still Trivial:
            // ref-counted types are intentional (Channel/Shared/Weak —
            // Copy + Trivial(decref)), so only warn when drop_strategy
            // is Recursive — that case combines "transitive drop" with
            // "Copy semantics" which is incoherent.
            if matches!(actual_drop, DropStrategy::Recursive) {
                let kind = match &td.kind {
                    TypeDefKind::Struct(_) => TypeMetadataCoherenceKind::Struct,
                    TypeDefKind::Enum(_) => TypeMetadataCoherenceKind::Enum,
                    TypeDefKind::Alias(_) => unreachable!(),
                };
                warnings.push(TypeMetadataCoherenceWarning {
                    type_name: td.name.clone(),
                    expected_drop,
                    expected_copy,
                    actual_drop,
                    actual_copy,
                    kind,
                });
            }
        }
    }
    warnings.sort_by(|a, b| a.type_name.cmp(&b.type_name));
    warnings
}

/// Identify locals that receive a bare-place `Drop`/`DropIfAlive`
/// instruction anywhere in `func`. These are "drop-registered" from the
/// validator's vantage point — the lowering passes emitted a drop site
/// for them, so a Move-mode assign of them must be followed through with
/// a MoveZero (otherwise the drop will fire on a slot whose ownership was
/// transferred elsewhere — snag #19 / #23 shape).
fn collect_drop_registered_locals(func: &Function) -> FxHashSet<LocalId> {
    let mut set = FxHashSet::default();
    for bb in &func.blocks {
        for inst in &bb.instructions {
            let place = match inst {
                Instruction::Drop { place } | Instruction::DropIfAlive { place } => place,
                _ => continue,
            };
            if place.projections.is_empty() {
                set.insert(place.local);
            }
        }
    }
    set
}

/// A drop-pre-rebind validation finding. Tier 2c: snag #23 class.
///
/// When a value flows into a heap-allocating consumer (`__gorget_box_alloc_*`,
/// `gorget_string_clone_to_owned`, `gorget_array_clone`, etc.) the source's
/// drop registration must be retired *before* any subsequent drop emission
/// targets the source — i.e. the source slot must be `MoveZero`'d AND
/// `drops.mark_moved` must fire, not just `drops.unregister`.
///
/// Snag #23's bug: `Box.new(!lhs)` only called `drops.unregister(lhs)`. The
/// scope-exit drop was retired, but `lower_assign`'s INSTRUCTION-LEVEL
/// pre-rebind drop still saw the source slot as alive — and freed the
/// interior pointers the new Box now owns. This validator locks the rule
/// so a future heap-allocating consumer can't forget the move-zero step.
#[derive(Debug, Clone)]
pub struct DropPreRebindWarning {
    pub function: String,
    pub block: BlockId,
    pub inst_index: usize,
    pub callee: String,
    pub source_local: LocalId,
}

impl std::fmt::Display for DropPreRebindWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@{}::bb{}::i{} — call to heap-allocating consumer `{}` with Copy/Move(_{}): source is drop-registered but is not MoveZero'd before the next drop site (snag #23 class)",
            self.function, self.block.0, self.inst_index, self.callee, self.source_local.0
        )
    }
}

/// Tier 2c — drop-tracking pre-rebind correctness. Walks every function's
/// blocks; for each `Call`/`CallExtern` to a **shallow-copy heap-allocating
/// consumer** with a `Copy(p)` or `Move(p)` arg where `p` is bare and
/// drop-registered, scan the same block forward for a `MoveZero(p)` before
/// the next bare `Drop`/`DropIfAlive(p)` site. Mismatch is the snag #23 shape.
///
/// **Shallow-copy heap-allocating consumer.** The class is narrow on
/// purpose: only `__gorget_box_alloc_<T>` qualifies today. Box.new
/// `__gorget_box_alloc_<T>(value)` shallow-copies the value's interior
/// pointers into a fresh heap slot — both source and the new Box now alias
/// the same heap data, so the source MUST be MoveZero'd before any
/// subsequent Drop, otherwise that drop frees what the new Box owns.
///
/// **NOT in scope** (deep-clone consumers — source stays independent, Drop
/// of source is fine):
/// - `gorget_string_clone_to_owned`, `gorget_array_clone`, `gorget_map_clone`,
///   `gorget_set_clone`: produce a new owned value with new interior storage;
///   source's storage is untouched. A later Drop of source is correct.
/// - `gorget_*_clone_inplace`: write into an existing slot, no shallow alias.
///
/// **Recognition is typed.** The classifier reads
/// `Module::heap_alloc_consumer_externs` — populated at the writer site
/// every time the GIR lowering emits a `__gorget_box_alloc_<T>` call.
/// Adding a new shallow-copy heap-allocating consumer at any future
/// writer site is a single `module.heap_alloc_consumer_externs.insert(...)`
/// call and the validator picks it up automatically. No
/// `name.starts_with(...)` substring match survives in this validator.
/// Per CLAUDE.md "No name matching".
pub fn validate_drop_pre_rebind(module: &Module) -> Vec<DropPreRebindWarning> {
    let mut warnings = Vec::new();

    // Typed metadata: the lowering populates this set at every Box.new
    // emission site (see `Module::heap_alloc_consumer_externs`). The
    // validator reads it as a structural fact — never re-derives it
    // from the callee identifier shape.
    let heap_alloc_consumers = &module.heap_alloc_consumer_externs;
    if heap_alloc_consumers.is_empty() {
        return warnings;
    }

    for func in &module.functions {
        let drop_registered = collect_drop_registered_locals(func);
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, inst) in bb.instructions.iter().enumerate() {
                let (callee, args) = match inst {
                    Instruction::Call { func: name, args, .. } => (name.as_str(), args),
                    Instruction::CallExtern { func: name, args, .. } => (name.as_str(), args),
                    _ => continue,
                };
                if !heap_alloc_consumers.contains(callee) { continue }

                for arg in args {
                    let src_place = match arg {
                        Operand::Copy(p) | Operand::Move(p) => p,
                        _ => continue,
                    };
                    if !src_place.projections.is_empty() { continue }
                    let src_local = src_place.local;
                    if !drop_registered.contains(&src_local) { continue }

                    // Walk forward in the same block. First event for src_local:
                    //   - MoveZero(src_local) → followed-through, OK
                    //   - Drop / DropIfAlive(src_local) → snag #23 violation
                    //   - end of block → conservatively pass (cross-block drop
                    //     would be a different shape, beyond this validator's
                    //     same-block scope, mirroring Tier 1b's bound).
                    let mut zeroed = false;
                    let mut violated = false;
                    for follow in bb.instructions.iter().skip(i + 1) {
                        match follow {
                            Instruction::MoveZero { place } if place.local == src_local && place.projections.is_empty() => {
                                zeroed = true;
                                break;
                            }
                            Instruction::Drop { place } | Instruction::DropIfAlive { place }
                                if place.local == src_local && place.projections.is_empty() =>
                            {
                                violated = true;
                                break;
                            }
                            _ => {}
                        }
                    }
                    if !zeroed && violated {
                        warnings.push(DropPreRebindWarning {
                            function: func.name.clone(),
                            block: BlockId(b as u32),
                            inst_index: i,
                            callee: callee.to_string(),
                            source_local: src_local,
                        });
                    }
                }
            }
        }
    }
    warnings
}

// ── Snag #32 family: None-literal materialisation at writer boundaries ─

/// One violation of the "no Null assign to tagged-enum slot" invariant.
#[derive(Debug, Clone)]
pub struct NullAssignToOptionWarning {
    pub function: String,
    pub block: BlockId,
    pub inst_index: usize,
    pub dst_type_name: String,
}

impl std::fmt::Display for NullAssignToOptionWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "@{}::bb{}::i{} — `Inst::Assign` of `Constant::Null` into a tagged-enum slot (`{}`); writer must materialise the variant via `coerce_null_to_option_none` or `materialise_none_for_expected_type` (snag #32 class)",
            self.function, self.block.0, self.inst_index, self.dst_type_name
        )
    }
}

/// Snag #32 family — None-literal materialisation at writer boundaries.
///
/// Walks every `Instruction::Assign { dst, value: Constant::Null, .. }` and
/// validates that the dst's *resolved* type isn't a tagged enum wrapper
/// (`Option__T` or `Result__T__E`). The C backend renders `Constant::Null` as
/// a 40-byte zero-store, which (given the `Some=0 / None=1` discriminator
/// layout) silently produces a `Some(empty payload)` zombie — Snag #32. The
/// IR-lowering writer must rewrite the value to an `enum_init <T> None []`
/// before emitting, via `LoweringContext::coerce_null_to_option_none` (chokepoint
/// for field-store / index-store / deref-store) or
/// `materialise_none_for_expected_type` (chokepoint for the
/// `Expr::NoneLiteral` and `Expr::Call { callee: NoneLiteral }` lowering).
///
/// **Recognition is structural, not name-pattern**: the dst's resolved type
/// name is matched against the typed `enum_category` metadata on the
/// TypeDef when available, falling back to the `Option__` / `Result__`
/// mangle-prefix in case the late-registered wrapper hasn't yet had its
/// metadata stamped. The fallback is the same shape as
/// `materialise_none_for_expected_type` — both will route through the
/// typed flag once the IR-layer Option/Result wrapper migration in
/// Cluster 1 lands.
pub fn validate_no_null_assign_to_option_slot(
    module: &Module,
) -> Vec<NullAssignToOptionWarning> {
    let mut warnings = Vec::new();
    for func in &module.functions {
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, inst) in bb.instructions.iter().enumerate() {
                let (dst, value) = match inst {
                    Instruction::Assign { dst, value, .. } => (dst, value),
                    _ => continue,
                };
                if !matches!(value, Operand::Constant(crate::ir::instructions::Constant::Null)) {
                    continue;
                }
                let Some(dst_ty) = resolve_place_type(dst, func, &module.type_registry) else { continue };
                let Some(GirType::Named(name)) = module.type_registry.get(dst_ty) else { continue };
                let name = name.clone();
                let is_tagged_enum = name.starts_with("Option__")
                    && !name.starts_with("Option__Ref__")
                    || name.starts_with("Result__");
                if !is_tagged_enum {
                    continue;
                }
                warnings.push(NullAssignToOptionWarning {
                    function: func.name.clone(),
                    block: BlockId(b as u32),
                    inst_index: i,
                    dst_type_name: name,
                });
            }
        }
    }
    warnings
}

// ── Tier 2a Phase 1: consume-site discipline (CoW write-side) ────────
// See `docs/devbook/25-structural-guards.md` Tier 2a (and the project
// brief in the Phase 1 task) for the full design.
//
// Companion to Phase C's READ-site validators (above): every consuming
// position (push / put / insert / send / IndexStore / EnumInit /
// StructInit / BoxNew / function arg with `ParamABI::ByValue`) must
// see an IR shape consistent with the source's typed `LocalOwnership`
// AND its post-call liveness. The four cases:
//
// | Source state                      | Required IR shape          |
// |-----------------------------------|----------------------------|
// | Owned AND dead at this call       | Move(p) + MoveZero(p)      |
// | Borrow OR owned-but-live          | Clone-then-Move            |
// | Static literal                    | Runtime *_materialize      |
//
// Today the IR commonly emits `Operand::Copy(p)` and relies on
// `drops.unregister(p)` to pretend it's a Move — which is wrong when the
// source is live past the call. Snag #24 (TODO) is the runtime
// double-free that motivates this work.
//
// Phase 1 (this commit): build the validator + liveness pass, run an
// initial sweep with env-gated logging, classify violations, file Phase
// 2 migration TODOs. NO writer-site migrations in this phase.

use crate::ir::liveness::Liveness;
use crate::ir::{LocalOwnership, BorrowOrigin};

/// Class of consuming position. Each variant carries enough metadata to
/// build a meaningful diagnostic. The classification is data-driven by
/// the IR shape, not by name-matching — `CollectionMutator` only fires
/// at runtime calls flagged via the typed `runtime_callees` table /
/// per-callee ABI metadata (see `for_each_consume_site` for how each
/// class is detected).
#[derive(Debug, Clone)]
pub enum ConsumeSiteClass {
    /// `Call/CallExtern` to a runtime collection mutator (push / put /
    /// insert / send / IndexStore lowered to a runtime call) where the
    /// element / value arg is consumed. Today these are the
    /// `gorget_array_push`, `gorget_map_put`, `gorget_set_insert`,
    /// `gorget_channel_send`, etc. families. Detection: callee in the
    /// inverse `runtime_callees` table OR a known runtime-prefix arg
    /// position with `AbiKind::VoidElem`/`Auto`.
    CollectionMutator { callee: String, arg_index: usize },
    /// `EnumInit { fields[arg_index] }` — every value-typed field is a
    /// consume site at the constructor.
    EnumInit { variant: String, arg_index: usize },
    /// `StructInit { fields[arg_index] }`.
    StructInit { type_name: String, arg_index: usize },
    /// `HeapAlloc` payload assignments (BoxNew lowered shape: alloc + a
    /// store of the payload through the new pointer). Today the GIR
    /// emits this as `HeapAlloc + Assign(deref) = value`; we treat the
    /// payload Assign as a consume site when the deref destination is
    /// resource-typed.
    BoxNew,
    /// `Call` arg position with `ParamABI::ByValue` (internal calls).
    /// Internal calls are validated by Phase C's read-side as
    /// `ShallowCopyOfResourceArg` for the *type* axis; Tier 2a extends
    /// to the *liveness* axis on top of that.
    CallByValueArg { callee: String, arg_index: usize },
    /// `CallExtern` arg position with `AbiKind::ByValue` /
    /// `AbiKind::GorgetString` — same idea, ABI-routed through the
    /// extern decl's per-param annotation.
    CallExternByValueArg { callee: String, arg_index: usize },
    /// `Inst::Assign { dst, value }` where `dst` is a resource-typed
    /// owned-required slot (Owned/FreshOwned/Untracked ownership) and
    /// `value` is a place operand. The motivating bug class is Snag #28
    /// (commit `179202ed`): a Ptr-typed borrow source flows into a
    /// resource-typed dst via `[Mv] _result = copy _ptr`, and the
    /// codegen materialises that as a memcpy of the pointee struct
    /// (data+cap+len+alloc) into the dst — both alias the same heap
    /// data, double-drop at scope exit. Per the CoW spec table in
    /// `AGENTS.md` *Ownership at Consuming Positions*, a borrow source
    /// crossing into an owned destination requires a clone. This class
    /// closes the gap left by the source-type-gated classes above:
    /// they require source's TYPE to be resource (which Ptr<T> isn't),
    /// missing exactly the Snag #28 shape.
    AssignIntoOwnedSlot { dst_type: String },
    /// `Inst::Assign { dst: _0, value }` — a store into the FUNCTION
    /// RETURN PLACE. Structurally identical to `AssignIntoOwnedSlot`
    /// (a resource-typed slot that the CALLER will drop), but invisible
    /// to that class because `_0` is minted `Untracked`
    /// (`builder.rs`, the `_0 = return place` slot) and the
    /// `AssignIntoOwnedSlot` gate skips every non-`Owned` dst. That gap
    /// is why the whole `return`-borrow double-free family — `return v`
    /// / `T local = v; return local` / `local = v; return local` over a
    /// `T &v` param — walked past an always-fatal validator unseen.
    ///
    /// `_0`-is-the-return-place is a STRUCTURAL IR invariant, so the
    /// predicate belongs here rather than in a writer that tags `_0`:
    /// tagging `_0` `Owned` at construction would silently no-op the
    /// `set_ref(LocalId(0))` on the return path's no-clone-fn
    /// Ptr-propagation leg (`set_ref` only writes into `Untracked`),
    /// leaving `_0` `Owned` while it holds a borrowed pointer — a NEW
    /// double-free injected by the guard itself.
    ///
    /// RUNWAY: this class is emitted NON-FATALLY (routed to the
    /// `assign_warnings` list in `lowering/mod.rs`) until its corpus
    /// count is burned down to zero; `GG_RETURN_SLOT_GUARD=fatal`
    /// promotes it for burn-down runs. See `docs/devbook/25`.
    AssignIntoReturnSlot { dst_type: String },
    /// `Inst::Assign { dst, value }` that is sound under the normal
    /// liveness rule ONLY because the site emitted its own `MoveZero` on
    /// the source — the **staging move** shape:
    ///
    /// ```text
    /// [Mv] _dst = copy _src
    ///      move_zero _src        // <- emitted by the same lowering site
    ///      ... copy _src ...     // <- a REAL later read
    /// ```
    ///
    /// `Liveness::compute` counts `MoveZero` as a kill (`liveness.rs`,
    /// `MoveZeroPolicy::Kills`), so `live_after(_src)` is `false` at the
    /// assign and [`validate_assign_consume`] returns "sound". **The defect
    /// is its own alibi**: the very instruction that creates the aliasing
    /// hazard is what makes the guard pass it. No walk over the instruction
    /// stream can observe the class; only the counterfactual can.
    ///
    /// So this class re-asks the same question against a
    /// [`MoveZeroPolicy::Blind`] liveness — "would the source still be read
    /// if the `MoveZero` were not there?" — and fires when the answer is
    /// yes. The consequence in the backend is a live aliasing hazard: the
    /// zero is elided whenever drop-tracking proves it unobservable (which
    /// the `Kills` liveness just concluded), leaving `_dst` and `_src`
    /// pointing at one buffer — double-free at scope exit, or a
    /// use-after-free if the survivor reallocs.
    ///
    /// RUNWAY: emitted NON-FATALLY (routed to the `assign_warnings` list in
    /// `lowering/mod.rs`) until its corpus count is burned down to zero;
    /// `GG_STAGING_MOVE_GUARD=fatal` promotes it for burn-down runs, and
    /// `scripts/staging_move_burndown.sh` runs that promotion in CI. See
    /// `docs/devbook/25`.
    StagingMoveIntoOwnedSlot { dst_type: String },
}

/// A single consume-site finding. The Phase 1 sweep emits these as
/// warnings — the validator log accumulates them per ConsumeSiteClass +
/// per `(ownership, live_after, is_move)` tuple so Phase 2 can plan the
/// migrations.
#[derive(Debug, Clone)]
pub struct ConsumeSiteWarning {
    pub function: String,
    pub block: BlockId,
    pub inst_index: usize,
    pub class: ConsumeSiteClass,
    /// LocalId of the source operand (the local being consumed).
    pub source_local: LocalId,
    /// Type name of the source for diagnostic clustering.
    pub source_type_name: String,
    /// The classification of the violation.
    pub violation: ConsumeSiteViolation,
}

/// Why a consume-site fails the rule. Each variant maps to one of the
/// "INVALID" rows in the Phase 1 brief's table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConsumeSiteViolation {
    /// Source is `Owned` AND live past this call, but the IR uses
    /// `Operand::Copy(p)` without a preceding `_temp = clone(p)` —
    /// the C-backend would either double-free at scope exit or leak.
    OwnedLiveSourceConsumed,
    /// Source is a borrow (Borrowed / View / SharedHeap shape) — the
    /// callee is going to take ownership but the IR is consuming a
    /// non-owning slot. Must be cloned at the boundary.
    BorrowedSourceConsumed,
    /// Source's `LocalOwnership` is `Untracked` AND the local is a
    /// resource-typed value at a consume site. Untracked is the
    /// FxHashMap-absence default — the lowering didn't decide. Phase D
    /// rules say resource locals must transit through a concrete state
    /// before crossing an ownership boundary; flag for review.
    UntrackedSourceConsumed,
    /// Source is `MaybeOwned`, meaning some paths borrowed and some
    /// materialised — the IR must follow the conditional-drop discipline
    /// (DropIfAlive + memcmp-zero) rather than emit a plain Copy.
    MaybeOwnedSourceConsumed,
}

impl std::fmt::Display for ConsumeSiteViolation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OwnedLiveSourceConsumed =>
                write!(f, "owned-but-live source consumed without preceding clone"),
            Self::BorrowedSourceConsumed =>
                write!(f, "borrowed source consumed at consuming position"),
            Self::UntrackedSourceConsumed =>
                write!(f, "untracked source consumed (ownership not decided)"),
            Self::MaybeOwnedSourceConsumed =>
                write!(f, "maybe-owned source consumed without conditional drop"),
        }
    }
}

impl std::fmt::Display for ConsumeSiteClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CollectionMutator { callee, arg_index } =>
                write!(f, "CollectionMutator({}, arg #{})", callee, arg_index),
            Self::EnumInit { variant, arg_index } =>
                write!(f, "EnumInit({}, arg #{})", variant, arg_index),
            Self::StructInit { type_name, arg_index } =>
                write!(f, "StructInit({}, arg #{})", type_name, arg_index),
            Self::BoxNew => write!(f, "BoxNew"),
            Self::CallByValueArg { callee, arg_index } =>
                write!(f, "CallByValueArg({}, arg #{})", callee, arg_index),
            Self::CallExternByValueArg { callee, arg_index } =>
                write!(f, "CallExternByValueArg({}, arg #{})", callee, arg_index),
            Self::AssignIntoOwnedSlot { dst_type } =>
                write!(f, "AssignIntoOwnedSlot(dst: {})", dst_type),
            Self::AssignIntoReturnSlot { dst_type } =>
                write!(f, "AssignIntoReturnSlot(dst: {})", dst_type),
            Self::StagingMoveIntoOwnedSlot { dst_type } =>
                write!(f, "StagingMoveIntoOwnedSlot(dst: {})", dst_type),
        }
    }
}

impl std::fmt::Display for ConsumeSiteWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}::bb{}::i{} — {} of _{} : {} : {}",
            self.function, self.block.0, self.inst_index,
            self.class, self.source_local.0, self.source_type_name, self.violation)
    }
}

/// Run the Tier 2a Phase 1 consume-site validator over the module.
///
/// Returns a flat `Vec<ConsumeSiteWarning>`. Caller groups by
/// `class` + `violation` to plan Phase 2 migrations.
///
/// ⚠ This validator is **NOT** env-gated and **NOT** warn-only. It runs
/// UNCONDITIONALLY from `lowering/mod.rs` and PANICS on the first fatal
/// violation. `GG_VALIDATE_CONSUME_SITES=<path>` only opens an optional
/// structured LOG alongside the panic — it neither enables nor suppresses
/// the check. (The former "the env gate is the only consumer today" note
/// here was a Phase-1 fossil that outlived Phase 3's promotion and has
/// misled readers into believing a new class could land warn-only.)
///
/// A NEW class is therefore fatal the instant it is emitted. The runway
/// for one is the non-fatal `assign_warnings` partition in
/// `lowering/mod.rs` plus its own opt-in promotion gate — see
/// [`ConsumeSiteClass::AssignIntoReturnSlot`].
///
/// Builds the module-wide clone-fn name set once via
/// [`TypeRegistry::clone_fn_names_set`] (Phase 2E migration); the
/// per-callee membership check in [`preceded_by_clone`] is a typed
/// O(1) lookup, never a `__clone` suffix match.
pub fn validate_consume_sites(module: &Module) -> Vec<ConsumeSiteWarning> {
    let mut warnings = Vec::new();
    let clone_fns = module.type_registry.clone_fn_names_set();
    for func in &module.functions {
        let liveness = Liveness::compute(func);
        // Second instrument, for the `StagingMoveIntoOwnedSlot` class only:
        // the same analysis with `MoveZero` neutralised, so a staging site
        // that manufactures its own alibi can still be seen. Consulted ONLY
        // after the normal run has already returned "sound" — the shared
        // kill edge is untouched for every other consumer.
        let liveness_mz_blind = Liveness::compute_move_zero_blind(func);
        for_each_consume_site(
            func, module, &liveness, &liveness_mz_blind, &clone_fns,
            |w| warnings.push(w),
        );
    }
    warnings
}

// ── G3: clone-reason validation ──────────────────────────────────────
// Design goal (TODO.md materialization-planner campaign; CLAUDE.md Core
// #2/#3/#6 + devbook/24 layering): every compiler-emitted CLONE carries a
// typed `MaterializeReason` (`Instruction::Call.reason`) naming WHICH
// ownership boundary demanded it. This validator asserts direction (a):
// no clone-emitting instruction without a reason. Direction (b) — no
// planned directive left unconsumed — arrives with the planner-directive
// table (a future `position` axis the planner emits and lowering
// consumes); this stub validates only (a) during burn-down.
//
// A clone Call is identified WITHOUT name-matching the callee: `clone_fns`
// is the module's authoritative typed clone-fn set (`clone_fn_names_set`,
// built from per-TypeDef `clone_fn_name_for_def`). `reason.is_some()` is
// the classified signal. The `func ∈ clone_fns` fallback lets the
// validator SEE a not-yet-migrated clone site (reason == None). When the
// census hits zero the fallback is pure belt-and-braces: every clone Call
// already carries `Some(reason)`.
//
// SCOPE BOUNDARY: this walks `Instruction::Call`, so it sees Call-shaped
// clones (the migrated warn sites + explicit `.clone()` dispatched as a
// call). It does NOT see clones born as `Assign{mode:Clone}` /
// `IndexLoad{read:Clone}` — those become clone CALLS only at LIR and
// self-classify via their typed `mode`, so they are OUT of this GIR
// foundation invariant by design (the planner decides whether var-copy
// materializations need the boundary WHY). It also cannot see closure
// clones emitted as `Instruction::CallExtern` (`gorget_closure_clone_to_owned`).

/// Per-module clone-reason census: how many clone Calls are tagged with a
/// real reason vs still unclassified (`reason.is_none()`, identified via
/// the typed clone-fn set).
#[derive(Debug, Clone, Default)]
pub struct CloneReasonCensus {
    /// Clone Calls carrying `Some(reason)` where reason != NeedsClassification.
    pub tagged: usize,
    /// Clone Calls carrying `Some(NeedsClassification)` (explicitly deferred).
    pub needs_classification: usize,
    /// Clone Calls with `reason.is_none()` whose callee IS in the typed
    /// clone-fn set — the real burn-down set.
    pub untagged: usize,
    /// Per-reason-display tagged breakdown.
    pub by_reason: rustc_hash::FxHashMap<String, usize>,
    /// (function, block, inst_index, callee) for each untagged clone Call.
    pub untagged_sites: Vec<(String, usize, usize, String)>,
}

impl CloneReasonCensus {
    pub fn total_clones(&self) -> usize {
        self.tagged + self.needs_classification + self.untagged
    }
}

/// Walk the module and census every clone-emitting `Instruction::Call`.
/// Env-gated by the caller (`GG_VALIDATE_CLONE_REASONS`).
pub fn validate_clone_reasons(module: &Module) -> CloneReasonCensus {
    let clone_fns = module.type_registry.clone_fn_names_set();
    let mut census = CloneReasonCensus::default();
    for func in &module.functions {
        for (b, bb) in func.blocks.iter().enumerate() {
            for (i, inst) in bb.instructions.iter().enumerate() {
                if let Instruction::Call { func: callee, reason, .. } = inst {
                    let is_clone_callee = clone_fns.contains(callee);
                    match reason {
                        Some(crate::ir::ImplicitCloneReason::NeedsClassification) => {
                            census.needs_classification += 1;
                        }
                        Some(r) => {
                            census.tagged += 1;
                            *census.by_reason.entry(r.to_string()).or_insert(0) += 1;
                        }
                        None if is_clone_callee => {
                            census.untagged += 1;
                            census.untagged_sites.push((
                                func.name.clone(), b, i, callee.clone(),
                            ));
                        }
                        None => {} // ordinary non-clone call — ignore
                    }
                }
            }
        }
    }
    census
}

/// Walker: identifies every consume site and routes through
/// [`validate_consume`]. The walker is shape-driven on Instruction
/// variants; ABI-based dispatch reads the typed `module.fn_param_abis`
/// (internal calls) and `extern_decl.param_abis` (extern calls).
///
/// `clone_fns` is the module-wide set of recognised clone fn names
/// (built once via [`TypeRegistry::clone_fn_names_set`]); threaded
/// through so [`preceded_by_clone`] can match producers without
/// inspecting the callee string.
///
/// `liveness_mz_blind` is the [`MoveZeroPolicy::Blind`] companion run. It
/// is consulted for exactly one class — `StagingMoveIntoOwnedSlot`, in the
/// `Assign` arm — and only after `liveness` has already judged the site
/// sound. See that variant's doc for why the class is invisible to the
/// normal instrument.
fn for_each_consume_site<F: FnMut(ConsumeSiteWarning)>(
    func: &Function,
    module: &Module,
    liveness: &Liveness,
    liveness_mz_blind: &Liveness,
    clone_fns: &rustc_hash::FxHashSet<String>,
    mut emit: F,
) {
    let registry = &module.type_registry;
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, inst) in bb.instructions.iter().enumerate() {
            match inst {
                Instruction::StructInit { type_name, fields, .. } => {
                    // Closure-env structs (`__Closure_N`) use lifetime-tied aliasing
                    // for captured locals: the closure env is always freed before the
                    // outer scope, so the outer scope's drops handle cleanup. These
                    // captures are intentional bitwise aliases — not ownership violations.
                    // Read `is_closure_env` from TypeDef metadata (set at registration,
                    // no name matching). See closures.rs and types.rs TypeMetadata.
                    let is_closure = registry.get_type_def(type_name)
                        .map(|td| td.metadata.is_closure_env)
                        .unwrap_or(false);
                    if is_closure { continue; }
                    for (idx, op) in fields.iter().enumerate() {
                        let class = ConsumeSiteClass::StructInit {
                            type_name: type_name.clone(),
                            arg_index: idx,
                        };
                        if let Some(w) = validate_consume(
                            func, registry, liveness, clone_fns, op, &bb.instructions, b, i,
                            class
                        ) {
                            emit(w);
                        }
                    }
                }
                Instruction::EnumInit { variant, fields, .. } => {
                    for (idx, op) in fields.iter().enumerate() {
                        let class = ConsumeSiteClass::EnumInit {
                            variant: variant.clone(),
                            arg_index: idx,
                        };
                        if let Some(w) = validate_consume(
                            func, registry, liveness, clone_fns, op, &bb.instructions, b, i,
                            class
                        ) {
                            emit(w);
                        }
                    }
                }
                Instruction::TupleInit { elements, .. } => {
                    // Tuples are anonymous structs at the GIR level — same
                    // consume semantics. We bin them under StructInit with
                    // a synthetic type name so the cluster table stays
                    // readable.
                    for (idx, op) in elements.iter().enumerate() {
                        let class = ConsumeSiteClass::StructInit {
                            type_name: "<tuple>".into(),
                            arg_index: idx,
                        };
                        if let Some(w) = validate_consume(
                            func, registry, liveness, clone_fns, op, &bb.instructions, b, i,
                            class
                        ) {
                            emit(w);
                        }
                    }
                }
                Instruction::Call { func: callee, args, .. } => {
                    use crate::ir::lowering::context::ParamABI;
                    let abis = module.fn_param_abis.get(callee);
                    let is_runtime_collection = is_consume_extern(module, callee);
                    for (idx, op) in args.iter().enumerate() {
                        // For internal calls: ByValue is a consume position.
                        // ByPtr / ByMutPtr are borrow shapes — the callee can't
                        // take ownership through a Ptr without an explicit
                        // clone, which the lowering already inserts elsewhere.
                        let consumes = match abis {
                            Some(av) => matches!(
                                av.get(idx).copied().unwrap_or(ParamABI::ByValue),
                                ParamABI::ByValue
                            ),
                            None => false, // unknown ABI — skip
                        };
                        if !consumes && !is_runtime_collection { continue; }
                        let class = if is_runtime_collection {
                            ConsumeSiteClass::CollectionMutator {
                                callee: callee.clone(),
                                arg_index: idx,
                            }
                        } else {
                            ConsumeSiteClass::CallByValueArg {
                                callee: callee.clone(),
                                arg_index: idx,
                            }
                        };
                        if let Some(w) = validate_consume(
                            func, registry, liveness, clone_fns, op, &bb.instructions, b, i,
                            class
                        ) {
                            emit(w);
                        }
                    }
                }
                Instruction::CallExtern { func: callee, args, .. } => {
                    use crate::ir::abi::AbiKind;
                    let extern_decl = module.find_extern(callee);
                    let is_runtime_collection = is_consume_extern(module, callee);
                    for (idx, op) in args.iter().enumerate() {
                        // Externs: ByValue/GorgetString consume the value;
                        // VoidElem also consumes (the callee writes the data
                        // into its slot — the source must own the data going
                        // in or pass a fresh clone). Ptr/CStr/BytePtr/Opaque/
                        // Scalar/Auto are borrow shapes for our purposes.
                        let abi = extern_decl
                            .and_then(|d| d.param_abis.get(idx).copied())
                            .unwrap_or(AbiKind::Auto);
                        let consumes = matches!(
                            abi,
                            AbiKind::ByValue | AbiKind::GorgetString | AbiKind::VoidElem
                        );
                        if !consumes && !is_runtime_collection { continue; }
                        let class = if is_runtime_collection {
                            ConsumeSiteClass::CollectionMutator {
                                callee: callee.clone(),
                                arg_index: idx,
                            }
                        } else {
                            ConsumeSiteClass::CallExternByValueArg {
                                callee: callee.clone(),
                                arg_index: idx,
                            }
                        };
                        if let Some(w) = validate_consume(
                            func, registry, liveness, clone_fns, op, &bb.instructions, b, i,
                            class
                        ) {
                            emit(w);
                        }
                    }
                }
                Instruction::Assign { mode, dst, value } => {
                    // Deref-store (`*box = value` / `*ptr = value`) lowers to a
                    // single `[Deref]`-projection dst. It writes a value INTO an
                    // owned pointee, so it is a consume site exactly like the
                    // whole-local `AssignIntoOwnedSlot` shape — but the bail
                    // below (`projections.is_empty`) would skip it, leaving the
                    // deref-store missing-clone UAF unguarded. Resolve the
                    // pointee type INLINE (the validator only holds
                    // `registry: &TypeRegistry`, not a `LoweringContext`, so it
                    // cannot call `deref_inner_type`; and `Box[T]` locals are
                    // `Named("Box__T")`, NOT `Ptr`, so the Ptr-only Deref
                    // resolvers above return None for a Box). Gate on the same
                    // `is_resource_type(pointee)` predicate the lowering uses
                    // (leg 1) so the two layers agree: enum-payload pointees
                    // (`Box[Option[String]]`) stay skipped on BOTH legs —
                    // `is_resource_type` doesn't descend enum variants (a
                    // deferred gap, recorded in TODO.md). Reuse the `BoxNew`
                    // class — this is its first real construction site, closing
                    // the dead-class gap.
                    if dst.projections.len() == 1
                        && matches!(dst.projections[0], Projection::Deref)
                        && !matches!(mode, ReadMode::Borrow)
                    {
                        let dst_idx = dst.local.0 as usize;
                        if dst_idx >= func.locals.len() { continue; }
                        let box_ty = func.locals[dst_idx].type_id;
                        // Resolve pointee: Box__T via the typed `is_box` flag +
                        // its single `_0` field (mirrors
                        // `LoweringContext::deref_inner_type`'s Box body), else
                        // the Ptr/MutPtr pointee for a true-pointer dst.
                        let pointee = if registry.is_box(box_ty) {
                            registry.get(box_ty).and_then(|t| {
                                if let GirType::Named(name) = t {
                                    registry.get_type_def(name).and_then(|td| {
                                        if let TypeDefKind::Struct(ref s) = td.kind {
                                            s.fields.first()
                                                .filter(|f| f.name == "_0")
                                                .map(|f| f.type_id)
                                        } else { None }
                                    })
                                } else { None }
                            })
                        } else {
                            match registry.get(box_ty) {
                                Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => {
                                    Some(*inner)
                                }
                                _ => None,
                            }
                        };
                        if let Some(pointee) = pointee {
                            if registry.is_resource_type(pointee) {
                                let class = ConsumeSiteClass::BoxNew;
                                if let Some(w) = validate_assign_consume(
                                    func, registry, liveness, clone_fns, value,
                                    &bb.instructions, b, i, class,
                                ) {
                                    emit(w);
                                }
                            }
                        }
                        continue;
                    }
                    // Whole-local assigns only — projections (FieldStore-
                    // like) are out of scope for this class.
                    if !dst.projections.is_empty() { continue; }
                    // Borrow-mode assigns are the "this is an alias" contract,
                    // not a consume. The dst is structurally aliased to the
                    // source's heap data — drop accountant is supposed to
                    // mark dst as Borrowed (set_ref / similar). Validator
                    // skipping Borrow-mode here mirrors `validate_consume`'s
                    // implicit assumption that Borrow assigns are non-
                    // consuming. Worked example: `match src_col:` staging
                    // emits `[Bw] scrut = copy src_col` when src_col is
                    // owned + live (subsequent `tag_of` reads source again).
                    if matches!(mode, ReadMode::Borrow) { continue; }
                    let dst_idx = dst.local.0 as usize;
                    if dst_idx >= func.locals.len() { continue; }
                    let dst_local = &func.locals[dst_idx];
                    // Dst must be resource-typed (the slot owns heap data
                    // that scope-exit drops will free). Non-resource dst
                    // is trivially copyable.
                    if !registry.needs_drop(dst_local.type_id) { continue; }
                    // Dst must be an OWNED-required slot — i.e. one whose
                    // ownership is `Owned` or `FreshOwned` post-assign,
                    // meaning the lowering committed to drop-tracking it.
                    // Snag #28's shape: a named result slot with `Owned`
                    // ownership receiving a borrow source via an auto-
                    // deref-and-memcpy. Excludes:
                    //   * `Borrowed | View | SharedHeap`: alias slots
                    //     (shape-preserving).
                    //   * `Untracked`: transient temps (e.g. printf arg
                    //     scratch, IndexLoad results) not drop-tracked
                    //     at scope exit. The structural shape is the
                    //     same as Owned, but runtime safety is preserved
                    //     by the lack of drop registration. Flagging
                    //     these is noise — they're not a CoW soundness
                    //     issue today (though they're brittle: any
                    //     downstream pass that promotes them to Owned
                    //     would re-introduce the bug).
                    //   * `MaybeOwned`: handled by conditional-drop
                    //     discipline elsewhere; flagging Assign into it
                    //     would duplicate that.
                    use LocalOwnership::*;
                    // `_0` is the FUNCTION RETURN PLACE (a structural IR
                    // invariant of `FunctionBuilder::new`). It is minted
                    // `Untracked` and therefore invisible to the
                    // `Owned | FreshOwned` gate below — which is precisely why
                    // the whole return-borrow double-free family walked past
                    // this always-fatal validator. The caller WILL drop the
                    // returned value, so a store into `_0` is an owned-required
                    // consume site exactly like any other. It is reported under
                    // its own class so the caller can keep it NON-FATAL during
                    // burn-down (see `AssignIntoReturnSlot`).
                    let is_return_place = dst.local == LocalId(0);
                    if !is_return_place && !matches!(dst_local.ownership, Owned | FreshOwned) {
                        continue;
                    }
                    // A `_0` explicitly tagged as a borrow is the Ptr-propagation
                    // return (`set_ref(LocalId(0))` on the no-clone-fn leg): the
                    // caller receives a borrow by contract, so it is not a
                    // consume.
                    if is_return_place && dst_local.ownership.is_ref() {
                        continue;
                    }
                    // Trivial-copy types (Shared/Weak/Channel/Guard) are
                    // bitwise-copyable at the GIR level — same skip as
                    // validate_consume.
                    if let Some(GirType::Named(name)) = registry.get(dst_local.type_id) {
                        if let Some(td) = registry.get_type_def(name) {
                            if td.metadata.copy_semantics == CopySemantics::Trivial {
                                continue;
                            }
                            // Closure-env slots use lifetime-tied aliasing
                            // (see StructInit case above for rationale).
                            if td.metadata.is_closure_env {
                                continue;
                            }
                        }
                    }
                    let dst_type = registry.type_name(dst_local.type_id)
                        .unwrap_or_else(|| format!("ty{}", dst_local.type_id.0));
                    let class = if is_return_place {
                        ConsumeSiteClass::AssignIntoReturnSlot { dst_type: dst_type.clone() }
                    } else {
                        ConsumeSiteClass::AssignIntoOwnedSlot { dst_type: dst_type.clone() }
                    };
                    if let Some(w) = validate_assign_consume(
                        func, registry, liveness, clone_fns, value, &bb.instructions, b, i,
                        class,
                    ) {
                        emit(w);
                    } else if let Some(w) = validate_assign_consume(
                        func, registry, liveness_mz_blind, clone_fns, value,
                        &bb.instructions, b, i,
                        ConsumeSiteClass::StagingMoveIntoOwnedSlot { dst_type },
                    ) {
                        // Sound under the normal rule, unsound once the
                        // site's own `MoveZero` stops vouching for it —
                        // the staging-move class. Its runway is the
                        // sibling `GG_STAGING_MOVE_GUARD` promoter in
                        // `lowering/mod.rs`.
                        emit(w);
                    }
                }
                _ => {}
            }
        }
    }
}

/// Sibling of [`validate_consume`] specialised for the
/// `AssignIntoOwnedSlot` class (Snag #28). The two helpers differ on
/// **which side decides resource-ness**:
///
/// * [`validate_consume`] gates on the SOURCE type — needs_drop(source).
///   Correct for call args / inits where the consumer takes ownership of
///   the value passed in: if the source can't be dropped, the consume is
///   trivially sound regardless of source ownership.
/// * `validate_assign_consume` gates on the DST type at the caller.
///   Correct for plain Assigns where the codegen may auto-deref a
///   Ptr<T> source into a T dst (a memcpy of the pointee struct). The
///   source's TYPE may be Ptr<T> (non-droppable) but its OWNERSHIP is
///   Borrowed/View — the existing source-gated check would skip it.
///
/// Source-side ownership rules are identical to [`validate_consume`]:
/// Borrowed/View → must be preceded by clone; Owned-live → invalid;
/// Untracked → invalid (lowering didn't decide).
fn validate_assign_consume(
    func: &Function,
    registry: &TypeRegistry,
    liveness: &Liveness,
    clone_fns: &rustc_hash::FxHashSet<String>,
    operand: &Operand,
    insts: &[Instruction],
    block: usize,
    inst_index: usize,
    class: ConsumeSiteClass,
) -> Option<ConsumeSiteWarning> {
    let place = match operand {
        Operand::Copy(p) | Operand::Move(p) => p,
        Operand::Constant(_) => return None,
    };
    if !place.projections.is_empty() { return None; }
    let local_idx = place.local.0 as usize;
    if local_idx >= func.locals.len() { return None; }
    let local = &func.locals[local_idx];

    // Skip Trivial-copy source types — same rationale as validate_consume.
    if let Some(GirType::Named(name)) = registry.get(local.type_id) {
        if let Some(td) = registry.get_type_def(name) {
            if td.metadata.copy_semantics == CopySemantics::Trivial {
                return None;
            }
        }
    }

    let live_after = liveness.is_live_after(place.local, BlockId(block as u32), inst_index);
    let is_move = matches!(operand, Operand::Move(_));

    use LocalOwnership::*;
    let violation = match (&local.ownership, live_after, is_move) {
        // VALID — Owned + dead source, classic transfer.
        (Owned | FreshOwned | SharedHeap { .. }, false, _) => return None,

        // INVALID — Owned but live: source still in use after assign.
        (Owned, true, _) => ConsumeSiteViolation::OwnedLiveSourceConsumed,
        (FreshOwned, true, _) => ConsumeSiteViolation::OwnedLiveSourceConsumed,
        (SharedHeap { .. }, true, _) => ConsumeSiteViolation::OwnedLiveSourceConsumed,

        (Borrowed { .. } | View { .. }, _, _) => {
            if preceded_by_clone(insts, inst_index, place.local, clone_fns) {
                return None;
            }
            ConsumeSiteViolation::BorrowedSourceConsumed
        }
        (MaybeOwned, _, _) => {
            if preceded_by_clone(insts, inst_index, place.local, clone_fns) {
                return None;
            }
            ConsumeSiteViolation::MaybeOwnedSourceConsumed
        }
        (Untracked, _, _) => {
            if preceded_by_clone(insts, inst_index, place.local, clone_fns) {
                return None;
            }
            ConsumeSiteViolation::UntrackedSourceConsumed
        }
    };

    let source_type_name = registry.type_name(local.type_id)
        .unwrap_or_else(|| format!("ty{}", local.type_id.0));
    Some(ConsumeSiteWarning {
        function: func.name.clone(),
        block: BlockId(block as u32),
        inst_index,
        class,
        source_local: place.local,
        source_type_name,
        violation,
    })
}

/// The unified consume-site rule. Returns `Some(warning)` when the
/// source operand's `(ownership, live_after, is_move)` tuple violates
/// the Tier 2a rule; `None` when the site is sound or out of scope.
///
/// The rule (matching the brief's table):
///
/// ```text
/// Source state                       | Required IR shape
/// -----------------------------------|----------------------------
/// Owned AND dead at this call        | Move(p) + MoveZero(p)
/// Borrow OR owned-but-live           | Clone-then-Move
/// Static literal                     | Runtime *_materialize
/// ```
///
/// Implementation detail: a "preceded by clone" source — the
/// `_temp = call clone_fn(orig); consume(_temp)` shape — is recognised
/// by checking that the source local is `FreshOwned` AND was defined
/// by a `Call` to a clone fn (or a fresh-allocating runtime fn). Such
/// temps are dead-after-consume by construction (`live_after` returns
/// false), so the `(FreshOwned, false, _)` tuple is sound regardless
/// of whether the consumer used Copy or Move.
fn validate_consume(
    func: &Function,
    registry: &TypeRegistry,
    liveness: &Liveness,
    clone_fns: &rustc_hash::FxHashSet<String>,
    operand: &Operand,
    insts: &[Instruction],
    block: usize,
    inst_index: usize,
    class: ConsumeSiteClass,
) -> Option<ConsumeSiteWarning> {
    // Constants are sound (they materialise via runtime helpers per the rule).
    let place = match operand {
        Operand::Copy(p) | Operand::Move(p) => p,
        Operand::Constant(_) => return None,
    };
    // Whole-local consumes only — projections (field/index extractions)
    // are handled by the Phase C read-site validators.
    if !place.projections.is_empty() { return None; }
    let local_idx = place.local.0 as usize;
    if local_idx >= func.locals.len() { return None; }
    let local = &func.locals[local_idx];

    // Skip non-resource args — trivially copyable.
    if !registry.needs_drop(local.type_id) { return None; }
    // Skip CopySemantics::Trivial types (Shared, Weak, Channel, Guard …).
    // These are bitwise-copyable at the GIR level — the runtime handles
    // refcount management via explicit drop calls, so a Copy of a live
    // Shared/Channel at a consume site is NOT an ownership violation.
    // Only CopySemantics::Resource types (GorgetString, GorgetArray, etc.)
    // require true move semantics at ownership boundaries.
    if let Some(GirType::Named(name)) = registry.get(local.type_id) {
        if let Some(td) = registry.get_type_def(name) {
            if td.metadata.copy_semantics == CopySemantics::Trivial {
                return None;
            }
        }
    }

    let live_after = liveness.is_live_after(place.local, BlockId(block as u32), inst_index);
    let is_move = matches!(operand, Operand::Move(_));

    use LocalOwnership::*;
    let violation = match (&local.ownership, live_after, is_move) {
        // VALID cases — no warning.
        // Owned + dead + Move: classic transfer of ownership.
        (Owned | FreshOwned | SharedHeap { .. }, false, true) => return None,
        // Owned + dead + Copy: backend treats Copy as Move when source
        // is dead and ownership is concrete. Fresh temps from clones
        // land here. Sound.
        (FreshOwned | Owned | SharedHeap { .. }, false, false) => return None,

        // INVALID cases.
        (Owned, true, _) => ConsumeSiteViolation::OwnedLiveSourceConsumed,
        // FreshOwned + live: the temp IS shared with someone? Treat as
        // owned-live for diagnostic purposes — same migration shape.
        (FreshOwned, true, _) => ConsumeSiteViolation::OwnedLiveSourceConsumed,
        (SharedHeap { .. }, true, _) => ConsumeSiteViolation::OwnedLiveSourceConsumed,

        (Borrowed { .. } | View { .. }, _, _) => {
            // Special-case: a freshly cloned temp at a consume site
            // shows up here when the source is the cloned destination
            // local (FreshOwned) but the lowering forgot to set the
            // ownership. Also: function params with `&` sigil get
            // `Borrowed { Param(self), .. }` and the lowering
            // intentionally consumes them (after a clone) — recognise
            // the "preceded by clone" shape to avoid double-counting.
            if preceded_by_clone(insts, inst_index, place.local, clone_fns) {
                return None;
            }
            ConsumeSiteViolation::BorrowedSourceConsumed
        }
        (MaybeOwned, _, _) => {
            if preceded_by_clone(insts, inst_index, place.local, clone_fns) {
                return None;
            }
            ConsumeSiteViolation::MaybeOwnedSourceConsumed
        }
        (Untracked, _, _) => {
            if preceded_by_clone(insts, inst_index, place.local, clone_fns) {
                return None;
            }
            ConsumeSiteViolation::UntrackedSourceConsumed
        }
    };
    let _ = BorrowOrigin::Param(LocalId(0));  // suppress unused-import warnings on BorrowOrigin

    let source_type_name = registry.type_name(local.type_id)
        .unwrap_or_else(|| format!("ty{}", local.type_id.0));
    Some(ConsumeSiteWarning {
        function: func.name.clone(),
        block: BlockId(block as u32),
        inst_index,
        class,
        source_local: place.local,
        source_type_name,
        violation,
    })
}

/// Did some instruction earlier in this block produce `local` via a
/// call that looks like a clone / fresh-allocation? This is the
/// "preceded by clone" recognition described in the Phase 1 brief:
///
///     _temp = call clone_fn(_orig)   // some earlier instruction
///     consume(_temp)                  // this consume site
///
/// The clone produces a fresh allocation; `_temp` doesn't alias `_orig`
/// at the heap level, so the consume is sound regardless of `_orig`'s
/// ownership / liveness. Recognition is shape-driven:
///
/// 1. Find the most recent instruction in this block that defines `local`.
/// 2. Check if that instruction is a `Call` / `CallExtern` whose callee
///    is recognised as a clone or fresh-allocator via [`is_clone_or_fresh_call`].
///
/// **Phase 2E (2026-05-07):** the recognition is fully typed — runtime
/// fns route through `RuntimeFn::from_c_name(...).signature().returns_fresh`
/// (single source of truth for "this runtime call returns an
/// independent heap buffer"); user-defined `T__clone` stubs are matched
/// via the module's `clone_fns` set built once at the validator entry by
/// [`TypeRegistry::clone_fn_names_set`]. No string-suffix or
/// runtime-symbol pattern matching survives in this predicate. The two
/// tables (`RuntimeSig.returns_fresh` and `TypeMetadata.clone_fn`) are
/// the load-bearing facts; we read them, never re-derive from names.
fn preceded_by_clone(
    insts: &[Instruction],
    inst_index: usize,
    local: LocalId,
    clone_fns: &rustc_hash::FxHashSet<String>,
) -> bool {
    // Walk backward from inst_index-1 looking for the most recent def of `local`.
    for k in (0..inst_index).rev() {
        let inst = &insts[k];
        let writes_local = match inst {
            Instruction::Assign { dst, .. } if dst.projections.is_empty() => Some(dst.local),
            Instruction::Call { dst: Some(d), .. }
            | Instruction::CallExtern { dst: Some(d), .. }
            | Instruction::CallIndirect { dst: Some(d), .. } => Some(*d),
            Instruction::BinOp { dst, .. }
            | Instruction::FaultableBinOp { dst, .. }
            | Instruction::UnOp { dst, .. }
            | Instruction::Cmp { dst, .. }
            | Instruction::Cast { dst, .. }
            | Instruction::BitCast { dst, .. }
            | Instruction::PtrCast { dst, .. }
            | Instruction::FieldLoad { dst, .. }
            | Instruction::IndexLoad { dst, .. }
            | Instruction::EnumFieldLoad { dst, .. }
            | Instruction::HeapAlloc { dst, .. }
            | Instruction::HeapAllocArray { dst, .. }
            | Instruction::StructInit { dst, .. }
            | Instruction::EnumInit { dst, .. }
            | Instruction::TupleInit { dst, .. }
            | Instruction::TagOf { dst, .. }
            | Instruction::Borrow { dst, .. }
            | Instruction::BorrowMut { dst, .. }
            | Instruction::LoadThreadLocal { dst, .. }
            | Instruction::LoadRef { dst, .. } => Some(*dst),
            _ => None,
        };
        if let Some(w) = writes_local {
            if w == local {
                // Found the producer. Now check shape.
                return is_clone_or_fresh_call(inst, clone_fns);
            }
        }
    }
    false
}

/// Does this instruction's callee return a fresh heap allocation OR is
/// it a known clone fn? Typed-only:
///
/// * Runtime fns: `RuntimeFn::from_c_name(name).signature().returns_fresh`
///   reads the const-buildable [`crate::lir::runtime::RuntimeSig`] table
///   — the single source of truth for which runtime calls produce
///   independent heap buffers (no aliasing into inputs). Replaces the
///   prior name-list of `gorget_*_clone`, `gorget_str_cat`, etc.
/// * User-defined / builtin clone fns: membership in the
///   pre-computed `clone_fns` set (built from
///   `TypeRegistry::clone_fn_names_set`). Each entry is the typed
///   `clone_fn_name_for_def(td)` value — the same name
///   `LoweringContext::clone_fn_for_ptr` would emit. No `__clone`
///   suffix recognition.
///
/// Per CLAUDE.md "No name matching": both branches read typed metadata
/// at the source of truth (RuntimeSig / TypeMetadata) and answer
/// without inspecting the callee identifier shape. The metadata is
/// load-bearing; the names are not.
fn is_clone_or_fresh_call(
    inst: &Instruction,
    clone_fns: &rustc_hash::FxHashSet<String>,
) -> bool {
    let name = match inst {
        Instruction::Call { func, .. } => func.as_str(),
        Instruction::CallExtern { func, .. } => func.as_str(),
        _ => return false,
    };
    // Runtime fresh: typed `RuntimeSig.returns_fresh` lookup.
    if let Some(rt) = crate::lir::runtime::RuntimeFn::from_c_name(name) {
        if rt.signature().returns_fresh {
            return true;
        }
    }
    // User-defined / collection / builtin clone fns: typed
    // `TypeMetadata.clone_fn` membership (or generated `T__clone` for
    // user structs / cloneable enums — same resolver as
    // `LoweringContext::clone_fn_for_ptr`).
    clone_fns.contains(name)
}

/// Is this callee one of the runtime collection mutators where the
/// element / value arg is consumed? Used to bin call-arg consume sites
/// under [`ConsumeSiteClass::CollectionMutator`] for clearer Phase 2
/// migration planning. Same name-match exception as [`preceded_by_clone`].
fn is_runtime_collection_mutator(name: &str) -> bool {
    matches!(name,
        "gorget_array_push" | "gorget_array_insert" | "gorget_array_set"
        | "gorget_map_put" | "gorget_map_set" | "gorget_map_insert"
        | "gorget_set_insert" | "gorget_set_add"
        | "gorget_channel_send" | "gorget_channel_send_blocking"
        | "gorget_array_extend" | "gorget_map_extend"
    )
}

/// Recognise consume-shape extern calls. Combines:
///   (1) `module.consume_externs` — typed registry populated at writer-
///       site registration (e.g., `lower_dict_literal` inserts the
///       mangled `Dict__K__V__put`) AND derived at module finalization
///       from `fn_param_ownerships` (any fn with `Ownership::Move`
///       params). Catches mangled mono names like `Dict__K__V__put`
///       that the runtime allowlist misses (post-mono runtime symbol
///       vs IR-stage mangled name; see `Module::consume_externs`).
///   (2) `is_runtime_collection_mutator` — historical name allowlist
///       for direct runtime-symbol emissions that bypass the lowering's
///       mangled-name path. Kept as fallback so the registry doesn't
///       need to enumerate every direct runtime emission site.
///
/// Promoted to the live classifier 2026-05-12 after the var-decl
/// closure auto-clone branch landed in `lower_var_decl` (the burn-down
/// of the latent 27-violation class). The branch keys off the
/// destination's `gir_type` (the user-declared `Callable[...]`) — the
/// source's `inferred` is unreliable because Callable params resolve
/// to `UNIT_TYPE` at the immutable `map_ast_type` path (intentional
/// design for the void* `__callable_N` ABI).
fn is_consume_extern(module: &Module, name: &str) -> bool {
    module.consume_externs.contains(name) || is_runtime_collection_mutator(name)
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
            returns_borrowed: false,
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
                deref_of_owning_param: None,
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
                deref_of_owning_param: None,
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
            locals: vec![Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None }],
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
            locals: vec![Local { type_id: I64_TYPE, name_hint: None, ownership: crate::ir::LocalOwnership::default(), slot_kind: crate::ir::SlotKind::default(), is_owning_param: false, deref_of_owning_param: None }],
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

    // ── Tier 2c: drop-tracking pre-rebind validator tests ─────────────
    //
    // Three shapes covered:
    //   1. Empty `heap_alloc_consumer_externs` set → no work, no warnings.
    //   2. Box.new alloc fn IS in the set, source IS drop-registered, no
    //      MoveZero between the call and a subsequent Drop → violation.
    //   3. Same shape but with an intervening MoveZero(p) → no violation.

    fn make_box_alloc_module(
        register_callee: bool,
        emit_move_zero: bool,
    ) -> Module {
        let mut module = Module::new();
        if register_callee {
            module.heap_alloc_consumer_externs.insert("__gorget_box_alloc_int64_t".to_string());
        }
        let mut b = FunctionBuilder::new("test_fn", I64_TYPE, &[]);
        // _0: source value (drop-registered via the trailing Drop).
        let src = b.add_local(I64_TYPE, None);
        // Result of box_alloc — type doesn't matter for the validator.
        b.assign(Place::local(src), FunctionBuilder::const_i64(42));
        let _dst = b.call_extern(
            "__gorget_box_alloc_int64_t",
            vec![FunctionBuilder::copy(src)],
            I64_TYPE,
        );
        if emit_move_zero {
            b.move_zero(Place::local(src));
        }
        b.drop(Place::local(src));
        b.ret(FunctionBuilder::const_i64(0));
        module.functions.push(b.build());
        module
    }

    #[test]
    fn drop_pre_rebind_empty_set_no_op() {
        // No box-alloc fn registered; validator must early-return.
        let module = make_box_alloc_module(false, false);
        let warnings = validate_drop_pre_rebind(&module);
        assert!(warnings.is_empty(), "Expected no warnings, got: {:?}", warnings);
    }

    #[test]
    fn drop_pre_rebind_violation_detected() {
        // Box.new alloc fn registered; source drop-registered (trailing
        // `Drop`); no intervening MoveZero. Snag #23 shape.
        let module = make_box_alloc_module(true, false);
        let warnings = validate_drop_pre_rebind(&module);
        assert_eq!(warnings.len(), 1,
            "Expected exactly one warning for the snag #23 shape, got: {:?}",
            warnings);
        assert_eq!(warnings[0].callee, "__gorget_box_alloc_int64_t");
    }

    #[test]
    fn drop_pre_rebind_move_zero_between_call_and_drop_ok() {
        // Same shape but with `MoveZero(src)` between the alloc call
        // and the trailing Drop — the writer-side fix at commit
        // `4ebefe44`. Validator must NOT flag this.
        let module = make_box_alloc_module(true, true);
        let warnings = validate_drop_pre_rebind(&module);
        assert!(warnings.is_empty(),
            "MoveZero between call and Drop must clear the violation; got: {:?}",
            warnings);
    }

    /// Tier 1c: struct with `..Default::default()` metadata + a resource
    /// field gets flagged by the validator.
    #[test]
    fn tier1c_coherence_struct_with_resource_flagged() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("OwnedBuf".into()));
        module.type_registry.add_type_def(TypeDef {
            name: "Holder".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "a".into(), type_id: I64_TYPE },
                    StructField { name: "b".into(), type_id: buf_id },
                ],
            }),
            metadata: TypeMetadata::default(),
        });
        let warnings = validate_type_metadata_coherence(&module);
        assert!(
            warnings.iter().any(|w| w.type_name == "Holder"
                && w.expected_drop == DropStrategy::Recursive
                && w.actual_drop == DropStrategy::None),
            "Holder should be flagged. Warnings: {:?}", warnings
        );
    }

    /// Tier 1c: struct with coherent metadata is NOT flagged.
    #[test]
    fn tier1c_coherence_struct_coherent_not_flagged() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("OwnedBuf".into()));
        module.type_registry.add_type_def(TypeDef {
            name: "Holder".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![StructField { name: "b".into(), type_id: buf_id }],
            }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Recursive,
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let warnings = validate_type_metadata_coherence(&module);
        assert!(warnings.is_empty(), "coherent Holder should NOT be flagged. Warnings: {:?}", warnings);
    }

    /// Tier 1c: smart-pointer wrapper carve-out — single-field `_0: T`
    /// struct with explicit (Trivial, None) is NOT flagged even when T
    /// is a resource type.
    #[test]
    fn tier1c_coherence_smart_pointer_wrapper_skipped() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("OwnedBuf".into()));
        module.type_registry.add_type_def(TypeDef {
            name: "Mutex__Buf".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![StructField { name: "_0".into(), type_id: buf_id }],
            }),
            metadata: TypeMetadata::default(),
        });
        let warnings = validate_type_metadata_coherence(&module);
        assert!(
            warnings.iter().all(|w| w.type_name != "Mutex__Buf"),
            "smart-pointer wrapper should NOT be flagged. Warnings: {:?}", warnings
        );
    }

    /// Tier 1c: enum with `..Default::default()` metadata + resource
    /// variant payload gets flagged.
    #[test]
    fn tier1c_coherence_enum_with_resource_variant_flagged() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("OwnedBuf".into()));
        module.type_registry.add_type_def(TypeDef {
            name: "MaybeBuf".into(),
            kind: TypeDefKind::Enum(EnumDef {
                variants: vec![
                    EnumVariant {
                        name: "Some".into(),
                        fields: vec![StructField { name: "_0".into(), type_id: buf_id }],
                    },
                    EnumVariant { name: "None".into(), fields: vec![] },
                ],
            }),
            metadata: TypeMetadata::default(),
        });
        let warnings = validate_type_metadata_coherence(&module);
        assert!(
            warnings.iter().any(|w| w.type_name == "MaybeBuf"
                && w.expected_drop == DropStrategy::Recursive
                && w.kind == TypeMetadataCoherenceKind::Enum),
            "MaybeBuf should be flagged. Warnings: {:?}", warnings
        );
    }

    /// Tier 1c: closure-env struct carve-out — a struct tagged
    /// `is_closure_env: true` with resource fields is NOT flagged, even
    /// when the helpers would compute `(Recursive, Resource)`. The closure
    /// captures are lifetime-tied aliases; outer-scope drops handle cleanup.
    #[test]
    fn tier1c_coherence_closure_env_skipped() {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("OwnedBuf".into()));
        module.type_registry.add_type_def(TypeDef {
            name: "Closure_env_42".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![StructField { name: "captured".into(), type_id: buf_id }],
            }),
            metadata: TypeMetadata {
                is_closure_env: true,
                ..Default::default()
            },
        });
        let warnings = validate_type_metadata_coherence(&module);
        assert!(
            warnings.iter().all(|w| w.type_name != "Closure_env_42"),
            "closure-env struct should NOT be flagged. Warnings: {:?}", warnings
        );
    }

    // ── The return-place predicate (Track B1 §5) ──────────────────────
    // POSITIVE CONTROL for the retargeted guard. Before this predicate the
    // `Instruction::Assign` walker VISITED the return-slot store and then
    // dropped it on the floor, because `_0` is minted `Untracked` and the
    // `AssignIntoOwnedSlot` gate accepts only `Owned | FreshOwned`. Every
    // arm of the return-borrow double-free family therefore walked past an
    // always-fatal validator unseen. A new class alone would NOT have fixed
    // that — the gate is what dropped them.

    /// Build `fn f(*mut Buf) -> Buf { _2 = copy _1.*; _0 = copy _2; ret }`
    /// with `_2` tagged as a borrow of the param — the exact shape
    /// `return v` over a `Buf &v` lowers to.
    fn return_borrow_module(tag_source_as_borrow: bool) -> Module {
        let mut module = Module::new();
        module.type_registry.add_type_def(TypeDef {
            name: "Buf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = module.type_registry.insert(GirType::Named("Buf".into()));
        let ptr_id = module.type_registry.insert(GirType::MutPtr(buf_id));

        let mut b = FunctionBuilder::new("f", buf_id, &[(ptr_id, Some("v"))]);
        // `_1` is the `&`-param.
        b.locals[1].ownership = LocalOwnership::Borrowed {
            origin: crate::ir::BorrowOrigin::Param(LocalId(1)),
            mutability: crate::ir::Mutability::Unique,
        };
        // `_2 = copy _1.*` — the auto-deref temp.
        let tmp = b.add_local(buf_id, None);
        b.assign(
            Place::local(tmp),
            Operand::Copy(Place { local: LocalId(1), projections: vec![Projection::Deref] }),
        );
        if tag_source_as_borrow {
            b.locals[tmp.0 as usize].ownership = LocalOwnership::Borrowed {
                origin: crate::ir::BorrowOrigin::Param(LocalId(1)),
                mutability: crate::ir::Mutability::Unique,
            };
        }
        // `[Mv] _0 = copy _2` — the return-slot store.
        b.assign_mode(
            crate::ir::instructions::AssignMode::Move,
            Place::local(LocalId(0)),
            FunctionBuilder::copy(tmp),
        );
        b.ret(FunctionBuilder::copy(LocalId(0)));
        module.functions.push(b.build());
        module
    }

    #[test]
    fn return_slot_predicate_fires_on_borrowed_source() {
        let module = return_borrow_module(true);
        let warnings = validate_consume_sites(&module);
        assert!(
            warnings.iter().any(|w| matches!(
                w.class,
                ConsumeSiteClass::AssignIntoReturnSlot { .. }
            ) && w.violation == ConsumeSiteViolation::BorrowedSourceConsumed),
            "the return-place predicate must flag a borrowed source stored into `_0` \
             — this is the whole `return`-borrow double-free family. Got: {warnings:?}"
        );
    }

    #[test]
    fn return_slot_predicate_fires_on_untracked_source() {
        // The pre-(a) shape: the deref temp left `Untracked`. Also visible,
        // and it is what caught the live `equip … : row` use-after-free.
        let module = return_borrow_module(false);
        let warnings = validate_consume_sites(&module);
        assert!(
            warnings.iter().any(|w| matches!(
                w.class,
                ConsumeSiteClass::AssignIntoReturnSlot { .. }
            ) && w.violation == ConsumeSiteViolation::UntrackedSourceConsumed),
            "an Untracked resource source stored into `_0` must be flagged too. \
             Got: {warnings:?}"
        );
    }

    #[test]
    fn return_slot_predicate_silent_on_owned_dead_source() {
        // The CORRECT shape: the return-slot store consumes a fresh owned
        // local that is dead afterwards. Must NOT fire — otherwise the guard
        // is noise, not a guard.
        let mut module = return_borrow_module(false);
        let f = &mut module.functions[0];
        let tmp = f.locals.len() - 1;
        f.locals[tmp].ownership = LocalOwnership::Owned;
        let warnings = validate_consume_sites(&module);
        assert!(
            warnings.is_empty(),
            "owned-and-dead source into the return slot is the sound transfer; \
             flagging it would make the guard noise. Got: {warnings:?}"
        );
    }

    #[test]
    fn return_slot_predicate_silent_on_ptr_propagation_return() {
        // The no-clone-fn Ptr-propagation return retypes `_0` and `set_ref`s
        // it: the caller receives a BORROW by contract, so the store is not a
        // consume. Tagging `_0` `Owned` at construction (the rejected
        // alternative) would have silently broken exactly this leg.
        let mut module = return_borrow_module(true);
        let f = &mut module.functions[0];
        f.locals[0].ownership = LocalOwnership::Borrowed {
            origin: crate::ir::BorrowOrigin::Alias(LocalId(0)),
            mutability: crate::ir::Mutability::Shared,
        };
        let warnings = validate_consume_sites(&module);
        assert!(
            warnings.is_empty(),
            "a borrow-contract return slot must not be flagged. Got: {warnings:?}"
        );
    }
}
