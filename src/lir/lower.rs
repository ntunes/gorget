//! GIR → LIR lowering.
//!
//! Converts a GIR `Module` into an `LirModule` in pre-SSA form (slot-based).
//! This is incremental — each phase adds support for more GIR constructs.
//!
//! Phase 2.1: Scalars, arithmetic, comparison, control flow, basic calls.
//! Phase 2.2: Function calls (Call, CallExtern, CallIndirect).
//! Phase 2.3: Structs + field access.
//! Phase 2.4: Enums + match (Switch).
//! Phase 2.5: Type conversions.

use crate::ir;
use crate::ir::instructions::{
    BinOp as GirBinOp, CmpOp as GirCmpOp, Constant, Instruction, Operand, Place, Projection,
    Terminator, UnOp as GirUnOp,
};
use crate::ir::types::{
    self as gir_types, GirType, TypeId as GirTypeId, TypeRegistry,
};

use super::types::{StructRegistry, builtin_struct_defs};
use super::*;

/// Context for lowering a single GIR module to LIR.
pub struct LoweringContext<'a> {
    /// The GIR module being lowered.
    gir: &'a ir::Module,
    /// The LIR module being built.
    pub module: LirModule,
    /// Struct name → StructId mapping.
    struct_reg: StructRegistry,
    /// GIR function name → LIR FuncId mapping (for Call resolution).
    func_index: std::collections::HashMap<String, FuncId>,
}

/// Context for lowering a single GIR function.
struct FuncLowering<'a> {
    gir_func: &'a ir::Function,
    gir_types: &'a TypeRegistry,
    lir_func: LirFunction,
    /// GIR LocalId → LIR SlotId mapping.
    local_to_slot: Vec<SlotId>,
    /// GIR BlockId → LIR BlockId mapping.
    block_map: Vec<BlockId>,
    /// Struct registry reference (for FieldPtr).
    struct_reg: &'a StructRegistry,
    /// Function name → FuncId (for Call).
    func_index: &'a std::collections::HashMap<String, FuncId>,
    /// Module struct definitions (for field-count checking).
    module_structs: &'a [StructDef],
}

impl<'a> LoweringContext<'a> {
    pub fn new(gir: &'a ir::Module) -> Self {
        let mut module = LirModule::new();
        module.source_filename = gir.source_filename.clone();

        // Register builtin struct types.
        let mut struct_reg = StructRegistry::new();
        for def in builtin_struct_defs() {
            let id = module.add_struct(def.clone());
            struct_reg.register(&def.name, id);
        }

        Self {
            gir,
            module,
            struct_reg,
            func_index: std::collections::HashMap::new(),
        }
    }

    /// Lower the entire GIR module to LIR.
    pub fn lower(mut self) -> LirModule {
        // Pre-register all function names so Call can resolve forward references.
        for (i, func) in self.gir.functions.iter().enumerate() {
            self.func_index
                .insert(func.name.clone(), FuncId(i as u32));
        }

        // Lower struct/enum type definitions from GIR type registry.
        self.lower_type_defs();

        // Lower extern declarations.
        for ext in &self.gir.externs {
            self.module.add_extern(LirExtern {
                name: ext.name.clone(),
                params: ext.params.iter().map(|t| map_gir_type_with_structs(t, &self.gir.type_registry, Some(&self.struct_reg))).collect(),
                return_type: map_gir_type_with_structs(&ext.return_type, &self.gir.type_registry, Some(&self.struct_reg)),
                is_variadic: ext.is_variadic,
            });
        }

        // Lower globals.
        for global in &self.gir.globals {
            self.module.add_global(LirGlobal {
                name: global.name.clone(),
                ty: map_gir_type_with_structs(&global.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                init: lower_global_init(&global.init),
                is_const: false, // GIR doesn't distinguish const vs mut globals
            });
        }

        // Lower functions.
        let funcs: Vec<LirFunction> = self
            .gir
            .functions
            .iter()
            .map(|f| {
                let mut fl = FuncLowering::new(
                    f,
                    &self.gir.type_registry,
                    &self.struct_reg,
                    &self.func_index,
                    &self.module.structs,
                );
                fl.lower();
                fl.lir_func
            })
            .collect();

        for func in funcs {
            self.module.add_function(func);
        }

        self.module
    }

    fn lower_type_defs(&mut self) {
        for def in self.gir.type_registry.type_defs() {
            if self.struct_reg.lookup(&def.name).is_some() {
                continue; // Already registered (builtin).
            }

            // Map collection instantiations to their runtime struct.
            // e.g., Vector__Str → GorgetArray, Dict__Str__int64_t → GorgetMap
            if let Some(runtime_name) = collection_runtime_type(&def.name) {
                if let Some(runtime_sid) = self.struct_reg.lookup(runtime_name) {
                    self.struct_reg.register(&def.name, runtime_sid);
                    continue;
                }
            }

            match &def.kind {
                gir_types::TypeDefKind::Struct(sdef) => {
                    let fields: Vec<(String, LirType)> = sdef
                        .fields
                        .iter()
                        .map(|f| {
                            (
                                f.name.clone(),
                                map_gir_type_with_structs(&f.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                            )
                        })
                        .collect();
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields,
                    });
                    self.struct_reg.register(&def.name, sid);
                }
                gir_types::TypeDefKind::Enum(edef) => {
                    // Enums become a struct with a tag field + variant fields.
                    let mut fields: Vec<(String, LirType)> = vec![("tag".into(), LirType::I32)];
                    for variant in &edef.variants {
                        for (i, f) in variant.fields.iter().enumerate() {
                            fields.push((
                                format!("{}_{}", variant.name, i),
                                map_gir_type_with_structs(&f.type_id, &self.gir.type_registry, Some(&self.struct_reg)),
                            ));
                        }
                    }
                    let sid = self.module.add_struct(StructDef {
                        name: def.name.clone(),
                        fields,
                    });
                    self.struct_reg.register(&def.name, sid);
                }
                gir_types::TypeDefKind::Alias(_) => {
                    // Type aliases are transparent — no LIR struct needed.
                }
            }
        }
    }
}

/// Map generic collection type names to their runtime struct name.
/// Returns None for non-collection types.
fn collection_runtime_type(name: &str) -> Option<&'static str> {
    if name.starts_with("Vector__") || name.starts_with("Shared__Vector__") {
        return Some("GorgetArray");
    }
    if name.starts_with("Dict__") || name.starts_with("GorgetDict__") {
        return Some("GorgetMap");
    }
    if name.starts_with("HashMap__") || name.starts_with("GorgetMap__") {
        return Some("GorgetMap");
    }
    if name.starts_with("Set__") || name.starts_with("GorgetSet__") {
        return Some("GorgetSet");
    }
    if name.starts_with("Result__") || name.starts_with("Option__") {
        // Result/Option are real structs with fields — don't alias.
        return None;
    }
    None
}

impl<'a> FuncLowering<'a> {
    fn new(
        gir_func: &'a ir::Function,
        gir_types: &'a TypeRegistry,
        struct_reg: &'a StructRegistry,
        func_index: &'a std::collections::HashMap<String, FuncId>,
        module_structs: &'a [StructDef],
    ) -> Self {
        let params: Vec<LirType> = gir_func
            .params
            .iter()
            .map(|t| map_gir_type_with_structs(t, gir_types, Some(struct_reg)))
            .collect();
        let return_type = map_gir_type_with_structs(&gir_func.return_type, gir_types, Some(struct_reg));
        let mut lir_func = LirFunction::new(gir_func.name.clone(), params, return_type);

        // Create LIR slots for each GIR local.
        let local_to_slot: Vec<SlotId> = gir_func
            .locals
            .iter()
            .map(|local| {
                let ty = map_gir_type_with_structs(&local.type_id, gir_types, Some(struct_reg));
                lir_func.add_slot(ty, local.name_hint.clone())
            })
            .collect();

        // Pre-create LIR blocks for each GIR block.
        let block_map: Vec<BlockId> = (0..gir_func.blocks.len())
            .map(|_| lir_func.add_block())
            .collect();

        Self {
            gir_func,
            gir_types,
            lir_func,
            local_to_slot,
            block_map,
            struct_reg,
            func_index,
            module_structs,
        }
    }

    fn lower(&mut self) {
        // Copy function parameters into their corresponding local slots.
        // GIR convention: locals 1..=N correspond to the N function parameters.
        let num_params = self.gir_func.params.len();
        if num_params > 0 && !self.block_map.is_empty() {
            let entry_bb = self.block_map[0];
            for param_idx in 0..num_params {
                let local_id = ir::types::LocalId((param_idx + 1) as u32);
                if (local_id.0 as usize) < self.local_to_slot.len() {
                    let param_val = self.lir_func.next_value();
                    let slot = self.local_to_slot[local_id.0 as usize];
                    let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
                    self.lir_func.block_mut(entry_bb).insts.push(Inst::ParamRef {
                        dst: param_val,
                        index: param_idx as u32,
                        ty: slot_ty,
                    });
                    self.lir_func.block_mut(entry_bb).insts.push(Inst::SlotStore {
                        slot,
                        value: param_val,
                    });
                }
            }
        }

        for (i, gir_block) in self.gir_func.blocks.iter().enumerate() {
            let lir_bb = self.block_map[i];

            // Lower instructions.
            for inst in &gir_block.instructions {
                self.lower_instruction(inst, lir_bb);
            }

            // Lower terminator (operand loads emitted into the same block).
            if let Some(ref term) = gir_block.terminator {
                let lir_term = self.lower_terminator(term, lir_bb);
                self.lir_func.block_mut(lir_bb).terminator = lir_term;
            }
            // If no terminator, leave as Unreachable (the default).
        }
    }

    fn lower_instruction(&mut self, inst: &Instruction, bb: BlockId) {
        match inst {
            Instruction::Assign { dst, value } => {
                let val = self.lower_operand(value, bb);
                self.store_to_place(dst, val, bb);
            }

            Instruction::BinOp {
                dst,
                op,
                type_id,
                lhs,
                rhs,
            } => {
                let l = self.lower_operand(lhs, bb);
                let r = self.lower_operand(rhs, bb);
                let result = self.lir_func.next_value();
                let ty = map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg));
                let inst = lower_binop(result, *op, l, r, ty);
                self.lir_func.block_mut(bb).insts.push(inst);
                self.store_to_local(*dst, result, bb);
            }

            Instruction::UnOp {
                dst,
                op,
                type_id,
                operand,
            } => {
                let val = self.lower_operand(operand, bb);
                let result = self.lir_func.next_value();
                let ty = map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg));
                let inst = lower_unop(result, *op, val, ty);
                self.lir_func.block_mut(bb).insts.push(inst);
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Cmp {
                dst,
                op,
                type_id: _,
                lhs,
                rhs,
            } => {
                let l = self.lower_operand(lhs, bb);
                let r = self.lower_operand(rhs, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Cmp {
                    dst: result,
                    op: map_cmp_op(*op),
                    lhs: l,
                    rhs: r,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Cast {
                dst,
                target_type,
                value,
            } => {
                let val = self.lower_operand(value, bb);
                let to = map_gir_type_with_structs(target_type, self.gir_types, Some(self.struct_reg));
                let result = self.lir_func.next_value();
                // Determine cast kind based on types.
                self.lir_func.block_mut(bb).insts.push(Inst::IntCast {
                    dst: result,
                    value: val,
                    to,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::BitCast {
                dst,
                target_type,
                value,
            } => {
                let val = self.lower_operand(value, bb);
                let to = map_gir_type_with_structs(target_type, self.gir_types, Some(self.struct_reg));
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Bitcast {
                    dst: result,
                    value: val,
                    to,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PtrCast { dst, value, .. } => {
                let val = self.lower_operand(value, bb);
                let result = self.lir_func.next_value();
                self.lir_func
                    .block_mut(bb)
                    .insts
                    .push(Inst::PtrCast { dst: result, value: val });
                self.store_to_local(*dst, result, bb);
            }

            // -- Calls --
            Instruction::Call { dst, func, args } => {
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());

                if let Some(fid) = self.func_index.get(func) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                } else {
                    // Unknown function — treat as extern.
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: result,
                        name: func.clone(),
                        args: lir_args,
                    });
                }

                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }
            }

            Instruction::CallExtern { dst, func, args } => {
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: result,
                    name: func.clone(),
                    args: lir_args,
                });
                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }
            }

            Instruction::CallIndirect { dst, callee, args } => {
                let callee_val = self.lower_operand(callee, bb);
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());
                self.lir_func.block_mut(bb).insts.push(Inst::CallPtr {
                    dst: result,
                    callee: callee_val,
                    args: lir_args,
                });
                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }
            }

            // -- Struct/aggregate init --
            Instruction::StructInit {
                dst,
                type_name,
                fields,
            } => {
                // Get or create the struct type.
                let struct_id = self
                    .struct_reg
                    .lookup(type_name)
                    .unwrap_or(StructId(0)); // fallback

                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });

                for (i, field_op) in fields.iter().enumerate() {
                    let val = self.lower_operand(field_op, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: i as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            Instruction::FieldLoad {
                dst,
                base,
                field,
            } => {
                let base_val = self.lower_place_addr(base, bb);
                // Use effective type after base projections (e.g., Deref→Field chain).
                let effective_type = self.effective_place_type(base);
                let struct_id = self.resolve_struct_id_for_field(effective_type, *field, self.module_structs);
                let fptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: *field,
                });
                let result = self.lir_func.next_value();
                let field_ty = self.resolve_field_type(effective_type, *field);
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: fptr,
                    ty: field_ty,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::IndexLoad { dst, base, index } => {
                let base_val = self.lower_place_addr(base, bb);
                let idx = self.lower_operand(index, bb);
                let elem_ptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::ElemPtr {
                    dst: elem_ptr,
                    base: base_val,
                    index: idx,
                    elem_size: 8, // default to 8; refined later
                });
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: elem_ptr,
                    ty: LirType::I64, // default; refined later
                });
                self.store_to_local(*dst, result, bb);
            }

            // -- Enum --
            Instruction::EnumInit {
                dst,
                type_name,
                variant,
                fields,
            } => {
                let struct_id = self
                    .struct_reg
                    .lookup(type_name)
                    .unwrap_or(StructId(0));

                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });

                // Store tag (field 0).
                let tag_val = self.lir_func.next_value();
                let tag_ordinal = self.resolve_variant_ordinal(type_name, variant);
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: tag_val,
                    ty: LirType::I32,
                    value: tag_ordinal as i64,
                });
                let tag_ptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: tag_ptr,
                    base,
                    struct_id,
                    field: 0,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::Store {
                    ptr: tag_ptr,
                    value: tag_val,
                });

                // Store variant fields (offset: 1 + sum of preceding variant fields).
                let field_offset = self.resolve_variant_field_offset(type_name, variant);
                for (i, field_op) in fields.iter().enumerate() {
                    let val = self.lower_operand(field_op, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: (field_offset + i) as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            Instruction::TagOf { dst, operand } => {
                let val = self.lower_operand(operand, bb);
                // Tag is at field 0 of the enum struct. Load it via FieldPtr.
                let tag_ptr = self.lir_func.next_value();
                // We need the struct id. For TagOf on an operand that's a local:
                let struct_id = if let Operand::Copy(p) | Operand::Move(p) = operand {
                    let gir_type_id = self.gir_func.locals[p.local.0 as usize].type_id;
                    self.resolve_struct_id(gir_type_id)
                } else {
                    StructId(0) // fallback
                };
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: tag_ptr,
                    base: val,
                    struct_id,
                    field: 0,
                });
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: tag_ptr,
                    ty: LirType::I32,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::EnumFieldLoad {
                dst,
                base,
                variant,
                field,
            } => {
                let base_val = self.lower_place_addr(base, bb);
                let gir_type_id = self.gir_func.locals[base.local.0 as usize].type_id;
                let struct_id = self.resolve_struct_id(gir_type_id);
                let type_name = self.resolve_type_name(gir_type_id);
                let field_offset = self.resolve_variant_field_offset(&type_name, variant);

                let fptr = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                    dst: fptr,
                    base: base_val,
                    struct_id,
                    field: (field_offset + *field as usize) as u32,
                });
                let result = self.lir_func.next_value();
                let field_ty = self.resolve_enum_field_type(gir_type_id, variant, *field);
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst: result,
                    ptr: fptr,
                    ty: field_ty,
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::TupleInit { dst, elements } => {
                // Tuples are stored as struct slots. Store each element by field index.
                let slot = self.local_to_slot[dst.0 as usize];
                let base = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotAddr {
                    dst: base,
                    slot,
                });
                // Need the struct_id for the tuple type.
                let gir_type_id = self.gir_func.locals[dst.0 as usize].type_id;
                let struct_id = self.resolve_struct_id(gir_type_id);

                for (i, elem) in elements.iter().enumerate() {
                    let val = self.lower_operand(elem, bb);
                    let fptr = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: fptr,
                        base,
                        struct_id,
                        field: i as u32,
                    });
                    self.lir_func.block_mut(bb).insts.push(Inst::Store {
                        ptr: fptr,
                        value: val,
                    });
                }
            }

            // -- Ownership / lifetime (pass-through as calls or nops) --
            Instruction::Drop { place } | Instruction::DropIfAlive { place } => {
                // For now, emit as Nop. Phase 2.6 will elaborate drops into call sequences.
                let _ = place;
                self.lir_func.block_mut(bb).insts.push(Inst::Nop);
            }

            Instruction::MoveZero { place } => {
                // Zero out a place after move. Emit memset(addr, 0, sizeof).
                let addr = self.lower_place_addr(place, bb);
                let zero = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: zero,
                    ty: LirType::I32,
                    value: 0,
                });
                let size = self.lir_func.next_value();
                let slot_idx = place.local.0 as usize;
                let slot_ty = &self.lir_func.slots[self.local_to_slot[slot_idx].0 as usize].ty;
                let byte_size = super::types::scalar_size(slot_ty).unwrap_or(8) as i64;
                self.lir_func.block_mut(bb).insts.push(Inst::IConst {
                    dst: size,
                    ty: LirType::I64,
                    value: byte_size,
                });
                self.lir_func.block_mut(bb).insts.push(Inst::Memset {
                    ptr: addr,
                    byte: zero,
                    size,
                });
            }

            Instruction::Borrow { dst, place } | Instruction::BorrowMut { dst, place } => {
                let addr = self.lower_place_addr(place, bb);
                self.store_to_local(*dst, addr, bb);
            }

            // -- Allocator --
            Instruction::HeapAlloc {
                dst,
                type_id: _,
                allocator,
            } => {
                // Placeholder: lower as CallExtern to malloc-like.
                let alloc = self.lower_operand(allocator, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc".into(),
                    args: vec![alloc],
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::HeapAllocArray {
                dst,
                type_id: _,
                count,
                allocator,
            } => {
                let cnt = self.lower_operand(count, bb);
                let alloc = self.lower_operand(allocator, bb);
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: "__gorget_alloc_array".into(),
                    args: vec![cnt, alloc],
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::Dealloc { ptr, allocator } => {
                let p = self.lower_operand(ptr, bb);
                let a = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_dealloc".into(),
                    args: vec![p, a],
                });
            }

            Instruction::LoadThreadLocal { dst, name } => {
                let result = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: Some(result),
                    name: format!("__gorget_tls_{name}"),
                    args: vec![],
                });
                self.store_to_local(*dst, result, bb);
            }

            Instruction::PushAllocator { allocator } => {
                let alloc = self.lower_operand(allocator, bb);
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_push_allocator".into(),
                    args: vec![alloc],
                });
            }

            Instruction::PopAllocator => {
                self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                    dst: None,
                    name: "__gorget_pop_allocator".into(),
                    args: vec![],
                });
            }

            Instruction::InlineC { code } => {
                // InlineC has no LIR equivalent — it's a C-backend-specific escape hatch.
                // Emit as a trap with a descriptive message for now.
                self.lir_func.block_mut(bb).insts.push(Inst::Trap {
                    msg: format!("InlineC not supported in LIR: {}", &code[..code.len().min(60)]),
                });
            }

            Instruction::Nop => {
                self.lir_func.block_mut(bb).insts.push(Inst::Nop);
            }
        }
    }

    fn lower_terminator(&mut self, term: &Terminator, bb: BlockId) -> Term {
        match term {
            Terminator::Return(operand) => {
                let ret_type = map_gir_type_with_structs(&self.gir_func.return_type, self.gir_types, Some(self.struct_reg));
                if ret_type == LirType::Void {
                    Term::RetVoid
                } else {
                    let val = self.lower_operand(operand, bb);
                    Term::Ret(val)
                }
            }
            Terminator::Jump(target) => {
                let lir_target = self.block_map[target.0 as usize];
                Term::Jump(lir_target, vec![])
            }
            Terminator::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let cond_val = self.lower_operand(cond, bb);
                Term::Branch {
                    cond: cond_val,
                    then_block: self.block_map[then_block.0 as usize],
                    then_args: vec![],
                    else_block: self.block_map[else_block.0 as usize],
                    else_args: vec![],
                }
            }
            Terminator::Switch {
                value,
                cases,
                default,
            } => {
                let val = self.lower_operand(value, bb);
                let lir_cases: Vec<(i64, BlockId, Vec<ValueId>)> = cases
                    .iter()
                    .map(|(v, b)| (*v, self.block_map[b.0 as usize], vec![]))
                    .collect();
                Term::Switch {
                    value: val,
                    cases: lir_cases,
                    default: self.block_map[default.0 as usize],
                    default_args: vec![],
                }
            }
            Terminator::Invoke {
                func,
                args,
                dst,
                normal,
                error,
            } => {
                // Invoke = call that can throw + branch on success/error.
                // Emit the call in the block, then jump to normal.
                // TODO: Phase 2.6 — proper try/catch lowering with error path.
                let lir_args: Vec<ValueId> =
                    args.iter().map(|a| self.lower_operand(a, bb)).collect();
                let result = dst.map(|_| self.lir_func.next_value());

                if let Some(fid) = self.func_index.get(func) {
                    self.lir_func.block_mut(bb).insts.push(Inst::Call {
                        dst: result,
                        func: *fid,
                        args: lir_args,
                    });
                } else {
                    self.lir_func.block_mut(bb).insts.push(Inst::CallExtern {
                        dst: result,
                        name: func.clone(),
                        args: lir_args,
                    });
                }

                if let (Some(d), Some(r)) = (*dst, result) {
                    self.store_to_local(d, r, bb);
                }

                let _ = error; // error path not yet lowered
                Term::Jump(self.block_map[normal.0 as usize], vec![])
            }
            Terminator::Unreachable => Term::Unreachable,
        }
    }

    // ── Operand lowering ────────────────────────────────────────────────────

    /// Lower a GIR operand, emitting load instructions into block `bb`.
    fn lower_operand(&mut self, operand: &Operand, bb: BlockId) -> ValueId {
        match operand {
            Operand::Copy(place) | Operand::Move(place) => self.lower_place_load(place, bb),
            Operand::Constant(c) => self.lower_constant(c, bb),
        }
    }

    /// Load a value from a GIR place.
    fn lower_place_load(&mut self, place: &Place, bb: BlockId) -> ValueId {
        if place.projections.is_empty() {
            // Simple local — SlotLoad.
            let slot = self.local_to_slot[place.local.0 as usize];
            let slot_ty = self.lir_func.slots[slot.0 as usize].ty.clone();
            if slot_ty.is_aggregate() {
                // For aggregates, return address of slot.
                let addr = self.lir_func.next_value();
                self.lir_func
                    .block_mut(bb)
                    .insts
                    .push(Inst::SlotAddr { dst: addr, slot });
                addr
            } else {
                let dst = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                    dst,
                    slot,
                    ty: slot_ty,
                });
                dst
            }
        } else {
            // Projected place — compute address then load.
            let addr = self.lower_place_addr(place, bb);
            let ty = self.resolve_place_type(place);
            if ty.is_aggregate() {
                addr // aggregates: the address IS the value
            } else {
                let dst = self.lir_func.next_value();
                self.lir_func.block_mut(bb).insts.push(Inst::Load {
                    dst,
                    ptr: addr,
                    ty,
                });
                dst
            }
        }
    }

    /// Get the address of a GIR place.
    fn lower_place_addr(&mut self, place: &Place, bb: BlockId) -> ValueId {
        let slot = self.local_to_slot[place.local.0 as usize];
        let mut addr = self.lir_func.next_value();
        self.lir_func
            .block_mut(bb)
            .insts
            .push(Inst::SlotAddr { dst: addr, slot });

        // Track the current GIR type through each projection step.
        let mut current_gir_type = self.gir_func.locals[place.local.0 as usize].type_id;

        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    let struct_id = self.resolve_struct_id_for_field(current_gir_type, *field, self.module_structs);
                    let next = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::FieldPtr {
                        dst: next,
                        base: addr,
                        struct_id,
                        field: *field,
                    });
                    addr = next;
                    // Update type to the field's type for subsequent projections.
                    current_gir_type = self.resolve_field_gir_type_id(current_gir_type, *field);
                }
                Projection::Index(idx_local) => {
                    let idx_slot = self.local_to_slot[idx_local.0 as usize];
                    let idx = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::SlotLoad {
                        dst: idx,
                        slot: idx_slot,
                        ty: LirType::I64,
                    });
                    let next = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::ElemPtr {
                        dst: next,
                        base: addr,
                        index: idx,
                        elem_size: 8,
                    });
                    addr = next;
                }
                Projection::Deref => {
                    // Load the pointer from addr, then use that as the new addr.
                    let ptr_val = self.lir_func.next_value();
                    self.lir_func.block_mut(bb).insts.push(Inst::Load {
                        dst: ptr_val,
                        ptr: addr,
                        ty: LirType::Ptr,
                    });
                    addr = ptr_val;
                    // Update type to the pointee type.
                    current_gir_type = self.resolve_deref_gir_type_id(current_gir_type);
                }
            }
        }

        addr
    }

    /// Lower a GIR constant to a LIR value.
    fn lower_constant(&mut self, c: &Constant, bb: BlockId) -> ValueId {
        let dst = self.lir_func.next_value();
        let inst = match c {
            Constant::Bool(v) => Inst::BoolConst { dst, value: *v },
            Constant::I8(v) => Inst::IConst { dst, ty: LirType::I8, value: *v as i64 },
            Constant::I16(v) => Inst::IConst { dst, ty: LirType::I16, value: *v as i64 },
            Constant::I32(v) => Inst::IConst { dst, ty: LirType::I32, value: *v as i64 },
            Constant::I64(v) => Inst::IConst { dst, ty: LirType::I64, value: *v },
            Constant::U8(v) => Inst::IConst { dst, ty: LirType::U8, value: *v as i64 },
            Constant::U16(v) => Inst::IConst { dst, ty: LirType::U16, value: *v as i64 },
            Constant::U32(v) => Inst::IConst { dst, ty: LirType::U32, value: *v as i64 },
            Constant::U64(v) => Inst::IConst { dst, ty: LirType::U64, value: *v as i64 },
            Constant::F32(v) => Inst::FConst { dst, ty: LirType::F32, bits: (*v as f64).to_bits() },
            Constant::F64(v) => Inst::FConst { dst, ty: LirType::F64, bits: v.to_bits() },
            Constant::Str(s) => Inst::StrLit { dst, value: s.clone() },
            Constant::Null => Inst::NullPtr { dst },
            Constant::Unit => Inst::IConst { dst, ty: LirType::I32, value: 0 }, // unit = zero
            Constant::SizeOf(type_id) => {
                let ty = map_gir_type_with_structs(type_id, self.gir_types, Some(self.struct_reg));
                let size = super::types::scalar_size(&ty).unwrap_or(8);
                Inst::IConst { dst, ty: LirType::I64, value: size as i64 }
            }
            Constant::FuncRef(name) => {
                if let Some(fid) = self.func_index.get(name) {
                    Inst::FuncAddr { dst, func: *fid }
                } else {
                    // Unknown function — emit as a string for now.
                    Inst::IConst { dst, ty: LirType::I64, value: 0 }
                }
            }
            Constant::GlobalRef(name) => {
                // Look up the global. For now emit a placeholder.
                let _ = name;
                Inst::NullPtr { dst }
            }
        };
        self.lir_func.block_mut(bb).insts.push(inst);
        dst
    }

    // ── Store helpers ───────────────────────────────────────────────────────

    fn store_to_local(&mut self, local: ir::types::LocalId, value: ValueId, bb: BlockId) {
        let slot = self.local_to_slot[local.0 as usize];
        self.lir_func
            .block_mut(bb)
            .insts
            .push(Inst::SlotStore { slot, value });
    }

    fn store_to_place(&mut self, place: &Place, value: ValueId, bb: BlockId) {
        if place.projections.is_empty() {
            self.store_to_local(place.local, value, bb);
        } else {
            let addr = self.lower_place_addr(place, bb);
            self.lir_func
                .block_mut(bb)
                .insts
                .push(Inst::Store { ptr: addr, value });
        }
    }

    // ── Type resolution helpers ─────────────────────────────────────────────

    fn resolve_struct_id(&self, gir_type_id: GirTypeId) -> StructId {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            if let Some(sid) = self.struct_reg.lookup(name) {
                return sid;
            }
        }
        StructId(0) // fallback
    }

    /// Resolve struct ID with field-count safety: if the resolved struct has
    /// fewer fields than the field index, try a wider compatible type.
    /// Handles Str→GorgetString promotion (GIR uses GorgetString fields on Str locals).
    fn resolve_struct_id_for_field(&self, gir_type_id: GirTypeId, field: u32, structs: &[StructDef]) -> StructId {
        let sid = self.resolve_struct_id(gir_type_id);
        let struct_def = &structs[sid.0 as usize];
        if (field as usize) < struct_def.fields.len() {
            return sid;
        }
        // Str (2 fields) with field >= 2 → GorgetString (3 fields).
        if struct_def.name == "Str" {
            if let Some(gs_sid) = self.struct_reg.lookup("GorgetString") {
                return gs_sid;
            }
        }
        sid
    }

    fn resolve_type_name(&self, gir_type_id: GirTypeId) -> String {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            name.clone()
        } else {
            String::new()
        }
    }

    fn resolve_field_type(&self, gir_type_id: GirTypeId, field: u32) -> LirType {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                    if let Some(f) = sdef.fields.get(field as usize) {
                        return map_gir_type_with_structs(&f.type_id, self.gir_types, Some(self.struct_reg));
                    }
                }
            }
        }
        LirType::I64 // fallback
    }

    /// Return the GIR TypeId of a struct field (for tracking types through projection chains).
    fn resolve_field_gir_type_id(&self, gir_type_id: GirTypeId, field: u32) -> GirTypeId {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                    if let Some(f) = sdef.fields.get(field as usize) {
                        return f.type_id;
                    }
                }
            }
        }
        gir_type_id // fallback: keep same type
    }

    /// Resolve the pointee type for a Deref projection.
    fn resolve_deref_gir_type_id(&self, gir_type_id: GirTypeId) -> GirTypeId {
        match self.gir_types.get(gir_type_id) {
            Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) => *inner,
            _ => gir_type_id, // fallback
        }
    }

    /// Compute the effective GIR type after following all projections in a place.
    fn effective_place_type(&self, place: &Place) -> GirTypeId {
        let mut ty = self.gir_func.locals[place.local.0 as usize].type_id;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    ty = self.resolve_field_gir_type_id(ty, *field);
                }
                Projection::Deref => {
                    ty = self.resolve_deref_gir_type_id(ty);
                }
                Projection::Index(_) => {
                    // Element type — keep as-is for now (array element type tracking TBD)
                }
            }
        }
        ty
    }

    fn resolve_enum_field_type(
        &self,
        gir_type_id: GirTypeId,
        variant_name: &str,
        field: u32,
    ) -> LirType {
        let gir_type = self.gir_types.get(gir_type_id);
        if let Some(GirType::Named(name)) = gir_type {
            if let Some(def) = self.gir_types.get_type_def(name) {
                if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                    for v in &edef.variants {
                        if v.name == variant_name {
                            if let Some(f) = v.fields.get(field as usize) {
                                return map_gir_type_with_structs(&f.type_id, self.gir_types, Some(self.struct_reg));
                            }
                        }
                    }
                }
            }
        }
        LirType::I64 // fallback
    }

    fn resolve_variant_ordinal(&self, type_name: &str, variant_name: &str) -> usize {
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                for (i, v) in edef.variants.iter().enumerate() {
                    if v.name == variant_name {
                        return i;
                    }
                }
            }
        }
        0
    }

    fn resolve_variant_field_offset(&self, type_name: &str, variant_name: &str) -> usize {
        // Field offset = 1 (tag) + sum of field counts of preceding variants.
        if let Some(def) = self.gir_types.get_type_def(type_name) {
            if let gir_types::TypeDefKind::Enum(edef) = &def.kind {
                let mut offset = 1; // tag field
                for v in &edef.variants {
                    if v.name == variant_name {
                        return offset;
                    }
                    offset += v.fields.len();
                }
            }
        }
        1
    }

    fn resolve_place_type(&self, place: &Place) -> LirType {
        let local_type = self.gir_func.locals[place.local.0 as usize].type_id;
        if place.projections.is_empty() {
            return map_gir_type_with_structs(&local_type, self.gir_types, Some(self.struct_reg));
        }

        // Walk projections to determine final type.
        let mut current_type = local_type;
        for proj in &place.projections {
            match proj {
                Projection::Field(field) => {
                    if let Some(GirType::Named(name)) = self.gir_types.get(current_type) {
                        if let Some(def) = self.gir_types.get_type_def(&name) {
                            if let gir_types::TypeDefKind::Struct(sdef) = &def.kind {
                                if let Some(f) = sdef.fields.get(*field as usize) {
                                    current_type = f.type_id;
                                    continue;
                                }
                            }
                        }
                    }
                    return LirType::I64; // fallback
                }
                Projection::Deref => {
                    if let Some(GirType::Ptr(inner)) | Some(GirType::MutPtr(inner)) =
                        self.gir_types.get(current_type)
                    {
                        current_type = *inner;
                    }
                }
                Projection::Index(_) => {
                    return LirType::I64; // default element type
                }
            }
        }

        map_gir_type_with_structs(&current_type, self.gir_types, Some(self.struct_reg))
    }
}

// ── Free functions ──────────────────────────────────────────────────────────

/// Map a GIR TypeId to an LIR type.
pub fn map_gir_type(type_id: &GirTypeId, registry: &TypeRegistry) -> LirType {
    map_gir_type_with_structs(type_id, registry, None)
}

pub fn map_gir_type_with_structs(
    type_id: &GirTypeId,
    registry: &TypeRegistry,
    struct_reg: Option<&StructRegistry>,
) -> LirType {
    // Check primitive type constants first.
    match *type_id {
        gir_types::BOOL_TYPE => return LirType::Bool,
        gir_types::I8_TYPE => return LirType::I8,
        gir_types::I16_TYPE => return LirType::I16,
        gir_types::I32_TYPE => return LirType::I32,
        gir_types::I64_TYPE => return LirType::I64,
        gir_types::U8_TYPE => return LirType::U8,
        gir_types::U16_TYPE => return LirType::U16,
        gir_types::U32_TYPE => return LirType::U32,
        gir_types::U64_TYPE => return LirType::U64,
        gir_types::F32_TYPE => return LirType::F32,
        gir_types::F64_TYPE => return LirType::F64,
        gir_types::UNIT_TYPE => return LirType::Void,
        _ => {}
    }

    if let Some(gir_type) = registry.get(*type_id) {
        match gir_type {
            GirType::Bool => LirType::Bool,
            GirType::I8 => LirType::I8,
            GirType::I16 => LirType::I16,
            GirType::I32 => LirType::I32,
            GirType::I64 => LirType::I64,
            GirType::U8 => LirType::U8,
            GirType::U16 => LirType::U16,
            GirType::U32 => LirType::U32,
            GirType::U64 => LirType::U64,
            GirType::F32 => LirType::F32,
            GirType::F64 => LirType::F64,
            GirType::Unit => LirType::Void,
            GirType::Ptr(_) | GirType::MutPtr(_) => LirType::Ptr,
            GirType::FnPtr { .. } => LirType::Ptr,
            GirType::Named(name) => {
                // Named types are aggregates — resolve to Struct if registered.
                if let Some(sr) = struct_reg {
                    if let Some(sid) = sr.lookup(name) {
                        return LirType::Struct(sid);
                    }
                }
                LirType::Ptr
            }
        }
    } else {
        LirType::I64 // fallback for unknown types
    }
}

fn lower_binop(dst: ValueId, op: GirBinOp, lhs: ValueId, rhs: ValueId, ty: LirType) -> Inst {
    match op {
        GirBinOp::Add => Inst::Add { dst, ty, lhs, rhs, overflow: Overflow::Trap },
        GirBinOp::Sub => Inst::Sub { dst, ty, lhs, rhs, overflow: Overflow::Trap },
        GirBinOp::Mul => Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Trap },
        GirBinOp::Div => Inst::Div { dst, ty, lhs, rhs },
        GirBinOp::Rem => Inst::Rem { dst, ty, lhs, rhs },
        GirBinOp::Mod => Inst::Mod { dst, ty, lhs, rhs },
        GirBinOp::Pow => {
            // Pow doesn't have a direct LIR instruction. Emit as CallExtern to pow().
            // For now, emit as Mul (placeholder).
            Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Trap }
        }
        GirBinOp::BitAnd => Inst::BitAnd { dst, ty, lhs, rhs },
        GirBinOp::BitOr => Inst::BitOr { dst, ty, lhs, rhs },
        GirBinOp::BitXor => Inst::BitXor { dst, ty, lhs, rhs },
        GirBinOp::Shl => Inst::Shl { dst, ty, lhs, rhs },
        GirBinOp::Shr => Inst::Shr { dst, ty, lhs, rhs },
        GirBinOp::AddWrap => Inst::Add { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
        GirBinOp::SubWrap => Inst::Sub { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
        GirBinOp::MulWrap => Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
    }
}

fn lower_unop(dst: ValueId, op: GirUnOp, operand: ValueId, ty: LirType) -> Inst {
    match op {
        GirUnOp::Neg => Inst::Neg { dst, ty, operand },
        GirUnOp::Not => Inst::Not { dst, operand },
        GirUnOp::BitNot => Inst::BitNot { dst, ty, operand },
    }
}

fn map_cmp_op(op: GirCmpOp) -> CmpOp {
    match op {
        GirCmpOp::Eq => CmpOp::Eq,
        GirCmpOp::Ne => CmpOp::Ne,
        GirCmpOp::Lt => CmpOp::Lt,
        GirCmpOp::Le => CmpOp::Le,
        GirCmpOp::Gt => CmpOp::Gt,
        GirCmpOp::Ge => CmpOp::Ge,
    }
}

fn lower_global_init(init: &ir::GlobalInit) -> LirGlobalInit {
    match init {
        ir::GlobalInit::Zeroed => LirGlobalInit::Zeroed,
        ir::GlobalInit::Bytes(b) => LirGlobalInit::Bytes(b.clone()),
        ir::GlobalInit::FnRef(_name) => {
            // We'd need to look up the FuncId. For now, zeroed.
            LirGlobalInit::Zeroed
        }
        ir::GlobalInit::Struct { fields, .. } => {
            LirGlobalInit::Struct {
                struct_id: StructId(0), // placeholder
                fields: fields.iter().map(|(_, f)| lower_global_init(f)).collect(),
            }
        }
        ir::GlobalInit::RuntimeCall(_) => LirGlobalInit::Zeroed,
    }
}

/// Top-level entry point: lower a GIR module to LIR.
pub fn lower_module(gir: &ir::Module) -> LirModule {
    let ctx = LoweringContext::new(gir);
    ctx.lower()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::instructions::{BinOp, Constant, Instruction, Operand, Place, Terminator};
    use crate::ir::types::{LocalId, BlockId as GirBlockId, I32_TYPE, I64_TYPE, BOOL_TYPE};
    use crate::ir::{BasicBlock, Function, Local, Module};

    fn make_simple_gir_module() -> Module {
        let mut module = Module::new();

        // fn main() -> i32:
        //   _0: i32 (return)
        //   _1: i32 = 42
        //   _2: i32 = _1 + 1
        //   return _2
        let func = Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: Some("return".into()) },
                Local { type_id: I32_TYPE, name_hint: Some("x".into()) },
                Local { type_id: I32_TYPE, name_hint: Some("y".into()) },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![
                    Instruction::Assign {
                        dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::I32(42)),
                    },
                    Instruction::BinOp {
                        dst: LocalId(2),
                        op: BinOp::Add,
                        type_id: I32_TYPE,
                        lhs: Operand::Copy(Place::local(LocalId(1))),
                        rhs: Operand::Constant(Constant::I32(1)),
                    },
                ],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                span_map: vec![None, None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: Some("main".into()),
            def_span: None,
            with_refresh_pairs: Vec::new(),
        };
        module.functions.push(func);
        module
    }

    #[test]
    fn lower_simple_function() {
        let gir = make_simple_gir_module();
        let lir = lower_module(&gir);

        assert_eq!(lir.functions.len(), 1);
        let func = &lir.functions[0];
        assert_eq!(func.name, "main");
        assert_eq!(func.return_type, LirType::I32);
        assert_eq!(func.blocks.len(), 1);

        // Should have: IConst(42), SlotStore, SlotLoad, IConst(1), SlotLoad, Add, SlotStore, SlotLoad, Ret
        let block = &func.blocks[0];
        assert!(!block.insts.is_empty());

        // Verify the terminator is Ret.
        assert!(matches!(block.terminator, Term::Ret(_)));
    }

    #[test]
    fn lower_branch() {
        let mut module = Module::new();
        let func = Function {
            name: "test_branch".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None },
                Local { type_id: BOOL_TYPE, name_hint: Some("cond".into()) },
                Local { type_id: I64_TYPE, name_hint: Some("result".into()) },
            ],
            blocks: vec![
                BasicBlock {
                    instructions: vec![Instruction::Assign {
                        dst: Place::local(LocalId(1)),
                        value: Operand::Constant(Constant::Bool(true)),
                    }],
                    terminator: Some(Terminator::Branch {
                        cond: Operand::Copy(Place::local(LocalId(1))),
                        then_block: GirBlockId(1),
                        else_block: GirBlockId(2),
                    }),
                    span_map: vec![None],
                    terminator_span: None,
                },
                BasicBlock {
                    instructions: vec![Instruction::Assign {
                        dst: Place::local(LocalId(2)),
                        value: Operand::Constant(Constant::I64(10)),
                    }],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                    span_map: vec![None],
                    terminator_span: None,
                },
                BasicBlock {
                    instructions: vec![Instruction::Assign {
                        dst: Place::local(LocalId(2)),
                        value: Operand::Constant(Constant::I64(20)),
                    }],
                    terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(2))))),
                    span_map: vec![None],
                    terminator_span: None,
                },
            ],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
        };
        module.functions.push(func);

        let lir = lower_module(&module);
        assert_eq!(lir.functions[0].blocks.len(), 3);

        let term = &lir.functions[0].blocks[0].terminator;
        assert!(matches!(term, Term::Branch { .. }));
    }

    #[test]
    fn lower_call() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "callee".into(),
            params: vec![I64_TYPE],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None },
                Local { type_id: I64_TYPE, name_hint: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                span_map: vec![],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
        });
        module.functions.push(Function {
            name: "caller".into(),
            params: vec![],
            return_type: I64_TYPE,
            locals: vec![
                Local { type_id: I64_TYPE, name_hint: None },
                Local { type_id: I64_TYPE, name_hint: Some("result".into()) },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Call {
                    dst: Some(LocalId(1)),
                    func: "callee".into(),
                    args: vec![Operand::Constant(Constant::I64(5))],
                }],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                span_map: vec![None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
        });

        let lir = lower_module(&module);
        assert_eq!(lir.functions.len(), 2);

        // Caller's block should contain a Call instruction.
        let caller = &lir.functions[1];
        let has_call = caller.blocks[0].insts.iter().any(|inst| {
            matches!(inst, Inst::Call { func, .. } if *func == FuncId(0))
        });
        assert!(has_call, "expected Call to callee (FuncId(0))");
    }

    #[test]
    fn lower_extern_call() {
        let mut module = Module::new();
        module.externs.push(ir::ExternDecl {
            name: "puts".into(),
            params: vec![],
            return_type: I32_TYPE,
            is_variadic: false,
        });
        module.functions.push(Function {
            name: "main".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: None },
                Local { type_id: I32_TYPE, name_hint: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::CallExtern {
                    dst: Some(LocalId(1)),
                    func: "puts".into(),
                    args: vec![],
                }],
                terminator: Some(Terminator::Return(Operand::Copy(Place::local(LocalId(1))))),
                span_map: vec![None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
        });

        let lir = lower_module(&module);
        assert_eq!(lir.externs.len(), 1);
        assert_eq!(lir.externs[0].name, "puts");

        let has_extern_call = lir.functions[0].blocks[0].insts.iter().any(|inst| {
            matches!(inst, Inst::CallExtern { name, .. } if name == "puts")
        });
        assert!(has_extern_call);
    }

    #[test]
    fn type_mapping_primitives() {
        let reg = TypeRegistry::new();
        assert_eq!(map_gir_type(&gir_types::BOOL_TYPE, &reg), LirType::Bool);
        assert_eq!(map_gir_type(&gir_types::I32_TYPE, &reg), LirType::I32);
        assert_eq!(map_gir_type(&gir_types::I64_TYPE, &reg), LirType::I64);
        assert_eq!(map_gir_type(&gir_types::F64_TYPE, &reg), LirType::F64);
        assert_eq!(map_gir_type(&gir_types::U8_TYPE, &reg), LirType::U8);
        assert_eq!(map_gir_type(&gir_types::UNIT_TYPE, &reg), LirType::Void);
    }

    #[test]
    fn lower_constants() {
        let mut module = Module::new();
        module.functions.push(Function {
            name: "consts".into(),
            params: vec![],
            return_type: I32_TYPE,
            locals: vec![
                Local { type_id: I32_TYPE, name_hint: None },
                Local { type_id: I32_TYPE, name_hint: None },
            ],
            blocks: vec![BasicBlock {
                instructions: vec![Instruction::Assign {
                    dst: Place::local(LocalId(1)),
                    value: Operand::Constant(Constant::I32(99)),
                }],
                terminator: Some(Terminator::Return(Operand::Constant(Constant::I32(0)))),
                span_map: vec![None],
                terminator_span: None,
            }],
            is_test_fn: false,
            display_name: None,
            def_span: None,
            with_refresh_pairs: Vec::new(),
        });

        let lir = lower_module(&module);
        let func = &lir.functions[0];
        let has_iconst_99 = func.blocks[0].insts.iter().any(|inst| {
            matches!(inst, Inst::IConst { value: 99, .. })
        });
        assert!(has_iconst_99, "expected IConst 99");
    }

    #[test]
    fn dump_lowered_module() {
        let gir = make_simple_gir_module();
        let lir = lower_module(&gir);
        let output = super::super::display::dump_module(&lir);
        assert!(output.contains("fn @main"));
        assert!(output.contains("iconst 42"));
        assert!(output.contains("add"));
        assert!(output.contains("ret"));
    }
}
