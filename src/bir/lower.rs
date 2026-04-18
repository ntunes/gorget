//! LIR → BIR lowering pass.
//!
//! Expands LIR's canonical-level high-level ops into sequences of primitive
//! instructions that backends can emit directly. Each canonical op listed in
//! `docs/internals/lir-backend-lift-plan.md` gets an expansion here as the
//! corresponding LIR variant is added.
//!
//! ## Current canonical ops expanded
//!
//! - `SizeOf { dst, ty }` → `IConst { dst, ty: I64, value: N }` via the shared
//!   `c_sizeof_lir_type` table.
//! - `EnumInit { target, struct_id, variant_tag, variant_idx, payload }` →
//!   `FieldPtr`(tag) + `IConst`(tag) + `Store` + optional `FieldPtr`(payload) +
//!   `Store`/`Memcpy` (depending on payload type).
//! - `EnumCheck { dst, value, struct_id, variant_tag }` → `FieldPtr`(tag) +
//!   `Load` + `IConst` + `Cmp`.
//! - `EnumExtract { dst, value, struct_id, payload_field, ty }` →
//!   `FieldPtr`(payload) + `Load`.
//!
//! Future canonical ops (`StructInit`, `NamedFieldPtr`, `HofExpand`, …) get
//! expanded here in subsequent steps.

use crate::bir::BirError;
use crate::lir::lower::types::c_sizeof_lir_type;
use crate::lir::{Inst, LirFunction, LirModule, LirType, StructDef, ValueId};

/// Expand all canonical-level ops in `module` into primitive instructions.
///
/// Returns the rewritten module. The caller (typically
/// [`crate::bir::BirModule::from_lir`]) then runs the validator to confirm
/// the invariant holds.
pub fn lower_lir_to_bir(mut module: LirModule) -> Result<LirModule, BirError> {
    // Swap `functions` out so we can iterate them mutably while holding an
    // immutable reference to `module.structs`. The structs table is the only
    // piece of module metadata the expansion reads (for c_sizeof / payload
    // type lookups); swapping functions is O(1), cloning structs would be O(N).
    let mut funcs = std::mem::take(&mut module.functions);
    for func in funcs.iter_mut() {
        expand_func(func, &module.structs);
    }
    module.functions = funcs;
    Ok(module)
}

fn expand_func(func: &mut LirFunction, structs: &[StructDef]) {
    // Fast path: skip the entire rebuild if no canonical ops are present.
    // Walking insts without cloning is O(n); rebuilding is O(n) plus allocation.
    if !func_needs_expansion(func) {
        return;
    }

    // We borrow `func` mutably to rewrite each block's inst list, so we
    // can't simultaneously call `func.next_value()`. Shadow the ValueId
    // counter locally and write it back when we're done.
    let mut next = func.next_value_raw();

    // Index-based iteration rather than `func.blocks.iter_mut()` — some
    // expansions (AddressOf) need to allocate fresh stack slots via
    // `func.add_slot(...)`, which requires a mutable borrow of `func.slots`
    // that would conflict with an outstanding `&mut block` borrow.
    let block_count = func.blocks.len();
    for bb_idx in 0..block_count {
        let old = std::mem::take(&mut func.blocks[bb_idx].insts);
        let mut new_insts: Vec<Inst> = Vec::with_capacity(old.len());
        for inst in old {
            match inst {
                Inst::SizeOf { dst, ty } => {
                    let value = c_sizeof_lir_type(&ty, structs) as i64;
                    new_insts.push(Inst::IConst { dst, ty: LirType::I64, value });
                }
                Inst::EnumInit { target, struct_id, variant_tag, fields } => {
                    // 1) Write tag field (field 0).
                    let tag_ptr = alloc_value(&mut next);
                    new_insts.push(Inst::FieldPtr {
                        dst: tag_ptr,
                        base: target,
                        struct_id,
                        field: 0,
                    });
                    let tag_val = alloc_value(&mut next);
                    new_insts.push(Inst::IConst {
                        dst: tag_val,
                        ty: LirType::I32,
                        value: variant_tag as i64,
                    });
                    new_insts.push(Inst::Store { ptr: tag_ptr, value: tag_val });

                    // 2) Write each payload field via a plain `Store`. Each
                    //    backend's Store handler already dispatches on
                    //    `val_types` to pick scalar-store vs aggregate-memcpy —
                    //    same mechanism `StructInit` relies on. EnumInit's
                    //    payload values come from heterogeneous sources
                    //    (`lower_operand` returns a scalar or aggregate value;
                    //    wrap helpers pass pointers from slot-addr); letting
                    //    the backend Store decide keeps the single source of
                    //    truth at the val_types layer.
                    for (field_idx, payload_val) in fields {
                        let payload_ptr = alloc_value(&mut next);
                        new_insts.push(Inst::FieldPtr {
                            dst: payload_ptr,
                            base: target,
                            struct_id,
                            field: field_idx,
                        });
                        new_insts.push(Inst::Store { ptr: payload_ptr, value: payload_val });
                    }
                }
                Inst::EnumCheck { dst, value, struct_id, variant_tag } => {
                    let tag_ptr = alloc_value(&mut next);
                    new_insts.push(Inst::FieldPtr {
                        dst: tag_ptr,
                        base: value,
                        struct_id,
                        field: 0,
                    });
                    let tag = alloc_value(&mut next);
                    new_insts.push(Inst::Load { dst: tag, ptr: tag_ptr, ty: LirType::I32 });
                    let expected = alloc_value(&mut next);
                    new_insts.push(Inst::IConst {
                        dst: expected,
                        ty: LirType::I32,
                        value: variant_tag as i64,
                    });
                    new_insts.push(Inst::Cmp {
                        dst,
                        op: crate::lir::CmpOp::Eq,
                        lhs: tag,
                        rhs: expected,
                    });
                }
                Inst::EnumExtract { dst, value, struct_id, payload_field, ty } => {
                    let payload_ptr = alloc_value(&mut next);
                    new_insts.push(Inst::FieldPtr {
                        dst: payload_ptr,
                        base: value,
                        struct_id,
                        field: payload_field,
                    });
                    new_insts.push(Inst::Load { dst, ptr: payload_ptr, ty });
                }
                Inst::StructInit { target, struct_id, fields } => {
                    // StructInit's field values come from `lower_operand` in GIR →
                    // LIR, which may yield a scalar value, an aggregate value, or a
                    // pointer to an aggregate. Emit a plain `Store` and let each
                    // backend's Store handler dispatch on val_types — the pre-op
                    // behaviour before StructInit was introduced.
                    for (field_idx, value) in fields {
                        let fptr = alloc_value(&mut next);
                        new_insts.push(Inst::FieldPtr {
                            dst: fptr,
                            base: target,
                            struct_id,
                            field: field_idx,
                        });
                        new_insts.push(Inst::Store { ptr: fptr, value });
                    }
                }
                Inst::NamedFieldPtr { dst, base, struct_name, field_name } => {
                    // Resolve via the opaque runtime layout table. If the struct
                    // has no known layout, fall through to looking up field index
                    // in the module's struct registry by field name.
                    let field_idx = lookup_named_field_index(&struct_name, &field_name, structs);
                    let struct_id = structs
                        .iter()
                        .position(|s| s.name == struct_name)
                        .map(|i| crate::lir::StructId(i as u32))
                        .unwrap_or(crate::lir::StructId(0));
                    new_insts.push(Inst::FieldPtr {
                        dst,
                        base,
                        struct_id,
                        field: field_idx,
                    });
                }
                Inst::CowClone { dst, src, ty } => {
                    // For strings: call gorget_string_copy_cow(&out, src).
                    // For now, only String is supported — other CoW types can
                    // extend the name resolution here.
                    let call_name = match &ty {
                        LirType::Struct(sid) => structs
                            .get(sid.0 as usize)
                            .map(|s| match s.name.as_str() {
                                "GorgetString" | "Str" => "gorget_string_copy_cow",
                                _ => "gorget_string_copy_cow",
                            })
                            .unwrap_or("gorget_string_copy_cow"),
                        _ => "gorget_string_copy_cow",
                    };
                    new_insts.push(Inst::CallExtern {
                        dst: Some(dst),
                        name: call_name.to_string(),
                        args: vec![src],
                        original_name: None,
                        arg_abis: vec![crate::ir::abi::AbiKind::Ptr],
                    });
                }
                Inst::AddressOf { dst, value, ty } => {
                    // Spill the SSA value into a fresh stack slot, then take
                    // its address. `dst` is the final address; `SlotAddr`
                    // binds it directly. For aggregate values arriving from
                    // elsewhere as pointers, the C/LLVM Store handlers
                    // dispatch on val_types — same mechanism StructInit/
                    // EnumInit rely on — so we don't need to special-case.
                    let slot = func.add_slot(ty.clone(), None);
                    new_insts.push(Inst::SlotStore {
                        slot,
                        value,
                        is_move: false,
                    });
                    new_insts.push(Inst::SlotAddr { dst, slot });
                }
                Inst::BoxAlloc { dst, inner_ty, value } => {
                    // 1) size = sizeof(inner_ty)
                    let sz = c_sizeof_lir_type(&inner_ty, structs) as i64;
                    let size_val = alloc_value(&mut next);
                    new_insts.push(Inst::IConst {
                        dst: size_val,
                        ty: LirType::I64,
                        value: sz,
                    });
                    // 2) dst = __gorget_alloc(size) — dst is the heap ptr.
                    new_insts.push(Inst::CallExtern {
                        dst: Some(dst),
                        name: "__gorget_alloc".to_string(),
                        args: vec![size_val],
                        original_name: None,
                        arg_abis: vec![crate::ir::abi::AbiKind::Scalar],
                    });
                    // 3) Write the value into *dst. Plain `Store` lets each
                    //    backend dispatch on val_types (scalar vs aggregate
                    //    vs Ptr-to-aggregate), matching StructInit/EnumInit.
                    new_insts.push(Inst::Store { ptr: dst, value });
                }
                other => new_insts.push(other),
            }
        }
        func.blocks[bb_idx].insts = new_insts;
    }

    func.set_next_value_raw(next);
}

/// Canonical field-index table for opaque runtime structs.
///
/// Must match the C runtime layouts in `src/backend/c/c_runtime.rs` and
/// `opaque_runtime_size`. Any discrepancy would be a data-layout bug.
fn lookup_named_field_index(struct_name: &str, field_name: &str, structs: &[StructDef]) -> u32 {
    // First, try the struct's LIR fields (for non-opaque types, this is
    // authoritative).
    if let Some(sd) = structs.iter().find(|s| s.name == struct_name) {
        if let Some(idx) = sd.fields.iter().position(|(n, _)| n == field_name) {
            return idx as u32;
        }
    }
    // Opaque runtime types follow the uniform view-discriminator layout:
    //   GorgetString { data, cap, len, alloc }
    //   GorgetArray  { data, cap, len, elem_size, ... }
    // Use the canonical ordering from `docs/internals/thin-pointer-string.md`.
    match (struct_name, field_name) {
        ("GorgetString" | "Str", "data") => 0,
        ("GorgetString" | "Str", "cap") => 1,
        ("GorgetString" | "Str", "len") => 2,
        ("GorgetString" | "Str", "alloc") => 3,
        ("GorgetArray", "data") => 0,
        ("GorgetArray", "cap") => 1,
        ("GorgetArray", "len") => 2,
        ("GorgetArray", "elem_size") => 3,
        _ => 0,
    }
}

/// Cheap scan: true iff any instruction in `func` is a canonical op.
fn func_needs_expansion(func: &LirFunction) -> bool {
    for block in &func.blocks {
        for inst in &block.insts {
            if matches!(
                inst,
                Inst::SizeOf { .. }
                    | Inst::EnumInit { .. }
                    | Inst::EnumCheck { .. }
                    | Inst::EnumExtract { .. }
                    | Inst::StructInit { .. }
                    | Inst::NamedFieldPtr { .. }
                    | Inst::CowClone { .. }
                    | Inst::TraitCall { .. }
                    | Inst::HofExpand { .. }
                    | Inst::AddressOf { .. }
                    | Inst::BoxAlloc { .. }
            ) {
                return true;
            }
        }
    }
    false
}

fn alloc_value(next: &mut u32) -> ValueId {
    let v = ValueId(*next);
    *next += 1;
    v
}

/// Emit either a plain `Store` (scalar) or a `Memcpy` (aggregate) to copy
/// `value` into the memory at `dst_ptr` assuming it holds a T of type `ty`.
///
/// For aggregate-valued payloads, `value` is expected to be a pointer to
/// the source aggregate (this matches how the LIR lifts pass aggregate
/// values around — via slot addresses).
#[allow(dead_code)]
fn emit_store_or_memcpy(
    insts: &mut Vec<Inst>,
    next: &mut u32,
    structs: &[StructDef],
    dst_ptr: ValueId,
    value: ValueId,
    ty: &LirType,
) {
    let sz = c_sizeof_lir_type(ty, structs);
    if ty.is_aggregate() && sz > 0 {
        let size_val = alloc_value(next);
        insts.push(Inst::IConst {
            dst: size_val,
            ty: LirType::I64,
            value: sz as i64,
        });
        insts.push(Inst::Memcpy {
            dst_ptr,
            src_ptr: value,
            size: size_val,
        });
    } else {
        insts.push(Inst::Store { ptr: dst_ptr, value });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::{BlockId, Block, LirFunction, SlotId, StructId, Term};

    fn empty_func() -> LirFunction {
        let mut f = LirFunction::new("test".to_string(), vec![], LirType::Void);
        let bb = f.add_block();
        f.block_mut(bb).terminator = Term::RetVoid;
        f
    }

    #[test]
    fn address_of_spills_then_addresses() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        let bb = BlockId(0);
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: value, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb).insts.push(Inst::AddressOf {
            dst,
            value,
            ty: LirType::I64,
        });
        let slots_before = func.slots.len();
        expand_func(&mut func, &[]);
        // IConst unchanged, AddressOf → SlotStore + SlotAddr.
        let insts = &func.blocks[0].insts;
        assert!(matches!(insts[0], Inst::IConst { .. }));
        assert!(matches!(insts[1], Inst::SlotStore { .. }),
            "expected SlotStore, got {:?}", insts[1]);
        assert!(matches!(insts[2], Inst::SlotAddr { dst: d, .. } if d == dst),
            "expected SlotAddr with dst={:?}, got {:?}", dst, insts[2]);
        assert_eq!(func.slots.len(), slots_before + 1,
            "AddressOf expansion should allocate exactly one fresh slot");
    }

    #[test]
    fn box_alloc_expands_to_size_alloc_store() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        let bb = BlockId(0);
        func.block_mut(bb).insts.push(Inst::IConst {
            dst: value, ty: LirType::I64, value: 42,
        });
        func.block_mut(bb).insts.push(Inst::BoxAlloc {
            dst,
            inner_ty: LirType::I64,
            value,
        });
        expand_func(&mut func, &[]);
        let insts = &func.blocks[0].insts;
        assert!(matches!(insts[0], Inst::IConst { value: 42, .. }));
        assert!(matches!(insts[1], Inst::IConst { ty: LirType::I64, value: 8, .. }),
            "expected sizeof(I64)=8, got {:?}", insts[1]);
        assert!(matches!(&insts[2], Inst::CallExtern { name, .. } if name == "__gorget_alloc"),
            "expected __gorget_alloc CallExtern, got {:?}", insts[2]);
        assert!(matches!(insts[3], Inst::Store { ptr: p, .. } if p == dst),
            "expected Store ptr=dst, got {:?}", insts[3]);
    }

    #[test]
    fn validator_rejects_unexpanded_address_of() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        func.block_mut(BlockId(0)).insts.push(Inst::AddressOf {
            dst,
            value,
            ty: LirType::I64,
        });
        let mut module = crate::lir::LirModule::new();
        module.functions.push(func);
        // Validator should reject unexpanded AddressOf.
        let err = crate::bir::validate::assert_primitives_only(&module);
        assert!(matches!(err, Err(crate::bir::BirError::UnloweredCanonicalOp { opcode: "AddressOf", .. })));
    }

    #[test]
    fn validator_rejects_unexpanded_box_alloc() {
        let mut func = empty_func();
        let value = func.next_value();
        let dst = func.next_value();
        func.block_mut(BlockId(0)).insts.push(Inst::BoxAlloc {
            dst,
            inner_ty: LirType::I64,
            value,
        });
        let mut module = crate::lir::LirModule::new();
        module.functions.push(func);
        let err = crate::bir::validate::assert_primitives_only(&module);
        assert!(matches!(err, Err(crate::bir::BirError::UnloweredCanonicalOp { opcode: "BoxAlloc", .. })));
    }

    // Unused imports in the test module.
    #[allow(dead_code)]
    fn _touch_unused(_: Block, _: SlotId, _: StructId) {}
}
