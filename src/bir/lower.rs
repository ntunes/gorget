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

    for block in func.blocks.iter_mut() {
        let old = std::mem::take(&mut block.insts);
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
                other => new_insts.push(other),
            }
        }
        block.insts = new_insts;
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
