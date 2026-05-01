//! BIR validator — asserts a lowered module contains only primitive ops.
//!
//! Runs at the end of [`crate::bir::BirModule::from_lir`]. If a canonical
//! op survived lowering, returns [`BirError::UnloweredCanonicalOp`] pointing
//! to the offending function / block / opcode so the author can either fix
//! the expansion in `bir::lower` or (in the rare case) add the op to the
//! primitives allowlist here.
//!
//! ## Design
//!
//! The match has two arms:
//!
//! - **Catch-all for primitives** (`_ => continue`) — anything not explicitly
//!   flagged as a canonical op is primitive by default. This keeps the
//!   validator cheap to maintain: adding a new primitive requires zero
//!   validator changes; adding a new canonical op requires exactly one.
//!
//! - **Explicit canonical-op arms** — one match arm per canonical op, each
//!   returning the `UnloweredCanonicalOp` error. Deleting one of these arms
//!   (after adding its primitive expansion in `bir::lower`) is how a
//!   canonical op graduates from "must lower" to "not a valid LIR op anymore."
//!
//! ## Current state (Step 0)
//!
//! No canonical ops exist in LIR yet, so this validator currently accepts
//! every LIR instruction. The framework is in place for Steps 1-8 to plug
//! canonical-op arms into the match.

use crate::bir::BirError;
use crate::lir::{Inst, LirModule};

/// Assert every instruction in `module` is a primitive op (no canonical
/// high-level ops remain).
///
/// Returns the first offender found, or `Ok(())` if the module is pure BIR.
pub fn assert_primitives_only(module: &LirModule) -> Result<(), BirError> {
    for func in &module.functions {
        for block in &func.blocks {
            for inst in &block.insts {
                check_inst(inst, &func.name, block.id.0)?;
            }
        }
    }
    Ok(())
}

fn check_inst(inst: &Inst, fn_name: &str, block_id: u32) -> Result<(), BirError> {
    // Canonical-op arms live here. As each canonical op is added to `Inst`
    // (Steps 1-8), add a match arm that returns `UnloweredCanonicalOp`.
    //
    // All other variants fall through the catch-all below — they're primitives.
    match inst {
        // === Canonical-op arms ===
        Inst::SizeOf { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "SizeOf",
        }),
        Inst::EnumInit { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "EnumInit",
        }),
        Inst::EnumCheck { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "EnumCheck",
        }),
        Inst::EnumExtract { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "EnumExtract",
        }),
        Inst::StructInit { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "StructInit",
        }),
        Inst::CowClone { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "CowClone",
        }),
        Inst::TraitCall { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "TraitCall",
        }),
        Inst::HofExpand { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "HofExpand",
        }),
        Inst::AddressOf { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "AddressOf",
        }),
        Inst::BoxAlloc { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "BoxAlloc",
        }),
        Inst::CollectionCtor { .. } => Err(BirError::UnloweredCanonicalOp {
            fn_name: fn_name.to_string(),
            block_id,
            opcode: "CollectionCtor",
        }),

        // === Primitives — the catch-all (default) ===
        _ => Ok(()),
    }
}
