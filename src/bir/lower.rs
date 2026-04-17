//! LIR → BIR lowering pass.
//!
//! Expands LIR's canonical-level high-level ops into sequences of primitive
//! instructions that backends can emit directly. Each canonical op listed in
//! `docs/internals/lir-backend-lift-plan.md` gets an expansion here as the
//! corresponding LIR variant is added.
//!
//! ## Current state (Step 3)
//!
//! Lowers `Inst::SizeOf { dst, ty }` into `Inst::IConst { dst, ty: I64, value }`
//! using the shared `c_sizeof_lir_type` table (which consults
//! `opaque_runtime_size` for monomorphized runtime types). This centralizes
//! the sizeof lookup so backends never see the symbolic `SizeOf` — they only
//! see a resolved integer constant, same as before the op existed.
//!
//! Future steps (4 onwards) expand `EnumInit` / `StructInit` / `HofExpand` /
//! etc. in the same walk.

use crate::bir::BirError;
use crate::lir::lower::types::c_sizeof_lir_type;
use crate::lir::{Inst, LirModule, LirType};

/// Expand all canonical-level ops in `module` into primitive instructions.
///
/// Returns the rewritten module. The caller (typically
/// [`crate::bir::BirModule::from_lir`]) then runs the validator to confirm
/// the invariant holds.
pub fn lower_lir_to_bir(mut module: LirModule) -> Result<LirModule, BirError> {
    // Step 3: resolve `Inst::SizeOf { dst, ty }` to `Inst::IConst { dst, I64, value }`.
    // Performed in-place so downstream passes see the same ValueId mapping.
    let structs_snapshot = module.structs.clone();
    for func in module.functions.iter_mut() {
        for block in func.blocks.iter_mut() {
            for inst in block.insts.iter_mut() {
                if let Inst::SizeOf { dst, ty } = inst {
                    let value = c_sizeof_lir_type(ty, &structs_snapshot) as i64;
                    *inst = Inst::IConst { dst: *dst, ty: LirType::I64, value };
                }
            }
        }
    }
    Ok(module)
}
