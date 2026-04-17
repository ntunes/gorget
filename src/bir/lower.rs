//! LIR → BIR lowering pass.
//!
//! Expands LIR's canonical-level high-level ops into sequences of primitive
//! instructions that backends can emit directly. Each canonical op listed in
//! `docs/internals/lir-backend-lift-plan.md` gets an expansion here as the
//! corresponding LIR variant is added.
//!
//! ## Current state (Step 0)
//!
//! No canonical ops exist in LIR yet, so this function is a no-op passthrough.
//! As each op (`SizeOf`, `EnumInit`, `HofExpand`, …) is added in subsequent
//! steps, its expansion logic moves in here.

use crate::bir::BirError;
use crate::lir::LirModule;

/// Expand all canonical-level ops in `module` into primitive instructions.
///
/// Returns the rewritten module. The caller (typically
/// [`crate::bir::BirModule::from_lir`]) then runs the validator to confirm
/// the invariant holds.
pub fn lower_lir_to_bir(module: LirModule) -> Result<LirModule, BirError> {
    // Step 0: no canonical ops in LIR yet — passthrough.
    //
    // Future steps will iterate functions/blocks/insts, match on canonical-op
    // variants, and splice in their primitive expansions (typically: replace
    // one canonical inst with a sequence of `alloca` / `FieldPtr` / `Store` /
    // `CallExtern` / `Branch` primitives, possibly creating new basic blocks
    // for loop bodies in `HofExpand` expansion).
    Ok(module)
}
