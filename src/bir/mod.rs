//! BIR — Backend IR.
//!
//! BIR is the final stage of the Gorget compilation pipeline before backend
//! emission:
//!
//! ```text
//! .gg → AST → GIR → LIR → BIR → machine code
//! ```
//!
//! BIR is a newtype wrapper around [`LirModule`] that **guarantees** any
//! canonical-level high-level ops in LIR (future `HofExpand`, `EnumInit`,
//! `TraitCall`, `StructInit`, `SizeOf`, `NamedFieldPtr`, `CowClone`) have
//! been expanded to primitive instructions. Backends take `&BirModule` and
//! the Rust type system prevents them from ever receiving an unlowered
//! `LirModule`.
//!
//! See `docs/internals/lir-backend-lift-plan.md` for the full design and
//! the migration roadmap.
//!
//! ## Current state (Step 0)
//!
//! The scaffolding exists but no canonical ops have been added to LIR yet,
//! so `lower_lir_to_bir` is a trivial passthrough and
//! `assert_primitives_only` has an empty allowlist of ops-to-reject. As each
//! canonical op lands in LIR (Steps 1-8), it gets a match arm in the
//! validator and an expansion in the lowering pass.

pub mod lower;
pub mod validate;

use crate::lir::LirModule;

/// Errors from lowering LIR to BIR or from validating a module is in BIR form.
#[derive(Debug, Clone)]
pub enum BirError {
    /// A canonical-level op reached the BIR validator without being expanded
    /// by `lower_lir_to_bir`. The op must either be added to the lowering
    /// pass or the validator allowlist.
    UnloweredCanonicalOp {
        fn_name: String,
        block_id: u32,
        opcode: &'static str,
    },
}

impl std::fmt::Display for BirError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BirError::UnloweredCanonicalOp { fn_name, block_id, opcode } => {
                write!(
                    f,
                    "BIR validator: canonical-level op `{opcode}` survived lowering \
                     in function `{fn_name}` (block bb{block_id}); either expand it \
                     in bir::lower or add it to the allowlist in bir::validate"
                )
            }
        }
    }
}

impl std::error::Error for BirError {}

/// Backend IR — a LIR module guaranteed to contain only primitive instructions.
///
/// Construct via [`BirModule::from_lir`]. Backends take `&BirModule` as input
/// so the type system enforces the "dumb backend" contract.
///
/// Deliberately no `Debug`/`Clone` derives — `LirModule` doesn't implement
/// them, and anything that wants to inspect the module should go through
/// [`BirModule::as_lir`] and the LIR display helpers.
pub struct BirModule(LirModule);

impl BirModule {
    /// Lower a LIR module to BIR by expanding canonical ops and validating
    /// that the result contains only primitives.
    pub fn from_lir(module: LirModule) -> Result<Self, BirError> {
        let lowered = lower::lower_lir_to_bir(module)?;
        validate::assert_primitives_only(&lowered)?;
        Ok(BirModule(lowered))
    }

    /// Read-only access to the underlying LIR module. Backends use this for
    /// their 1:1 translation to target source.
    pub fn as_lir(&self) -> &LirModule {
        &self.0
    }

    /// Mutable access, for the rare cases where a backend needs to poke at
    /// module-level metadata (e.g. setting `target` for freestanding builds).
    /// Normal instruction walking should go through `as_lir()`.
    pub fn as_lir_mut(&mut self) -> &mut LirModule {
        &mut self.0
    }

    /// Consume the wrapper and return the underlying module. Use sparingly —
    /// the whole point of the newtype is to preserve the invariant.
    pub fn into_lir(self) -> LirModule {
        self.0
    }
}
