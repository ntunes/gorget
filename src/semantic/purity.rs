//! Purity inference for Gorget functions.
//!
//! Automatically infers function purity — whether a function has side effects —
//! with zero annotations. Computed during borrow checking (Pass 5) alongside
//! lifetime and ownership analysis.
//!
//! Purity levels (from purest to most effectful):
//! - Pure: reads only its args, calls only pure functions, no globals/IO
//! - ReadOnly: reads globals but doesn't mutate them, no IO
//! - MutatesArgs: may mutate & or ! parameters but no globals/IO
//! - HasSideEffects: anything else (IO, global mutation, shared access)

use rustc_hash::FxHashMap;

use super::ids::DefId;

/// Purity level of a function, inferred automatically.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Purity {
    /// Reads only its args, calls only pure functions, no globals/IO.
    Pure,
    /// Reads globals but doesn't mutate them, no IO.
    ReadOnly,
    /// May mutate &/! parameters but no globals/IO.
    MutatesArgs,
    /// IO, global mutation, shared access, or unknown callees.
    HasSideEffects,
}

impl Purity {
    /// Combine two purity levels, returning the least pure.
    pub fn join(self, other: Purity) -> Purity {
        std::cmp::max(self, other)
    }
}

impl std::fmt::Display for Purity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Purity::Pure => write!(f, "pure"),
            Purity::ReadOnly => write!(f, "read_only"),
            Purity::MutatesArgs => write!(f, "mutates_args"),
            Purity::HasSideEffects => write!(f, "has_side_effects"),
        }
    }
}

/// Accumulates purity observations during function body analysis.
#[derive(Debug, Clone)]
pub struct PurityAccumulator {
    /// Current worst-case purity level.
    level: Purity,
}

impl PurityAccumulator {
    pub fn new() -> Self {
        Self { level: Purity::Pure }
    }

    /// Record that the function reads a global variable.
    pub fn reads_global(&mut self) {
        self.level = self.level.join(Purity::ReadOnly);
    }

    /// Record that the function writes to a global variable.
    pub fn writes_global(&mut self) {
        self.level = self.level.join(Purity::HasSideEffects);
    }

    /// Record that the function mutates a & or ! parameter.
    pub fn mutates_param(&mut self) {
        self.level = self.level.join(Purity::MutatesArgs);
    }

    /// Record that the function calls an extern/unknown function (IO).
    pub fn calls_extern(&mut self) {
        self.level = self.level.join(Purity::HasSideEffects);
    }

    /// Record that the function calls another function with known purity.
    pub fn calls_function(&mut self, callee_purity: Purity) {
        self.level = self.level.join(callee_purity);
    }

    /// Record that the function accesses a shared variable.
    pub fn accesses_shared(&mut self) {
        self.level = self.level.join(Purity::HasSideEffects);
    }

    /// Get the final purity level.
    pub fn finish(self) -> Purity {
        self.level
    }
}

/// Map of function DefId → inferred Purity.
pub type PurityMap = FxHashMap<DefId, Purity>;

/// Map of function name → inferred Purity (for IR/backend consumption).
pub type PurityByName = FxHashMap<String, Purity>;
