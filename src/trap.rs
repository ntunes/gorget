//! The production trap registry (D11 trap normalization).
//!
//! This is the compiler-backend mirror of the *definitional* registry in
//! `ggdef` (`spec/ggdef/src/eval.rs`, `TrapKind`). Both compilers — the
//! executable definition (ggdef) and production (Rust `gg`, C + LLVM) — must
//! agree on the closed set of trap classes and their stable `T_<Variant>`
//! codes. All traps are uncatchable (D25 removed the lexical fault-catch
//! recovery form); conformance compares only the `T_` code + process exit
//! **101** (D11, Q1); the human `detail` line is impl-defined and NEVER
//! compared.
//!
//! ## Why a DUPLICATE of ggdef's `TrapKind`
//! The import ratchet forbids `ggdef` from importing `src/` (definitional
//! independence — the definition must not depend on the implementation), so
//! the two registries are deliberately separate types. The correspondence is
//! pinned by the parity lint `trap_kind_parity_prod_vs_ggdef` in
//! `tests/lints.rs`, which asserts the two `code()` string SETS are identical.
//! A drift on either side trips that lint.
//!
//! ## Why the code is TYPED DATA, not a C-side name table
//! Backend emit sites obtain the `T_` code from [`TrapKind::code`] and thread
//! it as a runtime argument to `gorget_trap_at` (layering rule 2 — "typed
//! metadata, never name-matching"). There is NO C-side table mapping a trap
//! shape to a `T_` string; the only spelled symbol at the C boundary is the
//! fixed runtime entry `gorget_trap_at`.

/// The closed registry of production trap classes (D11). Mirrors ggdef's
/// `TrapKind` variant-for-variant. Unlike ggdef's, the production variants
/// carry NO payload — the human detail string is passed separately as a
/// runtime argument at the emit site, so the variant identity alone fixes the
/// `T_` code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrapKind {
    /// `T_Overflow` — an overflowing checked `+`/`-`/`*`/`/`/`%`/unary-neg, a
    /// signed `TYPE_MIN / -1`, or a shift count out of range (owner ruling
    /// 2026-07-10: shift-out-of-range normalizes to `T_Overflow`).
    Overflow,
    /// `T_DivByZero` — a `/` or `%` with a zero divisor.
    DivByZero,
    /// `T_Bounds` — an out-of-bounds index (production emit is T2b; here for
    /// registry parity with ggdef).
    Bounds,
    /// `T_UnwrapNone` — `.unwrap()` on a `None`.
    UnwrapNone,
    /// `T_UnwrapError` — `.unwrap()` on an `Error`.
    UnwrapError,
    /// `T_UnwrapErrorOnOk` — `.unwrap_error()` on an `Ok`.
    UnwrapErrorOnOk,
    /// `T_AssertFailed` — a failing `assert`.
    AssertFailed,
    /// `T_Panic` — an explicit `panic(msg)`.
    Panic,
}

impl TrapKind {
    /// The stable `T_<VariantName>` code — an exhaustive, catch-all-free match
    /// so `rustc`'s exhaustiveness check IS the registry ratchet (mirrors
    /// `SemanticErrorKind::code`, `src/semantic/errors.rs`, and ggdef's
    /// `TrapKind::code`). Derives from the variant identity alone.
    pub fn code(self) -> &'static str {
        match self {
            TrapKind::Overflow => "T_Overflow",
            TrapKind::DivByZero => "T_DivByZero",
            TrapKind::Bounds => "T_Bounds",
            TrapKind::UnwrapNone => "T_UnwrapNone",
            TrapKind::UnwrapError => "T_UnwrapError",
            TrapKind::UnwrapErrorOnOk => "T_UnwrapErrorOnOk",
            TrapKind::AssertFailed => "T_AssertFailed",
            TrapKind::Panic => "T_Panic",
        }
    }

}
