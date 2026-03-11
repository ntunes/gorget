pub mod c;
pub mod c_lir;

use crate::lir::LirModule;

/// Output from a backend code generation pass.
pub struct CodegenOutput {
    /// The generated source code (C, LLVM IR, WASM text, etc.).
    pub code: String,
    /// File extension for the output (e.g., "c", "ll", "wat").
    pub extension: &'static str,
}

/// Trait for LIR-consuming backends.
///
/// All backends consume an optimized `LirModule` and produce source code
/// that can be compiled to a binary by an external toolchain.
pub trait Backend {
    /// Human-readable backend name (e.g., "c-lir", "llvm").
    fn name(&self) -> &str;

    /// Generate source code from an LIR module.
    fn generate(&self, module: &LirModule) -> CodegenOutput;
}
