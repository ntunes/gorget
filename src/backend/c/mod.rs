/// C runtime definitions shared by all C-targeting backends.
///
/// The GIR→C codegen backend was removed — all compilation now goes through LIR.
/// This module exists only to host `c_runtime`, which the LIR C backend references.
pub mod c_runtime;
