//! ABI marshalling kinds for extern function parameters.
//!
//! When Gorget code calls an external function, each parameter must be
//! marshalled from the Gorget internal representation to the extern ABI.
//! `AbiKind` describes how each parameter crosses that boundary.
//!
//! This is backend-agnostic: the C backend, a future LLVM backend, and
//! a WASM backend all read the same `AbiKind` tags and emit their own
//! marshalling code.

/// How a parameter value is marshalled across the extern ABI boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AbiKind {
    /// Default — backend uses current behavior (whitelists). Migration shim.
    /// Once all extern functions are annotated, this variant is removed.
    #[default]
    Auto,
    /// Null-terminated byte pointer. Compiler extracts `.data` from String
    /// and ensures null termination (via `gorget_str_to_cstr` if needed).
    /// C: `const char*`, LLVM: `i8*`, WASM: `i32` memory offset.
    CStr,
    /// Raw byte pointer without null-termination guarantee. Compiler extracts
    /// `.data` only. Used for binary protocols and length-prefixed APIs.
    /// C: `const char*`, LLVM: `i8*`, WASM: `i32` memory offset.
    BytePtr,
    /// Full Gorget string struct by value. Compiler derefs CoW pointer and
    /// loads the complete struct. Callee understands Gorget's string layout.
    /// C: `Str` (32-byte struct), LLVM: `%Str`, WASM: struct in linear memory.
    GorgetString,
    /// Scalar value (int, float, bool). Passed by value, no transformation.
    Scalar,
    /// Typed pointer to aggregate. Passed directly as a pointer.
    /// Used for collection self-by-ptr, array/map element args, crypto structs.
    Ptr,
    /// Opaque handle. Passed as-is with no transformation.
    /// Used for opaque types (Regex, Window, Database handles).
    Opaque,
}
