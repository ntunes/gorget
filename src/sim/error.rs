/// Runtime error types for the GIR interpreter.
#[derive(Debug, Clone)]
pub enum SimError {
    /// gorget_panic() / assert failure.
    Panic(String),
    /// process::exit(n) was called.
    Exit(i32),
    /// Unimplemented runtime function — user should use `gg run` instead.
    Unimplemented(String),
    /// Block with no terminator (compiler bug).
    MissingTerminator(usize),
    /// Terminator::Unreachable was reached.
    Unreachable,
    /// Type mismatch during operation.
    TypeMismatch { expected: String, got: String },
    /// Array/string index out of bounds.
    IndexOutOfBounds { index: i64, len: usize },
    /// Null pointer dereference.
    NullDereference,
    /// Recursion limit exceeded.
    StackOverflow,
    /// Division by zero.
    DivisionByZero,
    /// Integer overflow.
    Overflow,

    // ── UB-detection errors (Phase 4) ────────────────────────────────────────
    /// Read or write through a pointer to already-freed memory.
    UseAfterFree { addr: usize, alloc_fn: String },
    /// Attempt to free memory that was already freed.
    DoubleFree { addr: usize, alloc_fn: String },
    /// Read of a local variable before it was initialized.
    UninitializedRead { local: u32, name: String },
    /// A bool-typed value holds something other than 0 or 1.
    InvalidBoolValue { got: i64 },
    /// An enum tag is outside the valid range for the type.
    InvalidEnumTag { type_name: String, tag: i64 },
    /// One or more heap allocations were not freed before program exit.
    MemoryLeak { count: usize },
    /// Real I/O attempted while isolation mode is active.
    IsolationViolation { operation: String, hint: String },
}

pub type SimResult<T> = Result<T, SimError>;

impl std::fmt::Display for SimError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimError::Panic(msg) => write!(f, "gorget: panic: {msg}"),
            SimError::Exit(code) => write!(f, "gorget: exit({code})"),
            SimError::Unimplemented(name) => {
                write!(f, "gg sim: unimplemented runtime function: {name}\n  hint: use `gg run` to compile and execute natively")
            }
            SimError::MissingTerminator(block) => write!(f, "gorget sim: block {block} has no terminator"),
            SimError::Unreachable => write!(f, "gorget: unreachable code reached"),
            SimError::TypeMismatch { expected, got } => {
                write!(f, "gorget sim: type mismatch: expected {expected}, got {got}")
            }
            SimError::IndexOutOfBounds { index, len } => {
                write!(f, "gorget: index {index} out of bounds (len={len})")
            }
            SimError::NullDereference => write!(f, "gorget: null pointer dereference"),
            SimError::StackOverflow => write!(f, "gorget: stack overflow"),
            SimError::DivisionByZero => write!(f, "gorget: division by zero"),
            SimError::Overflow => write!(f, "gorget: integer overflow"),
            SimError::UseAfterFree { addr, alloc_fn } => {
                write!(f, "gg sim: use-after-free: heap[{addr}] was allocated in {alloc_fn} and already freed")
            }
            SimError::DoubleFree { addr, alloc_fn } => {
                write!(f, "gg sim: double-free: heap[{addr}] (allocated in {alloc_fn}) freed twice")
            }
            SimError::UninitializedRead { local, name } => {
                if name.is_empty() {
                    write!(f, "gg sim: uninitialized read: local _{local} used before assignment")
                } else {
                    write!(f, "gg sim: uninitialized read: `{name}` (_{local}) used before assignment")
                }
            }
            SimError::InvalidBoolValue { got } => {
                write!(f, "gg sim: invalid bool value: {got} (must be 0 or 1)")
            }
            SimError::InvalidEnumTag { type_name, tag } => {
                write!(f, "gg sim: invalid enum tag: {type_name} tag={tag} is out of range")
            }
            SimError::MemoryLeak { count } => {
                write!(f, "gg sim: {count} allocation(s) leaked")
            }
            SimError::IsolationViolation { operation, hint } => {
                write!(f, "gg sim: isolation violation: {operation} blocked\n  hint: {hint}")
            }
        }
    }
}
