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
        }
    }
}
