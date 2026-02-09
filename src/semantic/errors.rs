use crate::span::Span;

/// A semantic analysis error.
#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    /// Name not found in any enclosing scope.
    UndefinedName { name: String },

    /// Same name defined twice in the same scope.
    DuplicateDefinition { name: String, original: Span },

    /// Type checking failure.
    TypeMismatch { expected: String, found: String },

    /// Function call with wrong number of arguments.
    WrongArgCount { expected: usize, found: usize },

    /// Calling something that isn't callable.
    NotAFunction { name: String },

    /// Used as a type but isn't one.
    NotAType { name: String },

    /// Struct literal for something that isn't a struct.
    NotAStruct { name: String },

    /// Trait impl is missing a required method.
    MissingTraitMethod {
        trait_: String,
        method: String,
        type_: String,
    },

    /// Method doesn't exist on type.
    NoMethodFound { method: String, type_: String },

    /// Insufficient info for `auto` type inference.
    CannotInferType,

    /// Field doesn't exist on struct.
    NoFieldFound { field: String, type_: String },

    /// Duplicate trait implementation.
    DuplicateImpl { trait_: String, type_: String },

    /// Method signature doesn't match trait definition.
    MethodSignatureMismatch {
        trait_: String,
        method: String,
        detail: String,
    },

    /// Break/continue outside of loop.
    BreakOutsideLoop,

    /// Return outside of function.
    ReturnOutsideFunction,

    /// Throw in non-throwing function.
    ThrowInNonThrowingFunction,

    // ── Borrow checking errors ──

    /// Variable used after ownership was moved.
    UseAfterMove { name: String, moved_at: Span },

    /// Non-Copy type assigned or passed without `!` move operator.
    MoveWithoutOperator { name: String },

    /// Borrow exclusivity violation.
    BorrowConflict { name: String, detail: String },

    /// Moving a variable inside a loop body.
    MoveInLoop { name: String },

    /// Same variable moved twice.
    DoubleMove { name: String, first_move: Span },

    /// Non-printable type used in string interpolation.
    NonPrintableInterpolation { var_name: String, type_name: String },
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            SemanticErrorKind::UndefinedName { name } => {
                write!(f, "undefined name `{name}`")
            }
            SemanticErrorKind::DuplicateDefinition { name, .. } => {
                write!(f, "duplicate definition of `{name}`")
            }
            SemanticErrorKind::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected `{expected}`, found `{found}`")
            }
            SemanticErrorKind::WrongArgCount { expected, found } => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, found {found}"
                )
            }
            SemanticErrorKind::NotAFunction { name } => {
                write!(f, "`{name}` is not a function")
            }
            SemanticErrorKind::NotAType { name } => {
                write!(f, "`{name}` is not a type")
            }
            SemanticErrorKind::NotAStruct { name } => {
                write!(f, "`{name}` is not a struct")
            }
            SemanticErrorKind::MissingTraitMethod {
                trait_,
                method,
                type_,
            } => {
                write!(
                    f,
                    "type `{type_}` is missing method `{method}` required by trait `{trait_}`"
                )
            }
            SemanticErrorKind::NoMethodFound { method, type_ } => {
                write!(f, "no method `{method}` found on type `{type_}`")
            }
            SemanticErrorKind::CannotInferType => {
                write!(f, "cannot infer type")
            }
            SemanticErrorKind::NoFieldFound { field, type_ } => {
                write!(f, "no field `{field}` found on type `{type_}`")
            }
            SemanticErrorKind::DuplicateImpl { trait_, type_ } => {
                write!(
                    f,
                    "duplicate implementation of trait `{trait_}` for type `{type_}`"
                )
            }
            SemanticErrorKind::MethodSignatureMismatch {
                trait_,
                method,
                detail,
            } => {
                write!(
                    f,
                    "method `{method}` signature doesn't match trait `{trait_}`: {detail}"
                )
            }
            SemanticErrorKind::BreakOutsideLoop => {
                write!(f, "break outside of loop")
            }
            SemanticErrorKind::ReturnOutsideFunction => {
                write!(f, "return outside of function")
            }
            SemanticErrorKind::ThrowInNonThrowingFunction => {
                write!(f, "throw in function that doesn't declare `throws`")
            }
            SemanticErrorKind::UseAfterMove { name, .. } => {
                write!(f, "use of moved value `{name}`")
            }
            SemanticErrorKind::MoveWithoutOperator { name } => {
                write!(
                    f,
                    "cannot copy `{name}`: non-Copy type requires `!` to move"
                )
            }
            SemanticErrorKind::BorrowConflict { name, detail } => {
                write!(f, "borrow conflict on `{name}`: {detail}")
            }
            SemanticErrorKind::MoveInLoop { name } => {
                write!(
                    f,
                    "cannot move `{name}` inside loop: value would be moved on first iteration"
                )
            }
            SemanticErrorKind::DoubleMove { name, .. } => {
                write!(f, "value `{name}` moved more than once")
            }
            SemanticErrorKind::NonPrintableInterpolation {
                var_name,
                type_name,
            } => {
                write!(
                    f,
                    "cannot interpolate `{var_name}` of type `{type_name}` in string"
                )
            }
        }
    }
}
