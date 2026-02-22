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

    /// `?` on Result in a function that doesn't return Result.
    TryOnResultInNonResultFunction,

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

    /// Call-site ownership annotation doesn't match parameter declaration.
    OwnershipMismatch {
        param_name: String,
        expected: String,
        found: String,
    },

    /// Generic type argument does not satisfy a `where` clause trait bound.
    UnsatisfiedTraitBound {
        type_name: String,
        trait_name: String,
        param_name: String,
    },

    /// Match expression is not exhaustive — some enum variants are not covered.
    NonExhaustiveMatch { missing_variants: Vec<String> },

    /// Named argument doesn't match any parameter.
    UnknownNamedArg { name: String },

    /// Same named argument passed twice.
    DuplicateNamedArg { name: String },

    /// Required parameter not provided (no default value).
    MissingRequiredArg { name: String },

    /// Positional argument follows a named argument.
    PositionalAfterNamed,

    /// Unknown directive name.
    UnknownDirective { name: String },

    /// Trait cannot be derived for this type.
    UnderivableTrait { trait_name: String, type_name: String },

    /// `@derive(From)` requires exactly one field (newtype pattern).
    DeriveFromRequiresSingleField { type_name: String },

    /// Assignment to an immutable variable (under `directive immutable-by-default`).
    AssignmentToImmutable { name: String },

    /// Assignment to a const binding (always an error).
    AssignmentToConst { name: String },

    /// `via` used without a trait in equip block.
    ViaWithoutTrait,

    /// `via` field does not exist on the struct.
    ViaFieldNotFound { field: String, type_: String },

    /// `via` field's type does not implement the target trait.
    ViaFieldTypeMissingTrait { field: String, field_type: String, trait_: String },

    /// Duplicate suite setup or teardown block.
    DuplicateSuiteBlock { kind: String },

    /// Callable/MutCallable/ConsumeCallable[...] requires a function type argument.
    InvalidFnTraitArg,

    /// Closure kind doesn't match the expected callable trait.
    ClosureKindMismatch { expected: String, found: String },

    /// Integer literal value doesn't fit in the declared type.
    ValueOutOfRange {
        value: i128,
        type_name: String,
        min: i128,
        max: i128,
    },

    // ── Lifetime errors ──

    /// Returning a reference to a local variable (would dangle after return).
    DanglingReturn { name: String, local_name: String, local_declared_at: Option<Span> },

    /// Using a reference-type variable after its source has been moved.
    UseAfterSourceMoved { name: String, source_name: String, moved_at: Span },

    /// Bodyless function returning a reference type with no `live` params.
    MissingLiveAnnotation { func_name: String },

    /// Binding a reference type to a temporary that will be immediately dropped.
    TemporaryBorrow { name: String, callee: String, temp_at: Option<Span> },

    /// `where a outlives b` violated: group a's source was invalidated
    /// while group b's source is still alive.
    OutlivesViolation {
        longer_group: String,
        shorter_group: String,
        longer_source: String,
        shorter_source: String,
    },

    /// Parameter mode (`&` or `!`) is invalid for this type (e.g. `&str`, `!str`).
    InvalidParameterMode {
        param_name: String,
        type_name: String,
        mode: String,
    },

    /// Returning a value whose borrow origin could not be determined.
    UnresolvedBorrowOrigin { name: String },
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
            SemanticErrorKind::TryOnResultInNonResultFunction => {
                write!(f, "`?` on Result requires enclosing function to return Result")
            }
            SemanticErrorKind::UseAfterMove { name, .. } => {
                write!(f, "use of moved value `{name}`")
            }
            SemanticErrorKind::MoveWithoutOperator { name } => {
                write!(
                    f,
                    "cannot copy `{name}`: non-Copy type requires `!` or `consuming` to transfer"
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
            SemanticErrorKind::OwnershipMismatch {
                param_name,
                expected,
                found,
            } => {
                write!(
                    f,
                    "ownership mismatch for `{param_name}`: expected `{expected}`, found `{found}`"
                )
            }
            SemanticErrorKind::UnsatisfiedTraitBound {
                type_name,
                trait_name,
                param_name,
            } => {
                write!(
                    f,
                    "type `{type_name}` does not satisfy trait bound `{param_name} is {trait_name}`"
                )
            }
            SemanticErrorKind::NonExhaustiveMatch { missing_variants } => {
                write!(
                    f,
                    "non-exhaustive match: missing variants: {}",
                    missing_variants.join(", ")
                )
            }
            SemanticErrorKind::UnknownNamedArg { name } => {
                write!(f, "unknown named argument `{name}`")
            }
            SemanticErrorKind::DuplicateNamedArg { name } => {
                write!(f, "duplicate named argument `{name}`")
            }
            SemanticErrorKind::MissingRequiredArg { name } => {
                write!(f, "missing required argument `{name}`")
            }
            SemanticErrorKind::PositionalAfterNamed => {
                write!(f, "positional argument cannot follow named argument")
            }
            SemanticErrorKind::UnknownDirective { name } => {
                write!(f, "unknown directive `{name}`")
            }
            SemanticErrorKind::UnderivableTrait { trait_name, type_name } => {
                write!(f, "cannot derive `{trait_name}` for `{type_name}`")
            }
            SemanticErrorKind::DeriveFromRequiresSingleField { type_name } => {
                write!(f, "`@derive(From)` on `{type_name}` requires exactly one field")
            }
            SemanticErrorKind::AssignmentToImmutable { name } => {
                write!(f, "cannot assign to immutable variable `{name}` (add `mutable` to declaration)")
            }
            SemanticErrorKind::AssignmentToConst { name } => {
                write!(f, "cannot assign to constant `{name}`")
            }
            SemanticErrorKind::ViaWithoutTrait => {
                write!(f, "`via` delegation can only be used in trait equip blocks")
            }
            SemanticErrorKind::ViaFieldNotFound { field, type_ } => {
                write!(f, "`via` field `{field}` not found on type `{type_}`")
            }
            SemanticErrorKind::ViaFieldTypeMissingTrait { field, field_type, trait_ } => {
                write!(f, "`via` field `{field}` of type `{field_type}` does not implement trait `{trait_}`")
            }
            SemanticErrorKind::DuplicateSuiteBlock { kind } => {
                write!(f, "duplicate `suite {kind}` block")
            }
            SemanticErrorKind::InvalidFnTraitArg => {
                write!(f, "Callable[...] requires a function type argument, e.g. Callable[int(int)]")
            }
            SemanticErrorKind::ClosureKindMismatch { expected, found } => {
                write!(f, "closure kind mismatch: expected `{expected}`, found `{found}`")
            }
            SemanticErrorKind::ValueOutOfRange { value, type_name, min, max } => {
                write!(f, "value {value} is out of range for type {type_name} (valid range: {min}..={max})")
            }
            SemanticErrorKind::DanglingReturn { name, local_name, .. } => {
                write!(f, "cannot return `{name}`: borrows from local variable `{local_name}` which will be dropped")
            }
            SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. } => {
                write!(f, "use of `{name}` after source `{source_name}` was moved")
            }
            SemanticErrorKind::MissingLiveAnnotation { func_name } => {
                write!(f, "function `{func_name}` returns a reference type but has no `live` parameter annotations")
            }
            SemanticErrorKind::TemporaryBorrow { name, callee, .. } => {
                write!(f, "cannot bind `{name}` to temporary from `{callee}()` — value will be dropped")
            }
            SemanticErrorKind::OutlivesViolation { longer_group, shorter_group, longer_source, shorter_source } => {
                write!(f, "borrow group `{longer_group}` must outlive `{shorter_group}`, but `{longer_source}` (group `{longer_group}`) was moved while `{shorter_source}` (group `{shorter_group}`) is still alive")
            }
            SemanticErrorKind::InvalidParameterMode { param_name, type_name, mode } => {
                write!(f, "parameter `{param_name}` of type `{type_name}` cannot use `{mode}` mode — `{type_name}` is Copy and always passed by value")
            }
            SemanticErrorKind::UnresolvedBorrowOrigin { name } => {
                write!(f, "cannot return `{name}`: borrow origin could not be determined — consider adding `live` annotation to the source function")
            }
        }
    }
}
