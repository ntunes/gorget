use crate::span::Span;

/// A semantic analysis error.
#[derive(Debug, Clone)]
pub struct SemanticError {
    pub kind: SemanticErrorKind,
    pub span: Span,
}

/// A semantic analysis warning (non-fatal).
#[derive(Debug, Clone)]
pub struct SemanticWarning {
    pub kind: SemanticWarningKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SemanticWarningKind {
    /// A `shared` binding never crosses a concurrency boundary.
    UnnecessaryShared { name: String },
    /// A local derived from a shared binding is used in a branch condition
    /// after an await point (stale check-then-act pattern).
    StaleSharedCondition {
        local_name: String,
        shared_name: String,
        /// Where the local was derived from the shared variable.
        derivation_span: Option<Span>,
        /// The await point that made the cached value stale.
        await_span: Option<Span>,
    },
    /// A yield point occurs inside a branch whose condition depends on a
    /// `with`-tracked shared variable — check-then-act race.
    WithCheckThenAct {
        /// The shared variable names used in the condition.
        shared_names: Vec<String>,
        /// Span of the condition expression.
        condition_span: Span,
        /// The yield point inside the branch body.
        yield_span: Span,
    },
    /// A stale value derived from a shared variable is written back to a
    /// shared variable after a yield — lost update.
    StaleSharedWriteBack {
        /// The local whose value is stale.
        local_name: String,
        /// The shared variable the local was derived from.
        source_shared_name: String,
        /// The shared variable being written to.
        target_shared_name: String,
        /// Where the local was derived from the shared variable.
        derivation_span: Option<Span>,
        /// The yield point that made the value stale.
        yield_span: Option<Span>,
    },
    /// A yield point inside a `for` loop iterating over a `with`-tracked
    /// shared collection — the iterator may be invalidated.
    SharedIteratorInvalidation {
        shared_name: String,
        iterable_span: Span,
        yield_span: Span,
    },
    /// A `with`-tracked binding is passed to a `spawn` call — the spawned
    /// task operates outside the `with` block's lock scope.
    SpawnWithTrackedBinding {
        shared_name: String,
        spawn_span: Span,
    },
    /// A compound assignment reads a shared variable before a yield point
    /// in the same expression, then writes the result back — TOCTOU race.
    CompoundYieldRace {
        shared_name: String,
        yield_span: Span,
    },
    /// A closure inside a `with` block captures a `with`-tracked binding.
    /// If a yield occurs between creation and invocation, the captured value is stale.
    ClosureCapturesWithBinding {
        var_name: String,
    },
    /// Statement after unconditional return/break/continue/throw.
    UnreachableCode,
    /// Local variable declared but never read.
    UnusedVariable { name: String },
    /// Imported name never referenced in code.
    UnusedImport { name: String },
    /// `.unwrap()` or `.expect()` called on Option/Result without prior guard.
    UncheckedUnwrap { name: String, type_name: String },
    /// Copy-type variable never reassigned — could be `const`.
    CouldBeConst { name: String },
    /// `&` parameter is never mutated in the function body.
    NeedlessMutableBorrow { name: String },
    /// Collection mutated while an implicit CoW borrow (from .get/.unwrap/index) is alive.
    /// The CoW system handles correctness, but the pattern may be confusing.
    CowBorrowMutation { source: String, borrow: String },
}

impl std::fmt::Display for SemanticWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            SemanticWarningKind::UnnecessaryShared { name } => {
                write!(f, "variable `{name}` is declared `shared` but never crosses a concurrency boundary")
            }
            SemanticWarningKind::StaleSharedCondition { local_name, shared_name, .. } => {
                write!(f, "`{local_name}` derived from shared `{shared_name}` may be stale after await")
            }
            SemanticWarningKind::WithCheckThenAct { shared_names, .. } => {
                let names = shared_names.iter().map(|n| format!("`{n}`")).collect::<Vec<_>>().join(", ");
                write!(f, "yield inside branch guarded by shared {names} — condition may no longer hold")
            }
            SemanticWarningKind::StaleSharedWriteBack { local_name, target_shared_name, .. } => {
                write!(f, "writing stale `{local_name}` to shared `{target_shared_name}` — lost update after yield")
            }
            SemanticWarningKind::SharedIteratorInvalidation { shared_name, .. } => {
                write!(f, "yield inside loop over shared `{shared_name}` — iterator may be invalidated")
            }
            SemanticWarningKind::SpawnWithTrackedBinding { shared_name, .. } => {
                write!(f, "spawning task with `with`-tracked `{shared_name}` — spawned task runs outside `with` lock scope")
            }
            SemanticWarningKind::CompoundYieldRace { shared_name, .. } => {
                write!(f, "compound assignment reads shared `{shared_name}` before yield and writes after — race condition")
            }
            SemanticWarningKind::ClosureCapturesWithBinding { var_name } => {
                write!(f, "closure captures `with`-tracked `{var_name}` — captured value may become stale after yield")
            }
            SemanticWarningKind::UnreachableCode => {
                write!(f, "unreachable code after diverging statement")
            }
            SemanticWarningKind::UnusedVariable { name } => {
                write!(f, "unused variable `{name}` — prefix with `_` to suppress")
            }
            SemanticWarningKind::UnusedImport { name } => {
                write!(f, "unused import `{name}`")
            }
            SemanticWarningKind::UncheckedUnwrap { name, type_name } => {
                write!(f, "calling `unwrap()` on `{name}` of type `{type_name}` without checking for None/Error first")
            }
            SemanticWarningKind::CouldBeConst { name } => {
                write!(f, "variable `{name}` is never reassigned — consider `const`")
            }
            SemanticWarningKind::NeedlessMutableBorrow { name } => {
                write!(f, "parameter `&{name}` is never mutated — consider removing `&`")
            }
            SemanticWarningKind::CowBorrowMutation { source, borrow } => {
                write!(f, "`{source}` mutated while `{borrow}` holds an element — clone is inserted automatically")
            }
        }
    }
}

/// How an arena-scoped value escapes its `with` block.
#[derive(Debug, Clone)]
pub enum ArenaEscapeKind {
    Return,
    AssignOuter { target: String },
}

#[derive(Debug, Clone)]
pub enum SemanticErrorKind {
    /// Name not found in any enclosing scope.
    UndefinedName { name: String, suggestion: Option<String> },

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

    /// A method-level generic param couldn't be inferred from the
    /// call's arg types. Emitted by Phase 2c inference (see
    /// `docs/internals/method-level-inference.md`) instead of the
    /// historical silent fallback to `NoMethodFound` /
    /// `WrongArgCount` / a link error. `unresolved` is the generic
    /// param name from the method's `[T1, T2, ...]` clause; `reason`
    /// is a short descriptor of why inference failed (no candidate,
    /// ambiguous, arg-typed-as-error, return-type-not-projectable).
    MethodGenericInferenceFailed {
        method: String,
        type_: String,
        unresolved: String,
        reason: String,
    },

    /// Insufficient info for `auto` type inference.
    CannotInferType,

    /// Field doesn't exist on struct.
    NoFieldFound { field: String, type_: String },

    /// Tuple field index out of bounds.
    TupleIndexOutOfBounds { index: usize, len: usize },

    /// Or-pattern alternatives bind different names.
    OrPatternBindingMismatch { missing: Vec<String>, extra: Vec<String> },

    /// Duplicate trait implementation.
    DuplicateImpl { trait_: String, type_: String },

    /// Cyclic trait inheritance (e.g. `trait A extends B` + `trait B extends A`).
    TraitCycle { trait_: String, cycle: String },

    /// Method signature doesn't match trait definition.
    MethodSignatureMismatch {
        trait_: String,
        method: String,
        detail: String,
    },

    /// Break outside of loop.
    BreakOutsideLoop,

    /// Continue outside of loop.
    ContinueOutsideLoop,

    /// Return outside of function.
    ReturnOutsideFunction,

    /// Throw in non-throwing function.
    ThrowInNonThrowingFunction,

    /// `rethrow` in non-throwing function.
    RethrowInNonThrowingFunction,

    /// `on error` in non-throwing function.
    OnErrorInNonThrowingFunction,

    /// `main()` can only throw `int` (the process exit code).
    MainThrowsNonInt,

    /// `await` used outside an `async` function.
    AwaitOutsideAsync,

    /// `select` used outside an `async` function.
    SelectOutsideAsync,

    /// `await` applied to a non-`Future[T]` value.
    AwaitNonFuture,

    /// `spawn` applied to a non-`Future[T]` value.
    SpawnNonFuture,

    /// Reference-typed variable used across an `await` suspension point.
    BorrowAcrossAwait { name: String },

    /// Borrowed reference passed as argument to `spawn` (fire-and-forget).
    /// Use `shared` declaration or explicit `Shared[T]` instead.
    SpawnWithBorrowedRef { name: Option<String> },

    /// `spawn` used with something other than a direct function call.
    /// Only `spawn fn_name(args)` is supported.
    SpawnRequiresDirectCall,

    /// Closure passed to `spawn` captures a borrowed variable.
    SpawnClosureCaptureBorrowed { var_name: String },

    /// Closure passed to `spawn` captures a variable mutably — the mutable
    /// capture stores a pointer to the parent stack frame, unsafe across threads.
    SpawnClosureCaptureMutable { var_name: String },

    /// Closure passed to `spawn` captures a `shared` keyword binding.
    /// The closure captures the facade local (plain value), not the underlying
    /// ARC+Mutex handle. Use a direct spawn argument instead.
    SpawnClosureCaptureShared { var_name: String },

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

    /// A field's type doesn't implement the trait required by @derive.
    FieldMissingDerivedTrait { struct_name: String, field_type: String, trait_name: String },

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

    /// Integer conversion that may lose data or sign information.
    UnsafeIntegerConversion {
        from: String,
        to: String,
    },

    // ── Lifetime errors ──

    /// Returning a reference to a local variable (would dangle after return).
    DanglingReturn { name: String, local_name: String, local_declared_at: Option<Span> },

    /// Using a reference-type variable after its source has been moved.
    UseAfterSourceMoved { name: String, source_name: String, moved_at: Span },

    /// Mutating a collection while an outstanding borrow exists.
    MutationWhileBorrowed { source: String, borrow: String },

    /// Binding a reference type to a temporary that will be immediately dropped.
    TemporaryBorrow { name: String, callee: String, temp_at: Option<Span> },

    /// Parameter mode (`&` or `!`) is invalid for this type (e.g. `&str`, `!str`).
    InvalidParameterMode {
        param_name: String,
        type_name: String,
        mode: String,
    },

    /// Returning a value whose borrow origin could not be determined.
    UnresolvedBorrowOrigin { name: String },

    // ── Arena escape errors ──

    /// Arena-scoped value escaping its `with` block.
    ArenaEscape { name: String, kind: ArenaEscapeKind },

    /// Compile-time meta evaluation error.
    MetaEvalError { message: String },

    /// Orphan rule violation: neither the trait nor the type is defined locally.
    OrphanImpl { trait_: String, type_: String },

    /// `await expr.await()` — expression is awaited twice.
    DoubleAwait,

    /// Variable read while mutably captured by a live closure.
    ReadWhileMutCaptured { var_name: String, closure_name: String },

    /// Variable written while mutably captured by a live closure.
    WriteWhileMutCaptured { var_name: String, closure_name: String },

    /// Attempt to import a private item from a module.
    PrivateImport { name: String, module: String },

    /// Public function exposes a private type in its signature.
    PrivateTypeInPublicSignature { type_name: String, fn_name: String, position: String },

    /// Required parameter follows a parameter with a default value.
    RequiredAfterDefault { name: String },

    /// Duplicate field in struct literal.
    DuplicateStructField { field: String },

    /// Wrong number of fields in struct literal.
    WrongFieldCount { type_: String, expected: usize, found: usize },

    /// Returning a closure that captures a local variable (use-after-free).
    ClosureEscapesScope { closure_name: String, captured_name: String },
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            SemanticErrorKind::UndefinedName { name, suggestion } => {
                write!(f, "undefined name `{name}`")?;
                if let Some(s) = suggestion {
                    write!(f, "; did you mean `{s}`?")?;
                }
                Ok(())
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
            SemanticErrorKind::MethodGenericInferenceFailed { method, type_, unresolved, reason } => {
                write!(
                    f,
                    "could not infer method-level generic `{unresolved}` for `{type_}.{method}` ({reason}); \
                     pass it explicitly via `{method}[<types>](...)`"
                )
            }
            SemanticErrorKind::CannotInferType => {
                write!(f, "cannot infer type")
            }
            SemanticErrorKind::NoFieldFound { field, type_ } => {
                write!(f, "no field `{field}` found on type `{type_}`")
            }
            SemanticErrorKind::TupleIndexOutOfBounds { index, len } => {
                write!(f, "tuple index `{index}` out of bounds for tuple with {len} elements")
            }
            SemanticErrorKind::OrPatternBindingMismatch { missing, extra } => {
                let mut parts = Vec::new();
                if !missing.is_empty() {
                    parts.push(format!("missing: {}", missing.join(", ")));
                }
                if !extra.is_empty() {
                    parts.push(format!("extra: {}", extra.join(", ")));
                }
                write!(f, "or-pattern alternatives must bind the same names ({})", parts.join("; "))
            }
            SemanticErrorKind::DuplicateImpl { trait_, type_ } => {
                if trait_ == "(inherent)" {
                    write!(f, "duplicate inherent impl block for type `{type_}`")
                } else {
                    write!(
                        f,
                        "duplicate implementation of trait `{trait_}` for type `{type_}`"
                    )
                }
            }
            SemanticErrorKind::TraitCycle { trait_, cycle } => {
                write!(f, "trait `{trait_}` has a cyclic inheritance: {cycle}")
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
            SemanticErrorKind::ContinueOutsideLoop => {
                write!(f, "continue outside of loop")
            }
            SemanticErrorKind::ReturnOutsideFunction => {
                write!(f, "return outside of function")
            }
            SemanticErrorKind::ThrowInNonThrowingFunction => {
                write!(f, "throw in function that doesn't declare `throws`")
            }
            SemanticErrorKind::RethrowInNonThrowingFunction => {
                write!(f, "rethrow in function that doesn't declare `throws`")
            }
            SemanticErrorKind::OnErrorInNonThrowingFunction => {
                write!(f, "`on error` in function that doesn't declare `throws`")
            }
            SemanticErrorKind::MainThrowsNonInt => {
                write!(f, "`main()` can only throw `int` (the process exit code)")
            }
            SemanticErrorKind::AwaitOutsideAsync => {
                write!(f, "`await` can only be used inside an `async` function")
            }
            SemanticErrorKind::SelectOutsideAsync => {
                write!(f, "`select` can only be used inside an `async` function")
            }
            SemanticErrorKind::AwaitNonFuture => {
                write!(f, "`await` requires a `Future[T]` value")
            }
            SemanticErrorKind::SpawnNonFuture => {
                write!(f, "`spawn` requires a `Future[T]` value (an async function call)")
            }
            SemanticErrorKind::BorrowAcrossAwait { name } => {
                write!(f, "cannot use reference `{name}` across `await` — move owned data instead")
            }
            SemanticErrorKind::SpawnWithBorrowedRef { name } => {
                if let Some(n) = name {
                    write!(f, "cannot pass borrowed reference '{n}' to spawned task — declare as `shared` for automatic synchronization, or use explicit Shared[T]")
                } else {
                    write!(f, "cannot pass borrowed reference to spawned task — declare as `shared` for automatic synchronization, or use explicit Shared[T]")
                }
            }
            SemanticErrorKind::SpawnRequiresDirectCall => {
                write!(f, "spawn requires a direct function call — use `spawn fn_name(args)`")
            }
            SemanticErrorKind::SpawnClosureCaptureBorrowed { var_name } => {
                write!(f, "cannot spawn closure that captures borrowed variable `{var_name}` — use owned or Copy types")
            }
            SemanticErrorKind::SpawnClosureCaptureMutable { var_name } => {
                write!(f, "cannot spawn closure that mutably captures `{var_name}` — mutable captures hold pointers to the parent stack")
            }
            SemanticErrorKind::SpawnClosureCaptureShared { var_name } => {
                write!(f, "cannot capture shared variable `{var_name}` in spawned closure — pass it as a direct spawn argument instead: `spawn fn_name({var_name})`")
            }
            SemanticErrorKind::UseAfterMove { name, .. } => {
                write!(f, "use of moved value `{name}`")
            }
            SemanticErrorKind::MoveWithoutOperator { name } => {
                write!(
                    f,
                    "cannot copy `{name}`: non-Copy type requires `!` or `move` to transfer"
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
            SemanticErrorKind::FieldMissingDerivedTrait { struct_name, field_type, trait_name } => {
                write!(f, "`@derive({trait_name})` on `{struct_name}`: field type `{field_type}` does not implement `{trait_name}`")
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
            SemanticErrorKind::UnsafeIntegerConversion { from, to } => {
                write!(f, "cannot implicitly convert `{from}` to `{to}` (use `as {to}` for explicit conversion)")
            }
            SemanticErrorKind::DanglingReturn { name, local_name, .. } => {
                write!(f, "cannot return `{name}`: borrows from local variable `{local_name}` which will be dropped")
            }
            SemanticErrorKind::UseAfterSourceMoved { name, source_name, .. } => {
                write!(f, "use of `{name}` after source `{source_name}` was moved")
            }
            SemanticErrorKind::MutationWhileBorrowed { source, borrow } => {
                write!(f, "cannot mutate `{source}` while `{borrow}` borrows from it")
            }
            SemanticErrorKind::TemporaryBorrow { name, callee, .. } => {
                write!(f, "cannot bind `{name}` to temporary from `{callee}()` — value will be dropped")
            }
            SemanticErrorKind::InvalidParameterMode { param_name, type_name, mode } => {
                write!(f, "parameter `{param_name}` of type `{type_name}` cannot use `{mode}` mode — `{type_name}` is Copy and always passed by value")
            }
            SemanticErrorKind::UnresolvedBorrowOrigin { name } => {
                write!(f, "cannot return `{name}`: borrow origin could not be determined")
            }
            SemanticErrorKind::ArenaEscape { name, kind } => {
                match kind {
                    ArenaEscapeKind::Return => {
                        write!(f, "cannot return arena-scoped value `{name}` — memory will be freed when arena is destroyed")
                    }
                    ArenaEscapeKind::AssignOuter { target } => {
                        write!(f, "cannot assign arena-scoped value `{name}` to outer variable `{target}` — memory will be freed when arena is destroyed")
                    }
                }
            }
            SemanticErrorKind::MetaEvalError { message } => {
                write!(f, "meta evaluation error: {message}")
            }
            SemanticErrorKind::OrphanImpl { trait_, type_ } => {
                write!(
                    f,
                    "orphan rule: `equip {type_} with {trait_}` requires that either `{type_}` or `{trait_}` is defined in this module"
                )
            }
            SemanticErrorKind::DoubleAwait => {
                write!(f, "expression is awaited twice — use either `await expr` or `expr.await()`, not both")
            }
            SemanticErrorKind::ReadWhileMutCaptured { var_name, closure_name } => {
                write!(f, "cannot read `{var_name}` while it is mutably captured by closure `{closure_name}`")
            }
            SemanticErrorKind::WriteWhileMutCaptured { var_name, closure_name } => {
                write!(f, "cannot write to `{var_name}` while it is mutably captured by closure `{closure_name}`")
            }
            SemanticErrorKind::PrivateImport { name, module } => {
                write!(f, "cannot import private item `{name}` from module `{module}`")
            }
            SemanticErrorKind::PrivateTypeInPublicSignature { type_name, fn_name, position } => {
                write!(f, "public function `{fn_name}` has private type `{type_name}` in its {position}")
            }
            SemanticErrorKind::RequiredAfterDefault { name } => {
                write!(f, "required parameter `{name}` follows a parameter with a default value")
            }
            SemanticErrorKind::DuplicateStructField { field } => {
                write!(f, "duplicate field `{field}` in struct literal")
            }
            SemanticErrorKind::WrongFieldCount { type_, expected, found } => {
                write!(f, "`{type_}` has {expected} fields but {found} were supplied")
            }
            SemanticErrorKind::ClosureEscapesScope { closure_name, captured_name } => {
                write!(f, "cannot return closure `{closure_name}`: captures local variable `{captured_name}` which will be dropped")
            }
        }
    }
}
