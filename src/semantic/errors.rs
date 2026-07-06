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
    /// A bare (borrow) resource parameter is mutated, materializing a private
    /// CoW copy (docs/language-design.md §3.1-3.2), and that copy is never
    /// read afterwards — the write is dead and the caller's value is
    /// unchanged. The user almost certainly meant `&param` (write-through).
    DeadBareParamWrite {
        name: String,
        /// The parameter's declaration site (secondary label).
        param_span: Span,
    },
    /// Collection mutated while an implicit CoW borrow (from .get/.unwrap/index) is alive.
    /// The CoW system handles correctness, but the pattern may be confusing.
    CowBorrowMutation { source: String, borrow: String },
    /// A function returning `Result[T, E]` contains one or more bindings of the
    /// `T x = match expr: case Ok(v): v; case Error(e): return Error(e)` shape.
    /// Declaring the function `throws E` would let `T x = expr` auto-propagate the
    /// error and remove the boilerplate. Emitted by the `suggest_throws` lint —
    /// one per function regardless of how many sites match.
    SuggestThrowsRefactor {
        fn_name: String,
        error_type: String,
        /// Number of match-unwrap-or-rethrow sites detected inside this function.
        occurrence_count: usize,
    },
}

impl SemanticWarningKind {
    /// A stable, symbolic diagnostic code for this warning kind, mirroring
    /// `SemanticErrorKind::code()` (uniform `W_<VariantName>` — the `W_` prefix
    /// distinguishes non-fatal warnings from `E_` errors).
    ///
    /// **Phase 1: NOT rendered.** Per the RFC/brief (pass-2 fold R-e), warning
    /// codes are deferred — `report_semantic_warning` (`src/errors.rs`) does not
    /// yet thread `.with_code(...)`, so this method exists only for the exhaustive
    /// ratchet (a new warning variant without a code is a build error) and to let
    /// the registry (`spec/prose/diagnostic-codes.md`) enumerate warning codes.
    /// This match has **no catch-all `_`** — rustc exhaustiveness is the guard.
    pub fn code(&self) -> &'static str {
        match self {
            SemanticWarningKind::UnnecessaryShared { .. } => "W_UnnecessaryShared",
            SemanticWarningKind::StaleSharedCondition { .. } => "W_StaleSharedCondition",
            SemanticWarningKind::WithCheckThenAct { .. } => "W_WithCheckThenAct",
            SemanticWarningKind::StaleSharedWriteBack { .. } => "W_StaleSharedWriteBack",
            SemanticWarningKind::SharedIteratorInvalidation { .. } => "W_SharedIteratorInvalidation",
            SemanticWarningKind::SpawnWithTrackedBinding { .. } => "W_SpawnWithTrackedBinding",
            SemanticWarningKind::CompoundYieldRace { .. } => "W_CompoundYieldRace",
            SemanticWarningKind::ClosureCapturesWithBinding { .. } => "W_ClosureCapturesWithBinding",
            SemanticWarningKind::UnreachableCode => "W_UnreachableCode",
            SemanticWarningKind::UnusedVariable { .. } => "W_UnusedVariable",
            SemanticWarningKind::UnusedImport { .. } => "W_UnusedImport",
            SemanticWarningKind::UncheckedUnwrap { .. } => "W_UncheckedUnwrap",
            SemanticWarningKind::CouldBeConst { .. } => "W_CouldBeConst",
            SemanticWarningKind::NeedlessMutableBorrow { .. } => "W_NeedlessMutableBorrow",
            SemanticWarningKind::DeadBareParamWrite { .. } => "W_DeadBareParamWrite",
            SemanticWarningKind::CowBorrowMutation { .. } => "W_CowBorrowMutation",
            SemanticWarningKind::SuggestThrowsRefactor { .. } => "W_SuggestThrowsRefactor",
        }
    }
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
            SemanticWarningKind::DeadBareParamWrite { name, .. } => {
                write!(f, "write to bare parameter `{name}` lands on a private copy that is discarded — the caller's value is unchanged; declare it `&{name}` to write through")
            }
            SemanticWarningKind::CowBorrowMutation { source, borrow } => {
                write!(f, "`{source}` mutated while `{borrow}` holds an element — clone is inserted automatically")
            }
            SemanticWarningKind::SuggestThrowsRefactor { fn_name, error_type, occurrence_count } => {
                let sites = if *occurrence_count == 1 {
                    "1 match-unwrap-or-rethrow pattern".to_string()
                } else {
                    format!("{occurrence_count} match-unwrap-or-rethrow patterns")
                };
                write!(
                    f,
                    "function `{fn_name}` contains {sites}; declare it `throws {error_type}` and write `T x = expr` to auto-propagate (see docs/language-reference.md `throws`)"
                )
            }
        }
    }
}

/// How an arena-scoped value escapes its `with` block.
#[derive(Debug, Clone)]
pub enum ArenaEscapeKind {
    Return,
    AssignOuter { target: String },
    /// Element-ingest (`push`/`insert`/`add`/`send`/...) of a bare LIVE
    /// outer identifier into an outer-rooted buffer inside the arena block:
    /// clone-if-live materializes the clone in the arena, so the ingested
    /// element dangles at block exit. Distinct kind so the fix (`!` move)
    /// can be suggested — the generic AssignOuter wording would misname the
    /// live outer value as "arena-scoped".
    IngestLiveOuter { target: String },
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

    /// `unwrap` / `expect` / `unwrap_or` called on a receiver whose type is
    /// neither `Option` nor `Result`. These methods only exist on the optional
    /// types; on anything else they used to silently fall through the IR
    /// lowering to a no-op (returning the receiver unchanged). Surfacing it
    /// here makes the failure a clean type error at `gg check`.
    UnwrapOnNonOptional { method: String, type_: String },

    /// `*expr` (dereference) applied to a value whose type is not a smart
    /// pointer (`Box[T]`). On any other type the type checker used to return
    /// the inner type unchanged (a silent no-op), and the IR lowering then
    /// emitted `*(int64_t*)(*(void**)&value)` — interpreting the value's bits
    /// as a pointer and dereferencing garbage, which segfaults at runtime.
    /// Surfacing it here makes the failure a clean type error at `gg check`.
    DerefNonBox { type_: String },

    /// `lhs ?? rhs` (default operator) applied to a `lhs` whose type is
    /// neither `Option` nor `Result`. `??` unwraps the carrier's first
    /// variant (`Some`/`Ok`) and substitutes `rhs` on `None`/`Error`; on a
    /// non-carrier type the type checker used to discard the inferred LHS
    /// type and return the RHS type (a silent no-op), and the IR lowering
    /// then assumed an enum LHS and fell back to `("Some", lhs_type)` —
    /// emitting C that reinterprets the LHS bits as an enum (e.g.
    /// `'void *' from 'int64_t'`), which crashes/exits-1 at runtime.
    /// Surfacing it here makes the failure a clean type error at `gg check`.
    /// (Sibling of `UnwrapOnNonOptional`/`DerefNonBox` — same "operator on
    /// the wrong carrier type" guard class; AGENTS.md Core invariant #8.)
    DefaultOpNonOptional { type_: String },

    /// A method-level generic param couldn't be inferred from the
    /// call's arg types. Emitted by Phase 2c inference (see
    /// `docs/devbook/09-type-checking.md`, method-level generic inference)
    /// instead of the
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

    /// A scalar primitive (int/float/bool/…, not String) was equipped with a
    /// trait. Scalar primitives have no addressable heap `self` and no vtable
    /// slot, so trait method dispatch (direct or via `Box[Trait]`) miscompiles
    /// — the C backend passes the value where a `self` pointer is expected and
    /// dereferences garbage (SEGV); LLVM rejects the NULL-degraded vtable
    /// global. Rejected up front.
    PrimitiveTraitImpl { trait_: String, type_: String },

    /// A user-defined type's field/variant graph forms an unbounded cycle —
    /// every loop through the cycle stores its members by value (no `Box[T]`,
    /// `Vector[T]`, or other heap indirection), so the type would have
    /// infinite size. Without this check, codegen recurses unboundedly while
    /// laying the type out and stack-overflows.
    RecursiveTypeNeedsBox {
        name: String,
        /// The cycle path as a sequence of type names, e.g. ["Spanned", "Node", "Spanned"].
        cycle: Vec<String>,
    },

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

    /// A throwing/`Result`-returning call auto-propagates an error of type
    /// `callee_err` into an enclosing function whose error type is
    /// `caller_err`, but the two differ and no `From[callee_err]` impl is
    /// equipped on `caller_err`. Auto-propagation across error types requires
    /// an infallible `From` conversion (language-design §36.3). Without the
    /// gate this miscompiled to a `memcpy(sizeof(caller_err))` over a
    /// `sizeof(callee_err)` value — a type-confused out-of-bounds read
    /// (gorget-js snag #11).
    UnconvertibleErrorPropagation { caller_err: String, callee_err: String },

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

    /// `s[i] = x` / `s[i] += x` on a String — strings are not
    /// index-assignable (`s[i]` is a read-only codepoint view; mutation is
    /// rebuild-based). See language-reference §Strings.
    StringIndexAssign,

    /// Call to a builtin name that `is_builtin` accepts but that has NO
    /// lowering (`str`, `int8`…`uint64`, `uint`, `float32`/`float64`,
    /// `byte`) — would emit a raw undefined extern call into the C.
    /// `int`/`float`/`bool` have real cast lowerings and are NOT gated.
    UnloweredBuiltinCall { name: String },

    /// Match expression is not exhaustive — some enum variants are not covered.
    NonExhaustiveMatch { missing_variants: Vec<String> },

    /// A non-void function has a path that reaches the end of the body
    /// without returning a value (definite-return analysis).
    MissingReturn { function: String, return_type: String },

    /// A function declared `noreturn` whose body can return control to
    /// the caller: falls off the end, executes a `return`, or has a
    /// non-diverging expression body. Callers type a noreturn call as
    /// `Never` and the IR emits `unreachable` right after it, so a
    /// returning noreturn function is a miscompile, not a style issue.
    NoreturnBodyReturns { function: String },

    /// `noreturn` combined with a `throws` clause: a `throw` returns
    /// control to the caller via the error channel, contradicting
    /// `noreturn` (callers would run into `unreachable`).
    NoreturnWithThrows { function: String },

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

    /// A module-level `const` initializer is not a compile-time constant
    /// (e.g. an enum/struct constructor). `const` is inlined at every use
    /// site, so its value must fold at compile time — use `static` for a
    /// runtime-initialized global instead.
    NonConstantConstInitializer { name: String },

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

    /// Attempt to import a name a module does not export (typo / wrong name).
    UnresolvedImport { name: String, module: String },

    /// Public function exposes a private type in its signature.
    PrivateTypeInPublicSignature { type_name: String, fn_name: String, position: String },

    /// Required parameter follows a parameter with a default value.
    RequiredAfterDefault { name: String },

    /// Duplicate field in struct literal.
    DuplicateStructField { field: String },

    /// Two fields with the same name in a struct *declaration*
    /// (`struct P: int x; int x`). Distinct from `DuplicateStructField`,
    /// which is a duplicate in a struct *literal*. The declaration form
    /// previously slipped through resolution and only failed downstream
    /// at the C compiler ("duplicate member"); this rejects it up front.
    DuplicateStructFieldDecl { field: String },

    /// Wrong number of fields in struct literal.
    WrongFieldCount { type_: String, expected: usize, found: usize },

    /// Returning a closure that captures a local variable (use-after-free).
    ClosureEscapesScope { closure_name: String, captured_name: String },

    /// Calling `.lock()` on a Mutex/RwLock that already has a live Guard in scope.
    /// Non-reentrant — the second lock would deadlock at runtime.
    MutexDoubleLock { mutex_name: String, prior_guard_name: String, prior_lock_at: Span },
}

impl SemanticErrorKind {
    /// A stable, symbolic diagnostic code for this error kind, rendered by the
    /// error reporter as `error[E_...]:` (see `src/errors.rs::report_semantic_error`)
    /// and referenced by the diagnostic registry (`spec/prose/diagnostic-codes.md`).
    ///
    /// # Why symbolic names, not numbers
    /// The scheme is uniform `E_<VariantName>` — mechanically derived from the
    /// variant identity, so it is simultaneously *systematic* (exhaustive by
    /// construction) and *meaningful* (the variant names are descriptive). Tying
    /// the code to the variant identity — not a sequence number — means codes
    /// never churn or collide across branches (the RFC §8 numbering bikeshed is
    /// sidestepped; the one normative anchor is `E_MoveWithoutOperator`, RFC §5.5).
    ///
    /// # Ratchet (CLAUDE.md core-invariant #6)
    /// This match MIRRORS the `Display` impl's arms and has **no catch-all `_`**,
    /// so `rustc`'s exhaustiveness check is the guard: a new `SemanticErrorKind`
    /// variant added without a code here is a hard build error, not a silent gap.
    pub fn code(&self) -> &'static str {
        match self {
            SemanticErrorKind::UndefinedName { .. } => "E_UndefinedName",
            SemanticErrorKind::DuplicateDefinition { .. } => "E_DuplicateDefinition",
            SemanticErrorKind::TypeMismatch { .. } => "E_TypeMismatch",
            SemanticErrorKind::WrongArgCount { .. } => "E_WrongArgCount",
            SemanticErrorKind::NotAFunction { .. } => "E_NotAFunction",
            SemanticErrorKind::NotAType { .. } => "E_NotAType",
            SemanticErrorKind::NotAStruct { .. } => "E_NotAStruct",
            SemanticErrorKind::MissingTraitMethod { .. } => "E_MissingTraitMethod",
            SemanticErrorKind::NoMethodFound { .. } => "E_NoMethodFound",
            SemanticErrorKind::UnwrapOnNonOptional { .. } => "E_UnwrapOnNonOptional",
            SemanticErrorKind::DerefNonBox { .. } => "E_DerefNonBox",
            SemanticErrorKind::DefaultOpNonOptional { .. } => "E_DefaultOpNonOptional",
            SemanticErrorKind::MethodGenericInferenceFailed { .. } => "E_MethodGenericInferenceFailed",
            SemanticErrorKind::CannotInferType => "E_CannotInferType",
            SemanticErrorKind::NoFieldFound { .. } => "E_NoFieldFound",
            SemanticErrorKind::TupleIndexOutOfBounds { .. } => "E_TupleIndexOutOfBounds",
            SemanticErrorKind::OrPatternBindingMismatch { .. } => "E_OrPatternBindingMismatch",
            SemanticErrorKind::DuplicateImpl { .. } => "E_DuplicateImpl",
            SemanticErrorKind::PrimitiveTraitImpl { .. } => "E_PrimitiveTraitImpl",
            SemanticErrorKind::RecursiveTypeNeedsBox { .. } => "E_RecursiveTypeNeedsBox",
            SemanticErrorKind::TraitCycle { .. } => "E_TraitCycle",
            SemanticErrorKind::MethodSignatureMismatch { .. } => "E_MethodSignatureMismatch",
            SemanticErrorKind::BreakOutsideLoop => "E_BreakOutsideLoop",
            SemanticErrorKind::ContinueOutsideLoop => "E_ContinueOutsideLoop",
            SemanticErrorKind::ReturnOutsideFunction => "E_ReturnOutsideFunction",
            SemanticErrorKind::ThrowInNonThrowingFunction => "E_ThrowInNonThrowingFunction",
            SemanticErrorKind::RethrowInNonThrowingFunction => "E_RethrowInNonThrowingFunction",
            SemanticErrorKind::OnErrorInNonThrowingFunction => "E_OnErrorInNonThrowingFunction",
            SemanticErrorKind::MainThrowsNonInt => "E_MainThrowsNonInt",
            SemanticErrorKind::UnconvertibleErrorPropagation { .. } => "E_UnconvertibleErrorPropagation",
            SemanticErrorKind::AwaitOutsideAsync => "E_AwaitOutsideAsync",
            SemanticErrorKind::SelectOutsideAsync => "E_SelectOutsideAsync",
            SemanticErrorKind::AwaitNonFuture => "E_AwaitNonFuture",
            SemanticErrorKind::SpawnNonFuture => "E_SpawnNonFuture",
            SemanticErrorKind::BorrowAcrossAwait { .. } => "E_BorrowAcrossAwait",
            SemanticErrorKind::SpawnWithBorrowedRef { .. } => "E_SpawnWithBorrowedRef",
            SemanticErrorKind::SpawnRequiresDirectCall => "E_SpawnRequiresDirectCall",
            SemanticErrorKind::SpawnClosureCaptureBorrowed { .. } => "E_SpawnClosureCaptureBorrowed",
            SemanticErrorKind::SpawnClosureCaptureMutable { .. } => "E_SpawnClosureCaptureMutable",
            SemanticErrorKind::SpawnClosureCaptureShared { .. } => "E_SpawnClosureCaptureShared",
            SemanticErrorKind::UseAfterMove { .. } => "E_UseAfterMove",
            SemanticErrorKind::MoveWithoutOperator { .. } => "E_MoveWithoutOperator",
            SemanticErrorKind::BorrowConflict { .. } => "E_BorrowConflict",
            SemanticErrorKind::MoveInLoop { .. } => "E_MoveInLoop",
            SemanticErrorKind::DoubleMove { .. } => "E_DoubleMove",
            SemanticErrorKind::NonPrintableInterpolation { .. } => "E_NonPrintableInterpolation",
            SemanticErrorKind::OwnershipMismatch { .. } => "E_OwnershipMismatch",
            SemanticErrorKind::UnsatisfiedTraitBound { .. } => "E_UnsatisfiedTraitBound",
            SemanticErrorKind::NonExhaustiveMatch { .. } => "E_NonExhaustiveMatch",
            SemanticErrorKind::MissingReturn { .. } => "E_MissingReturn",
            SemanticErrorKind::NoreturnBodyReturns { .. } => "E_NoreturnBodyReturns",
            SemanticErrorKind::NoreturnWithThrows { .. } => "E_NoreturnWithThrows",
            SemanticErrorKind::StringIndexAssign => "E_StringIndexAssign",
            SemanticErrorKind::UnloweredBuiltinCall { .. } => "E_UnloweredBuiltinCall",
            SemanticErrorKind::UnknownNamedArg { .. } => "E_UnknownNamedArg",
            SemanticErrorKind::DuplicateNamedArg { .. } => "E_DuplicateNamedArg",
            SemanticErrorKind::MissingRequiredArg { .. } => "E_MissingRequiredArg",
            SemanticErrorKind::PositionalAfterNamed => "E_PositionalAfterNamed",
            SemanticErrorKind::UnknownDirective { .. } => "E_UnknownDirective",
            SemanticErrorKind::UnderivableTrait { .. } => "E_UnderivableTrait",
            SemanticErrorKind::DeriveFromRequiresSingleField { .. } => "E_DeriveFromRequiresSingleField",
            SemanticErrorKind::FieldMissingDerivedTrait { .. } => "E_FieldMissingDerivedTrait",
            SemanticErrorKind::AssignmentToConst { .. } => "E_AssignmentToConst",
            SemanticErrorKind::NonConstantConstInitializer { .. } => "E_NonConstantConstInitializer",
            SemanticErrorKind::ViaWithoutTrait => "E_ViaWithoutTrait",
            SemanticErrorKind::ViaFieldNotFound { .. } => "E_ViaFieldNotFound",
            SemanticErrorKind::ViaFieldTypeMissingTrait { .. } => "E_ViaFieldTypeMissingTrait",
            SemanticErrorKind::DuplicateSuiteBlock { .. } => "E_DuplicateSuiteBlock",
            SemanticErrorKind::InvalidFnTraitArg => "E_InvalidFnTraitArg",
            SemanticErrorKind::ClosureKindMismatch { .. } => "E_ClosureKindMismatch",
            SemanticErrorKind::ValueOutOfRange { .. } => "E_ValueOutOfRange",
            SemanticErrorKind::UnsafeIntegerConversion { .. } => "E_UnsafeIntegerConversion",
            SemanticErrorKind::DanglingReturn { .. } => "E_DanglingReturn",
            SemanticErrorKind::UseAfterSourceMoved { .. } => "E_UseAfterSourceMoved",
            SemanticErrorKind::MutationWhileBorrowed { .. } => "E_MutationWhileBorrowed",
            SemanticErrorKind::TemporaryBorrow { .. } => "E_TemporaryBorrow",
            SemanticErrorKind::InvalidParameterMode { .. } => "E_InvalidParameterMode",
            SemanticErrorKind::UnresolvedBorrowOrigin { .. } => "E_UnresolvedBorrowOrigin",
            SemanticErrorKind::ArenaEscape { .. } => "E_ArenaEscape",
            SemanticErrorKind::MetaEvalError { .. } => "E_MetaEvalError",
            SemanticErrorKind::OrphanImpl { .. } => "E_OrphanImpl",
            SemanticErrorKind::DoubleAwait => "E_DoubleAwait",
            SemanticErrorKind::ReadWhileMutCaptured { .. } => "E_ReadWhileMutCaptured",
            SemanticErrorKind::WriteWhileMutCaptured { .. } => "E_WriteWhileMutCaptured",
            SemanticErrorKind::PrivateImport { .. } => "E_PrivateImport",
            SemanticErrorKind::UnresolvedImport { .. } => "E_UnresolvedImport",
            SemanticErrorKind::PrivateTypeInPublicSignature { .. } => "E_PrivateTypeInPublicSignature",
            SemanticErrorKind::RequiredAfterDefault { .. } => "E_RequiredAfterDefault",
            SemanticErrorKind::DuplicateStructField { .. } => "E_DuplicateStructField",
            SemanticErrorKind::DuplicateStructFieldDecl { .. } => "E_DuplicateStructFieldDecl",
            SemanticErrorKind::WrongFieldCount { .. } => "E_WrongFieldCount",
            SemanticErrorKind::ClosureEscapesScope { .. } => "E_ClosureEscapesScope",
            SemanticErrorKind::MutexDoubleLock { .. } => "E_MutexDoubleLock",
        }
    }
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
            SemanticErrorKind::UnwrapOnNonOptional { method, type_ } => {
                write!(
                    f,
                    "method `{method}` requires an `Option` or `Result` receiver, \
                     but `{type_}` is neither"
                )
            }
            SemanticErrorKind::DerefNonBox { type_ } => {
                write!(
                    f,
                    "cannot dereference `*` a value of type `{type_}` — \
                     `*` requires a `Box[T]`"
                )
            }
            SemanticErrorKind::DefaultOpNonOptional { type_ } => {
                write!(
                    f,
                    "default operator `??` requires an `Option` or `Result` \
                     left-hand side, but `{type_}` is neither"
                )
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
            SemanticErrorKind::PrimitiveTraitImpl { trait_, type_ } => {
                write!(
                    f,
                    "cannot equip scalar primitive `{type_}` with trait `{trait_}`: \
                    scalar primitives cannot be trait method receivers (no addressable \
                    `self`), so both direct dispatch and `Box[{trait_}]` trait-object \
                    dispatch would miscompile. Wrap the value in a struct and equip that."
                )
            }
            SemanticErrorKind::RecursiveTypeNeedsBox { name, cycle } => {
                write!(
                    f,
                    "recursive type `{name}` has infinite size: cycle {} \
                    stores members by value — break it with `Box[T]`, `Vector[T]`, \
                    or another heap-indirected wrapper",
                    cycle.join(" → "),
                )
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
            SemanticErrorKind::UnconvertibleErrorPropagation { caller_err, callee_err } => {
                write!(
                    f,
                    "cannot auto-propagate error of type `{callee_err}` into a function \
                     that throws `{caller_err}`: the error types differ and no \
                     `From[{callee_err}]` conversion is equipped on `{caller_err}`. \
                     Add `equip {caller_err} with From[{callee_err}]:` (defining \
                     `{caller_err} from({callee_err} e)`), or handle the error \
                     explicitly with `rethrow`/`catch` (language-design §36.3)"
                )
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
            SemanticErrorKind::MissingReturn { function, return_type } => {
                write!(
                    f,
                    "missing return: control can reach the end of `{function}` \
                     without returning a value — every path must end in `return` \
                     (expected `{return_type}`), `throw`, or a diverging call \
                     like `panic`"
                )
            }
            SemanticErrorKind::NoreturnBodyReturns { function } => {
                write!(
                    f,
                    "`{function}` is declared `noreturn` but control can reach \
                     the end of its body (or execute a `return`) — a noreturn \
                     function must diverge on every path: loop forever or end \
                     in a call to another noreturn function like `exit` or \
                     `panic`"
                )
            }
            SemanticErrorKind::NoreturnWithThrows { function } => {
                write!(
                    f,
                    "`{function}` is declared `noreturn` but has a `throws` \
                     clause — a noreturn function cannot return, not even an \
                     error"
                )
            }
            SemanticErrorKind::StringIndexAssign => {
                write!(
                    f,
                    "strings are not index-assignable: `s[i]` is a read-only \
                     codepoint view — build a new string instead (e.g. \
                     `s.replace(...)`, slicing + concatenation)"
                )
            }
            SemanticErrorKind::UnloweredBuiltinCall { name } => {
                if name == "str" {
                    write!(
                        f,
                        "no builtin `str(...)` call: convert with an f-string \
                         `f\"{{x}}\"`, `.display()`, or `std.conv` (e.g. \
                         `int_to_str`)"
                    )
                } else {
                    write!(
                        f,
                        "no builtin `{name}(...)` conversion call: use an \
                         `as` cast (`x as {name}`)"
                    )
                }
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
            SemanticErrorKind::NonConstantConstInitializer { name } => {
                write!(f, "`const {name}` initializer is not a compile-time constant; `const` values are inlined at every use site. Use `static {name}` for a runtime-initialized global")
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
                    ArenaEscapeKind::IngestLiveOuter { target } => {
                        write!(f, "cannot insert `{name}` into `{target}` inside an arena block: `{name}` is still live here, so its clone would be arena-allocated and freed when the arena is destroyed — use `!{name}` to move it into the collection, or clone outside the block")
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
            SemanticErrorKind::UnresolvedImport { name, module } => {
                write!(f, "module `{module}` does not export `{name}`")
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
            SemanticErrorKind::DuplicateStructFieldDecl { field } => {
                write!(f, "duplicate struct field `{field}`")
            }
            SemanticErrorKind::WrongFieldCount { type_, expected, found } => {
                write!(f, "`{type_}` has {expected} fields but {found} were supplied")
            }
            SemanticErrorKind::ClosureEscapesScope { closure_name, captured_name } => {
                write!(f, "cannot return closure `{closure_name}`: captures local variable `{captured_name}` which will be dropped")
            }
            SemanticErrorKind::MutexDoubleLock { mutex_name, prior_guard_name, .. } => {
                write!(f, "cannot lock `{mutex_name}`: already locked — guard `{prior_guard_name}` is still in scope (would deadlock at runtime; non-reentrant)")
            }
        }
    }
}

#[cfg(test)]
mod code_tests {
    use super::*;
    use crate::span::Span;

    fn sp() -> Span {
        Span { start: 0, end: 0 }
    }

    /// The one normative anchor (RFC §5.5 / CLAUDE.md core-invariant #3): the
    /// move-without-operator diagnostic is `E_MoveWithoutOperator`.
    #[test]
    fn normative_move_without_operator_code() {
        let k = SemanticErrorKind::MoveWithoutOperator { name: "x".into() };
        assert_eq!(k.code(), "E_MoveWithoutOperator");
    }

    /// Representative sample across unit variants, struct variants, and the
    /// D4/D5/D6-relevant ownership family — codes are the `E_<VariantName>` form.
    #[test]
    fn representative_error_codes() {
        let cases: Vec<(SemanticErrorKind, &str)> = vec![
            (SemanticErrorKind::UseAfterMove { name: "x".into(), moved_at: sp() }, "E_UseAfterMove"),
            (SemanticErrorKind::DoubleMove { name: "x".into(), first_move: sp() }, "E_DoubleMove"),
            (SemanticErrorKind::MoveInLoop { name: "x".into() }, "E_MoveInLoop"),
            (SemanticErrorKind::BorrowConflict { name: "x".into(), detail: "d".into() }, "E_BorrowConflict"),
            (SemanticErrorKind::TypeMismatch { expected: "int".into(), found: "str".into() }, "E_TypeMismatch"),
            (SemanticErrorKind::CannotInferType, "E_CannotInferType"),
            (SemanticErrorKind::BreakOutsideLoop, "E_BreakOutsideLoop"),
            (SemanticErrorKind::StringIndexAssign, "E_StringIndexAssign"),
            (SemanticErrorKind::DoubleAwait, "E_DoubleAwait"),
            (SemanticErrorKind::NonExhaustiveMatch { missing_variants: vec![] }, "E_NonExhaustiveMatch"),
        ];
        for (kind, expected) in cases {
            assert_eq!(kind.code(), expected, "code mismatch for {kind:?}");
        }
    }

    /// Every error code is non-empty and carries the `E_` namespace prefix.
    #[test]
    fn error_codes_are_e_prefixed_nonempty() {
        let samples = [
            SemanticErrorKind::UndefinedName { name: "n".into(), suggestion: None },
            SemanticErrorKind::MutexDoubleLock { mutex_name: "m".into(), prior_guard_name: "g".into(), prior_lock_at: sp() },
            SemanticErrorKind::ArenaEscape { name: "n".into(), kind: ArenaEscapeKind::Return },
            SemanticErrorKind::MetaEvalError { message: "m".into() },
        ];
        for k in &samples {
            let c = k.code();
            assert!(c.starts_with("E_"), "`{c}` should start with E_");
            assert!(c.len() > 2, "`{c}` should be non-empty after the prefix");
        }
    }

    /// Warning codes use the `W_` namespace (not rendered in phase 1, but the
    /// exhaustive match is the ratchet and the registry enumerates them).
    #[test]
    fn representative_warning_codes() {
        assert_eq!(SemanticWarningKind::UnreachableCode.code(), "W_UnreachableCode");
        assert_eq!(
            SemanticWarningKind::UnusedVariable { name: "x".into() }.code(),
            "W_UnusedVariable"
        );
        assert_eq!(
            SemanticWarningKind::DeadBareParamWrite { name: "x".into(), param_span: sp() }.code(),
            "W_DeadBareParamWrite"
        );
    }
}
