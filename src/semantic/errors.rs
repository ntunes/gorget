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
    /// A bare (Borrow) resource parameter is mutated inside a self-recursive
    /// call — through `&param` into a `&`-declared callee, a mutating `&self`
    /// method, a builtin mutator like `.push`, or direct field/index
    /// assignment — AND the parameter reaches a bare-borrow arg of the same
    /// self-recursive call. Each recursion level materializes a private copy
    /// per §3.1 (each frame is a fresh immutable context; the write lands on
    /// a private copy); recursion multiplies the cost (measured: O(N) linear,
    /// O(2^N) branching). The user almost certainly meant either to declare
    /// `&{name}` and spell `&arg` at callers (the write-through then reaches
    /// the true owner via an unbroken `&`-chain), OR to materialize
    /// explicitly with `{name}.clone()` (per-frame private copies made honest
    /// about intent). Charter-accepted §3.1 exception; steers `&`-forward —
    /// see docs/devbook/11-copy-on-write.md "Accepted charter exception".
    RecursiveBareParamMaterialize {
        name: String,
        /// The parameter's declaration site (primary label).
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
            SemanticWarningKind::RecursiveBareParamMaterialize { .. } => "W_RecursiveBareParamMaterialize",
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
                // D2-rider RATIFIED verbatim message (decisions.md LOG
                // 2026-07-16): one format string yields both flavors —
                // `&self` for the self param, `&<param>` otherwise.
                write!(f, "this writes to a private copy that is never read — the caller's value is unchanged; did you mean `&{name}`?")
            }
            SemanticWarningKind::RecursiveBareParamMaterialize { name, .. } => {
                // Message honestly names the two reference-grade fixes
                // (declare `&param` for caller-side write-through, OR
                // materialize explicitly with `.clone()`). "Mutated" (not
                // "`&`-formed") is the precise umbrella term: the diagnostic
                // covers `&arg`, mutating `&self` methods, builtin mutators
                // like `.push`, AND direct field/index assignment. For `self`
                // the same format string renders naturally as "parameter
                // `self` is mutated…".
                write!(
                    f,
                    "parameter `{name}` is mutated inside a recursive self-call — each recursion level materializes a private copy of `{name}`; declare `&{name}` and spell `&arg` at callers (the write-through then propagates via an unbroken `&`-chain, §3.1), or materialize explicitly with `{name}.clone()` (per-frame private copies)"
                )
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

/// D12/D4: WHY a bare copy of a non-Copy value is rejected. Controls the
/// "why" clause of `E_MoveWithoutOperator`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveReason {
    /// The value's transitive drop graph carries a custom `Drop` — a resource,
    /// single-owner by D4. (`is_drop_tainted_type`.)
    DropTaint,
    /// A single-owner-BY-DESIGN carve-out type (closure/`Callable`, `Owned[T]`,
    /// `Box[T]`, `Task`/`TaskGroup`/`Guard`) — no clone path in the lowering.
    SingleOwner,
}

/// D12/D4: the PLACE SHAPE of the rejected source, which decides the valid
/// remedy in the `E_MoveWithoutOperator` message. A pure function of the place
/// expr already in hand at the construction site (no new dataflow) — see
/// `safety::helpers::place_shape`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveShape {
    /// Whole-identifier / self / param place. Remedy: `!x` to move or
    /// `x.clone()` to copy.
    Whole,
    /// Field / index SUB-place (`obj.f`, `v[i]`). A bare `!obj.f` is a PARTIAL
    /// move (rejected), so the only remedy is `obj.f.clone()`.
    FieldIndex,
    /// A closure captures the value by value. Capture-list syntax (D5/D7) is
    /// unbuilt and a `.clone()`-into-local is equally tainted, so NEITHER `!`
    /// NOR `.clone()` is a valid remedy — pass it as an argument or wrap it in
    /// `Shared[T]`.
    Capture,
}

/// D29: which fallible-mark violation an `E_MissingFallibleMark` reports. One
/// code, two messages — the reason discriminates the teaching text (the code
/// registry stays one-per-variant; this is a payload, not a second code).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FallibleMarkReason {
    /// A bare fallible call with no `!` and no capturing `Result` destination.
    /// Teaches all three exits: mark it, handle it, or capture it.
    Bare,
    /// A `!`-marked call whose outcome is already captured by an explicitly
    /// `Result`-annotated destination — the mark is redundant (the annotation
    /// carries the visibility). Fix-it: remove the `!`.
    RedundantOnCapture,
    /// `Ok`/`Error` match arms over a marked (`f()!`) scrutinee, which peels to
    /// the success value `T` — the arms cannot inspect the whole `Result`. Bind
    /// the outcome first (`Result[T,E] r = f(); match r:`).
    ResultArmsOnPeeled,
    /// A `!` whose inner expression is NOT a fallible call — `5!`, `pure(3)!`,
    /// `r!` on a `Result` local (values are not calls), or the outer mark of
    /// `f()!!` (the "no second mark" pin). An unconsumed mark is a lie: it
    /// claims an error channel that does not exist at that expression.
    MarkOnInfallible,
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

    /// A type-defining name (struct / enum / trait / type alias) used in a
    /// value position (`match Direction:`, `Point p = Point`). A type is not a
    /// value; without this reject the program type-checks then SIGSEGVs.
    TypeInValuePosition { name: String, kind: String },

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

    /// Track P (owner Q1 2026-07-28): a NonDerefContainer
    /// (`Mutex` / `RWLock` / `Weak` / `Shared`) declared with a bare trait as
    /// its type-arg. Concurrency containers cannot hold a bare trait; the user
    /// must write `Container[Box[Trait]]` explicitly so the heap allocation is
    /// visible in the type. Silently boxing would violate D31's spelling
    /// philosophy (costs stay visible) and CoW's no-user-visible-`Ref[T]`
    /// principle. Rejected at type-resolution time; see
    /// `docs/define-gorget/decisions.md:1373` (D36 NonDerefContainer clause)
    /// and the owner Q1 ruling in TODO.md's Round XII handover.
    NonDerefContainerBareTrait { container: String, trait_: String },

    /// D36 face-split reject: a write-face method (`&self`) was called
    /// through a `ReadGuard`. Writes are forbidden through a shared-read
    /// view — mirrors the `for_write` gate on `&rg.field` at a `push`
    /// argument. Suggested fix: acquire a `WriteGuard` via `.write()` or
    /// change the invocation to a read-face method (bare `self`).
    /// See `docs/language-design.md` §9.3 for the ratified per-face rules.
    AutoDerefWriteThroughReadGuard { method: String, wrapper: String },

    /// D36 face-split reject: a consuming-face method (`!self`) was called
    /// through a `Guard` / `ReadGuard` / `WriteGuard`. Consuming through a
    /// guard breaks the guard's Drop invariant (moving the inner OUT would
    /// orphan the mutex-unlock in the guard's Drop). Consuming auto-deref
    /// is legal only through `Box`. Suggested fix: consume the inner
    /// explicitly (`!guard.into_inner()` is unavailable — release the
    /// guard first, then move the value out of the container).
    /// See `docs/language-design.md` §9.3 for the ratified per-face rules.
    AutoDerefConsumingThroughGuard { method: String, wrapper: String },

    /// `unwrap` / `expect` / `unwrap_or` called on a receiver whose type is
    /// neither `Option` nor `Result`. These methods only exist on the optional
    /// types; on anything else they used to silently fall through the IR
    /// lowering to a no-op (returning the receiver unchanged). Surfacing it
    /// here makes the failure a clean type error at `gg check`.
    UnwrapOnNonOptional { method: String, type_: String },

    /// Round XXVII Track D: `for (i, x) in <collection>.enumerate():` where
    /// the collection is `Set[T]` or `HashSet[T]`. Per `docs/book/05-collections.md`
    /// and `lib/std/iter.gg`, `.enumerate()` is an `Iterator[T]` adapter; Set
    /// and HashSet impl `Iterable[T]` (they expose `.iter()`) but are not
    /// themselves `Iterator[T]`. Pre-D27 the enumerate scaffold at
    /// `for_loops.rs:lower_for_enumerate` read `iter.Field(2)` (Vector/Str's
    /// length slot) against the Set's hash-table layout, terminating the
    /// counter loop before any entry emitted — silent zero output on both
    /// backends (Core #10 lower-or-reject class). Pointer at
    /// `.iter().enumerate()` gives the reference-grade fix.
    EnumerateOnNonIterator { type_: String },

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

    /// D10(a) (docs/define-gorget/decisions.md, ratified 2026-07-06):
    /// a mutable borrow (`&expr`) bound to a name — `auto r = &b`,
    /// `Vector[int] r = &b.data`, `r = &b`, or a module-level
    /// `static G = &BASE`. A named `&`-binding would create a second
    /// writable path to the same place for the rest of the scope, violating
    /// the one-exclusive-writer rule that licenses lazy CoW. The pre-D10
    /// write-through half-worked in practice (the explicitly-typed form
    /// ICE'd the consume-site validator; an element projection silently
    /// wrote to a private copy), so the form is rejected outright in v1.
    /// `&` forms a mutable borrow only at a call boundary (an argument to a
    /// `&` parameter); frame-scoped `&` params are unaffected.
    LocalBorrowBind,

    /// Round XXIII Track β (ratified handover ruling): `&`-of-a-place used in
    /// an OPERAND (READ) position — `match &c.fd:`, `1 + &c.fd`, `x += &c.fd`,
    /// `if &c.fd > 5:`, `v[&c.fd]`, `f"{&c.fd}"`, closure body `(): &c.fd`,
    /// `return &c.fd`, `throw &c.fd`, channel `send(&c.fd)`, etc. An operand
    /// is a READ; `&` on a `&` parameter says "the callee writes through the
    /// borrow", which is meaningless in a read position. Pre-reject: every
    /// costume silently miscompiled on C (raw address arithmetic, garbage
    /// prints, address comparisons masquerading as value compares) and
    /// hard-failed the LLVM verifier (`i64 but expected ptr`); the tainted
    /// twin duplicated the user `Drop` (`close 9` twice) on EVERY operand
    /// costume, not just `if`. This one-producer chokepoint retires the
    /// silent-wrong class + the double-Drop class in one arm.
    ///
    /// Legit operand-adjacent positions strip earlier: call-arg `f(&x)` is
    /// stripped by `parse_ownership_modifier` (never reaches this arm),
    /// `for x in (&coll)` and `.enumerate()`-receiver strip in
    /// `check_stmt::Stmt::For` (mirroring `for_loops.rs`), and VarDecl/Assign
    /// RHS is authoritatively rejected earlier by `E_LocalBorrowBind`
    /// (D10(a)) via an option-D direct-`Expr::MutableBorrow` intercept.
    AmpInOperandPosition,

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

    /// RV-A: a field PRESENT on a `Box[T]` wrapper's inner type was accessed
    /// directly (`box_val.field`), which requires §9.4 deref coercion — a
    /// feature whose backend (deref-field read) is not yet implemented. Staged
    /// reject (decisions.md 2026-07-16 STAGING RULING) until that track lands;
    /// distinct from `NoFieldFound` because the field DOES exist on the inner,
    /// so an "no field found" message would lie.
    DerefCoercionUnimplemented { field: String, inner: String, wrapper: String },

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

    /// D23 (throws totality): a `throws` call appears in a position where its
    /// error is neither auto-propagated (the enclosing function is not
    /// `throws`/`Result`-returning) nor handled (`catch`/`rethrow`/`Result`
    /// capture). A `throws` call is an expression of type `T` in every
    /// position; its `Result[T, E]` desugar is never observable. Emitting this
    /// at the producer (the call site) — rather than letting the raw
    /// `Result[T, E]` leak into a downstream `unify` — replaces the three
    /// pre-D23 failure modes with one clean diagnostic: the `found `Result[`
    /// desugar-leak (free-fn consumer positions), the silent swallow (match
    /// scrutinee / bare statement), and the silent miscompile-to-garbage
    /// (`throws` method calls, whose throws-ness was dropped entirely).
    /// `throws_type` is the callee's error type `E`.
    UnhandledThrows { throws_type: String },

    /// D29 (visible error propagation): a fallible call — one whose callee is
    /// `throws E` (kind-1) OR whose declared return is `Result[T, E]` (kind-2) —
    /// is used without the mandatory postfix `!`, and its outcome is neither
    /// captured by an explicitly `Result`-annotated destination nor attached to
    /// a `catch`/`rethrow` disposition. OR (the `RedundantOnCapture` reason) the
    /// `!` is present but the destination already captures the whole `Result` —
    /// the mark is redundant. `throws_type` is the callee's error type `E`; the
    /// message never surfaces the `Result[…]` desugar as a found-type (D23
    /// contract). Fix-it: insert `!` (or ` !` before `=`), or remove it for the
    /// redundant-capture reason.
    MissingFallibleMark { throws_type: String, reason: FallibleMarkReason },

    /// D29/A31: a bare `!` signature (`int f()!:`) — the reserved spelling for
    /// A31 inferred error sets — used before A31 is implemented. The grammar
    /// locks now (parses); the checker teaching-rejects until A31 lands, steering
    /// the user to the explicit `throws E` contract spelling.
    InferredThrowsUnsupported,

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

    /// Non-Copy type implicitly copied at an ownership boundary. `reason` is
    /// WHY (drop-taint vs single-owner-by-design); `shape` is the place shape
    /// that decides the valid remedy (Whole / field-index sub-place / capture).
    /// `write_through_available` (2T/D2): TRUE only at a materialize-on-write
    /// position (assign / compound / mutating-receiver / `&`-formation) where
    /// re-declaring the root `&self` / `&<param>` writes through — that `&`
    /// remedy is offered ONLY when this is set. At ctor/field-init/capture/bind
    /// it is FALSE (an `&` there is not a valid fix, e.g. `Some(&fh)`), and the
    /// message is byte-identical to the pre-discriminator text.
    MoveWithoutOperator {
        name: String,
        reason: MoveReason,
        shape: MoveShape,
        write_through_available: bool,
    },

    /// Borrow exclusivity violation.
    BorrowConflict { name: String, detail: String },

    /// Moving a variable inside a loop body.
    MoveInLoop { name: String },

    /// Same variable moved twice.
    DoubleMove { name: String, first_move: Span },

    /// Non-printable type used in string interpolation.
    NonPrintableInterpolation { var_name: String, type_name: String },

    /// Call-site ownership annotation doesn't match parameter declaration
    /// (D31 full-strict, `decisions.md` 2026-07-20 ADDENDUM-2): a `!` param
    /// requires `!` at the call site, a `&` param requires `&`, a bare param is
    /// borrowed — at every call site, free-fn and method, named place or
    /// temporary. Carries the RAW modes (not pre-rendered strings) so the
    /// message is kind-aware, and `arg_is_temp` so the DX rider's bare-temp
    /// case ("this call consumes the value — add `!`") is tailored.
    OwnershipMismatch {
        param_name: String,
        expected: crate::parser::ast::Ownership,
        found: crate::parser::ast::Ownership,
        arg_is_temp: bool,
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

    /// Binary or compound operator applied to a type that does not support it
    /// (e.g. `s.name -= "x"`, `s - "x"`, `m -= r` where `Money` equips only
    /// `Add`). Arithmetic needs a numeric primitive or a matching operator
    /// trait equip; String supports only `+`/`+=` (concatenation). Without
    /// this gate, typecheck accepted then lowering ICE'd (resource-moves) or
    /// emitted broken C. See language-reference §operators / §Strings.
    UnsupportedOperator { op: String, type_name: String },

    /// `5 += 1` / `foo() += 1` / `(a + b) = x` — the left side of an assignment
    /// or compound assignment is NOT an assignable place. Valid targets are a
    /// variable, field, tuple field, index (`v[i]`), or dereference (`*p`). The
    /// parser accepts any expression as a target and Gorget had no lvalue gate,
    /// so these formerly silently dropped (plain `=`) or ICE'd (compound) in the
    /// lowerer; this rejects them at check time (Core #10 lower-or-reject).
    InvalidAssignTarget,

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
            SemanticErrorKind::TypeInValuePosition { .. } => "E_TypeInValuePosition",
            SemanticErrorKind::NotAStruct { .. } => "E_NotAStruct",
            SemanticErrorKind::MissingTraitMethod { .. } => "E_MissingTraitMethod",
            SemanticErrorKind::NoMethodFound { .. } => "E_NoMethodFound",
            SemanticErrorKind::NonDerefContainerBareTrait { .. } => "E_NonDerefContainerBareTrait",
            SemanticErrorKind::AutoDerefWriteThroughReadGuard { .. } => "E_AutoDerefWriteThroughReadGuard",
            SemanticErrorKind::AutoDerefConsumingThroughGuard { .. } => "E_AutoDerefConsumingThroughGuard",
            SemanticErrorKind::UnwrapOnNonOptional { .. } => "E_UnwrapOnNonOptional",
            SemanticErrorKind::EnumerateOnNonIterator { .. } => "E_EnumerateOnNonIterator",
            SemanticErrorKind::DerefNonBox { .. } => "E_DerefNonBox",
            SemanticErrorKind::DefaultOpNonOptional { .. } => "E_DefaultOpNonOptional",
            SemanticErrorKind::LocalBorrowBind => "E_LocalBorrowBind",
            SemanticErrorKind::AmpInOperandPosition => "E_AmpInOperandPosition",
            SemanticErrorKind::MethodGenericInferenceFailed { .. } => "E_MethodGenericInferenceFailed",
            SemanticErrorKind::CannotInferType => "E_CannotInferType",
            SemanticErrorKind::NoFieldFound { .. } => "E_NoFieldFound",
            SemanticErrorKind::DerefCoercionUnimplemented { .. } => "E_DerefCoercionUnimplemented",
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
            SemanticErrorKind::UnhandledThrows { .. } => "E_UnhandledThrows",
            SemanticErrorKind::MissingFallibleMark { .. } => "E_MissingFallibleMark",
            SemanticErrorKind::InferredThrowsUnsupported => "E_InferredThrowsUnsupported",
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
            SemanticErrorKind::UnsupportedOperator { .. } => "E_UnsupportedOperator",
            SemanticErrorKind::InvalidAssignTarget => "E_InvalidAssignTarget",
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
            SemanticErrorKind::TypeInValuePosition { name, kind } => {
                let hint = match kind.as_str() {
                    "enum" => format!(" — did you mean a variant, e.g. `{name}.<Variant>(...)`?"),
                    "struct" | "type" => {
                        format!(" — did you mean to construct one, e.g. `{name}(...)`?")
                    }
                    _ => String::new(),
                };
                write!(
                    f,
                    "`{name}` names a type ({kind}) and cannot be used as a value here{hint}"
                )
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
            SemanticErrorKind::AutoDerefWriteThroughReadGuard { method, wrapper } => {
                write!(
                    f,
                    "cannot call write-face method `{method}` through a `{wrapper}` \
                     (RWLock read-only invariant; use `.write()` to obtain a `WriteGuard`, \
                     or call a read-face method instead). See docs/language-design.md §9.3."
                )
            }
            SemanticErrorKind::AutoDerefConsumingThroughGuard { method, wrapper } => {
                write!(
                    f,
                    "cannot call consuming-face (`!self`) method `{method}` through a `{wrapper}` \
                     (guard Drop invariant; release the guard first and consume the container). \
                     See docs/language-design.md §9.3."
                )
            }
            SemanticErrorKind::NonDerefContainerBareTrait { container, trait_ } => {
                write!(
                    f,
                    "`{container}[{trait_}]` cannot hold a bare trait — \
                     write `{container}[Box[{trait_}]]` to make the heap \
                     allocation explicit (concurrency containers require an \
                     owning wrapper for trait objects; D36, owner Q1 2026-07-28)"
                )
            }
            SemanticErrorKind::UnwrapOnNonOptional { method, type_ } => {
                write!(
                    f,
                    "method `{method}` requires an `Option` or `Result` receiver, \
                     but `{type_}` is neither"
                )
            }
            SemanticErrorKind::EnumerateOnNonIterator { type_ } => {
                write!(
                    f,
                    "`.enumerate()` requires an `Iterator[T]` receiver, but \
                     `{type_}` is `Iterable[T]` only (see `docs/book/05-collections.md` \
                     and `lib/std/iter.gg`). Use `.iter().enumerate()` — \
                     `.iter()` returns an iterator, which `.enumerate()` can then adapt"
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
            SemanticErrorKind::LocalBorrowBind => {
                write!(
                    f,
                    "cannot bind a mutable borrow (`&`) to a name — a place \
                     has one exclusive writer, and a named `&`-binding would \
                     alias a second writable path to it. Pass the borrow \
                     directly at a call site (`f(&x)`) or mutate the place \
                     itself (`x.push(..)`, `x.field = value`)"
                )
            }
            SemanticErrorKind::AmpInOperandPosition => {
                write!(
                    f,
                    "`&` is not valid in an operand (read) position — the \
                     sigil means \"the callee writes through this borrow\" \
                     and there is no callee here. Drop the `&` and read the \
                     place directly (`match c.fd:`, `x += c.fd`, `if c.fd > 5:`); \
                     use `&` only as a function/method argument (`f(&c.fd)`) \
                     or on a `for` iterable (`for x in &coll:`)"
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
            SemanticErrorKind::DerefCoercionUnimplemented { field, inner, wrapper } => {
                write!(
                    f,
                    "field `{field}` exists on `{inner}` but deref coercion (design-doc §9.4) \
                     is not yet implemented for `{wrapper}`"
                )
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
            SemanticErrorKind::UnhandledThrows { throws_type } => {
                // D29 flip: a MARKED call that cannot propagate here (non-throws
                // fn, no disposition). Teach handle / capture / propagate — never
                // surface the `Result[…]` desugar as a found-type (D23 contract).
                // Kind-neutral wording ("fail with", not "throws"): the same
                // message serves a `throws E` callee AND a declared-Result
                // (kind-2) callee, which does not literally `throw`.
                write!(
                    f,
                    "this call can fail with `{throws_type}` but the error is not \
                     handled here; handle it with `catch` or `rethrow`, capture its \
                     outcome in an explicitly-typed binding, or declare the enclosing \
                     function `throws {throws_type}` to propagate"
                )
            }
            SemanticErrorKind::MissingFallibleMark { throws_type, reason } => match reason {
                // Kind-neutral "fail with" — see the UnhandledThrows note.
                FallibleMarkReason::Bare => write!(
                    f,
                    "this call can fail with `{throws_type}` — mark it with `!` to \
                     propagate the error (`f()!`), handle it (`f()! catch (e): …` or \
                     `f()! rethrow (e): …`), or capture its outcome in an explicitly \
                     `Result`-typed binding"
                ),
                FallibleMarkReason::RedundantOnCapture => write!(
                    f,
                    "this call's outcome is captured by the explicitly-typed \
                     destination — remove the `!` (the annotation makes the \
                     fallibility visible; the mark would instead propagate the error)"
                ),
                FallibleMarkReason::ResultArmsOnPeeled => write!(
                    f,
                    "the marked call `f()!` peels to its success value, so these \
                     `Ok`/`Error` arms cannot match its whole outcome — capture the \
                     outcome in an explicitly `Result`-typed binding first, then \
                     `match` that binding"
                ),
                FallibleMarkReason::MarkOnInfallible => write!(
                    f,
                    "this `!` does not mark a fallible call — `!` attaches to a \
                     call whose callee is declared `throws` or returns \
                     `Result[T, E]`, exactly one mark per call; remove it"
                ),
            },
            SemanticErrorKind::InferredThrowsUnsupported => write!(
                f,
                "inferred error sets (a bare `!` on the signature) are not yet \
                 implemented — declare the error contract explicitly with `throws E`"
            ),
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
            SemanticErrorKind::MoveWithoutOperator { name, reason, shape, write_through_available } => {
                // The "why" clause depends on the reason; the REMEDY depends on
                // the place shape (D12 pin-4). `!` is today's move sigil — a
                // `# D27: !→^` breadcrumb marks it for D27's re-sigil sweep
                // (do NOT switch to `^` here: it does not parse yet).
                let why = match reason {
                    MoveReason::DropTaint => "a resource (a type with a custom `Drop` is single-owner)",
                    MoveReason::SingleOwner => "a single-owner type (no implicit copy)",
                };
                match shape {
                    // Materialize-on-write position (2T/D2): re-declaring the
                    // root `&self` / `&<param>` writes through, so that is the
                    // PRIMARY remedy the ledger names first — it leads, then the
                    // `!` move / `.clone()` copy. This flavor fires ONLY when
                    // `write_through_available` is set (the reject helper's
                    // assign / compound / receiver / `&`-formation sites); at
                    // ctor/field-init/bind an `&` is not a valid fix, so those
                    // sites keep `write_through_available == false` and render
                    // the byte-identical no-`&` Whole arm below. D27: !→^
                    MoveShape::Whole if *write_through_available => {
                        if name == "self" {
                            write!(
                                f,
                                "cannot copy `self`: `self` is {why} — declare the method \
                                 `&self` to write through, or write `!self` to move or \
                                 `self.clone()` to copy"
                            )
                        } else {
                            write!(
                                f,
                                "cannot copy `{name}`: `{name}` is {why} — declare the parameter \
                                 `&{name}` to write through, or write `!{name}` to move or \
                                 `{name}.clone()` to copy"
                            )
                        }
                    }
                    // D27: !→^
                    MoveShape::Whole => write!(
                        f,
                        "cannot copy `{name}`: `{name}` is {why} — write `!{name}` to move \
                         or `{name}.clone()` to copy"
                    ),
                    // Field / index sub-place: a bare `!` on the sub-place would
                    // be a PARTIAL move (rejected), so `.clone()` is the only
                    // remedy. `{name}` is the ROOT (the exact sub-place text is a
                    // filed LOW follow-up). D27: !→^
                    MoveShape::FieldIndex => write!(
                        f,
                        "cannot copy `{name}`: `{name}` is {why} — copy the sub-place with \
                         `{name}.clone()` (a bare `!` on a field/index sub-place is a partial \
                         move and is rejected)"
                    ),
                    // Capture: NEITHER `!` NOR `.clone()` is a valid remedy (no
                    // capture-list syntax; a `.clone()`-into-local is equally
                    // tainted). Pass it as an argument or share it. NO `!` here.
                    MoveShape::Capture => write!(
                        f,
                        "cannot capture `{name}` by value: `{name}` is {why}, so the capture \
                         would be an implicit copy — pass it as an argument or wrap it in `Shared[T]`"
                    ),
                }
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
                arg_is_temp,
            } => {
                use crate::parser::ast::Ownership::*;
                // Kind-aware explanation + an auto-fixable `help:` (the D31
                // ADDENDUM-2 DX rider). The explanation names the API CONTRACT
                // the call site must spell; the help gives the exact edit.
                let (expl, help): (String, String) = match (expected, found) {
                    (Move, Borrow) => (
                        if *arg_is_temp {
                            "this call consumes the value — the parameter takes ownership (`!`), \
                             so the move is spelled at the call site even for a temporary"
                                .to_string()
                        } else {
                            "this call consumes the argument — the parameter takes ownership (`!`), \
                             so the move is spelled at the call site"
                                .to_string()
                        },
                        "mark the argument with `!` (e.g. `f(!x)` or `f(!Ctor(...))`)".to_string(),
                    ),
                    (MutableBorrow, Borrow) => (
                        "this call mutates the argument through the parameter (`&`, write-through), \
                         so the mutable borrow is spelled at the call site"
                            .to_string(),
                        "mark the argument with `&` (e.g. `f(&x)`)".to_string(),
                    ),
                    (Borrow, Move) => (
                        "the parameter only borrows — it does not consume the argument".to_string(),
                        "remove the `!` (the value is not moved into this call)".to_string(),
                    ),
                    (Borrow, MutableBorrow) => (
                        "the parameter only borrows immutably — it is not a write-through (`&`) param"
                            .to_string(),
                        "remove the `&` (the callee does not mutate through this argument)"
                            .to_string(),
                    ),
                    (Move, MutableBorrow) => (
                        "this call consumes the argument (`!`), not a mutable borrow".to_string(),
                        "write `!` instead of `&`".to_string(),
                    ),
                    (MutableBorrow, Move) => (
                        "this call mutates the argument through a write-through (`&`) param, \
                         not a move"
                            .to_string(),
                        "write `&` instead of `!`".to_string(),
                    ),
                    // Equal modes never construct this error; render defensively.
                    _ => (
                        "the call-site sigil does not match the parameter declaration".to_string(),
                        "match the parameter's ownership at the call site".to_string(),
                    ),
                };
                write!(f, "ownership mismatch for `{param_name}`: {expl}; help: {help}")
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
            SemanticErrorKind::UnsupportedOperator { op, type_name } => {
                // Teaching messages: String only has concat; user types need equip.
                if type_name == "String" {
                    write!(
                        f,
                        "operator `{op}` is not defined for type `String` — \
                         String supports `+`/`+=` (concatenation) only"
                    )
                } else {
                    let (trait_name, method) = match op.as_str() {
                        "+" | "+=" => ("Add", "add"),
                        "-" | "-=" => ("Sub", "sub"),
                        "*" | "*=" => ("Mul", "mul"),
                        "/" | "/=" => ("Div", "div"),
                        "%" | "%=" => ("Rem", "rem"),
                        "mod" => ("Mod", "mod"),
                        _ => ("the matching operator trait", "the operator method"),
                    };
                    if trait_name.starts_with("the ") {
                        write!(
                            f,
                            "operator `{op}` is not defined for type `{type_name}` \
                             — only integer numeric types support this operator"
                        )
                    } else {
                        write!(
                            f,
                            "operator `{op}` is not defined for type `{type_name}` — \
                             equip with `{trait_name}[{type_name}]` (or implement \
                             `{method}`) to use `{op}`"
                        )
                    }
                }
            }
            SemanticErrorKind::InvalidAssignTarget => {
                write!(
                    f,
                    "invalid assignment target: the left side is not an \
                     assignable place — assign to a variable, field, tuple \
                     field, index (`v[i]`), or dereference (`*p`)"
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
        let k = SemanticErrorKind::MoveWithoutOperator {
            name: "x".into(),
            reason: MoveReason::DropTaint,
            shape: MoveShape::Whole,
            write_through_available: false,
        };
        assert_eq!(k.code(), "E_MoveWithoutOperator");
    }

    fn mwo(name: &str, reason: MoveReason, shape: MoveShape) -> String {
        // Message text is rendered by `impl Display for SemanticError`.
        // The bare-boundary helper renders the no-`&` flavor (ctor/bind/etc);
        // the write-through flavor is covered by `mwo_write_through` below.
        SemanticError {
            kind: SemanticErrorKind::MoveWithoutOperator {
                name: name.into(),
                reason,
                shape,
                write_through_available: false,
            },
            span: sp(),
        }
        .to_string()
    }

    fn mwo_write_through(name: &str, reason: MoveReason) -> String {
        // The materialize-on-write flavor: `&self` / `&<param>` write-through
        // leads. Always a Whole place (the reject helper's only shape).
        SemanticError {
            kind: SemanticErrorKind::MoveWithoutOperator {
                name: name.into(),
                reason,
                shape: MoveShape::Whole,
                write_through_available: true,
            },
            span: sp(),
        }
        .to_string()
    }

    /// D12 pin-4: each place shape advertises the CORRECT remedy.
    #[test]
    fn move_without_operator_per_shape_messages() {
        // Whole (drop-taint): both `!` move and `.clone()` are valid.
        let whole = mwo("x", MoveReason::DropTaint, MoveShape::Whole);
        assert!(whole.contains("!x"), "whole should offer `!x`: {whole}");
        assert!(whole.contains("x.clone()"), "whole should offer clone: {whole}");
        assert!(whole.contains("resource"), "whole drop-taint why-clause: {whole}");

        // Whole (single-owner): the why-clause differs, remedies are the same.
        let so = mwo("g", MoveReason::SingleOwner, MoveShape::Whole);
        assert!(so.contains("single-owner"), "single-owner why-clause: {so}");
        assert!(so.contains("!g") && so.contains("g.clone()"), "so remedies: {so}");

        // Field/Index sub-place: `.clone()` ONLY — a bare `!` is a partial move.
        let field = mwo("hh", MoveReason::DropTaint, MoveShape::FieldIndex);
        assert!(field.contains("hh.clone()"), "sub-place offers clone: {field}");
        assert!(field.contains("partial move"), "sub-place warns partial move: {field}");
    }

    /// 2T/D2 discriminator: the materialize-on-write flavor leads with the
    /// `&self` / `&<param>` write-through remedy the ledger names first, THEN
    /// offers `!` move / `.clone()` copy — and the no-`&` bare-boundary flavor
    /// (`write_through_available == false`) stays byte-identical to the
    /// pre-discriminator text (so ctor/bind/field-init `.expected` files never
    /// churn and no `&` is ever offered where it is an invalid fix).
    #[test]
    fn move_without_operator_write_through_flavor() {
        // Self root → "declare the method `&self` to write through".
        let self_msg = mwo_write_through("self", MoveReason::DropTaint);
        assert!(self_msg.contains("declare the method `&self` to write through"),
            "self write-through hint: {self_msg}");
        assert!(self_msg.contains("!self") && self_msg.contains("self.clone()"),
            "self still offers move/copy: {self_msg}");

        // Param root → "declare the parameter `&<name>` to write through".
        let param_msg = mwo_write_through("fh", MoveReason::DropTaint);
        assert!(param_msg.contains("declare the parameter `&fh` to write through"),
            "param write-through hint: {param_msg}");
        assert!(param_msg.contains("!fh") && param_msg.contains("fh.clone()"),
            "param still offers move/copy: {param_msg}");

        // The no-`&` Whole flavor is UNCHANGED and offers no `&` remedy — this
        // is the exact text ctor/bind/field-init positions render.
        let bare = mwo("fh", MoveReason::DropTaint, MoveShape::Whole);
        assert_eq!(
            bare,
            "cannot copy `fh`: `fh` is a resource (a type with a custom `Drop` \
             is single-owner) — write `!fh` to move or `fh.clone()` to copy",
            "bare Whole flavor must stay byte-identical (no `&` offered): {bare}"
        );
    }

    /// D12 pin-4 GATE (reference-grade): the CAPTURE-position message must
    /// advertise NEITHER `!` NOR `.clone()` — no capture-list syntax exists and
    /// a `.clone()`-into-local is equally drop-tainted. Executable guard so a
    /// future wording change cannot regress the remedy.
    #[test]
    fn move_without_operator_capture_message_has_no_bang() {
        let cap = mwo("hh", MoveReason::DropTaint, MoveShape::Capture);
        assert!(
            !cap.contains('!'),
            "capture message must contain no `!`: {cap}"
        );
        assert!(
            !cap.contains(".clone()"),
            "capture message must not advertise `.clone()`: {cap}"
        );
        assert!(
            cap.contains("Shared[T]") && cap.contains("pass it as an argument"),
            "capture message must offer pass-as-arg / Shared[T]: {cap}"
        );
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
            (
                SemanticErrorKind::UnsupportedOperator {
                    op: "-=".into(),
                    type_name: "String".into(),
                },
                "E_UnsupportedOperator",
            ),
            (SemanticErrorKind::InvalidAssignTarget, "E_InvalidAssignTarget"),
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
