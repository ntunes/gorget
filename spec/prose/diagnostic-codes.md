# Diagnostic-code registry

> **Phase-1 deliverable (RFC §5.5).** Stable, symbolic `E_`-codes for the
> compiler's semantic diagnostics: this registry maps **code → diagnostic kind →
> prose section**. Production adopts the codes incrementally; the static-error /
> parse-error conformance tiers reference a code via `expect: code: E_...`.

## Source of truth & the ratchet

The codes are defined — one per variant, no catch-all — on the diagnostic enums:

- `SemanticErrorKind::code()` (`src/semantic/errors.rs`) → the 98 `E_` codes below.
- `SemanticWarningKind::code()` (`src/semantic/errors.rs`) → the 17 `W_` codes below.

Each `code()` is an **exhaustive `match` mirroring the `Display` impl** with **no**
`_` catch-all, so `rustc`'s exhaustiveness check IS the registry ratchet: a new
variant added without a code is a hard build error, never a silent gap (CLAUDE.md
core-invariant #6 — convert a recurring class into an executable guard).

## Naming scheme

The scheme is uniform **`E_<VariantName>`** (errors) / **`W_<VariantName>`**
(warnings) — the code is mechanically the variant identity with a severity prefix.
This is simultaneously **systematic** (derived, exhaustive by construction) and
**meaningful** (the variant names are descriptive), so it needs no separate
"meaningful vs. placeholder" split. Tying the code to the variant identity — not a
sequence number — means codes are stable against re-numbering and never collide
across branches, which **sidesteps the `E_`-numbering bikeshed left open in RFC §8
(item 3)**. Stability is modulo variant RENAME: renaming a variant changes its
code, so **this registry file is the stability contract** — a rename that touches
a published code is a breaking spec change and must be recorded here. The one
normative anchor, `E_MoveWithoutOperator` (RFC §5.5 / §2.2 bullet 4), matches
exactly.

## Rendering

`ErrorReporter::report_semantic_error` (`src/errors.rs`) threads the code via
codespan-reporting's `Diagnostic::with_code`, so a semantic error renders as
`error[E_UndefinedName]: undefined name \`x\``. **Warning codes are NOT rendered in
phase 1** (RFC/brief pass-2 fold R-e): `report_semantic_warning` does not yet thread
`.with_code(...)`. `SemanticWarningKind::code()` exists for the ratchet and this
registry; warning-code rendering is deferred to a later increment.

The lex-time and parse-time diagnostics (`LexErrorKind`, `ParseErrorKind`) are not
yet coded — they render with the bare `error:` prefix as before, and are a
follow-up increment.

## `—` in the prose column

A `—` means the phase-1 normative prose section for that diagnostic has not been
authored yet. The code is **stable and registered now** so fixtures can reference
it before the prose lands. The §2.2 ownership/borrow/move/lifetime cluster already
has prose (files [`01`](01-implicit-copy-positions.md)–[`05`](05-resource-exhaustion.md))
and is cited concretely.

## Error codes (`E_`)

| Code | Kind (`SemanticErrorKind`) | Prose section |
|---|---|---|
| `E_UndefinedName` | `UndefinedName` | — |
| `E_DuplicateDefinition` | `DuplicateDefinition` | — |
| `E_TypeMismatch` | `TypeMismatch` | — |
| `E_WrongArgCount` | `WrongArgCount` | — |
| `E_NotAFunction` | `NotAFunction` | — |
| `E_NotAType` | `NotAType` | — |
| `E_NotAStruct` | `NotAStruct` | — |
| `E_MissingTraitMethod` | `MissingTraitMethod` | — |
| `E_NoMethodFound` | `NoMethodFound` | — |
| `E_UnwrapOnNonOptional` | `UnwrapOnNonOptional` | — |
| `E_DerefNonBox` | `DerefNonBox` | — |
| `E_DefaultOpNonOptional` | `DefaultOpNonOptional` | — |
| `E_DefaultOpRhsTypeMismatch` | `DefaultOpRhsTypeMismatch` | — |
| `E_LocalBorrowBind` | `LocalBorrowBind` | [`03`](03-writethrough-and-move.md) |
| `E_MethodGenericInferenceFailed` | `MethodGenericInferenceFailed` | — |
| `E_CannotInferType` | `CannotInferType` | — |
| `E_NoFieldFound` | `NoFieldFound` | — |
| `E_DerefCoercionUnimplemented` | `DerefCoercionUnimplemented` | — |
| `E_TupleIndexOutOfBounds` | `TupleIndexOutOfBounds` | — |
| `E_OrPatternBindingMismatch` | `OrPatternBindingMismatch` | — |
| `E_DuplicateImpl` | `DuplicateImpl` | — |
| `E_PrimitiveTraitImpl` | `PrimitiveTraitImpl` | — |
| `E_RecursiveTypeNeedsBox` | `RecursiveTypeNeedsBox` | — |
| `E_TraitCycle` | `TraitCycle` | — |
| `E_MethodSignatureMismatch` | `MethodSignatureMismatch` | — |
| `E_BreakOutsideLoop` | `BreakOutsideLoop` | — |
| `E_ContinueOutsideLoop` | `ContinueOutsideLoop` | — |
| `E_ReturnOutsideFunction` | `ReturnOutsideFunction` | — |
| `E_ThrowInNonThrowingFunction` | `ThrowInNonThrowingFunction` | — |
| `E_RethrowInNonThrowingFunction` | `RethrowInNonThrowingFunction` | — |
| `E_OnErrorInNonThrowingFunction` | `OnErrorInNonThrowingFunction` | — |
| `E_MainThrowsNonInt` | `MainThrowsNonInt` | — |
| `E_UnconvertibleErrorPropagation` | `UnconvertibleErrorPropagation` | — |
| `E_UnhandledThrows` | `UnhandledThrows` | — |
| `E_MissingFallibleMark` | `MissingFallibleMark` | — |
| `E_InferredThrowsUnsupported` | `InferredThrowsUnsupported` | — |
| `E_AwaitOutsideAsync` | `AwaitOutsideAsync` | — |
| `E_SelectOutsideAsync` | `SelectOutsideAsync` | — |
| `E_AwaitNonFuture` | `AwaitNonFuture` | — |
| `E_SpawnNonFuture` | `SpawnNonFuture` | — |
| `E_BorrowAcrossAwait` | `BorrowAcrossAwait` | — |
| `E_SpawnWithBorrowedRef` | `SpawnWithBorrowedRef` | — |
| `E_SpawnRequiresDirectCall` | `SpawnRequiresDirectCall` | — |
| `E_SpawnClosureCaptureBorrowed` | `SpawnClosureCaptureBorrowed` | — |
| `E_SpawnClosureCaptureMutable` | `SpawnClosureCaptureMutable` | — |
| `E_SpawnClosureCaptureShared` | `SpawnClosureCaptureShared` | — |
| `E_UseAfterMove` | `UseAfterMove` | [`03`](03-writethrough-and-move.md) |
| `E_MoveWithoutOperator` | `MoveWithoutOperator` | [`04`](04-drop-purity.md), [`03`](03-writethrough-and-move.md) |
| `E_BorrowConflict` | `BorrowConflict` | [`02`](02-borrow-and-materialize-on-write.md) |
| `E_MoveInLoop` | `MoveInLoop` | [`03`](03-writethrough-and-move.md) |
| `E_DoubleMove` | `DoubleMove` | [`03`](03-writethrough-and-move.md) |
| `E_NonPrintableInterpolation` | `NonPrintableInterpolation` | — |
| `E_OwnershipMismatch` | `OwnershipMismatch` | — |
| `E_UnsatisfiedTraitBound` | `UnsatisfiedTraitBound` | — |
| `E_NonExhaustiveMatch` | `NonExhaustiveMatch` | — |
| `E_MissingReturn` | `MissingReturn` | — |
| `E_NoreturnBodyReturns` | `NoreturnBodyReturns` | — |
| `E_NoreturnWithThrows` | `NoreturnWithThrows` | — |
| `E_StringIndexAssign` | `StringIndexAssign` | — |
| `E_UnloweredBuiltinCall` | `UnloweredBuiltinCall` | — |
| `E_UnknownNamedArg` | `UnknownNamedArg` | — |
| `E_DuplicateNamedArg` | `DuplicateNamedArg` | — |
| `E_MissingRequiredArg` | `MissingRequiredArg` | — |
| `E_PositionalAfterNamed` | `PositionalAfterNamed` | — |
| `E_UnknownDirective` | `UnknownDirective` | — |
| `E_UnderivableTrait` | `UnderivableTrait` | — |
| `E_DeriveFromRequiresSingleField` | `DeriveFromRequiresSingleField` | — |
| `E_FieldMissingDerivedTrait` | `FieldMissingDerivedTrait` | — |
| `E_AssignmentToConst` | `AssignmentToConst` | — |
| `E_NonConstantConstInitializer` | `NonConstantConstInitializer` | — |
| `E_ViaWithoutTrait` | `ViaWithoutTrait` | — |
| `E_ViaFieldNotFound` | `ViaFieldNotFound` | — |
| `E_ViaFieldTypeMissingTrait` | `ViaFieldTypeMissingTrait` | — |
| `E_DuplicateSuiteBlock` | `DuplicateSuiteBlock` | — |
| `E_InvalidFnTraitArg` | `InvalidFnTraitArg` | — |
| `E_ClosureKindMismatch` | `ClosureKindMismatch` | — |
| `E_ValueOutOfRange` | `ValueOutOfRange` | — |
| `E_UnsafeIntegerConversion` | `UnsafeIntegerConversion` | — |
| `E_DanglingReturn` | `DanglingReturn` | [`02`](02-borrow-and-materialize-on-write.md) |
| `E_UseAfterSourceMoved` | `UseAfterSourceMoved` | [`03`](03-writethrough-and-move.md) |
| `E_MutationWhileBorrowed` | `MutationWhileBorrowed` | [`02`](02-borrow-and-materialize-on-write.md) |
| `E_TemporaryBorrow` | `TemporaryBorrow` | [`02`](02-borrow-and-materialize-on-write.md) |
| `E_InvalidParameterMode` | `InvalidParameterMode` | — |
| `E_UnresolvedBorrowOrigin` | `UnresolvedBorrowOrigin` | [`02`](02-borrow-and-materialize-on-write.md) |
| `E_ArenaEscape` | `ArenaEscape` | — |
| `E_MetaEvalError` | `MetaEvalError` | — |
| `E_OrphanImpl` | `OrphanImpl` | — |
| `E_DoubleAwait` | `DoubleAwait` | — |
| `E_ReadWhileMutCaptured` | `ReadWhileMutCaptured` | — |
| `E_WriteWhileMutCaptured` | `WriteWhileMutCaptured` | — |
| `E_PrivateImport` | `PrivateImport` | — |
| `E_UnresolvedImport` | `UnresolvedImport` | — |
| `E_PrivateTypeInPublicSignature` | `PrivateTypeInPublicSignature` | — |
| `E_RequiredAfterDefault` | `RequiredAfterDefault` | — |
| `E_DuplicateStructField` | `DuplicateStructField` | — |
| `E_DuplicateStructFieldDecl` | `DuplicateStructFieldDecl` | — |
| `E_WrongFieldCount` | `WrongFieldCount` | — |
| `E_ClosureEscapesScope` | `ClosureEscapesScope` | [`02`](02-borrow-and-materialize-on-write.md) |
| `E_MutexDoubleLock` | `MutexDoubleLock` | — |

## Warning codes (`W_`) — not rendered in phase 1

| Code | Kind (`SemanticWarningKind`) | Prose section |
|---|---|---|
| `W_UnnecessaryShared` | `UnnecessaryShared` | — |
| `W_StaleSharedCondition` | `StaleSharedCondition` | — |
| `W_WithCheckThenAct` | `WithCheckThenAct` | — |
| `W_StaleSharedWriteBack` | `StaleSharedWriteBack` | — |
| `W_SharedIteratorInvalidation` | `SharedIteratorInvalidation` | — |
| `W_SpawnWithTrackedBinding` | `SpawnWithTrackedBinding` | — |
| `W_CompoundYieldRace` | `CompoundYieldRace` | — |
| `W_ClosureCapturesWithBinding` | `ClosureCapturesWithBinding` | — |
| `W_UnreachableCode` | `UnreachableCode` | — |
| `W_UnusedVariable` | `UnusedVariable` | — |
| `W_UnusedImport` | `UnusedImport` | — |
| `W_UncheckedUnwrap` | `UncheckedUnwrap` | — |
| `W_CouldBeConst` | `CouldBeConst` | — |
| `W_NeedlessMutableBorrow` | `NeedlessMutableBorrow` | — |
| `W_DeadBareParamWrite` | `DeadBareParamWrite` | [`11`](../../docs/devbook/11-copy-on-write.md) — D2-rider; extended to plain `self` 2026-07-17. Promotes to a reserved `E_DeadBareParamWrite` after corpus burn-down (Core-#6 ratchet). |
| `W_RecursiveBareParamMaterialize` | `RecursiveBareParamMaterialize` | [`11`](../../docs/devbook/11-copy-on-write.md) — Charter-accepted §3.1 exception; steers users to `&param` + `&arg` at callers (write-through) OR explicit `.clone()` (per-frame copies) for a bare-Res param mutated inside a self-recursive call. |
| `W_CowBorrowMutation` | `CowBorrowMutation` | — |
| `W_SuggestThrowsRefactor` | `SuggestThrowsRefactor` | — |

<!-- cites: src/semantic/errors.rs::SemanticErrorKind::code -->
<!-- cites: src/semantic/errors.rs::SemanticWarningKind::code -->
<!-- cites: src/errors.rs::ErrorReporter::report_semantic_error -->
