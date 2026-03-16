# `shared` Keyword with Token Semantics — Design

## Overview

The `shared` keyword extends the borrow checker's guarantees across thread boundaries.
The compiler manages synchronization automatically using **token semantics** (inspired by
DragonFlyBSD's kernel token model): locks are acquired for synchronous execution regions
and released at suspension points.

**CFA** in this document refers to **Custody Flow Analysis** — the compiler pass that tracks
which shared bindings flow across concurrency boundaries (spawn/await) and determines the
synchronization strategy each custodian requires. CFA builds on the borrow checker's
infrastructure but asks a different question: not "is this access safe?" but "what
synchronization does this custody transfer need?"

## Surface Syntax

```
shared int count = 0                           # CFA decides: ARC + Mutex (if mutable borrows cross boundaries)
shared Config config = load_config()           # CFA decides: ARC only (if only immutable borrows)
shared(rwlock) Dict[str, str] cache = Dict()   # User override: ARC + RwLock
shared(atomic) int flags = 0                   # User override: ARC + Atomic (scalars only)
```

## Semantic Model

A `shared` binding is:
1. **Always ARC-wrapped** — heap-allocated, atomically reference-counted
2. **Optionally sync-wrapped** — CFA determines whether a Mutex/RwLock/Atomic is also needed

| Borrows across spawn/await | Auto strategy | Codegen type |
|---|---|---|
| None (local-only use) | ARC only (warn: unnecessary) | `Shared[T]` |
| Immutable only | ARC only | `Shared[T]` |
| Mutable (`&T` params) | ARC + Mutex | `Shared[Mutex[T]]` |

Functions receive plain types — sync wrapper is the caller's concern (callee transparency).

## Token Semantics

A **token** is a lock that is:
- **Acquired** when entering a synchronous execution region accessing shared mutable bindings
- **Released** at suspension points (`.await()`) and at task/thread completion
- **Reacquired** when execution resumes after suspension

Multiple shared bindings: tokens acquired in declaration order (by DefId), released in reverse.

## Implementation Status

- **Phase 1 (Parser + AST):** DONE — `SharedKind` enum, `shared` field on VarDecl, parser support
- **Phases 2-4 (CFA + IR + Codegen):** DONE — CFA infrastructure in borrow checker, `SharedStrategy` enum, Mutex wrapping in IR lowering, spawn boundary relaxation
- **Phase 5 (Transparent Access):** DONE — reads/writes/compound-assigns auto-lock; spawned functions take explicit `Mutex[T]`
- **Phase 6 (CFA ArcOnly Optimization):** DONE — read-only shared bindings use `Shared[T]` (no locking); write tracking in Assign/CompoundAssign; param-type-aware CFA (Mutex[T] params force ArcMutex)
- **Phase 7 (`shared(atomic)` Codegen):** DONE — `shared(atomic) int`/`bool` uses AtomicInt/AtomicBool (lock-free ops); transparent read=load, write=store, `+=`=add, `-=`=sub; type validation (int/bool only); AtomicInt/AtomicBool ↔ T coercion; drop via `gorget_atomic_*_free`; CFA param-type-aware (AtomicInt/AtomicBool params force ArcAtomic)
- **Phase 0 (Async Mutex):** DONE — `gorget_mutex_poll_lock(m, out, waker)` with trylock + waker queue; `gorget_guard_release` wakes one waiter (FIFO); `gorget_mutex_free` for proper cleanup; blocking `gorget_mutex_lock` retained for sync contexts
- **Token Release/Reacquire at Await (§3.1/§3.2):** DONE — async functions with shared params release tokens before `.await()`, reacquire after resume; `build_shared_async_variant` re-lowers function body with `Shared[Mutex[T]]` params; spawn site detects async+await callees via AST scanner
- **Multi-Token Ordering (§3.3):** DONE — tokens acquired in ascending declaration order, released in reverse; deadlocks impossible by construction
- **Missing `shared` Suggestion (§7.2):** DONE — `SpawnWithBorrowedRef` error includes variable name and suggests `shared` declaration
- **Unnecessary `shared` Warning (§7.4):** DONE — warns when shared binding never crosses concurrency boundary
- **Phase 8 (Stale-Condition Warning §3.4):** DONE — locals derived from shared bindings are tracked; at await points, they become stale; using stale locals in If/While/Match conditions emits a warning; reassignment clears staleness
- **Phase 9 (Tests + Polish):** DONE — 7 new integration tests (float, string, atomic error, stale while/match, multi-spawn, early return). Fixed `c_type_name_for_id` bug (primitives fell through to int64_t). Updated outdated comments.

## See Also

Full design specification is in the implementation plan that generated this document.
