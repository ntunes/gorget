# Safety Checker — Contributor Guide

> **Module:** `src/semantic/safety/`
> **Entry point:** `pub fn check_module()` in `mod.rs`
> **Runs after:** name resolution (Pass 2), type checking (Pass 4), string provenance (Pass 4.5)

The safety checker enforces ownership, lifetime, and concurrency invariants.
It is the final semantic pass before IR lowering.

> **String provenance:** Pass 4.5 (`src/semantic/provenance.rs`) runs between
> type checking and safety checking. It infers view vs owned representation for
> `String`-typed bindings. The safety checker sees the provenance-adjusted TypeIds
> — view strings are Copy (no lifetime tracking), owned strings are Move.

## Module Layout

```
safety/
  mod.rs             Core types, BorrowChecker struct, check_module entry point
  type_utils.rs      is_copy_type, ref-type struct detection
  origins.rs         BorrowOrigin tracking, branch state save/restore/merge
  helpers.rs         Concurrency checks, call ownership, spawn safety
  check_expr.rs      Expression walker (check_expr)
  check_stmt.rs      Statement/block/function walker (check_stmt, check_function)
  return_borrows.rs  Pass 5a: return borrow inference, closure visitors, alias map
  validation.rs      Private-in-public, unused imports, purity inference
  tests.rs           Unit tests
```

All `BorrowChecker` fields are `pub(super)` so `impl` blocks can be split
across files while keeping the struct private to the module.

## Pass Order

`check_module()` orchestrates four sequential passes:

```
Phase 4    compute_ref_type_structs       which structs contain ref-type fields
           compute_struct_field_ref_flags  per-field boolean flags

Pass 5a    compute_all_return_borrows     which params appear in return values
                                          (populates FunctionInfo.return_borrows_from)

Pass 5b½   infer_purity                   classify functions as Pure/ReadOnly/
                                          MutatesArgs/HasSideEffects

Pass 5b    BorrowChecker::new + walk      full ownership/lifetime/concurrency check
           final CFA pass                 assign ArcMutex/ArcOnly to shared vars
           unused import detection
           private-in-public detection
```

Pass 5a must run before 5b because `compute_expr_origin()` in the main walk
needs `return_borrows_from` to trace origins through function calls.

## BorrowOrigin — The Core Abstraction

Every reference-typed variable gets an origin tracking where its data lives:

```
Static                              string literal, global constant — always valid
Param { param_index, def_id }      function parameter — valid in caller's scope
Local(def_id)                       local variable — can't escape the function
MatchBinding { binding_def_id,      match binding — borrows from scrutinee if
               scrutinee_origin,    is_ref=true, else owns data independently
               is_ref }
CallResult(Vec<BorrowOrigin>)       union of origins from return_borrows_from args
Unknown                             conservative fallback — treated as local
```

Origins propagate through expressions:

- Field access, indexing, tuple access: inherit from the object
- Calls: look up `return_borrows_from` on the callee, extract origins from
  the corresponding arguments
- If/match: union of all branch origins
- Struct literals: union of reference-type field arguments
- Closures: union of captured ref-type variables
- Literals, operators, ranges: `Static`

## Pass 5a — Return Borrow Inference

**Goal:** For each function, determine which parameter indices appear in the
return value's lifetime. Stored in `FunctionInfo.return_borrows_from`.

**Algorithm** (`compute_function_return_borrows` in `return_borrows.rs`):

1. **Body analysis** — Build a `LocalAliasMap` (local name -> set of param
   indices it may alias), then trace return expressions backward through
   assignments and calls to find which params contribute.

2. **Elision fallback** — If body analysis yields nothing:
   - Single ref-type param -> that param
   - First param named `self` -> self
   - No ref-type params -> mark `return_origin_is_static` (fresh data)

The `LocalAliasMap` is built by walking all statements before tracing.
Assignments like `auto x = param_a` create alias entries. Calls propagate
through the callee's own `return_borrows_from`.

### Closure Visitors

Four `ExprVisitor` structs walk closure bodies for different purposes:

| Struct | Collects | Used By |
|--------|----------|---------|
| `CapturedRefOriginCollector` | Origins of captured ref-type variables | `compute_expr_origin` for closures |
| `CapturedMutationCollector` | Names of mutated captured variables | Capture mode classification |
| `CaptureSetCollector` | Full capture set (name, mode, origin) | Spawn enforcement |
| `ClosureBodyParamTracer` | Param indices referenced in closure | Pass 5a alias tracing |

All skip nested closures (they have their own capture scope).

## Pass 5b — The Main Walk

`check_items_recursive` dispatches to `check_function` for each function,
method, test, and bench block. Each resets all per-function state.

`check_function` -> `check_block` -> `check_stmt` -> `check_expr` forms the
recursive AST walk. The checker maintains state as it walks:

### Ownership Tracking

- `var_states: Map<DefId, VarState>` — `Live` or `Moved { moved_at }`
- On use: `check_use()` verifies the variable is `Live`
- On `!expr` (move): `check_move()` transitions to `Moved`, adds to
  `invalidated_origins`, checks for double-move and move-in-loop

Move-in-loop is allowed for variables declared inside the loop body
(tracked via `loop_local_defs` stack).

### Lifetime Validation

- `var_origins: Map<DefId, BorrowOrigin>` — set on declaration
- `invalidated_origins: Set<DefId>` — grows as variables are moved
- On return: if origin `contains_local()`, error (dangling reference)
- On use after source moved: if origin `references_def(moved_id)`, error

### Branching

`save_branch_state()` snapshots all mutable state before each branch.
`merge_branch_states()` at the join point:

- Excludes diverging branches (return/break/throw) from the merge
- Unions `var_states` (moved in any branch = moved after)
- Unions `invalidated_origins` (conservative)
- Unions `mut_captured_vars`, `shared_derived`
- Fallible states: unchecked in any branch = unchecked

### For-Loop Iterator Safety

- `for_loop_iterables: Set<DefId>` — variables currently being iterated over
- Set on entry to a for-loop body, cleared on exit
- Any mutating method call on a variable in this set triggers
  `MutationWhileBorrowed` — the for-loop creates an implicit read-only
  borrow of the collection, and mutation would invalidate the iterator
- Applies to all mutating methods (push, pop, remove, clear, set, etc.),
  regardless of element type

### Async Safety

- `await_invalidated: Set<DefId>` — variables with non-static origins that
  were live before an await point
- Using these after await triggers `BorrowAcrossAwait`

### Closure Captures

Closures are classified by how they use captured variables:

- **Callable** — read-only captures
- **MutCallable** — mutates captured variables
- **ConsumeCallable** — moves captured variables

`mut_captured_vars` tracks which variables are mutably captured by live
closures. While any entry exists, direct reads/writes to that variable
are errors.

### Shared Variables & CFA

`shared` declarations trigger Custody Flow Analysis at spawn sites:

1. `cfa_at_spawn()` examines each spawn argument
2. If the argument is a shared binding, it checks the callee's param ownership
3. Decision: `ArcMutex` (mutable access), `ArcAtomic` (atomic types),
   `ArcRwLock` (explicit), or `ArcOnly` (read-only)
4. Upgrade rule: if already `ArcOnly` but now mutable, upgrade to `ArcMutex`

Stale-shared detection warns when a local derived from a shared variable
is used in a condition after an await point (the shared value may have
changed).

## Purity Inference

`infer_purity()` in `validation.rs` runs a two-pass fixed-point:

1. **Local pass** — Walk each function body, classify without considering callees:
   - Extern calls -> `HasSideEffects`
   - `&`/`!` params -> `MutatesArgs`
   - Global reads -> `ReadOnly`
   - Shared access, await, spawn -> `HasSideEffects`
   - No side effects -> `Pure`

2. **Propagation** — Iterate call graph until stable (max 100 iterations):
   - `caller_purity = local_purity JOIN max(callee_purities)`
   - Unknown callees default to `HasSideEffects`

Used by `is_yield_point_call()` in `helpers.rs` to determine whether a
call inside a `with` block might release the shared-variable token.

## Error Categories

The safety checker produces 85+ error kinds. Major categories:

| Category | Examples |
|----------|----------|
| Ownership | UseAfterMove, MoveInLoop, DoubleMove, OwnershipMismatch |
| Lifetimes | DanglingReturn, UseAfterSourceMoved, TemporaryBorrow |
| Concurrency | BorrowAcrossAwait, SpawnWithBorrowedRef, SpawnClosureCaptureMutable |
| Patterns | NonExhaustiveMatch (from type checker, validated here) |
| Arena | ArenaEscape (non-Copy value escaping arena scope) |

Warnings include `UnnecessaryShared`, `StaleSharedCondition`,
`UncheckedUnwrap`, `CouldBeConst`, and `UnusedVariable`.

## Adding a New Check

1. Add the error/warning kind to `src/semantic/errors.rs`
2. Implement the check in the appropriate file:
   - Expression-level: `check_expr.rs`
   - Statement-level: `check_stmt.rs`
   - Call-site validation: `helpers.rs`
   - Origin/lifetime: `origins.rs`
3. If the check needs new per-variable state, add a field to `BorrowChecker`
   in `mod.rs` (with `pub(super)`) and handle it in `save/merge_branch_states`
4. Add a unit test in `tests.rs`
