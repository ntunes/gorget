# Chapter 10 — Ownership: moves & borrows — the safety checker (Pass 5)

The **safety checker** (`src/semantic/safety/`) is the final semantic pass before
IR lowering. It enforces Gorget's ownership, lifetime, and concurrency invariants:
no use-after-move, no dangling references escaping a function, no data races across
`spawn`/`await` boundaries, and the single-owner discipline for closures, `Box[T]`,
`Task`, and friends. It is invoked from `semantic::analyze` as "Pass 5"
(`src/semantic/mod.rs:336-350`), after name resolution, type checking, and the
`lint:suggest_throws` lint. Its public entry point is `check_module`
(`mod.rs:525`), which returns the per-shared-binding synchronization strategies, a
warning list, the inferred per-function purity map, the borrow-dependency map
(consumed by the drop elaborator for drop ordering), and per-sub-pass timings.

This chapter describes the *implementation*: the nine submodules, the
`BorrowOrigin` abstraction, branch save/restore/merge, the concurrency/`shared`/spawn
machinery, and the Pass 5a return-borrow pre-pass. For the *language-level*
semantics of moves, borrows, and ownership, see `docs/language-reference.md` and the
"Ownership at Consuming Positions" section of `CLAUDE.md`.

## Module layout

The pass was carved out of a former monolithic `borrow.rs` into nine files. Line
counts move; the responsibilities are stable:

| File | Responsibility |
|------|----------------|
| `mod.rs` | Core types (`VarState`, `BorrowOrigin`, `BranchState`), the `BorrowChecker` struct, `check_module` orchestration |
| `type_utils.rs` | `is_copy_type`, AST ref-type detection, `compute_ref_type_structs` |
| `origins.rs` | `BorrowOrigin` computation, `check_use`/`check_move`, branch save/restore/merge |
| `helpers.rs` | Concurrency checks, call ownership/aliasing, spawn safety, CFA at spawn |
| `check_expr.rs` | Expression walker (`check_expr`) |
| `check_stmt.rs` | Statement/block/function walker (`check_function`, `check_block`, `check_stmt`) |
| `return_borrows.rs` | Pass 5a return-borrow inference + the four closure-body `ExprVisitor`s |
| `validation.rs` | Private-in-public, unused-import, purity inference |
| `tests.rs` | Unit tests |

All `BorrowChecker` fields are `pub(super)` (`mod.rs:241-418`) so the `impl` blocks
can be split across the submodules while keeping the struct private to the module.

> The string view/owned split is carried in the `TypeTable`, not a separate
> provenance pass: `types.string_id` is the Copy **view** representation (`Str`,
> a `const char*`-like non-owning slice) and `types.owned_string_id` is the
> non-Copy **owned** `String` (`src/semantic/types.rs:116-117`). The safety
> checker reads these TypeIds directly — e.g. owned-string VarDecl/Assign is
> treated as a view-creating borrow at `check_stmt.rs:1195`, and view-string
> params get `Param` origins at `check_stmt.rs:1436`. (An older internals doc
> described a standalone `provenance.rs` Pass 4.5; that file no longer exists.)

## Pass order inside `check_module`

`check_module` (`mod.rs:525`) runs four phases in sequence (`mod.rs:544-617`):

```
Phase 4   compute_ref_type_structs          which structs/enums contain ref-type fields
          compute_struct_field_ref_flags    per-field "is this field a reference?" bools
          compute_struct_field_mut_ref_flags per-field "is this a MutRef[T]/sigil &?" bools

Pass 5a   compute_all_return_borrows         populate FunctionInfo.return_borrows_from
                                             and return_origin_is_static

Pass 5b½  infer_purity                       classify functions Pure/ReadOnly/
                                             MutatesArgs/HasSideEffects

Pass 5b   BorrowChecker::new + check_items_recursive   the main ownership/lifetime/
                                                       concurrency walk
          final CFA pass                     assign ArcMutex/ArcOnly defaults to shared vars
          unused-import detection
          private-in-public detection
```

Pass 5a **must** precede 5b because `compute_call_origin`/`compute_method_call_origin`
in the main walk read `return_borrows_from` to trace origins through calls
(`origins.rs:326-465`). Purity is moved ahead of 5b deliberately
(`mod.rs:552-554`) so yield-point detection inside `with` blocks can consult it.

`check_items_recursive` (`mod.rs:627`) dispatches `Item::Function` and each
`Item::Equip` method to `check_function`, and `Test`/`Bench`/`SuiteSetup`/
`SuiteTeardown` bodies to `check_block` after a state reset. `Item::Module`
(an imported module) recurses with `imported_module_depth` bumped — while that
depth is `> 0`, dangling-return checks are skipped, because imported code is
validated by its own project's borrow checker and cross-module origin tracking is
conservative (built-in methods aren't in `method_resolutions`, so origins fall back
to `Local` and would false-positive) (`mod.rs:296-299`, `630-634`).

## `BorrowOrigin` — the core abstraction

Every reference-typed value gets a `BorrowOrigin` (`mod.rs:67-89`) tracking *where
its data lives*, so the checker can reject references that escape their source:

```
Static                                       string literal / global const — always valid
Param { param_index, def_id }                function parameter — valid in caller's scope
Local(def_id)                                local variable — must not escape the function
MatchBinding { binding_def_id,               match binding: borrows from scrutinee if
               scrutinee_origin, is_ref }     is_ref, else owns its extracted data
CallResult(Vec<BorrowOrigin>)                union of origins from the callee's
                                             return_borrows_from arguments
Owned                                        heap-allocated owned value (f-string,
                                             string concat) — local lifetime, no DefId
Unknown                                      conservative fallback — treated as local
```

The three escape-analysis predicates are the heart of lifetime checking
(`mod.rs:91-156`):

- `contains_local()` — true if any nested origin is `Local`, `Owned`, or `Unknown`
  (`mod.rs:94-106`). A non-ref `MatchBinding` is *not* local (it owns its data); a
  ref `MatchBinding` inherits from the scrutinee. This is the predicate a return
  expression is tested against for `DanglingReturn`.
- `references_def(target)` — true if the origin chain points at a specific DefId
  (`mod.rs:109-117`); used by `check_use` to detect use-after-source-moved. Note it
  checks only the binding itself for `MatchBinding`, not the scrutinee — sibling
  match bindings are independent.
- `contains_unknown()` — distinguishes a provably-local dangle from an
  unresolved-origin one, so the error can be `UnresolvedBorrowOrigin` vs
  `DanglingReturn` (`mod.rs:134-143`, used at `check_stmt.rs:1474`).

### How origins are computed

`compute_expr_origin` (`origins.rs:99-321`) is a recursive structural walk:

- **Identifiers** resolve to `Param` (if a parameter), the stored `var_origins`
  entry, or `Local` if the variable owns data (non-ref, non-callable type); ref-type
  locals with no recorded origin are views, not new sources, so they fall through to
  `Unknown` (`origins.rs:112-147`).
- **Field/tuple/index access** inherit the object's origin (`origins.rs:150-155`).
- **Calls / method calls** delegate to `compute_call_origin` /
  `compute_method_call_origin`, which look up the callee's `return_borrows_from` and
  extract the origins of exactly those argument positions; method calls offset by one
  because `self` is param 0 (`origins.rs:326-465`).
- **If / match** take the union (`merge_origins`) of branch origins
  (`origins.rs:90-96`, `166-185`). `merge_origins` collapses 0 origins to `Static`,
  1 to itself, and ≥2 to a `CallResult` wrapper.
- **Struct literals** union only the *reference-type* field arguments (via
  `struct_field_ref_flags`), excluding `Owned` f-string args since the struct takes
  ownership (`origins.rs:200-215`).
- **Closures** union the origins of captured ref-type free variables
  (`origins.rs:218-223`).
- **String concat (`+`)** on a string-typed operand produces `Owned`
  (`origins.rs:272-278`); **allocating string methods** (`to_upper`, `replace`,
  `join`, …) also produce `Owned` (`origins.rs:436-446`). `.clone()` on any value
  produces `Static` because the clone is a fresh independent allocation with no
  provenance back to the receiver (`origins.rs:459-461`). Plain string literals are
  `Static`; interpolated f-strings are `Owned` (`origins.rs:104-110`).

A subtle but load-bearing rule lives in `compute_call_origin`: a callee with a body,
no `return_borrows_from`, and a non-reference return type yields `Static` (fresh
data); a *bodyless* callee with ambiguous elision (multiple ref params, no explicit
annotation) stays `Unknown` to avoid wrongly clearing a real borrow
(`origins.rs:340-358`).

## Pass 5a — return-borrow inference

**Goal:** for each function, compute the set of parameter indices whose lifetime the
return value depends on, stored as `FunctionInfo.return_borrows_from` (and the
boolean `return_origin_is_static` for fresh-data returns). Driver:
`compute_function_return_borrows` (`return_borrows.rs:42`).

The function is skipped entirely unless its return type is a reference or callable
type (`return_borrows.rs:60-65`) — value-returning functions can't dangle. Then:

1. **Body analysis.** For block bodies, `build_local_alias_map`
   (`return_borrows.rs:418`) walks every statement first, mapping each local name to
   the set of param indices it may alias (over-approximating by unioning branches and
   tracing calls through their own `return_borrows_from`). Then
   `trace_block_returns_to_params` / `trace_expr_to_params`
   (`return_borrows.rs:583-749`) trace each `return` expression backward through field
   access, indexing, `if`/`match`, struct literals, calls, `default`, `!`/deref, and
   closures, accumulating contributing param indices.

2. **Elision fallback.** If body analysis (or a bodyless declaration/extern) yields
   nothing: a single ref-type param ⇒ that param; else a first param named `self` ⇒
   self; else no ref params ⇒ mark `return_origin_is_static` (fresh data)
   (`return_borrows.rs:98-164`). A function *with a body* that traces to nothing after
   elision is conclusively static (`return_borrows.rs:157-163`).

### The four closure-body visitors

Pass 5a and the main walk all need to inspect closure bodies, and each implements
the `crate::parser::visitor::ExprVisitor` trait so the default `walk_*` covers every
AST variant exhaustively (the old hand-written walkers silently missed statement
forms). All four skip *nested* closures, which own their own capture scope:

| Visitor | Collects | Consumer |
|---------|----------|----------|
| `CapturedRefOriginCollector` | origins of captured ref-type vars (`return_borrows.rs:171`) | `compute_expr_origin` for closures, via `collect_captured_ref_origins` (`helpers.rs:668`) |
| `CapturedMutationCollector` | names of mutated captured vars, incl. `&self`-method receivers (`return_borrows.rs:220`) | capture-mode classification |
| `CaptureSetCollector` | full capture set: name, DefId, Read/Mutable mode, borrowed-origin flag (`return_borrows.rs:294`) | spawn enforcement |
| `ClosureBodyParamTracer` | enclosing-function param indices referenced (`return_borrows.rs:378`) | Pass 5a alias tracing |

`compute_capture_set` (`helpers.rs:811`) runs `CapturedMutationCollector` then
`CaptureSetCollector` in two phases so the latter can mark each capture Read vs
Mutable from the mutated-names set.

## Pass 5b — the main walk

`check_function` (`check_stmt.rs:1391`) resets all per-function state
(`reset_per_function_state`, `check_stmt.rs:1367`, plus the extra clears at
`1394-1408`), then seeds parameter origins, fallible (Option/Result) states, and
`&`-param tracking before walking the body. `check_function → check_block →
check_stmt → check_expr` is the recursive descent.

### Ownership tracking

- `var_states: Map<DefId, VarState>` holds `Live` or `Moved { moved_at }`
  (`mod.rs:28-33`). `mark_live` (`origins.rs:14`) also records the def in the
  innermost `loop_local_defs` set so per-iteration moves are allowed.
- `check_use` (`origins.rs:25`) errors `UseAfterMove` on a moved variable, then
  checks reassignment-invalidation (`UseAfterSourceMoved`), origin-source moves
  (scanning `invalidated_origins` via `references_def`), and `await_invalidated`
  (`BorrowAcrossAwait`).
- `check_move` (`origins.rs:468`) marks the variable `Moved`, adds it to
  `invalidated_origins`, errors `DoubleMove` if already moved, and — crucially —
  releases any mutable-capture locks the variable held as a closure. A move is also a
  *use*, so it sets the variable's used-flag to suppress the unused-variable warning.

**Move-in-loop** is rejected (`MoveInLoop`) unless the moved variable was declared
inside the innermost loop body (`loop_local_defs`) or is rebound in the same
statement — the `x = f(!x, …)` left-fold pattern, tracked via
`assignment_rebind_target` (`origins.rs:495-503`, `mod.rs:257-262`).

### Single-owner enforcement — `MoveWithoutOperator`

CoW-by-default means a bare assignment `T b = a` *borrows* for almost every type and
needs no `!`. The exceptions are the single-owner-by-design types, enforced at
`check_stmt.rs:1184-1242`. After skipping params (borrowed from the caller), owned
strings (view-creating, `:1195`), and Copy types, the check fires
`MoveWithoutOperator` only for the shapes that are *not* CoW-eligible
(`check_stmt.rs:1217-1237`): function/callable types, `Owned[T]`, and the named
generics `Box`, `Task`, `TaskGroup`, `Guard`. Destructuring binds set
`in_destructuring_bind` to suppress the check (`mod.rs:407-410`, `check_stmt.rs:1181`).
This list is the safety-pass realization of the carve-outs documented in `CLAUDE.md`.

### Call-site ownership and aliasing

`check_call_ownership` (`helpers.rs:545`) compares each argument's sigil against the
callee's `param_ownerships`, emitting `OwnershipMismatch` when e.g. a function
expects `!` (consume) but the call passes a bare borrow. `check_call_aliasing`
(`helpers.rs:602`) detects intra-call conflicts on the same variable — two `&`
borrows, a `&` plus `!`, or a bare read plus `&` — and emits `BorrowConflict`.
`check_mut_ref_exclusive` (`helpers.rs:461`) and `check_borrow_field_mutation`
(`helpers.rs:508`) enforce `MutRef[T]` exclusivity, using the
`struct_field_mut_ref_flags` computed in Phase 4 (`type_utils.rs:231`).

### Mutation-while-borrowed

When a mutating builtin collection method is called (gated by
`is_mutating_builtin_method`, `check_expr.rs:303-305`), three independent borrow-
invalidation checks fire on the receiver's root DefId:

1. **Explicit reference borrows.** Any live `var_origin` that `references_def` the
   receiver, where the borrowing variable is a sigil `T &` (`ResolvedType::Ref`) or a
   struct transitively holding a `Ref[T]`/`MutRef[T]` field (`ref_type_structs`),
   emits `MutationWhileBorrowed` (`check_expr.rs:310-345`, error at `:337`).
2. **For-loop iterators.** `for_loop_iterables` (`mod.rs:263-265`) holds the DefIds of
   collections currently being iterated; mutating one inside the body emits
   `MutationWhileBorrowed` (`check_expr.rs:394`) — the for-loop shallow-copies the
   array struct, so reallocation from push/insert would dangle the iterator. This
   applies to all mutating methods regardless of element type.
3. **Implicit CoW borrows.** Variables bound from `vec.get(i)`/`vec[i]` on a
   resource-element collection are tracked in `index_borrow_sources`
   (`mod.rs:303-308`), recording the root binding plus the projected field path
   (`IndexBorrowSource`, `mod.rs:211-215`). A mutation whose path is a prefix of the
   borrow's source path emits the `CowBorrowMutation` **warning** (not an error) —
   `check_expr.rs:358-387`, warning pushed at `:378`. It is a warning because the CoW
   system already preserves correctness by materializing before mutation
   (`check_expr.rs:350-351`); the field-path prefix test means a mutation of a
   *disjoint sibling field* doesn't false-positive.

### Lifetime / dangling-return validation

On a `return` (or an expression-body function, `check_stmt.rs:1463-1496`), if the
return type is a reference/callable, the checker computes the expression's origin and
errors if `contains_local()`: `UnresolvedBorrowOrigin` when the origin is `Unknown`
(not a clean `Local`), else `DanglingReturn` naming the offending local and its
declaration span. `check_return_for_escaping_closures` (`helpers.rs:851`) additionally
rejects returning a struct/array/closure that captures locals.

### Branch save / restore / merge

Branching state is snapshotted and merged atomically through one `BranchState`
struct (`mod.rs:174-204`) that bundles *all* per-branch state — `var_states`,
`var_origins`, `invalidated_origins`, reassignment/await invalidation, mutable-capture
maps, shared-derived/stale-shared maps, fallible states, index-borrow sources, live
guards, and the `diverges` flag. `save_branch_state` / `restore_branch_state` clone it
in/out (`origins.rs:528-561`).

`merge_branch_states` (`origins.rs:568`) is the join point. It first filters out
*diverging* branches (return/break/continue/throw) — their state never reaches the
join, so a move in an early-return arm must not poison the merged state. If *all*
branches diverge, the merged state is marked `diverged`. Otherwise it merges the live
branches with the conservative rule appropriate to each axis:

- `var_states`: **moved in any branch ⇒ moved** after.
- `invalidated_origins`, `await_invalidated`: **union**.
- `var_origins`, reassignment-invalidated, shared-derived, stale-shared,
  index-borrow-sources, live-guards: **union** (keep-first / keep-any).
- `mut_captured_vars` / `mut_capture_owners`: **union** — a capture lock present in
  any branch is conservatively live after.
- `fallible_states`: **unchecked in any branch ⇒ unchecked** (so an `unwrap` is only
  safe if guarded on every path).

Whenever a new per-variable field is added to `BorrowChecker`, it must be threaded
through `BranchState` and all three of save/restore/merge — otherwise it leaks across
branches.

## Concurrency, `shared`, and spawn safety

### The `shared` keyword and Custody Flow Analysis (CFA)

A `shared` binding is always ARC-wrapped (heap, atomically reference-counted) and
*optionally* sync-wrapped. CFA decides which `SharedStrategy`
(`src/semantic/mod.rs:31-40`) each binding needs:

```
ArcOnly     Shared[T]   — no mutable borrows cross a concurrency boundary
ArcMutex    Mutex[T]    — mutable borrows cross spawn boundaries
ArcRwLock   RwLock[T]   — user override via shared(rwlock)
ArcAtomic   Atomic      — user override via shared(atomic), scalars only
```

`shared_var_defs` records each `shared` binding's DefId → (`SharedKind`, name, span)
(`mod.rs:360-361`). `cfa_at_spawn` (`helpers.rs:694`) runs at every spawn site: for
each argument that is a shared binding, it marks it `shared_spawned` and — for
`SharedKind::Auto` only (explicit overrides are respected) — chooses a strategy from
the callee's parameter ownership and type: a `&` (mutable-borrow) param or a `Mutex`
param ⇒ `ArcMutex`; an `AtomicInt`/`AtomicBool` param ⇒ `ArcAtomic`; an `RWLock`
param ⇒ `ArcRwLock`; otherwise `ArcOnly`. The upgrade rule promotes an `ArcOnly`
entry to `ArcMutex` if a later mutable use appears (`helpers.rs:731-735`).

The **final CFA pass** in `check_module` (`mod.rs:573-596`) assigns defaults to shared
bindings never decided at a spawn site: written-and-spawned ⇒ `ArcMutex` (main-thread
writes + spawned-thread reads = race without a mutex); never-spawned ⇒ `ArcMutex` if
written else `ArcOnly`. A shared binding that never crosses a concurrency boundary
gets an `UnnecessaryShared` warning. The resulting `shared_out` map is returned and
flows into IR lowering, which inserts the actual ARC/Mutex/Atomic wrapping — that
map plus the lowering *are* the live behaviour, and any status list elsewhere is a
historical record rather than a description of what runs.

### Token semantics — why multi-token code cannot deadlock

A **token** is the lock a shared binding carries. It is acquired on entering a
synchronous execution region that touches shared mutable bindings, released at
every suspension point (an `.await()`, or task/thread completion), and reacquired
when execution resumes. Between suspension points a region holds its tokens
continuously — which is what makes transparent access sound: reading or writing a
`shared` binding needs no explicit lock in user code.

With more than one shared binding in scope, the *order* is the load-bearing part.
`inject_shared_token_management` (`src/ir/transforms/shared_async.rs:106`) acquires
tokens in **ascending declaration order**, sorting by `decl_order` (`:177-179`,
whose comment states the reason: "Sort by decl_order for deadlock-free ordering").
`build_release_sequence` (`:380`) releases them in the **reverse** of that order,
and `build_reacquire_sequence` (`:400`) re-locks forward on resume.

A single global acquisition order with strictly-reverse release is the classic
deadlock-freedom argument: two regions can never hold tokens in opposite order, so
no cycle can form in the wait-for graph. The guarantee is **structural rather than
checked** — there is no deadlock detector at runtime, because the emission order
makes the cycle unconstructible in the first place. This is what the yield-point
analysis below means when it talks about a call "releasing the token".

### Stale-shared-condition detection

A local read from a shared binding before an `await` becomes *stale* after that await,
because another task may have mutated the shared value. `shared_derived` →
`stale_shared_derived` (`mod.rs:363-367`) track this; using a stale local in an
`if`/`while`/`match` condition emits a `StaleSharedCondition`-class warning via
`check_stale_condition` (`helpers.rs:16`). Bindings introduced by `with` over a shared
variable are exempt (`with_shared_tracked`, `mod.rs:379`) — the compiler guarantees
they are refreshed after every await (`check_stmt.rs:1014-1022`).

### `with` blocks and yield points

`with_depth` and `with_guarded_conditions` (`mod.rs:382-385`) track being inside a
`with` block and which enclosing branch/loop conditions reference a `with`-tracked
shared variable. A yield point (await, or a call that might release the
shared-variable token) inside such a region triggers a check-then-act / iterator-
invalidation warning. Whether a call is a yield point is decided by
`is_yield_point_call` / `expr_contains_yield_point` (`helpers.rs:213`, `:234`), which
are *purity-aware*: a call to a `Pure`/`ReadOnly` function cannot release the token.
The legacy `BLOCKING_CALL_NAMES` list (`mod.rs:223-226`) — `sleep`, `read_file`,
`http_get`, … — is retained for backward compatibility and must stay in sync with the
IR-lowering blocking-call lists noted in that comment. A `with`-block that performs an
arena (allocator) binding also bumps `arena_depth` so escape analysis can fire
`ArenaEscape` (`check_stmt.rs:1024-1038`).

### Spawn capture safety

`Expr::Spawn` / `Expr::SpawnBlocking` (`check_expr.rs:584`, `:671`) handle four spawn
shapes — direct function call, closure-variable call, inline-closure call, and method
call — and route through `check_spawn_args` (`helpers.rs:743`) plus, for closures,
`check_spawn_closure_captures` (`helpers.rs:791`). `spawn unchecked` is the opt-out
that skips all capture checks (`check_expr.rs:586`). `check_spawn_args` errors
`SpawnWithBorrowedRef` when a non-shared argument has a non-`Static` origin (a borrow
that might dangle once the task outlives the caller); the error names the variable so
the diagnostic can suggest a `shared` declaration. `check_spawn_closure_captures`
emits `SpawnClosureCaptureShared`, `SpawnClosureCaptureBorrowed`, and
`SpawnClosureCaptureMutable` per offending capture.

The opt-out exists because the check is necessarily conservative: a programmer
who has already synchronized access by hand — a hand-rolled mutex, an external
lock, a lock-free scheme, a single-reader/single-writer invariant, or a task
pinned to a thread — has discharged the obligation in a way the compiler cannot
see. `shared` is the right answer nearly always; `unchecked` exists so that
specialised code (async runtimes, lock-free structures, pinned workers, FFI
callbacks) need not contort its types to satisfy a check it has already met.

Three properties make that escape hatch reviewable rather than corrosive:

- **It is per-spawn, never per-function or per-module.** The opt-out attaches to
  one spawn site, so it stays local, greppable, and visible in review. There is
  deliberately no file-level or project-level switch.
- **It does not propagate.** A function called from an `unchecked` spawn does
  not inherit the exemption; if that function spawns internally without
  `shared`, the inner spawn needs its own `unchecked`. The suppression covers
  exactly the capture checks at the site that wrote it.
- **It shifts a proof obligation onto the author.** Reaching for `unchecked`
  means taking responsibility for a correctness property the compiler cannot
  verify, so the site should say which invariant discharges it.

### Mutable-capture aliasing

`mut_captured_vars` / `mut_capture_owners` (`mod.rs:351-359`) track variables mutably
captured by a *live* closure. While any entry exists, a direct read of the variable
errors `ReadWhileMutCaptured` (`check_expr.rs:61-72`); the locks are released when the
owning closure is moved or goes out of scope (`origins.rs:512-521`).

### Mutex double-lock

`live_guards` (`mod.rs:412-417`) maps a Mutex/RwLock DefId → its live `Guard`. A
second `.lock()` while the prior guard is still in scope emits `MutexDoubleLock`
(`check_stmt.rs:80-92`) — locks are non-reentrant, so this would deadlock. Guards are
dropped on block exit by diffing `live_guards` against the entry snapshot
(`check_stmt.rs:1118-1174`), and the map is merged conservatively across branches
(locked-in-any ⇒ locked, `origins.rs:672-677`).

## Purity inference

`infer_purity` (`validation.rs:379`) runs a two-pass fixed point producing a
`PurityByName` map over the four-level lattice in `src/semantic/purity.rs`:
`Pure < ReadOnly < MutatesArgs < HasSideEffects` (joined by
`Purity::join`).

1. **Local pass** (`infer_function_purity`, `validation.rs:459`): extern/declaration
   bodies ⇒ `HasSideEffects`; a `&`/`!` param ⇒ `MutatesArgs`; then the body walk
   accumulates global reads (`ReadOnly`), and side effects (`HasSideEffects`) for
   shared access, await, spawn, etc.
2. **Propagation** (`validation.rs:392-413`): iterate the call graph to a fixed point
   (capped at 100 iterations), `caller = caller JOIN max(callee)`; an unknown callee
   defaults to `HasSideEffects`.

The result feeds the yield-point detection above and is also returned from
`check_module` for downstream use.

## Validation extras

Two non-ownership checks ride along in `validation.rs`, invoked at the tail of
`check_module`:

- **Unused imports** (`mod.rs:599-614`): collect every used DefId from the resolution
  map plus type references, then warn `UnusedImport` for any imported def that is
  unused and not underscore-prefixed.
- **Private-in-public** (`check_private_in_public`, invoked at `mod.rs:617`): a
  public function/struct signature that mentions a `Private` user type is flagged.
  `BUILTIN_TYPE_NAMES` (`validation.rs:16-23`) is the always-public allowlist.

## Error and warning categories

The checker produces dozens of error/warning kinds (defined in
`src/semantic/errors.rs`). The major families:

| Family | Representative kinds |
|--------|----------------------|
| Ownership | `UseAfterMove`, `DoubleMove`, `MoveInLoop`, `MoveWithoutOperator`, `OwnershipMismatch`, `BorrowConflict` |
| Lifetimes | `DanglingReturn`, `UnresolvedBorrowOrigin`, `UseAfterSourceMoved`, `MutationWhileBorrowed` |
| Concurrency | `BorrowAcrossAwait`, `SpawnWithBorrowedRef`, `SpawnClosureCaptureMutable`/`Borrowed`/`Shared`, `ReadWhileMutCaptured`, `MutexDoubleLock` |
| Arena | `ArenaEscape` (non-Copy value escaping a `with`-arena scope) |
| Warnings | `UnnecessaryShared`, `StaleSharedCondition`, `CowBorrowMutation`, `UncheckedUnwrap`, `CouldBeConst` (opt-in via `--warn-const`), `UnusedVariable`, `UnusedImport` |

## Adding a new check

1. Add the kind to `src/semantic/errors.rs` (and its `Display` arm).
2. Implement it in the layer that owns the AST node: expression-level →
   `check_expr.rs`; statement-level → `check_stmt.rs`; call-site/concurrency →
   `helpers.rs`; origin/lifetime → `origins.rs`.
3. If the check needs new per-variable state, add a `pub(super)` field to
   `BorrowChecker` in `mod.rs`, clear it in `reset_per_function_state`
   (`check_stmt.rs:1367`), **and** thread it through `BranchState` +
   save/restore/merge in `origins.rs`.
4. Add a unit test in `tests.rs`.

## In the self-host

**The Gorget self-host has no Pass 5.** The self-host check pipeline is
parse → load → expand derives → expand meta → resolve → typecheck → format type
output (`tests/fixtures/self_host_check/driver.gg`). It stops at the type checker;
there is no port of the borrow checker, origin tracking, or CFA. The `check_comparison`
integration test (`tests/integration.rs:13193`) compares the *type* output of the full
Rust check path (`ModuleLoader` → `merge_modules` → `analyze`) against the self-host
driver — it exercises the loader and typecheck, **not** ownership/lifetime/concurrency
diagnostics. To gauge that parity, run:

```bash
cargo test --test integration check_comparison -- --nocapture
```

and read the printed matched-count (these `*_comparison` tests are
diagnostic-always-pass — a green test asserts nothing about parity; only the counts
do). The self-host typechecker source it shares lives under
`tests/fixtures/self_host_typechecker/` (the `self_host_check` dir is mostly symlinks
into it plus its own `loader.gg`/`driver.gg`). **Porting the safety checker to
self-host is open work** (the self-host is not feature-complete here); it is the
largest unported semantic pass.
