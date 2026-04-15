# Plan: Move Semantics for Last-Use Locals

## Problem

When a struct or collection containing resource-type fields (String, Vector, Dict)
is passed to an ownership boundary (function return, struct init, closure capture),
the compiler deep-clones it even when the source variable is never used again. A
deep clone of a struct with N Vector fields copies every element buffer — O(total
data size). A move is O(1): copy the struct, zero the source.

This is the primary cause of gorget-arena's memory doubling from ~798MB to ~1581MB.
The pattern:

```gorget
BspMap map = load_bsp(data)    # map owns 14 Vector fields
return map                      # map is last-use — but ensure_owned_at_boundary clones it
```

The clone duplicates every vector's data buffer. The original is then dropped at
scope exit, freeing the original buffers — but the cloned copies persist. Net
effect: every piece of data exists in memory twice during the transition.

## Current State

The compiler already checks `is_last_use_at` in 3 spots:

1. **Named-local assignment** (`src/ir/lowering/stmts/assigns.rs:210`)
   `y = x` where x is last-use → Move assignment, no clone.

2. **Collection consuming args** (`src/ir/lowering/context.rs:1414`)
   `v.push(x)` where x is last-use → no clone, MoveZero after call.

3. **Throws return of Ptr(T)** (`src/ir/lowering/stmts/mod.rs:847`)
   `return s` where s is borrowed last-use param → deref+zero, no clone.

The gap is `ensure_owned_at_boundary` (`src/ir/lowering/context.rs:1283`), which
is called at 7 sites and never checks last-use. It clones any operand flagged as
a borrow (bare param, ref-state, CoW) regardless of whether the source is dead
after this point.

## Target: `ensure_owned_at_boundary`

### Current logic (context.rs:1283-1351)

```
ensure_owned_at_boundary(builder, operand, span, reason):
  local = operand.local
  local_type = builder.local_type(local)

  // Case 1: Ptr(T) → always clone inner
  if pointee_type(local_type) is Some(inner):
    clone via clone_fn_for_ptr(inner)
    return cloned

  // Case 2: by-value resource, not a borrow → pass through
  if not is_resource_type(local_type): return operand
  if not is_borrow(local): return operand

  // Case 3: by-value resource, IS a borrow → clone
  clone via clone_fn_for_ptr(local_type)
  return cloned
```

### Call sites (7 total)

| # | File | Line | Context | Last-use possible? |
|---|------|------|---------|--------------------|
| 1 | `functions.rs` | 555 | Expression-body return | Yes |
| 2 | `functions.rs` | 810 | Expression-body return (async) | Yes |
| 3 | `functions.rs` | 1065 | Block-body expression return | Yes |
| 4 | `functions.rs` | 1290 | Block-body expression return (variant) | Yes |
| 5 | `exprs/mod.rs` | 1384 | Struct init field args | Yes |
| 6 | `closures.rs` | 254 | Closure capture clone | Maybe (depends on capture semantics) |
| 7 | `closures.rs` | 382 | Closure return | Yes |

### Proposed change

Add last-use elision to `ensure_owned_at_boundary`. Before cloning a by-value
resource that is flagged as a borrow, check if this is the variable's last use.
If so, the variable is about to go dead — move instead of clone.

**Design decision on Ptr(T) bare params:** There are two approaches to eliminating
the clone for Ptr(T) parameters at their last use:

- **Option A (callee-side):** The callee detects last-use, loads through the
  pointer, and memsets the caller's slot to zero through the Ptr. This works but
  introduces a cross-function memset that can't be eliminated.

- **Option B (caller-side, chosen):** The callee remains unchanged — it receives
  Ptr(T) and reads through it normally. The CALLER recognizes that the argument
  is a last-use resource and marks it as moved after the call returns. The caller's
  scope-exit drop is suppressed. No memset, no clone, no ABI change.

Option B is chosen because it is consistent with the ownership contract in
CLAUDE.md ("Ownership at Consuming Positions"): bare params are always borrowed
(`Ptr(T)`), and the caller is responsible for lifecycle. The callee's ABI is
unchanged — it still receives `Ptr(T)` and is NOT allowed to assume ownership.
The optimization is purely caller-local: the caller passes the borrow, the call
executes, and then the caller suppresses its own drop because it knows the value
was consumed.

This matches the existing "named local, last use → move_zero after call" row in
the consuming-positions table. The difference is that the current implementation
only applies this at explicit consuming positions (push/put/set). This plan
extends it to ALL call sites where the argument is a last-use resource-type local.

Auto-promoting bare params to move params (changing the callee's ABI) could be
revisited later as an advanced optimization but is out of scope for this plan.

```
ensure_owned_at_boundary(builder, operand, span, reason):
  local = operand.local
  local_type = builder.local_type(local)

  // Case 1: Ptr(T) → check last-use, then clone or pass through
  if pointee_type(local_type) is Some(inner):
+   if is_named_local(local) AND is_last_use_at(name, span):
+     // Last-use Ptr(T): the callee will read through the pointer.
+     // The CALLER will mark_moved after the call (see Step 1b).
+     // Here in ensure_owned_at_boundary we just need to load the
+     // owned value so it can be used directly (e.g., as a struct
+     // field or return value).
+     load value through pointer (deref)
+     drops.register(loaded_value, inner)    ← new temp owns the data
+     return owned value                     ← no clone
    if clone_fn_for_ptr(inner):
      clone and return                       ← existing path (non-last-use)

  // Case 2: by-value resource, not a borrow → pass through
  if not is_resource_type(local_type): return operand
  if not is_borrow(local): return operand

+ // Case 2b: by-value resource, borrow, but last-use → move
+ if is_named_local(local)
+   AND drops.is_registered(local)
+   AND is_last_use_at(name, span):
+     drops.unregister(local)                ← prevent scope-exit drop
+     clear borrow flags (ref-state, bare-param, cow)
+     return operand                         ← existing value, no clone

  // Case 3: by-value resource, IS a borrow, not last-use → clone
  clone via clone_fn_for_ptr(local_type)
  return cloned
```

## Implementation Steps

### Step 1a: Modify `ensure_owned_at_boundary` (context.rs)

In `src/ir/lowering/context.rs:1283`, add last-use checks before both clone paths.

**Case 1 (Ptr(T))**: When the operand is `Ptr(T)` and the local is a named param
at its last use, load the owned value through the pointer (deref copy) and return
it. Do NOT emit `MoveZero` with `Projection::Deref` — the caller handles the
lifecycle (see Step 1b). Guard with `clone_fn_for_ptr(inner).is_some()` to skip
primitive Ptr types (Ptr(double), Ptr(int)) that don't need cloning.

The load-through-pointer produces an owned T value on the stack. Register it with
drops so it gets cleaned up if the function exits early. The original Ptr local
remains valid (pointing at the caller's slot) but its value will not be freed by
the caller because the caller marks it as moved (Step 1b).

**Case 2b (by-value resource borrow)**: When the operand is a by-value resource
(GorgetString, GorgetArray, user struct) flagged as a borrow, and it's the local's
last use AND the local is drop-registered: unregister from drops and return the
operand as-is (it's already an owned value on the stack — the borrow flag just
means it might alias something). The caller's scope-exit drop is suppressed, and
the value transfers to the new owner.

**Safety constraint for Case 2b**: Only apply when the borrow flag comes from
`is_bare_param` (the common case — function parameters are Ptr(T) but lowered to
by-value copies). For `is_ref_local` or `is_cow_borrow`, the local might genuinely
alias another live variable, so the clone is still necessary. Add a helper:

```rust
fn is_borrow_only_from_param(&self, local: LocalId) -> bool {
    self.is_bare_param(local)
        && !self.is_ref_local(local)
        && !self.is_cow_borrow(local)
}
```

### Step 1b: Caller-side mark_moved for last-use arguments (Phase 2)

This step eliminates the memset that would otherwise be needed in Case 1. It can
be implemented in a follow-up commit after Step 1a is validated.

At each call site where an argument is a last-use resource-type bare param:

1. The caller computes `is_last_use_at(arg_name, call_span)` for each argument.
2. If last-use AND the argument is a resource type (or contains resource fields):
   emit `mark_moved(arg_local)` after the call instruction.
3. The caller's scope-exit drop for that local becomes a no-op.

This is the same pattern as the "named local, last use → move_zero after call"
row in the CLAUDE.md consuming-positions table, extended from collection consuming
methods to all function calls.

The callee is unchanged — it receives `Ptr(T)`, reads through it, and returns.
It does not zero the caller's slot. The caller simply doesn't drop it.

**Important**: This is only safe because the callee receives a borrow (Ptr).
The callee does not take ownership — it just reads. If the callee stored the Ptr
somewhere that outlives the call (e.g., in a global), the caller's mark_moved
would cause a use-after-free. But bare-param Ptr(T) values are stack-local by
construction — the callee can only use them during the call, not retain them.

### Step 2: Thread span through call sites

All 7 call sites already pass a `span` argument. No signature change needed —
the span is available inside `ensure_owned_at_boundary` as the `span` parameter.
The `is_last_use_at` check uses `span` directly.

However, we need the local's **name** for `is_last_use_at`. Currently the function
only has the `Operand` (which gives us the `LocalId`). Use `builder.local_name(local)`
to recover the name — same pattern used in `stmts/mod.rs:842`.

### Step 3: Handle the move-zero lifecycle

When we skip a clone in Case 2b, the caller must NOT double-free:
- `drops.unregister(local)` prevents the scope-exit drop.
- The value now belongs to whoever receives the returned operand (struct field,
  return slot, closure capture).
- If the caller later tries to `move_zero_and_mark` this local, it's a no-op
  (already unregistered).

For Case 1 (Ptr(T) last-use), the `MoveZero` with `Deref` projection zeroes the
caller's stack slot through the pointer. The loaded value is a fresh local that
the caller owns. The original Ptr local goes dead.

### Step 4: Add `is_last_use_at` to the non-throws return path

The non-throws return path in `stmts/mod.rs:883-945` has its own clone logic
(separate from `ensure_owned_at_boundary`). It should also check `is_last_use_at`
for the `Ptr(T) → T` auto-clone at line 922-943. This is the same pattern as the
throws path already optimized — just apply the same guard:

```rust
// stmts/mod.rs, around line 929 (inside the Ptr(T) → T return clone):
if let Some(clone_fn) = ctx.clone_fn_for_ptr(inner) {
    let param_name = builder.local_name(p.local).map(|s| s.to_string());
    let is_last = param_name.as_ref()
        .map_or(false, |n| ctx.is_last_use_at(n, expr.span));
    if is_last {
        // load through pointer + move_zero (same as throws path)
    } else {
        // clone (existing path)
    }
}
```

### Step 5: Struct init field args

The struct init path (`exprs/mod.rs:1384`) calls `ensure_owned_at_boundary` in a
loop over field operands. After Step 1, this automatically benefits — last-use
field args skip the clone.

One subtlety: if multiple fields of a struct init reference the same variable
(e.g., `Pair(x, x)`), only the LAST use in source order should be moved. The
liveness analysis already handles this — only the last use's span is in
`last_use_spans`. The first use will not match and will clone as before.

### Step 6: Test

**Existing fixtures**: Run full integration suite (`cargo test --test integration
-- --test-threads=4`). Current count: 966 tests.

**New fixture**: `tests/fixtures/move_last_use_struct.gg` — a function that
builds a struct with Vector fields, returns it, and verifies the data is intact.
The `--emit-lir` output should show NO clone calls for the last-use struct.

```gorget
struct Data:
    Vector[int] items
    String name

Data make_data():
    Vector[int] v = Vector[int]()
    v.push(1)
    v.push(2)
    v.push(3)
    String s = "hello"
    return Data(v, s)    # v and s are last-use — should move, not clone

void main():
    Data d = make_data()
    print(f"{d.items.len()}")
    print(d.name)
    print("done")
```

Expected output: `3\nhello\ndone`
Verification: `cargo run -- build --emit-lir tests/fixtures/move_last_use_struct.gg 2>/dev/null | grep clone` should show NO clone calls inside `make_data`.

**gorget-arena verification**: Build gorget-arena and count clone calls before/after:
```bash
grep -c "gorget_string_clone_to_owned\|gorget_array_clone\|gorget_map_clone" target/gorget-arena/src/main.c
```
Current: ~295 inline clone calls. Target: ~200 or fewer (30%+ reduction).

## Risks

1. **Aliased locals**: If a local is flagged as `is_bare_param` but also has an
   alias (e.g., via `&` borrow to another variable), moving it would invalidate
   the alias. The `is_borrow_only_from_param` guard mitigates this — bare params
   that are also ref-locals or CoW-borrows are excluded.

2. **Closures**: Closure captures that are moved (Case 2b) must not be accessed
   after the closure is created. Liveness analysis should handle this since the
   capture point is the last use, but verify with a test fixture that captures a
   Vector and then tries to use it (should get a compile error, not a runtime bug).

3. **Multi-field struct init ordering**: If struct init evaluates field expressions
   left-to-right and a later field's expression has side effects that read an
   earlier field's source, the move could cause a use-after-zero. In practice,
   Gorget evaluates field args left-to-right and liveness analysis accounts for
   this — only the chronologically last use is marked as last-use.

4. **Primitive Ptr(T)**: Same guard as the throws-return fix — only apply the
   Ptr(T) move optimization when `clone_fn_for_ptr(inner).is_some()`. Primitive
   types (Ptr(double), Ptr(int)) don't have clone functions and must pass through
   untouched.

## Reviewer Notes

**Shadowing false-positive concern (raised by reviewer)**: Not an issue.
`is_last_use_at` checks `liveness.last_use_spans.contains(&span.start)`, keyed
by source position. The `_name` parameter is unused in the implementation
(`context.rs:793`). Two variables with the same name but different source positions
get different spans.

**MoveZero cost (raised by reviewer)**: Original plan had Case 1 (Ptr(T)) emit
`Memset(ptr, 0, sizeof(Struct))` through the pointer. Revised design uses
caller-side `mark_moved` instead (Option B) — no memset at all. The callee
reads through the Ptr normally; the caller suppresses its scope-exit drop.
This is consistent with the CLAUDE.md consuming-positions contract where bare
params remain borrowed and the caller handles lifecycle. Case 2b also emits no
MoveZero — just unregisters from drops. The memset-free path can be implemented
in Phase 2 (Step 1b) as a follow-up after the clone elimination is validated.

**Most impactful path is Case 1 (Ptr(T)), not Case 2b.** Analysis of the
gorget-arena generated C shows the key clone site is `world_state_new`, which
takes `BspMap` as a bare param (Ptr(BspMap)). It memcpy's the struct, then
deep-clones it via `BspMap__clone` — copying all 14 Vector data buffers.
Locally-constructed structs are owned (not flagged as borrow) and already pass
through `ensure_owned_at_boundary` without cloning (line 1335). The optimization
primarily benefits Ptr(T) bare params at their last use inside a function.

## Implementation Status

### Phase 1 (shipped)

- **Case 2b (by-value resource borrow, last-use → move)**: Implemented in
  `ensure_owned_at_boundary` (context.rs). When a by-value resource local is
  flagged as `is_bare_param` (but not ref-local or CoW-borrow), is drop-registered,
  and is at its last use: unregister from drops and return as-is. No clone, no
  MoveZero.

- **Throws-return Ptr(T) callee-side move: REVERTED.** The earlier commit's
  MoveZero-through-Deref pattern was unsafe — the callee zeroed the caller's stack
  slot without knowing if the caller still needed the value. Reverted to always-clone
  for Ptr(T) in both throws and non-throws return paths.

- **Safety tests**: `move_last_use_safety.gg` verifies that bare params passed to
  functions are NOT corrupted after the call returns (caller can still use them).

- **968 integration tests pass, gorget-arena builds.**

### Phase 2 (deferred — requires caller-side cooperation)

- **Case 1 (Ptr(T) → move at return/struct-init boundaries)**: Requires the CALLER
  to mark arguments as moved after the call when they are last-use. The callee
  cannot safely zero the caller's slot. See Step 1b in the plan.

- **Non-throws return Ptr(T) → T optimization**: Same as above — requires
  caller-side mark_moved. Currently always clones.

- **Throws-return Ptr(T) → T optimization**: Same — currently always clones.

## Expected Impact

- **gorget-arena memory**: ~30-50% reduction in live heap from eliminating
  redundant deep clones at return and struct-init boundaries.
- **General performance**: Every last-use struct/collection transfer becomes O(1)
  instead of O(data size). Most impactful for functions that build and return
  large data structures.
- **Clone count**: ~30%+ reduction in inline clone call sites in generated C.
- **No behavior change**: The optimization is purely mechanical — the source
  variable was going to be dropped anyway. Moving it just does the drop earlier
  (zero instead of free, since the new owner now holds the data).
