# Scout: `return` as the tail of an expression-body function is mis-lowered

**Status:** PRE-EXISTING Rust-gg (reference compiler) correctness bug. Reproduced
on the clean gorget-1 base (tip `57b44418`, no error-model needed). Fix (A)
prototyped + measured in this scout's worktree. C backend.

## TL;DR

`int withret(int a, int b): return a * b` silently returns `0` instead of `12`.
The expression-body lowering unconditionally writes a *second* assign to the
return slot after the inner `return` already terminated the block — and that
second `emit` is **not** guarded against terminated blocks, so it clobbers the
real value with `unit`. The fix is **(A)**: skip the trailing
`assign_to_return_slot`/drops/`ret` when the tail already terminated the block
(`builder.is_terminated()`). This is the **reference-grade** answer because the
form is **documented as valid** in the user-facing book — rejecting it (option B)
would contradict the spec.

---

## 1. Measured repro (RUN on clean base, C backend)

| Program | Output (broken base) | Output (after fix A) | Correct? |
|---|---|---|---|
| `int withret(int a, int b): return a * b` → `withret(3,4)` | **`0`** | **`12`** | ✅ |
| `int f(int a, int b): a * b` (idiomatic) → `f(3,4)` | `12` | `12` | (unchanged) |
| `int scaled(self, int x): return self.base * x` (method, base=10) → `c.scaled(5)` | **`0`** | **`50`** | ✅ |
| `int idiom(self, int x): self.base * x` (method) → `c.idiom(5)` | `50` | `50` | (unchanged) |
| `T pick[T](T a, T b): return a` (generic fn) → `pick(7,9)` | **`0`** | **`7`** | ✅ |
| `String greet(String name): return "hi " + name` → `greet("bob")` | (returned garbage/wrong) | **`hi bob`** | ✅ |
| `int risky(int x) throws String: return x + 1` → `risky(41) catch (e): 0` | (wrong) | **`42`** | ✅ |
| `(int x): return x * 3` (**closure**) → `f(5)` | `15` | `15` | **already correct** |
| `int always_fail(int x) throws String: throw "nope"` → catch `99` | `99` | `99` | already correct* |

\* The `: throw` tail was already correct *by luck* — the throw terminator
branches to the catch/panic block, so the dead `_0 = const unit` it clobbers is
never read on that path. Fix (A) makes it principled (the dead store is no longer
emitted). Confirmed by stash-testing throw-tail on the pre-fix base: prints `99`.

The closure path was **already correct** and is **untouched** — see §2.4.

### GIR proof (the smoking gun)

`gg build --emit-gir /tmp/retbug.gg` for `int withret(int a, int b): return a*b`:

**Broken base:**
```
bb0:
    _3 = mul i64 copy _1, copy _2
    _0 = const unit            ; ← OUTER assign_to_return_slot CLOBBERS the slot
    return copy _0             ; → returns 0
```
(The inner return's `_0 = copy _3` is DCE'd as a dead store, killed by the
following `_0 = const unit`.)

**After fix A:**
```
bb0:
    _3 = mul i64 copy _1, copy _2
    _0 = copy _3               ; ← inner return's assign, no longer clobbered
    return copy _0             ; → returns 12
```

---

## 2. Root cause (verified against current source)

### 2.1 The AST shape

`gg parse` shows `int withret(...): return a*b` parses to
`FunctionBody::Expression( Spanned{ node: Block(Block{ stmts: [Stmt::Return(Some(a*b))] }) } )`.
So an inline `: <stmt>` body is wrapped as an `Expr::Block` containing the single
statement. (The parser accepts any inline statement here — `return`, `if`, etc.)

### 2.2 The lowering chain (where the value is dropped)

1. `FunctionBody::Expression(expr)` arm calls `lower_expr(ctx, builder, expr)`
   on the `Block` expr → `lower_block_expr` (`src/ir/lowering/exprs/mod.rs:3725`).
2. The block's single (= last) stmt goes to `lower_stmt_as_tail_value`
   (`exprs/mod.rs:3758`). `Stmt::Return` is **not** one of the three recognized
   tail-value shapes (`Expr` / `If` / `Match`), so it hits the `_ =>` arm
   (`exprs/mod.rs:3771`): lowers the return as a regular stmt (which assigns
   `_0 = a*b` and **terminates** the block with `ret copy _0`, via
   `lower_return` at `src/ir/lowering/stmts/mod.rs:1981` + `:2106`) and returns
   `None`.
3. `lower_block_expr` does `.unwrap_or(Operand::Constant(Constant::Unit))`
   (`exprs/mod.rs:3740`) → the expr-body arm's `operand` is **Unit**.
4. Back in the arm, `assign_to_return_slot(ctx, builder, Unit)`
   (`functions.rs:19`) → `builder.assign_mode(...)` → `emit(...)`. **`emit`
   (`src/ir/builder.rs:143`) does NOT check `block.terminator`** — so it pushes
   `_0 = const unit` into the already-terminated block, clobbering the real
   value.
5. The trailing `builder.ret(copy(_0))` *is* a no-op (`set_terminator`,
   `builder.rs:149`, returns early when the block is already terminated — added
   for Snag #33/#39), but the slot was already destroyed in step 4.

### 2.3 Why it's a write-site bug (layering discipline §24)

This is textbook "fix at the write site, not the read site": `assign_to_return_slot`
*writes* the slot, and the writer never checked the typed invariant that the block
was already terminated by a divergent tail. `set_terminator` was already hardened
against this exact class (Snag #33/#39: "no-op when already terminated") but the
sibling write path through `emit` (plain `assign`) was not. **One sibling of the
class was fixed; the `emit`/`assign` sibling was missed.** (Sibling-site drift.)

### 2.4 Why closures already work (the model for the fix)

The closure-body lowerer (`src/ir/lowering/closures.rs:498-529`) dispatches the
tail through the *same* `lower_stmt_as_tail_value`, but only calls
`emit_implicit_return` **when it returned `Some`** (`tail_handled = true`). When
it returns `None` (the `Stmt::Return` case), `tail_handled` stays `false`, and
the fallthrough `ret` at `:522` is guarded by
`if builder.blocks[last_block_idx].terminator.is_none()`. So the closure path
**never clobbers** — it defers to the inner return's terminator. The four
`functions.rs` arms lack this guard. **The fix makes them mirror the closure
path.**

---

## 3. Affected fix sites (all in `src/ir/lowering/functions.rs`)

The `FunctionBody::Expression` arm appears 6 times; the bug class spans **4** of
them (all the real lowering arms). The other 2 are NOT lowering write sites:

| Line (base) | Function | Role | Affected? |
|---|---|---|---|
| `1074` | `lower_function` | top-level non-generic fn | **YES** |
| `1370` | `lower_equip_method` | non-generic method | **YES** |
| `1494` | `lower_generic_function` (returned-param analysis) | reads tail to detect a returned param name; no lowering | no |
| `1659` | `lower_generic_function` | monomorphized generic top-level fn | **YES** |
| `2052` | `lower_method_instance` | monomorphized generic method | **YES** |
| `1803` | (a `match` that just `{}`-skips both body kinds) | no lowering | no |

The closure path (`closures.rs`) is **already correct** and must NOT be touched.

---

## 4. Recommended fix: (A) WRITE-SITE guard. NOT (B) reject.

### Doc-grounded rationale (this is the load-bearing decision)

- `docs/language-reference.md:613-619`: "Expression body shorthand … Equivalent
  to a block body with `return`." So `: expr` is *defined* as `return expr`.
- **`docs/book/04-functions.md:219`** (user-facing book, Summary table):
  > | Function | `RetType name(params): body` | `int add(int a, int b): return a + b` |

  The book documents `int add(...): return a + b` as a **canonical valid
  inline-body example** — a separate row from "Expression body" (`: x * 2`). The
  language *intends* `: <body>` (where body may be a statement like `return`) to
  work, distinct from `: <expr>`.
- `docs/language-design.md:632`: `int get_threshold(): return GLOBAL_MAX` — again
  `: return …` shown as valid.

**Conclusion: the language INTENDS `: return expr` to work.** This is exactly the
"ground the design in the docs, not just the code" / "don't redesign around
compiler gaps" principle — the docs show what's INTENDED, and they document this
form. **Option (B) (reject `: return` at parse/typecheck) would contradict the
published book and is wrong.** Fix (A) makes the documented form behave as
documented.

(Note: the form generalizes — `: throw …`, `: if …: return … else: …`, inline
multi-stmt bodies — all flow through the same `Expr::Block` tail path and the
same guard handles them. Verified: `int absval(int x): if x<0: return -x else:
return x` → correct after fix; `: throw` → correct.)

### The fix (prototyped + measured in this worktree)

In each of the 4 affected arms, wrap the post-`lower_expr` trailing work
(`ensure_owned_at_boundary` / `auto_deref_at_return` / `wrap_expr_tail_in_ok` /
`assign_to_return_slot` / `emit_early_exit_drops` / final `ret`) in
`if !builder.is_terminated() { … }`, and **always** call `pop_scope_no_emit()`
(the drop scope was pushed before the body; the inner `lower_return` already
emitted the early-exit drops via `emit_early_exit_drops` at `stmts/mod.rs:2105`
but does NOT pop the scope — the arm owns the pop). For `lower_function` (1074)
the `fault_return_bb` fill (Inc-2.1a) must still run on both branches.

Semantic delta per arm = **one guard**. The diff *looks* large only because
wrapping re-indents the existing body; the executor may prefer to extract the
trailing block into a small helper (`finalize_expr_body_return`) to cut churn and
DRY the four near-identical bodies — optional, the guard alone is correct.

**Measured result of the prototype:** all 9 repro rows in §1 correct; `gg build
--emit-gir` clean (`_0 = copy _N`, no `_0 = const unit`); idiomatic `: expr`,
method `: expr`, `void greet(): print(s)`, inline-`if`-with-`return` all
unchanged/correct.

#### Prototype diff (semantic core; full diff has re-indentation)

```rust
// src/ir/lowering/functions.rs — lower_function arm (1074), repeated (adapted)
// in lower_equip_method (1370), lower_generic_function (1659),
// lower_method_instance (2052):

let mut operand = lower_expr(ctx, &mut builder, expr);
ctx.func_state.expected_type = prev_expected;        // (lower_function/equip only)
+// A `return`/`throw` expr-body tail already terminated the block and wrote the
+// slot. The outer assign_to_return_slot (unguarded `emit`) would clobber it
+// with Unit. Skip the trailing assign/drops/ret; just balance the drop scope.
+// Mirrors the closure-body terminator guard (closures.rs:520).
+if !builder.is_terminated() {
     // ... existing ensure_owned / auto_deref / wrap_expr_tail_in_ok /
     // assign_to_return_slot / (fault fill) / emit_early_exit_drops ...
+}
 ctx.drops.pop_scope_no_emit();          // always (was inside the moved block)
 builder.ret(FunctionBuilder::copy(LocalId(0)));   // no-op if already terminated
```

(For `lower_function` the fault-block fill is duplicated into the `else` branch
so it runs whether or not the tail terminated; see the worktree diff for the
exact shape.)

---

## 5. Validation measured in this scout

- `cargo build` — clean, **no warnings**.
- `cargo test --lib` — **1084 passed, 0 failed**.
- Targeted integration: `expression_body_functions`, `generic_functions`,
  `catch_basic`, `throws_call_*`, `spawn_method_basic`, `test_generic_functions`,
  `throws_call_in_tail_return` — **10/10 pass**.
- Broad integration (`closure*`, `equip*`, `drop_*`, `fault_catch*`,
  **`self_host_runtime`** — the lock-in net): **88 passed, 0 failed** (149s; the
  self-host runtime-snapshot net passing confirms self-host emit is unaffected).
- The parent still owns the full `cargo test --test integration` sweep +
  `GG_BACKEND=llvm` parity + `self_host_bootstrap_fixed_point` before integrate.

---

## 6. Fixture to add: `tests/fixtures/return_expr_body.gg`

Covers all 4 affected arms (top-level fn, method, generic fn, throws) plus a
resource (String) return:

```gorget
# Regression: `return` as the TAIL of an expression-body function.
# `int f(...): return EXPR` is documented (docs/book/04-functions.md:219) and
# must behave identically to `int f(...): EXPR` / a block body `return EXPR`.
# Was silently mis-lowered to drop the value and return 0/unit (the outer
# assign_to_return_slot clobbered the inner return's slot write).

int withret(int a, int b): return a * b

struct Calc:
    int base

equip Calc:
    int scaled(self, int x): return self.base * x

T pick[T](T a, T b): return a

String greet(String name): return "hi " + name

int risky(int x) throws String: return x + 1

void main():
    print(withret(3, 4))
    Calc c = Calc(10)
    print(c.scaled(5))
    print(pick(7, 9))
    print(greet("bob"))
    int v = risky(41) catch (e): 0
    print(v)
```

**Expected stdout (MEASURED):**
```
12
50
7
hi bob
42
```

Also worth extending the existing `tests/fixtures/expression_body_functions.gg`
with a `: return` case (and its `runtime_snapshots/*.out`) so the canonical
expr-body fixture exercises the form. The Inc-2.1a fault fixtures all use the
idiomatic `: a * b` form (verified — no `): return` in any fixture), so they are
unaffected.

**Negative fixture (B): NOT warranted** — the form is documented-valid; rejecting
it is the wrong reference-grade behavior.

---

## 7. Fixture-impact / blast radius (premise #4 + executor notes)

- **No existing fixture uses the `): return` expr-body form.** `grep -rnE
  "\):[[:space:]]+return\b" tests/fixtures/*.gg` finds only match-arm bodies
  (`case Error(e): return …`) and comments — zero function expr-bodies. So the
  bug was entirely invisible (no test exercised it) and **fix (A) changes zero
  existing fixture behavior.** Confirmed empirically: `self_host_runtime` net
  (88 tests) green.
- **Idiomatic `: expr` is untouched** — the guard only diverts when the tail
  *terminated* the block; a normal `: a * b` tail does not terminate, so the
  existing path runs verbatim. (The `if`-as-value / `match`-as-value tails go
  through `lower_stmt_as_tail_value`'s recognized arms and return `Some` →
  non-terminated → existing path.)
- **Closures untouched** — already correct; do not edit `closures.rs`.
- **`self_host` impact: NONE (verified).** Searched all self-host dirs for a
  function expr-body `: return` — `grep -rnE "\):[[:space:]]+return\b"
  tests/fixtures/self_host_*/` yields only inline-`if` bodies
  (`if cn_has_prefix(...): return true` — the `:` is the `if`'s, already
  working), one closure body inside a *test string literal* in
  `self_host_resolver/main.gg:39`, and `case Error(e): return …` match-arm
  bodies. **No self-host code uses the broken function-expr-body form**, so the
  fix does not change self-host emit and `fixed_point` / `*_comparison` should
  stay stable. (Parent still validates `self_host_bootstrap_fixed_point` +
  `*_comparison` before integrate, per the gate-on-bootstrap rule.)
- **LLVM backend:** the fix is in shared GIR lowering (pre-backend), so both
  backends inherit it. Run `GG_BACKEND=llvm … return_expr_body` to confirm
  parity. No backend-specific code touched.

---

## 8. Recommended executor brief (one-line summary)

Fix (A): in `src/ir/lowering/functions.rs`, guard the trailing
return-slot-assign/drops/ret of the 4 `FunctionBody::Expression` lowering arms
(`lower_function` 1074, `lower_equip_method` 1370, `lower_generic_function` 1659,
`lower_method_instance` 2052) with `if !builder.is_terminated()`, always
`pop_scope_no_emit()`, keep the Inc-2.1a fault-block fill on both branches in
`lower_function`. Add `tests/fixtures/return_expr_body.gg` (expected output
above) + its runtime snapshot, extend `expression_body_functions.gg`. Do NOT
add a negative fixture and do NOT touch `closures.rs`. Validate: full
integration (C + `GG_BACKEND=llvm`), `self_host_bootstrap_fixed_point`,
`*_comparison`. Consider extracting the shared trailing block into a helper to
DRY the 4 arms (optional). Cite `docs/devbook/24-layering-discipline.md` (write-
site fix; sibling of the already-hardened `set_terminator` Snag #33/#39 guard).
