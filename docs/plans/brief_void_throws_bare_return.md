# Brief — Rust gg: bare `return` in a `void throws E` fn miscompiles (C type error)

**Track:** BUG (Rust backend correctness). **Scout:** a9938e3f (RUN-verified, exact patch prototyped).

## Repro (CONFIRMED)
A `void f(...) throws E` with an explicit bare `return` on some path → Rust gg emits a C type error:
```
error: incompatible types when returning type 'int32_t' but '__gg_Result__void__RuntimeException' was expected
  return __v20;   // __v20 = (int32_t)0LL;  ← const_unit()
```
The FALL-OFF path (implicit end-of-fn) already returns `*(Result__void__E*)_0` correctly; only the EXPLICIT bare-return path emits the wrong int32 `0`.

## Root cause
`src/ir/lowering/stmts/mod.rs:2070` — the `else` branch of `lower_return` (the `expr == None` bare-return case):
```rust
} else {
    ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
    builder.ret(FunctionBuilder::const_unit());   // ← BUG: const_unit() is int32 0
}
```
For a `void throws E` fn the return type is WIDENED to `Result__void__E` (widening at `src/ir/lowering/functions.rs:715` for fns, `:1060` for methods; `LocalId(0)`'s type is the widened Result). `const_unit()` is the wrong type. `lower_return` is the single dispatch from `Stmt::Return` (`stmts/mod.rs:213`) → functions AND methods both route here (one site = whole class). This also affects the wider `T throws E` class (e.g. `int throws E`), all fixed by the same gate.

## Self-host reference (mirror — already fixed there by `167cb1b6`)
The self-host `SReturn case None` gates on typed metadata `enum_category_of(ret_ty).category == ENUM_CAT_RESULT` and returns the zero-inited `_0` (tag 0 = Ok) when true, else the unit route. The Rust analog accessor exists: `TypeRegistry::enum_category(type_id) -> Option<EnumCategory>` (`src/ir/types.rs:735`), `EnumCategory::Result` (`:148`).

## The fix (typed metadata, NOT name-matching)
Replace the `else` branch at `src/ir/lowering/stmts/mod.rs:2067-2071`:
```rust
} else {
    // Bare `return` in a throws-widened fn: ret_type is Result__V__E, not unit.
    // Return the zero-inited _0 (tag 0 = Ok), not const_unit() (an int32 → C type
    // mismatch). Typed metadata (enum_category), not name-matching. Mirrors the
    // self-host fix 167cb1b6 and the fall-off path (which returns copy(_0)).
    let ret_type = builder.locals[0].type_id;
    if ctx.type_registry.enum_category(ret_type) == Some(crate::ir::types::EnumCategory::Result) {
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, Some(LocalId(0)));
        builder.ret(FunctionBuilder::copy(LocalId(0)));
    } else {
        ctx.drops.emit_early_exit_drops(builder, &ctx.type_registry, DropScopeKind::Function, None);
        builder.ret(FunctionBuilder::const_unit());
    }
}
```
Notes: `crate::ir::types::*` is already imported at `stmts/mod.rs:12` (so `EnumCategory`, `LocalId` are in scope — confirm and drop the path prefix if so). The throws-case passes `Some(LocalId(0))` to exclude `_0` from drops (it's being returned), matching the value-return path at `:2028`. The fix is in shared GIR→LIR lowering (before the C/LLVM split) → fixes BOTH backends.

**Executor: VERIFY the proposed patch compiles & the repro flips before committing — the scout reasoned it but did not apply it.** Confirm `enum_category(UNIT_TYPE) == None` so plain-void bare-return is untouched.

## Fixture to ADD (expected output = correct language behavior)
`tests/fixtures/void_throws_bare_return.gg`:
```
struct RuntimeException:
    String msg

void define_own_property(int obj_id, String key) throws RuntimeException:
    if obj_id < 0:
        return
    print(f"set {key} on obj {obj_id}")

void main():
    match define_own_property(1, "foo"):
        case Ok(_):
            print("ok path")
        case Error(_):
            print("err path")
    match define_own_property(-1, "bar"):
        case Ok(_):
            print("bare-return ok")
        case Error(_):
            print("bare-return err")
    print("done")
```
Expected stdout:
```
set foo on obj 1
ok path
bare-return ok
done
```
Wire `#[test] fn void_throws_bare_return()` in `tests/integration.rs` next to `void_throws()` (~`:3429`) using the `run_gg(fixture, expected)` helper (`:253`). The harness asserts an INLINE expected string (no `.expected` file): `run_gg("void_throws_bare_return.gg", "set foo on obj 1\nok path\nbare-return ok\ndone")` (confirmed `integration.rs:3447`). The fixture compiles under current Gorget syntax AS WRITTEN — review pass 1 prototyped the patch and RAN it: repro flips on both C and LLVM, no fixture correction needed.

**Review pass 1 SIGN OFF (measured-green):** the exact patch above compiled clean (`cargo build --release`) and flipped the repro on BOTH backends; `void_throws` (fall-off) unchanged. Minor citation note: the `functions.rs:715/:1060` "widening" refs are the throws-context TRACKING lines; the real widening is `src/ir/lowering/mod.rs:617-625` (free fns) / `functions.rs:911-917` (methods) — cosmetic, the fix is unaffected. The fall-off reference shape `ret(copy(LocalId(0)))` is at `functions.rs:846`.

## Gate battery
- `cargo build` + `cargo test --lib`.
- `cargo test --test integration void_throws_bare_return` (C) AND `GG_BACKEND=llvm cargo test --test integration --release void_throws_bare_return`.
- Re-run existing `void_throws` (fall-off) + `catch_basic` for no-regression.
- Parent runs full integration both backends.

## Sibling findings → TODO (do NOT fix here)
1. The same lowering bug hits any `T throws E` (e.g. `int throws E`) bare-return — the `enum_category == Result` gate fixes all; the fixture covers `void`, note the wider class.
2. Latent: `gg check` ACCEPTS a bare `return` inside `int throws E` (silently yields `Ok(0)`) — arguably a "missing return value" semantic error. Record as a separate TODO item.

## Risk
Very low. Same typed discriminator the fall-off path uses; plain-void untouched; no `*_comparison` divergence (no existing fixture exercises this).

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `src/ir/lowering/stmts/mod.rs`, `tests/fixtures/void_throws_bare_return.gg`, its `.expected` (if the harness uses one), and `tests/integration.rs`. No `git add -a`.
