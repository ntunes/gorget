# Brief — static-struct field access returns garbage (Bug #1 addressing + Bug #2 init)

**Track:** BUG (Rust gg correctness, both backends). **Scout:** aed47100 (RUN-verified, GIR-level smoking gun).

## Repro (CONFIRMED, broader than the TODO scoped it)
EVERY field read/store on a module-level `static` struct is broken — not just resource fields:
| Program | Self-host/Rust result | Expected |
|---|---|---|
| `static Box2 B = Box2(Vector[int]())`; `B.items.push(7); B.items.get(0).unwrap()` | `0` | `7` |
| LOCAL `Box2 b = Box2(Vector[int]())`; `b.items...` | `7` ✓ | `7` |
| `static Point P = Point(3,4)`; `print(P.x)` | `0` | `3` |
| `static Point P = Point(3,4)`; `P.x = 99; print(P.x)` | `0` (store dropped) | `99` |
| `static Point P = Point(3,4)`; `Point q = P; print(q.x)` | `3` ✓ | `3` |

Affects C AND LLVM (the bugs are in shared GIR lowering, pre-backend). Smoking gun (`--emit-gir`): the static repro's whole `main` lowers to `_1 = const unit; printf("%lld", _1)` — the push/get/store all vanish; `P.x = 99` produces ZERO instructions.

## Root cause — TWO compounding write-site bugs in GIR lowering
**Bug #1 — `Place` cannot root at a global; field projection degrades to `const unit`/no-op.**
`Place = { local: LocalId, projections }` (`src/ir/instructions.rs:5`) roots only at a LOCAL; globals are reached via `GlobalRef`/`GlobalRefPtr` constants + `GlobalAssign`. So a static base has no `Place` to project into:
- `try_resolve_field_place` (`src/ir/lowering/exprs/mod.rs:2304-2309`): the `Expr::Identifier` arm only does `ctx.lookup_local(name)` → a global returns `None`.
- `lower_field_access` (`:2073`): lowers the global object to `Operand::Constant(GlobalRef)` → the `if let Operand::Copy|Move(place)` block (`:2076`) is skipped → returns the `Operand::Constant(Constant::Unit)` fallback at `:2271` (the `_1 = const unit`).
- Field-store (`src/ir/lowering/stmts/assigns.rs:613` → `None`, then fallback at `:647` requires `Copy|Move(place)`) → store silently dropped (returns at `:731`).

**Bug #2 — `static Struct = Ctor(non-const args)` initializer never constructed at runtime.**
`initializer_needs_synthetic_fn` (`src/ir/lowering/mod.rs:2414-2417`) returns true ONLY for `Expr::ArrayLiteral | DictLiteral`. A struct-ctor RHS (`Box2(Vector[int]())`) falls to `eval_static_init` which has no struct-ctor arm → `GlobalInit::Zeroed`. So `B` is zeroed, `B.items` is a NULL GorgetArray, no `__gg_static_init_B` emitted. (In-code comment admits: "v1 deliberately does NOT widen to general non-const RHS.")

Both are required for the RESOURCE repro to print `7`. The SCALAR repro (`Point(3,4)`, const args → real `bytes[8]` init) isolates **Bug #1 alone** (`P.x`→0 while `Point q=P`→3).

## The fix
**Bug #1 (the addressing — primary):** make a static base addressable by materializing it into a local before projecting. Centralize in ONE helper (per "Sibling-site drift") used by `try_resolve_field_place`, `lower_field_access`, AND the `assigns.rs` field-store fallback (+ nested-chain callers `:854`/`:1278`/`:1345`): detect `Expr::Identifier(name)` where `ctx.global_names.contains(name)` and:
- emit `&NAME` via `Operand::Constant(Constant::GlobalRefPtr(name))` → `Inst::GlobalAddr` → a real `*mut <T>` pointer (`src/lir/lower/operands.rs:139`),
- store it into a fresh local typed `Ptr(<struct-type>)` — the struct type name is in `ctx.global_type_names` (`context.rs:349`, populated at `mod.rs:1230`),
- proceed through the EXISTING pointer-deref field path (the `pointee_type` → `Projection::Deref` branches at `mod.rs:2143-2151` reads, `assigns.rs:695-702` writes already handle a pointer-typed local). Resource-field reads still get the `Ptr(field)` borrow wrap (predicate at `:2207`), so `B.items.push()` borrows in place.

**Bug #2 (the init):** widen `initializer_needs_synthetic_fn` to also route a struct-constructor RHS through `synthesize_static_init_fn` (`:2373-2384`/`:2419`) so `__gg_static_init_B()` runs `Box2(Vector[int]())` at startup. The synthetic-fn route goes through normal `lower_function`, so it wires drops — likely also closing the deferred "static collection prologue doesn't set `.val_drop`/`.elem_drop`" note (`TODO.md`). **HEED the §3 init-ordering caveat** (`docs/plans/bugB_static_collection_init.md`): a struct ctor whose args read OTHER statics has the same ordering interaction as the enum/collection cases already routed there — scope the widening to struct ctors and verify ordering, do not over-widen to general non-const RHS without the §3 decision.

Cross-check the `static_vec_index_load` sibling (`TODO.md`, the static-base index-load zeroed-temp) — same `Place`-can't-root-at-global class; verify the Bug#1 helper generalizes (or note it as a follow-up).

## Fixtures to ADD (runtime-snapshot lock-in, both backends)
1. `static_struct_resource_field.gg` — resource repro, assert `7` (+ a `.len()`-after-two-pushes variant asserting `2`). Exercises #1 + #2.
2. `static_struct_scalar_field.gg` — `static Point P = Point(3,4)`; assert `P.x`→`3`, `P.y`→`4` (isolates #1; passes once addressing is fixed, independent of #2).
3. `static_struct_field_store.gg` — `P.x = 99; print(P.x)` → `99` (the dropped-store path).
Add each to `tests/fixtures/runtime_snapshots/` so the parity net catches regressions.

## Gate battery
- `cargo build` + `cargo test --lib`.
- `cargo test --test integration -- --test-threads=4` (C) + targeted `GG_BACKEND=llvm` run of the new fixtures + the `static_*` family (shared bug → both must be green).
- WATCH existing `static_*` fixtures (`static_vec_literal`, `static_enum_init`, `static_init_imported`, `static_vec_index_load`) for regression — they share the `__gg_static_init`/global-addressing machinery Bug #2 widens.
- ASan on the new fixtures + `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — Bug#2's runtime init + Bug#1's pointer materialization touch ownership/drop (CLAUDE.md invariant #7).

## Risk
Moderate. Bug #1 = several lowering sites → centralize the "global identifier → addressable pointer local" materialization in one helper (sibling-site discipline). Bug #2 carries the init-ordering caveat — scope to struct ctors, verify §3. Both are pure GIR-lowering changes; no ABI/backend touch. If Bug #2's ordering proves thorny, land Bug #1 alone first (it fixes the scalar-field class + is independently valuable) and split Bug #2 to a follow-up — but the resource repro needs both.

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `src/ir/lowering/exprs/mod.rs`, `src/ir/lowering/stmts/assigns.rs`, `src/ir/lowering/mod.rs`, the new fixture `.gg` files + `tests/fixtures/runtime_snapshots/*.out`, and `tests/integration.rs`. No `git add -a`.
