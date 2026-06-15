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

**Bug #1 — review R4 (PRECEDENT + design point, fold these):**
- The IDENTICAL fix already shipped for index-load: `lower_index_access` (`src/ir/lowering/exprs/methods.rs:3272-3282`, 2026-06-04) detects `Operand::Constant(GlobalRef)`, materializes into a local, and proceeds through the place path. Its regression test is `tests/fixtures/static_vec_index_load.gg` (passing). MIRROR / share this precedent — do not invent a parallel mechanism.
- **Why `Ptr`+`GlobalRefPtr`+`Deref` (and NOT the index-load's `Borrow`/`Copy` mode):** for the STORE path (`P.x = 99`, currently emits ZERO instructions), a `Copy`-mode value materialization would write to a local COPY, not the global (the by-value-copy hazard `static_global_method_call.gg`'s header comment warns about). The write path NEEDS the write-through-pointer (`Ptr`+`Deref`) approach. Call this out — it's the justified divergence from index-load.
- **Type caveat:** `GlobalRefPtr`'s type INFERENCE returns the BASE type, not `Ptr(base)` (`type_reg.rs:274-280`). Type the fresh local as a real `Ptr(struct)` via `register_mut_ptr_type` (`context.rs:1623`); do NOT reuse that inference. (`lookup_global_type` is at `type_reg.rs:287`.)

**Bug #2 (the init) — review R1+R2 (REFRAMED, literal-ness guard is BLOCKING):**
- **R2 — the machinery is ALREADY BUILT, this is a few-line predicate widen.** `synthesize_static_init_fn` (`mod.rs:2430-2474`), `ctx.synthetic_static_init_fns`, the lowering loop (`mod.rs:1271-1275`), the DCE root seed (`optimize.rs:217`), the prologue emit (`c_lir/mod.rs:1748-1768`) ALL exist and already fire for array/dict literals AND enum-typed statics (the dispatch `mod.rs:2373` is `if initializer_needs_synthetic_fn(...) || is_enum_typed`). Bug #2 = widen the predicate `initializer_needs_synthetic_fn` (`mod.rs:2414-2417`), REUSE the shipped route — do NOT build new infra.
- **R1 (BLOCKING) — scope by LITERAL-ness of the ctor args, NOT just "is a struct ctor".** `eval_static_init`'s catch-all (`mod.rs:2703-2732`) ALREADY turns a LITERAL-arg struct ctor into a compile-time `GlobalInit::Struct` (`:2720-2728`); only NON-literal-arg ctors fall to `GlobalInit::Zeroed` (`:2732`). RUN-VERIFIED: `static Point P = Point(3,4)` and `static Counter counter = Counter(0)` (`static_global_method_call.gg:27`) currently emit a compile-time `__gg_Counter __lir_g0 = {0LL}` — NO synthetic fn; their `P.x`/`counter` bug is PURELY Bug #1. **If you widen the predicate to ALL struct ctors, `Point(3,4)`/`Counter(0)` get diverted from `GlobalInit::Struct` to the runtime synthetic-fn path → regresses `static_global_method_call.gg` + non-neutral C for the scalar fixtures.** Route through the synthetic fn ONLY when the ctor args are NOT all compile-time literals (mirror the `literal_to_global_init`/`literal_to_global_init_arg` split at `mod.rs:2713-2720`, `:2769-2782`). Literal-arg ctors STAY on the compile-time `GlobalInit::Struct` path. This also makes Bug #2 output-neutral on every existing static fixture. The §3 init-ordering caveat (`docs/plans/bugB_static_collection_init.md`) then bounds it to "no worse than the already-accepted enum case" (a non-literal-arg ctor reading another static is the same ordering class enums already took on).

Cross-check the `static_vec_index_load` sibling (already FIXED by the index-load precedent above) — confirm the Bug#1 helper shares its shape.

**Bug #1 vs Bug #2 split (R1/R4):** the SCALAR repro is purely Bug #1 (its init is already correct compile-time `GlobalInit::Struct`); only the RESOURCE/non-literal-arg repro needs Bug #2. Bug #1 alone fixes the scalar-field + store classes and is safe to land first.

## Fixtures to ADD (runtime-snapshot lock-in, both backends)
1. `static_struct_resource_field.gg` — resource repro, assert `7` (+ a `.len()`-after-two-pushes variant asserting `2`). Exercises #1 + #2.
2. `static_struct_scalar_field.gg` — `static Point P = Point(3,4)`; assert `P.x`→`3`, `P.y`→`4` (isolates #1; passes once addressing is fixed, independent of #2).
3. `static_struct_field_store.gg` — `P.x = 99; print(P.x)` → `99` (the dropped-store path).
Add each to `tests/fixtures/runtime_snapshots/` so the parity net catches regressions.

## Review R3 — self-host is NOT mirrored; the bootstrap gate does NOT validate parity here (fold)
Neither bug is mirrored in the self-host (`self_host_lowerer/lower.gg:3182-3200` `IStaticDecl` only routes literal array/dict; no global-field/index materialization; the self-host sources contain ZERO `static` decls — grep-confirmed). So `self_host_bootstrap_fixed_point` CANNOT exercise either bug and stays green regardless — it does NOT validate the self-host side (the "diagnostic-always-pass" trap). `lowerer_comparison` fn-count won't catch it either. **Report to parent for TODO:** a self-host-mirror follow-up (Bug #1 global-field/index materialization + Bug #2 predicate widen in `lower.gg:3182`, matching `docs/plans/bugB_static_collection_init.md` §4.2/§6). This brief is the RUST-side fix only; do not imply the bootstrap covers the self-host.

## Gate battery
- `cargo build` + `cargo test --lib`.
- `cargo test --test integration -- --test-threads=4` (C) + targeted `GG_BACKEND=llvm` run of the new fixtures + the `static_*` family (shared bug → both must be green).
- WATCH existing `static_*` fixtures for regression — they share the `__gg_static_init`/global-addressing machinery: `static_vec_literal`, `static_enum_init`, `static_init_imported`, `static_vec_index_load`, AND **`static_global_method_call`** (`integration.rs:3141` — the R1 regression canary: its `Point(3,4)`/`Counter(0)` literal-arg ctors MUST stay compile-time `GlobalInit::Struct`, not divert to the synthetic-fn path).
- ASan on the new fixtures. Still run `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) as a no-regression guard for the broader lowering change — but per R3 it does NOT prove this fix; the new RUST-side runtime-snapshot fixtures (on both backends) are the load-bearing net.

## Risk
Moderate. Bug #1 = several lowering sites → centralize the "global identifier → addressable pointer local" materialization in one helper (sibling-site discipline). Bug #2 carries the init-ordering caveat — scope to struct ctors, verify §3. Both are pure GIR-lowering changes; no ABI/backend touch. If Bug #2's ordering proves thorny, land Bug #1 alone first (it fixes the scalar-field class + is independently valuable) and split Bug #2 to a follow-up — but the resource repro needs both.

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY `src/ir/lowering/exprs/mod.rs`, `src/ir/lowering/stmts/assigns.rs`, `src/ir/lowering/mod.rs`, the new fixture `.gg` files + `tests/fixtures/runtime_snapshots/*.out`, and `tests/integration.rs`. No `git add -a`.
