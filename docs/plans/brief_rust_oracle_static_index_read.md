# Brief — RUST-ORACLE fix: static-Vector struct index-read (recovers static_vec_index_load) — READY

⚠ A RUST `src/` CHANGE (the oracle is the buggy side; per [[feedback-rust-not-sacrosanct]] "fix the
oracle when Rust is buggy"). RUN-verified ship-ready + FULL-SUITE-clean by a scout. Recovers
`static_vec_index_load` 375→**376** AT the correct behavior + fixes a real Rust compiler bug. Owner
promotes `main`.

## Bug (Rust `gg` is wrong; the self-host is correct post-3h)
`tests/fixtures/static_vec_index_load.gg`: Rust `gg run` emits the BUGGY `i0=0:0`/`i2=0:0` (zeroed
structs) — the whole `TABLE[i].field` index-read collapses to literal 0 (the C `main` has NO array-read
call; `__v0 = (int32_t)0LL`). Self-host (post the landed 3h EIndex fix) emits the CORRECT
`i0=alpha:10`/`i2=gamma:30`.

## Root (`file:line`)
`src/ir/lowering/exprs/methods.rs:3185` — `lower_index_access`'s place-guard is
`if let Operand::Copy(ref place) | Operand::Move(ref place) = obj`. A module-level `static Vector[…]`
lowers to `Operand::Constant(Constant::GlobalRef(name))` (`src/ir/lowering/exprs/mod.rs:172`) — NEITHER
Copy nor Move → falls through to `Operand::Constant(Constant::Unit)` (`methods.rs:3288`), silently
dropping the read; the `Unit`/field-access const-folds to 0. (`.get(i).unwrap()` works because
method-call lowering handles a `GlobalRef` receiver, `methods.rs:1766`.) Exact Rust analogue of the
self-host EIndex gap.

## The fix (23 lines, `methods.rs` — mirror the EXISTING in-tree pattern)
Before the place-guard in `lower_index_access`, materialize a `GlobalRef` object operand into a local
place — mirroring `init_borrow_iter_local` (`src/ir/lowering/stmts/for_loops.rs:49`, the for-over-static
fix): `add_local(base_type)`, then `assign_mode(<Borrow for a resource / Copy for a value>, local, obj)`,
then rebind `obj = Operand::Copy(local)`. A `Vector`/`Dict` is a resource → **Borrow** (zero-cost,
CoW-default-borrow; the global retains ownership, freed once by static teardown — no double-free). Then
the existing place path emits the real `*(GorgetArray*)(…)` borrow + `gorget_array_get(...)` per read.
Plus: remove the `#[ignore]` at `tests/integration.rs:319` (−4 lines) — that test ALREADY asserts the
correct `i0=alpha:10\ni2=gamma:30` with a comment "flip to active when the index-load-on-global bug is
fixed." NO expected-output VALUE changes anywhere.

## Reviewers verify (≥3, full-suite — it's a Rust change)
1. Root confirmed: `lower_index_access` drops a `GlobalRef` object (Copy/Move-only guard). The
   materialize-to-local mirrors `init_borrow_iter_local` (resource→Borrow, value→Copy) correctly; no
   double-free (global owns; the local is a borrow).
2. **Full suite (NOT just self-host gates):** `cargo build` clean; `cargo test --lib` 1072/0;
   `cargo test --test integration -- --test-threads=4` **1187/0** (the now-un-ignored
   `static_vec_index_load` passes; ZERO other Rust regressions — esp. other index/static/vector
   fixtures). Confirm no expected-output value edits (only the `#[ignore]` removal).
3. Self-host: `cargo run -- run static_vec_index_load` → `alpha:10`/`gamma:30`; force-rebuild the
   self-host driver, `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → `static_vec_index_load` flips
   WRONG→MATCH (self == corrected-Rust) → 375→**376**; regen re-adds
   `tests/fixtures/runtime_snapshots/static_vec_index_load.out` (purely additive). Self-host lock-in
   unaffected (Rust-side change).
4. SERIAL gates; revert with `git checkout -- .` (NEVER `git stash`).

## Execute
Worktree, ff gorget-1. `git add src/ir/lowering/exprs/methods.rs tests/integration.rs
tests/fixtures/runtime_snapshots/static_vec_index_load.out`. Commit. Gate: integration 1187/0,
self_host_runtime_diff 376, lock-in re-add static_vec_index_load (376/0), lowerer 960 / c_emit 891
(unchanged by a Rust-side change). Files: `src/ir/lowering/exprs/methods.rs` (+23), `tests/integration.rs`
(−4), the re-added snapshot.
