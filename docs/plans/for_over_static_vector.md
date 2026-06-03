# FIDELITY brief — `for x in <module-level static Vector>` drops the loop (Rust + self-host, COUPLED)

## Goal
`for x in TABLE` where `TABLE` is a **module-level `static Vector[…]`** silently
emits NOTHING — the loop body never runs. Fix it in BOTH Rust `gg` (the oracle)
AND the self-host, COUPLED in one chain (owner-chosen) so parity doesn't dip and
Phase-2 Stage 1 unblocks. Root-caused 2026-06-03 (troubleshooter; confirmed by
experiment). Expected: flips `static_collection` + `static_vec_literal` (self-host
CRASH→MATCH) and makes the self-host's `lookup_resource_table` LIVE (it's
currently silently inert — a latent bug).

## Verified root cause
A module-level `static` lowers to `Operand::Constant(GlobalRef(name))`, NOT a
place. Both lowerers detect the collection kind only for a *place* operand:
- **Rust:** `src/ir/lowering/stmts/for_loops.rs:~193` (`lower_for`) —
  `if let Operand::Copy(p)|Operand::Move(p) = iter_op { … collection_kind … }`.
  A `GlobalRef` constant → `collection_kind=None` → falls to `lower_for_iterable`
  (`for_loops.rs:~791`) → no user `iter()`/`next()` on the raw `Vector__T` alias
  → `else { return }` at `for_loops.rs:~840` → **loop emits nothing.** ⚠ NOTE:
  `iter_type` IS resolved correctly to `Vector__T` (`GlobalRef`→`global_type_names`,
  `type_reg.rs:~268`) — the failure is purely the place-vs-constant guard, NOT
  type inference. Minimal repro `/tmp/static_for_repro.gg` (a `static Vector[Decl]`
  + `for entry in TABLE`): Rust `gg` prints `0`, should print `slice/trim/2`. A
  LOCAL Vector works; `TABLE.len()`/`.get(0)` on the static work — only the
  for-loop over the static is broken.
- **Self-host:** TWO stacked gaps — (1) `lower.gg`'s `IStaticDecl` handler
  (`~:11585`, the `else: pass` fall-through) + `lower_static_ref_ident` (`~:3384`)
  only register statics with int/float/runtime-call/`None` initializers, so a
  `static Vector = [literal]` is NEVER registered (→ `[bug] EIdentifier: unknown
  identifier` placeholder); (2) the self-host `lower_for` mirrors the Rust
  place-vs-constant guard.

## Part A — Rust fix (`src/ir/lowering/stmts/for_loops.rs`, `lower_for` ~187-216)
**Preferred (better-layered):** derive `collection_kind` from the already-correct
`iter_type` (`ctx.type_registry.get(iter_type)` → `TypeDef.metadata.collection_kind`)
instead of re-deriving it from the `iter_op` place — the place-keyed lookup is
REDUNDANT with `iter_type`. Re-pin by content.
- ⚠ **Borrow/drop:** the static is BORROWED (a `GlobalRef`), not owned — the loop
  must iterate it by reference and must NOT consume/drop it (a `static` lives for
  the program). If you instead materialize the constant into a local, ensure the
  local is a borrow/non-owning view, not an owning copy that gets dropped.
- ⚠ **No regression to for-over-LOCAL / for-over-range / for-over-Dict/Set/iter:**
  the common cases currently flow through the place path. Whatever you change must
  produce the IDENTICAL `collection_kind` for them. RUN the existing for-loop
  fixtures (and the whole suite) — this is a HOT path.
- Verify: `/tmp/static_for_repro.gg` → `slice/trim/2`; the LOCAL-Vector control
  still works.

## Part B — self-host fix (`tests/fixtures/self_host_lowerer/lower.gg`)
(a) **Register composite-init module-level statics:** the `IStaticDecl` handler
(`~:11585`) + `lower_static_ref_ident` (`~:3384`) must register + resolve a
`static Vector[T] X = [literal]` (and similar composite initializers), not just
the int/float/runtime-call/None cases. Mirror how Rust registers a `static`
GlobalRef with its `Vector__T` type.
(b) **Port the `lower_for` place-vs-constant fix** into the self-host `lower_for`
(the same iter_type-derived `collection_kind`, or materialize-the-GlobalRef).
- Verify by RUNNING through the driver: `/tmp/static_for_repro.gg` → `slice/trim/2`;
  `static_collection` + `static_vec_literal` → MATCH vs `gg run`.

## ⚠ Part C — the BIG RISK: `lookup_resource_table` goes LIVE
The self-host's `for entry in RESOURCES` (`lir_lower.gg:~197`, `lookup_resource_table`)
is currently DEAD in the self-compiled driver (the loop is dropped), so the driver
silently returns `None` and limps on the hardcoded `build_resource_metadata`
fallback. Part B makes that loop WORK → `lookup_resource_table` becomes LIVE and
starts driving ABI/type decisions from the `RESOURCES` table.
- **If the live table DISAGREES with the `build_resource_metadata` fallback on any
  symbol, the self-host's emitted C changes** → `c_emit_comparison` /
  `self_host_runtime` / `bootstrap_fixed_point` could move/regress.
- **REQUIRED:** `bootstrap_fixed_point` must stay GREEN (the driver self-compiles
  WITH the now-live table — the decisive signal) and `c_emit_comparison` must not
  DROP. If they do, the table-vs-fallback divergence is a SEPARATE bug — STOP and
  REPORT it (do NOT force, do NOT reshape the table to paper over). A clean partial
  (Part A + the user-facing Part B, with the RESOURCES activation surfaced as a
  blocker) is acceptable if the table diverges.

## Part D — snapshot re-validation (the oracle changed)
Part A changes the Rust oracle for any for-over-static fixture. After both fixes:
- Run `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → report the new MATCH total +
  which fixtures changed (oracle corrections / new MATCHes / any de-inflated
  both-wrong MATCH).
- `self_host_runtime` regressed=0 against committed snapshots, THEN regen. ⚠
  Because the oracle changed, a previously-committed snapshot for a both-wrong
  for-over-static fixture may now be STALE (self-host output == old wrong oracle).
  The stability-gated regen validates against the LIVE (corrected) oracle, so it
  will DROP any snapshot that no longer matches — report adds AND drops, and
  confirm every drop is an honest de-inflation (a fixture that was both-wrong), not
  a real regression.

## Files / zones
- Rust: `src/ir/lowering/stmts/for_loops.rs` (+ maybe `type_reg.rs` read-only).
- Self-host: `tests/fixtures/self_host_lowerer/lower.gg` (`IStaticDecl` ~:11585,
  `lower_static_ref_ident` ~:3384, `lower_for`). ⚠ Same file as recent chains but
  hunk-disjoint from the landed field-store (~:7130) / borrowed-String / bug-2.
- Snapshots: `tests/fixtures/runtime_snapshots/*.out` (adds + possibly drops).

## Gates (FULL suite — this is a Rust FRONTEND change, all fixtures recompile)
- `cargo test --lib` GREEN (baseline 1072/0).
- **FULL `cargo test --test integration --release -- --test-threads=4`** — ZERO
  new failures (this is the parent's sweep; the executor MUST run it here because a
  frontend for-loop change has whole-corpus blast radius — do not skip it).
- `bootstrap_fixed_point` GREEN (the table-live guard). `c_emit_comparison` ≥887,
  `lowerer_comparison` ≥958. `self_host_runtime` regressed=0 (+ regen, report
  adds/drops). `runtime_diff` MATCH — report the new total (target ≥334; +2 if
  static_collection/static_vec_literal flip; account honestly for any de-inflation).

## Worktree discipline (executor)
`pwd` + `git rev-parse --show-toplevel` FIRST; inside your worktree, NEVER
`/workspace/gorget-1`. `git merge --ff-only gorget-1`. Stage ONLY the touched
`src/...rs` + `lower.gg` + the new/removed `runtime_snapshots/*.out` (use
`git add <exact paths>` + `git rm` for dropped snapshots; NEVER `git add -A`).
Land Part A and Part B as **separate commits** (Rust fix, then self-host fix) so
they're independently reviewable + cherry-pickable. If Part C surfaces a
table-divergence blocker, commit Part A + the user-facing Part B and REPORT the
RESOURCES-activation blocker separately — do NOT force it.
