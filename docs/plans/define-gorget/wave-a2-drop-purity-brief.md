# Wave A2-R brief — D12: drop-purity enforcement, Rust half (straight to error)

> **Batch A, track 2** (ratified wave plan). **SPLIT (scout finding, reported):**
> the self-host has NO `E_MoveWithoutOperator` surface at all (no safety pass, no
> ownership diagnostics — `diagnostic.gg:41-60`); its half is a ~250-400-line
> NEW-PASS port. This brief is **A2-R** (Rust + fixtures + docs + the ICE rider);
> **A2-S** (the self-host port) is FILED HIGH for the next wave slot. Core #8
> assessment (scout): Rust-first creates no new OBSERVABLE divergence — it widens
> the same pre-existing structural check-surface gap the entire Box/Task
> single-owner family already has.
> **Zone:** `src/semantic/` (scope.rs, mod.rs, type_utils.rs, typecheck.rs,
> safety/) + `src/ir/lowering/` (the ICE rider in assigns.rs) + 3 fixture
> migrations + new probe fixtures + docs. ⚠ A1/A3 touch the same semantic files
> with disjoint hunks — parent integrates sequentially.
> **Scout:** `/tmp/scout_wA2_report.md`, prototype `/tmp/scout_wA2_prototype.patch`
> (500 lines), measured end-to-end. **Status:** v1 — awaiting review passes.

## Verified premises

1. **The normative model is ggdef's 9-unit-test suite** (`spec/ggdef/src/tests.rs:986-1128`
   — one test per position + legal counterparts), not just the helper at
   `elaborate/mod.rs:571-583`. Production must match those tests exactly.
2. **The census under-enumerated one position**: ggdef's `d4_position_4_return`
   rejects even `R a = R(1); return a` (forces `return !a`) — the return position
   was never in the census scan. Exact measured rejection set: **12 sites / 3
   fixture files / 0 spectests** (`drop_collection_custom_elem` 3,
   `drop_collection_custom_elem_clone` 1 — the sole live-source→`.clone()` case,
   `drop_struct_collection_fields` 8 incl. the transitive-fixpoint proof
   `Wrapper`→`Container`). The 4 `fault_*_drop.gg` pass UNTOUCHED (C2 owns them).
3. The compound-assign ICE (`src/ir/lowering/mod.rs:1763`) confirmed live.

## Design (prototyped; the executor applies + re-derives hunk by hunk)

- **Typed plumbing:** `DefInfo.is_drop_tainted` (`scope.rs:47-54`) + new pass-3.55
  `compute_drop_taint` (semantic/mod.rs) seeding from `TraitRegistry.impls` Drop
  equips, fixpointing over `field_types`/`variant_field_types`; accessor
  `is_drop_tainted_type` mirroring ggdef's `ty_tainted`. Layering rule 2 clean —
  no name-matching anywhere.
- **Critical enabler:** `is_copy_type` gains the Copy∧Drop exclusion
  (`type_utils.rs:95` area) — without it, ggdef's all-scalar `struct R: int id`
  never reaches any check.
- **The six positions:** 1-2 (bare-assign, ctor/field-init) ride the existing 5
  `E_MoveWithoutOperator` sites by unioning taint into `needs_explicit_move` +
  place-shape pre-checks; 3 (collection puts) reuses the arena-ingest gate's
  TYPED classifiers (`is_mutating_builtin_method` + `is_buffer_owning_receiver`);
  4 in `Stmt::Return`; 5 off `compute_capture_set`; 6 inside
  `mark_bare_param_write_def` (the one producer).
- **Fixture migrations:** the 12 sites, using CURRENT syntax (`!x` / `.clone()`)
  — D27's `^` re-sigils them in Batch C3 by design. Scout proved the migrated
  corpus runtime-stdout byte-identical AND parity-clean under the self-host
  oracle-diff.
- **New probe fixtures:** the six ggdef-mirror rejection probes + three legal
  counterparts (ggdef-exact outputs) — the position-per-fixture discipline.
- **ICE rider (~20-40 lines, closes TODO:278 + :314):** in
  `lower_compound_assign` (`assigns.rs:~1690-1727`), replace the shared
  vector/dict clone-read+shallow-assign branch with the existing
  `index_load_borrow` for the `self` borrow; `__set`'s pre-drop gives drop-once.
  `v[i] += x` thereby MOVES the dead element per the D12 ruling.
- **Docs write-through:** reference:2266 + design:460 + book/11:59 stop
  enumerating the closed single-owner set — custom-Drop types join per D4;
  D4's ledger consequences text is the source.

## Pins the reviewers must hold (scout §7)

1. **Shared/Weak/Mutex/Channel carve-out pinned EXPLICITLY**: refcounted/sync
   types do NOT taint (their drops are runtime-managed; ggdef has no model) —
   state it in code comment + brief; do not let the fixpoint swallow them.
2. **Dict-key-at-index-store** is a shared ggdef+production model edge (neither
   rejects) — note, don't fix here.
3. **Position 6 inherits the deliberate `self` exclusion** (the filed
   plain-`self` write-through bug / D2 track) — do NOT extend to `self` until
   that ruling's track runs.
4. **Position-5 fix-it wording must NOT advertise capture-list syntax** (D5/D7
   surface lands later) — suggest `!x` / `.clone()` only.
5. ggdef suite runs from `spec/ggdef/` (root `-p ggdef` selects 0 tests — scout
   measured; use `cd`-free invocation `cargo test --manifest-path spec/ggdef/Cargo.toml`).

## Gates (scout-measured; executor re-runs, FOREGROUND, chunked >600s)

build · lib 1105/0 · the 6 rejection probes + 3 counterparts (ggdef-exact) ·
integration `drop` 40/0, `move` 56/0, `clone` 17/0 (3 known 120s self-host
DEBUG-build stragglers — use `GG_BUILD_TIMEOUT_SECS=600`) · lints 53/0 · ggdef
104/0 · the 4 fault_*_drop fixtures UNTOUCHED-green · migrated fixtures
byte-identical stdout · `self_host_bootstrap_fixed_point` (bootstrap-inert
expected — 0 self-host code changes — but run it: the semantic pass could
surprise). Parent: full both-backend sweep at integration.

## Executor protocol

Standard multi-agent rules in full. Apply `/tmp/scout_wA2_prototype.patch`,
re-derive hunk by hunk. Explicit-file staging. Commit:
`feat(semantic): A2-R/D12 — drop-purity straight-to-error (is_drop_tainted
pass + six positions + Copy∧Drop exclusion) + compound-assign move rider` +
standard trailers. FILE A2-S (self-host port) in TODO as part of the commit if
the parent hasn't already.
