# Wave A2-R brief — D12: drop-purity enforcement, Rust half (straight to error)

> **Batch A, track 2** (ratified wave plan). **SPLIT (scout finding, reported):**
> the self-host has NO `E_MoveWithoutOperator` surface at all (no safety pass, no
> ownership diagnostics — `diagnostic.gg:41-60`); its half is a ~250-400-line
> NEW-PASS port. This brief is **A2-R** (Rust + fixtures + docs + the ICE rider);
> **A2-S** (the self-host port) is FILED HIGH for the next wave slot. Core #8
> assessment (scout): Rust-first creates no new OBSERVABLE divergence — it widens
> the same pre-existing structural check-surface gap the entire Box/Task
> single-owner family already has.
> **Zone (pass-1 corrected):** `src/semantic/` (scope.rs, mod.rs, type_utils.rs,
> errors.rs, safety/ — NOT typecheck.rs) + `src/ir/lowering/` (the ICE rider in
> assigns.rs) + `spec/ggdef/` (the Option-taint gap fix + 10th test — small,
> in-track) + 3 fixture migrations + probe fixtures + docs. ⚠ A1/A3 touch
> adjacent semantic files with disjoint hunks — parent integrates sequentially.
> **Scout:** `/tmp/scout_wA2_report.md`, prototype `/tmp/scout_wA2_prototype.patch`
> (500 lines), measured end-to-end.
> **Status:** v4 — pass-3 (executor-simulation) confirmed the closure ruling +
> message mechanism + all citations, and raised 6 reservations, ALL FOLDED:
> **(1) BLOCKING — the PLACE-SHAPE hole**: `tainted_place_name` was
> identifier-only in practice (`expr_types` is SPARSE — field/index spans never
> recorded), so `R c = hh.r` was production-ACCEPTED while ggdef REJECTS, and
> the accepted program DOUBLE-DROPS (measured: "drop R" printed twice). Now a
> mandated work item (see Design). (2) the `Expr::ImplicitClosure` sibling arm
> (`check_expr.rs:999` — `hs.map(it.r)`) joins via a SHARED tail-check helper.
> (3) generics are T-BLIND (pre-mono check): pinned out-of-scope + FILED
> (inherited single-owner-family debt). (4) site-count corrected: baseline has
> exactly 2 `E_MoveWithoutOperator` construction sites (not "5"), 8-9
> post-track — pin-4's reason-field touches ALL of them. (5) the gate list now
> enumerates the closure + field-place probes. (6) the ICE rider's TODO targets
> NAMED precisely. Prior history: pass-1's 8 folds, pass-2's 5.
> Awaiting pass 4.

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
  equips (cross-module + generic-equip shapes proven by pass-1's `VectorDrain`
  probe), fixpointing over `field_types`/`variant_field_types` (enum payloads
  proven; cycle-terminating — flags read, defs never re-entered); accessor
  `is_drop_tainted_type` mirroring ggdef's `ty_tainted`. **Name-matching honesty
  (pass-1 struck the "none anywhere" claim):** the refcounted/sync-handle
  carve-out EXTENDS the existing builtin name-list
  (`matches!(def.name.as_str(), "Channel"|"Shared"|"Weak"|"Mutex"|…)`) — same
  pre-existing debt family as the Box/Task list; a user `struct Shared` would
  dodge taint. Acceptable HERE only because it extends existing debt; the typed
  builtin-marker fix is FILED (TODO), not this track's scope.
- **Critical enabler:** `is_copy_type` gains the Copy∧Drop exclusion
  (`type_utils.rs:95` area) — without it, ggdef's all-scalar `struct R: int id`
  never reaches any check.
- **⚠ THE PLACE-SHAPE WORK ITEM (pass-3 BLOCKING — a live double-drop):**
  `tainted_place_name`'s primary lookup (`expr_types.get(&e.span)`) is SPARSE —
  only call/method/scrutinee spans are recorded (`typecheck.rs:2272+`);
  FieldAccess/Index spans never are, and the fallback covers only
  Identifier/SelfExpr. Consequence (measured): `R c = hh.r` binds a field place
  UNREJECTED and the program runs R's custom drop TWICE; `return h.r` and
  closure-tail `h.r` same; ggdef rejects all three. FIX AT THE WRITER (layering
  rule 1): either record FieldAccess/Index expression types into `expr_types`
  during typecheck, or resolve field chains from the typed
  `DefInfo.field_types` the taint pass already populates — mirroring ggdef's
  `infer_ast_ty` (`elaborate/mod.rs:522-542`). The prototype's "covers EVERY
  place shape" comments are FALSE — correct them. ADD field-place + index-place
  probe fixtures per position AND 1-2 field-place tests to ggdef's own suite so
  the parity is TEST-VISIBLE (the ported identifier-shaped suite stays green
  across this divergence — that's the trap).
- **The six positions:** 1-2 (bare-assign, ctor/field-init) ride the EXISTING 2
  `E_MoveWithoutOperator` construction sites (pass-3 corrected the count:
  baseline has exactly 2 — `check_expr.rs:33`, `check_stmt.rs:1460`; the track
  grows them to 8-9, one per position — pin-4's reason-field change touches ALL
  of them) by unioning taint into `needs_explicit_move` + place-shape pre-checks; 3 (collection puts) reuses the arena-ingest gate's
  TYPED classifiers (`is_mutating_builtin_method` + `is_buffer_owning_receiver`);
  **4 in `Stmt::Return` AND the `FunctionBody::Expression` tail arm
  (`safety/check_stmt.rs:1726` — pass-1's blocking fold: ggdef rejects
  `R passthru(R x): x` at the expression-body tail via `elaborate/mod.rs:349-352`
  [pass-2 corrected the citation]; add the `tainted_place_name` check with the
  same `imported_module_depth == 0` gate + a probe; fresh-temp expr bodies
  `R make(): R(7)` stay legal; pass-2 derived this hunk from the spec alone — 11
  lines) AND — pass-2's Core-#4 ruling — CLOSURE returns, BOTH spellings: the
  `Stmt::Return` check already fires in closure block bodies (pass-3 verified);
  extend to closure EXPR-TAILS (`Expr::Closure` arm, `check_expr.rs:948`, ~9
  lines, same `imported_module_depth == 0` gate) AND — pass-3's sibling — the
  `Expr::ImplicitClosure` arm (`check_expr.rs:999`, the `hs.map(it.r)` shape)
  via ONE SHARED tail-check helper for both arms; ggdef gains the matching
  closure-tail check + an 11th/12th test pair — both sides agree before
  landing**; 5 off `compute_capture_set`; 6 inside
  `mark_bare_param_write_def` (pre-filtered by `deadwrite_params`, which excludes
  `self` — pin 3 holds by construction; note: `_`-prefixed params also dodge
  position 6, a known micro-divergence from ggdef, acceptable).
- **Fixture migrations:** the 12 sites, using CURRENT syntax (`!x` / `.clone()`)
  — D27's `^` re-sigils them in Batch C3 by design. Scout proved the migrated
  corpus runtime-stdout byte-identical AND parity-clean under the self-host
  oracle-diff.
- **New probe fixtures (pass-1 widened): port ALL of ggdef's normative suite**
  (`tests.rs:961-1128` — 6 rejections + 3 legal counterparts + the
  `&self`-borrow rejection sibling) PLUS the expression-body probes (reject +
  fresh-temp-legal) — position-per-fixture discipline throughout.
- **ggdef Option-taint gap (in-track, Core #8 both-ways):** production rejects
  `Option[R]` bare-assign (principled per D4 — prelude Option is an enum
  carrying R); ggdef ACCEPTS (phase-0 `Ty` has no Option → `Unknown` →
  untainted, `elaborate/mod.rs:493-501`). Fix ggdef's taint model to cover
  Option + add the 10th test so both sides agree BEFORE this lands.
- **ICE rider (~20-40 lines) — ⚠ THE ONE UN-PROTOTYPED HUNK (pass-1; the scout
  sized it, did not build it). Closes exactly TWO TODO entries (pass-3 named
  them — the earlier grep anchors MISSED both): the 🐛💥 Compound-assign
  resource-element ICE entry AND the operator-overload validator-panic sibling
  (`assigns.rs:1665`). Do NOT close the D12 parent-track entry (this track only
  half-closes it — A2-S remains):** in
  `lower_compound_assign` (`assigns.rs:1714-1721` confirmed), replace the shared
  vector/dict clone-read+shallow-assign branch with the existing
  `index_load_borrow` (`validate.rs:1267-1269`) for the `self` borrow; `__set`'s
  pre-drop gives drop-once. `v[i] += x` thereby MOVES the dead element per D12.
  The executor derives this fresh and gates it with a NEW tainted-compound
  fixture + ASan (pass-1 confirmed the ICE is live: `v[0] += Acc(5)` check-passes
  then panics the build at `ir/lowering/mod.rs:1763`) + ALL non-tainted
  compound-assign fixtures byte-identical (grep-defined set:
  `ls tests/fixtures | grep compound` — ~20 files; run them all, both binaries).
- **Docs write-through:** reference:2266 + design:460 + book/11:59 stop
  enumerating the closed single-owner set — custom-Drop types join per D4;
  D4's ledger consequences text is the source.

## Pins the reviewers must hold (scout §7)

1. **Shared/Weak/Mutex/Channel carve-out pinned EXPLICITLY**: refcounted/sync
   handles do NOT taint. ⚠ Pass-1 corrected the RATIONALE: the old
   "drop-count determinism is owned by the refcount" premise is FALSE-IN-FACT —
   **`Shared[R]` never runs R's custom drop AT ALL today** (pre-existing
   payload-drop bug, FILED HIGH in TODO; distinct from the Box2/`Shared[int]`
   entry). The carve-out remains behaviorally safe (handle copies cannot
   duplicate payload drops — there are currently zero either way) and consistent
   with `is_copy_type`'s handle classification; cite the filed bug in the code
   comment so the pin isn't ratified on a false premise.
2. **Dict-key-at-index-store** is a shared ggdef+production model edge (neither
   rejects) — note, don't fix here.
3. **Position 6 inherits the deliberate `self` exclusion** (the filed
   plain-`self` write-through bug / D2 track) — do NOT extend to `self` until
   that ruling's track runs.
4. **Position-aware diagnostics (pass-1 refuted the draft fix-it; pass-2
   specified the mechanism):** extend the `MoveWithoutOperator` error variant
   with a reason/position field, rendered under the SAME `E_MoveWithoutOperator`
   code (no new code). Message content: name the drop-taint CAUSE (ggdef's
   message is the model) + per-position remedies — positions 1-4/6 suggest `!x`
   or `.clone()`; position 5 (captures) suggests PASS-AS-ARG or a `Shared[T]`
   wrap ONLY (the `.clone()`-into-local suggestion does not compile — the clone
   is equally tainted; capture-list syntax is D5/D7, unbuilt). **The current
   message's `` `move` `` alternative is DEAD SYNTAX (does not parse —
   reserved-only) — remove it while touching the message.** GATE: the rendered
   capture-position message contains no `!` suggestion. (ggdef's own message
   advertises `!{c}` capture syntax neither implements — noted in the ggdef-gap
   work item.)
5. ggdef suite runs from `spec/ggdef/` (root `-p ggdef` selects 0 tests — scout
   measured; use `cd`-free invocation `cargo test --manifest-path spec/ggdef/Cargo.toml`).

## Gates (scout-measured; executor re-runs, FOREGROUND, chunked >600s)

build · lib 1105/0 · ALL ported ggdef probes (10 + expr-body pair + the CLOSURE
pair + the FIELD-PLACE/INDEX-PLACE probes per position — pass-3 completed the
enumeration; every probe ggdef-exact, and ggdef's own suite gains the closure +
field-place tests) ·
integration `drop` 40/0, `move` 56/0, `clone` 17/0 (3 known 120s self-host
DEBUG-build stragglers — use `GG_BUILD_TIMEOUT_SECS=600`) · **pass-1's
census-hole fixtures EXPLICITLY (outside all the above filters):
`cow_element_borrow_source_mutate_with` + `test_cleanup` + `test_with_clause` +
`drop_struct_local` — byte-identical both binaries** · lints 53/0 · ggdef 104+
new/0 (from `spec/ggdef/` or `--manifest-path`) · the 4 fault_*_drop fixtures
UNTOUCHED-green · migrated fixtures byte-identical stdout · the ICE-rider
fixture + ASan · `self_host_bootstrap_fixed_point` (bootstrap-inert expected —
0 self-host code changes — but run it, chunked per stage). Parent: full
both-backend sweep at integration.

## Executor protocol

Standard multi-agent rules in full. Apply `/tmp/scout_wA2_prototype.patch`,
re-derive hunk by hunk. Explicit-file staging. Commit:
`feat(semantic): A2-R/D12 — drop-purity straight-to-error (is_drop_tainted
pass + six positions + Copy∧Drop exclusion) + compound-assign move rider` +
standard trailers. FILE A2-S (self-host port) in TODO as part of the commit if
the parent hasn't already.
