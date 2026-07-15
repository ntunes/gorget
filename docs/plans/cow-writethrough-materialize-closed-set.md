# Excellence plan: finish CoW write-through + materialize closed set

## Context

Gorget’s core ownership rule (§3.1 / D1 / devbook/11):

> A resource is a **borrow** until a write. If the write reaches a real owner through an unbroken **`&` chain**, it **writes through**. If the chain hits an **immutable binding**, the write **materializes** a private copy *there*.

G1 (projected mutation materialize) and G2 (`&`-of-bare formation materialize) landed. The **uniform rule is still incomplete** at several closed-set positions. Incomplete positions are silent wrong-output bugs (often Core #8: both compilers agree on wrong), not design forks.

**Excellence north star for this campaign:** every position in the closed set behaves per §3.1 on **Rust gg + self-host**, with fixtures locking correct stdout, ASan clean on materialize/untrack paths, and no clone-stats regression on self-host self-compile beyond noise.

Related owner notes (this session): local `&`-binds may reopen later only if exclusivity-safe (out of v1 burn-down until D10 place-overlap is solid); free views / low cloning remain production goals (D15 refinement, not eager clone everywhere).

---

## Definition of done

1. **Closed-set totality (runtime):** every position class below has ≥1 positive fixture (correct expected output) that **both** compilers match (C backend primary; LLVM parity where the fixture is not backend-specific).
2. **No known silent drop of write-through** on Vector / Dict / Set value-element field stores, `for x in &coll`, nested `&` field places (snag #53), or owned-index field places (self-host mirror).
3. **No known silent write-through where materialize is required** for tracked roots (G1/G2 already) **plus** D2 plain-`self` and untracked alias chains (or those two explicitly deferred with open fixtures + TODO, if scoped out of wave 1 — see waves).
4. **Promote** `rust_value_index_element_field_writethrough` from inline Rust-only to corpus once self-host matches.
5. **devbook/11** “converging” marker removed or reduced to only explicitly deferred residual classes.
6. **Gates:** `cargo test --lib`, targeted integration + `self_host_bootstrap_fixed_point` after each track; full `cargo test --test integration` at wave close; ASan on new materialize fixtures; re-measure clone baseline optional but preferred after D2/`for &` (low-clone goal).

---

## Closed set (spec inventory)

### Materialize-on-write (Borrow / immutable context)

| Class | Spec | Status |
|-------|------|--------|
| Bare param `T x` mutates | design §3.2; spectest `cow_bare_param_materialize` | **Done** (corpus + spectest) |
| Bare local / bare alias | design §3.3–3.4; `cow_bare_assign_sever`, sever fixtures | **Done** |
| Projected mutation through bare root (`v[i].f=`, `o.f.push`, …) | G1 | **Done** |
| `&`-of-bare formation (`f(&t)`, `&s.field`) | G2 | **Done** |
| Named bare-recv `&self` mutator (non-generic) | R38 T-B | **Done** (generic residual H) |
| Bare `for x in coll` element mutate | design §3.1 | **Partial** (reads OK; value-elem materialize may lag) |
| **Plain `self` (not `&self`)** | **D2** | **Both wrong** (write-through today) |
| Untracked alias chains (`&x.slice()[i]` mut) | devbook/11:443–450 | **Open** (last G1/G2 convergence class) |
| Loop-carried bare-param lazy mat | matcluster #2 | **Open** (lazy substrate) |
| Match pattern bindings | prose 02 | Treat as Borrow; verify if any hole (scout wave 0) |

### Write-through (`&` / owned place)

| Class | Spec | Status |
|-------|------|--------|
| `&` param / `f(&x)` unique | C3; `cow_amp_owned_writethrough` | **Done** |
| `&self` method | design §4.5 | **Done** (generic residual H) |
| Owned place `v.field=`, `v[i]=` | collections | **Done** for common paths |
| Value-type `v[i].field=` (Vector) | R39-T1 | **Rust done; self-host open (B)** |
| **`for x in &coll` element** | design/book | **Both open (A)** — no corpus fixture |
| **Dict/Set `d[k].field=`** | same class as R39 | **Both likely open (C)** — verify first |
| Nested place under `&` (`&outer.inner…`) | snag #53 | **Both open (F)** |
| Compound through write-through place | `cow_amp_compound_writethrough` | **Done** for covered shapes |

**Not in this campaign (defer):**

- Local `auto r = &b` reopening (owner: OK later if exclusivity-safe; keep D10 reject until place-overlap solid).
- D12 drop-purity enforcement wave (adjacent, separate).
- Comprehension `for x in &a` empty (I) — sibling of A; fold into A track as desugar parity.

---

## Open gaps → tracks

| ID | Gap | Compilers | Primary files |
|----|-----|-----------|---------------|
| **A** | `for x in &coll` element write-through lost | Both | Rust `src/ir/lowering/stmts/for_loops.rs`; SH `lower_loops.gg` (+ field/index mut on loop var) |
| **B** | Self-host value `v[i].field=` write-through | Self-host | `lower_stmt.gg` `lower_place_base` / `lower_field_write` (~1514–1638); promote corpus after |
| **C** | Dict/Set `d[k].field=` write-through | Both | Rust `try_resolve_field_place` Index arm `src/ir/lowering/exprs/mod.rs:2540+` (Array-only today); SH place twin; fix double-eval type-only pre-check |
| **D** | Untracked alias chains materialize | Both | `resolve_projection_root_local` `exprs/mod.rs:2374+`; SH `cow_source_root_name` / `cow_materialize_projected_root` |
| **E** | D2 plain-`self` materialize | Both | Receiver ABI + `cow_before_mutation` for bare self; SH equip path; migration sweep + DeadBareParamWrite extends to `self` |
| **F** | Snag #53 nested `&` field write-through | Both | Nested field place under MutPtr base; fixture `known_gaps/snag53_*` |
| **G** | Loop-carried bare-param materialize | Both | Lazy loop-carried substrate (`emit_lazy_loopcarried_borrow` / bare-param branch) |
| **H** | Generic-equip bare named-recv materialize | Self-host residual (Core #8 class) | `compute_method_mutates_self` generic equip |
| **I** | Comprehension over `&` iterable empty | Both | Desugar must match statement `for` |

---

## Recommended approach (waves)

### Wave 0 — Verify live symptoms (1 short scout, read-only + measure)

**Do not trust dated TODO numbers.** For each of A, B, C, E, F run compile+run on both compilers (and self-host lowerer for B):

| Probe | Expected if still broken |
|-------|--------------------------|
| A: `for c in &a: c.n += 100; print(a[0].n)` | `1` not `101` |
| B: self-host `v[0].x = 88` | stale `10` |
| C: `Dict[int, Point]; d[0].x = 99; print(d[0].x)` | stale / wrong |
| E: bare-`self` push; print caller | write-through (caller changed) |
| F: snag53 fixture | empty / no-op |

Deliverable: `/tmp/cow_wave0_measure.log` with stdout + exit codes. Re-order wave 1 if any already fixed.

Also inventory match-pattern bare binds for materialize holes (quick grep + 1 probe).

### Wave 1 — Place write-through class (lvalue completeness)

**Principle:** one **place-resolution** model: field/index stores go through addresses (`get_ptr` / MutPtr), never value copies. Fix the class, not one collection kind.

#### Track 1B — Self-host value index-element field write-through (first)

- **Why first:** Rust already correct; unlocks corpus fixture; pure SH place path; unblocks parity.
- **Fix shape:** mirror R39-T1 — when field-store base is `EIndex` on Array, force element **Ptr** (`gorget_array_get_ptr` / existing SH equivalent), write field through it, **untrack** transient element CoW handle (same class as Rust `untrack_transient_element_refs_in_range`).
- **Root cause (scout):** `lower_place_base` only special-cases Identifier/static/Deref; Index falls to `lower_expr` → **value copy** → field write dies (`lower_stmt.gg:1514–1638`).
- **Files:** `tests/fixtures/self_host_lowerer/lower_stmt.gg` (primary); possibly `lower_expr.gg` if index helpers live there. Serialize vs other SH tracks.
- **Fixtures:** promote `rust_value_index_element_field_writethrough` body to `tests/fixtures/cow_value_index_field_writethrough.gg` + expected; keep Rust inline or delete once corpus covers; both C + LLVM + self-host runtime.
- **Gates:** rebuild SH driver; `self_host_runtime` / targeted run; ASan on multilevel index+field fixtures already in corpus.

#### Track 1C — Dict/Set element field lvalue (both)

- **Verify** wave 0 first (Core #8 candidate).
- **Rust:** extend `try_resolve_field_place` `Expr::Index` beyond `CollectionKind::Array` — Map/Set via `gorget_map_get_ptr`-style + key typing; **type-only pre-check before `lower_expr(coll)`** to kill double-eval of side-effecting coll producers.
- **Self-host:** same place-base class as 1B (index/dict key path).
- **Fixtures:** `cow_dict_value_field_writethrough.gg` (+ Set if distinct); compound/nested optional.
- **Siblings:** grep all place-resolution sites (assigns, methods, compound) — Core #4.

#### Track 1A — `for x in &coll` write-through (both)

- **Root cause:** for-lowering binds elements without a **mode bit** (bare = Borrow / materialize-on-write; `&` = MutPtr write-through into collection element).
- **Rust:** `for_loops.rs` — detect `Expr::Unary`/`MutableBorrow` (or AST equivalent) on iterable; for Vector, bind loop var as **element place** (Ptr into slot), not value copy; field/index assigns on loop var must use that place. Dict/Set for-`&` if language allows (confirm grammar).
- **Self-host:** `lower_loops.gg` `lower_for_vector` (~224–285) currently always `borrow_only=true` payload read — branch on `&`.
- **Fixtures (must land with fix):**
  - `cow_for_amp_vector_field_writethrough.gg` → `101` (owned root)
  - `cow_for_amp_vector_alias_root.gg` → both aliases see write (or per CoW: if `b=a` bare alias then `for c in &b` — exclusivity/CoW interaction; wave 0 measure)
  - bare control: `for c in a: c.n += 100` → materialize, `a[0].n` unchanged
- **Follow-on in same track or immediate follow-up:** **I** comprehension desugar for `&` iterable (must not yield empty).
- **Smith:** remove exclusion in `tests/smith/generator.rs:32–33` after green.
- **Spectest seed** when ggdef can express (optional in wave 1; at least corpus).

**Wave 1 exit:** B + C + A green on both compilers; corpus fixtures committed; no ASan UAF on untrack.

### Wave 2 — Materialize completeness (immutable-context totality)

#### Track 2E — D2 plain-`self` materialize (both)

- **Spec:** bare `self` ≡ bare param: write materializes private copy; caller untouched; `&self` write-through.
- **Today:** both write through (TODO ~1109; decisions D2).
- **Fix:** route bare-self mutation through same materialize chokepoint as bare param (`cow_before_mutation` / SH equivalent); do **not** change `&self`.
- **Migration:** DeadBareParamWrite (or self-arm) + sweep self-host / fixtures that relied on bare-self write-through → `&self`. Scout blast radius first (owner rule for D12-like tracks).
- **Fixtures:** positive materialize (caller unchanged); `&self` still write-through; method that only reads bare self (no clone).

#### Track 2D — Untracked alias chains

- Extend `resolve_projection_root_local` (and SH twin) to name roots from view-returning methods / temps where sound, **or** materialize at the immutable link when root unnameable (prefer uniform chokepoint).
- Fixture: mutate through `&x.slice()[i]` or equivalent; owner buffer unchanged; no UAF.
- Remove devbook/11 “one remaining unconverged shape” when done.

#### Track 2F — Snag #53 nested write-through

- Nested field under `&` outer must remain MutPtr place chain.
- Un-`#[ignore]` `snag53_nested_struct_field_mut`; promote out of known_gaps when green.

#### Track 2G / 2H (rolling)

- **G** loop-carried bare-param: separate brief; wire expected `1,2,3,1` only after fix (do not lock wrong output).
- **H** generic-equip mutator: extend `compute_method_mutates_self` to generic equips; un-ignore `generic_equip_mutator_named_recv`.

### Wave 3 — Spec lock + docs + guards

1. Spectests for A, C, E (ggdef expectations via `ggdef -- gen` when elaborator covers).
2. Docs write-through: language-design §3.1 examples; book/11 for-`&`; devbook/11 implementation status (remove converging marker).
3. Structural guards (Core #6):
   - arm-count / place-resolver: Index place path must handle Array **and** Map (lint on `CollectionKind` exhaustiveness or sibling-site count).
   - optional: smith tier for `for x in &coll` field mut.
4. Clone baseline: `scripts/self_host_mem_baseline.sh` + `--clones=stats` after wave 1–2 (ensure free-view / low-clone goal not regressed by over-eager materialize).

---

## Process (non-negotiable)

Per `Agents.md`:

```
scout (measure e2e) → brief → ≥3 sequential fresh brief-reviews → execute (worktree) → fresh output-review → integrate
```

- **Always** `isolation: "worktree"`; stage by explicit file names; no stash.
- Parent runs full integration at wave close; agents run lib + targeted + bootstrap.
- Core #8: both-wrong is **not** a pass; fixtures encode **intended** stdout.
- Do **not** resurrect the abandoned reject-gate model for bare mutation (TODO.md:22).
- Fix **classes** (place-resolution, for-mode bit), not one call site.

### File-zone serialization (self-host)

| Zone | Tracks |
|------|--------|
| `lower_stmt.gg` place/field/index | 1B, 1C-SH, 2F |
| `lower_loops.gg` | 1A, 2G |
| `lower_expr.gg` / `lower.gg` CoW helpers | 1A-SH, 2D, 2E, 2H |
| Rust `for_loops.rs` | 1A-Rust |
| Rust `exprs/mod.rs` + `assigns.rs` + `methods.rs` | 1C-Rust, 2D-Rust, 2E-Rust |

Prefer: **1B → 1C → 1A** on SH (shared place helpers), with Rust 1C parallelizable after wave 0.

---

## Critical code map (reuse)

| Role | Path |
|------|------|
| Field place + Index Ptr force (Rust R39) | `src/ir/lowering/exprs/mod.rs` `try_resolve_field_place` ~2472–2582 |
| Assign untrack + field store | `src/ir/lowering/stmts/assigns.rs` |
| Method value-field recv | `src/ir/lowering/exprs/methods.rs` ~2010+ |
| `cow_before_mutation` | `src/ir/lowering/context.rs` ~2658+ |
| Projection root | `src/ir/lowering/exprs/mod.rs` `resolve_projection_root_local` ~2374+ |
| For-loops | `src/ir/lowering/stmts/for_loops.rs` |
| SH place base / field write | `tests/fixtures/self_host_lowerer/lower_stmt.gg` ~1514–1638 |
| SH for vector | `tests/fixtures/self_host_lowerer/lower_loops.gg` ~224–285 |
| SH projected materialize | `lower.gg` `cow_materialize_projected_root` ~758+ |
| Rust inline regression (B) | `tests/integration.rs` ~21080–21179 |
| Smith exclusion for for-`&` | `tests/smith/generator.rs` ~32–33 |
| Spec | `docs/language-design.md` §3.1; `docs/devbook/11-copy-on-write.md` §mutation; decisions D1/D2 |
| Prose stubs | `spec/prose/02-borrow-and-materialize-on-write.md`, `03-writethrough-and-move.md` |

---

## Verification checklist (per track)

```bash
# Wave 0 probes (example)
cargo build --release
# hand-run minimal .gg via target/release/gg run /tmp/probe.gg  (C + LLVM)
# self-host: rebuild lowerer driver then run probe

# Per-track
cargo test --lib
cargo test --test integration -- <fixture_filter> --test-threads=4 2>&1 | tee /tmp/cow-$RANDOM.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture

# After materialize/untrack changes
# ASan on new + multilevel cow_index_* fixtures

# Wave close
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/integration-$RANDOM.log
# optional: GG_BACKEND=llvm for parity sample of new fixtures
```

Clone/RSS (wave 3): `scripts/self_host_mem_baseline.sh` + compare to pre-wave capture in `/tmp`.

---

## Out of scope (this excellence slice)

- D10 place-overlap / local `&`-bind reopening (exclusivity package).
- D12 drop-purity straight-to-error (can follow; different diagnostic axis).
- Perf clone burn-down as primary goal (only **no regression** here; free views preserved).
- Async/shared CoW edges unless they fall out of A–F.
- ggdef elaboration of full closed set if elaborator subset blocks — corpus first, spectest when ready.

---

## Suggested execution order (summary)

```
Wave 0  measure A,B,C,E,F (+ match bind spot-check)
   ↓
Wave 1  1B self-host index field WT
        1C Dict/Set field WT (both) + double-eval fix
        1A for-in-& WT (both) + bare-for materialize control + comprehension I
   ↓
Wave 2  2E D2 plain-self
        2D untracked alias chains
        2F snag53
        2G / 2H rolling
   ↓
Wave 3  spectests, docs, lints, clone baseline, remove converging markers
```

**First concrete PR after wave 0:** Track **1B** (self-host value index-element field write-through + corpus promote).

---

## Risks

| Risk | Mitigation |
|------|------------|
| Untrack miss → UAF after materialize | ASan on all new mat fixtures; reuse G1 untrack pattern |
| Over-materialize (clone bombs) | Owned `&` path must remain write-through; measure clones after 1A/2E |
| SH/Rust drift | Corpus fixtures both compilers; promote only when MATCH |
| File-zone collisions | Serialize SH lower_*.gg tracks; worktrees |
| D2 migration blast | Scout blast radius; lint; fix-it to `&self` |
| Dict double-eval | Type-only pre-check before lower_expr |

---

## Success metrics

- Zero open rows in wave-1 table (A,B,C) for known wrong stdout.
- Wave-2 E/D/F closed or explicitly deferred with `#[ignore]` fixtures that encode **correct** expected output (never lock wrong).
- `devbook/11` no longer claims “one remaining unconverged shape” without naming only deferred items.
- Excellence table row **“Ownership model as fully implemented”** moves from Incomplete → Excellent for the closed set.
