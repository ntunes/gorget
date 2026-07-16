# Excellence plan: finish CoW write-through + materialize closed set

> **Status:** v3 — **WAVE 0 COMPLETE (2026-07-16)**: report `docs/plans/cow-wave0-measure.md`,
> raw log `docs/plans/artifacts/cow_wave0_measure.log`. All gaps CONFIRMED live (A/A-alias/B/C/E/F
> broken; C≡LLVM on every probe — all gaps are shared-GIR); **wave-1 order 1B → 1C → 1A(+I)
> UNCHANGED**. A-alias expectation LOCKED by derivation + measurement: `a=1, b=101` (materialize
> AT the bare alias; today the write is lost entirely, b=1). **Drop-taint × materialize:
> MEASURED — the bare-param taint gate is ALREADY LIVE on all three judges (reject
> E_MoveWithoutOperator); ggdef already rejects drop-tainted plain-`self`; production
> writes through AND skips the gate (worse-than-filed). Disposition "reject" recorded
> (owner ratification pending — see conversation 2026-07-16); 2E MUST route drop-tainted
> receivers through the existing reject or it converts write-through into silent
> clone→double-drop.** Discovery filed to TODO High: struct-value match patterns mis-bind
> (rotated-by-one + arity unchecked, Core-#8). v2 folded reviewer reservations
> (alias-root derivation, ggdef-as-oracle, drop-taint, gate/scoreboard bar).
> **1B EXECUTED 2026-07-16 (output-review in flight):** ⚠ ARCHITECTURE CORRECTION from
> execution — the wave-1 "one place-resolution model in the SHARED `lower_place_base`" premise
> is FALSE for the self-host: growing the shared resolver a pointer arm breaks value-struct
> `.clone()` method receivers (3 regex fixtures regressed, self-caught + reverted). The proven
> shape mirrors Rust's actual scoping: a WRITE-ONLY `lower_field_place_base` producer (called
> by both field-write sites), shared resolver untouched. **Tracks 1C and 2F extend
> `lower_field_place_base`, NOT `lower_place_base`.** New closed-set discovery filed HIGH:
> bare `v[i].method()` mutating-receiver write-through is broken on BOTH compilers (the
> method-receiver analog of gap B — needs a row + wave assignment when its track is scoped).

## Context

Gorget’s core ownership rule (§3.1 / D1 / devbook/11):

> A resource is a **borrow** until a write. If the write reaches a real owner through an unbroken **`&` chain**, it **writes through**. If the chain hits an **immutable binding**, the write **materializes** a private copy *there*.

G1 (projected mutation materialize) and G2 (`&`-of-bare formation materialize) landed. The **uniform rule is still incomplete** at several closed-set positions. Incomplete positions are silent wrong-output bugs (often Core #8: both compilers agree on wrong), not design forks.

**Excellence north star for this campaign:** every position in the closed set behaves per §3.1 on **Rust gg + self-host**, with fixture expectations **sourced from ggdef (the definition) when the shape is in-subset**, ASan clean on materialize/untrack paths, **WRONG-OUTPUT parity count drops** at each wave close, and no clone-stats regression on self-host self-compile beyond noise.

Related owner notes: local `&`-binds may reopen later only if exclusivity-safe (out of v1 burn-down until D10 place-overlap is solid); free views / low cloning remain production goals (D15 refinement, not eager clone everywhere).

---

## Standing rules for this campaign

1. **Expectations flow FROM the definition (ggdef), not from production measurement.**  
   Measuring compilers tells you what they *do*; if they write through where the chain is broken, pinning that locks Core #8 wrong-output. Wave 0 and every new fixture: run the probe under **ggdef** (when elaborable); expected stdout = ggdef verdict. If the shape is **outside ggdef’s subset**, state that explicitly and derive from §3.1 / D1 / prose 02–03 (never “whatever production printed”).
2. **Derive before measure for multi-hop alias chains** (see fixture `cow_for_amp_vector_alias_root` below).
3. **Materialize is an implicit copy** — drop-taint (D4/D12) applies; see closed-set row + **queued decision before 2E**.
4. Process: per **`CLAUDE.md` / `AGENTS.md`** (same symlink): scout → brief → ≥3 sequential fresh brief-reviews → worktree execute → output-review → integrate. Plan itself needs a **clean fresh review pass** after this v2 fold before the first track brief.

---

## Definition of done

1. **Closed-set totality (runtime):** every position class below has ≥1 positive fixture whose expected output is **ggdef-adjudicated (or prose-derived with explicit out-of-subset note)** and that **both** compilers match (C **and** LLVM full sweeps at wave close).
2. **No known silent drop of write-through** on Vector / Dict / Set value-element field stores, `for x in &coll`, nested `&` field places (snag #53), or owned-index field places (self-host mirror).
3. **No known silent write-through where materialize is required** for tracked roots (G1/G2) **plus** D2 plain-`self` and untracked alias chains (or those deferred only with fixtures that encode **definition-correct** expected output — never lock wrong).
4. **Drop-taint × materialize** disposition is **owner-ruled** and implemented (reject vs clone-with-explicit — see queued decision); closed-set row is not latent.
5. **Promote** `rust_value_index_element_field_writethrough` from inline Rust-only to corpus once self-host matches.
6. **devbook/11** “converging” marker removed or reduced to only explicitly deferred residual classes.
7. **Gates (round-close bar):** lib + lints + targeted integration + bootstrap + **full C integration** + **full LLVM integration** + **`spec_conformance` + full ggdef suite** at wave close (and on any track that **flips fixture expectations**, e.g. 2E — Batch-A lesson). ASan on new materialize/untrack fixtures.
8. **Scoreboard:** regen `self_host_runtime_diff` / parity at **wave-1 and wave-2 close**; WRONG-OUTPUT (and related non-MATCH) **must drop** relative to pre-wave baseline (command in gates; **no dated numbers in this plan**).

---

## Closed set (spec inventory)

### Materialize-on-write (Borrow / immutable context)

| Class | Spec | Status |
|-------|------|--------|
| Bare param `T x` mutates | design §3.2; spectest `cow_bare_param_materialize` | **Done** (corpus + spectest) |
| Bare local / bare alias | design §3.3–3.4; `cow_bare_assign_sever`, severorder | **Done** |
| Projected mutation through bare root | G1 | **Done** |
| `&`-of-bare formation | G2 | **Done** |
| Named bare-recv `&self` mutator (non-generic) | R38 T-B | **Done** (generic residual H) |
| Bare `for x in coll` element mutate | design §3.1 | **Partial** |
| **Plain `self` (not `&self`)** | **D2** (`decisions.md` D2) | **Both wrong** (write-through today) |
| Untracked alias chains | devbook/11 | **Open** |
| Loop-carried bare-param lazy mat | matcluster #2 | **Open** |
| Match pattern bindings | prose 02 | Borrow; wave-0 probe |
| **Drop-tainted value at any materialize-on-write site** | D4 / D12 + **this campaign’s seventh position** | **Open — owner ruling required before 2E** (see below) |

### Write-through (`&` / owned place)

| Class | Spec | Status |
|-------|------|--------|
| `&` param / unique owned `&` | C3; `cow_amp_owned_writethrough` | **Done** |
| `&self` | design §4.5 | **Done** (generic residual H) |
| Owned place `v.field=`, `v[i]=` | collections | **Done** (common paths) |
| Value-type `v[i].field=` (Vector) | R39-T1 | **Rust done; self-host open (B)** |
| **`for x in &coll` element** | design/book | **Both open (A)** |
| **Dict/Set `d[k].field=`** | same class as R39 | **Both likely open (C)** |
| Nested place under `&` (snag #53) | known_gaps | **Both open (F)** |
| Compound through write-through place | `cow_amp_compound_writethrough` | **Done** (covered shapes) |

### Drop-taint × materialize (queued decision — blocks Track 2E)

**Fact:** materialize-on-write is an **implicit copy**. D12/D4: drop-tainted types must not be implicitly duplicated at the six positions (`E_MoveWithoutOperator`). Materialize is a **seventh** implicit-copy position not yet enumerated as such in the closed-set tables.

**Reviewer derivation (recommended default for the owner brief):** **reject** bare mutation of a drop-tainted binding (same family as the six positions): user writes `&self` / `&param` (write-through, no clone of the resource identity in the CoW sense for taint) or explicit `.clone()` / `!` as appropriate — never silent double-drop of custom `Drop`.

**This plan does not rule.** Before Track 2E executes:

1. Wave-0 probe: bare param **and** bare `self` of a **custom-Drop** type; mutate; record **ggdef + both compilers** (stdout / diagnostic / crash).
2. Owner decision item (ledger or TODO): **reject at materialize sites for drop-tainted** vs other disposition.
3. Implement disposition + negative fixtures **with** 2E (or as 2E.0 prerequisite track).

Until then, 2E must not land bare-self materialize for drop-tainted receivers as silent clones.

**Defer (not this campaign’s identity):**

- Local `auto r = &b` reopening (exclusivity-safe later).
- D12 full six-position enforcement wave (coordinate; drop-taint materialize decision **aligns** with D12).
- Async/shared edges unless they fall out of A–F.

---

## Derived fixture expectations (do not “measure” these)

### `for c in &a` on owned root → write-through

```
Vector[Cell] a = [Cell(1), …]
for c in &a:
    c.n = c.n + 100
print(a[0].n)   # EXPECT 101
```

Chain: `c` —`&`→ element of owned `a`. Unbroken `&` to owner → write-through. Confirm with **ggdef** in wave 0.

### `cow_for_amp_vector_alias_root` — **derived, not measured**

```
Vector[Cell] a = [Cell(1), …]
Vector[Cell] b = a          # bare bind → Borrow alias of a
for c in &b:
    c.n = c.n + 100
print(a[0].n)               # EXPECT 1  — chain breaks at bare b
print(b[0].n)               # EXPECT 101 — materialize at b, then write-through into b’s private copy
```

**Derivation:** `b = a` is a bare binding (borrow). Write chain: `c` —`&`→ `b` —**bare**→ `a`. The unbroken-`&` chain ends at `b`; per §3.1 the write **materializes at `b`**. `a` is untouched. This is the same sever semantics as `cow_bare_assign_sever` / severorder with a loop in the middle — **not** “both aliases see write.”

**Do not** set expected output from production if production still write-throughs to `a` (that would lock Core #8). Wave 0 **records** production/ggdef deltas; expected stdout for the fixture is the **derivation + ggdef** above.

### Bare `for c in a` control → materialize

```
for c in a:          # bare element = Borrow
    c.n = c.n + 100
print(a[0].n)        # EXPECT 1 (private copy in c)
```

---

## Open gaps → tracks

| ID | Gap | Compilers | Primary files |
|----|-----|-----------|---------------|
| **A** | `for x in &coll` element write-through | Both | Rust `for_loops.rs`; SH `lower_loops.gg` |
| **B** | Self-host value `v[i].field=` | Self-host | `lower_stmt.gg` `lower_place_base` / `lower_field_write` ~1514–1638 |
| **C** | Dict/Set `d[k].field=` | Both | Rust `try_resolve_field_place` Index arm `exprs/mod.rs:2540+`; SH place twin; type-only pre-check before `lower_expr(coll)` |
| **D** | Untracked alias chains materialize | Both | `resolve_projection_root_local` `exprs/mod.rs:2374+`; SH CoW root helpers |
| **E** | D2 plain-`self` materialize | Both | Receiver + `cow_before_mutation` `context.rs:3325`; SH equip; **blocked on drop-taint ruling** |
| **F** | Snag #53 nested `&` field WT | Both | Nested MutPtr place; `known_gaps/snag53_*` |
| **G** | Loop-carried bare-param mat | Both | Lazy loop-carried substrate |
| **H** | Generic-equip bare named-recv mat | SH residual | `compute_method_mutates_self` generics |
| **I** | Comprehension over `&` iterable empty | Both | **Same iterable-mode helper as A** (not a second fix) |
| **T** | Drop-taint × materialize | Both + ggdef | Owner decision + reject/negative fixtures; prerequisite to 2E |

---

## Waves

### Wave 0 — Verify + adjudicate (ggdef-first)

For each probe (A, B, C, E, F, **T drop-taint**, alias-root derivation, bare-for control, match-bind spot-check):

| Lane | What to record |
|------|----------------|
| **ggdef** | elaborate+run (or ElabError / out-of-subset note) → **authoritative expected** when Value |
| Rust C / LLVM | stdout, exit, trap line |
| Self-host lowerer | where applicable (B) |

Deliverable: `/tmp/cow_wave0_measure.log` with **three-way** comparison and explicit **expected = ggdef | prose-derived (out of subset)**.

Re-order wave 1 only if a gap is already fixed **and** matches ggdef/derivation.

Also: capture **parity baseline** command output (WRONG count) for wave-1 scoreboard (record command + session regen only — no stale numbers in plan).

### Wave 1 — Place write-through class

**Principle:** one place-resolution model (addresses / `get_ptr` / MutPtr), not per-collection special cases. Expectations from ggdef/derivation from day one; spectest **lane** wiring may wait for elaborator (wave 3) but **stdout expectations do not**.

#### Track 1B — Self-host value index-element field write-through (**first**)

- Mirror R39-T1: Index base of field-store → force element Ptr + untrack transient CoW handle.
- Root: `lower_place_base` falls through Index to value `lower_expr` (`lower_stmt.gg:1514–1638`).
- Promote inline test body (`integration.rs` ~21089) → `tests/fixtures/cow_value_index_field_writethrough.gg` with ggdef/derived expected.
- Gates: SH driver rebuild; targeted integration; ASan multilevel cow_index_*; **ggdef suite** if expectations added to spectests later — corpus expected still ggdef-checked in wave 0.

#### Track 1C — Dict/Set element field lvalue (both)

- Wave-0 verify first.
- Extend `try_resolve_field_place` past `CollectionKind::Array`; type-only pre-check before lowering collection (no double-eval).
- Fixtures with ggdef/derived expected; sibling-site grep (assigns/methods/compound).

#### Track 1A — `for x in &coll` write-through (both) **+ I (comprehension)**

- Mode bit on iterable: bare = Borrow element; `&` = MutPtr element place.
- **Core #4:** one **shared iterable-mode helper** (or shared desugar) feeding **both** statement-`for` and **comprehension** lowering — not two parallel fixes. Absorb TODO-High “comprehension over `&` yields empty” (A3 gauntlet residual) into this track; reconcile TODO on land (move completed note to DONE, leave only residuals).
- Fixtures (expected = derivation + ggdef):
  - `cow_for_amp_vector_field_writethrough.gg` → `101`
  - `cow_for_amp_vector_alias_root.gg` → `1` then `101` (see derivation above)
  - bare-for control → `1`
  - comprehension twin once helper exists
- Un-exclude smith (`generator.rs:32–33`) after green.

**Wave 1 close gates + scoreboard:** full C integration, full LLVM integration, lib, lints, bootstrap, **spec_conformance + full ggdef suite**, ASan sample, **parity regen — expect WRONG drop**.

### Wave 2 — Materialize completeness

#### Track 2T / decision — drop-taint × materialize (**before 2E**)

- Owner ruling + implement reject (recommended) or alternate; negative fixtures; align messaging with D12 family.

#### Track 2E — D2 plain-`self` materialize (both)

- Only after 2T disposition is clear for drop-tainted receivers.
- Bare `self` ≡ bare param materialize; `&self` write-through.
- Migration: DeadBareParamWrite/`self` arm + sweep bare-self write-through → `&self`.
- **Any fixture expectation flip ⇒ full ggdef suite in track gates** (Batch-A).

#### Track 2D — Untracked alias chains

- Extend root oracle or materialize at immutable link; fixture; remove devbook converging marker when done.

#### Track 2F — Snag #53

- Nested MutPtr place chain; un-ignore when green.

#### Track 2G / 2H (rolling)

- Loop-carried bare-param; generic-equip mutator classification.

**Wave 2 close:** same gate battery as wave 1 + **parity regen — expect further WRONG drop**.

### Wave 3 — Spec lock + docs + guards

1. Spectest **lane** wiring for A/C/E/T when elaborator covers (expectations already from ggdef day one).
2. Docs: language-design §3.1, book/11 for-`&` + alias-root, devbook/11 status, D4/D12 seventh position if ruled.
3. Lints: place-resolver CollectionKind exhaustiveness / sibling counts; optional smith for-`&`.
4. Clone baseline: `scripts/self_host_mem_baseline.sh` + `--clones=stats` (no regression).

---

## Process

```
fresh plan review (clean pass on this v2) →
per track: scout (e2e + ggdef) → brief → ≥3 sequential reviews → worktree execute → output-review → integrate
```

- Always `isolation: "worktree"`; explicit `git add` paths; no stash.
- Parent: full C + LLVM integration at wave close; agents: lib + targeted + bootstrap + **ggdef/spec_conformance when expectations flip**.
- Core #8: both-wrong is not a pass.
- Do not resurrect reject-gate for **ordinary** bare mutation (TODO / CLAUDE.md); **do** reject drop-tainted materialize if owner rules that way (orthogonal: taint purity, not “bare always reject”).

### Coordination with enforcement wave (main)

| Overlap | Note |
|---------|------|
| `src/ir/lowering/*`, `lower_*.gg` | Shared with exclusivity / drop-purity / trap tracks — **serialize** or rebase often |
| **Batch C3** composed `gg fmt` / D27 sigil sweep (~1,114 move sites) | **Catastrophic conflict** with long-lived CoW branches. **Strategy:** land CoW waves **before** C3, **or** rebase CoW branch onto post-C3 main immediately after C3 lands (no multi-week dual branch). State choice at campaign kickoff. |

### File-zone serialization (self-host)

| Zone | Tracks |
|------|--------|
| `lower_stmt.gg` place/field/index | 1B, 1C-SH, 2F |
| `lower_loops.gg` + comprehension desugar | 1A, **I**, 2G |
| `lower_expr.gg` / `lower.gg` CoW | 1A-SH helpers, 2D, 2E, 2H, 2T |
| Rust `for_loops.rs` + comprehension | 1A-Rust, I-Rust |
| Rust `exprs/mod.rs`, `assigns.rs`, `methods.rs`, `context.rs` | 1C-Rust, 2D, 2E, 2T |

Prefer **1B → 1C → 1A(+I)** on SH; Rust 1C parallelizable after wave 0 if zones held.

---

## Critical code map (verified anchors)

| Role | Path |
|------|------|
| Field place + Index Ptr (R39) | `src/ir/lowering/exprs/mod.rs` `try_resolve_field_place` **:2472** |
| Projection root | `…/exprs/mod.rs` `resolve_projection_root_local` **:2374** |
| `cow_before_mutation` | `src/ir/lowering/context.rs` **:3325** (not ~2658 — stale) |
| Assign untrack / field store | `src/ir/lowering/stmts/assigns.rs` |
| For-loops | `src/ir/lowering/stmts/for_loops.rs` |
| SH place / field write | `self_host_lowerer/lower_stmt.gg` **:1514–1638** |
| SH for vector | `self_host_lowerer/lower_loops.gg` **:224+** |
| Rust inline (B) | `tests/integration.rs` **~21089** |
| Smith for-`&` exclusion | `tests/smith/generator.rs` **:32** |
| D2 | `docs/plans/define-gorget/decisions.md` D2 (~:79) |
| Spec / prose | language-design §3.1; devbook/11; `spec/prose/02`, `03` |
| ggdef | `spec/ggdef/` — oracle for in-subset CoW |

---

## Verification / gates

### Per-track (minimum)

```bash
cargo test --lib
cargo test --test lints   # when touching structural/lint sites
cargo test --test integration -- <filter> --test-threads=4 2>&1 | tee /tmp/cow-$RANDOM.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point -- --nocapture
# If track adds/flips expected stdout or spectests:
cargo test -p ggdef -- --test-threads=4 2>&1 | tee /tmp/ggdef-$RANDOM.log
cargo test --test spec_conformance -- --test-threads=4 2>&1 | tee /tmp/spec-$RANDOM.log
```

### Wave close (mandatory — not optional LLVM sample)

```bash
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/integration-c-$RANDOM.log
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release -- --test-threads=4 2>&1 | tee /tmp/integration-llvm-$RANDOM.log
cargo test -p ggdef -- --test-threads=4 2>&1 | tee /tmp/ggdef-wave-$RANDOM.log
cargo test --test spec_conformance -- --test-threads=4 2>&1 | tee /tmp/spec-wave-$RANDOM.log
# Scoreboard (regen; quote only this session’s numbers):
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture 2>&1 | tee /tmp/parity-$RANDOM.log
# Read PARITY = MATCH/(…) and WRONG-OUTPUT count — expect drop vs wave-0 baseline
```

ASan on new mat/untrack fixtures. Clone baseline after wave 1–2 preferred.

---

## Out of scope

- D10 local `&`-bind reopening (until place-overlap solid).
- Full D12 six-position burn-down (coordinate; 2T aligns).
- Perf clone burn-down as primary (no-regression only; free views preserved).
- Long dual-branch against Batch C3 without stated rebase plan.

---

## Success metrics

| Metric | Target |
|--------|--------|
| Wave-1 gaps A,B,C | Zero known wrong stdout vs ggdef/derived |
| Wave-2 E,D,F,T | Closed or deferred only with **definition-correct** expected fixtures |
| Drop-taint row | Owner-ruled + enforced |
| Parity WRONG | Drops at wave-1 and wave-2 close (session-regenerated) |
| devbook/11 | No stale “one remaining shape” without naming deferred only |
| Excellence table | “Ownership model as fully implemented” → Excellent for closed set |

---

## Execution order (summary)

```
v2 plan → fresh review clean pass
   ↓
Wave 0  ggdef+C+LLVM+SH probes; T drop-taint evidence; parity baseline; alias-root EXPECT locked by derivation
   ↓
Wave 1  1B → 1C → 1A(+I shared iterable-mode helper)
        close: full C + LLVM + ggdef + spec_conformance + parity (WRONG↓)
   ↓
Wave 2  2T owner ruling → 2E D2 → 2D → 2F → 2G/2H
        close: same battery + parity (WRONG↓)
   ↓
Wave 3  spectest lanes, docs, lints, clone baseline
```

**First PR after wave 0 + clean plan review:** Track **1B**.

---

## Risks

| Risk | Mitigation |
|------|------------|
| Pinning production wrong-output | ggdef/derivation first; never measure-as-expected for alias-root |
| Drop-taint silent double-drop via 2E | 2T blocks 2E; reject recommended |
| Untrack UAF | ASan; reuse G1 untrack |
| Over-materialize / clone bomb | Owned `&` stays WT; clone baseline |
| C3 / enforcement conflict | Land before C3 or rebase strategy at kickoff |
| SH/Rust drift | Corpus both compilers; ggdef oracle |
| Expectation flips break ggdef floors | Full ggdef suite in gates (Batch-A) |

---

## Review fold log (v1 → v2)

| # | Severity | Fold |
|---|----------|------|
| 1 | HIGH | Alias-root expected: `a=1`, `b=101` by §3.1 derivation (not “both see write” / not wave-0 measure) |
| 2 | HIGH | ggdef oracle from wave 0; expectations day-one; spectest lanes may wait wave 3 |
| 3 | HIGH | Drop-taint × materialize row + wave-0 probe + owner decision before 2E |
| 4 | MED | Gates: ggdef + spec_conformance; full C+LLVM at close; parity scoreboard WRONG↓ |
| — | small | `cow_before_mutation` → `context.rs:3325`; cite CLAUDE.md; 1A+I shared helper + TODO recon; C3 coordination |
