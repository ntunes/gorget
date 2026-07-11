# Enforcement-wave CENSUS scout — MANDATE (the wave's OPENING step; owner-directed 2026-07-11)

> **Purpose:** before ANY wave execution (D12/D10/…), ONE read-only scout measures
> the COMBINED migration blast radius of every pending breaking change across the
> four corpora and drafts the sequencing plan the owner ratifies once — so each
> corpus migrates per-batch, not serially per-decision. Origin: review-residuals
> pushback #2, accepted 2026-07-11 after D25-D28 tripled the pending-migration load.

## The pending changes to census (each: sites per corpus + which FILES overlap)

1. **D12** — drop-purity `E_MoveWithoutOperator` at the six implicit-copy positions
   (live drop-tainted values; the original blast-radius-scout mandate folds in here).
2. **D10** — exclusivity package: place-overlap rejection + local `&`-bind removal
   (both forms; bootstrap-gated).
3. **D15+D22** — slice surface: `T[]` removal + `.slice()` → `v[a:b]` migration.
4. **D19** — `break <value>` removal (expected ≈0 — verify).
5. **D25** (pending ratification) — fault-catch removal: the 31 feature fixtures
   (~10 → D26 positives, 2-3 → negatives per the A33 scout), the ~2,000-line
   both-compiler machinery deletion, the 8 cancelled tracks.
6. **D26+D28** — fallible operators `+! -! *! /! %! **!` + `**`/`**=` + pow()
   retirement + the xor-as-pow lint (additive surface + one tiny migration).
7. **D27** — the sigil migration: `!`→`^` at ~870 sites (fixtures 337, self-host
   365, gorget-js 36, arena 132 — re-verify the A33 census), diagnostics, docs.

Corpora: `tests/fixtures/` + spectests · self-host dirs · gorget-arena
(`target/gorget-arena/`) · gorget-js (`/workspace/gorget/.worktrees/gorget-js`,
READ-ONLY — same constraint as the A33 scout mandate).

## Deliverables

1. **The blast-radius matrix**: change × corpus → site count + file list; plus the
   OVERLAP analysis (which files are touched by ≥2 changes — those decide batching).
2. **The sequencing plan (draft for owner ratification)**: batch the syntax
   migrations onto shared `gg fmt` vehicles (D27 sigils + D22 `.slice()` + D28
   `pow()` are natural companions — verify no ordering hazard between them);
   order the rejection-tightening changes (D12, D10, D25) so each corpus takes
   one migration pass per batch; state which batches are bootstrap-gated; pin
   the "surprises are REPORTS, not downgrades" discipline per batch.
3. **The ratification packet**: the census + sequencing plan is the natural moment
   for the owner to formally ratify D24 (boundary) / D25 (fault-catch removal) /
   D26 (fallible operators) from the A33 scout report
   (`scouts/scout-a33-fault-model.md`) — bundle the three option-questions with
   the plan so one review closes the batch.
4. TODO/DONE hygiene notes: which filed tracks the plan supersedes, merges, or
   re-sequences (e.g., the 8 fault-machinery tracks D25 cancels; the D12
   blast-radius scout this mandate absorbs).

## Constraints

Read-only everywhere (the A33 mandate's gorget-js rules apply verbatim); regenerate
every count this session — cite zero dated figures; measure by grep/read, build
nothing; deliverable = report + the matrix + the plan as option-questions.
