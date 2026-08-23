# CODEBOOK — producing mechanism (assign exactly ONE term per item)

Classify by the PRODUCING MECHANISM — the reason the defect could exist at all — NOT by
area and NOT by symptom. "Double free" is a symptom; "an ownership fact re-derived at each
consuming position instead of carried" is a mechanism.

- **M1 LANE REPLICATION** — the same semantic rule exists as N hand-written implementations
  (Rust `gg`, the self-host compiler, the `ggdef` oracle, C backend vs LLVM backend, the 3
  driver-embedded lexer/parser copies). The item exists BECAUSE a rule must be hand-ported and
  the port lags or diverges. Includes "SH lane gap", "ggdef subset gap", "ggdef lags", "backend
  divergence", "port X to the self-host".
- **M2 UNCHOKED SIBLING SITES** — one decision is spelled at N sibling sites *within one lane*
  (emitters, lowering arms, registries, consume positions, parallel/duplicated functions) with
  no producer chokepoint. "The check exists and is not called at site N." Includes parallel
  registries/duplicated logic that are NOT keyed on a name.
- **M3 NAME- / SHAPE- / SENTINEL-MATCHED SEMANTICS** — a semantic decision reads an identifier
  string, a mangled-name slice, a substring/prefix predicate, a hand-synced name allow-list, or
  a syntactic-shape lookahead heuristic, instead of typed metadata.
- **M4a SILENT-DROP ARM** — a lowering/parse/emit arm discards user syntax and continues:
  `_ => {}`, a bare `return`, a blanket error suppression, a dropped named argument.
- **M4b HAND-WRITTEN ANALYSIS MEMBER SET** — a walker / checker / analysis pass enumerates the
  node kinds (or call shapes, or recursion cases) it handles by hand, so it silently
  under-approximates. Includes heuristics standing in for a derived analysis.
- **M4c ACCEPT-WITHOUT-LOWER** — `gg check` accepts a program no backend can emit; it dies at
  C-compile, at link, or as an ICE. (Use M4a instead when the result is a SILENT wrong answer.)
- **M5 INVARIANT DROPPED AT A LAYER BOUNDARY** — an upstream pass computed a fact (element type,
  ownership, span, reason, ABI, signature) and did not write it into the next layer's typed
  metadata, so the downstream re-derives, guesses, or defaults.
- **M6 ONE REPRESENTATION FOR TWO QUESTIONS** — a single bit / type / slot / sentinel conflates
  two independent axes, so every reader is wrong on one of them (e.g. an `i64` return meaning
  both "i64" and "unknown"; `&` and `!` params both tagged `Borrowed`).
- **M7 TYPED-AXIS PARTIAL COVERAGE** — a feature is implemented and fixtured for ONE cell of a
  typed axis (element type, receiver shape, wrapper kind, position) and is absent or broken for
  the other cells.
- **M8 OWNERSHIP NOT REGISTERED AT THE PRODUCER** — a freshly materialized owned value is not
  drop-registered (leak), is registered twice (double free), or a borrow is tagged owned (UAF).
- **M9 MISSING REJECTION RULE** — an ill-formed or unsound program is accepted because no rule
  exists at all (not because an arm is missing). Acceptance is the default.
- **M10 RATIFIED-BUT-UNIMPLEMENTED** — a ledger/owner decision (a D-number, an E-track, an
  A38-x, "ratified … not built") whose implementation IS the item.
- **M11 OPEN DESIGN QUESTION / OWNER KNOB** — no defect; a decision is owed.
- **M12 DOC <-> CODE DIVERGENCE** — docs/book/reference/ledger assert behaviour the compiler
  does not have (or cite stale lines/figures); nothing executes the prose.
- **M13 GUARD OR FIXTURE THAT CANNOT FAIL** — an always-pass diagnostic, a floorless comparison,
  a non-discriminating fixture, an allowlist blind to its own class, a gate that never runs, an
  instrument vacuous on some lane, a missing measurement.
- **M14 HARNESS / INSTRUMENT LIMITATION** — shared fixed scratch paths, torn shared state,
  platform-missing sanitizer, flakes, sweep wall time, an oracle's engineering limits.
- **M15 UNCONTRACTED CLONE / NO COST MODEL** — a clone/materialize where a borrow would do; a
  conservative analysis whose cost is unbounded; no cost contract or ceiling.
- **M16 PLAIN UNBUILT SCOPE** — a plainly missing capability, polish, or refactor. No generator.
- **M17 RECORD / COMMENT / DEAD-CODE HYGIENE** — the item is about the record itself: a census
  living only in /tmp, a stale or false comment, a wrong citation, dead code, a planning note.

Rules:
- Pick the SINGLE best primary term. If two fit, pick the one that explains why the defect could
  exist at all, not the one that describes the surface.
- If the bullet is a container of several heterogeneous sub-items, pick the DOMINANT mechanism.
