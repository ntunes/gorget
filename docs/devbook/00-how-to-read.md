# Chapter 0 — How to read this book

This chapter is the *contract* between this book and its readers: what the book
owns, what it defers to the code, how its claims are kept honest, and the
conventions every later chapter follows. It is a meta-chapter — it cites the
book's own scaffolding ([`README.md`](README.md)) rather than compiler
source. Read it once; the conventions it states are assumed everywhere else.

This is a **how-it-is-implemented** reference for *compiler developers*. It
assumes you read Rust and have met a compiler before (AST, pass, IR), but does
**not** assume you know Gorget's internals — that is what the book teaches,
frontend to backend ([`README.md:3-5`](README.md)). For the *language* (not its
compiler), three other corpora are authoritative and this book never restates
them: [The Gorget Book](../book/README.md) (how to *use* the language),
[`language-reference.md`](../language-reference.md) (the normative spec — *what*
the language is), and [`language-design.md`](../language-design.md) (the *why*).
The devbook's axis is the fourth: *how it is implemented*
(`devbook_plan.md:9` (devbook plan — git history)).

## The one-authoritative-source contract

This book owns exactly one thing: the **design narrative** — the architecture,
the invariants, and the *why*. It does **not** own *live behavioral facts*:
which function does what, exact ABI offsets, comparison scores, numeric budgets,
branch tables, `file:line`. For those, the **code and tests stay authoritative
and the book cites them — it never copies them**
([`README.md:11`](README.md), `devbook_plan.md:13` (devbook plan — git history)).

This is the same rule the compiler itself lives by, applied to documentation:
"one source of truth per axis" (CLAUDE.md layering-discipline rule 3). A fact
duplicated into prose is a fact that will drift; the only stable representation
of a live fact is a citation to the source that produces it.

The practical consequence for you as a reader:

> **If a number or a line reference in this book disagrees with the source, the
> source wins and the book is stale.** Report it.
> ([`README.md:11`](README.md))

And the practical rule for anyone *writing* a chapter: every load-bearing claim
must carry a `path:line` the author actually verified against current source.
Figures and line numbers are re-derived from the tree at authoring time — never
transcribed from an older doc, because those are presumed stale (see the fold
protocol below).

## Why `path:line` and not prose

Citing source instead of restating it buys two things. First, **it survives
refactors that prose cannot**: a `// See docs/devbook/16-bir.md#...` comment in
the source and a `path:line` cite in the book form a two-way link that a
reviewer can spot-check in seconds. Second, **it forces honesty**: an author who
must produce a `path:line` for a claim cannot quietly repeat a remembered figure
that has gone stale. The book is deliberately *cite-heavy and not padded*: short
IR/code excerpts with a `file:line` beat long descriptive prose, because the
excerpt is checkable and the prose is not.

The flip side is a maintenance duty: `path:line` cites *do* rot as the tree
moves. They are not meant to be eternally exact — they are meant to be *cheap to
re-verify*. That is what the freshness stamps (next) are for.

## Freshness: "verified against `<commit>`" stamps

Chapters that describe fast-moving internals carry a **"verified against
`<commit>`" stamp** ([`README.md:17`](README.md)). The stamp records the commit
at which every `path:line` and figure in the chapter was last re-derived from
source. It is an honesty signal, not a guarantee of current correctness:

- A **stamped** chapter's cites were correct *as of that commit*. If the tree
  has moved since, individual line numbers may have drifted — but the design
  narrative around them is intended to remain valid, and a cite that no longer
  resolves is a re-verify task, not a contradiction.
- An **un-stamped fast-moving claim is suspect**: treat it as unverified and
  re-check it against source before relying on it
  ([`README.md:17`](README.md)).

This mirrors the codebase's own discipline (CLAUDE.md: "re-verify a premise
against CURRENT source/tests before acting on it"). A dated figure — in this
book, in an internals doc, or in a memory note — is dated the moment it is
written. The stamp tells you *when* the re-derivation last happened so you know
how much to trust it.

## The fold protocol

This book was **grown by folding** the `internals/` deep-dives
into chapters, repointing their source citations at each chapter's stable
anchors, and then deleting the absorbed doc. What survives in `docs/internals/`
is designed-but-unbuilt material only — a chapter here describes what the
compiler does today, a design note there describes what has been ruled but not
yet built
([`README.md:19`](README.md),
`devbook_plan.md:15` (devbook plan — git history)). Folding was **lazy**: no
internals file was moved or deleted, and no source comment repointed, until the
chapter that absorbed it was actually written
(`devbook_plan.md:17` (devbook plan — git history)).

When a chapter folds in an internals doc, each piece of that doc's content is
classified into one of **four dispositions**
(`devbook_plan.md:23-27` (devbook plan — git history)):

| Disposition | Content | Action |
|-------------|---------|--------|
| **(a) evergreen** | architecture / rationale | **lift** into the chapter, re-derived from current source (cite, never copy) |
| **(b) live roadmap** | not-yet-shipped plans | route to `TODO.md`; the chapter only *notes the item exists* |
| **(c) dead status** | obsolete state | **drop** |
| **(d) IMPL-AHEAD stale status** | now-shipped work the doc still describes in false present/future tense | lift only the evergreen *rationale*; re-derive the *actual status* from source |

The decisive rule is that **disposition (d) applies to every folded doc by
default** — it is *not* gated on a hand-maintained "which docs are stale" list
(`devbook_plan.md:29` (devbook plan — git history)). The trigger is per-sentence
and mechanical: **every** status / present-tense / future-tense claim in a
folded doc is *presumed stale* and re-verified against current source at fold
time. A list of "known offenders" exists in the plan
(`devbook_plan.md:30` (devbook plan — git history)) but is explicitly
*illustrative and non-exhaustive* — a stale doc that is not on it is still
caught, because the check runs on every sentence, not against a list.

Why so aggressive? Because a "which docs are stale" list is itself a piece of
hand-maintained state whose completeness is load-bearing and drift-prone — the
exact anti-pattern this book exists to fight. The universal check is *"re-verify;
most will pass"*, not *"rewrite everything"*: of the planning audit, for
example, `safety-checker.md` verified clean while several others were flagged
IMPL-AHEAD (`devbook_plan.md:30` (devbook plan — git history)).

### The honesty gate

Folding is also where the four corpora are kept consistent. Each chapter's
accuracy pass triangulates three of them — `source+tests` /
`language-reference.md`+`language-design.md` / `docs/book/` — and classifies
every disagreement as **CONSISTENT / DOC-AHEAD-OF-IMPL / IMPL-AHEAD-OF-DOC /
CONTRADICTION** (`devbook_plan.md:41` (devbook plan — git history)). Findings go
to `TODO.md`; the book itself cites `language-reference.md` for normative facts
and never restates the spec. The planning audit already logged real drift — its
seed catalog records a CoW materialization-point count mismatch ("6 vs 7")
between the corpora (`devbook_plan.md:111-112` (devbook plan — git history)); the
fold then re-derives the live count from current source (the materialization
table in [`11-copy-on-write.md`](11-copy-on-write.md))
rather than repeating either figure. The devbook gate is the forcing function
that keeps all four corpora honest.

## Why chapters are subsystem-ordered, not pass-ordered

The table of contents walks the compiler **by subsystem**, not in strict
pipeline-pass order ([`README.md:13`](README.md),
`devbook_plan.md:39` (devbook plan — git history)). The linear pass order is
given once, in [Chapter 1](01-pipeline-and-driver.md); the chapters then group
related machinery so a reader learning a subsystem reads it as a unit rather
than scattered across the points where each pass happens to touch it.

The distinction matters because the pipeline order is itself easy to get wrong
from stale docs. The actual semantic pass order, read from
[`src/semantic/mod.rs`](../../src/semantic/mod.rs), is: Pass 0 meta
([`mod.rs:107`](../../src/semantic/mod.rs)) → 1 collect
([`mod.rs:234`](../../src/semantic/mod.rs)) → 1.5 import-alias rewrite
([`mod.rs:239`](../../src/semantic/mod.rs)) → 2 resolve
([`mod.rs:248`](../../src/semantic/mod.rs)) → 2.5 constructor-call rewrite
([`mod.rs:255`](../../src/semantic/mod.rs)) → 2.6 `.collect()` target selection
([`mod.rs:265`](../../src/semantic/mod.rs)) → 3 traits
([`mod.rs:270`](../../src/semantic/mod.rs)) → 3.5 derive-field validation
([`mod.rs:275`](../../src/semantic/mod.rs)) → 3.6 recursive-type cycle check
([`mod.rs:285`](../../src/semantic/mod.rs)) → 4 typecheck
([`mod.rs:290`](../../src/semantic/mod.rs)) → 4.5 method-targ sync
([`mod.rs:307`](../../src/semantic/mod.rs)) → 4.6 `suggest_throws` lint
([`mod.rs:317`](../../src/semantic/mod.rs)) → 5 borrow checking
([`mod.rs:336`](../../src/semantic/mod.rs)). There is **no** provenance pass and
**no** `src/semantic/provenance.rs` — a "Pass 4.5 provenance.rs" that older
pipeline diagrams carried is stale; the file does not exist in the tree. This
is exactly the kind of fact the book
cites to source rather than transcribes: [Chapter 1](01-pipeline-and-driver.md)
folds a corrected diagram from `src/semantic/mod.rs`, not from the old README.

## The "In the self-host" convention

Most phase / area chapters carry an **"In the self-host"** section describing how
the Gorget self-host compiler (`tests/fixtures/self_host_*`) implements the same
area, where it diverges from the Rust `gg`, and the current parity
([`README.md:15`](README.md),
`devbook_plan.md:32` (devbook plan — git history)). The self-host is the same
book, not a separate one: Chapters [26](26-self-host-frontend.md) and
[27](27-comparison-bootstrap.md) give the *system-level* treatment, while the
per-chapter sections are the *area-level* mirror.

The non-negotiable rule for these sections is **how parity is stated**:

> Parity is a **procedure, not a number.** Run the relevant comparison test and
> read the printed matched-count:
>
> ```bash
> cargo test --test integration <name>_comparison -- --nocapture
> ```

Never cite parity as a stable fact. The reason is mechanical: the
`*_comparison` tests are **diagnostic-always-pass**. The only hard assertion in,
e.g., `lexer_comparison` is a sanity check that fixtures were found —
`assert!(!fixtures.is_empty(), ...)`
([`tests/integration.rs:9362`](../../tests/integration.rs)) — and the
match/mismatch counts are merely `eprintln!`'d
([`tests/integration.rs:13522`](../../tests/integration.rs),
[`13691`](../../tests/integration.rs)). A green `cargo test` therefore says
*nothing* about parity; only the printed counts do. The comparison drivers are
`lexer_comparison`, `parser_comparison`, `resolver_comparison`,
`type_comparison`, `check_comparison`, `lowerer_comparison`, and
`c_emit_comparison`
([`tests/integration.rs:9346,12406,12683,12997,13193,13390,13549`](../../tests/integration.rs)).
A section *may* add a freshness-stamped dated reading as a snapshot, but the
procedure is the authority.

Where the self-host has no or partial coverage of an area — most backend
chapters, since the self-host frontend stops before code generation — the
section **says so plainly and points at the gap**
(`devbook_plan.md:32` (devbook plan — git history)) rather than omitting the
section or implying parity that does not exist.

## What you can rely on, in one paragraph

Trust the **architecture and the why** in this book. Verify every **number,
offset, score, and `file:line`** against the cited source — and if a chapter is
stamped, the cite was correct as of that commit. Treat any un-stamped
fast-moving claim, and any present/future-tense status sentence inherited from
an older internals doc, as *presumed stale* until re-checked. State self-host
parity as the comparison-test procedure, never as a frozen figure. When the book
and the source disagree, the source wins.

---

*Meta-chapter. Verified against `ffd58b65`. No compiler source is owned here;
the cited `src/` and `tests/` `path:line`s illustrate the conventions and are
re-derived per the freshness rule above.*
