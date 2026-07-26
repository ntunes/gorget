# The excellence system: how delegated rounds run

*Playbook chapter (dated war stories are in-policy here, as in [Chapter 29](29-contributor-playbook.md)). Verified against commit `8f1039ff`.*

`AGENTS.md`/`CLAUDE.md` states every rule of the excellence system — the round
lifecycle, the review gauntlet, orchestration discipline, model allocation — in
compact, normative form, so that any agent harness can replicate the system from
that one file. This chapter is the **extended treatment**: the reasoning behind
those rules and the measured incidents that earned them. [Chapter 29](29-contributor-playbook.md)
is the engineering sibling (the debugging heuristic, layering, gaps, fossils,
premises, plus the scout/review and worktree-discipline war stories); this
chapter covers the *process* side. When a rule here seems overcautious, the
section below it is the receipt.

Rule of the split: **the rule lives in `AGENTS.md`; the evidence lives here.**
A new lesson lands there as a compact rule and here (or in 29) as the story;
owner open-thinking that is not ratified also lands here, marked as such (§9) —
that is how `AGENTS.md` stays lean. The `agents_md_size_ratchet` lint in
`tests/lints.rs` caps the file's size so a lapse cannot go unnoticed; it
measures bytes, not the split itself, and its ceiling ratchets DOWN after
each compaction.

---

## 1. Rounds, lanes, and the round-close battery

**Why Core #9 (every lane, same round) exists.** The rule was ratified
2026-07-16 after an xhigh-effort review found that **9 of its 15 findings were
single-lane landings drifting** — changes correct on one lane (Rust gg, ggdef,
or the self-host) that had silently never landed on the others. It unified three
earlier lessons: the docs-write-through rule, the Batch-A ggdef-lane lesson (a
track flipped fixture expectations to a new rejection while ggdef still
modelled the old write-through semantics, so the definition lane went red after
the landing — hence: any track that flips fixture expectations carries the FULL
ggdef suite in its gates), and the A2-S self-host port pattern.
The cross-lane fixture is the enforcement: a lagging lane is a red test or an
explicit `#[ignore]` + citation, never a promise.

**Why the round-close battery must match CI's target set** (owner-required,
2026-07-23 post-mortem). Three CI-red causes sat red for a **week** while rounds
kept closing green on `--test integration`, because their only detectors lived
in `cargo` targets that `--test integration` never runs:

- a **ggdef gate-drift** — an auto-discovered `cow_*` fixture landed without
  refreshing the ggdef exclusion list (a Core #9 lane miss), red only in
  `cargo test -p ggdef`;
- an **LLVM-missing conformance build-fail**, red only in
  `--test spec_conformance`;
- a `.filter().map()` **byte-truncation MISCOMPILE** whose only detector lived
  in `--test security` — and even there the fixture used byte-sized values that
  hid the truncation, a Core #11 thin-net failure stacked on top.

The consequence is the battery rule in `AGENTS.md` § Round lifecycle step 4:
local-green is *sufficient* only because the local battery now covers every
target CI runs. The residual class a local run cannot catch — a CI-*config*
failure such as a job missing `llc` — is deliberately kept out of the per-round
gate and handled as periodic CI-hygiene instead.

**Why guards, not vigilance** (Core #6's type case). The 2G
loop/branch/comprehension materialize family kept reappearing across review
rounds in new syntactic costumes until the class was retired by an executable
guard. When successive rounds keep finding one class in new costumes, the
round owes the class-retiring guard, not more instances.

## 2. The gauntlet: passes, packs, and model allocation

**Why design-soundness is a blocking reservation even when the code works**
(owner 2026-07-22). The type case: a D10 place-exclusivity check implemented as
a *syntactic 3-arm* match — exactly the shape the No-name-matching/Layering
rules forbid — passed its own tests, then proved **check-clean yet broken on
five uncovered syntactic costumes** the 3-arm enumeration structurally could
not see: comprehension → GIR resource-move ICE; match guard, dict and enum
inline aggregates → ASan heap-UAF; array literal → ASan
allocation-size-too-big. A reviewer
who verified only premise-accuracy would have signed it off; the checklist
exists so the reviewer instead names the invariant (typed metadata at one
source of truth) and the reference-grade shape.

**Why pack reviews are forbidden** (owner 2026-07-21). A 10-track round was
"amortized" by handing a single reviewer all ten briefs in one conversation —
context reuse means every brief after the first is read through the lens of
the previous ones, and attention dilutes across N artifacts. One track, one
agent, clean context; parallelism goes *across* tracks.

**Model allocation, measured** (2026-07-18). In a 2G round with 22 folded
review reservations, the two strongest-model **first-pass** reviews contributed
**10 of the 22**, including two empirically-proven new bug classes — first
contact with a fresh artifact is where the structural defects surface, and
folding is cheapest there. The same round also produced the counter-lesson: a
**standard-model** pass overturned a strongest-model pass's directionally-wrong
claim, which is why every pass gets cross-checked by the orchestrator
regardless of model, and why mandate quality (the reviewer's checklist)
dominates model strength.

## 3. Fixture nets: RED-verification and axis-completeness

Core #12's two clauses were earned in one round (2026-07-25, the
`&`-of-projection work):

**The axis audit.** `&`-of-field write-through — pass a pointer to a struct
field into a `&`-taking function, observe the write in the caller — a 21-probe
battery measured **2 OK / 18 WRONG / 1 cc-fail**, identical on both backends. The integration suite had been
green over that feature for months, because a corpus audit found **exactly one**
fixture asserting runtime values for the cell (`cow_amp_field_arg.gg`) — and it
used `Vector[int]`, a **thin-pointer** field type, one of the two shapes that
work *by accident* (a thin-pointer field gets a slot pointer for reasons
unrelated to the mechanism under test). Two more fixtures **read** as covering
the cell and did not: `cow_fieldpath_mut_writethrough.gg` and
`cow_amp_compound_writethrough.gg` are `&`-of-**whole-value** — verified from
their source: `mutate(&x)` / `bump_field(&c)` borrow the whole local at the
call site, and the field writes happen inside the callee through the
already-formed `&` — a different cell that works for every type. (The audit
names a third: `borrow_struct_field_access.gg`, which the fixture catalog
files as covering the int-field cell, is also `&`-of-whole-value — the
sharpest case of name-suggested coverage.) The other 18 shapes were not badly
tested; **they had no fixture at all.** Hence: enumerate the axes *first*, check what
each fixture *actually* exercises (not what its name suggests), and RED-verify
every new fixture against the pre-fix compiler — a fixture that cannot go red
is not coverage.

**Type-first, measured.** The follow-up corpus-wide audit probed uncovered
cells on both major axes: uncovered **field/element-TYPE** cells broke at
roughly **1 in 2**; uncovered **root-shape** cells broke **0 in 12** — 12 root
shapes probed at a working field type, **zero defects**: 11 correct plus one
Box-deref row that is a documented `E_DerefCoercionUnimplemented` rejection,
not a silent wrong answer (which is why the same audit also reads "11/12
correct" — the two counts are one measurement). Bare-param materialize
measured 8/8 on the same run. Defects concentrate on the type axis, so a
budget-limited partial audit enumerates types first.

**Accidental greens go deep.** One level below the thin-pointer trap: a
by-value struct field that *carries a resource* (`struct Bag: Vector[int]
items`) reads back correctly before AND after a fix — the by-value copy shares
the inner buffer, so the cell can never go red. An axis cell for "plain struct"
must use a scalars-only struct, or it is fake coverage inside an
otherwise-correct enumeration. This is Core #15's question 6 showing up inside
Core #12.

### Green on arrival: the five-month regression

The clause that binds RED-verification to *every* new fixture, not just
bug-fix ones, was bought by a single bisect.

A write-through as plain as `inc(&c.fd)` — a local, a struct field with an
`int` in it, the sigil at an ordinary call argument — silently discarded the
callee's write. The definitional oracle disagreed with both production
backends. Bisected, the break was five months old, and it arrived inside a
commit whose subject was about compile-time builtins: the diff also carried a
few hundred lines of C-backend and lowering changes, which is the shape of
commit a regression rides in on unnoticed.

The commit's own message recorded that the whole integration suite passed. That
was true, and it is the uncomfortable part: at that revision **no fixture in
the corpus passed an `&`-of-a-projection to a call at all**. The suite was not
negligent, the shape was simply absent, and an absent shape cannot regress.

Coverage did arrive — four months later. Two fixtures landed whose names
promised exactly this feature. Both used a `Vector[int]` field, and a
thin-pointer field is one of the two cells that happen to work: the read path
already yields a pointer, so the sigil forwards it and the write lands. The
fixtures went green the day they were written and stayed green, and their names
told every subsequent reader that the ground was covered. Nothing measured the
type axis for another three weeks, and when something finally did, the feature
scored two working cells against eighteen broken ones.

Three failures stacked, and only the middle one is really about testing: the
defect was invisible when it landed; the net that eventually arrived sampled
the working cell; and no one asked which cell it sampled, because the filename
answered a question it had not actually been asked.

Hence the two clauses. A fixture that has never been observed to fail is a
claim, not a net — and when there is no pre-fix compiler to run against,
breaking the guarded mechanism on purpose is the only way to observe the red.
And a fixture's name is read far more often than its body, which makes the name
a scope claim in its own right: either it is true, or it is narrowed until it
is.

## 4. Verify the verifier: instruments and their scopes

**A gate never seen to fail is not evidence.** A scout once changed one lane's
behavior, and `-p ggdef`, `--test spec_conformance`, the full 1828-test
sweep, ASan, and the bootstrap **all stayed green** — no fixture pinned the
changed shapes cross-lane, so "all green" carried literally zero information
about the change. Since then (Core #13): before "gates green" supports any
claim, demonstrate at least one gate going RED on a deliberately broken
variant. This generalizes the leak-detection positive-control rule to every
gate.

**ggdef as a triage instrument.** Run the shape through the definitional
oracle *during triage*, not only at round close. `&` in owning positions,
by-value field write-through, and `&` of a non-place were each **one ggdef run
away from discovery** — and ggdef was right all three times. A
production-vs-oracle disagreement is a Core #8 event (at least one of them is
wrong), never a curiosity.

**…and its structural blind spot** (same-day correction, 2026-07-25). ggdef is
a value-semantics interpreter: realloc-induced use-after-free is
**unobservable in it by construction**. Measured: ggdef **accepts every
in-subset D10 place-exclusivity costume** — get-chain, field-path,
alias-provenance, mutating-receiver, all five nested-writer forms — and prints
the pre-mutation value cleanly, while the same programs are live heap-UAFs in
production. Reading that as "the programs are sound" is the exact trap the
rule exists to block. Division of labor: **ggdef adjudicates what an accepted
program computes and accept/reject within its model; ASan on the real backends
adjudicates memory validity.** Pick the instrument that can see the failure
class. And ggdef can simply **lag** a ratified decision — at the time of
writing it still rejects the aggregate-init mover shapes that D10(b)
ADDENDUM 3 ratified as accepted — which is a Core #9 lane gap to file, not an
oracle verdict to obey.

## 5. Invariant comments and false records

Core #14 ("an invariant-asserting comment needs an enforcing guard, or it gets
deleted") was earned when **four false invariant-comments surfaced in a single
round**, two of which actively misdesigned work:

1. a **Phase-1 fossil doc-comment** misled two separate readers into aiming a
   guard at the wrong target;
2. a *"source-unreachable for the tainted case"* comment sat directly over a
   **measured duplicated-`Drop`** — the "unreachable" path was the bug;
3. a *"clone to prevent aliasing"* comment sat over a **shallow copy** that
   prevented nothing;
4. the devbook documented a carve-out (**`Case 2b`**) whose guard condition was
   **unsatisfiable and had never executed** — a documented behavior that had
   never once happened.

Each read as an established fact; each was refutable by checking.
The rule is Core #6 applied to prose: either the claim is a
`debug_assert!`/lint/typed guard, or touching the code obliges you to verify
or delete it — never inherit it.

## 6. Mechanical rigor: the procedures and the six questions

**The audit that produced Core #15** (owner 2026-07-25, when model budget
forced a downgrade of the review fleet). Auditing what actually found defects
in the 2026-07-25 CoW round: almost every finding came from a **procedure**,
not from a clever reading — build-and-run instead of source-read; break the
mechanism and watch which fixtures go red; run the shape through ggdef;
enumerate a typed axis and probe every cell; grep the record for an
"already filed" claim. The clever readings found *less* than the dumb
procedures. So the procedures were encoded — the gauntlet must work even when
the reviewer is not sharp.

Receipts per procedure:

- **(a) verification commands** — briefs routinely carried post-patch or
  pre-drift line numbers; a claim with its literal re-check command is immune
  to that drift, and a weaker reviewer can execute a register where they could
  not judge load-bearingness.
- **(b) present the SET** — three consecutive review passes each found an
  undisposed site in the same class, because each pass audited only the file
  the *previous* pass had been burned on; the enumeration was partial, so its
  incompleteness was invisible. A 12-row table with a disposition column makes
  a missing row obvious to anyone.
- **(c) grep for the corrected thing** — folding corrections *additively*
  produced **five** stale remnants in one round, twice shipping instructions
  that contradicted a correction four lines above them. `must_replace`-style
  asserts catch a missing edit; only a grep catches a surviving contradiction.
- **(d) fixed procedures** — "fixture F guards mechanism M": **three of four
  cited guards did not guard** when M was actually broken and F run. "X is
  filed": a census claimed "split out and filed" that existed only in `/tmp`
  exhaust — grep of the record returned 0.

**The six questions, with the saves that earned them.** These are judgement
questions no checklist generates; the orchestrator asks them of every brief
and every "defect" before acting:

1. *Is this asymmetry a DEFECT, or two positions with different RATIFIED
   semantics?* — `Pair(v[0], !v)` accepted while `f(v[0], !v)` rejects looked
   like a bug. It isn't: a constructor argument is a consuming *boundary*; a
   call argument is a *borrow* — different ratified rules. Treating it as a
   bug would have shipped an over-rejection forcing a `.clone()` the compiler
   already places automatically.
2. *Can this guard catch its OWN class?* — caught twice in one round: an
   AST-payload scan advertised for a memory-unsafe class could not see
   `BinaryOp` (no `Vec` payload) — the one variant it was written for; and a
   participant-construction guard only fires where participants are already
   built, so a site that never routes constructs nothing and sails past it. A
   guard that green-lights its own class is worse than none — it reads as
   coverage.
3. *Is this enumeration TOTAL, or a selection?* — the three-passes story in
   (b); a fixture axis that sampled 4 of ~15 values, all four being ones that
   work; "the two `&`-formation sites" that was not the census.
4. *Does this rule's SUBJECT actually cover the case?* — an
   invalidation-centric aliasing rule had **no subject at all** for
   `f(&v, &v)`: two writers, no live borrow to invalidate. That is a category
   error, not a coverage gap — no amount of widening the live-range interval
   fixes a rule that quantifies over a thing the case doesn't contain. (Four
   in-repo NEG fixtures went green under the single-rule prototype before this
   was caught.)
5. *Am I reasoning about emission, or emission ORDER?* — "this position
   already clones, so the argument is a value, not a live alias" was refuted
   by reading the GIR: the boundary clone is emitted *after* every sibling
   argument evaluates, so the alias is live exactly when it matters. Relative
   order is only visible in the IR.
6. *Is this passing case ACCIDENTALLY correct?* — the thin-pointer and
   resource-carrying-struct greens of §3: cells green for reasons unrelated to
   the mechanism under test, i.e. cells that cannot go red.

**And the question about the record itself:** *is this premise still true, or
a filed fact that decayed?* Four filed premises were refuted by measurement in
a single round, every one reading as established fact. Recency and the
reasoning behind a decision matter as much as where it is written — a
considered decision in a scratch file outranks a stale one in the ledger, and
the correct response is to file the good decision properly, not to discount it
for living in the wrong place.

**The health metric:** the honest signal that the process has thinned is
reviews that only find compression errors and never a design defect.

### Folding is where the defects come from

A gauntlet catches defects in an artifact. It also *creates* them, and on one
campaign the creation rate was high enough to dominate: three consecutive
review passes each opened with a blocking finding that the previous fold had
introduced.

The mechanism is always the same. A reviewer reports that one clause in a
paragraph is false. The fold replaces the **paragraph**, because that reads as
the tidy edit — and the true material sitting beside the false clause goes with
it. In one case a list of rejecting positions lost the word "returning", and the
rule then licensed returning a borrow. In another, the paragraph carrying "this
requirement does not apply to the move sigil" was replaced while the
neighbouring sentence was strengthened into a universal, so the section rejected
a form that the same section, a hundred lines further down, described as
mandatory.

Neither survived contact with the next reviewer, which is the system working.
But each cost a full pass, and a pass is not cheap. The lesson is narrower than
"be careful": **fold at the granularity of the defect.** If one clause is wrong,
edit that clause. When a paragraph genuinely must be rewritten, diff the old
against the new and account for every sentence that disappeared — the question
is not "is the new text correct" but "what did the old text say that the new
text no longer says".

The same campaign gives the counter-example that makes the point precise. The
one artifact that never regressed across four passes was a three-row table, and
it never regressed because nobody ever needed to rewrite it — corrections landed
in the prose around it. Small, self-contained, load-bearing statements survive
folding. Long explanatory paragraphs do not.

## 7. Orchestration: how work actually gets lost

Chapter 29's "Worktree discipline" section carries the full war stories for
the orchestration rules — the nested-under-main path trap (a heredoc fallback
wrote 20 files into the main checkout), the repo-global stash race (two scouts
popped each other's stashes; reflog surgery), the killed-agent recovery drill
(a scout lost 26 minutes of un-checkpointed prototype; three agents stalled on
backgrounded final gates), and the nested-fork collision (an output-reviewer's
own un-isolated forks reverted its worktree mid-test — `isolation: "worktree"`
applies to *nested* forks too).

One story lives here because it is about round hygiene rather than worktrees:
**the disk-fill.** Agent build-scratch does not dispose of itself, and
"later" is when the disk is already full: **~475K stale `/tmp/.tmp*` /
`tmp.*` `gg`-build-scratch directories** accumulated across rounds until the
volume filled and a session died mid-task with "no space left on device". The
capture-first-prune-second procedure in `AGENTS.md` § Multi-agent
orchestration rule 6 — including the specific `find` sweep and the
stray-stash capture — is the executable form of that lesson, run at every
round close.

## 8. Durable repros: why `known_gaps/` exists

The owner rule (2026-07-24) that every filed reproducible bug ships a
committed `tests/fixtures/known_gaps/` reproducer was earned the hard way: an
enum-payload **leak**'s `/tmp` repro evaporated with its session. A later
hand-reconstructed probe did not faithfully reproduce the original shape, and
a Core #5 re-check could then neither confirm the fix nor attribute the
commit — the probe was clean at HEAD *and* at a should-be-buggy baseline,
i.e. **indeterminate**. A committed repro keeps the exact shape (for leaks
that means a *heap-forced* value, not a literal — a literal is a false
negative), reproduces consistently for whoever picks the item up, and
graduates to a live regression fixture the round the bug is fixed. Triage
paperwork stays in `/tmp`; the repro is the one triage artifact that gets
committed.

## 9. The implementation portfolio: succession and after

The **ratified** succession plan lives in `AGENTS.md` § Self-host as the
elegance showcase: the self-host replaces Rust gg as the primary reference
once runtime parity reaches ~100%; a "reference lags the self-host" finding is
a succession milestone; Rust-side fixes are oracle hygiene; ggdef adjudication
is the truth axis that makes the succession decision safe.

**Post-succession leaning (owner 2026-07-18, open thinking — NOT ratified):**
KEEP the Rust implementation even after full parity, for three reasons.
(1) **Triangulation** — an odd number of implementations disambiguates (the
adjudication split's first reading proved it: 13 two-compiler agreements were
overturned by the third opinion); caveat: Rust gg and the self-host share
semantic lineage, so ggdef remains the only structurally independent member —
the triad is a diversity portfolio, not three equal voters. (2) **ggdef must
stay SMALL** — its authority derives from being a readable definition; grown
to full coverage it just becomes another compiler, so the full-coverage third
opinion has to be Rust gg. The out-of-subset count should shrink toward "every
semantically load-bearing fixture adjudicated" and deliberately stop there,
not trend to zero. (3) **Bootstrap trust** — a self-hosted compiler alone has
the trusting-trust problem; an independent implementation built by a foreign
toolchain is the trust anchor (cross-build the self-host from Rust gg and
diff — diverse double-compiling). Likely end-state roles: ggdef = the norm
(bounded subset) · Rust gg = independent juror + trust anchor, frozen into a
conformance implementation (maintained for correctness, not perf/features —
which is what makes keeping it affordable) · self-host = the product and
showcase.

Because this is open thinking, it binds nothing: a round that needs a decision
in this area asks the owner (Round lifecycle step 7) rather than treating this
section as settled policy.

---

*The rules this chapter explains live in `AGENTS.md`/`CLAUDE.md`; if this
chapter and that file ever disagree, `AGENTS.md` wins and this chapter is
stale — fix it in the same round (Core #9 spans docs). The exception is §9,
which is open thinking `AGENTS.md` deliberately does not carry.*
