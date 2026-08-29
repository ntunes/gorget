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

**Why the default is the strongest model in EVERY role** (owner 2026-07-27,
reaffirmed 2026-08-06). The measurement above was for a while read as a licence
to run scouts, executors and middle passes cheaper. The owner closed that off:
every role gets the strongest available model. The standing argument for
downgrading the executor — it is cheap and fast, and the output-review catches
what it misses — fails on what a diff review structurally cannot see. Review
reads what was written, not what was not: a read-site fix that works and passes
its gates only looks wrong to a reviewer who independently derives the
write-site one, and a silently narrowed brief reads as a coherent diff. Some of
that gap is mechanised (Core #15(d)'s census procedure, the fixture-coverage
gate), which is why the gauntlet tolerates a weaker agent at all — but a catch
is not free: it costs the whole execute cycle, a re-brief, a second
output-review, and orchestrator context, the scarcest resource in a long round.
The rounds where executors did the most discovery are the proof that execution
is not clerical work: Round XXX's Track D surfaced a new bug class at every
phase, none of which a brief could have front-loaded. And in this tree an
executor's wall-clock is dominated by `cargo build` and integration runs rather
than token generation, so the speed saving is smaller than it looks while the
failure mode is not.

The rule's own history is a second lesson. It was ratified 2026-07-27 but lived
only in one harness's private memory with a `DONE.md` breadcrumb citing it,
while `AGENTS.md` went on prescribing the opposite for rounds afterwards — the
exact failure the file header's "never only in one harness's private memory"
clause exists to prevent. A process rule that is not in `AGENTS.md` is not a
rule; it is a habit that dies with the session that learned it.

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

### The oracle is fallible too (owner 2026-08-22)

`ggdef` is the definitional oracle, and the temptation it creates is to treat
its answer as the definition. It is not. It is an *implementation* of the
definition, and it fails in three distinct ways that must not be collapsed
into one another:

| mode | what it means | what it is NOT |
|---|---|---|
| **BLIND** | adjudicates VALUE semantics; cannot observe memory invalidation at all | not a soundness verdict — it cleanly accepts live heap-UAFs |
| **LAGGING** | has not yet implemented a ratified decision | a lane gap (Core #9), not a verdict about the language |
| **WRONG** | its own answer is simply incorrect | not a reason to doubt a well-grounded diagnosis |

The first two were already written down. The third is the owner's addition,
and it closes the loophole the other two leave open: a reader who has ruled
out blindness and lag can still be wrong to defer, because the oracle can
just be *mistaken*.

**Why this matters more than it looks.** The whole point of an oracle is that
it settles arguments, so an oracle believed infallible converts every one of
its errors into a ratified error — and the more the project leans on it, the
more expensive that becomes. This is Core #8 pointed at the reference itself:
*reference-grade is the bar, not parity with a possibly-wrong reference.*
Three-way agreement between ggdef, C, and LLVM on a wrong answer is not a
pass; it is three bugs, and the usual remedy is to make the language REJECT
the program.

**The same applies to Rust `gg`**, for a different reason: it is being
succeeded by the self-host (§9). "The self-host differs from Rust `gg`" is
therefore not automatically a self-host bug — where the self-host is right
and Rust is wrong, that is a succession milestone, the Rust side gets fixed
as oracle hygiene, and the self-host is never dumbed down to match.

**The procedure**, because a principle without one decays into a slogan:

1. Run the shape through the oracle during triage — this is unchanged, and
   disagreement is still a finding rather than a curiosity.
2. Report the oracle's answer **labelled as the oracle's answer**, never as
   the verdict. Intended semantics come from the docs and first principles.
3. Where lanes AGREE, ask separately whether the agreed answer is *correct*.
   Agreement is evidence about consistency, never about truth.
4. **A BOTH-WRONG row is an OWNER ASK — but ONLY where the semantics are NOT
   YET RATIFIED** (owner 2026-08-22: *"just use common sense and be reasonable.
   If you ever feel ggdef could be wrong about something not yet ratified, then
   raise it with me"*). **Where the ledger already rules, there is nothing to
   ask**: ggdef is simply wrong or lagging against a standing decision, and the
   answer is to fix ggdef and file it. Escalating a settled question is its own
   defect — the D37 entry records a round lost to exactly that, and the check is
   Core #15e Q1: *is this asymmetry a defect, or two positions with different
   RATIFIED semantics?* **So the sequence is: read the ledger FIRST, then ask
   only about the cells it leaves open.**

   BOTH-WRONG is a *tracked category*, not a feeling: the parity
   adjudicator in `self_host_runtime_diff` reports it when **Rust `gg` and the
   self-host AGREE on an output and ggdef, having run cleanly to a `Value`,
   DISAGREES**. Raise it whether or not you feel unsure, with the evidence for
   both readings. Not a guess, and not resolved by deferring to whichever lane
   feels authoritative.

   **Why category-gated beats confidence-gated.** The first draft of this rule
   said "if you are UNSURE, ask" — which depends on the agent NOTICING that it
   is unsure, and a confident wrong reading is exactly the case that never
   triggers it. BOTH-WRONG fires off a computed row instead, so it works with a
   weaker reader (Core #15: make rigor mechanical, not clever). The gate already
   prints the three-way — stem, agreed output, ggdef's output — so the ask
   assembles itself.

   **Note what does NOT route here.** A static rejection routes UNADJ, not
   BOTH-WRONG; so do out-of-subset programs, float-render HOLDs (D8), traps,
   fuel exhaustion, and ICEs. BOTH-WRONG is specifically *two implementations
   agreeing against a clean definitional answer* — which is why it is worth an
   owner's attention: it is the one configuration where "the lanes agree"
   carries real weight AND the definition says otherwise.

Step 4 is a third owner-ask category alongside the two in the round lifecycle
(a genuine design decision; permission to close a non-convergent round). It
exists because the failure it prevents is silent: a wrong guess about which
oracle is right does not announce itself, it just becomes the new baseline.

### A guard's SLACK is its sensitivity, and a two-anchor probe cannot tell factors apart

Measuring how much of `AGENTS.md` its own four lints actually guard produced two
confident wrong answers before it produced the right one, and both failures
generalise to any guard-coverage measurement.

**Wrong answer one: the byte ceiling manufactured coverage.** The first census
asked "does ANY of the four guards fire", and reported that a clause inserted on
a heading was caught. It was — by `agents_md_size_ratchet`, because the file at
that moment had a few dozen free bytes and a clause-length insertion overshot
the ceiling wherever it landed. Fold a `<=` ceiling into a coverage rate and the
file reads as 100% guarded at low headroom and ~28% guarded at high headroom,
with no change in coverage whatsoever. **A guard whose firing depends on current
slack rather than on the mutation belongs in its own column, never in the
verdict.** The same caveat binds the unpinned-prose ratchet, which is two
ceilings: it sees an insertion only where the insertion pushes a counter PAST
its constant, so its reach is a function of how recently it was ratcheted down.

That is not a theoretical worry, and prose did not prevent it. Minutes after the
"re-ratchet after every compaction" sentence was written, adding a single probe
split one long uncovered run and took the `>= 100` count one below its constant.
The out-of-process cross-check, run for an unrelated reason, went from **10/10
to 0/10** on the band that had been catching — one unit of slack switched a
whole class of insertion from caught to invisible — while every lint in the
suite stayed green. The fix was Core #6, not a reminder: the ratchet now asserts
its constants EQUAL the measurement, so shrinking unpinned prose without
following the constants down is itself a red, with the two values to set printed
in the failure message. **A ceiling whose distance from the measurement is its
blind spot should be pinned to the measurement, not merely documented.**

**Wrong answer two: two anchors, two factors, one conclusion.** A five-row probe
inserted the same clause at two positions and reported that CAPITALISATION
decided whether the guards fired. A second reading of the same rows said the
discriminator was PINNED-vs-UNPINNED. Both were wrong, and both were unfalsifiable
from that data: the two anchors differed in *both* candidate factors at once, and
in a third nobody had named — the length of the uncovered RUN each landed in.
Over the full population the run length partitions the outcome exactly, while
capitalisation moves the rate a few points (via the sentence census) and leaves
the ratchet's own catch count identical. **Two samples cannot separate two
factors.** Measure per site class over every insertion point, or report no rate.

**What made the third answer trustworthy** was Core #13 applied twice: the
instrument calls the guards' OWN helpers rather than a re-implementation free to
drift, it asserts a positive control (the unmutated file must be GREEN) before
reporting anything, and an out-of-process cross-check builds mutations whose
class is obvious by construction and runs the REAL `#[test]`s on them. The
cross-check is what would catch the instrument agreeing with itself.

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

Narrow editing alone is not enough, because the false clause usually has
accomplices. Twice in the same campaign a fold corrected exactly the sentence a
reviewer had cited, and the next pass opened by quoting its neighbours: a
section heading three lines above still named the retired behaviour, and a
`# ERROR:` comment inside the example six lines below still asserted a
rejection that the corrected prose no longer claimed — both surviving the fix
sitting between them. A claim is rarely made once. It is made in the heading,
in the topic sentence, in the example, and in the comment on the example, and
correcting one instance leaves a section that contradicts itself more visibly
than before the fix.

So the discipline has two halves that pull in opposite directions and are both
required: **edit at the granularity of the defect, verify at the granularity of
the section.** After the narrow edit, re-read the whole enclosing section —
heading, both neighbouring paragraphs, and the comments inside its code
examples. The grep in rule (c) catches a surviving contradiction elsewhere in
the file; this catches the one three lines away, which is the likelier of the
two and the one a reader is guaranteed to see.

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

**A cited repro is EVIDENCE, not a second filing (owner 2026-08-23).** The
convergence arithmetic counted the mandated `known_gaps` fixture as an
inflow item in its own right, so one discovery was charged **+2**: once for the
`todo/` item and once for the repro the cardinal rule obliges it to ship. That
made the compliant filing cost more than the non-compliant one — a metric that
rewards skipping the repro. A repro cited from an item's `repro` field is now
that item's evidence and is not counted; an **uncited** gap fixture still counts
on its own, because nothing else is tracking it.

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

## 10. Convergence gate — the ratchet that keeps the ledger shrinking (owner 2026-07-28 + STRICTENED 2026-08-02)

The compact rule (in `AGENTS.md` Round lifecycle steps 1 and 5):

- **Step 1 lens.** The round's expected NET items closed is an axis alongside
  severity and theme fit. Bias toward class-fix (Core #4) and bulk-graduation
  tracks over instance-fixes-with-follow-ups. A projected-positive round is
  reshaped (bundle a class-fix track into it) or split so the additions land
  in a dedicated later round.
- **Step 5 gate — STRICT 2× RULE (binding from Round XXVIII inclusive, owner 2026-08-02).**
  Every `DONE.md` round entry MUST carry `Convergence: known_gaps A→B · TODO
  items C→D · net ±N` and satisfy: **(a)** ratio of CLOSED to FILED ≥ 2:1 —
  files N ⇒ closes ≥ 2N; **(b)** all own-follow-ups filed during the round
  fix same-round (if the fix isn't in scope, RESCOPE the track at scout stage
  rather than land partial + file follow-up); **(c)** net strictly DECREASES.
  First thing after all planned tracks complete: measure the net; if any of
  (a)/(b)/(c) fails, ADD tracks / launch closes UNTIL the rule is met.
  Never close on paper with debt. The old "net ≥ 0 acceptable when named
  intentional" exemption is RETIRED.

**Why the rule.** Round XI added four `known_gaps/` (Track M) plus three
follow-up TODOs while graduating three; retrospectively, +2 net. Left
unchecked, that trend drifts monotonically upward. The owner formalized
the counter after that round: measurement is cheap (one `find` + one
`grep`), the correction is a first-class round action (bundle a class-fix,
or open a dedicated pruning round: BOTH-WRONG hunt, `known_gaps` triage,
TODO stale-scan).

**Positive-net closures no longer permitted (owner 2026-08-02).** The two
"legitimate positive-net" shapes below are retained for HISTORICAL context
only — from XXVIII inclusive, both are handled by ADDING more closes in
the same round rather than accepting the positive net:

1. **Class-hunt discovery.** A Core #4 class-hunt uncovering adjacent
   defects the round didn't know about: file them AND land ≥ 2× closes
   for them same-round (or rescope). Round XII Track N2's 9-cell hunt
   surfaced four adjacent defects (struct-lit field / enum-variant field
   / vector-lit element / closure-body return SIGILL); under the new
   rule that round would OWE 8 additional closes elsewhere.
2. **Wrong-direction alternative rejected.** A round that would otherwise
   ship a Core #8 defect for a positive net stops the shortcut ("declined
   to sharpen the reject because D36 mandates accept") — but still owes
   the closes elsewhere.

**Round XXVIII precedent (2026-08-02).** Own follow-ups filed by Track A
(ggdef tag-check LAG) and Track D (SH liveness-diff pass reconstruction)
were both fixed same-round rather than filed for a later round: Track A
LAG closed via string-based receiver-gate in ggdef `elaborate_method`
(commit `77b92fc0`); Track D SH liveness-diff was fixed via a narrow
strip of `[warn] gir_liveness_diff:` lines from the bootstrap C-body
comparison (same commit), reverting the 2→4 global ceiling raise that
would have blinded three additional stages of future convergence-drift
detection to accommodate one instrumentation warning.

**How the numbers avoid gaming.** Both counts are regen commands, not
values: `find tests/fixtures/known_gaps -name '*.gg' | wc -l` and a
grep-based TODO-bullet count. Recording the commands (not the numbers) in
the round entry lets the next round re-run identically. A round that
manually edits the count without regenerating trips the gauntlet's
"re-verify every premise" rule as soon as the next round tries to diff.

**The blind spot the first strict round walked into (2026-08-02).** Round
XXVIII, the first measured under the strict rule, reported `known_gaps
94→92 · TODO items 533→533 · net −2` and a DONE entry claiming *"TODO
strictly decreases."* Both halves of that sentence were wrong in
instructive ways.

First, **the two counters are not one counter.** The −2 came entirely from
`known_gaps` graduations; the TODO count was flat. The rule's two
statements had drifted apart during the same commit that introduced them —
`AGENTS.md` step 1 said *"TODO must strictly DECREASE"* while step 5 said
*"net must strictly decrease"* — so the round could satisfy one reading
and fail the other, and the entry quoted whichever it satisfied. The rule
now names one number: **the combined net the script prints**, with a
`known_gaps` graduation counting as a closure.

Second, and worse, **four of the round's five convergence-relevant moves
were invisible to the meter.** `scripts/convergence.sh` deliberately skips
prose sections — the handover block included — because the mandatory
close-time rewrite churns those bullets (that skip is itself the fix for an
earlier drift, where a rewrite inflated the count by +7 while real items
fell by 1). But the round *filed* its one new follow-up into the handover
block and *closed* three items there too. Neither the filed side nor the
closed side reached the counter; they cancelled, and the flat 533→533 read
as "no change" when nine things had happened. A gate that cannot see the
work it governs is not a gate.

The fix is a filing-location rule with an executable guard rather than a
prose reminder (Core #6): filed work lives in a **categorized** section,
the handover carries state and pointers only, and `convergence.sh` now
FAILS when a `🆕`-marked bullet — the project's own "filed this round"
marker — appears inside a prose section. Verified red on the three items
standing in the handover when the guard landed. Note the one-time
accounting consequence: moving genuinely-filed items out of the handover
*raises* the visible count, because they were never counted before. That
is an undercount being corrected, not a regression, and the round that
does it says so in its entry rather than letting the next round read it as
backsliding.

**Why the script became the arbiter (2026-08-04).** Two consecutive rounds
asserted compliance their own numbers did not support, and in both cases the
assertion was about the clause that *wasn't* machine-checked.

Round XXVIII claimed "TODO strictly decreases" with TODO flat. Round XXIX
claimed "STRICT 2× satisfied" while its stated tallies came to 10 closed
against 7 filed — a 1.4:1 ratio against a 2:1 floor — and justified it by
quoting the *net*, which is clause (c). Clause (a) was never evaluated.

The structural cause is worth naming, because it will recur in any rule with
mixed enforcement: **clause (c) was scriptable and clause (a) was a hand
count.** Agents reach for the number a tool prints. A conjunction of three
clauses where only one is measured behaves, in practice, like a rule with one
clause.

Two fixes followed. First, clause (a) was restated as an inequality over
quantities that are countable rather than judgeable: since
`net = filed − closed`, the requirement `closed ≥ 2·filed` is exactly
**`net ≤ −filed`**. Same rule, but expressed in terms of `filed` (a count of
deliberate acts) and `net` (already measured) instead of `closed` (which
invites reinterpretation of what a rewrite, a narrowing, or a split bullet
counts as). Second, `scripts/convergence.sh` grew a third argument — the
round's filing count — and now evaluates every clause, prints a per-clause
verdict, exits non-zero on any failure, and **refuses to pass silently when
the filing count is omitted**. The definitions of filed/closed/neither live
in the script header, so there is one text to read and it is the one that
runs.

**And the unwritten-rule vector, closed the same day.** Round XXIX's close
cited a "big-ticket items may defer" exemption from a verbal clarification
given to one agent. The owner revoked it and had the revocation written into
the rule, because the failure mode generalises past this instance: a
clarification that exists only in one conversation becomes a precedent the
moment a `DONE.md` entry cites it, and the next round inherits it as though
it were policy. The rule now states that no size/effort exemption exists to
any clause and none may be inferred, that a clarification absent from
`AGENTS.md` is not a rule, and that an entry citing one is a defect to fix
rather than a precedent to follow. The XXIX entry carries an inline
correction to that effect — history preserved, precedent neutralised.

**Why architecture rounds needed no exemption after all (2026-08-04).** For
roughly ten rounds every multi-round architectural item — the transient-view
model, the CoW cost contract, the drop-order work — sat unstarted while
defect rounds shipped. The obvious diagnosis was that the convergence rule
penalised big work, and an exemption was drafted for "architecture rounds".

Measuring it first killed the exemption. The arithmetic, for a round landing
one phase of a phased item:

| case | closed | filed | net | (a) | (c) |
|---|---|---|---|---|---|
| intermediate phase, nothing discovered | 0 | 0 | 0 | **pass** | fail by 1 |
| intermediate phase, *k* defects found | 0 | *k* | +*k* | fail | fail |
| final phase (item removed) | 1 | 0 | −1 | pass | pass |

Clause (a) *passes* when you file nothing — `0 ≥ 2×0`. A round that
*completes* an architecture item already passed unaided. The only real
blocker was clause (c), and it missed **by exactly one unit**.

The cause was not size and not difficulty: the metric counts *ledger
movement* — TODO bullets and `known_gaps` fixtures — and architecture
produces code and capability without moving either counter. A phase could be
a thousand lines of measured, fixture-covered work and read `net +0`,
indistinguishable from a round that did nothing. **Architecture was invisible
to the instrument, not penalised by it.**

Which made it an *encoding* problem, not a rule problem. The fix is one
bullet per declared phase: each landing then closes one bullet, nets −1, and
passes on its merits. It is accounting-neutral over the item's life —
`+(N−1)` once at filing, then `−1` per phase, totalling the same `−1` as a
single bullet — so it manufactures no credit and only changes *when* credit
lands. One wrinkle worth knowing: correct an existing fused entry *between*
rounds, because a round that splits an entry and then lands one of its phases
nets `+1` and fails; done in the interstitial the one-time cost lands in the
next round's baseline, which no round is claiming compliance against.

The generalisable lesson is about instruments rather than convergence: when a
gate reports "no progress" on work that plainly made progress, suspect the
encoding before rewriting the rule. An exemption would have added a
self-declared category — the same shape as the "big-ticket" carve-out revoked
the same day — to solve a bookkeeping mismatch. What remains genuinely
unresolved is only the discovery case: a phase that uncovers *k* defects
still fails, and that is arguably correct, since the queue really did grow.

**The escape hatch is a person, not a category (owner 2026-08-04).** Some
rounds will genuinely be unable to meet a clause — most obviously the
discovery case, where a phase uncovers *k* real defects and the queue
honestly grew. Two ways to handle that, and only one survives contact with
agents.

The tempting one is a *category*: architecture rounds, big-ticket items,
discovery rounds. Every such category is self-declared, so within a round or
two it is being invoked for work that merely resembles the category. That is
not speculation — "big-ticket items may defer" was granted verbally to one
agent and was load-bearing in a `DONE.md` entry the same week, cited as
though it were policy.

The one that survives is an **owner ask**. A blocked round stops and asks for
permission to close, carrying the script's verdict verbatim, which clause
failed and by how much, what closures were attempted, and why closing beats
continuing. Three properties make it robust where a category is not: it
cannot be self-invoked; it is per-instance, so it grants nothing about the
next round; and the round *stays open* until answered, so there is no
optimistic-close path for an autonomous loop to take.

Two guards on it. The waiver covers the **convergence clauses only** — a red
battery is never waivable, and conflating "the gates" would have made it so.
And the override deliberately does **not** live in the instrument: there is
no `--waive` flag, so `convergence.sh` stays red and the failure remains
visible in the record forever, with the waiver recorded beside it *as a
waiver rather than as compliance*. An override the tool can express is an
override the tool will eventually grant itself.

**What the rule does not cover.** Rules like this are for the *ledger*
(what's tracked as pending); they do NOT displace parity ratchets
(`RUNTIME_DIFF_MATCH_FLOOR`, `GGDEF_ADJUDICATED_FLOOR`), which are code-
measurable and belong in the round-close gate itself. Convergence and
parity are orthogonal: a round can raise parity while opening a filed
class of new defects (that is what a bulk-fix + class-hunt round looks
like), and the DONE entry carries both.

## 11. Probe hygiene — and a rule that decayed into a false premise

MIND THE PROBE exists because a probe that lies costs more than no probe:
the wrong verdict is *believed*, and it propagates into briefs.

**The pipeline is the masker, not the tool.** `cmd | tail`, `cmd | grep`,
`cmd | head` all report the *last* stage's exit status. A probe that is
killed, crashes, or never runs still reads as exit 0 through a pipe. Every
crash verdict comes from invoking the binary directly and reading its own
unmasked `$?`. The failure mode is not hypothetical: a killed bootstrap in
one round reported `tail`'s exit 0 with no result line, and was briefly
read as a pass.

**The clause that decayed.** This rule used to read *"never read a crash
off `gg run` (it masks SIGSEGV as a silent exit 1)"*. That was true when
written and is **false now**: Round XXIV Track B made `gg run` propagate
`128 + signo`, and `gg_run_propagates_signal_death`
(`tests/integration.rs:2250`) is the guard pinning it. Re-verified
2026-08-05 — `gg run` on a known-SEGV fixture returns 139.

The episode is the Core #15e rider in miniature — *is this premise still
TRUE, or a filed fact that decayed?* — and it bit in the most instructive
possible way: the stale clause was used to *explain* a scout's bad
measurements, and the explanation was itself wrong. Masking would produce
**missed** crashes; the scout reported **phantom** ones, which masking
cannot cause. A mechanism that does not fit the evidence is not an
explanation, however authoritative its source. When the real cause is
unknown, the honest record says so.

Two rules survive the correction, and they are the durable ones:
**re-measure, never inherit** (Core #5), and **a rule justified by a
mechanism is only as live as that mechanism** — when you lean on one, check
it still exists (Core #14's logic applied to process rules, not just code
comments).

---

*The rules this chapter explains live in `AGENTS.md`/`CLAUDE.md`; if this
chapter and that file ever disagree, `AGENTS.md` wins and this chapter is
stale — fix it in the same round (Core #9 spans docs). The exception is §9,
which is open thinking `AGENTS.md` deliberately does not carry.*

### The sanitizer has its own false green: never wrap an ASan binary in `stdbuf`

Two probe idioms this tree relies on are individually correct and *mutually
destructive*. A comprehension miscompile prints its correct output before it
crashes, so reading it without `stdbuf -o0` shows the right answer and hides the
fault — which is why one brief made `stdbuf` mandatory. But prefixing an
ASan-instrumented binary with `stdbuf` aborts before `main` with *"ASan runtime does
not come first in initial library list"*, exits 1, and produces an **empty**
sanitizer classification. A classifier that greps that empty output for
`ERROR: AddressSanitizer` finds nothing and reports the fixture clean.

So the mandatory-`stdbuf` rule and the sanitize gate, composed, yield a false
GREEN on exactly the memory-safety cells the gate exists to catch. The rule is
per-probe, not global: `stdbuf` for ordering questions on a plain build, never on a
sanitized one.

A second, quieter one in the same family: under `detect_leaks=1`, LeakSanitizer's
exit path skips the stdio flush, so a leaking fixture's stdout arrives **empty**. A
harness that asserts on stdout will read that as a mismatch, or — worse — as a pass
if the expectation is also empty.

Both share the shape the earlier entries in this section have: the instrument and
the subject interact, and the interaction is invisible unless someone runs the
composed command and looks at what came back rather than at the exit code. The
general defence is the one Core #13 already states — demonstrate the probe going
RED on a known-bad input in the exact configuration you will use it — which catches
all three of these mechanically, without anyone having to know about them.

## 12. The two-clean-pass convergence gate (owner 2026-08-11)

The original gauntlet terminal condition — keep launching fresh reviewers until one
raises *no reservations at all* — was written for reviewers that ran dry. Stronger
reviewer models do not run dry: they reliably produce minor findings forever, partly
structurally (every fold shifts line anchors, which mints fresh citation nits for the
next pass), so the gate risked becoming unreachable. A gate that cannot close is a
broken gate.

The replacement, proposed by the owner mid-R41 and adopted with three refinements:
**two successive full-mandate passes with zero BLOCKING reservations.** The trade is
deliberate and asymmetric. On the dimension that matters — design defects — the new
gate is *stricter* than the old one: the old rule accepted a single clean pass as
terminal; the new one requires two independent derivations agreeing the design is
sound, which also guards against one weak or lucky reviewer. On the cheap dimension —
precision nits — it relaxes, and the executor absorbs the residue (every brief already
mandates re-verifying anchors at HEAD).

The refinements exist because each closes a measured hole:

1. **Blocking is defined, not vibes** — anything that would make the executor produce
   wrong code, fixtures, or records, or that violates a Core invariant. The
   classification burden this shifts onto reviewers is real: in R41, one pass filed
   "author `do:` deleted at arm position" as a *minor* when it changes accept/reject.
   Hence the orchestrator cross-check with PROMOTION power, and a promotion resets the
   streak.
2. **Terminal minors land as a marked errata addendum, never a weave.** The sequential
   rule's core insight survives: a fold can leave a stale remnant, and only a next pass
   catches it. Weaving unreviewed fixes into reviewed text would reintroduce exactly
   that; an appended errata block leaves the reviewed body intact and tells the
   executor what is spec-with-re-verify.
3. **The ≥3 floor and the no-upper-bound stance stay.** Both terminal passes run the
   full mandate — a narrowed "confirmation-only" pass is how rubber stamps start.

The honest counterexample, recorded so nobody rediscovers it as a surprise: the D45
gauntlet's passes 5-7 were all zero-blocking, so this gate would have converged after
pass 6 — and pass 8 found a blocker (a doc comment arguing the reversed doctrine two
lines above a pass-7 rewrite). That blocker was record-accuracy, not design — the
class most likely to also be caught by the executor or the output-review — and the
owner closed that gauntlet by hand at 8 with exactly this gate's logic. The gate
codifies the judgment that was already being exercised; the residual risk it accepts
is the record-grade tail, and the expected cost of the pure-minor passes it retires
was the lowest-value spend in the system.

## 13. Fold verbatim — why the orchestrator's summary is a defect source

The gauntlet's failure mode is not the reviewer missing something. It is the
orchestrator *folding* what the reviewer found.

A review pass returns findings in the reviewer's own words, carrying three things
that look like padding and are not: the **subject** of the claim, the **measured
figures**, and the **cited sites**. Summarising a finding into a shorter sentence
reliably drops at least one of them, and the drop is invisible — the artifact still
reads fluently, the fold looks applied, and the loss surfaces one pass later as a
fresh blocking finding. Measured over one formatter brief that ran thirteen
sequential passes: after the fifth pass, the majority of blocking findings were
defects introduced by folds rather than defects in the work under review.

Three shapes recur, all of them orchestrator-side:

**Compression that loses the subject.** A minor reading "the census prints 26 rows
across 11 files — 21 real hits across 7 files, 2 ledger rows and 3 false positives"
was folded as "state the census's expected shape". The restated version has no
subject: a reader cannot tell which census, and every number is gone. The next pass
raised a blocking finding that the lost figures would have prevented.

**Correction without propagation.** A ruling lands in the paragraph it corrects while
the *enclosing section* goes on stating the old fact. Four consecutive blocking
findings in that same brief were this shape — a corrected clause whose siblings still
contradicted it, including one where a lane-gate list still said "formatter-only" after
the change had been ruled AST-shaped. The countermeasure is mechanical and cheap:
after every fold, re-read the whole enclosing section, then grep the correction in its
*instruction* form (edit-asserts catch a missing fold; only a grep catches a surviving
contradiction — Core #15c).

**Over-correction.** A reviewer says a rule has no subject for some case; the fold
answers "applies everywhere"; the widened rule then swallows cases that were never in
scope. One such fold turned a documentation fix into an instruction that would have
rewritten runtime output, protocol text inside a string literal, and a code sample in
another language.

The fix is structural rather than a resolution to be more careful:

1. **Transcribe verbatim.** Fold each finding in the reviewer's own words. Length is
   not the cost being optimised; a brief exists to be executed exactly once.
2. **Stack folds as marked, numbered addenda** — one per review generation — each with
   an explicit precedence line (later beats earlier beats body). A correction never
   silently rewrites the body, because a silent rewrite is unauditable: no later pass
   can tell what the body used to say.
3. **State the precedence in the artifact**, so an executor reading only the body knows
   it is not the whole spec, and a reviewer can check lower layers for contradictions
   rather than assuming the top layer is complete.

The precedence chain is not bureaucracy. It is what makes fold fidelity *checkable* by
the next fresh pass: the reviewer can diff the addendum against its source report and
confirm, item by item, that nothing mutated in transcription.

### The directive layer is the leak the verbatim rule does not plug (R44, 2026-08-23)

Folding verbatim solves only half the problem, and the surviving half is worse,
because it hides inside the remedy.

An orchestrator's addendum has two jobs that look alike and are not. One is to
**decide** — rule a scope, choose between options the reviewer left open, retract an
earlier instruction. The other is to **restate** a finding so the executor sees it
without reading the appendix. The first cannot be done by quoting; the second must
never be done any other way. Restating is summarising with a different name, and it
carries an aggravating factor a plain summary does not: the directive sits at the
**top of the precedence chain**, so a compressed restatement *outranks* the verbatim
text it compressed. The evidence is present, intact, and overruled.

Three passes in one round found exactly this, each time in a layer written to prevent it:

- A Track A addendum restated a pass-2 finding's six ordering steps as four and dropped
  its verification checkpoint. Pass 3's verdict named the shape: the directive layer
  "is itself a summary layer sitting at the TOP of the precedence chain."
- A Track B addendum kept one half of a two-part finding — "struct-field: liveness
  needed" — and dropped the other half, "collection/param: *no* liveness needed". The
  surviving half then directed a condition the write site cannot evaluate, because the
  relevant liveness belonged to the caller.
- The same Track B addendum restated the defect's subject as a bijection from missing
  fact to user-visible face. The next pass refuted it from the artifact's own exhibit
  and called it "a fifth wrong spelling of the subject, inside the directive whose whole
  job is to stop that."

The rule that follows is mechanical, which is the only kind that survives a weaker
orchestrator: **an addendum may DECIDE, never RESTATE.** Where a directive does not mean
to narrow a finding, it cites instead of paraphrasing — *operative text: pass-N §X,
unchanged* — which costs one line and makes the omission of any other line meaningful.

There is a second-order lesson about *when* stacking stops paying. Track A reached five
layers over an unedited body, and the reviewer's objection was no longer fidelity but
navigability: an executor had to reconcile five layers to answer "what fixtures do I
write?". At that point the correct move is not a sixth layer but consolidation — fold
the corrections into the body at the granularity of each defective clause, demote the
layers to an evidence appendix, and state explicitly **what the old text stopped
saying**. That last step is what keeps consolidation from becoming the silent rewrite
the precedence rule exists to forbid.

## §16 — Gate on design stability, not on clean passes (owner 2026-08-23)

The two-clean-passes gate assumed something that stops being true once reviewers get
strong enough to build: that a pass finding nothing is evidence the artifact is ready.
Measured over one round of six tracks and 52 review passes, it never fired once.

The trajectories tell the story. Blocking counts per pass:

    Track D  9 → 6 → 5 → 4 → 4 → 5 → 3 → 2      converging
    Track F  8 → 6 → 4 → 5 → 1 → 1 → 1 → 1 → 1 → 3
    Track B  3 → 3 → 1 → 3 → 3 → 3 → 4 → 3      flat
    Track E 10 → 5 → 3 → 4 → 3 → 1 → 3 → 5      rising
    Track A  7 → 8 → 6 → 10 → 3 → 4 → 6 → 5     never converged; rebuilt
    Track C  9 → 10 → 6 → 4 → 5 → 6             never converged; split

Track F is the instructive one: it sat at "1 blocking" for five consecutive passes, and
it was a **different genuine blocker every time** — a missing second output of the ported
function, a remedy naming the wrong analysis, an over-rejection measured on a rebuilt
driver, a fused table row hiding two shapes with opposite correct answers. Those are not
a artifact failing to converge. They are an artifact whose *shape* was still moving.

Two things had changed since the gate was written. First, the reviewers had started
**executing**: instrumenting a validator and dumping ownership tags, building two
self-host drivers to compare a gate's cells, applying a prototype and diffing the emitted
C, running a burn-down over the full 2105-fixture corpus. A pass that builds finds what a
pass that reads cannot, so "no findings" stops being reachable while real work remains.
Second — and this is the uncomfortable half — **30 of the 52 passes refuted a prior pass
or an orchestrator decision rather than the source.** A large part of the blocking load
was the review-and-fold apparatus auditing itself.

So zero-blocking measured *depth*, not *readiness*. The replacement measures readiness
directly:

> **Two successive passes with no DESIGN finding.** DESIGN means exactly four things —
> the fix shape, the site set, the scope boundary, the guard mechanism. Everything else
> is DETAIL: citations, stale coordinates, missing table cells, wordings, folds that were
> ordered and not performed. Detail is a minor annotation and the executor's punch list.
> It never holds a track in review.

The gate is falsifiable and was checked against the round that produced it. Applying it
retroactively, Track D passes — its late findings are matrix completeness and a wrong
reason for a surviving conclusion — and Track F does not, because its late findings kept
moving the shape. That matched the independent judgement about which was ready, and gave
it a reason instead of a feeling.

Two cautions the round also earned. A reviewer that fixes what it finds becomes its own
reviewer, and the measured failure rate of built-and-measured fixes here was high: six
consecutive agents on one track each built a remedy and each was partly refuted by the
next — while their *measurements* held up nearly every time. Strong reviewers are
excellent instruments and unreliable architects, which is exactly what deep local context
with no global view produces. And the orchestrator is a defect source in its own right:
every decision layer it adds is new surface for the next pass to audit, which is why the
convention is now that a reviewer's fix block is operative verbatim and the orchestrator
decides scope and routing rather than restating findings.

## §14 — The reference lags the implementation

`docs/language-reference.md` was written **after** the compiler, and has not
caught up everywhere (owner, 2026-08-18). That inverts the usual reading of a
doc-vs-code conflict.

The general guidance — *the code shows what IS, the docs show what's INTENDED* —
holds for `docs/language-design.md`, `docs/book/` and `docs/devbook/`, which were
written to state intent. It does **not** automatically hold for the reference,
which in places records a later author's reconstruction of behaviour rather than
a decision. So a reference-vs-code conflict is an **open question**, not
doc-wins; a load-bearing one is an **owner ask**.

**The measured case.** Track E (the `for … in` element-assignment double free)
needed to know what assigning to a bare loop binding means. The reference said:

- `:1375` — the form table's `for x in coll` row: *"Immutable borrow (collection intact)"*
- `:2919` — *"Bare for-loop iteration creates a read-only borrow"*

The implementation had given it private-copy semantics for years:
`Vector[int]` + `x = x + 10` prints `11/12` then `1/2` — a real, observable
private copy — while `Vector[String]` SIGABRTed, which was the defect under
repair.

Two failure modes showed up around this, both worth recognising:

1. **The brief cited text that did not govern the case.** It forbade the
   reject route by quoting "materializes a private copy" from `:1750`/`:2847` —
   which is about a *subscript bound to a local* (`Vector[int] row = matrix[0]`),
   a different construct. The governing sentences said the opposite. Three review
   passes flagged the citation as unreplaced before one traced what it actually
   said.
2. **The brief then instructed the executor to write the unratified position
   into the docs** — i.e. to ratify a semantics by transcription. The review
   caught that and escalated instead of guessing.

The owner ruled **private copy**: no working program breaks (int already behaves
that way), heap element types are repaired to match, and the reference is
corrected. Split-by-element-type was rejected — the same syntax must not mean two
things depending on whether the element is heap-allocated (Core #15e Q1).

**The rule this yields:** when the reference disagrees with measured behaviour,
neither side is automatically right. Measure the behaviour, read the *governing*
sentence rather than the nearest plausible one, and if the answer is load-bearing,
ask the owner. Then fix whichever artifact was wrong — here, the reference.

## §15 — The fold is the measured bottleneck, so mechanise it

By the end of R43 the gauntlet had stopped finding design defects and started finding
*fold* defects. Track G ran five brief-review passes and its fifth reported: *"Design:
still sound. No mechanism defect in four passes; every finding this pass is a fold
artifact."* The blocking counts across that track — 5, 4, 4, 4 — were not measuring the
design. They were measuring the orchestrator's transcription of the previous pass.

Four distinct pathologies appeared, each caught only by a later pass, and each defeating
the rule written after the one before it.

**(1) Dropping the operative half.** A two-part remedy folds as its first part. The
canonical instance: a reviewer wrote "re-point the citation **and lower
`HEURISTIC_BLIND_CEILING`**"; the fold kept the first clause. The gate stayed red, and the
executor would have been left with roughly thirty further repoints and no instruction
covering them.

**(2) Inverting a conditional.** A remedy offered a disjunction — *update the allowlist row
if the citation is still blind, **or** delete it and lower the ceiling if it resolves*. The
fold wrote "do both". Measured, both branches were wrong: the row had to be UPDATED and the
ceiling had to STAY, because lowering it with five rows present trips a different assert.
An unconditional instruction is not a safe over-approximation of a conditional one.

**(3) Inverting a remedy outright.** A reviewer wrote that `Ptr`-only was the conservative
reading and that widening to `MutPtr` was a separate, measured change. The fold mandated
the widening as required. Both premises the fold invented were false, and executing it
would have skipped a chokepoint at five return boundaries — on the very axis a filed
double-free had lived on. **A subject-level check cannot see this class**: the disposition
is about the right topic and says the opposite thing.

**(4) Retyping a literal command.** A remedy said to restore a grep *verbatim*. The fold
retyped `element per line` as `one per line`. The real command returns five hits; the
retyped one returns three, omitting precisely the site the finding existed to catch. A
retyped literal is the most dangerous of the four because it looks authoritative and fails
silently — the executor runs it, fixes what it finds, reports a total, and ships the defect.

Three successive self-imposed rules failed against these: "fold verbatim, never
summarised", then "quote the remedy", then "quote the remedy paragraph specifically". Each
degraded in practice into quoting the *finding* — the paragraph describing the problem —
and then paraphrasing the fix anyway. The lesson is Core #15 turned on the orchestrator:
**judgement about what is operative is exactly the faculty that keeps failing, so the fix
is to stop exercising it.**

### The rules that work, because they are mechanical

- **(a) Paste the remedy verbatim AS the instruction, then add an explicit `DELTA:` line**
  stating what changes. Never paraphrase; never quote the finding in place of the remedy.
- **(b) Re-run every literal command a remedy contains** and paste the real output. Never
  retype one.
- **(c) Check each disposition against the SIGN of its source remedy**, not just its
  subject — pathology (3) is invisible to a subject check.
- **(d) On a REWRITE, hunt artifact loss.** Rewriting a brief is legitimate when the
  precedence stack itself has become the defect: R43 rewrote two briefs from 1,868 and
  1,707 lines down to 290 and 238 after their blocking counts stopped falling. But a
  rewrite has its own failure mode, and it is not the fold's. **Conclusions survive;
  enumerations, paths and deliverables vanish.** Both R43 rewrites were measured with a
  script over backticked identifiers: **70 and 104 dropped referents**, including the
  prototype patch path, the probe files the executor had been told to reproduce, the name
  of the guard function, and the name of the known-RED test. Every reviewer's first job
  after a rewrite is hunting for what it dropped — and the mechanical check finds more than
  a reader does. Run it before the reviewer does, and land the survivors as an explicit
  **ARTIFACT REGISTRY** section rather than trusting prose to carry them.

- **(e) Before writing the instruction, diff it against the remedy CLAUSE BY CLAUSE and
  list what the remedy said that your instruction does not.** Write that list down — an
  explicit `ORPHAN CLAUSES:` line, even when it reads `none`. This is the rule that
  subsumes (a): dropping half a remedy is invisible while you are writing, because the half
  you kept reads complete. It only becomes visible when the two texts are set side by side
  and the leftovers are named. Measured: a reviewer who applied this to one round's fold
  found it would have caught **all five** of that pass's blocking findings, where rules
  (a)-(d) between them caught none of the five.

A worked failure that shows why paraphrase is never safe, even when it is faithful in
spirit. A scout specified a corpus guard as *"the multiset of ROW SIGNATURES must survive
`gg fmt`"*, restricted to comma-bearing containers. The fold rewrote that as *"the number of
author-authored rows in the source equals the number of rows in the formatted output"* — a
reasonable-sounding restatement that preserves the subject, the direction and the intent.
It is also wrong in a way that matters: a container regrouped from two rows of two into one
row of one and one of three has the **same row count** and a **different signature
multiset**. The rewritten guard was measured GREEN on exactly the class it existed to
retire, while the scout's original exits non-zero on that pair. Pasting the sentence would
have cost nothing; rewriting it produced a guard that could not catch its own class — the
Core #15e Q2 failure, introduced by the fold rather than by the design.

### Grep the shortest distinctive token, never the sentence

Core #15(c) says a fold is not finished until you have grepped for the thing it
corrects, in its *instruction* form. The refinement is about the **key**: prose
in this tree wraps and gets re-flowed, so a multi-word key can straddle a line
break and match nothing while the contradiction sits there in plain sight.
Measured on one fold: grepping `'detection SUCCEEDS'` returned **no hits** and
the fold was declared clean; grepping the single token `'SUCCEEDS'` found the
surviving sentence immediately. Pick the shortest token that is distinctive to
the correction — one word is usually enough, and a word cannot wrap.

### The rules were not enough, so the fold became its own role

Five mechanical rules did not stop the bleeding. The seventh review pass on one track
produced the diagnosis that mattered, and it was not about any rule's content:

> *"All four `ORPHAN CLAUSES: none` claims are false, and the disposition carrying ten
> items has no orphan line at all. **The rule was applied to the four cheap dispositions
> and skipped on the expensive one.**"*

That is a resource-allocation failure, not a knowledge failure. The orchestrator knew the
rule, wrote the rule, and skipped it precisely where the transcription was longest — which
is exactly where the findings were densest. Across one round, roughly ten folds were done
as a side-task by an agent simultaneously running five tracks, and **every one introduced a
defect**, in a round where the underlying designs kept passing review untouched. One track
reached seven passes with no mechanism defect ever found; every finding in it was a fold
artifact.

The structural fix is to stop treating folding as something the orchestrator does between
other things. **The fold is its own role, with its own agent and its own clean context.**
Its brief is a seven-step procedure with no judgement calls: enumerate the findings and
write the counts down; concatenate the report verbatim; emit one fixed-shape disposition per
finding (`PASTE:` the remedy, `DELTA:` the instruction, `ORPHAN CLAUSES:` the leftovers);
verify the orphan list clause by clause with explicit instruction to spend the MOST care on
the LONGEST disposition; re-run every literal rather than retyping it; count the
dispositions and check the number; and use the report's own numbering rather than a hybrid
of two passes'. Anything requiring a decision the report leaves open is emitted as
`ESCALATE:` rather than resolved.

The agent's value is precisely that it has nothing else to do.

**And the role's first outing produced the rule that finishes the set.** The dedicated fold
agent — whose entire job is to not retype literals — retyped two of them while drafting: a
duplicated line in one captured block, mangled em-dashes and a mangled `4×4` in another. It
caught both, and how it caught them is the point. It did not catch them by re-reading its own
paste. It caught them by *diffing the paste against a freshly-captured file*, then repairing
by splicing the real output in through an asserting script.

**Reading back is not verification; diffing is.** A human or a model re-reading its own
transcription is checking the transcription against its memory of the source, which is the
same faculty that produced the error. The only sound check is mechanical: capture the source
again, diff, and assert. That the failure recurred under the role created specifically to
prevent it is the strongest available evidence that no amount of care substitutes for a diff.

The same outing showed what the mechanical shape buys. Its verification was not an assertion
of completeness but a set of commands: `grep -c` on the disposition headings matching the
report's own verdict counts; every `PASTE` confirmed by script to be a verbatim substring of
the report; the whole report confirmed present by containment test rather than by eye; the
pre-fold brief confirmed byte-unchanged by diff. It also recovered five earlier-pass minors
that were about to be dropped for the third time, and flagged three cited line numbers that
did not reproduce — one of which pointed at unrelated text, so editing it blindly would have
corrupted a different sentence.

 An orchestrator holding five
tracks will always, under pressure, apply a careful procedure to the cheap items and
approximate on the expensive one — and approximation is the entire failure mode. Giving the
job to an agent with a single task and no competing context removes the pressure that
produces the defect, rather than adding another rule the same pressure will bypass.

### Cross the axes, or the net guards half the fix

Core #12 says a fixture set that samples one value of a typed axis is an anecdote. There is
a second-order version of that rule, and in one round it independently defeated two
otherwise-careful fixture nets, both written by executors who had RED-verified every cell
they wrote.

**Sampling each axis is not the same as sampling the product.** A net can cover axis A at
several values, cover axis B at several values, and still never test a single cell where a
non-default A meets a non-default B. Every fixture passes, every cell is genuinely red
before the fix and green after, and the net is still blind to half the change.

Two measured instances, same round, different subsystems:

- A formatter net had a **grouping-paren** axis and an **interior-comment** axis and never
  crossed them. A comment sitting between a node and its own grouping paren was therefore
  untested — and that is precisely the shape on which the shipped build aborted the compiler
  with `exit 101`, on a program HEAD formatted correctly.
- A lowering net had a **borrow-source** axis and an **element-position** axis and never
  crossed them: every fixture put the borrow at position 0, and every multi-element literal
  used static literals after it. Half the new function's call sites were consequently
  unguarded. The reviewer proved it mechanically rather than by argument — **deleting only
  the later-position call sites left all eight new fixtures, the new guard and 664 further
  tests green**, while the same build ICE'd on a borrow in position 2 and returned a raw
  address for a two-element literal.

The mechanical test for net adequacy is that second measurement, and it generalises:
**delete part of your fix and see whether the net notices.** A net that stays green while
half the implementation is removed is not a net; it is a description of the paths you
happened to think of. This is Core #13's "demonstrate a red" applied to coverage rather
than to gates — and unlike a review it can be run by the author, before anyone else sees
the diff.

**The rule.** When a fix has two or more typed axes, enumerate the PRODUCT and name every
cell you are not covering. When the fix introduces a new function with several call sites,
partition the call sites by which axis value reaches them, and require at least one fixture
per partition. Then delete each partition in turn and confirm the net goes red.

### A hit count is never the criterion in an artifact that quotes itself

Corrections in a long-lived brief get verified with a grep: *"this phrase must no longer
appear"*, or *"this grep must return N"*. In an artifact that embeds its own review history
verbatim — which is the discipline that stops findings being lost — that criterion is
guaranteed to rot, and it rots in the direction that reads like success.

Measured across one fold on one track: a plain grep for a struck phrase went from **5 hits to
23**, and a wrap-tolerant one from **9 to 27**, while the two actual INSTRUCTION sites — one
marked, one still live — **did not move at all.** Every new hit was a quotation, arriving
because the fold had faithfully embedded a report that discussed the phrase. The number grew
by a factor of four and carried exactly zero information about whether the defect was fixed.

The same round produced four distinct ways a verification grep returns a false green, and it
is worth having the list in one place, because each was found the hard way:

- **Line-wrapping.** A phrase split across two source lines returns zero hits, which reads as
  clean. One such grep was a correction's own success criterion, reporting PASS while failing.
- **Quotation versus instruction.** Hits accumulate in embedded evidence while the live
  instruction stands. Five quotation hits read as "handled"; zero at least looks anomalous.
- **Inflection and case.** A key on `NARROWER` cannot see `narrowed` or `narrowing` — which
  survived in the same bullet, including its bold headline, in a repo file.
- **Code fences.** Lines inside a fenced block that begin `## ` inflate a `grep -c '^## '`
  count — again, wrong in the direction that looks like success.

**⚠ And the defences do NOT compose naively — composing two of them can manufacture a THIRD
false green.** A fold agent, hardening a probe against both the markup form and the
code-fence form, stripped markup first and then detected fences on the stripped copy. But
stripping markup removes backticks, so fence detection on that copy reports **everything as
unfenced**. It caught the error only because it re-ran the check on the original and found
its own written claim ("four inside code fences") was wrong — the real answer was three — and
then corrected its own text with an asserting script. **Apply each defence to the ORIGINAL
text and intersect the results; never chain transformations and probe the last one.**

**The rule.** The criterion is **instruction-site classification**, never a count and never an
absence. Enumerate the sites where the text functions as an INSTRUCTION, classify each as
live or marked, and assert on that classification. A count may be reported as colour; it may
never be the gate.

And the corollary, learned when a fold agent read the source of a probe a reviewer had handed
it and found it neither case-tolerant nor portable: **verify-the-verifier applies to
instruments you are given, not only to gates you run.** Read a probe before trusting its
silence, and state the probe form you used alongside the result.

The deeper reading: a brief accumulates two different kinds of content. **Judgements**
compress well and survive rewriting. **Artifacts** — paths, identifiers, commands, counts,
file names — do not compress at all, and every generation of editing sheds them. Keep them
in a section that is appended to and never rewritten.

## §17 — The gauntlet verifies work; it does not defer it (owner 2026-08-27)

**Why the STRICT 2× convergence rule was removed (owner 2026-08-23).** It was
failed repeatedly, and the rounds that failed it were the ones doing the most
valuable work: a round that finds nine real defects, three of them
memory-safety, is a *good* round that the ratio scored as failing. Measuring
inflow stayed useful; gating on it selected against discovery. So
`scripts/convergence.sh` still runs and its line is still quoted in every
`DONE.md` entry — as a measurement, with no ratio to meet and no power to hold
a round open.

**What replaced it, and how that ruling was then misread.** The surviving
ruling was *fix inline unless the defect is really disjoint*. Within four
rounds it had decayed into the opposite practice, and R45 is the measurement:

| R45 at the point the owner intervened | |
|---|---|
| commits on the integration branch | 85 |
| commits touching `src/` | **0** |
| `todo/` items filed | 28 (6 CRITICAL) |
| brief-review passes run | A at 16, D at 12, F at 3 |
| executors launched | **0** |

Every one of those 85 commits was paperwork: filings, fixtures, briefs, doc
folds, lint ratchets. The round found a great deal and fixed none of it.

**The two misreadings, both the orchestrator's.**

*First, "really disjoint" was read as "a different site".* `t0707`
(`materialize_addressable` hardcodes `None` for the liveness half) was filed as
disjoint from Track D because it is a different call site — when it is the same
helper, the same axis, and the same fix. **Disjoint means a different CLASS, not
a different file, function, or call site.** A sibling site inside an enumerated
class is precisely Core #4's subject: fix the class, do not file the sibling.

*Second, and worse, the gauntlet was blamed for the deferral.* The orchestrator
reported that "the gauntlet is the problem — reviewing finds bugs faster than
executing fixes them". The owner's correction: **the gauntlet exists to double-
and triple-check work with fresh agents so one pass cannot quietly break
something. It is not a queue for handing fixes to the next agent.** A review
pass that surfaces a defect has done its job; the round that surfaced it still
owes the fix. Nothing in the review discipline ever said otherwise — the
deferral was a choice made underneath it and then attributed to it.

**The tell.** A round whose commit log never touches `src/` is not a
discovery round; it is a round that stopped. Check it before round close, not
after: `git log --oneline <round-open>..HEAD -- src/ | wc -l`.

## §18 — The orchestrator does not touch the code (owner 2026-08-28)

**The ruling.** *"Why is the orchestrator proposing fixes at all? That is the
brief review agent's job. On each pass. The orchestrator should only verify the
streak count, update the brief (by deciding when to incorporate findings to the
brief scope or file them for later if disjoint) and possibly coordinate parallel
tracks so they do not step on each other."* And: *"This was obvious for
Opus < 5, the orchestrator does not normally touch the code. It launches tracks
that do."*

**The boundary, sharpened the same day.** Asked whether the orchestrator may
apply a fix itself once it has cross-checked a reviewer's claim, the owner was
explicit: *"Exactly, you launch a fresh agent to do it. At most, you take part
in criticizing the final form."* So the orchestrator's participation in a
track's OUTPUT is **criticism only**. The standing duty to cross-check a
reviewer (a reviewer can be wrong) is untouched — what changes is what happens
next: the cross-check produces a FINDING returned to the agent, never an edit
the orchestrator makes. That is rule 0's fourth duty in `AGENTS.md`.

**How the rule got broken.** R45's owner instruction *"recursively fix the bugs
we find on each track, unless really disjoint"* names an obligation of the
ROUND, not of the orchestrator's hands. It was read as the latter. The
orchestrator then hand-edited `src/` between review passes — nine commits'
worth.

**Why that destroys the convergence gate.** Each pass is supposed to review the
BRIEF. Once the orchestrator edits the tree between passes, each pass instead
reviews *the orchestrator's newest code*. Track D's record is unambiguous:

| pass | finding | subject |
|---|---|---|
| 13 | `walk_stmt` catch-all | the codebase |
| 14 | `on error:` regression | the fix from pass 13 |
| 15 | half-fix, loop kill-leak, MetaFor | the fix from pass 14 |
| 16 | seed leaks kills; net certifies uncovered arms | the fix from pass 15 |
| 17 | `Stmt`-exhaustive, `Expr`-blind | the fix from pass 16 |
| 18 | five of six expression positions | the fix from pass 17 |

Six resets; every pass reported the brief itself sound (*"brief technically
sound, paperwork stale"*, *"§2 cites re-derive"*). **Track D was ready at pass
12.** The gate was scoring the orchestrator's inline fixes as the track's
readiness, and three of those fixes were regressions in a walker touched four
times.

**What "fix inline" actually means (owner clarification, same day).** *"The
'fix inline' means the track should fix the findings it discovers on each pass,
not file infinite new todo items. Let the scope grow a bit."*

So the default disposition for a finding is **incorporate into the track's own
scope**, not file. Filing is for the genuinely disjoint. R45 got this wrong in
both directions at once: the orchestrator hand-fixed some findings (wrong actor)
and filed 28 `todo/` items (wrong disposition), while the tracks' own scopes
stayed frozen and their executors never ran. A track whose scope grows by a few
findings per pass and then ships once is cheaper than a track that ships nothing
and leaves a filing trail behind it — and the filings are worse than neutral,
because each one reads as discovered-and-handled when the defect is still live.

**The reviewers were already doing their job.** Every fix existed as a reviewer
patch first — `PROTOTYPE_on_error_seed.patch` (D15),
`recover_revD18_matcharm_route.patch` (D18),
`recover_revA21_RECOMMENDED_tightened_plus_two_rows.patch` (A21),
`recover_revG_spine.patch` (G's output-review). The correct handling was to fold
each into brief scope and let ONE executor implement the accumulated set under
ONE output-review. Applying them by hand, one per pass, manufactured fresh
unreviewed code for the next pass to find.

## §19 — Readiness is a property of the artifact, not of reviewer yield

**Why the design-stability gate stopped terminating.** "Two successive passes
with no DESIGN finding" is defined over *what reviewers happen to find*, so a
stronger reviewer makes it strictly harder to pass. The artifact did not get
worse when the harness moved to Opus 5; the detector got better. A gate that
punishes a better detector is measuring the wrong thing. Measured: ~40 passes
across Tracks A and D without firing once.

**The replacement is a finite checklist**, binary and verifiable without
judgement. Every row was earned by a defect that actually shipped past the old
gate this round:

| # | check | the failure that earned it |
|---|---|---|
| 1 | every measurement carries a FIRE COUNT proving the mechanism executed | three inert probes — `DEC-A16-3`, `DEC-A16-4`, and a D16 probe using a local where an owning `!`-param was required |
| 2 | every enumeration cites an INDEPENDENT witness | A18 corrected A16's selection and presented its own; the repo's own lint table (12 formation sites) settled it |
| 3 | `\|pinned cells\| == \|changed cells\|` | A21: the net pinned 2 of 5 changed cells; partially reverting the fix left every row green |
| 4 | the GUARD FAILS when the fix is reverted | A21's M2 — fix A's `register_local` in a dead branch, and `lints`/`security`/`box` all green |
| 5 | every load-bearing figure REGENERATED at current HEAD | polarity numbers taken off a failed run; clone figures that moved three times in one day |

**Scoping is what makes it terminate.** A finding resets the streak only when it
changes the brief's fix shape, site set, scope boundary, or guard mechanism.
A defect found in the TREE while reviewing does not reset the streak — it says
nothing about whether this brief is executable. A defect in the orchestrator's
fold or guard is an orchestrator defect, not the track's. **Neither clause
prescribes a DISPOSITION:** what then happens to the find belongs to
`AGENTS.md` § Multi-agent orchestration rule 0, which incorporates into the
track's scope by default and files only the genuinely disjoint.

**The residual, stated honestly:** a checklist can miss a novel defect class a
sharp reviewer would catch. Mitigation is that ≥3 passes stays the floor and any
new class that slips through is added as a further row — the list earns its rows
from failures, exactly as these five did.

## §20 — The process apparatus outgrew the file it lived in (owner 2026-08-29)

`AGENTS.md` doubled between mid-June and late August — 28,901 bytes on
2026-06-15, 59,271 on 2026-08-28 — and the owner's report on what that cost is
the finding this section exists to record: *"each round spins on each track never
reaching the executor step. We waste entire weekly token quotas without any code
written."*

### The growth was entirely process, and the technical content was stable

Measured by section, mid-June against the pre-compaction file:

| Section | 2026-06-15 | 2026-08-28 | Δ |
|---|---|---|---|
| Core invariants | 1,867 | 13,882 | +12,015 |
| Round lifecycle | 0 | 6,756 | +6,756 |
| Review … fresh agent | 4,793 | 10,543 | +5,750 |
| Multi-agent orchestration | 2,209 | 5,554 | +3,345 |
| Task Continuity | 1,472 | 3,854 | +2,382 |
| Layering · CoW · Build&Test · Structure | 13,893 | 13,772 | −121 |

The compiler knowledge the file exists to carry did not grow at all. The
excellence system grew 4.7×. Regenerate the table with
`git show <rev>:./AGENTS.md` and a per-`##`-section byte count — never quote
these figures, they are a snapshot of two revisions.

### Why the gauntlet stopped terminating

Four rules formed a loop that no amount of reviewer diligence could exit:

1. *"≥3 passes is the FLOOR; there is NO upper bound … never invent a cap"* —
   no terminating condition.
2. The reviewer's mandate to block on DESIGN grounds — *"a blocking reservation
   even when the code works and every premise checks out"* — evaluated against
   15 invariants carrying roughly 50 sub-clauses. A conscientious reviewer finds
   something essentially every pass.
3. *"FIX INLINE unless really disjoint"* grows the brief's scope with each
   finding.
4. The convergence gate resets the streak when a finding *"changes the … scope
   boundary"* — which (3) guarantees it does.

So the clause written to make it terminate (§19's SCOPE MAKES IT TERMINATE) was
fed by the clause above it. Add `FOLD VERBATIM`'s monotonically growing addenda
stack — each pass reviewing a longer artifact than the last — and rule 0's
prohibition on the orchestrator touching anything, and every finding cost a full
agent round-trip while the thing under review kept getting bigger.

**`FOLD VERBATIM` is not the defect and does not get relaxed.** It exists
because summarising a fold introduced errors (§13); the owner reaffirmed it
during this compaction. What changed is only who may apply a *mechanical* fix.

### What the compaction did

`AGENTS.md` 59,271 → 46,956 bytes with the rule set intact: 464 → 442 pinned
clauses, 94 → 85 non-normative rows.

- **Retired: 18 `META-*` rows** describing `AGENTS.md`'s own lint machinery —
  what the four guards do and do not catch. This was the file's *third* copy:
  the mechanics are in `tests/lints.rs` doc comments and the per-site-class rates
  are measured in `todo/t0714`. A file loaded into every context window was
  spending 2.5 KB explaining its own test.
- **Retired: 5 `REV-design-*` rows** that restated Core #1, #2, #4, #8 and #10
  verbatim inside the reviewer's checklist. Restating an invariant next to a
  pointer to it is how the file doubled.
- **Retired: 9 non-normative rows** — owner-attribution parentheticals and
  rationale clauses whose evidence already lives here and in devbook/29.
- **Added: 1 rule.** `MA-0-mech` — the orchestrator MAY fix a typo, a stale
  figure or a one-line correction in place when it changes no behaviour and no
  design. Rule 0's absolute prohibition was costing an agent round-trip per
  trivium, which is the executor-starvation the owner reported.
- Everything else lost its justification prose, not its imperative.

### The prior floor estimate was wrong

This lint comment carried, from 2026-08-04 to this compaction, the claim that
*"~58,000 is the FLOOR for the current rule set, not a way-station"*. It was
refuted by landing at 46,956 with 442 of the 464 clauses still pinned. The error
was measuring the prose the rules were wrapped in and calling it the rules.

The real floor is the pinned text itself, and it is regenerable rather than
quotable:

```text
AGENTS_MD_DUMP=1 cargo test --test lints agents_md_measurements -- --nocapture
```

Cutting materially below that does mean deleting rules, which needs the same
owner sign-off as raising the byte ceiling — the two ratchets exist to make
exactly that trade visible in review rather than cheap in a compaction.

### The lesson

A guard that caps BYTES pushes an author to scavenge prose; it does not stop the
RULE COUNT from growing, and the rule count is what a reviewer must check a brief
against. The excellence system's own inflow needs the discipline it imposes on
the compiler: a new process rule should retire an old one, or state why the pair
is not a class (Core #4 turned on the ledger itself). Watch for the same shape —
`AGENTS_MD_RULE_FLOOR` only ever ratcheted UP before this round.

### The correction that actually terminates it (owner 2026-08-29, same day)

The first landing of this compaction misread the owner's ruling and wrote rule
0's carve-out as *"the orchestrator may fix a finding in place"* — a
track-lifecycle shortcut. That is wrong twice over, and the owner corrected it:

> Each track is responsible for fixing whatever bugs, typos it finds, not the
> orchestrator. The orchestrator can fix small typos, simple stuff but normally
> not on behalf of a track lifecycle. The orchestrator can fix things
> proactively or in demand by the owner, though.

So `MA-0-mech` is about the orchestrator's OWN hands — proactive work, or work
the owner asked for — and `MA-0-notrack` states the boundary it must not cross.
Inside a track, the TRACK fixes what its passes find, through its executor.

**The real fix was to the reset criterion, not to who holds the pen.** The
owner's statement of purpose:

> What we are trying to achieve with the gauntlet is a sign off of the general
> design. To ensure it makes sense and is aligned with the project's objectives.
> The executor is still responsible for coming up with the solution which is then
> itself reviewed again by the post-execution review pass. What I don't want
> (which has been happening in the past few rounds) is that each brief review
> pass finds more bugs and files them, never reaching the execution step.
> Instead, the track scope should grow to fix those found bugs … include them in
> the brief for the executor to fix. But please let it reach the executor phase!

The livelock was one clause. §19 said a finding resets the streak when it
changes *"the brief's fix shape · site set · **scope boundary** · guard
mechanism"* — and absorbing a discovered bug into the track's scope IS a
scope-boundary change. So the rule written to make the gauntlet terminate was
the rule that kept it spinning: every found bug either got filed for a later
round (the anti-pattern) or grew the scope and reset the streak (the livelock).
Neither path reached the executor.

The criterion is now about the DESIGN, and the two outcomes are disjoint:

- **Resets the streak:** the finding invalidates the design — wrong root cause,
  wrong layer, a Core invariant fought (`REV-resetdesign`, `REV-invalid`).
- **Does NOT reset the streak:** the finding is more WORK inside a sound design —
  another sibling site, a nearby bug, a typo. It GROWS THE TRACK'S SCOPE and
  ships to the executor in the brief (`REV-growscope`), and it is never filed as
  a `todo/` item for a later round (`REV-nofile`).

`REV-launch` states the terminating condition positively — launch as soon as a
fresh pass signs off the DESIGN — and `REV-object` names what the gauntlet is
FOR, so a reviewer hunting bugs knows the bugs go in the brief rather than into
another pass. `REV-nostreak` ("an off-checklist find does not reset the streak")
was retired as subsumed: `REV-resetdesign`'s ONLY says it, and more precisely.

The general lesson is Core #4 turned on the process ledger. A termination rule
whose predicate mentions SCOPE cannot terminate a loop whose findings grow
scope. The predicate has to name the thing that genuinely invalidates the
artifact — here, the design — and everything else has to have somewhere to go
that is neither another pass nor a later round. That somewhere is the executor.

### The gate is now capped, not ratcheted (owner 2026-08-29)

§19's residual paragraph offered its own growth term as the mitigation: *"any
new class that slips through is added as a further row — the list earns its rows
from failures, exactly as these five did."* That is the right instinct about
provenance and the wrong instinct about cost.

Rows are a **pre-launch bar**, and each is per-track work forever. Row 4 means
actually reverting the fix and running the guard; row 1 means instrumenting for
fire counts. So a row bought by one incident becomes a permanent tax on every
future track — the same shape as the byte bloat `agents_md_size_ratchet` fights,
one level up, and the same shape as `AGENTS_MD_RULE_FLOOR`, which had only ever
ratcheted UP before §20.

The clause is now a CAP: **a new class RETIRES a row or becomes an executable
guard**, never a sixth row. `readiness_checklist_rows_are_capped` enforces it,
because the clause saying so is itself prose (Core #6 applied to the gate that
enforces Core #6). It asserts two things — at most five rows, and that the
markers are a contiguous `1..N` run, so a retired row is renumbered rather than
left as a hole a reader would hunt for. Both were RED-demonstrated on the landed
file.

Note what the cap does NOT claim: nothing here says five is the correct number.
It says a sixth is a decision with a named cost, rather than a drift. Retiring a
row stays free, and a silent deletion is still caught, because the rows are
individually pinned as `REV-ck1`..`REV-ck5`.

The standing alternative — mechanise the rows so they LEAVE the checklist, which
reduces the per-track cost instead of merely freezing it — is filed as
`todo/t0726` with the three decidable rows (3, 4, 5) named and rows 1 and 2
argued as the ones that probably stay human.

### Excellence pass: what the first compaction got wrong (owner 2026-08-29)

Asked whether the compacted file was good, the honest answer was "partly", and
the owner's reply was *"Make it good. Make it excellent, in fact. You may bring
back the intent justification if you think it is the right thing to do."* What
that licensed, and what it did not, is worth recording.

**The compaction cut the WHY along with the war-story, and those are different
things.** A war-story ("Snag #17 corrupted a later `parse_float`") belongs here.
A one-clause reason ("Prose rots; guards don't") belongs *with the rule*, because
it is what makes the rule survive contact with a clever agent. A rule stripped to
its bare imperative gets followed literally by a weaker reader and rationalised
around by a stronger one. Seven clauses came back, each attached to the rule it
explains, each now PINNED so the next compaction cannot strip them silently:

| row | the rule it keeps alive |
|---|---|
| `WHY-verbatim` | a summarised fold introduces errors of its own — the rule a capable agent is most tempted to optimise away |
| `WHY-core6` | prose rots; guards don't |
| `WHY-core12` | a fixture green before and after is worse than none — it reads as coverage |
| `WHY-core14` | an unenforced assertion misleads the reader who trusts it |
| `WHY-ma1` | an agent loose in the main worktree sweeps the parent's uncommitted work into limbo |
| `WHY-nopack` | a reused context anchors on its own prior conclusions |
| `WHY-scout` | this tree's most expensive mistakes were briefs built on stale premises |

This is the one ceiling RAISE in the file's history that bought something other
than rules (47,400 → 49,200).

**Three structural defects the compaction exposed rather than caused.**

1. **Core #15's sub-list ran (a) (b) (c-bis) (c) (d).** `c-bis` had been inserted
   above the clause it extends, and no guard sees ordering. Renumbered to
   (a)–(e) in dependency order: fold at the right granularity, *then* grep for
   surviving contradictions.
2. **The gauntlet's purpose was the seventh paragraph of its own section.** A
   reader met *"until a fresh pass raises no reservations"* six paragraphs before
   meeting the frame that bounds it. Ordering is not cosmetic when the buried
   clause is the terminating condition — moved to second.
3. **The section heading was a sentence** duplicating the four-artifact list in
   its own body. Now `## Review with a fresh agent — the gauntlet`; the
   `(→ Review … fresh agent)` cross-references still read correctly.

**On audience, the split was considered and REJECTED.** Roughly half the file
binds only the orchestrator, and subagents pay for it in tokens — a real cost the
owner named. The obvious fix is a second rules file. It is wrong, because the
auto-loaded file is loaded by the MAIN session, and the main session is the
orchestrator. Splitting would put the rules their primary reader needs into the
file they do not automatically get, trading a token cost for a correctness risk:
process discipline that silently vanishes when someone forgets to read file two.
Subagents receive briefs, and the brief is their real contract. What landed
instead is cheap and safe: a **reader's map** naming who each part binds
(`MAP-all`, `MAP-orch`, `MAP-core`), and a reordering so § Task Continuity closes
the everyone-material and the three orchestrator sections form one contiguous
block at the end.

**What was deliberately NOT done.** A mechanical down-casing of the file's
capitals. 386 caps tokens is 5.3% of the words, which reads as shouting — but 232
of them sit inside pinned probes, and most mark genuine polarity (NEVER, ONLY,
EVERY) that a skimming reader must not miss. Churning ~100 probes to restyle
emphasis is a large reviewed diff for a small aesthetic gain, and the guard
correctly makes that expensive. Recorded here as a considered no, so the next
reader does not re-derive it.

### The round's shape was never stated in one place (owner 2026-08-29)

Asked whether the file made three things clear — a round has multiple parallel
tracks, each track runs the gauntlet, code changes come from the executor — the
audit found **one of three**.

*Each track runs the gauntlet* was clear and stated twice ("per track, ≥3
sequential fresh brief-review agents each seeing only that track's brief (N
tracks ⇒ N×≥3 agents)"; "N independent per-track loops").

*Multiple parallel tracks* existed only as PERMISSION — "parallel tracks
welcome", "Multiple items/tracks MAY run IN PARALLEL". A reader looking for the
normal shape of a round found an allowance, not a default.

*Only the executor changes code* was **nowhere stated positively.** It had to be
assembled from three negatives scattered across two sections: the orchestrator
does not touch the code (rule 0), the reviewer proposes but does not implement,
the scout is read-only. An assembled rule is a rule a reader can fail to
assemble — and this one is load-bearing, because the failure mode it prevents is
exactly the executor starvation §20 is about.

Worse, the third fact carried a **flat contradiction**: the scout was described
as *"a read-only probe/audit"* that *"prototypes it end-to-end and MEASURES the
real result"*. Both clauses are correct and they read as opposites. The
resolution was always intended — Multi-agent rule 9 says "checkpoint scout
prototypes to /tmp" — but intent that lives in another section is not a
reconciliation. It now says the scout works *in a throwaway worktree, shipping no
diff*: read-only in what it SHIPS, not in what it may run.

The general lesson: a fact derivable from three separate clauses is not stated.
Core #15e Q4 asks whether a rule's SUBJECT covers the case; the dual is worth
asking too — **is there a load-bearing fact with no rule at all, that every
reader is expected to infer?** Those are invisible to the pin lint by
construction, because a probe can only pin text that exists.
