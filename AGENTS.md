# Gorget Compiler

> **Note:** `CLAUDE.md` is a symlink to this file. Both names are kept so that
> Claude Code and other AI coding agents can discover these instructions.

## Overview

Gorget is a statically typed, Python-like language with Rust-inspired ownership and safety.

**Pipeline:** `.gg` source → lexer → parser → semantic analysis → IR lowering → backend → binary

**Binary:** `gg` with commands: `lex`, `parse`, `check`, `build`, `run`

## Core invariants (read first)

The sections below are the spec; these are the load-bearing rules they reduce to.

**How a rule lands here — a fresh owner ruling included.** It becomes ONE compact imperative in the present tense. Its provenance, measurement and war-story go to [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md) (engineering) or [`docs/devbook/30`](docs/devbook/30-excellence-system.md) (the excellence system); unratified owner open-thinking goes to devbook/30, marked as such. **Sharpening an existing rule EDITS that rule in place** — never appends a second dated copy beside it, which is how this file doubled.

**Four lints hold the line** (mechanics in `tests/lints.rs`): `agents_md_size_ratchet` caps the bytes — ratchets DOWN after each compaction, lowering free, raising needs owner sign-off; `agents_md_rule_inventory_is_pinned` requires every inventoried rule to still be stated, exactly once, and **fires when an inventoried rule is DELETED**; `agents_md_every_clause_is_classified` runs the file's own sentence census, so a new SENTENCE must be pinned or marked non-normative; `agents_md_unpinned_prose_ratchet` caps the GROWTH of what NO probe accounts for: the longest such stretch, and the count over 100.

⚠ **Know what they do NOT give you.** For **INSERTED** text reach depends on WHERE and on LENGTH: inside a probe's or an exemption's span it breaks the pin and is caught; in unpinned prose it is caught only if it starts a NEW sentence or pushes its run past the 100-char floor or the cap — a short mid-sentence one in a run already between them is invisible; on a heading nothing but the byte ceiling, in a fence usually nothing. The census's *contains-a-probe* test is the weak one, its exemption side weaker still — chunks the table asserts carry no obligation. **DELETED** text fires the ratchet only when it moves a run across a BAND EDGE — measured ~half; a deletion that shifts no band is silent. Nor does any lint check that the evidence reached devbook/29-30. Per-class rates + commands: `todo/t0714`. **So the diff is the guard: after editing a rule, re-read the whole rule, not just the clause you changed.** Those halves stay on the author.

**Every excellence-system rule (process, gates, gauntlet shapes, model allocation) is stated in THIS file** — never only in one harness's private memory — so any external agent harness can replicate the system; devbook/30 carries the extended treatment. (Owner 2026-07-18/25.)

1. **Fix at the write site, not the read site.** A complex read-side fix (save/restore, phi repair, per-case rules) means a writer one layer up dropped a typed invariant. (→ Layering discipline)
2. **Typed metadata, never name-matching.** No `name.starts_with("Vector__")` to decide *meaning* — put the flag on the typed decl, set at the source, read via an accessor. (→ Layering discipline)
3. **Register ownership at the value's birth.** Every freshly-materialized owned, droppable value is registered for drop (or provably moved) at the producer; the leak/double-free class is always a missing or mis-typed ownership tag. (→ Ownership at Consuming Positions)
4. **One fix, all siblings.** Fix the enumerated *class* (every consume/dispatch site), not the instance; centralize at the producer; add an arm-count lint. (→ Layering discipline)
5. **Re-verify every premise; regenerate every number.** No dated figure enters a plan/brief/commit/handover unless you regenerated it this session. (→ Solution Quality)
6. **Convert a recurring bug class into an executable guard** (validator or `tests/lints.rs` ratchet: env-gate → burn down → fatal). Prose rots; guards don't. When review passes or successive rounds keep finding ONE class in new syntactic costumes, the round's output owes the class-retiring guard — not just the instance fixes (owner 2026-07-18; type case: devbook/30 §1). (→ `docs/devbook/25-structural-guards.md`)
7. **Gate on the bootstrap and the sanitizer**, not just a green suite — `self_host_bootstrap_fixed_point` + ASan catch what `cargo test` and the always-pass `*_comparison` diagnostics miss. (→ Build & Test)
8. **Reference-grade is the bar, not parity with a possibly-wrong reference.** "Matches Rust gg" / "both backends agree" / "only fails on programs that are UB on both" is *necessary, not sufficient*. If the agreed-on behavior is itself wrong (garbage, crash, miscompile, or wrongly accepted), that is **≥2 bugs to fix in BOTH compilers** — most often by making the language *reject* it (typecheck error + negative fixture). "Benign because both backends are UB" is a **red flag, never a pass**; the final output-review must refuse to ship a known defect. (→ Review … fresh agent)
9. **A SEMANTIC change lands on every lane in the same round — ggdef (within its subset), Rust gg (C+LLVM), and the self-host — pinned by a cross-lane fixture, never by a promise.** Anything that alters accept/reject or what accepted programs do ships with the conformance fixture (or per-lane driver tests) encoding the intended FINAL state; a lagging lane is a red lane or an explicit `#[ignore]`+citation, never a silent gap; out-of-subset shapes get a note + a filed subset gap. Implementation-internal fixes (one backend's codegen) are exempt: lanes share semantics, not implementation. The round does not close with an undocumented lane divergence, and a track that flips fixture expectations carries the FULL ggdef suite (`cargo test -p ggdef`) in its own gates. (Owner 2026-07-16; origin in devbook/30 §1.) **⊕ (owner 2026-08-10):** a round's OWN new fixtures — any corpus growth — must COMPILE + MATCH on the self-host lane the SAME ROUND (port the SH lane, all driver copies). Only PRE-EXISTING non-MATCH are exempt from `RUNTIME_DIFF_NONMATCH_CEILING`; raising it for your OWN inflow is forbidden (that owner-ask is pre-existing inflow ONLY).
10. **Lower-or-reject — never silently drop user syntax.** Every lowering arm either lowers the construct or emits a check-time rejection; a `_ =>` fall-through (or missing arm) that discards a write or expression the user wrote is a miscompile-class defect (devbook/29), not a "not yet supported" comment. Enforcement: the `tests/lints.rs` silent-fallthrough allowlist ratchet (env-gate → burn down → fatal). (Owner 2026-07-18.)
11. **Every fix ships wide, genuinely-exercising regression fixtures, same round.** A bug fix isn't done until fixtures exercise the bug on the *real* path — non-constant operands (so const-fold can't elide it), wired to RUN (not just compile), **one per sibling for a class**, and **wide enough that a partial regression would trip them** (costumes · shapes · POS+NEG · lane pins). A single existing NEG with a thin harness pin is a floor, not the bar. The fixture lands WITH the fix, never "later", on **every** touch. (#6 retires a class; this is the per-fix net.) (Owner reinforce 2026-07-21.)
12. **A regression fixture is not coverage until it has been seen to FAIL — and a fixture set that samples one value of a typed axis is an anecdote, not a net.** (Owner 2026-07-25; measurements in devbook/30 §3.)
    - **RED-verify.** Run every new fixture against the PRE-fix compiler and record the observed failure in the commit or report. A fixture green before *and* after the fix tests nothing and is worse than none — it reads as coverage.
    - **⚠ GREEN ON ARRIVAL IS NOT COVERAGE — RED-verify binds EVERY new fixture, not just bug-fix ones.** A fixture for an already-shipped feature has no pre-fix compiler to run against, so **break the mechanism it claims to guard, confirm RED, restore** (the #15(d) procedure, as an AUTHORING obligation). If neither form of red is possible, say so in the header and state what it pins instead.
    - **A fixture's NAME is a claim about SCOPE — make it true or narrow it.** Record in the header which CELL of which axis it samples.
    - **Axis-complete.** When behaviour depends on a typed axis — field type · receiver/root shape · backend · lane · element type — the net **must cover every value of that axis**, or name each omitted cell and why; sampling one value proves nothing about the others. When you touch a feature, ENUMERATE its axes first, and check what each existing fixture *actually* exercises, not what its name suggests. **Go TYPE-first** when budget forces a partial audit.
13. **Verify the verifier — and pick an instrument that can SEE the failure class.** (Owner 2026-07-25; measurements in devbook/30 §4.)
    - **Demonstrate a red.** Before reporting "gates green" as support for a claim, show at least one gate going RED on a deliberately broken variant. A gate that has never been seen to fail is not evidence (devbook/30 §4).
    - **Ask ggdef FIRST — a TRIAGE instrument, not just a round-close gate — knowing its scope.** Run the shape through the oracle during triage and treat disagreement as the finding (a Core #8 event, not a curiosity). But **ggdef adjudicates VALUE SEMANTICS and is STRUCTURALLY BLIND to memory-invalidation** — it cleanly accepts live heap-UAFs. **ASan on the real backends adjudicates memory validity**; the two are not interchangeable. And ggdef can LAG a ratified decision (a lane gap, not a verdict) — **or simply be WRONG: it IMPLEMENTS the definition, it is not the definition.** Lanes AGREEING on a wrong answer is Core #8's trap, not a pass. **A BOTH-WRONG row is an OWNER ASK only if the semantics are UNRATIFIED**; where the ledger rules, ggdef is just wrong or lagging — **fix it**. (Owner 2026-08-22; devbook/30 §4.)
14. **An invariant-asserting comment needs an enforcing guard, or it gets DELETED.** "This is unreachable", "the only consumer is X", "this clones to prevent aliasing" — either it is a `debug_assert!`/lint/typed guard, or it is rot that will mislead a reader who trusts it. Core #6 applied to prose (devbook/30 §5). When you touch code near such a comment, verify it or delete it — never inherit it.
15. **Make rigor MECHANICAL, not clever — the gauntlet must still work with a weaker reviewer.** (Per-item evidence: devbook/30 §6.)
    - **(a) Every load-bearing claim in a brief carries its VERIFICATION COMMAND** — the literal command that re-checks it, so review becomes *executing a register*. A claim with no command is not a claim, it is a hope.
    - **(b) Scope over a SET → present the SET:** the total enumeration with a disposition per row (LAND / DEFER / NEVER + reason), never a selection — a selection hides its own gaps.
    - **(c-bis) FOLD AT THE GRANULARITY OF THE DEFECT — VERIFY AT THE GRANULARITY OF THE SECTION.** One clause wrong ⇒ edit that clause; replacing the paragraph takes the true material beside it with it. Then re-read the WHOLE enclosing section — heading, both neighbouring paragraphs, and the comments inside its examples — because the corrected sentence's SIBLINGS go on contradicting it. On a genuine rewrite, diff old against new and ask **"what did the old text stop saying"**, not "is the new text right".
    - **(c) Fold a correction → GREP for the thing it corrects**, in its *instruction form* (`old`→`new`), not the bare string — but grep the **SHORTEST DISTINCTIVE TOKEN, never the sentence**: prose WRAPS, so a multi-word key silently misses (devbook/30 §15). Instruction form, because explanatory prose legitimately mentions the old value. Edit-asserts catch a MISSING fold; only a grep catches a SURVIVING CONTRADICTION.
    - **(d) Fixed procedures for the recurring claim types** — a weaker reviewer runs these without judgement: *"fixture F guards mechanism M"* → break M, run F, confirm RED · *"X is filed"* → grep the record · *"there are N sites"* → run the census command and compare · *"shape S behaves B"* → build and run on C AND LLVM, plus ggdef when in-subset. **MIND THE PROBE**: never test accept/reject inside an f-string (a rejected expression reads as ACCEPTED — devbook/30 §10), never read a crash off a PIPELINE (`| tail` masks it; devbook/30 §11) · *"the gates are green"* → make one go RED, once (Core #13) · *"line L says X"* → read L at HEAD.
    - **(e) The SIX QUESTIONS no runbook generates** — (a)-(d) mechanise procedure, not taste; the orchestrator asks these of every brief and every "defect" before acting on it (narratives: devbook/30 §6):
      1. **Is this asymmetry a DEFECT, or two positions with different RATIFIED semantics?** Check the design record before calling an accept/reject asymmetry a bug.
      2. **Can this guard catch its OWN class?** A guard that green-lights the class it was written to retire is worse than none.
      3. **Is this enumeration TOTAL, or a selection?** A selection cannot show you what it omits.
      4. **Does this rule's SUBJECT actually cover the case** — or is there a case with no subject at all (a category error, which no widening of the rule fixes)?
      5. **Am I reasoning about emission, or emission ORDER?** WHEN a thing happens relative to its siblings is load-bearing and only visible in the IR.
      6. **Is this passing case ACCIDENTALLY correct?** A green cell may be green for a reason unrelated to what you think it tests — which is why Core #12 demands RED-verification.
      ⚠ Plus one about the record itself: *is this premise still TRUE, or a filed fact that decayed?* A considered decision in a scratch file outranks a stale one in the ledger, and the fix is to **file it properly**, not to discount it. The honest signal that the process has thinned: reviews that only find compression errors and never a design defect.

Delegated work runs **scout → brief → ≥3 fresh brief-reviews → launch (worktree) → fresh output-review → integrate** (→ Review … with a fresh agent), inside the **Round lifecycle** (see that section for how a round opens, closes, and chains).

## Build & Test

```bash
cargo build                                          # build the compiler
cargo test --lib                                     # unit tests (~1027)
scripts/run_integration.sh                  # integration tests (autoscaled)
cargo test                                           # all tests
```

**Always pipe integration tests through `tee`** with a random filename — a re-run just to find the failure is expensive, and parallel agents collide on fixed names:

```bash
scripts/run_integration.sh 2>&1 | tee /tmp/integration-$RANDOM.log
```

**LLVM backend.** Set `GG_BACKEND=llvm` to append `--backend=llvm` to every `gg build` (all-or-nothing per run; `tests/integration.rs:52-103`). Full sweeps autoscale via `scripts/run_integration.sh`; no `--test-threads=1` — the harness invokes the pre-built binary via `CARGO_BIN_EXE_gg` (`tests/integration.rs:84`, history in devbook/29).

```bash
GG_BACKEND=llvm GG_BUILD_TIMEOUT_SECS=600 scripts/run_integration.sh --release 2>&1 | tee /tmp/llvm-$RANDOM.log
GG_BACKEND=llvm cargo test --test integration --release dict_user_key_hashable
```

Backends should be at parity; a regression on one but not the other usually means the change touched a backend-specific path rather than shared LIR.

**Timeouts** (override on loaded hosts): `GG_BUILD_TIMEOUT_SECS` (outer `gg build`; default 120/180; bump to 600 on multi-agent boxes for DEBUG self-host builds), `GG_TEST_TIMEOUT_SECS` (per-test binary; default 30; bump for `stress_*` / p2p / gorget-arena).

## Documentation

- `docs/book/` — [The Gorget Book](docs/book/README.md): learn the language from scratch (assumes programming experience, not Gorget experience)
- `docs/language-reference.md` — Full syntax and semantics specification (the authoritative language spec)
- `docs/language-design.md` — Design philosophy, safety features, and rationale
- `docs/devbook/` — [Compiler Internals Book](docs/devbook/README.md): contributor-facing pipeline and design docs

**`docs/book/` and `docs/devbook/` read like a published book — timeless present-tense design narrative faithful to INTENDED behavior, never a fix-log.** No dates, commit hashes, `Snag #N`/`Root #N`/`Fix C` labels, or parity/perf "win" numbers in the design chapters — those belong in `DONE.md` and the playbook chapters (`docs/devbook/29`–`30`, the sanctioned home for dated war stories). A round that changes behavior owes a doc-write-through (Core #9 spans docs too); book-ifying a chapter that has rotted into changelog style is its own recurring DOC track, reviewed like any work (→ Round lifecycle).

## Project Structure

- `src/lexer/` — Logos-based tokenizer with indentation tracking
- `src/parser/` — Recursive descent parser producing AST
- `src/semantic/` — Name resolution, type checking, trait registry, borrow checking
- `src/ir/` — Intermediate representation and lowering from AST (monomorphization, drop insertion, closures)
- `src/lir/` + `src/backend/c_lir/` — SSA-based LIR; `src/bir/` — BIR lowers canonical ops before backend emit
- `src/backend/llvm/` — LLVM IR backend (`--backend=llvm`)
- `src/backend/c/` — C runtime library and SQLite amalgamation
- `src/formatter/` — Source formatter (`gg fmt`)
- `src/loader.rs`, `src/lockfile.rs`, `src/manifest.rs` — Package management
- `src/report.rs` — Test report generation
- `tests/fixtures/*.gg` — Integration test programs with deterministic stdout
- `tests/integration.rs` — Integration test harness: builds fixtures via `cargo run -- build`, executes, asserts stdout

## Language Syntax (Quick Reference)

- Indentation-based blocks (Python-style), type-first declarations: `int x = 5`, `String name = "hello"`
- Functions: `int add(int a, int b): return a + b` / expression-body: `int double(int x): x * 2`
- Closures: `(int x): x * 2` / function types: `int(int, int)` (return type first)
- Match uses `case`: `match x: case 1: ... else: ...`
- Enum variants are qualified: `Color.Red()` not `Red()` (prelude variants `Ok`, `Error`, `Some`, `None` stay bare)
- `meta` keyword for compile-time evaluation — see `docs/language-reference.md` for full builtin list
- Mutable borrow (`&`) / move (`^`) sigils go in the name's slot — before the name, or alone if unnamed (D35). Never before the type:
  `void modify(Message &msg)` ✓ — `void modify(&Message msg)` ✗
  `void consume(Message ^msg)` ✓ / `Callable[void(int &)]` ✓ — `^Message msg`/`(&int)` ✗

**Always use type-first Gorget syntax** in code, plans, and examples: `int x = 5`, `String greet(String name)`. The only string type is `String` — `str` is not a keyword.

## Ownership at Consuming Positions (push/put/set/insert/send, constructors, returns, captures)

CoW's default everywhere is **borrow** — bare-identifier assignments
(`Spanned b = a`), regular function call args, match scrutinees,
collection reads all propagate Ptr aliases at zero cost. Clones happen
only at ownership boundaries, where the destination must own
(collection puts, **constructor / struct / enum field init** like
`S(name)` / `Some(name)`, returns, closure captures). The rule is
**uniform across all of them** — there is no push-vs-constructor split:
clone-if-the-source-is-live, move-if-it-is-dead. Even at the boundary,
the compiler prefers move when liveness allows it.

The carve-outs to CoW-default-borrow are: closures / `Callable[T]`,
`Owned[T]`, `Box[T]`, `Task`, `TaskGroup`, `Guard`. These are
single-owner-by-design (no clone path in the lowering) — the safety pass
emits `MoveWithoutOperator` (E_MoveWithoutOperator) for these at
bare-assign sites AND at constructor / struct / enum-init sites, forcing
the user to write `^source` or `source.clone()`. (At a plain function /
method call these types are simply borrowed, so no operator is needed.)

At each consuming position (`push`, `put`, `set`, `insert`, `send`,
`v[i] = x`) the collection must own. The compiler picks per-arg from
typed ownership state (Phase D's `LocalOwnership`):

| Source                                            | Action                |
|---------------------------------------------------|-----------------------|
| Owns AND dead at this call                        | move after call       |
| Borrow, OR owned but live past this call          | clone before call     |
| Static literal                                    | runtime *_materialize |

The three move-eligible shapes are: `^arg` (user opt-in), expression
temp (last-use + owning by construction), and named local at last use,
bound to an owned value (not from `.get()`, a view-returning method,
or a parameter — those bind borrows).

On a valid move the source slot becomes logically dead (IR `MoveZero`; the
backend zeros the source only when drop-tracking would otherwise re-drop it,
eliding the zero when liveness proves it unobservable — a drop-correctness
optimization, not part of the move semantics). The clone case
is required, not a fallback: a borrowed or still-live source would be a
use-after-free if moved. The decision is mechanical, not heuristic.

**This is the compiler contract — not a suggestion.** Full spec:
[`docs/devbook/11-copy-on-write.md`](docs/devbook/11-copy-on-write.md#materialization-points--the-enforced-boundary-set).

## Solution Quality

- Prefer robust, architecturally sound solutions over quick fixes. When the trade-off is unclear, discuss both approaches and ask before proceeding.
- Aim for generic solutions that solve classes of problems, not just the immediate symptom. Be resourceful — read code, search the web, study how other compilers solve the same problem. Exhaust every avenue before concluding something can't be done.
- Flag code smells and structural issues you encounter, even if unrelated to the current task. Log non-trivial findings to `todo/`.
- You are allowed an opinion. If the user is proposing something dumb, call him out.
- You are allowed to swear if opportune. Don't over do it, but if something deserves a 'holy shit', use it!
- **Performance work measures MEMORY, not just time.** Every perf investigation/fix tracks peak RSS + alloc/clone counts (`--clones=stats` build flag → the `[clone-stats] array_clone=N` line, `/usr/bin/time -v`, `scripts/self_host_mem_baseline.sh`) alongside wall-clock — a memory balloon is as blocking as a time regression (devbook/29).
- **Re-verify a premise against CURRENT source/tests before acting on it (Core #5).** Diagnoses, plans, scores, and dated TODO/memory notes go stale — re-run the test, re-read the cited source, check the actual current code shape; don't trust dated figures or an agent's unverified conclusion. **No un-regenerated numbers:** a figure you did not regenerate this session enters no plan, brief, commit, handover, or statement to the owner — quote the *command*, not the stale value (the `*_comparison` tests are diagnostic-always-pass; only freshly-printed counts mean anything). Burned-cycle incidents in [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#re-verify-a-premise-against-current-source-before-acting).
- **Consult history before proposing a design or briefing a design-heavy task.** Before an architectural change, a diverge-vs-mirror-Rust call, a recommendation, or a design-heavy brief: grep `DONE.md`, `todo/`, `git log`, AND the Rust impl in `src/` (the blueprint for self-host work). Don't wait to be asked. Skip only for mechanical/greenfield changes — reinventing a rejected approach, or misframing "alignment with the existing design" as a "departure", burns real cycles.

## Layering discipline

How information crosses IR layer boundaries (AST → GIR → LIR → backend). Full rules in [`docs/devbook/24-layering-discipline.md`](docs/devbook/24-layering-discipline.md); four-line summary:

1. **Lossless on invariants, lossy on syntax.** Each layer may resolve abstractions (generics, methods, traits) and add information (control flow, SSA). It may not drop semantic invariants (ownership, drop strategy, view-vs-owned, ABI, copy semantics, borrow provenance). Invariants accumulate; abstractions evaporate.
2. **Typed metadata, not name-matched.** Facts cross boundaries as typed fields on structs — never as name prefixes, sentinel values, or runtime-symbol conventions. (See "No name matching" below.)
3. **One source of truth per axis.** For each kind of information, exactly one piece of metadata at exactly one location, read through one accessor. No parallel sidecar maps.
4. **Resolve once, write through.** When a pass resolves an abstraction, the result writes into the next layer's typed metadata. Downstream doesn't redo the work and doesn't get to disagree.

**Litmus test:** if a downstream pass reconstructs information from names, sentinel values, or shape heuristics, the boundary upstream was drawn wrong. The fix is always upstream — add the field, write it at the source, read it at the consumer. Cite the doc in PRs that touch IR layer boundaries.

### No name matching (rule 2 at the runtime-symbol boundary)

Do not pattern-match on function names, type names, runtime-symbol prefixes, or any other identifier string to make a semantic decision. If you're writing `matches!(name, "gorget_str_trim" | ...)` or `if name.starts_with("Vector__")` to decide what something *means* — stop. The metadata you need is missing one layer up.

Symptoms: parallel lists in different files kept in sync by hand; new methods silently misbehaving because a name list wasn't updated; `// keep both lists in sync` comments; lowering/backend decisions spelled as substring tests on identifiers.

The fix: put the semantic flag on the typed declaration (`BuiltinMethodDecl.returns_view`, `Inst::CallRuntime` sidecar, etc.), set once at the source, propagated as typed fields, read via typed accessors. If the metadata genuinely doesn't exist yet, **add it** rather than fishing for the answer in a name.

Exception: at the C-emit boundary you have to spell the runtime symbol (the name *is* the contract with the runtime). Even there, drive the spelling from a typed registry — never make a routing decision based on `if name == "..."`.

### Debugging heuristic — fix complexity as a signal of wrong layer

When you've localized a bug and the fix you're sketching is *intrinsically complex* — save/restore around branches, phi insertion at merges, scope-tracking name maps, manual SSA repair — stop. That complexity is almost always a tell that you're patching a *symptom* (devbook/29).

1. Trace the data the buggy site is reading. *Where was it last written?*
2. Look at the writer. *Did it respect all the typed metadata available?* Or did it default / hardcode / collapse cases the upstream had distinguished?
3. Writer was lossy → fix at the source; the downstream "complex fix" evaporates.
4. Writer was faithful → trace one more layer up. Repeat.

Every layer hop without finding the bug should make you *more* suspicious of your diagnosis, not less.

Worked examples (Snag #17, Snag #13): [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#the-debugging-heuristic-fix-complexity-is-a-signal-of-the-wrong-layer).

### Sibling-site drift — fix the class, not the instance

When you fix a bug at one position in an *enumerated set* — consume positions (`push`/`put`/`set`/`insert`/`send`/ctor/return/capture), tail-value dispatchers, container-literal arms, registration paths — fix the **class**, not the instance:

1. **Grep for the siblings before you commit.**
2. **Prefer centralizing at the producer** over patching each consumer (e.g. `maybe_auto_propagate` hoisted to the `lower_expr` exit; `builder.set_terminator` made a no-op when already terminated — one line that killed a whole class).
3. **Add an arm-count lint** (`tests/lints.rs`, like `container_literal_arms_count`) so the next sibling is forced through the shared path — as part of the fix, not after the next regression.

**Litmus test:** if your fix is "add the missing call to site N", ask "how many sites are there, and what stops site N+1 from the same hole?" If nothing does, you fixed the instance, not the class. Sagas: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#sibling-site-drift-fix-the-class-not-the-instance).

## Don't redesign around compiler gaps

When work hits a compiler bug, the response must be one of:

1. **Fix the gap.** Default move when scope allows.
2. **Write a fixture that exposes the gap + a sharp TODO entry citing it.** Wire as `#[ignore]` if leaving it failing would block other work — but the fixture's expected output must reflect what the language *should* do, not what it currently does.

Forbidden: reshaping the surrounding code (tests, fixtures, examples, even production code) to avoid the gap. Even when commented, this buries the bug — the wired-in expected output (or the surviving workaround idiom) is the load-bearing artifact, not the comment, and "passing" tests lock in buggy behavior as canonical.

Worked examples: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#dont-redesign-around-compiler-gaps).

**Litmus test:** a fixture using a more complex shape than necessary, or a workaround comment citing a bug, is a smell — likely a dodged gap. Verify the bug still exists before treating the workaround as canonical.

**This rule outranks Task Continuity's "Never silently work around a bug".** That one is satisfied by recording the bug; this one is not — a commented redesign is still a redesign, and it still ships.

## Self-host as the elegance showcase

The self-host frontend (`tests/fixtures/self_host_*/`) is the language's reference-grade demonstration. It must be written in **idiomatic Gorget** — the way the language is meant to look when it's working — not the way it had to be written to dodge a compiler bug six months ago. The self-host serves three roles simultaneously: a stress test for the compiler, a regression net (via `*_comparison` and `bootstrap_fixed_point` tests), AND a showcase for the language. The third role is non-negotiable.

**The succession plan (owner 2026-07-18; full treatment devbook/30 §9).** The self-host REPLACES Rust gg as the primary reference at ~100% runtime parity. So a "reference lags the self-host" finding (self-host correct where Rust gg is buggy) is a **succession milestone, not an embarrassment** — file it, fix the Rust side as **oracle hygiene** (a wrong oracle poisons measurements), never dumb the self-host down to match. As agreement-with-Rust loses meaning, **ggdef adjudication is the truth axis** that makes succession safe — subset expansion + the adjudicated-parity split rise accordingly.

Defensive code accumulated for past compiler gaps is **technical debt with a stale justification** — a false historical record that new contributors copy (devbook/29).

Fossils already burned in (all dodging since-fixed bugs) with the concrete list: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#self-host-as-the-elegance-showcase--and-retiring-fossils).

Rules:
1. **No defensive code without a live, cited bug.** If you find a workaround comment ("parallel because…", "wrapper to avoid…", "rebuild instead of mutate…"), verify the bug still exists. If it doesn't, delete the workaround and use the idiomatic shape.
2. **Self-host code reads like the user manual.** If you wouldn't recommend this pattern in `docs/book/`, don't write it in self-host.
3. **When you fix a compiler gap, also retire the workarounds.** A fix is incomplete until the dodge it enabled in self-host is gone. Search for the workaround pattern across all self-host directories before declaring the fix shipped.
4. **Periodically audit.** Compiler gaps that get fixed leave fossils. Treat the self-host as a living document and prune. The `*_comparison` and `bootstrap_fixed_point` tests will catch regressions.

This rule pairs with "Don't redesign around compiler gaps" — that one is about not creating new dodges; this one is about retiring old ones.

## Multi-agent orchestration

When you launch sub-agents via the `Agent` tool in this project, the following rules are **non-negotiable** — past sessions lost work treating them as suggestions (devbook/30 §7):

0. **THE ORCHESTRATOR DOES NOT TOUCH THE CODE — IT LAUNCHES TRACKS THAT DO (owner 2026-08-28; devbook/30 §18).** Its job is exactly four things:
   - **Verify the streak.**
   - **Update the brief.** Per finding: INCORPORATE into the track's scope BY DEFAULT — the TRACK fixes what its own passes discover; let the scope grow a bit — and FILE only when genuinely disjoint, never a stream of new items. Fold verbatim; keep the precedence stack straight.
   - **Coordinate parallel tracks** so they don't collide.
   - **CRITICISE the final form — and hand the criticism to a fresh agent.** Cross-checking a reviewer's claims stays a standing duty (a reviewer can be wrong); what the cross-check yields is a FINDING RETURNED TO THE AGENT, never an edit the orchestrator makes.

   Proposing the fix is the REVIEW AGENT's job, on every pass; implementing it is the EXECUTOR's. ⚠ *"Fix inline unless really disjoint"* binds the **ROUND**, not the orchestrator's hands: hand-editing `src/` between passes means each pass reviews the ORCHESTRATOR'S NEWEST CODE instead of the brief. The parent still drives the integration battery and round-close gates (rule 4).

0b. **Orchestrator is branch-agnostic.** Stay in the launch worktree — that *is* the session integration branch. Never hardcode a branch name (`gorget-1`, etc.) and never check a track branch out there (re-bases every read/gate). Subagents always get their own worktree (rule 1); parent integrates back.

1. **Always pass `isolation: "worktree"`.** No exceptions; applies to NESTED forks too (devbook/29).

2. **Brief the agent to verify its worktree on entry.** Open every agent prompt with:
   > Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree. NEVER touch the main checkout or the orchestrator worktree — every file op, `cargo`, and `git` command runs in your worktree path. Do NOT `cd` into either. Do NOT use absolute paths into main or the orchestrator worktree (worktrees nest UNDER main, so those write into MAIN — see rule 7). If `pwd` is main or the orchestrator worktree, STOP and report it. (Concrete paths live in the session handover.)

3. **Stage explicitly by file name.** Brief every agent: `git add <specific files>` only — NEVER `git add -a`, `git add .`, or `git commit -a`. A sweeping stage clobbers other agents' uncommitted work.

4. **Parent drives the integration sweep, not agents.** Agents run `cargo build` + `--lib` + targeted tests only; the 15-20 min full sweep is the parent's job (agents waiting on it stall and can be killed mid-commit).

5. **Brief file zones when running agents in parallel — disjointness is cheap insurance, NOT a hard requirement (owner 2026-07-21).** Tell each agent which files the other agents are touching. Do NOT defer or reshape a worthwhile parallel track to avoid overlap — worktree isolation makes overlap careful-merge work at integration. When two tracks must touch the same file, brief EACH on the other's exact edit regions (functions/lines) so the diffs stay mergeable, and integrate them deliberately (cherry-pick/merge with conflict resolution, re-run the combined gate). Scout the overlap first (a read-only probe reports the exact shared functions) so the integration is planned, not discovered.

6. **Clean up scratch and worktrees once integrated or abandoned** — they do NOT dispose of themselves (devbook/30 §7). Closing step of every round: **`scripts/round_cleanup.sh`** (dry-runs by default, `--yes` applies; its header documents the four steps). Read the dry-run first. ⚠ It prunes EVERY `agent-*` worktree: right at round CLOSE, WRONG mid-round — an unmerged deliverable or a live agent needs an explicit keep-list (owner 2026-08-16). ⚠ Also sweep `/tmp` — stale `gg build` scratch and prior-round cargo targets dwarf the worktrees. Not "later" — "later" is when the disk is already full.

7. **Worktree-RELATIVE paths only — agent worktrees nest UNDER main.** Agent worktrees live at `<main-checkout>/.claude/worktrees/agent-*`, *inside* the main checkout, so an unqualified absolute path — or a heredoc fallback after an Edit-tool desync — writes into MAIN (devbook/29). Brief every agent: all file ops use paths RELATIVE to its worktree; on an Edit-tool desync, re-Read and retry the Edit tool — never fall back to a shell heredoc with an absolute path; and after any non-Edit-tool write, run `git -C <main-checkout> status` and STOP if it shows changes. Worktree isolation is necessary but NOT sufficient when the worktrees are children of the thing they must not touch. (The concrete main-checkout path for the current environment is in the session handover.)

8. **NEVER `git stash` in agents — the stash stack is repo-GLOBAL across all worktrees.** Brief every agent: save/restore state with `git add <new files>` + `git diff HEAD > /tmp/<name>.patch` + `git apply` — a plain `git diff` LOSES untracked files.

9. **Checkpoint scout prototypes to /tmp EARLY; run final gates FOREGROUND.** Agents are killable at any moment: a prototype kept only in the worktree is lost, and a backgrounded final run can stall when the handoff is lost (devbook/29). Brief agents to checkpoint to `/tmp/recover_*.patch` after every meaningful step and to run their FINAL validation gates as foreground commands with explicit generous timeouts.

## Review plans, TODO items, AND agent briefs/outputs with a fresh agent

A **fresh** agent must review any non-trivial artifact before it's acted on, folding each pass's findings, until a fresh pass raises **no reservations**. ⚠ **THE GAUNTLET VERIFIES WORK; IT DOES NOT DEFER IT (owner 2026-08-27).** Fresh passes exist so one pass cannot quietly break something — NOT as a queue for handing fixes to the next agent or the next round. A pass that surfaces a defect has done its job; the round that surfaced it still owes the fix (devbook/30 §17). Use a *new* agent each pass — a reused one anchors on its prior conclusions. Brief every reviewer to verify each load-bearing claim against source with `file:line` and return SIGN OFF or cited reservations; cross-check them yourself — a reviewer can be wrong too.

**The reviewer's checklist is DESIGN-SOUNDNESS, not just premise-accuracy: a brief or diff that violates a Core invariant is a blocking reservation *even when the code works and every premise checks out* (owner 2026-07-22).** Brief every reviewer to test the artifact's DESIGN against the Core invariants + Layering discipline and raise any violation as a cited reservation — an instance-fix where a *class* exists (Core #4); semantics from name-matching or shape heuristics instead of typed metadata at one source of truth (Layering rule 2); information rebuilt at a *read* site a *writer* should carry (Core #1); a silently-dropping arm (Core #10); a known defect shipped (Core #8). "Correct and premise-accurate" is NOT a SIGN OFF if the design fights an invariant — the reviewer names the invariant and the reference-grade shape instead.

**Scout before you brief.** Before writing a brief — and before committing to any non-trivial plan — run a scout: a read-only probe/audit (often a delegated `Explore`/`general-purpose` agent) that verifies every load-bearing premise against CURRENT source with `file:line`, confirms the bug still reproduces, and where the plan claims a yield, **prototypes it end-to-end and MEASURES the real result.** Killing an unsound plan after a one-agent scout is a win. **Scout yield estimates MUST be end-to-end-verified — compile AND run AND diff whole output, never source-read.**

**Ground the scout's design in the docs, not just the code.** Every scout brief MUST tell the agent to consult the relevant documentation FIRST — `docs/language-design.md`, `docs/book/`, `docs/devbook/`, `docs/internals/` — and base the design on it, citing the sections it rests on. The code shows what IS; the docs show what's INTENDED, and a code-only design reproduces whatever fossil is already there. **⚠ EXCEPT `docs/language-reference.md`, written AFTER the implementation and not caught up everywhere (owner 2026-08-18): a reference-vs-code conflict is an OPEN QUESTION, not doc-wins, and a load-bearing one is an OWNER ASK — never a scout's or orchestrator's guess (case: devbook/30 §14).**

**The passes are SEQUENTIAL, not parallel**; a blocking pass always gets a confirming fresh pass after the fold. **≥3 passes is the FLOOR; there is NO upper bound** — consecutive blocking passes are the gauntlet CONVERGING, not failing; never invent a cap.

**Convergence gate — the READINESS CHECKLIST (owner 2026-08-28; devbook/30 §19, which also records the design-stability gate it supersedes).** A track is ready when the brief satisfies all FIVE, each binary and checkable without judgement: **(1)** every measurement carries a FIRE COUNT proving the mechanism executed; **(2)** every enumeration cites an INDEPENDENT witness (rustc exhaustiveness, a repo lint table — never the enumerator's own list); **(3)** `|pinned cells| == |changed cells|`; **(4)** the GUARD FAILS when the fix is reverted; **(5)** every load-bearing figure REGENERATED at current HEAD. **A new class that slips through EARNS A SIXTH ROW.**

⚠ **SCOPE MAKES IT TERMINATE: a finding resets only if it changes the brief's fix shape · site set · scope boundary · guard mechanism.** A TREE defect found while reviewing, and an ORCHESTRATOR fold/guard defect, are not the track's. Reviewers still hunt freely; an off-checklist find does not reset the streak. **What then happens to that find is not this rule's call — disposition belongs to Multi-agent rule 0.** Terminal-pass minors fold as MARKED ERRATA, never woven into the body; the executor treats errata as spec (devbook/30 §12).

**FOLD VERBATIM, NEVER SUMMARISED; STACK FOLDS AS PRECEDENCE-ORDERED ADDENDA (owner 2026-08-17; devbook/30 §13).** Each fold generation is its own marked addendum with an explicit precedence line (later > earlier > body); never rewrite the body silently. ⚠ **This binds the ORCHESTRATOR'S OWN directives too** — an addendum may DECIDE (scope, choice, retraction), never RESTATE; a restatement compresses, and it OUTRANKS the verbatim text it compressed. Not narrowing ⇒ cite *operative text: pass-N §X, unchanged*; overriding PART ⇒ NAME the part; errata are RESTATED, never pointed at. After each fold re-read the enclosing SECTION and grep the correction.

Rationale + D45: devbook/30 §12; examples: [`docs/devbook/29`](docs/devbook/29-contributor-playbook.md#scout-before-you-brief-review-in-sequential-fresh-passes).

**One track, one agent, clean context — NO pack reviews (owner 2026-07-21).** Each *track* is its own gauntlet unit. **Forbidden:** a single "pack" reviewer reading N track briefs (or N executor diffs) in one conversation and signing them off together — that reuses context and dilutes attention (origin: devbook/30 §2). Required instead: per track, ≥3 sequential fresh brief-review agents each seeing *only that track's brief* (N tracks ⇒ N×≥3 agents); one executor per track (worktree); one fresh output-review per track's diff before it integrates. Pass *k* may run in parallel *across* tracks; *within* a track passes stay sequential (pass 2 only after pass 1's findings are folded). Parallelism is *across tracks*, not *across roles for the same track*, and never by packing multiple tracks into one reviewer context.

**Model allocation (harness-agnostic; owner 2026-07-27, reaffirmed 2026-08-06).** EVERY agent — scout, executor, every review pass — runs the STRONGEST available model; "the output-review will catch it" is no licence to under-power one, since review sees the diff, not the road not taken. A rationing harness keeps it LAST at: (a) the FIRST review pass on a fresh artifact — first contact catches the structural defects while folding is cheapest; (b) the FINAL pre-integration output-review — maximum consequence, plus model diversity against the executor's blind spots; (c) ad-hoc arbitration when two agents disagree on a load-bearing conclusion. Mandate quality still dominates model strength: the reviewer's checklist is what catches lane-shaped misses, and every pass gets cross-checked regardless of model (devbook/30 §2).

This applies to four kinds of artifact:

1. **Plans / TODO items** — review before you start implementing.
2. **Agent briefs (≥3 fresh passes)** — a brief is a spec; review it *before launching*. A wrong brief wastes the whole execute + validate cycle; these passes routinely catch a mis-identified root cause, a wrong-layer fix, or a "fix" that's already implemented.
3. **Agent output** — when the executor finishes, a fresh agent reviews its diff/commits *before you integrate or run expensive validation*. This review includes three gates: the **breadcrumb-check** — no completed-status entries (`LANDED`/`FIXED`/`RESOLVED`/`DONE`/`SHIPPED`/`✅`) added to `todo/`; those are completed work to MOVE to `DONE.md` or pending follow-ups to REPHRASE as the work that remains; `todo/` holds pending work only. The **fixture-coverage gate** (Core #11/#12) — SIGN OFF requires wide, genuinely-exercising, RED-verified regression nets for what the diff changed, not a single thin pin if siblings exist. And the **reference-grade gate** (Core #8) — the acceptance bar is *correct/principled*, not "matches the reference"; a KNOWN DEFECT left in place is a reservation even when it reproduces identically in Rust gg or only manifests on a program that is UB on both backends, and "both backends agree on the wrong answer" / "benign because both are UB" is the exact phrasing that must trip the gate. The orchestrator must not accept it either — pushing the defect to a 'benign, filed' follow-up is the same failure.
4. **Session-handover / state snapshots** — the in-flight-state doc a fresh session resumes from (the `TODO.md` handover block, the `MEMORY.md` north-star/scores) is a spec the next session executes from; a stale one misleads it exactly as a wrong brief misleads an executor. Before relying on it, a fresh agent verifies every load-bearing claim against ACTUAL state: commit hashes resolve, scores re-confirmed from the `*_comparison` tests (not memory), durable artifacts present at cited paths, nothing stale or contradictory. Same trap as "re-verify a premise" (Core #5).

A multi-track round is N independent per-track loops (scout → brief → ≥3 reviews → executor → output-review → integrate), parallel across tracks, never one pack loop; a session handover runs the same loop on the state snapshot before the baton is passed. You (the orchestrator) hold the full context, brief each reviewer/executor with only the artifact they own, and keep them honest.

**Scouts, briefs, and review checkpoints are `/tmp`-only — never `git add` them.** The gauntlet's paperwork (scout reports, executor briefs, census reports, review notes) is exhaust: it lives in `/tmp`. Durable content goes to its official home (`docs/language-design.md` / the define-gorget ledger / book / devbook); `todo/` items are written **self-contained** — findings inline, never "see the scout file". The single session-state doc is `TODO.md`'s handover block. Round close `git rm`s any scout/brief that slipped into the repo (git-recoverable), guarded by the shrink-only allowlist lint `docs_plans_removed_and_define_gorget_is_ledger_only`; moving durable content out and deleting a completed plan is itself a reviewed change, not a silent bulk delete.

**Fold/patch scripts MUST assert their replace targets matched.** A stale target silently dropped wastes the entire pass: every fold asserts the old text was found and the new text landed (a `must_replace` helper), then greps for a distinctive fragment of the new text — or just use the Edit tool, which errors on no-match. `str.replace` silently no-ops; a "folded" print is not verification.

## Task Continuity

Work items live ONE PER FILE in `todo/` (owner 2026-08-23): TOML front matter, `+++`, prose verbatim — spec in `scripts/todo_index.py`. **A field the item's text does not state stays EMPTY.** `TODO.md` keeps the handover, the invariants, and a GENERATED index (`scripts/todo_index.py --write`; lint `todo_index_is_current`).

**Cardinal rule:** If any work is deferred — whether a discovered bug, a remaining sub-task, or a blocked feature — it **must** be filed as a `todo/<id>.md` item before moving on. Nothing falls through the cracks.

- **Adding work:** one new `todo/<id>.md`, then regenerate the index. Never replace existing items. Categorize by priority (High / Medium / Low).
- **Completing work:** `git rm` the item file and add it to the top of `DONE.md` with a date stamp: `- [2026-02-10] Task description`. Closure IS removal — never a `status` field; git keeps the item's whole life.
- **Before overwriting your plan:** Always check if there are incomplete items from the previous plan and file them.
- **Restoring context:** Read `TODO.md` at the start of every conversation and after finishing any tangential fix.
- **Discovered issues:** Fix small bugs inline. For anything too large to fix immediately, file it and move on. Never silently work around a bug — either fix it or record it.
- **Every filed reproducible bug/gap ships a DURABLE `known_gaps` repro — never a `/tmp` scratch file (owner 2026-07-24).** When you file a bug, commit a **minimal reproducer** to `tests/fixtures/known_gaps/` and wire an `#[ignore]`d test asserting the **CORRECT/intended** output (or an ASan/`security_safe_no_leak` fixture for a leak/UAF), cited from the item's `repro` (devbook/30 §8). The committed fixture keeps the exact shape (leaks need a *heap-forced* value, not a literal — a literal is a false negative) and **graduates to a live regression fixture** (un-ignore / promote out of `known_gaps/`) the same round the bug is fixed. This is the one exception to "scouts/briefs are `/tmp`-only": the triage *paperwork* is `/tmp`, the *repro* is committed. ⊕ **A repro CITED by an item is that item's EVIDENCE, not a second filing (owner 2026-08-23; devbook/30 §8).** An UNCITED gap fixture still counts on its own. Generalizes "Don't redesign around compiler gaps" rule 2 to every filed defect; non-reproducible items (design notes, refactors, perf without a repro) are naturally exempt.
- **GREP `todo/` BEFORE YOU FILE** — the symptom AND the mechanism (two duplicates filed in one session). When the defect joins a family, state what DISCRIMINATES it (panic site, lane, axis cell) and name the WHOLE family: "distinct from the two filed siblings" when there are five is a selection, not an enumeration (Core #15e Q3).
- **Never delete `TODO.md` or bulk-delete `todo/`** — only move completed items out.
- **The handover stores invariants and commands, not numbers.** The `TODO.md` handover block and any state snapshot are specs the next session executes from; a dated number in them is a stale premise waiting to happen. Record *what to run to get the current number* and *what it means*, not the number itself.
- **Commit autonomously when green.** Once `cargo test --lib` and the round's relevant integration tests pass, commit without asking — this waiver **overrides the harness default of "commit only when the user asks".** The waiver covers `git commit` only: still ask before push / force-push / `reset --hard` / `branch -D` / `rm -rf` / amend / rebasing onto a shared branch / opening or closing PRs. Never commit red or skipped.
- **Stale-pending scan.** Aggressively move completed items to `DONE.md` every session, and periodically stale-scan pending items — verify the cited bug/stub still exists in current source before keeping one. Keep items short and scannable, and keep the pending set small.

## Round lifecycle

The delegated-task pipeline (→ Review) is the atom; a **round** is the unit the orchestrator works in. By default rounds run back-to-back, autonomously, until the owner stops them.

1. **Open a round around a headline theme — parallel tracks welcome.** Pick the next headline from `TODO.md`'s handover block to give the round its identity and its `DONE.md` record. Multiple items/tracks may run IN PARALLEL within the round — disjoint file zones, per Multi-agent orchestration rule 5 (e.g. a ggdef-oracle track ∥ a wrong-code track, or an eight-track wave). "One campaign" is about the round's *theme*, NOT a limit on concurrency. The one thing to avoid is PRE-WARMING a FUTURE round's campaign: don't start the next headline's scouts/briefs while the current round's chains are still executing — the round boundary is a landing gate. Focus means not fragmenting attention across rounds, not serializing work within one.

   **Convergence lens (owner 2026-07-28; its STRICT 2× RULE is REMOVED — step 5):** expected NET items closed is one round-selection axis among several — bias class-fix (Core #4) and bulk-graduation over instance-fixes-with-follow-ups. It informs SELECTION; it no longer gates CLOSING, and a discovery round that files heavily is not thereby a worse round.
2. **Run the delegated pipeline** (→ Review): scout (verify premises + measure end-to-end) → brief → ≥3 fresh sequential brief-reviews → launch (worktree, → Multi-agent orchestration) → fresh output-review → integrate. A semantic change lands on every lane the same round (Core #9), each with its exercising fixture (Core #11).
3. **Commit as the chains land** (→ Task Continuity, "Commit autonomously when green").
4. **Round-close gate — the FULL local battery, matching CI's target set (owner-required 2026-06-20 + 2026-07-23 post-mortem).** With the round's commits on the integration branch:
   - **C sweep.** `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 scripts/run_integration.sh 2>&1 | tee /tmp/integration-$RANDOM.log` — **both** knobs; omitting the second false-reds `lowerer_comparison` on a loaded box. **Use the wrapper, never a hand-rolled thread count** — it autoscales on free cores AND RAM, which binds first.
   - **Then the LLVM sweep, SEQUENTIALLY, never in parallel** (concurrent `gg build`/linker runs thrash the toolchain, and `self_host_*` fixtures share fixed `/tmp` scratch paths), plus the bootstrap / parity-split gates the change touched.
   - **AND the separate `cargo` targets `--test integration` never touches:** `-p ggdef` (conformance corpus), `--test spec_conformance` (3-lane C/LLVM/SH adjudication), `--test security` (ASan), `--test lints` (guard ratchets), `--lib`.
   - **⊕ And `scripts/sanitize_sweep.sh`** (~25 min, ASan leak + corruption allowlists) — being in no gate list is exactly how it drifted RED unnoticed.
   - **⊕ Also run `python3 scripts/robustness_map.py`** — the beginner-code robustness map (700+ cells on five lanes: C · LLVM · self-host · ASan · ggdef; shapes from other languages' tutorials). It reports the WORKS share per topic and **fails on any WORKS→broken regression**; `--accept` folds genuine progress into the baseline as a reviewed change. It measures what the main suite structurally cannot. **Never edit an expectation to match what the compiler prints** — that pins a bug as canonical. Every hang/spin/timeout gets root-caused into a census row, never merely killed; a both-lane hang is still ≥2 bugs (Core #8). Prefer a no-new-hangs executable guard (CRASH-count ratchet / shrinking `EXPECTED_HANGS`).

   **The full battery is what makes local-green SUFFICIENT — it covers every target CI runs, so local-green IS the round-close sign-off; autonomous rounds do NOT wait for CI.** The one residual is a CI-*config* failure (a job missing `llc`, a Linux-only floor skipped on macOS) — separate CI-hygiene, glanced at periodically, NEVER a per-round blocker. Targeted and self-host gates are necessary, not sufficient (Core #7).
5. **Records + convergence RECORD (owner 2026-08-23: the STRICT 2× RULE is REMOVED; devbook/30 §17).** Add the round's `DONE.md` entry (date-stamped); update `TODO.md`'s handover block IN PLACE (pending-only, no completed breadcrumbs, invariants+commands not numbers); refresh any state snapshot.
   - **Every DONE round entry still ends with the `Convergence:` line QUOTED from `scripts/convergence.sh <prev_kg> <prev_todo> <filed>`** — but it is now a **MEASUREMENT, NOT A GATE**: no ratio to meet, no net-decrease requirement, and **a round never stays open on the arithmetic.**
   - **What SURVIVES, as an owner ruling in its own right: FIX INLINE unless the defect is REALLY DISJOINT** — the TRACK fixes the findings its own passes discover, via its executor, never the orchestrator (rule 0); a stream of new `todo/` items instead of a slightly larger scope is the anti-pattern.
   - ⚠ **DISJOINT MEANS A DIFFERENT *CLASS*, NOT A DIFFERENT SITE (owner 2026-08-27).** A sibling site in the same enumerated class — same helper, same axis, same fix shape — is Core #4's subject: fix the class, never file the sibling. Only a genuinely different class is FILED, with its durable `known_gaps` repro; everything else is fixed in the round that found it.
   - **A round whose commit log never touches `src/` has stopped, not discovered** — check `git log --oneline <round-open>..HEAD -- src/ | wc -l` BEFORE close.
   - **File follow-ups as `todo/` items, never into the handover** (the arbiter counts files). **Phased work is filed as ONE ITEM PER DECLARED PHASE**, so a landed phase is visible in the record.
   - **A red battery is still NEVER waivable.**
6. **Docs + hygiene.** Doc-write-through for behavior changes (→ Documentation); prune completed plans/briefs (`git rm`, git-recoverable); capture-then-prune agent worktrees, `/tmp` scratch, and any stray stash (→ Multi-agent orchestration rule 6).
7. **Open the next round autonomously.** STOP and ask the owner for exactly TWO things: **(i)** a genuine DESIGN decision — language semantics, a scope/sequencing trade-off, a knob such as error-vs-silent-no-op, retiring a feature; **(ii)** an UNRATIFIED semantics question — including any lane divergence whose correct direction is not already settled (see Core #9). Both are owner CALLS, not process questions. **Never stop for the discipline** — gauntlet, review passes, battery and parity regen all run AUTONOMOUSLY, never as a permission request — nor for a choice whose reference-grade answer is clear (implement, note, proceed). The owner may suspend autonomy for a stretch ("don't start the next round"); that is a live override of this default, not a change to it.
