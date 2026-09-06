# TODO


### 🚀 ROUND L (R50) IS OPEN — 2026-09-05. HEADLINE: **THE CRITICAL MEMORY-SAFETY SET + R49's DEFERRED HALVES.**
**Owner-directed.** Safety debt first; the optimality pivot follows it, not beside it.

### ✅ CLOSURE-RECORD AUDIT — **24 COMPLETIONS ALL IN `DONE.md`; 2 WITHDRAWALS CORRECTLY ABSENT**

All 26 range-walk closures checked. **24 are completions and every one is in `DONE.md`.** The two that are
not are **WITHDRAWALS, and belong nowhere near it** — putting a withdrawn item in `DONE.md` would record it as
an accomplishment. Both reasons re-verified here, not taken from their commit messages:
- **`t1287`** — withdrawn as duplicating `t1290` *and naming the wrong value*. `t1290` is live and its
  mechanism is the more precise one (a String **INDEX** read materialising through
  `gorget_string_clone_to_owned` into a value nothing registers for drop).
- **`t1444`** — dropped; its whole mechanism is *"the census exits 1 at pristine HEAD"*, and `t0121` is closed
  with the census rc 0.
⚠ **MY AUDIT INSTRUMENT OVER-FLAGGED — the SECOND time this round.** *"Is the id in `DONE.md`?"* cannot tell a
completion from a withdrawal, so it reported 2 defects where there are 0. Same shape as the revert audit
below, which reported 0 for two tracks that fully complied in different words. ⇒ **when a cheap textual audit
flags something, READ THE ITEM before believing it** — in both directions.

### ✅ REVERT-RULE AUDIT — **ALL TEN TRACKS PASS** (audited 2026-09-05, so R51 need not redo it)

*"A fixture set is complete only when EVERY PARTIAL REVERT turns a row RED"* checked against all ten
integrated tracks. **All ten satisfy it**, two of them in wording a keyword grep cannot see:
- **A1 (`t1077`)** — 9 fixtures RED-verified vs a pinned pre-fix binary; the 10th is exempt **by construction
  and says why**: it was ACCIDENTALLY correct pre-fix, so it is pinned must-stay-green (SIX-Q #6, handled).
- **H (`t1387`)** — per-revert ALLOCATION DELTAS rather than the phrase: pristine 38 B/7 · transfer-wrapper
  drop alone 28 B/5 · `put_cloned`'s `!__gg_hit` alone 10 B/2 · fixed rc 0.
⚠ **MY AUDIT INSTRUMENT WAS THE WEAK PART** — it grepped for the PHRASE and reported 0 for both, which is
Core #13 pointed at myself: a keyword probe cannot see a semantic property. **Read the two before believing a
future audit that flags them.**
⭐ **THE ONE FINDING WORTH CARRYING FORWARD, from C2:** its revert lattice is **NON-MONOTONE** — a SUPERSET
revert restores the pre-fix path and **greens rows its own subset fails**. ⇒ *"revert everything and watch it
go red"* is **NOT a valid substitute for atom-by-atom**, and a track that only does the coarse revert can
report full coverage while pinning nothing.
⊕ **K (`t1385`) is the model for the honest case**: it writes down the states nothing pins — *"reverting any
of them is GREEN under every gate … but 'no row pins it' has to be written down, or the next reader assumes
the suite covers it"* — including a fourth unpinned combination.

### ⚖ THE OWNER-ASK LEDGER FOR R50 — **SIX RAISED, ONE SURVIVES** (audited 2026-09-05)

⛔ **THE AUDIT'S OWN FINDING FIRST: two of these dissolved in conversation and the withdrawal was recorded
NOWHERE DURABLE.** The roster said `HELD — OWNER ASK` for both while the answer was already settled. **A
resolution that lives only in a transcript is exactly as ephemeral as a brief** — same failure as F1r's lost
omission, one heartbeat apart.

| ask | verdict | what settled it |
|---|---|---|
| **`t1443`** neg overflow | ✅ **WITHDRAWN**, committed `da541d17a` | Rust gg contradicts ITSELF (`0 - m` traps, `-m` wraps); wrap already spellable as `0 -% m`; D30 lists defined-wrap among REJECTED alternatives. ggdef corroborates, it does not decide. |
| **B** the capture cell | ✅ **WITHDRAWN — record it** | **`D7` (ratified, unimplemented) IS the reference-grade answer.** Recourse exists TODAY and is measured: `Callable c2 = c.clone()` then capture `c2`. `^c` at a capture is unspellable (`(^c)(): …` is a parse error), so there is no accept→reject cliff. |
| **K2** ggdef VERDICT taxonomy | ✅ **WITHDRAWN — record it** | ⭐ **THE REPO ALREADY ANSWERED IT.** `todo/t0338.md:11`, filed by R43 in August: *"the gap is that the shapes are OUT OF SUBSET — NOT that the tool fails quietly; ggdef's rejection is loud and correct (`EXIT_ILLFORMED = 1`)"*, and it prescribes the subset-expansion queue. `EXIT_ILLFORMED` was **deliberately collapsed 102→1** — the distinction K2 wanted to ADD once existed and was REMOVED. |
| **D1** `t1225` | 🔄 **NOT AN ASK — a sequencing dependency** | The named ruling is **absent from the ledger AND never was there**: `git log -S'callee-borrow' -- docs/define-gorget/decisions.md` is EMPTY, so there is no removal to find. The blocking work is filed: **`t1349`** (ctors store a BORROWED `Callable` into a returned field) + **`t1350`** (write site: `needs_explicit_move`'s `Generic` arm misses a type param bound to a single-owner type). Owner input is not what unblocks it. |
| **D0′** Ruling 3 | 🔄 **OWNER-FACING REPORT, not a question** | Ruling 3 is not implementable as written — all 16 builtin generics are `DefKind::Import`/`Span::dummy()`, identical to user imports; both branches are wrong and every discriminator is the forbidden name list in disguise (Core #2). **The prerequisite is FILED as `t1408`** — *"is this name a TYPE?" has TWO independent answers*, a `BUILTIN_GENERIC_TYPES` string list AND a separate `matches!` on literals. The owner's only real choice is *wait for `t1408`* vs *narrow Ruling 3*. |
| **C1** shadow-reject | ⚖⚖ **GENUINE — IT SURVIVES THE PROBE** | The ruling says *"at the ambiguous site"* but offers `Vector[int] Vector = [1,2,3]` as proof of an accept→reject change, **and that program has no `x[…](…)` site.** Measured at HEAD: the shadowing declaration checks **rc 0**, and `Vector.len()` on it checks **rc 0**. ⇒ reading (a) IS accept→reject; reading (b) leaves the example accepted and costs 0. **Two different semantics, and the ruling's text and its own example pick different ones.** |

⇒ ⭐ **BRING THE OWNER EXACTLY ONE QUESTION: C1.** D0′ rides along as a report, not a question.

⭐⭐ **AND THEY SHARE A ROOT CAUSE — `t1408` — WHICH CHANGES WHAT TO ASK FOR.** Regenerated 2026-09-06:
- `BUILTIN_GENERIC_TYPES` (`src/semantic/resolve.rs:19`) holds **16** names — `Vector, Deque, Dict, HashMap,
  Set, HashSet, Box, Future, Task, Channel, Shared, Weak, Mutex, Guard, TaskGroup, FxHasher`.
- **`Callable` is NOT among them** — it lives in a SECOND, independent `matches!` on string literals
  (`src/semantic/types.rs:661`, with `MutCallable`/`ConsumeCallable`).
- **`Owned` is in NEITHER** (`grep -c '"Owned"' src/semantic/resolve.rs` → **0**; no hit in `types.rs`).

`t1408`'s filed mechanism states exactly this. ⇒ **C1's fix (a shadow-check) and D0′'s blocker (the type-kind
set is not well-defined) are the SAME defect wearing two hats.** Any shadow-check keyed on the type namespace
is structurally blind to the two spellings most likely to be shadowed by a callable-valued local, and Ruling 3
cannot partition a set that has two disjoint definitions and a hole.
⚠ **SO THE ORDER MATTERS AND IS NOT A PREFERENCE: `t1408` FIRST.** After it lands, C1's two readings can be
costed against ONE type namespace instead of two-and-a-gap — and the answer may change shape, because reading
(b)'s *"cost is provably 0"* was measured with an instrument that cannot see `Callable`/`Owned` shadows at all.
⚠ **AND THE HAZARD C1's INSTRUMENT SURFACED, which any fix must survive:** `Callable` and `Owned` are **NOT in
the ScopeTable's type namespace** (`Option`/`Result`/`Set`/`Box` all are) — so a shadow-check built on that
namespace is **blind to the two spellings most likely to be shadowed by a callable-valued local.** Same root as
`t1408`.

### ✅ ALL TEN TRACKS INTEGRATED — ROUND-CLOSE BATTERY IN FLIGHT (2026-09-05)

**Integrated:** A1 · C2 · F1r · G · H · K · E · A2 · J · L. **Head at close-start: `ba1ca28b3`.**
**Convergence (MEASURED, does not gate):** `known_gaps 17→17 · TODO items 870→938 · net +68`
(regen: `scripts/convergence.sh 17 870 94` — **94 filed, 26 closed**).
⛔ **COUNT THE COMMIT RANGE, NOT THE TWO ENDPOINTS — MY FIRST FIGURE WAS WRONG IN BOTH COLUMNS.**
`git diff --diff-filter=A|D bee51d536..HEAD -- todo/` reports **78 filed / 12 closed**; walking the range
(`git log --diff-filter=D --name-only --format= bee51d536..HEAD -- todo/`) reports **94 / 26**. **Sixteen items
were filed AND closed inside the round** — `t1362`, `t1385`, `t1387`, `t1407`, `t1410`, `t1421` among them —
and an endpoint diff is structurally blind to every one. The round closed **more than twice** what I first
reported.
⚠⚠ **AND HERE IS WHY IT SURVIVED MY OWN CHECK: I VALIDATED AN ENDPOINT NUMBER AGAINST AN ENDPOINT NUMBER.**
The convergence script's *implied closed* matched my *measured deletions* exactly — 12 = 12 — so it looked
self-consistent. **Both came from the same blind method.** Two figures from one broken instrument agreeing is
not corroboration; it is the instrument agreeing with itself. Same family as devbook/29's *"a control that
shares the broken harness is not a control"*. ⇒ **the reconciliation that actually works is arithmetic against
the TREE: `base + filed − closed == ls todo/*.md | wc -l`** (870 + 94 − 26 = 938 ✓; the endpoint figures give
936 ✗).
⚠ **R49's RECORDED BASELINE DOES NOT REGENERATE — do not inherit it.** `DONE.md` records R49 as
`todo_items=887`; the tree at `bee51d536` has **870** (`git ls-tree -r --name-only bee51d536 -- todo/ |
grep -c '\.md$'`). Using 887 implied 29 closures against 12 measured deletions. **Measure the baseline from
the TREE, never from the previous round's line.**

**BATTERY LEDGER — rc off the BARE command every time:**
| gate | rc | note |
|---|---|---|
| C sweep (run 1) | **101** | 2788 passed · 1 failed · 6070s. ⚠ **its task notification said "exit code 0"** — that was the wrapper's trailing `tail`. |
| ↳ `self_host_runtime` after fix | **0** | 1376 passing · 0 regressed · 2 passed/0 failed |
| `self_host_bootstrap_fixed_point` | **ok** | inside the sweep, at the DEFAULT 600s stage timeout — no 1800 needed on a quiet box |
| LLVM sweep | ⏳ | running (`--release`) |
| C sweep (run 2) | **0** | 2789 passed · 0 failed · 6054s — **the owed clean single-run green, delivered** |
| LLVM sweep | **0** | 2789 passed · 0 failed · 5020s · clean single run |
| 8 fast gates | **0** | lib · lints · c_runtime · spec_conformance · security · ggdef · census · staging |
| **`GG_BACKEND=llvm cargo test --test security --release`** | **0** | ✅ **GREEN ON ITS FIRST-EVER RUN** — 223 ok · 31 ignored · 0 failed · **0 never-executed**, all 254 accounted for. Found only because `t1452` showed the lint exempted it. |
| `sanitize_sweep.sh` | **0** | `JOBS=4`, `REPS=3` intact. 16 rows now over-wide → filed `t1453` (the report is ADVISORY, cannot red its own gate) |
| `robustness_map.py --lanes all --jobs 4` | ⏳ | last gate; `--lanes c,llvm` is SUBSUMED, do not add it |

⛔ **DO NOT CHUNK `robustness_map.py` BY LANE, EVEN THOUGH `--lanes c` / `--lanes llvm` RUN INDIVIDUALLY.**
`scripts/robustness_map.py:74` — *"Only c/llvm/selfhost participate in the CROSS-LANE DIVERGENCE GATE"* —
so per-lane invocations measure each lane and **destroy the comparison between them**, which is a substantive
part of what the gate checks. It must be ONE `--lanes all` invocation. ⇒ **the memory lever here is
`--jobs 4` (default `min(8, cpu_count)`), never splitting the run.**
⭐ **THE GENERAL RULE THIS ROUND EARNED TWICE:** the security suite chunked safely because its cells are
INDEPENDENT; this one cannot because its cells are COMPARED. **Before slicing any gate, ask whether it
measures each unit or the RELATIONSHIP between units** — and if you slice anyway, prove the slices execute
the whole (223 + 31 == 254).

✅ **THE OWED SECOND C RUN IS DONE AND GREEN** — 2789 passed / 0 failed / rc 0, matching the LLVM sweep's
2789 exactly. Both lanes now stand green **in a single run each at the final tree**, which is the claim; the
per-test re-verify never was.
⊕ **The A2-class is DISCHARGED for this round by construction:** the sweep IS the detector for
stale-snapshot-after-fixture-edit, it ran over all 1376, and it found exactly one. No separate audit needed.

⛔ **RUN LONG GATES IN THE FOREGROUND — MA-9 SAYS SO AND I VIOLATED IT THREE TIMES.** Three BACKGROUND
attempts at the LLVM security gate were killed by the **HARNESS's low-memory watchdog**; `oom_kill` in
`/sys/fs/cgroup/memory.events` **did not move** (24 before, 24 after), so the kernel never OOM-killed
anything and `free`'s 9.4 GB free was honest. **30+ FOREGROUND invocations, zero kills.**
⚠ **READ `oom_kill` AS A DELTA, NEVER A LEVEL** — its non-zero absolute value is what made a harness
watchdog look like a kernel OOM.
⭐ **AND IF YOU SLICE A GATE, PROVE THE SLICES EXECUTE THE WHOLE.** Chunking by name prefix left **16 tests
uncovered** — including `sanitizer_gate_is_real_on_both_backends`, the verify-the-verifier cell. Filter
coverage is not execution coverage: five filters matched a test that was `#[ignore]`d and reported
`0 passed`. The accounting that settles it parses `^test <name> ... (ok|ignored)` out of every chunk log and
asserts **ran + ignored == declared**, 223 + 31 == 254.

⚖ **A REAL TENSION, STATED SO THE NEXT SESSION DOES NOT REDISCOVER IT: MA-9 SAYS FOREGROUND, THE HARNESS
CAPS FOREGROUND AT 10 MINUTES, AND `sanitize_sweep.sh` TAKES ~25.** It has no chunking flag (`--phase`
belongs to `verdict.py`, not the sweep), so background is the only option and MA-9 cannot be honoured
literally. ⇒ **attack the watchdog's actual trigger instead: `JOBS=4` (default 8) halves the concurrent ASan
processes and so the peak RSS.** ⛔ **Do NOT lower `REPS` (default 3) to buy time — that is the ANTI-FLAKE
repetition, and trading it for wall-clock silently weakens the instrument rather than the schedule.** If
`JOBS=4` is still killed, go to 2; the gate gets slower, never weaker.

**BATTERY ORDER (C and LLVM never simultaneously — owner):** C sweep → LLVM sweep → `self_host_bootstrap_fixed_point`
(**mandatory this round**: J changed `src/backend/c/runtime/runtime_array.c`, an `embed_file` input, and L
changed `self_host_lowerer/lir_codegen.gg`) → `sanitize_sweep.sh` → `robustness_map.py --lanes all` →
the cargo targets → the two script gates.
⛔ **THE TREE IS FROZEN FOR THE DURATION — no `src/`, `lib/`, `compiler/` or driver edits, comments included.**
That is not caution: J's bootstrap RED this round was a one-word comment edit to a runtime `.c` mid-run,
and it read as a genuine ratchet breach.

**ROSTER — five tracks, each with a private disjoint ID BLOCK (MA-3b; a track NEVER picks its own):**
| track | scope | ids |
|---|---|---|
| ✅ **A1** | **`t1077`** — **INTEGRATED 2026-09-05** at `14c624a7f`. 4 passes + output-review, all 3 gates. | `t1309`–`t1318` (`t1312` released; free `t1314`–`t1318`) |
| ✅✅ **A2** | **`t1374`+`t1375`+`t0104` INTEGRATED** (`4a5c6dad1`+`b9af510fe`). ⭐ **Its executor REFUTED the output-review's BLOCKING 1 with a 4-state × 2-backend table — and I had relayed that claim as a prescription without measuring it.** The reviewer said `n.0.get()` builds on neither backend at either state; measured, it builds rc 0 at POST on both. The real error was a different clause in the same sentence. | `t1378`–`t1382` |
| **B** | ✅ **ASK WITHDRAWN 2026-09-05 — `D7` is the reference-grade answer and recourse exists today (see the ask ledger above). Track still HELD on its own design.** Pass 1: NOT SIGN OFF, 5 BLOCKING.** The brief was STALE BY CONSTRUCTION (my revert landed 3 min after it) and **overrode a ratified `decisions.md` clause with an agent's derivation.** Streak 0/3. Scope GREW to **`t1067`+`t0948`+`t1210`**, one class-fix. **`t1067`** — a closure CAPTURING ANOTHER CLOSURE reads freed memory: **rc 0 with silently wrong output**, ASan UAF. R49-found. | `t1319`–`t1328` |
| **C1** | ⛔⛔ **BLOCKED A FOURTH TIME — `R`'s SUBJECT IS A SELECTION ON A THIRD AXIS. RECOMMEND CLOSE FOR R50: `R` NEEDS ITS OWN SCOUT.** Spine survives and measures STRONGER each pass. | `t1332`–`t1333` |
| ✅✅ **C2** | **`t0045`+`t0403` INTEGRATED** (4 commits). Construct-scoped instrument; 2 sites beyond what I reported. | `t1334`–`t1338` |
| **D0′** | 🔄 **OWNER-FACING REPORT, not a question — Ruling 3 is not implementable as written; the prerequisite is FILED as `t1408`.** ⛔ And as designed the track would ship an **accept→MEMORY-UNSAFETY** change. Streak 0/3. | `t1394`–`t1402` |
| **D1** | 🔄 **NOT AN OWNER ASK — a sequencing dependency on `t1349`+`t1350`; the named ruling never existed in the ledger (`git log -S` empty).** `t1225`'s own text says *"do not widen the reject ahead of"* a ruling that is **NOT in the ledger.** | `t1339`–`t1348` |
| ✅✅ **G** | **INTEGRATED** (9 commits, `2d647456c`). 819 cells + 3 guards + 4 filings. Gates on the merged tree: lib 1187, lints **237**, gen-check, known-gaps census — all green. | `t1384`–`t1392` |
| ✅✅ **F1r** | **`t1362`+`t0750` INTEGRATED** (2 commits, errata folded). ⭐ Its executor caught **my** mirror list SHORT BY FOUR. | `t1363`–`t1372` |
| **F2/F3** | ✅ SCOUTED. ⭐ **BOTH GATES NOW CLEAR** — F1r INTEGRATED, and **R1 IS RULED (Line A, signature-only, 'for now')**. **`D40`+`D52` — THE OPTIMALITY CAMPAIGN, owner-opened 2026-09-05.** ⏸ **QUEUED BEHIND THE SAFETY SET** per the owner's fixed order, not blocked. | `t1362`–`t1371` |
| ✅✅ **H** | **`t1387` INTEGRATED** 2026-09-05 (`8573b12ca`+`0c137cbe0`). Output-review SIGNED OFF; 3 errata fixed at `6b4c8a544`. ⭐ **Its executor found FOUR defects in my brief; the review found a fifth in ITS filing.** | `t1409`–`t1417` |
| ✅✅ **K** | **`t1385` INTEGRATED** 2026-09-05 (`b5c5eaabc`+`1c2513e45`); errata `9d7e9f71b`. Output-review **SIGNED OFF**. ⭐ **93 cell-lanes now gated; both allowlists ratchet BOTH ways.** | `t1434`–`t1437` |
| **K2** | **`t1432`** — the ggdef VERDICT taxonomy. ✅ **ASK WITHDRAWN 2026-09-05 — `todo/t0338.md:11` already recorded the ratified pattern, and `EXIT_ILLFORMED` 102→1 shows the distinction was deliberately REMOVED.** 40 baselined rows record a claim ggdef never made; **both available fixes fight ratified ground**, and the 40 need a **THREE-way** split (out-of-subset · genuine rejection · **ggdef defect**). | `t1432` |
| ✅✅ **L** | **`t1410` INTEGRATED** (`640235fc1`). Output-review **SIGNED OFF** after re-running the decisive reverts BY LINE itself. ⭐ **The executor REFUSED a prescribed repro shape it measured green-on-arrival AND ratchet-growing — my brief was wrong and the repo's own durable-repro contract backed the executor.** | `t1445`–`t1447` |
| ✅✅ **J** | **`t1407` INTEGRATED** (`d61e5af23`+`d26af1d42`, **NOT squashed**). Output-review **SIGNED OFF** and proved the `embed_file` mechanism from source without re-running the bootstrap. ⛔ **The two commits must stay separate: the first says "do NOT integrate this commit", the second retracts it. A squash inherits the false warning and deletes its repeal.** | `t1422`–`t1427` |
| ✅✅ **E** | **`t0953` INTEGRATED** 2026-09-05 (`ef25a8ab7`-era cherry-picks); output-review **SIGNED OFF** after re-running the matrix by line AND the full 25-min sweep itself (rc 0). Errata `4611ef045`. | `t1352`–`t1358` |
⊕ **`t0036`** (the fifth CRITICAL) is **held for a later track** — its axis was CORRECTED by a second pass and the first filing was measurably too narrow, so it needs its own scout rather than being bolted onto C.
⊕ **`t1303`** (HIGH, same class as A: the working lane is the unsafe one) rides with A or B once their scouts report — **both write sites are already localized**, so it is a fold, not a track.
⊕ **`t1308`** (owner-directed) folds into whichever track first re-grades an item.

### ⚠⚠ MY OWN `/tmp` PRUNE AUDIT WAS WRONG IN THE DANGEROUS DIRECTION — **THE CITATION SURFACE IS THE PROMPT, NOT JUST THE BRIEF**

**Measured this heartbeat: ~40 GB of agent scratch live** (`/tmp` 25 GB + 14.4 GB of worktrees), on a box that
has already crashed once on disk exhaustion — **three executors and one reviewer concurrent, the round's
heaviest load.**
⛔⛔ **I built the prune list by grepping the four BRIEF FILES for each directory's basename. That marked
`rev2_a2e446ab` (814 MB) as FREE — and it is A2's pass-2 artifact set, cited by the LIVE pass-4 PROMPT I wrote
myself minutes earlier** (four partial-revert compilers + 14 probes). **Deleting it would have destroyed a
running reviewer's evidence base mid-pass.**
⇒ ⭐ **THE RULE: A `/tmp` DIRECTORY IS LIVE IF IT IS CITED BY A BRIEF *OR BY ANY IN-FLIGHT AGENT'S PROMPT*.**
The prompt is the larger surface — a brief is written once, but every launch adds citations that exist **only**
in the spawn message. ⊕ **A basename grep over briefs is exactly the "instrument that cannot SEE the class"
shape (Core #13), applied to my own housekeeping.**
⇒ **PRUNED ONLY THE PROVABLY-DEAD SET: the three D0′ directories (~1.9 GB)** — no live agent, track not active,
claims folded into this handover long ago. **Everything else stayed.**
⚠ **The four biggest survivors are all live-cited:** `scoutE_01db12fc` (8.3 GB — it holds the `target_pre` and
`target_v3` compilers E's executor is RED-verifying against), `gg_fuzz_lint_target` (5.3 GB — a live
`CARGO_TARGET_DIR` for a lint; deleting it forces a full rebuild on every `cargo test --test lints`),
`execE_01db12fc`, and `rev2_a2e446ab`. **None of them is prunable until its track integrates.**

### ⛔ A2's OUTPUT-REVIEW — **A PARITY CLAIM THAT DOES NOT REGENERATE, AND A CELL FIXED BY THE TRACK THAT NOTHING PINS**

**Design sound, code right; both blockers grow scope and neither resets the streak.** Returned to the executor.
- ⛔⛔ **"PARITY WITH STRUCT" IS MEASURABLY FALSE IN THREE DURABLE ARTIFACTS** (a `known_gaps` fixture header,
  `t1373` and `DONE.md`). The struct's read shape is `s.b.get()`; **the newtype equivalent `n.0.get()` DOES NOT
  BUILD on either backend, at PRE or POST — it never prints anything**, so it cannot print *"the same garbage"*.
  ⭐ **The TRUE statement is STRONGER:** `newtype N(Box[int])` is **unchanged** under `.get()` and **repaired**
  under `*`-deref, so **it is the struct control's INVARIANCE that attributes the residual to `t1373`.**
- ⛔⛔ **THE `Box[T]` PAYLOAD CELL IS FIXED BY THE TRACK, PINNED BY NOTHING, AND IN NO NAMED OMISSION.** Measured
  both backends: `print(*(n.0))` is **rc 1 at PRE** and **prints `9` at POST**. ⇒ **a cell going from "compiles
  on NEITHER backend" to "correct on BOTH" has no fixture — while the artifacts affirmatively claim parity.**
  ⚠ **And `Box` is the payload the track OPENED on**; every shipped newtype fixture is String/Vector/Option/
  Callable.
- ✅ **`t1378` CONFIRMED INDEPENDENTLY** — one write to `module.fn_param_abis`, **four readers all BEFORE it**,
  and `devbook/25` lists `CallByValueArg` among the classes the walker *"actually emits"* **two clauses before
  correctly flagging a DIFFERENT variant as never-constructed.** **A Tier-2a guard class structurally
  unreachable since it was written.**
- ✅ **THE CENSUS DELTA IS ZERO, REFUTING MY BRIEF'S +1** — un-ignoring the graduated fixture REMOVES a roster
  row, and the new live tests are not census rows.
- ⭐ **THE `t1376`/`t1377` COLLISION IS ADJUDICATED IN THE EXECUTOR'S FAVOUR** — its files are strictly stronger
  (`t1377` is **measured, not inferred**: `newtype Set(String)` builds rc 0 and the emitted C conflates the user
  type with the runtime map). **My two files get dropped at integration**, with one clause and one grep folded in.
- ⛔ **AND A DEFECT IN MY OWN REVIEW PROMPT: the commit range I gave swept in TWO OF MY HANDOVER COMMITS**, so
  the reviewer had to isolate the artifact itself. ⊕ **Second time this round I briefed before filing.**
- ⚠ **THREE MORE GLOB-AS-TOTAL USES NAMED** — `ls tests/fixtures/*.gg` is **2250** where recursive is **5389**,
  and the census's own "2253" is **the three subdirectories alone**, not the set its comment names.

### ⚠⚠ MA-5 RE-MEASURED FOR THE THREE PENDING INTEGRATIONS — **MY EARLIER PREDICTION NAMED THE WRONG FILE**

I predicted the collision would be the **four leak counters**. Measured across all three pending branches:
`git diff --name-only 1d91951dc..<branch> | grep -E 'LEAK_ALLOWLIST|figures.db|lints.rs'` →
⭐ **NEITHER J NOR L TOUCHES `LEAK_ALLOWLIST.txt` OR `figures.db`.** Only **E** moved the four pins, and **E is
already integrated** ⇒ **the shared-counter hazard I flagged is DISCHARGED, not pending.**
⛔⛔ **THE REAL COLLISION IS `tests/lints.rs`, AND IT IS A CERTAINTY: J AND L BOTH EDIT IT AT THE SAME LINE**
(`git diff -U0 … -- tests/lints.rs | grep -oE '^@@ -[0-9]+'` → **the same hunk start for both**). Both append a
new lint at the file's end.
⇒ ⛔ **RESOLUTION IS PRESCRIBED IN ADVANCE, BECAUSE THE MECHANICAL ONE ALREADY BROKE THE BUILD THIS ROUND:**
when C2 × F1r collided, a *"keep both sides"* merge **spliced one function into the middle of another**. The
fix that worked was to **diff each branch against ITS OWN BASE, confirm each is a PURE APPEND, and rebuild as
base + append + append** — never to resolve the conflict hunk in place.
⊕ **AND THE LINT COUNT MUST BE RE-MEASURED, NOT PREDICTED**: it has moved 237 → 238 → 239 across this round's
integrations, and two more lints are inbound. **Read it off `cargo test --test lints`, do not arithmetic it.**
⊕ **A2 touches none of the three files** — it is the clean one to integrate first if ordering matters.

### ✅✅ E INTEGRATED (7th) · J's RED RESOLVED · L REFUSED A BRIEFED SHAPE AND WAS RIGHT

- ⭐ **THREE SEPARATE EXECUTORS INDEPENDENTLY FOUND THE CENSUS RED — E, J AND L — AND ALL THREE FROZE BEFORE THE
  FIX.** ⇒ **it was visible to everyone but me, for six integrations**, because the only instrument that sees it
  is a script no `cargo` target reaches. ✅ **`known_gaps_census.sh --check` is now `rc 0` at the merged tree**
  (E closed `t0121`), read off the BARE command after the shell's own exit code proved uninformative.
  ⛔ **L filed `t1444` for that same red from a frozen base — it files a RESOLVED problem, and the reviewer is
  adjudicating whether to drop it.**
- ⭐⭐ **L's EXECUTOR REFUSED THE REPRO SHAPE MY ADDENDUM PRESCRIBED, AND MEASURED WHY.** The ordered test —
  assert `runtime error:` present plus the current value — is **GREEN ON ARRIVAL** (Core #12: *not coverage*),
  **and being green it enters the census PASS set, forcing a row into an EXACT SHRINK-ONLY ratchet whose own
  header says a row is "NEVER a place to park a graduation you did not do."** ⇒ **it shipped a test asserting
  only that the emitted C is NOT UNDEFINED — true under BOTH candidate rulings of the open `Neg` question, RED
  at HEAD, pinning no value and growing no ratchet.** **My brief was wrong; the executor was right.**
- ⭐⭐ **AND IT INVERTED THE TRACK'S OWN JUSTIFICATION BY MEASUREMENT: `uint64_t` IS NOT HARDENING.** The
  same-width fix would have left `u16 *%` UB **and made `i16 *%` NEWLY UB** — **while passing the entire runtime
  battery, 24/24 byte-identical in both directions.** ⇒ **stdout and UBSan are BOTH blind; the emitted-shape
  guard is the only instrument that sees it (24 sites, 18 violations).**
- ⛔ **AND ONE HANDED-DOWN FIGURE DID NOT REGENERATE** — a review record's *"`uint8_t` accessor → −511"*
  measures **256**. **Claim survives, number did not**; the fixture now carries the measured value **plus the
  change that regenerates it.**
- ✅ **J's BOOTSTRAP RED WAS NEVER THE CODE.** base **stage-2 rc 0** · its commit **stage-3 rc 101** · **the SAME
  commit re-run stage-2 rc 0.** ⇒ **nothing to bisect.** ⭐ **Cause: a ONE-WORD COMMENT edit to a runtime `.c`
  while the first run was in flight** — `driver.gg` bakes all 62 runtime sources via `embed_file`, so two
  generations embedded different bytes and **convergence slipped exactly one generation.**
  ⛔ **AND IT CORRECTED MY FRAMING:** *"convergence stage is deterministic, so load cannot move it"* is **true
  about LOAD and FALSE AS A LICENCE — determinism holds only if the INPUTS hold still.** Now a rule in
  `AGENTS.md`. *(Had it blamed load it would have buried a real-looking signal; had I not insisted on the HEAD
  comparison it might have "fixed" a non-existent lane asymmetry.)*
- ⭐ **`t1448`'s PRODUCING SITE IS IDENTIFIED — AND THE GUESS I REFUSED TO MAKE WOULD HAVE BEEN WRONG.** The
  bare paths come from the **CALLER** handing `env::temp_dir()` to the helper; the helper's own `work_dir`
  really is process-unique. ⊕ **The finder asked that its case NOT inflate the item** (only one bootstrap was
  running — **verified, not assumed**), and it is recorded that way.

### ⛔⛔ J's BOOTSTRAP IS **RED — AND IT IS NOT THE TIMEOUT I BRIEFED FOR**

```
BOOTSTRAP rc=101
self-host bootstrap converged at stage-3, exceeding the tighten-only ceiling
BOOTSTRAP_MAX_CONVERGENCE_STAGE=2                       (1764.80s)
```
⛔ **I told the executor a stage timeout is not a regression until compared at HEAD. THIS IS NOT A TIMEOUT** —
**convergence stage is DETERMINISTIC; sibling load does not move it.** My briefed reassurance does not cover the
failure that actually happened.
⭐⭐ **AND ITS OWN SCOUT'S PROTOTYPE CONVERGED AT STAGE-2** (926 s, recorded in the brief) ⇒ **something in the
delta between that prototype and what shipped — the `CkDeque` row, the six rot-site corrections, 10 fixtures —
COSTS AN EXTRA GENERATION TO SETTLE.** ⊕ **Prior handed to the executor to CONFIRM OR REFUTE, not accept: the
SELF-HOST MIRROR, because a convergence generation counts what the self-host EMITS**, and the new `CkDeque fill`
row plus the hint path change self-host output directly — where the runtime split-loop and the Rust arm change
what **both** stages emit identically.
⛔⛔ **RAISING `BOOTSTRAP_MAX_CONVERGENCE_STAGE` IS FORBIDDEN.** The constant's own panic message offers it as an
option — **but it is documented TIGHTEN-ONLY, and raising a ceiling to admit your own inflow is exactly the move
this project forbids elsewhere.** If the extra generation proves correct and unavoidable, that is an
**owner-facing question, not a knob.**
⇒ **The one measurement that decides it: run the same gate on pristine `1d91951dc`.** HEAD stage-3 ⇒
pre-existing, not J's. HEAD stage-2 ⇒ J's to resolve. **Then bisect the mirror half against the runtime+Rust
half.** ⭐ **The work is SAFE — `0214de358` is committed — so there is time to get this right, and I am NOT
integrating until it is settled.**

⊕ ✅ **THE SECOND SCRIPT GATE IS GREEN, NOW MEASURED RATHER THAN ASSUMED:**
`GG_STAGING_MOVE_GUARD=fatal scripts/staging_move_burndown.sh --check` → **rc 0**
(`SAMPLED=2250 TRIPS=2 CLEAN=5 promoter=fatal`). **The new every-integration rule caught one red and one green
on its first application.**

### ⛔⛔ A ROUND-CLOSE GATE HAS BEEN **RED AT MY HEAD SINCE TRACK H INTEGRATED**, AND NOTHING NOTICED

**Measured this heartbeat, rc off the bare command: `scripts/known_gaps_census.sh --check` → `CENSUS_RC=1`.**
E's executor found it first, proved it was **not its own** (reverting all three of its `src/` files leaves both
rows clean), adjudicated all four witnesses the item names — **two of them RECONSTRUCTED from the item's prose**
— graduated both rows, **closed `todo/t0121`**, and attributed the fix to **Track H's `8573b12ca`.**
⇒ ⛔⛔ **THE PROCESS DEFECT IS MINE.** `AGENTS.md` names the census as one of *"the two SCRIPT gates no `cargo`
target reaches"*. **H's output-review re-ran an extensive `cargo` gate set — and `cargo` cannot reach this one.**
I integrated H, ran `--lib` and `--test lints` at the merged tree, and **never ran the script gates**, so the
branch has been red for six integrations without a single red signal.
⇒ ⭐ **RULE, EFFECTIVE NOW: THE TWO *CHEAP* SCRIPT GATES RUN AT EVERY INTEGRATION, NOT ONLY AT ROUND CLOSE** —
`known_gaps_census.sh --check` (~2 min) and `GG_STAGING_MOVE_GUARD=fatal staging_move_burndown.sh --check`
(~1 min). **A gate no `cargo` target reaches is a gate nothing schedules; "the suite is green" is not
evidence about it.**
⊕ ⚠ **AND "THE TWO SCRIPT GATES" IS THE WRONG COUNT — CI RUNS FOUR.** Regenerate:
`grep -oE 'scripts/[a-z_]+\.(sh|py)' .github/workflows/ci.yml | sort -u` → **`known_gaps_census.sh` ·
`robustness_map.py` · `sanitize_sweep.sh` · `staging_move_burndown.sh`.** The battery in `AGENTS.md` lists
all four (the other two under their own ⊕ bullets), so **the battery is complete and its reconciliation lint
passes — `round_close_battery_covers_ci_steps` rc 0, measured.** What was incomplete was **my summary of
it.** ⛔ **The other two are unreachable by `cargo` AND expensive (~25 min each), so they stay round-close
— which is exactly why they are the MOST likely to have gone stale, the same way the census did.**
⊕ **It goes green again when E integrates** — but that is luck, not process: **it was H's fix that silently
graduated two rows, and H's own review had no instrument that could see it.**

### ⚠ J's EXECUTOR STOPPED **UNCOMMITTED** MID-BOOTSTRAP — AND ITS REPORT CAUGHT A FALSE FACT ONE STEP FROM THE PERMANENT RECORD

**~20 files of finished work sat in its worktree with the branch still at base.** Resumed via `SendMessage`;
**the worktree is intact, which is the only reason it was resumable** — this is exactly why a live agent's tree
is never pruned. **Nothing is integrable until it commits.**
- ⛔⛔ **IT MEASURED ADDENDUM 5's ERRATUM (a) FALSE, AND I WAS ONE INTEGRATION FROM RECORDING IT PERMANENTLY.**
  *"The LLVM lane is stdout-only, blind to the leak class"* was **true only of the SCOUT'S AD-HOC HARNESS, which
  never passed `--sanitize`.** Built as the repo builds it, **LLVM reports the leak identically to C** — the
  runtime is `cc`-compiled with ASan flags and only generated USER code goes through `llc` uninstrumented, and
  `gorget_array_fill` is on the **instrumented** side. ⇒ **a false, COVERAGE-UNDERSTATING fact would have gone
  into `DONE.md` as a named omission.** *(A named omission is durable content — which cuts both ways: a WRONG
  one is durable too.)*
- ⛔ **`t1421` IS A DUPLICATE OF `t1083`, AND BOTH STATED MECHANISMS WERE WRONG. THE FILING ERROR IS MINE** — I
  filed it from the scout's report **without grepping `todo/` for the MECHANISM first**, which is the rule.
  ⭐ **And the real mechanism is neither item's:** not an unsubstituted type parameter — **a by-VALUE type drop
  colliding with the by-SLOT element-drop hook** — ⭐ **and `t1083`'s own discriminator (*"must also be
  trait-equipped"*) is FALSE: `Vector[Box[int]]` emits ZERO vtable references and collides identically.**
  ⇒ **the scope is EVERY `Vector[Box[T]]`.** Folded and corrected.
- ⭐ **PLACEMENT RESOLVED BETTER THAN THE BRIEF PLANNED FOR: all 10 fixtures were MEASURED to compile AND MATCH
  on self-host, so all stay top-level** — no subdirectory cells, **no `OUT` rows, no subset gaps.** ⊕ **And it
  wired the eight droppable-element cells through `assert_gg_sanitize_clean`, so R3 and R5 are pinned INSIDE
  `cargo test` rather than only in the 25-minute sweep** — 9 of 9 RED under the over-clone revert **while stdout
  is byte-identical in all 8 rows.**
- ✅ **The census gate is RED AT ITS BASE AND NOT ITS DOING** — measured by reverting its own two source files.
  **Two `t0121` rows. Confirmed independently on my side.**
- ⚠ **Owed to me at integration:** the handover still references `t1407` and `t1421`, both of which the diff
  removes. **That block is mine, correctly not touched by the track.**
- ⛔ **AND MY "t1421 IS STILL PRESENT ON THE BRANCH" READ WAS STALE — I QUERIED THE WRONG TREE.**
  `git ls-tree <branch>` reads the **committed** tree, which was still at base because the executor had not
  committed; the fold lives in the **WORKING** tree (`git status` shows `D todo/t1421.md`, `M todo/t1083.md`).
  ⭐ **When an agent has not committed, `git status` in ITS worktree is the only true reading — a branch query
  answers a different question and looks authoritative while doing it.**
- ⭐ **IT IS HOLDING THE COMMIT ON THE BOOTSTRAP rc, WHICH IS CORRECT** (*never commit red or skipped*).
  Stage 1 done, stage 2 climbing, a monitor armed on the rc file. ⚠ **Four executors on one box is the
  round's peak load and the bootstrap is paying for it** — a stage timeout here is **NOT a regression until
  compared at HEAD.**
- ⭐ **TWO MEASUREMENTS THAT STRENGTHEN THE READINESS ROWS:** the fire count read off the emitted C shows
  **3 fill sites → exactly 1 call-site clone** (the live local; the temp and the dead local **move**), and on
  the self-alias fixture **3 sites → 3 clones** with the chain `get_at → clone_to_owned → owned temp → fill`,
  so **the runtime never receives a pointer into the receiver's buffer.** *That is clone-if-live /
  move-if-dead mechanically visible — and it IS the n-vs-n+1 optimality claim.* ⊕ **And the LLVM lane was
  RE-MEASURED on its own 10 fixtures rather than inheriting the scout's number** — all CLEAN under
  `--sanitize --backend=llvm`, stdout byte-identical to C.

### 🟢 A2 LAUNCHED — **AND ITS PASS 4 RAISED THE TRACK'S SEVERITY BY MEASURING WHAT THE TITLE EXCLUDED**

**Four passes, direction confirmed at every one, and pass 4 prototyped the whole design and measured it: every
probe correct on both backends, `--lib` 1187/0, integration 3/3, and `--sanitize` clean on all eight probes —
no double-free, no leak, no abort. The severity excursion two earlier addenda feared DOES NOT SURVIVE THE FIX.**
- ⛔⛔ **THE TRACK'S OWN TITLE EXCLUDED ITS WORST CELL. `newtype NS(String)` — a NON-generic payload — PRINTS
  EMPTY ON C AND `hello` ON LLVM AT PRISTINE HEAD, rc 0.** ⭐ **The GIR is already correct, so the registration
  half is irrelevant to it — this is PURELY the StructInit half.** The emitted C brace-elides a `void*` into a
  32-byte `Str`, so `data` takes a **stack address** and `len` stays 0.
  ⛔ **ASan CANNOT SEE IT; `gg check` is blind by construction; `*_comparison` is always-pass. ONLY A READ-BACK
  FIXTURE CATCHES THIS CLASS** — and **my title and my "no generic newtype payload exists" line JOINTLY implied
  nothing shipping was affected. That was FALSE.** ⭐ **It is also the run-level RED-at-HEAD pin readiness row 4
  said did not exist.**
- ⛔⛔ **MY SIBLING WIDENING WAS RETIRED BY THE FIRE COUNT IT OWED.** Instrumented over **2250 fixtures + a total
  11-file axis scan, both backends**: the two `SlotStore` arms fire **0 times PRE *and* POST**. ⭐ **The
  instrument was verified in BOTH directions — the same harness reports 5 → 0.** ⇒ **pre-existing dead code,
  NOT siblings — and DO NOT DELETE THEM: a deletion no row reddens is an unguarded behaviour change.**
- ⛔ **MY N1 RETRACTION DELETED A TRUE CLAIM AND INSTALLED A FALSE ONE — FIFTH FAILED NUMERIC CLAIM, THIRD
  MINE.** Pass 2 was right: the 2 `Dict` newtype payloads exist. **My grep missed them because `[A-Za-z_]*` does
  not match digits.**
- ⭐ **THE DESIGN IS EXACTLY THREE EDITS** — the `rewrite.rs` `DefKind` gate, its mandatory `typecheck.rs`
  partner, and the registration patch. ⛔ **The patch is the REGISTRATION HALF ONLY and is unshippable alone**,
  which my ⛔ header still told the executor to apply verbatim.
- ⭐ **`t1375` IS REPAIRED BY THE `^arg` MOVE WRAP** — `Box[T]` is a single-owner carve-out, so the user writes
  `NX(^b)` and the rewrite carries that into the `StructInit` so `move_zero_consumed_args` fires. **Measured
  ASan-clean; the `call @NX(copy _2)` / `drop_if_alive _2` shape is GONE.** ⇒ **`t1374`, `t1375` and `t0104` all
  CLOSE; `t1373` STAYS OPEN** — the struct control proves the residual C garbage is not A2's.
- ⛔ **SELF-HOST DECIDED: fixtures land in `known_gaps/` with LIVE tests; the SH port stays `t0400`'s.**
  A top-level fixture would make that latent gap **non-latent the same round**, and **0 of the 11
  newtype-declaring files are under `self_host_*` — the lane has NEVER been exercised on this axis.**
- ⛔ **TWO NAMED OMISSIONS FILED SO THEY OUTLIVE THE BRIEF:** **`t1376`** (`UserId(1, 2)` is un-arity-rejected
  before AND after — adding the check is an accept→reject change) and **`t1377`** (SIX-Q #4: the
  `COLLECTION_TYPES` early-return fires **before** the gate, so a name-colliding newtype is **never reached by
  any widening of it**, and the cell is **untested by anything**).
- ⭐ **THE LESSON THAT GENERALISES: SHAPE-GREPS ENUMERATE COSTUMES. THE ONLY WITNESS THAT WAS TOTAL FOR THE LIVE
  CLASS WAS THE FIRE COUNT** — three of this brief's enumerations were asserted total from greps and were wrong.

### 🟢 L LAUNCHED (5 PASSES) — AND A2's PASS 3 INVERTED MY OWN "SCOPE HAS GROWN" PREMISE

**L — design signed at every one of five passes, shape never changed.** Pass 5's closing findings:
- ⛔⛔ **THE `Neg` SPLIT LEFT THE AXIS FIXTURE RED ON ARRIVAL** — with `Neg` unfixed the neg cells fire under
  UBSan at `-O0` **and** `-O2`, and that fixture sits in the sweep at `UBSAN_CEILING=0` ⇒ **the round-close
  sweep would be `ubsan: 1`.** The cells move to a `known_gaps` repro — **which also discharges `t1443`'s empty
  `repro`** — ⚠ **and that test must NOT assert sanitizer-clean: the `runtime error:` IS the finding.**
- ⛔⛔ **TWO ITEMS I FILED IN ONE COMMIT CONTRADICTED EACH OTHER ON `INeg`**, leaving the self-host's `INeg`
  owned by **nothing** — ***precisely* the defect that same commit had just fixed for `IShr`, one variant
  across. SIX-Q #4.** ✅ `t1443` now owns `Neg` on **both lanes** (Core #9); `t1442` keeps the shift family.
- ⛔⛔ **`Inst::Shr` MUST NOT READ THE SHARED ACCESSOR — it would be a SILENT-WRONG-OUTPUT MISCOMPILE at `I64`**
  (`(int64_t)((uint64_t)l >> r)` is a **logical** shift: measured `-4` vs `9223372036854775804`). ⭐ **Narrow
  widths agree only by luck — SIX-Q #6, and exactly why *"presumably no"* was not safe as a guess.**
- ⛔ **MY CITATION FOR THE GUARD'S PREDICATE POINTED AT A FILE THAT DOES NOT EXIST.** Pass 5 supplied a real one
  **with its regenerating command**, measured to **red the same-width prototype — the one failure mode UBSan is
  blind to** — and **0 false positives BY CONSTRUCTION** (its `__v` anchor excludes the whole runtime).
- ⛔ **AND *"THE CLASS IS TOTAL AND CLOSES WITH THIS TRACK"* IS NOW FALSE** — the **walk** is total, the
  **closure** is not. **It closes across THREE items** (`t1410` · `t1443` · `t1442`).
- ⭐⭐ **WHY FOUR PASSES BOUNCED OFF ONE WRONG NUMBER:** each supplied it as a **finding**, two said *"fold it as
  an instruction"* — **but none QUOTED THE STRING.** *A fold that names a fact leaves the executor to locate the
  sentence; a fold that quotes `old` → `new` does not.* **Now quoted.**

### ⛔ A2's PASS 3 — **MY PRESCRIBED FIX WOULD HAVE REJECTED EVERY NEWTYPE IN THE CORPUS**
- ⭐ **THE FIX IS *ONE `DefKind` GATE*, NOT A LOWERING REWRITE** — `Expr::StructLiteral` is produced at exactly
  one site gated on `DefKind::Struct`, and a newtype carries `DefKind::Newtype`. ⇒ **my "scope has grown, maybe
  split" framing is INVERTED: ONE track, and NO seam exists that leaves either half shippable.**
- ⛔⛔ **BUT `typecheck.rs`'s `StructLiteral` ARM ERRORS `NotAStruct` FOR A NON-STRUCT DEF** ⇒ **admitting
  newtypes to the rewrite WITHOUT the matching typecheck edit instantly rejects all 12 shipping `int` newtypes.**
  **My fold named the gate and not its mandatory partner.**
- ⛔ **AND MY SIBLING SET WAS ASSERTED TOTAL AND IS NOT (SIX-Q #3, third time in this brief)** — **two MORE
  newtype-shape special-cases in the C emitter's `SlotStore` arm**, self-described in their own comments ⇒
  **LLVM has ONE newtype site, C has THREE.**
- ⭐ **A DECISIVE CONTROL SETTLES THE SEVERITY QUESTION:** the POST **read path is already correct** — only the
  **store** is wrong, and a struct with the identical payload prints correctly ⇒ **routing through `struct_init`
  repairs the read BY CONSTRUCTION. No Core #8 regression from the upstream fix.**
- ⛔ **A FOURTH NUMERIC CLAIM FAILED, INSIDE THE FOLD THAT CORRECTED THE THIRD.** ⭐ **The true statement is
  STRONGER than either pass reached: NO generic newtype payload exists anywhere in the repo, in any file type.**
- ⭐ **THE READ-BACK RULE HAS EXACTLY ONE LIVE VIOLATOR, AND IT IS `tests/fixtures/newtype.gg` ITSELF** —
  constructs and never reads `.0`, **green on a compiler that stores garbage.**

### ⛔⛔ A2's PASS 2 — **I FILED A SYMPTOM AS THE ROOT CAUSE, AND THE REAL ONE IS Core #2**

**It read the GIR instead of the emitted C, and the three constructor forms settle it:**
```
newtype:  _3 = call @NX(copy _2)          <- a CALL. no move_zero. and: drop_if_alive _2
struct:   _3 = struct_init S { copy _2 }  +  move_zero _2   <- the move IS emitted
enum:     _3 = enum_init E::V { copy _2 } <- source correctly not dropped
```
- ⭐ **THE OWNERSHIP MACHINERY IS CORRECT — THE STRUCT SIBLING PROVES IT.** The newtype path **never reaches
  it**: it stays a `Call` to a synthesized `extern fn`. ⇒ **filing `t1375` as `areas = ["cow"]` would have sent
  the executor to a layer where nothing is wrong.** ✅ **Both items corrected in place (`b1f58c106`).**
- ⛔⛔ **AND BOTH BACKENDS RE-DERIVE THE CONSTRUCTION BY NAME-MATCHING** (`s.name == *name && s.fields.len() ==
  1`, in **both** arms) — **Core #2 / Layering rule 2, sitting directly UNDER the defect.** ⇒ **`t1374` is the
  SAME root cause, not a sibling** — the C arm has **no pointer/value adaptation** where **LLVM's does**, and
  **that asymmetry IS the measured C/LLVM divergence.**
- ⇒ ⭐ **ONE TRACK, AND THE FIX IS UPSTREAM: lower newtype construction to `StructInit` — WHICH ALSO DELETES
  BOTH NAME-MATCHING ARMS.** *(Splitting would split ONE write-site fix across two worktrees.)*
- ⛔⛔ **MY DAMAGE ASSESSMENT ONLY MEASURED *CONSTRUCTION*. READING THE PAYLOAD BACK IS SILENTLY WRONG ON C** —
  `NO(Some(5))` prints **`none`**, deterministic; `NV`/`NS` **trap bounds on valid indices** — where PRE was a
  **loud build failure.** ⇒ **a Core #8 excursion up the ladder.**
- ⛔ **AND IT BREAKS MY OWN PIN:** I called `nt_opt` *"a valid BUILD-level pin"* — **it asserts `"built"`, so it
  is GREEN on a compiler that returns `none` for `Some(5)`.** ⇒ ⭐ **EVERY newtype fixture MUST READ THE PAYLOAD
  BACK, never print `"built"`.**
- ⛔ **THE CENSUS GATE IS ALREADY RED AT HEAD** (2 rows, both **`t0121`, not A2's**); **A2's delta is exactly
  +1.** **Say so, or the executor chases a failure it did not cause.**
- ⭐⭐ **`t0104`'s SECOND FACE *DOES* REPRODUCE — ITS SPELLING IN THE ITEM WAS WRONG.** With **no `Result`
  anywhere**, `Box[(P, int)]` + `&(*b).0.x` fails on **both** backends at PRE and works at POST ⇒ **pin 6's
  claim is TRUE and the fix graduates BOTH faces — CLOSE it.** ⊕ **Bonus run-level pin: `&(*b).1` prints `0` at
  PRE and `1` at POST — a silent wrong answer on BOTH backends that this fix repairs.**
- ⛔ **A THIRD NUMERIC CLAIM DID NOT REGENERATE** — *"33 declarations, ZERO generics anywhere"* is **17: 12 int,
  3 float, 2 `Dict`**. ⭐ **The generics are REAL but live in a FORMATTER-ONLY fixture** ⇒ the defensible claim
  is sharper: ***"no COMPILED fixture has a generic newtype payload."***
- ⭐ **DO NOT REDISCOVER THE SELF-HOST HALF — `t0400` ALREADY FILES IT**; ⊕ **`t0351` already flags the LLVM
  newtype-ctor arm**, which this fix makes live for pointer payloads for the first time.

### ⚠⚠ MA-5 PREDICTED FOR J × E — **RECORDED BEFORE EITHER RETURNS, BECAUSE THE PARENT IS THE ONE WHO RE-MEASURES**

**Two executors are live on coupled ownership machinery. File zones are disjoint; MA-5 says that settles
NOTHING.** Concretely, at the merged tree I must check:
1. ⛔ **THE FOUR LEAK PINS ARE SHARED COUNTERS, AND THIS EXACT HAZARD ALREADY BIT THIS ROUND** (C2 × F1r).
   **E moves them** (its own delta plus **retiring/tightening rows**), and **J may** (its fixtures are
   leak-gated). **Each is correct alone and possibly RED together, because each measured base+itself.**
   ⇒ ⛔ **RE-MEASURE ALL FOUR FROM THE MERGED TREE AND SET THEM FROM THE MEASURED OUTPUT — NEVER BY ADDING**
   (Core #5). The values are `assert_eq!` and figures-DB mirrored, so an arithmetic guess is a red gate.
2. ⛔ **E SWAPS THE SANITIZE GATE'S POSITIVE CONTROLS.** Until that lands, **the sweep hard-fails *"the gate's
   own instrument is broken"* BEFORE producing any corpus verdict** ⇒ **integrate E's control swap before
   running ANY sweep on the merged tree**, or the round-close battery reds for a reason unrelated to either
   diff.
3. ⚠ **A `Vector[Callable]` ELEMENT WOULD ROUTE THROUGH BOTH PATCHES** — J makes `fill`'s element a **consuming
   position** (`consuming_positions_by_name`), and E rewrites how a **closure argument** is minted and drained.
   **Neither track's fixtures exercise that intersection**, and *"a diff of two PROGRAMS cannot show whether two
   PATCHES compose."* ⇒ **build `Vector[Callable].fill` at the merged tree; it is one probe.**
4. ⊕ **Both touch `tests/sanitize/LEAK_ALLOWLIST.txt`** — E for the controls and its retirements, J for its
   fixtures. **A textual merge that resolves cleanly can still be semantically wrong: re-derive the row set, do
   not eyeball the conflict.**

⭐ **Recorded NOW rather than at integration, because the merged-tree re-measure is the parent's job and the
briefs that would have reminded me are deleted at round close.**

### ⚖⚖ A GENUINE OWNER ASK — **AND MY BRIEF TOLD THE EXECUTOR NOT TO RAISE IT** (`t1443`, non-blocking)

**L's pass 4 refused an instruction reading *"✅ NO OWNER ASK — Settled. Do not raise a question."* It was right.**
- **MEASURED AT HEAD:** `0 - INT64_MIN` **traps `T_Overflow`**; **`-INT64_MIN` SILENTLY WRAPS.** Structural:
  **`Inst::Neg` carries NO `Overflow` field**, so it has **no policy at all** — nobody ever chose this.
- ⛔ **`D30` LISTS THE FIX'S OWN CHOICE AMONG ITS *REJECTED* ALTERNATIVES** — *"defined-wrap (silent corruption +
  inconsistent with int's trap)"* — ⛔ **and `D30`'s PREMISE, *"64-bit `int` overflow already trapped"*, IS THE
  CLAIM NEGATION FALSIFIES.** ⊕ **But D30's SUBJECT is the NARROW paths and `-INT64_MIN` is not narrow — SIX-Q
  #4, a case with NO SUBJECT.** ⊕ **Unary negation's overflow semantics appear NOWHERE in the ledger** (3 hits,
  all `**` precedence).
- ⛔ **Core #8: BOTH BACKENDS AGREE ON THE WRAP** — necessary, not sufficient. **My *"LLVM is correct"* was an
  assertion, not a citation.**
- ⛔⛔ **AND THE FIX WOULD HAVE RATIFIED IT BY FIXTURE** — the top-level fixture pins `neg64`, which auto-enters
  the parity corpus **and** the sweep. **The same durable-false-content hazard the track had just refused
  elsewhere.**
- ⇒ ⭐ **DECISION: `Neg` IS SPLIT OUT.** Ship `Add`/`Sub`/`Mul` (genuinely D18/D30-ratified); **remove the `neg`
  cells from the top-level fixture.** **`t1443` carries the question with its archaeology and BLOCKS NOTHING.**
  *(Shipping defined-wrap would PRE-EMPT a ruling that may go the other way and then have to be undone.)*
- ⛔ **AND MY `-O2` HEADLINE FIGURE WAS CONTRADICTED BY THE ARTIFACT IT CITED** — five of six shapes identical
  modulo labels; **`Shl` genuinely widens to 64-bit register ops** (zero-extending → sign-extending load).
  ⚠ **`Shl` is the ONE discretionary decision AND the ONLY shape that changes** — the cost basis did not survive
  its own command.
- ⛔ **AND NARROWING `t1442` TO `IShl` ALONE WOULD HAVE LEFT `IShr` BELONGING TO NO ITEM** — same bare shift,
  same discarded `ty`, same missing trap, and the Rust lane traps both. **`t1442` widened to the shift family.**

### 🟢🟢 J AND E BOTH LAUNCHED — **AND EACH LAUNCH BRIEF HAD TO RETRACT ONE OF MY OWN RETRACTIONS**

- ⛔⛔ **J's PASS 3 RETRACTED MY ADDENDUM 4 §1: IT HAD RETRACTED A *TRUE* CLAIM.** Pass 2 read a **superseded**
  artifact (12:49, 19 rows) instead of the final one (13:40, **24 rows**), and I folded the timestamp without
  checking it. **Pass 3 re-ran all five cells itself: "LLVM identical to C, 23/23" was CORRECT.** *(Fourth
  retraction-scope error of the round — this one mine for folding a timestamp I never verified.)*
  ⭐ **TWO SUB-FINDINGS SURVIVE:** the LLVM lane's verdict is **stdout + rc only, NO sanitizer** ⇒ *"identical to
  C"* is true **on stdout** and **the lane is BLIND to the leak class**; and at HEAD the heap-view cells print
  **garbage on LLVM** where C recorded blanks.
- ⛔⛔ **J: TOP-LEVEL IS THE *ONLY* AUTOMATIC LSan GATE — AND TOP-LEVEL IS EXACTLY WHAT BOOKS PARITY INFLOW.**
  Every first-level fixture directory is `OUT`. **My "move at-risk cells to a subdirectory" fallback would have
  REMOVED THE ONLY INSTRUMENT PINNING TWO REVERTS. Two readiness rows would have died silently.**
- ⛔⛔ **E: `#[must_use]` DOES NOT CATCH ITS CLASS — MEASURED WITH rustc.** `let t = mint();` produces **NO
  WARNING**, and **every call site binds**; rustc even suggests `let _ =` as the way to *silence* it. **I had
  credited it as the guard. SIX-Q #2.** ⊕ **And the call-site-parity lint was killed for the WRONG reason —
  the fourth site is a deliberate `drop(...)`, so 3 + 1 == 4 is EXACT.**
- ⛔⛔ **E: THE SAFETY PREMISE IS FALSE FOR THE SORT FAMILY.** `SortBy`/`SortedBy`/`SortByKey`/`SortedByKey` do
  **NOT** expand to an inlined loop — they synthesize a `sort_impl` and emit **a real call passing the closure**.
  **A callee EXISTS**, and **the patch ships *"the loop is inlined"* as a code comment at all three sites.**
  ⭐ **The conclusion survives with a STRONGER statement** (the only callee is a synthesized comparator that
  calls and never stores) — ⭐⭐ **and this upgrades `attack_60` from a bonus cell to the NECESSARY pin, since it
  is the only routed shape where "a callee could retain" is a live question.**
- ⛔ **E: `attack_60` IS GREEN TODAY BECAUSE ITS HELPER RUNS `detect_leaks=0`.** SIX-Q #6 again — **the fix is a
  one-line helper flip.**
- ⛔⛔ **L: THE SCOUT'S SAME-WIDTH FIX IS A NET REGRESSION.** `I16 × Mul` is **DEFINED at HEAD** and casting
  through `uint16_t` **makes it UB** — and `U16 × Mul` was **already UB at HEAD**, so *"exactly 6 UB cells"* and
  *"every unsigned cell is C-defined"* were **wrong about HEAD, not just about the fix.** **True count: 9 of 26,
  of which 8 are UBSan-visible and exactly one is not.** ⇒ **widening is not hardening; it is REQUIRED to avoid
  shipping a new defect of the class the track exists to close.**
- ⛔ **L: `todo/t1442` WAS REPORTED MISSING — AND THAT ONE IS MINE.** It exists (`c584e1f71`) but **POSTDATES
  the reviewer's frozen base, because I wrote the brief citing it BEFORE filing it.** ⛔ **My own loop rule —
  *"file first, THEN brief"* — broken by me.**

### ⛔ A2's PASS 1 — **THE FIX'S DESIGN SIGNS OFF; MY *DISPOSITION* WAS A FALSE BINARY**

- ⛔⛔ **GATING ON `t1374` WOULD NOT HAVE PROTECTED THE SHAPE.** The reviewer read the emitted C and found the
  newtype constructor carries **TWO** defects: the address store **AND a missing MOVE of the source temp**
  (dropped, never MoveZero'd). **Fix the address store alone and both hold the same pointer and both are freed —
  still a double free, BY CONSTRUCTION.** ⇒ **filed `t1375`.** *(`AGENTS.md` names constructors a consuming
  position with "expression temp (last-use + owning by construction)" as move-eligible: the move is OWED.)*
- ⛔ **AND THE REGRESSED SET IS THREE SHAPES, NOT "Box-INNER"** — `newtype NV(Vector[int])` regresses too, **with
  a C/LLVM DIVERGENCE (LLVM clean, C panics)**. ⊕ **`t1374`'s own quoted panic and snippet were from THAT
  Vector case, so the filing contradicted its own framing.** Corrected.
- ⛔ **`nt_opt` IS ACCIDENTALLY CORRECT (SIX-Q #6)** — the **same bad store**, green only because `Option[int]`
  gets **no drop glue**. **A build-level pin for the registration fix; NOT evidence the lowering is sound for a
  droppable payload.**
- ⇒ ⭐ **THE DISPOSITION IS A THIRD OPTION: SHIP BOTH IN THE SAME TRACK** (same class — **disjoint means a
  different CLASS, not a different SITE**), **or SPLIT into two tracks in the SAME round.**
- ⭐⭐ **GOOD NEWS: SITE 3 *IS* PINNABLE — "no repro reaches it" was FALSE.** `enum E: V(Callable[int(int)])` and
  a tuple variant both differ in GIR — **and the tuple case takes the enum from `drop: None, copy: Copy` to
  `drop: Recursive, copy: Move`** ⇒ **the fix repairs a MISSING DROP (a leak) and WRONG COPY SEMANTICS, not just
  a codegen crash.** *(My own suggested shape was attributable to site 2 — not a clean pin.)*
- ⭐ **THE FIX GRADUATES A KNOWN GAP NOBODY NOTICED** (`sound_amp_box_tuple_field_cc_fail`), RED-verified both
  directions — ⚠ **but `t0104` owns that repro with an explicit *"DO NOT CLOSE"* directive on a second face the
  reviewer COULD NOT REPRODUCE.** The executor owes a regenerated disposition + a census re-run.
- ⛔ **MY "STRICT SUPERSET / BYTE-IDENTICAL" ARGUMENT IS FALSE AS STATED** — the mut path has **four extra
  auto-registering branches** and diverges on Tuple/Function/Ref. **The conclusion survives via the 2250-fixture
  GIR sweep; the STRUCTURAL argument does not.**
- ⛔ **"43 / 39" DOES NOT REGENERATE — IT IS 44 / 40.** *(Second numeric claim of this brief to fail its own
  header warning.)* ⊕ **AND MY LINE CITE WAS ALREADY WRONG — `register_newtype` is at 833, and
  `try_map_ast_type`'s OWN DOC COMMENT records the precedent: *"Both were cited by line number and both had
  rotted; cite by symbol."***
- ⚠ **THE PAIRED LINT COUNT MUST BE ANCHORED**: `grep -rn 'StructField {' src/` → **67 lines, including the
  struct DEFINITION itself** and 4 unrelated matches — **a figure its own command contradicts.**

### ⚠⚠ ROUND-MANAGEMENT HAZARD, RECORDED BEFORE IT BITES — **THE FOUR LIVE BRIEFS ARE THE ROUND'S LARGEST UNDURABLE ASSET**

**Measured this heartbeat:** `brief_J` **506 lines**, `brief_E` **404**, `brief_L` **279**, `brief_A2` **125** —
**1314 lines across four `/tmp` files, carrying 2–4 precedence-ordered addenda each.** Behind them sit roughly
**fifteen agent-hours of MEASUREMENT** that no other artifact holds: the site-attributed 5383-file census, the
`HofOp` 28-variant witness, the 24-cell fill matrix with its `n`-partition proof, the value-identity matrix over
26×26×8×3, the objdump identity at `-O2` and `-O3`, and the injected-UBSan negative result.
⛔ **ROUND CLOSE DELETES ALL OF IT** (*"scouts, briefs and review checkpoints are `/tmp`-only … round close
`git rm`s any that slipped into the repo"*).
⇒ ⛔⛔ **IF THIS ROUND CLOSES BEFORE J / E / L / A2 INTEGRATE, THEIR BRIEFS MUST BE MADE DURABLE FIRST — the
MEASUREMENTS and the NAMED OMISSIONS into their `todo/` items, not a promise to re-derive.** *"Nothing recorded
what was never run"* is how a family gets declared closed, and **a track carried across a round boundary with a
deleted brief is exactly that failure at track scale.**
⊕ **The cheap discipline, applicable now:** every pass that produces a measurement which OUTLIVES its track
should land it in the item **in the same fold**, not at launch. Several already have — `t1350`, `t1373`, `t1374`,
`t1421`, `t1428`–`t1442` — **which is why those survive regardless of what happens to the briefs.**

⊕ **Round shape, for the record:** **6 tracks INTEGRATED** (A1 · C2 · F1r · G · H · K) — already at the top of
the owner's 4–6 band — with **4 more in the gauntlet and none yet at an executor.** **No new track opens.**

### ⛔⛔ E AND L — **TWO PINS THAT PIN NOTHING, AND A RED-VERIFICATION THAT CANNOT HAPPEN**

**E's pass 3 (design signed a THIRD time) and L's pass 2 (blocked) landed together. Both found the same shape of
defect in my briefs: a guard cited as evidence that cannot fire.**
- ⛔⛔ **E: THE REVERT TABLE'S FIRST ROW PINS NOTHING — `hof_call_env_leak_unbounded` IS `#[ignore]`d.** An
  ignored test never runs. ⭐ **SIX-Q #6's sharpest instance of the round: a green-because-nobody-runs-it row,
  cited as an existing pin.** ⊕ **And variant C makes it CLEAN while it is absent from `PASSING_ALLOWLIST.txt`
  ⇒ `known_gaps_census.sh --check` EXITS 1. The fix REDS a round-close gate and the brief never mentioned it.**
- ⛔ **E: `|changed| = 3` WAS A *SITE* COUNT PRESENTED AS A *CELL* COUNT.** The `HofOp` enum is **28 variants**
  (a rustc-exhaustive witness the brief lacked); the proposed pins cover **3**. ⊕ **Two more changed cells the
  site framing hid: a DEQUE receiver** (name-stripped into the vector expander, so **one drained site serves TWO
  receiver families and only one is pinned**) **and `sort_by`.**
- ⛔ **E: THE ARM-COUNT LINT IS GREEN ON ARRIVAL AND CANNOT SEE ITS CLASS** — an arm minting via a helper leaves
  the count unchanged. ⭐ **The fix is a CHOKEPOINT: collapse the three mint sites into one that mints AND
  registers from a typed argument every arm must supply** (Core #4). **That also makes the `debug_assert!`
  deletable rather than documentable — it is DEAD BY CONSTRUCTION today.**
- ⛔⛔ **L: I ORDERED A RED-VERIFICATION THAT CANNOT HAPPEN. BOTH INSTRUMENTS ARE BLIND.** Stdout is
  value-identical across 26×26 operand pairs × 8 types × 3 ops at `-O0` and `-O2`; and UBSan, run on **the
  scout's own emitted C** in the pre-widening shape, produces **zero diagnostics** — GCC narrows the truncated
  multiply. ⇒ **the widening revert can red NO row, and gate 4 needs an EMITTED-SHAPE guard.**
- ⛔⛔ **L: THE SELF-HOST LANE HAS THE IDENTICAL DEFECT AND MY BRIEF WAS SILENT** — same three-arm shape, plain
  signed C on the fall-through; **`INeg` UB at `INT64_MIN`; `IShl` with NEITHER a count trap NOR an unsigned
  companion — strictly WORSE than the Rust lane** — **and `self_host_lowerer` is an `OUT` row, so nothing
  watches it.** **Filed `t1442`.** *(The succession plan makes that the lane meant to become the reference.)*
- ⛔ **L: MY RETRACTION WAS TOO WIDE AGAIN, AND THE TRUE SCOPE IS TWO CELLS.** *"False for MULTIPLY"* is **true
  for 8-bit multiply**; after the companion cast only **W=16** overflows `int`, and W≥32 does not promote ⇒
  **exactly `{I16,U16} × Mul`.** ⊕ **`Neg` and `Shl` are already safe at every width.**
- ⛔ **L: MY ID BLOCK COLLIDED ON ITS FIRST ID (MA-3b)** — `t1441` was already the orphan-reaper flake, **which
  the same brief tells reviewers to read.** Reissued `t1443`–`t1447`.

### ✅ J's PASS 2 SIGNED OFF — **AND CAUGHT ME FIXING A ROTTED LINE NUMBER WITH ANOTHER ROTTED LINE NUMBER**

**Streak 2/3.** The design survived the boundary it turns on; every finding is a correction or a scope-add.
- ⛔⛔ **"LLVM: IDENTICAL TO C, 23/23" IS FALSE — IT IS 19**, and the five missing cells are **exactly the
  heap-view class** (`c13`, `c14`, `c15` + two literal cells) — **the class ADDENDUM 2 had just WIDENED `t1407`
  for**, and `c14` **double-frees at HEAD**. ⇒ **the LLVM lane has ZERO measurement of that class**, and a
  Core #9 lane claim was pinned by a promise for 5 of 24 cells.
- ⛔ **I CORRECTED A ROTTED `DONE.md` LINE CITE *TO ANOTHER ROTTED LINE CITE*** — the number I wrote is a
  **blank line** — **inside the item whose own §5 says *"cite the grep, not the line"*. Fifth rotted cite of the
  round.** ⊕ "10 DOUBLE_FREE" is **9**, over 19 of 24 cells.
- ⛔ **THE ROT IS SIX SITES, NOT FOUR** — `schema.gg`'s per-`(kind, method)` owning enumeration omits `fill` **in
  the schema that DEFINES the column**, and `lower.gg`'s by-pointer list omits a call the lowerer **already had
  at HEAD**. ⚠ **AND ONE OF MY PRESCRIBED EDITS WOULD DELETE A TRUE CLAUSE** — the sentence covers
  `fill`/`get_or_put` **jointly**, and `get_or_put` genuinely still carries `[]`. **Narrow, do not delete.**
- ⛔ **NB-14 NAMES THE WRONG CLAUSE, AND THE RIGHT ONE IS *VIOLATED BY THE FIX*** — `devbook/11`'s *"a writer
  that duplicates a source must clone per slot"* is exactly what the hybrid does not do (n−1 clones, one slot
  inherits). ⊕ **NB-13 IS STALE: the "second clause" ALREADY EXISTS AT HEAD; only the GUARD is outstanding.**
- ⭐ **`n=2` IS NOT NEEDED, MEASURED** — the patch partitions `n` into {0}, {1}, {≥2}, so n=2 and n=3 are
  structurally identical and give the same verdict with **no new information**. **One sentence recording that
  {0,1,3,4096} is the TOTAL partition** saves the next reviewer re-deriving it.
- ⛔⛔ **THE BIGGEST LAUNCH RISK IS PLACEMENT, NOT THE FIX: committing ~28 cells top-level AUTO-ENROLS every one
  in the self-host parity corpus**, booking **own inflow** against a ceiling **the owner forbids raising for
  your own inflow.** Existing fixtures sit in `known_gaps/` **for placement reasons precisely to dodge this.**
- ⛔ **NB-12's REVERT LIST WAS STALE — WRITTEN AGAINST THE DEAD CANDIDATE** (its `memset` revert no longer
  exists). Replaced with **nine** revertible pieces; **R1 is unpinnable by construction, R3 unpinned, R9
  half-pinned.**
- ⭐ **KEEP THE RUNTIME ABORT.** A check-time rejection would be an **unpinnable accept→reject** — the
  configuration is unreachable, so **no fixture can turn RED** and Core #12 is unsatisfiable. **Core #10 does
  not bind: it forbids a SILENT discard, and this is a loud abort asserting a COMPILER invariant.**
- ⛔ **NB-4's SUPPORTING FACT WAS FALSE** — `gorget_shared_array_set` is a **real C function**, not
  string-emitted. **The conclusion survives and it STRENGTHENS the guard** (its root does see `t1418`'s
  function) — but an executor told otherwise scopes the root wrong.

### ⭐⭐ A2's SCOUT — **THE TRACK'S OWN TITLE WAS WRONG, AND THE FIX MAKES ONE SHAPE WORSE**

Brief `/tmp/brief_A2_v1.md`; patch `/tmp/scoutA2_a02efd745052694ac/recover_a02efd745052694ac_FIX_ONLY.patch`.
- ⭐⭐ **IT IS NOT ABOUT NESTING.** `newtype` wrapping **ANY** generic type — `Box[int]`, `Vector[int]`,
  `Option[int]`, **single level** — **fails to compile on LLVM and silently truncates the payload pointer into a
  1-byte field on C.** ⭐ **Why nobody noticed: all four newtypes in the 2250-fixture corpus wrap a PRIMITIVE.
  One sampled value on the typed axis — Core #12's anecdote, verbatim.** ⛔ **A brief scoped to "nesting" would
  ship the fix and never test `newtype NX(Box[int])`.**
- ⛔⛔ **THE COUPLING IS THE DECISION: THE FIX MOVES `Box`-INNER NEWTYPES *UP* THE SEVERITY LADDER** — silent
  truncation → **double-free** (`rc 0` → **rc 134/139**) — because making the payload field faithful **UNMASKS**
  a pre-existing defect that `_0: Unit` was hiding. **Filed `t1374`** (a newtype ctor storing a **slot address**
  into a by-value field; a **fifth route** into `t1388`'s meta-class). ⭐ **Its source site is explicitly
  UNDETERMINED — the scout measured the emitted C and refused to guess a line.**
- ⛔⛔ **`t1373` MAY BE THE MOST SEVERE THING FOUND TODAY: `Box[T].get()` THROUGH A STRUCT-FIELD RECEIVER IS
  SILENT-WRONG ON C** (garbage vs LLVM's `9`), from `struct { Box[int] }` — the plainest shape there is.
  ⭐⭐ **It INVERTS a filed measurement**: `t0685` records the cell green on both backends — **true for a LOCAL
  receiver, false for a FIELD one.** ⇒ **the axis that item enumerates is the PAYLOAD TYPE; the axis that
  DISCRIMINATES is the RECEIVER PLACE.** `t0685` corrected in place.
- ⭐ **THE FIX CANNOT ICE THE TRAIT FIXTURES, STRUCTURALLY:** `map_ast_type_mut` on a `Named` with **empty**
  generic args falls through to `UNIT_TYPE` **byte-identically**, and a bare trait name has no generic args ⇒
  **strict superset.** Corpus probe over all 2250: **15 fires → 14, exactly one row removed.**
- ⛔ **MY THREE STRUCK CLAIMS WERE DIRECTIONALLY RIGHT AND NUMERICALLY WRONG.** *"SEVEN sites"* → **4** — **and
  the grep I cited was SINGLE-FILE SCOPED**, hiding that repo-wide it is **43, of which 39 are the documented,
  INTENDED `Callable` local-form invariant.** *"No fire count"* → **it fires 12 times, the dominant site.**
  *"Would ICE ten"* → **twelve.**
- ⛔ **DO NOT REBUILD THE `UNIT_TYPE` GUARD IN ANY FORM** — **14 of 15 corpus fires are legitimate bare trait
  names**, so it green-lights nothing and reddens 12 shipping fixtures: **it fails its own class test.** The
  roster lint replaces it. ⚠ **It lands in `tests/lints.rs`, which is Track L's zone — sequence it.**
- ⚠ **TWO OF FOUR SITES ARE NOT PINNED** (`register_enum`, `monomorphize_struct`), and the scout said so rather
  than claiming coverage. **The executor owes a repro or a documented unreachability statement for each.**
- ⚠ **The self-host is ARCHITECTURALLY DIFFERENT** — it stores the inner type's **NAME**, not a TypeId, so it
  has no registration to miss. **It may still be broken by a different mechanism; probe before any top-level
  fixture.**

### ✅✅ L's SCOUT — **THE FIX IS FREE, AND MY BLAST RADIUS WAS OVERSTATED TWICE**

Brief `/tmp/brief_L_v1.md`; patches `/tmp/scoutL_a5f963b6738b7e830/recover_scoutL_{1_fix,2_fixtures}.patch`.
- ⛔⛔ **A FOURTH EMIT SITE WITH NO SUBJECT (SIX-Q #4): `Inst::Neg` CARRIES NO `Overflow` FIELD AT ALL**, so
  `-INT64_MIN` is UB and **no widening of a rule about `Overflow` variants can reach it.** ⭐ The family's typed
  witness — `grep -c 'overflow: Overflow }' src/lir/mod.rs` → **3** (Add/Sub/Mul), rustc-checkable — **is
  exactly why `Neg` is invisible.** *(A lint built on that witness inherits the blindness; it must say so.)*
- ⛔ **MY AMPLIFICATION WAS WRONG TWICE.** It is **SIX** hash sites, not four (3 inherent + 3 `equip` mirrors);
  and ***"every `@derive(Hashable)` with a String field executes this UB"* IS TOO STRONG** — the state must
  actually overflow int64, needing **~13+ characters**, and **0 of 24 hashing fixtures fire.** *(Positive
  control confirms the sweep CAN see the class.)* **Real exposure, LENGTH-GATED.** Corrected in `t1410`.
- ⛔⛔ **NO STDOUT TEST CAN EVER SEE THIS DEFECT — all 26 axis values are BYTE-IDENTICAL pre-fix.** The
  instrument must be UBSan (Core #13). Exactly **6 UB cells** = {int32,int64} × {+%,−%,*%}, plus both `neg`;
  int8/int16 are laundered by C integer promotion and every unsigned cell is C-defined.
- ⭐⭐ **THE PERF QUESTION IS ANSWERED THE RIGHT WAY: NOT A WALL-CLOCK NUMBER BUT THE DISASSEMBLY.** `-O2`
  dependent-chain, 7 interleaved reps → **−0.07%**, and **`objdump` diff = 4 LINES, all the filename.** GCC
  emits the same instructions. **Provably free.**
- ⚠⚠ **A TOP-LEVEL FIXTURE AUTO-ENTERS `runtime_parity_corpus`, AND THE SELF-HOST DIVERGES ON 21 OF 26 CELLS**
  (it treats every narrow integer as int64 — `t0655`, still live) ⇒ **top-level placement of the full axis
  would book FORBIDDEN OWN-INFLOW into `RUNTIME_DIFF_NONMATCH_CEILING`.** The fixture split is **required, not
  stylistic**: 4 self-host-MATCHING int64 cells top-level, the full 26 in `known_gaps/` **for placement only,
  with a LIVE test.**
- ✅ **NO OWNER ASK — D18 ratifies that const `+%` wraps and D30 forbids signed-overflow UB in the C backend.**
  ⛔ **But ggdef has NO wrapping variants at all** ⇒ out of subset, oracle silent — **filed `t1440`**, because a
  cell the oracle cannot see reads as *absence of disagreement*, which is not agreement.
- ✅ **`t1411` IS NOT A PREREQUISITE, MEASURED** — the sweep already classifies `runtime error:` as `UBSAN` at
  ceiling 0 with a self-tested classifier, and **H's committed repro already avoided the blind helper.**
- ⛔ **TWO ADJACENT DEFECTS FILED.** **`t1438`**: `uint8 255 + 1` prints **`0` on C** and **traps on LLVM** — a
  divergence in the **ANSWER**, not defined-ness; D30 ratifies LLVM's behaviour, and **`t0655`'s own recorded
  measurement has DECAYED** (it says 256; it is 0). **`t1439`**: a struct Dict-key link failure — **filed WITH a
  committed GREEN counter-example** (H's `Dict[Named,int]` fixture uses the very shape the symptom blames), so
  **the discriminator is explicitly UNKNOWN rather than guessed.**

### ⛔ E's PASS 1 — **THE DESIGN IS SOUND AND INDEPENDENTLY REPRODUCED; MY *CLAIMS* WERE THE PROBLEM**

⭐ **The reviewer re-ran the refusal at pristine HEAD: `iter_map_after_filter` goes from FULLY CLEAN to
heap-use-after-free, `linked_list` from an 8 B leak to attempting double-free — under a patch that is variant C
plus ONE LINE.** ⭐ **And variant C is not "3 of N" — I undersold it.** It is **3 of the 4 call sites of
`wrap_closure_call_args`** plus the correct no-op at **both** `VoidElem` sites — a **grep-total** set, with the
one exclusion excluded *because draining there is measured memory corruption.* **Core #8 cuts AGAINST the full fix.**
- ⛔⛔ **BLOCKING 1 — I FOLDED A SELECTION AS A TOTAL CENSUS, AND ITS HEADLINE ROW WAS FALSE.** The walk covered
  **2250 of 5383** fixture files; **2733 were never walked** (`closure_identity/`, `d53_unique_lock/`,
  `single_owner_subplace/`, `liveness/`, `security/`). Re-run over the excluded set, the *"0 of 2612, a real
  coverage hole"* cell **FIRES** — at a **live, `run_gg`-asserted fixture** leaking **96 B / 4 allocs**.
  ⭐ **THE INSTRUMENT WAS NEVER BLIND — THE WALK WAS SHORT.** **Retracted in `t0953` at the coverage claim's own
  scope (`0a59a0081`); the cell and mechanism are untouched.** *(SIX-Q #3, and R49's own named lesson repeating.)*
- ⛔ **BLOCKING 2 — THREE OF SIX PROPOSED FIXTURES LEAK UNDER THE FIX, WITH NO STATED PLACEMENT.** The allowlist
  is explicit that a genuinely **NEW INFLOW** row is an **OWNER ASK**, and the sweep walks **uncommitted**
  top-level files ⇒ **shipping as written REDS THE ROUND-CLOSE GATE and manufactures an unplanned owner ask.**
  Fix: the two that pin OPEN items go to `known_gaps/` (out of corpus, costs nothing); **the third dissolves.**
- ⭐ **N1 — THE GUARD IS STRONGER THAN THE BRIEF ADVERTISED, AND MY REVERT ROW NAMED THE WRONG INSTRUMENT.**
  Flipping the ownership variant **fails to COMPILE**, and suppressing that it **panics the compiler on an
  existing live fixture** — **no ASan row involved.** The pin is **rustc + `debug_assert!`**, and the proposed
  new fixture is **unnecessary**.
- ⛔ **N3 — Core #4's ARM-COUNT LINT IS MISSING, AND IT IS THE EXACT CLASS BEING FIXED.** `#[must_use]` catches a
  **caller** forgetting to drain; **nothing catches a FOURTH ARM forgetting `minted.push`.**
- ⚠⚠ **N5 COULD RESCOPE THE TRACK.** `t1349`'s filed mechanism is incomplete (the **source** predicate also
  rejects the closure, so widening the destination alone would not fix it) — and **`AGENTS.md` already says
  plain-call `Callable` params *"are simply borrowed"***, so **cell B may be LESS gated on an unratified rule
  than the filing asserts.** If it is not gated, the full fix may be shippable. **Pass 2 must resolve it.**
- ⚠ **A THIRD RECORD CLAIMS RETIREMENT ON `t0953`** (`CORPUS_MANIFEST.txt`'s `closure_identity OUT` row) — under
  variant C it does **not** retire. **The DONE.md enumeration is THREE, not two.**

### ✅✅ K IS INTEGRATED — **AND THE ROTTED-ANCHOR RULE PROVED ITSELF ON ITS OWN CORRECTION**

`b5c5eaabc` + `1c2513e45`; errata `9d7e9f71b`. Merged-tree gates, every rc off the BARE command: `--test lints`
**237 rc 0** · `--lib` **1187 rc 0** · `todo_index_is_current` rc 0 · **`--lanes all --topic "07 Dict"` rc 0 on
the MERGED tree** — the check the review specifically asked for, since H flips ASan verdicts and K seeded 22
`asan=SANITIZE-FAIL`. **H flipped none of them.**
- ⭐⭐ **THE FOUR RED-DEMO ANCHORS IN `DONE.md` HAD ALREADY ROTTED INSIDE THEIR OWN COMMIT** (off by 6). ⭐ **And
  in my merged tree they moved AGAIN, to three different lines.** *That is the argument for Core #15a's "cite
  the grep, not the number" — demonstrated twice on one artifact in one day.* Replaced with regenerating greps.
- ⛔ **`t1428`'s HEADLINE FIGURE DID NOT REGENERATE FROM THE CELL IT CITES.** It claimed 28 B / 6 allocations,
  measured on a **six-element scout variant**; the committed cell holds **three** elements and measures **13 B /
  3**. **The mechanism and the n−1 scaling stand — only the number was wrong, and it was wrong because it named
  a cell it had never been measured on.** Corrected in place with its regenerating command.
- ⭐ **THE REVIEW TESTED THE Core #8 ARGUMENT RATHER THAN INHERITING IT, AND FOUND A BETTER ONE:** with an empty
  column the scorer's `if base:` **never runs at all**, so a ggdef `WRONG` there is **nothing**; seeded, the same
  event is a DRIFT. ⇒ **there is no configuration in which the empty column reports MORE than the seeded one.**
- ⛔ **THREE ATOMS ARE PINNED BY NOTHING** — the hint correction, the doc-prose rewrites, the four unit-message
  fixes. Expected (they are prose), **but recorded in `DONE.md` now rather than only in a commit body**, which
  is the least durable place a named omission can sit.
- ⚠ **A THIRD, MILDER VARIANT OF THE REGENERATOR FAMILY:** `t1433`'s census command scopes to the **whole
  corpus** while the claim is about **the 93**. Not a grep counting its own subject — the superset entails the
  claim — but it does not ISOLATE its subject.
- ⚠ **STALENESS RISK, NOT A GATE RISK:** if Track E lands anything reaching `__gorget_closure_env_alloc`, up to
  **22 of K's asan seeds go `WORKS`** — which folds as PROGRESS and **never reds.**

### ✅✅ J's RE-SCOUT: **SHIP THE HYBRID — AND THE FALLBACK MY OWN REVIEW PRESCRIBED IS DEFECTIVE**

Patch `/tmp/scoutJ2_abbd894b5fb3c7064/recover_scoutJ2_04_FINAL_hybrid_loopsplit.patch`.
- ⭐⭐ **STRICTLY BETTER ON EVERY AXIS MEASURED, not marginally.** 24-cell matrix: HEAD **11 memory findings +
  4 silent-wrong** → hybrid **23/23 CLEAN**, LLVM identical; allocations **n** (v3: n+1), and **n−1** for a
  static literal because the last slot keeps the immortal view. ⭐ **THE HYBRID INTRODUCES NO SCRATCH BUFFER AT
  ALL, so `t1419`'s hazard class is STRUCTURALLY ABSENT rather than mitigated.** Bootstrap converged.
- ⛔⛔ **`v3` — THE FALLBACK PASS 1 PRESCRIBED — STILL DOUBLE-FREES.** Its snapshot clone is guarded
  `if (arr->elem_clone)`, so with no clone hook the snapshot is an **unowned copy**: every slot aliases it and
  the `n == 0` arm frees the caller's payload. **Measured. A "safe fallback" that was never safe.**
- ⛔ **AND ADDENDUM 1's BLOCKING 1 WAS WRONG ON ITS FACTS — MINE.** I wrote *"nothing says 'deliberately'"*;
  `methods.rs:2795-2800` **says it verbatim.** ⭐ **But its PREMISE is false** — it justifies the exclusion with
  *"`fill` clones its value per element internally"*, **which it never did.** ⇒ **Core #14 rot, not an
  unexplained absence — and the conclusion is STRONGER, because the recorded design is the one the ratified
  2026-04-11 runtime contract rejects.** ⊕ **The same false premise is DUPLICATED in the self-host.**
- ⭐ **AND THE `t0872` `known_gaps` CELL IS NOT OWED** — the brief's own discriminator was *"a temp at a
  NON-CONSUMING position"*, so **making the position consuming REMOVES it**: those cells are **clean under the
  hybrid and leak under v3.** *(Do not ship a `known_gaps` entry for a gap the fix closes.)*
- ⛔ **THE SELF-HOST LANE IS MANDATORY, AND IT IS THE ONE REAL RISK: THE RUNTIME IS SHARED.** A runtime-only
  change without the lowering mirror **IS** the measured "revert the lowering half" column — **8 double-frees,
  3 UAFs, and a NEW defect where the `n == 0` arm frees a live source it does not own.** ⊕ The mirror adds a
  **`CkDeque` fill row THAT DID NOT EXIST**, and the existing symmetry lint **cannot see it: its roots are Rust
  dirs and its predicate is `strip_prefix("Vector__")` — SIX-Q #4, no subject.**
- ⭐ **SUCCESSION-PLAN DATUM: THE SELF-HOST ALREADY FIXED THE Core #2 SMELL.** Rust name-matches
  (`consuming_positions_by_name`) where the check-time gate for the same axis is typed; the self-host carries a
  typed `owning_arg_positions` column **and names the Rust list as the lagging reference.**
- ⛔ **`Vector[Box[T]]` DOES NOT COMPILE FOR ANY `T`** (`redefinition of 'Box__T__drop'`) — filed **`t1421`
  (HIGH)**. It is the only surface route to `elem_drop && !elem_clone`, so **no fixture exercises that runtime
  configuration at all.**
- ⛔ **`t1407` WIDENED: any HEAP-VIEW source prints blanks** (`s[a:b]`, `v[i]`, `.get()`), **ASan-invisible**.
  The two self-alias cells were merely the loud ones.

### ⚠ INTEGRATION HAZARD RECORDED — **K's BRANCH PREDATES H's DELETION OF `t1387`**

Measured, not anticipated: `git ls-tree --name-only worktree-agent-a413b5cda3b617268 todo/t1387.md` **still
returns the file.** K was cut before H integrated, so K's regenerated `TODO.md` index contains a `t1387`
pointer to an item that no longer exists on the integration branch.
⇒ **`TODO.md` WILL CONFLICT on K's cherry-pick, exactly as it did on H's.** ⭐ **THE RESOLUTION IS FIXED AND
MECHANICAL: keep the integration branch's `TODO.md` (`git checkout HEAD -- TODO.md`), then REGENERATE the index
(`python3 scripts/todo_index.py --write`) — never merge the index text.** The regenerator drops the stale
pointer on its first pass and *then* reports it, so a second run is what confirms green.
⛔ **DO NOT hand-delete the pointer line by number** — a line-number edit against the regenerator's output
asserted the wrong line once already today, and the assert is what caught it.

⊕ **ID AUDIT, run this heartbeat: 13 issued blocks, ZERO duplicate ids, ZERO cross-block collisions**, 96 ids
free inside issued blocks. `t1387` and `t1433` show only on K's un-integrated branch — **the watermark must be
read across BRANCHES, not just the integration tree**, or the next block reuses a live id (MA-3b).

### ⛔⛔⛔ TRACK E's SCOUT BUILT THE FILED FIX AND MEASURED IT MAKING THINGS **WORSE**

**The most valuable scout result of the round.** Brief `/tmp/brief_E_v1.md`; recommendation
`/tmp/scoutE_01db12fc/recover_01db12fc_variantC.patch`.
- ⛔⛔ **FREEING THE CLOSURE-ARG TEMP TURNS THE LEAK INTO A USE-AFTER-FREE AND A DOUBLE-FREE.** The lazy
  iterator adapters **RETAIN** the closure; `sanitize_sweep.sh` goes `corruption: 1 → 5` on four corpus
  fixtures. ⭐ **SIX-Q #6 at full strength: those cells are green today ONLY because the caller's temp is never
  freed. The leak is not a mitigation — it is a SECOND DEFECT CANCELLING THE FIRST.**
- ⭐ **THE REAL WRITE SITE IS IN THE STDLIB**: `lib/std/iter.gg`'s `map`/`filter` store a **borrowed** `Callable`
  parameter into a returned struct **without cloning**, because the parameter is a **generic `F`** that
  `pack_closure_at_dest_type`'s destination predicate never sees. **Two owners, one drop.** ⇒ **filed `t1349`
  (HIGH)**, and it shares its gate with `t1225`'s callee-borrow sequencing.
- ⭐ **THE CENSUS IS TOTAL — 2612 fixtures, six cells, every reachable one.** `wrap_single_closure_arg`
  drop-registers in **none of three arms** and has **TWO CALLERS WITH OPPOSITE OWNERSHIP SEMANTICS AND NO
  PARAMETER DISTINGUISHING THEM** — the invariant is dropped on the way in (**layering rule 1**). ⊕ **The
  reference-grade idiom already exists one file over** (`printf_str_temps`: temps *"born below GIR drop
  registration"*, so the call emitter frees them).
- ⛔ **THE ITEM'S SCOPE WAS TOO NARROW.** A packed `Callable` **LOCAL** at a call arg leaks identically via a
  different arm ⇒ **a GIR-level fix converts a case-2 leak into a case-2a leak, MEASURED.** That cell fires in
  **0 of 2612** fixtures — **a real coverage hole.**
- ⚠⚠ **THREE OF THE SANITIZE GATE'S POSITIVE CONTROLS ARE MANUFACTURED WITH THIS EXACT BUG.** With the fix in,
  the sweep **hard-fails `❌ THE SANITIZE GATE'S OWN INSTRUMENT IS BROKEN` before producing ANY corpus verdict.**
  Its README anticipates it — *"re-point the controls … in the same commit that retires the old one"* — but
  **says TWO where the measurement says THREE**, and `selftest_leak_twice` needs two distinct call **SITES**
  because **LSan merges identical stacks.** A bug-independent replacement is prototyped and verified.
- ⛔ **THE FORCING FUNCTION IS HALF-TRUE: under the safe subset only ONE of the two owner admissions retires.**
  The other is a direct-call-argument fixture — the blocked cell.
- ⛔ **AND MY OWN FIGURES WERE WRONG: 84 of 293 rows carry the frame and 3 cite the item, not 93/301 and 17.**
  I had run `grep -c` over a file whose **comment prose** contains the string. Corrected in the item.

### ✅✅ H IS INTEGRATED — **AND ITS OUTPUT-REVIEW FOUND A FIFTH DEFECT, IN THE EXECUTOR'S OWN FILING**

`8573b12ca` + `0c137cbe0`; errata at `6b4c8a544`. `--lib` 1187 and `--test lints` 237 both **rc 0 off the bare
command at the MERGED tree** — the four leak pins validate against the committed file, not against arithmetic.
- ⭐⭐ **MY "TWO HALVES" FRAMING WAS UNDER-SPECIFIED — IT IS SIX PARTIAL REVERTS**, three map modes × two
  wrappers. **A reviewer who checked only the two halves would have concluded the UNORDERED mode was
  unguarded.** All six enumerated: four red, one (`R2`) is **unreachable dead code** and therefore
  *unpinnable* — the honest answer, not a coverage hole.
- ⛔⛔ **THE 12 ALLOWLIST TIGHTENINGS ARE NOT BOOKKEEPING.** `test_hashmap_all str_alloc_copy*2` is the **SOLE
  guard for the entire unordered mode** — the new fixture pins only the DENSE mode. Reverting either unordered
  half takes it to `*3` and reds the sweep. **Nothing in the tree said so** ⇒ annotated in the allowlist, or a
  future round loosens a "noisy" row and silently unpins half the fix.
- ⛔ **AND THE FIFTH DEFECT IS IN `t1411`'s OWN FILING: "50 gates" is 44.** Its regenerating grep **counted the
  function's own DEFINITION** — *a regenerator that includes its own subject is not one* (Core #15a) — and it
  added in `assert_box_deref_asan_clean`, which builds `-fsanitize=address` **only** and so was **never
  configured to emit a UBSan report at all.** ⊕ **22 of the 44 are live, 22 `#[ignore]`d.**
- ⛔ **A COMMENT THAT CONTRADICTED THE NUMBER IT EXPLAINS**: *"the eight retired rows take one pair"* — they
  take **eight**; 8 + 3 = 11 **is** the 498→487 delta.
- ✅ **Core #8 ADJUDICATED: filing `t1410` was CORRECT, not a dodge** — different file, layer, invariant and fix
  shape from map-key ownership, and the escape hatch was taken properly (a `known_gaps` repro asserting the
  **intended** behaviour). ⭐ **The repro is genuinely red, NOT accidentally green** — a naive probe is
  constant-folded, which the executor caught itself (SIX-Q #6).
- ⚠ **The review REFUSED to inherit the 25-min sweep number** on a loaded box, and ran a targeted 21-fixture
  sweep instead. **Saying "I did not run it" is the right answer**; the full sweep is the parent's at round close.

### 🚀 TRACK L OPENED — `t1410`, **A STDLIB-WIDE UB THE ROUND ALMOST CARRIED FORWARD**

The output-review's orchestrator finding: *"per 'SPLIT also when the scope genuinely grows too much — each half
becomes its own track in the SAME round; a split is division, never deferral', that should be a Track in this
round."* **Measured before opening: `lib/std/hash.gg`'s `FxHasher` is `state *% 31 +% input` at FOUR sites**
(`grep -n '31' lib/std/hash.gg`) ⇒ **every `@derive(Hashable)` with a String field runs it.**
⚠ **`t1411` may be a PREREQUISITE** — if the gate cannot see UBSan, what pins the fix afterwards? The scout owes
a measured answer.

### 🟢 K's EXECUTOR IS LAUNCHED — **3/3 — AND PASS 4 CAUGHT A SECOND RED-ON-ARRIVAL ASSERT ONE LEVEL DOWN**

**Four sequential passes; 2, 3 and 4 all signed off, and each found real defects in the earlier text.**
- ⛔⛔ **THE SIX-Q #4 ASSERT PASS 3 ASKED FOR WOULD ITSELF HAVE BEEN RED ON ARRIVAL.** `RMAP_LANE_COLS`
  **includes `(2, "c")` and `RMAP_COL_C == 2`** — a CONTROL row's `c` column holds the literal `"CONTROL"`,
  **not empty** — so *"no CONTROL row has a populated lane column"* **fails on both rows immediately.** ⭐ **The
  same trap as pass 3's, one level down, in the fix pass 3 proposed for it.** Skip `*col == RMAP_COL_C`.
- ⛔ **TWO INSTRUCTIONS INSIDE ONE ADDENDUM CONTRADICTED EACH OTHER.** R2 said *"do not delete the tail loop
  wholesale"*; R3, folded in the same breath, **subsumes both halves of it.** Precedence cannot break a tie
  inside one generation ⇒ **retired R2's clause BY NAME; the whole loop goes.**
- ⛔ **AND MY *"TOTAL — exactly five hits"* WAS FALSE, WITH NO REGENERATING GREP — the same defect I had just
  corrected in myself one addendum earlier.** Six sites, not five. The sixth is **`TODO.md:133`**, do-not-touch
  for a **different** reason (**MA-3**) and correct as-is because it **describes** the broken hint rather than
  prescribing it.
- ⛔ **FOLD 6 FIXED ONE OF *FOUR* SIBLING UNIT SLIPS** — `progress`, `drifts` and `seeded` all append per
  **lane** while the summary calls all three **"rows"**. **Core #4: fix the class.** The slip had already
  propagated into `t1385`'s own text.
- ⭐⭐ **K2 IS NOW A FILED ITEM (`t1432`), NOT A PARAGRAPH IN A `/tmp` BRIEF.** Pass 4 caught that its entire
  content — the owner ask, the three-way classification, the unfoldable regression row — lived **only** in a
  brief round-close deletes, while K removes the only item covering that ground. **"A named omission is durable
  content"**, caught one step before it stopped being true.

### ✅ K's PASS 3 SIGNED OFF — **AND CAUGHT TWO THINGS THAT WOULD HAVE SHIPPED BROKEN, BOTH MINE**

**Streak 2/3.** Nine non-blocking; the design is untouched.
- ⛔⛔ **MY PRESCRIBED ASSERT WAS RED ON ARRIVAL.** Both CONTROL rows carry **four** empty lane columns, so the
  naive unbaselined set is **33, not 31**, and the set-equality fails the moment the list is emptied. ⛔ **And
  the file DOCUMENTS THIS EXACT TRAP 130 LINES ABOVE** — *"written as a by-name allowlist this assertion goes
  red the moment anyone adds a control … red on arrival, from its own required precondition."* **I prescribed
  the form and dropped its precondition.** Fix: build the set with the same `CONTROL` predicate the sibling
  loop already uses, never a name list.
- ⛔ **MY RETRACTION WAS ONE SENTENCE TOO WIDE — the rule I wrote this round, broken by me twice now.** The
  retracted block had two clauses; the second is genuinely dead, but the first (*"if the ggdef half is bigger
  than measured, come back and say so"*) **is still TRUE for K2**. ⇒ **retract clause (ii); clause (i) is
  TRANSFERRED, not retracted.**
- ⛔ **MY Core #15a SLIP, ALSO TWICE:** the MA-5 grep I quoted returns **six** paths, not four. **The verdict
  survives** — the three extras are a different manifest, a `.gitignore` line, and a cell-count counter K does
  not move (K changes lane COLUMNS, not the cell count) — **but the sentence did not regenerate its own claim.**
- ⛔ **AND I MIS-ATTRIBUTED THE STALE PROSE.** *"The 31 named exemptions … predate those lanes"* is in the
  **FUNCTION's rustdoc** — the paragraph a reader hits first — **not** the const's. Following me literally
  would have fixed the const and left the visible claim standing: **Core #14 rot surviving the round that
  retired it.**
- ⭐ **AND MY `DECLARED_HELPERS` SPLIT WAS A Core #4 DODGE.** *"A different list about helpers"* is a
  **subject-matter** distinction; **disjoint means a different CLASS**, and the class is **Core #6's "a ratchet
  needs BOTH directions."** Three declared allowlists in ONE function, one ratcheted both ways and two not.
  ⇒ **folded in, `t1432` NOT filed.** Today a stale helper entry **pre-authorises the next orphan that takes
  the name** — verbatim the hazard the sibling list's own message describes.
- ⛔ **A NAMED OMISSION HAD NO DURABLE HOME.** The `{¬seed ∧ ¬delete}` revert is pinned by **no `cargo test`
  row**, and I told the track to say so *in the commit message* — but `t1385` is `git rm`'d this round, so the
  omission would die with it. ⇒ **it lands in the rewritten lint doc comment instead.**
- ⚠ **I NAMED THE RARER LOAD HAZARD.** Seeding at `good` then measuring non-good is a permanent unfoldable
  REGRESSION — real, but the **LIKELY** artefact is the mirror: **a lane times out, seeds a false non-good, and
  is later foldable as PROGRESS, so it NEVER REDS ANYTHING.** Both directions now stated.
- ⭐ **SIX-Q #4, a case with NO SUBJECT: a CONTROL row with a POPULATED lane column.** The runner `continue`s
  before the lane loop, the controls' own assert checks only the NAME set, and no widening of the exemption
  rule reaches it. **Vacuous today — but silence is not the disposition.**
- ⭐ **SIX-Q #6: `hof_dict_filter_untyped`'s own note says `accidentally correct at HEAD for the same Str/int
  reason`** — and it is **one of the three rows taking a knowingly-false `ggdef=REJECTED`.** **Two soft-false
  signals on one row; the commit must say the word.**

### ✅ K's PASS 2 SIGNED OFF — AND PROVED SIX-Q #3 **STRUCTURALLY**, WHERE PASS 1 HAD ONLY MEASURED IT

**Streak 1/3.** No blocking findings; seven folds, all scope-growth inside a sound design.
- ⭐⭐ **THE CLINCHER IS EXACT, VERIFIED FOUR WAYS** — the `good` derivation; the 3 rows' `expected` (none
  starts with `REJECTED` ⇒ third-quadrant DRIFT); that **both target topics genuinely carry uncoded rows K2 must
  fold anyway** (all six `ggdef=REJECTED` rows in topics 07 and 11 were RUN, not assumed, and all six are
  uncoded); and that **the ggdef wall is confined to a topic K never runs** (of six expected-`REJECTED` rows
  corpus-wide, **five are CODED** and untouched by K2).
- ⭐⭐ **THE HIDDEN-SHAPE ABSENCE IS NOW STRUCTURAL, NOT MEASURED.** Pass 1 rested on the observed selfhost
  bucket; pass 2 proved it for **every possible** bucket: only `selfhost` can create a divergence;
  `BUILD-FAIL`/`REJECTED` collapse to bucket-only so keys match whatever selfhost measures; the two already-
  `DIVERGENT` rows are pinned forever; a `WORKS` key carries `actual == expected`, identical across lanes —
  **except** under a `loud failure` expectation, of which **21 exist corpus-wide and none is `hof_*`**.
- ⛔ **THE DANGEROUS REVERT HAS NO CARGO-TEST PIN AT ALL.** `{¬seed ∧ ¬allowlist-delete}` is **GREEN** under
  `cargo test`; it is pinned **only** by the four `--lanes all --topic` runs going rc 1 → rc 0. **That must be
  said in the commit, or the next reader thinks the suite covers it.**
- ⛔ **THE BROKEN HINT IS AT *TWO* SITES, AND I HAD NAMED ONE.** The const's **own doc comment** prescribes the
  refuted command three lines above the list — and still says the rows *"predate those lanes"* (refuted) and
  *"The 31 named exemptions"* (false once emptied).
- ⛔⛔ **AND A NAIVE Core #4 SWEEP WOULD BREAK THE ONE PATH THAT WORKS.** The same five-lane command is
  **CORRECT** in three other places, because topic 30's rows are born with **every** value lane empty so
  `first_seen` is True. ⇒ **the correction is a PRECONDITION, not a lane count.** *(A textbook SIX-Q #4: the
  rule's subject does not cover those sites at all.)*
- ⭐ **THE SCRIPT ITSELF IS THE SOURCE OF THE UNIT SLIP** — `seeded.append` sits inside `for lane in lanes:`
  while the message prints `"rows SEEDED"`. **The script calls cell-lanes rows**, and every reader inherits it.
- ⚠ **A SHAPE NOBODY HAD NAMED: a FLAKY LANE measured under load and seeded at `good` becomes a permanent
  REGRESSION that NO flag can fold** — both `--accept` and `--accept-drift` refuse while `regressions` is
  non-empty. The confirmation runs are the mitigation; **say that is what they are for**, and set both timeouts.
- **Errata:** the K2 sizing figure is **55** `Halt::IllFormed` sites, not 62; the base carry-forward touches
  **eight** `todo/*.md`, not three (**the load-bearing half — no `src/`, `scripts/`, `tests/` — holds**).

### ⚖⚖⚖ TWO OWNER ASKS ARE OPEN — BOTH ISOLATED BY A REVIEWER, BOTH RATIFIED-TAXONOMY QUESTIONS

**Round-lifecycle 7(ii). Neither is derivable from the docs below the ledger; both were nearly derived anyway.**

**ASK 1 — TRACK B, the CAPTURE cell.** `decisions.md:1605` says of a closure capturing a
`Callable`/`Box`/`Owned`/`Task`/`Guard`: *"clone breaches the carve-out, **move breaches it under D31**, and
reject cannot be spelled until D7's capture list exists."* But **D31 full-strict says non-`!` consuming
positions still AUTO-MOVE-WHEN-DEAD (CoW, not contract).** ⇒ **Does the DEAD-source move retire `:1605`'s
"move breaches it"?** Both cannot hold for a capture whose source is provably dead.
⛔ **B's brief answered this ITSELF from `language-design.md` §3.5 — a doc ranked BELOW the ledger — and I
folded it. That is the precedence order inverted, on this same cell, for the second time.**

**ASK 2 — TRACK K2, the ggdef VERDICT taxonomy.** `run_ggdef` scores an uncoded `Outcome::IllFormed` as
**`REJECTED`**, i.e. *"the definition rejects this program"* — a claim ggdef never made, on **40 baselined
rows**. Both available fixes fight ratified ground: a reject code contradicts a **RED-pinned** invariant
(*"eval-internal IllFormed carries no ratified code"*), and an out-of-subset marker needs a distinction the
**owner-ratified, FIRM** exit-code scheme deliberately collapses (*"`1` static rejection (parse OR semantic OR
may-move IllFormed — **ONE class**)"*). ⇒ **Does the exit-1 "ONE class" grow an out-of-subset distinction?**

### ⛔⛔ PASS 1 INVERTED MY OWN SCOPE DECISION ON K — AND THE CLINCHER WAS A MEASUREMENT

I grew K to *"fix ggdef FIRST, then seed"* and marked it **NOT OPTIONAL**. Pass 1 refuted it:
- **My cost model was FALSE.** `good` is not always `WORKS` (`good = "REJECTED" if expected.startswith(...)`),
  so one row is a **REGRESSION, not a drift** — and a regression **refuses the whole write**, blocking 7 other
  rows in its topic. **The executor would have hit a wall my brief said could not exist.**
- **My Core #2 call was directionally right with the WRONG DISCRIMINATOR**: `run_ggdef` **already** matches
  three stderr substrings, so *"a fourth is name-matching"* condemns the three already there.
- ⭐ **THE CLINCHER: seeding FIRST costs the ggdef track ZERO.** The 3 rows' `expected` values don't start with
  `REJECTED`, so after the classifier lands they are **DRIFT**, folded by two topics K2 must already run.
  Withholding the seed keeps **93 cell-lanes ungated AND CI red** — strictly worse on every axis.
- ⛔ **AND THE DANGEROUS PARTIAL REVERT WAS ONE I NEVER ENUMERATED**: `{seed lands, the 31 allowlist entries
  STAY}` is **GREEN** — the lint `continue`s on listed names *before* the empty-lane check. A list documented
  *"SHRINK-ONLY"* with **nothing enforcing the shrink** (Core #6, both directions). K now lands the reverse assert.
- ⛔ **`error: unresolved local \`Direction\`` (7 of the 40) is a ggdef DEFECT, not a subset boundary.
  Flipping it to `NO-VERDICT` LAUNDERS A BUG INTO A DECLINE.** K2 owes a **three-way** classification.
- ⭐ **SIX-Q #6, and it is the same cell as the wall:** `doc_b03_match_nonexhaustive_neg` claims to test
  *"REJECTED: non-exhaustive match"* and ggdef scores it `REJECTED` because **it cannot resolve the enum** —
  it never adjudicated exhaustiveness at all. **Green for a reason unrelated to what it tests.**

### ⛔⛔ J's PASS 1 KILLED `v2b` — THE SNAPSHOT IS THE READ-SITE SHAPE, AND THE `memset` HAS NO RED ROW

It first **reproduced thirteen** of the brief's claims, including the LSan-root one (`HEAD 140 B/8` ·
`no-memset **105 B/6** — two allocations silently suppressed` · `v2b 140 B/8`). Then:
- ⛔ **THE PREMISE WAS ASSERTED, NOT VERIFIED.** *"`fill` is deliberately NOT a consuming position"* rests on
  `fill` being **ABSENT FROM A NAME-MATCH LIST**. Nothing says deliberately; **`AGENTS.md` says the opposite** —
  *"the POSITION is the rule"*. *(SIX-Q #1 answered the wrong way round: an unexplained absence read as ratified.)*
- ⛔ **AND THE EMITTED C SHOWS THE WRITER**: the call site hands the runtime `gorget_array_get_at(...)` — **a raw
  pointer INTO the receiver's own buffer** — where `push` two lines up hands `&__s8`, **an owned temp.**
- ⛔ **v2b's snapshot is LITERALLY the `save/restore` shape Core #1 names as a read-site patch**, and it
  contradicts the ratified 2026-04-11 runtime contract: *"no internal deep-clone. **Compiler owns independence
  at the call site.**"*
- ⛔ **The `memset` can have NO RED ROW** — dropping it makes the suite **MORE GREEN** (105 B/6 vs 140 B/8).
  SIX-Q #2. The reviewer **measured an automatic (stack) buffer instead** — `use_stacks=0` already disables it
  as a root — `140 B/8, 3/3`, no memset needed. **That is the fallback; `v2b` is dead either way.**
- ⭐ **THE HYBRID (now being measured by J2): make `fill`'s element a CONSUMING POSITION.** Clone-if-live /
  move-if-dead once at the call site + runtime clones n−1 ⇒ **both defects close at the WRITE site**, no
  snapshot, no static root, and **n allocations instead of n+1** — a charter question under the optimality pivot.
  ⭐⭐ **AND ggdef ALREADY HAS THIS SHAPE**: `repeat_n` **consumes** its argument and clones n−1.
- ⛔ **v2b is UNSOUND when `elem_drop != NULL && elem_clone == NULL`** — a configuration the file documents as
  legitimate: N aliases survive **and** the `n == 0` arm frees the caller's payload from a never-cloned
  snapshot, **a NEW double-free introduced by the fix.**
- ⛔ **The registry is NOT a total witness** — `gorget_shared_array_set` is string-emitted from
  `src/backend/c_lir/helpers.rs` and has **no registry row**. Wider witness: loop-invariant `memcpy` across
  `src/backend/c/runtime/*.c`.

### ✅✅ TRACK K SCOUTED — **A GREEN `--lanes all` IS TWO THINGS AWAY, AND THE GUARD'S OWN INSTRUCTIONS DON'T WORK**

Brief `/tmp/brief_K_v1.md`; full triage `/tmp/scoutK_t1385_scout/CHECKPOINT.md` + `seed.patch` + 7 scripts.
- ⭐⭐ **THE HEADLINE.** Whole-corpus `python3 scripts/robustness_map.py --lanes all --detail --jobs 6` (26m01s,
  rc 1) = **1 REGRESSION + 11 NEW DIVERGENCE + 0 FATAL DRIFT.** The regression is **`t1387` = Track H**; **all
  11 divergences are inside K's 31-row set.** ⇒ **H + K green the five-lane CI step. Nothing else is in the way.**
- ⛔⛔ **THE LINT TELLS YOU TO RUN A COMMAND THAT REFUSES ON THE EXACT ROWS IT NAMES.** `tests/lints.rs`
  prescribes `--lanes all --accept --seed-new --topic "<topic>"`; measured, it **refuses the write wholesale**
  (`first_seen` needs *no* value lane populated, and `c`/`llvm` are). That is what an earlier revision of
  `t1385` hit when it recorded *"`--seed-new` does not help"*. **SIX-Q #2 — a guard that cannot be discharged
  by its own instructions.** The three-lane form works because the divergence set becomes a singleton.
- ⛔ **ggdef's SCORER STATES A CLAIM ggdef NEVER MADE.** `Outcome::IllFormed` with no reject code prints a bare
  `error:` and falls into the scorer's **`REJECTED` default** — the script's own docstring calls this *"the worst
  available outcome."* **3 of the 31 seeds land there; a census of all 72 baselined `REJECTED` rows found 40
  more.** ⇒ **K's scope GREW to fix it at ggdef** (an out-of-subset marker), **not** by adding a fourth stderr
  substring to the script — that would be **Core #2 name-matching to decide meaning.**
- ⛔ **`t1385`'s `mechanism` FIELD WAS FALSE AND I CORRECTED IT IN PLACE.** The 31 rows do **not** predate the
  lanes: all were created by **one commit `ef171a34a` (R49 Track F, 2026-09-04)**, nine days after the lane
  schema and eight after five-lane baselining. **This is LAST ROUND'S INFLOW, not legacy debt**, and
  `LEGACY_UNBASELINED` is a misnomer.
- ✅ **THE COLUMN-COPYING SUSPICION IS ANSWERED: NO.** One writer per lane column, five distinct runners; 0
  FATAL DRIFT over 819×5 means every recorded bucket reproduced today. ⭐ **And the counter-example is inside
  the seed set — 22 rows measure `asan=SANITIZE-FAIL` while `c` is `WORKS`; a copy cannot produce that.**
- ⛔ **`--topic` IS MANDATORY**: the four scoped runs fold **0** foreign rows; a bare `--lanes all --accept`
  folds **689** (real uncollected progress, not this track's to collect).
- ⭐ **THREE UNFILED DEFECTS, NOW FILED**: **`t1428`** (HIGH — `fold`'s accumulator leaks every intermediate,
  **unbounded**, both backends; isolated from `t0953` by a **named-callee negative control**), **`t1429`** (HIGH
  — self-host `Option[String].map` prints `0` vs `5`, **silent wrong output** from an incompatible fn-ptr call:
  the **PARAMETER** half of `t0877`, whose return half is a loud BUILD-FAIL), **`t1430`** (MED — one cell burns
  **~15 of the 26 minutes** on four lanes, and `HANG_CENSUS` is **C-lane only**, a selection).
- ⭐⭐ **3 OF THE 11 DIVERGENCES ARE THE SELF-HOST BEING RIGHT AND RUST gg WRONG** (`hof_for_each_strings_*`,
  `t0987`'s class) — **a succession-plan data point: fix the Rust side as oracle hygiene.**
- ⚠ **MA-9's shared-predicate deadlock fired a THIRD time** — two watchdogs spinning on `pgrep -f` patterns
  that matched **each other's own command lines**, alive 4h21m. **Filed `t1431` with a lint ratchet design**;
  prose alone has now failed three times in one round (Core #6).

### ✅ TRACK J SCOUTED — **TWO DEFECTS IN ONE RUNTIME FUNCTION**, AND THE `memset` IS A GUARD-BLINDING FIX

Brief `/tmp/brief_J_v1.md`; measured prototype **`/tmp/scoutJ_a0389441ab0625440/recover_scoutJ_04_FINAL_v2b.patch`**
(use it, never retype). Findings that outlive the track:
- ⛔ **TWO INDEPENDENT DEFECTS.** (1) `gorget_array_fill` memcpys one source into N slots with no per-slot
  clone ⇒ N-way double-free. (2) **`v.fill(4096, v[0])` is a heap-UAF inside fill's OWN memcpy** — `val_src`
  points into `arr->data` and `ensure_capacity` reallocs it away; **`v.fill(3, v[0])` prints EMPTY STRINGS.**
  ⇒ **a per-slot-clone-only fix closes ONE of the two.** The fix must snapshot BEFORE the drop loop AND the
  realloc.
- ⭐ **`fill` is deliberately NOT a consuming position** (`grep -n "consuming_positions_by_name"
  src/ir/lowering/exprs/methods.rs`) ⇒ **ALL N slots clone, not N−1** — the source is never consumed.
- ⭐⭐ **THE `memset` IS LOAD-BEARING AND WAS FOUND ONLY BY MEASURING.** Under the snapshot-without-memset
  intermediate a **pre-existing `Vector[Box[Speaker]]` leak DISAPPEARED, 3/3 runs** — the static thread-local
  scratch is an **LSan ROOT**, so a stale element copy keeps a genuinely-leaked payload reachable and
  **silently suppresses the report.** `sanitize_sweep.sh`'s `LSAN_OPTIONS='use_stacks=0'` disables **stack**
  roots, not static ones. **A guard-blinding regression that would have shipped invisible.** Pre-existing
  sites (`gorget_array_swap`, `gorget_array_remove_opt`) share the property with no memset — filed `t1419`.
- ⭐ **NO OWNER ASK.** Ratified **D1** settles it (*copy timing/placement is unobservable implementation
  freedom*; *allocator introspection observes the implementation, not the language*), and **ggdef — the
  oracle — ALREADY implements it** (`repeat_n`) ⇒ the fix makes C/LLVM/self-host **converge on ggdef**.
- ⭐ **THE ENUMERATION IS TOTAL, WITH A REAL WITNESS**: the runtime-symbol registry (`src/lir/runtime.rs`),
  predicate run mechanically over all single-by-value-element rows. `gorget_array_fill` is the only member.
  ⊕ **`gorget_heap_push` HAS NO C DEFINITION ANYWHERE** — a dangling registry row; `Heap.push` would fail to
  link. ⊕ **`gorget_shared_array_set` double-frees too — a NEW CRITICAL, filed `t1418`, independent.**
- ⚠ **THE SURFACE CLASS HAS TWO MEMBERS, NOT ONE**: `Vector.fill` **and `Deque.fill`** (`DEQUE.methods =
  VECTOR.methods`), which is why a `grep 'name: "fill"'` witness undercounts.
- ⛔ **`Deque.get(i)` IS A BLIND OBSERVATION CHANNEL** (`t1087`) — the scout's first probe read through it and
  printed pointers; through `d[0]` it prints the string. **Any Deque fixture must use indexing or iteration.**
- ⛔ **THE FIX UNMASKS A THIRD FACE OF `t0872`** (inline-ctor temp at a NON-consuming value position leaks;
  a named local at the same position is clean). Pre-existing — proven by two controls that leak at HEAD and
  at the fix alike. Must be fixed or landed `#[ignore]`+`known_gaps` citing `t0872`, never shipped silent.
- **Cost, regenerated at both levels:** only the view/literal element at 2M scale moves (`-O0` ≈6.1×, `-O2`
  ≈4.7×, peak RSS +12 KB); POD control unmeasurable. ⭐ **Every `.fill` in the self-host is `Vector[bool]`**
  ⇒ the hot path pays nothing. **"Keep views as views" is UNSOUND — a `cap == 0` `Str` is not always static.**
- **Allowlist delta ZERO (provisional)** ⇒ **no collision with H**, which retires 8 rows the same round.

⭐ **TRACK A's SCOUT LANDED — brief at `/tmp/brief_A_v1.md`, measured prototype at
`/tmp/scout_a_protoA.patch` (USE IT, do not retype).** Findings that outlive the track:
- **ONE LINE.** The `Expr::Deref` value-lowering arm discriminates a `Box[T]` PARAMETER (needs two peels)
  from a plain local (needs one) **by testing the RESULT of the first peel instead of the SOURCE
  representation** — so when `T` is itself a box, it peels twice. ⭐ **The correct discriminator
  (`ptr_to_box`) was ALREADY COMPUTED THREE LINES ABOVE and used only for drop-registration.** Core #1
  verbatim. Blast radius: **1 divergence in 2594 programs.**
- ⭐⭐ **ONE DEFECT, TWO FACES — AND THE SECOND FACE IS A READ-SIDE COMPENSATION.** Both lanes get the SAME
  wrong GIR. The C emitter rewrites `%lld → %.*s` for `Str` args, which repairs the **TYPE** but keeps the
  extra **DEREF**, casting `char*` to `Str*` → SIGSEGV; LLVM prints the `i64` faithfully. **Fixing the writer
  closes both. There is no second write site.**
- ⚠ **`t1077` UNDER-SCOPES ITSELF:** "SIGSEGV on C / silent-wrong on LLVM" is **true only for `String`** —
  `int`/`float`/`bool` SIGSEGV on **BOTH**, a user struct is silent-wrong `0` on **BOTH**. The two-faces
  property is an artifact of the **payload type**, not the axis.
- ⛔ **FIXTURE PLACEMENT IS FORCED TO `known_gaps/` WITH LIVE TESTS.** A top-level fixture would enter the
  parity corpus (**the self-host cannot compile the shape at all**) AND the ASan corpus (leak vs a
  shrink-only allowlist). `known_gaps/` is `OUT` of both, the census scans only `#[ignore]`d tests, and the
  in-tree precedent exists. **No ceiling moves.**
- ⚠ **THE FIX TRADES CORRUPTION FOR A PRE-EXISTING LEAK** (`ASAN_SEGV` → `LEAK`), introduces no new leak, and
  the leak is **blocked on `t0096`** by the source's own comment. **Up the severity ladder, NOT
  reference-grade — the round says so rather than claiming a clean close.**
- 🚨 **A NEW CRITICAL-CLASS SELF-HOST FINDING: THE CONSTRUCTOR SPELLING CHANGES MEMORY SAFETY.**
  `Box[String](mk(a,b))` → **rc 134 double free** on the self-host; `Box.new(mk(a,b))` → rc 0 correct;
  **Rust gg correct on both.** Byte-identical but for the spelling, single level, not nesting. Filed as
  `t1310` by the track.
⭐ **PASS 1 CONFIRMED THE DESIGN AND BLOCKED THE BRIEF (folded into `/tmp/brief_A_v1.md` ADDENDUM 1,
which takes PRECEDENCE over the body).** It re-verified every body premise as TRUE and validated the one-line
fix on **17 measured cells**. What it falsified was the brief's *enumeration* and its *`s06` rule*:
- ⛔ **`s06`'s WRITE SITE IN THE BODY WAS WRONG — twice.** The real producer is **Box-TypeDef registration
  site 3**, whose non-mutating `map_ast_type` is `try_map_ast_type(ty).unwrap_or(UNIT_TYPE)`, so
  `Box__Box__int64_t` registers `_0: Unit`. Regenerate:
  `grep -n 'let inner_type = mapper.map_ast_type(&_type_args\[0\].node);' src/ir/lowering/types.rs` and
  `grep -n 'pub fn map_ast_type\b' -A 3 src/ir/lowering/types.rs`. **One-token fix verified green, and
  SEPARABLE from `t1077`.** ⇒ **FOLDED IN under Core #4** (the 4 sites are lint-pinned) — **`t1312` RELEASED**.
- ⛔ **THE FACE LIST WAS A SELECTION.** The class includes an **ICE (rc 101)** and a **link failure**, not just
  SIGSEGV/silent-wrong — so **`t1077`'s severity text AND its `DONE.md` line UNDER-STATE it.** The real
  discriminator is **`payload type × consume shape`**, not payload alone.
- ⛔ **THE `^param` CELL THE BRIEF ASKED FOR LANDS RED AND IS NOT THIS TRACK'S INFLOW** — `int take(Box[int] ^b): return 1`
  is rc 134 on the **pristine** compiler, wider than filed `t0010`. **Excluded; filed as `t1313`.**
- ⭐ **THE SWEEP PROVIDES ESSENTIALLY ZERO EVIDENCE HERE** — the change is a no-op on ~2593 of 2594 programs.
  **The new fixtures are the ENTIRE guard.**
- ⚠ **A FALSE-CONFIRMATION TRAP:** the `--lir-c` driver spelling emits a **fragment** that fails `cc` for an
  unrelated reason — reaching for it "confirms" *SH cannot compile it* for the wrong reason.
⊕ **IDS ISSUED FROM A's BLOCK:** `t1309` nested-box scope-exit LEAK (**`t0096` asked for this filing and
nobody made it**) · `t1310` the SH constructor-spelling double free · `t1311` the SH `__gg_Box__<inner>`
undefined typedef · `t1312` `s06` (nested box through a struct FIELD) **only if it is a different producer**;
if it is the same one, the track FIXES it (Core #4).
⚠ **A stale-instrument trap the scout hit: the cached `/tmp/*_sh_driver` binaries are STALE** — self-host
source moved after them. **Rebuild the driver before believing any SH result.**

### ⛔ THE LEDGER EDIT WAS REVERTED — MY REPORT TO THE OWNER CONTAINED TWO FALSIFIED MEASUREMENTS

The owner authorized a ledger correction (*"Fix the ledger according to language-design"*) **on the strength of
an orchestrator report that Track B's brief-review pass 1 then FALSIFIED at the same HEAD.** The edit is
**reverted**; `decisions.md:1605` stands as written. `todo/t1067`'s body **STANDS — act on it.**

| I reported | pass 1 measured |
|---|---|
| live-`Callable` `.clone()` then capture → rc 0, correct, both backends | ⛔ **heap-use-after-free, ASan rc 1, garbage on BOTH backends.** The clone gets its own env, but **the CAPTURE still copies the handle bitwise and the clone is still dropped at block exit** — cloning the *source* cannot fix a defect whose mechanism is the *capture*. |
| "capturing `h.f` is already rejected today" | ⛔ **MISATTRIBUTED** — that probe rejected at a **bare-assign** site, then captured a bare identifier. **The direct capture is ACCEPTED.** ⇒ `:1605`'s *"reject cannot be spelled"* is **CONFIRMED, not refuted.** |

⭐ **AND I INVERTED LEDGER PRECEDENCE.** `:1605` is **2026-09-04** and is its own ruling's **exception clause**;
D31 Addendum-2 at `:1516` is **2026-07-20** and **general**. **Later + specific does not lose to earlier +
general**, and `docs/language-design.md` ranks **BELOW** `decisions.md` — a §3.5 derivation by an agent cannot
retire a ratified owner exception naming this exact cell.

⚖ **WHAT REMAINS A GENUINE OWNER ASK (one claim, not three):** does *"move breaches it under D31"* survive?
The DEAD-cell argument is sound and pass 1 confirmed it independently — live ranges do not intersect, so §3.5
finds **no conflict**, and reader-vs-writer does not gate that row. **But that is a SEMANTICS question for the
owner, not something a measurement settles.**

### 🟢 A1's EXECUTOR IS LAUNCHED — AND THE LAUNCH FOLD CAUGHT A FOURTH-GENERATION DEFECT IN MY OWN ADDENDUM

The confirming pass re-verified readiness row (4) a **third** time independently (binary hashes as positive
controls; revert restored the pristine hash exactly, twice) and found **three launch-blocking brief defects,
all one-line text fixes, none touching the design.** Folded as ADDENDUM 4:

- ⛔ **N5 WAS ORPHANED AND LIVE, TARGETING THE LINE BEING EDITED.** ADDENDUM 3's D12 struck D9 — but **D9's
  other job was retracting N5**, so under strict precedence N5's *"make it an ICE"* at
  `src/ir/lowering/exprs/mod.rs:735` came back to life **inside the edited block**. Instrumented **by line
  index**: **PRE-fix 1 fire in 207 fixtures scanned, and the single fire IS THIS TRACK'S OWN FIXTURE**;
  POST-fix 0. An ICE there would make the RED-verify emit an ICE instead of `139` and the executor would
  conclude the revert did not take. ⊕ **D9's own basis was also wrong** — it claimed *"no fire count exists in
  either direction"*; **it is 1 PRE / 0 POST.** Conclusion unchanged, now better founded. ⭐ **Free finding:
  line 735 is the mechanism's SECOND HALF** (`__slit_2` goes `"%lld\n"` → `"%.*s\n"` in emitted C) ⇒ N1's
  causal story localized to a line **plus a measured Core #6 ratchet, 1 → 0.**
- ⛔ **ADDENDUM 3's E7 IS MEASURED FALSE — AND IT IS THE FOURTH GENERATION OF WHAT D13 EXISTS TO STOP, INSIDE
  THE ADDENDUM THAT CONTAINS D13.** E7 ordered the leak's mechanism into a durable filing. The type change is
  real; the consequence is not: `--clones=sites` PRE and POST are **identical** (1 implicit `String` clone, CoW
  materialization), emitted C has **no `Box__GorgetString__clone` in either state**, and ASan is
  **byte-identical to the no-op-cell control in both states**. Why it cannot fire:
  `grep -n 'pub fn is_resource_type' -A 4 src/ir/types.rs` matches `GirType::Named(name)`, which **does not
  admit `Box__GorgetString`**. ⇒ **`t1309` records the leak as construction-only, MECHANISM UNDETERMINED.**
  ⊕ It **strengthens** the Core #8 ship disposition, which had rested on a control that **structurally could
  not discriminate a read-path clone**.
  ⛔ **THE DEFECT WAS FOLDING A REVIEWER'S *"plausible mechanism… give it a disposition"* AS *"say so"* — AN
  ILLUSTRATION PROMOTED TO A PRESCRIPTION**, the exact class AGENTS.md already names. ⇒ **D13 SHARPENED: a fold
  states a MECHANISM only with the command that measured it, or states the mechanism is UNDETERMINED.**
- ⛔ **THE SPLIT DID NOT RE-PARTITION THE ID BLOCK (MA-3b)** — A1 and A2 briefs both offered `t1314`–`t1318`.
  **A1 owns `t1309`–`t1318`; A2 owns `t1373`–`t1382` and nothing in A1's range.**
⊕ **D10's open question is ANSWERED, not open:** `SlotKind::BorrowedPtr` says a slot holds a non-owning pointer
and **nothing about the pointee being a Box**, so it cannot decide the peel alone; `ptr_to_box` is built from
two typed accessors. ⇒ **`ptr_to_box` IS the minimal typed discriminator; `SlotKind` is orthogonal — no Core #1
conflict.**

### ⛔ TRACK A IS SPLIT — A1 (`t1077`) IS READY; s06 BECOMES A2 WITH ITS OWN SCOUT

Pass 3: *"The root cause, write site, prototype, fixture placement, D6, D7, D11 and readiness row (4) for the
`t1077` half are all sound and independently re-confirmed — **that half is ready.** The blocker is the
folded-in s06 half."*

⛔ **THREE ORCHESTRATOR DECISIONS ARE STRUCK — each measured false by pass 3. A2's scout STARTS from these:**
- **D9's guard is FALSE ON THE COMMITTED CORPUS IN BOTH STATES.** 26 fires / 72 fixtures, **12 return
  `UNIT_TYPE`, every one a `Box[Trait]`** — `dynamic_dispatch.gg`, `print_trait_object.gg`, `serializable.gg`,
  `box_trait_drop.gg`, +6. **It would ICE ten SHIPPING fixtures**, and D8 applied verbatim does not change
  them. ⭐ **That is the exact release-crash risk D9 raised against N5 — reproduced in the sentence that
  replaced it.**
- **D8's site 4 has NO FIRE COUNT and its fall-through is DOCUMENTED AS INTENDED** for the trait case
  (`sed -n '2412,2416p' src/ir/lowering/generics/mod.rs`). **I caught ADDENDUM 1 aiming at site 2 — a
  rationale, not a hazard — then made the MIRROR ERROR one site over.**
- **The class is SEVEN sites, not six:** `grep -n '\.map_ast_type(' src/ir/lowering/types.rs | grep -v map_ast_type_mut`
  → **`:849` `register_newtype`**, never counted. `newtype NB(Box[Box[int]])` reproduces, **survives site 3's
  fix**, and fixing `:849` **exposes a further defect** (C rc 134 double free, LLVM rc 139).

⛔ **D13 — FOLD DISCIPLINE, NOW BINDING.** *"Three generations of this brief have shipped an enumeration defect
in the fold written to correct the previous one"* (body → N5 → D9). ⇒ **EVERY COUNT IN A FOLD IS REGENERATED BY
A COMMAND BEFORE IT IS WRITTEN.** A fold that REASONS its way to a number instead of MEASURING it is how all
three generations failed. **If you cannot regenerate it, do not state it.** ⊕ **The "17 cells" figure is
RETIRED outright** — name the cells you ship, state no total.
⊕ **Fourth-axis probe NEGATIVE:** constructor spelling is orthogonal on Rust gg; the three-axis matrix
(payload × consume shape × storage class) is total as far as it can be pushed. Spelling stays SH-scoped via `t1310`.

### ⛔ TRACK B IS SPLIT — SHIP THE DEAD CELL ONLY (orchestrator decision, pass-1 recommended)

Pass 1: **R4 — three items, one hat, two of them UNBUILT** (`t0948` regresses `p12` to a link failure;
`t1210` never prototyped). Gauntlet rule: **rebuilt or SPLIT, never reviewed harder.** ⊕ D2's
"cannot be split" is refuted by its own evidence — `t1067` alone *"converts UAFs into leaks"*, and on the
ratified ladder (mem-unsafety > silent-wrong > ICE > leak) **a half that is strictly better IS a shippable
increment.**

**B1 = the DEAD cell ONLY.** No conflict under §3.5 · reader/writer irrelevant · **no dependency on `t1210`** ·
**NO accept/reject surface change**, so Core #9's ggdef+NEG obligation never fires and the zero-slack SH
ceiling is not stressed · guard already wired (`grep -n 'closure_capture_callable_block_scope_uaf' tests/integration.rs`).
**File BOTH live sub-cells as NAMED remaining cells** (Core #12).

⛔ **EVIDENCE THAT MUST NOT SURVIVE INTO ANY BRIEF:**
- **The live-`Callable` `.clone()` fix-it is a UAF on BOTH backends** (`bash /tmp/revB1_a7fa6e85b7a7f5e03/full_a7f.sh p15_clone_fixit.gg`).
  ⭐ **SEPARATE TWO CLAIMS:** the **FIX-IT** claim ("the user writes `.clone()` and it is correct") is
  **FALSIFIED**; the **MATERIALIZE** claim ("the compiler clones AT THE CAPTURE BOUNDARY and the env OWNS it")
  is untouched and **never prototyped**. ⛔ **And materialize cannot be correct until `t1210` lands** — the env
  owning a deep copy needs `__Closure_N__drop`, which `t1210` records as **emitted with zero call sites**. It
  converts the UAF into a **LEAK**, not into correctness.
- **"Capturing `h.f` is already rejected today" is MISATTRIBUTED** — that probe rejects at a **bare-assign**
  site. **The direct capture `(): h.f()` PASSES `gg check`** and link-fails with `undefined reference to
  'Holder__f'`. ⊕ **Same shape as the `p12` failure the brief blamed on the `t0948` prototype ⇒ that failure is
  PRE-EXISTING, not a prototype artifact.**
- **The fire count is a SELECTION** — 171 of 426 files (**40%**) unbaselined, booked as "evidence already in
  hand". Baseline them or say "255 baselined / 171 unknown" and drop the word TOTAL.
- ⭐ **`clone_fn` PRESENCE IS NOT A SUFFICIENT DISCRIMINATOR** — it says a source can be duplicated and says
  **nothing** about whether the capture boundary transfers ownership of the duplicate.
- ⭐ **THE `is_move` PATH BYPASSES THE READER ASSUMPTION ENTIRELY:**
  `grep -n 'let mutated = if is_move' src/ir/lowering/closures.rs` — for a move-closure the mutation set is
  unconditionally empty ⇒ **every capture is `ByValue`, WRITERS INCLUDED.** Probe it or name it omitted.

⚠ **I OVER-STATED THE LANE OBLIGATION, IN THE HARMFUL DIRECTION.** A `Mutex`/`RWLock` reject does **NOT** owe
"a NEG fixture on every lane": **ggdef CANNOT adjudicate `Mutex`** (outside phase-0 subset — `t1067` says so
verbatim), so it owes **a filed SUBSET GAP + a note, NOT a mirrored reject** (Core #9's own escape clause);
self-host owes **a filed lane-lag citation**, not a same-round mirror. Only C+LLVM owe the reject + NEG
fixtures + census. **Written as "every lane" the cell looks un-shippable, and both escapes are bad — stall, or
stress the zero-slack ceiling to discharge an obligation Core #9 never imposed.**
⊕ **D31's DX RIDER CANNOT BE SATISFIED for a live `Mutex` reject** — `clone_fn = None` by design and D53's
`Shared[Mutex[T]]` is a **restructuring, not a fix-it**. The diagnostic must **TEACH THE SHAPE**, or the
executor ships a bare `E_`.

### ✅ TRACK D SCOUTED — AND THE DEFERRAL'S COST PREMISE IS REFUTED

**`Router__dispatch_inner` ALREADY emits 3 matched `gorget_closure_clone_to_owned`/`_free`** — the per-request
clone the deferral existed to avoid **is already shipped**, and it is what makes the 8 httpserver sites safe,
**not "distinct keys"**. 5000 requests, HEAD vs prototype: `total_allocs` unchanged, `live_bytes` unchanged,
wall **5.87 → 5.42 ms**, RSS +4 kB. ⇒ **THE MEASURED HOT-PATH COST OF WIDENING THE REJECT IS ZERO.**
**Write site:** `grep -n "Expr::FieldAccess { .. } | Expr::TupleFieldAccess { .. }" src/semantic/safety/check_expr.rs`
— adding `| Expr::Index { .. }` is the whole Rust-lane change. Prototypes:
`/tmp/recover_scoutD_a09352_gate.patch`, `/tmp/recover_scoutD_a09352_full.patch`.
⛔ **BLOCKING PREREQUISITE — `t0873(b)` LANDS THE SAME ROUND OR THIS TRACK REDS `sanitize_sweep.sh`.**
`tests/fixtures/dict_box_callable.gg` is ASan-clean at HEAD **only** because each element is bound exactly
once; **every remedy the reject offers turns clean into leaking** (`Vector`/`Dict` of `Callable` never
synthesises `elem_drop`). It is top-level, in the sanitize corpus, **not** allowlisted, and a new row is
forbidden inflow.
🆕 **NEW HIGH — THE RETURN POSITION HAS NO SUBJECT AT ALL (SIX-Q#4):** `return v[0]` does not reject — and it
is **not index-specific**: `return h.f` is **also** `gg check` rc 0 + ASan double-free, **a hole in the field
arm R49 ALREADY SHIPPED**. `check_stmt.rs` calls `tainted_place_name` but never
`require_explicit_move_for_single_owner_init`. ⇒ **SPLIT: D1 = index widening + `t0873(b)`; D2 (`t1339`) = the
return-position hole; D3 (`t1340`) = the `d[k](v)` parser fix (optimality, not a blocker — cost measures zero).**
⚠ **The item's recorded discriminator is a THIRD incomplete reading:** full discriminator = (element read >1)
**AND** (read materializes as DerefLoad, not Clone) — the second conjunct is a **whole-program property**
(`t0949`). **Safety here is NOT locally auditable.**

⚖ **SECOND MISSING-RULING ASK (Track D, independent):** *"the callee position is a borrow position"* is cited
by `t1225`, `devbook/11` and three lane comments — **and is NOT in the ledger.** Exhaustive search of 3365
lines: no entry. **All occurrences were written by R49 Track S-a2, citing each other.**

⚠ **OPS: `SendMessage` TO A **COMPLETED** AGENT RESUMES IT WITH NO WORKTREE.** Pass 1 had finished and its tree
had self-disposed; my message revived it into a treeless shell that could not re-verify anything. **Do not
message a completed agent expecting it to re-measure — respawn instead.**

⛔ **LESSON, AND IT IS THE ROUND'S THIRD OF THIS SHAPE:** I put a scout's unverified measurements in front of
the owner as grounds for editing the RATIFIED LEDGER. **A scout measurement has not been through the gauntlet.
Nothing reaches the ledger — or the owner as a basis for ratification — before a fresh pass has tried to
falsify it.**
⊕ **A SECOND, INDEPENDENT INSTANCE THE SAME DAY (Track D scout):** *"the callee position is a borrow position"*
is cited by `t1225`, `devbook/11` and three lane comments — **and is NOT in the ledger at all.** Exhaustive
search of 3365 lines: no entry. **All occurrences were written by R49 Track S-a2, citing each other.** ⚖ Also
an owner ask.

`decisions.md:1605` makes **three** claims about a closure capturing a `Callable`/`Box`/`Owned`/`Task`/`Guard`:
*"clone breaches the carve-out, **move breaches it under D31**, and **reject cannot be spelled until D7's
capture list exists**."* **Two are measurably false:**
- **MOVE.** D31 Addendum-2 — the ratified text itself, `sed -n '1516p' docs/define-gorget/decisions.md` —
  says *"full strict governs CONTRACTUAL consumption only — bare values at non-`!` consuming positions
  (push/ctor/return/**capture**) still **auto-move-when-dead** per CoW (unobservable optimization, not
  contract; no sigil marks it)."* The 2026-09-04 capture ruling agrees. **A move creates no second owner, so it
  cannot breach a SINGLE-OWNER carve-out.**
- **REJECT.** Capturing `h.f` is **already rejected today** — `error[E_MoveWithoutOperator]: … 'h.f' is a
  single-owner type` — and the fix-it it recommends **works**: `c6 = c5.clone()` then capture, `c5` still live
  → rc 0, `41\n41`, both backends, matches ggdef, no UAF.

⭐ **AND §3.5 RESOLVES IT FROM THE DESIGN, so the track does not depend on which ledger line wins** (owner asked
2026-09-05). **`:1605` ASKED A TYPE-FAMILY QUESTION** — *is this type in the single-owner carve-out?* — which
§3.5 forbids in its own first two bullets: ***"Overlap is about storage, not spelling"*** and ***"The test is
ability to write, not the sigil."*** Apply the real predicate (storage overlaps × live ranges intersect × at
least one can write) and there is **no blocked cell**:

| cell | §3.5 verdict | why | measured |
|---|---|---|---|
| source **DEAD** | **NO CONFLICT EXISTS** | live ranges do not intersect | move → rc 0 ✔ |
| **LIVE** `Callable` | **MATERIALIZE** | reader + `clone_fn` exists ⇒ the clone can be lazy | `.clone()` → rc 0, both backends ✔ |
| **LIVE** `Mutex`/`RWLock` | **REJECT** | `clone_fn = None` **by design** ⇒ no lazy escape. §3.5: *"reject when the only rescue is a guess"* — a Mutex has **no clone at all** | D53 already names it: `Shared[Mutex[T]]` |

⚠ **`clone_fn` PRESENCE IS THE DISCRIMINATOR AND IT SPLITS THE FAMILY** — do NOT write one rule for
"single-owner captured live". Read it via the accessor, never by type name (Core #2).
⛔ **A Mutex/RWLock REJECT is an ACCEPT/REJECT SURFACE CHANGE** ⇒ owes ggdef **+ a NEG fixture on EVERY lane**,
and the SH lane is UNMEASURED with the ceiling at ZERO SLACK. If it cannot land cleanly, ship the dead-cell
move + `Callable` live materialization and file the reject as a **NAMED** remaining cell (Core #12).

⚠⚠ **THIS IS THE THIRD TIME THIS ROUND THE IMPLEMENTATION RE-DERIVED §3.5 WRONG BY REASONING ABOUT SPELLING OR
TYPE FAMILY INSTEAD OF STORAGE AND LIVENESS** — R49's S-a2 gate read the source's SPELLING; `t0045` warns in
its own text that the `&` IS NOT THE DISCRIMINATOR; now `:1605`. **A fix that introduces a NEW type-family test
is a design defect regardless of whether it passes.**

⇒ **`t1067`'s "all three answers are blocked" premise is FALSE, and D7 gates neither the dead cell nor
`Callable`'s live cell.** What is left for D7 is **ergonomics, not soundness**. ⚠ **The track does NOT wait on
this** — the dead cell is unblocked by the ratified text as it stands; only the ledger's own consistency needs
the owner.

⛔ **STANDING CONSTRAINT, UNCHANGED: the non-MATCH ceiling and the ggdef floor are at ZERO SLACK.** Any track
adding a non-MATCH fixture reds immediately. **Own new fixtures must COMPILE + MATCH on self-host the SAME
ROUND** (Core #9) — raising the ceiling for your own inflow is forbidden.

⊕ **A DESIGN SCOUT RUNS ALONGSIDE, AND IT IS NOT A TRACK:** the CoW-cost / stored-borrow / `D41` conflict
(`t1307`, `t0538`). It ships no diff and files nothing — **it produces input to an OWNER RATIFICATION**, which
is the long pole because its latency is not ours. ⚖ **Owner granted a ONE-OFF Fable spawn for it
(2026-09-05); the default remains the harness model.**

⭐⭐ **OWNER DIRECTION 2026-09-05 — THE SCOUT OWES A *UNIFIED MODEL*, not three answers side by side:**
*"propose a unified model that encompasses the stored borrows alternative solution, transient-views and
cow-cost. All three aim at performant, simple and safe gorget user code. And lazy in the case of the iterator,
which makes all the difference."*
⛔ **THE 2026-07-28 LEGALITY/COST SCOPE SPLIT IS NOW A HYPOTHESIS, NOT A CONSTRAINT.** The owner is asking
whether that seam is in the right place; the scout may reject it if a unified model is better.
⭐ **THE LOAD-BEARING CLAIM TO TEST: LAZINESS IS THE FORCING FUNCTION, NOT AN INCIDENTAL.** An EAGER iterator
materialises and has **no borrow to store** — no legality problem, and the cost is the copy. The LAZY one
walks the bucket array in place, which is **exactly why it is fast (cost)** and **exactly why it must hold
something across `next()` (legality)**. ⇒ **Laziness cuts ACROSS both notes, which would mean the current seam
runs through the middle of the phenomenon rather than around it.** **So the real question may be `eager vs
lazy`, with legality and cost falling out differently on each side** — and if so the unified model is SIMPLER
than either note alone, which is the finding.
⊕ **It generalises past iterators** — a builtin's returned view, a closure capturing a collection, a Vector
HOF's element pointer. `D40` (return-view lazy materialization) is already in that territory.
⊕ **The owner's three words are the ACCEPTANCE CRITERIA: PERFORMANT · SIMPLE · SAFE.** A model that is safe
and fast but makes users reason about compiler internals fails *simple* — the exact objection that killed
`Ref[T]`-by-default.
⭐⭐ **THE LAZY ITERATOR *IS* DOCUMENTED, AND ONE OF THE THREE DOCS MAY ALREADY BE THE UNIFIED MODEL**
(owner recalled it 2026-09-05; orchestrator located it):
- **`docs/language-design.md` §3.5 — "The Borrow Rule — one rule, with a lazy escape."** ⭐ **It is ALREADY a
  single rule spanning legality AND cost:** *"A conflict is rejected — **unless** the conflicting path is a
  reader and the compiler can place its clone **lazily**, at a visible mutation point, in which case it
  materializes instead."* ⇒ **The legality/cost seam lives INSIDE one sentence.** The two internals notes may
  be elaborations that DRIFTED APART from a rule that was already unified. **So the question is not "what
  unified model should exist" but "why did §3.5 need two notes, and what does each add that the rule does not
  already say?"**
  ⚠ **Its own follow-on is this round's recurring defect, stated as DESIGN years before the bugs:**
  *"The test is **ability to write**, not the sigil"* and *"Overlap is about storage, not spelling."* **R49's
  S-a2 found a gate that read the source's SPELLING not the position; `t0045` (CRITICAL) warns in its own text
  that THE `&` IS NOT THE DISCRIMINATOR.** The doc already says the rule; **the implementation keeps
  re-deriving it wrong.** ⇒ **Can the model make that MECHANICAL rather than remembered?**
- **`docs/language-design.md` line 54 — the OWNER'S END GOAL, already a stated pillar:** *"Value semantics at
  hand-optimal cost — CoW + liveness + lazy materialization make the compiler place the minimal clone set,
  **as if the user had written every copy by hand**."* ⇒ **The pivot is NOT a new direction — it is a return
  to a pillar that was written down and not yet delivered.** It is the acceptance criterion in the project's
  own words, and it PREDATES the ask.
- **`docs/devbook/23-stdlib.md` §23.6 "Iterator: the M+N payoff"** — *"Lazy adapters (concrete return, not
  trait object)"* and *"Lazy by default — no eager interim."* **Laziness here is a DELIBERATE design choice,
  not an implementation accident** — which is why the stored borrow exists at all.

⚠ **The measured baseline still comes FIRST.** *"Whatever we replace it with must be equally fast"* is a hard
gate on any model, so a model that cannot be measured against the current fast path is a proposal, not an
answer.

### ✅ THE COW DESIGN SCOUT RETURNED — AND IT INVERTED THE PREMISE IT WAS SENT TO PROTECT.
**Its verdict on the owner's unified-model ask: `D41` DOES NOT NEED TO BEND, `SelfRef[T]` IS UNNECESSARY, and
`docs/language-design.md` §3.5 ALREADY IS THE UNIFIED MODEL** — legality and cost are two outcomes of one
conflict predicate, and *"eager vs lazy is not a rival seam; it is the length of the live range."* The
apparent `D41` violation was **manufactured by a compiler defect**, so there was no design conflict to
resolve.

⛔ **`t1307`'s PREMISE IS MEASURED FALSE — the stored-borrow path was NEVER fast.** Regenerate (orchestrator
ran this at HEAD, 500-entry `Dict[int,int]`, both forms printing the same correct answer):
```bash
./target/debug/gg run <fixture with `for k, v in d.iter()`> --clones=stats 2>&1 | grep clone-stats
./target/debug/gg run <same fixture with builtin `for k, v in d`> --clones=stats 2>&1 | grep clone-stats
```
`.iter()` clones 4× per element and frees almost none; the builtin clones zero. The scout took it to
**OOM-kill at N=200 000**. **Clone counts are IDENTICAL at the commit that introduced the "lazy bucket walk"
and every commit since — the pointer walk NEVER EXISTED.** The `DONE.md` entry claiming *"no allocation
beyond the iterator struct"* was **a source-read, not a run**, quoted forward for months.
⚠ **It was already filed as `t0952`, and admitted on TEN `LEAK_ALLOWLIST.txt` rows — so the sanitizer has
been green BY ADMISSION**, which is exactly why nobody looked. `t0952` **re-graded MED → HIGH**; its write
site is **one layer above where the item pointed** (the generic collector degrades a declared `Ref[T]` param
to `Unit`/`ByValue` through a mapper that refuses `Ref`). Measured prototype: **zero clones, zero leak** at
`/tmp/recover_scout_cow_t0952_prototype.patch` — ⛔ **an executor USES THE PATCH, never a retyped snippet.**
⊕ **This makes the owner's constraint EASY, not hard.** *"Whatever we replace it with must be equally fast"*
names a bar that does not exist. ⚠ **`t1307` stays LOW/documentation per the owner, and the owner said DO NOT
FIX IT NOW** — the addendum only records what was measured.

**NEW ITEMS FROM THE SCOUT + THE ORCHESTRATOR'S OWN VERIFICATION — none is on a track yet:**
- **`t1359` CRITICAL** — `gorget_set_clone` copies `key_drop` but NOT `key_clone`/`key_materialize`, while its
  sibling `gorget_map_clone` copies all three; a cloned Set owns its keys and shallow-copies them on the next
  clone. **Double-free from safe syntax.** Core #4 drift; ⭐ **fits R50's headline exactly — it is the next
  track to launch when a slot frees.**
- **`t1360` HIGH** — the leak sweep builds "no longer leaking" as *in the allowlist and not seen leaking*, and
  `seen` is set ONLY by a `LEAK` verdict, so `SKIP_COPY`/`NO_BINARY`/`BUILD_FAIL`/`RUNNER_FAIL` all read as
  FIXED. **Not merely advisory: the same blind set feeds `retire_due`, a ⛔ BLOCKING gate** — a build break can
  force deletion of a row documenting a live defect. Fix is one intersection with `$OUT/covered`, already
  computed three lines earlier. ⚠ **`t0952`'s ten-row retirement DEPENDS ON THIS** — the sweep currently
  cannot tell "fixed" from "never ran".
- **`t1361` HIGH** — member access on a `for p in <expr>.iter():` binding is UNCHECKED: `gg check` says
  `OK: no semantic errors` to bogus field reads (run → prints `0`), bogus field WRITES (**silently
  discarded** — Core #10 verbatim), bogus methods (build rc 1) and `String x = p` (ICE rc 101). The builtin
  `for p in xs:` rejects every one. ⚠ **RE-GRADED CRITICAL → HIGH and the memory-safety claim WITHDRAWN** —
  `print(p + 1)` prints `8`, proving the binding IS typed where used legitimately; there is no wild read.
  **Do not let the withdrawn CRITICAL framing reach an executor** — it would send someone hunting a wrong
  ownership tag when the write site is a missing type check on member access. **Blocks `t0041`** (the
  owner-ratified campaign to make `for x in xs` the default idiom).
  ⚖ **OWNER RULED 2026-09-05:** *"Right, if p is an int, no fields should be accepted. gg check should
  reject that, no question about it."* ⇒ **SIX-QUESTIONS #1 is CLOSED for this item — it is a hole, not two
  ratified semantics, and the builtin's behaviour is the correct one.** The rejection belongs at **`gg check`**,
  not at lowering (today the ICE comes from the Tier 2a consume-site validator — wrong layer, wrong
  diagnostic). **No design question remains; this is an implementation track.**

### ⛔ I REPORTED "WIDENING THE REJECT COSTS ZERO" TO THE OWNER. THE INSTRUMENT COULD NOT SEE THE COST.

**Core #13 verbatim — pick an instrument that can SEE the failure class.** `total_allocs` and `live_bytes`
increment **only** inside `__gorget_global_alloc_fn`
(`grep -n '__gorget_total_allocated +=' src/backend/c/runtime/runtime_preamble.c`), and
`__gorget_closure_env_alloc` calls **raw `malloc`**
(`grep -n 'gorget_closure_env_alloc' -A 6 src/backend/c/runtime/runtime_string.c`). ⇒ **a closure-env
allocation CANNOT MOVE EITHER COUNTER.** The headline was internally inconsistent on its face — **+5000 clones,
+0 allocations** — and I carried it anyway.
⭐ **WHAT SURVIVES:** wall **5.87 → 5.42 ms** and peak RSS **9512 → 9516 kB** are instrument-independent. **The
two allocation figures are NON-EVIDENCE.** Re-argue from ASan allocation counts, valgrind, or `/usr/bin/time -v`.
⇒ **D13 EXTENDED: regenerating a number is not enough — CHECK THE INSTRUMENT CAN SEE THE CLASS.**

### ⛔ TRACK D IS SPLIT — D0 GATES D1

- ⛔ **THE BLAST RADIUS WAS A SELECTION: THREE fixtures move, not one, and TWO ARE ALLOWLIST-COUNT
  VIOLATIONS** (`new_class` ⇒ **fatal**): `dict_box_callable` (no row at all ⇒ `new_leak`),
  `httpserver_middleware` and `httpserver_router_extended` (env_alloc **×1 → ×2**). **And there are 12
  httpserver rows over 24 importing fixtures, not "five"** (`grep -c '^httpserver' tests/sanitize/LEAK_ALLOWLIST.txt`).
  **A raised count is forbidden inflow exactly as a new row is.**
- ⛔ **`t0873(b)` AS FILED DOES NOT COVER THE CASE.** Its mechanism, headline, cited site (`insts.rs`, the
  **array** ctor) and **both** repros are `Vector`. **The three failing fixtures are `Dict`.** ⇒ an executor
  ships an array-path fix, greens both its repros, **and leaves D red on all three.**
  ⭐ **The good news the brief must use: the fix site IS shared** — `elem_drop_fn_for_type` is called by both
  the array path and the map `val_drop` path. **One fix can cover both; nothing in `t0873` says so.**
- ⇒ **D0 = the collection-`Callable` `elem_drop` class fix across BOTH paths + a Dict repro + driving the two
  httpserver rows to zero** — which is a **SHRINK**, so `sanitize_allowlists_shrink_only` then requires those
  rows **retired the same round**. A third deliverable nobody had named. **D1 is gated on it.**
⊕ **Confirmed and kept:** D1's core reversal is TRUE (with a regenerating command, and ⚠ **the function body is
~1670 lines — a short grep window reads 0 and looks like a refutation**); ggdef is genuinely in-subset and
genuinely 1 line; the return-position hole is real and **not index-specific** (`return h.f` → rc 0 + ASan
double-free).
⊕ **Errata worth keeping:** the omitted axis cells are covered **BY CONSTRUCTION** (the change adds a *shape*;
the *type* test is the shared `needs_explicit_move`) — **stronger than a risk list**. ⚠ **The stale-`!` fix is
8 sites and a BLIND SED IS DANGEROUS** — `!` is still the correct D29 fallible mark elsewhere. ⚠ **The full
patch's `dict_box_callable.gg` hunk ships the remedy the scout measured as LEAKING — unshippable until D0
lands.** 🆕 **Pre-existing ICE in the exact shape space the executor will write fixtures in:**
`v.push(^afn)` on `Vector[Callable[int(int)]]` → rc 101 Tier 2a violation.

### ✅ TRACK C IS SPLIT — TWO CLASSES, PROVEN TWO-DIRECTIONALLY (MA-5 DONE RIGHT)

**The scout did exactly what MA-5 demands — applied each half ALONE to pristine HEAD and ran the OTHER half's
cells:**

| tree | 4 `t0011` cells | 7 `t0045` cells |
|---|---|---|
| HEAD | 134 double free | 134 double free |
| **+ C1 alone** | **0** | **134 — UNMOVED** |
| **+ C2 alone** | **134 — UNMOVED** | **0, ASan-clean** |
| both | 0 | 0 |

⭐ **What discriminates them:** `t0011` is a producer that **DESTROYED** provenance that existed; `t0045` is a
producer that **NEVER MINTED** it. *"Same theme — but a theme is not a class, and neither fix moves the other's
cells."*
⭐ **C2's three halves are each other's positive controls** — view-only is safe but **LEAKS 34 B**; drop-reg
without the view is **still 134**. **The third half was found by the Tier 2a validator refusing the second.**
⊕ `self_host_bootstrap_fixed_point` **PASS (910 s)**; perf 4M iterations **no regression**.

⛔ **THREE DECAYED PREMISES:** `t0011`'s *"same edit as `t0697`"* — **`t0697` CLOSED 2026-09-04**, the coupling
is discharged. `t0045`'s *"ggdef prints the ratified answer while Rust gg SIGABRTs — a live Core #8 event"* —
**FALSE: ggdef has NO for-loop at all** (13 `Stmt::` arms, no `For`); it **abstains**. And **the ratified
2026-08-18 doc write-through NEVER LANDED** though the ruling calls it *"MANDATORY … part of the fix"*.
⛔ **A GUARD THAT CANNOT SEE ITS OWN CLASS (SIX-Q #2):** `str_view_producer_enumeration_is_closed` calls itself
*"THE ENUMERATION … every view producer"* but **enumerates SYMBOLS, not SITES** — C2's brand-new emit site is
invisible and `--test lints` stayed **231/0**. **Core #6 widening owed.**
🆕 **`Box.new(1, 2)` BUILDS at HEAD, silently discarding argument 2** — a live **Core #10** violation found
incidentally. Reference-grade is a **check-time arity diagnostic**, not the `cc` failure C1 would otherwise ship.

### ✅ C2's OUTPUT-REVIEW: THE PATCH SURVIVED EVERYTHING — AND THE ONE BLOCKER IS A **DOC CLAIM THE FIX MADE FALSE**

⭐ **All three integration gates PASS, and gate 2 was RED-VERIFIED BY THE REVIEWER, not accepted on report:** it
built the pre-fix compiler at `7785c1221^` and ran all 11 new fixtures — **8 `security/` rows + the `spectests/`
seed RED** (`attempting double-free` / `heap-use-after-free`), green under the fix. ⊕ **The two green-at-HEAD
rows are not green-on-arrival coverage** — their own headers say their instrument is a **compile-time refusal,
not a crash**, and the reviewer confirmed it fires. ⊕ **Gate 3 is the strong form of Core #8:** ggdef MATCHes the
bare cell ⇒ **this is the REFERENCE being brought into line with the DEFINITION**, not two backends agreeing.
⊕ **Three revert atoms independently RED-verified with LINE-anchored breaks** — and Core #13 paid off twice:
`for_loops.gg` has **three identically-spelled** `set_collection_ref` lines and **two** identical
`register_local` lines; a substring break hits the wrong one. ⊕ **`Dp` fires ONLY under `detect_leaks=1`** ⇒
**the `security_safe_no_leak` wiring is LOAD-BEARING; under `security_safe` that row would be inert.**

⛔ **THE BLOCKER: `docs/language-reference.md:2554` still says the construct *"double-frees at runtime"*.**
Regenerate: `grep -n "double-frees at runtime" docs/language-reference.md`. The reviewer ran that exact program
under the fix: **rc 0, prints `x`, ASan- and LSan-clean.** ⇒ **the tree CONTRADICTS ITSELF in the user-facing
reference**, because the same commit rewrites two `#[ignore]` reasons to say *"it no longer double-frees"*.
⊕ **And the element-type list is now a stale SELECTION — `String` belongs IN the "rebinding is silently lost"
set**, which is exactly `t1404`'s residual.
⭐ **ROOT CAUSE, AND IT IS MINE: my doc-rot row was a SELECTION (SIX-Q #3).** It enumerated six citation SITES
and **never grepped for behaviour CLAIMS about the construct.** ⇒ the executor is told to **run that grep across
`docs/` and present the SET with a disposition per row** — two members are already known (`:2921` understates
now that `String` is affected; `docs/book/11-ownership.md:511,517` still teaches the retired *"borrows each
element read-only"* label).

⊕ **A GUARD GAP THE FIX ITSELF CREATES, measured:** `no_growth_in_lir_view_callee_rewrites` is **41 at parent,
41 at the fix, BUDGET 41** — no slack, nothing to lower. **But the new emit reaches `gorget_string_borrow_view`
through typed metadata and never SPELLS it in `src/lir/`**, so a future `borrow_view_fn: Some(..)` row mints a
new LIR view producer with **ZERO ratchet movement.** ⇒ **the devbook row was added by hand; the GUARD for that
path does not exist.** Filed from the spare block.

⛔ **`todo/t0041.md:11`'s blocker line goes STALE AT INTEGRATION, NOT NOW** — it cites the `for x in &coll`
double-free, which is fixed in `7785c1221` and **not yet on this branch**. ⚠ **The blocker CHANGES (to `t1404`'s
lost write), it does not VANISH.** ⇒ **correct it AT integration, or a true statement is deleted early** — the
retraction rule, one heartbeat old, applied prospectively for once.

### ✅ RULED — **D27's `^` MIGRATION EXTENDS EVERYWHERE** (owner 2026-09-05)

> *"D27's ^ migration extends everywhere. That is the new move operator that will eventually free the
> `!` operator."*

⛔ **SUPERSEDES the 2026-08-06 scoping** (`src`/`tests`/`lib`/`spec` only). **`docs/`, `README.md`, `spectests/`
are now IN SCOPE.** ⭐ **The END STATE is the point: `^` IS the move operator and prefix-`!` is to be FREED** —
so this is a prerequisite for retiring the old spelling from the grammar, not a cosmetic pass.

⛔⛔ **`!` DOES NOT DISAPPEAR, AND A BLIND SED BREAKS THE LANGUAGE.** D26 ratified **suffix-`!` as the FALLIBLE
marker** — `+! -! *! /! %!`, `<<! >>!`, `**!`, `f()!` (`grep -n "FALLIBLE OPERATORS ADOPTED"
docs/define-gorget/decisions.md`). ⇒ **POSITION-SENSITIVE: the NAME-SLOT prefix `!` becomes `^`; the SUFFIX `!`
STAYS.**

⚠ **NOTHING FLAGS THE OLD SPELLING TODAY** — the parser accepts both
(`grep -rn "Token::Bang) || self.check(&Token::Caret)" src/parser/` → 3 sites), so a `!`-move compiles clean and
**the tree cannot drift toward `^` on its own.**

⚠⚠ **MY FIRST MEASUREMENT WAS UNUSABLE AND I SAY SO RATHER THAN QUOTE IT.** A regex cannot separate a call-site
move `f(!x)` from a negation `if (!ok)` — **the instrument could not SEE the class** (Core #13). The one
unambiguous position is the declaration `Type !name` (negation never follows a type name):
**236 decl-position `!`-moves across 158 `.gg` files, against 49 already on `^`** — regenerate:
`grep -rhoE '[A-Za-z_][A-Za-z0-9_\]\[]* +![a-z_][a-z0-9_]* *[,)]' --include='*.gg' . | wc -l`.
⛔ **THAT IS A LOWER BOUND AND THE ITEM SAYS SO.**

⇒ **FILED AS `todo/t1405`** (MED, campaign): **instrument that counts by PARSE POSITION → migrate → ratchet
(Core #6, both directions) → only THEN reject prefix-`!`**, which is an accept→reject surface change ⇒ **Core #9
binds all three lanes.** ⊕ **`gg fmt` is an UNCONFIRMED lever** — `grep -rn 'Bang\|Caret' src/formatter/`
returns nothing.

⚠ **THE LEDGER STILL CARRIES THE OLD SCOPE. `docs/define-gorget/decisions.md` IS OWNER-EDIT-ONLY — I have not
touched it, and the ruling above is the authority until the owner records it.**

### ⛔⛔ C2's DOC FIX REPRODUCED ITS OWN ROOT CAUSE **TWICE** — A SELECTION, AND A SELF-CONTRADICTION

⭐⭐ **THE LESSON OF THE ROUND, IN ONE SENTENCE: THE FIX FOR A SELECTION WAS ITSELF SCOPED, AND SO WAS A
SELECTION.** The commit's own message says the replacement grep ran *"across `docs/`"* — **and the single
surviving instance of the retired label in the entire repository is `README.md:120`**, the project's front door:
`grep -rn "borrows each element read-only" . --include='*.md'` → **exactly one hit.** ⇒ ⛔ **SIX-Q #3
REPRODUCED ONE DIRECTORY OUT.** *Scoping an enumeration to a directory is how you re-make the mistake you are
fixing.*

⛔ **AND THE SECOND BLOCKER IS THE FIRST BLOCKER'S SHAPE, INSIDE ONE FILE.** The new §9.6 sentence says *"only
a mutation **through** the element lands"* — **measured FALSE for `String`** — and its own cross-reference
target, **written by the same commit**, says *"a `String` element additionally loses a mutation-through"*.
⇒ **the tree contradicts itself in the SAME FILE, one section apart — exactly the defect that spawned this
commit.** ⚠ **The executor had MEASURED this correctly (that is why §9.1 is right); it was lost the moment a
sentence GENERALISED OVER ELEMENT TYPES.**

⊕ **`t1335` cites a file with ZERO hits** (`grep -c "borrow_view_fn: Some" src/ir/types.rs` → **0**; the only
one is `src/ir/lowering/types.rs:139`) **and its "regenerating" grep returns ~50 lines, not the 3 consumers it
claims** — Core #15(a) inside a fresh filing. ⊕ **4 further doc sites are in NEITHER disposition list** — all
TRUE, so **no rot escaped**, but *"dispositions the whole set"* is not sustained. ⊕ **One NO-CHANGE row is the
right outcome for the WRONG reason:** the bullet it waves through was **FALSE for the String cell pre-fix** and
**the fix made it true.**

⭐ **WHAT THE CONFIRMING PASS ALSO DID — INDEPENDENTLY CONFIRM A CLAIM I HAD PRAISED.** The executor's decision
to leave the `e.push('!')` claim standing was re-measured at BOTH compilers: **correct.** ⇒ **praise verified,
not inherited.**

⚠ **OBSERVED, NOT ACTED ON — `README.md` teaches `!` for move while D27 ratified `^`.** Both are still accepted
(`grep -n "Token::Bang) || self.check(&Token::Caret" src/parser/mod.rs`), so this is a stale SPELLING, not
broken code. ⊕ **But the tree is overwhelmingly UNMIGRATED: `lib/` carries ~120 `!`-move spellings against 2
`^`.** ⇒ **a RATIFIED rule with a phased in-repo scope, and the scope is an OWNER question — not a track's, not
mine.** Executor told to REPORT and change no sigil.

### ⛔⛔ C2 AND F1r COLLIDE ON THE CORPUS PINS — **BOTH CORRECT ALONE, RED TOGETHER, AND NEITHER COULD SEE IT**

⭐ **Found BEFORE integration, by comparing the two diffs rather than by integrating and misreading the red.**
Regenerated on this branch:

| | `C_` | `LLVM_` | `SELFHOST_` | `MIN_FIXTURES` | new `spectests/run/*.gg` |
|---|---|---|---|---|---|
| **this branch** (`ls spectests/run/*.gg \| wc -l` → **243**) | 243 | 243 | 242 | 243 | — |
| **C2** `a1e57be97` | 244 | 244 | 243 | 244 | `cow_bare_for_elem_rebind.gg` |
| **F1r** `504bf5ad5` | 244 | 244 | 243 | 244 | `cow_scope_carried_sever.gg` |

⇒ **BOTH LANDED = 245 FILES AGAINST A PIN OF 244**, and `MIN_FIXTURES` is an **EXACT PIN**, not a `>=`
(`grep -n "EXACT PIN as of R48 close" -B 4 tests/spec_conformance.rs`). ⛔ **`spec_conformance` REDS AT
INTEGRATION ON ARITHMETIC, NOT ON BEHAVIOUR** — and a parent who did not know this would read it as a track
regression and bisect a green patch.

⭐⭐ **THIS IS MA-5's WARNING WITH REAL NUMBERS: *a diff of two PROGRAMS cannot show whether two PATCHES
compose*.** File-zone disjointness was never the issue — **the two tracks never touch the same `src/` line.**
They collide on a SHARED COUNTER, and **each measured the only tree it could see: base + its own fixture.**
⇒ ***Neither executor could have caught this, and no per-track review can either.*** **It is visible ONLY from
the round.**

⇒ ⛔ **AT INTEGRATION, EVERY ONE OF THESE IS RE-MEASURED FROM THE MERGED TREE AND SET FROM THE MEASURED OUTPUT —
NEVER BY ADDING ONE** (Core #5: regenerate, do not derive). ⊕ **And the same applies to `GGDEF_MATCH_FLOOR`,
`corpus_b`/`corpus_b1`'s opt-in-by-default census rows and cited `EXCLUDE` entries, `figures.db`'s mirrors, and
the two exact gate-set pins** — F1r measured **fifteen declarations, six files, four targets**, and **the
per-FIXTURE members move TWICE when both land while the per-ROUND members move once.** ⚠ **That
per-fixture-vs-per-round split is NOT in either track's report; it is asked of F1r's reviewer.**

⊕ **This joins `todo/t0801`'s class from the other side:** t0801 says *the landing boundary of a constant is not
the constant*; **this says the landing boundary is not even a property of ONE TRACK.**

### ⭐⭐ F1r's EXECUTOR REFUSED TWO BRIEFED INSTRUCTIONS — **AND IT WAS RIGHT BOTH TIMES**

⛔⛔ **MY BRIEF ASSIGNED A TRACK WORK THAT AN IN-TREE RULE RESERVES FOR THE INTEGRATING PARENT.** Regenerated in
MY tree (`grep -n "TRACK must" tests/integration.rs`):
> *"…which is precisely why **a TRACK must never write a pin**. The pin now has **exactly one writer, the
> integrating parent**, and `tests/lints.rs::clone_meter_pins_carry_their_provenance` makes a pin that moves
> without rewriting its `PINNED-BY:` line RED."*

⇒ **REFUSAL (b) — declining to re-pin — IS CORRECT, and my `E2`/`E-e` *"the re-pin is a DUTY"* was wrong.**
⊕ **E-e was worse than wrong: it required a same-`root_len`, UNSATISFIABLE across a track worktree and mine.**
⇒ ⚠ ***An instruction that cannot be executed from where the executor stands is not a demanding instruction,
it is a defective one.***
⇒ **REFUSAL (a) — not moving `RUNTIME_DIFF_MATCH_FLOOR` — IS ALSO CORRECT.** The gate is
`matched.len() >= FLOOR` (`grep -n "RUNTIME_DIFF_MATCH_FLOOR" tests/integration.rs`), so **a MATCH INFLOW
CANNOT RED IT**, and its own text says raise it *"in the same commit"* — which, under the one-writer rule, is
the INTEGRATING commit. ⚠ **A mid-round ratchet from one worktree is how a round close false-reds.**

⭐ **THIS IS THE THIRD BRIEF DEFECT OF MINE IN ONE FAMILY THIS ROUND: ASSIGNING TO A TRACK WHAT BELONGS TO THE
ROUND.** ⇒ ⛔ **BOTH ARE NOW MY OBLIGATIONS AT INTEGRATION, with the measurements parked in `t1365`** — the
*"named omission is durable content"* rule doing exactly the job it was landed for, on its first live use.

⚠ **AND THE CITATION DRIFTED 47 LINES** — the executor cited `tests/integration.rs:35032`; in my tree the text
is at `:34985`, because its own commit moved it. **Core #15(a) verbatim: cite the GREP that regenerates a line,
never the bare number.** *(Substance unaffected — but I could only confirm it BY grepping.)*

### ⛔ F1r ALSO CAUGHT MY CONSTANT COUNT AT LESS THAN HALF — A FILED, RECURRING CLASS

**Briefed "six". Measured: FIFTEEN declarations, SIX files, FOUR targets** — and **five of the omissions RED a
round-close target.** Nine were found **only by running the bare gates**: `scripts/figures.db` mirrors four
constants behind its own lint; new top-level `cow_*` fixtures are opted **INTO** `corpus_b`/`corpus_b1` **BY
DEFAULT** (3 census rows × 2, 2 cited `EXCLUDE` × 2); plus two exact gate-set pins. ⇒ **`todo/t0801`'s recorded
class RECURRING — *the landing boundary of a constant is not the constant.***

### 🚨 `t1364` IS A SUCCESSION-PLAN EVENT: **THE REFERENCE LAGS THE SELF-HOST**

**A mutation inside an `on error` block traps `T_UnwrapNone` on a run that NEVER ERRORS — on BOTH Rust
backends — and THE SELF-HOST PRINTS THE CORRECT ANSWER.** ⇒ **the oracle adjudicates AGAINST both backends.**
⛔ **A Core #8 filing, not a footnote**, and the direction is charter-fixed: **fix the RUST side as oracle
hygiene; NEVER dumb the self-host down to match.** ⊕ **My brief understated it IN THE TRACK'S FAVOUR** — the
direction a green suite never catches.

### ⚠ AN UNFLAGGED MA-9 HAZARD IN A COMMITTED SCRIPT — RECORDED HERE SO IT OUTLIVES THE BRIEF

`scripts/self_host_mem_baseline.sh` writes a **FIXED** `/tmp` path — regenerate:
`grep -n 'DRIVER_EXE' scripts/self_host_mem_baseline.sh` → `:63` `DRIVER_EXE="/tmp/self_host_driver_bench"`
(plus its generated `.c`). ⇒ **two agents running it on this box clobber each other SILENTLY.**
⚠ ***The `/tmp`-namespacing rule is breached by a COMMITTED SCRIPT, not by an agent — the rule binds briefs,
and NOTHING binds the repo.*** **Owed a filing from F1r's block.**

### ⭐ C2's DOC FOLD FOUND A SITE **NOBODY HAD NAMED** — WHICH IS THE POINT OF ASKING FOR A SET

**`a1e57be97`, docs-only, on top of `7785c1221`. 5 sites FIXED, and the 5th was found BY THE GREP, not by the
checklist:** a `devbook/11` per-loop-kind table row that described only the zero-copy view and not the
drop-safe one. ⇒ ⭐ **"present the SET with a disposition per row" is not bookkeeping — it is the only thing
that finds the row no enumerator thought of.**

⭐⭐ **AND THE EXECUTOR APPLIED THE NEW FOLD RULE TO A CLAIM IT WAS ASKED TO CARRY.** The same §9.1 note also
says a `String` element loses a mutation-through (`e.push('!')`). **Rather than propagate it, it MEASURED:
`aa` at BOTH the pre-fix and post-fix compiler** ⇒ unchanged by this fix ⇒ **the claim STAYS.** ⚠ ***That is
the retraction rule working in the other direction: I only ever caught myself retracting too WIDE — this is
the discipline that stops a fold deleting a TRUE neighbour of a false claim.***

⊕ **The `E5` sentence now records WHY there is no fixture instead of promising one:** a realloc-forcing shape
lands in the still-open `sound_excl_forbody_amp_writer` gap, so it would **pin THAT gap, not this tag.**
⛔ **No fixture invented** — which is the correct outcome of *"do not promise a fixture you do not name"*.

⊕ **`t1335` filed with the DISCRIMINATOR stated, not just the symptom:** unlike the rest of the
*"guard cannot see its own class"* family, **this guard's SUBJECT is present and correct — it loses the
producer because the producer STOPPED BEING NAMEABLE** ⇒ **widening either sibling does not reach it.**
(That is SIX-Q #4 used as a filing tool: *a case with no subject at all, which no widening fixes.*)

### ⛔⛔⛔ D0′ REBUILD #1 IS ALSO DEAD — THE WRITE SITE DOUBLE-FREES, AND ITS "ZERO CORRUPTION" WAS THE **NO-OP COLUMN**

⭐⭐⭐ **THE SINGLE SHARPEST MEASUREMENT OF THIS ROUND:** `diff c_HEAD_<s>.c c_WS_<s>.c | grep -c '^[<>]'` is
**0 on ALL ELEVEN matrix cells.** **The write-site prototype emits BYTE-IDENTICAL C to HEAD on every row of the
table that certified it "corruption-free".** ⇒ ***I CERTIFIED A FIX ON THE ONE AXIS WHERE IT DOES NOTHING.***
The matrix varied container × read-shape while holding **provenance fixed** at `Vector[…]()` + `^`-push — the
one provenance `gg_ws` never touches. ⛔ **SIX-Q #6 AT THE LEVEL OF A WHOLE CERTIFICATION TABLE, AND SIX-Q #3
FOR THE FOURTH PASS RUNNING.**

**Run as a PRODUCT instead, the `[]`-literal + bare index cell — seven lines of ordinary safe syntax:**
`Vector[Callable[int(int)]] fs = []` · two pushes · **`Callable[int(int)] f = fs[0]`** · `print(f(1))`
⇒ HEAD 16 B leak / prints `41`; **`gg_ws` rc 134 DOUBLE-FREE on C *and* LLVM.** From the emitted C, **the pair
IS wired** (`gorget_closure_free` at elem_drop, `gorget_closure_clone_inplace` at elem_clone) **and the read
still emits NO `clone_to_owned`** ⇒ ⛔ **answer (a) is TRUE AND INSUFFICIENT: the pair invariant governs
CONTAINER CLONE and says nothing about the ELEMENT READ.** My own brief said that under LANES and then offered
(a) as a live answer two sections earlier.

⇒ ⭐ **THE DISCRIMINATOR WAS NEVER WRITE-SITE-VS-READ-SITE. It is `elem_drop` INSTALLED AT ALL, FROM ANY SITE,
on a container whose element can be read through a bare subscript place.** `gg_d0` fires on the
`Callable__GorgetClosure` spelling (5 corrupting cells); `gg_ws` fires on `GorgetClosure` (1 corrupting cell +
4 genuine heals). **The write site is not SAFER than the read site — it is NARROWER.**
⛔ **AND MY SCOPE DIRECTIVE WOULD HAVE RESTORED THE REST:** *"close the provenance axis — THIS IS YOUR SCOPE"*
means installing `elem_drop` for BOTH spellings and for struct fields ⇒ **the union of the two prototypes'
corruptions**, including `httpserver.gg`'s 8 route sites. **My own mitigation note called that "luck, not a
guarantee" — and the directive spent the luck.**

### ⭐⭐ BUT THE KNOT UNTIES: IT IS A **PARSER** DEFECT WEARING AN OWNERSHIP COSTUME

**Three facts, each regenerated, that together settle it WITHOUT an owner ruling:**
1. **`AGENTS.md`'s own carve-out already decides the bind** (`grep -n 'single-owner-by-design' -A 6 AGENTS.md`):
   `Callable[T]` is single-owner-by-design, **`E_MoveWithoutOperator` at bare-assign sites**, user writes
   `^source` or `.clone()` — **and *"at a plain function / method call these types are simply borrowed"*.**
   ⇒ **`Callable f = fs[0]` MUST reject; `fs[0](1)` MUST borrow.** `gg_full` does exactly the first.
2. **ALL EIGHT `httpserver.gg` dispatch sites are BINDS, not callee positions**
   (`grep -n '_routes\[\|middlewares\[' lib/xtd/httpserver.gg` → `Callable[…] h = self.exact_routes[key]`).
   **So the reject hits all 8 — and the zero-cost recourse is the CALLEE spelling.**
3. ⛔ **AND THAT RECOURSE DOES NOT PARSE — FOR A REASON THAT HAS NOTHING TO DO WITH OWNERSHIP.**
   `src/parser/expr.rs:1123-1141` resolves `expr[...](` **UNCONDITIONALLY** in favour of a generic call when the
   brackets parse as type args, so a bare identifier index becomes a TYPE NAME:
   `d[k](v)` → `Call{callee: d, generic_args: [k]}` → `E_NotAFunction`. **`d["a"](5)` and `v[0](5)` parse, check,
   build and RUN today; only the VARIABLE-index forms reject.**

⇒ ⭐ **THE ORDER IS PARSER FIRST.** Fix the ambiguity → the 8 sites migrate bind → callee (**a borrow, zero
clones, no charter breach**) → the bind reject becomes ratified-AND-with-recourse → **`elem_drop` is then safe on
every provenance, because no bind can silently mint a second owner.** ⊕ **And `t1225`'s directive is HONOURED,
not overridden: we never widen the reject to the callee position — we make the callee position WRITABLE.**
⚠ **THE ZERO-CLONE YIELD IS A CLAIM, NOT YET A MEASUREMENT — the scout prototypes and measures it end-to-end
before any brief asserts it.**

⊕ **NOTE THE SHAPE C1 SHARES:** *a reject with no legal spelling*. **The parser fix IS the recourse here.**

### ⛔ AND THE REVIEW CAUGHT MY BRIEF STRIKING A **TRUE** CLAIM — THE RETRACTION RULE, ONE HOUR OLD, BROKEN AGAIN
I told the executor `t0873(a)`'s *"rc 139 SEGV"* was wrong and to overwrite it with *"heap-buffer-overflow"*.
**Measured unsanitized: rc 139. Under ASan the SAME defect reports heap-buffer-overflow.** ⇒ **the item is
correct as filed and I was about to overwrite a RED-verified symptom with an INSTRUMENT ARTIFACT.** ⛔ **DELETE
THAT CORRECTION.** ⚠ *This is Core #13 inverted: I picked the instrument that could see the class, then quoted
its reading as if it were the symptom.*

⊕ **ERRATA that make the brief's own regeneration path a dead letter:** `plainrun.sh`/`llvmrun.sh`/`dirdiff.sh`
hardcode **PRUNED** worktrees and swallow the failure (`2>/dev/null`) ⇒ every row returns `BUILD_FAIL`;
`llvmrun.sh` loops `gg_head gg_d0` only and **cannot measure `gg_ws` at all**, so the brief's LLVM claim for the
write site is **not regenerable by the command it names**; and *"sources for every cell are in `mx2/`"* is 8 of
10. ⊕ `retire_fatal`'s *"shrinks OR vanishes"* reach holds **only when `COVERAGE_FLOOR > 0`**.
⇒ ⚠ **THE ORACLE SET MUST BE PRESERVED WHOLE, NOT JUST `gg_ws`** — `gg_head`/`gg_d0`/`gg_full` are each other's
controls and MA-6 prunes them at round close.

### ▸ D0′ v3 IS A SCOUT, AND THE **CHEAP TRUE THINGS MUST NOT DIE WITH THE BRIEF**

⭐ **Even a track that ships no fix this round owes these — all measured, all independent of which route wins:**
`t1393` (Deque's index read elides `clone_to_owned`: 2 vs 3; **width is correct at 16 B**) · `t1394`
(`Shared[T]`/`Box[T]` element never reaches the ctor decider) · `t1395` (`Vector[Box[UserStruct]]`
`redefinition` — **PRE-EXISTING, identical on all four oracles**) · `t1396` (`Set`/`HashSet` accept a
non-`Hashable` `Callable`) · the **3 Core #2 `starts_with("Callable__")` sites** · the **Core #14 dead
"defensive backup" comment** · the **`Callable_Foo` prefix hazard** (prefix + next-byte-`_`, so a SINGLE
underscore matches). ⚠ **The MECHANISMS are recorded ABOVE, in this committed file, precisely because briefs are
`/tmp`-only and pruned at round close — that is the new rule applied to its own author.**

⊕ **THE ORACLE SET IS PRESERVED WHOLE** — `gg_head` · `gg_d0` · `gg_full` · `gg_ws` + cell sources, out of
`/tmp` and out of MA-6's round-close prune. **They are each other's CONTROLS; keeping only the "winner" is how
the no-op column went unnoticed.**

**Readiness: 0 of 5.** ⛔ **SIX-Q #2 IN ITS PUREST FORM: the full sanitize sweep, the 20-fixture set AND the
10-cell matrix ALL PASS `gg_ws` while it double-frees.** The prescribed guard set green-lights the class it
exists to retire — and *"the 20-fixture corpus contains no `[]`-literal + bare-index cell"* is **coverage luck,
not safety.**

### ⭐ THREE FOLD RULES LANDED IN `AGENTS.md` — THEY WERE LIVING ONLY IN A HARNESS PROMPT

**`AGENTS.md` says it in its own words: *state every excellence-system rule HERE, never only in one harness's
private memory.*** These three were earned this round and were living exactly there. **Each SHARPENS AN
EXISTING RULE IN PLACE — no second dated copy, no sixth readiness row (the FIVE are CAPPED):**

- **Readiness row (4)** now reads *"the GUARD FAILS when the fix is reverted — **and a fixture set is complete
  only when EVERY PARTIAL REVERT turns a row RED: enumerate the reverts, name the row that pins each**"*.
  ⇒ **a revert with no pinning row is a HOLE, not a footnote.**
- **The FOLD paragraph** gains: ⛔ ***a fold may only assert what a command in that same fold REGENERATES*** — a
  figure quoted from a scout is NOT exempt, and the instrument must be able to SEE the class it is asked about;
  ⊕ ***a retraction quotes the retracted claim's OWN scope*** — **retract wider and you delete a true claim;
  retract narrower and the false one still stands.** (Both measured on my own folds this round.)
- **The `/tmp`-only paragraph** gains: ⚠ ***a NAMED OMISSION is durable content*** — *"not measured"*,
  *"scoped enumeration"*, *"lane not run"* noted only in a brief **dies WITH the brief at round close**, and
  *nothing recorded what was never run* **is how a family gets declared closed.**

**Verify:** `sed -n '286p;288p;312p' AGENTS.md`. Size ratchet + heading inventory green (`cargo test --test lints`,
rc off the bare command); **no new heading**, so `agents_md.heading_count` does not move.

### ⚠ F1r's EXECUTOR PARKED ITSELF ON A WAITER THE HARNESS DOES NOT TRACK

It stopped with *"waiting on the self-host lane… I'll resume when the background waiter fires"* — **but a
background waiter is not a harness-tracked child, so nothing was ever going to wake it.** ⭐ **Its build IS
live** (rustc on `src/lib.rs`, pids confirmed in ITS worktree), and **its tree carries substantial uncommitted
work** ⇒ **DO NOT PRUNE IT.** Resumed by message, told to poll its OWN run in the foreground, to checkpoint
before anything else, and — ⛔ **because its tree shows `TODO.md` dirty from a snapshot taken at spawn** — **not
to stage `TODO.md`, which would revert every handover write since.**
⚠ **THIS IS THE SPAWN-FREEZE HAZARD IN ITS SECOND FORM:** the known one is *a reviewer reports a just-filed item
as missing*; this one is *an executor silently reverts the session state doc*. **Same cause, and every brief
already states the base commit — the missing half is telling executors which files are NOT theirs.** ⇒ landed as
`AGENTS.md` Multi-agent rule 3 (`grep -n "HANDOVER BLOCK IS NEVER" AGENTS.md`): *naming the base commit tells an
agent what it HAS, not what it may TOUCH.*

⛔ **AND MY FIRST SPELLING OF THAT RULE WAS TOO WIDE — I RETRACTED IT WITHIN THE HOUR, WHICH IS THE NEW
RETRACTION RULE EATING ITS OWN AUTHOR.** I told F1r *"do NOT stage or commit `TODO.md`"*. ⭐ **Measured against
C2's actual commit, the ONLY thing it wrote to that file was the GENERATED INDEX** — correct, required, and red
without it (`todo_index_is_current`). ⇒ **the carve-out is real and both halves must be stated: the INDEX is the
track's; the HANDOVER BLOCK is not.** ⚠ ***A ban wider than the hazard is not the safe direction — it would have
turned a green track red.***

### ⛔⛔ D0′ IS **REBUILT**, NOT AMENDED — ITS PRESCRIBED FIX REDS A **COMMITTED, WIRED** FIXTURE ON BOTH BACKENDS

⭐⭐ **THE ROUTE WAS WRONG, AND ONLY A BUILD COULD SHOW IT.** `tests/fixtures/dict_box_callable.gg` — top-level,
in-corpus, wired (`grep -n 'dict_box_callable' tests/integration.rs`) — is **CLEAN at HEAD** and under the
prescribed read-site patch **aborts printing nothing**: `rc=134`, `free(): double free detected in tcache 2`.
**On the LLVM lane too.** ⇒ the C sweep, the LLVM sweep **and** `CORRUPTION_CEILING` (an `assert_eq!`) all go
red — not just the sanitize sweep the brief was reasoning about. ⭐ **And the reviewer reproduced it with a
compiler IT built, not only from the pinned binary.**

⛔ **IT IS NOT ONE CELL — THE PATCH SHIPS FIVE CORRUPTING CELLS.** Vector · Dict · Deque · HashMap, on the
**bare index read** (plus Deque's `.clone()` read). ⇒ **MY BRIEF NAMED ONE CELL OF THE CLASS AND ITS REMEDY —
*"gate the fallback to exclude the Deque ctor path"* — WOULD HAVE LEFT FOUR.** The class is *a collection
element read that emits no `gorget_closure_clone_to_owned` while the local is still dropped as owned*.

⭐ **THE SEQUENCING WAS INVERTED, AND `todo/t1225` ALREADY SAID SO IN ITS OWN WORDS** — *"do not widen the
reject ahead of it"*. Measured: **patch + `t1225` REJECTS** all four bare-read cells at check; **patch alone
ACCEPTS and double-frees** them. ⇒ **the read-site `elem_drop` is what makes the missing clone LETHAL**, and at
HEAD the program is **ACCIDENTALLY CORRECT (SIX-Q #6)** — the NULL `elem_drop` exactly cancels the missing
clone-on-read. ⊕ **It also ESCALATES a filed HIGH item**: `t1225`'s discriminator is *"the same element is read
more than once"*, but once the container is the second owner **one bind is enough** — the property that keeps
`httpserver.gg`'s 8 dispatch sites safe stops holding.

⭐ **THE WRITE SITE WINS ON MEASUREMENT, NOT ON QUOTATION.** Over the 20 top-level collection-of-`Callable`
fixtures, in the allowlist's own unit: **identical leak burn-down, ZERO corruption**, plus `t0873(b)`'s repro
graduating. ⇒ the read-site route is not a *more complete* alternative — **it buys nothing extra and costs a
double-free.**

⚠ **AND THE PRESCRIBED ROUTE'S PROTOTYPE SOURCE IS LOST.** Two passes built and measured it; **neither
checkpointed the diff**, both worktrees are gone, and the quoted build id is not a reachable commit
(`git cat-file -t 14148dc6` → *Not a valid object name*). **Only the BINARY survives** — I copied it out of
`/tmp` into the session scratchpad as the oracle. ⇒ **MA-9's "checkpoint EARLY" is not about crash recovery
alone; an unsaved prototype makes the NEXT agent retype it, which is exactly how a fold ships a defect.**

⛔ **TWO CLAIMS IN THIS HANDOVER WERE MEASURED FALSE AND ARE STRUCK ABOVE:** `Set`/`HashSet` do **NOT** reject
non-`Hashable` elements at check time (they accept and leak), and `t1393`'s *"8-byte mis-size ⇒ rc 139"* is
false in **both** halves. ⊕ **`t1393` was never a filed item at all** — it is an unallocated id, so the brief's
instruction to *"correct the item"* was unexecutable.

⭐ **THE ONE QUESTION THE REBUILT BRIEF PUTS FIRST:** the write-site fix installs a `drop_strategy` too — **so
why does it NOT reproduce the double-free?** If the answer is *"it also supplies `clone_inplace_fn`, so drop and
clone are wired as a PAIR"* it is reference-grade. If the answer is *"the decider is never queried with that
spelling on the read path"* it is **accidentally correct and the same corruption is ONE registration away.**
**The executor is told not to proceed past that question.**

### 🟢 G's EXECUTOR IS LAUNCHED — AND PASS 6 CAUGHT A SELECTION **INSIDE THE ERRATUM ABOUT SELECTIONS**

⭐⭐ **My E-c measured 198 cells — TWO of the seven shipping payloads — and generalised to 693. That was
SIX-Q #3 INSIDE THE ERRATUM THAT ANSWERS SIX-Q #6.** Pass 6 ran the other five payloads (**990 builds**):
**0 cells where the source reads its pre-mutation value, 0 verdict disagreements over 495 comparable cells** ⇒
**E-c now holds over the WHOLE forward corpus.** ⊕ **And D15 was sharper than I stated: 2 of the 21 leaked rows
are `REJECTED` — ggdef rejecting a program the map says should print a value, ratcheted in with zero review.**

⛔ **FIVE MORE OF MY OWN, AND TWO ARE OPPOSITE ERRORS IN ONE FOLD:**
- ⛔ **I OVER-GENERALISED (D19):** I called the drift branch *"the SAME defect"* and gated both behind one flag.
  **They are different: the seed branch leaks WITHIN the topic (needs a branch guard); the drift branch leaks
  ACROSS topics, where `--topic` genuinely DOES fix it.** ⇒ **`--seed-new` over a drift row is a LIE, does not
  fix the rider, breaks a measured rc 0 — and coupling two unrelated widenings behind one flag IS THE VERY
  CLASS DEFECT D15 DIAGNOSES.**
- ⛔ **I UNDER-GENERALISED (D21):** my replacement topic string fixed the **direction** half and **broke the
  SOURCE half** — *"9 sources"* reads as *"there ARE 9"*, not D8's *"9 of N"*, **and D5's entire point is that
  four source kinds are unprobed.** ⚠ ***My own standard — "a WRONG durable disclosure is worse than an
  incomplete one" — broken in the fold that repaired the other half.***
- ⛔ **THE PROMOTION HAD TWO MUTUALLY EXCLUSIVE POLARITIES.** D9 decided an **exempt list**; D14 mandated a
  patch implementing a **fatal allowlist** — the opposite — and **nothing retracted the rider.** ⚠ **Under the
  measured polarity the rot INVERTS: rename the topic and the prefix matches ZERO rows, so the promotion
  SILENTLY GOES INERT.** ⇒ **fatal-prefix `"30 "`, D9's rider RETRACTED, and ONE lint running BOTH directions.**
- ⛔ **The control cell would be RED ON ARRIVAL on 30% of naive picks** — on the 21 broken rows the compiler
  **already prints the mutated value**, so the control MATCHES and fires *"CONTROL PASSED — harness is blind"*.
  ⚠ **Worse than red: it SILENTLY INVERTS MEANING.**
- ⛔ **And my *"the landing command stays correct"* was FALSE under my own fix** — with `--seed-new` it writes
  nothing and exits **rc 0**, a silent no-op.

⭐ **THE MEASURED ANSWER TO "IS THE MIRROR SLICE REDUNDANT?" — IT IS NOT.** The two directions differ on
**exactly one cell of the SOURCE axis**: `getvia` is **1/7 payloads broken forward** but **7/7 in the mirror**;
all eight other sources are identical. ⇒ **the mirror finds six payload-cells the forward direction
STRUCTURALLY CANNOT.**

### ⛔ G's FIFTH FOLD: A GUARD THAT WOULD BE SATISFIED BY THE BRANCH IT POLICES

⛔ **D13's *"one flag, both problems"* is FALSE as a class claim.** Measured with the patch applied:
`--accept --topic "11 closures"` **silently wrote 21 LEGACY rows** while printing
**`baseline updated (0 progress rows folded)`** — twenty-one changed, zero reported. **The patch has NO topic
guard** (verified: zero occurrences) ⇒ **`--topic` is a RUN-SCOPING flag standing in for a BRANCH GUARD.**
⛔⛔ **And that is what makes it blocking: precondition 1's lint — *"every row measured on a lane has a
non-empty baseline"* — would be SATISFIED BY THE VERY BRANCH IT EXISTS TO POLICE. SIX-Q #2.**
⊕ **Rider (b) is the same defect in a second branch, and my answer there was again "scope it with `--topic`"** —
**two instances of one class, answered twice with the instance fix.**
⭐ **THE FIX IS PASS 3's OWN ERRATUM, WHICH I DISCARDED: an explicit `--seed-new` flag on both widening
branches, with `N rows SEEDED` printed.**

⛔⛔ **AND THE DURABLE DISCLOSURE STRING IS WRONG UNDER MY OWN LATER FOLD.** A3 ratified
`… (9 sources, 1 direction)`; **A3's D11 then decided to SHIP the mirror slice and A4 confirmed it** ⇒ **the
corpus ships TWO directions and the string would say "1".** ⚠ ***That is verbatim D11's own standard — "a WRONG
durable disclosure is worse than an incomplete one" — reproduced by the fold chain.***
⚠ **And I had the exempt-list mechanism BACKWARDS:** topic 30 is **not** among the 33 (verified), so the list
does not rot that way — **it rots the OPPOSITE way: renaming any of the 33 silently drops it and makes its
drift FATAL, reddening the map for drift nobody caused.**

⭐ **FOUR ERRATA CHANGE GENERATED OUTPUT — the most valuable one SAVES work:** **SIX-Q #6 on the FORWARD
direction is answered NO, measured over 198 cells** (view-only and both-observed verdicts agree on all 198)
⇒ ⛔ **DO NOT widen the forward observation — doing so would invalidate D3's 60/45/15 and the 210 baseline FOR
NOTHING.** ⊕ **And D12's rule had no subject for 6 of 9 sources (SIX-Q #4)** — the source observation is a
place **expression**, not a name ⇒ **restate over PLACES: *"a mutation through one place is observable through
that place only; every other place reads the value it had before."*** ⊕ **Multi-line output joins with `" / "`,
not a newline — unsaid, EVERY mirror cell baselines WRONG for a reason unrelated to the compiler.**

⚠⚠ **PASS 6 IS BRIEFED WITH A WARNING I SHOULD HEED TOO: this track is at risk of being REVIEWED rather than
SHIPPED. The DESIGN has been signed off four times over; what keeps failing is MY PRESCRIPTIONS. Hold the bar,
do not manufacture work — if the two one-line fixes are right, LET IT LAUNCH.**

### ⛔⛔ D01 IS RE-SPLIT — THE MERGED COMMIT SHIPS A MEMORY-CORRUPTION REGRESSION, AND HALF OF IT DECIDES AN OPEN OWNER QUESTION

**The merge premise was confirmed cell by cell** — HEAD really is wrong in both cells of the controlled pair,
and `CORRUPTION_CEILING` really is an `assert_eq!`. ⛔ **But the merge's CONCLUSION is false over an axis nobody
enumerated.**

⛔⛔ **B1 — THE FIX TURNS A LEAK INTO A DOUBLE-FREE ON `Deque[Callable]`:**

| same shape | HEAD | D0 half | D0+full |
|---|---|---|---|
| `Vector[Callable]` | 32 B leak | **CLEAN** | CLEAN |
| `Dict[String,Callable]` | 32 B leak | **CLEAN** | CLEAN |
| **`Deque[Callable]`** | 16 B leak | ⛔ **double-free** | ⛔ **double-free** |

**Mechanism, from the emitted C: `Deque`'s `main` DOES NOT emit the `.clone()`** — it is silently elided, the
local aliases the element, and the newly-installed `elem_drop` frees the env the local also frees.
⇒ ⛔ ***"Together → exactly one free" holds for Vector and Dict and IS A DOUBLE-FREE AT DEQUE — and the
brief's OWN fatality argument applies to the merged commit.***
⛔ **AND `t1393` IS NOT "STRUCTURALLY UNREACHABLE" — IT IS A PRECONDITION.** I wrote that the decider is never
called so the fix cannot reach it; **measured, the fix demonstrably CHANGES Deque behaviour.** ⚠ **And the
reviewer could NOT reproduce `t1393`'s stated `rc 139 SEGV` — RE-DERIVE THAT FILING'S SYMPTOM BEFORE QUOTING
IT.** ⊕ It is invisible today only because **no `Deque[Callable]` fixture exists in the tree** (verified) — so
it lands as **inflow** the moment the 60-cell matrix does.

⛔ **B5 — AND THAT IS THE PROOF THE 60-CELL ENUMERATION IS A SELECTION: it has no READ-SHAPE axis.** Run under
the fix with a `.clone()` read, the Deque cell would have shown B1. It was not. **Name all three axes; run the
matrix at HEAD *and* under the fix; cover {push-only, bare index read, `.clone()` read, read-and-call}.**

⛔ **B3/B4 — THE ROOT CAUSE IS TWO MECHANISMS CONFLATED, AND HALF OF MY STATEMENT IS FALSE.** Instrumented:
**Vector** → the registrar **fires 8×** but the LIR asks the **other spelling** (a pure *spelling mismatch*);
**Dict** → the registrar **never fires** (a pure *missing registration*). **"Never called at all" is false for
the case my paragraph was about, and the two cells fail for OPPOSITE reasons.**
⇒ ⛔ **And Core #1 bites: I picked a READ-SITE fallback while my own evidence named the WRITE-SITE defect.** The
reviewer **prototyped the write site: ONE edit** (register the missing singleton), measured, healing the Vector
cell. ⇒ **take the write-site route, or present the enumeration that shows it is open-ended — with a
disposition per row.**

### ⚖⚖ SECOND OWNER ASK — `t1225` DECIDES A QUESTION WHOSE RULING IS **NOT IN THE LEDGER**

⛔ **`t1225`'s OWN TEXT: *"the sequence is: land the callee-borrow rule, then this closes; **do not widen the
reject ahead of it**."*** The same sentence is in `src/semantic/safety/check_expr.rs` and `docs/devbook/11`.
⛔ **And the ruling it names is ABSENT from the ratified ledger — verified again this heartbeat: zero hits.**
⛔ **The prescribed diff IS the 8-site `.clone()` on `lib/xtd/httpserver.gg` that the item calls the charter
breach the ruling exists to remove.** My only counter was a COST premise — **which is not what the ruling is
about**, and rests on **one wall-clock pair (n=1, no variance) showing an ~8% IMPROVEMENT from ADDING clones,
which reads as noise.**
⊕ **B6 has teeth: under the fix, `known_gaps/callable_index_place_double_free.gg` flips rc 0 → REJECT** — and
`t1225` says pinning the reject *"would pin one of two open answers."* **The commit silently picks one.**

⇒ ⭐ **RE-SPLIT (the reviewer's own recommendation): `D0′` = the `elem_drop`/`elem_clone` half **plus the Deque
precondition**, self-contained. `D1` (`t1225`) HELD on the owner ask.**

### ⭐⭐ TWO TRACKS INDEPENDENTLY FOUND THE SAME UNNAMED ZERO-SLACK GATE — IT IS SYSTEMIC

**C2's pass 7 and F1r's pass 3, on different fixtures, the same day, both found `GGDEF_SKIP_CEILING`** — a
**SHRINK-ONLY** ratchet in `spec/ggdef/tests/spec_conformance_ggdef.rs`, **at ZERO SLACK** (measured:
`total=243 · MATCH=225 · MISMATCH=0 · GGDEF-SKIP=18`, **exact**). ⛔ **`--test spec_conformance` NEVER REACHES
IT — it is a different crate and a different cargo target.** An **out-of-subset** seed becomes a `GGDEF-SKIP`
and **reds `cargo test -p ggdef`**, a round-close battery target, **with a message that MISDIAGNOSES the
cause** and says *"Never raise it to make a new seed fit."*
⇒ ⭐ **THAT CEILING IS THE ENFORCER FOR "partition per cell with ggdef" — the out-of-subset half is a
`tests/fixtures/*.gg` because the gate SAYS SO, not as a style choice.** ⊕ **And `#!spectest` frontmatter is
MANDATORY** — generate it with `ggdef gen`, never by hand.
⛔ **`cargo test -p ggdef` IS NOW IN BOTH TRACKS' GATE LISTS.** ⚠ **Two independent discoveries in one day means
the next track will hit it too.**

### 🟢 F1r's EXECUTOR IS LAUNCHED — AND ITS LANDING BOUNDARY HELD THE REMAINING RISK

**The design was signed off and re-measured end-to-end twice**; pass 3 added **nothing** against it. All four
blockers were **brief completeness at the landing boundary**:
- ⛔ **The constant set is SIX, across TWO CRATES and THREE TEST TARGETS** — the brief knew four.
- ⛔ **Graduating `t0750` moves TWO runtime-diff constants**, because **ggdef ADJUDICATES that fixture and
  agrees with POST** ⇒ it lands in ADJ-MATCH and `GGDEF_ADJUDICATED_FLOOR` bumps too.
- ⛔ **"Name the omission" was not available: the firing comprehension cell EXISTS and pass 3 BUILT IT** (25
  lines, `PRE a-comp 1 1 3 → POST 1 4 3`). **5 of the 8 loop-hook sites had fire count ZERO without it.**
- ⛔ **The gate list omitted BOTH gates Core #7 names** — bootstrap and sanitize sweep — **on a change that
  hoists materializations into loop pre-headers**, i.e. new heap allocations and drop obligations, *"precisely
  the axis ASan adjudicates and ggdef is structurally blind to."*
⭐⭐ **AND THE CORE #8 ANSWER THE BRIEF NEVER SUPPLIED: THE ORACLE FOR THE OUT-OF-SUBSET HALF IS THE SELF-HOST.**
ggdef reaches neither user-`Iterator` `for` loops nor `on error`; **the self-host adjudicates both and agrees
with POST.** ⇒ **no cell rests on C/LLVM agreement alone.**
⊕ **`t1225`-style erratum caught: my *"established precedent at FOUR sites"* is off by ~32× — it is the
DOMINANT house pattern.** **The disposition survives for a different reason**, and **the number is dropped** —
which is the brief's own closing lesson applied to the brief.

### 🟢 C2's EXECUTOR IS LAUNCHED — SEVEN PASSES, AND THE LAST ONE SIGNED OFF THE DELIVERABLE

⭐ **Pass 7 reproduced all four lanes on the exact candidate, and its self-host measurement is STRONGER than the
one it checked: it rebuilt the driver with the PATCHED compiler** — the configuration `spec_conformance_selfhost`
actually uses. ⭐ **And it discharged a harness risk nobody had named: `ggdef gen` on the hand-written candidate
is BYTE-IDENTICAL, so the prescribed `expect:` block is already the gen fixed point** — which `gen_idempotent.rs`
requires and whose doc forbids hand-editing.

⭐ **DECIDED — the spectest REPLACES the top-level fixture.** The fork had **different constant obligations**:
replace ⇒ five bumps and **zero parity inflow**; keep both ⇒ a top-level fixture entering the parity corpus, a
**hand-run** self-host MATCH (⚠ **the non-MATCH ceiling NO-OPS IN DEBUG**), and a **seventh** ratchet.
**The spectest is four-lane BY CONSTRUCTION — precisely what going top-level was for.**

⛔ **THERE IS A SIXTH CONSTANT, AND ITS CORRECT ACTION IS *NO CHANGE* — WHICH IS EXACTLY WHY IT MUST BE NAMED.**
`GGDEF_SKIP_CEILING` is **shrink-only — the second direction the MATCH floor structurally cannot see.** The new
seed MATCHes, so the floor rises and the ceiling is untouched. ⚠ **Had the seed landed OUT-OF-SUBSET it would
have become a SKIP, the floor would NOT have risen, the ceiling would have gone RED — and the executor's
obvious move is the one that file's own comment calls "the drift it exists to stop."**
⛔ **And it lives in a DIFFERENT CARGO TARGET: `--test spec_conformance` never reaches it. `cargo test -p ggdef`
added to the gate list.**

⛔⛔ **AND THE CHECKLIST BUILT TO STOP DROPPED PRESCRIPTIONS DROPPED ITS OWN HEADLINE — TWICE.** D24 was missing
**four rows and one undecided fork**, including **no row at all for the spectest + five bumps**, the single most
consequential deliverable in the addendum that created it. ⇒ **D24 is now 19 rows, row 8 is resolved, and the
executor is told: *if something in an addendum has no row, that is a defect in the CHECKLIST, not an optional
item.***

### ⛔⛔ D0 AND D1 ARE **ONE OWNERSHIP INVARIANT** — MERGED. NEITHER IS SHIPPABLE ALONE.

**D0's fix clears NONE of D1's three fixtures — and the correct conclusion is STRONGER than "blocked".** A
controlled pair, one edit apart:

| program | HEAD | D0 fix |
|---|---|---|
| `dict_box_callable.gg` verbatim (bare index read binds a 2nd owner) | rc 0 **CLEAN** | rc 1 **double-free** |
| same file, the read made **owning** (`.clone()`) | rc 1, **16 B leaked** | rc 0 **CLEAN** |

⭐ **HEAD IS WRONG IN BOTH CELLS.** One leaks; the other is **ACCIDENTALLY CORRECT (SIX-Q #6)** — *the missing
element-drop and the extra owner CANCEL.* ⇒ **D0's fix does not create `dict_box_callable`'s bug; it UNMASKS
`t1225`'s.**

⛔ **SO THEY ARE NOT PREREQUISITE-AND-DEPENDENT — THEY ARE TWO HALVES OF ONE INVARIANT:**
- **D0 alone** → a clean fixture becomes a **double-free**, and `CORRUPTION_CEILING` is `assert_eq!(…, 1)`
  (verified) ⇒ admitting a row **ships a known double-free. FATAL.**
- **D1 alone** → the three fixtures leak. **FATAL.**
- **Together** → the collection owns and drops; the read binds a borrow or an explicit clone ⇒ **exactly one
  free.** Proven by the `.clone()` control.
⇒ ⭐ **MERGED AS D01: ONE TRACK, ONE COMMIT.** `dict_box_callable.gg` needs a source edit in that commit; its
pinned stdout is preserved — measured.

⛔ **`t0873(b)` NEEDS REWRITING, NOT EXTENDING — ITS FILED MECHANISM IS MEASURED FALSE.** For `Vector` the
element type **IS** `FnPtr`, the rewrite **does** fire, and the lookup **still misses**. **The real mechanism:
the `GorgetClosure` TypeDef is NEVER REGISTERED in the `TypeRegistry` the LIR reads** — `register_callable_alias`
is **never called at all**, and `GorgetClosure` is **missing from the eagerly-registered singleton set**.
⚠ **And the two paths pass DIFFERENT STRINGS** — `"GorgetClosure"` (minted in the LIR) vs
`"Callable__GorgetClosure"` (minted by the GIR mangler) ⇒ **a fix covering one spelling covers neither.**

⭐ **THE ENUMERATION IS TOTAL OVER 60 CELLS, with two independent witnesses** — rustc exhaustiveness over
`CollectionCtorKind` (6 variants) × the **schema-versioned SSoT** `compiler/data/resources.gg` (consumed by
both Rust and the self-host). **All 60 built and run under LSan.** ⛔ **BUT *"Set/HashSet reject non-Hashable elements at check time — a
REJECTION, not an omission"* IS MEASURED FALSE (2026-09-05):** `Set[Callable].add(^a)` and
`HashSet[Callable].add(^a)` are **ACCEPTED and leak 16 B**, at HEAD and under **all three** prototypes ⇒ **the
axis is NOT closed, and a `Set` of a non-`Hashable` `Callable` compiling at all is its own soundness smell.**

⛔ **THREE NEW DEFECTS THE ORIGINAL FRAMING WOULD HAVE HIDDEN — a capped enumeration shows one Callable column;
the total matrix shows four:** **`t1393`** ⛔ **MECHANISM WITHDRAWN 2026-09-05 — MEASURED FALSE, TWICE OVER.** *"mis-sizes its element to
8 bytes ⇒ rc 139 SEGV"*: Deque and Vector emit the **SAME** ctor width (`16`), at HEAD and under the patch, and
**both natural Deque shapes are rc 0 at HEAD** — no `139` from either. *"the ctor never calls the decider"* is
false too: the patch demonstrably CHANGES Deque behaviour. ⭐ **The REAL mechanism: Deque's index read silently
ELIDES `gorget_closure_clone_to_owned`** — Deque emits 2, Vector and Dict emit 3 for the same-shaped program ⇒
**leak at HEAD, escalating to double-free the moment any `elem_drop` lands.**;
**`t1394`** `Shared[T]`/`Box[T]` as an element never reaches the ctor decider ⇒ leak; **`t1395`**
`Vector[Box[UserStruct]]` **does not compile on either backend** (`redefinition of 'Box__Blob__drop'`).

⛔ **THE ALLOWLIST PREDICTION WAS WRONG IN BOTH DIRECTIONS.** The two `httpserver` rows **DO NOT MOVE** — their
frame #2 is `main`, a closure **literal** env (`t0953`'s class), a different allocation entirely. **But FIVE
other rows do**, and **one is CITED ⇒ `retire_fatal`, rc 1** unless tightened in the same commit.
⚠ **And that table is itself a SELECTION — 18 fixtures probed against the WHOLE SWEEP POPULATION (regenerate: `grep -n 'sanitize.coverage_floor' scripts/figures.db`). The executor owes
a FULL sweep before touching `LEAK_CEILING`.**

⭐⭐ **THE REFERENCE LAGS THE SELF-HOST, AND THE SELF-HOST SAYS THE SAME THING IN ITS OWN WORDS.**
`lir_codegen.gg` already handles **both spellings** and wires drop+clone **as a pair**, with a comment naming
**the exact double-free the drop-only revert produces**: *"elem_drop and elem_clone MUST be wired as a pair —
wiring drop without clone turns the prior leak into a clone-path double-free."* ⇒ **the self-host arrived at
this design first; that is independent corroboration the shape is reference-grade. Do not touch the SH.**
⊕ **Revert table shipped, and R3/R4 are honestly named as UNCATCHABLE by any fixture** — they need unit tests,
and the scout says so rather than papering over it.

### ⛔⛔ G's D6 WAS WRONG TWICE, AND THE SECOND WAY MEANT THE CORPUS COULD **NEVER** BE BASELINED

**Both blockers are in the decision the brief itself flags as determining whether the track lands.**
1. ⛔ **D6's RED-VERIFY RECIPE CANNOT GO RED.** Measured in exactly D6's direction: **`WRONG → CRASH` scores as
   DRIFT, which the report says in its own words is *"report-only, does not gate"* — `BARE_RC=0`.** ⇒ **seeding
   moves the 210 cells from INVISIBLE to REPORT-ONLY, not to GATED. SIX-Q #2 still answers NO for the exact
   transition D6 named, on the exact 30% D6 named. D6 refuted its own justification.**
   ⭐ **Fix: seeding PLUS promoting DRIFT to fatal FOR THE NEW TOPIC.** The drift branch carries its own staging
   (*"report → measure → burn down → THEN promote"*), and **a brand-new fully-seeded topic satisfies that
   staging ON DAY ONE BY CONSTRUCTION — day-one drift is 0 — so it can be BORN AT STAGE 3** while the legacy
   1242 stay at stage 1. ⊕ **An exempt-topic list seeded with the 33 existing topics makes the NEXT new topic
   fatal by default (Core #4).**
2. ⛔⛔ **D6's "ONE CONDITION" CHANGES NOTHING — `new_div` DEADLOCKS THE WRITE FOREVER.** Pass 3 applied my
   prescription **verbatim** and measured the row still empty, `--accept refused … 1 NEW DIVERGENCE(S)`,
   `BARE_RC=1`. **Mechanism:** an all-empty row is not "divergent", `--accept` can only CLEAR the divergence
   column never SET it, and the write is refused **wholesale** on any `new_div`. ⚠ **And `t0750` records the
   self-host is CORRECT on this family while C/LLVM are wrong ⇒ ~210 NEW DIVERGENCES on the first full-lane
   run, EVERY RUN, FOREVER. THE CORPUS COULD NEVER HAVE BEEN BASELINED.**
   ⭐ **Measured two-condition patch shipped: `/tmp/revG3_a219d1a6/recover_a219d1a6_seed_fix.patch`.**
   ⛔ **AND A PARTIAL SEED RE-DEADLOCKS — SEED IN ONE `--lanes all --accept` RUN, NEVER LANE-BY-LANE.**

⛔ **AND MY D8 SHIPPED A FALSE SYMMETRY CLAIM INTO WHAT WAS MEANT TO BE THE DURABLE DISCLOSURE.** I wrote
*"(`local`/`alias2` are symmetric; the rest are not)"* — **refuted by the very item I cited.** `t0750`: *"both
directions break … (A) mutation THROUGH THE ALIAS loses every write … (B) mutation THROUGH THE ROOT never
severs the alias"*, **different symptoms**, and *"whether A and B are one mechanism or two is OPEN."*
⇒ **DIRECTION is unprobed for 9 of 9 sources, not 7 of 9.** ⚠ ***A WRONG durable disclosure is worse than an
incomplete one*** — and the durable disclosure is D5's entire purpose.
⭐ **And my *"DIRECTION has NO WITNESS"* was also wrong, in the track's favour: `t0750` + `cow_transitive_alias.gg`
ARE the witness — a MEASURED BEHAVIOURAL one, stronger than the other axes' enumerations.**
⭐ **DECIDED: SHIP THE MIRROR SLICE** at the one discriminating site pair — **126 cells, +18% not +100%.**

### ⭐ F1r's PASS 2 — AN ALARM I RAISED WAS UNMEASURED, AND MEASURED IT RESOLVES **SAFE**

⛔ **I warned that graduating `t0750` adds a row to the ZERO-SLACK runtime-diff denominator. Pass 2 MEASURED
the thing I only warned about:** the graduating fixture **and** a 12-cell probe are **byte-identical to
POST-fix C and LLVM ⇒ MATCH.** ⇒ ⭐ **`non_match` does NOT grow; the ceiling is untouched; the real duty is
raising the MATCH FLOOR.** ⚠ ***An unresolved alarm stalls an executor as effectively as a wrong one.***
⊕ **And `MIN_FIXTURES` is `const_assert`ed ⇒ raising it alone breaks the TEST TARGET'S BUILD, not just a test.**
⛔ **My call-site count was 6; it is 13** — and **the comprehension surface is 5 of the 8 loop-hook sites with a
FIRE COUNT OF ZERO.** ⛔ **And `on error` has NO ORACLE** (ggdef rejects it outright), so *"correct PRE and
POST"* rests on **C/LLVM agreement alone — Core #8's exact trap** — while a mutation-inside-`on error` shape
**traps on both lanes at PRE and POST.**
⊕ **The design itself was re-measured end-to-end a SECOND time and holds: all 207 `cow_*` fixtures PRE vs POST
— 207 SAME, 0 CHANGED**, and the guard fixture POST is correct on **four lanes including self-host.**

### ⭐⭐ C2's DESIGN IS SIGNED OFF — AND PASS 6 FOUND ggdef IS A **LIVE ORACLE**, WHICH I DENIED FOUR TIMES

⛔ **I ASSERTED "ggdef ABSTAINS" IN THE BODY AND THREE ADDENDA, MEASURED AT THE WRONG LAYER.** I grepped
`Stmt::` in `spec/ggdef/src/eval.rs` — **which interprets the CORE IR, not the surface AST.** The surface `for`
is desugared one layer earlier (`grep -n "ast::Stmt::For" -A 2 spec/ggdef/src/elaborate/mod.rs`).
**Orchestrator-measured:**

| cell | ggdef | Rust gg HEAD |
|---|---|---|
| **bare** `for s in v: s = "zz"` | ⭐ **rc 0 → `aa` / `bb`** | **SIGABRT double free** |
| **`&`** form | *"`for &`/`for !` iteration is Increment B2"* | SIGABRT double free |

⭐⭐ **THIS MAKES THE TRACK BETTER, NOT LONGER — THREE CONSEQUENCES:**
1. **Core #9's escape clause does NOT apply to the bare cell** — there is no subset gap to file. **Ship the
   FOUR-LANE conformance pin Core #9 actually wants**, at the cost of five constant bumps in one commit
   (`MIN_FIXTURES` is an **EXACT** pin with `const_assert`s; `GGDEF_MATCH_FLOOR` lives in a **different file**).
   **Zero parity-corpus inflow.**
2. ⭐ **THE CORE #8 ARGUMENT IS UPGRADED FROM ITS WEAKEST FORM TO ITS STRONGEST.** Not *"all three lanes agree
   on the wrong answer, ships on the severity ladder"* — **that is true only of the `&` cell.** For the bare
   cell: ***the definitional interpreter prints `aa/bb`, and the fix makes Rust gg MATCH THE DEFINITION.***
3. ⛔ **I MARKED A TRUE CLAIM FALSE.** The body ⛔-flags `t0045`'s *"ggdef already prints the ratified answer
   while Rust gg SIGABRTs — a live Core #8 event"* as **decayed**. **It is TRUE for the bare form.** `t0045`
   was wrong only about the **CELL**.

⛔ **AND MY ENUMERATION MISSED A SECOND `#[ignore]`d TEST AND A SECOND FILED ITEM FOR THE SAME DEFECT** —
`sound_loop_string_elem_assign_double_free` and `todo/t0403`, **zero mentions across six layers.** It is row
(ii)'s self-append shape under `&`, **SIGABRT at HEAD**, and ⛔ **its un-ignore trigger LITERALLY FIRES under
the fix while its assertion would still FAIL** — the same trap flagged for `attack_99`, on a test nobody
enumerated. ⊕ **`t0403`'s own header carries an INDEPENDENT four-cell element-type table — a better
readiness-row-(2) witness than my five samples.**
⛔ **AND D24's CHECKLIST — BUILT TO STOP PRESCRIPTIONS BEING DROPPED — HAD ITSELF DROPPED THE MOST-WORKED ITEM
IN THE BRIEF:** no row carried the Core #8 per-cell disposition that four folds produced. ⚠ **The output-review
reads the DIFF, not the brief — so it must travel with the diff.**

⭐ **D23's NON-MONOTONICITY NOW HAS ITS MECHANISM, so nobody re-measures it:**
`is_borrow = !drops.is_registered(local) || … || is_cow_borrow(local)`. HEAD and the two-atom revert → not
registered → borrow → clone → green; the **subset** → registered **and** `Untracked` → no clone → rc 101; FULL
→ registered **but tagged** → clone → green. ⇒ ***a superset restores HEAD; the subset creates a config that
never existed.***

⇒ ⭐ **PASS 7 IS DELIBERATELY NARROW: the ONE new deliverable (the spectest + five constant bumps against exact
pins) and the checklist's completeness. Nothing else is in scope** — the design has been signed off and
re-reviewing it costs a pass for nothing.

### ⭐ C2's PASS 5 — THE FIRST FOLD OF MINE THIS ROUND THAT HELD, AND IT FOUND A DEEPER RULE

**D17 is TOTAL.** Pass 5 derived the revert atoms **from the patch text** — five of them — built a config nobody
had (**both drop registrations reverted while KEEPING both tags and the view**), and confirmed **every one of
the 31 non-empty partial reverts turns a prescribed row RED.** ⇒ **no fourth generation.**

⚠⚠ **BUT THE REVERT LATTICE IS NON-MONOTONE, AND THAT CHANGES HOW MY OWN RULE CAN EVER BE DISCHARGED.**
Measured: `{De, Te}` leaves **both** push-escape rows GREEN, while its own **SUBSET** `{Te}` turns one **RED** —
the Tier 2a refusal needs the drop-registration **present** to fire. ⇒ ⛔ **"ENUMERATE THE REVERTS" IS
DISCHARGEABLE **ATOM-BY-ATOM ONLY**. A future fold that verifies it by testing SUPERSETS will conclude a live
row is dead.**

⛔ **One genuine gap remained: the `cid` source-provenance axis is pinned at 1 of 3 cells.** `iter_source_coll`
is three-valued (`Local` · `FieldPath` · fallback), **and for a plain local the correct derivation and the
degenerate fallback COINCIDE** — so **no prescribed row could tell a correct `cid` from a hardcoded one.** Five
unpinned shapes measured rc 134 → rc 0. **Row (iv) added.**
⊕ ⭐ **And the good news buried in it: THE TWO WRITE SITES *ARE* THE TOTAL CLASS** — the `is_string_type`
carve-outs exist at exactly two sites, the dict/set/iterable lowerers have none, and those shapes are rc 0 at
HEAD. **Core #4 satisfied.**

⭐ **AND A STRUCTURAL DEFENCE AGAINST MY OWN FOLD PROBLEM: `ADDENDUM 5` NOW CARRIES A 14-ROW CONSOLIDATED
DELIVERABLES CHECKLIST.** *"Five precedence layers with two retractions is itself the hazard — four folds each
dropped a prescription."* **A flat checklist at the end is the cheapest defence left.**

### ⚠ F1r's PASS 1 — SIX BLOCKING, ALL BRIEF-COMPLETENESS, AND ONE IS THE SPAWN-SNAPSHOT HAZARD IN REVERSE

**The design was re-measured end-to-end and holds** — including pass 1 building its **own half-S by line** to
confirm the halves do not interact, and measuring **Cases 4/6 correct pre-fix on six shapes**, which vindicates
the NARROW predicate.
⛔ **I DECLARED A BASE THAT WAS TOO OLD.** At `d0ab310df`, `t1362` carries **two** corrections; the third landed
in `7e111d2d9`. **An executor honouring my declared base would find the SECOND correction standing — the exact
framing the rebuild withdrew.** ⚠ **The spawn-snapshot hazard cuts both ways: file first, THEN brief, and state
a base NO OLDER than the artifacts you cite.**
⛔ **My placement instruction named the WRONG CORPUS:** `known_gaps/` is under `tests/fixtures/`, read
**non-recursively**, so **graduating `t0750` ADDS A ROW TO THE RUNTIME-DIFF DENOMINATOR — the zero-slack gate**,
and the brief was silent on it. ⊕ **And the constructs the guard must add are OUTSIDE ggdef's subset, so one
spectest cannot carry them ⇒ TWO fixtures.**
⛔ **My count lint PROVABLY CANNOT CATCH the `on error` trap it was paired with** — switching to
`lower_block_scoped` does not change the `.save_locals(` count, because **that site is INSIDE it.** SIX-Q #2,
again. **Replacement: pin `lower_block_scoped(` CALL sites at 2.**
⛔ **And my own prose would have deleted two cases:** I wrote *"the narrowed predicate (`cow_aliases_of ||
cow_collection_refs_for` **only**)"* — **the real predicate also carries `self_is_borrow(…)`, i.e. Cases 1/1b.**

### ⛔⛔ TRACK G's LANDING PROCEDURE WAS UNEXECUTABLE — ~210 CELLS WOULD HAVE SHIPPED **UNGATED**

**Two of my own directives were in direct conflict and I did not notice:** the body says *"baseline at the
MEASURED bucket"*; my D2 says *"the generator owns `topic`/`cell`/`expected`/`note` ONLY"*. **Composed, nothing
can seed a non-good baseline.**

**Verified in source:** `--accept` writes a lane column **only when `bucket == good`**, and `--accept-drift`
needs `drifted`, **which is set inside `if base:`** ⇒ **an EMPTY baseline can NEVER receive a non-good bucket.**
Pass 2 proved it with a probe row: after `--accept --accept-drift` the column is **still empty.**

⛔ **CONSEQUENCE: the ~210 `WRONG` cells sit at `base == ""` FOREVER, so the regression branch NEVER RUNS on
them — `WRONG → CRASH` is INVISIBLE.** ⭐ **That is precisely the transition `D52` predicts** (*"a bug there is
a UAF, not a wrong answer"*) ⇒ **the track's own central argument for the ASan lane was neutralised by the
seeding hole. SIX-Q #2: the guard could not catch its own class on 30% of its corpus.**
⊕ **And `--accept` is ALL-OR-NOTHING:** any self-host disagreement reds the map and **refuses to write a single
row** — and **`t0750` records the self-host is CORRECT on this family while C/LLVM are wrong**, so that is the
**EXPECTED** outcome on ~210 cells, not a tail risk.
⭐ **DECIDED — THE CLASS FIX, NOT THE WORKAROUND:** extend the accept branch so an empty baseline **records its
measured bucket** (one condition; the file already carries the *"a ratchet needs both directions"* reasoning),
seed all five lanes + `divergence` in the same commit, and have precondition 1 assert **no row is measured on a
lane where its baseline is empty.**

⛔ **AND MY D1 MITIGATION DOES NOT EXIST.** `--topic` is **INCLUSION-ONLY** (`startswith`) — there is no way to
exclude one. **And the five-lane step is ALSO PER-PR** (same `test` job; the workflow triggers on `push` **and**
`pull_request`) ⇒ **the map runs TWICE per PR and BOTH steps grow ~67%.**
⭐ **DECIDED — ACCEPT THE COST AND WRITE IT INTO THE ROUND ENTRY.** Excluding the topic from CI **defeats the
argument that justified the design**: a topic CI does not run is a script nobody runs — **2026-07-06 again.**

⛔ **AND MY D5 DISCLOSURE WAS ITSELF A SELECTION — SIX-Q #3 APPLIED TO THE DISCLOSURE.** The generator always
mutates the **SOURCE** and reads the **VIEW**; the mirror — **mutate the VIEW, read the SOURCE** — is
**UNPROBED for 7 of 9 sources**, and `t0750` names **both directions as one defect**. ⭐ **The proof was in my
own errata: `misc_vector_alias_copy.gg` is the MIRROR shape, not the duplicate I called it.** ⇒ **DIRECTION is
a FOURTH AXIS with no witness, no coverage and no name.**
⊕ **And one of my two "durable homes" does not exist** — `MANIFEST.tsv` is a flat TSV; a `#` header row would
be silently dropped and **break precondition 1's set-equality.** ⇒ **the homes are the GENERATOR DOCSTRING, the
per-row `note` COLUMN, and a `todo/` ITEM.**
⭐ **STRENGTHENER TAKEN, better than all three: PUT THE SCOPE LIMIT IN THE TOPIC STRING** — it is column 1 of
every row and prints on every report line.

⊕ **E5 WAS ITSELF A WRONG-INSTRUMENT READING, INSIDE THE ADDENDUM THAT EXISTED TO CORRECT ONE:** *"90 of 99
fail to build; 9 build"* read `c_verdict`, not build status. **All 99 fail to build on both value lanes**; the
"9" are labelled ggdef-abstain because that branch is tested **before** the build status.

### ⭐⭐ TWO FINDINGS THIS HEARTBEAT THAT OUTLIVE THEIR TRACKS

**1. A BRIEF THAT NAMES ITS OWN OMISSIONS IS NOT ENOUGH, BECAUSE BRIEFS ARE `/tmp`-ONLY AND PRUNED AT ROUND
CLOSE.** Track G's brief is honest that its SOURCE axis is a **selection** and names four unprobed kinds. Pass 1:
*"the honesty is sufficient AS REASONING and insufficient AS AN ARTIFACT."* **What survives the round is a
693-cell topic that LOOKS like a total enumeration — and the next round concludes the family is closed.**
⛔ ***"That is the 2026-07-06 failure in a new dress: not 'nothing obliged a re-run', but 'NOTHING RECORDED WHAT
WAS NEVER RUN.'"***
⇒ ⭐ **RULE: a named omission must land somewhere DURABLE — the generator docstring, the manifest note column, a
`todo/` item — never only in the brief that named it.** ⚠ **This generalises past Track G: every "named
omission" this round lives in a `/tmp` brief.**

**2. C2's FIXTURE-SET DEFECT HAS A GENERAL FORM I KEPT MISSING.** Pass 3 found the set greens the view-only
revert; my D16 **pinned that one instance**; pass 4 found **two more partial reverts still fully green**
(`lower_for_enumerate` hunks only → all green; both `set_collection_ref` calls → all green, ASan-clean).
⇒ ⭐ **RULE: a fixture set is complete only when EVERY PARTIAL REVERT of the patch turns at least one row RED.
ENUMERATE THE REVERTS; do not pin the one you happened to find.** ⊕ **And state, per row, WHICH revert it
pins** — so the next fold cannot silently drop one.
⊕ **SIX-Q #2 fired a THIRD time inside my own prescription:** I wired the leak-detecting row through
`security_safe`, which routes to `ASAN_OPTS_NO_LEAK` (`detect_leaks=0`) — **the row is INERT.** The tree already
has `security_safe_no_leak`. ⚠ ***"`--sanitize` alone is not the instrument."***

⊕ **AND TWO OF MY OWN DECISIONS CONTRADICTED EACH OTHER ON C2:** `security/` costs **zero inflow BECAUSE the
parity corpus is non-recursive** — which is **exactly why a `security/` fixture is INVISIBLE to the self-host
lane**, so my Core #9 "cross-lane pin" could never have pinned anything. **Resolved: one BARE-FORM fixture goes
TOP LEVEL** (measured MATCH, zero non-MATCH inflow); memory-safety rows and the `&` cell stay in `security/`.
⛔ **Never a top-level `&`-form fixture — its expected output would ENSHRINE THE LOST WRITE.**

⭐ **C2's Core #8 GATE IS NOW CLEARED BY MEASUREMENT, not by a severity argument:** the `&` lost write is
**pre-existing and universal** (`Vector[int]` + `for i in &v: i = i + 10` → rc 0, prints `1 2` at HEAD), so the
fix **moves the String cell OUT of memory-unsafety INTO an already-filed class**; and **String was the LAST
broken cell of ratified consequence (a)** — nested Vector, struct and dict-value already bare-rebind as
non-crashing private copies. ⇒ **the fix COMPLETES the ratified rule.**

### ✅✅ C2 AND F1r ARE INTEGRATED — AND THE FORECAST COLLISION FIRED EXACTLY AS PREDICTED

**6 commits on the integration branch.** Three conflicts, **all three in RATIONALE COMMENTS, none in a value** —
`DONE.md`, `tests/spec_conformance.rs`, `spec/ggdef/tests/spec_conformance_ggdef.rs`. ⭐ **Both narratives were
TRUE, so both were KEPT** — each documents a different seed, and deleting either would have left the surviving
comment describing a corpus that no longer exists.

⭐⭐ **THE COLLISION FIRED, MEASURED NOT PREDICTED:** after both stacks landed,
`ls spectests/run/*.gg | wc -l` → **245** against a `MIN_FIXTURES` of **244**, which is an **EXACT PIN**.
⇒ **`spec_conformance` would have RED on ARITHMETIC**, and a parent who had not forecast it would have read a
green patch as a regression. ⛔ **Neither track could have seen it: each measured base + its own fixture.**

**The parent's constant pass — all PER-FIXTURE, so each moved TWICE:** `MIN_FIXTURES` · `C_MATCH_FLOOR` ·
`LLVM_MATCH_FLOOR` 243→**245**; `SELFHOST_MATCH_FLOOR` 242→**244**; `GGDEF_MATCH_FLOOR` 225→**227**;
`GGDEF_SKIP_CEILING` **UNMOVED** (both seeds carry `adjudicator: ggdef` and both measured SKIP=18).
⊕ **Plus the four `figures.db` mirrors** — `parity.spec_conformance.{c,llvm,selfhost}.match_floor` and
`spec_conformance.corpus.fixtures` — which `figures_db_mirrors_agree` enforces.
⛔ **THESE ARE THE ARITHMETIC EXPECTATION AND THEY ARE BEING RE-MEASURED, NOT TRUSTED (Core #5). The gate's
printed counts win over my addition.**

### ⛔ F1r's EXECUTOR CAUGHT **MY** MIRROR MAP SHORT BY FOUR — AND THE MISS HAD A SHARPER CAVEAT THAN THE HIT

I handed it seven constant↔mirror pairs. `grep -n "\.home = " scripts/figures.db` also returns the
**`clone.stage1.*` QUARTET** — `STAGE1_{ARRAY,STRING}_CLONE_{PIN,ROUND_OPEN}`. ⇒ **they belong in the same
deferral**, and they carry a caveat the stage-0 pair does not:
⭐⭐ ***THE TRACK'S CLONE EVIDENCE IS STAGE-0 ONLY. Stage 1 pushes a much larger program through the changed
lowering, so a stage-0 result is NO EVIDENCE AT ALL about stage 1.*** ⛔ **I must not move a stage-1 pin on
stage-0 evidence at round close.** `t1365` now says so.
⊕ **And the shape recurses:** I told the track *"the landing boundary of a constant is not the constant"*; the
executor answered that **the DEFERRAL RECORD has the same landing boundary as the constant** — the same failure,
one level up, found in my own correction.

⭐ **A THIRD-ORDER FINDING WORTH KEEPING:** the executor reports **erratum 1 is the THIRD mechanism-claim on this
track that survived review by being PLAUSIBLE rather than MEASURED** (the other two: `t1362`'s original filed
double-free, and the *"gate 2 is not a root cause"* correction). ⭐ **All three were STRUCTURAL claims about
WHICH CODE PATH a case takes.** ⇒ its fix is the cheapest possible guard: the predicate's docstring now
**distinguishes ESTABLISHED-MECHANISM exclusions from MEASURED-ONLY ones**, and Case 6 is explicitly marked
*"measured correct, reason not established"* with a warning that the structural argument covering Case 4 does
**not** cover it. **No mechanism invented.**

### 🚀 TRACK K OPENED — `t1385`, THE LAST BLOCKER TO A GREEN ROUND-CLOSE BATTERY

⛔ **`--lanes all` is RED on exactly two causes: `t1387` (H's executor is fixing it now) and `t1385`.** **A red
battery is NEVER waivable** ⇒ **R50 cannot close until both are discharged.**

⛔ **AND THE ITEM'S OWN SCOPE FIGURE IS WRONG BY TWO.** Regenerated
(`awk -F'\t' 'NR>1 && ($8=="" || $10=="" || $11=="")' tests/fixtures/robustness_map/MANIFEST.tsv | wc -l`) →
**33, not 31**: `00 harness controls` 1 · `07` 5 · `10` 1 · **`11` 21** · `13` 4 · `30` 1.
⇒ **the scout establishes whether the topic-00 and topic-30 rows are DELIBERATE CONTROLS or two more
unbaselined rows, and corrects the item either way.**

⭐⭐ **THE QUESTION THAT COULD CHANGE THE WHOLE TRIAGE, HANDED TO THE SCOUT FIRST — AND IT COMES FROM TRACK H:**
pass 3 measured that **topic 30's `asan` column is a BYTE-FOR-BYTE COPY of its C-lane column** (819/820 rows
identical) ⇒ **those baselines were NEVER MEASURED ON THAT LANE.** ⛔ **If the same holds for the legacy rows,
then *"seeding a measured verdict"* and *"seeding a COPIED one"* are different acts and the triage changes
shape.** *A `WORKS` row whose column was copied from another lane is SIX-Q #6 in its purest form.*

⚠ **AND THE 13 IS A LOWER BOUND:** G's executor censused **topic 11 only** (21 rows: 13 `SANITIZE-FAIL` ·
6 `BUILD-FAIL` · 1 `REJECTED` · 1 `WORKS`); **topics 07/10/13 were never censused.**
⚠ **COLLISION WARNING BRIEFED: H is editing the map runtime and will retire 8 leak-allowlist rows this round.**
**If K also moves allowlist rows or manifest columns it must say so** — *this round has already been bitten by
two tracks colliding on shared counters.*
⊕ **The scout is told that if a full triage cannot land this round, it must SAY SO and propose the smallest
honest alternative (the sanctioned `LEGACY_UNBASELINED` mechanism — DECLARED, not hidden), and that editing an
expectation to match what the compiler prints is forbidden by the map's own header.**

⊕ **DISK: 22G → 5.1G.** All completed agent worktrees pruned; only the two live agents remain. ⊕ **The two
shared-`pgrep` watchdogs pass 3 flagged are GONE — they died with their agents.**

### ✅✅✅ H IS SIGNED OFF 3/3 — EXECUTOR LAUNCHED. **AND PASS 3 FOUND THE THIRD SELECTION.**

⭐ **THE SET-DIFF IS FIVE, SETTLED BY A THIRD MEASUREMENT** — and this time with the decisive control: **the 227
`vsm_*` rows and the 5 DRIFT rows are BYTE-IDENTICAL across the pristine and fixed runs.** *Provably not the
fix's.* **Manifest `|changed|` = 6; TOTAL `|changed|` = 26 across two instruments.**
⚠ **And the instrument's own limit, stated: `--lanes asan` is the right PRIMARY lane but NOT TOTAL — 133 of
1856 cells (7.2%) never reach a run verdict** (REJECTED 100 · BUILD-FAIL 28 · ICE 3 · TIMEOUT 1 · UNKNOWN 1).

⭐⭐⭐ **E-f — THE THIRD SELECTION, AND IT IS THE ONE THAT MATTERED: THE ENTIRE EVIDENCE BASE WAS
STRING-KEYED.** `key_drop` is **not** only `NULL | gorget_string_free` — a generated **`<T>__drop`** is stored
for any **non-str-keyed** Dict/HashMap/Set/HashSet with a droppable key. **The 5 cells, the 15 A/B programs and
`dict_update` are ALL String-keyed** — *one value of a typed axis (Core #12).*
⭐ **PASS 3 BUILT THE MISSING CELL RATHER THAN FILING THE GAP:** `Dict[Named,int]` and `Set[Named]` →
**pristine LEAK 33 B / 35 B, fixed rc 0 clean, caller's local INTACT.**
⇒ ⭐⭐ ***THAT IS THE ONE WAY THIS COULD HAVE BEEN A MEMORY-UNSAFETY CHANGE — an arbitrary generated drop on the
incoming key — AND IT IS MEASURED SAFE. It is why pass 3 SIGNS OFF rather than blocks.*** ⊕ **And no committed
fixture pins that half: the executor ships one this round.**
⚠ **A measured executor constraint that would have looked like a defect: `@derive(Hashable)` emits `h * 31` on
`int64` and trips UBSan SIGNED OVERFLOW on long keys** ⇒ a long-key cell would be SANITIZE-FAIL **for an
unrelated reason.** ⊕ **That overflow is UNFILED — the executor files it.**

⛔ **THE FOLD REMEDY WAS NOT EXECUTABLE AS I WROTE IT.** *"Fold only the named cells"* — **there is no per-cell
flag.** And **no single `--topic` folds exactly the five; they span two.** ⇒ **two scoped runs, and the second
also folds `pair_upper_d_map_named`, which is NOT the fix's — DISCLOSE IT.**
⭐⭐ **AND B2 IS STRONGER THAN STATED: topic 30's `asan` column is a BYTE-FOR-BYTE COPY OF THE C-LANE COLUMN**
(819/820 identical) ⇒ **those baselines were NEVER MEASURED ON THE ASan LANE**, and a bare `--accept` would
write **227 unadjudicated verdicts into a column populated by copying a different lane.** *Pass 3 declined to
invent a mechanism for why they read WORKS. Correct.*

⛔ **THE COST: PASS 2 UNDERSTATED IT AND MY CITATION WAS THE WRONG LANE.** Noise floor at `-O0` is
**stdev/median 10.5% (hit) and 15.9% (miss)**, min→max **47%/65%** ⇒ ***the ±1–2% "regressions" are AN ORDER OF
MAGNITUDE INSIDE NOISE.*** ⊕ **`let user_opt = if release` is inside the LLVM `llc` pipeline — THE DEFAULT C
BUILD PASSES NO `-O` FLAG AT ALL.** Conclusion unaffected; the command was wrong.
⊕ **`dict_update` is NOT a map cell** — it is an allowlist row + a fixture, **and because the row is UNCITED its
retirement is ADVISORY (rc 0)**; it becomes a RED-going pin **only after the row is DELETED.** ⊕ **The
figures-db value prohibition binds ONE of the four constants, not all four** (only `uncited_class_pairs` has
`scan = standard`). ⊕ **Retirement is FATAL only for CITED rows — 15 of 301.**

### ⛔⛔⛔ C1 IS BLOCKED A **FOURTH** TIME — AND THE SPINE SURVIVES EVERY TIME. **`R` NEEDS ITS OWN SCOUT.**

⭐ **THE PATTERN IS THE FINDING.** v2 blocked on DESIGN · v3 on ARITHMETIC (two changes named, three needed) ·
v4 on SCOPE (position axis) · **v5 on the POSITION AXIS AGAIN, plus two false premises.** ***Every block is
`R`'s SUBJECT; the spine — `U`+`C`+`R`, one commit, `R` with or before `C` — survives each pass and measures
STRONGER.*** ⇒ ⭐ **`R`'s subject is a RESEARCH problem (which ARMS at which POSITIONS without over-refusing),
and nobody has measured the ARM × POSITION matrix. That is a SCOUT deliverable, not a brief deliverable.**
⇒ ⚖ **RECOMMEND: CLOSE C1 FOR R50 alongside D0′. It has produced THREE new CRITICAL/HIGH filings — more value
than most tracks that shipped.**

🚨 **B1 — THE THIRD UNENFORCED POSITION IS *CLOSURE CAPTURES*, AND IT IS SILENT WRONG OUTPUT. FILED `t1331`.**
`Callable[void()] f = (): print((*b))` over a `Box[String]` prints **`0`** while **the identical deref outside
the closure prints correctly**, `gg check` rc 0, ASan **CLEAN**. ⊕ **Negative control: a plain `String` capture
prints correctly twice** ⇒ **the discriminator is the SINGLE-OWNER TYPE at the boundary, not closures.**
⛔⛔ **AND IT IS EXCLUDED FROM `t1067` BY CONSTRUCTION, not by judgement:** that item's set is
`FnPtr ∪ Mutex ∪ RWLock`, and **`Box__X` is minted `CopySemantics::Resource` at all four sites**, so
`lacks_materialization_path(Box)` is **false**. *The gate that legitimately parks that class cannot park this.*
⇒ ⭐⭐ **v5's OWN NOMINATED WITNESS YIELDED *TWO* OMITTED POSITIONS — returns AND captures — AND I TOOK ONE.**

⛔ **B2 — `R` AT THE RETURN POSITION WOULD RED A COMMITTED GREEN TOP-LEVEL FIXTURE.** The helper's
whole-`Expr::Identifier` arm rejects **unconditionally, liveness-independent** (measured: a DEAD local still
rc 1). ⇒ wiring it at returns rejects `return f` — **and
`tests/fixtures/closure_escape_capture_axis_param_named.gg` is committed, green, top-level, non-`#[ignore]`d,
and its header asserts *"INTENDED: prints `hello`, rc 0, both backends"*.** ⇒ **the brief must name WHICH ARMS
run at the return position — the design consistent with `t1329` is the SUB-PLACE arms ONLY, never the
whole-Identifier arm.** ⚠ ***My brief warned that "no half may ship an un-refusal" and was blind to the
OVER-refusal in the other direction.***

⛔ **B3 — THE METHOD-CALL PREMISE IS FALSE, IN THE SAME SHAPE AS THE ERRATUM I RETRACT ONE SECTION LATER.**
`require_explicit_move_for_single_owner_init(` has **SEVEN** call sites, and **TWO are inside the
`Expr::MethodCall` arm** ⇒ **the arm ALREADY CALLS THE HELPER TWICE.** `Box.new` needs **one more GATE**, not a
missing caller. **KEEP the conclusion, DELETE the reason** — acting on it either routes every method call into
the ctor path or duplicates a call beside an existing one (Core #4 drift).
⊕ **AND A FRAMING SLIP THAT LET B1 THROUGH: `Box.new` is NOT a third POSITION — it is the CONSTRUCTOR position
under a SECOND SPELLING.** `R`'s real position count is **two** (constructor, return); **the boundary set's
third unenforced row is CAPTURES.**

⭐ **VERIFIED, AND ONE LEG MEASURES STRONGER THAN I CLAIMED:** `t1329` and `t1330` **both reproduce exactly** ·
the `clone_fn_for_ptr` correction is **load-bearing, not cosmetic** (`lower_return` uses it at three sites and
never touches `ptr_materialization_kind`) · ⚠ **BUT all three gate on `place.projections.is_empty()`, so
`return h.b` — WITH a projection — does NOT reach them: `C` alone does NOT make `t1329`'s cell clone** ·
and **leg 1 of "no clean split" is stronger than stated: with the box OBSERVED, the `Box.new` nested shape at
HEAD is rc 0, correct output, ASan CLEAN** ⇒ ***`U` alone converts a CLEAN, CORRECT program into a compiler
panic.***
⊕ **E5 — my *"ONLY ggdef abstains"* is OVER-BROAD:** `Ty` has a **`Callable` variant with `param_ownerships`**,
so `R`'s `Callable`-payload cells **ARE ggdef-adjudicable.** *Do not let the `t1227` cite excuse the whole lane.*

### ⭐⭐⭐ H PASS 2 RAN THE CONTROL PASS 1 NEVER RAN — AND THE FIGURE CAME BACK TO WHERE IT STARTED

⭐⭐ **PASS 1 RAN ONLY THE POST-FIX LANE AND CREDITED EVERY `PROGRESS` ROW TO THE FIX.** Pass 2 ran the
**pristine control**: fixed **7 PROGRESS / 0 REGRESSION**; pristine **2 PROGRESS / 1 REGRESSION**.
**Set-diff: FIVE attributable — THE BODY'S ORIGINAL FIVE — and TWO green on pristine HEAD.**
⇒ ⭐⭐⭐ ***AN ATTRIBUTION WAS MADE, CORRECTED, AND CORRECTED BACK: pass 1's charge that "five is a selection"
WAS ITSELF THE SELECTION. A "PROGRESS" FIGURE WITH NO CONTROL IS NOT A MEASUREMENT.***
⊕ The 2 stale-baseline cells are a **separate** finding (same under-reporting family). ⊕ **The population figure
87 IS sound — COLUMN-audited, `SANITIZE-FAIL` absent from every other column, so the line-grep was not
accidentally correct (SIX-Q #6).** ⛔ **Readiness row 3 has now been stated as 20, 8, 7 and 6 — pass 3 must
SETTLE it, and ask whether `--lanes asan` is even the right instrument.**

⛔⛔ **A CROSS-TRACK INTERACTION I CREATED BY INTEGRATING G, AND NEITHER THE BODY NOR PASS 1 SAW IT:** the same
command now reports **234 `PROGRESS` rows — 227 of them `WRONG → WORKS` on `vsm_*`, TRACK G's corpus.**
**`--accept` refuses ONLY on regressions; with zero regressions a bare `--accept` WRITES ALL 234 PLUS 5 DRIFTS
INTO H's COMMIT**, silently ratcheting another track's baseline. ⇒ **the fold must be SCOPED.**

⛔ **AND AN ID COLLISION THAT IS MINE:** I issued H the block `t1408`–`t1416` **and then filed `t1408` myself**
for the D0′ registration defect — **the exact MA-3b failure that once "forced a renumber and left `t0946`
permanently unused".** ⇒ **re-issued `t1409`–`t1417`; J gets `t1418`–`t1427`.**

⛔ **THE COST TABLE DOES NOT SURVIVE RE-MEASUREMENT.** 40 **interleaved** reps: **only the `-O2` hit-path win
(−15.6%) is real; every `-O0` cell is within ±3% noise and FLIPS SIGN across runs**; pass 1's `miss -O2 −4.7%`
measures **~0%**. ⊕ **`always_inline` recovers NOTHING and would leave an `always_inline` function with EXTERNAL
linkage under the LLVM path's `.replace("static inline ", "")`.** ⇒ **do not add it.**

⭐ **WHAT PASS 2 ADDED THAT PASS 1 COULD NOT: the LLVM lane MEASURED END-TO-END** (pristine `rc 1`,
`Direct leak of 4 byte(s)`; fixed `rc 0`, zero leaks) ⇒ **no longer an inherited argument** — **and an EXACT
fire count: `total_frees` 60 → 200034, `live_bytes` 399948 → 0, `string_clone` IDENTICAL ⇒ 200034 − 60 =
199,974 = 200,000 − 26 duplicate puts. ONE FREE PER DUPLICATE PUT, EXACTLY.**

### ⛔⛔ C1 v4 PASS 1 — SPINE CONFIRMED, **SCOPE WRONG**, AND IT FOUND TWO CRITICALS AT PRISTINE HEAD

⭐ **THE SPINE IS NOW MEASURED, NOT ARGUED:** `U` alone takes `t0011` rc 134 → **rc 0 ASan-CLEAN** (`--lib`
1187/0) · `R` **cannot** touch `t0011` (payload is a `String`) · `C` alone **un-refuses** row 4 · the four
`t0682` rows are **exactly** as v4 stated · row 3's fix-it **ICEs** · the self-host mirrors the carve-out
**including `Box`** · ggdef abstains.

⛔ **BUT `R`'s SUBJECT WAS A SELECTION ON THE POSITION AXIS.** v4 derived it from `t0682`'s four historical rows.
**Derived instead from `AGENTS.md`'s RATIFIED BOUNDARY SET, it is three positions:**
1. **constructor** (rows 2/4) · 2. ⛔ **the METHOD-CALL spelling** — `Box.new(…)` is an `Expr::MethodCall`, and
`require_explicit_move_for_single_owner_init` runs **only** from `Expr::Call` / `Expr::DotShorthand`
⇒ ***under `U` the BOOK'S OWN SPELLING becomes a compiler PANIC with `gg check` rc 0*** — **a second "POSITION
WITH NO SUBJECT"** · 3. 🚨 **RETURNS.**

🚨🚨 **TWO CRITICALS FILED, BOTH MEASURED AT PRISTINE HEAD, BOTH FOUND BY A *BRIEF REVIEW*:**
- **`t1329`** — `Box[String] take(H &h): return h.b` → **`gg check` rc 0, then ASan HEAP-USE-AFTER-FREE.**
  ⭐ **The same sub-place read is REJECTED at bare-assign, container literal, `push`, `Some(…)` and `d.put(…)`.
  RETURN IS THE ONE THAT GETS THROUGH, AND IT IS THE MEMORY-UNSAFE ONE.** ⭐⭐ **And the INDEPENDENT WITNESS was
  a contradiction already in the tree: `AGENTS.md`'s boundary set NAMES returns while `needs_explicit_move`'s
  own doc-comment OMITS them.** *Documented on one side, invisible on the other.*
- **`t1330`** — a struct clone of a `Box[String]` field **DOUBLE-FREES at pristine HEAD.** ⛔ **I had written
  *"build one, OR file it as unmeasured-by-reading"*; the reviewer BUILT it in ONE COMMAND, and it is a
  double-free rather than the "shared buffer" I described.** ⇒ ***the disjunction would have licensed an
  UNMEASURED PREMISE into `todo/`. That branch is struck.***

⛔⛔ **AND MY MANDATED ENUMERATION NAMED THE WRONG ACCESSOR.** `C` acts through **`clone_fn_for_ptr` (38 sites)**,
of which `ptr_materialization_kind` (4) is **one consumer** — plus `clone_fn_name_for_def`, feeding the IR
validator, **which no walk of `ptr_materialization_kind` reaches.** ⭐ **PROOF IT MATTERS: `t1329`'s return cell
reaches `C` through a plain `clone_fn_for_ptr` site — AN EXECUTOR OBEYING v4 LITERALLY WOULD NOT HAVE FOUND THE
UAF.** ⇒ ***SIX-Q #2 applied to the INSTRUCTION rather than the guard.***
⊕ **And readiness row 2's nominated witness DISCLAIMS ITSELF** — the registration-count lint's own doc says it is
*"bookkeeping, not a class-retiring guard"* and *"says nothing about whether the four agree on the OTHER
metadata fields"*. ⭐ **The reference-grade fix is one function away: route the four Box sites through the SINGLE
WRITER, as `ensure_shared_type_def` already does — disagreement becomes IMPOSSIBLE BY CONSTRUCTION, and it
dissolves the "exclusion list is a SELECTION" leg by removing the need for a list.**
⊕ ⛔ **MY LLVM REASON WAS FALSE:** `generate_llvm_wrappers` **IS** `generate_c_inner_impl(…, wrappers_only=true)`
and calls the same wrapper emitter ⇒ **the c_lir emitter IS the LLVM lane's.** **Keep *"measure LLVM"*; DELETE
the reason — acting on it adds a SECOND emitter and the duplicate-symbol link collision the code documents.**

⭐ **`v5` DERIVES `R` FROM THE BOUNDARY SET AND STATES THAT NO CLEAN SPLIT EXISTS — checked, not assumed:**
`U` alone panics, `C` alone un-refuses, `R` without returns leaves a CRITICAL open. ⇒ **one commit, `R` with or
before `C`.**

### ⭐ C1 `v4` — THE THREE CHANGES ARE NAMED, AND THE ORDERING IS THE SAFETY ARGUMENT

**`U`** = the `Box.new` → ctor **unification** (`methods.rs`) — ⭐ **the ONLY change that closes `t0011`**, whose
repro payload is a **`String`, not a carve-out**, so the reject structurally cannot touch it.
**`C`** = `Box[T]`'s **`clone_fn`** at the four lint-pinned `is_box: true` sites.
**`R`** = `t0682`'s **check-time reject** in `src/semantic/safety/`.
⛔ **v3 wrote *"half"* to mean sometimes `{U}` and sometimes `{C,R}` ⇒ the executor got a `half × program`
matrix WITH ITS OWN AXIS UNDEFINED.** **v4 uses `U`/`C`/`R` throughout and forbids the word "half".**

⛔⛔ **THE ORDERING IS NOW MANDATORY AND MEASURED: `R` LANDS WITH OR BEFORE `C`, IN ONE COMMIT.** `C` alone takes
`t0682`'s row 4 from **rc 101 (the loud lower-or-reject stopgap)** to **`Built:` + a 32 B leak**, because
registering `clone_fn` flips `ptr_materialization_kind` — *"the ONE policy"* — from `PassThrough` to `Clone`
**everywhere.** ⇒ **that is v2's blocked defect re-entering through ORDERING.** ⊕ **And the brief now demands the
thing v3 skipped: an ENUMERATION of every position that policy reaches, with `R`'s coverage against it and an
INDEPENDENT witness** — *v3's coverage set was a selection with none.*

⭐⭐ **THE DEMONSTRATION IS NOW MEASURED AND SHIPPED, NOT HYPOTHETICAL:** `t0682` **row 3 is ALREADY REJECTED at
HEAD**, its diagnostic says *"copy the sub-place with `h.b.clone()`"*, **and writing that fix-it ICEs** — while
**under `C` alone the ICE becomes `Built:`.** ⇒ ***the coupling is visible today, on a shipped diagnostic, with
a before/after.*** ⊕ **The Tier-2a validator is a THIRD code region v3 never named.**

⊕ **`t0682`'s ROW TABLE HAS MOVED SINCE FILING** — row 3 is now REJECTED (R49 Track S-a2) ⇒ **`R`'s work is rows
2 and 4, BOTH CONSTRUCTOR positions**, and **`t0682` closes only if row 3's fix-it ICE closes too.**
⊕ **The write site is documented IN-SOURCE and I no longer send anyone hunting:** `is_constructor` matches
`Variant | Newtype` only, with the comment ***"A POSITION WITH NO SUBJECT … stays open under `todo/t0682`"***
— **SIX-Q #4, already written down by someone else.**
⊕ **LANES CORRECTED: LLVM is WORK, not a measurement** (no `Box__<inner>__clone` counterpart in that backend),
**and the self-host is NOT excused by `t1311`** — that is a **C-EMIT** defect while a check-time rejection never
reaches C-emit, **and the self-host already mirrors the carve-out INCLUDING `Box`.** ⇒ **three lanes pin the
final state; only ggdef abstains (cite `t1227`, do not re-file).**
⊕ **The 73 B residual is NOT `t1309`** — the allocating frame is **the fix's own new clone**, and `C`-alone leaks
**32 B where HEAD REFUSES TO BUILD AT ALL** ⇒ *definitionally new*, **the fix's own unregistered births
(Core #3).**

### ⛔⛔ C1 v3 PASS 1 — **THE COUPLING IS REAL, BUT MY DESIGN NAMES TWO CHANGES AND THE TRACK NEEDS THREE**

⛔ **B1 — *"HALF"* DENOTES THREE DIFFERENT CHANGES.** The design says *"ship BOTH halves"* = `clone_fn` + reject.
The fold says *"**C1** alone takes `t0011`'s repro to rc 0"* and orders a matrix over *"half × program"*.
⭐ **BUT `C1` IS NEITHER HALF — it is a THIRD change** (the `Box.new` → ctor unification, `methods.rs` only).
⇒ **the executor gets the round's central obligation with its own AXIS UNDEFINED, pointed at a patch containing
a change the design never names.**

⛔⛔ **B2 — THE CHANGE THAT CLOSES `t0011` IS NOT IN THE DESIGN, YET THE BRIEF PROMISES `t0011` GRADUATES.**
Measured, pristine HEAD: **C1-only → `t0011`'s repro rc 0, ASan-CLEAN**; **clone_fn-only → ASan
`attempting double-free`.** ⭐ **And the reason is structural: `t0011`'s repro payload is a `String` — NOT a
carve-out — so `needs_explicit_move` never applies and THE REJECT CANNOT TOUCH IT.** ⇒ ***as written the brief
CANNOT DELIVER ITS OWN HEADLINE ITEM.*** **The design is three changes: the unification (closes `t0011`) +
`clone_fn` + the reject.**

⛔⛔⛔ **B3 — `clone_fn` LANDED ALONE SILENTLY UN-REFUSES THE PROGRAM `t0682` RULES MUST BE REJECTED.** Measured:
HEAD **rc 101** with the loud *"lower-or-reject (Core #10) … stopgap for `todo/t0682`"*; **clone_fn-only →
`Built:`, runs, leaks 32 B.** **Mechanism:** setting `clone_fn` at all four `is_box: true` sites makes
`clone_fn_for_ptr` return `Some`, which flips `ptr_materialization_kind` — *"the ONE policy"*, 4 call sites —
from `PassThrough` to **`Clone` EVERYWHERE.**
⇒ ⭐⭐ ***THIS IS THE DEFECT v2 WAS BLOCKED FOR, RE-ENTERING THROUGH ORDERING.*** **v3 states NO ordering
constraint, NO one-commit requirement, and NO guard** — while Core #14 says that comment needs one.
⇒ **v4 must require the reject lands WITH OR BEFORE `clone_fn` in ONE commit, plus an ENUMERATION of every
position `ptr_materialization_kind` reaches versus the reject's coverage** — **the reject-coverage set is
currently a SELECTION with no witness (SIX-Q #3).**

⛔ **B4 — THE SELF-HOST IS *NOT* EXCUSED BY `t1311`, AND MY *"cite it, or measure it"* INVITED THE DODGE.**
`t1311` is a **C-EMIT** defect; **an accept→reject conformance fixture is a CHECK-TIME rejection and never
reaches C-emit.** ⊕ **And the self-host ALREADY MIRRORS THE CARVE-OUT INCLUDING `Box`** — its
`is_single_owner_type` lists it, and it carries the FieldIndex message verbatim. ⇒ **honest Core #9 disposition:
Rust C + LLVM + SELF-HOST all pin the final state; ONLY ggdef abstains.**

⭐⭐ **AND THE REVIEW HANDED ME A FAR BETTER DEMONSTRATION THAN MY HYPOTHETICAL — E-b.** `t0682` **row 3
(`Box[String] taken = h.b`) IS ALREADY REJECTED AT HEAD** with `E_MoveWithoutOperator`, whose own text says
*"copy the sub-place with `h.b.clone()`"* — **and writing that fix-it ICEs** (`Tier 2a consume-site violation —
untracked source consumed`). **Under `clone_fn`-only the ICE becomes `Built:`.**
⇒ ***THE COUPLING IS DEMONSTRABLE TODAY, ON A SHIPPED DIAGNOSTIC, WITH A MEASURED BEFORE/AFTER.*** ⊕ **And the
Tier-2a validator is a THIRD CODE REGION my scope never named.**

⊕ **E-c — `t0682`'s ROW TABLE HAS MOVED AND I DID NOT KNOW: row 3 is NOW REJECTED** (R49 Track S-a2 landed it).
⇒ **the reject work is rows 2 and 4, BOTH CONSTRUCTOR positions** — and **`t0682` closes only if row 3's fix-it
ICE closes too.** ⊕ **E-d names the write site I left vague:** `is_constructor` matches `Variant | Newtype` only,
with an in-source comment reading *"A POSITION WITH NO SUBJECT … stays open under `todo/t0682`"* — **SIX-Q #4,
already written down.** ⊕ **And I conflated a PARAMETER source with a FIELD one** in the fixture re-point.
⊕ **E-e — THE LLVM LANE IS WORK, NOT A MEASUREMENT:** the LLVM backend forward-declares `Box__<inner>__drop`
with **no clone counterpart**, and the prototype adds the emitter **only to the C-LIR path.**
⊕ **E-a — *"the only recourse"* is an OVERCLAIM:** `Box[String]((*h.b))` builds and runs at HEAD. **The Core #10
breach stands — the DIAGNOSTIC'S OWN fix-it is broken — but say that.**
⊕ **E-f — the residual leaks are the FIX'S OWN unregistered births (Core #3), NOT `t1309`** — 32 B where HEAD
refuses to build at all is **definitionally new**.
⭐ **VERIFIED: the killer sentence is LIVE**, the `.clone()` C is **byte-identical (3906 lines each)**, all five
of my other errata land, and **the four `is_box: true` sites are LINT-PINNED** — a real readiness-row-2 witness.

### ⭐⭐⭐ THE REFERENCE-GRADE END STATE FOR D0′ **AND** `t1408` (owner-asked 2026-09-05) — **`Import` IS A FIELD, NOT A KIND**

⛔ **ROOT CAUSE, AND EVERY FRAMING SO FAR — MINE INCLUDED — WAS AT THE WRONG LAYER.** `DefKind` has 12 variants:
`Function`, `Struct`, `Enum`, `Variable`, … **and `Import`.** ⭐ **But `Function` says what a name IS; `Import`
says how the name GOT HERE. THOSE ARE TWO AXES COLLAPSED INTO ONE ENUM** — and a name can be both.

⛔⛔ **THE RESOLVER RESOLVES AN IMPORT AND THEN DISCARDS WHAT IT RESOLVED TO.** Seven sites write
`DefKind::Import` (`grep -n 'DefKind::Import' src/semantic/resolve.rs`) — builtins with `Span::dummy()`, and
every user form (`import std.io`, `from X import y`, `as`-aliases, globs). **The TARGET's kind is never written
through.** ⇒ ***LAYERING RULE 4 VERBATIM: "resolve once, write through — downstream doesn't redo the work and
doesn't get to disagree."*** **Today downstream redoes it TWICE** (the hardcoded `BUILTIN_GENERIC_TYPES` list;
the `Callable` `matches!`) **and DISAGREES WITH ITSELF about `Owned`.** ⊕ **And the one existing consumer
APPROXIMATES the question it cannot ask:** `def.kind != DefKind::Import || def.span == Span::dummy()` is
*"is this an unresolved user import?"* **reconstructed from a placeholder kind plus a synthetic-span
heuristic** — the read-site reconstruction Layering rule 4 exists to forbid.

⭐⭐ **THE END STATE: `Import` STOPS BEING A `DefKind` AND BECOMES A PROVENANCE *FIELD* ON `DefInfo`; THE `kind`
OF AN IMPORTED NAME IS THE TARGET'S KIND.** Everything falls out:
- `def_namespace(kind)` answers *"is this a type?"* **TOTALLY** — no list, no span heuristic, no `matches!`.
- ⭐ **`t1408` EVAPORATES.** One registration path because there is ONE QUESTION; the `matches!` retires and
  `Owned` stops being special **because nothing is special.**
- ⭐ **D0′'s shadow-reject becomes THREE LINES AND CANNOT BE INCOMPLETE** — which is exactly what Ruling 3 asked
  for and what the current tree cannot deliver.
- `resolve.rs`'s consumer asks its REAL question (`provenance == Import && !synthetic`) instead of
  approximating it.

⭐⭐⭐ **THE STRONGEST SIGN IT IS THE RIGHT LAYER: THE FIX HAS THE SAME SHAPE AS MACHINERY ALREADY IN THAT
FILE.** `deref_wrapper_kind` and `has_intrinsic_equality` are **seeded-once TYPED FIELDS on `DefInfo`**, added
precisely so downstream reads a flag instead of re-deriving from a name (`grep -n "deref_wrapper_kind" -B 8
src/semantic/scope.rs`). ***Provenance is the third field of that family. It was always a field pretending to be
a kind.***

⇒ ⛔ **SEQUENCING CONSEQUENCE, AND IT REFRAMES BOTH ITEMS: `t1408` AS FILED IS THE READ-SITE FIX — it makes the
LIST complete without making the LIST UNNECESSARY. The write-through is the WRITE-SITE fix (Core #1).** ⇒ the
long-term answer is **NOT** *"`t1408`, then D0′"* — it is ***"do the write-through, and `t1408` stops existing
while D0′ becomes trivial."***

⚠ **THE HONEST COUNTER-ARGUMENT, TO BE PRICED BY A SCOUT, NOT WAVED THROUGH:** imports carry **visibility,
module provenance, `as`-aliasing and re-export** — things a target kind does not. **If `Import` stops being a
kind, that information needs a home.** ⭐ **But that IS the point: it should have had one anyway. Two axes in one
enum is Layering rule 3, and the collapse is precisely why *"is this a type"* has THREE answers today.**

### ⚖⚖⚖ OWNER ASK — **RULING 3 CANNOT BE IMPLEMENTED AS WRITTEN, AND THE REASON IS ITS OWN WORKED EXAMPLE**

⛔⛔ **THE `DefKind` IS AVAILABLE. THE TYPE-KIND SET IS NOT WELL-DEFINED, AND NO PARTITION OF IT WORKS.**
**All 16 builtin generic types are registered as `DefKind::Import` with `Span::dummy()`** — **the SAME kind
`collect_import` assigns to every user `import X` / `from X import y`, including imports of FUNCTIONS and
CONSTANTS.** Measured with a probe in `define_with_mutability`:

| case | `lookup_type` hit | required verdict |
|---|---|---|
| **`int Vector = 5`** — *ruling 2's own example* | `kind=Import dummy=true` | **must be CAUGHT** |
| `from std.os import args` + a param named `args` | `kind=Import dummy=false` | **must NOT be caught** |
| `from gir import CMP_NE` + a sibling's `const int CMP_NE = 1` | `kind=Import dummy=false` | **must NOT be caught** |

⇒ ⛔ **BOTH BRANCHES ARE WRONG.** **Include `Import`** → **1191 fires across 92 distinct files** (1190 of them
`dummy=false`) — **ordinary correct code**: `args` ×326, `CMP_NE`/`CMP_EQ` ×27 each, `OP_ADD`, `stderr`; **all
five self-host drivers among the 92.** **Exclude `Import`** → **the rule does NOT reject `int Vector = 5`**, nor
`Box`/`Dict`/`Set`/`Shared`/… — residual coverage across the whole corpus is **exactly ONE cell.**
⛔⛔ **AND EVERY DISCRIMINATOR INSIDE `Import` IS THE FORBIDDEN NAME LIST IN DISGUISE:** `span == Span::dummy()`
selects **exactly `BUILTIN_GENERIC_TYPES` + prelude** — ***the list, keyed on a span.*** Resolving the import to
its TARGET's kind would work, **but the resolver never writes that through** — imports stay `Import`
placeholders, and other code depends on that.

⭐⭐ **AND THE FAILURE IS MINE IN A NAMED WAY: MY B1 COUNTED THE FIRES AND REPORTED THEM AS A *COST TO BE
BORNE*. THEY ARE PROOF THE PREDICATE IS WRONG.** *I asked "how many fire?" and never asked "what should the
check DO with `DefKind::Import`?" — nor noticed that the ruling's own worked example IS one.* **That is the axis
v5 held fixed.**

⇒ ⚖ **THE ASK: `t1408` — WIDENED TO COVER `Import` — MUST LAND *BEFORE* D0′'s SHADOW-REJECT.** ⛔ **AND THIS
REVERSES WHAT I TOLD THE OWNER TWO HEARTBEATS AGO** (*"`t1408` next round; landing it second is the experiment
that proves D0′'s layering"*). **That advice was sound as an ORDERING PREFERENCE and is FALSIFIED AS A
FEASIBILITY CLAIM: the check is not merely UNDER-COVERING without `t1408`, it is UNDECIDABLE ON THE FLAGSHIP
CASE.**

### 🚨🚨 AND AS DESIGNED, D0′ WOULD SHIP AN **ACCEPT→MEMORY-UNSAFETY** CHANGE

⛔ **My B5 blamed the residual SIGSEGVs on "construction sensitivity". WRONG — they have NOTHING to do with
`expr[...](args)`.** `gg parse` on the literal-index cell shows `Call { callee: Index { Index { … } } }` — **no
generic call, no ambiguity** — and the bind-then-call cell has **no call-through-index at all.** They are
**`Vector[Callable[…]]` built from a CONTAINER LITERAL, memory-unsafe on ANY read, both backends.**
⭐ **ALREADY FILED AS `todo/t0873(a)`**, HIGH, with a durable repro and **explicitly sequenced behind `t0406`.**
⇒ **cite `t0873(a)` and `t0406` and sequence around them; do NOT pin a symptom of theirs inside `t1393`.**

⛔⛔ **CORE #8 — THE REFERENCE-GRADE GATE FIRES:** D0′ turns `fs[n](7)` from a **safe REJECT** into an **ACCEPT**.
On a literal-built container that accept is **`gg check` clean, `gg build` rc 0, SIGSEGV.** ⊕ **And
`row = vv[0]; row[n](7)` REJECTS CLEANLY AT HEAD and would become an accept onto the crashing path.**
⭐ **SIX-Q #6 AT THE DESIGN LEVEL: the prototype's ONE green cell is `.push`-constructed — and `.push`-constructed
`vv[0][0](7)` ALREADY PRINTS 21 AT HEAD WITH NO FIX.** ⇒ **the disambiguation does real work only in a narrow
cell, and the brief never separates the two effects.**

⊕ **ER-a — MY ER1 OVER-CORRECTED, AND THE INSTRUCTION WAS THE DANGEROUS DIRECTION.** `self_host_check` and
`self_host_lowerer`'s `resolve.gg` are **SYMLINKS** (`-type f` → **2**, `-type l` → **2**). ⇒ ***"two copies" was
RIGHT; my "FOUR files ⇒ two drivers unported" is FALSE, and an executor told "four files" MAY CONVERT SYMLINKS
INTO REGULAR FILES AND MANUFACTURE THE DIVERGENCE THE ERRATUM WARNED ABOUT.***
⊕ **ER-e — THE CITED PROTOTYPE IS NOT ON THIS BOX** (`ls /tmp/*ckpt*` → no matches) ⇒ **every "under the
prototype" claim is unverifiable by the executor**, against the fold rule. ⊕ **ER-b — `4909 files` has NO
regenerating command and does NOT reproduce** (5730 / 5380 / 2249 depending on the corpus definition).
⊕ **ER-c — the f-string split names no RECEIVING TRACK**, and the rule is *"each half becomes its own track in
the SAME round; a split is division, never deferral."*
⭐ **VERIFIED CORRECT:** B4's two mandatory defences (both hazards real, both defences right), B2 (**ggdef
accepts and runs BOTH shadow shapes** ⇒ mirror + conformance fixture owed), ER6, **ER9's D38/D39 asymmetry
framing**, and the ID block.

### ✅✅✅ RULING 3 (owner 2026-09-05) — **THE CHECK KEYS ON `DefKind`, NEVER A NAME LIST**

> *"Reject a value binding whose name resolves to a type-kind def — read `DefKind` from the resolver, not from
> any list. This is the way to go."*

⭐ **All three D0′ questions are now settled, and `v5` is written on them.** ⛔ **Ruling 3 is the one that keeps
the executor honest: it needs NO name set, so it CANNOT be incomplete, and it is Core #2-clean BY
CONSTRUCTION.** ⇒ **`todo/t1408`'s two-sources-of-truth defect stops being a hole in the CHECK and becomes a
registration repair on its own schedule — close it and the check picks up the callable family with ZERO CHANGE.**
⚠ **The residual is stated in the brief and belongs in the check's own DOC COMMENT, never papered with
literals.**

### ⭐ SCOPE CALL — **THE F-STRING WALK IS PULLED OUT OF D0′**

My E2 called it *"one line, blast radius one row"*. **Measured with an instrument blind to the class** (a
`gg check` rc diff, while the walk's effect is on **LOWERING**). **Real radius: SEVEN files — six
`robustness_map/cells/*` and a filed HIGH it CLOSES (`t0691`, both lanes, LLVM SILENT WRONG VALUE, with a
durable repro and an `#[ignore]`d test).** ⇒ **it touches a five-lane gate whose SELF-HOST lane will not have
the fix, and graduating a `known_gaps` fixture carries census + sanitize + Core #9 consequences of its own.**
**OUT of D0′; `t0691` becomes its own candidate.**
⊕ **AND MY *"redesign around a gap"* CHARGE IS WITHDRAWN** — `t0691`'s own text says the tree has a standing
rule that accept/reject must NEVER be observed through an f-string, *"it changes the LOWERING"* ⇒ **rewriting
the repro to the plain spelling is RESTORING PROBE HYGIENE.** *The reviewer who overruled me was itself
overruled, on the item's own words.*

### ⭐ THE `Callable`/`Owned` GAP IS NOT A CAVEAT — IT IS A **SECOND SOURCE OF TRUTH**, FILED AS `t1408`

⚠ **I RAISED IT AS A FLAG AND THE OWNER CORRECTLY ASKED WHETHER I WAS ASKING OR PROPOSING.** *A fact without a
recommendation is the "unfinished scout wearing a ballot" pattern again.* **Proposal below.**

⛔ **THERE ARE TWO INDEPENDENT ANSWERS TO *"IS THIS NAME A TYPE?"*:** a **hardcoded string list**
(`BUILTIN_GENERIC_TYPES`, 16 names) seeding the ScopeTable's type namespace, **and a separate `matches!` on
string literals** for `Callable`/`MutCallable`/`ConsumeCallable`. **`Owned` is in NEITHER.**
⇒ **Layering rule 3 (one source of truth per axis) with a Core #2 name-match inside the second mechanism.**

**MEASURED, at HEAD — the asymmetry is real:**
`Box[int] Box = …` → **noticed** (unused-variable warning names `Box`) · `Callable[int(int)] Callable = …` →
**`gg check` CLEAN, nothing notices.**

⭐⭐ **THE REFERENCE-GRADE SOLUTION KEYS THE CHECK ON THE THING THE RULING ALREADY NAMES AND DELETES THE
QUESTION: reject a value binding whose NAME RESOLVES TO A TYPE-KIND DEF — read `DefKind` from the resolver, not
from any list.** ***That IS "resolve by kind", applied one layer over: it needs no name set, so it CANNOT be
incomplete, and it is Core #2-clean by construction.***
⇒ **Then `Callable`/`Owned` stop being a hole in the CHECK and become a separate registration defect. Close
`t1408` and the check picks them up WITH ZERO CHANGE — one fix, and the other stops needing a special case,
which is the test for whether a fix sits at the right layer.**
⛔⛔ **WHAT NOT TO DO, STATED EXPLICITLY: do NOT add `"Callable"`/`"Owned"` to a list inside the shadow-check —
that replicates the exact defect in the code written to fix it.**
⚠ **HONEST RESIDUAL: until `t1408` lands, a kind-keyed check still misses `Callable`. Ship the check keyed on
`DefKind`, and state the gap in the check's own doc comment rather than papering it with two string literals.**
⊕ **And `t1408` flags a design question before its own fix: the callable family may need distinct TREATMENT
(it parses as a function-type form) — so the fix is one source of truth for MEMBERSHIP, not necessarily one code
path for BEHAVIOUR.**

⊕ **A FALSE ALARM I CAUGHT BEFORE RAISING IT:** a first grep appeared to show `"A"` and `"B"` inside the builtin
TYPE list — which would have meant `int A = 5` becoming an error under the ruling. **They came from a DOC
COMMENT about `Pair[A, B]` generic params; the list is 16 genuine type names.** ⚠ ***A grep window one line too
wide invented a catastrophic consequence. Read the match, not the count.***

### ✅✅ RULED 2026-09-05 — **READING (a): THE SHADOWING DECLARATION FAILS.** *"`Vector` is a type, must not be used as identifier."*

⚠ **AND A CLARIFICATION THE OWNER HAD TO PULL OUT OF ME:** `Vector[int] v = [1,2,3]` is **NEVER AFFECTED** — the
rule asks **only** whether the *variable's NAME* also resolves as a type. `v` resolves as nothing; the
`Vector[int]` on the left is the **type ANNOTATION**, a type POSITION the rule never inspects. ⛔ **My example
differed by one token and read as if normal declarations were at risk. Say the rule in terms of the NAME, never
by showing a declaration.**

### 🔎 ARCHAEOLOGY (owner-requested) — **IT WAS NEVER ALLOWED ON PURPOSE. IT FALLS OUT OF A DESIGN BUILT FOR SOMETHING ELSE.**

⭐⭐ **THE MECHANISM IS TWO DISJOINT NAMESPACES, AND ITS STATED PURPOSE IS UNRELATED TO BUILTINS.**
`Scope` keeps `types` and `values` as **separate maps** (`grep -n "two disjoint name maps" -A 8
src/semantic/scope.rs`), *"so that e.g. `Error` can live simultaneously as a user-defined trait AND the
`Result.Error` variant constructor — the former looked up at type positions, the latter at expression /
pattern positions."*
⇒ **a value named `Vector` goes in the VALUE map while the type sits in the TYPE map. THEY NEVER MEET** — no
duplicate-definition check fires, and **nothing ever had to decide the question.**

⭐ **WHAT THE DESIGN *DID* ANTICIPATE — AND HANDLED CAREFULLY — IS A *TYPE* SHADOWING A BUILTIN TYPE.** Builtins
register as **dummy-span `Import` placeholders** that *"can be replaced by anything"*, and a user `struct
Vector` that replaces one gets a **DISTINCT DefId** with `deref_wrapper_kind: None` and
`has_intrinsic_equality: false` — **explicitly so it cannot inherit builtin behaviour**; the comment names the
bug that would otherwise follow (*"the garbage-0 miscompile"*). ⇒ **that half is DESIGNED. It simply never
contemplated a VALUE taking the name, because a value cannot carry those flags and never needed to.**

⭐⭐⭐ **THE DEEPER POINT, AND IT EXPLAINS WHY THIS SURFACES ONLY NOW: TWO DISJOINT NAMESPACES ARE SOUND EXACTLY
AS LONG AS NO OPERATOR SPANS BOTH. `expr[...]` DOES** — subscript in the value namespace, instantiation in the
type namespace. **The collision was invisible until `[]` began resolving BY KIND.**

⊕ **AND THE RULING CONTINUES AN EXISTING REPAIR LINE RATHER THAN OPENING ONE.** *Snag #29 follow-up #2
(2026-05-10)* narrowed a **sibling clause** for exactly this class: a permissive rule let a user definition
silently replace a same-named import, which *"produced wrong resolution at use sites"*; the fix made **both
orders error consistently — *"the user must rename one or remove one."*** **Same remedy, one namespace over.**
⇒ **`struct Vector` shadowing the builtin STAYS LEGAL AND SAFE (designed). `Vector[int] Vector = …` becomes an
ERROR.**

### ⚖⚖ OWNER ASK — **WHERE DOES THE SHADOW-REJECT FIRE?** THE TWO READINGS ARE DIFFERENT SEMANTICS

⛔⛔ **THE RULING AND MY BRIEF BOTH SAY *"AT THE AMBIGUOUS SITE"*, AND BOTH OFFER
`Vector[int] Vector = [1,2,3]` AS PROOF IT IS A REAL ACCEPT→REJECT CHANGE. THOSE TWO STATEMENTS ARE
INCONSISTENT — that program contains NO `x[…](…)` SITE AT ALL.**
- **(a) reject at the shadowing DECLARATION** ⇒ the example errors; **the cost is EVERY value binding whose name
  collides with a generic type** — locals, params, for-vars, consts, statics, pattern binds.
- **(b) reject at the ambiguous USE `x[k](v)`** ⇒ **the example STAYS ACCEPTED, my accept→reject claim is
  FALSE**, and the cost is **provably 0** straight off the fire counter.
⇒ **different semantics, different blast radii, different instruments. A hard error on three lanes cannot be
built on the phrase as written.** ⚖ **Round-lifecycle 7(ii): unratified semantics ⇒ OWNER ASK.**

⭐ **AND THE MEASUREMENT WAS RE-RUN WITH AN INSTRUMENT THAT RESOLVES INSTEAD OF EYEBALLING** — a probe inside
`ScopeTable::define_with_mutability` firing when a value is defined while `lookup_type` resolves.
**4909 files: 791 `Import` shadows + 1 `Enum` shadow, and ZERO shadowing a builtin generic type name.**
⇒ ⭐ **MY ANSWER WAS RIGHT; MY JUSTIFICATION WAS NOT** (I classified by the type-first syntax rule; this
resolves). ⊕ Coverage hole stated: **68 of 4909 files never reach resolution.**
⚠⚠ **AND A HAZARD THE INSTRUMENT SURFACED: `Callable` and `Owned` ARE NOT IN THE ScopeTable's TYPE NAMESPACE**
(`Option`/`Result`/`Set`/`Box` all are). ⇒ **any shadow-check built on the type namespace is BLIND to the two
generic spellings most likely to be shadowed by a callable-valued local.**

### ⛔ FOUR MORE BLOCKING ON D0′ v4 — AND THREE ARE MINE

⛔ **B2 — THE SHADOW-REJECT BINDS ggdef WITH NO ESCAPE CLAUSE, AND MY ONLY ggdef PARAGRAPH SAYS THE OPPOSITE
ABOUT THE *OTHER* HALF.** Measured: **ggdef ACCEPTS AND RUNS both shadow shapes** — squarely inside phase-0.
⇒ **Core #9 requires a REAL `spec/ggdef` MIRROR plus a conformance fixture pinning the REJECT, same round.**
**A reader carries "ggdef takes the escape clause" across the section break and mis-scopes the track.**
⊕ **Stronger than I stated for the disambiguation half: the ALREADY-ACCEPTED literal-index sibling is itself
out-of-subset ⇒ ggdef has NEVER adjudicated this family.**

⛔⛔ **B3 — E2's BLAST RADIUS WAS MEASURED WITH AN INSTRUMENT BLIND TO THE CLASS.** *"Exactly ONE row across
4909 files"* came from a **`gg check` rc diff** — but the walk's whole effect is on **LOWERING** (it un-blinds
the sibling `Call → StructLiteral` rewrite). **Real radius, via a fire counter: SEVEN files** — including a
filed **HIGH** known-gap and **SIX five-lane robustness-map cells**, three of which go **build-rc-1 → correct
output**. ⇒ ⛔ **THE WALK CLOSES `todo/t0691`** (filed 2026-08-27, HIGH, both lanes, LLVM **silent wrong value**,
with a durable repro and an `#[ignore]`d test) — **my E2 treated it as unfiled.** ⇒ ***not an erratum fold: a
track, or a scoped sub-deliverable with its own lane discharge.***
⊕ **AND MY "REDESIGN AROUND A GAP" CHARGE IS ANSWERED BY `t0691`'s OWN TEXT** — the tree has a standing rule
that accept/reject must never be observed through an f-string, *"it changes the LOWERING"*. ⇒ **rewriting the
repro to the plain spelling is RESTORING PROBE HYGIENE.** *The reviewer who overruled me was itself overruled.*

⭐ **B4 — THE RESOLUTION-BASED RULE IS IMPLEMENTABLE. MEASURED.** The identical lookup the `Identifier` arm
already performs answers for field and index heads, **because a postfix expression's span STARTS AT ITS ROOT
IDENTIFIER** ⇒ **close my escape hatch and PRESCRIBE it.** ⛔ **And the arm is WRONG, not merely unguarded, with
a witness rather than an argument from absence:** an enum-constructor head resolves to **`Enum`** — instantiate
under the ruling — **and the unguarded arm indexes anyway.**
⚠⚠ **THE MECHANISM RESTS ON AN UNGUARDED SPAN INVARIANT: keyed on span-start, it resolves the ROOT of the
access path, not the head — and THE SIBLING REWRITE IN THE SAME FUNCTION DOCUMENTS THIS HAZARD AND DEFENDS
AGAINST IT** with a name-equality check against *"span collisions from derive-generated code"*. **Require the
same defence, or "resolution-based" is SPAN ARITHMETIC WEARING A `DefKind`.**
⊕ **The `.or_else(scopes.lookup)` fallback is SCOPE-BLIND** (the pass never tracks scopes) ⇒ **on the shadow path
that fallback IS the innermost-wins machinery the ruling REJECTS. It must not be consulted for that decision.**

⛔ **B5 — `t1393` OVERSTATES THE FIX AND THE CELLS ARE CONSTRUCTION-SENSITIVE.** *"Already measured to close all
THREE"* is **false** — the third face is untouched (0 fires), exactly as my own E3 says. **The item and the
brief contradict each other on the artifact the executor works from.** ⊕ **And the crash SURVIVES in adjacent
cells for an unrelated reason:** built by list literal instead of `.push`, `vv[0][n](7)` **still SIGSEGVs under
the prototype** — as does the unambiguous `vv[0][0](7)`. ⇒ **the durable repro must PIN THE CONSTRUCTION, and
neither the item nor the brief may say the shape is closed — only that ONE construction of it is** (Core #12:
the fixture's NAME is a claim about SCOPE).

⊕ **ERRATA WORTH CARRYING:** **hash counts are not file counts** — `resolve.gg` is **FOUR files / two hashes**,
`parser.gg` **FIVE / three**; *"two copies"* leaves two drivers unported. · ⭐ **A MEASURED WAY OUT OF THE
SANITIZER QUESTION: a leak-free exercising fixture EXISTS** (a named `int triple(int)` instead of a closure
literal → prints `21`, **ASan CLEAN**) ⇒ **ship that top-level, keep the closure spelling in `known_gaps/`, and
the allowlist question closes with NO owner ask.** · ⛔ **MY SYNTAX-BUDGET FRAMING IS WRONG (SIX-Q #4 AGAIN):
the directive's SUBJECT is decisions that ADD surface syntax; this ruling adds NONE.** The section is owed on the
**ruling's own authority** — and the real tension is sharper: ***D38/D39 established that when `[]` is ambiguous
Gorget REJECTS the spelling and moves the operation to a named accessor. This ruling picks a reading BY
RESOLUTION for the index half while doing the D38 thing for the shadow half. THAT ASYMMETRY is what the section
must justify.***

### 🚨 `t1388` IS CONFIRMED AT THE INTEGRATION TIP — AND ITS FILED **MECHANISM IS MEASURABLY FALSE**

⭐ **The SEGV reproduces at `19211162e`** — *ahead* of the commit the item cites — **so it survives everything
R50 has landed.** `gg check` clean; ASan `SEGV in Shared__int64_t__get`; both backends.
⭐⭐ **AND A ROW THE ITEM DOES NOT RECORD: bind + `b.get()` ONLY → `rc 0` WITH A GARBAGE VALUE, BOTH BACKENDS.**
⇒ **a SILENT-WRONG-OUTPUT defect sitting beside the crash — an executor who fixes the SEGV and stops LEAVES IT.**

⛔⛔ **THE FILED MECHANISM IS REFUTED BY THE EMITTED C.** The item says *"duplicates the handle without an incref
and drop-registers BOTH names, so the first scope-exit drop frees the control block."* **An incref IS emitted,
and NOTHING has been dropped** (ASan's frame is the FIRST `.get()`; both drops take the correct `&local` form).
⭐ **THE REAL MECHANISM: `Shared__T__clone` takes the handle BY VALUE while `Shared__T__drop` takes it BY
ADDRESS — and the bind passes `&slot`.** Since `strong` is field 0, the atomic increment **bumps the POINTER
VARIABLE ITSELF BY ONE BYTE** (UBSan: misaligned address) and returns `&slot` as `b` ⇒ **`a` reads misaligned →
SEGV, and `b` is a handle pointing at `a`'s OWN STACK SLOT → garbage.**

⛔⛔⛔ **AND THE CLASS IS ALREADY FIXED TWICE, AT TWO OTHER SITES.** `src/lir/lower/insts.rs` describes the
identical failure **VERBATIM** — *"Passing the slot address made `Shared__T__clone` incref whatever the SLOT
ADDRESS pointed at (t0840…)"* — and gates on `is_refcount_clone_type_name`; `methods.rs` does the same for the
explicit `.clone()` path, **which is exactly why `a.clone()` prints `7 7`.** **The bare bind is the THIRD
consumer that never consults the predicate** (`grep -c "is_refcount_clone_type" src/ir/lowering/stmts/mod.rs`
→ **0**). ⇒ ⭐ ***`t1388` is a Core #4 SIBLING-SITE-DRIFT INSTANCE OF `t0840` — and the item cites `t0840`
NOWHERE.*** Its discrimination *from* `t0108`/`t0620` is **sound and verified**; its **positive family
attribution** is wrong, and it **points the executor at `drops.rs` where nothing is broken.**

⛔ **AND THE FOLD LANDED IN THE GENERATOR ONLY.** The same commit **edits `todo/t1384.md`** and leaves its bullet
list carrying the **superseded four-value enumeration the fold existed to retire** — including the `None` row
asserting *"no subject; widening cannot manufacture one"*, **which is `(Trivial, None)` only and omits the
`(Resource, None)` the fold measured and left OPEN**, and the `Trivial` row that **omits `(Trivial, Trivial)` —
the row that turned out to be `t1388`'s SEGV.** ⇒ ⭐⭐ ***`todo/` IS THE DURABLE RECORD; A PYTHON DOCSTRING IS
NOT*** — and an **OPEN row living only in a docstring is an UNFILED DEFERRAL.** *Fold rule (c)/(d) missed on a
bullet list the commit was already editing.*

⭐ **VERIFIED AND STRONGER THAN CLAIMED:** the six rows are **TOTAL**, with a witness better than the doc table —
**`src/ir/validate.rs` is a rustc-EXHAUSTIVE match over the `(CopySemantics, DropStrategy)` PAIR.** The
generator's dead flag and its fix are **RED-verified in BOTH directions**, with SIX-Q #2 answered. `(Resource,
None)` OPEN judged **honest**. E5b's figures regenerate exactly and the set-identity claim holds. **And
item-by-item, NOTHING was lost in the executor's `git checkout --` recovery.**
⊕ **ERRATUM WORTH KEEPING: `.expect("{CONTROL_SRC} …")` — `Option::expect` takes a PLAIN `&str`, so the braces
print LITERALLY.** *The message that names the guarded cell now names a Rust identifier instead.*
⊕ **`todo/t1265` asserts *"`b = a` makes `b` the SAME handle"* — REFUTED by this measurement.** Cross-cite.

### ⛔⛔ C1 v2 PASS 1 — THE MEASUREMENTS SURVIVE; **THE DESIGN CONCLUSION DOES NOT**

⭐ **CONFIRMED, all of it:** HEAD aliases (one allocation, two owners; the recursive drop **emitted with ZERO
call sites**; the escaping cell is an ASan **heap-use-after-free**) · `.clone()` on a `Box` emits **byte-identical
C** via the `return recv` fall-through · regression #1's unreachability confirmed **BEHAVIOURALLY** (cell 7 alone
moves `60`→`0`) · **all four ratified cites literally accurate** · ggdef's abstention **re-measured** (0 hits,
and the `Ty` enumeration is the durable witness).

⛔⛔ **BLOCKING 1 — I SHIP `t0682`'s OPEN CELL IN THE DIRECTION `t0682` RULES AGAINST.** I never quoted its
disposition. It is decisive: `t0682` (HIGH, **open**) measures `Box[Box[String]](h.b)` as **ACCEPTED ⛔** and
rules ***"LAND-AS-REJECTION … instead of lowering something the language says is illegal and hoping the drop
model copes."*** **And the fixture's OWN header prescribes its end state:** *"EXPECTED (once `todo/t0682`
lands): `gg check` FAILS with `E_MoveWithoutOperator`."*
⇒ ⛔ **MY "PREMISE RETIRED — re-point it at the correct output" DELETES A TRUE CLAIM.** *(The retraction rule,
broken again by its author.)*
⇒ ⛔ **AND MY CAUSAL SENTENCE IS WRONG.** The rc 101 is **not** *"C1 correctly refusing to mint from a borrow it
cannot materialize"* — the reviewer read it: it is a **LABELLED PLACEHOLDER FOR THE REJECT**, whose message ends
*"a lowering-layer stopgap for `todo/t0682`, which owes the check-time diagnostic."*
⇒ ⛔⛔ **AND THE LEDGER NAMES MY OUTCOME AS A BREACH.** D32 (owner **2026-09-04**, one day before my brief):
a closure capturing a `Callable`/`Box`/… — ***"clone breaches the carve-out."*** And `type_utils.rs` asserts of
`Box[T]`: *"they are NOT CoW-eligible, so there is **no implicit clone-if-live path**."* **My success criterion —
*"both cells compile and run UNCHANGED"* — IS that path**, and would falsify that comment with **no guard
noticing** (Core #14).

⭐⭐⭐ **THE REFERENCE-GRADE SHAPE, AND IT IS STRICTLY BETTER: SHIP `clone_fn` *AND* `t0682`'s CHECK-TIME
REJECT.** Then `Box.new(h.b)` **rejects at check with a fix-it — `h.b.clone()` — THAT NOW ACTUALLY WORKS.**
⇒ ***closing the Core #10 hole is what MAKES the reject shippable***; the `panic!` becomes an unreachable
backstop; the fixture is re-pointed to `check_gg_fails` **as its own header prescribes**; and **`t0682` CLOSES.**
⚠ **THE OWNER'S "it should work" IS HONOURED — "work" MEANS *A LEGAL SPELLING EXISTS AND RUNS*, NOT *the bare
form is accepted*. That distinction is the whole finding, and it is mine to put to the owner.**
⊕ **SIX-Q #4 was already written down and I never asked it:** `is_constructor` matches `Variant | Newtype` only,
so the helper never runs for `Box[Box[String]](…)` — ***"a case with no subject, which no widening of this arm
reaches."***

⛔⛔ **BLOCKING 2 — MA-5 FALSIFIED: `C1` ALONE CLOSES `t0011`, ASan-CLEAN.** Applied each half alone to pristine
HEAD: **`t0011`'s own repro goes rc 134 → rc 0, ASan-clean, under C1 ALONE.** ⇒ **my two MA-5 clauses are each
true of a DIFFERENT PROGRAM, presented as one:** *"clone_fn alone leaves `t0011` at 134"* ✅ true; *"C1 alone is
rc 101"* ⛔ **FALSE for `t0011`** — true only for the scout's **nested-Box** cell, which is `t1077`/`t0682`
territory. ⇒ ***"only together do they work" is FALSE.***
⭐ **THE AXIS I HELD FIXED WAS THE PROGRAM — the round's signature failure, this time INSIDE THE MA-5 MATRIX
THAT EXISTS TO CATCH IT.** ⊕ **Readiness row 4 is unsatisfiable as written:** reverting C1 leaves `t0011` **red**,
reverting `clone_fn` leaves it **green**.

⊕ **SEVEN ERRATA, and two are broken citations of mine** (`is_resource_name` is in `src/ir/types.rs`, not
`lowering/types.rs`; `has_inner_clone` is in `src/backend/c_lir/emit_types.rs` — **the file I cited does not
exist**). ⊕ **The ggdef subset gap is ALREADY FILED as `t1227`** — *cite it, do not re-file*. ⊕ **The LLVM cite
shows the symbol EXISTS, not that it is unguarded — and it is about DROP wrappers while the fix adds a CLONE
emitter.** ⊕ **The 73 B / 2-alloc residual is MATCHED, NOT MEASURED** — the allocating frame is **the fix's own
new `Box__GorgetString__clone`**, while `t1309`'s fixture *contains no clone at all*: **same magnitude ≠ same
cause (SIX-Q #6).** ⊕ **`ensure_shared_type_def`'s exclusion list is a SELECTION** (it enumerates wrappers built
by a *different* function than the one that registers `Box`) — **argument from silence; drop that leg, D4 and
D53 carry the claim alone.**
⭐ **AND THE SHARPEST FACT WAS SITTING IN MY OWN QUOTE, UNUSED:** the `E_PartialMove` diagnostic says
***"…or copy the sub-place with `.clone()`"*** ⇒ **THE COMPILER'S OWN FIX-IT IS THE NO-OP.** *That single
sentence is the Core #10 argument.*

### ⭐⭐⭐ TRACK H — MY HYPOTHESIS WAS WRONG ABOUT THE SITE, AND THE DEFECT IS **UNBOUNDED**, NOT 2 BYTES

⛔ **NOT a loop-head clone missing drop registration. THE PRODUCER IS CORRECT.** The `for ch in s` element is a
**view**, and it IS registered for drop. **The leak is at the `.put()` CONSUMING-POSITION clone, and the
CONSUMER drops ownership on the DUPLICATE-KEY path.** ⭐ **One-line proof, over the three hit branches:**
`val_drop` appears **6** times, `key_drop` **0**. ⇒ ***the VALUE has a drop discipline; the KEY has none.***
On a miss the memcpy transfers the key; **on a hit nobody owns it.**

🚨 **UNBOUNDED, NOT 2 BYTES — and the mechanism gives the scaling law:** 5 identical keys → 4 duplicate puts →
**exactly 4 leaked allocations.** 200k-put benchmark: **`total_frees` 60 → 200034 · `live_bytes` 399948 → 0 ·
peak RSS 7480 → 1240 kB (6×)**, with `string_clone` **identical** (no extra clones).
⭐⭐ **AND THE COST IS NEGATIVE: 0.0074 s → 0.0065 s.** The only added work is one `free()` per *duplicate* put.

⭐ **A CLASS AT A SINGLE CHOKEPOINT, WITH INDEPENDENT WITNESSES** (the runtime registry and the builtin tables,
**not the scout's own list**): every surface — `.put/.set/.add/.insert`, `d[k]=v`, literals, comprehensions,
`__get_or_put` — funnels into two functions in one file.
⚠⚠ **AND THE SCOUT CAUGHT ITS OWN ENUMERATION AS A SELECTION:** a `head -40` truncated it, so it first concluded
`put_cloned`'s hit path was unreachable. **It is not — 23 call sites**; `Dict/HashMap.update` and `Set.union`
reach it, where the pre-fix code **RE-CLONES THE ALREADY-OWNED STORED KEY AND LEAKS THE OLD BUFFER.** *"That half
is load-bearing, not defensive."*

⛔⛔ **WHY ONLY ONE CELL RED-LINED — TWO MEASURED REASONS, AND THE SECOND IS A FINDING IN ITS OWN RIGHT:**
1. `tests/sanitize/CORPUS_MANIFEST.txt` carries **`robustness_map OUT`** — the sweep **never builds those 1011
   cells**.
2. 🚨 **THE MAP'S ASan BASELINE ALREADY PINS FIVE SIBLINGS OF THIS EXACT CLASS AS `SANITIZE-FAIL`** — all
   pre-dating this round, **all measured RED pre-fix and GREEN post-fix.** ⇒ ***the map's ASan lane is
   UNDER-REPORTING: five pinned instances of a live bug.***

⭐ **SIX-Q #6 LEFT OPEN AND SAID SO.** *Why* `ex_char_frequency` was green before is **NOT established** — the
scout formed a hypothesis, **falsified it itself from the emitted C**, and **invented no mechanism.**

⭐ **THE SWEEP WAS RUN BOTH WAYS — the attribution is load-bearing and it was done properly.** leaks **301 →
293**; **8 retirements and 12 new tightenings, ALL the fix's** (pristine reported **zero** retirements); **0
flaky, 0 class-drift, 0 new classes.** ⚠ **Executor obligation: those counters are `assert_eq!`, so deleting the
8 rows moves four constants — and they are FIGURES-DB COVERED. Cite the regen, never the value.**

⭐ **LANES — ONE EDIT COVERS EVERYTHING.** Stdout byte-identical ⇒ implementation-internal runtime hygiene, **no
conformance fixture owed**. C via `include_str!`; **LLVM shares the embedded runtime and was MEASURED**;
**self-host inherits via `embed_file` with ZERO `.gg` changes** (rebuild required — the `include_str!` staleness
trap); ggdef structurally blind (Core #13). ⚠ **Standing hazard recorded: the self-host hand-copies a sibling
emitter's stub STRINGS, so a future fix touching those WOULD owe a port. This one does not.**

### 🚨🚨 AND A **MORE SEVERE** DEFECT FOUND ALONG THE WAY — FILED `t1407`, CRITICAL

**`Vector[String].fill(3, mk("ab","cd"))` → ASan `attempting double-free`, on PRISTINE HEAD.**
`gorget_array_fill` **memcpys ONE source into N slots with no per-slot clone**, so every slot aliases one
payload. **Witness:** `grep -n "elem_materialize(" src/backend/c/runtime/runtime_array.c` → **2 hits, both in
`gorget_array_push`**; **`set`, `insert`, `fill` and `gorget_shared_array_set` have NONE** (⚠ only `fill`
measured live).
⭐ **WHY IT WAS NEVER SEEN: every existing `.fill` fixture uses POD elements (`bool`)** — *a fixture set that
samples one value of a typed axis (Core #12); the droppable-element cell was never covered.*
⛔ **AND IT FALSIFIES THE SAME `devbook/11` SENTENCE A SECOND TIME** — the *"every consuming runtime function
obeys the same three-step shape"* contract, which **nothing enforces**. **SIX-Q #4: the case has NO SUBJECT, so
no widening reaches it — it needs a second clause AND a guard.** ⊕ **By the severity ranking this OUTRANKS
`t1387`.**

### ⭐⭐⭐ FIXING THE **ENUMERATION AXIS** PAID FOR ITSELF TWICE — A HIGH MEMORY-SAFETY BUG AND A DEAD FLAG

**`fa4a76d92`.** Re-presenting four `DropStrategy` values as **six `(CopySemantics, DropStrategy)` pairs** was
supposed to be bookkeeping. **It was not.**

🚨 **`(Trivial, Trivial)` WAS NOT MERELY UNCOVERED — IT SEGFAULTS. FILED `t1388`, HIGH.**
**`Shared[int] b = a` — A BARE BIND, NO CALL — SEGVs ON BOTH BACKENDS** (`AddressSanitizer: SEGV in
Shared__int64_t__get`); **delete the bind and the same program prints `7` at rc 0.**
⭐ **DISCRIMINATED, not merged:** `t0108` is scoped to the **PARAM/boundary** model and **all three of its repros
are CALLS**, so an executor scoped to `needs_param_drop` **would fix those and leave this one segfaulting** —
and this repro is **smaller than every repro `t0108` carries**. `t0620` is the **leak** direction; this is the
**double-drop** direction. ⇒ ***a row that four values could not express turned out to be a memory-safety
defect. That is the enumeration argument, paid in full.***

⊕ **`(Resource, None)` IS LEFT OPEN, NOT CLOSED** — a second live place exists but has **no observable
payload**. ⭐ **That is precisely the third disposition four `DropStrategy` values had no room for.**

⭐⭐ **AND THE RED-VERIFICATION FOUND A DEAD FLAG IN THE GENERATOR — CORE #13 PAYING OUT DIRECTLY.**
`--force-expectation-change` **mutated the owned columns in memory and NEVER WROTE THE FILE**, because the write
was gated on `new_rows` alone — **and on an already-landed topic there are none.** ⇒ ***the flag silently did
nothing and exited 0.*** **Found ONLY because the executor ran the break it was told to run.** *"Without running
the break I'd have shipped a flag that does nothing."*

⊕ **E5b — THE `489` WAS DEFINITION-DEPENDENT AND I QUOTED IT AS A FACT.** Regenerating it gives **317** by a
minimal edit script and **489** index-by-index — **neither wrong.** ⇒ ***"N rows moved" without the definition
is not regenerable*** (Core #15a). The comment now carries **both measures, the command, and the claim that
actually does the work: the SET IS IDENTICAL, only the ORDER changed.**

⚠ **A PROCESS FAILURE THE EXECUTOR RECORDED RATHER THAN HID:** it ran `git checkout --` to undo a RED-verify
probe and **discarded its own uncommitted work**, because the file held both the probe and real edits. Re-applied
from context, then **checkpointed BEFORE redoing the probe.** ⇒ **the confirming pass is told to verify the final
state is COMPLETE item-by-item — that is exactly the scenario where something goes missing silently.**

### ✅ G SIGNED OFF — AND THE FOLD THAT FIXED A FALSE ENUMERATION MADE A **FINER FALSE ENUMERATION ON THE SAME AXIS**

⭐ **MY `Recursive` ERROR IS CONFIRMED BY MECHANISM, NOT ONLY BY MEASUREMENT.** `--emit-gir`: none of the seven
payloads is `Recursive` (all `Trivial(@…_free)`); a `Recursive` struct bare-binds to a **Ptr alias** — *a genuine
second live place, NOT accidentally green*; the custom-`Drop` contrast rejects. ⭐⭐ **And the WHY:
`is_drop_tainted` is seeded EXACTLY from `equip T with Drop` plus a transitive field-graph fixpoint, and the
reject reads THAT TYPED FLAG AND NEVER CONSULTS `CopySemantics`.** ⇒ *"specific to a custom `Drop`" is right for
the right reason* — **which is exactly what I could not have known by reading, because the two are set one line
apart inside a single `if needs_upgrade`.**

⛔⛔ **ERRATUM 2 IS THIRD-ORDER AND IT IS THE ONE TO INTERNALISE: THE ROWS ARE ENUMERATED OVER `DropStrategy` BUT
ARGUED FROM `CopySemantics`, AND THE TWO AXES DO NOT ALIGN.** The independent witness (`src/ir/types.rs`) shows
**SIX legal `(CopySemantics, DropStrategy)` pairs**. The `Trivial` row covers only `(Resource, Trivial)` —
**`(Trivial, Trivial)` (`Shared`/`Weak`/`Channel`) is UNNAMED.** And ⛔ **the `None` row's REASON is false for
half of what it covers**: it argues *"a Copy bind DUPLICATES the storage ⇒ no subject"*, but `None` also covers
**`(Resource, None)` — `Thread[T]`/`Process` — where a bind does NOT duplicate storage.** **Measured:
`Process c = proc` checks clean, rc 0** ⇒ **a second live place demonstrably exists**, and the enumeration has
**no row for that third disposition.**
⇒ ⭐⭐⭐ ***THE FOLD THAT FIXED A FALSE ENUMERATION PRODUCED A FINER FALSE ENUMERATION ON THE AXIS IT WAS
REWRITING.*** **Fix: present the SIX PAIRS with a disposition per row.** *Enumerate over the product, not over
one factor — the round's recurring lesson, now at its third depth.*

⊕ **ERRATUM 1 — THE GUARD ADDED TO ANSWER SIX-Q #2 FAILS SIX-Q #2.** The new control-row lint is coupled to the
generator by a **hand-copied literal nothing cross-checks** (4 hits in `tests/lints.rs`, **0 in the generator**;
the cell name is a literal independent of the `CONTROL_*` triple that derives its source). ⇒ **re-point and
regenerate ⇒ `--check` green, `DECLARED_CONTROLS` green, and the guard silently asserts the OLD row.**
**One-line close: the control row's `note` column already carries the true source name — assert it.**

⊕ **ERRATUM 4 IS MINE:** I wrote *"the same two lines set `Custom` and `CopySemantics::Resource`"* — **it is the
line ABOVE**, and **the grep I cited returns THREE lines, not two** (one is a read), so **it does not regenerate
its own claim without a filter.** *Core #15(a), in a sentence I wrote to enforce Core #15(a).*
⊕ **ERRATUM 3:** the 13-SANITIZE-FAIL census **does not name its lane** (it is the asan lane's 21 of 63 writes)
**and is topic-11 ONLY — topics 07/10/13 were never seeded, so 13 is a LOWER BOUND on the triage.**

### 🔎 ARCHAEOLOGY (owner-requested) — THE GENERIC `[]` WAS **NEVER DISCUSSED. IT WAS IMPLEMENTED.**

⭐⭐ **BORN 2026-02-09 IN A CODEGEN COMMIT**, `31fead734` *"Phase 8: generics codegen & monomorphization"* —
whose own message says *"…**with backtracking to disambiguate from indexing**."* **The tie-break it wrote is
still running today**, verbatim in the comment it added:
> *"If the next token after `]` is `(`, it's a generic call. Otherwise, restore and parse as index."*
⇒ ⛔ **THE CURRENT RULE IS A PURELY SYNTACTIC BIAS TOWARD THE GENERIC READING, CHOSEN SO MONOMORPHIZATION COULD
SHIP.** That is why `d[k]` indexes and `d[k](v)` does not: **the `(` flips it.**

**WHAT THE RECORD CONTAINS — regenerated:**
- **ZERO D-numbered entries decide generic bracket syntax.** `grep -ciE '^- 20[0-9-]+ — .*generic'
  docs/define-gorget/decisions.md` → **0**; the word appears **10** times in the whole ledger, all incidental.
- `docs/language-design.md` §4.3 states it as a **FACT, NOT A RATIONALE**: *"Square brackets `[]` for type
  parameters."* **One sentence; the collision with subscripting is never mentioned.**
- The ledger's own **SYNTAX-BUDGET directive (added 2026-07-18)** — any syntax-adding decision must state
  **(a)** the full sigil inventory, **(b)** the newcomer cost, **(c)** collisions/near-collisions — **postdates
  the generic `[]` by five months. It never got one.**

⭐⭐⭐ **AND THE SHARP PART: D38/D39 APPLIED A STRICT AMBIGUITY STANDARD TO `[]` ON *VALUES* AND NEVER LOOKED AT
`[]` ACROSS *KINDS*.** D38 rejected `s[i]` **on ambiguity grounds**; D39 re-affirmed it, ratified
***"`[]` takes the collection's lookup key"***, recorded that ***"the ambiguity is the durable objection"***, and
rejected a coherent `[]`/`{}` split on **syntax-budget** grounds. ⇒ ***the ledger refused a second meaning for
`[]` on values while `expr[...]` had carried two meanings across KINDS since February — unruled, unbudgeted, and
invisible to the very decisions policing that operator.***

⇒ ⭐ **CONSEQUENCE FOR THE OPEN QUESTION: there is NO prior ruling to reconcile with — only an IMPLEMENTATION
DEFAULT to ratify or replace.** ⛔ **And by the ledger's own 2026-07-18 directive, deciding it now OWES A
SYNTAX-BUDGET SECTION** — the inventory, the newcomer cost, and the collision list. **That is the document that
was never written, and it is the deliverable that should accompany the ruling.**
⊕ *Incidental: the project was called **Vyper** as late as Phase 2 (`f13ca0d61`).*

### ⛔⛔⛔ D0′ v3 PASS 1 — **THE FACT I GAVE THE OWNER TO JUSTIFY THE RULING IS FALSE AS STATED**

⭐⭐⭐ **I TOLD THE OWNER: *"across all 4762 ambiguous sites, NO HEAD IS EVER A VALUE"* — AND OFFERED IT AS THE
MEASUREMENT THAT MAKES THE RULING SAFE. IT IS LITERALLY FALSE:**
`grep -P '\thead=Ident\(fs\)\t' <census>` → `tests/fixtures/known_gaps/indexed_callee_variable_index.gg`,
**where `fs` is a `Vector`.** ⛔ **AND THE INSTRUMENT COULD NEVER HAVE ESTABLISHED IT: the census is a
PARSE-TIME eprintln recording the head's SPELLING — structurally incapable of seeing what a head RESOLVES TO.**
The classification of the other 230 head names into *"type or generic fn"* was done **by eyeballing**.
⇒ **readiness row 2's "independent witness" does not cover the claim it was offered for.**
⭐ **THE REAL SAFETY EVIDENCE IS A DIFFERENT MEASUREMENT, AND THE REVIEWER HAD TO BUILD IT:** a fire counter at
the rewrite ⇒ **0 fires and 0 behavioural diffs across 4909 files.** **That is still good evidence — but it is
NOT the sentence I gave the owner, and `|changed cells|` in the corpus is therefore 0.**
⊕ Figures drifted too: **4894→4909 files, 4762→4776 sites** (1167 regenerates exactly).

🚨 **AND IT FOUND A CRITICAL — FILED AS `t1393`.** `vv[0][n](7)` and `(r.routes)[k](5)`: **`gg check` OK,
`gg build` rc 0, then SIGSEGV on BOTH backends.** ⇒ **MEMORY-UNSAFE FROM ORDINARY SAFE SYNTAX**, ranking above
every leak in this family. ⊕ **A third face fails even later:** a struct-field dict with a variable key checks
**`OK`** — with a tell-tale `warning: unused variable 'k'`, *the key eaten as a TYPE* — then fails at **LINK**
time with `undefined reference`. ⚠ **The brief said that site fails `E_NoMethodFound`; measured, it SILENTLY
ACCEPTS. My filing instruction was less severe than the truth.**

⛔ **BLOCKING 2 — THE PROTOTYPE EMBEDS THE EXACT SHORTCUT THE RULING SECTION FORBIDS, AND IT IS REACHABLE.**
`Expr::FieldAccess { .. } | Expr::Index { .. } => true` **consults no resolution at all**; its licence is a
COMMENT arguing from corpus absence. **Both arms FIRE** (fire counter, two spans). ⇒ **it is the SOURCE of the
two SIGSEGV cells being fixed** — so the fix currently **trades a crash for a shape heuristic**, under a brief
whose own ruling says *"the discriminator is what the head RESOLVES to, never how it is SPELLED."*

⛔ **BLOCKING 3 — MY ggdef INSTRUCTION CANNOT ACHIEVE WHAT IT CLAIMS, AND I ASSERTED IT AFTER TELLING THE
EXECUTOR NOT TO.** I wrote *"ggdef DOES NOT ABSTAIN AND MUST BE MIRRORED"* — **the reviewer RAN it**:
`elaborate_call` hard-requires an `Identifier` callee, so **a mirrored rewrite produces an `Expr::Index` callee
that ggdef rejects as OUT-OF-SUBSET.** ⇒ ***the mirror converts a wrong answer into an out-of-subset error; it
CANNOT produce lane agreement.*** Correct disposition is Core #9's own escape clause — **note + a FILED SUBSET
GAP**, plus an explicit statement that **no ggdef conformance fixture can pin the accept.** ⚠ **I wrote *"DO NOT
ASSERT ggdef's BEHAVIOUR — RUN IT"* in the same brief and then asserted it two lines later.**

⛔ **BLOCKING 4 — I PRESENTED THE LEAK AS THE WIN.** The headline yield `deep_clone=0 **closure_free=0**
shallow_memcpy=0` — ⛔ **`closure_free=0` IS THE LEAK.** Measured: the newly-accepted program leaks **8 B** under
ASan; the **bind** control is **CLEAN**; the already-accepted literal-index sibling **also leaks**. So the leak
is pre-existing and the fix **extends the accept set INTO it** — defensible as parity, **but round close runs
`sanitize_sweep.sh` over EVERY top-level `*.gg`, `sanitize_allowlists_shrink_only` is `==`, and its own header
says genuinely NEW inflow REMAINS AN OWNER ASK — while the brief forbids touching `elem_drop`, the fix.**
**The brief mentions the sanitizer ZERO times.** ⇒ **decide before launch, not at round close.**

⭐ **E2 — AND THE REVIEWER DISAGREED WITH ME ON MEASUREMENT, CORRECTLY.** I told the executor to fix `t0957`'s
repro and file the f-string blindness separately. **The fix is ONE LINE in the function the track is already
editing** — its sibling walker `rename_expr` **already recurses into interpolations** (textbook Core #4 sibling
drift) — and the blast radius is measured: **exactly ONE row changes across 4909 files, the track's own repro
going GREEN AS WRITTEN.** ⇒ ***"rewriting the repro to dodge a defect you could close in the same function, in
the same round, reads as REDESIGN AROUND A COMPILER GAP."*** **Folding it in.**

⊕ **E1 — *"91 lines, parser untouched"* is FALSE:** 24 of the 91 are an **env-gated debug instrument in
`src/parser/expr.rs`** that must be stripped, and the brief says to apply the patch verbatim. ⊕ **`ckpt2` and
`ckpt3` are BYTE-IDENTICAL — the "final" checkpoint changed nothing.**
⊕ **E4 — self-host: right count, WRONG PLACE.** Three `parser.gg` copies ✅ — **but the design is NOT a parser
fix, and the parser driver has no `resolve.gg`/`scope.gg` at all**, so a resolution-aware disambiguation cannot
live there. **The real home is `resolve.gg`, which has TWO copies.** *"Fix primitives in ALL parser copies" was
unactionable as written.*
⊕ **E5 — filing #4 has no discriminator** (overlaps `t0949`/`t0948`, and `t0948`'s measured figure is the same
16 B I quoted). ⊕ **E6 — filing #5 and the ID block check out.**

⭐ **WHAT PASS 1 CONFIRMED:** the prototype applies clean, `--lib` 1187/0, both cells work, **the
whole-file-grep instrument warning is TRUE and matters** (constant at 3 across all three cells; body-sliced, the
fixed cell is **byte-identical to the already-working literal-index cell**), the 10 non-`Identifier` heads are
real and **double-protected**, and an adversarial generic-call fixture shows **0 diff lines** HEAD vs prototype.

### ⭐⭐⭐ C1 RESOLVED — *"IT SHOULD WORK"* WAS RIGHT, AND HEAD WAS ONE `return` FROM CORRUPTION

**Q1 — HEAD ALIASES. BOTH CELLS ARE ACCIDENTALLY CORRECT (SIX-Q #6).** From the emitted C: `__v8 = *(Box__…*)`
is a **shallow load of the raw box pointer**; `*outer` and `h.b` are **ONE ALLOCATION**. It survives only
because **TWO BUGS CANCEL** — the scope-exit drop is the *shallow* free, and the recursive
`Box__Box__GorgetString__drop` is **emitted but NEVER CALLED** (`t1309`).
🚨 **AND THE ALIASING HAS TEETH, MEASURED:** make `outer` escape so `h` dies first ⇒ **ASan
heap-use-after-free.** ⇒ ⛔ **HEAD IS NOT A PROGRAM WORTH PRESERVING — IT IS ONE `return` FROM MEMORY
CORRUPTION.**

**Q2 — BOTH PROMISED RECOURSES EXIST AND BOTH ARE SILENTLY INERT.** `^h.b` → `E_PartialMove` rc 1.
⛔⛔ **`h.b.clone()` → rc 0, builds, runs — AND EMITS BYTE-IDENTICAL C.** An explicit `.clone()` on a `Box`
**EVAPORATES INTO THE IDENTITY** at the `.clone()` arm's terminal fall-through
(*"Non-resource type: .clone() is a trivial copy … return recv"*). ⇒ **A CORE #10 LOWER-OR-REJECT BREACH:
not a missing feature — a SILENTLY WRONG one.**

⭐ **AND THE RATIFIED RECORD SAYS BOX SHOULD CLONE.** D4: *"Explicit `.clone()` stays legal"*; D53 scopes
*"must not grow one"* **specifically to Mutex/RWLock**, for a reason that does not apply to Box;
*"Box has no orphaned invariant"*; and `ensure_shared_type_def`'s OWN comment lists the exclusions as
*"Mutex/RWLock/Guard"* — **Box is ABSENT from it.** ⇒ **the missing `clone_fn` is an OVERSIGHT, NOT A POLICY.**

**Q3/Q5 — ⭐⭐ THE ANSWER: `Box[T]` GETS A `clone_fn`, AND IT SHIPS AS *ONE* CHANGE WITH C1.** Prototyped
end-to-end; **both cells work UNCHANGED — no user-visible spelling change** — because the ownership chokepoint
can now **materialize** the borrow instead of refusing. `t0011`'s double-free repro **rc 134 → rc 0**.
⛔ **MA-5 applied: the halves DO NOT COMPOSE, they only work TOGETHER.** `clone_fn` alone leaves `t0011` at
**rc 134**; C1 alone is rc 101 with nothing to materialize.
⇒ ⭐⭐⭐ ***C1 IS NOT "A FIX WITH A REGRESSION TO PRICE". C1 IS HALF OF ONE FIX.*** The rc 101 was **C1 correctly
refusing to mint from a borrow it cannot materialize** — and the reason it cannot is the clone the ratified rule
already promises. **Honest cost: a real allocation the aliasing path never paid — the correct CoW price, and
what an expert would hand-write.** Residual **73 B / 2 allocs** = `t1309`'s pre-existing gap ⇒ **severity moves
CORRUPTION → LEAK.**

⛔⛔ **AND A CORRECTION I OWE: MY RUST REFRAMING WAS SIX-Q #1, NOT A FINDING.** I reported *"Rust ALLOWS the
partial move ⇒ Gorget's `E_PartialMove` is unconditional"* as evidence of over-strictness. **`E_PartialMove` IS
RATIFIED** — `grep -n '2026-09-02' docs/define-gorget/decisions.md` → **D10(a) ADDENDUM, owner 2026-09-02:
*"NO PARTIAL MOVES, NO UNPACK"***, which **explicitly closed** the *"Rust-style destructuring partial moves
remain a possible future widening"* sentence and closed `t0437`. ⇒ **the ledger considered Rust's exact shape
THREE DAYS EARLIER and refused it.** ⭐ **It does not weaken the case — it NARROWS it: with `^` closed by
ratification, `.clone()` is the ONLY ratified recourse, which makes the missing `clone_fn` the SOLE REMAINING
ROUTE the design leaves open.** Rust stays a valid Core #8 witness that a **safe program is rejected**.
⊕ **My `^s` hypothesis was also measured FALSE — a THIRD false recourse:** `Box.new(^s)` on a borrowed parameter
builds at HEAD but **still heap-use-after-frees when it escapes**; `^` on a borrow transfers nothing.

⊕ **TWO RESIDUALS + A BONUS THE EXECUTOR INHERITS:** a pre-existing **place-vs-value** defect unmasked
(`*(*f) = 60` then read gives `0` — the write lands in a clone because the `Deref` **value** arm takes a *place*
projection; **its own fixture is the independent witness — every pure-read cell passed, only the write-through
cell moved**) · a fixture asserting a now-retired refusal, to be re-pointed at the correct output · and
**`H__clone` deep-clones the Box but SHALLOW-COPIES the `Str` header**, so a cloned struct's boxed String
**shares its char buffer** — structural, not yet demonstrated by a failing program.

### ⛔⛔ MY FOLD PRESCRIBED THE **OPPOSITE DISPOSITION** — AND THE EXECUTOR MEASURED IT RATHER THAN OBEYING

**`7f68508b0`.** I told it to write that the seven payloads cover *"`Trivial` and `Recursive`"*. ⛔ **MEASURED,
NEITHER HALF HOLDS:** no payload is `Recursive` (all seven are collections/`String`), **and — the part I had
exactly backwards — `Recursive` IS BARE-BINDABLE.** `Store t = s` for `struct Store: Vector[int] f` checks
**clean, rc 0**, *even though the upgrade scan sets `CopySemantics::Resource` right beside
`DropStrategy::Recursive`*.
⇒ ⭐⭐ **THE REJECTION IS SPECIFIC TO A CUSTOM `Drop`, NOT TO `Resource` IN GENERAL** ⇒ **`Recursive` is a REAL
WIDENING TARGET, not a no-subject cell — THE OPPOSITE DISPOSITION FROM WHAT I PRESCRIBED.**
⭐ **And the docstring now carries a *"MEASURED, NOT DEDUCED"* warning, because the natural SOURCE-READ gets that
row backwards** — which is exactly how I got it wrong: **I reasoned from `Resource` implying single-owner and
never built it.** *The fold rule I landed this morning — a fold may only assert what a command in that same fold
regenerates — broken by its own author, again, on a durable disclosure.*

### 🚨🚨 THE `t1385` "QUICK CLEAR" WOULD HAVE RATCHETED **THIRTEEN MEMORY-SAFETY FINDINGS** INTO THE BASELINE

⭐⭐⭐ **THE SINGLE MOST CONSEQUENTIAL MEASUREMENT OF THE ROUND.** The executor reproduced the clearing command
**and reverted byte-identically — nothing landed.** The 21 rows that command rewrites propose:
**13 `SANITIZE-FAIL` · 6 `BUILD-FAIL` · 1 `REJECTED` · 1 `WORKS`.**
⇒ ⛔ **RUNNING IT UNREVIEWED WOULD PIN THIRTEEN MEMORY-SAFETY FINDINGS IN ONE KEYSTROKE — the exact failure
`--accept-drift` EXISTS TO PREVENT.**
⚠⚠ **I DESCRIBED THIS LAST HEARTBEAT AS *"clears by command + a bucket review"*. THAT ORDERING WAS WRONG: THE
BUCKET REVIEW IS NOT A FOLLOW-UP, IT IS THE ENTIRE WORK, AND THE COMMAND IS THE DANGEROUS PART.** `t1385` now
carries the census as the reason.
⇒ ⛔ **ROUND-CLOSE CONSEQUENCE: the five-lane gate does NOT get cleared by a command this round.** Either the 13
buckets are triaged (a real track), or the legacy rows go into the sanctioned `LEGACY_UNBASELINED` mechanism —
**declared, not hidden.** ⚖ **That is a design call and it is NOT mine to make silently.**

⊕ **E2 quantified and it indicts an earlier check of MINE:** **489 legacy rows moved** by the sort — and the
executor's earlier *"0 pre-existing rows changed"* was **KEYED BY CELL NAME AND THEREFORE ORDER-BLIND.**
**Content was never at risk** (`comm` on full rows proved 0 removed), **but the instrument could not see the
axis it was asked about.** *Third instance this round of an instrument that cannot see its own class.*
⊕ **E3/E4 both RED-verified by the executor:** the control lint now asserts all five lane columns of its source
row read `WORKS`; `DECLARED_CONTROLS` is an **EXACT SET, not a floor**, so a row can no longer exempt itself by
writing `CONTROL` into column 3.

### ⛔⛔ THE ROUND-CLOSE BATTERY WAS **FALSE IN ITS OWN TEXT**, AND THE LINT THAT POLICES IT IS BLIND BY CONSTRUCTION

⭐⭐ **I CORRECTED THE COMMAND IN PLACE AND FILED THE GUARD (`t1406`, HIGH).** Three halves, each regenerated:
1. `grep -n 'robustness_map' AGENTS.md` — the battery prescribed **BARE** `python3 scripts/robustness_map.py`
   and called it *"five lanes (C · LLVM · self-host · ASan · ggdef)"*.
2. `grep -n 'default=' scripts/robustness_map.py | grep -i lane` — **`--lanes` defaults to `"c"`.**
   ⇒ ⛔ **THE BARE COMMAND RUNS ONE LANE. THE PROSE BESIDE IT WAS FALSE, FOR TWO ROUNDS.**
3. `grep -n 'robustness_map' .github/workflows/ci.yml` — CI runs **`--lanes c,llvm`** *and* **`--lanes all`**.
⇒ **CI's five-lane step is a GATE THE BATTERY DID NOT RUN.**

⛔⛔ **AND THE RECONCILING LINT REPORTS IT COVERED.** `grep -n "battery.contains(target)" tests/lints.rs` — the
coverage predicate is **SUBSTRING CONTAINMENT** over the battery text, so **a CI step counts as covered the
moment the battery mentions the same SCRIPT PATH, whatever ARGUMENTS either side passes.**
⭐⭐⭐ **THIS IS THE GUARD'S OWN SENTENCE FAILING.** `AGENTS.md` says local-green is the sign-off *"only for as
long as that lint holds it true"* — and records that the previous prose was **measured FALSE, three CI steps
absent, a round green while CI was red.** ⇒ ***the lint was written for exactly this class and cannot see this
instance.*** **SIX-Q #2, on the guard that guards the round.**

⛔ **THE FIX IS NOT "ADD A ROW"** — the predicate must compare what is **GATED**, not what is **SPELLED**.
⚠ **A fix that special-cases `--lanes` repeats the defect on the next flag: `GG_STAGING_MOVE_GUARD=fatal` and
`--release` are the same shape.** ⊕ **And its RED direction must be a CI step whose ARGUMENTS differ from the
battery's — shown going red, or the replacement is the same guard with a longer body.**

⚠⚠ **CONSEQUENCE, STATED PLAINLY: the battery is now HONEST AND THEREFORE RED.** `--lanes all` fails on `t1385`
(11 legacy divergences) + `t1387` (the ASan leak). **A red battery is NEVER waivable** ⇒ **R50 cannot close
until both are discharged.** ⭐ **Both are actionable: `t1385` clears by command + a bucket review; `t1387` has
NO accept path and must be FIXED.**

### 🚀 TRACK H OPENED — `t1387`, THE LEAK THAT CANNOT BE ACCEPTED

⛔⛔ **THE LOOP-HEAD MECHANISM BELOW IS FALSIFIED — IT IS THE OPENING HYPOTHESIS, KEPT FOR THE RECORD ONLY.** The real
write site is **`gorget_map_put`'s duplicate-key path, which ran `val_drop` six times and `key_drop` ZERO times**
(the executor's fire count: `total_frees` 60 → 200034, `live_bytes` 399948 → 0 — **one `free()` per duplicate
put**). **`DONE.md` carries the corrected mechanism.** Everything below this line predates that measurement.

**Scout launched, base `be3eb61b6`.** `ex_char_frequency.gg` leaks **2 bytes** via
`gorget_string_clone_to_owned` ← `str_alloc_copy` ← `main` — a clone made at a **`for ch in s:` LOOP HEAD** and
**never registered for drop.** **Reproduced independently by two agents.**
⭐ **Hypothesis is Core #3 verbatim** (*register ownership at the value's birth… the leak class is always a
missing or mis-typed ownership tag*) — **and the scout is told to PROVE IT FROM THE EMITTED C, not from reading
the lowering**, because this round has had three certifications falsified by source-reading where a build would
have settled it.
⭐⭐ **THE TWO QUESTIONS THAT DECIDE ITS SCOPE:** (a) **instance or CLASS?** `for ch in s:` over a `String` is an
extremely common shape — enumerate the sibling producers **with an INDEPENDENT witness**; (b) ⛔ **WHY DOES ONLY
THIS CELL SHOW IT?** 819 value-semantics cells + ~1000 legacy rows, and exactly one reds under ASan.
**That asymmetry is a clue, not noise (SIX-Q #6) — if the leak is broader, the map's ASan lane is
UNDER-REPORTING, and that is a second finding.**
⚠ **Crowded family: `t0045`/`t0403` (integrated this round) and `t1362`/`t0750` both touched for-element
ownership. The scout must say what DISCRIMINATES this, or that it is the same class.**

### ⭐⭐⭐ G's OUTPUT-REVIEW — THE RED FIVE-LANE GATE **IS CLEARABLE THIS ROUND**, BY A COMMAND ITS OWN ITEM SAYS DOES NOT EXIST

⭐⭐ **`t1385`'s CENTRAL MECHANISM CLAIM IS WRONG, AND THE REVIEWER MEASURED THE ALTERNATIVE END-TO-END.** The
item says *"`--seed-new` does not help either, for the same reason"* — **that reasoning covers only the
`first_seen` branch.** It misses the **per-lane SECOND seeding branch**, which fires regardless of `first_seen`;
and `diverges` is computed over VALUE lanes only, so ⭐ **a run measuring ONE value lane cannot produce a
divergence at all.** Measured:
```
--lanes selfhost,asan,ggdef --accept --seed-new --topic "11 closures"   ->  63 rows SEEDED, BARE_RC=0
--lanes all --topic "11 closures"                                        ->  8 divergences, 0 NOT in baseline, rc 0
```
⇒ **4 short runs (topics 07/10/11/13, 93 cell-lanes) + REVIEW THE PROPOSED BUCKETS + delete the 31 legacy
entries.** ⚠ **The item was RIGHT that the real work is the bucket review, not the flag.** ⛔ **AND THE LIMIT IS
NAMED: this works because all 11 divergences are BUCKET-VISIBLE; a future WRONG-vs-WRONG-DIFFERENT-VALUE row
would NOT clear this way.**
⛔ **`t1387` (the ASan leak) has NO ACCEPT PATH AT ALL and MUST BE FIXED** — `base` is non-empty `WORKS` ⇒ the
REGRESSION branch; `--accept` refuses regressions, `--seed-new` needs `not base`, `--accept-drift` needs
`base != good`. **Accepting it would pin a bug.** ⇒ **its own track — a drop-registration defect is a different
CLASS from a corpus track.**

⛔⛔ **AND A FACT THAT DECIDES WHETHER THE BATTERY IS EVEN GREEN: `AGENTS.md`'s round-close row prescribes BARE
`python3 scripts/robustness_map.py`, WHICH IS `--lanes c` (the default) AND IS GREEN — while the prose beside it
says "five lanes". CI runs `--lanes c,llvm` AND `--lanes all`, so CI's FIVE-LANE STEP IS RED.**
⇒ ⭐ **THE BATTERY IS GREEN AS WRITTEN AND RED IN SPIRIT.** ⚠ **`round_close_battery_covers_ci_steps` reconciles
the battery against CI and did NOT catch this — so it matches STEPS, not ARGUMENTS.** *That is the exact
sentence the lint exists to keep true, failing on a flag.* **ORCHESTRATOR'S CALL, and it is not waivable.**

### ⛔ G's BLOCKING FINDING IS **SIX-Q #4 IN THE SCOPE DISCLOSURE ITSELF**

The generator claims its seven payloads *"cover all four values"* of `DropStrategy`. **`Custom` is NOT covered —
zero cells carry a user `equip … with Drop`.** ⭐⭐ **And it is not a miscount: the same two lines that set
`Custom` also set `CopySemantics::Resource`, so the BARE VIEW BIND ALL 819 CELLS RELY ON IS REJECTED**
(`E_MoveWithoutOperator`). ⇒ ***the fourth value is A CELL WITH NO SUBJECT** — the rule "every other place reads
the value it had before" has no second place there, and NO WIDENING REACHES IT.* **Text-only fix; by the
track's own D11/D21 standard, a WRONG durable disclosure is worse than an incomplete one.**

⊕ **Three more Core #14 / SIX-Q #2 residues:** a comment stating that sorting the whole file *"would rewrite
rows this generator does not own"* — **on the line immediately above the code that sorts the whole file**, which
**did** reorder legacy rows (content provably preserved) · the control's *"drawn from a row green on every
lane"* invariant has **no enforcing guard**, so if that row regresses the control starts MATCHING and the map
reports *"harness is blind"* **for a compiler regression** · and ⭐ **the CONTROL carve-out is
SELF-AUTHORISING: a normal row can exempt itself from the empty-baseline guard by writing `CONTROL` into column
3**, with `control_rows >= 2` a **floor and no ceiling** — a cell broken on all five lanes would be silently
exempt AND green.

⭐ **CORE #8 AT SCALE, CONFIRMED BY BUILDING BOTH LANES:** 237 DIVERGENT rows, **all C/LLVM against a self-host
that is correct on all 819.** The reviewer's honest scoping: **narrower than *"the self-host is the better
compiler"*, stronger than an anecdote — 819 programs in ONE bug family where the self-host implements the
ratified §3.5/D52 answer and Rust gg does not on 237 of them, GATED rather than swept.** ⊕ **And the
subdirectory premise is FALSE HERE:** the map runs its OWN self-host lane over every cell, per-row and gated
⇒ **819/819 WORKS is a STRONGER same-round SH discharge than a top-level fixture would give.**

### ✅✅ RULED 2026-09-05 — **`d[k](v)` IS INDEX-THEN-CALL, BY *KIND*, AND SHADOWING IS A HARD ERROR**

> *"I think we should take A — resolve by kind."* … *"Agreed, and shadow-reject as a hard error."*

**THE RULE.** `expr[...](args)` resolves by **what the head NAMES**: a **type** or **generic function** ⇒
instantiation; a **value** ⇒ **index-then-call**. ⛔ **AND A VALUE THAT SHADOWS A GENERIC TYPE NAME IS A HARD
ERROR AT THE AMBIGUOUS SITE** — not innermost-wins.

⭐ **WHY THE SHADOW-REJECT, AND WHY IT IS FREE.** Ordinary shadowing changes *which value* you get; here it would
change **WHICH OPERATOR YOU ARE USING** — a reader could not tell construction from indexing without knowing the
whole scope, in a language whose pitch is Python-like readability. **Measured cost: ZERO.** No `.gg` file in the
tree shadows a generic type name with a value (the 35 apparent parameter hits are bare types in function-type
positions, classified by the type-first syntax rule — **an indication, not a proof; the executor re-measures**).
⚠ **`Vector[int] Vector = [1,2,3]` checks CLEAN at HEAD**, so this IS a real accept→reject change.

**WHAT THE RULING BUYS AND COSTS — recorded so it is not rediscovered:**
- ⭐ **0 rewrite fires, 0 behavioural diffs across 4909 files** ⇒ **no existing program changes meaning.**
- ⭐ **It closes `t1393`** — the silent-accept-then-SIGSEGV cells. **The status quo is the memory-unsafe option.**
- ⛔ **The grammar becomes RESOLUTION-DEPENDENT.** Already true of the `Call`→`StructLiteral` rewrite in the same
  pass, so not a new CLASS of cost — but real for tooling and error recovery.
- ⛔ **The self-host pays a price the alternatives would not:** its **parser** drivers have no `resolve.gg` or
  `scope.gg`, so a resolution-aware disambiguation **cannot live where the three parser copies are**. Its home is
  `resolve.gg` (**two** copies).
- ⛔ **ggdef CANNOT MIRROR IT INTO AGREEMENT** — `elaborate_call` hard-requires a named callee, so a mirror yields
  an **out-of-subset error, not lane agreement.** ⇒ Core #9's escape clause: **note + FILED SUBSET GAP**, and an
  explicit statement that **no ggdef conformance fixture can pin the accept.**
- ⛔ **The newly-accepted program LEAKS 8 B** (pre-existing, shared with the already-accepted literal-index
  sibling). **Round close sweeps every top-level fixture; price it before landing.**
- ⛔ **THE RULING OWES A SYNTAX-BUDGET SECTION** per the ledger's own 2026-07-18 directive — full sigil
  inventory, newcomer cost, collisions. **That document has never been written for `[]`, and its absence is how
  this survived from February.**
- ⚠ **The shadow-reject is an accept→reject surface change ⇒ Core #9 binds ALL THREE LANES with a conformance
  fixture for the FINAL state.**
⊕ **NOT taken, and recorded so it is not re-proposed:** dropping the 516 explicit-type-arg function sites to
"simplify". Inference covers the easy ones, but **constrained generics (`hash_of[Hashable T]`) are where explicit
args earn their keep**, and once the kind rule lands the ambiguity is already gone.
⛔ **`docs/define-gorget/decisions.md` IS OWNER-EDIT-ONLY — this entry is the authority until the owner records
it there.**

⛔⛔ **RETRACTION, QUOTING ITS OWN SCOPE.** I recorded *"RULED — `d[k](v)` MEANS INDEX-THEN-CALL (owner
2026-09-05)"* in the handover **and in commit `27d769b21`'s subject line**. **The owner then said: *"Wait, let
me understand the generic instantiation before ruling."*** ⇒ **the heading was PREMATURE: the owner's *"I
believe yes"* was a POSITION, and I promoted it to a RULING.** ⚠ ***What I retract is the RULED status ONLY —
every MEASUREMENT under it stands*** (the 4762-site census, no-head-is-a-value, the 10 non-Identifier heads,
the layer being `rewrite.rs`). **The track's brief v3 is written and under review; it must NOT land until the
owner rules.**

### ⭐ THE SWIFT ANSWER — AND IT REFRAMES THE QUESTION ENTIRELY

**Swift has NO SUCH AMBIGUITY, because Swift NEVER OVERLOADED `[]`.** Verified against the Swift book's
grammar: **`generic-argument-clause → < generic-argument-list >`** — Swift generics are **ANGLE brackets**;
`[]` in expression position is **exclusively SUBSCRIPTING** (`[T]`/`[K:V]` are type-position sugar only).
⇒ **in Swift, `d[k](v)` is UNAMBIGUOUSLY subscript-then-call. There is no competing reading to weigh.**
⊕ **And Swift has historically REFUSED explicit specialization at a call site altogether** — `f<Int>(x)` was
invalid; **SE-0460 "Explicit Specialization"** was under review in early 2025 to add it, still with `<>`.

⛔⛔ **SO "WE FOLLOW SWIFT THERE" DOES NOT HOLD FOR THIS CONSTRUCT, AND THAT IS THE FINDING.** Gorget spells
generics with **SQUARE** brackets (`docs/language-reference.md`: *"generic arguments in square brackets"*,
`grep -n 'square brackets' docs/language-reference.md`) — that is **PYTHON's** convention (`List[int]`), not
Swift's. **The repo's documented Swift debts are elsewhere**: CoW (as an explicit CONTRAST — Swift refcounts,
Gorget does not), dot-shorthand enum inference, uncatchable faults, `String` unification, assert-rewriting
(`grep -rn 'Swift' docs/language-design.md`).

⇒ ⭐⭐ **THE AMBIGUITY IS SELF-INFLICTED AND BOTH REFERENCE LANGUAGES AVOIDED IT DELIBERATELY.** Swift kept
`<>` and `[]` disjoint. **Rust made its turbofish `::<>` DELIBERATELY UGLY for exactly this reason** — to keep
`<` unambiguous at a call site. **Gorget took Python's `[]` for generics while also using `[]` for
subscripting, and inherited the collision neither reference has.**
⚠ **This does NOT decide the ruling** — it says the owner cannot import Swift's answer, because Swift never
faced the question. **The escape remains the MEASURED one: no head ever resolves to a value, so RESOLUTION
disambiguates.**



⭐ **THE OTHER INTERPRETATION IS GENERIC INSTANTIATION** — `d[k](v)` = call `d` with TYPE ARG `k` (Rust's
`d::<k>(v)`) — **and it is the one the parser picks today, for a good reason: it is the DOMINANT construct.**
Measured by the scout over 4894 `.gg` files: **4762 sites, 1167 with the ambiguous all-bare-name shape**;
heads are `Vector` **2829**, `Dict` **563**, … ⇒ **the parser is serving a real, heavily-used form; `d[k](v)`
merely falls into it because a bare identifier is a syntactically valid TYPE NAME.**

⭐⭐ **WHAT MAKES THE RULING SAFE IS A MEASUREMENT, NOT A PREFERENCE: across all 4762 sites NO HEAD IS EVER A
VALUE.** Every head is a type constructor or a generic free function. ⇒ **the two readings are DISJOINT AFTER
NAME RESOLUTION** — head resolves to a VALUE of collection type ⇒ index-then-call; head resolves to a TYPE or
generic fn ⇒ instantiation. **Nothing is given up.**

⛔ **AND THAT IS EXACTLY WHY IT CANNOT BE A PARSER FIX: the parser does not know what `d` IS.** It belongs
after resolution — `src/semantic/rewrite.rs`, which already disambiguates the SAME class
(`Expr::Call`→`Expr::StructLiteral`).

⛔ **THE SHORTCUT THAT LOOKS RIGHT AND IS WRONG:** *"non-`Identifier` head ⇒ indexing"* — **10 sites have a
non-Identifier head** (`None[String](...)` in the self-host parsers) **and it would break them.** ⇒ **the
discriminator is WHAT THE HEAD RESOLVES TO, never how it is SPELLED.** *(Core #2, at the grammar.)*
⊕ **No third reading:** the colon-slice `v[a:b]` (D22) is a distinct grammar and single-index `d[k]` does not
collide with it.

### ⭐⭐⭐ C2 FINALLY FIXED THE **INSTRUMENT**, NOT THE HITS — AND IT FOUND 2 MORE THAN I DID

**`635d98496`.** After three generations of string-bounded sets, the executor built a **CONSTRUCT-SCOPED**
scanner (`/tmp/recover_execC2_affaa7f6_construct_scan.py`, described in full in the commit message because
`/tmp` is not durable): anchor on the CONSTRUCT (a bare `for X in Y`, or one of its NAMES) · prose positions
**repo-wide, every file type** · a **±4-line window** *(because the description routinely sits on an adjacent
line — **both blockers did**)* · flag any window carrying a retired-concept word, where that vocabulary is the
**CLOSED COMPLEMENT of the ratified rule**: if the binding is a mutable private copy then *immutable /
read-only / const / frozen / unmodifiable / "cannot be modified"* are all wrong **however spelled**.
⇒ ⭐ **THAT LAST CLAUSE IS WHAT STOPS GEN 3 RECURRING** — it bounds by MEANING, not by string.

**85 candidates, each read and dispositioned. It found 2 sites BEYOND the 2 I reported** — including
`src/ir/lowering/stmts/for_loops.rs`, **the compiler's own doc comment**, saying *"(§3.1 immutable
for-element)"* — **attributing the retired label to the section that retired it.**
⭐ **AND IT STATED ITS BLIND SPOT INSTEAD OF CLAIMING TOTALITY (Core #13):** it cannot see a site that describes
the construct without naming it in-window, nor a retired concept outside that vocabulary. ***"Strictly better
than a phrase-grep — not a proof of totality, and I'm not claiming one."***

⊕ **My judgment call came back MEASURED, not argued:** `docs/book/12-borrowing.md:189` —
`String x = v.get(0).unwrap(); x = "zz"` prints `zz`, collection still `aa` ⇒ **the same looseness at a position
the ledger never ruled on.** Out of scope, filed **`t1336`** as an owner/ledger question. ⛔ **Correcting one of
the three identical sentences alone would manufacture exactly the inconsistency this track spent three commits
removing.**

### ✅ F1r IS SIGNED OFF — AND THE REVIEW HANDED ME THE MERGED-TREE CONSTANT MAP

**Both refusals CORRECT on the merits**, re-verified: the parity floor is `matched.len() >= FLOOR` **and its own
comment says raising it is a ROUND-CLOSE action**; the clone pins say *"a TRACK must never write a pin. The pin
now has exactly one writer, the integrating parent."* ⚠ **The rule text is at `:35059`, not the cited `:35032`**
— and **`grep "a TRACK must never write a pin"` returns NOTHING because the phrase WRAPS across two comment
lines.** Use `grep -n "never write a pin"`. *(Core #15(a), demonstrated by the reviewer needing it.)*

⭐ **THE PARTIAL-REVERT MATRIX IS EXACT, AND L1≡L2 WAS ESTABLISHED TWO WAYS** — structurally (the pair composes
as a logical AND) **and empirically (the row SETS are byte-identical, not merely equal in cardinality)**.
⚠ **That distinction is the whole point: equal counts are also consistent with a MIS-LANDED break.**
**20 ∪ 11 = 31 = exactly the PRE-wrong set; intersection 0; the 3 straight-line twins green under all four.**

⭐ **SIX-Q #2 ANSWERED HONESTLY BY THE REVIEWER'S OWN PROBE:** it applied the real trap and found **guard row 1
stays at 19 — BLIND** (the `save_locals` lives inside the callee) while **row 2 goes 2 → 3 — RED.**
⇒ *"row 1 cannot catch its own class's known trap, which is exactly why row 2 exists."*

⛔ **TWO ERRATA I OWE:**
- **E1 — a Core #14 defect:** the predicate's docstring says Cases 4 **and 6** *"take a boundary-safe route"*.
  **True of 4, FALSE of 6** — Case 6 calls `cow_materialize_alias`, the SAME rebinding materializer as Cases 1
  and 2. **The DECISION is sound (probed: prints the pre-mutation value on both PRE and POST)** — but the
  recorded MECHANISM is wrong *in exactly the way that stops the next editor from re-measuring.*
- **E2 — `t1365` names the four deferred constants but NOT their `figures.db` MIRRORS**, every one of which
  `figures_db_mirrors_agree` enforces. ⇒ ⛔ **`t1365` is the record ROUND CLOSE INHERITS: without the mirrors
  the PARENT moves a constant and REDS the lint.** Map given (`grep -n "\.home = " scripts/figures.db`).

### ▸ THE MERGED-TREE CONSTANT PASS — ALL FIFTEEN ARE **PER-FIXTURE**, SO THEY MOVE **TWICE**

**Per new `spectests/run/*.gg` (C2 1 + F1r 1 ⇒ +2 each):** `MIN_FIXTURES` (EXACT PIN) · `C_MATCH_FLOOR` ·
`LLVM_MATCH_FLOOR` (both `const_assert`ed `== MIN`) · `SELFHOST_MATCH_FLOOR` (`<= MIN`; +1 **only if the seed
MATCHes on SH**) · the four `figures.db` mirrors · **`GGDEF_MATCH_FLOOR`** (+1 per in-subset MATCHing seed).
⚠ **`GGDEF_SKIP_CEILING` is SHRINK-ONLY AT ZERO SLACK** — an out-of-subset seed becomes a SKIP and REDS
`-p ggdef`. ⭐ **CHECKED: BOTH seeds carry `adjudicator: ggdef` and BOTH tracks measured `GGDEF-SKIP=18`
unchanged with MATCH 225→226 each** ⇒ merged expectation MATCH **227**, SKIP **18**. ⛔ **STILL RE-MEASURED AT
THE MERGED TREE, NEVER DERIVED (Core #5).**
**Per new top-level `tests/fixtures/*.gg` matching `cow_`/`deadwrite_`/`combinator_` (F1r 3):** `corpus_b.rs`
(exact) · `corpus_b1.rs` (exact) · the two cited `EXCLUDE` arrays · one census row each in
`GGDEF_CORPUS_B{,1}_CENSUS`.
**PER-ROUND, mine, once:** `RUNTIME_DIFF_MATCH_FLOOR` · `GGDEF_ADJUDICATED_FLOOR` ·
`SELF_COMPILE_{ARRAY,STRING}_CLONE_PIN` + their `_ROUND_OPEN` anchors · **plus
`RUNTIME_DIFF_NONMATCH_CEILING`, which is NOT a constant to bump but an OBLIGATION TO DISCHARGE.**
⭐ **F1r's three top-level fixtures were self-host-verified BY HAND and all three MATCH** ⇒ **the ceiling cannot
grow from that inflow.**

### ⭐⭐ D0′ v3 SCOUT — THE HYPOTHESIS IS **HALF RIGHT, AND THE WRONG HALF REVERSES THE ORDER**

**RIGHT:** `d[k](v)` is fixable and **the zero-clone claim is TRUE — MEASURED, not asserted.** `callee_var`
goes EMIT_FAIL → emits C **byte-identical to the already-working `callee_lit`** but for two filename strings;
`deep_clone=0 closure_free=0 shallow_memcpy=0`. **The bind spelling costs an extra closure slot + memcpy +
free.** Unchanged cells: **0 diff lines vs HEAD.**
⚠⚠ **AND THE INSTRUMENT WARNING IS THE ROUND'S THEME AGAIN:** `grep -c gorget_closure_clone_to_owned` over the
whole file is **CONSTANT AT 3 FOR EVERY CELL** — it counts runtime boilerplate and **would have reported the fix
as doing nothing.** It must be sliced to the function body. *(Core #13: the instrument must SEE the class.)*

**WRONG — AND I NAMED THE LAYER WRONG:** ⛔ **IT IS NOT A PARSER FIX AND CANNOT BE.** The parser genuinely
cannot decide whether `k` is a value or a type. **But `Type::Named{name, generic_args:[]}` LOSSLESSLY ENCODES
`Expr::Identifier`, so the AST is NOT lossy** — disambiguating downstream is **Layering rule 4 (resolve once,
write through), not a read-site patch.** ⭐ **And the precedent already exists:** `src/semantic/rewrite.rs` is a
post-resolution pass that ALREADY converts `Expr::Call`→`Expr::StructLiteral` on exactly this class of parse
ambiguity. **The fix goes there: 91 lines, parser untouched.**
Proto: `/tmp/scoutD0v3_a4799cbf/ckpt3_final_proto.patch`.

⛔⛔ **TWO FINDINGS THAT BREAK MY PROPOSED ORDER OUTRIGHT:**
1. **THERE IS A SECOND AMBIGUITY SITE I MISSED.** `expr.field[...](` is a **different parser branch**
   (`src/parser/expr.rs:1057-1071`) producing a `MethodCall` and failing **`E_NoMethodFound`**, not
   `E_NotAFunction`. ⇒ **ALL 8 httpserver sites are `self.X[k]`/`sub.X[k]`, so ALL 8 need THAT site** — which
   needs the receiver's TYPE, so `rewrite.rs` cannot reach it. **The prototype does not fix them.**
2. ⛔ **THE MIGRATION WOULD MAKE `httpserver` WORSE.** At HEAD the **bind** spelling is CLEAN and the **callee**
   spelling **LEAKS 16 B**. ⇒ ***migrating converts clean sites into leaking ones*** unless `elem_drop` lands the
   same round. **My "parser first → migrate → reject" order is dead as stated.**

⛔⛔⛔ **AND THE SAME CERTIFICATION MISTAKE, A THIRD TIME — THIS TIME IN MY OWN BRIEF.** I called `gg_full`
*"the one measured configuration that is both leak-free and memory-safe."* **Run as a genuine product
(18 cells × 4 oracles, container × read-shape × PROVENANCE), `gg_full` STILL LEAKS 16 B in 4 cells on the
STRUCT-FIELD provenance** — the provenance the certifying matrix omitted. ***A claim certified on a matrix that
omits the axis where it fails.***

🚨 **A THIRD, INDEPENDENT DEFECT, AND IT SITS ON THE `httpserver` PATH:** a container reached **through a struct
field** clones the closure env and **never registers the clone for drop** (`deep_clone=1, closure_free=0`).
**Present identically at PURE HEAD** ⇒ caused by no prototype, and **neither read-site nor write-site
`elem_drop` touches it.**

⭐ **THE ENUMERATION HAS A REAL INDEPENDENT WITNESS — THE PARSER ITSELF.** Instrumented the branch and swept
**4894 `.gg` files**: **4762 sites, 1167 with all-bare-name type args** (the ambiguous shape). **Every head is a
type constructor or a generic free function — NO head is a value**, which is why the fix fires only on the
current error path. ⊕ **10 sites have a non-Identifier head** (`None[String](...)` in the self-host parsers)
⇒ **a "non-Identifier ⇒ index" rule would BREAK them.** Blast radius: **1187/1187 unit, 1723 fixtures, 0
failures.**

⊕ **TWO PRE-EXISTING DEFECTS FOUND IN PASSING — and the second is deliciously self-defeating:**
`rewrite_expr` **never walks f-string interpolations** (`Expr::StringLiteral` sits in its LEAF list,
`src/semantic/rewrite.rs:930`) ⇒ at pure HEAD `print(getx(P(7)))` prints 7 while **`print(f"{getx(P(7))}")`
fails with a RAW C ERROR**. ⇒ ⛔ **`t0957`'s OWN COMMITTED REPRO is written in the one position where the
rewrite layer is BLIND** (`f"{fs[n](21)}"`), so it still fails under the prototype while the plain form prints
42. **A repro that cannot pass the fix it was filed for.**

⭐ **SHIPPABLE ALONE, AND IT SHOULD SHIP ALONE — WITHOUT the migration.** It only turns ERRORS into
ACCEPTANCES; no accepted program changes. ⚠ **But it IS a semantic change (Core #9): ggdef reuses the root
parser but NOT `src/semantic/rewrite.rs`, so it needs a ggdef mirror in `elaborate/`; self-host has 3 parser
copies.**

### ⛔⛔⛔ C2's DOC ROT, GENERATION **THREE** — AND I CAUGHT IT BY NOT TRUSTING A TOTAL CLAIM

⭐⭐ **THE CLAIM WAS *"post-fix sweep for the label is now ZERO HITS REPO-WIDE"*. TRUE FOR THE PHRASE, FALSE FOR
THE CLASS — falsified in ONE grep**
(`grep -nE 'element is immutable|bare element immutable' tests/integration.rs`):

```
/// CoW Track 1A — bare `for c in a` element is immutable (materialize control).
#[test]
fn cow_for_bare_vector_control() {
```
⇒ ⛔ **THE EXECUTOR FIXED THE FIXTURE'S HEADER — which now correctly reads *"a MUTABLE PRIVATE COPY (§3.1)"* —
AND LEFT THE DOC COMMENT THAT DOCUMENTS THAT VERY FIXTURE SAYING THE OPPOSITE**, same round, same construct.
⊕ **A second hit is self-contradictory INSIDE ONE SENTENCE:** *"§3.1 makes the bare element **immutable**, so
the write **lands in a private copy**"* — **if it were immutable the write would be REJECTED, not land**, and
the fixture proves it is ACCEPTED. **It cites §3.1, the section that says the opposite.**

⭐⭐⭐ **THE THREE GENERATIONS ARE ONE LESSON, AND IT IS ABOUT THE INSTRUMENT, NOT THE DILIGENCE:**
1. enumerated citation **SITES** naming a deleted symbol — never behaviour **CLAIMS**;
2. the replacement grep scoped to **`docs/`** — the README survived;
3. the repo-wide grep scoped to **ONE PHRASE** — a **different spelling of the same retired concept** survived.
⇒ ⛔ **A SET DEFINED BY A STRING IS NEVER TOTAL. DEFINE IT BY THE CONSTRUCT** — *every site that describes what
the bare `for x in coll` element binding IS* — and **name the instrument that BOUNDS it (Core #13), because a
phrase-grep FINDS candidates and cannot BOUND a class.**

⚠⚠ **AND THE SAME BINDS ME: MY OWN SEARCH WAS ALSO PHRASE-SCOPED.** I grepped three spellings. **I am not
claiming I found them all, and I told the executor not to claim it either.** *The failure is not that anyone
searched carelessly — it is that a phrase is the wrong KIND of boundary for this set.*

⊕ **ONE JUDGMENT CALL HANDED BACK RATHER THAN DECIDED:** `docs/book/12-borrowing.md:189` — *"Reading an element
from a collection returns a **read-only borrow**"* — is very likely a DIFFERENT construct (a `.get()` bind, which
`tests/fixtures/cow_p3_readonly_borrow.gg` treats as its own position). **Close enough to need a STATED
disposition rather than silence.**

⭐ **THE PROCESS POINT: I VERIFIED A TOTAL CLAIM MYSELF INSTEAD OF SPAWNING A THIRD REVIEW OF A DOCS DELTA.**
Two greps, no agent, and it was the right call — **the base rate of defects in this track's folds is what
justified the check, not suspicion of the executor.**

### ✅ C1's OWNER ASK IS **WITHDRAWN** — THE OWNER REJECTED THE QUESTION, NOT THE ANSWER (2026-09-05)

> *"I think the reference grade option would be to understand why under C1: gg build rc 101. It should work,
> shouldn't it?"*

⛔⛔ **RETRACTION, AND IT QUOTES ITS OWN SCOPE.** What I retract is exactly this: the block below offers
**"(a) SHIP IT / (b) SHIP IT + a CHECK-TIME DIAGNOSTIC / (c) NARROW C1"** and calls the choice *"genuinely the
owner's"*. ⇒ **ALL THREE PRESUPPOSE THE PROGRAM STOPS COMPILING. I COSTED THREE WAYS OF ACCEPTING A FAILURE
WITHOUT ASKING WHY IT FAILS.** ⭐ **The MEASUREMENTS below all stand** — the `rc 101`, the three closed
recourses, the `139`-was-false correction, the second parameter cell. **Only the FRAMING is withdrawn.**

⭐ **AND THE RATIFIED TEXT SAYS THE OWNER IS RIGHT TO PUSH.** `AGENTS.md`'s carve-out
(`grep -n 'single-owner-by-design' -A 6 AGENTS.md`) makes `Box[T]` single-owner with no implicit-copy path and
fires `E_MoveWithoutOperator` **at constructor sites** — so rejecting the BARE form is ratified-CORRECT. ⛔ **But
the SAME SENTENCE promises two recourses — *"require the user to write `^source` or `source.clone()`"* — AND
BOTH ARE UNAVAILABLE:** `^h.b` → `E_PartialMove` (a partial move out of a live struct, correctly rejected) and
**`Box[T]` has NO `clone_fn`.** ⇒ ⭐⭐ **THE HOLE IS NOT THE REJECT. IT IS THAT THE RECOURSE THE RATIFIED RULE
NAMES DOES NOT EXIST** — which makes this a LANGUAGE hole, not a property of the program.

⭐ **THE QUESTION THAT DECIDES THE TRACK, AND IT IS SIX-Q #6:** at HEAD cell 1 **prints `hi`, rc 0, ASan
silent**. **Does HEAD DEEP-COPY the `Box`, or ALIAS `h.b`'s pointee?**
- **Deep-copies** ⇒ **the capability EXISTS and C1 REMOVES it** ⇒ the fix keeps the copy while delegating.
- **Aliases** ⇒ two owners of one allocation ⇒ **the cell is ACCIDENTALLY CORRECT** and HEAD is not worth
  preserving as-is; the question becomes what it SHOULD do.
⛔ **Settled from the EMITTED C, not from reading the lowering.** Scout launched with Q1–Q5 and one hard
constraint: ***"there is no recourse" is not a reference-grade answer — if the program should be rejected, SHOW
THE LEGAL SPELLING, COMPILED AND RUN.***

⊕ **LIKELY LANDING: `Box[T]` GETS A `clone_fn`** (deep clone the pointee). Then `Box.new(h.b.clone())` compiles,
the reject becomes correct-WITH-recourse, and the spelling is the one **an expert would hand-write** — which is
what the optimality pivot asks for. ⚠ **Cost stated honestly: a real allocation the aliasing path never paid.**

⚠⚠ **THE PROCESS LESSON IS MINE.** *An owner ask is for a DESIGN DECISION or an UNRATIFIED SEMANTICS QUESTION.
"Which of three bad outcomes do you accept" is NEITHER — it is an unfinished scout wearing a ballot.* **Round
lifecycle #7 says stop for (i) a genuine design decision and (ii) an unratified semantics question. This was a
third thing: a question I had not finished investigating.**

### ⚖⚖ OWNER ASK — TRACK C1 MAKES A **WORKING** PROGRAM STOP COMPILING, AND THERE IS NO WAY TO WRITE IT INSTEAD

⛔ **MY BRIEF SAID HEAD WAS `rc 139 SIGSEGV`. MEASURED FALSE at HEAD by the orchestrator:**

```gorget
struct H:
    Box[String] b
void main():
    H h = H(Box.new("hi"))
    Box[Box[String]] outer = Box.new(h.b)
    print((*(*outer)))          # HEAD: prints "hi", rc 0, ASan SILENT
```
**Under C1's prototype: `gg build` rc 101, `lower-or-reject`.** ⛔ **The `139` was carried from `t0682`'s note
about the *ctor* spelling on a *PRE-M1* compiler** — so the fixture the executor was told to write would have
recorded a false HEAD measurement.

⛔ **NO RECOURSE — all three measured:** `Box.new(^h.b)` → **`E_PartialMove`** (already rejected at HEAD) ·
**`Box[T]` has NO `clone_fn`** · **`t1077` records that `^` does not rescue nested `Box[Box[T]]`.**
⇒ **under C1 there is NO WAY TO WRITE THIS PROGRAM.** ⊕ **A second cell has the same trade** — a Box-typed
**parameter** (`Box[Box[String]] wrap(Box[String] s): return Box.new(s)`): HEAD rc 0 correct, prototype rc 101.

⛔ **AND IT IS THE WRONG KIND OF REFUSAL.** `t0682` asks for a **CHECK-TIME DIAGNOSTIC**; the prototype ships a
**`panic!` with a `RUST_BACKTRACE` note** on a program that previously produced the right answer. **Core #8's
reference-grade bar, failed.**

⚖ **THE DECISION, and it is genuinely the owner's:** C1 fixes a **double free** (`gg check` clean, rc 134) by
deleting a 123-line hand-rolled mint and delegating to the shared producer — Core #4's preferred shape, and the
self-host already does exactly this. **The cost is that two nested-`Box` shapes that WORK today stop
compiling.** Options, with what each costs:
- **(a) SHIP IT** — the double free goes, two working shapes become uncompilable with no recourse. ⚠ Both are
  shapes `t0682`/`t1077` already track as *intended* to be refused, so this is arguably early rather than
  wrong — **but "intended to be refused" was decided when they were believed BROKEN, and they are not.**
- **(b) SHIP IT + a CHECK-TIME DIAGNOSTIC** (what `t0682` actually asks for). **Strictly better than (a)
  regardless of the ruling** — a cited refusal instead of a compiler panic. **Costs an accept→reject surface
  change ⇒ Core #9 binds all three lanes.**
- **(c) NARROW C1** so the nested cells keep working. ⚠ **UNMEASURED — it may not be possible without giving up
  the delegation that fixes the double free.** Owes a scout.

⛔ **HELD until ruled. I am not launching an executor against a ⭐ "improvement" that is a regression.**

### ✅ THE F1 REBUILD IS CONFIRMED — ONE FIX, ONE CONSUMER, AND `array_clone` MOVES BY ZERO

⭐ **THE MACHINERY ALREADY EXISTED AND WAS SCOPED TOO NARROWLY.**
`grep -n "ctx.is_bare_param(builder, \*lid)" src/ir/lowering/stmts/mod.rs` → **exactly 2 hits**: the candidate
filters of the two **existing** pre-header hoist hooks — the layering-correct machinery, **whose own docstrings
state the `restore_locals` problem verbatim**, scoped to bare params only. **The fix widens that filter to
`is_bare_param || cow_scope_carried_candidate`, read entirely off `Local.ownership` — no names.
+2 filter lines, +2 guard lines, +1 predicate.**

**MEASURED, BOTH REPROS, BOTH BACKENDS:** `t1362` `777` → **`10`**; `t0750` `1 1`/`4 4` → **`1 4`/`4 1`** —
exactly what `t0750`'s `#[ignore]`d test asserts, **so it GRADUATES.**
⭐ **MA-5 DONE PROPERLY, 51 cells:** PRE-wrong 48 · half-L fixes **exactly** the 18 loop cells and **zero**
non-loop · half-S **exactly** the 30 non-loop and **zero** loop · **18 + 30 = 48, an exact disjoint partition.**
Each half applied **alone to pristine HEAD**, four distinct binaries.
⭐⭐ **COST: `array_clone` moves by EXACTLY 0** on the declared meter; peak RSS **−404 kB**. ⇒ **NO CEILING
BUMP.** Contrast the disable-the-arm negative: **8.93× site hits, +13.6% RSS.**
⭐ **The guard fixture is entirely inside ggdef's phase-0 subset and COMPILEs + MATCHes on self-host the same
round** ⇒ it lands in the **MAIN CORPUS**, not `known_gaps/`, with the three MATCH floors **rising together**.

⛔ **AND IT CORRECTS `t1362`'s OWN SECOND CORRECTION — WHICH I WROTE.** *"Gate 2 is not a root cause… `t0750`
owes the fix in its own track"* is **measured false**. The Case 1/2 vs Case 3 distinction is about **which case
of `cow_before_mutation` fires**, **not** about the **consumer that loses the result** — both end in the
identical `register_local` rebind and **one wholesale line reverts both.** ⇒ **the two items MERGE; the round
gets a CLOSURE of the family.**

⛔ **AND IT CORRECTS ME AGAIN, PRECISELY:** I wrote *"the sidecar, which the severance path NEVER READS."*
**TRUE of `cow_collection_refs_for_id` specifically; FALSE generally —
`grep -rn "\.cow_borrow_source(" src/ --include='*.rs'` → 8 LIVE READERS** serving the lazy-rescue mechanism.
⚠ **An executor told the sidecar is unread would DELETE LIVE CODE.**
⊕ **DO NOT COLLAPSE THE STORES — the collapse was MEASURED:** behaviourally green (222/222 `cow_`) but
**`array_clone` +22.6%**, wall +14% — **real cost, zero behavioural gain, orthogonal to this fix.** ⭐ **The
genuine Layering-3 defect is CORE #14 ROT:** the `BorrowOrigin::CowBorrowPending` docstring says a later
`set_cow_borrow_source` *"upgrades the entry"*; the setter says it *"Does NOT upgrade"*; **the body writes only
the sidecar.** **The enum docstring is the rot.**

⭐ **SIX-QUESTIONS #4 ANSWERED BY MEASUREMENT: `Stmt::OnError` is the construct with NO SUBJECT — and needs
none.** `emit_on_error_cleanups` uses **`lower_block`, not `lower_block_scoped`**, so there is **no
`save_locals` boundary to lose the rebind at**. ⚠ **A LIVE TRAP: switching it to `lower_block_scoped` would
silently open the gap with NO CELL WATCHING IT.**
⊕ **The scout caught its own SIX-Q #3 defect and named the rule:** its first `save_locals` grep was **truncated
at 60 lines, reporting 16 sites where the true count is 19.** ⭐ ***"A capped grep is a selection wearing an
enumeration's clothes."***

### ⛔⛔ F1 IS REBUILT — PASS 3 FOUND MY TWO FOLDS HAD INVERTED THE ROOT CAUSE

⛔ **I RULED GATE 2 OUT (D9). GATE 2 *IS* THE REPRO.** With it out of scope **the track could not have fixed the
bug it was written for.** The chain: pass 1's **D2** claimed *"gate 1 stops any provenance match, so
`cow_materialize_alias` never runs, so there is no rebind for `restore_locals` to discard"*; pass 2 re-confirmed
it; I built **D9** on it. **Every clause of D2 is measurably false.**

**Instrumented, then reverted:** the provenance match **SUCCEEDS**, Case 3 **FIRES**,
`cow_materialize_collection_ref` **IS called** — `matched=true`, `CASE3 ref#13 … is_ref_local=true`.
**The controlled pair, ONE EDIT APART:**

| | severance decision | emitted C | run |
|---|---|---|---|
| straight-line | `matched=true`, Case 3 fires | read is `&__s17` — **the clone** | **`10`** ✅ |
| nested in `if` | **IDENTICAL** | read is `gorget_array_safe_get(__v18,…)` — **the ORIGINAL**, then `gorget_array_free(&__s20)` | **`777`** ✗ |

⇒ **the severance decision is the same in both; the only difference is the enclosing block. That is
`restore_locals` — gate 2 — and it is the SOLE mechanism.**
⭐⭐ **AND THE BODY ALREADY SAID SO:** *"pays for a clone AND returns the wrong answer — clones into a
block-local temp, mutates the original, frees the clone unread."* **That sentence is direct evidence the
materialize fired. D2 contradicted the brief's own measurement, and I folded it anyway.**

⛔ **THE ENUMERATION AXIS WAS WRONG TOO — AND SO WAS THE STORE.** I enumerated **13 `set_cow_borrow_source`
sites**. That writes the `func_state.cow_borrow_sources` **sidecar, which the severance path NEVER READS**:
`cow_collection_refs_for_id` matches on **`l.ownership`**, written by **`set_collection_ref` — 5 sites**
(`grep -rn "set_collection_ref(" src/ | grep -v "fn set_collection_ref" | wc -l` → 5). ⇒ ⛔ **TWO PARALLEL
STORES FOR ONE AXIS — Layering rule 3 violated IN THE SOURCE**, and three of my decisions rested on the store
the reader ignores.
⊕ **`p6` is NOT a sixth closure at a new write site** — it has the **same nested/straight signature**, i.e. the
**same consumer defect reached by a different producer.**

⭐⭐ **`t0750` ALREADY NAMES THE SHARED CONSUMER IN ITS OWN `mechanism` FIELD:** *"does not survive the enclosing
loop's `save_locals`/`restore_locals` boundary."* ⇒ **GATE 2 IS NOT SEPARABLE — IT IS THE SHARED ROOT. Fix the
consumer and BOTH go green; fix one producer's spelling and NEITHER does.** ⇒ **`t1362` + `t0750` are plausibly
ONE fix, and the round gets a CLOSURE instead of a fifth narrow patch.**

⛔ **NEITHER OF D8's TWO OPTIONS FIXES THE REPRO — both measured non-fixes.** (a) write-site: recording
`FieldPath(s.f)` only changes *which scan* finds the ref; `cow_before_field_mutation` calls the **identical**
`cow_materialize_collection_ref` ⇒ same rebind, same discard, **still `777`**. (b) unify on `loop_set_mutates`:
that predicate is `starts_with("@mut:{name}.")` over **name strings** ⇒ **name-matching to decide meaning,
Core #2 — a REGRESSION.** ⭐ **The real fix is neither: MAKE THE REBIND SURVIVE THE SCOPE BOUNDARY.**

⚠ **THE DISCRIMINATING AXIS WAS NEVER NAMED AS AN AXIS: ENCLOSING-CONSTRUCT vs STRAIGHT-LINE.** The
"view-producing shape" column I added **does not discriminate** — all three values behave identically.
⛔ **EVERY CELL NEEDS ITS STRAIGHT-LINE TWIN**, or a "fix" that simply disables bind-freedom goes green —
against the measured negative that disabling it costs **8.93×** site hits.

⊕ **WHAT SURVIVES the rebuild:** the defect · the CRITICAL grade · the Core #8 framing · the three-lane
disagreement (independently reproduced a third time) · **the self-host-is-ahead finding** · both measured
negatives (do NOT disable the arm; `FieldPath`-only gating fixes neither repro) · the ID block · the `t0750`
scope correction. ⊕ **`bdef3d375`'s subject CORROBORATES the inversion** — *"materialize FIELD-PATH projected
mutation"* — it fixed the field-path **producer**, which is exactly why the straight-line half works and the
nested half does not.
⊕ **Floors named for whenever a conformance fixture lands:** `C_MATCH_FLOOR` 243 · `LLVM_MATCH_FLOOR` 243 ·
`SELFHOST_MATCH_FLOOR` 242 · `MIN_FIXTURES` 243 (`grep -n "const .*FLOOR\|const MIN_FIXTURES" tests/spec_conformance.rs`).
⚠ **A site-count lint COUNTS; it does not FAIL ON REVERT.** Readiness row (4) needs a **behavioural** fixture;
the count lint separately satisfies Core #4. **Name both.** And post-rebuild the thing to count is the
**`restore_locals` / materialize-rebind pairing**, not write sites.

### ⚠ F1's PASS 1 BLOCKED ON THREE COUNTS — AND ONE OF THEM IS AN UNVERIFIED CLAIM I FILED

- ⛔ **THE ARM DISCRIMINATOR WAS FACTUALLY WRONG.** The brief said the trivial-getter arm is *"the only sibling
  arm with no `is_source_mut_unsafe_at` guard"*. **Measured: 3 of 3 `Local` sites are UNGUARDED; 6 of 6
  `FieldPath` sites are guarded — and TWO of the three unguarded arms are MEASURABLY CORRECT.** An executor
  following it finds three arms and either stalls or regresses correct behaviour. ⛔ **Guard presence is a RED
  HERRING: adding the guard fixes nothing**, because the predicate walks **strict prefixes** and the marker is
  a **descendant**. ⭐ **Real discriminator: the only site where the returned view aliases a DESCENDANT of the
  receiver while provenance names the RECEIVER.** Independent witness: `CollectionId` has **exactly two
  variants** (rustc-exhaustive).
- ⛔ **"TWO INDEPENDENT GATES" IS FALSE FOR THIS REPRO — gate 2 is LATENT BEHIND GATE 1** (gate 1 blocks the
  provenance match, so the rebind never happens and there is nothing for `restore_locals` to discard).
  **SIX-QUESTIONS #6: the repro is ACCIDENTALLY INSENSITIVE to gate 2 and cannot be its instrument.** Gate 2 is
  real but needs a **different program** (probe G: C `2 2` · ggdef `1 2` · SH `1 2`). ⇒ an executor fixes gate
  1, sees green, ships, **and probe G stays wrong.**
- ⛔ **THE DOUBLE-FREE I FILED AS THE CRITICAL JUSTIFICATION IS UNREPRODUCED.** ASan positive-controlled first,
  then four shapes: **no finding on either backend.** ⛔ **I pruned the originating scout's worktree before this
  surfaced, so the claim CANNOT BE RE-ASKED and has no recoverable evidence.** ⭐ **CRITICAL stands without
  it** — the oracle adjudicating against **both** backends (Core #8) plus silent wrong output. ⚠ **And the
  per-cell instrument was wrong in the same direction: all three enum shapes are wrong-VALUE, visible to a
  PLAIN RUN** — an executor reaching for ASan there finds nothing and calls the cell clean.

⭐ **UPGRADE: the self-host lane is MEASURED, not source-read** — SH prints `10` and `1 2`, **genuinely ahead of
Rust gg on both shapes.** A *"reference lags the self-host"* finding: fix the Rust side as oracle hygiene.

⛔ **D13 SHARPENED AGAIN — FIFTH GENERATION, AND THE CAUSE IS NOW PRECISE.** The brief carried three
irreconcilable cell counts (*"21 of 30"* vs 8 constructs vs *"12 × 7"* = 84) **while itself ordering "state NO
total"**. **The cause: I QUOTED a scout's numbers instead of REGENERATING them.** ⇒ **A NUMBER QUOTED FROM A
SCOUT IS NOT EXEMPT FROM D13. Regenerate it or drop it.**

⚠ **OPS, MEASURED THE HARD WAY: PRUNING A COMPLETED AGENT'S WORKTREE MAKES IT UNRESUMABLE.** `SendMessage`
returns *"cannot be resumed: its worktree no longer exists"*. ⇒ **DO NOT PRUNE A SCOUT'S TREE WHILE ANY OF ITS
CLAIMS IS STILL UNVERIFIED BY A FRESH PASS** — the disk is worth less than the ability to ask.
⊕ **And the lock-reason pid is the HARNESS (pid 60), identical across all agent worktrees**, so the recorded
check *"`ps -p <pid>` before overriding"* is **INERT** — it can never distinguish a stale lock from a live one.

### ✅ TRACK G SCOUTED — MY FRAMING WAS HALF WRONG, IN THE IMPORTANT HALF

⛔ **A ggdef-ADJUDICATED matrix CANNOT SEE 23% OF THIS FAMILY'S LIVE DEFECTS.** Of 792 generated cells,
**162 are ggdef-MISMATCH — and 48 MORE are provably wrong that ggdef CANNOT ADJUDICATE AT ALL**
(`named_scope` outside its statement subset; `Dict` outside its expression subset). **A ggdef-only matrix
reports those 48 as GREEN.** ⚠ **The scout reproduced the hazard inside its own prototype in the first hour** —
its summary printed `named_scope 0/8`, reading exactly like *"named scope is fine"*, while the C lane printed
`777`.

⛔ **AND NO NEW INSTRUMENT IS NEEDED — `scripts/robustness_map.py` ALREADY IS ONE.** Five lanes
(c · llvm · selfhost · asan · ggdef), hand-derived expectations never captured from the compiler,
`NO-VERDICT` as a first-class bucket *"emphatically not 'ggdef agrees'"*, a permanent
`_POSITIVE_CONTROL_broken` cell. **It grades 492 rows ggdef declines, and 13 of those are graded WRONG on
C** — exactly the capability the proposed matrix lacked.
⇒ ⭐⭐ **WHAT IS MISSING IS A CORPUS, NOT AN INSTRUMENT.** The map's 30 topics come from beginner tutorials in
other languages; **not one is about aliasing or value semantics under mutation.**

⭐⭐ **AND THIS ANSWERS THE OWNER'S REAL QUESTION — why 2026-07-06 died and what stops a repeat.** That sweep
was *"a shell invocation someone ran once"*; nothing obliged anyone to re-run it. The map has three properties
it lacked, **verified at HEAD**: **4 references in `.github/workflows/ci.yml` · a round-close battery row in
`AGENTS.md` · `round_close_battery_covers_ci_steps` reconciling the two.** ⇒ **A GENERATED TOPIC IN AN EXISTING
GATED HARNESS IS THE ONLY SHAPE OF THIS DELIVERABLE THAT CANNOT BE FORGOTTEN.** A new script would need a CI
step, a battery row, and trust in the lint — **three chances to repeat 2026-07-06.**

**MEASURED, BOTH DIRECTIONS.** Deliberate break **anchored BY LINE** after `context.rs:1540`:
**GREEN→RED 45 · RED→GREEN 0 · unchanged 747**; row-invariance wrong cells 210 → 270. Gate direction: a mutated
expectation gives `1 REGRESSION(S)`, **bare rc=1**. **Cost: ≈ +24 s on the CI C-lane, ≈ +4.5 min on the
five-lane round-close, 330 KB of source.**
⭐ **CORPUS INFLOW IS ZERO BY CONSTRUCTION** — the parity corpus auto-scans **top-level `tests/fixtures/*.gg`
only**, so a subdirectory is out; the ASan corpus carries an explicit `OUT` row for `robustness_map`; the ggdef
floor derives from the same top-level set. ⊕ **And the self-host obligation is met HONESTLY rather than dodged:
the map HAS a `selfhost` lane, so a lagging cell is baselined as `BUILD-FAIL` in that column — declared,
visible, gated against regression — instead of pushing a ceiling.**

⛔ **PRECONDITION THE EXECUTOR OWES FIRST — `t1360`'s CLASS IN A SECOND INSTRUMENT.**
`scripts/robustness_map.py:548` **silently DROPS a MANIFEST row whose cell file is missing** — not measured,
not reported, not counted. The reverse is unguarded too, and is **non-empty today: 1039 cells vs 1038 manifest
rows** (verified). ⇒ **Close the reconciliation BOTH WAYS with a declared-helper allowlist BEFORE adding ~693
rows**, or the topic quietly measures 640.
⊕ Also owed: **baseline the `selfhost` column honestly** (the scout did NOT build the driver and refuses to
carry `t1362`'s SH claim), and **add the topic's own positive control**.
⊕ **BASELINE AT THE MEASURED BUCKET, NOT AT `WORKS`** — ~210 cells land as `WRONG`. **Say so loudly in the
round entry**; a later `WRONG→WORKS` flip is PROGRESS needing a reviewed `--accept`.

⭐ **THE DISCOVERY IS MUCH BIGGER THAN `t1362` — 4 of 9 SOURCE shapes are broken:**
- **`&`-param bind loses value semantics in ANY nested block — UNFILED, now `t1383`** (6 of 8 payloads).
- **`String c = v.get(0).unwrap()` — the §3.5 DOCUMENTED EXAMPLE — breaks in any nested block** (C `abZ` vs
  ggdef `ab`). **`String` payload ONLY (1 of 8), so any payload-sampled fixture set misses it.**
- **`t0750` is FAR wider than its title** — not *"inside a `while` loop"*: **all 10 non-straight sites, 7 of 8
  payloads.**
- ⚠ **SIX-QUESTIONS #6 LIVE: the two-hop alias `b = v; c = b` is CLEAN while the one-hop `c = v` is BROKEN.**
  A hand-picked fixture using the two-hop spelling would have concluded aliasing works.
⊕ **`getter_deep` being clean is the sharpest mechanistic confirmation of `t1362`'s root cause available:** a
2-level getter is **not recognised as trivial**, so it is not clone-elided, so it is **correct**.
⊕ **Existing coverage of the broken class is ZERO, not one** — the single candidate is a *root-reassign*
program, the shape `t1362`'s controlled pair says WORKS.
⊕ **All 99 BUILD-FAILs attribute to ONE filed item (`t0002(i)`)** — a clean attribution, not noise.

⚠ **THE AXIS THE SCOUT COULD NOT CLOSE, AND IT IS THE ONE THAT MATTERS:** **SOURCE is a selection.**
`BorrowOrigin` bounds *what the compiler records*, not *what a user can write*. **Closure captures,
trait-default receivers, comprehension binds and generic-equip receivers are UNPROBED** — and **no widening of
SITE or PAYLOAD reaches them.** SITE and PAYLOAD are closed with rustc-exhaustive witnesses (`Stmt` 28 variants;
`DropStrategy` 4 values).
⊕ **ASan found ZERO — and must be in the instrument anyway.** `D52` says of this family's obligation (ii),
verbatim: *"a bug there is a **UAF, not a wrong answer**."* **Every wrong cell violates (ii).** Today the class
yields wrong values; **under the ratified D52 direction the same class yields use-after-free.**

### 🚀 TRACK G OPENED 2026-09-05 — WHY `t1362` ELUDED US, AND THE INSTRUMENT THAT WOULD HAVE CAUGHT IT

**Owner: *"How has it eluded us so long? We need a fixture or something to test it extensively, this is a
crucial feature of gorget!"*** ⇒ **I pushed back on "a fixture" and the owner's intent is served by a GATE.**

⛔ **THE FAMILY — *"a view is not severed when its source is mutated"* — HAS BEEN CLOSED FOUR TIMES, EACH FOR A
DIFFERENT SOURCE KIND, NEVER AS A CLASS** (`grep -n 'materialize' DONE.md`):

| date | shape closed |
|---|---|
| 2026-07-17 | for-loop element binding, mode-driven |
| 2026-07-18 | loop-carried bare-param materialize |
| 2026-07-19 | branch/scope bare-param pre-header materialize |
| 2026-07-02 (`bdef3d375`) | getter view — **STRAIGHT-LINE HALF ONLY** |

**`t1362` is the same family for *getter view × nested block*.** ⇒ **Core #4's litmus — *"ask how many sites
there are and what stops site N+1"* — was never applied to this family.**

⭐⭐ **THE INSTRUMENT EXISTED ONCE AND WAS NOT KEPT.** `DONE.md` 2026-07-06: *"**3 of the 4 ggdef-adjudicated
materialize-on-write holes** FIXED (both backends; #4 was a memory-safety SIGSEGV). The definition's first
production catches."* ⇒ **ggdef SYSTEMATICALLY FOUND FOUR HOLES IN EXACTLY THIS CLASS — and there is NO
standing sweep and NO gate** (searched `scripts/`; nothing). **A one-off. Nothing re-asks the question.**

**Two measured coverage facts:**
- **1 of 2246** `.gg` fixtures resembles the shape at all (crude shape-search, orchestrator).
- **`GGDEF_ADJUDICATED_FLOOR` = 496** of 2246 ⇒ ggdef adjudicates **~22%**, **and it is a FLOOR that ratchets
  upward, NOT a requirement that risky shapes be covered.**

⛔ **WHY "A FIXTURE" IS THE WRONG ANSWER: hand-written fixtures closed this family FOUR TIMES and missed the
fifth.** A fifth pins the cell we just found and says nothing about cell six.
⇒ **The instrument is 2026-07-06's sweep MADE STANDING: a GENERATED cross-product, ggdef-adjudicated, GATED.**
Generated so the enumeration cannot be a selection; oracle-adjudicated so it needs no hand-written expected
output; gated so cell six cannot land silently.

⚠ **THE SCOUT IS BRIEFED TO ATTACK THIS FRAMING, NOT IMPLEMENT IT POLITELY.** Three things could sink it and
it must say so:
1. ⛔ **Core #13 — ggdef is STRUCTURALLY BLIND to memory invalidation, and `t1362`'s worst cell is a DOUBLE
   FREE.** **A ggdef-only matrix cannot see its own worst cell.** Per-cell-class oracle assignment is required,
   ASan included, plus a rule for cells where ggdef **abstains** (out-of-subset — an abstention that reads as
   green is SIX-QUESTIONS #6).
2. ⛔ **CORPUS INFLOW.** A ~250-cell matrix landing as top-level fixtures is enormous inflow against a
   **ZERO-SLACK** non-MATCH ceiling. **This may decide the design.**
3. ⛔ **"NEVER RAN" MUST NOT READ AS "PASSED"** — a generated program that fails to compile is an ABSENT cell,
   not a passing one. ⚠ **That exact hole is LIVE in another gate right now (`t1360`).**

### 🚀 TRACK F OPENED 2026-09-05 — `D40`/`D52`, THE OPTIMALITY CAMPAIGN (owner-directed, in parallel with the safety tracks)

**Owner: *"Yes, open D40/D52 as a track now. I also need some help to decide on R1."*** ⇒ **The scout's SECOND
deliverable IS the R1 material.** ⛔ **THE SCOUT DOES NOT DECIDE R1 — IT MAKES IT DECIDABLE.** R1 gates the
EXECUTOR, not the scout, so the track runs now and waits on the ruling.

⭐ **THE DESIGN IS ALREADY RATIFIED; WHAT IS MISSING IS THE BUILD.** From `docs/define-gorget/decisions.md`:
- **`D40`** (RATIFIED 2026-07-21, recorded 2026-08-04) — return-view lazy materialization, **static provenance,
  NEVER a runtime refcount; materialize-when-unsure, never reject.** ⚠ **"STATUS: RULED, NOT IMPLEMENTED"** —
  the ledger itself says *"today both compilers materialise at the return boundary
  (`ensure_owned_at_boundary` → `ReturnFromBorrow`)"*.
- **`D52`** (RATIFIED 2026-08-30) — CoW **Rule 3 AMENDED**: a bind **materializes unless PROVABLY FREE**;
  #13 **does** cover binds.

⭐ **SO THE THREE THINGS THE OWNER ASKED TO UNIFY ARE NOT THREE PROBLEMS** — stored borrows, transient views and
cow-cost are three **POSITIONS** where ONE unbuilt mechanism is absent. That is the prior design scout's point
about §3.5 restated as a work plan: *"eager vs lazy is not a rival seam; it is the length of the live range."*

**MEASURED AT HEAD (orchestrator) — regenerate, do not inherit:** a getter returning a struct field, called 10×,
**clones 10× WHETHER OR NOT THE CALLER MUTATES** — no discrimination at all. Returning an **owned local** clones
**zero**. ⇒ **the move-on-return path is correct; the VIEW-return path is unconditional.**
```bash
./target/debug/gg run <getter fixture> --clones=stats 2>&1 | grep clone-stats
```
⚠ **THAT IS ONE TYPE AND ONE SHAPE (`Dict` getters).** **Do NOT inherit "1 clone per call" as a general
figure** — whether it is uniform across `String`/`Vector`/`Set`/nested-struct/enum-payload/`Box` is the scout's
first deliverable, and treating it as general would be exactly the selection-as-enumeration defect this round
keeps paying for.

⭐ **PRIOR ART SHARPENS R1 — TWO FINDINGS THAT CUT AGAINST THIS TRACK'S OWN DIRECTION. CARRY THEM.**
- ⛔ **LAZY IS NOT MONOTONICALLY BETTER THAN EAGER.** Deferring a clone **EXTENDS the source's live range**:
  **Lean measured 2× peak memory** on one benchmark, and **Koka refuses to ship borrow inference over exactly
  this**; Morphic measured holding a view past a mutation **FORCING** a clone that eager-cloning would have
  avoided (**6.4% of in-place mutations**). ⇒ **Every D40/D52 measurement must be against the EAGER baseline,
  not only against itself.** A favourable-direction number does not license the inverse.
- ⭐ **THE CONSERVATIVE CORE IS AGREED ACROSS THE FIELD — this IS the R1 table's spine.** Every system that
  returns a projection without copying enforces four conditions: **C1** the signature alone identifies the
  source *(Gorget satisfies this TODAY via sigils)* · **C2** the projection is **stored, never computed**
  (Swift SE-0507 rejects returning a local or temporary outright) · **C3** no conflicting mutation while the
  view is live, **by LIVENESS, not lexical scope** · **C4** the view does not escape the caller's frame.
- **Two blind spots resolve differently:** indirect calls (`Callable`/vtable/extern) are **SETTLED — everyone
  assumes the worst** (ARC's source literally comments *"Assume the worst"*); **clone there and stop looking
  for a third answer.** Non-mutating `&` params are **the field's OPEN FRONTIER, not a Gorget gap** — RFC
  2094's own listed non-goal, **unfixed in Rust for nine years**, whose intended fix is *view types*.
- ⊕ **Gorget's position is Rust's architecture MINUS the rejection**, and declared conventions are the
  mainstream. **Swift tried inferring the convention and gave up in writing**, which is why SE-0377 made it
  explicit. ⇒ **the conservative line is the STANDARD architecture, not a compromise.**

⛔ **AND A SEQUENCING CORRECTION TO `t0538`, WITH THE TREE AS WITNESS.** MLKit's retrospective names the region
profiler as the one thing that made an inferred, invisible, non-rejecting memory optimization usable, and names
***"given an apparent space leak, how would a programmer locate it?"*** as what nearly killed the project.
**`t0538` phases the knob at stage C** (*"A(summary+arg elision) → B(spec+fixtures) → C(knob …)"*).
⭐ **THE CONFLICT DISSOLVES ONCE D42'S KNOB IS SPLIT IN TWO, because the halves have different prerequisites:**
- **ATTRIBUTION — LARGELY BUILT ALREADY; I SAID OTHERWISE AND WAS WRONG.** `--clones=stats` prints an opaque
  `[clone-site] #0=10`, **but that is the coarse view, not the instrument.** `--clones=sites-tsv=PATH` emits
  **file · line · column · type · REASON · bytes · symbol** per CloneId — e.g.
  `0  <file>  5  12  Dict[int, int]  ReturnFromBorrow  144  gorget_map_clone`. ⇒ **MLKit's "how would a
  programmer locate it?" ALREADY HAS AN ANSWER.** Regenerate:
  `./target/debug/gg run <getter fixture> --clones=sites-tsv=/tmp/x.tsv >/dev/null 2>&1; cat /tmp/x.tsv`.
  **The real gap is that it is a DUMP, not a DIAGNOSTIC** — nothing surfaces it to a user who did not already
  suspect a clone. ⇒ **what must ship with the analysis is `warn`, not the whole knob.**
- **THE `deny` CONTRACT — STAYS AFTER §3, and `t0538` is RIGHT about that:** *"§3 BEFORE §4 IS NOT
  NEGOTIABLE — without the specified set, `deny` pins user code to optimizer internals and every analysis
  improvement is a potential breaking change."*
⇒ **Ship attribution early, the contract late. Neither note is wrong; they are about different halves.**

✅ **F's MAIN BODY ARRIVED (see the R1 table + return matrix below).** ⊖ *(superseded: it was missing)* ⚠ **THE MAIN BODY OF F's SCOUT REPORT HAD NOT ARRIVED** — only its prior-art addendum, which references
Lines A/B/C, an 8.9× figure, a `peek`/`advance` measurement and an F1/F2/F3 split **none of which I hold**.
**Requested; do NOT act on F until the R1 table and the return-position matrix are in hand.**
⚠ **OPS (self-reported by F's scout, MA-1 violation):** it spawned nested research agents **without
`isolation: "worktree"`**, so they shared its tree. **Verified no contamination** — `git -C /workspace/gorget status --porcelain`
and the orchestrator worktree are both clean. ⇒ **MA-1 applies to NESTED spawns and a scout will forget it;
say so in every scout brief.**

### ✅ R1 IS RULED — **LINE A** (owner 2026-09-05)

**Owner, verbatim: *"For R1 let's go with line A for now. We may revisit in the future to optimize even
more."*** ⇒ **signature-only freedom: accepts decision-table rows 1–3 and 5–9, clones at rows 10 and 12.**
⭐ **Line A needs ZERO NEW MACHINERY** — liveness, `fn_param_ownerships` and the mutation scan all exist. It is
**Rust's architecture minus the rejection**, and the standard one: Swift tried inferring the convention and
**gave up in writing** (hence SE-0377).
⛔ **"For now" is an explicit door left open — do NOT record Line A as closing Lines B/C.** Line B buys back
row 10 only, needs a call graph that does not exist, and its measured upside is **14 params across the whole
self-host closure — a bound on PLACES, not on clone VOLUME**, so one hot-path param could still dominate.
⚠ **F3 is unblocked; F2 remains gated on F1**, and ⛔ **row 4 of the decision table IS `t1362` — unsound at
HEAD.** The ruling sits on top of a mechanism that is currently broken.

### ✅ R2 — RULED, WITH TWO CONDITIONS THE OWNER RATIFIED (2026-09-05)

**Owner 2026-09-05: *"We have the `for i in v` vs `for i in &v` distinction. I think I would lean snapshot in
the bare version."*** ⭐ **This is §3.5 read LITERALLY, through a sigil the compiler currently ignores at this
position — not a new rule.**

| spelling | what `&` ALREADY means | §3.5 | proposed |
|---|---|---|---|
| `for i in v` | no write path ⇒ **READER** | *"a reader can be rescued"* | **SNAPSHOT** |
| `for i in &v` | *"the write goes through"* ⇒ **WRITER** | *"a **writer** never can, because copying a writer would silently discard its writes"* | **REJECT** |

⭐⭐ **THE DISTINCTION IS ALREADY RATIFIED — `tests/fixtures/known_gaps/sound_for_amp_scalar_elem_writethrough.gg`
says so in its own header:** *"`for x in &v` binds each element as a mutable borrow, **which is the whole point
of the sigil** (D31 full-strict: `&` means the write goes through)"*, and *"the resource-element twins of this
shape **already write through**"*. ⇒ **the owner is pointing §3.5 at a distinction the language already has,
half-implemented and pinned.**

⛔ **MEASURED AT HEAD — BOTH SPELLINGS REJECT IDENTICALLY** (`error[E_MutationWhileBorrowed]` for `for i in v`
**and** `for i in &v`). **The sigil carries NO distinction at this position today**, so the ruling SUPPLIES
meaning where the compiler collapses two cases — it does not undo a deliberate choice.

⛔ **I RETRACT MY "REJECT BOTH" RECOMMENDATION.** It rested on *"§3.5's escape is calibrated for element-sized
views"* — **a size criterion I INVENTED and attributed to §3.5, which says reader-vs-writer and nothing about
size.** The charter objection also fails: under reject the user hand-writes `d.keys()` first, **the same O(n)
copy, merely visible**; beating it needs ALGORITHM RESTRUCTURING, and the charter governs **clone placement**.

### ⛔⛔ THE ROUND'S REAL LESSON ABOUT ME: **EVERY FOLD I WRITE INTRODUCES A DEFECT IN THE FOLD ITSELF**

**Measured across two tracks, six generations, and it is now the most reliable pattern of the round:**

| track | generation | the fold's own defect |
|---|---|---|
| F1 | body → N5 | ICE prescribed on an unmeasured path |
| F1 | N5 → **D9** | retraction correct; **replacement guard FALSE on the committed corpus** (would ICE 10 shipping fixtures) |
| F1 | D8 | caught ADDENDUM 1 aiming at a **rationale** site, then made the **mirror error** one site over |
| F1 | E7 | folded a reviewer's *"plausible mechanism"* as *"say so"* — **illustration promoted to prescription** |
| F1 | **D2/D9** | **inverted the root cause**; the brief's own body contained the refutation |
| C2 | **D7** | **over-retracted** D6 — dropped the ratified entry's own scoping clause |
| C2 | **D10** | quoted a `tests/lints.rs` comment scoped to a **DIFFERENT producer family** and derived a placement rule from it |
| C2 | **ADDENDUM 1's fixture set** | **greens a two-thirds revert** — readiness row (4) unenforced |

⭐ **THE CAUSES ARE THREE, AND THEY REPEAT:**
1. **I quote text scoped to a different case.** ⚠ **The ratified ledger records this exact failure happening
   BEFORE** — *"an executor brief leaned on"* text about a **different construct**, caught at Track E pass 6.
   **I then did it twice more.**
2. **I over-correct.** A retraction that drops the retracted claim's *scoping clause* is a new defect pointing
   the other way (D7).
3. **I promote an illustration to a prescription** — a reviewer's *"plausible mechanism"* becomes *"say so"*,
   and a hypothesis lands in a durable filing.

⇒ ⛔ **STANDING RULE, BINDING ON EVERY FOLD FROM HERE:** **a fold may only assert what a command in that same
fold regenerates; a retraction must quote the retracted claim's OWN scope; and a reviewer's hypothesis stays a
hypothesis until measured.** ⭐ **The gauntlet is catching all of these — that is the system working — but each
one costs a pass, and passes are the expensive part.**

### ⭐⭐ C2's PASS 2 FOUND THE FIX'S ACTUAL AUTHORITY — AND IT INVERTS THE CORE #8 ARGUMENT

**Neither the body nor my fold cited the RATIFIED ledger entry that governs this fix.**
`grep -n "MUTABLE PRIVATE COPY" docs/define-gorget/decisions.md` → **2026-08-18**: *"THE BARE `for x in coll`
BINDING IS A **MUTABLE PRIVATE COPY**, NOT A READ-ONLY BORROW… the collection is untouched and **the program is
ACCEPTED**. The `&` form keeps its ratified write-through semantics… only the bare form was in question.*
**Consequences: (a) `String` (and every heap element type) is FIXED to behave like `int` — private copy, no
crash.**"

⇒ ⛔ **MY D6 WAS DEFENSIVE AND WRONG.** I argued the fix ships a known defect, justified on the severity ladder.
**It does not: it IMPLEMENTS ratified consequence (a) VERBATIM.** ⭐ **Only the `&` cell stays open — that is
`t1404`.** ⊕ **The same entry records the gauntlet catching this exact error before:** an executor brief
*"leaned on"* text about a **different construct**, and *"the brief-review gauntlet caught that (Track E pass 6)
and escalated rather than guessing."*

⛔ **AND MY D4 WAS RETRACTED — THE SELF-HOST HAS NO DEFECT TO PORT.** Measured with the driver built by the
**UNPATCHED HEAD** compiler (the control that matters): **all four cells — `Vector[String]` / `Deque[String]`,
bare and `&` — print `aa/bb` rc 0 on the SH at HEAD**, while Rust HEAD gives **rc 134**. ⇒ ⭐ **THE FIX BRINGS
RUST INTO LINE WITH THE SELF-HOST** — the *"reference lags the self-host"* case. **Core #9 is discharged by a
cross-lane fixture, NOT a port.**
⚠ **And I named the WRONG SH BRANCH:** `for s in &v` over `Vector[String]` takes the **write-through pointer**
branch, not the bind I cited — my description fits the **bare** form only. ⭐ **The lanes gate OPPOSITELY:**
Rust **excludes** String from that path; the SH **includes** it. **An executor told to "port the carve-out"
edits the wrong branch entirely.**

⛔ **CORE #13 — ASan IS STRUCTURALLY BLIND TO THE ESCAPE-SHAPE CLASS** (`sed -n '16769,16773p' tests/lints.rs`):
the runtime allocates from a custom pool, so **stdout is the instrument** and those fixtures live **TOP LEVEL**,
not under `security/`. ⇒ **the body's "four escape shapes verified ASan-clean" rests on an instrument that
cannot see the class**, and **the placement rule SPLITS BY CLASS** — `security/` (zero inflow, no SH build) for
the double-free; **TOP LEVEL, which DOES incur inflow against a zero-slack ceiling**, for any escape shape.

⛔ **TWO GUARDS I PROPOSED CANNOT SEE THEIR OWN CLASS — THE SECOND ONE I DIAGNOSED AND THEN PRESCRIBED.**
`VIEW_PRODUCERS_INTO_CONSUMERS` asserts **a fixture file EXISTS** (`if !path.exists()`), so **reverting a `src/`
change cannot make it fail** — and **its own header documents that limitation.** And my typed-axis enumeration
was **three sites, not two** (it missed the eligibility gate that decides whether the emitter runs at all).
⭐ **PASS 2 SUPPLIED ONE THAT WORKS AND VERIFIED IT BOTH DIRECTIONS:** a grep for the unsafe cap-copying
primitive, excluding `_view`, the runtime, and comments — **HEAD count 1 ⇒ RED; fixed count 0 ⇒ GREEN.** Closed
over the whole tree, and it encodes the class exactly: ***never pick the unsafe borrow primitive by spelling
its name.***

⚠ **OPS — A NEW HAZARD, MEASURED:** pass 2 reported `todo/t1404.md` as **missing**. It exists, committed at
`651471950`. **Its worktree was SNAPSHOTTED BEFORE that commit.** ⇒ ⛔ **AN AGENT'S TREE IS FROZEN AT SPAWN —
anything filed afterwards reads as MISSING. FILE FIRST, THEN BRIEF, or state the base commit in the brief.**

⇒ ⭐ **R2's PREREQUISITE IS NOW `t1404`, FILED AND SCOPED — AND IT HAD NO OWNER UNTIL NOW.** The durable repro
`tests/fixtures/known_gaps/sound_for_amp_scalar_elem_writethrough.gg` has existed, `#[ignore]`d and asserting
the CORRECT answer `11/12/13`, and was **cited from NO `todo/` item**
(`grep -rln "sound_for_amp_scalar_elem_writethrough" todo/` → empty before the filing).
⚠ **A PINNED REPRO WITH NO OWNER IS HOW A RATIFIED RULE STAYS UNBUILT** — the same failure mode as the
2026-07-06 sweep that Track G exists to fix.
⊕ **`t0045` (C2) closes when the double-free is gone; `t1404` carries the lost-write residual.** Orchestrator
decision, so the executor is not left to invent it.

⛔⛔ **CORRECTION 2026-09-05 — I TOLD THE OWNER CONDITION 2 WAS "THE BY-VALUE HALF OF A RULE ALREADY
HALF-SHIPPED". THAT IS FALSE, AND IT MAKES R2's PREREQUISITE BIGGER THAN I REPRESENTED.**

I quoted the pinned fixture's header — *"the resource-element twins of this shape ALREADY write through"* — and
carried it as framing. **Measured: that "twin" is a FIELD WRITE, a different mechanism.**
`tests/fixtures/cow_for_amp_resource_elem_writethrough.gg` is `x.v = x.v + 100`. ⇒ **it is not a twin of
`i = i * 2` at all.**

**Same type, same `&`, same loop — the orchestrator ran this at HEAD:**

| shape | result |
|---|---|
| **field write** `x.v = x.v + 100` | **101** ✅ writes through |
| **whole-binding rebind** `x = Rec("z", 999)` | **1** ⛔ silently discarded |

⇒ ⭐ **THE DISCRIMINATOR IS THE MUTATION *SHAPE*, NOT THE ELEMENT TYPE.** And the reviewer's full axis, measured
both sides of C2's prototype, shows **ZERO cells of the whole-binding-rebind axis write through — at ANY element
type, before OR after the fix**: `Vector[int]` `1/2/3` · `Vector[Vector[int]]` `1/3` · struct `aa/bb` ·
`Vector[String]` rc 134 → `aa/bb` · `Deque[String]` rc 134 → `aa/bb`. **The only cells that write through are
FIELD writes.**

⇒ ⛔ **R2's CONDITION 2 IS NOT "FINISHING A HALF-SHIPPED RULE" — IT IS A CLASS WITH NO WORKING PRECEDENT
ANYWHERE IN THE TREE.** The ruling stands; **its prerequisite is larger than I costed it.**
⛔ **AND C2 DOES NOT DISCHARGE IT — NOT PARTIALLY, NOT AT ALL.** Three binaries as positive controls: the pinned
fixture asserts `11/12/13`; HEAD gives `1/2/3`; the full prototype gives `1/2/3` — **byte-identical**. It
**cannot** move: every hunk is String-gated (`borrow_view_fn: Some` at exactly one site, String only; the tag
write sits inside `is_string_type`; the drop-reg change is a no-op for non-String).
⊕ **The fixture header owes the same correction.**

**TWO CONDITIONS — ⚖ OWNER RATIFIED THEM VERBATIM: *"I agree with your conditions."*** They are therefore part of the ruling, not advice:
1. ⛔ **`implicit_clones=warn` SHIPS WITH IT, NOT AFTER.** The rule **UN-REJECTS** — a compile error becomes a
   silent O(n) **inside a loop**, the worst place for an invisible cost. ⭐ **Track F's scout reached the same
   sequencing from MLKit's retrospective, independently and from the cost axis. Two lines landing on one
   conclusion.**
2. ⛔ **SCALAR `&` WRITE-THROUGH IS A PREREQUISITE, NOT A FOLLOW-UP.** Measured: `for i in &v: i = i * 2`
   **type-checks clean and prints `1`** — the write is **silently discarded** (Core #10). **Until `&` honours
   its own meaning it cannot be the discriminator** — it would reject on the strength of a capability it does
   not have. Already scoped as [[t0045]] with a live pinned test asserting the correct answer.
⊕ **A good property of the ruling: the implicit clone is ESCAPABLE BY A ONE-CHARACTER EDIT.** Writing `&v`
gets an error telling you to restructure, instead of a snapshot you did not ask for.
⭐ **CONSEQUENCES ALREADY ACTED ON:**
- **`t0538`'s PHASING IS AMENDED** — `warn` moves OUT of stage C to ship **with** the analysis; **`deny` stays
  behind §3's guaranteed-elision set**, whose own reasoning is untouched. ⊕ **Cheap, because the instrument is
  largely built:** `--clones=sites-tsv=PATH` already emits file · line · column · type · **reason** · bytes ·
  symbol per CloneId. **`warn` is the SURFACING, not new analysis.**
- **`t1403` FILED** — the `.iter()` hang, deliberately kept **separate from the ruling** because it is wrong
  under **either** side of it.
- ⭐ **`t0045` (Track C2) MOVES ONTO R2's CRITICAL PATH.** It is no longer routine safety work: **`&` cannot be
  the discriminator while writing through it is silently dropped.** Brief it as a prerequisite for a ratified
  ruling, not as a filed double-free.

⛔ **AND IT IS A DEFECT UNDER EITHER RULING TODAY:** `for k, v in d.iter(): d.put(...)` **HANGS and is
OOM-KILLED (rc 137)** — it neither rejects nor snapshots. **Needs filing regardless of which way R2 falls.**

### ⚖ R2 — SUPERSEDED REASONING (retained; do not act on it)

⛔ **I told the owner `t1361` and R2 were structurally identical. THEY ARE NOT, and the owner caught it.**
`t1361` is a **missing TYPE CHECK** — `p` is an `int`, `int` has no fields, `p.bogus_field` is ill-typed;
nothing to do with aliasing or liveness. **R2 is a WELL-TYPED program** where every access is type-correct and
the question is what mutation *during iteration* means. They share only the surface observation *"builtin
rejects, `.iter()` does not"*, and **the reasons are unrelated — so the `t1361` ruling does NOT transfer.**

⛔ **AND MY §3.5 ARGUMENT POINTED THE WRONG WAY.** §3.5 rejects a conflict ***unless the conflicting path is a
READER and the clone can be placed lazily at a VISIBLE mutation point***. The iterator **is** a reader,
`d.put()` **is** visible, and §3.5's closing line reserves rejection for copies that would be **SPECULATIVE** —
*"paid unconditionally … to guard against a mutation the compiler cannot see."* **By its own criterion this
should be ACCEPTED with a materialize.**

⭐ **THE ARGUMENT THAT ACTUALLY SURVIVES IS A COST ARGUMENT, NOT A LEGALITY ONE.** §3.5's escape clones **the
VIEW**, and in its own worked example the view is **ONE ELEMENT** (`String s = v.get(0).unwrap()`). **For an
iterator the "view" is the WHOLE COLLECTION** ⇒ materialize-to-snapshot is an **implicit O(n) copy the user
never wrote**, which breaches the charter *"implicit clones as good as the best hand-written"*. ⇒ **§3.5's
escape is CALIBRATED FOR ELEMENT-SIZED VIEWS, and an iterator is not one.** **That is the cost axis
constraining the legality axis** — the unification the CoW scout described, doing real work.
⊕ **Counter-argument, recorded so it is not lost:** Swift and C# snapshot; value semantics arguably imply
snapshot; and rejecting makes users hand-write `d.keys()` first — **the same O(n) copy, merely visible.**

⛔ **MEASURED AT HEAD, AND IT IS A DEFECT UNDER *EITHER* RULING:** `for k, v in d.iter(): d.put(...)` does NOT
produce a "silent live view" — **it HANGS and is OOM-KILLED (rc 137)**, because the iterator observes the
entries it is adding. `gg check` accepts it. The builtin rejects the identical program with
`error[E_MutationWhileBorrowed]`. ⇒ **`.iter()` is wrong in the worst available way — it neither rejects nor
snapshots — so this needs filing regardless of which way R2 falls.**

⚖ **R1, SHARPENED — this is what the owner actually has to rule on:** `D40` says *"materialize-when-unsure,
never reject"*; `D52` says *"unless PROVABLY FREE"*. **Those two phrasings must agree on what "provable" means
before ANY executor can implement either — that predicate IS the mechanism.** The scout owes the **enumerated
set of shapes where freedom is decidable and where it is not**, each with the measured cost of being
conservative there, **and BOTH defensible lines where two exist** — so the ruling is a line drawn through a
table, not a definition invented in the abstract.

⊕ **`t0952`'s prototype is a DATAPOINT for this track, not its scope** — the `Ref[T]`-writer fix measures
**zero clones, zero leak** at `/tmp/recover_scout_cow_t0952_prototype.patch`, which is evidence about what the
mechanism yields once the writer is right.
⚠ **ACCEPTANCE IS THE OWNER'S THREE WORDS: PERFORMANT · SIMPLE · SAFE.** A model that is safe and fast but
makes users reason about compiler internals fails *simple* — the objection that killed `Ref[T]`-by-default.
**And perf NEVER trades against safety.**

⚖ **TWO OWNER ASKS ARE OPEN. They block the CoW work; they do NOT block the safety tracks — do not stall.**
- **R1 — cost-axis ratification.** `docs/internals/cow-cost-contract.md` is **LEANING, not ratified**, except
  the owner-chosen knob spelling. It needs a RATIFICATION pass before any executor, not a scout.
- **R2 — the one genuine semantics question.** What does `for p in d.iter(): d.put(...)` mean — **reject, or
  materialize-to-snapshot?** Today it is a silent live view and `remove` skips elements, while the builtin
  `for` **rejects the same program**. ⚠ **Measurement cannot settle this one** (SIX-QUESTIONS #1: an
  accept/reject asymmetry may be two ratified semantics, not a defect).

### 🟢 R49 IS CLOSED (2026-09-05).
**R49's full record is in `DONE.md`** — seven tracks, two integration tracks, both owner rulings, and the
complete battery. **Nothing about R49 belongs in this block any more.** This handover was 5550 lines of
running log at close; it is now pending-only, which is what AGENTS.md asks for and what a cold restart can
actually read.

**STATE AT R50's OPEN:** working tree clean · **zero agent worktrees** · `/tmp` ~4G · full battery GREEN at
`1453b22be` (C 2747/0/256 · LLVM 2747/0/256 · six cargo targets · four script gates · sanitize · robustness ·
**parity 1566/1712 = 91.5%, non-MATCH at its ceiling with ZERO SLACK**).
⛔ **THE CLONE-BAND ANCHORS WERE RE-SEEDED AT THIS ROUND'S OPEN** (date 2026-09-05, one sha, values
unchanged — R49 moved the clone meter not at all). `clone_band_anchor_is_reseeded_before_work_resumes` is the
gate that enforces it; do not let it drift.
⛔ **FIRST UNISSUED `todo/` ID: `t1405`.** ⊕ `t1383` issued from G's block. ⊕ **BLOCKS: A1 `t1309`–`t1318` · B `t1319`–`t1328` · C `t1329`–`t1338` · D `t1339`–`t1348` · E `t1349`–`t1358` · F2/F3 `t1362`(used)+`t1363`–`t1371` → **F1 owns `t1363`–`t1372`** · A2 `t1373`–`t1382`.** Allocate a private disjoint block per executor (MA-3b).

⚠ **THE ONE THING R49 PAID FOR REPEATEDLY, AND THE ONE THING TO CARRY:** **A SELECTION PRESENTED AS AN
ENUMERATION.** It fired on a constant censused without the branch that moved it · on figures inherited rather
than regenerated · on a premise measured inside the one window guaranteed to make its class vanish · on an
end-state measured on a tree that was green only because the rest of the change was absent · on a `/tmp` sweep
that filtered `-type d` while the largest leftovers were files · and on a round-close headline written in a
shape its own instrument could not read. **Before trusting any enumeration, ask what window it was taken in
and what it structurally cannot see.**

### 🧭 THE PROJECT PIVOTS — OWNER DIRECTIVE 2026-09-05, verbatim:
> *"We have been working on making gorget work (safety), now we want to start making it fast and optimal too.
> Implicit CoW clones as if manually written by an expert engineer is the end goal of gorget."*

⭐ **THAT IS THE END GOAL, not a nice-to-have.** It sharpens the standing charter (*implicit clones must be AS
GOOD AS the best hand-written; excess is a CHARTER BREACH*) into a **phase**: the safety campaign has been the
work; **optimality is now the direction of travel.**
⛔ **THE ORDER IS FIXED BY THE OWNER: finish the safety debt FIRST, then move on the two CoW plans.**

**R50's SCOPE, owner-directed:**
1. **The CRITICAL memory-safety set** (below).
2. ⭐ **ALL R49-DEFERRED TRACKS — explicitly including Track S-a2's deferred half, `t1225`.**
3. **Then** the direction of travel: optimal-implicit-clone via the two plans.

⛔ **"ALL R49-DEFERRED" MEANS **HALF-IMPLEMENTED WORK WHOSE SECOND HALF WAS DEFERRED** — owner-corrected
2026-09-05. It does NOT mean R49's 72 open filings.** An earlier draft of this block read it that way and the
owner corrected it: *"I did not mean fix all the opened items. But close or complete half-implemented stuff
from R49 that saw their second half deferred."*

⭐⭐ **AND R49's OWN NEW GUARD MADE THAT SET MACHINE-ENUMERABLE — this is the payoff nobody planned.** Track
INT-B's `# RETIRES:` requirement means **every temporary admission in the tree now DECLARES what would retire
it**, so the deferred-work census is a grep:
```
grep -oE '^# RETIRES: .*' tests/sanitize/LEAK_ALLOWLIST.txt | sort -u     # each line = a deferred fix
grep -c '^# ⚖ ADMITTED (' tests/sanitize/LEAK_ALLOWLIST.txt              # how many admissions ride on them
```
**Every id on those lines is a half whose other half is deferred, by the tree's own declaration.**

**THE R50 "CLOSE THE DEFERRED HALVES" SET, measured — roughly SEVENTEEN, not seventy-two:**
- ⭐ **`t1225` — Track S-a2's deferred half, NAMED BY THE OWNER.** HIGH, memory-unsafe from ordinary safe
  syntax, `gg check` rc 0 AND `gg build` rc 0.
- **`t1118`** (HIGH) · **`t1216`** (MED) · **`t1066`** (MED) — items whose own text says a half was deferred.
- **`t1295`** (MED) — the retirement guard's **ADVISORY half shipped, FATAL half deferred.**
- **`t1210`** (HIGH) — R49-filed and already carrying an admission that retires on it.
- ⚖ **`t0953`** — **TWO of the owner's R49 admissions retire on it**, and a third new-inflow fixture would be
  a THIRD owner ask. **The highest-leverage single fix in the set.**
- **The rest of the retire census** — `t0609(2)`, `t0616`, `t0790`, `t0873(b)`, `t0948`, `t0949`, `t0951`,
  `t0952`, `t0971`, `t0972`. ⚠ **These predate R49**, so they are the SUPERSET, not the owner's R49 scope —
  but each is a live admission and the grep above is the honest census. **Ask before widening to them.**
⊕ **OWNER-DIRECTED INTO R50 (2026-09-05): `t1308`** — `scripts/todo_index.py --write` handles pointer ADD and
DELETE but **not MOVE**, so a re-graded item's pointer strands in its old region and `--check` reports a state
`--write` cannot repair. ⚠ **R50 re-grades the CRITICAL and deferred sets, so this stops being rare** — which
is why it is scoped here rather than left filed. ⊕ It strands on an `areas` change too, not just `priority`.

⊕ **The other ~55 R49 filings are ordinary inflow, NOT deferred halves** — LOW/MED hygiene, bulk-graduation
work (Core #4 favours the class-fix). **They are not R50's obligation under this directive.**

⭐ **THE TWO PLANS, and what each needs before an executor:**
- **`docs/internals/cow-transient-view-model.md` — the LEGALITY axis. `RATIFIED-UNBUILT`.** `D41` is ruled
  (views internal to builtins only, **no user-visible `Ref[T]`**, user methods return owned, closures are the
  sanctioned mutate-through path); owner ruled **no stored borrows** and shelved `a = &f()`. **Contestable:**
  user-writable `Ref[T]` returns + view-of-self. **Measurement-gated:** the #13 read-clone elision. **One
  OPEN:** prove type-driven builtin descent covers every case the current mechanism does.
- **`docs/internals/cow-cost-contract.md` — the COST axis, `t0538` HIGH.** `D42` (the `implicit_clones` knob
  — one name, three scopes, `allow|warn|deny`, explicit `.clone()` exempt) is RULED 2026-07-28. **FIVE things
  are LEANING and unratified:** the four-layer design (signature summary → arg-side elision →
  guaranteed-elision set → knob) · #13 merging into the cost axis while transient-views keeps legality ·
  transitive-guarantee / non-transitive-obligation · error-biased checker vs clone-biased optimizer ·
  **§3-before-§4 ordering.**

  ⚖⚖ **THE ONE ASK THAT UNBLOCKS THE PIVOT — put to the owner 2026-09-05 with this recommendation:**
  ⭐ **RATIFY §3-BEFORE-§4, and treat it as the whole point rather than a sequencing detail.** *"As good as
  meticulously optimized manual cloning"* is only CHECKABLE against a stable model of when a copy is required
  — which is what an expert hand-writing clones reasons from. **If `deny` means "the optimizer happened to
  manage it this week", then a compiler improvement cannot be relied on, a regression silently breaks user
  code, and A CALLEE BODY EDIT CAN FLIP A CALLER'S ANNOTATION** — action-at-a-distance on legality, the exact
  objection that killed `Ref[T]`-by-default. ⇒ **Specify the elision set as a LANGUAGE GUARANTEE first; the
  knob asserts against the SPEC, never the implementation. The optimizer may beat the spec; it may never fall
  below it.** Prior art the note already cites: **C++17 mandatory RVO**, which turned returning big values by
  value from a gamble into an idiom.
  ⭐ **THE MECHANISM THAT ACTUALLY REACHES THE GOAL IS LAYER 1, THE SIGNATURE SUMMARY.** An expert knows at a
  call site whether the callee CONSUMES the argument; without a per-signature ownership summary **every call
  boundary is a pessimistic clone**, which is exactly where hand-written code beats us today. **Measured
  evidence in the note: mutual recursion `ping→pong→ping` clones 201 times and warns ZERO times**, because the
  shipped diagnostic sees only DIRECT self-recursion. A call-graph fixed point sees it; nothing else will.
  ⭐ **KEEP THE ASYMMETRY: transitive GUARANTEE, non-transitive OBLIGATION.** If I elide, my caller may rely on
  it; my `deny` does NOT force my callees to be `deny`. **That is what makes it adoptable incrementally rather
  than viral** — the failure mode of every effect system that demanded the opposite.
  ⊖ **DELIBERATELY SCOPED OUT so the ask is decidable in one pass:** the **runtime tripwire** (a real
  complement — it catches unbounded MULTIPLICITY a static check cannot — but separable, and bundling it makes
  the ask bigger than the decision); **`warn` per-site vs per-function** → take **PER SITE with dedup**, which
  matches how an expert reasons (a specific copy is wrong, not a function); and **legality's B**
  (user-writable `Ref[T]` returns + view-of-self) → **keep REJECTED, consistent with `D41`**, because
  re-opening it re-imports the very action-at-a-distance the cost axis is built to avoid.
  ⭐ **WHAT IT UNBLOCKS: §1 and §2 are machinery buildable without a ruling; §4 CANNOT EXIST HONESTLY UNTIL §3
  IS WRITTEN DOWN.** Ratifying the ordering is what turns this from a brainstorm into an executable track —
  `t0538` becomes scoutable the same day.
⊕ **A cleared blocker worth knowing:** `t0544` was RULED 2026-08-30 as **`D52` — *"materializes unless
provably free"***, which settled that #13 covers binds and **AMENDED CoW Rule 3**.
⚠ **`t1065` (LOW) first, it is cheap and it misleads:** the legality note's header says `RATIFIED-UNBUILT`
while **part of it is BUILT at HEAD** — a scout will otherwise re-implement something.

### 🎯 R50's FIRST HALF IS THE **CRITICAL MEMORY-SAFETY SET** (owner-directed 2026-09-05)
⛔ **FIVE CRITICAL items are open, EVERY ONE memory-safety, EVERY ONE reachable from SAFE, SPEC-DOCUMENTED
SYNTAX THAT `gg check` PASSES CLEAN.** This outranks the guard-hygiene cluster R49 produced — that is fill-in
work, not a headline.
- ✅ ~~**`t1077`** — reading through a nested `Box[Box[T]]`~~ **CLOSED 2026-09-05 by Track A1 (read half).**
  ⚠ **"READ FIXED, LEAK NOT"** — the leak half is **`t1309`**, mechanism recorded **UNDETERMINED** and blocked
  on `t0096`. **Do not read this as a clean close of a CRITICAL.** ⊕ The fix also surfaced `t1310` (**CRITICAL**,
  self-host: the constructor SPELLING changes memory safety), `t1311` and `t1313`.
- **`t1067`** — a closure that **CAPTURES ANOTHER CLOSURE** reads freed memory: **rc 0 with SILENTLY WRONG
  OUTPUT**, heap-use-after-free under `--sanitize`. *(found by R49 Track L's pass-2 review)*
- **`t0011`** — `Box[T](struct.field)` takes a **SHALLOW** clone when the field transitively owns `Box`es →
  **DOUBLE FREE**, both backends.
- **`t0045`** — `for x in &coll` + assigning to the loop binding **DOUBLE FREES**. ⚠ **The item's own warning:
  THE `&` IS NOT THE DISCRIMINATOR** — the same proxy-vs-mechanism trap that turned R49 Track T1's comment fix
  into a reproducible defect. **Do not scope this one by the sigil.**
- **`t0036`** — `unwrap()` from a plain READ of safe syntax, both backends. ⚠ **Its axis was CORRECTED by a
  second pass; the first filing was measurably too narrow** — re-read the correction before scoping.
⊕ **`t1303`** (HIGH, R49-found) belongs with them: a closure literal at `unwrap_or_else` over a `Callable`
payload — **Rust gg builds, runs, prints rc 0 AND is a stack-buffer-overflow; the self-host is caught by
`cc`.** ⭐ **Both write sites are already localized:** Rust at the **call-result slot typing** (do NOT touch
the closure signature), self-host at the **signature**, one layer earlier.

⚠ **THE ENUMERATION IS TOTAL, VERIFIED — not a field selection.** `severity = "CRITICAL"` yields exactly these
five, AND no item states CRITICAL in prose while leaving the field empty (295 of 887 items have an empty
severity field, so that check was necessary). Regenerate BOTH halves:
`for f in todo/*.md; do sev=$(grep -m1 '^severity' "$f" | cut -d'"' -f2); [ "$sev" = CRITICAL ] && echo "$f";
done` and the same loop inverted, grepping prose for `CRITICAL` where the field is empty.

⭐ **TWO OF THE FIVE WERE FOUND BY R49 ITSELF** — by a pass-2 brief-review and by a track writing an error
message. **The review discipline is generating CRITICALs faster than rounds are closing them**, which is the
argument for making them the headline rather than fitting them around other work.
⭐ **`t1077` AND `t1303` SHARE A SHAPE: THE TWO LANES ARE BROKEN *DIFFERENTLY*.** "Both backends agree" would
have hidden both; only a lane-divergence instrument or ASan sees them. Core #8, twice.
✅ **The previous CRITICAL trio is CLOSED** — `t0763`, `t0770`, `t0771` and `t0772` are all in `DONE.md`.

**⚖ FORCING FUNCTION — `t0953`:** two of the owner's R49 admissions RETIRE when it lands, and a third
new-inflow fixture would be a THIRD owner ask. Fixing it discharges both and closes the class.
**CARRIED, unscouted:** W + S-a3 (merged, brief measured against source three times) · U1 · U2 · S-a1 · S-b ·
T2. **DEMOTED to fill-in:** `t1302` (pins regenerated from the artifact they pin) · `t1304` (46 unwired
top-level fixtures) · `t1295` · `t1301` · `t1300` · `t1296`.
⛔ **STANDING CONSTRAINT: the non-MATCH ceiling and the ggdef floor are at ZERO SLACK — any R50 track adding a
non-MATCH fixture reds immediately.**

### ✅ OWNER RULINGS — do not re-litigate
- **No partial moves (2026-09-02):** only whole-value `^m`; field/index `^` is `E_PartialMove`.
- **D53 (2026-09-01):** `Mutex`/`RWLock` are unique locks; share via `Shared[Mutex[T]]`; reject at assign,
  init AND consuming positions; the diagnostic names `^source`, never `.clone()`.
- **⭐ THE LEAK RATCHET (2026-09-02):** ADMIT THE SIX. The R47 ruling ("four rows … and ONLY those") is
  EXTENDED to cover **pre-existing leaks newly made VISIBLE by a graduation out of `known_gaps/`, and to
  those only**. A row whose leak is genuinely NEW inflow is still an owner ask. Grounds: all six verified
  pre-existing against a pre-fix compiler; the count grew only because two fixtures stopped SEGV-ing;
  deleting the cells would delete regression coverage of the defect R48 fixed.
  ⊕ **The long-term intent is to fix ALL of them, not just these** (owner: *"These must be burnt down and
  fixed at some point. Not only these 3 but all of them!"*).
- **⭐ LEAK DEBT IS MEASURED AS A TREND, NOT AN EXPIRY (2026-09-02):** **DOWN is silent; UP must be
  acknowledged (owner ask).** Pair with a coverage floor so a fall achieved by DELETING a fixture is visible.
- **⭐ THE PARITY CEILING IS NEVER RAISED (2026-09-02):** if the non-MATCH backlog grows, **FIX the
  self-host and PORT the rows** — do not raise `RUNTIME_DIFF_NONMATCH_CEILING`. R48 did exactly this
  (Track U) and the ceiling is untouched at 147 for the whole round. Lowering it when the backlog shrinks
  remains REQUIRED and needs no sign-off.
- **The clone re-anchor (2026-09-03):** stage-1 `array_clone` moved **+1.016%**, past the ~1% band, and the
  owner AUTHORIZED it on the attribution that Track U's lowerer edits ARE the stage-1 meter's workload.
  Both `.pin` AND `.round_open` moved, because the band is computed FROM the anchor — pinning `.pin` alone
  leaves the gate red against a stale anchor and the authorization would be inert. The four anchors record
  this as an owner-authorized re-anchor, **NOT a routine round-open reset**; do not "correct" them.
- **ggdef stays simple (2026-08-31):** out-of-model surface ABSTAINS loudly. ⚠ But see the trap below.

### ⛔ THE TRAP THIS ROUND PAID FOR TWICE — RUN ggdef, NEVER ASSERT ITS ABSTENTION
Two briefs claimed "ggdef: out of model, ABSTAIN", each citing a grep for a DIFFERENT type than the one at
issue (`Mutex|RWLock` while the subject was `Callable`). Both were false and ggdef was RIGHT both times:
`ggdef run` prints `2` on the `Callable` `.clone()` repro while BOTH production compilers SEGV'd; and
ggdef's D53 exclusion comment rested on *"measured: accepted on both compilers"*, a premise that DECAYED
the moment D53 landed, leaving ggdef divergent at 7 consuming positions.
⇒ **Core #13 says ask ggdef FIRST. `cargo build -p ggdef --bin ggdef` then `./target/debug/ggdef run <f>`
costs seconds. A grep for another type is not evidence about abstention.**

### ⛔ INSTRUMENT FINDINGS THAT OUTLIVE THIS ROUND
- **`t0924`:** ALL SIX assertions in `self_host_runtime_diff` are `cfg!(debug_assertions)`-skipped, so the
  round-close C sweep arms NONE of them. Only `--release GG_RUNTIME_DIFF=1 … -- --nocapture` does.
  ⊕ Both horns of the timeout conflict were measured: `GG_TEST_TIMEOUT_SECS=600` (step 4's mandate) makes
  the allowlisted `async_select` hang stall a worker 20x; the default false-REDs `lowerer_comparison` when
  other work shares the box. **A round-close sweep must run on a QUIET tree — AGENTS.md does not say so.**
- **`t0925`:** `scripts/run_integration.sh` puts `"$@"` before cargo's `--`, so a harness arg becomes a
  filter and the sweep reports a GREEN ZERO.
- **`t0824`:** `known_gaps_census.sh --check` is a CI step, is RED, and is **ABSENT from the round-close
  battery** — so AGENTS.md's *"The full battery covers every target CI runs"* is **measured false**.
- **`t0870`:** `RustRejected` rows return before the self-host is invoked, so a self-host OVER-ACCEPTANCE
  is structurally invisible to the parity gate. Five live instances named in the item.
- **`sanitize_sweep.sh` had NEVER been run during R48** — which is why three leaks sat unnoticed from
  Aug 31 to the close. It is in the battery; run it *during* the round, not only at the end.

### 🔁 THE ROUND-CLOSE BATTERY — COMMANDS, NOT NUMBERS (regenerate every figure; Core #5)
```
scripts/run_integration.sh                                    # C sweep (GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600)
GG_BACKEND=llvm scripts/run_integration.sh --release          # LLVM sweep — SEQUENTIALLY, never parallel
cargo test --test spec_conformance -- --test-threads=1 --nocapture   # ⚠ --nocapture or the lane counts are swallowed
cargo test --lib && cargo test -p ggdef && cargo test --test security && cargo test --test lints
scripts/sanitize_sweep.sh                                     # ~25 min; ASan leak + corruption allowlists
python3 scripts/robustness_map.py --lanes all                 # five lanes; never edit an expectation to match
GG_RUNTIME_DIFF=1 cargo test --test integration --release self_host_runtime_diff -- --nocapture
                                                              # the ONLY invocation that arms the parity floor/ceiling (t0924)
bash scripts/clone_meter_check.sh --anchor-age                # must exit 0
scripts/convergence.sh <prev_kg> <prev_todo> <filed>          # MEASURES, does not gate
```
⚠ **Run these on a QUIET tree** — no agents building. R48 paid for this twice with false REDs.

## ⏱ NEXT 1–3 ROUNDS (hot-list)

- **🔒 THE 2 REMAINING `EXPECTED_BOTH_WRONG` PARITY ROWS — ⚠ NOT AN OWNER ASK, NOT A PRODUCTION BUG; FIX ggdef** (`drop_collection_custom_elem_leak` · `drop_struct_collection_fields` — authoritative list is `EXPECTED_BOTH_WRONG` in `tests/integration.rs`, not this line). ⚠ **CORRECTED 2026-08-31 at R48 open — this line previously read *"by Core #8 each is ≥1 real bug in BOTH compilers; ggdef contradicts"*, which is FALSE and had survived its own settlement in `t0304` (filed 2026-08-22) to mislead at least one scout and one orchestrator.** Re-measured at HEAD: row 1 — production `start/done/drop 1/drop 2` (**D37's own forward-order exemplar**), ggdef `start/done`, the user `Drop` bodies never firing; row 2 — production 15 lines, ggdef 10, omitting struct-field and nested-container destructors and leading with `drop third len=1` BEFORE `len 3`. **The lanes are RIGHT; ggdef does not run container-element destructors at all.** Core #13 governs: *a BOTH-WRONG row is an owner ask ONLY if the semantics are UNRATIFIED; where the ledger rules, fix ggdef* — and D37 rules, as does the owner's 2026-08-22 ruling quoted verbatim in `t0304` (*"collections own their items, hence when collections are dropped they need to recursively drop their items too"*). The harness's own seed comment agrees: *"every entry is a GGDEF-side defect — production agrees with the fixture-documented expected output."* ⚠ **The bucket NAME is the trap** — `BOTH-WRONG` means *both real lanes disagree with the oracle*, NOT *both compilers are wrong*. Both are the G-class Drop rows 2+3 work, so closing that item closes these. Was 8 rows when filed; six burned down across XII/XXV/XXVI (struct-value match patterns · print kwargs + f-string format specs · the two Displayable-render cells · whole-local reassign-drop). Own round, own scout.
- **THEN:** MaterializePlan campaign follow-up (auto-move for post-materialize params) · Round XI Track J follow-up: typed `borrow_read: bool` · Track K class-siblings · SH-lane `W_*` parity port · `lower_tuple_field_assign` silent-drop fallback · Instrument C (cell matrix) · #13 perf reclaim · SH bare-arg CoW residual · D30+C1 · class-A/B ggdef · RV-C/E/H + R6 realloc UAF · D6 refcount params (design first). (Family-3 getchain **closed in XVIII**.)

## Operating invariants (load-bearing — process/reference context, not filed work)

**NORTH STAR = RUNTIME PARITY: self-host-compiled binary produces SAME output as Rust gg.** RE-MEASURE (never trust a dated number — `*_comparison`/`runtime_diff` are diagnostic-always-pass). **Round-close procedure (the three gotchas):**
```
# 1. FORCE-REBUILD the cached self-host driver (THE load-bearing step)
rm tests/fixtures/self_host_lowerer/driver{,.c}
# 2. Regen — GG_BUILD_TIMEOUT_SECS=600 only; leave GG_TEST_TIMEOUT_SECS at default
#    (setting test timeout to 600 changes zero counts but stalls hang-class fixtures ~20×)
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 \
  cargo test --test integration --release self_host_runtime_diff -- --nocapture
```
Read the printed `PARITY = MATCH/(...)` line and the adjudication split (ADJ-MATCH · UNADJ · BOTH-WRONG). Diagnostic-always-pass — only the printed count means anything. Floors `RUNTIME_DIFF_MATCH_FLOOR` + `GGDEF_ADJUDICATED_FLOOR` ratchet **up only**; if MATCH/ADJ rose, reseed the const in `tests/integration.rs` same commit (never lower).

**OPERATING INVARIANTS (load-bearing):**
- **⚡ BRANCH RULE — DO NOT RESTATE IT HERE. IT LIVES IN `AGENTS.md` MULTI-AGENT RULE 0b.** Owner
  2026-09-03: *"work should ALWAYS land on the directory/worktree claude is invoked from."* ⭐ **That rule
  ALREADY EXISTED in `AGENTS.md` 0b** (*"Stay in the launch worktree — that IS the session integration
  branch. Never hardcode a branch name"*), and 0b now carries the owner's general phrasing plus the
  precedence line. ⛔ **THE FAILURE WAS THIS BULLET, and the lesson generalises: a HANDOVER invariant
  CONTRADICTED a live `AGENTS.md` rule, and the orchestrator — which reads the handover FIRST by process —
  followed the stale one.** Its 2026-07-07 text said *"in THIS container, land on `main` via reviewed
  worktree merges"*; that was wrong and stood for two months. ⇒ **When a handover bullet and `AGENTS.md`
  disagree, `AGENTS.md` WINS** — the handover is rewritten every round close and decays by design; the
  rules file is the spec. ⊕ Practical note worth keeping: the session worktree can legitimately sit BEHIND
  `main` after an owner sync, and the repair is `git merge --ff-only main` — a pure fast-forward that
  rewrites nothing and leaves every live agent worktree's base an ancestor of HEAD. ⛔ **Never `rebase`
  while agents are live**: `main` is pushed, and a rewrite would strand them.
- **Agent-worktree base:** every delegated agent preamble opens with `git merge --ff-only gorget-1` (worktrees branch from main, lag without it). ALWAYS pass `isolation:"worktree"` PARAMETER + pwd-check preamble + `git add <exact files>` only.
- **Per-track loop (NO pack reviews — owner 2026-07-21):** scout (verify premises + MEASURE end-to-end: compile AND run AND diff stdout, never source-read) → brief → ≥3 fresh SEQUENTIAL brief-reviews **of that brief only** (new clean-context agent each pass; N tracks ⇒ N×≥3 brief-review agents; passes across tracks may parallelize, never pack N briefs into one agent) → **one** executor (worktree) per track → **one** fresh output-review per track → integrate + COMBINED gate. A measured-green tight prototype may collapse to output-review→integrate.
- **Gate battery** (re-run, don't trust): `cargo test --lib` (~1085/0 debug; the 2 former `--release` `should_panic`-over-`debug_assert` reds in `src/lir/validate.rs` are now `#[cfg_attr(not(debug_assertions), ignore)]`d — fixed) + `--test lints` + `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600` — load-bearing canary, double-frees on wrong drop model) + full integration + `self_host_runtime` (snapshot lock-in) + `*_comparison`/`runtime_diff` diagnostics. **⛳ OWNER-REQUIRED (2026-06-20): run the FULL `cargo test --test integration` at every round's close to confirm green — not just the targeted/self-host gates** (`GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration -- --test-threads=4 2>&1 | tee /tmp/integration-$RANDOM.log`; debug = the clean baseline). **⛳ OWNER-REQUIRED (2026-07-23 post-mortem): the local battery must MATCH CI's test-job target set — ALSO run `cargo test -p ggdef` + `--test spec_conformance` + `--test security` (three SEPARATE targets `--test integration` never touches; three CI-red causes — ggdef gate-drift, `.filter().map()` byte-trunc MISCOMPILE, LLVM-missing conformance build-fail — hid here red for a week while rounds closed on integration-green). This makes local-green SUFFICIENT (it now covers every target CI runs, so no code defect hides in an unrun sibling) → local-green IS the sign-off; autonomous continuous local rounds do NOT wait for CI. Residual: a pure CI-*config* failure (missing-`llc` job, Linux-only floor skipped locally) is invisible to local runs but is NOT a per-round gate — separate CI-hygiene (periodic glance), never a round-close blocker.** [[run-full-integration-each-round]] CoW changes add ASan + eager/pre-change Step-0 baseline (ASan is BLIND to wrong-output AND view-UAF — stdout fixtures are primary). Integration: partition (heavy self-host serial, rest `--test-threads=4`) OR whole suite `--test-threads=1` (peak RSS ~1.2GB/self-compile, not OOM — the constraint is 10 CPUs). `cargo test` takes ONE positional filter; extra name filters go after `--`.
- **`lower.gg` MODULE MAP:** lowerer split into core `lower.gg` + `lower_drops/liveness/types/expr/stmt/match/loops/closures/generics/cow.gg`. ANY `lower.gg:NNNN` citation predates the split — re-grep the cited FUNCTION across `lower*.gg` before acting.

**ACTIONABLE NEXT (re-verify each target by RUNNING the code end-to-end, not source-reading):**

**PARALLELIZATION MODEL (owner-updated 2026-06-15 — SUPERSEDES old "1 parity chain only"):** run as many well-scoped tracks CONCURRENTLY as you have work for, in PARALLEL worktrees, EVEN IF they touch the SAME files (worktree isolation + orchestrator-merge-at-integrate makes file overlap safe). **PARITY tracks PARALLELIZE too** (the old "wins serialize on the lower*.gg cluster" predated the lowerer split + ignores wins in `traits.gg`/`infer.gg`/`lir_lower.gg`/`lir_codegen.gg`). **ONLY constraint: do NOT parallelize a track that is REALLY BROAD** (sprawls the whole lowerer cluster, e.g. method-generic-mono); run that alone. (1-PERF/1-CLEANUP/1-DOC alongside is a useful MINIMUM, not a cap.) Narrower "sequence X" notes elsewhere are superseded EXCEPT where genuinely broad.

**LESSONS (load-bearing):** scout parity estimates MUST be end-to-end-verified (compile+run, whole-stdout MATCH) — multiple estimates this arc were ~0 real until proven by running. Re-verify every premise against CURRENT source/tests before acting. CLEAN single-fix parity wins are nearly exhausted — what remains is a deep interconnected cluster.

- ℹ **`lower.gg` MODULE MAP (navigation):** core `lower.gg` (LowerCtx + named_locals/`add_local` + `lower_fail` + `lower_module`, ~3.3k lines) + leaf modules: `lower_drops.gg` / `lower_liveness.gg` / `lower_types.gg` (type naming/mangling, `type_id_to_name`, `get_fn_param_types`, builtin return types) / `lower_expr.gg` (`lower_expr`/`lower_call`, value-position EIf/EMatch/EDo) / `lower_stmt.gg` (`lower_stmt`/`lower_if`/`lower_field_write`) / `lower_match.gg` (`lower_match_stmt`/`lower_match_expr`, payload reads) / `lower_loops.gg` (`lower_for*`) / `lower_closures.gg` (`lower_function`/`lower_closure_body`, fn-sig reads) / `lower_generics.gg` (proto-walk/meta-expand) + phase-1 `lower_cow.gg`. **Any `lower.gg:NNNN` predates the split — re-grep the cited FUNCTION across `lower*.gg`.**

## CoW / ownership / materialization

- [`t0001`](todo/t0001.md) **HIGH** — 🆕🚨 [HIGH — TWO REMAINING STAGING-MOVE WRITE SITES, both a live read-after-move; measured 2026-08-23 by R44 Track B, both…
- [`t0002`](todo/t0002.md) **HIGH** — 🆕🚨 [HIGH — A DOCUMENTED IDIOM DOES NOT COMPILE, plus its sibling and an ICE; all found 2026-08-22 by the R44 Track-B sco…
- [`t0003`](todo/t0003.md) **HIGH** — 🆕🚨 [HIGH — A DOCUMENTED FEATURE ICEs: a comprehension whose RESULT element is a RESOURCE dies before codegen; measured 2…
- [`t0705`](todo/t0705.md) **MED** — 🆕🐛 [MED — an UNNAMED TEMP in MATCH SCRUTINEE POSITION leaks its payload, live at pristine HEAD with no closure anywhere;…
- [`t0722`](todo/t0722.md) **HIGH** — 🆕🚨 [HIGH — a FALSE INVARIANT COMMENT that a fix is actively resting on (Core #14), found 2026-08-29 by the R46 Track C s…
- [`t0724`](todo/t0724.md) **HIGH** — 🆕🚨 [HIGH — R46 Track B, STOPPED BEFORE EXECUTION by owner decision 2026-08-29. This item preserves everything the round…
### High


- [`t0004`](todo/t0004.md) — 🆕⚖️ [OWNER KNOB — surfaced 2026-08-19 by Track G brief-review pass 2, NOT decided; durable home for a decision that was…

- [`t0005`](todo/t0005.md) **MED** — 🆕🚨 [MED — CROSS-LANE ACCEPT/REJECT DIVERGENCE, Core #8 (≥2 bugs) + Core #9; found 2026-08-19 by the Track-C brief-review…

- [`t0006`](todo/t0006.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, then LINK FAILURE on a mangled internal symbol; found 2026-08-19 by the Track-M1 brief-revie…

- [`t0007`](todo/t0007.md) **MED** — 🆕🐛 [MED — PARSER INCONSISTENCY, ⚡ OWNER-RATIFIED AS A DEFECT 2026-08-24: if and case MUST BE CONSISTENT and BOTH ACCEPTE…
- [`t0008`](todo/t0008.md) **MED** — 🆕📐 [MED — FEATURE GAP, owner-ratified 2026-08-26: EXTEND THE FALLIBLE-ARITH OPERATORS /! and %! TO FLOATS] A float divid…
- [`t0009`](todo/t0009.md) **HIGH** — 🆕🚧 [HIGH — 3 of 4 CLONE RATCHETS STILL RED after a MEASURED 92% RECLAIM; the remaining lever is NAMED and costed] R44's…
- [`t0010`](todo/t0010.md) **HIGH** — 🆕🚨 [HIGH — RUST LANE DOUBLE FREE from TWELVE LINES using the language's own move operator; gg check CLEAN; found 2026-08…
- [`t0011`](todo/t0011.md) **CRITICAL** — 🆕🚨 [CRITICAL — MEMORY SAFETY, BOTH BACKENDS, gg check CLEAN; found 2026-08-27 by the clone-reclaim phase-2 track, orches…
- [`t0012`](todo/t0012.md) **HIGH** — 🆕📐 [HIGH — THE NEXT CLONE LEVER IS NOT THE RECORDER; measured 2026-08-27 by scripts/clone_attribution.sh] 71% of attribu…
- [`t0014`](todo/t0014.md) **MED** — 🆕🐛 [MED — ⚠ THE gg fmt SWEEP (A2) CANNOT FIX THIS: docs/ IS IN NO SWEEP ROOT AND gg fmt TAKES <file.gg>, NOT MARKDOWN. F…
- [`t0015`](todo/t0015.md) **HIGH** — 🆕📊 [HIGH — THE DOCUMENTATION-EXAMPLES CORPUS: 288 cells derived from our own docs, measured on C and LLVM] 239/288 = 83%…
- [`t0016`](todo/t0016.md) **HIGH** — 🆕🚨 [HIGH — A USER FUNCTION NAMED abs DOES NOT COMPILE, on BOTH backends, with a raw C error; found 2026-08-24 by the rob…
- [`t0017`](todo/t0017.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG ANSWER IN A CORE CONTROL-FLOW CONSTRUCT, BOTH BACKENDS, ON TEXTBOOK CODE; found 2026-08-24 by th…
- [`t0018`](todo/t0018.md) **HIGH** — 🆕📊 [HIGH — THE BEGINNER ROBUSTNESS MAP, owner-requested 2026-08-24: 354 cells of common beginner code, discovery-only] 8…
- [`t0019`](todo/t0019.md) **HIGH** — 🆕📐 [HIGH — CAMPAIGN, owner-ratified 2026-08-24: AXIS COVERAGE MUST BE OBSERVED AND DERIVED, NEVER DECLARED] Build a sema…
- [`t0020`](todo/t0020.md) **HIGH** — 🆕🚨 [HIGH — BOTH LANES SILENTLY PRINT A RAW POINTER FOR ORDINARY CODE, rc 0, no diagnostic; found 2026-08-24 by the R44 T…
- [`t0021`](todo/t0021.md) **HIGH** — 🆕🚨 [HIGH — SILENTLY WRONG VALUES FROM UNINITIALIZED MEMORY ON THE RUST LANE, at rc 0, no crash, no sanitizer report; fou…
- [`t0022`](todo/t0022.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, BOTH LANES THEN FAIL AT CODEGEN ON A SYMBOL THAT DOES NOT EXIST; found 2026-08-24 by the R44…
- [`t0023`](todo/t0023.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, THEN THE C COMPILER FAILS; found 2026-08-24 by the R44 Track-K brief-review pass 4, orchestr…
- [`t0024`](todo/t0024.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, BOTH BACKENDS THEN FAIL AT CODEGEN; found 2026-08-18 by the A2-0 scout as an incidental, orc…

- [`t0025`](todo/t0025.md) **MED** — 🆕🐛 [MED — Core #10 SILENT DROP: gg check ACCEPTS, methods VANISH, build dies at LINK; found 2026-08-18 by the A2-0 scout…

- [`t0026`](todo/t0026.md) **HIGH** — 🆕🐛 [HIGH — closure capture of a COLLECTION-ELEMENT BORROW returns the ADDRESS; gg check clean, both backends rc 0; measu…
- [`t0027`](todo/t0027.md) **MED** — 🆕🧹 [MEDIUM — Layering rule 2 debt exposed by R43 M1; NOT a live miscompile] override_is_trait_box (src/ir/lowering/exprs…
- [`t0028`](todo/t0028.md) **LOW** — 🆕🧹 [LOW — auto_clone_if_ptr's String carve-out is a type-identity test standing in for typed metadata; measured LIVE 202…
- [`t0029`](todo/t0029.md) **HIGH** — 🆕🛡 [HIGH — STANDING OBJECTIVE, owner-ratified 2026-08-18 (ledger): "we should be running everything under a sanitizer".…
- [`t0030`](todo/t0030.md) **MED** — 🆕🧹 [MED — NAME-MATCHING at a link decision + real OVER-LINKING; found 2026-08-18 by the Track-F executor while closing a…
- [`t0031`](todo/t0031.md) **HIGH** — 🆕🐛 [HIGH — check-clean MISCOMPILE, BOTH LANES; found 2026-08-18 by the Track-F output review, orchestrator-reproduced] O…
- [`t0032`](todo/t0032.md) **HIGH** — 🆕🚨 [HIGH — Core #8 SOUNDNESS + LANE-DIVERGENT SILENT MISCOMPILE; found 2026-08-17 as bycatch of the Track-F brief-review…
- [`t0033`](todo/t0033.md) **MED** — 🆕🐛 [MED — LLVM-LANE ONLY, C builds clean; found 2026-08-17 by the Track-F brief-review pass 3, orchestrator-reproduced]…
- [`t0034`](todo/t0034.md) **HIGH** — 🆕🐛 [HIGH — A SHIPPED STDLIB FUNCTION THAT CAN NEVER BUILD; found 2026-08-17 by the Track-F brief-review pass 2, orchestr…
- [`t0035`](todo/t0035.md) **HIGH** — 🆕🐛 [HIGH — TWO FACES, ONE ROOT, both gg check-clean; found 2026-08-17 by the Track-E brief-review pass 2, orchestrator-r…
- [`t0036`](todo/t0036.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY SAFETY, BOTH BACKENDS, from a plain READ of safe syntax; found 2026-08-17 by the Track-E brief-re…
- [`t0037`](todo/t0037.md) **HIGH** — 🆕🐛💥 [HIGH — Core #10 SILENT DATA LOSS BY THE FORMATTER; found 2026-08-17 by the Phase-4c brief-review pass 2, orchestrat…
- [`t0039`](todo/t0039.md) **MED** — 🆕🧹 [MED — NAME-MATCHING CLASS, CLAUDE.md "No name matching"; scoped 2026-08-17] The C runtime-chunk selector routes 759…
- [`t0040`](todo/t0040.md) **MED** — 🧹 [MED — examples/ IS GATED ON BUILD ONLY; the RUN half is deferred, owner 2026-08-17] Nothing asserts that an example s…
- [`t0041`](todo/t0041.md) **HIGH** — 🆕📐 [HIGH — CAMPAIGN, owner-ratified 2026-08-17 (ledger); "fix first, promote second"] Make for x in xs the ACTUAL defaul…
- [`t0042`](todo/t0042.md) **HIGH** — 🆕🐛💥 [HIGH — LEAK/OOM; found 2026-08-17 by the idiom scout] Dict.iter() clones the map ~4× per element and leaks it — 983…
- [`t0043`](todo/t0043.md) **HIGH** — 🆕🐛 [HIGH — Core #10 SILENT DROP; found 2026-08-17 by the idiom scout] A for-loop over a NON-ITERABLE type is silently di…
- [`t0044`](todo/t0044.md) **MED** — 🆕🐛 [MED — the Book teaches two functions that DO NOT EXIST; found 2026-08-17] zip(a, b) and enumerate(c) appear in docs/…
- [`t0046`](todo/t0046.md) **MED** — 🆕🧹 [MED — A2 SWEEP HAZARD, no executable guard; found 2026-08-17 by the Phase-4b confirming pass] 40 fixtures carry a MU…
- [`t0047`](todo/t0047.md) **LOW** — 🆕🧹 [LOW — coverage gap, found 2026-08-17 by the Phase-4b confirming pass] tests/fixtures/fmt_magic_comma/ has no directo…
- [`t0048`](todo/t0048.md) **HIGH** — 🆕⚡ [HIGH — BLOCKS THE A2 SWEEP; ratified 2026-08-16 (define-gorget ledger); R42 Phase 4c] Implement @fmt(skip) on items…
- [`t0049`](todo/t0049.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG-CODE, BOTH LANES, gg check clean; ratified 2026-08-16 (define-gorget ledger), surfaced by the R4…
- [`t0050`](todo/t0050.md) **HIGH** — 🆕🐛💥 [HIGH — VISIBLE-FLOW HOLE, filed 2026-08-10; found by the Fable error-model review, orchestrator-verified] A bare Re…
- [`t0051`](todo/t0051.md) **HIGH** — 🆕🐛 [HIGH — check-clean MISCOMPILE, filed 2026-08-10; found by the Fable review, orchestrator-verified] catch on a VOID f…
- [`t0052`](todo/t0052.md) **HIGH** — 🆕🐛 [HIGH — exit-taxonomy violation, filed 2026-08-10; found by the Fable review, orchestrator-verified] The C runtime ca…
- [`t0053`](todo/t0053.md) **MED** — 🆕📖 [MED — the book documents a FICTIONAL stdlib, filed 2026-08-10; orchestrator-verified] docs/book/10-errors.md's flags…
- [`t0055`](todo/t0055.md) **MED** — 🆕🐛 [MED — Round XXXII round-close revert, filed 2026-08-06] Track B SIBLING D2 (Dict/HashMap put/set + Set.insert Tier 2…
- [`t0056`](todo/t0056.md) **MED** — 🆕🐛 [MED — Round XXXII round-close revert, filed 2026-08-06 · NARROWED 2026-08-07 by Round XXXIV Track C2] Track A SH mir…
- [`t0057`](todo/t0057.md) **MED** — 🆕🧹 [MED — BUILD-TIME / round-close wall clock; measured 2026-08-05] The sweep compiles ~18.6k lines of IDENTICAL self-ho…

- [`t0058`](todo/t0058.md) **HIGH** — 🆕🐛 [HIGH — Core #4 SIBLING-DRIFT class candidates, filed 2026-08-02 by Round XXVI Track D; ride-along HIGH follow-ups to…
- [`t0059`](todo/t0059.md) **LOW** — 🆕📐 [LOW — ggdef SUBSET GAP, Core #9; filed 2026-08-06 by Round XXXII Track A · NARROWED 2026-08-07 by Round XXXIV Track…
- [`t0060`](todo/t0060.md) **HIGH** — 🆕🐛 [HIGH — Track B sibling audit follow-up, filed 2026-08-06 by round MEMORY SAFETY / ONE OWNERSHIP BOUNDARY] register_c…
- [`t0061`](todo/t0061.md) **MED** — 🆕📐 [MED — ggdef SUBSET GAP, Core #9; surfaced 2026-07-25, ⚠ NARROWED 2026-07-30 — the family is now PARTLY in-subset] Th…
- [`t0062`](todo/t0062.md) **HIGH** — 🆕⚠ [HIGH — DESIGN CONSTRAINT discovered by Track-B2 review pass 8, ASan-measured; NOT a live bug — HEAD is correct] Rout…
- [`t0063`](todo/t0063.md) **MED** — 🆕🧹 [MED — dead code / Core #14, measured by Track-B2 review pass 8] exprs/methods.rs:2815 (the consuming-arg fix-up clon…
- [`t0064`](todo/t0064.md) **MED** — 🆕🐛 [MED-HIGH — gg check PASSES then C EMIT REFERENCES AN UNDECLARED FUNCTION; measured 2026-07-26] Binding .enumerate()…
- [`t0065`](todo/t0065.md) **LOW** — 🆕⚖ [LOW — ergonomic consistency, NOT a correctness bug; re-triaged 2026-08-03, supersedes the "Track D reject may be wro…
- [`t0066`](todo/t0066.md) **MED** — 🆕📐 [MED — Layering rule 3 collapse, filed Round XXIX Track B 2026-08-03 as Option C sibling] Collapse the three parallel…
- [`t0067`](todo/t0067.md) **MED** — 🆕🐛 [MED — SH-lane parity gap, filed Round XXX Track D.A.3 2026-08-04] SH lane returns None from gorget_map_swap_remove_o…
- [`t0068`](todo/t0068.md) **HIGH** — 🆕📋 [LOW — Protocol-vs-Rust-convention disagreements, filed Round XXIX Track B 2026-08-03] Three builtin method return-ty…
- [`t0069`](todo/t0069.md) **LOW** — 🆕🧹 [LOW-MED — UX regression, filed 2026-08-01 by Round XXIII Track β Pass-4 fold] Compound-shape D10(a) sites emit 2 err…
- [`t0070`](todo/t0070.md) **LOW** — 🆕📋 [LOW — Core #12 axis-completeness residual, filed 2026-08-01 by Round XXIII Track β Pass-1 fold] 5 un-fixtured operan…
- [`t0071`](todo/t0071.md) **HIGH** — 🆕🐛 [HIGH — Core #8, fresh and unrelated to the &-projection class; measured 2026-07-25] A push onto a TUPLE FIELD of a &…
- [`t0072`](todo/t0072.md) **HIGH** — 🆕🛡 [HIGH — a CENSUS THAT EXISTS ONLY IN /tmp EXHAUST; file before it evaporates. Flagged 2026-07-25 by Track-C review pa…
- [`t0073`](todo/t0073.md) **MED** — 🆕🧹 [MED — three fixtures cited as a guard DO NOT guard; measured 2026-07-25 by Track-B2 review pass 2] The assigns.rs:20…
- [`t0074`](todo/t0074.md) **MED** — 🆕⚠️ [MED — a FAKE-COVERAGE trap inside the "plain struct" cell; measured 2026-07-25] A by-value struct field that CARRIE…
- [`t0075`](todo/t0075.md) **HIGH** — 🆕🐛💥 [HIGH — FIVE MORE LIVE D10 COSTUMES, all gg check-CLEAN; measured 2026-07-25 by Track-A v3 review pass 1] The exclus…
- [`t0076`](todo/t0076.md) **HIGH** — 🆕🛡 [HIGH — the Core #6 guard shape for D10; measured 2026-07-25] A participant-construction guard CANNOT see UNROUTED si…
- [`t0077`](todo/t0077.md) **HIGH** — 🆕⚖️ [HIGH — D10 NEEDS TWO CLAUSES ON TWO AXES, not one; measured 2026-07-25 by the live-range scout] The "everything is…
- [`t0078`](todo/t0078.md) **MED** — 🆕🧹 [MED — missing typed write-through, Layering rule 4; measured 2026-07-25] Operator-overload resolution is recorded NO…
- [`t0079`](todo/t0079.md) **LOW** — 🆕🧰 [LOW — harness crash, will bite any diagnostic-wording change; measured 2026-07-25] tests/integration.rs:19029 slices…
- [`t0080`](todo/t0080.md) **HIGH** — 🆕🐛 [HIGH — SILENT WRONG OUTPUT + Core #4 sibling drift + Core #10; measured 2026-07-25, WIDENED 2026-07-31 by Round XX p…
- [`t0081`](todo/t0081.md) **MED** — 🆕🐛 [MED-HIGH — COMPILER PANIC after gg check passes; measured 2026-07-25 by the Core-#12 coverage audit] The single-owne…
- [`t0082`](todo/t0082.md) **MED** — 🆕🧹 [MED — three fixtures READ as coverage but CANNOT FAIL (Core #12 red-verifiability); measured 2026-07-25] Non-discrim…
- [`t0083`](todo/t0083.md) **HIGH** — 🆕🐛💥 [HIGH — SILENT GARBAGE-VALUE MISCOMPILE, both backends, Core #8 + Core #4; measured 2026-07-25 by the Track-C brief-…
- [`t0084`](todo/t0084.md) **HIGH** — 🆕🐛💥 [HIGH — DUPLICATED USER Drop (D4/D12 single-owner violation), both backends; measured 2026-07-25, same family as the…
- [`t0085`](todo/t0085.md) — 📌 DURABLE REPROS for the &-in-an-OWNING-POSITION class (committed 2026-07-25; RED-verified). FOUR OF THE SEVEN ARE NOW L…
- [`t0086`](todo/t0086.md) **MED** — 🆕🧰 [MED — TEST INFRA, filed 2026-07-25 by the Track-C scout] self_host_bootstrap_fixed_point uses FIXED /tmp/self_host_s…
- [`t0087`](todo/t0087.md) **HIGH** — 🆕🐛 [HIGH — LEAK, both backends, filed 2026-07-25 by the Track-B1 output-review] Re-assigning THROUGH a &-param never dro…
- [`t0088`](todo/t0088.md) **MED** — 🆕🧹 [MED — deferred from Track B1, filed 2026-07-25] Retire the tag_ownership.rs:318-335 ownership-laundering rule (suppr…

- [`t0089`](todo/t0089.md) **HIGH** — 🆕🐛💥 [HIGH — MEMORY-UNSAFE, both backends] An equip method whose body is an EXPRESSION-BODY return of its own OWNING ! pa…

- [`t0090`](todo/t0090.md) **MED** — 🆕🧹 [MED — Layering, typed metadata] The &/!-param auto-deref site reads the WRONG typed bit for "is this an owning param…

- [`t0091`](todo/t0091.md) **MED** — 🆕📐 [MED — charter] A String &-param bind still costs ONE clone where a hand-writer pays ZERO. The other resource types b…

- [`t0092`](todo/t0092.md) **MED** — 🆕🛡 [MED — Core #6 burn-down] Promote ConsumeSiteClass::AssignIntoReturnSlot from the non-fatal assign_warnings partition…

- [`t0093`](todo/t0093.md) **HIGH** — 🆕🐛💥 [HIGH — SOUNDNESS, MEMORY-UNSAFE, both backends, adversarial audit 2026-07-22, parent-reproduced+ASan-confirmed] The…

#### 🆕 R-STRING / SH-CoW ROUND RESIDUALS (filed 2026-07-21 at A+B integration; round-close DONE entries pending Track C)
- [`t0094`](todo/t0094.md) **MED** — [MED — ggdef lag Core #9] Non-Add resource/binary OP static ElabError. Rust + SH reject E_UnsupportedOperator (Wave 1 T1…
- [`t0095`](todo/t0095.md) **HIGH** — 🐛💥 [HIGH — MEMORY-SAFETY, both backends, R1 — box-fix output-review 2026-07-24, parent-reproduced] Moving a Box[T] via !…
- [`t0096`](todo/t0096.md) **HIGH** — 🐛 [HIGH — pre-existing C-emit name collision, BOTH backends, found 2026-07-24 fixing the Box[resource-struct] leak] Box_…
- [`t0097`](todo/t0097.md) **HIGH** — 🐛 [HIGH — pre-existing leak, BOTH backends, found 2026-07-24] Box[R] where R has a USER Drop impl AND a droppable field…
- [`t0098`](todo/t0098.md) — ⚠ CORRECTION (A output-review): the compound-path DOUBLE-EVAL is NOT closed. Any note claiming Target-2/Face-A "closed t…

- [`t0099`](todo/t0099.md) — 🎯 FULL LAZY CoW — the single mental model (owner-confirmed 2026-07-02; continuation of Feb-22 immutable-borrow-args 22b6…


- [`t0100`](todo/t0100.md) **HIGH** — 🆕🐛 [HIGH — LANE GAP, Core #9: the self-host has the FAMILY-1 defect the Rust lane fixed on 2026-07-27. MEASURED with the…

- [`t0101`](todo/t0101.md) **HIGH** — 🆕🐛 [HIGH — SILENT CALL LOSS, gg check clean, BOTH backends; found 2026-07-27 by the Family-1 output review, reproduced b…

- [`t0102`](todo/t0102.md) **HIGH** — 🆕🛡 [HIGH-attention LESSON, no open defect — the arm-set lints have a PERMANENT blind spot; recorded 2026-07-27 after &g.…

- [`t0103`](todo/t0103.md) **MED** — 🆕🧹 [MED — LATENT SILENT-DISAGREEMENT SOURCE, no live defect known; found 2026-07-27 while root-causing the Family-1 auto…


- [`t0105`](todo/t0105.md) **MED** — 🆕🐛 [MED — ACCEPT/REJECT ASYMMETRY between two argument positions, gg check-visible; found 2026-07-27 auditing the Family…
- [`t0106`](todo/t0106.md) **HIGH** — 🆕🐛 [HIGH — SH-lowerer bare-arg CoW gap, surfaced by the Class-C round 2026-07-19; the SH-CoW campaign's headline] The se…
- [`t0107`](todo/t0107.md) **MED** — 🧹 [MED — Class-C residuals 2026-07-19, filed at round close] (1) lib/xtd read-only-&-param sweep — the Class-C burn-down…
- [`t0108`](todo/t0108.md) — 🆕🐛 [D6 — TRACK 2: refcount PARAM / boundary ownership model, filed by the stage-1b wrong-code TRACK-1 executor 2026-07-1…
- [`t0109`](todo/t0109.md) **LOW** — 🆕🐛 [LOW — SH-lag / Core #9 shape-only, not a live SEGV lane; filed 2026-07-28 by Track K] SH lowerer's GtFnPtr(Vector[in…
- [`t0110`](todo/t0110.md) **HIGH** — 🐛💥 [HIGH — ICE, BOTH BACKENDS — A2-R2 M1 scout/executor 2026-07-12; Core-#4 sibling of the closed vector/dict ICE] Custo…

- [`t0111`](todo/t0111.md) — [CoW WAVE-2 landing-1 follow-ups — filed 2026-07-17]

- [`t0112`](todo/t0112.md) **LOW** — 🧹 [LOW — typed-metadata hygiene, filed 2026-07-28 by Round XI Track J] Retire the is_elem_borrow_read name-whitelist via…

- [`t0113`](todo/t0113.md) **LOW** — 🧹 [LOW — Track E2 advisory A1, filed 2026-07-27] GuardAccept lumps Guard/ReadGuard/WriteGuard uniformly; typecheck reads…
- [`t0114`](todo/t0114.md) **LOW** — 🧹 [LOW — Track E2 advisory A2, filed 2026-07-27] E2 axis-coverage samples 10 of the 12 wrapper×face cells. Missing: Writ…

- [`t0682`](todo/t0682.md) **HIGH** — 🆕🚨 [HIGH — A RATIFIED SAFETY RULE IS UNENFORCED AT TWO OF ITS THREE POSITIONS, and the unenforced cases are exactly the…
- [`t0684`](todo/t0684.md) **HIGH** — 🆕🚨 [HIGH — binding Box.get() to a local DOUBLE-FREES; gg check clean; measured 2026-08-27, orchestrator-verified] The tr…
- [`t0698`](todo/t0698.md) **HIGH** — 🆕🚨 [HIGH — READING through an EXPLICIT & BORROW PARAMETER DESTROYS the caller's value; gg check clean; found 2026-08-27…
- [`t0707`](todo/t0707.md) **HIGH** — 🆕🐛💥 [HIGH — CRASH ON A VALID PROGRAM: shared T x = <a local that is still live> ICEs gg build rc 101 with local _1 read…
- [`t0715`](todo/t0715.md) **HIGH** — 🆕📉 [HIGH — a MEASURED +13.08% stage-1 string-clone regression, correctness-required but reclaimable; ceilings re-seeded…
- [`t0872`](todo/t0872.md) **HIGH** — 🆕🐛 [HIGH — a 6-byte leak on EVERY inline-constructor receiver, and on a plain struct-ctor field read; isolated 2026-08-3…
- [`t0873`](todo/t0873.md) **HIGH** — 🆕🚨 [HIGH — TWO defects on Vector[Callable], found 2026-08-31 by the R48 Track-B1 brief-review pass 1 while probing the r…
- [`t0907`](todo/t0907.md) **HIGH** — 🆕🚨💥 [HIGH — MEMORY-UNSAFE, gg check rc 0, and BOTH VALUE LANES PRINT THE RIGHT ANSWER; found 2026-08-31 by R48 Track D2…
- [`t1048`](todo/t1048.md) **HIGH** — 🚨 [HIGH — MEMORY, both routes measured; found 2026-09-03 by the R49 Track K brief-review gauntlet, executor-verified at…
- [`t1088`](todo/t1088.md) **HIGH** — 🚨 [HIGH — a Set[T] stored as a Vector element double-frees: rc 134 on BOTH backends, no higher-order call involved; foun…
- [`t1089`](todo/t1089.md) **HIGH** — 🚨 [HIGH — Vector[Option[String]] element READ double-frees the payload String: rc 134 on BOTH backends; found 2026-09-04…
- [`t1067`](todo/t1067.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — rc 0 with SILENTLY WRONG OUTPUT, AddressSanitizer: heap-use-after-free under --sanitize; found R49 Track…
- [`t1210`](todo/t1210.md) **HIGH** — 🆕🚨 [HIGH — a LEAK of EVERY heap value a closure captures, from ordinary safe syntax, gg check clean and rc 0 on both bac…
- [`t1225`](todo/t1225.md) **HIGH** — 🆕🚨 [HIGH — MEMORY-UNSAFE FROM ORDINARY SAFE SYNTAX, both backends, gg check rc 0 AND gg build rc 0; the DEFERRED half of…
- [`t0952`](todo/t0952.md) **HIGH** — 🆕🐛 [HIGH (re-graded from MED 2026-09-05) — a LEAK *and* an O(n) deep copy per iteration step, from ordinary safe syntax,…
- [`t1383`](todo/t1383.md) **HIGH** — 🆕🐛 [HIGH — SILENT WRONG OUTPUT, BOTH BACKENDS, ORACLE-ADJUDICATED, gg check CLEAN; found 2026-09-05 by the R50 Track G s…
- [`t1309`](todo/t1309.md) **HIGH** — 🆕🐛 [HIGH — BOTH lanes, leak at scope exit, gg check clean; filed 2026-09-05 by R50 Track A1 as the OTHER HALF of t1077;…
- [`t1313`](todo/t1313.md) **HIGH** — 🆕🚨 [HIGH — RUST lane, BOTH backends, DOUBLE FREE from SIX LINES using the language's own move operator; gg check clean;…
- [`t1404`](todo/t1404.md) **HIGH** — 🆕📐 [HIGH — SILENT LOST WRITE FROM A RATIFIED SPELLING, both backends, gg check clean; ⚖ R2's RATIFIED PREREQUISITE (owne…
- [`t1364`](todo/t1364.md) **HIGH** — 🆕🚨 [HIGH — A RUN THAT NEVER ERRORS TRAPS T_UnwrapNone. Both Rust backends; the SELF-HOST LANE IS CORRECT, so the oracle…
- [`t1388`](todo/t1388.md) **HIGH** — 🆕🚨 [HIGH — A BARE LOCAL BIND OF A REFCOUNT HANDLE CORRUPTS THE SOURCE AND FABRICATES THE DESTINATION. No call, no contai…
- [`t1331`](todo/t1331.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT FROM ORDINARY SAFE SYNTAX; gg check rc 0, build rc 0, run rc 0, ASan CLEAN; found 2026-09…
- [`t1428`](todo/t1428.md) **HIGH** — 🆕🚨 [HIGH — AN UNBOUNDED LEAK IN fold's ACCUMULATOR SLOT, ON BOTH BACKENDS. Found by R50 Track K's scout while triaging t…
- [`t1349`](todo/t1349.md) **HIGH** — 🆕🚨 [HIGH — A LATENT DOUBLE-FREE THAT IS CURRENTLY MASKED BY A LEAK, AND THE LEAK'S FIX UNMASKS IT. Found 2026-09-05 by R…
### Medium

- [`t0115`](todo/t0115.md) **MED** — 🆕🐛 [MED — COMMENT MISATTRIBUTION, PRE-EXISTING on both lanes; found 2026-08-19 by the R43 Track G output review, executo…

- [`t0116`](todo/t0116.md) **MED** — 🆕⚖️ [MED — OWNER-RULED during the paused CoW-aliasing round (between 2026-07-22 and 07-24; exact date not recovered from…
- [`t0117`](todo/t0117.md) **MED** — 🆕🧹 [MED — OWNER-RULED during the paused CoW-aliasing round (between 2026-07-22 and 07-24; exact date not recovered — do…

- [`t0118`](todo/t0118.md) — 🆕 ROUND-8 FOLLOW-UP (filed 2026-06-28, from the get_or ownership executor ac9980be):

- [`t0119`](todo/t0119.md) — 🆕 ROUND-7 FOLLOW-UPS (filed 2026-06-28, from the Option-combinator scout a96dc0e2 + the Dict.filter executor a3c3d622):

- [`t0120`](todo/t0120.md) — CoW TRACK (lazy borrow-by-default, #2 priority per owner): L1 (position-gated borrow-flip, RUN-proven reachable lower.gg…

#### 🆕 CoW 2G FOLLOW-UPS (filed 2026-07-18; both lanes' 2G fixes are LANDED — the items below are the siblings and gaps the fixes exposed, each with a `known_gaps/` fixture asserting the INTENDED output)
- [`t0122`](todo/t0122.md) **MED** — [MED — self-host lane lag, Core #9] The SH CoW scan never marks self — cow_loop_bare_param_self_field stays in known_gap…
- [`t0123`](todo/t0123.md) **LOW** — [LOW — SH over-materialization; scout residual] method_mutates_receiver's step-3 conservative default over-marks READ-ON…
- [`t0124`](todo/t0124.md) **MED** — [MED — WRONG-CODE; comprehension-loop sibling of 2G, LANE-ASYMMETRIC] The 4 comprehension emitters emit header/body/incr…
- [`t0126`](todo/t0126.md) **LOW** — [LOW — ggdef subset gap, Core #9] String.push_char (+ the other 8 typed is_mutating builtins the old MUTATING_METHODS li…
- [`t0127`](todo/t0127.md) **LOW** — [LOW — shared under-approx, PRE-EXISTING in BOTH the pre-2G walker and the prescan] cow_after_expr_moves does not recurs…
- [`t0128`](todo/t0128.md) — [move-despite-later-read on a Shared-containing struct — RUST LANE FIXED c0c5d59c (2026-07-20); residuals only.] The Rus…
- [`t0129`](todo/t0129.md) **MED** — [MED — SH lane, Core #9; G4-review-PROVEN] SH field_clone_c is missing the Weak/Channel retain arms (the same class G4 j…
- [`t0130`](todo/t0130.md) **LOW** — [LOW — coverage] An exercising fixture that reaches emit_recursive_enum_clones with a refcount payload. The G4 fix cover…
- [`t0131`](todo/t0131.md) **LOW** — [LOW — G3 convergence note] When the MaterializeReason/typed-field work lands, migrate refcount_field_retain_fn (emit_ty…
- [`t0132`](todo/t0132.md) **MED** — [MED — self-host emit] Shared[Vector[T]].get() synthesized wrapper references the payload C type (__gg_Vector__int64_t)…

#### 🆕 ROUND-38 FOLLOW-UPS + DISCOVERED RESIDUALS (filed 2026-07-04; T-B landed = self-host `&self` MUTATION-INFERENCE pass — `compute_method_mutates_self` classifies each non-generic equip method read-only-vs-writes-self via a monotone fixpoint over self-callee edges, and the named-receiver CoW gate materializes ONLY genuine mutators. Both the SCAN (`mutinf_scan_expr`) and the lower_expr GATE are USER→BUILTIN order (Core #4), so a user `&self`-mutator NAMED like a read-only builtin (`get`/`map`/`peek`/…) is classified correctly for BOTH a RESOLVABLE named receiver AND a RESOLVABLE projected receiver (`v[i].get()` / `s.v[i].get()` / `o.inner.get()`) — the GATE's name-collision guard resolves the projected element/field type via `mutinf_recv_type_name` (the typed local slot + `index_value_type_name` + `GirTypeInfo.fields`) and reads the PRECISE `method_mutates_self` map (bomb-safe: a genuine projected builtin `v[i].len()` resolves to `Elem__len`, absent → no clone). Closes the R37 T1 named-user-`&self`-mutator write-through divergence for NON-GENERIC equips [see the ROUND-37 item below]; +5 MATCH fixtures; measured peak self-compile RSS ~625-628 MB == BASE, self-compile wall-time ~128.9s == BASE (NOT the 14GB bomb). New `gir.gg` field `method_mutates_self`; `lower.gg` mutinf_* walker+fixpoint + projected-type resolver + two arm-count lints; `lower_expr.gg` gate wiring + name-collision guard (named + projected).)
- [`t0133`](todo/t0133.md) **HIGH** — [HIGH — self-host latent FOOTGUN; from T-B] apply_collect_target_rewrites (lower.gg:2665) RESETS equip-method self param…
- [`t0135`](todo/t0135.md) **LOW** — [LOW — self-host, Rust; T-B residual, Core #8] name-collision on an UNRESOLVABLE projection stays read-only (both the SC…
- [`t0136`](todo/t0136.md) **LOW** — [LOW — self-host, Rust; T-B residual] self reborrowed into a local then mutated is not traced by the scan. Holder r = se…
- [`t0137`](todo/t0137.md) **LOW** — [LOW — self-host; T-D adv(c) deferred capability, NOT a regression] scalar/struct-field & element &-bind write-through i…

- [`t0138`](todo/t0138.md) — 🧩 CoW MATERIALIZATION — durable reference (THE RULE + GROUND TRUTH).

- [`t0139`](todo/t0139.md) — [Planner round 3, 2D warning-track — D2-rider] Extend the dead-write diagnostic to METHOD-ROOTED dead temps. The 2D clos…

- [`t0140`](todo/t0140.md) **MED** — 🧹 [MED — MaterializePlan campaign follow-up, filed 2026-07-28 (owner-suggested during Track I close)] Extend auto-move-a…

- [`t0141`](todo/t0141.md) — ensure_owned_at_boundary migration — remaining specialized sites. 5 remaining each have logic beyond pure boundary-clone…

- [`t0752`](todo/t0752.md) **MED** — 🆕🧹🔍 [MED — cow_sever_all_aliases_from IS NEVER ENTERED BY THE ENTIRE ALIAS CORPUS, and its two conditions may be MUTUALL…
- [`t0790`](todo/t0790.md) **MED** — 🆕🐛 [MED — LEAK, gg check clean and stdout CORRECT; found 2026-08-29 by R47 Track E2's brief-review pass 1, re-measured b…
- [`t0792`](todo/t0792.md) **MED** — 🆕🐛 [MED — WRONG ANSWER, gg check clean, both backends, ASan-clean; found 2026-08-29 by R47 Track E2's executor while clo…
- [`t0860`](todo/t0860.md) — [MED — owner ruling 2026-08-30, mechanism landed R48 Track F, TRIGGER STILL MISSING] Wire the
- [`t0862`](todo/t0862.md) — [MED — owner-directed 2026-08-31, R48 candidate] slice MATERIALIZES AT THE READ SITE ON BOTH RECEIVERS.
- [`t0880`](todo/t0880.md) **MED** — 🆕🐛 [MED — a stdout-invisible 5-byte leak on every Result.unwrap_or with a heap default; found R48 Track A while widening…
- [`t0948`](todo/t0948.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, both backends, gg check clean; and the code comment that licenses it asserts…
- [`t0949`](todo/t0949.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, BOTH backends, gg check clean; found 2026-09-02 by the R48 Track R POST-HOC…
- [`t0951`](todo/t0951.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, BOTH backends, gg check clean; found 2026-09-03 by R48 Track T-a1 while ASan…
- [`t0953`](todo/t0953.md) **MED** — 🆕🐛 [MED — a LEAK from the most ordinary syntax in the language, BOTH backends, gg check clean; found 2026-09-03 by R48 T…
- [`t0967`](todo/t0967.md) **MED** — 🆕⚖️ [MED — A RATIFIED REJECT IS UNIMPLEMENTED, *and* the measurement the ruling rests on did not
- [`t0992`](todo/t0992.md) **MED** — 🆕🧹 [MED — Core #2 / Layering rule 2 violation, NAMED VERBATIM by CLAUDE.md § "No name matching"
- [`t1076`](todo/t1076.md) **MED** — 📐 [MED — D47's "enumerate the boundary set" discharged as a DURABLE TABLE rather than a brief; filed 2026-09-04 by R49 T…
- [`t1071`](todo/t1071.md) **MED** — 🆕🧹 [MED — LAYERING RULE 3, a second source of truth on the ownership axis, with NO measured live defect today; found R49…
- [`t1290`](todo/t1290.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax on BOTH backends, gg check clean and rc 0 with correct stdout; found 2026-09-…
- [`t1216`](todo/t1216.md) **MED** — 🆕⚡ [MED — a CoW charter breach with a USER-OBSERVABLE signature: flat_map runs the result element's Drop body TWICE per…
- [`t1365`](todo/t1365.md) **LOW** — [MED — NOT A DEFECT. This is the durable record of what R50 Track F1r deliberately did NOT run or write, so that "nothin…
### Low

- [`t0142`](todo/t0142.md) **LOW** — 🆕📋 [LOW — GUARD SHAPE, measured four times in one track; filed 2026-08-19 by R43 Track G] doc_source_citations_name_the_…

- [`t0143`](todo/t0143.md) **LOW** — 🆕📋 [LOW — OPEN DESIGN QUESTION, filed 2026-08-19 by R43 Track G because its code comment asserted a filing that did not…

- [`t0144`](todo/t0144.md) **LOW** — 🆕📋 [LOW — DOC ROT, whole-file; found 2026-08-19 by R43 Track G while refreshing the three formatter rows] Every LOC figu…

- [`t0145`](todo/t0145.md) **LOW** — 🆕📋 [LOW — LEDGER WRITE-THROUGH, orchestrator's file; found 2026-08-19 by R43 Track G] Two docs/define-gorget/decisions.m…

- [`t0146`](todo/t0146.md) — 🐛 LATENT LIMIT (CoW, ⊆ tier-1 whole-fn-scan, NOT L1-introduced; low-pri). A closure that captures+mutates a collection i…

- [`t0147`](todo/t0147.md) — Deferred String materialization — Site #4 (borrow-checker decidability) [LOW]. The lifetime question — "can we staticall…

- [`t0148`](todo/t0148.md) — Replace auto-borrow with explicit reference semantics: Phase 1 done (const_params). Phase 2 (const propagation) not star…

- [`t0149`](todo/t0149.md) — Collection Resource semantics: remaining call-site ownership gaps: borrow checker doesn't cover field assignment or meth…

- [`t0150`](todo/t0150.md) — 🧹 [1B Fable output-review 2026-07-16, LOW-MED — pre-existing, self-host only] Self-host compound v[i].f += n evaluates b…
- [`t0151`](todo/t0151.md) — 🧹 [Track D advisory A1 residual 2026-07-27, LOW — self-host only] amp_object_base's EIndex arm emits lower_index_element…

- [`t0152`](todo/t0152.md) **LOW** — 🧹 [LOW — Track F sibling audit, filed 2026-07-28; latent perf shape, not a bug today] lower_call_arg MutableBorrow arm (…

- [`t0153`](todo/t0153.md) **LOW** — 🧹 [LOW — Track I measurement scout sibling, filed 2026-07-28; latent perf shape, not a bug today] lower_call_arg Move if…

- [`t0154`](todo/t0154.md) **LOW** — 🧹 [LOW — Track I measurement scout sibling, filed 2026-07-28; latent perf shape, not a bug today] lower_call_arg G2 proj…

- [`t0761`](todo/t0761.md) **LOW** — [LOW — PRECISION residual, not a correctness one; measured while closing t0699] Call sites inside meta-expanded bodies g…
- [`t1307`](todo/t1307.md) **LOW** — 🆕⚖ [LOW — A DESIGN RECORD, NOT A DEFECT. ⚠ Filed HIGH and CORRECTED TO LOW by the owner 2026-09-05: *"It's documentation…
- [`t1363`](todo/t1363.md) **LOW** — [LOW — LAYERING RULE 3 ("one source of truth per axis") vs. a MEASURED cost. Not a behavioural defect: no known program…
- [`t1450`](todo/t1450.md) **LOW** — 🆕 [LOW — A NAMED OMISSION THAT WAS ABOUT TO DIE WITH ITS BRIEF. Recovered 2026-09-05 by the R50 orchestrator during the…
## Self-host parity

- [`t0155`](todo/t0155.md) **HIGH** — 🆕🐛 [HIGH — SELF-HOST LANE MEMORY-SAFETY, measured 2026-08-26 in the R44 Track K fix-up; this is the ROOT of Track K's "s…

- [`t0156`](todo/t0156.md) **LOW** — 🧹 [LOW — Core #4 residual, NO repro (a wrong spelling fails LOUDLY at link, not silently at rc 0); named by R44 Track K…

- [`t0157`](todo/t0157.md) **MED** — 🆕🐛 [MED — Core #9 LANE DIVERGENCE ON ACCEPTANCE, the SH lane OVER-REJECTS; measured 2026-08-23 by R44 Track B, re-verifi…
- [`t0158`](todo/t0158.md) **HIGH** — 🆕🚨 [HIGH — Core #9 LANE GAP IN A GUARD, not in code; measured 2026-08-22, owner-scoped as R44 Track F] The SELF-HOST TYP…
- [`t0159`](todo/t0159.md) **HIGH** — 🆕🐛 [HIGH — R42's UNPORTED PARITY INFLOW, the exact debt the reseeded ceiling now records; filed 2026-08-22] Three fixtur…
### High
- [`t1055`](todo/t1055.md) **HIGH** — 🆕🐛 [HIGH — ⚠ THE DISPOSITION IS PER-CELL, AND ONE ROW IS A LIVE MISCOMPILE. Filed 2026-09-04 by R49 Track A1-IDENTITY as…
- [`t0160`](todo/t0160.md) **HIGH** — 🚨 [HIGH — Core #4 CLASS, BOTH LANES, TWO SEVERITY TIERS] for … else / while … else: 18 walker arms still swallow else_bo…

- [`t0161`](todo/t0161.md) **HIGH** — 🚨 [HIGH — the method-targ recorder class: the PRODUCER and the proto_* CONSUMER halves are CLOSED (R44 Track D); FOUR re…

- [`t0162`](todo/t0162.md) **MED** — 🆕🐛 [MED-HIGH — Core #9 LANE GAP, measured 2026-08-05 by the round-close battery] The SELF-HOST mis-lowers an iterator te…

- [`t0163`](todo/t0163.md) **HIGH** — 🆕🐛 [HIGH — silent WRONG output, SELF-HOST LANE ONLY; filed 2026-07-29 by Round XIV ride-along scout] A top-level static…

- [`t0164`](todo/t0164.md) — ⭐ PENDING PARITY BACKLOG (RE-MEASURE via the command at the top of this file before acting; the non-MATCH set = the back…

- [`t0165`](todo/t0165.md) — 🆕 stdlib_io / trait-dispatch RESIDUAL ROOTS (Root C + Root D; round-40-T-B-filed, still CC-FAIL after round-41 S-A close…

- [`t0166`](todo/t0166.md) — 🆕 ROUND-41 RESIDUALS FILED (from S-A/S-B/S-D landing + the S-B/S-C scouts' measured DEEP fragmentation; each own scout,…
- [`t0167`](todo/t0167.md) **HIGH** — 🐛 S-B: the "X from int" return-type cluster is ~6 DISTINCT DEEP roots (NOT one shared gap — measured). Still CC-FAIL: (a…

- [`t0168`](todo/t0168.md) — 🆕 ROUND-42 RESIDUALS FILED (from R42-A/C/D landing + the R42-B/C/D scouts' measured DEEP fragmentation; each own scout,…

- [`t0169`](todo/t0169.md) — 🆕 ROUND-39 T3 DCE intel (future-round candidates) — self-host DCE/monomorph DROPS a called user fn: 6 REMAINING roots [r…

- [`t0170`](todo/t0170.md) **HIGH** — 🆕🐛 [HIGH — parity WRONG newcomers, from the 2026-07-16 battery regen; PRE-EXISTING lane gaps that entered the corpus via…

- [`t0741`](todo/t0741.md) **HIGH** — 🆕🐛 [HIGH — latent today, but it is the reason a shipped safety check has to be weaker than its Rust counterpart; found R…
- [`t0712`](todo/t0712.md) **HIGH** — 🐛 [HIGH — five live self-host miscompiles, one of them a deterministic SIGBUS on ordinary beginner code; Rust gg is corr…
- [`t0823`](todo/t0823.md) **HIGH** — [HIGH — the self-host's int32 → void* emit is not a portability nuisance, it is an endemic LIR→C TYPE-FIDELITY defect, a…
- [`t0922`](todo/t0922.md) **HIGH** — 🆕🚨 [HIGH — SELF-HOST CC-FAIL: a generic FREE function's body is never emitted, so the emitted C does not LINK; found 202…
- [`t0923`](todo/t0923.md) **HIGH** — 🆕🚨 [HIGH — SELF-HOST SILENT WRONG OUTPUT, not a crash: a static TRAIT method returning a String returns its LENGTH; foun…
- [`t0903`](todo/t0903.md) **HIGH** — 🆕🐛 [HIGH — LAGGING LANE (Core #9); the Rust lane is now CORRECT and the self-host still SEGFAULTS; found 2026-08-31 by R…
- [`t0931`](todo/t0931.md) **HIGH** — 🆕🚨 [HIGH — self-host lane; the LAST row of EXPECTED_NONDETERMINISTIC, found 2026-08-31 by R48 Track γ] health.each((Enti…
- [`t0941`](todo/t0941.md) **HIGH** — 🆕🐛 [HIGH — Core #9 LANE DIVERGENCE, under-rejection: self-host ACCEPTS a D53 unique-lock copy that Rust rejects] variant…
- [`t0944`](todo/t0944.md) **HIGH** — 🆕🐛 [HIGH — Core #9 LANE DIVERGENCE, under-rejection: self-host ACCEPTS a D4/D12 drop-tainted copy that Rust rejects] inf…
- [`t0959`](todo/t0959.md) **HIGH** — 🆕🐛💥 [HIGH — Core #9 LANE LAG, MEMORY-UNSAFE, gg check ACCEPTS; measured 2026-09-03 by R48 Track U while retiring the cal…
- [`t1118`](todo/t1118.md) **HIGH** — 🆕🐛 [HIGH — self-host SEGV on a program Rust gg now compiles and runs correctly. Found 2026-09-04 by R49 Track A2-α while…
- [`t1078`](todo/t1078.md) **HIGH** — 🆕🔧 [HIGH — SELF-HOST LANE, C-EMIT: the self-host types a resource FIELD READ feeding a Box mint as void*, so cc rejects…
- [`t1129`](todo/t1129.md) **HIGH** — 🆕🚨 [HIGH — the self-host's builtin-vs-user discriminator IS ALWAYS TRUE: every top-level def is registered with the dumm…
- [`t1084`](todo/t1084.md) **HIGH** — 🆕🚨 [HIGH — SELF-HOST LANE DEBT, Core #9: R49 Track M2 landed a semantic change on the Rust C and LLVM lanes and the self…
- [`t1310`](todo/t1310.md) **CRITICAL** — 🆕🚨 [CRITICAL — SELF-HOST lane, DOUBLE FREE, gg check clean; found 2026-09-05 by R50 Track A1 while measuring the t1077 l…
- [`t1311`](todo/t1311.md) **HIGH** — 🆕🐛 [HIGH — SELF-HOST lane, the self-host emits C that cc rejects; filed 2026-09-05 by R50 Track A1; BLOCKS graduating t1…
- [`t1429`](todo/t1429.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT ON THE SELF-HOST LANE, rc 0 AND gg check CLEAN. Found by R50 Track K's scout.] Option[Str…
- [`t1442`](todo/t1442.md) **HIGH** — 🆕🚨 [HIGH — THE SAME UNDEFINED BEHAVIOUR, IN THE LANE THAT IS MEANT TO BECOME THE PRIMARY REFERENCE. Found 2026-09-05 by…
### Medium

- [`t0171`](todo/t0171.md) **MED** — 🆕 [MED — self-host lane gap, Core #9; R40 Track B] The 3 driver-embedded lexer copies lack the \xHH arm + unknown-escape…


- [`t0172`](todo/t0172.md) **MED** — 🆕⚠ [MED — R39 close-time discovery, test-infra] sh_bootstrap_stage2_double_free_after_fmt_sweep needs a SCRATCH-TREE imp…

- [`t0173`](todo/t0173.md) **MED** — 🆕🐛 [MED — Core #9 lane gap, found + measured by R41 T-RB0 2026-08-11] The self-host resolver records NO RES edge for the…
- [`t0174`](todo/t0174.md) **LOW** — 🆕🐛 [LOW — R39 Phase 2e process, comparison-test coverage gap] parser_comparison / lowerer_comparison / typechecker_compa…

- [`t0175`](todo/t0175.md) **LOW** — 🆕⚠ [LOW — R39 close-blocker fold follow-up A, filed 2026-08-09 by output-review] self_host_parser/parser.gg asymmetry: s…

- [`t0176`](todo/t0176.md) **LOW** — 🆕⚠ [LOW — R39 close-blocker fold follow-up B, filed 2026-08-09 by output-review] SH slice arm at self_host_lowerer/lower…

- [`t0177`](todo/t0177.md) **LOW** — 🆕🐛 [LOW — Round XVI residual, ONE remaining] 3B next-layer typing. ExecResult field projection I64; X25519 block-param I…


- [`t0178`](todo/t0178.md) **LOW** — 🧹 [LOW — SH lane parity, Track I follow-up, filed 2026-07-28] Port the Rust W_* warning set (NeedlessMutableBorrow, Dead…

- [`t0179`](todo/t0179.md) **LOW** — 🧹 [LOW — Layering debt, Round XIII Track W follow-up, filed 2026-07-29] SH getter-slot producer should emit LoBorrowed(G…

- [`t0180`](todo/t0180.md) **LOW** — 🧹 [LOW — SH shape audit, Round XIII Track W follow-up, filed 2026-07-29] x in coll → Type__contains at SH lower_expr.gg:…

- [`t0181`](todo/t0181.md) — 🧹 SELF-HOST SMELL (Core #4 sibling-drift) — lower_equip_block inlines a hand-synced COPY of lower_function's param loop.…

- [`t0182`](todo/t0182.md) — 🆕 NAME-SLICE SIBLING SITES (corrected enumeration, round-32 C-track scout — the old "Thread.join is the last name-slice…

- [`t0183`](todo/t0183.md) — 🆕 ROUND-9 FOLLOW-UP (filed 2026-06-28, from the DEEP-meta output-review ac8c0e28):

- [`t0184`](todo/t0184.md) — 🆕 ROUND-6 FOLLOW-UPS (filed 2026-06-28):

- [`t0185`](todo/t0185.md) — ★★★ TRIAGE round-2 (scout a2c1746e, 2026-06-20 — RUN-confirmed at 677/1046):

- [`t0186`](todo/t0186.md) — ★★ TRIAGE round-1 (scout a561c163, 2026-06-20 — RUN-confirmed at 668/1044):

- [`t0187`](todo/t0187.md) — ★ TRIAGE-RANKED PARITY (triage scout a0cad9ad 2026-06-17). Clean one-arm 20-fixture wins are GONE — every high-count clu…

- [`t0188`](todo/t0188.md) — METHOD-GENERIC MONO + meta_* — NO bounded quick-win follow-up left (earlier rounds in DONE.md); what remains:

- [`t0189`](todo/t0189.md) — ① ITERATOR family — remaining gaps (the chain-link work landed, see DONE.md). REMAINING (each its OWN scout, RUN-verify)…

- [`t0190`](todo/t0190.md) — DEFERRED behind ①/CoW (next picks once those land — all detailed in ## High Priority): enum_category migration Phase 2 (…

#### 🆕 ROUND-37 FOLLOW-UPS + DISCOVERED BUGS (filed 2026-07-04; R37 landed on `a610d925` = T2A genparam-subst · T2B generic-discovery · T5 README-CoW · snag#2 name-match · snag#1 !-move-param · T1 CoW-tail-S3/S4/S5-1 +6 · T3+T4 primitive-.display+fill. Parity 1052→1068 = 90.4%→91.3%, both backends 1504/0/3. gorget-arena snags #1/#2 RESOLVED → DONE; R36-D session-2 + R36-C fill + R36-B S3/S4/S5-1 landed.)
- [`t0191`](todo/t0191.md) **MED** — [MED — Rust, PRE-EXISTING; from snag#1 bonus] GIR-validator panic on loop-reassigned !-param pushed into a collection —…
- [`t0192`](todo/t0192.md) **MED** — [MED — Rust; from snag#1] precise not-reassigned-!-param liveness gate — is_single_use is conservative (a multi-consume…
- [`t0193`](todo/t0193.md) **MED** — [MED — self-host, from T3+T4] Dict.get_or_put(k, bare None) still CC-FAILs on the deeper __gg_Option__int64_t bug (the 4…
- [`t0194`](todo/t0194.md) **LOW** — [LOW — Rust, PRE-EXISTING; from snag#1] Box[T:resource] drop-glue leak — a resource-with-String, boxed (return Box(item)…
- [`t0195`](todo/t0195.md) **LOW** — [LOW — Rust; from snag#1] String-!-into-collection leak — pass-3 reported a 7B baseline leak; the executor's specific he…
- [`t0196`](todo/t0196.md) **LOW** — [LOW — Rust; from T2A] nested generic-param field falls through — struct Bag[A]: Vector[A] items; b.items resolves to er…
- [`t0197`](todo/t0197.md) **LOW** — [LOW — Rust, cosmetic; from snag#1] src/ir/lowering/context.rs:2267-2268 redundant resolved == owned_string_type || reso…

#### 🆕 ROUND-36 FOLLOW-UPS + DISCOVERED BUGS (filed 2026-07-03; R36 landed on `0dca772b` = B self-host-CoW-session-2 +12 · C Rust-bare-None-value Core#8 · D generic-struct-concrete-field-reject Core#8 · E unwrap/clone/join/static leak-class Core#4. Parity 1039→1052 = 89.4%→90.4%, both backends 1496/0/3. R35 items RESOLVED by round-36 → DONE: Dict.put(bare None)→C, generic-struct-field-silent-miscompile→D, Option-unwrap-leak→E, self-host-CoW-tail-session-1→advanced by B; the round-34 C#2 Option[Resource]-leak booking NARROWED (shared_weak/shared_struct now full parity).)
- [`t0198`](todo/t0198.md) **MED** — [MEDIUM — self-host, CoW-INDEPENDENT; filed by R36-B ASan] for-loop-inline-iterable-literal drop LEAK. for k in [1,2,3]:…
- [`t0199`](todo/t0199.md) **MED** — [MEDIUM — Core #8-adjacent typecheck gap, BOTH compilers; from R36-C output-review] builtin collection value args are no…
- [`t0200`](todo/t0200.md) **LOW** — [LOW — pre-existing, HARD-ERROR not silent; from R36-C v2-pass-2] Dict.update(key, val) 2-arg form arity hole. Typecheck…
- [`t0201`](todo/t0201.md) **LOW** — [LOW — pre-existing, HARD-ERROR not silent; from R36-C v2-pass-2] Channel.send(bare None). send is in the (unchanged) pu…
- [`t0202`](todo/t0202.md) **MED** — [MEDIUM — self-host, pre-existing; surfaced by R36-E ASan sweep] arena_escape_*_error negative fixtures are COMPILED ins…

#### 🆕 ROUND-35 FOLLOW-UPS + DISCOVERED BUGS (filed 2026-07-03; R35 landed = T1 Option-drop leak+double-free · T3 basic-WRONG +4 · T4 self-host-CoW-slice +2 · T5 STATIC[i]=x + compound-leak · T6 gorget-js-snag-13 · T2 Slice-A named-fn-HOF +1) — ⚠ items 215/216/218/219 RESOLVED by round-36 (→ DONE.md)
- [`t0203`](todo/t0203.md) **MED** — [MED — Core #5 SELF-CORRECT 2026-07-03, R36-A scout] The "parity-metric undercounts ~21" claim (round-35 T2b) was FALSE…
- [`t0205`](todo/t0205.md) **LOW** — [LOW — self-host, T1 residual] BUG C: struct Box2 { Shared[int] s } emits Box2__drop but the struct LOCAL is never drop-…

#### 🆕 ROUND-34 FOLLOW-UPS (filed 2026-07-03; R34 landed = A static-C-data −7.56% C + B G2 &-formation-materialize + C#2 self-host Weak upgrade/downgrade + D#2 primitive-equip-reject)
- [`t0206`](todo/t0206.md) **MED** — [MED — Core #8, from C#2; NARROWED by R36-E 2026-07-03] Option[Resource] drop-elaboration leak (self-host; Rust CLEAN).…
- [`t0207`](todo/t0207.md) **MED** — [MEDIUM] Self-host doesn't reject type X = <scalar>; equip X with Trait (Rust DOES via meta-inline). collect_equip (self…

- [`t0208`](todo/t0208.md) **LOW** — [LOW — T5 follow-up, pass-1 R4, non-blocking] Dedup the three "materialize a static GlobalRef receiver into a local" sit…
- [`t0209`](todo/t0209.md) **LOW** — [LOW] C#2 latent Weak siblings (not reachable in shared_weak/shared_struct): Shared.at→elem uses the I64 default (wrong…
- [`t0210`](todo/t0210.md) **LOW** — [LOW — Core #6] Track A regression-fixture ratchet: commit guards for empty-nested-Vector (.data=NULL), Custom-drop-elem…
- [`t0211`](todo/t0211.md) **LOW** — [LOW] Track A broader user yield: the conservative "any method on a static receiver → imperative" block means only for/i…

- [`t0212`](todo/t0212.md) — 🐛 [round-33 alloc scout filing; LOWERING half DONE R39-T4] SELF-HOST mirror: builtin-ctor named-arg VALIDATION (typechec…

- [`t0213`](todo/t0213.md) — 🧹 RUNTIME-ARG ABI is a hand-maintained name+index allow-list — derive it from the typed runtime sig registry (filed by t…

- [`t0214`](todo/t0214.md) — 🐛 SELF-HOST — !-param double-drop at exit (owning_param_drop_at_exit; CORRECTED diagnosis, 2026-06-26). ⚠ Earlier triage…

- [`t0215`](todo/t0215.md) — 🐛 SELF-HOST — snag51 closure-call ABI mirage (filed by A_closure scout a96850ea, 2026-06-24; INDEPENDENT of the lexer/Bo…

- [`t0216`](todo/t0216.md) — 🧹 EnumInit SSOT follow-ups (the type-aware enum-variant-ctor resolution itself shipped on BOTH compilers — Rust 847e767b…

- [`t0217`](todo/t0217.md) — 🐛 SELF-HOST — extern PARAM-type registration + borrowed→clone consumer (the residuals after 239083f2). The entry-module…

- [`t0218`](todo/t0218.md) — 🐛 SELF-HOST PARITY BACKLOG (surfaced by the 718/1066 runtime_diff, 2026-06-22 — re-confirm each by RUNNING): (1) MATCH-G…

- [`t0219`](todo/t0219.md) — 🗺 PARITY MAP — LIVE = 800/1083 = 73.9% [re-measured 2026-06-26 round-7 from self_host_runtime_diff at tip faf7fadb (+8:…

- [`t0220`](todo/t0220.md) — 🐛 SELF-HOST PARITY GAPS pinned by Chain D (pre-existing miscompiles): (a) fstring_binary_spec_leak — emits bin=10 vs Rus…

- [`t0221`](todo/t0221.md) — 🐛 self-host miscompiles a Vector[T]-of-Box passed as a fn PARAM and read back (scout a3de43194cae82a02 RUN + review a912…

- [`t0222`](todo/t0222.md) — 🧹 ENUM-CATEGORY MIGRATION — Phase 2: burn the Option/Result prelude name-matches to 0 (~17 remain; ratchet no_growth_in_…

- [`t0223`](todo/t0223.md) — 🎯 PARITY FAMILY — (T){0} type-loss / aggregate-as-I64 (scout a09db088; in lower.gg/infer.gg). The C-emit (T){0} give-up…

- [`t0224`](todo/t0224.md) — 🐛 self-host match-arm binding of a STRING/resource payload from an owned scrutinee CLONES but never DROPS the bound clon…

- [`t0225`](todo/t0225.md) — 🐛 option_result_combinators CC-FAIL→CRASH: the Option/Result closure-COMBINATOR family (.unwrap_or_else/.filter/.map/.an…

- [`t0226`](todo/t0226.md) — 🧹 self-host fossil audit DEFERRED (#4/#5; guarded by DIAGNOSTIC resolver_comparison/parser_comparison — re-check MATCHED…

- [`t0227`](todo/t0227.md) — 🐛 gorget-js Families B+C — REMAINING = self-host port (Chain 2). (Rust gg fix done; Family A stays INTENDED.) The self-h…

- [`t0228`](todo/t0228.md) — 🐛 ③(b)-SURFACED pre-existing gaps (baseline-present):

- [`t0229`](todo/t0229.md) — 🐛 CORRECTNESS — OPEN self-host bugs (Rust gg is oracle):

- [`t0230`](todo/t0230.md) — 🔭 EXPRESSION-POSITION-CF FOLLOW-UPS (lower_expr.gg value-position cluster — EIf/EMatch/EDo/EBlock done):

- [`t0231`](todo/t0231.md) — 🧹 NEXT PARALLEL ROUND FIDELITY candidates (re-verify root + LAYER before briefing — only COMPILING the fix reveals truth…

- [`t0232`](todo/t0232.md) — 🧹 R12 CLEANUP CHAIN — migrate the self-host runtime-ABI NAME-LISTS to a TYPED extern-signature registry. SCOUTED: the re…

- [`t0233`](todo/t0233.md) — 🎯 CHAIN 3 RUNTIME-PARITY BACKLOG (re-generate counts from runtime_diff — dated counts are stale). CC-FAIL = uncompilable…

- [`t0234`](todo/t0234.md) — 🔭 R8 FIDELITY FOLLOW-UPS (each its own round):

- [`t0235`](todo/t0235.md) — 🔭 R9 FIDELITY FOLLOW-UPS:

- [`t0236`](todo/t0236.md) — 🐛 3 self-host runtime gaps blocking the 3 &global fixtures from c_emit parity (the &global READ works — static_init_impo…

- [`t0237`](todo/t0237.md) — 🚨 (High, CORRECTNESS) Bug B STAGE 2 — remaining = #2 ONLY (__gorget_box_clone link marker); #1 variant-collision LANDED…

- [`t0238`](todo/t0238.md) — [G3 → Core #9] Self-host symmetric port of the clone reason. Promote the reason String scaffold in the self-host lowerer…

- [`t0239`](todo/t0239.md) — Self-host parser: bare-return-type ownership sigil (int ! foo() / String & bar() OUTSIDE [...]). The 2026-06-22 parser r…

- [`t0240`](todo/t0240.md) — Self-host snag #4: scalar slot for struct value via EFieldAccess-as-method return-type inference fallback. String x = pa…

- [`t0241`](todo/t0241.md) — Self-host: extend tc_types.expr_types write-through to more Expr arms; consider span-keyed-sidecar replacement. Rust wri…

- [`t0242`](todo/t0242.md) — Typed Diagnostic struct in the self-host — sweep remaining stages. Resolver/parser/typechecker/lexer migrated off Vector…

- [`t0243`](todo/t0243.md) — Self-host check_comparison residual gaps — 8 mismatches [1013/1021 = 99.2%]: (a) Type-variable preservation (~5: corouti…

- [`t0244`](todo/t0244.md) — 🗺 [ROADMAP, owner-raised 2026-07-06 — GATED on C-path parity closure] Self-host LLVM backend. The self-host emits C only…

- [`t0245`](todo/t0245.md) — 🔍 [unwrap fix scout 2026-07-06, investigate] Self-host feeds its -1 unresolved-type sentinel into get_rtype_at(v, i) 7×…

- [`t0246`](todo/t0246.md) — 🐛 SELF-HOST MISCOMPILES in-place dict.get(k).unwrap().push(v) → SIGSEGV stage-1 binary, CONTEXT-DEPENDENT (discovered 20…

- [`t0247`](todo/t0247.md) — 🧹 Unify the two cstr-returning registries (is_cstr_returning_call in lower_types.gg + is_cstr_returning_fn in lir_codege…

- [`t0248`](todo/t0248.md) — 🐛 SELF-HOST MISLOWERS None() (with parens) → mistyped Option[int] temp (#14 P0). The PARSER accepts both None() and bare…

- [`t0249`](todo/t0249.md) — (self-host showcase cleanup) Retire the byte-by-byte Box-inner-name extraction loops at the EFieldAccess box-deref (grep…

- [`t0250`](todo/t0250.md) — (self-host showcase cleanup) Retire the stale "memset bug" comment + get-mutate-set shape in scope.gg set_def_param/set_…

- [`t0251`](todo/t0251.md) — SELF-HOST lexer bug (NOT a Rust bug): string literal inside an f-string interpolation truncates the enclosing match. A n…

#### [DEEP track — NOT single-zone] Full loop-else (for-else all paths + while-else)
Loop-else scout (round-8) proved the full feature is NOT bounded to lower_loops.gg. Ship the zone-clean `lower_for_iterator` slice first (+1 `iterable`); the REST is a dedicated serialized broad track:
- [`t0252`](todo/t0252.md) — for-else — ⚠ CLAIMS REGENERATED 2026-08-23; the old row set was wrong in both directions. The axis is 7/7 and TOTAL, tra…
- [`t0253`](todo/t0253.md) — while-else on the SELF-HOST TYPECHECKER/LOWERER LANE IS DONE — the remaining gap is the standalone PARSER + RESOLVER ast…
- Rust ref: `src/ir/lowering/stmts/for_loops.rs` `alloc_for_blocks`(:33-44)+`emit_else_arm_tail`(:68-81); `mod.rs:2290-2343` (while dual-target). Docs: `docs/language-reference.md:1276` (else runs on natural exhaustion, break skips it).
- Yield when COMPLETE: `iterable`(+via slice) + `for_else` + `break_nested` = +3 total. Bootstrap-safe (self-host source has zero loop-else).

#### [round-9 follow-ups, from 9A triage]
#### sibling of 9-1 (throws Result-deferral) — EXPR-BODY path
9-1 fixes the match-as-value path (`lower_match_expr`). The SAME bare-T-into-Result-slot bug exists in the throws EXPR-BODY return path (fixtures `throws_expr_body_tail`, `throws_t_result_resource_inner`). The match fix does NOT reach them (different lowering path). Sibling-site (core invariant #4): after 9-1 lands, scout-prototype the expr-body Result-deferral (find the expr-body tail-return lowering that seeds result type from `ctx.expected_type` without the ENUM_CAT_RESULT defer). Likely +2-3.

#### type_alias_struct_ctor — no struct_aliases mechanism (DEEP)
Self-host has NO `struct_aliases` (Rust `LirModule::struct_aliases`). `type Handle = SlotKey` registers `Handle` as a separate EMPTY struct (`struct __gg_Handle { char __pad; }`), ctor drops args, field access fails (`unknown field`). Needs a new alias-resolution mechanism across registration + ctor-routing + field-access (multi-file). The old TODO note "no struct-emit involvement" is REFUTED. DEEP — not a parallel-round candidate.

#### [follow-up, from 9-2 output-review] struct/collection static-by-&-ref fixture
9-2's `EIdentifier` arm in `ad_param_by_ptr` (lower_expr.gg) fires for ANY static (not just scalars — matches the type-agnostic `lower_place_base`). The only `&static` free-fn by-ptr arg in the corpus is the scalar `static_ref_param`, so the broadened struct/collection-static-by-& path is intercepted-but-UNTESTED. Add a fixture (`set_struct(&counter)` where counter is a module-level struct static, mutate a field) asserting the static is mutated through its real address, to lock the broadened behavior on both backends. Low-risk (both arms store &__lir_gN), but currently unguarded.

#### ====== CLOSURE PHASE-2 DEEP TRACK (owner-chosen 2026-06-26; roadmap from mapping scout) ======
CONFIRMED landed (run-verified): Phase 1 (non-capturing bodies), 2a (make-site refactor: LiftedClosure/drain-pass), 2b (RESOURCE/CoW captures — `cow_closure_*`/`copy_struct_closure_capture` MATCH). ~25 closure fixtures already MATCH.
KEY: self-host source uses ZERO closures → NO increment can break bootstrap_fixed_point (track is low-risk; gate on self_host_runtime/_diff + targeted fixtures).
Design refs: `docs/devbook/12-gir-lowering.md:342-448`, `docs/language-design.md:1369-1538` §7; Rust `src/ir/lowering/closures.rs` + LIR `Inst::CallClosure`/`ClosurePack`.
Sequenced increments (ordered yield/risk/unblock):
1. (done, see DONE.md) IIFE call wiring.
2. ByMutRef / 2c mutable captures (closure mutates outer var `count=count+1`): `lower_expr.gg:3148-3156` drops the mutation→stub today; add `MutPtr(T)` env field + deref body-load. Est +4-6 (test_multiline_closures, closures.gg tail). Rust `closures.rs:101-138,413-425` (detect_mutations→ByMutRef). MED.
3. Multi-statement body w/ control-flow + early return (CRASH today): `lower_closure_body` `lower_closures.gg:1787` (block lowering + early-return drops). Est +2-4 (closure_multiline_return). MED (root-cause the crash first).
4. Option/Result `.map`/`.and_then`/`or_else`/`map_err` + closure (the REAL `.map(it)` unblock — Vector HOFs ALREADY work via AST-inline `try_lower_vector_hof` lower_expr.gg:3691): combinator dispatch + `lir_lower.gg:2154` ptr arg + return-type wiring. Est +6-10 (implicit_it/option_map/result_map/option_result_combinators/coroutine_*_combinators — LARGEST cluster). HIGH (return-type CC-FAILs).
5. Callable-param-through-extern callback (`df.apply_float(col, fn)`): extern-method ABI + `__callable_N` threading. Est +3-5 (closure_float_ret/callable_ref_param/vector_callable_two_locals). MED-HIGH. Rust `closures.rs:233-248`.
6. Nested closures: drop `stmts_have_nested_closure` guard `lower_expr.gg:3158` + env nesting. Est +2-3. MED.
7. Box[Callable] / shared callable / non-resource-enum capture: `lir_lower.gg` Box/shared paths. Est +3-4 (box_callable/shared_callable/test_closures_edge_cases). MED.

#### [follow-up, from none_literal review] map_ast_type registration side-effect is a latent footgun
`map_ast_type` calls `lookup_or_register_named` (lower_types.gg:~645) — a REGISTRATION SIDE EFFECT, so it is NOT a pure type-query. The fn_param_sigs pre-pass guard `not fsdef.is_extern_stub` (lower.gg:3550) LOCALLY contains it for stub PARAMS, but the unguarded return-path registration (`map_fn_return_type`→`map_ast_type`, lower.gg:3555) would re-trip it for a future `extern Guard[bool] make()` (a stub whose RETURN is a named resource). Principled fix: split a side-effect-free pure type-query path from the register-on-demand path, OR make the pre-pass use the query-only variant. Not urgent (no current fixture trips the return path), but the footgun is real.

#### [defensive, from Increment-2 review] is_move guard on ByMutRef reclassify
The ByMutRef reclassify (lower_expr.gg:~3186) emits a `MutPtr(&outer_slot)` env field. Escape-unsafe by design (like Rust closures.rs:131) — safe ONLY because Rust gg rejects escaping mutating closures at borrow-check + the parity harness gates on the Rust oracle, so such programs are excluded. The self-host driver OMITS the borrow-check diagnostic (pre-existing, separately-filed gap), so for a Rust-REJECTED escaping mutating closure it would emit dangling-pointer C (vs stub garbage pre-Increment-2). Unreachable from valid Gorget, but defensive hardening = add an `is_move`/escape guard at the reclassify mirroring `closures.rs:102` (only ByMutRef a NON-escaping closure), OR wire the self-host borrow-check diagnostic. Belongs to the borrow-check-diagnostic-gap track.

#### 🆕 DISCOVERED DEFECTS (R5/R6 parity wave, 2026-06-27 — filed, parity-neutral/latent)
- [`t0254`](todo/t0254.md) — 🐛 ROUND-31 FOLLOW-UP (Core #8) — SELF-HOST semantic-reject port (scope WIDENED round-32). Round-31 (79c3ee30) made the R…
- [`t0255`](todo/t0255.md) — 🧹 ROUND-31 FOLLOW-UP (Core #1/#3, layering) — the builtin_method_type vs IR GORGET_STRING_VIEW two-list drift. Round-31'…
- [`t0256`](todo/t0256.md) **MED** — 🐛 [MED — WIDENED TO THE CLASS 2026-08-11 (T-PRUNE gauntlet pass-2, orchestrator-arbitrated on both backends): float math…
- [`t0257`](todo/t0257.md) **MED** — 🐛 [MED — REPHRASED 2026-08-11 by the T-PRUNE fold, orchestrator-verified: the filed ICE is GONE but the site is NOT clea…
- [`t0258`](todo/t0258.md) — 🐛 ROUND-31 PRE-EXISTING (minor) — f-string interpolation error CARET is mislocated (reject FIRES correctly, but the erro…
- [`t0259`](todo/t0259.md) — 🐛 Self-host f-string interpolation arg-ORDERING bug — string_algorithms WRONG (L18 oracle="()[]{}: true" self="()[]true:…
- [`t0260`](todo/t0260.md) — 🧹 ROUND-30 FOLLOW-UP — retire the now-dead SharedHeap machinery. Fix C (64a0d16e) deleted the sole set_shared_heap calle…
- [`t0261`](todo/t0261.md) — 🐛 ROUND-30 FOLLOW-UP — pre-existing enum-payload C-only leak (C/LLVM divergence, Core #8). Msg.Text(alias) (enum-payload…
- [`t0262`](todo/t0262.md) — 🔧 ROUND-30 FOLLOW-UP — the integration harness asserts STDOUT ONLY, masking the whole leak/UAF class. Fix C's 5 leaks +…

- [`t0263`](todo/t0263.md) — 🐛 self-host primitive-equip INSTANCE-method body lowers self/params as void* (from method-default pass-3 review): equip…
- [`t0264`](todo/t0264.md) **LOW** — 🆕 [LOW — Root-A scout limitation 2026-07-17] Deeper-chain / non-bare-base field receivers still value-copy: the landed f…
- [`t0265`](todo/t0265.md) **MED** — 🆕🐛 [MED — pre-existing, confirmed on the pre-fix driver by the Root-A scout] Chained-adapter return-type registration: D…
#### [closure follow-ups, from Increment-3 scout — SEPARATE classes, not Increment 3]
- [`t0266`](todo/t0266.md) — snag51_closure_block_tail_value (CRASH): a String double-free in main (ASan: double-free in gorget_string_free) — closur…

- [`t0268`](todo/t0268.md) — [CLOSURE INCREMENT 4 (Option/Result combinators)] The 4a–4e sub-track (4a/4b/4c/4e-1/4d + the R2-class __result_unwrap_o…

#### [bug, tangential — from 4d scout] self-host f-string codegen SIGSEGV on multi-interpolation + struct-field access
The 4d scout hit a self-host driver SIGSEGV when a debug `print(f"DBG ... {field}")` had MULTIPLE f-string interpolations + a struct-field access in the interpolation. Plain string-concat printing worked fine. Tangential to the closure work (only surfaced during probing). Repro: an f-string with ≥2 `{}` where one is `obj.field`. Likely an f-string codegen / arg-threading bug in the self-host. Needs its own scout to isolate + a minimal repro. Low priority (debug-only path), but a real codegen bug.

#### [closure 4e-2 — ⚠ NO LONGER "LATENT": MEASURED LIVE, Core #8 silent-wrong-output] Option/Result combinator adapter SKIPS the payload's user `Drop` and LEAKS
**⚠ PREMISE CORRECTED 2026-07-25 by the Track-B2 scout — the "Rust handles it" claim below is REFUTED.**
Measured at HEAD, C backend: `Option[Money].map(...)` with `equip Money with Drop` prints the value but
**NEVER prints "dropping"** — the user `Drop` is silently skipped. That is **silent-wrong-output**
(Core #8), not merely a latent leak. LSan on a 2000-iteration `Option[Vector[int]].map` probe:
**176,000 B in 4,000 allocations**. It is combinator-SPECIFIC — the same program written
`if o is Some(v)` is clean. **D-2T also fails to reject a drop-tainted value at this materialize
position**, so the drop-purity gate does not cover it either. Reproduces at baseline AND under the B2
prototype, i.e. pre-existing and independent of the consume-position migration. Per Core #8 a
pre-existing known defect is NOT a licence to ship past it.
**DURABLE REPRO (committed 2026-07-25):** `tests/fixtures/security/sound_option_map_user_drop_leak.gg`
+ `security.rs::sound_option_map_user_drop_no_leak` (`#[ignore]`d, `security_safe_no_leak`, asserts
the INTENDED `Drop`-runs / no-leak state). RED-verified at HEAD: prints
`512/control-end/dropping/512/map-end` — the trailing `dropping` MISSING — plus
`LeakSanitizer: 8192 bytes in 2 allocations` (`Money__clone` via `Option__Money__clone`, and the
payload). The fixture carries the `if o is Some(v)` CONTROL LEG in the same program, which is clean,
so the map leg's silence is pinned as a divergence rather than a design choice.
⚠ AMBIGUITY recorded in-fixture: the expected stdout asserts ONE `dropping` per leg (the CoW-charter
answer — the combinator's implicit `Option__Money__clone` is itself excess implicit cloning). A fix
that keeps the clone and drops BOTH copies prints `dropping` twice and fails — the intended signal,
not a false red. Out of ggdef's phase-0 subset. **Un-ignore when the combinator adapter owns its
payload.**
**ORIGINAL FILING BELOW — its "LATENT / Rust handles it" framing is superseded:**
The (B) ownership/double-free tail of 4e is LATENT: the whole corpus returns only STATIC-LITERAL Strings (cap=0, .rodata) from map/map_err/unwrap_or_else closures → no heap double-free. It only surfaces with a HEAP String return (e.g. `.map((x): f"{x}")` or `.map((s): s.to_upper())`). Rust handles it (clone-receiver + Move-mode + scrut-zero, methods.rs:2924-2970/:3083-3109). To pursue: (1) ADD a heap-return combinator fixture, (2) port Rust's ownership adapter into the inline-C template/dispatch, (3) ASan-gate (a wrong fix trades the absence-of-crash for a silent double-free — worse). DEFERRED: no corpus fixture currently fails on it, so it's +0 parity until a fixture exists; do it when a heap-return combinator program is added or surfaces. The combinator sub-track (4a/4b/4c/4e-1/4d + R2-class) is otherwise COMPLETE.

#### [closure Increment 6 = DROPPED] nested closures — no corpus gap
The "nested closures" increment is a NON-issue: no corpus fixture has a lexically-nested closure (a closure literal whose body contains another), the `stmts_have_nested_closure` guard (lower_expr.gg:3369/:3766) is DORMANT, and closure-returning-closure / partial-application / compose all already MATCH. Dropping it (un-validatable, nothing to flip).

#### [follow-up un-masked by the Ref[T]-field fix] lazy Dict.keys_iter/values_iter/set-iter in a for-loop infinite-loops
With a VALID `source` pointer (post Ref[T]-field fix), `dict_keys_lazy`/`dict_values_lazy`/`stdlib_iter_set` go SIGSEGV→TIMEOUT: the lazy `DictKeysIter`/`DictValuesIter`/set-iter FOR-LOOP driver loops forever (the direct `.next()` form `borrow_field_lazy_dict_iter` works). Separate, deeper for-loop-over-lazy-iterator bug. Not a regression (already broken). Needs its own scout.

#### [closure Increment 7 = SPLIT into 3 — from Inc-7 scout]
- 7c `shared_callable`: DEEP/DEFERRED. Needs full `Shared[T]` refcount infra: Shared ctor emitted as bare `Shared__…(…)` (runtime is `_new`); `.clone()` refcount-bump not lowered (clone locals read uninitialized); `.get()()` double-dispatch not lowered. PLUS pre-existing entanglement: `shared_basic` itself CC-FAILs (17 errors, crypto-preamble "invalid initializer" emit bug). Multi-file, deferred with this map.

#### ====== POST-CLOSURE PARITY (triage @ 65c2cdc0; LIVE parity = re-measure via runtime_diff) ======
Remaining from this triage (GO #1 / GO #2 v2 / named-arg-reorder candidate #3 are in DONE.md):
- `test_hashset_all` advanced PAST the set-op gap but stays CC-FAIL on a SEPARATE `HashSet.fold` closure-lowering gap (closure-track territory, not set-ops).
- ⚠ HARNESS BUG: `self_host_runtime_diff` SIGABRT ("non-unwinding panic" — a Drop panicking during another panic) crashes the parallel worker under the triage's 12s timeouts; the test's 30s timeouts ran clean at checkpoints. Investigate: which fixture's Drop double-panics; harden run_with_timeout_catching. (Verify whether it reproduces at the test's default timeout.)
#### NEXT-ROUND CLUSTER MAP (sized):
- trait-object/Box vtable dispatch ~10 (DEEP): dynamic_dispatch/print_trait_object/serializable/deserializable/default_trait/from_trait{,_multi}/measurable_trait/via_delegation — `Box__Trait__method` undefined. (`operator_overload` RESOLVED by R42-A user-operator-overload dispatch; `derive_debuggable` RESOLVED by R41 S-A → both moved to DONE.)
- String-builder ~8 (DEEP feature gap): string_builder{,_loop}/json_pretty/json_edge_cases/xml_*/string_unicode_stress — `String()` builder + .push/.push_line/.str() mis-routed; needs the gorget_string_* builder API.
- stdlib_io Writer trait 3 (R40-T-B landed writer + flush → MATCH; residual): stdlib_io_file_writer/_tls_writer/_stdout_typed. (Detail in the ROUND-40 T-B RESIDUAL ROOTS block above.)
- lazy-iter adapter-chain infinite-loop (DEEP, KILLED): FilterIter.next()'s `self.inner` EFieldAccess copies the inner iterator (lower_place_base lower_stmt.gg:1215 has no EFieldAccess case → value copy not field-borrow); Ref[Dict]/Ref[Set] source isn't is_resource_field_type (lower_types.gg:2521). Real fix = place-projection receiver (&self.inner), architectural (self-host has no Place/Projection::Field IR). DEEP.
- collection type-alias (type IntList = Vector[int]) — meta_aliases drops the [int] targ; needs targ-preserving alias storage. Separate from GO #1.

- [`t0773`](todo/t0773.md) **HIGH** — 🆕🐛 [HIGH — Core #9 LANE LAG, filed by R47 Track B the round the Rust lane landed, per this tree's recorded practice: lan…
- [`t0740`](todo/t0740.md) **MED** — 🆕🐛 [MED — the RECEIVER/ROOT-SHAPE axis of R47 Track D1's primitive-receiver reject: four of its five cells are unsampled…
- [`t0744`](todo/t0744.md) **MED** — 🆕🐛 [MED — TWO of the THREE named residuals of R47 Track D1's primitive-receiver reject; all are lane divergences with a…
- [`t0746`](todo/t0746.md) **MED** — 🆕🐛 [MED — the NOMINAL-receiver cell of the "name list answers without checking the receiver" family; a lane divergence w…
- [`t0747`](todo/t0747.md) **MED** — 🆕🐛 [MED — a VIEW-VS-OWNED axis divergence, i.e. Layering rule 1's explicitly named invariant, on a path the bootstrap us…
- [`t0791`](todo/t0791.md) **MED** — 🆕🐛 [MED — Core #9 LANE LAG, opened 2026-08-29 by R47 Track E2's own fix; source-verified, not yet run-measured on the se…
- [`t0877`](todo/t0877.md) **MED** — 🆕🐛 [MED — SELF-HOST lane; the surviving POSITION of a four-arm class whose other position closed] A closure LITERAL at a…
- [`t0879`](todo/t0879.md) **MED** — 🆕🐛 [MED — SELF-HOST lane; a USER-DEFINED generic method taking a callable fails on EVERY argument spelling; isolated R48…
- [`t0928`](todo/t0928.md) **MED** — 🆕🐛 [MED — SELF-HOST DIAGNOSTIC QUALITY, a LOSSLESSNESS gap at the parser→AST boundary (Layering rule 1). Filed by R48 Tr…
- [`t0929`](todo/t0929.md) **MED** — 🆕🐛 [MED — Core #10 lower-or-reject + Core #9 accept/reject LANE DIVERGENCE, on the self-host. Filed by R48 Track β, whos…
- [`t0932`](todo/t0932.md) **MED** — 🆕 [MEDIUM — self-host lane, ASan-BLIND; found 2026-08-31 by R48 Track γ while building the index/slice derivation-join n…
- [`t0969`](todo/t0969.md) **MED** — 🆕🐛 [MED — self-host lane SEGVs (rc 139) where Rust gg is correct on BOTH backends; found 2026-09-03 by R49 Track A1-M's…
- [`t0970`](todo/t0970.md) **MED** — 🆕🐛 [MED — the self-host REJECTS a program Rust gg compiles and runs, and the diagnostic names a type the expression cann…
- [`t1128`](todo/t1128.md) **MED** — 🆕⚠ [MED — LANE GAP, deliberately narrower not broken: the self-host's D46 equality gate accepts every generic instantiat…
- [`t1069`](todo/t1069.md) **MED** — 🆕🐛 [MED — SELF-HOST lane LEAK, gg check clean and rc 0 on a plain run; found R49 Track L, ASan-measured over the self-ho…
- [`t1286`](todo/t1286.md) **MED** — 🆕🐛 [MED — a LANE DIVERGENCE with a settled direction: Deque.map does not compile on the self-host at all, while Rust gg…
### Low

- [`t0269`](todo/t0269.md) — 🐛 SELF-HOST (SH-lane miscompile, filed R40 Track-J review 2026-08-10) — for (i, b) in s.bytes().enumerate() over-reads t…

- [`t0270`](todo/t0270.md) — 🐛 SELF-HOST (SH-typechecker catch-binding scope, filed R40 Track-J review 2026-08-10) — the SH typechecker rejects the c…

- [`t0271`](todo/t0271.md) — 🧹 F-STRING SPEC follow-ups (filed by the f-string format-spec round 99839eeb, 2026-06-25; non-blocking, file-and-forget)…

- [`t0272`](todo/t0272.md) — 🧹 SELF-HOST ELEGANCE — retire the mangle_trait_name .contains("[") fossil now that index_of returns a real Option (filed…

- [`t0273`](todo/t0273.md) — 🐛 SELF-HOST — if/block-AS-VALUE path leaks shadowed name-locals (sibling of the statement-lower_if fix c1df55f1; filed b…

- [`t0274`](todo/t0274.md) — 🧹 SELF-HOST — cstr→GorgetString refine-remap missing on the inline IFunction extern arm (minor, filed by the imported-ex…

- [`t0275`](todo/t0275.md) — 🐛 SELF-HOST (verify-by-RUNNING) — does the self-host lower_catch_expr have the resource-payload err-binding gap Rust-gg…

- [`t0276`](todo/t0276.md) — 🐛 SELF-HOST — unwrap()/unwrap_error() on a Some(String)/Ok(String)/Error(String) payload LEAKS ~6B (ASan; Rust gg CLEAN)…

- [`t0277`](todo/t0277.md) — 🐛 FILED 2026-06-16 (parity follow-ups, none blockers):

- [`t0278`](todo/t0278.md) — 🐛 SELF-HOST PARITY TRIAGE BACKLOG (RUN-confirmed, ranked bounded wins; re-count from runtime_diff):

- [`t0279`](todo/t0279.md) — 🐛 NAMED-ARG MODELING follow-ups (builds on the array_with_capacity/alloc= round — {Vector,Deque} interception, see DONE.…

- [`t0280`](todo/t0280.md) — 🐛 SET-PARITY FOLLOW-UPS (builds on the CkSet→CkOrderedSet/CkHashSet typed-split round, see DONE.md):

- [`t0281`](todo/t0281.md) — 🐛 SELF-HOST: QUALIFIED variant-ctor EnumName.Variant([elem]) array/dict-literal arg gets NO element-size hint → hardcode…

- [`t0282`](todo/t0282.md) — 🔭 R3 free-fn HOF (vector_userspace_hofs) — SCOUT-KILL a43f7dcb: NOT a bounded win. gorget_array_new(2 args) is one sympt…

- [`t0283`](todo/t0283.md) — 🐛 BURNDOWN QUEUE (this arc's scouts):

- [`t0284`](todo/t0284.md) — 🐛 self-host match *(box_field) (Box-deref in a match scrutinee) drops the arm body (low-pri). Matching on a dereferenced…

- [`t0285`](todo/t0285.md) — 🐛 LATENT silent-None class (self-host) — bare None arg to a NOT-YET-LOWERED callee = silent (Option__T){0} (zeroed tag r…

- [`t0286`](todo/t0286.md) — !None at a &/! (GtMutPtr-wrapped) resource-Option param CC-FAILs through the self-host — fixture none_literal_sigiled_ar…

- [`t0287`](todo/t0287.md) — 🐛 self-host CATCH-bound error-payload leaks the PErr.msg String (~4B via PErr__clone→gorget_string_clone_to_owned; ASan)…

- [`t0288`](todo/t0288.md) — 🧹 typed enum-category channel 59985a10 follow-ups (low-pri, measured-neutral; readers stay name-free):

- [`t0289`](todo/t0289.md) — 🐛 module-level meta int/meta bool consts resolve to 0 in the self-host driver (confirmed pre/post gap-B). Self-host does…

- [`t0290`](todo/t0290.md) — 🧹 self-host collection_ctor_init_expr (lower_types.gg) static-ctor routing — NOT dead code (the former "currently DEAD C…

- [`t0291`](todo/t0291.md) — 🐛 3 OUT-OF-SCOPE latent stdlib bugs surfaced by the imported-check fix (NONE imported by any fixture; lib/xtd/{query,ssh…

- [`t0292`](todo/t0292.md) — (FIDELITY) Self-host MIS-LOWERS print(X, file=stderr) to stdout (drops file= → routes to stdout). Affects self_host_lowe…

- [`t0293`](todo/t0293.md) — (Fidelity) Propagate the A2/r2 lexer keyword/StringKind alignment to the OTHER 5 self-host dirs (parser/resolver/typeche…

- [`t0294`](todo/t0294.md) — Branch-merging-expression helper (Cluster A, post-Snag-#39 audit). Defer. Three sites use the discipline (assign_match_a…

- [`t0295`](todo/t0295.md) — Self-host silent-fallback audit — IN PROGRESS. 3 sites emit /* [bug] ... */ (af0cb513: map_binop unknown op, EIdentifier…

- [`t0296`](todo/t0296.md) — 🔗 [P1-G follow-up] Self-host diagnostic-code parity. Wire the E_/W_ codes into the self-host front-end (tests/fixtures/s…
- [`t0297`](todo/t0297.md) — 🧹 [unwrap output-review 2026-07-06, LOW] Self-host panic spans emit <unknown>:0:0: — the self-host's emitted gorget_pani…

- [`t0298`](todo/t0298.md) — (durable end-state for the cstr-return registry — STOPGAP shipped 2026-06-14) Retire the runtime_fn_returns_cstr name-li…

- [`t0299`](todo/t0299.md) — 🔧 (self-host) Phase 2c COMMIT 3 — drop-emission flip (keystone). Prereqs done (drop_elab packed-2-bit 1289a7d7, GIMoveZe…

- [`t0300`](todo/t0300.md) — 🔧 (self-host) Phase 2c COMMIT 2 Site 2b root-cause. The GICallExtern pass-by-ptr OpClone flip (in-place-mutator runtimes…

- [`t0301`](todo/t0301.md) — Audit other method-chain sites for LoBorrowed propagation gaps (follow-up to Prereq B-extension). .unwrap_or(default) (d…

#### [follow-up, unrelated to loop-else, pre-existing] CC-FAIL baseline fixtures
`string_iterators` (int64_t vs incompatible assign) + `iterator_adapters` (GorgetArray vs incompatible assign) CC-FAIL on baseline self-host — separate backlog. `test_multiline_closures` WRONG (all-zeros) = closure-capture miscompile, not loop-else.

#### [nit, non-blocking, from B output-review] `byte` alias on signed arm in resolve_field_gir_type
`resolve_field_gir_type` (lower_types.gg) keeps `byte` on the signed arm (`int8_t|i8|byte→I8_TYPE`), but Rust treats `byte` as a `uint8` alias (`src/lexer/token.rs:471`, `src/ir/lowering/exprs/mod.rs:2566`) and the sibling `prim_name_to_type` (lower_types.gg:56-58) correctly maps `byte→U8_TYPE`. Currently DEAD (field type names reach this fn in canonical `uint8_t` form, not surface `byte`), so not a regression — but tighten for sibling-site consistency: move `byte` to the unsigned `uint8_t|u8|byte→U8_TYPE` arm. Verify no fixture flips before/after (should be a no-op).

#### [from C output-review] for-paths that drop else_body SILENTLY — ⚠ ROW SET REGENERATED 2026-08-23
The old text named `lower_for_range` as one of the silent droppers. **It is FIXED** — `lower_for_range` (`lower_loops.gg:919`) takes `else_body` and lowers it, as do `lower_for_set`(:797) and `lower_for_iterator`(:1012). The **actual** silent droppers are `lower_for_string`(:427) **and `lower_for_string_bytes`(:537)**, which the old text omitted entirely; `lower_for_vector`(:298) and `lower_for_dict`(:700) drop too but at least leave a `lower_fail` marker in the emitted C (dispatcher `lower_loops.gg:176-177`/`:188-189`). ⚠ **The marker is not a gate:** `lower.gg:545` emits it as a **C comment** (`print("/* [bug] …")`-style `lower_fail` text), so even the "loud" pair compiles and runs. Everything here is subsumed by the for-else row set in the deep loop-else track above, which now carries the full 7/7 disposition, the regenerated 15-call-site cost, and the durable repro `known_gaps/sh_forelse_else_body_dropped.gg`. **Do NOT add a marker-grep ratchet** — `grep -c lower_fail` reads 1 for vector/dict and 0 for string/bytes, i.e. blind to half the class.

#### [nit, from D output-review] stale docstring on decide_operand_at_consuming_arg
`lower.gg:~1808` docstring still says "Status: dead code in this commit. No caller exists." — FALSE: the fn is wired via `wire_one_operand` (lower_liveness.gg:970). Pre-existing, comment-only (no codegen impact). Correct the docstring to reflect it's the live CFG-aware wire-pass operand-mode decision. 1-line cleanup.

#### [nit, non-blocking, from throws-expr-body review] Defect-A peel is single-level
`lower_stmt.gg:545` peels `GtPtr`/`GtMutPtr` off `val`'s type ONCE (not recursive like `peel_ptr_tid`). Sufficient today (return-site operands are at most single-Ptr-wrapped Result; green 813-net), but a future borrow-of-borrow Result return would under-peel. If `peel_ptr_tid` is cheap to call there, prefer it for robustness. Inconsequential now.

#### [closure follow-up, from Increment-3 review] closure_value_ret_type channel follow-the-value gap
The Increment-3 `closure_value_ret_type` channel is populated only at the closure make-site + the SVarDecl-MoveDirect copy-forward. A closure value reaching a callable local via a NON-MoveDirect VarDecl branch (CoW/Branch-C), a reassignment (`f = (...)`), or store-in-collection-then-retrieve MISSES the channel → falls back to `expected_type` (same as pre-Increment-3, no regression — just unaddressed). The fn-ARG-then-called path IS handled (GtFnPtr param fires the FnPtr-recovery arm first). Extend the copy-forward to the other VarDecl branches + reassignment when a later increment needs it.

- [`t0302`](todo/t0302.md) **MED** — 🆕🚨 [MED-HIGH — SH TYPE-INFERENCE hole that silently disables every type-directed check on one expression shape; found 20…

## ggdef / define-gorget

- [`t0303`](todo/t0303.md) **MED** — 🆕🐛 [MED — ggdef LANE GAP (lagging, not wrong): it models NO match exhaustiveness at all, and STRUCTURALLY cannot express…

- [`t0304`](todo/t0304.md) **HIGH** — 🆕🚨 [HIGH — ggdef CONTRADICTS RATIFIED D37: it does not run container-element destructors at all; measured 2026-08-22 at…
- [`t0696`](todo/t0696.md) **MED** — 🆕📐 [MED — ggdef SUBSET GAP: Gorget's Box[T] is not in the definitional oracle's type language at all; filed 2026-08-27 b…
### High

- [`t0305`](todo/t0305.md) **HIGH** — 🆕🐛 [HIGH — Core #9 lane divergence, oracle ACCEPTS 3 programs the language REJECTS; filed 2026-07-31 by Round XX pass 6,…

- [`t0306`](todo/t0306.md) **MED** — 🆕🐛 [MED — Core #4 sibling of Track M's fix; filed 2026-07-31 by Round XX output-review] The E_ClosureKindMismatch class…

- [`t0307`](todo/t0307.md) **MED** — 🆕⚙ [MED — probe owed, precedent-driven; filed 2026-07-31 by Round XX pass 6] Does ggdef model is (pattern test, §7.17) c…

- [`t0308`](todo/t0308.md) — 🐛 [SUBSET GAP — Track B 2026-07-21, Core #9 explicit] Non-Add OP= / binary op static reject not in ggdef. Rust gg lands…

- [`t0309`](todo/t0309.md) — 🧹 [D29 FOLLOW-UPS — core feature complete 2026-07-17; remaining self-host residual gaps (C/LLVM cover them):]

- [`t0310`](todo/t0310.md) — 🚀⚙️ [THE RATIFIED ENFORCEMENT-WAVE PLAN — owner 2026-07-11, census-backed (the wave-census scout (git history)); ledger…
- [`t0311`](todo/t0311.md) — 🆕⚙️ [D27 RATIFIED 2026-07-11 → SIGIL-ECONOMY phase: A2 BULK SWEEP (Round A Phase 2) — tool pieces LANDED R37; in-place s…
- [`t0312`](todo/t0312.md) — 🆕⚙️ [D27 RATIFIED 2026-07-11 → SIGIL-ECONOMY phase: ROUND B (reject ! at parse + "use ^" fix-it) — R41 T-RB1, post-A2; c…
- [`t0313`](todo/t0313.md) — ⚙️ [D23 RATIFIED 2026-07-07 → enforcement track; RIDES the trap-normalization wave or stands alone] The throws totality…

- [`t0314`](todo/t0314.md) — ⚙️ [D11 RATIFIED IN FULL 2026-07-06 → THE TRAP-NORMALIZATION TRACK] One TrapKind registry, trap[T_X] rendering, exit 101…
- [`t0315`](todo/t0315.md) — ⚙️ [D12 RATIFIED 2026-07-06 → implementation track, STRAIGHT TO ERROR] D4 enforcement lands in production. Per the ledge…

- [`t0316`](todo/t0316.md) — ⚙️ [D15+D22 RATIFIED 2026-07-06 → the COMBINED slice-surface track] One sequence type, one slice spelling. (a) Remove in…

- [`t0317`](todo/t0317.md) — ⚙️ [D17 RATIFIED 2026-07-06 → implementation track] read_file becomes throws — and the STDLIB FALLIBILITY CLASS sweeps w…

- [`t0318`](todo/t0318.md) — ⚙️ [D10 RATIFIED 2026-07-06 → implementation tracks] The exclusivity package lands in BOTH compilers + prose. Per decisi…

- [`t0319`](todo/t0319.md) — ⚙️ [D14 RATIFIED 2026-07-06 → implementation track] get_or/get_or_put/get_or_else become VIEWS; retire the round-8 uncon…

- [`t0320`](todo/t0320.md) — 📋 [P1-D increment D2 — DEFERRED by the 4-pass brief gauntlet; own scout→brief→reviews before any work] The production-v1…

#### 🆕 D30 IMPLEMENTATION TRACK (HIGH — ratified 2026-07-19, ledger `d2da1cfe`; own scout→gauntlet)
- [`t0321`](todo/t0321.md) — Narrow-type overflow traps uniformly. Today int8 127+1 prints 128 (bound never enforced); int already traps T_Overflow.…

#### 🆕 D45 ERROR-MODEL CONSOLIDATION — IMPLEMENTATION ROUNDS (ratified 2026-08-10; ledger D45 is NORMATIVE — these entries hold only what the ledger does not carry: track membership, zones, gates)
- [`t0322`](todo/t0322.md) — ⚙️ E0 — "kill the fictions" (pure defect-closure round; convergence-friendly; open any time). Members = the already-file…
- [`t0323`](todo/t0323.md) — ⚙️ E1 — "the model gets real users." = the ratified D17 stdlib-throws class sweep (its own ⚙️ entry above — grep D17 RAT…
- [`t0324`](todo/t0324.md) — ⚙️ E2 — "sets" (A31 impl per D45 pins 1–3, 8, 9). type Name = A | B alias unions (pin 1's rules as corrected by pass 3:…
- [`t0325`](todo/t0325.md) — ⚙️ E3 — "subtraction" (D45 pins 4–5). catch: + case arms — ordinary match patterns, type-first member pattern (case IoEr…
- [`t0326`](todo/t0326.md) — ⚙️ E4 — "history + concurrency." A34b chain (debug-only, NOT value-reachable v1, after E0's auto-prop kill; format trigg…
- [`t0327`](todo/t0327.md) — ⚙️ A38-a — registry as shipped toolchain data (D45 pin 10a). Columns {code, default_level, configurable, group, since, f…
- [`t0328`](todo/t0328.md) — ⚙️ A38-b — E_MissingFallibleMark split BY FIX DIRECTION (D45 pin 10b). FOUR reasons at HEAD (src/semantic/errors.rs:331-…
- [`t0329`](todo/t0329.md) — ⚙️ A38-c — structured fix-its with applicability (D45 pin 10c). {span, replacement, applicability ∈ machine-applicable/c…
- [`t0330`](todo/t0330.md) — ⚙️ A38-d — machine output + terminal hygiene (D45 pin 10d) [R41 W2 = T-A38 scope]. --diagnostics=json (NDJSON, versioned…
- [`t0331`](todo/t0331.md) — ⚙️ A38-e — check⇒build guard ratchet + C-ERROR-leak grep-guard (D45 pin 10e) [OWNED BY T-E0b this round (pass-2 catch: p…
- [`t0332`](todo/t0332.md) — ⚙️ A38-f — determinism fixture (D45 pin 10f) [R41 W2 = T-A38 scope]. Same source ⇒ byte-identical diagnostics across run…
- [`t0333`](todo/t0333.md) — ⚙️ A38-g — frozen runtime grammars (D45 pin 10g), SPLIT per pass-13: the TRAP-line freeze is Q5-independent and rides E0…

#### ====== 🏛 DEFINE GORGET — THE ACTIVE FLAGSHIP TRACK (owner GO 2026-07-05) ======
**The executable definition of Gorget's semantics.** The normative ledger is `docs/define-gorget/decisions.md` (owner decisions D1–D29 + standing directives + the open decision queue A31/A32/A33) — read it FIRST for this track. The former orchestration scaffolding (`HANDOVER.md`, the `rfc-ggc-ggdef.md` architecture RFC, `phase0-brief.md`, and every per-round scout/brief/proposal) was retired to git history in the 2026-07-17 repo-hygiene slice; recover any with `git log`/`git show`. Phase 0 and the phase-1 enforcement wave (Batches A/B + D11/D23/D29) have LANDED; the live forward work is the RATIFIED ENFORCEMENT-WAVE PLAN entry above (Batch C: C1 operators → C2 fault-catch removal → C3 sigil sweep, then the out-of-repo coordination round + D13/D14/D17 + riders) plus the CoW WAVE 2 queue. Owner directives: run subagents with `model: "opus"`; briefs are written to be Opus-executable, and an Opus stall is a BRIEF defect to fix.
- [`t0334`](todo/t0334.md) **MED** — [MEDIUM — after phase 2's context pack] 🤖 LLM-correctness KPI (owner directive 2026-07-05, recorded in decisions.md): a…

- [`t0335`](todo/t0335.md) **HIGH** — [HIGH — ggdef phase-1 MUST, from B2 output-review 2026-07-06 + XXVI Track E scout depth §2 2026-08-02] 🐛 ggdef transitiv…

- [`t0336`](todo/t0336.md) **HIGH** — [HIGH — ggdef G-class row 3, filed XXVI Track E 2026-08-02; DEPENDS ON the transitive-drop row above] 🐛 drop_struct_coll…
- [`t0337`](todo/t0337.md) **HIGH** — [HIGH — ggdef native-recursion SIGABRT; separate track split off from the P1-A throw-drop fix 2026-07-06] 🐛 ggdef's big-…
- [`t1130`](todo/t1130.md) **HIGH** — 🆕🚨 [HIGH — LANE DIVERGENCE ON ACCEPTED, CORRECT, DOCUMENTED CODE: ggdef's == NEVER DISPATCHES a user equip T with Equata…
- [`t1432`](todo/t1432.md) **HIGH** — 🆕🔬 [HIGH — THE DASHBOARD STATES A CLAIM THE DEFINITION NEVER MADE, ON 40 BASELINED ROWS. This is R50 Track K2, split out…
### Medium
- [`t0338`](todo/t0338.md) **MED** — 🆕📐 [MED — ggdef SUBSET GAP, Core #9; measured 2026-08-19 by R43 Track C] The whole Callable-valued indirect-call family…

- [`t0339`](todo/t0339.md) **MED** — 🆕📐 [MEDIUM — Core #9 ggdef subset-gap, from Track A struct-value match 2026-07-17] Struct-value constructor patterns (ca…
#### 🆕 GUARDS-SLICE G2 DISCOVERIES (filed 2026-07-18 by the adjudication split's first reading — every current BOTH-WRONG fixture is a GGDEF-side defect, pinned by the shrink-only `EXPECTED_BOTH_WRONG` allowlist [10 after the 2026-07-19 class-A oracle fixes]; three-way outputs re-derivable by running the split)
- [`t0340`](todo/t0340.md) **MED** — [MED — ggdef silent mis-modeling → must become LOUD ElabError per classify.rs invariant #8, 10 fixtures] Str/Display imp…
- [`t0341`](todo/t0341.md) **MED** — [MED — census candidate] 29 MATCH fixtures are ggdef-ILLFORMED (ggdef rejects what both compilers accept) — audit as a D…
- [`t0342`](todo/t0342.md) **LOW** — [LOW — masking rider] The D8 float-HOLD heuristic (decimal-in-diff → UNADJ) can MASK a real BOTH-WRONG whose diff lines…
- [`t0343`](todo/t0343.md) **LOW** — [LOW — riders from the 2026-07-19 class-A oracle-fix scout, 3 items] (a) ggdef has NO match-exhaustiveness static check…

- [`t0344`](todo/t0344.md) **LOW** — [LOW — ggdef subset gaps, Core #9] The out-of-subset scope shapes from the round: select/channels; ggdef also REJECTS ma…
- [`t0345`](todo/t0345.md) — ⚙️ [D18 RATIFIED 2026-07-06 → implementation track] Const-eval fault mirror: runtime faults become compile errors. The g…

- [`t0346`](todo/t0346.md) — ⚙️ [D19 RATIFIED 2026-07-06 → removal track] Remove break <value> / loop-as-expression from the surface. Owner: "no loop…

- [`t0347`](todo/t0347.md) — 🐛 [ggdef, disclosed by P1-A executor 2026-07-06 — was report-only, filed by parent] Error-unwind drop timing: on Halt::P…

- [`t0348`](todo/t0348.md) **MED** — 🆕🐛 [MED — ggdef ORACLE under-rejection blocked on a TYPING SUBSTRATE gap, from RV-F pass-3 2026-07-16] ggdef never types…
#### Trap-normalization (D11) T2a-rust follow-ups (filed 2026-07-10 by the T2a-rust executor)
- [`t0349`](todo/t0349.md) **MED** — [MEDIUM — T1-zone] spectests/run/trap_shift.gg conformance fixture (BLOCKED on ggdef modeling shift). T2a-rust made an o…
- [`t0350`](todo/t0350.md) **MED** — [MEDIUM — T1-zone] spectests/run/trap_assert_cmp.gg conformance fixture for the MESSAGE-LESS comparison assert (assert a…
- [`t0351`](todo/t0351.md) **MED** — [MEDIUM — structural guard] The LLVM block_exit_labels twin pre-pass silently drifts from the emit. src/backend/llvm/mod…
- [`t0352`](todo/t0352.md) **LOW** — [LOW — cosmetic, not conformance-compared] Message-form assert trap line renders at <unknown>:0:0. T2a-rust reroutes ass…

#### D23-throws smith follow-ups (filed 2026-07-10 by the T3b executor)
- [`t0353`](todo/t0353.md) **MED** — [MEDIUM] 🎲 T3c — a POSITIVE throws DIFFERENTIAL smith tier. T3b (LANDED, see DONE.md) is the NEGATIVE/rejection tier: GG…
- [`t0354`](todo/t0354.md) **LOW** — [LOW] 🎲 T3b equip-method receiver-position extension (deferred from T3b). T3b's generator ships the free-fn risky() form…
#### Trap-normalization (D11) T2a-selfhost follow-ups (filed 2026-07-10 by the T2a-selfhost executor)
- [`t0355`](todo/t0355.md) **MED** — [MEDIUM — self-host-shift-parity, Core-#8] The self-host emits an UNGUARDED out-of-range shift — the lone backend with s…

#### Self-host reject-diagnostic-rendering alignment (filed 2026-07-15; split off from the ggdef elab∘eval landing, owner-confirmed — the COMMITTED next track that completes four-lane-green)
  - **🆕🐛 [HIGH — DEFINITION-INTEGRITY (oracle under-rejection), from the coarse-kind scout 2026-07-16] ggdef ACCEPTS (exit 0) programs BOTH production compilers reject on the range/signature/main-throws axes — and its `reject_code` verdict axis is MAY-MOVE-ONLY (`spec/ggdef/src/lib.rs:148`).** Measured: `ggdef run` on the real coarse-kind reject corpus yields NO `reject: E_X` verdict for ANY of them — buckets: FrontendError-SKIP (deref/default-op/positional/throw), eval-IllFormed-codeless (string-index/break/continue), and outright ACCEPT (value-range / required-after-default / main-throws — the oracle blesses ill-formed programs, same under-rejection class as the ConsumeCallable bug). Per the ratified `verdict = elaborate ∘ eval` boundary (2026-07-16): **ggdef-elaborate must model every ratified static rejection within its subset** — extend elaborate beyond may-move to the type/control-flow/signature axes (each E_ code as a typed rule, static walk, no execution), which THEN unblocks the coarse-kind four-lane spectests migration (floors move only after this). Sequencing: elaborate-extension track AFTER the coarse-kind split lands (the split is floor-neutral and independent). Own scout→brief→gauntlet (Rust, spec/ggdef/, not bootstrap-gated).
  - **🆕 [LOW — registry-prose completeness, from the self-host reject-diagnostic pass-1 review 2026-07-16] `E_LocalBorrowBind` is emitted by BOTH production (`src/semantic/errors.rs:715`) and (post-reject-diagnostic-landing) the self-host, but is ABSENT from the registry-prose table `spec/prose/diagnostic-codes.md` (which the briefs call "the source of truth").** Add the missing `E_LocalBorrowBind` row so the registry is actually complete. Zero conformance impact today (no d10a spectest exists), pure doc completeness. Trivial — fold into the next diagnostic-codes doc touch.

- [`t0356`](todo/t0356.md) **MED** — [MEDIUM — D8 float chain, filed by P1-infra review 2026-07-06; ~63-79 fixtures held behind it] Three sequenced prerequis…
- [`t0357`](todo/t0357.md) **MED** — [MEDIUM — ggdef, owner-question-driven 2026-07-06] 🔍 HOST-INHERITANCE AUDIT: enumerate and PIN every place spec/ggdef's…
- [`t0920`](todo/t0920.md) **MED** — [MED — ggdef PHASE-0 SUBSET GAP, filed while closing the per-function-prescan class] The 3 standing generic-equip EXCLUD…
- [`t0921`](todo/t0921.md) **MED** — [MED — ggdef ORACLE HYGIENE; the same generic equip gets two different subset answers depending on how it is spelled] re…
- [`t1081`](todo/t1081.md) **MED** — 🆕📐 [MED — ggdef SUBSET GAP, Core #9's *"out-of-subset shapes get a note + a filed subset gap"*; filed 2026-09-04 by R49…
- [`t1440`](todo/t1440.md) **MED** — 🆕🔬 [MED — A SUBSET GAP ON A RATIFIED OPERATOR FAMILY, RECORDED SO IT IS NOT MISTAKEN FOR AGREEMENT. Found 2026-09-05 by…
### Low

- [`t0358`](todo/t0358.md) **MED** — 🆕🧹 [MED-LOW — typed-metadata smell in OUR OWN test infra, self-filed 2026-07-16] The ggdef corpus out-of-subset mechanis…
- [`t0359`](todo/t0359.md) — 🧹 [P1-B review follow-up, 2026-07-06] ggdef frontmatter reader: unknown TOP-LEVEL keys with nested/|-block values error…

- [`t0360`](todo/t0360.md) — 🧹 [P1-B review follow-up, 2026-07-06] ggdef frontmatter Expect{exit,stdout} is RUN-TIER-ONLY — not the all-tier contract…

- [`t0361`](todo/t0361.md) — 🧹 [ggdef hardening, from P1-A pass-2 review 2026-07-06; PRE-EXISTING, unreachable via gg check] Closure bodies inherit t…

- [`t0362`](todo/t0362.md) — 🧹 [P1-G follow-up] Extend diagnostic codes to lex/parse time. LexErrorKind/ParseErrorKind still render bare error: (note…

- [`t0363`](todo/t0363.md) **LOW** — 🆕 [LOW — T1-zone, from the R-C scout 2026-07-10] Combinator-route spectest twin for trap_unwrap_error_on_ok — the existi…

#### Trap-normalization (D11) T2b follow-ups (filed 2026-07-10 by the T2b executor)
- [`t0364`](todo/t0364.md) **LOW** — [LOW — reference-grade span plumbing; not conformance-compared] The RARER bounds-class sites still trap trap[T_Bounds] a…

- [`t0762`](todo/t0762.md) **LOW** — [LOW — ggdef SUBSET GAP, Core #9 note] meta for as a MATCH-ARM generator is outside the ggdef subset, so that shape has…
- [`t0745`](todo/t0745.md) **LOW** — 🆕 [LOW — ggdef subset gap, Core #9's "out-of-subset shapes get a note + a filed subset gap"; filed by R47 Track D1 as th…
- [`t0753`](todo/t0753.md) **LOW** — 🆕📐 [LOW — ggdef SUBSET GAP, Core #9's *"out-of-subset shapes get a note + a filed subset gap"*; filed by R47 Track A1 as…
- [`t0906`](todo/t0906.md) **LOW** — 🆕 [LOW — ggdef subset gap, Core #9's "out-of-subset shapes get a note + a filed subset gap"; filed 2026-08-31 by R48 Tra…
- [`t0930`](todo/t0930.md) **LOW** — 🆕📐 [LOW — ggdef SUBSET GAP, Core #9's "out-of-subset shapes get a note + a filed subset gap"; filed by R48 Track β as th…
- [`t1227`](todo/t1227.md) **LOW** — 🆕📋 [LOW — SUBSET GAP, filed so a Core #9 ABSTENTION is a citation rather than a silent omission; from R49 Track S-a2] gg…
## Semantics / reference-grade rejection

- [`t0556`](todo/t0556.md) **HIGH** — [HIGH — SH lane, Core #10 lower-or-reject. A SILENT-TRUNCATION MISCOMPILE, not a missing feature.] The self-host silentl…

- [`t0365`](todo/t0365.md) **HIGH** — 🆕🚨 [HIGH — Core #10 CATEGORY ERROR, not a too-narrow rule; found 2026-08-24 by R44 Track A, grep-before-file done] There…

- [`t0366`](todo/t0366.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG VALUE from safe syntax, both backends, gg check clean; found 2026-08-24 by R44 Track A] A compre…

- [`t0367`](todo/t0367.md) **HIGH** — 🆕🚨 [HIGH — Core #8 WRONG-ACCEPT then SILENT RUNTIME FALL-THROUGH on BOTH compilers, and ggdef is RIGHT; found 2026-08-22…

- [`t0368`](todo/t0368.md) **HIGH** — 🆕🚨 [HIGH — Core #8 BOTH-LANE WRONG-ACCEPT, ggdef is RIGHT; found 2026-08-22 by the R44 Track-C scout; SECOND MEMBER of t…
- [`t0369`](todo/t0369.md) **HIGH** — 🆕🚨 [HIGH — Core #10 SILENT FALL-THROUGH class in RUST gg; ⚠ ATTRIBUTION REFUTED AND RE-ROOTED 2026-08-22 by the R44 Trac…

- [`t0370`](todo/t0370.md) **HIGH** — 🆕🚨 [HIGH — Core #10 SILENTLY MISSING DIAGNOSTIC in RUST gg; THIRD MEMBER of the Rust catch-all-walker class; filed 2026-…

- [`t0371`](todo/t0371.md) **HIGH** — 🆕🐛💥 [HIGH — MEMORY-UNSAFE, BOTH backends, gg check clean; measured 2026-08-19 by R43 Track C] auto f = bump loses the &…
- [`t0372`](todo/t0372.md) **HIGH** — 🆕🚨 [HIGH — MEMORY-UNSAFE misparse, gg check clean, BOTH backends; found by the R41 S4 scout 2026-08-10, orchestrator-ver…
- [`t0373`](todo/t0373.md) **HIGH** — 🆕🚨 [HIGH — Core #8 CROSS-BACKEND DIVERGENCE, found by the R41 S3 scout 2026-08-10, measured both backends] main throws i…
- [`t0374`](todo/t0374.md) **LOW** — 🆕📐 [LOW — docs rot invisible to CI; R41 S4 scout 2026-08-10] ~933 gorget code blocks in docs/ are compiled by NO harness…
- [`t0375`](todo/t0375.md) **MED** — 🆕⚙️ [MED — RATIFIED-UNIMPLEMENTED rider, surfaced by the T-FMT-A brief gauntlet pass-4 2026-08-10 (Core #15e Q1: pass-3…
- [`t0376`](todo/t0376.md) **LOW** — 🆕🐛 [LOW — lexer gap; R41 S1 scout 2026-08-10] Exponent float literals (1e10) do NOT parse — a lexer gap, not an fmt defe…
- [`t0377`](todo/t0377.md) **HIGH** — 🆕🚨 [HIGH — VISIBILITY MODEL: spec-vs-implementation divergence + OWNER DESIGN CALL REQUIRED; measured 2026-08-08 by dire…
- [`t0378`](todo/t0378.md) **MED** — 🆕🐛 [MED — D26 shift-fallible Route B lowering gap, filed 2026-08-06, tightened in the F4 review fold] <<! / >>! at a Res…
- [`t0379`](todo/t0379.md) **MED** — 🆕🐛 [MED — D26 auto-infer refinement, filed 2026-08-06] Closure auto-infer: a +! inside a Expr::Closure body currently do…
- [`t0380`](todo/t0380.md) **MED** — 🆕🐛 [MED — D26 SH lane-lag Route B + typecheck rejects + auto-infer, filed 2026-08-06 by the D26 F2 landing] The Round XX…
- [`t0381`](todo/t0381.md) **HIGH** — 🆕🚨 [HIGH — Core #8 both backends silently wrong output; discovered 2026-08-06 by Round XXXIII D26 F1 executor while writ…
- [`t0382`](todo/t0382.md) **HIGH** — 🆕🚨 [HIGH — Core #8 BOTH BACKENDS, filed 2026-08-06, sibling of Round XXXII OPAQUE-HANDLE RECEIVER-ABI class fix] Vector[…
- [`t0383`](todo/t0383.md) **MED** — 🆕🐛 [MED — self-host lane gap surfaced by the Round XXXII SH-mirror scout 2026-08-06] SH-Ax1: struct { AtomicInt c } fail…
- [`t0384`](todo/t0384.md) **HIGH** — 🆕🚨 [HIGH — Core #9 lane-lag; measured by Round XXXII Track D+E on 2026-08-06 while implementing sub-track D′] SH's lower…
- [`t0385`](todo/t0385.md) **HIGH** — 🆕🚨 [HIGH — Core #8 both backends, runtime double-free; measured 2026-08-06 by Round XXXII Track D+E output-review B3; re…
- [`t0386`](todo/t0386.md) **HIGH** — 🆕🚨 [HIGH — Core #10 accept-should-reject; measured 2026-08-06 by Round XXXII Track D+E output-review B4; repro committed…
- [`t0387`](todo/t0387.md) **HIGH** — 🆕🚨 [HIGH — Core #10 accept-should-reject vs docs (docs/language-reference.md:1366); OWNER RECONCILIATION REQUIRED, filed…
- [`t0388`](todo/t0388.md) **HIGH** — 🆕🚨 [HIGH — Core #8 REVERSE SUCCESSION; the SELF-HOST half of the callable-& ABI defect, re-measured 2026-08-19] The self…
- [`t0390`](todo/t0390.md) **MED** — 🆕🐛 [MED — Core #9 accept/reject divergence; measured 2026-08-05 by the array scout] for (i,v) in a.enumerate() MATCHes,…
- [`t0391`](todo/t0391.md) **MED** — 🆕🐛 [MED — latent alias, ACCIDENTALLY correct today; found 2026-08-05 by the guard scout] Shared[T].get() has the SAME al…

- [`t0392`](todo/t0392.md) — 🆕🐛 [xhigh review of c3237b7b..cbb21f28 (the B1/B2/liveness/ggdef-verdict wave), filed 2026-07-16 — 15 CONFIRMED findings…
- [`t0393`](todo/t0393.md) **HIGH** — 🆕🐛 [HIGH — SILENT-ACCEPT-GARBAGE residual; facet (ii) TUPLE named-field CLOSED 2026-07-17 Track C — t.foo → E_NoFieldFou…
- [`t0394`](todo/t0394.md) — 🆕🐛 [xhigh code-review of f42eea96..7aad1844, filed 2026-07-10] D11/D23-wave RESIDUALS — 3 HIGH correctness holes + verif…
- [`t0395`](todo/t0395.md) — 🐛 [PRODUCTION MISCOMPILE — surfaced by P1-A §10.3 fold 2026-07-06, ggdef differential; PENDING pass-2 review confirmatio…

- [`t0396`](todo/t0396.md) **MED** — 🆕🐛 [MED — D36 read-face .method() auto-deref on Mutex[Box[Trait]] / Guard[Box[Trait]] (post-Tracks-P/R/N2 residual)] Gua…
- [`t0038`](todo/t0038.md) **HIGH** — 🆕🚨 [HIGH — Core #8 SOUNDNESS: an UNDEFINED TYPE NAME is silently accepted in 9 of 10 type positions; found 2026-08-17 by…

#### 🆕 GUARDS-SLICE G1 DISCOVERIES (filed 2026-07-18 by the silent-fallthrough enumeration — each a Core-#10 silent drop, empirically verified with wrong-output probes; the ratchet pins the SITES, the fixes are separate Core-#9 all-lanes tracks)
- [`t0397`](todo/t0397.md) **HIGH** — [HIGH — WRONG-CODE, Rust lane] for (a,b,c) in Dict[int,int] silently binds ZEROS. lower_for_dict's _ => (src/ir/lowering…
- [`t0398`](todo/t0398.md) **HIGH** — [HIGH — WRONG-CODE, BOTH LANES] Nested destructure over a Dict value drops the inner bindings. for k,(a,b) in Dict[int,(…
- [`t0399`](todo/t0399.md) **LOW** — [LOW — defensive arm] lower_var_decl's _ => (stmts/mod.rs:1154) is a silent no-op arm reachable only if parser/semantic…

- [`t0400`](todo/t0400.md) — 🚩 REFERENCE-GRADE DEFECT — the self-host (gg-selfhost) BUILD path silently ACCEPTS ill-typed programs (surfaced 2026-06-…
- [`t0401`](todo/t0401.md) **HIGH** — 🆕🐛💥 [HIGH — check-accepts + BACKEND ICE (≥2 production bugs), from RV-F pass-1 2026-07-16] A bare callable IDENTIFIER at…
#### ====== REFERENCE-GRADE REJECTION TRACK (owner-asked 2026-06-27; audit @ 65c2cdc0) ======
⚠⚠ OWNER STEER 2026-06-27 (TWO-PHASE): **PHASE 1 (NOW, until 100% parity)** = PARITY is PRIMARY; do bounded single-node rejections OPPORTUNISTICALLY only (FP-enumerable, parity-neutral; they seed the negative corpus). **PHASE 2 (AFTER 100% parity)** = build the GENERAL type-enforcement pass (explicit-VarDecl type-mismatch → un-ignores `self_host_check_rejects_illtyped`; #6 TYPE-mismatch ~42-`self.error`-site migration; #8 borrow-check port). DEFERRED-NOT-DECLINED — the enforcement pass is worth doing, just AFTER parity (it's parity-orthogonal + reads more-trustworthy types once inference is maximally correct + the dominant risk is FP). So in PHASE 1: do NOT start #6/#7/#8 or the VarDecl-mismatch as a track; only land a rejection if it's bounded + FP-enumerable.
SELF-HOST IS TOO PERMISSIVE: Rust gg rejects 116 fixtures, self-host ACCEPTS 109 (emits C). Self-host typecheck is an INFERENCE pass (~6 control-flow diagnostics only), NO general type/arg/trait/const enforcement, NO borrow/move/safety pass. Parity-NEUTRAL (rejected fixtures are RUST-REJECTED, excluded from denom) — pure correctness wins per invariant #8.
TEST MECHANISM (exists, under-used): `check_gg_fails` (integration.rs:6486) = Rust rejects; `self_host_driver_rejects_invalid_program` (integration.rs:16510, runs driver `--lir-c`, asserts non-zero exit + codespan + EMPTY stdout) = self-host rejects. Clone the latter per gap. (The hole: c_emit_comparison:15182 + self_host_runtime_diff:17897 `return RustRejected` without running the self-host.)
★ EVERY rejection MUST gate on: 1208-Rust-accepted-fixture FP sweep (0 false positives) + `self_host_bootstrap_fixed_point` (a new reject that trips the self-host's OWN 667K-line source breaks bootstrap) + `type_comparison`.
WRITE SITE: self_host_typechecker/typecheck.gg (SYMLINKED into self_host_lowerer → affects build/check + type_comparison + bootstrap).
LANDING ORDER (most-bounded first; items 1/2/3a are in DONE.md):
3b. wrong_arg_count / wrong_field_count (signature via infer_expr_type/scope) — ⚠ wrong_arg_count is ENTANGLED with the filed 🐛 Rust-gg method-default-arg-fill defect (Rust mis-counts method calls that omit a defaulted arg); resolve that defect FIRST, or scope 3b to free-function arg-count only.
4. CONVERT/CAST single-node form-rejections (str()/cast-name calls, deref-non-box, string-index-assign, out-of-range).
5. non_exhaustive_match (MODERATE — collect-all-variants + diff; else/_ wildcard + qualified-vs-bare must be exact).
6. TYPE-mismatch family ~19 (BIGGER behavioral decision — typecheck deliberately non-enforcing; respect the 42 type_comparison supersets; FP-sweep each).
7. const_assign / await_outside_async — parser DISCARDS local-`const` (parser.gg:3546) + `async` (:3113) flags → thread flag through parser+AST FIRST, then a bounded check.
8. DEEP BORROW-CHECK port ~44 (MOVE ~17/BORROW ~8/ARENA ~12/CLOSURE-CAPTURE ~5/CLOSURE-ESCAPE ~2): port Rust `BorrowChecker` (safety/mod.rs:241, ~2000 lines: var_states move-dataflow, var_origins lifetime, arena_depth, loop_depth, struct-field-ref flags, CoW carve-outs). HIGH FP risk (carve-outs: loop-local re-create, `x=f(!x)` rebind waiver, CoW accept-live-at-ctor, imported-module skip). Multi-session sub-project. The escaping-mutating-closure + reassign-while-captured cases I originally cited live HERE (CLOSURE-ESCAPE/CAPTURE).

#### [rejection follow-up, invariant #8 — from Rejection-#1 review] positional-after-named on METHOD calls accepted by BOTH compilers
Rust gg's `check_named_args_and_defaults` (PositionalAfterNamed) is invoked at ONLY the free-function ECall path (typecheck.rs:1569), NOT for method calls — Rust gg ACCEPTS `s.compute(a=1, 2)` (exit 0), and so does the self-host. So Rejection #1 (self-host ECall, not EMethodCall) faithfully mirrors Rust's CURRENT scope. But per invariant #8 this is a ≥2-bug defect BOTH compilers share: reject positional-after-named on METHOD calls in Rust gg (extend the check to the EMethodCall path) AND the self-host (add the same walk to the EMethodCall typecheck arm) + a negative fixture asserting both reject. Separate follow-up.

- [`t0402`](todo/t0402.md) **HIGH** — [HIGH — PRODUCTION BUG, found by B1 output-review probing 2026-07-06] 🐛 Struct/enum-CTOR named args bind POSITIONALLY in…
- [`t0683`](todo/t0683.md) **HIGH** — 🆕🚨 [HIGH — == ON TWO EQUAL TUPLES RETURNS false; measured 2026-08-27, orchestrator-verified] Tuple equality answers ADDR…
- [`t0691`](todo/t0691.md) **HIGH** — 🆕🚨 [HIGH — an INLINE STRUCT CONSTRUCTOR inside an f-string lowers as a TUPLE; gg check CLEAN, C refuses to compile, LLVM…
- [`t0692`](todo/t0692.md) **HIGH** — 🆕🚨 [HIGH — Core #10 SILENT DROP: assigning to the VALUE binding of a dict loop is ACCEPTED and SILENTLY DISCARDED, rc 0,…
- [`t0693`](todo/t0693.md) **HIGH** — 🆕🚨 [HIGH — d[k] = v while iterating the dict DIRECTLY aborts the compiler at rc 101 with the internal validator label E_…
- [`t0694`](todo/t0694.md) **HIGH** — 🆕⚖️ [HIGH — IMPLEMENT D49 (ratified owner 2026-08-27): for x in &set and for k in &dict must be a CHECK-TIME REJECTION;…
- [`t0701`](todo/t0701.md) **HIGH** — 🆕🚨💥 [HIGH — STRUCT CONSTRUCTOR ARGUMENTS ARE NOT TYPE-CHECKED AGAINST THE FIELD TYPE; gg check ACCEPTS and the payload s…
- [`t0710`](todo/t0710.md) **HIGH** — 🆕🚨 [HIGH — AN IMPORT STATEMENT CHANGES WHETHER A PROGRAM TYPECHECKS; found 2026-08-27 by R45 Track A brief-review pass 2…
- [`t0718`](todo/t0718.md) **HIGH** — 🆕🚨 [HIGH — Layering rule 2 class retirement: NAME MATCHING DECIDES TYPE SEMANTICS, and the typed replacement already exi…
- [`t0721`](todo/t0721.md) **HIGH** — 🆕🐛 [HIGH — a WRONG ANSWER with rc 0, the worst outcome; the REFERENCE LAGS THE SELF-HOST, i.e. a succession milestone (s…
- [`t0878`](todo/t0878.md) **HIGH** — 🆕🐛 [HIGH — Vector[T].map(f) / .flat_map(f) with f a Callable[U(T)] PARAMETER links against nothing; found R48 Track A, w…
- [`t0927`](todo/t0927.md) **HIGH** — 🆕🐛 [HIGH — a closure that CAPTURES a Callable[T] parameter cannot be compiled AT ALL, on either backend; found R48 Track…
- [`t0940`](todo/t0940.md) **MED** — 🆕🐛 [MED — Core #10 SILENT DROP: gg check ACCEPTS, the C build dies] Guard[T].clone() lowers to a call on gorget_guard_cl…
- [`t0947`](todo/t0947.md) **HIGH** — 🆕🐛 [HIGH — Core #10 SILENT DROP of user STATEMENTS, gg check clean, both backends; live in shipped stdlib; found 2026-09…
### High


- [`t0434`](todo/t0434.md) **HIGH** — Result→T auto-propagation — retire the residual consumer-side maybe_auto_propagate safety nets. Producer-side centraliza…

- [`t1018`](todo/t1018.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT on ordinary safe syntax, gg check CLEAN, both backends; found
- [`t0989`](todo/t0989.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT on the most ordinary syntax in the language, gg check CLEAN, rc 0, both
- [`t1022`](todo/t1022.md) **MED** — 🆕🐛 [MED — Core #9 ACCEPT/REJECT LANE DIVERGENCE, created by R49 Track C and recorded rather than
- [`t1198`](todo/t1198.md) **HIGH** — 🆕🚨 [HIGH — A TYPE CHECK THE COMPILER ALREADY PERFORMS IS ABSENT AT EVERY CONSUMING POSITION, and every position that esc…
- [`t1126`](todo/t1126.md) **HIGH** — 🆕🚨 [HIGH — < > <= >= ON A STRUCT ARE ACCEPTED AND ANSWERED FROM ALLOCATION ADDRESSES, WITH THREE DIFFERENT ANSWERS ACROS…
- [`t1132`](todo/t1132.md) **HIGH** — 🆕🚨 [HIGH — D46's RIDER MAKES SIX MORE AGGREGATES INTRINSICALLY COMPARABLE AND NONE OF THEM HAS A LOWERING: Some(1) == So…
- [`t1082`](todo/t1082.md) **HIGH** — 🆕🐛 [HIGH — THREE CELLS OF ONE FAMILY: a Box[Trait] collection's element type reads back WITHOUT THE BOX at three separat…
- [`t1270`](todo/t1270.md) **HIGH** — 🆕🐛💥 [HIGH — CRASH ON A VALID, DOCUMENTED PROGRAM: with m.lock() as g: ICEs gg build rc 101 on BOTH backends; found 2026-…
- [`t1303`](todo/t1303.md) **HIGH** — 🆕🚨 [HIGH — MEMORY UNSAFETY on the REFERENCE lane, in a program with no unsafe, no ownership operator and no FFI. Core #8…
- [`t1361`](todo/t1361.md) **HIGH** — 🆕🔥 [HIGH (re-graded from CRITICAL 2026-09-05, see addendum) -- AN UNCHECKED MEMBER-ACCESS HOLE ON THE ENTIRE LAZY-ITERAT…
- [`t1403`](todo/t1403.md) **HIGH** — 🆕🐛 [HIGH — NON-TERMINATION + OOM FROM A gg check-CLEAN PROGRAM, both backends; found 2026-09-05 by the orchestrator whil…
- [`t1393`](todo/t1393.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY-UNSAFE FROM ORDINARY SAFE SYNTAX; gg check CLEAN, gg build rc 0, then SIGSEGV on BOTH backends; f…
- [`t1329`](todo/t1329.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY-UNSAFE FROM ORDINARY SAFE SYNTAX; gg check rc 0, then heap-use-after-free; found 2026-09-05 by R5…
- [`t1350`](todo/t1350.md) **HIGH** — 🆕🚨 [HIGH — THE SINGLE-OWNER CARVE-OUT HAS A HOLE THE SIZE OF EVERY GENERIC ADAPTER. Found 2026-09-05 by R50 Track E's br…
- [`t1443`](todo/t1443.md) **HIGH** — ⚖🆕 [HIGH — AN UNRATIFIED SEMANTICS QUESTION, AND THE FIX FOR t1410 WOULD HAVE RATIFIED IT BY FIXTURE. Found 2026-09-05 b…
### Medium



- [`t0404`](todo/t0404.md) **HIGH** — 🆕🚨 [HIGH — check-passes / debug-ICE / RELEASE SILENT MISCOMPILE; found by the R42 Track-D brief-review pass 6, orchestra…

- [`t0405`](todo/t0405.md) — 🐛 A Callable-typed &-PARAMETER ICEs the compiler (filed 2026-07-26). gg check passes, then gg build panics: "GIR validat…

- [`t0406`](todo/t0406.md) **HIGH** — 🆕🐛💥 [HIGH — TWO CELLS REMAIN post-Track-K; MEMORY-UNSAFE, gg check ACCEPTS; measured 2026-07-28] Non-identifier-callee c…

- [`t0407`](todo/t0407.md) **MED** — 🆕🐛 [MED — SEVENTH Callable costume, and the only one that fails at LINK; gg check ACCEPTS; measured 2026-07-27] Calling…

- [`t0408`](todo/t0408.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, C backend then FAILS TO BUILD; found 2026-07-26 by sigil-prose gauntlet pass 15, parent-repr…

- [`t0409`](todo/t0409.md) — 🐛 E_BorrowAcrossAwait HAS NO POSITIVE CONTROL EITHER — and the two dead guards PROP EACH OTHER UP (filed 2026-07-26). la…

- [`t0410`](todo/t0410.md) — 🐛 E_SpawnWithBorrowedRef HAS NO POSITIVE CONTROL AND MAY BE A DEAD GUARD (filed 2026-07-26). docs/book/14-concurrency.md…

- [`t0411`](todo/t0411.md) — 📄 thread.spawn IS A DOC FOSSIL — 6 sites across two docs (filed 2026-07-26). thread.spawn(...) does not exist: gg check…

- [`t0412`](todo/t0412.md) — 🆕 ROUND-16 (T3) ??-REJECT FOLLOW-UPS (filed 2026-06-30; the ??/*x-on-non-(Option|Result) reject LANDED e27b9c25+7e341681…

- [`t0413`](todo/t0413.md) — 🆕 POST-CLOSURE ROUND FOLLOW-UPS (filed 2026-06-27, discovered by the GO #2 v2 + Rejection #1/#2 output-reviews):

- [`t0414`](todo/t0414.md) — 🚀 CAMPAIGN NEXT-WAVE (owner-funded + decided 2026-06-22; all scouted, briefs/scouts next — each its own brief→≥3 reviews…

- [`t0415`](todo/t0415.md) — 🎨 CAST-VIA-CONSTRUCTION LANGUAGE REDESIGN (owner-approved DESIGN 2026-06-20; impl deferred) — RFC the cast-via-construct…
- [`t0416`](todo/t0416.md) — 🧭 ERROR-MODEL LANGUAGE DIRECTION (owner brainstorm 2026-06-20; DRAFT — SCOUT + 3 SEQUENTIAL REVIEWS DONE, clean SIGN OFF…
- [`t0417`](todo/t0417.md) **MED** — 🆕🐛 [MED — residual of Track A struct-value match 2026-07-17] Generic-struct constructor patterns (Pair[A,B] / ResolvedTy…
- [`t0418`](todo/t0418.md) **MED** — 🆕🐛 [MED — residual of Track A struct-value match 2026-07-17] Wrong constructor name on a struct scrutinee (match p: case…
- [`t0419`](todo/t0419.md) — 🐛 [gorget-sheets snag #58 — check-time, 2026-07-07] Cross-module int bindings require public (E_PrivateImport). from cod…

#### 🆕 STAGE-1B #2 LANDING FILINGS (2026-07-20; the E_TypeInValuePosition reject LANDED Rust-side — record in DONE.md; these are the remaining lane + the siblings it exposed)
- [`t0420`](todo/t0420.md) **MED** — [MED — SH lane mirror, Core #9] The SH typechecker has the IDENTICAL type-name-in-value-position bug (self_host_typechec…
- [`t0421`](todo/t0421.md) **LOW** — [LOW/MED — pre-existing FieldAccess sibling, Core #8] Type.instance_field (int y = Point.x where Point is the TYPE) type…
- [`t0422`](todo/t0422.md) **MED** — [MED — accept-then-CC-FAIL, Core #10 lower-or-reject] Variant-as-HOF-value (xs.map(Some)) CHECKS clean but CC-FAILs at b…
- [`t0423`](todo/t0423.md) **MED** — [MED — SH lane, pre-existing subset gap found by the D31 scout] The SH typechecker has NO call-site ownership check AT A…

#### 🆕 D31 FULL-STRICT FOLLOW-UPS (filed 2026-07-20 by the D31 ADDENDUM-2 executor; the full-strict flip itself LANDED — record in DONE.md)
- [`t0424`](todo/t0424.md) **MED** — [MED — DX tooling, the ADDENDUM-2 rider] gg fmt-adjacent auto-insertion of the missing call-site sigil. The E_OwnershipM…
- [`t0425`](todo/t0425.md) **LOW** — [LOW — pre-existing, zero blast radius, Core #8-adjacent] & on a TEMPORARY into a & param is accepted but is a dead writ…

- [`t0426`](todo/t0426.md) — 🌟 PRELUDE ARC — OWNER-CHOSEN 2026-06-09 (name-match-free, "typed-but-compiler-known"; touches BOTH compilers; serializes…

- [`t0427`](todo/t0427.md) — ⚙ ERROR-MODEL FEATURE — From-on-bare-rethrow implicit conversion (owner-designed; Rust-FIRST then self-host mirror; NOT…

- [`t0428`](todo/t0428.md) — 🐛 ESCAPING SLICE (int[]) silently MISCOMPILES — reject it at check. Returning a slice of a *local* is rejected (Dangling…

- [`t0429`](todo/t0429.md) — 🐛 AGGREGATE-LITERAL auto-prop desync — 3 same-class positions remain (RUN-verify before fixing): ⚡ FIX SHAPES SUPERSEDED…

- [`t0430`](todo/t0430.md) — 🔒 METHOD-RESOLUTION TOTALITY — Phase 2 (widen the unresolved-method gate to emit NoMethodFound) is BLOCKED by a measured…

- [`t0431`](todo/t0431.md) — 🐛 Self-host lowerer driver OMITS a borrow-check pass (scout agent af99281f). Pipeline is parse→resolve→typecheck→lower→v…

- [`t0432`](todo/t0432.md) — Safety-pass branch-divergence audit (Cluster C, post-Snag-#39 audit). Defer. Snag #39's fix added save_branch_state/rest…

- [`t0433`](todo/t0433.md) — caller_location() builtin + multi-frame stack walking [LOW]. (a) a #[track_caller]-like attribute / implicit caller_loca…

- [`t0435`](todo/t0435.md) **MED** — 🆕🐛 [MED — module-loader leniency, found by the CoW-1A output-review probe 2026-07-17] Importing ANY name from a module s…

- [`t0436`](todo/t0436.md) — 🆕 [D10(b) self-root follow-up, filed 2026-07-14 — OWNER QUESTION] For-loop iterator-invalidation is ROOT-granular for BO…
- [`t0439`](todo/t0439.md) — 🆕 [D23/T3a follow-up, filed 2026-07-10, MEDIUM-design] General must-use on Result. D23/T3a pins ONLY that an unhandled t…
- [`t0440`](todo/t0440.md) — ⚖️ [language-design question, from P1-A §10.3 fold 2026-07-06 — route to decisions.md open queue at the next owner batch…

- [`t0441`](todo/t0441.md) — 🐛 [SILENT MISCOMPILE — D20 RATIFIED 2026-07-06, ruling settled: reject with the f-string fix-it; fold with the to_string…

- [`t0442`](todo/t0442.md) — 🧹 [round-33 alloc scout filing — DIRECTION RATIFIED 2026-07-06 as D13 TWO-STEP (decisions.md LOG): step 1 = REJECT bare…

- [`t0443`](todo/t0443.md) — 🐛 LATENT: is_collection_assignment accepts ANY array/set literal regardless of ELEMENT type. is_collection_assignment (s…

- [`t0444`](todo/t0444.md) — Ordinal built-in trait never registered → wrong-signature equip X with Ordinal: compiles clean (latent validation gap).…

- [`t0445`](todo/t0445.md) — Compiler footguns (latent type-system validation gaps, same class as Ordinal):

- [`t0446`](todo/t0446.md) — Drop the imported-module typecheck-error truncate (writer-site fixes). check_items_recursive_tc (typecheck.rs) truncates…

- [`t0447`](todo/t0447.md) **LOW** — 🆕 [LOW — self-host over-reject, DORMANT + UNREACHABLE; from the flip-tracks landing 2026-07-17] The self-host EStructLit…
- [`t0448`](todo/t0448.md) — 🧹 [resources scout 2026-07-06, LOW] to_string/String(int) rejections need a FIX-IT pointing at the canonical f"{n}" (and…

- [`t0449`](todo/t0449.md) — 🔵 TAIL-CALL OPTIMIZATION (TCO) — LATER (owner-set; a real language feature). gg has no TCO. Phase 1 (tractable, high-val…

- [`t0450`](todo/t0450.md) **MED** — 🆕 [MEDIUM — same staleness family as R-A's Fix 1, from R-A pass-1 review 2026-07-10] ast_type_to_resolved binds stale Im…

- [`t0451`](todo/t0451.md) **MED** — 🆕🐛 [MED — fix-it-validity CLASS, filed Round XXIX Track C 2026-08-03; the class-retirement mechanism landed with 1 row (…

- [`t0681`](todo/t0681.md) **MED** — 🐛 [MED — a SELF-CONTRADICTING DIAGNOSTIC; filed 2026-08-27 by R45 Track A brief-review passes 1–2; RE-SCOPED 2026-09-04…
- [`t0760`](todo/t0760.md) **MED** — [MED — Layering rule 4, two sites; found while closing t0699] TWO consumers re-derive f-string interpolations from RAW T…
- [`t0904`](todo/t0904.md) **MED** — 🆕🧹 [MED — layering; no user-visible defect, AGENTS.md § No name matching violation; filed 2026-08-31 by R48 Track D2] Th…
- [`t0945`](todo/t0945.md) **MED** — 🆕🐛 [MED — Core #8 BOTH LANES AGREE ON THE WRONG ANSWER, over-rejection with a FALSE diagnostic] c[i] on a user generic i…
- [`t0950`](todo/t0950.md) **MED** — 🆕🧹 [MED — a Core #10 guard that rejects LOUDLY but in the WRONG PHASE: an ICE where the invariant asks for a check-time…
- [`t0957`](todo/t0957.md) **MED** — 🆕🐛 [MED — an ACCEPT/REJECT split driven by the SPELLING of the index, not by anything semantic; found 2026-09-03 in pass…
- [`t1020`](todo/t1020.md) **MED** — 🆕🐛 [MED — Core #9 lane lag: the self-host cannot compile ANY method call on a Box[UserStruct];
- [`t1021`](todo/t1021.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT on the self-host lane, gg check clean; found 2026-09-03 by R49
- [`t1023`](todo/t1023.md) **MED** — 🆕🐛 [MED — Core #10 lower-or-reject, gg check CLEAN on a method no bound provides; both backends;
- [`t0990`](todo/t0990.md) **MED** — 🆕🐛 [MED — DESIGN SMELL: a type whose whole reason to exist is its two ends has no way to name them.
- [`t1080`](todo/t1080.md) **MED** — 🆕📝 [MED — RATIFIED D27 IS NOT REFLECTED IN THE DIAGNOSTIC THAT TEACHES IT: E_MoveWithoutOperator still tells the user to…
- [`t1127`](todo/t1127.md) **MED** — 🆕🐛 [MED — @derive(Equatable) ON A GENERIC STRUCT DOES NOT REQUIRE ITS PARAMETER TO BE Equatable, so Pair[NonEquatable] =…
- [`t1265`](todo/t1265.md) **MED** — 🆕⚖ [MED — AN UNRULED CELL THE RATIFIED TEXT CLAIMS NOT TO HAVE: == on the D53 single-owner HANDLE family (Shared / Weak…
- [`t1068`](todo/t1068.md) **MED** — 🆕⛔ [OVER-REJECTION — a correct program is refused, and the refusal's stated premise is false; found R49 Track L while wi…
- [`t1405`](todo/t1405.md) **MED** — 🆕📐 [MED — CAMPAIGN, owner-ratified 2026-09-05: D27's ^ MIGRATION EXTENDS EVERYWHERE, superseding the earlier src/tests/l…
- [`t1408`](todo/t1408.md) **MED** — 🆕🧹 [MED — LAYERING RULE 3 + CORE #2: TWO SOURCES OF TRUTH FOR "IS THIS NAME A TYPE", ONE OF THEM A STRING MATCH; surface…
- [`t1376`](todo/t1376.md) **MED** — 🆕🐛 [MED — A ONE-FIELD newtype ACCEPTS ANY ARITY AT CHECK TIME. Found 2026-09-05 by R50 Track A2's brief-review pass 4; P…
- [`t1377`](todo/t1377.md) **MED** — 🆕🐛 [MED — A CASE WITH NO SUBJECT (SIX-Q #4): the rule that would fix it never gets to run. Found 2026-09-05 by R50 Track…
### Low

- [`t0452`](todo/t0452.md) **LOW** — 🆕 [LOW — diagnostic ergonomics follow-up from Round XXIX Track A close 2026-08-03] E_NotIndexable message text should na…
- [`t0453`](todo/t0453.md) **LOW** — [LOW — A2-R2 M2 polish 2026-07-12] E_MoveWithoutOperator field/index sub-place message renders the ROOT name, not the ex…
- [`t0454`](todo/t0454.md) — ✨ FEATURE — support forward / cross-module const-refs in a const initializer (fixpoint registration; Low) (filed by the…

- [`t0455`](todo/t0455.md) — 🌀 DEFERRED: lexical wrapping { … } scope (filed when --overflow was retired 2026-06-21). After the flag retirement, +%/-…

- [`t0456`](todo/t0456.md) — (Low/Medium, CORRECTNESS) Static-decl initializers are NOT type-checked against their declared type. public static int X…

- [`t0457`](todo/t0457.md) — @[no_alloc] function annotation: compiler error on allocating operations.

- [`t0458`](todo/t0458.md) **LOW** — 🆕 [LOW-MED — production OVER-REJECTION (false positive), from RV-D pass-1 2026-07-16] Production's match fall-through su…
- [`t0459`](todo/t0459.md) — 🧹 [coarse-kind Fable output-review 2026-07-16, LOW — diagnostic-cascade polish] Production emits a cascading second E_Aw…

- [`t0460`](todo/t0460.md) **LOW** — 🧹 [LOW — Track I v2, filed 2026-07-28] Widen W_RecursiveBareParamMaterialize to MUTUAL recursion. v1 checks DIRECT self-…

- [`t0461`](todo/t0461.md) **LOW** — 🧹 [LOW — Track I v2, filed 2026-07-28] W_RecursiveBareParamMaterialize downgrade when the reached callee already fires W…
- [`t0462`](todo/t0462.md) **HIGH** — 🆕🚨 [HIGH — Core #10 SILENT DROP with TWO OPPOSITE FACES, one a wrong-REJECTION and one a wrong-ACCEPTANCE; filed 2026-08…

- [`t0463`](todo/t0463.md) **MED** — 🆕📐 [MED — Core #9 SUBSET GAP on BOTH non-Rust lanes, filed 2026-08-23 by R44 Track E; measured, not assumed] FIXED-SIZE…
- [`t0464`](todo/t0464.md) **MED** — 🆕🚨 [MED-HIGH — BOTH-LANE OVER-REJECTION (false positive), found 2026-08-23 by R44 Track F; two repros committed] A varia…

- [`t0465`](todo/t0465.md) **MED** — 🆕⚖ [MED — OPEN QUESTION, lane split at the PARSER; found 2026-08-23 by R44 Track F; repro committed] Rust gg REJECTS an…

- [`t0466`](todo/t0466.md) **MED** — 🆕🚨 [MED-HIGH — SELF-HOST WRONG-ACCEPT of a use-after-move; found 2026-08-23 by the R44 Track F OUTPUT-REVIEW; repro comm…

- [`t0830`](todo/t0830.md) **LOW** — 🆕🐛 [LOW — a fixture that pins a SHAPE but not the MECHANISM it names; found 2026-08-30 by the R47 D3a output review, whi…
- [`t0978`](todo/t0978.md) **LOW** — 🆕🧹 [LOW — typed-metadata debt, filed 2026-09-03 by R49 Track E as its own reference-grade follow-up; the one-line note i…
- [`t1131`](todo/t1131.md) **LOW** — 🆕🐛 [LOW — DIAGNOSTIC QUALITY, pre-existing and wider than the gate that surfaced it: an error raised inside f"{...}" ren…
- [`t1217`](todo/t1217.md) **LOW** — 🆕🐛 [LOW — a SURFACE GAP that makes one cell of the HOF result-element axis inexpressible; found 2026-09-04 by R49 Track…
## Backend / codegen

- [`t0467`](todo/t0467.md) **MED** — 🆕🐛 [MED — duplicate drop-glue emission; found 2026-08-24 by R44 Track A while rebuilding the comprehension emitters] A c…

### High

- [`t0468`](todo/t0468.md) — 🆕📐 [OWNER DIRECTIVE 2026-07-13 — standing] RETIRE sidecars + parallel structures (audit sweep). "All opportunities to re…
- [`t0469`](todo/t0469.md) — 🐛 [BACKEND DIVERGENCE — resources decision scout 2026-07-06] LLVM does not honor alloc= into a bare Arena the way C does…
- [`t0470`](todo/t0470.md) — 🆕🐛 [gorget-arena snag #1 — filed 2026-08-09, R39] LLVM backend drops the C typedef for FFI-only-module structs. A plain…

- [`t0471`](todo/t0471.md) **MED** — 🆕🐛 [MED — Layering rule-2 debt, GREW 2026-08-01 by Round XXIII γδ 3→5 prefixes; then Round XXIV Track E 5→7 prefixes; ME…
- [`t0472`](todo/t0472.md) **MED** — 🆕🐛 [MEDIUM — RESIDUAL after XIX Track Y Stmt::Expr fix; rephrased 2026-08-01 to reflect Round XXI Track A closing the SH…
- [`t0473`](todo/t0473.md) **MED** — 🆕🐛 [MEDIUM — codegen type-precision; discovered 2026-07-24 chasing the CI timeout] Self-host emits int32 → void* assignm…

- [`t0685`](todo/t0685.md) **HIGH** — 🆕🚨 [HIGH — Box[T].get() for any NON-PRIMITIVE T does not COMPILE on the LLVM backend; measured 2026-08-27 at HEAD, orche…
- [`t0687`](todo/t0687.md) **HIGH** — 🆕🚨 [HIGH — CALLING a Box[Callable[...]] is a compiler ICE; found 2026-08-27 by R45 Track A brief-review pass 7, orchestr…
- [`t0688`](todo/t0688.md) **HIGH** — 🆕🚨 [HIGH — Box[enum] with a resource-carrying variant: C REFUSES TO COMPILE, LLVM compiles then DOUBLE-FREES; found 2026…
- [`t0700`](todo/t0700.md) **HIGH** — 🆕🐛💥 [HIGH — Vector[Box[Trait]] NEVER DROPS ITS ELEMENTS: a LIVE LEAK at HEAD in a COMMITTED, PASSING fixture; found 2026…
- [`t0711`](todo/t0711.md) **HIGH** — 🆕🐛 [HIGH — CC-FAIL on BOTH lanes, so ≥2 bugs (Core #8); found R45 Track G while enumerating the postfix-link axis] A met…
- [`t0720`](todo/t0720.md) **HIGH** — 🆕🐛💥 [HIGH — ICE on Rust gg (exit 101), resolve-reject on the self-host, so ≥2 bugs (Core #8); found R45 Track G while en…
- [`t0729`](todo/t0729.md) **HIGH** — [HIGH — UNDEFINED BEHAVIOUR IN EMITTED CODE: an aggregate copy overruns its destination slot on the LLVM lane. RE-SCOPED…
- [`t0933`](todo/t0933.md) **HIGH** — 🆕🚨 [HIGH — an ICE on a program gg check ACCEPTS; found 2026-08-31 by R48 Track γ while probing the Iterator trait-arg sh…
- [`t0987`](todo/t0987.md) **HIGH** — 🆕🐛 [HIGH — Core #10 lower-or-reject violated by a _ => FALL-THROUGH, which Core #10 names as a
- [`t0977`](todo/t0977.md) **HIGH** — 🆕🐛 [HIGH — SILENT WRONG OUTPUT on ordinary safe syntax, gg check clean; found 2026-09-03 by R49 Track E while re-writing…
- [`t1049`](todo/t1049.md) **HIGH** — 🚨 [HIGH — for i, c in s.enumerate() TRAPS on any multibyte String, and is ACCIDENTALLY CORRECT on every ASCII one; found…
- [`t1050`](todo/t1050.md) **HIGH** — 🚨 [HIGH — SILENT WRONG OUTPUT at exit 0, no diagnostic; found 2026-09-03 by the R49 Track K brief-review gauntlet, execu…
- [`t1086`](todo/t1086.md) **HIGH** — 🚨 [HIGH — a higher-order call on Vector[Callable[…]] reads and frees through a misaligned function pointer: rc 139 on BO…
- [`t1087`](todo/t1087.md) **HIGH** — 🚨 [HIGH — SILENT WRONG OUTPUT: Deque[T].get(i) prints a small integer instead of the element, rc 0, gg check clean, on B…
- [`t1197`](todo/t1197.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG VALUE on C, llc BUILD FAILURE on LLVM, gg check clean; found 2026-09-04 by R49 Track M1's output…
- [`t1083`](todo/t1083.md) **HIGH** — 🆕🐛 [HIGH — a program the compiler ACCEPTS emits C that a C compiler REFUSES: error: redefinition of 'Box__Robot__drop'.…
- [`t1359`](todo/t1359.md) **CRITICAL** — 🆕🔥 [CRITICAL -- DOUBLE-FREE REACHABLE FROM ORDINARY SAFE SYNTAX, gg check clean; found 2026-09-05 by the R50 CoW design…
- [`t1330`](todo/t1330.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY-UNSAFE AT PRISTINE HEAD; found 2026-09-05 by R50 Track C1's brief-review pass 1, which BUILT the…
- [`t1418`](todo/t1418.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY-UNSAFE AT PRISTINE HEAD, BOTH BACKENDS, gg check CLEAN; found 2026-09-05 by R50 Track J's scout w…
- [`t1438`](todo/t1438.md) **HIGH** — 🆕🚨 [HIGH — A LANE DIVERGENCE IN THE *ANSWER*, NOT IN DEFINED-NESS. Found 2026-09-05 by R50 Track L's scout while measuri…
- [`t1373`](todo/t1373.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT ON THE DEFAULT BACKEND FROM struct { Box[int] }, THE PLAINEST SHAPE THERE IS. Found 2026-…
### Medium
- [`t0474`](todo/t0474.md) **MED** — 🆕🔧 [MED — prerequisite for retiring the last indirect-call shape heuristic; filed 2026-08-19 by R43 Track C] Tag LARGE n…
- [`t0475`](todo/t0475.md) **LOW** — 🧹 [LOW — Layering rule 3, one source of truth per axis; found 2026-08-19 by R43 Track C] src/backend/c_lir/helpers.rs ca…
- [`t0476`](todo/t0476.md) **MED** — 🆕🧹 [MED→ small CLASS FIX; SCOUTED 2026-08-05, and the original filing's PREMISE WAS WRONG] THREE parallel hand-lists dec…
- [`t0477`](todo/t0477.md) **MED** — 🆕🧹 [MED — Core #4 sibling drift; found 2026-08-05] The LLVM backend re-implements printf format fixing instead of callin…
- [`t0478`](todo/t0478.md) **MED** — 🆕🧹 [MED — dead code carrying a live miscompile arm AND an optimizer pessimization; found 2026-08-05] Inst::InlineC has Z…
- [`t0479`](todo/t0479.md) **MED** — 🆕🧹 [MED — Layering rule 1 (lossless on invariants); unfiled until now, found 2026-08-05] LIR Slot drops Local.ownership…
- [`t0480`](todo/t0480.md) **MED** — 🆕🧹 [MED — Core #4 sibling drift + Layering "no name matching"; found 2026-08-05, counts re-derived post-rebase] last_err…

- [`t0481`](todo/t0481.md) — 🆕 ROUND-32 EXCELLENCE-AUDIT CAMPAIGN FILINGS (2026-07-02; full report + regenerated numbers in the audit appendix — rege…

- [`t0482`](todo/t0482.md) — 🐛 RUST-GG CODEGEN — rethrow (String e): <String-transform> LEAKS the transformed error String (LeakSanitizer) (DISCOVERE…

- [`t0483`](todo/t0483.md) — 🖥 LLVM-BACKEND — 2 minor/distinct bugs filed by the alloca-hoist scout a09ff841 (2026-06-22; the coal_compute_live_block…
- [`t0484`](todo/t0484.md) — 🐛 LLVM latent: sibling variadic-sret extern path drops the varargs type (same class as b04684f4 Fix 2, UNREACHABLE today…

- [`t0485`](todo/t0485.md) — 🖥 x86_64 LLVM residuals (the 34 CI failures landed, see DONE.md; these REMAIN): (1) async_blocking_coroutine flaky runti…

- [`t0486`](todo/t0486.md) — Layering smell (NOT a quick cleanup — REFUTED ae8738a0; owner decided 2026-06-15 LEAVE the guard, keep this entry): name…

- [`t0487`](todo/t0487.md) — 🐛 CONFORMANCE: static collection prologue doesn't set .val_drop/.elem_drop — local ctor wires gorget_array_free (helpers…

- [`t0488`](todo/t0488.md) — 🔥 BURN-DOWN ROUND — remaining name-matching (DONE parts → DONE.md):

- [`t0489`](todo/t0489.md) — 🐛 PRE-EXISTING match-arm-binding LEAK (ASan, orthogonal to snag #11). An owned-String error caught by match … case Error…

- [`t0490`](todo/t0490.md) — 🔧 FIX-THE-ORACLE (Rust gg LEAK, low-pri) — Custom-drop struct as a FIELD leaks its inner fields. Wrapper{Container inner…

- [`t0491`](todo/t0491.md) — 🧹 retire the mangled-name inner-type slicing in unwrap/expect/unwrap_or lowering (src/ir/lowering/exprs/methods.rs:~620-…

- [`t0492`](todo/t0492.md) — 🐛 UPSTREAM GAP — enum_category NOT registered for Result with a USER error enum (Result[float, ParseError]/Result[int, P…

- [`t0493`](todo/t0493.md) — 🐛 LATENT: Box[Option[String]] / Box[Result[T]] deref-store-of-borrow is a UAF (ENUM-PAYLOAD POINTEES; STRUCT-resource ca…

- [`t0494`](todo/t0494.md) — (FIDELITY) THREE enum-side drop-name sites reconstruct tn + "__drop" by name WITHOUT the drop_collision_types guard — em…

- [`t0495`](todo/t0495.md) — (Medium, LLVM, FLAKY?) leak_string_heavy fails under the LLVM backend on x86_64 CI (run 28223658994, 2026-06-26): prints…

- [`t0496`](todo/t0496.md) — [Planner round 3, D4(i)] File: push_char on a String-view & binding ICEs at src/backend/c_lir/emit_types.rs:850 (GorgetS…

- [`t0497`](todo/t0497.md) — [Planner round 3, D4(ii)] File: a nested index-assign rooted at windows/chunks (v.windows(2)[0][0] = 777) ICEs at src/ir…

- [`t0498`](todo/t0498.md) — [G3 follow-up] insts.rs __clone name-match kill is DEFERRED (ABI-trap, not done in G3). emit_extern_call (src/lir/lower/…

- [`t0499`](todo/t0499.md) — [G3 follow-up] CallExtern reason field for closure clones. The closure-capture clone at src/ir/lowering/stmts/mod.rs (go…

- [`t0500`](todo/t0500.md) — Rust frontend: unify the 7 lower_for_* functions into a single scaffold + per-type element extractor (src/ir/lowering/st…

- [`t0501`](todo/t0501.md) — Runtime-side panic locations (~50+ sites in src/backend/c/c_runtime.rs). Compiler-side emit produces file:line:col: for…


- [`t0502`](todo/t0502.md) — Stdlib narrow waist — Phase 2c residual. Vector.each/for_each void-return entries retired (user-space lib/std/iter.gg wr…

- [`t0503`](todo/t0503.md) — C backend: retire local val_types/ptr_pointee fixup phases (after the 2026-05-15 seed migration). Seeds val_types/ptr_po…

- [`t0504`](todo/t0504.md) — Decompose emit_call_extern.rs (~908 lines). Tier 1-3 + HOF (→emit_hof.rs) + printf (→emit_printf.rs) lifted. Remaining g…

- [`t0505`](todo/t0505.md) — Phase A's resources build-tooling — the cross-language single source of truth (docs/devbook/18-runtime-abi.md + 26-self-…

- [`t0506`](todo/t0506.md) **MED** — 🆕🐛 [MED — generic-indirection escape, from the RV-A Fable review 2026-07-16] Box[T].x / Guard[T].x through a generic fn…

- [`t0507`](todo/t0507.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, then C BUILD FAILS at the callee body; measured 2026-07-27 in the Family-1 Track A guard-fam…

- [`t0508`](todo/t0508.md) **MED** — 🆕🐛 [MED — Core #10 SILENT WRITE-DROP, gg check ACCEPTS, ALL THREE write faces; found 2026-07-27 in the Family-1 round's…

- [`t0509`](todo/t0509.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, then NEITHER BACKEND PRODUCES A BINARY; measured 2026-07-27] A Guard[T] in any AGGREGATE pos…

- [`t0510`](todo/t0510.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, then NEITHER BACKEND LINKS; measured 2026-07-27] RETURNING a Guard[T] calls gorget_guard_clo…

- [`t0511`](todo/t0511.md) **MED** — 🆕🐛 [MED — SILENT NAME COLLISION between user code and a builtin wrapper, gg check ACCEPTS; measured 2026-07-27] A USER s…

- [`t0512`](todo/t0512.md) **MED** — 🆕🐛 [MED — parser disambiguator, from the D29 scout 2026-07-16] fs[i](10) where fs: Vector[int(int)] fails error[E_NotAFu…

- [`t0513`](todo/t0513.md) — 🐛 [unwrap fix scout 2026-07-06, MEDIUM] expect("custom msg")'s message argument is DROPPED at lowering — methods.rs:975…

- [`t0514`](todo/t0514.md) — 🧹 [unwrap fix scout 2026-07-06, LOW] Fossil: the unwrap_error abort() combinator paths in emit_hof.rs:156/emit_types.rs/…


- [`t0515`](todo/t0515.md) — 🐛 LLVM backend: --sanitize is SILENTLY DROPPED — an "LLVM ASan" run is VACUOUS. compile_llvm_pipeline (src/main.rs:1177)…

- [`t0516`](todo/t0516.md) — 🐛 LLVM backend: in-loop sret/temp allocas emitted PER ITERATION → a print-heavy loop stack-exhausts (SIGSEGV) at ~399k b…

- [`t0517`](todo/t0517.md) — 🐛 ASan: residual Rust-gg leak cohort AFTER the print-temp leak-class fix (re-measure via gg build --sanitize + ASAN_OPTI…


- [`t0519`](todo/t0519.md) — 🐛 Print-temp site-C ungated sub-shape: a Clone-of-NON-string-AGGREGATE f-string interp temp stays UNREGISTERED (delibera…

- [`t0520`](todo/t0520.md) — 🐛 ASan: gorget_js_snag_3_match_struct_literal_use stack-buffer-overflow in __interceptor_memcpy (pre-existing, IDENTICAL…

- [`t0521`](todo/t0521.md) — 🐛 LATENT DCE gap: has_side_effects (src/lir/optimize.rs:576) omits Inst::CallRuntime (lists CallExtern/Call/CallPtr/Call…

- [`t0522`](todo/t0522.md) — Remove name-matching from consuming-position lowering — add an EXPLICIT per-method consuming-param signal (hardened by r…

- [`t0523`](todo/t0523.md) — panic as builtin — option (a) follow-on: retire the hardcoded gorget_panic lowering at assert. Option (b) shipped (panic…

- [`t0524`](todo/t0524.md) — Drop elaboration — remaining cleanup: (1) 24 Memsets across 17 fixtures (IndexLoad element zeroing + projected Deref/Fie…

- [`t0525`](todo/t0525.md) — 🧹 [unwrap output-review 2026-07-06, LOW] emit_name.contains("option") message-word pick at the no-StructId unwrap fallba…

- [`t0526`](todo/t0526.md) — 🐛 (G1 follow-up, PRE-EXISTING) N2 — closure-return-String 2-byte/call leak. Res r=Res("A"); auto cb=(): r.name; print(cb…
#### [CLASS FIX — layering, from Increment-2 scout + Round-8 Track A] typed is_borrow_ptr flag on GirFieldInfo
Multiple sites reconstruct "is this field a borrow pointer" from the field-type-NAME prefix `Ref__`/`MutRef__` (core-invariant-#2 smell): `lir_lower.gg:4360` GIFieldLoad dispatch (Round-8 Track A added the `MutRef__` read), `resolve_field_lir_type:708`, `resolve_field_gir_type:942`, closure Increment-2's ByMutRef env field (which must be NAMED `MutRef__<T>` to take the load-stored-pointer path), and now (2026-07-27 Track D advisory A2 sibling) `field_storage_holds_pointer` in `lir_lower.gg` — hoisted into a helper by Track D as a strict improvement over the open-coded prefix test, but the underlying `starts_with("Ref__"/"MutRef__")` shape remains and is a member of THIS census, not a new class. All reuse the documented accepted residual because `GirFieldInfo` stores only a type-name String, not a typed id. CLEAN CLASS FIX: add a typed `bool is_borrow_ptr` (or a typed pointee-id) to `GirFieldInfo` (gir.gg), set it at field registration, and read it at all the above sites instead of the name prefix — retiring the whole residual class. Mirrors Rust's typed `field_is_ptr` (insts.rs:1029). Medium track (gir.gg + ~5 consumers + careful review); do AFTER the closure increments that depend on the current residual behavior have landed (so the cleanup is a pure no-op refactor). This subsumes the earlier "MutRef__ symmetry" nit. **🆕 (2026-07-27, filed by Track B1 A-2 output-review) — Rust-side sibling on TYPE-NAME rather than field-name: `callable_local_return_type` + `callable_local_param_types` (`src/ir/lowering/stmts/mod.rs:96/151`) recognise callable ast types by matching the outer name against `"Callable" | "MutCallable" | "ConsumeCallable"` — a name-string test at a semantic decision (Layering rule 2 debt, same class as the field-name-prefix reconstruction above). The three names are the built-in callable-family types; a user `struct MutCallable[T]` or a future callable variant would silently mis-route through this helper. Reference-grade fix: a typed `is_callable_family` (or `callable_kind: Option<CallableKind>`) flag on the resolved type, seeded once at registration (as `guard_inner_suffix`'s planned funnel does for the guard family). Track B1's brief for a `tests/lints.rs` name-list ratchet on these callable name-match sites was noted but never landed — file it in the same ratchet when the typed-flag lands.

- [`t0527`](todo/t0527.md) **MED** — 🆕 [MEDIUM — typed-metadata smell, from the R-C scout 2026-07-10] The LIR extern decl hardcodes an i64 return for the mon…
- [`t0689`](todo/t0689.md) **MED** — 🆕🐛 [MED — Box[Optional[T]] fails to BUILD on BOTH lanes; found 2026-08-27 by R45 Track A brief-review pass 7, orchestrat…
- [`t0690`](todo/t0690.md) **MED** — 🆕🧹 [MED — Layering rule 2 census owed: 8 starts_with("Box__") sites at/below the LIR boundary, some of which read as ROU…
- [`t0727`](todo/t0727.md) **MED** — [MED — --sanitize --backend=llvm instruments the RUNTIME ONLY; generated user code carries no shadow checks. Found 2026-…
- [`t0728`](todo/t0728.md) **MED** — [MED — A NAME MATCH OUTRANKS THE USER'S TYPED DECLARATION: an extern bound to malloc/calloc/realloc has its declared ret…
- [`t0742`](todo/t0742.md) **MED** — 🆕🐛 [MED — BOTH LANES AGREE ON THE WRONG ANSWER, which is a red flag and not a pass (Core #8); found R47 Track D1 while m…
- [`t0743`](todo/t0743.md) **MED** — 🆕🐛 [MED — BOTH LANES AGREE ON THE WRONG ANSWER (Core #8 red flag); found R47 Track D1 while building the ACCEPT control…
- [`t0876`](todo/t0876.md) **MED** — 🆕🧹 [MED — THE EMITTED C IS NOT REPRODUCIBLE: the SAME compiler binary on the SAME input emits two different files run to…
- [`t0902`](todo/t0902.md) **MED** — 🆕🐛 [MED — C BACKEND ONLY, LLVM IS CORRECT (Core #9 lane divergence); gg check rc 0, gg build fails at cc; found 2026-08-…
- [`t0968`](todo/t0968.md) **MED** — 🆕🐛 [MED — COMPILER ICE (panic, not a diagnostic) on ordinary safe syntax; both backends; found 2026-09-03 by R49 Track A…
- [`t0971`](todo/t0971.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, both backends, gg check clean and the printed value CORRECT; found 2026-09-0…
- [`t0972`](todo/t0972.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, both backends, gg check clean and the printed value CORRECT; found 2026-09-0…
- [`t0991`](todo/t0991.md) **MED** — 🆕🐛 [MED — C-BACKEND-ONLY BUILD FAILURE on ordinary code, gg check CLEAN, and a LANE DIVERGENCE:
- [`t1052`](todo/t1052.md) **MED** — 🆕🐛 [MED — a compile-time failure on ordinary safe syntax, BOTH backends, gg check clean; found 2026-09-03 by the R49 Tra…
- [`t1054`](todo/t1054.md) **MED** — 🆕🧹 [MED — a live Layering-rule-2 site with ~50 arms, IDENTICAL IN SHAPE to the R49 A1-IDENTITY miscompile, and the sourc…
- [`t1116`](todo/t1116.md) **MED** — 🆕🧹 [MED — a blanket fallback that converts "the lowering emitted a call nobody defines" into a silently-linked binary. F…
- [`t1079`](todo/t1079.md) **MED** — 📐 [MED — Layering rule 3 (one source of truth per axis): the Gorget-type → C-name-fragment axis has TWO parallel primiti…
- [`t1215`](todo/t1215.md) **MED** — 🆕🐛 [MED — a HOF result array minted with no element metadata on the LLVM lane, and the SAME program not linking at all o…
- [`t1226`](todo/t1226.md) **MED** — 🆕🐛💥 [MED — check-accepts + BACKEND ICE on the SANCTIONED REMEDY; found 2026-09-04 by R49 Track S-a2's executor while bui…
- [`t1439`](todo/t1439.md) **MED** — 🆕 [MED — A LINK FAILURE WHOSE DISCRIMINATOR IS UNKNOWN, FILED WITH ITS OWN COUNTER-EXAMPLE. Found 2026-09-05 off-track b…
### Low

- [`t0528`](todo/t0528.md) — 🔭 (low-pri, const-bytes #10) — scalar-FLOAT static const-init uses %g (float_to_str), not bit-exact. scalar_static_c_lit…

- [`t1117`](todo/t1117.md) **LOW** — 🆕🧹 [LOW — a sibling-site gap the A2-α change made VISIBLE rather than created. Found 2026-09-04 by R49 Track A2-α] The t…
- [`t1235`](todo/t1235.md) **LOW** — 🆕🧹 [LOW — the RESIDUAL of the class R49 Track A2-α retired, and the reason A2-α's claim is scoped rather than absolute.…
- [`t1218`](todo/t1218.md) **LOW** — 🆕🧹 [LOW — a SECOND, WEAKER writer on the element-metadata axis (Layering rule 3); found 2026-09-04 by R49 Track N2 while…
- [`t1334`](todo/t1334.md) **LOW** — 🆕🧹 [LOW — LAYERING RESIDUAL, no known user-visible misbehaviour; surfaced 2026-09-05 by the R50 Track C2 brief-review ga…
- [`t1351`](todo/t1351.md) **LOW** — 🆕🔧 [LOW — LATENT, NOT LIVE. A structural hazard in a sibling of the ledger R50 Track E deliberately did not build. Found…
### Rust gg bugs surfaced by R39 Phase 2e Sub-task 0 probe (2026-08-09)

The 4 bugs below blocked R39's owner-chosen Option C helper design
(`Vector[T] parse_comma_separated_list[T](Parser &, Token, Callable[T(Parser &)])`)
and forced the shipped Path 3 fallback (single boolean `consume_comma_or_tok`
helper, no generics, no Callable). Each has a durable `known_gaps/rust_gg_bug_*/`
repro + `#[ignore]`d integration test asserting the INTENDED behavior. Un-ignore
each test as the underlying bug fixes; graduate the fixture out of `known_gaps/`
when it passes cleanly (per Task Continuity).

- [`t0529`](todo/t0529.md) **HIGH** — 🆕🐛 [HIGH — Rust gg, closure-inference gap] Callable[T(Struct &)] + closure literal (p): p.field — Rust gg infers p as in…


- [`t0530`](todo/t0530.md) **MED** — 🆕🐛 [MED — Rust gg, closure struct capture wrong-code] Closure capturing a local struct's &self-method mutations does NOT…

- [`t0531`](todo/t0531.md) **MED** — 🆕🐛 [MED — Rust gg, scale-dependent monomorphization] Generic user-fn monomorphization SKIPPED at parser.gg scale — decla…

- [`t0532`](todo/t0532.md) **HIGH** — 🆕🚨 [HIGH — Core #8 check-clean MISCOMPILE, rc 0 and GARBAGE OUTPUT on the C backend; filed 2026-08-23 by R44 Track E, gr…
- [`t0533`](todo/t0533.md) **MED** — 🆕🐛 [MED — check-clean, raw generated-C error leaks to the user; filed 2026-08-23 by R44 Track E, grep-before-file done]…

- [`t0938`](todo/t0938.md) **MED** — 🆕🐛 [MED — COMPILER ICE (panic, not a diagnostic) on syntax the language ratifies] Some(^h) where h is a Callable local p…
- [`t0939`](todo/t0939.md) **MED** — 🆕🐛 [MED — Core #10 SILENT DROP: gg check ACCEPTS, the build dies at LINK] Calling a Callable-typed struct FIELD dies wit…
- [`t0942`](todo/t0942.md) **HIGH** — 🆕🐛 [HIGH — the THREE Callable cells that t0936's fix CANNOT reach, filed so they do not end the round as unrecorded RED]…
## Perf / clone-pressure / compile-time

### High

- [`t0534`](todo/t0534.md) **HIGH** — ⭐🆕 [HIGH — DENSE INDEX-MAP LAYOUT for Dict/Set; design OWNER-RATIFIED 2026-08-03 as D39 (docs/define-gorget/decisions.md…
- [`t0535`](todo/t0535.md) **MED** — ⭐🆕 [MED — D39 PHASE B of 3: extract StableMap/StableSet into lib/std/stablemap.gg.] Preserve today's tombstone layout as…
- [`t0536`](todo/t0536.md) **MED** — ⭐🆕 [MED — D39 PHASE C of 3: the .nth(i) / .key_at(i) ordinal accessors.] Small and additive, and the phase that actually…
- [`t0537`](todo/t0537.md) **MED** — 🆕 [MED — SCC residual of Round X's Track I, MEASURED at HEAD 2026-07-28] W_RecursiveBareParamMaterialize covers DIRECT s…
- [`t0538`](todo/t0538.md) **HIGH** — ⭐ [HIGH — CoW COST CONTRACT campaign; design note docs/internals/cow-cost-contract.md, knob spelling OWNER-CHOSEN 2026-0…
- [`t0539`](todo/t0539.md) **LOW** — [LOW — doc bug, found 2026-07-28] docs/language-reference.md:595 advertises @inline as an example attribute; it is REJEC…

- [`t0540`](todo/t0540.md) **HIGH** — [HIGH — PROVISIONAL re-pin, do not forget — lag-close Wave 2 2026-07-21] Stage-0/1 clone ceilings raised; charter is han…

- [`t0541`](todo/t0541.md) — 🎯 ROUND-33 SHAPE (owner-directed 2026-07-02): 3 DEEP multi-session tracks + 1 ROLLING follow-ups slot (refill from the r…

- [`t0542`](todo/t0542.md) — 🆕⚡ [OWNER QUESTION 2026-07-13] ECS / data-oriented compilation-speed — PROFILE FIRST, don't guess. Can ECS ideas speed u…
#### 🆕 ROUND-2 MEASUREMENT-SLICE FILINGS (2026-07-19; the attribution + stage-bench scripts + string-clone ratchet are the round's landables — these are the quantified targets and follow-ups the instruments exposed)
- [`t0543`](todo/t0543.md) **HIGH** — ⚖️ SEQUENCING RULED BY THE OWNER 2026-08-27 (recorded here, NOT in the decisions ledger — it changes no semantics and so…
- [`t0544`](todo/t0544.md) **HIGH** — ✅ RULED 2026-08-30 — D52: *"materializes unless provably free"*. #13 DOES cover binds; CoW Rule 3 is AMENDED. The open q…
- [`t0545`](todo/t0545.md) **MED** — [MED — targeted subset of the above, ~74% ALREADY CONSUMED by the Class-C round 2026-07-19] CoWMaterialization residual:…
- [`t0546`](todo/t0546.md) **MED** — [MED — instrument v2] Mint CloneIds at the LIR layer — the documented un-attributed residual (devbook/11:~1055); today t…
- [`t0547`](todo/t0547.md) **LOW** — [LOW — meter hardening] Ratchet-ify bench_stages.sh: per-stage wall/RSS as a tracked meter is landed as a SCRIPT; a ratc…
- [`t0548`](todo/t0548.md) **LOW** — [LOW — benchmark-informed knob] The bootstrap stage binaries are O0-compiled and dominate the chain (~260s each for S1→2…
### Medium

#### 🆕 OWNER-APPROVED SUITE-SPEED OPTIMIZATIONS (filed 2026-07-20; scout NEXT, not mid-burn-down)
- [`t0549`](todo/t0549.md) **MED** — [MED — scout] Shared self-compile artifacts across the heavy self-host tests. ~7 full driver self-compiles today (self_h…
- [`t0550`](todo/t0550.md) **MED** — [MED — scout] --clones=stats unsupported under --backend=llvm → the LLVM lane's clone behavior is UNMEASURED by the ceil…

- [`t0551`](todo/t0551.md) — ⚡ PERF — self-compile clone-elision NEAR-EXHAUSTED; route the perf slot to PARITY (re-measured 2026-06-14, scout ae21798…

- [`t0552`](todo/t0552.md) — ⏱ SWEEP WALL-TIME (owner raised "300s→859s, overflow checks?" 2026-06-23) — REGRESSION PREMISE REFUTED by bisect (scout…
#### 🆕 THE WASTED-CLONE DETECTOR (owner-proposed 2026-07-19; the 4th instrument — MED, own scout; lands BEFORE/WITH the SH bare-arg CoW fix as its meter)
- [`t0553`](todo/t0553.md) — Dynamic dead-clone detection: count clones where NEITHER source nor copy is written afterward — the clone was provably u…

#### 🆕 PLANNER CONSUMER-#1 ROUND FILINGS (2026-07-19; the branch/scope class + hotfix are LANDED both lanes — these are the residuals the round exposed)
- [`t0554`](todo/t0554.md) **MED** — [MED — Core #6 guard, output-review finding] ONE table-equivalence lint for the lane-symmetry mutability mirror. The mut…
- [`t0555`](todo/t0555.md) **MED** — [MED — both lanes, scan-invisibility sibling] The SMeta* family (SMetaFor/SMetaIf/SMetaMatch/SMetaWhile, runtime stmt bo…
- [`t0557`](todo/t0557.md) **LOW** — [LOW — Core #10 sibling, Rust] lower_stmt's Select dispatch DISCARDS else_arm (Stmt::Select { arms, else_arm: _ }, stmts…
- [`t0558`](todo/t0558.md) **LOW** — [LOW — perf, benchmark-round input] builtin_method_mutates returns Some(d.clone()) (deep clone incl. a Vector) per class…
- [`t0559`](todo/t0559.md) — [NOTE — timing expectation] self_host_bootstrap_fixed_point wall is now ~867s solo (vs ~594s guards-close era) — legitim…
- [`t0560`](todo/t0560.md) — [PENDING targets exposed by the stage-1 clone ceiling self_host_stage1_clone_ceiling (tests/integration.rs; landed recor…
- [`t0561`](todo/t0561.md) **LOW** — [LOW] Per-reason clone budgets. The scripts/clone_attribution.sh CloneId×reason join makes per-reason ceilings (e.g. Var…

- [`t0562`](todo/t0562.md) — ⚡ PERF — systemic ~400 TYPE x = coll.get(i).unwrap() clone-binds (157 lower.gg, 122 lir_codegen.gg, 46 lir_lower.gg) clo…

- [`t0563`](todo/t0563.md) — [Planner round 3 → next conversion] Pick the next at-site CLASS deliberately. Class A landed (ratchet 20→14). Class C RE…

- [`t0564`](todo/t0564.md) — [G3 → planner] Thread MaterializeReason GIR→LIR + fold the CloneId onto the carrier. The reason is GIR-only today (dropp…

- [`t0565`](todo/t0565.md) — #13 borrow-default STEP 2 (field-path RSS) — cheap path REFUTED (leaf-borrow probe KILL, historical). Attribution scout…

- [`t0566`](todo/t0566.md) — ⚡ PERF (owner-flagged, NOT URGENT) — close the CPython gap on the 2 benchmark cases where Gorget LOSES. (docs/profiling/…

- [`t0567`](todo/t0567.md) **MED** — [MEDIUM] 📊 Macro-benchmark suite (the "very fast" pillar's missing evidence). docs/profiling/cross-lang-bench-2026-06-01…
- [`t0850`](todo/t0850.md) **MED** — [MED — R47 Track F5a, measured] parent_dir costs ONE heap String per CHARACTER of the path, and
- [`t1051`](todo/t1051.md) **MED** — [MED — THE REFERENCE LAGS THE SELF-HOST ON COST (not on soundness); measured 2026-09-03 by R49 Track K while porting the…
### Low

- [`t0568`](todo/t0568.md) — ⚡ PERF (self-host) print(f"…") materializes a gorget_string_format temp per call where Rust direct-splices into printf.…

- [`t0569`](todo/t0569.md) — c_emit parity nit: dataframe_tier2_joins self-host OVER-clones Rust by ~5 (~34 vs 29). The OpBorrow-as-liveness-use fix…

- [`t0570`](todo/t0570.md) — 🔵 BENCHMARK COVERAGE GAP: the bench corpus does NOT exercise #37 lazy CoW. A clean A/B (eager vs GG_COW_LAZY_LOOP=1 at 0…

- [`t1070`](todo/t1070.md) **LOW** — 🆕🧹 [LOW — EXCESS CLONE, safe not unsound; found R49 Track L pass-4 review, measured 2026-09-04] A closure capture whose…
## Guards / lints / test-infra

- [`t0571`](todo/t0571.md) **MED** — 🆕🧹 [MED — A PROCESS TRAP THAT SHIPPED A DEFECT THIS ROUND; found 2026-08-24 by R44 Track A, on itself] git mv + "stage e…

- [`t0572`](todo/t0572.md) **HIGH** — 🐛 [HIGH — re-measure, never quote: scripts/sanitize_sweep.sh (~25 min for the builds, ~4.4 min per extra repetition)] FO…

- [`t0573`](todo/t0573.md) **MED** — 🆕🐛 [MED — Layering rule 2 / "No name matching" INSIDE A LINT, and it cost a mid-gate panic; found 2026-08-24 by R44 Trac…

- [`t0574`](todo/t0574.md) **MED** — 🆕🐛 [MED — a citation guard that structurally cannot see the failure it exists for (Core #15e Q2); found 2026-08-24 by R4…

- [`t0576`](todo/t0576.md) **MED** — 🆕🧹 [MED — Core #4 PARTIAL EXTRACTION; filed 2026-08-23 by R44 Track G, which created the helper and converted only its o…
- [`t0577`](todo/t0577.md) **HIGH** — 🆕🐛 [HIGH — A GATE THAT NEVER RUNS AT ROUND CLOSE; found 2026-08-22 measuring R43] The self-host runtime PARITY CEILING (…
- [`t0578`](todo/t0578.md) **MED** — 🆕🐛 [MED — fmt width, filed by R42 Track C 2026-08-15] Stmt::VarDecl's INITIALIZER has no Doc layer, so a wide declaratio…
- [`t0579`](todo/t0579.md) **MED** — 🆕🐛 [MED — fmt width, filed by R42 Track C 2026-08-15] format_pattern has NO Doc layer — pattern wrapping is an unimpleme…
- [`t0580`](todo/t0580.md) **MED** — 🆕🐛 [MED — fmt width, found by R42 Track C's discovery sweep 2026-08-15] Enum TUPLE-VARIANT field lists have no Doc layer…
- [`t0581`](todo/t0581.md) **MED** — 🆕🐛 [MED — fmt width, filed by R42 Track C 2026-08-15] A Doc::Group-clothed carrier's FIRST piece is pre-rendered for the…

- [`t0582`](todo/t0582.md) **MED** — 🆕🐛 [MED — Core #13 class, filed from the T-RB0 output-review 2026-08-11] FIVE *_comparison tests remain FLOORLESS always…
- [`t0583`](todo/t0583.md) **MED** — 🆕🐛 [MED — SH runtime lane, filed from the T-RB0 output-review 2026-08-11] The INDEXED-CALLEE callable shape TIMES OUT on…

- [`t0714`](todo/t0714.md) **MED** — MED — a heading inventory does not pin clauses. That is the design. AGENTS.md is held by a size ceiling and a heading-id…
### Medium
- [`t0584`](todo/t0584.md) **MED** — 🐛 [MED — fuzz corpus coverage + scratch leak] fuzz_roundtrip gates its assert on parser.errors.is_empty(), so once D27 R…

### High
- [`t0585`](todo/t0585.md) **HIGH** — 🆕🐛 [HIGH — Core #6 guard, MEASURED 2026-08-05: the predicted drift HAPPENED and GREW 7×] REGISTRY (Rust, 306) and RUNTIM…

- [`t0586`](todo/t0586.md) **MED** — 🧹 [MED — burn no_growth_in_phase_d_proxy_reads back DOWN; the gate itself is GREEN] Migrate the migratable proxy reads t…

- [`t0587`](todo/t0587.md) — 🆕🧹 [Core #6 guard — CI-triage 2026-07-23] The ggdef corpus gates drift RED whenever a round adds an out-of-subset cow_*…
- [`t0588`](todo/t0588.md) **LOW** — 🆕⚖ [LOW — adjudication coverage] Promote the retborrow_* boundary fixtures into the ggdef adjudication corpus. They are…

- [`t0589`](todo/t0589.md) — 🆕🐛 [macOS dev-box false RED] security_safe_no_leak mis-reports on any platform without LeakSanitizer. The 5 cow_bareassi…

- [`t0695`](todo/t0695.md) **HIGH** — 🆕🚨 [HIGH — THE ROBUSTNESS MAP MEASURES 27 BUILD-FAIL CELLS AND NOT ONE IS TRACKED AS WORK; found 2026-08-27 while re-der…
- [`t0870`](todo/t0870.md) **HIGH** — \U0001F195\U0001F6A8 [HIGH — THE PARITY GATE CANNOT CATCH ITS OWN CLASS (six-questions #2); found 2026-08-31 by the R48…
- [`t0875`](todo/t0875.md) **HIGH** — 🆕🚧 [HIGH — Core #6 OWES A CLASS-RETIRING GUARD AND FOUR ATTEMPTS FAILED; measured 2026-08-31 across four R48 Track-D1 re…
- [`t0925`](todo/t0925.md) **HIGH** — 🆕🚨 [HIGH — THE ROUND-CLOSE SWEEP CAN PASS HAVING RUN ZERO TESTS, AND EXIT 0; observed 2026-08-31 by the R48 Track-D2 exe…
- [`t0924`](todo/t0924.md) **HIGH** — 🆕🚧 [HIGH — A GATE THAT NO-OPS IN THE PROFILE EXECUTORS RUN, demonstrated by a live breach it let through 2026-08-31] RUN…
- [`t0993`](todo/t0993.md) **HIGH** — 🆕🔬 [HIGH — THE DRIFT CENSUS. Stage 2 of a three-stage ratchet (Core #6, devbook/25): report → burn
- [`t1360`](todo/t1360.md) **HIGH** — 🆕🛡 [HIGH -- A GUARD THAT CANNOT CATCH ITS OWN CLASS (SIX-QUESTIONS #2): THE LEAK SWEEP TREATS did not run AS did not lea…
- [`t1406`](todo/t1406.md) **HIGH** — 🆕🚨 [HIGH — A GUARD THAT GREEN-LIGHTS ITS OWN CLASS (SIX-Q #2); found 2026-09-05 by R50 Track G's output-review, verified…
- [`t1419`](todo/t1419.md) **HIGH** — 🆕🚨 [HIGH — A GUARD-BLINDING HAZARD IN THE RUNTIME: a static scratch buffer SILENTLY SUPPRESSES LEAK REPORTS; found 2026-…
- [`t1409`](todo/t1409.md) **MED** — 🆕 [MED — A GUARD THAT RECORDS A LIVE BUG AS EXPECTED. Found by R50 Track H while landing t1387's fix; the pinning pre-da…
- [`t1378`](todo/t1378.md) **HIGH** — 🆕🚨 [HIGH — A TIER-2A GUARD CLASS THAT HAS NEVER FIRED AND CANNOT FIRE, AND IT IS THE ROOT OF THE "STRUCTURALLY BLIND VAL…
- [`t1452`](todo/t1452.md) **HIGH** — 🆕🚨 [HIGH — A RECONCILIATION LINT WHOSE EXEMPTION REINTRODUCES THE HOLE IT EXISTS TO CLOSE. Found 2026-09-05 by the R50 o…
### Medium
- [`t0590`](todo/t0590.md) **LOW** — 🆕🧹 [LOW — lint ergonomics; flagged by the R42 Track-B executor 2026-08-15 (bit three times in one track), filed by the o…

- [`t0591`](todo/t0591.md) **MED** — 🆕🐛 [MED — the durable-repro rule broken in the LEDGER itself; found 2026-08-05] docs/define-gorget/decisions.md:1429 cit…

- [`t0592`](todo/t0592.md) **MED** — 🆕🧹 [MEDIUM — Core #4 guard, from CallArg output-review 2026-07-14] Add a self-host regression fixture that compiles v.ma…

#### 🆕 STAGE-1 CLOSE FILINGS (2026-07-20)
- [`t0593`](todo/t0593.md) **MED** — [MED — harness race, concrete incident] c_emit_comparison under 4-thread sweep load can read TORN shared-driver state →…

#### 🆕 RECOVERY-REVIEW FILINGS (2026-07-19, from the stage-1-recovery output-review's adversarial probes)
- [`t0595`](todo/t0595.md) **LOW** — [LOW] is_elem_borrow_read accessor precision: doc claims equivalence to the ret_option_ref_or_val_* decls but the name-s…
- [`t0596`](todo/t0596.md) **LOW** — [LOW — Core #4] Unify the sibling hand-spelled name lists onto the new accessor: helpers.rs:534/539 (find_collection_sou…

- [`t0597`](todo/t0597.md) — 🛡 map_binop unknown-op→OP_ADD footgun (forward-guard for future fallible arithmetic operators; filed 2026-06-22, REPOINT…

- [`t0598`](todo/t0598.md) — 🛡 STRUCTURAL-GUARD GAP (filed alongside the .mod() LLVM INT_MIN/-1→0 + div0 fix, see DONE.md): no arm-count lint forces…

- [`t0599`](todo/t0599.md) — 🛡 STRUCTURAL GUARD — producer-side leak validator (design grounded; NEEDS the ≥3-pass plan-review before execute). Conve…

- [`t0600`](todo/t0600.md) — (systemic smell) lower_fail (lower.gg ~:352) SILENTLY drops unhandled constructs to a stdout comment + continues → wrong…

- [`t0601`](todo/t0601.md) — 🔒 [P1-G follow-up] Full-code-list pin test for the diagnostic registry. Add a test pinning ALL SemanticErrorKind::code()…

- [`t0602`](todo/t0602.md) — ⚙ ASan-battery structural guard (CLAUDE.md #6) — convert the manual leak sweep into an executable ratchet. Build a tests…


#### [round-8 follow-up] Snapshot lock-in catch-up — 35 unprotected stable matches **⚠ COUNT STALE — RE-MEASURED 2026-07-17 (the Root-A landing's snapshot regen, STOP-AND-REPORT surprise): the regen found 1171 stable matches vs 979 committed snapshots = ~188 unprotected stable fixtures (and 0 drifted). The under-seeding grew ~5x since this entry's 35. Own net-reseed track with reference-grade triage per this entry's procedure; the Root-A landing seeded only its 5 per its brief's discipline.**
Track-B executor (uint8 fix) found that a full `GG_REGEN_RUNTIME_SNAPSHOT=1` regen yields ~38 new stable-match `.out` files (765→803); ~35 are pre-existing drift = fixtures already MATCHing on gorget-1 but NOT in the `self_host_runtime` lock-in net (so they can silently regress). They are already counted in the `self_host_runtime_diff` parity number; this is REGRESSION-PROTECTION hygiene, not a parity increase.
ACTION (after the round's snapshot-adding tracks integrate so the set is stable): run a triage that, for each of the ~35, confirms the output is genuinely CORRECT (reference-grade — not merely self==rust "both-wrong"; CLAUDE.md core invariant #8) BEFORE committing its snapshot. Lock in the verified-correct ones; for any "stable but wrong", file the real defect instead of snapshotting it. Zone: `tests/fixtures/runtime_snapshots/*.out` only.
Re-derive the list: `GG_REGEN_RUNTIME_SNAPSHOT=1 cargo test --test integration --release self_host_runtime` then `git status --short tests/fixtures/runtime_snapshots/` (REVERT the regen after capturing the list — do not commit a blind full regen).

- [`t0603`](todo/t0603.md) **MED** — 🔧 [MED — capped-drain landing follow-ups, filed by the Fable delta-review 2026-07-16 (Core #4 siblings of the LANDED 1a7…
- [`t0604`](todo/t0604.md) **MED** — [MEDIUM] 🔔 DeadBareParamWrite lint FOLLOW-UPS (v1 LANDED 2026-07-05, see DONE.md — the follow-ups below are the remainin…
- [`t0605`](todo/t0605.md) **MED** — [MEDIUM] 🎲 gorget-smith FOLLOW-UPS (tier-0 LANDED 2026-07-05, see DONE.md; round 1 found 2 HIGH bugs — entries above). R…
- [`t0675`](todo/t0675.md) **MED** — 🆕🧹 [MED — ratified 2026-08-23 as one of the two lints the todo/ split was supposed to make possible; filed 2026-08-27 by…
- [`t0676`](todo/t0676.md) **MED** — 🆕🧹 [MED — the SECOND of the two lints ratified 2026-08-23 with the todo/ split; filed 2026-08-27 by the split migration,…
- [`t0677`](todo/t0677.md) **LOW** — 🆕📋 [LOW — the one ratified todo/ FIELD the split migration could not populate; filed 2026-08-27] mechanism is EMPTY on a…
- [`t0678`](todo/t0678.md) **LOW** — 🆕📋 [LOW — the last unimplemented clause of the ratified todo/ split; filed 2026-08-27 by the migration that deliberately…
- [`t0679`](todo/t0679.md) **LOW** — 🆕🧹 [LOW — four dead TODO.md:<line> coordinates in COMPILER SOURCE; enumerated 2026-08-27 by the todo/ split, which was f…
- [`t0713`](todo/t0713.md) **MED** — 🆕🐛 [MED — THE ROUND-CLOSE C SWEEP RUNS ITS MOST EXPENSIVE TEST WITH BOTH GATES INERT; found 2026-08-28 by the D50 track,…
- [`t0726`](todo/t0726.md) **MED** — 🆕🧹 [MED — READINESS CHECKLIST rows should GRADUATE to executable guards and LEAVE the checklist (owner 2026-08-29). Defe…
- [`t0851`](todo/t0851.md) **MED** — [MED — RE-OPENED 2026-08-31 by R48 Track F's own output-review, after being closed the same
- [`t0820`](todo/t0820.md) **MED** — [MED — a fixture the tree already knows is wall-clock dependent sits in the byte-compared corpus, unexcluded] test_proce…
- [`t0821`](todo/t0821.md) **MED** — [MED — #[ignore] silences the unit test but NOT the auto-scanned parity corpus] vector_task_mixed_await_int is a known S…
- [`t0826`](todo/t0826.md) **MED** — [MED — Core #2 name-matching inside the census tool, and it makes --fast a lie] scripts/known_gaps_census.sh's is_sh_row…
- [`t0827`](todo/t0827.md) **MED** — [MED — two CI-wiring guards are satisfied by a COMMENTED-OUT step, and the tree already has the helper that fixes it] te…
- [`t0828`](todo/t0828.md) **MED** — [MED — 24 fixtures are OUT of the runtime-parity corpus with no established reason, and the reason they are out is that…
- [`t0829`](todo/t0829.md) **MED** — [MED — the BARE :NNNN citation form is invisible to BOTH doc-citation guards, and docs/devbook/27 is the tree's largest…
- [`t0863`](todo/t0863.md) — ⊕ R48 CLOSE (2026-09-03): FIVE MORE, and they share ONE signature — measured, again NOT accepted. python3 scripts/robust…
- [`t0874`](todo/t0874.md) **MED** — 🆕🐛 [MED — A LINT IS INTERMITTENT UNDER PARALLEL AGENTS, and its own doc comment asserts the opposite; suspected 2026-08-…
- [`t0910`](todo/t0910.md) **MED** — 🆕🧹 [MED — A STRUCTURAL CONSEQUENCE OF THE PARALLELISM THE OWNER ASKED FOR; measured 2026-08-31 at R48 integration] Six c…
- [`t0905`](todo/t0905.md) **MED** — 🆕🧹 [MED — guard hygiene, Core #6 ⊕; filed 2026-08-31 by R48 Track D2] Seven <= burn-down ratchets remain in tests/lints.…
- [`t0926`](todo/t0926.md) — [MED — R48 Track F, declared phase 2 of the figures DB] EACH BURN-DOWN TRACK ADOPTS scripts/figures.db FOR ITS OWN ROWS…
- [`t0934`](todo/t0934.md) **MED** — [MED — R48 Track E-B3 remaining robustness adjudication] After M4 (good from COL_EXPECTED.startswith("REJECTED"), never…
- [`t0935`](todo/t0935.md) **MED** — [MED — Core #4 sibling of the awk attribution fix] tests/lints.rs known_gaps_passing_allowlist_shrink_only enumerator (t…
- [`t0956`](todo/t0956.md) — 🆕⚙️ [MED — GUARD SCOPE, needs an OWNER RULING before any of it can land; filed 2026-09-03 by R48 Track T-a1 as the defer…
- [`t0958`](todo/t0958.md) **MED** — 🆕🧹 [MED — SIX QUESTIONS #2, a guard that cannot catch its own class ON THE LANE THE CLASS KEEPS RECURRING ON; SPLIT out…
- [`t0960`](todo/t0960.md) **MED** — 🆕🧹 [MED — a GUARD THAT LIES ABOUT WHICH THING IS BROKEN when its own tooling fails; isolated 2026-09-03 by R48 Track U's…
- [`t0964`](todo/t0964.md) **MED** — 🆕 [MEDIUM — DERIVED DATA WITH NO PROVENANCE; found 2026-09-03 by the R48 round-close C sweep] The 1377 runtime_snapshots…
- [`t1047`](todo/t1047.md) **MED** — [MED — CLASS-RETIRING GUARD, filed 2026-09-03; the class already fired once and cost a RED CI gate for three days] Nothi…
- [`t0997`](todo/t0997.md) **MED** — 🆕🧹 [MED — INSTRUMENT DEFECT, the residual set after R49 Track H fixed the two rows it was scoped to] Leak gaps pinned on…
- [`t1064`](todo/t1064.md) **MED** — 🚧 [MED — Core #6, A FILED ITEM'S EVIDENCE CAN BE A DANGLING PATH AND NOTHING NOTICES; found 2026-09-04 by the R49 Track…
- [`t1066`](todo/t1066.md) **MED** — 🆕🚨 [MED — A WHOLE-TREE GUARD GAP, PROVEN BY A LIVE ESCAPE: conflict markers were COMMITTED to TODO.md in R49 and survive…
- [`t1236`](todo/t1236.md) **MED** — 🆕🧹 [MED — Core #4 census owed. Inherited from t0389, which R49 Track A2-α closed; this is the clause that would otherwis…
- [`t1090`](todo/t1090.md) **MED** — [MED — the reference-grade instrument for the borrow-into-an-owning-destination class; filed 2026-09-04 by R49 Track N1,…
- [`t1255`](todo/t1255.md) **MED** — 🆕🧹 [MED — TODO.md HAS NO FINAL NEWLINE, so every >> append silently CORRUPTS the last pointer row; found 2026-09-04 by R…
- [`t1302`](todo/t1302.md) **MED** — 🆕🛡 [MED — A PIN REGENERATED FROM THE ARTIFACT IT PINS IS A MIRROR, NOT A GUARD; found 2026-09-04 by R49 Track INT's firs…
- [`t1304`](todo/t1304.md) **MED** — 🆕🧹 [MED — Core #4/#6: a wiring guard that covers one directory and not the one beside it. Found by hitting the INSTANCE…
- [`t1295`](todo/t1295.md) **MED** — 🆕🐛 [MED — a GUARD THAT ASSERTS MORE THAN IT MEASURED, caught on its first real firing; found 2026-09-04 by R49's sanitiz…
- [`t1335`](todo/t1335.md) **MED** — 🆕🧹 [MEDIUM — GUARD COVERAGE HOLE, no live defect; found 2026-09-05 by the R50 Track C2 output-review] The LIR view-calle…
- [`t1384`](todo/t1384.md) **MED** — 🆕🔬 [MED — WHAT TOPIC 30 DOES NOT COVER, WRITTEN DOWN SO THE FAMILY IS NOT DECLARED CLOSED. Filed BY the track that built…
- [`t1386`](todo/t1386.md) **MED** — 🆕🔬 [MED — A WRONG CELL THAT STARTS PRINTING A *DIFFERENT* WRONG VALUE IS INVISIBLE TO THE MAP, and topic 30 is 237 cells…
- [`t1430`](todo/t1430.md) **MED** — 🆕 [MED — ONE CELL IS 58% OF THE FIVE-LANE WALL CLOCK, AND THE CENSUS THAT EXISTS TO CATCH HANGS CANNOT SEE IT. Found by…
- [`t1431`](todo/t1431.md) **MED** — 🆕 [MED — A DEADLOCK CLASS THAT HAS NOW FIRED THREE TIMES IN ONE ROUND AND IS POLICED BY PROSE ONLY. Core #6: prose rots,…
- [`t1411`](todo/t1411.md) **MED** — 🆕 [MED — A GATE THAT CANNOT SEE HALF OF WHAT IT ENABLES (Core #13). Found by R50 Track H while filing t1410.] assert_gg_…
- [`t1433`](todo/t1433.md) **MED** — 🆕🛡 [MED — THE PRESCRIBED PROTECTION AGAINST A LOAD ARTEFACT DOES NOT EXIST, AND THE ARTEFACT IT WOULD PREVENT IS SELF-ER…
- [`t1441`](todo/t1441.md) **MED** — 🆕 [MED — A LOAD-SENSITIVE GUARD, WHICH IS A GUARD THAT REPORTS RED WITHOUT A DEFECT. Observed 2026-09-05 by the R50 orch…
- [`t1448`](todo/t1448.md) **MED** — 🆕 [MED — A FIXED /tmp FILENAME IN THE PATH OF A ROUND-CLOSE GATE. Observed 2026-09-05 by the R50 orchestrator with four…
- [`t1352`](todo/t1352.md) **MED** — 🆕🛡 [MED — A RATCHET THAT CAN BE SILENCED BY REWORDING A COMMENT, AND ITS OWN SIBLING ALREADY SAYS SO. Found 2026-09-05 b…
- [`t1449`](todo/t1449.md) **MED** — 🆕🛡 [MED — A GENERATOR WHOSE OWN EXIT CODE DESCRIBES THE STATE IT JUST REPLACED. Observed 2026-09-05 by the R50 orchestra…
- [`t1451`](todo/t1451.md) **MED** — 🆕🛡 [MED — THE ONLY DETECTOR FOR A WHOLE CLASS IS A 100-MINUTE SWEEP TEST. Measured 2026-09-05 by the R50 orchestrator; i…
- [`t1453`](todo/t1453.md) **MED** — 🆕🛡 [MED — A SHRINK-ONLY RATCHET THAT WENT UNSHRUNK FOR A WHOLE ROUND. Measured 2026-09-06 by the R50 round-close sanitiz…
### Low

- [`t0606`](todo/t0606.md) — 🧹 (G1 follow-up) lint-file-scope: widen g1_projected_materialize_sites_untrack files[] IF a projected-materialize cow_be…
- [`t0731`](todo/t0731.md) **LOW** — [LOW — the sanitize sweep covers ONE lane and ONE directory level; extending it to the LLVM lane is now possible for the…
- [`t0965`](todo/t0965.md) **LOW** — 🆕 [LOW — ADOPTION GAP, not a defect; identified 2026-09-03 by R48 Track T-a2 while closing its
- [`t1301`](todo/t1301.md) **LOW** — 🆕🛡 [LOW — A GUARD NARROWER THAN THE CLASS IT NAMES; found 2026-09-04 by R49 Track S-a2's fixup pass, which narrowed the…
- [`t1296`](todo/t1296.md) **LOW** — 🆕🐛 [LOW — a CLASS KEY that stopped discriminating, latent rather than live; found 2026-09-04 by R49's sanitize re-seed]…
- [`t1308`](todo/t1308.md) **LOW** — 🆕🧹 [LOW — A GENERATOR THAT CANNOT REACH ONE OF ITS OWN ERROR STATES; found 2026-09-05 by the orchestrator re-grading t13…
- [`t1420`](todo/t1420.md) **LOW** — 🆕 [LOW — HYGIENE. A REGISTRY ROW POINTING AT NOTHING, BACKING A PROTOCOL NO PROGRAM CAN NAME. Found by R50 Track J's sco…
## Concurrency

### High

- [`t0607`](todo/t0607.md) **HIGH** — 🆕🐛 [HIGH — MEMORY-UNSAFE, DOUBLE-FREE at scope exit, SELF-HOST LANE ONLY; filed 2026-07-29 by Round XIV ride-along scout…

- [`t0608`](todo/t0608.md) — 🆕 ROUND-33 shared-override follow-ups (filed at inc-1 landing per output-review): (a) self-host ESpawn raw-passthrough i…

- [`t0609`](todo/t0609.md) — 🆕 ROUND-39 T2 spawn/blocking follow-ups (filed at landing per brief; the +5 fixtures now MATCH, these are the residual s…

- [`t0610`](todo/t0610.md) — ROUND-16 LANDED (Phase-1 f8357b1f + close: Phase-2 await-splice 0a8ee959, Shared[T]-wrapper LIR prerequisite 1acac6ee, ?…

- [`t0611`](todo/t0611.md) — 🆕 ROUND-16 PHASE-2 FOLLOW-UPS (filed 2026-06-30 by the Phase-2 await-splice output-review aad66092; the splice itself LA…

- [`t0612`](todo/t0612.md) — ⭐ Shared[T] cluster — LANDED 1acac6ee (LIR wrappers + size-fix) + Inc-1 clone-refcount c8278bdf (+2) + Inc-2 get-return-…

- [`t0613`](todo/t0613.md) — 🆕 INLINE-CLOSURE-SPAWN path (the round-16 decomposition's bounded RUNNER-UP, road-not-taken when the owner chose the sha…

- [`t0614`](todo/t0614.md) — The two "Inc-B" / "Keystone" bullets below (round-15 keystone Channel-spawn + Inc-B RWLock, in DONE.md) are RETAINED ONL…

- [`t0615`](todo/t0615.md) — The PENDING item in this block is the NEW Core #8 register_param bug (2nd bullet); the keystone Slice-1+2 bullet below i…

- [`t0616`](todo/t0616.md) — 🆕 ROUND-11 INC-1 FOLLOW-UPS (async spawn/await cluster — remaining work after the Inc-1 base spawn/await wiring for name…

- [`t0617`](todo/t0617.md) — 🔑 KEYSTONE async/concurrency lever — spawn-OPAQUE-PARAM support (the deferred "Inc-2b", RE-PRIORITIZED to HIGH by the Ch…

- [`t0618`](todo/t0618.md) **HIGH** — 🆕🐛 [HIGH — self-host Defect-B lane gap, filed by the stage-1b TRACK-1 executor 2026-07-19 (Core #9 explicit-citation for…
- [`t0619`](todo/t0619.md) **HIGH** — 🆕🐛 [HIGH — pre-existing, filed by the stage-1b TRACK-1 executor 2026-07-19; the scout's confound, deliverable 5] gorget_…
- [`t0620`](todo/t0620.md) **MED** — 🆕🐛 [MED-HIGH — pre-existing, filed by the stage-1b TRACK-1 executor 2026-07-19] Weak[T].upgrade()-in-match leaks the upg…
- [`t0621`](todo/t0621.md) **MED** — 🆕🐛 [MED — pre-existing, filed by the stage-1b TRACK-1 executor 2026-07-19] Calling a ByValue-self method through a &/! r…

- [`t0622`](todo/t0622.md) — 🐛 NON-VOID ambiguous Task[T] collection = SILENT MISCOMPILE / garbage (scout abff0e7fa3afcea8a, MEASURED). A Vector[Task…

- [`t0623`](todo/t0623.md) — 🧵 SELF-HOST SYNC-PRIMITIVE remaining (the 4 originals + local-ctor + Guard-field-access are in DONE.md). NEW (non-blocki…

- [`t0624`](todo/t0624.md) — 🔶 DEEP TRACK (ENGAGED, scoped by scout a9d29513 2026-06-27) — the shared facade desugar + spawn-token-wrapper. ⚠ CONSOLI…
### Medium

- [`t0625`](todo/t0625.md) — 🐛 shared(atomic) AtomicInt LEAKS (pre-existing, ASan, scout abff0e7fa3afcea8a). gorget_atomic_int_new never freed at sco…

- [`t0626`](todo/t0626.md) — 🧹 consolidate the 3 sync-ctor handlers (one-source-of-truth, scout aba2f460). sync_constructor_runtime_name(fname) (lowe…

- [`t0627`](todo/t0627.md) — 🧹 CONCURRENCY FOLLOW-UPS — ORTHOGONAL items surfaced by the shared_with_check_then_act investigation (the CI-failure fix…
- [`t0628`](todo/t0628.md) — 🔭 FIDELITY (atomics/sync) — retire the Rust map_stdlib_name sync INSTANCE-method entries (DEFERRED, PROVEN UNSAFE; the s…

- [`t0732`](todo/t0732.md) **MED** — [MED — shared(atomic) int LEAKS 8 BYTES ON BOTH BACKENDS, and no gate in this tree can see it. Adjudicated 2026-08-29 by…
- [`t0822`](todo/t0822.md) **MED** — [MED — a documented suspension point is not one under scheduler=single] Under directive scheduler=single (the N:1 cooper…
### Low

- [`t0629`](todo/t0629.md) — Spawn captures don't check stale shared-derived: spawned closures can capture stale pre-await data.

## Tooling / CLI / formatter / LSP

#### 🆕 R41 PRE-A2 FMT-HARDENING WAVE (moved here 2026-08-10 from Self-host parity — these are tooling entries; the ledger/handover 'Tooling/CLI' pointers were correct, the filing location was not)

- [`t0630`](todo/t0630.md) **MED** — 🆕🐛 [MED — fmt form-changing round trip; found by the R42 Track-D brief-review pass 2, orchestrator-verified at HEAD 2026…

- [`t0631`](todo/t0631.md) **MED** — 🆕🐛 [MED — docs-vs-parser divergence; found by the R42 Track-D brief-review pass 9, orchestrator-verified 2026-08-15] The…

- [`t0632`](todo/t0632.md) **MED** — 🆕🐛 [MED — typecheck hole in the same postcondition family; found by the R42 Track-D brief-review pass 10, orchestrator-v…

- [`t0633`](todo/t0633.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG VALUES, both backends; surfaced by the R42 Track-B executor 2026-08-15, orchestrator-verified] a…

- [`t0634`](todo/t0634.md) **MED** — 🆕🐛 [MED — reserved-name leak, the FOURTH postcondition-family sibling; found by the R42 Track-D brief-review pass 11, or…
- [`t0635`](todo/t0635.md) **MED** — 🆕🧹 [MED — Layering rule 3, one source of truth; census taken by R41 T-FMT-A 2026-08-11] Rewire the 44 remaining name.nod…

### High


- [`t0636`](todo/t0636.md) **HIGH** — 🆕🚨 [HIGH — manifest error-swallowing + no format-evolution path; ALL FOUR CELLS MEASURED 2026-08-08 by direct gg check p…
- [`t0637`](todo/t0637.md) — 📋 [gorget-sheets snag #57 — tooling/UX, 2026-07-07] Import path model: file-relative, no package src/ root. Same-dir imp…
- [`t0638`](todo/t0638.md) **MED** — 🆕🐛 [MED — fmt interior-comment escape: the residual OUTSIDE the fill-emitter chokepoint] every FILL-emitted delimited li…

- [`t0900`](todo/t0900.md) **HIGH** — ⚠ THE FILED MECHANISM WAS WRONG, AND THE CORRECTION IS THE USEFUL PART. Re-measured 2026-08-30 at 05f72286. This item sa…
- [`t1146`](todo/t1146.md) **HIGH** — 🆕 [HIGH — a --yes mid-round deletes live executors; measured 2026-09-04 during R49] scripts/round_cleanup.sh has no live…
### Medium

- [`t0639`](todo/t0639.md) — 🆕 CI/TOOLING FOLLOW-UPS (filed 2026-06-30, from the test_result_advanced CI-red fix — see DONE):

- [`t0640`](todo/t0640.md) — 🏁 GG_IMPL ENDGAME TRACK (owner-chosen 2026-06-19, ACTIVE) — build-time compiler selector: GG_IMPL=selfhost|rust produces…
- [`t0641`](todo/t0641.md) **MED** — 🔧 EARLY-RETURNING BUILD SUB-PATHS SILENTLY DROP FLAGS — a CLASS, not one flag. try_build_ir has FIVE build sub-paths, an…

- [`t0642`](todo/t0642.md) — 🐛 GG_IMPL self-host exec is shell-based (system() → sh -c) — word-splits on spaces/shell-metacharacters in paths (filed…
- [`t0643`](todo/t0643.md) — LSP server. language-design.md lists it but nothing ships. The gorget-js agent flagged it as the single biggest DX gap (…

- [`t0644`](todo/t0644.md) **LOW** — [LOW — deferred tidy, discovered during D21 gg sim deletion 2026-07-24] regex = "1" in Cargo.toml:26's [dependencies] is…

- [`t0645`](todo/t0645.md) **LOW** — [LOW — honest coverage note, from D21 gg sim deletion 2026-07-24] No exact live successor for sim's type-agnostic Uninit…

#### Toolchain exit-code scheme follow-ups (filed 2026-07-15 — production polish, surfaced by the exit-code research backing the ggdef elab∘eval verdict-triple; scheme RATIFIED Option A in decisions.md: 0 success / 1 static-rejection / 2 usage / 101 trap+ICE / 103 ggdef-only fuel)
- [`t0646`](todo/t0646.md) **LOW** — [LOW — production gg, consensus polish] Usage/CLI errors collapse into exit 1 instead of 2. Per the ratified scheme, gen…
- [`t0647`](todo/t0647.md) **LOW** — [LOW — production gg, Core-#8 flavor] Internal runtime panics exit 1, colliding with the compile-error code. gorget_pani…

- [`t0648`](todo/t0648.md) **LOW** — [LOW] 🐞 DWARF line-table debug info (gdb/lldb stepping). Backend.debug_info: bool exists but is hardwired false (src/bac…


- [`t0649`](todo/t0649.md) **MED** — 🆕🐛 [MED — R40, gg fmt CLI arg handling] Two pre-existing gg fmt CLI arg-parsing bugs (surfaced by Track G doc verificati…

- [`t0730`](todo/t0730.md) **MED** — [MED — ⚠ OWNER CALL, NOT A MECHANICAL FIX: gg run --backend=llvm and gg run --target=freestanding are ACCEPTED AND SILEN…
- [`t0901`](todo/t0901.md) **MED** — 🆕🐛 [MEDIUM — THE RECORD'S OWN CITATIONS ARE UNGUARDED, measured at R47 integration: 9 of 173 cited known_gaps repro path…
- [`t0842`](todo/t0842.md) **MED** — 🆕🐛 [MED — TWO classes that R47 Track F4a deliberately did NOT fix, both dispositioned here; every figure regenerated at…
- [`t0844`](todo/t0844.md) **MED** — 🆕⚖ [MED — A DECISION, NOT A BUG FIX; the half of the shipped-compiler orphan that R47 Track F4a deliberately did NOT tak…
## Docs / devbook + misc language features

- [`t0650`](todo/t0650.md) **MED** — 🆕📖 [MED — doc hygiene around match/exhaustiveness, three items; found 2026-08-22 by the R44 Track-F scout] (a) REFERENCE…

- [`t0651`](todo/t0651.md) **LOW** — 🆕🧹 [LOW — docs hygiene, found 2026-08-22 by the Track G confirming output-review] docs/devbook/05-formatter.md cites src…
### Medium
- [`t0652`](todo/t0652.md) **MED** — 🆕✨ [MED — RATIFIED 2026-08-11 (owner, live session; ledger docs/define-gorget/decisions.md), not built] The PARENTHESIZE…
- [`t0653`](todo/t0653.md) **MED** — 🆕✨ [MED — designed, never built, never filed; found 2026-08-05] Meta "diamond inference" (Vector[int] items = Vector())…
- [`t0654`](todo/t0654.md) **MED** — 🆕✨ [MED — the ITERATOR-SURFACE RESIDUE, consolidated 2026-08-05 from two design notes slated for deletion (stdlib-design…

#### 🆕 BOOK-FOLLOW AUDIT #1 FINDINGS (filed 2026-07-18; the first newcomer-simulation audit, ch1-3 — full report was /tmp/bookaudit_report.md, re-runnable; ch1 10/10, ch2 6/10, ch3 7/10)
- [`t0655`](todo/t0655.md) **HIGH** — [HIGH — language semantics, ledger-grade] Narrow-type overflow is unenforced: uint8 255 + 1 → 256 (no panic), 255 +% 1 →…
- [`t0656`](todo/t0656.md) **MED** — [MED — emit] f-string mixing int + float leaks C %lld-vs-int64_t format warnings to the user's terminal (aarch64: int64_…
- [`t0657`](todo/t0657.md) **LOW** — [LOW — UX] gg build leaves the emitted .c in the cwd — route to a temp/build dir unless --emit-c is asked.
- [`t0658`](todo/t0658.md) **LOW** — [LOW — book, one-liners batched for the next docs touch] ch2 float64 precise = 3.14 example doesn't compile (type table…

#### 🆕 OWNER-SESSION OUTCOMES 2026-07-19 (the 3-item pass)
- [`t0659`](todo/t0659.md) — Dogfood next domain = NETWORK SERVICE (owner-picked 2026-07-19; DEFERRED same day — owner: backlog burns down first, and…
- [`t0660`](todo/t0660.md) — Sigil pass outcomes: (a) SETTLED — D28 AMENDED with three riders (ledger 8a1f00e5: unary-minus parens reject / right-ass…

- [`t0661`](todo/t0661.md) — 🐛 DOC honesty + validation gaps (each empirically reproduced):

- [`t0662`](todo/t0662.md) — (Documentation chain — 4th chain of the 1:1:1:1 rotation; a doc finding that is really a Rust bug escalates to fix-Rust-…

- [`t0663`](todo/t0663.md) — 📖 DOC TRACK (recurring) — book-ify docs/book + docs/devbook so they read like a publishable book, not a fix registry (da…

- [`t0664`](todo/t0664.md) — 📖 docs/book/12-borrowing.md §"MutationWhileBorrowed" is REFUTED-BY-RUN. The section claims auto entry = v.get(0).unwrap(…

#### ====== STRATEGIC ASSESSMENT RECOMMENDATIONS (owner-requested 2026-07-05; from the full docs+repo three-pillars review) ======
Only NEW items are listed — recommendations already tracked elsewhere are cross-referenced, not duplicated: slice-escape rejection ("ESCAPING SLICE miscompiles" High entry), LSP (Medium entry), CPython-gap perf + lazy-CoW bench coverage (Low entries), self-host fossil audit + borrow-check port (existing entries), doc honesty-audit catalog (Medium entry). Each item below gets its own scout→brief→≥3 reviews before launch.

- [`t0665`](todo/t0665.md) **MED** — [MEDIUM] 📖 Book accuracy + completeness fixes (learner-facing defects found 2026-07-05). (1) parse_int error type contra…
- [`t0666`](todo/t0666.md) **MED** — [MEDIUM] 📜 Doc-drift items NOT yet in the honesty-audit catalog (fold into the devbook honesty-audit catalog (git histor…
- [`t0667`](todo/t0667.md) **MED** — [MEDIUM] 🧪 Learnability field test + pitch reframing (the only pillar with zero measurement). Once the book fixes above…
- [`t0668`](todo/t0668.md) **LOW** — [LOW] 🗺 Competitive positioning doc vs the real neighbors. README compares only Rust/Go/Python. The nearest competitors…
- [`t0669`](todo/t0669.md) **MED** — [MED — doc-track, EXTRACTED 2026-07-21 from the drained NEXT-ROUND-QUEUE line (was TODO:30); pending-only, existed nowhe…

- [`t0670`](todo/t0670.md) **MED** — [MED — doc-track; EXTRACTED 2026-07-21 from the drained NEXT-ROUND-QUEUE line (was TODO:30); SPLIT 2026-08-05 out of the…

- [`t0686`](todo/t0686.md) **MED** — 🆕📄 [MED — docs/language-design.md §10's OPENING concurrency example does not typecheck; measured 2026-08-27 at HEAD in t…
- [`t0751`](todo/t0751.md) **MED** — 🆕🧹 [MED — A DRIFT COHORT OF EIGHT STALE path:line CITATIONS IN docs/devbook/11-copy-on-write.md, and the guard that woul…
- [`t0733`](todo/t0733.md) **MED** — [MED — A CAPABILITY GAP IN THE FFI SURFACE, left standing once the fiction that hid it was removed: there is no way to d…
- [`t0800`](todo/t0800.md) — [MED — DOC WRITE-THROUGH BACKLOG. The burn-down tests/lints.rs's BUDGET re-seed points at.]
- [`t0810`](todo/t0810.md) **MED** — [MED — A SECOND FICTIONAL FEATURE IN THE SAME NARRATIVE RawPtr LIVED IN: Sendable/Syncable are documented as compile-tim…
### Low

- [`t0671`](todo/t0671.md) — 🧹 [resources scout 2026-07-06, LOW/doc] docs/book/19-stdlib.md:563-572 teaches a PHANTOM Arena API — bare local + manual…

- [`t0672`](todo/t0672.md) — (Deferred) Reimplement EntityPool atop a value-less SlotMap. Optional follow-up to the ECS-onto-SlotKey unification. Ent…

- [`t0673`](todo/t0673.md) — (self-host debugging gotcha) driver.gg is NOT the lib_dir — manual stage1 <input.gg> <lib_dir> --lir-c must pass lib (or…

- [`t0674`](todo/t0674.md) — (doc-honesty) Stale test comments: tests/integration.rs:13760-13767 claims sb_push fixed a 5 GB OOM (retracted — real pe…


- [`t1065`](todo/t1065.md) **LOW** — 🆕🧹 [LOW — a DESIGN NOTE claims its own subject is unbuilt while the tree has built part of it; found 2026-09-04 by the o…
- [`t1256`](todo/t1256.md) **LOW** — 🆕🧹 [LOW — a devbook chapter describes tests/lints.rs as five layering ratchets over two directories; it is 219 guards, s…
- [`t1300`](todo/t1300.md) **LOW** — 🆕📄 [LOW — A CORE #15(a) VIOLATION WHOSE DECAY HAS ALREADY HAPPENED; found 2026-09-04 by R49 Track S-a2's fixup pass, the…
- [`t1285`](todo/t1285.md) **LOW** — 🆕🧹 [LOW — a RATIFIED SPELLING that the compiler's own diagnostics never adopted; found 2026-09-04 by R49 Track N2's outp…

- [`t1336`](todo/t1336.md) **LOW** — 🆕📐 [LOW — DOC FRAMING ON AN UNRATIFIED POSITION; surfaced 2026-09-05 by the R50 Track C2 construct-scoped doc scan] The…