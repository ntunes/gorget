# TODO

## ⏭ CURRENT NEXT (the HANDOVER — UPDATE IN PLACE each session; state + NEXT only, no completed recap — landed work lives in DONE.md)

**▶ ROUND XLIX IS OPEN (2026-09-03), owner-authorised — the R48-close suspension of Round-lifecycle
step 7 is SPENT. Headline: THE `Callable` VALUE FORM CARRIES LOSSY TYPE METADATA — a memory-safety
class fix, plus the owner's ease ruling.** No track has been scouted or briefed yet; the roster below
is the ROSTER, not a set of signed-off designs. Every track still owes scout → brief → ≥3 fresh
sequential brief-reviews → executor → fresh output-review before it integrates.

### 📍 R49 LIVE STATE (2026-09-03, orchestrator) — supersedes the per-track prose below on STATUS
⭐ **TRACK E IS SIGNED OFF AND EXECUTING — the round's first executor.** FIVE sequential fresh brief-reviews,
**five confirmations, and not one ever attacked the removal.** Brief: `/tmp/brief_E_v2.md` (body + 2 addenda).
⚠ **§ E's prose below carries STALE SIZINGS** (152 occurrences · 1181 lib tests · 12 symlinks · four parser
stacks). **THE BRIEF WINS OVER § E.** Corrected: **207 lines across 25 real files**, **1185/0**, **29
symlinks**, **five stacks**, and a FOURTH self-host representation (`call_arg_depth`, 18 sites) on top of
`KwIt` / `KW_IT` / bare `"it"`.
⛔ **E's two must-fold errata, both found only at the fifth gate:** (a) **the `figures.db` WAIVER CASCADE** —
moving any parity floor reds three waiver rows, and one is **THE PLANT (`tests/lints.rs:27171`)** whose red
message tells you to *undo a refactor you never made*; three of five `regen` commands begin with an `rm -f`
of the self-host driver, so omitting them measures against a **stale driver** (Core #7's false-green).
(b) **a SIXTH artifact category** — walker-enumeration headers (`compound_yield_race_rhs_walker_underrecurses.gg`,
`t0370`, `t0379`, `c1_d26_closure_body_no_auto_infer.gg`) carrying an *explicitly-total* enumeration this
track invalidates, **one of which § E knew about and the v2 rebuild LOST.**
⚡ **Two decisions the orchestrator settled rather than passing down:** ship the ONE-LINE `E_UndefinedName`
note (the typed retired-keyword table is filed as the follow-up); **UPDATE `beginner_map/MANIFEST.tsv`'s four
rows in place — do NOT delete the corpus, that is Track F's call under `t0018`.**

**⛔ SIX DESIGNS HAVE BEEN REFUTED THIS ROUND, EVERY ONE BY EXECUTION, NOT ARGUMENT. The pattern is the
finding, and it belongs in `docs/devbook/30` at round close:**
- **A1 v1** *"pack at birth + delete the name-matchers"* — deleting them removes the mechanism the fix needs.
- **A1 v1's fold** *"delete the IIFE arm"* — measured: one working fixture → BUILD FAIL, two → SEGV.
- **A1 v2** *"typed GIR pack + keep the escape condition"* — **mutually unimplementable**: there is NO escape
  analysis in the tree, and pack-at-birth makes the IIFE and every HOF arg allocate where they allocate ZERO.
- **A1-M v1** *"hoist `pack_closure_for_smart_ptr_ctor`"* — it keys on a TypeDef **NAME**; a `Callable[T]`
  field is `GirType::FnPtr`, which has none. Hoisting it verbatim moved **ZERO of six cells**. ⊕ And *"the
  single chokepoint"* does not exist (**4 / 26 / 13** callers).
- **C mechanism 1** the one-shot — cannot reach a match-expression **arm tail**, and misses the SECOND `auto`
  statement path (`lower_shared_var_decl`).
- **C mechanism 2** the typed ambient — Form A fixes nothing; **Form B BREAKS TWO OF D23's EIGHT RATIFIED
  `throws_autoprop_*` POSITIVES**, turning `8 / ok / neg` into garbage. **Not narrowable**: only D45-E0's
  typed `!`-provenance can tell an unmarked arm tail from a marked one.

⭐ **AND THE TWO REPLACEMENT DESIGNS BOTH CAME FROM REVIEWERS, PROTOTYPED:**
- **A1-M's fifth design** — `pack_closure_at_dest_type(…, dest_ty: TypeId)` keyed on `GirType::FnPtr`, temp
  typed to the destination, `set_owned_fresh` on it. **6/6 cells green on C AND LLVM.** ⚠ Container-literal
  elements (**D53-ratified**, `decisions.md:529`) are still rc 139 — `t0873(a)` is NOT closed.
- **C v2** — stop making the peel correct everywhere; **MAKE THE SILENT DROP LOUD** at
  `exprs/methods.rs:1653`'s `Bool(false)` fold. ⭐ **Whole-corpus fire count: EXACTLY 1 IN 4348, and it is
  the bug.** It catches its OWN class, is mechanism-independent, and turns a Core #10 violation into the
  LEGAL outcome — loud is legal, silent is not, **even for the cell the track cannot fix.**

⚡ **TRACK F-G SPLIT OUT OF F (2026-09-03)** — D46 on the **ggdef** lane only. F had taken four blocking
passes and the ggdef half was the recurring blocker in two; `AGENTS.md` says REBUILT or SPLIT, never
reviewed harder. F-G inherits: the `Ty::Unknown` decision, the `eval.rs:2294` /
`Value::Tuple|Struct|Enum` alternative that needs **zero inference**, the `spec_conformance_ggdef.rs`
**FrontendError-as-SKIP false green**, and the **open check-time-vs-eval-time question** (D46 says
CHECK-time; an eval-time trap cannot satisfy an `expect.reject` frontmatter). ⛔ **F must not touch
`spec/ggdef/`.** Core #9 still binds the ROUND: if F lands D46's production lanes without F-G, the round
owes a note + a filed subset gap.

⚠ **INSTRUMENT LESSONS THAT OUTLIVE THIS ROUND:**
- **The whole-corpus GIR sweep is a REGRESSION BOUND, not coverage.** It gave `CHANGED=1` for BOTH of C's
  mechanisms while they differ on FOUR cells. **820 of 4348 fixtures do not build at HEAD**, so the positive
  denominator is **3528**, and negatives are compared **by rc only — a changed REJECTION MESSAGE is
  invisible.**
- **`cargo test --test lints` is FLAKY at HEAD** — `orphan_reaper_self_test` (`lints.rs:25954`): one run
  exits 101, the next 0; passes in isolation. **Pre-existing, unrelated to any track. Do not chase it.**
- **A stdout/md5 gate is structurally blind to the leak class a materialization change moves** — ASan is
  mandatory wherever a fix allocates an env (Core #13).
- **`ConsumeSiteClass` is the WRONG WITNESS for AST-lowering sites** — a category error: it enumerates GIR
  *instruction* kinds, and one `StructInit` arm has FOUR producers. All nine arms can be dispositioned while
  `Vector[Callable] = [closure]` still SEGVs.

### ⛔ R49's ROSTER MISSED THE CRITICAL SET — TRACK K OPENED 2026-09-03 (owner-directed)
**Owner asked: *"Is there any other critical item in `todo/` not being fixed in this round?"* The answer was
YES, and the count is stark: `grep -l 'severity = "CRITICAL"' todo/*.md` returns **TWELVE**, and R49's roster
was aimed at exactly ONE (`t0937`, via A1-M).**
⛔ **AND `todo/t0871` CARRIED A STANDING OWNER DIRECTION THE ORCHESTRATOR NEVER READ:** *"The `s[a:b]`
soundness fix opens in **R49**, not R48 (owner: the round is already full)."* It was not a candidate to
weigh — **it was already assigned to this round.**
⇒ **THE ROSTER-LEVEL FAILURE IS THE SAME ONE THE REVIEWERS CAUGHT SEVEN TIMES INSIDE THE TRACKS: an
ENUMERATION replaced by a SELECTION.** The roster was built from the ease assessment + R48's own debt +
what the handover foregrounded, and **`grep severity = "CRITICAL"` was never run.** ⭐ **A ROUND'S ROSTER
OWES THE SAME WITNESS DISCIPLINE AS A TRACK'S FIXTURE AXIS — enumerate the severity field, do not select
from the narrative.**
✅ **TRACK K OPENED**, headlined on `t0871` (⭐ *a GUARD was enforcing migration onto the broken spelling* —
the damage grows while it sits), with `t0697` as a single secondary probe to decide same-root-or-separate.
**ID block `t1048`–`t1057`. First unissued is now `t1058`.**
**THE EIGHT CRITICALS NO R49 TRACK TOUCHES**, for whoever plans R50 — `t0011` (`Box[T](struct …)`) ·
`t0036` (memory safety from a plain READ of safe syntax) · `t0045` (from SAFE SPEC-DOCUMENTED syntax) ·
`t0680` (live miscompile at clean HEAD: silent wrong value on C, hard `llc` error on LLVM) · `t0697`
(reachable by changing ONE TOKEN in a committed PASSING fixture) · `t0703` (which NAME a view is spelled
through decides memory safety, and the CoW rescue itself reads the freed buffer) · `t0709`
(`Vector[Box[Trait]]` returned from a helper → rc 139 both backends) · `t0871` (now Track K).
⊕ **`t0704`/`t0771` are CITED by A1-M and explicitly HANDED OFF, not fixed** — do not read A1-M's citation
as coverage. ⊕ **`t0988` was FILED this round and F confirmed it does NOT fix it.**
⚠ **ASan IS STRUCTURALLY BLIND to `t0871`'s class** — the custom `__gorget_current_alloc` pool emits no
report. **stdout is the only instrument.** Do not read a green sanitize sweep as coverage of it.

### 🔢 R49 ID BLOCKS — orchestrator-allocated per AGENTS.md multi-agent rule **MA-3b** (owner 2026-09-03)
**A TRACK NEVER PICKS ITS OWN `todo/` ID.** Each executor gets a private disjoint block in its brief and
asks the orchestrator to extend it if exhausted. This retires the collision that forced the Track P/R
renumber.

| A(-M) | E | F | H | B | C | D | G |
|---|---|---|---|---|---|---|---|
| `t0967`–`t0976` | `t0977`–`t0986` | `t0987`–`t0996` | `t0997`–`t1006` | `t1007`–`t1016` | `t1017`–`t1026` | `t1027`–`t1036` | `t1037`–`t1046` |

⭐ **`t1047` IS ALLOCATED TO THE OWNER (2026-09-03, owner request).** RESERVED — ⛔ no track may use it,
and the orchestrator must not re-issue it.

⛔ **SECOND-WAVE BLOCKS, ISSUED AFTER THE TABLE ABOVE — READ THIS BEFORE ALLOCATING ANYTHING.**
**A's original block is FULLY CONSUMED by A1-M**, so the tracks split out mid-round got fresh blocks:

| K (`t0871`) | A1-I |
|---|---|
| **`t1048`–`t1057`** | **`t1051`–`t1060`** ⚠ overlaps K's tail — see below |

⚠ **THE OVERLAP IS REAL AND IS RESOLVED THIS WAY: K holds `t1048`–`t1050` ONLY** (issued for the `&`-route
CoW leak, the String `.enumerate()` byte/codepoint defect, and the string-iteration torn read); **A1-I holds
`t1051`–`t1060`.** If K needs more, it gets `t1061`+, never `t1051`–`t1057`.
⛔⛔ **COLLISION, 2026-09-04 — I CAUSED IT AND IT IS RECORDED HERE SO THE SHAPE IS NOT REPEATED.** I
narrowed K's block in this ledger **without propagating the change to K, whose brief still said
`t1057`.** K then filed **`t1048`–`t1051`**, so **`t1051` collided with A1-I's block.** Resolved by
moving A1-I: **K holds `t1048`–`t1051` (all four SPENT); A1-I holds `t1054`–`t1063`**, with `t1052`
and `t1053` unchanged and still A1-I's. ⚡ **THE LESSON, WHICH IS NOT THE ONE MA-3b ALREADY ENCODES:**
**a block NARROWING is an ALLOCATION EVENT and must be pushed to the holder — the ledger being right
is not enough if the brief that the executor reads is stale.**
⚡ **WAVE-3 BLOCKS (the owner lifted the sizing hold 2026-09-04 to bring more CRITICALs in):**

| L — closure-capture | M — Box/trait-object | N — `t0988` | P — drop side |
|---|---|---|---|
| **`t1066`–`t1075`** | **`t1076`–`t1085`** | **`t1086`–`t1095`** | **`t1096`–`t1105`** |

⊕ **`t1053` RETURNED TO THE POOL** — A1-I refused to file it because `todo/t0774` already owned the subject,
**following GREP-BEFORE-YOU-FILE over my instruction.** It is free.
⚡ **FIRST UNISSUED ID IS NOW `t1106`** (`t1065` filed; `t1053` and `t1056`–`t1063` free for re-issue).
⚠ **The issued ids are NOT yet on disk** — their tracks are still executing, so `ls todo/` cannot tell you
what is taken. **This table is the only record. A `ls`-based "next free id" WOULD RE-ISSUE `t1048`, which is
exactly the collision MA-3b exists to prevent.**

⚠ A block is a CEILING on collision, not a filing quota — R49's mandate is burn-down, and an unused block
is the good outcome. **Wave 1 in flight: A · E · F · H** (scouts launched 2026-09-03). Wave 2 launches one
track at a time as each wave-1 track INTEGRATES; **B must not launch before E integrates** (E dissolves
`t0963`, which would otherwise be B's work).

Next free todo id is **`t0967`** (`t0946` is an unused gap from the Track P/R collision renumber and stays
unused). `AGENTS.md` is **36,186 bytes** against the 49,400 ceiling — **`t0714` and `t0577` are UNBLOCKED**;
the headroom is DELIBERATE (the lint says compact a neighbouring rule rather than raise it), so do NOT
lower the ceiling to "lock in" Track S's compaction.

### 🔻 THE ROUND'S SHAPE — ⛔ CORRECTED 2026-09-03: **CONVERGENCE IS NOT A GATE AND NEVER WAS THIS ROUND**
⚠ **THIS SECTION PREVIOUSLY READ "BURN-DOWN, NOT DISCOVERY" AND THAT FRAMING WAS BUILT ON A RETIRED RULE.**
The orchestrator opened R49 off an orchestrator-memory entry dated 2026-08-02 (strict 2×: TODO must
strictly decrease, close ≥ 2× filings). **The owner RETIRED that rule on 2026-08-23**, and Track F's scout
caught it against the source. Verify at the source, not here:
- `scripts/convergence.sh`: *"The STRICT 2x RULE was REMOVED. It was failed repeatedly, and the rounds that
  failed it were the ones doing the most valuable work: a round that finds nine real defects — three of
  them memory-safety — is a GOOD round that the ratio scored as failing. Measuring inflow is useful;
  GATING on it selected against discovery."* The block reports and **always exits 0**.
- `AGENTS.md` step 5: *"a MEASUREMENT, NOT A GATE"*. `grep -c '2×\|2x\|STRICT 2' AGENTS.md` → **0**.

⇒ **R48's net +47 is NOT an indictment**, and no track in this round is scoped, deferred or rescoped to
protect a ratio. **Rank by SOUNDNESS** (mem-unsafety > silent-wrong-output > ICE > leak). ⛔ **DELETED: the
old warning that "the one track that can blow convergence is F."** It was the stale rule talking; F's
scout confirmed the accounting is safe anyway, and the whole framing was wrong.
✅ **What SURVIVES the correction, on its own merits:** `t0936`–`t0964` are R48's own debt, most carry a
durable RED-verified repro, and they are genuinely cheap — take them because they are ripe, not to feed a
number. Bulk-graduation out of `known_gaps/` is still good value.
📌 **THE LESSON, for the next orchestrator:** an orchestrator-memory entry is a claim that DECAYED, not a
fact. A binding rule gets re-verified at its SOURCE before a round rests on it (SIX QUESTIONS coda: *is
this premise still TRUE, or a filed fact that decayed?*). The memory entry is now corrected at its own file.

### 🧭 R49 ROSTER — 8 TRACKS (owner-selected 2026-09-03; sizing answer was "6 + graduations", then the
### `it` ruling landed and E/F/G split out of the ease pick — a SPLIT, never a deferral)
- **A · SPLIT INTO A1/A2/A3/A4 BY ITS OWN SCOUT (2026-09-03) — ⚠ THE ONE-ROOT PREMISE IS REFUTED.**
  ⛔ **DO NOT BRIEF "the Callable cluster" AS ONE TRACK.** The scout measured **FOUR** roots with four fix
  sites and no implication between them: its prototype moved every Root-A cell and moved **nothing** in
  B, C or D. A split is division, never deferral — all four are R49 tracks.
  - ⛔ **A1 IS SPLIT AGAIN (2026-09-03) — THREE DESIGN REFUTATIONS, AND THE THIRD REVIEW GAVE THE ANSWER.**
    v1 (pack at birth + delete the name-matchers) and v2 (typed GIR pack + keep the escape condition) were
    BOTH refuted by execution. **v2's fatal flaw: §2.1 and §2.3 are MUTUALLY UNIMPLEMENTABLE.** There is
    **NO escape analysis in the tree** (`grep -rni escape src/ir/ src/lir/` → only arena prose), and the
    only pack implementation (`operands.rs:1444-1476`) allocates **unconditionally**. Measured allocs in
    `main` at HEAD: `((int x): x*x)(5)` → **0** · `v.map((int x): x*2)` → **0** ·
    `Callable c = literal; c(1)` → **1**. ⇒ pack-at-birth makes the IIFE **and every HOF closure argument**
    allocate — the charter breach v2 itself forbade. ⊕ And what v2 called *"the escape condition"* is a
    **REPRESENTATION check** (`operands.rs:1384-1392`, destination slot is a `GorgetClosure` struct), not
    escape analysis — an executor told to "keep it" would believe it had analysis it does not have.
    ⭐ **THE SPLIT THAT WORKS — separate the two facts the word "pack" conflates. NEITHER NEEDS AN ESCAPE
    ANALYSIS, and BOTH have shipped precedent in the files they touch:**
    - **A1-IDENTITY (the Core #2 half) — resolve once, write through, at THREE carriers.** One typed field
      on GIR `Local` (`ir/mod.rs:700-733`, precedent `deref_of_owning_param: Option<LocalId>`), written by
      `lower_closure` — retires the 13 GIR/LIR-lower sites. Plus **`StructDef.closure_call_fn`**
      (`lir/mod.rs:1554`), populated exactly as `elem_drop_fn` (`:1576`) and `elem_clone_fn` (`:1584`)
      already are — both documented as *"Replaces the c_lir `elem_drop_fn_for_c_type` name-prefix
      matching"*. ⭐ **THIS EXACT FIX HAS SHIPPED TWICE IN THAT FILE FOR THIS EXACT REASON.** Plus
      `LirFunction.takes_env: bool` for `lir/lower/mod.rs:257` and `bir/synth.rs:72` — a per-FUNCTION fact,
      not a per-value one. ⚠ **A per-value carrier alone CANNOT reach 6 of the 19 sites — and v2's own
      argument against `GirType` (it cannot reach the backends) kills v2's chosen carrier for the same 3.**
    - **A1-MATERIALIZATION (the `t0937` half) — keep it demand-driven; make the SET total at ONE
      chokepoint.** ⭐ **The reference-grade shape ALREADY EXISTS and no draft cited it:**
      `pack_closure_for_smart_ptr_ctor` (`calls.rs:863-895`) reads `TypeDef.metadata.c_runtime_alias ==
      "GorgetClosure"` — typed metadata, its own comment saying *"not by name — per CLAUDE.md 'no name
      matching'"* — and already has **seven** call sites. **Hoist it to the single consuming-position
      chokepoint** (the move AGENTS.md prescribes verbatim: *"prefer centralizing at the producer … e.g.
      `maybe_auto_propagate` hoisted to the `lower_expr` exit"*) so StructInit / EnumInit / TupleInit /
      projection-assign / CallByValueArg all route through one packer; **delete `wrap_single_closure_arg`'s
      duplicate**; pin with an arm-count lint on the `container_literal_arms_count` precedent
      (`tests/lints.rs:1268`). **Closes `t0937` ONLY** — plus TupleInit, `s.f = literal` and the SIGBUS
      cell. ⛔ **CORRECTED 2026-09-03 by A1-M's own executor: it does NOT close `t0938`** (`Some(^h)` /
      `push(^h)` still ICE at `mod.rs:2144`, re-verified) **nor `t0873(a)`** (`S([lit])` stays RED in
      `known_gaps/callable_literal_in_container_literal.gg`). The handover asserted both for two
      generations; a track that reports what it did NOT close is doing the gauntlet's work for it.
    ⭐ **AND THE THREE-PASS MYSTERY IS SOLVED: there are TWO pack implementations.** `try_closure_pack`
    (`operands.rs:1357`, one caller) and `wrap_single_closure_arg` Case 2 (`:1703-1780`) are near-duplicate
    alloc+memcpy+`ClosurePack` sequences; `Inst::ClosurePack` is emitted from **4** sites
    (orchestrator-verified). **That is why `fs[0] = literal` is GREEN (prints `3`, both backends) while
    `s.f = literal` SEGVs** — index-store routes through `wrap_single_closure_arg`, and `try_closure_pack`
    is gated on `dst.projections.is_empty()` (`insts.rs:43`). The control carried UNVERIFIED for three
    passes is now explained.
    ⚠ **THE CENSUS WAS A SELECTION FOR THE FOURTH TIME — and the PATTERN is the finding.**
    `lookup_closure_info` has **11 references across 4 files** (orchestrator-verified); every draft counted
    2. Each pass found one more instance *by reading around the previous one*; **nobody ran the total
    grep.** ⊕ A whole SECOND runtime-symbol convention is absent from every draft: `__callable_N` /
    `__gorget_closure_call_N`, minted by `format!` at `calls.rs:1898/1993/2340`, `methods.rs:3829`, decoded
    by `starts_with` at `insts.rs:3892/3895`, `validate.rs:420/428`, `llvm/mod.rs:1792/1975`, plus
    `optimize.rs:218` and `insts.rs:551`. **Draw the A1/A2 boundary explicitly or count them.**
    ⚠ **THE GATES ARE STRUCTURALLY BLIND TO THE HAZARD A1 MUST PREVENT (Core #13).** `Holder(^c)` under
    `--sanitize` prints `42` then **leaks 16 bytes** — it does NOT double-free, because
    `field_is_transitively_droppable` requires `GirType::Named` so an FnPtr field never drops. ⇒ **the
    double-free A1 must prevent cannot be observed by ANY gate at HEAD.** **MANDATORY POSITIVE CONTROL:
    locally patch that predicate to return `true` for FnPtr, run the whole cell battery under `--sanitize`
    on BOTH backends, confirm no double-free, revert.** ✅ A1 and A3 ARE separable (with A3 unlanded,
    birth-registration degrades to today's leak, never a double-free) — **but that safety is exactly what
    hides the gap, so the control is not optional.**
    ⚠ **`ConsumeSiteClass` CANNOT DISCRIMINATE TupleInit** — `validate.rs:2641-2651` bins it under
    `StructInit { type_name: "<tuple>" }`. So *"a disposition per row over the nine arms"* is satisfiable
    **with TupleInit still broken** (SIX QUESTIONS #6). Add the discriminator or pin TupleInit's own cell.
    ⚠ **`todo/t0681` IS UNOWNED AND A1 WALKS INTO IT.** `methods.rs:276` is in every census AND is the exact
    site `t0681` blames for `Box[Callable[int(int)]](literal)` ICE-ing while `Box.new(literal)` prints `42`.
    Deleting that early-return drops `Box.new(closure)` into the consuming shim whose own comment warns of
    the double-free A3 makes real. **Claim it or state the carve-out is rewritten-not-deleted.** ⊕ This
    RETIRES the *"whether `Box[Callable]` shares the root"* line carried unverified for three passes: it has
    a filed item, a mechanism and a durable repro.
    ⚠ **UNGUARDED MIRROR HAZARD:** if the closure's GIR type stops being `Named("__Closure_N")`,
    `insts.rs:3305`'s `wrappable` gate goes false and the HOF wrapper stops firing, and `methods.rs:5110`
    returns `None` so `map`/`flat_map` lose return-type inference. **`v.map((int x): x*2)` works TODAY
    BECAUSE it keeps the raw env and passes it by pointer.** v2 warned against creating reachability; the
    mirror — LOSING it — was unguarded.
    ⊕ **`t0967` FILED** (from A1's block) for the disjoint D10(b) finding this review turned up.

  **[superseded — v1/v2 framing kept for provenance]**
  - **A1 · PACK-AT-BIRTH (⭐ the CRITICAL one; wave 1).** `src/ir/lowering/closures.rs:360` returns the raw
    `Named("__Closure_N")` capture ENV, not the `GorgetClosure` fat pointer `Callable[T]` denotes, so
    materialization is deferred to consumers that each re-recognise a closure BY NAME PREFIX. Owns
    `t0937` ⭐ (`t0938` and `t0873(a)` are **NOT closed** — see the correction above) + **3 cells NOBODY HAS FILED** (`TupleInit`; `Assign`-with-projection;
    and a THIRD fault mode — rc **135 bus error** on a capturing ctor literal). ⚠ **Those three are A1's
    SCOPE, not new filings** (rule 0: incorporate by default, file only when genuinely disjoint).
  - **A2 · `Callable` TYPE ERASURE.** `try_map_ast_type` returns `None` for the Callable family
    (`src/ir/lowering/types.rs:244`) and `Type::Function` (`:259`); `map_ast_type` (`:166`) then falls back
    to `UNIT_TYPE`. Owns `t0942` · `t0927` · `t0406`×4 · **`t0959`** — which is Root **B**, NOT Root A, so
    `t0959`'s `param_ownerships` / `callable_alias_sigs` shape belongs to A2 and the SH port is A2's.
  - **A3 · `FnPtr` DROP RECOGNITION — owns 100% of the leak class.** Two predicates on ONE axis disagree
    on purpose (Layering rule 3, in plain sight): `needs_drop` (`src/ir/types.rs:487`) says FnPtr ⇒ true,
    `field_is_transitively_droppable` (`:537`) says false. The exclusion is licensed by a doc-comment
    invariant at `:531-536` — *"captures use `MutPtr(T)` / value fields, never `FnPtr`-as-field… a
    function-body local, not a struct field"* — which **`t0948`'s struct-field repro FALSIFIES (Core #14)**.
    Owns `t0948` · `t0949` · `t0953` · `t0873(b)`. ⚠ **SEQUENCE A1 → A3**: A1 changes what a closure value
    IS, hence what A3 must register. Landing A3 first means redoing it. ⚠ `t0949` also warns it and
    `t0871` want the same site — that constraint lands here.
  - **A4 · FIELD-CALL RESOLUTION.** `s.f(1)` resolves to method symbol `S__f`. Owns `t0939`. Small, disjoint.
  ⚠ **MY LEAK FIGURE WAS MISLABELLED AND IS CORRECTED HERE.** I wrote *"56 of the 754 rows"*; **754 is
  `wc -l` including comments.** Regenerated at `a1eaba9c1`: **294 DATA rows**, of which `__gorget_closure_env_alloc`
  is the sole frame on **56** and appears in **81** (not 80), covering **320 records** of the total pinned
  by `sanitize.leak.records.pin` (`scripts/figures.db` — cite the row, never re-spell its value).
  ⛔ **AND THE CLASS IS NOT A1's.** Root A's signature is that the env is **never allocated**; an allowlist
  row exists because the fixture ran and leaked with that frame on the stack — i.e. it **was** allocated.
  **A1 and the 56-row leak class are DISJOINT BY CONSTRUCTION.** The leak class is **A3's** in full.
  ⭐ **THE SELF-HOST IS GREEN ON ALL NINE ROOT-A CELLS — the reference lags it, and the fix is already
  written in Gorget.** `tests/fixtures/self_host_lowerer/lower_expr.gg:6058-6059` packs at birth, with its
  own comment at `:6022` saying so. **A1's SH lane is NONE — do NOT touch the self-host**; per the
  succession plan, fix Rust gg to match. (`lower_expr.gg` has exactly ONE copy — no driver-embedded twin.)
  ⊕ **NAME-MATCH SITE COUNT — the scout said 3, I measured 6.** `src/lir/lower/operands.rs:1417`, `:1711`,
  `src/lir/lower/insts.rs:3305`, `src/lir/lower/mod.rs:257`, `src/ir/lowering/exprs/methods.rs:276`, `:5110`.
  The Core #2 violation is retired AS PART OF A1, not filed beside it — so the brief must carry all six.
- **B · HOF ACCUMULATOR / ITERATOR OWNERSHIP.** `t0954` (`map` mints the accumulator with a NULL
  `elem_drop`, leaking every element) · `t0955` (`flat_map` leaks the callee's `Vector` per input element)
  · `t0952` (`DictIter.next()` deep-clones the WHOLE Dict 4× per call and frees none) · `t0951` (`^self`
  never emits the callee-side drop). ⚠ **`t0963` MOVES TO TRACK E — see the interaction note below.**
- **C · ✅ SCOUTED — THE CLEANEST RESULT OF THE ROUND, and it CORRECTS MY LEDGER READING.**
  **WRITE SITE: `src/ir/lowering/context.rs:2121`** — `infer_type_from_expr`'s `_ => I64_TYPE`. The function
  has 10 arms and **no `MethodCall` arm**, so a method call falls through; `stmts/mod.rs:613` writes that
  GUESS as `expected_type`; `maybe_auto_propagate` reads it, concludes "destination is not a Result", and
  peels. ⭐ **The sharper framing is LAYERING RULE 3/4, not just Core #10: `infer_type_from_expr` is a
  SECOND, WEAKER, INDEPENDENT type inference that lowering runs after the typechecker already resolved the
  expression.** Measured proof they disagree: `bool j = s.join(g); j.is_error()` is REJECTED, but
  `auto j = s.join(g); j.is_error()` is **`gg check` CLEAN** — the typechecker says `Result[bool,String]`,
  the lowering says `bool`. ⚠ The guess is wrong CONSTANTLY: **1194 type disagreements across 4312
  fixtures**; the corpus is green only because the reader usually does not care.
  ⛔ **I BRIEFED THAT D45 PIN 6 "LEANS (b)" — I HAD IT BACKWARDS. PIN 6 MANDATES (a), so there is NO OWNER
  ASK.** `decisions.md:2069-2072`: *"propagation happens at `!`-marked calls only"*; `auto` is not a `T`
  position and `s.join(g)` carries no `!` ⇒ no propagation ⇒ it binds the `Result`. Corroborated four ways,
  each measured: **ggdef RUN and adjudicates (a)** (`handled`/`custom message`, while Rust gg pre-fix fails
  to COMPILE the same program) · D23's subject is a **`throws`** call, not a `Result`-returning one
  (SIX QUESTIONS #4) · `book/02-types.md:35` (*"`auto` … infers and locks"* — from the RHS) · D29's own
  diagnostic already names *"an explicitly `Result`-typed binding"*.
  ⭐ **SEPARATE ROOT from `t0050`/`t0101`/`t0105`/`t0434` — MEASURED, not argued:** the prototype fixes the
  repro and moves **none** of their fixtures. Four write sites on one machinery; D45 pin 6's **E0**
  subsumes all four, and this fix is a strict, zero-blast-radius SUBSET of E0 in the shape E0 mandates.
  ⭐ **BLAST RADIUS MEASURED OVER THE WHOLE CORPUS, not spot-checked:** `SWEPT=4312 · CHANGED=1 ·
  POST_FAIL_ONLY=0 · PRE_FAIL_ONLY=0`. **14 lines, one file**, reusing the EXISTING typed
  `suppress_auto_prop` one-shot. `--lib` 1185/0; the `#[ignore]`d test RED at HEAD and GREEN with the fix;
  **LLVM measured too** — the fix is in GIR, so no backend-specific path.
  ⭐ **SEVEN UNFILED CELLS, ALL FIXED BY THE SAME ONE-SITE FIX — they are the FIXTURE SET OWED, NOT
  FILINGS.** Witness: **rustc E0004** — deleting the `_` arm names *"37 of 47 `Expr` variants"* uncovered.
  Broken beyond the filed method-call cell: **generic free call** (contradicting `t0947`'s own *"FREE call
  is safe"* row) · chained method · static/assoc · closure-var call · `T f() throws E` context · and two
  consumer modes that are HARD failures rather than silent (`match x: case Error(e)` → C-emit error;
  `.unwrap_error()` → link error). ⚠ **READINESS ITEM 3 FAILS TODAY: |pinned| = 1, |changed| ≥ 8.**
  ⊕ `!`-marked calls peel CORRECTLY today and are **ACCIDENTALLY CORRECT** (SIX Q#6) — the fix must
  preserve that, and does.
  ⊕ **SH port required and NOT a copy:** `self_host_lowerer/lower_stmt.gg:135-153` sets `expected_type`
  only when the declared type is neither `I64_TYPE` nor `UNIT_TYPE`, so for `auto` it leaves the AMBIENT
  value and still calls `maybe_auto_propagate` at `:153`. Probe before porting.
  ⊕ **Guard: an arm-count ratchet on `infer_type_from_expr`** (precedent `container_literal_arms_count`,
  `tests/lints.rs:1268`). The IDEAL guard — assert lowering's local type equals `expr_types[span]` — is
  `t0434`/D45-E0 work; say so rather than half-building it.
  ⊕ **Stdlib CLEAN** (3 `lib/` sites, all the safe cell, instrument-verified). **But `cli_basic.gg:16,34`
  and `cli_advanced.gg` carry 7 live sites green ONLY because `main` is `void`** — move any into a
  `Result`-returning helper and it silently miscompiles. That is the idiomaticity argument for the DONE entry.
  ⚠ My `auto`-usage figures were stale: **604** in fixtures ex-self-host (not ~450) · **19** in self-host
  (not 4) · **80** in `lib/`.

  **[superseded framing kept for provenance]** — **C · AUTO-PROPAGATION SILENT STATEMENT DROP.** `t0947` + the family it discriminates itself from
  (`t0050` · `t0101` · `t0105` · `t0434`). `auto x = <METHOD call returning Result>` binds the Ok payload
  and **silently discards the user's whole `if x.is_error():` block**; it was LIVE in shipped
  `lib/xtd/p2p.gg` with 13 fixtures passing BY LUCK. ⚡ **POSSIBLE OWNER ASK, not yet raised:** the item
  says either direction is legal (run the block, OR auto-propagate + REJECT `.is_error()` on the
  payload-typed local, which is where D45 pin 6 leans) — but *accepting the block and discarding it* is
  legal under neither. Default to the first, which needs no ruling and matches the explicit binding.
- **D · SELF-HOST LANE PORTS (Core #9).** `t0941` · `t0944` · `t0962` · `t0958` · `t0961`. (`t0959` sits in
  Track A, because it is that class, not a separate port.) Under the owner's 2026-08-08 standing
  obligation: parity is no longer the north star, but SH stays in sync.
- **E · REMOVE THE IMPLICIT `it` CLOSURE PARAMETER — ⚡ OWNER-RULED 2026-09-03: REMOVE.** *"Remove it. Do
  not rename"* — every structural cost (body scan, third closure spelling, shadowing trap, lane
  divergence) survives a rename. ✅ **SCOUTED AND PROTOTYPED END-TO-END: the Rust half is DONE and GREEN**
  — `/tmp/recover_trackE_full.patch`, 23 files, **+34 / −540**, `cargo test --lib` **1181 passed / 0
  failed**, `cargo test -p ggdef` 187+ green with **ZERO ggdef edits**.
  ⭐ **THE REMOVAL IS STRUCTURAL, NOT JUST EMPIRICAL — both AST nodes have exactly ONE producer:**
  `Expr::It` ← `src/parser/expr.rs:496` (only from `Token::Keyword(Keyword::It)`) and
  `Expr::ImplicitClosure` ← `:2290` (only when `contains_it(&value)`). `ImplicitClosure` is reachable ONLY
  through `Expr::It`, which is reachable ONLY through the keyword. **Delete the keyword and both become
  unconstructible.**
  ⚠ **THREE OF MY NUMBERS WERE WRONG; the scout's win and are used here.** Self-host is **152 occurrences
  across 25 REAL files**, not 91 — I counted symlinks as content and missed `expr_has_it` /
  `EImplicitClosure`. Fixture usage is **22 lines in 11 files**, not 12 in 3 — I missed four
  `robustness_map/` cells, `known_gaps/hof_call_env_leak_unbounded.gg` and `fmt_author_parens/wrappers.gg`,
  and **those six carry the round's best evidence**. (Rust: 22 files / 99 in `expr.rs` — both confirmed.)
  ⛔ **AND MY "rustc exhaustiveness is the independent witness" INSTRUCTION WAS WRONG AS A TOTAL CENSUS.**
  Deleting the variants yields `E0599`, never `E0004` — it enumerates the AST-arm class COMPLETELY and
  **nothing else**. **SIX further sites are STRING-ANCHORED and invisible to the type checker**, appearing
  only when the tests RUN: `tests/lints.rs::{expr_stmt_walker_population_is_pinned` (loses 2 rows),
  `fmt_author_paren_dedup_class` (caller count 2→1), `formatter_collection_literal_interior_hook_dispatch`,
  `d29_propagate_walker_arm_coverage` (arm count 2→1), `doc_source_citations_name_the_right_line` (**8
  citations in `docs/devbook/05-formatter.md` drift**)`}` and `src/parser/tests.rs::test_var_decl_with_it_binding_rejected`.
  ⊕ Also invisible: **removing a struct field does not orphan its doc comment** — the scout's
  `call_arg_depth` removal silently reattached its `///` to `expr_depth`, giving that field two doc
  comments under `warnings = "deny"`. ⇒ Gate order: `cargo check --all-targets --message-format=short`
  → `cargo test --test lints` → `cargo test --lib`.
  ⭐ **DISSOLVES THREE ITEMS OUTRIGHT PLUS ONE BULLET, not just `t0963`:** `t0963` (Rust `gg` miscompiles
  `flat_map(it)`) · **`t0962`** (self-host over-rejects comprehension-bodied implicit-`it` — the whole
  class IS implicit-`it` HOF args) · **`t0961`** (self-host has TWO implicit-`it` detectors, a split
  decision — both deleted) · and the `t0310` bullet *"the `it`-lambda tail dodges the drop-purity return
  check"*, which cites `Expr::It` + `implicit_it_type` by name. ⚠ **`t0310` is the RATIFIED
  enforcement-wave plan — strike ONLY that bullet, with a note, never the item.** Also answers the open
  sub-question in `known_gaps/compound_yield_race_rhs_walker_underrecurses.gg` (17 hiding places → 16).
  ⊕ **The scout ASKED SIX QUESTIONS #3 of the dissolution rather than assuming it.** `t0963`'s repro
  samples a SELECTION — typed closure and named fn, but not the untyped `(v): v`, the nearest sibling. If
  the real axis were "untyped identity closure", removal would RELOCATE the bug. Measured: typed ✅ named ✅
  **untyped `(v): v` → `1 1 2 2` ✅**. The defect is unique to the `Expr::It` lowering path.
  ⚡ **TRANSITION SURFACE — the scout's recommendation, and option (b) as I framed it is SELF-DEFEATING.**
  Keeping `it` reserved to carry a fix-it means `for it in cart:` still fails, forfeiting the `t0018` win.
  **Recommend (a) ordinary identifier PLUS an `it`-keyed note on `E_UndefinedName`** — *"`it` is no longer
  a keyword; write an explicit closure, e.g. `(x): x * 2`"* — slot already exists at
  `src/semantic/errors.rs:1160`/`:1192`; it should DISPLACE the unhelpful *"did you mean 'xs'?"*.
  Measured post-removal: `for it in cart:` **runs** (prints `6`; `ex_shopping_cart.gg` prints its expected
  output exactly) and `xs.map(it * 2)` gives ONE clean spanned error. ⊕ Brief the reviewers that a
  diagnostic keyed on the string `it` is NOT a "No name matching" violation (that rule governs SEMANTIC
  decisions in the pipeline, not diagnostic text) so nobody has to adjudicate it cold.
  ⭐ **FIVE ARGUMENTS FOR THE RULING THE ASSESSMENT DID NOT HAVE — the owner asked to be told, not agreed
  with, and the evidence came back STRONGER:**
  1. **`it` is MEMORY-UNSAFE on three `robustness_map` cells TODAY.** Every implicit-`it` cell:
     `hof_implicit_it` (selfhost **TRAP**, asan **SANITIZE-FAIL**) · `hof_filter_implicit_it` (**WRONG** +
     **SANITIZE-FAIL**) · `doc_ld_implicit_it` (**WRONG** + **SANITIZE-FAIL**) — all three DIVERGENT. Top
     of the owner's own severity ladder on every cell that exercises it.
  2. **`docs/language-design.md:1713` rejects `$1`/`$2` for the reasons `it` INCURS** — *"introduce arity
     ambiguity (the compiler must scan the body to determine parameter count)"*. That scan IS
     `contains_it`: **212 lines of five mutually-recursive full-AST walkers with exactly ONE call site.**
  3. **The keyword already cost a revert and a silent miscompile** — `src/parser/stmt.rs:678-686`: commit
     `089b8e48` accepted `it` as a binding; `int it = 42; print(it)` **printed garbage** with only an
     unused-variable warning. The current hard parse error is the scar tissue.
  4. **A DOCUMENTED §7.6 example has NEVER worked.** `matrix.map(it.map(it * 2))` (`language-design.md:1732`):
     `gg check` says *"OK: no semantic errors"*, `gg run` dies with raw **C compiler** errors. Check-clean,
     codegen-broken, UNFILED — a Core #10 hole in a construct the design doc advertises.
  5. **Zero real users** — `lib/std`, `lib/xtd`, `examples/`, `spec/`: not one use. Every occurrence is
     test corpus or self-host implementation.
  ⊕ Removal also SHRINKS two structural ratchets and retires the "transparent wrapper" hazard class
  entirely (`ImplicitClosure` was the ONLY such wrapper).
  **FIXTURES — the split matters:** DELETE `implicit_it.gg`, `hof_implicit_it_collection_axis.gg`,
  `known_gaps/flat_map_implicit_it_zeroes.gg`, `known_gaps/sh_comprehension_bodied_implicit_it.gg`,
  `robustness_map/cells/doc_ld_implicit_it.gg`, `hof_implicit_it.gg`, `hof_filter_implicit_it.gg` + their
  wired tests (`integration.rs:6493`, `:6513`, `:7348`). **REWRITE, do NOT delete** (the `it` is
  incidental): `closure_mixed_implicit_explicit_wiring.gg:18` · `fmt_author_parens/wrappers.gg:29-31`
  (rewrite as an explicit closure so `fmt_author_parens_round_trip_semantic`'s expectation string is
  UNCHANGED) · `known_gaps/hof_call_env_leak_unbounded.gg:35`. **KEEP** `ex_shopping_cart.gg` and
  regenerate its MANIFEST row REJECTED→WORKS; rule explicitly on its now-near-duplicate companion
  `ex_shopping_cart_total.gg`, which exists only to work around the rejection.
  ⚠ **THE MANIFEST's own `actual` field is STALE** — records 4 errors, measured **10**. Regenerate, never copy.
  ⚠ **HARD SEQUENCING inside the track:** `integration.rs::format_expr_canonical` carries an `Expr::It` arm
  labelled *"LANE PARITY, not a spelling preference"* — the Rust and self-host canonical printers must emit
  identical text, so the Rust arm and the self-host `format.gg` arm come out **in the SAME commit** or
  `parser_comparison`/`resolver_comparison` diverge. ⚠ `self_host_parser/` and `self_host_resolver/` carry
  their OWN independent `parser.gg`/`ast.gg`/`format.gg`/`lexer.gg` — **four parser implementations**; 12
  paths under `self_host_check/`/`self_host_lowerer/` are symlinks and follow automatically.
  ⚠ **THE SELF-HOST HALF IS SCOPED BUT NOT PROTOTYPED and is the LARGER half** — expect the four
  independent parser copies plus the `*_comparison` byte-identity constraint to dominate the effort.
  **DOCS:** `language-design.md` §7.1 (1550-1552), §7.5 table (1709, three tiers → two), **§7.6 DELETED**
  (1717-1737), comparison table 2197, strays 965/1578/1702/1726/1732 — ⚠ **line 2992 is Jest/Mocha
  `describe`/`it`, unrelated, LEAVE IT**; `book/05-collections.md:601-602`; `devbook/04-parser-ast.md` (3);
  `devbook/07-name-resolution.md` (1); `language-reference.md` (5); plus the 8 drifted `devbook/05-formatter.md`
  citations. ⛔ The D-ledger entry is an **owner ask — NO AGENT EDITS `decisions.md`.**
  ⊕ **The `d29_propagate_walker_arm_coverage` guard's message enumerates only *(a) routed* and *(b) arm
  deleted* — it has no case for *walker deleted entirely*. EXTEND the message, do not just lower the count.**
  ⚠ **CROSS-TRACK COLLISION WITH H:** both touch `known_gaps/hof_call_env_leak_unbounded`. **E owns the
  `.gg`** (rewriting `it` out — mechanically required); **H owns the assertion** at `integration.rs:6532`.
  Brief each on the other's exact region.
  ⭐ **ORPHAN FOUND, and the two scouts disagreed — ARBITRATED 2026-09-03, Scout E is right.** Scout H
  assumed `hof_call_env_leak_unbounded` was `t0953`'s repro; **NO todo item cites it**
  (`grep -rln hof_call_env_leak_unbounded todo/` → empty), and `t0953` actually cites
  `known_gaps/closure_literal_call_arg_env_leak.gg`. So it is an `#[ignore]`d leak repro with **no filed
  item** — a durable-repro-contract violation. **Track H files it from its own block.**
- **F · DRIVE THE TWO ROBUSTNESS CORPORA TO GREEN — ⚡ owner-selected. ⚠ RE-SCOPED BY ITS OWN SCOUT: the
  "WIRE IT" HALF IS ALREADY DONE.** Both corpora are ALREADY GATED IN CI — `.github/workflows/ci.yml:96`
  (`--lanes c,llvm`) and `:228` (`--lanes all`). All 288 `t0015` cells are in the manifest as topics
  `20/21/22 docs/*` (231+36+21, subtotals exact). **EXTEND `scripts/robustness_map.py`; do NOT build a
  second harness** — it already runs 5 lanes, gates cross-lane divergence, refuses `--accept` when
  regressions exist, and genuinely exits non-zero (`:762`, demonstrated RED in BOTH directions, including
  the wrongly-ACCEPTED direction `t0015` feared).
  **Regenerated at HEAD (map is green, so baseline == measurement):** `t0015` **239/288 = 83.0%** (exact
  match to the item) · book 202/231 · language-design 23/36 · language-reference 14/21. ⚠ **`t0018`'s
  corpus has DOUBLED since filing — 617/721 = 85.6%, not 354 cells @ 87.3%.** ⚠ **CORRECTED: whole map is 856/1009 = 84.8% under the map's own `good` rule** (my earlier *842/1009 = 83.4%* was inconsistent — 1009−842=167, not 153; state the rule with the figure). Thesis line *"one in eight"* is really ~1 in 6.6.
  **153 failing cells** (85 REJECTED · 32 WRONG · 25 BUILD-FAIL · 6 CRASH · 3 ICE · 1 TIMEOUT · 1 UNKNOWN).
  ⭐ **ESSENTIALLY ALL 153 ARE ALREADY HOMED at MECHANISM level** — this is CLOSURE work, not filing work.
  The three to spend the track on, costed end-to-end:
  - **M1 · the sort family, element-type axis — 7 cells, ONE root, and a SILENT NO-OP.** In one program:
    `Vector[int].sort(cmp)` ✅ · **`Vector[Person].sort(cmp)` → rc 0, NO diagnostic, LIST UNCHANGED** ·
    `Vector[String].sort(cmp)` → `undefined reference to 'int64_t__len'`. The comparator's param is typed
    `int64_t` regardless of element type. `sort(cmp)`→`sort_by` at `exprs/methods.rs:2170`; the synth bails
    at `lir/lower/insts.rs:3405` (`if is_sort && !sig_known { return None }`) to a TLS trampoline.
    ⊕ SIX QUESTIONS #6 was ASKED and answered: a descending comparator on `Vector[int]` gives `40/30/25`,
    so the int cell is genuinely right and the axis claim holds.
  - **M2 · `.iter()` adapter chains — 6 cells, all Core #10 lower-or-reject.** `VectorIter__int64_t__fold`
    / `__zip` undefined; `gorget_map_iter` implicit-declaration; `pair_joinwords_c_fold` **ICEs** at
    `ir/lowering/mod.rs:2144`. `t0167` names the family for self-host; **the Rust-lane `fold` failure is
    sharper than what is filed.**
  - **M3 · `==` without `Equatable` — RATIFIED AND UNIMPLEMENTED, no owner ask needed.** **D46 (2026-08-27)**
    already rules it: check-time rejection for structs/enums, intrinsic structural equality for tuples.
    ⚠ **`t0013`'s own headline is STALE** — D46's measured table says the current answer is *address
    identity, nondeterministic across lanes*, not "silently answers false".
  - **THEN `t0695`'s asked-for pipe + shrink-only BUILD-FAIL ratchet**, converting all 25 BUILD-FAIL cells
    into ONE guard (Core #6) instead of 25 filings — the item explicitly *"wants the PIPE, not 27 filings"*.
  ⭐ **SHARPEST SINGLE CELL, and the best fit for the track's thesis:** Book Ch4 § "Capturing Variables" is
  **unwritable in either spelling** — `(&count)():` is a PARSE ERROR (no such grammar) and without it the
  same example is `E_ReadWhileMutCaptured`. 5 cells. Whichever way the doc adjudication goes, a
  compiler-side change is required.
  ⊕ **The doc-defect half is real and costs no filing: 31 of 49 doc failures are `REJECTED`** — the docs
  teach code the compiler refuses (`float64 x = 1.5` → `E_TypeMismatch`; `m.get(k) ?? 0` →
  `E_DefaultOpRhsTypeMismatch` since `.get()` returns `Option[int &]`; `enumerate` → `E_UndefinedName`).
  ⚠ `language-reference` cells (7) are ADJUDICATIONS, not defects — it is written AFTER the code.
  ⚠ **SEQUENCING vs Track A:** `func_type_value` / `func_closure_reassign` (function-typed reassignment is
  a no-op) may belong to **A1/A2**. Scout F did NOT verify a shared root — CONFIRM before F takes them.
  ⊕ **`beginner_map` currently counts OPEN in the convergence classifier** because `t0018` cites the bare
  directory, not a `.gg`. Adding a `.gg` citation is a COUNTING CORRECTION, not a closure — do not bank it.
  ⊕ **Doc-block coverage, regenerated: 944 ```gorget``` blocks in `docs/`** (book 478 · language-reference
  272 · language-design 139) against 288 celled = **32% covered, 601 uncelled**. Celling all of them is a
  FOLLOW-UP track, not this one (needs an extractor + hand-derived expected output; ~3× the map's runtime).
- **G · EASE: PRUNE THE SHOWCASE + PRELUDE THE COLLECTIONS — ⚡ owner-selected.** `CLAUDE.md`
  § "Self-host as the elegance showcase" already MANDATES the prune and it has not happened: 822 `bool` +
  `match` + `else: pass` arms where `if d.kind is DkVariable():` exists · 356 `x = x + 1` where `+=` does
  · 228 copy-mutate-write-back `.set(` rituals where `v[i].x = 99` works in 21 fixtures · 4 `auto` in 97k
  lines against 450 in ordinary fixtures · sentinels (`-1` × 178 returns, `""` × 433 compares)
  outnumbering real `Option` (461). Plus: `Vector`/`Dict`/`Set`/iteration in the prelude, retiring 254
  hand-written import lines. ⚠ **Verify the "parallel vectors because Gorget has no tuple fields"
  workaround** (`self_host_lowerer/lower.gg:246-250`, `lir_ssa.gg:82-86`) before deleting it — if
  tuple-typed fields really fail it is a robustness FILING, if not it is a fossil (showcase rule 1).
- **✅ E · INTEGRATED — `4ffec3b36`, THE ROUND'S FIRST LANDING. Worktree pruned.** Squash-merged (4 commits)
  **together with the D54 ledger entry and its two spec citations**, so the ratified decision and its
  write-through are inseparable in history. Squash rather than merge because `13793b85b` was RED IN
  ISOLATION for `todo_index_is_current`; the tip was always green, and squashing kills the bisect hazard
  without rewriting the track's branch. **Gates at the integrated state, bare: `--test lints` 218/0/1 ·
  `--lib` 1181/0.**
  ⭐ **E's EXECUTOR VERIFIED THE MECHANISM I ASKED ABOUT WHEN IT COULD NOT SEE IT.** The D54 entry was
  uncommitted in the orchestrator worktree and invisible to E's branch, so rather than guess it
  **synthesised a local D54 entry, measured both states, and restored the file to its exact original md5**:
  declared-but-uncited → red at the **BUDGET** assert (not the reconciliation assert), declared+cited →
  green. **It then flagged that its own citation was INERT until my entry landed.** ⊕ It also rewrote both
  citation sentences out of fix-log voice into the timeless present, unprompted.
  ⚡ **THE TENTH LINT NOW GUARDS BOTH DIRECTIONS.** The reverse assertion (every `NO_MUTABLE_PATH` name must
  still be a live `ast.gg` variant) was **demonstrated, not asserted**: replanting `EIt`/`EImplicitClosure`
  makes it go RED naming both — **the state the entire `--test lints` run previously reported GREEN
  through**, which is exactly why those two names had to be found by grepping the guard.
  ⛔ **AND TWO OF MY OWN FOLDS WERE GUARD VIOLATIONS.** `figures_db_values_have_one_spelling` caught raw
  ratchet and floor figures I had written into this handover — twice — against the standing rule that
  **the handover stores INVARIANTS AND COMMANDS, NOT NUMBERS.** Replaced with the constant names and the
  commands that regenerate them.
  ⚡ **STANDING CONSEQUENCE, recorded so it is not rediscovered:** the uncited-decisions count sits **exactly
  at its budget with zero headroom**, so **every future ratification must ship its spec citation IN THE SAME
  COMMIT** or `ratified_decisions_are_cited_in_the_spec` reds. Raising the budget is never the remedy.
  ⊕ **`t0977` and `t0978` filed from E's block; `t0961`/`t0962`/`t0963` closed to `DONE.md`.**

- **✅ A1-M · INTEGRATED — `34ac3acab`, the round's FOURTH landing. Worktree pruned.** Closes `t0937`;
  files `t0968`–`t0972`; adds a **third repro** to `t0704`. Gates bare at the integrated state: build 0 ·
  `--lib` 1181/0 · `--test lints` **221/0/1**.
  ⚠ **THE MERGE NEEDED RECONCILIATION, NOT A SIDE.** Track E **deleted** an allowlist row and A1-M **added**
  one, so all three leak ratchets were recomputed from both: **`LEAK_CEILING` · `LEAK_CLASS_PAIRS` ·
  `LEAK_RECORDS`** all recomputed and mirrored in `figures.db` (read the values off `tests/lints.rs`;
  regenerate with the row's own `regen` awk census). ⊕ Then `figures_db_values_have_one_spelling` caught that
  my rewritten ledger comment **no longer re-spells the value**, making the R48 waiver dead — **retired
  rather than carried.** **Two guards, two catches, both on my own edits.**

- **⚖ OWNER RULING 2026-09-04 — THE LEAK ROW IS ADMITTED, GATED ON ALL THREE ITEMS.** *"I would also say
  admit the row, gated on all three items rather than `t0948` alone."* ⇒ A1-M's row stands with retirement
  requiring **`t0948` ∧ `t0971` ∧ `t0972`**; landing `t0948` alone only tightens it.
  ⚡ **CONSEQUENCE THE EXECUTOR PRE-RECORDED, NOW LIVE:** A1-I kept its six `closure_identity/` fixtures out
  of the top-level sanitize scan **only** to avoid putting a second identical question to the owner while
  this one was pending. **That reason is now spent.** Its manifest row's SECOND trigger fires — the row is
  **DELETED, not waited on**, and moving them top-level buys back the continuously-enforced SH parity gate.
  **Owed as a follow-up, not a blocker: one `git mv`, six `⚖ ADMITTED` rows citing `t0953`, the path prefix
  off six `run_gg` calls.**

- **⚖ OWNER ASK 2026-09-04 — "close the drop side this round? or too much work?" ANSWERED WITH THE GATE.**
  ⛔ **`t0948` IS NOT A THIS-ROUND FIX, and its own HARD GATE says why** (added by A1-M, measured):
  *"FLIPPING THIS PREDICATE ARMS A DOUBLE-FREE ON EVERY PLAIN `Callable`-FIELD READ, AND R49 TOOK THAT
  SURFACE FROM ONE SPELLING TO FOUR."* **The defect is on the READ side** — `Callable[int(int)] g = h.f`
  with no `.clone()` binds the field's `GorgetClosure` by value without materializing, so `main` and
  `Holder__drop` free the same region. ⇒ **it needs a read-side materializer that does not exist. That is a
  CoW-layer design track, not a fold.**
  ✅ **`t0971` + `t0972` ARE tractable AS ONE TRACK** — both are enum-payload drop-EMISSION defects, both
  with durable RED repros **already committed by A1-M**, and **`t0972` is GENERAL, not `Callable`-specific**,
  which widens its value. ⚠ **But both are MED leaks — LAST on the owner's severity ranking — so CRITICALs
  outrank them for the same capacity.**

- **⚖ OWNER DIRECTION 2026-09-04 — BRING MORE CRITICALS IN BEFORE ROUND CLOSE. THE SIZING HOLD IS LIFTED.**
  **The CRITICAL set is TEN** (`grep -l 'severity = "CRITICAL"' todo/*.md`, not a selection): `t0011` ·
  `t0036` · `t0045` · `t0680` · `t0697` · `t0703` · `t0704` · `t0709` · `t0771` · `t0988`.
  ⭐ **SEVEN OF THE TEN FALL INTO TWO CLASSES, which is the Core #4 leverage:**
  **CLOSURE-CAPTURE — ⚠ CORRECTED 2026-09-04 BY THE L SCOUT: it is TWO, not three, and one of my three
  WAS ALREADY FIXED.** `t0704` · `t0771` are **ONE class, ONE write site, TWO arms** (ablation-proven).
  ⛔ **`t0703` IS FIXED** — both repros are LIVE, non-`#[ignore]`d tests passing at HEAD on C and LLVM,
  closed by `e7967d570` (2026-08-29) which **moved them out of `known_gaps/` and edited the item WITHOUT
  CLOSING IT**; its own cited fix site `resolve_collection_identity` **exists only because that commit
  created it**, and **neither repro contains a closure.** ⇒ **the CRITICAL set is NINE, not ten** — a captured/aliased handle is a BORROW, so a source
  realloc or a scope exit frees it under the closure. ⊕ **This round already sharpened `t0704`'s scope**
  (the class is wider than "collection" — a captured plain `String` corrupts the same way).
  **BOX / TRAIT-OBJECT (4):** `t0011` (`Box[T](struct.field)` shallow copy) · `t0680` (live miscompile,
  silent wrong on C + hard `llc` error on LLVM) · `t0697` (root measured by K's scout:
  `pack_trait_object_for_smart_ptr_ctor`, `calls.rs:1038-1041` — assign + `set_owned_fresh`, **never
  consumes the source**) · `t0709` (`Vector[Box[Trait]]` from a helper, rc 139).
  **STANDALONE (3):** `t0036` (plain READ of safe syntax) · `t0045` (`for x in &coll` + assign) ·
  `t0988` (⭐ **corrected THREE times this round by F and its axis is FINALLY right — unusually
  well-prepared, cheap leverage**).

- **✅ A1-I · INTEGRATED — `935863366`, the SEVENTH landing.** Files `t1052`/`t1054`/`t1055`; re-scopes
  `t0681`; enriches `t0774` instead of filing a duplicate. Gates bare: build 0 · `--lib` 1185/0 ·
  `--test lints` **224/0**. ⭐ **It deletes ONE OF THE FIVE name-keyed sidecars** — the only one this round
  could reach; `fn_sigs` (238 refs) · `fn_param_abis` (52) · `fn_param_ownerships` (44) ·
  `callable_alias_sigs` (7) **survive and are A2's root.**
  ⛔ **AND ITS REVIEW CAUGHT CONFLICT MARKERS I HAD COMMITTED** into `TODO.md`'s generated index at
  `ef171a34a` (Track F's integration). **`t1066` filed for the class:** planting `<<<<<<< HEAD` leaves
  `--test lints` **GREEN**, and **no lint, script or CI step looks for markers in ANY file type.** ⚠ **In
  `.rs` a marker fails the build loudly; in `.md`/`.tsv`/`.txt`/`.db` it is SILENT** — and `MANIFEST.tsv`,
  `LEAK_ALLOWLIST.txt`, `CORPUS_MANIFEST.txt` and `figures.db` were **all conflict-prone this round and are
  all parsed leniently.**
  ⚡ **SECOND LIVE INSTANCE OF THE SAME SIGNAL, same session:** the index generator printed
  **`OK — 844 item(s), 841 pointer(s)`** — a 3-row mismatch **next to the word OK** — and a second pass gave
  844/844. **That is `t1066`'s second half: make the generator FAIL rather than skip.**

- **⭐ N · SCOUT RETURNED — THE WRITE SITE IS FOUND AND `find` IS NOT ALONE. Brief written; pass 1 launched.**
  **`bir/lower.rs:1946` `Memcpy`s a BORROWED element pointer into an OWNED, DROPPABLE `Option[T]` payload**
  — Core #3, the ownership tag mis-typed **at the value's birth**; ASan names the alloc/free/free triple.
  ⭐⭐ **THE REFERENCE-GRADE SHAPE ALREADY EXISTS TWO METHODS OVER:** `v.get(0)` yields
  **`Option[Ref[T]]`** — bare `void*` payload, **NO drop emitted**, consumer clones at the read. **The tree
  already distinguishes owned from view; `expand_find` DECLARES ONE AND FILLS IT LIKE THE OTHER.**
  ⛔ **SIBLING CLASS, witnessed by a REPO-INTERNAL CONTRAST:** Dict/Set expanders use `_new_like` +
  `put_cloned` and are clean; the Vector ones use `gorget_array_new` + a bare push. ⭐ **There is NO
  `gorget_array_new_like` while `MapNewLike` and `SetNewLike` BOTH EXIST — that asymmetry IS the class**
  (the result array is minted with `elem_drop`/`elem_clone`/`elem_materialize` **all NULL**, Layering
  rule 1). ⛔ **`expand_filter`'s local cleanliness is ACCIDENTAL — the ESCAPE shape is a measured
  heap-use-after-free on BOTH backends**, and **the self-host gets it right.**
  ⛔ **`t0988` IS STILL WRONG IN EIGHT WAYS.** The worst: **on LLVM, five cells C REJECTS build — and one
  PRINTS A RAW POINTER at rc 0 with `gg check` clean.** That is **severity #2, ABOVE the leak the item
  trades against, recorded as a benign build failure.** ⊕ **The typechecker is RIGHT — the disagreement is
  introduced AFTER typecheck, in lowering** (Layering rule 4). ⊕ **The `int` cell is ACCIDENTALLY CORRECT
  TWICE — the FOURTH instance of the failure mode behind this item's three earlier wrong discriminators.**
  ✅ **AND ITS "no green control exists" IS REFUTED:** `v.get(0)` is green **for the right reason**,
  closure-free so `t0953` cannot touch it. **The true, narrower claim is that no CLOSURE-TAKING cell can be
  ASan-gated while `t0953` is live** — a scheduling constraint on the assertion, not an absence of controls.
  ⛔ **ggdef CANNOT ADJUDICATE** (`.find()`/`.filter()` outside phase-0) ⇒ **a subset gap is owed**, and
  Core #13's *"ask ggdef first"* returns NO-VERDICT.
  ⚡ **GUARD: make the misuse FAIL TO COMPILE** — newtype `BorrowedElemPtr` with a private field, so
  `src_ptr: ctx.elem_ptr` **stops compiling** at exactly **5 consumer sites**. **A textual ratchet is the
  wrong instrument here; one was evaded by a `const` hoist this round.**

- **📋 R49 ROUND-CLOSE CHECKLIST — STAGED NOW so it runs MECHANICALLY, not from memory.** ⛔ **Every leg
  AFTER A1-I integrates**, on the integration branch, **every rc read off the BARE command.**
  1. `scripts/convergence.sh` — **FIRST, before the sweeps** (owner 2026-08-06: a fail means fix or ask
     BEFORE burning ~30 min). Quote its line into the `DONE.md` entry — **a MEASUREMENT, not a gate.**
  2. **C sweep:** `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600 scripts/run_integration.sh` — **both
     knobs, the wrapper, never a hand-rolled thread count.**
  3. **THEN the LLVM sweep — SEQUENTIALLY, NEVER in parallel with the C sweep** (owner 2026-07-29: linker
     thrash + `/tmp` scratch collisions).
  4. **The cargo targets `--test integration` never touches:** `-p ggdef` · `--test spec_conformance` ·
     `--test security` · `--test lints` · **`--test c_runtime`** · `--lib`.
  5. **The two SCRIPT gates NO cargo target reaches** (added by H this round):
     `scripts/known_gaps_census.sh --check` and
     `GG_STAGING_MOVE_GUARD=fatal scripts/staging_move_burndown.sh --check`.
  6. `scripts/sanitize_sweep.sh` (~25 min). 7. `python3 scripts/robustness_map.py` — **all five lanes.**
  ⚠ **CARRIED OBLIGATIONS, each owed by a specific track:** H's two new top-level fixtures are parity-corpus
  inflow and **the floors are RELEASE-ONLY — confirm the release `self_host_runtime_diff` line** · K's SH
  port must still show **no new nondeterministic rows** · A1-I's `closure_identity/` MATCH is **a
  measurement, not a gate** (by ruling) so it will NOT appear in the sweep — **re-run it by hand or say it
  was not re-run.**
  ⛔ **A RED BATTERY IS NEVER WAIVABLE.** ⊕ Then: `DONE.md` entry · handover rewritten IN PLACE
  (pending-only, **invariants and commands, never numbers** — that rule caught me three times this round) ·
  prune worktrees and `/tmp` · **report to the owner BEFORE opening R50.**

- **⚡ A1-I · FOLLOW-UP LANDED (`2de5d0f06`, RECORDS ONLY) — the in-flight output-review was WARNED its base
  moved.** ⚠ **A branch moving under a live reviewer is a real hazard and it nearly happened silently.**
  ⭐ **AND THE EXECUTOR RECORDED A DISTINCTION I DID NOT ASK FOR AND SHOULD HAVE.** The `OUT` row now carries
  **TWO triggers, not one**: the row **retires** when `t0953` is fixed, **and SEPARATELY the placement
  becomes WRONG** if the owner's ruling on A1-M's pending ask admits rows of this shape — in which case
  **the row should be DELETED, not waited on.** ⇒ *"so a future reader doesn't have to wait for `t0953` to
  conclude the placement is stale."* **Conflating a retirement condition with a re-evaluation trigger is
  exactly how a stale row survives a ruling that invalidated it.**
  ⊕ Cost recorded in **two durable places**, not just the test comment: the `CORPUS_MANIFEST.txt` `OUT` row
  and a **new `tests/fixtures/closure_identity/README.md`** (precedent `sanitize_selftest/README.md`), the
  latter with a **"Moving them top-level"** section listing what a mover owes — six `⚖ ADMITTED` rows citing
  `t0953`, deletion of the manifest row, and the path prefix off six `run_gg` calls.
  ⊕ **`t1055` filed with the framing FIRST and in caps — LANE DEBT, NOT A SOUNDNESS LAG** — plus two claims
  **a Rust-side reader would miss**: `lir_codegen.gg:1468` is a **verbatim mirror of the `optimize.rs` DCE
  seed retired one commit earlier**, and **`__adapt_` has NO Rust counterpart at all**, so **no Rust-side
  census could ever see it.** ⊕ Its own ratchet is labelled **bookkeeping, not a class retirement**, citing
  the Rust twin's `const`-hoist evasion as the demonstration.
  **Gates after the follow-up:** `--test lints` **222/0/1** · targeted integration 6/0 · index clean.

- **✅ F · INTEGRATED — `ef171a34a`, the SIXTH landing.** Files `t0990`–`t0993`; corrects `t0987`, `t0988`,
  `t0068` in place. Gates bare: build 0 · `--lib` 1184/0 · `--test lints` **222/0**. ⚠ Worktree
  harness-LOCKED at merge; prune when it releases. ✅ **G's BLOCKER IS DISCHARGED** — F shipped the
  `for_each`-without-import RED cell that pins `t0987`'s trigger.
  ⭐⭐ **ITS EXECUTOR CLOSED WITH A SCOREBOARD ON ITS OWN ERRORS, AND IT NAMES THE ROUND'S SIGNATURE FAILURE:**
  *"I was wrong three times, and each time the same way — **HOLDING AN AXIS CONSTANT WHILE VARYING
  ANOTHER**. The `_full`-vs-2-arg witness (checked C, missed LLVM), the `t0993` attribution (dates instead
  of measurement), and now `Set.find` (checked `check`, not `build`). All three were caught by review, none
  by me. **The countermeasure that actually worked was MECHANICAL ENUMERATION FROM A TABLE, not more careful
  probing.**"* ⇒ **which is why `t0987`'s fifth correction is a SET DIFFERENCE** — `calls.rs`'s three
  per-family exclusion arms minus the bare `equip` blocks gives the fabrication set in **four methods, one
  grep.**
  ⭐ **AND IT FIXED THE GUARD'S SCRAPE RATHER THAN NOTING IT** — the naive version was **dropping `fold` and
  `map` from BOTH equip blocks entirely**, leaving those rows standing on the HofOp arms alone: **exactly
  the single-witness dependency the guard exists to remove**, and exactly what `iter.gg:396` plans to
  retire. **RED-verified against the `("Set","map")` evasion.**
  ⊕ It **applied the Core #5 rule the same hour it was added** — rebuilding from the committed tree
  *between* `cargo test` and the map run — and fixed **two further stale cites** in `t0068`'s body, staled
  by its own 266-line insertion.

- **⚡ A1-I · EXECUTOR RETURNED (`fc3c3c23a`) — OUTPUT-REVIEW LAUNCHED.** Three typed carriers;
  **28 of 29 sites changed, 1 deliberately kept**, and **`grep` for `__Closure_`/`__call` predicates in
  `src/` now returns ZERO.** Gates bare: build 0 · `--lib` **1182/0** · `--test lints` **222/0/1** ·
  `-p ggdef` 187/0 · `spec_conformance` 3/0 · `security` 213/0 · targeted **C 479/0 and LLVM 485/0.**
  ⭐ **CARRIER #1 GREW TO CARRY THE CAPTURES RATHER THAN RE-KEYING THE MAP — so the name-keyed sidecar is
  DELETED, NOT RELOCATED** (Layering rule 3). Its third component needed no home: `register_named` interns
  **one id per name**, so it **is** the id whose metadata this is.
  ✅ **THREE-STATE DEMONSTRATION ON ITS OWN BUILD:** pristine **RED** → carriers **GREEN (all ten shapes
  matching ggdef on C and LLVM)** → const-hoist evasion **RED with `--test lints` GREEN and the ratchet
  still 30 == 30.** ⊕ And it states honestly that **the `assert_eq!` probe still compiles and fires** — the
  carriers add a typed route, they do not make the old fact unavailable — **so both ratchets ship EXPLICITLY
  LABELLED BOOKKEEPING** and `-D dead-code` is called an accident, never a guard.
  ⭐⭐ **IT REFUSED AN INSTRUCTION OF MINE AND WAS RIGHT: `t1053` WOULD HAVE BEEN A DUPLICATE.** `todo/t0774`
  already owns that subject **in its own words**, down to naming `Instruction::CallIndirect` as the unused
  typed representation and `call_indirect_tracked` as the migration point. **I told it to file; the
  GREP-BEFORE-YOU-FILE rule says don't; it followed the rule over the instruction** and enriched `t0774`
  with A1-I's measurement instead. **`t1053` returns to the pool.**
  ⚡ **PLACEMENT RULED: the fixtures STAY in `tests/fixtures/closure_identity/`.** Their env leak is
  **`todo/t0953`, measured identically on the PRE-FIX compiler**, so it is not A1-I's inflow — and six rows
  on a **shrink-only** allowlist would put a **second** identical leak question to the owner while A1-M's is
  still pending. ⛔ **THE COST IS RECORDED, NOT WAIVED: out of the top-level scan is out of
  `runtime_parity_corpus` too, so the SH MATCH is A MEASUREMENT, NOT A CONTINUOUSLY ENFORCED GATE.**
  Reversible with one `git mv`; revisit if the owner's A1-M ruling admits rows of this shape.
  ⚡ **`t1055` ORDERED: the self-host's OWN name-match debt** — 12 predicates including a **verbatim mirror
  of the DCE seed just retired**. ⚠ **The SH is CORRECT on the miscompile; this is name-match debt only.**
  ⊕ **`t0681` RE-SCOPED, NOT CLOSED** (its `Box[Callable]` deref half is still live, now wired;
  `ALLOWED_UNWIRED` 30 → 29), and **`box_callable_call_through_box_undefined_function` still ICEs and was
  NOT claimed** — a `Callable`-typed struct FIELD has no closure env, so the carve-out correctly does not
  fire.

- **✅ K · INTEGRATED — `5d2ff2213`, the round's FIFTH landing and the CRITICAL the owner assigned to R49 BY
  NAME. Worktree pruned.** Closes **`t0871`**; files `t1048`–`t1051` and `t1064`. Gates bare: build 0 ·
  `--lib` 1181/0 · `--test lints` **221/0**.
  ⭐ **THE EXECUTOR CAME BACK BETTER THAN ITS ERRATA ASKED, THREE TIMES:**
  1. **It made the fixture split SELF-DEFENDING rather than merely correct.** Splitting the `.insert()` cell
     recovers three ggdef adjudications — **but nothing goes RED if someone tidies it back together**, so
     the *"do not re-merge, it costs three adjudications and nothing goes red"* warning sits at **FOUR**
     sites: fixture header, ratchet row block, test docstring, `DONE.md`.
  2. **It re-measured the refuted cost claim at TWO path lengths, not one** — `7`/`7` and `67`/`67`,
     identical for both spellings — **and rather than just deleting the false sentence it KEPT WHAT THE
     FIXTURE ACTUALLY HOLDS: the clone count scales with path LENGTH, an ALGORITHM defect no spelling
     migration was ever going to fix.**
  3. ⭐ **`t1064` CAME BACK SPLIT IN TWO.** I handed it 9 broken repro paths; it re-derived **17 of 247**, in
     two sub-classes a naive `exists()` guard would blur — **9 GENUINELY MISSING** (the real debt) and
     **8 MERELY MIS-SPELLED** (the file exists; the path omits `tests/fixtures/`). ⊕ And
     `self_host_cited_fixture_paths_resolve` **already IS that guard and already tolerates both spellings —
     only its POPULATION needs widening**, which is why the mis-spellings never surfaced.
     **Separating them stops a rename being counted as missing evidence.**
  ⊕ It also **declined one erratum for the right reason** (a stale distance in an already-committed comment
  would have needed the amend I forbade) **and flagged it instead of silently leaving it** — **I corrected
  it in `DONE.md` at integration; no source comment carries the phrase.**

- **✅ K · OUTPUT-REVIEW: INTEGRATE AFTER THREE ERRATA. All three mandatory gates PASS; the fix is called
  REFERENCE-GRADE.** Errata sent; merge on their return.
  ⭐⭐ **THE SH PORT WAS NECESSARY — PROVEN THREE INDEPENDENT WAYS, so "already correct / one-lane fix" was
  FALSE BY CONSTRUCTION.** (1) **Source:** `lower.gg:2202-2207` folds `LoBorrowed` and `LoView` into ONE
  bool and Branch A at `:2251` returns `BorrowAlias()` for both, **while Branch E declared itself dead
  BECAUSE *"the design intent is to alias, not materialize"***. (2) **Owner rule:** all eight fixtures are
  top-level ⇒ auto-scanned, so the no-own-inflow rule owes the SH lane the same round **regardless of
  pre-existing state**. (3) ⭐ **MECHANICAL — the decisive one:** the `NEW NONDETERMINISTIC row(s)` assertion
  at `integration.rs:40610` is **exactly the gate the executor reported firing pre-port**, and it is **green
  post-port with NO allowlist edit and NO parity constant touched.** ⇒ **A "NO PORT OWED" LANDING WOULD HAVE
  BEEN RED.**
  ✅ **The cost claim holds BY CONSTRUCTION**, traced through `cow_source_root_name` peeling
  `EIndex`/`EFieldAccess` to the root and `cow_mark_assign_target` marking only assign targets ⇒ **zero
  clones where nothing is mutated.** ✅ **The equivalence guard FIRES IN BOTH DIRECTIONS**, reproduced at
  **`(0,100)`** and **`(100,0)`**, with the for-element cell going four-empty-lines.
  ⛔ **ERRATUM 1 — THE ggdef ADJUDICATION IS OVERCLAIMED ON EXACTLY THE FOUR NOVEL CELLS.** `.insert()` is
  out of the phase-0 subset and **elaboration fails for the WHOLE FILE**, so **A5 (field assign), A6
  (`Vector.set`) and A7 (`v[i] =`) get NO adjudication either** — **8 of 12, not 11.**
  ⚠ **Why it matters more than a number: A5–A8 are the round's most novel finding, and as recorded they rest
  on "both backends agree" ALONE — which Core #8 calls NECESSARY, NOT SUFFICIENT — while the record says the
  DEFINITION backs them.** ⚡ **Two-minute remedy, verified: split the `.insert()` cell out and three cells
  convert to ggdef-adjudicated.**
  ⛔ **ERRATUM 2 — CORE #14 ROT THIS DIFF ITSELF CREATES** (Core #15(d)): a `known_gaps` fixture still says
  it *"is allowlisted in `no_dot_slice_after_d22`"* — **which this diff DELETES** — and asserts the colon
  form *"materializes NOTHING — 1,002 vs 2"*, **measured at this HEAD as `string_clone=65`, identical to
  `.slice()`** ⇒ **its stated conclusion is INVERTED.** `t0316`/`t0850` still call the deleted lints
  "live and SUSPENDED".
  ⛔ **ERRATUM 3 — A FABRICATED SYMBOL INSIDE A NEW CORE #14 COMMENT:** `assigns.rs:2088` cites
  `check_index_assign_target`, which **exists nowhere in the tree**; the real function is
  `check_index_mut_assign`. **Substance verified sound.**
  ⚡ **FILED, NOT FIXED → `t1064`: `todo/` `repro` PATHS ARE UNGUARDED.** `t0871`'s cited repro **never
  existed on any branch**, and a scan of all **247** `repro` paths finds **9 MORE BROKEN across 8 items**
  (`t0106` ×3, `t0124`, `t0126`, `t0311`, `t0387`, `t0591`, `t0633`). The existing guard covers **only**
  `tests/fixtures/self_host_*`.
  ⊕ **Noted, no work:** the ratchet's header claims *"every {producer, consuming position} pair"* while the
  String family has **no rows** for `push`/`put`/`send`/`returns`/`captures` — **all measured SOUND at the
  pre-fix compiler**, so they need a **named non-claim row**, not fixtures (**SIX Q#3, the error its own
  header calls out**). ⊕ `bind→return` is a third `t0871` shape the matrix does not pin — same root, no
  hole, but **Core #12 says NAME each omitted cell.**

- **✅ F · CONFIRMING REVIEW: INTEGRATE. THE EXECUTOR WAS RIGHT AND MY DELETE ORDER WAS WRONG.**
  Independently confirmed: `lib/std/iter.gg:818` is a bare `equip [T] Set[T]:` block declaring `each`,
  `for_each`, `any`, `all`, **`find`**, **`find_index`**, `fold`; `Set[String].find(…)` is `gg check` **OK
  WITH the import** and `E_NoMethodFound` **WITHOUT**. **Only `Dict.update` was fabricated.**
  ⭐⭐ **AND THE PER-FAMILY GUARD WAS PROVEN BY AN EVASION NEITHER OF US THOUGHT OF.** The reviewer's own
  third attempt used **`("Set","map")` — decisive, because `map` is BOTH a Vector `HofOp` arm AND in the
  Vector `equip` block, so A FLAT UNION WOULD HAVE GREEN-LIT IT.** The shipped guard catches it and the
  HashSet twins. ⇒ **the per-family design was not a preference, it was necessary.**
  ✅ **The `--accept` fix confirmed by count: DIVERGENT 155 → 157 → 157 (ZERO erasures); empty-LLVM
  baselines 1 → 24 → 1** (the survivor is the pre-existing positive control). **The bad run was indeed not
  committed.**
  ⛔ **ERRATUM · `t0987` HAS NARROWED A FIFTH TIME — AND THE ENUMERATOR WAS ALREADY IN HAND.** The fourth
  correction says *"Measured, `Set[String]`, EVERY CELL"* and marks **`Set.find` ✅ "check OK"** — measured,
  that program **BUILD-FAILs** with `implicit declaration of 'gorget_set_find'`, **in a column where every
  other ✅ means "builds"**; and **`Set.find_index` is absent and also fabricates.**
  ⭐ **`calls.rs` HAS THREE per-family exclusion arms** (`gorget_array` `:356-363`, `gorget_map` `:410-411`,
  **`gorget_set` `:435` listing only `filter|fold|each|any|all|map`**) ⇒ the Set fabrication set is
  **`for_each` ∪ `find` ∪ `find_index`, derivable in ONE GREP.** ⛔ **The item's OWN rule — "enumerate from
  that exclusion arm, never by analogy" — WAS APPLIED TO THE ARRAY ARM ONLY.**
  ⊕ **Scope errata:** three cites regenerated against the **PRE-diff** file (right at base; **the same
  commit inserted 266 lines above them**) · the census figure has **THREE spellings on one branch**
  (672 base / 671 mid / **673** at branch HEAD — its own cells moved it) · S5 fixed the MANIFEST note but
  **not the fixture** · the lint's `equip` scrape mis-parses multi-generic decls (**harmless today, but
  `iter.gg:396-401` PLANS deleting the builtin HOF entries**, which would red it).
  ⚠⚠ **A PROCESS FINDING THAT INVALIDATED THE REVIEWER'S OWN FIRST RUN, NOW A RULE:** **`cargo test`
  REBUILDS `target/debug/gg`**, so RED-verification edits it had made *and reverted* were **baked into the
  binary the robustness map then measured.** It killed that run and rebuilt from committed source.
  ⇒ **AGENTS.md Core #5 SHARPENED: a measurement must be taken against the binary the COMMIT builds.**

- ⚠ **AND I WROTE A COVERED FIGURE INTO THIS HANDOVER FOR THE THIRD TIME** — `figures_db_values_have_one_spelling`
  caught it again. **The guard is faster at auditing me than I am.** Constants cited, values removed.

- ⚖ **ROUND SIZING — I AM NOT OPENING AN EIGHTH TRACK, AND THE REASON SHOULD BE ON THE RECORD.** R49 carries
  **SEVEN** tracks (E · H · C · A1-M landed; K · F · A1-I in their final gate), above the owner's 4-6
  default. Each queued track (**F-2 · F-G · D · G · B**) costs a full scout → ≥3 passes → executor →
  output-review cycle, and the box is the constraint — the A1-I scout **measured** a full sweep autoscaling
  to one thread under load and had to kill it. **Opening an eighth now roughly doubles the remaining tail
  for no extra throughput.** ⇒ **Drive K, F and A1-I to integration, then the round-close battery.**
  ⊕ **The queued five carry forward intact**, with G's discharge already named (F's `for_each` RED cell) and
  B unblocked since E landed.

- **✅ A1-M · CONFIRMING REVIEW: INTEGRATE. THE LEAK BLOCK IS FIT FOR THE OWNER.** Two closing errata sent;
  merge on their return.
  ⭐ **THE REVIEWER RE-MEASURED ALL SIX SHAPES FROM SCRATCH, ON BOTH LANES, WITHOUT USING THE EXECUTOR'S
  NUMBERS — every v3 row EXACT.** ⭐⭐ **AND IT REPRODUCED THE FALSE ACQUITTAL ITSELF:** short-literal twins
  of **both** leaking shapes read **CLEAN** on C. **So the stated mechanical cause was not merely plausible
  — it was SEEN FIRING.**
  ⚡ **ITS VERDICT ON WHY THIS VERSION IS DIFFERENT, and it is the reason the ask can go up:** *"the previous
  two versions read as authoritative because they asserted a SCOPE on the strength of a CONTROL NOBODY
  RE-DERIVED. v3 does not do that … it now carries the FALSIFIER — the short-literal control — as a STATED
  RULE rather than an anecdote, so the specific failure mode that produced v1 and v2 is CLOSED rather than
  merely apologised for."* ⊕ And on keeping both corrections visible: *"the owner is ruling on inflow
  attribution, and the reasoning's HISTORY is part of what he is ruling on."*
  ✅ **ALL SIX PACK SITES NEUTRALISED BY LINE:** 1, 2, 3, 4, 6 → rc 139; **site 5 → rc 0, stdout
  byte-identical, LEAK PROFILE UNCHANGED** ⇒ **neither stdout, exit code NOR allowlist pins it** — *"the
  lint is the only thing pinning site 5"* is exactly true. ⭐ **The PEEL warning is LOAD-BEARING** —
  neutralising the `expected_type` override takes the fixture to rc 139.
  ⭐ **AND IT BACKS THE EXECUTOR'S REFUSAL TO DELETE SITE 5, in stronger terms:** deleting it would trade a
  real invariant for a cosmetic one, because **relying on site 3 to catch the prelude payload is PRECISELY
  THE DOWNSTREAM-REPAIR SHAPE CORE #1 EXISTS TO FORBID**, leaving the class one refactor from silent
  regression.
  ⛔ **ERRATUM 1 · "41 characters" IS OFF BY ONE** — the literal is 41; the **runtime value is 42** (43 B =
  42 + NUL), in **six** places. **Cosmetic anywhere else; not in a block whose entire authority is
  measurement precision, on its third revision, that the owner is about to rule on.**
  ⛔ **ERRATUM 2 (real coverage) · `t0972`'s CLEAN LOCAL-SCRUTINEE CONTROL LIVES ONLY IN PROSE.** It is the
  discriminator that makes the item's axis meaningful and **it is not a cell** — **wire it, or a future fix
  that regresses the local form goes uncaught** (Core #12).
  ⚠ **AND MY OWN SCOPE INSTRUCTION TO THAT REVIEWER WAS WRONG** — I gave the range one commit too early, so
  `src/` is **not** empty over the span I named. The reviewer caught it, checked the **actual** range, and
  proceeded. **A scoping instruction is a load-bearing claim too.**

- **⚡ K · EXECUTOR RETURNED (`a4f63e55a`, `96f88408b`) — OUTPUT-REVIEW LAUNCHED. AND THE BRIEF WAS WRONG
  ABOUT THE BIGGEST THING.**
  ⛔⛔ **"SELF-HOST: ALREADY CORRECT … NO PORT IS OWED" IS FALSE — and §5, ADDENDUM 4 S-E and ADDENDUM 5 ALL
  REST ON IT.** The SH **does** tag both producers `LoView()` as claimed — **but
  `decide_svardecl_emission`'s Branch A (`self_host_lowerer/lower.gg:2210`) FOLDS `LoView` AND `LoBorrowed`
  INTO ONE `source_is_borrow_alias` STATE** and returns `BorrowAlias()` for both, **while Branch E carried a
  comment declaring ITSELF DEAD** because *"the CoW-default-borrow design intent is to alias, not
  materialize."* ⭐ **The distinction that comment misses: a `LoBorrowed` points into an aggregate whose
  owner OUTLIVES the bind; a `LoView` is a cap=0 header over a buffer THE SOURCE CAN REALLOCATE.**
  ⇒ **All four index/slice bind cells printed NONDETERMINISTIC GARBAGE on the SH lane — 7 DISTINCT OUTPUTS
  OVER 7 RUNS — and `self_host_runtime_diff` FAILED with a NEW NONDETERMINISTIC ROW.**
  ⚡ **It PORTED rather than stopping**, because the owner's 2026-08-10 rule leaves no other move (excluding
  the fixture is the gate's own named *"forbidden parity-inflation"*), **and the fix uses the SELF-HOST'S OWN
  TIER-2 IDIOM so it costs ZERO where nothing is mutated** — a view aliases only while both the bound name
  and the source root are `cow_pristine_after` the bind, **the same test the `GtPtr`-to-resource branch
  twelve lines below already applies.** Both heavy SH gates re-run clean afterwards.
  ✅ **THE EQUIVALENCE GUARD (owner ruling) SHIPPED AND WAS SEEN RED IN BOTH DIRECTIONS.**
  `no_dot_slice_after_d22` and its walker **DELETED**; `slice_spelling_cost_equivalence` asserts both
  spellings cost `0` at a temp and `LOOP_N` at a bind — **`assert_eq!`, not `<=`** — and dropping one
  producer tag at a time (**line-anchored**, since the two `set_view_of` calls are near-identical) gives
  **`(0,100)` and `(100,0)`.**
  ⭐ **CORE #14: SIX COMMENTS, NOT FIVE** — a sixth found in `emit_call_extern.rs` repeating the same refuted
  claim. **One was GUARDED rather than deleted:** `assigns.rs:2102` got a `debug_assert!` that a String base
  never reaches index-assign lowering — **the comment's claim is TRUE for collection elements and FALSE for
  a String base, and nothing said so.**
  ⊕ **Filings `t1048`–`t1051`, each with a durable repro** — `t1048` **wired to read `live_bytes`, NOT
  stdout, because ASan is blind** · `t1049` bytes-vs-codepoints · `t1050` **re-measured: TWO unguarded
  inline helpers, both with DEAD GUARDED DEFINITIONS in the emitted C** · **`t1051` — the reference now lags
  the self-host ON COST (100 clones vs 0 on the same program).**
  ⚠ **No `known_gaps/string_index_slice_bind_dangles.gg` was created** — the defect is fixed, so an
  `#[ignore]`d test asserting correct output would be **a dead row**; the repro **graduated into four live
  top-level fixtures** instead. The review adjudicates.
  ⚠ **Brief errata pushed back on:** ADDENDUM 5's *"`context.rs:3983` cites a devbook section that does not
  exist"* is **FALSE** — `docs/devbook/11-copy-on-write.md:1240` is `#### View-producer enumeration rule`
  (doc-write-through'd) · `check_expr.rs:868` not `:869`.
  **Gates bare:** build 0 · `--lib` **1181/0/0** · `--test lints --release` **220/0** · `-p ggdef` 187/0 ·
  `spec_conformance` 3/0 · targeted string/cow/slice on **C and LLVM** · **`self_host_runtime_diff --release`
  ok (MATCH 1541, ADJ-MATCH 482, BOTH-WRONG 2 unchanged, NO new nondeterministic rows)** ·
  **`self_host_bootstrap_fixed_point --release` ok.**

- **⚡ F · FOLD LANDED (`537d95628`) — CONFIRMING OUTPUT-REVIEW LAUNCHED. AND MY INSTRUCTION WAS HALF WRONG.**
  ⛔⛔ **THREE OF THE FOUR ROWS I ORDERED DELETED ARE REAL AND STAY — AND THE REVIEWER MADE THIS ROUND'S
  SIGNATURE ERROR.** `lib/std/iter.gg:818` is a bare `equip [T] Set[T]:` block declaring `for_each`, `find`
  and `find_index`. Measured: `Set[String].find((e): e.len() == 3)` is **`gg check` OK WITH the `std.iter`
  import** and `E_NoMethodFound` **WITHOUT** it. ⇒ **the reviewer probed only the no-import side — the same
  HOLD-ONE-AXIS-CONSTANT error `t0987` and `t0988` each made THREE times, on THE VERY AXIS ITS OWN S8
  FINDING WAS ABOUT.** **Deleting them would have removed WORKING METADATA on a FALSE WITNESS.** Only
  `Dict.update` was genuinely fabricated (`gorget_map_update` is a **merge**, no HofOp arm) — row deleted.
  ⭐ **AND IT CHANGED THE GUARD'S DESIGN:** the witness union **must include the `equip` blocks**, and must
  be **PER-FAMILY, not flat — a flat union accepts `Dict.reduce`.** New lint
  `closure_shape_rows_have_a_callback_witness` **RED-verified TWICE** (re-adding `Dict.update` names it;
  `("Dict","reduce")` — a real method in the wrong family — is **also** caught).
  ⛔⛔ **A THIRD DEFECT THE FOLD TURNED UP — A GUARD THAT VIOLATED ITS OWN COMMENT.** `--accept` retired a
  divergence whenever `len(div_lanes) > 1`, but `diverges` is computed over **MEASURED** lanes — so a
  `c,llvm` run **ERASED `DIVERGENT` ON 137 UNRELATED ROWS**, every flag raised against the **UNMEASURED
  selfhost lane**, including succession-plan findings (`str_slice_colon` selfhost=WRONG;
  `func_closure_reassign` / `loop_range_paren_endpoint` selfhost=WORKS). ⭐ **THE COMMENT DIRECTLY ABOVE THAT
  CONDITION ALREADY FORBADE IT; THE CONDITION DID NOT IMPLEMENT ITS OWN COMMENT.** Fixed to
  `len(div_lanes) == len(VALUE_LANES)`; **the first run was DISCARDED, NOT COMMITTED**; re-run diff is
  **exactly 25 lines, 0 erasures, 0 empty LLVM baselines, every changed row `hof_*`.** ⊕ Recorded in `t0993`
  **with a warning that earlier two-lane `--accept` runs likely did the same.**
  ✅ **B2 REWRITTEN AND NOW ANSWERED EMPIRICALLY, NOT BY DATES** — real cause `efaabeadc` against labels
  written `f62b5472b`; `doc_ld_in_range_inclusive` on LLVM exits **rc 139, SIGSEGV** ⇒ **YES, always a signal
  death.** ⊕ The `pair_upperstr_c_map_join` catch is confirmed: **the load-bearing predicate is UNTYPED, and
  zero of the seven have one.**
  ✅ **S3 — the executor CONCEDES it under-claimed**: it had checked only the C lane, where the cell is
  BUILD-FAIL either way. **"Single-lane blindness, exactly what I flagged in others' work."**
  ⊕ **`t0987`'s FOURTH correction states the WHOLE axis this time:** `Set.for_each` fabricates
  `gorget_set_for_each`, and **the two protocols fail DIFFERENTLY** (no-import: **Set rejects cleanly,
  Vector fabricates**) — **two distinct routes into the catch-all.**
  ⊕ **S9 `--accept-drift` split out** — *"a `TRAP → CRASH:sig11` fold under a 'progress rows folded' summary
  is a RATCHETED SEGFAULT"*; it now declines out loud. ⊕ **S7: `hof_for_each_strings_noimport_namedfn` is
  the ONLY corner G will flip** — the review confirms it before G is released.
  ⚠ **One erratum of mine PUSHED BACK ON: `closures.rs:182` was NOT stale** (grep confirms 182).
  **Gates bare:** build 0 · `--lib` **1188/0** · `--test lints` **219/0** · `robustness_map --lanes c,llvm`
  **0, no REGRESSION line, 0 new divergences.** `t0994`–`t0996` **unused — nothing here was a new class.**

- **✅ A1-I · PASS 4 SIGNED OFF — EXECUTOR LAUNCHED.** Folded as ADDENDUM 4 (7 fold items, all scope/errata
  inside a sound design; streak NOT reset).
  ✅ **THE THREE-STATE DEMONSTRATION HOLDS, REBUILT FROM A TYPED SOURCE** (not a name re-derivation):
  pristine **`7028`** / carrier **`7063`** / const-hoist evasion **`7028`** — with the textual ratchet
  reading **19 in BOTH the correct and broken states** and `--test lints` **220/0/1 in all three.**
  ⇒ **ONLY THE STDOUT FIXTURE DISCRIMINATES. READINESS 4 CLOSED.**
  ⚠ **AND AN ACCIDENT NOT TO LEAN ON:** with a *minimal* carrier the evasion **failed to build** on
  `-D dead-code` — **that backstop EVAPORATES the moment the field has a second reader, and the real change
  will have several.**
  ⛔ **THE WORK-LIST WAS 5 ROWS SHORT — `|changed| = 29`, not 24.** A total `__call`-derivation sweep returns
  **11**; the brief named **5**. ⚡ **`lir/lower/mod.rs:373` and `llvm/mod.rs:346` are GENUINELY ORPHANED — an
  executor working row-by-row would MISS them.** ⚠ **Two rows are TRAPS:** `ir/lowering/mod.rs:5196` asserts
  the **MINT's OUTPUT**, not a decision — **it stays**; and **`optimize.rs:218` is a DCE ROOT-SET SEED** —
  narrowing it **removes user `call*` methods from the root set**, with **vtable-dispatched
  `equip X with Trait:` the exposed shape**, pinned by the `traitequip` cell.
  ⭐⭐ **THE SELF-HOST IS THE CORRECT COMPILER HERE AND RUST gg IS THE BROKEN ONE.** All **TEN** shapes
  compile + run + **MATCH** on the SH lane **today**, including the **six Rust gets wrong** ⇒ **owner
  2026-08-10 satisfied BY MEASUREMENT, and NO PORT is owed for the miscompile.** **This is the
  succession-plan case: file "reference lags the self-host", fix Rust as ORACLE HYGIENE.**
  ⛔ **AND THE CORPUS MECHANICS ARE A TRAP:** `runtime_parity_corpus` **auto-scans top-level fixtures and
  ORACLES AGAINST RUST**, so **the five silently-wrong cells score `WrongOutput` the moment they land —
  own non-MATCH inflow, which Core #9 ⊕ FORBIDS.** ⇒ **FIXTURE AND FIX IN ONE COMMIT.** ⊕ **`runtime_diff`
  can NEVER be the guard — it blesses whatever Rust prints.**
  ⭐ **AN EIGHTH BLIND SPOT, AND IT IS A2's:** the census's MAPKEY regex covers **19 of 243** string-keyed
  accessors — **`lookup` alone is 125 and entirely invisible**, hiding **5 production closure-identity
  decisions** (all literal `GorgetClosure` ⇒ C3 ⇒ **A2's bucket**). **A1-I's work-list is unaffected; the
  totality caveat must NAME it beside C7.**
  ⊕ **`traitequip` has NO ggdef axis** (trait items are out of the phase-0 subset) — its oracle is the SH
  lane plus the non-trait twin. ⊕ **`varonly` re-confirmed ACCIDENTALLY CORRECT** — a labelled scope
  control, never evidence.

- **⚡ A1-M · SECOND FOLD LANDED (`c8461014b`) — SCOPED CONFIRMING REVIEW LAUNCHED (records only; NO `src/`
  change in this fold).**
  ⭐⭐ **THE MECHANICAL CAUSE OF THE FALSIFIED CONTROL, FOUND BY THE EXECUTOR: its `String` control used a
  24-CHARACTER LITERAL, which never leaves the small-string buffer and ALLOCATES NOTHING.**
  ⚡ **"A CONTROL THAT CANNOT ALLOCATE CANNOT ACQUIT ANYTHING"** — the same lesson `t0948`'s own repro carries
  about closures needing to **capture**. **The transferable rule is now stated where the next person reads
  it: a `String` control for a leak claim must be HEAP-FORCED, exactly as a closure control must CAPTURE.**
  ⭐ **BOTH falsified controls now stand, labelled `CORRECTION 1` and `CORRECTION 2`, each with its measured
  rows beneath** — *"hiding either version would have deleted the one thing that makes the third version
  trustworthy."*
  ⊕ **`t0972` re-scoped as GENERAL and its fixture RENAMED** `..._callable_binding_...` → `..._payload_...`
  **and widened to carry BOTH payload types, so it cannot drift back** (Core #12 — the name was making the
  wrong claim). ⊕ **`t0971`'s mechanism SURVIVES and its discriminator was verified off the emitted C**
  (`Option__Option__GorgetString__drop` = **3**: declared, defined, **called**; the `Callable` spelling =
  **0**), with its repro moved to the **NO-MATCH** form.
  ⊕ **`DONE.md` now says "five behaviourally pinned, the sixth by the arm-count lint"** — ⭐ **and records
  that THE PEEL in that helper IS LOAD-BEARING** (remove it and `Some(Some(<lit>))` is rc 139 again),
  **which is what stops a future reader deleting a call that looks dead.**
  ⚠ **The executor's third flag about the handover's `t0938`/`t0873(a)` claims is ITSELF STALE** — corrected
  at `522a89bd9`; its branch predates that commit.

- **⛔ A1-M · CONFIRMING OUTPUT-REVIEW: THE CODE IS SIGNED OFF; 2 BLOCKING, BOTH IN THE RECORDS.** All three
  mandatory gates PASS. Returned to the executor; **not integrated, and the LEAK ASK DOES NOT GO TO THE
  OWNER YET.**
  ✅ **THE SITE DISPUTE IS ADJUDICATED FOR THE EXECUTOR, BY CONSTRUCTION.** `functions.rs:215` calls
  `emit_enum_init_owned`, so `wrap_expr_tail_in_ok` **already routed through the chokepoint — it was never
  the gap.** The live site is `stmts/mod.rs:2047-2051`, and the placement is right (`:2052-2056` re-derives
  `returned_local` from the **packed** operand; safe when the pack no-ops).
  ⛔⛔ **BLOCKING-1 · THE REPLACEMENT CONTROL IS FALSIFIED TOO — THE SAME MOVE ONE LEVEL DOWN.** The
  `Callable`-specific claim is false on **both lanes**: `Option[Option[String]]` in the *identical shape to
  `t0971`'s repro* leaks **42 B / 1**, and `Result[String,String]` on a **call result** leaks **42 B / 1** —
  both claimed CLEAN. The LOCAL-scrutinee form and the flat `Some(s)` control are genuinely clean, so the
  loop is not the source. ⇒ **`t0972`'s axis is REAL but NOT `Callable`-specific — it UNDER-SCOPES a general
  defect**, and **`t0971`'s MECHANISM *is* `Callable`-specific** (`Option__Option__GorgetString__drop` is
  emitted; the `Callable` one has **zero** hits) **but its filed repro CONFLATES it with the match-move-out
  leak, which is not.** The clean discriminator is the **NO-MATCH** form: **8 B for `Callable`, CLEAN for
  `String`.** ⚡ Widened `scope`/`mechanism` lines on the two existing items — **not new filings.**
  ⛔ **AND THE REVIEWER'S FRAMING IS WHY THIS BLOCKS:** *"the block's whole rhetorical weight rests on having
  JUST corrected a control that was generalised from one cell. Ship it as-is and the owner reads a corrected
  paragraph that REPEATS THE ERROR IT CORRECTS."* **Third generation of one error class.**
  ⛔ **BLOCKING-2 · `DONE.md` OVERCLAIMS READINESS #4.** *"every one of the six is behaviourally pinned"* is
  **false for site 5** — neutralising it by line leaves the fixture **rc 0 with full correct output**. The
  cause is **STRUCTURAL, not accidental**: every `lower_prelude_variant_payload` caller goes on to
  `emit_enum_init_owned`, so **site 3 packs whatever site 5 didn't.** ⚡ **Site 5 IS pinned — by the
  ARM-COUNT LINT, which would go 5≠6.** Say "five behaviourally pinned, the sixth by the lint", or add the
  cell. ⊕ The reviewer neutralised **all six by line**: 1, 2, 3, 4 and 6 each take the fixture to rc 139.
  ✅ **ON LEAVING THE FALSIFIED CONTROL IN THE BLOCK, THE REVIEWER AGREES AND SAYS KEEP THE PATTERN** — it is
  the reasoning the owner was going to rule on, the measured rows sit beneath it, **and deleting it would
  hide the one thing the owner needs to see.**
  ✅ **Verified clean:** all four isolated cells reproduce · **144 B / 16 allocations on BOTH lanes** ·
  in-situ attribution holds for **both** items · both filings durable, `#[ignore]`d, RED at 8 B/1 on C and
  LLVM · blast radius `throws` **45/0 on both lanes** · **SH lane MATCHes with a driver rebuilt at HEAD,
  zero parity inflow** · bare gates `--lib` 1185/0, `--test lints` 219/0, `-p ggdef` 187+7/0 · **the helper
  uses typed predicates on both sides — no name matching** — and `extract_result_field_types` is total, so
  site 6 cannot panic.

- **⛔⛔ A1-I · PASS 3 — THE DEFECT IS WORSE THAN A CRASH, AND MY GUARD RULING IS DISPROVEN.** 3 blocking,
  folded as ADDENDUM 3; **pass 4 launched.** ⭐ Pass 3 states explicitly this is **NOT a "review harder"
  case** — the spine (carrier #1 on `TypeMetadata`, the `ClosureCallSig` route, the A1/A2 split) **is sound
  and it concurs**; what was unsound is **one ruling and one omission**, both now folded.
  ⛔⛔ **B6 · THE CAPTURING CELL IS SILENT WRONG OUTPUT, AND ASan + UBSan ARE BLIND TO IT.**
  A capturing closure whose **first capture is itself a `Callable`** — so the mis-shaped env's first 8 bytes
  are **a valid code pointer instead of an integer** — gives **ggdef `7063` vs Rust `7028` on BOTH backends,
  rc 0, `gg check` clean, and `--sanitize` rc 0 WITH NO DIAGNOSTIC.** `7028 = (1004 + 0) × 7`: **the capture
  silently read as 0.** ⇒ **§5's instrument claim is FALSE for the highest-severity cell — only a wired
  stdout-diff against ggdef sees it (Core #13 failing on the brief's OWN instrument claim).** ⇒ ⚡ **THE
  FIXTURE NET MUST ASSERT STDOUT, NOT `rc != 139`** — a fix validated on exit codes **greens the loud cells
  and leaves the silent one live.** ⊕ `_call` as a method name is **also silently wrong**; a **FREE**
  function named `call` is the **green control** proving only MANGLED METHODS collide.
  ⛔⛔ **B7 · "CARRIERS ARE THE CLASS-RETIRING GUARD" IS DISPROVEN BY PROTOTYPE — MY ADDENDUM-2 DECISION WAS
  WRONG.** With the carrier landed, an `assert_eq!` probe at `:551` **compiles and FIRES**
  (`left: true, right: false` on `Runner__call`) ⇒ **the carrier ADDS a typed route; it does not make the
  old fact UNAVAILABLE** — and **the const-hoist evasion still works with the carrier in place**
  (`--test lints` **220/0 green**, capturing cell back to `7028`). **SIX Q#2 answered NO for BOTH halves.**
  ⭐ **BUT THE ANSWER IS ALREADY IN THE TRACK'S OWN SCOPE: THE GUARD THAT CAN CATCH THIS CLASS IS
  BEHAVIOURAL.** The stdout-asserting fixture net is **RED at pristine HEAD (`7028`) · GREEN with the
  carrier (`7063`) · RED AGAIN under the evasion WHERE THE LINT STAYS GREEN.** **It cannot be respelled
  around, because it tests BEHAVIOUR.** ⚡ **DECIDED: strike the class-retiring claim; name the FIXTURE NET
  as the Core #6 guard with that three-state proof; keep the ratchet as labelled BOOKKEEPING; file the truly
  class-retiring form (typed callee identity on `Instruction::Call`) as `t1053`.**
  ⊕ **The typed route also FIXES THE SILENT CELL** (`7028 → 7063`) — **§3's yield had only ever measured the
  CRASH cells.**
  ⛔ **B8 · THE CENSUS'S ARITHMETIC CLOSES; ITS SCOPE DOES NOT — AND `|changed|` WAS UNDEFINED.** All 99 rows
  reproduce, **but they are LITERAL-KEYED PREDICATE rows**, blind to **variable-key sidecar maps**
  (`closure_info`: 1 writer + **8 call sites**, **ZERO in the 99**, and **Layering rule 3 names it exactly**)
  and to name-DERIVATION rows. **The body's site list had been retracted with nothing in its place.**
  ⚡ **WORK-LIST NOW STATED:** 10 distinct predicate sites (2 `#[cfg(test)]`) · the 3 byte-identical
  key-derivations (**Core #4, all three or none**) · the 2 backend-only derivations → carrier #2 · the
  `closure_info` sidecar → carrier #1.
  ⭐⭐ **THE SEVENTH CONVENTION — IDENTITY BY *SENTINEL*, NOT BY NAME.** `calls.rs:1897`'s
  `if local_type_id == UNIT_TYPE` **is in the class by the repo's OWN wording** — Layering rule 2 forbids
  *"name prefixes, SENTINEL VALUES, or runtime-symbol conventions"* **in one breath** — and it is **invisible
  to ANY ratchet regex, because there is no string to match.** It is **A2's by the split** so it does not
  grow A1-I, **but the totality claim must NAME it, or the census is a selection for the SEVENTH time —
  this time selecting on MECHANISM (strings) rather than on keyword list.**
  ⊕ **`t1051` gains an unnamed sibling:** `llvm/mod.rs:4766-4769` derives `elem_size` from
  `name.contains("int64_t")/…` with `else { 8 }` — **the same shape, and IT IS THE ARM THAT SENDS `t1052`'s
  `Vector__Tally` TO A WRONG DEFAULT.** The item claims both.

- **✅ C · INTEGRATED — `d419176b6`, the round's THIRD landing.** Squash-merge (4 commits, 39 files).
  **Closes `t1017`, `t1019`; narrows `t0025`; widens `t0434`; files `t1020`–`t1023`.** Gates at the
  integrated state, bare: build 0 · `--lib` 1181/0 · `--test lints` **220/0/1**.
  ⚠ **Worktree NOT pruned — harness-LOCKED** (`cannot remove a locked working tree`). Do **not** force it.
  ⭐ **ERRATUM B CHANGED THE ANSWER, AND THAT IS THE POINT OF CORE #15(b).** Told to name the third
  "nondeterministic" row, the executor **measured it instead of asserting it** — and `test_process` is
  **not per-run-random but COLD-START SENSITIVE**: outside the sweep's 8-way parallelism, **HEAD, the fix
  and a deliberately WRONG variant all produce the IDENTICAL hash.** ⇒ **the conclusion survived and got
  STRONGER** (three different compilers agreeing is better evidence than "nondeterministic"), **but the
  blanket verdict had been doing work no measurement supported.** The same wording was live in **three
  places**; all three now carry the identical per-row disposition **so they cannot drift.**
  ⊕ **The `--lib` "discrepancy" was not one:** `1183 passed + 2 ignored = 1185 total` — the reviewer counted
  the total, the executor counted passes. Recorded so nobody chases it.
  ⊕ `figures.db`'s `roundish_headlines` **regenerated from `clone_meter_check.sh --round-close-census`
  rather than hand-merged** → **83**, with **`anchor_age_matched` unmoved at 4 across all three R49
  landings — which is what says none of them is a round close.**

- **⚠ SHARED-`/tmp` COLLISION HAZARD, FOUND BY THE A1-I SCOUT ON ITS WAY OUT — AGENTS.md MA-9 SHARPENED.**
  Three patches under its own `recover_a1i_*` prefix were **not its own**: a later brief-review agent on the
  same track independently chose the same mnemonic and wrote to the same prefix, touching **exactly the two
  sites the scout's report names.** ⭐ **No clobber occurred — the suffixes differed and all three of the
  scout's checkpoints still `git apply --check` clean — but a collision there would have been SILENT.**
  ⇒ **MA-9 now requires every `/tmp` artifact to be NAMESPACED BY AGENT (`recover_<agentid>_*`).**
  ⊕ The scout's report stands unchanged, and it re-verified the load-bearing half itself: p1/p2/p3 apply
  clean at HEAD, and **the lint ratchet is blind — 0 of 12 shapes match, confirmed empirically by lints
  staying green while 12 name-match sites were deleted.**

- **✅ C · OUTPUT-REVIEW: INTEGRATE — all three mandatory gates PASS; 2 record errata sent to the executor.**
  Gates bare at `3fb8d6276`: build clean · `--lib` **1185/0** · `--test lints` **218/0/1** · `-p ggdef`
  **187 + all 7 targets** · `spec_conformance` 3/0 · `security` 213/0 ·
  **`self_host_bootstrap_fixed_point` converged at stage-2** · `robustness_map --lanes c` **842/1009,
  exit 0** · LLVM at parity on all 7 new cells. **Every positive RED-verified by the reviewer against a
  compiler it built at `4b0b2ed23`.**
  ⭐⭐ **BOUNDEDNESS ADJUDICATED BY TOTAL ENUMERATION, NOT SAMPLING.** All **148** fixtures containing any of
  the four names, build+run+stdout-md5 on both compilers: **11 rows move, 10 are the track's OWN new
  fixtures**, and the single pre-existing row is **`t0947`'s own `#[ignore]`d repro, RED before and after.**
  The **4368-fixture** check sweep gives exactly 5 differing rows, **all the track's own**, with **ZERO**
  pre-existing fixture changing rc *or* diagnostic text. ⊕ It chased `lib/xtd/p2p.gg:351` **specifically**
  because `t0947` says the shape is live in shipped stdlib — **it is a FREE call, so no peel**, and `:361`
  already uses the explicit binding. **57 stdlib-importing fixtures: NO DELTA.**
  ⚡ **THE HONEST DISPOSITION, and it belongs in the record:** the class is bounded **in KIND, not
  enumerable in count** — **and every such program was ALREADY miscompiled**, so **no previously-correct
  program regresses**; the failure moves from silent wrong output to a loud build failure, **DOWN the
  owner's severity ranking.**
  ⭐ **CORE #8 IS WHAT THIS TRACK RETIRES:** pre-fix, **C AND LLVM both printed `false|false|false`** — the
  literal *"both backends agree on the wrong answer"* red flag — and **two independent oracles adjudicate
  the new answer** (ggdef in-subset, plus the SH lane MATCHing post-fix Rust on all 5 new corpus fixtures).
  ⊕ **ASan is STRUCTURALLY BLIND to the return-type axis** — rc 0 clean on **both** the broken and the fixed
  program — so the stdout compare is the only instrument (Core #13).
  ⊕ **ZERO runtime-diff inflow from the Core #9 divergence**: `integration.rs:40060-40064` short-circuits a
  Rust-rejecting fixture as `RustRejected` **before** the self-host runs. **No ceiling or floor is touched
  anywhere in the diff.**
  ⛔ **ERRATUM A — `integration.rs:1357-1365` STILL SHIPS THE REFUTED PREDICTION** (*"under the wrong edit
  these two print HEAD's output byte for byte"*, and *"AND NOTHING ELSE CAN BE"*), **contradicting
  `methods.rs:1655-1668` IN THE SAME COMMIT SERIES**, which is correct. ⛔ **ERRATUM B — `DONE.md` claims
  "3 differing rows, all three PROVEN NONDETERMINISTIC" and names only TWO** (Core #15(b): a set claim owes
  a disposition per row).
  ⊕ **My `doc_b09_optional_chaining` claim is CONFIRMED WRONG** — `t0015:11` names it verbatim.
  ⚠ **INTEGRATION: C branched at `4b0b2ed23`, BEFORE E and H landed ⇒ a MERGE, not a fast-forward**, and the
  `figures.db` `roundish_headlines` line conflicts with every other track's `DONE.md` entry. **At round
  close take `scripts/clone_meter_check.sh --round-close-census`'s number rather than hand-merging.**

- **⛔ A1-I · PASS 2 — 3 BLOCKING, 6 SCOPE. THE CENSUS CLOSES; THE PRESCRIBED RATCHET IS DEMONSTRATED
  USELESS.** Folded as ADDENDUM 2; streak stays 0, **pass 3 launched to decide whether an executor runs.**
  ✅ **THE CENSUS CLOSES — AND IT IS SIX CONVENTIONS, NOT THREE.** The fix was to stop post-filtering a
  hand-written keyword list and **derive the literal set from the side that CANNOT be a selection: every
  closure-identity name must first be MINTED.** W-PRODUCER gives **29 mint spellings**; the consumer sweep
  gives **99 decision rows, EVERY ONE ASSIGNED, and the per-convention totals SUM.** Two conventions no
  draft had (`__adapt_`, and the spawn/async family) plus **C6, a `contains("Callable")` on a type name with
  NO `__` at all that DECIDES A LOAD WIDTH** — **invisible to every witness including ADDENDUM 1's own
  proving command.** ⚖ Honest limit, the reviewer applying my S5 distinction to itself: **NO EXPLOIT
  DEMONSTRATED — a census row, not a claimed defect.**
  ⊕ ⭐ **A DEAD ARM PROVEN MECHANICALLY:** `__spawn_thread_wrap_` has **two lines in the whole tree, both
  predicates, ZERO producers** — its doc already confesses *"kept for backwards compatibility"* with no
  guard. **Core #14: delete it, and the deletion is one command to prove.**
  ⛔⛔ **B4 · THE RATCHET I PRESCRIBED CANNOT CATCH ITS OWN CLASS, AND IT WAS DEMONSTRATED, NOT ARGUED.**
  Prototyped and driven LINE-anchored: it goes RED in both directions on the INLINE spelling — **and then a
  one-line `const CALL_MARK: &str = concat!("__", "call")` hoist leaves it `test result: ok`, `cargo build`
  rc 0, and THE BINARY AT rc 139. The miscompile live, the guard green.**
  ⭐⭐ **AND THE REPO ALREADY SAID SO — I CITED THE WRONG PRECEDENT.** `assert_exact_ratchet`'s own doc:
  *"this is a BOOKKEEPING guard, not a class-retiring one… a site respelled outside its pattern is
  invisible… Retiring a class takes a guard that cannot be respelled around — a type whose misuse fails to
  COMPILE."* ⚡ **DECISION: carriers #1/#3 ARE the class-retiring form** (they make the fact unavailable
  except through the accessor), **and the ratchet ships EXPLICITLY LABELLED BOOKKEEPING THAT BACKSTOPS THE
  CARRIER.** ⛔ **Its literal set derives from W-PRODUCER, never a hand list — otherwise a future `__thunk_`
  enters invisible on day one, which is EXACTLY how C4, C5 and C6 got here.**
  ⛔ **B5 · THE CARRIER'S OWN DOC-COMMENT IS FALSE.** `mod.rs:114-119` says `closure_call_sigs` is *"indexed
  by the full call-function name"* — **it is keyed by EVERY GIR fn name, and that false belief is exactly
  what makes `contains_key` look like a discriminator.** Core #14, **sitting on the carrier the executor
  must use.** ✅ The route itself is confirmed end-to-end.
  ⛔ **S7 · THE FIXTURE NET OMITS THE CELL THAT MAY TURN A CRASH INTO SILENT WRONG OUTPUT.** Every probe so
  far uses a **ZERO-CAPTURE** closure — ASan's object is a **1-byte** env. **A CAPTURING closure has a real
  env and MAY PRINT A WRONG NUMBER INSTEAD OF CRASHING.** Pass 3 is measuring it; **if it miscompiles
  silently the severity framing changes.** ⊕ Six further named omissions, incl. **a FREE function named
  `call` as a GREEN control** — without it the fixture NAME over-claims scope, since only MANGLED METHODS
  collide.
  ⚡ **S8 DECIDED — AND THE CLAIM CHANGES.** `struct_aliases` keys are **type-alias names, never method
  names**, so `contains_key` alone already discriminates and **the ~50 predicates are DEAD WEIGHT.**
  ⚠ **But that is an ACCIDENT, not a guarantee** — a user-struct element *would* produce `Vector__setting`,
  containing `__set`. **`t1051` is filed as "not currently reachable — the gate is sufficient — the 50
  predicates are dead weight whose REMOVAL is the fix", NOT "no exploit demonstrated".**
  ⛔ **S9 · A SEPARATE UNFILED DEFECT ON BOTH BACKENDS → `t1052`:** `Vector[UserStruct](capacity)` is
  **C rc 101 ICE** (*"Ptr ABI received scalar value"*) and **LLVM build-fail**; the callee reaches the
  backend as **`gorget_array_tally`** — `Vector__Tally` name-stripped and re-prefixed. `Vector[int](4)` is
  fine.
  ⊕ **S1 is BIGGER THAN 7: 55 SH rows across FOUR driver dirs** — the `Callable|MutCallable|ConsumeCallable`
  block is duplicated in `self_host_{check,lowerer,resolver,typechecker}`. **Fix primitives in ALL copies.**

- **⚡ A1-M · FOLD LANDED (`364a24a14`) — FRESH CONFIRMING OUTPUT-REVIEW LAUNCHED.** All five blocking items
  answered. Gates bare: build 0 · `--lib` **1185/0** · `--test lints` **219/0** · `-p ggdef` 0 ·
  `spec_conformance` 3/0 · `security` 213/0; targeted `--release` **12 C filters and 5 LLVM filters, every
  one exit 0** — including `throws`/`autoprop`/`rethrow` **run specifically for B1's blast radius**, since
  the return-`Ok` path is shared with every `throws` function in the corpus.
  ⭐⭐ **B1 · THE REVIEWER WAS RIGHT ABOUT THE DEFECT AND WRONG ABOUT THE SITE — AND THE DISTINCTION IS THE
  WHOLE FINDING.** The `throws` auto-`Ok` wrap has **TWO write sites**, and only one reaches the chokepoint:

  | `throws` body form | wrap path | at `dc29f0faf` |
  |---|---|---|
  | **expression body** | `wrap_expr_tail_in_ok` → `emit_enum_init_owned` → **site 3/5** | **prints `2`** |
  | **`return <literal>`** | raw `builder.enum_init`, its own `assign_mode(Move)` | ⛔ **rc 139** |

  ⇒ the live site is **`stmts/mod.rs:2036`**, not `functions.rs:215`. ⭐ **"The GREEN TWIN is exactly what
  made the position look enumerated — SIX QUESTIONS #3 AND #6 IN ONE CELL."** Fixed as **site 6/6**, with
  the packed temp becoming the `returned_local` so `move_zero` targets the operand actually consumed
  (GIR-verified).
  ⭐ **B2 · REFUTED BY MEASUREMENT, AND THAT IS WHAT CLOSES IT.** Site 3/5 is **not** uncovered — deleting it
  makes the **expression-body `throws`** cell go **rc 139**. It was simply **not in the fixture.** Both body
  forms are now cells ⇒ **all SIX sites behaviourally pinned (readiness #4)**; guard bumped 5→6 and
  re-RED-verified.
  ⭐ **B3 · REPRODUCED, THEN PUSHED ONE STEP FURTHER — AND BOTH MECHANISMS ARE `Callable`-SPECIFIC.**
  `Option[Option[String]]` is **CLEAN** and `Result[String,String]` on a call result is **CLEAN**, so the
  filings are sharp: **`t0971`** — the outer `Option__Option__Callable__GorgetClosure__drop` **does not exist
  in the emitted file** while the inner one is correct · **`t0972`** — the arm `memset`s the payload slot and
  *then* frees the zeroed slot, **and the LOCAL-scrutinee form emits `gorget_closure_free` while the
  CALL-RESULT form does not.** Attribution was **measured, not assumed** (deleting the nested cell takes the
  fixture 16 → 15). **Retirement now gated on all THREE of `t0948`/`t0971`/`t0972`** — landing `t0948` alone
  only tightens it.
  ⭐ **AND THE EXECUTOR LEFT THE FALSIFIED CONTROL IN THE BLOCK, WITH THE FOUR MEASURED ROWS BENEATH IT** —
  *"it is the reasoning the owner would otherwise have been ruling on."* **That is the right instinct: an
  owner-facing block should show the refuted claim, not quietly replace it.**
  ⊕ **B4 corrected: 144 bytes in 16 allocations** (the constants were already right). ⚠ The two new `throws`
  cells add **ZERO** net records — both ASan-CLEAN in isolation, itself an attribution datum.
  ⚖ **THE LEAK ESCALATION IS NOW FIT FOR THE OWNER** — three mechanisms named and individually measured,
  retirement gated on three items, count verified on both lanes. **Goes up once the confirming review signs.**
  ✅ **The handover's `t0938`/`t0873(a)` claims were already corrected** (the executor flagged them twice;
  the correction landed at `522a89bd9`).

- **⛔ F · OUTPUT-REVIEW: 2 BLOCKING (both foldable) + 10 SCOPE. ALL THREE MANDATORY GATES PASS.** Returned
  to the executor; **not integrated.** Gates re-run bare at `22b8dec2f`: build 0 · `--lib` **1187/0** ·
  `--test lints` **218/0/1** · `robustness_map --lanes c,llvm` **rc 0, no REGRESSION line, ZERO PROGRESS
  lines** (⇒ the committed baselines are exactly what the compiler produces). **All 8 PINs RED-verified by
  the reviewer itself** against a compiler built at `a534fbe3d`.
  ⛔⛔ **B1 · SIX Q#2 LANDS ON THE FIX'S OWN GUARD — and four rows are ALREADY WRONG.**
  **The defect F fixed was a missing METHOD in a hand-maintained list; `closure_shape_rows_are_total`
  catches a missing PROTOCOL. It cannot catch its own class**, and the proof is in the diff:
  **`Dict.update` is a map MERGE, not a callback method** (`gorget_map_update(void*, GorgetMap)`, **no HofOp
  arm**) and **`for_each`/`find`/`find_index` on `Set` DO NOT EXIST** (`E_NoMethodFound`; `SET.methods` ends
  at `filter/fold/each/any/all`, and there is no `equip Set`). ⭐ **The bad rows came from
  `BuiltinMethodDecl`'s `key_val_params` + its "// Higher-order" banner — THE VERY SOURCE THE COMMIT MESSAGE
  SAYS IT DELIBERATELY DID NOT REUSE.** All four are inert today, **but a table sold as the single typed
  source of truth, green-lit by its own totality test while carrying four fabricated rows, is exactly what
  this gate exists to catch.** ⚡ **Remedy ordered: delete the four AND ship a cross-check against THREE
  INDEPENDENT WITNESSES** — `protocol.methods` ∪ the LIR HofOp dispatch arms ∪ the `equip [T] Vector[T]`
  block — **feasibility already checked: that union covers every legitimate row and flags all four bad ones.**
  ⛔ **B2 · `t0993`'s CAUSAL ATTRIBUTION IS FALSE; THE CONCLUSION SURVIVES, THE ARGUMENT DOES NOT.** The
  seven `TRAP → CRASH:sig11` drifts cannot be today's `verdict.py` change — **its only functional delta is
  BUILD-phase, while both classifications are RUN-phase and untouched.** The real cause is **`efaabeadc`
  "ONE verdict classifier"**: the previous adjudicator returned `TRAP` for **any** non-zero run exit, those
  baselines were written under it, and afterwards **no `--accept` could move a non-good → non-good bucket**,
  so the stale label persisted. ⇒ **the item's own open question is DECIDABLE and the answer is YES** — as
  written it points the disposition at the softer hypothesis on a false premise. ⊕ Its *"not one drifted
  cell contains a collection HOF closure"* is also false. ⊕ **The conclusion is right and the reviewer
  verified it EMPIRICALLY, which the item did not.**
  ⭐ **S3 · THE EXECUTOR UNDER-CLAIMED — readiness #4's "stated hole" is CLOSED.** Perturbing to the 2-arg
  `infer_operand_type` flips `hof_fold_nonconstant_accumulator` on **LLVM** from `WORKS` to BUILD-FAIL,
  which the map scores a **REGRESSION**. **There IS a gated fixture witness.**
  ⛔ **S1 · 23 of the 29 new cells are UNGATED ON LLVM, including all 8 PINs** — an empty baseline is
  unregressable (`robustness_map.py:629`), and **all 23 empty-`llvm` rows in the file are this commit's**.
  **Readiness #3 holds on C and FAILS on LLVM.** Remedy is one **scoped** `--accept`.
  ⚖ **DIVERGENCE ADJUDICATED — TWO DIFFERENT ANIMALS, and my framing merged them.**
  **`find_index` bound is the silent-wrong one**: C accept-then-cc-fail, **LLVM builds and prints `none`
  where the answer is `1`** — and the recorded INTENDED (`1`) is right, because `lib/std/iter.gg:427`
  declares `Option[int] find_index`, so `builtins.rs`'s `ret_int` is the side that must move. **Neither lane
  is accidentally correct; the LOW → HIGH raise is justified.** **`fold_nonconstant` is NOT silent-wrong** —
  **LLVM's `<abc` is the CORRECT answer**, hand-derivable; it is a **C-emit defect**, filed correctly.
  ✅ **S7 · G'S BLOCKER DISCHARGES** — the `for_each` cell genuinely pins the no-import trigger, and the
  third correction means **the defect SURVIVES G**. ⚠ **But the corner G will actually flip —
  `for_each` + NAMED fn + NO import — HAS NO CELL**, so once `std.iter` is preluded the import half loses
  its only possible witness. **Two cheap cells close it.**
  ⛔ **S8 · `Set[String].for_each` WITH the import fabricates `gorget_set_for_each`** — **`t0987`'s class on
  a SECOND protocol**, which its "specific to Vector-without-the-import" narrowing does not cover.
  **A fourth correction to that item; state the whole axis this time.**

- **⛔ A1-I · PASS 1 — 2 BLOCKING, 6 SCOPE. Headline REPRODUCES; carrier #1's host SIGNED OFF; the split
  VERIFIED STRUCTURAL. Folded as ADDENDUM 1; streak reset, pass 2 launched, SCOPED to closing the census.**
  ⭐ **THE CAUSAL CHAIN IS NOW PROVEN, NOT INFERRED** — a **LINE-ANCHORED** probe (the Core #13 rule added
  this round) editing **only** `insts.rs:551` gives **rc 0 on C AND LLVM**. Emitted C shows the colliding
  spelling passing `__gg___Closure_0 __s5 = {0}` — **a ZEROED 1-BYTE env** — then calling through
  `((void**)__v3)[0]`, **a null fn_ptr read past the end of a 1-byte object.** ⊕ SIX Q#1 settled: `call` has
  **no reserved status in the ledger** and ggdef executes it correctly ⇒ **a defect, not a ratified
  asymmetry.**
  ⛔ **B1 · THE BRIEF NEVER NAMED THE CARRIER THAT RETIRES THE HEADLINE SITE — AND CARRIER #3 CANNOT REACH
  IT.** `FuncLowering` holds the CURRENT function only; no `&[LirFunction]`, no handle to `gir.functions`.
  ⭐ **But the route exists and was found: `FuncLowering.closure_call_sigs` is ALREADY a per-function typed
  sidecar, and its `skip` field at `lir/lower/mod.rs:257` is LITERALLY `takes_env` UNDER ANOTHER NAME.**
  ⚠ It is keyed by EVERY GIR fn name, so `contains_key` alone is **not** a discriminator.
  ⛔⛔ **B2 · THE CENSUS DOES NOT CLOSE — A FOURTH CONVENTION, INVISIBLE TO ALL FOUR WITNESSES.**
  **The witnesses are a SELECTION BY CONSTRUCTION** — W1 post-filters on a fixed keyword list, so anything
  outside it cannot appear, and its `== *"` regex cannot see `matches!` arm lists. Found outside all four:
  **`lir/queries.rs:49-54` `is_spawn_wrapper`** with FOUR more prefixes, where **`__spawn_wrap_<struct>` is
  minted FROM THE CLOSURE ENV STRUCT NAME** (`exprs/spawn.rs:387`; the SH spells it
  `__spawn_wrap___Closure_N`). ⛔ **THIS CENSUS HAS BEEN A SELECTION FIVE TIMES RUNNING**, and the brief
  never assigns four W1 rows to any convention, so its totals are **not checkable.** **Readiness item 2 fails.**
  ⭐ **TWO AXIS CELLS THE BRIEF LACKED:** a **STORED closure variable is GREEN** — **SIX Q#6, green for an
  UNRELATED reason** (the slot is materialised at the `let`), **so a variable-only fixture would be
  ACCIDENTALLY CORRECT and prove nothing** — and **`equip X with Trait:` ALSO FAULTS.**
  ⛔ **S3 · THE GUARD SPEC CANNOT CATCH ITS OWN CLASS AS WRITTEN.** §5 names four PREFIXES but not the
  PREDICATE SHAPES, **and the defect that opened this track is a `contains(…)`, which no `starts_with` regex
  sees** — the very blindness §5 diagnoses in the existing ratchet. ⊕ **That ratchet is blinder than
  briefed: 0 of 22, not 0 of 12**, and `comm -12` against the census is **EMPTY**.
  ⛔ **S4 · `--lib 1181/0` IS NOT EVIDENCE** — at pristine HEAD it is **identical** to the prototype figure
  ⇒ no regression **and no new coverage**; the yield section ships **no exercising fixture.**
  ⛔ **S2 · `t0681`'s REPRO IS UNWIRED** (`ALLOWED_UNWIRED`, `tests/lints.rs:24262`) — *"an UNWIRED `.gg` is
  half a repro"*. **Claiming the item MEANS WIRING IT**, and its `cites` have drifted.
  ⚡ **S5 · AN IDENTICAL-SHAPE SIBLING FAMILY: `llvm/mod.rs:4745-4762` is ~50 `!name.contains("__method")`
  predicates** against mangled names — **the same shape that produced this SIGSEGV, at 50× the arm count**,
  gated by `struct_aliases.contains_key` so **no exploit was demonstrated.** **DECISION: not A1-I's scope —
  FILE as `t1051`, carrying "no exploit demonstrated" rather than "not exploitable".**
  ⚡ **S6 · STATE WHICH FIX SHIPS: the miscompile is closed BY THE TYPED CARRIER, never by narrowing the
  predicate** (Core #2 forbids a narrower name-match).
  ⚡ **A1-I's NEW ID BLOCK: `t1051`–`t1060`** — A1's original `t0967`–`t0976` is fully consumed by A1-M.

- **⭐⭐ A1-I · SCOUT FOUND A LIVE MEMORY-UNSAFETY MISCOMPILE, UNFILED AND UNCOVERED. Brief written
  (`/tmp/brief_A1I.md`); pass 1 launched.**
  **`src/lir/lower/insts.rs:551` tests `!func.contains("__call")` — a SUBSTRING match against a MANGLED USER
  METHOD NAME (`{Type}__{method}`) — so ANY user method whose name starts with `call` COLLIDES.**
  `equip Runner: int call(self, Callable[int(int)] f, int x)` → **rc 139 on BOTH backends, `gg check` clean**;
  rename it `apply` and it works; **ggdef prints `80`/`done` and adjudicates.** A **named function** instead
  of a closure literal is also a working control.
  **MECHANISM PROVEN IN THE EMITTED C, NOT INFERRED:** the control emits `__gorget_closure_env_alloc` +
  `memcpy` + a `GorgetClosure` initialiser and passes `&__s10`; **the `call` spelling emits NONE of it** and
  passes the raw 8-byte env. **ASan names frame, object and offset exactly.**
  ⇒ **MEMORY-UNSAFETY FROM SAFE, SPEC-DOCUMENTED SYNTAX — the top severity rank — and grepping all 827
  `todo/` items on symptom AND mechanism finds NOTHING.** ⚡ **In A1-I's SCOPE, not a new filing.**
  ⭐ **AND IT IS CORE #2's RATIONALE DEMONSTRATED IN USER CODE: the no-name-matching rule exists to prevent
  exactly this, and here the violation SIGSEGVs a user program.**
  ⭐ **CENSUS FINALLY MECHANICAL: 41 SITES ACROSS THREE CONVENTIONS** (drafts said 3 → 6 → 19), with four
  reproducible `git grep … HEAD` witnesses. **`lookup_closure_info` = 11 — the handover's figure CONFIRMED.**
  ⛔ **THE GUARD IS BLIND TO ITS OWN CLASS (SIX Q#2), AND IT WAS MEASURED:** the repo's name-prefix ratchets
  compile `starts_with\("({MANGLED_PREFIXES})__"\)`, which matches **0 of the 12** closure-identity
  predicate shapes — `__Closure_` is `__X_` not `X__`, and `contains(…)` is not `starts_with` at all.
  **`--test lints` stayed 218/0 GREEN while the prototype DELETED 12 name-match sites.** ⇒ **A1-I owes a NEW
  ratchet covering `src/` AND `tests/fixtures/self_host_*`.**
  ⚡ **CARRIER CORRECTION: `TypeMetadata`, NOT GIR `Local`** — identity is a property of the TYPE, and
  **`TypeMetadata.is_closure_env` already exists there with a single writer.** ⭐ **Totality witness for the
  `takes_env` carrier is rustc E0063, not a reading:** adding the field names **exactly 21 sites, EVERY ONE
  `#[cfg(test)]`** ⇒ **exactly ONE production construction of `ir::Function`.** ✅ And the
  "backends unreachable from GIR" argument is now cited: `grep -rn "ir::Module\|TypeRegistry" src/backend/`
  → **ZERO**.
  **MEASURED YIELD:** 12 of 20 convention-1 sites retired · **rc 139 → rc 0 on C AND LLVM** · `--lib` 1181/0
  · targeted 164/0 (C) and 108/0 (LLVM). ⚠ The full C sweep **autoscaled to one thread under load** and was
  killed at 1460/0 — **the executor and I still owe the full battery.**
  ⛔ **THE HANDOVER WAS WRONG THREE MORE WAYS:** `bir/synth.rs:72` is **`#[cfg(test)]`** · **"A1's SH lane is
  NONE" is right for A1-M and WRONG here — 7 mirror sites, one a VERBATIM mirror of `optimize.rs:218`** ·
  and ⭐ **a typed carrier for convention 2 ALREADY EXISTS** — `ClosureDispatchKind` (`lir/mod.rs:445-455`)
  **whose doc-comments literally read *"Originally `__callable_N`"***, leaving `insts.rs:3892/3895` the sole
  surviving name-decode. ⊕ **`t0681` already owns `methods.rs:276` and already prescribes this fix — CLAIM,
  do not duplicate.** ⊕ `c_lir/emit_types.rs:24` emits `/* UNKNOWN_CLOSURE_CALL */` **into the generated C**
  — a silent codegen failure dressed as a comment.
  ⚡ **SPLIT ON THE MECHANISM BOUNDARY: convention 2 → A2, for a STRUCTURAL reason** — the synthetic name is
  **a MAP KEY into four sidecar tables**, and **the convention exists BECAUSE of A2's erasure**
  (`calls.rs:1897` is gated on `local_type_id == UNIT_TYPE`). **Fix A2 and that arm becomes unreachable.**

- **⚡ C · EXECUTOR RETURNED — 3 commits on `worktree-agent-a5ae02a1866616051`, 39 files / +2034.
  OUTPUT-REVIEW LAUNCHED; NOT INTEGRATED.** The name-match is **deleted**; `t1019`'s check-side rule lands
  at `typecheck.rs:3528`; the `shared auto` re-infer is gated on `Type::Inferred`. **Closes `t1017` + `t1019`.**
  **Sweeps, all regenerated by the executor:** whole-corpus CHECK **4348 → exactly ONE changed row** ·
  full **2209**-fixture BUILD+RUN+STDOUT-MD5 → **3 differing rows, ALL THREE PROVEN NONDETERMINISTIC**
  (5 runs of the SAME binary → 5 hashes) ⇒ **zero deterministic corpus change** · `--lib` 1183/0 ·
  `--test lints` 218/0 · `-p ggdef` 187/0 · `spec_conformance` 3/0 · `security` 213/0 ·
  **`self_host_bootstrap_fixed_point` converged at stage-2** · `robustness_map --lanes c` **842/1009, zero
  REGRESSION** · LLVM identical on all 7 cells · **all 5 new corpus fixtures MATCH on the SH lane.**
  ⛔⛔ **MY ADDENDUM-5 PAYLOAD-TYPE AXIS TABLE IS FALSE, AND THE CORRECTION MOVES THE DISCRIMINATOR.**
  It recorded `Result[int,_]` and `Result[String,_]` as *"unchanged"* — **both were measured inside
  `void main()`.** Inside a function that can **auto-propagate**, **EVERY payload lacking its own
  `is_error` becomes a build failure** (`int64_t__is_error`, `double__is_error`, `bool__is_error`,
  `Payload__is_error`, and for String an **invented `gorget_str_is_error`** that dies one stage earlier at
  C-compile). ⇒ **THE DISCRIMINATOR IS THE ENCLOSING CONTEXT, NOT THE PAYLOAD TYPE** — the class is *"the
  `auto` var-decl peel fired"*. **`t0434` widened to HIGH with a corrected table and three repros split by
  failure mode.** ⚠ **The output-review must re-adjudicate whether the class is still BOUNDED.**
  ⛔ **`|pinned| == |changed|` FAILED AS THE BRIEF LEFT IT** — sweeping the receiver axis by hand found **two
  more changed cells no pass named**: an **unbounded generic parameter** and a **closure local**. Both
  pinned; the first filed as **`t1023`**, deliberately **NOT** a widening of `t1019`, since excluding
  `DefKind::GenericParam` is exactly what keeps the 26 `iter_*` fixtures accepted.
  ⭐ **NO FIXTURE COVERED THE RETURN-TYPE AXIS — the highest-severity thing the track fixes.**
  `String is_some(self)` at HEAD is check-clean, builds, and prints `false`: **a `Bool` constant delivered
  into a `String` slot.** New axis fixture `false|false|false` → `true|107|payload-string-value`.
  **ASan is structurally blind; the stdout compare is the instrument.**
  ⭐ **READINESS ROW 4 IS NOW A MEASUREMENT, NOT AN ARGUMENT.** The WRONG edit was built and swept over the
  2209 corpus: it differs from the shipped code on **only the three nondeterministic rows** — **the corpus
  is structurally incapable of seeing it** — while **all four hand-written positives go RED**.
  ⊕ **AND MY CLAIM THAT THE `doc_b09_optional_chaining` PANIC WAS UNFILED IS WRONG** — `t0015` names it
  verbatim and `t0720` carries the same site's mechanism.
  ✅ **`t0025` NARROWED, NOT CLOSED** — the link-error half graduated with `check_gg_fails` pinning the
  DIAGNOSTIC TEXT; the `Box[Trait]` rc-139 half and the teaching-diagnostic half each keep their own repro.
  ⊕ Filed `t1020` (SH `Box[UserStruct]` dispatch — **`plain_method` proves it is not four-name-specific**) ·
  `t1021` (SH `shared auto` → int64 carrier; **discriminated from `t0384` because it fires on a `float`,
  which passes that gate**) · `t1022` (**the Core #9 accept/reject divergence — `#[ignore]` + citation +
  filed subset gap, NOT ported**; the port's shape is a NEW function, never a tenth arm) · `t1023`.
  ⚠ **CROSS-TRACK: `scripts/figures.db` `roundish_headlines` 81→82** per
  `done_md_round_close_shapes_are_pinned`. **Every track landing a `DONE.md` entry this round conflicts on
  that ONE line** — K has been warned; I resolve at integration.

- **⚡ F · EXECUTOR RETURNED — `22b8dec2f`, one commit. OUTPUT-REVIEW LAUNCHED; NOT INTEGRATED.**
  Typed `ClosureShape` enum + a per-`(protocol, method)` `CLOSURE_SHAPES` table read through **one accessor**
  (Core #2), SET at the **top** of the arg-lowering loop, `fold`'s accumulator from arg 0's **lowered** type
  via the 3-arg `infer_operand_type_full`. ⊕ `builtin_type_args_from_name` extracted as the **single source
  of truth** for elem/key/val; both closure callers of `extract_elem_type_id_from_type_name` retired, its
  last caller filed as `t0992`. ⊕ Report-only DRIFT branch in `robustness_map.py`.
  **Gates bare:** build 0 · `--lib` **1187/0** · `--test lints` **218/0** · `robustness_map --lanes c,llvm`
  **0, no REGRESSION line** · targeted integration **599/0**. Whole-map **885/1038 = 85.3%**.
  ⭐ **29 CELLS AS 8 PINS / 15 CONTROLS / 6 RED — and the CONTROLS were shown to DISCRIMINATE**: rebuilt with
  `effective_name` (BR2-3's exact executor error) `map`/`filter`/`any`/`all`/`count`/`each` on
  `Vector[String]` **all go BUILD-FAIL.** `ALL_PROTOCOLS` guard RED-verified — dropping the Deque row names
  it and reds both Deque cells while every Vector cell stays green; **corpus Deque HOF coverage 0 → 3.**
  ⛔ **THE BRIEF WAS WRONG IN FOUR PLACES.** (1) ⭐ **ADDENDUM 5's `for_each`-with-import "positive control"
  IS NOT GREEN** — the real discriminator is **import × CALLABLE FORM** (named fn ✅, typed closure ✅,
  **untyped closure ⛔**), and **`t0987` had held the callable form CONSTANT while varying the import** —
  its own v2 correction warns about exactly that. **Third correction to that item; it cost a 29th cell.**
  (2) `t0988` wrong a third time, as predicted — **and the consequence the brief did not state: NO GREEN
  CONTROL EXISTS for it**, because every rc-0 shape is ASan-red, so a `match`-form fixture would be
  green-on-arrival. (3) The DRIFT branch's first run found **9 drifts, 7 of them LLVM `TRAP → CRASH`**,
  attributed to today's `verdict.py` change and filed as **`t0993`**; **419 of the 672 non-good cell-lanes
  remain unmeasured and are NAMED as omitted.** (4) `hof_fold_nonconstant_accumulator` **is C-only** —
  **LLVM compiles it and prints `<abc`.**
  ⚡⚡ **A LANE DIVERGENCE WITH SILENT WRONG OUTPUT, which is why `t0068` went LOW → HIGH:**
  **`find_index` bound — C REJECTS, LLVM SILENTLY ANSWERS `none`.** A silent wrong answer on one lane
  outranks a rejection on the other; **the output-review adjudicates it under Core #8.**
  ✅ **G'S BLOCKER DISCHARGES HERE** — F ships `for_each` RED cells citing `t0987`; the review confirms they
  genuinely pin the no-import trigger before G is released.
  ⚠ **Stated holes:** readiness #4's `_full`-vs-2-arg choice has **no fixture witness** (kept with the TRAP
  comment, **not claimed as pinned**) · **23 of 1038 rows have an EMPTY `llvm` baseline and are ungated on
  that lane** · `Dict.update` is newly in the table with **no corpus cell**.
  ⊕ New filings: **`t0990`** (Deque has NO end-specific API — and the CoW consume-position tables already
  name `push_back`/`push_front`, **which no type declares**: a Core #14 shape) · `t0991` · `t0992` · `t0993`.
  Corrected in place: `t0987`, `t0988`, `t0068`. `reduce` joined `t0068` with **no new id**, as directed.

- **⚡ G's BLOCKER HAS A NAMED DISCHARGE, NOT AN OPEN-ENDED ONE.** G must not land before `t0987`'s repro
  exists, because **G's prelude of `std.iter` erases that defect's only trigger and there is no fallback**
  (Deque rejects cleanly — measured). ⇒ **Track F ships that repro**: its owed-cell set carries
  *"`for_each` WITHOUT the import → stays RED, cites `t0987`"* (`/tmp/brief_F_v2.md` S-vi, re-verified
  end-to-end at pass 4). **So G becomes eligible the moment F INTEGRATES — not on a judgement call.**
  ⛔ **If F's diff arrives without that RED cell, G stays blocked and F owes it before integration.**

- **🧹 WORKTREE HYGIENE (rule 6):** E's and H's output-review worktrees pruned at their tracks' integration;
  two further clean ones are **harness-LOCKED** (`cannot remove a locked working tree`) and will release on
  their own — **do NOT `remove -f -f`**. One carries 23 uncommitted files and is **not** prunable
  (*"branches survive a removal, uncommitted work does not"*). **Disk 58G/453G = 14%, no pressure.**

- **✅ H · INTEGRATED — `cacd3dfce`. Worktree pruned.** Squash-merged (8 commits). Closes **`t0966`,
  `t0054`, `t0824`**; files `t0997`. Gates at the integrated state, bare: build 0 · `--lib` 1181/0 ·
  `--test lints` **220/0/1** · **`known_gaps_census.sh --check` rc 0, "PASS set matches
  `PASSING_ALLOWLIST.txt` exactly"**. ⊕ `AGENTS.md`'s round-close battery gained the three missing CI legs,
  and the **measured-false** sentence claiming it covered every CI target is replaced by a **lint that
  reconciles the list against `ci.yml`**.
  ⭐ **THE EXECUTOR IMPROVED ON AN ERRATUM RATHER THAN OBEYING IT.** Told to restate a stale ggdef offset,
  it chose the *other* option and said why: *"an absolute offset into a file whose header I just edited will
  rot on the next header edit too"* — so the fixture now carries **the COMMAND plus the construct**, not a
  number. **Quote the command, never the value** — the same rule the handover lives under.
  ⭐ **AND IT REFUSED A ONE-LINE SIMPLIFICATION BY MEASURING IT.** Asked to pin `t0997`'s "39" with a script,
  it tried the obvious contiguous-`#[…]`-block matcher first: **it returns 16, not 39**, because a multi-line
  `#[ignore = "… \` continuation does not start with `#[`, silently ending the attribute block. ⇒ it
  shipped the 32-line walker instead. **A scanner that under-counts by 59% while looking correct is worse
  than the prose it replaces.**

- **✅ K · PASS 4 — THE WITNESS CLOSES. DESIGN SOUND A FOURTH TIME. EXECUTOR LAUNCHED (no pass 5).**
  Folded as ADDENDUM 5. Pass 4's own ruling: every finding is scope growth or a figure correction, **none
  resets the streak.**
  ⭐⭐ **THE INDEPENDENT WITNESS WAS IN THE TREE THE WHOLE TIME.** `src/ir/lowering/context.rs:3981-3992`
  carries a pre-existing enumeration — *"FOUR call sites, the complete view-producer read set … walk each
  hit to its GIR producer before adding a sibling"* — whose **W3c row literally states this defect**:
  *"`lower_index_access` place-arm — never consult `returns_view`, carry NO View tag."* **Three passes hunted
  for a witness that was written down in-source.**
  ✅ **PRODUCER POPULATION TOTAL AT THREE**, closed two ways: all **12** `returns_view` methods plus 14 other
  String-producing spellings run through the 64-realloc probe (**only index/slice break**), and **one**
  view-returning callee among all **70** `call_extern` sites in `src/ir/lowering/`.
  ⛔ **`|changed| = 12, NOT 6** — and the axis was mis-named. ADDENDUM 4 used **a different second axis per
  site**, so it was not a product. The honest decomposition is **{producer} × {CONSUMING POSITION}**, whose
  witness is **AGENTS.md's own consuming-position list**. **Six cells appear in NO earlier addendum**:
  field-assign, `v.set`, `v[0] =`, `v.insert`, field-rooted for-source, and for-element field-assign.
  ⭐ **AND THEY REFUTE THE CORE #14 COMMENT FURTHER THAN §2a ADMITTED** — the collection-put hooks cover
  `push`/`put`/ctor-init and **NOT** `set`/`insert`/`v[i]=`/field-assign ⇒ **at least FIVE resting positions
  unimplemented, not 2.** ⊕ **Rule 3's "five boundaries" is itself a SELECTION used as a TOTAL** (its own
  text says *"a local bind, a field init, a closure capture, a collection `push`, a `return`-as-owned"* —
  illustrative). ⊕ Its path is `docs/internals/`, not `docs/devbook/`.
  ✅ **S-D CLOSED AT ZERO — the premise was FALSE.** The self-host does **not** scan character-by-character
  for this construct; an env-gated counter gives **0 for-string sites across all six drivers**, and the
  emitted C is **byte-identical with and without the tag** (lexer 1,013,393 B; lowerer 34,872,774 B).
  ⭐ **A FOURTH ACCIDENTALLY-CORRECT CELL, AND IT IS A LIVE SILENT MISCOMPILE → `t1050`.** `for c in s:`
  where `s` is **reassigned inside the loop** reads a **torn string**: `len` is snapshotted once while
  `iter_local` is a Ptr to `s`'s slot whose `->data` is re-read every iteration. Rust prints **`aZZ`**;
  **ggdef prints `abc`** — definitive (Core #8). ⚡ **SIX Q#4: the case has NO SUBJECT.**
  `E_MutationWhileBorrowed` **does** guard this class — but **only in the mutating-METHOD-CALL arm**, so a
  plain **reassignment**, the only way to mutate a String, reaches no rule at all; **and the collection
  sibling it does cover is already correct.** ⊕ Same filing: the two emissions of `gorget_str_codepoint_at`
  **disagree** — the helper form bounds-checks, **the INLINE form does not, and the inline form is the one
  used** (verified reading `data[19]` off a 1-byte buffer). A heap OOB read and a Layering-rule-3 break.
  ⚠ **ADDENDUM 4's "exactly FOUR runtime call sites" was the THIRD different wrong number** for that figure
  (the truth: **14 static + 2 emitted**, 22 grep hits). Every figure now carries its profile and bare command.

- **⛔ A1-M · OUTPUT-REVIEW: RESERVATIONS (5 blocking). *"The compiler fix is sound and I would integrate it
  as-is; the RECORD that ships with it is not, and one live sibling of the fixed class was missed."***
  All three mandatory gates PASS; `src/` needs **no rework**. Returned to the executor; **NOT integrated.**
  ⭐ **B1 · A LIVE SIBLING, `gg check`-CLEAN, rc 139 ON BOTH BACKENDS, UNFIXED AND UNFILED.**
  `Callable[int(int)] mk() throws String: return (int x): x + 1`. The emitted C is **exactly `t0937`'s
  signature** — `memcpy(__v7, __v2, sizeof(GorgetClosure))` with **no `__gorget_closure_env_alloc` and no
  `.fn_ptr =` anywhere in `mk`**. The site is the **`throws` auto-`Ok` wrap** (`functions.rs:215`), which
  routes through **A1M-PACK-SITE (3/5)** and the packer does not fire. Not a registration-order artifact.
  **Same class, different site ⇒ FIX INLINE** (SIX Q#3 — the enumeration was a selection).
  ⛔ **B2 · SITE 3/5 HAS ZERO BEHAVIOURAL COVERAGE.** Deleting `context.rs:1765` leaves **both** fixtures
  fully green — prelude `Some`/`Ok`/`Error` are already packed by 5/5 before reaching it. **Readiness #4
  holds for 1/2/4/5 and NOT for 3**, and **B1 is the shape that should have covered it.**
  ⛔⛔ **B3 · THE LEAK ROW'S ATTRIBUTION CONTROL IS FALSIFIED AND ITS RETIREMENT CONDITION WILL NOT HOLD.**
  The block tells the owner the enum-payload cells are ASan-clean and the row retires with `t0948`.
  Measured: that control holds for **ONE cell only**; `Some(Some(<lit>))` leaks **8 B / 1** (the outer
  generic enum's drop is emitted and never called) and `Ok(<lit>)` **returned and matched on the CALL
  RESULT** leaks **8 B / 1** (the match arm `memset`s the payload slot; the moved-out binding is never
  dropped — discriminator is **call-result-vs-local**, not Option-vs-Result). ⇒ **≥2 of the 16 records are
  NOT `t0948`; landing `t0948` leaves the row non-empty.** Two unfiled mechanisms.
  ⛔ **B4 · A WRONG COUNT IN THE TEXT THE OWNER WOULD RULE ON** — the block says "20 records"; measured on
  both lanes it is **144 bytes in 16 allocations**. The constants are right; the prose is not.
  ⚖ **LEAK ADJUDICATION: GENUINELY NEW INFLOW, NOT A GRADUATION — the executor was RIGHT to escalate.**
  At `dc29f0faf~1` the fixture is rc 139 with **empty stdout on both backends**; it dies before those
  allocations ever run, and the retired predecessor allocated nothing either. **Class old, bytes new.**
  ⛔ **BUT IT DOES NOT REACH THE OWNER UNTIL B3/B4 ARE CORRECTED** — the attribution and the retirement
  condition are precisely what the owner would be ruling on. **A1's block extended to `t0971`–`t0976`.**
  ✅ **RE-VERIFIED AND CONFIRMED:** every fixture rc 139 on both backends pre-fix · guard RED at
  `4 vs expected 5` · the **B3 cell's honesty on all three legs** (ggdef `7`/`7`, the test asserting the
  CORRECT value, ASan naming the UAF) · the nested-`Some` diagnosis at `exprs/mod.rs:1900-1917` with the
  prelude sibling set **TOTAL** · `t0968`'s two-profile split · **the SH lane byte-identical, zero parity
  inflow** · and neither `t0938` nor `t0873(a)` closed.

- **✅ H · OUTPUT-REVIEW: INTEGRATE — all three mandatory gates PASS; 4 marked errata sent to the executor.**
  The reviewer **re-verified rather than trusted** every load-bearing claim: the leak guard goes RED pre-fix
  **naming all six real cells** with the `gg build` control at 0 throughout · the axis is **rc 101 with
  exactly 7 violations vs 1** for the single filed cell, and the seven are named · **the `int` cells BUILD
  against the broken compiler, so they are CONFIRMED negative controls, not assumed** · `case [s]:` is a
  parse error, so the two omitted cells genuinely **have no subject** · the `-> !` inventory really is **1**
  · the battery lint names **exactly the three** absent CI legs and reds when one is removed.
  ⭐ **THE DOT-PREFIX IS LOAD-BEARING, AND THE REVIEWER PROVED IT THE ONLY WAY THAT COUNTS:** against a
  seeded `.tmpDECOY` the correct counter reads 1 while a skips-dot-dirs variant and a files-only variant
  both read **0** — and **against a VISIBLE decoy the broken counter reads 1 and the control passes
  silently.** The instrument choice, not just the instrument, is what makes the guard real.
  ✅ **CONVERGENCE 21→16 BANKED CORRECTLY AS A COUNTING FIX:** old vs new recogniser on ONE identical tree
  gives exactly five newly-exempt units, **zero newly open**, all directories, all cited bare —
  and `snag52b` is **LIVE-wired** at `integration.rs:7301`; the old regex could not see it because the
  string has no `.gg`.
  ✅ **CORE #9 ⊕ SATISFIED BY HAND-REGENERATION, NOT ASSERTION:** both new top-level fixtures are
  `runtime_parity_corpus` inflow, and the reviewer ran the SH lane itself (`driver --emit-c` → `cc` → run) —
  **byte-identical to the Rust oracle on both**, so no ceiling was raised for own inflow. ⚠ **Confirm the
  release `self_host_runtime_diff` line at round-close; the floors are release-only.**
  ⊕ **`t0054`'s over-specified bisection CONFIRMED** — `if` cells fail too (three of the seven). The filed
  *"the `match` arm is the discriminator"* **was a selection reported as a discriminator.**
  ⊕ **ERRATA (non-blocking, with the executor):** a doc comment detached from its function by an inserted
  test · a lint message still naming `propagate_child_status()`, **which this diff deletes** · the axis
  header's ggdef offset is **body-relative** (318) where the committed file yields `4102..4146` · `DONE.md`'s
  census figures predate H's own `beginner_map` wiring (roster 196 / FAIL 190, not 195 / 189).
  ⚠ **Cross-track checked and CLEAR:** no other agent branch has touched `todo/t0018.md` since
  `74fc27705`; H's edit is a 2-line append, and the annotation is honest — **18 repros, one wired, 17 owed.**

- **A1-I · SCOUT LAUNCHED (the Core #2 half; A1-M's `t0937` half already shipped as `dc29f0faf`).**
  Three carriers, each with **shipped precedent in the file it touches**: a typed field on GIR `Local`
  (precedent `deref_of_owning_param`) · **`StructDef.closure_call_fn`**, populated exactly as `elem_drop_fn`
  and `elem_clone_fn` already are — **both documented as "Replaces the c_lir `elem_drop_fn_for_c_type`
  name-prefix matching", i.e. THIS EXACT FIX HAS SHIPPED TWICE IN THAT FILE FOR THIS EXACT REASON** ·
  `LirFunction.takes_env: bool` for the two per-FUNCTION sites.
  ⛔ **THE SCOUT'S PRIMARY JOB IS THE CENSUS, WHICH HAS BEEN A SELECTION FOUR TIMES RUNNING.**
  `lookup_closure_info` was counted as "2 references" in every draft; it is **11 across 4 files**. Each pass
  found one more *by reading around the previous one* — **nobody ran the total grep.** ⊕ And a whole
  **SECOND runtime-symbol convention is absent from every draft** (`__callable_N` /
  `__gorget_closure_call_N`, minted by `format!` at four sites and decoded by `starts_with` at eight).
  **The scout owes a mechanically-derived TOTAL census with an independent witness, and an EXPLICIT
  A1-I / A2 boundary.**

- **✅ H · EXECUTOR RETURNED — 7 commits on `worktree-agent-af4ec5946c049e114`. OUTPUT-REVIEW LAUNCHED.**
  `t0966` fix · `t0953`/`t0620` rewire · `t0054` graduation + axis · `t0824` battery lint · convergence
  regex · `known_gaps` directory lint · `t0953` erratum. Closes **`t0966`, `t0054`, `t0824`**; files
  **`t0997`** (only ID used — `t0998`–`t1006` unspent). Gates bare: build 0 · `--lib` 0 · `--test lints` 0
  (**220**) · `security` 0 · `c_runtime` 0 · **`known_gaps_census.sh --check` REAL_EXIT 1 → 0** ·
  `staging_move_burndown.sh --check` 0.
  ⭐ **FIRE COUNTS, ALL BURNED TO ZERO:** scratch-dir terminations **21 → 0** (18 raw + 3 `-> !` helper
  calls) — **exactly the reviewers' independent figure** · directory-arm lint **1 → 0** (11 dirs, zero
  collateral as predicted) · battery lint **3 → 0**.
  ⭐ **NINE GUARDS SHOWN RED ON DELIBERATELY BROKEN VARIANTS**, including the two the design turned on: a
  `-> !` wrapper re-added and called (**the leak's original spelling**), and BOTH broken counters against
  the **dot-prefixed** decoy. ⊕ **Each new guard also carries a SCANNER-ALIVE assertion** (`scopes >= 3`,
  `dirs_seen >= 10`, `checked >= 10`) **so it cannot green on a stopped parser** — a class nobody briefed.
  ⭐ **THE BRIEF WAS WRONG IN FIVE PLACES AND THE EXECUTOR SAID SO.** (1) `propagate_child_status` **cannot
  survive as a wrapper** — with all three arms fixed it has zero callers and `-D warnings` makes it a hard
  error, so it is **DELETED** and the `-> !` inventory is **1, not 2**; ADDENDUM 2 BLOCKING A's worry is
  moot, though the lint still enumerates the class repo-wide with a pinned count. (2) **`t0054`'s own
  bisection was over-specified** — it called the `match` arm the discriminator; the axis shows **every
  non-`int` cell except the three `bare` ones fails, `if` included**. (3) SCOPE F's "23 candidates" is
  rule-dependent — the executor's rule returns **39**, split 30 loud / 9 silent, **7 of the 9 false
  positives of the word "leak"**; two real residuals, all 39 dispositioned in `t0997`.
  ⚡⚡ **(4) IS A METHODOLOGY FINDING AND IT IS NOW A RULE.** The executor's own verification probe was wrong
  twice before it was right: `lower_rethrow_expr` carries the sibling guard and spells
  `ctx.set_owned(builder, err_local);` **identically apart from indentation**, so a SUBSTRING-anchored
  deliberate break silently hit the **sibling** and reported *"no-op `set_owned` → still green, the brief is
  wrong."* **The brief was right; the probe was wrong.** ⇒ **AGENTS.md Core #13 SHARPENED IN PLACE: anchor a
  deliberate break BY LINE, never by substring.** Same class as reading an exit code through a pipe.
  ⛔ **COUNTING CORRECTION — DO NOT BANK AS R49 CONVERGENCE.** `convergence.sh` known_gaps **21 → 16** is the
  bare-directory spelling fix, **not** five closures; zero newly-open gaps hidden.
  ⚠ **CROSS-TRACK, relayed to F:** `beginner_map` was **not deleted or moved**; the directory lint caught it,
  `ALLOWED_UNWIRED` was forbidden, and a red tree is unshippable, so H **wired ONE cell** (`p18.gg`, the only
  one whose intended output is not a design question) and annotated `t0018` with what remains owed.
  **F can revert it with one `continue` if it wants a different disposition.**
  ✅ H re-measured the leak on **E's rewritten** `hof_call_env_leak_unbounded` body: **identical** — both
  spellings are non-capturing, so nothing in the pin rested on the `it` spelling. **The census went green
  exactly as Track C predicted.**

- **✅ E · OUTPUT-REVIEW: INTEGRATE, WITH TWO PRECONDITIONS — both sent back to E's executor.**
  All THREE mandatory gates PASS. Every gate re-run bare by the reviewer: `check --all-targets` · `--lib` ·
  `--test lints` · `spec_conformance` · `security` · `-p ggdef` · `build --release` ·
  **`self_host_bootstrap_fixed_point`** · `resolver_comparison` · `c_emit_comparison` · the three new
  fixtures · `fmt_` 183/0 — **all exit 0.**
  ✅ **THE EDIT I FLAGGED AS HIGHEST-RISK IS CLEARED.** `ex_shopping_cart`'s `expected` string is
  **byte-identical to the fixture's BIRTH COMMIT `86feb444a`**, to the deleted fossil companion's MANIFEST
  row, and to hand-derivation (150×2 + 250×1 = 550). The `.gg` file is untouched by the diff. ⇒ **derived
  from the record, then confirmed — NOT fitted to the binary. `t0934` satisfied.**
  ✅ **REMOVAL TOTALITY REGENERATED:** across **4685** committed `.gg` files with comments stripped, a bare
  `it` token survives in exactly **7** — the 2 new fixtures, `ex_shopping_cart.gg`'s loop variable, and 4
  hits **inside string literals**. Zero in `docs/`, `lib/`, `spec/prose`. No symlink converted to a real file.
  ✅ **`t0977` PROVEN NOT INTRODUCED BY THIS DIFF, by construction** — its repro contains no `it` token, and
  every `src/` hunk removes only `Expr::It`/`ImplicitClosure` arms. ⊕ It is **mutually consistent** with
  `t0954`, whose own example IS `t0977`'s cell 4.
  ✅ **ALL THREE LEAK RATCHETS MOVED DOWN** (`LEAK_CEILING`, `LEAK_CLASS_PAIRS`, `LEAK_RECORDS` — read the
  current values off `tests/lints.rs`; `UNCITED_LEAK_CLASS_PAIRS` correctly held, the deleted pair being
  cited). `python3 scripts/figures.py --validate` 0 errors, `--scan` 0 discrepancies. ⊕ **The reviewer ran
  the two grow-only `matched` floors the executor did NOT re-measure** —
  `cargo test --test integration --release resolver_comparison` and `… c_emit_comparison`, each against
  `RESOLVER_MATCH_FLOOR` / `C_EMIT_MATCH_FLOOR` — **both hold with wide slack. The direction is the point:
  deleting a MATCHING fixture LOWERS a floor, unlike the ceiling, which is invariant.**
  ⛔ **PRECONDITION 1 — cite **D54**.** The lint `ratified_decisions_are_cited_in_the_spec` reds at 26 vs
  `BUDGET = 25`, which **already sits at ZERO headroom** (the reviewer re-implemented the population parse
  and reproduced the 25-name roster exactly). Citation goes in E's own paragraph at
  `language-reference.md:2039-2044` **and** at `language-design.md:1550`, where E wrote D54's rationale.
  **Conflict-free — only E's branch touches those files, and its docs diff adds/removes no `D<N>` token.**
  ⛔ **BUDGET STAYS 25.** ⚠ **A STANDING CONSEQUENCE FOR THE HANDOVER: with uncited pinned at the ceiling,
  EVERY future ratification must ship its spec citation IN THE SAME COMMIT or this lint reds.**
  ⛔ **PRECONDITION 2 (BLOCKING) — the tenth lint's finding is named in `DONE.md` but NEITHER GUARDED NOR
  FILED.** `self_host_cow_write_walkers_share_one_root_peel` (`tests/lints.rs:7076-7084`) computes
  `decided = NO_MUTABLE_PATH ∪ write_arms` and asserts only `undecided.is_empty()` — **a STALE name in the
  hardcoded roster only GROWS `decided`, so it can never fail.** Core #6 (both directions) + SIX Q#2.
  **The round fixed the INSTANCE and left the CLASS open.** ⚡ **Discharge = the 4-line reverse assertion**
  (every `NO_MUTABLE_PATH` name must appear in `variants`); **verified green today and RED on the pre-clean
  tree — its own Core #13 demonstration.**
  ⚠ **Commit `13793b85b` is RED IN ISOLATION** (`todo_index_is_current`). **I SQUASH-MERGE at integration** —
  fixes the bisect hazard without rewriting E's branch; E adds the preconditions as a new commit on top.
  ⊕ **SCOPE carried:** `figures.db:930`'s caveat still justifies **294** while the row now reads 293
  (`--where 294` → 33 spellings, `--where 293` → 7) — **one row got Core #5, its neighbour didn't** ·
  **the NEW doc example at `language-design.md:1557` is itself a live `t0977` instance** (prints a raw
  pointer), which is the strongest argument for that item's HIGH severity — cross-reference it ·
  `beginner_map`'s retired rows still read `bucket = WORKS` (Track F's call).

- **⛔ K · PASS 3 — 2 BLOCKING. DESIGN SOUND A THIRD TIME; BOTH MY FOLDS NAME THE WRONG WRITE SITE.**
  Folded as ADDENDUM 4; **streak reset, pass 4 launched.** ⛔ **THE ENUMERATION HAS NOW FAILED THREE TIMES,
  EACH TIME BECAUSE THE WITNESS WAS OVER THE WRONG POPULATION** — runtime functions (pass 1) · `index_load`
  call sites (pass 2) · **and the producer is a `call_extern`, not an `IndexLoad` at all** (pass 3).
  ⛔⛔ **`for c in s:` REACHES THE RUNTIME VIA `call_extern gorget_str_codepoint_at`
  (`for_loops.rs:728-732`), and `for_loops.rs:940` — which BOTH my folds named — IS `lower_for_array`,
  i.e. `for x in Vector[String]`. AN EXECUTOR FOLLOWING EITHER FOLD WOULD HAVE EDITED AN INERT LINE.**
  Proof is in pass 2's own emitted C: an inlined `gorget_str_view_region` with **no `gorget_str_index` call
  anywhere.** ⊕ The real site is **`for_loops.rs:742`**, and **S-C measured tagging `:940`/`:1166` to be
  behaviourally INERT** — claiming them would break readiness row 4.
  ⭐ **SIX Q#4 LANDS THE OPPOSITE WAY FROM MY FOLD: the for-element is not UNTAGGED, it is MIS-TAGGED.**
  `bind_owned_for_drop(..., LoopOwned::Fresh)` stamps **FreshOwned on a `cap=0` view** — **the worst
  possible tag**, because FreshOwned+dead is the one shape Tier 2a green-lights unconditionally
  (`validate.rs:2955`, `:3066`). **That is why the class was invisible.**
  ⚡ **CORE #14 COMMENT NUMBER FOUR, MEASURED FALSE** (`for_loops.rs:734-741`, *"run-proven sound … boundary
  clones upgrade cap=0 views on entry"*). ⭐ **The CTOR row refutes it: `v.push(c)` materializes,
  `Holder(c)` does not.** Both fixed by a `:742` View tag; ggdef and LLVM agree.
  ⛔ **P2 · THE TAG DOES NOT CLOSE THE `.enumerate()` ICE — the root is a DIFFERENT defect.**
  `infer_collection_element_type` (`methods.rs:4819`) has **no `GorgetString → GorgetString` arm** and falls
  through to `I64_TYPE`, so a String `.enumerate()` types its element **i64** and nothing in K can reach it.
  ⭐⭐ **AND IT IS WORSE THAN AN ICE — THE THIRD ACCIDENTALLY-CORRECT CELL:** the loop bounds on **BYTE**
  length while `gorget_str_index` indexes by **CODEPOINT**, so `for i, c in "héllo".enumerate()` **TRAPS**
  while plain `for c in s:` prints correctly. **ASCII is green; multibyte traps — accidentally correct on
  every ASCII fixture in the corpus.** ⇒ `:1166` is a **NAMED OMITTED CELL**; root filed as **`t1049`**
  (reference-lags-self-host — SH's `lower_loops.gg:466` carries its own codepoint-ordinal counter and gets
  it right).
  ✅ **P3 SETTLED BY MEASUREMENT: `set_view_of`, NOT the Borrowed tag** — a `Borrowed{CollectionElement}`
  String matches **no** clone branch and falls to **Branch F → `AssignMode::Move`, a move out of a borrow**.
  ✅ **HONEST `|changed| = 6`**: 4 at `methods.rs:4762` (including the heap-rooted **`h.s[4]`** index cell
  nobody had measured) + 2 at `for_loops.rs:742`.
  ⛔ **S-B · MY OWN ADDENDUM-2 FIGURE CORRECTION WAS FALSE** — pristine HEAD `--lib` is **1185 / 0 / 0** and
  `src/` has **ZERO** `#[ignore]` attributes, so "1183/0/2" was impossible. ⚡ **EVERY FIGURE MUST NOW CARRY
  ITS PROFILE AND ITS BARE COMMAND.**
  ⚠ **S-D · AN UNSIZED COST: the `:742` tag charges ONE CLONE PER ITERATION, and the self-host scans source
  text CHARACTER-BY-CHARACTER.** The executor must measure the **bootstrap's** `string_clone` and peak RSS
  before/after — not the 1000-bind microbenchmark. **Pass 4 is measuring it.**

- **⚡ A1-M · EXECUTOR RETURNED — `dc29f0faf` on `worktree-agent-af3d2f03b79e6b6b8`, 4 files +205/−8.
  OUTPUT-REVIEW LAUNCHED; NOT INTEGRATED.** `pack_closure_at_dest_type` in `calls.rs`, **FIVE** call sites
  each marked `A1M-PACK-SITE (n/5)`, destination predicate on `GirType::FnPtr` + a `GorgetClosure` alias and
  **source predicate on `TypeMetadata::is_closure_env` — no name-matching** (Core #2). **Closes `t0937`.**
  Gates bare: build 0 · `--lib` 1185/0 · `--test lints` **219/0 (+1 new)** · `-p ggdef` 0 · `spec_conformance`
  0; targeted C **and** LLVM across callable/closure/option/result/struct/tuple/enum/sh_ all 0.
  ⭐ **IT IS FIVE SITES, NOT FOUR — the fifth is a SECOND, DISTINCT DEFECT at the same layer.** A nested
  `Some(Some(<literal>))` resolves the INNER enum's type from the ambient `expected_type`, i.e. the **OUTER**
  `Option[Option[Callable]]` — GIR shows `enum_init Option__Option__Callable__GorgetClosure::Some` for the
  *inner* init. **Invisible with an `int` payload** because `Option__int64_t` is pre-registered so the
  fallback never runs (SIX Q#6). Fixed through one shared helper for `Some`/`Ok`/`Error` (Core #4);
  `Some(Some(lit))` rc 139 → `2` on both backends.
  ⛔ **THE B3 CELL IS NOT AN IMPROVEMENT AND THE EXECUTOR SAID SO FIRST.**
  `known_gaps/callable_capture_overlap_aggregate_init_uaf.gg` goes rc 139 → **rc 0 printing `7` then `70`**;
  correct is `7`/`7` (ggdef agrees). Still `heap-use-after-free in __Closure_0__call` under C+ASan. Shipped
  as an `#[ignore]`d cell asserting the **CORRECT** value, cited to `t0704`. **That is Core #8 self-applied.**
  ⚡⚡ **NEW OWNER ASK — LEAK INFLOW, and the executor refused to slip it under a standing ruling.**
  `callable_literal_at_consuming_positions` leaks **16 records / 144 B** (`__gorget_closure_env_alloc`),
  identical C and LLVM. **All three leak ratchets move UP** (`LEAK_CEILING`, `LEAK_RECORDS`,
  `LEAK_CLASS_PAIRS` in `tests/lints.rs`, mirrored in `scripts/figures.db` — regenerate with
  `python3 scripts/figures.py --validate`). **The class is old; the BYTES ARE NEW** — at HEAD those positions never
  constructed the closure, so they allocated nothing. The 2026-09-02 ruling admits *"pre-existing leaks newly
  made VISIBLE by a graduation"* **and only those**; *"a row whose leak is genuinely NEW inflow is still an
  owner ask."* **The output-review adjudicates first; if it agrees the bytes are new, this goes to the owner.**
  ⊕ Attribution control: the enum-payload cells of the same fixture are **ASan-CLEAN**, pinning the row on
  `t0948`'s struct predicate rather than on this fix.
  ⊕ **`t0968` SPLITS ACROSS COMPILER PROFILES:** `Ok(Some(<literal>))` ICEs at `lir/validate.rs:137` in
  **debug** but **builds and prints correctly under `--release` while leaking 8 B** — the validator is
  `cfg!(debug_assertions)`-gated. Both halves owed; the `#[ignore]`d test is RED in debug (the census
  profile) and green in release.
  ⊕ **Filed: `t0968` · `t0969`** (a `Callable[String(String)]` struct field SEGVs on self-host where Rust is
  correct on both backends — **the signature, not the position**: `int(int)`/`int()`/`int(int,int)` MATCH) ·
  **`t0970`** (the SH typechecker types a closure PARAMETER from a **later** same-named local —
  **order-dependent**, so a scoping defect, not a shadowing rule).
  ⚠ **POSITIVE CONTROL RUN WITH THE FIX (S7 confirmed exactly):** A1-M takes A3's hazard surface from ONE
  spelling to FOUR — three cells that looked safe were only **masked** because ASan halts on an earlier
  stack-buffer-overflow. A **hard gate** is appended to `t0948`: it does not land without a read-side
  materializer for `Callable g = <aggregate>.f`.

- **⚖ OWNER RULING 2026-09-03 — ALL THREE LIVE ASKS ANSWERED: *"Take the reference grade (starred) option
  on each ask."*** The parked three (`t0844` · `t0842`(A) · `t0863`) stay parked; `t0947`'s direction needed
  no ruling (Track C pass 5 confirmed `decisions.md:2081-2093` already answers it).
  1. ✅ **THE `s[a:b]` CEILING IS RETIRED AND REPLACED BY AN EQUIVALENCE GUARD (option C).** Track K
     **deletes** the `CEILING = 0` lint at `tests/lints.rs:18406-18410` — not merely leaves it `#[ignore]`d
     — because its premise is measurably dead (the clone reclaim it existed to drive measures **ZERO**), it
     **fights D22 Rider 2**'s permanent aliases, and as a shrink-only ratchet it **cannot go RED on a
     shrink** (Core #6 needs both directions). In its place: **assert `s[a:b]` and `.slice(a,b)` cost the
     SAME — both `string_clone == 0` at a TEMP, both `== 1` at a BIND — failing in EITHER direction**,
     instrumented with `--clones=stats` (**never** stdout, **never** ASan — blind here). **Verify the
     verifier: revert the tag on ONE spelling and show the guard fire.** Folded as brief_K ADDENDUM 3;
     supersedes §7's "report, do not decide".
  2. ✅ **THE `it`-REMOVAL LEDGER ENTRY IS RATIFIED AS DRAFTED (option A) — it becomes D54.** The
     `E_UndefinedName` retirement note stays IN the entry: it is the migration path, and in a comment it
     would rot.
  3. ✅ **D22's FIRST RIDER IS AMENDED IN PLACE (option A) — conclusion stands, mechanism refuted.**
     `.slice()` is **FREE at a temp** and materializes only at the **bind**; the "1,002 vs 2" figure was
     bind-vs-bind and **its "2" IS the dangling-view defect**. ⇒ the migration's clone-reclaim yield is
     **ZERO**, which is why `t0316`/`t0850` withdrew the 205-site recommendation.
  ⛔ **BOTH LEDGER EDITS ARE PREPARED, NOT APPLIED — `docs/define-gorget/decisions.md` IS OWNER-EDIT-ONLY.**
  Ready to paste verbatim at **`/tmp/owner_ledger_drafts_2026-09-03.md`** (D54 goes at the TOP of `## LOG`,
  ~line 519; the D22 amendment goes INSIDE the first rider at `:1310` — **amend in place, never a second
  dated copy**).

- **📋 F-2 · SEEDED at `/tmp/brief_F2_seed.md` (M1b · M2 · M4 · M5). ⛔ NOT A BRIEF — a SCOUT runs first.**
  Every premise dates from F's ADDENDUM 2 or earlier and must be re-verified at HEAD (Core #5).
  ⭐ **M1b IS THE HIGHEST-VALUE HALF AND IS THE REFERENCE-GRADE VERSION OF WHAT F JUST SHIPPED.** An
  **untyped** closure param gets `self.fresh_type_var()` at `typecheck.rs:4460` and **is never unified**, so
  **the whole body escapes type checking**: `Callable[int(int,int)] f = (a, b): a.age - b.age` is
  **`OK: no semantic errors`**, builds, and **prints `0`** — while the annotated twin correctly gives
  `E_NoFieldFound` ×2. **F's M1 fixes six cells and leaves this class fully live** for every untyped closure
  whose hint is absent or wrong. Core #1 names the write site: **the CHECKER owns a parameter's type**;
  `closure_param_type_hints` is a sidecar reconstructing downstream what the checker should have resolved.
  ⭐ **`devbook/25` ALREADY DOCUMENTS THIS CLASS — with a guard SCOPED TO `clone`. SIX Q#2: it green-lights
  the class it was written to retire. WIDEN IT; do not invent a new mechanism.**
  ⚠ **Sequencing:** **M4 collides with F's landed M1 on 3 of its 25 census rows**, so F-2 re-measures the
  census **AFTER F integrates**, and retires those rows. ⚠ **M2's `pair_joinwords_c_fold` is a CoW
  consume-site defect, not an iterator one — and its `v[1:3]` shape is now Track K's neighbourhood**, so the
  scout checks whether K's landing moves it. ⛔ **The MANIFEST `actual` column is NOT evidence** (it names
  the wrong error for `doc_b04_ownership_modes`); every M2 cell is re-measured BY BUILDING IT.
  ⛔ **M3 and the two live ggdef findings went to F-G, not here.**

- **✅ C · PASS 5 SIGNED OFF (THIRD CONSECUTIVE) — EXECUTOR LAUNCHED.** Folded as ADDENDUM 5.
  ⛔⛔ **AND IT FOUND A ROUND-LEVEL RED THAT IS NOT TRACK C's.** `known_gaps_census.sh --check` run in full:
  **the gate is ALREADY RED on the integration branch at `908ea0ad6`** — **two pre-existing unallowlisted
  passers**, `catch_binding_throw_in_match_arm_ice` and `hof_call_env_leak_unbounded`. ⚡ **BOTH ARE TRACK
  H's** (H2 graduates the first, H3 REWIRES the second) — **H is executing them now.** The second is a
  textbook Core #12 violation shipped by an earlier landing: its `#[ignore]` reason says the gap is *"visible
  only under `--sanitize` + LeakSanitizer"* while its body is `run_gg(..., "120")` — **a stdout assertion
  that can never observe a leak.** ⛔ **Track C is NOT charged for them, and C's executor is briefed NOT to
  use the census as an instrument until H lands.**
  ⛔ **A SECOND AUTO-PEEL CELL, WORSE IN REACHABILITY THAN THE FIRST:** a payload **without** `is_error` —
  **HEAD builds and prints the CORRECT output; the PAIR is build rc 1.** A **build regression on a program
  correct at HEAD**, needing **no** user-defined four-name method, only the ordinary
  `auto x = <method returning Result[UserType, E]>` idiom. **ADDENDUM 4 pinned only the has-method variant.**
  ⭐ **THE MECHANISM, NAMED AT LAST — and it is visible in the emitted C.** `auto j = s.join(g)` lowers to a
  **PEEL**: `__bb1` extracts `Ok_0` into a `Payload` slot and binds `j` to it. **So LOWERING binds the
  payload while the CHECKER binds the `Result`** (D45 pin 6: an unmarked position never peels) — `t0947`/
  `t0434` exactly. **`x_payload_ok.PAIR.c:3760` emits `Payload__is_error(...)` where HEAD emits no call.**
  ⭐ **THE CLASS IS BOUNDED, NOT UNBOUNDED — `|pinned| == |changed|` IS ACHIEVABLE.** Full payload-type axis
  measured (7 rows): only two cells change; `Result[int,_]`, `Result[String,_]`, `Option[Payload]` and the
  **explicit** `Result[…] j = …` binding are all **unchanged** — **the peel is Result-only and `auto`-only.**
  ⭐ **CORPUS WITNESS (row 2, mechanical):** across **4408** `.gg` files, those containing **both**
  `auto X = …` and `X.is_{some,none,ok,error}()` = **ZERO** ⇒ **the corpus is STRUCTURALLY INCAPABLE of
  seeing this class**, so no green fixture regresses and no sweep bounds `|changed|` for it.
  ✅ **THE SHIP ARGUMENT, CHECKABLE AT OUTPUT-REVIEW:** by the owner's ranking the pair moves **no cell UP** —
  A is silent-wrong → silent-wrong (lateral); A(2) and `t0947` are silent-wrong → **build failure, BELOW
  silent-wrong-output**. Step 1 fixes the track's highest-severity cell (`String is_some(&self)` delivering
  `Bool(false)` into a `String` slot) plus **six** healthy provenances, and `t1019` closes a real Core #10
  hole. **Core #8's teeth are about a defect the round CREATED and PRETENDED WAS FINE; this one is filed,
  RED-verified, and moves DOWN the ranking.**
  ⊕ **SIXTH FIX ROW, in no table before:** a trait-BOUND generic param (`bool probe[Checkable T](T a)`) is
  **HEAD `false` → PAIR `true`** — §2 listed the mono'd generic param as a BREAK row; **with the bound
  providing the method it is a FIX.**
  ⚡ **REQUIRED OF THE EXECUTOR:** the **FULL 2209-fixture build+run+stdout-hash sweep** — **the check-only
  sweep is MEASURED STRUCTURALLY BLIND to the pair's worst class**, and the gate table topped out at 258.
  ⛔ **DO NOT CLOSE `t0025`** — it narrows; **both** halves stay live (`Box[Trait]` rc 139 on HEAD *and*
  PAIR, and the teaching-diagnostic half the pair does not deliver).
  ⚠ **SH:** step 1 needs **no port** (Rust converges onto SH); `t1019` **is** a genuine lag creating an
  accept/reject divergence this round — port as a **NEW FUNCTION**, not a tenth arm
  (`sh_reject_wrong_receiver_combinator_arms_count` pins the set at **9**), or `#[ignore]` + citation +
  subset gap.

- **✅ F · PASS 6 SIGNED OFF — EXECUTOR LAUNCHED. Six passes; the design needed none of them, MY FOLDS did.**
  Pass 6 built the REAL patch from ADDENDUM 6 + 7 B1, measured it, RED-verified the guard, reverted clean.
  `cargo build` 0 · `--lib` **1186/0** · `--test lints` **218/0** · `robustness_map --lanes c` **rc 0,
  6 PROGRESS, 0 regressions, WORKS 848/1009** (HEAD 842). Six M1 cells **6/6**; `fold("",…)` threads String;
  `fold(0,…len())` → `5` where HEAD ICEs.
  ⭐ **THE NEW GUARD CAUGHT ONE OF MY ERRORS ON ITS FIRST RUN.** The `ALL_PROTOCOLS`-iterating test (in
  `collection_protocols_have_full_metadata`'s shape, `builtins.rs:1285`) is **green as written and RED on a
  dropped Deque row** — and it revealed that **there is no protocol named `GorgetStringView`**:
  `GORGET_STRING_VIEW.base_name` is **`"GorgetString"`** (`builtins.rs:821`), so ADDENDUM 6's *"it does not
  collide ✓"* **verified a non-existent entry.** Core #6 working exactly as intended.
  ⚡ **SCOPE-4 DECIDED — M1b · M2 · M4 · M5 ARE STRUCK FROM F and become TRACK F-2 IN THIS ROUND.** Since
  ADDENDUM 3 every pass reviewed **only M1 + the scorer**; M2/M4/M5 have had **no pass since ADDENDUM 2**,
  and M4 carries an unresolved collision with readiness #3 explicitly uncheckable. **A split is division,
  never deferral. F ships M1 + the report-only scorer and nothing else.**
  ⛔ **S3 · MY "`_pp_`, NOT `_vv_`" WAS A BLANKET AND IS FALSE — the symbol is PER-CELL.**
  `vec_sort_structs_comparator` and `vec_sorted_structs_comparator` **do** emit `_vv_`. Saying it
  unqualified is exactly the failure the fire-count correction existed to prevent.
  ⛔ **S4 · `#[ignore]` IS A CATEGORY ERROR for robustness-map cells** — they are not cargo tests.
  `expected` = INTENDED output · lane column = measured broken bucket · `note` = citation. **The wrong
  reading pushes an executor toward editing `COL_EXPECTED`, which `t0934` FORBIDS.**
  ⛔ **S2 · NEW AXIS GAP — the accumulator is sampled at ONE VALUE (Core #12).** The carrier's whole
  justification is *"the accumulator is a CALL-SITE property"*, yet every measuring cell uses a **literal**
  accumulator; `Vector[String].fold(<non-constant>,…)` is BUILD-FAIL at HEAD **and stays BUILD-FAIL**.
  Unfiled ⇒ cover it or file `t0991`.
  ⚠ **S1 · the `_full`-vs-2-arg choice has NO WITNESS** — byte-identical on every probe. Keep `_full` + the
  TRAP comment, but **do not claim a fixture pins it.**
  ⊕ IDs: **`t0990`** = `Deque[String].push_back` → `E_NoMethodFound`; **`t0991`** = the accumulator axis.

- **⛔ K · PASS 1 — 4 BLOCKING, 5 SCOPE. Root cause, layer and direction RIGHT; the ENUMERATION, the
  "already correct" list, the COST claim and the FIGURES all fail.** Folded as ADDENDUM 1; **streak reset to
  0, pass 2 launched.** ✅ **Independently confirmed:** both-backend repro · **`ggdef run` → `abcde`**
  (Core #8 satisfied) · **ASan blindness VERIFIED, not assumed** (`--sanitize` + `detect_leaks=1` → rc 0, no
  report, garbage stdout) · prototype fixes all 6 cells on C AND LLVM · **readiness item 4 PASSES** ·
  the cited repro **resolves nowhere** · the self-host mirror is real and at the same layer.
  1. ⛔ **THE TOTALITY WITNESS IS OVER THE WRONG POPULATION.** It counts **runtime functions**
     (`gorget_str_view_region` sites vs `returns_view` entries) while **the fix is at a LOWERING site**, so
     it **structurally cannot see a second lowering site reaching the same helper** — and there is one:
     **`for c in s:` binds via `index_load_borrow` (`for_loops.rs:940`, `.enumerate()` sibling `:1166`) with
     NO `set_view_of`.** Broken at baseline **and still broken under the prototype**, both backends;
     `ggdef` prints the right answer. **Not in the changed set, not in the "already correct" list, not filed
     anywhere.** ⚡ **Core #4: the changed set EXTENDS to those siblings.** ⊕ And the count is **14, not 16**
     (a forward declaration and a definition were double-counted).
  2. ⛔⛔ **THE `&`-ROUTE CELLS ARE NOT "ALREADY CORRECT" — THEY ARE GREEN BECAUSE OF A LEAK** (SIX Q#6, the
     round's sharpest). `--clones=stats` on the `&` route: **`string_clone=0, string_cow=64,
     live_bytes=32640`** ⇒ **nothing is materialized; the bind is the SAME untagged dangling view, and it
     prints correctly ONLY because the 64 superseded buffers are NEVER FREED.** ⚠ **Given owner-binding
     leak-freedom, fixing that leak turns EVERY `&` cell into a UAF.** The leak is separate (reproduces with
     **no slice at all**) and **UNFILED** ⇒ **`todo/t1048` from K's block.**
  3. ⛔ **THE COST CLAIM IS WRONG, AND MY DIRECTIVE WOULD HAVE RE-CORRECTED A SETTLED RECORD.** `.slice()`
     is **FREE at a TEMP (0 clones)** and materializes only at the **bind** — so "same cost as `.slice()`"
     holds only for binds, and **the reclaim yield after the fix is EXACTLY ZERO at every measured
     position.** And `t0850:113-117` / `t0316:55-59` **already carry the correction verbatim**; my §4 order
     to "correct that line" is **STRUCK** — it would have made a third dated layer on a settled point.
  4. ⚡ **A RATIFIED-LEDGER CONTRADICTION → OWNER ASK (below).**
  ⛔ **S3 · the prototype's view-source provenance is WRONG for a field-rooted base** — it records the ROOT
  LOCAL (`h`), not the field path, while **the sibling five lines away (`methods.rs:4766-4773`) already uses
  `extract_field_path_string`.** Safe today only because the bind clones eagerly; wrong under Layering
  rule 3 and **must not be inherited.**
  ⛔ **S4 · the Core #14 finding is UNDER-scoped — there are TWO unguarded comments**, and the second
  (`methods.rs:4659-4665`, *"even a NAMED bind … would dangle"*) **is made false by the fix.**
  ⛔ **S1 · `slice 421/0` IS A MISLABELLED SUM** (189 string + 222 cow + 10 slice); the slice suite is
  **10/0/1**, and `--lib` is **1183**, not 1185. ⛔ **S2 · the `clone_meter_*` worktree trap again.**
  ⚠ **S5 · the SH obligation is too weak** — §6's fixtures GRADUATE, so the owner's 2026-08-10 rule binds:
  **compile + MATCH on the SH lane the SAME ROUND; raising the ceiling for own inflow is forbidden.**

- **⚡ E · EXECUTOR RETURNED — 3 commits on `worktree-agent-a0f9f2926c665565b`, 91 files, +589/−1917.
  OUTPUT-REVIEW LAUNCHED; NOT YET INTEGRATED.** `13793b85b` removes the implicit `it` closure parameter
  (`Keyword::It` / `Expr::It` / `Expr::ImplicitClosure` gone; **207 self-host lines across 25 files**, all
  four representations regenerated to **0**; `LiftedClosure.is_implicit` deleted — written at two sites,
  read at none). `58668de53` docs+ledger; `4e12d85a6` spells `t0977`'s glyphs as words.
  **Executor gates, bare:** `cargo check --all-targets` 0 · `--test lints` 0 (218/0) · `--lib` 0 (**1181/0**
  = 1185 − the 4 deleted `it` parser tests) · **`self_host_bootstrap_fixed_point` 0** · `spec_conformance` 0
  · `security` 0 · `sanitize_sweep.sh` 0 · `robustness_map.py` 0 · `-p ggdef` 0 · all five `*_comparison` 0.
  ⛔ **THE HIGHEST-RISK EDIT, SELF-FLAGGED:** `ex_shopping_cart`'s `expected` column was an **ellipsis
  placeholder** and was replaced with a real string sourced from the fixture's header comment + a deleted
  fossil companion. **`t0934` binds those rows — never edit an expectation to match what the compiler
  prints.** The output-review must adjudicate it from the header and git history, **not** from the binary.
  ⚠ **Commit 1 is RED IN ISOLATION** for `todo_index_is_current` (three `git rm`'d items rode along; the
  regenerated `TODO.md` is in commit 2). Tip is green; squash-at-integration is the open call.
  ✅ **ZONE EDGES HELD:** `integration.rs:6532` (Track H's assertion) **untouched** — only the `.gg` body and
  the two comment sentences the rewrite falsified; `beginner_map`'s corpus **not** deleted (F's call), but
  one row struck from `FINDINGS.md`'s *"all nine rejections are correct"* table, which the change falsified.
  Two one-line deletions in Track C's `context.rs` / `stmts/mod.rs`.
  ⭐ **FILED `t0977` (HIGH, silent wrong output):** `auto out = v.map(<closure>)` **reads back a raw pointer**
  when the closure's result type differs from the source's and is a heap `String`. Six cells; **the four
  CORRECT ones are the finding.** Same symptom as `t0823` γ1a, whose R48 fix covered only the same-type
  case. **Claimed NOT introduced here** — the review verifies that against the pre-diff compiler.
  ⊕ **`t0978`** files the typed retired-keyword table (`mod` was retired years earlier with no note at all).
  ⚡ **OWNER LEDGER ENTRY OWED — the executor correctly did NOT touch `decisions.md`:** *the implicit `it`
  closure parameter is removed from the surface; `it` is an ordinary identifier at every naming position; a
  single-parameter closure names its parameter like any other, with the type omittable where context fixes
  it; `E_UndefinedName` on `it` carries a retirement note that displaces the edit-distance suggestion.*
  ⚠ **`figures.db`:** no floor moved, so the parity waiver cascade never fired (the ceiling is **invariant**
  under deleting a matching row — both terms of `non_excluded − matched` drop). It **did** cascade on the
  leak side: the `class_pairs` row now carries `scan = none` with a stated spelling caveat and the
  `records` row an exact waiver of 1 (`scripts/figures.db`; regenerate with
  `python3 scripts/figures.py --validate` and `--scan`). ⚠ **The review checked those are honest and not
  a tolerance band greening its own drift (Core #6) — and found ONE of the two stale: the `class_pairs`
  caveat still justifies the PRE-MOVE value, so the stated reason for disabling its scan is now false.**

- **⛔ C · PASS 4 — 3 BLOCKING, 5 SCOPE. DESIGN SIGNED A SECOND CONSECUTIVE TIME; READINESS ROWS 2 AND 3
  FAIL.** Folded as ADDENDUM 4; **pass 5 launched, scoped to confirming the fold.** Pass 4 reproduced the
  4348-fixture sweep EXACTLY (one changed row) and **discharged the whole previously-unverified gate list**
  (`-p ggdef` 187/0 · `spec_conformance` 3/0 · `security` 213/0 · **LLVM lane** · `robustness_map --lanes c`
  rc 0 zero-regression · a **258-fixture** union sweep, superset of the brief's 161, **one differing row**).
  1. ⛔ **THE PAIR CREATES A THIRD SILENT-WRONG-OUTPUT CELL — a MISCOMPILE that outranks every link-error
     cell in the track.** `auto j = s.join(g)` returning `Result[Payload, String]` where `Payload` carries
     its own `is_error()`: HEAD prints **nothing**; the PAIR prints `handled`/`custom message` **by
     dispatching `Payload__is_error` — the PAYLOAD's method — on a value the source treats as a `Result`.**
     The output is decided by the payload's method, not the Result's error state. **`t1019` cannot see it:
     at check the method legitimately EXISTS on the `Result`.**
     ⭐ **ROOT CAUSE IS MY CLASS LABEL.** Class (3) said *"no usable type name (Callable / closure / mono'd
     param / `bool`)"* — **but `bool` and `Payload` BOTH HAVE usable type names.** The real discriminator is
     **"the receiver's LOWERING type DISAGREES with the CHECKER's type"**, which is **unbounded**. The
     partition I recorded as *"PROVABLE, not sampled"* is falsified.
  2. ⛔ **THE SELF-HOST DISPOSITION WAS BACKWARDS.** SH already DISPATCHES all four names correctly and
     returns the right type; **step 1 CONVERGES RUST ONTO THE SELF-HOST and needs NO port.** Addendum 1's
     *"genuine divergences needing a PORT"* would have **broken a correct lane** (§ Self-host: fix Rust as
     oracle hygiene). ⊕ **But `t1019` IS a genuine SH lag** — after the pair Rust rejects at check while SH
     accepts ⇒ **a Core #9 accept/reject lane divergence CREATED THIS ROUND that the brief never named.**
  3. ⛔ **The SH site enumeration is a SELECTION** — two more four-name sites (`typecheck.gg:4861`, which
     `self_host_lowerer/typecheck.gg` SYMLINKS; `lir_lower.gg:2117`).
  ⛔ **S1 · AN `#[ignore]`d TEST STARTS PASSING AND A CI GATE REDS.** The census does **set equality**
  against a 6-row allowlist whose count is asserted EXACTLY (`lints.rs:22608`) ⇒ graduate it.
  ⛔⛔ **DO NOT CLOSE `t0025`** — its `Box[Trait]` half is **still rc 139 on HEAD and PAIR**; it narrows.
  ⛔ **S2 · The row-4 instrument samples ONE value of a typed axis** — `MyOpt(7)` leaves `is_none`/`is_error`
  **accidentally correct**; only `MyOpt(7)` + `MyOpt(0)` + `MyOpt(-1)` cover all four names.
  ⛔ **S3 · The `--sanitize` cell is the WRONG INSTRUMENT** — both sides ASan-clean because nothing is
  dereferenced; **the stdout compare is the discriminator** (Core #13).
  ⛔⛔ **S5 · ENVIRONMENT TRAP FOR EVERY FRESH WORKTREE:** three `clone_meter_*` lints fail with
  `fatal: detected dubious ownership`. **Fix first: `git config --global --add safe.directory "$(git rev-parse --show-toplevel)"`.**
  ✅ **Q1 SETTLES THE `t0947` ERRATA INDEPENDENTLY** — `decisions.md:2081-2093` reads exactly as `7275d56f9`
  says. **No owner ask.**

- **⛔ F · PASS 5 — 3 BLOCKING, 5 SCOPE. DESIGN HOLDS FOR A FOURTH PASS; ALL THREE BLOCKERS ARE IN MY
  FOLD.** Folded as ADDENDUM 7. **Streak reset to 0; pass 6 launched, SCOPED to confirming the fold, not
  re-litigating the design.**
  ⛔ **THE PATTERN IS NOW THE ORCHESTRATOR'S, NOT THE TRACK'S — three folds this round shipped the defect
  the next pass had to catch.** ADDENDUM 6's code block **does not compile**: `infer_operand_type_full`
  (`type_reg.rs:263`) takes **THREE** params and the fold passed two. ⛔⛔ **And the natural repair is the
  silently-wrong sibling** — the two-arg `infer_operand_type` (`:285`) scans only `ctx.locals_iter()` and
  returns **`I64_TYPE` for a builder-only temp**, i.e. **exactly the `fold("",…) → a` silent-wrong class the
  whole carrier redesign exists to prevent.** I promoted the prescription to code *because prose had
  misfired three times* — and then did not compile it. **ROOT CAUSE: I folded pass 4's ILLUSTRATION (with
  `…` elisions) AS A PRESCRIPTION.** ⇒ **AGENTS.md's FOLD VERBATIM rule is SHARPENED IN PLACE: a fold that
  prescribes a WRITE SITE or ships CODE cites the reviewer's MEASURED PROTOTYPE, never a retyped snippet.**
  2. ⛔ **Readiness gate #1's FIRE COUNT is a wrong number** (Core #5): the post-fix symbol is
     `__gg_synth_sort_impl_s8_pp_i64`, **not `_vv_`** — an executor grepping the brief's value finds nothing
     and concludes the mechanism did not fire.
  3. ⛔ **The DEQUE hazard is understated and READINESS #2 AND #3 BOTH FAIL.** `builtins.rs:441` aliases
     `DEQUE.methods = VECTOR.methods`, so a base-name-keyed table silently drops Deque. Measured:
     `Deque[String].each` is **green at HEAD** (a silent-regression cell) and `Deque[String].sort_by_key`
     goes **BUILD-FAIL → green** (an UNPINNED progress cell) — while
     `grep -l Deque tests/fixtures/robustness_map/cells/*.gg` returns **ZERO**, so the whole-C-lane evidence
     **cannot witness Deque in either direction.**
     ⭐ **AND THE BRIEF THREW AWAY AN AVAILABLE WITNESS.** B-4's arm-count retirement does **not** transfer:
     it retired six tables with six domains; this one has **one** domain, and **`ALL_PROTOCOLS`
     (`builtins.rs:1167-1176`, 30 entries) is an independent in-repo witness.** The repo already carries the
     pattern to mirror — **`collection_protocols_have_full_metadata` (`builtins.rs:1285`)**, whose own
     doc-comment describes this exact failure mode. ⇒ **Remedy folded: an `ALL_PROTOCOLS`-iterating unit
     test + a Deque control + a Deque pin** (Core #6, SIX Q#2).
  ✅ **Independently reconfirmed, no Core #5 finding against pass 4:** closure is arg 0 in all six cells ·
  top-of-body placement **6/6** · `robustness_map.py --lanes c` **rc 0, 6 PROGRESS · 0 REGRESSION · WORKS
  848/1009** · the one-reader claim holds (2 setters, **1 reader**, 1 field decl) · every B3 classification
  reproduces · **the five new cells DO discriminate the carrier swap** (rebuilt with BR2-3's exact wrong
  binding they go BUILD-FAIL) — **label them CONTROLS, not pins** · `t0988`'s refutation reproduces exactly.
  ⚡ **S2 DECIDED — genuinely disjoint, so FILE don't fix:** `Deque[String].push_back(…)` →
  `E_NoMethodFound`, because the aliasing gives Deque **no end-specific API at all**. **`todo/t0990` from
  F's block.**

- **✅ K · SCOUT RETURNED — BRIEF WRITTEN (`/tmp/brief_K.md`), PASS 1 LAUNCHED. It is a ONE-LANE fix.**
  Repro CONFIRMED at HEAD both backends; **ggdef prints the correct answer on every row** and `.slice()` is
  out of its subset, so **ggdef covers only the colon form — the broken one — and adjudicates both Rust
  backends DEFINITIVELY WRONG.** Not a "both backends agree" ambiguity (Core #8).
  ⭐ **RUST GG LAGS THE SELF-HOST — the succession-plan case.** `self_host_lowerer/lower_expr.gg:5293`
  (slice) and `:5403` (index) already call `add_local_with(..., LoView(), ...)` **with a comment naming this
  very UAF.** Fix Rust as oracle hygiene; never dumb the self-host down to match.
  ⛔ **RUNTIME MECHANISM:** `gorget_str_slice` returns `gorget_str_view_region` — `cap=0`, `alloc=NULL`,
  borrowing the source buffer (`runtime_string.c:738,750-753`) — while `gorget_array_slice` allocates +
  memcpys + deep-clones (`runtime_array.c:462-496`). **String is the only broken container BY CONSTRUCTION.**
  ⛔ **ASan IS STRUCTURALLY BLIND** (custom `__gorget_current_alloc` pool) — **a green sanitize sweep says
  NOTHING here.** stdout is the only instrument.
  ⛔ **TWO AXES `t0871` NEVER NAMED, and one FAKES A NON-REPRODUCTION:** the `&`-sig-arg mutation route
  (`grow(&s)`) is **ALREADY CORRECT**, so an executor probing that way sees green and concludes wrongly —
  use INLINE REASSIGN. Second axis: field-rooted receivers (`h.s[0:5]`), broken and fixed by the same tag.
  ⭐ **TOTALITY WITNESS:** 16 `gorget_str_view_region` call sites × 12 `returns_view: true` entries
  (`builtins.rs:853-875`) — every view-returning helper is method-tagged **except the two the index route
  reaches untagged**. Derivation axis **total at 2**; **|changed| = 6** = {slice, index} × {bare-local bind,
  field-rooted bind, return}.
  ⚡ **COST MEASURED: +1 clone per BIND, +12 KB (~1%) RSS; TEMPS STAY FREE** (identical before/after).
  ⭐ **AND IT KILLS A PREMISE:** this makes `s[a:b]` cost the same as `.slice()`, so **`t0316`/`t0850`'s
  "1,002 vs 2 MEASURED FREE" clone-reclaim premise EVAPORATES — it was free because it did not do the work.**
  ⛔ **THE CRITICAL ITEM HAS NO DURABLE REPRO:** `t0871`'s cited
  `known_gaps/string_index_slice_bind_dangles.gg` **was never committed on any branch** and no `#[ignore]`d
  test exists — a Task Continuity violation the executor closes.
  ⚠ **Core #14:** `methods.rs:4726-4730` asserts *"boundary clones own it when it escapes"* — **false for 2
  of Rule 3's 5 boundaries. It is WHY the defect survived.** Guard it or delete it.
  ⚠ **All of `t0871`'s line cites drifted** (`methods.rs:3748`/`:4762`/`:4662-4663`, not `:3475`/`:4394`/
  `:4291`), **and both decision cites are stale** (D22 Rider 2 is `decisions.md:1311` not `:1278`; D52 is
  `:3191` not `:3158`). Its "slice 398/0" gate figure is stale too (**421/0**).
  ⛔ **THE CEILING IS AN OWNER ASK, NOT THE TRACK'S CALL** — `t0871` says do not restore it, and D22 Rider 2
  keeps `.slice()`/`.substring()` as PERMANENT ALIASES, so whether a ceiling is wanted **at all** is open.
  **The lint stays `#[ignore]`d; the track reports and does not decide.**
  ⊕ **OUT OF SCOPE, stated so nobody pulls them in:** `t0697` is its OWN track (root
  `pack_trait_object_for_smart_ptr_ctor`, `calls.rs:1038-1041` — assign + `set_owned_fresh`, never consumes
  the source; **no shared root**). And the fifth Rule-3 boundary — closure capture of a reallocating
  `String` — **reproduces with NO SLICE and through the METHOD route `t0871` calls correct**, so it is the
  `t0704`/`t0771` family. **`t0704`'s scope erratum is FILED** (its `mechanism` said "captured COLLECTION
  handle"; the class is wider — a captured plain `String` corrupts the same way).

- **✅ H · PASS 3 SIGNED OFF THE DESIGN (0 blocking, 8 scope + 6 errata). EXECUTOR LAUNCHED.** Folded as
  `/tmp/brief_H.md` ADDENDUM 3. **Both ADDENDUM 2 blockers are closed, and the termination-spelling
  enumeration is now TOTAL with independent witnesses per row** — `process::exit` (18) · `-> !` helper (3,
  and `grep -rn -- "-> !" src/` finds **exactly two** such fns repo-wide) · `abort`/`libc::_exit`/`.exec()`/
  `into_path()`/`mem::forget` all **0**, and `Cargo.toml` has no `panic="abort"` so `panic!` unwinds and
  Drop runs. ⇒ **21 of 21 caught today.**
  ⭐ **THE CONTROL IS NOW PROVABLY FALSIFIABLE, AND ITS SHAPE IS LOAD-BEARING:** the leaked artifact is a
  **HIDDEN** dir (`/tmp/…/.tmplhc6AG`), so the decoy must be **dot-prefixed** — a visible-named decoy passes
  a counter that skips dotdirs and catches nothing. Measured against two deliberately-broken counters: the
  dot-prefixed decoy reds both. **All three arms leak exactly 1 dir at HEAD; `gg build` produces not even a
  FILE** (readiness item 4, independently measured).
  ⭐ **`main.rs:3714`'s comment is FALSE ON TWO COUNTS** — `trace_filename` comes from
  `input_path.parent()`, i.e. beside the SOURCE, never inside `tmp_dir`.
  ⚡ **DECIDED AND SHIPPED TO THE EXECUTOR:** widen the lint's subject to the repo-wide `-> !` inventory +
  a closed literal set + an expected-count assertion (else factoring the helper elsewhere greens the guard)
  · **no ctor-closure exemption — convert all three, zero-tolerance** · **one ERROR-PATH cell per arm**
  (all four prototyped cells assert success, so **18 of 21 sites never execute**, and the `return n` failure
  mode is pinned by nothing) · the TUI's hand-rolled `env::temp_dir().join("gorget_tui")` at `main.rs:2147`
  is **not** the `t0966` class but the `grep tempfile` witness is blind to it — state its disposition ·
  bound the leak-gap pass to the two named rows and **file the other 21 as `t0997`** · the SH fallback is
  file-a-subset-gap + `#[ignore]` + citation · the census regex needs a **terminating boundary** (`*`/`{`)
  or `known_gaps/beginner_map*` in prose would silently exempt a real unit — **the guard green-lighting its
  own class**.
  ⚠ **Two readiness gaps handed to the executor:** H2/H3's byte-and-rc figures are **pass-0 numbers no later
  pass regenerated** (Core #5); and H1's conversion has 21 changed / 3 pinned at runtime.

- **⛔ F · PASS 4 — 3 BLOCKING, 7 SCOPE. DESIGN SIGNED FOR A THIRD CONSECUTIVE PASS; MY OWN FOLD IS WHAT
  BROKE.** Folded as `/tmp/brief_F_v2.md` ADDENDUM 6 (precedence 6 > 5 > … > BODY). **Streak reset to 0;
  pass 5 launched.**
  1. ⛔ **Addendum 5's R2 placement is MEASURED-FATAL.** It told the executor to set the closure hints
     **after** `lower_call_arg` in the arg-map body. **In all six M1 cells the closure IS arg index 0** —
     only `fold` has an arg before it. Prototyped: that placement is **byte-identical to HEAD, 0 of 6
     fixed**; the top-of-body placement is **6/6 correct**, whole C lane **0 REGRESSION, 0 NEW DIVERGENCE,
     EXIT=0, WORKS 842→848**. ADDENDUM 6 folds the corrected placement **AS CODE**, so the executor never
     has to know the closure's index — the ambiguity that has now misfired three times.
  2. ⛔ **`t0988` is wrong for the THIRD time.** A 24-cell grid at HEAD finds a shape that **compiles and
     does NOT double-free**: `match v.find((s): s.len() == 3): case Some(s):` with the `std.iter` import
     builds, prints `abc`, rc 0 — and under `--sanitize` **leaks 8 B at `__gorget_closure_env_alloc`**,
     `t0953`'s class. **The refuting cell is one the item's OWN AXIS omits** (`match`-scrutinee and
     `if … is` are absent from its shape list). SIX Q#3, verbatim, for the third version of one item.
  3. ⛔ **The owed fixture set does not pin what it claims.** 13 green + 5 RED = **18**, not the stated
     19-20 · `Dict.fold`/`Dict.filter` **as written are green at HEAD** (controls, not pins — the pinning
     spelling is `d.fold(0, (acc,k,v): acc + k.len())`) · `reduce` on String has **NO filed item** and joins
     `t0068` (no new id) · "Deque HOF" is too wide (only `.map` fails; cite `t0058`) · **HashSet HOF is
     probably not RED at all** — an `#[ignore]`d cell there would pin a PASSING program · and **the five
     cells the carrier swap most needs are ABSENT** (`map`/`filter`/`any`/`all`/`count` untyped on
     `Vector[String]` — the ONLY witness for the `protocol_for_mangled_name` swap).
  ✅ **CORE #9 FOR M1 IS ANSWERED — note-and-cite, not port.** The six cells fail on the SH lane for four
  reasons, **none of them the closure hint**, and `grep -rn "param_type_hint" tests/fixtures/self_host_*/`
  returns **ZERO** — there is no hint mechanism to mirror. Both blockers already filed (`t0166` S-D,
  `t0167` S-B(b)). ⊕ **S1 DECISION: map cells + `known_gaps/` ONLY** — a new top-level `tests/fixtures/*.gg`
  auto-joins `self_host_runtime_diff`, which is F's own inflow against a ceiling forbidden to raise.
  ⊕ **S3 DECISION: M3 SPLITS OUT to the F-G track (same round).** It has no write site, no diagnostic text
  and no floor plan; S-i's two live findings move with it. **F ships M1 + the report-only scorer.**

- **✅ C · PASS 3 — DESIGN SIGNED OFF, AND THE HEADLINE RISK IS DEAD.** The `t1019` check-side rule was
  BUILT and swept check-only over **all 4348 fixtures**: **exactly ONE changed row**, and it is
  `known_gaps/equip_paren_trait_spelling_silently_dropped.gg` — **`t0025`'s own repro, whose filed INTENDED
  is "`gg check` REJECTS"**. **Zero healthy rejections; zero of mechanism 5's 57.** 17 lines at one site;
  **no split needed.** The discriminator mechanism 5 lacked is **`DefKind::GenericParam`** — scoping to
  `Struct|Enum|Newtype` at `typecheck.rs:3528` leaves all 26 `iter_*` fixtures unchanged (independent
  witness). Folded as ADDENDUM 3; **pass 4 launched as the confirming pass** after 3 blocking CLAIM
  corrections:
  1. ⛔ **ADDENDUM 2's R8 IS FALSE — and it was the brief's only answer to its own §2b.** Under the pair
     `t0947` is **still a raw linker error with no diagnostic**, not `E_NoMethodFound`. **No
     method-existence rule can ever reach it:** the CHECKER binds `j` as `Result[bool,String]` while
     LOWERING binds it as `bool` — a check-vs-lowering **type DISAGREEMENT**, not a missing method.
     §3's original disposition (`t0434`'s `expr_types` plumbing) **was right, and my fold overturned it
     without measuring**. ⊕ **This corroborates the `t0947` header errata at `7275d56f9`:** the checker
     already binds the `Result`, which is the direction D45 pin 6's re-gate mandates.
  2. ⛔ **The pair does NOT close "all six residue cells".** `Callable` param and `Callable &` param still
     print `0` — the two cells the brief ranks WORST. They belong to `t0942`; owe both as RED-verified
     `known_gaps` citing it, and correct the verdict sentence.
  3. ⛔ **The row-4 instrument was unavailable AND unnecessary.** `Plain`/`Color` is check-rejected under
     the pair so it cannot be a runnable fixture — and the wrong edit was built: `MyOpt` `true|false|false`
     and `OptionalConfig` `true`, **both identical to HEAD**. The two positives the track already owes
     discriminate it perfectly; readiness row 4 is satisfied by them.
  ⭐ **A TOTAL PARTITION replaces the sampled table:** receivers reaching the gate with `opt=false` are
  exactly (1) user type with no equip → `t1019` rejects, (2) user type WITH the method → step 1 dispatches,
  (3) no usable type name → `t0942`/`t0947`, out of reach. **The compiler's own existing `E_NoMethodFound`
  is the independent witness that (1)-with-equip is unreachable — totality PROVABLE, not sampled.**
  ⊕ **Attack #4 DECIDED: the re-infer predicate is `Type::Inferred`** (`context.rs:2044-2047`), not
  `actual != UNIT_TYPE` — the latter is a read-site shape test that lets an inferred type override a type
  the user WROTE (Layering 1, Core #2). Prototyped: three cells fixed, two unchanged BY CONSTRUCTION.
  ⊕ **`t1018` contamination NARROWED:** only trait-provenance cells need the multi-line workaround; the two
  fixtures C owes are bare-equip and may use natural single-line bodies.
  ⚠ **STILL UNVERIFIED AFTER THREE PASSES:** LLVM · `-p ggdef` · `spec_conformance` · `security` ·
  `--sanitize` · SH driver-embedded copies · `robustness_map.py`. **The executor runs these.**

- **⛔ H · PASS 2 RETURNED 2 BLOCKING + 7 SCOPE — STREAK RESET TO 0; pass 3 launched.** Both blocking
  findings are **SIX Q#2 — a guard that cannot catch its own class**, folded as `/tmp/brief_H.md`
  ADDENDUM 2 (precedence: 2 > 1 > LATE CORRECTION > BODY).
  1. ⛔ **The lint misses the `-> !` HELPER spelling, which is the leak's ORIGINAL spelling.**
     `propagate_child_status` is `-> !` at `src/main.rs:40`; its call sites `:2633` `:3335` `:3715` sit
     inside all three destructor-bearing scopes. A `process::exit(`-only grep misses **3 of 21**, and the
     fix KEEPS the helper as a wrapper — so the next site reintroduces the class and the guard stays green.
     ⚡ The repo already has the convention (`src/main.rs:51` `LINT-CHOKEPOINT-FALLBACK`): enumerate the
     `-> !` fns, never special-case the one name (Core #2).
  2. ⛔ **The `gg build` negative control is VACUOUS, and `t0966` states a FALSE mechanism.** `gg build`
     **never creates a tempdir** — `tempfile` appears at `src/main.rs:2596/3288/3510` only, and the build
     arm passes `None` for scratch (`:3219`). So `t0966`'s *"its tempdir is dropped normally"* and its
     `t0840` corroboration are false (Core #5): **it leaks 0 because it MAKES NONE.** The cell reads zero
     pre-fix, post-fix, and with a deliberately broken counter — **a control that cannot fail is not a
     control.** Executor owes the item-correction AND a seeded-decoy control in the same commit.
  ✅ **The lint sibling is now PROTOTYPED with a real fire count** (21 at HEAD → 18 with the proto patch)
  and it **independently rediscovers S2**: the surviving row is `src/main.rs:3343`. S3's escalation is
  discharged; no further design work on the lint.
  ⭐ **SCOPE folded:** conversion set is **21 edits (18 raw + 3 helper), 13 `unwrap_or_else`** — S1's
  *"19 of 35"* counted a set S9 forbids touching · `impl Drop` audit CLEAN (none of `RawModeGuard` /
  `CallArgGuard` / `ExprDepthGuard` is live in the three scopes — the premise that makes the IIFE safe, and
  no pass had stated it) · Costume 2 needs the `return` spelling **and** an expected-COUNT assertion, plus
  a `gg test` integration test (both existing ones are `gg run`) · `t0054` is `lane = "self-host"` so
  `self_host_gaps/` would dodge its own ask — hand-run the SH lane · **B3's 16 cells all run at HEAD, no
  ICE, and two `Vector[String]` match cells have NO SUBJECT** (Gorget has no vector pattern; `match e.len()`
  moves the scrutinee) — name the omission, never substitute · `if matches!(err_mode` has **TWO** sites
  (`exprs/mod.rs:4698` + `:4863`), the brief cited its own list · **the brief's targeted gate goes RED as
  written** — needs both timeout knobs, and the filter substring-matches `sh_*` · S6's "14 sites" is stale
  (**18**) · `.gorget/` is gitignored so B5's collision is result-file interference only.

- **✅ H · HOLD LIFTED — OWNER, 2026-09-03: *"please unblock track H. the other agent work is not
  usable."*** The external `t0824` fix did not arrive.
  ⭐ **ALL H4 PREMISES RE-VERIFIED AT HEAD ON RESUME (the rule this hold set for itself) — EVERY ONE HOLDS
  UNCHANGED, so the brief stands as written and needs no re-scoping:**
  `scripts/known_gaps_census.sh --check` read BARE → **`REAL_EXIT=1`**, roster **195 · PASS 8 · FAIL 187 ·
  SKIPPED_SH 0**, and the **same two** un-allowlisted extras (`catch_binding_throw_in_match_arm_ice`,
  `hof_call_env_leak_unbounded`) · all three CI steps still absent from the battery
  (`grep -c` in `AGENTS.md` → **0, 0, 0**) · `AGENTS.md`'s *"The full battery covers every target CI runs"*
  still present and still measured-false · and `git log` shows **nothing landed** on
  `scripts/known_gaps_census.sh`, `todo/t0824.md`, `.github/workflows/ci.yml` or `AGENTS.md` during the hold.
  ⚠ **THE STREAK RESTARTS AT ZERO** — its pass-1 review was STOPPED mid-flight, and a stopped review is not
  a passed review. Re-run ≥3 fresh sequential passes from pass 1.
  ⊕ Unaffected by the hold and unchanged: **H1** (`t0966`, fix + guard prototyped, RED in both directions) ·
  **H2** (the `catch_binding` graduation, both assertion halves shown load-bearing) · **H3** (the
  `hof_call_env_leak_unbounded` REWIRE + the orphan filing) · **H5** (the `convergence.sh` bare-directory
  over-count of 5) · **H6** (two unwired `known_gaps` fixtures).

  ⚠ **LIVE CROSS-TRACK DEBT (carried forward from the lifted hold):** Track **E** was briefed that **H owns
  the assertion** at `tests/integration.rs:6532` while E owns the `hof_call_env_leak_unbounded.gg` body.
  **H is now EXECUTING and does own it** — `/tmp/brief_H.md` ADDENDUM 3 erratum **E3** assigns it explicitly,
  including the `#[ignore]` string at `:6529-6531` that H3's own rewire makes false. **E must NOT silently
  take it.**

  ---
  **[H's scoped work, now LIVE — the executor is running against `/tmp/brief_H.md`, whose ADDENDA 1-3
  supersede everything below. Kept here for the retraction lesson it carries.]**
- **H · GRADUATIONS + HYGIENE. ⚠ RE-SCOPED BY ITS OWN SCOUT — AND MY CENSUS CLAIM WAS FALSE.**
  ⛔ **RETRACTED: I wrote that `known_gaps_census.sh --check` "prints ✗ and then exits 0" and therefore
  "cannot gate anything." THAT IS WRONG — it exits 1 and gates correctly.** Re-measured bare:
  `REAL_EXIT=1`. The `exit 0` I reported came from my own wrapper ending in a PIPE, so `$?` was the
  pipe's — **the `${PIPESTATUS[0]}` trap from devbook/30 §21, which bit THREE TIMES in this session
  alone** (this claim; my `cargo test … | tail` gate, which committed a RED lint; and the scout's own
  task notification). ⇒ **Read an exit code off the BARE command, never through a pipe.** Nothing in the
  script masks it: `exit $rc` at `:343`, `set -uo pipefail` without `-e`, EXIT trap only `rm -f`.
  ⇒ **`t0824`'s battery ask is NOT inert — it works today. And `t0824` UNDERCOUNTS BY 3×:** three CI
  `run:` steps are absent from the round-close battery (`grep -c <name> AGENTS.md` → 0 for each) —
  `known_gaps_census.sh --check` (`ci.yml:131`, **rc 1, RED right now**) · `staging_move_burndown.sh
  --check` (`:153`, rc 0) · `cargo test --test c_runtime` (`:163`, rc 0). CI invokes the census BARE, so
  **CI is red at HEAD while the documented battery can be all-green.** ⊕ The sibling `--check` audit found
  NO other script with the defect I hypothesised — `convergence.sh` and `resolver_totality.sh` exit 0 **by
  documented design**, so do NOT "fix" them. ⊕ One real, small census defect: `--check <name>` does not
  drop unrun rows from the expected side the way `--fast` does, so a filtered check reports 6 phantom rows.
  **THE TWO PASSING ROWS, adjudicated by MECHANISM (fix sites broken, both directions watched):**
  - **`catch_binding_throw_in_match_arm_ice` → GRADUATE.** Not accidentally green: both assertion halves
    are load-bearing. No-op'ing `ctx.set_owned` (`exprs/mod.rs:4864`) returns the filed ICE (rc 101,
    *"untracked source consumed"*); no-op'ing `drops.register_local` (`:4871`) returns the leak (LSAN 2 B).
    LLVM lane green. ⚠ **The graduation OWES `t0054`'s axis fixtures (Core #12)** — live pins are only 2
    droppable-enum cells, while `t0054` promises `{int, String, droppable enum, Vector[String]}` ×
    `{bare, if, match arm, nested match}`. Six cells verified clean at HEAD and ready to become them.
    ⚠ UNVERIFIED: `t0054`'s *"`throw e` inside an `if` → OK"* did not reproduce in the scout's spelling
    (`E_TypeMismatch` — an `if/else` block is not an expression there); use `t0054`'s OWN spelling.
  - **`hof_call_env_leak_unbounded` → REWIRE, NOT graduate.** ⭐ **SIX QUESTIONS #6, confirmed by
    measurement: it is green for a reason unrelated to what it tests.** It is `run_gg(…, "120")` — a
    STDOUT assertion for a LEAK gap — and was GREEN ON ARRIVAL; its own text says so. **The leak is LIVE
    at HEAD: 160 bytes in 20 allocations, all `__gorget_closure_env_alloc`.** Sibling-site drift inside
    one item's own evidence set: `t0953`'s other repro 174 lines away (`:6706`) already uses
    `assert_gg_sanitize_clean`. One-line rewire to match; demonstrated RED. **`t0953` stays OPEN** — this
    fixes the EVIDENCE, not the bug. ⊕ **Fold in `t0620`'s twin**: its own text says wiring an ASan twin
    IS its graduation signal, and its 40 B `gorget_weak_upgrade` leak is live. Same class, same helper —
    do ONE pass over *"leak gaps pinned only on the value lane."*
  - **`t0966`** — mechanism CONFIRMED, all three arms leak, and the fix AND guard are already prototyped
    and measured by the scout. **The item has been corrected in place** (its first exit-site set was wrong).
  ⛔ **THERE IS NO BULK-GRADUATION HARVEST — DO NOT BRIEF ONE.** The census IS that instrument and it has
  already run the whole population: roster 195, **PASS 8 · FAIL 187**, `SKIPPED_SH 0`; of the 8 PASS, 6 are
  adjudicated allowlist rows and 2 are the rows above. The scout then attacked the 76 units the census is
  structurally blind to (live-only wiring) and checked nine items by hand — `t0009` `t0132` `t0139` `t0338`
  `t0474` `t0620` `t0462` `t0463` `t0532` — **all OPEN, zero graduations.** *A live-only fixture means one
  CELL was fixed, not that the item closed.* Regenerated: 820 items, **226** cite `known_gaps` (not my 215).
  ⭐ **REAL FINDING — `scripts/convergence.sh` OVER-COUNTS `known_gaps` BY 5.** Its citation grep is
  `known_gaps/[A-Za-z0-9_/]+\.gg`, so it **cannot see a bare-directory citation**, while
  `known_gaps_census.sh`'s enumerator explicitly recognises both spellings. Measured **23 → 18**; the five
  are `beginner_map`, `gorget_arena_snag_1_llvm_ffi_only_typedef`, `manifest_malformed`, `snag52b`,
  `snag58_private_int_import`. `snag52b` is doubly wrong — it is LIVE-wired at `tests/integration.rs:6884`.
  ⛔ **THE −5 IS A COUNTING CORRECTION AND MUST NOT BE BANKED AS R49 CONVERGENCE** — `convergence.sh`'s own
  header rules that it *"lands BETWEEN rounds and belongs to the NEXT baseline."*
  ⊕ **Two `known_gaps` fixtures have NO wired test**, violating the durable-repro contract:
  `known_gaps/beginner_map/` — a SCOUT CORPUS (`FINDINGS.md` + 32 KB `MANIFEST.tsv` + 18 programs)
  committed to the repo, which AGENTS.md keeps `/tmp`-only and `git rm`s on sight (`t0018` owns it and its
  own text says the wiring is OWED) — and `known_gaps/sh_targ_recorder_pregate_nested_positions.gg`, which
  appears only as a bare string in a `tests/lints.rs:23790` membership list.

### 🧹 R49 OPEN — WORKTREE / DISK STATE (done 2026-09-03, supersedes R48's carry-forward block)
The 14 carried-forward agent worktrees are **PRUNED**. Each one's uncommitted work was COMMITTED to its
own `worktree-agent-*` branch first (rule 6: *branches survive a removal, uncommitted work does not*), then
all 14 were correlated against pending items. **13 carried nothing that is not already on main and their
branches are deleted.** ⭐ **ONE IS KEPT: `worktree-agent-a4a5d1e5b9113c3e5`** — 5 fixtures that exist
nowhere on main (`known_gaps/unsafe_equip_lane_divergence.gg`, `known_gaps/sh_parse_errors_discarded_where.gg`,
`reject_use_after_unsafe_scope.gg`, `sh_unsafe_block_is_not_a_drop_zone.gg`,
`sh_unsafe_block_tail_and_nesting.gg`), against **PENDING `t0727`–`t0732`**. Spend it before re-deriving.
✅ **R48's flagged `agent-aa19c1e589090caae` is RESOLVED, not lost:** its `sanitize_empty_sort_*` set is
SUPERSEDED — `t0780` closed 2026-08-31 with a better artifact (`security/sort_empty_collection_no_ub.gg`
+ the `emitted_qsort_is_guarded` lint, which catches its own class). Do not re-derive it.
**Disk: `/tmp` 79 GB → 2.9 GB, overlay 122 G → 45 G used.** 93% of that was `t0966`'s leak.

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

### ⚠ ORCHESTRATOR ERRATA — R48's are in the devbook, not here
`docs/devbook/30-excellence-system.md` **§21** carries them: the brief that decayed faster than it
could be executed (T-a2 — six review passes, a rebuild, and NOT ONE blocking pass found a design
defect); reaching a class by resemblance three times on one defect; merge with `--no-commit`, verify,
THEN commit; disjoint todo-id ranges for concurrent tracks; a guard green wherever its blind spot is
its own execution environment; `${PIPESTATUS[0]}` and `git -C`; and the four times this round's own
new guards caught the orchestrator hours later.
⇒ **Errata are WAR STORIES and belong in the devbook, where they survive.** The handover is rewritten
every round close by mandate, so anything recorded only here is deleted by the next one — which is why
this pointer exists instead of the list.

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

### ⚖ PARKED OWNER ASKS — ask when the work is SCHEDULED, not before
- **`t0844`** — `process_group(0)` + SIGINT forwarding: user-visible Ctrl-C semantics in the shipped
  compiler.
- **`t0842`(A)** — `wait_timeout` returns `-2` and does **not** kill the child, so a Gorget program leaks
  a timed-out child *by design*. Unratified language surface.
- **`t0863`** — robustness divergences left **UNACCEPTED** (5 more added at R48 close, all one signature:
  C and self-host agree on accept-then-build-fail, LLVM merely unclassifiable via `t0646`). Verify the
  prior per-lane grade before folding.

### 🗂 14 AGENT WORKTREES CARRIED FORWARD (R47 keep-list, verified at R48 close)
All of R48's OWN track/review worktrees were pruned at integration (clean). What remains is the R47
keep-list, still DIRTY with real uncommitted work — rule 6 keeps them: *branches survive a removal,
uncommitted work does not.*
⚠ **THE TWO FLAGGED ENTRIES ARE NOW SPENT and no longer block anything:**
- `agent-a619349ec03b80e93` — the F3 scout lost to the disk crash. **Its head start WAS used**: the
  recovered prototype seeded R48's memory-safety tracks and `t0770`/`t0772`/`t0763`/`t0134` are closed.
- `agent-aa19c1e589090caae` — F2's `sanitize_empty_sort_*.gg` fixtures, against `t0863`/`t0572`. Still
  unspent; check before re-deriving.
⚠ Verified at R48 close: `agent-a0e6b997f0aec720a`'s staged work (`todo/t0727`–`t0732` plus their
fixtures) is **ALREADY ON MAIN** — so at least one is a stale duplicate and the set is smaller than 14 in
substance. A future close may dispose of them, but only file-by-file.
⚠ **58 leftover BRANCHES** (`git branch | grep worktree-agent`) are the residue of every agent this round
and prior; they cost nothing and hold the only copy of some captured work. Not cleaned at this close.
⚠ **Do not run `scripts/round_cleanup.sh` without a keep-list** — it prunes EVERY `agent-*` worktree.

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
- [`t0013`](todo/t0013.md) **HIGH** — 🆕🚨 [HIGH — == ON A TYPE WITH NO Equatable IMPL IS ACCEPTED AND SILENTLY ANSWERS false; found 2026-08-26 by the documenta…
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
- [`t0045`](todo/t0045.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY SAFETY, BOTH BACKENDS, from SAFE SPEC-DOCUMENTED SYNTAX; found 2026-08-17 by the for-in idiom sco…
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

- [`t0104`](todo/t0104.md) **MED** — 🆕🐛 [MED — gg check ACCEPTS, C backend then FAILS TO BUILD; PRE-EXISTING, re-confirmed 2026-07-27] &(*b).0 — a TUPLE FIEL…

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

- [`t0680`](todo/t0680.md) **CRITICAL** — 🆕🚨 [CRITICAL — LIVE MISCOMPILE AT CLEAN HEAD, silent wrong value on C, hard llc error on LLVM; found 2026-08-27 by R45 T…
- [`t0682`](todo/t0682.md) **HIGH** — 🆕🚨 [HIGH — A RATIFIED SAFETY RULE IS UNENFORCED AT TWO OF ITS THREE POSITIONS, and the unenforced cases are exactly the…
- [`t0684`](todo/t0684.md) **HIGH** — 🆕🚨 [HIGH — binding Box.get() to a local DOUBLE-FREES; gg check clean; measured 2026-08-27, orchestrator-verified] The tr…
- [`t0697`](todo/t0697.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — LIVE MEMORY-SAFETY DEFECT AT HEAD, BOTH LANES, reachable by changing ONE TOKEN in a committed PASSING fi…
- [`t0698`](todo/t0698.md) **HIGH** — 🆕🚨 [HIGH — READING through an EXPLICIT & BORROW PARAMETER DESTROYS the caller's value; gg check clean; found 2026-08-27…
- [`t0703`](todo/t0703.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — WHICH NAME THE VIEW IS SPELLED THROUGH DECIDES MEMORY SAFETY, and the CoW rescue ITSELF reads the freed…
- [`t0704`](todo/t0704.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — capturing a collection in a CLOSURE and then mutating the source is a USE-AFTER-FREE; rc 139 SIGSEGV on…
- [`t0707`](todo/t0707.md) **HIGH** — 🆕🐛💥 [HIGH — CRASH ON A VALID PROGRAM: shared T x = <a local that is still live> ICEs gg build rc 101 with local _1 read…
- [`t0709`](todo/t0709.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — a Vector[Box[Trait]] built with Box.new + push and RETURNED FROM A HELPER segfaults: rc 139 on BOTH back…
- [`t0715`](todo/t0715.md) **HIGH** — 🆕📉 [HIGH — a MEASURED +13.08% stage-1 string-clone regression, correctness-required but reclaimable; ceilings re-seeded…
- [`t0750`](todo/t0750.md) **HIGH** — 🆕🚨 [HIGH — A CoW COLLECTION ALIAS LOSES ITS VALUE SEMANTICS INSIDE A while LOOP, IN BOTH DIRECTIONS. Silent wrong values…
- [`t0771`](todo/t0771.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — AddressSanitizer: heap-use-after-free under --sanitize, and on a plain build rc 0 with SILENTLY WRONG OU…
- [`t0872`](todo/t0872.md) **HIGH** — 🆕🐛 [HIGH — a 6-byte leak on EVERY inline-constructor receiver, and on a plain struct-ctor field read; isolated 2026-08-3…
- [`t0873`](todo/t0873.md) **HIGH** — 🆕🚨 [HIGH — TWO defects on Vector[Callable], found 2026-08-31 by the R48 Track-B1 brief-review pass 1 while probing the r…
- [`t0907`](todo/t0907.md) **HIGH** — 🆕🚨💥 [HIGH — MEMORY-UNSAFE, gg check rc 0, and BOTH VALUE LANES PRINT THE RIGHT ANSWER; found 2026-08-31 by R48 Track D2…
- [`t0988`](todo/t0988.md) **CRITICAL** — 🆕🚨💥 [CRITICAL — MEMORY UNSAFETY from ordinary safe syntax, gg check CLEAN, both backends.
- [`t1048`](todo/t1048.md) **HIGH** — 🚨 [HIGH — MEMORY, both routes measured; found 2026-09-03 by the R49 Track K brief-review gauntlet, executor-verified at…
### Medium

- [`t0115`](todo/t0115.md) **MED** — 🆕🐛 [MED — COMMENT MISATTRIBUTION, PRE-EXISTING on both lanes; found 2026-08-19 by the R43 Track G output review, executo…

- [`t0116`](todo/t0116.md) **MED** — 🆕⚖️ [MED — OWNER-RULED during the paused CoW-aliasing round (between 2026-07-22 and 07-24; exact date not recovered from…
- [`t0117`](todo/t0117.md) **MED** — 🆕🧹 [MED — OWNER-RULED during the paused CoW-aliasing round (between 2026-07-22 and 07-24; exact date not recovered — do…

- [`t0118`](todo/t0118.md) — 🆕 ROUND-8 FOLLOW-UP (filed 2026-06-28, from the get_or ownership executor ac9980be):

- [`t0119`](todo/t0119.md) — 🆕 ROUND-7 FOLLOW-UPS (filed 2026-06-28, from the Option-combinator scout a96dc0e2 + the Dict.filter executor a3c3d622):

- [`t0120`](todo/t0120.md) — CoW TRACK (lazy borrow-by-default, #2 priority per owner): L1 (position-gated borrow-flip, RUN-proven reachable lower.gg…

- [`t0121`](todo/t0121.md) **MED** — 🆕🐛 [MED — LEAK, both backends, gg check clean and stdout CORRECT; found 2026-08-23 by the R44 Track-G census while closi…
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
- [`t0952`](todo/t0952.md) **MED** — 🆕🐛 [MED — a LEAK *and* an O(n) deep copy per iteration step, from ordinary safe syntax, BOTH backends, gg check clean; f…
- [`t0953`](todo/t0953.md) **MED** — 🆕🐛 [MED — a LEAK from the most ordinary syntax in the language, BOTH backends, gg check clean; found 2026-09-03 by R48 T…
- [`t0967`](todo/t0967.md) **MED** — 🆕⚖️ [MED — A RATIFIED REJECT IS UNIMPLEMENTED, *and* the measurement the ruling rests on did not
- [`t0992`](todo/t0992.md) **MED** — 🆕🧹 [MED — Core #2 / Layering rule 2 violation, NAMED VERBATIM by CLAUDE.md § "No name matching"
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
## Self-host parity

- [`t0155`](todo/t0155.md) **HIGH** — 🆕🐛 [HIGH — SELF-HOST LANE MEMORY-SAFETY, measured 2026-08-26 in the R44 Track K fix-up; this is the ROOT of Track K's "s…

- [`t0156`](todo/t0156.md) **LOW** — 🧹 [LOW — Core #4 residual, NO repro (a wrong spelling fails LOUDLY at link, not silently at rc 0); named by R44 Track K…

- [`t0157`](todo/t0157.md) **MED** — 🆕🐛 [MED — Core #9 LANE DIVERGENCE ON ACCEPTANCE, the SH lane OVER-REJECTS; measured 2026-08-23 by R44 Track B, re-verifi…
- [`t0158`](todo/t0158.md) **HIGH** — 🆕🚨 [HIGH — Core #9 LANE GAP IN A GUARD, not in code; measured 2026-08-22, owner-scoped as R44 Track F] The SELF-HOST TYP…
- [`t0159`](todo/t0159.md) **HIGH** — 🆕🐛 [HIGH — R42's UNPORTED PARITY INFLOW, the exact debt the reseeded ceiling now records; filed 2026-08-22] Three fixtur…
### High
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
- [`t0877`](todo/t0877.md) **MED** — 🆕🐛 [MED — SELF-HOST lane, two arms of one helper; found R48 Track A while porting the t0770 fix to the self-host] A clos…
- [`t0879`](todo/t0879.md) **MED** — 🆕🐛 [MED — SELF-HOST lane; a USER-DEFINED generic method taking a callable fails on EVERY argument spelling; isolated R48…
- [`t0928`](todo/t0928.md) **MED** — 🆕🐛 [MED — SELF-HOST DIAGNOSTIC QUALITY, a LOSSLESSNESS gap at the parser→AST boundary (Layering rule 1). Filed by R48 Tr…
- [`t0929`](todo/t0929.md) **MED** — 🆕🐛 [MED — Core #10 lower-or-reject + Core #9 accept/reject LANE DIVERGENCE, on the self-host. Filed by R48 Track β, whos…
- [`t0932`](todo/t0932.md) **MED** — 🆕 [MEDIUM — self-host lane, ASan-BLIND; found 2026-08-31 by R48 Track γ while building the index/slice derivation-join n…
- [`t0969`](todo/t0969.md) **MED** — 🆕🐛 [MED — self-host lane SEGVs (rc 139) where Rust gg is correct on BOTH backends; found 2026-09-03 by R49 Track A1-M's…
- [`t0970`](todo/t0970.md) **MED** — 🆕🐛 [MED — the self-host REJECTS a program Rust gg compiles and runs, and the diagnostic names a type the expression cann…
- [`t1055`](todo/t1055.md) **MED** — 🆕🧹 [MED — LANE DEBT, NOT A SOUNDNESS LAG. ⚠ THE SELF-HOST IS *CORRECT* ON THE MISCOMPILE THIS DESCRIBES; do not read it…
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
- [`t0389`](todo/t0389.md) **HIGH** — 🆕🚨 [HIGH — Core #8 / succession; measured 2026-08-05 by the callable scout] Callable[void(String !)] cb = eat; cb(!g) PA…
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
### Medium


- [`t0403`](todo/t0403.md) **HIGH** — 🆕🐛💥 [HIGH — MEMORY-UNSAFE, gg check ACCEPTS; found 2026-07-26 by sigil-prose gauntlet pass 10, parent-reproduced] Assign…

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
- [`t0943`](todo/t0943.md) **MED** — 🆕🐛 [MED — Core #8 REFERENCE LAGS THE SELF-HOST: the same place, two spellings, two verdicts] Rust gg accepts v.push(t._0…
- [`t0945`](todo/t0945.md) **MED** — 🆕🐛 [MED — Core #8 BOTH LANES AGREE ON THE WRONG ANSWER, over-rejection with a FALSE diagnostic] c[i] on a user generic i…
- [`t0950`](todo/t0950.md) **MED** — 🆕🧹 [MED — a Core #10 guard that rejects LOUDLY but in the WRONG PHASE: an ICE where the invariant asks for a check-time…
- [`t0957`](todo/t0957.md) **MED** — 🆕🐛 [MED — an ACCEPT/REJECT split driven by the SPELLING of the index, not by anything semantic; found 2026-09-03 in pass…
- [`t1020`](todo/t1020.md) **MED** — 🆕🐛 [MED — Core #9 lane lag: the self-host cannot compile ANY method call on a Box[UserStruct];
- [`t1021`](todo/t1021.md) **HIGH** — 🆕🚨 [HIGH — SILENT WRONG OUTPUT on the self-host lane, gg check clean; found 2026-09-03 by R49
- [`t1023`](todo/t1023.md) **MED** — 🆕🐛 [MED — Core #10 lower-or-reject, gg check CLEAN on a method no bound provides; both backends;
- [`t0990`](todo/t0990.md) **MED** — 🆕🐛 [MED — DESIGN SMELL: a type whose whole reason to exist is its two ends has no way to name them.
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
- [`t0729`](todo/t0729.md) **HIGH** — [HIGH — UNDEFINED BEHAVIOUR IN EMITTED CODE: the LLVM backend copies overlapping stack slots with memcpy. Found 2026-08-…
- [`t0933`](todo/t0933.md) **HIGH** — 🆕🚨 [HIGH — an ICE on a program gg check ACCEPTS; found 2026-08-31 by R48 Track γ while probing the Iterator trait-arg sh…
- [`t0987`](todo/t0987.md) **HIGH** — 🆕🐛 [HIGH — Core #10 lower-or-reject violated by a _ => FALL-THROUGH, which Core #10 names as a
- [`t0977`](todo/t0977.md) **HIGH** — 🆕🐛 [HIGH — SILENT WRONG OUTPUT on ordinary safe syntax, gg check clean; found 2026-09-03 by R49 Track E while re-writing…
- [`t1049`](todo/t1049.md) **HIGH** — 🚨 [HIGH — for i, c in s.enumerate() TRAPS on any multibyte String, and is ACCIDENTALLY CORRECT on every ASCII one; found…
- [`t1050`](todo/t1050.md) **HIGH** — 🚨 [HIGH — SILENT WRONG OUTPUT at exit 0, no diagnostic; found 2026-09-03 by the R49 Track K brief-review gauntlet, execu…
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
- [`t0774`](todo/t0774.md) **MED** — 🆕🧹 [MEDIUM — a live Layering-rule-2 smell and the natural migration point for the indirect-dispatch chokepoint; raised b…
- [`t0727`](todo/t0727.md) **MED** — [MED — --sanitize --backend=llvm instruments the RUNTIME ONLY; generated user code carries no shadow checks. Found 2026-…
- [`t0728`](todo/t0728.md) **MED** — [MED — A NAME MATCH OUTRANKS THE USER'S TYPED DECLARATION: an extern bound to malloc/calloc/realloc has its declared ret…
- [`t0742`](todo/t0742.md) **MED** — 🆕🐛 [MED — BOTH LANES AGREE ON THE WRONG ANSWER, which is a red flag and not a pass (Core #8); found R47 Track D1 while m…
- [`t0743`](todo/t0743.md) **MED** — 🆕🐛 [MED — BOTH LANES AGREE ON THE WRONG ANSWER (Core #8 red flag); found R47 Track D1 while building the ACCEPT control…
- [`t0876`](todo/t0876.md) **MED** — 🆕🧹 [MED — THE EMITTED C IS NOT REPRODUCIBLE: the SAME compiler binary on the SAME input emits two different files run to…
- [`t0902`](todo/t0902.md) **MED** — 🆕🐛 [MED — C BACKEND ONLY, LLVM IS CORRECT (Core #9 lane divergence); gg check rc 0, gg build fails at cc; found 2026-08-…
- [`t0954`](todo/t0954.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, BOTH backends, gg check clean; found 2026-09-03 by R48 Track T-a1 while ASan…
- [`t0955`](todo/t0955.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, BOTH backends, gg check clean; found 2026-09-03 by R48 Track T-a1 while ASan…
- [`t0968`](todo/t0968.md) **MED** — 🆕🐛 [MED — COMPILER ICE (panic, not a diagnostic) on ordinary safe syntax; both backends; found 2026-09-03 by R49 Track A…
- [`t0971`](todo/t0971.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, both backends, gg check clean and the printed value CORRECT; found 2026-09-0…
- [`t0972`](todo/t0972.md) **MED** — 🆕🐛 [MED — a LEAK from ordinary safe syntax, both backends, gg check clean and the printed value CORRECT; found 2026-09-0…
- [`t0991`](todo/t0991.md) **MED** — 🆕🐛 [MED — C-BACKEND-ONLY BUILD FAILURE on ordinary code, gg check CLEAN, and a LANE DIVERGENCE:
- [`t1052`](todo/t1052.md) **MED** — 🆕🐛 [MED — a compile-time failure on ordinary safe syntax, BOTH backends, gg check clean; found 2026-09-03 by the R49 Tra…
- [`t1054`](todo/t1054.md) **MED** — 🆕🧹 [MED — a live Layering-rule-2 site with ~50 arms, IDENTICAL IN SHAPE to the R49 A1-IDENTITY miscompile, and the sourc…
### Low

- [`t0528`](todo/t0528.md) — 🔭 (low-pri, const-bytes #10) — scalar-FLOAT static const-init uses %g (float_to_str), not bit-exact. scalar_static_c_lit…

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
### Low

- [`t0606`](todo/t0606.md) — 🧹 (G1 follow-up) lint-file-scope: widen g1_projected_materialize_sites_untrack files[] IF a projected-materialize cow_be…
- [`t0731`](todo/t0731.md) **LOW** — [LOW — the sanitize sweep covers ONE lane and ONE directory level; extending it to the LLVM lane is now possible for the…
- [`t0965`](todo/t0965.md) **LOW** — 🆕 [LOW — ADOPTION GAP, not a defect; identified 2026-09-03 by R48 Track T-a2 while closing its
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