# ENFORCEMENT-WAVE CENSUS SCOUT — FULL REPORT
> Run 2026-07-11, read-only, per `docs/plans/define-gorget/wave-census-scout-mandate.md`.
> Worktree: `/workspace/gorget/.claude/worktrees/agent-afda6af73663f530e` (base `c437f65e`).
> EVERY count regenerated this session; zero dated figures quoted as truth. Where the A33 scout
> (`docs/plans/define-gorget/scouts/scout-a33-fault-model.md`) gave a number, it was re-measured.

## 0. Corpus definitions (as measured this session)

| Corpus | Path | .gg files |
|---|---|---|
| fixtures | `tests/fixtures/` excl. `self_host_*` (0 symlinks) | 1,680 |
| spectests | `spectests/` | 195 |
| self-host | `tests/fixtures/self_host_*` (-type f; 30 symlinks deduped) | 65 |
| gorget-js | `/workspace/gorget/.worktrees/gorget-js` (READ-ONLY) | 18 |
| gorget-arena | **`/workspace/gorget/.worktrees/gorget-arena`** (commit 6abfa15) | 67 |
| lib (stdlib) | `lib/` — **NOT in the mandate's corpus list; added (pow() lives here; real blast radius found)** | 60 |

**⚠ Corpus surprises (REPORTS, not downgrades):**
1. **The mandate's arena path is STALE**: `/workspace/gorget-1/target/gorget-arena` is the same repo
   but its checkout now contains ZERO .gg files. The canonical live copy is
   `/workspace/gorget/.worktrees/gorget-arena` (67 files) — used for this census.
2. **`lib/` is a missing fifth corpus**: 60 .gg files carrying 224 move-sigil sites, 1 `.slice()`,
   the `pow()` definition, and 3 D10(b) rejection sites. Every count below includes a lib column.
3. **Two additional real-Gorget projects exist** at `/workspace/gorget/.worktrees/`: **gglox**
   (6 files, a Lox VM — plain dir inside the main checkout, not its own repo) and
   **gorget-conformance** (own repo, commit 0eb3d30; 87 files). Affected ONLY by D27
   (54 + 12 move sites respectively). Zero slice/pow/fault-catch/custom-Drop/&-bind hits.

---

## 1. THE BLAST-RADIUS MATRIX (real code sites; comment/string-literal hits excluded)

| Change | fixtures | spectests | self-host | gorget-js | arena | lib | TOTAL |
|---|---|---|---|---|---|---|---|
| **D12** drop-purity newly-rejects (est.) | ≤10 sites in the 26 drop-defining files | ~0–3 in 6 files | **0** | **0** | **0** | 0 (VectorDrain def only) | **≤13, all inside the drop-fixture family** |
| **D10(a)** local `&`-binds | **2** (`cow_amp_bind_ref{,_field}.gg` → flip to negatives) | 0 | 0 | 0 | 0 | 0 | **2** |
| **D10(b)** same-call place-overlap | 0 new (2 existing `*_error` negatives) | 0 | **8** | **24** | **1** | **3** | **36** |
| **D15** `T[]` removal | 0 | 0 | 0 (6 grep hits = comments) | 0 | 0 | 0 | **0** |
| **D22** `.slice()` → `v[a:b]` | 6 (5 files) | 0 | **201 (11 files)** | 0 (comments/JS-data only) | 0 | 1 (`lib/std/io.gg:432`) | **208 — 97% self-host** |
| **D19** `break <value>` | 0 | 0 | 0 (+1 formatter ARM — see §1.6) | 0 | 0 | 0 | **0** |
| **D25** fault-catch removal | **31 files** (50 `catch Fault.X` + 10 `catch f` + 1 negative) + 33 `fn fault_*` in integration.rs | 0 | 0 uses (machinery only) | 0 | 0 | 0 | **31 fixture files + both-compiler machinery** |
| **D26** fallible operators (additive) | 0 | 0 | 0 | 0 | 0 | 0 | **0 migration** (hazards re-measured clean) |
| **D28** `pow()` retirement | 1 (`math_stdlib.gg:12`) | 0 | 0 | **6 (2 files)** | 0 | 1 (the def, `lib/std/math.gg:26`) | **8** |
| **D28** xor-as-pow lint exposure | 2 REAL xor uses (`5 ^ 3`) | 0 | 0 | 0 | 0 | 0 | **2 — lint-shape flag, §1.9** |
| **D27** `!`→`^` sigil sites | 310 (130 files) | 70 (24 files) | ~278 (27 files) | ~34 (4 files) | 132 (24 files) | **224 (20 files)** | **~1,048** (+66 in gglox/gorget-conformance ⇒ **~1,114**) |

### 1.1 D12 — drop-purity (method + confidence)
Method: grep `drop(!self)` definitions per corpus, then a shape-scan (bare-assign / ctor-ident-arg /
push/put/insert/send-of-ident) INSIDE the defining files, since fixture programs are single-file and
no other corpus defines a custom-Drop type. Results:
- Custom-Drop definitions: fixtures **26 files / 30 occurrences** (D4's dated "22" grew — 4 of the
  new ones are `fault_*_drop.gg`, which D25 deletes/migrates); spectests **6 files**
  (`spectests/run/drop_*.gg`, `owning_param_drop_at_exit.gg`); self-host **0 real** (2 grep hits are
  comments — `lir_lower.gg:5728`, `traits.gg:446`; D4's dated "2 self-host uses" measures **0** today);
  gorget-js **0**; arena **0**; lib **1** (`VectorDrain[T]`, `lib/std/iter.gg:370`).
- Newly-rejecting sites: heuristic scan yields 16 candidates; most are false (int ctor args) or
  move-at-last-use legal. All corpus `.drain()` uses are `for x in v.drain():` consumed-temp shapes
  (move-legal, so lib's VectorDrain taints nothing downstream: re-verified `dict_drain_basic.gg:18`
  etc., and the one self-host `.drain` hit is a comment).
- **Confidence: HIGH that self-host/gorget-js/arena/lib have ZERO newly-rejecting sites** (no
  tainted type is reachable there in an implicit-copy position). **MEDIUM on the exact fixture
  count** (≤10) — liveness decides move-vs-clone, and only the compiler has it; the D12 executor's
  first build IS the exact measurement. The drop fixtures exist to pin drop semantics; the ones that
  newly reject are the natural D12 negative-fixture set (per D4: "drop-count determinism spectests
  become writable"). ggdef already models the rule (`spec/ggdef/src/elaborate/mod.rs`
  `reject_if_tainted_live_place`, ~:567-576).

### 1.2 D10(a) — local `&`-binds
Method: grep decl-shaped `= &ident` (`^\s*(auto|T) name = &x`) + `T &name = expr` across all corpora.
- **Exactly 2 sites in the world**: `tests/fixtures/cow_amp_bind_ref.gg:6` (`auto r = &b`) and
  `tests/fixtures/cow_amp_bind_ref_field.gg:13` (`auto r = &b.data`) — precisely the two fixtures
  TODO.md:268 already plans to flip to negatives. `T &a = b` form: zero everywhere. Confidence: HIGH.

### 1.3 D10(b) — same-call place-overlap (the census's biggest surprise)
Method: python scan of single-paren-level call arg lists for two access paths where one is a
PREFIX of the other (root + projection prefix, per the D10 ruling decisions.md:439-451) and ≥1
carries `&`/`!`. Nested-paren shapes uncounted → mild undercount. Confidence: MEDIUM-HIGH on the
found sites (each hand-checked), MEDIUM on completeness.
- fixtures: only the two existing negative fixtures (`borrow_conflict_error.gg:6`,
  `double_mutable_borrow_error.gg:7` — name-exact `f(&v,&v)`, already rejected today). 0 migration.
- **self-host: 8 REAL sites** (bootstrap-gated migration — hoist the read into a local):
  `self_host_lowerer/lower_expr.gg:5002` (`add_local(&ctx, ctx.expected_type, None)`),
  `self_host_lowerer/lower_types.gg:561,568,595` (`resolved_to_gir_type(…, gmod.tc_types,
  gmod.tc_scopes, &gmod)`), `self_host_typechecker/infer.gg:276,303,382` +
  `self_host_typechecker/typecheck.gg:1119` (`resolve_method_full(types.trait_registry, …, &types)`).
  (2 further grep hits are pthread C-code inside string literals — excluded.)
- **gorget-js: 24 REAL sites** — the `f(&r, r.function_prototype)` family:
  `env.gg:253,332,416,428,461,479,493,506,540,639,905`;
  `eval.gg:30,33,36,39,45,2085,2101,2106,2110,4901,4905,4909,4913`.
- arena: 1 (`src/game/game_state.gg:168` — `weapon_fire_update(&gs, gs.game_time_ms, new_ammo)`).
- lib: 3 (`lib/xtd/p2p.gg:2057` — a DOUBLE-writer `(&node, &node.disc_socket)` — `:2067`,
  `lib/xtd/ssh.gg:633` `channel_write_data(&self, self.channel_id, data)`).
- Disjoint-field pairs (e.g. `InvalidSyntax(p.pos, !p.err)` in lib/xtd/{json,toml,xml,yaml}) do NOT
  overlap under the prefix rule and were excluded (they'd be false rejections — the check must be
  prefix-keyed, exactly as the ruling says).
- **REPORT: gorget-js takes its single biggest hit from D10(b) (24 sites), larger than its D27
  share (~34 mechanical sigil sites but those are fmt-automated; the D10 hoists are hand-edits).**
  Not a ruling contradiction (D10 premised no count), but the first real measurement.

### 1.4 D15 — `T[]` removal
Zero live uses in every corpus. All 6 grep hits are comments (`keep[]`, `order[]`, `states[]`
prose + a `__fa_X[]` C-string in a comment). **D15's "C-interop residue, nothing live" premise
CONFIRMED.** The removal is machinery-only (grammar/typecheck/lowering + docs). Confidence: HIGH.

### 1.5 D22 — `.slice()` migration
- fixtures: 6 sites/5 files (`bench_string_methods.gg:52`, `coroutine_vector_ops.gg:28`,
  `leak_reassign.gg:65`, `test_vector_all.gg:108,113`, `vector_methods2.gg:30`).
- **self-host: 201 real sites in 11 files** (`lir_lower.gg` 61, `lir_codegen.gg` 59,
  `lower_types.gg` 41, `lower_expr.gg` 15, `driver.gg` 6, `loader.gg` 6, `lower_loops.gg` 6, + 4
  more ≤3 each). Receivers are dominantly String (`arg.slice(14, arg.len())` shapes).
  **97% of the D22 migration is self-host source ⇒ the slice-surface track is bootstrap-HEAVY,
  not the "small rider" its TODO placement suggests. REPORT.**
- gorget-js 0 real (2 comments + JS-source-in-string-literal data); arena 0; spectests 0;
  lib 1 (`lib/std/io.gg:432`, `Vector[byte]` receiver).
- `.byte_slice(`: 23 self-host uses — a DIFFERENT method, not in D22's scope; the D22 docs
  write-through should state whether bytes keep `.byte_slice` or gain `[a:b]` later (flag).
- Zero user-defined `slice` methods anywhere ⇒ a name-keyed fmt rewrite is collision-free.

### 1.6 D19 — `break <value>` removal
Zero live uses (every grep hit is a comment or string; fixtures/self-host/js/lib re-verified
line-by-line). **Premise confirmed.** One machinery correction: TODO.md:707 says "self-host:
already has no SBreak arm" — that's true of the LOWERER, but the self-host PARSER/RESOLVER
formatter carries a live break-VALUE arm: `self_host_parser/format.gg:471` +
`self_host_resolver/format.gg:471` (`case SBreak(val): … "break " + format_expr(v)`), which means
the self-host AST/parser accepts the form too. The removal track must delete the self-host
parse/format arms as well — same class as the Rust typecheck.rs arms the entry already lists.

### 1.7 D25 — fault-catch removal
- **31 of 33 `fault_*.gg` fixtures use fault-catch** (50 `catch Fault.X` + 10 binding-form
  `catch f` + 1 `catch Bogus.Overflow` negative); 2 panic-default fixtures survive
  (`fault_panic_default.gg`, `fault_bounds_panic_default.gg` — catch appears only in a comment).
  **A33's "31" re-verified EXACTLY.**
- self-host: 0 uses (17 hits, all comments — re-verified 0 non-comment). gorget-js/arena/
  spectests: 0. The 8 `trap_*` spectests are untouched by removal.
- integration.rs: 33 `fn fault_*` test fns (~340 lines).
- Machinery floor re-measured this session: `src/ir/lowering/fault_participation.rs` = 237 lines
  exact; the 3 Faultable GIR variants + `Inst::FaultCheck` (`src/lir/mod.rs:857`) +
  `lower_fault_catch_expr` (`src/ir/lowering/exprs/mod.rs:3739`, dispatch :1579) all present;
  Rust `[Ff]ault` mention-lines (excl. `default`) = **773**; self-host = **599** (raw, symlink
  dirs included). A33's ~2,000-physical-line both-compiler estimate stands or grows.

### 1.8 D26 — fallible operators (additive)
Zero migration sites by construction. Hazards re-measured this session: `+ !x`/`+!x` adjacency =
**0 code occurrences** in all corpora (1 grep hit = a gorget-js test-NAME string); existing `**` =
**0 code occurrences** (21 self-host hits are all `void**`/`char** argv` inside emitted-C string
literals — untouched by lexing). A33 §6.3's "hazard measured empty" re-confirmed.

### 1.9 D28 — `**` + pow() retirement + xor-as-pow lint
- `pow(` call sites: fixtures 1 (`math_stdlib.gg:12`, float args), gorget-js 6 real calls
  (`abstract_ops.gg:201,203,205,207`, `eval.gg:3771,3826` — all FLOAT pow; `**` float form is
  IEEE-identical, safe rewrite), lib 1 = the definition (`lib/std/math.gg:26`). Self-host/arena/
  spectests 0. Zero user-defined `pow` elsewhere ⇒ name-keyed rewrite collision-free.
- **Lint-shape flag (REPORT): the ruling's `literal ^ literal` shape false-fires on the two
  canonical xor fixtures** (`bitwise_ops.gg:5` `5 ^ 3`, `test_bitwise_ops.gg:11`) — real xor tests.
  GCC-12's `-Wxor-used-as-pow` fires only when the LHS literal is 2 or 10 (decimal); adopting that
  exact restriction keeps both fixtures clean with zero suppression machinery. Recommend pinning
  this in the D28 track brief.

### 1.10 D27 — the sigil migration (the ruling's ~870 is an undercount — REPORT)
Method: grep `![A-Za-z_]` (excludes `!=`), comment-lines excluded, string-literal hits identified
by class-regex + sampling. Per corpus (code sites / files):
- fixtures **310 / 130** (call 192, `(!self` 42, param 85, assign 28, return 1)
- spectests **70 / 24** — the raw grep says 468 because every spectest carries `#!spectest`/`#!end`
  frontmatter markers (390 comment hits). NOTE: sed would corrupt these; `gg fmt` won't. Spectests
  were absent from the A33 table.
- self-host **~278 / 27** (call 221, param 45, assign 12; the raw 365 includes C-in-string false
  positives like `"if (!p) return;"`)
- gorget-js **~34 / 4** (eval_test hits are JS `!true` string data)
- arena **132 / 24** (call 95, param 30, assign 1)
- **lib 224 / 20** (call 180, param 19, assign 17, return 8, `!self` 9) — **absent from the A33
  census and from the ~870 in the D27 ruling/TODO entry**
- gglox **54 / 3**, gorget-conformance **12 / 6** — also absent.
**Corrected total ≈ 1,048 core (+66 extras) ≈ 1,114 — vs the ruling's ~870.** Direction unchanged
(mechanical, fmt-automated); the ruling text and TODO.md:244 item (3) should be corrected.
Machinery surface re-verified: `Token::Bang` 10 occurrences across `src/parser/` (+ lexer defs in
`src/lexer/token.rs`/`mod.rs`); `E_MoveWithoutOperator` strings in 5 `src/semantic/` files; the
`move`-keyword diagnostic arm at `src/parser/expr.rs:586-595` ("use `!` for move expressions…" —
becomes "use `^`…"); `Token::Caret` sits in the never-sigil closure-lookahead list
(`src/parser/expr.rs:~1459`) and must leave it, exactly as TODO.md:244 item (1) says; self-host
lexer/parser twin sites (6 `Bang` hits in `self_host_lexer/lexer.gg` + `self_host_typechecker/parser.gg`).

---

## 2. OVERLAP ANALYSIS (files touched by ≥2 changes — 44 total)

| Combo | Files | Consequence for batching |
|---|---|---|
| D12+D27 | 28 (all drop_* fixtures + spectest twins) | D12's expectation churn and D27's resigil hit the same files → do D12 (Batch A) BEFORE the fmt sweep (Batch C3) so the sweep runs once over settled fixtures |
| D25+D12+D27 | 4 (`fault_catch_drop.gg`, `fault_deep_catch_{bounds,divzero,}_drop.gg`) | **These die/migrate under D25 — neither D12 nor the D27 sweep should touch them first. D25's fixture disposition must precede both.** |
| D22+D27 | 5 (`lib/std/io.gg`, sh `lir_codegen/lir_lower/lower/lower_closures.gg`) | natural ONE-fmt-pass companions |
| D10+D22+D27 | 2 (sh `lower_expr.gg`, `lower_types.gg`) | the self-host hot files: D10 hoists (hand) + slice/sigil (fmt) — one bootstrap round |
| D10+D27 | 4 (arena `game_state.gg`, `lib/xtd/p2p.gg`, `lib/xtd/ssh.gg`, sh `typecheck.gg`) | same |
| D10+D27+D28 | 1 (gorget-js `eval.gg`) | gorget-js needs exactly one coordinated migration round |

Headline: **the syntax changes (D27/D22/D28-retire) overlap heavily with each other and with
D10(b)'s hand-hoists in the SAME self-host/gorget-js/lib files** — which is exactly why the
per-corpus one-pass batching below puts all hand-edits before the single fmt sweep.

---

## 3. SEQUENCING PLAN (draft for owner ratification)

**Composition test (mandate question): D27 sigils + D22 `.slice()` + D28 `pow()` DO compose in
one fmt-driven pass — verified no ordering hazards:** disjoint token spaces (`^`-prefix vs
`[a:b]` vs `**`); zero `+ !`/`+!`/`**` code-adjacency (re-measured §1.8); both rewrites that are
name-keyed (`.slice`, `pow`) are collision-free (zero user definitions, §1.5/§1.9); spectest
frontmatter survives because fmt preserves comments (sed would not — fmt is mandatory, §1.10).
Precondition: the NEW grammar (prefix `^`, colon-slice, `**`) must already parse in BOTH compilers
before the sweep — the standard accept-both → migrate → retire-old bootstrap three-step.

### Batch A — zero/near-zero-blast rejections (D19 + D12 + D10a) — partial bootstrap gate
- **D19**: grammar+typecheck arm deletion (Rust `typecheck.rs:7338/:7526/:7760` family per
  TODO:707) + the self-host parser/format SBreak(Some) arms (§1.6 correction) + negative fixture.
  0 corpus sites. Bootstrap-gated only via the self-host formatter/parser arm deletion.
- **D12**: `E_MoveWithoutOperator` for tainted types at the six positions, both compilers,
  matching ggdef; negative fixtures per position. Corpus churn confined to the 26+6 drop files
  (≤13 sites; the executor's first build is the exact measurement — surprises are REPORTS).
  SKIP the 4 `fault_*_drop.gg` (Batch C2 owns their fate). Bootstrap-gated (self-host check).
- **D10(a)**: reject both `&`-bind forms; flip the 2 fixtures to negatives; retire the round-38
  T-D self-host intercept. Bootstrap-gated.
- Discipline pin: any site the compilers newly reject beyond the counts above is a REPORT to the
  owner in the batch close, never a silent expectation edit.

### Batch B — D10(b) place-overlap + the 36-site hand-migration — bootstrap-gated; the out-of-repo batch
- Land the all-sigil-pairs prefix-overlap check + negative fixtures (both compilers).
- One migration pass per corpus, all hand-hoists: self-host 8 → bootstrap; lib 3; arena 1;
  gorget-js 24 (one coordinated round in that repo — its biggest hit of the whole wave, §1.3).
- Order before Batch C3 so the fmt sweep doesn't have to re-run over these files after hand edits
  (they overlap: §2).
- Discipline pin: nested-paren overlap shapes my scan missed will surface as extra rejections —
  REPORT counts per corpus at batch close; do not downgrade the check to warn.

### Batch C — the operator/spelling batch (C1 → C2 → C3) — the big bootstrap-gated round
- **C1 (additive; gates D25):** `+! -! *! /! %!` (+ lean `<<! >>!`), `**`/`**=`/`**!`,
  prelude `ArithError`, the xor-as-pow lint **pinned to the GCC-12 2/10-literal-base shape**
  (§1.9 — the ruling's bare `literal ^ literal` false-fires on the 2 xor fixtures), ggdef model +
  spectests (MIN_FIXTURES shifts). Zero corpus migration. D26+D28 as one combined operator round
  (TODO:243's own suggestion).
- **C2 (D25 removal; after C1 per the D13 with-or-before pattern):** migrate the 31 fault
  fixtures (~10 → D26 positives per A33 §5.6, 2-3 → negatives asserting `catch Fault.X` rejects
  with a fix-it, rest delete) + delete the both-compiler machinery (§1.7 inventory) + the 33
  integration test fns + docs write-through (incl. the already-stale §10.5 "local and lexical").
  Bootstrap-gated (self-host lowerer machinery). Fixture disposition FIRST so C3 never touches
  doomed files (§2 overlap).
- **C3 (the composed fmt sweep):** accept-both grammar lands (both compilers) → ONE
  `gg fmt`-driven pass per corpus rewriting `!x`→`^x` (~1,114 sites), `.slice(a,b)`→`[a:b]`
  (208), `pow(a,b)`→`a ** b` (7 calls) → retire old forms (prefix `!` gone from the grammar,
  `.slice` removed with fix-it, `pow()` deleted from lib/std/math.gg) + transition fix-its
  ("prefix `!` is no longer the move sigil; write `^x`") + diagnostics/docs sweep (§1.10
  machinery list; E_MoveWithoutOperator text, expr.rs:586-595 arm, CLAUDE.md quick-ref, README,
  reference, book, devbook). Corpus order within C3: in-repo (fixtures+spectests+self-host+lib)
  in one bootstrap round, then gorget-js / arena / gglox / gorget-conformance each in one pass
  against the updated compiler.
- Discipline pin: any file fmt cannot round-trip (parse error, formatting loss) is a REPORT;
  no hand-patching source to dodge a formatter gap (that's a formatter bug to fix).

**Bootstrap-gated batches: A (partial), B, C1, C2, C3.** Effectively the whole wave — every batch
touches self-host source or its compilers; the gate is `self_host_bootstrap_fixed_point` +
both-backend sweeps per batch, as usual.

**What stays OUT of these batches** (already-sequenced separately in HANDOVER/TODO): D13/D14/D17
implementation tracks; D24 boundary spec work (spec-only, rides ratification); A31/A32 design scouts.

---

## 4. RATIFICATION PACKET (D24 / D25 / D26 — one owner review closes the batch)

**Q1 — D24, the supervised boundary.** RECOMMEND: Task join = the ONLY v1 fault→value conversion
point (no `supervise:` block); fault value = prelude `TaskFault` carrying the closed `TrapCode`
(ALL 8 classes; code = the only normative surface; detail/location impl-observed); conversion =
ordinary `throws`-typed join, so `catch (e):`/propagation/D23 totality apply with zero new
control flow; permanence set incl. whole-unit discard, no drops on the trap path, unobserved
supervised trap re-panics at drop-join. Scheduling semantics stay phase-3. *Preview:* spec prose +
ggdef hook note now; implementation deferred to the async/task phase. *Would change it:* a
phase-3 scheduler that can't enumerate per-task resources; a demonstrated sub-task containment
need (census: none). [A33 §3, §8-D24 — unchanged by this census.]

**Q2 — D25, lexical fault-catch disposition.** RECOMMEND: REMOVE (Swift model), gated on D26
with-or-before. Census re-verified this session: 31/33 fixture files are tests-of-the-feature,
**zero organic uses in ~166k lines across six corpora** (self-host hits all comments; js/arena/
spectests zero); machinery floor re-confirmed (fault_participation.rs 237 lines exact; 773 Rust +
599 self-host fault mention-lines); ggdef already models NO catch (removal completes the
definition for free); every catchable class has an equal-or-better value-level twin
(`.get()`/`??`, D26 family); the §10.5 "local and lexical" doc text is stale either way — a ruling
fixes docs or impl, and removal fixes both. 8 pending machinery tracks retire (§5). *Preview:*
Batch C2 above (~10 fixtures → D26 positives, 2-3 negatives, machinery deletion, docs sweep).
*Would change it:* a genuine pre-check-to-dodge-faults site (still none — the A33 (b)/(c) census
re-verified empty); an owner domain where task-wrapping is unacceptable AND per-op forms too
fine; D26 rejection.

**Q3 — D26, the fallible-operator family.** RECOMMEND: ADOPT `+! -! *! /! %!` + lean `<<! >>!`
(+ `**!` per ratified D28), throwing payload-free prelude `enum ArithError: Overflow | DivByZero`
into the ONE channel; D23-total (`int` in every position, auto-propagates, catchable via existing
`catch (e):`); integer-only with a float fix-it; compound `+!=` EXCLUDED v1; `+|` saturating
name-reserved. Glyph is already pinned `+!` by ratified D27. Census re-verified: zero `+!`/`**`
code-token collisions, zero `+ !x` adjacency anywhere (§1.8) — lexing is clean in both lexers.
*Preview:* Batch C1; the D26+D28 combined operator round. *Would change it:* nothing found this
census; the remaining open detail is catch-in-const timing (safe to reject-v1 with a clear
diagnostic per A33 §5.8).

---

## 5. TODO/DONE HYGIENE NOTES

**This census ABSORBS/COMPLETES:**
- TODO.md:242 — the wave-census scout entry (this report). Move to DONE with the report path.
- TODO.md:260 step (1) — the D12 blast-radius scout ("scout first — measure the blast radius").
  DONE by §1.1; the D12 entry's remaining steps (2)-(4) are Batch A work.
- TODO.md:262 step (a)(1) — the D15 live-use scan ("confirm nothing live"). DONE: zero live (§1.4).
- TODO.md:707's "re-verify first that no fixture/self-host code uses `break <expr>`" — DONE (§1.6),
  WITH the correction: add the self-host parser/format.gg SBreak(Some)-arm deletion to the entry.

**Cancelled under D25 (once ratified) — the 8 fault-machinery tracks (A33 §2.5, all re-located
this session in TODO.md):**
1. Inc-C method/equip cross-frame fault participation (ROUND-18 FAULT-PROPAGATION FOLLOW-UPS block).
2. `lower_equip_block` param-loop dedup — the fault-slot MOTIVATION dies; the sibling-drift smell
   itself is generic → REWORD to a plain self-host-elegance cleanup, don't delete outright.
3. Error-model Inc-2.1 "PENDING frontier: PHASE 2 (deep/boundary catch …)" — the deep-catch half
   is REJECTED-on-the-merits (ledger LOG 2026-07-11); the boundary half is SUPERSEDED by D24.
   Rewrite the entry to point at D24; `Fault equip Error` dies with the Fault enum; OOM moves to
   the D24 boundary reservation.
4. 2.3b fn-value/indirect fault propagation (same block) — machinery deleted.
5. Both-compiler uncaught-fault RE-PANIC normalization ("[MEDIUM — both-compiler uncaught-fault
   RE-PANIC normalization…]") — the re-panic sites ARE fault-catch scope machinery
   (exprs/mod.rs:3786-3802, functions.rs fill_fault_return_block, self-host twins); they are
   deleted wholesale by C2. Cancel; note the direct trap sites already landed via T2a/T2b.
6. Dead-fault-catch lint ("🧹 [error-model review 2026-07-07, LOW-M]") — no catch left to be dead.
7. Self-host fault-catch own-track ("Self-host fault-catch (its own local lowering) is also still
   its own track") — cancelled.
8. Self-host deep-parity residuals tied to fault-catch (same Inc-2.1 block) — cancelled.
**Keep-and-verify note:** cancellations are gated on the D25 ratification this packet requests —
file them as "CANCELLED-BY-D25 pending ratification" until the owner signs.

**Corrections to write back on ratification:**
- decisions.md D27 LOG + TODO.md:244 item (3): ~870 → ~1,114 (+ lib 224 / spectests 70 /
  gglox 54 / gorget-conformance 12; per-corpus table §1.10).
- The D22 slice-surface track (TODO.md:262): add the census (208 sites, 97% self-host,
  bootstrap-heavy); the "Medium track" sizing under-calls the self-host share.
- D28 track (TODO.md:243 item 6): pin the xor-as-pow lint to the GCC-12 2/10-base shape (§1.9).
- The wave-census mandate's arena path (`target/gorget-arena/`) → `.worktrees/gorget-arena`.
- NEW TODO candidates surfaced: (i) gorget-js D10(b) migration round (24 sites — coordinate with
  that project's agent); (ii) gglox + gorget-conformance D27 migration passes (66 sites); (iii)
  D22 bytes question (`.byte_slice` stays or gains `[a:b]` — one-line ruling rider).

## 6. Corrections to prior premises (Core #5 discipline)
1. D27 ruling's "~870 sites" → **~1,114** (lib + spectests + 2 extra projects omitted). Direction
   unchanged; magnitude +28%.
2. A33 "31 fixture files use fault-catch" → CONFIRMED exact.
3. D4/D12's dated "custom Drop: 22 fixtures … 2 self-host uses" → **26 fixtures, 0 self-host**
   (comments only), +6 spectests, +1 lib (VectorDrain).
4. TODO:707 "self-host has no SBreak arm" → the LOWERER has none; the PARSER/RESOLVER
   format.gg:471 DO carry a break-value arm.
5. Mandate's `target/gorget-arena/` path → stale; canonical copy at `.worktrees/gorget-arena`.
6. D28's xor-as-pow lint shape as ruled would false-fire on the two canonical xor fixtures;
   GCC-12's 2/10-base restriction fixes it for free.
7. First-ever D10(b) measurement: 36 real sites, 24 of them in gorget-js — the exclusivity
   package is NOT a repo-internal change.
