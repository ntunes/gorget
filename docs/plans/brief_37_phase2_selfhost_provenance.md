# BRIEF — #37 Phase 2: lazy CoW in the SELF-HOST, provenance-direct (the documented `ViewOf` design)

Status: v5 (pass-4 review folded 2026-06-10: ⚠ the Scope "typed axis" bullet
REWRITTEN [p4-R1, blocking]: the v1 `borrow_view_fn` schema field-add was not
executable in-zone (shared schema `include_str!`'d into Rust with an arity
assert) AND unnecessary — the EXISTING `materialize_fn` presence via
`resource_meta_for` is the typed discriminator (Some for exactly
GorgetString); ViewOf cite :2236→:2247 [p4-N1]; oracle-exception harness
helpers named [p4-N2]. Pass 4 verified the oracle-exception clause is
internally consistent at all four sites, implementable in the harness, the
snapshot-regen skip-on-mismatch premise solid, no v2/v3 remnants. v4 was
pass-3 folded 2026-06-10: ⚠ **MOVE-SHAPE ORACLE EXCEPTION
added** [p3-R1, the load-bearing one]: Rust gg is VALUE-WRONG on both EMove
shapes (re-proven fresh), so EMove-row fixtures assert the EAGER-SEMANTICS
stdout through the SELF-HOST route (gate-ON == gate-OFF, run-proven equal),
are listed EXPECTED-WRONG in runtime_diff and EXCLUDED from the Gate-4 flip
arithmetic, and get snapshots only after the Rust HIGH TODO lands — never
bake Rust's wrong output (Don't redesign around compiler gaps); W2 hook list
regrammared — FIVE hooks, EMove covered by EXCLUSION not hook [p3-R2];
per-source-NAME whole-fn granularity stated honestly + the borrow-checker
narrowing note [p3-R3]; the per-position-hooks laziness-upgrade TODO added to
W3's list [p3-R4]. Pass 3 RUN-PROVED the v3 exclusion end-to-end: both move
shapes garbage→eager-correct; exotics (`return !v`, `[!v]`, `match !v`) all
fall back eager (scan walks all three); witnesses/d1/battery non-interference
exact; stage2 gate-ON self-compile cc-clean with the SAME 7 live lazy binds
(exclusion costs zero on the compiler's own code); N-member family guard
chain run-proven; the materialize clone fn is `clone_to_owned`
(deep-copies views), not `copy_cow`. v3 was pass-2 folded 2026-06-10: ⚠ the EMove class executes at
MULTIPLE lowering positions (no choke point — move-REASSIGN `w = !v` to an
existing local is a run-proven UAF even WITH the v2 move-bind hook) → the
PRIMARY fix is now the **`cow_moved_names` EXCLUSION** (one scan side-channel
+ one eligibility term, sound for the entire EMove class at every position
incl. `return !v`/literal-element/scrutinee exotics); the run-proven
per-position hooks (bind + reassign, pass-2-prototyped) are recorded as the
future laziness upgrade [p2-R1+R2]; class-rule producer list corrected to the
ACTUAL six scan arms, SWith/spawn/comprehension = no-distinct-route rows
[p2-R3]; table drafted in W2, lands in devbook/11 in W3 [p2-R4]; snapshot
wording covers route fixtures that never flip [p2-R5]; the Rust HIGH entry
names BOTH move shapes [p2-R6]; inline-closure-spawn-arg pre-existing gap →
TODO [p2-note]. Pass 2 also CLOSED the self-compile gap: stage2-lazy 386s,
exactly 7 live lazy binds, 3/3 stage2-compiled fixtures oracle-exact; RSS
script runs (JSON peak_rss_kb + clone_stats + --compare). v2 was pass-1
folded 2026-06-10: ⚠ TWO run-proven BLOCKING holes —
(1) the `w = !v` MOVE-BIND mutation route escapes the family (lazy UAF,
ASan-silent; AND Rust Phase 1 is VALUE-WRONG on the same shape vs eager —
filed as a Rust TODO) → W2 gains the move-bind hook + the SCAN-ARM↔HOOK 1:1
CLASS RULE [p1-R1]; (2) the F1 scan fix was redirect-blind for IMPORTED
callees (self-host loader mangles names; Rust's doesn't — the parity argument
breaks there) → W1 resolves `call_redirects` first + two-module fixture
[p1-R2]; W1 scoped scan-side-only [p1-R3]; devbook/11 named as the
Chain-A-shared file [p1-R4]; cite-by-function-name rule [p1-R5]; pre-existing
bare-alias gap → TODO [p1-note]. Pass 1 re-proved end-to-end: gate-OFF
byte-identical (10 cmp), witnesses 0/1/0, d1 dead/taken 0/1 vs Rust 2/2 with
fresh traces BOTH sides, 29/29 non-lazy canaries gate-ON, fixed_point GREEN
475.65s with the prototype in-tree, w3d-join-required confirmed, F2
behavior-neutrality confirmed. v1 was the orchestrator draft from scout
`agent-a04df9b351a864cb2`; prototype committed at
`docs/plans/cow_phase2_selfhost_prototype.diff` (`2f40a7f4`).)

## Mission

Productionize the RUN-PROVEN provenance-direct prototype as the SELF-HOST's
default CoW lowering for lazy-eligible binds: cap=0 view bind + per-family
materialized-flag + family-materialize at every mutation site, with provenance
JOINS at derivation choke points instead of Rust Phase 1's read-site
materialize hooks. Identical observable outputs; strictly lazier than Rust
(d1_alias dead-path: SH-lazy **0** executed clones vs Rust Phase 1 **2**).
Also lands the F1 parser/scan soundness fix UNGATED (it is a correctness fix
independent of lazy). Owner-directed design goal; doc-grounded:
`docs/language-design.md:2247` specifies "compile-time **ViewOf(source)
provenance tracking** to auto-materialize views when the source is mutated" —
provenance IS the documented spec; Rust Phase 1's four hooks are the
deviation, and the Rust 1b back-port becomes a TODO informed by this chain.

## Ground truth (scout-verified, pass-1 re-proven; ⚠ cite-by-FUNCTION-NAME
[p1-R5]: the line numbers below matched neither pristine nor diff-applied
tree exactly — locate every site by function name via TODO.md's MODULE MAP,
never by the numbers)

- **Substrate (all verified):** the self-host CoW model = whole-fn AST
  pre-scan (`lower_cow.gg:66-353`) + bind-time `decide_svardecl_emission`
  (core `lower.gg:1090+`; the Branch C-pre GtPtr arm `:1235-1273` is the
  bind gate — both-pristine → `BorrowAlias`, source-mutated-forward → eager
  `CloneAndMove`, the arm Phase 2 replaces). There is NO mutation-site
  dispatch in the pristine self-host — the port INTRODUCES one. Bind emission
  `lower_stmt.gg:199-428` (match plan; prototype adds `LazyViewBind` `:429`).
  Flat `named_locals` + memory-slot model: NO name rebind ever happens → no
  restore_locals problem; only the bool flag needs SSA carry and **`lir_ssa`
  handles it with ZERO changes** (empirical: the flag is a loop-header block
  param threaded around the back-edge; `lir_ssa.gg` untouched by the
  prototype).
- **Provenance-by-slot-aliasing (the headline mechanism):** the lazy slot is
  a C local mutated in place; `String x = s` routes through existing Branch A
  which emits a POINTER TO THE SLOT (`__v27 = &__s9`) — the alias derefs at
  read time and sees the materialized value. The W3a derivation route needs
  NO code. This is exactly where Rust's SSA-versioned locals broke (the D1
  alias-captured-a-stale-version class) — the objection that made Rust reject
  provenance dissolves on this substrate.
- **Escape safety for free:** `op_consume` LoView→OpClone at every consume
  kind (return/push/ctor of a still-view deep-copies; `cow_lazy_escape_return`
  MATCH at 1 clone, no SReturn hook). Runtime `elem_materialize` upgrades any
  cap=0 view landing in a collection slot.
- **Typed metadata present:** `BuiltinMethodDecl.returns_view`
  (`compiler/data/schema.gg:224`, table `resources.gg:2721+`) consumed at
  exactly ONE choke point (`lower_expr.gg:1578-1583`, the LoView tag site) —
  the self-host permits the single-choke-point centralization Rust could not
  do. `LocalOwnership.LoView` + `BorrowOrigin.BoRuntimeView/BoCollectionElement`
  (`gir.gg:178+/:240+`). GIR format UNCHANGED (the prototype emits only
  existing instruction kinds).
- **Prototype results (env-gated `GG_COW_LAZY=1`; executed clones via the
  transient `GG_CLONE_TRACE` instrumentation — re-add for measurement, REVERT
  before committing, exactly like Phase 1):** witnesses 0/1/0 (eager 1/1/1);
  d1_alias dead-path **0** / taken **1** (Rust 2/2, SH-eager 2/2);
  `mutarg_probe` + `cow_lazy_move_consume` flip WRONG→MATCH (the F1 fix);
  18-fixture battery 13 MATCH with w3d regressing gate-ON (the for-string
  join is REQUIRED pre-flip); 8 CoW canaries MATCH; 11-fixture cow_*/string
  sweep MATCH. Gate-OFF emitted C byte-identical to pristine. fixed_point
  GREEN (539s) with the prototype in-tree. Gate-ON SELF-COMPILE works: stage2
  has 7 live lazy binds in the compiler's own code; stage2-lazy driver
  compiles fixtures to oracle-exact output. ASan clean on 9 gate-ON binaries
  (defense-in-depth only — ASan is proven BLIND to the wrong-output and
  view-UAF classes; the stdout battery is the primary net).
- **Three PRE-EXISTING self-host bugs found while baselining (eager mode,
  not lazy-caused):**
  - **F1 (soundness, root-caused, FIXED-in-prototype via signature-driven
    scan):** `parser.gg` `skip_ownership_markers` in `parse_call_args`
    DISCARDS `&`/`!` sigils on call arguments — no `EMutableBorrow`/`EMove`
    wrapper, so `lower_cow.gg`'s scan arms are dead code for call args and
    the pristine gate mis-fires BorrowAlias on mutated-via-call collections
    (wrong output/UAF; `mutarg_probe`, `cow_lazy_move_consume`,
    `cow_lazy_w3b_*` WRONG today). devbook/24 rule-1 violation (parser drops
    a typed invariant). The signature-driven scan fix (mirroring Rust's
    prescan `functions.rs:446-505`) is correct AND is Rust-parity; the
    parser keeping the sigil as a typed arg-ownership field is the long-term
    fix → TODO.
  - **F2:** string index/slice (`s[0..5]`, `s[1]`) miscompiles in the
    self-host (binds int 0 / null) — pre-existing WRONG-OUTPUT on
    `cow_lazy_w3c_*`; BLOCKS the index/slice provenance join → that join is
    deferred behind F2 (TODO), and lazy does not change those shapes (they
    miscompile before CoW matters).
  - **F3:** `string_stress_methods` CC-FAIL (`int64_t__str`) — unrelated,
    TODO.

## Scope decisions

- **Bound-name-pristine eligibility gate KEPT** (the prototype's
  simplification): no write-site tag-clearing machinery; member reassignment
  shapes (`staletag`/`compound`) stay eager at 1 clone vs Rust's 0 — outputs
  identical, simpler invariant. Document the trade in devbook/11; porting
  W4-style clearing is a possible later optimization (TODO, low).
- **Index/slice join deferred behind F2** (above). For-string join is IN
  SCOPE and REQUIRED pre-flip (w3d regresses gate-ON without it).
- **F1 fix lands UNGATED and FIRST** (its own commit; it corrects eager-mode
  wrong outputs — possible runtime_diff parity WINS; measure).
- **Typed axis, no name-matching [REWRITTEN per p4-R1 — the v1 field-add was
  NOT executable in-zone]:** replace the prototype's
  `runtime_name == "GorgetString"` read with a PRESENCE-CHECK of the
  EXISTING typed discriminator `ResourceMetadata.materialize_fn` via the
  existing `resource_meta_for` accessor — `materialize_fn` is `Some` for
  exactly GorgetString (the canonical table's own comment: "the only resource
  with a view discriminator today", `compiler/data/resources.gg:62/:73`;
  schema doc `schema.gg:82`). Zero schema edits, no name-matching, one source
  of truth. ⚠ The field's PRESENCE is the eligibility discriminator ONLY —
  the lazy materialize still calls `clone_to_owned` via the already-typed
  `pointee_clone_fn`; do NOT wire `gorget_string_materialize_inplace` into
  the lazy path. (Rationale for abandoning the v1 `borrow_view_fn` field-add:
  the schema struct is SHARED with Rust — `compiler/data/schema.gg:74` is
  `include_str!`'d into the Rust binary with a 13-arity hard assert
  `src/resources.rs:355` — so the field-add would force out-of-zone `src/` +
  schema/owner-gated edits; and `build_resource_metadata`'s table-first
  lookup makes the lir_lower fallback row DEAD for String anyway.)

## The work

### W1 — Commit 1 (UNGATED): the F1 soundness fix — SCAN-SIDE ONLY
Signature-driven `&`/`!`-arg mutation detection in the `lower_cow.gg` scan
(the prototype's change in the scan's ECall arm), covering BOTH free-call and
method non-receiver args at the SCAN level. ⚠ p1-R2 (run-proven): the lookup
MUST resolve `gmod.call_redirects` BEFORE consulting
`fn_borrow_params`/`fn_move_params` — the self-host loader MANGLES imported
fn names (`loader.gg`, mod_prefix + name) so the maps are keyed by the
mangled name while the AST callee is bare; `lower_call` resolves redirects
first (mirror it). Without this, F1 survives W1 for EVERY cross-module
`&`/`!` call (probe: imported `poke(&c)` — Rust `s = alpha`, self-host
`s = mutated` both modes). Rust's prescan keys raw names safely only because
the Rust loader does not mangle. NOTE (p1-R3): W1 is the SCAN fix only — the
mutation HOOKS at the lowering sites are W2 (env-gated); do not add gated
code in this ungated commit (the pristine lowering's classification at those
sites is already signature-driven). Fixtures: the single-module
`mutarg_probe` AND a TWO-MODULE import variant (the p1-R2 shape);
`mutarg_probe` (the scout's repro shape) wired via `run_gg`; flip
`cow_lazy_move_consume` expectations are already correct (Rust oracle) — it
should now MATCH through the self-host; record which runtime_diff rows flip.
Gate: full battery + canaries through the self-host (stdout diff vs Rust),
fixed_point GREEN, `self_host_runtime` 0-regress, comparisons
baseline-relative, runtime_diff ≥ Step-0 baseline. Commit.

### W2 — Commits 2..n-1 (env-gated `GG_COW_LAZY=1`): the provenance mechanism
Apply/productionize the prototype: `LazyMember{root, slot, flag, slot_type,
clone_fn}` + `LowerCtx.cow_lazy_members`; `LazyViewBind` plan variant + bind
emission; ONE shared `cow_lazy_materialize_family` guard emitter
(`lower.gg:1313`); mutation hooks = method-receiver (in `lower_method_call`'s mutating-receiver
arm), SAssign target root (in `lower_assign`), free-call `&`/`!` args (in
`lower_call`'s arg loop, redirect-resolved per W1) + ADD SCompoundAssign,
method non-receiver `&`/`!` args — FIVE hooks total. The SIXTH scan arm, the
**EMove class**, is covered by ELIGIBILITY EXCLUSION, NOT a hook
(p1-R1 + p2-R1, both run-proven lazy UAFs, ASan-silent): `EMove` survives the parser at EVERY
expression position except call args (decl-init `w = !v` move-BIND;
assign-RHS move-REASSIGN to an existing local — which defeats the
SAssign-target-root hook since the family is keyed by the SOURCE name;
`return !v`; literal elements; match scrutinee) and has NO lowering choke
point (`lower_expr`'s EMove arm is a passthrough). **PRIMARY fix [p2-R2]:
the `cow_moved_names` EXCLUSION** — a new scan side-channel set written by
the scan's EMove arm (the existing `cow_mark_name` records only
name+position, NOT kind — "the scan already knows" was refuted), checked at
`decide_svardecl_emission`: a bind whose source collection ∈
`cow_moved_names` is NOT lazy-eligible (falls back eager). ONE predicate
term, sound for the entire EMove position class including the exotics.
HONEST granularity [p3-R3]: the exclusion is WHOLE-FN per-source-NAME — one
`!v` anywhere (even on a never-taken branch; run-proven 1 clone where lazy
would be 0) makes every bind from `v` in that fn eager. Acceptable: the
borrow checker independently rejects conditional-move-then-use shapes
("use of moved value"), so the practical loss window is narrow; document the
trade in devbook/11. The pass-2-PROTOTYPED per-position
hooks (materialize-family-before-move at the SVarDecl-EMove and
SAssign-EMove-RHS sites — both run-proven printing the eager value) are the
future laziness UPGRADE → TODO entry, not Phase-2 scope.
**THE CLASS RULE (p1-R1/p2-R1, the load-bearing generalization):** the
`cow_mark_*`-producing scan arms are EXACTLY SIX (pass-2 ground truth):
EMethodCall-mutating-receiver, EMutableBorrow, EMove, the W1 ECall sig-args
arm, SAssign target, SCompoundAssign target. The devbook table enumerates
**(scan arm × lowering position)** — per-arm alone is insufficient for arms
without a lowering choke point (EMove proved it) — and requires a 1:1
lowering hook OR eligibility exclusion per row, with ONE FIXTURE PER ROW;
SWith/spawn/comprehension are recorded as no-distinct-route rows (they only
recurse; their lowerings route through hooked paths — pass-2-verified), and
excluded/unreachable rows (`return !v` = no post-read possible) carry their
justification. The executor cites the complete table in the PR;
derivation JOINS = for-string source (`lower_loops.gg:326-334`, one site —
REQUIRED) and `returns_view` results at the single choke point
(`lower_expr.gg:1578-1583`, receiver-is-member → result joins the family
with its own flag — this also flips the pre-existing `cow_lazy_w3b_*`
WRONG-OUTPUTs to MATCH while keeping them lazy); the typed eligibility read
(`materialize_fn` presence via `resource_meta_for`, per the rewritten Scope
bullet — NOT a schema field-add); drop-tracking via a typed override (the prototype pushes the DropEntry
directly because `register_local_for_drop` skips LoView — productionize,
don't hack); statement-scope retirement for temp family members (avoid dead
guards). Every intermediate commit green with the gate OFF (gate-OFF emitted
C byte-identical — the scout's construction; verify per commit with the cmp
trick).

### W3 — Final commit: the default flip (atomic, like Phase 1)
Remove the env-gate (lazy becomes the default arm of the Branch C-pre else);
add fixtures: `d1_alias_deadpath` + `d1_alias_takenpath` (the beats-Rust
deltas; outputs Rust-oracle), an emitted-C clone-shape lock-in for the
SELF-HOST driver output mirroring Rust's (witness_never: borrow_view present,
exactly-1 clone_to_owned in main, dynamically dead), and snapshot additions
for every fixture that flips to MATCH (move_consume, w3b pair, the new
probes). REVERT any transient GG_CLONE_TRACE instrumentation. Docs: devbook/11
gains the Phase-2 section (provenance-by-slot-aliasing; the family model; the
single returns_view choke point; the pristine-gate trade; the self-host vs
Rust mechanism comparison + the beats-Rust deltas; **the (scan-arm ×
lowering-position) mutation-route table** — drafted as code comments + the
W2 commit message during W2, LANDED here [p2-R4]); language-design.md cross
-reference (the ViewOf spec is now implemented in the self-host). Snapshots:
every NEW class-table route fixture AND every pre-existing fixture that
flips to MATCH (route fixtures that pass identically in both modes are still
wired + snapshotted as gate-ON regression guards [p2-R5]) — **EXCEPT the
MOVE-SHAPE ORACLE EXCEPTION [p3-R1]: Rust gg is VALUE-WRONG on both EMove
shapes (the HIGH Rust TODO), so EMove-row fixtures (a) assert the
EAGER-SEMANTICS stdout (the self-host's gate-ON == gate-OFF output,
run-proven equal) via tests against the SELF-HOST route, NOT the Rust oracle
— directly expressible in the harness [p4-N2]: compose
`build_gg_dir_cached("self_host_lowerer", "driver.gg")` (OnceLock-cached,
`integration.rs:~9483`) + `self_host_emit_cc_run(...)` (`:~15676`) under
`#[serial(self_host_lowerer_driver)]`, asserting the literal expected
string; (W1's two-module fixture similarly needs `run_gg_dir`, not
`run_gg`); (b) are listed EXPECTED-WRONG in runtime_diff and EXCLUDED
from Gate-4's flip arithmetic; (c) are NOT snapshotted until the Rust HIGH
TODO lands (the snapshot regen mechanism seeds only self==Rust matches, and
baking Rust's wrong output is forbidden per Don't-redesign-around-gaps)**.
TODO/DONE
per Task Continuity (pending-phrased only; the Rust 1b back-port entry with
the scout's §9 sketch — slot-address alias at Branch C + family-keyed
cow_lazy_mat_flag + hooks become joins; F2/F3 entries; parser typed
arg-ownership field entry; W4-clearing-port low-pri entry; the EMove
per-position-hooks laziness UPGRADE (pass-2-prototyped, run-proven at the
SVarDecl-EMove and SAssign-EMove-RHS sites — replaces the exclusion with
materialize-family-before-move if move-shape laziness ever matters) [p3-R4];
⚠ **HIGH-pri RUST entry [p1-R1/p2-R6]: Rust Phase 1 is VALUE-WRONG vs eager
semantics on BOTH move shapes** — move-BIND (`Vector[String] w = !v`) AND
move-REASSIGN (`w = !v` to an existing local), each followed by `w.set(0,…)`
then a read of the lazy-bound `s`: eager prints the pre-mutation value,
Rust-lazy prints the post-mutation value (read-through; memory-safe but a
Phase-1 behavior regression that slipped through all 21 fixtures — the W5
list had `consume(!v)` but neither move shape; re-confirmed fresh at the
current tip by pass 2); fix in the Rust 1b chain or as a fast-follow, same
fixture shapes; ALSO the pre-existing bare-alias gap (`w = v` collection
alias then mutate — WRONG vs Rust identically in both modes, a pristine-flip
name-keying gap, NOT lazy-introduced [p1-note]); ALSO the pre-existing
inline-closure-spawn-arg gap (`spawn ((…):…)(&v)` is scan-invisible AND
hook-less but mode-INDEPENDENT — breaks the pristine flip in eager
identically [p2-note])).

## Gates (executor runs 0-6; parent re-runs the battery + full suite on the
integrated tree)

0. Step-0 on the pristine worktree: runtime_diff baseline (record `PARITY =`),
   comparisons matched-counts, eager ASan table over the battery+canaries,
   **self-compile RSS baseline via `scripts/self_host_mem_baseline.sh`**
   (CLAUDE.md: perf work measures MEMORY — avoided clones become real memory
   at self-compile scale; fixture-scale RSS is noise).
1. Per-commit: `cargo test --lib` + `--test lints` (Chain A's 3 lints are
   LANDED at `e22183fb` — 10 total; they scan src/ only, no interaction);
   battery + canaries stdout-diff vs Rust (THE primary net) — EXCEPT the
   EMove-row fixtures, which diff against the EAGER-SEMANTICS expected output
   per the move-shape oracle exception [p3-R1]; gate-OFF emitted-C cmp (W2
   commits).
2. fixed_point GREEN per landed commit (`GG_BUILD_TIMEOUT_SECS=600`; ~450-540s).
3. `self_host_runtime` 0-regress; comparisons baseline-relative (0 new
   mismatches among pre-existing fixtures).
4. Post-flip: runtime_diff — expect ≥ baseline PLUS the flipped rows
   (move_consume, w3b pair, mutarg_probe, the d1 probes); the EMove-row
   fixtures are EXPECTED-WRONG rows (Rust oracle is the buggy side — list
   them explicitly, excluded from the flip arithmetic [p3-R1]); report the
   exact row movements.
5. Post-flip ASan sweep vs the Step-0 eager table (no NEW findings;
   supplementary only).
6. Post-flip self-compile RSS vs Step-0 (report the delta; a regression needs
   diagnosis before integration — stale family entries are the suspected
   mechanism, statement-scope retirement the mitigation).

## Constraints

- Executor: isolated worktree; `pwd` + `git rev-parse --show-toplevel`
  verification + `git merge --ff-only gorget-1` first; never touch
  `/workspace/gorget-1` or `main`; explicit-file `git add` only; driver-gated
  tests ONE at a time; tee long runs to `/tmp/<name>-$RANDOM.log`.
- File zone: `tests/fixtures/self_host_lowerer/*` (lower, lower_cow,
  lower_stmt, lower_expr, lower_loops, lower_closures as per the diff),
  `tests/fixtures/*.gg` (new), `tests/fixtures/runtime_snapshots/*`,
  `tests/integration.rs` (append), `docs/devbook/11` (⚠ SHARED with Chain A —
  it edits the §enumeration-rule section, you add the Phase-2 section;
  different sections, parent merges; rebase/re-grep if Chain A lands first),
  `docs/language-design.md` (cross-ref only), TODO.md, DONE.md. Do NOT touch `src/` (the transient
  GG_CLONE_TRACE re-add is allowed but MUST be reverted pre-commit; verify
  with `git status` before every commit). Do NOT touch `tests/lints.rs`.
- The scout's worktree (`agent-a04df9b351a864cb2`) holds the live prototype
  as fallback; the committed diff (`docs/plans/cow_phase2_selfhost_prototype.diff`)
  is the authority.
- Commit messages cite this brief + the scout; Co-Authored-By trailer.
- The self-host is the elegance showcase: the production code must read like
  the user manual — typed accessors, no name-matching, comments that state
  constraints (the provenance model, the pristine-gate trade) not history.
