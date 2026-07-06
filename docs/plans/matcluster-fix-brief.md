# EXECUTOR BRIEF: production materialize-on-write cluster (4 roots, 1 track, 4 sequenced commits)

> **STATUS: v3 — passes 1 (6 res) + 2 (2 blocking + 2 minor) folded; 0 clean of >=3. Executor: not launched.**
> ggdef-RATIFIED expected outputs: #1 warn_compound=10 · #2 loop_read_before_write=1,2,3,1 ·
> #3 ok_rebind=3,1 · #4 dead_branch_alias_bind=9. Scout artifacts: /tmp/recover_matcluster/
> (findings incl. the full draft brief + the measured #1 prototype patch).
> SELF-HOST SCOPE: #1/#2 fix BOTH compilers (bootstrap gate mandatory); #3/#4 Rust-only
> (self-host is the reference oracle). D2 plain-self is OUT (separate track, TODO:952).
> ⚠ Scout caveat: the self-host #1/#2 mirror was NOT bootstrap-gated in the scout session —
> the executor rebuilds the self-host and gates bootstrap_fixed_point before landing #1/#2.


## ⚡ REVIEW PASS-1 FOLDS (2026-07-06) — OVERRIDE the corresponding draft text below

- **R1 (#2 gate)**: #2 is the ONLY unprototyped root and the substrate extension is heavier than
  "add a branch" (the lazy machinery triggers at a `for s in coll` VarDecl via set_collection_ref;
  a bare param has NO VarDecl trigger — a NEW pre-loop analysis is needed: detect bare param
  mutated-in-loop w/ loop-carried read-after-write → pre-loop flag+slot → clone-once; self-host
  mirror: LazyMember is SVarDecl-only, lower_stmt.gg:695). The executor MUST build a Rust
  mini-prototype that measures `1,2,3,1` end-to-end BEFORE committing to the approach; if the
  trigger proves heavier than budgeted, RE-SCOPE #2 to its own track (pre-authorized) and land
  #1/#3/#4 without it.
- **R2 (self-host #1 mirror)**: the index-compound call goes at the **EIndex compound arm,
  lower_stmt.gg:1254, with `target_expr`** (exact mirror of SAssign :1032) — NOT inside
  lower_index_compound_assign (no target_expr param there). EFieldAccess arm (:1241) as drafted.
  Reminder: #1's "proto ready" is RUST-only; the self-host mirror is unprototyped and MUST be
  bootstrap-gated before landing.
- **R3 (#3 shape) — SUPERSEDED BY PASS-2 (the pass-1 wording contradicted the draft: "keep the
  pre-clone" retains the :92 slot-upgrade, which IS the bug)**: mirror the self-host oracle's
  UNIFORM shape — on a bare-param full rebind, re-point the NAME to a FRESH owned local
  (register_local); the param slot stays `void*`; the `builder.locals[..].type_id = inner`
  upgrade at assigns.rs:92 is REMOVED for all bare-param full rebinds; NO independent-vs-
  self-derived distinction (the self-host has none — lower_stmt.gg:945-953 drop_old=false for
  LoBorrowed → uniform consume+assign). REQUIRED #3 MINI-PROTOTYPE before commit: `xs=[9,9]`→
  3,1 AND `xs=xs.slice(...)`→correct, BOTH backends. Only if the prototype proves a distinction
  necessary: the RHS-references-local test is a recursive AST walk of `value` for
  Expr::Identifier == `name` (both in scope at :76-98) — and :92 is retained in NEITHER branch.
- **R4**: the compound-arm ratchet (mirror container_literal_arms_count) is a REQUIRED #1
  deliverable, same commit — not optional.

### Pass-2 folds (override precedence: pass-2 > pass-1 > draft)
- **#4 MECHANISM NAMED (was two unchosen options — now ONE, write-site, layer-correct):**
  `restore_locals` (context.rs:1654-1670) drops CollectionElement|FieldPath|CowBorrowPending|
  View for branch-local locals but KEEPS `Alias` — the :1635-1637 comment's "already severed by
  runtime CoW" claim is FALSE for a dead-branch bind (the alias slot is NULL on the not-taken
  path). FIX: add `BorrowOrigin::Alias(_)` to the :1659-1666 drop_state match → branch-local v5
  resets to unowned at scope exit → cow_aliases_of(v0) skips it → no clone(NULL). The runtime
  alias-live-flag option is DROPPED (read-side patch, wrong layer). Oracle: the self-host
  prints 9. Gates: fixture→9 with ZERO clones; NO regression on cow_lazy_d1_alias_deadpath
  (different topology — verified NOT covered by this change); ASan.
- **Stale remnant struck**: draft FIX DIRECTIONS #1's "inside lower_index_compound_assign
  (1932)" is superseded by pass-1 R2 (the :1254 arm with target_expr). Do not edit :1932.
- **R1 stop-condition named**: PROCEED = a contained extension of the cow_lazy_mat_flag
  substrate to the is_bare_param branch that measures 1,2,3,1 on both backends; RE-SCOPE = it
  requires a new whole-function pre-loop analysis pass.
- **Self-host #1 EFieldAccess placement**: top of the :1241 arm, before the :1246 read
  (faithful mirror of SAssign :1023).
- **R5 (zones)**: tests/integration.rs is SHARED with the concurrent P1-infra track — both
  additive; parent reconciles at merge. (P1-C was re-pointed to a new tests/spec_conformance.rs
  on the other track, reducing this further.)
- **R6**: post-fix, MOVE cow_dead_branch_alias_bind.gg from known_gaps/ into tests/fixtures/
  proper (it stops being a gap; promotes it into the runtime-diff corpus) — plus its test un-ignored.

## Operational rules
Worktree preamble verbatim (CLAUDE.md rule 2): pwd + rev-parse inside YOUR worktree; never touch
main/gorget-1; worktree-RELATIVE paths; never git stash (checkpoint /tmp patches); Edit-desync →
re-Read+retry; non-Edit writes → check main status, STOP on surprises. Zones: src/ir/lowering/
stmts/assigns.rs + src/ir/lowering/context.rs, tests/fixtures/self_host_lowerer/{lower_stmt,
lower_cow}.gg (#1/#2 mirrors ONLY), tests/fixtures/deadwrite_*.gg + cow_dead_branch_alias_bind
wiring, tests/integration.rs (your test fns), tests/lints.rs (the compound-arm ratchet, additive).
NEVER: TODO.md, docs/**, spec/**, spectests/**, tests/smith/** (concurrent P1-infra track).
FOUR SEQUENCED COMMITS (#1 → #3 → #2 → #4 order at executor discretion with rationale; #1 first —
proto ready), prefix fix(cow):, explicit file lists. Gates FOREGROUND, teed, GG_BUILD_TIMEOUT_
SECS=600; bootstrap_fixed_point after any self-host source change; ASan mandatory on #4.
Divergence discipline: any EXISTING fixture whose output changes = triage against ggdef (run it!)
and report — never silently accept. Full sweep + runtime_diff = parent.

--- SCOUT FINDINGS + DRAFT BRIEF (authoritative detail) ---

# Scout: materialize-on-write bug cluster — findings (in progress)

## Ratified expected outputs (ggdef)
1. deadwrite_warn_compound -> 10  (prod: 11 = write-through)
2. deadwrite_ok_loop_read_before_write -> 1,2,3,1 (prod: 1,1,1,1)
3. deadwrite_ok_rebind -> 3,1 (prod: CC-FAIL invalid C)
4. cow_dead_branch_alias_bind (known_gaps, #[ignore]) -> 9 (prod: SIGSEGV in gorget_array_clone)
5. D2 plain-self: materialize (prod: writes through) — inclusion TBD

## Key source: cow_before_mutation @ src/ir/lowering/context.rs:3312
- Handles bare param via is_bare_param -> cow_materialize_alias(local,local)
- Case1 alias, Case1b element borrow, Case2 aliases, Case3 collection refs (lazy loop path), Case4 str views, Case5 shared heap, Case6 field borrows
- Call sites: assigns.rs (index/field assign), calls.rs, exprs/mod.rs, exprs/methods.rs

## TODO
- read compound-assign path assigns.rs:607-942
- reproduce all 4 on built gg
- self-host verdict

## REPRODUCTION CONFIRMED (both backends, prod = current worktree HEAD)
| # | fixture | C backend | LLVM backend | ggdef-correct |
|---|---|---|---|---|
| 1 | deadwrite_warn_compound | prints 11 | prints 11 | 10 |
| 2 | deadwrite_ok_loop_read_before_write | 1,1,1,1 | 1,1,1,1 | 1,2,3,1 |
| 3 | deadwrite_ok_rebind | cc: incompatible types GorgetArray from void* | llc: %p0 ptr vs GorgetArray | 3,1 |
| 4 | cow_dead_branch_alias_bind | SIGSEGV exit139 | SIGSEGV exit139 | 9 |

All four reproduce on BOTH backends -> bug is in shared src/ir/lowering, NOT backend-specific.
Fix belongs in the GIR lowering layer (materialize machinery), applies to both.

## ROOT CAUSE #1 (compound-assign index) — CONFIRMED
- lower_index_assign (assigns.rs:920) calls cow_before_mutation on the root FIRST (lines 931-947).
- lower_compound_assign Index arm (assigns.rs:1480+) has NO cow_before_mutation prologue -> bare param not materialized -> writes through.
- SIBLING: lower_compound_assign FieldAccess arm (assigns.rs:1407) ALSO lacks it (s.field += x on bare param would write through). Verify.
- lower_field_assign (assigns.rs:591) — check it has the prologue (plain field assign).
- Fix: add the same root-materialize prologue (Identifier-direct + resolve_projection_root_local + cow_before_field_mutation) to BOTH compound arms. Core #4 class fix.

## RC #1 — FULLY ENUMERATED (Core #4 class)
lower_compound_assign (assigns.rs:1136) has THREE target arms; TWO lack the root-materialize prologue:
- Identifier arm (`x += 1`): whole-local, has W4 lazy-clear but the plain scalar case is fine (whole-value materialize handled by is_bare_param path when it fires? actually bare Vector param `xs += ...` is rebind-like) — NOT the bug shape.
- FieldAccess arm (assigns.rs:1407 `s.field OP= x`): NO cow_before_mutation prologue. GAP confirmed: `v[0].n += 1` on bare Vec[S] writes through (11) vs plain `v[0].n = 99` (10).
- Index arm (assigns.rs:1480 `xs[i] OP= x`): NO cow_before_mutation prologue. GAP confirmed direct (`xs[0] += 1` -> 11) AND projected (`s.counts[0] += 1` -> 11).
Fix: insert the SAME prologue lower_field_assign:604-625 / lower_index_assign:931-947 uses (Identifier-direct cow_before_mutation + resolve_projection_root_local + cow_before_field_mutation) at the TOP of BOTH compound arms, BEFORE they lower `object`. Plain struct-int fields already isolate via by-value copy (not a gap).
DeadBareParamWrite lint: still fires on the fixed shapes (write still dead); its warning text ("private scratch copy") becomes TRUE post-fix. No lint change needed for correctness.

## RC #2 — loop non-persistence — DISTINCT ROOT from #1
Emitted C for grow() (captured_c2.c:2815): loop body __bb2 does EACH iteration:
  __s1 = __p0 (xs permanently aliases caller); print len(__s1)=1;
  __v14 = gorget_array_clone(__s1)  -> __s6 (fresh clone of CALLER, always len1)
  memcpy(__s7,__s6); gorget_array_push(__s7,i); gorget_array_free(__s7)  <- THROWAWAY per iter
So the bare-param materialize is a per-statement throwaway that (a) re-clones from the caller each iteration and (b) is freed each iter -> pushes lost, private copy never persists.
ROOT: cow_before_mutation is_bare_param branch (context.rs:3319-3322) uses cow_materialize_alias (fresh-local, NOT loop-carried). Case 3 collection-refs ALREADY has a loop-carried variant (cow_materialize_view_lazy_in_place + cow_lazy_mat_flag, context.rs:3401). The bare-param path lacks it.
Straight-line works (xs.push(2);xs.push(3);len -> 3/1; scratch_read 99/1; value_pop 2/2) because the compile-time rebind persists across statements in one block; only ACROSS loop iterations does it fail (loop body lowered once; restore_locals reverts; runtime re-clones caller).
This is TODO:742 (N1 while-loop) generalized to for-loops. DISTINCT mechanism from #1 (which-shapes-call-the-hook); #2 is HOW-the-hook-materializes (needs loop-carried flag+slot for bare params).
Fix = extend the loop-carried lazy-materialize substrate to the is_bare_param case: pre-loop flag+slot, clone-once guarded, write back into the binding slot the loop-top read sees.

## RC #3 — full-rebind type transition — DISTINCT ROOT
Working push (captured_cpush.c): `__v0 = (void*)__p0` where __v0 is void*; materialize clones into a SEPARATE __s2/__s3 owned local; xs's slot STAYS void*. Correct.
Rebind (captured_c3.c): `xs = [9,9]` hits lower_assign Identifier arm clone-on-mutate block (assigns.rs:76-98). For a bare Ptr param (is_mut=false, GirType::Ptr(inner)) it clones AND does `builder.locals[local_id].type_id = inner` (line 92) UPGRADING xs's OWN slot to owned GorgetArray. This retro-types the entry binding `__v0 = (void*)__p0` (void* -> GorgetArray) = invalid C ("incompatible types GorgetArray from void*") / invalid LLVM ("ptr vs %GorgetArray").
ROOT: bare-param FULL-rebind is treated as in-place CoW upgrade. A full rebind DISCARDS the old value — cloning is wasteful (and mis-frees), and the slot-type upgrade breaks the entry param binding. Fix: rebind path should bind xs to the RHS's owned value WITHOUT the Ptr-clone-in-place upgrade (mirror the push case: param slot stays borrow, fresh owned local holds the rebind), OR emit the entry binding compatibly. NOT the compound-hook gap, NOT loop-carried.

## RC #4 — dead-branch conditional alias provenance — DISTINCT ROOT (SIGSEGV)
main() C (captured_c4.c): `v5 = v0` in the if-branch creates a merge-point phi `__bp48` for the alias provenance ptr: bb2 (taken) -> &__s1 (v0); bb3 (not taken) -> __v49 = NULL. At merge bb1 the `v0[2]=9` fires cow_before_mutation(v0) Case 2 (aliases) -> `gorget_array_clone(__v48)` UNCONDITIONALLY on the phi. Runtime takes the FALSE branch (3<3 false -> bb3) => __v48 = NULL => gorget_array_clone(NULL) => SIGSEGV. Both backends.
ROOT: conditional alias bind phis a NULL provenance on the not-taken path; the merge-point materialize clones it unconditionally. ggdef-correct = 9 with ZERO clones (dead branch never runs; v0 never aliased). Sibling of cow_lazy_d1_alias_deadpath (mutation-in-dead-branch, HANDLED) — this is BIND-in-dead-branch (MISSED). Fix per Core #1 at the WRITE/bind site: alias-set membership must be path-sensitive (v0's alias-set at merge must not blind-clone a maybe-uninitialized alias) OR the materialize clone guarded by an alias-live flag (the cow_lazy_mat_flag substrate is the precedent). DISTINCT from #1/#2/#3.

## HYPOTHESIS VERDICT: "one enumeration hole" REFUTED
The four share the bare-binding/alias materialize SUBSYSTEM (src/ir/lowering context.rs + assigns.rs) but have FOUR distinct roots:
 #1 which-shapes-call-the-hook (enumeration hole; clean Core#4 class fix)
 #2 how-the-hook-materializes-in-loops (loop-carried persistence)
 #3 full-rebind type transition (clone-on-mutate slot upgrade)
 #4 conditional alias provenance (dead-branch phi NULL + unconditional clone)
Only #1 is a single-class enumeration fix. #2/#3/#4 each need targeted work. Coherent as ONE TRACK (same files/subsystem/reviewer) but NOT one fix.

## D2 (plain-self) INCLUSION DECISION: SCOPE OUT — file separately
Evidence:
- self IS set_bare_param (functions.rs:1320/1336/2054) so is_bare_param(self)=true, BUT
- self is passed by POINTER (self_ptr_type, functions.rs:1317/2041) — materializing self = clone STRUCT POINTEE + rebind, a DISTINCT op from the by-value collection/array clone the cluster fixes.
- resolve_projection_root_local (exprs/mod.rs:2380) does NOT handle Expr::SelfExpr -> falls to `_ => None`. So self-rooted field/index/method mutations NEVER resolve a root -> materialize hook never fires. This is a DISTINCT resolution gap, not covered by the cluster's #1 fix.
- Confirmed: `self.n = 99` -> 99 (write-through; D2 wants 1); `&self` push -> 2 (correct intended write-through).
- D2 ledger (decisions.md) mandates a codebase-wide MIGRATION SWEEP (self-host/gorget-js/gorget-arena + many equip methods rely on plain-self write-through today) + a design confirmation. Broad blast radius.
- Already filed: TODO:952 (HIGH) + decisions.md D2. DeadBareParamWrite lint EXCLUDES self pending this (TODO:948c).
VERDICT: 4-bug cluster = one bounded track (localized bare-value-param/alias, no migration sweep, committed fixtures). self/D2 = separate track (SelfExpr root resolution + struct-pointee materialize + migration sweep + owner design call). Scout brief's own criterion ("if it's a separate subsystem (receiver lowering), scope it OUT") -> OUT.

## SELF-HOST VERDICT (driver = /tmp/scout_matcluster/sh_driver; emit-c -> cc -> run)
| # | shape | production | self-host | scope |
|---|---|---|---|---|
| 1 | compound `xs[0]+=1` | 11 (bug) | 11 (BUG) | BOTH compilers -> fix Rust + self-host |
| 2 | loop persistence | 1,1,1,1 (bug) | 1,1,1,1 (BUG) | BOTH compilers -> fix Rust + self-host |
| 3 | bare-param rebind | CC-FAIL (bug) | 3,1 (CORRECT) | Rust-ONLY -> self-host already right; mirror its lowering |
| 4 | dead-branch alias bind | SIGSEGV (bug) | 9 (CORRECT) | Rust-ONLY -> matches adjudication |
Implications:
- #1,#2 require fixing the self-host Gorget sources too (compound-assign + loop-materialize in lower_stmt.gg/lower_cow.gg) AND gating on self_host_bootstrap_fixed_point.
- #3,#4 are Rust-only; the self-host is the reference oracle. For #3 the self-host's rebind lowering is CORRECT — study it and make Rust match (self-host does NOT do the Ptr-clone-in-place type upgrade that breaks Rust's entry binding).

## PROTOTYPE #1 (LANDED in worktree, measured)
Edit: src/ir/lowering/stmts/assigns.rs — added materialize_assign_target_root() helper + called at top of compound FieldAccess arm (1407) and Index arm (1484).
Results (Rust, C backend): compound 10, projected s.counts[0]+=1 10, vecelem v[0].n+=1 10; LLVM compound 10; &-param write-through PRESERVED (11, 11). cargo test --lib: 1101 passed 0 failed.
Prototype patch saved to /tmp/scout_proto_matcluster_1.patch

## GATES MEASURED (with #1 prototype)
- cargo test --lib: 1101 passed, 0 failed.
- integration slice `deadwrite cow` (--test-threads=4): 112 passed, 2 failed, 2 ignored.
  - The 2 "failures" = cow_lazy_move_bind_self_host + cow_lazy_move_reassign_self_host = self-host BUILD TIMEOUTS (default GG_BUILD_TIMEOUT under 4x concurrent DEBUG self-host builds). Verified NOT a regression: my pre-built driver emits correct output for cow_lazy_move_bind (s = hello / w0 = mutated).
  - deadwrite_warn_compound PASSED -> DeadBareParamWrite lint still fires on the fixed shape (warning text now TRUE).
- Prototype patch: /tmp/scout_proto_matcluster_1.patch (56 lines, assigns.rs only).

## FIX DIRECTIONS (for brief)
#1 [DONE-proto, BOTH compilers]: add root-materialize prologue to compound FieldAccess+Index arms.
   Rust: materialize_assign_target_root() helper (proto). Self-host: mirror — call cow_materialize_projected_root(&ctx,&gmod,target_expr) in SCompoundAssign EFieldAccess arm (lower_stmt.gg:1241) + inside lower_index_compound_assign (1932), exactly as SAssign does (1023/1032). Helper already imported (lower_stmt.gg:31).
#2 [BOTH compilers]: extend the loop-carried lazy-materialize substrate (cow_lazy_mat_flag + cow_materialize_view_lazy_in_place, context.rs:3401) to the is_bare_param branch of cow_before_mutation (context.rs:3319). Self-host: cow_lazy_materialize_family already exists (lower_stmt.gg:1168) — the bare-param family member must join the lazy set so the materialize is loop-carried, not per-iteration throwaway.
#3 [Rust-ONLY; self-host is the CORRECT oracle -> mirror it]: bare-param FULL-rebind. Root = lower_assign Identifier arm clone-on-mutate (assigns.rs:76-98) does `builder.locals[local_id].type_id = inner` upgrading the param slot in place, breaking the entry binding. Fix = do NOT in-place-upgrade a bare Ptr param on a full rebind; bind the RHS owned value to a fresh/retyped local without the void*->owned entry-binding conflict. Study the self-host's rebind lowering (produces 3,1 correctly) for the shape.
#4 [Rust-ONLY; self-host CORRECT=9; SIGSEGV/memory-safety]: dead-branch conditional alias bind. Root = alias provenance phis NULL on the not-taken path; merge-point cow_before_mutation Case 2 clones it unconditionally (gorget_array_clone(NULL)). Fix per Core #1 at the bind/write site: path-sensitive alias-set membership OR guard the materialize clone by an alias-live flag (cow_lazy_mat_flag substrate precedent). Sibling of cow_lazy_d1_alias_deadpath (mutation-in-dead-branch handled). Existing #[ignore]d fixture: known_gaps/cow_dead_branch_alias_bind.gg (expects 9). Separable brief candidate (memory-safety, distinct root, existing fixture).

## WHY THESE ESCAPED (brief note)
deadwrite_* fixtures are wired via check_gg_warns / check_gg_silent_for = `gg check` ONLY (tests/integration.rs:7026/7056) — never build+run, never assert stdout. So #1/#2 (wrong stdout) and #3 (invalid C at cc) were invisible. The ggdef differential (run BOTH compilers, diff stdout) caught them. Fix: give these fixtures committed run_gg(stdout) expectations (the first time). Current warn text for compound: "write to bare parameter `xs` lands on a private copy that is discarded" — a LIE today (writes through), TRUE after #1.

## WORKTREE / ARTIFACTS
- Prototype #1 LEFT in worktree (src/ir/lowering/stmts/assigns.rs). Patch: /tmp/scout_proto_matcluster_1.patch
- Self-host driver (pre-#1, reference oracle): /tmp/scout_matcluster/sh_driver
- Repros: /tmp/scout_matcluster/repros/  ; captured C: /tmp/scout_matcluster/captured_*.c
- Findings: /tmp/scout_matcluster_findings.md
