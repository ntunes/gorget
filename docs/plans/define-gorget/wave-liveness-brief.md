# Executor Brief — Self-host UNIFIED SAFETY WALK + liveness/use-after-move pass

**Track:** Wave-Liveness (owner-ruled HIGH, ONE gauntlet). **Base:** main `1b478631`.
**Deliverable:** replace the self-host's TWO parallel safety walks (`check_carrier_ops_*` stateless +
the proto's standalone `check_liveness_*`) with ONE unified `check_safety_*` walk where drop-purity
(A2-S) + place-overlap (B2) + liveness/use-after-move are ARMS of a single traversal — mirroring
production's `src/semantic/safety/` module layout. Fold in the owner's 4 decisions (Parts A–C below).

This closes the self-host's missing liveness axis: it currently ACCEPTS every use-after-move /
double-move that Rust gg + ggdef reject (`E_UseAfterMove`/`E_DoubleMove`). A conformance lane that
can't emit ratified registry diagnostics isn't conformant (same class as the D23 gate).

---

## 0. WORKTREE PREAMBLE (non-negotiable — read first)
Run `pwd` and `git rev-parse --show-toplevel` FIRST and confirm both point inside your worktree.
NEVER touch `/workspace/gorget` (main) or `/workspace/gorget-1` directly — every file operation,
`cargo` command, and `git` command runs in your worktree path. Do NOT `cd` into either. Do NOT use
absolute paths starting with `/workspace/gorget/...` or `/workspace/gorget-1/...` (your worktree nests
UNDER `/workspace/gorget`, so an absolute path there writes into MAIN). If `pwd` reports
`/workspace/gorget` or `/workspace/gorget-1`, STOP and report back. All file ops use paths RELATIVE to
your worktree. On an Edit-tool desync, re-Read and retry the Edit tool — NEVER fall back to a shell
heredoc with an absolute path. After any non-Edit-tool write, run `git -C /workspace/gorget status` and
STOP if it shows changes.

**Staging:** `git add <specific files>` ONLY — NEVER `git add -a`/`git add .`/`git commit -a`.
**NEVER `git stash`** (repo-global stack) — save state with `git diff > /tmp/live_exec_<name>.patch`.
**Checkpoint to /tmp EARLY** and after each phase — you are killable at any moment. **Run your FINAL
validation gates as FOREGROUND commands with generous explicit timeouts** — a backgrounded final run
stalls the completion handoff (this has killed three prior executors).

**CONTAMINATION CAVEAT (obey it):** concurrent-agent `/tmp` gg-build scratch intermittently reverts the
worktree driver to a STALE binary. So: build the self-host driver, then IMMEDIATELY `cp` the binary to a
PRIVATE `/tmp/live_exec_$$/` path, and run every measurement against THAT copy. Do NOT trust an
in-place driver after any concurrent build.

---

## 1. GROUND IN THE DOCS FIRST (mandatory — read before coding)
The code shows what IS; the docs show what's INTENDED. Base every design choice on these (cite in your report):
- `docs/language-design.md` — ownership/move semantics, use-after-move, the exclusivity/liveness model.
- `docs/book/` — the user mental model of `!`/moves/use-after-move (how it's MEANT to read).
- `docs/devbook/24-layering-discipline.md`, `docs/devbook/11-copy-on-write.md`,
  `docs/devbook/29-contributor-playbook.md` — safety-pass layering, "fix at the write site",
  "sibling-site drift — fix the class", "self-host as the elegance showcase".
- Production reference tracker `src/semantic/safety/origins.rs`: `check_use` (~:25 read-of-moved→UAM),
  `check_move` (~:468 mark; 2nd move→DoubleMove), `merge_branch_states` (~:568 moved-in-either;
  diverging arms filtered ~:574), loop handling (~:495-503), `check_stmt.rs:988-1022` (if-without-else
  merge + loop). This is the reference-grade shape you mirror.
- **The acceptance ORACLE is ggdef (the executable DEFINITION), NOT the Rust tracker.** `spec/ggdef/src/tests.rs`:
  `move_then_read_is_illformed` (~:173), `d10b_mover_copy_read_is_illformed_not_overlap` (~:1773),
  `d10b_order_twin_read_before_move_legal` (~:1799); `spec/ggdef/src/eval.rs` (~:21/:745, `Slot::Moved`→IllFormed).

---

## 2. THE PROVEN PROTOTYPE (your starting reference — DO NOT skip verifying it)
`docs/plans/define-gorget/scouts/patches/liveness-ext-proto.patch` — a **buildable core+A+B prototype**
(+462 over main; applies CLEAN via `git apply`). It is the TWO-WALK form: adds `check_liveness_*` as a
standalone second walk in `type_check_function` AFTER `check_carrier_ops_stmts`, plus `DkUseAfterMove` in
`diagnostic.gg`, plus Part A's skip-move-first line and Part B's `!self`/ConsumeCallable tracking.
It is your PROVEN behavioral oracle: the scout MEASURED it (whole-source blast = 0; the d10b fixture
table in §5; the Part B probe table). Read the full scout report `scouts/scout-liveness-ext.md` and the
core report `scouts/scout-liveness.md` for the infra map, the FP fix, and the found codegen bug.

**The proto is a TWO-WALK intermediate. The owner ruled the LANDED artifact must be the UNIFIED walk
(Part D). Recommended de-risking sequence (respects the ruling — the two-walk stage is a verification
checkpoint, not the final state):**

### PHASE 0 — reproduce the proven baseline (checkpoint)
Apply `liveness-ext-proto.patch`. `cargo build --release`. Build the self-host driver to a PRIVATE
path and re-measure whole-source blast = 0 (`driver ... lib --lir-c 2>err`; `wc -c err` == 0). Run every
`d10b_place_overlap/*.gg` and confirm the §5 table (two flips: `move_noncopyread_reject`→UAM,
`double_move_reject`→DM; both single-diagnostic). This confirms your base matches the scout. Commit on
your branch as an intermediate ("self-host liveness core+A+B, two-walk baseline").

### PHASE 1 — UNIFY into one `check_safety_*` walk (Part D)
Merge the two parallel walks into ONE `check_safety_stmts(stmts, int scope_id, &SafetyState, &scopes,
&types, &ctx)` (+ `_stmt`, `_expr`). Each arm runs BOTH the stateless carrier-ops rejects AND the
stateful liveness update in a single traversal. The two walks already mirror the same Stmt/Expr enums
arm-for-arm, so the merge is mechanical (scout confirmed: NO arm where they disagree on traversal).

`SafetyState`:
```
struct SafetyState:
    Dict[String, int] moved       # branched flow-state (cloned per branch, unioned moved-in-any)
    int loop_depth                # structural loop context — saved/restored, NOT branch-cloned
    Dict[String, int] loop_locals
    String rebind
```
`scope_id` STAYS an ambient `int` param (constant per function body, never branches). Extensible: a new
axis adds a field. Branch arms clone `moved` (via `live_branch`/`live_commit`, keep the proto's
merge-moved-in-any that filters diverging arms via `live_stmts_diverge`). Delete the separate
`check_carrier_ops_stmts(func.body, ...)` invocation (:1472) and the separate `check_liveness_stmts`
invocation the proto added — replace with ONE `check_safety_stmts(func.body, body_scope, &state, ...)`
with a fresh empty `SafetyState` per function.

**CALL-ARM EXACT ORDER (ECall + EMethodCall) — the pass-order rider, liveness PRECEDES aliasing:**
1. callee/receiver liveness (`check_safety_expr` on callee/receiver) + ConsumeCallable-consume (Part B)
   + `!self`-consume if the method resolves to `self_ownership==2` (Part B);
2. per-arg loop, left-to-right: if `a.ownership == OWN_MOVE` do the liveness move/double-move mark
   (`live_move_operand`), else liveness use-check; AND the existing carrier-ops ctor/ingest
   `reject_tainted_place` gate;
3. THEN `check_call_aliasing(args, ...)` — with Part A's **skip-move-first** line already in it.

Re-run ALL Phase-0 measurements — they must match EXACTLY (blast 0, same d10b results) — PLUS the
bootstrap fixed-point in isolation (see §6). Commit as the unified walk.

### PHASE 2 — Part C precise loops (RE-MEASURE — this is where FP risk lives)
Replace the proto's clone-and-discard loop handling (`SFor`/`SWhile`/`SLoop`) with precise tracking
per the scout's Part D/C design: walk the loop body IN-PLACE with `loop_depth+1`, a fresh innermost
`loop_locals` set (add the for-pattern binding + body `SVarDecl` names), track same-stmt `rebind` in the
`SAssign` arm; in `live_mark_move`: `loop_depth>0 ∧ root∉loop_locals ∧ root≠rebind → MoveInLoop`
diagnostic; after the body `merge(before, after_body)`. Mirror `origins.rs:495-503` + `check_stmt.rs:988-1022`.
Add a `DkMoveInLoop` diag kind (or reuse `DkUseAfterMove` with a loop-specific message — check what
production/ggdef distinguish; prefer a distinct kind if production has `E_MoveInLoop`).
**RE-MEASURE whole-source blast = 0** — the scout did NOT run a full precise-build whole-source blast
(the SafetyState threading was too much for a standalone scout build), so THIS is the load-bearing
measurement for precision. If ANY residual false positive surfaces that you cannot eliminate,
**fall back to clone-and-discard** (the owner-accepted floor) and FILE the precision follow-up in a
report note — do NOT ship a false positive (Core #8 anti-FP: over-rejection is worse than under-detection).

---

## 3. THE d10b INTEGRATION REJECT-LANE REWIRE (part of the fix, MEASURED by the scout — do NOT skip)
`tests/integration.rs` `self_host_driver_rejects_d10b_place_overlap` currently asserts
`stderr.contains("their places overlap")` for all 5 reject fixtures. After the pass-order flip, TWO of
them emit liveness messages instead:
- `move_noncopyread_reject.gg` (`f(!n, n.data)`) → **"use of `n` after it was moved"** (UAM). Update its
  assertion to the UAM message; update the `.gg` header comment ("overlap diagnostic" → use-after-move).
- `double_move_reject.gg` (`f(!n, !n)`) → **"`n` moved more than once (double move)"** (DM). Update its
  assertion to the DM message; the `.gg` header already anticipates the flip — confirm/refine it.
The other 3 (`writer_writer_reject`, `writer_subfield_reject`, `read_move_reject`) stay
"their places overlap". Restructure the test so each fixture asserts its correct axis message.
This convergence is the intended effect of the ratified pass-order rider (self-host now MATCHES
production/ggdef on `f(!x,!x)` — via UAM/DM, not the interim overlap code).

---

## 4. NEW liveness reject/accept fixtures + acceptance set
Mirror the existing `self_host_driver_rejects_d12_*` / `self_host_driver_rejects_d10b_*` integration
pattern (grep them in `integration.rs`). Add self-host reject fixtures covering the production negatives:
`double_move_error`, `use_after_move_error`, `use_after_move_branch_error`, `fstring_use_after_move_error`,
`consuming_self_use_after_move_error`, `borrow_field_use_after_move_error` — each asserting the self-host
driver emits the UAM/DM message. Add ACCEPT fixtures for the legal twins (read-before-move
`f(x.field, !x)`; branch save/restore `if c: sink(!x) else: use x`; re-init `y=!x; x=fresh(); use x`;
loop-local move; ConsumeCallable-once accept; `!self`-consume-with-no-post-use accept).
**Acceptance is ggdef fixture-for-fixture** — verify the self-host AGREES with ggdef's
`move_then_read_is_illformed` / `d10b_mover_copy_read_is_illformed_not_overlap` /
`d10b_order_twin_read_before_move_legal` (the DEFINITION oracle). Do NOT eyeball the Rust tracker; where
the self-host and production DIVERGE, ggdef breaks the tie.

**Reference-grade note (Core #8):** the scout MEASURED that production (Rust gg) DOUBLE-FIRES on
`f(!x, x.noncopy_field)` (emits BOTH BorrowConflict AND UseAfterMove — a redundant second diagnostic),
while the self-host correctly emits a SINGLE UAM (matching ggdef's single IllFormed). **The self-host is
reference-grade-BETTER here — do NOT replicate production's double-fire.** ggdef is the oracle; the
self-host matching the DEFINITION and improving on the reference is correct. (Production's redundant
double is filed as a LOW Rust-gg diagnostic wart — not your concern this gauntlet, not blocking: the
program is correctly rejected either way.)

---

## 5. d10b fixture table (the scout's MEASURED expectations — your regression oracle)
| fixture | source | expected (post-unification) | axis |
|---|---|---|---|
| `writer_writer_reject` | `f(&n,&n)` | REJECT "their places overlap" | overlap |
| `writer_subfield_reject` | `h(&n,&n.data)` | REJECT "their places overlap" | overlap |
| `read_move_reject` | `g(n,!n)` non-Copy read-before-move | REJECT "their places overlap" | overlap |
| `move_noncopyread_reject` | `f(!n,n.data)` move-before-read | REJECT "use of `n` after it was moved" | liveness UAM |
| `double_move_reject` | `f(!n,!n)` | REJECT "`n` moved more than once (double move)" | liveness DM |
| `disjoint_siblings_accept` | `f(&m.a,&m.b)` | ACCEPT | — |
| `writer_copyread_accept` | `f(&s,s.tag)` | ACCEPT | — |

---

## 6. GATES (agent runs the fast ones; parent drives the big sweeps)
YOU (executor) run, FOREGROUND, in your worktree:
- `cargo build --release` + `cargo test --lib` (must stay green).
- The whole-source blast re-measurement (private driver, `wc -c err` == 0) after Phase 1 AND Phase 2.
- Every `d10b_place_overlap/*.gg` + the new liveness fixtures against the private driver.
- **`self_host_bootstrap_fixed_point` in ISOLATION** — build to an isolated path, or run when no other
  agent is bootstrapping (the scout's 661 s bootstrap ran on a possibly-cache-clobbered driver; rigor
  demands a clean run). `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release
  self_host_bootstrap_fixed_point -- --test-threads=1 2>&1 | tee /tmp/live_exec_boot_$RANDOM.log`.
- Targeted: `cargo test --test integration -- self_host_driver_rejects` + the d10b tests + a ggdef
  run (`cargo test -p ggdef` or the ggdef test target — confirm the liveness fixtures pass).
The PARENT drives the full C + full LLVM integration sweeps (15-20 min each) and the final integration
gate — do NOT wait on those yourself; report when your foreground gates are green and your branch is
committed.

---

## 7. DO NOT (scope guards)
- Do NOT fix the discarded-`Dict.remove` `Option[int]` coalesce miscompile — KEEP the `live_reinit`
  helper idiom (owner-ruled: that's its own Rust-gg track). The helper is arguably cleaner style anyway.
- Do NOT add field-granularity partial-move precision (`f(!x.a, x.b)` disjoint-field) — root-name
  granularity is intentional (matches the proto); partial-move precision is a filed Medium follow-up.
- Do NOT touch production Rust (`src/`) or ggdef (`spec/ggdef/`) — this gauntlet is the SELF-HOST pass
  only. (You READ them as references; you do not modify them.)
- Do NOT reshape self-host source to dodge any gap — if the precise-loop pass hits a compiler bug,
  FILE it + fixture, do not rewrite around it.

---

## 8. YOUR FINAL REPORT MUST CONTAIN
1. Commit hash(es) on your branch + a one-line-per-commit summary (Phase 0 baseline / Phase 1 unify /
   Phase 2 precise loops / fixtures+rewire).
2. The MEASURED whole-source blast after Phase 1 AND Phase 2 (command + `wc -c err` == 0).
3. The d10b fixture results (all 7) + the new liveness fixture results — actual emitted messages.
4. `self_host_bootstrap_fixed_point` result (isolated run) + `cargo test --lib` result.
5. Net LOC of the unified walk vs main; confirmation the two-walk intermediate is GONE (one walk landed).
6. Whether Part C precise loops landed or fell back to clone-and-discard (+ why, if fallback) — with the
   measured blast that decided it.
7. Any surprise / new bug / FP — flagged LOUDLY (do NOT bury it; do NOT ship a known defect).
8. Confirm `git -C /workspace/gorget status` is CLEAN (you never wrote to main).
