# BRIEF — lower.gg module-split RETRY (peel fix + 9-cluster split re-land)

Status: v5 (pass-4 review folded 2026-06-10: `git apply --verbose` — plain apply is
SILENT on success [p4-R1]; comparison gate made arithmetically consistent — "0 new
mismatches" scoped to PRE-EXISTING fixtures, the mismatch set may grow only by the
3 new fixtures [p4-R2]; TODO grep pattern colon dropped [p4-R3]. Pass 4 also
re-verified: the temp-copy apply mechanism lands at the CORRECT site (`:7967`,
context unique 1/15,760), inline patch byte-EXACT vs the scout file, fixture-1's
struct-field shape is NECESSARY (Ptr-wrap requires `optionlike_resource_types`,
populated only from struct/enum-variant fields — `lower.gg:4980/:14262/:14266`),
Track-1 disjointness holds. v4 folded pass 3 [header-targeting + Step-0
unconditional]; v3 folded pass 2 [rehearsed conflict shape]; v2 folded pass 1;
v1 = orchestrator draft from the scout; scout worktree `agent-a7e4f59ccd22bc376`,
numbers regenerated at tip `a044a10f`.)

## Mission

Two-commit chain: **(1)** land the self-host `None`-arg expected-type **peel fix** —
a real user-facing self-host miscompile fix, independent of the split — then
**(2)** re-land the reverted 9-cluster `lower.gg` module split, which that fix
unblocks. Plus fixtures and TODO/DONE bookkeeping (the now-refuted TODO:172-178
root-cause narrative is retired per W3: entry deleted, completion → DONE.md,
pending residue re-added pending-phrased).

## Ground truth (scout-proven at `a044a10f`, pass-1-reviewer re-verified
independently — incl. reproducing the bug AND the fix; re-grep line numbers)

- **The reverted split** (preserved worktree `agent-a6756e70babfe0633`, staged
  uncommitted, based at `0a7703d7` — an ancestor of `a044a10f`; safe ONLY because
  zero commits touched `tests/fixtures/self_host_lowerer/` between them
  [reviewer-verified]; do not assume that generalizes after new commits): 10 files,
  `lower.gg` 15,760 → 3,325 lines, 9 new modules (`lower_drops` 463L,
  `lower_liveness` 1087L, `lower_types` 2044L, `lower_expr` 3086L, `lower_stmt`
  1148L, `lower_match` 1263L, `lower_loops` 1045L, `lower_closures` 1727L,
  `lower_generics` 911L), mechanical verbatim moves + 8 import lines + the
  NO_NAME→bare-`None` substitution (**152 substituted lines**; residue in core =
  the decl `:331` + 5 uses). `split-full-diff.patch` (scout worktree, repo root)
  applies cleanly to `a044a10f` (`git apply --check` re-verified by pass 1) and is
  a proper git diff with index lines → `--3way` viable.
- **Root cause of the old `fixed_point` failure — CONFIRMED; the import/no_name
  framing is REFUTED.** 146 identical cc errors:
  `incompatible types when assigning to type 'int64_t' from type 'Option__…'`.
  Chain (every link reviewer-re-verified at unsplit line numbers):
  1. `lower_function` Ptr-wraps bare resource-typed params and MutPtr-wraps `&`/`!`
     (unsplit `lower.gg:11583-11595`; wrapped tid → `GirFunction.param_types`
     `:11659`; split: `lower_closures.gg:105-112`).
  2. `lower_call`'s per-arg writer sets `ctx.expected_type` to that UNPEELED tid
     (unsplit `lower.gg:7966-7971`; split `lower_expr.gg:2861+`).
  3. `ENoneLiteral` arm (unsplit `:7000-7019`): the `enum_category_of` guard peels
     Ptr internally (`gir.gg:~746-750` `peel_ptr_tid`) so the Option branch fires,
     but `add_local(&ctx, ctx.expected_type, None)` types the None dst with the RAW
     Ptr tid — that asymmetry IS the bug.
  4. LIR: Ptr slot = scalar → `try_lower_prelude_variant` requires
     `slot_ty >= LT_STRUCT_BASE` (`lir_lower.gg:2434-2457`) → falls back to
     `ICallExtern("None")` → lir_codegen pick-first-`Option__X` fallback
     (`lir_codegen.gg:4421-4427`) emits `(Option__int64_t){.tag=1}` into an
     `int64_t` temp → cc rejects.
  - Reviewer reproduced PRE-fix (13-line single-module repro → the exact cc error)
    and POST-fix (prints `5` == Rust gg) on the unsplit tree → commit 1 is a
    genuine standalone fix and the W1 fixture WILL fail pre-fix. The corpus never
    hit it because `NO_NAME` routed through the correctly-typed `none_decls`
    EIdentifier path — the global was MASKING the gap.
- **The fix is the right LAYER (reviewer-confirmed):** Rust gg registers `fn_sigs`
  with **BASE types ("before pointer wrapping", `src/ir/lowering/functions.rs:659-669`)
  and threads expected_type from them (`calls.rs:1239-1255`) — the peel restores
  exactly Rust's value-type channel. The sibling writer already does it:
  `lower_field_write` Ptr/MutPtr-unwraps for the same bare-None reason (unsplit
  `lower.gg:9091-9122`; split `lower_stmt.gg:948-968`). Reader blast-radius clean:
  pass 1 enumerated ALL `ctx.expected_type` value-readers (if-chain seed `:5249`,
  closure-ret fallback `:5898`, ENoneLiteral `:7011`, array/tuple/struct-literal
  element derivation `:7042/:7155/:7216`, prelude-variant ctor `:8052`, auto-prop
  capture `:10278`) — none needs Ptr-ness; `:8052` is IMPROVED. 28-fixture
  snapshot spot-check through the post-fix driver: 28/28 MATCH.
- **THE PEEL PATCH (inlined — the authoritative shape; also at scout worktree
  `/workspace/gorget/.claude/worktrees/agent-a7e4f59ccd22bc376/none-peel-fix.patch`;
  written against split `lower_expr.gg:~2862`, transplant to the IDENTICAL unsplit
  block at `lower.gg:7966-7971`):**

  ```
  @@ -2862,6 +2862,25 @@
               ctx.expected_type = -1
               if ai < callee_param_types.len():
                   int pt = callee_param_types.get(ai).unwrap()
  +                # Peel the bare-resource-param ABI wrapper. `lower_function`
  +                # Ptr-wraps bare resource params (by-pointer ABI) and that
  +                # WRAPPED tid is what GirFunction.param_types records — but
  +                # the arg EXPRESSION's expected type is the VALUE type; the
  +                # pointer-ness is the call's ABI concern (OpBorrow at the
  +                # consume site), not the arg producer's. Without the peel, a
  +                # bare `None` arg against an `Option[String]` param types its
  +                # dst local as the Ptr tid → scalar LIR slot → the
  +                # prelude-variant intercept falls back → cc-invalid
  +                # `(Option__T){.tag=1}` assigned into an int64 temp (the
  +                # split-attempt's stage-1 cc failure, 146 sites). GtMutPtr
  +                # (`&`/`!` params) stays unpeeled: those args are explicitly
  +                # sigiled at source.
  +                if pt >= PRIM_COUNT and pt < gmod.type_table.len():
  +                    match gmod.type_table.get(pt).unwrap():
  +                        case GtPtr(pt_inner):
  +                            pt = pt_inner
  +                        else:
  +                            pass
                   if pt != I64_TYPE and pt != UNIT_TYPE:
                       ctx.expected_type = pt
               # Prelude-variant ctor (Ok/Error/Some): no callee_param_types, so
  ```

  (Hunk = 6 context + 19 added lines, exactly as the header counts; the trailing
  `# Prelude-variant ctor…` context line is part of the hunk. The 6-line context
  is UNIQUE in unsplit `lower.gg` [pass-2-verified] — no wrong-site risk; expect
  an offset (~`:7967`). All names resolve at that site: `PRIM_COUNT`/`I64_TYPE`/
  `UNIT_TYPE` imported at `lower.gg:13`, `GtPtr` via the `GirType` import `:11`,
  `gmod.type_table` is `Vector[GirType]` `gir.gg:414`.)

- **⚠ GtMutPtr-stays-unpeeled is a SCOPING decision with a RECORDED residual gap,
  not a correctness claim (pass-1 finding):** `pick3(5, !None)` against
  `Option[String] !label` runs under Rust gg but still CC-FAILs through the
  post-fix self-host (same error class — the sigiled form reaches the same
  raw-tid dst via the GtMutPtr wrapper). Bare `None` at `&`/`!` params is rejected
  by semantics ("ownership mismatch"), so only the SIGILED `!None` shape is
  affected. Zero `!None` occurrences in the split source (verified) → not
  split-blocking. Handled via fixture + TODO (W1), per "Don't redesign around
  compiler gaps" rule 2. Extending the peel to GtMutPtr is NOT in scope (would
  need its own gate battery + a think about move-ABI).
- **Measured proof on the combined tree (split + peel) at `a044a10f` (scout):**
  `self_host_bootstrap_fixed_point` ok 446s (pre-fix: 146 cc errors; post-fix
  `(Option__int64_t){.tag = 1}` 0× in stage1.c); `self_host_runtime` ok (443/0);
  `lowerer_comparison` 989/0 == baseline; `c_emit_comparison` 927/0 == baseline.
- **Visibility premise:** "plain top-level globals are un-importable" is TRUE by
  language spec (`docs/language-reference.md:890-897` §5.9, private-by-default;
  Rust gg `src/semantic/errors.rs:808` PrivateImport; empirically confirmed). The
  `no_name()` wrapper option is REJECTED — bare `None` is the documented idiom
  (`docs/language-reference.md:243,683`, `docs/book/09-option-result.md`) and a
  wrapper would dodge a live miscompile.
- **Two side-findings (TODO entries, NOT fixed in this chain), both
  reviewer-re-verified:**
  (a) LATENT silent-None class: bare `None` arg to a NOT-YET-LOWERED callee —
  reproduces SINGLE-MODULE (caller textually before callee: Rust prints 7,
  post-fix self-host prints 0). `get_fn_param_types` scans only already-lowered
  `gmod.functions` (unsplit `lower.gg:3765-3774`) → expected-type miss → silent
  `(Option__T){0}` = tag-0 Some miscompile. All 152 substituted split sites target
  earlier-lowered callees (carried by the scout's fixed_point GREEN) → doesn't
  block this chain. Durable fix = a param-type pre-pass mirroring Rust's typed
  fn-sig registration (`functions.rs:659-669`).
  (b) LIVE Rust gg bug: top-level `Option[T] G = None` statics zero-init → tag 0 →
  match as `Some("")` (plain AND `public static`; repro prints "some:"). High
  priority — a Rust miscompile.

## The work

### W0 — Step 0 (BEFORE any edits): regenerate the runtime_diff baseline
On the pristine worktree at tip (= the pre-change tree), run
`GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration
--release self_host_runtime_diff -- --nocapture 2>&1 | tee /tmp/rdiff-base-$RANDOM.log`
(~13 min) and record the freshly-printed `PARITY = MATCH/(...)` count. This is the
baseline W2's final-tree run compares against (never trust a dated number). Also
record the freshly-printed `lowerer_comparison` / `c_emit_comparison` matched
counts NOW, unconditionally, on this pristine tree — the W1/W2 gates are defined
relative to these Step-0 baselines and cannot be recovered after the tree changes.

### W1 — Commit 1: the peel fix on the UNSPLIT tree
- Obtain the peel patch: copy the scout's
  `/workspace/gorget/.claude/worktrees/agent-a7e4f59ccd22bc376/none-peel-fix.patch`
  and NORMALIZE its headers to standard relative `a/…`/`b/…` form (the original
  mixes an absolute `---` path with a relative `+++` path — plain `git apply`
  fails; un-normalized it needs `git apply -p0` [pass-2-verified]). The inlined
  text above is the FALLBACK if the worktree is gone (recreate the file from it,
  including the trailing context line, stripping the 2-space markdown indent, and
  add proper `---`/`+++` headers).
- Apply to the UNSPLIT tree via a TEMPORARY copy with BOTH header paths rewritten
  to `tests/fixtures/self_host_lowerer/lower.gg` — `git apply` targets the
  header-named file, never context (pass-3-verified: the `lower_expr.gg`-headed
  copy hard-fails on the unsplit tree; the `lower.gg`-headed copy succeeds).
  Use `git apply --verbose` — PLAIN `git apply` IS SILENT ON SUCCESS
  (pass-4-verified); with `--verbose` expect
  `Hunk #1 succeeded at 7967 (offset 5105 lines)`; alternatively verify via
  `git diff --stat` → `lower.gg | 19 +`. The context IS unique in `lower.gg`
  (1/15,760 lines), which is what makes the offset application safe. The COMMITTED
  `docs/plans/none_peel_fix.patch` keeps the normalized `lower_expr.gg` headers —
  it documents the final split layout and is what W2 step (b) re-applies.
  Peel `GtPtr` only; the comment must state the scoping decision + cite the
  `!None` residual gap TODO.
- Commit the NORMALIZED patch as `docs/plans/none_peel_fix.patch` in commit 1 —
  the durable-artifact precedent (`cow_loopcarried_prototype.diff`; /tmp and
  worktrees are ephemeral).
- Fixture 1 — `none_literal_at_call_arg_resource.gg` (named as the sibling of the
  EXISTING `none_literal_at_call_arg.gg`, which covers `Option[int]` — primitive
  payload, never Ptr-wrapped; header comment cites it to preempt "duplicate?"):
  struct with an `Option[String]` field (triggers optionlike-resource
  registration) + `Option[String]`-param fn called with bare `None` + printed
  result. Wire via `run_gg` embedded-stdout; add its
  `tests/fixtures/runtime_snapshots/<stem>.out` snapshot (it must PASS under the
  fixed self-host).
- Fixture 2 — forward-callee latent class, SINGLE-module (caller textually before
  callee; no 2-module dir needed): correct under Rust gg (passes integration);
  do NOT snapshot it (self-host still miscompiles); cite it in the TODO entry.
- Fixture 3 — `!None` sigiled shape (Rust-correct, runs under Rust gg): expected
  output per Rust; do NOT snapshot; cite in its TODO entry.
- Gate (driver-gated tests ONE at a time, `GG_BUILD_TIMEOUT_SECS=600`):
  `cargo build` + `cargo test --lib` + `cargo test --test lints`; the new fixture
  tests; `self_host_bootstrap_fixed_point` GREEN; `self_host_runtime` 0-regress;
  `lowerer_comparison` / `c_emit_comparison`: **0 new mismatches AMONG
  PRE-EXISTING fixtures and 0 crashes — the mismatch/crash columns may grow ONLY
  by (some of) the 3 new fixtures, and matched == the Step-0 baseline + however
  many of the 3 match** (fixtures 2/3 are deliberately self-host-broken shapes;
  whether they fn-count-match is unmeasured — record which of the 3 landed in
  which column). These tests auto-discover all `tests/fixtures/*.gg`, so adding
  fixtures moves the counts — the scout's 989/927 were pre-fixture figures;
  record the freshly-printed counts, do not gate on the literals. Commit.

### W2 — Commit 2: re-land the split
- Recover `split-full-diff.patch` from the scout worktree
  (`/workspace/gorget/.claude/worktrees/agent-a7e4f59ccd22bc376/`, repo root) —
  do NOT regenerate from the preserved a6756e70 worktree by hand.
- Apply over commit 1 with `git apply --3way`. What actually happens
  (pass-2 REHEARSED end-to-end): `lower_expr.gg` is created cleanly WITHOUT the
  peel (the patch predates it); the conflict is ONE giant delete/modify block in
  `lower.gg` (ours = the entire ~12k-line moved region incl. the peel; theirs =
  the deletion), file left UU — loud, never silent. Resolve in TWO steps:
  (a) `git checkout --theirs -- tests/fixtures/self_host_lowerer/lower.gg`
  (yields the 3,325-line core), then
  (b) apply the peel patch to the split `lower_expr.gg` (`git apply -p0` for the
  scout's original headers, or plain apply for the W1-normalized copy; lands at
  `lower_expr.gg:~2865`).
- **Equivalence proof (mandatory):** after resolution, diff the 10 changed `.gg`
  files against the scout's worktree copies (split + peel applied there), e.g.
  `diff -r --exclude=driver --exclude=driver.c` over
  `tests/fixtures/self_host_lowerer/` — the final tree must be textually
  IDENTICAL to the proven tree (pass-2's rehearsal achieved byte-identical; the
  scout tree's repo-root scratch files and the `driver`/`driver.c` build
  artifacts inside the fixture dir are the only legitimate noise). Any divergence
  must be explained line-by-line or eliminated.
- NO_NAME residue: the global + 5 core uses stay (correct and same-module).
  Optional commit 3 — retire them to bare `None` (elegance-showcase cleanup) ONLY
  if gates stay green; otherwise leave + TODO.
- Gate: same battery as W1 (incl. lints; comparison counts baseline-relative per
  W1's rule) + **the full runtime_diff on the final tree** (same command as
  Step 0, ~13 min): assert the final-tree MATCH count is **not below the Step-0
  baseline** — the 443-snapshot net alone is blind to parity movement on the
  other ~480 corpus fixtures. (The +3 new fixtures can only add MATCHes;
  fixtures 2/3 land in WRONG-OUTPUT/CC-FAIL and don't inflate.) Commit.

### W3 — Bookkeeping
- TODO.md: **DELETE the `🏗 SPLIT lower.gg` entry** (~:172-178) — after commit 2
  the split is COMPLETE, and completed work lives in DONE.md only (Task Continuity
  cardinal rule: never mark items done/landed in TODO.md; the output-review
  breadcrumb-check hunts exactly that). Keep only genuinely-PENDING residue, each
  phrased as the work that remains: the optional NO_NAME retirement if skipped,
  the `!None` GtMutPtr gap, side-findings (a)/(b).
  **Sweep ALL TODO entries citing `lower.gg`** (grep `lower\.gg` — NO trailing
  colon, :25/:195 mention it without one; includes :25's "Caveat until lower.gg
  is SPLIT…", :55, :195-196) — update or
  annotate each to point at the split modules (e.g. `lower_for` →
  `lower_loops.gg`). ADD entries: side-finding (a) latent silent-None class
  (single-module repro shape + Fixture 2 + "param-type pre-pass" fix sketch);
  side-finding (b) Rust gg `static Option[T] = None` zero-init bug (High — a Rust
  miscompile); the `!None` GtMutPtr residual gap (Fixture 3).
- DONE.md: one entry covering both commits (root-cause chain, the peel, the split
  shape, gate numbers freshly regenerated — quote commands, not stale figures).
- `docs/plans/none_peel_fix.patch` is committed in W1; do not delete it. Do not
  touch `docs/plans/cow_loopcarried_prototype.diff` (Track-1 artifact).

## Sequencing & constraints

- Nothing else is currently EXECUTING on `lower.gg` (the TODO IN-FLIGHT chains are
  queued, not running). This chain serializes BEFORE any of them; after it lands,
  their briefs re-grep against the new module layout.
- Track 1 (#37 lazy CoW Phase 1) runs concurrently but touches Rust `src/` +
  `tests/fixtures/witness_*` + `tests/integration.rs` — the ONLY shared file is
  `tests/integration.rs` (both add `run_gg` entries; trivial append-merge) and
  TODO/DONE (parent merges). The parent (orchestrator) owns the final merged-tree
  integration sweep.
- Executor: isolated worktree; open with `pwd` + `git rev-parse --show-toplevel`
  verification + `git merge --ff-only gorget-1`; never touch `/workspace/gorget-1`
  or `main`; `git add` explicit file names only; driver-gated tests one at a time;
  the parent runs the FULL integration suite on the integrated tree.
- The scout's worktree (`agent-a7e4f59ccd22bc376`) must NOT be deleted before the
  executor recovers `split-full-diff.patch` (the preserved `a6756e70` worktree is
  the fallback authority, read-only).
- Self-host source style: the moved modules are verbatim text — do not "improve"
  code while moving (the split's reviewability rests on mechanical-move purity).
