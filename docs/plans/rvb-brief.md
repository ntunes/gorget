# EXECUTOR BRIEF — RV-B: DotShorthand enum-init consume position (both lanes)

**Status:** ✅ v3 GAUNTLET-CLEAN (pass 3 SIGN OFF 2026-07-17 — every premise re-derived
independently, both lower.gg sites verified in source AND proto, symlink topology fully
verified; its one non-blocking hardening note folded in place per explicit reviewer
clearance) — **EXECUTOR PENDING the Root-A landing (the sequencing gate below).**
Gauntlet history: DRAFT v3 — Pass 1 (all premises confirmed
by reproduction incl. the compile-error safety property; extraction byte-faithful;
is_constructor sound; 12-site set complete; 85/86 baselines fresh; 4 reservations folded:
+2 matrix fixtures, TODO:264 narrowing, `#[ignore]` intended-output, sequencing wording) →
v2. Pass 2 (independent re-derivation clean on all premises; 2 fold-defects: the two
pass-1-added fixture rows were never AUTHORED and the header claimed false provenance —
now marked EXECUTOR-AUTHORED with expected-output specs and a VERIFY-FIRST self-host UAM
instruction [the rejects_d12 grep would never match a UAM message]; the arm enumeration
omitted the REAL `lower.gg:1310` hunk while implying the no-op was lower.gg's only arm —
corrected) FOLDED into this v3. Do not execute until a clean pass.
**⚠ SEQUENCING GATE:** the self-host half changes the `EDotShorthand` AST shape
(`Vector[SpannedExpr]` → `Vector[CallArg]`), which requires **6 mechanical hunks across 7
lowerer case-arms**. The SIX hunked arms (pass-2-corrected enumeration): `lower_expr.gg`,
`lower.gg:~1310` (`mutinf_scan_expr` — a REAL hunk, `.value` access), `lower_closures.gg`
×3, `lower_generics.gg`. The SEVENTH arm, `lower.gg:~596` (the node-name debug helper),
binds `_,_` and is a DELIBERATE no-op — it alone will correctly NOT produce a compile
error during the error-chase; every other lowerer arm WILL. The Root-A iterator-receiver
track is landing in those files first. **This executor launches only after Root-A has landed
on gorget-1 (or its track is killed), and REBASES the prototype on that state.** (NOTE the
handover queue lists RV-B before Root-A — that is PRIORITY order, not landing order; this
gate governs landing order and the pass-1 review verified the rationale.) If the patch no
longer applies after Root-A, re-derive the mechanical arms by compiler-error-chasing —
pass 1 VERIFIED the safety property: `CallArg` and `SpannedExpr` are distinct nominal types,
so every stale value-binding site fails to compile; no silent-pass risk — and
STOP-AND-REPORT only if a site is non-mechanical.

**Scout evidence (THE measured spec):** `docs/plans/rvb-scout.md` — GO, both lanes
prototyped + measured. Prototype: `/tmp/rvb_proto.patch` (465 lines; live-diff variant
`/tmp/recover_rvb_proto_live.patch` — the delta is the untracked fixture files, which live at
`/tmp/rvb_fixtures/` + `/tmp/recover_rvb_fixtures/`). Verified in the scout's worktree:
lib 1107/0 · `dot_shorthand d12_` 20/0 · self-host driver suite 26/0 (both new driver tests
RED pre-fix) · bootstrap fixed-point ok 592s · zero corpus blast radius (every existing
dot-shorthand-with-args uses a literal).

## Mission

Make dot-shorthand enum-init EXACTLY equivalent to the longhand ctor at every ownership
check, on both lanes (Core #4 — fix the class at one shared site):

- **Rust:** extract the Call arm's per-arg ownership loop into a shared
  `check_call_arg_ownership(args, is_constructor)`; `Expr::Call` routes through it;
  `Expr::DotShorthand` calls `check_call_aliasing(args)` + the shared fn with
  `is_constructor=true` (dot-shorthand always resolves to an enum-variant ctor,
  `typecheck.rs:~3681`). This closes: bare drop-tainted `.Wrap(r)` wrongly ACCEPTED (the
  drop side-effect then runs twice — the HIGH miscompile); `.Wrap(!r); use r` wrongly
  accepted (should be `E_UseAfterMove`).
- **Self-host (Option A — reference-grade):** `EDotShorthand(String, Vector[CallArg])` —
  completing the CallArg normalization ECall/EMethodCall already have (the lone holdout
  node). The safety arm becomes a mirror of the ECall ctor arm (gate on `a.ownership`; add
  `check_call_aliasing`). This closes the over-reject of legal `.Wrap(!r)` AND the two
  documented latent misfires from the flip-tracks landing (the drop-taint one and the
  single-owner one) — retire their in-code notes + the LOW TODO (~:873) with it.
- The 12-site update set from the scout: `ast.gg`, `parser.gg`, `typecheck.gg` ×2,
  `resolve.gg`, `format.gg` + the 7 lowerer arms. The mini-drivers
  (`self_host_parser`/`self_host_resolver`) keep their own `ast.gg` UNTOUCHED.

## Fixtures (into `tests/fixtures/d12_drop_purity/`. PROVENANCE: rows 1/2/5 exist at
`/tmp/rvb_fixtures/` and are scout-verified both lanes; **rows 3/4 are EXECUTOR-AUTHORED**
— added by review pass 1, they exist nowhere yet)

| Fixture | Wiring | Expected |
|---|---|---|
| `dotshorthand_tainted_bare_reject.gg` | `check_gg_fails(…, E_MoveWithoutOperator)` + add to `self_host_driver_rejects_d12_drop_purity` | reject, both compilers, same code |
| `dotshorthand_move_ok.gg` | `run_gg(…, "built\ndrop 1")` + add to `self_host_driver_accepts_d12_legal` | single drop; ASan clean (pins the double-drop) |
| `dotshorthand_move_then_use_reject.gg` (EXECUTOR-AUTHORED, pass-1 R1) | Rust: `check_gg_fails(…, "error[E_UseAfterMove]")` — verify the exact code the longhand emits FIRST and use that. Self-host: a DEDICATED reject test — do NOT add to `rejects_d12_drop_purity` (its assertion greps `"cannot copy"`; a self-host use-after-move emits `"use of \`r\` after it was moved"`, `typecheck.gg:~1342` DkUseAfterMove). **VERIFY-FIRST:** the self-host behavior on `.Wrap(!r); use r` post-fix is UNMEASURED (the scout's 5 self-host cases didn't include it) — measure it; if the self-host does not yet reject this shape, scope the fixture's self-host half to a REPORT + filed gap, never a silently-weakened assertion | `.Wrap(!r); use r` rejects — pins the SECOND Rust bug (the ignored `!` → missed move); nothing else guards it |
| `dotshorthand_bare_value_ok.gg` (EXECUTOR-AUTHORED, pass-1 R1) | `run_gg` POS: enum `E: Wrap(String)`, `String s = "hi"`, `E e = .Wrap(s)`, then `print(s)` (source still live — CoW clones) and a match printing the payload; expected stdout `hi` then `hi` (derive precisely from the CLAUDE.md ownership table when authoring; both backends). **Also wire into `self_host_driver_accepts_d12_legal`** (pass-3 hardening note: the scout measured the self-host accepts this; committing it guards the no-over-reject else-branch) | legal bare non-resource `.Wrap(s)` ACCEPTS and runs — pins the "dot-shorthand == longhand" property against future over-tightening |
| `dotshorthand_callable_move_ok.gg` | `check_gg_ok` (both lanes accept) **+ a `run_gg(…, "built")` runtime test wired `#[ignore]` citing the filed callable-enum-payload lowering-panic TODO** — the ignored test asserts the INTENDED runtime behavior per don't-redesign-around-gaps; un-ignore when that gap lands | check-accept all lanes; intended runtime pinned |

ggdef lane: value-position dot-shorthand is OUT of the ggdef subset (`expr_kind` catch-all;
ggdef handles DotShorthand PATTERNS only) — no ggdef edit; add the explicit subset-gap note
to the RFC/plan location the corpus EXCLUDE convention uses (report where you put it).
Per Core #9 this is accept/reject-changing → the NEG fixture pins the Rust flip and the
self-host driver tests pin that lane; the ggdef note covers the third.

## Gates (FOREGROUND; self-host commands `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`;
chunk >10min; NEVER background a final gate)

1. `cargo build` + `cargo test --lib`.
2. Rust: the `dot_shorthand`/`d12_` integration filter (scout: 20 tests) on C AND LLVM; the
   NEG fixture rejects on both; the POS fixture runs `built\ndrop 1` on both + ASan.
3. Self-host: rebuild the driver; the driver suite incl. both new tests; the three fixtures
   through the driver (NEG exit=1 with the message; POS emits C).
4. `self_host_bootstrap_fixed_point` (AST-shape change is on the bootstrap path; budget
   900s, chunked foreground).
5. Targeted `type_comparison`/`check_comparison` (typecheck.gg changed): counts must equal
   the documented 85/86 baselines (any delta is a STOP-AND-REPORT with the fixture list).
6. Blast radius re-confirm on the post-Root-A base: re-grep for dot-shorthand-with-args
   corpus programs; run any new at-risk ones.

## Bookkeeping (same commit)
- TODO: RV-B entry (~:243) retires to DONE (datestamped, with the double-drop precision
  wording); the LOW EDotShorthand-misfire entry (~:873) retires with a pointer to the
  callable fixture; the flip-track in-code misfire notes at the typecheck.gg arms are
  updated to "resolved by RV-B" or removed (one coherent story per arm — no stale notes);
  **the MEDIUM "Extend CallArg to EStructLiteral/EDotShorthand" entry (~:264) is NARROWED to
  EStructLiteral-only** (its "these two nodes still keep bare Vector[SpannedExpr]" and
  "unconditional reject" claims become false for EDotShorthand after this landing — leaving
  them is the false-historical-record class the breadcrumb check exists to catch).
- The two ORTHOGONAL gaps the scout found stay filed (do NOT touch them).
- Stage EXPLICITLY by file name (the 12 self-host sites + `src/semantic/safety/check_expr.rs`
  + fixtures + `tests/integration.rs` wiring + TODO/DONE — adjust to actual). Trailers:

      Co-Authored-By: Claude Opus <noreply@anthropic.com>
      Claude-Session: https://claude.ai/code/session_01TYkkHveF8WhhTVX4DjbCTN

- Checkpoint `/tmp/rvb_exec_progress.md` after every gate. STOP-AND-REPORT on any conflict,
  gate failure, or surprise. Final message: commit hash + branch, the six-case ownership
  matrix (bare-tainted/`!`-move/`!`+use/String-bare/clone/callable × both lanes), every gate
  count verbatim, the staged list, smells.
