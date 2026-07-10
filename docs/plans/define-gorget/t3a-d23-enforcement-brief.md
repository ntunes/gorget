# T3a — D23 throws-totality ENFORCEMENT (executor brief)

> **Wave position:** the D23 enforcement track, DISJOINT from the D11 trap tracks (T1/T2)
> — different files (`src/semantic/*` + `tests/*` vs `spec/ggdef/*`). Runs in parallel with them.
> The ONE shared file is `docs/language-reference.md` (this brief edits §10.1; T1 edits §10.9 —
> different sections). To avoid any merge coordination, **this executor launches AFTER T1 is
> integrated**, so it branches off a main that already has T1's §10.9 edit.
>
> **Grounded in:** `docs/plans/define-gorget/scouts/scout-d23-throws-totality.md` (the measured
> scout — READ §3 for the exact code sketches this brief references) and `decisions.md` (the D23
> LOG entry 2026-07-07 + the gorget-js dogfood findings + the A33 rider).

## What D23 ratifies (the target — do NOT relitigate)
Normative: **"a `throws` call is an expression of type T in EVERY position; its Result-ness is
unobservable except at a Result-typed binding or a `catch`."** Diagnostic contract: a user-facing
unhandled-throws message must NEVER surface the desugar as the found type (never `found \`Result[`);
it says *"this call throws E; declare `throws E` or handle it (catch / rethrow / Result capture)."*
NO semantic change — the virality is pre-existing; this pins coverage-totality + UX. A33 rider
(faults enter the error/value world only via explicit conversion points) is NOT edited here
(it lives in §10.9, T1's zone).

## The problem, as MEASURED (scout §1-§2) — THREE failure modes, not one
An unhandled `throws` call today produces one of:
1. **LEAK `Result[`** — binary operand, match-arm tail, plain bind, fn arg, `return`, expr-body
   tail: a bare `E_TypeMismatch` from the generic `unify` whose rendered message contains
   `found \`Result[int, String]\`` (leaks the desugar). `risky() + 1` leaks TWICE.
2. **SILENT SWALLOW** — match scrutinee with non-Result arms, and bare-statement discard: NO
   diagnostic, the error is silently dropped.
3. **SILENT MISCOMPILE (the worst — invariant #8)** — ANY `throws` METHOD call in an unhandled
   position: `s.risky()` types as bare `int` (throws-ness dropped by `build_function_sig`,
   `traits.rs:1683`), so `int x = 1 + s.risky()` PASSES `gg check` and produces GARBAGE at runtime
   (measured `x=281474674991985`). This is a silent-miscompile from safe code — the reason the
   prior "intercept `E_TypeMismatch`" plan was WRONG (it can't even see this case).

**Root cause (one site):** the throws-fn producer-peel at `typecheck.rs:1973-2038`. In a
non-propagating context the `else` (line ~2035) returns `raw_result` = `Result[T,E]`, which then
leaks on `unify`. The fix is at this PRODUCER (the write site), shared by the free-fn AND method
paths — NOT at the `unify` read site.

## Rulings on the scout's 7 design questions (decided; execute these)
- **Q1 — the method miscompile is IN SCOPE (mandatory).** Same layer, same `throws_type_id`
  metadata, worst mode. Fixing free-fns only = fixing the instance not the class (and shipping a
  known silent-miscompile = an invariant-#8 violation). Do NOT split it out.
- **Q2 — message per §3a; ratchet scoped to the LEAK substring.** The message's capture suggestion
  MAY name `Result[T, E]` (teaching prose); the ratchet bans only the desugar-as-found-type leak
  `found \`Result[`. (Recommendation (b).)
- **Q3 — REPLACE, don't supplement.** Emit `E_UnhandledThrows` at the producer and return
  `error_id` so downstream `unify` sees `error_id` and stays silent — collapses the 1-2-error
  cascades to ONE clean diagnostic.
- **Q4 — ONE diagnostic (`E_UnhandledThrows`) for all three modes.** The producer emit unifies
  leak/swallow/garbage by construction. (A general must-use-on-`Result`-even-inside-a-throws-fn is
  OUT of D23 scope — note as a follow-up in your report, do NOT expand T3.)
- **Q5 — the expr-body widening is IN T3a.** The D23 LOG explicitly names the expr-body asymmetry;
  ~8 lines, same "route through the return path" theme; landing it separately would leave §5.1's
  stated equivalence violated between two commits.
- **Q6 — behavioral negative fixtures via `check_gg_fails_no_desugar`** (§3b).
- **Q7 — the smith throws tier is a SEPARATE follow-on brief (T3b), not this one.** T3a is the
  diagnostic + method + expr-body + fixtures + reference. (T3b ships regardless of ggdef; flagged
  separately.)

## Work items (each references the scout's §3 code sketch — read it)

### W1 — the `E_UnhandledThrows` diagnostic (scout §3a)
- New variant in `src/semantic/errors.rs`: `UnhandledThrows { throws_type: String }`. Add its
  `code()` arm → `"E_UnhandledThrows"` (the exhaustive no-catch-all match at `errors.rs:641` is the
  ratchet — a missing arm is a build error). `Display` per §3a: *"this call throws
  `{throws_type}` but the error is not handled here; declare the enclosing function `throws
  {throws_type}`, or handle it with `catch`, `rethrow`, or by binding the result to a `Result[T,
  {throws_type}]`"*. `{throws_type}` = `describe_resolved_type(err_ty)` (the callee's `E`, in hand
  at the peel). NOTE the message's `Result[T,` token is in the SUGGESTION (allowed per Q2), not the
  leaked found-type.

### W2 — the shared producer helper + BOTH call sites (scout §3a — the core fix)
- Extract a shared helper `fn resolve_throws_call_type(&mut self, return_type, err_ty, suppress,
  span) -> TypeId` implementing the split `else` (§3a): `suppress || dest_is_result` → `raw_result`
  (legit whole-Result positions: capture/scrutinee/catch/rethrow — UNCHANGED); else if
  `current_fn_can_propagate()` → the Route-A peel (unchanged) → `return_type`; else → emit
  `E_UnhandledThrows` and return `self.types.error_id` (collapses the cascade via unify's error_id
  short-circuit at ~:855). **When lifting the existing 2011-2040 block into the helper, PRESERVE
  its `match self.scopes.lookup("Result") { … None => return_type }` fallback** (the `raw_result`
  interning at ~:2013-2016) — the scout's §3a sketch drops the `None` arm; keep it so a
  "Result"-out-of-scope build doesn't regress.
- **Free-fn site** `typecheck.rs:2011-2038`: route through the helper.
- **Method site (REQUIRED — this is the silent-miscompile fix)** `typecheck.rs:2419-2523`: read
  `self.function_info.get(&stored_def_id).and_then(|fi| fi.throws_type_id)`. Methods carry
  `throws_type_id` in `function_info` (populated at `resolve.rs:745`); the arm's PRIMARY return
  (~:2422) currently returns bare `sig.return_type` — that is the measured silent miscompile.
  (Note: the `function_info.get(&stored_def_id)` read that already exists at ~:2383 is for
  `param_names`/`param_defaults`, NOT the throws field — the map + `stored_def_id` are in scope, but
  the throws field is not read today.) When `Some(err_ty)`, return
  `resolve_throws_call_type(sig.return_type, err_ty, suppress_auto_prop, span)`; else `sig.return_type`.
- **The method arm has MULTIPLE throws-carrying return points** — the PRIMARY `stored_def_id` path
  (~:2422, the one the measured `s.risky()` miscompile flows through), PLUS the FALLBACK return
  sites: the trait-default path (~:2465) and the cross-module-equip path (~:2523, where the def_id
  is currently discarded as `_def_id` — you must un-discard it).
- **CAVEAT — the "primary" path is only safe for CONCRETE-method def_ids.** When `resolve_method`
  (~:2362-2367) returns a substituted trait-DEFAULT, `stored_def_id` is itself a TRAIT def_id, so
  `function_info.get(&stored_def_id)` yields None at ~:2422 too and you'd fall back to bare
  `sig.return_type` — the same hole, on the primary path. Do NOT conclude "primary path done" after
  the concrete-method case works. Your "throws method via trait-default" fixture (below) exercises
  this regardless of which internal resolution path fires — that's the real gate. **It is NOT established that `function_info` carries `throws_type_id` keyed by the
  fallback def_ids.** So: apply the helper at EACH throws-carrying return; for the two fallback
  paths, write a fixture that routes a `throws` method through a trait-default and a cross-module
  `equip` and CONFIRM it now rejects. If `throws_type_id` is genuinely unreachable at a fallback
  def_id, DOCUMENT why that path cannot carry a throws method (in your report + a code comment) —
  do NOT leave a silent hole. Add the **arm-count lint** (W4) forcing new method-return sites
  through the helper (fix the class, not the instance).
- **Auto-prop-gate interaction (confirm green, not a regression):** routing the method path through
  the helper means `auto_prop_error_gate` (~:5416) now fires for throws METHODS in PROPAGATING
  contexts (previously they auto-propagated purely via lowering's desugar). This is CORRECT gating:
  same-`E` is a true no-op (byte-identical C), different-`E` records a `From` or emits
  `UnconvertibleErrorPropagation` — both right. Confirm the existing `throws*` method-auto-prop
  fixtures stay green (the gate battery's `throws` set + bootstrap canary cover this).
- **Verify §10.3 capture is unbroken:** `dest_is_result` (reads `decl_type_hint`, ~:2017-2019) and
  the `suppress_auto_prop` (match-scrutinee-with-Result-arms, catch/rethrow inner) short-circuits
  fire BEFORE the emit, so `Result[T,E] r = f()` and legitimate captures are never flagged. Test
  this explicitly (a positive fixture that MUST still compile).

### W3 — the expr-body widening (scout §3d)
- Replace `typecheck.rs:6999-7001` (`FunctionBody::Expression`, currently no hint + unconditional
  unify) with the `Stmt::Return` logic (`typecheck.rs:3893-3927`): set `decl_type_hint =
  Some(return_type)` around `infer_expr`, then skip unify when
  `is_collection_assignment` / `auto_prop_skips_unify` / `is_result_capture_compatible`. **Do this
  as a shared `check_return_value(return_type, expr, span)` helper called by BOTH the expr-body arm
  AND `Stmt::Return`** (kills the sibling-drift class). This is a WIDENING (expr-body `throws` tails
  now capture/peel like block-body); it composes with W2's peel. Keep the existing noreturn check.

### W4 — the ratchet + negative fixtures (scout §3b)
- Add `fn check_gg_fails_no_desugar(fixture: &str, expect: &str)` to `tests/integration.rs` (near
  `check_gg_fails` at ~:7049): assert (i) `gg check` FAILS, (ii) stderr contains `expect` (use
  `"throws"`), (iii) stderr does NOT contain `found \`Result[`.
- Add negative fixtures `tests/fixtures/d23_unhandled_{binop,arg,bind,scrutinee,statement,matcharm,method}.gg`
  — one per position. The `scrutinee`/`statement`/`method` fixtures are LOAD-BEARING: they assert
  the swallow/garbage now FAILS (was accepted/miscompiled) — the invariant-#8 gate made executable.
- Add the **arm-count lint** to `tests/lints.rs` pinning the method-arm throws-return sites through
  `resolve_throws_call_type` (so the next method-return site can't silently reintroduce the hole).
  Model it on an existing arm-count lint (e.g. `container_literal_arms_count`).
- Add positive fixtures that MUST still compile (proving no over-rejection): a legitimate §10.3
  `Result[T,E] r = f()` free-fn capture + a `throws`-fn that auto-propagates, AND — because the
  method path is the newly-wired code — an explicit **METHOD-capture** positive fixture
  `Result[int,String] r = s.risky()` (throws method captured into a Result binding must still
  compile). The method-capture fixture is the direct guard against method-path over-rejection.

### W5 — reference §10.1 sentence (scout §3c)
- Insert after `docs/language-reference.md:2416` (the auto-propagate paragraph) the totality
  paragraph from §3c (the "expression of type T in every position … `E_UnhandledThrows` … never a
  silently-typed `Result`" text). Do NOT edit §10.9 (T1's zone) or §10.3's existing capture text
  beyond a cross-reference.

## Gate battery (run FOREGROUND, generous timeouts; PASTE actual output)
```
cargo build
cargo test --lib 2>&1 | tee /tmp/t3a_lib_$$.log
cargo test --test lints 2>&1 | tee /tmp/t3a_lints_$$.log
# targeted: the new negative + positive fixtures, and the throws regression set
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration d23_ -- --nocapture 2>&1 | tee /tmp/t3a_d23_$$.log
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration throws -- --nocapture 2>&1 | tee /tmp/t3a_throws_$$.log
# the bootstrap canary — a throws-typing change can shift self-host lowering
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration self_host_bootstrap_fixed_point -- --nocapture 2>&1 | tee /tmp/t3a_boot_$$.log
```
Acceptance: builds; `--lib` + lints green (incl. your new arm-count lint); the 7 negative
fixtures FAIL `gg check` with a `throws` message and NO `found \`Result[` leak; the positive
capture/auto-prop fixture(s) still COMPILE; the existing `throws*` integration fixtures stay green
(this is a WIDENING + reject-more; a fixture that previously relied on the silent-swallow/garbage
would newly fail — if so, that fixture was locking in a bug: report it, do NOT weaken the fix);
bootstrap fixed-point green. **Do NOT run the full `cargo test --test integration`** — that's the
parent's job; run the targeted sets above.

## Scope fences (do NOT cross)
- Touch ONLY: `src/semantic/{errors,typecheck}.rs` (+ `traits.rs`/`resolve.rs` ONLY if the method
  `throws_type_id` read genuinely needs it — the scout says it's already in `function_info`),
  `tests/integration.rs`, `tests/fixtures/d23_*.gg`, `tests/lints.rs`, `docs/language-reference.md`
  (§10.1 ONLY).
- Do NOT touch `spec/ggdef/*`, `spectests/*`, `spec/prose/*`, reference §10.9 (all T1/D11). Do NOT
  touch `tests/smith/*` (that's T3b). Do NOT change the throws SEMANTICS (virality is pre-existing;
  you are pinning coverage + UX, not redesigning).

## Worktree & agent discipline (NON-NEGOTIABLE)
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm BOTH inside your worktree (under
`/workspace/gorget/.claude/worktrees/`). If either is `/workspace/gorget` or `/workspace/gorget-1`,
STOP. NEVER touch main/gorget-1 directly; no `cd` into either; paths RELATIVE to your worktree; on
Edit desync re-Read + retry the Edit tool (NEVER a heredoc with an absolute path); after any
non-Edit write run `git -C /workspace/gorget status` and STOP if it shows changes. Entry: `git
merge --ff-only gorget-1 2>/dev/null || true`. **Checkpoint to `/tmp/t3a_report_$$.md` after each
work item.** Stage ONLY the exact files by name (NEVER `git add -a`/`.`/`commit -a`); NEVER `git
stash`. Commit on your worktree branch with the two trailers:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01YWwxrY4NAvQ5uv43X4VjHL
```

## Deliverable
A report (checkpointed to /tmp): per-work-item files+file:line and one-line what; the PASTED gate
output (esp. the 7 negative fixtures' failure lines proving no `Result[` leak, and the positive
fixture still compiling); confirmation the method miscompile now REJECTS (show the before/after of
`int x = 1 + s.risky()`); any brief premise that differed from reality (corrected); your branch +
commit hash; and a note on any existing fixture that newly failed (with your judgment on whether it
was locking in a bug).
