# Scout Report — self-host liveness / use-after-move pass

Prototyped end-to-end + MEASURED (scout `agent-affa6eaa32f5f1f6c`, off main `f858154a`, 2026-07-14).
Prototype patch: `scouts/patches/liveness-proto.patch` (`+400 typecheck.gg`, `+3 diagnostic.gg`).
(Persisted by the orchestrator — the scout harness blocks agents writing `.md` report files.)

## VERDICT: FEASIBLE — TRACTABLE — LAND IT. Blast radius = 0.

A minimal-but-complete self-host liveness pass (**+403 lines, ONE gauntlet**) over the ENTIRE
self-host source emits **ZERO diagnostics** (no over-rejection, full 37 MB C emitted) while
correctly rejecting the whole use-after-move / double-move class the self-host previously accepted.
**The self-host source has NO real use-after-moves to fix → the per-module parallelization
contingency does NOT apply (Track P is one gauntlet).**

## The load-bearing measurement — blast radius
`<driver> self_host_lowerer/driver.gg lib --lir-c` over the whole self-host source (~68k lines +
imported std lib): **exit 0, 37,357,898 bytes of C, stderr EMPTY (0 diagnostics)**. Re-confirmed
after the FP fix. `self_host_bootstrap_fixed_point`: 1 passed (661 s) — but see the contamination
caveat; the executor MUST re-run the bootstrap in ISOLATION (blast=0 + additive-only independently
implies FP preservation, but rigor demands a clean run).

**Probe matrix — 12/12 match Rust gg exactly:**
- REJECT (was accept — divergence class closed): `f(!x,!x)` double-move; `f(!x, x.tag)` mover-Copy
  (UAM, ggdef IllFormed); `consume(!a); a.text` move-then-read; **UAM/DM nested inside if/do/match**
  (dm_in_if, intra_branch, branch_move_join, do_block, toplevel_nested — nested detection works).
- ACCEPT: `f(x.tag, !x)` read-before-move; `y=!x; x=fresh(); use x` re-init; `if c: sink(!x) else:
  use x` branch save/restore; recursive-divergence `if c: sink(!r); {…return/return}; use r`.

## Found + fixed en route
1. **A real FALSE POSITIVE (fixed, re-verified):** `live_stmts_diverge` originally checked only the
   LAST statement, so a branch ending in a fully-diverging nested `if`/`match` wasn't seen as
   diverging → over-rejected a valid move-in-a-branch-that-actually-diverges. Fixed with recursive
   `live_stmt_diverges` (SIf: all arms + unconditional else; SMatch: all arms). Purely anti-FP;
   didn't occur in the self-host source (blast stayed 0) but a shippable pass must not have it (Core #8).
2. **A latent Rust-gg C-backend codegen bug (NEW, distinct from the fieldaccess/error_id fix):** a
   DISCARDED `Dict[_,int].remove` result (`Option[int]`) inside a LARGE function miscompiles via the
   coalesce path (`Option[int]` slot ← `int32 0`): `error: incompatible types … __gg_Option__int64_t
   from int32_t`. Small functions unaffected. Worked around by isolating the discarded remove in a
   tiny `live_reinit` helper. **FILE + FIX in Rust gg (owner Q1), or accept the helper idiom.**

## Infra map (verified)
- **Move shapes (two):** `CallArg.ownership == OWN_MOVE(2)` bare value (`ast.gg`, `parser.gg:2059/2079`);
  `EMove(Box[SpannedExpr])` prefix (`parser.gg:2496`). `not`=EUnaryOp, `!=` separate token — no ambiguity.
- **The A2-S/D12 walker `check_carrier_ops_stmt/_expr` (`typecheck.gg:952/1076`) is STATELESS** — the
  pivotal structural fact: liveness is inherently STATEFUL (threaded move-state + branch
  save/restore/merge), so it needs a NEW stateful walk, not a stateless arm.
- Name-keyed root (`place_root_name` `:639`; `self` unresolved, B2 finding); DkVariable filter
  (`place_root_def_spanned` `:712`). `Dict.clone` deep-copies (ergonomic branch save/restore).
  Diagnostics via `ctx.diagnostics.push`; added `DkUseAfterMove` to `diagnostic.gg`. Symlink note:
  edit canonical `self_host_typechecker/`.

## Reference-grounded design (per-rule citations)
Mirrors Rust `src/semantic/safety/origins.rs`: `check_use:25` (read of Moved → UAM), `check_move:468`
(mark; 2nd move → DoubleMove), `merge_branch_states:568` ("moved in either = moved", diverging arms
filtered `:574`). `check_stmt.rs:1021/1134`: if-without-else = `merge([before, after_body])` →
moved-in-any-branch. The walk mirrors `check_carrier_ops_expr` arm-for-arm (exhaustive): name-keyed
`Dict[String,int] moved`; OWN_MOVE args + EMove → move (left-to-right); reads → check_use;
SVarDecl/SAssign(ident) → re-init; SIf/SMatch/EIf/EMatch → clone-per-branch + union; loops →
clone-and-discard; closures → fresh state.

## ggdef acceptance set (the port's spec, fixture-for-fixture)
ggdef models liveness dynamically (`Slot::Moved`→IllFormed, `eval.rs:745`); straight-line coincides
with the static pass. ggdef spec fixtures: `spec/ggdef/src/tests.rs` — `move_then_read_is_illformed:173`,
`d10b_mover_copy_read_is_illformed_not_overlap:1773`, `d10b_order_twin_read_before_move_legal:1799`.
Production negative fixtures to mirror: `double_move_error.gg`, `use_after_move_error.gg`,
`use_after_move_branch_error.gg`, `fstring_use_after_move_error.gg`, `consuming_self_use_after_move_error.gg`,
`borrow_field_use_after_move_error.gg`.

## Size calibration
Production move-tracker fn spans (`origins.rs`): `check_use` 61, `check_move` 55, save 17, restore 15,
`merge_branch_states` 126 — but save/restore/merge are MULTI-AXIS (13 borrow-check axes); the
pure-liveness slice ≈ 120–180 lines. Self-host prototype: **+403 lines** (11 fns, ~60 comment lines) —
in the owner's ~250–400 (A2-S-comparable) range.

## Unified safety module + pass order
Prototype currently runs as a STANDALONE per-function pass AFTER `check_carrier_ops` (drop-purity).
Elegance-showcase target: ONE `check_safety_stmts(stmts, &SafetyState, …)` where drop-purity (A2-S,
stateless) + liveness (stateful) + place-overlap (B2, per-call) are ARMS of one walk. The liveness
walk already mirrors `check_carrier_ops_*` arm-for-arm, so **merging is mostly mechanical** (Track U).
**Pass order (ratified rider): liveness precedes aliasing** — in the unified ECall/EMethodCall arm run
the OWN_MOVE move/double-move check BEFORE B2's place-overlap, so `f(!x,!x)` is a double-move (liveness)
preempting B2's `(Move,Move)` arm (matching production).

## Decomposition / sequencing
- **Track P (land liveness): ONE gauntlet.** The prototype IS the pass. Remaining = the FP fix (done
  in proto), the ggdef/production fixture suite, integration reject/accept tests (mirror
  `self_host_driver_rejects_d12_*`), the codegen-bug decision. Blast radius 0 → no per-module fan-out.
- **Track U (unify into one safety walk): follow-on gauntlet** — mechanical given the shared shape.
- **Follow-on scope (UNDER-detection, never FP; neither blocks landing):** `!self`-consuming method
  receivers + `ConsumeCallable` (production tracks these; closes `consuming_self_use_after_move_error.gg`);
  loop `MoveInLoop` precision (prototype uses clone-and-discard).

## Owner design questions (4)
1. **Codegen bug:** file+fix the discarded-`Dict.remove` `Option[int]` coalesce miscompile in Rust gg,
   or accept the `live_reinit` helper idiom in self-host? (Helper is arguably cleaner style anyway.)
2. **Unify now or follow-on?** Recommend: land liveness STANDALONE first (proto-ready), unify (Track U)
   next — the walks already share structure.
3. **`!self` receiver + ConsumeCallable moves:** this gauntlet or a follow-on? (Needed for full
   production parity + the `consuming_self` fixture.)
4. **Loop precision:** ship clone-and-discard (no-FP, under-detects cross-iteration `MoveInLoop`) or add
   loop-local + same-stmt-rebind tracking?

## Caveats
- **Concurrent-agent /tmp contamination:** the shared gg-build scratch intermittently reverted the
  worktree driver to a stale binary; an early "nested detection fails" scare was 100% this artifact.
  All valid results come from building then IMMEDIATELY copying the binary to a private `/tmp` path and
  testing that. The executor should build to an isolated path (or run when no other agent bootstraps).
- The 661 s bootstrap may have run on a cache-clobbered driver → the executor MUST re-run it in isolation.

## Artifacts
- Prototype patch: `scouts/patches/liveness-proto.patch` (2 files: `+400 typecheck.gg`, `+3 diagnostic.gg`).
- Verify: `gg build self_host_lowerer/driver.gg` → `cp` to a PRIVATE path → `<driver> …/driver.gg lib
  --lir-c 2>err` (blast = bytes of `err`); the probe harness + probes were in `/tmp/live_scout/`.
