# Brief — GG_IMPL sub-req 2: self-host driver CLI surface (`check`/`--help`/passthrough) + pipeline test

**Track:** GG_IMPL endgame, sub-req 2 (full CLI surface). Scout: `a1a5aa9c` (an AGENT id / worktree
scout — NOT a git object; don't `git show` it; state verified by running, corroborated by the
`d9059114` landing + the source). Small, tractable increment; disjoint files (`tests/fixtures/self_host_lowerer/driver.gg`
+ `tests/integration.rs` + the doc). Does NOT touch the LLVM-panic or parity tracks.

**Executor constraints:** worktree off `gorget-1` (`git merge --ff-only gorget-1`; tip `10d3f33f`+).
`git add` ONLY the files you change. NEVER `git stash` (shared stack — use `cp`).

## Verified current state (don't re-derive)
`driver.gg:run_build_mode` (~:243-350) owns source→C→cc→link via `std.process.exec`; `build`,
`-o`, `--runtime-dir=`/`--lib-dir=`, and `run` (with exit-code propagation) WORK. Missing on the
new subcommand path: **`check`**, **`--help`**, and **`--backend`/`--emit-c`/`--lir-c`/`--emit-gir`
/`--emit-lir` passthrough** (the LEGACY path has the emit flags; the new subcommand dispatch
doesn't). There is NO integration test exercising the `gg-selfhost` pipeline (manual-only today).

## What to implement (mirror the existing legacy flag-parsing in `driver.gg`)
1. **`check` subcommand:** run the parse + resolve + `type_check_module` passes and report
   `has_errors` → exit NONZERO on diagnostics, exit 0 + (optionally) "ok" if clean. Do NOT cc/link.
   ⚠ NOTE: the self-host typechecker is currently PERMISSIVE (it accepts ill-typed programs — a
   filed reference-grade defect, TODO). So `check` will exit 0 for type-mismatch/undefined-ident
   programs TODAY. That's the typechecker's bug, NOT this CLI work — wire `check` to surface
   whatever `has_errors` reports (correct plumbing); it improves automatically when the typechecker
   is fixed. Do NOT try to fix the typechecker here.
2. **`--help`:** print a usage summary (subcommands build/run/check, the flags). Exit 0.
3. **`--backend=<b>` + `--emit-c`/`--lir-c`/`--emit-gir`/`--emit-lir` passthrough** on the
   build/run subcommand path: wire them the way the legacy path does (the legacy path already
   parses these — reuse that logic). For `--backend`, the self-host emits C regardless, so document
   what it actually does (likely accept-and-note, or pass through to the cc step if meaningful).
4. **Update `docs/plans/gg_impl_endgame.md` "Current state"** — it's STALE (still says "NOT
   started"). Set it to the verified state: sub-req 1 LANDED (`d9059114`); sub-req 2 PARTIAL (this
   increment adds check/--help/passthrough); 3 (include_str bundling) + 4 (relocate driver.gg) NOT
   done; 5 (orchestrator `scripts/gg_impl.sh`) mostly done, relocatable-install blocked on 3.

## Guard (REQUIRED — the pipeline is manual-only)
Add an integration test (`tests/integration.rs`) exercising the `gg-selfhost`/self-host-driver
CLI end-to-end — DO NOT rely on bad-program rejection (the typechecker is permissive; that's filed
separately). Test the cases the CLI plumbing genuinely handles:
- `build hello.gg -o <out>` → produces a binary; `<out>` runs + exits 0.
- `run <exitN>.gg` → propagates a NON-zero exit code. **Author a trivial fixture for this**
  (review pass-1, R2) — none exists off-the-shelf (`print_tail_exit_*` exit 0; the only `exit()`
  fixtures are conditional/panic-shaped). Add `tests/fixtures/gg_impl_exit7.gg` = `void main(): exit(7)`
  and assert `run` propagates exit 7.
- `check <valid.gg>` → exits 0.
- `--help` → exits 0 + prints usage.
Find how the existing GG_IMPL exit-code fixtures (`print_tail_exit_*`) + `scripts/gg_impl.sh` are
invoked, and mirror that. Make it deterministic (no network, no GG_RUNTIME_DIR drift — pass the
repo runtime/lib dirs explicitly as the wrapper does).
**(R3) Interim-framing for `check`:** in a code comment at the `check` arm AND the commit message,
cite the filed typechecker defect (TODO.md "the self-host BUILD path silently ACCEPTS ill-typed
programs" + the `gg check` PERMISSIVENESS entry) so the `check`-exits-0-on-bad-programs behavior is
unambiguously interim-pending-a-filed-defect, not silently accepted. Per "don't redesign around
gaps," ALSO add a `#[ignore]`'d test wired to the INTENDED behavior (`check` on an ill-typed program
→ NONZERO exit) as a live executable breadcrumb that flips green when the typechecker is fixed.
**(R4, FYI — don't conflate):** there is a pre-existing `self_host_check/driver.gg` dir powering the
`check_comparison` test (a typecheck-OUTPUT comparison, NOT a `check` subcommand) — your `check` is a
NEW subcommand on `self_host_lowerer/driver.gg`, distinct + non-conflicting.

## Gates (your worktree; parent runs the full both-backend sweep)
- The new integration test passes.
- `cargo test --lib`; `bootstrap_fixed_point` GREEN (driver.gg is self-compiled — the new CLI arms
  must not perturb the fixed point; verify).
- `self_host_runtime` 0 regressed; `lowerer_comparison`/`c_emit_comparison` no regression (driver.gg
  change is in the CLI dispatch, not lowering — should be structurally neutral; confirm).

## Commit (YOUR worktree branch only)
`git add` ONLY: `tests/fixtures/self_host_lowerer/driver.gg`, `tests/integration.rs`,
`docs/plans/gg_impl_endgame.md`. NEVER `git add -a`. Commit message:
```
feat(gg-impl): self-host driver check/--help/emit-flag CLI surface + pipeline test

Add the check subcommand (parse+resolve+typecheck, has_errors->exit code),
--help, and --backend/--emit-* passthrough to driver.gg's run_build_mode
dispatch (sub-req 2). Add the first integration test exercising the
gg-selfhost build/run/check pipeline (was manual-only). check is correctly
permissive today pending the filed self-host-typechecker diagnostic gap.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpfZ7JHtxtsgbL3AwzoAc6
```

REPORT BACK: worktree path + branch + commit hash; `git show --stat`; the driver.gg CLI-dispatch hunk; the new test + its results; bootstrap_fixed_point + lib + comparison results. If you hit a gap you can't resolve cleanly, STOP and report.
