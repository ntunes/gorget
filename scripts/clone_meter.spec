# ═══════════════════════════════════════════════════════════════════════════
# THE CLONE METER — ONE DECLARED WORKLOAD, ONE SOURCE OF TRUTH
# ═══════════════════════════════════════════════════════════════════════════
#
# WHY THIS FILE EXISTS. Four ratchet constants in `tests/integration.rs` pin a
# clone count taken from ONE workload. Until R47 that workload was spelled out
# THREE times — once in the Rust gate (`fn build_instrumented_clone_driver` /
# `fn self_host_clone_ceiling`), once in `scripts/self_host_mem_baseline.sh`,
# once in `scripts/bench_stages.sh` — and the three spellings had drifted apart
# on four axes (gg build profile, driver argv spelling, cwd, stdout target).
# Two of those instruments then reported DIFFERENT numbers for the same meter
# on the same tree, and nobody could say which was right. That is Layering
# rule 3 (one source of truth per axis) violated at the harness boundary.
#
# ⇒ EVERY instrument that reads this meter builds its invocation FROM THIS
#   FILE. `tests/lints.rs::clone_meter_instruments_read_the_declared_spec`
#   fails when one of them spells the invocation itself instead.
#
# FORMAT: `key = value`, one per line; `#` comments; blank lines ignored.
# Bash reads it with `scripts/clone_meter.sh`; Rust reads it with
# `fn clone_meter_spec()` in `tests/integration.rs`.

# ── THE INVOCATION ─────────────────────────────────────────────────────────
# cwd is ALWAYS the repo root; every path below is relative to it, so the meter
# is invariant under where the checkout lives (a worktree three components
# deeper than main measures the same number).
driver     = tests/fixtures/self_host_lowerer/driver.gg
lib        = lib
build_args = --clones=stats
run_args   = --lir-c
stdout     = discard
counters   = array_clone string_clone

# ── THE INPUTS THAT ARE *NOT* INPUTS, ESTABLISHED BY MEASUREMENT ────────────
# Each row below was suspected of moving the meter and was MEASURED to move it
# by exactly zero. They are declared here so the next reader does not re-derive
# them, and so a future disagreement has a list to check against.
# Regenerate the whole table: `bash scripts/clone_meter_probe.sh`
#
#   axis                       varied over                          delta
#   gg build profile           debug gg vs release gg               0  (the
#                              emitted driver C is byte-identical:
#                              md5 ef7e1a7c7ef2b8a05c74280c98f301a5)
#   driver argv spelling       repo-relative vs absolute            0
#   lib argv spelling          repo-relative vs absolute            0
#   stdout target              /dev/null vs a file                  0
#   build-time argv spelling   relative vs absolute at `gg build`   0  (same
#                              emitted C md5)
#
# ⚠ `total_allocs` is NOT in this set: it jitters (68,260,765 vs 68,260,767 on
#   two runs of the same binary). Only the two `counters` above are pinned, and
#   only they have been shown stable. Do not ratchet a counter that has not
#   been shown to be deterministic.
profile_is_an_input = no

# ── THE WORKLOAD'S TRUE CLOSURE ────────────────────────────────────────────
# Measured, not globbed: the set of files the meter's run actually OPENS.
#   bash scripts/clone_meter_probe.sh --closure
# (strace -f -e trace=openat over the driver's own run, canonicalised through
# realpath and deduplicated — so a file reached through a SYMLINK is recorded
# at its real path, which is the whole point: see the seam below.)
#
# ⚠ THE SEAM. 15 of the 38 `.gg` files in `tests/fixtures/self_host_lowerer/`
# are SYMLINKS into `tests/fixtures/self_host_typechecker/`. A change to one of
# those 14 live targets moves this meter while `git diff -- tests/fixtures/
# self_host_lowerer/` shows NOTHING. That is why the closure is declared by
# REAL path and why the symlink manifest below is pinned: a change to the seam
# shows up as a diff in THIS file.
# ⚠ THE CLOSURE HAS TWO HALVES, AND ONLY ONE OF THEM IS TRACEABLE.
#   * the WORKLOAD half — the files the meter's run OPENS. Measured (below).
#   * the PRODUCER half — Rust `gg` itself, which BUILDS the driver being
#     measured. It opens none of these files at run time, but a change to
#     `src/` changes the C the driver is built from and therefore its counts.
#     No trace can find it; it is declared.
# `closure_roots` is the UNION, because the question every consumer asks is
# "can this diff move the meter", and both halves answer yes.
workload_closure_roots = tests/fixtures/self_host_lowerer tests/fixtures/self_host_typechecker lib/std compiler/data src/backend/c/runtime
producer_closure_roots = src
closure_roots     = tests/fixtures/self_host_lowerer tests/fixtures/self_host_typechecker lib/std compiler/data src
closure_gg_files  = 66
closure_gg_lines  = 81509
closure_all_files = 132

# WHICH STAGES A TOUCH OBLIGES. Deliberately the SAME set: any closure touch
# owes BOTH meters. The tempting narrowing — "a Rust-frontend-only change
# cannot move stage-1, because stage1.c is emitted by the self-host" — is
# UNPROVEN, and stage-1 is the only meter that can see a self-host-lowering-only
# clone bomb (the 2026-07-19 blowout: 7x, invisible to stage-0). A narrowing
# that is wrong once costs exactly what it was meant to save. Narrow it only
# with a measured pair behind it.
stage1_closure_roots = tests/fixtures/self_host_lowerer tests/fixtures/self_host_typechecker lib/std compiler/data src

# The `.gg` half of the closure, by directory (66 files, 81,509 lines):
#   tests/fixtures/self_host_lowerer      22
#   tests/fixtures/self_host_typechecker  14   ← reached ONLY through the seam
#   lib/std                               28
#   compiler/data                          2
# The non-`.gg` half is 63 files under `src/backend/c/runtime/` — the C runtime
# sources the driver reads to emit its preamble. A `src/backend/c/` change can
# therefore move BOTH stage meters (at stage 1 the counters ride that preamble,
# so it moves them with `stage1.c` byte-identical).
#
# ⚠ TWO `self_host_lowerer/*.gg` FILES ARE IN THE DIRECTORY BUT NOT IN THE
# CLOSURE — `format.gg` (a symlink) and `reachability.gg`. The driver never
# opens them. 22 real + 14 linked = 36 = 38 − 2.

# ── THE SYMLINK MANIFEST — the seam, pinned ────────────────────────────────
# `tests/lints.rs::clone_meter_closure_declares_the_symlink_seam` fails when the
# tree and this list disagree, in either direction.
symlink = ast.gg -> ../self_host_typechecker/ast.gg
symlink = derive.gg -> ../self_host_typechecker/derive.gg
symlink = diagnostic.gg -> ../self_host_typechecker/diagnostic.gg
symlink = format.gg -> ../self_host_typechecker/format.gg
symlink = format_types.gg -> ../self_host_typechecker/format_types.gg
symlink = ids.gg -> ../self_host_typechecker/ids.gg
symlink = infer.gg -> ../self_host_typechecker/infer.gg
symlink = lexer.gg -> ../self_host_typechecker/lexer.gg
symlink = meta.gg -> ../self_host_typechecker/meta.gg
symlink = parser.gg -> ../self_host_typechecker/parser.gg
symlink = resolve.gg -> ../self_host_typechecker/resolve.gg
symlink = scope.gg -> ../self_host_typechecker/scope.gg
symlink = traits.gg -> ../self_host_typechecker/traits.gg
symlink = typecheck.gg -> ../self_host_typechecker/typecheck.gg
symlink = types.gg -> ../self_host_typechecker/types.gg
