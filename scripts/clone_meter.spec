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
# FORMAT: `key = value`, one per line. WHOLE-LINE `#` comments only — no inline
# trailing comment, and no trailing whitespace on a value. Blank lines ignored.
# ⚠ The DATA is single-source, which is the half of Layering rule 3 that
# matters; the READERS are three, and they are not identical. `clone_meter_get`
# (awk, `scripts/clone_meter.sh`), `clone_meter_spec_get`
# (`tests/integration.rs`) and `clone_meter_spec_values` (`tests/lints.rs`) are
# three implementations with no agreement guard — the two Rust ones live in
# separate test binaries that cannot share a module without a helper crate. The
# Rust readers `trim()` a value; awk strips only the leading run, so a trailing
# space would reach the shell's `"$driver"` intact and break the build. Keeping
# the format to the restriction above is what makes the three agree.

# ── THE INVOCATION ─────────────────────────────────────────────────────────
# cwd is ALWAYS the repo root and every path below is relative to it, so both
# instruments run provably the SAME workload. That is all this spelling buys.
#
# ⛔ IT DOES NOT BUY CHECKOUT-INVARIANCE, AND AN EARLIER REVISION OF THIS VERY
# PARAGRAPH CLAIMED IT DID — "a worktree three components deeper than main
# measures the same number". THAT IS FALSE, IT IS THE EXACT BELIEF THAT
# PRODUCED THE 294, and it was written into the file created to retire it.
# A worktree 42 characters deeper measures 294 MORE on the string axis; see
# `root_path_is_an_input` below and `todo/t0850`.
# ⊕ Nor is the relative SPELLING what would make it invariant: the argv
# spelling was MEASURED to move both counters by exactly zero (the table
# below), so relative-vs-absolute makes no difference either way. The spelling
# is declared to pin the WORKLOAD, not to normalise the path.
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

# ⚠ THE ROW THAT IS NOT ZERO — declare it before you compare two readings.
#   the ABSOLUTE PATH of the checkout    string_clone = K + 7 x len(root)
#                                        array_clone  unchanged
# Measured on ONE fixed binary at four roots spanning 6..84 characters, zero
# residual, attributed by `--clones=sites-tsv` to ONE CloneId
# (`self_host_lowerer/loader.gg:32:25`, `VarDeclFromBorrow`, `String` — the
# `path.slice(i, i + 1)` in `parent_dir`'s per-character scan, which runs 7
# times per compile). This is what made the main checkout and an agent worktree
# disagree by 294 on one axis. Filed as `todo/t0850`; the gate PRINTS
# `[clone-meter] root_len=` beside every reading until it is fixed. The effect
# is stage-0 only: both stage-1 pins reproduce to the digit across the same
# 42-character difference.
root_path_is_an_input = yes-on-the-string-axis

# ⚠ AND THE ROWS NOBODY HAS MEASURED, named so the list is a TOTAL and not a
# SELECTION. Each of these is an input to the stage-1 meter by construction and
# has simply never been varied:
#   * the RUNTIME PREAMBLE. The stage-1 binary is `preamble ++ stage1.c` and the
#     counters ride the preamble, so a `src/backend/c/` change moves the stage-1
#     counts with `stage1.c` BYTE-IDENTICAL. (This is why no "cheap signal"
#     based on the stage1.c hash alone is authorised: it names half its input.)
#   * the stage-1 `cc` flags (`-O0 -w … -lm -lpthread`).
#   * `GG_STAGE1_TIMEOUT_SECS` and host load — deadline knobs, believed not to
#     move a count, never checked.
#   * the BACKEND. All four meters are C-only: `--clones=stats` is rejected
#     under `--backend=llvm` (`grep -n "TODO(llvm-clone-stats)" src/main.rs`),
#     so an LLVM-only clone regression rides under every one of them.
#     Filed as `todo/t0550`.
# Do not read this list as "measured to be zero" — only the table above is.

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
