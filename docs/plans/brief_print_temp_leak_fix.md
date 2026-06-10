# BRIEF — Chain D: print-temp String leak family (one class, five producer sites)

Status: v2 (pass-1 review folded 2026-06-10: the sweep LIST embedded — the
scout worktree is gone and the list existed nowhere durable [R1]; all 11
probe shapes enumerated [R2]; gate counts replaced by filter COMMANDS per the
no-un-regenerated-numbers rule [R3]; the site-C ungated sub-shape TODO added
to the Work-item-3 list [R4]. Pass 1 independently re-proved: diff applies
clean, all named leaks exact pre-fix → ZERO post-fix, stdout byte-identical
×14, the site-C no-double-free argument re-derived from source (move_zero +
mark_moved = exactly one free; double-USE rejected by the validator),
site-B's single pusher/drain straight-line, the 5-site enumeration complete
('b' is the only String-producing format arm), fixed_point GREEN 494.91s,
LLVM spot-checks ASan-clean, RSS direction 10MB→1MB on the print loop. v1
was the orchestrator draft from scout `agent-ab272cfd08b23d4d4` at
`6894cb6a`; the RUN-PROVEN prototype is committed at
`docs/plans/print_temp_leak_fix.diff` — 65 insertions, 3 files.)

## Mission

Land the scout's run-proven class fix: a String temp freshly materialized to
feed a printf/format consumer is never drop-registered at its birth
(CLAUDE.md invariant #3). FIVE producer sites, ONE doctrine: register at the
producer. Measured results to reproduce: the 5 named leaks → 0; 15 of 22
leaky string-heavy fixtures fully CLEAN (+4 improved); stdout byte-identical
across the 28-fixture sweep + 11 probes; `fixed_point` GREEN (512s);
print-heavy-loop wall-clock identical with peak RSS 29,344 KB → 9,440 KB.

## The five sites (the committed diff is the authority; re-derive line nums)

- **A** `format_for_printf` Ptr(String) branch (`src/ir/lowering/exprs/
  calls.rs:1904-1907`): the `copy_cow` materialized copy → one-line
  `ctx.drops.register_local(...)` (registration is unconditionally safe:
  owned copies free; cap=0 view copies no-op free). Covers leaks 1/3/4/5.
- **C** `lower_interp_segment` branches 2 & 3 (`calls.rs:1829/:1857`):
  register the interp tmp — **GATED to owning modes only (Move, or
  Clone-of-String)**: `AssignMode::Clone` on a NON-string aggregate is a
  shallow memcpy today → registering would double-free (scout-found; gate is
  in the diff with a comment). The ungated sub-shape → TODO.
- **D** `format_for_printf` Displayable branch (`calls.rs:1959`): register
  the `Type__display` result.
- **E** `apply_format_spec` `'b'` arm (`calls.rs:2126`): register the
  `gorget_int_to_binary` result.
- **B** the LIR-layer bool temp (`src/lir/lower/insts.rs:4251-4297`
  `__bool_str`): born BELOW GIR drop registration → layer-appropriate fix:
  `FuncLowering.printf_str_temps: Vec<SlotId>` (`lower/mod.rs:124`), pushed
  at the bool-conversion arm, drained right after `emit_extern_call` for
  printf-like calls (`insts.rs:~690`) as SlotAddr + `CallExtern
  gorget_string_free` (`StringFree` already in the registry,
  `runtime.rs:362`). Shared LIR → covers LLVM too (scout spot-checked).

REJECTED alternative (record in the PR): making `gorget_bool_to_str` return
a static `.rodata` view — zero-alloc but flips its `sig_fresh` registry
classification and couples into the view-producer enumeration + lints;
the conservative post-call free avoids that. Owner may revisit.

## Work

1. Apply/productionize the committed diff (comments per house style — state
   the constraint, not the history). One commit is acceptable; two (GIR
   sites, then LIR site) also fine.
2. Fixtures: commit ALL ELEVEN probe shapes as integration fixtures with
   embedded stdout [p1-R2] — field-print (TODO:746 names
   `print_struct_string_field_leak.gg`; use an OWNED-heap field, not a
   literal-initialized one — literal fields are static views and leak-free),
   bool-print, f-string-bool, format-assign (`String t = f"{b}"`),
   `f"{expr}"` Move shape, display, `'b'` spec, loop accumulation,
   match-arm-early-return interp, if-block interp, view-source `trim()`.
3. Doc/TODO updates (per the scout's §6, verbatim targets): handover-block
   exemption note (TODO:31) deleted; TODO:737 rephrased to the perf-only
   residual; TODO:746 + TODO:843 → DONE; TODO:738 REWRITTEN to the real
   remaining cohort (cli_basic strip_dashes 7B, yaml_parse method-body
   escape-clones, string_conversions std.conv externs, datetime_format
   extern, print_trait_object Box+trait-dispatch — note the trait-dispatch
   result is a COUSIN of this class in trait-call lowering → its own entry,
   fstring_method_chain closure-env) and the stale `cow_elem_overwrite_
   witness` name removed; ADD the site-C ungated sub-shape TODO [p1-R4]
   (Clone-of-NON-string-aggregate interp temps stay unregistered — the
   pre-existing shallow-Clone gap; registering would double-free until the
   Clone lowering deep-copies aggregates); DONE.md entry states the
   known-exempt basis for
   the 2 cow canary leaks is RETIRED (future ASan tables over the cow
   battery expect ZERO leaks) — the Phase-1 brief/DONE historical mentions
   stay as records.
4. Consider (small, optional): an ASan-battery structural guard per
   CLAUDE.md #6 — if not now, file the TODO.

## Gates (executor; parent re-runs the full battery on the integrated tree)

0. Step-0 pre-change ASan table over: the 5 named leaks + the 11 probe
   shapes + **THE SWEEP LIST [p1-R1, embedded — the durable record]:**
   fstring_expressions, string_builder, string_builder_loop,
   string_chained_methods, string_concat, string_fstring_stress,
   char_methods, chars, json_parse, csv_stringify, dict_string_keys,
   cow_escape_boundaries, string_struct_complex, leak_string_heavy,
   leak_string_ops, snag30_field_alias_in_match_arm, fstring_format,
   yaml_parse, string_conversions, cli_basic, fstring_method_chain,
   print_trait_object, datetime_format (+ bench_string_methods is a
   KNOWN pre-existing link-fail both ways — record, don't gate).
   Capture stdout with `detect_leaks=0` — LSan's exit path can skip stdio
   flush on pipes.
1. Post-change vs YOUR Step-0 table (absolute bytes are shape/tree-specific
   — the LIST + per-fixture direction is the contract): the named/probe set
   ZERO; the first 17 sweep names CLEAN; yaml_parse / string_conversions /
   cli_basic / fstring_method_chain / print_trait_object IMPROVED (≤ Step-0,
   residuals are the documented other-class cohort); datetime_format may be
   unchanged; **NO fixture worse than Step-0**.
2. stdout byte-diff pre/post over the whole affected set.
3. `cargo test --lib` and `cargo test --test lints` (quote the freshly
   printed counts); the cow/witness batteries via
   `cargo test --test integration cow_ -- --test-threads=4` and
   `cargo test --test integration witness` — gate on 0 failures with the
   freshly printed totals, NOT on a baked count [p1-R3].
4. `self_host_bootstrap_fixed_point` GREEN (`GG_BUILD_TIMEOUT_SECS=600`).
5. LLVM spot-checks (`--backend=llvm`): bool-print + field-print.
6. Perf per the Performance pillar: the 300k-iteration print-loop wall-clock
   (expect identical) + peak RSS (expect the ~3× improvement direction).

## Constraints

- Standard worktree preamble (pwd check, `git merge --ff-only gorget-1`,
  never touch main or `/workspace/gorget-1`); explicit-file adds; no pushes;
  STOP on contradicted premises.
- File zone: `src/ir/lowering/exprs/calls.rs`, `src/lir/lower/insts.rs`,
  `src/lir/lower/mod.rs`, new fixtures, `tests/integration.rs` (append),
  TODO.md, DONE.md. ⚠ SEQUENCING vs Chain C: C's item 3 also edits
  `src/lir/lower/insts.rs` (different region — `try_enum_payload_extract`
  caller at the top vs the printf arms ~:690/:4251). Chain D executes FIRST
  (smaller, prototype-complete); C rebases over it. Do NOT touch Chain B's
  zone (`tests/fixtures/self_host_lowerer/**`) or Chain C's other files.
- The scout's risk #1 (the Move-mode registration no-double-free argument
  for interp branches 2/3) is the one thing reviewers must re-derive from
  source rather than trust.
- Commit messages cite this brief + the scout; Co-Authored-By trailer.
