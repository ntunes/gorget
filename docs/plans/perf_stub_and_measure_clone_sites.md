# PERF brief — attribution-first stub-and-measure of named clone sites (self-host)

## Goal (HONEST, BOUNDED — measurement-first, conditional fix)
The handover states perf has **no clean quick-win**: ~15.9M array_clone /
~1.30GB RSS, dominated by the *systemic* ~800 `.get().unwrap()` value-bind sites
(a deep CoW audit, its own chain). enum_variant_parent index + drop_elab
packed-bitset are ALREADY landed. So this slot is **attribution-first**: measure
the 1–2 concrete named candidate sites; **land a fix ONLY if a single site
clones >1M AND the borrow rewrite is clean + output-neutral.** Otherwise the
deliverable is the measurement + an honest TODO update. Do NOT manufacture a
fix the data doesn't support (CLAUDE.md "refusing to manufacture work").

## Measurement method (the `[clone-stats]` runtime counter)
`--clones=stats` instruments the COMPILED binary to print
`[clone-stats] array_clone=N` at exit (`src/lir/mod.rs:1755`). (The pre-unification
`--clone-stats` alias was removed — use `--clones=stats`.)
To attribute the **self-host driver's** clones:
1. Build the driver binary with stats on (⚠ R1 fold: `gg build` takes ONE
   positional filename — NO `lib` arg here; `lib` is a DRIVER run-time arg):
   `cargo run --release -- build --clones=stats tests/fixtures/self_host_lowerer/driver.gg -o /tmp/driver_stats`
   (matches `scripts/self_host_mem_baseline.sh:60` + `build_gg_dir` at
   `tests/integration.rs:9223`).
2. Run the instrumented driver on a representative heavy workload (the driver
   compiling ITSELF is the canonical one, per `self_host_mem_baseline.sh`):
   `/tmp/driver_stats tests/fixtures/self_host_lowerer/driver.gg lib --emit-c > /dev/null`
   → read the `[clone-stats] array_clone=N` baseline from stderr.
3. STUB the candidate site (rewrite the value-bind to a borrow / index-read that
   avoids the deep clone), rebuild the driver, re-run the same workload →
   `array_clone=N'`. **Delta = N − N'** is that site's attributable clones.

## Candidate sites (RE-LOCATE before measuring — line cites drift)
1. **`build_struct_names` (lir_codegen.gg:~245):**
   `LirStructDef sdef = m.structs.get(i).unwrap()` value-binds (deep-clones) a
   whole `LirStructDef` per struct just to read `sdef.name` (6 reads). Stub: read
   the name without binding the whole struct — `m.structs.get(i).unwrap().name`
   (that exact borrow form already exists in-file at lir_codegen.gg:723). ⚠ **R2
   fold: this is called ONCE per compile via `generate_c` (lir_codegen.gg:5407),
   so its loop runs `m.structs.len()` (hundreds) times per compile — it is
   STRUCT-COUNT-BOUNDED, expected FAR below the >1M threshold. A few-hundred
   clone delta here is the EXPECTED result, not a surprise; it would be a tidy
   micro-cleanup but will NOT clear the ">1M → land it" gate. Measure it to
   confirm the bound, don't expect a win.** Note: a `LirStructDef` deep-clone is
   a STRUCT clone; it only bumps `array_clone` via its inner fields-vector.
2. **The GirType value-bind in `lower.gg` (cite `:901` is STALE — re-grep):**
   find the hottest `GirType x = ....get(...).unwrap()` / `.type_table.get(...)`
   value-bind in a per-instruction or per-type hot loop in `lower.gg`. Measure.
3. If neither is >1M, **stop** — report that the remaining clones are the
   systemic 800-site CoW class (no single dominant site) and recommend the deep
   CoW/borrow audit as a dedicated future chain. That honest conclusion IS a
   valid deliverable.

## If (and only if) a clean >1M win is found — land it
- The rewrite must be **output-neutral**: prove via `self_host_bootstrap_fixed_point`
  GREEN (byte-identical reconverge) + `lowerer_comparison`/`c_emit_comparison`
  UNCHANGED + `self_host_runtime` 284/0.
- Report the measured before/after `array_clone` delta in the commit + TODO.

## Scope discipline / file zone
- Touch ONLY `tests/fixtures/self_host_lowerer/lir_codegen.gg` and/or `lower.gg`
  (the site you measure-and-maybe-fix). If you fix in `lower.gg`, AVOID the
  EDo/EBlock hunks (~5845/6015) and the dead-field/ctor hunks
  (177/8899/9093/10347/11891) — those belong to the concurrent FIDELITY + CLEANUP
  chains. Prefer the `lir_codegen.gg` site to stay fully file-disjoint.
- Run in your OWN worktree; NEVER touch `/workspace/gorget-1`.
- Do NOT commit the `--clones=stats` measurement artifacts or any temporary
  instrumentation — only a clean borrow rewrite (if landed).

## Deliverable
Either: (a) one clean output-neutral micro-fix with a measured >1M clone
reduction + gates green; OR (b) a measurement report (per-site deltas) + a TODO
line concluding "no single >1M site; remaining = systemic CoW, deep audit
needed." Both are acceptable; (b) is NOT a failure.
