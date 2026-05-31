# Executor Brief — R5 PERF: precompute the `enum_variant_parent` index (self-host)

**Status:** DRAFT — under fresh-review discipline before launch. Scout-designed (the index already half-exists).
**Risk:** LOW-MEDIUM (output-NEUTRAL self-host perf; touches `lower.gg`+`gir.gg`). **Branch from the
post-imported-check base** (`gg` now strictly checks `lower.gg`/`gir.gg` — keep new code type/exhaustiveness-
clean).

## 0. Worktree discipline
`pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree. NEVER touch `/workspace/gorget-1`.
`git add <specific files>` only. FORCE-REBUILD the driver before comparison/bootstrap runs
(`rm -f tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.c`). `cargo build` +
`cargo test --lib` + the gates below — NOT the full sweep. `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=120`.

## 1. The hot spot (scout-measured) — ⚠ ALL line cites RE-ANCHORED to current HEAD (brief-review pass-1)
`enum_variant_parent(name, &gmod)` (`tests/fixtures/self_host_lowerer/lower.gg:3122-3130`) does, per call,
`gmod.type_infos.keys()` + a full `GirTypeInfo` deep-clone (`gmod.type_infos.get(tname).unwrap()` at `:3125`)
for EVERY type, scanning for a variant name → O(callsites × types). 2 hot call sites
(`lower.gg:3303`, `lower.gg:5790`). It's the #1 `array_clone` site (~748K). Rust has a precomputed flat
`enum_variants: FxHashMap` (`src/ir/lowering/context.rs:267`, O(1) lookup `:1553`) — never ported.
⚠ **Confirm these line numbers yourself before editing — they drift; grep the fn names.**

## 2. The fix (mirror Rust's flat index — and the self-host ALREADY half-has it)
`GirModule` already carries `enum_registry: Dict[String, Vector[String]]` (`gir.gg:351`) built ONCE at
construction. ⚠ **There are THREE `case IEnum(edef):` arms in `lower.gg` (`:9491` resource-detection, `:9568`
the type_infos/enum_registry BUILD arm, `:10080` local-name registration) — the build arm you want is the one
at `lower.gg:9568` that does `enum_registry.put(...)` at `:9583` and iterates `for var in edef.variants` at
`:9572`. Edit THAT arm, not the others.** Add a FLAT inverted index alongside `enum_registry`:
1. Add field `Dict[String, String] enum_variant_parent_idx` to the `GirModule` struct (`gir.gg:319-410`).
   ⚠ **APPEND AT END** (follow the `globals` field's documented convention `gir.gg:407-409` "appended at end
   to avoid shifting positional ctor call sites") so the positional ctor stays in lockstep. There is exactly
   ONE positional `GirModule(...)` ctor (`lower.gg:9697`, 24 args → 25) — a missed field/arg is a COMPILE
   ERROR (not a silent empty index), so it's safe; grep `GirModule(` to confirm it's the only one.
2. In the build `IEnum` arm (`lower.gg:9568`, inside the `for var in edef.variants` at `:9572`), add a
   **FIRST-WRITE-WINS** populate:
   `if not enum_variant_parent_idx.contains(var.name): enum_variant_parent_idx.put(var.name.clone(),
   edef.name.clone())` — ~2 lines, ZERO new traversal.
   ⚠⚠ **(brief-review pass-1 — BLOCKING) FIRST-WRITE-WINS IS LOAD-BEARING, not optional.** The current scan
   (`enum_variant_parent`) returns the FIRST enum (insertion order) whose variant matches; a plain `.put`
   keeps the LAST. They diverge IFF a compilation unit has TWO enums sharing a bare variant name. The corpus
   has ZERO such collisions TODAY (so a plain `.put` happens to be neutral now) — BUT the fn-count gate CANNOT
   detect a collision flip (it changes WHICH type emits, not the count; `fixed_point` reconverges since the
   self-host's own source has no colliding variants), so a future colliding fixture would SILENTLY MISCOMPILE
   and pass every gate. `if not contains: put` makes the index provably first-write-wins == the scan's
   first-match, a true no-op. (Precondition stated; do NOT use a plain `.put`.)
3. Rewrite `enum_variant_parent` (`lower.gg:3122-3130`) to:
   `if gmod.enum_variant_parent_idx.contains(name): return gmod.enum_variant_parent_idx.get(name).unwrap()
   else: return ""` (preserve the exact empty-string-on-miss behavior; the index stores `edef.name`, identical
   to the current scan's return `tname == edef.name`).
4. **LEAVE `find_enum_by_variant` (`gir.gg:438-452`) AS-IS** — do NOT repoint it (brief-review pass-1 nit): it
   is deliberately first-wins (`and result == ""` guard `gir.gg:448` + the determinism comment `:435-437`),
   and repointing it at the index would be the SAME first→last flip risk for a SMALLER win (it scans
   `enum_registry`, a `Vector[String]` clone per enum — cheaper than the per-type `GirTypeInfo` deep-clone the
   primary fix targets). Note it as a future follow-up; don't touch it here.

## 3. Gates (OUTPUT-NEUTRAL — this changes nothing the compiler emits)
- `cargo build` clean; `cargo test --lib` green.
- Force-rebuild the driver, then `--nocapture` matched-counts UNCHANGED vs the post-imported-check baseline:
  c_emit **850**, lowerer **952** (and resolver/parser/type/check unchanged — but those don't run lower.gg's
  index, so just confirm c_emit + lowerer). `self_host_bootstrap_fixed_point` GREEN (byte-reconverges
  identically — the index changes ONLY internal lookup cost, not output).
- ⚠ If `--clone-stats` is available on a self-compile, confirm the ~748K `array_clone` at this site drops
  (the perf win). Not required for the gate (output-neutrality is), but report it if you measure it.

## 4. Report back
Diff + commit; confirm c_emit 850 + lowerer 952 UNCHANGED + fixed_point GREEN (output-neutral); whether you
repointed `find_enum_by_variant` or left it; any clone-count delta you measured. Confirm you stayed in the
file zone (`lower.gg` + `gir.gg`) and never touched `/workspace/gorget-1`.

## 5. Don't-dodge
This is a pure internal-index optimization — the output MUST be byte-identical. If c_emit/lowerer counts
move or fixed_point reconverges to a DIFFERENT signature, you changed behavior — STOP and find why (a stale
index entry, a missed ctor site leaving the index empty, a `.clone()` semantics change). Do NOT adjust the
gate to pass.
