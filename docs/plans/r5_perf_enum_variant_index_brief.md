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

## 1. The hot spot (scout-measured)
`enum_variant_parent(name, &gmod)` (`tests/fixtures/self_host_lowerer/lower.gg:3095-3103`) does, per call,
`gmod.type_infos.keys()` + a full `GirTypeInfo` deep-clone (`gmod.type_infos.get(tname).unwrap()`) for EVERY
type, scanning for a variant name → O(callsites × types). 2 hot call sites in `lower_call`
(`lower.gg:3276` `lower_nullary_variant_ident`, `lower.gg:5727` general call return-type inference). It's the
#1 `array_clone` site (~748K). Rust has a precomputed flat `enum_variants: FxHashMap` (`src/ir/lowering/
context.rs:267`, O(1) lookup `:1553`) — never ported.

## 2. The fix (mirror Rust's flat index — and the self-host ALREADY half-has it)
`GirModule` already carries `enum_registry: Dict[String, Vector[String]]` (`gir.gg:339`) built ONCE at
construction (`lower.gg:9426-9451` — the `IEnum` arm already iterates every variant name). Add a FLAT
inverted index alongside it:
1. Add field `Dict[String, String] enum_variant_parent_idx` to the `GirModule` struct (`gir.gg:~307-346`).
   Update the single `GirModule(...)` constructor call (`lower.gg:9565`) to pass an empty `{}` (and any
   other ctor/`new_*` site — grep `GirModule(` to be sure).
2. In the existing `IEnum` arm (`lower.gg:9448-9449`) where it already iterates `var.name`, also
   `enum_variant_parent_idx.put(var.name.clone(), edef.name.clone())` — ~2 lines, ZERO new traversal.
3. Rewrite `enum_variant_parent` (`lower.gg:3095-3103`) to:
   `if gmod.enum_variant_parent_idx.contains(name): return gmod.enum_variant_parent_idx.get(name).unwrap()
   else: return ""` (preserve the exact empty-string-on-miss behavior).
4. (Optional, same class) repoint `find_enum_by_variant` (`gir.gg:426-440`) — which does the SAME variant→
   parent lookup over `enum_registry` with a `Vector[String]` clone per enum — at the new flat index to kill
   the second linear scan. Only if it's a clean drop-in; otherwise leave it + note it.

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
