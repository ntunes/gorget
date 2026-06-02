# Brief — concrete user enum named V/E/K/U/T no longer dropped as a "generic placeholder"

Self-host-dir only, **`tests/fixtures/self_host_lowerer/lir_lower.gg` ONLY** (2-line change). File-DISJOINT
from `lower.gg`/`gir.gg`/`lir_codegen.gg`/`loader.gg`. DEEP-scout END-TO-END verified (2026-06-02, tip
`6b9dbde3`): hand-applied + rebuilt driver + ran the full affected set + re-checked all 267 snapshots
(0 regressions). ⚠ Needs ≥3 fresh sequential reviews before the executor.

## The bug (RUNNING + END-TO-END verified)
`is_generic_placeholder_name` (`lir_lower.gg:469-491`) is a NAME BLOCKLIST: `name == "T"/"E"/"K"/"V"/"U"`
(+ `__T`/`__E`/… suffixes). It's consulted at TWO `lower_type_defs` registration sites:
- Pass 1 (`lir_lower.gg:700`, iterates `gmod.type_table` GtNamed): `if is_generic_placeholder_name(name): continue`
- **Pass 1b (`lir_lower.gg:856`, iterates `gmod.type_infos.keys()` — the typed registry of REAL declared
  types): `if is_generic_placeholder_name(tname): continue`** ← the bug.

When a fixture declares `enum V` (e.g. `catch_into_noncopy_dest.gg`: `enum V: A(int) B(String)`), GIR
registers it as a real type in `gmod.type_infos["V"]` (is_enum=true). But Pass 1b SKIPS it on the name
match → `V` never enters `sr`/`m.structs` → Pass 2 (`:884`) `continue`s (`sr.get("V") < 0`) → no typedef,
no struct body. The drop/clone fns for `V` still emit (separate `m.recursive_drop_enums` channel) and
`find_c_name` (`lir_codegen.gg:4743-4750`) falls back to the raw name `V` (not in `m.structs`) → emits
`V V__clone(...)` → **`error: unknown type name 'V'`**. (Secondary: the auto-`Result__V__GorgetString`
wrapper's `Ok_0` field resolves to `void*` since `resolve_field_lir_type("V")` falls back to `LT_PTR` —
so a C-only typedef hand-patch does NOT compile; the fix MUST be at the registration source so bodies
re-lower `V` as a real struct.)

Per CLAUDE.md "fix is always upstream": lir_codegen faithfully emits whatever `m.structs` holds; the
bug is the upstream registration name-heuristic. Rust gg has NO such heuristic — it iterates only types
with a real TypeDef (`src/lir/lower/mod.rs:826`); generic params are erased by monomorphization so they
never appear. Rust emits `typedef struct __gg_V __gg_V;` + the struct body (verified via `--emit-c-lir`).

## The fix (2 lines, lir_lower.gg ONLY — typed discriminator, removes the over-match)
Guard BOTH skip sites with a typed check that the name is NOT a real declared type:
```
# lir_lower.gg:700 (Pass 1):
if is_generic_placeholder_name(name) and not gmod.type_infos.contains(name):
    continue
# lir_lower.gg:856 (Pass 1b):
if is_generic_placeholder_name(tname) and not gmod.type_infos.contains(tname):
    continue
```
`gmod.type_infos` is populated ONLY from real `IStruct`/`IEnum` module items (`lower.gg:11137/11159`) +
mono-instances — every key is a concrete declared type by construction, so it's the correct typed
discriminator (CLAUDE.md no-name-matching: distinguish real-type from generic-param by registry
membership, not by spelling). Pass 1b iterates `type_infos.keys()`, so its guard is now always-false
(correct — Pass 1b should NEVER skip a real declared type). Pass 1 still skips genuine generic-param
names that have NO TypeDef (preserves the original intent). Keep `is_generic_placeholder_name` itself
(still needed for the genuine-param case in Pass 1).
⚠ VERIFY `gmod`/`type_infos` is in scope at both sites with that accessor (`.contains`); the scout
applied exactly this. If the local is named differently in `lower_type_defs`, adapt.

## Scope + realistic yield (END-TO-END verified — a fixture counts ONLY if its WHOLE stdout MATCHes)
**+3 fixtures truly flip to MATCH** (byte-identical, verified): `catch_into_noncopy_dest`,
`catch_divergent_arm`, `snag41_match_scrutinee_consume`. The class has exactly 12 single-letter-enum
CC-FAIL fixtures; the other 9 clear the typedef layer but hit ORTHOGONAL pre-existing blockers (NOT
introduced by this fix — verified the baseline C already contained them, masked behind the first error):
- dominant (6 of 9): a bare unmonomorphized `Result`/`Option` template name leaking into function
  bodies (`'Result' undeclared`) — a SEPARATE follow-up (+5-6), its own chain.
- `rethrow_catch_binding` (rethrow-payload garbage), `snag42` (value-lowering), `dict_nested_pattern_noncopy_enum`
  (`'Option' undeclared` + GorgetMap-from-int) — distinct.
⚠ Do NOT claim ~12 or the `result_*` fixtures (those use prelude `Result[int,String]`, a DIFFERENT
class — not affected). Snapshot ONLY the 3 verified MATCHes (re-confirm each by running).

## Validation gate (self-host-dir only; FORCE-REBUILD driver before each comparison/diff run)
1. `cargo build` + `cargo build --release` + `cargo test --lib` (~1066/0).
2. Force-rebuild driver (`rm -f tests/fixtures/self_host_lowerer/driver{,.c}`; `GG_BUILD_TIMEOUT_SECS=600`).
3. `self_host_runtime` ≥ **267/0** + 3 new snapshots (`catch_into_noncopy_dest`, `catch_divergent_arm`,
   `snag41_match_scrutinee_consume`) — verify each vs `cargo run -- run` byte-identical.
4. `lowerer_comparison` ≥ **954**, `c_emit_comparison` ≥ **882** (re-confirm from `--nocapture`).
   ⚠ This change makes the self-host EMIT struct/typedef defs for V/E/K/U-named enums where it didn't —
   `c_emit` counts USER FNS (not structs/typedefs), so fn-count should be unchanged-or-better; but a
   V-named enum's now-correctly-typed drop/clone fn bodies could shift counts — investigate any delta,
   ensure it's ≥.
5. `GG_RUNTIME_DIFF=1 … self_host_runtime_diff` → MATCH ≥ **270** (267 + 3), NO fixture MATCH→worse.
6. `bootstrap_fixed_point` GREEN (the driver itself may declare single-letter generic params — this is
   the KEY regression guard: confirm genuine generic-param names with no TypeDef are STILL skipped in
   Pass 1, i.e. the driver still bootstraps byte-identically).

## Files (stage by name only)
`tests/fixtures/self_host_lowerer/lir_lower.gg` + new `tests/fixtures/runtime_snapshots/*.out` (the 3).
Do NOT touch `lower.gg`/`gir.gg`/`lir_codegen.gg`/`loader.gg`/`src/`/`TODO`/`DONE`.

## Follow-ups to LOG
- **Bare unmonomorphized `Result`/`Option` in function bodies (+5-6, 6 of the 9 remaining):** the
  `throws` desugaring's `Result[T,E]`/`Option[T]` isn't resolved to its monomorphized name in the body
  (`'Result'/'Option' undeclared`). Its own chain (likely `lower.gg`/`lir_lower.gg` mono path).
- `rethrow_catch_binding` rethrow-payload; `snag42` value-lowering; `dict_nested_pattern_noncopy_enum`.
