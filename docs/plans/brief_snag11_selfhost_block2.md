# BRIEF — snag #11 self-host Block 2 (From emission + conversion)

Status: v1 (orchestrator draft from scout `agent-aab00ebbb5ae081a4`, proven
prototype `b81f4ee6` / `docs/plans/snag11_selfhost_block2_prototype.diff`,
2026-06-11). Owner: SHIP Block 2 (the reject-gate Block 1 is DEFERRED — see the
SELF-HOST snag #11 remainder entry in TODO; gorget-js is NOT blocked by snag
#11). **SELF-HOST ONLY** — the Rust side already mangles correctly (it is the
reference); do NOT touch Rust. Pass-1 fold (fresh reviewer, all 5 claims PASS,
2 sharpenings): corrected the sibling-site map (line numbers had drifted) + the
centralization is REQUIRED-for-consistency framing (vtable FnRef dangles
otherwise); EXCLUDE the two `_VTable` struct-name sites from the helper (per-
trait, not per-instantiation). NEEDS a fresh confirming pass (pass-1 raised
reservations → cannot stop).

## Mission
Bring the self-host's trait-equip `From` mangling up to Rust parity so the
already-landed dormant lowering no-op (`maybe_emit_from_conversion`,
`tests/fixtures/self_host_lowerer/lower_match.gg`) activates → the self-host
CONVERTS From-mediated cross-error propagation correctly (`B.from(e)` instead of
the type-confused bit-copy), closing the From-path OOB-read in self-host-compiled
code. Proven end-to-end: From-positive runs `code=42` (was OOB garbage),
`fixed_point` GREEN 466s, parity +1 (518→519/1009), corpus `--emit-c` byte-diff
**1213 identical / 5 changed** (all parity-neutral — the changed symbols now
match Rust + a latent `From`-collision is fixed). **Fix the CLASS, not just the
two instances.**

## The proven core (apply + productionize)
`docs/plans/snag11_selfhost_block2_prototype.diff` (28 lines, 2 files):
- `lower_closures.gg` `lower_equip_block`: append `spanned_type_to_name(tga)`
  for each `eqblk.trait_generic_args` to the trait prefix → emits
  `From__GorgetString_for_BigErr__from` (was `From_for_BigErr__from`, dropping
  the `[String]` arg; the parser stores the bare `"From"` in `eqblk.trait_name`
  with args separate, `parser.gg:~3596`).
- `lower.gg` `IEquip` fn_sigs pre-pass (~`:2768`): also register the PREFIXED
  symbol (not only the short `BigErr__from`) so the conversion lookup +
  `maybe_emit_from_conversion`'s `gmod.fn_sigs.contains("From__…")` match.

## Fix the CLASS (CLAUDE.md "fix the class, not the instance") — REQUIRED for consistency, not just hygiene
The prototype fixes the 2 sites on `From`'s path. `From` itself does NOT reach
the `did_split` route (it's a builtin-method trait, `traits.gg:~437`, not a
bodied `trait From:`, so `trait_defs.contains("From")` is false → the 2-site fix
is provably sufficient FOR `From`). But the SAME drop-the-arg drift lives at the
`did_split` vtable/default-method sites, and centralizing is **required for
internal consistency** (not optional): site 1 already changes the own-method
body name for a generic-arg trait, so the vtable slot FnRef would DANGLE unless
spelled by the same helper. The actual equip-`_for_`-symbol sites (pass-1
reviewer-verified — earlier line numbers had drifted):

| # | Site | Builds | Action |
|---|------|--------|--------|
| 1 | `lower_closures.gg:~297` | `lower_equip_block` body mangling (From / not-did_split) | prototype fixes |
| 2 | `lower.gg:~2771` | IEquip fn_sigs short symbol | prototype fixes (register prefixed) |
| 3 | `lower.gg:~3248` | did_split own-trait default-method body | route through helper |
| 4 | `lower.gg:~3300` | did_split vtable slot FnRef (`_for_…__slot`) | route through helper |
| 5 | `lower.gg:~3303` | did_split vtable global name (`_for_…_vtable`) | route through helper |
| 6 | `lower.gg:~3340` | not-did_split default-method body | route through helper |

(The `did_split` own-method BODIES are NOT a separate site — they route through
site 1 via `lower.gg:~3219` calling `lower_equip_block(own_eqblk)`.)

REQUIRED:
1. **Centralize:** add ONE `mangle_trait_equip_name(tname, trait_generic_args)`
   helper (mirror Rust `src/ir/lowering/traits.rs:~1614`) that appends the args,
   and route sites 1, 3, 4, 5, 6 (the `_for_<type>__<method>` and
   `_for_<type>_vtable` symbols) through it. One source of truth (devbook/24
   rule 3) — keeps the FnRef + body + vtable-slot mutually consistent.
2. ⚠ **EXCLUDE the `_VTable` STRUCT-name sites** (`lower.gg:~2947` and the
   did_split `_VTable` at `~:3295`, `mangle_trait_name(...)+"_VTable"`) — these
   spell a TYPE name, kept PER-TRAIT (not per-instantiation) by Rust, so they
   must NOT get the generic-arg suffix. The helper applies ONLY to
   `_for_<type>__<method>` / `_for_<type>_vtable` symbols, NOT `_VTable` type
   names. A naive "route every `mangle_trait_name(` call" would wrongly suffix
   these — do NOT.
3. **Site-count lint** (`tests/lints.rs`, cf. `container_literal_arms_count` /
   `snag11_auto_prop_gate_site_count`) so a new equip-symbol-mangle site is
   forced through the helper.

(Empty-arg traits — Writer/Reader/Serializer/Deserializer — are byte-identical
through the helper since it appends nothing; only generic-arg traits reaching
`did_split` shift, and that shift is TOWARD Rust parity. The full-corpus
emit-diff gate must confirm each delta is intended; STOP on an unexpected one.)

## Snapshot
Regen the `snag11_from_mediated_propagation` runtime snapshot
(`tests/fixtures/runtime_snapshots/`) so `self_host_runtime` locks the new MATCH
(the fixture flips WRONG→MATCH).

## Gates (executor; parent re-runs the battery)
- `cargo build`; `cargo test --lib`; `cargo test --test lints` (incl. any new
  site-count lint).
- `self_host_bootstrap_fixed_point` GREEN (`GG_BUILD_TIMEOUT_SECS=600`) —
  byte-identity is load-bearing (the self-host lowering change must self-compile
  to a fixed point; the scout measured 466s GREEN on the 2-site prototype —
  re-confirm with the centralized class-fix).
- **Full-corpus `--emit-c` byte-diff** old-vs-new self-host driver: ONLY the
  expected fixtures change (prototype baseline: 1213 identical / 5 changed — the
  `From`/operator fixtures whose symbols now match Rust; the 4 trait fixtures
  CC-FAIL both before AND after = pre-existing, parity-neutral). ⚠ The
  centralization touches MORE sites than the prototype → re-run this diff and
  CONFIRM no UNexpected fixture changes (a `did_split` sibling now emitting a
  different symbol for a generic-arg trait could shift others — verify each
  delta is intended + parity-neutral).
- `self_host_runtime_diff` (`GG_RUNTIME_DIFF=1`): parity +1 (519/1009),
  `snag11_from_mediated_propagation` WRONG→MATCH, NO regression.
- `c_emit_comparison` no regression (read the count).

## Constraints
- **SELF-HOST ONLY** zone: `tests/fixtures/self_host_lowerer/{lower.gg,
  lower_closures.gg}`, `tests/fixtures/runtime_snapshots/`, possibly
  `tests/lints.rs`, `TODO.md`/`DONE.md`. Do NOT change `src/` (Rust is already
  correct + is the reference). ⚠ Mind self-host symlinks — md5sum `lower.gg`/
  `lower_closures.gg` across the self_host_* dirs and edit the right copy
  (`lower.gg` lives in `self_host_lowerer`; confirm whether `lower_closures.gg`
  is shared).
- Worktree preamble (`pwd`/toplevel check, `git merge --ff-only gorget-1`, never
  touch `/workspace/gorget-1`); explicit-file `git add`; no push; STOP on a
  contradicted premise (esp. `fixed_point` regressing, or the emit-diff showing
  an unexpected fixture changing).
- Commit cites this brief + the scout; Co-Authored-By trailer. Do NOT mark
  anything LANDED in TODO (the Rust+self-host snag #11 closure → DONE.md when
  integrated; the deferred Block 1 stays in TODO).
