# BRIEF — snag #11 self-host Block 2 (From emission + conversion)

Status: v1 (orchestrator draft from scout `agent-aab00ebbb5ae081a4`, proven
prototype `b81f4ee6` / `docs/plans/snag11_selfhost_block2_prototype.diff`,
2026-06-11). Owner: SHIP Block 2 (the reject-gate Block 1 is DEFERRED — see the
SELF-HOST snag #11 remainder entry in TODO; gorget-js is NOT blocked by snag
#11). **SELF-HOST ONLY** — the Rust side already mangles correctly (it is the
reference); do NOT touch Rust. Pass-2 fold (fresh reviewer) caught an INVERTED
class-fix that pass-1 introduced: Rust uses TWO mangling paths — UNREGISTERED
(From etc.) suffixes, REGISTERED/vtable (bodied/`did_split` traits) stays BARE —
so v2's "route sites 3/4/5 through a suffix helper" would DIVERGE the self-host
from Rust. v3 corrects: suffix the unregistered path only (site 1 gated on
`not trait_defs.contains`, site 2, site 6); keep registered/`did_split` (sites
3/4/5) + `_VTable` BARE; the prototype's unconditional site-1 suffix is made
route-aware. (Prototype From-core is correct and unchanged; the class-fix beyond
From is LATENT — no corpus delta — so REVIEW vs Rust's two-path mangling is the
gate, not emit-diff.)
✅ REVIEW-CLEAN: pass-3 (fresh) SIGN OFF — independently verified the Rust
two-path mangling against `traits.rs` (unregistered `:1614` suffixes;
registered/vtable body/FnRef/global/`_VTable` all bare) + confirmed v3 fixes the
v2 inversion + re-confirmed all-latent-beyond-From (`BoundedRange[T]` is the only
generic user trait, never equipped). 3 sequential fresh passes (2 → inversion →
clean). READY for the executor. (Pass-3 non-blocking note: site 6's suffix
direction is debatable-in-principle but provably UNREACHABLE/zero-delta — if
uncertain, leave it bare and review-match Rust; do not let it block.)

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

## Fix the CLASS — mirror Rust's TWO mangling paths (⚠ pass-2 correction: the v2 "route everything through a suffix helper" direction was INVERTED)
Rust mangles trait-equip symbols via TWO paths (verified `src/ir/lowering/traits.rs`):
- **UNREGISTERED path** (`mangle_trait_equip_name` `:1614`, used for
  From/TryFrom/Default/operators — NON-bodied traits): appends a `generic_suffix`
  (`__<args>`) → `From__GorgetString_for_BigErr__from`.
- **REGISTERED/vtable path** (bodied traits with a `TraitDef`): keeps the trait
  name **BARE** — the body (`:271-273`/`:516-518`), the vtable slot FnRef
  (`:1019`), and the vtable global (`:1011`) are ALL `{trait_name}_for_{type}…`
  with **NO suffix**.
The self-host `did_split` gate (`lower.gg:~3200`, `trait_defs.contains(tname)`)
**IS** Rust's registered path → it must stay BARE. So the fix is **suffix on the
UNREGISTERED path only; keep the registered/`did_split` path BARE** — NOT "route
every site through a suffix helper" (the v2 error: that would make the self-host
DIVERGE from Rust for bodied generic-arg traits). `From` is unregistered
(`trait_defs.contains("From")` is false, `traits.gg:~437`) → the prototype's
2-site fix is correct for From.

| # | Site | Rust path | Correct action |
|---|------|-----------|----------------|
| 1 | `lower_closures.gg:~297` (`lower_equip_block`, route-AGNOSTIC today) | both | suffix ONLY when `not trait_defs.contains(tname)`; **BARE on the did_split route** |
| 2 | `lower.gg:~2771` (IEquip fn_sigs) | matches site 1 | register the spelling matching the body (suffixed unregistered / bare registered) |
| 6 | `lower.gg:~3340` (not-did_split default-method body) | UNREGISTERED | suffix (Rust-faithful) |
| 3 | `lower.gg:~3248` (did_split own-method body) | REGISTERED | **keep BARE — NO change** |
| 4 | `lower.gg:~3300` (did_split vtable slot FnRef) | REGISTERED | **keep BARE — NO change** |
| 5 | `lower.gg:~3303` (did_split vtable global) | REGISTERED | **keep BARE — NO change** |

REQUIRED:
1. Add ONE `mangle_trait_equip_name(tname, trait_generic_args)` helper (mirror
   Rust `:1614`) that appends `__<args>`. Use it on the UNREGISTERED path ONLY:
   site 1 GATED on `not gmod.trait_defs.contains(tname)`, site 2 (matching
   spelling), site 6. ⚠ **The prototype's site-1 suffix is currently
   UNCONDITIONAL** → make it route-aware (the gate) so it does NOT suffix
   `did_split` own-method bodies — a latent Rust-divergence the prototype
   introduces (no corpus trait exercises it, so it's emit-diff-INVISIBLE; fix it
   for showcase faithfulness — "self-host = elegance showcase" + devbook/24
   "resolve once, write through toward the reference").
2. Sites 3/4/5 (registered/`did_split`) stay BARE — **do NOT route them through
   the suffix helper** (that was the v2 error). The two `_VTable` STRUCT-name
   sites (`lower.gg:~2947`/`~3295`, `mangle_trait_name(...)+"_VTable"`) also stay
   BARE — Rust keeps `_VTable` PER-TRAIT (`traits.rs:179`), not per-instantiation.
3. **Site-count lint** (`tests/lints.rs`, cf. `container_literal_arms_count` /
   `snag11_auto_prop_gate_site_count`) over the equip-symbol-mangle sites.

⚠ **ALL of this beyond the From path is LATENT** (pass-2-verified): the ONLY
bodied generic trait in the corpus is `BoundedRange[T]` (never equipped), and no
not-did_split default method takes generic args — so sites 2/3/4/5/6 produce
ZERO emit-diff delta today; the only observable change is the From symbol via
site-1-unregistered. **The emit-diff gate therefore CANNOT catch a wrong
direction here** (no fixture produces the delta) — the GATE is matching Rust's
two-path mangling by REVIEW, not the corpus. (Empty-arg registered traits —
Writer/Reader/Serializer/Deserializer — already stay bare on both sides; no change.)

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
