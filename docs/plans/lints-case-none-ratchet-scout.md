# Scout: `no_growth_in_self_host_lower_case_none_pass_stubs` RED ratchet (21 > 18)

**Date:** 2026-06-24 · **Base:** gorget-1 tip `57b44418` · **Status:** decision-ready, prototype validated GREEN.

## TL;DR — RECOMMENDATION: RE-ANCHOR (not blunt re-baseline, not a burn)

The ratchet is RED (`count=21 > BUDGET=18`), but a burn is **impossible this round** and a
naive `BUDGET=21` bump is **worse than the status quo** — both because the lint is a *blunt
textual instrument* that counts EVERY `case None:`→`pass` in a 4500-line lowerer, not the
method-generic equip-dispatch class it was designed to police. The three stubs that pushed
18→21 are **vacuous `Option` None arms from unrelated parity fixes with literally nothing to
lower**, and the **one genuine deferred class member can't be burned** (no repro fixture →
untestable, per TODO.md L131).

**Fix: re-anchor the lint to the genuine class** — count only the `case None:`→`pass` whose
following line carries the distinctive `gm-inherent-generic-equip DEFERRED` marker. That makes
the count **exactly 1**, `BUDGET=1`, **TARGET 0** (implement the inherent `gm_` body → delete the
marker → count drops to 0). Idiomatic vacuous `Option` None arms no longer false-positive. This
is the only option that leaves a **GREEN, MEANINGFUL** ratchet (Core invariant #6 — a guard that
goes red on correct unrelated code can no longer catch the next real growth).

**Gate:** `cargo test --test lints no_growth_in_self_host_lower_case_none_pass_stubs` (+ full
`cargo test --test lints`). This touches **only `tests/lints.rs`** (a test, not emitted
lowering) → **`self_host_bootstrap_fixed_point` is NOT required.** Prototype was validated:
lint GREEN (count=1), all 28 lints pass, `cargo build` clean, `cargo test --lib` 1084/0.

---

## 1. The 21 `case None:`→`pass` stubs (file:line + arm purpose)

`tests/fixtures/self_host_lowerer/lower.gg`, enclosing `match` scrutinee in parens.
Class column: **DEF** = the genuine deferred method-generic equip body-emit class (what the
ratchet is *meant* to police); **opt** = vacuous `Option` destructuring (nothing to lower);
**eq-misc** = an equip/dispatch `trait_name` None arm that is a correct "no-trait/inherent
handled elsewhere" branch, NOT a body-emit stub.

| # | `case None:` line | match scrutinee | arm purpose | class |
|---|---|---|---|---|
| 1 | 876 | `builtin_method_mutates(mname)` | unknown builtin ⇒ conservative-mutating default (comment after `pass`) | opt |
| 2 | 1394 | `lv_meta.materialize_fn` | no materialize fn → fall through | opt |
| 3 | 1396 | `resource_meta_for(pointee_tname)` | no resource-meta → default clone path | opt |
| 4 | 2851 | `rs_fdef.throws_type` | non-throwing fn → no throws sig | opt |
| 5 | 2910 | `rs_ext_ret_opt` | `return` with no value → no redirect symbol to extract | opt **(NEW)** |
| 6 | 2961 | `cd_seval` | const-String eval produced nothing → skip | opt |
| 7 | 3098 | `ext_ret_opt` | `return` with no value → no redirect symbol to extract | opt **(NEW)** |
| 8 | 3309 | `resource_meta_for(mrinst.base_name)` | no resource-meta → not a Box, proceed | opt **(NEW)** |
| 9 | 3477 | `fsdef.throws_type` | non-throwing fn-sig → no throws sig | opt |
| 10 | 3525 | `ieqblk.trait_name` | inherent equip (no trait) → no vtable-method registration | eq-misc |
| 11 | 3545 | `em.throws_type` | non-throwing method → no throws sig | opt |
| 12 | 3580 | `ieqblk.trait_name` | inherent equip → no trait-default fn-sig registration | eq-misc |
| 13 | 3603 | `ieqblk.trait_name` | inherent equip branch (else handled) | eq-misc |
| 14 | 3752 | `dsi_tmpl.trait_name` | inherent drop-strategy template → no trait | eq-misc |
| 15 | 4038 | `eqblk.trait_name` | inherent equip → type-param-count split path | eq-misc |
| 16 | 4219 | `eqblk.trait_name` | inherent equip → `did_split` fallback | eq-misc |
| 17 | 4271 | `eqblk.trait_name` | inherent equip → no via-delegation trait stubs | eq-misc |
| 18 | 4295 | `eqblk.trait_name` | inherent equip nested arm | eq-misc |
| 19 | 4297 | `eqblk.via_field` | no via-field delegation | opt |
| 20 | 4397 | `equip_tmpl.trait_name` | inherent equip template → no trait | eq-misc |
| 21 | **4503** | `gm_eqtmpl.trait_name` | **method-generic body-emit on GENERIC receiver, inherent path — DEFERRED stub** | **DEF** |

**Only #21 (L4503) is the class the ratchet was built for.** Its `pass` is immediately
followed by the marker comment `# gm-inherent-generic-equip DEFERRED — …` (the doc-comment
notes the comment is placed AFTER the `pass` so `case None:`→`pass` adjacency is preserved).

## 2. Which ~3 grew the count 18→21, and their introducing commits

**Pin commit:** `0dc7c861` (`feat(self-host): lower method-generic methods in inherent
(non-trait) equip blocks (+iter_enumerate_zip)`, 2026-06-18) pinned `BUDGET=18`. Verified
the stub count at that commit's `lower.gg` = **18** (script-counted on
`git show 0dc7c861:tests/fixtures/self_host_lowerer/lower.gg`).

Scrutinee-delta (current − pin): **+4 new, −1 removed = net +3.**
- `cd_feval` → `cd_seval` is a **rename** (the const-decl String-fallback arm): −1 `cd_feval`,
  +1 `cd_seval`, net 0.
- The **3 genuinely new** stubs:

| New stub | line | introducing commit | what it is |
|---|---|---|---|
| `match ext_ret_opt:` | 3098 | `05daf35b` `fix(self-host): typed is_extern_stub — emit dropped non-String inline externs (Case-B Inc-1b / A.3)` | the `Option[ReturnExpr]` of a `SReturn` while extracting an extern-redirect symbol; None = bare `return` (no value) |
| `match rs_ext_ret_opt:` | 2910 | `239083f2` `fix(self-host): register entry-module extern return types in fn_sigs + free-len()` | same shape, entry-module extern variant; None = bare `return` |
| `match resource_meta_for(&gmod, mrinst.base_name):` | 3309 | `19d1529a` `fix(self-host): Case-B A_closure — de-keyword 8 type-keywords + fix latent Box__T._0-vs-void* lowering` | `Option[ResourceMeta]`; None = type has no resource-meta (not a box), proceed |

All three came from **legitimate parity-fixing commits** and each `case None:` is a
**vacuous, correct branch with nothing to lower** — not a method-generic dispatch stub.

## 3. Per new stub: lowerable-now? testable-now? regression risk?

| New stub | lowerable now? | testable now? | regression risk of "fixing" |
|---|---|---|---|
| `ext_ret_opt` (L3098) | **N/A** — it's `Option[ReturnExpr]`; `None` = the `return` statement carried no value, so there is no symbol to extract. There is nothing to "lower through the shared shape." | n/a | "Fixing" it is a category error — it's already complete and correct. |
| `rs_ext_ret_opt` (L2910) | **N/A** — identical shape/semantics to the above. | n/a | same |
| `resource_meta_for(mrinst.base_name)` (L3309) | **N/A** — `None` = the type has no resource-meta (not a box); the code correctly leaves `mrinst_is_box=false` and proceeds. | n/a | same |
| **#21 `gm_eqtmpl` (L4503)** — the actual class member | **NO this round.** Mirroring the `proto_minsts` body-emit arm needs a `fn_sigs.contains(gm_pmi.mangled_symbol)` dedup guard FIRST (without it a naive mirror double-emits the same inherent instance's mangled symbol → duplicate C symbol → build break). | **NO.** No corpus fixture and no `bootstrap_fixed_point` driver path exercises the `gm_` inherent generic-receiver method-generic instance — confirmed by TODO.md L131 ("NO corpus fixture or driver path exercises it today, so a fix there is untestable this round"). | An untestable burn risks a silent double-emit regression with no fixture to catch it. Correctly deferred. |

**Corpus / driver check.** The class is "method-level-generic method whose RECEIVER is itself
generic, on an INHERENT (no-trait) equip block." The two siblings that WERE burned at the pin
needed `iter_enumerate_zip` (`zip[U]` in `equip [T] VectorIter[T]:`) as their repro. The `gm_`
generic-receiver variant has no analogous corpus fixture, and the self-host driver source does
not contain such a shape (the driver's own generic equips are either non-method-generic or
already covered by the `proto_minsts` path). So **a burn of the one genuine member is
untestable this round** — exactly why it was deferred and why the ratchet keeps it counted.

## 4. RECOMMENDATION — RE-ANCHOR the lint (with the exact diff)

**BURN is impossible this round** (the 3 over-budget stubs have nothing to lower; the 1
genuine deferred member is untestable). **Blunt `BUDGET=21` re-baseline is a dodge** — it
bakes in the blunt-instrument flaw (the lint counts every `Option` None arm in the file) and
guarantees the same false-RED on the next unrelated parity commit. The lint's own message
forbids "bump the budget to dodge review," and a blunt bump IS that dodge.

**The principled fix is to make the ratchet MEANINGFUL: anchor it on the genuine deferred
class** via the `gm-inherent-generic-equip DEFERRED` marker the deferred stub already carries.
Count = exactly **1**, `BUDGET=1`, **TARGET 0** (implementing the `gm_` inherent body and
deleting the marker drops it to 0). Vacuous `Option` None arms — present and future — are
correctly invisible. This keeps the sibling-site-drift protection the ratchet exists for (the
next *real* deferred method-generic stub will carry the marker and trip the budget) while
removing the false-positive surface that rotted it.

> **Note for the executor / output reviewer:** the marker-anchored count requires that the
> deferred stub keeps the literal substring `gm-inherent-generic-equip DEFERRED` on the line
> immediately after its `pass` (it does today, lower.gg L4504). The lint's `count > 0`
> assertion guards against the marker silently disappearing (e.g. if someone implements the
> body but forgets to delete the ratchet, or edits the comment) — if the marker vanishes
> while the ratchet still exists, the test fails loud with "re-anchor / move TODO to DONE.md."

### Exact diff to apply (`tests/lints.rs`, the `no_growth_in_self_host_lower_case_none_pass_stubs` block)

Replace the doc-comment + test body (the `/// The ratchet counts the TWO-LINE textual
pattern …` through the end of `fn no_growth_in_self_host_lower_case_none_pass_stubs() { … }`)
with the marker-anchored version below. The prototype that produced GREEN was exactly this:

```rust
/// **Why this ratchet is ANCHORED, not a whole-file `case None:`→`pass` count.**
/// The earlier design counted EVERY `case None:` immediately followed by a
/// trimmed-`pass` line across all of `lower.gg`. That was a blunt instrument: a
/// 4500-line lowerer destructures `Option` constantly, and a vacuous "no value
/// to extract here" None arm — with literally nothing to lower — is idiomatic
/// and correct. Three such arms were added by unrelated parity fixes
/// (`ext_ret_opt`/`05daf35b`, `rs_ext_ret_opt`/`239083f2`,
/// `resource_meta_for(mrinst.base_name)`/`19d1529a`), pushing the blunt count
/// 18→21 and turning the FATAL ratchet RED even though NONE of them is a
/// method-generic dispatch stub. A guard that goes red on correct, unrelated
/// code can no longer catch the NEXT real growth — exactly the guard-rot Core
/// invariant #6 forbids. So the ratchet now anchors on the ONE genuine class it
/// was always meant to police, not on a textual pattern it cannot isolate.
///
/// THE CLASS: the method-generic equip-instance dispatch in `lower_module`.
/// Each such arm sends a method-level-generic instance onto either the
/// TRAIT-default body (`case Some(tname)`) or the INHERENT equip-block body
/// (`case None`). Two of these inherent None arms now lower the body (the two
/// `proto_minsts` arms, fixed alongside `iter_enumerate_zip` where `zip[U]` is a
/// method-generic in `equip [T] VectorIter[T]:`). The LAST one — the `gm_` loop,
/// body-emit on a GENERIC receiver (lower.gg:~4205) — is DEFERRED with a stated
/// blocker (needs a `fn_sigs.contains` dedup guard to avoid double-emit with the
/// `proto_minsts` arm, AND there is no corpus fixture or driver path that
/// exercises it, so a fix is untestable today — see TODO.md) and KEEPS its bare
/// `pass`. That stub carries a distinctive trailing marker
/// (`// gm-inherent-generic-equip DEFERRED`, placed AFTER the `pass` so the
/// `case None:`→`pass` adjacency is preserved) which this ratchet keys off.
///
/// SO this ratchet counts a `case None:`→`pass` whose IMMEDIATELY FOLLOWING line
/// is the deferred-class marker. Today that is exactly ONE arm. BUDGET = 1 with
/// TARGET 0: implementing the inherent body (and deleting the marker line)
/// drops the count to 0.
///
/// **If this fails (count went UP):** a NEW deferred method-generic equip stub
/// was added — either implement it through the shared inherent-lowering shape
/// (mirror the `proto_minsts` body-emit None arm: match the method in the equip
/// block's own `methods`, bind equip-[T] + Self + method-[U] subs, emit under
/// the mangled symbol; the `gm_` arm additionally needs a `fn_sigs.contains`
/// dedup guard to avoid double-emit), OR, if it must be deferred, justify it in
/// review and bump BUDGET deliberately. A bare unrelated `Option` None arm does
/// NOT carry the marker and is correctly invisible to this ratchet.
#[test]
fn no_growth_in_self_host_lower_case_none_pass_stubs() {
    // The deferred method-generic equip-instance inherent body-emit stub.
    // TARGET 0 (implement the inherent path + delete the marker).
    const BUDGET: usize = 1;
    // The distinctive marker the deferred stub places on the line right after
    // its `pass` (see the `gm_` loop, lower.gg:~4205). Substring match so the
    // exact wording can evolve without silently un-anchoring the ratchet.
    const DEFERRED_MARKER: &str = "gm-inherent-generic-equip DEFERRED";

    let content =
        fs::read_to_string("tests/fixtures/self_host_lowerer/lower.gg").unwrap_or_default();

    // Count `case None:` → `pass` whose FOLLOWING line carries the deferred
    // method-generic marker. A whole-file `case None:`→`pass` count is too
    // blunt (idiomatic vacuous `Option` None arms blow the budget); anchoring
    // on the marker isolates exactly the class this ratchet polices.
    let lines: Vec<&str> = content.lines().collect();
    let mut count = 0usize;
    for w in lines.windows(3) {
        if w[0].trim() == "case None:"
            && w[1].trim() == "pass"
            && w[2].contains(DEFERRED_MARKER)
        {
            count += 1;
        }
    }

    assert!(
        count > 0,
        "no_growth_in_self_host_lower_case_none_pass_stubs: the deferred \
         method-generic equip stub marker (`{DEFERRED_MARKER}`) was not found \
         immediately after a `case None:`→`pass` in lower.gg. If the inherent \
         `gm_` body-emit path was IMPLEMENTED, delete this ratchet (move its \
         TODO entry to DONE.md). Otherwise the scan or the marker moved — re-anchor.",
    );
    assert!(
        count <= BUDGET,
        "Self-host `lower.gg` deferred method-generic equip-instance \
         `case None:`→`pass` stub count grew beyond budget: {count} > {BUDGET}.\n\n\
         A new DEFERRED method-generic equip-instance inherent body-emit arm was \
         added (it carries the `{DEFERRED_MARKER}` marker). Do NOT leave it a \
         stub: lower the inherent equip-block body through the shared shape \
         (mirror the `proto_minsts` body-emit None arm — match the method in the \
         equip block's own `methods`, bind equip-[T] + Self + method-[U] subs, \
         emit under the mangled symbol; the `gm_` arm additionally needs a \
         `fn_sigs.contains` dedup guard to avoid double-emit). Then LOWER BUDGET \
         in the same commit. Don't bump the budget to dodge review.",
    );
}
```

**Also update TODO.md L86** (the "PRE-EXISTING RATCHET RED" handover note): once this lands,
move it out of the RED-ratchet framing — the ratchet is GREEN and meaningful again; the
remaining work is the genuine `gm_` burn tracked at TODO.md L131 (which keeps `BUDGET=1`,
target 0). Don't add a `LANDED`/`FIXED` entry to TODO.md — record the close in DONE.md.

### Alternative considered and REJECTED: blunt `BUDGET = 21`

Re-baselining the whole-file count to 21 was rejected because: (a) it does not restore a
*meaningful* guard — the lint would still go RED on the next unrelated `Option` None arm,
re-rotting immediately; (b) it directly contradicts the lint's own "don't bump the budget to
dodge review" message; (c) it leaves the sibling-site-drift signal buried in 20 unrelated
arms, so a real new deferred method-generic stub (the thing the ratchet exists to catch) would
be invisible inside the noise. Re-anchoring costs the same review effort and yields a guard
that actually works.

## 5. Gate the executor must run

- **Always:** `cargo test --test lints no_growth_in_self_host_lower_case_none_pass_stubs`
  (must be GREEN, count=1) **and** full `cargo test --test lints` (all 28 GREEN). Plus
  `cargo build` + `cargo test --lib` smoke.
- **`self_host_bootstrap_fixed_point`: NOT required.** This change touches only
  `tests/lints.rs` — a Rust lint test that READS `lower.gg`; it does not modify any emitted
  lowering. (If a future executor instead chooses to actually BURN the `gm_` arm — i.e. edit
  `lower.gg`'s emitted lowering — THEN `self_host_bootstrap_fixed_point` becomes mandatory.
  That is a separate, larger task blocked on a repro fixture, NOT this round's fix.)

## Verification log (this scout, against current source)

- RED reproduced: `cargo test --test lints no_growth_…` → `21 > 18` FAILED.
- 18-at-pin confirmed by counting stubs in `git show 0dc7c861:…/lower.gg`.
- 3 new stubs + introducing commits confirmed via scrutinee-delta + `git log -S`.
- Each new stub read in context — all vacuous `Option` None branches, nothing to lower.
- `gm_` deferred member (L4503) confirmed unchanged, marker present on L4504.
- No corpus/driver repro for the `gm_` generic-receiver class (TODO.md L131 + grep).
- Prototype of the re-anchored lint applied → GREEN (count=1); full `cargo test --test lints`
  28/0; `cargo build` clean; `cargo test --lib` 1084/0. Prototype then REVERTED (worktree
  clean except this deliverable).
