# Track B (T4) — `gg check` rejects unknown type names

## Goal
`gg check` currently SILENTLY ACCEPTS unknown type names, defaulting them to
`unit`. `Floobar x = 5` (undefined) → "OK: no semantic errors"; `u8 n = 2`
(`u8` isn't a keyword — only `uint8`/`byte` are) → the same path → C
"void value not ignored". Make an undefined type name a HARD semantic error.
Reference-grade correctness fix; a typo footgun closed. Do NOT add Rust-style
shorthand keywords (`u8`/`i8` — a language-design call, owner's).

## Root cause (scout a046dc03)
`src/semantic/types.rs:466-470`: for an unknown `Type::Named`, `ast_type_to_resolved`
returns `Ok(types.error_id)` instead of `Err` → the ~12 call sites that swallow it
(`.ok()`, `.unwrap_or(error_id|void_id)`) never surface anything → the LIR layer
defaults to `UNIT_TYPE` (`src/lir/lower/mod.rs:~1686`). `UndefinedName`
(`src/semantic/errors.rs:181`) already exists as a usable diagnostic kind.

## ⚠ BLAST RADIUS — the load-bearing risk (scout's "zero blast radius" was NOT verified)
The scout's empirical check was flawed (it ran `gg check` on all fixtures as one
arg-list). The real risks:
1. **Cross-module forward refs.** The resolve pass runs BEFORE all modules are
   merged; a type can be "unknown" on first sight and defined later. There's a
   fixup (`resolve.rs:~270-299`) but it is **RETURN-TYPE-ONLY** (`collect_top_level`).
   So hard-erroring at the RESOLVE-pass sites (params/throws/extern,
   `resolve.rs:376/389/544/572/583/624/644/650`) could SPURIOUSLY error on a
   cross-module forward ref. **Therefore fire the error at the TYPECHECK pass**
   (runs late, after all collection/resolve — every defined type, incl. cross-module,
   is in scope): the VarDecl site `typecheck.rs:~3008` + sibling typecheck-pass
   declaration sites. Keep the resolve-pass sites swallowing (forward-ref tolerance).
2. **Generic params.** `T`/`K`/`V` are registered as `DefKind::GenericParam` at
   collection time (`resolve.rs:~914/979`), so they're in scope and WON'T hit the
   unknown path. Confirm — a spurious error on a generic param is a blocker.

## Approach
Prefer: at the TYPECHECK-pass declaration sites, when `ast_type_to_resolved`
returns the unknown-type signal, surface `UndefinedName { name }` (with an
optional "did you mean `uint8`?" hint for `u8`/`i8`/`u16`/…). Either (A) make the
`types.rs:466` chokepoint return `Err` and have ONLY the typecheck-pass sites
surface it (resolve-pass sites keep `.ok()`), or (B) add a dedicated unknown-type
check at the typecheck declaration sites. Choose whichever keeps resolve-pass
forward-ref tolerance intact. TYPED (no name-matching).

## THE GATE (non-negotiable — this is how we know blast radius is zero)
Before/after `gg check` sweep, PER-FIXTURE (not one arg-list):
- Run `gg check` on EVERY `tests/fixtures/*.gg` + EVERY self-host driver source
  (`tests/fixtures/self_host_*/*.gg`) on the CURRENT tree → record which PASS.
- Apply the fix, re-run → **no fixture that currently PASSES check may newly FAIL.**
- `self_host_bootstrap_fixed_point` MUST stay green — if any self-host driver
  source has a latent unknown-type-as-unit, the fix breaks the bootstrap → STOP,
  report it (it's a real self-host bug to fix separately, or the fix needs scoping).

## Zone + disjointness
`src/semantic/` ONLY (`types.rs` + the typecheck-pass call sites). Fully disjoint
from Track A (`lower_expr.gg`), DOC, CLEANUP.

## Gates
`cargo build`, `cargo test --lib`, `cargo test --test lints`, the before/after
`gg check` sweep (the load-bearing one), `self_host_bootstrap_fixed_point`, +
a new negative fixture `unknown_type_error.gg` (`Floobar x = 5` → error;
`u8 n = 2` → error w/ the hint) wired via `check_gg_fails`. Then the parent's
full integration.

## Honesty clause
If the before/after sweep shows ANY legitimate currently-passing fixture newly
failing (forward-ref or otherwise), do NOT ship the broad version — scope tighter
(typecheck-pass only, exclude the failing pattern) and report what failed and why.

## Brief-review pass-1 RE-SCOPE (aaa86c6d — DO NOT proceed on the broad version)
Pass-1 refuted the scout's "clean, all-sites" framing with two real findings:
1. **Generic-param timing.** Function PARAMETER types are resolved in `collect_item`
   (Pass 1, `resolve.rs:~372-384`) BEFORE generic params are registered in scope
   (`resolve_function`, Pass 2, `resolve.rs:~911-920`). So a hard error at the
   resolve-pass param/field sites would SPURIOUSLY fire on legit generics like
   `T foo[T](T a)` (the `T` isn't in scope yet at collect_item). `numeric_trait_ops.gg`
   currently works — almost certainly because such unresolved generics silently
   become `error_id` today and that's benign downstream. Hard-erroring there breaks them.
2. **Typecheck-pass-only is INSUFFICIENT for full coverage:** it catches VarDecl
   annotations but MISSES param types (resolve pass) and struct/enum FIELD types
   (`populate_def_field_types` pass) — both resolved before typecheck.

**RE-SCOPED PLAN (the shippable, SOUND subset):** fire the error ONLY at the
**typecheck-pass VarDecl site** (`typecheck.rs:~3008`). This:
- Catches the REPORTED footgun (`Floobar x = 5`, `u8 n = 2` — both VarDecls).
- Is SAFE: by the typecheck pass, ALL types are in scope — cross-module types
  (resolve fixup done) AND the enclosing fn's generic params (registered in Pass 2)
  — so `T x = …` inside `fn foo[T]` resolves fine; no spurious generic/forward-ref error.
- Is verified by the before/after per-fixture `gg check` sweep (must be 0 newly-failing).

**FILE AS FOLLOW-UP (do NOT attempt this round — deeper semantic-ordering work):**
unknown-type errors at PARAM and struct/enum FIELD positions. That needs the
generic-param-timing fixed (register generics before param resolution, or gate the
error on "not a possible generic param") + cross-module forward-ref tolerance at the
resolve pass. Also investigate/confirm the `numeric_trait_ops.gg` mechanism (do generic
param types currently resolve to `error_id` silently?). This is a real second footgun,
but it's a project, not a slot.
