# Parity Triage Scout — 2026-06-24

**Scout:** parity scout, base = gorget-1 tip `57b44418`.
**Mission:** triage three candidate WRONG-OUTPUT / drop-the-branch self-host parity bugs
(A `match_nested_enum`, B string range-slice, C inclusive-range `for i in 0..=3`),
RUN-confirm each, prototype the narrowest fix, MEASURE the corpus-level flip.

**Method:** built Rust gg + the self-host driver (`tests/fixtures/self_host_lowerer/driver.gg`),
emit+cc+ran each fixture through the self-host vs the live `gg run` oracle, and re-measured
the FULL `self_host_runtime_diff` PARITY before/after the prototype.

---

## TL;DR — one clean confirmed win, two mirages

| Candidate | RUN-confirmed bug? | Fix prototyped? | Corpus flip (measured) |
|-----------|-------------------|-----------------|------------------------|
| **A — nested enum sub-pattern** | **YES** (run-confirmed both outputs) | **YES** | **+2 MATCH, 0 regressions** ✅ |
| B — string range-slice `s[a..b]` | **NO — MIRAGE** | n/a | already MATCH on current self-host |
| C — inclusive-range `for i in 0..=3` | **NO — MIRAGE** | n/a | already MATCH on current self-host |

**Measured corpus PARITY (force-rebuilt driver, full `self_host_runtime_diff`):**
- BEFORE (clean tip): **754 / 1071 = 70.4%** — WRONG-OUTPUT 90, CC-FAIL 194, CRASH 33.
- AFTER (Candidate A prototype): **756 / 1071 = 70.6%** — WRONG-OUTPUT 88, CC-FAIL 194, CRASH 33.
- **Delta = +2 MATCH, exactly 2 WRONG-OUTPUT cleared, ZERO new WRONG / CC-FAIL / CRASH (no mirage).**

The two flipped fixtures: **`match_nested_enum`** and **`snag42_scrutinee_move_inside_arm`**
(the latter is a free bonus — same nested-`PConstructor`-inside-`PConstructor` shape:
`case C.Normal(V.NumberV(n)):`).

**RECOMMENDATION: brief an executor on Candidate A.** It is a clean, bounded, reference-grade
win: a single sub-pattern arm in one self-host file, mirroring the Rust reference, +2 corpus MATCH,
snapshot-lock-in green, zero regressions. B and C are mirages — the bugs they cite do not reproduce
on the current self-host (do NOT brief them).

---

## Candidate A — `match_nested_enum` nested sub-pattern dropped — **CONFIRMED, FIX WORKS**

### Bug (RUN-confirmed, both outputs captured)

Fixture: `tests/fixtures/match_nested_enum.gg`. It matches `Option[Color]` / `Option[Shape]`
with nested constructor sub-patterns (`case Some(Color.Green()):`, `case Some(Shape.Rect(w, h)):`).

```
                Rust gg (oracle)        self-host (BEFORE)
                ----------------        ------------------
                some red                some red
                some green       →      some red       ← WRONG (inner Color discriminant dropped)
                no color                no color
                circle r=5       →      circle r=0     ← WRONG (inner Shape discriminant + field dropped)
                rect 3x4         →      circle r=0     ← WRONG
                ok: 42                  ok: 42
                err: oops               err: oops
```

The nested `Color.Green()` / `Shape.Rect(w,h)` sub-pattern's inner discriminant is silently
dropped → the FIRST arm of each match always wins, and the nested payload field is never read
(`r` binds 0).

### Root cause (exact post-split site)

`tests/fixtures/self_host_lowerer/lower_match.gg`, function **`lower_ctor_pattern`**
(the lowerer was split — the cited `lower_match.gg:792` is now the sub-pattern binding loop
inside `lower_ctor_pattern`, which starts at `:578`). The sub-pattern `match sp.pat:` loop
(`:676`–`:794`) handles `PWildcard` / `PBinding` / `PLiteral`, but a nested
`PConstructor` / `PDotShorthand` hit the `else: lower_fail(...)` arm at **`:792-793`**,
which emits no discriminant check and no field bind — then the loop falls through to the
unconditional `set_terminator(&ctx, GTJump(match_bb))` at `:795`. The arm matches unconditionally
and the nested payload is never destructured.

### Rust reference

`src/ir/lowering/stmts/patterns.rs`, **`lower_pattern_condition`** (`:541`), the
`Pattern::Constructor` arm (`:609-730`):
- `:644-648` `has_nested` — detects any sub-pattern that is not a plain bind/wildcard/rest.
- `:654-727` short-circuit: only when the OUTER tag matches, read each non-trivial sub-pattern's
  field with **`enum_field_load_borrow`** (`:696`, the Snag #34 / `EnumFieldLoadMode::Borrow`
  rationale — the condition read must NOT zero the source, else the binding loop sees zeros),
  then **recurse** `lower_pattern_condition(ctx, builder, field_pat, field_local, field_type)`
  (`:702`) and AND the results.

Rust separates `lower_pattern_condition` (recursive discriminant test) from
`emit_pattern_bindings` (`:796`); the self-host interleaves both in `lower_ctor_pattern`.
The fix adapts the Rust recursion to the self-host's interleaved layout by recursing back
into `lower_ctor_pattern` itself (which does both the tag check AND the binding) on the payload
field as the new scrutinee.

### Prototype diff (the actual landed prototype, run-verified)

In `lower_ctor_pattern`'s sub-pattern loop, replace the `else: lower_fail` with explicit
nested-constructor arms BEFORE the `else`:

```gorget
            case PConstructor(sub_variant, sub_sub_pats):
                # Nested constructor sub-pattern (`case Some(Color.Green()):`,
                # `case Some(Shape.Rect(w, h)):`). Mirrors Rust's recursive
                # `lower_pattern_condition` (patterns.rs:609-730): read the
                # outer variant's payload field, then recurse on the INNER
                # constructor — checking its discriminant AND binding/checking
                # its own sub-patterns. Read the payload as a BORROW
                # (borrow_all=true) — non-destructive, Snag #34 rationale.
                # Pass "" as the inner enum name so lower_ctor_pattern
                # re-infers from the field local's type (NOT the OUTER enum).
                int nctor_fty = lookup_ctor_field_type(effective_enum, effective_variant, fi, &gmod)
                int nctor_field_val = emit_payload_read_mode_full(&ctx, scrutinee_ptr, effective_enum, effective_variant, fi, nctor_fty, &gmod, false, true)
                int nctor_continue_bb = new_block(&ctx)
                lower_ctor_pattern(&ctx, nctor_field_val, sub_variant, sub_sub_pats, "", nctor_continue_bb, no_match_bb, &gmod)
                switch_to(&ctx, nctor_continue_bb)
            case PDotShorthand(sub_variant, sub_sub_pats):
                # `.VariantName(pats)` nested sub-pattern — same recursion.
                int ndot_fty = lookup_ctor_field_type(effective_enum, effective_variant, fi, &gmod)
                int ndot_field_val = emit_payload_read_mode_full(&ctx, scrutinee_ptr, effective_enum, effective_variant, fi, ndot_fty, &gmod, false, true)
                int ndot_continue_bb = new_block(&ctx)
                lower_ctor_pattern(&ctx, ndot_field_val, sub_variant, sub_sub_pats, "", ndot_continue_bb, no_match_bb, &gmod)
                switch_to(&ctx, ndot_continue_bb)
```

Why each choice:
- **`emit_payload_read_mode_full(..., borrow_only=false, borrow_all=true)`** — reads the inner
  enum value as a LoBorrowed alias into the outer scrutinee's payload, eliding the clone. The
  nested tag/field reads are read-only; cloning here would leak (mirrors Rust `EnumFieldLoadMode::Borrow`).
- **enum name `""`** — `lower_ctor_pattern` re-infers the inner enum from the field local's typed
  id (its scrutinee-type fallback `:611-625`) AND from the qualified `Color.Green` variant name
  (`:589-595`). Reusing the OUTER enum name would mis-resolve.
- **`nctor_continue_bb` + recurse with `match_bb=continue_bb`, `no_match_bb=outer no_match_bb`** —
  the inner `lower_ctor_pattern` branches to `continue_bb` on a nested match and to the outer
  `no_match_bb` on a nested mismatch; the loop resumes at `continue_bb`. This composes the
  short-circuit exactly like the `PLiteral` arm's `lit_continue_bb` (`:789-791`).

### Measured result (force-rebuilt driver, end-to-end)

- `match_nested_enum`: WRONG-OUTPUT → **MATCH** (byte-identical to Rust oracle, confirmed via
  single-fixture emit+cc+run+diff).
- `snag42_scrutinee_move_inside_arm` (`case C.Normal(V.NumberV(n)):`): WRONG-OUTPUT → **MATCH**
  (free bonus, same shape).
- Full corpus: **754 → 756 (70.4% → 70.6%), +2 MATCH, 0 regressions** (WRONG-OUTPUT 90→88,
  CC-FAIL 194→194, CRASH 33→33; no fixture newly entered WRONG/CC-FAIL/CRASH).
- `self_host_runtime` lock-in net: **GREEN** (2 passed, 0 failed — no snapshot regression).

---

## Candidate B — string range-slice `s[a..b]` — **MIRAGE (already MATCH)**

The handover (TODO.md:106 item 4b) claims `s[a..b]` "now routes to single-index/first-char
instead of `gorget_str_slice`", affecting `string_indexing` / `str_codepoint_index` /
`cow_lazy_w3c_named_bind`.

**RUN result on the current self-host: all three already MATCH the Rust oracle.**

```
[string_indexing]            MATCH
[str_codepoint_index]        MATCH
[cow_lazy_w3c_named_bind]    MATCH
```

The string-index round (`d10d1a1d`, DONE) already added the String EIndex arm; the range-slice
sub-claim does not reproduce. The corpus run confirms none of these is in the WRONG-OUTPUT backlog.
**Do NOT brief this** — the bug is gone. (If a regression reappears later, re-RUN before treating
the TODO note as live; the citation is stale.)

## Candidate C — inclusive-range `for i in 0..=3` — **MIRAGE (already MATCH)**

The handover claims `for i in 0..=3:` doesn't bind `i` → self-host prints garbage (`inc:5`).

**RUN result on the current self-host: `range_operations` already MATCHes** — the inclusive
range correctly prints `inc:0 inc:1 inc:2 inc:3` (and the half-open + negative ranges).

```
[range_operations]           MATCH
```

The `..=` lowering works. **Do NOT brief this** — the bug does not reproduce.

---

## Briefable target for the executor

**Brief Candidate A.** Single self-host file, one sub-pattern arm, mirrors the Rust reference,
+2 corpus MATCH, snapshot-safe, zero regressions. Reference-grade (matches Rust gg behavior;
no reference defect — Rust gg is correct on all three fixtures).

**Fix site:** `tests/fixtures/self_host_lowerer/lower_match.gg`, `lower_ctor_pattern`
sub-pattern loop, the `else: lower_fail` at `:792-793` (replace with the two nested-ctor arms above).

**Required executor gates (orchestrator drives the full integration sweep):**
1. **`self_host_runtime`** snapshot lock-in (default-running, build-breaking) — **scout confirmed GREEN.**
2. **`self_host_bootstrap_fixed_point`** — REQUIRED because this changes the self-host lowerer.
   The scout confirmed the driver self-compiles cleanly (the precondition) but did NOT run the
   full ~6-min fixed-point gate; the executor MUST run it (the self-host's own AST matches use
   nested constructor patterns sparingly, but the gate is non-negotiable for any lowerer change).
3. **`lowerer_comparison`** + **`c_emit_comparison`** — diagnostic-always-pass; re-print counts
   to confirm no fn-shape regression.
4. **Full `cargo test --test integration -- --test-threads=4`** — the parent's job.
5. Re-run `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture` and confirm PARITY ≥ 756/1071 with no newly-worse fixture.

**TODO.md bookkeeping:** TODO.md:106 item 1 (`match_nested_enum` RESIDUAL) is resolved by this
fix; move it to DONE.md on landing. The string-range-slice (item 4b tail) and inclusive-range
sub-claims are stale — re-RUN before acting; they currently MATCH.
