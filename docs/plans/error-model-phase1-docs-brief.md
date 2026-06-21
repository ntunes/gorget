# Brief — Error-model Phase-1 documentation (docs follow-up (d) + (B2))

**Track:** Error-model Phase-1 follow-up (d). Document the SHIPPED local fault-catch behavior
in the three user-facing docs (`language-design.md`, `book/10-errors.md`,
`language-reference.md`), PLUS the owner-decided B2 minimal clarification to `error-model.md`
(one §4 sentence + neutralizing three stale reframe-anticipation notes). **Four files total**
— see "File 4" below.

**B2 OWNER DECISION (2026-06-21):** the §1/§4 "two channels" framing is KEPT — "Minimal:
clarify, don't restructure." Do NOT restructure §1/§4. The ONLY `error-model.md` edit is ONE
clarifying sentence added to §4 (File 4). The "one typed error channel, two kinds" framing
is unchanged.

**Executor constraints:** docs-only. No source edits, no fixtures, no build. Land on a
worktree branched from `gorget-1` (`git merge --ff-only gorget-1` first). `git add` ONLY the
FOUR doc files named below (three user-facing + the one-sentence `error-model.md` edit). Keep
EVERY example **lexical/local** — Phase-2 deep/boundary catch is NOT shipped and must not be
implied anywhere.

---

## Ground truth (what shipped — verify against these before writing)

Phase 1 fully landed: Increment 1 (`8ab75635`, Overflow + DivByZero) + Increment 2
(`a447c726`, Bounds + Div-split + qualifier-validation + plain-op INT_MIN/-1 fix).

**Surface syntax** (verify against the fixtures, do not trust this brief's spelling):
- Pattern form: `int r = (a * b) catch Fault.Overflow: -1` — catches one variant, binds nothing.
- Binding form: `int r = (a * b) catch f: match f:` then `case Fault.Overflow(): …` — binds the constructed `Fault`.
- Variants are **qualified-only**: `Fault.Overflow`, `Fault.DivByZero`, `Fault.Bounds`.

**`Fault` variant set = exactly `Overflow`, `DivByZero`, `Bounds`.** OOM is OUT (Phase 2).

**Semantics:**
- Panic-by-default preserved — uncaught overflow/div0/bounds still `exit(1)`.
- **Local (lexical) catch only** — catches faultable ops in the wrapped expression's OWN
  basic blocks; a fault inside a *called function* still panics (Phase 2).
- `INT_MIN / -1` and `INT_MIN % -1` are `Fault.Overflow` (not DivByZero). Partial-catch
  (`catch Fault.DivByZero` on `INT_MIN/-1`) does NOT catch → panics.
- Exhaustiveness via implicit-panic-default over the **closed** `Fault` enum only (every
  OTHER enum stays strictly exhaustive); an unnamed `Fault` variant falls through to panic.
- Wrong qualifier (`Bogus.Overflow`) is a typecheck error.
- NO `equip Error` on `Fault` this phase (that's Phase 2's `dyn Error` surface).
- `--overflow` flag RETIRED (`fb2e5037`): plain `+`/`-`/`*` always check; `+%`/`-%`/`*%`
  (and `+%=`/`-%=`/`*%=`) are the ONLY wrapping escape; `directive overflow=wrap` →
  `UnknownDirective` error. No global wrap mode exists.

**Fixtures to read first** (in `tests/fixtures/`): `fault_catch_overflow.gg`,
`fault_catch_binding.gg`, `fault_catch_bounds.gg`, `fault_catch_intmin_div.gg`,
`fault_catch_bad_qualifier.gg` (negative), `fault_panic_default.gg`,
`fault_intmin_partial.gg`. Copy the EXACT surface syntax from these — the brief's snippets
are illustrative, the fixtures are authoritative.

**Parser AST (for the grammar production):** `src/parser/ast.rs:600`
`Expr::FaultCatch { expr, pattern, handler }`; `FaultCatchPattern` =
`Variant { qualifier, variant }` | `Binding(name)`. `catch` is the lowest-BP infix
(shared with contract `catch`/`rethrow`), so `a*b catch …` parses as `(a*b) catch …`.

---

## Curation decisions (resolved — do NOT re-litigate; reviewers may challenge with cause)

1. **`Fault` type reference placement** → in `language-reference.md` §10 (Error Handling),
   for discoverability (it's a compiler-internal closed enum, not a normal user type).
2. **Lexical-reach footgun in the book** → state the basic-block rule plainly, WITH the
   called-function caveat in one sentence (`(compute()*2) catch Fault.Overflow:` catches the
   `*2`, not an overflow inside `compute()`). Users need the precision; don't hand-wave it.
3. **`language-design.md` §6 rule-of-thumb** → ADDITIVE carve-out beside the existing
   "caller can prevent → panic" rule. Panic-by-default still governs the *uncaught* case
   (that's what shipped); recovery is opt-in. Do NOT delete/invert the rule — annotate it.
4. **Scope = four files:** `docs/language-design.md`, `docs/book/10-errors.md`,
   `docs/language-reference.md`, and a ONE-SENTENCE clarification to
   `docs/plans/error-model.md` §4 (File 4 — the B2 minimal edit). NOT devbook/internals,
   nothing else in error-model.md.

## Pre-existing doc state to RECONCILE (review pass-1 heads-ups — don't duplicate/contradict)
- **`language-reference.md` ~:1538** already NAMES `Fault` (`catch Fault.Overflow` in the
  wrapping-operators prose) and already says there's no global wrap mode. The new §10 `Fault`
  subsection is ADDITIVE — don't duplicate or contradict line 1538.
- **`language-design.md` ~:211-213** is already Phase-1-aware (`catch Fault.Overflow`,
  per-operator wrapping, no global wrap mode). The §2.2 rewrite must RECONCILE/dedupe with
  these lines — do NOT treat §2.2 as flatly "panics on overflow" and re-add what's there.
- **Bounds scope wording:** `Fault.Bounds` fires for `CollectionKind::Array`, which also
  includes `Deque`. Prefer "indexed array-backed collections (Vector/Deque)" over bare
  "array/Vector" for precision.

---

## File 1 — `docs/language-design.md`

### §2.2 Integer Overflow (~lines 189-213)
- Reframe the flat "panics on overflow" prose to **additive**: panics **by default** in debug
  AND release (the safety rationale stays), AND is now **locally recoverable** via
  `catch Fault.Overflow`. Forward-point to §6 and the book's error chapter.
- **Sweep the stale dead example (lines ~198-209):** it imports a NON-EXISTENT module
  (`import std.math.wrapping` / `wrapping.add(x, 1)` — there is no `lib/std/math/wrapping.gg`,
  zero `wrapping` refs in `lib/std/math.gg`). Replace with the real per-operator `+%` form
  (already shown just below at ~207-208, so the stale lines are pure dead weight). Note: there
  is **no wrapping division** (`INT_MIN/-1` is an overflow; div-by-zero panics).
- Target shape (adapt to current text):
  > Integer arithmetic **panics on overflow by default**, in debug and release — this catches
  > the silent-corruption bugs C/Go ship. The panic is recoverable locally: wrap the
  > expression in `catch Fault.Overflow` to compute a fallback instead of aborting (§6; *The
  > Gorget Book*, Error Handling). For unconditional wrapping use the per-operator `+%`/`-%`/`*%`
  > (and `+%=`/`-%=`/`*%=`) — Zig-style. There is no global "wrap the build" mode and no
  > wrapping division.

### §6 Panic vs Result (~lines 1295-1312)
- The list at ~1298 flatly files "integer overflow, division by zero, out of bounds" under
  Panic. Insert, after the Panic list (~1303), a paragraph introducing the **recoverable-fault
  class** (closed `Fault` enum: `Overflow`, `DivByZero`, `Bounds`; lexical recovery; uncaught →
  panic, so the default is unchanged). Keep contract-error/Result side untouched.
- Amend the rule-of-thumb at ~1312 additively (decision #3): panic-by-default still holds; the
  fault class lets a caller **recover at the point of use** (not *prevent*), opt-in.

## File 2 — `docs/book/10-errors.md`
- **Opening two-category split (~lines 7-14):** soften the "programmer errors… panic
  immediately" bullet — overflow/bounds/div0 still panic by default BUT are locally
  recoverable via `catch Fault.X` (forward-point to the new section).
- **NEW section "Recovering from Faults"** (place within the Panics material, ~after the
  Integer Overflow subsection ~line 535). Teach: the closed `Fault` enum (3 variants);
  pattern form; binding form (`catch f: match f`); **lexical reach + the called-function
  caveat** (decision #2); panic-by-default still holds (unnamed variant → panic); qualified
  spelling; and an **explicit contrast with the contract `catch (e):`** form taught earlier
  at ~169-195 (faults are in NO signature, need no `throws`, work on a bare expression).
- **§"Panics are for programmer errors" (~481-502):** add one sentence pointing to the new
  fault-recovery section so the reader doesn't conclude faults are only fatal.
- **Summary table (~608-620):** add a row `(expr) catch Fault.X: fallback` | recover a fault
  (overflow/div0/bounds) locally | at the faulting expression — distinct from the existing
  contract `catch (e):` row.

## File 3 — `docs/language-reference.md`
- **§10.5 Catch (~2441-2476):** currently only the contract `catch (e):` form. Add a
  clearly-delimited **Fault catch** subsection: the two spellings (pattern + binding), the
  lexical/basic-block reach, no-`throws`/unrelated-to-`Result`, wrong-qualifier = compile
  error. Keep the contract form's text intact.
- **`Fault` type reference (NEW, §10):** a short subsection defining the closed
  compiler-internal enum — `Fault.Overflow` (incl. `INT_MIN/-1` and `INT_MIN%-1`),
  `Fault.DivByZero`, `Fault.Bounds`; qualified-only; panic by default, `catch Fault.X`
  recovers locally; NOT part of any signature or the `Result` channel.
- **Appendix A grammar (~6534):** extend `catch_expr` to cover the fault forms. Render the two
  AST cases faithfully — `Variant{qualifier,variant}` = the `Fault.X` path (un-parenthesized);
  `Binding(name)` = bare identifier (un-parenthesized); the existing `"(" IDENTIFIER ")"` is
  the contract form. Example shape:
  ```
  catch_expr    = expr "catch" ( "(" IDENTIFIER ")" | fault_pattern ) ":" expr ;
  fault_pattern = path | IDENTIFIER ;   (* Fault.X variant path, or binding name *)
  ```
- **Bounds note:** where indexing is specified, note that a raw `xs[i]` out-of-bounds panics
  by default but is locally catchable via `catch Fault.Bounds` (array/Vector element reads
  only — dict-get / string-index / range-slice are NOT Bounds-catchable, per the Inc-2 brief).

## File 4 — `docs/plans/error-model.md` §4 (the B2 minimal edit — owner-decided)
- Do NOT restructure §1 or §4. Keep the "one typed error channel, two kinds" framing intact.
- Add exactly ONE clarifying sentence to §4 (the "two kinds, side by side" section), to the
  effect of: *"Note: faults, unlike contract errors, are not carried in a function's signature
  — they are recovered locally via `catch Fault.X`. The 'two kinds' framing is unchanged."*
  (Adapt wording to fit §4's prose; it is a clarification, not a reframe.)
- ALSO neutralize **ALL THREE** now-stale forward-notes that anticipate a "§1/§4 'one channel
  / two kinds' → two channels reframe" (from Q14's follow-up). Neutralizing only one leaves the
  doc self-contradictory (review pass-3, "fix the class not the instance"). The owner decided
  NOT to reframe ("minimal: clarify, don't restructure"), so each anticipation is now FALSE.
  Apply the SAME minimal one-line correction at each — state the framing is KEPT and the reframe
  is not happening (the §4 clarification notes faults are out-of-signature). These are caveat
  SENTENCES, not §1/§4 table/structure — do NOT restructure §1 or the §4 table. The three sites:
  - `error-model.md` ~:422-426 (the §9 Q14 follow-up — the origin note).
  - `error-model.md` ~:510 (the §9.1 Phase-2 block tail).
  - `error-model.md` ~:676-679 (the §11.4 "framing changes that land WITH Phase 1" checklist —
    this one even asserts "the reframe lands with Phase 1 / edits §1-§8"; that is outright false
    post-decision, so it MUST be corrected).
- Nothing else in `error-model.md` changes (no §1/§4 restructuring; §1 and the §4 table keep
  the "one channel, two kinds" framing).

---

## Acceptance bar (output-review against this)
- Every claim in the docs matches a fixture or shipped semantic above; nothing describes
  Phase-2 (deep/boundary catch, `equip Error`, OOM) as shipped.
- No surviving `--overflow` flag / `directive overflow` / `std.math.wrapping` references.
- All examples lexical/local. Surface syntax matches the fixtures verbatim (qualified
  `Fault.X`, the two forms).
- `error-model.md` §1/§4 NOT restructured — only the single §4 clarifying sentence added
  (File 4). Four files total changed.
- No duplication/contradiction with the pre-existing `Fault` mentions
  (`language-reference.md` ~:1538, `language-design.md` ~:211-213).
- Line-number cites in this brief are approximate — the executor MUST re-locate each section
  in the current file (they drift), not trust the numbers.
