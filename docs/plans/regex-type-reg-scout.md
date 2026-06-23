# Scout: regex type-registration gap (`GorgetRegexMatch` undeclared)

**Throwaway prototype — DO NOT INTEGRATE to gorget-1.** Report-only.

Tip scouted: `12d699cd` (gorget-1). Worktree branch: `worktree-agent-a5205d76d4da87845`.

## TL;DR

The cluster (`regex_basic`, `regex_corpus`, `regex_extended`) splits into **TWO
independent bugs**, both required to flip to MATCH:

1. **(IN SCOPE, FIXED)** A stale runtime-type-name registration in the self-host
   `lir_codegen.gg` renames the user structs `Regex`/`Match` to the non-existent
   runtime types `GorgetRegex`/`GorgetRegexMatch` AND skips emitting their
   typedefs → `error: unknown type name 'GorgetRegexMatch'`. Fixed by retiring
   the stale entries, mirroring Rust gg (which already removed them when regex
   became pure-Gorget). **Proven end-to-end: this error class is eliminated for
   all 3 fixtures.**

2. **(OUT OF SCOPE — DEEPER, the boundary)** Once (1) is fixed, the C compiles
   further and exposes a *previously-masked* bare-variant-resolution bug: the
   self-host resolves bare `Empty()` (an `RxNode` variant in `lib/xtd/regex.gg`)
   to `ParseError.Empty` (an unrelated `lib/std/conv.gg` enum that also has a
   tag-0 `Empty` variant), via a **name-based, first-write-wins**
   `enum_variant_parent_idx` that ignores the expected/return type →
   `error: incompatible types when assigning to type '__gg_RxNode' from type
   '__gg_ParseError'`. All 3 fixtures hit ONLY this residual after fix (1).

**Parity delta from fix (1) alone: +0** (724/1066 → 724/1066). The 3 fixtures do
not flip because bug (2) still CC-FAILs them. Fix (1) is nonetheless **correct,
reference-grade, regression-free, and a prerequisite** — it is necessary but not
sufficient. Bug (2) needs its own scout/brief/review cycle.

## 1. Reproduce-before (all 3, current tip `12d699cd`)

Self-host emit C → `cc -O0 -w … -lm -lpthread`:

```
regex_basic.c:3067:5: error: unknown type name 'GorgetRegexMatch'
regex_basic.c:3073:5: error: unknown type name 'GorgetRegex'
regex_corpus.c:3071:5: error: unknown type name 'GorgetRegexMatch'  (+ GorgetRegex)
regex_extended.c:3067:5: error: unknown type name 'GorgetRegexMatch' (+ GorgetRegex)
```

The undeclared reference, emitted C (`regex_basic.c:3064`):

```c
// Option__Match
struct Option__Match {
    int32_t tag;
    GorgetRegexMatch Some_0;   // <-- GorgetRegexMatch never typedef'd
};
// Result__Regex__GorgetString
struct Result__Regex__GorgetString {
    int32_t tag;
    GorgetRegex Ok_0;          // <-- GorgetRegex never typedef'd
    Str Error_0;
};
```

## 2. Root cause (bug 1) — `file:line`

`GorgetRegexMatch` / `GorgetRegex` are **not runtime types** — regex is now pure
Gorget (`lib/xtd/regex.gg`, `struct Regex` @1533, `struct Match` @1536). Rust gg
deliberately removed regex from its runtime-name map:

- **Rust (CORRECT, mirror target):** `src/backend/c_lir/mod.rs:210 lir_to_runtime_name`
  — the `Regex`/`Match` arms are GONE; only a comment at `:229-231` records the
  removal. So `Regex`/`Match` fall through (`_ => None`) to the
  `__gg_<sanitized>` path. Rust's emitted C (`tests/fixtures/regex_basic.c`):
  `typedef struct __gg_Regex …; typedef struct __gg_Match …;` (@3527-3528),
  `struct __gg_Match { … }` (@3677), `struct __gg_Regex { … }` (@3777).

- **Self-host (STALE, the bug):** `tests/fixtures/self_host_lowerer/lir_codegen.gg`
  still carries the PCRE2-era entries:
  - `lir_to_runtime_name` @213-216: `Regex → "GorgetRegex"`,
    `RegexMatch`/`Match → "GorgetRegexMatch"`.
  - `is_runtime_defined_named` @173-174: `GorgetRegex`/`GorgetRegexMatch → true`.

  Consequence, both at write sites:
  - `build_struct_names` @254-256 pushes the bogus runtime name as the struct's
    C type → all references spell `GorgetRegexMatch`/`GorgetRegex`.
  - `should_skip_struct` @298 (`lir_to_runtime_name(name) != ""`) SKIPS emitting
    the real `Regex`/`Match` typedef → the name is never declared.

This is a Core-invariant-#2 violation (name-matched runtime mapping) that went
stale when the upstream PCRE2→pure-Gorget migration landed in Rust but not in the
self-host copy.

## 3. The fix (bug 1) — mirror Rust, retire the stale entries

`tests/fixtures/self_host_lowerer/lir_codegen.gg`:

- Remove the `Regex`/`RegexMatch`/`Match` arms from `lir_to_runtime_name`
  (was @213-216) → they fall through to the `__gg_<sanitized>` else-branch,
  exactly like Rust.
- Remove the `GorgetRegex`/`GorgetRegexMatch` arm from `is_runtime_defined_named`
  (was @173-174) → the structs are no longer skipped, so their typedefs emit.

Both replaced with a comment matching Rust's `mod.rs:229-231` rationale. Net
diff: 5 insertions, 6 deletions, one file.

## 4. PROVEN before/after (bug 1 eliminated; bug 2 exposed)

After rebuilding the self-host driver and re-emitting:

| fixture        | before (CC-FAIL)                          | after fix (1)                                                                 |
|----------------|-------------------------------------------|-------------------------------------------------------------------------------|
| regex_basic    | `unknown type name 'GorgetRegexMatch'`    | `incompatible types … '__gg_RxNode' from type '__gg_ParseError'` (regex_basic.c:9170) |
| regex_corpus   | `unknown type name 'GorgetRegexMatch'`    | `… '__gg_RxNode' from '__gg_ParseError'` (regex_corpus.c:10824)               |
| regex_extended | `unknown type name 'GorgetRegexMatch'`    | `… '__gg_RxNode' from '__gg_ParseError'` (regex_extended.c:5820)              |

The `GorgetRegexMatch`/`GorgetRegex` error class is GONE in all 3 (confirmed via
both the manual emit→cc loop and the authoritative `self_host_runtime_diff`
backlog, which now lists the NEW `__gg_RxNode`/`__gg_ParseError` error). **No
fixture reaches byte-exact RUN** — bug (2) blocks them.

## 5. Residual — bug 2 (the boundary; OUT OF SCOPE)

`lib/xtd/regex.gg:211` (and ~30 sibling sites) write bare `return Empty()` where
`Empty` is `RxNode.Empty` (tag 0, `lib/xtd/regex.gg:62`). The transitively-imported
`ParseError` (`lib/std/conv.gg:46-50`) ALSO has a tag-0 `Empty` variant.

The self-host resolves the bare variant's enum via a **flat, name-keyed,
first-write-wins** index, NOT by expected/return type:
`tests/fixtures/self_host_lowerer/lower.gg:2541-2585` builds
`enum_variant_parent_idx` (`if not …contains(var.name): …put(var.name, edef.name)`).
`ParseError` is registered before `RxNode`, so `enum_variant_parent("Empty")`
returns `"ParseError"`, and `Empty()` lowers to `(__gg_ParseError){.tag=0}` even
though the function returns `RxNode`. Rust gg disambiguates by the expected type.

This is a genuinely deep fix: thread the function's declared return type / the
assignment's expected type into the bare-variant-constructor lowering
(`lower_expr.gg`, consumers of `enum_variant_parent` @4125/@2124/@4607), so a
typed expectation overrides the name-based first-write-wins fallback. It violates
Core invariant #2 the same way bug (1) did, one layer up. **Recommend a separate
scout+brief+review round.** It likely unblocks more than just these 3 fixtures
(any program mixing two enums with same-named variants).

## 6. Gates (all GREEN in this worktree)

| gate                              | result                                                              |
|-----------------------------------|--------------------------------------------------------------------|
| `cargo test --lib`                | 1084 passed / 0 failed                                             |
| `self_host_bootstrap_fixed_point` | **GREEN** (passed, 274.59s) — self-host still self-reproduces      |
| `c_emit_comparison`               | 1031 matched (baseline: 1031 — **0 regression**, verified by revert)|
| `lowerer_comparison`              | 1067 matched, 114 mismatch, 5 crash (all 5 are pre-existing negative-test fixtures, none regex) |
| `self_host_runtime_diff` (PARITY) | **724/1066 = 67.9% before AND after** (+0; the 3 regex stay CC-FAIL on bug 2) |

## 7. Edit plan (for integration, once bug 2 is also fixed)

`tests/fixtures/self_host_lowerer/lir_codegen.gg` only:

1. In `is_runtime_defined_named`, delete the
   `if name == "GorgetRegex" or name == "GorgetRegexMatch": return true` arm
   (replace with the pure-Gorget comment).
2. In `lir_to_runtime_name`, delete the `elif name == "Regex": return "GorgetRegex"`
   and `elif name == "RegexMatch" or name == "Match": return "GorgetRegexMatch"`
   arms (replace with the pure-Gorget comment).

**Integration caveat:** landing fix (1) alone changes no parity number and the 3
fixtures still CC-FAIL — but it is the correct reference-grade state and a hard
prerequisite for the +3. Pair it with the bug-2 fix so the round shows the +3, OR
land it standalone as a correctness/fossil-retirement cleanup (it removes a
stale, Core-#2-violating name map from the self-host showcase).
