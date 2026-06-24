# Case-B "A_closure" scout — self-host lexer keyword-divergence fossil

**Scout date:** 2026-06-23/24 · **Base tip:** gorget-1 `2fa251ff` · **Prototype commit:** see `PROTOTYPE(scout)` on this branch — **DO NOT INTEGRATE**.

All numbers below were regenerated THIS session from the cited commands.

---

## TL;DR / verdict

- **Diagnosis CONFIRMED end-to-end.** The self-host lexer (in the non-`self_host_lexer` dirs) still treats `Box`/`Rc`/`Arc`/`Weak`/`Cell`/`RefCell`/`Mutex`/`RwLock` as KEYWORDS; Rust's lexer treats them as identifiers (`src/lexer/token.rs:316-317`). For `snag51_closure_block_tail_value.gg`, `enum Box:` mis-lexes the type NAME → broken `__drop`/`__clone` emit AND the `case Box.A(s):` pattern collapses to `PWildcard` (bindings `s`/`n` lost).
- **The keyword removal is reference-grade-correct at the parser/resolver level** and slightly IMPROVES the comparison scores (+1 parser, +1 resolver). It makes `s`/`n` genuinely BOUND (proven: `__gg_Box->A_0`/`->B_0` extraction appears in the emit).
- **⛔ BLOCKER — the keyword removal REGRESSES `self_host_bootstrap_fixed_point` ("stage-1 cc failed").** Removing the keyword exposes a LATENT self-host lowering bug: the self-host's OWN AST source uses `Box[SpannedType]`/`Box[SpannedExpr]` as enum-variant payloads, and `Box(elem)` construction at those sites lowers to a `((Box__T *)…)->_0 = …` struct-init that is incompatible with `Box__T`'s `typedef void*`. This path was SUPPRESSED while `Box` was a keyword. **Baseline (keyword) stage-1 cc = GREEN; prototype (de-keyworded) stage-1 cc = RED — isolated, proven by stash/rebuild.**
- **Net parity = 0** (as the brief predicted). `snag51` mirages downstream on the closure-call ABI gap (the `mk` closures emit `void`-returning bodies that never propagate their tail value — see "Downstream blocker" below). It moves CC-FAIL→CRASH (both failing buckets).

**Recommendation:** This is **NOT a 6-file mechanical edit**. The keyword removal is the easy 20%; the load-bearing 80% is FIXING the latent `Box__T ._0`-vs-`void*` lowering bug so the bootstrap re-converges. The executor MUST land both together (de-keyword + lowering fix), or the bootstrap stays red. The Track-β-flip-prereq VALUE (genuine `s`/`n` binding) is real and delivered by the de-keyword half.

---

## 1. Diagnosis confirmed (end-to-end)

### Rust reference: the 8 names are identifiers
`src/lexer/token.rs:316-317`:
```
// Smart pointer / concurrency types — demoted to identifiers.
// Box, Rc, Arc, Weak, Cell, RefCell, Mutex, RwLock are regular identifiers.
```
`grep -rE 'KwBox|"Box"|…' src/lexer/ src/parser/` → **zero** hits. They are pure identifiers in the reference.

`Box` (and `Weak`/`Mutex`/`Shared`/`Guard`/…) are made available unqualified WITHOUT an import via the resolver's builtin-generic-types registration, NOT via the lexer:
- Rust: `src/semantic/resolve.rs:19-22` `BUILTIN_GENERIC_TYPES` (registered as dummy-span Import placeholders).
- Self-host: `tests/fixtures/self_host_lowerer/resolve.gg:144-161` — `col_names.push("Box")` … all 15 names registered as `DkImport()`. **This already exists and works** — so the de-keyworded `Box` still resolves unqualified.

### snag51 mis-lex reproduced
`tests/fixtures/snag51_closure_block_tail_value.gg` defines `enum Box: A(String) B(int)` (lines 71-73) and pattern-matches `case Box.A(s):` / `case Box.B(n):` in `main` (lines 162-167).

- Rust oracle (`./target/release/gg run …`): correct — prints `Enum match: A('from-match-arm')`.
- **Baseline self-host emit** (`driver … --emit-c`): the `Box` enum's drop/clone come out MANGLED — `* self = (*)__p;` (empty type name) and `__clone` with no return type → **`cc` HARD-FAILS** (`'self' undeclared`, `'z' undeclared`). The type NAME `Box` was lost because it lexed as `KwBox`, not an identifier.

### Bisection proof (`Box`→`Shape`)
`sed 's/\bBox\b/Shape/g'` on snag51:
- Rust oracle: still correct.
- Self-host emit: `Shape__drop`/`Shape__clone` are now WELL-TYPED (`__gg_Shape* self = (__gg_Shape*)__p`), **`cc` SUCCEEDS**. (Output still empty — the downstream closure mirage, unrelated.)

This is a clean, decisive bisection: the keyword mis-lex destroys the enum TYPE NAME; renaming away from the keyword fixes the type-name emit.

### Pattern-binding loss mechanism (the β-flip-prereq)
`tests/fixtures/self_host_typechecker/parser.gg::parse_pattern_atom`:
- The `TOK_IDENT` branch (`:1662`) handles `Name.Variant(args)` → `PConstructor("Box.A", [PBinding("s")])` — the CORRECT path. Fires only for identifiers.
- The keyword-constructor branch (`:1692`) matches only `KW_SOME/KW_OK/KW_ERROR/KW_NONE` — NOT `KW_BOX`.
- The fallthrough `TkKeyword(kw)` arm (`:1726-1737`) calls `type_keyword_name(kw)` (`""` — `KwBox` not a type-keyword) then `non_type_keyword_name(kw)` (`:736` — `KwBox` hits `else: return ""`).
- → falls to `:1739 self.errors.push("expected pattern"); self.advance(); return PWildcard()`. **The bindings are LOST.**

After the de-keyword, `Box` arrives as `TOK_IDENT`, the `:1662` branch fires, and the binding is preserved. **PROVEN by the prototype emit**: the lowered `main` now does `((__gg_Box *)(…))->tag` switch + `->A_0` (the `s` binding) + `->B_0` (the `n` binding) extraction.

---

## 2. The COMPLETE site-list

### Symlink topology (`ls -l tests/fixtures/self_host_*/{lexer,parser,ast}.gg`)
- `lexer.gg`: **independent copies** in `self_host_lexer` (already fixed reference), `self_host_parser`, `self_host_resolver`, `self_host_typechecker`. `self_host_check/lexer.gg` and `self_host_lowerer/lexer.gg` are **symlinks → `self_host_typechecker/lexer.gg`**.
- `parser.gg`: **independent copies** in `self_host_parser`, `self_host_resolver`, `self_host_typechecker`. `self_host_check/parser.gg` and `self_host_lowerer/parser.gg` are **symlinks → `self_host_typechecker/parser.gg`**.

⇒ **6 files to edit** (3 lexer + 3 parser independent copies); `check`/`lowerer` inherit via symlink. `self_host_lexer` is ALREADY correct — leave it (it's the reference for the fix shape).

### A. Lexer arms to remove (each of `self_host_{parser,resolver,typechecker}/lexer.gg`)
1. **`Keyword` enum** — remove the line (line 33 in all three):
   ```
   KwBox  KwRc  KwArc  KwWeak  KwCell  KwRefCell  KwMutex  KwRwLock
   ```
2. **`keyword_from_str`** — remove the 8 `elif s == "Box": return Some(KwBox())` … `RwLock` arms:
   - `self_host_parser/lexer.gg:247-262`
   - `self_host_resolver/lexer.gg:251-266`
   - `self_host_typechecker/lexer.gg:255-270`

   (After removal `keyword_from_str("Box")` returns `None` → the scanner emits `TkIdentifier("Box")` at `lexer.gg::lex_scan_ident` (`…:454-461` in typechecker copy).)

### B. Parser `Kw*`-VARIANT consumers to repoint (each of `self_host_{parser,resolver,typechecker}/parser.gg`)
The ONLY `Kw{Box,Rc,…}`-VARIANT references in the parsers are inside `int keyword_tag(Token tok)`. Remove the 8 `case KwBox: return KW_BOX` … `RwLock` arms (the `match` has `else: return 0`, so removal is safe; they're dead once the lexer stops emitting them):
   - `self_host_parser/parser.gg:411-415` (Box/Rc/Arc) + `:461-470` (Weak/Cell/RefCell/Mutex/RwLock)
   - `self_host_resolver/parser.gg:410-414` + `:460-469`
   - `self_host_typechecker/parser.gg:411-415` + `:467-475`

### C. Parser `KW_*`-INT consumers — LEAVE AS DEAD CODE (verified harmless superset)
These compare the INT tag (`keyword_tag(...) == KW_BOX`, `check_kw(KW_BOX)`). After the fix `keyword_tag` never returns `KW_BOX` (returns 0), so these branches are simply never taken; the identifier path handles every construct identically (and is a strict SUPERSET). **Verified each is a no-regression superset:**
- `extract_name` (`…:1109-1127`) — keyword→name reconstruction; identifiers already return the name via `token_ident` at `:1106`. Dead.
- **Type position** `Box[T]` (`…:1384-1409` `parse_type`) builds `TNamed(wrapper_name, args)`. The identifier branch (`…:1437-1462`) builds the SAME `TNamed` AND additionally handles function-type / postfix-`*` (superset).
- **Expr ctor** `Box(expr)`/`Box[T]()` (`…:2701-2745` `parse_prefix`) builds `ECall`. The bare-identifier path (`:2748-2751`) + the postfix loop in `parse_expr_bp_with_lhs` (`:1787-1814`: `Name[T](args)`→generic `ECall`, `Name(args)`→`ECall`, `Name[idx]`→`EIndex`) produce the SAME `ECall` and MORE (superset).
- **Type lookahead** `Box[T]` (`…:3377-3381` `skip_type_lookahead`). The identifier branch (`:3391-3398`) skips ident+bracket+paren identically (superset).

The executor's reference-grade version SHOULD delete the dead C-block intercepts + the `KW_BOX..KW_RWLOCK` int consts (`…:124-126`,`:153-157`) too, for cleanliness — but they are inert and not required for correctness/measurement.

> Track-α disjointness: my parser edits are at `keyword_tag` (`…:408-475`); Track-α's meta-for region is `…:3011-3291`. **No overlap.** Lexer edits are entirely separate.

---

## 3. Prototype measurement (this session, force-rebuilt driver)

Prototype = the A+B removals above across all 6 files. Driver rebuilt (`gg build self_host_lowerer/driver.gg`, exit 0 — it self-compiles the de-keyworded lexer fine).

### snag51 — binding RESTORED, mirage CONFIRMED
- Prototype emit: `Box__drop`/`Box__clone` now `__gg_Box`-typed (was broken `* self`); **`cc` SUCCEEDS** (was hard CC-FAIL).
- `main`'s match emits `((__gg_Box *)…)->tag` + `->A_0`(=`s`) + `->B_0`(=`n`) — **the `s`/`n` bindings are genuinely BOUND** (the β-flip-prereq value).
- Program still prints EMPTY → downstream closure-call ABI mirage (separate, below). snag51 stays failed.

### Full parity — `GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test … self_host_runtime_diff -- --nocapture`
```
PARITY = MATCH/(MATCH+WRONG+CC-FAIL+CRASH+DRIVER-FAIL) = 747/1069 = 69.9%
MATCH 747 · WRONG 89 · CC-FAIL 204 · CRASH 29 · DRIVER-FAIL 0
```
**Identical to the brief's cited baseline 747/1069. Net parity delta = 0.** `snag51` lands in **CRASH** (`exit=None`, the UB of calling a `void`-returning closure body through an `int64_t(*)` pointer). No other fixture flips in either direction.

### Lock-in regression net — `cargo test … self_host_runtime -- --test-threads=1`
```
passing set : 720 · regressed : 0
```
**Zero regressions.** Every committed snapshot (incl. all `Box[T]`-using passers: `box_deref_*`, `box_heap`, `box_int_local_deref`, `box_struct_inner_deref`, `box_in_recursive_struct`, `async_mutex_lock`, `guard_*`, `mutex_*`, `import_type_alias_box`, …) still byte-matches.

### Comparison tests (diagnostic; counts regenerated)
| test | baseline (`2fa251ff`) | prototype | delta |
|------|----------------------|-----------|-------|
| `lexer_comparison` | 0 mismatch / 0 crash | 0 mismatch / 0 crash | 0 |
| `parser_comparison` | 1270/1294 | **1271/1294** | **+1** |
| `resolver_comparison` | 1280/1294 | **1281/1294** | **+1** |

Both improve by 1 (a Box/identifier fixture now agrees with Rust). The `import_*`/`set_literal_*`/`chars`/`fstring_format`/`sigil_type_args` mismatches in the list are PRE-EXISTING (present in baseline too).

### ⛔ Bootstrap fixed-point — `cargo test … self_host_bootstrap_fixed_point -- --nocapture`
```
thread '…' panicked at tests/integration.rs:15204:5: stage-1 cc failed
test result: FAILED
```
**This is the blocker.** See §4.

---

## 4. ⛔ BLOCKER — the latent `Box__T ._0`-vs-`void*` lowering bug (bootstrap regression)

### Symptom
Stage-1 of the bootstrap = the de-keyworded driver re-emits its OWN source (`driver.gg` + `lexer.gg`/`parser.gg`/`ast.gg`/…) as C; that C fails `cc` at **3 sites**:
```
stage1.c:54720:  ((Box__SpannedType *)(__v652))->_0 = (void*)&__v651;
stage1.c:59539:  ((Box__SpannedExpr *)(__v324))->_0 = (void*)&__v322;
stage1.c:59596:  ((Box__SpannedExpr *)(__v357))->_0 = (void*)&__v355;
```
`error: request for member '_0' in something not a structure or union` — because `Box__SpannedType` is `typedef void* Box__SpannedType` (`stage1.c:5616`), NOT a struct with a `_0` field. A `._0` struct-init is being written onto a `void*`.

### Root cause / why the keyword suppressed it
The self-host's OWN AST (`tests/fixtures/self_host_typechecker/ast.gg`) uses `Box[T]` as enum-variant payloads:
```
TArray(Box[SpannedType], int)   TSlice(Box[SpannedType])   TFunction(Box[SpannedType], …)
ECall(Box[SpannedExpr], …)      EAs(Box[SpannedExpr], Box[SpannedType])   ERethrow(…, Option[Box[SpannedType]], …)
```
and `parser.gg` CONSTRUCTS them with `Box(elem)` / `Box[SpannedType](rt)` (e.g. `:1418 TSlice(Box(elem))`, `:1789 EIndex(Box(lhs), Box(idx))`, `:1879 Some(Box[SpannedType](rt))`). **No self-host file imports `Box`** — they relied on `Box` being a keyword. With `Box` keyworded, these `Box(…)` AST-payload constructions took a code path that emitted correctly; de-keyworded, they hit a path that emits the `._0`-on-`void*` write.

The Box type registration is dual: `lir_lower.gg:643-656` registers `Box__<inner>` as a STRUCT with a single `_0` field (`StructField("_0", LT_PTR)`) when `resource_meta_for` returns `BkRegularBox` (`build_resource_metadata`, `:307-311`, `name.starts_with("Box__")`), yet the typedef-emit pass spells `Box__SpannedType` as `typedef void*`. The construction-site lowering and the typedef are out of sync ALONG THIS NEWLY-REACHED PATH only.

### Isolation proof — the keyword IS the trigger
- Baseline (keyword `Box`): rebuilt driver, reproduced stage-1 by hand (`driver … --lir-c` + preamble splice + `cc`) → **cc exit 0, zero errors** (bootstrap stage-1 GREEN).
- Prototype (de-keyworded): same procedure → **cc exit 1, the 3 `._0` errors**.
- Stash/pop verified both directions on the same tree.

### Why minimal repros DON'T reproduce it (executor caution)
`Box[P](p)`, `Some(Box[P](p))`, `Ty.TSlice(Box(elem))`, `Box[int]` etc. in standalone fixtures (with or without `from std.collections import Box`) **all emit + cc + run correctly** with the prototype driver. The break only manifests in the FULL driver self-compile — a monomorphization-ordering / multi-file interaction where `Box__SpannedType` is registered as `void*` in one pass and struct-with-`_0` in another, and the keyword removal changed which path fires first. The executor must reproduce via the actual stage-1 path (`driver self_host_lowerer/driver.gg lib --lir-c`), not a toy.

### Likely fix locus (executor to confirm)
The construction-site lowering for a bare `Box(x)` whose result type is a known `Box__<inner>` regular box (`type_runtime_map[name]=="Box"`, registered struct id in `sr` with the `_0` field) must use the **box-alloc** path (`emit_box_alloc`, `lir_lower.gg:2916`) consistently — NOT a generic `IStructInit`/`._0` write — and the `Box__T` C type must be spelled consistently (`void*` ptr) at the construction site. Candidate sites: the `Box`/`Some`/`None` skip in `try_lower_user_struct_ctor` (`:2808`), the `func_name == "Box"` IBoxAlloc dispatch, and the `IStructInit` fallback that's currently catching these `Box(elem)` calls. Confirm by diffing the lowering trace for `TSlice(Box(elem))` (parser.gg:1418) between the keyword and identifier paths.

---

## 5. Downstream blocker for snag51 (the mirage — SEPARATE follow-up)

Even with `Box` de-keyworded AND the bootstrap fixed, `snag51` will NOT reach MATCH. Its `mk` closures (`auto mk = (): match …`) emit a **`void`-returning** closure body that computes the tail value into a local but `return;`s without propagating it:
```c
void __Closure_0__call(void* __p0) { … __s3 = 1; … return; }   // signature is void!
…
__v3 = ((int64_t(*)(void*))…)(…);   // caller calls it AS int64_t-returning → garbage/UB
```
This is the **closure-call ABI gap** (the closure's inferred return type — `int`/`String`/`Box` — is not reflected in the emitted closure-function signature, so the tail value never crosses the call boundary). It is INDEPENDENT of the keyword/lexer work and of the §4 Box-lowering bug.

**Filed follow-up name:** *"self-host closure-call ABI: non-void closure bodies emit `void`-returning `__Closure_N__call`, dropping the tail value (snag51 `mk()` family)."* This is the gate that unblocks snag51 → MATCH and likely a broader `.map(it…)` / IIFE-closure class.

---

## 6. Reference-grade gate (Core invariant #8)

- The de-keyword half is **reference-grade-correct**: it matches Rust (`Box` = identifier), makes the names genuinely BOUND (not a workaround), and IMPROVES the comparison parity (+1/+1). It is the legitimate Track-β-flip prerequisite.
- But **shipping the de-keyword ALONE is NOT acceptable** — it regresses `self_host_bootstrap_fixed_point` (a hard gate per Core invariant #7). The §4 lowering bug is a real defect (a `._0` write on a `void*` — a silent miscompile that `cc` happens to catch here) that the keyword was MASKING. Per "Don't redesign around compiler gaps" + Core #8: the executor must FIX the §4 lowering bug, not re-keyword `Box` to dodge it.
- **Executor's definition of done:** all 6 files de-keyworded + §4 lowering fixed → `self_host_bootstrap_fixed_point` GREEN, `self_host_runtime` 720/0, `lexer/parser/resolver_comparison` ≥ baseline (expect +1/+1), full integration green. snag51 stays failed on §5 (a separately-filed follow-up, with an `#[ignore]` fixture whose expected output is the CORRECT output) — that is acceptable ONLY because §5 is a distinct, named, filed bug.

---

## 7. Commands to regenerate every number (run after a force-rebuild)

```bash
# driver (self-compiles the de-keyworded lexer)
GG_BUILD_TIMEOUT_SECS=600 ./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg

# parity (the honest north-star number)
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_runtime_diff -- --nocapture          # → PARITY = …/1069

# regression net (build-breaking)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_runtime -- --nocapture --test-threads=1   # → passing 720, regressed 0

# bootstrap (the BLOCKER gate)
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release \
    self_host_bootstrap_fixed_point -- --nocapture

# comparisons (diagnostic; counts only)
for t in lexer_comparison parser_comparison resolver_comparison; do
  GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release $t -- --nocapture --test-threads=1
done

# manual stage-1 repro (to debug §4 without the full test):
./tests/fixtures/self_host_lowerer/driver tests/fixtures/self_host_lowerer/driver.gg lib --lir-c > /tmp/s1body.c
PE=$(grep -n $'\ntypedef struct __gg_' tests/fixtures/self_host_lowerer/driver.c | head -1 | cut -d: -f1)
head -n $((PE-1)) tests/fixtures/self_host_lowerer/driver.c > /tmp/pre.c
cat /tmp/pre.c /tmp/s1body.c > /tmp/s1.c && cc -O0 -w -o /tmp/s1 /tmp/s1.c -lm -lpthread   # cc-fails at the ._0 sites
```
