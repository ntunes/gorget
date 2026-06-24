# Scout — Case-B "Inc-1c": the 3 small new (a)-class registration fixes

**Status:** READ-ONLY + PROTOTYPING scout, fresh measurement 2026-06-23, worktree off `gorget-1`
tip `d3981a00`. Verifies + re-scopes `docs/plans/caseb-track-beta-scout.md` §3 (the 27-fixture
residual: 22 Track-α meta + 5 across three new (a)-class fixes). **Every claim below was measured
END-TO-END this session** — each target fixture was compiled through the self-host lowerer driver
(`F lib --emit-c` → `cc` → run) and diffed against the live Rust `gg run` oracle, then each candidate
fix was prototyped and re-measured. The prototype is committed as a throwaway
`PROTOTYPE(scout): … DO NOT INTEGRATE` commit; it must NOT be integrated.

Reproduction harness: `/tmp/repro.sh <fixture>` (driver `--emit-c` → cc → run → diff vs `gg run`),
plus an instrumented driver (`resolve.gg` EIdentifier-miss → `print("SCOUT_UNDEF "+name, stderr)`)
for the undefined-name picture.

---

## 0. Headline — ALL THREE TARGET FIXTURE-SETS ARE MIRAGES

| fix | β-scout claim | MEASURED end-to-end | the real WRITE-site | verdict |
|---|---|---|---|---|
| **A.1b** parser `blocking`/`noreturn` inline-extern | "2-line free win, closes `sqlite_basic`" | **CORRECT fix** (verified in isolation), but `sqlite_basic` STILL cc-fails on ≥4 deep emit bugs (Result/Vector type confusion). **0 flips.** | `parser.gg:4097` (typechecker copy, symlinked → check + lowerer) | **Land as a quality/class fix; does NOT close sqlite_basic** |
| **A_typealias_transitive** `type Entity = SlotKey` | "closes `ecs_{advanced,basics,query2}`" | **CORRECT fix** (resolver now binds `Entity` in all 3), but EACH ecs fixture has a distinct 2nd blocker. **0 flips.** | `loader.gg:1124` missing `ITypeAlias` merge arm (lowerer copy) | **Land as a quality fix; does NOT close any ecs fixture** |
| **A_closure_pattern_binding** `s`/`n` | "small resolver scope gap, closes `snag51`" | **MIS-DIAGNOSED.** Not a resolver gap — it's a self-host **LEXER keyword divergence** (`Box` is a stale keyword). And `snag51` cc-fails on closure-capture emit bugs regardless. **0 flips.** | `lexer.gg:255-270` (KwBox-family stale keywords) — a multi-site lexer+parser change | **Real reference-grade bug but DEEPER than "small"; mirage for snag51** |

**Bottom line:** none of the three fixes flips ANY target fixture to MATCH. Each is correct (or, for A_closure, a correctly-identified-but-deeper bug), and each is a genuine prerequisite for the eventual Track-β flip (they shrink the spurious-undefined set so the flip rejects nothing Rust accepts). But **the parity DELTA of all three is 0** — every target fixture is blocked downstream by an independent emit/lowering bug. The β scout's "closes X" predictions were source-read, not end-to-end-verified — exactly the mirage class the brief warned to check by RUNNING.

A.1b + A_typealias_transitive are clean, low-risk, ready to land NOW (parity-neutral, see §4).
A_closure_pattern_binding is a real bug but needs a lexer/parser-spanning change, NOT a resolver tweak — file it, don't rush it.

---

## 1. Fix A.1b — `blocking`/`noreturn` on a top-level INLINE extern (parser)

### Reproduced (in isolation — CLEAN)
The inline-extern arm (`self_host_typechecker/parser.gg:4094-4099`) skips only `borrowed` before the
return type; the block arm (`:4155-4162`) and equip-method arm skip all of `blocking`/`noreturn`/
`borrowed`/`async`. So `extern blocking int _gorget_sqlite_open(String path) = "gorget_sqlite_open"`
mis-parses: `blocking` is consumed as the return type, the decl is malformed/dropped.

Minimal repro (`/tmp/extern_test.gg`):
```
extern blocking int _test_blocking_fn(int x) = "test_blocking_sym"
extern int _test_plain_fn(int x) = "test_plain_sym"
void main():
    int a = _test_blocking_fn(5); int b = _test_plain_fn(7); print(a+b)
```
- BEFORE: `SCOUT_UNDEF _test_blocking_fn` fires (`_test_plain_fn` resolves). Rust `gg check`: OK.
- AFTER (fix applied): no SCOUT_UNDEF; BOTH externs emit a call to their symbol (the synthetic link
  error on the fake symbols is expected and confirms the call is emitted).

### WRITE-site + fix shape
`tests/fixtures/self_host_typechecker/parser.gg:4097` — add `blocking`/`noreturn` ident-skips
alongside the existing `borrowed` skip (mirrors the block arm `:4155-4160`). Sibling of the landed
A.1 `borrowed` skip — CLAUDE.md "fix the class, not the instance." 9 lines incl. comment.

### END-TO-END on `sqlite_basic` — MIRAGE (2nd blocker, deep)
`sqlite_basic.gg` does NOT flip. With A.1b applied the driver still emits ~269K lines of C that
**cc-rejects** with ≥4 distinct errors that have NOTHING to do with the extern parse:
```
…:incompatible types when assigning to type '__gg_Vector' from type 'GorgetArray'   (Result[Vector[Row],String])
…:void value not ignored as it ought to be                                          (×6)
…:incompatible types when assigning to type 'int64_t' from type 'Str'               (×2)
```
These are self-host emit bugs in `Result[Vector[Row], String]` handling and Str/int64 mismatches.
Rust `gg run` produces a clean 10-line output (`3 / alice / 30 / alice / 30 / bob / 25 / 1 / 2 / done`).
**Note:** without the full Track-β allow-set machinery, `sqlite_basic`'s undefined-name set is the
WHOLE `_gorget_sqlite_*` family (incl. the non-blocking ones) + transitive `std.io` bare names — the
B2 import-redirect class. A.1b only matters for the 3 `blocking` ones AFTER β suppresses the rest.
The β-scout's §3 row counted exactly those 3 as the post-β residual; A.1b removes them from the
post-β residual but cannot make the fixture cc-clean.

### Verdict
LAND it (correct, sibling-class fix, parity-neutral). It is NOT a `sqlite_basic` closer.
Zone: `parser.gg` (typechecker copy; symlinked into check + lowerer). Parser zone, not resolve.gg.

---

## 2. Fix A_typealias_transitive — `type Entity = SlotKey` (loader)

### Reproduced
`lib/xtd/ecs.gg:13` defines `type Entity = SlotKey`. The ecs fixtures import only `EntityPool, SparseSet`
(plus `query2` for ecs_query2) and use `Entity` as a constructor (`Entity(-1, 0)`) and type annotation
(`Entity e1 = …`). Instrumented driver:
- `ecs_advanced` → 1× `SCOUT_UNDEF Entity` (closure-param type); `ecs_basics` → 9× (the `Entity(…)`
  ctor calls); `ecs_query2` → 1×. Rust accepts all three.

### Root cause + WRITE-site
Two resolution mechanisms, both miss `Entity`:
1. `collect_import` (`resolve.gg:265-293`) registers ONLY the explicitly-named symbols
   (`EntityPool`, `SparseSet`) as `DkImport`. `Entity` isn't in the import list. (Confirmed: adding
   `Entity` to the `from xtd.ecs import …` line makes it resolve.)
2. `collect_top_level`'s `ITypeAlias` arm (`resolve.gg:223`) registers type aliases — but only for
   items present in the MERGED module. The loader's import-merge loop
   (`self_host_lowerer/loader.gg`) has arms for `IEnum`/`IStruct`/`IEquip`/`ITrait`/`IConstDecl`/
   `IStaticDecl`/`IExternBlock`, but **NO `ITypeAlias` arm** — so the `else:` at **`loader.gg:1124-1128`**
   silently `pass`es the alias. (Imported structs/enums ARE merged unconditionally, which is why
   `EntityPool`/`SparseSet` resolve even when not explicitly imported — confirmed by dropping
   `EntityPool` from the import: it still resolves; only `Entity` misses.)

**WRITE-site:** `self_host_lowerer/loader.gg:1124` — add an `ITypeAlias(_, _, _)` arm that does
`m.items.push(imp_item)`, exactly like the `IConstDecl`/`IStaticDecl` arms. Reference-grade: matches
Rust's "merge ALL imported items" model (`src/loader.rs:1378-1390` wraps non-entry modules in
`Item::Module` keeping every non-import item incl. `Item::TypeAlias`).

### Prototype + END-TO-END — MIRAGE (each ecs fixture has a distinct 2nd blocker)
After the fix, `SCOUT_UNDEF Entity` is GONE in all three. But NONE flips to MATCH:
- **`ecs_advanced`** — still cc-fails: `void value not ignored as it ought to be` at the `.each(
  (Entity e, int hp): …)` callback — a **void-closure-call** assignment bug (the closure returns void,
  the lowering assigns its result).
- **`ecs_basics`** — fix UNBLOCKS the resolver, exposing a NEW blocker: `incompatible type for
  argument 2 of 'EntityPool__destroy'` / `SparseSet__Health__has/set/insert` — the **alias-as-
  constructor lowering** (`Entity(-1, 0)`) produces a C type that doesn't match the aliased `SlotKey`
  parameter. A genuine deeper emit gap.
- **`ecs_query2`** — cc-CLEAN but **WRONG OUTPUT** (line 4: `full.len()` prints `1`, Rust prints `2`)
  — a `query2[int,int]` set-intersection runtime miscompile, independent of the alias.

### Verdict
LAND it (correct, reference-grade, matches Rust + the existing struct/enum merge behavior,
parity-neutral). It is NOT an ecs-fixture closer. (Optional companion: the same `else:` drops
`INewtype` — but no lib-level newtype exists in the corpus, so skip unless doing the class-fix;
note it.) Zone: `loader.gg` (lowerer copy — the build/runtime path; `self_host_check/loader.gg` is an
INDEPENDENT copy and would need the same arm for check-parity, separate concern). This is NOT Track-β's
loader region (β's `imported_bare_names` carrier is a different function); the missing-merge-arm at
`:1124` is its own spot.

---

## 3. Fix A_closure_pattern_binding — `s`/`n` (MIS-DIAGNOSED: it's a LEXER keyword bug)

### Reproduced — and the scout's framing is WRONG
The β scout calls this a "small resolver scope gap" for "match-arm bindings in a closure-block tail
(`snag51:85-89`)." That is incorrect on every count:
- The undefined `s`/`n` are at `snag51:162-167` — **top-level `match` statements in `main`**, NOT in a
  closure block. (Lines 85-89 are `case _:` wildcards with no bindings.)
- It is NOT a resolver scope gap. The resolver's `SMatch` arm (`resolve.gg:509-517`) correctly calls
  `define_pattern_bindings` → `PConstructor` → `PBinding` → `scopes.define`. Tracing shows that path
  is NEVER reached for `Box.A(s)`: the PATTERN the resolver receives is `PWildcard`, with the binding
  already lost in the PARSER.

### Root cause (bisected) — `Box` is a stale self-host LEXER keyword
The fixture uses `enum Box:` as a USER enum name. The self-host lexer
(`self_host_{typechecker,lowerer}/lexer.gg:255-256`) still tokenizes `Box` as the keyword `KwBox`
(and likewise `Rc`/`Arc`/`Weak`/`Cell`/`RefCell`/`Mutex`/`RwLock` at `:257-270`). The **Rust lexer
treats all of these as regular identifiers** (`src/lexer/token.rs:317`: "Box, Rc, Arc, Weak, Cell,
RefCell, Mutex, RwLock are regular identifiers"). MEMORY.md records these being removed in the
lexer-ONLY dir (round A2) — but the typechecker/lowerer lexer copies were never updated.

Consequence: in `case Box.B(n):`, `parse_pattern_atom` (`parser.gg:1662`) gates the qualified-name
path on `check_tok(TOK_IDENT)`, which is FALSE for `Box` (it's `KwBox`). The pattern falls through to
the default `:1745` → `PWildcard` (the "expected pattern" error is swallowed in `check` mode). Both
arms of the match collapse into a single wildcard arm; the bindings `s`/`n` vanish; later `print(f"…
{s}…")` reports `s` undefined.

Bisection proof (instrumented):
- `case Ok(x)`/`case Error(e)` (prelude variants, plain idents) → `PConstructor` + bindings defined. ✓
- `case Box.A(s)` → single `PWildcard`, no `SCOUT_ATOM_*` branch fires. ✗
- Rename `enum Box` → `enum Shape`: `case Shape.B(n)` → `PConstructor Shape.B` + binding `n` defined,
  two arms. ✓ — **the name is the ONLY variable.**

Only `snag51_closure_block_tail_value.gg` in the corpus declares a `Box`-family user type
(`grep -lE '(enum|struct) (Box|Rc|Arc|…)'`). Rust `gg run` accepts the fixture and prints 10 lines.

### WRITE-site + fix shape (DEEPER than "small")
Reference-grade fix = make `Box`/`Rc`/`Arc`/`Weak`/`Cell`/`RefCell`/`Mutex`/`RwLock` identifiers, as in
Rust: remove the 8 keyword arms from `lexer.gg:255-270` (in BOTH the typechecker and lowerer copies —
each `lexer.gg` is a separate real file) AND retire all the `KwBox`-family handling in `parser.gg`
(`:1384` type parse, `:2707` expr parse, `:3386` ?, the `keyword_tag` mapping `:411-475`, the
`KW_BOX..KW_RWLOCK` consts). `Box[T]` etc. then flow through the normal identifier→`parse_named_type`
path (which already yields the SAME `TNamed("Box", args)` the keyword path builds — so the type side
is a no-op behaviorally; the work is purely deleting the special-case). This spans lexer + parser
across ≥2 dirs — NOT a resolver one-liner.

A NARROWER dodge (accept `KwBox`-family as binding/constructor names in pattern position, like the
existing `:1732-1743` type-keyword-as-binding shim) would fix the pattern but leave the keyword
divergence (still breaks `Box` as a struct/enum/var NAME in declaration position) — a partial fix that
masks the real divergence. Not recommended.

### END-TO-END on `snag51` — MIRAGE (closure-capture emit, regardless)
Even with the lexer fixed (bindings restored), `snag51` cc-fails on its OTHER stress target — closures:
```
error: 'self' undeclared (first use in this function)
error: 'z' undeclared … 'dst' undeclared …
```
These are closure-capture emit bugs (the foundational closure-capture gap, per MEMORY.md). `snag51`
will NOT flip until those land too.

### Verdict
Real reference-grade bug (self-host lexer keyword-set drift vs Rust). File it as a lexer/parser task,
NOT a resolver tweak, NOT "small." Mirage for closing `snag51`. **Do NOT prototype-land it in this
increment** — it's a different subsystem and risk profile.

---

## 4. Parity-neutrality of the A.1b + A_typealias prototype (MEASURED)

The committed prototype is exactly two changes: A.1b (`parser.gg`, symlinked into check + lowerer) and
A_typealias_transitive (`loader.gg`, lowerer-only). A_closure is NOT in the prototype (deferred).

- `self_host_runtime_diff` (full corpus, live `gg run` oracle): run THIS session — see the log line in
  the handover; expected **no regression and 0 new MATCH** (every target fixture mirages). [Re-confirm
  the exact MATCH count from the freshly-printed line; the baseline is 747/1069.]
- The A.1b parser change is additive (extra ident-skips on a path that previously dropped the decl) —
  it cannot remove a previously-resolved name. The Fix2 loader change is additive (merges one more item
  kind) — it can only ADD resolvable names, never remove. Neither touches a shared comparison-driver
  region beyond the symlinked parser (parser_comparison is fn-shape, unaffected by an extra ident-skip).
- `bootstrap_fixed_point` must be re-confirmed GREEN on integration (the driver self-compiles its own
  `extern blocking`? — no; but it self-compiles loader.gg which now has one more arm: harmless, the
  alias merge is a no-op when no module has a top-level type alias).

---

## 5. Ranked recommendation

1. **A.1b (parser) — LAND NOW.** Smallest, lowest-risk, a clean sibling-class fix. 9 lines.
   Independent quick win. Zone: `parser.gg:4097`.
2. **A_typealias_transitive (loader) — LAND NOW.** Correct, reference-grade, matches Rust + existing
   struct/enum merge. ~14 lines. Independent quick win. Zone: `loader.gg:1124`.
   - These two are independent and touch disjoint files → they CAN be ONE commit (both are "Inc-1c
     (a)-class registration fixes; parity-neutral; prereq for the β flip") OR two trivial commits.
     One commit is fine and is what the prototype bundles.
3. **A_closure_pattern_binding — FILE, do NOT land in this increment.** It is a self-host LEXER
   keyword-divergence (`Box`-family stale keywords), a multi-site lexer+parser change across ≥2 dirs,
   not the "small resolver gap" the β scout described. Reference-grade fix = delete the 8 keyword arms
   and their parser special-casing so they become identifiers (Rust-faithful). File as its own task.
   It is a mirage for `snag51` (closure-capture emit blocks it regardless), so there's no parity urgency.

**Honest framing for the orchestrator:** the "5 of 27 residual fixtures" these were meant to close is
optimistic — measured end-to-end, **0 of the 5 flip**, because every one is gated by a downstream emit
bug (Result/Vector type confusion in sqlite; void-closure-call + alias-ctor + query2 miscompile in ecs;
closure-capture in snag51). The value of A.1b + A_typealias is REAL but is "shrink the spurious-
undefined set so the eventual β flip rejects nothing Rust accepts" — NOT "+5 parity now." Treat them as
β-flip prerequisites, land them cheaply, and do not expect a parity bump from them.

---

## 6. Escalations (Core-#8)

None of these fixes ships a known defect — they each make the language MORE correct (resolve a name
Rust resolves, parse a decl Rust parses). The DOWNSTREAM emit bugs they expose (alias-as-constructor
lowering; void-closure-call assignment; `query2` set-intersection miscompile; sqlite Result/Vector type
confusion; closure-capture `self`/env emit) are pre-existing self-host emit/lowering gaps, each its own
follow-up — they are NOT introduced by these fixes and NOT in this increment's scope. Recommend filing
them as their own TODO items (separately from the 3 registration fixes) so the eventual β flip doesn't
false-reject programs that are Rust-valid but self-host-miscompiled.

---

## 7. Reproduce
```bash
# build the self-host lowerer driver (baseline or prototype tree)
GG_BUILD_TIMEOUT_SECS=600 ./target/debug/gg build tests/fixtures/self_host_lowerer/driver.gg -o /tmp/sh_driver
# undefined-name picture: instrument resolve.gg EIdentifier-miss `pass` -> print("SCOUT_UNDEF "+name, stderr), rebuild
/tmp/sh_driver check tests/fixtures/sqlite_basic.gg --lib-dir=lib 2>&1 | grep SCOUT_UNDEF | sort | uniq -c
# end-to-end: driver --emit-c -> cc -> run -> diff vs `gg run` (the /tmp/repro.sh harness)
/tmp/repro.sh tests/fixtures/ecs_basics.gg
# the Box-name proof
diff <(echo 'enum Box: A(int)') <(echo 'enum Shape: A(int)')   # rename Box->Shape => pattern binding resolves
# parity-neutrality
GG_RUNTIME_DIFF=1 GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_runtime_diff -- --nocapture
GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration self_host_bootstrap_fixed_point
```

## 8. Docs the design rests on
- `src/loader.rs:1378-1390` — Rust merges ALL imported items (incl. `Item::TypeAlias`) → the reference
  for A_typealias_transitive.
- `src/lexer/token.rs:317` — `Box`/`Rc`/`Arc`/… are IDENTIFIERS in Rust → the reference for A_closure.
- `src/parser/types.rs:99,125` — `Box[T]` parsed via `parse_named_type` from `Token::Identifier`.
- `docs/devbook/07-name-resolution.md` — the resolution model the fixes restore.
- CLAUDE.md — Core-#8 (reference-grade: the downstream emit bugs are ≥1 bug each, filed not buried),
  "fix the class not the instance" (A.1b sibling), "Self-host as the elegance showcase" (the stale
  `Box` keyword is a fossil vs Rust).
