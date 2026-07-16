# D29 RATIFICATION PACKET — visible error propagation (`f()!` + `!` replaces `throws`)

Scout deliverable. Direction pre-agreed (decisions.md LOG, 2026-07-16 "D29 DIRECTION
AGREED"). This packet = census + proven accept-both grammar prototype + collision-corner
dispositions + semantic pinning + readability pages + migration sketch.

Prototype patch: `/tmp/recover_d29_proto.patch` (566 lines; builds clean, `cargo test --lib`
1107/0, parser suite 103/0, end-to-end build+run verified). Instruments/tooling:
`examples/d29_census.rs`, `examples/d29_corners.rs`, `examples/d29_listfail.rs`.

---

## 0. TL;DR recommendations

1. **Call sites** — postfix `!` on the call: `f()!`, `a.m()!`, `g(f()!)!`. Additive to the
   grammar (postfix `!` was a parse error before; adding it cannot change any currently-valid
   parse). `!=` keeps lexing as one token (`BangEq`) under Logos maximal munch, so `a()!=b`
   stays a not-equal comparison — the only real corner, resolved by "insert `! ` (bang-space)".
2. **Signatures** — `!` replaces `throws`, in the same slot after `)`:
   `int f(args) ! E:` (explicit) and `int f(args)!:` (bare = A31 inferred set). The
   explicit/inferred switch is a clean one-token lookahead (token after `!` is `:`/`=`/NEWLINE
   ⇒ inferred; else ⇒ parse the error type). `throws E` kept during the accept-both window.
3. **Function types** — recommend the bracketed `Callable[int(int)!]` /
   `Callable[int(int)! E]` as the canonical throwing-callable spelling; reserve bare
   `int(int)!` (inferred) only at delimiters. Bare-param explicit-error is genuinely
   ambiguous — see §4.
4. **D26 operators** are the same rule at a different operation-glyph (`+!` = mark on `+`);
   no double-marking (`a +! b`, never `(a +! b)!`). Not implemented in this prototype
   (its `+!` lexing collides with pre-D27 prefix-move `!b`; lands with C1+D27).

---

## 1. CENSUS (measured, not estimated)

### 1a. Throws-function DECLARATIONS (parser-based, exact)

Instrument: `examples/d29_census.rs` — parses every `.gg` with the real parser, walks the AST
(free fns + equip methods + trait methods + extern-block + nested modules), counts
`FunctionDef.throws.is_some()`. Reproduce:

```
cargo build --example d29_census
./target/debug/examples/d29_census "fixtures=tests/fixtures" "spectests=spectests" "lib=lib"
```

| Corpus | files | fn defs | **throws decls** | inferred `!:` |
|---|---|---|---|---|
| fixtures (incl. self_host; self_host contributes 0) | 1858 | 6196 | **163** | 0 |
| spectests | 202 | 430 | **16** | 0 |
| lib (stdlib) | 60 | 1989 | **0** | 0 |
| self_host (all 6 driver dirs) | 95 | 2652 | **0** | 0 |
| **IN-REPO TOTAL** | | | **179** | 0 |

Out-of-repo (READ-only; migrate in the later coordination round per the batch-5 ruling):

| Corpus | files | fn defs | throws decls |
|---|---|---|---|
| gorget-js | 18 | 271 | **113** |
| gorget-arena | 67 | 724 | 0 |
| gglox | 6 | 134 | 0 |
| gorget-conformance | 87 | 474 | 0 |

**Key finding #1 — lib has ZERO throws declarations today.** `read_file` is
`extern blocking String read_file(cstr path)` (`lib/std/fs.gg:6`), NOT `throws`. D17 ruled it
fallible but the impl lags (already recorded in MEMORY). So the entire in-repo throws surface
is 179 decls, concentrated in test fixtures; the real error-model dogfood lives out-of-repo
in gorget-js (113 decls).

**Key finding #2 — `grep -l throws` massively over-counts.** The self-host drivers show 22
files "containing throws" to grep, but 0 real declarations — every hit is the self-host
compiler's own keyword table / string data recognising the `throws` token. The parser-based
count is authoritative.

### 1b. PROPAGATION call sites (semantic instrument)

A propagation site = the exact spot D29 makes `!` mandatory: a throws call auto-propagating in
a throws context (NOT a handling position). This is the single chokepoint
`resolve_throws_call_type` (`src/semantic/typecheck.rs:5431`), Route A —
`!suppress_auto_prop && !dest_is_result && current_fn_can_propagate()` → `auto_prop_error_gate`.
Method calls route through `resolve_throws_method_ret` → the same helper, so both free-fn and
method sites are covered by one instrument (env-gated `GG_D29_CENSUS`).

Reproduce (per-file `gg check`, count `[d29-prop]` lines; lib/self-host have 0 throws so
0 sites — no cross-file double-count since imported lib contributes nothing):

```
# for each file containing throws|Result[: GG_D29_CENSUS=1 gg check <f> 2>&1 | grep -c '^\[d29-prop\]'
```

| Corpus | prop-bearing files checked | **propagation sites** | files w/ sites |
|---|---|---|---|
| fixtures | 250 | **53** | 32 |
| spectests | 17 | **8** | 6 |
| lib / self-host | — | **0** | 0 |
| **IN-REPO TOTAL** | | **61** | 38 |

Caveat (honest): 61 is a measured floor. A negative-test fixture that fails its check
*before* the checker reaches a propagation expression won't emit that site (the instrument
fires during typecheck). Files that fail *after* the site still count. The undercount is
small and only affects files that would not compile post-migration anyway.

Secondary surface not in the 61: calls to *explicitly* `Result[T,E]`-returning functions
(non-`throws`) also auto-propagate per §10.1. Those take a different type path (no
`throws_type_id`) and are NOT in the count. The corpus has very few; whether D29's `!` extends
to them is an owner sub-call (recommend: yes, same rule — a fallible call is a fallible call).

**Migration size, in-repo: ~61 `!` insertions at call sites + 179 signature rewrites
(`throws E` → `! E`).** Trivial blast radius. The real work is the later gorget-js round
(113 decls + its propagation sites).

---

## 2. RECOMMENDED SIGNATURE GRAMMAR (with the A31 reservation)

EBNF (replaces `throws_clause = "throws" [ type ]`):

```ebnf
function_def  = ... "(" [ param_list ] ")" [ error_clause ] body ;
error_clause  = "!" [ type ] ;          (* accept-both also keeps: "throws" [type] *)
```

Semantics of `error_clause`:
- `! E`  — explicit error set `E` (paves nothing new; direct `throws E` replacement).
- `!`    — **A31 inferred error set** (bare `!:` / `!` before body). RESERVED; inference is
           NOT implemented — the checker treats the set as opaque for now. This *is* A31's
           surface, designed together per the owner sub-answer.
- absent — non-throwing.

Disambiguation is a clean one-token lookahead after `!`: if the next token opens the body
(`:`, `=` for extern, or NEWLINE for a bare decl) it's inferred; otherwise parse the error
type. Verified for every body form: block, expr-body (`int f()!: expr`), extern
(`= "sym"`), and trait-method declaration (no body). All parse (see §3 harness).

Placement rationale: `!` sits in the exact slot `throws` occupied (after `)`, before body),
so it composes unchanged with multi-return tuples (`String, int f() ! E:`), qualifiers
(`async int f() ! E:`), generics, and `main() ! int:` (exit-code form). It visually rhymes
with the call-site postfix `!` — both read "the error channel is here".

Zero bare `throws` (no error type) exists in-repo, so the signature migration is uniformly
`throws E → ! E`; no legacy inferred form to map.

---

## 3. COLLISION CORNERS — empirical dispositions

Every row is a live parse from `examples/d29_corners.rs` against the production parser
(accept-both build). `Prop(x)` = `Expr::Propagate`.

### Expression corners

| Corner | Input | Parsed AST | Disposition |
|---|---|---|---|
| **maximal munch** | `a()!=b` | `Neq(Call(a), b)` | `!=` lexes as one token (BangEq); postfix `!` is NOT seen. `a()!=b` stays a not-equal comparison. This is the correct, zero-change behavior. |
| eq-eq abut | `a()!==b` | **PARSE-ERR** | `!=` then `=` then `b` → invalid. Rare; write `a()! == b`. |
| propagate-then-neq | `a()! != b` | `Neq(Prop(Call(a)), b)` | A space frees `!` as postfix, then `!=`. This is how you propagate *and* compare. |
| propagate-then-eq | `a()! == b` | `Eq(Prop(Call(a)), b)` | works with the space. |
| **chain** | `f()!.m()!` | `Prop(MCall(Prop(Call(f)).m))` | chains left-to-right (postfix bp 35, same as `.`/call). Exactly `((f()!).m())!`. |
| chain + field | `a.b()!.c` | `Field(Prop(MCall(a.b)).c)` | `((a.b()!).c)`. |
| index + call | `v[i]()!` | `Prop(Call(v<[i]>))` | parses (note: `v[i]()` hits a *pre-existing* generic-call-vs-index ambiguity — see §7 bug). `!` wraps whatever `v[i]()` resolves to. |
| propagate + `??` | `f()! ?? d` | `Default(Prop(Call(f)), d)` | `(f()!) ?? d` — propagate the throw, then default the Option. |
| propagate + `?.` | `f()!?.field` | `OptChain(Prop(Call(f))?.field)` | `(f()!)?.field`. |
| nested calls | `g(f()!)!` | `Prop(Call(g,[Prop(Call(f))]))` | both marks attach to their own call; totality-uniform (D23). |
| unary minus | `-f()!` | `Neg(Prop(Call(f)))` | postfix (17) binds tighter than prefix `-` (16): `-(f()!)`. |
| prefix-move + postfix | `!x!` | `Move(Prop(x))` | prefix move of postfix-propagate. Post-D27 becomes `^x!`. |
| propagate-then-call | `foo!(x)` | `Call(Prop(foo),[x])` | `(foo!)(x)` — call the propagated value. Grammatically fine; niche. |
| plain | `f()!` | `Prop(Call(f))` | the base case. |
| additive | `a + f()!` | `Add(a, Prop(Call(f)))` | only the call is marked; `+` stays plain (contrast D26 `+!`). |

**Maximal-munch rule, stated:** the lexer never splits `!=`, `!==`… — `!` becomes a postfix
propagate token only when the following character is not `=`. Consequence: a throws call
directly abutting `=`/`==` requires a space (`f()! == b`); the fmt migration inserts `! `
(bang-space) so the mark never fuses. No lexer change is needed — this falls out of Logos
maximal munch automatically.

### Signature corners

| Corner | Input | Result |
|---|---|---|
| explicit `! E` | `int f() ! E:` | throws=Explicit(E) ✓ |
| inferred `!:` | `int f()!:` | throws=INFERRED ✓ (A31 reservation) |
| inferred + expr-body | `int f()!: 1 + 1` | throws=INFERRED ✓ |
| legacy (accept-both) | `int f() throws E:` | throws=Explicit(E) ✓ |
| main exit-code | `void main() ! int:` | throws=Explicit(int) ✓ |
| explicit + expr-body | `int f() ! E: 1 + 1` | throws=Explicit(E) ✓ |
| non-throwing | `int f():` | throws=None ✓ |
| generic error | `int f() ! AppError[int]:` | throws=Explicit(AppError[int]) ✓ |

### Function-type corners

| Corner | Input | Result | Disposition |
|---|---|---|---|
| bracketed inferred | `Callable[int(int)!] cb` | parses ✓ | **RECOMMENDED spelling** |
| bracketed explicit | `Callable[int(int)! E] cb` | parses ✓ | RECOMMENDED for explicit error |
| bare param inferred | `int(int)! cb` (param) | **PARSE-ERR** | collides with the `!` move-sigil in param position (pre-D27). See below. |
| non-throwing | `int(int) cb` | parses ✓ | unchanged |

**Function-type disposition:** in bare *param* position, `T(args)! name` collides three ways:
(1) pre-D27, `!` is the move ownership sigil (`Token !tok`); (2) even post-D27, the inferred
form `int(int)! name` cannot distinguish "inferred-throws param `name`" from "explicit-throws
`name`, param-name-missing". **Recommendation:** explicit throwing function types are spelled
ONLY in the bracketed `Callable[ret(args)! E]` form (the `]` delimits the error type
unambiguously); bare `ret(args)!` is inferred-only and legal only at a hard delimiter
(binding/return-type). This is also the idiomatic form the book already uses. The
bare-param collision then never arises. (The prototype greedily eats a bare-param `!`; the
corpus has ZERO function-type move-params, so this is zero-regression, but the production
rule should be the bracket recommendation above, not the prototype's greedy eat.)

---

## 4. SEMANTIC PINNING (design only)

### Handling forms post-D25 — the bare-call rule

D25 removes lexical fault-catch; faults reach values only at the D24 supervised boundary.
So the post-D25 forms that consume a *thrown error* without a `!` are:

| Form | §ref | Consumes bare call? | `!` needed? |
|---|---|---|---|
| propagate (throws ctx) | §10.1 | — | **YES** `f()!` |
| bare statement in throws ctx (result discarded) | §10.1 | — | **YES** `f()!` (still propagates) |
| `catch (e):` / `catch (_):` / block | §10.5 | **yes** | no |
| `rethrow E` / `rethrow (e): …` | §10.4 | **yes** | no |
| Result-typed binding capture (`Result[T,E] r = f()`) | §10.3 | **yes** | no |
| `on error:` cleanup | §10.7 | n/a (statement, not a call handler) | body calls still need `!` |
| D24 Task-join → `TaskFault` (phase 3) | §10.9 | — | future `task.join()!` |

Rule: **`!` marks *propagation*; the handling forms `catch`/`rethrow`/Result-capture consume
the bare call.** This is exactly Rust's `?`-vs-`match` split. The mark attaches to the CALL,
uniformly, in every D23 position (binding / operand / arg / return-tail / match scrutinee /
match arm / bare statement) — nested calls each carry their own (`g(f()!)!`).

### Diagnostics (message drafts)

- **Missing mark** (bare throws call in a throws context — today's invisible auto-prop):
  `error[E_UnhandledThrows]: this call can fail (throws E) — mark the propagation with '!'`
  `(f()!), or handle it with catch / rethrow / a Result[T,E] binding`.
  (Reuses D23's existing E_UnhandledThrows; the message flips from "declare throws or handle"
  to "propagate with `!` or handle it", per the owner sub-answer.)
- **Mark in non-throws context** (`!` where the fn can't propagate):
  `error[E_PropagateOutsideThrows]: '!' propagates an error, but <fn> is not declared`
  `throwing — declare it '! E' (or 'throws E'), or handle the error with 'catch'`.
- D23 diagnostic contract preserved: never surface `Result[` in a user-facing message.

### D23 totality interaction

D29 realises D23 syntactically: D23 says a throws call is type `T` in every position with
`Result`-ness unobservable; D29 requires exactly one `!` at each such call, in every position.
The mark is on the CALL regardless of surrounding context, so totality and visibility compose
without special cases. D26 fallible operators are the same rule at the operator glyph (`+!`),
so a fallible op is not double-marked.

---

## 5. READABILITY PAGES (full post-wave surface: D27 `^` + D22 `[a:b]` + D28 `**` + D29 `!`)

See the separate rendered file `/tmp/recover_d29_readability.md` (3 pages: book error example,
real self-host excerpt, dense synthetic sample), each with before/after side-by-sides.

---

## 6. MIGRATION PLAN sketch

- **Mechanical enumeration is PROVEN:** the census instrument IS the migration oracle — every
  Route-A site the checker flags is one `!` insertion; `resolve_throws_call_type` already
  centralises the class (one site, per the "fix the class" discipline).
- **`gg fmt` insertion:**
  1. Signatures: `throws E` → `! E` (regex-free AST rewrite; 179 in-repo).
  2. Call sites: at each flagged propagation span, insert `!` immediately after the call's
     closing `)` (or the postfix-chain segment that is the throws call). ~61 in-repo.
  3. **The one ambiguous insertion class:** a throws call whose result is immediately
     compared with `!=`/`==` with no space (`f() != b`). Inserting a bare `!` would fuse into
     `!=`. Fmt MUST insert `! ` (bang-space): `f()! != b`. Detectable mechanically (next
     non-space char is `=`). No other ambiguous class.
- Rides the C3 fmt vehicle (composes with D27/D22/D28 — the wave-census "composition test
  PASSED"), or a D29 sibling pass. In-repo only; gorget-js/arena/gglox/gconf in the later
  coordination round (owner ruling).

---

## 7. NEW pre-existing bug found (file-don't-fix)

**`v[i]()` — indexed-callable call misparses as a generic call.** `fs[i](10)` where
`fs: Vector[int(int)]` fails with `error[E_NotAFunction]: 'fs' is not a function`: the
postfix `[...]` disambiguator (`src/parser/expr.rs:1257`, the `Token::LBracket` arm) greedily
tries `fs[i](args)` as a generic call (parsing `i` as a type arg) before index-then-call.
Workaround: bind first (`int(int) f = fs[i]` then `f(10)` — verified works). Independent of
D29 but adjacent to the `v[i]()!` corner. Recommend a TODO entry: "index-vs-generic-call:
`v[i](...)` on a non-generic indexable should fall back to index-then-call when the callee
isn't a generic function".

---

## 8. Gates (all green)

- `cargo build` — clean.
- `cargo test --lib` — 1107/0.
- `cargo test --lib parser` — 103/0.
- Full-corpus parse sweep (`examples/d29_listfail.rs`): only the 8 intentionally-negative
  `*_error*` / `security/attack_*` fixtures fail to parse (pre-existing); **zero new parse
  regressions**. Changes are provably additive (postfix/signature/fn-type `!` all sit in
  previously-error positions; 0 function-type move-params in the corpus).
- End-to-end: legacy `throws` fixture builds+runs unchanged; a NEW postfix-`!` program AND a
  `! E`-signature program both parse → check → build → RUN with correct output
  (`outer(5)=11`, caught `outer(-3)=-99`); bare `!:` inferred form parses + checks.
