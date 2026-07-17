# D29 RATIFICATION PACKET — visible error propagation (`f()!`; `!` JOINS `throws`)

Scout deliverable + **currency write-through 2026-07-16** (post owner amendments).

**Normative authority:** `docs/plans/define-gorget/decisions.md` LOG — D29 formal
ratification + catch-attachment pin (2026-07-16 session). Where this packet's historical
prototype text diverges, the LOG wins. This file remains census + grammar evidence.

**Ratified surface (do not re-litigate in the implementation brief):**
- Call-site postfix `!` is MANDATORY on every fallible use (Swift always-mark).
- `throws E` REMAINS the explicit contract spelling on declarations.
- Bare signature `!:` is grammar-locked for A31 inferred sets only (parses; teaching reject until A31).
- `!` NEVER takes a type on a signature (`! E` was a prototype spelling — **cancelled**).
- Implementation scope = **CALL-SITES ONLY** (signature migration cancelled).
- Disposition attaches to the **marked** expression (bare fallible call always illegal).

Original scout direction was "D29 DIRECTION AGREED" (including a later-amended
signature-replace path). Amendments consolidated in the LOG: `!` joins `throws`, does not
replace it.

Prototype patch: `/tmp/recover_d29_proto.patch` (566 lines; builds clean, `cargo test --lib`
1107/0, parser suite 103/0, end-to-end build+run verified). Instruments/tooling:
`examples/d29_census.rs`, `examples/d29_corners.rs`, `examples/d29_listfail.rs`.
Accept-both prototype also parsed `! E` signatures — **historical only**; not v1 surface.

---

## 0. TL;DR recommendations (currency)

1. **Call sites** — postfix `!` on every fallible call: `f()!`, `a.m()!`, `g(f()!)!`,
   including handled forms (`f()! catch (e): …`, `Result[T,E] r = f()!`). Additive to the
   grammar (postfix `!` was a parse error before; adding it cannot change any currently-valid
   parse). `!=` keeps lexing as one token (`BangEq`) under Logos maximal munch, so `a()!=b`
   stays a not-equal comparison — the only real corner, resolved by "insert `! ` (bang-space)".
2. **Signatures** — **unchanged explicit form:** `int f(args) throws E:`. Bare
   `int f(args)!:` is reserved for A31 (inferred error set); until A31, parse + teaching
   reject ("declare `throws E`"). Do **not** migrate `throws E` → `! E`.
3. **Function types** — fallible callables keep effect visibility at the type; prefer
   bracketed forms that do not collide with param-position sigils. A31 may pin
   `Callable[int(int)!]` for inferred-throws callables; explicit-error callables stay
   keyword/typed-contract shaped until A31 designs the type spelling. Bare-param
   `int(int)! name` stays rejected (ambiguous). See §3 historical prototype notes.
4. **D26 operators** are the same rule at a different operation-glyph (`+!` = mark on `+`);
   no double-marking (`a +! b`, never `(a +! b)!`). Not implemented in this prototype
   (its `+!` lexing collides with pre-D27 prefix-move `!b`; lands with C1+D27).
5. **Disposition** — bare fallible call always illegal; `catch` / `rethrow` / Result capture
   attach to the marked expression (see §4).

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
`throws_type_id`) and were NOT in the prop census. **RATIFIED 2026-07-16 (owner): same
rule — mandatory `!` on both throws callees and declared-`Result[T,E]`-returning
calls/methods.** Instruments/fmt must cover both paths; `Result` remains a value type.

**Migration size, in-repo (ratified scope): call-site `!` insertions only** — ~61
propagation sites (floor) + handled-sites count still to measure. **No signature rewrites**
(`throws E` stays). Trivial blast radius. The real work is the later gorget-js round
(113 decls + its propagation/handled sites) and, after D17, stdlib call sites.

The 179 throws **declarations** remain a census fact (how many fallible APIs exist), not
a migration worklist.

---

## 2. SIGNATURE GRAMMAR — ratified vs historical prototype

### 2a. Ratified v1 surface (implementation target)

```ebnf
function_def  = ... "(" [ param_list ] ")" [ throws_clause ] body ;
throws_clause = "throws" type ;     (* explicit contract — unchanged *)
(* future A31: bare "!" before body = inferred error set; teaching-reject until then *)
```

- `throws E` — explicit error contract (public APIs, all current code).
- `!:` / bare signature `!` before body — **A31 reservation only**; grammar may lock now;
  checker rejects with "inferred error sets are not yet implemented — declare `throws E`".
- absent — non-throwing.
- **`! E` (sigil + type) is NOT v1** — cancelled by the 2026-07-16 amendment.

`main() throws int:` stays the exit-code form.

### 2b. Historical accept-both prototype (evidence only — not v1)

The scout prototype also parsed signature `! E` / `!:` as an alternate throws clause to
prove lookahead and collision corners. That experiment is **superseded**:

| Prototype spelling | Ratified disposition |
|---|---|
| `int f() ! E:` | **cancelled** — write `throws E` |
| `int f()!:` | A31 only (teaching reject until inference) |
| `int f() throws E:` | **v1 explicit contract** |
| `void main() ! int:` | write `throws int` |

Do not cite §2b tables as the shipping surface.

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

### Signature corners (prototype harness — historical)

The accept-both prototype accepted the rows below. **Shipping rows are marked.**

| Corner | Input | Prototype | v1 disposition |
|---|---|---|---|
| explicit keyword | `int f() throws E:` | throws=Explicit(E) ✓ | **SHIP** |
| main exit-code | `void main() throws int:` | throws=Explicit(int) ✓ | **SHIP** |
| non-throwing | `int f():` | throws=None ✓ | **SHIP** |
| inferred `!:` | `int f()!:` | throws=INFERRED ✓ | A31 reservation (teaching reject until then) |
| prototype `! E` | `int f() ! E:` | throws=Explicit(E) ✓ | **CANCELLED** — use `throws E` |
| prototype main `! int` | `void main() ! int:` | throws=Explicit(int) ✓ | **CANCELLED** — use `throws int` |

### Function-type corners

| Corner | Input | Result | Disposition |
|---|---|---|---|
| non-throwing | `int(int) cb` | parses ✓ | unchanged |
| bare param `!` | `int(int)! cb` (param) | **PARSE-ERR** | stays rejected (ambiguous with sigils) |
| bracketed `!` forms | `Callable[int(int)!]` etc. | prototype parses | A31 / type-spelling follow-up — not a D29 call-site deliverable |

**Function-type note:** D29 call-sites do not require shipping a new callable-type spelling.
A31 should design effect-on-callable-types together with inferred sets. Until then, existing
`throws` on function values / book forms stand; bare-param `T(args)! name` remains illegal.

---

## 4. SEMANTIC PINNING (ratified — owner 2026-07-16)

### Fallible-use mark + disposition (always-mark; bare call always illegal)

D25 removes lexical fault-catch; faults reach values only at the D24 supervised boundary.
D29: every fallible call carries `!`. Disposition attaches to the **marked** expression
(Swift model — **not** Rust's `?`-vs-`match` split). The scout's earlier "handlers eat bare
calls" wording is **superseded** by the catch-attachment pin in `decisions.md`.

| Disposition | Spelling | Notes |
|---|---|---|
| Propagate | `f()!` inside `throws E` | auto-prop under the mark |
| Bare statement (discard Ok, prop Error) | `f()!` | still marked |
| Recover | `f()! catch (e): fallback` | postfix `catch` on the marked expr |
| Transform + rethrow | `f()! rethrow (e): wrap(e)` | postfix `rethrow` on the marked expr |
| Capture as data | `Result[int, Error] r = f()` — **UNMARKED (2026-07-17 CAPTURE AMENDMENT)** | an EXPLICITLY Result-annotated destination captures without the mark; mark + Result destination together is an ERROR (fix-it: remove the `!`) |
| `on error:` cleanup | body calls still use `!` | cleanup is not a call handler |
| D24 Task-join (phase 3) | future `task.join()!` | supervised boundary |

**Precedence:** `!` binds to the call first; then `catch` / `rethrow` attach to that marked
expression (`(f()!) catch …`). Nested calls each carry their own mark:
`g(f()!)! catch (e): …`.

**⚠ 2026-07-17 CAPTURE AMENDMENT (owner-ratified — the LOG entry is normative; supersedes
this section's original "still requires `!`" capture row and the "no handle without `!`"
absolutism for the capture form ONLY):** `!` marks error-channel ACTIVATION (propagate /
catch / rethrow — control flow on the Error case), both call kinds. Value-plane capture by
an explicitly Result-annotated destination (binding / param / return) is legal UNMARKED —
the annotation carries the visibility. Inferred/`auto` destinations don't capture (type as
`T`, mark required); bare-discard is illegal both kinds; match scrutinees stay `T`-typed
per D23 (bind first to match the Result); kind-2 calls stay Result-typed everywhere, their
`!` peels + activates. Consequence: the kind-2 lib migration collapses (bind/match/pass/
combinator sites unchanged); migration = the 267 throws-kind marks + the kind-2
bare-discard census.

**Bare fallible call otherwise = always an error** ("mark the fallible call `f()!`, handle
it, or capture it: `Result[T,E] r = f()`" — `E_MissingFallibleMark` teaches all three).

The mark attaches to the CALL uniformly in every D23 position (binding / operand / arg /
return-tail / match scrutinee / match arm / bare statement).

### Diagnostics (message drafts — currency)

Prefer **`throws E`** as the contract teaching spelling; do not push bare `!:` until A31.
Two codes per the LOG's "D29 DIAGNOSTIC CODES SPLIT" pin (2026-07-16), not one template:

- **Missing mark** (bare fallible call anywhere) — **`E_MissingFallibleMark`** (new code):
  `error[E_MissingFallibleMark]: this call can fail (throws E) — mark the fallible call with '!'`
  `(f()!), or handle it: f()! catch … / f()! rethrow … / Result[T,E] r = f()!`.
- **Marked call that cannot propagate here** (non-`throws` fn, no disposition) —
  **`E_UnhandledThrows`** (existing code, message flips):
  `error[E_UnhandledThrows]: this call can fail (throws E) — handle it with catch /`
  `rethrow / a Result[T,E] binding, or declare the function 'throws E' to propagate`.
- D23 diagnostic contract preserved: never surface `Result[` in a user-facing message for
  unhandled-throws (Result capture is an intentional disposition spelling).

### D23 totality interaction

D29 realises D23 syntactically: D23 says a throws call is type `T` in every position with
`Result`-ness unobservable; D29 requires exactly one `!` at each such call, in every position
(including handled dispositions). The mark is on the CALL regardless of surrounding context,
so totality and visibility compose without special cases. D26 fallible operators are the same
rule at the operator glyph (`+!`), so a fallible op is not double-marked.

---

## 5. READABILITY PAGES (full post-wave surface: D27 `^` + D22 `[a:b]` + D28 `**` + D29 `!`)

See `scouts/scout-d29-readability.md` (currency write-through 2026-07-16: signatures stay
`throws E`; call sites carry `!`). Three pages: book error example, real self-host excerpt,
dense synthetic sample — each with before/after side-by-sides.

---

## 6. MIGRATION PLAN sketch (call-sites only)

- **Mechanical enumeration is PROVEN:** the census instrument IS the migration oracle — every
  Route-A site the checker flags is one `!` insertion; `resolve_throws_call_type` already
  centralises the class (one site, per the "fix the class" discipline). Extend the instrument
  to **handled** sites (catch / rethrow / Result capture) so always-mark insertion is complete.
- **`gg fmt` insertion:**
  1. **Signatures: NO rewrite** — leave `throws E` as-is.
  2. Call sites: at each fallible call (propagation **and** handled dispositions), insert `!`
     immediately after the call's closing `)` (or the postfix-chain segment that is the
     throws call). ~61 prop sites measured; handled-sites census pending.
  3. **The one ambiguous insertion class:** a throws call whose result is immediately
     compared with `!=`/`==` with no space (`f() != b`). Inserting a bare `!` would fuse into
     `!=`. Fmt MUST insert `! ` (bang-space): `f()! != b`. Detectable mechanically (next
     non-space char is `=`). No other ambiguous class.
- Rides the C3 fmt vehicle (composes with D27/D22/D28 — the wave-census "composition test
  PASSED"), or a D29 sibling pass sequenced **before** C1/C3. In-repo only first;
  gorget-js/arena/gglox/gconf in the later coordination round (owner ruling).

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
