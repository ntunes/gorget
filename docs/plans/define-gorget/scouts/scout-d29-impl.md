# D29 IMPLEMENTATION SCOUT — call-site `!` (visible error propagation)

Scout deliverable for the D29 implementation BRIEF. Verification + census + prototype +
surface-map. NOT design (D29 is RATIFIED). Checkpointed after every step.

Worktree: `/workspace/gorget/.claude/worktrees/agent-a40f3291754ce00c3`
Date: 2026-07-16.

---

## DOC GROUNDING (cited; the design rests on these)

- `decisions.md` LOG 2026-07-16 — D29 formal ratification (line 396) + the six follow-through
  pins (all postdate the ratification, all owner-confirmed):
  - **Catch-attachment / disposition grammar** (line 384): bare fallible call ALWAYS illegal;
    disposition attaches to the MARKED expr; precedence `(f()!) catch …`; nested `g(f()!)! catch`.
  - **Packet/readability write-through DONE** (line 378).
  - **One mark for BOTH call kinds** (line 364): mark applies to throws-callee calls AND
    declared-`Result[T,E]`-return calls; `Result` stays a first-class value type.
  - **Diagnostic codes SPLIT** (line 352): `E_MissingFallibleMark` (new, bare fallible call) +
    `E_UnhandledThrows` (existing, message flips). Never surface `Result[` in these messages.
  - **D29 ↔ D17 sequencing + dogfood hardening** (line 337): D29 lands first (small blast
    radius, lib/self-host have zero throws); D17 class sweep is the dogfood gate; ship an
    integration hardening fixture with a thin local `throws` wrapper exercising always-mark +
    all dispositions end-to-end.
  - (packet currency — packet §0 marks `! E` signature rows CANCELLED.)
- `decisions.md` A31 reservation (line 404-428): `!` NEVER takes a type; bare `int f()!:`
  grammar-locks for A31 inferred error sets (parses, teaching-rejects until A31); `throws E`
  REMAINS the explicit contract spelling. `!` JOINS `throws` (sigils = flow at use-sites;
  keywords = contracts at declarations).
- `scout-d29-packet.md` — census method (§1), grammar evidence (§2/§3), migration (§6). NOTE
  §2b + `! E` rows are CANCELLED history; v1 = call-sites only, `throws E` unchanged.
- `scout-d29-readability.md` — post-D29 surface pages.
- `docs/language-reference.md` §10 (lines 2489-2666) — error-handling sections the landing
  updates. KEY: the existing `catch`/`rethrow` grammar is ALREADY postfix-expression
  attachment (`read_file(path) rethrow 1`, `parse_port(input) catch (e): 8080`, §10.4/§10.5) —
  so D29's ONLY new grammar is the postfix `!`; dispositions already attach postfix.
- CLAUDE.md Core invariant #9 — SEMANTIC change: lands on ggdef (subset), Rust gg (C+LLVM),
  self-host in ONE round, pinned by cross-lane fixtures.

### FLAG — my-summary vs LOG mismatches: NONE.
The brief's "ratified semantics" summary matches the LOG exactly. Verified point-by-point:
mandatory postfix `!` on every fallible call (throws-decl OR declared `Result[T,E]` return);
bare call → new `E_MissingFallibleMark`; marked-but-cannot-propagate → `E_UnhandledThrows`
(message flips); dispositions attach to marked expr; `throws E` signatures unchanged; bare
`int f()!:` parses + teaching-rejects (A31); `! E` does not exist; `!=` stays one token; HOF/A32
out of this track.

---

## STEP 0 — INSTRUMENT STATUS (finding)

**The census instruments do NOT exist in the worktree or in git history.**
`examples/d29_census.rs`, `d29_corners.rs`, `d29_listfail.rs` — `find` + `git log --all` both
empty. The packet (line 22-23) cites them; the round-close pruned them (docs-plans-hygiene) and
they were never committed. **Consequence for the brief: the census instrument must be rebuilt
(or the numbers re-derived by another method). I rebuild a minimal parser-based census below.**
The historical prototype patch `/tmp/recover_d29_proto.patch` (packet line 21) is also gone;
the reusable half survives as `docs/plans/define-gorget/scouts/patches/d29_acceptboth_proto.patch`
(580 lines).

<!-- checkpoint: step 0 done -->

---

## STEP 1 — CENSUS RE-VERIFIED (fresh numbers, this session)

Instrument REBUILT: `examples/d29_census.rs` (parser-based; walks free fns + equip methods +
trait methods + extern-block + nested modules; counts `throws` decls, declared-`Result[T,E]`
returns with no throws, and bare `!:`). Reproduce:

```
cargo build --example d29_census
./target/debug/examples/d29_census "fixtures=tests/fixtures" "spectests=spectests" "lib=lib" \
    "self_host_lowerer=tests/fixtures/self_host_lowerer" ... "examples=examples"
```

Fresh output (2026-07-16):

| Corpus | files | fn defs | **throws decls** | **Result-ret (no throws)** | inferred `!:` |
|---|---|---|---|---|---|
| fixtures (incl. self_host; self_host = 0) | 1886 | 6272 | **163** | **55** | 0 |
| spectests | 214 | 450 | **16** | **5** | 0 |
| lib (stdlib) | 60 | 1989 | **0** | **146** | 0 |
| self_host (all 6 dirs) | 94 | 2661 | 0 | 0 | 0 |
| examples | 42 | 127 | 0 | 0 | 0 |
| **IN-REPO TOTAL** | | | **179** | **206** | 0 |

- **throws-decl count = 179 — EXACTLY matches the packet.** Stable premise. (163 fixtures +
  16 spectests; lib/self-host/examples contribute 0.)
- Out-of-repo corpuses (gorget-js, gorget-arena, gglox, gorget-conformance) are **NOT present**
  in this worktree — the later coordination-round migration cannot be measured here (as the
  packet noted). gorget-js's 113 throws decls stand as the packet's read-only figure.

### 🔴 FINDING 1 — the SECOND fallibility kind is NOT "very few": 206 in-repo decls (lib alone 146).
The packet (§1b) hand-waved declared-`Result`-returning calls as "very few". That is WRONG at
the DECLARATION level. lib has **zero** `throws` but **146** `Result[T,E]`-returning functions —
these are the genuine data-first fallible APIs: `parse_int`/`parse_float` (`lib/std/conv.gg`),
`file_open`/`read_to_string`/`write_string` (`lib/std/io.gg`), `url_decode`/`form_decode`
(`lib/std/encoding.gg`), `http_single`/`get`/`post` (`lib/xtd/http.gg`), `xml_parse`,
`df_from_csv`, `parse_table`, `open` (sqlite/gfx/influx), `Socket.read`/`.write`, etc. Under the
ratified "one mark for BOTH kinds", **every call to any of these needs `!`**. The migration
surface for kind-2 is real and lib-centred; the packet's throws-only 61 propagation count is a
severe undercount of the true call-site work. (Call-site counts: Step 2.)

### 🔴 FINDING 2 — the COMBINATOR TENSION: the rule as literally worded sweeps in Result/Option combinators.
`Result.map`/`.and_then`/`.or_else`/`.map_err` and `Option.map`/`.and_then`/`.or_else` are
BUILTIN methods (`src/semantic/typecheck.rs:6252-6319`, `infer_closure_method_return`) whose
declared return type IS `Result[U,E]`. A literal reading of "calls/methods whose declared return
type is Result[T,E]" would force `r.and_then(f)!` — absurd (a combinator does not "fail", it
threads a Result as data). The LOG's scope note ("**not** every expression of type Result;
locals/**combinators are separate**", line 370-371) says they're excluded, but gives the checker
no predicate. **The brief MUST pin the concrete predicate.** Recommended (typed, not
name-matched): a call is a *fallible producer* iff its callee's declared return is `Result[T,E]`
AND the callee is **not a combinator method whose receiver type is `Result`/`Option`**. i.e.
*receiver-is-Result/Option ⟹ combinator ⟹ no mark*; *free fn / method on any other receiver with
Result return ⟹ fallible ⟹ mark*. This falls out of the receiver type (already resolved), needs
no name list, and matches "Result stays a value type; holding/threading one is not a call". The
brief should confirm against the combinator set at `typecheck.rs:6252-6319` + spec §7.13/§10.3.

<!-- checkpoint: step 1 done -->

---

## STEP 4 — ENFORCEMENT SURFACE MAP (file:line, verified this session)

### Where the checker centrally sees "this call is fallible"

**Kind 1 (throws callee): ONE chokepoint — CONFIRMED, but the packet's line is stale.**
`resolve_throws_call_type` is now at **`src/semantic/typecheck.rs:5499`** (packet said 5431).
Every throws call funnels here:
- Free-fn call: `typecheck.rs:2073` (guarded by `func_info.throws_type_id.is_some()`, else 2079
  returns bare `return_type`).
- Method calls: three sites — primary `2462`, trait-default `2518`, cross-module-equip `2591` —
  all via `resolve_throws_method_ret` (`5565`) → `resolve_throws_call_type`.
Inside `resolve_throws_call_type` (5499-5538) the three branches are the exact enforcement fork:
- `suppress_auto_prop || dest_is_result` (5514) → **HANDLED** (catch/rethrow set
  `suppress_auto_prop`; Result-capture sets `dest_is_result`). D29: legal, but the call must
  still carry `!`.
- `current_fn_can_propagate()` (5518) → **PROPAGATE** (Route A). D29: legal iff marked.
- else (5524) → today emits `E_UnhandledThrows` (`SemanticErrorKind::UnhandledThrows`).
An arm-count lint already pins the method sites: `method_throws_return_sites` (referenced in the
5540-doc). D29 enforcement (require `!`) folds in cleanly here: the `!` presence is known from
the AST (`Expr::Propagate` wrapper) at the call node; the checker gates on it.

**Kind 2 (declared-`Result`-return callee): NO chokepoint exists — must be BUILT.** Confirmed
by the comment at `typecheck.rs:2061-2065`: "the peel is gated to *throws*-fn calls, so an
explicit `Result`-returning fn ... is never peeled". So a call to `parse_int(s)` today just
yields a raw `Result[int,ParseError]` value; there is no fallibility detection. For D29 kind-2,
a NEW detection point is required at the call sites:
- Free-fn call `else` branch (`typecheck.rs:2079`): when `return_type` resolves to `Result[_,_]`
  and the callee is a user/lib function (not a combinator), this is a fallible call.
- Method fallbacks (`2531` `infer_closure_method_type`; `2550` `builtin_method_type`; `2582`
  cross-module by-name): a method whose declared return is `Result[_,_]` and whose **receiver is
  NOT Result/Option** is a fallible producer.
Per the sibling-site rule, the class-fix is a shared helper `mark_required_for_result_return()`
(or extend `resolve_throws_call_type` to also accept a "callee-returns-Result" flag) with an
arm-count lint pinning the free-fn + 3 method sites, mirroring the throws arm-count lint.

### Diagnostic code registry
`src/semantic/errors.rs` — this is `SemanticErrorKind`. Current `E_`/code registry: verified
below (Step 4 run). `E_UnhandledThrows` exists (`SemanticErrorKind::UnhandledThrows`); the new
`E_MissingFallibleMark` must be added there + `spec/prose/diagnostic-codes.md` gains a row.
D23/smith ratchets that assert on these codes: `tests/lints.rs`, spectests.

### `gg fmt` insertion hook
`src/formatter/mod.rs` (`format_expr`) already has an `Expr::Propagate` arm in the proto (emits
inner then `!`). Mechanical migration = the checker's own missing-mark diagnostics as the oracle
(no separate fmt pass logic needed for enumeration). Bang-space corner handled by the lexer
(never splits `!=`); fmt must emit `! ` when the next non-space char is `=`.

### 🟢 FINDING 3 — the disposition grammar ALREADY EXISTS as postfix; D29 adds only the `!`.
The existing `catch`/`rethrow` are ALREADY postfix-expression forms wrapping an inner expr
(`Expr::Catch { expr, error_binding, recovery }` / `Expr::Rethrow { expr, error_binding,
transform }`, `ast.rs:577/585`; langref §10.4/§10.5: `read_file(path) rethrow 1`,
`parse_port(input) catch (e): 8080`). They set `suppress_auto_prop=true` on the inner
(`typecheck.rs:3728/3775`). So the ratified precedence `(f()!) catch …` is ALREADY the grammar
shape — the ONLY new token is the postfix `!` between the call and the disposition. In the proto,
`f()! catch (e): …` parses as `Catch { expr: Propagate { Call(f) }, … }` for free (postfix `!`
binds at bp 35, tighter than the catch/rethrow statement-level attach). This SHRINKS the grammar
work dramatically vs. the packet's framing. Verified by prototype test below (Step 3).

<!-- checkpoint: step 4 partial (surface map) done -->

---

## STEP 3 — GRAMMAR PROTOTYPE (built + tested this session, in my worktree)

Prototype (patch: `/tmp/d29_impl_proto.patch`) adds `Expr::Propagate { expr }` + postfix `!`
at bp 35, the bare `!:` A31 signature reservation, and env-gated census + enforcement. Reuses
the call-site half of `d29_acceptboth_proto.patch`; does NOT port the CANCELLED `! E` signature
or bare-param fn-type `!` halves. Files touched (all `Expr` consumers needing a `Propagate` arm):
`parser/{ast,expr,mod,visitor}.rs`, `semantic/{typecheck,meta,resolve,rewrite,safety/check_expr}.rs`,
`ir/lowering/exprs/mod.rs`, `loader.rs`, `formatter/mod.rs`, `tests/integration.rs`
(canonical), + `examples/d29_census.rs`. **No new non-exhaustive-match breakage beyond the
patch's set** — the same ~13 arms still cover it (sim/bir/lir/backend either wildcard or don't
match `Expr`).

### Gates (FOREGROUND, this session)
- `cargo build --lib` — clean.
- `cargo test --lib` — **1119 passed / 0 failed** (1107 baseline + 12 new D29 tests).
- `cargo test --lib parser` — passes (103 pre-existing + 12 D29).

### The 12 D29 parser tests (`src/parser/tests.rs`, all green) — corner dispositions PROVEN:
| Test | Input | Parsed | Verdict |
|---|---|---|---|
| plain | `f()!` | `Propagate(Call)` | ✓ base case |
| method | `a.m()!` | `Propagate(MethodCall)` | ✓ |
| nested | `g(f()!)!` | `Propagate(Call(g,[Propagate(Call(f))]))` | ✓ each call its own mark |
| chain | `f()!.m()!` | `Propagate(MCall(Propagate(Call(f)).m))` | ✓ left-to-right, bp 35 |
| **maximal munch** | `a()!=b` | `Neq(Call(a), b)` — **NO Propagate** | ✓ `!=` one token; comparison unchanged |
| bang-before-neq | `f()!= b` | `Neq(Call(f), b)` | ✓ still fuses (`!=`); no mark |
| prop-then-eqeq | `f()! == b` | `Eq(Propagate(Call(f)), b)` | ✓ space frees `!` as postfix |
| **catch attach** | `f()! catch (e): 0` | `Catch{ expr: Propagate(Call(f)) }` | ✓ disposition on the MARKED expr |
| **rethrow attach** | `f()! rethrow 1` | `Rethrow{ expr: Propagate(Call(f)) }` | ✓ (bare form) |
| sig bare `!:` | `int f()!:` | `throws = Named("!inferred")` | ✓ parses (A31 reservation) |
| sig `! E` | `int f() ! E:` | **PARSE-ERR** | ✓ cancelled form rejected |
| sig `throws E` | `int f() throws E:` | `throws = Named("E")` | ✓ unchanged |

**Key confirmations for the brief:**
- `!=` maximal-munch corner needs NO lexer change — falls out of Logos (`BangEq` never splits).
  The only fmt insertion corner is bang-space before `=` (emit `! ` not `!`).
- `catch`/`rethrow` disposition attachment is FREE (postfix `!` at bp 35 nests inside the
  bp-1 infix `catch`/`rethrow`) — the ratified precedence `(f()!) catch …` is exactly what parses.
- Bare `int f()!:` parses to a sentinel `Named("!inferred")` in the `throws` field; the checker
  teaching-reject is a small follow-up (detect the sentinel → emit the A31 diagnostic). Production
  should replace the sentinel with a proper `ThrowsSpec::{No, Inferred, Explicit(Type)}` enum
  (the string sentinel is a prototype shortcut; a name-matched sentinel violates layering rule 2).

<!-- checkpoint: step 3 done -->

---

## STEP 2 — CALL-SITE CENSUS (measured this session, env-gated semantic instrument)

Instrument = env-gated `eprintln` counters wired at the EXACT enforcement points (proto patch):
- Throws kind, in `resolve_throws_call_type` (5499): `[d29-prop]` (Route A), `[d29-handled]`
  (suppress/`dest_is_result`), `[d29-unhandled]` (today's error).
- Result kind (NEW detection, no chokepoint existed): `[d29-result-freefn]` (free-fn non-throws
  returning Result) + `[d29-result-method]` (method non-throws returning Result, receiver not
  Result/Option).
Reproduce: `GG_D29_CENSUS=1 gg check <file>` and count the lines.

### Throws kind (CLEAN — lib/self-host have 0 throws, so no cross-file double-count)
| Corpus | files w/ sites | **PROP** | **HANDLED** | UNHANDLED |
|---|---|---|---|---|
| fixtures | 84 | 53 | 194 | 13 |
| spectests | 8 | 8 | 12 | 0 |
| **TOTAL** | | **61** | **206** | 13 |

- **PROP = 61 — reproduces the packet's 61 EXACTLY.** ✓
- **HANDLED = 206** — this is the PINNED-OPEN "handled-sites census" (catch / rethrow /
  Result-capture on a throws call). It was never measured before. **So the throws-kind in-repo
  migration is 61 + 206 = 267 marks, NOT the packet's headline "~61".**
- UNHANDLED = 13 — negative fixtures asserting today's `E_UnhandledThrows` on a bare
  no-disposition call; under D29 these flip to `E_MissingFallibleMark` (bare) — fixture-expectation
  migration, not a `!` insert.

### Result kind (kind-2) — LARGE, lib-dominated; census over-counts via re-checked imports
| Measurement | count | note |
|---|---|---|
| lib Result-returning DECLARATIONS (Step 1) | 206 in-repo (146 lib) | the fallible-API supply |
| lib module internal kind-2 CALLS (per module, incl. its imports) | http.gg 37 · httpserver.gg 53 · io/conv/socket 9 ea · json 11 · yaml 10 | lib is saturated with Result calls |
| examples (self-contained) apparent | 130 | but mostly re-counted lib internals (http_get own ≈ 1) |
| fixtures marker-subset (245 files) raw | 2103 | **heavily double-counted**: httpserver_basic shows 58 but its OWN file has ~4 |

**MEASUREMENT CAVEAT (honest):** `gg check <fixture>` re-typechecks the imported lib bodies, so
kind-2 counts aggregated across importers double-count lib internals (proof: httpserver_basic
own-file Result calls ≈ 4, census = 58; two different importers of the same lib share ~50 common
counts). A CLEAN per-corpus kind-2 count needs per-file span attribution — a follow-up instrument
the brief should specify. What IS solid: kind-2 is **not "very few"** — lib is essentially
all-Result, dozens of internal calls per module.

### 🔴🔴 FINDING 4 (HEADLINE) — the D17-sequencing "small blast radius" premise is INVALIDATED by kind-2.
The ratified D29↔D17 pin (decisions.md:337) says "D29 lands first — small blast radius while
lib/self-host have zero throws". That reasoning only holds for the THROWS kind. The **one-mark-
for-both-kinds** rule means every call to a `Result[T,E]`-returning function needs `!` NOW —
and lib already ships **146** such APIs (parse_int, file_open, http.get, xml_parse, Socket.read,
…) whose internal callers AND every user call site must be marked, independent of D17. So the
in-repo D29 landing is NOT trivial: it must migrate all of lib's internal Result-call sites plus
every fixture/example/spectest call to a lib Result API. **The brief MUST resolve the sequencing:**
either (a) D29 lands with lib fully kind-2-migrated (large, lib-wide `!` insertion — but lib is
"in-repo", so it's in scope), or (b) enforcement is STAGED (throws-kind `!` enforced first;
Result-kind `!` enforced when the fmt sweep + lib migration land together) — but (b) softens the
"one uniform rule" and must be an explicit owner decision, not a silent gap. Recommendation: pin
this before writing the enforcement brief; do NOT carry the packet's "trivial blast radius, ~61".

<!-- checkpoint: step 2 done -->

---

## ENFORCEMENT PROTOTYPE + 🔴 FINDING 5 — the two-layer `Propagate`-transparency requirement

The prototype threads the mark from `Expr::Propagate` through a one-shot `fallible_call_marked`
(mirror of `suppress_auto_prop`) to the chokepoint, and env-gates enforcement (`GG_D29_ENFORCE`)
so the suite stays green while the corpus is unmarked. Verified end-to-end (`gg check`/`gg run`):
- BARE `int v = risky(x)` under `GG_D29_ENFORCE` → error fires (proto piggybacks the message on
  `E_UnhandledThrows` with a `MISSING-MARK:` sentinel; production adds the distinct
  `E_MissingFallibleMark` code + message).
- MARKED `risky(x)!` → clean.
- Un-enforced (default) → clean; **`cargo test --lib` = 1119/0** (no regression).

### 🔴 FINDING 5 (blast-radius, layering) — `Propagate` must be TRANSPARENT to the `suppress_auto_prop` one-shot in BOTH the typechecker AND the IR lowerer.
The historical proto (`d29_acceptboth_proto.patch`) lowered `Propagate` as a naive
`=> lower_expr(inner)` and typed it as `=> infer_expr(inner)`. Both are WRONG for handled
dispositions: the `Propagate` node sits BETWEEN a disposition (`catch`/`rethrow`/match-scrutinee)
and its call, and the one-shot `suppress_auto_prop` (which tells the call "keep the raw Result so
the disposition can read it") is consumed by the intervening `Propagate` frame — so the call
auto-props to `T`, and:
- typecheck side: `int a = risky(5)! catch (e): 0` → spurious semantic error (call typed as
  unhandled/mismatched throws). **Scout-reproduced.**
- lowering side: `match risky(5)!:` with `Ok`/`Error` arms → **SIGSEGV at runtime** (the match
  reads a peeled `int` as a Result discriminant). **Scout-reproduced (RUN-EXIT=139).** `gg check`
  passes — so a test that only checks, not runs, would miss it. (This is exactly the Core-#7
  "gate on running, not a green check" lesson.)
The fix (applied in the proto, verified): forward the one-shot through `Propagate` on BOTH sides
— typecheck `Expr::Propagate` arm re-sets `self.suppress_auto_prop = <captured>` before inferring
inner (`typecheck.rs` infer arm); lowering intercepts `Propagate` at the TOP of `lower_expr`
BEFORE the `mem::replace` reset (`ir/lowering/exprs/mod.rs`). After the fix, all four dispositions
RUN correctly: propagate, `catch`, `rethrow`, match-scrutinee, Result-capture (`ok 5` / `z=3` /
`a=5`). **The brief MUST call out this two-layer transparency (and its match-scrutinee run-test)
as an acceptance gate** — it is the single subtlest correctness point in the landing and the proto
that shipped it to the packet did not test it.

<!-- checkpoint: enforcement + finding 5 done -->

---

## STEP 5 — THE OTHER TWO LANES (Core invariant #9)

### (a) ggdef lane
`ggdef` is the definitional interpreter for Gorget Core; it **shares the production
lexer+parser+AST** (`spec/ggdef/src/lib.rs:1-13`) and lowers production AST → GGC
(`ggc.rs`), then evaluates (`eval.rs`). Findings:
- GGC ALREADY has `Expr::Propagate(Box<Expr>)` (`ggc.rs:293`) and eval handles it
  (`eval.rs:165,377`). Today the ELABORATOR **auto-detects** bare throws calls and emits GGC
  `Propagate` itself (`elaborate/mod.rs`: `fn_throws`, `current_fn_throws`,
  `maybe_wrap_throws_call`, capture-context enum at 217-236).
- My production `Expr::Propagate` addition compiled cleanly in ggdef → its elaborator's
  production-Expr match is non-exhaustive (stop-and-report `ElabError` for unrecognized
  constructs, per lib.rs's "never a silent approximation"). So a `f()!` in a ggdef program TODAY
  would reclassify to elaboration-error (no arm yet).
- **"D29 within the ggdef subset" concretely requires:** (1) add a production-`Expr::Propagate`
  → GGC-`Propagate` elaboration arm (map the explicit mark; GGC Propagate + eval already exist);
  (2) post-D29, the elaborator REQUIRES the mark on a fallible call (reject bare = the ggdef
  analog of `E_MissingFallibleMark`) and RETIRES the auto-wrap. 
- **Which fixtures flip:** the ggdef fixture subset is `cow_*`/`deadwrite_*` (`classify.rs:64`),
  which is **throws-FREE** (grep of `cow_*.gg` for throws/`()!` = 0). So the ggdef FIXTURE corpus
  is UNAFFECTED. The only flips are ggdef's ~19 inline `throws` UNIT tests (`tests.rs`, the s103_*
  desugar tests) which gain `!` marks. Small, contained.

### (b) self-host lane
Full parser (`self_host_parser/parser.gg`) + typechecker (`self_host_typechecker/typecheck.gg`) +
lowerer. The self-host itself has **0 throws decls / 0 Result-return decls** (census) — but it
must still PARSE + TYPECHECK + LOWER the fixtures it compiles (via `*_comparison` tests), and
those fixtures gain `!` marks under migration. Port shape (do NOT implement; est.):
- **Lexer: already done.** `self_host_parser/lexer.gg:854,940` emit `TkBangEq` vs `TkBang`
  separately (maximal munch) — the `a()!=b` corner is FREE, same as production.
- **Parser:** prefix `TOK_BANG`→`EMove` at `parser.gg:2206`; the postfix/infix loop is
  `parse_expr_bp` (1634-1693). Port = add an `EPropagate` postfix arm in that loop (mirror the
  production `parse_postfix` Bang arm) + the `EPropagate` AST variant + bare `!:` signature
  handling near the throws-clause parse (`parser.gg:3256` "Skip optional 'throws Type'").
- **Typecheck:** it tracks `current_function_throws` / `throws_type` and rejects
  `main() throws NonInt` (`typecheck.gg:1949-1958`, `DkMainThrowsNonInt`); port = add mark
  tracking + reject bare fallible calls. Lowerer: add the transparent `EPropagate` handling with
  the SAME two-layer suppress-transparency (Finding 5).
- **Port pattern citation:** `scouts/scout-a2-s.md` — a semantic feature ports to the self-host
  by extending the EXISTING per-function walker (not a from-scratch pass), modeled on the
  analogous production pass, prototyped end-to-end + reverted. Same disjoint-copy caveat: the
  self-host driver dirs have parallel copies (`selfhost-frontend-archive` note: "fix primitives
  in ALL copies; lowerer↔typechecker parser/ast are symlinked").

### Cross-lane conformance fixtures the landing needs (Core #9)
- **NEG (per D23 position):** bare fallible call at binding / binary-operand / call-arg /
  return-tail / expr-body-tail / match-scrutinee / match-arm / bare-statement → `E_MissingFallibleMark`.
  (8 positions × {throws callee, Result-return callee} = the reject matrix.)
- **NEG:** marked call that cannot propagate + no disposition → `E_UnhandledThrows` (message
  flipped). NEG: bare `int f()!:` signature → A31 teaching-reject. NEG: `int f() ! E:` → parse error.
- **POS (per disposition):** `f()!` (propagate), `f()! catch (e): …`, `f()! rethrow (e): …`,
  `Result[T,E] r = f()!` (capture), `match f()!:` (scrutinee) — each build+RUN with correct stdout.
  MUST include the marked-match-scrutinee RUN test (Finding 5).
- **Hardening fixture (pinned, decisions.md:344):** a stdlib-shaped fallible API — a thin local
  `throws` wrapper (until D17 makes a real `read_file`/peer throws) exercising always-mark + ALL
  dispositions end-to-end. Plus a kind-2 fixture: a local non-throws `Result[T,E]`-returning
  producer, its call marked `!`, distinct from a `Result` combinator call which is NOT marked
  (Finding 2 predicate).

---

## STEP 6 — MIGRATION MECHANICS (measured this session)

The census instrument IS the migration oracle: each emitted span's END offset is exactly the byte
after the call's `)`. A ~20-line Python migrator (`/tmp/d29_migrate.py`) inserts `!` at each END
(reverse order to preserve offsets; bang-space `! ` when the next char is `=`). Verified on 3 real
throws fixtures — **41 marks inserted, every one build+RUN STDOUT-IDENTICAL to the original**:

| Fixture | marks | result |
|---|---|---|
| `test_error_handling.gg` | 14 | stdout identical, exit 0 |
| `snag48_throws_match_scrutinee.gg` | 12 (incl. `match f()!:`) | stdout identical, exit 0 |
| `throws_expr_body_tail.gg` | 15 (expr-body tails) | stdout identical, exit 0 |

Marks land correctly in EVERY D23 position: `match stringv_throws()!:`, `Tagged t = …()!`,
`show_str(…, case_string_inline()!)` (call arg), expr-body tails. **`gg fmt` round-trips the mark**
(the `Expr::Propagate` formatter arm renders `expr!`; 14/14 preserved). The only insertion corner
is bang-space before `=`/`==`/`!=` (handled by the migrator; the parser tests prove `f()! == b`
and `f()!= b` disposition). No other insertion corner found.

---

## STEP 7 — BLAST-RADIUS HONESTY (surprises, reported not buried)

1. **Instruments were pruned** (Step 0) — the census had to be rebuilt; the brief cannot assume
   `examples/d29_census.rs` exists (I recreated a cleaner parser-based one).
2. **Kind-2 is the dominant surface, not "very few"** (Findings 1, 4) — 206 in-repo Result-return
   decls (lib 146); the D17 "small blast radius" sequencing premise is invalidated. **Owner-level
   sequencing decision needed before the brief.**
3. **The combinator tension** (Finding 2) — the rule as worded sweeps in Result/Option
   combinators; the brief must pin the receiver-based predicate.
4. **The two-layer suppress-transparency** (Finding 5) — the subtlest correctness point; a
   check-only test misses the match-scrutinee SIGSEGV. The historical proto shipped WITHOUT it.
5. **Census double-counts imported lib bodies** (Step 2 caveat) — a clean per-corpus kind-2 count
   needs per-file span attribution; the brief should spec that instrument.
6. **The `!inferred` sentinel** in the `throws` field is a prototype shortcut that VIOLATES
   layering rule 2 (name-matched sentinel) — production must use a typed
   `ThrowsSpec::{No, Inferred, Explicit(Type)}` enum.
7. **Pre-existing bug still open** (packet §7): `v[i](...)` indexed-callable call misparses as a
   generic call — adjacent to the `v[i]()!` corner; independent of D29; keep the filed TODO.
8. **`E_UnhandledThrows` message currently says "throws \`…\`"** and my proto piggybacked the
   MISSING-MARK case on it — production needs the DISTINCT `E_MissingFallibleMark` code +
   `describe_resolved_type` must never be fed the sentinel; registry currently 96 `E_` codes
   (`spec/prose/diagnostic-codes.md`), gains one row.

---

## GATES SUMMARY (all FOREGROUND this session)
- `cargo build` + `cargo build --lib` — clean.
- `cargo test --lib` — **1119 / 0** (1107 baseline + 12 new D29 parser tests).
- `cargo test --lib parser` — clean (115 incl. 12 D29).
- Targeted `cargo test --test integration throws` — 38/1 → the 1 (`self_host_driver_rejects_main_throws_non_int`)
  is a load/timeout FLAKE: **PASSES in isolation** (150s, GG_BUILD_TIMEOUT_SECS=600). NOT a regression.
- End-to-end `gg check`/`gg build`/`gg run`: all dispositions (propagate/catch/rethrow/capture/
  match-scrutinee) RUN correctly; enforcement fires on bare (env-gated); migration parity proven.
- Main tree (`/workspace/gorget`) clean — no contamination.

## DELIVERABLES
- This report: `/tmp/d29_impl_scout.md`.
- Prototype patch (complete, incl. census example): `/tmp/d29_impl_proto.patch` (755 lines).
- Migrator: `/tmp/d29_migrate.py`.






