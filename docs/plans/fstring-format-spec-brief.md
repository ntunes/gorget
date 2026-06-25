# EXECUTOR BRIEF — Self-host f-string FORMAT-SPEC threading (+2 parity)

## North-star
The self-host **silently drops** the f-string format spec (`{n:08d}`, `{pi:.2f}`, `{x:b}`, `{255:#x}`) — a documented language feature (`docs/language-reference.md` §14.1, lines 2692-2734). This round threads the spec through the self-host lexer → AST → parser → lowering, mirroring Rust, so two corpus fixtures flip to MATCH and the feature is reference-grade.

**Verified flip set (honest, +2):** `tests/fixtures/fstring_format.gg`, `tests/fixtures/fstring_binary_spec_leak.gg`. Neither has a runtime snapshot today.
**Do NOT bundle** `fstring_unicode_passthrough.gg` — it has NO `:`-spec; it fails for a SEPARATE pre-existing template-truncation bug (emitted `__slit` truncated at `.len`). File that separately; do not touch it here.

## Worktree discipline (MANDATORY — do FIRST)
Run `pwd` and `git rev-parse --show-toplevel`; confirm BOTH point inside YOUR worktree (NOT `/workspace/gorget-1`). If `pwd` reports `/workspace/gorget-1`, STOP and report. Then `git merge --ff-only gorget-1` (your worktree branches from main and lags without this). Do NOT `cd` into `/workspace/gorget-1`, do NOT use `/workspace/gorget-1/...` paths, do NOT touch/advance/reset `main`. Stage ONLY the exact files you change (named below) — NEVER `git add -a`/`.`/`commit -a`. Another parallel track is editing `tests/fixtures/self_host_lowerer/lir_codegen.gg` — DO NOT TOUCH that file; you have no reason to.

## Ground in docs + the authoritative Rust source FIRST
- `docs/language-reference.md:2692-2734` (§14.1 Format Specifiers): syntax `[#][0][width][.precision][type]`; int types `d x X o b`, float types `f e E`. Examples: `{n:08d}`→`00000042`, `{255:#x}`→`0xff`, `{255:b}`→`11111111`, `{3.14159:.2f}`→`3.14`. `:223-224` grammar `interpolation = "{" expression [":" format_spec] "}"`.
- Authoritative Rust reference (mirror these EXACTLY for semantics):
  - `src/lexer/token.rs:754-759` — `StringSegment::Interpolation(String, Option<String>)` (expr, spec).
  - `src/lexer/mod.rs:766` + `:997-1033` — `split_interpolation_spec` (depth-0 colon split, honors paren/bracket/brace depth + quote-skip).
  - `src/ir/lowering/exprs/calls.rs:2096-2254` — `apply_format_spec` (the specifier-selection logic to port: width/zero-pad/alt/precision → printf specifier, plus the binary case).

## The gap (CONFIRMED by scout, re-verify each file:line — they may have drifted)
| Layer | File:line | Drop |
|---|---|---|
| AST (lexer enum) | `self_host_typechecker/lexer.gg:42-44` | `enum StringSegment: SegInterpolation(String)` — no spec slot |
| Lexer scan | `self_host_typechecker/lexer.gg:639-640` | pushes raw `expr_text` (spec still attached, never split) |
| AST node | `self_host_typechecker/ast.gg:98` | `EFString(String, Vector[SpannedExpr])` — no spec field |
| Parser | `self_host_typechecker/parser.gg:2367-2384` | `parse_expr()` stops at `TOK_COLON` (`parser.gg:276`) → `:spec` silently discarded, template gets bare `{}` |
| Lowering | `self_host_lowerer/lower_expr.gg:386-527` | byte-walks template for `{}` (0x7B 0x7D), emits `%lld`/`%.*s`/`%f` by ARG TYPE only; spec never reaches here |

**Splitter — exists in ONLY ONE lexer; you must PORT it (review pass 2).** `split_interpolation_expr()` lives ONLY in `self_host_lexer/lexer.gg:1066-1105` — the CORRECT depth-0 colon split (mirrors Rust), but it strips+DISCARDS the spec (returns expr_only, ~:643-644). It is **ABSENT** from `self_host_typechecker/lexer.gg`, `self_host_parser/lexer.gg`, `self_host_resolver/lexer.gg` (they each push raw `SegInterpolation(expr_text)` with the spec still attached — `:640`/`:632`/`:636` respectively). So you must **COPY** the depth-0 splitter into those 3 real lexer copies, each returning BOTH the expr AND the spec. **The self-host has NO tuple returns** (`grep` confirms no `(T,U)` returns) — so do NOT try to return a pair: keep `split_interpolation_expr(String) -> String` (the expr) and add a sibling `split_interpolation_spec_part(String) -> Option[String]` (the depth-0 spec, or `None[String]()` if no depth-0 colon), and call BOTH at each scan site. The depth-0 requirement is MANDATORY (a naive last-colon split regresses closure-colon fixtures like `(int x): x>3` — see canary below).

## Design — Option A (typed field, layering-correct; mirror Rust). Implement THIS, not the string-encode shortcut.
**(a) AST enum** — `StringSegment::SegInterpolation(String)` → `SegInterpolation(String, Option[String])` (expr_text, spec). Mirror Rust `token.rs:759`.
**(b) Lexer scan** — at each scan site (`self_host_typechecker/lexer.gg:640`, `self_host_parser/lexer.gg:632`, `self_host_resolver/lexer.gg:636`, `self_host_lexer/lexer.gg:643`) call BOTH the ported `split_interpolation_expr` (expr) and the new `split_interpolation_spec_part` (spec) and `push(SegInterpolation(expr_only, spec_opt))`.
**(c) Parser** — `case SegInterpolation(expr_text, spec):` — parse `expr_only` (now spec-free → strictly cleaner), carry `spec` alongside.
**(d) EFString** — in the copies that HAVE an EFString node (`self_host_typechecker/ast.gg:98` and `self_host_resolver/ast.gg:101`): add a parallel `Vector[Option[String]]` aligned by index with the exprs: `EFString(String, Vector[SpannedExpr], Vector[Option[String]])`. Populate the REAL specs at BOTH populate sites — typechecker `parser.gg:2367`/`:2384` AND resolver `parser.gg:2108`/`:2119` (do NOT stub an empty vector at the resolver site). ⚠ **`self_host_parser` has NO EFString node** — it renders f-strings as `EStringLiteral(token_string_val(...), token_has_interpolation(...))` at `parser.gg:2136`, so (d) does NOT apply there (it only needs (a)(b)(c)).
**(e) Lowering apply** — `self_host_lowerer/lower_expr.gg:386-503`: at the per-placeholder arm, read `specs.get(fi)`. If `Some(spec)`, port Rust `apply_format_spec` to choose the printf specifier (emit `%{flags}{width}{.prec}llx/llX/llo/lld/e/E/f` into `fmt`) or, for `b`, call `gorget_int_to_binary(val, alt)` (clone the existing bool arm at `lower_expr.gg:449-461`). Fall through to the existing type-based default (UNCHANGED) in BOTH of these cases: (i) spec is `None` (no spec string present), AND (ii) the ported `apply_format_spec` itself returns `None` (a spec present but inapplicable to the arg's type — Rust `calls.rs:2096` returns `Option` and yields `None` there). Do NOT hard-apply a bogus specifier when `apply_format_spec` declines. Note honestly in a comment: narrow-int widening (`widen_int`) and unsigned `%llu` are completeness items NOT required for the 2-fixture flip (corpus candidates are I64/F64 only) — mirror them from Rust if cheap, else leave a `// TODO` citing this brief.

**No new runtime symbols.** `gorget_string_format` (`runtime_string.c:289`) + `gorget_int_to_binary` (`runtime_string.c:506`, already in the String-return registry `lir_codegen.gg:7767` — re-grep to confirm) suffice. The C emitter already decomposes String args to `.len,.data` for `%.*s` and passes int/float raw — emitting a richer specifier into `fmt` needs ZERO codegen change.

## Multi-copy propagation (ALL must move in lockstep or drivers fail to BUILD)
`SegInterpolation`/`EFString` are matched/constructed in ~48 sites across copies (`grep -rn "SegInterpolation\|EFString" tests/fixtures/self_host_*`). Bump arity at EVERY arm.
- `self_host_typechecker` (lexer.gg/ast.gg/parser.gg are REAL) — change (a)(b)(c)(d). `self_host_lowerer` + `self_host_check` SYMLINK these → inherit a/b/c/d; `self_host_lowerer/lower_expr.gg` gets (e) (its own file, NOT symlinked). `self_host_check` needs NO direct edits (its lexer/ast/parser are symlinks; its `loader.gg:24` only IMPORTS the EFString name, arity-agnostic).
- `self_host_parser` (real copies) — (a)(b)(c) ONLY. It has NO EFString node (renders f-strings as `EStringLiteral` at `parser.gg:2136`), so (d) does NOT apply — just the 2-field `SegInterpolation` + ported splitter to stay buildable.
- `self_host_resolver` (real copies) — (a)(b)(c)(d); its OWN EFString decl is `ast.gg:101` and its OWN populate site is `parser.gg:2108`/`:2119` (populate real specs, do not stub empty).
- `self_host_lexer` (real) — this is the SOURCE copy of the depth-0 splitter (`lexer.gg:1066`). Apply (a)(b): 2-field `SegInterpolation` + the new spec-returning sibling helper. (Copy FROM here into the other 3 real lexers.)
- Confirm with `md5sum`/`ls -l` which lexer.gg/ast.gg/parser.gg are symlinks vs real before editing, so you don't double-edit a symlink.

### COMPLETE arm inventory (review pass 1 — these break the build OR silently drop the spec if missed; re-grep to confirm current lines)
The `grep -rn "SegInterpolation\|EFString" tests/fixtures/self_host_*` above is authoritative — but DO NOT rely only on the build break to find arms. Beyond lexer/ast/parser/lower_expr, the arity bump ALSO touches these semantic-pass + lowering-helper arms (build FAILS without them):
- `self_host_typechecker`: `infer.gg:126`, `typecheck.gg:783`, `resolve.gg:887`, `meta.gg:492/498, 760/766, 1389/1395`.
- `self_host_lowerer`: `loader.gg:248`, `lower_closures.gg:555/699/908/1218/1482`, `lower_cow.gg:221`, `lower_generics.gg:413/920/924/1793`.
- `self_host_resolver`: `resolve.gg:669`.
- `self_host_lexer`: `format.gg:216`, `main.gg:215` (SegInterpolation arms).
- `self_host_{typechecker,parser,resolver}`: `parser.gg:550` (`token_string_val`) + `parser.gg:566` (`token_has_interpolation`) `SegInterpolation` arms (×3 real parser copies) — build-guarded arity bumps. ⚠ `token_string_val:550` reconstructs the template as `{s}` and DROPS the spec — harmless for the lowering pipeline (typechecker/lowerer route interpolated strings via `EFString`, not this arm) but it DOES affect `parser_comparison`/`resolver_comparison` rendering (self_host_parser's `:2136` routes interpolated strings through `token_string_val`). Bump arity; leave the `{s}` rendering as-is unless you want comparison-fidelity (out of scope for the +2 flip).

### ⚠ MANDATORY — thread the spec through every EFString RECONSTRUCTION (silent-drop risk, gates are BLIND to it)
Several sites DESTRUCTURE then RE-CONSTRUCT an `EFString` (not just match it): `meta.gg:492-498, 760-766, 1389-1395` and `lower_generics.gg:920-924`. With a 3-field `EFString`, you MUST carry the spec vector through unchanged:
`case EFString(raw, parts, specs): … EFString(raw, np, specs)` — do NOT synthesize an empty `Vector[Option[String]]()`. If you drop it, arity still matches → the build PASSES → f-strings inside meta-substituted / generic-substituted code silently lose their spec, and NO gate catches it (neither flip fixture exercises meta/generic substitution of an f-string). Grep every `EFString(` CONSTRUCTION site and confirm each either (i) preserves the original segment's spec list or (ii) is a genuinely new f-string built with the correct specs.

## Gates (run; report each)
1. Build all 6 self-host drivers (the arity refactor is build-guarded — a missed arm fails to compile loudly).
2. The 2 candidates flip: build each through the self-host pipeline (driver → `--emit-c` → `cc` → run) and diff stdout vs the **FULL** `gg run` oracle output (each fixture prints MORE lines than the spec examples — `fstring_format` ~14 lines incl. hex/oct/bin-alt/padded/sci/neg-hex/zero cases; `fstring_binary_spec_leak` includes a `zero=0` line from `{z:b}`). Match the COMPLETE oracle, do not hand-write a partial expected. Confirm determinism (run twice).
   **Snapshot mechanism (review pass 1 — regen can SILENTLY DROP a regression):** `self_host_runtime` AUTO-DISCOVERS every `tests/fixtures/runtime_snapshots/*.out` — there is NO explicit passing-list in `tests/integration.rs` (do NOT stage that file for snapshots). DO NOT blind-run `GG_REGEN_RUNTIME_SNAPSHOT=1` (it wipes + reseeds to the current stable-match set, so any UNRELATED regressed fixture's `.out` just disappears with no failure). Instead: (i) run gate 5 (`self_host_runtime`) against the CURRENT committed snapshots FIRST and confirm `regressed : 0` (catches any regression your change caused); (ii) then HAND-ADD exactly the two new `.out` files (`fstring_format.out`, `fstring_binary_spec_leak.out`) by capturing the verified oracle output (single trailing `\n`, matching existing snapshot format); (iii) re-run gate 5 and confirm the passing set GREW by exactly 2 with `regressed : 0`. If you prefer regen, diff the snapshot dir before/after and ASSERT +2 with zero drops.
3. **CANARY — `vector_any_all_bool` must STAY MATCH** (closure-colon `(): 0` / `(int x): x>3` at paren-depth>0 must NOT be split). It already has a snapshot — confirm `self_host_runtime` keeps it green. This proves the depth-0 splitter is correct.
4. `cargo test --lib` + `cargo test --test lints`.
5. `cargo test --test integration --release self_host_runtime` (snapshot net, with the 2 added + canary).
6. `GG_BUILD_TIMEOUT_SECS=600 cargo test --test integration --release self_host_bootstrap_fixed_point` — must RECONVERGE (arity-refactor sanity; the feature path is unexercised by self-emission, so any break = a refactor typo).
Do NOT run the full `cargo test --test integration` — that's the parent's job at integrate time.

## Commit (in YOUR worktree)
Stage ONLY the self-host files you changed (across all copies) + the 2 new `.out` snapshots. Do NOT stage `tests/integration.rs` (the snapshot set is auto-discovered; no list to edit). Write the message to a temp file via heredoc and `git commit -F <file>` — NEVER inline `-m` with backticks/`$()` (they execute in zsh). Message shape:
```
feat(self-host): thread f-string format-spec through lexer/AST/parser/lowering (+2 parity)

SegInterpolation/EFString now carry a typed Option<spec> (mirror Rust
token.rs StringSegment::Interpolation + apply_format_spec). Lowering
applies width/zero-pad/alt/precision printf specifiers + the binary arm
(gorget_int_to_binary); None falls through to the existing default.
Depth-0 colon split (ported into all real lexers from self_host_lexer) preserves closure-colon
fixtures (vector_any_all_bool canary). Flips fstring_format +
fstring_binary_spec_leak; locked into the self_host_runtime snapshot net.
```

## Report back
The per-copy diff summary, the 2 before/after run outputs, the canary result, each gate result, the commit hash. Do NOT integrate to gorget-1 — the parent fresh-output-reviews then integrates. Flag any arm you were unsure about.
