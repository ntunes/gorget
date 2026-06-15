# Brief — self-host comprehension-filter AST lossiness (EDictComp filter + ESetComp sentinel)

**Track:** BUG / fidelity cleanup ("self-host = elegance showcase"). **Scout:** a2873cf4 (RUN-verified).
**Parity:** 0 flip today (no dict-comp-with-filter fixture). Set-comps-with-filter already work via the sentinel and ARE in the runtime snapshot set → the swap must stay **byte-neutral on runtime output**. This is a layering-discipline / elegance debt retirement, not a parity chain.

## The two anti-patterns (current file:line — TODO's cited lines drifted)
1. **`EDictComp` drops the filter** — `self_host_parser/parser.gg:2316-2319` parses `if cond` via `self.parse_expr()` then DISCARDS it. AST node `EDictComp(Box, Box, String, Box)` has NO filter field (`ast.gg:84`). Format never renders one (`format.gg:404-407`). Rust supports it: `DictComprehension.condition: Option<Box<Spanned<Expr>>>` (`src/parser/ast.rs:651-657`), honored at `src/ir/lowering/exprs/collections.rs:799,843-847`.
2. **`ESetComp` uses an `EIntLiteral(0)` SENTINEL** for "no filter" — `self_host_parser/parser.gg:2301`, detected via magic-number `match` at `format.gg:396-401`. 4th AST field is a raw `Box[SpannedExpr]`, not `Option` (`ast.gg:83`).

## Reference idiom to mirror (already correct in-tree)
`EListComp(Box[SpannedExpr], String, Box[SpannedExpr], Option[Box[SpannedExpr]])` (`ast.gg:82`) — 4th field is `Option[Box[SpannedExpr]]`. Copy its end-to-end wiring verbatim for both set & dict:
- **parse** (`parser.gg:2272-2277` EListComp template): `Option[Box[SpannedExpr]] cond = None; if self.check_kw(KW_IF): ... cond = Some(Box(cond_expr))`
- **resolve** (`resolve.gg:717-718`): `if opt_cond is Some(b): resolve_expr(*b, ...)`
- **format** (typechecker `format.gg:284-285`; parser `format.gg:387-388` via `has_opt_boxed_expr`/`get_opt_boxed_expr`)
- **cow** (`lower_cow.gg:261-265`): `match opt_filt: case Some(filt_box): cow_scan_expr(...*filt_box...)`
- **lower** (`lower_expr.gg:2735-2736`): pass the `Option` straight into the comprehension lowering

## Complete blast radius (SYMLINK-AWARE — edit independent copies in LOCKSTEP)
Independent copies (the symlink-followers `self_host_check` + `self_host_lowerer` inherit parser.gg/ast.gg/resolve.gg from `self_host_typechecker` — do NOT edit those, they follow):
- **ast.gg (3):** `self_host_parser`, `self_host_resolver`, `self_host_typechecker`
- **parser.gg (3):** same three
- **format.gg (3):** same three (`self_host_lexer/format.gg` has no comprehension code — skip)
- **resolve.gg (2):** `self_host_resolver`, `self_host_typechecker`
- **lowerer-only (independent):** `self_host_lowerer/lower_cow.gg`, `self_host_lowerer/lower_expr.gg`, and `self_host_lowerer/lower.gg` (the name-debug stringifier at `:496-501` — ARITY wildcard only)

### Fix A — ESetComp sentinel → Option (Box→Option is SAME arity, so `lower.gg:498` wildcard `ESetComp(_,_,_,_)` needs NO change):
| File | Change |
|---|---|
| `{parser,resolver,typechecker}/ast.gg:83` | `ESetComp(..., Option[Box[SpannedExpr]])` |
| `{parser,resolver,typechecker}/parser.gg` (ESetComp ~2301/~2277/~2424) | replace sentinel build with `None` / `Some(Box(...))` |
| `{parser,resolver,typechecker}/format.gg` (~391-402/~388-400/~288-300) | replace `match...EIntLiteral` with `if cond is Some(b)` |
| `{resolver,typechecker}/resolve.gg` (~802-808/~721-727) | guard `resolve_expr(*cond)` with `is Some(b)` |
| `self_host_lowerer/lower_cow.gg:266-269` | `match sfilt_box: case Some(b): cow_scan_expr(...)` |
| `self_host_lowerer/lower_expr.gg:2737-2745` + **DELETE `setcomp_filter_opt` `:2961-2975`** (the sentinel-converter fossil + comment block) | pass the `Option` straight through |

### Fix B — EDictComp add filter field (arity CHANGES → `lower.gg:500` wildcard `EDictComp(_,_,_,_)` → `EDictComp(_,_,_,_,_)`):
| File | Change |
|---|---|
| `{parser,resolver,typechecker}/ast.gg:84` | `EDictComp(Box, Box, String, Box, Option[Box[SpannedExpr]])` |
| `{parser,resolver,typechecker}/parser.gg` (EDictComp ~2316/~2292/~2439) | capture `if cond` into `Option`, pass as 5th arg (stop discarding) |
| `{parser,resolver,typechecker}/format.gg` (~404-407/~401-403/~301-304) | append ` if ...` when `Some` |
| `{resolver,typechecker}/resolve.gg` | resolve the filter when `Some` |
| `self_host_lowerer/lower_cow.gg:270-273` | scan the filter when `Some` |
| `self_host_lowerer/lower_expr.gg:2746-2751` + ~`2987-3010` (mirror the set/list filter wrap at `comp_synth_body` ~`2950-2958`) + **retire the "AST has NO filter field" comment fossils at `:2747-2750`, `:2980-2986`, `:3025-3032`** | thread `Option` into `lower_dict_comprehension`, add the `if Some` filter branch in the synth body |
| `self_host_lowerer/lower.gg:500` | wildcard arity `+1` |

## Executor notes (from brief-review pass 1, SIGN OFF — folded)
- **N0 — the lower side is ALREADY Option-wired.** `lower_set_comprehension` (`lower_expr.gg:2896`) and `lower_list_comprehension` (`:2780`) already take `Option[Box[SpannedExpr]] filt_opt`, and `comp_synth_set_body` (`:2943`) already has the full `Some`/`None` filter-wrap. So Fix A's lower side = DELETE the `setcomp_filter_opt(*sc_cond_box)` conversion (`lower_expr.gg:2744` caller + `:2968` def) and pass the AST's `Option` straight through. (Fix B's dict synth body still needs the `if Some` wrap added — mirror `comp_synth_set_body`.)
- **N1 — per-dir format idiom differs; do NOT cross-pollinate.** parser/resolver `format.gg` use helpers `has_opt_boxed_expr(cond)` / `get_opt_boxed_expr(cond)` (defined `format.gg:66`/`:73` in those two dirs) + string `+` concat. The typechecker `format.gg` uses native `if cond is Some(b)` + f-strings and does NOT define those helpers. Mirror each dir's OWN `EListComp` template (parser `:387-388` vs typechecker `:284-285`).
- **N2 — cow set-comp scan becomes conditional.** `lower_cow.gg:269` currently UNCONDITIONALLY does `cow_scan_expr(... *sfilt_box ...)` on the sentinel Box. After Fix A, `sfilt_box` is an `Option` → must become `match Some(b)`. Byte-neutral (scanning the old `EIntLiteral(0)` sentinel was a cow no-op).
- **N3 — fossil-comment line refs drifted.** The `setcomp_filter_opt` comment (`:2961-2967`) cites `parser.gg:2419`; dict-comp comments (`:2747-2750`, `:2980-2986`, `:3025-3032`) cite `parser.gg:2434-2437`. Actual current lines are `:2301`/`:2306` (set sentinel) and `:2316-2319`/`:2321` (dict discard). Confirm each fossil resolves before deleting it with the code it describes.

## Sequencing (avoid a broken intermediate)
Do parser + ast + the `setcomp_filter_opt` deletion ATOMICALLY (the converter is only safe to delete once the parser stops emitting the sentinel). Edit ALL independent copies before building — a single missed copy or stale wildcard arm is a hard compile error across the driver dirs.

## Gate battery
- `cargo build` + `cargo test --lib`.
- ALL 5 self-host drivers must still BUILD: `parser_comparison`, `resolver_comparison`, `type_comparison`, `lowerer_comparison`, `c_emit_comparison` (arity mismatch in any copy = hard fail; matched-counts must not regress).
- `self_host_runtime` + the `comprehensions` / `test_comprehensions` runtime fixtures — must stay BYTE-EXACT (the set-comp-with-filter output `set2 len: 4` is load-bearing).
- `self_host_bootstrap_fixed_point` (`GG_BUILD_TIMEOUT_SECS=600`) — the critical gate. First GREP the driver/self-host sources for any set/dict comprehension; if the self-host's OWN code uses one, the AST-shape change ripples through self-compilation and fixed_point re-convergence is the proof.

## Risk
Low but MUST be gated, not assumed. An AST arity change is a breaking ABI-style change across all copies; a missed copy = compile failure or fixed-point divergence. Mitigations: edit all independent copies in lockstep; the `EListComp` template is a proven-correct per-line reference; do parser+ast+converter-deletion atomically. No other consumers (typecheck/validate/meta/infer reference neither node — confirmed by scout).

## Discipline
Worktree off gorget-1 (`git merge --ff-only gorget-1` first). Stage ONLY the specific self-host files listed above. No `git add -a`.
