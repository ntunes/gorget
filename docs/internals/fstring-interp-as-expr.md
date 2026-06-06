# F-String Interpolation as Real Expr Nodes

> Design doc, 2026-04-21. Not yet implemented. Surfaced during the
> Phase 2c convenience-wrapper migration when
> `f"{v.iter().any(p)}"` link-failed against the un-mangled symbol.

## TL;DR

Today `StringSegment::Interpolation(String, Option<String>)` stores
the raw text between `{...}`. At IR-lowering time
(`src/ir/lowering/exprs/calls.rs:1236`),
`Parser::new(var_name).parse_expr()` re-parses the text and lowers
the freshly-built AST.

That re-parse bypasses every semantic pass: typecheck doesn't run on
it, the Pass 4.5 inferred-targs rewriter doesn't see it, the borrow
checker doesn't validate it, the resolver doesn't look at it. The
re-parsed AST is a parallel universe.

The fix: parse interp segments to real `Expr` nodes at parse time,
then let them flow through the normal pipeline.

## The Concrete Failure

`f"{v.iter().any(is_even)}"` where `is_even: bool(int)`:

1. Lexer produces `StringSegment::Interpolation("v.iter().any(is_even)", None)`.
2. Parser keeps it as raw text inside the `StringLiteral`.
3. Typecheck visits `StringLiteral` and only checks identifier-shaped
   interpolations for `Displayable` (typecheck.rs:925). The string
   `"v.iter().any(is_even)"` isn't a single identifier, so it's
   skipped.
4. Pass 4.5 walks `MethodCall` nodes in the regular AST. There ARE
   no `MethodCall` nodes inside the interp segment — just a string.
   So nothing to rewrite.
5. IR-lowering hits the `lower_interp_segment` path. Re-parses
   `"v.iter().any(is_even)"` → builds a fresh AST with a
   `MethodCall` inside. This AST has `generic_args: None` (parser
   default) and never went through inference.
6. IR-lowering emits `CallExtern("VectorIter__int64_t__any", ...)`
   — un-mangled. Link fails because the actual symbol is
   `VectorIter__int64_t__any__GorgetClosure` (only registered when
   the AST goes through the normal pipeline).

Workaround applied in `stdlib_iter_terminals.gg`: bind the call to
a local first, then interpolate the local.

```gorget
bool any_gt5 = v.iter().any(gt_five)   # normal AST → inference fires
print(f"any_gt5={any_gt5}")             # interp is just `any_gt5`
```

## Why This Matters Beyond Inference

The re-parse bypass isn't just an inference problem. Any semantic
analysis built on the AST silently misses interp segments:

- **Borrow checker**: a closure captured inside `f"{call(x)}"` could
  use-after-move `x` and the borrow checker wouldn't notice. The
  re-parsed AST has its own spans that aren't tied to the borrow
  graph.
- **Resolver**: identifier resolution inside interp goes through a
  separate path (typecheck.rs:927 `lookup_within_function`). Cross-
  scope references that the resolver would catch are silently
  skipped.
- **Privacy / visibility**: `f"{private_struct.field}"` may compile
  even when direct access wouldn't.
- **Meta evaluation**: `meta if`-substituted identifiers don't
  propagate into interp text.
- **Drop elaboration**: temporaries produced by interp expressions
  aren't tracked through the normal drop mechanism. Today the
  interp lowering inserts its own drop registration ad-hoc.

The inference link-failure is the most visible symptom but the
underlying defect is "interp segments aren't real AST."

## The Design

### Change `StringSegment::Interpolation`

```rust
// Before:
pub enum StringSegment {
    Literal(String),
    Interpolation(String, Option<String>),  // raw text
}

// After:
pub enum StringSegment {
    Literal(String),
    Interpolation {
        expr: Spanned<Expr>,      // parsed at parse time
        fmt_spec: Option<String>,
    },
}
```

Spans inside the parsed `Expr` use the same source offsets as the
rest of the file — they point at the original interpolated text in
the f-string body. The downstream tools (LSP, error reporting,
formatter) see them naturally.

### Lexer changes

`scan_string_literal` already extracts the raw text. Instead of
storing it, hand it to a sub-parser:

```rust
let expr_source = &self.source[expr_start..i];
let expr_span_offset = expr_start;
// Defer parsing — record the slice + offset; the parser turns it
// into Expr during the normal parse pass. Lexer stays one-pass.
segments.push(StringSegment::InterpolationSrc {
    text: expr_source.to_string(),
    span_offset: expr_span_offset,
    fmt_spec,
});
```

Lexer stays a single forward pass — it doesn't try to tokenize
inside the interpolation (which is recursive: `f"{f"{x}"}"` is a
real thing). Just records the slice + offset.

### Parser changes

A new parse path: when the parser sees a `StringLiteral` token
containing `InterpolationSrc` segments, it creates a sub-parser per
segment and parses the text. The sub-parser uses an **offset-aware
span builder** so spans in the produced AST point back into the
original source file (not into a synthetic per-segment buffer).

The sub-parser shares the same token enum + recursive-descent code
as the main parser; it just operates on a slice. Most of the
machinery already exists (`Parser::new(text).parse_expr()` at
calls.rs:1236) — lift it from IR-lowering to the parse pass.

### Semantic changes

Now that interp is real AST, the existing walkers descend into it
naturally. Specifically:

- **Resolver** (`src/semantic/resolve.rs`): add a `walk_expr` arm
  for `Expr::StringLiteral` that resolves each `Interpolation.expr`.
- **Typecheck** (`src/semantic/typecheck.rs:922`): replace the
  identifier-only `Displayable` check with a full `infer_expr` on
  each `Interpolation.expr`. Then check the inferred type against
  `Displayable` (or accept if it's a primitive). Inferred targs
  for any nested method calls land in the side-table naturally.
- **Pass 4.5 rewriter** (`apply_inferred_method_targs`): the
  `walk_expr` for `StringLiteral` recurses into interp segments.
  No special-casing needed — it walks `Expr` like any other.
- **Borrow checker**: same — the existing AST walkers see interp
  segments and check ownership / capture rules.

### IR-lowering changes

`lower_interp_segment` deletes its re-parse path. Instead it lowers
the pre-parsed `Expr` directly:

```rust
StringSegment::Interpolation { expr, fmt_spec } => {
    let val = lower_expr(ctx, builder, expr);
    let type_id = infer_operand_type_full(ctx, &val, builder);
    let tmp = builder.add_local(type_id, None);
    builder.assign(Place::local(tmp), val);
    let (spec, args) = format_for_printf(ctx, builder, type_id,
        FunctionBuilder::copy(tmp), fmt_spec.as_deref());
    format_str.push_str(&spec);
    printf_args.extend(args);
}
```

Cleaner: no fallback, no re-parse, no synthetic spans.

## Files Touched

| File | Change | Lines |
|---|---|---|
| `src/lexer/token.rs` | `StringSegment` enum: parsed Expr variant + transitional `InterpolationSrc` | ~20 |
| `src/lexer/mod.rs` | `scan_string_literal` produces `InterpolationSrc` instead of `Interpolation` | ~5 |
| `src/parser/expr.rs` | New `parse_interp_segments` lifts text → Expr, replaces `InterpolationSrc` with `Interpolation { expr, fmt_spec }` | ~80 |
| `src/parser/mod.rs` | Sub-parser entry that takes `(source: &str, offset: usize)` | ~30 |
| `src/semantic/resolve.rs` | StringLiteral arm walks interp expressions | ~20 |
| `src/semantic/typecheck.rs` | Replace identifier-only Displayable check with full `infer_expr` | ~30 |
| `src/semantic/typecheck.rs` (apply_inferred_method_targs) | Already walks recursively; just needs StringLiteral arm | ~10 |
| `src/semantic/safety/*` | Walk interp segments in borrow / drop / closure passes | ~50 |
| `src/ir/lowering/exprs/calls.rs:1194` (lower_interp_segment) | Delete re-parse path; lower pre-parsed Expr directly | ~30 (-50, +20) |
| `src/ir/lowering/exprs/mod.rs:2412` | Update StringSegment match arm | ~10 |
| `src/ir/lowering/closures.rs:543` | Update StringSegment match arm | ~10 |
| `src/ir/lowering/generics/mod.rs:680` | Update StringSegment match arm | ~10 |
| `src/formatter/mod.rs` | StringLiteral formatting walks Expr → text (or keeps source text in a side field) | ~40 |
| Lexer tests | Update `Interpolation` constructions to new shape | ~30 |

Net: ~400 lines new + restructured. Sequence as:

1. **Add `InterpolationSrc` transitional variant** alongside the
   existing `Interpolation(String, Option<String>)`. Lexer emits the
   new variant. Old code paths still see the old variant (the
   transition layer is at parse time). No behaviour change.
2. **Parser sub-parses `InterpolationSrc` → `Interpolation { expr,
   fmt_spec }`.** Old `Interpolation(String, Option<String>)` stays
   for back-compat during the migration. Parser converts in one pass.
3. **Migrate consumers one by one.** Each pass that reads
   `StringSegment::Interpolation` gets a new arm for the parsed
   variant. The text-based variant stays as a fallback. After every
   commit: full integration suite green.
4. **Delete the text-based variant** when no consumer reads it.

## What This Does NOT Solve

- **F-string formatting spec semantics.** `{x:.2f}` still gets the
  fmt_spec parsed as a string and applied at format time. Real
  parsing of fmt_spec into a typed FormatSpec is orthogonal.
- **Recursive interpolation** (`f"{f"{x}"}"`). Today the lexer
  handles nested f-strings via brace depth tracking; the parsed-Expr
  approach would need the sub-parser to recursively handle nested
  StringLiterals. Not new complexity, just the same complexity in a
  different place.
- **Performance.** Re-parsing per call site is wasteful but
  imperceptible. Parsing once at parse time is a wash — same work,
  earlier. Cache impact unknown but unlikely to matter.

## Risks

1. **Span correctness.** Sub-parser must produce spans in the
   original source coordinates. If `f"abc{x + y}def"` starts at
   offset 100, then `x + y` starts at offset 105 (after `f"abc{`).
   The sub-parser needs an offset arg threaded through every span
   construction. Most parser code uses `self.previous_span()` or
   similar — those need offset-aware variants OR the sub-parser
   stores the offset on `Parser` and applies it in one place.
2. **Error attribution.** Parse errors inside interp segments need
   to point at the right location in the source. The offset story
   above handles this if implemented correctly.
3. **Closure inside interp**: `f"{(int x): x*2}"` — does that
   produce a Closure expression as the interp value? Currently the
   re-parse just calls `parse_expr` which would. The new design
   needs to handle this; same code path, just earlier.
4. **Format-spec lookahead**: `{x:.2f}` — the lexer splits
   `expr_text` from `fmt_spec` at the `:`. The sub-parser receives
   only `x` and not `.2f`. Today's `split_interpolation_spec` runs
   in the lexer; it stays there.
5. **Backward compat during migration.** Step 1's transitional
   variant means consumers read either text-or-Expr until the
   migration completes. Each commit must keep both paths working.

## Why Not Just Fix the Inference Bypass?

Smaller fix: keep the raw-text storage, but during Pass 4.5, walk
`StringLiteral` segments, re-parse each interp text, run inference
on the parsed AST, then re-serialize back to text. Or pass the
inferred-targs side-table to IR-lowering's re-parse so it can apply
mutations.

Both are uglier. The re-parse-and-re-serialize is fragile (round-tripping AST through text loses span info, formatting). Threading the side-table to IR-lowering breaks the layering — we'd be doing semantic analysis at codegen time.

The structural fix (parse interp at parse time) eliminates the
class of bugs (not just the inference one), simplifies IR-lowering
(deletes the embedded sub-parser), and aligns with how every other
language handles it (Python's f-strings parse to AST, JS template
literals parse to AST, Rust's `format!` parses arguments at the
macro level).

## Sequencing After This Lands

1. Delete the inline-bind-to-local workaround in
   `tests/fixtures/stdlib_iter_terminals.gg` — once parsed-Expr
   interp ships, the original `f"{v.iter().any(p)}"` form works.
2. Audit other fixtures for similar workarounds; remove them.
3. Update the language reference docs to reflect that f-string
   interpolations are real expressions for all semantic purposes.
