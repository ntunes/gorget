# 4 — The parser & the AST

The parser turns the lexer's token stream into an **AST** (`Module` →
`Item` → `Stmt` / `Expr` / `Type` / `Pattern`). It lives in
`src/parser/`: the `Parser` struct and the statement/item/block
machinery in `mod.rs`, expression parsing (a Pratt loop) in `expr.rs`,
statements in `stmt.rs`, types in `types.rs`, patterns in `pattern.rs`,
the AST node definitions in `ast.rs`, and a generic AST walker in
`visitor.rs`. It is a hand-written recursive-descent parser with a
Pratt sub-loop for binary/postfix operators — no parser generator. The
token stream it consumes already carries synthetic `Indent` / `Dedent`
/ `Newline` tokens injected by the lexer (Chapter 3), so the parser
treats indentation as ordinary structural tokens rather than
re-deriving block structure.

> Verified against tree at branch `gorget-2` (commit `209813e8`). All
> `file:line` citations below were read from current source; re-verify
> fast-moving lines against the tree.

## What the parser is and where it lives

`Parser` (`src/parser/mod.rs:35`) holds the token stream as **two
parallel arrays** — `kinds: Vec<Token>` and `spans: Vec<Span>` —
indexed by a single cursor `pos: usize`. Splitting kind from span keeps
the hot `peek()`/`advance()` path touching a `Vec<Token>` rather than a
`Vec<Spanned<Token>>`. The split is done once in
`new_with_offset` (`mod.rs:64`), which also peels `Token::Comment` out
into a side-table (`comments`, `mod.rs:80-87`) so the formatter can
recover them later while the parser proper never sees them, and appends
a sentinel `Token::Eof` (`mod.rs:90-92`) so `peek()` past the end is
always safe.

Core cursor primitives are all small and infallible at the boundary:
`peek()` / `peek_span()` / `peek_ahead(n)` return `&Token` / `Span`
defaulting to `Eof` / `Span::dummy()` past the end (`mod.rs:117-128`);
`advance()` returns the consumed `Spanned<Token>` and steps `pos`
(`mod.rs:130-135`); `check`, `match_token`, `expect`,
`expect_keyword`, `expect_identifier` build on those. There is no
separate "current token" field — everything is `kinds[pos]`.

Callers construct a `Parser` over source text and call `parse_module()`
to get a `Module`. There are many entry points: the driver/loader
(`src/loader.rs:699`, `:812`, via `new_with_offset` so multi-file
programs get disjoint span ranges), the embedded stdlib
(`src/stdlib.rs`, many sites), and compile-time data
(`src/compiler_data.rs:26`). The same `Parser::new(text).parse_expr()`
entry is reused recursively for f-string sub-expressions (see
[F-string interpolation](#f-string-interpolation-the-stringliteral-sidecar)).

## The AST shape

The root is `Module { items: Vec<Spanned<Item>>, span }`
(`ast.rs:9`). Almost every node is wrapped in `Spanned<T>` (`{ node:
T, span: Span }`), so spans are uniform and pervasive — see Chapter 2
for `Span`/`Spanned`. `Module::all_items()` (`ast.rs:20`) gives a flat
view that recursively unwraps the `Item::Module` wrappers the loader
inserts when merging multi-file programs (`ast.rs:64-68`); most passes
want that flat view.

`Item` (`ast.rs:39`) is the top-level sum: `Function`, `Struct`,
`Enum`, `Trait`, `Equip`, `Import`, `TypeAlias`, `Newtype`,
`ConstDecl`, `StaticDecl`, `ExternBlock`, `Directive`, `Test`,
`Bench`, suite setup/teardown, a family of `Meta*` items
(`MetaConst`, `MetaType`, `MetaTypeFunc`, `MetaAssert`, `MetaLog`,
`MetaIf` — consumed by Chapter 6), and the loader-only `Module`
wrapper.

The three workhorse node families:

- **`Expr`** (`ast.rs:485`) — literals, `Identifier`, `Path`,
  `UnaryOp`, `BinaryOp`, `Call`, `MethodCall`, `FieldAccess`,
  `TupleFieldAccess`, `Closure` / `ImplicitClosure`, `Await`,
  `Rethrow` / `Catch`, `Block` / `Do`, `It`, and more. Calls and
  method calls carry an optional `generic_args: Option<Vec<Spanned<Type>>>`
  (`ast.rs:525`, `:533`) populated only when the source wrote explicit
  `[T]` turbofish; inferred type-args are filled in later by Pass 4.5,
  not by the parser.
- **`Stmt`** (`ast.rs:912`) — `VarDecl` (carrying `is_const`,
  `is_mutable`, `shared: SharedKind`, `type_`, a destructuring
  `pattern`, and `value`; `ast.rs:914-921`), `Expr`, `Assign`,
  `CompoundAssign`, `Return`, `Throw`, `OnError`, `Break`, `Continue`,
  `Pass`, `For`, `While`, and the control-flow forms.
- **`Pattern`** (`ast.rs:857`) — `Wildcard`, `Literal`, `Binding`,
  `Constructor { path, fields }`, `Tuple`, `Or`, `Rest`, and
  `DotShorthand` (the `.Variant(...)` form resolved against the
  scrutinee type in a later pass).

`Type` (`ast.rs:436` for the `Function` variant; the full enum is
nearby) models `Primitive`, `Named { .. }`, `Ref`, `Owned`, `Tuple`,
`Array`, `Slice`, `Inferred` (the `auto` placeholder), and `Function {
return_type, params, param_ownerships }`. Note the **return-type-first**
function-type shape, matching the surface syntax `int(int, int)`.

The AST does not carry resolved names, types, or DefIds — those are
side-tables produced by later passes keyed on spans. The parser's job
is purely syntactic shape plus spans.

## Recursive descent: items, statements, blocks

`parse_module()` (`mod.rs:464`) loops calling `parse_item()`, skipping
stray top-level newlines, until EOF or the error limit. `parse_item()`
(`mod.rs:498`) collects an optional doc-comment and `@`-attributes,
reads an optional `private` / `public` visibility prefix (public by
default, except `static`/module-vars which are private by default;
`mod.rs:520-526`, `:586-592`), then dispatches on the leading keyword
to the right `parse_*` routine. The fallthrough case
(`mod.rs:625-637`) is the interesting one: a top-level construct that
doesn't start with a keyword is *either* a function definition (starts
with a return type, `int add(...)`) *or* a module-level variable
(`int x = 5`). `looks_like_module_var_decl()` (`mod.rs:1497`)
disambiguates by lookahead.

### Indentation-based blocks

Because the lexer emits explicit `Indent` / `Dedent` / `Newline`
tokens, blocks are parsed structurally rather than by tracking columns.
The canonical shape is `COLON NEWLINE INDENT stmt* DEDENT`:

- `expect_block_start()` (`mod.rs:314`) consumes `Colon Newline
  Indent`.
- `parse_block()` (`mod.rs:374`) consumes the colon then calls
  `parse_block_body()` (`mod.rs:382`), which expects `Newline Indent`,
  loops `parse_stmt()` until `Dedent` (recovering on error,
  `mod.rs:387-395`), then consumes `Dedent`.

Two convenience forms exist for Python-style one-liners.
`parse_block_or_inline_stmt()` (`mod.rs:417`) handles `if x: stmt` on a
single line: after the colon, if the next token is *not* `Newline` it
parses a single statement and wraps it in a one-element `Block`.
`parse_arm_body()` (`mod.rs:449`) and `parse_body_or_expr()`
(`mod.rs:437`) do the analogous indented-block-or-inline-expr choice
for `match` arms and `rethrow`/`catch` bodies.

### Error recovery

The parser is error-tolerant: it collects errors into `errors`
(`mod.rs:39`) rather than bailing on the first one, capped at
`MAX_ERRORS = 10` (`mod.rs:32`, checked via `at_error_limit`,
`mod.rs:367`). On a statement error inside a block it pushes the error
and calls `synchronize_with_progress()` (`mod.rs:323`), which runs
`synchronize()` (skip to the next `Newline`/`Dedent`/statement-starting
keyword, `mod.rs:285`) and then *forces* one token of progress if
synchronize stalled — the explicit guard against infinite loops on
malformed input. Top-level item errors use
`synchronize_to_top_level()` (`mod.rs:333`), which skips a whole
malformed item body by tracking `Indent`/`Dedent` balance.

### Speculative parsing (`try_parse`)

Some constructs are genuinely ambiguous on a fixed lookahead and need
backtracking. `try_parse` (`mod.rs:1937`) saves `pos`, runs a closure
returning `Option<T>`, and restores `pos` on `None`. Because the whole
parser state-of-record is just the `pos` cursor (the token arrays are
immutable after construction), backtracking is a single integer
assignment — cheap. It's used for, e.g., the `mutable type ownership
name =` variable-declaration probe (`stmt.rs:655`) and the generic
method-call-vs-index disambiguation (`expr.rs:1095`, see below).

## Pratt expression parsing

Expressions use precedence-climbing (a Pratt parser) layered on top of
recursive descent. `parse_expr()` (`expr.rs:337`) calls
`parse_expr_bp(0)` (`expr.rs:347`): parse a prefix expression, then run
the infix/postfix loop with minimum binding power 0.

`parse_expr_bp_with_lhs()` (`expr.rs:353`) is the climbing loop. Each
iteration: (1) if the current token is a **postfix** operator with
binding power `>= min_bp`, parse it and continue; (2) else if it's an
**infix** operator whose *left* binding power `>= min_bp`, parse it and
continue; (3) else stop. Postfix is checked before infix.

- **Prefix** (`parse_prefix`, `expr.rs:382`): literals, identifiers,
  `self`, `it`, unary `not` / `-`, parenthesized/tuple/closure forms,
  and the divergent-expressions-in-expression-position trick where
  `throw e` / `return e` parse to a synthetic single-statement
  `Expr::Block` so they can appear inside `?? throw err()` and inline
  `if`/`match` arms (`expr.rs:428-455`).
- **Infix** binding powers live in `infix_bp()` (`expr.rs:684`),
  returning an `InfixBP { left, right, op }` (`expr.rs:260`). Higher =
  tighter; left-assoc ops use `(bp, bp+1)`, right-assoc `(bp+1, bp)`
  (`expr.rs:256-258`). The table runs from `rethrow`/`catch` at
  `left:1` (`expr.rs:695-708`) and `??` at `left:3` (`:709`), up
  through `or` (5), `and` (7), comparisons (11–13), bitwise (17–22),
  shifts (25), to arithmetic at the top.
- **Power `**`** binds at `left:34, right:33` (`expr.rs`) — tighter
  than unary prefix (`-`/`*`/`!` at bp 33) so `2 ** -1` parses as
  `2 ** (-1)`. Right-associative via the `(left, left-1)` idiom —
  `2 ** 3 ** 2` groups as `2 ** (3 ** 2)` (Fortran/Python/JS/Ruby
  convention). The parser also rejects `-x ** 2` at parse time
  (`ParseErrorKind::AmbiguousUnaryMinusPow`, the JS/TC39 guardrail):
  after consuming `-`, if the token right after is NOT `(` and the
  operand ends up as a top-level `Pow`, the reject fires. See D28
  amendment R1/R2 in `docs/define-gorget/decisions.md:1197`.
- **Postfix** binding powers are in `postfix_bp()` (`expr.rs:1038`):
  `.` / `?.` / `[` / `(` all at 35 (the tightest), range `..` /
  `..=` at 23. `parse_postfix()` (`expr.rs:1054`) builds
  `FieldAccess`, `TupleFieldAccess` (`.0`, `.1`), `MethodCall`,
  `Await` (from postfix `.await()`), index, and call nodes.

One sharp ambiguity handled here: `expr.field[...]` could be a generic
method call `expr.method[T](args)` or a field access followed by
indexing. The parser uses `try_parse` to speculatively read `[T]` as
generic args, and only commits if a `(` follows; otherwise it
backtracks and returns a plain `FieldAccess` (`expr.rs:1090-1115`).

### Implicit `it`-closures

`it` parses to `Expr::It` (`expr.rs:423-426`). At the **outermost**
call-argument level only, `parse_call_arg` (the per-argument helper,
`expr.rs:1889`) checks whether an argument expression `contains_it`
and, if so, auto-wraps it in `Expr::ImplicitClosure { body }`
(`expr.rs:1917-1922`). The
"outermost" restriction is enforced by a `call_arg_depth` counter
(`mod.rs:43`) bumped by an RAII guard during nested call-arg parsing
(`expr.rs:8-24`), so `and_then(Some(it + 1))` wraps once, not twice.

## Bare-tuple syntax

Gorget allows comma-separated tuples without surrounding parens in
several positions, and the parser materializes them as
`Expr::TupleLiteral` / `Pattern::Tuple` at each site rather than via a
single shared "comma expression" rule:

- **Parenthesized expr vs tuple** (`expr.rs:1277-1306`): after `(`,
  parse one expression; if a `,` follows it's a `TupleLiteral`,
  otherwise it's just a parenthesized expression. `()` is the empty
  tuple (`expr.rs:1279-1285`).
- **Return** (`stmt.rs:116-128`): `return a, b` collects a bare tuple
  into `Expr::TupleLiteral`.
- **`auto` destructuring** (`parse_auto_var_decl`, `stmt.rs:607`):
  `auto a, b = f()` parses comma-separated binding patterns into a
  `Pattern::Tuple` (`stmt.rs:613-623`), which then drives multi-binding
  in lowering.
- **For-loop patterns** and **assert-message lists** likewise read
  comma tails into tuples.

Because there is no single comma-precedence operator, each construct
opts into bare-tuple parsing explicitly — the comma is structural at
these sites, not an infix operator in `infix_bp()`.

## Function types are return-type-first

`parse_type()` (`types.rs:10`) parses a base type then a postfix.
The function-type postfix (`types.rs:194-225`) fires when a
`Primitive` or `Named` base is immediately followed by `(`
(gated by `is_function_type_context`, `types.rs:233-239`), producing
`Type::Function { return_type, params, param_ownerships }` — the return
type is the *base* read first, so the surface form `int(int, int)`
means "function from `(int, int)` to `int`." Each parameter slot can
carry an ownership sigil (`&` → `MutableBorrow`, `!` → `Move`,
`types.rs:202-208`). This is the same return-type-first convention the
language reference specifies for function types; see
`docs/language-reference.md`.

The Rust parser stores the primitive base as `Primitive(PrimitiveType)`
— an enum, not a `String` (`ast.rs:413`) — so the function-type return
type is fully resolved by the time the postfix fires; there is no
string payload to alias. (See the self-host section below for a
function-type defect that the *self-host* parser has but the Rust
parser structurally cannot.)

## F-string interpolation: the `StringLiteral` sidecar

This is the one place the AST stores a *parsed sub-expression tree*
alongside the lexer literal, and it crosses every frontend pass —
forward-referenced in Chapter 7 (resolution) and Chapter 9 (type
checking), and consumed at IR-lowering (Chapter 12).

### The problem it solves

The lexer represents an `f"..."` string as a `StringLiteral { kind:
Format, segments }` where each interpolation is
`StringSegment::Interpolation(text, fmt_spec)` carrying the **raw
text** between `{...}` (`src/lexer/token.rs:754-760`). Historically,
IR-lowering re-parsed that raw text at codegen time
(`Parser::new(text).parse_expr()`) and lowered the fresh AST. That
re-parsed AST was a parallel universe: it never went through name
resolution, type checking, the Pass 4.5 inferred-targs rewriter, or the
borrow checker. The visible symptom was `f"{v.iter().any(p)}"`
link-failing against an un-mangled monomorphized symbol because the
method-mangling rewriter never saw the call — but the underlying defect
was structural: *interpolation segments weren't real AST*, so any
AST-based semantic analysis silently skipped them.

### The shipped design: a parsed-expr sidecar

The fix that shipped (note: **not** the enum-replacement an earlier design
proposed — the lexer
`StringSegment::Interpolation(String, Option<String>)` shape is
unchanged) is a **sidecar** on the AST node. `Expr::StringLiteral`
carries a second field:

```rust
// src/parser/ast.rs:497
StringLiteral(crate::lexer::token::StringLiteral, Vec<Spanned<Expr>>),
```

The first field is the lexer literal (text + raw segments); the second
is **one parsed `Expr` per `Interpolation` segment, in order**
(`ast.rs:490-497`). It is populated only for `Format`-kind strings, and
empty for normal strings and for synthetic literals constructed during
lowering.

`parse_format_string_interp_exprs()` (`expr.rs:302`) builds the
sidecar. When `parse_prefix` hits a `StringLiteral` token
(`expr.rs:399-409`) it calls this helper, which — for each
`Interpolation(text, _)` segment — spins up a sub-parser
`Parser::new_with_offset(text, base).parse_expr()` (`expr.rs:316`) and
pushes the result. On a sub-parse error it falls back to a literal
fragment carrying the original text (`expr.rs:318-328`) so the
IR-lowering backstop can still re-parse it.

### Synthetic span offsets and determinism

Spans inside the parsed segments need keys that are **unique across the
module** (typecheck indexes `inferred_method_targs` by
`method.span.start`) yet **deterministic across parallel test runs**.
The lexer doesn't record per-segment source offsets, so a real source
span isn't available. The parser instead assigns each segment a
synthetic base from a per-`Parser` counter `next_interp_offset`
(`mod.rs:56`), starting at `1 << 40` (well above any plausible
source-file size) plus a per-module `base_offset << 20` shift
(`mod.rs:64-72`), bumped by `1 << 20` per segment (`expr.rs:314-315`).
The counter is **per-instance, not a process-global atomic**, on
purpose: a global atomic would produce non-deterministic span *values*
across concurrent fixture parses, breaking the exact-match
`*_comparison` tests. The per-module shift is load-bearing too —
without it, every module's first interp segment shares span `1 << 40`,
the resolver's `resolution_map[span_start]` collides last-write-wins,
and lowering emits the wrong mangled symbol (`mod.rs:65-72`). The
trade-off: diagnostics inside f-string segments point at synthetic
offsets rather than real source positions (`expr.rs:299-301`).

### Downstream consumers (forward references)

Because the sidecar exprs are real AST, the existing pass walkers
descend into them naturally:

- **Resolver** (Chapter 7): `Expr::StringLiteral(_, interp_exprs)`
  resolves each segment (`src/semantic/resolve.rs:1333`), with errors
  *suppressed* there because meta-for loop variables inside `f"{...}"`
  only materialize at monomorphization time.
- **Type checker** (Chapter 9): infers each interp expr's type and
  checks `Displayable` (`src/semantic/typecheck.rs:1029` and the walker
  arms at `:5709`, `:5871`, `:6044`).
- **IR-lowering** (Chapter 12): `lower_string_interpolation` uses the
  pre-parsed sidecar when present, indexing it positionally against the
  raw segments (`src/ir/lowering/exprs/calls.rs:1617-1634`). The
  re-parse path **still exists as a backstop** in `lower_interp_segment`
  (`calls.rs:1728`, backstop at `:1832`) for synthesized f-strings and
  parse-failed segments — so, contrary to the old design doc's "delete
  the re-parse path," the re-parse remains, just demoted to a fallback.

### Caveat: the generic AST visitor still reads raw text

`walk_expr` in `src/parser/visitor.rs:60-73` ignores the sidecar
(`StringLiteral(s, _)`) and instead synthesizes a *fake* `Identifier`
expr from each raw segment's text with `Span::dummy()`. This is a
deliberate lossy shortcut for the generic walker; passes that need the
real sub-expressions (resolver, typecheck, lowering) read the sidecar
directly rather than through `walk_expr`. If you add a pass over the
visitor, be aware it will not see real interp sub-expressions.

## In the self-host

The Gorget self-host frontend (`tests/fixtures/self_host_parser/`,
`self_host_resolver/`, etc.) reimplements the lexer + parser in
Gorget. Its parser is a structurally similar recursive-descent parser
(`parser.gg`) producing an AST defined in `ast.gg`.

The biggest **divergence** is exactly the f-string sidecar. The
self-host AST models a string literal as `EStringLiteral(String, bool)`
(`tests/fixtures/self_host_parser/ast.gg:54`) — a plain text payload
plus a *boolean* "has interpolation" flag (`token_has_interpolation`,
`parser.gg:575`). It does **not** pre-parse interpolation segments into
sub-expression nodes. Consequently the self-host resolver doesn't emit
the per-segment `RES` entries (with span keys `>= 2^40`) that the Rust
resolver produces for f-string interp sub-expressions, which is a known
contributor to the `resolver_comparison` gap. Closing it would require
adding the segment-pre-parsing path plus the synthetic-offset span
machinery across the self-host parser, AST, and resolver copies.

> **Self-host-only function-type defect.** The self-host parser has a
> long-standing `String`-aliasing bug in the function-type parse path:
> the outer return type's primitive name gets corrupted to the first N
> bytes of the identifier following the closing `)` — e.g.
> `int(int) make_adder()` mis-renders the return type. The root cause
> is the self-host AST modelling a primitive type as `TPrimitive(String)`
> (`tests/fixtures/self_host_parser/ast.gg:27`); the move of that
> `String` into the `TFunction` enum variant in `parser.gg`'s
> function-type branch aliases the backing buffer. The **Rust parser
> does not and structurally cannot have this defect** — it stores
> `Primitive(PrimitiveType)` (an enum, `src/parser/ast.rs:413`), and
> `cargo run -- parse` on `int(int) make_adder()` yields a correct
> `return_type: Function { return_type: Primitive(Int) }`. The bug
> reproduces across all self-host parser copies and surfaces as
> `*_comparison` mismatches on `generic_callable*` / function-type
> fixtures; treat those as this issue, not a Rust-side regression.
> Re-verify it still reproduces before acting on it.

To check current parity, run the relevant comparison test and read the
printed matched-count (these tests are **diagnostic-always-pass** —
they assert nothing, so a green `cargo test` says nothing about
parity):

```bash
cargo test --test integration parser_comparison -- --nocapture
cargo test --test integration resolver_comparison -- --nocapture
```

The driver lives at `tests/integration.rs:12406` (`parser_comparison`)
and `:12683` (`resolver_comparison`); each builds its own self-host
driver dir, parses every `tests/fixtures/*.gg`, and diffs the
self-host parser's output against the Rust parser's, reporting matched
/ mismatched / crashed counts. Note that the parser/ast/lexer copies
differ per driver dir (some are symlinked, some independent) — a
primitive change must be applied in every relevant directory. See
Chapter 26 (the self-host frontend) and Chapter 27 (comparison &
bootstrap) for the full picture.
