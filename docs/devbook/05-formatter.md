# 5. The formatter (`gg fmt`)

The formatter is the source-canonicalizer: it parses a `.gg` file and re-prints it
as canonical Gorget source from two inputs — the AST, and the original source text
at the AST's spans. It lives entirely in
[`src/formatter/`](../../src/formatter/) — two files: [`mod.rs`](../../src/formatter/mod.rs)
(the AST walker / emitter) and [`doc.rs`](../../src/formatter/doc.rs) (a small
Wadler-Lindig pretty-printer used for the few constructs that need line-width-aware
wrapping). It depends only on the lexer's token types and the parser's AST
(`src/formatter/mod.rs:5-6`); it runs *before* semantic analysis and is the only
consumer of the AST that intentionally produces text rather than more IR.

The second input is what keeps the AST honest. A whole family of authorial choices
is *syntax*, not semantics — an integer's radix and digit grouping, which escape
spelled a character, which quote style wrapped a string, whether a primitive was
written `byte` or `uint8` — so the AST is right to drop them, and the formatter is
the one consumer that needs them back. It recovers them from the source text at the
node's span, the same way it recovers comments, behind a re-lex check that makes a
stale span harmless (see [String literals and the verbatim
chokepoint](#string-literals-and-the-verbatim-chokepoint)).

What neither input preserves is genuinely lost: original whitespace, line breaks
that don't matter, redundant parentheses, the order of an import list. Everything
else round-trips, and the formatter goes out of its way to preserve a handful of
facts that *look* cosmetic but are semantically load-bearing on re-parse
(visibility on statics, the `:`-vs-`= "sym"` shape of a function body, the
author's suite layout). The goal is **idempotence**: `fmt(fmt(x)) == fmt(x)`,
which the unit tests assert directly (`src/formatter/mod.rs:5318`, `:5486`, and
many more).

## The `gg fmt` command

`gg fmt <file>` is dispatched in the driver at `src/main.rs:3540`. It supports three
modes, all keyed off one entry point:

- default — print the formatted source to stdout (`src/main.rs:3568`);
- `--in-place` / `-i` — overwrite the file in place (`src/main.rs:3562-3566`);
- `--check` / `-c` — exit non-zero if the file is not already formatted, printing
  `"<file>: not formatted"` to stderr via `eprintln!` (`src/main.rs:3557-3561`). The
  check is a raw `formatted != source` string comparison — there is no structural diff.

The public API is one function, and its return type is the interesting part:

```rust
// src/formatter/mod.rs:4904
pub fn format_source_result(source: &str) -> Result<String, Vec<ParseError>> {
    let mut parser = crate::parser::Parser::new(source);
    let module = parser.parse_module();
    if !parser.errors.is_empty() {
        return Err(parser.errors);
    }
    let comments = parser.comments;
    Ok(Formatter::new(comments, Rc::from(source)).format(&module))
}
```

The formatter builds its output from the AST, so a statement the parser could not
build would simply be **absent** from the result — silent data loss on the user's own
file. Refusing is the only safe answer: on any parse error the driver renders the
diagnostics and exits non-zero *without* writing to disk or printing a partial
format. Call sites that are contractually fed valid Gorget (unit tests, fixtures) use
`format_source_infallible` (`src/formatter/mod.rs:4919`), which panics rather than
returning a truncated file.

What `gg fmt` still does not do is run semantic analysis: type errors, ownership
errors and name-resolution errors are `gg check`'s job, and a file that parses but
does not check formats normally.

## Two-layer architecture: the `Emitter` and the `Doc` IR

The formatter is a *hybrid* printer. Most of the work is done by a direct,
imperative AST walk that writes into an indentation-aware buffer; a minority of
constructs that need "fit-on-one-line-or-break" decisions are lowered into a small
document algebra and rendered separately, then spliced back into the buffer.

### The `Emitter` (the imperative layer)

`Emitter` (`src/formatter/mod.rs:13`) is a string buffer that tracks the current
indentation level, the current column, and whether the cursor is at line start. Its
contract is simple:

- `write(s)` lazily emits the pending indentation (four spaces per level,
  `src/formatter/mod.rs:39-47`) the first time text is written on a fresh line, then
  appends `s`.
- `indent()` / `dedent()` bump the level; `newline()` ends the current line and
  is **idempotent at line start**; `blank_line()` emits *at most* one blank line
  and never doubles up.
- `current_col()` (`src/formatter/mod.rs:61`) answers *which column the next character
  will occupy* — which is not the same as the `col` field while indentation is still
  pending. Anything making a width decision asks through it.

`newline()`'s idempotence is what keeps blank lines out of nested emissions. An
expression-position suite ends by newline-terminating its last statement, and
the enclosing statement then terminates its own line — `int r = match x:` with
an indented `else:` has both happen in sequence. Neither side is wrong; each is
honouring "I end my own line". Making the *producer* treat "end a line that has
already ended" as a no-op settles it once, for every combination of host and
suite, instead of asking each host to know what its child did. `blank_line()`
stays the one way to ask for a blank, so a deliberate one is never lost; a
`tests/lints.rs` guard keeps raw newline writing inside `Emitter` so the
property stays total.

Indentation is always four spaces, hardcoded (`src/formatter/mod.rs:41`, mirrored by
`doc::INDENT_WIDTH = 4` in `src/formatter/doc.rs:13`). There is no configuration:
the formatter is opinionated and has no options struct.

The `Formatter` struct (`src/formatter/mod.rs:408`) wraps an `Emitter` plus the
comment side-table. The walk is a conventional recursive descent:
`format_module → format_item → format_function / format_struct / … →
format_stmt → format_expr / format_type / format_pattern`. Each handler writes
tokens and manages its own indentation.

### The `Doc` IR (the wrapping layer)

For constructs where a long line should wrap onto multiple indented lines —
call-argument lists, generic-parameter lists, method chains, binary-operator chains,
comprehensions, grouped imports — the formatter builds a `Doc` tree and renders it.

`Doc` (`src/formatter/doc.rs:21`) is a textbook Wadler-Lindig document algebra, plus
one node — `Fill` — for the layout Gorget actually wants:

| node | flat mode | broken mode |
|------|-----------|-------------|
| `Text(s)` | literal text; *may* contain `\n` when it is a pre-rendered sub-element | same |
| `Line` | a single space | newline + indent |
| `SoftLine` | nothing | newline + indent |
| `HardLine` | — | always newline; *forces* the enclosing group to break |
| `Indent(d)` | +1 level for `d` | same |
| `Concat([..])` | render in order | same |
| `Group(d)` | try flat; break if it doesn't fit | — |
| `IfBreak{flat,broken}` | emit `flat` | emit `broken` |
| `Fill{items,close}` | `item, item, item` + `close` | pack greedily; break to `indent+1` and resume |

The renderer (`src/formatter/doc.rs:284`) walks a `Doc` against a `max_width`. For
each `Group` it calls `measure_flat` (`src/formatter/doc.rs:437`) — which returns the
single-line width, or `None` when the subtree cannot be flattened at all — and renders
flat iff `current_col + width <= max_width`, else broken
(`src/formatter/doc.rs:325-336`). The maximum line width is `MAX_WIDTH = 120`
(`src/formatter/doc.rs:10`).

Two things make `measure_flat` return `None`: a `HardLine`, and a `Text` that already
contains newlines. The second is not a corner case — it is how a *pre-rendered
sub-element that had to wrap* arrives (see `element_to_string` below), and reporting a
width for it would let an enclosing group choose "flat" for something that is already
several lines tall. `None` propagates through every combinator, `Fill` included: one
unflattenable item makes the whole list unflattenable.

Widths are counted in **characters, not bytes**, throughout — the budget is a display
property, and a byte count inflates every decision downstream of a non-ASCII literal.

### Fill: the canonical broken-list layout

`surround_fill` (`src/formatter/doc.rs:232`) is the workhorse for every list the
formatter lays out — parameter lists, call arguments, generic parameters and
arguments, closure parameters, array / tuple / dict literals, grouped imports. It
emits `open item1, item2 close` when the list fits, and when it does not, it **packs
greedily**: as many items per line as the budget allows, continuation lines at the
block indent one level in, and the closing delimiter following the last item inline.

```text
void draw_text_atlas(GpuContext &ctx, FontAtlas font, String text, float x, float y, float char_size, float r, float g,
    float b, float a):
```

Three properties fall out of the packing loop (`render_fill`,
`src/formatter/doc.rs:349`) and are worth stating because each is load-bearing:

- **The fit test for the last item includes the closing delimiter.** Otherwise the
  final line overruns by exactly `close.len()`, which is why `Fill` owns its `close`
  instead of leaving it as a sibling `Text`.
- **At least one item lands on every line.** An item too wide to fit even alone at the
  continuation indent is emitted there anyway and overflows. (The other budget
  escape is caller-emitted text after the close — an extern's `= "symbol"`, a
  signature's `:` — which the packer cannot see; filed as a known gap.)
- **A multi-line item never shares a line.** It measures `None`, so the packer always
  breaks before it, which places it at precisely the column its sub-render assumed;
  packing then resumes from the column its last line ended at.

There is **no trailing comma** in a fill-broken list: the close is inline after the
last item, so there is nothing for a trailing comma to precede.

`surround` (`src/formatter/doc.rs:184`) is the other shape — one item per indented
line, *with* a trailing comma, built from `Group`/`Line`/`IfBreak`. `IfBreak` is what
makes that comma conditional: `IfBreak { flat: "", broken: "," }`, so a flat list has
no trailing comma and a broken one does (`src/formatter/doc.rs:189-190`, exercised at
`doc.rs:580` and `:602`). No production call site uses it — a comment-bearing list
reaches the same exploded shape imperatively, through
`format_bracketed_broken_with_comments`, because comments cannot survive the
pre-render that fill packing depends on. The `formatter_list_emit_fill_census` lint
counts both spellings so that opting a list out of the canon stays a visible decision.

### Splicing the two layers together

The bridge is `write_doc` (`src/formatter/mod.rs:550`). It renders the `Doc` with
`doc::render_at` — passing the emitter's *current column and indent level as the
starting state* so the wrapping decision accounts for text already on the line — then
writes the pre-rendered (newline-bearing) string back via `Emitter::write_preformatted`
(`src/formatter/mod.rs:73`), which prepends the base indentation and recomputes the
column from the last newline of the spliced text.

"Current column" is asked for through `Emitter::current_col`
(`src/formatter/mod.rs:61`), never off the raw `col` field. Indentation is written
*lazily*, on the first `write` of a line, so at line start `col` is 0 while the text
about to be emitted will land at `indent * 4`. A statement that *begins* with a list
would otherwise be measured that many columns too narrow and silently overrun the
budget.

A subtlety of the hybrid design: the imperative layer frequently needs a *string*
for a sub-expression to drop into a `Doc::Text`. It gets one via `element_to_string`
(`src/formatter/mod.rs:506`), which spins up a throwaway `Formatter` (with no
comments), runs a closure against it, and returns its buffer. This is how
`format_method_chain` (`src/formatter/mod.rs:2016`) and `format_binary_chain`
(`src/formatter/mod.rs:2057`) turn each chain segment / operand into a `Doc::Text`
leaf before grouping them — the wrapping is decided over the *segments*, while each
segment is formatted by an ordinary recursive call.

The throwaway formatter is seeded with both the indent level *and* the column its
output will be spliced at (`element_to_string_at`, `src/formatter/mod.rs:515`). It has
to be: a sub-render that measured from column 0 would believe it had a whole
indentation's worth of extra budget, and emit lines that overflow by exactly that
much once spliced. Seeding makes the assumption self-consistent — an element that
decides to break becomes multi-line, and a multi-line element is always placed at
precisely the column it assumed.

## How canonical output is produced

The formatter does more than echo the AST — it normalizes. The normalizations are
the interesting part of the implementation; each is a deliberate canonical-form
choice.

### What the formatter does *not* own: suite layout

Canonical output stops where the author made a real choice. Gorget accepts a
suite on the header's own line (`if c: stmt`) or indented beneath it, and the
formatter keeps whichever was written — symmetrically, so it neither explodes a
one-liner nor collapses a short indented suite.

That choice cannot be recovered downstream: the parser folds both spellings into
the same one-statement `Block`, and comparing spans cannot help, because the same
`Block` shape is also *synthesized* — for `throw x` / `return x` in expression
position, for a normalized closure body — at positions where no suite was written
at all. So the fact travels as typed metadata: **`Block.layout: SuiteLayout`**
(`src/parser/ast.rs`), written at the parser's suite-construction sites and read
by the emitters that have two spellings to choose between. Constructions outside
the parser have no author to preserve and go through `Block::synthetic`.

`Expr::Do` carries the same kind of bit for the same reason. It has two
producers — the `do` keyword, and `parse_body_or_expr` synthesizing the variant
for an indented `catch` / `rethrow` body — and the formatter must neither delete
the author's keyword nor invent one where the parser supplied it.
`author_spelled` separates them. It is not cosmetic: a `do:` wrap makes its tail
a read position, so inserting or removing one can flip a program between
accepted and rejected.

Reading the *shape* instead of the layout is the trap this replaces, and it
fails in both directions — a synthetic `Block { Throw }` is not an indented
suite, and a one-statement indented suite is not a one-liner.

Two positions accept only one spelling, and the emitters there deliberately
carry no layout read: a statement-position `match` rejects `else: stmt`, and
`on error`'s inline form is colon-less (`on error stmt`), so the inline emitter
takes its header spelling as a parameter rather than hardcoding `":"`.

Clause headers (`elif:`, `else:`) need one more thing. They are not statements,
so the per-statement blank-line preservation in `format_block_stmts` cannot see
them, and each clause site checks for an author blank above itself. The order is
load-bearing — blank, then leading comments, then the header — or a
`blank` / comment / `else:` run comes out comment-then-blank and the comment
detaches from the clause it documents.

### Blank-line and trailing-newline normalization

After the walk, `format` runs a single-pass collapse over the whole buffer: any run
of 3+ consecutive newlines is squeezed to 2 (i.e. at most one blank line)
(`src/formatter/mod.rs:461-476`), and a trailing newline is guaranteed
(`src/formatter/mod.rs:477-480`). Blank lines *between* top-level items and between
trait/equip members are inserted explicitly during the walk via `blank_line`
(`src/formatter/mod.rs:1136`, `:1672`, `:1735`). The trailing-comment aligner runs
before this collapse (`src/formatter/mod.rs:457`) and is unaffected by it — the
collapse only removes newlines, never touches a within-line gap.

### Import sorting

`format_module` (`src/formatter/mod.rs:1040`) partitions items into leading
directives, imports, and "the rest" — the partition stops at the first non-import,
non-directive item (`past_imports`, `src/formatter/mod.rs:1045-1052`), so only the
*leading* import block is reordered. Within that block imports are sorted with
std/`xtd` libraries first, then alphabetically (`src/formatter/mod.rs:1060-1071`;
`is_std_import` at `src/formatter/mod.rs:4948`). Names *inside* an import are also
sorted: `import a.{X, Y}` groups are sorted alphabetically and laid out with
`surround_fill`, so a long group packs across continuation lines
(`src/formatter/mod.rs:1761`), and `from a import …` name lists are sorted too — but
**not** wrapped, because in indentation-based syntax a bare name on a fresh line would
re-parse as a new statement (`src/formatter/mod.rs:1787-1794`).

### Type-first re-printing and bare-tuple positions

Gorget is type-first (`int x = 5`), and the formatter prints declarations that way:
`format_param` emits `type [&|!]name` (`src/formatter/mod.rs:1020-1024`), `VarDecl`
emits `type name = expr` (`src/formatter/mod.rs:1084-1104`). Ownership sigils (`&`,
`!`) print *immediately before the name*, via `format_ownership_prefix`
(`src/formatter/mod.rs:2085`), matching the language rule that the sigil binds the
binding, not the type.

Several positions canonicalize a tuple to its **bare** (parens-free) spelling because
that is the idiomatic form the parser accepts: function return types
(`src/formatter/mod.rs:491-497`), `return a, b` (`src/formatter/mod.rs:1132-1138`),
`auto a, b = …` destructuring (`src/formatter/mod.rs:1089-1099`), and `for x, y in …`
patterns (`src/formatter/mod.rs:1175-1181`).

### Visibility: the load-bearing "cosmetic" cases

`public Foo` and a bare `Foo` both parse to `Visibility::Public`, so the value alone
cannot say which the author wrote — and both directions of guessing are wrong.
Emitting a keyword only for `Private` deletes every explicit `public` in the tree;
emitting one unconditionally rewrites every declaration that relies on the default.
The parser therefore records which spelling it consumed, on `explicit_visibility`,
and `format_visibility` (`src/formatter/mod.rs:1385`) reads that fact and nothing
else: **the keyword is emitted iff one was written.** Nine declaration kinds carry
the flag, and `formatter_visibility_emit_site_count` in `tests/lints.rs` cross-checks
the emit sites against the carriers so a tenth kind cannot quietly skip the path.

Statics invert the default and keep their own rule (`format_static_decl`,
`src/formatter/mod.rs:1842-1858`). They are private-by-default, so `public` is
emitted whenever the value *is* public — for a parsed static that means the author
wrote it, and for a synthesised one it stops the emission from silently flipping the
declaration to private and breaking cross-module imports — while `private` is emitted
only when explicitly written, since inventing the default everywhere would rewrite
the tree.

### String literals and the verbatim chokepoint

A string literal is emitted **verbatim first**: `format_string_lit`
(`src/formatter/mod.rs:4304`) asks for the author's own lexeme and, when it can have
it, writes exactly that. Quote style, the prefix letter, which escape spelled a
character, the f-string brace form and — the case that makes a long `"""` block
readable — its physical line layout all survive, because nothing was regenerated.

Recovery goes through one helper, `Formatter::verbatim`
(`src/formatter/mod.rs:3367`), and it is the same helper behind every other form the
AST drops: an integer's radix and digit grouping, a float's trailing zeros, `b'A'`
versus `65`, `byte` versus `uint8`, and the quoted **name-strings** the AST stores
decoded (test and bench names, snapshot names, attribute string arguments, extern ABI
tags). Routing them through one place is what makes the property below a property of
the class rather than a habit of each arm; `formatter_verbatim_emit_arm_count` in
`tests/lints.rs` keeps a new arm from spelling its own quotes.

**The property: a recovered lexeme is re-lexed and compared before it is trusted.**
`verbatim` slices the source at the node's span, hands the slice to
`relex_single_token` (`src/formatter/mod.rs:4378`) — which asks the *real lexer* and
returns a token only when the slice lexes cleanly into exactly one value-bearing
token covering the whole slice — and then checks that token against the value the
caller is about to emit. Asking the lexer rather than mirroring its rules is the
point: "this lexeme denotes this value" is decided by the same code that produced the
value from that lexeme. So a stale, synthetic, or merely mis-computed span cannot
produce output meaning something other than the node it came from. It can only fail
the comparison and fall back. That is the difference between preservation and a
silent rewrite of the user's source.

**The fallback path reconstructs.** When recovery fails — a synthesised literal, an
f-string interpolation's sub-expression parsed at a synthetic base offset, a span that
no longer denotes this node — `format_string_lit` rebuilds the literal from its
`StringKind` prefix (`r"`, `b"`, `c"`, `f"`, `"""`, `"`) and its segment list,
re-emitting interpolation segments as `{expr_text[:spec]}` from the stored source text
rather than re-formatting the embedded expression. Bodies are escaped by
`canonical_string_escape` (`src/formatter/mod.rs:4414`): raw strings pass through,
`{`/`}` double inside f-strings, every control character is escaped — C0 and DEL as
`\xHH`, C1 (`0x80-0x9F`) as `\u{XX}`, because the lexer rejects `\x` above `0x7F` and a
raw C1 byte would plant an invisible control character in the user's source. Printable
non-ASCII stays raw.

No `.gg` source can reach that path, which is exactly why the escape policy lives in a
free function with unit cells of its own (`src/formatter/mod.rs:5171`) instead of a
fixture that would be green for the wrong reason.

### Function body shapes

`format_function` (`src/formatter/mod.rs:481`) preserves the four distinct body
shapes the AST distinguishes, each of which round-trips to a different construct:
block body (`:` + indented stmts), expression body (`: expr` on one line), a bare
declaration (signature + newline, for trait method signatures), and an extern body
(`= "symbol"`) (`src/formatter/mod.rs:511-533`).

## `meta` constructs

Compile-time `meta` forms are kept verbatim — the formatter does **not** evaluate or
expand them (that is Pass 0's job; see chapter 6). They re-print as written:

- **Item-level**: `meta const`, `meta type` (plain / conditional / call RHS variants
  at `src/formatter/mod.rs:298-315`), `meta type … (params)` functions, `meta assert`,
  `meta if`/`elif`/`else` over *items* (`src/formatter/mod.rs:342-375`), and
  `meta log` — all in `format_item` (`src/formatter/mod.rs:285-383`).
- **Statement-level**: `meta if` (`:1370`), `meta for` (`:1386`), `meta match`
  (`:1398`), `meta while` (`:1422`), `meta const` (`:1431`), `meta log` (`:1438`) in
  `format_stmt`.
- **Expression-level**: `meta`-prefixed operators — `a meta[op] b` for infix
  (`src/formatter/mod.rs:2052-2055`) and `meta <op>` token form
  (`src/formatter/mod.rs:2057-2060`).

The AST is deliberately structured so that `meta if`/`meta for` carry their bodies as
*real* statements/items (so resolution and the rest of the pipeline can see inside
them), and the formatter walks those bodies normally with `format_block_stmts` /
`format_item` — it does not collapse a `meta` block to a placeholder. (Contrast this
with the self-host AST-canonicalizer, below, which *does* collapse them.)

### `meta for` inside `match`

A `match` arm list can contain a `meta for` that templates arms. The formatter handles
the `MatchItem::MetaFor` variant inline inside the `Stmt::Match` walk
(`src/formatter/mod.rs:1258-1269`): it prints `meta for vars in range:` and then the
single `arm_template` indented beneath it, rather than treating it as a regular arm.

## Match arms and guards

`format_match_arm` (`src/formatter/mod.rs:3059`) prints `case <pattern>`, then — **if
the arm has a guard** — ` if <guard>`. The arm body is laid out two ways
according to the author's `Block.layout`: an indented `Block` becomes a newline
plus indented statements, and everything else is printed inline after the colon.

The layout read matters here specifically because a `Block` body is *not* the
same thing as an indented arm: `case 1: throw "bad"` also arrives as a `Block`,
since the parser wraps a `throw`/`return` expression-prefix, and an emitter that
branched on the AST shape put those on their own line.

An inline arm also flips the order of the header's trailing-comment hook. Firing
it at the colon would put the comment ahead of the statement it heads
(`case 1:  # one` followed by the body), which does not re-parse. The comment is
*claimed* at the header — which is what keeps a leading-comment hook inside the
body from taking it first — and emitted after the body. Suppressing it instead is
not available at this site: an inline arm body is an expression, so no
statement-side hook exists to claim it, and it would drift down to lead the next
arm.

> **Note on a stale claim.** Older project memory states that "the formatter
> suppresses match guards for canonical output." That is **not** true of the Rust
> `gg fmt` here — `MatchArm.guard` is a real AST field
> (`src/parser/ast.rs:967`) and the production formatter emits it verbatim
> (`src/formatter/mod.rs:3062-3064`). The guard-suppression behavior belongs to the
> *self-host AST-debug canonicalizer* (`tests/fixtures/self_host_*/format.gg`), a
> different program with a different purpose — see "In the self-host" below.

## Comment handling

Comments are not part of the AST. The parser drops them into a side-table —
`parser.comments`, a `Vec<Spanned<String>>` collected in lexer-token order
(`src/parser/mod.rs:57-58`), which therefore comes pre-sorted by source position. The
`Formatter` holds that vector plus a forward-only `comment_cursor`
(`src/formatter/mod.rs:408-411`).

Interleaving is span-driven and one-directional: before emitting an item, field,
variant, statement, or branch body, the walker calls `emit_comments_before(pos)`
(`src/formatter/mod.rs:586`), which flushes every comment whose span starts before
`pos`, each on its own line, advancing the cursor. After the whole module is walked,
`emit_remaining_comments` (`src/formatter/mod.rs:609`) flushes whatever's left (e.g.
trailing comments after the last item).

A leading flush would move every end-of-line comment onto its own line, so a comment
that *trails* its node is claimed before the cursor can reach it.
`emit_trailing_comment_after(prev_end)` (`src/formatter/mod.rs:864`) asks one
question — is this comment's start on the same source line as the previous emit's
last character? — and if so injects it after the emitted line, ahead of the newline
that statement already wrote. A comment separated by a line break fails that test and
stays a leading comment at the next sibling's indent, which is the right answer for a
standalone comment *between* two nodes. Headers get the same treatment one level up:
a comment after `if c:` is claimed at the header and emitted after the body, because
emitting it at the colon would put it ahead of the statement it heads and the result
would not re-parse.

Two constants finish the shape. A trailing comment is injected exactly
`TRAILING_COMMENT_GAP` spaces after the code (`src/formatter/mod.rs:220`), and a
*run* of them is then aligned to a shared column — the smallest multiple of
`ALIGN_STRIDE` that clears the widest line in the run, with a comment that would
overrun `MAX_WIDTH` excluded as an outlier rather than dragging the group right.
That alignment is a pure post-pass over the finished buffer (`plan_trailing_aligns`,
`src/formatter/mod.rs:233`, plans the gap rewrites; the mutating half applies them
last-to-first so earlier offsets stay valid), which is why the same one constant can
be both the writer's gap and the aligner's minimum: the pass rewrites exactly the run
of spaces the writer laid down.

## In the self-host

**There is no self-host `gg fmt`.** The Gorget self-host frontend
(`tests/fixtures/self_host_*`) does not reimplement the source formatter — there is
no Gorget-language reimplementation of `src/formatter/` and no `fmt` subcommand in the
self-host drivers.

The `format.gg` files that exist in the self-host directories
(`tests/fixtures/self_host_parser/format.gg` and its siblings) are a *different
artifact*: an AST-debug **canonicalizer** that emits a normalized textual
representation of the parsed AST so the `*_comparison` tests can diff the self-host
parser/resolver/typechecker output against the Rust pipeline's. Its header says so
directly — "Produces canonical string representations of AST nodes for testing"
(`tests/fixtures/self_host_parser/format.gg:1-2`). It is a debug-dump, not a
source-pretty-printer: it does not preserve comments, does not wrap to a line width,
and intentionally *drops* information (e.g. match guards) to match Rust's canonical
dump. Do not confuse it with the production formatter — they share neither code nor
goals.

Consequently, the formatter has no comparison-test parity story to report. Its
regression net is the Rust unit tests in `src/formatter/mod.rs` (the `#[test]`
block from `:2400`, heavy on idempotence assertions) and `src/formatter/doc.rs`
(the pretty-printer algebra tests from `:311`).
