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

There is a third input, smaller than either: the parser's own **trivia
sidebands** — the comment table, and the author's grouping parentheses. A
grouping paren carries no semantics (`parse_paren_expr` returns the inner
expression unchanged), which is exactly what makes it authorial: `(y % 4) == 0`
tells a reader which sub-expression to bind first, and the formatter is not
entitled to that edit. The parser records the wrapped span, one entry per
layer, and the emitter reads it — see [Author
parentheses](#author-parentheses).

What none of the three inputs preserves is genuinely lost: original whitespace,
and line breaks that don't matter. Everything
else round-trips, and the formatter goes out of its way to preserve a handful of
facts that *look* cosmetic but are semantically load-bearing on re-parse
(visibility on statics, the `:`-vs-`= "sym"` shape of a function body, the
author's suite layout). The goal is **idempotence**: `fmt(fmt(x)) == fmt(x)`,
which the unit tests assert directly (`src/formatter/mod.rs:7653`, `:7953`, and
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
// src/formatter/mod.rs:6691
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
`format_source_infallible` (`src/formatter/mod.rs:7431`), which panics rather than
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

`Emitter` (`src/formatter/mod.rs:15`) is a string buffer that tracks the current
indentation level, the current column, and whether the cursor is at line start. Its
contract is simple:

- `write(s)` lazily emits the pending indentation (four spaces per level,
  `src/formatter/mod.rs:41-49`) the first time text is written on a fresh line, then
  appends `s`.
- `indent()` / `dedent()` bump the level; `newline()` ends the current line and
  is **idempotent at line start**; `blank_line()` emits *at most* one blank line
  and never doubles up.
- `current_col()` (`src/formatter/mod.rs:63`) answers *which column the next character
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

Indentation is always four spaces, hardcoded (`src/formatter/mod.rs:43`, mirrored by
`doc::INDENT_WIDTH = 4` in `src/formatter/doc.rs:13`). There is no configuration:
the formatter is opinionated and has no options struct.

The `Formatter` struct (`src/formatter/mod.rs:595`) wraps an `Emitter` plus the
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

The renderer carries one piece of state the algebra itself does not: `tail_reserve`,
the width the CALLER has committed to emitting after this Doc on its final line. It
is consumed at exactly two fit tests — `Group`'s flat test and `Fill`'s last-item
test — and is the subject of [The tail reserve](#the-tail-reserve).

The renderer (`src/formatter/doc.rs:393`) walks a `Doc` against a `max_width`. For
each `Group` it calls `measure_flat` (`src/formatter/doc.rs:586`) — which returns the
single-line width, or `None` when the subtree cannot be flattened at all — and renders
flat iff `current_col + width + tail_reserve <= max_width`, else broken
(`src/formatter/doc.rs:433-448`). The maximum line width is `MAX_WIDTH = 120`
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

`surround_fill` (`src/formatter/doc.rs:255`) is the workhorse for every list the
formatter lays out — parameter lists, call arguments, generic parameters and
arguments, closure parameters, array / tuple / dict literals, grouped imports. It
emits `open item1, item2 close` when the list fits, and when it does not, it **packs
greedily**: as many items per line as the budget allows, continuation lines at the
block indent one level in, and the closing delimiter following the last item inline.

Every one of those kinds reaches it through a single chokepoint,
`Formatter::emit_delimited_list`, and so does the `Expr::StructLiteral` arm, which
`gg fmt`'s parse-only pipeline never reaches but which is kept converted so the
class rule has no exception: ten call sites in all (array and set literals share one
arm, so the two spellings count once). The grouped import was the last hold-out and
joined them when its carve-out's reason — the name sort — was retired. What the
chokepoint decides is described under [Interior comments and the delimited-list
chokepoint](#interior-comments-and-the-delimited-list-chokepoint).

```text
void draw_text_atlas(GpuContext &ctx, FontAtlas font, String text, float x, float y, float char_size, float r, float g,
    float b, float a):
```

Four properties fall out of the packing loop (`render_fill`,
`src/formatter/doc.rs:463`) and are worth stating because each is load-bearing:

- **The fit test for the last item includes the closing delimiter.** Otherwise the
  final line overruns by exactly `close.len()`, which is why `Fill` owns its `close`
  instead of leaving it as a sibling `Text`.
- **At least one item lands on every line.** An item too wide to fit even alone is
  emitted anyway and overflows — at the continuation indent when the packer broke to
  reach it, and at the CALLER's own column when the break was suppressed because it
  would not have narrowed anything (see the fourth property below; the suppressed
  shape is pinned by
  `tests/fixtures/fmt_tail_reserve/narrow_suppressed_half.gg`). (The other budget
  escape is the `from x import a, b, c` name list, which never reaches a packer at
  all: it is the one undelimited list in the language, and an undelimited list
  cannot wrap in indentation-based syntax without re-parsing as a new statement. The
  ratified parenthesized form retires that exemption: once the list has
  delimiters it routes through the chokepoint like every other, which also gives
  it the interior-comment gate for free.)
- **A multi-line item never shares a line.** It measures `None`, so the packer always
  breaks before it, which places it at precisely the column its sub-render assumed;
  packing then resumes from the column its last line ended at.
- **A break is spent only when it NARROWS.** A one-item list whose open delimiter
  already sits at or left of the continuation column gains nothing from breaking —
  `f(` at column 6 with a continuation column of 8 would move the item two columns
  RIGHT and hand back a line just as wide, plus a second one. The break is suppressed
  there, and only there: the item must be flat-measurable (a multi-line item was
  pre-rendered for the continuation column and has to go to it), and it must be the
  list's ONLY item. The `fits` test still chooses the MODE; this rule decides only
  whether a newline is written, and the separating comma is emitted regardless.

  There is deliberately no `Doc::Group` analogue. A group's flat test is
  all-or-nothing: a group that does not fit falls back to its own internal break
  positions rather than choosing between two columns for a single atom, so there is
  no "would this break narrow anything?" question to ask.

There is **no trailing comma** in a fill-broken list: the close is inline after the
last item, so there is nothing for a trailing comma to precede. That is a statement
about the SHAPE, not about the author's input — a list whose author wrote a trailing
comma never reaches this shape at all, because the comma routes it to the exploded
one (see [The magic trailing comma](#the-magic-trailing-comma)).

`surround` (`src/formatter/doc.rs:207`) is the other shape — one item per indented
line, *with* a trailing comma, built from `Group`/`Line`/`IfBreak`. `IfBreak` is what
makes that comma conditional: `IfBreak { flat: "", broken: "," }`, so a flat list has
no trailing comma and a broken one does (`src/formatter/doc.rs:213-216`, exercised at
`doc.rs:723` and `:753`). No production call site uses it — a comment-bearing list
reaches the same exploded shape imperatively, through
`format_bracketed_broken_with_comments`, because comments cannot survive the
pre-render that fill packing depends on. The `formatter_list_emit_fill_census` lint
counts both spellings — and the chokepoint's own call sites and carve-outs — so that
opting a list out of the canon, or out of the gate, stays a visible decision.

### Interior comments and the delimited-list chokepoint

A comment written *inside* a delimited list is the one thing fill packing cannot
carry. `Fill` writes the `", "` separator **after** each pre-rendered item, so an
item whose text ended in `# note` would swallow the separator and the rest of the
list, and the output would not re-parse. That is why `element_to_string` hands its
sub-`Formatter` an empty comment side-table: the sub-render is structurally
comment-blind, by design rather than by omission.

The consequence is that the decision has to be taken **before** the `Doc` layer —
the last point at which the comment side-table is still visible. That point is
`Formatter::emit_delimited_list`. It takes a typed `Gate`, and the type is the
design: `Gate::Span(open, end)` says *consult the side-table over this source
range*, while `Gate::UngatedCarveOut(reason)` says *do not* and carries the reason it
does not. An `Option` would have let the next list emitter opt out with an
anonymous `None` and every guard count unchanged, which is exactly the class the
chokepoint exists to retire.

A `Gate::Span` whose range contains an unemitted comment routes the list to
`format_bracketed_broken_with_comments`, which re-renders every element on the
**outer** formatter — so a nested list recurses back into the gate and reaches the
same exploded shape. Hence the canonical form:

> A fill-emitted container with an interior comment breaks fully, and so does every
> ancestor fill-emitted container on the path to it.

An interior comment is not the only thing that routes a list to that shape — a
magic trailing comma does too, and it does so WITHOUT a gate. The blockquote is
therefore about the comment cause specifically; the two causes and how they
differ are set out under [The magic trailing
comma](#the-magic-trailing-comma).

Three consequences are worth stating rather than discovering. First, a gated list that
would have fit on one line breaks anyway — one comment explodes the whole nested
chain of containers. Second, the exploded form carries a **trailing comma**, so a
commented parameter list is printed

```text
int add(
    int a,
    # the second addend
    int b,
):
```

and the same shape reaches trait signatures and extern declarations, which share the
parameter-list emitter. Both re-parse and both are idempotent; the trailing comma is
the existing exploded-list canon, not a new spelling invented here.

Third, once a container is exploded its elements occupy whole lines, so the
author's blank lines BETWEEN them become expressible — and are therefore kept,
under the same preserve-and-cap rule the member containers follow. The
asymmetry is worth naming: a blank line survives in an EXPLODED list and cannot
survive in a fill-broken one, because a fill-broken list has no per-element
line to hang it on. That is not a special case for lists but the general shape
of the formatter's bargain — it owns layout, and a blank can only be preserved
where the chosen layout has somewhere to put it.

The gate's open position is the **open delimiter itself**, never the first element's
start — a comment between `(` and the first argument is before every element and
would otherwise fall outside the window. Container literals and generic *parameters*
get that position free from the AST; the rest derive it with a comment-aware byte
scan over a window that provably holds no string literal. The window rule has one
sharp edge: explicit generic arguments can contain a `(` of their own, as
`identity[Callable[void(int)]](c)` does, so the argument tuple anchors at the
generic-args `]` rather than at the callee name, and if that scan finds nothing the
argument tuple inherits the miss instead of falling back to the unsafe anchor.

Scanning rather than threading a parser-recorded span is a deliberate call: the
formatter is already a source-consulting consumer — comments themselves are a span
side-table rather than AST — and a delimiter offset is a pure layout fact with
exactly one consumer.

One region stays outside the gate, for a stated reason: method-chain segments are
pre-rendered into the comment-blind sub-formatter one level above the list emitter,
so a gate there would read as live while being dead code; closing that needs the
same decide-before-`Doc` move in the chain builder itself. It is spelled
`Gate::UngatedCarveOut` with its reason, at the two `format_chain_segment` sites;
the only other such site is `gate_or_scan_miss`, the single `Option -> Gate`
converter through which every delimiter-scan miss routes.
`formatter_list_emit_fill_census` pins that exact (enclosing fn, reason) set — a new
carve-out is a visible decision, not a silent one.

The grouped import used to be a second such region, and its reason was the
alphabetical name SORT: emitted order was not source order, and the comment cursor
is forward-only, so interleaving per element would have flushed a later name's
comment against an earlier one. The sort is gone (see [Import
sorting](#import-sorting)), and the group now passes the gate like every other
list.

### The magic trailing comma

A trailing comma after a list's last element is the AUTHOR saying **keep this
exploded**; its absence lets the formatter pack. Before that rule the token carried
no meaning at all — the formatter deleted it when packing and added one when
exploding, so writing it or not made no difference to anything. Black's rule gives it
one, and the comma becomes explicit, greppable intent in the code rather than
metadata above it.

It is a second disjunct at the same chokepoint, so the **honour set is the gated
set**: routing a list kind through `emit_delimited_list` buys it the interior-comment
gate and the magic comma together, and a new kind cannot get one without the other.
That equality is a claim about the KIND SET, and it must not be read as a claim about
propagation, which is asymmetric:

- `has_interior_comments` reads the whole container span, so a comment nested
  anywhere inside is visible to **every ancestor** — hence the blockquote above;
- the comma probe's window is the last element's span-end to the close, i.e.
  **per-container**, so a nested magic comma explodes only its OWN list. The parent
  then rides the ordinary "a multi-line item never shares a line" rule.

The probe is deliberately **gate-independent**, and that is not a refinement but a
correctness requirement. `format_chain_segment` hands `Gate::UngatedCarveOut`
straight into the call-argument emitter for every method chain of two or more
segments, so a read taken inside the `Gate::Span` arm would be dead there and would
silently DELETE the author's comma inside a kind that is supposed to honour it. The
comment TABLE cannot travel into a pre-rendered sub-formatter; the SOURCE can, so a
source read works where a table read does not.

Two properties make the read total rather than heuristic.

**It is a decisive-BYTE read, not a window scan.** Advance past the last element's
end, skip whitespace and comments, and look at the first byte that is neither: `,` is
the author's comma and anything else — the close delimiter included — is not. There
is no upper bound, and none is available: `container_end` is exactly what a carve-out
does not have, and `source.len()` would be wrong rather than merely loose, because in
`[a.map(f).filter(g), b]` it reads the OUTER list's separator as the segment's own
comma. The comment skipping is the probe's own `#`-to-EOL scan and must stay that way
— it runs on sub-formatters, whose comment sideband is empty by design, so a
table-based skip is vacuous there and a `,` written inside a comment reads as the
author's.

**The advance past AUTHOR PARENS is what makes the last element's end usable.**
`parse_paren_expr` returns the inner node and pushes the grouping paren to a sideband,
so an element's recorded span stops BEFORE the author's `)`. Reading the byte at that
position for `f(1, (2 + 3),)` finds `)`, not the comma. The advance uses
`AuthorParenTable::layers_ending_at`, keyed by END offset rather than by the wrapped
node's exact span, because two common shapes make an exact-span lookup miss: a binary
node is spanned `lhs.merge(paren-stripped rhs)`, and a call argument is spanned from
the argument's own start. Summing across different spans that share an end is correct
rather than an over-count — same-offset keys nest — and the advance skips whitespace
and comments before each `)` rather than counting bytes, because `x + ( y )` puts
space between the node's end and its paren. The same advance fixed a live defect at
the sibling scan in `paren_tuple_gate`, where locking onto the author's `)` truncated
the gated window and let an interior comment escape the call it annotated.

The one-element tuple `(x,)` is exempt and always will be: there the comma IS the
syntax that makes it a tuple rather than a parenthesised expression, not a layout
signal. Black exempts the same shape.

### Splicing the two layers together

The bridge is `write_doc` (`src/formatter/mod.rs:1139`). It renders the `Doc` with
`doc::render_at` — passing the emitter's *current column and indent level as the
starting state* so the wrapping decision accounts for text already on the line — then
writes the pre-rendered (newline-bearing) string back via `Emitter::write_preformatted`
(`src/formatter/mod.rs:75`), which prepends the base indentation and recomputes the
column from the last newline of the spliced text.

"Current column" is asked for through `Emitter::current_col`
(`src/formatter/mod.rs:63`), never off the raw `col` field. Indentation is written
*lazily*, on the first `write` of a line, so at line start `col` is 0 while the text
about to be emitted will land at `indent * 4`. A statement that *begins* with a list
would otherwise be measured that many columns too narrow and silently overrun the
budget.

A subtlety of the hybrid design: the imperative layer frequently needs a *string*
for a sub-expression to drop into a `Doc::Text`. It gets one via `element_to_string`
(`src/formatter/mod.rs:1053`), which spins up a throwaway `Formatter` (with no
comments), runs a closure against it, and returns its buffer. In production the
spelling is almost always `element_to_string_reserving`, which additionally states
what the caller will write after the spliced element — see [The tail
reserve](#the-tail-reserve); the bare form survives for the one position with no
tail of its own, and a `tests/lints.rs` ratchet pins that at one. This is how
`format_method_chain` (`src/formatter/mod.rs:3819`) and `format_binary_chain`
(`src/formatter/mod.rs:3876`) turn each chain segment / operand into a `Doc::Text`
leaf before grouping them — the wrapping is decided over the *segments*, while each
segment is formatted by an ordinary recursive call.

The throwaway formatter is seeded with both the indent level *and* a starting column
(`element_to_string_at`, `src/formatter/mod.rs:1062`). It has to be: a sub-render that
measured from column 0 would believe it had a whole indentation's worth of extra
budget, and emit lines that overflow by exactly that much once spliced. The seed it
is given is the **continuation column**
(`base_indent * 4`), and it is self-consistent at the two splice positions that land
there: a broken list's continuation-indented element, and the fill packer's next
line.

It is *not* self-consistent at a third position, and that gap is filed rather than
papered over. A `Doc::Group`-clothed carrier's FIRST piece — a binary chain's operand
zero, a method chain's root — is spliced at the **caller's** current column, because
nothing in the Doc precedes it. The sub-render still measured from the continuation
column, so it believes it has the difference in extra budget and can keep a nested
call flat that should have broken. The repro is
`tests/fixtures/known_gaps/fmt_prerender_column_binary_chain.gg`, asserted by the
`#[ignore]`d `fmt_prerender_column_binary_chain_stays_in_budget`; the fix is a
start-column parameter on the sub-render, not a change to the reserve, which already
reaches those pieces correctly.

### The tail reserve

The column a `Doc` *starts* at is only half the context. The other half is what the
caller is going to write **after** it, on the same line. An `extern`'s parameter list
is followed by ` = "gorget_symbol"`; a function signature's by `throws E:`; a
statement header's by `:`; a packed call's `)` by a `.field` the packer never saw.
Every one of those lands on the Doc's final line, and a budget read as a global
constant at the decision site cannot see any of them.

So the number is carried DOWN rather than reconstructed. `Renderer` holds a
`tail_reserve` (`src/formatter/doc.rs:360`), `Formatter` holds one (`src/formatter/mod.rs:652`),
`write_doc` hands it across, and exactly two fit tests consume it: `Doc::Group`'s
flat test and `Doc::Fill`'s last-item test. This is Wadler's and Prettier's
`fits(rest)` algebra restored one step past the delimiter — `Fill` already charges
its own `close` to the last item for precisely this reason, and the caller's suffix
is the same quantity one layer out.

**What the number covers**, stated once because everything else refers to it: the
caller's tail on the Doc's final line, up to the **next render's first break
opportunity**. A following render's leading literals — its open delimiter, its
` = ` — count INTO the reserve, because a `Doc::Text` is pushed with no fit test at
all; only from that render's first fit-tested node onward does it manage its own
budget. Both neighbouring readings are wrong, and measurably so: stopping at the next
breakable *region* under-reserves by exactly the leading literal, and charging the
whole following region over-reserves and breaks lines that were within budget.

Three consequences follow, and none of them is silent:

- The reserve is installed by a scoped, **additive** helper immediately around the
  render it charges — never around a larger region that also emits other things. A
  nested list inside a suffixed header therefore inherits its parent's reserve for
  free, and a chain of postfix operators accumulates its own tails as it recurses.
- Consuming it at *every* `Group`/`Fill` fit test rather than only the outermost is
  safe-not-exact: an inner node may break one step early, but no line can overrun.
  The exactly-120 fixtures under `tests/fixtures/fmt_tail_reserve/` are what guard
  that over-reserve direction — each cell pairs a construct that must stay flat at
  120 with its twin at 121 that must break.
- A `Doc` spliced in several pieces charges the reserve to each piece, which is the
  same residual in a different costume.

Where the tail contains *rendered* content rather than pure literal text, it is
MEASURED, not guessed: the caller pre-renders it at width 0 — every break
opportunity breaks — and takes the FIRST LINE, which is exactly the leading
unbreakable run. A type alias's tail is `" = Dict["`, not `" = "` and not the whole
flat type. That probe is a *measurement*, marked as such by a typed
`doc::RenderPurpose` rather than inferred from the width, and the
break-only-if-it-NARROWS rule is inert inside it: "would a break narrow this line?"
has no meaning at a probe width, and asking it there would glue a short one-item tail
onto the measured prefix and over-reserve every caller downstream.

Two positions are outside the mechanism, and both are ruled rather than pending:

- **The exploded comment-bearing path.** When an interior comment routes a list to
  `format_bracketed_broken_with_comments`, every element is re-rendered on its own
  line ending in `,`, so each is charged an EXACT reserve of 1 — exact, not additive,
  because a live caller reserve belongs to the CLOSE line and would over-reserve
  every element by tens of columns. That path is a hand-rolled loop with no `Fill`
  and no fit test, so the close line itself has no enforcement at all: it is written
  at the outer indent and the caller's suffix follows it unmeasured.
- **The inline-BODY collision.** When the author put a suite, a match arm, or a
  closure body INLINE on its header's line, the header is charged for the `:`, the
  separating space, and the body's leading unbreakable prefix. A breakable header
  breaks and the line comes back into budget with the layout untouched. But when the
  body's leading unbreakable text alone exceeds what remains, no break can save the
  line — and converting the suite to a block form to reclaim the width would
  overwrite a choice the author made, for a reason invisible in the source. The line
  overruns, the form stands. Width may break the header; it never re-decides the
  suite's FORM.

Finally, some positions have no width-decided render at all. A `Stmt::VarDecl` whose
initializer is a single long string, a `case` pattern, an enum tuple-variant field
list — each is emitted by an arm with no `Doc` layer, so no fit test ever runs and no
reserve can help. Those are missing capabilities, not budget escapes; they are filed
as feature gaps rather than papered over by widening what "unbreakable atom" means.

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

A **nullary variant pattern** is the same story one layer down. `case Idle:`
and `case Idle():` are the same match, and a zero-field
`Pattern::Constructor` is what both a parenthesised nullary and a qualified
bare name (`Mode.Fast`) parse to — so a formatter that always emits `()` and
one that never does are each wrong for half the corpus. `paren_spelled` on
`Pattern::Constructor` and `Pattern::DotShorthand` carries the choice, written
at the four parser branches that did or did not consume the `(`. It is the
`ArrayLiteralSpelling` shape: the node already exists and already reaches the
formatter, so the bit belongs on it rather than in a span-keyed sideband. The
emit rule is `!fields.is_empty() || paren_spelled` — the first disjunct keeps a
*synthesised* constructor (the loader's variant-qualification rewrite, which
has no author and writes `false`) emitting legal syntax.

The distinction is not cosmetic either: an unqualified bare name in a pattern is
also the spelling of a *binding*, so the two forms diverge exactly when the name
is not a variant of the scrutinee — one fails to resolve, the other silently
becomes a catch-all.

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
(`src/formatter/mod.rs:1010-1025`), and a trailing newline is guaranteed
(`src/formatter/mod.rs:1026-1029`). That collapse is the CAP half of the rule.

The PRESERVE half happens during the walk, and the rule is uniform: **a blank
line is emitted where the author wrote one, and nowhere else.** Every emit site
is guarded by a predicate that reads the source — `has_blank_line_between` for
the member and statement loops, `blank_before_clause` for clause headers (it is
`has_blank_line_between(0, anchor)` under the name), and
`blank_line_directly_above` / `blank_line_follows` for the comment flushes,
which own the blanks below and between a comment run so the two families cannot
double-count. The formatter manufactures nothing, and it deletes nothing but the
second and later blanks of a run.

The exception set is CLOSED at two sites, both in `format_module`, and both
about SECTIONS rather than items. The directives loop calls
`blank_line` between the directive block and the first import
(`src/formatter/mod.rs:2384`); the imports loop calls `blank_line` between the
std and non-std groups (`src/formatter/mod.rs:2409`). These separate regions the
formatter itself reorders, so there is no author position to read — the blank
belongs to the emitted layout, not to the source. Everything after them —
`format_module`'s own rest loop, nested items, struct fields, enum variants,
trait items, equip methods, `extern` declarations, block statements, a
destructuring closure's body, and every clause header — reads the author.

Two censuses in `tests/lints.rs` hold the class, on the two axes it can fail
along. `formatter_child_collection_loop_census` is keyed on child-collection
LOOPS and carries a derived blank-policy column, which is the axis that can see
a container with **no** emitter at all — the shape a call-site census is
structurally blind to, and the shape the `extern` block had.
`formatter_blank_emit_site_census` is keyed on the emit CALLS and covers the
non-loop positions plus any loop that emits through a closure; it is what keeps
the two-site exception set closed.

Two neighbours are deliberately not cited above: `Emitter::blank_line` itself
(`src/formatter/mod.rs:132`) is the definition, not an emit site.

The trailing-comment aligner `align_trailing_comments` runs before this collapse
(`src/formatter/mod.rs:1005`) and is unaffected by it — the collapse only removes
newlines, never touches a within-line gap.

### Import sorting

Two axes, and only one of them is sorted.

The STATEMENT axis is. `format_module` (`src/formatter/mod.rs:2338`) partitions
items into leading directives, imports, and "the rest" — the partition stops at the
first non-import, non-directive item (`past_imports`,
`src/formatter/mod.rs:2343-2356`), so only the *leading* import block is reordered.
Within that block imports are sorted with std/`xtd` libraries first, then
alphabetically (`src/formatter/mod.rs:2358-2371`; `is_std_import` at
`src/formatter/mod.rs:7472`).

The NAME axis is not. The names inside an import keep the order the author wrote,
on both spellings: `import a.{X, Y}` groups are fill-packed in source order, so a
long group packs across continuation lines (`src/formatter/mod.rs:3366-3413`), and a
`from a import …` name list is emitted in source order too — but **not** wrapped,
because in indentation-based syntax a bare name on a fresh line would re-parse as a
new statement (`src/formatter/mod.rs:3414-3445`). Ordering a name list is a
statement the author is making — a `CollectionKind, CkNotCollection, CkVector, …`
list puts the enum type at the head of its own variants — and a formatter that
alphabetises it deletes that statement while preserving every character of it.

The `from` spelling's two entry kinds, plain names and `Type.*` globs, share ONE
ordered vector discriminated by `ImportName::glob`. That is not an implementation
detail: they interleave freely in the source, so parsing them into two vectors
loses the relative order irrecoverably, and no amount of care at the emitter can
put it back.

Removing the name sort also RETIRED the grouped import's carve-out from the
interior-comment gate. The carve-out's whole reason was the sort — emitted order
was not source order, and the comment cursor is forward-only, so interleaving per
element would have flushed a later name's comment against an earlier one. With
emitted order equal to source order the group routes through `emit_delimited_list`
like every other list, and a comment written inside a grouped import now stays
inside it.

### Type-first re-printing and bare-tuple positions

Gorget is type-first (`int x = 5`), and the formatter prints declarations that way:
`format_param` emits `type [&|!]name` (`src/formatter/mod.rs:3264-3314`), `VarDecl`
emits `type name = expr` (`src/formatter/mod.rs:4604-4654`). Ownership sigils (`&`,
`!`) print *immediately before the name*, via `format_ownership_prefix`
(`src/formatter/mod.rs:6722`), matching the language rule that the sigil binds the
binding, not the type.

Several positions canonicalize a tuple to its **bare** (parens-free) spelling because
that is the idiomatic form the parser accepts: function return types
(`src/formatter/mod.rs:2885-2893`), `return a, b` (`src/formatter/mod.rs:4669-4679`),
`auto a, b = …` destructuring (`src/formatter/mod.rs:5204-5221`), and `for x, y in …`
patterns (`src/formatter/mod.rs:4709-4718`).

### Visibility: the load-bearing "cosmetic" cases

`public Foo` and a bare `Foo` both parse to `Visibility::Public`, so the value alone
cannot say which the author wrote — and both directions of guessing are wrong.
Emitting a keyword only for `Private` deletes every explicit `public` in the tree;
emitting one unconditionally rewrites every declaration that relies on the default.
The parser therefore records which spelling it consumed, on `explicit_visibility`,
and `format_visibility` (`src/formatter/mod.rs:2798`) reads that fact and nothing
else: **the keyword is emitted iff one was written.** Nine declaration kinds carry
the flag, and `formatter_visibility_emit_site_count` in `tests/lints.rs` cross-checks
the emit sites against the carriers so a tenth kind cannot quietly skip the path.

Statics invert the default and keep their own rule (`format_static_decl`,
`src/formatter/mod.rs:3507-3527`). They are private-by-default, so `public` is
emitted whenever the value *is* public — for a parsed static that means the author
wrote it, and for a synthesised one it stops the emission from silently flipping the
declaration to private and breaking cross-module imports — while `private` is emitted
only when explicitly written, since inventing the default everywhere would rewrite
the tree.

### String literals and the verbatim chokepoint

A string literal is emitted **verbatim first**: `format_string_lit`
(`src/formatter/mod.rs:6825`) asks for the author's own lexeme and, when it can have
it, writes exactly that. Quote style, the prefix letter, which escape spelled a
character, the f-string brace form and — the case that makes a long `"""` block
readable — its physical line layout all survive, because nothing was regenerated.

Recovery goes through one helper, `Formatter::verbatim`
(`src/formatter/mod.rs:5684`), and it is the same helper behind every other form the
AST drops: an integer's radix and digit grouping, a float's trailing zeros, `b'A'`
versus `65`, `byte` versus `uint8`, and the quoted **name-strings** the AST stores
decoded (test and bench names, snapshot names, attribute string arguments, extern ABI
tags). Routing them through one place is what makes the property below a property of
the class rather than a habit of each arm; `formatter_verbatim_emit_arm_count` in
`tests/lints.rs` keeps a new arm from spelling its own quotes.

**The property: a recovered lexeme is re-lexed and compared before it is trusted.**
`verbatim` slices the source at the node's span, hands the slice to
`relex_single_token` (`src/formatter/mod.rs:6888`) — which asks the *real lexer* and
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
`canonical_string_escape` (`src/formatter/mod.rs:6935`): raw strings pass through,
`{`/`}` double inside f-strings, every control character is escaped — C0 and DEL as
`\xHH`, C1 (`0x80-0x9F`) as `\u{XX}`, because the lexer rejects `\x` above `0x7F` and a
raw C1 byte would plant an invisible control character in the user's source. Printable
non-ASCII stays raw.

No `.gg` source can reach that path, which is exactly why the escape policy lives in a
free function with unit cells of its own (`src/formatter/mod.rs:6651`) instead of a
fixture that would be green for the wrong reason.

### Author parentheses

A grouping paren is the one authorial form the AST cannot carry at all. Comments
are partitioned out of the token stream; a literal's radix survives at its span.
But `parse_paren_expr`'s parenthesized-expression branch returns the INNER
expression node, unchanged — after it, `(a + b) + c` and `a + b + c` are the same
tree. The parser is therefore the only place that knows a paren was written, and
it records the wrapped expression's span in a trivia table
(`src/parser/parens.rs`), one entry per layer, the same sideband idea as the
comment table. There is deliberately no `Expr::Paren` node: a wrapper every
semantic pass had to see through is one pass away from a mis-analysis, and the
formatter is the only consumer.

The emitter's rule is **`max(author_layers, required)` per node**, never
`author + required`. The sum has no fixed point: the formatter's added pair
re-parses as another author paren on the next run, so `(` becomes `((` becomes
`(((`. Taking the maximum instead means one paren on the page satisfies both
readings. `format_expr` emits the author's layers and delegates the expression
match to `format_expr_inner`; the two sites that add a paren of their own stand
down when an author layer covers the SAME node:

- `format_expr_maybe_parens` — the precedence wrap, shared by the operand
  emitters;
- `wrap_multiline_expr_in_parens` — the broken-chain wrap, which is the second
  documented use of `IfBreak`. A chain that breaks across lines emits its
  continuations as `+ b` / `?? b`, and a bare leading-operator continuation is
  not valid Gorget, so the pair appears in broken mode only:
  `IfBreak { flat: "", broken: "(" }`. That is what makes the wrap invisible
  when the chain fits, and load-bearing when it does not.

Suppression is **same-span only**. A paren on a different node never cancels a
required one, because nothing then guarantees the reparse-required paren is on
the page at all: on `for i in (^start)..end:` the author's paren sits on
`^start` and the required wrap on the range, so both are emitted —
`((^start)..end)`, idempotent, re-parsing to the same program.

Two consequences worth stating, because both look like bugs from a distance:

- The flattener has to ask too. `collect_binary_operands` flattens same-operator
  left-nested chains, and a sub-chain the author parenthesised has the identical
  AST — so the check runs at every recursion step, and a paren-grouped pair
  stays ONE operand of a broken chain instead of splitting across lines.
- Speculation has to unwind the log. `parse_expr` is reachable under
  `Parser::try_parse` (a generic argument's array size), so the backtrack path
  truncates the push-log; otherwise an abandoned parse leaves a phantom layer
  and the fallback parse records the same paren twice.

### Function body shapes

`format_function` (`src/formatter/mod.rs:2808`) preserves the four distinct body
shapes the AST distinguishes, each of which round-trips to a different construct:
block body (`:` + indented stmts), expression body (`: expr` on one line), a bare
declaration (signature + newline, for trait method signatures), and an extern body
(`= "symbol"`) (`src/formatter/mod.rs:2972-2976`).

## `meta` constructs

Compile-time `meta` forms are kept verbatim — the formatter does **not** evaluate or
expand them (that is Pass 0's job; see chapter 6). They re-print as written:

- **Item-level**: `meta const`, `meta type` (plain / conditional / call RHS variants
  at `src/formatter/mod.rs:2523-2557`), `meta type … (params)` functions, `meta assert`,
  `meta if`/`elif`/`else` over *items* (`src/formatter/mod.rs:2595-2670`), and
  `meta log` — all in `format_item` (`src/formatter/mod.rs:2485-2685`).
- **Statement-level**: `meta if` (`:5061`), `meta for` (`:5072`), `meta match`
  (`:5085`), `meta while` (`:5140`), `meta const` (`:5150`), `meta log` (`:5157`) in
  `format_stmt`.
- **Expression-level**: `meta`-prefixed operators — `a meta[op] b` for infix
  (`src/formatter/mod.rs:6637-6642`) and `meta <op>` token form
  (`src/formatter/mod.rs:6643-6646`).

The AST is deliberately structured so that `meta if`/`meta for` carry their bodies as
*real* statements/items (so resolution and the rest of the pipeline can see inside
them), and the formatter walks those bodies normally with `format_block_stmts` /
`format_item` — it does not collapse a `meta` block to a placeholder. (Contrast this
with the self-host AST-canonicalizer, below, which *does* collapse them.)

### `meta for` inside `match`

A `match` arm list can contain a `meta for` that templates arms. The formatter handles
the `MatchItem::MetaFor` variant inline inside the `Stmt::Match` walk
(`src/formatter/mod.rs:4764-4777`): it prints `meta for vars in range:` and then the
single `arm_template` indented beneath it, rather than treating it as a regular arm.

## Match arms and guards

`format_match_arm` (`src/formatter/mod.rs:5234`) prints `case <pattern>`, then — **if
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
> (`src/parser/ast.rs:1015`) and the production formatter emits it verbatim
> (`src/formatter/mod.rs:5233-5236`). The guard-suppression behavior belongs to the
> *self-host AST-debug canonicalizer* (`tests/fixtures/self_host_*/format.gg`), a
> different program with a different purpose — see "In the self-host" below.

## Comment handling

Comments are not part of the AST. The parser drops them into a side-table, and what
that table CARRIES is the whole design. Each entry is a `CommentEntry`
(`src/parser/comments.rs`): the text, the span, the byte offset of its own line start,
the character column of its `#`, whether code precedes that `#` on its line
(`CommentPlacement`), and the index of the run head it continues. The `Formatter`
holds the table plus a forward-only `comment_cursor`.

Those facts are WRITTEN where they are known, not reconstructed where they are used.
The lexer has two comment producers and they ARE the two placements — a comment-only
line and a comment after code — so each producer records its own placement and column
as it scans (`src/lexer/mod.rs`), and both normalize the token's span to start at the
`#` so that one position means one thing. The one CROSS-ENTRY fact, run membership, is
derived once at the parser's collection site (`CommentTable::build`), because it is a
relation between neighbours that no single token knows.

Interleaving is span-driven and one-directional: before emitting an item, field,
variant, statement, or branch body, the walker calls `emit_comments_before(pos)`
(`src/formatter/mod.rs:1190`), which flushes every comment whose `#` precedes `pos`,
each on its own line, advancing the cursor. After the whole module is walked,
`emit_remaining_comments` (`src/formatter/mod.rs:1211`) flushes whatever is left (e.g.
trailing comments after the last item).

A leading flush would move every end-of-line comment onto its own line, so a comment
that *trails* its node is claimed before the cursor can reach it.
`emit_trailing_comment_after(prev_end)` (`src/formatter/mod.rs:2181`) asks one
question — is this comment's start on the same source line as the previous emit's
last character? — and if so injects it after the emitted line, ahead of the newline
that statement already wrote. A comment separated by a line break fails that test and
stays a leading comment at the next sibling's indent, which is the right answer for a
standalone comment *between* two nodes.

### The run: a trailing comment is not always one line

A trailing comment continued on the lines below it — each `#` at the head's column, no
blank and no code line between — is ONE logical comment, and it is how a field's
rationale gets written without a 200-column line. `CommentEntry::continues` is that
concept, defined once in the table; a doc comment cannot join a run, because an
own-line `#/` lexes as a different token that never enters this table at all.

Consumption is a single pair of functions. `claim_run_at_cursor`
(`src/formatter/mod.rs:1246`) is the ONLY place the cursor advances: it takes the
comment at the cursor plus every continuation of it, so whichever hook claims a HEAD
takes the whole run. `emit_claimed_run` then places it, at one of three typed
positions — on its own lines at the current indent (the leading and EOF flushes, and
the orphan flush below), injected onto the buffer's current line (the trailing hook),
or injected onto a line RECORDED EARLIER. That third position is the inline suite,
whose body renders between the claim and the emission and may itself end the line; the
claim records the header's position and the emission resolves it, so the comment stays
where the author wrote it. Continuation lines always keep their own lines: collapsing a
run onto one line re-lexes as a single comment token, which makes the collapse silent
and a fixpoint.

A run is also why the aligner measures group adjacency from a head's LAST line rather
than its own: the continuation lines carry no trailing entry, so an unmodelled run
looks like a gap in the run of commented lines and splits the alignment group.

### The orphan before a block closes

A comment written after a block's LAST child leads no next child and trails no previous
one, so no sibling hook claims it. `emit_orphan_comments_before_close`
(`src/formatter/mod.rs:1489`) is the shared chokepoint that does — it takes the block's
`header_start` and its end, and every closing block calls it immediately before its
`dedent()`.

Membership is the language's own layout rule, read from typed metadata: an own-line
comment whose `#` column is strictly greater than the indent of the block's HEADER
line, and whose `#` starts before the block's recorded end, was written inside the
block. COLUMN decides, not the span — a block's `span.end` is its DEDENT token, whose
span reaches into the NEXT code line, so a span ceiling alone drags the next item's
leading comment into the closing block. And the flush claims a PREFIX and stops: the
cursor is forward-only, so the first comment that fails either test ends the flush.

The anchor is the OWNING CONSTRUCT's first line, which is why `Block` carries a typed
`header_start` field written at `parse_block_body`. A block's own `span.start` is
whatever introducer the parser had in hand — the colon at most sites, a clause keyword,
a closure `(`, a body-line NEWLINE — and on a wrapped header the colon sits on a
continuation line indented at or past the body, where the column test refuses
everything. The container, arm-container and nested-item families are not `Block`-based
and anchor on their own node spans.

Nested blocks flush before enclosing ones, which emission order gives for free (an
inner `dedent()` precedes its outer one), and the forward-only prefix claim then
guarantees an outer flush never sees a comment an inner one owns. `formatter_dedent_close_census`
(`tests/lints.rs`) classifies every `dedent()` in the formatter against this family and
REDs on one it cannot attribute, so a new block close cannot ship without deciding
which class it is.

A comment interior to a delimited list is the third position, and it is the one the
forward-only cursor cannot reach on its own: the list's elements are pre-rendered
through a comment-blind sub-formatter, so by the time the cursor next runs the
comment has no element left to attach to and dedents to the enclosing scope. That
position is handled a layer up, at the delimited-list chokepoint described under
[Interior comments and the delimited-list
chokepoint](#interior-comments-and-the-delimited-list-chokepoint) — the list is
routed to an imperative exploded emission that re-renders each element on the outer
formatter, so the ordinary leading, trailing and orphan-before-close hooks all fire
at the list's interior indent. `format_bracketed_broken_with_comments`
(`src/formatter/mod.rs:1619`) is that emission: per element it flushes leading
comments, renders, writes `,` and a newline, then claims a trailing comment; after
the last element it flushes once more against the container's end, which is what
catches a comment sitting on its own line between the last element and the closing
delimiter.

Clause and block HEADERS get the same treatment one level up, and the layout decides
where the comment goes. When the suite is indented, the header owns its line, so the
comment is emitted at the header — `if c:  # why` keeps its comment on the `if`. The
anchor is the header's own `:`, recorded by the parser (`expect_block_start` returns
it; each container stores it as `header_colon_span`), because the colon is the one
position guaranteed to be on the header's LAST source line whatever the header's
shape — a wrapped generic parameter list, a wrapped `extends` list, a `with T via f`
tail. Anchoring on the container NAME instead works only for a single-line header, and
on a multi-line one the comment is correctly rejected and then falls into the body,
where it documents the first member. (`equip`'s colon is optional — the blank
`equip S with T` form has no body and therefore no header line to trail — so its field
is an `Option`.)
When the suite is INLINE the body shares that line, so emitting at the colon would put
the comment ahead of the statement it heads and the output would not re-parse; there
the comment is *claimed* at the header (which stops a leading hook inside the body
from taking it first) and emitted after the body. Every header position needs one of
the two: a clause that has neither drops its comment into the branch body, where
`format_block_stmts` re-emits it as a leading comment on the first statement and it
silently starts documenting that statement instead of the clause.

Two constants finish the shape. A trailing comment is injected exactly
`TRAILING_COMMENT_GAP` spaces after the code (`src/formatter/mod.rs:339`), and a
*run* of them is then aligned to a shared column — the smallest multiple of
`ALIGN_STRIDE` that clears the widest line in the run, with a comment that would
overrun `MAX_WIDTH` excluded as an outlier rather than dragging the group right.
That alignment is a pure post-pass over the finished buffer (`plan_trailing_aligns`,
`src/formatter/mod.rs:359`, plans the gap rewrites; the mutating half applies them
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
block from `:5302`, heavy on idempotence assertions) and `src/formatter/doc.rs`
(the pretty-printer algebra tests from `:478`).
