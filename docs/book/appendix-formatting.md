# Appendix E — Formatting and Code Style

Gorget ships a canonical source formatter, `gg fmt`. It settles every layout
question for you — indentation, wrapping, spacing, blank-line runs, comment
alignment — with no per-project style configuration and no options to bikeshed.
What it does *not* touch is the way you spelled the program: the formatter
changes the layout it owns, and nothing else. Running it is how Gorget code is
meant to look; the standard library and the self-hosting compiler are both kept
formatted.

## Running the formatter

| Command | Effect |
| --- | --- |
| `gg fmt <file>` | Print the formatted source to stdout. |
| `gg fmt <file> --in-place` (`-i`) | Rewrite the file in place. |
| `gg fmt <file> --check` (`-c`) | Exit non-zero if the file is not already formatted (for CI). |

Each invocation formats a single file.

## What the formatter guarantees

- **Value-preserving.** Formatting never changes what a program means or what
  its literals evaluate to. Every string value, every numeric value, and the
  structure of every expression survive untouched.
- **Form-preserving.** Stronger than value-preserving, and the rule that
  decides the cases below: where the language accepts more than one *spelling*
  of the same thing, the formatter keeps the one you wrote. Which escape spelled
  a character, which quotes wrapped a string, the base of a number, `byte`
  versus `uint8`, `await x` versus `x.await()`, `case Idle()` versus
  `case Idle`, the parentheses you added to show what groups first — these are
  choices you made for a reader, and re-spelling them is not the formatter's
  job. What the formatter owns is layout: indentation, wrapping, spacing,
  blank-line runs, comment columns.
- **Re-parseable.** The output is always a program the compiler accepts, and it
  parses to the same tree as the input.
- **Idempotent.** `gg fmt` applied to already-formatted source is a no-op:
  `fmt(fmt(x)) == fmt(x)`. This is what makes `--check` reliable.
- **Deterministic.** The output is a pure function of the input source — never
  of the environment, the file's path, or the order files are processed. Two
  authors who format the same file get byte-identical results.

## Layout

Blocks are indented four spaces per level, Python-style — indentation is the
block structure, and the formatter normalizes it.

The formatter preserves the blank lines you write as paragraph breaks, and
collapses any run of consecutive blank lines down to a single one. It never adds
one you did not write. A blank line
keeps its meaning next to comments, too: a blank above or below a comment, or
between two comments, is a deliberate break and is kept. That includes a blank
above a clause header — the space you leave between a long branch body and the
`else:` that follows it is paragraphing like any other, and survives:

```gorget
if ready:
    prepare()
    launch()

# nothing to do
else:
    wait()
```

The rule holds **inside every container**, not just between top-level items: the
members of a `trait`, the methods of an `equip` block, the declarations in an
`extern "C":` block, struct fields, enum variants, the statements of a function
or a closure body. Wherever you can write two things one after another, the
space you leave between them is yours.

It reaches inside a **list that is exploded** too — a list broken across lines
because you asked for it — so a long table of entries keeps the paragraphs you
grouped it into. The one place a blank cannot survive is a list the formatter
*packed*: the formatter chose those line breaks, so there is no boundary of yours
for a paragraph break to sit on. A blank line is kept wherever the layout has
somewhere to put it.

### Suite layout is yours

Gorget accepts two spellings for a suite: on the header's own line, or indented
beneath it. Both are idiomatic, and which one reads better depends on the code —
so the formatter keeps whichever you wrote, everywhere it is legal:

```gorget
if n == 1: print(1)          # stays a one-liner
elif n == 2:                 # stays indented
    print(2)
else: print(3)               # each clause keeps its OWN spelling
```

This holds for `if` / `elif` / `else`, `match` arms and their `else`, `meta
match` arms and their `else`, `on error`, and closure bodies — and it holds
symmetrically: the formatter neither explodes a one-liner nor collapses a short
indented suite. A one-liner that runs past the width budget stays a one-liner,
because the width budget governs how an expression wraps, not which form of
suite you chose.

Two spellings are not interchangeable everywhere, and there the language decides
rather than you: `on error` takes no colon in its inline form
(`on error cleanup()`), and a statement-position `match` accepts only the
indented form after `else:`.

An explicit `do:` block is likewise kept where you wrote it, and never inserted
where you did not. That one is more than cosmetic — `do:` makes its tail a read
position, so `else: do:` followed by `^value` is rejected where `else: ^value`
compiles.

Synonym spellings you chose are preserved — the formatter does not rewrite one
construct into another:

```gorget
byte channel = 3               # stays `byte`; `uint8 channel` stays `uint8`
Set[int] seen = {1, 2}         # stays braces; a Vector literal stays brackets
public int visible_api = 1     # an explicit `public` is kept where you wrote it
int module_local = 2           # …and never added where you did not
```

The same holds for `await x` versus `x.await()`, for the bare `with r:` against
`with make() as r:`, and for a module-level `int counter = 0` against the
explicit `static int counter = 0`. If you ever want the opposite — one canonical
construct chosen for you — that is a rewriting tool's job, not the formatter's.

A **nullary variant pattern** is in the same family. `Idle` and `Idle()` match
the same thing, and the parentheses are how you say "this is a variant, not a
name I am binding" — so both spellings are kept, wherever a pattern is legal:

```gorget
match state:
    case Idle():                 # stays parenthesised
    case Mode.Fast:              # …and a bare qualified variant stays bare
    case .Slow():                # dot-shorthand keeps its choice too
```

This is not cosmetic. A bare name in a pattern is *also* the spelling of a
binding, so `case Idle:` and `case Idle():` differ exactly when the name does
not resolve to a variant — one fails, the other silently matches everything.
Rewriting one into the other would trade a compile error for a catch-all.

The single exception is the pure keyword synonym `else if`, which carries no
information `elif` does not; it is canonicalized to `elif`.

**Your parentheses stay.** A grouping paren that the precedence rules make
redundant is still how you tell a reader what binds first, so the formatter
keeps it:

```gorget
bool leap = (y % 4) == 0 and ((y % 100) != 0 or (y % 400) == 0)
int total = base + (rate * hours)     # stays parenthesised
```

The formatter may still ADD a paren of its own next to yours when re-parsing
demands one — an expression that starts with an ownership sigil is the case that
comes up, where `for i in (^start)..end:` is emitted as
`for i in ((^start)..end):` — but it never removes what you wrote, and the
result is stable: running `gg fmt` again changes nothing.

### Line width and wrapping

Lines are budgeted at **120 columns** — counted in characters, so accented
letters and emoji cost one column each, not one per byte.

A list that fits stays on one line, *unless you say otherwise* — see the
trailing comma below. A list that doesn't fit is **packed**: the
formatter fills each line up to the budget and then wraps, rather than breaking it
into a vertical list. Continuation lines are indented one level
in from the line the list started on, and the closing bracket follows the last
element:

```gorget
void draw_text_atlas(GpuContext &ctx, FontAtlas font, String text, float x, float y, float char_size, float r, float g,
    float b, float a):
    pass
```

This applies to every horizontally-broken list: parameter lists, call arguments,
generic parameters and arguments, closure parameters, array, tuple and
dictionary literals, and grouped imports.

A packed list carries **no trailing comma** — the closing bracket is right after
the last element, so there is nowhere for one to sit. The other shape is
**exploded**: broken across lines *with* a trailing comma and a closing bracket on
its own line — one element per line unless you grouped them yourself, which the
next section is about. A list takes that shape when its elements carry comments
(comments belong to the lines they annotate), and when you ask for it:

One element can be wider than the whole budget — a long qualified name, a deeply
generic type. The formatter puts it on its own continuation line and lets it
overrun rather than breaking it somewhere meaningless. Four things can push a
line past 120: that single over-wide element; text the formatter writes after a
list's closing bracket — the `= "symbol"` of an `extern` declaration, or the `:`
that ends a signature — which the packer does not measure; an **import line**,
which is exempt from the budget entirely; and a **row you grouped yourself**
inside an exploded list, which is kept as you wrote it and never re-wrapped.

The import exemption is a consequence of the syntax, not a preference. A `from x
import a, b, c` name list is the one list in the language with no delimiter
around it, and in indentation-based syntax an undelimited list cannot wrap — a
bare name on a fresh line re-parses as a new statement. So a long import stays
long. The parenthesized form `from x import (a, b, c)` is ratified and retires
this exemption when it lands: parentheses suspend newline-significance the way
they do in an expression, and the name list then packs at 120 like every other
list.

### The trailing comma is yours to write

**Write a trailing comma after the last element and the list stays exploded** —
broken across lines, however comfortably it would have fitted. Leave it off and
the formatter packs as usual. It is the one layout decision the formatter hands
back to you, and you make it in the code rather than in a directive above it:

```gorget
Vector[String] verbs = ["get", "put", "post"]      # packs — no comma, no opinion

Vector[String] methods = [
    "get",
    "put",
    "post",                                        # …this comma keeps it open
]
```

Use it where the vertical form is the readable one: a table you will keep adding
rows to, a call whose arguments each deserve their own comment, a list whose order
is the point. Diffs get smaller too — adding a row touches one line instead of
rewrapping the paragraph.

This is honoured wherever a list can wrap: parameter lists, call and method-call
arguments, generic parameters and arguments, closure parameters, array, set,
tuple and dictionary literals, and grouped imports. It is not yet honoured in a
few positions that have no wrapping layer of their own — enum tuple-variant field
lists, patterns, tuple and function types, `with` bindings, attribute arguments —
where the comma is still dropped; those are being routed through the same
machinery.

The one place a trailing comma means something else is the **one-element tuple**:
`(x,)` is how the language spells a 1-tuple at all, so the comma there is syntax,
not a layout signal, and nothing is inferred from it.

A trailing comma is a signal, never a requirement: a list you never comma stays
packed forever, and both shapes are equally idiomatic.

### Your line grouping is yours too

The comma says the list stays open. It does not say where the breaks go — you do.
Inside an exploded list, the formatter keeps the lines you grouped the elements
into:

```gorget
Vector[String] mutators = [
    "push", "pop", "set", "insert", "remove", "clear", "sort", "sorted",
    "reverse", "swap", "swap_remove", "extend", "append", "truncate",
]
```

Twenty-five names on four lines is a decision, and one name per line would be
twenty-five lines of scrolling. So the pair of rules is: **the trailing comma says
keep this open; your own newlines say where to break.** Write every element on its
own line and that is what you get; group them into rows and the rows stay.

Three things are worth knowing about the edges.

**It preserves rows, not columns.** If you padded the elements into aligned
columns, the padding collapses to a single space after each comma — the rows
survive, the alignment does not. The formatter rewrites elements as it goes, so a
column you measured by hand would be wrong the moment one of them changed width.

**The brackets stay canonical.** The opening bracket ends its line, the closing one
gets its own line with the trailing comma before it, and the rows are indented one
level. Where you put the brackets is the formatter's to normalize; where you put the
elements is yours.

**A row you wrote is never re-wrapped**, even past the 120-column budget. The row is
the unit you chose, so the formatter will not second-guess it — if a row runs long,
that is your call to make and yours to change.

There is nothing to preserve in a list you wrote on one line, so `[1, 2, 3,]`
becomes the plain one-element-per-line shape. And grouping alone is not a request to
explode: without the trailing comma the list is packed as usual, rows and all.
Otherwise every list you ever saved would be frozen in whatever shape it happened to
have, and `gg fmt` would stop normalizing anything.

## Comments

A **trailing comment** stays attached to the line it annotates, four spaces
after the code:

```gorget
static int width = 80    # characters per line
```

Within a contiguous run of lines that *each* carry a trailing comment — struct
fields, enum variants, or a block of consecutive statements — the comments are
aligned to a common column so they read as a table. The column is the first
multiple of four that leaves at least that four-space gap after the longest line
in the run:

```gorget
struct Camera:
    Vec3 position       # world-space eye point
    Vec3 forward        # unit view direction
    float fov           # vertical field of view, radians
```

The run is broken by any line without a trailing comment (and by a blank line),
so each group aligns independently. A comment whose aligned column would push it
past the line-width budget is left at its natural position rather than dragging
the whole group to the right.

**A trailing comment may run onto the lines below it.** Keep each `#` at the
first one's column, with no blank line and no code line in between, and the
whole thing is ONE comment on the line it started on — which is how you write a
field's rationale without a 200-column line:

```gorget
struct Frame:
    int slot        # index of this frame's slot in the arena, assigned at
                    # push time and never reused while the frame is live
    int depth       # nesting depth from the root frame
```

The continuation lines move with their `#`: when the group above picks a column,
the whole comment follows it. They do not break the run either — `int depth`
still shares the group's column, because the comment above it is one comment,
not one comment and two stray lines. Change the column, leave a blank line, or
put a line of code in between, and you have written two separate comments
instead.

**Standalone comments** — a comment on its own line — keep their own line and
indentation and are never merged into the code around them. That includes a
comment written after the LAST thing in a block: it belongs to the block it was
indented into, and it stays there rather than sliding out to the next one.

## Literals

**Numeric spelling is preserved.** The formatter emits each numeric literal
exactly as written — the base, the digit case, the digit-group underscores, a
byte literal's character form, and a float's trailing zeros:

```gorget
static int mask = 0xFF_FF        # stays hexadecimal, underscore kept
static int perms = 0o755         # stays octal
static int flags = 0b1011        # stays binary
static int channels = 48000      # stays decimal
static uint8 newline = b'\n'     # stays a byte literal, not 10
static float gain = 1.50         # stays 1.50, not 1.5
```

`gg fmt` will never rewrite `0x5C` to `92`. The base you chose carries meaning —
a byte pattern, a permission mask, a bit set — and so does `b'\n'` over `10`,
and so does a column of constants whose trailing zeros line their decimal points
up. All of it is part of the source.

**String spelling is preserved too.** A string literal is re-emitted exactly as
you wrote it: the escape you chose stays that escape, the quotes you chose stay
those quotes, and a multi-line `"""` block keeps its physical lines. The
recognized escape sequences are `\n \t \r \0 \\ \" \' \{ \} \xHH \u{...}` and
`\uXXXX` (see the [language reference](../language-reference.md) for their exact
meaning and the `\xHH` ASCII-versus-byte rules); the formatter treats your
choice among them as part of the source.

```gorget
static String tab_escape = "\x41\tB"     # stays \x41 and \t — not "A<TAB>B"
static String emoji = "\u{1F600}"        # stays the escape you wrote
static String literal_emoji = "😀"        # …and this stays the character
static String apostrophes = 'don\'t'     # single quotes stay single quotes
```

That extends to the strings that are not expressions — a test or bench name, a
snapshot name, a string attribute argument, an `extern` ABI tag or symbol name.
They are quoted strings in your source, and they round-trip like any other.

The formatter re-encodes a string only when it has no author spelling to work
from, which happens for a literal the compiler synthesised rather than one you
typed. In that case each character is printed in canonical readable form:
printable characters as themselves, including non-ASCII Unicode; the familiar
control characters under their names `\n`, `\t`, `\r`, `\0`; and any other
control character as an escape rather than a raw byte, so a synthesised string
holding an ANSI sequence stays legible and re-parseable — `ESC` as `\x1b`, and
a C1 control such as U+0085 as `\u{85}`, since `\x` above `0x7F` is not a valid
escape.

## Formatting in CI

Because `gg fmt --check` exits non-zero on unformatted input and the formatter
is idempotent, a CI step can reject any file that is not already formatted. The
formatter checks one file per run, so walk the tree and fail on the first
offender:

```bash
for f in $(find src -name '*.gg'); do
    gg fmt --check "$f" || exit 1
done
```

A clean tree passes silently; the first unformatted file exits the step
non-zero.
