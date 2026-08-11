# Appendix E — Formatting and Code Style

Gorget ships a canonical source formatter, `gg fmt`. It settles every layout
question for you — indentation, wrapping, spacing, blank-line runs, comment
alignment — with no per-project style configuration and no options to bikeshed.
What it does *not* touch is the way you spelled the program: the formatter
changes the layout it owns, and nothing else. Running it is how Gorget code is
meant to look; the standard library and the self-hosting compiler are both kept
formatted.

> **Ruled, not shipped.** Two of this appendix's rulings are ratified and not
> yet implemented: the author's inline-vs-next-line suite choice is preserved
> SYMMETRICALLY; the width becomes 120 with fill-pack wrapping, and the
> trailing-comment gap becomes 4. Until then this appendix describes shipped
> behavior. Normative record: `docs/define-gorget/decisions.md`.

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
  versus `uint8`, `await x` versus `x.await()` — these are choices you made for
  a reader, and re-spelling them is not the formatter's job. What the formatter
  owns is layout: indentation, wrapping, spacing, blank-line runs, comment
  columns.
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
collapses any run of consecutive blank lines down to a single one. A blank line
keeps its meaning next to comments, too: a blank above or below a comment, or
between two comments, is a deliberate break and is kept.

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

The single exception is the pure keyword synonym `else if`, which carries no
information `elif` does not; it is canonicalized to `elif`.

## Comments

A **trailing comment** stays attached to the line it annotates:

```gorget
static int width = 80  # characters per line
```

Within a contiguous run of lines that *each* carry a trailing comment — struct
fields, enum variants, or a block of consecutive statements — the comments are
aligned to a common column so they read as a table:

```gorget
struct Camera:
    Vec3 position   # world-space eye point
    Vec3 forward    # unit view direction
    float fov       # vertical field of view, radians
```

The run is broken by any line without a trailing comment (and by a blank line),
so each group aligns independently. A comment whose aligned column would push it
past the line-width budget is left at its natural position rather than dragging
the whole group to the right.

**Standalone comments** — a comment on its own line — keep their own line and
indentation and are never merged into the code around them.

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
