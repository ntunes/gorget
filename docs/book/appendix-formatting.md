# Appendix E — Formatting and Code Style

Gorget ships a canonical source formatter, `gg fmt`. There is one correct
formatting for any program, and `gg fmt` produces it — no per-project style
configuration, no options to bikeshed. Running the formatter is how Gorget code
is meant to look; the standard library and the self-hosting compiler are both
kept formatted.

> **Ruled, not shipped — the 2026-08-11 form-preservation rulings.** This
> appendix's doctrine is ratified to change: **`gg fmt` changes layout it owns
> and nothing the author spelled.** Literal and escape forms are preserved
> VERBATIM (the "canonical, not verbatim" escaping section below is superseded);
> the author's inline-vs-next-line suite choice is preserved SYMMETRICALLY;
> synonym spellings (`byte`/`uint8`, set-literal/vector, `await` forms,
> `public`, …) are never rewritten by `gg fmt` — construct canonicalization, if
> ever wanted, is `gg fix` territory. This appendix is rewritten by
> the R41 formatter-hardening tracks as their write-through; until then it
> describes shipped behavior. Normative record:
> `docs/define-gorget/decisions.md` (the FMT FORM-PRESERVATION TRIO entry and
> the three 2026-08-10 fmt entries).

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
- **Idempotent.** `gg fmt` applied to already-formatted source is a no-op:
  `fmt(fmt(x)) == fmt(x)`. This is what makes `--check` reliable.
- **Canonical.** The output is a pure function of the parsed program, the
  line-width budget, and the handful of choices the language leaves to the
  author (below). It does not depend on incidental spacing, so two authors who
  format the same program get byte-identical results.

## Layout

Blocks are indented four spaces per level, Python-style — indentation is the
block structure, and the formatter normalizes it.

The formatter preserves the blank lines you write as paragraph breaks, and
collapses any run of consecutive blank lines down to a single one. A blank line
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

Where the language accepts more than one spelling of a construct, the formatter
emits the canonical one — for example `elif` rather than `else if`.

### Line width and wrapping

Lines are budgeted at **120 columns** — counted in characters, so accented
letters and emoji cost one column each, not one per byte.

A list that fits stays on one line. A list that doesn't is **packed**: the
formatter fills each line up to the budget and then wraps, rather than exploding
every element onto a line of its own. Continuation lines are indented one level
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
the last element, so there is nowhere for one to sit. The exception is a list
whose elements carry comments: comments belong to the lines they annotate, so
such a list falls back to one element per line, *with* a trailing comma and a
closing bracket on its own line. Those two shapes are the whole vocabulary; you
never have to choose between them.

One element can be wider than the whole budget — a long qualified name, a deeply
generic type. The formatter puts it on its own continuation line and lets it
overrun rather than breaking it somewhere meaningless. That is the only case
where a formatted line exceeds 120 columns.

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

**Standalone comments** — a comment on its own line — keep their own line and
indentation and are never merged into the code around them.

## Literals

**Numeric radix is preserved.** The formatter emits each integer literal exactly
as written, including the base, digit case, and digit-group underscores. A hex
constant stays hex:

```gorget
static int mask = 0xFF_FF       # stays hexadecimal, underscore kept
static int perms = 0o755        # stays octal
static int flags = 0b1011       # stays binary
static int channels = 48000     # stays decimal
```

`gg fmt` will never rewrite `0x5C` to `92`; the base you chose carries meaning
(a byte pattern, a permission mask, a bit set) and is part of the source.

**String escaping is canonical, not verbatim.** Unlike a numeric literal — whose
exact spelling is kept — a string is re-emitted from its *value*, so the precise
way you wrote an escape is not preserved, only the character it denotes. The
formatter decodes the string and prints each character in canonical readable
form, which is why two strings with the same value always format identically. The
recognized escape sequences are `\n \t \r \0 \\ \" \' \{ \} \xHH \u{...}` and
`\uXXXX` (see the [language reference](../language-reference.md) for their exact
meaning and the `\xHH` ASCII-versus-byte rules). Canonical form means:

- **Printable characters print as themselves**, including non-ASCII Unicode.
  `"héllo 😀"` is left exactly as written, and an escape that spells a printable
  character is decoded back to it — `"\u{1F600}"` formats as `"😀"` and `"\x41"`
  as `"A"`.

- **An ASCII control character prints as an escape, never as a raw byte in your
  source file.** The familiar ones keep their names — `\n`, `\t`, `\r`, `\0` —
  and the rest use `\xHH`. Every other character is emitted as itself, so a
  string holding an ANSI sequence stays legible — its `ESC` becomes `\x1b`, not a
  raw control byte:

  ```gorget
  static String reset = "\x1b[0m"  # the ESC stays an escape, not a raw 0x1B byte
  ```

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
