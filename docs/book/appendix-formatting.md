# Appendix E — Formatting and Code Style

Gorget ships a canonical source formatter, `gg fmt`. There is one correct
formatting for any program, and `gg fmt` produces it — no per-project style
configuration, no options to bikeshed. Running the formatter is how Gorget code
is meant to look; the standard library and the self-hosting compiler are both
kept formatted.

## Running the formatter

| Command | Effect |
| --- | --- |
| `gg fmt <file>` | Print the formatted source to stdout. |
| `gg fmt -i <file>` / `--in-place` | Rewrite the file in place. |
| `gg fmt -c <file>` / `--check` | Exit non-zero if the file is not already formatted (for CI). |

## What the formatter guarantees

- **Value-preserving.** Formatting never changes what a program means or what
  its literals evaluate to. The bytes of a string, the value of a number, and
  the structure of every expression survive untouched.
- **Idempotent.** `gg fmt` applied to already-formatted source is a no-op:
  `fmt(fmt(x)) == fmt(x)`. This is what makes `--check` reliable.
- **Canonical.** The output is a pure function of the parsed program and the
  line-width budget — it does not depend on how the input was spaced, so two
  authors who format the same program get byte-identical results.

## Layout

Blocks are indented four spaces per level, Python-style — indentation is the
block structure, and the formatter normalizes it.

The formatter preserves the blank lines you write as paragraph breaks, and
collapses any run of consecutive blank lines down to a single one. A blank line
keeps its meaning next to comments, too: a blank above or below a comment, or
between two comments, is a deliberate break and is kept.

Where the language accepts more than one spelling of a construct, the formatter
emits the canonical one — for example `elif` rather than `else if`.

## Comments

A **trailing comment** stays attached to the line it annotates:

```gorget
int width = 80  # characters per line
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

**Numeric radix is preserved.** The formatter emits each integer literal exactly
as written, including the base, digit case, and digit-group underscores. A hex
constant stays hex:

```gorget
int mask     = 0xFF_FF   # stays hexadecimal, underscore kept
int perms    = 0o755     # stays octal
int flags    = 0b1011    # stays binary
int channels = 48000     # stays decimal
```

`gg fmt` will never rewrite `0x5C` to `92`; the base you chose carries meaning
(a byte pattern, a permission mask, a bit set) and is part of the source.

**String escapes are kept readable.** The recognized escape sequences are
`\n \t \r \\ \0 \" \' \{ \} \xHH \u{...}` and `\uXXXX` (see the
[language reference](../language-reference.md) for their exact meaning and the
`\xHH` ASCII-versus-byte rules). Two formatting guarantees follow from working
at the level of escapes rather than raw bytes:

- A **control character** in a string is emitted as a `\xHH` escape, never as a
  raw control byte written into your source file. A string holding an ANSI
  sequence stays legible:

  ```gorget
  String reset = "\x1b[0m"   # the ESC stays an escape, not a raw 0x1B byte
  ```

- **Printable text is left exactly as written**, including non-ASCII Unicode —
  `"héllo 😀"` is never mangled into escape sequences.

## Formatting in CI

Because `gg fmt --check` exits non-zero on unformatted input and the formatter
is idempotent, a one-line check keeps a codebase uniformly formatted:

```bash
gg fmt --check src/*.gg
```
