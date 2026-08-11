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
> ever wanted, is `gg fix` territory. Width becomes 120 with fill-pack
> wrapping; the trailing-comment gap becomes 4. This appendix is rewritten by
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
