# R39 snag #2 residual — `gg fmt` mis-attributes header-line trailing on MULTI-LINE container headers

Filed 2026-08-09 as a residual of the R39 snag #2 fix (commit
`3644cd4a6`). The main fix's `emit_trailing_comment_after_header` hook
correctly handles single-line container headers (`struct S:  # doc`,
`enum E[T]:  # doc`) but does NOT handle the case where the container
header itself spans multiple source lines, with the trailing comment
sitting on the LAST source line of the header — after the `:` on a line
that is NOT the container name's line.

## TL;DR

The header-hook's anchor is the container name's span end
(`s.name.span.end`). For a multi-line header the comment sits on a
LATER source line than the name, so the same-source-line check (no `\n`
between anchor and comment) correctly rejects it — the comment is then
picked up by the normal body loop's `emit_comments_before` and emitted
as a LEADING comment of the first body element.

Rare — the vast majority of `gorget-arena`-class trailing comments are
on SINGLE-LINE headers or on plain body statements, all covered by the
R39 fix. Filed here so a future round can extend the header-hook to
track the true header-end position (the `:` byte, not the name-end
byte) and cover the multi-line-header case as well.

## Reproducer

`repro.gg` — a struct with a multi-line generic-params header:

```gorget
struct S[
    T
]:  # multi-line header trailing
    T x
```

Actual `gg fmt` output (BUG — comment DEDENTED into body):

```gorget
struct S[T]:
    # multi-line header trailing
    T x
```

Intended: `struct S[T]:  # multi-line header trailing\n    T x`
(single-line reformat, comment stays on the header line).

## Fix direction

The header-hook needs the position of the `:` token that closes the
container header (not the name identifier's end). That position is
tracked by the parser but not currently exposed on the container's AST
node — adding a `header_colon_span: Span` field would let the hook
use it directly. Alternative: walk backwards from the FIRST body
element's `span.start` to find the immediately-preceding `:` byte in
source. Both are ~10 LOC + wiring the four container formatters.

## Repro environment

- gorget HEAD `3644cd4a6` (R39 snag #2 fix commit).
- Not exercised by `fmt_idempotent` (idempotent on the wrong shape).
- Not exercised by `fmt_trailing_comment_axis_all_classes` (axis fixture
  uses single-line headers).
