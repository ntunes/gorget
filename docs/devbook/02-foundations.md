# 02 — Foundations: spans, interning & diagnostics

This chapter covers the three cross-layer primitives every other subsystem
builds on: the byte-offset **Span** model (`src/span.rs`), the global
**Symbol** interner (`src/intern.rs`), and the **diagnostic / error
reporting** infrastructure (`src/errors.rs`, `src/semantic/errors.rs`,
`ErrorReporter` in `src/errors.rs`). None of these is a "pass" — they are
the shared vocabulary the lexer, parser, semantic analysis, IR lowering, and
both backends all speak. Spans are attached at lex time and survive,
unchanged in meaning, all the way down to runtime panic messages emitted by
codegen. Symbols are interned at lex time and used as cheap identity handles.
Diagnostics are the single rendering path from any layer's typed error to
stderr.

## The Span model

A `Span` is a half-open-ish byte range into source: two `usize` fields,
`start` and `end`, both *global* byte offsets (see
[Global offsets and `base_offset`](#global-offsets-and-base_offset) below).
It is `Copy` and derives `Hash`/`Eq` so it can be a map key
(`src/span.rs:1-6`).

```rust
pub struct Span { pub start: usize, pub end: usize }
```

`Span::new` `debug_assert!`s `start <= end` — an inverted span is a bug, not
a tolerated state (`src/span.rs:9-12`). The only other constructors are
`merge` (the smallest span covering two — `min(start)`/`max(end)`,
`src/span.rs:14-19`) and `dummy` (the `0..0` sentinel for synthesized nodes
with no source location, `src/span.rs:21-23`).

### `Spanned<T>`

The generic carrier `Spanned<T> { node: T, span: Span }` (`src/span.rs:149-154`)
is how a value is annotated with its origin. It provides `new`, `dummy`
(wraps a node with `Span::dummy()`), and `map` (transform the node, keep the
span — `src/span.rs:156-174`). This is the wrapper the lexer emits
(`Spanned<Token>`) and the parser threads through. Note the two distinct
conventions in the tree:

- **`Spanned<T>` wrapper** — used where a value is logically spanless but
  needs a location glued on (tokens, and expression nodes carried as
  `Spanned<Expr>` throughout the AST — e.g. `src/parser/ast.rs:497`, `:512`,
  `:517`, ~100 occurrences). Used pervasively across the IR lowering and
  parser layers (grep `Spanned` across `src/ir/lowering/*`, `src/parser/*`).
- **A bare `span: Span` field** — most AST/semantic struct definitions
  embed the span directly as a field rather than wrapping the whole node
  (e.g. `LexError`, `ParseError`, `SemanticError` all carry `pub span: Span`
  — `src/errors.rs:11`, `src/errors.rs:67`, `src/semantic/errors.rs:7`).

Both patterns coexist by design; neither is being migrated away.

### Global offsets and `base_offset`

Gorget compiles multi-module programs by conceptually concatenating every
loaded file into one global byte space. Each module is assigned a
`base_offset` and every span it produces is shifted by that base, so a span
is globally unique across the whole program without needing to carry a file
id.

The loader assigns the bases sequentially. `next_offset` starts at 0; each
file gets `module_base_offset = next_offset`, then `next_offset` advances by
`source.len() + 1` (`src/loader.rs:810-811`). The `+ 1` leaves a one-byte gap
between modules so no real span of file N can ever exactly abut the first
byte of file N+1 (offsets stay strictly disjoint). The base is passed into
`Parser::new_with_offset` (`src/loader.rs:812`), which forwards it to
`Lexer::new_with_offset`.

The lexer applies the base in exactly one place: its private `span()` helper
adds `base_offset` to both ends (`src/lexer/mod.rs:88-90`).

```rust
fn span(&self, start: usize, end: usize) -> Span {
    Span::new(self.base_offset + start, self.base_offset + end)
}
```

Every token span — keywords, identifiers, INDENT/DEDENT/EOF sentinels,
f-string interpolation segments — flows through this one function
(`src/lexer/mod.rs:168`, `:286`, `:332-334`, …), so the global-offset
invariant is established at a single chokepoint and is true for the rest of
the pipeline.

### Synthetic spans above `1 << 40`

The parser reserves a high region of the offset space for *synthetic* spans
that have no byte in any real file. The f-string interpolation sub-parser
allocates spans starting at `1 << 40`, plus a per-module shift of
`base_offset << 20`, so each module's synthetic range is disjoint
(`src/parser/mod.rs:64-72`):

```rust
let interp_base = (1usize << 40).wrapping_add(base_offset.wrapping_shl(20));
```

The comment there records *why* the shift exists: without it every module's
first interp token would share span `1 << 40`, colliding in the resolver's
`resolution_map[span_start]` last-write-wins, and `lower_call` would emit the
wrong mangled symbol (`src/parser/mod.rs:65-71`). The takeaway for anyone
reading spans downstream: a `span.start >= 1 << 40` is synthetic and will not
resolve to a `(file, line, col)` — see the next section's behavior on
out-of-range offsets.

### `FileInfo` and offset → `(file, line, col)`

The reverse map — turning a global offset back into a human location — lives
in `FileInfo` and `offset_to_location` (`src/span.rs:43-147`). A `FileInfo`
holds the `filename`, the `source`, the `base_offset`, a precomputed
`line_starts` index, and a precomputed `filename_c_escaped`
(`src/span.rs:43-50`).

`FileInfo::new` walks the source once, pushing the byte offset after every
`\n` into `line_starts` (which always begins with `0`), and pre-escapes the
filename for C string literals (`src/span.rs:56-66`). Both are hot-path
optimizations explicitly documented in the doc comment: the `line_starts`
index makes offset→line an O(log n) binary search instead of a linear walk
over source bytes on every panic-emit at codegen, and the pre-escaped
filename avoids re-escaping the same name per inst (the comment cites a
measured ~5ms per 100k insts on `self_host_lowerer`, `src/span.rs:34-42`).

`offset_to_location_full` is the workhorse (`src/span.rs:120-147`): it
linear-scans the file list (small N, typically <50 files) to find the
containing file — preferring the one with the *largest* `base_offset <=`
the query so the one-byte gaps resolve to the right side
(`src/span.rs:124-133`) — then binary-searches `line_starts` for the line and
computes a 1-based `(line, col)` (`src/span.rs:139-146`). An offset inside no
known file (including the synthetic `>= 1 << 40` region) returns `None`
(`src/span.rs:133`, tested at `src/span.rs:207-217`). `offset_to_location`
(`src/span.rs:107-114`) is the thin wrapper returning just `(&str, u32, u32)`;
the `_full` variant exists so codegen can grab the pre-baked
`filename_c_escaped` without re-running the lookup (`src/span.rs:116-119`).

This is the function both backends call to attach source locations to runtime
panics: the LLVM backend via `offset_to_location` (`src/backend/llvm/mod.rs:508`)
and the C backend via `offset_to_location_full` (`src/backend/c_lir/mod.rs:113`).

Note `escape_for_c_string` (`src/span.rs:73-99`) deliberately lives in
`span.rs`, duplicating the logic of
`src/backend/c_lir/helpers.rs::escape_c_string`, specifically so `FileInfo`
can pre-bake the escaped name without pulling the backend into the dependency
graph of the IR/semantic layers (`src/span.rs:69-72`). This is a deliberate
layering trade-off — the comment documents it as such.

## Symbol interning

`Symbol` is a `u32` newtype handle for an interned string
(`src/intern.rs:11-13`). It is `Copy`, `Eq`, `Hash`, and `Ord`, so identifier
comparison is a `u32` compare and identifiers can be map keys or sorted
cheaply. `Symbol::as_str()` recovers the original text
(`src/intern.rs:24-31`).

### The interner

The interner is a thread-local `RefCell<Interner>` (`src/intern.rs:97-99`).
`Interner` holds two structures (`src/intern.rs:72-75`):

- `map: FxHashMap<String, Symbol>` — dedup index, text → existing symbol.
- `strings: Vec<String>` — symbol id (the `u32`) → text, by index.

`intern(s)` returns the existing symbol on a hit, otherwise allocates the
next id (`strings.len()`), pushes the text, and inserts the reverse mapping
(`src/intern.rs:85-94`). The public free function `intern` just borrows the
thread-local and delegates (`src/intern.rs:102-104`). It uses `rustc_hash`'s
`FxHashMap` rather than the default SipHash map — the same fast,
non-DoS-resistant hasher used throughout the compiler.

### The `as_str` lifetime trick

`as_str` returns a `&str` by transmuting away the `RefCell` borrow's
lifetime with an `unsafe` cast (`src/intern.rs:28-29`). The safety
justification (`src/intern.rs:18-23`) is: the `strings` `Vec` only ever
**grows** — entries are never removed or mutated — so each `String`'s heap
buffer is stable across `Vec` reallocations (the `Vec` reallocates its array
of `(ptr, len, cap)` tuples, but the pointed-to character data does not
move), and the thread-local interner lives for the thread's whole duration.
The returned reference is therefore valid for any practical lifetime. This is
the load-bearing invariant: **do not add a remove/clear/mutate path to the
interner** — it would invalidate every outstanding `Symbol::as_str`.

### `Display`, `Debug`, and `str` comparisons

`Symbol` implements `Display` (writes the resolved string,
`src/intern.rs:40-44`) and `Debug` (`Symbol(<id>: "<text>")`,
`src/intern.rs:34-38`). For ergonomic comparison against literals it
implements `PartialEq` in all four directions between `Symbol` and `str`/`&str`
(`src/intern.rs:46-68`), so `sym == "foo"` and `"foo" == sym` both compile and
compare resolved text (tested at `src/intern.rs:122-129`).

### Where Symbol is actually used

Despite "all identifiers pass through the interner" framing in the module
doc, `Symbol` is currently referenced in exactly one place outside the
interner itself: the lexer's `Token::Identifier(Symbol)` variant
(`src/lexer/token.rs:1`, `src/lexer/token.rs:558`), produced by the one
`intern` call in the lexer when it emits an identifier token
(`src/lexer/mod.rs:413`):

```rust
Token::Identifier(crate::intern::intern(slice))
```

In other words: interning happens at lex time on identifier text, and the
`Symbol` rides inside the token stream. Downstream layers that compare or
store identifier text largely do so as `String`/`&str` (the AST and semantic
structs use `String` fields — see the `name: String` fields throughout
`src/semantic/errors.rs`). The interner is therefore a lexer-level
optimization today rather than a fully-threaded compiler-wide identity
mechanism; if you are extending it, that is the current scope of its reach.

## Diagnostics & error reporting

Errors are *typed per layer* and *rendered centrally*. Each frontend layer
defines its own error enum (kind + span); a single `ErrorReporter` turns any
of them into a `codespan-reporting` diagnostic on stderr. The split keeps
each layer's error vocabulary precise while there is exactly one place that
knows how to draw an underlined source label.

### Per-layer error types

- **Lex** (`src/errors.rs:7-45`): `LexError { kind: LexErrorKind, span }`.
  `LexErrorKind` enumerates the lexer's failure modes —
  `UnterminatedString`, `InvalidEscapeSequence(String)`,
  `IndentationMismatch { got }`, `TabCharacter`, etc.
  (`src/errors.rs:14-26`) — with `Display` producing the human message
  (`src/errors.rs:28-45`).
- **Parse** (`src/errors.rs:63-104`): `ParseError { kind: ParseErrorKind, span }`.
  `ParseErrorKind` covers `UnexpectedToken { expected, got }`,
  `UnexpectedEof`, `ExpectedBlock`, `InvalidAssignmentTarget`, etc.
  (`src/errors.rs:70-84`). There is also a `ParseWarning` type whose kind enum
  is currently **empty** (`ParseWarningKind {}`, `src/errors.rs:47-61`) — the
  plumbing exists (`report_parse_warning`, `src/errors.rs:191-196`) but no
  parse warning is emitted yet.
- **Semantic** (`src/semantic/errors.rs`): `SemanticError { kind, span }` and
  `SemanticWarning { kind, span }` (`src/semantic/errors.rs:3-15`).
  `SemanticErrorKind` is the large one — on the order of 80 variants
  (`src/semantic/errors.rs:179` onward), from `UndefinedName { name, suggestion }`
  and `TypeMismatch { expected, found }` to ownership/borrow errors like
  `UseAfterMove`, `DoubleMove`, `DanglingReturn`, `TemporaryBorrow`.
  `SemanticWarningKind` (`src/semantic/errors.rs:17-105`) carries the
  concurrency-lint family (`StaleSharedCondition`, `WithCheckThenAct`,
  `SharedIteratorInvalidation`, …) plus the general lints (`UnusedVariable`,
  `UnusedImport`, `UncheckedUnwrap`, `CouldBeConst`, `NeedlessMutableBorrow`,
  `SuggestThrowsRefactor`). Many of these kinds carry *additional* spans
  beyond the primary one (e.g. `DuplicateDefinition { original: Span }`,
  `StaleSharedCondition { derivation_span, await_span }`) — those secondary
  spans drive the multi-label rendering below.

### `ErrorReporter`

`ErrorReporter` (`src/errors.rs:106-388`) wraps a
`codespan_reporting::files::SimpleFiles<String, String>` plus the bookkeeping
to map global spans back to file-local positions. It has two constructors:

- `new(filename, source)` — single file, `file_ranges` empty
  (`src/errors.rs:116-120`).
- `new_multi(file_infos)` — takes `Vec<(filename, source, base_offset)>` (the
  same shape the loader produces), adds each file to `SimpleFiles`, records
  `(base_offset, len, file_id)` in `file_ranges`, and sorts by `base_offset`
  for binary search (`src/errors.rs:126-141`).

The core of multi-file support is `resolve_offset` (`src/errors.rs:143-155`):
given a global offset, binary-search `file_ranges` for the last range whose
`base_offset <= global`, then subtract the base to get the file-local offset
and the matching `file_id`. This is the `ErrorReporter`'s parallel to
`span.rs`'s `offset_to_location` — same global-offset model, different
backing store (`SimpleFiles` vs `FileInfo`). `primary_label` /
`secondary_label` (`src/errors.rs:163-175`) build `codespan` labels by
resolving the start offset and re-deriving the local end as
`local_start + (span.end - span.start)`.

### Rendering: primary + secondary labels + notes

Each `report_*` method builds a `Diagnostic` and calls the private `emit`
(`src/errors.rs:383-387`), which writes to stderr through a
`StandardStream` with `ColorChoice::Auto`. The lex/parse reporters are
single-label (`src/errors.rs:177-196`). The semantic reporters are where the
secondary-span metadata pays off:

- `report_semantic_error` (`src/errors.rs:198-252`) starts with one primary
  label, then `match`es on the error kind to attach **secondary labels**
  pointing at the related location — e.g. `DuplicateDefinition` underlines
  `original` with "originally defined here", `UseAfterMove` underlines
  `moved_at` with "value moved here", `DanglingReturn` underlines the
  local's declaration site (`src/errors.rs:204-246`).
- `report_semantic_warning` (`src/errors.rs:254-359`) does the same for the
  concurrency lints, and additionally attaches **notes** (the "help:" lines)
  — e.g. the `StaleSharedCondition` warning gets both a derivation-site and
  an await-site secondary label plus a note suggesting `with` or a re-read
  (`src/errors.rs:260-276`), and `UnusedVariable` gets a note suggesting the
  `_` prefix (`src/errors.rs:332-334`).

This is the concrete payoff of carrying multiple spans on a single error
kind: one diagnostic can underline several source regions and explain the
relationship between them.

### Structured location & the fatal/non-fatal split

`span_location` (`src/errors.rs:362-371`) resolves a span to
`(filename, line, column)` via `codespan`'s `Files::location`, for callers
that want structured output rather than a rendered diagnostic.
`is_entry_file` (`src/errors.rs:157-161`) reports whether a span belongs to
file id 0 (the user's source) — used to suppress diagnostics that originate
in imported/library modules.

Whether a diagnostic is *fatal* is decided by the driver, not the reporter.
In `main.rs` the pattern is uniform: collect the layer's errors, report each,
and if the list is non-empty return an `Err(format!("{n} ... error(s) found"))`
that aborts the command (parse errors at `src/main.rs:425-430`, semantic
errors at `src/main.rs:449-454`). Warnings are reported and execution
continues. `ErrorReporter` itself is constructed with `new` for single-file
commands and `new_multi` for the loaded multi-module case (e.g.
`src/main.rs:426`, `src/main.rs:450`).

## How it all fits together

1. The **loader** assigns each module a `base_offset` and advances by
   `len + 1` (`src/loader.rs:810-811`).
2. The **lexer** interns identifier text into a `Symbol` and stamps every
   token with a global span via the single `span()` chokepoint
   (`src/lexer/mod.rs:88-90`, `:413`).
3. Spans ride, unchanged in meaning, through parser → semantic → IR → LIR.
   Synthesized nodes use `Span::dummy()`; the f-string sub-parser uses the
   `>= 1 << 40` synthetic region.
4. Any layer that fails produces a typed `{Lex,Parse,Semantic}Error` carrying
   the offending span (and any related spans).
5. The driver hands those to an `ErrorReporter` (`new` or `new_multi`), which
   uses `resolve_offset` to map global → file-local and renders an underlined,
   labelled, optionally-noted diagnostic to stderr.
6. Spans that survive to codegen become runtime panic locations via
   `offset_to_location[_full]` against the `FileInfo` table
   (`src/backend/c_lir/mod.rs:113`, `src/backend/llvm/mod.rs:508`).

Two independent reverse maps exist for global-offset → location:
`FileInfo`/`offset_to_location` (`src/span.rs`, used by codegen) and
`SimpleFiles`/`resolve_offset` (`src/errors.rs`, used by the reporter). They
implement the same model with different backing stores; both are kept in sync
with the loader's `base_offset` assignment.
