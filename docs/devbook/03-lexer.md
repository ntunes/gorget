# Chapter 3 — The lexer & indentation

*Verified against `ffd58b65`.*

The lexer turns Gorget source text into a flat stream of `Spanned<Token>` for the parser. It lives in `src/lexer/`: `token.rs` defines the token vocabulary (the logos-generated `RawToken`, the `Keyword` enum, and the final `Token`), and `mod.rs` is the indentation-aware driver that wraps logos and synthesizes the `Indent`/`Dedent`/`Newline` tokens a Python-style grammar needs. The output is consumed by `Parser::new_with_offset`, which drains the whole `Lexer` into parallel `kinds`/`spans` arrays before parsing begins (`src/parser/mod.rs:73-92`).

The non-obvious work is *not* the token recognition — logos does that declaratively. It is (a) the line-oriented state machine that decides where `Indent`/`Dedent`/`Newline` go, (b) the hand-written string/char scanners that logos never sees, and (c) f-string interpolation segmentation. Those three are the bulk of this chapter.

## Two layers: logos `RawToken` vs. the emitted `Token`

There are two distinct token types and the distinction is load-bearing.

`RawToken` (`src/lexer/token.rs:7-141`) is the `#[derive(Logos)]` enum. It covers numeric literals, every operator and delimiter, a bare `\n`, and a single catch-all `Identifier` regex (`src/lexer/token.rs:139`). It deliberately does **not** know about keywords, strings, chars, comments, or indentation — `#[logos(skip r"[ \t]+")]` (`src/lexer/token.rs:8`) throws away inline whitespace, and the driver feeds logos only the non-string, non-comment *interior* of a line.

`Token` (`src/lexer/token.rs:554-640`) is the public vocabulary the parser sees. It is a superset of `RawToken`: it adds resolved keywords (`Token::Keyword(Keyword)`), parsed literal payloads (`IntLiteral(i64)`, `FloatLiteral(f64)`, `StringLiteral(StringLiteral)`), the structural tokens `Indent`/`Dedent`/`Newline`, `Comment`/`DocComment`, `Eof`, and a recovery `Error`. The conversion from one to the other is `convert_raw_token` (`src/lexer/mod.rs:388-474`), a near-mechanical 1:1 map — the only non-trivial cases are the literal parsers and keyword promotion.

Keyword promotion happens *here*, not in logos: a `RawToken::Identifier` slice is run through `Keyword::from_str` (`src/lexer/token.rs:455-544`), and on a hit becomes `Token::Keyword(kw)` rather than `Token::Identifier(sym)` (`src/lexer/mod.rs:407-415`). This is why adding a keyword is a one-line change to `from_str` (plus `as_name` for round-tripping) — there is no separate lexer rule. `RawToken` has **no keyword `#[token]` rules at all**; keywords exist only as a post-conversion lookup. Two consequences worth knowing:

- `byte` is *not* its own keyword; `from_str` aliases it straight to `Keyword::Uint8` (`src/lexer/token.rs:471`).
- `None`/`Some`/`Ok`/`Error` are deliberately **not** keywords — they lex as plain identifiers and are registered as prelude variants in `semantic::resolve`, so `Error` can also be used as a trait name (`src/lexer/token.rs:268-272`, `505-507`).

Identifiers are interned at conversion time: `Token::Identifier(crate::intern::intern(slice))` stores a `Symbol`, not a `String` (`src/lexer/mod.rs:413`; interning is covered in Chapter 2).

### Longest-match ordering in the logos grammar

The operator tokens are ordered longest-first in the enum (`src/lexer/token.rs:26-80`) — `..=` before `..`, `+%=` before `+%` before `+`, `<<=` before `<<` before `<`. Logos resolves overlapping `#[token]` rules by maximal munch, so the source order is a readability aid rather than a correctness requirement, but it documents the intent. The identifier regex carries `priority = 1` (`src/lexer/token.rs:139`), a *low* priority in logos. Because `RawToken` has no keyword rules, this priority has nothing keyword-related to win against; its effect is to lose to the exact `#[token]` rules on a length tie — most visibly the bare `_`, which on the length tie resolves to `Underscore` (`#[token("_")]`, `src/lexer/token.rs:131-132`, default priority 2) rather than the identifier regex. (Both `Underscore` and `Identifier` are then handled together in `convert_raw_token`, `src/lexer/mod.rs:407`.) Numeric literals are split into `HexLiteral`/`OctalLiteral`/`BinaryLiteral`/`FloatLiteral`/`IntLiteral` regexes (`src/lexer/token.rs:11-24`), each allowing `_` digit separators which `parse_int_literal` strips before `i64::from_str_radix` (`src/lexer/mod.rs:373-374`).

## The line-oriented driver

`Lexer` is an `Iterator<Item = Spanned<Token>>` (`src/lexer/mod.rs:888-917`). Its state (`src/lexer/mod.rs:14-32`) is small but every field earns its place:

- `pos` — current byte offset into `source`.
- `base_offset` — added to every span so multiple modules occupy disjoint span ranges (`span()` at `src/lexer/mod.rs:89-91`; see Chapter 22 on module loading).
- `indent_stack: Vec<usize>` — the stack of active indentation widths in *spaces*, seeded with `[0]` (`src/lexer/mod.rs:79`).
- `bracket_depth: usize` — nesting depth of `(`/`[`/`{`; when `> 0`, indentation is ignored entirely.
- `pending: VecDeque<Spanned<Token>>` — a buffer, because one source line can produce several synthetic tokens (a `Newline`, one or more `Dedent`s) *before* the line's own content.
- `need_newline: bool` — whether a `Newline` should precede the next logical line.
- `finished` — guards EOF flushing.
- `errors: Vec<LexError>` — collected, never fatal; the lexer recovers and keeps going.

`next()` is a buffer-drainer: pop from `pending` if non-empty, otherwise call `scan_next_line()` to refill, then pop (`src/lexer/mod.rs:891-916`). A `Token::Eof` in the buffer is converted to `None` (`src/lexer/mod.rs:894`, `909`) — so the iterator *ends* at EOF; it does not yield an `Eof` token. (The parser re-appends its own `Eof` sentinel afterward, `src/parser/mod.rs:90-92`.)

### `scan_next_line`: the indentation state machine

This is the heart of the chapter (`src/lexer/mod.rs:105-157`). Per logical line:

1. Count leading spaces with `count_leading_spaces` (`src/lexer/mod.rs:160-176`). A **tab** in the indentation is an error (`LexErrorKind::TabCharacter`) but is consumed and counted as one position so lexing continues (`src/lexer/mod.rs:165-171`) — Gorget indentation is spaces-only.
2. Look at the first non-space character (`src/lexer/mod.rs:123`):
   - **EOF** → `emit_eof()`.
   - **`\n`** (blank line) → skip; blank lines never produce tokens or affect indentation (`src/lexer/mod.rs:129-133`).
   - **`#`** (comment-only line) → emit a `Comment`/`DocComment` token and loop; comment lines also don't affect indentation (`src/lexer/mod.rs:134-140`). Doc comments are distinguished by a `#/` prefix (`src/lexer/mod.rs:136-137`).
   - **`.` while `bracket_depth == 0 && need_newline`** → leading-dot continuation (see below).
   - **anything else** → if `bracket_depth == 0`, run `process_indentation`; then tokenize the line content (`src/lexer/mod.rs:147-153`).

Only non-blank, non-comment lines reach `process_indentation` — and only when not inside brackets. That is the whole rule for *which* lines indentation cares about.

### `process_indentation`: emitting INDENT / DEDENT

`process_indentation(spaces, line_start)` (`src/lexer/mod.rs:189-221`) compares `spaces` to the top of `indent_stack`:

- **Equal** → emit a pending `Newline` if one is owed (`emit_pending_newline`, `src/lexer/mod.rs:179-186`); the line continues at the same block level.
- **Greater** → push `spaces` and emit one `Indent`. *Any* positive increase opens a block — Gorget does not require a fixed indent unit (Python-style; `src/lexer/mod.rs:195-202`).
- **Less** → pop the stack and emit one `Dedent` per popped level until the top is `<= spaces` (`src/lexer/mod.rs:203-212`). If after popping the top does not *equal* `spaces`, that is an `IndentationMismatch` error (a dedent that doesn't line up with any enclosing block, `src/lexer/mod.rs:213-218`).

After processing, `need_newline = true` is set unconditionally (`src/lexer/mod.rs:220`): once we've emitted real content on a line, the *next* logical line owes a `Newline`. The pending-`Newline` is emitted *before* the `Indent`/`Dedent` for the new line (note the ordering in all three branches), which is what gives the parser the canonical `… Colon Newline Indent …` shape it pattern-matches on (`src/parser/mod.rs:108-113`).

Span widths encode the columns: `Indent`'s span is `[line_start, line_start + spaces]`, and each `Dedent` carries the *new* (smaller) indent width as its span (`src/lexer/mod.rs:199-211`). `Newline` spans are zero-width points at the line start (`src/lexer/mod.rs:181-184`).

### EOF flushing

`emit_eof` (`src/lexer/mod.rs:224-250`) is where dangling structure is closed: it emits a final `Newline` if one is owed, then pops the indent stack all the way back to the sentinel `[0]`, emitting a `Dedent` for each popped level, and finally pushes `Eof`. The `finished` flag makes it idempotent (`src/lexer/mod.rs:225-228`). This guarantees the token stream is balanced — every `Indent` has a matching `Dedent` even if the file ends mid-block.

### Brackets suppress layout

While `bracket_depth > 0`, `scan_next_line` skips `process_indentation` entirely (`src/lexer/mod.rs:149`), so newlines and indentation inside `(…)`, `[…]`, `{…}` produce *no* `Newline`/`Indent`/`Dedent`. The depth is maintained in `tokenize_line_content` as the only place that sees the actual bracket tokens: each `LParen`/`LBracket`/`LBrace` increments and each closer decrements (saturating at 0) right after `convert_raw_token` produces the token (`src/lexer/mod.rs:338-346`). This lets a list or call argument list span lines freely (`tests` `test_brackets_suppress_newlines`, `test_multiline_brackets`, `src/lexer/mod.rs:1047-1062`, `1460-1476`).

### Leading-dot continuation

A line whose first non-space character is `.` (outside brackets, when a `Newline` would otherwise be owed) is treated as a continuation of the previous expression: no `Newline`, no indentation change — the dot and the rest of the line are tokenized as if they followed the previous line directly (`src/lexer/mod.rs:141-146`). This is what makes fluent method chains work across lines:

```gorget
x = foo
    .bar()
    .baz()
```

lexes with no intervening `Newline`/`Indent` (`tests` `test_leading_dot_continuation`, `src/lexer/mod.rs:1289-1309`).

## Hand-written scanners: strings, chars, bytes

`tokenize_line_content` (`src/lexer/mod.rs:255-370`) scans a line character by character. Most of the line is *not* hand-scanned: the driver finds the next string/char/comment boundary and hands the contiguous run in between to a fresh `RawToken::lexer(segment)` (`src/lexer/mod.rs:316-360`). But anything that logos can't handle losslessly is scanned by hand and never reaches logos:

- **`#`** → consume to end of line, emit `Comment`, stop (`src/lexer/mod.rs:270-281`).
- **`"`** → `scan_string_literal` (`src/lexer/mod.rs:284-289`).
- **`r"` `b"` `c"` `f"` `f'` `b'`** → prefixed literal; `string_prefix_kind` (`src/lexer/mod.rs:54-62`) classifies the two-byte lookahead and routes to `scan_string_literal` or, for `b'`, `scan_byte_literal` (`src/lexer/mod.rs:292-304`).
- **`'`** → `scan_char_literal` (`src/lexer/mod.rs:307-312`).

The reason these can't go through logos is that they all carry *structured payloads* (escape-decoded text, interpolation segments, a decoded byte value) that a regex match can't produce. Keeping them out of logos also means the catch-all segment fed to logos is guaranteed free of quotes and `#` (the boundary scan at `src/lexer/mod.rs:318-323` stops at any of `"`, `'`, `#`, `\n`, or a string prefix via `is_string_prefix`).

### `scan_string_literal` and `StringKind`

`scan_string_literal` (`src/lexer/mod.rs:573-741`) handles all double-quoted and prefixed forms and produces `Token::StringLiteral(StringLiteral { kind, segments })`. The `StringKind` (`src/lexer/token.rs:744-752`) has six variants: `Normal`, `Format` (`f"…"`/`f'…'`), `Raw` (`r"…"`), `MultiLine` (triple-quoted), `Byte` (`b"…"`), and `CStr` (`c"…"`). The prefix byte is consumed first (`src/lexer/mod.rs:577-592`); a `"""` after a Normal prefix re-classifies to `MultiLine` (`src/lexer/mod.rs:600-609`) — only the Normal prefix re-classifies, so an `f"""…"""` keeps `StringKind::Format` while still consuming the triple terminator.

Three behaviors differ by kind:

- **Escapes** are decoded for every kind *except* `Raw` (`src/lexer/mod.rs:652`), via `parse_escape` (`src/lexer/mod.rs:478-570`) — `\n \t \r \\ \0`, the context-gated `\"` `\{` `\}`, plus two Unicode escape forms. `\u{…}` (variable hex, `src/lexer/mod.rs:493-515`) accepts the full Unicode range including supplementary planes via `char::from_u32`. `\uXXXX` (exactly four hex digits, `src/lexer/mod.rs:516-557`) can only encode the BMP — supplementary planes (> U+FFFF) are unrepresentable in four hex digits and require the `\u{…}` form. In **either** form, a lone surrogate (U+D800..U+DFFF) is rejected with an error and replaced by `U+FFFD`.
- **Interpolation** (`{…}`) is recognized **only** for `Format` strings (`src/lexer/mod.rs:668`). In a `Normal` (or `CStr`) string `{name}` is literal text — the `test_string_no_interpolation` and `cstr_no_interpolation` tests pin this (`src/lexer/mod.rs:1080-1093`, `1608-1619`).
- **`{{` / `}}`** are escaped braces in `Format` strings (→ a single brace, `src/lexer/mod.rs:668-674`, `716-724`); in `Normal` strings they are literal pairs (`test_normal_string_literal_braces`, `src/lexer/mod.rs:1326-1340`).

The result is a `Vec<StringSegment>` (`src/lexer/token.rs:754-760`): runs of literal text become `StringSegment::Literal(String)` and each interpolation becomes `StringSegment::Interpolation(expr_text, Option<fmt_spec>)`. `StringLiteral` carries two helpers the parser/lowering use — `as_plain_text()` (concatenate just the literal segments) and `has_interpolation()` (`src/lexer/token.rs:721-742`).

### `quote_char`: one scanner, both quote styles

The single most reused trick in the string scanner is the `quote_char` byte captured at the opening quote (`src/lexer/mod.rs:598`). Both `"` and `'` open a string, and the scanner remembers *which* and uses it as the closing delimiter (`src/lexer/mod.rs:638-641`). This is why `f"…"` and `f'…'` share one code path — the `f` prefix sets `StringKind::Format`, and `quote_char` lets the same loop terminate on whichever quote opened it. Triple-quoting is only recognized for `"` (`src/lexer/mod.rs:601-604`), so `'` is always a single-quoted form.

### Interpolation-expression scanning

When a `Format` string hits an unescaped `{`, the literal run is flushed and the lexer scans the interpolation expression by **brace-depth counting** (`src/lexer/mod.rs:683-712`). It tracks nested `{`/`}` so a dict literal or a nested block inside the interpolation doesn't terminate it early, and it calls `skip_quoted_string` (`src/lexer/mod.rs:922-935`) to step over any string literal *inside* the interpolation (so a `}` inside a nested string isn't miscounted). An unbalanced brace at end of input is `UnterminatedInterpolation` (`src/lexer/mod.rs:702-706`).

The captured expression text is then split into expression vs. format spec by `split_interpolation_spec` (`src/lexer/mod.rs:940-976`): it finds the **last** `:` at paren/bracket/brace depth 0 and outside quotes. The "last, at depth 0, outside quotes" rule is what prevents false splits on a dict literal `{a: b}`, a slice `x[1:3]`, or a ternary inside the interpolation (`src/lexer/mod.rs:937-939`). So `{x:.2f}` yields `Interpolation("x", Some(".2f"))`. Note the lexer does **not** parse the expression text — it stays a `String` and is re-lexed/parsed later (the parser owns a synthetic span range starting at `1 << 40` for these sub-parsed interp expressions, `src/parser/mod.rs:64-72`).

### Single-quoted and byte literals

`scan_char_literal` (`src/lexer/mod.rs:750-796`) handles plain `'…'`. Despite the name, a single-quoted literal is **not** a distinct char token — it produces a `Token::StringLiteral` with `StringKind::Normal` and no interpolation (a "raw 1-codepoint-ish string"); `'A'`, `'hello'`, and `''` are all valid (`src/lexer/mod.rs:743-749`, `test_char_literal` at `src/lexer/mod.rs:1223-1228`). `f'…'` and `b'…'` are routed away to `scan_string_literal`/`scan_byte_literal` before this function runs, so it only ever sees plain single quotes.

`scan_byte_literal` (`src/lexer/mod.rs:821-861`) handles `b'X'` and emits a plain `Token::IntLiteral(byte_value)` — a byte literal is just an integer. It decodes exactly one char/escape via the shared `parse_one_quoted_char` (`src/lexer/mod.rs:800-817`), validates it fits in a `u8` (else `InvalidEscapeSequence: byte literal: escape value > 255`), and requires the closing `'` (`src/lexer/mod.rs:840-860`). `b'A'` → `IntLiteral(65)` (`test_byte_literal`, `src/lexer/mod.rs:1230-1235`).

## Error model

Lexing never aborts. Every problem is pushed to `Lexer::errors: Vec<LexError>` (`src/lexer/mod.rs:31`) and the scanner recovers locally — an invalid escape becomes `U+FFFD`, a bad number becomes `Token::Error`, an unterminated string ends the literal at the line/EOF. The error kinds are in `src/errors.rs:14-26`: `UnterminatedString`, `UnterminatedCharLiteral`, `InvalidEscapeSequence`, `IndentationMismatch { got }`, `InvalidCharacter`, `InvalidNumericLiteral`, `UnterminatedInterpolation`, `TabCharacter`. The parser collects these alongside its own (the lexer is drained inside `Parser::new`), and the CLI reports them after the run rather than at first sight.

## The `gg lex` command

`gg lex <file>` is the rawest view of this subsystem: it constructs a `Lexer::new(&source)` and prints each `Spanned<Token>` as `[start..end] Token` using the `Debug` impl (`src/main.rs:2712-2720`). Useful for eyeballing exactly where the synthetic `Indent`/`Dedent`/`Newline` tokens land relative to source spans.

## In the self-host

The self-host lexer is `tests/fixtures/self_host_lexer/lexer.gg` (~1090 lines) — the Gorget reimplementation of this same subsystem, written in idiomatic Gorget. It mirrors the Rust architecture closely:

- Same indentation state machine: a `lex_indent_stack: Vector[int]`, `lex_bracket_depth`, and `lex_need_newline` field (`lexer.gg:97-101`), with a stack-flushing EOF path (`lex_emit_eof`, `lexer.gg:358-370`) structurally identical to `emit_eof` above. `lex_process_indentation` (`lexer.gg:347-356`) has only a greater (`>` → push + `Indent`) and a less (`elif <` → pop + `Dedent`s) branch; the equal case is the implicit no-op fall-through, and the self-host omits the `IndentationMismatch` error path the Rust version emits (`src/lexer/mod.rs:213-218`).
- Same `quote_char` trick: the string scanner captures the opening byte and uses it as the closing delimiter, so `f"…"` and `f'…'` share one loop (`lexer.gg:561-582`), and recognizes triple-quotes only for `"` (`lexer.gg:566-568`).
- Same f-string interpolation by brace-depth counting with nested-string skipping (`lexer.gg:602-639`), and the same `{{`/`}}` escape handling.

Two divergences from Rust, the first material:

1. **No `SkFormat` (or `SkCStr`).** The self-host `StringKind` is a 4-variant subset — `SkNormal SkRaw SkMultiLine SkByte` (`lexer.gg:42`) — lacking both `Format` and `CStr` (cf. the 6-variant Rust enum at `src/lexer/token.rs:744-752`). The final-kind selection (`lexer.gg:655-662`) defaults to `SkNormal` and has no Format branch, so `f"…"` and `c"…"` both lex as `SkNormal`. The comparison describer maps `StringKind::Format => "fstr:"` and `Normal => "str:"` (`tests/integration.rs:9226-9227`), so **every fixture containing an f-string mismatches** — this is the dominant source of the standing mismatch count below.
2. **No format-spec split.** The self-host `SegInterpolation(String)` carries a single expression string (`lexer.gg:46`, pushed at `lexer.gg:636`), where the Rust `split_interpolation_spec` splits out an `Option<fmt_spec>`. This is the narrower difference and only matters once an f-string carries a `:spec`.

To check parity, run

```bash
cargo test --test integration lexer_comparison -- --nocapture
```

and read the printed `=== Lexer Comparison Results ===` block — specifically the `Mismatches:` and `Crashes:` counts. **Do not assert "green"**: the test is diagnostic-always-pass — it asserts nothing and passes even with mismatches (`tests/integration.rs:9526-9528`), so a green `cargo test` says nothing; only the printed counts do. At `ffd58b65` the run prints `Fixtures compared: 1120 / Crashes: 0 / Mismatches: 601` — crashes are zero, but there is a large standing mismatch count driven by the missing `SkFormat` kind above (f-strings classified as `str:` rather than `fstr:`). The test (`tests/integration.rs:9346-9530`) lexes every top-level `tests/fixtures/*.gg` with both the Rust `Lexer` and the compiled `lexer.gg` driver and diffs the token streams (comments filtered out). There is also a `self_host_lexer` fixture test (`tests/integration.rs:1965`) that checks the driver's own stdout against a recorded expectation.
