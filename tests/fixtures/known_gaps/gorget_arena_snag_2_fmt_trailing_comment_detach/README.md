# gorget-arena snag #2 — `gg fmt` detaches every trailing comment and misattributes it to the following construct

Filed 2026-08-09 during R39 by user report from a real ~12.7k-line codebase
(gorget-arena) sweep that relocated all 344 trailing comments in one run.

## TL;DR

**Formatter-only bug (backend-independent — runs before codegen).**
`gg fmt` strips every trailing comment — a comment sharing a line with a
statement, `stmt    # comment` — and re-emits it as a standalone comment
line that leads the NEXT statement, at the following statement's indentation.
When the annotated statement is the last in its block, the comment is
instead dedented clean out of the block, to the enclosing/top-level scope.
Comment text is never lost (count conserved), but every trailing comment
ends up documenting code it was never about.

- **Silent:** build still succeeds, program still runs — invisible to
  `gg build` / tests / lints. Only shows on human read.
- **Idempotent:** a second `gg fmt` does not move the comments back.
- **Direction is always downward** — attaches to the next sibling. Even
  the charitable "hoist trailing→leading" reading is off by one construct.
- **Struct field docs are the worst hit:** every field's doc slides down
  to the next field; the last field's doc escapes the struct entirely.
- Trigger: any comment sharing a line with preceding code, regardless of
  leading-whitespace width. Statement kind is irrelevant (struct/enum
  fields, expression statements, `pass`, `return`, match-arm bodies — all
  affected). Full-line (leading) comments are handled correctly; only the
  trailing position is broken.

## Reproducer

`repro.gg` — self-contained, no runtime dependencies. Run:

```bash
./target/release/gg fmt tests/fixtures/known_gaps/gorget_arena_snag_2_fmt_trailing_comment_detach/repro.gg
```

### Input

```gorget
struct Face:
    int first_index       # offset into index buffer
    int num_indices       # number of indices
    int last_field        # doc on the LAST field

void apply(int x):
    match x:
        case 1:
            do_a()        # belongs to case 1
        else:
            pass          # belongs to else
```

### Actual `gg fmt` output (BUG)

```gorget
struct Face:
    int first_index
    # offset into index buffer      ← was on first_index; now leads num_indices
    int num_indices
    # number of indices             ← was on num_indices; now leads last_field
    int last_field

# doc on the LAST field             ← DEDENTED out of the struct, to col 0

void apply(int x):
    match x:
        case 1:
            do_a()
        else:
            # belongs to case 1     ← moved from do_a() line → leads pass in else
            pass

# belongs to else                   ← DEDENTED to top level
```

### Intended output

Each trailing comment stays attached to the statement it annotates —
either kept in the trailing position (`stmt    # comment`), or hoisted to
a leading comment above the SAME statement at the SAME indentation.
Never deferred to the next sibling, never dedented out of its block.

## Likely root cause (user's hypothesis, unverified)

The formatter collects a line's trailing comment and flushes it into the
leading-comment slot of the NEXT AST node it emits, instead of the
trailing slot of the node it belongs to. A block-final statement has no
following sibling in the block, so the pending comment flushes at the
parent scope's indentation — hence the dedent-out-of-block variant.

Fix direction: attach the trailing comment to its own statement (owning
node's trailing slot, not next node's leading slot). Likely lives in the
comment-attach pass or the parser's comment-tokenization glue.

## Real-world impact

Confirmed on gorget-arena (~12.7k lines): a single `gg fmt` sweep
relocated all 344 trailing comments. Struct fields worst hit; every field's
doc slid down to the next field; the last field's doc escaped the struct
to the enclosing module scope. Silent + idempotent means no automated
check catches it — only human review of every diff. Blocks any repo-wide
fmt normalization on codebases that use trailing comments (which is
idiomatic Gorget per the language reference).

## Repro environment

- gorget HEAD `1e03c109` at file time; also reproduces at HEAD `41e9b75be`
  (R39 formatter widening fix, does not touch comment attachment).
- Linux x86-64, Rust release build.
- No env vars required.
