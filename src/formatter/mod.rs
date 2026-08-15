pub mod doc;

use std::rc::Rc;

use crate::lexer::token::{StringKind, StringLiteral, StringSegment, Token};
use crate::parser::ast::*;
use crate::parser::comments::CommentTable;
use crate::span::{Span, Spanned};

// ══════════════════════════════════════════════════════════════
// Emitter — indentation-aware output buffer
// ══════════════════════════════════════════════════════════════

struct Emitter {
    buf: String,
    indent: usize,
    col: usize,
    at_line_start: bool,
}

impl Emitter {
    fn new() -> Self {
        Self {
            buf: String::new(),
            indent: 0,
            col: 0,
            at_line_start: true,
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        debug_assert!(self.indent > 0);
        self.indent -= 1;
    }

    fn write(&mut self, s: &str) {
        if self.at_line_start && !s.is_empty() {
            let indent_width = self.indent * 4;
            for _ in 0..self.indent {
                self.buf.push_str("    ");
            }
            self.at_line_start = false;
            self.col = indent_width;
        }
        self.buf.push_str(s);
        self.col += s.chars().count();
    }

    /// The column the NEXT character will occupy.
    ///
    /// `col` alone is not that number: after `newline()` / `blank_line()` the
    /// cursor sits at line start with the indentation still UNWRITTEN (it is
    /// emitted lazily by the first `write`), so `col` reads 0 while the real
    /// column is `indent * 4`. Every consumer that makes a WIDTH decision —
    /// `write_doc`, which seeds the Doc renderer's starting column — must ask
    /// through here, or a line-initial list is measured `indent * 4` columns
    /// too narrow and silently overruns `MAX_WIDTH` by that much.
    fn current_col(&self) -> usize {
        if self.at_line_start {
            self.indent * 4
        } else {
            self.col
        }
    }

    /// Write pre-formatted text from the Doc renderer.
    /// The text may contain newlines with indentation already baked in.
    /// If we're at line start, prepends the emitter's base indentation first
    /// (just like `write()` does), since the Doc renderer doesn't know about it.
    fn write_preformatted(&mut self, s: &str) {
        if s.is_empty() {
            return;
        }
        if self.at_line_start {
            let indent_width = self.indent * 4;
            for _ in 0..self.indent {
                self.buf.push_str("    ");
            }
            self.col = indent_width;
        }
        self.at_line_start = false;
        self.buf.push_str(s);
        // Columns count CHARACTERS, not bytes: `s` is pre-rendered source that
        // can carry non-ASCII literals, and a byte count would over-report the
        // cursor for everything emitted after them on the same line.
        if let Some(last_nl) = s.rfind('\n') {
            self.col = s[last_nl + 1..].chars().count();
        } else {
            self.col += s.chars().count();
        }
    }

    /// End the current line — IDEMPOTENT AT LINE START.
    ///
    /// **Why idempotent (R41 T-FMT-C).** An expression-position suite ends by
    /// newline-terminating its last statement, and then the ENCLOSING
    /// statement newline-terminates itself. Two newlines render as a blank
    /// line the author never wrote, and the class had members in every host
    /// that can hold a suite-bearing expression (`int r = match ...`, `r +=
    /// match ...`, `throw match ...`, `assert match ...`, an
    /// expression-bodied function, a top-level `const`/`static`) crossed with
    /// every suite that can sit there (match `else`, `catch`, `rethrow`, a
    /// block-bodied closure, an author `do:`).
    ///
    /// Neither side is wrong on its own — each is honouring "I terminate my
    /// own line" — so there is no write site to blame and a per-host list
    /// could only ever chase the members it knew about (it would have missed
    /// `format_const_decl` and `format_static_decl`, which are not
    /// `format_stmt` arms at all). The producer is the right layer: asking to
    /// end a line that has already ended is a no-op.
    ///
    /// `blank_line()` remains the ONE way to ask for a blank, so nothing that
    /// wants one loses it. Verified before the change that no site emits two
    /// consecutive `newline()`s on purpose.
    fn newline(&mut self) {
        if self.buf.ends_with('\n') {
            // Already at line start — the previous emitter closed the line.
            self.at_line_start = true;
            self.col = 0;
            return;
        }
        self.buf.push('\n');
        self.col = 0;
        self.at_line_start = true;
    }

    fn blank_line(&mut self) {
        // Only emit blank line if we're not already on an empty line
        if !self.buf.ends_with("\n\n") && !self.buf.is_empty() {
            if !self.buf.ends_with('\n') {
                self.buf.push('\n');
            }
            self.buf.push('\n');
            self.at_line_start = true;
            // Same reset `newline()` does — leaving the previous line's column
            // behind makes `col` stale while `at_line_start` says otherwise.
            self.col = 0;
        }
    }

    fn finish(self) -> String {
        self.buf
    }

    /// R39 snag #2 trailing-comment helper: inject `s` immediately BEFORE
    /// the buffer's trailing newline (if any), preserving the newline.
    ///
    /// After `format_stmt(prev)` runs the buf typically ends with `\n`.
    /// A trailing comment `# c` that shared the source line with `prev`
    /// must attach to `prev`, not lead the next sibling — so we splice
    /// `# c` in ahead of that `\n`. If the buf doesn't end with `\n`
    /// (rare: sibling emit didn't newline-terminate) we simply append.
    ///
    /// Column/indent bookkeeping: injecting text does NOT change
    /// `at_line_start` (still true after the trailing `\n`) and does
    /// not need `col` update since `at_line_start` invalidates it.
    fn inject_before_newline(&mut self, s: &str) {
        if s.is_empty() {
            return;
        }
        if self.buf.ends_with('\n') {
            // Pop the `\n`, push `s`, re-push `\n`.
            self.buf.pop();
            self.buf.push_str(s);
            self.buf.push('\n');
        } else {
            self.buf.push_str(s);
        }
    }

    /// Add one CONTINUATION line of a multi-line trailing comment: a fresh
    /// output line carrying `pad` spaces then `text`, spliced ahead of the
    /// buffer's trailing newline exactly like [`inject_before_newline`].
    ///
    /// Returns the buffer offset of the pad's FIRST space — the run the
    /// aligner may later re-space when it moves the whole comment run to its
    /// group's column.
    ///
    /// The line break belongs here rather than at the caller: `'\n'` is the
    /// emitter's to write, and `formatter_no_raw_newline_outside_emitter`
    /// enforces it.
    fn inject_continuation_line(&mut self, pad: usize, text: &str) -> usize {
        let line = Self::continuation_line(pad, text);
        // The pad starts one byte past the `\n` this line begins with. When
        // the buffer ends in `\n` the injection pops it first, so the whole
        // spliced run starts at `len - 1`; otherwise it appends at `len`.
        let line_nl = if self.buf.ends_with('\n') {
            self.buf.len() - 1
        } else {
            self.buf.len()
        };
        self.inject_before_newline(&line);
        line_nl + 1
    }

    /// The same continuation line, spliced at a RECORDED position rather than
    /// at the buffer's tail — the deferred header-comment path, whose target
    /// line stopped being the last one while the body rendered. Returns the
    /// number of bytes inserted (the caller shifts the offsets it invalidated).
    fn insert_continuation_line_at(&mut self, at: usize, pad: usize, text: &str) -> usize {
        let line = Self::continuation_line(pad, text);
        self.insert_at(at, &line);
        line.len()
    }

    /// `\n` + `pad` spaces + `text`. The ONE place a continuation line's bytes
    /// are built, so the line break stays the emitter's to write.
    fn continuation_line(pad: usize, text: &str) -> String {
        let mut line = String::with_capacity(1 + pad + text.len());
        line.push('\n');
        for _ in 0..pad {
            line.push(' ');
        }
        line.push_str(text);
        line
    }

    /// Splice `s` into the buffer at `at`, an offset the caller recorded
    /// earlier. Used by the deferred header-comment path, whose target line is
    /// no longer the buffer's last one by the time the comment is emitted.
    fn insert_at(&mut self, at: usize, s: &str) {
        self.buf.insert_str(at, s);
    }

    /// End of the output LINE containing `at` — the position a deferred
    /// header comment is spliced to, resolved at EMISSION time from an anchor
    /// recorded at CLAIM time.
    fn line_end_from(&self, at: usize) -> usize {
        let at = at.min(self.buf.len());
        self.buf[at..]
            .find('\n')
            .map(|off| at + off)
            .unwrap_or(self.buf.len())
    }
}

// ══════════════════════════════════════════════════════════════
// Trailing-comment alignment (R40, owner-directed 2026-08-10)
// ══════════════════════════════════════════════════════════════

/// One recorded trailing-comment injection, captured at the single
/// `emit_trailing_comment_after` chokepoint for the post-pass aligner.
///
/// The aligner groups a contiguous run of ≥2 stmt-lines that each carry a
/// trailing comment (struct fields, enum variants, consecutive vardecls,
/// collection-literal elements) and moves their `#` to a common column so
/// they read as a table. Grouping keys off this TYPED metadata — never a
/// `#` text-search over the buffer (Layering rule 2).
struct TrailingAlign {
    /// Byte offset in `emitter.buf` of the FIRST of the two gap spaces
    /// injected before the comment (i.e. the end of the rendered LHS).
    buf_offset: usize,
    /// Display (char) width of the comment text, leading `#` included —
    /// used by the budget guard, which measures the comment's END column.
    comment_len: usize,
    /// True iff this is a header-line trailing comment (`struct B:  # hdr`,
    /// `if flags:  # b`). Header comments never join a body group.
    is_header: bool,
    /// Width of the rewritable space run that starts at `buf_offset`.
    ///
    /// `TRAILING_COMMENT_GAP` for a head — the gap the writer injected between
    /// the code and the `#`. For a CONTINUATION line it is the WHOLE leading
    /// pad, because the line has no LHS of its own: its `#` column is entirely
    /// made of that pad. The mutating half splices over exactly this many
    /// bytes, so writer and rewriter cannot disagree per entry.
    gap_len: usize,
    /// True iff this entry is a continuation line of the head recorded before
    /// it. A continuation carries no LHS, never drives its group's column, and
    /// is rewritten to whatever column its head lands at.
    is_continuation: bool,
}

/// A claimed comment plus the continuation lines that belong to it — the unit
/// every comment hook takes off the cursor. See
/// [`Formatter::claim_run_at_cursor`].
struct RunClaim {
    head: String,
    /// The head's continuation lines, in source order. Empty for an ordinary
    /// single-line comment, which is the overwhelmingly common case.
    conts: Vec<String>,
    /// SOURCE offset of the head's `#` — the blank-ABOVE probe position.
    head_start: usize,
    /// SOURCE end of the run's LAST line — the blank-BELOW probe position.
    last_end: usize,
}

/// What [`Formatter::claim_header_trailing_comments`] took off the cursor: the
/// claimed runs plus the buffer position that identifies the HEADER's output
/// line, recorded at claim time and resolved to that line's end at emission.
struct HeaderClaim {
    runs: Vec<RunClaim>,
    anchor: usize,
}

/// Where [`Formatter::emit_claimed_run`] puts a claimed run.
#[derive(Clone, Copy)]
enum EmitPos {
    /// Each line on its own output line at the emitter's current indent — the
    /// leading-comment and EOF flushes, and the orphan-pre-close flush.
    OwnLine,
    /// The head is injected onto the buffer's CURRENT last line after the
    /// canonical gap; continuations follow on their own lines under its `#`.
    Inline { is_header: bool },
    /// Like `Inline`, but onto the output line containing `anchor` — a
    /// position recorded when the comment was CLAIMED, because the body that
    /// follows the header renders in between and may itself end the line.
    AtAnchor { anchor: usize, is_header: bool },
}

/// Column geometry derived for one recorded entry from the EMITTED buffer.
struct AlignGeom {
    buf_offset: usize,
    output_line: usize,
    indent_width: usize,
    lhs_width: usize,
    comment_len: usize,
    is_header: bool,
    /// Indices (into the entry list) of the continuation lines belonging to
    /// this head — they move to whatever column the head lands at.
    conts: Vec<usize>,
}

/// Owner-ratified alignment constants (FMT CANON PAIR). Column =
/// smallest multiple of `STRIDE` that is ≥ `max_lhs + TRAILING_COMMENT_GAP`; a
/// comment whose END column would exceed `MAX_WIDTH` triggers outlier
/// exclusion.
///
/// `TRAILING_COMMENT_GAP` serves BOTH roles from one definition: it is the gap
/// `emit_trailing_comment_after` injects between the code and the `#`, and it
/// is the aligner's minimum gap when it computes a group's shared column.
/// Because the injected gap is exactly this many ASCII spaces,
/// `align_trailing_comments` can splice over `[off, off + TRAILING_COMMENT_GAP)`
/// — one constant keeps the writer and the rewriter from ever disagreeing.
const TRAILING_COMMENT_GAP: usize = 4;
const ALIGN_STRIDE: usize = 4;

/// Smallest multiple of `ALIGN_STRIDE` that is ≥ `x`.
fn round_up_to_stride(x: usize) -> usize {
    (x + ALIGN_STRIDE - 1) / ALIGN_STRIDE * ALIGN_STRIDE
}

/// Pure planning half of the trailing-comment aligner: given the emitted
/// buffer and the recorded entries (in buf order), return the gap rewrites
/// `(buf_offset, old_gap_len, new_gap_len)` to apply, sorted LAST→FIRST so
/// earlier offsets stay valid as each is spliced in. Reads only `&str` +
/// typed entries so the mutating half can borrow `buf` mutably afterward.
///
/// ⚠ **Entries MUST be in increasing `buf_offset` order.** The single walk
/// below advances a scan cursor monotonically, so an out-of-order entry would
/// silently take the previous entry's line geometry. Emission order gives the
/// invariant for free everywhere except the one path that injects BACK into an
/// earlier line (`EmitPos::AtAnchor`), which re-sorts itself in — see
/// `record_trailing_align_at`.
fn plan_trailing_aligns(buf: &str, entries: &[TrailingAlign]) -> Vec<(usize, usize, usize)> {
    debug_assert!(
        entries.windows(2).all(|w| w[0].buf_offset <= w[1].buf_offset),
        "plan_trailing_aligns requires entries in buffer order"
    );
    if entries.len() < 2 {
        return Vec::new();
    }
    let bytes = buf.as_bytes();

    // Entries are recorded in emission order → `buf_offset` strictly
    // increasing. Walk the buffer once, deriving each entry's output line
    // (newline count), line start, indent width, and LHS width.
    //
    // A CONTINUATION entry is not a geometry of its own: it is folded into the
    // head recorded before it, whose column it follows. Only its output LINE
    // is kept, because that is what the head's group-adjacency test measures
    // from.
    let mut geoms: Vec<AlignGeom> = Vec::with_capacity(entries.len());
    let mut cont_line: Vec<usize> = vec![0; entries.len()];
    let mut scan = 0usize;
    let mut nl_count = 0usize;
    let mut line_start = 0usize;
    for (ei, e) in entries.iter().enumerate() {
        while scan < e.buf_offset && scan < bytes.len() {
            if bytes[scan] == b'\n' {
                nl_count += 1;
                line_start = scan + 1;
            }
            scan += 1;
        }
        if e.is_continuation {
            cont_line[ei] = nl_count;
            if let Some(head) = geoms.last_mut() {
                head.conts.push(ei);
            }
            continue;
        }
        // Indent width = leading spaces on this output line (the emitter
        // writes exactly `indent*4` spaces before content).
        let mut iw = 0usize;
        let mut k = line_start;
        while k < e.buf_offset && bytes[k] == b' ' {
            iw += 1;
            k += 1;
        }
        // LHS width in DISPLAY columns, i.e. characters — `buf_offset`,
        // `line_start` and `iw` are all byte quantities, so subtracting them
        // yields a BYTE length that over-counts every non-ASCII character on
        // the line and pushes its `#` left of the group's column. Both bounds
        // are char boundaries: `line_start + iw` follows a run of ASCII spaces
        // and `buf_offset` is where the injected gap begins.
        let lhs_width = buf[line_start + iw..e.buf_offset].chars().count();
        geoms.push(AlignGeom {
            buf_offset: e.buf_offset,
            output_line: nl_count,
            indent_width: iw,
            lhs_width,
            comment_len: e.comment_len,
            is_header: e.is_header,
            conts: Vec::new(),
        });
    }

    // Enforce ONE alignment entry per output line (keep the first, lowest
    // buf_offset). Today each `emit_trailing_comment_after` call injects at
    // most one trailing entry per line — `# a  # b` is a SINGLE comment token,
    // not two — so this is a belt-and-suspenders guard that keeps `max_lhs`
    // clean and makes the one-entry-per-line invariant explicit should a
    // future injection path ever record more. Same-line entries are
    // consecutive in buf order, so comparing the previous kept entry suffices.
    let mut deduped: Vec<AlignGeom> = Vec::with_capacity(geoms.len());
    for g in geoms {
        if deduped
            .last()
            .map_or(true, |p| p.output_line != g.output_line)
        {
            deduped.push(g);
        }
    }

    // Group adjacent output lines at equal indent; a header entry never
    // joins a group (it breaks the run and stays a singleton). Group-break
    // on any interior blank / standalone-comment / no-trailing-comment line
    // falls out for free — such a line carries no entry, so the output-line
    // delta jumps ≥ 2.
    //
    // A head's CONTINUATION lines sit between it and the next member, so
    // adjacency is measured from the run's LAST line: a multi-line trailing
    // comment is one logical comment and must not split the alignment group.
    let last_line = |g: &AlignGeom| -> usize {
        g.conts.last().map(|&i| cont_line[i]).unwrap_or(g.output_line)
    };
    let mut groups: Vec<Vec<AlignGeom>> = Vec::new();
    for g in deduped {
        let start_new = match groups.last().and_then(|grp| grp.last()) {
            None => true,
            Some(prev) => {
                prev.is_header
                    || g.is_header
                    || g.output_line != last_line(prev) + 1
                    || g.indent_width != prev.indent_width
            }
        };
        if start_new {
            groups.push(vec![g]);
        } else {
            groups.last_mut().unwrap().push(g);
        }
    }

    let mut rewrites: Vec<(usize, usize, usize)> = Vec::new();
    for group in &groups {
        if group.len() < 2 {
            continue;
        }
        let indent_width = group[0].indent_width;
        // `active` = indices (into `group`) still aligned to the common
        // column. The budget guard drops outliers from this set until no
        // surviving comment's END column exceeds MAX_WIDTH.
        let mut active: Vec<usize> = (0..group.len()).collect();
        let align_col: usize;
        loop {
            if active.len() < 2 {
                // Too few remain to form a table — nobody is realigned;
                // survivors + excluded all keep their natural gap.
                active.clear();
                align_col = 0;
                break;
            }
            let max_lhs = active.iter().map(|&i| group[i].lhs_width).max().unwrap();
            let col = round_up_to_stride(max_lhs + TRAILING_COMMENT_GAP);
            // Overflow measured at the comment END column, in display width.
            let overflow: Vec<usize> = active
                .iter()
                .copied()
                .filter(|&i| indent_width + col + group[i].comment_len > doc::MAX_WIDTH)
                .collect();
            if overflow.is_empty() {
                align_col = col;
                break;
            }
            // A "self-overflower" overflows even at its OWN minimal column
            // (short LHS + long comment) — no amount of dropping wider LHS
            // entries will fit it, so exclude it directly.
            let self_over: Vec<usize> = overflow
                .iter()
                .copied()
                .filter(|&i| {
                    indent_width
                        + round_up_to_stride(group[i].lhs_width + TRAILING_COMMENT_GAP)
                        + group[i].comment_len
                        > doc::MAX_WIDTH
                })
                .collect();
            let drop_idx = if !self_over.is_empty() {
                // Exclude the self-overflower with the LONGEST comment
                // (tie → lowest buf_offset), deterministically.
                *self_over
                    .iter()
                    .max_by(|&&a, &&b| {
                        group[a]
                            .comment_len
                            .cmp(&group[b].comment_len)
                            .then(group[b].buf_offset.cmp(&group[a].buf_offset))
                    })
                    .unwrap()
            } else {
                // Overflow driven purely by a wide LHS inflating the shared
                // column — drop the max-LHS contributor (tie → lowest
                // buf_offset) to lower `align_col` and iterate.
                *active
                    .iter()
                    .max_by(|&&a, &&b| {
                        group[a]
                            .lhs_width
                            .cmp(&group[b].lhs_width)
                            .then(group[b].buf_offset.cmp(&group[a].buf_offset))
                    })
                    .unwrap()
            };
            active.retain(|&i| i != drop_idx);
        }
        for &i in &active {
            let g = &group[i];
            // `align_col ≥ max_lhs + MIN_GAP ≥ lhs_width + MIN_GAP`, so the
            // new gap is always ≥ MIN_GAP.
            rewrites.push((g.buf_offset, TRAILING_COMMENT_GAP, align_col - g.lhs_width));
            // The head's continuation lines move with its `#`. A continuation
            // carries no LHS, so its whole leading pad becomes the head's
            // final `#` column. Continuations of an EXCLUDED head (one the
            // budget guard dropped) or of a singleton group are not listed
            // here and keep the natural pad the writer gave them — which is
            // already the head's own `#` column.
            for &ci in &g.conts {
                rewrites.push((
                    entries[ci].buf_offset,
                    entries[ci].gap_len,
                    indent_width + align_col,
                ));
            }
        }
    }

    // Apply LAST→FIRST so a splice never shifts an as-yet-unwritten offset.
    rewrites.sort_by(|a, b| b.0.cmp(&a.0));
    rewrites
}

// ══════════════════════════════════════════════════════════════
// Formatter — walks AST and emits formatted source
// ══════════════════════════════════════════════════════════════

/// Whether a fill-emitted delimited list consults the comment sideband,
/// and — when it does — over which source range.
///
/// A TYPED decision rather than an `Option<(usize, usize)>`: an
/// un-named `None` hatch lets the next emitter opt out of the interior-
/// comment gate with every guard count unchanged, which is exactly the
/// class the chokepoint exists to retire (Layering rule 2 — the reason
/// crosses the boundary as data, not as an absence). Every
/// `UngatedCarveOut` carries the reason it is one, and
/// `formatter_list_emit_fill_census` pins the whole set.
///
/// `Copy` because a caller that also MEASURES its list (the R42 tail reserve's
/// leading-text pre-render) must hand the very same gate to both renders —
/// two independently-derived gates would be two sources of truth.
#[derive(Clone, Copy)]
enum Gate {
    /// Consult the sideband over `(open_delim_pos, one_past_close_delim)`.
    /// `open_delim_pos` is the byte offset of the OPEN delimiter itself —
    /// never the first element's start, which would leave a comment
    /// between the delimiter and the first element outside the window.
    Span(usize, usize),
    /// Do not consult the sideband. The `&'static str` is the reason, and
    /// the reason is always a property of the CONTEXT (an empty comment
    /// sideband, or a delimiter scan that found nothing), never a
    /// "not implemented yet".
    UngatedCarveOut(&'static str),
}

/// The width budget a `Formatter`'s Doc renders are decided against, and what
/// those renders are FOR.
///
/// A TYPED state rather than a bare `max_width: usize`: one of the two is a
/// MEASUREMENT probe whose width is chosen to force an extreme, and recovering
/// "this is a probe" from the number would be a sentinel read of a write-site
/// fact. `doc::RenderPurpose` carries the same distinction across the boundary
/// into the renderer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RenderBudget {
    /// Real layout at the ratified 120-column budget.
    Layout,
    /// Measurement probe: break at EVERY opportunity, so the first line is
    /// exactly the leading UNBREAKABLE text. Consumer: `measure_leading_text`,
    /// the reserve primitive for a MEASURED tail.
    ///
    /// There is deliberately NO whole-flat (`usize::MAX`) sibling. It would
    /// have exactly one plausible consumer — charging a pre-rendered `Group`
    /// item the flat width of its later siblings — and that charge is
    /// measurably wrong; `comprehension_reserves` records the measurement.
    BreakEverywhere,
}

impl RenderBudget {
    fn width(self) -> usize {
        match self {
            RenderBudget::Layout => doc::MAX_WIDTH,
            RenderBudget::BreakEverywhere => 0,
        }
    }

    fn purpose(self) -> doc::RenderPurpose {
        match self {
            RenderBudget::Layout => doc::RenderPurpose::Layout,
            RenderBudget::BreakEverywhere => doc::RenderPurpose::Measure,
        }
    }
}

/// The escape-(c) reserves for a comprehension's three pre-rendered pieces
/// (`Formatter::comprehension_reserves`).
struct ComprehensionReserves {
    /// Base indent LEVEL the pieces are pre-rendered for.
    indent: usize,
    /// Reserve for the element (or `key: value`) render.
    element: usize,
    /// Reserve for the iterable render.
    iterable: usize,
    /// Reserve for the optional `if` condition render.
    condition: usize,
}

pub struct Formatter {
    emitter: Emitter,
    /// The parser's typed comment sideband. Every attachment fact this
    /// formatter needs — where the `#` is, its column, whether code precedes
    /// it, whether it continues the comment above — is READ from here, never
    /// re-derived by scanning `source` for a newline.
    comments: CommentTable,
    /// Forward-only cursor into `comments`. It advances at exactly ONE place,
    /// [`Formatter::claim_run_at_cursor`], so "claim a head ⇒ claim its whole
    /// run" cannot be bypassed by a new claimer.
    comment_cursor: usize,
    /// Where `comment_cursor` stood at the previous orphan-pre-close flush.
    /// Read only by that flush's `debug_assert`, which is how the
    /// inner-before-outer ordering (an outer flush must never see a comment an
    /// inner block claimed) is CHECKED rather than merely stated.
    last_flush_cursor: usize,
    /// Sideband recorded at the `emit_trailing_comment_after` chokepoint:
    /// one entry per injected trailing comment, consumed by the
    /// `align_trailing_comments` post-pass (owner-directed R40 alignment).
    trailing_aligns: Vec<TrailingAlign>,
    /// Source text, needed for span-based lookups when interleaving
    /// trailing comments — the "same source line as previous emit" check
    /// requires reading the original bytes between two span endpoints
    /// (looking for a `\n` separator). Held as `Rc<str>` so `Formatter`
    /// stays `Clone`-cheap and sub-formatters (the `element_to_string_at`
    /// helper) can pass a trivial empty source without allocating.
    source: Rc<str>,
    /// Width the caller has committed to emitting after the Doc that is about
    /// to be rendered, on that Doc's LAST line. Read by `write_doc` — the one
    /// splice chokepoint — and handed to the renderer, whose
    /// `Renderer::tail_reserve` field doc in `doc.rs` is the single statement
    /// of exactly what the number covers (including the safe-not-exact
    /// inner-node charging and the multi-splice residual). Installed by
    /// `with_tail_reserve` (additive) and, in the one sanctioned exception,
    /// `with_exact_tail_reserve`.
    tail_reserve: usize,
    /// Whether this Formatter's Doc renders are real layout or a measurement
    /// probe. `Layout` for the real one; the probe variants are reached only
    /// through the two measurement primitives below.
    budget: RenderBudget,
}

impl Formatter {
    pub fn new(comments: CommentTable, source: Rc<str>) -> Self {
        Self {
            emitter: Emitter::new(),
            comments,
            comment_cursor: 0,
            last_flush_cursor: 0,
            trailing_aligns: Vec::new(),
            source,
            tail_reserve: 0,
            budget: RenderBudget::Layout,
        }
    }

    // ── Tail reserve: scoped install + the measurement primitives ──

    /// Run `f` with `extra` MORE characters reserved on the line's tail, then
    /// restore. Additive by design: a nested list inside a suffixed header
    /// inherits its parent's reserve for free, and a chain of postfix
    /// operators accumulates its own tails as it recurses.
    ///
    /// **Scope-tightness rule:** install the reserve IMMEDIATELY around the
    /// width-decided render it charges — never around a larger region that
    /// also emits other things. `format_type`'s recursive arms honour this by
    /// installing their INTRA-type tails as they recurse, so each inner
    /// `write_doc` sees exactly its own true remaining tail; that is what
    /// makes the rule hold at every level rather than only the outermost.
    fn with_tail_reserve<R>(&mut self, extra: usize, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = self.tail_reserve;
        self.tail_reserve = saved + extra;
        let r = f(self);
        self.tail_reserve = saved;
        r
    }

    /// Run `f` with the tail reserve REPLACED by `value` (not added to), then
    /// restore.
    ///
    /// The one sanctioned exception to additive-only, and it exists for one
    /// caller: `format_bracketed_broken_with_comments` re-renders every
    /// element on its own line ending in `,`, so a live caller reserve would
    /// be charged to element renders that are nowhere near the caller's
    /// suffix (~25 columns of over-reserve per element).
    fn with_exact_tail_reserve<R>(&mut self, value: usize, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = self.tail_reserve;
        self.tail_reserve = value;
        let r = f(self);
        self.tail_reserve = saved;
        r
    }

    /// Width in CHARACTERS of `f`'s LEADING UNBREAKABLE text — everything up
    /// to its first fit-tested node.
    ///
    /// THE reserve primitive for a MEASURED tail. Rendering at width 0 makes
    /// every break opportunity break, so the FIRST LINE is precisely the
    /// leading literal run: `" = Dict["` for `type A[T] = Dict[…]`, not `" = "`
    /// (which under-reserves by the open delimiter — a measured 121-char line)
    /// and not the whole flat type (which over-reserves and breaks lines that
    /// were in budget). See `doc::Renderer::tail_reserve` for why that is the
    /// boundary.
    ///
    /// Taking the FIRST LINE also makes the trailing-newline question moot:
    /// statements newline-terminate, so a whole-text measurement would have to
    /// trim or over-count by exactly 1 — a defect planted in the measuring
    /// instrument. And a body that cannot render on one line still yields a
    /// correct leading prefix here, so there is no separate escape to detect.
    fn measure_leading_text(&self, f: impl FnOnce(&mut Formatter)) -> usize {
        let s = self.sub_render(0, 0, RenderBudget::BreakEverywhere, f);
        s.lines().next().unwrap_or("").chars().count()
    }

    /// `measure_leading_text`, skipped when this Formatter is itself a
    /// measurement probe.
    ///
    /// Inside a probe the reserve cannot change the outcome — at width 0 every
    /// break opportunity breaks regardless of it — so the measurement would be
    /// pure cost. Skipping it also CAPS the recursion: a remainder measure (a
    /// type's following siblings, a param loop's tail) would otherwise measure
    /// its own remainder once per level, which is exponential in nesting depth
    /// rather than linear, and it is what keeps the measured corpus cost at
    /// ~4% rather than unbounded.
    fn measured_reserve(&self, f: impl FnOnce(&mut Formatter)) -> usize {
        if self.budget != RenderBudget::Layout {
            return 0;
        }
        self.measure_leading_text(f)
    }

    /// The reserve a block header owes when the author put the suite INLINE on
    /// the header's own line: the header suffix (`":"`, or `""` at `on error`),
    /// the separating space, and the body's LEADING UNBREAKABLE PREFIX.
    ///
    /// The prefix, NOT the body's whole flat width — the body manages its own
    /// budget from its first fit-tested node onward, so charging the whole
    /// thing breaks in-budget headers whose body fit-tests a few characters in.
    ///
    /// Installed at the CALLER, around the HEADER-expression render:
    /// `format_inline_suite` runs after the header expression is already
    /// emitted, so a reserve installed inside it could never reach the
    /// header's fit test.
    ///
    /// When the header cannot absorb the reserve the line still overruns —
    /// that is the ruled inline-BODY collision escape (suite member), and
    /// converting the suite to a block form to save width would overwrite the
    /// author's own layout choice.
    fn inline_suite_reserve(&self, header_suffix: &str, block: &Block) -> usize {
        if self.budget != RenderBudget::Layout {
            return 0;
        }
        header_suffix.chars().count()
            + 1
            + self.measure_leading_text(|s| s.format_block_stmts(block))
    }

    /// B1's two SUITE-LAYOUT cells in one call, for a header whose body is a
    /// `Block`: an INDENTED suite owes only the header's `:`; an INLINE one
    /// also owes the separating space and the body's leading unbreakable
    /// prefix.
    ///
    /// Callers compute this BEFORE their clause-header blank/comment hooks —
    /// it reads only the AST, so hoisting is free, and it keeps the hooks
    /// adjacent to the `format_block_stmts` call that
    /// `formatter_suite_layout_hook_census` classifies them by.
    fn suite_header_reserve(&self, body: &Block) -> usize {
        if body.layout == SuiteLayout::Inline {
            self.inline_suite_reserve(":", body)
        } else {
            1
        }
    }

    /// The reserve a `case`/`catch`/`rethrow` header owes for whatever
    /// `format_arm_body` writes on the header's OWN line — EXCLUDING the
    /// caller's own `:`, which each caller spells for itself.
    ///
    /// Mirrors `format_arm_body`'s dispatch exactly:
    ///   * author-spelled `do:` suite → a FIXED 4 (` do:`), so a `case` arm's
    ///     total tail there is 5;
    ///   * an indented (`NextLine`) suite → 0, the body owns its own lines;
    ///   * an inline expression body → the space plus the body's leading
    ///     unbreakable prefix — the inline-BODY collision escape's arm member.
    fn arm_body_reserve(&self, body: &Spanned<Expr>) -> usize {
        if self.budget != RenderBudget::Layout {
            return 0;
        }
        if let Expr::Do {
            author_spelled: true,
            ..
        } = &body.node
        {
            return 4;
        }
        let block_opt = match &body.node {
            Expr::Block(block) => Some(block),
            Expr::Do { body, .. } => Some(body),
            _ => None,
        };
        if let Some(block) = block_opt {
            if block.layout == SuiteLayout::NextLine {
                return 0;
            }
        }
        1 + self.measure_leading_text(|s| s.format_expr(body))
    }

    pub fn format(mut self, module: &Module) -> String {
        self.format_module(module);
        // R39 snag #2 sub-task 5: defensive anchor for the block-final
        // case at EOF. The per-item trailing hook in the module loop
        // already handles a same-line trailing on each top-level item
        // (including the last), and per-field/variant/stmt hooks handle
        // container internals. This EOF hook is a belt-and-suspenders
        // catch for any comment that slipped past those hooks but is
        // still on the same source line as some just-emitted content
        // (rare — would need an item.span.end that under-shot the
        // item's real last byte). Uses `source.len()` as the anchor
        // ceiling; the helper's own `last_real_content_before` walks
        // back over trailing whitespace / comments to find the true
        // last-content position, so passing an over-shoot is safe.
        self.emit_trailing_comment_after(self.source.len(), false);
        self.emit_remaining_comments();
        // R40 (owner-directed 2026-08-10): align contiguous runs of
        // trailing comments to a common column. Runs on the recorded
        // sideband + the emitted buffer, BEFORE `finish()` and BEFORE the
        // blank-line normalization below (which only collapses newlines and
        // cannot affect a within-line gap).
        self.align_trailing_comments();
        let mut result = self.emitter.finish();
        // Normalize blank lines: collapse 3+ consecutive newlines to 2 (one blank line max).
        // Single-pass: track consecutive newlines and skip extras.
        {
            let mut normalized = String::with_capacity(result.len());
            let mut consecutive_newlines = 0u32;
            for ch in result.chars() {
                if ch == '\n' {
                    consecutive_newlines += 1;
                    if consecutive_newlines <= 2 {
                        normalized.push(ch);
                    }
                } else {
                    consecutive_newlines = 0;
                    normalized.push(ch);
                }
            }
            result = normalized;
        }
        // Ensure trailing newline
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result
    }

    // ── Doc IR integration ────────────────────────────────

    /// Format an AST element to a string using a temporary formatter.
    /// Used to produce string representations of elements for Doc wrapping.
    ///
    /// FMT-B (Round XXXVI): seeds the sub-Emitter's `indent` from the
    /// caller's current indent so that any INTERNAL newlines produced by
    /// the sub-render (e.g. a long binary chain that wraps mid-arg) are
    /// indented to the caller's context, not to column 4 (the previous
    /// hardcoded default). Also sets `at_line_start = false` so the
    /// sub-Emitter doesn't prepend indent-spaces on its FIRST write —
    /// the outer's `write_preformatted` handles first-line indent based
    /// on the outer's own cursor.
    ///
    /// The `+ 1` bump matches where the returned string is actually spliced:
    /// every caller of this helper (call args, params, generic args,
    /// comprehensions, method chain, binary chain continuations, closure
    /// params, container literals) places its elements one level deeper than
    /// the caller's cursor — via `Doc::Indent(...)` for the group-based
    /// builders, and via `Doc::Fill`'s own continuation indent for the
    /// fill-packed lists. Callers whose sub-render placement is different can
    /// use `element_to_string_at(base_indent, f)` directly to override.
    fn element_to_string(&self, f: impl FnOnce(&mut Formatter)) -> String {
        self.element_to_string_at(self.emitter.indent + 1, f)
    }

    /// FMT-B (Round XXXVI) explicit-indent variant of `element_to_string`.
    /// Seeds the sub-Emitter with the given `base_indent` (in indent LEVELS,
    /// each = 4 spaces). The sub-Emitter starts `at_line_start = false`
    /// so its first write emits no leading spaces — the outer decides
    /// where to place the sub-buffer.
    fn element_to_string_at(
        &self,
        base_indent: usize,
        f: impl FnOnce(&mut Formatter),
    ) -> String {
        self.sub_render(base_indent, 0, RenderBudget::Layout, f)
    }

    /// Escape-(c) variant: pre-render an element that will be spliced back in
    /// with `reserve` characters still to come on its OWN last line — the
    /// separating `,` for a non-last fill item, or `close + tail_reserve` for
    /// the last one.
    ///
    /// The reserve is an EXPLICIT parameter, never ambient state: one source
    /// of truth, visible at the call site, and impossible to leave set.
    fn element_to_string_reserving(
        &self,
        base_indent: usize,
        reserve: usize,
        f: impl FnOnce(&mut Formatter),
    ) -> String {
        self.sub_render(base_indent, reserve, RenderBudget::Layout, f)
    }

    /// The single sub-render primitive behind `element_to_string_at`,
    /// `element_to_string_reserving` and `measure_leading_text`.
    fn sub_render(
        &self,
        base_indent: usize,
        reserve: usize,
        budget: RenderBudget,
        f: impl FnOnce(&mut Formatter),
    ) -> String {
        // Sub-formatter for Doc-tree rendering has no user comments to
        // interleave (the outer Formatter owns the sideband + cursor) —
        // the trailing-comment helpers find no matching ranges because
        // `comments` is empty. BUT it DOES need the real `source`: the
        // elements it renders carry their original spans, and the
        // `Expr::IntLiteral` arm slices `source[span]` to recover the
        // author's radix / digit-case / underscores. Threading the real
        // source (a cheap `Rc` clone) is what keeps a hex/oct/bin literal
        // inside a broken multi-line collection from reverting to decimal
        // (gorget-js snag #15f). Comment interleaving stays inert because
        // `comments` is empty regardless of source content.
        let mut fmt = Formatter::new(CommentTable::empty(), self.source.clone());
        fmt.tail_reserve = reserve;
        fmt.budget = budget;
        fmt.emitter.indent = base_indent;
        fmt.emitter.at_line_start = false;
        // Seed the CURSOR too, not just the indent level. The rendered string
        // is spliced back in at column `base_indent * 4` — as a broken list's
        // continuation-indented element, or as the fill packer's next line —
        // so a sub-render that measures from column 0 believes it has
        // `base_indent * 4` more columns than it does, and emits lines that
        // overflow the budget by exactly that much.
        //
        // ⚠ The assumption is self-consistent at those two splice positions
        // and NOT at a third, which this round filed rather than inherited:
        // a `Doc::Group`-clothed carrier's FIRST piece — a binary chain's
        // operand zero, a method chain's root — is spliced at the CALLER's
        // current column, because nothing precedes it in the Doc. There the
        // seed is wrong by (caller column − `base_indent * 4`), measured at 23
        // columns on the live corpus line, and the sub-render keeps a nested
        // call flat that should have broken. Repro:
        // `tests/fixtures/known_gaps/fmt_prerender_column_binary_chain.gg`,
        // asserted by the `#[ignore]`d
        // `fmt_prerender_column_binary_chain_stays_in_budget`. The fix is a
        // start-column parameter here, not a reserve — the reserve reaching
        // those pieces is already correct.
        fmt.emitter.col = base_indent * 4;
        f(&mut fmt);
        fmt.emitter.finish()
    }

    /// Render a Doc tree at the current cursor position and write it
    /// into the output buffer. The Doc handles line-break decisions.
    fn write_doc(&mut self, doc: &doc::Doc) {
        let rendered = doc::render_at_reserving(
            doc,
            self.budget.width(),
            // `current_col()`, never the raw `col`: at line start the indent
            // has not been written yet, so the raw field reads 0 while the Doc
            // will actually be placed at `indent * 4`.
            self.emitter.current_col(),
            self.emitter.indent,
            // The caller's committed tail — see `Formatter::tail_reserve` and,
            // for what the number covers, `doc::Renderer::tail_reserve`.
            self.tail_reserve,
            self.budget.purpose(),
        );
        self.emitter.write_preformatted(&rendered);
    }

    // ── Comment interleaving ────────────────────────────────

    /// gorget-arena snag #3b (R40): the single leading-comment chokepoint
    /// (~14 callers). Emits each standalone comment whose span precedes
    /// `pos`, AND — the #3b fix — preserves an author-written blank line
    /// that immediately FOLLOWS a comment in the source: a blank BELOW a
    /// comment (`# c\n\nstmt`) or BETWEEN two comments (`# c1\n\n# c2`).
    /// Pre-#3b this loop dumped comments back-to-back with a bare
    /// `newline()`, so `has_blank_line_between` (which walked transparently
    /// through the run and could place only ONE blank ABOVE the whole
    /// group) was the sole blank source — moving a below-comment blank
    /// above the comment and gluing `# c1`/`# c2` together.
    ///
    /// The blank ABOVE the run stays owned by `has_blank_line_between`
    /// (called by the sibling loops BEFORE this); this helper owns only
    /// the blanks WITHIN/BELOW the run, so the two never double-count.
    ///
    /// The follow-check MUST be `blank_line_follows` (a reorder-immune
    /// forward source scan from the comment's own end) and NOT
    /// `blank_between(comment.end, next_item.span.start)`: imports are
    /// SORTED, so the next AST item's start is a reordered source position
    /// and would false-positive a blank (this exact bug tripped the golden
    /// sample in the scout's first prototype).
    fn emit_comments_before(&mut self, pos: usize) {
        while self.comment_cursor < self.comments.len() {
            if self.comments[self.comment_cursor].hash_pos() >= pos {
                break;
            }
            // CLAIM SITE 1 of 5 — leading comments. Claiming the run keeps a
            // head and its continuation lines together even when `pos` falls
            // between them.
            let claim = self.claim_run_at_cursor();
            self.emit_claimed_run(claim, EmitPos::OwnLine);
        }
    }

    /// EOF-orphan comment flush (gorget-arena snag #3b, R40): same blank
    /// awareness as `emit_comments_before`, for the trailing comment group
    /// at end-of-file. Preserves the blank ABOVE the first orphan comment
    /// (between the last real item and the group) and blanks BETWEEN
    /// consecutive orphan comments. A trailing blank after the LAST comment
    /// is intentionally dropped by `blank_line_follows` (EOF whitespace is
    /// not a paragraph break) and by `format`'s final trailing-newline
    /// normalization.
    fn emit_remaining_comments(&mut self) {
        let mut first = true;
        while self.comment_cursor < self.comments.len() {
            let c_start = self.comments[self.comment_cursor].hash_pos();
            if first {
                first = false;
                if self.blank_line_directly_above(c_start) {
                    self.emitter.blank_line();
                }
            }
            // CLAIM SITE 2 of 5 — the EOF flush.
            let claim = self.claim_run_at_cursor();
            self.emit_claimed_run(claim, EmitPos::OwnLine);
        }
    }

    // ── The claiming chokepoint ─────────────────────────────
    //
    // A trailing comment continued on the lines below it is ONE logical
    // comment (`CommentTable`'s `continues`), so whichever hook claims the
    // HEAD must claim the whole run — otherwise the run splits and the
    // continuation lines document whatever follows them. That rule lives in
    // exactly one pair of functions, and `comment_cursor` advances in exactly
    // one of them, so a new claimer cannot take a head and leave its tail.
    //
    // `formatter_comment_claim_site_census` (tests/lints.rs) pins the set of
    // callers with a reason per site; an unattributed cursor advance is RED.

    /// A claimed comment: the head plus the continuation lines that belong to
    /// it. Produced ONLY by [`Formatter::claim_run_at_cursor`].
    ///
    /// Carries the two SOURCE positions its emitter needs for blank fidelity —
    /// the head's start (the blank ABOVE the run) and the last member's end
    /// (the blank BELOW it). The book: "a blank above or below a comment, or
    /// between two comments, is a deliberate break and is kept."
    fn claim_run_at_cursor(&mut self) -> RunClaim {
        debug_assert!(self.comment_cursor < self.comments.len());
        let head_idx = self.comment_cursor;
        let head_entry = &self.comments[head_idx];
        let head_start = head_entry.hash_pos();
        let head = head_entry.text.clone();
        let mut last_end = head_entry.span.end;
        // THE ONE cursor advance.
        self.comment_cursor += 1;
        let mut conts = Vec::new();
        while self.comment_cursor < self.comments.len()
            && self.comments[self.comment_cursor].continues == Some(head_idx)
        {
            let e = &self.comments[self.comment_cursor];
            conts.push(e.text.clone());
            last_end = e.span.end;
            // THE ONE cursor advance (same statement, continuation half).
            self.comment_cursor += 1;
        }
        RunClaim {
            head,
            conts,
            head_start,
            last_end,
        }
    }

    /// Emit a claimed run at the given position.
    ///
    /// The three positions are what the four claim sites actually need: two
    /// emit standalone lines, one injects onto the line the buffer is
    /// currently on, and one injects onto a line recorded EARLIER (the inline
    /// suite, whose body renders between claim and emission).
    ///
    /// A run's continuation lines are ALWAYS emitted on their own lines, never
    /// collapsed onto the head's (`# one  # two` re-lexes as a single comment
    /// token, so the collapse is silent and idempotent).
    fn emit_claimed_run(&mut self, claim: RunClaim, at: EmitPos) {
        // Set by the `AtAnchor` arm: true iff the splice landed at the end of
        // the buffer, i.e. the construct it belongs to is finished.
        let mut spliced_to_end = false;
        match at {
            EmitPos::OwnLine => {
                self.emitter.write(&claim.head);
                self.emitter.newline();
                for cont in &claim.conts {
                    self.emitter.write(cont);
                    self.emitter.newline();
                }
            }
            EmitPos::Inline { is_header } => {
                let mut inlined = String::with_capacity(claim.head.len() + TRAILING_COMMENT_GAP);
                for _ in 0..TRAILING_COMMENT_GAP {
                    inlined.push(' ');
                }
                inlined.push_str(&claim.head);
                // Record the gap-start for the aligner BEFORE the injection.
                // The buffer currently ends `...LHS\n`; `inject_before_newline`
                // pops that `\n`, so the first gap space lands where the `\n`
                // sits now (`buf.len() - 1`). If the buffer does NOT end in
                // `\n` (rare — the sibling emit didn't newline-terminate), the
                // injection appends, so the gap starts at `buf.len()`.
                let buf_offset = if self.emitter.buf.ends_with('\n') {
                    self.emitter.buf.len() - 1
                } else {
                    self.emitter.buf.len()
                };
                self.emitter.inject_before_newline(&inlined);
                self.record_trailing_align(TrailingAlign {
                    buf_offset,
                    comment_len: claim.head.chars().count(),
                    is_header,
                    gap_len: TRAILING_COMMENT_GAP,
                    is_continuation: false,
                });
                self.emit_continuation_lines(buf_offset, TRAILING_COMMENT_GAP, &claim.conts);
            }
            EmitPos::AtAnchor { anchor, is_header } => {
                // The header's own line, resolved NOW: at claim time the body
                // had not been rendered, and on a multi-line inline body it is
                // the body that owns the buffer's last line.
                let mut at = self.emitter.line_end_from(anchor);
                let mut spliced = String::with_capacity(claim.head.len() + TRAILING_COMMENT_GAP);
                for _ in 0..TRAILING_COMMENT_GAP {
                    spliced.push(' ');
                }
                spliced.push_str(&claim.head);
                let head_offset = at;
                self.emitter.insert_at(at, &spliced);
                self.shift_trailing_aligns_from(at, spliced.len());
                self.record_trailing_align(TrailingAlign {
                    buf_offset: head_offset,
                    comment_len: claim.head.chars().count(),
                    is_header,
                    gap_len: TRAILING_COMMENT_GAP,
                    is_continuation: false,
                });
                at += spliced.len();
                // Continuations follow on their own lines, under the head's
                // `#`, still spliced at the recorded position.
                let head_col = self.emitted_hash_col(head_offset, TRAILING_COMMENT_GAP);
                for cont in &claim.conts {
                    let written = self.emitter.insert_continuation_line_at(at, head_col, cont);
                    self.shift_trailing_aligns_from(at, written);
                    self.record_trailing_align(TrailingAlign {
                        buf_offset: at + 1,
                        comment_len: cont.chars().count(),
                        is_header: false,
                        gap_len: head_col,
                        is_continuation: true,
                    });
                    at += written;
                }
                // Whether this splice ended the construct — see
                // `owns_blank_below` below.
                spliced_to_end = self.emitter.buf[at..].trim_end_matches('\n').is_empty();
            }
        }
        // Blank fidelity: an author blank directly BELOW the run is
        // paragraphing and survives. Asked ONCE, of the run's LAST member — by
        // construction no blank can sit inside a run.
        //
        // For an INLINE head with no continuations the blank below is already
        // owned by the sibling loop's `has_blank_line_between`, which walks up
        // from the next sibling and finds the head's own CODE line. Add
        // continuation lines and that walk stops on a comment line instead and
        // reports "no blank" — so the run's emitter owns the cell exactly when
        // the run has a tail. (`blank_line()` is a no-op when the buffer is
        // already blank-terminated, so the two never double-count.)
        //
        // `AtAnchor` owns it under the same rule PLUS one more condition: the
        // splice must have ended the construct. When the body continues below
        // the spliced run there is no position in the output that corresponds
        // to "below the run" — the author's blank sat between the header's
        // comment and a body that now precedes it — so nothing is emitted
        // rather than something in the wrong place.
        let owns_blank_below = match at {
            EmitPos::OwnLine => true,
            EmitPos::Inline { .. } => !claim.conts.is_empty(),
            EmitPos::AtAnchor { .. } => !claim.conts.is_empty() && spliced_to_end,
        };
        if owns_blank_below && self.blank_line_follows(claim.last_end) {
            self.emitter.blank_line();
        }
    }

    /// Emit a run's continuation lines under a head whose `#` is at the
    /// buffer offset `head_gap_offset` (+ `head_gap_len` for the gap it sits
    /// behind). The column is derived from the head's TYPED record, never by
    /// searching the emitted buffer for a `#` — a head whose own TEXT contains
    /// a `#` (`# see #42`) would mis-column its run and break idempotence.
    fn emit_continuation_lines(
        &mut self,
        head_gap_offset: usize,
        head_gap_len: usize,
        conts: &[String],
    ) {
        if conts.is_empty() {
            return;
        }
        let head_col = self.emitted_hash_col(head_gap_offset, head_gap_len);
        for cont in conts {
            let buf_offset = self.emitter.inject_continuation_line(head_col, cont);
            self.record_trailing_align(TrailingAlign {
                buf_offset,
                comment_len: cont.chars().count(),
                is_header: false,
                gap_len: head_col,
                is_continuation: true,
            });
        }
    }

    /// The CHARACTER column at which a trailing comment's `#` was emitted,
    /// from the typed record of its gap: the gap starts at `gap_offset` and is
    /// `gap_len` wide, so the `#` sits that far past the line start.
    ///
    /// ⚠ Unit trap: `gap_offset` is a BYTE offset and the answer is a
    /// character column, so the prefix is measured with `chars().count()` —
    /// the same byte/char distinction `plan_trailing_aligns` makes for its LHS
    /// widths.
    fn emitted_hash_col(&self, gap_offset: usize, gap_len: usize) -> usize {
        let buf = &self.emitter.buf;
        let gap_offset = gap_offset.min(buf.len());
        let line_start = buf[..gap_offset].rfind('\n').map(|p| p + 1).unwrap_or(0);
        buf[line_start..gap_offset].chars().count() + gap_len
    }

    /// Record an aligner entry, keeping `trailing_aligns` in BUFFER order.
    ///
    /// Emission order gives that for free — except on the deferred
    /// header-comment path, which splices BACK into an earlier line after
    /// later entries have already been recorded. `plan_trailing_aligns` walks
    /// its scan cursor monotonically and would silently mis-attribute an
    /// out-of-order entry, so the insert is sorted rather than pushed.
    fn record_trailing_align(&mut self, entry: TrailingAlign) {
        let at = self
            .trailing_aligns
            .partition_point(|e| e.buf_offset <= entry.buf_offset);
        self.trailing_aligns.insert(at, entry);
    }

    /// THE ORPHAN-PRE-CLOSE FLUSH — the shared chokepoint for a comment
    /// written after a block's LAST child, called immediately before the
    /// block's `dedent()`.
    ///
    /// Such a comment leads no next child and trails no previous one, so no
    /// sibling hook claims it and it falls out to whatever hook fires next —
    /// in the worst case the module-level flush at column 0, which reads as
    /// "this documents the NEXT item". This flush claims it at the still-open
    /// body indent instead.
    ///
    /// **Membership is the language's own layout rule, read from typed
    /// metadata:** an own-line comment whose `#` column is STRICTLY GREATER
    /// than the indent of the block's HEADER line, and whose `#` starts before
    /// the block's recorded end, was written inside the block.
    ///
    /// Three load-bearing properties:
    ///
    ///  * **COLUMN decides, not the span.** A block's `span.end` is its DEDENT
    ///    token, whose span reaches into the NEXT code line — so a span
    ///    ceiling alone drags a column-0 comment written for the next item
    ///    into the closing block. The negative cells in the fixture net pin
    ///    exactly that.
    ///  * **It claims a PREFIX and stops.** `comment_cursor` is forward-only,
    ///    so the first comment failing either test must `break`; skipping one
    ///    and continuing would emit the rest out of order.
    ///  * **INNER blocks flush before OUTER containers.** Emission order gives
    ///    this for free (an inner `dedent()` precedes its outer one), and every
    ///    nested flush site depends on it: the forward-only prefix claim then
    ///    guarantees an outer flush never sees a comment an inner one owns.
    ///    The `debug_assert` below is that invariant's guard.
    ///
    /// `header_start` is any byte on the OWNING CONSTRUCT's FIRST source line
    /// — for statement suites the typed `Block::header_start` field, for the
    /// container/arm families the node's own span start. Not the block's
    /// `span.start`, which is the COLON: on a wrapped header the colon sits on
    /// a continuation line indented at or past the body, and the column test
    /// would then refuse everything.
    ///
    /// A run's continuation lines never reach here: they belong to their head
    /// and are claimed with it at the claiming chokepoint
    /// (`claim_run_at_cursor`), whose call-site census is the guard.
    fn emit_orphan_comments_before_close(&mut self, header_start: usize, block_end: usize) {
        // The inner-before-outer ordering, checked rather than asserted in
        // prose: every flush must see the cursor at or past where the previous
        // one left it. A rewind is the one way an outer flush could re-emit a
        // comment an inner block already claimed.
        debug_assert!(
            self.comment_cursor >= self.last_flush_cursor,
            "the comment cursor REWOUND between flushes ({} -> {}); an outer \
             flush can now re-emit a comment an inner block already claimed",
            self.last_flush_cursor,
            self.comment_cursor,
        );
        self.last_flush_cursor = self.comment_cursor;
        let header_col = self.line_indent_of(header_start);
        let mut first = true;
        while self.comment_cursor < self.comments.len() {
            let c = &self.comments[self.comment_cursor];
            if c.hash_pos() >= block_end || !c.is_own_line() || c.hash_col <= header_col {
                break;
            }
            // CLAIM SITE 5 of 5 — the orphan-pre-close flush.
            let claim = self.claim_run_at_cursor();
            // Same blank-awareness as `emit_remaining_comments`: an author
            // blank ABOVE the orphan run is paragraphing and survives.
            if first {
                first = false;
                if self.blank_line_directly_above(claim.head_start) {
                    self.emitter.blank_line();
                }
            }
            self.emit_claimed_run(claim, EmitPos::OwnLine);
        }
    }

    /// Character count of the leading whitespace on the source line containing
    /// `pos` — the line's INDENT, not the column of `pos` itself (a container
    /// header may carry `public ` before its keyword and still be at indent 0,
    /// and a mid-line anchor is entirely normal).
    fn line_indent_of(&self, pos: usize) -> usize {
        let bytes = self.source.as_bytes();
        let mut i = pos.min(bytes.len());
        while i > 0 && bytes[i - 1] != b'\n' {
            i -= 1;
        }
        let mut n = 0usize;
        while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
            i += 1;
            n += 1;
        }
        n
    }

    /// Shift the recorded aligner offsets that a back-injection invalidated.
    ///
    /// `buf_offset` is an ABSOLUTE position, so inserting `len` bytes at `at`
    /// moves every entry at or after it. Without this the aligner splices over
    /// a stale offset — silent, mid-line corruption rather than a panic.
    fn shift_trailing_aligns_from(&mut self, at: usize, len: usize) {
        for e in &mut self.trailing_aligns {
            if e.buf_offset >= at {
                e.buf_offset += len;
            }
        }
    }

    /// Interior-comment gate: returns true iff any UNEMITTED comment's
    /// span.start lies STRICTLY INSIDE `(start, end)`.
    ///
    /// TWO call sites, and only two: the delimited-list chokepoint
    /// `emit_delimited_list`, and the `Expr::TupleLiteral` single-element
    /// branch, which emits `(x,)` flat rather than fill-packed and so does
    /// not pass through the chokepoint. A true answer routes the region to
    /// `format_bracketed_broken_with_comments` instead of the fill-packed
    /// `doc::surround_fill` path. When ANY element-interior
    /// comment exists, the doc DSL cannot preserve it — the sub-formatter
    /// used by `element_to_string` is passed an EMPTY comment sideband
    /// (it does hold the real `source` for span lookups, but no comments),
    /// so any comment interior to a collection literal is
    /// silently dropped by the sub-render and then dedented to column 0
    /// by the OUTER `emit_trailing_comment_after` after the literal
    /// closes. The broken-with-comments path reuses the outer formatter's
    /// comment sideband cursor to place each comment at the correct
    /// interior indent, retiring the whole class.
    ///
    /// Strict-interior: a comment at exactly `start` or `end` belongs to
    /// the ENCLOSING scope's sibling-loop hooks (leading of container,
    /// trailing after container). Only comments in `(start, end)` are
    /// collection-literal-interior and need the broken path.
    fn has_interior_comments(&self, start: usize, end: usize) -> bool {
        self.comments
            .from(self.comment_cursor)
            .iter()
            .any(|c| c.hash_pos() > start && c.hash_pos() < end)
    }

    /// Interior-comment exploded emission (Core #4 producer chokepoint):
    /// render `open elems close` in BROKEN (multi-line) form with the
    /// outer formatter's comment sideband interleaved per element.
    ///
    /// ONE caller: `emit_delimited_list`, when its gate fires — i.e. when
    /// the fill-packed `doc::surround_fill` path would DROP interior
    /// comments (the exact snag class). The lint
    /// `formatter_collection_literal_interior_hook_dispatch` pins that at
    /// one, so a new list emitter cannot reach this shape without passing
    /// the gate on the way.
    ///
    /// Emit order per element:
    ///   1. `emit_comments_before(elem_start)` — flush standalone-line
    ///      leading comment(s) at the currently-indented interior cursor.
    ///   2. `format_elem(self, elem)` — render the element via the OUTER
    ///      formatter (so nested literals ALSO route through this helper
    ///      if they too have interior comments; Core #4 recursion).
    ///   3. `,` + newline — one element per line, trailing comma always.
    ///   4. `emit_trailing_comment_after(elem_end)` — attach any same-
    ///      source-line trailing comment via `inject_before_newline`.
    ///
    /// After the loop, BEFORE the closing bracket:
    /// `emit_comments_before(container_end)` flushes any ORPHAN comment
    /// that sits after the last element's span-end but before the closing
    /// bracket (e.g. `[a, b,\n    # tail\n]` where `# tail` shares no
    /// element line). Without this, the orphan escapes to the enclosing
    /// scope's next comment hook and dedents to column 0 — the same bug
    /// in miniature (pass 1 R2 fold).
    ///
    /// `container_end` is the EXCLUSIVE end of the container (the byte
    /// AFTER the closing bracket). For AST-span gates it comes from the
    /// node's recorded span (logos' end-exclusive convention); for the
    /// scan-derived gates it is one past the scanned close delimiter —
    /// both satisfy the same contract (output-review corrected the older
    /// "MUST be AST-recorded" claim, falsified by the scan-derived seven).
    fn format_bracketed_broken_with_comments<E>(
        &mut self,
        open: &str,
        close: &str,
        container_end: usize,
        elems: &[E],
        span_of: impl Fn(&E) -> (usize, usize),
        mut format_elem: impl FnMut(&mut Formatter, &E),
    ) {
        self.emitter.write(open);
        self.emitter.newline();
        self.emitter.indent();
        for elem in elems {
            let (elem_start, elem_end) = span_of(elem);
            self.emit_comments_before(elem_start);
            // Reserve EXACTLY 1 — the `,` written below, unconditionally,
            // after every element including the last. Exactly, not additively:
            // a live caller reserve belongs to the CLOSE line, not to these
            // element lines, and charging it here over-reserves each of them.
            //
            // What has NO enforcement here, stated rather than implied: the
            // CLOSE line. This path is a hand-rolled loop — no `Doc::Fill`, no
            // fit test — so `close` is written at the outer indent and the
            // caller's suffix follows it unmeasured. Outer indent + close +
            // suffix is short in every real shape; a hypothetical overrun
            // there is out of scope and not enforced. The trailing comment an
            // element line may carry is likewise not fit-tested.
            self.with_exact_tail_reserve(1, |f| format_elem(f, elem));
            self.emitter.write(",");
            self.emitter.newline();
            self.emit_trailing_comment_after(elem_end, false);
        }
        // Orphan-comment flush before close (pass 1 R2): a comment on its
        // own line between the last element and the closing bracket has no
        // element to attach to; without this it escapes to the outer
        // scope's next `emit_trailing_comment_after` and dedents.
        self.emit_comments_before(container_end);
        self.emitter.dedent();
        self.emitter.write(close);
    }

    // ── The delimited-list chokepoint ───────────────────────
    //
    // Every FILL-EMITTED delimited list in the language funnels through
    // these two functions. `emit_delimited_list` decides — BEFORE the Doc
    // layer — whether the region carries an interior comment; only
    // `emit_delimited_texts` reaches `doc::surround_fill`.
    //
    // The decision cannot be taken any lower. `Doc::Fill` writes `", "`
    // AFTER each pre-rendered item, so an item text ending in `# c` would
    // swallow the separator and the rest of the list; that is why
    // `element_to_string` hands its sub-`Formatter` an EMPTY comment
    // sideband, and why a comment-bearing list must reach its exploded
    // shape imperatively instead. The boundary is therefore the last
    // point at which the sideband is still visible.

    /// Terminal splice of a pre-rendered delimited list into the buffer.
    ///
    /// The ONLY `doc::surround_fill` call site in the formatter. A new
    /// list emitter that wants fill packing has to come through here, so
    /// it cannot silently skip the gate above it — which is what
    /// `formatter_list_emit_fill_census` pins at 1.
    fn emit_delimited_texts(&mut self, open: &str, close: &str, items: Vec<String>) {
        let docs: Vec<doc::Doc> = items.into_iter().map(doc::text).collect();
        let doc = doc::surround_fill(open, docs, close);
        self.write_doc(&doc);
    }

    /// The chokepoint for a fill-emitted delimited list.
    ///
    /// On `Gate::Span`, an interior comment routes the list to
    /// `format_bracketed_broken_with_comments`, which re-renders every
    /// element on the OUTER formatter — so a nested list recurses back
    /// into this gate and reaches the same exploded shape (Core #4
    /// recursion). Hence the canonical form: *a FILL-EMITTED container
    /// with an interior comment breaks fully, and so does every ancestor
    /// FILL-EMITTED container on the path to it.* The scope is part of the
    /// rule, not a caveat on it — see below for what is outside.
    ///
    /// Scope: FILL-EMITTED lists only. Three other mechanisms build
    /// delimited regions and none of them comes through here, so an
    /// interior comment is still re-parented in all three:
    ///
    ///  * the hand-rolled `write(", ")` comma loops — e.g. pattern field
    ///    lists, enum tuple-variant field lists, tuple and function TYPES.
    ///    ⚠ Those are EXAMPLES, not the set: the set is pinned as a COUNT
    ///    by `formatter_list_emit_fill_census`'s `EXPECTED_WRITE_SEP` and
    ///    enumerated row-by-row with dispositions in `TODO.md`;
    ///  * regions pre-rendered one level ABOVE a list emitter (method
    ///    chains of 2+ segments, binary chains, `??`, comprehensions);
    ///  * the non-list `Expr::Index` brackets.
    ///
    /// All three are filed with durable repros — see `TODO.md`.
    fn emit_delimited_list<E>(
        &mut self,
        open: &str,
        close: &str,
        gate: Gate,
        elems: &[E],
        span_of: impl Fn(&E) -> (usize, usize),
        format_elem: impl Fn(&mut Formatter, &E),
    ) {
        if let Gate::UngatedCarveOut(reason) = gate {
            // Core #14 — the reason is load-bearing, not decoration. In a
            // DEBUG build the scan-miss path panics inside
            // `gate_or_scan_miss` before it can arrive here, so every
            // carve-out that reaches this point is one of the declared
            // empty-sideband ones — and THAT is checkable: their soundness
            // rests entirely on the sideband being inert.
            debug_assert!(
                self.comments.is_empty(),
                "ungated carve-out `{reason}` fired on a formatter that DOES \
                 hold comments — the stated reason (an empty comment sideband) \
                 does not hold, so an interior comment escapes here"
            );
        }
        if let Gate::Span(interior_start, container_end) = gate {
            // Provenance guard: the gate's open position must BE the open
            // delimiter. A guessed or drifted offset would silently widen
            // or narrow the window instead of failing, which is the whole
            // class of bug the scan design exists to avoid.
            debug_assert_eq!(
                self.source.as_bytes().get(interior_start).copied(),
                open.as_bytes().first().copied(),
                "Gate::Span open position {interior_start} is not the `{open}` \
                 delimiter (source byte: {:?})",
                self.source.as_bytes().get(interior_start).map(|b| *b as char),
            );
            if self.has_interior_comments(interior_start, container_end) {
                self.format_bracketed_broken_with_comments(
                    open,
                    close,
                    container_end,
                    elems,
                    span_of,
                    |f, e| format_elem(f, e),
                );
                return;
            }
        }
        // Escape (c), closed for all ten list kinds at once: an item's
        // sub-render renders at the full budget and so is blind to whatever
        // its parent appends after it on its OWN last line. Charge it — the
        // separating `,` for every item but the last, and for the last one the
        // list's `close` plus this Formatter's live tail reserve (the caller's
        // suffix, which lands on the same line as the close).
        let last = elems.len().saturating_sub(1);
        let last_reserve = close.chars().count() + self.tail_reserve;
        let base_indent = self.emitter.indent + 1;
        let items: Vec<String> = elems
            .iter()
            .enumerate()
            .map(|(i, e)| {
                let reserve = if i == last { last_reserve } else { 1 };
                self.element_to_string_reserving(base_indent, reserve, |f| format_elem(f, e))
            })
            .collect();
        self.emit_delimited_texts(open, close, items);
    }

    /// The single `Option<(usize, usize)> -> Gate` converter: every
    /// delimiter-scan result becomes a gate here, and every scan MISS
    /// becomes the one carve-out spelled `"scan miss"`.
    ///
    /// A miss loses the container's END, and every substitute end
    /// available at a scan-derived site reaches into the FUNCTION BODY —
    /// so gating on a guessed span would hoover body comments into the
    /// parameter list, which is this class of bug in reverse. The safe
    /// fallback is today's pre-render behaviour.
    ///
    /// The `debug_assert!` is the miss DETECTOR (no corpus input reaches
    /// it); the census pins that the fallback exists in exactly ONE place,
    /// so the number of carve-out sites does not grow with the number of
    /// callers that can miss.
    fn gate_or_scan_miss(&self, scanned: Option<(usize, usize)>) -> Gate {
        match scanned {
            Some((open_pos, container_end)) => Gate::Span(open_pos, container_end),
            None => {
                debug_assert!(
                    false,
                    "delimiter scan missed: a fill-emitted list fell back to the \
                     ungated pre-render path. The window argument at \
                     `delim_pos_after` no longer holds for some source shape."
                );
                Gate::UngatedCarveOut("scan miss")
            }
        }
    }

    /// Byte offset of the first `ch` in `[from, upto)` that is not inside
    /// a comment span, mirroring `last_real_content_before`'s use of the
    /// parser's comment table.
    ///
    /// WINDOW RULE (the safety argument, and the reason this is a source
    /// scan rather than a parser-recorded span). Callers must pass a
    /// window that provably contains no string literal, because a `(` or
    /// `[` inside one would be indistinguishable from the real delimiter.
    /// The window that satisfies this is the POST-ANCHOR one, where the
    /// anchor has ADVANCED PAST any explicit generic-argument region:
    ///
    ///  * a call / method call with explicit generic args anchors at the
    ///    generic-args `]` (one past it), never at the callee or method
    ///    name — `identity[Callable[void(int)]](c)` puts a `(` inside the
    ///    generic-args region, so the pre-generic-args window is NOT safe;
    ///  * a function's parameter list anchors at the generic-PARAMS span
    ///    end when the function is generic, else at the name;
    ///  * a closing-delimiter search runs from the LAST element's span end.
    ///
    /// Between such an anchor and the delimiter, only whitespace, commas
    /// and comments can appear. If a caller cannot advance its anchor —
    /// because the gate it would anchor on MISSED — it must not fall back
    /// to the pre-anchor position; it propagates the miss instead (see
    /// `gate_or_scan_miss`).
    fn delim_pos_after(&self, from: usize, upto: usize, ch: u8) -> Option<usize> {
        let bytes = self.source.as_bytes();
        let hi = upto.min(bytes.len());
        let mut i = from.min(hi);
        while i < hi {
            if bytes[i] == ch
                && !self
                    .comments
                    .iter()
                    .any(|cm| cm.span.start <= i && i < cm.span.end)
            {
                return Some(i);
            }
            i += 1;
        }
        None
    }

    /// Scan a parenthesized argument/parameter tuple: `(` .. one past `)`.
    ///
    /// `anchor` is the end of whatever immediately precedes the `(` under
    /// the window rule at `delim_pos_after` (callee end, method-name end,
    /// generic-args `]`, or generic-PARAMS span end). `outer_end` bounds
    /// both searches. `first_elem_start` tightens the `(` search so a
    /// delimiter inside an element cannot be mistaken for the opener; the
    /// `)` search starts at the LAST element's end for the same reason.
    ///
    /// PREMISE (pinned by `fmt_delimited_list_window_safety.gg`):
    /// `Param.span` INCLUDES the default-value expression, so
    /// `String s = "a)b"` ends AFTER the string and the `)` search never
    /// starts inside it. Were the premise false, the truncated end would
    /// land on the `)` inside the literal — observable only through an
    /// orphan-pre-close comment, which is exactly what that fixture uses.
    fn paren_tuple_gate(
        &self,
        anchor: usize,
        outer_end: usize,
        first_elem_start: Option<usize>,
        last_elem_end: Option<usize>,
    ) -> Option<(usize, usize)> {
        let upto = first_elem_start.unwrap_or(outer_end);
        let lp = self.delim_pos_after(anchor, upto, b'(')?;
        let rp = self.delim_pos_after(last_elem_end.unwrap_or(lp), outer_end, b')')?;
        Some((lp, rp + 1))
    }

    /// Scan a bracketed generic-argument list: `[` .. one past `]`.
    /// Same window rule as `paren_tuple_gate`; `upto` bounds the closing
    /// search (the enclosing node's end).
    fn generic_args_gate(
        &self,
        anchor: usize,
        first_arg_start: Option<usize>,
        last_arg_end: Option<usize>,
        upto: usize,
    ) -> Option<(usize, usize)> {
        let lb = self.delim_pos_after(anchor, first_arg_start.unwrap_or(upto), b'[')?;
        let rb = self.delim_pos_after(last_arg_end.unwrap_or(lb + 1), upto, b']')?;
        Some((lb, rb + 1))
    }

    /// Derive both gates of a callee-shaped expression — `name[generic
    /// args](call args)` — in ONE place, so the window rule's
    /// ANCHOR-ADVANCE and its miss PROPAGATION are stated once rather
    /// than re-derived at `Expr::Call`, `Expr::MethodCall`,
    /// `Expr::StructLiteral` and `Expr::DotShorthand`.
    ///
    /// ANCHOR-ADVANCE: with explicit generic args the argument tuple
    /// anchors at the generic-args `]`, never at the name. Explicit
    /// generic args can themselves contain a `(` —
    /// `identity[Callable[void(int)]](c)` — so a scan started at the name
    /// end would return the `(` INSIDE the brackets and gate over a
    /// window beginning mid-generic-args.
    ///
    /// MISS PROPAGATION: if the generic-args scan misses there is no
    /// advanced anchor, and falling back to the name end is precisely the
    /// unsafe window above. The sibling therefore inherits the miss and
    /// becomes an ungated carve-out too. (No source shape reaches this;
    /// the `debug_assert!` in `gate_or_scan_miss` is the detector.)
    ///
    /// Returns `None` for the generic-args gate exactly when the
    /// expression has no explicit generic args.
    fn callee_arg_gates(
        &self,
        name_end: usize,
        generic_args: Option<&[Spanned<Type>]>,
        outer_end: usize,
        first_arg_start: Option<usize>,
        last_arg_end: Option<usize>,
    ) -> (Option<Gate>, Gate) {
        let (ga_gate, anchor) = match generic_args {
            Some(ga) => {
                let scanned = self.generic_args_gate(
                    name_end,
                    ga.first().map(|t| t.span.start),
                    ga.last().map(|t| t.span.end),
                    outer_end,
                );
                (
                    Some(self.gate_or_scan_miss(scanned)),
                    scanned.map(|(_, ga_end)| ga_end),
                )
            }
            None => (None, Some(name_end)),
        };
        let args_scanned = anchor.and_then(|a| {
            self.paren_tuple_gate(a, outer_end, first_arg_start, last_arg_end)
        });
        (ga_gate, self.gate_or_scan_miss(args_scanned))
    }

    /// R39 snag #2 sub-task 5b — container-header trailing comment.
    ///
    /// A comment on the SAME source line as a container-header
    /// (`struct S:  # header`, `enum E:  # what`, `trait T:  # note`,
    /// `equip S with T:  # via`) belongs to that header, not to the
    /// first body element. Without this hook the emit order is:
    ///   `struct S:\n` → `emit_comments_before(field.span.start)` fires
    ///   on the loop's first iteration → header comment is emitted at
    ///   field indent as a leading comment of the first field.
    /// With the hook fired right after `newline()`, the comment is
    /// spliced inline via `inject_before_newline` ahead of the just-
    /// emitted `\n`, restoring `struct S:  # header\n`.
    ///
    /// The `header_anchor_end` argument is the source byte position of
    /// the LAST anchor token on the header line — for the four
    /// structural containers this is the container name's span end
    /// (`s.name.span.end`), which is always on the same source line as
    /// the `:` for the common single-line-header shape. A multi-line
    /// header (`struct S[\n T]:  # x`) is out of scope and falls
    /// through to `emit_comments_before` (deferred to R40 —
    /// `known_gaps/gorget_arena_snag_2_intra_item_multiline_header`).
    ///
    /// **Separate from `emit_trailing_comment_after` on purpose:**
    /// the semantics are distinct (same-line-as-header vs same-line-
    /// as-previous-sibling) even though the underlying same-line
    /// mechanic is identical. Keeping them separate keeps the
    /// `formatter_sibling_loops_hook_pairing` lint's sibling-pairing
    /// counts for `emit_comments_before` / `emit_trailing_comment_after`
    /// honest; this header hook is counted SEPARATELY by that same lint,
    /// as `EXPECTED_EMIT_TRAILING_AFTER_HEADER` (structural containers
    /// plus the control-flow and function-definition openers — see the
    /// constant's own comment for the live roster).
    fn emit_trailing_comment_after_header(&mut self, header_anchor_end: usize) {
        // Same semantics — reuse the base helper (single-implementation
        // chokepoint per Core #4). If the multi-line-header rescope
        // (R40) needs different behaviour, this wrapper adapts here
        // while the base helper stays unchanged. `is_header=true` so the
        // R40 aligner never groups a header comment with its body fields
        // (or with a same-indent sibling stmt — `int x = 5  # a` followed
        // by `if flags:  # b`).
        self.emit_trailing_comment_after(header_anchor_end, true);
    }

    /// R41 T-FMT-C: CLAIM a header's trailing comment now, EMIT it after the
    /// inline suite body — the header hook's two halves, split.
    ///
    /// **Why not just emit it in place.** On an INLINE suite the body follows
    /// the header on the same output line, so a comment emitted at the header
    /// swallows the statement: `case 1: 10  # one` became
    /// `case 1:  # one 10`, which does not re-parse.
    ///
    /// **Why not just suppress it.** At a match arm the body is an
    /// EXPRESSION, so there is no statement-side hook to claim the comment
    /// instead; it would drift down and lead the NEXT arm.
    ///
    /// **Why CLAIM here rather than defer the whole hook.** Claiming advances
    /// the comment cursor before the body is emitted, which is what stops a
    /// leading-comment hook INSIDE the body from taking the comment first —
    /// for a multi-line inline child (`if true: int x = match y:  # note`)
    /// the nested match-arm loop's `emit_comments_before` would otherwise
    /// pull it into the arm list, where it documents the wrong thing.
    ///
    /// Safe in both directions: the cursor is monotone, so nothing is emitted
    /// twice, and `emit_remaining_comments` flushes anything a caller drops.
    ///
    /// **The claim takes the WHOLE run.** When the header's trailing comment
    /// is the head of a multi-line run, the continuation lines below it are
    /// part of the same logical comment; leaving them on the cursor split the
    /// run — and when the inline suite was its block's last statement, the
    /// orphan flush then saw a run member with no head.
    ///
    /// **The ANCHOR is recorded here, not at emission.** The comment belongs
    /// on the HEADER's output line, and by the time the body has rendered that
    /// line may no longer be the buffer's last one (a multi-line inline child
    /// — `if true: int x = match y:  # note` — ends on the BODY's last line).
    /// Recording `buf.len()` now and resolving it to that line's end later
    /// puts the comment where the author wrote it, and the result re-parses
    /// and is a fixpoint. Emitting at claim time instead is not an option: the
    /// body has not been written yet and would land AFTER the comment.
    fn claim_header_trailing_comments(&mut self, header_anchor_end: usize) -> HeaderClaim {
        let mut runs = Vec::new();
        let anchor = self.emitter.buf.len();
        while self.comment_cursor < self.comments.len() {
            let comment_start = self.comments[self.comment_cursor].hash_pos();
            if comment_start > self.source.len() {
                break;
            }
            let Some(content_end) = self.last_real_content_before(header_anchor_end) else {
                break;
            };
            if comment_start < content_end {
                break;
            }
            if self.source[content_end..comment_start].contains('\n') {
                break;
            }
            // CLAIM SITE 4 of 5 — the deferred header hook (inline suites and
            // inline arm bodies).
            runs.push(self.claim_run_at_cursor());
        }
        HeaderClaim { runs, anchor }
    }

    /// Emit comments claimed by [`claim_header_trailing_comments`], now that
    /// the inline suite body has been written. Mirrors the injection half of
    /// `emit_trailing_comment_after` — the canonical `TRAILING_COMMENT_GAP`
    /// gap, recorded for the aligner as a HEADER comment so it is never
    /// grouped with body lines — but splices onto the line the claim recorded
    /// rather than the buffer's current last one.
    fn emit_claimed_header_comments(&mut self, claim: HeaderClaim) {
        for run in claim.runs {
            self.emit_claimed_run(
                run,
                EmitPos::AtAnchor {
                    anchor: claim.anchor,
                    is_header: true,
                },
            );
        }
    }

    /// R39 snag #2 (`gorget-arena` snag #2 — durable repro at
    /// `tests/fixtures/known_gaps/gorget_arena_snag_2_fmt_trailing_comment_detach/`):
    /// after emitting a sibling item/stmt/field that ends at source
    /// position `prev_end`, flush any trailing comment(s) that shared
    /// the SAME SOURCE LINE as that previous emit. The comments stay
    /// attached to their owning node (inline, after a
    /// `TRAILING_COMMENT_GAP` visual gap) instead of drifting to lead the
    /// NEXT sibling — the exact bug the fixture pins.
    ///
    /// A comment qualifies as "trailing" iff:
    ///   1. it hasn't been emitted yet (cursor > it),
    ///   2. `comment.span.start >= prev_end` (comment follows prev in
    ///      source), and
    ///   3. `source[prev_end..comment.span.start]` contains no `\n`
    ///      (still on the same source line — no line break separates
    ///      them, only whitespace / punctuation).
    ///
    /// When those hold, the comment is injected inline via
    /// `Emitter::inject_before_newline` (which splices ahead of the
    /// trailing `\n` `format_stmt` left behind), and the comment cursor
    /// advances. The loop repeats because a single emit can own more than one
    /// comment — a MULTI-LINE construct whose trailing comments the walker
    /// reaches all at once.
    ///
    /// It is NOT what makes `stmt  # a  # b` work: a `#` comment runs to the
    /// end of the line, so the lexer records that as ONE `Comment` token whose
    /// text happens to contain a second `#` (verified: `gg lex` reports a
    /// single `Comment("# a  # b")`). The inner spacing is the author's, inside
    /// the comment, and is reproduced verbatim — only the gap BEFORE the first
    /// `#` is the formatter's to set. The same fact is what lets
    /// `plan_trailing_aligns` assume one alignment entry per output line.
    ///
    /// A comment that fails condition (3) is left for `emit_comments_before`
    /// on the next iteration — it will be emitted as a LEADING comment
    /// at the next sibling's indent, which is the correct behaviour for a
    /// standalone-line comment BETWEEN two siblings.
    fn emit_trailing_comment_after(&mut self, prev_end: usize, is_header: bool) {
        // The "trailing comment" question — is comment C a trailing
        // comment on the just-emitted node I? — reduces to:
        //
        //   "Is I's ACTUAL LAST EMITTED CHARACTER on the same source
        //    line as C's start?"
        //
        // `prev_end` is I's AST-recorded span end, which is UNRELIABLE
        // for multi-line items: it may sit at a `Dedent` position on
        // the NEXT construct's line (`struct.span.end` on the `enum`
        // header line), or MID-comment (parse_function_def extending
        // through a trailing comment via consume_newline). Both give a
        // `prev_end` that "runs past" the item's real content.
        //
        // Fix: compute an ANCHOR = the last SOURCE byte before
        // `prev_end` that is NEITHER whitespace NOR inside a comment
        // span. That byte is guaranteed to be part of the item's own
        // textual content (comments live in the sideband, whitespace is
        // just separator). Then C is a trailing on I iff there is no
        // `\n` in `source[anchor..C.start]` — i.e. C is on the same
        // source line as the item's last real byte.
        //
        // Load-bearing correctness cases (RED-verified by
        // `fmt_trailing_comment_axis_all_classes`):
        //   - `struct S: ... \n\nenum E:  # x` → struct.span.end sits
        //     on enum's line; anchor walks back over `\n\n` to the
        //     last field's identifier → `\n` in [anchor..#x] → break.
        //     The enum's OWN header hook then fires with a clean
        //     cursor.
        //   - `trait T:\n    void m()  # x\n    void n()` →
        //     item.span.end for `void m()` sits on line 3 (past `# x`);
        //     anchor walks back over `\n` + across comment `# x` (both
        //     skipped) → hits `)` at end of `m()` → no `\n` between
        //     `)` and `# x` → fire.
        while self.comment_cursor < self.comments.len() {
            let comment_start = self.comments[self.comment_cursor].hash_pos();
            if comment_start > self.source.len() {
                break;
            }
            // No real content precedes this position ⇒ there is no
            // just-emitted node for the comment to trail, so it cannot be a
            // trailing comment. Leave it to `emit_comments_before` /
            // `emit_remaining_comments`, which emit it as a LEADING comment
            // on its own line. (R41 T-FMT-A: the comment-only-file fix.)
            let Some(anchor) = self.last_real_content_before(prev_end) else {
                break;
            };
            if comment_start < anchor {
                // Comment predates the anchor — should have been
                // consumed by an earlier `emit_comments_before`; break
                // defensively rather than duplicate-emit.
                break;
            }
            let between = &self.source[anchor..comment_start];
            if between.contains('\n') {
                // A newline separates the item's last real byte from
                // the comment — the comment is on a later line, treat
                // as leading of the next sibling.
                break;
            }
            // Same-line trailing: inject inline with the canonical gap.
            //
            // CLAIM SITE 3 of 5 — the trailing-comment hook, and the one that
            // owns a multi-line run's HEAD. Claiming the run here is what
            // keeps its continuation lines with the member they annotate.
            let claim = self.claim_run_at_cursor();
            self.emit_claimed_run(claim, EmitPos::Inline { is_header });
        }
    }

    /// R40 owner-directed trailing-comment aligner (2026-08-10). Post-pass
    /// over the recorded `trailing_aligns` sideband + the emitted buffer:
    /// re-spaces the gap of each grouped trailing comment so a contiguous
    /// run lands its `#` at a common `next_multiple_of(STRIDE)` column.
    /// Pure planning lives in `plan_trailing_aligns` (reads `&str`); this
    /// half only splices the computed gaps into `self.emitter.buf`.
    ///
    /// Idempotent: the plan is a pure function of LHS widths + comment
    /// lengths, both stable across a re-run (the formatter re-emits the same
    /// LHS text and the same comment), so `fmt(fmt(x)) == fmt(x)`.
    fn align_trailing_comments(&mut self) {
        let entries = std::mem::take(&mut self.trailing_aligns);
        let rewrites = plan_trailing_aligns(&self.emitter.buf, &entries);
        // Already sorted LAST→FIRST; each gap is exactly the run of ASCII
        // spaces the writer injected at `[off, off + old_gap)` — the entry's
        // OWN width, since a continuation line's pad is its whole `#` column
        // rather than the constant (ASCII → valid char boundaries).
        for (off, old_gap, new_gap) in rewrites {
            self.emitter
                .buf
                .replace_range(off..off + old_gap, &" ".repeat(new_gap));
        }
    }

    /// Walk `self.source` backwards from `pos` and return the byte
    /// position AFTER the LAST byte that is NEITHER whitespace nor
    /// inside a comment span (both from `self.comments`). Returns `0`
    /// if the walk reaches the source start without hitting a real
    /// content byte.
    ///
    /// The two skips are load-bearing:
    ///   - **Whitespace** because `prev_end` for a multi-line item
    ///     sits on the NEXT construct's line, PAST intervening blank
    ///     lines; walking back over `\n`s + indent lands on the item's
    ///     own last content.
    ///   - **Comments** because some AST spans extend through trailing
    ///     comments via `consume_newline` (e.g. `parse_function_def`);
    ///     without skipping comment bytes, the anchor would land inside
    ///     the comment we're deciding about (or an earlier trailing
    ///     comment) rather than on the item's real code.
    ///
    /// O(n × m) worst case (source position × comment count) — small
    /// files this is fine; if it shows on the profiler for very
    /// comment-heavy inputs, replace the `iter().any` with a binary
    /// search over pre-sorted comment spans.
    /// Returns `None` when NO real content precedes `pos` — the source up to
    /// `pos` is nothing but whitespace and comments.
    ///
    /// R41 T-FMT-A: this used to return the sentinel `0`, which is
    /// indistinguishable from "real content ends at byte 0" — and that
    /// conflation is the whole comment-only-file corruption. In a file with
    /// no items, `format`'s EOF hook anchored at `0`, found `source[0..0]`
    /// empty (no `\n`), and therefore classified the file's FIRST comment as
    /// a *trailing* comment on a node that was never emitted: it was injected
    /// with the inline trailing-comment gap into an empty buffer, and the
    /// next comment then glued onto the same line. Making the
    /// "nothing precedes" case a distinct TYPE rather than a magic value
    /// forces every caller to handle it (Layering rule 2: typed metadata, not
    /// sentinel values).
    fn last_real_content_before(&self, pos: usize) -> Option<usize> {
        let bytes = self.source.as_bytes();
        let mut i = pos.min(bytes.len());
        while i > 0 {
            let idx = i - 1;
            let c = bytes[idx];
            if c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' {
                i -= 1;
                continue;
            }
            // Byte `idx` is inside a comment span iff some comment
            // covers `[span.start, span.end)` and `span.start <= idx <
            // span.end`.
            let in_comment = self
                .comments
                .iter()
                .any(|cm| cm.span.start <= idx && idx < cm.span.end);
            if in_comment {
                i -= 1;
                continue;
            }
            return Some(i);
        }
        None
    }

    // ── Module ──────────────────────────────────────────────

    fn format_module(&mut self, module: &Module) {
        // Partition items into leading directives, imports, and the rest.
        let mut directives: Vec<&Spanned<Item>> = Vec::new();
        let mut imports: Vec<&Spanned<Item>> = Vec::new();
        let mut rest: Vec<&Spanned<Item>> = Vec::new();
        let mut past_imports = false;

        for item in &module.items {
            match &item.node {
                Item::Directive(_) if !past_imports => directives.push(item),
                Item::Import(_) if !past_imports => imports.push(item),
                _ => {
                    past_imports = true;
                    rest.push(item);
                }
            }
        }

        // Sort imports: std/gg first, then third-party, alphabetically within groups.
        if !imports.is_empty() {
            imports.sort_by(|a, b| {
                let path_a = import_sort_key(a);
                let path_b = import_sort_key(b);
                let is_std_a = is_std_import(&path_a);
                let is_std_b = is_std_import(&path_b);
                // std/gg imports come first
                match (is_std_a, is_std_b) {
                    (true, false) => std::cmp::Ordering::Less,
                    (false, true) => std::cmp::Ordering::Greater,
                    _ => path_a.cmp(&path_b),
                }
            });
        }

        // Emit directives.
        // Trailing-hook placement (R39 snag #2): `emit_comments_before`
        // BEFORE `format_item` flushes any standalone leading comments;
        // `emit_trailing_comment_after(item.span.end)` AFTER `format_item`
        // splices any comment that shared the item's LAST source line
        // ahead of the newline `format_item` left behind. This
        // sequencing (leading → item → trailing) is the same at all 12
        // sibling-loop sites — see `emit_trailing_comment_after` for the
        // full same-line semantics.
        let mut emitted = 0;
        for item in &directives {
            if emitted > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            self.emit_trailing_comment_after(item.span.end, false);
            emitted += 1;
        }

        // Emit sorted imports. Blank line only on group transition (std vs
        // non-std), not between every import — the every-import-blank shape
        // inflates line counts ~30% on import-heavy files (owner 2026-08-09,
        // gorget-arena verdict). Group-with-single-blank-between-groups is
        // the canonical shape (mirror gofmt's behavior).
        let mut prev_import_is_std: Option<bool> = None;
        let mut first_import = true;
        for item in &imports {
            let cur_is_std = is_std_import(&import_sort_key(item));
            let need_blank = if first_import {
                // First import iter: blank iff prior section (directives) emitted.
                emitted > 0
            } else {
                // Subsequent iters: blank iff group transition.
                prev_import_is_std != Some(cur_is_std)
            };
            if need_blank {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            self.emit_trailing_comment_after(item.span.end, false);
            emitted += 1;
            prev_import_is_std = Some(cur_is_std);
            first_import = false;
        }

        // Emit remaining items.
        // gorget-arena verdict follow-up (owner 2026-08-09): preserve
        // AUTHOR-written blank lines between top-level items instead of
        // blindly emitting one between every item. Same rule as intra-
        // block (`format_block_stmts`): if source has ≥ 2 newlines
        // between prev and cur, emit ONE blank; else no blank. Collapse-
        // runs-to-1 semantics. First iter: blank iff prior section
        // (directives/imports) emitted.
        for (i, item) in rest.iter().enumerate() {
            let need_blank = if i == 0 {
                emitted > 0
            } else {
                self.has_blank_line_between(rest[i - 1].span.end, item.span.start)
            };
            if need_blank {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            self.emit_trailing_comment_after(item.span.end, false);
            emitted += 1;
        }
        // LEAK GUARD (Core #14 — the invariant gets an enforcer, not a
        // comment). Every `with_tail_reserve` / `with_exact_tail_reserve`
        // restores on exit, so a non-zero reserve here means some scope was
        // left open — which would silently over-reserve every later line in
        // the module rather than fail.
        debug_assert_eq!(
            self.tail_reserve, 0,
            "tail reserve leaked out of a `with_tail_reserve` scope"
        );
    }

    // ── Items ───────────────────────────────────────────────

    /// THE producer for a NESTED item sequence — every `meta if` / `elif` /
    /// `else` block body.
    ///
    /// A blank line between two definitions is paragraphing, and it does not
    /// stop being paragraphing one level in. `format_module` preserves it
    /// between TOP-LEVEL items and `format_block_stmts` preserves it between
    /// statements; the nested-item loops were the one member of that family
    /// that did not, so an entire platform backend wrapped in
    /// `meta if platform() == "macos":` came back with every definition
    /// welded to the next.
    ///
    /// Same rule as both siblings — a blank iff the author left one, runs
    /// collapse to one, and the FIRST item never gets one (a blank between a
    /// block opener and its first item is sparseness, not paragraphing). Three
    /// call sites route through here rather than repeating the loop, so a
    /// fourth nested-item block cannot quietly ship without it;
    /// `formatter_child_collection_loop_census` (tests/lints.rs) carries the row
    /// that makes it a guarantee rather than a habit — a new loop with its own
    /// copy of the hooks shows up there and has to be classified.
    /// `header_start` anchors the branch's own clause line; `block_end` is the
    /// NEXT clause's header (or the whole `meta if`'s end for the last branch)
    /// — a per-branch ceiling, because with one ceiling for all three the
    /// then-branch's flush would swallow the elif branch's leading comment and
    /// document the wrong branch.
    fn format_nested_items(
        &mut self,
        items: &[Spanned<Item>],
        header_start: usize,
        block_end: usize,
    ) {
        for (i, item) in items.iter().enumerate() {
            if i > 0 && self.has_blank_line_between(items[i - 1].span.end, item.span.start) {
                self.emitter.blank_line();
            }
            // R39 snag #2: trailing-hook after each nested item — same
            // pairing as the `format_module` loops.
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            self.emit_trailing_comment_after(item.span.end, false);
        }
        // NESTED-ITEMS flush — the item-level `meta if` branch bodies are the
        // third face of the tail-orphan class: one producer, three arms.
        self.emit_orphan_comments_before_close(header_start, block_end);
    }

    fn format_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Function(f) => self.format_function(f),
            Item::Struct(s) => self.format_struct(s),
            Item::Enum(e) => self.format_enum(e),
            Item::Trait(t) => self.format_trait(t),
            Item::Equip(e) => self.format_equip(e),
            Item::Import(i) => self.format_import(i),
            Item::TypeAlias(ta) => self.format_type_alias(ta),
            Item::Newtype(nt) => self.format_newtype(nt),
            Item::ConstDecl(cd) => self.format_const_decl(cd),
            Item::StaticDecl(sd) => self.format_static_decl(sd),
            Item::ExternBlock(eb) => self.format_extern_block(eb),
            Item::Directive(d) => {
                self.emitter.write("directive ");
                self.emitter.write(&d.name);
                if let Some(ref val) = d.value {
                    self.emitter.write("=");
                    self.emitter.write(val);
                }
                self.emitter.newline();
            }
            Item::Test(t) => self.format_test(t),
            Item::Bench(b) => self.format_bench(b),
            Item::SuiteSetup(s) => self.format_suite_setup(s),
            Item::SuiteTeardown(s) => self.format_suite_teardown(s),
            Item::MetaConst(mc) => {
                self.emitter.write("meta ");
                self.format_type(&mc.type_);
                self.emitter.write(" ");
                self.emitter.write(&mc.name.node);
                self.emitter.write(" = ");
                self.format_expr(&mc.value);
                self.emitter.newline();
            }
            Item::MetaType(mt) => {
                self.emitter.write("meta type ");
                self.emitter.write(&mt.name.node);
                self.emitter.write(" = ");
                match &mt.rhs {
                    MetaTypeRhs::Plain(t) => self.format_type(t),
                    MetaTypeRhs::Conditional { then_type, condition, else_type } => {
                        // Family O — the meta-conditional type arms: operator
                        // text (` if ` / ` else `) written BETWEEN renders.
                        let then_tail = self.measured_reserve(|s| {
                            s.emitter.write(" if ");
                            s.format_expr(condition);
                        });
                        self.with_tail_reserve(then_tail, |s| s.format_type(then_type));
                        self.emitter.write(" if ");
                        let cond_tail = self.measured_reserve(|s| {
                            s.emitter.write(" else ");
                            s.format_type(else_type);
                        });
                        self.with_tail_reserve(cond_tail, |s| s.format_expr(condition));
                        self.emitter.write(" else ");
                        self.format_type(else_type);
                    }
                    MetaTypeRhs::Call { callee, args } => {
                        self.emitter.write(&callee.node);
                        self.emitter.write("(");
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 { self.emitter.write(", "); }
                            self.format_expr(arg);
                        }
                        self.emitter.write(")");
                    }
                }
                self.emitter.newline();
            }
            Item::MetaTypeFunc(mtf) => {
                self.emitter.write("meta type ");
                self.emitter.write(&mtf.name.node);
                self.emitter.write("(");
                for (i, p) in mtf.params.iter().enumerate() {
                    if i > 0 { self.emitter.write(", "); }
                    // B10 — `Item::MetaTypeFunc` is a RESERVED row, not the
                    // reserve-0 one an earlier reading assumed: its param loop
                    // reaches `write_doc` through `format_param` →
                    // `format_type` → `format_generic_args_wrapped`. The loop
                    // is hand-rolled, so each param is its OWN width-decided
                    // render with its own tail.
                    let rest = &mtf.params[i + 1..];
                    let tail = self.measured_reserve(|s| {
                        for q in rest {
                            s.emitter.write(", ");
                            s.format_param(&q.node);
                        }
                        s.emitter.write("):");
                    });
                    self.with_tail_reserve(tail, |s| s.format_param(&p.node));
                }
                self.emitter.write("):");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(&mtf.body);
                self.emitter.dedent();
            }
            Item::MetaAssert(ma) => {
                self.emitter.write("meta assert ");
                self.format_expr(&ma.condition);
                if let Some(ref msg) = ma.message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Item::MetaIf(mi) => {
                // The ITEM-level twin of the statement-level clause family, and
                // it had the same two holes: a trailing comment on any of the
                // three headers fell through to the branch body (where the
                // nested-item loop re-emitted it as a LEADING comment on the
                // first definition), and an author blank above `elif:`/`else:`
                // was deleted because a clause header is not an item either.
                self.emitter.write("meta if ");
                // B10 — `Item::MetaIf`, FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(&mi.condition));
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(mi.condition.span.end);
                self.emitter.newline();
                self.emitter.indent();
                // Tight per-branch ceiling: the NEXT clause's header, so a
                // leading comment of the next branch's first item is not
                // swallowed by this one.
                let then_end = mi
                    .elif_branches
                    .first()
                    .map(|(c, _)| c.span.start)
                    .or(mi.else_branch.as_ref().map(|(k, _)| k.start))
                    .unwrap_or(mi.span.end);
                self.format_nested_items(&mi.then_items, mi.condition.span.start, then_end);
                self.emitter.dedent();
                for (bi, (cond, items)) in mi.elif_branches.iter().enumerate() {
                    // Blank, then leading comments, then the header — the order
                    // every clause site keeps.
                    if self.blank_before_clause(cond.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(cond.span.start);
                    self.emitter.write("elif ");
                    // B10 — the item-level `meta elif` twin, FIXED 1.
                    self.with_tail_reserve(1, |s| s.format_expr(cond));
                    self.emitter.write(":");
                    self.emit_trailing_comment_after_header(cond.span.end);
                    self.emitter.newline();
                    self.emitter.indent();
                    let elif_end = mi
                        .elif_branches
                        .get(bi + 1)
                        .map(|(c, _)| c.span.start)
                        .or(mi.else_branch.as_ref().map(|(k, _)| k.start))
                        .unwrap_or(mi.span.end);
                    self.format_nested_items(items, cond.span.start, elif_end);
                    self.emitter.dedent();
                }
                if let Some((else_kw, ref else_items)) = mi.else_branch {
                    // The clause's own position, WRITTEN by the parser
                    // (`MetaIf.else_branch`). This used to walk backwards from
                    // the first item to find the colon, which worked but
                    // reconstructed at the read site a fact the writer had in
                    // hand — and the first item's start is NOT usable directly,
                    // since it sits one line below and makes every author blank
                    // above the clause read as absent.
                    //
                    // The keyword's END, not its start: both hooks walk BACK
                    // from the anchor to the last real content, so an anchor at
                    // the `e` of `else` lands them on the PREVIOUS line and the
                    // trailing comment after the colon reads as a line away.
                    let anchor = else_kw.end;
                    if self.blank_before_clause(anchor) {
                        self.emitter.blank_line();
                    }
                    // A comment on its own line above `else:` documents the
                    // BRANCH; without this it fell to the nested-item loop's
                    // flush and was re-emitted INSIDE the branch, leading the
                    // first definition.
                    self.emit_comments_before(anchor);
                    self.emit_else_header(anchor);
                    self.emitter.indent();
                    self.format_nested_items(else_items, else_kw.start, mi.span.end);
                    self.emitter.dedent();
                }
            }
            Item::MetaLog(ml) => {
                self.emitter.write("meta log ");
                for (i, arg) in ml.args.iter().enumerate() {
                    if i > 0 { self.emitter.write(", "); }
                    self.format_expr(arg);
                }
                self.emitter.newline();
            }
            Item::Module { items, .. } => {
                for inner in items {
                    self.format_item(inner);
                }
            }
        }
    }

    fn format_test(&mut self, t: &TestDef) {
        self.format_doc_comment(&t.doc_comment);
        self.format_attributes(&t.attributes);
        self.emitter.write("test ");
        self.emit_quoted_string(&t.name.node, Some(t.name.span));
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&t.body);
        self.emitter.dedent();
    }

    fn format_bench(&mut self, b: &BenchDef) {
        self.format_doc_comment(&b.doc_comment);
        self.format_attributes(&b.attributes);
        self.emitter.write("bench ");
        self.emit_quoted_string(&b.name.node, Some(b.name.span));
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&b.body);
        self.emitter.dedent();
    }

    fn format_suite_setup(&mut self, s: &SuiteSetup) {
        self.emitter.write("suite setup:");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&s.body);
        self.emitter.dedent();
    }

    fn format_suite_teardown(&mut self, s: &SuiteTeardown) {
        self.emitter.write("suite teardown:");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&s.body);
        self.emitter.dedent();
    }

    fn format_doc_comment(&mut self, doc: &Option<String>) {
        if let Some(doc) = doc {
            for line in doc.lines() {
                self.emitter.write(line);
                self.emitter.newline();
            }
        }
    }

    fn format_attributes(&mut self, attrs: &[Spanned<Attribute>]) {
        for attr in attrs {
            self.emitter.write("@");
            self.emitter.write(&attr.node.name.node);
            if !attr.node.args.is_empty() {
                self.emitter.write("(");
                for (i, arg) in attr.node.args.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    match arg {
                        AttributeArg::Identifier(s) => self.emitter.write(s),
                        AttributeArg::StringLiteral(s) => {
                            self.emit_quoted_string(&s.node, Some(s.span));
                        }
                        AttributeArg::KeyValue(k, v) => {
                            self.emitter.write(k);
                            self.emitter.write(" = ");
                            // The two producers are NOT interchangeable: a
                            // bare identifier stays bare. Quoting it (which
                            // this arm used to do unconditionally) invents a
                            // string the author did not write — and the
                            // result re-parses cleanly as the OTHER producer,
                            // so no round-trip gate could ever see it.
                            match v {
                                AttributeArgValue::Ident(name) => {
                                    self.emitter.write(&name.node);
                                }
                                AttributeArgValue::Str(s) => {
                                    self.emit_quoted_string(&s.node, Some(s.span));
                                }
                            }
                        }
                    }
                }
                self.emitter.write(")");
            }
            self.emitter.newline();
        }
    }

    /// Emit the visibility keyword IFF the author wrote one.
    ///
    /// Both `public Foo` and a bare `Foo` parse to `Visibility::Public`, so
    /// the value alone cannot tell the two apart — which is why this used to
    /// emit a keyword only for `Private` and silently DELETED every explicit
    /// `public` in the tree. Emitting one unconditionally is equally wrong in
    /// the other direction: it would rewrite every declaration that relies on
    /// the default. The parser records which spelling it consumed; this reads
    /// that fact and nothing else. (Statics have their own path — they are
    /// private by default, the opposite convention.)
    fn format_visibility(&mut self, vis: &Visibility, explicit: bool) {
        if !explicit {
            return;
        }
        match vis {
            Visibility::Private => self.emitter.write("private "),
            Visibility::Public => self.emitter.write("public "),
        }
    }

    fn format_function(&mut self, f: &FunctionDef) {
        self.format_doc_comment(&f.doc_comment);
        self.format_attributes(&f.attributes);
        self.format_visibility(&f.visibility, f.explicit_visibility);
        if matches!(f.body, FunctionBody::Extern(_)) {
            self.emitter.write("extern ");
        }
        // R41 T-FMT-A (silent-drop class, 2nd episode after R39's
        // `blocking`/`noreturn`): emit the extern ABI tag and the
        // `borrowed` return marker. Both are FunctionDef facts that the
        // formatter used to drop on the floor, and both are load-bearing:
        //
        //   - `extern_abi == Some("C")` selects `AbiKind::CStr` over
        //     `GorgetString` at lowering (`ir/lowering/mod.rs`), so a
        //     dropped tag re-marshals a `String` param as a by-value
        //     32-byte `Str` struct into a `const char*` — silent UB, live
        //     across 9 `lib/std` declarations. It also gates the `cstr`
        //     type's legality, so dropping it can even make the output
        //     fail to re-parse.
        //   - `returns_borrowed` says the FFI returned pointer is NOT owned,
        //     so the caller must clone at the ownership boundary.
        //
        // Order mirrors `parse_function_def` exactly — `extern` → ABI
        // string → qualifier loop → `borrowed` → return type — so the
        // emission re-parses to the same AST.
        //
        // ONE call site for each, serving BOTH extern forms. Items of an
        // `extern "C":` BLOCK carry `extern_abi = None` (the tag lives on
        // `ExternBlock` and is emitted by `format_extern_block`), so this
        // cannot double-emit the block header; `returns_borrowed` however
        // IS per-item in both forms, which is why the block form dropped it.
        if let Some(ref abi) = f.extern_abi {
            self.emit_quoted_string(&abi.node, Some(abi.span));
            self.emitter.write(" ");
        }
        self.format_qualifiers(&f.qualifiers);
        if f.returns_borrowed {
            self.emitter.write("borrowed ");
        }
        // Window rule (`delim_pos_after`): anchor PAST the generic-PARAMS
        // region when the function is generic — a generic param's default
        // or bound can otherwise sit between the name and the `(`.
        let anchor = f.generic_params.as_ref().map_or(f.name.span.end, |gp| gp.span.end);
        let params_gate = self.gate_or_scan_miss(self.paren_tuple_gate(
            anchor,
            f.span.end,
            f.params.first().map(|p| p.span.start),
            f.params.last().map(|p| p.span.end),
        ));

        // ── R42 tail reserves (rows A1, A2, and A10's fn-return-type cell) ──
        //
        // Every width-decided render in this header has caller-emitted text
        // still to come on the same line. Each reserve is installed
        // IMMEDIATELY around the render it charges (the scope-tightness rule),
        // and each one measures the caller's tail up to the NEXT render's
        // first break opportunity — see `doc::Renderer::tail_reserve`.
        //
        // A1 — the parameter list: the throws clause plus the body opener,
        // which for an expression body carries on into the expression's own
        // leading unbreakable text. Residual, NAMED rather than implied: an
        // expression body whose leading unbreakable text alone exceeds the
        // remaining budget still overruns — no break can save that line.
        let header_tail = self.measured_reserve(|s| s.format_function_header_tail(f));
        // A2 — the generic parameter list. EXACT, not approximated: when the
        // value-parameter list is non-empty its own Fill fit-tests right after
        // the `(`, so the tail is that one character; when it is empty
        // `format_params_wrapped` short-circuits to the literal `()` with no
        // fit test anywhere, so the tail runs on through it to A1's.
        let generic_params_tail = if f.params.is_empty() {
            2 + header_tail
        } else {
            1
        };
        // A10 — the return type. Its tail is ` name` plus the leading literal
        // of whichever list comes next (`[` of the generic params, `(` of the
        // value params, or `()` and onward when both are empty).
        let return_tail = self.measured_reserve(|s| {
            s.emitter.write(" ");
            s.emitter.write(&f.name.node);
            if let Some(ref gp) = f.generic_params {
                s.format_generic_params_wrapped(gp);
            }
            s.format_params_wrapped(&f.params, params_gate);
            s.format_function_header_tail(f);
        });

        // type-first: `ReturnType name(params)`
        // Bare tuple return: emit `T1, T2` not `(T1, T2)` in return position
        if let Type::Tuple(types) = &f.return_type.node {
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(", ");
                }
                // A bare tuple return is emitted member-by-member, so each
                // member's tail is the rest of the tuple plus `return_tail`.
                let rest = &types[i + 1..];
                let member_tail = return_tail
                    + self.measured_reserve(|s| {
                        for t in rest {
                            s.emitter.write(", ");
                            s.format_type(t);
                        }
                    });
                self.with_tail_reserve(member_tail, |s| s.format_type(ty));
            }
        } else {
            self.with_tail_reserve(return_tail, |s| s.format_type(&f.return_type));
        }
        self.emitter.write(" ");
        self.emitter.write(&f.name.node);
        if let Some(ref gp) = f.generic_params {
            self.with_tail_reserve(generic_params_tail, |s| {
                s.format_generic_params_wrapped(gp)
            });
        }
        self.with_tail_reserve(header_tail, |s| {
            s.format_params_wrapped(&f.params, params_gate)
        });
        self.format_function_header_tail(f);
        match &f.body {
            FunctionBody::Block(block) => {
                // R39 gorget-arena verdict follow-up (owner 2026-08-09):
                // preserve `int f(int x): # doc` trailing comment on the
                // function-header line — same class as R39 block-header
                // trailing (if/while/for/match/case) + snag #2 residual
                // container-header (struct/enum/trait/equip). Anchor at
                // f.name.span.end (before `:` and any trailing comment
                // on the same source line).
                self.emit_trailing_comment_after_header(f.name.span.end);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(block);
                self.emitter.dedent();
            }
            _ => self.emitter.newline(),
        }
    }

    /// Everything a function header writes AFTER its parameter list's `)`, on
    /// the same line: the throws spec, the body opener (`:` / `: ` / ` = "…"`),
    /// and — for an expression body — the expression itself, which continues on
    /// that line.
    ///
    /// ONE spelling, shared by the real emission and by A1's reserve
    /// measurement, so the two cannot drift. Writes no newline and takes no
    /// indent; the block body's own lines stay with the caller.
    fn format_function_header_tail(&mut self, f: &FunctionDef) {
        match &f.throws {
            ThrowsSpec::Explicit(throws) => {
                self.emitter.write(" throws ");
                self.format_type(throws);
            }
            // D29/A31 bare `!` inferred-error-set signature (`int f()!:`).
            ThrowsSpec::Inferred(_) => self.emitter.write("!"),
            ThrowsSpec::No => {}
        }
        match &f.body {
            FunctionBody::Block(_) => self.emitter.write(":"),
            FunctionBody::Expression(expr) => {
                self.emitter.write(": ");
                self.format_expr(expr);
            }
            FunctionBody::Declaration => {}
            FunctionBody::Extern(sym) => {
                self.emitter.write(" = ");
                self.emit_quoted_string(&sym.node, Some(sym.span));
            }
        }
    }

    fn format_qualifiers(&mut self, q: &FunctionQualifiers) {
        if q.is_async {
            self.emitter.write("async ");
        }
        if q.is_const {
            self.emitter.write("const ");
        }
        if q.is_static {
            self.emitter.write("static ");
        }
        if q.is_unsafe {
            self.emitter.write("unsafe ");
        }
        // R39 Track A output-review finding (filed 2026-08-09): the `blocking`
        // and `noreturn` extern qualifiers were dropped by `gg fmt`. Both are
        // load-bearing at lowering (`src/ir/lowering/mod.rs` — is_blocking
        // gates the shared_async lock release/reacquire transform; is_noreturn
        // makes the call type as Never + terminates the block with unreachable),
        // so silently stripping them mis-lowered `blocking`/`noreturn` externs
        // after a fmt sweep. Same class as gorget-js snag #15b (fmt silently
        // dropping user syntax at the arm-body position).
        if q.is_blocking {
            self.emitter.write("blocking ");
        }
        if q.is_noreturn {
            self.emitter.write("noreturn ");
        }
    }

    fn format_struct(&mut self, s: &StructDef) {
        self.format_doc_comment(&s.doc_comment);
        self.format_attributes(&s.attributes);
        self.format_visibility(&s.visibility, s.explicit_visibility);
        self.emitter.write("struct ");
        self.emitter.write(&s.name.node);
        if let Some(ref gp) = s.generic_params {
            // A3 — FIXED 1: the header's `:`.
            self.with_tail_reserve(1, |f| f.format_generic_params_wrapped(gp));
        }
        self.emitter.write(":");
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `struct S:  # header` shape —
        // header trailing comment stays on the header line, not
        // dedented into the body.
        // The header's `:`, not the container NAME: on a MULTI-LINE header
        // (`struct S[\n    T\n]:  # x`) the name is on an earlier source line,
        // so a same-line test against it rejects the header's own comment and
        // drops it into the body as a leading comment of the first member.
        self.emit_trailing_comment_after_header(s.header_colon_span.end);
        self.emitter.indent();
        // R37 empty-body chip: an empty struct body must emit `pass` so the
        // reformatted source PARSES. The parser rejects `struct X:` with no
        // indented body (`expected INDENT, got 'struct'` at the next line),
        // which broke bootstrap on `lib/std/collections.gg` (`struct Vector[T]: pass`
        // → `struct Vector[T]:` → parse error). Same class as `format_equip`
        // (:718) which already emits `pass` for empty item lists — extended
        // here to struct/enum/trait for the empty-container class.
        if s.fields.is_empty() {
            self.emitter.write("pass");
            self.emitter.newline();
        } else {
            for (i, field) in s.fields.iter().enumerate() {
                // gorget-arena snag #3: preserve author-written blank
                // lines between struct fields (paragraphing).
                if i > 0
                    && self.has_blank_line_between(s.fields[i - 1].span.end, field.span.start)
                {
                    self.emitter.blank_line();
                }
                self.emit_comments_before(field.span.start);
                // The struct-FIELD visibility write — the third emit path,
                // and the one that used to delete an author's `public float x`
                // because `Public` is the parsed default here too.
                self.format_visibility(
                    &field.node.visibility,
                    field.node.explicit_visibility,
                );
                // type-first: `type name`
                self.format_type(&field.node.type_);
                self.emitter.write(" ");
                self.emitter.write(&field.node.name.node);
                self.emitter.newline();
                // R39 snag #2: trailing-hook for `int x  # doc` on a
                // struct field. field.span.end sits at the end of the
                // identifier token (see parse_struct_body's
                // field_end = self.previous_span() after
                // expect_identifier), so a same-source-line comment
                // that follows the name is spliced inline.
                self.emit_trailing_comment_after(field.span.end, false);
            }
        }
        // CONTAINER flush — a comment after the last field.
        self.emit_orphan_comments_before_close(s.span.start, s.span.end);
        self.emitter.dedent();
    }

    /// An enum variant's tuple-field list.
    ///
    /// A HAND-ROLLED comma loop, not a fill-emitted list: each field type is
    /// its own width-decided render, charged for the rest of the list plus
    /// the `)`. ⚠ FEATURE GAP, named rather than silently inherited: a
    /// `Type::Named` with no generic args produces NO Doc at all, so a wide
    /// variant has no fit test anywhere and cannot break — the same
    /// no-Doc-layer shape as `format_pattern` and the var-decl initializer.
    /// Filed in `TODO.md`; a reserve cannot close it, only a Doc layer can.
    ///
    /// Factored out of the variant loop so the loop's leading/trailing comment
    /// hooks stay inside `formatter_child_collection_loop_census`'s window.
    fn format_variant_tuple_fields(&mut self, types: &[Spanned<Type>]) {
        self.emitter.write("(");
        for (i, ty) in types.iter().enumerate() {
            if i > 0 {
                self.emitter.write(", ");
            }
            let rest = &types[i + 1..];
            let tail = self.measured_reserve(|s| {
                for t in rest {
                    s.emitter.write(", ");
                    s.format_type(t);
                }
                s.emitter.write(")");
            });
            self.with_tail_reserve(tail, |s| s.format_type(ty));
        }
        self.emitter.write(")");
    }

    fn format_enum(&mut self, e: &EnumDef) {
        self.format_doc_comment(&e.doc_comment);
        self.format_attributes(&e.attributes);
        self.format_visibility(&e.visibility, e.explicit_visibility);
        self.emitter.write("enum ");
        self.emitter.write(&e.name.node);
        if let Some(ref gp) = e.generic_params {
            // A4 — FIXED 1: the header's `:`.
            self.with_tail_reserve(1, |f| f.format_generic_params_wrapped(gp));
        }
        self.emitter.write(":");
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `enum E:  # header` shape.
        // The header's `:`, not the container NAME: on a MULTI-LINE header
        // (`struct S[\n    T\n]:  # x`) the name is on an earlier source line,
        // so a same-line test against it rejects the header's own comment and
        // drops it into the body as a leading comment of the first member.
        self.emit_trailing_comment_after_header(e.header_colon_span.end);
        self.emitter.indent();
        // R37 empty-body chip: mirror format_struct — an empty enum body
        // must emit `pass` so the reformatted source parses.
        if e.variants.is_empty() {
            self.emitter.write("pass");
            self.emitter.newline();
        } else {
            for (i, variant) in e.variants.iter().enumerate() {
                // gorget-arena snag #3: preserve author-written blank
                // lines between enum variants (paragraphing).
                if i > 0
                    && self
                        .has_blank_line_between(e.variants[i - 1].span.end, variant.span.start)
                {
                    self.emitter.blank_line();
                }
                self.emit_comments_before(variant.span.start);
                self.emitter.write(&variant.node.name.node);
                match &variant.node.fields {
                    VariantFields::Unit => {}
                    VariantFields::Tuple(types) => self.format_variant_tuple_fields(types),
                }
                self.emitter.newline();
                // R39 snag #2: trailing-hook for `Variant()  # doc`
                // on enum variants — same shape as struct fields.
                self.emit_trailing_comment_after(variant.span.end, false);
            }
        }
        // CONTAINER flush — a comment after the last variant.
        self.emit_orphan_comments_before_close(e.span.start, e.span.end);
        self.emitter.dedent();
    }

    /// A trait header's tail after the generic-parameter list: the `extends`
    /// supertrait list and the closing `:`. ONE spelling, shared by the
    /// emission and by A5's reserve measurement.
    fn format_trait_extends_and_colon(&mut self, t: &TraitDef) {
        if !t.extends.is_empty() {
            self.emitter.write(" extends ");
            for (i, bound) in t.extends.iter().enumerate() {
                if i > 0 {
                    // Parser consumes `&` between supertrait names
                    // (parse_trait_bound_list); emit the same so fmt
                    // round-trips.
                    self.emitter.write(" & ");
                }
                let rest = &t.extends[i + 1..];
                let tail = self.measured_reserve(|s| {
                    for b in rest {
                        s.emitter.write(" & ");
                        s.format_trait_bound(b);
                    }
                    s.emitter.write(":");
                });
                self.with_tail_reserve(tail, |s| s.format_trait_bound(bound));
            }
        }
        self.emitter.write(":");
    }

    fn format_trait(&mut self, t: &TraitDef) {
        self.format_doc_comment(&t.doc_comment);
        self.format_attributes(&t.attributes);
        self.format_visibility(&t.visibility, t.explicit_visibility);
        self.emitter.write("trait ");
        self.emitter.write(&t.name.node);
        if let Some(ref gp) = t.generic_params {
            // A5 — MEASURED: the `extends` clause's leading unbreakable text
            // (or, with no supertraits, just the `:`).
            let tail = self.measured_reserve(|s| s.format_trait_extends_and_colon(t));
            self.with_tail_reserve(tail, |f| f.format_generic_params_wrapped(gp));
        }
        self.format_trait_extends_and_colon(t);
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `trait T:  # header` / `trait T extends A:  # x`
        // — for the same-line-header shape, `t.name.span.end` is on the
        // same source line as the `:`, so the helper's newline scan
        // covers `extends`-list variants without extra tracking.
        // The header's `:`, not the container NAME: on a MULTI-LINE header
        // (`struct S[\n    T\n]:  # x`) the name is on an earlier source line,
        // so a same-line test against it rejects the header's own comment and
        // drops it into the body as a leading comment of the first member.
        self.emit_trailing_comment_after_header(t.header_colon_span.end);
        self.emitter.indent();
        // R37 empty-body chip: mirror format_struct — an empty trait body
        // must emit `pass` so the reformatted source parses.
        if t.items.is_empty() {
            self.emitter.write("pass");
            self.emitter.newline();
        } else {
            for (i, item) in t.items.iter().enumerate() {
                if i > 0 {
                    self.emitter.blank_line();
                }
                self.emit_comments_before(item.span.start);
                match &item.node {
                    TraitItem::Method(f) => self.format_function(f),
                    TraitItem::AssociatedType(at) => {
                        self.emitter.write("type ");
                        self.emitter.write(&at.name.node);
                        if !at.bounds.is_empty() {
                            self.emitter.write(": ");
                            for (i, bound) in at.bounds.iter().enumerate() {
                                if i > 0 {
                                    self.emitter.write(" & ");
                                }
                                // B10-adjacent: `TraitItem::AssociatedType`
                                // reaches `write_doc` through
                                // `format_trait_bound` → `format_type`, so it
                                // is a RESERVED row, not a reserve-0 one.
                                let rest = &at.bounds[i + 1..];
                                let tail = self.measured_reserve(|s| {
                                    for b in rest {
                                        s.emitter.write(" & ");
                                        s.format_trait_bound(b);
                                    }
                                    if let Some(ref d) = at.default {
                                        s.emitter.write(" = ");
                                        s.format_type(d);
                                    }
                                });
                                self.with_tail_reserve(tail, |s| s.format_trait_bound(bound));
                            }
                        }
                        if let Some(ref default) = at.default {
                            self.emitter.write(" = ");
                            self.format_type(default);
                        }
                        self.emitter.newline();
                    }
                }
                // R39 snag #2: trailing-hook for `void m()  # doc` on a
                // trait item (method or associated type). Same-line
                // comments after the item's last identifier land inline
                // instead of leading the next item.
                self.emit_trailing_comment_after(item.span.end, false);
            }
        }
        // CONTAINER flush — a comment after the last trait item.
        self.emit_orphan_comments_before_close(t.span.start, t.span.end);
        self.emitter.dedent();
    }

    fn format_equip(&mut self, e: &EquipBlock) {
        self.emitter.write("equip ");
        // A6 — three width-decided renders in one header (`[T]`, the equipped
        // type, the `with` trait), each charged for exactly what still follows
        // it up to the next render's first break opportunity.
        let via_and_colon = |s: &mut Formatter| {
            if let Some(ref via) = e.via_field {
                s.emitter.write(" via ");
                s.emitter.write(&via.node);
            }
            s.emitter.write(":");
        };
        let with_onward = |s: &mut Formatter| {
            if let Some(ref trait_) = e.trait_ {
                s.emitter.write(" with ");
                s.format_type(&trait_.trait_name);
            }
            via_and_colon(s);
        };
        if let Some(ref gp) = e.generic_params {
            let tail = self.measured_reserve(|s| {
                s.format_type(&e.type_);
                with_onward(s);
            });
            self.with_tail_reserve(tail, |f| f.format_generic_params_wrapped(gp));
        }
        let type_tail = self.measured_reserve(with_onward);
        self.with_tail_reserve(type_tail, |s| s.format_type(&e.type_));
        if let Some(ref trait_) = e.trait_ {
            self.emitter.write(" with ");
            let trait_tail = self.measured_reserve(via_and_colon);
            self.with_tail_reserve(trait_tail, |s| s.format_type(&trait_.trait_name));
        }
        if let Some(ref via) = e.via_field {
            self.emitter.write(" via ");
            self.emitter.write(&via.node);
        }
        self.emitter.write(":");
        self.emitter.newline();
        // `equip S:  # x`, `equip S with T:  # x`, `equip S via f:  # x` —
        // anchor on the header's own `:`, which is on the header's LAST
        // source line whatever the shape (a wrapped `[T]` list, a `with T`
        // tail). The colon is optional in the blank form; that form has no
        // body and therefore no header line to trail, so the equipped TYPE's
        // end is the fallback.
        let equip_anchor = e
            .header_colon_span
            .map_or(e.type_.span.end, |c| c.end);
        self.emit_trailing_comment_after_header(equip_anchor);
        self.emitter.indent();
        if e.items.is_empty() {
            self.emitter.write("pass");
            self.emitter.newline();
        } else {
            for (i, method) in e.items.iter().enumerate() {
                if i > 0 {
                    self.emitter.blank_line();
                }
                self.emit_comments_before(method.span.start);
                self.format_function(&method.node);
                // R39 snag #2: trailing-hook for methods inside an
                // `equip … with T:` block. Same class as trait items.
                self.emit_trailing_comment_after(method.span.end, false);
            }
        }
        // CONTAINER flush — a comment after the last method.
        self.emit_orphan_comments_before_close(e.span.start, e.span.end);
        self.emitter.dedent();
    }

    fn format_import(&mut self, i: &ImportStmt) {
        match i {
            ImportStmt::Simple { path, .. } => {
                self.emitter.write("import ");
                self.format_dotted_path(path);
                self.emitter.newline();
            }
            ImportStmt::Grouped { path, names, .. } => {
                self.emitter.write("import ");
                self.format_dotted_path(path);
                self.emitter.write(".");
                let mut sorted: Vec<&str> = names.iter().map(|n| n.node.as_str()).collect();
                sorted.sort_unstable();
                // The one DECLARED carve-out from the gate, and the only
                // direct `emit_delimited_texts` caller. It has no `Gate`
                // at all: the names are SORTED, so emitted order is not
                // source order, and the comment cursor is forward-only —
                // interleaving per element would flush a later name's
                // comment against an earlier one. A comment inside a
                // grouped import therefore still leaves the group; that
                // is a real defect, filed with the residual family in
                // `TODO.md`, and closing it needs an order-aware cursor
                // rather than a gate here.
                let items: Vec<String> = sorted.iter().map(|n| (*n).to_string()).collect();
                self.emit_delimited_texts("{", "}", items);
                self.emitter.newline();
            }
            ImportStmt::From { path, names, glob_types, wildcard, .. } => {
                self.emitter.write("from ");
                self.format_dotted_path(path);
                self.emitter.write(" import ");
                if *wildcard {
                    self.emitter.write("*");
                    self.emitter.newline();
                    return;
                }
                // Merge regular names (with optional `as` alias) and glob types
                // (with .* suffix), then sort.
                let mut sorted: Vec<String> = names
                    .iter()
                    .map(|n| match &n.alias {
                        Some(a) => format!("{} as {}", n.name.node, a.node),
                        None => n.name.node.clone(),
                    })
                    .collect();
                for gt in glob_types {
                    sorted.push(format!("{}.*", gt.node));
                }
                sorted.sort_unstable();
                // No wrapping for `from` imports — bare names on new lines
                // would be parsed as new statements in indentation-based syntax.
                for (j, name) in sorted.iter().enumerate() {
                    if j > 0 {
                        self.emitter.write(", ");
                    }
                    self.emitter.write(name);
                }
                self.emitter.newline();
            }
        }
    }

    fn format_dotted_path(&mut self, path: &[Spanned<String>]) {
        for (i, seg) in path.iter().enumerate() {
            if i > 0 {
                self.emitter.write(".");
            }
            self.emitter.write(&seg.node);
        }
    }

    fn format_type_alias(&mut self, ta: &TypeAlias) {
        self.format_visibility(&ta.visibility, ta.explicit_visibility);
        self.emitter.write("type ");
        self.emitter.write(&ta.name.node);
        if let Some(ref gp) = ta.generic_params {
            // A7 — MEASURED, and the whole-flat reading is WRONG here: the
            // tail is `" = Dict["`, not `" = "` + the flat type. Charging the
            // whole type breaks aliases that were in budget.
            let tail = self.measured_reserve(|s| {
                s.emitter.write(" = ");
                s.format_type(&ta.type_);
            });
            self.with_tail_reserve(tail, |f| f.format_generic_params_wrapped(gp));
        }
        self.emitter.write(" = ");
        self.format_type(&ta.type_);
        self.emitter.newline();
    }

    fn format_newtype(&mut self, nt: &NewtypeDef) {
        self.format_visibility(&nt.visibility, nt.explicit_visibility);
        self.emitter.write("newtype ");
        self.emitter.write(&nt.name.node);
        self.emitter.write("(");
        // The inner type's tail is the `)` this arm writes after it.
        self.with_tail_reserve(1, |s| s.format_type(&nt.inner_type));
        self.emitter.write(")");
        self.emitter.newline();
    }

    fn format_const_decl(&mut self, cd: &ConstDecl) {
        self.format_visibility(&cd.visibility, cd.explicit_visibility);
        self.emitter.write("const ");
        // A10's var-decl cell, const sibling: the type's tail is ` name = `
        // plus the initializer's leading unbreakable text.
        let tail = self.measured_reserve(|s| {
            s.emitter.write(" ");
            s.emitter.write(&cd.name.node);
            s.emitter.write(" = ");
            s.format_expr(&cd.value);
        });
        self.with_tail_reserve(tail, |s| s.format_type(&cd.type_));
        self.emitter.write(" ");
        self.emitter.write(&cd.name.node);
        self.emitter.write(" = ");
        self.format_expr(&cd.value);
        self.emitter.newline();
    }

    fn format_static_decl(&mut self, sd: &StaticDecl) {
        // Static globals are private-by-default (the opposite of functions /
        // structs, which are public-by-default), so this path has always had
        // its own visibility rule rather than sharing `format_visibility`.
        //
        // `public` is emitted whenever the value IS Public: for a parsed
        // static that implies the author wrote it (the parser only reaches
        // `Public` here via an explicit keyword), and for a synthetically
        // built `StaticDecl` it keeps the emission from silently FLIPPING the
        // declaration to private. `private` is emitted only when written —
        // it is the default, so inventing it would rewrite the whole tree,
        // and DELETING an author's `private` is what this path used to do.
        if sd.visibility == Visibility::Public {
            self.emitter.write("public ");
        } else if sd.explicit_visibility {
            self.emitter.write("private ");
        }
        // Q3 PRESERVE: a bare `int counter = 0` at file scope is implicitly
        // static and builds the same node — inserting the keyword would be
        // writing code the author did not.
        if sd.explicit_static_kw {
            self.emitter.write("static ");
        }
        // A10's var-decl cell, static sibling.
        let tail = self.measured_reserve(|s| {
            s.emitter.write(" ");
            s.emitter.write(&sd.name.node);
            s.emitter.write(" = ");
            s.format_expr(&sd.value);
        });
        self.with_tail_reserve(tail, |s| s.format_type(&sd.type_));
        self.emitter.write(" ");
        self.emitter.write(&sd.name.node);
        self.emitter.write(" = ");
        self.format_expr(&sd.value);
        self.emitter.newline();
    }

    fn format_extern_block(&mut self, eb: &ExternBlock) {
        // Header anchor for the trailing-comment hook: the last real byte of
        // the header is the closing quote of the ABI string (`extern "C":`).
        // A block always carries one today (`parse_item`'s block-vs-inline
        // disambiguation REQUIRES the string literal), but the field is an
        // Option, so fall back to the `extern` keyword's own position.
        // The header's `:` — on the header's LAST source line whatever
        // precedes it. (The ABI string's end, which this used to use, is on
        // the same line for `extern "C":` and is now redundant.)
        let header_anchor_end = eb.header_colon_span.end;
        self.emitter.write("extern");
        if let Some(ref abi) = eb.abi {
            self.emitter.write(" ");
            self.emit_quoted_string(&abi.node, Some(abi.span));
        }
        self.emitter.write(":");
        // R41 T-FMT-A (S1 N13): `extern "C":  # why` — the block header's own
        // trailing comment, same class as struct/enum/trait/equip headers.
        self.emit_trailing_comment_after_header(header_anchor_end);
        self.emitter.newline();
        self.emitter.indent();
        for func in &eb.items {
            // R41 T-FMT-A (S1 N13): this child-collection loop was the ONE
            // such loop in the formatter with NO comment hooks at all, so
            // every comment interior to an `extern:` block escaped the block
            // and re-emerged at COLUMN 0 after it (via the EOF/leading
            // flush) — leading comments and between-item comments alike.
            // Pairing the two hooks here matches the trait/equip member
            // loops exactly; `formatter_child_collection_loop_census` in
            // tests/lints.rs now pins the whole family so the next such
            // loop cannot ship hookless.
            self.emit_comments_before(func.span.start);
            self.format_function(&func.node);
            self.emit_trailing_comment_after(func.span.end, false);
        }
        // CONTAINER flush — a comment after the last extern declaration.
        self.emit_orphan_comments_before_close(eb.span.start, eb.span.end);
        self.emitter.dedent();
    }

    // ── Generics & Bounds ───────────────────────────────────

    fn format_generic_param(&mut self, param: &GenericParam) {
        match param {
            GenericParam::Type { name, bounds } => {
                // Every bound is its own width-decided render, charged for the
                // rest of the bound list plus the ` name` that closes the
                // parameter — ADDED to whatever the enclosing generic-param
                // list already reserved for this item.
                for (i, tb) in bounds.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(" & ");
                    }
                    let rest = &bounds[i + 1..];
                    let tail = self.measured_reserve(|s| {
                        for b in rest {
                            s.emitter.write(" & ");
                            s.format_trait_bound(b);
                        }
                        s.emitter.write(" ");
                        s.emitter.write(&name.node);
                    });
                    self.with_tail_reserve(tail, |s| s.format_trait_bound(tb));
                }
                if !bounds.is_empty() {
                    self.emitter.write(" ");
                }
                self.emitter.write(&name.node);
            }
            GenericParam::Const { type_, name } => {
                self.emitter.write("const ");
                self.with_tail_reserve(1 + name.node.chars().count(), |s| {
                    s.format_type(type_)
                });
                self.emitter.write(" ");
                self.emitter.write(&name.node);
            }
        }
    }

    fn format_trait_bound(&mut self, tb: &Spanned<TraitBound>) {
        self.emitter.write(&tb.node.name.node);
        let has_args = tb.node.generic_args.as_ref().is_some_and(|a| !a.is_empty());
        let has_bindings = !tb.node.assoc_type_bindings.is_empty();
        if has_args || has_bindings {
            self.emitter.write("[");
            let mut first = true;
            // Another HAND-ROLLED comma loop, so every member is its own
            // width-decided render and each is charged for the rest of the
            // list plus the `]`.
            if let Some(ref args) = tb.node.generic_args {
                for (i, arg) in args.iter().enumerate() {
                    if !first {
                        self.emitter.write(", ");
                    }
                    let rest = &args[i + 1..];
                    let tail = self.measured_reserve(|s| {
                        s.format_trait_bound_tail(rest, &tb.node.assoc_type_bindings);
                    });
                    self.with_tail_reserve(tail, |s| s.format_type(arg));
                    first = false;
                }
            }
            for (i, binding) in tb.node.assoc_type_bindings.iter().enumerate() {
                if !first {
                    self.emitter.write(", ");
                }
                self.emitter.write(&binding.name.node);
                self.emitter.write(" = ");
                let rest = &tb.node.assoc_type_bindings[i + 1..];
                let tail = self.measured_reserve(|s| {
                    s.format_trait_bound_tail(&[], rest);
                });
                self.with_tail_reserve(tail, |s| s.format_type(&binding.type_));
                first = false;
            }
            self.emitter.write("]");
        }
    }

    /// The remainder of a trait bound's `[…]` list from a given point on, used
    /// only to MEASURE a member's tail; the emission walks the list itself.
    fn format_trait_bound_tail(
        &mut self,
        rest_args: &[Spanned<Type>],
        rest_bindings: &[AssocTypeBinding],
    ) {
        for t in rest_args {
            self.emitter.write(", ");
            self.format_type(t);
        }
        for b in rest_bindings {
            self.emitter.write(", ");
            self.emitter.write(&b.name.node);
            self.emitter.write(" = ");
            self.format_type(&b.type_);
        }
        self.emitter.write("]");
    }

    // ── Parameters ──────────────────────────────────────────

    /// Format a parenthesized parameter list with line-width-aware wrapping.
    /// Writes `(param1, param2)` on one line if it fits, otherwise FILL-PACKS
    /// it — as many parameters per line as the budget allows, continuation
    /// lines at the block indent, no trailing comma:
    /// ```text
    /// (param1, param2, param3, param4,
    ///     param5, param6)
    /// ```
    /// With an interior comment the list instead breaks fully, one
    /// parameter per line WITH a trailing comma — the canonical exploded
    /// form shared with every other gated list.
    fn format_params_wrapped(&mut self, params: &[Spanned<Param>], gate: Gate) {
        self.emit_delimited_list(
            "(", ")", gate, params,
            |p| (p.span.start, p.span.end),
            |f, p| f.format_param(&p.node),
        );
    }

    /// Format a parenthesized call argument list with line-width-aware wrapping.
    fn format_call_args_wrapped(&mut self, args: &[Spanned<CallArg>], gate: Gate) {
        self.emit_delimited_list(
            "(", ")", gate, args,
            |a| (a.span.start, a.span.end),
            |f, a| f.format_call_arg(&a.node),
        );
    }

    /// Format a bracketed generic parameter list with line-width-aware wrapping.
    fn format_generic_params_wrapped(&mut self, gp: &Spanned<GenericParams>) {
        // No scan needed: `parse_generic_params` merges the `[` and `]`
        // token spans, so `gp.span` already IS the gate range.
        self.emit_delimited_list(
            "[", "]", Gate::Span(gp.span.start, gp.span.end), &gp.node.params,
            |p| (p.span.start, p.span.end),
            |f, p| f.format_generic_param(&p.node),
        );
    }

    /// Format a parenthesized CLOSURE-parameter list with wrapping.
    ///
    /// Named like its four siblings rather than spelled inline at the
    /// `Expr::Closure` arm: `formatter_list_emit_fill_census` counts
    /// `self.emit_delimited_list(` call sites, so a list emitter written
    /// inside a closure — where the receiver is not `self` — would leave the
    /// census one short while the list kind still existed.
    fn format_closure_params_wrapped(&mut self, params: &[Spanned<ClosureParam>], gate: Gate) {
        self.emit_delimited_list(
            "(", ")", gate, params,
            |p| (p.span.start, p.span.end),
            |f, p| f.format_closure_param(&p.node),
        );
    }

    /// Format a bracketed generic argument list (types) with wrapping.
    fn format_generic_args_wrapped(&mut self, args: &[Spanned<Type>], gate: Gate) {
        self.emit_delimited_list(
            "[", "]", gate, args,
            |t| (t.span.start, t.span.end),
            |f, t| f.format_type(t),
        );
    }

    /// Format a method chain with line-width-aware wrapping.
    /// When the chain fits on one line: `items.filter(pred).map(f).collect()`
    /// When broken:
    /// ```text
    /// items
    ///     .filter(pred)
    ///     .map(f)
    ///     .collect()
    /// ```
    /// The C10 reserves a comprehension's pre-rendered pieces need.
    ///
    /// Same broken-layout rule as the chain carriers: `build_comprehension_doc`
    /// puts a `softline` before the close, so in BROKEN mode the element, the
    /// `for` clause and the `if` clause each own a line and NONE of them
    /// carries the close or the caller's tail. All three reserves are
    /// therefore 0, and the FLAT decision is taken by the enclosing group's
    /// own fit test, which now consumes `tail_reserve`.
    ///
    /// The function survives as the ONE place that statement is written down —
    /// a bare `element_to_string` at the three call sites would leave the next
    /// reader to re-derive it, and re-deriving it as "the clause text that
    /// follows me when flat" is precisely the over-reserve that degraded 291
    /// already-in-budget chain hunks when the binary chain was spelled that
    /// way.
    fn comprehension_reserves(&self) -> ComprehensionReserves {
        ComprehensionReserves {
            indent: self.emitter.indent + 1,
            element: 0,
            iterable: 0,
            condition: 0,
        }
    }

    /// One `.method[GA](args)` segment of a method chain. ONE spelling,
    /// shared by the emission and by C10's flat measurement.
    fn format_chain_segment(
        &mut self,
        method: &Spanned<String>,
        generic_args: &Option<Vec<Spanned<Type>>>,
        args: &[Spanned<CallArg>],
    ) {
        self.emitter.write(".");
        self.emitter.write(&method.node);
        // DECLARED carve-outs. This runs on the sub-`Formatter` built by
        // `element_to_string*`, whose comment sideband is EMPTY — a
        // `Gate::Span` would evaluate against no comments and read as a live
        // gate while being dead code. The escape is real (a comment inside a
        // chain segment's arguments re-parents), and it belongs to the
        // pre-render-ABOVE mechanism: the chain itself must decide before it
        // pre-renders. Filed with a repro in `TODO.md`.
        if let Some(ga) = generic_args {
            let r = Gate::UngatedCarveOut("chain segment generic args: empty sideband");
            self.format_generic_args_wrapped(ga, r);
        }
        let r = Gate::UngatedCarveOut("chain segment call args: empty sideband");
        self.format_call_args_wrapped(args, r);
    }

    fn format_method_chain(&mut self, expr: &Spanned<Expr>) {
        let (root, segments) = collect_method_chain(expr);

        // C10, method-chain carrier — the same rule as the binary chain: a
        // pre-rendered piece is charged only what CERTAINLY shares its line,
        // which for a `Doc::Group`-clothed carrier is the BROKEN layout. Each
        // segment then owns a line, so only the LAST inherits the caller's
        // tail; the root and the intermediate segments carry nothing. The FLAT
        // case is decided by the enclosing group's own fit test, which now
        // consumes `tail_reserve`.
        let base = self.tail_reserve;
        let last = segments.len().saturating_sub(1);

        let root_reserve = if segments.is_empty() { base } else { 0 };
        let root_str = self
            .element_to_string_reserving(self.emitter.indent + 1, root_reserve, |f| {
                f.format_expr(root)
            });

        let mut parts = Vec::with_capacity(segments.len() + 1);
        // Format each .method(args) segment as a string
        for (i, (method, generic_args, args)) in segments.iter().enumerate() {
            let reserve = if i == last { base } else { 0 };
            let seg_str =
                self.element_to_string_reserving(self.emitter.indent + 1, reserve, |f| {
                    f.format_chain_segment(method, generic_args, args)
                });
            parts.push(seg_str);
        }

        // Build Doc: root + indent(softline + .method1() + softline + .method2() + ...)
        let mut inner_docs = Vec::with_capacity(parts.len() * 2);
        for part in &parts {
            inner_docs.push(doc::softline());
            inner_docs.push(doc::text(part));
        }

        let chain_doc = doc::group(doc::concat(vec![
            doc::text(root_str),
            doc::indent(doc::concat(inner_docs)),
        ]));
        self.write_doc(&chain_doc);
    }

    /// Format a binary expression with line-width-aware wrapping.
    /// Flattens chains of the same operator for clean breaking.
    /// When the expression fits: `a + b + c`
    /// When broken:
    /// ```text
    /// a
    ///     + b
    ///     + c
    /// ```
    fn format_binary_chain(
        &mut self,
        left: &Spanned<Expr>,
        op: BinaryOp,
        right: &Spanned<Expr>,
    ) {
        let outer_left_bp = binary_op_left_bp(op);
        let outer_right_assoc = binary_op_is_right_assoc(op);

        // Flatten same-operator chains for clean wrapping — but ONLY
        // for LEFT-associative operators. Right-associative flattening
        // is unsafe: `(a ** b) ** c` and `a ** (b ** c)` both AST-shape
        // as `BinaryOp(Pow, ...)` with the same operand vector after
        // flattening, so a re-emit as `a ** b ** c` would silently
        // collapse the two into the parser-default right-associative
        // reading. Handle right-assoc as a plain 2-operand shape and
        // let the paren-wrap helpers keep semantics true.
        let mut operands: Vec<&Spanned<Expr>> = Vec::new();
        if outer_right_assoc {
            operands.push(left);
        } else {
            collect_binary_operands(left, op, &mut operands);
        }
        operands.push(right);

        let op_str = binary_op_str(op);

        // FMT-A (Round XXXVI): wrap operand in `(...)` when its own
        // precedence would misparse against the outer op. Position 0 is
        // the LEFT operand of the leftmost pairing; positions >0 are all
        // in RIGHT positions of successively-nested left-assoc pairings
        // (or the single right-operand of a right-assoc op).
        // C10, binary-chain carrier. **What a pre-rendered piece may be
        // charged is what CERTAINLY shares its line**, and for a
        // `Doc::Group`-clothed carrier that is the BROKEN layout: each
        // operand then owns a line, so only the LAST one carries anything —
        // the `)` that `wrap_multiline_expr_in_parens` appends in broken mode,
        // plus the caller's own tail.
        //
        // The FLAT case needs nothing here: the enclosing `Doc::Group`'s own
        // fit test now consumes `tail_reserve`, so it decides flat-vs-broken
        // with the caller's suffix already counted. Charging each operand the
        // whole REMAINING CHAIN instead — the tempting "what shares its line
        // when flat" reading — measured as a corpus-wide degradation: 291
        // already-broken, already-in-budget chain hunks re-broke into nested
        // paren-wrapped fragments, because the reserve forced each operand's
        // own sub-render to break internally. Safe-not-exact must not become
        // pessimistic-and-wrong.
        //
        // ⚠ Track D rewrites `wrap_multiline_expr_in_parens` after this
        // change; the `+ 1` below is the coupling between the two.
        let base = self.tail_reserve;
        let last = operands.len().saturating_sub(1);
        let operand_strs: Vec<String> = operands
            .iter()
            .enumerate()
            .map(|(i, o)| {
                let position = if i == 0 { BinOpPos::Left } else { BinOpPos::Right };
                let reserve = if i == last {
                    base + usize::from(operands.len() > 1)
                } else {
                    0
                };
                self.element_to_string_reserving(self.emitter.indent + 1, reserve, |f| {
                    f.format_binop_operand(o, outer_left_bp, position, outer_right_assoc);
                })
            })
            .collect();

        // Build: operand1 <line " op "> operand2 <line " op "> operand3 ...
        let mut docs = Vec::with_capacity(operand_strs.len() * 2);
        for (i, s) in operand_strs.iter().enumerate() {
            if i > 0 {
                // In flat mode: ` op `. In broken mode: newline + indent + `op `.
                docs.push(doc::line());
                docs.push(doc::text(format!("{op_str} ")));
            }
            docs.push(doc::text(s));
        }

        // Wrap the chain via the shared `wrap_multiline_expr_in_parens` helper
        // so a broken chain emits `(a\n    + b)` (parseable) rather than
        // `a\n    + b` (unparseable — bare leading-operator continuation drops
        // the orphan lines on a second `gg fmt` pass, silently LOSING code).
        // See the helper's docstring for the full rationale; guard is
        // `fmt_binary_chain_round_trips` in tests/integration.rs.
        let inner = doc::concat(vec![
            docs.remove(0), // first operand
            doc::indent(doc::concat(docs)),
        ]);
        let bin_doc = wrap_multiline_expr_in_parens(inner);
        self.write_doc(&bin_doc);
    }

    fn format_param(&mut self, param: &Param) {
        // meta op parameter: `meta name` — carries only an operator token, no
        // runtime type. The parser stores it as `type_=Void, is_meta_op=true`
        // (parser `parse_param`), so WITHOUT this arm we fall through to the
        // type-first path below and re-emit the placeholder as `void name`. On
        // reparse `is_meta_op` is then false → the op-binding is filtered out of
        // `meta_env` → the substitution sweep is skipped → a `meta[op]` infix
        // survives to GIR lowering and panics (`MetaOpInfix not substituted`).
        // Fix at the write site (Core #1): emit the `meta` keyword form.
        if param.is_meta_op {
            self.emitter.write("meta ");
            self.emitter.write(&param.name.node);
            return;
        }
        // METHOD RECEIVER (`self` / `&self` / `^self`) — the one param
        // whose surface form is the bare sigil+keyword, with no type and
        // no separate name to emit.
        //
        // Keyed off the typed `Param::is_receiver` axis, NOT off
        // `type_ == Type::SelfType`: a `Self`-TYPED regular param is legal
        // (`int get(Self a)`) and the type-based inference collapsed it to
        // `self`, DESTROYING the user's parameter name — the emitted body
        // then referenced an undefined `a`. The flag is written once at the
        // parser's receiver chokepoint (`make_self_param`), so this read
        // cannot disagree with the parse (Layering rule 4).
        if param.is_receiver {
            match param.ownership {
                Ownership::Borrow => self.emitter.write("self"),
                Ownership::MutableBorrow => self.emitter.write("&self"),
                // D27 Round A: `^self` (was `!self`); `!` is the error channel.
                Ownership::Move => self.emitter.write("^self"),
            }
            return;
        }
        // type-first: `type [&|!]name`
        //
        // A10's param-type position: the type's tail is the sigil, the name
        // and any ` = default`, ADDED to whatever the enclosing list already
        // reserved for this item (its `,` or the list's close + the caller's
        // own suffix) — which is what `with_tail_reserve`'s additivity buys.
        let tail = self.measured_reserve(|s| {
            s.emitter.write(" ");
            s.format_ownership_prefix(param.ownership);
            s.emitter.write(&param.name.node);
            if let Some(ref default) = param.default {
                s.emitter.write(" = ");
                s.format_expr(default);
            }
        });
        self.with_tail_reserve(tail, |s| s.format_type(&param.type_));
        self.emitter.write(" ");
        self.format_ownership_prefix(param.ownership);
        self.emitter.write(&param.name.node);
        if let Some(ref default) = param.default {
            self.emitter.write(" = ");
            self.format_expr(default);
        }
    }

    // ── Statements ──────────────────────────────────────────

    fn format_block_stmts(&mut self, block: &Block) {
        // R39 snag #2: `emit_trailing_comment_after(stmt.span.end)` after
        // each stmt captures the `stmt  # doc` case for ALL block-body
        // contexts — function bodies, if/elif/else, for/while, match arm
        // bodies, try/catch/rethrow, with, unsafe, select. This is the
        // largest coverage site (Core #4 producer chokepoint): a fix
        // here retires the class for the whole block-stmt family instead
        // of at each match arm's `format_stmt` call site.
        //
        // gorget-arena snag #3 (R39, owner 2026-08-09): preserve
        // author-written blank lines between consecutive stmts. Pre-fix,
        // `gg fmt` deleted all intra-block blanks (844 stripped on the
        // 12.7k-line arena codebase). Per black/rustfmt/gofmt: preserve
        // blanks, collapse runs of ≥2 → 1. Check the source bytes between
        // prev.span.end and cur.span.start for a blank-line separator (a
        // `\n` beyond the mandatory line-end `\n`). Skip on the first
        // stmt (blank between block opener and first stmt is not
        // paragraphing, it's stylistic sparseness we canonicalize away).
        for (i, stmt) in block.stmts.iter().enumerate() {
            if i > 0 && self.has_blank_line_between(block.stmts[i - 1].span.end, stmt.span.start) {
                self.emitter.blank_line();
            }
            self.emit_comments_before(stmt.span.start);
            self.format_stmt(stmt);
            self.emit_trailing_comment_after(stmt.span.end, false);
        }
        // THE ROUTED flush — statement suites share the container class's
        // tail-orphan hole, and this is the one function every one of them
        // reaches (fn / if / elif / else / while / for / for-else / loop /
        // unsafe / with / named scope / on error / do / catch / rethrow /
        // match-arm / select-arm / test / bench / suite setup+teardown / the
        // `meta` suites / closures). It runs at the BODY indent — its callers
        // own the surrounding `indent()`/`dedent()` — so the flush lands at
        // the right column here.
        //
        // Indented suites ONLY, for two reasons: an INLINE suite has no body
        // of its own to orphan into, and `format_inline_suite` routes THROUGH
        // this function while the buffer sits mid-line, where emitting a
        // standalone comment line would split the statement.
        if block.layout == SuiteLayout::NextLine {
            self.emit_orphan_comments_before_close(block.header_start, block.span.end);
        }
    }

    /// gorget-arena snag #3 (R39) + #3b reconciliation (R40): true iff the
    /// author wrote a blank line DIRECTLY ABOVE the comment run that leads
    /// `cur_start` — equivalently, a blank between prev's last content line
    /// and the TOPMOST comment of the run (or above `cur` itself when there
    /// is no run). Walks upward from `cur_start`, skipping the run's comment
    /// lines, and reports the blankness of the line immediately above prev's
    /// content.
    ///
    /// **#3b reconciliation (why NON-transparent about ownership):** the
    /// R39 version walked TRANSPARENTLY through comments and returned true
    /// on the FIRST blank anywhere in the region, so a blank BELOW a
    /// comment (`stmt\n# c\n\nstmt`) was reported here and emitted ABOVE the
    /// comment — moving it. With #3b, blanks BELOW/BETWEEN the run's
    /// comments are owned by `emit_comments_before` (`blank_line_follows`);
    /// this predicate must therefore own ONLY the blank ABOVE the run, or
    /// the two double-count (blank both sides ⇒ two blanks). It reports the
    /// classification of the line DIRECTLY above prev's content: blank there
    /// ⇒ blank-above-run; a comment or content line there ⇒ no blank-above.
    ///
    /// **Why backward walk (not `source[prev_end..cur_start]`):** for
    /// container-type items (`struct`/`enum`/`trait`/`equip`/`function`),
    /// the AST `span.end` sits at the DEDENT token — often ZERO WIDTH at the
    /// same byte position as the NEXT item's start, so `source[prev.span.end
    /// ..cur.span.start]` is empty even when the author wrote blanks. The
    /// walk-back from `cur_start` past trailing whitespace and the comment
    /// run reaches prev's real last content regardless. `_prev_end` is
    /// unused for the same zero-width reason.
    fn has_blank_line_between(&self, _prev_end: usize, cur_start: usize) -> bool {
        if cur_start == 0 || cur_start > self.source.len() {
            return false;
        }
        let bytes = self.source.as_bytes();
        let mut i = cur_start;
        // Skip cur's own line (from its last `\n` to cur_start).
        while i > 0 && bytes[i - 1] != b'\n' {
            i -= 1;
        }
        // Walk lines upward, remembering the blankness of the line last
        // examined. On reaching prev's content line, return that remembered
        // value: it is the classification of the line DIRECTLY above prev's
        // content (the topmost line of the comment run, or cur's own
        // predecessor when the run is empty).
        let mut last_was_blank = false;
        while i > 0 {
            i -= 1; // step past the `\n` that ended the line above
            let line_end = i;
            while i > 0 && bytes[i - 1] != b'\n' {
                i -= 1;
            }
            let line = &bytes[i..line_end];
            match line.iter().position(|b| !b.is_ascii_whitespace()) {
                None => last_was_blank = true,               // blank line
                Some(pos) if line[pos] == b'#' => last_was_blank = false, // comment
                _ => return last_was_blank,                  // prev's content line
            }
        }
        false
    }

    /// gorget-arena snag #3b (R40): reorder-immune forward source scan.
    /// True iff the line IMMEDIATELY after the source line containing `pos`
    /// is a blank (all-whitespace, `\n`-terminated) line. `pos` is a
    /// comment's own span end; scanning forward from it reads only the
    /// physical source right after the comment, so it is immune to import
    /// SORTING (unlike `blank_between(comment.end, next_item.span.start)`,
    /// whose right endpoint is a reordered AST position — that form
    /// false-positived the golden sample in the scout's first prototype).
    ///
    /// A trailing all-whitespace tail at EOF (no terminating `\n`) is NOT a
    /// paragraph blank, so it returns false — the trailing-newline
    /// normalization in `format` owns that.
    fn blank_line_follows(&self, pos: usize) -> bool {
        let bytes = self.source.as_bytes();
        let mut i = pos.min(bytes.len());
        // Advance to the `\n` ending pos's own line.
        while i < bytes.len() && bytes[i] != b'\n' {
            i += 1;
        }
        if i >= bytes.len() {
            return false; // pos's line is the final line — nothing follows
        }
        i += 1; // step past that `\n` onto the next line
        let line_start = i;
        while i < bytes.len() && bytes[i] != b'\n' {
            i += 1;
        }
        // Blank line = all-whitespace AND `\n`-terminated (i < len).
        i < bytes.len() && bytes[line_start..i].iter().all(|b| b.is_ascii_whitespace())
    }

    /// gorget-arena snag #3b (R40): true iff the line IMMEDIATELY above the
    /// source line containing `pos` is a blank (all-whitespace) line. Used
    /// by `emit_remaining_comments` to preserve a blank the author wrote
    /// between the last real item and the first EOF-orphan comment (the
    /// blank ABOVE the EOF run, which no sibling loop's
    /// `has_blank_line_between` covers).
    fn blank_line_directly_above(&self, pos: usize) -> bool {
        if pos == 0 || pos > self.source.len() {
            return false;
        }
        let bytes = self.source.as_bytes();
        let mut i = pos;
        // Walk back to the start of pos's own line.
        while i > 0 && bytes[i - 1] != b'\n' {
            i -= 1;
        }
        if i == 0 {
            return false; // pos is on the first source line
        }
        // bytes[i-1] is the `\n` ending the previous line; examine that line.
        let line_end = i - 1;
        let mut j = line_end;
        while j > 0 && bytes[j - 1] != b'\n' {
            j -= 1;
        }
        bytes[j..line_end].iter().all(|b| b.is_ascii_whitespace())
    }

    /// gorget-js snag #15b (R39 Track F) + #15c (R39 Track G): the arm body of
    /// `else:` / `catch (e):` / `rethrow (e):` accepts either an inline
    /// expression or an indented block WITHOUT a `do:` keyword — the parser
    /// takes the indented block directly at those positions. Formatting the
    /// arm body through `format_expr` would emit `Expr::Block` as a `do:` wrap
    /// (a formatter-only artifact), which:
    ///   - #15b (single-expr body): breaks move-tail semantics (`else: do:\n
    ///     ^x` rejects with E_MoveInOperandPosition; `else: ^x` doesn't).
    ///     Fixed by Track F's Stmt::Expr inline carve-out at Expr::Block.
    ///   - #15c (multi-stmt body): compiles but adds noise + rot (users read
    ///     the reformatted `catch (e): do:` and re-add the `do:` on rewrites).
    ///
    /// This helper handles both by calling from the arm-body sites directly.
    /// Caller writes the `else:`/`catch (e):`/`rethrow (e):` prefix WITHOUT
    /// trailing space; helper emits either a leading space + inline expression
    /// (single-expr / non-Block body) or a newline + indented bare block
    /// (multi-stmt Block body).
    /// If `block` is a single-stmt block whose sole stmt is a bare terminal
    /// expression (Stmt::Throw / Stmt::Return / Stmt::Expr), emit it inline
    /// and return true; otherwise leave the emitter untouched and return
    /// false.
    ///
    /// Core #4 producer chokepoint (R39 fold, 2026-08-09): the two `format_expr`
    /// arms that used to hand-roll this carve-out (Expr::Block and Expr::Do)
    /// now delegate here, so a future third block-like AST variant needs a
    /// one-line delegation instead of a copy-pasted 17-line match. The
    /// carve-out itself is load-bearing for TWO defect classes:
    ///   - Synthetic Expr::Block wrappers around throw/return (parser
    ///     wraps them to make `throw x`/`return x` parse as expression
    ///     prefixes) — inlining round-trips; do:-wrapping drifts and
    ///     breaks `fmt_idempotent` (gorget-js critique #2, 2026-05-13).
    ///   - Move-sigil arm tails (`else: ^b`, `catch (e): ^b`) — do:-
    ///     wrapping makes the expression a READ position and rejects
    ///     with `E_MoveInOperandPosition` under the D27 emit swap
    ///     (gorget-js snag #15b, R39 Track F for Expr::Block +
    ///     widening fix for Expr::Do).
    fn try_inline_single_terminal_stmt(&mut self, block: &Block) -> bool {
        self.try_inline_single_terminal_stmts(&block.stmts)
    }

    /// The slice form of [`Self::try_inline_single_terminal_stmt`], for the
    /// one caller that must skip a parser-synthesized prelude: a
    /// destructuring closure's `Block` carries the destructure binds ahead of
    /// the author's body, so the "is this a single terminal statement?"
    /// question is about the tail, not the whole block.
    fn try_inline_single_terminal_stmts(&mut self, stmts: &[Spanned<Stmt>]) -> bool {
        if stmts.len() != 1 {
            return false;
        }
        let stmt = &stmts[0];
        match &stmt.node {
            Stmt::Throw(value) => {
                self.emitter.write("throw ");
                self.format_expr(value);
            }
            Stmt::Return(value) => {
                self.emitter.write("return");
                if let Some(v) = value {
                    self.emitter.write(" ");
                    self.format_expr(v);
                }
            }
            Stmt::Expr(expr) => {
                self.format_expr(expr);
            }
            _ => return false,
        }
        // gorget-js snag 15e (R39, owner 2026-08-09): when we inline a
        // single-stmt block (`else:\n    return cc  # comment`), the
        // trailing comment on the ORIGINAL stmt's source line would
        // otherwise be orphaned (dedents to enclosing scope's `#`-line at
        // indent 2 — user-reported class). Attach it here — the caller
        // is about to emit a newline after the inline output; the hook
        // splices via `inject_before_newline` (or appends if no newline
        // yet), so the trailing comment lands correctly regardless of
        // caller's emit order.
        self.emit_trailing_comment_after(stmt.span.end, false);
        true
    }

    /// THE producer for an arm/clause BODY written after a header colon —
    /// match arms, the expression-match `else`, and the `catch` / `rethrow`
    /// recovery arms — INCLUDING where the header's trailing comment goes.
    ///
    /// The body has exactly three shapes, and each puts the header comment in a
    /// different place. Keeping the shape decision and the comment placement in
    /// ONE function is the point: they were split before, with the shape test
    /// hand-mirrored at the call site, and the copy disagreed with the original
    /// on the shape it was written to exclude.
    ///
    /// * **Author `do:`** — the keyword belongs to the HEADER's line, so the
    ///   comment is emitted after ` do:`. Firing the hook before it would spell
    ///   `else:  # note do:`, eating the author's keyword into a comment; that
    ///   is not cosmetic, because the `do:` changes what the arm MEANS. Measured
    ///   (2026-08-11), with `String b` moved out of a `String`-typed match:
    ///     `else: ^b`                 → compiles
    ///     `else: do:` + `\n    ^b`   → REJECTED, `E_MoveInOperandPosition`
    ///     `else: do: ^b` (one line)  → PARSE error, "expected NEWLINE"
    ///   So the reject is a property of the INDENTED `do:` suite; the one-line
    ///   spelling is not a legal program at all, and the two must not be quoted
    ///   as if they were the same case (an earlier version of this comment did).
    /// * **Indented suite** — the header owns its line, so the comment is
    ///   emitted at the header, exactly as at every other clause position.
    /// * **Inline expression** — the body SHARES the header's line, so the
    ///   comment is CLAIMED first and emitted after the body. Emitting it at the
    ///   colon would put the comment ahead of the value it heads and the output
    ///   would not re-parse. Claiming (rather than deferring the whole hook) is
    ///   what stops a leading hook inside the body from taking it first.
    ///
    /// Snag #15c note: `parse_body_or_expr` (catch/rethrow arm) SYNTHESIZES the
    /// indented body as `Expr::Do`, NOT `Expr::Block`. The match `else:` uses
    /// `parse_arm_body`, which returns `Expr::Block` for the indented form.
    /// Both carry an indented statement list and format WITHOUT re-emitting
    /// `do:` — the parser accepts the indented form directly at all three sites.
    /// An AUTHOR-written `do:` is the different case, and `author_spelled` is
    /// what separates the two producers; the one-path treatment deleted it.
    ///
    /// `header_anchor` is the last real byte of the header before its colon.
    fn format_arm_body(&mut self, body: &Spanned<Expr>, header_anchor: usize) {
        if let Expr::Do {
            body: block,
            author_spelled: true,
        } = &body.node
        {
            self.emitter.write(" do:");
            self.emit_trailing_comment_after_header(header_anchor);
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(block);
            self.emitter.dedent();
            return;
        }
        let block_opt = match &body.node {
            Expr::Block(block) => Some(block),
            Expr::Do { body, .. } => Some(body),
            _ => None,
        };
        if let Some(block) = block_opt {
            // R41 T-FMT-C: the question is WHAT THE AUTHOR SPELLED, not how
            // many statements the suite happens to hold. The old
            // `stmts.len() > 1` test collapsed every one-statement indented
            // `else:` onto its header while the sibling `case` arms — reading
            // the AST shape instead — kept theirs, so one construct formatted
            // two different ways.
            if block.layout == SuiteLayout::NextLine {
                self.emit_trailing_comment_after_header(header_anchor);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(block);
                self.emitter.dedent();
                return;
            }
        }
        let deferred = self.claim_header_trailing_comments(header_anchor);
        self.emitter.write(" ");
        self.format_expr(body);
        self.emit_claimed_header_comments(deferred);
    }

    /// Emit an INLINE statement suite — the one the author wrote on the
    /// header's own line (`if c: stmt`, `elif c: stmt`, `else: stmt`,
    /// `on error stmt`, meta `case e: stmt`).
    ///
    /// `header_suffix` is the per-site spelling between header and body:
    /// `":"` everywhere except `on error`, whose inline form takes NO colon
    /// (`on error: print(1)` is a parse error).
    ///
    /// Three deliberate properties:
    ///   * **no `indent()`** — a block-bearing child (`if true: int x = match
    ///     y:` followed by an indented suite) nests relative to the HEADER's
    ///     indent, which is where the author put it. Indenting first would
    ///     push the nested suite a level deeper on every pass.
    ///   * **delegation through `format_block_stmts`** — the child then gets
    ///     the same leading/trailing comment hooks as any other statement,
    ///     rather than a second, subtly different comment path.
    ///   * **no width check** — this path emits through `write`, not the Doc
    ///     renderer, so an over-width one-liner stays a one-liner. Letting
    ///     width re-decide the suite's FORM would rewrite a choice the author
    ///     made, for a reason invisible in the source.
    fn format_inline_suite(&mut self, header_suffix: &str, block: &Block, header_anchor_end: usize) {
        self.emitter.write(header_suffix);
        // Claim the header's trailing comment BEFORE the body is emitted (so
        // a leading hook inside the body cannot take it first) and emit it
        // AFTER (so it does not swallow the statement).
        let deferred = self.claim_header_trailing_comments(header_anchor_end);
        self.emitter.write(" ");
        self.format_block_stmts(block);
        self.emit_claimed_header_comments(deferred);
        self.emitter.newline();
    }

    /// A closure's body — everything the `Expr::Closure` arm writes AFTER its
    /// `:`. ONE spelling, shared by the emission and by A8's reserve
    /// measurement, so the block-body and inline-body cells fall out of the
    /// same code rather than a duplicated layout test.
    fn format_closure_body(&mut self, body: &Spanned<Expr>, params: &[Spanned<ClosureParam>]) {
        // Total prelude stmts injected by the parser for tuple destructuring.
        let prelude_skip: usize = params
            .iter()
            .filter_map(|p| p.node.destructure.as_ref().map(|b| b.len()))
            .sum();
        let Expr::Block(ref block) = body.node else {
            self.emitter.write(" ");
            self.format_expr(body);
            return;
        };
        let post_prelude: Vec<&Spanned<Stmt>> = block.stmts.iter().skip(prelude_skip).collect();
        // R41 T-FMT-C: layout-GATED, not layout-only. `NextLine` means the
        // author indented the body and it stays indented; `Inline` still has
        // to pick WHICH inline form, because the parser normalizes an
        // expression body into `Block { ..prelude.., Return(e) }` and that
        // `return` is the parser's spelling, not the author's.
        //
        // The old code inferred the form from the shape
        // (`post_prelude.len() == 1 && Stmt::Return`), which COLLAPSED an
        // author-indented single-`return` body onto the header. A
        // multi-statement indented body survived, which is why a fixture
        // carrying only that shape would have been green throughout.
        if block.layout != SuiteLayout::Inline {
            self.emitter.newline();
            self.emitter.indent();
            self.format_closure_post_prelude(block, &post_prelude, prelude_skip);
            self.emitter.dedent();
            return;
        }
        self.emitter.write(" ");
        let inline_expr = if post_prelude.len() == 1 {
            match &post_prelude[0].node {
                Stmt::Return(Some(e)) => Some(e.clone()),
                _ => None,
            }
        } else {
            None
        };
        if let Some(expr) = inline_expr {
            self.format_expr(&expr);
            return;
        }
        // A non-`return` inline shape — `(int x): throw "a"` is a synthetic
        // `Block { Throw }` with no expression form. Route it through the
        // shared single-terminal inliner rather than exploding it onto its
        // own line.
        let tail: Vec<Spanned<Stmt>> = post_prelude.iter().map(|s| (*s).clone()).collect();
        if !self.try_inline_single_terminal_stmts(&tail) {
            // Unreachable for parser output: an `Inline` closure body is
            // always a one-statement synthetic wrap. Emit the suite rather
            // than drop it (Core #10 — never silently discard what the author
            // wrote).
            self.emitter.newline();
            self.emitter.indent();
            self.format_closure_post_prelude(block, &post_prelude, prelude_skip);
            self.emitter.dedent();
        }
    }

    /// Emit an indented closure body, skipping the parser-synthesized
    /// destructure prelude.
    ///
    /// With no prelude this is plain `format_block_stmts`. With one, the
    /// shared helper cannot be used — it would emit the destructure binds the
    /// parser injected — so the per-statement leading/trailing comment hooks
    /// are mirrored here for the author's tail.
    fn format_closure_post_prelude(
        &mut self,
        block: &Block,
        post_prelude: &[&Spanned<Stmt>],
        prelude_skip: usize,
    ) {
        if prelude_skip == 0 {
            self.format_block_stmts(block);
            return;
        }
        for stmt in post_prelude {
            self.emit_comments_before(stmt.span.start);
            self.format_stmt(stmt);
            self.emit_trailing_comment_after(stmt.span.end, false);
        }
        // CLOSURE ROUTING: the prelude-skipping loop is the ONE statement body
        // that cannot delegate to `format_block_stmts` (that would re-emit the
        // parser-synthesized destructure binds), so it owes the routed
        // chokepoint's tail flush explicitly. Without it a plain closure is
        // fixed and a DESTRUCTURING one is not — the same class, half done.
        if block.layout == SuiteLayout::NextLine {
            self.emit_orphan_comments_before_close(block.header_start, block.span.end);
        }
    }

    /// True iff the author left a blank line above a CLAUSE header
    /// (`elif:` / `else:` / a `case` arm).
    ///
    /// `format_block_stmts` preserves an author blank between two statements,
    /// but a clause header is not a statement — it is written by its own emit
    /// site, and before this predicate existed none of those sites checked
    /// (the current site set is `grep -n "blank_before_clause(" ` on this
    /// file — regenerated, never quoted). A blank the author put
    /// between a long branch body and the `else:` that follows it is
    /// paragraphing exactly like any other, and deleting it is the same
    /// defect at a position the statement loop cannot see.
    ///
    /// `anchor` is any position on the clause's own source line. The
    /// predicate is `has_blank_line_between` rather than
    /// `blank_line_directly_above` because the blank may sit ABOVE a comment
    /// run that leads the clause (`blank`, `# note`, `else:`); the
    /// directly-above form reports the COMMENT line there and drops the cell.
    fn blank_before_clause(&self, anchor: usize) -> bool {
        // `has_blank_line_between` walks BACKWARD from its second argument and
        // never reads the first (block-bearing spans are zero-width at DEDENT
        // positions), so there is no meaningful `prev` to pass here.
        self.has_blank_line_between(0, anchor)
    }

    /// THE producer for an `else:` clause header whose body is INDENTED.
    ///
    /// Writes the header, fires the trailing-comment hook, ends the line. The
    /// hook is the part every `else` site was missing: `elif` had it and the
    /// `case` arms had it, so `else:  # note` — alone among clause headers —
    /// dropped its comment through to `format_block_stmts`, which re-emitted it
    /// as a LEADING comment on the branch's first statement. The comment then
    /// documents the wrong thing: a note about the `else` reads as a note about
    /// whatever happens to come first inside it. That is the misattribution
    /// class R41 T-FMT-A retired at the other clause positions.
    ///
    /// INLINE bodies do NOT come here — they need the claim-then-emit split
    /// (`format_inline_suite`), because a comment emitted at the header would
    /// land ahead of a body that shares the line and the output would not
    /// re-parse.
    ///
    /// `anchor` is any position on the clause's own source line; every caller
    /// passes the same one it gives `blank_before_clause`, which is the
    /// clause's colon.
    fn emit_else_header(&mut self, anchor: usize) {
        self.emitter.write("else:");
        self.emit_trailing_comment_after_header(anchor);
        self.emitter.newline();
    }

    fn format_elif_else_blocks(
        &mut self,
        elif_branches: &[(Spanned<Expr>, Block)],
        else_body: Option<&Block>,
    ) {
        for (cond, body) in elif_branches {
            // R41 T-FMT-C: ORDER IS LOAD-BEARING — blank check, then leading
            // comments, then the header. Reversed, a `blank` / `# note` /
            // `elif` run comes out comment-then-blank and the comment detaches
            // from the clause it documents. Mirrors `format_block_stmts`.
            if self.blank_before_clause(cond.span.start) {
                self.emitter.blank_line();
            }
            // R41 T-FMT-A follow-up: a comment on its own line BEFORE `elif`
            // documents the BRANCH, but with no leading hook here it fell
            // through to `format_block_stmts` and was re-emitted INSIDE the
            // branch body — same misattribution class as the match/select arm
            // loops. Claim it at branch indent first.
            self.emit_comments_before(cond.span.start);
            self.emitter.write("elif ");
            // B2 — the `elif` sibling of B1, same two layout cells.
            let cond_tail = self.suite_header_reserve(body);
            self.with_tail_reserve(cond_tail, |s| s.format_expr(cond));
            if body.layout == SuiteLayout::Inline {
                self.format_inline_suite(":", body, cond.span.end);
            } else {
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(cond.span.end);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
        }
        if let Some(else_body) = else_body {
            // Same class as the `elif` hooks above. There is no recorded span
            // for the `else` KEYWORD, but the body's span starts at the
            // clause's own colon (the parser records `start` BEFORE consuming
            // it) — after any comment written above `else:`, and on the
            // clause's own source line, so it is a sound anchor for both the
            // blank check and the comment hook.
            if self.blank_before_clause(else_body.span.start) {
                self.emitter.blank_line();
            }
            self.emit_comments_before(else_body.span.start);
            if else_body.layout == SuiteLayout::Inline {
                self.emitter.write("else");
                self.format_inline_suite(":", else_body, else_body.span.start);
            } else {
                self.emit_else_header(else_body.span.start);
                self.emitter.indent();
                self.format_block_stmts(else_body);
                self.emitter.dedent();
            }
        }
    }

    fn format_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                is_const,
                is_mutable,
                shared,
                type_,
                pattern,
                value,
            } => {
                if *is_const {
                    self.emitter.write("const ");
                } else if *is_mutable {
                    self.emitter.write("mutable ");
                }
                match shared {
                    SharedKind::Auto => self.emitter.write("shared "),
                    SharedKind::RwLock => self.emitter.write("shared(rwlock) "),
                    SharedKind::Atomic => self.emitter.write("shared(atomic) "),
                    SharedKind::None => {}
                }
                // type-first: `type name = expr`
                //
                // A10's var-decl cell. The type's tail is ` name = ` PLUS the
                // initializer's leading unbreakable text: an atomic init (a
                // literal, an identifier) goes straight through the Emitter
                // with no fit test anywhere, so the whole of it rides on this
                // line and stopping the reserve at ` = ` under-reserves.
                let decl_tail = self.measured_reserve(|s| {
                    s.emitter.write(" ");
                    s.format_var_decl_pattern(type_, pattern);
                    s.emitter.write(" = ");
                    s.format_expr(value);
                });
                self.with_tail_reserve(decl_tail, |s| s.format_type(type_));
                self.emitter.write(" ");
                self.format_var_decl_pattern(type_, pattern);
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Expr(expr) => {
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Assign { target, value } => {
                // Family O — a statement operator written BETWEEN two renders.
                // The target's own render is blind to the ` = <value…>` that
                // lands on its line; the value's leading unbreakable text
                // counts in, exactly as at the var-decl above.
                let tail = self.measured_reserve(|s| {
                    s.emitter.write(" = ");
                    s.format_expr(value);
                });
                self.with_tail_reserve(tail, |s| s.format_expr(target));
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::CompoundAssign { target, op, value } => {
                // Family O, compound costume.
                let tail = self.measured_reserve(|s| {
                    s.emitter.write(" ");
                    s.emitter.write(compound_op_str(*op));
                    s.emitter.write(" ");
                    s.format_expr(value);
                });
                self.with_tail_reserve(tail, |s| s.format_expr(target));
                self.emitter.write(" ");
                self.emitter.write(compound_op_str(*op));
                self.emitter.write(" ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Return(None) => {
                self.emitter.write("return");
                self.emitter.newline();
            }
            Stmt::Return(Some(expr)) => {
                self.emitter.write("return ");
                // Bare tuple: emit `a, b` not `(a, b)` in return position
                if let Expr::TupleLiteral(elems) = &expr.node {
                    for (i, e) in elems.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_expr(e);
                    }
                } else {
                    self.format_expr(expr);
                }
                self.emitter.newline();
            }
            Stmt::Throw(expr) => {
                self.emitter.write("throw ");
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Break => {
                self.emitter.write("break");
                self.emitter.newline();
            }
            Stmt::Continue => {
                self.emitter.write("continue");
                self.emitter.newline();
            }
            Stmt::Pass => {
                self.emitter.write("pass");
                self.emitter.newline();
            }
            Stmt::For {
                pattern,
                ownership,
                iterable,
                body,
                else_body,
            } => {
                self.emitter.write("for ");
                // Bare tuple: emit `x, y` not `(x, y)` in for-loop pattern
                if let Pattern::Tuple(pats) = &pattern.node {
                    for (i, p) in pats.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_pattern(p);
                    }
                } else {
                    self.format_pattern(pattern);
                }
                self.emitter.write(" in ");
                self.format_ownership_prefix(*ownership);
                // R41 T-FMT-A: `parse_for_stmt` strips the iterable's
                // ownership sigil BEFORE parsing the iterable expression, so
                // an iterable whose own emission leads with `&`/`!`/`^` must
                // be parenthesised or the reparse steals it into `ownership`.
                //
                // B4 — FIXED 1: the header's `:`. (A `for` suite is always
                // indented; there is no inline `for` layout to measure.)
                self.with_tail_reserve(1, |s| s.format_ownership_modifier_operand(iterable));
                self.emitter.write(":");
                // R39 gorget-arena block-header trailing (owner 2026-08-09):
                // preserve `for x in xs:  # comment` as trailing on the header
                // line, not pushed onto the first body stmt. Same class as
                // snag #2; anchor at iterable.span.end (before `:` and any
                // trailing comment on the same source line).
                self.emit_trailing_comment_after_header(iterable.span.end);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
                if let Some(else_body) = else_body {
                    // R41 T-FMT-C: the `for`/`while` `else` clauses are
                    // members of the clause-header class — same blank deletion
                    // and same comment misattribution as `elif`/`else`, at
                    // sites the filed report never named.
                    if self.blank_before_clause(else_body.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(else_body.span.start);
                    self.emit_else_header(else_body.span.start);
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
            }
            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                self.emitter.write("while ");
                // B3 — FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(condition));
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(condition.span.end);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
                if let Some(else_body) = else_body {
                    // Clause-header class — see the `for` `else` above.
                    if self.blank_before_clause(else_body.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(else_body.span.start);
                    self.emit_else_header(else_body.span.start);
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
            }
            Stmt::Loop { body } => {
                self.emitter.write("loop:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                self.emitter.write("if ");
                // B1 — the two suite-layout cells.
                let cond_tail = self.suite_header_reserve(then_body);
                self.with_tail_reserve(cond_tail, |s| s.format_expr(condition));
                if then_body.layout == SuiteLayout::Inline {
                    self.format_inline_suite(":", then_body, condition.span.end);
                } else {
                    self.emitter.write(":");
                    self.emit_trailing_comment_after_header(condition.span.end);
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(then_body);
                    self.emitter.dedent();
                }
                self.format_elif_else_blocks(elif_branches, else_body.as_ref());
            }
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.emitter.write("match ");
                // B5 — FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(scrutinee));
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(scrutinee.span.end);
                self.emitter.newline();
                self.emitter.indent();
                for item in arms {
                    // Clause-header class, `case` face: an arm header is not a
                    // statement either, so `format_block_stmts` never sees the
                    // author's blank above it. Same ORDER as every other clause
                    // site — blank, then leading comments, then the header.
                    let arm_anchor = match item {
                        crate::parser::ast::MatchItem::Arm(arm) => arm.span.start,
                        crate::parser::ast::MatchItem::MetaFor { span, .. } => span.start,
                    };
                    if self.blank_before_clause(arm_anchor) {
                        self.emitter.blank_line();
                    }
                    match item {
                        crate::parser::ast::MatchItem::Arm(arm) => {
                            self.emit_comments_before(arm.span.start);
                            self.format_match_arm(arm);
                        }
                        crate::parser::ast::MatchItem::MetaFor { vars, range, arm_template, span } => {
                            self.emitter.write("meta for ");
                            let joined = vars.iter().map(|v| v.node.as_str()).collect::<Vec<_>>().join(", ");
                            self.emitter.write(&joined);
                            self.emitter.write(" in ");
                            // B10 — `MatchItem::MetaFor`, FIXED 1.
                            self.with_tail_reserve(1, |s| s.format_expr(range));
                            self.emitter.write(":");
                            self.emitter.newline();
                            self.emitter.indent();
                            self.format_match_arm(arm_template);
                            // SITE 13 — the `meta for …:` block INSIDE a
                            // match statement owns its own indent, child and
                            // dedent, so it gets its own flush anchored on
                            // ITS header line. With only the match
                            // container's flush the tail is re-parented to
                            // the ARMS level, one block out from where it
                            // was written.
                            self.emit_orphan_comments_before_close(span.start, span.end);
                            self.emitter.dedent();
                        }
                    }
                }
                if let Some(else_body) = else_arm {
                    // Clause-header class. ⚠ NO layout read here: a
                    // statement-match `else: stmt` is a PARSE ERROR, so an
                    // inline branch at this site could never be reached and a
                    // cell for it could never go RED — a guard that cannot
                    // catch its own class is worse than none.
                    if self.blank_before_clause(else_body.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(else_body.span.start);
                    self.emit_else_header(else_body.span.start);
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                // ARM-CONTAINER flush. The children here are ARMS, not
                // statements, so `format_block_stmts` — the routed
                // chokepoint — is structurally absent and this container
                // needs its own. Anchored on the CONTAINER's header line: a
                // body-indent tail was already claimed by the arm's own
                // flush, and a tail at the arms' indent fails the arm's
                // column test and is claimed here.
                //
                // ⚠ The `SuiteLayout::NextLine` guard has no subject at an
                // arm container — it carries no `Block` — which is benign,
                // not an omission.
                self.emit_orphan_comments_before_close(stmt.span.start, stmt.span.end);
                self.emitter.dedent();
            }
            Stmt::Select { arms, else_arm } => {
                self.emitter.write("select:");
                self.emitter.newline();
                self.emitter.indent();
                for arm in arms {
                    // Clause-header class, `case` face — the select sibling of
                    // the two match arm loops.
                    if self.blank_before_clause(arm.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(arm.span.start);
                    self.emitter.write("case ");
                    // B10 — the `select` arm is a MULTI-RENDER header: two or
                    // three width-decided renders with DIFFERENT tails, so one
                    // row per header would under-specify it. Each reserve is
                    // MEASURED to the next render's first break opportunity —
                    // the `.send(` head, the `.recv():` tail — never the whole
                    // arm text.
                    match &arm.op {
                        SelectOp::Recv { type_, name, channel } => {
                            let type_tail = self.measured_reserve(|s| {
                                s.emitter.write(" ");
                                s.emitter.write(&name.node);
                                s.emitter.write(" = ");
                                s.format_expr(channel);
                            });
                            self.with_tail_reserve(type_tail, |s| s.format_type(type_));
                            self.emitter.write(" ");
                            self.emitter.write(&name.node);
                            self.emitter.write(" = ");
                            self.with_tail_reserve(8, |s| s.format_expr(channel));
                            self.emitter.write(".recv()");
                        }
                        SelectOp::Send { channel, value } => {
                            // `.send(` — the value's own render fit-tests from
                            // inside the parens onward.
                            self.with_tail_reserve(6, |s| s.format_expr(channel));
                            self.emitter.write(".send(");
                            self.with_tail_reserve(2, |s| s.format_expr(value));
                            self.emitter.write(")");
                        }
                    }
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(&arm.body);
                    self.emitter.dedent();
                }
                if let Some(else_body) = else_arm {
                    // Clause-header class — see the `for` `else` above.
                    if self.blank_before_clause(else_body.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(else_body.span.start);
                    self.emit_else_header(else_body.span.start);
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                // ARM-CONTAINER flush — see `Stmt::Match`.
                self.emit_orphan_comments_before_close(stmt.span.start, stmt.span.end);
                self.emitter.dedent();
            }
            Stmt::With { bindings, body } => {
                self.emitter.write("with ");
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    // Q3 PRESERVE: the bare `with r:` form is not shorthand
                    // the formatter gets to expand — `with r as r:` is a
                    // different thing to read.
                    //
                    // B10 — a NON-LAST binding's tail is the `as` alias plus
                    // `", "` plus the NEXT binding's leading text (MEASURED);
                    // the last one's is the alias plus the header's `:`.
                    let rest = &bindings[i + 1..];
                    let tail = self.measured_reserve(|s| {
                        if binding.explicit_as {
                            s.emitter.write(" as ");
                            s.emitter.write(&binding.name.node);
                        }
                        for b in rest {
                            s.emitter.write(", ");
                            s.format_expr(&b.expr);
                        }
                        s.emitter.write(":");
                    });
                    self.with_tail_reserve(tail, |s| s.format_expr(&binding.expr));
                    if binding.explicit_as {
                        self.emitter.write(" as ");
                        self.emitter.write(&binding.name.node);
                    }
                }
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::Unsafe { body } => {
                self.emitter.write("unsafe:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::Assert { condition, message } => {
                self.emitter.write("assert ");
                // Family O — the `, <message>` an assert writes after its
                // condition is on the condition's own line.
                let tail = self.measured_reserve(|s| {
                    if let Some(msg) = message {
                        s.emitter.write(", ");
                        s.format_expr(msg);
                    }
                });
                self.with_tail_reserve(tail, |s| s.format_expr(condition));
                if let Some(msg) = message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Stmt::AssertReturn { condition, message } => {
                self.emitter.write("assert return");
                // Family O — the assert-postcondition sibling.
                let tail = self.measured_reserve(|s| {
                    if let Some(msg) = message {
                        s.emitter.write(", ");
                        s.format_expr(msg);
                    }
                });
                self.with_tail_reserve(tail, |s| s.format_assert_return_expr(condition));
                if let Some(msg) = message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Stmt::Snapshot { name, value } => {
                self.emitter.write("snapshot ");
                self.emit_quoted_string(&name.node, Some(name.span));
                self.emitter.write(" ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Item(item) => {
                let spanned = Spanned::new(*item.clone(), stmt.span);
                self.format_item(&spanned);
            }
            Stmt::MetaIf {
                condition,
                then_body,
                elif_branches,
                else_body,
                ..
            } => {
                self.emitter.write("meta if ");
                // B10 — `Stmt::MetaIf`, FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(condition));
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(then_body);
                self.emitter.dedent();
                self.format_elif_else_blocks(elif_branches, else_body.as_ref());
            }
            Stmt::MetaFor { vars, range, body, .. } => {
                self.emitter.write("meta for ");
                let joined = vars.iter().map(|v| v.node.as_str()).collect::<Vec<_>>().join(", ");
                self.emitter.write(&joined);
                self.emitter.write(" in ");
                // B10 — `Stmt::MetaFor`, FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(range));
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
                self.emitter.write("meta match ");
                // B10 — `Stmt::MetaMatch`, FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(scrutinee));
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for (case_expr, body) in arms {
                    // B10 — the `meta match` case arm. Computed FIRST so the
                    // clause hooks below stay adjacent to their suite call.
                    let case_tail = self.suite_header_reserve(body);
                    // Clause-header class, `case` face — see the statement-match
                    // arm loop. Anchored at the case EXPRESSION, which sits on
                    // the clause's own source line.
                    if self.blank_before_clause(case_expr.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(case_expr.span.start);
                    self.emitter.write("case ");
                    self.with_tail_reserve(case_tail, |s| s.format_expr(case_expr));
                    if body.layout == SuiteLayout::Inline {
                        self.format_inline_suite(":", body, case_expr.span.end);
                    } else {
                        self.emitter.write(":");
                        self.emitter.newline();
                        self.emitter.indent();
                        self.format_block_stmts(body);
                        self.emitter.dedent();
                    }
                }
                if let Some(else_body) = else_arm {
                    // Clause-header class + the inline layout read. Anchored at
                    // the clause's own colon, like every other site —
                    // `parse_meta_match_arm_body` used to start its span at
                    // the NEWLINE token instead, which put `span.start` on the
                    // line BELOW the clause and made the walk-back miss the
                    // author's blank entirely.
                    if self.blank_before_clause(else_body.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(else_body.span.start);
                    if else_body.layout == SuiteLayout::Inline {
                        self.emitter.write("else");
                        self.format_inline_suite(":", else_body, else_body.span.start);
                    } else {
                        self.emit_else_header(else_body.span.start);
                        self.emitter.indent();
                        self.format_block_stmts(else_body);
                        self.emitter.dedent();
                    }
                }
                // ARM-CONTAINER flush — see `Stmt::Match`.
                self.emit_orphan_comments_before_close(stmt.span.start, stmt.span.end);
                self.emitter.dedent();
            }
            Stmt::MetaWhile { condition, body, .. } => {
                self.emitter.write("meta while ");
                // B10 — `Stmt::MetaWhile`, FIXED 1.
                self.with_tail_reserve(1, |s| s.format_expr(condition));
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::MetaConst { name, value, .. } => {
                self.emitter.write("meta const ");
                self.emitter.write(&name.node);
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::MetaLog { args, .. } => {
                self.emitter.write("meta log ");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { self.emitter.write(", "); }
                    self.format_expr(arg);
                }
                self.emitter.newline();
            }
            Stmt::NamedScope { name, body } => {
                self.emitter.write(&name.node);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::OnError { body } => {
                self.emitter.write("on error");
                if body.layout == SuiteLayout::Inline {
                    // ⚠ THE INLINE FORM TAKES NO COLON. `on error print(1)`
                    // parses; `on error: print(1)` is a parse error. This is
                    // the site whose header spelling differs from every other
                    // suite in the family, which is why the inline emitter
                    // takes the suffix as a parameter instead of hardcoding
                    // `":"`.
                    self.format_inline_suite("", body, body.span.start);
                } else {
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(body);
                    self.emitter.dedent();
                }
            }
        }
    }

    /// The remaining clauses of an expression-position `if`, from a given
    /// point on. Used only to MEASURE a branch expression's tail (B6) — the
    /// emission walks the clauses itself.
    fn format_expr_if_tail(
        &mut self,
        elifs: &[(Spanned<Expr>, Spanned<Expr>)],
        else_branch: Option<&Spanned<Expr>>,
    ) {
        for (cond, body) in elifs {
            self.emitter.write(" elif ");
            self.format_expr(cond);
            self.emitter.write(": ");
            self.format_expr(body);
        }
        if let Some(e) = else_branch {
            self.emitter.write(" else: ");
            self.format_expr(e);
        }
    }

    /// The declared name(s) of a `Stmt::VarDecl`. Factored out so the emission
    /// and A10's var-decl tail measurement share one spelling.
    fn format_var_decl_pattern(&mut self, type_: &Spanned<Type>, pattern: &Spanned<Pattern>) {
        // For auto declarations with tuple patterns, emit bare (no parens):
        // `auto a, b = ...` not `auto (a, b) = ...`
        if matches!(&type_.node, Type::Inferred) {
            if let Pattern::Tuple(pats) = &pattern.node {
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_pattern(p);
                }
                return;
            }
        }
        self.format_pattern(pattern);
    }

    fn format_match_arm(&mut self, arm: &MatchArm) {
        self.emitter.write("case ");
        // B7 / B11. The GUARD is the arm header's width-decided render (the
        // pattern has no Doc layer at all — pattern wrapping is its own,
        // unimplemented feature). Its tail is the arm's `:` plus whatever
        // `format_arm_body` puts on this same line: nothing for an indented
        // suite (B7's block-body cell), ` do:` for an author-spelled `do`
        // suite, or the space plus the body's leading prefix for an inline
        // expression body (B11's arm member of the inline-BODY escape).
        self.format_pattern(&arm.pattern);
        if let Some(ref guard) = arm.guard {
            self.emitter.write(" if ");
            let guard_tail = 1 + self.arm_body_reserve(&arm.body);
            self.with_tail_reserve(guard_tail, |s| s.format_expr(guard));
        }
        self.emitter.write(":");
        // R39 gorget-arena block-header trailing: preserve `case P:  # comment`
        // as trailing on the header line. Uses arm.pattern.span.end (or
        // guard.span.end if a guard is present) as the anchor before the `:`
        // and any trailing comment.
        let arm_anchor = arm.guard.as_ref().map(|g| g.span.end).unwrap_or(arm.pattern.span.end);
        // R41 T-FMT-C: read the author's LAYOUT, not the AST shape. A `Block`
        // body is an indented suite only when the author indented it —
        // `case 1: throw "bad"` also arrives as a `Block`, because the parser
        // wraps a `throw`/`return` expression-prefix, and the old
        // `if let Expr::Block(..)` test could not tell the two apart. It
        // exploded every inline `throw`/`return` arm onto its own line.
        // The three body shapes and where each puts the header's trailing
        // comment live in `format_arm_body`. This site used to hand-mirror the
        // shape test and handle two of the three itself, which is how the
        // author-`do:` arm ended up placing the comment on the LAST statement
        // of the branch.
        self.format_arm_body(&arm.body, arm_anchor);
        // Terminates the arm. Required after an INLINE body (which has emitted
        // no newline yet); a no-op after an indented suite, whose last
        // statement already closed the line — `Emitter::newline` is idempotent
        // at line start.
        self.emitter.newline();
    }

    // ── Types ───────────────────────────────────────────────

    fn format_type(&mut self, ty: &Spanned<Type>) {
        match &ty.node {
            Type::Primitive(p) => {
                let text = self.type_lexeme_text(*p, ty.span);
                self.emitter.write(&text);
            }
            Type::Named { name, generic_args } => {
                self.emitter.write(&name.node);
                if !generic_args.is_empty() {
                    // `ty.span` ends one past the `]`, so only the `[`
                    // needs scanning — from the name end, which is the
                    // anchor the window rule requires here (a named type
                    // has nothing between its name and its `[`).
                    let gate = self
                        .delim_pos_after(
                            name.span.end,
                            generic_args[0].span.start,
                            b'[',
                        )
                        .map(|lb| (lb, ty.span.end));
                    self.format_generic_args_wrapped(generic_args, self.gate_or_scan_miss(gate));
                }
            }
            Type::Array { element, size } => {
                // A10 composition: a type's recursive arms install their own
                // INTRA-type tails ADDITIVELY as they recurse, so each inner
                // `write_doc` sees exactly its true remaining tail — the
                // scope-tightness rule holding at every level, not just the
                // outermost. `[size]` is the element's tail here.
                let tail = self.measured_reserve(|s| {
                    s.emitter.write("[");
                    s.format_expr(size);
                    s.emitter.write("]");
                });
                self.with_tail_reserve(tail, |s| s.format_type(element));
                self.emitter.write("[");
                self.format_expr(size);
                self.emitter.write("]");
            }
            Type::Slice { element } => {
                self.with_tail_reserve(2, |s| s.format_type(element));
                self.emitter.write("[]");
            }
            Type::Tuple(types) => {
                self.emitter.write("(");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    // Tail of member `i`: `", "` plus the NEXT member's leading
                    // unbreakable text, or `")"` for the last one.
                    let rest = &types[i + 1..];
                    let tail = self.measured_reserve(|s| {
                        for t in rest {
                            s.emitter.write(", ");
                            s.format_type(t);
                        }
                        s.emitter.write(")");
                    });
                    self.with_tail_reserve(tail, |s| s.format_type(ty));
                }
                self.emitter.write(")");
            }
            Type::Function {
                return_type,
                params,
                param_ownerships,
            } => {
                // D35 (docs/define-gorget/decisions.md, ratified 2026-07-26):
                // an unnamed parameter's sigil is spelled AFTER the type
                // (`int &`, `String !`) — uniform with the named form
                // (`Message &msg`) and with `Type::Ref`/`Type::Owned` above.
                // A10 composition again: the return type's tail is `(` plus
                // the first param's leading text (the param loop is
                // hand-rolled, so no Fill fit-tests after the `(`), and each
                // param's tail is its ownership sigil plus the rest of the
                // list.
                let fn_tail = self.measured_reserve(|s| {
                    s.emitter.write("(");
                    s.format_fn_type_params(params, param_ownerships, 0);
                    s.emitter.write(")");
                });
                self.with_tail_reserve(fn_tail, |s| s.format_type(return_type));
                self.emitter.write("(");
                self.format_fn_type_params(params, param_ownerships, 0);
                self.emitter.write(")");
            }
            Type::Ref(inner) => {
                self.with_tail_reserve(2, |s| s.format_type(inner));
                self.emitter.write(" &");
            }
            Type::Owned(inner) => {
                self.with_tail_reserve(2, |s| s.format_type(inner));
                // D27 Round A: type-arg suffix `Vector[T ^]` (was `Vector[T !]`).
                self.emitter.write(" ^");
            }
            Type::Pointer(inner) => {
                self.with_tail_reserve(1, |s| s.format_type(inner));
                self.emitter.write("*");
            }
            Type::SelfType => self.emitter.write("Self"),
            Type::Inferred => self.emitter.write("auto"),
        }
    }

    /// The `Type::Function` parameter list, from index `from` onward. Factored
    /// out so the emission and the A10 tail measurement share one spelling.
    fn format_fn_type_params(
        &mut self,
        params: &[Spanned<Type>],
        param_ownerships: &[Ownership],
        from: usize,
    ) {
        for (i, p) in params.iter().enumerate().skip(from) {
            if i > from {
                self.emitter.write(", ");
            }
            let sigil_w = match param_ownerships.get(i) {
                Some(Ownership::MutableBorrow) | Some(Ownership::Move) => 2,
                _ => 0,
            };
            let tail = sigil_w
                + self.measured_reserve(|s| {
                    if i + 1 < params.len() {
                        s.emitter.write(", ");
                        s.format_fn_type_params(params, param_ownerships, i + 1);
                    }
                    s.emitter.write(")");
                });
            self.with_tail_reserve(tail, |s| s.format_type(p));
            if let Some(ownership) = param_ownerships.get(i) {
                match ownership {
                    Ownership::MutableBorrow => self.emitter.write(" &"),
                    // D27 Round A: `Type ^` in fn-type param list (was `Type !`).
                    Ownership::Move => self.emitter.write(" ^"),
                    Ownership::Borrow => {}
                }
            }
        }
    }

    // ── Patterns ────────────────────────────────────────────

    fn format_pattern(&mut self, pat: &Spanned<Pattern>) {
        match &pat.node {
            Pattern::Wildcard => self.emitter.write("_"),
            Pattern::Literal(expr) => self.format_expr(expr),
            Pattern::Binding(name) => self.emitter.write(name),
            Pattern::Constructor { path, fields } => {
                for (i, seg) in path.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(".");
                    }
                    self.emitter.write(&seg.node);
                }
                if !fields.is_empty() {
                    self.emitter.write("(");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_pattern(field);
                    }
                    self.emitter.write(")");
                }
            }
            Pattern::Tuple(pats) => {
                self.emitter.write("(");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_pattern(p);
                }
                self.emitter.write(")");
            }
            Pattern::Or(alts) => {
                for (i, alt) in alts.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(" | ");
                    }
                    self.format_pattern(alt);
                }
            }
            Pattern::Rest => self.emitter.write(".."),
            Pattern::DotShorthand { variant, fields } => {
                self.emitter.write(".");
                self.emitter.write(&variant.node);
                if !fields.is_empty() {
                    self.emitter.write("(");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_pattern(field);
                    }
                    self.emitter.write(")");
                }
            }
        }
    }

    // ── Expressions ─────────────────────────────────────────

    /// Format an expression for `assert return`, replacing `__return__` with `return`.
    fn format_assert_return_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) if name == "__return__" => {
                // __return__ is a parser-internal placeholder; the surrounding
                // assert-return handler emits the keyword.
            }
            Expr::BinaryOp { left, op, right } => {
                // Family O in the assert-postcondition walk: this arm emits
                // the operator BETWEEN two renders, so the left one is charged
                // for ` op ` plus the right one's leading unbreakable text.
                let tail = self.measured_reserve(|s| {
                    s.emitter.write(" ");
                    s.emitter.write(binary_op_str(*op));
                    s.emitter.write(" ");
                    s.format_assert_return_expr(right);
                });
                self.with_tail_reserve(tail, |s| s.format_assert_return_expr(left));
                self.emitter.write(" ");
                self.emitter.write(binary_op_str(*op));
                self.emitter.write(" ");
                self.format_assert_return_expr(right);
            }
            _ => self.format_expr(expr),
        }
    }

    /// Emit `format_expr(expr)`, wrapping in `(...)` when `should_wrap` is true.
    /// Used by the paren-aware operand emitters (FMT-A, Round XXXVI) so a
    /// wrap-then-render path lives in ONE place — no ad-hoc `if { emit("(") ... }`
    /// scattered across every arm.
    fn format_expr_maybe_parens(&mut self, expr: &Spanned<Expr>, should_wrap: bool) {
        if should_wrap {
            self.emitter.write("(");
            // The precedence wrap's own `)` is a caller-emitted tail like any
            // other, and it is charged HERE rather than at the three operand
            // emitters — this is the one place that knows whether the wrap
            // fired, so a fourth emitter cannot forget it.
            self.with_tail_reserve(1, |s| s.format_expr(expr));
            self.emitter.write(")");
        } else {
            self.format_expr(expr);
        }
    }

    /// FMT-A helper: emit an infix operand, wrapping in parens when the
    /// operand's own precedence would misparse against the outer op.
    fn format_binop_operand(
        &mut self,
        operand: &Spanned<Expr>,
        outer_left_bp: u8,
        position: BinOpPos,
        outer_right_assoc: bool,
    ) {
        let wrap =
            needs_parens_as_binop_operand(&operand.node, outer_left_bp, position, outer_right_assoc);
        self.format_expr_maybe_parens(operand, wrap);
    }

    /// FMT-A helper: emit a prefix-operator operand, wrapping in parens
    /// when the operand's own precedence would misparse against the prefix.
    fn format_prefix_operand(&mut self, operand: &Spanned<Expr>, prefix_bp: u8) {
        let wrap = needs_parens_as_prefix_operand(&operand.node, prefix_bp);
        self.format_expr_maybe_parens(operand, wrap);
    }

    /// FMT-A helper: emit a postfix-operator receiver, wrapping in parens
    /// when the receiver's own precedence would misparse against the postfix.
    ///
    /// FAMILY P (R42) — the postfix operator the caller is about to write
    /// lands on the RECEIVER's own last line: a packed call whose `)` ends in
    /// budget can still be carried past it by a `.field` the packer never saw.
    /// `operator_tail` is that operator's text up to ITS first break
    /// opportunity (`.name` wholly fixed; `[` plus the index's leading text;
    /// `!`; `?.name`; `.await()`), and it is a REQUIRED parameter precisely so
    /// a ninth postfix site cannot silently skip it — the chokepoint installs
    /// it, the caller only states it. Additive, so a chain of postfixes
    /// accumulates its own tails.
    fn format_postfix_receiver(&mut self, receiver: &Spanned<Expr>, operator_tail: usize) {
        let wrap = needs_parens_as_postfix_receiver(&receiver.node);
        // The precedence wrap's own `)` is charged inside
        // `format_expr_maybe_parens`, which is the one place that knows
        // whether it fired.
        self.with_tail_reserve(operator_tail, |s| {
            s.format_expr_maybe_parens(receiver, wrap)
        });
    }

    /// R41 T-FMT-A: emit an expression at a position where the parser runs
    /// `parse_ownership_modifier` BEFORE `parse_expr`, wrapping in parens
    /// when the emission would otherwise begin with a sigil that pre-pass
    /// would swallow.
    ///
    /// This is a fourth sibling of the precedence-aware operand emitters
    /// above, and it exists because these positions have a parse ORDER the
    /// binding-power table cannot express: the sigil (`&` / `!` / `^`) is
    /// stripped by a *token* pre-pass into the node's own `ownership` field
    /// and never reaches the Pratt parser. So an expression that legitimately
    /// *starts* with a move/borrow sigil (because the author parenthesised it
    /// to force the expression reading) round-trips into a DIFFERENT AST:
    /// the sigil migrates from the expression into the enclosing node's
    /// `ownership` field. That is an accept/reject change, not a cosmetic one
    /// — RED-verified in both directions:
    ///
    ///   - `for i in (^start)..end:` rejects `E_MoveInOperandPosition`, but
    ///     re-emitted as `for i in ^start..end:` it is ACCEPTED (the sigil
    ///     became the iterable modifier).
    ///   - `apply_once((^(int x): x + n), 3)` COMPILES AND PRINTS `10`, but
    ///     re-emitted as `apply_once(^(int x): x + n, 3)` it is REJECTED
    ///     `E_OwnershipMismatch` — the move-CLOSURE became a moved argument.
    ///
    /// The parens are not "author paren preservation" (no `Paren` node
    /// exists, and span recovery would silently fall back to the canonical
    /// spelling, reopening the flip with the guard green). They are a pure
    /// function of the parsed program: emit exactly the parens the reparse
    /// requires, and no others.
    fn format_ownership_modifier_operand(&mut self, operand: &Spanned<Expr>) {
        let wrap = emits_leading_ownership_sigil(&operand.node);
        self.format_expr_maybe_parens(operand, wrap);
    }

    // ── The verbatim chokepoint ─────────────────────────────
    //
    // Several kinds of authorial choice are SYNTAX, not semantics, so the AST
    // is right to drop them (Layering rule 1: lossy on syntax, lossless on
    // invariants): an int literal's radix and digit grouping, a float's
    // trailing zeros, whether a byte was written `b'A'` or `65`, which escape
    // spelled a character, which quote style wrapped a string, whether a
    // primitive type was spelled `byte` or `uint8`. The formatter is the ONE
    // consumer that needs them back, and it recovers them the same way it
    // recovers comments: from the source text at the node's span.
    //
    // Every one of those arms goes through `verbatim` below, and `verbatim`
    // enforces the property that makes the whole class safe: the recovered
    // lexeme is RE-LEXED and compared against the AST value the caller is
    // about to emit. A stale, synthetic, or merely mis-computed span cannot
    // produce output that means something other than the node it came from —
    // it can only fall back to the canonical spelling. That is the difference
    // between preservation and a silent miscompile of the user's source.

    /// Recover the author's own lexeme for the node at `span`, but only if
    /// re-lexing it yields a token that `accepts` — i.e. one that denotes
    /// EXACTLY the value being emitted.
    ///
    /// Total on every span: `str::get` returns `None` for an out-of-bounds or
    /// synthetic span (f-string interpolation sub-expressions are parsed at a
    /// synthetic base offset), for an empty sub-formatter source, and for a
    /// span that does not land on char boundaries. There is no panic path and
    /// no arm that needs to know why recovery failed — it just emits canonical.
    fn verbatim(&self, span: Span, accepts: impl FnOnce(&Token) -> bool) -> Option<&str> {
        let lexeme = self.source.get(span.start..span.end)?;
        let tok = relex_single_token(lexeme)?;
        if accepts(&tok) {
            Some(lexeme)
        } else {
            None
        }
    }

    /// Choose the surface text for an integer literal (gorget-js snag #15f).
    ///
    /// Preserves the author's radix, hex digit-case and `_` grouping — and,
    /// because the oracle is the lexer itself, the BYTE-literal spelling
    /// `b'A'` too (it lexes to `IntLiteral(65)`, so a hand-written
    /// integer-syntax mirror could never accept it).
    fn int_literal_text(&self, n: i64, span: Span) -> String {
        match self.verbatim(span, |t| matches!(t, Token::IntLiteral(v) if *v == n)) {
            Some(lexeme) => lexeme.to_string(),
            None => n.to_string(),
        }
    }

    /// Choose the surface text for a float literal.
    ///
    /// `format!("{n}")` prints the shortest decimal that round-trips, which is
    /// value-preserving and form-destroying: `1.50` came back as `1.5`, and a
    /// table of aligned constants lost its alignment. The comparison is on the
    /// f64 VALUE, so the recovered lexeme provably denotes the same number.
    fn float_literal_text(&self, n: f64, span: Span) -> String {
        if let Some(lexeme) =
            self.verbatim(span, |t| matches!(t, Token::FloatLiteral(v) if *v == n))
        {
            return lexeme.to_string();
        }
        let s = format!("{}", n);
        // Ensure it still looks like a float.
        if !s.contains('.') && !s.contains('e') && !s.contains('E') {
            format!("{}.0", s)
        } else {
            s
        }
    }

    /// Choose the surface text for a primitive TYPE.
    ///
    /// `byte` is a user-facing alias the lexer folds into `Keyword::Uint8`
    /// (`src/lexer/token.rs`), so by the time the parser builds
    /// `Type::Primitive(Uint8)` the author's spelling is gone — the same
    /// situation as a literal's radix, and it takes the same cure. The
    /// re-lexed keyword is mapped back through the parser's own
    /// keyword-to-primitive table, so an alias is accepted only when it
    /// denotes the very primitive being emitted.
    fn type_lexeme_text(&self, p: PrimitiveType, span: Span) -> String {
        let accepts = |t: &Token| match t {
            Token::Keyword(kw) => crate::parser::types::keyword_to_primitive(*kw) == Some(p),
            _ => false,
        };
        match self.verbatim(span, accepts) {
            Some(lexeme) => lexeme.to_string(),
            None => primitive_type_str(p).to_string(),
        }
    }

    /// Choose the surface text for a string literal, INCLUDING its delimiters.
    ///
    /// Returns `None` when the author's lexeme cannot be recovered, leaving the
    /// caller to emit the canonical spelling. The acceptance test is structural
    /// equality of the re-lexed literal with the AST node — same kind, same
    /// segments — which pins quote style, prefix letter, escape spelling, the
    /// f-string brace form, AND the physical line layout of a `"""` block, all
    /// in one check.
    fn string_literal_text(&self, s: &StringLiteral, span: Span) -> Option<String> {
        let accepts = |t: &Token| match t {
            Token::StringLiteral(relexed) => {
                relexed.kind == s.kind && relexed.segments == s.segments
            }
            _ => false,
        };
        self.verbatim(span, accepts).map(|l| l.to_string())
    }

    /// Choose the surface text for a NAME-string — a quoted string the AST
    /// stores DECODED, as a plain `String`: test and bench names, snapshot
    /// names, attribute string arguments, extern ABI tags and symbol names.
    ///
    /// Unlike `Expr::StringLiteral`, these carry no `StringLiteral` token to
    /// compare against, so the check is on the decoded text: the recovered
    /// lexeme must re-lex to a literal whose plain text is exactly `value`.
    /// That is what makes a stale-but-in-bounds span harmless — several of
    /// these fields are also written by compiler passes that synthesise nodes,
    /// and a span pointing at the wrong literal falls back to canonical
    /// escaping instead of emitting a name the AST does not contain.
    fn quoted_string_text(&self, value: &str, span: Option<Span>) -> String {
        if let Some(span) = span {
            let accepts = |t: &Token| match t {
                Token::StringLiteral(relexed) => relexed.as_plain_text() == value,
                _ => false,
            };
            if let Some(lexeme) = self.verbatim(span, accepts) {
                return lexeme.to_string();
            }
        }
        format!(
            "\"{}\"",
            canonical_string_escape(value, StringKind::Normal)
        )
    }

    /// THE producer for every quoted name-string the formatter emits.
    ///
    /// Eight sites used to spell this by hand as `write("\"")` + `write(name)`
    /// + `write("\"")`, which re-emitted the DECODED text with no re-escaping:
    /// `\x41` came back as `A`, a `\t` as a literal TAB, and an escaped quote
    /// as a BARE quote that terminated the string early and broke the file.
    /// Routing all eight through one producer is what makes the class fixable
    /// once; `formatter_verbatim_emit_arm_count` in `tests/lints.rs` is what
    /// keeps a ninth site from spelling its own quotes again.
    ///
    /// `write_preformatted` rather than `write`: a recovered lexeme may be a
    /// multi-line `"""` block, and `write` would advance the emitter's column
    /// by the literal's whole byte length, desyncing every later fit decision
    /// on the line. (Only fit decisions — `emitter.col`'s one consumer is
    /// `write_doc`. Trailing comments are injected into the finished buffer and
    /// aligned by `plan_trailing_aligns`, which never reads the column.)
    fn emit_quoted_string(&mut self, value: &str, span: Option<Span>) {
        let text = self.quoted_string_text(value, span);
        self.emitter.write_preformatted(&text);
    }

    fn format_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::IntLiteral(n) => {
                let text = self.int_literal_text(*n, expr.span);
                self.emitter.write(&text);
            }
            Expr::FloatLiteral(n) => {
                let text = self.float_literal_text(*n, expr.span);
                self.emitter.write(&text);
            }
            Expr::BoolLiteral(b) => {
                self.emitter.write(if *b { "true" } else { "false" });
            }
            Expr::StringLiteral(s, _) => {
                self.format_string_lit(s, expr.span);
            }
            Expr::NoneLiteral => self.emitter.write("None"),
            Expr::Identifier(name) => self.emitter.write(name),
            Expr::SelfExpr => self.emitter.write("self"),
            Expr::Path { segments } => {
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(".");
                    }
                    self.emitter.write(&seg.node);
                }
            }
            Expr::UnaryOp { op, operand } => {
                self.emitter.write(unary_op_str(*op));
                // FMT-A: `not` parses operand at bp 20; `-`/`~` at bp 33
                // (src/parser/expr.rs:530,554,621).
                let prefix_bp = match op {
                    UnaryOp::Not => 20,
                    UnaryOp::Neg | UnaryOp::BitNot => 33,
                };
                self.format_prefix_operand(operand, prefix_bp);
            }
            Expr::BinaryOp { left, op, right } => {
                self.format_binary_chain(left, *op, right);
            }
            Expr::Call {
                callee,
                generic_args,
                args,
            } => {
                // FMT-A: callee is a postfix-position receiver (bp 35). A
                // BinaryOp callee like `(f + g)(x)` must stay wrapped.
                let (ga_gate, args_gate) = self.callee_arg_gates(
                    callee.span.end,
                    generic_args.as_deref(),
                    expr.span.end,
                    args.first().map(|a| a.span.start),
                    args.last().map(|a| a.span.end),
                );
                // P — the callee's tail is the argument list's open delimiter
                // (`[` or `(`), or the whole literal `()` when there is no
                // fit-tested node after it at all.
                let callee_tail = self.measured_reserve(|s| {
                    if let Some(ga) = generic_args {
                        s.format_generic_args_wrapped(
                            ga,
                            ga_gate.expect("generic args present => generic-args gate derived"),
                        );
                    }
                    s.format_call_args_wrapped(args, args_gate);
                });
                self.format_postfix_receiver(callee, callee_tail);
                if let Some(ga) = generic_args {
                    self.format_generic_args_wrapped(
                        ga,
                        ga_gate.expect("generic args present => generic-args gate derived"),
                    );
                }
                self.format_call_args_wrapped(args, args_gate);
            }
            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                // Detect method chains (2+ consecutive .method() calls).
                // Flatten and wrap with Doc for line-width-aware breaking.
                let chain_len = method_chain_length(expr);
                if chain_len >= 2 {
                    self.format_method_chain(expr);
                } else {
                    // FMT-A: receiver at postfix bp 35 — wrap infix/prefix
                    // receivers like `(a + b).foo()`.
                    let (ga_gate, args_gate) = self.callee_arg_gates(
                        method.span.end,
                        generic_args.as_deref(),
                        expr.span.end,
                        args.first().map(|a| a.span.start),
                        args.last().map(|a| a.span.end),
                    );
                    // P — `.method` plus the argument list's open delimiter.
                    let recv_tail = self.measured_reserve(|s| {
                        s.emitter.write(".");
                        s.emitter.write(&method.node);
                        if let Some(ga) = generic_args {
                            s.format_generic_args_wrapped(
                                ga,
                                ga_gate
                                    .expect("generic args present => generic-args gate derived"),
                            );
                        }
                        s.format_call_args_wrapped(args, args_gate);
                    });
                    self.format_postfix_receiver(receiver, recv_tail);
                    self.emitter.write(".");
                    self.emitter.write(&method.node);
                    if let Some(ga) = generic_args {
                        self.format_generic_args_wrapped(
                            ga,
                            ga_gate.expect("generic args present => generic-args gate derived"),
                        );
                    }
                    self.format_call_args_wrapped(args, args_gate);
                }
            }
            Expr::FieldAccess { object, field } => {
                // FMT-A: object at postfix bp 35.
                // P — `.name` is wholly fixed text, no break opportunity.
                self.format_postfix_receiver(object, 1 + field.node.chars().count());
                self.emitter.write(".");
                self.emitter.write(&field.node);
            }
            Expr::TupleFieldAccess { object, index } => {
                // FMT-A: object at postfix bp 35.
                let index_text = index.to_string();
                self.format_postfix_receiver(object, 1 + index_text.chars().count());
                self.emitter.write(".");
                self.emitter.write(&index_text);
            }
            Expr::Index { object, index } => {
                // FMT-A: object at postfix bp 35. `index` is inside `[...]`
                // brackets, which reset precedence — no wrap needed there.
                //
                // D22 colon-slice: a Range payload carrying `colon: true`
                // (from `v[a:b]` / `v[a:]` / `v[:b]` / `v[:]`) renders with
                // `:` between the endpoints — the parser preserves the
                // user's source shape via the marker rather than
                // canonicalising every Range payload to `:` (which would
                // also rewrite standalone `for i in a..b` iterables).
                // P — `[` plus the index expression's leading unbreakable
                // text. ⚠ Family O's `v[…] = 7` costume is NOT covered here:
                // this charges the INDEX RECEIVER; the index EXPRESSION's own
                // render carries `]` plus the statement operator, and that is
                // installed at the assignment sites.
                let index_tail = self.measured_reserve(|s| {
                    s.emitter.write("[");
                    s.format_expr(index);
                });
                self.format_postfix_receiver(object, index_tail);
                self.emitter.write("[");
                // Family O — the INDEX EXPRESSION's own tail: the `]` this arm
                // writes below PLUS whatever the enclosing statement still
                // writes (` = 7`, ` += 7`), which arrives through the ambient
                // reserve because `with_tail_reserve` is additive. Charging
                // only the receiver (family P, just above) leaves
                // `v[<packed call>] = 7` overrunning by exactly the
                // statement operator — measured, and its own §4 cell.
                if let Expr::Range { start, end, inclusive: false, colon: true } = &index.node {
                    // D22 slice: `[a:b]`, `[a:]`, `[:b]`, `[:]`. Endpoints
                    // are inside `[...]` — precedence resets, so no wrap
                    // logic needed for the operands.
                    if let Some(s) = start {
                        let slice_tail = 1 + self.measured_reserve(|f| {
                            f.emitter.write(":");
                            if let Some(e) = end {
                                f.format_expr(e);
                            }
                        });
                        self.with_tail_reserve(slice_tail, |f| f.format_expr(s));
                    }
                    self.emitter.write(":");
                    if let Some(e) = end {
                        self.with_tail_reserve(1, |f| f.format_expr(e));
                    }
                } else {
                    self.with_tail_reserve(1, |f| f.format_expr(index));
                }
                self.emitter.write("]");
            }
            Expr::Range {
                start,
                end,
                inclusive,
                colon: _,
            } => {
                // FMT-A: Range is bp 23 (postfix in parser sense, but rendered
                // infix here). START was parsed at the outer bp of whichever
                // context Range appeared in — treat as LEFT operand of an infix
                // at bp 23 (left-assoc). END is parsed at bp 24 (parser::expr.rs:1394,1411),
                // so END is a prefix operand at bp 24.
                //
                // D22 colon-slice `[a:b]` is only valid INSIDE an `Index`
                // wrapper, and the `Index` arm above catches that shape
                // before falling here. A standalone Range emits `..`/`..=`
                // regardless of the `colon` marker.
                // Family O — the range costume: `..` / `..=` plus the end
                // operand's leading unbreakable text ride on the START
                // operand's line.
                let op_text = if *inclusive { "..=" } else { ".." };
                if let Some(s) = start {
                    let start_tail = self.measured_reserve(|f| {
                        f.emitter.write(op_text);
                        if let Some(e) = end {
                            f.format_prefix_operand(e, 24);
                        }
                    });
                    self.with_tail_reserve(start_tail, |f| {
                        f.format_binop_operand(s, 23, BinOpPos::Left, false)
                    });
                }
                self.emitter.write(op_text);
                if let Some(e) = end {
                    self.format_prefix_operand(e, 24);
                }
            }
            Expr::OptionalChain { object, field } => {
                // FMT-A: object at postfix bp 35.
                // P — `?.name`, all fixed text.
                self.format_postfix_receiver(object, 2 + field.node.chars().count());
                self.emitter.write("?.");
                self.emitter.write(&field.node);
            }
            Expr::DefaultOp { lhs, rhs } => {
                // FMT-A: `??` is bp 3/4 (left-assoc). Only `Rethrow`/`Catch`
                // (bp 1/2) bind looser — those need operand-wrapping. E.g.
                // `(a rethrow b) ?? c` (handled by `format_binop_operand`).
                //
                // R38 Track C (Core #10 lower-or-reject): also wrap the whole
                // emission in `if_break` parens via
                // `wrap_multiline_expr_in_parens` so a long RHS that breaks
                // across lines emits `(a\n    ?? b)` (parseable) rather than
                // `a\n    ?? b` (unparseable — a bare leading `??`
                // continuation is not valid Gorget; the lexer only carves
                // out leading `.` at `src/lexer/mod.rs:161`). Same fix R36
                // applied to `format_binary_chain`; this arm was missed at
                // that time (gorget-js snag #15 Class 2).
                // C10, nil-coalesce carrier — same broken-layout rule as the
                // two chains above: the RHS inherits the caller's reserve plus
                // the `)` the paren wrap appends when broken; the LHS owns its
                // own line and carries nothing.
                let base = self.tail_reserve;
                let indent = self.emitter.indent + 1;
                let lhs_s = self.element_to_string_reserving(indent, 0, |f| {
                    f.format_binop_operand(lhs, 3, BinOpPos::Left, false);
                });
                let rhs_s = self.element_to_string_reserving(indent, base + 1, |f| {
                    f.format_binop_operand(rhs, 3, BinOpPos::Right, false);
                });
                let inner = doc::concat(vec![
                    doc::text(lhs_s),
                    doc::indent(doc::concat(vec![
                        doc::line(),
                        doc::text(format!("?? {rhs_s}")),
                    ])),
                ]);
                let nil_doc = wrap_multiline_expr_in_parens(inner);
                self.write_doc(&nil_doc);
            }
            Expr::Move { expr } => {
                // D27 Round A: prefix move sigil `^` (was `!`); `!` is now the error channel.
                self.emitter.write("^");
                // FMT-A: Move `^` parses operand at bp 33 (parser::expr.rs:596).
                self.format_prefix_operand(expr, 33);
            }
            // D29: postfix error-propagation renders the `!` AFTER the inner
            // expression. No bang-space corner here: a `!=`/`==` comparison is
            // a `BinaryOp` whose arm already emits ` != `/ ` == ` with spaces,
            // so a re-rendered `f()! != b` never fuses. (The raw-text migrator
            // handles bang-space when INSERTING into un-spaced source.)
            Expr::Propagate { expr } => {
                // FMT-A: Propagate is POSTFIX at bp 35 — a bare BinaryOp
                // operand like `(a + b)!` needs wrap, else `a + b!` reparses
                // as `Add(a, Propagate(b))`.
                // P — the propagate `!`.
                self.format_postfix_receiver(expr, 1);
                self.emitter.write("!");
            }
            Expr::MutableBorrow { expr } => {
                self.emitter.write("&");
                // FMT-A: MutableBorrow `&` parses operand at bp 33 (parser::expr.rs:635).
                self.format_prefix_operand(expr, 33);
            }
            Expr::Deref { expr } => {
                self.emitter.write("*");
                // FMT-A: Deref `*` parses operand at bp 33 (parser::expr.rs:576).
                self.format_prefix_operand(expr, 33);
            }
            Expr::If {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            } => {
                // B6 — the expression-position `if` is a MULTI-RENDER header:
                // every branch expression and every following clause keyword
                // shares ONE line, so each render's tail is the next
                // `": "` / `" elif "` / `" else: "` plus the following
                // render's own leading unbreakable text.
                self.emitter.write("if ");
                // Each measurement runs to the END of the clause chain, never
                // just to the next branch: the width-0 probe truncates at the
                // first REAL break opportunity, and when a branch is an atom
                // (`1111111`) there is none, so the boundary carries on into
                // ` else: `. Stopping at the next branch under-reserved a
                // measured 121-char line.
                let cond_tail = self.measured_reserve(|s| {
                    s.emitter.write(": ");
                    s.format_expr(then_branch);
                    s.format_expr_if_tail(elif_branches, else_branch.as_deref());
                });
                self.with_tail_reserve(cond_tail, |s| s.format_expr(condition));
                self.emitter.write(": ");
                let then_tail = self.measured_reserve(|s| {
                    s.format_expr_if_tail(elif_branches, else_branch.as_deref());
                });
                self.with_tail_reserve(then_tail, |s| s.format_expr(then_branch));
                for (i, (cond, body)) in elif_branches.iter().enumerate() {
                    self.emitter.write(" elif ");
                    let rest = &elif_branches[i + 1..];
                    let elif_cond_tail = self.measured_reserve(|s| {
                        s.emitter.write(": ");
                        s.format_expr(body);
                        s.format_expr_if_tail(rest, else_branch.as_deref());
                    });
                    self.with_tail_reserve(elif_cond_tail, |s| s.format_expr(cond));
                    self.emitter.write(": ");
                    let body_tail = self.measured_reserve(|s| {
                        s.format_expr_if_tail(rest, else_branch.as_deref());
                    });
                    self.with_tail_reserve(body_tail, |s| s.format_expr(body));
                }
                if let Some(else_branch) = else_branch {
                    self.emitter.write(" else: ");
                    self.format_expr(else_branch);
                }
            }
            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.emitter.write("match ");
                // B6 — `Expr::Match` writes `":"` and then a NEWLINE, so its
                // scrutinee tail is FIXED 1. A measured whole-arm reserve here
                // would be the wrong KIND and would break in-budget scrutinees
                // corpus-wide.
                self.with_tail_reserve(1, |s| s.format_expr(scrutinee));
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for arm in arms {
                    // Clause-header class, `case` face — see the statement-match
                    // arm loop. Blank, then comments, then the header.
                    if self.blank_before_clause(arm.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(arm.span.start);
                    self.format_match_arm(arm);
                }
                if let Some(else_arm) = else_arm {
                    // Clause-header class — the expression-position `else` had
                    // neither the blank check nor a leading-comment hook, so a
                    // comment written above it landed after the whole match.
                    // The anchor is the clause's colon for an indented body
                    // and the inline expression's start otherwise; both sit on
                    // the `else:` source line, which is all either use needs.
                    if self.blank_before_clause(else_arm.span.start) {
                        self.emitter.blank_line();
                    }
                    self.emit_comments_before(else_arm.span.start);
                    self.emitter.write("else:");
                    // The header's trailing comment is placed by
                    // `format_arm_body`, which is the one function that knows
                    // which of the three body shapes it is about to emit. This
                    // site used to re-derive that decision and fire the hook
                    // itself, which is two sources of truth for one predicate.
                    self.format_arm_body(else_arm, else_arm.span.start);
                    // Terminates the clause. Required for the INLINE form
                    // (`else: 20` has emitted no newline yet); a no-op for the
                    // indented form, whose last statement already closed the
                    // line — `Emitter::newline` is idempotent at line start.
                    self.emitter.newline();
                }
                // ARM-CONTAINER flush — the expression-position match, whose
                // children are arms. See `Stmt::Match`.
                self.emit_orphan_comments_before_close(expr.span.start, expr.span.end);
                self.emitter.dedent();
            }
            Expr::Block(block) => {
                // gorget-js critique #2 (2026-05-13): `throw expr` and
                // `return [expr]` parse as expression prefixes by wrapping
                // the corresponding statement in a synthetic `Expr::Block`.
                // The formatter must round-trip those as the inline
                // expression form, not as `do:\n    throw expr` — the do-
                // wrapped form breaks `fmt_idempotent` (re-parsing the
                // do-block re-wraps it, then drops the surrounding var
                // decl as the syntactic shape drifts).
                //
                // gorget-js snag #15b (2026-08-09) extended the carve-out
                // to Stmt::Expr: a single-expression Block wrapped in `do:`
                // makes the expression a READ position, breaking move-sigil
                // tails (an INDENTED `else: do:` with a `^b` tail rejects
                // `E_MoveInOperandPosition`; `else: ^b` compiles — the one-line
                // `else: do: ^b` is a PARSE error, not this reject). Into
                // `try_inline_single_terminal_stmt` (R39 follow-up to
                // Tracks F/G/widening-fix); the helper handles all three
                // arms (Throw/Return/Expr) and is shared with Expr::Do.
                if self.try_inline_single_terminal_stmt(block) {
                    return;
                }
                self.emitter.write("do:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(block);
                self.emitter.dedent();
            }
            Expr::Do {
                body,
                author_spelled,
            } => {
                // gorget-js snag #15c widening (R39 follow-up to Track G,
                // 2026-08-09): a `do:` wrapping a SINGLE bare terminal
                // expression (Throw/Return/Expr) inlines to the bare form,
                // matching Expr::Block's carve-out above. Reached via
                // `format_arm_body` for `catch (e):`/`rethrow (e):` bodies
                // whose parser (`parse_body_or_expr`) synthesizes an
                // Expr::Do wrap around an indented body — without this
                // mirror, `catch (e):\n    fallback(x)` reformats to
                // `catch (e): do:\n    fallback(x)` (cosmetic rot + snag
                // #15b move-tail regression class for Throw/Return).
                // Consolidated via the shared helper.
                //
                // R41 T-FMT-C: ONLY the synthesized wrap may be inlined away.
                // Applied to an author-written `do:`, this same inlining ATE
                // the keyword — `int s = do:` + `1 + 1` came back as
                // `int s = 1 + 1`. `author_spelled` is what separates the two
                // producers of this variant; the shape cannot.
                if !*author_spelled && self.try_inline_single_terminal_stmt(body) {
                    return;
                }
                self.emitter.write("do:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Expr::Closure {
                is_move,
                is_async,
                params,
                body,
            } => {
                if *is_async {
                    self.emitter.write("async ");
                }
                if *is_move {
                    // D27 Round A: move-closure prefix `^` (was `!`).
                    self.emitter.write("^");
                }
                // The closure's own span start anchors the scan: nothing
                // but the optional `async`/`^` prefixes precedes the
                // parameter `(`, and neither can contain one.
                let params_gate = self.paren_tuple_gate(
                    expr.span.start,
                    expr.span.end,
                    params.first().map(|p| p.span.start),
                    params.last().map(|p| p.span.end),
                );
                // A8 — the closure-parameter list's reserve SPLITS on the
                // BODY-LAYOUT axis. A block-bodied closure owes only its `:`;
                // an INLINE-bodied one owes the `:`, the space, and the body's
                // leading unbreakable prefix — it is the THIRD inline-BODY
                // sibling, and it is invisible to the `format_inline_suite` /
                // `format_arm_body` needles because it writes its body inline
                // inside this very arm. Measuring `":"` + the body and taking
                // the leading text yields both cells from one spelling.
                let params_tail = self.measured_reserve(|s| {
                    s.emitter.write(":");
                    s.format_closure_body(body, params);
                });
                let params_gate = self.gate_or_scan_miss(params_gate);
                self.with_tail_reserve(params_tail, |s| {
                    s.format_closure_params_wrapped(params, params_gate)
                });
                // R41 T-FMT-C: the colon and the separator are SEPARATE
                // writes. Writing `": "` unconditionally and then newlining
                // left `(int x): ` — a trailing space — on every block-bodied
                // closure, which the corpus-wide sweep would have baked in.
                self.emitter.write(":");
                self.format_closure_body(body, params);
            }
            Expr::ImplicitClosure { body } => {
                // ImplicitClosure is a parser artifact wrapping `it` expressions.
                // The formatter emits the body directly — the `it` keyword inside
                // already serves as the implicit parameter marker.
                self.format_expr(body);
            }
            Expr::ListComprehension {
                expr,
                variable,
                ownership,
                iterable,
                condition,
            } => {
                let var_s = self.element_to_string(|f| f.format_pattern(variable));
                let own_prefix = match ownership {
                    Ownership::Borrow => "",
                    Ownership::MutableBorrow => "&",
                    // D27 Round A: comprehension for-binder `^` (was `!`).
                    Ownership::Move => "^",
                };
                let r = self.comprehension_reserves();
                let expr_s =
                    self.element_to_string_reserving(r.indent, r.element, |f| f.format_expr(expr));
                let iter_s = self
                    .element_to_string_reserving(r.indent, r.iterable, |f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string_reserving(r.indent, r.condition, |f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "[", &expr_s, &var_s, own_prefix, &iter_s, cond_s.as_deref(), "]",
                );
                self.write_doc(&comp_doc);
            }
            Expr::DictComprehension {
                key,
                value,
                variables,
                iterable,
                condition,
            } => {
                let vars_s = variables.iter().map(|v| v.node.as_str())
                    .collect::<Vec<_>>().join(", ");
                let r = self.comprehension_reserves();
                let kv_s = self.element_to_string_reserving(r.indent, r.element, |f| {
                    f.format_expr(key);
                    f.emitter.write(": ");
                    f.format_expr(value);
                });
                let iter_s = self
                    .element_to_string_reserving(r.indent, r.iterable, |f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string_reserving(r.indent, r.condition, |f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "{", &kv_s, &vars_s, "", &iter_s, cond_s.as_deref(), "}",
                );
                self.write_doc(&comp_doc);
            }
            Expr::SetComprehension {
                expr,
                variable,
                iterable,
                condition,
            } => {
                let r = self.comprehension_reserves();
                let expr_s =
                    self.element_to_string_reserving(r.indent, r.element, |f| f.format_expr(expr));
                let iter_s = self
                    .element_to_string_reserving(r.indent, r.iterable, |f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string_reserving(r.indent, r.condition, |f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "{", &expr_s, &variable.node, "", &iter_s, cond_s.as_deref(), "}",
                );
                self.write_doc(&comp_doc);
            }
            Expr::ArrayLiteral(elems, spelling) => {
                // Q3 PRESERVE: a set literal `{a, b}` and an array literal
                // `[a, b]` share this node, so the delimiters come from the
                // recorded spelling — never from a hardcoded `[`. BOTH emit
                // paths below spell their own delimiters, so both dispatch:
                // patching only the flat one leaves a multi-line set with an
                // interior comment silently rewritten into array syntax, and
                // no round-trip gate can see it because the result re-parses.
                let (open, close) = match spelling {
                    ArrayLiteralSpelling::Braces => ("{", "}"),
                    ArrayLiteralSpelling::Brackets => ("[", "]"),
                };
                // Interior-comment gate (Core #4 chokepoint): the range
                // comes free from the AST — `expr.span` runs from the open
                // delimiter to one past the close.
                self.emit_delimited_list(
                    open, close, Gate::Span(expr.span.start, expr.span.end), elems,
                    |e| (e.span.start, e.span.end),
                    |f, e| f.format_expr(e),
                );
            }
            Expr::TupleLiteral(elems) => {
                // The ONE gate evaluated outside `emit_delimited_list`.
                // A single-element tuple is spelled `(x,)` — the trailing
                // comma IS the tuple, so the flat form is not a packing
                // choice but the syntax, and it does not route through the
                // chokepoint. It must still be skipped when a comment sits
                // inside the parens, or the comment escapes; the exploded
                // form carries its own trailing comma and re-parses as a
                // 1-tuple, so the broken path is safe for this shape too.
                let has_interior = self.has_interior_comments(expr.span.start, expr.span.end);
                if elems.len() == 1 && !has_interior {
                    // The two `has_interior_comments` evaluations (here and in
                    // `emit_delimited_list`) are NOT redundant: collapsing them
                    // reopens the `(x,)` interior-comment escape. Enforced by
                    // `formatter_collection_literal_interior_hook_dispatch`'s
                    // EXPECTED_INTERIOR_CHECK == 2 pin, not by an assert (a
                    // debug_assert inside this `!has_interior` branch would be
                    // tautological — output-review catch).
                    self.emitter.write("(");
                    // Family O — the 1-tuple's `,)` tail. It does not route
                    // through the chokepoint (the trailing comma IS the
                    // syntax, not a packing choice), so its element render
                    // needs its own charge.
                    self.with_tail_reserve(2, |s| s.format_expr(&elems[0]));
                    self.emitter.write(",)");
                } else {
                    self.emit_delimited_list(
                        "(", ")", Gate::Span(expr.span.start, expr.span.end), elems,
                        |e| (e.span.start, e.span.end),
                        |f, e| f.format_expr(e),
                    );
                }
            }
            Expr::DictLiteral(pairs) => {
                // `span_of` returns `(key.span.start, value.span.end)` so
                // the range covers the WHOLE pair — a comment between key
                // and value (rare) OR between pairs both count as
                // pair-interior.
                self.emit_delimited_list(
                    "{", "}", Gate::Span(expr.span.start, expr.span.end), pairs,
                    |pair| (pair.0.span.start, pair.1.span.end),
                    |f, pair| {
                        f.format_expr(&pair.0);
                        f.emitter.write(": ");
                        f.format_expr(&pair.1);
                    },
                );
            }
            Expr::StructLiteral { name, generic_args, args } => {
                // fmt-UNREACHABLE arm, kept in sync as class hygiene.
                // `gg fmt` is parse-only, and the parser constructs zero
                // `Expr::StructLiteral` — the sole producer is
                // `rewrite_struct_calls` in semantic analysis, which fmt
                // never reaches. Every `Foo(a, b)` the formatter sees is an
                // `Expr::Call`, which is where the user-visible fix lands.
                // The lint `formatter_collection_literal_interior_hook_dispatch`
                // pins the unreachability so this comment cannot rot.
                //
                // Span note for anyone who does reach it:
                // `rewrite_struct_calls` keeps the outer `expr.span` and
                // sets `name.span` to the callee span, so both anchors used
                // here are valid on a rewritten node.
                self.emitter.write(&name.node);
                let (ga_gate, args_gate) = self.callee_arg_gates(
                    name.span.end,
                    generic_args.as_deref(),
                    expr.span.end,
                    args.first().map(|a| a.span.start),
                    args.last().map(|a| a.span.end),
                );
                if let Some(ga) = generic_args {
                    self.format_generic_args_wrapped(
                        ga,
                        ga_gate.expect("generic args present => generic-args gate derived"),
                    );
                }
                self.emit_delimited_list(
                    "(", ")", args_gate, args,
                    |a| (a.span.start, a.span.end),
                    |f, a| f.format_expr(a),
                );
            }
            Expr::As { expr, type_ } => {
                // FMT-A: `as` is bp 31/32 (left-assoc). Operand at LEFT position.
                // Family O — the ` as <type>` costume: operator text written
                // BETWEEN two renders.
                let as_tail = self.measured_reserve(|s| {
                    s.emitter.write(" as ");
                    s.format_type(type_);
                });
                self.with_tail_reserve(as_tail, |s| {
                    s.format_binop_operand(expr, 31, BinOpPos::Left, false)
                });
                self.emitter.write(" as ");
                self.format_type(type_);
            }
            Expr::Await { expr: inner, prefix_form } => {
                // Q3 PRESERVE: both spellings build this node, so the author's
                // choice is honoured. They are NOT interchangeable for the
                // paren predicates — prefix `await` parses its operand at bp 2
                // (looser than every infix operator), postfix `.await()` is a
                // bp-35 postfix. `effective_outer_bp` and
                // `emits_leading_ownership_sigil` dispatch on this same flag.
                //
                // NOTE the shadowing hazard: `inner` is the AWAITED
                // expression; the node being formatted is `expr`.
                if *prefix_form {
                    self.emitter.write("await ");
                    self.format_prefix_operand(inner, 2);
                } else {
                    // P — the postfix `.await()` form, 8 fixed characters.
                    self.format_postfix_receiver(inner, 8);
                    self.emitter.write(".await()");
                }
            }
            Expr::Spawn { expr, unchecked } => {
                self.emitter.write(if *unchecked { "spawn unchecked " } else { "spawn " });
                // FMT-A: `spawn` parses operand at bp 2 (parser::expr.rs:660).
                self.format_prefix_operand(expr, 2);
            }
            Expr::SpawnBlocking { expr, unchecked } => {
                self.emitter.write(if *unchecked { "spawn blocking unchecked " } else { "spawn blocking " });
                // FMT-A: `spawn blocking` parses operand at bp 2.
                self.format_prefix_operand(expr, 2);
            }
            Expr::Is {
                expr,
                negated,
                pattern,
            } => {
                // FMT-A: `is` / `is not` is bp 9/10 (left-assoc, treated as
                // an infix with a pattern RHS). Only the LEFT expr can be a
                // looser expression that needs paren-wrapping.
                self.format_binop_operand(expr, 9, BinOpPos::Left, false);
                if *negated {
                    self.emitter.write(" is not ");
                } else {
                    self.emitter.write(" is ");
                }
                self.format_pattern(pattern);
            }
            Expr::It => {
                self.emitter.write("it");
            }
            Expr::DotShorthand { variant, args } => {
                self.emitter.write(".");
                self.emitter.write(&variant.node);
                if !args.is_empty() {
                    // No generic-args region on a dot-shorthand, so the
                    // variant name end IS the anchor.
                    let (_, args_gate) = self.callee_arg_gates(
                        variant.span.end,
                        None,
                        expr.span.end,
                        args.first().map(|a| a.span.start),
                        args.last().map(|a| a.span.end),
                    );
                    self.format_call_args_wrapped(args, args_gate);
                }
            }
            Expr::MetaOpInfix { left, op_name, right } => {
                // FMT-A: `meta[op]` infix is bp 27/28 (mirrors Add, left-assoc).
                self.format_binop_operand(left, 27, BinOpPos::Left, false);
                self.emitter.write(&format!(" meta[{op_name}] "));
                self.format_binop_operand(right, 27, BinOpPos::Right, false);
            }
            Expr::MetaOpToken(op) => {
                self.emitter.write("meta ");
                self.emitter.write(binary_op_str(*op));
            }
            Expr::Rethrow { expr, error_binding, transform } => {
                // FMT-A: Rethrow is bp 1/2 (left-assoc). LHS is at bp 1 —
                // basically nothing binds looser, so a leaf/atom is fine
                // as LHS (no wrap needed). But the transform (RHS) is
                // parsed at bp 2 (see parser). If the transform contains
                // a nested `rethrow` (a Rethrow expression at bp 1), it's
                // 1 <= 2-1 = 1, WRAP. (Catch bp 1 also wraps.)
                // B11 — `rethrow` is a MULTI-RENDER header, one disposition per
                // render: the LHS is charged for the whole ` rethrow (…):`
                // composite plus the arm body's inline part, and the bound
                // error TYPE is charged for ` name):` plus the same.
                let rethrow_lhs_tail = self.measured_reserve(|s| {
                    if let Some((error_type, error_name)) = error_binding {
                        s.emitter.write(" rethrow (");
                        s.format_type(error_type);
                        s.emitter.write(" ");
                        s.emitter.write(&error_name.node);
                        s.emitter.write("):");
                    } else {
                        s.emitter.write(" rethrow ");
                        s.format_binop_operand(transform, 1, BinOpPos::Right, false);
                    }
                }) + if error_binding.is_some() {
                    self.arm_body_reserve(transform)
                } else {
                    0
                };
                self.with_tail_reserve(rethrow_lhs_tail, |s| {
                    s.format_binop_operand(expr, 1, BinOpPos::Left, false)
                });
                if let Some((error_type, error_name)) = error_binding {
                    self.emitter.write(" rethrow (");
                    // ` ` + name + `):` — 3 fixed chars around the name.
                    let type_tail = 3
                        + error_name.node.chars().count()
                        + self.arm_body_reserve(transform);
                    self.with_tail_reserve(type_tail, |s| s.format_type(error_type));
                    self.emitter.write(" ");
                    self.emitter.write(&error_name.node);
                    self.emitter.write("):");
                    // The bound-form transform after `):` is parsed by
                    // parse_body_or_expr (parser::expr.rs:507) which reads
                    // an expression at low precedence — safe from further
                    // nesting hazards (like the Catch recovery arm).
                    // Snag #15b/#15c: use `format_arm_body` so multi-stmt
                    // transforms don't get wrapped in a spurious `do:` — and so
                    // a trailing comment on the `rethrow (T e):` header stays on
                    // the header instead of falling into the body.
                    self.format_arm_body(transform, error_name.span.end);
                } else {
                    self.emitter.write(" rethrow ");
                    // Bare-form transform: nested Rethrow/Catch (bp 1) at
                    // this position would silently re-associate. WRAP.
                    self.format_binop_operand(transform, 1, BinOpPos::Right, false);
                }
            }
            Expr::Catch { expr, error_binding, recovery } => {
                // FMT-A: LHS at Catch bp 1 — same story as Rethrow LHS.
                // B11 — the `catch` composite: `" catch ("` + the binding
                // name + `"):"` — 10 fixed chars — plus whatever
                // `format_arm_body` writes inline. The four `format_arm_body`
                // callers do NOT share one value; assuming they did
                // under-reserves this one by the composite's width.
                let catch_tail = 10
                    + error_binding.node.chars().count()
                    + self.arm_body_reserve(recovery);
                self.with_tail_reserve(catch_tail, |s| {
                    s.format_binop_operand(expr, 1, BinOpPos::Left, false)
                });
                self.emitter.write(" catch (");
                self.emitter.write(&error_binding.node);
                self.emitter.write("):");
                // Recovery parsed via parse_body_or_expr (parser::expr.rs:507)
                // at bp 0 — absorbs everything on its line, so no wrap
                // hazard for the recovery arm itself.
                // Snag #15b/#15c: use `format_arm_body` so multi-stmt
                // recovery bodies don't get wrapped in a spurious `do:` — and
                // so a trailing comment on the `catch (e):` header stays on the
                // header instead of falling into the body.
                self.format_arm_body(recovery, error_binding.span.end);
            }
        }
    }


    fn format_ownership_prefix(&mut self, ownership: Ownership) {
        match ownership {
            Ownership::Borrow => {}
            Ownership::MutableBorrow => self.emitter.write("&"),
            // D27 Round A: generic ownership-prefix helper — `^` (was `!`).
            // Chokepoint for named-param emission (`format_param` non-self path).
            Ownership::Move => self.emitter.write("^"),
        }
    }

    fn format_call_arg(&mut self, arg: &CallArg) {
        // `parse_call_arg` runs `parse_ownership_modifier` BEFORE the `name =`
        // lookahead, so the sigil position is ahead of the NAME, not ahead of
        // the value. Both branches below follow from that one fact.
        if let Some(ref name) = arg.name {
            // NAMED argument. `CallArg.ownership` was written by a sigil in
            // the NAME's slot (`f(&b = x)` — the D35 spelling), so it must be
            // re-emitted there. Emitting it after the `=` instead
            // (`f(b = &x)`) puts it where the pre-pass cannot see it: the
            // sigil is re-parsed as part of the VALUE expression, and
            // `CallArg.ownership` comes back `Borrow`.
            //
            // That is a silent re-homing of a fact-carrying field — the exact
            // class this track exists to retire — and it BREAKS WORKING CODE:
            // `takes_mut(1, &b = x)` runs and prints `2`, while the
            // pre-fix emission `takes_mut(1, b = &x)` is REJECTED
            // (E_OwnershipMismatch + E_AmpInOperandPosition).
            //
            // No paren guard is needed on the value here, and adding one is
            // pure churn (`b = &x` → `b = (&x)`, measured live on
            // `known_gaps/sound_named_arg_sigil_dropped.gg`): with the sigil
            // emitted in its own slot, the value is parsed by `parse_expr`
            // with no pre-pass ahead of it.
            //
            // ⚠ This does NOT make the after-`=` spelling sound — a `&` there
            // is still silently dropped by the OWNERSHIP CHECK, the separate
            // already-filed defect that same fixture pins. This is only about
            // the formatter re-emitting the ownership it was GIVEN.
            self.format_ownership_prefix(arg.ownership);
            self.emitter.write(&name.node);
            self.emitter.write(" = ");
            self.format_expr(&arg.value);
        } else {
            // POSITIONAL argument — sibling of the for-iterable site. The
            // sigil slot is immediately ahead of the value expression, so a
            // value whose own emission LEADS with a sigil must be
            // parenthesised or the reparse steals it into `ownership`.
            self.format_ownership_prefix(arg.ownership);
            self.format_ownership_modifier_operand(&arg.value);
        }
    }

    fn format_closure_param(&mut self, param: &ClosureParam) {
        // Tuple destructuring: print `(T1 x, T2 y, ...)` from the source-level metadata
        // rather than the synthesised `(T1, T2) __dp_N` form.
        if let Some(ref bindings) = param.destructure {
            self.emitter.write("(");
            // Another hand-rolled comma loop: each binding's TYPE is charged
            // for its own ` name`, the rest of the list, and the `)`.
            for (i, b) in bindings.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(", ");
                }
                let rest = &bindings[i + 1..];
                let tail = self.measured_reserve(|s| {
                    s.emitter.write(" ");
                    s.format_ownership_prefix(b.ownership);
                    s.emitter.write(&b.name.node);
                    for r in rest {
                        s.emitter.write(", ");
                        s.format_type(&r.type_);
                        s.emitter.write(" ");
                        s.format_ownership_prefix(r.ownership);
                        s.emitter.write(&r.name.node);
                    }
                    s.emitter.write(")");
                });
                self.with_tail_reserve(tail, |s| s.format_type(&b.type_));
                self.emitter.write(" ");
                self.format_ownership_prefix(b.ownership);
                self.emitter.write(&b.name.node);
            }
            self.emitter.write(")");
            return;
        }
        // type-first: `[type] [&|!]name`
        if let Some(ref ty) = param.type_ {
            // The type's tail is ` ` + the sigil + the name, ADDED to whatever
            // the enclosing closure-param list already reserved for this item.
            let tail = self.measured_reserve(|s| {
                s.emitter.write(" ");
                s.format_ownership_prefix(param.ownership);
                s.emitter.write(&param.name.node);
            });
            self.with_tail_reserve(tail, |s| s.format_type(ty));
            self.emitter.write(" ");
        }
        self.format_ownership_prefix(param.ownership);
        self.emitter.write(&param.name.node);
    }

    // ── String formatting ───────────────────────────────────

    fn format_string_lit(&mut self, s: &StringLiteral, span: Span) {
        // Verbatim first: the author's own lexeme, when it re-lexes to exactly
        // this literal. That single check preserves quote style, prefix letter,
        // escape spelling, the f-string brace form, and — the case that made a
        // 90-line shader unreadable — the PHYSICAL LINE LAYOUT of a `"""`
        // block, which the canonical path below flattens into one `\n`-escaped
        // line. `write_preformatted` keeps the emitter's column honest across
        // those embedded newlines.
        if let Some(text) = self.string_literal_text(s, span) {
            self.emitter.write_preformatted(&text);
            return;
        }
        match s.kind {
            StringKind::Raw => self.emitter.write("r\""),
            StringKind::Byte => self.emitter.write("b\""),
            StringKind::CStr => self.emitter.write("c\""),
            StringKind::Format => self.emitter.write("f\""),
            StringKind::MultiLine => self.emitter.write("\"\"\""),
            StringKind::Normal => self.emitter.write("\""),
        }
        for seg in &s.segments {
            match seg {
                StringSegment::Literal(text) => {
                    self.format_string_escape(text, s.kind);
                }
                StringSegment::Interpolation(expr_text, spec) => {
                    self.emitter.write("{");
                    self.emitter.write(expr_text);
                    if let Some(fmt) = spec {
                        self.emitter.write(":");
                        self.emitter.write(fmt);
                    }
                    self.emitter.write("}");
                }
            }
        }
        match s.kind {
            StringKind::MultiLine => self.emitter.write("\"\"\""),
            _ => self.emitter.write("\""),
        }
    }

    /// The canonical (non-verbatim) escaping path. Reached only when the
    /// author's lexeme could not be recovered — a synthesised literal, or a
    /// span that no longer denotes this node. See `canonical_string_escape`
    /// for the escape policy itself; keeping the policy in a free function
    /// makes it unit-testable without a `Formatter`, which matters because
    /// no `.gg` source can force this path.
    fn format_string_escape(&mut self, text: &str, kind: StringKind) {
        let escaped = canonical_string_escape(text, kind);
        self.emitter.write(&escaped);
    }

}

// ══════════════════════════════════════════════════════════════
// Helper functions
// ══════════════════════════════════════════════════════════════

/// Re-lex an isolated source slice and return the SINGLE token it denotes.
///
/// This is the oracle behind every verbatim-preservation arm in the formatter.
/// It asks the real lexer rather than mirroring its rules, which is what makes
/// the round-trip check meaningful: "this lexeme denotes value V" is decided by
/// the same code that produced V from that lexeme in the first place. The
/// earlier hand-written `parse_int_lexeme` had to promise, in prose, that it
/// mirrored `Lexer::parse_int_literal` exactly — a promise nothing enforced,
/// and one that was already false for byte literals (`b'A'`, which the lexer
/// turns into `IntLiteral(65)` and the mirror rejected).
///
/// Returns `None` unless the slice lexes cleanly into exactly one value-bearing
/// token that covers the WHOLE slice. Trailing structural tokens the lexer
/// synthesises at end-of-input (`Newline`, `Dedent`, `Eof`) are expected and
/// ignored; anything else means the slice was not one literal.
fn relex_single_token(lexeme: &str) -> Option<Token> {
    if lexeme.is_empty() {
        return None;
    }
    let mut lexer = crate::lexer::Lexer::new(lexeme);
    let tokens: Vec<Spanned<Token>> = lexer.by_ref().collect();
    if !lexer.errors.is_empty() {
        return None;
    }
    let mut it = tokens.into_iter();
    let first = it.next()?;
    // The token must span the entire slice. The multi-token check below cannot
    // see slack that produces no token of its own (trailing spaces, a leading
    // indent), and slack would be emitted verbatim along with the literal.
    if first.span.start != 0 || first.span.end != lexeme.len() {
        return None;
    }

    for rest in it {
        match rest.node {
            Token::Newline | Token::Dedent | Token::Indent | Token::Eof => {}
            _ => return None,
        }
    }
    Some(first.node)
}

/// Canonically escape a string body for re-emission — the FALLBACK spelling,
/// used only when the author's own lexeme could not be recovered verbatim.
///
/// Every control character is escaped. C0 and DEL take the `\xHH` byte escape
/// (the lexer's `\x` accepts `<= 0x7F` in string context); C1 (`0x80-0x9F`)
/// takes `\u{XX}`, because `\x` above `0x7F` is REJECTED by the lexer and a
/// raw C1 byte is an invisible control character planted in the user's source.
/// Printable non-ASCII (accented letters, emoji, …) stays raw UTF-8 — it
/// round-trips fine and escaping it would be noise.
fn canonical_string_escape(text: &str, kind: StringKind) -> String {
    let mut out = String::with_capacity(text.len());
    if kind == StringKind::Raw {
        out.push_str(text);
        return out;
    }
    for ch in text.chars() {
        match ch {
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\0' => out.push_str("\\0"),
            '{' if kind == StringKind::Format => out.push_str("{{"),
            '}' if kind == StringKind::Format => out.push_str("}}"),
            c if c.is_control() => {
                if (c as u32) <= 0x7F {
                    out.push_str(&format!("\\x{:02x}", c as u32));
                } else {
                    out.push_str(&format!("\\u{{{:X}}}", c as u32));
                }
            }
            c => out.push(c),
        }
    }
    out
}

fn binary_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Mod => "mod",
        BinaryOp::Pow => "**",
        BinaryOp::AddWrap => "+%",
        BinaryOp::SubWrap => "-%",
        BinaryOp::MulWrap => "*%",
        BinaryOp::Eq => "==",
        BinaryOp::Neq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Gt => ">",
        BinaryOp::LtEq => "<=",
        BinaryOp::GtEq => ">=",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::In => "in",
        // D26 fallible arithmetic.
        BinaryOp::AddFallible => "+!",
        BinaryOp::SubFallible => "-!",
        BinaryOp::MulFallible => "*!",
        BinaryOp::DivFallible => "/!",
        BinaryOp::RemFallible => "%!",
        BinaryOp::ShlFallible => "<<!",
        BinaryOp::ShrFallible => ">>!",
    }
}

fn compound_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+=",
        BinaryOp::Sub => "-=",
        BinaryOp::Mul => "*=",
        BinaryOp::Pow => "**=",
        BinaryOp::Div => "/=",
        BinaryOp::Rem => "%=",
        BinaryOp::AddWrap => "+%=",
        BinaryOp::SubWrap => "-%=",
        BinaryOp::MulWrap => "*%=",
        BinaryOp::BitAnd => "&=",
        BinaryOp::BitOr => "|=",
        BinaryOp::BitXor => "^=",
        BinaryOp::Shl => "<<=",
        BinaryOp::Shr => ">>=",
        _ => unreachable!("no compound assignment for {:?}", op),
    }
}

fn unary_op_str(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "not ",
        UnaryOp::BitNot => "~",
    }
}

// ══════════════════════════════════════════════════════════════
// Precedence-aware paren-wrapping helpers (FMT-A, Round XXXVI)
// ══════════════════════════════════════════════════════════════
//
// The parser's Pratt binding-power table (src/parser/expr.rs:775-1029)
// is mirrored here as `binary_op_left_bp`, `binary_op_is_right_assoc`,
// and `effective_outer_bp`. The formatter needs to know when an operand
// expression's own outer precedence is LOWER than the containing
// operator's, so it can wrap the operand in `(...)` — otherwise
// `gg fmt` on `(a+b)/2` re-emits `a+b/2`, silently flipping arithmetic.
//
// Two per-context helpers:
//   - `needs_parens_as_binop_operand` for infix operands (left/right).
//   - `needs_parens_as_prefix_operand` for prefix operators (`-x`,
//     `not x`, `!x` move, `&x` mutable-borrow, `*x` deref, `~x` bitnot,
//     `spawn x` / `spawn blocking x` / `await x`, Range `..end`).
//   - `needs_parens_as_postfix_receiver` for postfix (`.field`,
//     `.method()`, `[i]`, `(args)`, `!` propagate, `?.field`).
//
// Rules (mirror standard Pratt-parser paren-preservation logic):
//   LEFT position of infix (left_bp `L`):
//     - LEFT-assoc outer: wrap if inner_bp < L (strict)
//     - RIGHT-assoc outer: wrap if inner_bp <= L
//   RIGHT position of infix (left_bp `L`):
//     - LEFT-assoc outer: wrap if inner_bp <= L
//     - RIGHT-assoc outer: wrap if inner_bp < L
//   PREFIX operand: wrap if inner_bp < prefix_bp
//   POSTFIX receiver: wrap if inner_bp < 35 (postfix bp)
//
// A leaf/atom/postfix expression has no "outer precedence" to compare —
// `effective_outer_bp` returns `None` and no wrap is needed.

/// Mirror of `Parser::infix_bp` left binding-power for a BinaryOp.
/// Keep in sync with `src/parser/expr.rs:775-1029` (`Parser::infix_bp`).
fn binary_op_left_bp(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::Or => 5,
        BinaryOp::And => 7,
        BinaryOp::Eq | BinaryOp::Neq => 11,
        BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => 13,
        BinaryOp::In => 15,
        BinaryOp::BitOr => 17,
        BinaryOp::BitXor => 19,
        BinaryOp::BitAnd => 21,
        BinaryOp::Shl | BinaryOp::Shr | BinaryOp::ShlFallible | BinaryOp::ShrFallible => 25,
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::AddWrap
        | BinaryOp::SubWrap
        | BinaryOp::AddFallible
        | BinaryOp::SubFallible => 27,
        BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Rem
        | BinaryOp::Mod
        | BinaryOp::MulWrap
        | BinaryOp::MulFallible
        | BinaryOp::DivFallible
        | BinaryOp::RemFallible => 29,
        // `**` is RIGHT-associative — parser encodes as (left=34, right=33)
        // so `a ** b ** c` parses `a ** (b ** c)`. The "effective left bp"
        // for embedding checks is 34 — that's what a `**` at this position
        // takes as its left-side binding priority.
        BinaryOp::Pow => 34,
    }
}

/// Only Pow (`**`) is right-associative in Gorget.
fn binary_op_is_right_assoc(op: BinaryOp) -> bool {
    matches!(op, BinaryOp::Pow)
}

/// The effective "outer precedence" (left binding-power) of an
/// expression when embedded in a larger expression context.
/// Returns `None` for leaves/postfixes/atoms that never need paren-wrapping.
fn effective_outer_bp(expr: &Expr) -> Option<u8> {
    match expr {
        // Loose infix keywords — bp 1/2 (Rethrow, Catch).
        Expr::Rethrow { .. } | Expr::Catch { .. } => Some(1),
        // Default operator `??` — bp 3/4.
        Expr::DefaultOp { .. } => Some(3),
        // `is` / `is not` — bp 9/10.
        Expr::Is { .. } => Some(9),
        // Range `..` / `..=` — bp 23 (parser::expr.rs:1225).
        Expr::Range { .. } => Some(23),
        // `as` cast — bp 31/32.
        Expr::As { .. } => Some(31),
        // BinaryOp — look up the operator's left bp.
        Expr::BinaryOp { op, .. } => Some(binary_op_left_bp(*op)),
        // `meta[op]` infix — bp 27/28 (mirrors Add).
        Expr::MetaOpInfix { .. } => Some(27),
        // Prefix `not` — parses operand at bp 20 (parser::expr.rs:530).
        Expr::UnaryOp { op: UnaryOp::Not, .. } => Some(20),
        // Other prefix unary and sigil ops — bp 33 (parser::expr.rs:554,576,596,621,635).
        Expr::UnaryOp { .. }
        | Expr::Move { .. }
        | Expr::MutableBorrow { .. }
        | Expr::Deref { .. } => Some(33),
        // spawn / spawn blocking — prefix bp 2 (parser::expr.rs:660).
        Expr::Spawn { .. } | Expr::SpawnBlocking { .. } => Some(2),
        // Await's effective bp DEPENDS ON THE RENDERED FORM, because the two
        // spellings the language accepts are not interchangeable at this
        // level: the parser reads prefix `await e` with the operand at bp 2
        // (looser than every infix operator) and postfix `e.await()` as a
        // bp-35 postfix. Reporting the postfix answer for a prefix-rendered
        // node re-emits `(await f()) + 1` as `await f() + 1`, which re-parses
        // as `Await(f() + 1)` — a silent AST change that compiles and runs.
        // Same shape as Spawn/SpawnBlocking above, which are prefix-only.
        Expr::Await { prefix_form: true, .. } => Some(2),
        Expr::Await { prefix_form: false, .. } => None,
        // "Statement-like" value-producing expressions — closure, if,
        // match, do, block. Their surface syntax spans a body/branches
        // that extend as far right as the parser will let them, so
        // embedding them without paren-wrap in a postfix or infix
        // context lets the outer op steal from the body. E.g. the IIFE
        // `((int x): x * x)(5)` MUST wrap the closure — without the
        // parens, `(int x): x * x(5)` reparses with `x * x(5)` as the
        // closure body and drops the outer call.
        //
        // Report the LOOSEST bp (0) so ANY infix/prefix/postfix
        // embedding triggers the wrap. Idempotent when the outer
        // context is a bare statement/assign right-hand side (no
        // operator to compare against — no wrap invoked at all).
        Expr::Closure { .. }
        | Expr::If { .. }
        | Expr::Match { .. }
        | Expr::Do { .. }
        | Expr::Block(_) => Some(0),
        // Everything else (literals, identifiers, calls, method calls,
        // field access, index, propagate, container literals, struct
        // literals, comprehensions, patterns, path, self, etc.) —
        // no wrap needed as a nested operand.
        _ => None,
    }
}

/// Position of an operand relative to its containing binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinOpPos {
    Left,
    Right,
}

/// True when `operand` needs `(...)` wrapping to preserve its own
/// AST shape when spliced as the given `position` operand of an
/// outer binary operator with left-bp `outer_left_bp`. Mirror of
/// the parser's binding-power precedence resolution.
fn needs_parens_as_binop_operand(
    operand: &Expr,
    outer_left_bp: u8,
    position: BinOpPos,
    outer_right_assoc: bool,
) -> bool {
    let Some(inner) = effective_outer_bp(operand) else {
        return false;
    };
    match position {
        BinOpPos::Left => {
            if outer_right_assoc {
                inner <= outer_left_bp
            } else {
                inner < outer_left_bp
            }
        }
        BinOpPos::Right => {
            if outer_right_assoc {
                inner < outer_left_bp
            } else {
                inner <= outer_left_bp
            }
        }
    }
}

/// True when `operand` needs `(...)` wrapping when spliced as the
/// operand of a prefix operator that parses its operand at bp `prefix_bp`.
/// (Prefixes: `not` bp 20; `-`/`~`/`!`/`&`/`*`/`^` bp 33; `spawn`/`await` bp 2;
/// Range end `..end` bp 24.)
fn needs_parens_as_prefix_operand(operand: &Expr, prefix_bp: u8) -> bool {
    let Some(inner) = effective_outer_bp(operand) else {
        return false;
    };
    // If inner infix's left_bp < prefix_bp, the parser would treat the
    // infix as CONTINUATION of the outer expression (`not X + y` parses
    // as `not (X + y)`, `Not(Add(X,y))`), losing the intended
    // `Not(X) + y` shape — must wrap.
    inner < prefix_bp
}

/// True when `operand` needs `(...)` wrapping when spliced as the
/// receiver of a postfix operator (field access, tuple field access,
/// method call, index, call, `?.field` optional chain, `!` propagate,
/// `.await()` method-call form).
///
/// Postfix ops all bind at bp 35 (parser::expr.rs:1215-1223). Any infix
/// or prefix expression as receiver would be dismantled by the parser —
/// `a + b.foo()` = `Add(a, MethodCall(b, foo))`, `-a.foo()` = `Neg(MethodCall(a, foo))`.
fn needs_parens_as_postfix_receiver(operand: &Expr) -> bool {
    let Some(inner) = effective_outer_bp(operand) else {
        return false;
    };
    inner < 35
}

/// R41 T-FMT-A — the PARSE-ORDER predicate behind
/// `Formatter::format_ownership_modifier_operand`.
///
/// True iff `format_expr(expr)` would emit a first character that
/// `Parser::parse_ownership_modifier` consumes as an ownership sigil —
/// `&` (MutableBorrow), `!` or `^` (Move). At the two positions where that
/// pre-pass runs before the expression parser (`parse_for_stmt`'s iterable
/// and `parse_call_arg`'s value), such an emission silently re-homes the
/// sigil from the expression into the enclosing node's `ownership` field.
///
/// It answers a question about the EMITTED TEXT, so it walks the same
/// left spine `format_expr` does and consults the SAME wrap predicates the
/// emission arms use: whenever an arm wraps its leftmost child in parens,
/// the emitted text begins with `(` and the sigil is already shielded.
///
/// **The match is deliberately exhaustive — no `_` arm.** A new `Expr`
/// variant is then a COMPILE ERROR here rather than a silent `false`, which
/// is how this class (a leftmost-emitting variant added without a paren
/// rule) would otherwise re-open. Core #4: the class, not the instance;
/// Core #10: no silent fall-through.
fn emits_leading_ownership_sigil(expr: &Expr) -> bool {
    /// Recurse into a child that the emitter renders FIRST, unless the
    /// emitter parenthesises it (in which case `(` leads and we are safe).
    fn through(child: &Spanned<Expr>, wrapped: bool) -> bool {
        !wrapped && emits_leading_ownership_sigil(&child.node)
    }

    match expr {
        // ── The sigil producers ────────────────────────────────────
        // `Expr::Move` emits `^`, `Expr::MutableBorrow` emits `&`.
        Expr::Move { .. } | Expr::MutableBorrow { .. } => true,
        // A MOVE CLOSURE emits its `^` prefix before the param list
        // (`^(int x): x`). Non-move closures lead with `(` or `async `.
        Expr::Closure { is_move, .. } => *is_move,
        // `meta +` / `meta &` / `meta !=` — emitted as the literal `meta `
        // prefix followed by the operator token, so the LEADING character is
        // always `m`. The operator's own spelling is irrelevant here: this
        // predicate is about the first character of the emission, and
        // `binary_op_str` is never reached by the parser's sigil pre-pass.
        //
        // (The first cut of this arm tested `binary_op_str(op)` for a sigil,
        // which made `meta !=` — a real shape, `lib/xtd/tensor.gg` — come out
        // as `(meta !=)`. Testing the operator instead of the leading char is
        // exactly the mistake the doc comment above warns about.)
        Expr::MetaOpToken(_) => false,

        // ── Leftmost child is an INFIX LEFT operand ────────────────
        // Each mirrors its emission arm's `format_binop_operand(.., Left, ..)`.
        Expr::BinaryOp { left, op, .. } => through(
            left,
            needs_parens_as_binop_operand(
                &left.node,
                binary_op_left_bp(*op),
                BinOpPos::Left,
                binary_op_is_right_assoc(*op),
            ),
        ),
        Expr::Range { start, .. } => match start {
            // `..end` leads with the `..` token.
            None => false,
            Some(s) => through(
                s,
                needs_parens_as_binop_operand(&s.node, 23, BinOpPos::Left, false),
            ),
        },
        Expr::As { expr, .. } => through(
            expr,
            needs_parens_as_binop_operand(&expr.node, 31, BinOpPos::Left, false),
        ),
        Expr::Is { expr, .. } => through(
            expr,
            needs_parens_as_binop_operand(&expr.node, 9, BinOpPos::Left, false),
        ),
        Expr::DefaultOp { lhs, .. } => through(
            lhs,
            needs_parens_as_binop_operand(&lhs.node, 3, BinOpPos::Left, false),
        ),
        Expr::MetaOpInfix { left, .. } => through(
            left,
            needs_parens_as_binop_operand(&left.node, 27, BinOpPos::Left, false),
        ),
        Expr::Rethrow { expr, .. } | Expr::Catch { expr, .. } => through(
            expr,
            needs_parens_as_binop_operand(&expr.node, 1, BinOpPos::Left, false),
        ),

        // ── Leftmost child is a POSTFIX RECEIVER (bp 35) ───────────
        // Both sigil producers report bp 33 < 35, so these always wrap —
        // but route through the shared predicate rather than assert it.
        Expr::Call { callee: recv, .. }
        | Expr::MethodCall { receiver: recv, .. }
        | Expr::FieldAccess { object: recv, .. }
        | Expr::TupleFieldAccess { object: recv, .. }
        | Expr::Index { object: recv, .. }
        | Expr::OptionalChain { object: recv, .. }
        | Expr::Propagate { expr: recv }
        | Expr::Await { expr: recv, prefix_form: false } => {
            through(recv, needs_parens_as_postfix_receiver(&recv.node))
        }

        // A PREFIX-rendered await leads with the `await` keyword, not with its
        // operand — so unlike the postfix rendering above it never begins with
        // an ownership sigil, and the recursion into the operand would be
        // asking about a token that is no longer leftmost. Same reasoning as
        // the `spawn ` / `spawn blocking ` forms in the arm below.
        Expr::Await { prefix_form: true, .. } => false,

        // ── Transparent wrapper ────────────────────────────────────
        // `ImplicitClosure` emits its body directly (the `it` inside is the
        // implicit-parameter marker), so the body's first char is ours.
        Expr::ImplicitClosure { body } => emits_leading_ownership_sigil(&body.node),

        // ── Leads with its OWN token ───────────────────────────────
        // Literals and atoms; prefix operators whose spelling is not a
        // stripped sigil (`-`, `~`, `not `, `*`, `spawn `, `spawn blocking `);
        // bracketed/keyword-introduced forms (`[`, `(`, `{`, `if`, `match`,
        // `Name(`, `.variant`, `it`, `self`).
        //
        // `Block`/`Do` lead with `do:` unless
        // `try_inline_single_terminal_stmt` inlines a lone terminal stmt —
        // and those spellings lead with `throw `/`return `/the bare
        // expression. The bare-expression case cannot be reached at a
        // position this predicate guards: a `Block`/`Do` is never the direct
        // value of a for-iterable or a call argument (the parser builds them
        // only for arm/branch bodies and `throw`/`return` prefixes), so the
        // conservative `false` here is not load-bearing. If that ever
        // changes, the fixture matrix's differential pairs are what fails.
        Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::BoolLiteral(_)
        | Expr::StringLiteral(..)
        | Expr::NoneLiteral
        | Expr::Identifier(_)
        | Expr::SelfExpr
        | Expr::Path { .. }
        | Expr::UnaryOp { .. }
        | Expr::Deref { .. }
        | Expr::If { .. }
        | Expr::Match { .. }
        | Expr::Block(_)
        | Expr::Do { .. }
        | Expr::ListComprehension { .. }
        | Expr::DictComprehension { .. }
        | Expr::SetComprehension { .. }
        | Expr::ArrayLiteral(_, _)
        | Expr::TupleLiteral(_)
        | Expr::DictLiteral(_)
        | Expr::StructLiteral { .. }
        | Expr::Spawn { .. }
        | Expr::SpawnBlocking { .. }
        | Expr::It
        | Expr::DotShorthand { .. } => false,
    }
}

fn primitive_type_str(p: PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::Int => "int",
        PrimitiveType::Int8 => "int8",
        PrimitiveType::Int16 => "int16",
        PrimitiveType::Int32 => "int32",
        PrimitiveType::Int64 => "int64",
        PrimitiveType::Uint => "uint",
        PrimitiveType::Uint8 => "uint8",
        PrimitiveType::Uint16 => "uint16",
        PrimitiveType::Uint32 => "uint32",
        PrimitiveType::Uint64 => "uint64",
        PrimitiveType::Float => "float",
        PrimitiveType::Float32 => "float32",
        PrimitiveType::Float64 => "float64",
        PrimitiveType::Bool => "bool",
        PrimitiveType::CStr => "cstr",
        PrimitiveType::StringType => "String",
        PrimitiveType::Void => "void",
    }
}

// ══════════════════════════════════════════════════════════════
// Public API
// ══════════════════════════════════════════════════════════════

/// Fallible formatter entry point.
///
/// Parses `source`, and if the parse recovered but recorded errors, returns
/// those errors instead of producing a formatted output. The formatter builds
/// its output from the AST, so any statement that failed to parse would be
/// silently dropped from the formatted result if we ignored `parser.errors`
/// — Core #8 (silent data loss) if we did not surface them.
///
/// Callers that are expected to feed valid Gorget (unit tests, integration
/// fixtures) can use [`format_source_infallible`] which panics on any parse
/// error with a clear message.
pub fn format_source_result(source: &str) -> Result<String, Vec<crate::errors::ParseError>> {
    let mut parser = crate::parser::Parser::new(source);
    let module = parser.parse_module();
    if !parser.errors.is_empty() {
        return Err(parser.errors);
    }
    let comments = parser.comments;
    Ok(Formatter::new(comments, Rc::from(source)).format(&module))
}

/// Infallible convenience: panics if the source has parse errors.
///
/// For tests and internal call sites that MUST be fed valid Gorget. Real
/// user-facing `gg fmt` goes through [`format_source_result`] so it can
/// surface diagnostics without silently dropping unparseable lines.
pub fn format_source_infallible(source: &str) -> String {
    match format_source_result(source) {
        Ok(s) => s,
        Err(errs) => {
            panic!(
                "format_source: input has {} parse error(s); first: {:?}",
                errs.len(),
                errs.first()
            );
        }
    }
}


// ── Import sorting helpers ──────────────────────────────────

/// Extract the dotted path from an import item for sorting purposes.
fn import_sort_key(item: &Spanned<Item>) -> String {
    match &item.node {
        Item::Import(ImportStmt::Simple { path, .. })
        | Item::Import(ImportStmt::Grouped { path, .. })
        | Item::Import(ImportStmt::From { path, .. }) => {
            path.iter().map(|s| s.node.as_str()).collect::<Vec<_>>().join(".")
        }
        _ => String::new(),
    }
}

/// Returns true if the import path starts with `std` or `gg` (standard library).
fn is_std_import(path: &str) -> bool {
    path.starts_with("std.") || path.starts_with("xtd.") || path == "std" || path == "xtd"
}

// ── Expression chain helpers ────────────────────────────────

/// Count the length of a method call chain (consecutive `.method()` calls).
/// Returns 1 for a single method call, 2+ for chains.
fn method_chain_length(expr: &Spanned<Expr>) -> usize {
    match &expr.node {
        Expr::MethodCall { receiver, .. } => 1 + method_chain_length(receiver),
        _ => 0,
    }
}

/// Collect method chain segments from outermost to innermost.
/// Returns (root_expr, vec of (method_name, generic_args, args)) from left to right.
fn collect_method_chain<'a>(
    expr: &'a Spanned<Expr>,
) -> (
    &'a Spanned<Expr>,
    Vec<(
        &'a Spanned<String>,
        &'a Option<Vec<Spanned<Type>>>,
        &'a Vec<Spanned<CallArg>>,
    )>,
) {
    let mut segments = Vec::new();
    let mut current = expr;
    loop {
        match &current.node {
            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                segments.push((method, generic_args, args));
                current = receiver;
            }
            _ => break,
        }
    }
    segments.reverse();
    (current, segments)
}

/// Flatten a left-associative binary expression chain of the same operator.
/// `a + b + c` is parsed as `(a + b) + c`. This collects `[a, b]` into `operands`
/// (the caller adds `c`).
fn collect_binary_operands<'a>(
    expr: &'a Spanned<Expr>,
    target_op: BinaryOp,
    operands: &mut Vec<&'a Spanned<Expr>>,
) {
    match &expr.node {
        Expr::BinaryOp { left, op, right } if *op == target_op => {
            collect_binary_operands(left, target_op, operands);
            operands.push(right);
        }
        _ => {
            operands.push(expr);
        }
    }
}

/// Round XXXVI FMT-A / Round XXXVIII Track C chokepoint (Core #1 + #4):
/// wrap a multi-line expression emission in parentheses that appear
/// ONLY in broken mode. In flat mode nothing is added; in broken mode
/// the emitted doc becomes `(<inner>)`.
///
/// **Why:** when an expression emission breaks across lines, the
/// continuation lines start with the operator (`+ a`, `?? b`, ...).
/// Bare leading-operator continuations are NOT valid Gorget — the
/// parser rejects them, and a second `gg fmt` pass then drops the
/// orphaned lines, silently LOSING code on round-trip (Core #8/#10).
/// The lexer only suppresses NEWLINE/INDENT/DEDENT inside brackets
/// (`bracket_depth > 0`, `src/lexer/mod.rs:22`) or after a leading `.`
/// carve-out (`src/lexer/mod.rs:161`), so the multi-line form of an
/// operator chain is only parser-valid when wrapped in parentheses.
/// Emit `(` / `)` via `if_break` so the parens appear ONLY in broken
/// mode (flat mode stays clean, no noise). The wrapped form re-parses
/// to the same bare expression node with a paren wrapper that
/// re-formats to the same parenthesized shape (idempotent). Parens are
/// semantically transparent, so adding them never changes meaning.
///
/// **Callers must route through here** for any `doc::group` containing
/// `doc::line`/`doc::softline` outside of a bracketed
/// (`[`/`{`/`(...)`)  context and outside a leading-`.` chain. The
/// `tests/lints.rs` guard `fmt_multiline_group_paren_wrap_class`
/// counts callers and asserts the class-invariant. Guards:
/// `fmt_binary_chain_round_trips` (parse+idempotence) and
/// `fmt_round_trip_semantic` (build+run) in `tests/integration.rs`.
fn wrap_multiline_expr_in_parens(inner: doc::Doc) -> doc::Doc {
    doc::group(doc::concat(vec![
        doc::if_break(doc::text(""), doc::text("(")),
        inner,
        doc::if_break(doc::text(""), doc::text(")")),
    ]))
}

/// Build a Doc for a comprehension expression with line-width-aware wrapping.
///
/// Flat: `[expr for var in iterable if cond]`
/// Broken:
/// ```text
/// [
///     expr
///     for var in iterable
///     if cond
/// ]
/// ```
fn build_comprehension_doc(
    open: &str,
    expr_s: &str,
    var_s: &str,
    own_prefix: &str,
    iter_s: &str,
    cond_s: Option<&str>,
    close: &str,
) -> doc::Doc {
    let mut inner = vec![
        doc::text(expr_s),
        doc::line(),
        doc::text(format!("for {var_s} in {own_prefix}{iter_s}")),
    ];
    if let Some(cond) = cond_s {
        inner.push(doc::line());
        inner.push(doc::text(format!("if {cond}")));
    }

    doc::group(doc::concat(vec![
        doc::text(open),
        doc::indent(doc::concat(vec![doc::softline(), doc::concat(inner)])),
        doc::softline(),
        doc::text(close),
    ]))
}

// ══════════════════════════════════════════════════════════════
// Tests
// ══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(source: &str) -> String {
        format_source_infallible(source)
    }

    // ── Emitter cursor (R41 T-FMT-D) ──────────────────────────
    //
    // The column the emitter reports is what seeds every width decision the
    // Doc renderer makes. These pin the two ways it used to lie. Neither has a
    // `.gg` spelling that changes the OUTPUT today, so they live here rather
    // than in the `fmt_fill_pack` fixture matrix.

    #[test]
    fn test_emitter_current_col_at_line_start_is_the_pending_indent() {
        // After `newline()` the indentation has NOT been written yet, so the
        // raw `col` field is 0 while the next character will land at
        // `indent * 4`. Reading the raw field here is what let a line-initial
        // list be measured `indent * 4` columns too narrow.
        let mut e = Emitter::new();
        e.indent();
        e.indent();
        e.newline();
        assert_eq!(e.col, 0);
        assert_eq!(e.current_col(), 8);
        // Once something is written, the two agree again.
        e.write("abc");
        assert_eq!(e.current_col(), 11);
    }

    #[test]
    fn test_emitter_blank_line_resets_the_column() {
        let mut e = Emitter::new();
        e.write("some text");
        e.blank_line();
        assert!(e.at_line_start);
        assert_eq!(e.col, 0, "blank_line must reset the cursor like newline");
    }

    #[test]
    fn test_emitter_columns_count_characters_not_bytes() {
        // "naïve café" is 10 characters and 12 UTF-8 bytes.
        let mut e = Emitter::new();
        e.write("naïve café..");
        assert_eq!(e.col, 12, "12 characters (14 bytes)");

        // Same for a pre-formatted splice with no newline...
        let mut e = Emitter::new();
        e.write_preformatted("naïve café..");
        assert_eq!(e.col, 12);

        // ...and for one whose LAST LINE carries the multi-byte text, where
        // the column restarts from that line's own baked-in indentation.
        let mut e = Emitter::new();
        e.write_preformatted("[\n    \"naïve café\"]");
        assert_eq!(e.col, 17, "4 indent + 12 literal + 1 bracket, in chars");
    }

    // ── R41 T-FMT-B: the two verbatim cells no .gg source can reach ──
    //
    // Both of these guard the FALLBACK half of the verbatim chokepoint, and
    // neither is reachable from a fixture: with span recovery working, every
    // literal in a real source file takes the verbatim path. They are unit
    // cells for that reason, not for convenience — an integration fixture
    // claiming to cover them would be green for the wrong reason.

    /// The C1 FALLBACK cell.
    ///
    /// When a string cannot be recovered verbatim, its control characters are
    /// re-escaped. C0 and DEL take `\xHH`; C1 (`0x80-0x9F`) must take
    /// `\u{XX}`, because the lexer REJECTS `\x` above `0x7F` — so emitting
    /// `\x85` would produce a file that no longer lexes, and emitting the raw
    /// byte (what this path used to do) plants an invisible control character
    /// in the user's source.
    ///
    /// RED pre-fix: `canonical_string_escape("\u{85}", Normal)` returned the
    /// raw two-byte UTF-8 sequence.
    #[test]
    fn canonical_escape_c1_uses_unicode_escape() {
        assert_eq!(
            canonical_string_escape("a\u{85}b", StringKind::Normal),
            "a\\u{85}b",
            "C1 NEL must escape as \\u{{85}} — \\x85 is rejected by the lexer \
             and a raw byte corrupts the source"
        );
        assert_eq!(
            canonical_string_escape("\u{9F}", StringKind::Normal),
            "\\u{9F}",
            "the top of the C1 range takes the same escape"
        );
        // C0 and DEL keep the byte escape they already had.
        assert_eq!(
            canonical_string_escape("\u{1B}", StringKind::Normal),
            "\\x1b"
        );
        assert_eq!(
            canonical_string_escape("\u{7F}", StringKind::Normal),
            "\\x7f"
        );
        // Printable non-ASCII is NOT escaped — it round-trips as itself.
        assert_eq!(
            canonical_string_escape("café 😀", StringKind::Normal),
            "café 😀"
        );

        // And the fallback spelling must actually re-lex to the same scalar.
        let src = format!(
            "void main():\n    String s = \"{}\"\n    print(s.len())\n",
            canonical_string_escape("\u{85}", StringKind::Normal)
        );
        let mut parser = crate::parser::Parser::new(&src);
        let _ = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "the C1 fallback spelling does not re-lex: {:?}",
            parser.errors.first()
        );
    }

    /// The DEGENERATE-SPAN cell.
    ///
    /// `relex_single_token` is the gate every verbatim arm passes through, and
    /// its contract is that a slice which is not exactly one clean token is
    /// REFUSED — so a stale, truncated, or synthetic span can only ever cost
    /// the author their spelling, never change what their program means.
    ///
    /// RED-verify by deleting the whole-slice span check inside
    /// `relex_single_token`: `"1.5"` then accepts as `IntLiteral(1)` and a
    /// float literal re-emits as an integer.
    #[test]
    fn relex_single_token_refuses_partial_and_multi_token_slices() {
        // Exactly one token covering the whole slice — accepted.
        assert!(matches!(
            relex_single_token("0x5C"),
            Some(Token::IntLiteral(92))
        ));
        assert!(matches!(
            relex_single_token("b'A'"),
            Some(Token::IntLiteral(65))
        ));
        assert!(matches!(
            relex_single_token("1.50"),
            Some(Token::FloatLiteral(_))
        ));

        // More than one token — refused, so no arm can emit half a slice.
        assert!(relex_single_token("1 + 2").is_none());
        assert!(relex_single_token("\"a\" \"b\"").is_none());
        // One token that does not COVER the slice — refused by the
        // whole-slice span check specifically (the multi-token check above
        // cannot see this: the trailing bytes produce no token of their own).
        // A span with slack would otherwise emit the slack along with the
        // literal.
        assert!(relex_single_token("42   ").is_none());
        assert!(relex_single_token("  42").is_none());
        // Not lexable at all — refused rather than panicking.
        assert!(relex_single_token("\"unterminated").is_none());
        assert!(relex_single_token("").is_none());
    }

    /// Totality over a WRONG span: the formatter must never index-panic and
    /// must never emit a name the AST does not contain.
    ///
    /// A `Formatter` is built over the real source, so this exercises the
    /// property through the public entry point on a program whose literals sit
    /// at every offset — including the f-string interpolation sub-expressions,
    /// which the parser deliberately gives SYNTHETIC spans far outside the
    /// source. If any arm indexed instead of `get`-ing, this panics.
    #[test]
    fn verbatim_recovery_is_total_over_synthetic_spans() {
        let src = concat!(
            "void main():\n",
            "    int n = 0x1F\n",
            "    float f = 2.50\n",
            "    String s = f\"n={n} f={f} lit={0x1F}\"\n",
            "    print(s)\n",
        );
        let out = fmt(src);
        assert_eq!(out, src, "f-string interpolation spans must not misroute");
    }

    #[test]
    fn test_simple_function() {
        let input = "void main():\n    pass\n";
        let output = fmt(input);
        assert_eq!(output, "void main():\n    pass\n");
    }

    #[test]
    fn test_expression_body() {
        let input = "int double(int x): x * 2\n";
        let output = fmt(input);
        assert_eq!(output, "int double(int x): x * 2\n");
    }

    #[test]
    fn test_struct() {
        let input = "struct Point:\n    float x\n    float y\n";
        let output = fmt(input);
        assert_eq!(output, "struct Point:\n    float x\n    float y\n");
    }

    #[test]
    fn test_enum() {
        let input = "enum Color:\n    Red\n    Green\n    Blue\n";
        let output = fmt(input);
        assert_eq!(output, "enum Color:\n    Red\n    Green\n    Blue\n");
    }

    #[test]
    fn test_comment_preservation() {
        let input = "# This is a comment\nvoid main():\n    pass\n";
        let output = fmt(input);
        assert!(output.contains("# This is a comment"));
    }

    #[test]
    fn test_inline_comment() {
        let input = "void main():\n    int x = 5  # inline\n";
        let output = fmt(input);
        // The inline comment should be preserved somewhere in the output
        assert!(output.contains("# inline"));
    }

    #[test]
    fn test_idempotency_simple() {
        let input = "void main():\n    int x = 42\n    print(\"{x}\")\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "Formatter is not idempotent");
    }

    #[test]
    fn test_meta_op_param_round_trips() {
        // A `meta op` parameter (parser: `is_meta_op=true`, placeholder
        // `type_=Void`) must re-emit as `meta op`, NOT `void op`. Emitting the
        // placeholder type loses `is_meta_op` on reparse, so the op-binding is
        // dropped from `meta_env`, the substitution sweep is skipped, and a
        // `meta[op]` infix survives to GIR lowering and panics
        // (`MetaOpInfix not substituted`). RED-verify: revert the `format_param`
        // meta-op arm and this test fails on both asserts (`void op` appears,
        // `meta op` does not).
        let input =
            "int apply[Numeric T](T a, T b, meta op):\n    T r = a meta[op] b\n    return r\n";
        let output = fmt(input);
        assert!(
            output.contains(", meta op)"),
            "meta op param must survive formatting, got:\n{output}"
        );
        assert!(
            !output.contains("void op"),
            "meta op param must NOT re-emit as its placeholder type `void op`, got:\n{output}"
        );
        // The infix `meta[op]` in the body is preserved verbatim.
        assert!(output.contains("meta[op]"), "meta[op] infix must survive, got:\n{output}");
        // Idempotent: formatting the output again is a no-op.
        let second = fmt(&output);
        assert_eq!(output, second, "meta op formatting is not idempotent");
    }

    #[test]
    fn test_static_public_preserved() {
        // Static globals are private-by-default, so the `public` keyword
        // is meaningful and must survive formatting — otherwise a
        // re-parse assigns Private visibility and the global stops being
        // importable from other modules.
        let input = "public static int x = 42\n";
        let output = fmt(input);
        assert_eq!(output, "public static int x = 42\n");
    }

    #[test]
    fn test_static_private_unchanged() {
        let input = "static int x = 42\n";
        let output = fmt(input);
        assert_eq!(output, "static int x = 42\n");
    }

    #[test]
    fn test_import() {
        let input = "import std.io\n";
        let output = fmt(input);
        assert_eq!(output, "import std.io\n");
    }

    #[test]
    fn test_from_import() {
        let input = "from std.fmt import Displayable\n";
        let output = fmt(input);
        assert_eq!(output, "from std.fmt import Displayable\n");
    }

    #[test]
    fn test_trait_and_equip() {
        let input = "\
trait Shape:
    float area(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return 3.14
";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "Formatter is not idempotent for trait+equip");
    }

    #[test]
    fn test_match_stmt() {
        let input = "\
void main():
    match x:
        case 1: print(\"one\")
        case 2: print(\"two\")
        else:
            print(\"other\")
";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "Formatter is not idempotent for match");
    }

    #[test]
    fn test_closure() {
        let input = "void main():\n    auto add = (int a, int b): a + b\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_if_elif_else() {
        let input = "\
void main():
    if x > 0:
        pass
    elif x < 0:
        pass
    else:
        pass
";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_for_loop() {
        let input = "void main():\n    for i in 0..10:\n        print(\"{i}\")\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_type_alias() {
        let input = "type StringList = Vector[String]\n";
        let output = fmt(input);
        assert_eq!(output, "type StringList = Vector[String]\n");
    }

    #[test]
    fn test_newtype() {
        let input = "newtype UserId(int)\n";
        let output = fmt(input);
        assert_eq!(output, "newtype UserId(int)\n");
    }

    #[test]
    fn test_import_name_sorting() {
        // Names within `from` imports should be sorted alphabetically.
        let input = "from std.io import Writer, Reader, Closer\n";
        let output = fmt(input);
        assert_eq!(output, "from std.io import Closer, Reader, Writer\n");
    }

    #[test]
    fn test_import_order_sorting() {
        // std/xtd imports come first, followed by third-party imports.
        // R39 gorget-arena verdict: blank line only on group transition, NOT
        // between every import (was `+30%` line inflation on import-heavy
        // files). Both std.io and xtd.log are in the "std/gg" group per
        // `is_std_import`; they emit consecutively. mylib.utils is a
        // different group; single blank line at the transition.
        let input = "import mylib.utils\n\nimport std.io\n\nimport xtd.log\n";
        let output = fmt(input);
        assert_eq!(output, "import std.io\nimport xtd.log\n\nimport mylib.utils\n");
    }

    #[test]
    fn test_method_chain_idempotent() {
        let input = "void main():\n    auto x = items.filter(pred).map(f).collect()\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_binary_expr_idempotent() {
        let input = "void main():\n    int x = a + b + c\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_binary_expr_preserves_operators() {
        let input = "void main():\n    bool x = a and b or c\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_list_comprehension_idempotent() {
        let input = "void main():\n    auto items = [x * 2 for x in range(10) if x > 0]\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_dict_comprehension_idempotent() {
        let input = "void main():\n    auto d = {k: v for k, v in items}\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_set_comprehension_idempotent() {
        let input = "void main():\n    auto s = {x for x in items if x > 0}\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }
}
