pub mod doc;

use std::rc::Rc;

use crate::lexer::token::{StringKind, StringLiteral, StringSegment};
use crate::parser::ast::*;
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
        self.col += s.len();
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
        if let Some(last_nl) = s.rfind('\n') {
            self.col = s.len() - last_nl - 1;
        } else {
            self.col += s.len();
        }
    }

    fn newline(&mut self) {
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
}

/// Column geometry derived for one recorded entry from the EMITTED buffer.
struct AlignGeom {
    buf_offset: usize,
    output_line: usize,
    indent_width: usize,
    lhs_width: usize,
    comment_len: usize,
    is_header: bool,
}

/// Owner-ratified alignment constants (R40, 2026-08-10). Column =
/// smallest multiple of `STRIDE` that is ≥ `max_lhs + MIN_GAP`; a comment
/// whose END column would exceed `MAX_WIDTH` triggers outlier exclusion.
const ALIGN_MIN_GAP: usize = 2;
const ALIGN_STRIDE: usize = 4;

/// Smallest multiple of `ALIGN_STRIDE` that is ≥ `x`.
fn round_up_to_stride(x: usize) -> usize {
    (x + ALIGN_STRIDE - 1) / ALIGN_STRIDE * ALIGN_STRIDE
}

/// Pure planning half of the trailing-comment aligner: given the emitted
/// buffer and the recorded entries (in buf order), return the gap rewrites
/// `(buf_offset, new_gap_len)` to apply, sorted LAST→FIRST so earlier
/// offsets stay valid as each is spliced in. Reads only `&str` + typed
/// entries so the mutating half can borrow `buf` mutably afterward.
fn plan_trailing_aligns(buf: &str, entries: &[TrailingAlign]) -> Vec<(usize, usize)> {
    if entries.len() < 2 {
        return Vec::new();
    }
    let bytes = buf.as_bytes();

    // Entries are recorded in emission order → `buf_offset` strictly
    // increasing. Walk the buffer once, deriving each entry's output line
    // (newline count), line start, indent width, and LHS width.
    let mut geoms: Vec<AlignGeom> = Vec::with_capacity(entries.len());
    let mut scan = 0usize;
    let mut nl_count = 0usize;
    let mut line_start = 0usize;
    for e in entries {
        while scan < e.buf_offset && scan < bytes.len() {
            if bytes[scan] == b'\n' {
                nl_count += 1;
                line_start = scan + 1;
            }
            scan += 1;
        }
        // Indent width = leading spaces on this output line (the emitter
        // writes exactly `indent*4` spaces before content).
        let mut iw = 0usize;
        let mut k = line_start;
        while k < e.buf_offset && bytes[k] == b' ' {
            iw += 1;
            k += 1;
        }
        let lhs_width = e.buf_offset.saturating_sub(line_start).saturating_sub(iw);
        geoms.push(AlignGeom {
            buf_offset: e.buf_offset,
            output_line: nl_count,
            indent_width: iw,
            lhs_width,
            comment_len: e.comment_len,
            is_header: e.is_header,
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
    let mut groups: Vec<Vec<AlignGeom>> = Vec::new();
    for g in deduped {
        let start_new = match groups.last().and_then(|grp| grp.last()) {
            None => true,
            Some(prev) => {
                prev.is_header
                    || g.is_header
                    || g.output_line != prev.output_line + 1
                    || g.indent_width != prev.indent_width
            }
        };
        if start_new {
            groups.push(vec![g]);
        } else {
            groups.last_mut().unwrap().push(g);
        }
    }

    let mut rewrites: Vec<(usize, usize)> = Vec::new();
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
                // survivors + excluded all keep their natural 2-space gap.
                active.clear();
                align_col = 0;
                break;
            }
            let max_lhs = active.iter().map(|&i| group[i].lhs_width).max().unwrap();
            let col = round_up_to_stride(max_lhs + ALIGN_MIN_GAP);
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
                        + round_up_to_stride(group[i].lhs_width + ALIGN_MIN_GAP)
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
            rewrites.push((g.buf_offset, align_col - g.lhs_width));
        }
    }

    // Apply LAST→FIRST so a splice never shifts an as-yet-unwritten offset.
    rewrites.sort_by(|a, b| b.0.cmp(&a.0));
    rewrites
}

// ══════════════════════════════════════════════════════════════
// Formatter — walks AST and emits formatted source
// ══════════════════════════════════════════════════════════════

pub struct Formatter {
    emitter: Emitter,
    comments: Vec<Spanned<String>>,
    comment_cursor: usize,
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
}

impl Formatter {
    pub fn new(comments: Vec<Spanned<String>>, source: Rc<str>) -> Self {
        Self {
            emitter: Emitter::new(),
            comments,
            comment_cursor: 0,
            trailing_aligns: Vec::new(),
            source,
        }
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
    /// The `+ 1` bump matches the fact that almost every caller of this
    /// helper (call args, params, generic args, comprehensions, method
    /// chain, binary chain continuations, closure params, container
    /// literals) wraps the returned strings in an outer `Doc::Indent(...)`
    /// so items land one level deeper than the caller's cursor. Callers
    /// whose sub-render placement is different can use
    /// `element_to_string_at(base_indent, f)` directly to override.
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
        let mut fmt = Formatter::new(vec![], self.source.clone());
        fmt.emitter.indent = base_indent;
        fmt.emitter.at_line_start = false;
        f(&mut fmt);
        fmt.emitter.finish()
    }

    /// Render a Doc tree at the current cursor position and write it
    /// into the output buffer. The Doc handles line-break decisions.
    fn write_doc(&mut self, doc: &doc::Doc) {
        let rendered = doc::render_at(
            doc,
            doc::MAX_WIDTH,
            self.emitter.col,
            self.emitter.indent,
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
            if self.comments[self.comment_cursor].span.start >= pos {
                break;
            }
            let c_end = self.comments[self.comment_cursor].span.end;
            self.emitter.write(&self.comments[self.comment_cursor].node);
            self.emitter.newline();
            self.comment_cursor += 1;
            if self.blank_line_follows(c_end) {
                self.emitter.blank_line();
            }
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
            let c_start = self.comments[self.comment_cursor].span.start;
            let c_end = self.comments[self.comment_cursor].span.end;
            if first {
                first = false;
                if self.blank_line_directly_above(c_start) {
                    self.emitter.blank_line();
                }
            }
            self.emitter.write(&self.comments[self.comment_cursor].node);
            self.emitter.newline();
            self.comment_cursor += 1;
            if self.blank_line_follows(c_end) {
                self.emitter.blank_line();
            }
        }
    }

    /// R39 fmt collection-literal interior-comment escape: returns true iff
    /// any UNEMITTED comment's span.start lies STRICTLY INSIDE `(start, end)`.
    ///
    /// Used by the 4 collection-literal arms of `format_expr`
    /// (ArrayLiteral / TupleLiteral / DictLiteral / StructLiteral) to
    /// decide whether to dispatch to `format_bracketed_broken_with_comments`
    /// instead of the flat `doc::surround` path. When ANY element-interior
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
        self.comments[self.comment_cursor..]
            .iter()
            .any(|c| c.span.start > start && c.span.start < end)
    }

    /// R39 fmt collection-literal interior-comment escape (Core #4
    /// producer chokepoint): render `open elems close` in BROKEN
    /// (multi-line) form with the outer formatter's comment sideband
    /// interleaved per element. Called by the 4 collection-literal arms
    /// of `format_expr` when `has_interior_comments(container_span)`
    /// fires — i.e. when the flat `doc::surround` path would DROP interior
    /// comments (the exact snag class).
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
    /// `container_end` MUST be the AST-recorded EXCLUSIVE end of the
    /// container (position of the byte AFTER the closing bracket, matching
    /// logos' end-exclusive convention — verified at `src/parser/expr.rs`
    /// `parse_array_or_comprehension` which merges `start.merge(previous_span())`
    /// where `previous_span` returns the `]` token span end-exclusive).
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
            format_elem(self, elem);
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
    /// `formatter_sibling_loops_hook_pairing` lint clean at exactly 12
    /// sibling call-sites for `emit_trailing_comment_after`; this
    /// header hook has its own count (4 sites, one per structural
    /// container) enforced by
    /// `formatter_container_header_hook_arm_count`.
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

    /// R39 snag #2 (`gorget-arena` snag #2 — durable repro at
    /// `tests/fixtures/known_gaps/gorget_arena_snag_2_fmt_trailing_comment_detach/`):
    /// after emitting a sibling item/stmt/field that ends at source
    /// position `prev_end`, flush any trailing comment(s) that shared
    /// the SAME SOURCE LINE as that previous emit. The comments stay
    /// attached to their owning node (inline, with a two-space visual
    /// gap) instead of drifting to lead the NEXT sibling — the exact
    /// bug the fixture pins.
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
    /// advances. The loop repeats to handle the rare multi-comment same-
    /// line case (e.g. `stmt  # a  # b`, which the lexer records as two
    /// separate `Comment` tokens on one line — both are trailing).
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
            let c = &self.comments[self.comment_cursor];
            let comment_start = c.span.start;
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
            // Same-line trailing: inject inline with a two-space gap.
            let comment_len = c.node.chars().count();
            let mut inlined = String::with_capacity(c.node.len() + 2);
            inlined.push_str("  ");
            inlined.push_str(&c.node);
            // Record the gap-start for the R40 aligner BEFORE the injection.
            // The buffer currently ends `...LHS\n`; `inject_before_newline`
            // pops that `\n`, so the first gap space lands where the `\n`
            // sits now (`buf.len() - 1`). If the buffer does NOT end in `\n`
            // (rare — sibling emit didn't newline-terminate), the injection
            // appends, so the gap starts at `buf.len()`. (`c` is no longer
            // borrowed past `inlined`, so the mutable pushes below are fine.)
            let buf_offset = if self.emitter.buf.ends_with('\n') {
                self.emitter.buf.len() - 1
            } else {
                self.emitter.buf.len()
            };
            self.emitter.inject_before_newline(&inlined);
            self.trailing_aligns.push(TrailingAlign {
                buf_offset,
                comment_len,
                is_header,
            });
            self.comment_cursor += 1;
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
        // Already sorted LAST→FIRST; each gap is exactly the injected two
        // spaces at `[off, off+2)` (both ASCII → valid char boundaries).
        for (off, new_gap) in rewrites {
            self.emitter
                .buf
                .replace_range(off..off + 2, &" ".repeat(new_gap));
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
    /// with the two-space inline gap into an empty buffer (`  # a`), and the
    /// next comment then glued onto the same line (`  # a# b`). Making the
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
    }

    // ── Items ───────────────────────────────────────────────

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
                        self.format_type(then_type);
                        self.emitter.write(" if ");
                        self.format_expr(condition);
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
                    self.format_param(&p.node);
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
                self.emitter.write("meta if ");
                self.format_expr(&mi.condition);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                // R39 snag #2: trailing-hook after each nested item —
                // same pairing as the format_module loops.
                for item in &mi.then_items {
                    self.emit_comments_before(item.span.start);
                    self.format_item(item);
                    self.emit_trailing_comment_after(item.span.end, false);
                }
                self.emitter.dedent();
                for (cond, items) in &mi.elif_branches {
                    self.emitter.write("elif ");
                    self.format_expr(cond);
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    for item in items {
                        self.emit_comments_before(item.span.start);
                        self.format_item(item);
                        self.emit_trailing_comment_after(item.span.end, false);
                    }
                    self.emitter.dedent();
                }
                if let Some(ref else_items) = mi.else_items {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    for item in else_items {
                        self.emit_comments_before(item.span.start);
                        self.format_item(item);
                        self.emit_trailing_comment_after(item.span.end, false);
                    }
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
        self.emitter.write("test \"");
        self.emitter.write(&t.name.node);
        self.emitter.write("\":");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&t.body);
        self.emitter.dedent();
    }

    fn format_bench(&mut self, b: &BenchDef) {
        self.format_doc_comment(&b.doc_comment);
        self.format_attributes(&b.attributes);
        self.emitter.write("bench \"");
        self.emitter.write(&b.name.node);
        self.emitter.write("\":");
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
                            self.emitter.write("\"");
                            self.emitter.write(s);
                            self.emitter.write("\"");
                        }
                        AttributeArg::KeyValue(k, v) => {
                            self.emitter.write(k);
                            self.emitter.write(" = ");
                            self.emitter.write("\"");
                            self.emitter.write(v);
                            self.emitter.write("\"");
                        }
                    }
                }
                self.emitter.write(")");
            }
            self.emitter.newline();
        }
    }

    fn format_visibility(&mut self, vis: &Visibility) {
        // Public is the default — no keyword needed.
        // Private is the opt-in keyword.
        if *vis == Visibility::Private {
            self.emitter.write("private ");
        }
    }

    fn format_function(&mut self, f: &FunctionDef) {
        self.format_doc_comment(&f.doc_comment);
        self.format_attributes(&f.attributes);
        self.format_visibility(&f.visibility);
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
            self.emitter.write("\"");
            self.emitter.write(abi);
            self.emitter.write("\" ");
        }
        self.format_qualifiers(&f.qualifiers);
        if f.returns_borrowed {
            self.emitter.write("borrowed ");
        }
        // type-first: `ReturnType name(params)`
        // Bare tuple return: emit `T1, T2` not `(T1, T2)` in return position
        if let Type::Tuple(types) = &f.return_type.node {
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(", ");
                }
                self.format_type(ty);
            }
        } else {
            self.format_type(&f.return_type);
        }
        self.emitter.write(" ");
        self.emitter.write(&f.name.node);
        if let Some(ref gp) = f.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.format_params_wrapped(&f.params);
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
            FunctionBody::Block(block) => {
                self.emitter.write(":");
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
            FunctionBody::Expression(expr) => {
                self.emitter.write(": ");
                self.format_expr(expr);
                self.emitter.newline();
            }
            FunctionBody::Declaration => {
                self.emitter.newline();
            }
            FunctionBody::Extern(sym) => {
                self.emitter.write(" = \"");
                self.emitter.write(sym);
                self.emitter.write("\"");
                self.emitter.newline();
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
        self.format_visibility(&s.visibility);
        self.emitter.write("struct ");
        self.emitter.write(&s.name.node);
        if let Some(ref gp) = s.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.emitter.write(":");
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `struct S:  # header` shape —
        // header trailing comment stays on the header line, not
        // dedented into the body.
        self.emit_trailing_comment_after_header(s.name.span.end);
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
                if field.node.visibility == Visibility::Private {
                    self.emitter.write("private ");
                }
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
        self.emitter.dedent();
    }

    fn format_enum(&mut self, e: &EnumDef) {
        self.format_doc_comment(&e.doc_comment);
        self.format_attributes(&e.attributes);
        self.format_visibility(&e.visibility);
        self.emitter.write("enum ");
        self.emitter.write(&e.name.node);
        if let Some(ref gp) = e.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.emitter.write(":");
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `enum E:  # header` shape.
        self.emit_trailing_comment_after_header(e.name.span.end);
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
                    VariantFields::Tuple(types) => {
                        self.emitter.write("(");
                        for (i, ty) in types.iter().enumerate() {
                            if i > 0 {
                                self.emitter.write(", ");
                            }
                            self.format_type(ty);
                        }
                        self.emitter.write(")");
                    }
                }
                self.emitter.newline();
                // R39 snag #2: trailing-hook for `Variant()  # doc`
                // on enum variants — same shape as struct fields.
                self.emit_trailing_comment_after(variant.span.end, false);
            }
        }
        self.emitter.dedent();
    }

    fn format_trait(&mut self, t: &TraitDef) {
        self.format_doc_comment(&t.doc_comment);
        self.format_attributes(&t.attributes);
        self.format_visibility(&t.visibility);
        self.emitter.write("trait ");
        self.emitter.write(&t.name.node);
        if let Some(ref gp) = t.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        if !t.extends.is_empty() {
            self.emitter.write(" extends ");
            for (i, bound) in t.extends.iter().enumerate() {
                if i > 0 {
                    // Parser consumes `&` between supertrait names
                    // (parse_trait_bound_list); emit the same so fmt
                    // round-trips.
                    self.emitter.write(" & ");
                }
                self.format_trait_bound(bound);
            }
        }
        self.emitter.write(":");
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `trait T:  # header` / `trait T extends A:  # x`
        // — for the same-line-header shape, `t.name.span.end` is on the
        // same source line as the `:`, so the helper's newline scan
        // covers `extends`-list variants without extra tracking.
        self.emit_trailing_comment_after_header(t.name.span.end);
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
                                self.format_trait_bound(bound);
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
        self.emitter.dedent();
    }

    fn format_equip(&mut self, e: &EquipBlock) {
        self.emitter.write("equip ");
        if let Some(ref gp) = e.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.format_type(&e.type_);
        if let Some(ref trait_) = e.trait_ {
            self.emitter.write(" with ");
            self.format_type(&trait_.trait_name);
        }
        if let Some(ref via) = e.via_field {
            self.emitter.write(" via ");
            self.emitter.write(&via.node);
        }
        self.emitter.write(":");
        self.emitter.newline();
        // R39 snag #2 sub-task 5b: `equip S:  # x`, `equip S with T:  # x`,
        // `equip S via f:  # x` — anchor on the type's own span end.
        // For the same-line-header shape, whatever `with T` / `via f`
        // sits between anchor and `:` stays on the same source line, so
        // the helper's newline scan is unaffected.
        self.emit_trailing_comment_after_header(e.type_.span.end);
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
                let items: Vec<doc::Doc> = sorted.iter().map(|n| doc::text(*n)).collect();
                let doc = doc::surround("{", items, "}", true);
                self.write_doc(&doc);
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
        self.format_visibility(&ta.visibility);
        self.emitter.write("type ");
        self.emitter.write(&ta.name.node);
        if let Some(ref gp) = ta.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.emitter.write(" = ");
        self.format_type(&ta.type_);
        self.emitter.newline();
    }

    fn format_newtype(&mut self, nt: &NewtypeDef) {
        self.format_visibility(&nt.visibility);
        self.emitter.write("newtype ");
        self.emitter.write(&nt.name.node);
        self.emitter.write("(");
        self.format_type(&nt.inner_type);
        self.emitter.write(")");
        self.emitter.newline();
    }

    fn format_const_decl(&mut self, cd: &ConstDecl) {
        self.format_visibility(&cd.visibility);
        self.emitter.write("const ");
        self.format_type(&cd.type_);
        self.emitter.write(" ");
        self.emitter.write(&cd.name.node);
        self.emitter.write(" = ");
        self.format_expr(&cd.value);
        self.emitter.newline();
    }

    fn format_static_decl(&mut self, sd: &StaticDecl) {
        // Static globals are private-by-default (opposite of functions / structs
        // which are public-by-default). Emit `public` explicitly — `format_visibility`
        // drops it for the regular-item convention, which would silently flip
        // visibility on round-trip through `gg fmt`.
        if sd.visibility == Visibility::Public {
            self.emitter.write("public ");
        }
        self.emitter.write("static ");
        self.format_type(&sd.type_);
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
        let header_anchor_end = eb.abi.as_ref().map_or(eb.span.start, |a| a.span.end);
        self.emitter.write("extern");
        if let Some(ref abi) = eb.abi {
            self.emitter.write(" \"");
            self.emitter.write(&abi.node);
            self.emitter.write("\"");
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
        self.emitter.dedent();
    }

    // ── Generics & Bounds ───────────────────────────────────

    fn format_generic_param(&mut self, param: &GenericParam) {
        match param {
            GenericParam::Type { name, bounds } => {
                for (i, tb) in bounds.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(" & ");
                    }
                    self.format_trait_bound(tb);
                }
                if !bounds.is_empty() {
                    self.emitter.write(" ");
                }
                self.emitter.write(&name.node);
            }
            GenericParam::Const { type_, name } => {
                self.emitter.write("const ");
                self.format_type(type_);
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
            if let Some(ref args) = tb.node.generic_args {
                for arg in args {
                    if !first {
                        self.emitter.write(", ");
                    }
                    self.format_type(arg);
                    first = false;
                }
            }
            for binding in &tb.node.assoc_type_bindings {
                if !first {
                    self.emitter.write(", ");
                }
                self.emitter.write(&binding.name.node);
                self.emitter.write(" = ");
                self.format_type(&binding.type_);
                first = false;
            }
            self.emitter.write("]");
        }
    }

    // ── Parameters ──────────────────────────────────────────

    /// Format a parenthesized parameter list with line-width-aware wrapping.
    /// Writes `(param1, param2)` on one line if it fits, otherwise wraps:
    /// ```text
    /// (
    ///     param1,
    ///     param2,
    /// )
    /// ```
    fn format_params_wrapped(&mut self, params: &[Spanned<Param>]) {
        let items: Vec<doc::Doc> = params.iter().map(|p| {
            doc::text(self.element_to_string(|f| f.format_param(&p.node)))
        }).collect();
        let doc = doc::surround("(", items, ")", true);
        self.write_doc(&doc);
    }

    /// Format a parenthesized call argument list with line-width-aware wrapping.
    fn format_call_args_wrapped(&mut self, args: &[Spanned<CallArg>]) {
        let items: Vec<doc::Doc> = args.iter().map(|a| {
            doc::text(self.element_to_string(|f| f.format_call_arg(&a.node)))
        }).collect();
        let doc = doc::surround("(", items, ")", true);
        self.write_doc(&doc);
    }

    /// Format a bracketed generic parameter list with line-width-aware wrapping.
    fn format_generic_params_wrapped(&mut self, gp: &Spanned<GenericParams>) {
        let items: Vec<doc::Doc> = gp.node.params.iter().map(|p| {
            doc::text(self.element_to_string(|f| f.format_generic_param(&p.node)))
        }).collect();
        let doc = doc::surround("[", items, "]", true);
        self.write_doc(&doc);
    }

    /// Format a bracketed generic argument list (types) with wrapping.
    fn format_generic_args_wrapped(&mut self, args: &[Spanned<Type>]) {
        let items: Vec<doc::Doc> = args.iter().map(|t| {
            doc::text(self.element_to_string(|f| f.format_type(t)))
        }).collect();
        let doc = doc::surround("[", items, "]", true);
        self.write_doc(&doc);
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
    fn format_method_chain(&mut self, expr: &Spanned<Expr>) {
        let (root, segments) = collect_method_chain(expr);
        let root_str = self.element_to_string(|f| f.format_expr(root));

        let mut parts = Vec::with_capacity(segments.len() + 1);
        // Format each .method(args) segment as a string
        for (method, generic_args, args) in &segments {
            let seg_str = self.element_to_string(|f| {
                f.emitter.write(".");
                f.emitter.write(&method.node);
                if let Some(ga) = generic_args {
                    f.format_generic_args_wrapped(ga);
                }
                f.format_call_args_wrapped(args);
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
        let operand_strs: Vec<String> = operands
            .iter()
            .enumerate()
            .map(|(i, o)| {
                let position = if i == 0 { BinOpPos::Left } else { BinOpPos::Right };
                self.element_to_string(|f| {
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
        self.format_type(&param.type_);
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
        if block.stmts.len() != 1 {
            return false;
        }
        let stmt = &block.stmts[0];
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

    fn format_arm_body(&mut self, body: &Spanned<Expr>) {
        // Snag #15c note: `parse_body_or_expr` (catch/rethrow arm) wraps
        // the indented body as `Expr::Do { body }`, NOT `Expr::Block`. The
        // match `else:` uses `parse_arm_body` which returns Expr::Block for
        // indented. Both AST forms carry an indented statement list and
        // should format WITHOUT re-emitting `do:` at these arm positions —
        // the parser accepts the indented form directly at all three sites.
        let block_opt = match &body.node {
            Expr::Block(block) => Some(block),
            Expr::Do { body } => Some(body),
            _ => None,
        };
        if let Some(block) = block_opt {
            if block.stmts.len() > 1 {
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(block);
                self.emitter.dedent();
                return;
            }
        }
        self.emitter.write(" ");
        self.format_expr(body);
    }

    fn format_elif_else_blocks(
        &mut self,
        elif_branches: &[(Spanned<Expr>, Block)],
        else_body: Option<&Block>,
    ) {
        for (cond, body) in elif_branches {
            // R41 T-FMT-A follow-up: a comment on its own line BEFORE `elif`
            // documents the BRANCH, but with no leading hook here it fell
            // through to `format_block_stmts` and was re-emitted INSIDE the
            // branch body — same misattribution class as the match/select arm
            // loops. Claim it at branch indent first.
            self.emit_comments_before(cond.span.start);
            self.emitter.write("elif ");
            self.format_expr(cond);
            self.emitter.write(":");
            self.emit_trailing_comment_after_header(cond.span.end);
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(body);
            self.emitter.dedent();
        }
        if let Some(else_body) = else_body {
            // Same class as the `elif` hook above. There is no recorded span
            // for the `else` KEYWORD, but the body's span starts after it —
            // and after any comment written above `else:` — so it is a sound
            // upper bound for "comments that belong to this branch".
            self.emit_comments_before(else_body.span.start);
            self.emitter.write("else:");
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(else_body);
            self.emitter.dedent();
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
                self.format_type(type_);
                self.emitter.write(" ");
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
                    } else {
                        self.format_pattern(pattern);
                    }
                } else {
                    self.format_pattern(pattern);
                }
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Expr(expr) => {
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Assign { target, value } => {
                self.format_expr(target);
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::CompoundAssign { target, op, value } => {
                self.format_expr(target);
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
                self.format_ownership_modifier_operand(iterable);
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
                    self.emitter.write("else:");
                    self.emitter.newline();
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
                self.format_expr(condition);
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(condition.span.end);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
                if let Some(else_body) = else_body {
                    self.emitter.write("else:");
                    self.emitter.newline();
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
                self.format_expr(condition);
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(condition.span.end);
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(then_body);
                self.emitter.dedent();
                self.format_elif_else_blocks(elif_branches, else_body.as_ref());
            }
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.emitter.write("match ");
                self.format_expr(scrutinee);
                self.emitter.write(":");
                self.emit_trailing_comment_after_header(scrutinee.span.end);
                self.emitter.newline();
                self.emitter.indent();
                for item in arms {
                    match item {
                        crate::parser::ast::MatchItem::Arm(arm) => {
                            self.emit_comments_before(arm.span.start);
                            self.format_match_arm(arm);
                        }
                        crate::parser::ast::MatchItem::MetaFor { vars, range, arm_template, .. } => {
                            self.emitter.write("meta for ");
                            let joined = vars.iter().map(|v| v.node.as_str()).collect::<Vec<_>>().join(", ");
                            self.emitter.write(&joined);
                            self.emitter.write(" in ");
                            self.format_expr(range);
                            self.emitter.write(":");
                            self.emitter.newline();
                            self.emitter.indent();
                            self.format_match_arm(arm_template);
                            self.emitter.dedent();
                        }
                    }
                }
                if let Some(else_body) = else_arm {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                self.emitter.dedent();
            }
            Stmt::Select { arms, else_arm } => {
                self.emitter.write("select:");
                self.emitter.newline();
                self.emitter.indent();
                for arm in arms {
                    self.emit_comments_before(arm.span.start);
                    self.emitter.write("case ");
                    match &arm.op {
                        SelectOp::Recv { type_, name, channel } => {
                            self.format_type(type_);
                            self.emitter.write(" ");
                            self.emitter.write(&name.node);
                            self.emitter.write(" = ");
                            self.format_expr(channel);
                            self.emitter.write(".recv()");
                        }
                        SelectOp::Send { channel, value } => {
                            self.format_expr(channel);
                            self.emitter.write(".send(");
                            self.format_expr(value);
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
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                self.emitter.dedent();
            }
            Stmt::With { bindings, body } => {
                self.emitter.write("with ");
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_expr(&binding.expr);
                    self.emitter.write(" as ");
                    self.emitter.write(&binding.name.node);
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
                self.format_expr(condition);
                if let Some(msg) = message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Stmt::AssertReturn { condition, message } => {
                self.emitter.write("assert return");
                self.format_assert_return_expr(condition);
                if let Some(msg) = message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Stmt::Snapshot { name, value } => {
                self.emitter.write("snapshot \"");
                self.emitter.write(&name.node);
                self.emitter.write("\" ");
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
                self.format_expr(condition);
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
                self.format_expr(range);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
                self.emitter.write("meta match ");
                self.format_expr(scrutinee);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for (case_expr, body) in arms {
                    self.emit_comments_before(case_expr.span.start);
                    self.emitter.write("case ");
                    self.format_expr(case_expr);
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(body);
                    self.emitter.dedent();
                }
                if let Some(else_body) = else_arm {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                self.emitter.dedent();
            }
            Stmt::MetaWhile { condition, body, .. } => {
                self.emitter.write("meta while ");
                self.format_expr(condition);
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
                self.emitter.write("on error:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
        }
    }

    fn format_match_arm(&mut self, arm: &MatchArm) {
        self.emitter.write("case ");
        self.format_pattern(&arm.pattern);
        if let Some(ref guard) = arm.guard {
            self.emitter.write(" if ");
            self.format_expr(guard);
        }
        self.emitter.write(":");
        // R39 gorget-arena block-header trailing: preserve `case P:  # comment`
        // as trailing on the header line for indented-body arms. Uses
        // arm.pattern.span.end (or guard.span.end if a guard is present) as
        // the anchor before the `:` and any trailing comment.
        let arm_anchor = arm.guard.as_ref().map(|g| g.span.end).unwrap_or(arm.pattern.span.end);
        self.emit_trailing_comment_after_header(arm_anchor);
        // Check if the body is a Block expression (multi-line arm)
        if let Expr::Block(ref block) = arm.body.node {
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(block);
            self.emitter.dedent();
        } else {
            self.emitter.write(" ");
            self.format_expr(&arm.body);
            self.emitter.newline();
        }
    }

    // ── Types ───────────────────────────────────────────────

    fn format_type(&mut self, ty: &Spanned<Type>) {
        match &ty.node {
            Type::Primitive(p) => self.emitter.write(primitive_type_str(*p)),
            Type::Named { name, generic_args } => {
                self.emitter.write(&name.node);
                if !generic_args.is_empty() {
                    self.format_generic_args_wrapped(generic_args);
                }
            }
            Type::Array { element, size } => {
                self.format_type(element);
                self.emitter.write("[");
                self.format_expr(size);
                self.emitter.write("]");
            }
            Type::Slice { element } => {
                self.format_type(element);
                self.emitter.write("[]");
            }
            Type::Tuple(types) => {
                self.emitter.write("(");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_type(ty);
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
                self.format_type(return_type);
                self.emitter.write("(");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_type(p);
                    if let Some(ownership) = param_ownerships.get(i) {
                        match ownership {
                            Ownership::MutableBorrow => self.emitter.write(" &"),
                            // D27 Round A: `Type ^` in fn-type param list (was `Type !`).
                            Ownership::Move => self.emitter.write(" ^"),
                            Ownership::Borrow => {}
                        }
                    }
                }
                self.emitter.write(")");
            }
            Type::Ref(inner) => {
                self.format_type(inner);
                self.emitter.write(" &");
            }
            Type::Owned(inner) => {
                self.format_type(inner);
                // D27 Round A: type-arg suffix `Vector[T ^]` (was `Vector[T !]`).
                self.emitter.write(" ^");
            }
            Type::Pointer(inner) => {
                self.format_type(inner);
                self.emitter.write("*");
            }
            Type::SelfType => self.emitter.write("Self"),
            Type::Inferred => self.emitter.write("auto"),
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
                self.format_assert_return_expr(left);
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
            self.format_expr(expr);
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
    fn format_postfix_receiver(&mut self, receiver: &Spanned<Expr>) {
        let wrap = needs_parens_as_postfix_receiver(&receiver.node);
        self.format_expr_maybe_parens(receiver, wrap);
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

    /// Choose the surface text for an integer literal (gorget-js snag #15f).
    ///
    /// The lexer discards an int literal's RADIX — `0x5C`, `0o134`, `0b1011100`
    /// and `92` all lex to the same `IntLiteral(92)` — because radix is *syntax*
    /// (Layering rule 1: layers are lossy on syntax, lossless on invariants), so
    /// the AST is correct to carry only the value. The formatter is the ONE
    /// consumer that needs the original spelling, and it recovers it the same way
    /// it recovers comments: from the source text at the node's span.
    ///
    /// If the original lexeme at `span` is in-bounds AND parses back to exactly
    /// `n` under the lexer's own rules (`parse_int_lexeme`), emit it verbatim —
    /// this preserves the author's radix, hex digit-case, and `_` grouping in a
    /// single round-trip-checked step. Otherwise fall back to canonical decimal.
    /// The round-trip check is self-verifying: it is impossible to emit a lexeme
    /// that denotes a different value than the AST node.
    ///
    /// The decimal fallback covers every non-preservable case safely, WITHOUT a
    /// panic: an out-of-bounds/synthetic span (f-string interp spans live at
    /// `1 << 40`), an empty sub-formatter source, a byte literal `b'A'` (lexes to
    /// `IntLiteral(65)` but its lexeme does not parse as an integer), or a span
    /// that does not land on char boundaries — `str::get` returns `None` for all
    /// of them.
    fn int_literal_text(&self, n: i64, span: Span) -> String {
        if let Some(lexeme) = self.source.get(span.start..span.end) {
            if parse_int_lexeme(lexeme) == Some(n) {
                return lexeme.to_string();
            }
        }
        n.to_string()
    }

    fn format_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::IntLiteral(n) => {
                let text = self.int_literal_text(*n, expr.span);
                self.emitter.write(&text);
            }
            Expr::FloatLiteral(n) => {
                let s = format!("{}", n);
                // Ensure it looks like a float
                if !s.contains('.') && !s.contains('e') && !s.contains('E') {
                    self.emitter.write(&format!("{}.0", s));
                } else {
                    self.emitter.write(&s);
                }
            }
            Expr::BoolLiteral(b) => {
                self.emitter.write(if *b { "true" } else { "false" });
            }
            Expr::StringLiteral(s, _) => {
                self.format_string_lit(s);
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
                self.format_postfix_receiver(callee);
                if let Some(ga) = generic_args {
                    self.format_generic_args_wrapped(ga);
                }
                self.format_call_args_wrapped(args);
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
                    self.format_postfix_receiver(receiver);
                    self.emitter.write(".");
                    self.emitter.write(&method.node);
                    if let Some(ga) = generic_args {
                        self.format_generic_args_wrapped(ga);
                    }
                    self.format_call_args_wrapped(args);
                }
            }
            Expr::FieldAccess { object, field } => {
                // FMT-A: object at postfix bp 35.
                self.format_postfix_receiver(object);
                self.emitter.write(".");
                self.emitter.write(&field.node);
            }
            Expr::TupleFieldAccess { object, index } => {
                // FMT-A: object at postfix bp 35.
                self.format_postfix_receiver(object);
                self.emitter.write(".");
                self.emitter.write(&index.to_string());
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
                self.format_postfix_receiver(object);
                self.emitter.write("[");
                if let Expr::Range { start, end, inclusive: false, colon: true } = &index.node {
                    // D22 slice: `[a:b]`, `[a:]`, `[:b]`, `[:]`. Endpoints
                    // are inside `[...]` — precedence resets, so no wrap
                    // logic needed for the operands.
                    if let Some(s) = start {
                        self.format_expr(s);
                    }
                    self.emitter.write(":");
                    if let Some(e) = end {
                        self.format_expr(e);
                    }
                } else {
                    self.format_expr(index);
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
                if let Some(s) = start {
                    self.format_binop_operand(s, 23, BinOpPos::Left, false);
                }
                self.emitter.write(if *inclusive { "..=" } else { ".." });
                if let Some(e) = end {
                    self.format_prefix_operand(e, 24);
                }
            }
            Expr::OptionalChain { object, field } => {
                // FMT-A: object at postfix bp 35.
                self.format_postfix_receiver(object);
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
                let lhs_s = self.element_to_string(|f| {
                    f.format_binop_operand(lhs, 3, BinOpPos::Left, false);
                });
                let rhs_s = self.element_to_string(|f| {
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
                self.format_postfix_receiver(expr);
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
                self.emitter.write("if ");
                self.format_expr(condition);
                self.emitter.write(": ");
                self.format_expr(then_branch);
                for (cond, body) in elif_branches {
                    self.emitter.write(" elif ");
                    self.format_expr(cond);
                    self.emitter.write(": ");
                    self.format_expr(body);
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
                self.format_expr(scrutinee);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for arm in arms {
                    self.emit_comments_before(arm.span.start);
                    self.format_match_arm(arm);
                }
                if let Some(else_arm) = else_arm {
                    self.emitter.write("else:");
                    self.format_arm_body(else_arm);
                    self.emitter.newline();
                }
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
                // tails (`else: do: ^b` rejects `E_MoveInOperandPosition`;
                // `else: ^b` compiles). Consolidated into
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
            Expr::Do { body } => {
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
                if self.try_inline_single_terminal_stmt(body) {
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
                let items: Vec<doc::Doc> = params.iter().map(|p| {
                    doc::text(self.element_to_string(|f| f.format_closure_param(&p.node)))
                }).collect();
                let params_doc = doc::surround("(", items, ")", true);
                self.write_doc(&params_doc);
                self.emitter.write(": ");
                // Total prelude stmts injected by the parser for tuple destructuring.
                let prelude_skip: usize = params
                    .iter()
                    .filter_map(|p| p.node.destructure.as_ref().map(|b| b.len()))
                    .sum();
                if let Expr::Block(ref block) = body.node {
                    // If the only post-prelude stmt is `return expr;`, render the closure
                    // as expression-body — mirrors the parser's wrap of inline `((...)): expr`
                    // bodies into `Block { ..prelude.., Stmt::Return(Some(expr)) }`.
                    let post_prelude: Vec<&Spanned<Stmt>> =
                        block.stmts.iter().skip(prelude_skip).collect();
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
                    } else {
                        self.emitter.newline();
                        self.emitter.indent();
                        if prelude_skip > 0 {
                            for stmt in &post_prelude {
                                self.emit_comments_before(stmt.span.start);
                                self.format_stmt(stmt);
                                // R39 snag #2: mirror `format_block_stmts`
                                // hook. This branch bypasses the shared
                                // helper because it must SKIP the parser-
                                // synthesized destructure-prelude stmts,
                                // but the trailing-comment semantics are
                                // identical for the post-prelude tail.
                                self.emit_trailing_comment_after(stmt.span.end, false);
                            }
                        } else {
                            self.format_block_stmts(block);
                        }
                        self.emitter.dedent();
                    }
                } else {
                    self.format_expr(body);
                }
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
                let expr_s = self.element_to_string(|f| f.format_expr(expr));
                let var_s = self.element_to_string(|f| f.format_pattern(variable));
                let own_prefix = match ownership {
                    Ownership::Borrow => "",
                    Ownership::MutableBorrow => "&",
                    // D27 Round A: comprehension for-binder `^` (was `!`).
                    Ownership::Move => "^",
                };
                let iter_s = self.element_to_string(|f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string(|f| f.format_expr(c))
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
                let kv_s = self.element_to_string(|f| {
                    f.format_expr(key);
                    f.emitter.write(": ");
                    f.format_expr(value);
                });
                let vars_s = variables.iter().map(|v| v.node.as_str())
                    .collect::<Vec<_>>().join(", ");
                let iter_s = self.element_to_string(|f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string(|f| f.format_expr(c))
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
                let expr_s = self.element_to_string(|f| f.format_expr(expr));
                let iter_s = self.element_to_string(|f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string(|f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "{", &expr_s, &variable.node, "", &iter_s, cond_s.as_deref(), "}",
                );
                self.write_doc(&comp_doc);
            }
            Expr::ArrayLiteral(elems) => {
                // R39 fmt collection-literal interior-comment escape (Core
                // #4 chokepoint): if any un-emitted comment sits strictly
                // inside `[...]`, the flat `doc::surround` path would
                // silently drop it (sub-formatter has empty comment
                // sideband — see `element_to_string_at`) and the OUTER
                // trailing-hook would then dedent the comment to column
                // 0. Route through the shared broken-with-comments helper
                // so leading, trailing, and orphan-pre-close comments all
                // land at the correct interior indent.
                if self.has_interior_comments(expr.span.start, expr.span.end) {
                    self.format_bracketed_broken_with_comments(
                        "[", "]", expr.span.end,
                        elems,
                        |e| (e.span.start, e.span.end),
                        |f, e| f.format_expr(e),
                    );
                    return;
                }
                let items: Vec<doc::Doc> = elems.iter().map(|e| {
                    doc::text(self.element_to_string(|f| f.format_expr(e)))
                }).collect();
                let doc = doc::surround("[", items, "]", true);
                self.write_doc(&doc);
            }
            Expr::TupleLiteral(elems) => {
                if elems.len() == 1 {
                    // Single-element tuples always need trailing comma.
                    // R39 known gap (durable repro at
                    // `tests/fixtures/known_gaps/fmt_collection_literal_interior_tuple_single_elem.gg`):
                    // interior comments inside a single-element tuple
                    // still escape — the broken-with-comments helper is
                    // NOT dispatched here because a single-elem tuple
                    // stays flat by design. Rare shape; filed as follow-up.
                    self.emitter.write("(");
                    self.format_expr(&elems[0]);
                    self.emitter.write(",)");
                } else {
                    // R39 fmt collection-literal interior-comment escape
                    // (Core #4 chokepoint): see ArrayLiteral above.
                    if self.has_interior_comments(expr.span.start, expr.span.end) {
                        self.format_bracketed_broken_with_comments(
                            "(", ")", expr.span.end,
                            elems,
                            |e| (e.span.start, e.span.end),
                            |f, e| f.format_expr(e),
                        );
                        return;
                    }
                    let items: Vec<doc::Doc> = elems.iter().map(|e| {
                        doc::text(self.element_to_string(|f| f.format_expr(e)))
                    }).collect();
                    let doc = doc::surround("(", items, ")", true);
                    self.write_doc(&doc);
                }
            }
            Expr::DictLiteral(pairs) => {
                // R39 fmt collection-literal interior-comment escape
                // (Core #4 chokepoint): see ArrayLiteral above. `span_of`
                // returns `(key.span.start, value.span.end)` so the range
                // covers the WHOLE pair — a comment between key and value
                // (rare) OR between pairs both count as pair-interior.
                if self.has_interior_comments(expr.span.start, expr.span.end) {
                    self.format_bracketed_broken_with_comments(
                        "{", "}", expr.span.end,
                        pairs,
                        |pair| (pair.0.span.start, pair.1.span.end),
                        |f, pair| {
                            f.format_expr(&pair.0);
                            f.emitter.write(": ");
                            f.format_expr(&pair.1);
                        },
                    );
                    return;
                }
                let items: Vec<doc::Doc> = pairs.iter().map(|(k, v)| {
                    doc::text(self.element_to_string(|f| {
                        f.format_expr(k);
                        f.emitter.write(": ");
                        f.format_expr(v);
                    }))
                }).collect();
                let doc = doc::surround("{", items, "}", true);
                self.write_doc(&doc);
            }
            Expr::StructLiteral { name, generic_args, args } => {
                self.emitter.write(&name.node);
                if let Some(ga) = generic_args {
                    self.format_generic_args_wrapped(ga);
                }
                // R39 fmt collection-literal interior-comment escape
                // (Core #4 chokepoint): NARROW the interior-check to the
                // ARG TUPLE only — using `expr.span.start` as the interior
                // start would fire on a comment inside `generic_args`
                // (`Foo[T, # C](a)`) with the wrong container_end (arg-
                // tuple end, not generic-args end), miscoloring the
                // dispatch. Use the first arg's span.start as the interior
                // start so only comments inside the arg tuple qualify.
                //
                // R39 known gaps (durable repros in
                // `tests/fixtures/known_gaps/`):
                //   - `args.is_empty()` (e.g. `Foo()\n  # C\n)`): no first-
                //     arg span to derive the arg-tuple start; SKIP dispatch
                //     — file
                //     `fmt_collection_literal_interior_struct_no_args.gg`.
                //   - `generic_args`-interior comment: current scope covers
                //     only the arg tuple — file
                //     `fmt_collection_literal_interior_struct_generic_args.gg`.
                if !args.is_empty() {
                    let args_start = args.first().unwrap().span.start;
                    if self.has_interior_comments(args_start, expr.span.end) {
                        self.format_bracketed_broken_with_comments(
                            "(", ")", expr.span.end,
                            args,
                            |a| (a.span.start, a.span.end),
                            |f, a| f.format_expr(a),
                        );
                        return;
                    }
                }
                let items: Vec<doc::Doc> = args.iter().map(|a| {
                    doc::text(self.element_to_string(|f| f.format_expr(a)))
                }).collect();
                let doc = doc::surround("(", items, ")", true);
                self.write_doc(&doc);
            }
            Expr::As { expr, type_ } => {
                // FMT-A: `as` is bp 31/32 (left-assoc). Operand at LEFT position.
                self.format_binop_operand(expr, 31, BinOpPos::Left, false);
                self.emitter.write(" as ");
                self.format_type(type_);
            }
            Expr::Await { expr } => {
                // FMT-A: postfix `.await()` — receiver at bp 35.
                self.format_postfix_receiver(expr);
                self.emitter.write(".await()");
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
                    self.format_call_args_wrapped(args);
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
                self.format_binop_operand(expr, 1, BinOpPos::Left, false);
                if let Some((error_type, error_name)) = error_binding {
                    self.emitter.write(" rethrow (");
                    self.format_type(error_type);
                    self.emitter.write(" ");
                    self.emitter.write(&error_name.node);
                    self.emitter.write("):");
                    // The bound-form transform after `):` is parsed by
                    // parse_body_or_expr (parser::expr.rs:507) which reads
                    // an expression at low precedence — safe from further
                    // nesting hazards (like the Catch recovery arm).
                    // Snag #15b/#15c: use `format_arm_body` so multi-stmt
                    // transforms don't get wrapped in a spurious `do:`.
                    self.format_arm_body(transform);
                } else {
                    self.emitter.write(" rethrow ");
                    // Bare-form transform: nested Rethrow/Catch (bp 1) at
                    // this position would silently re-associate. WRAP.
                    self.format_binop_operand(transform, 1, BinOpPos::Right, false);
                }
            }
            Expr::Catch { expr, error_binding, recovery } => {
                // FMT-A: LHS at Catch bp 1 — same story as Rethrow LHS.
                self.format_binop_operand(expr, 1, BinOpPos::Left, false);
                self.emitter.write(" catch (");
                self.emitter.write(&error_binding.node);
                self.emitter.write("):");
                // Recovery parsed via parse_body_or_expr (parser::expr.rs:507)
                // at bp 0 — absorbs everything on its line, so no wrap
                // hazard for the recovery arm itself.
                // Snag #15b/#15c: use `format_arm_body` so multi-stmt
                // recovery bodies don't get wrapped in a spurious `do:`.
                self.format_arm_body(recovery);
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
        if let Some(ref name) = arg.name {
            self.emitter.write(&name.node);
            self.emitter.write(" = ");
        }
        self.format_ownership_prefix(arg.ownership);
        // R41 T-FMT-A: sibling of the for-iterable site — `parse_call_arg`
        // strips the sigil before parsing the argument expression.
        //
        // POSITIONAL args ONLY. `parse_call_arg` runs
        // `parse_ownership_modifier` BEFORE the `name =` lookahead, so on a
        // NAMED argument the stripped position is ahead of the NAME, not ahead
        // of the value: `f(&b = x)` is the spelling that yields
        // `CallArg.ownership`, while the value in `f(b = &x)` is parsed by
        // `parse_expr` with no pre-pass and therefore keeps its sigil
        // unaided. Guarding it too is pure churn (`b = &x` → `b = (&x)`,
        // measured live on `known_gaps/sound_named_arg_sigil_dropped.gg`).
        //
        // ⚠ NOT a statement that the named form is sound: a `&` after `=` is
        // silently dropped by the OWNERSHIP CHECK, which is the separate,
        // already-filed defect that fixture pins. This is only about which
        // position the FORMATTER must protect.
        if arg.name.is_some() {
            self.format_expr(&arg.value);
        } else {
            self.format_ownership_modifier_operand(&arg.value);
        }
    }

    fn format_closure_param(&mut self, param: &ClosureParam) {
        // Tuple destructuring: print `(T1 x, T2 y, ...)` from the source-level metadata
        // rather than the synthesised `(T1, T2) __dp_N` form.
        if let Some(ref bindings) = param.destructure {
            self.emitter.write("(");
            for (i, b) in bindings.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(", ");
                }
                self.format_type(&b.type_);
                self.emitter.write(" ");
                self.format_ownership_prefix(b.ownership);
                self.emitter.write(&b.name.node);
            }
            self.emitter.write(")");
            return;
        }
        // type-first: `[type] [&|!]name`
        if let Some(ref ty) = param.type_ {
            self.format_type(ty);
            self.emitter.write(" ");
        }
        self.format_ownership_prefix(param.ownership);
        self.emitter.write(&param.name.node);
    }

    // ── String formatting ───────────────────────────────────

    fn format_string_lit(&mut self, s: &StringLiteral) {
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

    fn format_string_escape(&mut self, text: &str, kind: StringKind) {
        if kind == StringKind::Raw {
            self.emitter.write(text);
            return;
        }
        for ch in text.chars() {
            match ch {
                '\n' => self.emitter.write("\\n"),
                '\t' => self.emitter.write("\\t"),
                '\r' => self.emitter.write("\\r"),
                '\\' => self.emitter.write("\\\\"),
                '"' => self.emitter.write("\\\""),
                '\0' => self.emitter.write("\\0"),
                '{' if kind == StringKind::Format => self.emitter.write("{{"),
                '}' if kind == StringKind::Format => self.emitter.write("}}"),
                // Control chars with no named escape above (C0 minus the
                // already-handled \0 \t \n \r, plus DEL) would otherwise write
                // a RAW control byte into the source (a data-loss defect, since
                // e.g. a raw 0x1B corrupts the file and cannot be read back as
                // the author intended). Emit the `\xHH` byte escape (lowercase
                // 2-hex, matching the lexer's `\x` arm which accepts <=0x7F in
                // string context), which re-lexes to the same scalar. Printable
                // Unicode (>= 0x80: accented letters, emoji, …) is left as raw
                // UTF-8 — it round-trips fine and escaping it would be noise.
                c if (c.is_control() && (c as u32) <= 0x7F) => {
                    self.emitter.write(&format!("\\x{:02x}", c as u32));
                }
                c => {
                    let mut buf = [0u8; 4];
                    self.emitter.write(c.encode_utf8(&mut buf));
                }
            }
        }
    }

}

// ══════════════════════════════════════════════════════════════
// Helper functions
// ══════════════════════════════════════════════════════════════

/// Parse an integer-literal lexeme back to its `i64` value using the SAME
/// rules the lexer applies in `Lexer::parse_int_literal`
/// (`src/lexer/mod.rs`): an optional leading sign, a case-insensitive
/// `0x`/`0o`/`0b` radix prefix (else decimal), and `_` digit separators
/// stripped before `i64::from_str_radix`. Returns `Some(value)` on a clean
/// parse, `None` otherwise.
///
/// The formatter uses this to decide whether a literal's ORIGINAL source
/// lexeme round-trips to the AST value (`int_literal_text`); mirroring the
/// lexer exactly means "the lexeme parses back to `n`" is equivalent to
/// "the lexer produced `n` from this lexeme", so overflow/edge behaviour
/// (e.g. `0x7FFF…` at the i64 ceiling) matches by construction.
///
/// The leading sign is accepted defensively: the `IntLiteral` operand span
/// for `-0x10` covers only `0x10` (the unary `-` is a separate AST node),
/// but even if a signed slice arrived, the caller's `== n` equality check
/// guards against ever emitting a lexeme that denotes a different value.
fn parse_int_lexeme(lexeme: &str) -> Option<i64> {
    let s = lexeme.trim();
    let (neg, rest) = match s.as_bytes().first()? {
        b'-' => (true, &s[1..]),
        b'+' => (false, &s[1..]),
        _ => (false, s),
    };
    let (radix, digits) = if let Some(h) =
        rest.strip_prefix("0x").or_else(|| rest.strip_prefix("0X"))
    {
        (16u32, h)
    } else if let Some(o) = rest.strip_prefix("0o").or_else(|| rest.strip_prefix("0O")) {
        (8, o)
    } else if let Some(b) = rest.strip_prefix("0b").or_else(|| rest.strip_prefix("0B")) {
        (2, b)
    } else {
        (10, rest)
    };
    let clean: String = digits.chars().filter(|c| *c != '_').collect();
    if clean.is_empty() {
        return None;
    }
    // `from_str_radix` rejects any non-radix-digit char (including a stray
    // interior sign), so a byte literal `b'A'` or a char slice fails here.
    let mag = i64::from_str_radix(&clean, radix).ok()?;
    if neg {
        mag.checked_neg()
    } else {
        Some(mag)
    }
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
        // Await is ALWAYS rendered postfix as `.await()` by
        // `format_expr` (Round XXXVI Expr::Await arm). The parser
        // accepts prefix `await x` too, but they both AST-shape as
        // `Expr::Await` and the formatter picks the postfix rendering,
        // so the effective outer bp for embedding checks is 35
        // (postfix bp), NOT the prefix parser bp of 2.
        Expr::Await { .. } => None,
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
        | Expr::Await { expr: recv } => {
            through(recv, needs_parens_as_postfix_receiver(&recv.node))
        }

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
        | Expr::ArrayLiteral(_)
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
