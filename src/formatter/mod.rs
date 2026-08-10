pub mod doc;

use std::rc::Rc;

use crate::lexer::token::{StringKind, StringLiteral, StringSegment};
use crate::parser::ast::*;
use crate::span::Spanned;

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
// Formatter — walks AST and emits formatted source
// ══════════════════════════════════════════════════════════════

pub struct Formatter {
    emitter: Emitter,
    comments: Vec<Spanned<String>>,
    comment_cursor: usize,
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
        self.emit_trailing_comment_after(self.source.len());
        self.emit_remaining_comments();
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
        // interleave (the outer Formatter owns the sideband + cursor),
        // and no source-lookup is possible since we don't have any
        // spans of interest here. Pass an empty `Rc<str>` — the trailing
        // comment helpers below will simply find no matching comment
        // ranges because `comments` is empty.
        let mut fmt = Formatter::new(vec![], Rc::from(""));
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

    fn emit_comments_before(&mut self, pos: usize) {
        while self.comment_cursor < self.comments.len() {
            let c = &self.comments[self.comment_cursor];
            if c.span.start < pos {
                self.emitter.write(&c.node);
                self.emitter.newline();
                self.comment_cursor += 1;
            } else {
                break;
            }
        }
    }

    fn emit_remaining_comments(&mut self) {
        while self.comment_cursor < self.comments.len() {
            self.emitter.write(&self.comments[self.comment_cursor].node);
            self.emitter.newline();
            self.comment_cursor += 1;
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
    /// used by `element_to_string` is passed empty source + no comment
    /// sideband, so any comment interior to a collection literal is
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
            self.emit_trailing_comment_after(elem_end);
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
        // while the base helper stays unchanged.
        self.emit_trailing_comment_after(header_anchor_end);
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
    fn emit_trailing_comment_after(&mut self, prev_end: usize) {
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
            let anchor = self.last_real_content_before(prev_end);
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
            let mut inlined = String::with_capacity(c.node.len() + 2);
            inlined.push_str("  ");
            inlined.push_str(&c.node);
            self.emitter.inject_before_newline(&inlined);
            self.comment_cursor += 1;
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
    fn last_real_content_before(&self, pos: usize) -> usize {
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
            return i;
        }
        0
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
            self.emit_trailing_comment_after(item.span.end);
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
            self.emit_trailing_comment_after(item.span.end);
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
            self.emit_trailing_comment_after(item.span.end);
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
                    self.emit_trailing_comment_after(item.span.end);
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
                        self.emit_trailing_comment_after(item.span.end);
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
                        self.emit_trailing_comment_after(item.span.end);
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
        self.format_qualifiers(&f.qualifiers);
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
                self.emit_trailing_comment_after(field.span.end);
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
                self.emit_trailing_comment_after(variant.span.end);
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
                self.emit_trailing_comment_after(item.span.end);
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
                self.emit_trailing_comment_after(method.span.end);
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
        self.emitter.write("extern");
        if let Some(ref abi) = eb.abi {
            self.emitter.write(" \"");
            self.emitter.write(&abi.node);
            self.emitter.write("\"");
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for func in &eb.items {
            self.format_function(&func.node);
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
        // self parameter (same in both modes)
        if matches!(param.type_.node, Type::SelfType) {
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
            self.emit_trailing_comment_after(stmt.span.end);
        }
    }

    /// gorget-arena snag #3 (R39, 2026-08-09): true iff the author wrote
    /// at least one blank line before `cur_start`. Walks BACKWARDS from
    /// `cur_start` to find the last non-whitespace byte (or 0), then
    /// counts `\n`s in that trailing-whitespace region. ≥ 2 `\n` = the
    /// user wrote at least one wholly-empty line (one `\n` is the natural
    /// EOL of prev's content line; the second `\n` starts the blank).
    ///
    /// **Why backward walk (not `source[prev_end..cur_start]`):** for
    /// container-type items (`struct`/`enum`/`trait`/`equip`/`function`),
    /// the AST `span.end` sits at the DEDENT token — which is often
    /// ZERO WIDTH at the same byte position as the NEXT item's start.
    /// `source[prev.span.end..cur.span.start]` is then empty even when
    /// the author wrote blank lines in between. Walking back from
    /// `cur_start` past all trailing whitespace catches the blanks
    /// regardless of where the AST considers prev to end.
    ///
    /// The `prev_end` param is kept for compatibility with callers that
    /// want an explicit floor, but the primary signal is the walk-back
    /// from `cur_start`.
    fn has_blank_line_between(&self, _prev_end: usize, cur_start: usize) -> bool {
        if cur_start == 0 || cur_start > self.source.len() {
            return false;
        }
        let bytes = self.source.as_bytes();
        // Walk lines backward from cur_start looking for a fully-blank line
        // (a line whose bytes are all whitespace) between prev's content
        // and cur's content. Standalone-comment lines (`<ws>*#…\n`) are
        // TRANSPARENT to this walk: they attach to cur, so a blank the
        // user wrote BEFORE such a comment still expresses paragraphing.
        // Without this, `stmt\n\n    # comment\n    stmt` collapses to
        // `stmt\n# comment\nstmt` because the walk stops at `#`, missing
        // the blank above it (fmt_idempotent regression, R39 close).
        //
        // `_prev_end` is intentionally unused: container-type items
        // (struct/enum/fn/etc.) have zero-width AST span.end at the same
        // position as the next item's start, so using it as a floor
        // would defeat the walk. The whitespace walk-back reaches the
        // last actual content byte from prev regardless.
        let mut i = cur_start;
        // Skip cur's own line (from its last `\n` to cur_start).
        while i > 0 && bytes[i - 1] != b'\n' {
            i -= 1;
        }
        while i > 0 {
            i -= 1; // step past the `\n` that ended the line above
            let line_end = i;
            while i > 0 && bytes[i - 1] != b'\n' {
                i -= 1;
            }
            let line = &bytes[i..line_end];
            match line.iter().position(|b| !b.is_ascii_whitespace()) {
                None => return true, // fully-blank line = user paragraph break
                Some(pos) if line[pos] == b'#' => continue, // comment, keep walking
                _ => return false, // content line — no blank between prev and cur
            }
        }
        false
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
        self.emit_trailing_comment_after(stmt.span.end);
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
                self.format_expr(iterable);
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

    fn format_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::IntLiteral(n) => {
                self.emitter.write(&n.to_string());
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
                                self.emit_trailing_comment_after(stmt.span.end);
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
        self.format_expr(&arg.value);
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
