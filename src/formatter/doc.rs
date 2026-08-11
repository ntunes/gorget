// ══════════════════════════════════════════════════════════════
// Document IR — Wadler-Lindig pretty printer for gg fmt
// ══════════════════════════════════════════════════════════════
//
// A lightweight document algebra that enables "fit on one line or break"
// decisions. The formatter builds a Doc tree from AST, then the renderer
// resolves groups against a max line width.

/// Maximum line width for the formatter (owner-ratified FMT CANON PAIR).
pub const MAX_WIDTH: usize = 120;

/// Indentation unit: 4 spaces.
pub const INDENT_WIDTH: usize = 4;

/// A document node in the formatting intermediate representation.
///
/// The key insight: `Group` tries to render its contents flat (all on one line).
/// If the flat rendering exceeds the remaining width, it switches to broken mode
/// where `Line` nodes become actual newlines with indentation.
#[derive(Debug, Clone)]
pub enum Doc {
    /// Literal text. It MAY contain newlines: the hybrid formatter feeds
    /// pre-rendered sub-elements in here via `Formatter::element_to_string`,
    /// and a sub-element that had to wrap arrives as a multi-line string with
    /// its indentation already baked in at the caller's continuation level.
    /// Such text cannot be flattened, so `measure_flat` reports `None` for it
    /// (same treatment as `HardLine`) and the renderer recomputes the column
    /// from the text's LAST line.
    Text(String),

    /// In flat mode: a single space. In broken mode: newline + indentation.
    Line,

    /// In flat mode: nothing. In broken mode: newline + indentation.
    /// Useful for positions where a space is unwanted when inline
    /// (e.g., after opening paren, before closing paren).
    SoftLine,

    /// Always a newline + indentation, regardless of mode.
    /// Forces the enclosing group to break.
    HardLine,

    /// Increase indentation by one level for the contents.
    Indent(Box<Doc>),

    /// A sequence of documents rendered in order.
    Concat(Vec<Doc>),

    /// Try to render contents flat (on one line). If it doesn't fit
    /// within the remaining line width, render in broken mode.
    Group(Box<Doc>),

    /// Emit different content depending on whether the enclosing group
    /// is rendering flat or broken. Used for trailing commas:
    /// `IfBreak { flat: empty, broken: "," }`.
    IfBreak {
        flat: Box<Doc>,
        broken: Box<Doc>,
    },

    /// Greedy **fill packing** of a comma-separated list — the canonical
    /// layout for every horizontally-broken list (`surround_fill`).
    ///
    /// Rendering starts at the CURRENT column (right after the caller's open
    /// delimiter — there is no leading break) and places as many items on the
    /// line as fit. When the next item would not fit, the separating comma is
    /// written, the line breaks to the **block continuation indent**
    /// (`indent_level + 1`, i.e. `(outer + 1) * 4` spaces — the very column
    /// `Formatter::element_to_string` pre-renders sub-elements for), and
    /// packing resumes. At least one item is always placed per line, so the
    /// only line that may exceed the budget is one holding a single item that
    /// does not fit alone at the continuation indent.
    ///
    /// `close` is owned by the node rather than being a sibling `Text` because
    /// the fit test for the LAST item must include the closing delimiter's
    /// width (otherwise the final line can overrun by `close.len()`).
    /// There is **no trailing comma** in a fill-broken list; the
    /// one-item-per-line-with-comma shape lives in
    /// `Formatter::format_bracketed_broken_with_comments` (comment-bearing
    /// lists) and in `surround`.
    ///
    /// In `Mode::Flat` (an enclosing group chose flat) the items render on one
    /// line separated by `", "`, with no breaks at all.
    Fill {
        items: Vec<Doc>,
        close: String,
    },
}

// ── Builder helpers ──────────────────────────────────────────

/// Create a Text node. Returns Empty for empty strings.
pub fn text(s: impl Into<String>) -> Doc {
    let s = s.into();
    if s.is_empty() {
        return Doc::Concat(vec![]);
    }
    Doc::Text(s)
}

/// Space if flat, newline+indent if broken.
pub fn line() -> Doc {
    Doc::Line
}

/// Nothing if flat, newline+indent if broken.
pub fn softline() -> Doc {
    Doc::SoftLine
}

/// Always a newline. Forces enclosing group to break.
pub fn hardline() -> Doc {
    Doc::HardLine
}

/// Increase indent for contents.
pub fn indent(doc: Doc) -> Doc {
    Doc::Indent(Box::new(doc))
}

/// Sequence of documents.
pub fn concat(docs: Vec<Doc>) -> Doc {
    // Flatten nested Concats for cleaner trees
    let mut flat = Vec::with_capacity(docs.len());
    for d in docs {
        match d {
            Doc::Concat(inner) => flat.extend(inner),
            other => flat.push(other),
        }
    }
    if flat.len() == 1 {
        return flat.into_iter().next().unwrap();
    }
    Doc::Concat(flat)
}

/// Try to render contents on one line; break if it doesn't fit.
pub fn group(doc: Doc) -> Doc {
    Doc::Group(Box::new(doc))
}

/// Emit `flat` when the enclosing group renders flat,
/// `broken` when it renders broken.
pub fn if_break(flat: Doc, broken: Doc) -> Doc {
    Doc::IfBreak {
        flat: Box::new(flat),
        broken: Box::new(broken),
    }
}

/// Join documents with a separator between each pair.
pub fn join(docs: Vec<Doc>, sep: Doc) -> Doc {
    let mut result = Vec::with_capacity(docs.len() * 2);
    for (i, doc) in docs.into_iter().enumerate() {
        if i > 0 {
            result.push(sep.clone());
        }
        result.push(doc);
    }
    concat(result)
}

/// Surround a list of items with open/close delimiters, **one item per line
/// when broken**.
///
/// This is NOT the canonical layout for Gorget lists — `surround_fill` is (it
/// packs greedily; see its doc comment and `Doc::Fill`). `surround` survives as
/// the exploded shape: it is what a comment-bearing list needs, it is the
/// algebra's reference `Group`/`Line`/`IfBreak` composition, and the
/// `formatter_list_emit_fill_census` lint pins the number of production call
/// sites so that picking it over `surround_fill` is always a deliberate act.
///
/// When the group fits on one line: `open item1, item2 close`
/// When broken:
/// ```text
/// open
///     item1,
///     item2,
/// close
/// ```
///
/// The `trailing_comma` parameter controls whether a trailing comma
/// is added when the group breaks.
pub fn surround(open: &str, items: Vec<Doc>, close: &str, trailing_comma: bool) -> Doc {
    if items.is_empty() {
        return concat(vec![text(open), text(close)]);
    }

    let trailing = if trailing_comma {
        if_break(text(""), text(","))
    } else {
        text("")
    };

    // Build: sep item1 "," sep item2 "," sep ... itemN trailing_comma
    let mut inner = Vec::with_capacity(items.len() * 3);
    for (i, item) in items.into_iter().enumerate() {
        if i > 0 {
            inner.push(text(","));
            inner.push(line());
        }
        inner.push(item);
    }
    inner.push(trailing);

    group(concat(vec![
        text(open),
        indent(concat(vec![softline(), concat(inner)])),
        softline(),
        text(close),
    ]))
}

/// Surround a list of items with open/close delimiters, **greedily packed**
/// (fill) when it does not fit on one line. This is the canonical Gorget list
/// layout (owner-ratified: "fill long lines before breaking to the next", with
/// a BLOCK-INDENT continuation).
///
/// Fits: `open item1, item2 close` — identical to `surround`.
/// Does not fit:
/// ```text
/// open item1, item2, item3, item4,
///     item5, item6 close
/// ```
///
/// Note what this deliberately does NOT emit: no leading break after `open`
/// (packing starts at the current column), no trailing comma, and no line of
/// its own for `close` (it follows the last item inline).
///
/// The empty list short-circuits to `openclose` — no `Fill` node is built, so
/// the packer never sees a zero-item list.
pub fn surround_fill(open: &str, items: Vec<Doc>, close: &str) -> Doc {
    if items.is_empty() {
        return concat(vec![text(open), text(close)]);
    }
    concat(vec![
        text(open),
        Doc::Fill {
            items,
            close: close.to_string(),
        },
    ])
}

// ── Renderer ─────────────────────────────────────────────────

/// Render a Doc tree to a string, breaking groups that exceed `max_width`.
pub fn render(doc: &Doc, max_width: usize) -> String {
    let mut renderer = Renderer {
        out: String::new(),
        col: 0,
        max_width,
    };
    renderer.render_doc(doc, 0, Mode::Break);
    renderer.out
}

/// Render a Doc tree starting at a given column and base indentation level.
/// Used by the hybrid formatter to splice Doc output into the Emitter's buffer.
pub fn render_at(doc: &Doc, max_width: usize, start_col: usize, base_indent: usize) -> String {
    let mut renderer = Renderer {
        out: String::new(),
        col: start_col,
        max_width,
    };
    renderer.render_doc(doc, base_indent, Mode::Break);
    renderer.out
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// Render everything on one line (inside a group that fits).
    Flat,
    /// Render with line breaks at `Line`/`SoftLine` positions.
    Break,
}

struct Renderer {
    out: String,
    col: usize,
    max_width: usize,
}

impl Renderer {
    fn render_doc(&mut self, doc: &Doc, indent_level: usize, mode: Mode) {
        match doc {
            Doc::Text(s) => {
                self.out.push_str(s);
                self.col = advance_col(self.col, s);
            }

            Doc::Line => match mode {
                Mode::Flat => {
                    self.out.push(' ');
                    self.col += 1;
                }
                Mode::Break => {
                    self.emit_newline(indent_level);
                }
            },

            Doc::SoftLine => match mode {
                Mode::Flat => {
                    // nothing
                }
                Mode::Break => {
                    self.emit_newline(indent_level);
                }
            },

            Doc::HardLine => {
                self.emit_newline(indent_level);
            }

            Doc::Indent(inner) => {
                self.render_doc(inner, indent_level + 1, mode);
            }

            Doc::Concat(docs) => {
                for d in docs {
                    self.render_doc(d, indent_level, mode);
                }
            }

            Doc::Group(inner) => {
                // Measure flat width. If it fits, render flat; otherwise break.
                let flat_width = measure_flat(inner);
                if let Some(w) = flat_width {
                    if self.col + w <= self.max_width {
                        self.render_doc(inner, indent_level, Mode::Flat);
                        return;
                    }
                }
                // Doesn't fit or contains HardLine → break
                self.render_doc(inner, indent_level, Mode::Break);
            }

            Doc::IfBreak { flat, broken } => match mode {
                Mode::Flat => self.render_doc(flat, indent_level, mode),
                Mode::Break => self.render_doc(broken, indent_level, mode),
            },

            Doc::Fill { items, close } => self.render_fill(items, close, indent_level, mode),
        }
    }

    /// Greedy fill packing (see `Doc::Fill`). `indent_level` is the OUTER
    /// level; continuation lines land one level deeper.
    fn render_fill(&mut self, items: &[Doc], close: &str, indent_level: usize, mode: Mode) {
        let close_width = close.chars().count();

        // An enclosing group already decided everything fits on one line —
        // honour it and emit the flat spelling, breaks suppressed.
        if mode == Mode::Flat {
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    self.out.push_str(", ");
                    self.col += 2;
                }
                self.render_doc(item, indent_level, Mode::Flat);
            }
            self.out.push_str(close);
            self.col += close_width;
            return;
        }

        let cont_indent = indent_level + 1;
        let last = items.len().saturating_sub(1);
        for (i, item) in items.iter().enumerate() {
            // Width this item claims on the line it lands on: the separating
            // ", " when it follows another item on the same line, the item
            // itself, and its terminator — a "," for every item but the last,
            // the closing delimiter for the last one (a fill-broken list has
            // no trailing comma).
            let lead = if i == 0 { 0 } else { 2 };
            let tail = if i == last { close_width } else { 1 };
            // A multi-line (or HardLine-bearing) item measures `None`: it can
            // never share a line, so it always takes the break branch. That
            // puts it at exactly the continuation indent — the column
            // `element_to_string` pre-rendered it for.
            let fits = measure_flat(item)
                .is_some_and(|w| self.col + lead + w + tail <= self.max_width);

            if i > 0 {
                self.out.push(',');
                self.col += 1;
            }
            if fits {
                if i > 0 {
                    self.out.push(' ');
                    self.col += 1;
                }
            } else {
                // Always at least one item per line: after this break the item
                // is alone, so a break here can never loop.
                self.emit_newline(cont_indent);
            }
            // Measured flat and it fit → render flat. Otherwise let any nested
            // group inside the item make its own decision at the new column.
            self.render_doc(
                item,
                cont_indent,
                if fits { Mode::Flat } else { Mode::Break },
            );
        }
        self.out.push_str(close);
        self.col += close_width;
    }

    fn emit_newline(&mut self, indent_level: usize) {
        self.out.push('\n');
        let spaces = indent_level * INDENT_WIDTH;
        for _ in 0..spaces {
            self.out.push(' ');
        }
        self.col = spaces;
    }
}

/// Advance a column cursor over `s`, which MAY be multi-line. Columns are
/// counted in CHARACTERS, not bytes — a multi-byte character occupies one
/// column, and measuring it as its UTF-8 length inflates every width decision
/// downstream of a non-ASCII literal. After a newline the column restarts from
/// the text's own baked-in indentation.
fn advance_col(col: usize, s: &str) -> usize {
    match s.rfind('\n') {
        Some(nl) => s[nl + 1..].chars().count(),
        None => col + s.chars().count(),
    }
}

/// Measure the flat (single-line) width of a Doc, in characters.
/// Returns `None` when the doc cannot be flattened — it contains a `HardLine`,
/// or a `Text` that already carries newlines (a pre-rendered sub-element that
/// had to wrap). `None` propagates: one unflattenable child makes the whole
/// subtree unflattenable.
fn measure_flat(doc: &Doc) -> Option<usize> {
    match doc {
        Doc::Text(s) => {
            if s.contains('\n') {
                None // pre-rendered multi-line element — cannot flatten
            } else {
                Some(s.chars().count())
            }
        }
        Doc::Line => Some(1),       // space in flat mode
        Doc::SoftLine => Some(0),   // nothing in flat mode
        Doc::HardLine => None,      // cannot flatten
        Doc::Indent(inner) => measure_flat(inner),
        Doc::Concat(docs) => {
            let mut total = 0;
            for d in docs {
                total += measure_flat(d)?;
            }
            Some(total)
        }
        Doc::Group(inner) => measure_flat(inner),
        Doc::IfBreak { flat, .. } => measure_flat(flat),
        // Flat spelling of a fill is `item, item, item` + close: every item,
        // plus a two-character `", "` separator between each adjacent pair.
        // `saturating_sub` keeps the empty list at 0 separators (the builder
        // never makes one, but the formula must not underflow if it ever does).
        Doc::Fill { items, close } => {
            let mut total = 2 * items.len().saturating_sub(1) + close.chars().count();
            for item in items {
                total += measure_flat(item)?;
            }
            Some(total)
        }
    }
}

// ══════════════════════════════════════════════════════════════
// Tests
// ══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_text_only() {
        let doc = text("hello");
        assert_eq!(render(&doc, 80), "hello");
    }

    #[test]
    fn test_group_fits_flat() {
        // "hello world" is 11 chars, fits in 80 cols
        let doc = group(concat(vec![text("hello"), line(), text("world")]));
        assert_eq!(render(&doc, 80), "hello world");
    }

    #[test]
    fn test_group_breaks_when_too_wide() {
        // "hello world" is 11 chars, doesn't fit in 5 cols
        let doc = group(concat(vec![text("hello"), line(), text("world")]));
        assert_eq!(render(&doc, 5), "hello\nworld");
    }

    #[test]
    fn test_group_with_indent() {
        let doc = group(concat(vec![
            text("("),
            indent(concat(vec![softline(), text("a"), text(","), line(), text("b")])),
            softline(),
            text(")"),
        ]));
        // Fits: "(a, b)"
        assert_eq!(render(&doc, 80), "(a, b)");
        // Doesn't fit:
        // (
        //     a,
        //     b
        // )
        assert_eq!(render(&doc, 5), "(\n    a,\n    b\n)");
    }

    #[test]
    fn test_hardline_forces_break() {
        let doc = group(concat(vec![text("a"), hardline(), text("b")]));
        // Even at wide width, HardLine forces break
        assert_eq!(render(&doc, 80), "a\nb");
    }

    #[test]
    fn test_if_break_flat() {
        let doc = group(concat(vec![
            text("a"),
            if_break(text(""), text(",")),
        ]));
        // Fits on one line → flat → no comma
        assert_eq!(render(&doc, 80), "a");
    }

    #[test]
    fn test_if_break_broken() {
        let doc = group(concat(vec![
            text("a"),
            hardline(),
            text("b"),
            if_break(text(""), text(",")),
        ]));
        // HardLine forces break → broken → comma
        assert_eq!(render(&doc, 80), "a\nb,");
    }

    #[test]
    fn test_nested_groups_break_independently() {
        // Outer group: "f(" inner_group ")"
        // Inner group: "a, b, c"
        let inner = group(concat(vec![
            text("a"),
            text(","),
            line(),
            text("b"),
            text(","),
            line(),
            text("c"),
        ]));
        let outer = group(concat(vec![text("f("), inner, text(")")]));

        // Wide enough for everything: "f(a, b, c)"
        assert_eq!(render(&outer, 80), "f(a, b, c)");

        // Too narrow for inner but outer structure fits:
        // Inner breaks but outer stays intact
        assert_eq!(render(&outer, 8), "f(a,\nb,\nc)");
    }

    #[test]
    fn test_surround_fits() {
        let items = vec![text("a"), text("b"), text("c")];
        let doc = surround("(", items, ")", true);
        // Fits: "(a, b, c)"
        assert_eq!(render(&doc, 80), "(a, b, c)");
    }

    #[test]
    fn test_surround_breaks() {
        let items = vec![text("alpha"), text("beta"), text("gamma")];
        let doc = surround("(", items, ")", true);
        // Doesn't fit in 10 cols:
        // (
        //     alpha,
        //     beta,
        //     gamma,
        // )
        assert_eq!(
            render(&doc, 10),
            "(\n    alpha,\n    beta,\n    gamma,\n)"
        );
    }

    #[test]
    fn test_surround_empty() {
        let doc = surround("(", vec![], ")", true);
        assert_eq!(render(&doc, 80), "()");
    }

    #[test]
    fn test_surround_no_trailing_comma() {
        let items = vec![text("alpha"), text("beta"), text("gamma")];
        let doc = surround("(", items, ")", false);
        // Broken without trailing comma
        assert_eq!(
            render(&doc, 10),
            "(\n    alpha,\n    beta,\n    gamma\n)"
        );
    }

    #[test]
    fn test_softline_flat_is_empty() {
        let doc = group(concat(vec![text("("), softline(), text("x"), softline(), text(")")]));
        // Fits: "(x)" — softline produces nothing
        assert_eq!(render(&doc, 80), "(x)");
    }

    #[test]
    fn test_softline_broken_is_newline() {
        let doc = group(concat(vec![
            text("("),
            softline(),
            text("very_long_name"),
            softline(),
            text(")"),
        ]));
        // Doesn't fit in 10:
        // (
        // very_long_name
        // )
        assert_eq!(render(&doc, 10), "(\nvery_long_name\n)");
    }

    #[test]
    fn test_join() {
        let items = vec![text("a"), text("b"), text("c")];
        let doc = join(items, text(", "));
        assert_eq!(render(&doc, 80), "a, b, c");
    }

    #[test]
    fn test_measure_flat_hardline_returns_none() {
        let doc = concat(vec![text("a"), hardline(), text("b")]);
        assert_eq!(measure_flat(&doc), None);
    }

    #[test]
    fn test_measure_flat_normal() {
        let doc = concat(vec![text("hello"), line(), text("world")]);
        // "hello" (5) + line→space (1) + "world" (5) = 11
        assert_eq!(measure_flat(&doc), Some(11));
    }

    #[test]
    fn test_deep_indent() {
        let doc = indent(indent(concat(vec![hardline(), text("deep")])));
        // Two indent levels = 8 spaces
        // Start in break mode since it's outside a group
        assert_eq!(render(&doc, 80), "\n        deep");
    }

    #[test]
    fn test_concat_flattening() {
        // Nested concats should flatten
        let doc = concat(vec![
            concat(vec![text("a"), text("b")]),
            concat(vec![text("c"), text("d")]),
        ]);
        assert_eq!(render(&doc, 80), "abcd");
    }

    #[test]
    fn test_realistic_function_signature() {
        // Simulates: int process(int value, str name, bool verbose):
        let params = vec![
            concat(vec![text("int"), text(" "), text("value")]),
            concat(vec![text("str"), text(" "), text("name")]),
            concat(vec![text("bool"), text(" "), text("verbose")]),
        ];
        let sig = concat(vec![
            text("int process"),
            surround("(", params, ")", true),
            text(":"),
        ]);

        // Fits on one line
        assert_eq!(
            render(&sig, 100),
            "int process(int value, str name, bool verbose):"
        );

        // Doesn't fit in 30 cols
        assert_eq!(
            render(&sig, 30),
            "int process(\n    int value,\n    str name,\n    bool verbose,\n):"
        );
    }

    // ── Fill (R41 T-FMT-D) ────────────────────────────────────
    //
    // Algebra-level cells the `.gg` fixture matrix cannot reach: every
    // `surround_fill` item built by `src/formatter/mod.rs` is a `doc::text`
    // leaf, so a genuine `HardLine` item, a `Mode::Flat` fill and a zero-item
    // fill only exist here.

    #[test]
    fn test_fill_fits_flat() {
        let items = vec![text("alpha"), text("beta"), text("gamma")];
        let doc = surround_fill("(", items, ")");
        assert_eq!(render(&doc, 80), "(alpha, beta, gamma)");
    }

    #[test]
    fn test_fill_packs_greedily_at_block_indent() {
        // "(alpha, beta" ends at column 12; `gamma` then needs
        // 12 + 2(", ") + 5 + 1(")") = 20, which does not fit in 19 — so it
        // breaks to the continuation indent (level 0 + 1 = 4 spaces).
        // No trailing comma, and `)` follows the last item inline.
        let items = vec![text("alpha"), text("beta"), text("gamma")];
        let doc = surround_fill("(", items, ")");
        assert_eq!(render(&doc, 19), "(alpha, beta,\n    gamma)");
    }

    #[test]
    fn test_fill_last_item_measures_the_close() {
        // Exactly wide enough for `(alpha, beta)` = 13 → one line.
        let items = vec![text("alpha"), text("beta")];
        assert_eq!(render(&surround_fill("(", items, ")"), 13), "(alpha, beta)");
        // One column narrower and the LAST item must break — the fit test for
        // the last item includes `)`. Without that term this stayed flat at 13.
        let items = vec![text("alpha"), text("beta")];
        assert_eq!(
            render(&surround_fill("(", items, ")"), 12),
            "(alpha,\n    beta)"
        );
    }

    #[test]
    fn test_fill_single_over_width_item_breaks_and_overflows_alone() {
        let items = vec![text("aaaaaaaaaaaaaaaaaaaaaaaaa")];
        assert_eq!(
            render(&surround_fill("(", items, ")"), 10),
            "(\n    aaaaaaaaaaaaaaaaaaaaaaaaa)"
        );
    }

    #[test]
    fn test_fill_always_places_at_least_one_item_per_line() {
        // Every item is wider than the budget: each lands alone on its own
        // continuation line rather than looping on an impossible break.
        let items = vec![text("wwwwwwwwww"), text("xxxxxxxxxx"), text("yyyyyyyyyy")];
        assert_eq!(
            render(&surround_fill("[", items, "]"), 8),
            "[\n    wwwwwwwwww,\n    xxxxxxxxxx,\n    yyyyyyyyyy]"
        );
    }

    #[test]
    fn test_fill_hardline_element_breaks_and_packing_resumes() {
        // A HardLine-bearing item can never share a line: the packer breaks
        // before it, then resumes from the column its last line ended at.
        let items = vec![
            text("a"),
            concat(vec![text("m1"), hardline(), text("m2")]),
            text("b"),
        ];
        assert_eq!(
            render(&surround_fill("(", items, ")"), 80),
            "(a,\n    m1\n    m2, b)"
        );
    }

    #[test]
    fn test_fill_multiline_text_element_breaks_and_packing_resumes() {
        // Same behaviour for the shape the real formatter produces: a
        // pre-rendered sub-element that arrives as multi-line TEXT.
        let items = vec![text("a"), text("m1\n    m2"), text("b")];
        assert_eq!(
            render(&surround_fill("(", items, ")"), 80),
            "(a,\n    m1\n    m2, b)"
        );
    }

    #[test]
    fn test_fill_empty_is_just_the_delimiters() {
        assert_eq!(render(&surround_fill("(", vec![], ")"), 80), "()");
        assert_eq!(render(&surround_fill("[", vec![], "]"), 80), "[]");
        assert_eq!(render(&surround_fill("{", vec![], "}"), 80), "{}");
    }

    #[test]
    fn test_measure_flat_fill_counts_separators_and_close() {
        // "alpha, beta, gamma)" = 5+4+5 items + 2*2 separators + 1 close = 19.
        let doc = Doc::Fill {
            items: vec![text("alpha"), text("beta"), text("gamma")],
            close: ")".to_string(),
        };
        assert_eq!(measure_flat(&doc), Some(5 + 4 + 5 + 4 + 1));
    }

    #[test]
    fn test_measure_flat_fill_propagates_none() {
        // One unflattenable item makes the whole fill unflattenable — without
        // this an enclosing Group would pick Flat over an overflowing list.
        let doc = Doc::Fill {
            items: vec![text("a"), concat(vec![text("x"), hardline(), text("y")])],
            close: ")".to_string(),
        };
        assert_eq!(measure_flat(&doc), None);
        let doc = Doc::Fill {
            items: vec![text("a"), text("multi\nline")],
            close: ")".to_string(),
        };
        assert_eq!(measure_flat(&doc), None);
    }

    #[test]
    fn test_measure_flat_multiline_text_is_none() {
        assert_eq!(measure_flat(&text("no newlines")), Some(11));
        assert_eq!(measure_flat(&text("has\nnewline")), None);
    }

    #[test]
    fn test_measure_flat_counts_characters_not_bytes() {
        // "naïve café" is 10 characters but 12 UTF-8 bytes.
        assert_eq!(measure_flat(&text("naïve café")), Some(10));
    }

    #[test]
    fn test_fill_measures_elements_in_characters_not_bytes() {
        // Each item is 10 chars / 12 bytes. Budget 24 fits `(x, y)` at
        // 1+10+2+10+1 = 24 in CHARACTERS; measured in bytes it would be 28 and
        // the list would break spuriously.
        let items = vec![text("naïve café"), text("naïve café")];
        assert_eq!(
            render(&surround_fill("(", items, ")"), 24),
            "(naïve café, naïve café)"
        );
    }

    #[test]
    fn test_fill_resumes_from_a_multiline_element_in_characters() {
        // The column after a multi-line element is its LAST line's width, in
        // characters. That last line is 12 chars / 14 bytes, so the trailing
        // `b` needs 12 + 2 + 1 + 1 = 16 columns and fits a budget of 16 —
        // whereas a BYTE count reads the column as 14, computes 18, and breaks.
        let items = vec![text("x"), text("m\n\"naïve café\""), text("b")];
        assert_eq!(
            render(&surround_fill("(", items, ")"), 16),
            "(x,\n    m\n\"naïve café\", b)"
        );
    }

    #[test]
    fn test_fill_inside_flat_group_renders_flat() {
        // An enclosing group that fits forces Mode::Flat — the fill must NOT
        // introduce breaks of its own inside it.
        let inner = surround_fill("(", vec![text("a"), text("b"), text("c")], ")");
        let doc = group(concat(vec![text("f"), inner]));
        assert_eq!(render(&doc, 80), "f(a, b, c)");
    }

    #[test]
    fn test_realistic_method_chain() {
        // items.filter(pred).map(f).collect()
        let doc = group(concat(vec![
            text("items"),
            indent(concat(vec![
                softline(),
                text(".filter(pred)"),
                softline(),
                text(".map(f)"),
                softline(),
                text(".collect()"),
            ])),
        ]));

        // Fits
        assert_eq!(render(&doc, 80), "items.filter(pred).map(f).collect()");

        // Doesn't fit
        assert_eq!(
            render(&doc, 20),
            "items\n    .filter(pred)\n    .map(f)\n    .collect()"
        );
    }
}
