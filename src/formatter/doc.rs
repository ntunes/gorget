// ══════════════════════════════════════════════════════════════
// Document IR — Wadler-Lindig pretty printer for gg fmt
// ══════════════════════════════════════════════════════════════
//
// A lightweight document algebra that enables "fit on one line or break"
// decisions. The formatter builds a Doc tree from AST, then the renderer
// resolves groups against a max line width.

/// Maximum line width for the formatter.
pub const MAX_WIDTH: usize = 100;

/// Indentation unit: 4 spaces.
pub const INDENT_WIDTH: usize = 4;

/// A document node in the formatting intermediate representation.
///
/// The key insight: `Group` tries to render its contents flat (all on one line).
/// If the flat rendering exceeds the remaining width, it switches to broken mode
/// where `Line` nodes become actual newlines with indentation.
#[derive(Debug, Clone)]
pub enum Doc {
    /// Literal text — never contains newlines.
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

/// Surround a list of items with open/close delimiters.
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

/// Render a Doc tree with the default max width.
pub fn render_default(doc: &Doc) -> String {
    render(doc, MAX_WIDTH)
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
                self.col += s.len();
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
        }
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

/// Measure the flat (single-line) width of a Doc.
/// Returns `None` if the doc contains a `HardLine` (cannot be flattened).
fn measure_flat(doc: &Doc) -> Option<usize> {
    match doc {
        Doc::Text(s) => Some(s.len()),
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
