/// A byte-offset range in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end, "inverted span: start ({start}) > end ({end})");
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
    }
}

/// Per-module source information for byte-offset → `(file, line, col)` resolution.
///
/// `base_offset` is the offset within the concatenated multi-module source
/// where this file's bytes begin (assigned by the loader, mirroring the
/// `ErrorReporter::new_multi` shape). Spans in the IR pipeline use these
/// global offsets; `offset_to_location` maps them back to `(file, line, col)`.
#[derive(Debug, Clone)]
pub struct FileInfo {
    pub filename: String,
    pub source: String,
    pub base_offset: usize,
}

/// Map a global byte offset to `(filename, line, column)` (1-based).
/// Returns `None` if the offset doesn't fall inside any known file.
///
/// Implementation: linear-walk the file list to find the containing file
/// (small N — typically <50 files), then linear-walk the file's source to
/// find the line. Speed is not critical; this is called once per
/// panic-emit site at C codegen time.
pub fn offset_to_location<'a>(
    file_infos: &'a [FileInfo],
    byte_offset: usize,
) -> Option<(&'a str, u32, u32)> {
    let mut best: Option<&'a FileInfo> = None;
    for fi in file_infos {
        if byte_offset >= fi.base_offset && byte_offset <= fi.base_offset + fi.source.len() {
            match best {
                Some(prev) if prev.base_offset >= fi.base_offset => {}
                _ => best = Some(fi),
            }
        }
    }
    let fi = best?;
    let local = byte_offset.saturating_sub(fi.base_offset);
    let mut line: u32 = 1;
    let mut last_nl: usize = 0;
    for (i, b) in fi.source.as_bytes().iter().enumerate() {
        if i >= local {
            break;
        }
        if *b == b'\n' {
            line += 1;
            last_nl = i + 1;
        }
    }
    let col = (local.saturating_sub(last_nl) as u32) + 1;
    Some((fi.filename.as_str(), line, col))
}

/// A value annotated with its source location.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn dummy(node: T) -> Self {
        Self {
            node,
            span: Span::dummy(),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fi(name: &str, src: &str, off: usize) -> FileInfo {
        FileInfo { filename: name.into(), source: src.into(), base_offset: off }
    }

    #[test]
    fn offset_to_location_single_file() {
        let files = vec![fi("a.gg", "void main():\n    print(1)\n", 0)];
        assert_eq!(offset_to_location(&files, 0), Some(("a.gg", 1, 1)));
        assert_eq!(offset_to_location(&files, 4), Some(("a.gg", 1, 5)));
        // Position after first '\n' (index 12) — start of line 2, column 1.
        assert_eq!(offset_to_location(&files, 13), Some(("a.gg", 2, 1)));
        assert_eq!(offset_to_location(&files, 17), Some(("a.gg", 2, 5)));
    }

    #[test]
    fn offset_to_location_multi_file() {
        let a = "void main():\n    print(1)\n"; // 26 bytes
        let b = "int x = 1\n";
        let files = vec![
            fi("a.gg", a, 0),
            fi("b.gg", b, 26),
        ];
        assert_eq!(offset_to_location(&files, 0), Some(("a.gg", 1, 1)));
        assert_eq!(offset_to_location(&files, 26), Some(("b.gg", 1, 1)));
        assert_eq!(offset_to_location(&files, 30), Some(("b.gg", 1, 5)));
    }

    #[test]
    fn offset_to_location_out_of_range() {
        let files = vec![fi("a.gg", "abc", 0)];
        assert_eq!(offset_to_location(&files, 100), None);
    }

    #[test]
    fn offset_to_location_empty() {
        let files: Vec<FileInfo> = vec![];
        assert_eq!(offset_to_location(&files, 0), None);
    }
}
