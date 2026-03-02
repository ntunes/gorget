/// Source location utilities for the GIR interpreter error display.
///
/// Provides line/column mapping from byte offsets and rich error formatting
/// with optional source snippets and call stack backtraces.

use crate::span::Span;
use super::config::BacktraceLevel;
use super::dispatch::StackFrame;

/// Pre-computed line-start byte offsets for efficient span → line/col mapping.
pub struct LineIndex {
    /// Byte offset of the start of each line (0-indexed: line_starts[0] = 0).
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Build a line index for the given source text.
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0usize];
        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Convert a byte offset to a 1-based (line, col) pair.
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line_idx = self.line_starts.partition_point(|&s| s <= offset).saturating_sub(1);
        let line_start = self.line_starts[line_idx];
        let col = offset - line_start + 1;
        (line_idx + 1, col) // 1-based
    }

    /// Return the text of a 1-based line number (without the trailing newline).
    pub fn line_text<'a>(&self, source: &'a str, line: usize) -> &'a str {
        if line == 0 || line > self.line_starts.len() {
            return "";
        }
        let start = self.line_starts[line - 1];
        let end = if line < self.line_starts.len() {
            self.line_starts[line].saturating_sub(1) // before the \n
        } else {
            source.len()
        };
        &source[start..end.min(source.len())]
    }
}

/// Format a runtime error with source location and backtrace to stderr.
///
/// Output style matches Rust/Gorget compiler error format:
/// ```text
/// gorget: index 5 out of bounds (len=3)
///   --> example.gg:42:15
///    |
/// 42 |     items.get(5)
///    |               ^
///    = in 'process_items' (example.gg:28)
///    = called from 'main' (example.gg:12)
/// ```
pub fn format_sim_error(
    error_msg: &str,
    error_span: Option<Span>,
    backtrace: Option<&[StackFrame]>,
    filename: &str,
    source: Option<&str>,
    line_index: Option<&LineIndex>,
    level: &BacktraceLevel,
) {
    eprintln!("{error_msg}");

    if matches!(level, BacktraceLevel::Off) {
        return;
    }

    let (Some(li), Some(src)) = (line_index, source) else {
        return;
    };

    // Print the primary error location with a source snippet.
    if let Some(span) = error_span {
        if span.start < src.len() {
            let (line, col) = li.line_col(span.start);
            eprintln!("  --> {filename}:{line}:{col}");
            let text = li.line_text(src, line);
            let line_label = format!("{line}");
            let padding = " ".repeat(line_label.len());
            eprintln!("   {padding}|");
            eprintln!("{line_label:>3} | {text}");
            // Underline: compute width in characters (capped to remaining line).
            let col0 = col.saturating_sub(1); // 0-based column
            let span_len = span.end.saturating_sub(span.start);
            let available = text.len().saturating_sub(col0);
            let width = span_len.max(1).min(available).max(1);
            let caret_pad = " ".repeat(col0);
            eprintln!("   {padding}| {caret_pad}{}", "^".repeat(width));
        }
    }

    // Print backtrace frames.
    let frames = match backtrace {
        Some(f) if !f.is_empty() => f,
        _ => return,
    };

    let max_frames = match level {
        BacktraceLevel::Off => return,
        BacktraceLevel::Pruned => 3,
        BacktraceLevel::Full => frames.len(),
    };

    for (i, frame) in frames.iter().take(max_frames).enumerate() {
        let name = frame.display_name.as_deref().unwrap_or(&frame.fn_name);
        // For the innermost frame (i==0): show where the error happened (current_span).
        // For outer frames (i>0): show where in that frame the inner function was called.
        //   The call site within frames[i] is recorded as frames[i-1].call_span,
        //   because call_span tracks "where I was called from" (in the caller's body).
        let loc_span = if i == 0 {
            frame.current_span
        } else {
            frames[i - 1].call_span
        };
        if let Some(span) = loc_span {
            if span.start < src.len() {
                let (line, _) = li.line_col(span.start);
                if i == 0 {
                    eprintln!("   = in '{name}' ({filename}:{line})");
                } else {
                    eprintln!("   = called from '{name}' ({filename}:{line})");
                }
                continue;
            }
        }
        if i == 0 {
            eprintln!("   = in '{name}'");
        } else {
            eprintln!("   = called from '{name}'");
        }
    }

    if matches!(level, BacktraceLevel::Pruned) && frames.len() > max_frames {
        eprintln!(
            "   = ... {} more frame(s) (use --backtrace=full to see all)",
            frames.len() - max_frames
        );
    }
}
