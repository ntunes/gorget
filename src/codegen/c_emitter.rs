/// Output buffer with indentation management for C code generation.
pub struct CEmitter {
    output: String,
    indent_level: usize,
    temp_counter: usize,
}

impl CEmitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            temp_counter: 0,
        }
    }

    /// Append raw text (no newline, no indent).
    pub fn emit(&mut self, text: &str) {
        self.output.push_str(text);
    }

    /// Append an indented line with trailing newline.
    pub fn emit_line(&mut self, line: &str) {
        for _ in 0..self.indent_level {
            self.output.push_str("    ");
        }
        self.output.push_str(line);
        self.output.push('\n');
    }

    /// Emit a blank line.
    pub fn blank_line(&mut self) {
        self.output.push('\n');
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    /// Generate a fresh temporary variable name.
    pub fn fresh_temp(&mut self) -> String {
        let name = format!("__gorget_tmp_{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    /// Consume and return the generated output.
    pub fn finish(self) -> String {
        self.output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emit_line_with_indent() {
        let mut e = CEmitter::new();
        e.emit_line("int x = 5;");
        e.indent();
        e.emit_line("x++;");
        e.dedent();
        e.emit_line("return x;");
        let out = e.finish();
        assert_eq!(out, "int x = 5;\n    x++;\nreturn x;\n");
    }

    #[test]
    fn fresh_temp_increments() {
        let mut e = CEmitter::new();
        assert_eq!(e.fresh_temp(), "__gorget_tmp_0");
        assert_eq!(e.fresh_temp(), "__gorget_tmp_1");
        assert_eq!(e.fresh_temp(), "__gorget_tmp_2");
    }

    #[test]
    fn emit_raw() {
        let mut e = CEmitter::new();
        e.emit("hello");
        e.emit(" world");
        assert_eq!(e.finish(), "hello world");
    }
}
