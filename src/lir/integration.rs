//! Integration test: lower a real GIR module from the compilation pipeline to LIR.

#[cfg(test)]
mod tests {
    use crate::lir::{display, lower, ssa, validate};
    use crate::ir::lowering::{lower_module as gir_lower, LoweringOptions};
    use crate::parser::Parser;

    /// Helper: compile a Gorget source string through the pipeline to GIR,
    /// then lower to LIR and validate.
    fn compile_to_lir(source: &str) -> Result<super::super::LirModule, String> {
        // Parse
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        if !parser.errors.is_empty() {
            return Err(format!("parse errors: {:?}", parser.errors));
        }

        // Semantic analysis
        let result = crate::semantic::analyze(&mut module, &[]);
        if !result.errors.is_empty() {
            return Err(format!("semantic errors: {:?}", result.errors));
        }

        // GIR lowering
        let gir = gir_lower(&module, &result, &LoweringOptions::default());

        // GIR → LIR lowering
        let mut lir = lower::lower_module(&gir);

        // Run SSA construction on each function
        for func in &mut lir.functions {
            ssa::construct_ssa(func);
        }

        // Validate
        let errors = validate::validate_module(&lir);
        if !errors.is_empty() {
            let msgs: Vec<String> = errors.iter().map(|e| format!("{e}")).collect();
            return Err(format!("LIR validation errors: {}", msgs.join("; ")));
        }

        Ok(lir)
    }

    #[test]
    fn lower_hello_world() {
        let source = "void main():\n    print(\"Hello, world!\")\n";
        let lir = compile_to_lir(source).expect("should lower successfully");
        assert!(!lir.functions.is_empty());

        let dump = display::dump_module(&lir);
        assert!(dump.contains("fn @main"), "should contain main function");
    }

    #[test]
    fn lower_arithmetic() {
        let source = "void main():\n    int x = 10\n    int y = 20\n    int z = x + y\n    print(\"{z}\")\n";
        let lir = compile_to_lir(source).expect("should lower successfully");
        let dump = display::dump_module(&lir);
        assert!(dump.contains("add"), "should contain add instruction: {dump}");
    }

    #[test]
    fn lower_branch() {
        let source = "void main():\n    int x = 5\n    if x > 3:\n        print(\"yes\")\n    else:\n        print(\"no\")\n";
        let lir = compile_to_lir(source).expect("should lower successfully");
        let dump = display::dump_module(&lir);
        assert!(
            dump.contains("br ") || dump.contains("cmp"),
            "should contain branch or compare: {dump}"
        );
    }

    #[test]
    fn lower_function_call() {
        let source = "int add(int a, int b):\n    return a + b\n\nvoid main():\n    int result = add(3, 4)\n    print(\"{result}\")\n";
        let lir = compile_to_lir(source).expect("should lower successfully");
        assert!(
            lir.functions.len() >= 2,
            "should have at least 2 functions, got {}",
            lir.functions.len()
        );
    }

    #[test]
    fn lower_while_loop() {
        let source = "void main():\n    int i = 0\n    while i < 5:\n        i = i + 1\n    print(\"{i}\")\n";
        let lir = compile_to_lir(source).expect("should lower successfully");
        let dump = display::dump_module(&lir);
        assert!(
            dump.contains("jmp bb") || dump.contains("br "),
            "should contain control flow: {dump}"
        );
    }
}
