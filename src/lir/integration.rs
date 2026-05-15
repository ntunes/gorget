//! Integration test: lower a real GIR module from the compilation pipeline to LIR.

#[cfg(test)]
mod tests {
    use crate::lir::{display, lower, optimize, split_edges, ssa, validate};
    use crate::ir::lowering::{lower_module as gir_lower, LoweringOptions};
    use crate::parser::Parser;

    /// Helper: compile a Gorget source string through the pipeline to GIR,
    /// then lower to LIR, optimize, and validate.
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

        // GIR → LIR lowering, then critical-edge split (Tier E §8.2) so SSA
        // construction sees a critical-edge-free CFG.
        let mut lir = lower::lower_module(&gir);
        split_edges::split_critical_edges_module(&mut lir);
        // Tier E §8.3: per-pass invariant validation. The same hooks live in
        // the production pipeline (`src/main.rs`); replicating them here lets
        // this helper exercise the full validator under debug-build assertions.
        validate::assert_module_valid(&lir, "lir-lowering");

        // Run SSA construction on each function
        for func in &mut lir.functions {
            ssa::construct_ssa(func);
        }
        validate::assert_module_valid(&lir, "ssa-construction");

        // Run optimizer
        crate::lir::types::wire_collection_bridges(&mut lir);
        validate::assert_module_valid(&lir, "wire-collection-bridges");
        crate::lir::runtime::promote_runtime_calls(&mut lir);
        validate::assert_module_valid(&lir, "promote-runtime-calls");
        optimize::optimize_module(&mut lir);
        validate::assert_module_valid(&lir, "optimize");
        // Order: pointee_types FIRST so value_types can read it for the
        // `Inst::Load { ty: Void }` fallback.
        crate::lir::types::compute_module_pointee_types(&mut lir);
        crate::lir::types::compute_module_value_types(&mut lir);
        crate::lir::types::compute_module_value_origins(&mut lir);
        validate::assert_module_valid(&lir, "compute-types");

        // Validate (post-optimization). `assert_module_valid` already
        // panicked above on debug builds; this final check still surfaces
        // errors as a `Result` for release builds where `GG_VALIDATE_PASSES`
        // is not set, so the test asserts on them either way.
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
        // After optimization, constants may be folded — just verify it lowered.
        assert!(dump.contains("fn @main"), "should contain main: {dump}");
    }

    #[test]
    fn lower_branch() {
        let source = "void main():\n    int x = 5\n    if x > 3:\n        print(\"yes\")\n    else:\n        print(\"no\")\n";
        let lir = compile_to_lir(source).expect("should lower successfully");
        let dump = display::dump_module(&lir);
        // After optimization, the constant branch may be folded away.
        assert!(dump.contains("fn @main"), "should contain main: {dump}");
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

    #[test]
    fn lower_struct() {
        let source = "struct Point:\n    int x\n    int y\n\nvoid main():\n    Point p = Point(10, 20)\n    print(\"{p.x}\")\n    print(\"{p.y}\")\n";
        let lir = compile_to_lir(source).expect("should lower struct");
        assert!(!lir.functions.is_empty());
    }

    #[test]
    fn lower_enum() {
        let source = "enum Color:\n    Red()\n    Green()\n    Blue()\n\nvoid main():\n    Color c = Color.Red()\n    match c:\n        case Color.Red():\n            print(\"red\")\n        case Color.Green():\n            print(\"green\")\n        case Color.Blue():\n            print(\"blue\")\n";
        let lir = compile_to_lir(source).expect("should lower enum + match");
        assert!(!lir.functions.is_empty());
    }

    #[test]
    fn lower_closure() {
        let source = "int apply(Callable[int(int)] f, int x):\n    return f(x)\n\nvoid main():\n    auto double = (int n): n * 2\n    int result = apply(double, 21)\n    print(\"{result}\")\n";
        let lir = compile_to_lir(source).expect("should lower closure");
        assert!(!lir.functions.is_empty());
    }

    #[test]
    fn optimizer_preserves_validity() {
        let source = "int unused():\n    return 42\n\nvoid main():\n    int x = 10\n    int y = 20\n    int z = x + y\n    print(\"{z}\")\n";
        let lir = compile_to_lir(source).expect("optimizer should preserve validity");
        // After optimization, unused() should be eliminated
        let has_unused = lir.functions.iter().any(|f| f.name.contains("unused"));
        assert!(!has_unused, "dead function 'unused' should be eliminated");
    }
}
