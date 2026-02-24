pub mod context;
pub mod exprs;
pub mod functions;
pub mod stmts;
pub mod types;

use crate::ir::types::*;
use crate::ir::{ExternDecl, Module};
use crate::parser::ast::{self, Item};
use crate::semantic::AnalysisResult;

use context::LoweringContext;
use functions::lower_function;
use types::TypeMapper;

/// Lower an AST module + analysis result into a GIR module.
pub fn lower_module(
    ast_module: &ast::Module,
    analysis: &AnalysisResult,
) -> Module {
    let mut module = Module::new();

    // Create type mapper
    let type_mapper = TypeMapper::new(&mut module.type_registry);

    // Register printf as an extern (variadic)
    module.externs.push(ExternDecl {
        name: "printf".into(),
        params: vec![], // variadic — actual params vary per call
        return_type: I32_TYPE,
        is_variadic: true,
    });

    // Create lowering context
    let mut ctx = LoweringContext::new(analysis, type_mapper);

    // Pre-scan: build fn_sigs map for all functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            let name = &func.name.node;
            let is_main = name == "main";

            let ret_type = if is_main {
                I32_TYPE
            } else {
                ctx.type_mapper.map_ast_type(&func.return_type.node)
            };

            let param_types: Vec<TypeId> = func
                .params
                .iter()
                .map(|p| ctx.type_mapper.map_ast_type(&p.node.type_.node))
                .collect();

            ctx.fn_sigs.insert(name.clone(), (param_types, ret_type));
        }
    }

    // Lower all functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            lower_function(&mut ctx, &mut module, func);
        }
    }

    // Validate the resulting module
    let errors = crate::ir::validate::validate(&module);
    if !errors.is_empty() {
        eprintln!("GIR validation errors:");
        for err in &errors {
            eprintln!("  {}", err);
        }
        panic!("GIR module failed validation ({} errors)", errors.len());
    }

    module
}

/// Create an empty AnalysisResult for testing by parsing an empty module.
#[cfg(test)]
pub fn empty_analysis_for_test() -> AnalysisResult {
    let mut parser = crate::parser::Parser::new("void main():\n    pass\n");
    let mut module = parser.parse_module();
    crate::semantic::analyze(&mut module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn parse_and_analyze(source: &str) -> (ast::Module, AnalysisResult) {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "Parse errors: {:?}", parser.errors);
        let result = crate::semantic::analyze(&mut module);
        assert!(result.errors.is_empty(), "Semantic errors: {:?}", result.errors);
        (module, result)
    }

    #[test]
    fn lower_hello_world() {
        let source = r#"void main():
    print("Hello, World!")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        assert_eq!(gir.functions.len(), 1);
        assert_eq!(gir.functions[0].name, "main");
        assert_eq!(gir.functions[0].return_type, I32_TYPE);

        // Should have at least one block with a printf call
        let main = &gir.functions[0];
        let has_printf = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::CallExtern { func, .. } if func == "printf")
            })
        });
        assert!(has_printf, "main should contain a printf call");
    }

    #[test]
    fn lower_function_with_params() {
        let source = r#"int add(int a, int b):
    return a + b

void main():
    int r = add(1, 2)
    print("{r}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        assert_eq!(gir.functions.len(), 2);

        let add_fn = gir.find_function("add").unwrap();
        assert_eq!(add_fn.params.len(), 2);
        // _0 = return, _1 = a, _2 = b
        assert_eq!(add_fn.locals[1].name_hint.as_deref(), Some("a"));
        assert_eq!(add_fn.locals[2].name_hint.as_deref(), Some("b"));
    }

    #[test]
    fn lower_multiple_functions() {
        let source = r#"int double(int x) = x * 2

int triple(int x):
    return x * 3

void main():
    int a = double(5)
    int b = triple(5)
    print("{a}")
    print("{b}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        assert_eq!(gir.functions.len(), 3);
        assert!(gir.find_function("double").is_some());
        assert!(gir.find_function("triple").is_some());
        assert!(gir.find_function("main").is_some());
    }
}
