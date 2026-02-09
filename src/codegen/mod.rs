/// C code generation backend for the Vyper compiler.
pub mod c_emitter;
pub mod c_expr;
pub mod c_item;
pub mod c_mangle;
pub mod c_runtime;
pub mod c_stmt;
pub mod c_types;

use std::cell::RefCell;

use rustc_hash::FxHashMap;

use crate::parser::ast::Module;
use crate::semantic::ids::DefId;
use crate::semantic::resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::TypeTable;
use crate::semantic::AnalysisResult;

use c_emitter::CEmitter;

/// A closure that has been lifted to a top-level function.
pub struct LiftedClosure {
    pub id: usize,
    /// Captured variable names and their C types.
    pub captures: Vec<(String, String)>,
    /// Parameter names and their C types.
    pub params: Vec<(String, String)>,
    /// The C return type.
    pub return_type: String,
    /// The C expression body.
    pub body: String,
}

/// Context threaded through all codegen functions.
pub struct CodegenContext<'a> {
    pub scopes: &'a ScopeTable,
    pub types: &'a TypeTable,
    pub resolution_map: &'a ResolutionMap,
    pub struct_fields: &'a FxHashMap<DefId, StructFieldInfo>,
    pub enum_variants: &'a FxHashMap<DefId, EnumVariantInfo>,
    pub function_info: &'a FxHashMap<DefId, FunctionInfo>,
    pub current_self_type: Option<String>,
    pub current_function_throws: bool,
    /// Closures collected during codegen, emitted in a later pass.
    /// Uses RefCell to allow mutation from `&self` methods (gen_expr).
    pub lifted_closures: RefCell<Vec<LiftedClosure>>,
    pub closure_counter: RefCell<usize>,
}

/// Generate C source code from a parsed and analyzed Vyper module.
pub fn generate_c(module: &Module, analysis: &AnalysisResult) -> String {
    let mut ctx = CodegenContext {
        scopes: &analysis.scopes,
        types: &analysis.types,
        resolution_map: &analysis.resolution_map,
        struct_fields: &analysis.struct_fields,
        enum_variants: &analysis.enum_variants,
        function_info: &analysis.function_info,
        current_self_type: None,
        current_function_throws: false,
        lifted_closures: RefCell::new(Vec::new()),
        closure_counter: RefCell::new(0),
    };

    let mut emitter = CEmitter::new();

    // 1. Runtime preamble (includes)
    emitter.emit(c_runtime::RUNTIME);

    // 2. Forward declarations
    ctx.emit_forward_declarations(module, &mut emitter);

    // 3. Type definitions (structs, enums, type aliases, newtypes)
    ctx.emit_type_definitions(module, &mut emitter);

    // 4. Function declarations
    ctx.emit_function_declarations(module, &mut emitter);

    // 5. Function definitions (closures are collected during this pass)
    ctx.emit_function_definitions(module, &mut emitter);

    // 6. Emit lifted closures (env structs + functions) — these were
    //    collected during expression codegen in step 5. We insert them
    //    before main by re-emitting into a separate buffer and splicing.
    if !ctx.lifted_closures.borrow().is_empty() {
        let mut closure_buf = CEmitter::new();
        closure_buf.emit_line("// ── Lifted Closures ──");
        ctx.emit_lifted_closures(&mut closure_buf);
        closure_buf.blank_line();
        // The closures need to appear before the function definitions, so
        // we prepend them. Since they reference types already declared, this
        // is safe. We insert just before "// ── Function Definitions ──".
        let output = emitter.finish();
        let marker = "// ── Function Definitions ──";
        if let Some(pos) = output.find(marker) {
            let mut combined = String::with_capacity(output.len() + 512);
            combined.push_str(&output[..pos]);
            combined.push_str(&closure_buf.finish());
            combined.push_str(&output[pos..]);
            return combined;
        }
        return output + &closure_buf.finish();
    }

    emitter.finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::semantic;

    fn compile_to_c(source: &str) -> String {
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}",
            parser.errors
        );

        let result = semantic::analyze(&module);
        assert!(
            result.errors.is_empty(),
            "semantic errors: {:?}",
            result.errors
        );

        generate_c(&module, &result)
    }

    #[test]
    fn hello_world_generates_c() {
        let source = "void main():\n    print(\"Hello, World!\")\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int main(void)"));
        assert!(c_code.contains("printf"));
        assert!(c_code.contains("Hello, World!"));
        assert!(c_code.contains("return 0;"));
    }

    #[test]
    fn function_with_return() {
        let source = "int add(int a, int b):\n    return a + b\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int64_t add(int64_t a, int64_t b)"));
        assert!(c_code.contains("return (a + b);"));
    }

    #[test]
    fn expression_body_function() {
        let source = "int double(int x) = x * 2\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int64_t __vyper_double(int64_t x)"));
        assert!(c_code.contains("return (x * INT64_C(2));"));
    }

    #[test]
    fn struct_definition() {
        let source = "struct Point:\n    float x\n    float y\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef struct Point Point;"));
        assert!(c_code.contains("struct Point {"));
        assert!(c_code.contains("double x;"));
        assert!(c_code.contains("double y;"));
    }

    #[test]
    fn enum_definition() {
        let source = "enum Color:\n    Red\n    Green\n    Blue\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Color_TAG_Red"));
        assert!(c_code.contains("Color_TAG_Green"));
        assert!(c_code.contains("Color_TAG_Blue"));
        assert!(c_code.contains("Color_Tag"));
        assert!(c_code.contains("Color__Red"));
    }

    #[test]
    fn if_statement() {
        let source = "void main():\n    int x = 5\n    if x > 0:\n        print(\"positive\")\n    else:\n        print(\"negative\")\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("if ((x > INT64_C(0)))"));
        assert!(c_code.contains("} else {"));
    }

    #[test]
    fn for_loop_with_range() {
        let source = "void main():\n    for i in 0..10:\n        print(\"{i}\")\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("for (int64_t i = INT64_C(0); i < INT64_C(10); i++)"));
    }

    #[test]
    fn while_loop() {
        let source = "void main():\n    int x = 0\n    while x < 10:\n        x += 1\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("while ((x < INT64_C(10)))"));
        assert!(c_code.contains("x += INT64_C(1);"));
    }

    #[test]
    fn variable_declaration() {
        let source = "void main():\n    int x = 5\n    const int y = 10\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int64_t x = INT64_C(5);"));
        assert!(c_code.contains("const int64_t y = INT64_C(10);"));
    }

    #[test]
    fn impl_block_methods() {
        let source = "\
struct Point:
    float x
    float y

implement Point:
    Point origin():
        return Point(0.0, 0.0)

    float magnitude(self):
        return 0.0
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Point Point__origin(void)"));
        assert!(c_code.contains("double Point__magnitude(const Point* self)"));
    }

    // ── Phase 5 tests ──────────────────────────────────────

    #[test]
    fn type_alias() {
        let source = "type Score = int\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef int64_t Score;"));
    }

    #[test]
    fn newtype_def() {
        let source = "newtype UserId(int)\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef struct { int64_t value; } UserId;"));
    }

    #[test]
    fn match_with_binding() {
        let source = "\
void main():
    int x = 42
    match x:
        case 0:
            print(\"zero\")
        case n:
            print(\"other\")
";
        let c_code = compile_to_c(source);
        // Should use __typeof__ for the temp and binding
        assert!(c_code.contains("__typeof__"));
        // Binding pattern always matches (condition is 1)
        assert!(c_code.contains("if (") || c_code.contains("else if ("));
    }

    #[test]
    fn match_with_enum_destructuring() {
        let source = "\
enum Option:
    Some(int)
    None

void main():
    auto val = Some(42)
    match val:
        case Some(x):
            print(\"has value\")
        case None:
            print(\"empty\")
";
        let c_code = compile_to_c(source);
        // Should check the tag for enum variant patterns
        assert!(c_code.contains("Option_TAG_Some"));
        assert!(c_code.contains("Option_TAG_None"));
    }

    #[test]
    fn throw_statement() {
        let source = "\
void fail() throws str:
    throw \"something went wrong\"
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("VYPER_THROW("));
    }

    #[test]
    fn runtime_includes_setjmp() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("#include <setjmp.h>"));
        assert!(c_code.contains("VyperError"));
        assert!(c_code.contains("VYPER_TRY"));
        assert!(c_code.contains("VyperClosure"));
        assert!(c_code.contains("VyperArray"));
        assert!(c_code.contains("VyperString"));
    }

    #[test]
    fn list_comprehension() {
        let source = "\
void main():
    Vector[int] squares = [x * x for x in 0..10]
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("vyper_array_new"));
        assert!(c_code.contains("vyper_array_push"));
        assert!(c_code.contains("VyperArray"));
    }

    #[test]
    fn for_in_over_array() {
        let source = "\
void main():
    int[3] nums = [1, 2, 3]
    for n in nums:
        print(\"{n}\")
";
        let c_code = compile_to_c(source);
        // Should use sizeof-based iteration
        assert!(c_code.contains("sizeof(nums)/sizeof(nums[0])"));
        assert!(c_code.contains("__typeof__"));
    }

    #[test]
    fn vector_type_maps_to_vyper_array() {
        let source = "\
void main():
    Vector[int] items = [1, 2, 3]
";
        let c_code = compile_to_c(source);
        // Vector[int] should be VyperArray, not void*
        assert!(c_code.contains("VyperArray"));
    }
}
