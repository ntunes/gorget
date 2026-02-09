/// C code generation backend for the Gorget compiler.
pub mod c_emitter;
pub mod c_expr;
pub mod c_item;
pub mod c_mangle;
pub mod c_runtime;
pub mod c_stmt;
pub mod c_types;

use std::cell::RefCell;
use std::collections::HashSet;

use rustc_hash::FxHashMap;

use crate::parser::ast::Module;
use crate::semantic::ids::DefId;
use crate::semantic::resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::TypeTable;
use crate::semantic::traits::TraitRegistry;
use crate::semantic::AnalysisResult;

use crate::parser::ast::{EnumDef, FunctionDef, StructDef};
use c_emitter::CEmitter;

/// A registered generic instantiation.
#[derive(Clone)]
pub struct GenericInstance {
    pub base_name: String,
    pub mangled_name: String,
    pub c_type_args: Vec<String>,
    pub kind: GenericInstanceKind,
}

/// The kind of generic definition being instantiated.
#[derive(Clone)]
pub enum GenericInstanceKind {
    Struct,
    Enum,
    Function,
}

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
    pub traits: &'a TraitRegistry,
    pub current_self_type: Option<String>,
    pub current_function_throws: bool,
    /// Closures collected during codegen, emitted in a later pass.
    /// Uses RefCell to allow mutation from `&self` methods (gen_expr).
    pub lifted_closures: RefCell<Vec<LiftedClosure>>,
    pub closure_counter: RefCell<usize>,
    /// Generic instantiations registered during codegen.
    pub generic_instances: RefCell<Vec<GenericInstance>>,
    /// Generic struct templates stored for monomorphization.
    pub generic_struct_templates: RefCell<FxHashMap<String, StructDef>>,
    /// Generic enum templates stored for monomorphization.
    pub generic_enum_templates: RefCell<FxHashMap<String, EnumDef>>,
    /// Generic function templates stored for monomorphization.
    pub generic_fn_templates: RefCell<FxHashMap<String, FunctionDef>>,
    /// Variables declared with GorgetClosure type (need fn_ptr dispatch on call).
    pub closure_vars: RefCell<HashSet<String>>,
}

/// Generate C source code from a parsed and analyzed Gorget module.
pub fn generate_c(module: &Module, analysis: &AnalysisResult) -> String {
    let mut ctx = CodegenContext {
        scopes: &analysis.scopes,
        types: &analysis.types,
        resolution_map: &analysis.resolution_map,
        struct_fields: &analysis.struct_fields,
        enum_variants: &analysis.enum_variants,
        function_info: &analysis.function_info,
        traits: &analysis.traits,
        current_self_type: None,
        current_function_throws: false,
        lifted_closures: RefCell::new(Vec::new()),
        closure_counter: RefCell::new(0),
        generic_instances: RefCell::new(Vec::new()),
        generic_struct_templates: RefCell::new(FxHashMap::default()),
        generic_enum_templates: RefCell::new(FxHashMap::default()),
        generic_fn_templates: RefCell::new(FxHashMap::default()),
        closure_vars: RefCell::new(HashSet::new()),
    };

    let mut emitter = CEmitter::new();

    ctx.collect_generic_templates(module);
    ctx.discover_generic_usages(module);

    // 1. Runtime preamble (includes)
    emitter.emit(c_runtime::RUNTIME);

    // 2. Forward declarations
    ctx.emit_forward_declarations(module, &mut emitter);

    // 3. Type definitions (structs, enums, type aliases, newtypes)
    ctx.emit_type_definitions(module, &mut emitter);

    // 3b. Emit monomorphized generic instantiations
    ctx.emit_generic_instantiations(&mut emitter);

    // 4. Function declarations
    ctx.emit_function_declarations(module, &mut emitter);

    // 4b. Vtable instances (after function declarations, before definitions)
    ctx.emit_vtable_instances(module, &mut emitter);

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
        assert!(c_code.contains("int64_t __gorget_double(int64_t x)"));
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

equip Point:
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
        assert!(c_code.contains("struct UserId {"));
        assert!(c_code.contains("int64_t value;"));
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
        assert!(c_code.contains("GORGET_THROW("));
    }

    #[test]
    fn runtime_includes_setjmp() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("#include <setjmp.h>"));
        assert!(c_code.contains("GorgetError"));
        assert!(c_code.contains("GORGET_TRY"));
        assert!(c_code.contains("GorgetClosure"));
        assert!(c_code.contains("GorgetArray"));
        assert!(c_code.contains("GorgetString"));
    }

    #[test]
    fn list_comprehension() {
        let source = "\
void main():
    Vector[int] squares = [x * x for x in 0..10]
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("gorget_array_new"));
        assert!(c_code.contains("gorget_array_push"));
        assert!(c_code.contains("GorgetArray"));
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
    fn vector_type_maps_to_gorget_array() {
        let source = "\
void main():
    Vector[int] items = [1, 2, 3]
";
        let c_code = compile_to_c(source);
        // Vector[int] should be GorgetArray, not void*
        assert!(c_code.contains("GorgetArray"));
    }

    // ── Phase 6 tests ──────────────────────────────────────

    #[test]
    fn trait_definition_comment() {
        let source = "\
trait Drawable:
    void draw(self)
    str name(self)
";
        let c_code = compile_to_c(source);
        // Trait should be emitted as a descriptive comment
        assert!(c_code.contains("/* trait Drawable:"));
        assert!(c_code.contains("draw"));
        assert!(c_code.contains("name"));
        // Should also emit vtable and trait obj structs
        assert!(c_code.contains("Drawable_VTable"), "Should emit vtable struct");
        assert!(c_code.contains("Drawable_TraitObj"), "Should emit trait obj struct");
    }

    #[test]
    fn extern_block() {
        let source = "\
extern \"C\":
    int printf(str fmt)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("extern"));
        assert!(c_code.contains("printf"));
    }

    #[test]
    fn nil_coalescing_expr() {
        let source = "\
void main():
    auto a = None
    auto b = a ?? 42
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("!= NULL"));
    }

    #[test]
    fn await_stub() {
        let source = "\
async void main():
    auto x = await 42
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("/* await */"));
    }

    #[test]
    fn spawn_stub() {
        let source = "\
void main():
    auto x = spawn 42
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("/* spawn */"));
    }

    #[test]
    fn tuple_type_mapping() {
        // Test that tuple types in AST map to anonymous structs
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Tuple(vec![
            crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            },
            crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Float),
                span: crate::span::Span::new(0, 0),
            },
        ]);
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert!(result.contains("struct"));
        assert!(result.contains("int64_t _0"));
        assert!(result.contains("double _1"));
    }

    #[test]
    fn function_pointer_type_mapping() {
        // Test that function types in AST map to function pointers
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Function {
            return_type: Box::new(crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }),
            params: vec![
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
            ],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "int64_t (*)(int64_t, int64_t)");

        // c_declare should splice the name into (*)
        let decl = c_types::c_declare(&result, "add");
        assert_eq!(decl, "int64_t (*add)(int64_t, int64_t)");
    }

    #[test]
    fn slice_type_mapping() {
        // Test that slice types in AST map to struct { data, len }
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Slice {
            element: Box::new(crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }),
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert!(result.contains("struct"));
        assert!(result.contains("data"));
        assert!(result.contains("len"));
        assert!(result.contains("int64_t"));
    }

    #[test]
    fn dynamic_type_mapping() {
        // Test that dynamic trait objects map to void*
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Dynamic {
            trait_: Box::new(crate::span::Spanned {
                node: crate::parser::ast::Type::Named {
                    name: crate::span::Spanned {
                        node: "Drawable".to_string(),
                        span: crate::span::Span::new(0, 0),
                    },
                    generic_args: vec![],
                },
                span: crate::span::Span::new(0, 0),
            }),
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "Drawable_TraitObj");
    }

    // ── Phase 7 tests ──────────────────────────────────────

    #[test]
    fn runtime_includes_gorget_map() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GorgetMap"));
        assert!(c_code.contains("__gorget_fnv1a"));
        assert!(c_code.contains("gorget_map_new"));
        assert!(c_code.contains("gorget_map_put"));
        assert!(c_code.contains("gorget_map_get"));
        assert!(c_code.contains("gorget_map_contains"));
        assert!(c_code.contains("gorget_map_len"));
        assert!(c_code.contains("gorget_map_free"));
    }

    #[test]
    fn runtime_includes_gorget_set() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GorgetSet"));
        assert!(c_code.contains("gorget_set_new"));
        assert!(c_code.contains("gorget_set_add"));
        assert!(c_code.contains("gorget_set_contains"));
        assert!(c_code.contains("gorget_set_len"));
        assert!(c_code.contains("gorget_set_free"));
    }

    #[test]
    fn runtime_includes_string_format() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("gorget_string_format"));
        assert!(c_code.contains("gorget_string_cstr"));
    }

    #[test]
    fn runtime_includes_array_at_macro() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GORGET_ARRAY_AT"));
    }

    #[test]
    fn set_type_maps_to_gorget_set() {
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Set".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "GorgetSet");
    }

    #[test]
    fn dict_type_maps_to_gorget_map() {
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Dict".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
            ],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "GorgetMap");
    }

    #[test]
    fn set_comprehension_generates_gorget_set() {
        let source = "\
void main():
    Set[int] evens = {x for x in 0..10 if x % 2 == 0}
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("/* TODO"));
        assert!(c_code.contains("gorget_set_new"));
        assert!(c_code.contains("gorget_set_add"));
    }

    #[test]
    fn dict_comprehension_generates_gorget_map() {
        let source = "\
void main():
    Dict[int, int] squares = {x: x * x for x in 0..10}
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("/* TODO"));
        assert!(c_code.contains("gorget_map_new"));
        assert!(c_code.contains("gorget_map_put"));
    }

    #[test]
    fn no_unsupported_expr_for_handled_variants() {
        // Ensure basic programs don't emit "unsupported expr"
        let source = "\
void main():
    int x = 5
    if x > 0:
        print(\"yes\")
    else:
        print(\"no\")
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("unsupported expr"));
    }

    // ── Phase 8 tests ──────────────────────────────────────

    #[test]
    fn generic_struct_skips_template() {
        // The bare generic struct (with type params) should NOT be emitted to C output
        let source = "\
struct Pair[A, B]:
    A first
    B second
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("struct Pair {"), "Generic template should not be emitted directly");
        assert!(!c_code.contains("typedef struct Pair Pair;"), "Generic template forward decl should not be emitted");
    }

    #[test]
    fn generic_struct_emits_specialized() {
        let source = "\
struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](1, 2.0)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Pair__int64_t__double"), "Should contain mangled struct name");
        assert!(c_code.contains("int64_t first;"), "Specialized struct should have int64_t field");
        assert!(c_code.contains("double second;"), "Specialized struct should have double field");
    }

    #[test]
    fn generic_struct_forward_decl() {
        let source = "\
struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](1, 2.0)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef struct Pair__int64_t__double Pair__int64_t__double;"),
            "Monomorphized struct should get typedef forward decl");
    }

    #[test]
    fn generic_type_maps_correctly() {
        // Set[int] → GorgetSet (built-in), Pair[int, str] → mangled name (user-defined)
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();

        // Set[int] should still map to GorgetSet
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Set".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }],
        };
        assert_eq!(c_types::ast_type_to_c(&ty, &scopes), "GorgetSet");

        // Unknown generic type should produce mangled name
        let ty2 = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Pair".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Str),
                    span: crate::span::Span::new(0, 0),
                },
            ],
        };
        let result = c_types::ast_type_to_c(&ty2, &scopes);
        assert_eq!(result, "Pair__int64_t__const_char_ptr");
    }

    #[test]
    fn generic_call_mangles_name() {
        let source = "\
int max[T](T a, T b) = a

void main():
    int x = max[int](1, 2)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("max__int64_t("), "Generic call should use mangled name");
    }

    #[test]
    fn generic_function_emits_specialized() {
        let source = "\
int identity[T](T x) = x

void main():
    int y = identity[int](42)
";
        let c_code = compile_to_c(source);
        // The generic template should not be emitted
        assert!(!c_code.contains("int64_t identity("), "Generic template should not be emitted");
        // The specialized version should be emitted
        assert!(c_code.contains("identity__int64_t"), "Specialized function should be emitted");
    }

    // ── Phase 9 tests ──────────────────────────────────────

    #[test]
    fn trait_emits_vtable_struct() {
        let source = "\
trait Shape:
    float area(self)
    void draw(self)
";
        let c_code = compile_to_c(source);
        // Vtable struct with function pointers
        assert!(c_code.contains("Shape_VTable"), "Should emit vtable struct name");
        assert!(c_code.contains("double (*area)(const void*)"), "Should have area fn ptr");
        assert!(c_code.contains("void (*draw)(const void*)"), "Should have draw fn ptr");
    }

    #[test]
    fn trait_emits_trait_obj_struct() {
        let source = "\
trait Shape:
    float area(self)
";
        let c_code = compile_to_c(source);
        // Trait object struct with data + vtable
        assert!(c_code.contains("Shape_TraitObj"), "Should emit trait obj struct name");
        assert!(c_code.contains("void* data;"), "Trait obj should have data pointer");
        assert!(c_code.contains("const Shape_VTable* vtable;"), "Trait obj should have vtable pointer");
    }

    #[test]
    fn trait_impl_emits_vtable_instance() {
        let source = "\
trait Shape:
    float area(self)
    void draw(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return 3.14
    void draw(self):
        pass
";
        let c_code = compile_to_c(source);
        // Static vtable with function pointer assignments
        assert!(c_code.contains("static const Shape_VTable Shape_for_Circle_vtable"),
            "Should emit static vtable instance");
        assert!(c_code.contains(".area = "),
            "Should assign area in vtable");
        assert!(c_code.contains(".draw = "),
            "Should assign draw in vtable");
        assert!(c_code.contains("Shape_for_Circle__area"),
            "Should reference mangled impl function for area");
        assert!(c_code.contains("Shape_for_Circle__draw"),
            "Should reference mangled impl function for draw");
    }

    #[test]
    fn dynamic_type_maps_to_trait_obj() {
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Dynamic {
            trait_: Box::new(crate::span::Spanned {
                node: crate::parser::ast::Type::Named {
                    name: crate::span::Spanned {
                        node: "Shape".to_string(),
                        span: crate::span::Span::new(0, 0),
                    },
                    generic_args: vec![],
                },
                span: crate::span::Span::new(0, 0),
            }),
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "Shape_TraitObj");
    }

    #[test]
    fn box_dynamic_trait_maps_to_trait_obj() {
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Box".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![crate::span::Spanned {
                node: crate::parser::ast::Type::Dynamic {
                    trait_: Box::new(crate::span::Spanned {
                        node: crate::parser::ast::Type::Named {
                            name: crate::span::Spanned {
                                node: "Shape".to_string(),
                                span: crate::span::Span::new(0, 0),
                            },
                            generic_args: vec![],
                        },
                        span: crate::span::Span::new(0, 0),
                    }),
                },
                span: crate::span::Span::new(0, 0),
            }],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "Shape_TraitObj");
    }

    #[test]
    fn trait_vtable_forward_decl() {
        let source = "\
trait Drawable:
    void draw(self)
";
        let c_code = compile_to_c(source);
        // Forward declaration should appear in forward declarations section
        assert!(c_code.contains("typedef struct Drawable_VTable Drawable_VTable;"),
            "Should forward-declare vtable struct");
    }

    #[test]
    fn trait_method_with_params() {
        let source = "\
trait Renderer:
    void render(self, int width, int height)
";
        let c_code = compile_to_c(source);
        // Vtable fn ptr should include non-self params
        assert!(c_code.contains("void (*render)(const void*, int64_t, int64_t)"),
            "Vtable fn ptr should include non-self params");
    }
}
