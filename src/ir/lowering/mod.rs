pub mod closures;
pub mod context;
pub mod drops;
pub mod exprs;
pub mod functions;
pub mod generics;
pub mod stmts;
pub mod traits;
pub mod types;

use crate::ir::types::*;
use crate::ir::{ExternDecl, Module};
use crate::parser::ast::{self, Item};
use crate::semantic::AnalysisResult;

use context::LoweringContext;
use functions::lower_function;
use generics::GenericCollector;
use types::TypeMapper;

/// Lower an AST module + analysis result into a GIR module.
pub fn lower_module(
    ast_module: &ast::Module,
    analysis: &AnalysisResult,
) -> Module {
    let mut module = Module::new();

    // Create type mapper
    let mut type_mapper = TypeMapper::new(&mut module.type_registry);

    // Pre-scan: register non-generic struct and enum type definitions
    for item in &ast_module.items {
        match &item.node {
            Item::Struct(struct_def) => {
                types::register_struct_type(
                    &mut type_mapper,
                    &mut module.type_registry,
                    struct_def,
                );
            }
            Item::Enum(enum_def) => {
                types::register_enum_type(
                    &mut type_mapper,
                    &mut module.type_registry,
                    enum_def,
                );
            }
            _ => {}
        }
    }

    // P2.3: Generic monomorphization — collect templates, discover usages, monomorphize
    let mut generic_collector = GenericCollector::new();
    generic_collector.collect_templates(ast_module);
    generic_collector.discover_usages(ast_module);
    generic_collector.monomorphize_types(&mut type_mapper, &mut module.type_registry);

    // Register printf as an extern (variadic)
    module.externs.push(ExternDecl {
        name: "printf".into(),
        params: vec![], // variadic — actual params vary per call
        return_type: I32_TYPE,
        is_variadic: true,
    });

    // Move type_registry into LoweringContext for the lowering phase
    let type_registry = std::mem::take(&mut module.type_registry);
    let mut ctx = LoweringContext::new(analysis, type_mapper, type_registry);

    // P2.5: Register VTable and TraitObj types for all trait definitions
    let trait_info = traits::register_trait_types(&mut ctx, ast_module);

    // Populate struct field info cache (includes monomorphized + vtable/trait obj types)
    ctx.populate_struct_fields();

    // Pre-scan: register enum variant → (enum_name, variant_name) mappings
    for item in &ast_module.items {
        if let Item::Enum(enum_def) = &item.node {
            if enum_def.generic_params.is_some() {
                continue;
            }
            let enum_name = &enum_def.name.node;
            for variant in &enum_def.variants {
                let variant_name = &variant.node.name.node;
                ctx.enum_variants.insert(
                    variant_name.clone(),
                    (enum_name.clone(), variant_name.clone()),
                );
            }
        }
    }

    // Register monomorphized enum variants from TypeDefs
    for type_def in ctx.type_registry.type_defs() {
        if let TypeDefKind::Enum(ref e) = type_def.kind {
            // Only register for monomorphized types (contain __ separator)
            if type_def.name.contains("__") {
                for variant in &e.variants {
                    ctx.enum_variants.insert(
                        variant.name.clone(),
                        (type_def.name.clone(), variant.name.clone()),
                    );
                }
            }
        }
    }

    // Pre-scan: build fn_sigs map for all non-generic functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // Generic functions handled via monomorphization
            }
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

    // Register monomorphized function signatures
    generic_collector.register_fn_sigs(&ctx.type_mapper, &mut ctx.fn_sigs);

    // Pre-scan: register non-generic equip method signatures
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            // Skip generic equip blocks and trait equip blocks
            if equip.generic_params.is_some() || equip.trait_.is_some() {
                continue;
            }
            // Skip equip blocks on generic types (they're handled via monomorphization)
            if let ast::Type::Named { generic_args, .. } = &equip.type_.node {
                if !generic_args.is_empty() {
                    continue;
                }
            }

            if let ast::Type::Named { name: type_name, .. } = &equip.type_.node {
                for method in &equip.items {
                    let method_def = &method.node;
                    let mangled = format!("{}__{}", type_name.node, method_def.name.node);

                    let ret_type = ctx.type_mapper.map_ast_type(&method_def.return_type.node);
                    // First param is self (pointer to the type)
                    let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                    let self_ptr_type = ctx.register_ptr_type(self_type_id);

                    let mut param_types = vec![self_ptr_type];
                    for p in &method_def.params {
                        param_types.push(ctx.type_mapper.map_ast_type(&p.node.type_.node));
                    }

                    ctx.fn_sigs.insert(mangled, (param_types, ret_type));
                }
            }
        }
    }

    // Register monomorphized equip method signatures
    generic_collector.register_equip_sigs(&ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs);

    // P2.5: Register trait equip method signatures
    traits::register_trait_equip_sigs(&mut ctx, &trait_info, ast_module);

    // Lower all non-generic functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // Generic functions are lowered as monomorphized instances
            }
            lower_function(&mut ctx, &mut module, func);
        }
    }

    // Lower monomorphized generic function instances
    for (base_name, type_args, mangled_name) in generic_collector.function_instances() {
        if let Some(template) = generic_collector.get_fn_template(base_name) {
            functions::lower_generic_function(
                &mut ctx,
                &mut module,
                template,
                type_args,
                mangled_name,
            );
        }
    }

    // Lower non-generic equip blocks as functions
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            if equip.generic_params.is_some() || equip.trait_.is_some() {
                continue;
            }
            // Skip equip blocks on generic types
            if let ast::Type::Named { generic_args, .. } = &equip.type_.node {
                if !generic_args.is_empty() {
                    continue;
                }
            }

            if let ast::Type::Named { name: type_name, .. } = &equip.type_.node {
                for method in &equip.items {
                    functions::lower_equip_method(
                        &mut ctx,
                        &mut module,
                        &method.node,
                        &type_name.node,
                        &equip.type_.node,
                    );
                }
            }
        }
    }

    // Lower monomorphized equip method instances
    for (base_name, type_args, mangled_type_name) in generic_collector.equip_instances() {
        if let Some(equip_blocks) = generic_collector.get_equip_templates(base_name) {
            let equip_blocks = equip_blocks.clone();
            for equip in &equip_blocks {
                functions::lower_generic_equip_methods(
                    &mut ctx,
                    &mut module,
                    equip,
                    type_args,
                    mangled_type_name,
                );
            }
        }
    }

    // P2.5: Lower trait equip methods and emit vtable globals
    traits::lower_trait_equip_methods(&mut ctx, &mut module, &trait_info, ast_module);
    traits::emit_vtable_globals(&mut module, &trait_info, ast_module);

    // P2.4: Emit lifted closure call functions
    let closures = std::mem::take(&mut ctx.closures);
    for lifted in &closures.lifted {
        let func = closures::emit_closure_call_function(&mut ctx, lifted);
        module.functions.push(func);
    }

    // Move type_registry back to module for validation
    module.type_registry = std::mem::take(&mut ctx.type_registry);

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

    #[test]
    fn lower_struct_type() {
        let source = r#"struct Point:
    int x
    int y

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify the struct type was registered
        let point_def = gir.type_registry.get_type_def("Point").unwrap();
        assert_eq!(point_def.name, "Point");
        if let TypeDefKind::Struct(ref s) = point_def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "x");
            assert_eq!(s.fields[0].type_id, I64_TYPE);
            assert_eq!(s.fields[1].name, "y");
            assert_eq!(s.fields[1].type_id, I64_TYPE);
        } else {
            panic!("Expected Struct TypeDef for Point");
        }
    }

    #[test]
    fn lower_enum_type() {
        let source = r#"enum Color:
    Red()
    Green()
    Blue()

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let color_def = gir.type_registry.get_type_def("Color").unwrap();
        assert_eq!(color_def.name, "Color");
        if let TypeDefKind::Enum(ref e) = color_def.kind {
            assert_eq!(e.variants.len(), 3);
            assert_eq!(e.variants[0].name, "Red");
            assert_eq!(e.variants[1].name, "Green");
            assert_eq!(e.variants[2].name, "Blue");
        } else {
            panic!("Expected Enum TypeDef for Color");
        }
    }

    #[test]
    fn lower_struct_construction_and_field_access() {
        let source = r#"struct Point:
    int x
    int y

void main():
    Point p = Point(10, 20)
    int px = p.x
    print("{px}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let main = gir.find_function("main").unwrap();
        // Should have StructInit and FieldLoad instructions
        let has_struct_init = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::StructInit { type_name, .. } if type_name == "Point")
            })
        });
        assert!(has_struct_init, "main should have a StructInit for Point");

        let has_field_load = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::FieldLoad { field: 0, .. })
            })
        });
        assert!(has_field_load, "main should have a FieldLoad for field 0 (x)");
    }

    #[test]
    fn lower_enum_construction() {
        let source = r#"enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let main = gir.find_function("main").unwrap();
        let has_enum_init = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::EnumInit { type_name, variant, .. }
                    if type_name == "Color" && variant == "Red")
            })
        });
        assert!(has_enum_init, "main should have an EnumInit for Color::Red");
    }

    #[test]
    fn lower_struct_c_output() {
        let source = r#"struct Point:
    int x
    int y

void main():
    Point p = Point(10, 20)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);
        let c_code = crate::backend::c::generate_c(&gir);

        assert!(c_code.contains("typedef struct Point Point;"), "Should forward-declare Point");
        assert!(c_code.contains("struct Point {"), "Should define Point struct");
        assert!(c_code.contains("int64_t x;"), "Should have field x");
        assert!(c_code.contains("int64_t y;"), "Should have field y");
        assert!(c_code.contains("(Point){"), "Should construct Point");
    }

    #[test]
    fn lower_equip_method() {
        let source = r#"struct Point:
    int x
    int y

equip Point:
    int sum(self):
        return self.x + self.y

void main():
    Point p = Point(3, 4)
    int s = p.sum()
    print("{s}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have a mangled method function
        let sum_fn = gir.find_function("Point__sum");
        assert!(sum_fn.is_some(), "Should have Point__sum function");
        let sum_fn = sum_fn.unwrap();
        // First param is self (pointer to Point)
        assert!(sum_fn.params.len() >= 1, "Point__sum should have at least self param");
    }

    // ── P2.3: Generic Monomorphization Tests ──

    #[test]
    fn lower_generic_struct_type() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](10, 3.14)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have a monomorphized TypeDef for Pair__int64_t__double
        let pair_def = gir.type_registry.get_type_def("Pair__int64_t__double");
        assert!(pair_def.is_some(), "Should have monomorphized Pair__int64_t__double TypeDef");
        let pair_def = pair_def.unwrap();
        if let TypeDefKind::Struct(ref s) = pair_def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "first");
            assert_eq!(s.fields[0].type_id, I64_TYPE);
            assert_eq!(s.fields[1].name, "second");
            assert_eq!(s.fields[1].type_id, F64_TYPE);
        } else {
            panic!("Expected Struct TypeDef for monomorphized Pair");
        }
    }

    #[test]
    fn lower_generic_struct_c_output() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](10, 3.14)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);
        let c_code = crate::backend::c::generate_c(&gir);

        assert!(c_code.contains("Pair__int64_t__double"),
            "C output should contain monomorphized Pair type name. C code:\n{c_code}");
    }

    #[test]
    fn lower_generic_function_monomorphized() {
        let source = r#"T identity[T](T x) = x

void main():
    int a = identity[int](42)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have a monomorphized function identity__int64_t
        let fn_name = gir.functions.iter()
            .find(|f| f.name.contains("identity") && f.name.contains("int64_t"));
        assert!(fn_name.is_some(),
            "Should have monomorphized identity__int64_t function. Functions: {:?}",
            gir.functions.iter().map(|f| &f.name).collect::<Vec<_>>());

        let identity_fn = fn_name.unwrap();
        assert_eq!(identity_fn.params.len(), 1, "identity should have 1 param");
        assert_eq!(identity_fn.return_type, I64_TYPE, "identity[int] should return int64");
    }

    #[test]
    fn lower_generic_enum_type() {
        let source = r#"enum Maybe[T]:
    Just(T)
    Nothing()

void main():
    Maybe[int] m = Just(42)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let maybe_def = gir.type_registry.get_type_def("Maybe__int64_t");
        assert!(maybe_def.is_some(), "Should have monomorphized Maybe__int64_t TypeDef");
        let maybe_def = maybe_def.unwrap();
        if let TypeDefKind::Enum(ref e) = maybe_def.kind {
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Just");
            assert_eq!(e.variants[0].fields.len(), 1);
            assert_eq!(e.variants[0].fields[0].type_id, I64_TYPE);
            assert_eq!(e.variants[1].name, "Nothing");
            assert_eq!(e.variants[1].fields.len(), 0);
        } else {
            panic!("Expected Enum TypeDef for monomorphized Maybe");
        }
    }

    #[test]
    fn lower_multiple_generic_instantiations() {
        let source = r#"struct Wrapper[T]:
    T value

void main():
    Wrapper[int] a = Wrapper[int](42)
    Wrapper[float] b = Wrapper[float](3.14)
    Wrapper[bool] c = Wrapper[bool](true)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have three distinct monomorphized types
        assert!(gir.type_registry.get_type_def("Wrapper__int64_t").is_some(),
            "Should have Wrapper__int64_t");
        assert!(gir.type_registry.get_type_def("Wrapper__double").is_some(),
            "Should have Wrapper__double");
        assert!(gir.type_registry.get_type_def("Wrapper__bool").is_some(),
            "Should have Wrapper__bool");
    }

    // ── P2.5: Trait & Vtable Lowering Tests ──

    #[test]
    fn lower_trait_vtable_types() {
        let source = r#"trait Shape:
    float area(self)
    void draw(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14
    void draw(self):
        pass

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify VTable and TraitObj types were created
        assert!(gir.type_registry.has_type_def("Shape_VTable"),
            "Should have Shape_VTable TypeDef");
        assert!(gir.type_registry.has_type_def("Shape_TraitObj"),
            "Should have Shape_TraitObj TypeDef");

        // Verify VTable struct fields
        let vtable_def = gir.type_registry.get_type_def("Shape_VTable").unwrap();
        if let TypeDefKind::Struct(ref s) = vtable_def.kind {
            assert_eq!(s.fields.len(), 2, "VTable should have 2 method slots");
            assert_eq!(s.fields[0].name, "area");
            assert_eq!(s.fields[1].name, "draw");
        } else {
            panic!("Expected Struct TypeDef for Shape_VTable");
        }

        // Verify TraitObj struct fields
        let obj_def = gir.type_registry.get_type_def("Shape_TraitObj").unwrap();
        if let TypeDefKind::Struct(ref s) = obj_def.kind {
            assert_eq!(s.fields.len(), 2, "TraitObj should have data + vtable");
            assert_eq!(s.fields[0].name, "data");
            assert_eq!(s.fields[1].name, "vtable");
        } else {
            panic!("Expected Struct TypeDef for Shape_TraitObj");
        }
    }

    #[test]
    fn lower_trait_equip_methods() {
        let source = r#"trait Shape:
    float area(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify trait impl function exists
        let area_fn = gir.find_function("Shape_for_Circle__area");
        assert!(area_fn.is_some(),
            "Should have Shape_for_Circle__area function. Functions: {:?}",
            gir.functions.iter().map(|f| &f.name).collect::<Vec<_>>());
        let area_fn = area_fn.unwrap();
        // First param is void* self
        assert!(area_fn.params.len() >= 1,
            "Trait impl should have at least self_void param");
    }

    #[test]
    fn lower_trait_vtable_globals() {
        let source = r#"trait Shape:
    float area(self)
    void draw(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14
    void draw(self):
        pass

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify vtable global constant exists
        let vtable_global = gir.globals.iter()
            .find(|g| g.name == "Shape_for_Circle_vtable");
        assert!(vtable_global.is_some(),
            "Should have Shape_for_Circle_vtable global. Globals: {:?}",
            gir.globals.iter().map(|g| &g.name).collect::<Vec<_>>());

        let vg = vtable_global.unwrap();
        if let crate::ir::GlobalInit::Struct { type_name, fields } = &vg.init {
            assert_eq!(type_name, "Shape_VTable");
            assert_eq!(fields.len(), 2, "VTable should have 2 method entries");
            assert_eq!(fields[0].0, "area");
            assert_eq!(fields[1].0, "draw");
            // Verify FnRef entries point to correct functions
            if let crate::ir::GlobalInit::FnRef(fn_name) = &fields[0].1 {
                assert_eq!(fn_name, "Shape_for_Circle__area");
            } else {
                panic!("Expected FnRef for area slot");
            }
        } else {
            panic!("Expected Struct initializer for vtable global");
        }
    }

    #[test]
    fn lower_trait_c_output() {
        let source = r#"trait Shape:
    float area(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);
        let c_code = crate::backend::c::generate_c(&gir);

        // VTable type
        assert!(c_code.contains("typedef struct Shape_VTable Shape_VTable;"),
            "Should forward-declare Shape_VTable. C code:\n{c_code}");
        assert!(c_code.contains("struct Shape_VTable {"),
            "Should define Shape_VTable struct. C code:\n{c_code}");
        // Function pointer field: `double (*area)(const void*)`
        assert!(c_code.contains("(*area)"),
            "Should have function pointer field 'area'. C code:\n{c_code}");

        // TraitObj type
        assert!(c_code.contains("typedef struct Shape_TraitObj Shape_TraitObj;"),
            "Should forward-declare Shape_TraitObj. C code:\n{c_code}");

        // VTable global constant
        assert!(c_code.contains("Shape_for_Circle_vtable"),
            "Should emit vtable global constant. C code:\n{c_code}");

        // Trait impl function
        assert!(c_code.contains("Shape_for_Circle__area"),
            "Should emit trait impl function. C code:\n{c_code}");
    }

    // ── P2.4: Closure Tests ──

    #[test]
    fn lower_closure_unit_test() {
        // Unit test: manually verify closure lowering creates struct and call fn
        use crate::ir::instructions::Operand;
        use crate::ir::lowering::closures::ClosureLowering;
        use crate::ir::builder::FunctionBuilder;
        use crate::ir::lowering::types::TypeMapper;
        use crate::parser::ast::{ClosureParam, Ownership, PrimitiveType};
        use crate::span::Spanned;

        let analysis = Box::leak(Box::new(empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);
        let mut ctx = crate::ir::lowering::context::LoweringContext::new(analysis, mapper, reg);
        ctx.register_local("x", LocalId(1), I64_TYPE);

        let mut closures = ClosureLowering::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let params = vec![Spanned::dummy(ClosureParam {
            type_: Some(Spanned::dummy(ast::Type::Primitive(PrimitiveType::Int))),
            ownership: Ownership::Borrow,
            name: Spanned::dummy("y".to_string()),
        })];

        let body = Spanned::dummy(ast::Expr::BinaryOp {
            left: Box::new(Spanned::dummy(ast::Expr::Identifier("x".to_string()))),
            op: ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(ast::Expr::Identifier("y".to_string()))),
        });

        let operand = closures.lower_closure(&mut ctx, &mut builder, &params, &body, false);
        assert!(matches!(operand, Operand::Copy(_)), "Should return a Copy operand");

        // Verify struct type was created
        let type_def = ctx.type_registry.get_type_def("__Closure_0");
        assert!(type_def.is_some(), "Should have __Closure_0 TypeDef");
        let td = type_def.unwrap();
        if let TypeDefKind::Struct(ref s) = td.kind {
            assert_eq!(s.fields.len(), 1, "Should capture 'x'");
            assert_eq!(s.fields[0].name, "x");
        } else {
            panic!("Expected struct TypeDef");
        }

        // Verify call function was registered
        assert!(ctx.fn_sigs.contains_key("__Closure_0__call"),
            "Should have registered __Closure_0__call signature");

        // Verify lifted closure is stored
        assert_eq!(closures.lifted.len(), 1);
        assert_eq!(closures.lifted[0].struct_type_name, "__Closure_0");
        assert_eq!(closures.lifted[0].captures.len(), 1);
        assert_eq!(closures.lifted[0].captures[0].name, "x");

        // Emit the call function
        let func = super::closures::emit_closure_call_function(&mut ctx, &closures.lifted[0]);
        assert_eq!(func.name, "__Closure_0__call");
        assert!(func.params.len() >= 2, "Should have env + y params");
    }
}
