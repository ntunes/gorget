use super::*;

/// Helper: parse source, return module. Panics if there are errors.
fn parse(source: &str) -> Module {
    let mut parser = Parser::new(source);
    let module = parser.parse_module();
    if !parser.errors.is_empty() {
        for e in &parser.errors {
            eprintln!("Parse error: {:?}", e);
        }
        panic!("Parser produced {} error(s)", parser.errors.len());
    }
    module
}

/// Helper: parse source, expect errors.
fn parse_with_errors(source: &str) -> (Module, Vec<crate::errors::ParseError>) {
    let mut parser = Parser::new(source);
    let module = parser.parse_module();
    (module, parser.errors)
}

// ── Import ──────────────────────────────────────────────────

#[test]
fn test_from_import() {
    let module = parse("from std.fmt import Displayable\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::Import(ImportStmt::From { path, names, .. })
        if path.len() == 2 && names.len() == 1));
}

#[test]
fn test_from_import_alias() {
    let module = parse("from std.math import sin as msin, cos as mcos\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Import(ImportStmt::From { names, .. }) = &module.items[0].node {
        assert_eq!(names.len(), 2);
        assert_eq!(names[0].name.node, "sin");
        assert_eq!(names[0].alias.as_ref().unwrap().node, "msin");
        assert_eq!(names[1].name.node, "cos");
        assert_eq!(names[1].alias.as_ref().unwrap().node, "mcos");
    } else {
        panic!("expected From import");
    }
}

#[test]
fn test_from_import_mixed_alias() {
    let module = parse("from std.math import sin, cos as mcos\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Import(ImportStmt::From { names, .. }) = &module.items[0].node {
        assert_eq!(names.len(), 2);
        assert_eq!(names[0].name.node, "sin");
        assert!(names[0].alias.is_none());
        assert_eq!(names[1].name.node, "cos");
        assert_eq!(names[1].alias.as_ref().unwrap().node, "mcos");
    } else {
        panic!("expected From import");
    }
}

#[test]
fn test_from_import_wildcard() {
    let module = parse("from std.math import *\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Import(ImportStmt::From { names, wildcard, glob_types, .. }) = &module.items[0].node {
        assert!(*wildcard);
        assert!(names.is_empty());
        assert!(glob_types.is_empty());
    } else {
        panic!("expected From import");
    }
}

#[test]
fn test_simple_import() {
    let module = parse("import std.io\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::Import(ImportStmt::Simple { path, .. })
        if path.len() == 2));
}

#[test]
fn test_grouped_import() {
    let module = parse("import std.sync.{Arc, Mutex}\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::Import(ImportStmt::Grouped { path, names, .. })
        if path.len() == 2 && names.len() == 2));
}

// ── Struct ──────────────────────────────────────────────────

#[test]
fn test_struct_def() {
    let module = parse("struct Point:\n    float x\n    float y\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Struct(ref s) = module.items[0].node {
        assert_eq!(s.name.node, "Point");
        assert_eq!(s.fields.len(), 2);
        assert_eq!(s.fields[0].node.name.node, "x");
        assert_eq!(s.fields[1].node.name.node, "y");
    } else {
        panic!("Expected struct");
    }
}

#[test]
fn test_generic_struct() {
    let module = parse("struct Pair[A, B]:\n    A first\n    B second\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Struct(ref s) = module.items[0].node {
        assert!(s.generic_params.is_some());
        let gp = s.generic_params.as_ref().unwrap();
        assert_eq!(gp.node.params.len(), 2);
    } else {
        panic!("Expected struct");
    }
}

// ── Enum ────────────────────────────────────────────────────

#[test]
fn test_enum_def() {
    let module = parse("enum Color:\n    Red\n    Green\n    Blue\n    Custom(uint8, uint8, uint8)\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Enum(ref e) = module.items[0].node {
        assert_eq!(e.name.node, "Color");
        assert_eq!(e.variants.len(), 4);
        assert!(matches!(e.variants[0].node.fields, VariantFields::Unit));
        assert!(matches!(&e.variants[3].node.fields, VariantFields::Tuple(types) if types.len() == 3));
    } else {
        panic!("Expected enum");
    }
}

// ── Function ────────────────────────────────────────────────

#[test]
fn test_function_with_block() {
    let module = parse("int add(int a, int b):\n    return a + b\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.name.node, "add");
        assert_eq!(f.params.len(), 2);
        assert!(matches!(f.body, FunctionBody::Block(_)));
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_function_expression_body() {
    let module = parse("int double(int x): x * 2\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.name.node, "double");
        assert!(matches!(f.body, FunctionBody::Expression(_)));
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_void_function() {
    let module = parse("void main():\n    pass\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.name.node, "main");
        assert!(matches!(f.return_type.node, Type::Primitive(PrimitiveType::Void)));
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_function_with_throws() {
    let module = parse("int parse_int(String s) throws ValueError:\n    pass\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Function(ref f) = module.items[0].node {
        assert!(f.throws.declares_throws());
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_async_function() {
    let module = parse("async int fetch():\n    pass\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Function(ref f) = module.items[0].node {
        assert!(f.qualifiers.is_async);
    } else {
        panic!("Expected function");
    }
}

// ── Equip Block ──────────────────────────────────────────────

#[test]
fn test_trait_impl() {
    let module = parse("equip Point with Displayable:\n    String to_string(self):\n        return \"point\"\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Equip(ref imp) = module.items[0].node {
        assert!(imp.trait_.is_some());
        assert_eq!(imp.items.len(), 1);
    } else {
        panic!("Expected equip block");
    }
}

#[test]
fn test_inherent_impl() {
    let module = parse("equip Point:\n    float distance(self):\n        return 0.0\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Equip(ref imp) = module.items[0].node {
        assert!(imp.trait_.is_none());
        assert_eq!(imp.items.len(), 1);
    } else {
        panic!("Expected equip block");
    }
}

#[test]
fn test_self_param_variants() {
    // Test bare self (immutable borrow)
    let module = parse("equip Foo:\n    void a(self):\n        pass\n");
    if let Item::Equip(ref imp) = module.items[0].node {
        let param = &imp.items[0].node.params[0].node;
        assert_eq!(param.ownership, Ownership::Borrow);
    } else {
        panic!();
    }

    // Test &self (mutable borrow)
    let module = parse("equip Foo:\n    void b(&self):\n        pass\n");
    if let Item::Equip(ref imp) = module.items[0].node {
        let param = &imp.items[0].node.params[0].node;
        assert_eq!(param.ownership, Ownership::MutableBorrow);
    } else {
        panic!();
    }

    // Test !self (move)
    let module = parse("equip Foo:\n    void c(!self):\n        pass\n");
    if let Item::Equip(ref imp) = module.items[0].node {
        let param = &imp.items[0].node.params[0].node;
        assert_eq!(param.ownership, Ownership::Move);
    } else {
        panic!();
    }
}

// ── Ownership Sigil Self Params ───────────────────────────

#[test]
fn test_mutable_self_param() {
    // &self = mutable borrow
    let module = parse("equip Foo:\n    void b(&self):\n        pass\n");
    if let Item::Equip(ref imp) = module.items[0].node {
        let param = &imp.items[0].node.params[0].node;
        assert_eq!(param.ownership, Ownership::MutableBorrow);
    } else {
        panic!();
    }
}

#[test]
fn test_move_self_param() {
    // !self = consuming/move
    let module = parse("equip Foo:\n    void c(!self):\n        pass\n");
    if let Item::Equip(ref imp) = module.items[0].node {
        let param = &imp.items[0].node.params[0].node;
        assert_eq!(param.ownership, Ownership::Move);
    } else {
        panic!();
    }
}

#[test]
fn test_move_param() {
    // ! sigil on regular param = move
    let module = parse("void take(String !s):\n    pass\n");
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.params[0].node.ownership, Ownership::Move);
    } else {
        panic!();
    }
}

#[test]
fn test_mutable_param() {
    // & sigil on regular param = mutable borrow
    let module = parse("void modify(String &s):\n    pass\n");
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.params[0].node.ownership, Ownership::MutableBorrow);
    } else {
        panic!();
    }
}

// ── Trait ───────────────────────────────────────────────────

#[test]
fn test_trait_def() {
    let module = parse("trait Drawable:\n    void draw(self)\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Trait(ref t) = module.items[0].node {
        assert_eq!(t.name.node, "Drawable");
        assert_eq!(t.items.len(), 1);
    } else {
        panic!("Expected trait");
    }
}

#[test]
fn test_trait_extends() {
    let module = parse("trait Drawable extends Displayable:\n    void draw(self)\n");
    if let Item::Trait(ref t) = module.items[0].node {
        assert_eq!(t.extends.len(), 1);
    } else {
        panic!("Expected trait");
    }
}

// ── Statements ──────────────────────────────────────────────

#[test]
fn test_var_decl() {
    let module = parse("void main():\n    int x = 5\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert_eq!(block.stmts.len(), 1);
            assert!(matches!(&block.stmts[0].node, Stmt::VarDecl { is_const: false, .. }));
        } else {
            panic!("Expected block body");
        }
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_var_decl_with_it_binding_rejected() {
    // `it` is the implicit-closure-parameter keyword: every *read* of `it`
    // parses to `Expr::It`, so a local named `it` would be unreadable.
    // `int it = 5` used to be accepted (commit 089b8e48), producing a silent
    // miscompile (`int it = 42; print(it)` printed garbage). The parser now
    // rejects a keyword in binding-name position with a clear error.
    let (_module, errors) = parse_with_errors("void main():\n    int it = 5\n");
    assert!(!errors.is_empty(), "expected a parse error for `int it = 5`");
    let rendered = errors.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n");
    assert!(
        rendered.contains("reserved keyword") && rendered.contains("it"),
        "expected a 'reserved keyword' error mentioning `it`, got: {rendered}"
    );

    // `mutable` prefix variant is rejected the same way.
    let (_module, errors) = parse_with_errors("void main():\n    mutable int it = 42\n");
    assert!(!errors.is_empty(), "expected a parse error for `mutable int it = 42`");
    let rendered = errors.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n");
    assert!(
        rendered.contains("reserved keyword") && rendered.contains("it"),
        "expected a 'reserved keyword' error mentioning `it`, got: {rendered}"
    );
}

#[test]
fn test_type_path_followed_by_infix_keyword_not_rejected() {
    // Regression guard for the `=`-immediately-after lookahead: a parsed
    // type-path followed by an infix keyword (`as` cast here) is a valid
    // expression statement and must NOT be mis-rejected as a keyword-as-name.
    let (_module, errors) =
        parse_with_errors("void main():\n    int x = 5\n    x as float\n");
    assert!(
        errors.is_empty(),
        "expected no errors for `x as float`, got: {errors:?}"
    );
}

#[test]
fn test_const_var_decl() {
    let module = parse("void main():\n    const int y = 10\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::VarDecl { is_const: true, .. }));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_auto_var_decl() {
    let module = parse("void main():\n    auto name = \"gorget\"\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { ref type_, .. } = block.stmts[0].node {
                assert!(matches!(type_.node, Type::Inferred));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_if_elif_else() {
    let module = parse("void main():\n    if x > 0:\n        pass\n    elif x < 0:\n        pass\n    else:\n        pass\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::If { ref elif_branches, ref else_body, .. } = block.stmts[0].node {
                assert_eq!(elif_branches.len(), 1);
                assert!(else_body.is_some());
            } else {
                panic!("Expected if statement");
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_for_loop() {
    let module = parse("void main():\n    for i in 0..10:\n        pass\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::For { .. }));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_while_loop() {
    let module = parse("void main():\n    while x > 0:\n        x = x - 1\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::While { .. }));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_compound_assignment() {
    let module = parse("void main():\n    x += 1\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::CompoundAssign { op: BinaryOp::Add, .. }));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_return_stmt() {
    let module = parse("int foo():\n    return 42\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::Return(Some(_))));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// ── Expressions ─────────────────────────────────────────────

#[test]
fn test_binary_expr() {
    let module = parse("int foo(): 1 + 2 * 3\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Expression(ref expr) = f.body {
            // Should be Add(1, Mul(2, 3))
            if let Expr::BinaryOp { ref op, ref right, .. } = expr.node {
                assert_eq!(*op, BinaryOp::Add);
                assert!(matches!(&right.node, Expr::BinaryOp { op: BinaryOp::Mul, .. }));
            } else {
                panic!("Expected binary op");
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_method_call() {
    let module = parse("void main():\n    x.foo(1, 2)\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::MethodCall { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_field_access() {
    let module = parse("void main():\n    x.y\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::FieldAccess { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_list_comprehension() {
    let module = parse("void main():\n    auto squares = [x * x for x in 0..10]\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { ref value, .. } = block.stmts[0].node {
                assert!(matches!(&value.node, Expr::ListComprehension { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_optional_chaining() {
    let module = parse("void main():\n    a?.b\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::OptionalChain { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_default_op() {
    let module = parse("void main():\n    auto x = a ?? b\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { ref value, .. } = block.stmts[0].node {
                assert!(matches!(&value.node, Expr::DefaultOp { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// ── Type Alias & Newtype ────────────────────────────────────

#[test]
fn test_type_alias() {
    let module = parse("type StringList = Vector[String]\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::TypeAlias(_)));
}

#[test]
fn test_newtype() {
    let module = parse("newtype UserId(int)\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::Newtype(_)));
}

// ── Attributes ──────────────────────────────────────────────

#[test]
fn test_attribute() {
    let module = parse("@derive(Debuggable)\nstruct Point:\n    int x\n");
    if let Item::Struct(ref s) = module.items[0].node {
        assert_eq!(s.attributes.len(), 1);
        assert_eq!(s.attributes[0].node.name.node, "derive");
    } else {
        panic!();
    }
}

// ── Visibility ──────────────────────────────────────────────

#[test]
fn test_public_visibility() {
    let module = parse("public int add(int a, int b): a + b\n");
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.visibility, Visibility::Public);
    } else {
        panic!();
    }
}

// ── Generic Functions ───────────────────────────────────────

#[test]
fn test_generic_function_with_where() {
    let module = parse("void print_all[Displayable T](Vector[T] items):\n    pass\n");
    if let Item::Function(ref f) = module.items[0].node {
        let gp = f.generic_params.as_ref().expect("generic params");
        assert_eq!(gp.node.params.len(), 1);
        if let GenericParam::Type { name, bounds } = &gp.node.params[0].node {
            assert_eq!(name.node, "T");
            assert_eq!(bounds.len(), 1);
            assert_eq!(bounds[0].node.name.node, "Displayable");
        } else {
            panic!("expected Type param");
        }
    } else {
        panic!();
    }
}

// ── Complex Programs ────────────────────────────────────────

#[test]
fn test_basics_gg() {
    let source = std::fs::read_to_string("examples/basics.gg")
        .expect("Could not read examples/basics.gg");
    let module = parse(&source);
    // 5 items: struct Point, enum Color, add, double, main
    assert_eq!(module.items.len(), 5);
}

// ── Error Recovery ──────────────────────────────────────────

#[test]
fn test_error_recovery() {
    // Parser should recover from errors and continue parsing
    let (module, errors) = parse_with_errors("struct Point:\n    float x\n\n!@#$%\n\nstruct Size:\n    int w\n");
    assert!(!errors.is_empty());
    // Should parse both structs despite the error line between them
    assert_eq!(module.items.len(), 2);
}

#[test]
fn test_recovery_struct_bad_field() {
    // One bad field in a struct shouldn't lose the other fields
    let (module, errors) = parse_with_errors(
        "struct Point:\n    int x\n    !@#$ garbage\n    int z\n"
    );
    assert!(!errors.is_empty());
    assert_eq!(module.items.len(), 1);
    if let Item::Struct(ref s) = module.items[0].node {
        // Should have recovered and parsed x and z
        assert!(s.fields.len() >= 2, "expected at least 2 fields, got {}", s.fields.len());
    } else {
        panic!("Expected struct");
    }
}

#[test]
fn test_recovery_enum_bad_variant() {
    // One bad variant shouldn't lose the others
    let (module, errors) = parse_with_errors(
        "enum Color:\n    Red\n    !@#$\n    Blue\n"
    );
    assert!(!errors.is_empty());
    assert_eq!(module.items.len(), 1);
    if let Item::Enum(ref e) = module.items[0].node {
        assert!(e.variants.len() >= 2, "expected at least 2 variants, got {}", e.variants.len());
    } else {
        panic!("Expected enum");
    }
}

#[test]
fn test_recovery_multiple_top_level_items() {
    // Three items, middle one broken — first and third should survive
    let (module, errors) = parse_with_errors(
        "struct A:\n    int x\n\n!@#$ broken item\n\nstruct C:\n    int z\n"
    );
    assert!(!errors.is_empty());
    assert_eq!(module.items.len(), 2);
}

#[test]
fn test_recovery_error_limit() {
    // Generate many errors — parser should stop after MAX_ERRORS
    let bad_lines = (0..20).map(|_| "!@#$%").collect::<Vec<_>>().join("\n");
    let (_, errors) = parse_with_errors(&bad_lines);
    assert!(errors.len() <= super::MAX_ERRORS, "errors {} should be <= {}", errors.len(), super::MAX_ERRORS);
}

#[test]
fn test_match_stmt() {
    let module = parse(
        "void main():\n    match x:\n        case 1: print(\"one\")\n        case 2: print(\"two\")\n        else:\n            print(\"other\")\n"
    );
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Match { ref arms, ref else_arm, .. } = block.stmts[0].node {
                assert_eq!(arms.len(), 2);
                assert!(else_arm.is_some());
            } else {
                panic!("Expected match stmt");
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_generic_type_in_decl() {
    let module = parse("void main():\n    Vector[int] items = [1, 2, 3]\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { ref type_, .. } = block.stmts[0].node {
                assert!(matches!(type_.node, Type::Named { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_comprehensive_gg() {
    let source = std::fs::read_to_string("examples/comprehensive.gg")
        .expect("Could not read examples/comprehensive.gg");
    let module = parse(&source);
    // Should have many items: type alias, newtype, structs, enums,
    // traits, impl block, functions
    assert!(module.items.len() >= 10);
}

#[test]
fn test_enum_with_keyword_variants() {
    let module = parse("enum Option[T]:\n    Some(T)\n    None\n");
    if let Item::Enum(ref e) = module.items[0].node {
        assert_eq!(e.variants.len(), 2);
        assert_eq!(e.variants[0].node.name.node, "Some");
        assert_eq!(e.variants[1].node.name.node, "None");
    } else {
        panic!();
    }
}

#[test]
fn test_loop_stmt() {
    let module = parse("void main():\n    loop:\n        break\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::Loop { .. }));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_pass_stmt() {
    let module = parse("void main():\n    pass\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            assert!(matches!(&block.stmts[0].node, Stmt::Pass));
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_multiple_items() {
    let module = parse(
        "struct A:\n    int x\n\nstruct B:\n    int y\n\nint add(int a, int b): a + b\n"
    );
    assert_eq!(module.items.len(), 3);
}

#[test]
fn test_nested_blocks() {
    let module = parse(
        "void main():\n    if true:\n        if true:\n            pass\n"
    );
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::If { ref then_body, .. } = block.stmts[0].node {
                assert!(matches!(&then_body.stmts[0].node, Stmt::If { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_string_interpolation_in_expr() {
    let module = parse("void main():\n    print(\"hello {name}\")\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::Call { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_doc_comments() {
    let module = parse("#/ Documentation for the function\nint foo(): 42\n");
    if let Item::Function(ref f) = module.items[0].node {
        assert!(f.doc_comment.is_some());
    } else {
        panic!();
    }
}

#[test]
fn test_empty_module() {
    let module = parse("");
    assert_eq!(module.items.len(), 0);
}

#[test]
fn test_module_with_only_comments() {
    let module = parse("# This is a comment\n# Another comment\n");
    assert_eq!(module.items.len(), 0);
}

// ── Phase 8: Generic Call Parsing ─────────────────────────

#[test]
fn test_parse_generic_method_call() {
    let module = parse("void main():\n    x.convert[str]()\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                if let Expr::MethodCall { ref method, ref generic_args, .. } = expr.node {
                    assert_eq!(method.node, "convert");
                    assert!(generic_args.is_some());
                    let args = generic_args.as_ref().unwrap();
                    assert_eq!(args.len(), 1);
                } else {
                    panic!("Expected MethodCall");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_parse_generic_function_call() {
    let module = parse("void main():\n    max[int](a, b)\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                if let Expr::Call { ref generic_args, ref args, .. } = expr.node {
                    assert!(generic_args.is_some());
                    let type_args = generic_args.as_ref().unwrap();
                    assert_eq!(type_args.len(), 1);
                    assert_eq!(args.len(), 2);
                } else {
                    panic!("Expected Call with generic args");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// ── Implicit `it` in closures ──────────────────────────────

#[test]
fn test_implicit_it_wraps_in_method_call() {
    // `some.map(it * 2)` → MethodCall with an ImplicitClosure arg
    let module = parse("void main():\n    some.map(it * 2)\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                if let Expr::MethodCall { ref args, .. } = expr.node {
                    assert_eq!(args.len(), 1);
                    assert!(
                        matches!(&args[0].node.value.node, Expr::ImplicitClosure { .. }),
                        "Expected ImplicitClosure, got {:?}",
                        args[0].node.value.node
                    );
                } else {
                    panic!("Expected MethodCall");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_implicit_it_not_wrapped_in_explicit_closure() {
    // `(x): it + x` — `it` inside an explicit closure should NOT
    // produce ImplicitClosure at the arg level (it's just a free reference).
    let module = parse("void main():\n    some.map((int x): it + x)\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                if let Expr::MethodCall { ref args, .. } = expr.node {
                    assert_eq!(args.len(), 1);
                    assert!(
                        matches!(&args[0].node.value.node, Expr::Closure { .. }),
                        "Expected Closure (not ImplicitClosure), got {:?}",
                        args[0].node.value.node
                    );
                } else {
                    panic!("Expected MethodCall");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

#[test]
fn test_implicit_it_nested_call() {
    // `some.and_then(Some(it + 1))` → single ImplicitClosure wrapping `Some(it + 1)`
    let module = parse("void main():\n    some.and_then(Some(it + 1))\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                if let Expr::MethodCall { ref args, .. } = expr.node {
                    assert_eq!(args.len(), 1);
                    if let Expr::ImplicitClosure { ref body } = args[0].node.value.node {
                        // Body should be a Call to Some, not another ImplicitClosure
                        assert!(
                            matches!(&body.node, Expr::Call { .. }),
                            "Expected Call inside ImplicitClosure, got {:?}",
                            body.node
                        );
                    } else {
                        panic!(
                            "Expected ImplicitClosure, got {:?}",
                            args[0].node.value.node
                        );
                    }
                } else {
                    panic!("Expected MethodCall");
                }
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// ── Directive ──────────────────────────────────────────────

#[test]
fn test_directive_strip_asserts() {
    let module = parse("directive strip-asserts\nvoid main():\n    pass\n");
    assert_eq!(module.items.len(), 2);
    if let Item::Directive(ref d) = module.items[0].node {
        assert_eq!(d.name, "strip-asserts");
        assert_eq!(d.value, None);
    } else {
        panic!("Expected Directive, got {:?}", module.items[0].node);
    }
}

#[test]
fn test_parse_index_still_works() {
    // Regression: arr[0] should still produce Index, not a generic call
    let module = parse("void main():\n    arr[0]\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::Expr(ref expr) = block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::Index { .. }));
            } else {
                panic!();
            }
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// ── Test Blocks ──────────────────────────────────────────────

#[test]
fn test_parse_test_block() {
    let module = parse("test \"basic math\":\n    assert 1 + 1 == 2\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Test(ref t) = module.items[0].node {
        assert_eq!(t.name.node, "basic math");
        assert_eq!(t.body.stmts.len(), 1);
    } else {
        panic!("Expected Test, got {:?}", module.items[0].node);
    }
}

#[test]
fn test_parse_test_with_tag() {
    let module = parse("@tag(\"smoke\")\ntest \"tagged test\":\n    assert true\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Test(ref t) = module.items[0].node {
        assert_eq!(t.name.node, "tagged test");
        assert_eq!(t.attributes.len(), 1);
        assert_eq!(t.attributes[0].node.name.node, "tag");
    } else {
        panic!("Expected Test, got {:?}", module.items[0].node);
    }
}


#[test]
fn test_parse_suite_setup() {
    let module = parse("suite setup:\n    print(\"setup\")\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::SuiteSetup(_)));
}

#[test]
fn test_parse_suite_teardown() {
    let module = parse("suite teardown:\n    print(\"done\")\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::SuiteTeardown(_)));
}

#[test]
fn test_parse_multiple_tests() {
    let module = parse("test \"a\":\n    assert true\n\ntest \"b\":\n    assert false\n");
    assert_eq!(module.items.len(), 2);
    assert!(matches!(&module.items[0].node, Item::Test(_)));
    assert!(matches!(&module.items[1].node, Item::Test(_)));
}

// ── Extern function defs ──────────────────────────────────

#[test]
fn test_extern_function_def() {
    let module = parse("extern int abs(int x) = \"abs\"\n");
    assert_eq!(module.items.len(), 1);
    if let Item::Function(ref f) = module.items[0].node {
        assert_eq!(f.name.node, "abs");
        if let FunctionBody::Extern(ref sym) = f.body {
            assert_eq!(sym.node, "abs");
        } else {
            panic!("Expected FunctionBody::Extern, got {:?}", f.body);
        }
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_extern_method_in_equip() {
    let source = "\
struct Foo:
    int x

equip Foo:
    extern int value(self) = \"foo_get_value\"
";
    let module = parse(source);
    assert_eq!(module.items.len(), 2);
    if let Item::Equip(ref eq) = module.items[1].node {
        assert_eq!(eq.items.len(), 1);
        let method = &eq.items[0].node;
        assert_eq!(method.name.node, "value");
        if let FunctionBody::Extern(ref sym) = method.body {
            assert_eq!(sym.node, "foo_get_value");
        } else {
            panic!("Expected FunctionBody::Extern, got {:?}", method.body);
        }
    } else {
        panic!("Expected equip block");
    }
}

#[test]
fn test_extern_block_still_works() {
    let source = "extern \"C\":\n    int printf(str fmt)\n";
    let module = parse(source);
    assert_eq!(module.items.len(), 1);
    assert!(matches!(module.items[0].node, Item::ExternBlock(_)));
}

// ── Meta ──────────────────────────────────────────────────

#[test]
fn test_meta_const() {
    let module = parse("meta int MAX = 1024\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::MetaConst(_)));
    if let Item::MetaConst(mc) = &module.items[0].node {
        assert_eq!(mc.name.node, "MAX");
    }
}

#[test]
fn test_meta_type_alias() {
    let module = parse("meta type Vec = Vector[int]\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::MetaType(_)));
    if let Item::MetaType(mt) = &module.items[0].node {
        assert_eq!(mt.name.node, "Vec");
        assert!(matches!(&mt.rhs, MetaTypeRhs::Plain(_)));
    }
}

#[test]
fn test_meta_type_func() {
    let source = "meta type sized_int(int bits):\n    return int\n";
    let module = parse(source);
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::MetaTypeFunc(_)));
    if let Item::MetaTypeFunc(mtf) = &module.items[0].node {
        assert_eq!(mtf.name.node, "sized_int");
        assert_eq!(mtf.params.len(), 1);
    }
}

#[test]
fn test_meta_type_conditional() {
    let source = "meta type Map = Dict if true else HashMap\n";
    let module = parse(source);
    assert_eq!(module.items.len(), 1);
    if let Item::MetaType(mt) = &module.items[0].node {
        assert_eq!(mt.name.node, "Map");
        assert!(matches!(&mt.rhs, MetaTypeRhs::Conditional { .. }));
        if let MetaTypeRhs::Conditional { then_type, else_type, .. } = &mt.rhs {
            assert!(matches!(&then_type.node, Type::Named { name, .. } if name.node == "Dict"));
            assert!(matches!(&else_type.node, Type::Named { name, .. } if name.node == "HashMap"));
        }
    } else {
        panic!("expected MetaType");
    }
}

#[test]
fn test_meta_type_call() {
    let source = "meta type Word = sized_int(8)\n";
    let module = parse(source);
    assert_eq!(module.items.len(), 1);
    if let Item::MetaType(mt) = &module.items[0].node {
        assert_eq!(mt.name.node, "Word");
        assert!(matches!(&mt.rhs, MetaTypeRhs::Call { .. }));
        if let MetaTypeRhs::Call { callee, args } = &mt.rhs {
            assert_eq!(callee.node, "sized_int");
            assert_eq!(args.len(), 1);
        }
    } else {
        panic!("expected MetaType");
    }
}

#[test]
fn test_meta_assert() {
    let module = parse("meta assert true, \"ok\"\n");
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::MetaAssert(_)));
    if let Item::MetaAssert(ma) = &module.items[0].node {
        assert!(ma.message.is_some());
    }
}

#[test]
fn test_meta_assert_no_message() {
    let module = parse("meta assert true\n");
    assert_eq!(module.items.len(), 1);
    if let Item::MetaAssert(ma) = &module.items[0].node {
        assert!(ma.message.is_none());
    } else {
        panic!("expected MetaAssert");
    }
}

#[test]
fn test_meta_if() {
    let source = "meta if true:\n    void f():\n        pass\n";
    let module = parse(source);
    assert_eq!(module.items.len(), 1);
    assert!(matches!(&module.items[0].node, Item::MetaIf(_)));
    if let Item::MetaIf(mi) = &module.items[0].node {
        assert_eq!(mi.then_items.len(), 1);
        assert!(matches!(&mi.then_items[0].node, Item::Function(_)));
        assert!(mi.elif_branches.is_empty());
        assert!(mi.else_branch.is_none());
    }
}

#[test]
fn test_meta_if_elif_else() {
    let source = "\
meta if true:
    void f():
        pass
elif false:
    void g():
        pass
else:
    void h():
        pass
";
    let module = parse(source);
    assert_eq!(module.items.len(), 1);
    if let Item::MetaIf(mi) = &module.items[0].node {
        assert_eq!(mi.then_items.len(), 1);
        assert_eq!(mi.elif_branches.len(), 1);
        assert!(mi.else_branch.is_some());
        assert_eq!(mi.else_branch.as_ref().unwrap().1.len(), 1);
    } else {
        panic!("expected MetaIf");
    }
}

// ── Dot-shorthand ─────────────────────────────────────────────

#[test]
fn test_dot_shorthand_expr_unit() {
    // .Red() as an expression
    let source = "void f():\n    auto x = .Red()\n";
    let module = parse(source);
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::VarDecl { value, .. } = &block.stmts[0].node {
                assert!(matches!(&value.node, Expr::DotShorthand { variant, args }
                    if variant.node == "Red" && args.is_empty()));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_dot_shorthand_expr_with_args() {
    // .Blue(42) as an expression
    let source = "void f():\n    auto x = .Blue(42)\n";
    let module = parse(source);
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::VarDecl { value, .. } = &block.stmts[0].node {
                assert!(matches!(&value.node, Expr::DotShorthand { variant, args }
                    if variant.node == "Blue" && args.len() == 1));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_dot_shorthand_pattern_unit() {
    // case .Red(): in a match
    let source = "void f():\n    match x:\n        case .Red():\n            pass\n";
    let module = parse(source);
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::Match { arms, .. } = &block.stmts[0].node {
                if let Some(arm) = arms[0].arm() {
                    assert!(matches!(&arm.pattern.node, Pattern::DotShorthand { variant, fields }
                        if variant.node == "Red" && fields.is_empty()));
                    return;
                }
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_dot_shorthand_pattern_with_binding() {
    // case .Blue(n): in a match
    let source = "void f():\n    match x:\n        case .Blue(n):\n            pass\n";
    let module = parse(source);
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::Match { arms, .. } = &block.stmts[0].node {
                if let Some(arm) = arms[0].arm() {
                    assert!(matches!(&arm.pattern.node, Pattern::DotShorthand { variant, fields }
                        if variant.node == "Blue" && fields.len() == 1));
                    return;
                }
            }
        }
    }
    panic!("unexpected AST shape");
}

// ── Bare tuple syntax ──────────────────────────────────────────

#[test]
fn test_bare_tuple_return_type() {
    // `str, int f():` should parse as return type `(str, int)`
    let module = parse("str, int f():\n    pass\n");
    if let Item::Function(f) = &module.items[0].node {
        assert!(matches!(&f.return_type.node, Type::Tuple(types) if types.len() == 2));
        return;
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_bare_tuple_return_type_three() {
    // Three-element bare tuple return type
    let module = parse("str, int, bool parse_header():\n    pass\n");
    if let Item::Function(f) = &module.items[0].node {
        assert!(matches!(&f.return_type.node, Type::Tuple(types) if types.len() == 3));
        return;
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_bare_tuple_return_stmt() {
    // `return a, b` should parse as TupleLiteral
    let module = parse("void f():\n    return a, b\n");
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::Return(Some(expr)) = &block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::TupleLiteral(elems) if elems.len() == 2));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_bare_tuple_return_stmt_three() {
    // Three-element bare tuple return
    let module = parse("void f():\n    return x, y, z\n");
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::Return(Some(expr)) = &block.stmts[0].node {
                assert!(matches!(&expr.node, Expr::TupleLiteral(elems) if elems.len() == 3));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_bare_tuple_auto_destructure() {
    // `auto a, b = f()` should parse pattern as Tuple([Binding("a"), Binding("b")])
    let module = parse("void g():\n    auto a, b = f()\n");
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::VarDecl { pattern, .. } = &block.stmts[0].node {
                assert!(matches!(&pattern.node, Pattern::Tuple(pats) if pats.len() == 2));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_bare_tuple_auto_destructure_wildcard() {
    // `auto a, _, c = f()` with wildcard
    let module = parse("void g():\n    auto a, _, c = f()\n");
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::VarDecl { pattern, .. } = &block.stmts[0].node {
                if let Pattern::Tuple(pats) = &pattern.node {
                    assert_eq!(pats.len(), 3);
                    assert!(matches!(&pats[1].node, Pattern::Wildcard));
                    return;
                }
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_bare_tuple_for_loop() {
    // `for x, y in items:` should parse pattern as Tuple
    let module = parse("void g():\n    for x, y in items:\n        pass\n");
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::For { pattern, .. } = &block.stmts[0].node {
                assert!(matches!(&pattern.node, Pattern::Tuple(pats) if pats.len() == 2));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_parenthesized_tuple_return_type_still_works() {
    // Parenthesized form must still parse correctly (regression)
    let module = parse("(str, int) f():\n    pass\n");
    if let Item::Function(f) = &module.items[0].node {
        assert!(matches!(&f.return_type.node, Type::Tuple(types) if types.len() == 2));
        return;
    }
    panic!("unexpected AST shape");
}

#[test]
fn test_parenthesized_tuple_auto_still_works() {
    // Parenthesized auto destructuring must still parse correctly (regression)
    let module = parse("void g():\n    auto (a, b) = f()\n");
    if let Item::Function(f) = &module.items[0].node {
        if let FunctionBody::Block(block) = &f.body {
            if let Stmt::VarDecl { pattern, .. } = &block.stmts[0].node {
                assert!(matches!(&pattern.node, Pattern::Tuple(pats) if pats.len() == 2));
                return;
            }
        }
    }
    panic!("unexpected AST shape");
}

// ── shared keyword ───────────────────────────────────────────

#[test]
fn test_shared_var_decl() {
    let module = parse("void main():\n    shared int count = 0\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { shared, type_, .. } = &block.stmts[0].node {
                assert_eq!(shared, &SharedKind::Auto);
                assert!(matches!(&type_.node, Type::Primitive(PrimitiveType::Int)));
                return;
            }
        }
    }
    panic!("expected shared VarDecl");
}

#[test]
fn test_shared_rwlock_var_decl() {
    let module = parse("void main():\n    shared(rwlock) int cache = 0\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { shared, .. } = &block.stmts[0].node {
                assert_eq!(shared, &SharedKind::RwLock);
                return;
            }
        }
    }
    panic!("expected shared(rwlock) VarDecl");
}

#[test]
fn test_shared_atomic_var_decl() {
    let module = parse("void main():\n    shared(atomic) int flags = 0\n");
    if let Item::Function(ref f) = module.items[0].node {
        if let FunctionBody::Block(ref block) = f.body {
            if let Stmt::VarDecl { shared, .. } = &block.stmts[0].node {
                assert_eq!(shared, &SharedKind::Atomic);
                return;
            }
        }
    }
    panic!("expected shared(atomic) VarDecl");
}

#[test]
fn test_paren_expr_before_colon_not_closure() {
    // (A == B or C == D) before a colon must parse as a grouped expression, not a closure.
    // Regression: looks_like_closure() used to fire here, causing a confusing error.
    let source = "void f(bool cond, int a, int b, int c, int d):\n    if cond and (a == b or c == d):\n        print(1)\n";
    parse(source); // must not panic
}

// ══════════════════════════════════════════════════════════════
// D29: postfix `!` (visible error propagation) + `!=` corners
// ══════════════════════════════════════════════════════════════

/// Parse `int g() throws E:\n    return <expr>\n` and return the return expr.
fn d29_tail(expr_src: &str) -> Expr {
    let src = format!("int g() throws E:\n    return {expr_src}\n");
    let m = parse(&src);
    for it in &m.items {
        if let Item::Function(f) = &it.node {
            if let FunctionBody::Block(b) = &f.body {
                if let Some(Spanned { node: Stmt::Return(Some(e)), .. }) = b.stmts.last() {
                    return e.node.clone();
                }
            }
        }
    }
    panic!("no tail expr for `{expr_src}`");
}

#[test]
fn d29_plain_propagate() {
    // f()! => Propagate(Call)
    assert!(matches!(d29_tail("f()!"),
        Expr::Propagate { expr } if matches!(expr.node, Expr::Call { .. })));
}

#[test]
fn d29_method_propagate() {
    // a.m()! => Propagate(MethodCall)
    assert!(matches!(d29_tail("a.m()!"),
        Expr::Propagate { expr } if matches!(expr.node, Expr::MethodCall { .. })));
}

#[test]
fn d29_nested_calls_each_marked() {
    // g(f()!)! => Propagate(Call(g, [Propagate(Call(f))]))
    let e = d29_tail("g(f()!)!");
    let inner = match e {
        Expr::Propagate { expr } => *expr,
        _ => panic!("outer not Propagate"),
    };
    let arg0 = match &inner.node {
        Expr::Call { args, .. } => args[0].node.value.node.clone(),
        _ => panic!("inner not Call"),
    };
    assert!(matches!(arg0, Expr::Propagate { .. }), "arg not marked: {arg0:?}");
}

#[test]
fn d29_chain_left_to_right() {
    // f()!.m()! => Propagate(MethodCall(Propagate(Call(f)).m))
    let e = d29_tail("f()!.m()!");
    let mcall = match e {
        Expr::Propagate { expr } => *expr,
        _ => panic!("outer not Propagate"),
    };
    let recv = match &mcall.node {
        Expr::MethodCall { receiver, .. } => receiver.node.clone(),
        _ => panic!("not MethodCall"),
    };
    assert!(matches!(recv, Expr::Propagate { .. }), "receiver not Propagate: {recv:?}");
}

#[test]
fn d29_maximal_munch_neq_is_not_propagate() {
    // a()!=b  => Neq(Call(a), b)  — `!=` lexes as one token; NO Propagate.
    let e = d29_tail("a()!=b");
    match e {
        Expr::BinaryOp { op, left, .. } => {
            assert_eq!(op, BinaryOp::Neq);
            assert!(matches!(left.node, Expr::Call { .. }), "lhs should be bare Call, not Propagate");
        }
        other => panic!("expected Neq comparison, got {other:?}"),
    }
}

#[test]
fn d29_neq_with_space_before_bang_is_still_neq() {
    // f()!= b  => Neq(Call(f), b) — the `!=` still fuses (space is AFTER `!=`).
    let e = d29_tail("f()!= b");
    match e {
        Expr::BinaryOp { op, left, .. } => {
            assert_eq!(op, BinaryOp::Neq);
            assert!(matches!(left.node, Expr::Call { .. }));
        }
        other => panic!("expected Neq, got {other:?}"),
    }
}

#[test]
fn d29_propagate_then_eqeq_with_space() {
    // f()! == b => Eq(Propagate(Call(f)), b) — a space frees `!` as postfix.
    let e = d29_tail("f()! == b");
    match e {
        Expr::BinaryOp { op, left, .. } => {
            assert_eq!(op, BinaryOp::Eq);
            assert!(matches!(left.node, Expr::Propagate { .. }), "lhs should be Propagate");
        }
        other => panic!("expected Eq(Propagate,..), got {other:?}"),
    }
}

#[test]
fn d29_catch_attaches_to_marked_expr() {
    // f()! catch (e): 0  => Catch { expr: Propagate(Call(f)), .. }
    let e = d29_tail("f()! catch (e): 0");
    match e {
        Expr::Catch { expr, .. } => {
            assert!(matches!(expr.node, Expr::Propagate { .. }),
                "catch inner should be Propagate, got {:?}", expr.node);
        }
        other => panic!("expected Catch(Propagate,..), got {other:?}"),
    }
}

#[test]
fn d29_rethrow_attaches_to_marked_expr() {
    // f()! rethrow 1 => Rethrow { expr: Propagate(Call(f)), .. } (bare form)
    let e = d29_tail("f()! rethrow 1");
    match e {
        Expr::Rethrow { expr, .. } => {
            assert!(matches!(expr.node, Expr::Propagate { .. }),
                "rethrow inner should be Propagate, got {:?}", expr.node);
        }
        other => panic!("expected Rethrow(Propagate,..), got {other:?}"),
    }
}

#[test]
fn d29_signature_bare_bang_parses_as_inferred() {
    // int f()!:  parses to ThrowsSpec::Inferred (A31 reservation; NOT a
    // `Named("!inferred")` string sentinel — a typed axis).
    let m = parse("int f()!:\n    return 1\n");
    let f = m.items.iter().find_map(|it| match &it.node {
        Item::Function(f) => Some(f), _ => None,
    }).expect("no fn");
    assert!(matches!(&f.throws, ThrowsSpec::Inferred(_)),
        "bare `!:` should populate ThrowsSpec::Inferred, got {:?}", f.throws);
    assert!(f.throws.declares_throws());
    assert!(f.throws.explicit_type().is_none());
}

#[test]
fn d29_signature_bang_type_is_rejected() {
    // int f() ! E:  is NOT a valid signature form (cancelled) — parse error.
    let (_m, errs) = parse_with_errors("int f() ! E:\n    return 1\n");
    assert!(!errs.is_empty(), "`! E` signature should be a parse error");
}

#[test]
fn d29_legacy_throws_signature_unchanged() {
    // int f() throws E:  still parses to an explicit throws.
    let m = parse("int f() throws E:\n    return 1\n");
    let f = m.items.iter().find_map(|it| match &it.node {
        Item::Function(f) => Some(f), _ => None,
    }).expect("no fn");
    let t = f.throws.explicit_type().expect("throws clause");
    assert!(matches!(&t.node, Type::Named { name, .. } if name.node == "E"));
}

// ── `Block` position fields: the two Core #14 pins ───────────
//
// `Block` carries two source positions and they mean DIFFERENT lines. Both are
// write-site facts nothing downstream can recover, and each has a consumer that
// silently does the wrong thing when the value drifts — so each gets a pin
// rather than a comment.
//
//   * `header_start` is on the owning construct's FIRST line. The formatter's
//     orphan-pre-close flush compares a comment's column against the INDENT of
//     that line; seeded from the colon instead, a WRAPPED header puts it on a
//     continuation line indented at or past the body, and the column test then
//     refuses every comment written inside the block.
//   * `span.start` is on the header's LAST line (the colon's, at the sites that
//     have one). The blank/lookback logic walks BACK from it, so a value on the
//     BODY's own line starts every such walk one line below its target.
//
// The assertions are the PROPERTIES those consumers depend on, not per-probe
// line numbers: an expected-line table has to be re-derived by hand for every
// clause in every probe, and the first thing it does when a probe grows a
// clause is go wrong.

/// One collected block, flattened to what these pins assert. Owned values: the
/// visitor trait cannot hand out references that outlive its walk.
///
/// Deliberately NOT recording `Block::layout`: every probe below spells an
/// INDENTED suite, so there is nothing to filter, and reading the author's
/// suite spelling outside the formatter is what
/// `suite_layout_is_read_only_by_the_formatter` exists to stop. If a probe ever
/// grows an inline suite, these assertions fail loudly on it rather than
/// quietly skipping it.
struct BlockProbe {
    header_start: usize,
    span_start: usize,
    first_stmt_start: Option<usize>,
}

/// Collect every `Block` reachable from a module.
fn collect_blocks(module: &Module) -> Vec<BlockProbe> {
    use crate::parser::visitor::{walk_block, ExprVisitor};

    struct C {
        out: Vec<BlockProbe>,
    }
    impl ExprVisitor for C {
        fn visit_block(&mut self, block: &Block) {
            self.out.push(BlockProbe {
                header_start: block.header_start,
                span_start: block.span.start,
                first_stmt_start: block.stmts.first().map(|s| s.span.start),
            });
            walk_block(self, block);
        }
    }

    let mut c = C { out: Vec::new() };
    for item in &module.items {
        let body = match &item.node {
            Item::Function(f) => match &f.body {
                FunctionBody::Block(b) => Some(b),
                _ => None,
            },
            Item::Test(t) => Some(&t.body),
            Item::Bench(b) => Some(&b.body),
            Item::SuiteSetup(x) => Some(&x.body),
            Item::SuiteTeardown(x) => Some(&x.body),
            Item::MetaTypeFunc(m) => Some(&m.body),
            _ => None,
        };
        if let Some(b) = body {
            c.visit_block(b);
        }
    }
    c.out
}

/// 1-based source line number of `pos`.
fn line_no_of(src: &str, pos: usize) -> usize {
    src[..pos.min(src.len())].matches('\n').count() + 1
}

/// Leading-space count of the source line containing `pos`.
fn line_indent_of(src: &str, pos: usize) -> usize {
    let ls = src[..pos.min(src.len())].rfind('\n').map(|i| i + 1).unwrap_or(0);
    src[ls..].chars().take_while(|c| *c == ' ').count()
}

/// The probe corpus: one source per block-carrying construct, WRAPPED wherever
/// wrapping is legal, since a wrapped header is the only shape where the two
/// positions land on different lines.
const BLOCK_PROBES: &[(&str, &str)] = &[
    ("fn, WRAPPED signature", "int f(int a_long_one,\n      int b_long_one):\n    return a_long_one\n"),
    ("if / elif / else, WRAPPED conditions", "void f(int i):\n    if (i <\n        3):\n        print(1)\n    elif (i >\n        9):\n        print(2)\n    else:\n        print(3)\n"),
    ("while, WRAPPED condition", "void f(int i):\n    while (i <\n        3):\n        print(i)\n"),
    ("for + for-else", "void f():\n    for i in [1]:\n        print(i)\n    else:\n        print(0)\n"),
    ("while + while-else", "void f():\n    while false:\n        print(1)\n    else:\n        print(0)\n"),
    ("loop", "void f():\n    loop:\n        break\n"),
    ("unsafe", "void f():\n    unsafe:\n        print(1)\n"),
    ("with … as …", "void f(int r):\n    with r as held:\n        print(held)\n"),
    ("named scope", "void f():\n    cleanup:\n        print(1)\n"),
    ("on error", "void f():\n    on error:\n        print(1)\n"),
    ("do:", "void f():\n    int d = do:\n        1\n    print(d)\n"),
    ("match arm", "void f(int x):\n    match x:\n        case 1:\n            print(1)\n"),
    // The DISCRIMINATING `else:` probe: the else BODY's header is the `else`
    // clause line, not the `match` line — which is what keeps an arms-level
    // tail beside a match `else:` claimed at the MATCH level instead of being
    // pulled inside the else body.
    ("match + else", "void f(int x):\n    match x:\n        case 1:\n            print(1)\n        else:\n            print(0)\n"),
    ("select arm + else", "void f():\n    select:\n        case int v = c().recv():\n            print(v)\n        else:\n            print(0)\n"),
    ("closure body", "void f():\n    Callable[void()] g = ():\n        print(1)\n    g()\n"),
    ("catch", "void f(int x):\n    int a = fallible(x) catch (e):\n        print(1)\n        0\n"),
    ("rethrow", "int f(int x) throws String:\n    int a = fallible(x) rethrow (String e):\n        print(1)\n        e\n    return a\n"),
    ("meta if / elif / else", "void f[T]():\n    meta if bitwidth(T) > 4096:\n        print(1)\n    elif bitwidth(T) > 2048:\n        print(2)\n    else:\n        print(3)\n"),
    ("meta for", "void f():\n    meta for i in 0..2:\n        print(i)\n"),
    // WRAPPED, and it has to be: an unwrapped `meta while cond:` has its colon
    // on the `meta` line, so a colon-seeded `header_start` has the same indent
    // as the right answer and the probe cannot fail. Wrapping is what separates
    // the construct's FIRST line from the colon's.
    ("meta while, WRAPPED condition", "void f[T]():\n    meta while (bitwidth(T) >\n            128):\n        print(1)\n"),
    ("meta match arm, WRAPPED case expr", "void f[T]():\n    meta match typename(T):\n        case (\"i\" +\n                \"nt\"):\n            print(1)\n"),
    ("meta match + else", "void f[T]():\n    meta match typename(T):\n        case \"int\":\n            print(1)\n        else:\n            print(0)\n"),
    ("test body", "test \"t\":\n    print(1)\n"),
    ("bench body", "bench \"b\":\n    print(1)\n"),
    ("suite setup / teardown", "suite setup:\n    print(1)\n\nsuite teardown:\n    print(2)\n"),
    ("meta type fn", "meta type W(Type t):\n    return t\n"),
];

/// `Block::header_start` sits on a line LESS INDENTED than the block's body —
/// which is exactly what the formatter's column-based membership test needs,
/// and exactly what a colon-seeded value loses on a wrapped header.
#[test]
fn block_header_start_is_on_the_constructs_first_line() {
    for (label, src) in BLOCK_PROBES {
        let module = parse(src);
        let blocks = collect_blocks(&module);
        assert!(!blocks.is_empty(), "{label}: no blocks collected");
        for b in &blocks {
            let Some(first) = b.first_stmt_start else { continue };
            let header_indent = line_indent_of(src, b.header_start);
            let body_indent = line_indent_of(src, first);
            assert!(
                header_indent < body_indent,
                "{label}: `header_start` (line {}, indent {header_indent}) is not \
                 LESS indented than the block body (line {}, indent {body_indent}). \
                 A position at or past the body's indent is a CONTINUATION line of \
                 a wrapped header, and the formatter's orphan-pre-close flush — \
                 whose rule is 'indented past the header line' — then refuses every \
                 comment written inside the block.\nin:\n{src}",
                line_no_of(src, b.header_start),
                line_no_of(src, first),
            );
            assert!(
                b.header_start <= b.span_start,
                "{label}: `header_start` is AFTER `span.start`. The construct \
                 begins at or before the introducer that opens its body.\nin:\n{src}"
            );
        }
    }
}

/// `Block::span.start` sits STRICTLY ABOVE the block's first statement — i.e.
/// on the header's last line, not on the body's own.
///
/// The walk-BACK consumers (the author-blank probe above a clause, the comments
/// that lead it) start from this position, so a value on the body's line puts
/// them one line past everything they are looking for. `meta while` is the site
/// that got this wrong: it took its span AFTER consuming the colon, which is the
/// NEWLINE token at the start of the body's line.
#[test]
fn block_span_start_is_on_the_headers_last_line() {
    for (label, src) in BLOCK_PROBES {
        let module = parse(src);
        for b in collect_blocks(&module) {
            let Some(first) = b.first_stmt_start else { continue };
            assert!(
                line_no_of(src, b.span_start) < line_no_of(src, first),
                "{label}: `Block.span.start` is on line {}, the same line as the \
                 block's first statement ({}). It must be on the header's LAST \
                 line; every walk-BACK consumer starts one line below its target \
                 otherwise.\nin:\n{src}",
                line_no_of(src, b.span_start),
                line_no_of(src, first),
            );
        }
    }
}
