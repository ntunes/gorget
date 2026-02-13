/// Virtual stdlib module system.
///
/// When the loader encounters `std.*` imports, it generates synthetic
/// `Module` ASTs with `FunctionDef` / `FunctionBody::Declaration` nodes.
/// No filesystem files needed — the C runtime already has the implementations.
///
/// All synthetic defs use `Span::dummy()`, which distinguishes them from
/// user-defined code and enables the `is_stdlib_call()` guard in codegen.
use crate::parser::ast::*;
use crate::span::{Span, Spanned};

/// Check if an import path refers to a stdlib module.
pub fn is_stdlib_module(segments: &[String]) -> bool {
    if segments.first().map(|s| s.as_str()) != Some("std") {
        return false;
    }
    match segments.len() {
        2 => matches!(segments[1].as_str(), "fs" | "path" | "os" | "conv" | "io" | "random" | "time" | "collections" | "math" | "fmt"),
        3 => segments[1] == "test" && segments[2] == "process",
        _ => false,
    }
}

/// Generate a synthetic `Module` for a stdlib module.
pub fn generate_stdlib_module(segments: &[String]) -> Option<Module> {
    if segments.first().map(|s| s.as_str()) != Some("std") {
        return None;
    }
    match segments.len() {
        2 => match segments[1].as_str() {
            "fs" => Some(gen_fs_module()),
            "path" => Some(gen_path_module()),
            "os" => Some(gen_os_module()),
            "conv" => Some(gen_conv_module()),
            "io" => Some(gen_io_module()),
            "random" => Some(gen_random_module()),
            "time" => Some(gen_time_module()),
            "collections" => Some(gen_collections_module()),
            "math" => Some(gen_math_module()),
            "fmt" => Some(gen_fmt_module()),
            _ => None,
        },
        3 => match (segments[1].as_str(), segments[2].as_str()) {
            ("test", "process") => Some(gen_test_process_module()),
            _ => None,
        },
        _ => None,
    }
}

// ─── Module Generators ──────────────────────────────────────

fn gen_fs_module() -> Module {
    make_module(vec![
        decl_fn("read_file", &[("path", ty_str())], ty_str()),
        decl_fn("write_file", &[("path", ty_str()), ("content", ty_str())], ty_void()),
        decl_fn("append_file", &[("path", ty_str()), ("content", ty_str())], ty_void()),
        decl_fn("file_exists", &[("path", ty_str())], ty_bool()),
        decl_fn("delete_file", &[("path", ty_str())], ty_bool()),
    ])
}

fn gen_path_module() -> Module {
    make_module(vec![
        decl_fn("path_join", &[("a", ty_str()), ("b", ty_str())], ty_str()),
        decl_fn("path_parent", &[("path", ty_str())], ty_str()),
        decl_fn("path_basename", &[("path", ty_str())], ty_str()),
        decl_fn("path_extension", &[("path", ty_str())], ty_str()),
        decl_fn("path_stem", &[("path", ty_str())], ty_str()),
    ])
}

fn gen_os_module() -> Module {
    make_module(vec![
        decl_fn("exec", &[("cmd", ty_str())], ty_int()),
        decl_fn("exit", &[("code", ty_int())], ty_void()),
        decl_fn("getenv", &[("name", ty_str())], ty_str()),
        decl_fn("setenv", &[("name", ty_str()), ("value", ty_str())], ty_void()),
        decl_fn("getcwd", &[], ty_str()),
        decl_fn("platform", &[], ty_str()),
        decl_fn("args", &[], ty_vector_str()),
        decl_fn("readdir", &[("path", ty_str())], ty_vector_str()),
    ])
}

fn gen_conv_module() -> Module {
    make_module(vec![
        decl_fn("ord", &[("c", ty_char())], ty_int()),
        decl_fn("chr", &[("n", ty_int())], ty_char()),
        decl_fn("parse_int", &[("s", ty_str())], ty_int()),
        decl_fn("parse_float", &[("s", ty_str())], ty_float()),
        decl_fn("int_to_str", &[("n", ty_int())], ty_str()),
        decl_fn("float_to_str", &[("x", ty_float())], ty_str()),
        decl_fn("bool_to_str", &[("b", ty_bool())], ty_str()),
        decl_fn("char_to_str", &[("c", ty_char())], ty_str()),
    ])
}

fn gen_io_module() -> Module {
    let file_type = Type::Named {
        name: Spanned::dummy("File".to_string()),
        generic_args: vec![],
    };
    let mut items = vec![
        Spanned::dummy(Item::StaticDecl(StaticDecl {
            visibility: Visibility::Public,
            name: Spanned::dummy("stderr".to_string()),
            type_: Spanned::dummy(file_type.clone()),
            value: Spanned::dummy(Expr::IntLiteral(0)), // placeholder — codegen special-cases
            span: Span::dummy(),
        })),
        Spanned::dummy(Item::StaticDecl(StaticDecl {
            visibility: Visibility::Public,
            name: Spanned::dummy("stdout".to_string()),
            type_: Spanned::dummy(file_type),
            value: Spanned::dummy(Expr::IntLiteral(0)), // placeholder
            span: Span::dummy(),
        })),
    ];
    // Add functions
    for name in &["getchar", "term_cols", "term_rows"] {
        items.push(Spanned::dummy(Item::Function(
            decl_fn(name, &[], ty_int()),
        )));
    }
    items.push(Spanned::dummy(Item::Function(
        decl_fn("input", &[("prompt", ty_str())], ty_str()),
    )));
    items.push(Spanned::dummy(Item::Function(
        decl_fn("readline", &[], ty_str()),
    )));
    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_random_module() -> Module {
    make_module(vec![
        decl_fn("rand", &[], ty_int()),
        decl_fn("seed", &[("n", ty_int())], ty_void()),
        decl_fn("rand_range", &[("lo", ty_int()), ("hi", ty_int())], ty_int()),
    ])
}

fn gen_time_module() -> Module {
    make_module(vec![
        decl_fn("time", &[], ty_int()),
        decl_fn("time_ms", &[], ty_int()),
        decl_fn("sleep_ms", &[("ms", ty_int())], ty_void()),
    ])
}

fn gen_math_module() -> Module {
    make_module(vec![
        // Integer math
        decl_fn("abs", &[("x", ty_int())], ty_int()),
        decl_fn("min", &[("a", ty_int()), ("b", ty_int())], ty_int()),
        decl_fn("max", &[("a", ty_int()), ("b", ty_int())], ty_int()),
        // Float math
        decl_fn("sqrt", &[("x", ty_float())], ty_float()),
        decl_fn("pow", &[("base", ty_float()), ("exp", ty_float())], ty_float()),
        decl_fn("floor", &[("x", ty_float())], ty_float()),
        decl_fn("ceil", &[("x", ty_float())], ty_float()),
        decl_fn("round", &[("x", ty_float())], ty_float()),
        decl_fn("log", &[("x", ty_float())], ty_float()),
        decl_fn("log2", &[("x", ty_float())], ty_float()),
        decl_fn("log10", &[("x", ty_float())], ty_float()),
        decl_fn("sin", &[("x", ty_float())], ty_float()),
        decl_fn("cos", &[("x", ty_float())], ty_float()),
        decl_fn("tan", &[("x", ty_float())], ty_float()),
        decl_fn("asin", &[("x", ty_float())], ty_float()),
        decl_fn("acos", &[("x", ty_float())], ty_float()),
        decl_fn("atan", &[("x", ty_float())], ty_float()),
        decl_fn("atan2", &[("y", ty_float()), ("x", ty_float())], ty_float()),
        decl_fn("fabs", &[("x", ty_float())], ty_float()),
        decl_fn("fmin", &[("a", ty_float()), ("b", ty_float())], ty_float()),
        decl_fn("fmax", &[("a", ty_float()), ("b", ty_float())], ty_float()),
    ])
}

fn gen_fmt_module() -> Module {
    // Displayable trait and format() are already in the prelude/builtins.
    // This module exists so `from std.fmt import Displayable` doesn't error.
    make_module(vec![])
}

fn gen_test_process_module() -> Module {
    let process_result_type = Type::Named {
        name: Spanned::dummy("ProcessResult".to_string()),
        generic_args: vec![],
    };
    let struct_def = StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("ProcessResult".to_string()),
        generic_params: None,
        fields: vec![
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("output".to_string()),
                type_: Spanned::dummy(ty_str()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("errors".to_string()),
                type_: Spanned::dummy(ty_str()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("exit_code".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
        ],
        doc_comment: None,
        span: Span::dummy(),
    };
    let items = vec![
        Spanned::dummy(Item::Struct(struct_def)),
        Spanned::dummy(Item::Function(decl_fn("run", &[("cmd", ty_str())], ty_int()))),
        Spanned::dummy(Item::Function(decl_fn("run_output", &[("cmd", ty_str())], process_result_type))),
    ];
    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_collections_module() -> Module {
    let type_defs: Vec<(&str, usize)> = vec![
        ("Vector", 1), ("List", 1), ("Array", 1),   // [T]
        ("Dict", 2), ("HashMap", 2), ("Map", 2),     // [K, V]
        ("Set", 1), ("HashSet", 1),                   // [T]
        ("Box", 1),                                    // [T]
        ("File", 0),                                   // no generics
    ];
    let items = type_defs
        .into_iter()
        .map(|(name, n_params)| {
            Spanned::dummy(Item::Struct(StructDef {
                attributes: vec![],
                visibility: Visibility::Public,
                name: Spanned::dummy(name.to_string()),
                generic_params: if n_params > 0 {
                    Some(Spanned::dummy(GenericParams {
                        params: (0..n_params)
                            .map(|i| {
                                let param_name = if n_params == 2 && i == 0 {
                                    "K"
                                } else if n_params == 2 && i == 1 {
                                    "V"
                                } else {
                                    "T"
                                };
                                Spanned::dummy(GenericParam::Type(Spanned::dummy(
                                    param_name.to_string(),
                                )))
                            })
                            .collect(),
                    }))
                } else {
                    None
                },
                fields: vec![],
                doc_comment: None,
                span: Span::dummy(),
            }))
        })
        .collect();
    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── Helpers ────────────────────────────────────────────────

fn make_module(fns: Vec<FunctionDef>) -> Module {
    let items: Vec<Spanned<Item>> = fns
        .into_iter()
        .map(|f| Spanned::dummy(Item::Function(f)))
        .collect();
    Module {
        items,
        span: Span::dummy(),
    }
}

fn decl_fn(name: &str, params: &[(&str, Type)], ret: Type) -> FunctionDef {
    FunctionDef {
        attributes: Vec::new(),
        visibility: Visibility::Public,
        qualifiers: FunctionQualifiers::default(),
        return_type: Spanned::dummy(ret),
        name: Spanned::dummy(name.to_string()),
        generic_params: None,
        params: params
            .iter()
            .map(|(pname, pty)| {
                Spanned::dummy(Param {
                    type_: Spanned::dummy(pty.clone()),
                    ownership: Ownership::Borrow,
                    name: Spanned::dummy(pname.to_string()),
                    default: None,
                    is_live: false,
                })
            })
            .collect(),
        throws: None,
        where_clause: None,
        body: FunctionBody::Declaration,
        doc_comment: None,
        span: Span::dummy(),
    }
}

fn ty_str() -> Type {
    Type::Primitive(PrimitiveType::Str)
}

fn ty_int() -> Type {
    Type::Primitive(PrimitiveType::Int)
}

fn ty_bool() -> Type {
    Type::Primitive(PrimitiveType::Bool)
}

fn ty_char() -> Type {
    Type::Primitive(PrimitiveType::Char)
}

fn ty_float() -> Type {
    Type::Primitive(PrimitiveType::Float)
}

fn ty_void() -> Type {
    Type::Primitive(PrimitiveType::Void)
}

fn ty_vector_str() -> Type {
    Type::Named {
        name: Spanned::dummy("Vector".to_string()),
        generic_args: vec![Spanned::dummy(ty_str())],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_stdlib() {
        assert!(is_stdlib_module(&["std".into(), "fs".into()]));
        assert!(is_stdlib_module(&["std".into(), "path".into()]));
        assert!(is_stdlib_module(&["std".into(), "os".into()]));
        assert!(is_stdlib_module(&["std".into(), "conv".into()]));
        assert!(is_stdlib_module(&["std".into(), "io".into()]));
        assert!(is_stdlib_module(&["std".into(), "random".into()]));
        assert!(is_stdlib_module(&["std".into(), "time".into()]));
        assert!(is_stdlib_module(&["std".into(), "math".into()]));
        assert!(is_stdlib_module(&["std".into(), "fmt".into()]));
        assert!(is_stdlib_module(&["std".into(), "test".into(), "process".into()]));
        assert!(!is_stdlib_module(&["std".into(), "test".into(), "unknown".into()]));
        assert!(!is_stdlib_module(&["std".into(), "foo".into()]));
        assert!(!is_stdlib_module(&["foo".into(), "fs".into()]));
        assert!(!is_stdlib_module(&["std".into()]));
    }

    #[test]
    fn generate_fs() {
        let m = generate_stdlib_module(&["std".into(), "fs".into()]).unwrap();
        assert_eq!(m.items.len(), 5);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"read_file".to_string()));
        assert!(names.contains(&"write_file".to_string()));
        assert!(names.contains(&"append_file".to_string()));
        assert!(names.contains(&"file_exists".to_string()));
        assert!(names.contains(&"delete_file".to_string()));
    }

    #[test]
    fn generate_conv() {
        let m = generate_stdlib_module(&["std".into(), "conv".into()]).unwrap();
        assert_eq!(m.items.len(), 8);
    }

    #[test]
    fn generate_io() {
        let m = generate_stdlib_module(&["std".into(), "io".into()]).unwrap();
        assert_eq!(m.items.len(), 7); // stderr, stdout, getchar, term_cols, term_rows, input, readline
    }

    #[test]
    fn generate_random() {
        let m = generate_stdlib_module(&["std".into(), "random".into()]).unwrap();
        assert_eq!(m.items.len(), 3);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"rand".to_string()));
        assert!(names.contains(&"seed".to_string()));
        assert!(names.contains(&"rand_range".to_string()));
    }

    #[test]
    fn generate_time() {
        let m = generate_stdlib_module(&["std".into(), "time".into()]).unwrap();
        assert_eq!(m.items.len(), 3);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"time".to_string()));
        assert!(names.contains(&"sleep_ms".to_string()));
        assert!(names.contains(&"time_ms".to_string()));
    }

    #[test]
    fn generate_math() {
        let m = generate_stdlib_module(&["std".into(), "math".into()]).unwrap();
        assert_eq!(m.items.len(), 21);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"abs".to_string()));
        assert!(names.contains(&"sqrt".to_string()));
        assert!(names.contains(&"sin".to_string()));
        assert!(names.contains(&"atan2".to_string()));
        assert!(names.contains(&"fmin".to_string()));
    }

    #[test]
    fn generate_collections() {
        let m = generate_stdlib_module(&["std".into(), "collections".into()]).unwrap();
        assert_eq!(m.items.len(), 10);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Struct(s) => s.name.node.clone(),
            _ => panic!("expected struct"),
        }).collect();
        assert!(names.contains(&"Vector".to_string()));
        assert!(names.contains(&"Dict".to_string()));
        assert!(names.contains(&"Set".to_string()));
        assert!(names.contains(&"Box".to_string()));
        assert!(names.contains(&"File".to_string()));
    }

    #[test]
    fn generate_unknown_returns_none() {
        assert!(generate_stdlib_module(&["std".into(), "foo".into()]).is_none());
    }
}
