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
    if segments.len() != 2 || segments[0] != "std" {
        return false;
    }
    matches!(segments[1].as_str(), "fs" | "path" | "os" | "conv" | "io" | "random" | "time" | "collections")
}

/// Generate a synthetic `Module` for a stdlib module.
pub fn generate_stdlib_module(segments: &[String]) -> Option<Module> {
    if segments.len() != 2 || segments[0] != "std" {
        return None;
    }
    match segments[1].as_str() {
        "fs" => Some(gen_fs_module()),
        "path" => Some(gen_path_module()),
        "os" => Some(gen_os_module()),
        "conv" => Some(gen_conv_module()),
        "io" => Some(gen_io_module()),
        "random" => Some(gen_random_module()),
        "time" => Some(gen_time_module()),
        "collections" => Some(gen_collections_module()),
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
        decl_fn("args", &[], ty_vector_str()),
        decl_fn("readdir", &[("path", ty_str())], ty_vector_str()),
    ])
}

fn gen_conv_module() -> Module {
    make_module(vec![
        decl_fn("ord", &[("c", ty_char())], ty_int()),
        decl_fn("chr", &[("n", ty_int())], ty_char()),
        decl_fn("parse_int", &[("s", ty_str())], ty_int()),
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
    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_random_module() -> Module {
    make_module(vec![
        decl_fn("rand", &[], ty_int()),
        decl_fn("seed", &[("n", ty_int())], ty_void()),
    ])
}

fn gen_time_module() -> Module {
    make_module(vec![
        decl_fn("time", &[], ty_int()),
        decl_fn("sleep_ms", &[("ms", ty_int())], ty_void()),
    ])
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
        assert_eq!(m.items.len(), 3);
    }

    #[test]
    fn generate_io() {
        let m = generate_stdlib_module(&["std".into(), "io".into()]).unwrap();
        assert_eq!(m.items.len(), 5); // stderr, stdout, getchar, term_cols, term_rows
    }

    #[test]
    fn generate_random() {
        let m = generate_stdlib_module(&["std".into(), "random".into()]).unwrap();
        assert_eq!(m.items.len(), 2);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"rand".to_string()));
        assert!(names.contains(&"seed".to_string()));
    }

    #[test]
    fn generate_time() {
        let m = generate_stdlib_module(&["std".into(), "time".into()]).unwrap();
        assert_eq!(m.items.len(), 2);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"time".to_string()));
        assert!(names.contains(&"sleep_ms".to_string()));
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
