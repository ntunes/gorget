/// Mapping from Gorget types to C type strings.
use crate::parser::ast::PrimitiveType;
use crate::semantic::ids::{DefId, TypeId};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::{ResolvedType, TypeTable};

/// Convert a Gorget PrimitiveType to its C representation.
pub fn primitive_to_c(prim: PrimitiveType) -> &'static str {
    match prim {
        PrimitiveType::Void => "void",
        PrimitiveType::Bool => "bool",
        PrimitiveType::Int => "int64_t",
        PrimitiveType::Int8 => "int8_t",
        PrimitiveType::Int16 => "int16_t",
        PrimitiveType::Int32 => "int32_t",
        PrimitiveType::Int64 => "int64_t",
        PrimitiveType::Uint => "uint64_t",
        PrimitiveType::Uint8 => "uint8_t",
        PrimitiveType::Uint16 => "uint16_t",
        PrimitiveType::Uint32 => "uint32_t",
        PrimitiveType::Uint64 => "uint64_t",
        PrimitiveType::Float => "double",
        PrimitiveType::Float32 => "float",
        PrimitiveType::Float64 => "double",
        PrimitiveType::Char => "char",
        PrimitiveType::Str | PrimitiveType::StringType => "const char*",
    }
}

/// Convert an AST Type to C type string (used for declarations where we have the AST type directly).
pub fn ast_type_to_c(ty: &crate::parser::ast::Type, scopes: &ScopeTable) -> String {
    match ty {
        crate::parser::ast::Type::Primitive(prim) => primitive_to_c(*prim).to_string(),
        crate::parser::ast::Type::Named { name, generic_args } => {
            if !generic_args.is_empty() {
                match name.node.as_str() {
                    "Vector" | "List" | "Array" => "GorgetArray".to_string(),
                    "Set" => "GorgetSet".to_string(),
                    "Dict" | "Map" | "HashMap" => {
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| ast_type_to_c(&a.node, scopes))
                            .collect();
                        super::c_mangle::mangle_generic("GorgetMap", &c_args)
                    }
                    "Box" if generic_args.len() == 1 => {
                        // Box[Trait] → Trait_TraitObj (automatic dispatch)
                        if let crate::parser::ast::Type::Named { name: inner_name, generic_args: inner_args } = &generic_args[0].node {
                            if inner_args.is_empty() {
                                if let Some(def_id) = scopes.lookup(&inner_name.node) {
                                    if scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Trait {
                                        return super::c_mangle::mangle_trait_obj(&inner_name.node);
                                    }
                                }
                            }
                        }
                        // Box[T] → T* (regular box)
                        let inner = ast_type_to_c(&generic_args[0].node, scopes);
                        format!("{inner}*")
                    }
                    _ => {
                        // User-defined generic type → mangled name
                        let c_args: Vec<String> = generic_args
                            .iter()
                            .map(|a| ast_type_to_c(&a.node, scopes))
                            .collect();
                        super::c_mangle::mangle_generic(&name.node, &c_args)
                    }
                }
            } else if name.node == "File" {
                return "GorgetFile".to_string();
            } else {
                name.node.clone()
            }
        }
        crate::parser::ast::Type::Array { element, size } => {
            let elem_c = ast_type_to_c(&element.node, scopes);
            let size_val = match &size.node {
                crate::parser::ast::Expr::IntLiteral(n) => format!("{n}"),
                _ => "0".to_string(),
            };
            format!("{elem_c}[{size_val}]")
        }
        crate::parser::ast::Type::Tuple(fields) => {
            let c_field_types: Vec<String> = fields
                .iter()
                .map(|f| ast_type_to_c(&f.node, scopes))
                .collect();
            super::c_mangle::mangle_tuple(&c_field_types)
        }
        crate::parser::ast::Type::Function { return_type, params } => {
            let ret = ast_type_to_c(&return_type.node, scopes);
            let param_types: Vec<String> = params
                .iter()
                .map(|p| ast_type_to_c(&p.node, scopes))
                .collect();
            let params_str = if param_types.is_empty() {
                "void".to_string()
            } else {
                param_types.join(", ")
            };
            format!("{ret} (*)({params_str})")
        }
        crate::parser::ast::Type::Slice { element } => {
            let elem_c = ast_type_to_c(&element.node, scopes);
            format!("struct {{ const {elem_c}* data; size_t len; }}")
        }
        crate::parser::ast::Type::SelfType => "/* Self */".to_string(),
        crate::parser::ast::Type::Inferred => "/* auto */".to_string(),
    }
}

/// Convert a resolved TypeId to C type string.
pub fn type_id_to_c(type_id: TypeId, types: &TypeTable, scopes: &ScopeTable) -> String {
    match types.get(type_id) {
        ResolvedType::Primitive(prim) => primitive_to_c(*prim).to_string(),
        ResolvedType::Void => "void".to_string(),
        ResolvedType::Defined(def_id) => def_name_to_c(*def_id, scopes),
        ResolvedType::Error => "/* error */ int64_t".to_string(),
        ResolvedType::Never => "/* never */ void".to_string(),
        ResolvedType::Array(elem, size) => {
            let elem_c = type_id_to_c(*elem, types, scopes);
            format!("{elem_c}[{size}]")
        }
        ResolvedType::Tuple(fields) => {
            let c_field_types: Vec<String> = fields
                .iter()
                .map(|tid| type_id_to_c(*tid, types, scopes))
                .collect();
            super::c_mangle::mangle_tuple(&c_field_types)
        }
        ResolvedType::Function { params, return_type } => {
            let ret = type_id_to_c(*return_type, types, scopes);
            let param_types: Vec<String> = params
                .iter()
                .map(|tid| type_id_to_c(*tid, types, scopes))
                .collect();
            let params_str = if param_types.is_empty() {
                "void".to_string()
            } else {
                param_types.join(", ")
            };
            format!("{ret} (*)({params_str})")
        }
        ResolvedType::Slice(elem) => {
            let elem_c = type_id_to_c(*elem, types, scopes);
            format!("struct {{ const {elem_c}* data; size_t len; }}")
        }
        ResolvedType::Generic(def_id, args) => {
            let base = def_name_to_c(*def_id, scopes);
            match base.as_str() {
                "Vector" | "List" | "Array" => "GorgetArray".to_string(),
                "Set" => "GorgetSet".to_string(),
                "Dict" | "Map" | "HashMap" => {
                    let c_args: Vec<String> = args
                        .iter()
                        .map(|tid| type_id_to_c(*tid, types, scopes))
                        .collect();
                    super::c_mangle::mangle_generic("GorgetMap", &c_args)
                }
                "Box" if args.len() == 1 => {
                    let inner = type_id_to_c(args[0], types, scopes);
                    format!("{inner}*")
                }
                _ => {
                    let c_args: Vec<String> = args
                        .iter()
                        .map(|tid| type_id_to_c(*tid, types, scopes))
                        .collect();
                    super::c_mangle::mangle_generic(&base, &c_args)
                }
            }
        }
        ResolvedType::TraitObject(def_id) => {
            let name = def_name_to_c(*def_id, scopes);
            super::c_mangle::mangle_trait_obj(&name)
        }
        ResolvedType::Var(_) => {
            // Type variable should not escape inference
            "/* type var */ void*".to_string()
        }
    }
}

/// Get the C type name for a defined type (struct/enum).
pub(super) fn def_name_to_c(def_id: DefId, scopes: &ScopeTable) -> String {
    let name = scopes.get_def(def_id).name.clone();
    match name.as_str() {
        "File" => "GorgetFile".to_string(),
        _ => name,
    }
}

/// Produce a valid C declaration by splicing `name` into the type string.
/// For function pointer types like `int64_t (*)(int64_t)`, this inserts the
/// name inside the `(*)` to produce `int64_t (*name)(int64_t)`.
/// For plain types, returns `"{c_type} {name}"`.
pub fn c_declare(c_type: &str, name: &str) -> String {
    if let Some(pos) = c_type.find("(*)") {
        format!("{}(*{}){}", &c_type[..pos], name, &c_type[pos + 3..])
    } else {
        format!("{c_type} {name}")
    }
}

/// Get the printf format specifier for a primitive type.
/// Returns a simple format specifier that can be embedded in a format string.
pub fn printf_format_for_primitive(prim: PrimitiveType) -> &'static str {
    match prim {
        PrimitiveType::Int | PrimitiveType::Int64 => "%lld",
        PrimitiveType::Int8 | PrimitiveType::Int16 | PrimitiveType::Int32 => "%d",
        PrimitiveType::Uint | PrimitiveType::Uint64 => "%llu",
        PrimitiveType::Uint8 | PrimitiveType::Uint16 | PrimitiveType::Uint32 => "%u",
        PrimitiveType::Float | PrimitiveType::Float32 | PrimitiveType::Float64 => "%f",
        PrimitiveType::Bool => "%s",
        PrimitiveType::Char => "%c",
        PrimitiveType::Str | PrimitiveType::StringType => "%s",
        PrimitiveType::Void => "",
    }
}

/// Given a C type name (e.g., "int64_t"), return the printf format and argument
/// expression for string interpolation. Used when substituting generic type params.
pub fn format_for_c_type(c_type: &str, expr: &str) -> (String, String) {
    match c_type {
        "int64_t" => ("%lld".to_string(), format!("(long long){expr}")),
        "int8_t" | "int16_t" | "int32_t" => ("%d".to_string(), expr.to_string()),
        "uint64_t" => ("%llu".to_string(), format!("(unsigned long long){expr}")),
        "uint8_t" | "uint16_t" | "uint32_t" => ("%u".to_string(), expr.to_string()),
        "double" | "float" => ("%f".to_string(), expr.to_string()),
        "bool" => ("%s".to_string(), format!("{expr} ? \"true\" : \"false\"")),
        "char" => ("%c".to_string(), expr.to_string()),
        "const char*" => ("%s".to_string(), expr.to_string()),
        _ => ("%lld".to_string(), format!("(long long){expr}")),
    }
}

/// Convert an AST Type to a Gorget-syntax display name for trace output.
pub fn ast_type_to_gorget(ty: &crate::parser::ast::Type) -> String {
    match ty {
        crate::parser::ast::Type::Primitive(prim) => primitive_to_gorget(*prim).to_string(),
        crate::parser::ast::Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                name.node.clone()
            } else {
                let args: Vec<String> = generic_args
                    .iter()
                    .map(|a| ast_type_to_gorget(&a.node))
                    .collect();
                format!("{}[{}]", name.node, args.join(", "))
            }
        }
        crate::parser::ast::Type::Tuple(fields) => {
            let parts: Vec<String> = fields
                .iter()
                .map(|f| ast_type_to_gorget(&f.node))
                .collect();
            format!("({})", parts.join(", "))
        }
        crate::parser::ast::Type::Function { return_type, params } => {
            let ret = ast_type_to_gorget(&return_type.node);
            let ps: Vec<String> = params
                .iter()
                .map(|p| ast_type_to_gorget(&p.node))
                .collect();
            format!("{}({})", ret, ps.join(", "))
        }
        crate::parser::ast::Type::Array { element, size } => {
            let elem = ast_type_to_gorget(&element.node);
            match &size.node {
                crate::parser::ast::Expr::IntLiteral(n) => format!("{elem}[{n}]"),
                _ => format!("{elem}[]"),
            }
        }
        crate::parser::ast::Type::Slice { element } => {
            let elem = ast_type_to_gorget(&element.node);
            format!("{elem}[]")
        }
        crate::parser::ast::Type::SelfType => "Self".to_string(),
        crate::parser::ast::Type::Inferred => "auto".to_string(),
    }
}

/// Convert a Gorget PrimitiveType to its Gorget-syntax display name.
fn primitive_to_gorget(prim: PrimitiveType) -> &'static str {
    match prim {
        PrimitiveType::Void => "void",
        PrimitiveType::Bool => "bool",
        PrimitiveType::Int | PrimitiveType::Int64 => "int",
        PrimitiveType::Int8 => "int8",
        PrimitiveType::Int16 => "int16",
        PrimitiveType::Int32 => "int32",
        PrimitiveType::Uint | PrimitiveType::Uint64 => "uint",
        PrimitiveType::Uint8 => "uint8",
        PrimitiveType::Uint16 => "uint16",
        PrimitiveType::Uint32 => "uint32",
        PrimitiveType::Float | PrimitiveType::Float64 => "float",
        PrimitiveType::Float32 => "float32",
        PrimitiveType::Char => "char",
        PrimitiveType::Str | PrimitiveType::StringType => "str",
    }
}

/// Check if a C type is suitable for variable substitution in trace reports.
/// Only primitive scalar types (int, float, bool, char, string) are traceable;
/// structs, enums, arrays, and void are not.
pub fn is_traceable_for_vars(c_type: &str) -> bool {
    matches!(
        c_type,
        "int64_t"
            | "int8_t"
            | "int16_t"
            | "int32_t"
            | "uint64_t"
            | "uint8_t"
            | "uint16_t"
            | "uint32_t"
            | "double"
            | "float"
            | "bool"
            | "const char*"
            | "char"
    )
}

/// Return the trace runtime formatter function name for a given C type.
/// Used to emit `__gorget_trace_val_TYPE(fp, expr)` calls.
pub fn trace_formatter_for_c_type(c_type: &str) -> &'static str {
    match c_type {
        "int64_t" | "int8_t" | "int16_t" | "int32_t" | "uint64_t" | "uint8_t"
        | "uint16_t" | "uint32_t" => "__gorget_trace_val_int",
        "double" | "float" => "__gorget_trace_val_float",
        "bool" => "__gorget_trace_val_bool",
        "const char*" => "__gorget_trace_val_str",
        "char" => "__gorget_trace_val_char",
        "void" => "__gorget_trace_val_void",
        _ => "__gorget_trace_val_int", // fallback for struct/enum types
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn primitive_mapping() {
        assert_eq!(primitive_to_c(PrimitiveType::Int), "int64_t");
        assert_eq!(primitive_to_c(PrimitiveType::Bool), "bool");
        assert_eq!(primitive_to_c(PrimitiveType::Float), "double");
        assert_eq!(primitive_to_c(PrimitiveType::Char), "char");
        assert_eq!(primitive_to_c(PrimitiveType::Str), "const char*");
        assert_eq!(primitive_to_c(PrimitiveType::Void), "void");
        assert_eq!(primitive_to_c(PrimitiveType::Int8), "int8_t");
        assert_eq!(primitive_to_c(PrimitiveType::Uint64), "uint64_t");
        assert_eq!(primitive_to_c(PrimitiveType::Float32), "float");
    }

    #[test]
    fn c_declare_plain_type() {
        assert_eq!(c_declare("int64_t", "x"), "int64_t x");
        assert_eq!(c_declare("const char*", "s"), "const char* s");
    }

    #[test]
    fn c_declare_function_pointer() {
        assert_eq!(
            c_declare("int64_t (*)(int64_t)", "cb"),
            "int64_t (*cb)(int64_t)"
        );
        assert_eq!(
            c_declare("void (*)(void)", "f"),
            "void (*f)(void)"
        );
        assert_eq!(
            c_declare("int64_t (*)(int64_t, int64_t)", "add"),
            "int64_t (*add)(int64_t, int64_t)"
        );
    }

    #[test]
    fn printf_formats() {
        assert_eq!(printf_format_for_primitive(PrimitiveType::Int), "%lld");
        assert_eq!(printf_format_for_primitive(PrimitiveType::Float), "%f");
        assert_eq!(printf_format_for_primitive(PrimitiveType::Bool), "%s");
        assert_eq!(printf_format_for_primitive(PrimitiveType::Char), "%c");
    }
}
