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
                    "Dict" | "Map" | "HashMap" => "GorgetMap".to_string(),
                    "Box" if generic_args.len() == 1 => {
                        // Box[dynamic Trait] → TraitObj type
                        if let crate::parser::ast::Type::Dynamic { trait_ } = &generic_args[0].node {
                            if let crate::parser::ast::Type::Named { name: trait_name, .. } = &trait_.node {
                                return super::c_mangle::mangle_trait_obj(&trait_name.node);
                            }
                        }
                        // Box[T] → T*
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
            } else if let Some(def_id) = scopes.lookup(&name.node) {
                let def = scopes.get_def(def_id);
                match def.kind {
                    crate::semantic::scope::DefKind::Struct => name.node.clone(),
                    crate::semantic::scope::DefKind::Enum => name.node.clone(),
                    _ => name.node.clone(),
                }
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
            let field_defs: Vec<String> = fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let ft = ast_type_to_c(&f.node, scopes);
                    format!("{ft} _{i}")
                })
                .collect();
            format!("struct {{ {}; }}", field_defs.join("; "))
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
        crate::parser::ast::Type::Dynamic { trait_ } => {
            // Extract trait name from the inner type
            if let crate::parser::ast::Type::Named { name, .. } = &trait_.node {
                super::c_mangle::mangle_trait_obj(&name.node)
            } else {
                "void*".to_string()
            }
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
            let field_defs: Vec<String> = fields
                .iter()
                .enumerate()
                .map(|(i, tid)| {
                    let ft = type_id_to_c(*tid, types, scopes);
                    format!("{ft} _{i}")
                })
                .collect();
            format!("struct {{ {}; }}", field_defs.join("; "))
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
                "Dict" | "Map" | "HashMap" => "GorgetMap".to_string(),
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
fn def_name_to_c(def_id: DefId, scopes: &ScopeTable) -> String {
    scopes.get_def(def_id).name.clone()
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
    fn printf_formats() {
        assert_eq!(printf_format_for_primitive(PrimitiveType::Int), "%lld");
        assert_eq!(printf_format_for_primitive(PrimitiveType::Float), "%f");
        assert_eq!(printf_format_for_primitive(PrimitiveType::Bool), "%s");
        assert_eq!(printf_format_for_primitive(PrimitiveType::Char), "%c");
    }
}
