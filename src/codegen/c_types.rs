/// Mapping from Vyper types to C type strings.
use crate::parser::ast::PrimitiveType;
use crate::semantic::ids::{DefId, TypeId};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::{ResolvedType, TypeTable};

/// Convert a Vyper PrimitiveType to its C representation.
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
            if let Some(def_id) = scopes.lookup(&name.node) {
                let def = scopes.get_def(def_id);
                match def.kind {
                    crate::semantic::scope::DefKind::Struct => name.node.clone(),
                    crate::semantic::scope::DefKind::Enum => name.node.clone(),
                    _ => name.node.clone(),
                }
            } else if !generic_args.is_empty() {
                // Generic collection types → VyperArray
                match name.node.as_str() {
                    "Vector" | "List" | "Array" | "Set" => "VyperArray".to_string(),
                    _ => "void*".to_string(),
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
        crate::parser::ast::Type::Ref { inner } => {
            let inner_c = ast_type_to_c(&inner.node, scopes);
            format!("const {inner_c}*")
        }
        crate::parser::ast::Type::Tuple(_) => "/* tuple */ void*".to_string(),
        crate::parser::ast::Type::Function { .. } => "/* fn ptr */ void*".to_string(),
        crate::parser::ast::Type::Slice { .. } => "/* slice */ void*".to_string(),
        crate::parser::ast::Type::Dynamic { .. } => "/* dyn */ void*".to_string(),
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
        ResolvedType::Ref(inner) => {
            let inner_c = type_id_to_c(*inner, types, scopes);
            format!("const {inner_c}*")
        }
        ResolvedType::Array(elem, size) => {
            let elem_c = type_id_to_c(*elem, types, scopes);
            format!("{elem_c}[{size}]")
        }
        _ => "/* unsupported */ void*".to_string(),
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
