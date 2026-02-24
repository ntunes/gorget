use crate::ir::types::*;
use crate::parser::ast::{PrimitiveType, Type};

/// Maps AST types to GIR TypeIds.
pub struct TypeMapper {
    /// `str` maps to `Ptr(U8)` in GIR.
    pub str_type: TypeId,
}

impl TypeMapper {
    pub fn new(registry: &mut TypeRegistry) -> Self {
        let str_type = registry.insert(GirType::Ptr(U8_TYPE));
        Self { str_type }
    }

    /// Map an AST `Type` to a GIR `TypeId`.
    pub fn map_ast_type(&self, ty: &Type) -> TypeId {
        match ty {
            Type::Primitive(prim) => self.map_primitive(prim),
            Type::Inferred => panic!("BUG: Inferred type should be resolved before GIR lowering"),
            // Phase 1: only primitives are handled
            _ => UNIT_TYPE,
        }
    }

    /// Map a primitive type to its GIR TypeId.
    pub fn map_primitive(&self, prim: &PrimitiveType) -> TypeId {
        match prim {
            PrimitiveType::Int | PrimitiveType::Int64 => I64_TYPE,
            PrimitiveType::Int8 => I8_TYPE,
            PrimitiveType::Int16 => I16_TYPE,
            PrimitiveType::Int32 => I32_TYPE,
            PrimitiveType::Uint | PrimitiveType::Uint64 => U64_TYPE,
            PrimitiveType::Uint8 => U8_TYPE,
            PrimitiveType::Uint16 => U16_TYPE,
            PrimitiveType::Uint32 => U32_TYPE,
            PrimitiveType::Float | PrimitiveType::Float64 => F64_TYPE,
            PrimitiveType::Float32 => F32_TYPE,
            PrimitiveType::Bool => BOOL_TYPE,
            PrimitiveType::Str | PrimitiveType::CStr => self.str_type,
            PrimitiveType::Char => U32_TYPE, // char as u32 codepoint
            PrimitiveType::StringType => self.str_type, // Phase 1: treat String as str
            PrimitiveType::Void => UNIT_TYPE,
        }
    }

    /// Return the printf format specifier for a GIR type.
    pub fn format_specifier(&self, type_id: TypeId) -> &str {
        if type_id == I64_TYPE || type_id == I32_TYPE || type_id == I16_TYPE || type_id == I8_TYPE {
            "%lld"
        } else if type_id == U64_TYPE || type_id == U32_TYPE || type_id == U16_TYPE || type_id == U8_TYPE {
            "%llu"
        } else if type_id == F64_TYPE || type_id == F32_TYPE {
            "%.17g"
        } else if type_id == BOOL_TYPE {
            "%s"
        } else if type_id == self.str_type {
            "%s"
        } else {
            "%lld" // fallback
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn map_primitives() {
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);

        assert_eq!(mapper.map_primitive(&PrimitiveType::Int), I64_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Float), F64_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Bool), BOOL_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Void), UNIT_TYPE);
        // str maps to a Ptr(U8) type
        let str_id = mapper.map_primitive(&PrimitiveType::Str);
        assert_eq!(str_id, mapper.str_type);
        assert!(matches!(reg.get(str_id), Some(GirType::Ptr(U8_TYPE))));
    }

    #[test]
    fn map_int_variants() {
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);

        assert_eq!(mapper.map_primitive(&PrimitiveType::Int8), I8_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Int16), I16_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Int32), I32_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Int64), I64_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Uint8), U8_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Uint16), U16_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Uint32), U32_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Uint64), U64_TYPE);
        assert_eq!(mapper.map_primitive(&PrimitiveType::Float32), F32_TYPE);
    }

    #[test]
    fn format_specifiers() {
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);

        assert_eq!(mapper.format_specifier(I64_TYPE), "%lld");
        assert_eq!(mapper.format_specifier(F64_TYPE), "%.17g");
        assert_eq!(mapper.format_specifier(mapper.str_type), "%s");
        assert_eq!(mapper.format_specifier(BOOL_TYPE), "%s");
        assert_eq!(mapper.format_specifier(U64_TYPE), "%llu");
    }
}
