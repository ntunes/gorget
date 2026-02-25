use rustc_hash::FxHashMap;

use crate::ir::types::*;
use crate::parser::ast::{self, PrimitiveType, Type};
use crate::span::Spanned;

/// Maps AST types to GIR TypeIds.
pub struct TypeMapper {
    /// `str` maps to `Ptr(U8)` in GIR (Phase 1 compat) — will be Named("Str") later.
    pub str_type: TypeId,
    /// Cache of Named type → GIR TypeId.
    pub named_types: FxHashMap<String, TypeId>,
}

impl TypeMapper {
    pub fn new(registry: &mut TypeRegistry) -> Self {
        let str_type = registry.insert(GirType::Ptr(U8_TYPE));
        Self {
            str_type,
            named_types: FxHashMap::default(),
        }
    }

    /// Map an AST `Type` to a GIR `TypeId`.
    pub fn map_ast_type(&self, ty: &Type) -> TypeId {
        match ty {
            Type::Primitive(prim) => self.map_primitive(prim),
            Type::Inferred => panic!("BUG: Inferred type should be resolved before GIR lowering"),
            Type::Named { name, generic_args } => {
                if !generic_args.is_empty() {
                    // Generic type — look up monomorphized name
                    let mangled = mangle_generic_name(&name.node, generic_args);
                    if let Some(&id) = self.named_types.get(&mangled) {
                        return id;
                    }
                    return UNIT_TYPE; // not yet registered (P2.3 handles this)
                }
                // Non-generic named type
                if let Some(&id) = self.named_types.get(name.node.as_str()) {
                    return id;
                }
                UNIT_TYPE // not yet registered
            }
            Type::Tuple(elems) => {
                if elems.is_empty() {
                    return UNIT_TYPE;
                }
                // Look up anonymous tuple type by mangled name
                let mangled = mangle_tuple_name(elems);
                if let Some(&id) = self.named_types.get(&mangled) {
                    return id;
                }
                UNIT_TYPE // not yet registered
            }
            Type::Function { return_type, params, .. } => {
                // Function pointer type
                let ret = self.map_ast_type(&return_type.node);
                let param_types: Vec<TypeId> = params.iter()
                    .map(|p| self.map_ast_type(&p.node))
                    .collect();
                let _ = (ret, param_types);
                UNIT_TYPE
            }
            _ => UNIT_TYPE,
        }
    }

    /// Mutable version of map_ast_type that can register new types.
    pub fn map_ast_type_mut(&mut self, ty: &Type, registry: &mut TypeRegistry) -> TypeId {
        match ty {
            Type::Named { name, generic_args } => {
                if !generic_args.is_empty() {
                    let mangled = mangle_generic_name(&name.node, generic_args);
                    if let Some(&id) = self.named_types.get(&mangled) {
                        return id;
                    }
                    return UNIT_TYPE;
                }
                if let Some(&id) = self.named_types.get(name.node.as_str()) {
                    return id;
                }
                UNIT_TYPE
            }
            Type::Tuple(elems) => {
                if elems.is_empty() {
                    return UNIT_TYPE;
                }
                let mangled = mangle_tuple_name(elems);
                if let Some(&id) = self.named_types.get(&mangled) {
                    return id;
                }
                // Create the tuple TypeDef on-the-fly
                let fields: Vec<StructField> = elems.iter().enumerate()
                    .map(|(i, elem)| {
                        let field_type = self.map_ast_type_mut(&elem.node, registry);
                        StructField {
                            name: format!("_{i}"),
                            type_id: field_type,
                        }
                    })
                    .collect();
                let type_def = TypeDef {
                    name: mangled.clone(),
                    kind: TypeDefKind::Struct(StructDef { fields }),
                    metadata: TypeMetadata::default(),
                };
                registry.add_type_def(type_def);
                let type_id = registry.insert(GirType::Named(mangled.clone()));
                self.named_types.insert(mangled, type_id);
                type_id
            }
            _ => self.map_ast_type(ty),
        }
    }

    /// Register a named type that has already been added to the TypeRegistry.
    pub fn register_named(&mut self, name: String, type_id: TypeId) {
        self.named_types.insert(name, type_id);
    }

    /// Look up a named type's GIR TypeId.
    pub fn lookup_named(&self, name: &str) -> Option<TypeId> {
        self.named_types.get(name).copied()
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

/// Register a user-defined struct from AST into the TypeRegistry and TypeMapper.
pub fn register_struct_type(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    struct_def: &ast::StructDef,
) {
    let name = &struct_def.name.node;

    // Skip generic structs — they'll be monomorphized in P2.3
    if struct_def.generic_params.is_some() {
        return;
    }

    // Already registered?
    if mapper.named_types.contains_key(name.as_str()) {
        return;
    }

    // Map fields
    let fields: Vec<StructField> = struct_def.fields.iter()
        .map(|f| {
            let field_type = mapper.map_ast_type(&f.node.type_.node);
            StructField {
                name: f.node.name.node.clone(),
                type_id: field_type,
            }
        })
        .collect();

    let type_def = TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields }),
        metadata: TypeMetadata::default(), // Copy semantics by default
    };

    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(name.clone()));
    mapper.named_types.insert(name.clone(), type_id);
}

/// Register a user-defined enum from AST into the TypeRegistry and TypeMapper.
pub fn register_enum_type(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    enum_def: &ast::EnumDef,
) {
    let name = &enum_def.name.node;

    // Skip generic enums — they'll be monomorphized in P2.3
    if enum_def.generic_params.is_some() {
        return;
    }

    // Already registered?
    if mapper.named_types.contains_key(name.as_str()) {
        return;
    }

    // Map variants
    let variants: Vec<EnumVariant> = enum_def.variants.iter()
        .map(|v| {
            let fields = match &v.node.fields {
                ast::VariantFields::Unit => vec![],
                ast::VariantFields::Tuple(types) => {
                    types.iter().enumerate()
                        .map(|(i, t)| {
                            let field_type = mapper.map_ast_type(&t.node);
                            StructField {
                                name: format!("_{i}"),
                                type_id: field_type,
                            }
                        })
                        .collect()
                }
            };
            EnumVariant {
                name: v.node.name.node.clone(),
                fields,
            }
        })
        .collect();

    let type_def = TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Enum(EnumDef { variants }),
        metadata: TypeMetadata::default(),
    };

    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(name.clone()));
    mapper.named_types.insert(name.clone(), type_id);
}

/// Mangle a generic name: `Vector[int]` → `Vector__int64_t`.
pub fn mangle_generic_name(base: &str, args: &[Spanned<Type>]) -> String {
    let mut result = base.to_string();
    for arg in args {
        result.push_str("__");
        result.push_str(&mangle_type_for_name(&arg.node));
    }
    result
}

/// Mangle a tuple type name: `(int, float)` → `Tuple__int64_t__double`.
fn mangle_tuple_name(elems: &[Spanned<Type>]) -> String {
    let mut result = "Tuple".to_string();
    for elem in elems {
        result.push_str("__");
        result.push_str(&mangle_type_for_name(&elem.node));
    }
    result
}

/// Produce a C-compatible name fragment for a type (used in name mangling).
fn mangle_type_for_name(ty: &Type) -> String {
    match ty {
        Type::Primitive(prim) => match prim {
            PrimitiveType::Int | PrimitiveType::Int64 => "int64_t".to_string(),
            PrimitiveType::Int8 => "int8_t".to_string(),
            PrimitiveType::Int16 => "int16_t".to_string(),
            PrimitiveType::Int32 => "int32_t".to_string(),
            PrimitiveType::Uint | PrimitiveType::Uint64 => "uint64_t".to_string(),
            PrimitiveType::Uint8 => "uint8_t".to_string(),
            PrimitiveType::Uint16 => "uint16_t".to_string(),
            PrimitiveType::Uint32 => "uint32_t".to_string(),
            PrimitiveType::Float | PrimitiveType::Float64 => "double".to_string(),
            PrimitiveType::Float32 => "float".to_string(),
            PrimitiveType::Bool => "bool".to_string(),
            PrimitiveType::Str => "Str".to_string(),
            PrimitiveType::CStr => "cstr".to_string(),
            PrimitiveType::StringType => "GorgetString".to_string(),
            PrimitiveType::Char => "uint32_t".to_string(),
            PrimitiveType::Void => "void".to_string(),
        },
        Type::Named { name, generic_args } => {
            if !generic_args.is_empty() {
                return mangle_generic_name(&name.node, generic_args);
            }
            name.node.clone()
        }
        Type::Tuple(elems) => mangle_tuple_name(elems),
        _ => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned { node, span: Span { start: 0, end: 0 } }
    }

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

    #[test]
    fn map_named_type() {
        let mut reg = TypeRegistry::new();
        let mut mapper = TypeMapper::new(&mut reg);

        // Register a named type
        let point_id = reg.insert(GirType::Named("Point".to_string()));
        mapper.register_named("Point".to_string(), point_id);

        let ty = Type::Named {
            name: spanned("Point".to_string()),
            generic_args: vec![],
        };
        assert_eq!(mapper.map_ast_type(&ty), point_id);
    }

    #[test]
    fn map_unknown_named_type() {
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);

        let ty = Type::Named {
            name: spanned("Unknown".to_string()),
            generic_args: vec![],
        };
        assert_eq!(mapper.map_ast_type(&ty), UNIT_TYPE);
    }

    #[test]
    fn mangle_generic() {
        let name = mangle_generic_name(
            "Vector",
            &[spanned(Type::Primitive(PrimitiveType::Int))],
        );
        assert_eq!(name, "Vector__int64_t");

        let name = mangle_generic_name(
            "Result",
            &[
                spanned(Type::Primitive(PrimitiveType::StringType)),
                spanned(Type::Primitive(PrimitiveType::Str)),
            ],
        );
        assert_eq!(name, "Result__GorgetString__Str");
    }

    #[test]
    fn map_tuple_type() {
        let mut reg = TypeRegistry::new();
        let mut mapper = TypeMapper::new(&mut reg);

        let tuple_ty = Type::Tuple(vec![
            spanned(Type::Primitive(PrimitiveType::Int)),
            spanned(Type::Primitive(PrimitiveType::Float)),
        ]);

        let id = mapper.map_ast_type_mut(&tuple_ty, &mut reg);
        assert_ne!(id, UNIT_TYPE);

        // Should be cached now
        let id2 = mapper.map_ast_type(&tuple_ty);
        assert_eq!(id, id2);

        // TypeDef should exist
        let def = reg.get_type_def("Tuple__int64_t__double").unwrap();
        assert_eq!(def.name, "Tuple__int64_t__double");
        if let TypeDefKind::Struct(ref s) = def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "_0");
            assert_eq!(s.fields[1].name, "_1");
        } else {
            panic!("Expected Struct");
        }
    }
}
