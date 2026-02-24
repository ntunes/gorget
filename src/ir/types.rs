use rustc_hash::FxHashMap;
use std::fmt;

/// Index into the GIR type table. Distinct from semantic `TypeId`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

/// Index into a function's local table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

/// Index into a function's basic block list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeId({})", self.0)
    }
}

impl fmt::Display for LocalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// A GIR type — all types are concrete (post-monomorphization).
#[derive(Debug, Clone, PartialEq)]
pub enum GirType {
    // Primitives
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Unit,

    // Pointers
    Ptr(TypeId),
    MutPtr(TypeId),

    // Function pointer
    FnPtr {
        params: Vec<TypeId>,
        return_type: TypeId,
    },

    // Named type (references a TypeDef by name)
    Named(String),
}

/// A named type definition.
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub kind: TypeDefKind,
    pub metadata: TypeMetadata,
}

#[derive(Debug, Clone)]
pub enum TypeDefKind {
    Struct(StructDef),
    Enum(EnumDef),
    Alias(TypeId),
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub type_id: TypeId,
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<StructField>,
}

/// Layout and ownership metadata for a type.
#[derive(Debug, Clone)]
pub struct TypeMetadata {
    pub size: Option<u64>,
    pub align: Option<u64>,
    pub drop_strategy: DropStrategy,
    pub copy_semantics: CopySemantics,
}

impl Default for TypeMetadata {
    fn default() -> Self {
        Self {
            size: None,
            align: None,
            drop_strategy: DropStrategy::None,
            copy_semantics: CopySemantics::Copy,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DropStrategy {
    /// No cleanup needed (primitives, Copy structs).
    None,
    /// Single free function call (e.g., "gorget_string_free").
    Trivial(String),
    /// Field-by-field drop (compiler-generated glue).
    Recursive,
    /// User-defined Drop::drop (function name).
    Custom(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CopySemantics {
    /// Bitwise copy (primitives, `str`, Copy structs).
    Copy,
    /// Ownership transfer.
    Move,
}

// Pre-allocated primitive type IDs.
pub const BOOL_TYPE: TypeId = TypeId(0);
pub const I8_TYPE: TypeId = TypeId(1);
pub const I16_TYPE: TypeId = TypeId(2);
pub const I32_TYPE: TypeId = TypeId(3);
pub const I64_TYPE: TypeId = TypeId(4);
pub const U8_TYPE: TypeId = TypeId(5);
pub const U16_TYPE: TypeId = TypeId(6);
pub const U32_TYPE: TypeId = TypeId(7);
pub const U64_TYPE: TypeId = TypeId(8);
pub const F32_TYPE: TypeId = TypeId(9);
pub const F64_TYPE: TypeId = TypeId(10);
pub const UNIT_TYPE: TypeId = TypeId(11);

/// Registry of all GIR types in a module.
pub struct TypeRegistry {
    /// All types, indexed by TypeId.
    types: Vec<GirType>,
    /// Named type definitions.
    type_defs: Vec<TypeDef>,
    /// Name → index in `type_defs`.
    name_to_def: FxHashMap<String, usize>,
}

impl fmt::Debug for TypeRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeRegistry")
            .field("types_count", &self.types.len())
            .field("type_defs_count", &self.type_defs.len())
            .finish()
    }
}

impl Clone for TypeRegistry {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            type_defs: self.type_defs.clone(),
            name_to_def: self.name_to_def.clone(),
        }
    }
}

impl TypeRegistry {
    /// Create a new registry with pre-allocated primitive types at indices 0–11.
    pub fn new() -> Self {
        let types = vec![
            GirType::Bool, // 0
            GirType::I8,   // 1
            GirType::I16,  // 2
            GirType::I32,  // 3
            GirType::I64,  // 4
            GirType::U8,   // 5
            GirType::U16,  // 6
            GirType::U32,  // 7
            GirType::U64,  // 8
            GirType::F32,  // 9
            GirType::F64,  // 10
            GirType::Unit, // 11
        ];
        Self {
            types,
            type_defs: Vec::new(),
            name_to_def: FxHashMap::default(),
        }
    }

    /// Insert a type and return its TypeId.
    pub fn insert(&mut self, ty: GirType) -> TypeId {
        let id = TypeId(self.types.len() as u32);
        self.types.push(ty);
        id
    }

    /// Look up a type by its TypeId.
    pub fn get(&self, id: TypeId) -> Option<&GirType> {
        self.types.get(id.0 as usize)
    }

    /// Total number of types (including primitives).
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Whether the registry contains only the pre-allocated primitives.
    pub fn is_empty(&self) -> bool {
        self.types.len() <= 12
    }

    /// Register a named type definition. Returns its index.
    pub fn add_type_def(&mut self, def: TypeDef) -> usize {
        let idx = self.type_defs.len();
        self.name_to_def.insert(def.name.clone(), idx);
        self.type_defs.push(def);
        idx
    }

    /// Look up a type definition by name.
    pub fn get_type_def(&self, name: &str) -> Option<&TypeDef> {
        self.name_to_def.get(name).map(|&idx| &self.type_defs[idx])
    }

    /// Iterate over all type definitions.
    pub fn type_defs(&self) -> &[TypeDef] {
        &self.type_defs
    }

    /// Check if a named type definition exists.
    pub fn has_type_def(&self, name: &str) -> bool {
        self.name_to_def.contains_key(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_registry_primitives() {
        let reg = TypeRegistry::new();
        assert_eq!(reg.len(), 12);
        assert_eq!(reg.get(BOOL_TYPE), Some(&GirType::Bool));
        assert_eq!(reg.get(I8_TYPE), Some(&GirType::I8));
        assert_eq!(reg.get(I16_TYPE), Some(&GirType::I16));
        assert_eq!(reg.get(I32_TYPE), Some(&GirType::I32));
        assert_eq!(reg.get(I64_TYPE), Some(&GirType::I64));
        assert_eq!(reg.get(U8_TYPE), Some(&GirType::U8));
        assert_eq!(reg.get(U16_TYPE), Some(&GirType::U16));
        assert_eq!(reg.get(U32_TYPE), Some(&GirType::U32));
        assert_eq!(reg.get(U64_TYPE), Some(&GirType::U64));
        assert_eq!(reg.get(F32_TYPE), Some(&GirType::F32));
        assert_eq!(reg.get(F64_TYPE), Some(&GirType::F64));
        assert_eq!(reg.get(UNIT_TYPE), Some(&GirType::Unit));
    }

    #[test]
    fn type_registry_insert() {
        let mut reg = TypeRegistry::new();
        let ptr_id = reg.insert(GirType::Ptr(I32_TYPE));
        assert_eq!(ptr_id, TypeId(12));
        assert_eq!(reg.get(ptr_id), Some(&GirType::Ptr(I32_TYPE)));
        assert_eq!(reg.len(), 13);
    }

    #[test]
    fn type_def_struct() {
        let mut reg = TypeRegistry::new();
        let def = TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata {
                size: Some(16),
                align: Some(8),
                drop_strategy: DropStrategy::None,
                copy_semantics: CopySemantics::Copy,
            },
        };
        reg.add_type_def(def);
        let retrieved = reg.get_type_def("Point").unwrap();
        assert_eq!(retrieved.name, "Point");
        assert!(matches!(retrieved.kind, TypeDefKind::Struct(_)));
        assert_eq!(retrieved.metadata.size, Some(16));
    }

    #[test]
    fn type_def_enum() {
        let mut reg = TypeRegistry::new();
        let def = TypeDef {
            name: "Option__int".into(),
            kind: TypeDefKind::Enum(EnumDef {
                variants: vec![
                    EnumVariant {
                        name: "Some".into(),
                        fields: vec![StructField {
                            name: "_0".into(),
                            type_id: I64_TYPE,
                        }],
                    },
                    EnumVariant {
                        name: "None".into(),
                        fields: vec![],
                    },
                ],
            }),
            metadata: TypeMetadata::default(),
        };
        reg.add_type_def(def);
        let retrieved = reg.get_type_def("Option__int").unwrap();
        if let TypeDefKind::Enum(ref e) = retrieved.kind {
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Some");
            assert_eq!(e.variants[1].name, "None");
            assert_eq!(e.variants[1].fields.len(), 0);
        } else {
            panic!("Expected Enum");
        }
    }
}
