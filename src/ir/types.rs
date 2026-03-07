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
///
/// # Drop contract
///
/// `DropElaborator` (in `ir/lowering/drops.rs`) decides WHEN to drop based on:
/// - `copy_semantics == Move` → register for drop at scope exit
/// - `drop_strategy != None` → also register (even for Copy types, e.g., ref-counted)
///
/// The C backend decides HOW to drop by looking up `drop_strategy` via
/// `lookup_drop_strategy()` when it encounters a `Drop`/`DropIfAlive` instruction.
///
/// # Valid combinations
///
/// | CopySemantics | DropStrategy    | Use case                               |
/// |---------------|-----------------|----------------------------------------|
/// | Copy          | None            | Primitives, plain value structs        |
/// | Copy          | Trivial(fn)     | Ref-counted types (Shared, Weak, Channel) — Copy at GIR level, decrement on drop |
/// | Move          | None            | Ownership-tracked handles (Thread, Process) — no heap to free, move semantics prevent duplication |
/// | Move          | Trivial(fn)     | Standard owned types (String, Vector, Guard) — single free call |
/// | Move          | Recursive       | Structs containing droppable fields — auto-upgraded by lowering |
/// | Move          | Custom(fn)      | User-defined Drop::drop — runs custom cleanup then field drops |
///
/// **Suspicious** (flagged by validator): Copy + Recursive, Copy + Custom
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

/// Determines HOW a type is cleaned up when dropped.
///
/// The `DropElaborator` emits `Drop { place }` instructions; the backend
/// looks up the strategy from the `TypeRegistry` to generate actual cleanup code.
#[derive(Debug, Clone, PartialEq)]
pub enum DropStrategy {
    /// No cleanup needed (primitives, Copy structs, ownership-only handles).
    None,
    /// Single free function call (e.g., "gorget_string_free", "gorget_array_free").
    /// Backend emits: `fn_name(&place);`
    Trivial(String),
    /// Field-by-field drop (compiler-generated glue).
    /// Auto-assigned by lowering to structs containing Move/droppable fields.
    /// Backend walks fields and emits per-field cleanup.
    Recursive,
    /// User-defined `Drop::drop` implementation.
    /// Backend calls the custom function, then drops fields recursively.
    Custom(String),
}

/// Determines whether a value can be bitwise-copied or requires ownership transfer.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CopySemantics {
    /// Bitwise copy (primitives, `str`, Copy structs, ref-counted types).
    Copy,
    /// Ownership transfer — value cannot be used after move.
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

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
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

    /// Get the type name for a Named type, or None if not Named.
    pub fn type_name(&self, id: TypeId) -> Option<String> {
        match self.get(id)? {
            GirType::Named(name) => Some(name.clone()),
            _ => None,
        }
    }

    /// Return the canonical Gorget-language name for a TypeId.
    /// Works for both pre-allocated primitive types and Named types.
    /// Returns "unknown" only for internal/unresolvable types.
    pub fn type_id_to_canonical_name(&self, id: TypeId) -> String {
        match id {
            BOOL_TYPE  => "bool".to_string(),
            I8_TYPE    => "int8".to_string(),
            I16_TYPE   => "int16".to_string(),
            I32_TYPE   => "int32".to_string(),
            I64_TYPE   => "int".to_string(),
            U8_TYPE    => "uint8".to_string(),
            U16_TYPE   => "uint16".to_string(),
            U32_TYPE   => "uint32".to_string(),
            U64_TYPE   => "uint".to_string(),
            F32_TYPE   => "float32".to_string(),
            F64_TYPE   => "float".to_string(),
            UNIT_TYPE  => "void".to_string(),
            _ => {
                if let Some(GirType::Named(name)) = self.get(id) {
                    return name.clone();
                }
                "unknown".to_string()
            }
        }
    }

    /// Total number of types (including primitives).
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Whether the registry contains only the pre-allocated primitives.
    pub fn is_empty(&self) -> bool {
        self.types.len() <= 13
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

    /// Get a mutable reference to a type definition by name.
    pub fn get_type_def_mut(&mut self, name: &str) -> Option<&mut TypeDef> {
        self.name_to_def.get(name).copied().map(|idx| &mut self.type_defs[idx])
    }

    /// Iterate over all type definitions.
    pub fn type_defs(&self) -> &[TypeDef] {
        &self.type_defs
    }

    /// Check if a named type definition exists.
    pub fn has_type_def(&self, name: &str) -> bool {
        self.name_to_def.contains_key(name)
    }

    /// Iterate over all type definition names.
    pub fn all_type_def_names(&self) -> impl Iterator<Item = &String> {
        self.name_to_def.keys()
    }

    /// Check whether a type has Move copy semantics (owns heap-allocated buffers).
    pub fn is_move_type(&self, type_id: TypeId) -> bool {
        if type_id.0 < 12 { return false; } // primitives
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                return type_def.metadata.copy_semantics == CopySemantics::Move;
            }
        }
        false
    }
}

/// Format a TypeId as a mangle-safe string fragment (for tuple/generic type names).
pub fn format_type_for_mangle(type_id: TypeId, registry: &TypeRegistry) -> String {
    if type_id == BOOL_TYPE { return "bool".to_string(); }
    if type_id == I8_TYPE { return "int8_t".to_string(); }
    if type_id == I16_TYPE { return "int16_t".to_string(); }
    if type_id == I32_TYPE { return "int32_t".to_string(); }
    if type_id == I64_TYPE { return "int64_t".to_string(); }
    if type_id == U8_TYPE { return "uint8_t".to_string(); }
    if type_id == U16_TYPE { return "uint16_t".to_string(); }
    if type_id == U32_TYPE { return "uint32_t".to_string(); }
    if type_id == U64_TYPE { return "uint64_t".to_string(); }
    if type_id == F32_TYPE { return "float".to_string(); }
    if type_id == F64_TYPE { return "double".to_string(); }
    if type_id == UNIT_TYPE { return "void".to_string(); }
    if let Some(gir_type) = registry.get(type_id) {
        if let GirType::Named(name) = gir_type {
            return name.clone();
        }
    }
    format!("T{}", type_id.0)
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
