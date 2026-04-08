//! Type helpers and struct registry for LIR.

use super::{LirType, StructDef, StructId};
use std::collections::HashMap;

/// Registry mapping struct names to their StructIds.
/// Used during GIR→LIR lowering to look up or create struct definitions.
pub struct StructRegistry {
    name_to_id: HashMap<String, StructId>,
}

impl StructRegistry {
    pub fn new() -> Self {
        Self {
            name_to_id: HashMap::new(),
        }
    }

    /// Register a struct definition. Returns `None` if the name was new,
    /// or `Some(existing_id)` if it was already registered.
    pub fn register(&mut self, name: &str, id: StructId) -> Option<StructId> {
        self.name_to_id.insert(name.to_string(), id)
    }

    /// Look up a struct by name.
    pub fn lookup(&self, name: &str) -> Option<StructId> {
        self.name_to_id.get(name).copied()
    }

    /// Number of registered structs.
    pub fn len(&self) -> usize {
        self.name_to_id.len()
    }

    pub fn is_empty(&self) -> bool {
        self.name_to_id.is_empty()
    }
}

impl Default for StructRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Well-known struct layouts for Gorget runtime types.
pub fn builtin_struct_defs() -> Vec<StructDef> {
    vec![
        // GorgetString — 32-byte unified string struct (cap==0 ⟺ view, cap>0 ⟺ owned).
        // The C runtime typedef is "Str". gorget_string_free checks cap before freeing.
        // C layout: { data, len, cap, alloc } — 4 × 8 = 32 bytes.
        StructDef {
            name: "GorgetString".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("len".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
            ],
            is_enum: false,
            computed_c_size: Some(32),
        },
        // GorgetArray — dynamic array (Vector[T] backing).
        // C layout: { data, len, cap, elem_size, alloc, elem_drop, elem_clone } — 7 × 8 = 56 bytes.
        // LIR only models 4 fields; the extra 3 are runtime-internal.
        StructDef {
            name: "GorgetArray".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("len".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("elem_size".into(), LirType::I64),
            ],
            is_enum: false,
            computed_c_size: Some(56),
        },
        // Closure — function pointer + environment. 2 × 8 = 16 bytes.
        StructDef {
            name: "GorgetClosure".into(),
            fields: vec![
                ("fn_ptr".into(), LirType::Ptr),
                ("env".into(), LirType::Ptr),
            ],
            is_enum: false,
            computed_c_size: Some(16),
        },
        // Trait object — data pointer + vtable pointer. 2 × 8 = 16 bytes.
        StructDef {
            name: "TraitObj".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("vtable".into(), LirType::Ptr),
            ],
            is_enum: false,
            computed_c_size: Some(16),
        },
        // Task handle — task pointer + drop function. 2 × 8 = 16 bytes.
        StructDef {
            name: "TaskHandle".into(),
            fields: vec![
                ("task_ptr".into(), LirType::Ptr),
                ("drop_fn".into(), LirType::Ptr),
            ],
            is_enum: false,
            computed_c_size: Some(16),
        },
        // GorgetMap — hash map backing Dict[K,V] and HashMap[K,V].
        // C layout: 17 fields × 8 = 136 bytes (includes key_drop, key_clone, etc.).
        // LIR models 13 fields; the extra 4 are runtime-internal.
        StructDef {
            name: "GorgetMap".into(),
            fields: vec![
                ("keys".into(), LirType::Ptr),
                ("values".into(), LirType::Ptr),
                ("states".into(), LirType::Ptr),
                ("count".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("key_size".into(), LirType::I64),
                ("val_size".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
                ("order".into(), LirType::Ptr),
                ("order_len".into(), LirType::I64),
                ("tombstones".into(), LirType::I64),
                ("hash_fn".into(), LirType::Ptr),
                ("eq_fn".into(), LirType::Ptr),
            ],
            is_enum: false,
            computed_c_size: Some(136),
        },
        // GorgetSet — typedef alias for GorgetMap, backs Set[T] and HashSet[T].
        // Same C layout as GorgetMap: 136 bytes.
        StructDef {
            name: "GorgetSet".into(),
            fields: vec![
                ("keys".into(), LirType::Ptr),
                ("values".into(), LirType::Ptr),
                ("states".into(), LirType::Ptr),
                ("count".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("key_size".into(), LirType::I64),
                ("val_size".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
                ("order".into(), LirType::Ptr),
                ("order_len".into(), LirType::I64),
                ("tombstones".into(), LirType::I64),
                ("hash_fn".into(), LirType::Ptr),
                ("eq_fn".into(), LirType::Ptr),
            ],
            is_enum: false,
            computed_c_size: Some(136),
        },
        // GorgetRange — range iterator. 3 × 8 = 24 bytes.
        StructDef {
            name: "GorgetRange".into(),
            fields: vec![
                ("start".into(), LirType::I64),
                ("end".into(), LirType::I64),
                ("step".into(), LirType::I64),
            ],
            is_enum: false,
            computed_c_size: None,
                      },
    ]
}

/// Size of a scalar LIR type in bytes. Returns `None` for aggregates and void.
pub fn scalar_size(ty: &LirType) -> Option<u32> {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => Some(1),
        LirType::I16 | LirType::U16 => Some(2),
        LirType::I32 | LirType::U32 | LirType::F32 => Some(4),
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) => Some(8),
        LirType::Struct(_) | LirType::Void => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_registry() {
        let mut reg = StructRegistry::new();
        assert!(reg.is_empty());

        assert!(reg.register("GorgetString", StructId(0)).is_none());
        assert_eq!(reg.lookup("GorgetString"), Some(StructId(0)));
        assert_eq!(reg.len(), 1);

        // Re-registering returns old ID
        assert_eq!(reg.register("GorgetString", StructId(5)), Some(StructId(0)));
    }

    #[test]
    fn builtin_structs() {
        let defs = builtin_struct_defs();
        assert!(defs.len() >= 8);
        assert_eq!(defs[0].name, "GorgetString");
        assert_eq!(defs[0].fields.len(), 4);
        assert_eq!(defs[1].name, "GorgetArray");
        assert_eq!(defs[1].fields.len(), 4);
    }

    #[test]
    fn scalar_sizes() {
        assert_eq!(scalar_size(&LirType::I8), Some(1));
        assert_eq!(scalar_size(&LirType::I16), Some(2));
        assert_eq!(scalar_size(&LirType::I32), Some(4));
        assert_eq!(scalar_size(&LirType::I64), Some(8));
        assert_eq!(scalar_size(&LirType::F32), Some(4));
        assert_eq!(scalar_size(&LirType::F64), Some(8));
        assert_eq!(scalar_size(&LirType::Ptr), Some(8));
        assert_eq!(scalar_size(&LirType::Bool), Some(1));
        assert_eq!(scalar_size(&LirType::Struct(StructId(0))), None);
        assert_eq!(scalar_size(&LirType::Void), None);
    }
}
