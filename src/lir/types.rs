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
        // GorgetStringView — non-owning string view (cap==0, alloc==NULL).
        // Identical layout to GorgetString. The C runtime typedef is "Str".
        StructDef {
            name: "GorgetStringView".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("len".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
            ],
            is_enum: false,
                      },
        // GorgetString — 32-byte string struct (cap==0 ⟺ view, cap>0 ⟺ owned)
        StructDef {
            name: "GorgetString".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("len".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("alloc".into(), LirType::Ptr),
            ],
            is_enum: false,
                      },
        // GorgetArray — dynamic array (Vector[T] backing)
        StructDef {
            name: "GorgetArray".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("len".into(), LirType::I64),
                ("cap".into(), LirType::I64),
                ("elem_size".into(), LirType::I64),
            ],
            is_enum: false,
                      },
        // Closure — function pointer + environment
        StructDef {
            name: "GorgetClosure".into(),
            fields: vec![
                ("fn_ptr".into(), LirType::Ptr),
                ("env".into(), LirType::Ptr),
            ],
            is_enum: false,
                      },
        // Trait object — data pointer + vtable pointer
        StructDef {
            name: "TraitObj".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("vtable".into(), LirType::Ptr),
            ],
            is_enum: false,
                      },
        // Task handle — task pointer + drop function
        StructDef {
            name: "TaskHandle".into(),
            fields: vec![
                ("task_ptr".into(), LirType::Ptr),
                ("drop_fn".into(), LirType::Ptr),
            ],
            is_enum: false,
                      },
        // GorgetMap — hash map backing Dict[K,V] and HashMap[K,V]
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
                      },
        // GorgetSet — typedef alias for GorgetMap, backs Set[T] and HashSet[T]
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
                      },
        // GorgetRange — range iterator
        StructDef {
            name: "GorgetRange".into(),
            fields: vec![
                ("start".into(), LirType::I64),
                ("end".into(), LirType::I64),
                ("step".into(), LirType::I64),
            ],
            is_enum: false,
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

        assert!(reg.register("GorgetStringView", StructId(0)).is_none());
        assert_eq!(reg.lookup("GorgetStringView"), Some(StructId(0)));
        assert_eq!(reg.len(), 1);

        // Re-registering returns old ID
        assert_eq!(reg.register("GorgetStringView", StructId(5)), Some(StructId(0)));
    }

    #[test]
    fn builtin_structs() {
        let defs = builtin_struct_defs();
        assert!(defs.len() >= 9);
        assert_eq!(defs[0].name, "GorgetStringView");
        assert_eq!(defs[0].fields.len(), 4);
        assert_eq!(defs[1].name, "GorgetString");
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
