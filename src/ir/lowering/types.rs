use rustc_hash::FxHashMap;

use crate::ir::types::*;
use crate::parser::ast::{self, PrimitiveType, Type};
use crate::span::Spanned;

/// Maps AST types to GIR TypeIds.
pub struct TypeMapper {
    /// `str` maps to `Ptr(U8)` in GIR (Phase 1 compat) — will be Named("Str") later.
    pub str_type: TypeId,
    /// `String` (owned) maps to Named("GorgetString") for string interpolation results.
    pub owned_string_type: TypeId,
    /// Cache of Named type → GIR TypeId.
    pub named_types: FxHashMap<String, TypeId>,
}

impl TypeMapper {
    pub fn new(registry: &mut TypeRegistry) -> Self {
        // Register Str as a named type matching the runtime's fat pointer struct
        let str_type = registry.insert(GirType::Named("Str".to_string()));
        // Register GorgetString with Move semantics + trivial drop (gorget_string_free)
        registry.add_type_def(TypeDef {
            name: "GorgetString".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("gorget_string_free".to_string()),
                copy_semantics: CopySemantics::Move,
            },
        });
        let owned_string_type = registry.insert(GirType::Named("GorgetString".to_string()));
        Self {
            str_type,
            owned_string_type,
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
                    // Auto-register Option[T] and Result[T, E] types
                    let base = name.node.as_str();
                    if base == "Option" && generic_args.len() == 1 {
                        let inner_type = self.map_ast_type_mut(&generic_args[0].node, registry);
                        let type_def = TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Enum(EnumDef {
                                variants: vec![
                                    EnumVariant {
                                        name: "Some".to_string(),
                                        fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
                                    },
                                    EnumVariant {
                                        name: "None".to_string(),
                                        fields: vec![],
                                    },
                                ],
                            }),
                            metadata: TypeMetadata::default(),
                        };
                        registry.add_type_def(type_def);
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.named_types.insert(mangled, type_id);
                        return type_id;
                    }
                    if base == "Result" && generic_args.len() == 2 {
                        let ok_type = self.map_ast_type_mut(&generic_args[0].node, registry);
                        let err_type = self.map_ast_type_mut(&generic_args[1].node, registry);
                        let type_def = TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Enum(EnumDef {
                                variants: vec![
                                    EnumVariant {
                                        name: "Ok".to_string(),
                                        fields: vec![StructField { name: "_0".to_string(), type_id: ok_type }],
                                    },
                                    EnumVariant {
                                        name: "Error".to_string(),
                                        fields: vec![StructField { name: "_0".to_string(), type_id: err_type }],
                                    },
                                ],
                            }),
                            metadata: TypeMetadata::default(),
                        };
                        registry.add_type_def(type_def);
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.named_types.insert(mangled, type_id);
                        return type_id;
                    }
                    // Auto-register collection types (Vector[T], etc.) with proper drop metadata
                    if matches!(base, "Vector" | "Set" | "HashSet" | "Dict" | "HashMap") {
                        let drop_fn = match base {
                            "Dict" | "HashMap" => "gorget_map_free",
                            "Set" | "HashSet" => "gorget_set_free",
                            _ => "gorget_array_free",
                        };
                        registry.add_type_def(TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                drop_strategy: DropStrategy::Trivial(drop_fn.to_string()),
                                copy_semantics: CopySemantics::Move,
                            },
                        });
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.named_types.insert(mangled, type_id);
                        return type_id;
                    }
                    // Auto-register concurrency types: Channel[T], Shared[T], Weak[T], Mutex[T], Guard[T].
                    // These are opaque C pointer typedefs with no GIR-level fields.
                    // Registering here (in the fn_sigs pre-scan path) means function parameters
                    // of these types resolve to the correct TypeId before bodies are lowered.
                    if matches!(base, "Channel" | "Shared" | "Weak" | "Mutex" | "Guard") {
                        let (copy_sem, drop_strat) = match base {
                            "Guard" => {
                                // Guard[T] is Move + RAII drop that unlocks the mutex.
                                (CopySemantics::Move, DropStrategy::Trivial(format!("{mangled}__drop")))
                            }
                            "Shared" | "Weak" | "Channel" => {
                                // Shared[T] / Weak[T] / Channel[T]: Copy pointer + Trivial RAII drop (refcount).
                                (CopySemantics::Copy, DropStrategy::Trivial(format!("{mangled}__drop")))
                            }
                            _ => {
                                // Mutex: opaque pointer — Copy, no drop.
                                (CopySemantics::Copy, DropStrategy::None)
                            }
                        };
                        registry.add_type_def(TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                copy_semantics: copy_sem,
                                drop_strategy: drop_strat,
                            },
                        });
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.named_types.insert(mangled, type_id);
                        return type_id;
                    }
                    // Auto-register std.sync generic types: RWLock[T], ReadGuard[T], WriteGuard[T].
                    if matches!(base, "RWLock" | "ReadGuard" | "WriteGuard") {
                        let (copy_sem, drop_strat) = match base {
                            "ReadGuard" => (CopySemantics::Move, DropStrategy::Trivial(format!("{mangled}__drop"))),
                            "WriteGuard" => (CopySemantics::Move, DropStrategy::Trivial(format!("{mangled}__drop"))),
                            _ => (CopySemantics::Copy, DropStrategy::None), // RWLock is a pointer (Copy)
                        };
                        registry.add_type_def(TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                copy_semantics: copy_sem,
                                drop_strategy: drop_strat,
                            },
                        });
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.named_types.insert(mangled, type_id);
                        return type_id;
                    }
                    // Auto-register std.thread generic type: Thread[T].
                    if base == "Thread" {
                        registry.add_type_def(TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                            metadata: TypeMetadata {
                                size: None,
                                align: None,
                                copy_semantics: CopySemantics::Move,
                                drop_strategy: DropStrategy::None, // join() consumes it
                            },
                        });
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.named_types.insert(mangled, type_id);
                        return type_id;
                    }
                    // Callable/MutCallable/ConsumeCallable generics: return a FnPtr TypeId
                    // so locals declared as Callable[T(P)] get GorgetClosure C type and
                    // use __gorget_closure_call_N dispatch.
                    // NOT cached in named_types so map_ast_type (immutable, used for
                    // function parameters) still returns UNIT_TYPE → void* __callable_N ABI.
                    if matches!(base, "Callable" | "MutCallable" | "ConsumeCallable") {
                        return if generic_args.len() == 1 {
                            self.map_ast_type_mut(&generic_args[0].node, registry)
                        } else {
                            registry.insert(GirType::FnPtr { params: vec![], return_type: UNIT_TYPE })
                        };
                    }
                    return UNIT_TYPE;
                }
                if let Some(&id) = self.named_types.get(name.node.as_str()) {
                    return id;
                }
                // Auto-register the non-generic TaskGroup type (Move pointer, RAII join+free).
                if name.node == "TaskGroup" {
                    registry.add_type_def(TypeDef {
                        name: "TaskGroup".to_string(),
                        kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                        metadata: TypeMetadata {
                            size: None,
                            align: None,
                            copy_semantics: CopySemantics::Move,
                            drop_strategy: DropStrategy::Trivial("gorget_task_group_free".to_string()),
                        },
                    });
                    let type_id = registry.insert(GirType::Named("TaskGroup".to_string()));
                    self.named_types.insert("TaskGroup".to_string(), type_id);
                    return type_id;
                }
                // Auto-register non-generic std.sync types (AtomicInt, AtomicBool, Barrier).
                if matches!(name.node.as_str(), "AtomicInt" | "AtomicBool" | "Barrier") {
                    let n = name.node.clone();
                    registry.add_type_def(TypeDef {
                        name: n.clone(),
                        kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                        metadata: TypeMetadata {
                            size: None,
                            align: None,
                            copy_semantics: CopySemantics::Copy, // opaque pointer
                            drop_strategy: DropStrategy::None,
                        },
                    });
                    let type_id = registry.insert(GirType::Named(n.clone()));
                    self.named_types.insert(n, type_id);
                    return type_id;
                }
                // Auto-register std.process Process type (non-generic, Move, RAII).
                if name.node == "Process" {
                    registry.add_type_def(TypeDef {
                        name: "Process".to_string(),
                        kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                        metadata: TypeMetadata {
                            size: None,
                            align: None,
                            copy_semantics: CopySemantics::Move,
                            drop_strategy: DropStrategy::None,
                        },
                    });
                    let type_id = registry.insert(GirType::Named("Process".to_string()));
                    self.named_types.insert("Process".to_string(), type_id);
                    return type_id;
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
            Type::Function { return_type, params, .. } => {
                let ret = self.map_ast_type_mut(&return_type.node, registry);
                let param_types: Vec<TypeId> = params.iter()
                    .map(|p| self.map_ast_type_mut(&p.node, registry))
                    .collect();
                registry.insert(GirType::FnPtr { params: param_types, return_type: ret })
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

    /// Reverse-lookup: find the registered name for a GIR TypeId.
    /// Returns None for primitive types (caller should handle those separately).
    pub fn name_for_type_id(&self, type_id: TypeId) -> Option<String> {
        self.named_types.iter()
            .find(|(_, tid)| **tid == type_id)
            .map(|(name, _)| name.clone())
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
            PrimitiveType::StringType => self.owned_string_type,
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
            "%f"
        } else if type_id == BOOL_TYPE {
            "%s"
        } else if type_id == self.str_type || type_id == self.owned_string_type {
            "%s"
        } else {
            "%lld" // fallback
        }
    }

    /// Returns true if this type needs special printf handling (e.g., Str → two args).
    pub fn is_str_type(&self, type_id: TypeId) -> bool {
        type_id == self.str_type || type_id == self.owned_string_type
    }
}

/// Register a user-defined struct from AST into the TypeRegistry and TypeMapper.
pub fn register_struct_type(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    struct_def: &ast::StructDef,
    generic_templates: &[&ast::Item],
) {
    let name = &struct_def.name.node;

    // Skip generic structs — they'll be monomorphized in P2.3
    if struct_def.generic_params.is_some() {
        return;
    }

    // Already fully registered (TypeDef present)?
    if registry.get_type_def(name.as_str()).is_some() {
        return;
    }

    // Pre-register the struct name → TypeId if not already done (e.g., by a pre-pass).
    // This allows recursive references within the same struct's fields.
    if !mapper.named_types.contains_key(name.as_str()) {
        let placeholder_id = registry.insert(GirType::Named(name.clone()));
        mapper.named_types.insert(name.clone(), placeholder_id);
    }

    // Pre-register any generic types used as field types (e.g., Option[Color])
    for f in &struct_def.fields {
        ensure_generic_field_type_registered(mapper, registry, &f.node.type_.node, generic_templates);
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
    // TypeId already registered via placeholder above — no need to insert again
}

/// Register a newtype (single-field wrapper struct) as a GIR type.
pub fn register_newtype(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    nt: &ast::NewtypeDef,
) {
    let name = &nt.name.node;
    // Already fully registered?
    if registry.get_type_def(name.as_str()).is_some() {
        return;
    }
    // Pre-register name → TypeId if not already done by a pre-pass
    if !mapper.named_types.contains_key(name.as_str()) {
        let placeholder_id = registry.insert(GirType::Named(name.clone()));
        mapper.named_types.insert(name.clone(), placeholder_id);
    }

    let inner_type = mapper.map_ast_type(&nt.inner_type.node);
    let fields = vec![StructField {
        name: "_0".to_string(),
        type_id: inner_type,
    }];
    let type_def = TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields }),
        metadata: TypeMetadata::default(),
    };
    registry.add_type_def(type_def);
}

/// Ensure a generic type used in a struct field (like Option[Color]) is registered.
pub fn ensure_generic_field_type_registered(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    ty: &ast::Type,
    generic_templates: &[&ast::Item],
) {
    use crate::parser::ast::Type;
    if let Type::Named { name, generic_args } = ty {
        if generic_args.is_empty() {
            return;
        }
        let mangled = mangle_generic_name(&name.node, generic_args);
        if mapper.named_types.contains_key(&mangled) {
            return; // Already registered
        }
        // Handle built-in generic types: Option[T], Result[T, E], and collections
        match name.node.as_str() {
            "Option" if generic_args.len() == 1 => {
                register_builtin_option(mapper, registry, generic_args, &mangled);
                return;
            }
            "Result" if generic_args.len() == 2 => {
                register_builtin_result(mapper, registry, generic_args, &mangled);
                return;
            }
            // Collection types: all resolve to GorgetArray/GorgetMap/etc. but need
            // a registered TypeId so fields referencing them don't get UNIT_TYPE.
            "Vector" | "Dict" | "HashMap" | "Set" | "HashSet" | "Box" => {
                register_collection_alias(mapper, registry, &name.node, generic_args, &mangled);
                return;
            }
            _ => {}
        }
        // Find the template in user-defined generics
        for template in generic_templates {
            match template {
                ast::Item::Enum(enum_def) if enum_def.name.node == name.node => {
                    super::generics::monomorphize_generic_type(
                        mapper, registry, template, generic_args, &mangled,
                    );
                    return;
                }
                ast::Item::Struct(struct_def) if struct_def.name.node == name.node => {
                    super::generics::monomorphize_generic_type(
                        mapper, registry, template, generic_args, &mangled,
                    );
                    return;
                }
                _ => {}
            }
        }
    }
}

/// Register a monomorphized Option[T] type (built-in: Some(T) | None).
fn register_builtin_option(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    type_args: &[crate::span::Spanned<ast::Type>],
    mangled_name: &str,
) {
    let inner_type = mapper.map_ast_type(&type_args[0].node);
    let type_def = TypeDef {
        name: mangled_name.to_string(),
        kind: TypeDefKind::Enum(EnumDef {
            variants: vec![
                EnumVariant {
                    name: "Some".to_string(),
                    fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
                },
                EnumVariant {
                    name: "None".to_string(),
                    fields: vec![],
                },
            ],
        }),
        metadata: TypeMetadata::default(),
    };
    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.named_types.insert(mangled_name.to_string(), type_id);
}

/// Register a monomorphized Result[T, E] type (built-in: Ok(T) | Error(E)).
fn register_builtin_result(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    type_args: &[crate::span::Spanned<ast::Type>],
    mangled_name: &str,
) {
    let ok_type = mapper.map_ast_type(&type_args[0].node);
    let err_type = mapper.map_ast_type(&type_args[1].node);
    let type_def = TypeDef {
        name: mangled_name.to_string(),
        kind: TypeDefKind::Enum(EnumDef {
            variants: vec![
                EnumVariant {
                    name: "Ok".to_string(),
                    fields: vec![StructField { name: "_0".to_string(), type_id: ok_type }],
                },
                EnumVariant {
                    name: "Error".to_string(),
                    fields: vec![StructField { name: "_0".to_string(), type_id: err_type }],
                },
            ],
        }),
        metadata: TypeMetadata::default(),
    };
    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.named_types.insert(mangled_name.to_string(), type_id);
}

/// Register a collection type alias (Vector[T], Dict[K,V], etc.) as a named GIR type.
/// These all map to the same runtime struct (GorgetArray, GorgetMap, etc.) but need
/// unique TypeIds so that fields referencing them don't resolve to UNIT_TYPE.
pub(super) fn register_collection_alias(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    base_name: &str,
    _type_args: &[crate::span::Spanned<ast::Type>],
    mangled_name: &str,
) {
    // All collection instances are structurally identical at runtime.
    // We register them as named types without a TypeDef — the C backend handles
    // collection_type_alias for the actual C type name.
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.named_types.insert(mangled_name.to_string(), type_id);

    // For Box types, also register a TypeDef so the C backend can emit the typedef
    if base_name == "Box" {
        let inner_type = mapper.map_ast_type(&_type_args[0].node);
        let type_def = TypeDef {
            name: mangled_name.to_string(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
            }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                copy_semantics: CopySemantics::Move,
                drop_strategy: DropStrategy::Trivial("free".to_string()),
            },
        };
        registry.add_type_def(type_def);
    } else {
        // Register a simple struct TypeDef so the C backend can find it and emit collection alias
        let type_def = TypeDef {
            name: mangled_name.to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata::default(),
        };
        registry.add_type_def(type_def);
    }
}

/// Register a user-defined enum from AST into the TypeRegistry and TypeMapper.
pub fn register_enum_type(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    enum_def: &ast::EnumDef,
    generic_templates: &[&ast::Item],
) {
    let name = &enum_def.name.node;

    // Skip generic enums — they'll be monomorphized in P2.3
    if enum_def.generic_params.is_some() {
        return;
    }

    // Already fully registered (TypeDef present)?
    if registry.get_type_def(name.as_str()).is_some() {
        return;
    }

    // Pre-register the enum name → TypeId if not already done (e.g., by a pre-pass).
    // This allows recursive references (e.g., Box[Json] in Json) to resolve.
    if !mapper.named_types.contains_key(name.as_str()) {
        let placeholder_id = registry.insert(GirType::Named(name.clone()));
        mapper.named_types.insert(name.clone(), placeholder_id);
    }

    // Pre-register generic types used in variant fields (e.g., Vector[Json], Dict[str, Json])
    for v in &enum_def.variants {
        if let ast::VariantFields::Tuple(types) = &v.node.fields {
            for t in types {
                ensure_generic_field_type_registered(mapper, registry, &t.node, generic_templates);
            }
        }
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
    // TypeId already registered via placeholder above — no need to insert again
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
pub fn mangle_type_for_name(ty: &Type) -> String {
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
            PrimitiveType::Void => "void".to_string(),
        },
        Type::Named { name, generic_args } => {
            if !generic_args.is_empty() {
                return mangle_generic_name(&name.node, generic_args);
            }
            name.node.clone()
        }
        Type::Tuple(elems) => mangle_tuple_name(elems),
        // Callable[T(Params)] has a Type::Function as its generic arg — all callables
        // are GorgetClosure at runtime, so use that as the C name fragment.
        Type::Function { .. } => "GorgetClosure".to_string(),
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
        // str maps to a Named("Str") type (matches the runtime fat pointer struct)
        let str_id = mapper.map_primitive(&PrimitiveType::Str);
        assert_eq!(str_id, mapper.str_type);
        assert!(matches!(reg.get(str_id), Some(GirType::Named(name)) if name == "Str"));
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
        assert_eq!(mapper.format_specifier(F64_TYPE), "%f");
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
