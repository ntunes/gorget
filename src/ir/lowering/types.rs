use rustc_hash::FxHashMap;

use crate::ir::types::*;
use crate::ir::lowering::builtins::{self, BuiltinTypeArgs};
use crate::parser::ast::{self, PrimitiveType, Type};
use crate::span::Spanned;

/// A deferred builtin type registration: populated during `map_ast_type_mut`,
/// consumed later to populate `fn_sigs` and `runtime_callees` on LoweringContext.
pub struct DeferredBuiltin {
    /// Mangled type name (e.g., "Vector__int64_t").
    pub mangled_name: String,
    /// Reference to the builtin protocol.
    pub protocol: &'static builtins::BuiltinTypeProtocol,
    /// Resolved type arguments.
    pub type_args: BuiltinTypeArgs,
}

/// Maps AST types to GIR TypeIds.
pub struct TypeMapper {
    /// The GIR type for string views (cap=0). Maps to Named("GorgetStringView").
    pub string_view_type: TypeId,
    /// `String` (owned) maps to Named("GorgetString") for string interpolation results.
    pub owned_string_type: TypeId,
    /// Cache of Named type → GIR TypeId.
    pub named_types: FxHashMap<String, TypeId>,
    /// Builtin types registered during `map_ast_type_mut`, pending fn_sigs population.
    pub deferred_builtins: Vec<DeferredBuiltin>,
}

impl TypeMapper {
    pub fn new(registry: &mut TypeRegistry) -> Self {
        // Register Str as a named type matching the runtime's fat pointer struct
        let string_view_type = registry.insert(GirType::Named("GorgetStringView".to_string()));
        // Register GorgetString with Move semantics + trivial drop (gorget_string_free)
        registry.add_type_def(TypeDef {
            name: "GorgetString".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("gorget_string_free".to_string()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let owned_string_type = registry.insert(GirType::Named("GorgetString".to_string()));
        Self {
            string_view_type,
            owned_string_type,
            named_types: FxHashMap::default(),
            deferred_builtins: Vec::new(),
        }
    }

    /// Map an AST `Type` to a GIR `TypeId`.
    ///
    /// Returns `UNIT_TYPE` for types not yet registered. Use `try_map_ast_type`
    /// when you need to distinguish "genuinely void" from "unknown type."
    pub fn map_ast_type(&self, ty: &Type) -> TypeId {
        self.try_map_ast_type(ty).unwrap_or(UNIT_TYPE)
    }

    /// Try to map an AST `Type` to a GIR `TypeId`.
    ///
    /// Returns `None` when the type is not registered (not yet monomorphized,
    /// unresolved generic, function pointer, etc.). Unlike `map_ast_type`, this
    /// lets callers distinguish "genuinely void" from "unknown type."
    pub fn try_map_ast_type(&self, ty: &Type) -> Option<TypeId> {
        match ty {
            Type::Primitive(prim) => Some(self.map_primitive(prim)),
            Type::Inferred => panic!("BUG: Inferred type should be resolved before GIR lowering"),
            Type::Named { name, generic_args } => {
                if !generic_args.is_empty() {
                    let mangled = mangle_generic_name(&name.node, generic_args);
                    return self.named_types.get(&mangled).copied();
                }
                self.named_types.get(name.node.as_str()).copied()
            }
            Type::Tuple(elems) => {
                if elems.is_empty() {
                    return Some(UNIT_TYPE);
                }
                let mangled = mangle_tuple_name(elems);
                self.named_types.get(&mangled).copied()
            }
            Type::Function { .. } => {
                // Immutable path can't register FnPtr types — return None.
                // Callers needing function pointer types should use map_ast_type_mut.
                None
            }
            Type::Ref(_) => {
                // Immutable path can't register Ptr types — return None.
                // Callers needing Ref types should use map_ast_type_mut.
                None
            }
            Type::Owned(inner) => {
                // Type ! → just the inner type (ownership annotation only)
                self.try_map_ast_type(&inner.node)
            }
            Type::Pointer(inner) => {
                // T* in extern "C" — map to the inner type. The Ptr ABI is handled
                // by AbiKind::Ptr at the call site, not by the type system.
                self.try_map_ast_type(&inner.node)
            }
            Type::SelfType => None,
            Type::Array { .. } | Type::Slice { .. } => None,
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
                        return self.get_or_register(&mangled, registry, |n| {
                            make_option_type_def(n, inner_type)
                        });
                    }
                    if base == "Result" && generic_args.len() == 2 {
                        let ok_type = self.map_ast_type_mut(&generic_args[0].node, registry);
                        let err_type = self.map_ast_type_mut(&generic_args[1].node, registry);
                        return self.get_or_register(&mangled, registry, |n| {
                            make_result_type_def(n, ok_type, err_type)
                        });
                    }
                    // Auto-register builtin generic types via protocol table.
                    // Collections, concurrency types, etc. — drop metadata, clone_fn,
                    // collection_kind all come from the BuiltinTypeProtocol.
                    if let Some(protocol) = builtins::lookup_protocol(base) {
                        // Resolve type args to TypeIds (available from generic_args)
                        let elem = self.map_ast_type_mut(&generic_args[0].node, registry);
                        let val = if generic_args.len() >= 2 {
                            self.map_ast_type_mut(&generic_args[1].node, registry)
                        } else {
                            elem
                        };

                        // Drop strategy: protocol provides the free function, but some
                        // types use per-monomorphization drop wrappers.
                        let drop_strat = match base {
                            "Guard" | "Shared" | "Weak" | "Channel"
                            | "ReadGuard" | "WriteGuard" =>
                                DropStrategy::Trivial(format!("{mangled}__drop")),
                            _ => match protocol.drop_fn {
                                Some(f) => DropStrategy::Trivial(f.to_string()),
                                None => DropStrategy::None,
                            },
                        };

                        let type_id = self.get_or_register(&mangled, registry, |n| {
                            make_opaque_type_def(n, protocol.copy_semantics, drop_strat)
                        });

                        // Set protocol-derived metadata on the TypeDef
                        if let Some(td) = registry.get_type_def_mut(&mangled) {
                            td.metadata.clone_fn = protocol.clone_fn.map(String::from);
                            td.metadata.collection_kind = protocol.collection_kind;
                        }

                        // Defer fn_sigs population (needs LoweringContext, not available here)
                        let elem_name = crate::ir::types::format_type_for_mangle(elem, registry);
                        self.deferred_builtins.push(DeferredBuiltin {
                            mangled_name: mangled.clone(),
                            protocol,
                            type_args: BuiltinTypeArgs {
                                elem,
                                key: elem,
                                val,
                                self_type: type_id,
                                self_name: mangled,
                            },
                        });

                        // Suppress unused variable warning
                        let _ = elem_name;

                        return type_id;
                    }
                    // Thread[T] — resource semantics but no protocol methods (yet).
                    if base == "Thread" {
                        return self.get_or_register(&mangled, registry, |n| {
                            make_opaque_type_def(n, CopySemantics::Resource, DropStrategy::None)
                        });
                    }
                    // Task[T] — hand-emitted by C backend.  Register only a Named
                    // GirType (no TypeDef) so that containers like Option[Task[void]]
                    // get a real inner TypeId instead of UNIT_TYPE.
                    if base == "Task" {
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
                    return self.get_or_register("TaskGroup", registry, |n| {
                        make_opaque_type_def(n, CopySemantics::Resource, DropStrategy::Trivial("gorget_task_group_free".to_string()))
                    });
                }
                // Auto-register non-generic std.sync types (AtomicInt, AtomicBool, Barrier).
                if matches!(name.node.as_str(), "AtomicInt" | "AtomicBool" | "Barrier") {
                    return self.get_or_register(&name.node, registry, |n| {
                        make_opaque_type_def(n, CopySemantics::Trivial, DropStrategy::None)
                    });
                }
                // WaitGroup, Semaphore — heap-allocated pointer types, Copy semantics
                // (shared across threads by copying the pointer).
                if matches!(name.node.as_str(), "WaitGroup" | "Semaphore" | "OnceFlag") {
                    return self.get_or_register(&name.node, registry, |n| {
                        make_opaque_type_def(n, CopySemantics::Trivial, DropStrategy::None)
                    });
                }
                // Auto-register std.process Process type (non-generic, Move, RAII).
                if name.node == "Process" {
                    return self.get_or_register("Process", registry, |n| {
                        make_opaque_type_def(n, CopySemantics::Resource, DropStrategy::None)
                    });
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
            Type::Ref(inner) => {
                let inner_id = self.map_ast_type_mut(&inner.node, registry);
                registry.insert(GirType::Ptr(inner_id))
            }
            Type::Owned(inner) => {
                self.map_ast_type_mut(&inner.node, registry)
            }
            _ => self.map_ast_type(ty),
        }
    }

    /// Register a named type that has already been added to the TypeRegistry.
    pub fn register_named(&mut self, name: String, type_id: TypeId) {
        self.named_types.insert(name, type_id);
    }

    /// Idempotent type registration: returns existing TypeId if `name` is already
    /// registered, otherwise creates the TypeDef + GirType::Named entry.
    ///
    /// `make_def` is called only when the type doesn't exist yet; it receives the
    /// name and must return the TypeDef to register.
    pub fn get_or_register(
        &mut self,
        name: &str,
        registry: &mut TypeRegistry,
        make_def: impl FnOnce(&str) -> TypeDef,
    ) -> TypeId {
        if let Some(&id) = self.named_types.get(name) {
            return id;
        }
        let type_def = make_def(name);
        registry.add_type_def(type_def);
        let type_id = registry.insert(GirType::Named(name.to_string()));
        self.named_types.insert(name.to_string(), type_id);
        type_id
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
            PrimitiveType::StringView | PrimitiveType::CStr => self.string_view_type,
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
        } else if type_id == self.string_view_type || type_id == self.owned_string_type {
            "%s"
        } else {
            "%lld" // fallback
        }
    }

    /// Returns true if this type needs special printf handling (e.g., Str → two args).
    pub fn is_string_type(&self, type_id: TypeId) -> bool {
        type_id == self.string_view_type || type_id == self.owned_string_type
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

    // Map fields. String fields keep owned_string_type (GorgetString) so the struct
    // OWNS its strings and recursive drop frees them. Field LOADS return str_type (Str
    // view) to prevent shallow-copy double-frees — this is handled in lower_field_access.
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
    // Use map_ast_type_mut to ensure generic inner types (like Task[void])
    // get registered so the Option TypeDef references a valid TypeId.
    let inner_type = mapper.map_ast_type_mut(&type_args[0].node, registry);
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
        metadata: TypeMetadata {
            enum_category: Some(EnumCategory::Option),
            ..Default::default()
        },
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
        metadata: TypeMetadata {
            enum_category: Some(EnumCategory::Result),
            ..Default::default()
        },
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

    // Collection types are registered WITHOUT TypeDefs. Drops are handled by:
    //   - needs_drop() → name-based detection for direct collection locals
    //   - infer_drop_strategy() → name-based fallback in LIR lowering
    // NOT registering TypeDefs prevents the struct field scan from detecting
    // collection fields as "droppable", which would transitively upgrade structs
    // (like CliParser, HttpServer) to Recursive drop. Those structs often return
    // shallow copies of their collection fields → double-free if both are dropped.
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
                copy_semantics: CopySemantics::Resource,
                drop_strategy: DropStrategy::Trivial("free".to_string()),
                ..Default::default()
            },
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

/// Return the name suffix used when mangling a `meta op` parameter.
pub fn op_mangle_suffix(op: crate::parser::ast::BinaryOp) -> &'static str {
    use crate::parser::ast::BinaryOp;
    match op {
        BinaryOp::Add   => "add",
        BinaryOp::Sub   => "sub",
        BinaryOp::Mul   => "mul",
        BinaryOp::Div   => "div",
        BinaryOp::Eq    => "eq",
        BinaryOp::Neq   => "ne",
        BinaryOp::Lt    => "lt",
        BinaryOp::LtEq  => "le",
        BinaryOp::Gt    => "gt",
        BinaryOp::GtEq  => "ge",
        _               => "op",
    }
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
            PrimitiveType::StringView => "GorgetStringView".to_string(),
            PrimitiveType::CStr => "cstr".to_string(),
            PrimitiveType::StringType => "GorgetStringView".to_string(),
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
        Type::Ref(inner) => format!("Ref_{}", mangle_type_for_name(&inner.node)),
        Type::Owned(inner) => mangle_type_for_name(&inner.node),
        _ => "unknown".to_string(),
    }
}

// ── TypeDef factory helpers (used by get_or_register + ensure_*_type_def) ──

/// Create an opaque struct TypeDef with no fields (pointers, handles, etc.).
pub fn make_opaque_type_def(name: &str, copy_semantics: CopySemantics, drop_strategy: DropStrategy) -> TypeDef {
    TypeDef {
        name: name.to_string(),
        kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
        metadata: TypeMetadata {
            size: None,
            align: None,
            copy_semantics,
            drop_strategy,
            ..Default::default()
        },
    }
}

/// Create an Option[T] enum TypeDef (Some(_0: T) | None).
pub fn make_option_type_def(name: &str, inner_type: TypeId) -> TypeDef {
    TypeDef {
        name: name.to_string(),
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
        metadata: TypeMetadata {
            enum_category: Some(EnumCategory::Option),
            ..Default::default()
        },
    }
}

/// Create a Result[T, E] enum TypeDef (Ok(_0: T) | Error(_0: E)).
pub fn make_result_type_def(name: &str, ok_type: TypeId, err_type: TypeId) -> TypeDef {
    TypeDef {
        name: name.to_string(),
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
        metadata: TypeMetadata {
            enum_category: Some(EnumCategory::Result),
            ..Default::default()
        },
    }
}

/// Create a single-field wrapper TypeDef (Box[T], Shared[T], etc.).
pub fn make_wrapper_type_def(name: &str, inner_type: TypeId, copy_semantics: CopySemantics, drop_strategy: DropStrategy) -> TypeDef {
    TypeDef {
        name: name.to_string(),
        kind: TypeDefKind::Struct(StructDef {
            fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
        }),
        metadata: TypeMetadata {
            size: None,
            align: None,
            copy_semantics,
            drop_strategy,
            ..Default::default()
        },
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
        let str_id = mapper.map_primitive(&PrimitiveType::StringView);
        assert_eq!(str_id, mapper.string_view_type);
        assert!(matches!(reg.get(str_id), Some(GirType::Named(name)) if name == "GorgetStringView"));
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
        assert_eq!(mapper.format_specifier(mapper.string_view_type), "%s");
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
    fn try_map_distinguishes_unknown_from_void() {
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);

        // Void type → Some(UNIT_TYPE)
        let void_ty = Type::Primitive(PrimitiveType::Void);
        assert_eq!(mapper.try_map_ast_type(&void_ty), Some(UNIT_TYPE));

        // Empty tuple → Some(UNIT_TYPE)
        let empty_tuple = Type::Tuple(vec![]);
        assert_eq!(mapper.try_map_ast_type(&empty_tuple), Some(UNIT_TYPE));

        // Unknown named type → None (NOT Some(UNIT_TYPE))
        let unknown = Type::Named {
            name: spanned("Unknown".to_string()),
            generic_args: vec![],
        };
        assert_eq!(mapper.try_map_ast_type(&unknown), None);

        // Unknown generic → None
        let unknown_generic = Type::Named {
            name: spanned("Vector".to_string()),
            generic_args: vec![spanned(Type::Primitive(PrimitiveType::Int))],
        };
        assert_eq!(mapper.try_map_ast_type(&unknown_generic), None);

        // Known primitive → Some
        let int_ty = Type::Primitive(PrimitiveType::Int);
        assert_eq!(mapper.try_map_ast_type(&int_ty), Some(I64_TYPE));
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
                spanned(Type::Primitive(PrimitiveType::StringView)),
            ],
        );
        assert_eq!(name, "Result__GorgetStringView__GorgetStringView");
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
