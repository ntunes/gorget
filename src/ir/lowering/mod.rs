pub mod closures;
pub mod context;
pub mod drops;
pub mod exprs;
pub mod functions;
pub mod generics;
pub mod stmts;
pub mod traits;
pub mod types;

use crate::ir::types::*;
use crate::ir::{ExternDecl, Module};
use crate::parser::ast::{self, FunctionBody, Item};
use crate::semantic::AnalysisResult;

use context::LoweringContext;
use functions::lower_function;
use generics::GenericCollector;
use types::TypeMapper;

/// Lower an AST module + analysis result into a GIR module.
pub fn lower_module(
    ast_module: &ast::Module,
    analysis: &AnalysisResult,
) -> Module {
    let mut module = Module::new();

    // Create type mapper
    let mut type_mapper = TypeMapper::new(&mut module.type_registry);

    // Pre-scan: collect generic templates for monomorphization of field types
    let generic_templates: Vec<&ast::Item> = ast_module.items.iter()
        .filter(|item| match &item.node {
            Item::Struct(s) => s.generic_params.is_some(),
            Item::Enum(e) => e.generic_params.is_some(),
            _ => false,
        })
        .map(|item| &item.node)
        .collect();

    // Pre-scan pass 1: register ALL non-generic type NAMES before filling in fields.
    // This prevents UNIT_TYPE field values from mutual/forward references (e.g., struct A
    // has a field of type B, and B is defined later in the file). Without this pre-pass,
    // the field type resolution falls back to UNIT_TYPE, which marks the type as a
    // generic template placeholder and suppresses its C output.
    for item in &ast_module.items {
        match &item.node {
            Item::Struct(s) if s.generic_params.is_none() => {
                let name = &s.name.node;
                if !type_mapper.named_types.contains_key(name.as_str()) {
                    let tid = module.type_registry.insert(GirType::Named(name.clone()));
                    type_mapper.register_named(name.clone(), tid);
                }
            }
            Item::Enum(e) if e.generic_params.is_none() => {
                let name = &e.name.node;
                if !type_mapper.named_types.contains_key(name.as_str()) {
                    let tid = module.type_registry.insert(GirType::Named(name.clone()));
                    type_mapper.register_named(name.clone(), tid);
                }
            }
            Item::Newtype(nt) => {
                let name = &nt.name.node;
                if !type_mapper.named_types.contains_key(name.as_str()) {
                    let tid = module.type_registry.insert(GirType::Named(name.clone()));
                    type_mapper.register_named(name.clone(), tid);
                }
            }
            _ => {}
        }
    }

    // Pre-scan pass 2: register non-generic struct and enum type definitions
    for item in &ast_module.items {
        match &item.node {
            Item::Struct(struct_def) => {
                types::register_struct_type(
                    &mut type_mapper,
                    &mut module.type_registry,
                    struct_def,
                    &generic_templates,
                );
            }
            Item::Enum(enum_def) => {
                types::register_enum_type(
                    &mut type_mapper,
                    &mut module.type_registry,
                    enum_def,
                    &generic_templates,
                );
            }
            Item::Newtype(nt) => {
                types::register_newtype(
                    &mut type_mapper,
                    &mut module.type_registry,
                    nt,
                );
            }
            _ => {}
        }
    }

    // Register opaque allocator pointer types (runtime types not defined in .gg source).
    // These C functions return pointers (e.g., gorget_pool_new → GorgetPoolAllocator*),
    // so we register them as Ptr(Named(...)) so that method-call lowering skips the
    // extra borrow (is_ptr check in lower_method_call).
    {
        let alloc_types: &[(&str, &str)] = &[
            ("Arena", "GorgetArena"),
            ("TrackingAllocator", "GorgetTrackingAllocator"),
            ("PoolAllocator", "GorgetPoolAllocator"),
        ];
        for &(gorget_name, c_name) in alloc_types {
            let inner = module.type_registry.insert(GirType::Named(c_name.to_string()));
            // Use MutPtr since allocators are passed to non-const functions (destroy, bytes_used, etc.)
            let ptr = module.type_registry.insert(GirType::MutPtr(inner));
            type_mapper.register_named(gorget_name.to_string(), ptr);
        }
    }

    // Scan for `equip T with Drop:` blocks and upgrade type metadata to Move + Custom drop
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            if let Some(trait_ref) = &equip.trait_ {
                let trait_name_str = match &trait_ref.trait_name.node {
                    ast::Type::Named { name, .. } => name.node.as_str(),
                    _ => "",
                };
                if trait_name_str == "Drop" {
                    if let ast::Type::Named { name: type_name, .. } = &equip.type_.node {
                        let name_str = &type_name.node;
                        // Upgrade the TypeDef metadata
                        if let Some(type_def) = module.type_registry.get_type_def_mut(name_str) {
                            type_def.metadata.copy_semantics = CopySemantics::Move;
                            type_def.metadata.drop_strategy = DropStrategy::Custom(format!("{name_str}__drop"));
                        }
                    }
                }
            }
        }
    }

    // Scan for structs with droppable fields — mark as Recursive drop if not already Custom
    // This ensures auto field drops fire for structs like `Wrapper { inner: Inner }` where
    // Inner has Drop but Wrapper does not.
    {
        // First collect which type names need dropping
        let droppable_names: Vec<String> = module.type_registry.all_type_def_names()
            .filter(|name| {
                if let Some(td) = module.type_registry.get_type_def(name) {
                    td.metadata.copy_semantics == CopySemantics::Move
                        || td.metadata.drop_strategy != DropStrategy::None
                } else {
                    false
                }
            })
            .cloned()
            .collect();

        // Now scan all structs for fields whose type name is in droppable_names
        let struct_names: Vec<String> = module.type_registry.all_type_def_names().cloned().collect();
        for name in &struct_names {
            let needs_upgrade = {
                let td = match module.type_registry.get_type_def(name) {
                    Some(td) => td,
                    None => continue,
                };
                // Skip if already has a drop strategy (Custom or Recursive)
                if td.metadata.drop_strategy != DropStrategy::None {
                    // But check if Custom drop also needs field drops
                    if matches!(td.metadata.drop_strategy, DropStrategy::Custom(_)) {
                        // Check if it has droppable fields — if so, we need to know at codegen time
                        if let TypeDefKind::Struct(ref sdef) = td.kind {
                            let has_droppable_fields = sdef.fields.iter().any(|f| {
                                if let Some(GirType::Named(field_type_name)) = module.type_registry.get(f.type_id) {
                                    droppable_names.contains(field_type_name)
                                } else {
                                    false
                                }
                            });
                            has_droppable_fields // need to upgrade so C backend emits field drops
                        } else {
                            false
                        }
                    } else {
                        continue; // Already Trivial or Recursive, skip
                    }
                } else if let TypeDefKind::Struct(ref sdef) = td.kind {
                    sdef.fields.iter().any(|f| {
                        if let Some(GirType::Named(field_type_name)) = module.type_registry.get(f.type_id) {
                            droppable_names.contains(field_type_name)
                        } else {
                            false
                        }
                    })
                } else {
                    false
                }
            };

            if needs_upgrade {
                if let Some(td) = module.type_registry.get_type_def_mut(name) {
                    if td.metadata.drop_strategy == DropStrategy::None {
                        td.metadata.drop_strategy = DropStrategy::Recursive;
                    }
                    td.metadata.copy_semantics = CopySemantics::Move;
                }
            }
        }
    }

    // Register runtime types needed by expression lowering
    // Str and GorgetString: register in named_types so method dispatch can find them
    type_mapper.register_named("Str".to_string(), type_mapper.str_type);
    type_mapper.register_named("GorgetString".to_string(), type_mapper.owned_string_type);

    // GorgetArray: opaque runtime array (element_size, data, len, cap)
    {
        module.type_registry.add_type_def(TypeDef {
            name: "GorgetArray".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("gorget_array_free".to_string()),
                copy_semantics: CopySemantics::Move,
            },
        });
        let array_type_id = module.type_registry.insert(GirType::Named("GorgetArray".to_string()));
        type_mapper.register_named("GorgetArray".to_string(), array_type_id);
    }
    // GorgetMap: runtime hash-map (Dict/HashMap both map to this)
    {
        module.type_registry.add_type_def(TypeDef {
            name: "GorgetMap".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("gorget_map_free".to_string()),
                copy_semantics: CopySemantics::Move,
            },
        });
        let map_type_id = module.type_registry.insert(GirType::Named("GorgetMap".to_string()));
        type_mapper.register_named("GorgetMap".to_string(), map_type_id);
    }
    // GorgetSet: runtime hash-set (thin wrapper over GorgetMap)
    {
        module.type_registry.add_type_def(TypeDef {
            name: "GorgetSet".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                drop_strategy: DropStrategy::Trivial("gorget_set_free".to_string()),
                copy_semantics: CopySemantics::Move,
            },
        });
        let set_type_id = module.type_registry.insert(GirType::Named("GorgetSet".to_string()));
        type_mapper.register_named("GorgetSet".to_string(), set_type_id);
    }
    // NOTE: Typed vector aliases (Vector__Str, etc.) are registered AFTER generic
    // monomorphization to avoid conflicting with the generic collector's type registration.
    // See post-monomorphization section below.
    // GorgetRange: standalone range value (start, end, inclusive)
    {
        let range_def = TypeDef {
            name: "GorgetRange".to_string(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "start".to_string(), type_id: I64_TYPE },
                    StructField { name: "end".to_string(), type_id: I64_TYPE },
                    StructField { name: "inclusive".to_string(), type_id: BOOL_TYPE },
                ],
            }),
            metadata: TypeMetadata {
                copy_semantics: CopySemantics::Copy,
                ..TypeMetadata::default()
            },
        };
        module.type_registry.add_type_def(range_def);
        let range_type_id = module.type_registry.insert(GirType::Named("GorgetRange".to_string()));
        type_mapper.register_named("GorgetRange".to_string(), range_type_id);
    }

    // P2.3: Generic monomorphization — collect templates, discover usages, monomorphize
    let mut generic_collector = GenericCollector::new();
    generic_collector.collect_templates(ast_module);
    generic_collector.discover_usages(ast_module);

    // Pre-register collection type names BEFORE monomorphization so that
    // monomorphize_enum can resolve inner types like Vector[int] in Option[Vector[int]].
    for (base_name, _type_args, mangled_name, kind) in generic_collector.instances_raw() {
        if !matches!(kind, generics::TemplateKind::Struct | generics::TemplateKind::Enum) {
            continue;
        }
        let is_collection = matches!(base_name.as_str(),
            "Vector" | "List" | "Array" | "Dict" | "HashMap" | "Set" | "HashSet");
        if is_collection && !type_mapper.named_types.contains_key(mangled_name) {
            let drop_fn = match base_name.as_str() {
                "Dict" | "HashMap" => "gorget_map_free",
                "Set" | "HashSet" => "gorget_set_free",
                _ => "gorget_array_free",
            };
            module.type_registry.add_type_def(TypeDef {
                name: mangled_name.clone(),
                kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                metadata: TypeMetadata {
                    size: None,
                    align: None,
                    drop_strategy: DropStrategy::Trivial(drop_fn.to_string()),
                    copy_semantics: CopySemantics::Move,
                },
            });
            let tid = module.type_registry.insert(GirType::Named(mangled_name.clone()));
            type_mapper.register_named(mangled_name.clone(), tid);
        }
    }

    generic_collector.monomorphize_types(&mut type_mapper, &mut module.type_registry);

    // Register typed vector aliases AFTER generic monomorphization so we reuse
    // any TypeIds already created by the generic collector (e.g., Vector__Str from
    // explicit Vector[str] usage in source code).
    for (name, gir_name) in &[
        ("Vector__Str", "Vector__Str"),
        ("Vector__uint8_t", "Vector__uint8_t"),
        ("Vector__int64_t", "Vector__int64_t"),
    ] {
        if !type_mapper.named_types.contains_key(*name) {
            if !module.type_registry.has_type_def(gir_name) {
                module.type_registry.add_type_def(TypeDef {
                    name: gir_name.to_string(),
                    kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                    metadata: TypeMetadata {
                        size: None,
                        align: None,
                        drop_strategy: DropStrategy::Trivial("gorget_array_free".to_string()),
                        copy_semantics: CopySemantics::Move,
                    },
                });
            }
            let tid = module.type_registry.insert(GirType::Named(gir_name.to_string()));
            type_mapper.register_named(name.to_string(), tid);
        }
    }

    // Register printf as an extern (variadic)
    module.externs.push(ExternDecl {
        name: "printf".into(),
        params: vec![], // variadic — actual params vary per call
        return_type: I32_TYPE,
        is_variadic: true,
    });

    // Move type_registry into LoweringContext for the lowering phase
    let type_registry = std::mem::take(&mut module.type_registry);
    let mut ctx = LoweringContext::new(analysis, type_mapper, type_registry);

    // Extract directive flags from AST
    for item in &ast_module.items {
        if let Item::Directive(d) = &item.node {
            match d.name.as_str() {
                "strip-asserts" => ctx.strip_asserts = true,
                "overflow" if d.value.as_deref() == Some("wrap") => ctx.overflow_wrap = true,
                _ => {}
            }
        }
    }

    // Register well-known stdlib constants
    {
        use crate::ir::instructions::Constant;
        ctx.module_constants.insert("PI".into(), Constant::F64(std::f64::consts::PI));
        ctx.module_constants.insert("E".into(), Constant::F64(std::f64::consts::E));
        ctx.module_constants.insert("TAU".into(), Constant::F64(std::f64::consts::TAU));
        ctx.module_constants.insert("INFINITY".into(), Constant::F64(f64::INFINITY));
        ctx.module_constants.insert("NAN".into(), Constant::F64(f64::NAN));
        ctx.module_constants.insert("INT_MAX".into(), Constant::I64(i64::MAX));
        ctx.module_constants.insert("INT_MIN".into(), Constant::I64(i64::MIN));
    }
    // Scan for module-level const and meta declarations
    for item in &ast_module.items {
        if let Item::ConstDecl(const_def) = &item.node {
            if let Some(val) = eval_const_expr(&const_def.value.node, &ctx.module_constants) {
                ctx.module_constants.insert(const_def.name.node.clone(), val);
            }
        }
        if let Item::MetaConst(mc) = &item.node {
            if let Some(val) = eval_const_expr(&mc.value.node, &ctx.module_constants) {
                ctx.module_constants.insert(mc.name.node.clone(), val);
            }
        }
        // Handle meta if blocks — extract nested meta constants
        if let Item::MetaIf(meta_if) = &item.node {
            // Evaluate condition
            let cond = eval_const_expr(&meta_if.condition.node, &ctx.module_constants);
            let active = matches!(cond, Some(crate::ir::instructions::Constant::Bool(true)));
            if active {
                for sub_item in &meta_if.then_items {
                    if let Item::MetaConst(mc) = &sub_item.node {
                        if let Some(val) = eval_const_expr(&mc.value.node, &ctx.module_constants) {
                            ctx.module_constants.insert(mc.name.node.clone(), val);
                        }
                    }
                }
            }
        }
    }

    // P2.5: Register VTable and TraitObj types for all trait definitions
    let trait_info = traits::register_trait_types(&mut ctx, ast_module);

    // Populate struct field info cache (includes monomorphized + vtable/trait obj types)
    ctx.populate_struct_fields();

    // Pre-scan: register enum variant → (enum_name, variant_name) mappings
    for item in &ast_module.items {
        if let Item::Enum(enum_def) = &item.node {
            if enum_def.generic_params.is_some() {
                continue;
            }
            let enum_name = &enum_def.name.node;
            for variant in &enum_def.variants {
                let variant_name = &variant.node.name.node;
                ctx.enum_variants.insert(
                    variant_name.clone(),
                    (enum_name.clone(), variant_name.clone()),
                );
            }
        }
    }

    // Register monomorphized enum variants from TypeDefs
    for type_def in ctx.type_registry.type_defs() {
        if let TypeDefKind::Enum(ref e) = type_def.kind {
            // Only register for monomorphized types (contain __ separator)
            if type_def.name.contains("__") {
                for variant in &e.variants {
                    ctx.enum_variants.insert(
                        variant.name.clone(),
                        (type_def.name.clone(), variant.name.clone()),
                    );
                }
            }
        }
    }

    // Pre-scan: build fn_sigs map for all non-generic functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // Generic functions handled via monomorphization
            }
            let name = &func.name.node;
            let is_main = name == "main";

            let ret_type = if is_main {
                I32_TYPE
            } else if func.throws.is_some() {
                // `int foo() throws str` → Result[int, str]
                let ok_type = ctx.type_mapper.map_ast_type_mut(&func.return_type.node, &mut ctx.type_registry);
                let err_type = ctx.type_mapper.map_ast_type_mut(&func.throws.as_ref().unwrap().node, &mut ctx.type_registry);
                let ok_c = crate::ir::lowering::types::mangle_type_for_name(&func.return_type.node);
                let err_c = crate::ir::lowering::types::mangle_type_for_name(&func.throws.as_ref().unwrap().node);
                let result_name = format!("Result__{ok_c}__{err_c}");
                if let Some(&id) = ctx.type_mapper.named_types.get(&result_name) {
                    id
                } else {
                    use crate::ir::types::*;
                    let type_def = TypeDef {
                        name: result_name.clone(),
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
                    ctx.type_registry.add_type_def(type_def);
                    let type_id = ctx.type_registry.insert(GirType::Named(result_name.clone()));
                    ctx.type_mapper.register_named(result_name, type_id);
                    type_id
                }
            } else {
                // Use map_ast_type_mut so tuple return types get registered on the fly
                ctx.type_mapper.map_ast_type_mut(&func.return_type.node, &mut ctx.type_registry)
            };

            let param_types: Vec<TypeId> = func
                .params
                .iter()
                .map(|p| ctx.type_mapper.map_ast_type_mut(&p.node.type_.node, &mut ctx.type_registry))
                .collect();

            ctx.fn_sigs.insert(name.clone(), (param_types, ret_type));

            // Record parameter names for named-arg reordering
            let param_names: Vec<String> = func.params.iter()
                .map(|p| p.node.name.node.clone())
                .collect();
            ctx.fn_param_names.insert(name.clone(), param_names);

            // Record default parameter values
            let defaults: Vec<(usize, ast::Expr)> = func.params.iter()
                .enumerate()
                .filter_map(|(i, p)| {
                    p.node.default.as_ref().map(|d| (i, d.node.clone()))
                })
                .collect();
            if !defaults.is_empty() {
                ctx.fn_defaults.insert(name.clone(), defaults);
            }

            // Record extern binding: Gorget name → C symbol
            if let FunctionBody::Extern(c_symbol) = &func.body {
                ctx.extern_bindings.insert(name.clone(), c_symbol.clone());
            }
        }
    }

    // Register monomorphized function signatures
    generic_collector.register_fn_sigs(&ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs);

    // Pre-scan: register non-generic equip method signatures
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            // Skip generic equip blocks and trait equip blocks
            if equip.generic_params.is_some() || equip.trait_.is_some() {
                continue;
            }
            // Skip equip blocks on generic types (they're handled via monomorphization)
            if let ast::Type::Named { generic_args, .. } = &equip.type_.node {
                if !generic_args.is_empty() {
                    continue;
                }
            }

            if let ast::Type::Named { name: type_name, .. } = &equip.type_.node {
                for method in &equip.items {
                    let method_def = &method.node;
                    let mangled = format!("{}__{}", type_name.node, method_def.name.node);

                    let ret_type = ctx.type_mapper.map_ast_type_mut(&method_def.return_type.node, &mut ctx.type_registry);
                    let has_self = method_def.params.first()
                        .map(|p| p.node.name.node == "self")
                        .unwrap_or(false);

                    let mut param_types = Vec::new();
                    if has_self {
                        let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                        let self_is_mutable = method_def.params.first()
                            .map(|p| matches!(p.node.ownership, ast::Ownership::MutableBorrow))
                            .unwrap_or(false);
                        let self_ptr_type = if self_is_mutable {
                            ctx.register_mut_ptr_type(self_type_id)
                        } else {
                            ctx.register_ptr_type(self_type_id)
                        };
                        param_types.push(self_ptr_type);
                    }
                    for p in &method_def.params {
                        if p.node.name.node == "self" {
                            continue; // self handled above
                        }
                        param_types.push(ctx.type_mapper.map_ast_type(&p.node.type_.node));
                    }

                    ctx.fn_sigs.insert(mangled.clone(), (param_types, ret_type));

                    // Register extern binding for equip methods (e.g., UdpSocket__local_addr → gorget_udp_local_addr)
                    if let FunctionBody::Extern(c_symbol) = &method_def.body {
                        ctx.extern_bindings.insert(mangled, c_symbol.clone());
                    }
                }
            }
        }
    }

    // Register monomorphized equip method signatures (including default trait methods)
    generic_collector.register_equip_sigs_with_defaults(
        &ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs, Some(ast_module));

    // Register built-in method signatures for Option/Result instantiations.
    // These methods are inlined by the C backend (not real functions), but
    // fn_sigs must know about them so the lowering creates properly-typed locals.
    register_builtin_enum_method_sigs(&mut ctx, &generic_collector);
    // Register built-in collection method signatures (Vector, Dict, HashMap, etc.)
    register_collection_method_sigs(&mut ctx, &generic_collector);

    // P2.5: Register trait equip method signatures
    traits::register_trait_equip_sigs(&mut ctx, &trait_info, ast_module);

    // Register fn_sigs for trait equip blocks with unregistered traits
    // (built-in traits like From, Default, Equatable, Displayable, etc.)
    traits::register_unregistered_trait_equip_sigs(&mut ctx, &trait_info, ast_module);

    // Register runtime built-in method signatures (Str methods, etc.)
    {
        let str_type = ctx.type_mapper.str_type;
        let owned_str_type = ctx.type_mapper.owned_string_type;
        let array_type = ctx.type_mapper.named_types.get("GorgetArray").copied()
            .unwrap_or(UNIT_TYPE);

        // Str methods taking (self) returning various types
        let str_self = vec![str_type]; // &self lowered as Str ptr, but sig says Str
        let str_str = vec![str_type, str_type];

        // Methods returning typed Vector (element type embedded in name)
        let vec_str_type = ctx.type_mapper.named_types.get("Vector__Str").copied()
            .unwrap_or(array_type);
        let vec_u8_type = ctx.type_mapper.named_types.get("Vector__uint8_t").copied()
            .unwrap_or(array_type);
        let vec_i64_type = ctx.type_mapper.named_types.get("Vector__int64_t").copied()
            .unwrap_or(array_type);
        ctx.fn_sigs.insert("Str__bytes".to_string(), (str_self.clone(), vec_u8_type));
        ctx.fn_sigs.insert("Str__codepoints".to_string(), (str_self.clone(), vec_i64_type));
        ctx.fn_sigs.insert("Str__chars".to_string(), (str_self.clone(), vec_str_type));
        ctx.fn_sigs.insert("Str__split".to_string(), (str_str.clone(), vec_str_type));
        // Methods returning Str
        for m in &["trim", "strip", "lstrip", "rstrip", "removeprefix", "removesuffix"] {
            ctx.fn_sigs.insert(format!("Str__{m}"), (str_self.clone(), str_type));
        }
        ctx.fn_sigs.insert("Str__byte_slice".to_string(), (vec![str_type, I64_TYPE, I64_TYPE], str_type));
        // char_at returns char (uint32_t) at the semantic level.
        // The C backend handles the Str→uint32_t conversion via inline helper.
        ctx.fn_sigs.insert("Str__char_at".to_string(), (vec![str_type, I64_TYPE], CHAR_TYPE));
        // Methods returning GorgetString
        for m in &["to_upper", "to_lower"] {
            ctx.fn_sigs.insert(format!("Str__{m}"), (str_self.clone(), owned_str_type));
        }
        ctx.fn_sigs.insert("Str__replace".to_string(), (vec![str_type, str_type, str_type], owned_str_type));
        ctx.fn_sigs.insert("Str__repeat".to_string(), (vec![str_type, I64_TYPE], owned_str_type));
        ctx.fn_sigs.insert("Str__pad_left".to_string(), (vec![str_type, I64_TYPE, str_type], owned_str_type));
        ctx.fn_sigs.insert("Str__pad_right".to_string(), (vec![str_type, I64_TYPE, str_type], owned_str_type));
        // Methods returning int64_t
        for m in &["len", "byte_len", "index_of", "count", "find"] {
            let params = if *m == "len" || *m == "byte_len" {
                str_self.clone()
            } else {
                str_str.clone()
            };
            ctx.fn_sigs.insert(format!("Str__{m}"), (params, I64_TYPE));
        }
        // Methods returning bool
        for m in &["contains", "starts_with", "ends_with", "is_empty"] {
            let params = if *m == "is_empty" { str_self.clone() } else { str_str.clone() };
            ctx.fn_sigs.insert(format!("Str__{m}"), (params, BOOL_TYPE));
        }
        ctx.fn_sigs.insert("Str__eq".to_string(), (str_str.clone(), BOOL_TYPE));
        ctx.fn_sigs.insert("Str__join".to_string(), (vec![str_type, array_type], owned_str_type));
    }

    // Register char builtin method signatures
    {
        let char_self = vec![CHAR_TYPE];
        for m in &["is_alpha", "is_digit", "is_alphanumeric", "is_whitespace",
                    "is_upper", "is_lower", "is_ascii", "is_hex_digit"] {
            ctx.fn_sigs.insert(format!("char__{m}"), (char_self.clone(), BOOL_TYPE));
        }
        ctx.fn_sigs.insert("char__to_upper".to_string(), (char_self.clone(), CHAR_TYPE));
        ctx.fn_sigs.insert("char__to_lower".to_string(), (char_self.clone(), CHAR_TYPE));
    }

    // Register primitive static method signatures (int.parse, int.default, etc.)
    {
        let str_type = ctx.type_mapper.str_type;
        // Create Option TypeIds for parse results WITHOUT registering in named_types
        // (registering in named_types would cause infer_collection_method_return_type
        // to find these types when looking up Vector.get etc., even when the
        // Option type definitions haven't been emitted in C)
        let opt_int_type = ctx.type_mapper.named_types.get("Option__int64_t").copied()
            .unwrap_or_else(|| {
                module.type_registry.insert(GirType::Named("Option__int64_t".to_string()))
            });
        let opt_float_type = ctx.type_mapper.named_types.get("Option__double").copied()
            .unwrap_or_else(|| {
                module.type_registry.insert(GirType::Named("Option__double".to_string()))
            });
        let opt_bool_type = ctx.type_mapper.named_types.get("Option__bool").copied()
            .unwrap_or_else(|| {
                module.type_registry.insert(GirType::Named("Option__bool".to_string()))
            });
        // int.parse(str) → Option[int], int.default() → int
        ctx.fn_sigs.insert("int64_t__parse".to_string(), (vec![str_type], opt_int_type));
        ctx.fn_sigs.insert("int64_t__default".to_string(), (vec![], I64_TYPE));
        // float.parse(str) → Option[float], float.default() → float
        ctx.fn_sigs.insert("double__parse".to_string(), (vec![str_type], opt_float_type));
        ctx.fn_sigs.insert("double__default".to_string(), (vec![], F64_TYPE));
        // bool.parse(str) → Option[bool], bool.default() → bool
        ctx.fn_sigs.insert("bool__parse".to_string(), (vec![str_type], opt_bool_type));
        ctx.fn_sigs.insert("bool__default".to_string(), (vec![], BOOL_TYPE));
    }

    // Lower all non-generic functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // Generic functions are lowered as monomorphized instances
            }
            lower_function(&mut ctx, &mut module, func);
        }
    }

    // Lower monomorphized generic function instances
    for (base_name, type_args, mangled_name) in generic_collector.function_instances() {
        if let Some(template) = generic_collector.get_fn_template(base_name) {
            functions::lower_generic_function(
                &mut ctx,
                &mut module,
                template,
                type_args,
                mangled_name,
            );
        }
    }

    // Lower non-generic equip blocks as functions
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            if equip.generic_params.is_some() || equip.trait_.is_some() {
                continue;
            }
            // Skip equip blocks on generic types
            if let ast::Type::Named { generic_args, .. } = &equip.type_.node {
                if !generic_args.is_empty() {
                    continue;
                }
            }

            if let ast::Type::Named { name: type_name, .. } = &equip.type_.node {
                for method in &equip.items {
                    functions::lower_equip_method(
                        &mut ctx,
                        &mut module,
                        &method.node,
                        &type_name.node,
                        &equip.type_.node,
                    );
                }
            }
        }
    }

    // Lower monomorphized equip method instances
    for (base_name, type_args, mangled_type_name) in generic_collector.equip_instances() {
        if let Some(equip_blocks) = generic_collector.get_equip_templates(base_name) {
            let equip_blocks = equip_blocks.clone();
            for equip in &equip_blocks {
                functions::lower_generic_equip_methods_with_defaults(
                    &mut ctx,
                    &mut module,
                    equip,
                    type_args,
                    mangled_type_name,
                    Some(ast_module),
                );
            }
        }
    }

    // Lower test items: each test becomes a void function, then generate test runner main
    let has_tests = ast_module.items.iter().any(|item| matches!(&item.node, Item::Test(_)));
    let has_main = ast_module.items.iter().any(|item| matches!(&item.node, Item::Function(f) if f.name.node == "main"));
    if has_tests && !has_main {
        lower_test_items(&mut ctx, &mut module, ast_module);
    }

    // P2.5: Lower trait equip methods and emit vtable globals
    traits::lower_trait_equip_methods(&mut ctx, &mut module, &trait_info, ast_module);
    traits::emit_vtable_globals(&mut module, &trait_info, ast_module);

    // Lower trait equip blocks with unregistered traits (From, Default, Equatable, etc.)
    traits::lower_unregistered_trait_equip_methods(&mut ctx, &mut module, &trait_info, ast_module);

    // P2.4: Emit lifted closure call functions
    let closures = std::mem::take(&mut ctx.closures);
    for lifted in &closures.lifted {
        let func = closures::emit_closure_call_function(&mut ctx, lifted);
        module.functions.push(func);
    }

    // Move type_registry back to module for validation
    module.type_registry = std::mem::take(&mut ctx.type_registry);

    // Auto-register all CallExtern targets as externs if not already known.
    // This handles runtime functions (gorget_throw, gorget_array_new, etc.)
    // without needing to enumerate each one manually.
    auto_register_externs(&mut module);

    // Validate the resulting module
    let errors = crate::ir::validate::validate(&module);
    if !errors.is_empty() {
        eprintln!("GIR validation errors:");
        for err in &errors {
            eprintln!("  {}", err);
        }
        panic!("GIR module failed validation ({} errors)", errors.len());
    }

    // Propagate directive flags to module
    module.overflow_wrap = ctx.overflow_wrap;

    // Collect channel element types (Channel__T → T) for C backend wrapper emission
    for name in module.type_registry.all_type_def_names() {
        if let Some(elem) = name.strip_prefix("Channel__") {
            if !module.channel_types.contains(&elem.to_string()) {
                module.channel_types.push(elem.to_string());
            }
        }
    }

    // Collect spawned function metadata for C backend spawn/await helper emission
    for (fn_name, _) in &ctx.spawned_fn_names {
        if let Some((param_types, ret_type)) = ctx.fn_sigs.get(fn_name.as_str()) {
            let param_names = ctx.fn_param_names.get(fn_name.as_str())
                .cloned()
                .unwrap_or_else(|| {
                    param_types.iter().enumerate().map(|(i, _)| format!("__p{i}")).collect()
                });
            let params: Vec<(String, TypeId)> = param_names.iter()
                .zip(param_types.iter())
                .map(|(n, &t)| (n.clone(), t))
                .collect();
            module.spawned_fns.push((fn_name.clone(), params, *ret_type));
        }
    }
    module.has_spawn = !ctx.spawned_fn_names.is_empty();

    module
}

/// Lower test items into test functions and generate a test runner main().
fn lower_test_items(
    ctx: &mut LoweringContext,
    module: &mut Module,
    ast_module: &ast::Module,
) {
    use crate::ir::builder::FunctionBuilder;

    let mut test_fn_names: Vec<(String, String)> = Vec::new(); // (fn_name, test_name)

    // Lower each test body as a standalone void function
    for (idx, item) in ast_module.items.iter().enumerate() {
        if let Item::Test(test_def) = &item.node {
            let fn_name = format!("__test_{idx}");
            let test_name = test_def.name.node.clone();

            let mut builder = FunctionBuilder::new(&fn_name, UNIT_TYPE, &[]);
            ctx.clear_locals();

            ctx.drops.push_scope(drops::DropScopeKind::Function);
            stmts::lower_block(ctx, &mut builder, &test_def.body);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }

            module.functions.push(builder.build());
            test_fn_names.push((fn_name, test_name));
        }
    }

    // Store test function names in module metadata for C backend to generate test runner main.
    for (fn_name, test_name) in test_fn_names {
        module.test_fns.push((fn_name, test_name));
    }
}

/// Scan all functions for `CallExtern` and `Invoke` targets and auto-register
/// any that aren't already declared as functions or externs.
fn auto_register_externs(module: &mut Module) {
    use crate::ir::instructions::{Instruction, Terminator};
    use rustc_hash::FxHashSet;

    // Collect known callables
    let mut known: FxHashSet<String> = FxHashSet::default();
    for f in &module.functions {
        known.insert(f.name.clone());
    }
    for e in &module.externs {
        known.insert(e.name.clone());
    }

    // Collect all call targets that are missing
    let mut missing: FxHashSet<String> = FxHashSet::default();
    for func in &module.functions {
        // Build a set of parameter local name hints for this function
        // (Callable params get lowered as void* locals with the param name)
        let param_names: FxHashSet<&str> = func.locals.iter()
            .filter_map(|l| l.name_hint.as_deref())
            .collect();
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::CallExtern { func: name, .. }
                    | Instruction::Call { func: name, .. } => {
                        if !known.contains(name.as_str())
                            && !param_names.contains(name.as_str())
                        {
                            missing.insert(name.clone());
                        }
                    }
                    _ => {}
                }
            }
            if let Some(ref term) = block.terminator {
                if let Terminator::Invoke { func: name, .. } = term {
                    if !known.contains(name.as_str())
                        && !param_names.contains(name.as_str())
                    {
                        missing.insert(name.clone());
                    }
                }
            }
        }
    }

    // Register each missing target as a variadic extern (safe fallback)
    for name in missing {
        module.externs.push(ExternDecl {
            name,
            params: vec![],
            return_type: I32_TYPE,
            is_variadic: true,
        });
    }
}

/// Register fn_sigs for built-in Option/Result methods per discovered instantiation.
/// These methods are inlined by the C backend as statement expressions, but the
/// lowering needs their signatures to create properly-typed locals.
fn register_builtin_enum_method_sigs(
    ctx: &mut LoweringContext,
    collector: &GenericCollector,
) {
    use crate::ir::types::TypeDefKind;

    // Collect signatures first (to avoid borrow conflicts on ctx)
    let mut sigs_to_add: Vec<(String, Vec<TypeId>, TypeId)> = Vec::new();

    for (base_name, mangled_name) in collector.type_instances() {
        let type_def = ctx.type_registry.get_type_def(mangled_name);
        let type_def = match type_def {
            Some(td) => td.clone(),
            None => continue,
        };
        let variants = match &type_def.kind {
            TypeDefKind::Enum(e) => e.variants.clone(),
            _ => continue,
        };

        let self_type = ctx.type_mapper.lookup_named(mangled_name).unwrap_or(UNIT_TYPE);
        let self_ptr = ctx.register_ptr_type(self_type);

        if base_name == "Option" {
            let inner_type = variants.iter()
                .find(|v| v.name == "Some")
                .and_then(|v| v.fields.first())
                .map(|f| f.type_id)
                .unwrap_or(I64_TYPE);

            let self_param = vec![self_ptr];
            sigs_to_add.push((format!("{mangled_name}__unwrap"), self_param.clone(), inner_type));
            sigs_to_add.push((format!("{mangled_name}__expect"), vec![self_ptr, ctx.type_mapper.str_type], inner_type));
            sigs_to_add.push((format!("{mangled_name}__unwrap_or"), vec![self_ptr, inner_type], inner_type));
            sigs_to_add.push((format!("{mangled_name}__is_some"), self_param.clone(), BOOL_TYPE));
            sigs_to_add.push((format!("{mangled_name}__is_none"), self_param, BOOL_TYPE));
        } else if base_name == "Result" {
            let ok_type = variants.iter()
                .find(|v| v.name == "Ok")
                .and_then(|v| v.fields.first())
                .map(|f| f.type_id)
                .unwrap_or(I64_TYPE);

            let self_param = vec![self_ptr];
            sigs_to_add.push((format!("{mangled_name}__unwrap"), self_param.clone(), ok_type));
            sigs_to_add.push((format!("{mangled_name}__expect"), vec![self_ptr, ctx.type_mapper.str_type], ok_type));
            sigs_to_add.push((format!("{mangled_name}__unwrap_or"), vec![self_ptr, ok_type], ok_type));
            sigs_to_add.push((format!("{mangled_name}__is_ok"), self_param.clone(), BOOL_TYPE));
            sigs_to_add.push((format!("{mangled_name}__is_err"), self_param, BOOL_TYPE));
        }
    }

    for (name, params, ret) in sigs_to_add {
        ctx.fn_sigs.insert(name, (params, ret));
    }
}

/// Register fn_sigs for built-in collection methods (Vector, Dict, HashMap, etc.)
fn register_collection_method_sigs(
    ctx: &mut LoweringContext,
    collector: &GenericCollector,
) {
    use crate::ir::types::TypeDefKind;

    let array_type = ctx.type_mapper.named_types.get("GorgetArray").copied()
        .unwrap_or(UNIT_TYPE);

    for (base_name, mangled_name) in collector.type_instances() {
        let self_type = ctx.type_mapper.lookup_named(mangled_name).unwrap_or(UNIT_TYPE);
        let self_ptr = ctx.register_ptr_type(self_type);

        // Get element type from the struct's first generic field
        let elem_type = ctx.type_registry.get_type_def(mangled_name)
            .and_then(|td| match &td.kind {
                TypeDefKind::Struct(s) => s.fields.first().map(|f| f.type_id),
                _ => None,
            })
            .unwrap_or(I64_TYPE);

        match base_name {
            "Vector" => {
                let sigs = vec![
                    (format!("{mangled_name}__push"), vec![self_ptr, elem_type], UNIT_TYPE),
                    (format!("{mangled_name}__pop"), vec![self_ptr], elem_type),
                    (format!("{mangled_name}__get"), vec![self_ptr, I64_TYPE], elem_type),
                    (format!("{mangled_name}__set"), vec![self_ptr, I64_TYPE, elem_type], UNIT_TYPE),
                    (format!("{mangled_name}__len"), vec![self_ptr], I64_TYPE),
                    (format!("{mangled_name}__contains"), vec![self_ptr, elem_type], BOOL_TYPE),
                    (format!("{mangled_name}__remove"), vec![self_ptr, I64_TYPE], elem_type),
                    (format!("{mangled_name}__insert"), vec![self_ptr, I64_TYPE, elem_type], UNIT_TYPE),
                    (format!("{mangled_name}__clear"), vec![self_ptr], UNIT_TYPE),
                    (format!("{mangled_name}__clone"), vec![self_ptr], array_type),
                    (format!("{mangled_name}__iter"), vec![self_ptr], array_type),
                ];
                for (name, params, ret) in sigs {
                    ctx.fn_sigs.insert(name, (params, ret));
                }
            }
            "Dict" | "HashMap" => {
                // Dict/HashMap have key and value types
                // (GorgetDict/GorgetMap are opaque C types, not struct-based in GIR)
                let sigs = vec![
                    (format!("{mangled_name}__len"), vec![self_ptr], I64_TYPE),
                    (format!("{mangled_name}__contains"), vec![self_ptr, I64_TYPE], BOOL_TYPE),
                    (format!("{mangled_name}__clear"), vec![self_ptr], UNIT_TYPE),
                ];
                for (name, params, ret) in sigs {
                    ctx.fn_sigs.insert(name, (params, ret));
                }
            }
            "Set" | "HashSet" => {
                let sigs = vec![
                    (format!("{mangled_name}__add"), vec![self_ptr, elem_type], UNIT_TYPE),
                    (format!("{mangled_name}__contains"), vec![self_ptr, elem_type], BOOL_TYPE),
                    (format!("{mangled_name}__remove"), vec![self_ptr, elem_type], BOOL_TYPE),
                    (format!("{mangled_name}__len"), vec![self_ptr], I64_TYPE),
                    (format!("{mangled_name}__clear"), vec![self_ptr], UNIT_TYPE),
                ];
                for (name, params, ret) in sigs {
                    ctx.fn_sigs.insert(name, (params, ret));
                }
            }
            _ => {}
        }
    }
}

/// Evaluate a compile-time constant expression (for `const` and `meta` declarations).
fn eval_const_expr(
    expr: &ast::Expr,
    known: &rustc_hash::FxHashMap<String, crate::ir::instructions::Constant>,
) -> Option<crate::ir::instructions::Constant> {
    use crate::ir::instructions::Constant;
    use crate::parser::ast::{BinaryOp, Expr};

    match expr {
        Expr::IntLiteral(v) => Some(Constant::I64(*v)),
        Expr::FloatLiteral(v) => Some(Constant::F64(*v)),
        Expr::BoolLiteral(v) => Some(Constant::Bool(*v)),
        Expr::StringLiteral(lit) => {
            // Simple non-interpolated string
            use crate::lexer::token::StringSegment;
            if lit.segments.len() == 1 {
                if let StringSegment::Literal(s) = &lit.segments[0] {
                    return Some(Constant::Str(s.clone()));
                }
            }
            None
        }
        Expr::Identifier(name) => known.get(name).cloned(),
        Expr::BinaryOp { left, op, right } => {
            let l = eval_const_expr(&left.node, known)?;
            let r = eval_const_expr(&right.node, known)?;
            match (l, r) {
                (Constant::I64(a), Constant::I64(b)) => match op {
                    BinaryOp::Add => Some(Constant::I64(a.wrapping_add(b))),
                    BinaryOp::Sub => Some(Constant::I64(a.wrapping_sub(b))),
                    BinaryOp::Mul => Some(Constant::I64(a.wrapping_mul(b))),
                    BinaryOp::Div if b != 0 => Some(Constant::I64(a / b)),
                    BinaryOp::Mod if b != 0 => Some(Constant::I64(a % b)),
                    BinaryOp::Gt => Some(Constant::Bool(a > b)),
                    BinaryOp::Lt => Some(Constant::Bool(a < b)),
                    BinaryOp::GtEq => Some(Constant::Bool(a >= b)),
                    BinaryOp::LtEq => Some(Constant::Bool(a <= b)),
                    BinaryOp::Eq => Some(Constant::Bool(a == b)),
                    BinaryOp::Neq => Some(Constant::Bool(a != b)),
                    _ => None,
                },
                (Constant::F64(a), Constant::F64(b)) => match op {
                    BinaryOp::Add => Some(Constant::F64(a + b)),
                    BinaryOp::Sub => Some(Constant::F64(a - b)),
                    BinaryOp::Mul => Some(Constant::F64(a * b)),
                    BinaryOp::Div if b != 0.0 => Some(Constant::F64(a / b)),
                    _ => None,
                },
                _ => None,
            }
        }
        Expr::UnaryOp { op, operand } => {
            let val = eval_const_expr(&operand.node, known)?;
            match (op, val) {
                (ast::UnaryOp::Neg, Constant::I64(v)) => Some(Constant::I64(-v)),
                (ast::UnaryOp::Neg, Constant::F64(v)) => Some(Constant::F64(-v)),
                (ast::UnaryOp::Not, Constant::Bool(v)) => Some(Constant::Bool(!v)),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Create an empty AnalysisResult for testing by parsing an empty module.
#[cfg(test)]
pub fn empty_analysis_for_test() -> AnalysisResult {
    let mut parser = crate::parser::Parser::new("void main():\n    pass\n");
    let mut module = parser.parse_module();
    crate::semantic::analyze(&mut module, &[])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn parse_and_analyze(source: &str) -> (ast::Module, AnalysisResult) {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(parser.errors.is_empty(), "Parse errors: {:?}", parser.errors);
        let result = crate::semantic::analyze(&mut module, &[]);
        assert!(result.errors.is_empty(), "Semantic errors: {:?}", result.errors);
        (module, result)
    }

    #[test]
    fn lower_hello_world() {
        let source = r#"void main():
    print("Hello, World!")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        assert_eq!(gir.functions.len(), 1);
        assert_eq!(gir.functions[0].name, "main");
        assert_eq!(gir.functions[0].return_type, I32_TYPE);

        // Should have at least one block with a printf call
        let main = &gir.functions[0];
        let has_printf = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::CallExtern { func, .. } if func == "printf")
            })
        });
        assert!(has_printf, "main should contain a printf call");
    }

    #[test]
    fn lower_function_with_params() {
        let source = r#"int add(int a, int b):
    return a + b

void main():
    int r = add(1, 2)
    print("{r}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        assert_eq!(gir.functions.len(), 2);

        let add_fn = gir.find_function("add").unwrap();
        assert_eq!(add_fn.params.len(), 2);
        // _0 = return, _1 = a, _2 = b
        assert_eq!(add_fn.locals[1].name_hint.as_deref(), Some("a"));
        assert_eq!(add_fn.locals[2].name_hint.as_deref(), Some("b"));
    }

    #[test]
    fn lower_multiple_functions() {
        let source = r#"int double(int x) = x * 2

int triple(int x):
    return x * 3

void main():
    int a = double(5)
    int b = triple(5)
    print("{a}")
    print("{b}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        assert_eq!(gir.functions.len(), 3);
        assert!(gir.find_function("double").is_some());
        assert!(gir.find_function("triple").is_some());
        assert!(gir.find_function("main").is_some());
    }

    #[test]
    fn lower_struct_type() {
        let source = r#"struct Point:
    int x
    int y

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify the struct type was registered
        let point_def = gir.type_registry.get_type_def("Point").unwrap();
        assert_eq!(point_def.name, "Point");
        if let TypeDefKind::Struct(ref s) = point_def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "x");
            assert_eq!(s.fields[0].type_id, I64_TYPE);
            assert_eq!(s.fields[1].name, "y");
            assert_eq!(s.fields[1].type_id, I64_TYPE);
        } else {
            panic!("Expected Struct TypeDef for Point");
        }
    }

    #[test]
    fn lower_enum_type() {
        let source = r#"enum Color:
    Red()
    Green()
    Blue()

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let color_def = gir.type_registry.get_type_def("Color").unwrap();
        assert_eq!(color_def.name, "Color");
        if let TypeDefKind::Enum(ref e) = color_def.kind {
            assert_eq!(e.variants.len(), 3);
            assert_eq!(e.variants[0].name, "Red");
            assert_eq!(e.variants[1].name, "Green");
            assert_eq!(e.variants[2].name, "Blue");
        } else {
            panic!("Expected Enum TypeDef for Color");
        }
    }

    #[test]
    fn lower_struct_construction_and_field_access() {
        let source = r#"struct Point:
    int x
    int y

void main():
    Point p = Point(10, 20)
    int px = p.x
    print("{px}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let main = gir.find_function("main").unwrap();
        // Should have StructInit and FieldLoad instructions
        let has_struct_init = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::StructInit { type_name, .. } if type_name == "Point")
            })
        });
        assert!(has_struct_init, "main should have a StructInit for Point");

        let has_field_load = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::FieldLoad { field: 0, .. })
            })
        });
        assert!(has_field_load, "main should have a FieldLoad for field 0 (x)");
    }

    #[test]
    fn lower_enum_construction() {
        let source = r#"enum Color:
    Red()
    Green()
    Blue()

void main():
    Color c = Red()
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let main = gir.find_function("main").unwrap();
        let has_enum_init = main.blocks.iter().any(|bb| {
            bb.instructions.iter().any(|inst| {
                matches!(inst, crate::ir::instructions::Instruction::EnumInit { type_name, variant, .. }
                    if type_name == "Color" && variant == "Red")
            })
        });
        assert!(has_enum_init, "main should have an EnumInit for Color::Red");
    }

    #[test]
    fn lower_struct_c_output() {
        let source = r#"struct Point:
    int x
    int y

void main():
    Point p = Point(10, 20)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);
        let c_code = crate::backend::c::generate_c(&gir);

        assert!(c_code.contains("typedef struct Point Point;"), "Should forward-declare Point");
        assert!(c_code.contains("struct Point {"), "Should define Point struct");
        assert!(c_code.contains("int64_t x;"), "Should have field x");
        assert!(c_code.contains("int64_t y;"), "Should have field y");
        assert!(c_code.contains("(Point){"), "Should construct Point");
    }

    #[test]
    fn lower_equip_method() {
        let source = r#"struct Point:
    int x
    int y

equip Point:
    int sum(self):
        return self.x + self.y

void main():
    Point p = Point(3, 4)
    int s = p.sum()
    print("{s}")
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have a mangled method function
        let sum_fn = gir.find_function("Point__sum");
        assert!(sum_fn.is_some(), "Should have Point__sum function");
        let sum_fn = sum_fn.unwrap();
        // First param is self (pointer to Point)
        assert!(sum_fn.params.len() >= 1, "Point__sum should have at least self param");
    }

    // ── P2.3: Generic Monomorphization Tests ──

    #[test]
    fn lower_generic_struct_type() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](10, 3.14)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have a monomorphized TypeDef for Pair__int64_t__double
        let pair_def = gir.type_registry.get_type_def("Pair__int64_t__double");
        assert!(pair_def.is_some(), "Should have monomorphized Pair__int64_t__double TypeDef");
        let pair_def = pair_def.unwrap();
        if let TypeDefKind::Struct(ref s) = pair_def.kind {
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "first");
            assert_eq!(s.fields[0].type_id, I64_TYPE);
            assert_eq!(s.fields[1].name, "second");
            assert_eq!(s.fields[1].type_id, F64_TYPE);
        } else {
            panic!("Expected Struct TypeDef for monomorphized Pair");
        }
    }

    #[test]
    fn lower_generic_struct_c_output() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](10, 3.14)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);
        let c_code = crate::backend::c::generate_c(&gir);

        assert!(c_code.contains("Pair__int64_t__double"),
            "C output should contain monomorphized Pair type name. C code:\n{c_code}");
    }

    #[test]
    fn lower_generic_function_monomorphized() {
        let source = r#"T identity[T](T x) = x

void main():
    int a = identity[int](42)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have a monomorphized function identity__int64_t
        let fn_name = gir.functions.iter()
            .find(|f| f.name.contains("identity") && f.name.contains("int64_t"));
        assert!(fn_name.is_some(),
            "Should have monomorphized identity__int64_t function. Functions: {:?}",
            gir.functions.iter().map(|f| &f.name).collect::<Vec<_>>());

        let identity_fn = fn_name.unwrap();
        assert_eq!(identity_fn.params.len(), 1, "identity should have 1 param");
        assert_eq!(identity_fn.return_type, I64_TYPE, "identity[int] should return int64");
    }

    #[test]
    fn lower_generic_enum_type() {
        let source = r#"enum Maybe[T]:
    Just(T)
    Nothing()

void main():
    Maybe[int] m = Just(42)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        let maybe_def = gir.type_registry.get_type_def("Maybe__int64_t");
        assert!(maybe_def.is_some(), "Should have monomorphized Maybe__int64_t TypeDef");
        let maybe_def = maybe_def.unwrap();
        if let TypeDefKind::Enum(ref e) = maybe_def.kind {
            assert_eq!(e.variants.len(), 2);
            assert_eq!(e.variants[0].name, "Just");
            assert_eq!(e.variants[0].fields.len(), 1);
            assert_eq!(e.variants[0].fields[0].type_id, I64_TYPE);
            assert_eq!(e.variants[1].name, "Nothing");
            assert_eq!(e.variants[1].fields.len(), 0);
        } else {
            panic!("Expected Enum TypeDef for monomorphized Maybe");
        }
    }

    #[test]
    fn lower_multiple_generic_instantiations() {
        let source = r#"struct Wrapper[T]:
    T value

void main():
    Wrapper[int] a = Wrapper[int](42)
    Wrapper[float] b = Wrapper[float](3.14)
    Wrapper[bool] c = Wrapper[bool](true)
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Should have three distinct monomorphized types
        assert!(gir.type_registry.get_type_def("Wrapper__int64_t").is_some(),
            "Should have Wrapper__int64_t");
        assert!(gir.type_registry.get_type_def("Wrapper__double").is_some(),
            "Should have Wrapper__double");
        assert!(gir.type_registry.get_type_def("Wrapper__bool").is_some(),
            "Should have Wrapper__bool");
    }

    // ── P2.5: Trait & Vtable Lowering Tests ──

    #[test]
    fn lower_trait_vtable_types() {
        let source = r#"trait Shape:
    float area(self)
    void draw(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14
    void draw(self):
        pass

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify VTable and TraitObj types were created
        assert!(gir.type_registry.has_type_def("Shape_VTable"),
            "Should have Shape_VTable TypeDef");
        assert!(gir.type_registry.has_type_def("Shape_TraitObj"),
            "Should have Shape_TraitObj TypeDef");

        // Verify VTable struct fields
        let vtable_def = gir.type_registry.get_type_def("Shape_VTable").unwrap();
        if let TypeDefKind::Struct(ref s) = vtable_def.kind {
            assert_eq!(s.fields.len(), 2, "VTable should have 2 method slots");
            assert_eq!(s.fields[0].name, "area");
            assert_eq!(s.fields[1].name, "draw");
        } else {
            panic!("Expected Struct TypeDef for Shape_VTable");
        }

        // Verify TraitObj struct fields
        let obj_def = gir.type_registry.get_type_def("Shape_TraitObj").unwrap();
        if let TypeDefKind::Struct(ref s) = obj_def.kind {
            assert_eq!(s.fields.len(), 2, "TraitObj should have data + vtable");
            assert_eq!(s.fields[0].name, "data");
            assert_eq!(s.fields[1].name, "vtable");
        } else {
            panic!("Expected Struct TypeDef for Shape_TraitObj");
        }
    }

    #[test]
    fn lower_trait_equip_methods() {
        let source = r#"trait Shape:
    float area(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify trait impl function exists
        let area_fn = gir.find_function("Shape_for_Circle__area");
        assert!(area_fn.is_some(),
            "Should have Shape_for_Circle__area function. Functions: {:?}",
            gir.functions.iter().map(|f| &f.name).collect::<Vec<_>>());
        let area_fn = area_fn.unwrap();
        // First param is void* self
        assert!(area_fn.params.len() >= 1,
            "Trait impl should have at least self_void param");
    }

    #[test]
    fn lower_trait_vtable_globals() {
        let source = r#"trait Shape:
    float area(self)
    void draw(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14
    void draw(self):
        pass

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);

        // Verify vtable global constant exists
        let vtable_global = gir.globals.iter()
            .find(|g| g.name == "Shape_for_Circle_vtable");
        assert!(vtable_global.is_some(),
            "Should have Shape_for_Circle_vtable global. Globals: {:?}",
            gir.globals.iter().map(|g| &g.name).collect::<Vec<_>>());

        let vg = vtable_global.unwrap();
        if let crate::ir::GlobalInit::Struct { type_name, fields } = &vg.init {
            assert_eq!(type_name, "Shape_VTable");
            assert_eq!(fields.len(), 2, "VTable should have 2 method entries");
            assert_eq!(fields[0].0, "area");
            assert_eq!(fields[1].0, "draw");
            // Verify FnRef entries point to correct functions
            if let crate::ir::GlobalInit::FnRef(fn_name) = &fields[0].1 {
                assert_eq!(fn_name, "Shape_for_Circle__area");
            } else {
                panic!("Expected FnRef for area slot");
            }
        } else {
            panic!("Expected Struct initializer for vtable global");
        }
    }

    #[test]
    fn lower_trait_c_output() {
        let source = r#"trait Shape:
    float area(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return self.radius * self.radius * 3.14

void main():
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result);
        let c_code = crate::backend::c::generate_c(&gir);

        // VTable type
        assert!(c_code.contains("typedef struct Shape_VTable Shape_VTable;"),
            "Should forward-declare Shape_VTable. C code:\n{c_code}");
        assert!(c_code.contains("struct Shape_VTable {"),
            "Should define Shape_VTable struct. C code:\n{c_code}");
        // Function pointer field: `double (*area)(const void*)`
        assert!(c_code.contains("(*area)"),
            "Should have function pointer field 'area'. C code:\n{c_code}");

        // TraitObj type
        assert!(c_code.contains("typedef struct Shape_TraitObj Shape_TraitObj;"),
            "Should forward-declare Shape_TraitObj. C code:\n{c_code}");

        // VTable global constant
        assert!(c_code.contains("Shape_for_Circle_vtable"),
            "Should emit vtable global constant. C code:\n{c_code}");

        // Trait impl function
        assert!(c_code.contains("Shape_for_Circle__area"),
            "Should emit trait impl function. C code:\n{c_code}");
    }

    // ── P2.4: Closure Tests ──

    #[test]
    fn lower_closure_unit_test() {
        // Unit test: manually verify closure lowering creates struct and call fn
        use crate::ir::instructions::Operand;
        use crate::ir::lowering::closures::ClosureLowering;
        use crate::ir::builder::FunctionBuilder;
        use crate::ir::lowering::types::TypeMapper;
        use crate::parser::ast::{ClosureParam, Ownership, PrimitiveType};
        use crate::span::Spanned;

        let analysis = Box::leak(Box::new(empty_analysis_for_test()));
        let mut reg = TypeRegistry::new();
        let mapper = TypeMapper::new(&mut reg);
        let mut ctx = crate::ir::lowering::context::LoweringContext::new(analysis, mapper, reg);
        ctx.register_local("x", LocalId(1), I64_TYPE);

        let mut closures = ClosureLowering::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[(I64_TYPE, Some("x"))]);

        let params = vec![Spanned::dummy(ClosureParam {
            type_: Some(Spanned::dummy(ast::Type::Primitive(PrimitiveType::Int))),
            ownership: Ownership::Borrow,
            name: Spanned::dummy("y".to_string()),
        })];

        let body = Spanned::dummy(ast::Expr::BinaryOp {
            left: Box::new(Spanned::dummy(ast::Expr::Identifier("x".to_string()))),
            op: ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(ast::Expr::Identifier("y".to_string()))),
        });

        let operand = closures.lower_closure(&mut ctx, &mut builder, &params, &body, false);
        assert!(matches!(operand, Operand::Copy(_)), "Should return a Copy operand");

        // Verify struct type was created
        let type_def = ctx.type_registry.get_type_def("__Closure_0");
        assert!(type_def.is_some(), "Should have __Closure_0 TypeDef");
        let td = type_def.unwrap();
        if let TypeDefKind::Struct(ref s) = td.kind {
            assert_eq!(s.fields.len(), 1, "Should capture 'x'");
            assert_eq!(s.fields[0].name, "x");
        } else {
            panic!("Expected struct TypeDef");
        }

        // Verify call function was registered
        assert!(ctx.fn_sigs.contains_key("__Closure_0__call"),
            "Should have registered __Closure_0__call signature");

        // Verify lifted closure is stored
        assert_eq!(closures.lifted.len(), 1);
        assert_eq!(closures.lifted[0].struct_type_name, "__Closure_0");
        assert_eq!(closures.lifted[0].captures.len(), 1);
        assert_eq!(closures.lifted[0].captures[0].name, "x");

        // Emit the call function
        let func = super::closures::emit_closure_call_function(&mut ctx, &closures.lifted[0]);
        assert_eq!(func.name, "__Closure_0__call");
        assert!(func.params.len() >= 2, "Should have env + y params");
    }
}
