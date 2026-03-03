pub mod closures;
pub mod context;
pub mod drops;
pub mod exprs;
pub mod functions;
pub mod generics;
pub mod stmts;
pub mod traits;
pub mod types;

use crate::ir::instructions::Operand;
use crate::ir::types::*;
use crate::ir::{ExternDecl, Module};
use crate::parser::ast::{self, FunctionBody, Item};
use crate::semantic::AnalysisResult;

use context::LoweringContext;
use functions::lower_function;
use generics::GenericCollector;
use types::TypeMapper;

/// Options controlling the GIR lowering pass, typically sourced from CLI flags.
#[derive(Debug, Default, Clone)]
pub struct LoweringOptions {
    /// Override `directive strip-asserts`: asserts become no-ops.
    pub strip_asserts: bool,
    /// Override `directive strip-asserts` off: force-keep asserts.
    pub no_strip_asserts: bool,
    /// Override `directive overflow wrap`: integer overflow wraps silently.
    pub overflow_wrap: bool,
    /// Override overflow to checked mode (abort on overflow).
    pub overflow_checked: bool,
    /// When true, lower test items even when a `main()` exists (for `gg test`).
    pub test_mode: bool,
    /// Only run tests whose tags include one of these (empty = run all).
    pub test_tags: Vec<String>,
    /// Skip tests whose tags include any of these.
    pub test_exclude_tags: Vec<String>,
    /// Only run tests whose display name contains this substring.
    pub test_name_filter: Option<String>,
    /// Enable trace instrumentation and write events to this file path.
    pub trace_filename: Option<String>,
    /// Enable hot-reload mode (directive hot-reload or --hot-reload flag).
    pub hot_reload: bool,
    /// Compile with AddressSanitizer + UBSan (`-fsanitize=address,undefined`).
    pub sanitize: bool,
}

/// Lower an AST module + analysis result into a GIR module.
pub fn lower_module(
    ast_module: &ast::Module,
    analysis: &AnalysisResult,
    options: &LoweringOptions,
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
            ("TlsfAllocator", "GorgetTlsfAllocator"),
            ("FixedBufferAllocator", "GorgetFixedBufferAllocator"),
            ("FallbackAllocator", "GorgetFallbackAllocator"),
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
            "Vector" | "Dict" | "HashMap" | "Set" | "HashSet");
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
    // CLI flags override directives
    if options.strip_asserts { ctx.strip_asserts = true; }
    if options.no_strip_asserts { ctx.strip_asserts = false; }
    if options.overflow_wrap { ctx.overflow_wrap = true; }
    if options.overflow_checked { ctx.overflow_wrap = false; }

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
        &mut ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs, Some(ast_module));

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

    // Pre-register module-level static variables so functions can reference them.
    // Skip stdlib StaticDecl items (identified by dummy spans — start == end == 0);
    // those are handled by the C backend as well-known names (stderr, stdout, etc.).
    for item in &ast_module.items {
        if let Item::StaticDecl(decl) = &item.node {
            // Stdlib items use Span::dummy() { start: 0, end: 0 } — skip them.
            if decl.span.start == decl.span.end {
                continue;
            }
            ctx.global_names.insert(decl.name.node.clone());
            // Store the mangled type name for method dispatch inference.
            // For generic types like Mutex[int] store "Mutex__int64_t" (not just "Mutex")
            // so that infer_type_name_from_operand_full can dispatch correctly.
            if let ast::Type::Named { name: type_name, generic_args } = &decl.type_.node {
                let mangled = if generic_args.is_empty() {
                    type_name.node.clone()
                } else {
                    crate::ir::lowering::types::mangle_generic_name(&type_name.node, generic_args)
                };
                ctx.global_type_names.insert(decl.name.node.clone(), mangled);
            }
        }
    }

    // Lower module-level static declarations → Globals.
    // Skip stdlib StaticDecl items (dummy spans) — handled by C backend as well-known names.
    for item in &ast_module.items {
        if let Item::StaticDecl(decl) = &item.node {
            if decl.span.start == decl.span.end { continue; }
            lower_static_decl(&mut ctx, &mut module, decl);
        }
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

    // Lower test items: each test becomes a void function, then generate test runner main.
    // In test_mode (gg test), run even when a main() exists — the C backend will skip it.
    let has_tests = ast_module.items.iter().any(|item| matches!(&item.node, Item::Test(_)));
    let has_main = ast_module.items.iter().any(|item| matches!(&item.node, Item::Function(f) if f.name.node == "main"));
    if has_tests && (options.test_mode || !has_main) {
        lower_test_items(&mut ctx, &mut module, ast_module, options);
        // Mark module as a test module so the C backend always emits a test runner,
        // even when all tests were filtered out (e.g., --tag X --exclude-tag X → 0 tests).
        module.is_test_module = true;
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

    // Trace: filename provided by options (derived from source path in main.rs)
    module.trace_filename = options.trace_filename.clone();

    // Hot-reload: scan for directive + find state type from init() + compute state hash
    let mut has_hot_reload_directive = false;
    for item in &ast_module.items {
        if let Item::Directive(d) = &item.node {
            if d.name == "hot-reload" { has_hot_reload_directive = true; }
        }
    }
    module.hot_reload = has_hot_reload_directive || options.hot_reload;
    if module.hot_reload {
        // Find state type from init() return type
        for item in &ast_module.items {
            if let Item::Function(f) = &item.node {
                if f.name.node == "init" {
                    if let crate::parser::ast::Type::Named { name, .. } = &f.return_type.node {
                        module.hot_reload_state_type = Some(name.node.clone());
                    }
                    break;
                }
            }
        }
        // Compute state hash from the State struct field layout
        if let Some(ref state_type) = module.hot_reload_state_type.clone() {
            for item in &ast_module.items {
                if let Item::Struct(s) = &item.node {
                    if &s.name.node == state_type {
                        let mut layout = String::new();
                        for field in &s.fields {
                            let field_type = format!("{:?}", field.node.type_.node);
                            let field_name = &field.node.name.node;
                            layout.push_str(&format!("{field_type} {field_name};"));
                        }
                        module.hot_reload_state_hash = fnv1a_hash(&layout);
                        break;
                    }
                }
            }
        }
        // Check if reload() function exists
        module.hot_reload_has_reload_fn = ast_module.items.iter().any(|item| {
            if let Item::Function(f) = &item.node {
                f.name.node == "reload"
            } else { false }
        });
    }

    // Collect channel/shared/mutex element types for C backend wrapper emission
    for name in module.type_registry.all_type_def_names() {
        // Skip template placeholders. A type is considered abstract/generic if any
        // `__`-separated component is entirely uppercase letters (e.g. "T", "K").
        // This catches "T" directly as well as nested cases like "Vector__T" where
        // the inner element `T` has not been monomorphized.
        let is_template = |elem: &str| {
            elem.split("__").any(|part| !part.is_empty() && part.chars().all(|c| c.is_uppercase()))
        };
        if let Some(elem) = name.strip_prefix("Channel__") {
            if !is_template(elem) && !module.channel_types.contains(&elem.to_string()) {
                module.channel_types.push(elem.to_string());
            }
        }
        if let Some(elem) = name.strip_prefix("Shared__") {
            if !is_template(elem) && !module.shared_types.contains(&elem.to_string()) {
                module.shared_types.push(elem.to_string());
            }
        }
        if let Some(elem) = name.strip_prefix("Weak__") {
            if !is_template(elem) && !module.weak_types.contains(&elem.to_string()) {
                module.weak_types.push(elem.to_string());
            }
        }
        if let Some(elem) = name.strip_prefix("Mutex__") {
            if !is_template(elem) && !module.mutex_types.contains(&elem.to_string()) {
                module.mutex_types.push(elem.to_string());
            }
        }
        if name == "TaskGroup" {
            module.has_task_group = true;
        }
        // std.sync: collect RWLock element types for wrapper emission
        if let Some(elem) = name.strip_prefix("RWLock__") {
            // Skip template placeholders (single uppercase letter = generic param e.g. "T")
            if !elem.chars().all(|c| c.is_uppercase()) && !module.rwlock_types.contains(&elem.to_string()) {
                module.rwlock_types.push(elem.to_string());
            }
        }
        // Any sync type signals has_sync
        if matches!(name.as_str(), "AtomicInt" | "AtomicBool" | "Barrier" | "CondVar")
            || name.starts_with("RWLock__")
            || name.starts_with("ReadGuard__")
            || name.starts_with("WriteGuard__")
        {
            module.has_sync = true;
        }
        // std.thread: collect Thread types (skip template placeholders like "T")
        if let Some(elem) = name.strip_prefix("Thread__") {
            if !elem.chars().all(|c| c.is_uppercase()) {
                if !module.thread_types.contains(&elem.to_string()) {
                    module.thread_types.push(elem.to_string());
                }
                module.has_thread = true;
            }
        }
        // std.process: Process type
        if name == "Process" {
            module.has_process = true;
        }
    }

    // Collect thread-spawned function metadata for C backend helper emission
    for (fn_name, ret_type) in &ctx.thread_spawned_fns {
        if !module.thread_spawned_fns.iter().any(|(n, _)| n == fn_name) {
            module.thread_spawned_fns.push((fn_name.clone(), *ret_type));
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

/// Lower a module-level static declaration into a Global IR node.
///
/// The initializer is evaluated as a C expression string for types that require
/// runtime initialization (heap allocation): AtomicInt, AtomicBool, Barrier,
/// CondVar, Mutex[T], RWLock[T].  All others use `Zeroed`.
fn lower_static_decl(
    ctx: &mut LoweringContext,
    module: &mut Module,
    decl: &crate::parser::ast::StaticDecl,
) {
    use crate::ir::Global;

    // Use map_ast_type_mut so that generic types like Mutex[int] get registered
    // (map_ast_type returns UNIT_TYPE for unregistered generic types).
    let type_id = ctx.type_mapper.map_ast_type_mut(&decl.type_.node, &mut ctx.type_registry);
    let name = decl.name.node.clone();

    let init = eval_static_init(&decl.type_.node, &decl.value.node);
    module.globals.push(Global { name, type_id, init });
}

/// Evaluate a static initializer expression into a GlobalInit value.
/// Supports constructor calls for sync primitives that require heap allocation.
fn eval_static_init(ty: &crate::parser::ast::Type, expr: &crate::parser::ast::Expr) -> crate::ir::GlobalInit {
    use crate::ir::GlobalInit;
    use crate::parser::ast::Expr;

    // Extract the type name (ignoring generic args) for dispatch
    let type_name = match ty {
        crate::parser::ast::Type::Named { name, .. } => name.node.as_str(),
        _ => return GlobalInit::Zeroed,
    };

    // Constructor syntax: TypeName(args...) is parsed as StructLiteral.
    // Fallback: plain function Call (e.g. from explicit call-style expressions).
    let (callee_name, literal_args): (&str, Vec<String>) = match expr {
        Expr::StructLiteral { name, args, .. } => {
            let largs = args.iter().map(|a| eval_literal_arg(&a.node)).collect();
            (name.node.as_str(), largs)
        }
        Expr::Call { callee, args, .. } => {
            let cname = match &callee.node {
                Expr::Identifier(n) => n.as_str(),
                _ => return GlobalInit::Zeroed,
            };
            let largs = args.iter().map(|a| eval_literal_arg(&a.node.value.node)).collect();
            (cname, largs)
        }
        _ => return GlobalInit::Zeroed,
    };

    // Dispatch by type/callee name
    let c_call = match type_name {
        "AtomicInt" if callee_name == "AtomicInt" => {
            let n = literal_args.first().cloned().unwrap_or_else(|| "0".to_string());
            format!("gorget_atomic_int_new({n})")
        }
        "AtomicBool" if callee_name == "AtomicBool" => {
            let b = literal_args.first().map(|s| if s == "true" { "1" } else { "0" }.to_string())
                          .unwrap_or_else(|| "0".to_string());
            format!("gorget_atomic_bool_new({b})")
        }
        "Barrier" if callee_name == "Barrier" => {
            let n = literal_args.first().cloned().unwrap_or_else(|| "1".to_string());
            format!("gorget_barrier_new({n})")
        }
        "CondVar" if callee_name == "CondVar" => {
            "gorget_condvar_new()".to_string()
        }
        "Mutex" if callee_name == "Mutex" => {
            // Determine element C type from the generic arg
            let elem_c = generic_elem_c_type(ty);
            let v = literal_args.first().cloned().unwrap_or_else(|| "0".to_string());
            format!("Mutex__{elem_c}__new({v})")
        }
        "RWLock" if callee_name == "RWLock" => {
            let elem_c = generic_elem_c_type(ty);
            let v = literal_args.first().cloned().unwrap_or_else(|| "0".to_string());
            format!("RWLock__{elem_c}__new({v})")
        }
        _ => return GlobalInit::Zeroed,
    };

    GlobalInit::RuntimeCall(c_call)
}

/// Evaluate a simple literal expression to a C string for use in static initializers.
fn eval_literal_arg(expr: &crate::parser::ast::Expr) -> String {
    use crate::parser::ast::Expr;
    match expr {
        Expr::IntLiteral(n) => n.to_string(),
        Expr::FloatLiteral(f) => f.to_string(),
        Expr::BoolLiteral(b) => if *b { "1".to_string() } else { "0".to_string() },
        Expr::StringLiteral(s) => format!("\"{}\"", s.as_plain_text()),
        _ => "0".to_string(),
    }
}

/// Extract the C element type name from a generic type like `Mutex[int]`.
fn generic_elem_c_type(ty: &crate::parser::ast::Type) -> String {
    use crate::parser::ast::{PrimitiveType, Type};
    if let Type::Named { generic_args, .. } = ty {
        if let Some(arg) = generic_args.first() {
            return match &arg.node {
                Type::Primitive(p) => match p {
                    PrimitiveType::Int | PrimitiveType::Int64 => "int64_t".to_string(),
                    PrimitiveType::Float | PrimitiveType::Float64 => "double".to_string(),
                    PrimitiveType::Bool  => "bool".to_string(),
                    PrimitiveType::Str   => "Str".to_string(),
                    PrimitiveType::Char  => "int32_t".to_string(),
                    PrimitiveType::Uint8 => "uint8_t".to_string(),
                    _ => "int64_t".to_string(),
                },
                Type::Named { name, .. } => name.node.clone(),
                _ => "int64_t".to_string(),
            };
        }
    }
    "int64_t".to_string()
}

/// Lower test items into test functions and generate a test runner main().
fn lower_test_items(
    ctx: &mut LoweringContext,
    module: &mut Module,
    ast_module: &ast::Module,
    options: &LoweringOptions,
) {
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::TestFnInfo;
    use crate::parser::ast::AttributeArg;

    /// Check whether a test should run given the filtering options.
    fn should_run_test(test_def: &ast::TestDef, options: &LoweringOptions) -> bool {
        // Name filter: skip if test name doesn't contain the substring.
        if let Some(ref filter) = options.test_name_filter {
            if !test_def.name.node.contains(filter.as_str()) {
                return false;
            }
        }
        // Exclusion wins: if any of the test's tags is in exclude_tags, skip.
        if !options.test_exclude_tags.is_empty() {
            for attr in &test_def.attributes {
                if attr.node.name.node == "tag" {
                    for arg in &attr.node.args {
                        if let AttributeArg::StringLiteral(s) = arg {
                            if options.test_exclude_tags.contains(s) {
                                return false;
                            }
                        }
                    }
                }
            }
        }
        // Inclusion filter: if --tag was given, only run tests with a matching tag.
        if !options.test_tags.is_empty() {
            for attr in &test_def.attributes {
                if attr.node.name.node == "tag" {
                    for arg in &attr.node.args {
                        if let AttributeArg::StringLiteral(s) = arg {
                            if options.test_tags.contains(s) {
                                return true;
                            }
                        }
                    }
                }
            }
            return false;
        }
        true
    }

    // Lower suite setup as __suite_setup() void function.
    for item in &ast_module.items {
        if let Item::SuiteSetup(setup) = &item.node {
            let mut builder = FunctionBuilder::new("__suite_setup", UNIT_TYPE, &[]);
            ctx.clear_locals();
            ctx.drops.push_scope(drops::DropScopeKind::Function);
            stmts::lower_block(ctx, &mut builder, &setup.body);
            let last = builder.current_block.0 as usize;
            if builder.blocks[last].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }
            module.functions.push(builder.build());
            module.has_suite_setup = true;
        }
    }

    // Lower suite teardown as __suite_teardown() void function.
    for item in &ast_module.items {
        if let Item::SuiteTeardown(teardown) = &item.node {
            let mut builder = FunctionBuilder::new("__suite_teardown", UNIT_TYPE, &[]);
            ctx.clear_locals();
            ctx.drops.push_scope(drops::DropScopeKind::Function);
            stmts::lower_block(ctx, &mut builder, &teardown.body);
            let last = builder.current_block.0 as usize;
            if builder.blocks[last].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }
            module.functions.push(builder.build());
            module.has_suite_teardown = true;
        }
    }

    // Lower each matching test body as a standalone void function.
    for (idx, item) in ast_module.items.iter().enumerate() {
        if let Item::Test(test_def) = &item.node {
            if !should_run_test(test_def, options) {
                continue;
            }

            let fn_name = format!("__test_{idx}");
            let test_name = test_def.name.node.clone();

            // Extract @should_panic metadata
            let should_panic = test_def.attributes.iter()
                .any(|a| a.node.name.node == "should_panic");
            let expected_panic_msg: Option<String> = test_def.attributes.iter()
                .find(|a| a.node.name.node == "should_panic")
                .and_then(|a| a.node.args.first())
                .and_then(|arg| {
                    if let AttributeArg::StringLiteral(s) = arg { Some(s.clone()) } else { None }
                });

            // Lower with-bindings as separate init functions + pointer parameters.
            // This ensures that with-binding storage lives in the TEST RUNNER's stack frame
            // (not the test function's frame), so drops work correctly even when
            // gorget_panic() calls longjmp() back to the test runner's setjmp.
            let mut with_binding_infos: Vec<crate::ir::TestWithBinding> = Vec::new();
            let mut wb_param_types: Vec<TypeId> = Vec::new();
            let mut wb_ptr_types: Vec<TypeId> = Vec::new();

            for (wb_idx, binding) in test_def.with_bindings.iter().enumerate() {
                let init_fn_name = format!("__test_{idx}_wb_{wb_idx}_init");

                // Create init function: lowers the initializer and returns the value.
                // The value is MOVED out (not dropped by this function).
                {
                    // We don't know the return type yet, use UNIT_TYPE as placeholder.
                    let mut ib = FunctionBuilder::new(&init_fn_name, UNIT_TYPE, &[]);
                    ctx.clear_locals();
                    ctx.drops.push_scope(drops::DropScopeKind::Function);
                    let operand = exprs::lower_expr(ctx, &mut ib, &binding.expr);
                    let gir_type = stmts::infer_operand_type_with_builder(ctx, &operand, &ib);
                    // Store the value in a local. The local is the value we will return.
                    let local_id = ib.add_local(gir_type, Some("__wb_val"));
                    ctx.drops.register_local(local_id, gir_type, &ctx.type_registry);
                    ib.assign(crate::ir::instructions::Place::local(local_id), operand);
                    // Fix up return type and the return place type now that we know it.
                    ib.return_type = gir_type;
                    ib.locals[0].type_id = gir_type; // _0 is the return place
                    // Drop everything in scope EXCEPT local_id — it's being moved out.
                    // Using emit_early_exit_drops mirrors how lower_return handles exclusion.
                    ctx.drops.emit_early_exit_drops(&mut ib, &ctx.type_registry, drops::DropScopeKind::Function, Some(local_id));
                    ctx.drops.pop_scope_no_emit();
                    ib.ret(Operand::Move(crate::ir::instructions::Place::local(local_id)));
                    let init_fn = ib.build();
                    let actual_type = init_fn.return_type;

                    // Create a MutPtr type for the test function parameter.
                    let ptr_type = ctx.type_registry.insert(crate::ir::types::GirType::MutPtr(actual_type));
                    wb_param_types.push(actual_type);
                    wb_ptr_types.push(ptr_type);
                    with_binding_infos.push(crate::ir::TestWithBinding {
                        var_name: binding.name.node.clone(),
                        init_fn_name: init_fn_name.clone(),
                        type_id: actual_type,
                    });
                    module.functions.push(init_fn);
                }

                // Restore for next iteration
                ctx.clear_locals();
            }

            // Build the test function. If there are with-bindings, it takes MutPtr parameters.
            let param_specs: Vec<(TypeId, Option<&str>)> = wb_ptr_types.iter()
                .map(|&t| (t, None))
                .collect();

            let mut builder = FunctionBuilder::new(&fn_name, UNIT_TYPE, &param_specs);
            ctx.clear_locals();
            ctx.drops.push_scope(drops::DropScopeKind::Function);

            // Register with-binding pointer parameters and expose them as dereferences.
            // In the test body, `r` refers to `*__wb_ptr_0` (the pointer param).
            // We register `r` as a local that IS the pointer param, type = T (not *T),
            // by storing it in a deref local.
            for (wb_idx, binding_info) in with_binding_infos.iter().enumerate() {
                // The pointer parameter is local _1, _2, ... (after _0 = return place).
                // FunctionBuilder params are locals 1..=n.
                let ptr_local_id = crate::ir::types::LocalId((wb_idx + 1) as u32);
                let ptr_type = wb_ptr_types[wb_idx];
                // Dereference: create a local for the dereferenced value
                let val_local_id = builder.add_local(wb_param_types[wb_idx], Some(&binding_info.var_name));
                // Emit load: val = *ptr
                builder.assign(
                    crate::ir::instructions::Place::local(val_local_id),
                    Operand::Copy(crate::ir::instructions::Place {
                        local: ptr_local_id,
                        projections: vec![crate::ir::instructions::Projection::Deref],
                    }),
                );
                ctx.register_local(&binding_info.var_name, val_local_id, wb_param_types[wb_idx]);
                // DO NOT register with drop elaborator — with-bindings are dropped by
                // the test runner via the cleanup stack, not the test function's scope exit.
                let _ = ptr_type; // suppress unused warning
            }

            stmts::lower_block(ctx, &mut builder, &test_def.body);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }

            let mut test_fn = builder.build();
            test_fn.is_test_fn = true;
            module.functions.push(test_fn);
            module.test_fns.push(TestFnInfo {
                fn_name,
                display_name: test_name,
                should_panic,
                expected_panic_msg,
                with_bindings: with_binding_infos,
            });
        }
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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
    Color c = Color.Red()
    pass
"#;
        let (module, result) = parse_and_analyze(source);
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());
        let c_code = crate::backend::c::generate_c(&gir).c_code;

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());
        let c_code = crate::backend::c::generate_c(&gir).c_code;

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());

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
        let gir = lower_module(&module, &result, &LoweringOptions::default());
        let c_code = crate::backend::c::generate_c(&gir).c_code;

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

/// FNV-1a 64-bit hash for stable struct layout hashing (used by hot-reload).
fn fnv1a_hash(s: &str) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;
    for b in s.bytes() {
        hash ^= b as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}
