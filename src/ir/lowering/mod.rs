pub mod builtins;
pub mod closures;
pub mod context;
pub mod drops;
pub mod exprs;
pub mod fault_participation;
pub mod functions;
pub mod generics;
pub mod liveness;
pub mod stmts;
pub mod traits;
pub mod types;

use crate::ir::types::*;
use crate::ir::{ExternDecl, Module};
use crate::parser::ast::{self, FunctionBody, Item};
use crate::semantic::AnalysisResult;

use std::collections::HashMap;
use std::time::{Duration, Instant};

use context::LoweringContext;
use functions::lower_function;
use generics::GenericCollector;
use types::TypeMapper;

/// Local timing helper for per-sub-pass instrumentation in `lower_module`.
/// Wraps an inline block so callers don't have to manage two `Instant`s
/// across borrow boundaries (the lowering body interleaves `&mut module`
/// and `&mut ctx` everywhere).
macro_rules! time_pass {
    ($map:expr, $name:literal, $body:block) => {{
        let __t = std::time::Instant::now();
        let __r = $body;
        *$map.entry($name).or_insert_with(std::time::Duration::default) += __t.elapsed();
        __r
    }};
}

/// Options controlling the GIR lowering pass, typically sourced from CLI flags.
#[derive(Debug, Default, Clone)]
pub struct LoweringOptions {
    /// Override `directive strip-asserts`: asserts become no-ops.
    pub strip_asserts: bool,
    /// Override `directive strip-asserts` off: force-keep asserts.
    pub no_strip_asserts: bool,
    /// When true, lower test items even when a `main()` exists (for `gg test`).
    pub test_mode: bool,
    /// When true, lower bench items instead of test items (for `gg test --bench`).
    pub bench_mode: bool,
    /// Only run tests whose tags include one of these (empty = run all).
    pub test_tags: Vec<String>,
    /// Skip tests whose tags include any of these.
    pub test_exclude_tags: Vec<String>,
    /// Only run tests whose display name contains this substring.
    pub test_name_filter: Option<String>,
    /// Global default timeout in milliseconds for all tests (from `--timeout`).
    pub default_timeout_ms: Option<u64>,
    /// Names of previously-failed tests to run first (for `--failed-first`).
    pub failed_first_names: Vec<String>,
    /// Whether snapshot capture mode is active (`--snapshot save`).
    pub snapshot_mode: bool,
    /// Enable trace instrumentation and write events to this file path.
    pub trace_filename: Option<String>,
    /// Enable hot-reload mode (directive hot-reload or --hot-reload flag).
    pub hot_reload: bool,
    /// Compile with AddressSanitizer + UBSan (`-fsanitize=address,undefined`).
    pub sanitize: bool,
    /// Override scheduler backend (pool, thread, inline, single).
    pub scheduler_mode: Option<crate::ir::SchedulerMode>,
    /// Release build (`--release`): compile the generated C at `-O2` instead of
    /// the `-O0` default. Affects only the backend `cc`/`llc` opt level — never
    /// program behavior. Default `false` keeps the implicit `-O0` path (no `-g`
    /// unless `--sanitize`).
    pub release: bool,
    /// `--clones=stats`: emit per-clone-site runtime attribution. Each implicit
    /// clone site (identified by its `CloneId`) gets a counter bump emitted
    /// immediately before the clone call; the backend emits the counter table
    /// + atexit `[clone-site]` report. Zero-cost when false: no instructions
    /// are emitted at all.
    pub clone_stats: bool,
}

/// Lower an AST module + analysis result into a GIR module.
pub fn lower_module(
    ast_module: &ast::Module,
    analysis: &AnalysisResult,
    options: &LoweringOptions,
) -> Module {
    let mut module = Module::new();
    let mut pass_times: HashMap<&'static str, Duration> = HashMap::new();
    let __lower_module_t0 = Instant::now();

    // Phase 5: pre-compute module function name manglings BEFORE flattening so we
    // retain the module path information.  Maps func_name.span.start → mangled C name
    // for every non-generic function inside an `Item::Module` wrapper.  The mangled
    // name has the form  `seg1__seg2___func_name`  (module segments joined by `__`,
    // then `___` separator, then the Gorget function name).  This prevents C linker
    // collisions when multiple file-based modules define the same function name.
    let __pass_t = Instant::now();
    let module_fn_manglings: rustc_hash::FxHashMap<usize, String> = {
        fn collect(
            path: &[String],
            items: &[crate::span::Spanned<ast::Item>],
            out: &mut rustc_hash::FxHashMap<usize, String>,
        ) {
            for item in items {
                match &item.node {
                    ast::Item::Function(f)
                        if f.generic_params.is_none()
                            && !path.is_empty()
                            // Exclude synthetic (Declaration) and extern functions — they are
                            // implemented in the C runtime and must keep their original C names.
                            && !matches!(
                                f.body,
                                ast::FunctionBody::Declaration | ast::FunctionBody::Extern(_)
                            ) =>
                    {
                        out.insert(
                            f.name.span.start,
                            format!("{}___{}", path.join("__"), f.name.node),
                        );
                    }
                    ast::Item::Module { path: mod_path, items: inner } => {
                        collect(mod_path, inner, out);
                    }
                    _ => {}
                }
            }
        }
        let mut map = rustc_hash::FxHashMap::default();
        collect(&[], &ast_module.items, &mut map);
        map
    };

    // Flatten `Item::Module` wrappers produced by `merge_modules()` so all subsequent
    // lowering passes see a unified item list (matching the pre-module-scope behavior
    // where all items from all imported modules were at the top level). Semantic analysis
    // has already resolved all name references using the module-scoped information; GIR
    // lowering only needs the items themselves.
    let flat_module_items: Vec<crate::span::Spanned<ast::Item>> = {
        fn flatten(items: &[crate::span::Spanned<ast::Item>]) -> Vec<crate::span::Spanned<ast::Item>> {
            let mut result = Vec::new();
            for item in items {
                if let ast::Item::Module { items: inner, .. } = &item.node {
                    result.extend(flatten(inner));
                } else {
                    result.push(item.clone());
                }
            }
            result
        }
        flatten(&ast_module.items)
    };
    // Shadow `ast_module` with the flat view for all subsequent passes in this function
    // and in any helper functions that receive `ast_module` as a parameter.
    let flat_module = ast::Module { items: flat_module_items, span: ast_module.span };
    let ast_module = &flat_module;
    *pass_times.entry("flatten_and_manglings").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
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

    // Register the compiler-internal `Fault` enum (error-model.md §11.1) as a
    // concrete non-generic enum TypeDef. Unlike the generic Option/Result
    // (lazily monomorphized from `enum_templates` on use), `Fault` is
    // non-generic and used in a TYPED position (`Fault f = …`, `match f`,
    // `Fault.Overflow()`), so its TypeDef + variant layout must exist eagerly.
    // `register_enum_type` is idempotent (no-op if a user already defined a
    // `Fault`, though `Fault` is a reserved built-in name here).
    types::register_enum_type(
        &mut type_mapper,
        &mut module.type_registry,
        &crate::ir::lowering::generics::builtin_fault_enum(),
        &generic_templates,
    );

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
                            type_def.metadata.copy_semantics = CopySemantics::Resource;
                            type_def.metadata.drop_strategy = DropStrategy::Custom(format!("{name_str}__drop"));
                        }
                    }
                }
            }
        }
    }

    // Scan for types with droppable fields — see upgrade_types_from_fields()
    upgrade_types_from_fields(&mut module);

    // Register runtime types needed by expression lowering
    // GorgetString: register in named_types so method dispatch can find them
    type_mapper.register_named("GorgetString".to_string(), type_mapper.owned_string_type);

    // GorgetArray: opaque runtime array (element_size, data, len, cap)
    {
        module.type_registry.add_type_def(TypeDef {
            name: "GorgetArray".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: Some(64),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("gorget_array_free".to_string()),
                copy_semantics: CopySemantics::Resource,
                clone_fn: Some("gorget_array_clone".to_string()),
                clone_inplace_fn: Some("gorget_array_clone_inplace".to_string()),
                materialize_fn: None,
                // No view-safe free for collections yet (Phase 1b) — see
                // TypeMetadata::borrow_view_fn.
                borrow_view_fn: None,
                collection_kind: Some(CollectionKind::Array),
                enum_category: None,
                c_runtime_alias: None,
                is_closure_env: false,
                is_box: false,
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
                size: Some(152),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("gorget_map_free".to_string()),
                copy_semantics: CopySemantics::Resource,
                clone_fn: Some("gorget_map_clone".to_string()),
                clone_inplace_fn: Some("gorget_map_clone_inplace".to_string()),
                materialize_fn: None,
                // No view-safe free for collections yet (Phase 1b) — see
                // TypeMetadata::borrow_view_fn.
                borrow_view_fn: None,
                collection_kind: Some(CollectionKind::Map),
                enum_category: None,
                c_runtime_alias: None,
                is_closure_env: false,
                is_box: false,
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
                size: Some(152),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("gorget_set_free".to_string()),
                copy_semantics: CopySemantics::Resource,
                clone_fn: Some("gorget_set_clone".to_string()),
                clone_inplace_fn: Some("gorget_set_clone_inplace".to_string()),
                materialize_fn: None,
                // No view-safe free for collections yet (Phase 1b) — see
                // TypeMetadata::borrow_view_fn.
                borrow_view_fn: None,
                collection_kind: Some(CollectionKind::Set),
                enum_category: None,
                c_runtime_alias: None,
                is_closure_env: false,
                is_box: false,
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
                copy_semantics: CopySemantics::Trivial,
                ..TypeMetadata::default()
            },
        };
        module.type_registry.add_type_def(range_def);
        let range_type_id = module.type_registry.insert(GirType::Named("GorgetRange".to_string()));
        type_mapper.register_named("GorgetRange".to_string(), range_type_id);
    }

    *pass_times.entry("prescan_types").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // P2.3: Generic monomorphization — collect templates, discover usages, monomorphize
    let mut generic_collector = GenericCollector::new();
    generic_collector.collect_templates(ast_module);
    generic_collector.discover_usages(ast_module);
    generic_collector.discover_transitive(Some(ast_module));
    // P2.3b: Per-call-site specialization for method-level-generic equip methods
    // (e.g. `v.iter().map[U, F](f)`). Requires typecheck output (`expr_types`)
    // to resolve the receiver's concrete type at each call site.
    generic_collector.discover_method_instances(ast_module, analysis);

    // Pre-register collection type names BEFORE monomorphization so that
    // monomorphize_enum can resolve inner types like Vector[int] in Option[Vector[int]].
    for (base_name, _type_args, mangled_name, kind) in generic_collector.instances_raw() {
        if !matches!(kind, generics::TemplateKind::Struct | generics::TemplateKind::Enum) {
            continue;
        }
        // Phase A: pull metadata from BuiltinTypeProtocol so the same fields
        // populate as in map_ast_type_mut and register_collection_alias —
        // single source of truth via the protocol table.
        if let Some(protocol) = builtins::lookup_protocol(base_name.as_str()) {
            if protocol.collection_kind.is_some()
                && !type_mapper.named_types.contains_key(mangled_name)
            {
                let drop_strat = match protocol.drop_fn {
                    Some(f) => DropStrategy::Trivial(f.to_string()),
                    None => DropStrategy::None,
                };
                module.type_registry.add_type_def(TypeDef {
                    name: mangled_name.clone(),
                    kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                    metadata: TypeMetadata {
                        size: None,
                        align: None,
                        drop_strategy: drop_strat,
                        copy_semantics: protocol.copy_semantics,
                        clone_fn: protocol.clone_fn.map(String::from),
                        clone_inplace_fn: protocol.clone_inplace_fn.map(String::from),
                        materialize_fn: protocol.materialize_fn.map(String::from),
                        borrow_view_fn: protocol.borrow_view_fn.map(String::from),
                        collection_kind: protocol.collection_kind,
                        enum_category: None,
                        c_runtime_alias: protocol.c_runtime_alias.map(String::from),
                        is_closure_env: false,
                is_box: false,
                    },
                });
                let tid = module.type_registry.insert(GirType::Named(mangled_name.clone()));
                type_mapper.register_named(mangled_name.clone(), tid);
            }
        }
    }

    generic_collector.monomorphize_types(&mut type_mapper, &mut module.type_registry);

    // Register typed vector aliases AFTER generic monomorphization so we reuse
    // any TypeIds already created by the generic collector (e.g., Vector__Str from
    // explicit Vector[str] usage in source code). Phase A: pull metadata from
    // BuiltinTypeProtocol so clone_fn is populated.
    if let Some(vector_protocol) = builtins::lookup_protocol("Vector") {
        let drop_strat = match vector_protocol.drop_fn {
            Some(f) => DropStrategy::Trivial(f.to_string()),
            None => DropStrategy::None,
        };
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
                            drop_strategy: drop_strat.clone(),
                            copy_semantics: vector_protocol.copy_semantics,
                            clone_fn: vector_protocol.clone_fn.map(String::from),
                            clone_inplace_fn: vector_protocol.clone_inplace_fn.map(String::from),
                            materialize_fn: vector_protocol.materialize_fn.map(String::from),
                            borrow_view_fn: vector_protocol.borrow_view_fn.map(String::from),
                            collection_kind: vector_protocol.collection_kind,
                            enum_category: None,
                            c_runtime_alias: vector_protocol.c_runtime_alias.map(String::from),
                            is_closure_env: false,
                is_box: false,
                        },
                    });
                }
                let tid = module.type_registry.insert(GirType::Named(gir_name.to_string()));
                type_mapper.register_named(name.to_string(), tid);
            }
        }
    }

    // Register printf as an extern (variadic)
    module.externs.push(ExternDecl {
        name: "printf".into(),
        params: vec![], // variadic — actual params vary per call
        return_type: I32_TYPE,
        is_variadic: true,
        param_abis: vec![],
        returns_borrowed: false,
    });
    *pass_times.entry("monomorphize").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Move type_registry into LoweringContext for the lowering phase
    let type_registry = std::mem::take(&mut module.type_registry);
    let mut ctx = LoweringContext::new(analysis, type_mapper, type_registry);

    // Extract directive flags from AST
    for item in &ast_module.items {
        if let Item::Directive(d) = &item.node {
            match d.name.as_str() {
                "strip-asserts" => ctx.strip_asserts = true,
                "scheduler" => match d.value.as_deref() {
                    Some("pool") => ctx.spawn.scheduler_mode = crate::ir::SchedulerMode::Pool,
                    Some("thread") => ctx.spawn.scheduler_mode = crate::ir::SchedulerMode::Thread,
                    Some("inline") => ctx.spawn.scheduler_mode = crate::ir::SchedulerMode::Inline,
                    Some("single") => ctx.spawn.scheduler_mode = crate::ir::SchedulerMode::Single,
                    _ => {}
                },
                _ => {}
            }
        }
    }
    // CLI flags override directives
    if options.strip_asserts { ctx.strip_asserts = true; }
    if options.no_strip_asserts { ctx.strip_asserts = false; }
    if options.snapshot_mode { ctx.snapshot_mode = true; }
    if options.clone_stats { ctx.clone_stats = true; }
    if let Some(m) = options.scheduler_mode { ctx.spawn.scheduler_mode = m; }

    // Snag #29 follow-up: PI/E/TAU/INT_MAX/INT_MIN auto-injection removed.
    // `lib/std/math.gg` carries `const float PI = …` (etc.) — users `from
    // std.math import PI, E, TAU`, the const-eval path resolves them at
    // compile time, and the stdlib's `const` declarations are the single
    // source of truth (Layering rule 3). INT_MAX / INT_MIN were unused
    // outside comments and dropped; if needed they belong on a typed
    // stdlib module.
    //
    // INFINITY / NAN: defined as runtime-init statics in lib/std/math.gg
    // (`public float INFINITY = _math_infinity()`). Lowered through the
    // normal cross-module global-init path — no auto-injection here.
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

    *pass_times.entry("setup_ctx_and_directives").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // P2.5: Register VTable and TraitObj types for all trait definitions
    let trait_info = traits::register_trait_types(&mut ctx, ast_module);

    // Populate struct field info cache (includes monomorphized + vtable/trait obj types)
    ctx.populate_struct_fields();
    *pass_times.entry("register_trait_types").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();

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

    *pass_times.entry("prescan_enum_variants").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Pre-scan: build fn_sigs map for all non-generic functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // Generic functions handled via monomorphization
            }
            let name = &func.name.node;
            let is_main = name == "main";

            let ret_type = if is_main && !func.throws.declares_throws() {
                I32_TYPE
            } else if func.throws.declares_throws() {
                // `int foo() throws str` → Result[int, str]. One source of
                // truth (devbook-24 rule 3): synthesize via the shared helper.
                crate::ir::lowering::types::synthesize_throws_result_type(
                    &mut ctx.type_mapper,
                    &mut ctx.type_registry,
                    &func.return_type.node,
                    &func.throws.explicit_type().unwrap().node,
                )
            } else {
                // Use map_ast_type_mut so tuple return types get registered on the fly
                ctx.type_mapper.map_ast_type_mut(&func.return_type.node, &mut ctx.type_registry)
            };

            let param_types: Vec<TypeId> = func
                .params
                .iter()
                .map(|p| ctx.type_mapper.map_ast_type_mut(&p.node.type_.node, &mut ctx.type_registry))
                .collect();

            // Check whether this function is from a non-entry module and needs mangling.
            let mangled_name = module_fn_manglings.get(&func.name.span.start).cloned();

            // Register under the bare name (always — call sites may use the short name).
            ctx.fn_sigs.insert(name.clone(), (param_types.clone(), ret_type));

            // Record parameter names for named-arg reordering
            let param_names: Vec<String> = func.params.iter()
                .map(|p| p.node.name.node.clone())
                .collect();
            ctx.fn_param_names.insert(name.clone(), param_names.clone());

            // Record parameter ownerships for token wrapper generation
            let param_ownerships: Vec<ast::Ownership> = func.params.iter()
                .map(|p| p.node.ownership.clone())
                .collect();
            ctx.fn_param_ownerships.insert(name.clone(), param_ownerships.clone());

            // Compute unified ParamABI from base types + ownerships
            let param_abis: Vec<context::ParamABI> = param_types.iter().zip(param_ownerships.iter())
                .map(|(&base_type, ownership)| ctx.compute_param_abi(base_type, ownership.clone()))
                .collect();
            ctx.fn_param_abis.insert(name.clone(), param_abis.clone());

            // Record extern ABI kinds from FunctionDef (for Declaration functions)
            if !func.param_abis.is_empty() {
                ctx.fn_extern_abi_kinds.insert(name.clone(), func.param_abis.clone());
            }

            // Record default parameter values
            let defaults: Vec<(usize, ast::Expr)> = func.params.iter()
                .enumerate()
                .filter_map(|(i, p)| {
                    p.node.default.as_ref().map(|d| (i, d.node.clone()))
                })
                .collect();
            if !defaults.is_empty() {
                ctx.fn_defaults.insert(name.clone(), defaults.clone());
            }

            // Store function AST for async-shared variant generation
            if func.qualifiers.is_async && !matches!(func.body, FunctionBody::Extern(_) | FunctionBody::Declaration) {
                ctx.shared.fn_ast_bodies.insert(name.clone(), func.clone());
            }

            // Track yield points (async or blocking functions).
            if func.qualifiers.is_async || func.qualifiers.is_blocking {
                ctx.yield_point_fns.insert(name.clone());
                // Also register C symbol name for extern functions
                if let FunctionBody::Extern(c_symbol) = &func.body {
                    ctx.yield_point_fns.insert(c_symbol.clone());
                }
            }

            // Track noreturn functions — IR must terminate the basic block
            // with `unreachable` after the call so divergent match arms
            // compose correctly with the surrounding result type.
            if func.qualifiers.is_noreturn {
                ctx.noreturn_fns.insert(name.clone());
                if let FunctionBody::Extern(c_symbol) = &func.body {
                    ctx.noreturn_fns.insert(c_symbol.clone());
                }
            }

            // Record extern binding: Gorget name → C symbol (takes priority over mangling).
            // Declaration functions are C-runtime inline implementations; do not rename them.
            if let FunctionBody::Extern(c_symbol) = &func.body {
                ctx.extern_bindings.insert(name.clone(), c_symbol.clone());
                // Derive param ABI from inline extern's language tag or explicit cstr types.
                // extern "C" → String params become CStr (same as extern "C": blocks).
                {
                    use crate::ir::abi::AbiKind;
                    // Bare `extern` (no `extern "C"`/`extern "Gorget"` annotation) defaults
                    // to Gorget — String params cross the FFI as a 32-byte `Str` struct by
                    // value, matching the convention every gorget_* runtime function uses.
                    // Without this default, the LIR registers String params as Auto, which
                    // the LLVM x86_64 backend can't distinguish from "pass a pointer", so
                    // it omits the byval(...) attr and the C side reads the Str struct
                    // from the wrong place. (aarch64 large-aggregate-by-value happens to
                    // be compatible with bare `ptr` IR, hiding the bug there.)
                    let string_abi = match func.extern_abi.as_deref() {
                        Some("C") => AbiKind::CStr,
                        _ => AbiKind::GorgetString,
                    };
                    let abis: Vec<AbiKind> = func.params.iter().map(|p| {
                        let tid = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                        // See the ExternBlock path below: `Ref[T]`/`MutRef[T]` borrow
                        // params are aggregate-by-pointer and must pin to AbiKind::Ptr
                        // (is_resource_type can't see through the Ptr wrapper).
                        let is_borrow_named = matches!(&p.node.type_.node,
                            ast::Type::Named { name, generic_args }
                                if (name.node == "Ref" || name.node == "MutRef")
                                    && generic_args.len() == 1);
                        if matches!(p.node.type_.node, ast::Type::Primitive(ast::PrimitiveType::CStr)) {
                            AbiKind::CStr
                        } else if is_borrow_named {
                            AbiKind::Ptr
                        } else if ctx.type_mapper.is_string_type(tid) {
                            string_abi
                        } else if ctx.type_registry.is_resource_type(tid) && !ctx.type_mapper.is_string_type(tid) {
                            AbiKind::Ptr
                        } else {
                            AbiKind::Auto
                        }
                    }).collect();
                    if abis.iter().any(|a| *a != AbiKind::Auto) {
                        ctx.fn_extern_abi_kinds.insert(name.clone(), abis.clone());
                        ctx.fn_extern_abi_kinds.insert(c_symbol.clone(), abis);
                    }
                    // Derive return ABI from explicit cstr return type.
                    if matches!(func.return_type.node, ast::Type::Primitive(ast::PrimitiveType::CStr)) {
                        ctx.fn_return_abis.insert(name.clone(), AbiKind::CStr);
                        ctx.fn_return_abis.insert(c_symbol.clone(), AbiKind::CStr);
                    }
                    // `extern borrowed T f(...)` — record the function as
                    // returning a borrowed pointer (auto-clone TODO).
                    if func.returns_borrowed {
                        ctx.fn_returns_borrowed.insert(name.clone());
                        ctx.fn_returns_borrowed.insert(c_symbol.clone());
                    }
                }
            } else if !matches!(func.body, FunctionBody::Declaration) {
                if let Some(ref mangled) = mangled_name {
                // Phase 5: also register the mangled name so fn_sigs lookups using the
                // mangled name (from lower_function) resolve correctly.
                ctx.fn_sigs.insert(mangled.clone(), (param_types, ret_type));
                ctx.fn_param_names.insert(mangled.clone(), param_names);
                ctx.fn_param_ownerships.insert(mangled.clone(), param_ownerships);
                ctx.fn_param_abis.insert(mangled.clone(), param_abis);
                if !defaults.is_empty() {
                    ctx.fn_defaults.insert(mangled.clone(), defaults);
                }
                // Map bare name → mangled C name at every call site that uses the short name.
                ctx.extern_bindings.insert(name.clone(), mangled.clone());
                }  // if let Some(ref mangled)
            }  // else if !Declaration
        }

        // Handle extern blocks: register each function + derive ABI from block's ABI string.
        // extern "C": → String params → CStr
        // extern "Gorget": → String params → GorgetString
        // bare extern: → defaults to Gorget (matches the inline `extern void f(...)`
        //   form's default — every gorget_* runtime function takes Str by value).
        if let Item::ExternBlock(ext) = &item.node {
            use crate::ir::abi::AbiKind;
            let string_abi = match ext.abi.as_ref().map(|a| a.node.as_str()) {
                Some("C") => AbiKind::CStr,
                _ => AbiKind::GorgetString,
            };

            for func_spanned in &ext.items {
                let func = &func_spanned.node;
                let name = &func.name.node;

                let ret_type = ctx.type_mapper.map_ast_type_mut(&func.return_type.node, &mut ctx.type_registry);
                let param_types: Vec<TypeId> = func.params.iter()
                    .map(|p| ctx.type_mapper.map_ast_type_mut(&p.node.type_.node, &mut ctx.type_registry))
                    .collect();

                ctx.fn_sigs.insert(name.clone(), (param_types.clone(), ret_type));

                let param_names: Vec<String> = func.params.iter()
                    .map(|p| p.node.name.node.clone()).collect();
                ctx.fn_param_names.insert(name.clone(), param_names);

                let param_ownerships: Vec<ast::Ownership> = func.params.iter()
                    .map(|p| p.node.ownership.clone()).collect();
                ctx.fn_param_ownerships.insert(name.clone(), param_ownerships.clone());

                let param_abis: Vec<context::ParamABI> = param_types.iter().zip(param_ownerships.iter())
                    .map(|(&base_type, ownership)| ctx.compute_param_abi(base_type, ownership.clone()))
                    .collect();
                ctx.fn_param_abis.insert(name.clone(), param_abis);

                // Track yield points (async or blocking functions)
                if func.qualifiers.is_async || func.qualifiers.is_blocking {
                    ctx.yield_point_fns.insert(name.clone());
                    if let FunctionBody::Extern(c_symbol) = &func.body {
                        ctx.yield_point_fns.insert(c_symbol.clone());
                    }
                }

                // Track noreturn extern functions (exit, abort, …) so the IR
                // can emit `unreachable` after the call. Without this the
                // basic block doesn't terminate and divergent uses (e.g.
                // an Error-arm `exit(2)` in a `T x = match …` expression)
                // fall through to a bogus arm-value assign.
                if func.qualifiers.is_noreturn {
                    ctx.noreturn_fns.insert(name.clone());
                    if let FunctionBody::Extern(c_symbol) = &func.body {
                        ctx.noreturn_fns.insert(c_symbol.clone());
                    }
                }

                // Register extern binding: name → C symbol
                if let FunctionBody::Extern(c_symbol) = &func.body {
                    ctx.extern_bindings.insert(name.clone(), c_symbol.clone());
                }

                // Derive ABI kinds from block's ABI string
                // Param ABI: derive from explicit cstr type in declaration.
                // cstr param → CStr ABI. String param in extern "Gorget" → GorgetString.
                // String param in extern "C" → CStr (backward compat until all migrated to explicit cstr).
                {
                    let abis: Vec<AbiKind> = func.params.iter().zip(param_types.iter()).map(|(p, &tid)| {
                        // `Ref[T]` / `MutRef[T]` extern params lower to GirType::Ptr/MutPtr
                        // (a borrow of an aggregate the C runtime takes by pointer). The
                        // resolved tid is a Ptr, which `is_resource_type` does NOT classify
                        // as a resource (it only matches GirType::Named), so without this
                        // branch the param falls through to AbiKind::Auto — and on x86_64
                        // the LLVM backend then byval's the aggregate (corruption) instead
                        // of passing the pointer. Detect the borrow at the AST level and
                        // pin it to AbiKind::Ptr.
                        let is_borrow_named = matches!(&p.node.type_.node,
                            ast::Type::Named { name, generic_args }
                                if (name.node == "Ref" || name.node == "MutRef")
                                    && generic_args.len() == 1);
                        if matches!(p.node.type_.node, ast::Type::Pointer(_)) || is_borrow_named {
                            AbiKind::Ptr
                        } else if matches!(p.node.type_.node, ast::Type::Primitive(ast::PrimitiveType::CStr)) {
                            AbiKind::CStr
                        } else if ctx.type_mapper.is_string_type(tid) {
                            string_abi
                        } else if !ctx.type_mapper.is_string_type(tid) && ctx.type_registry.is_resource_type(tid) {
                            // Resource types (collections, opaque handles) are passed by pointer in C
                            AbiKind::Ptr
                        } else {
                            AbiKind::Auto
                        }
                    }).collect();
                    if abis.iter().any(|a| *a != AbiKind::Auto) {
                        ctx.fn_extern_abi_kinds.insert(name.clone(), abis.clone());
                        if let FunctionBody::Extern(c_symbol) = &func.body {
                            ctx.fn_extern_abi_kinds.insert(c_symbol.clone(), abis);
                        }
                    }
                }
                // Return ABI: derive from explicit cstr return type.
                if matches!(func.return_type.node, ast::Type::Primitive(ast::PrimitiveType::CStr)) {
                    ctx.fn_return_abis.insert(name.clone(), AbiKind::CStr);
                    if let FunctionBody::Extern(c_symbol) = &func.body {
                        ctx.fn_return_abis.insert(c_symbol.clone(), AbiKind::CStr);
                    }
                }
                // `extern borrowed T f(...)` → record the function as
                // returning a borrowed pointer. The call-site auto-clone
                // consumer is not yet wired (see TODO.md).
                if func.returns_borrowed {
                    ctx.fn_returns_borrowed.insert(name.clone());
                    if let FunctionBody::Extern(c_symbol) = &func.body {
                        ctx.fn_returns_borrowed.insert(c_symbol.clone());
                    }
                }
            }
        }
    }

    *pass_times.entry("prescan_fn_sigs").or_default() += __pass_t.elapsed();

    // Cross-frame fault-propagation participation (error-model.md §11, Inc-2.1a).
    // Compute the set of functions that BOTH have an uncaught faultable
    // arithmetic op AND are directly called from a `FaultCatch`-Overflow scope.
    // ONLY these get the synthesized trailing `MutPtr<i32>` fault-slot param,
    // and every direct caller passes the trailing arg (uniform signature, D5).
    // Empty for the entire existing suite + self-host (no `catch Fault.X` over a
    // user CALL) → zero signature change there. Must run BEFORE body lowering so
    // the signature lowering and the call-site gate both see the typed flag.
    ctx.participating_fault_fns =
        crate::ir::lowering::fault_participation::compute_participating_fault_fns(
            &ast_module.items,
        );
    // Append the synthesized trailing fault-slot's ByMutPtr ABI to each
    // participating fn's `fn_param_abis` (the prescan recorded only the user
    // params). The consume-site / read-site validators read this so the slot
    // arg of a `FaultableCall` resolves to a borrow shape, never a consume.
    {
        let participating: Vec<String> =
            ctx.participating_fault_fns.iter().cloned().collect();
        for name in participating {
            if let Some(abis) = ctx.fn_param_abis.get_mut(&name) {
                abis.push(context::ParamABI::ByMutPtr);
            }
        }
    }

    let __pass_t = Instant::now();
    // Build call_resolved_names: for each entry in resolution_map that points to a
    // module-mangled function, record call_span → mangled_name.  This lets call lowering
    // pick the correct target when multiple modules define the same bare function name.
    for (&call_span_start, &def_id) in &analysis.resolution_map {
        let def_info = analysis.scopes.get_def(def_id);
        if def_info.kind == crate::semantic::scope::DefKind::Function {
            if let Some(mangled) = module_fn_manglings.get(&def_info.span.start) {
                ctx.call_resolved_names.insert(call_span_start, mangled.clone());
            }
        }
    }

    // Register monomorphized function signatures
    generic_collector.register_fn_sigs(&ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs, &mut ctx.fn_param_ownerships, &mut ctx.fn_param_abis);

    // Propagate extern bindings to monomorphized instances of generic extern fns.
    // A generic `extern "Gorget" int f[K, V](Dict[K, V] &m) = "c_symbol"` has one
    // C symbol (`c_symbol`), but callers with type args get mangled names
    // (`f__int64_t__int64_t`). Without this pass, the mangled call site misses the
    // extern binding and codegen emits the mangled name as the callee — linker
    // fails because only the base name maps to `c_symbol`. Propagate the mapping +
    // ABI metadata from the base to every monomorphized instance.
    for (base_name, _type_args, mangled_name, kind) in generic_collector.instances_raw() {
        if !matches!(kind, generics::TemplateKind::Function) { continue; }
        if base_name == mangled_name { continue; }
        let Some(template) = generic_collector.get_fn_template(base_name) else { continue; };
        let FunctionBody::Extern(c_symbol) = &template.body else { continue; };
        // extern_bindings: mangled → C symbol (same C symbol for every mono instance).
        ctx.extern_bindings.insert(mangled_name.clone(), c_symbol.clone());
        // Propagate per-function ABI metadata keyed by the base name.
        if let Some(abis) = ctx.fn_extern_abi_kinds.get(base_name).cloned() {
            ctx.fn_extern_abi_kinds.insert(mangled_name.clone(), abis);
        }
        if let Some(abi) = ctx.fn_return_abis.get(base_name).copied() {
            ctx.fn_return_abis.insert(mangled_name.clone(), abi);
        }
        if template.qualifiers.is_async || template.qualifiers.is_blocking {
            ctx.yield_point_fns.insert(mangled_name.clone());
        }
    }

    // Register ABI tags for compiler-emitted runtime calls (not declared in .gg files).
    // Under 32-byte Str, functions that take const char* for Str params need CStr
    // marshalling so the caller extracts .data from the Str struct.
    {
        use crate::ir::abi::AbiKind;
        ctx.fn_extern_abi_kinds.insert("gorget_panic".to_string(), vec![AbiKind::CStr]);
        // gorget_trap(code, detail) — both const char* (D11). Without this the
        // new 2-Str-arg extern gets default ABIs; the C/LLVM boundary rewrites
        // it to gorget_trap_at threading the span (mirrors gorget_panic).
        ctx.fn_extern_abi_kinds.insert("gorget_trap".to_string(), vec![AbiKind::CStr, AbiKind::CStr]);
        // File operations: gorget_file_open(path, mode), gorget_file_write(file*, content)
        ctx.fn_extern_abi_kinds.insert("gorget_file_open".to_string(), vec![AbiKind::CStr, AbiKind::CStr]);
        ctx.fn_extern_abi_kinds.insert("gorget_file_write".to_string(), vec![AbiKind::Auto, AbiKind::CStr]);
        ctx.fn_extern_abi_kinds.insert("gorget_read_file".to_string(), vec![AbiKind::CStr]);
        ctx.fn_extern_abi_kinds.insert("gorget_write_file".to_string(), vec![AbiKind::CStr, AbiKind::CStr]);
        ctx.fn_extern_abi_kinds.insert("gorget_append_file".to_string(), vec![AbiKind::CStr, AbiKind::CStr]);
        ctx.fn_extern_abi_kinds.insert("gorget_file_exists".to_string(), vec![AbiKind::CStr]);
        ctx.fn_extern_abi_kinds.insert("gorget_read_file_bytes".to_string(), vec![AbiKind::CStr]);
        // Process operations
        ctx.fn_extern_abi_kinds.insert("gorget_process_write_stdin".to_string(), vec![AbiKind::Auto, AbiKind::CStr]);
        // Bytes conversion
        ctx.fn_extern_abi_kinds.insert("gorget_bytes_from_str".to_string(), vec![AbiKind::CStr]);
    }

    // Pre-scan: register non-generic equip method signatures
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            // Skip generic equip blocks (handled via monomorphization)
            if equip.generic_params.is_some() {
                continue;
            }
            // Trait equip blocks: fn_sigs registered by register_trait_equip_sigs
            if equip.trait_.is_some() {
                continue;
            }

            // Accepts Type::Named (non-generic) AND Type::Primitive — the latter
            // handles `equip String:` / `equip int:` / etc. Returns None for
            // generic-Named (monomorphization handles those) and non-equippable
            // types. See docs/devbook/17-c-backend.md.
            if let Some(type_name) = types::equip_target_name(&equip.type_.node) {
                for method in &equip.items {
                    let method_def = &method.node;
                    let mangled = format!("{}__{}", type_name, method_def.name.node);

                    // `int add(self) throws str` → Result[int, str]. This
                    // pre-scan once registered bare `int` (no throws branch),
                    // so a `c.add(5) catch (e): …` call read `int64_t` while
                    // the emitted C method returned `Result` → ill-typed C.
                    // Route through the shared helper — same synthesis as the
                    // free-fn pre-scan and the method body (`functions.rs`).
                    let ret_type = if let Some(throws) = method_def.throws.explicit_type() {
                        types::synthesize_throws_result_type(
                            &mut ctx.type_mapper,
                            &mut ctx.type_registry,
                            &method_def.return_type.node,
                            &throws.node,
                        )
                    } else {
                        ctx.type_mapper.map_ast_type_mut(&method_def.return_type.node, &mut ctx.type_registry)
                    };
                    let has_self = method_def.params.first()
                        .map(|p| p.node.name.node == "self")
                        .unwrap_or(false);

                    let mut param_types = Vec::new();
                    if has_self {
                        let self_type_id = ctx.type_mapper.map_ast_type(&equip.type_.node);
                        let self_needs_mut_ptr = method_def.params.first()
                            .map(|p| matches!(p.node.ownership, ast::Ownership::MutableBorrow | ast::Ownership::Move))
                            .unwrap_or(false);
                        let self_ptr_type = if self_needs_mut_ptr {
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

                    // Compute and insert ParamABI for equip methods
                    let mut param_abis = Vec::new();
                    if has_self {
                        let self_needs_mut_ptr = method_def.params.first()
                            .map(|p| matches!(p.node.ownership, ast::Ownership::MutableBorrow | ast::Ownership::Move))
                            .unwrap_or(false);
                        param_abis.push(if self_needs_mut_ptr {
                            context::ParamABI::ByMutPtr
                        } else {
                            context::ParamABI::ByPtr
                        });
                    }
                    for p in &method_def.params {
                        if p.node.name.node == "self" { continue; }
                        let base = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                        param_abis.push(ctx.compute_param_abi(base, p.node.ownership));
                    }
                    ctx.fn_param_abis.insert(mangled.clone(), param_abis);

                    // Register param ownerships for equip methods (for ! param MoveZero)
                    let param_ownerships: Vec<ast::Ownership> = method_def.params.iter()
                        .map(|p| p.node.ownership)
                        .collect();
                    ctx.fn_param_ownerships.insert(mangled.clone(), param_ownerships);

                    // Register param names for equip methods, surface-indexed with
                    // `self` at idx 0. Used by the method-call default-fill /
                    // named-arg reordering in `methods.rs::lower_method_call`.
                    let method_param_names: Vec<String> = method_def.params.iter()
                        .map(|p| p.node.name.node.clone())
                        .collect();
                    ctx.fn_param_names.insert(mangled.clone(), method_param_names);

                    // Register default parameter values for equip methods, keyed by
                    // the mangled `Type__method` name. Surface-indexed with `self`
                    // at idx 0 (parser injects a synthetic self param) — the call-
                    // site default-fill in `methods.rs::lower_method_call` strips
                    // the self slot. Without this, a `p.add(5)` call to a method
                    // with a trailing default would not fill → "too few arguments
                    // to function 'P__add'". Mirrors the free-fn registration above.
                    let method_defaults: Vec<(usize, ast::Expr)> = method_def.params.iter()
                        .enumerate()
                        .filter_map(|(i, p)| p.node.default.as_ref().map(|d| (i, d.node.clone())))
                        .collect();
                    if !method_defaults.is_empty() {
                        ctx.fn_defaults.insert(mangled.clone(), method_defaults);
                    }

                    // Register extern binding for equip methods (e.g., UdpSocket__local_addr → gorget_udp_local_addr)
                    if let FunctionBody::Extern(c_symbol) = &method_def.body {
                        ctx.extern_bindings.insert(mangled.clone(), c_symbol.clone());

                        // Derive param ABI from inline extern's language tag or explicit cstr types.
                        // Prepend Auto for implicit self (always position 0 in C call).
                        //
                        // Note: bare `extern` here intentionally stays `Auto` (unlike inline
                        // top-level / extern-block bare extern, which default to Gorget).
                        // The hardcoded fn_extern_abi_kinds inserts at line 824-839 (e.g.
                        // `gorget_process_write_stdin → [Auto, CStr]`) cover the C-symbol
                        // names that bare equip externs bind to — and the `any(!= Auto)`
                        // guard below means an all-Auto vector here leaves those overrides
                        // intact. Promoting bare to Gorget would clobber them.
                        use crate::ir::abi::AbiKind;
                        let string_abi = match method_def.extern_abi.as_deref() {
                            Some("C") => AbiKind::CStr,
                            Some("Gorget") => AbiKind::GorgetString,
                            _ => AbiKind::Auto,
                        };
                        let mut abis: Vec<AbiKind> = Vec::new();
                        if has_self {
                            // Explicit self — included in the loop below
                        } else {
                            // Implicit self — not in method_def.params but present in C call
                            abis.push(AbiKind::Auto);
                        }
                        abis.extend(method_def.params.iter().map(|p| {
                            let tid = ctx.type_mapper.map_ast_type(&p.node.type_.node);
                            // `Ref[T]`/`MutRef[T]` borrow params are aggregate-by-pointer
                            // → AbiKind::Ptr (is_resource_type can't see the Ptr wrapper).
                            let is_borrow_named = matches!(&p.node.type_.node,
                                ast::Type::Named { name, generic_args }
                                    if (name.node == "Ref" || name.node == "MutRef")
                                        && generic_args.len() == 1);
                            if matches!(p.node.type_.node, ast::Type::Primitive(ast::PrimitiveType::CStr)) {
                                AbiKind::CStr
                            } else if is_borrow_named {
                                AbiKind::Ptr
                            } else if string_abi != AbiKind::Auto && ctx.type_mapper.is_string_type(tid) {
                                string_abi
                            } else if ctx.type_registry.is_resource_type(tid) && !ctx.type_mapper.is_string_type(tid) {
                                AbiKind::Ptr
                            } else {
                                AbiKind::Auto
                            }
                        }));
                        if abis.iter().any(|a| *a != crate::ir::abi::AbiKind::Auto) {
                            ctx.fn_extern_abi_kinds.insert(mangled.clone(), abis.clone());
                            ctx.fn_extern_abi_kinds.insert(c_symbol.clone(), abis);
                        }
                        // Derive return ABI from explicit cstr return type.
                        if matches!(method_def.return_type.node, ast::Type::Primitive(ast::PrimitiveType::CStr)) {
                            ctx.fn_return_abis.insert(mangled.clone(), crate::ir::abi::AbiKind::CStr);
                            ctx.fn_return_abis.insert(c_symbol.clone(), crate::ir::abi::AbiKind::CStr);
                        }
                    }
                }
            }
        }
    }

    // Generic `equip [T] X[T] with Drop:` — upgrade Drop metadata on every
    // monomorphized instance of X. The non-generic Drop scan early in
    // lower_module (line ~230) only handles concrete types because mono'd
    // instances aren't yet registered there. By this point all mono'd
    // structs/enums are in the type_registry, so we can iterate the
    // matching instances and apply Resource + Custom("<Mangled>__drop")
    // metadata per instance. The actual `<Mangled>__drop` body gets
    // emitted later by `lower_generic_equip_methods_with_defaults` walking
    // the same equip block — this just makes sure the metadata points at
    // it so the auto-drop machinery dispatches correctly.
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            let trait_name_str = match equip.trait_.as_ref().map(|t| &t.trait_name.node) {
                Some(ast::Type::Named { name, .. }) => name.node.as_str(),
                _ => continue,
            };
            if trait_name_str != "Drop" {
                continue;
            }
            // Generic if the equip carries generic params or the equipped type
            // has unresolved generic args.
            let is_generic = equip.generic_params.as_ref().map_or(false, |gp| !gp.node.params.is_empty())
                || matches!(&equip.type_.node, ast::Type::Named { generic_args, .. } if !generic_args.is_empty());
            if !is_generic {
                continue;
            }
            let base_name = match &equip.type_.node {
                ast::Type::Named { name, .. } => name.node.clone(),
                _ => continue,
            };
            let prefix = format!("{base_name}__");
            let matching: Vec<String> = ctx.type_registry
                .all_type_def_names()
                .filter(|n| n.starts_with(&prefix))
                .cloned()
                .collect();
            for mangled in matching {
                if let Some(td) = ctx.type_registry.get_type_def_mut(&mangled) {
                    td.metadata.copy_semantics = CopySemantics::Resource;
                    td.metadata.drop_strategy = DropStrategy::Custom(format!("{mangled}__drop"));
                }
            }
        }
    }

    // Register monomorphized equip method signatures (including default trait methods)
    generic_collector.register_equip_sigs_with_defaults(
        &mut ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs, &mut ctx.fn_param_abis,
        &mut ctx.fn_param_ownerships, Some(ast_module));

    // Register signatures for per-call-site method instances (method-level-generic
    // equip methods). Each one becomes a free-function-shaped symbol in fn_sigs,
    // with the self pointer as param 0 and merged equip+method substitutions.
    generic_collector.register_method_instance_sigs(
        &mut ctx.type_mapper, &mut ctx.type_registry, &mut ctx.fn_sigs,
        &mut ctx.fn_param_ownerships, &mut ctx.fn_param_abis);

    // Register built-in method signatures for Option/Result instantiations.
    // These methods are inlined by the C backend (not real functions), but
    // fn_sigs must know about them so the lowering creates properly-typed locals.
    register_builtin_enum_method_sigs(&mut ctx, &generic_collector);
    // Register built-in collection method signatures (Vector, Dict, HashMap, etc.)
    register_collection_method_sigs(&mut ctx, &generic_collector);

    // Builtin method signatures are resolved on-the-fly from the BuiltinTypeProtocol
    // table in builtins.rs via ctx.resolve_builtin_method_return_type() when fn_sigs
    // doesn't have an entry. Pre-populating fn_sigs at startup is deferred until the
    // protocol return types are fully verified (Phase 3b). Runtime callees are populated
    // here for the LIR backend.
    ctx.register_builtin_runtime_callees();

    // P2.5: Register trait equip method signatures
    traits::register_trait_equip_sigs(&mut ctx, &trait_info, ast_module);

    // Register fn_sigs for trait equip blocks with unregistered traits
    // (built-in traits like From, Default, Equatable, Displayable, etc.)
    traits::register_unregistered_trait_equip_sigs(&mut ctx, &trait_info, ast_module);

    // Populate gir_equip_methods: walk all equip blocks and mark methods with GIR-lowered
    // bodies (Block or Expression). This determines caller-side pass-by-pointer for method calls.
    populate_gir_equip_methods(&mut ctx, ast_module, &generic_collector);

    // Pre-scan for trivial getter methods (clone elision candidates).
    // Must run after type registration so is_resource_type works.
    populate_trivial_getter_methods(&mut ctx, ast_module);

    // Patch fn_sigs for trivial getters: override return type from T to Ptr(T).
    // The pre-scan at line ~777 registered these with the original type; update them
    // so callers see the borrow return type.
    for name in ctx.trivial_getter_methods.iter().cloned().collect::<Vec<_>>() {
        if let Some((params, ret)) = ctx.fn_sigs.get(&name).cloned() {
            if !matches!(ctx.type_registry.get(ret), Some(GirType::Ptr(_))) {
                let ptr_ret = ctx.register_ptr_type(ret);
                ctx.fn_sigs.insert(name, (params, ptr_ret));
            }
        }
    }

    // Re-scan monomorphized enum variants: trait sig registration (above) may create new
    // generic enum instantiations (e.g., Option__Color via map_ast_type_mut) whose variants
    // weren't in the type_registry during the initial enum_variants scan. Re-running here
    // ensures `case Some(_)` and `case None:` pattern matching resolves correctly.
    for type_def in ctx.type_registry.type_defs() {
        if let TypeDefKind::Enum(ref e) = type_def.kind {
            if type_def.name.contains("__") {
                for variant in &e.variants {
                    ctx.enum_variants
                        .entry(variant.name.clone())
                        .or_insert_with(|| (type_def.name.clone(), variant.name.clone()));
                }
            }
        }
    }

    // Register runtime built-in method signatures (Str, uint8_t, primitive statics)
    register_runtime_method_sigs(&mut ctx);

    *pass_times.entry("register_mono_and_trait_sigs").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Pre-register module-level static variables so functions can reference them.
    for item in &ast_module.items {
        if let Item::StaticDecl(decl) = &item.node {
            ctx.global_names.insert(decl.name.node.clone());
            // Store the type name for type inference on GlobalRef operands.
            // For generic types like Mutex[int] store "Mutex__int64_t" (not just "Mutex")
            // so that infer_type_name_from_operand_full can dispatch correctly.
            let type_name_str = match &decl.type_.node {
                ast::Type::Named { name: type_name, generic_args } => {
                    if generic_args.is_empty() {
                        Some(type_name.node.clone())
                    } else {
                        Some(crate::ir::lowering::types::mangle_generic_name(&type_name.node, generic_args))
                    }
                }
                ast::Type::Primitive(p) => Some(match p {
                    ast::PrimitiveType::Int | ast::PrimitiveType::Int64 => "int",
                    ast::PrimitiveType::Float | ast::PrimitiveType::Float64 => "float",
                    ast::PrimitiveType::Bool => "bool",
                    ast::PrimitiveType::StringType => "str",
                    ast::PrimitiveType::Int8 => "i8",
                    ast::PrimitiveType::Int16 => "i16",
                    ast::PrimitiveType::Int32 => "i32",
                    ast::PrimitiveType::Uint8 => "u8",
                    ast::PrimitiveType::Uint16 => "u16",
                    ast::PrimitiveType::Uint32 => "u32",
                    ast::PrimitiveType::Uint | ast::PrimitiveType::Uint64 => "u64",
                    ast::PrimitiveType::Float32 => "f32",
                    _ => "int",
                }.into()),
                _ => None,
            };
            if let Some(tn) = type_name_str {
                ctx.global_type_names.insert(decl.name.node.clone(), tn);
            }
        }
    }

    // R34 Track A: whole-program mutation prescan. MUST run BEFORE the static-
    // lowering loop below — statics lower before functions, so populating the
    // set inside the static loop would leave it EMPTY when `lower_static_decl`
    // consumes it (→ a mutated static wrongly emitted as an immutable rodata
    // view → UB). Scans EVERY function-like body for direct mutations of a
    // static (mutating-method receiver, assign/compound-assign target root,
    // `&STATIC`, `!STATIC` — incl. index/field projections) and records the
    // static's original `DefId`.
    ctx.mutated_static_defs = scan_mutated_statics(ctx.analysis, &ast_module.items);

    // Lower module-level static declarations → Globals.
    // Skip duplicate globals (same name from different modules merged into one AST).
    let mut seen_globals: rustc_hash::FxHashSet<String> = rustc_hash::FxHashSet::default();
    for item in &ast_module.items {
        if let Item::StaticDecl(decl) = &item.node {
            if !seen_globals.insert(decl.name.node.clone()) { continue; }
            lower_static_decl(&mut ctx, &mut module, decl);
        }
    }

    *pass_times.entry("lower_globals").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Lower all non-generic functions
    for item in &ast_module.items {
        if let Item::Function(func) = &item.node {
            if func.generic_params.is_some() {
                continue; // Generic functions are lowered as monomorphized instances
            }
            // Phase 5: use the module-mangled name if this function came from a
            // non-entry module (identified by its definition span).
            let name_override = module_fn_manglings
                .get(&func.name.span.start)
                .map(|s| s.as_str());
            // Register as GIR-lowered so call_tracked upgrades string returns
            let fn_name = name_override.unwrap_or(func.name.node.as_str());
            ctx.gir_equip_methods.insert(fn_name.to_string());
            lower_function(&mut ctx, &mut module, func, name_override);
        }
    }

    // Bug B: lower the synthetic `__gg_static_init_<name>()` functions built
    // during static-decl lowering. Placed HERE (after the non-generic function
    // loop) — not in the globals loop — so the type registry + monomorph
    // collection have run. The synthetic fns are non-generic (concrete element
    // types), so they need no monomorph instantiation.
    let synth_static_fns: Vec<_> = std::mem::take(&mut ctx.synthetic_static_init_fns);
    for f in &synth_static_fns {
        ctx.gir_equip_methods.insert(f.name.node.clone());
        lower_function(&mut ctx, &mut module, f, None);
    }

    *pass_times.entry("lower_functions").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Lower monomorphized generic function instances
    for (base_name, type_args, mangled_name) in generic_collector.function_instances() {
        if let Some(template) = generic_collector.get_fn_template(base_name) {
            ctx.gir_equip_methods.insert(mangled_name.to_string());
            let op_bindings = generic_collector.meta_op_bindings_for(mangled_name);
            functions::lower_generic_function(
                &mut ctx,
                &mut module,
                template,
                type_args,
                mangled_name,
                op_bindings,
            );
        }
    }

    *pass_times.entry("lower_generic_functions").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Lower non-generic equip blocks as functions
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            if equip.generic_params.is_some() || equip.trait_.is_some() {
                continue;
            }

            if let Some(type_name) = types::equip_target_name(&equip.type_.node) {
                for method in &equip.items {
                    // Method-level generics go through per-call-site mono
                    // (lower_method_instance). Skip here so we don't emit a
                    // stub body with unsubstituted generic params — that
                    // silently drops calls like `h.write_int(...)` inside
                    // the unresolvable template body.
                    if method.node.generic_params.is_some() {
                        continue;
                    }
                    functions::lower_equip_method(
                        &mut ctx,
                        &mut module,
                        &method.node,
                        &type_name,
                        &equip.type_.node,
                    );
                }
            }
        }
    }

    *pass_times.entry("lower_equip_methods").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
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

    *pass_times.entry("lower_generic_equip_methods").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Lower per-call-site method instances (method-level-generic equip methods).
    // Each MethodInstance captures the equip + method + concrete type args, and
    // lowers to a free-function-shaped symbol that the MethodCall dispatch path
    // targets directly (see src/ir/lowering/exprs/methods.rs).
    //
    // Falls back to default-trait-method bodies (e.g. `bool any[F](&self, F pred)`
    // declared on `trait Iterator[T]:`) when the equip block doesn't override —
    // mirrors `register_method_instance_sigs`'s lookup so sig and body stay in sync.
    for mi in generic_collector.method_instances().to_vec() {
        if let Some(equip_blocks) = generic_collector.get_equip_templates(&mi.equip_base) {
            let equip_blocks = equip_blocks.clone();
            let mut emitted = false;
            'method_search: for equip in &equip_blocks {
                for method in &equip.items {
                    if method.node.name.node == mi.method_name
                        && method.node.generic_params.as_ref()
                            .map(|gp| gp.node.params.len() == mi.method_type_args.len())
                            .unwrap_or(false)
                    {
                        ctx.gir_equip_methods.insert(mi.mangled_symbol.clone());
                        functions::lower_method_instance(
                            &mut ctx,
                            &mut module,
                            equip,
                            &method.node,
                            &mi.equip_type_args,
                            &mi.method_type_args,
                            &mi.mangled_type,
                            &mi.mangled_symbol,
                        );
                        emitted = true;
                        break 'method_search;
                    }
                }
            }
            if !emitted {
                if let Some(default_m) = generic_collector.find_default_trait_method(
                    &equip_blocks, &mi.method_name, mi.method_type_args.len(),
                ) {
                    if let Some(equip) = equip_blocks.first() {
                        ctx.gir_equip_methods.insert(mi.mangled_symbol.clone());
                        functions::lower_method_instance(
                            &mut ctx,
                            &mut module,
                            equip,
                            &default_m,
                            &mi.equip_type_args,
                            &mi.method_type_args,
                            &mi.mangled_type,
                            &mi.mangled_symbol,
                        );
                    }
                }
            }
        }
    }

    *pass_times.entry("lower_method_instances").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Lower test items: each test becomes a void function, then generate test runner main.
    // In test_mode (gg test), run even when a main() exists — the C backend will skip it.
    let has_tests = ast_module.items.iter().any(|item| matches!(&item.node, Item::Test(_)));
    let has_benches = ast_module.items.iter().any(|item| matches!(&item.node, Item::Bench(_)));
    let has_main = ast_module.items.iter().any(|item| matches!(&item.node, Item::Function(f) if f.name.node == "main"));
    if options.bench_mode && has_benches {
        lower_bench_items(&mut ctx, &mut module, ast_module, options);
        module.runtime.is_test_module = true;
    } else if has_tests && (options.test_mode || !has_main) {
        lower_test_items(&mut ctx, &mut module, ast_module, options);
        // Mark module as a test module so the C backend always emits a test runner,
        // even when all tests were filtered out (e.g., --tag X --exclude-tag X → 0 tests).
        module.runtime.is_test_module = true;
    }

    *pass_times.entry("lower_tests_benches").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // P2.5: Lower trait equip methods and emit vtable globals
    traits::lower_trait_equip_methods(&mut ctx, &mut module, &trait_info, ast_module);
    traits::emit_vtable_globals(&mut ctx, &mut module, &trait_info, ast_module);

    // Lower trait equip blocks with unregistered traits (From, Default, Equatable, etc.)
    traits::lower_unregistered_trait_equip_methods(&mut ctx, &mut module, &trait_info, ast_module);
    *pass_times.entry("lower_traits").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // P2.4: Emit lifted closure call functions
    let closures = std::mem::take(&mut ctx.closures);
    for lifted in &closures.lifted {
        let func = closures::emit_closure_call_function(&mut ctx, lifted);
        module.functions.push(func);
    }

    // P2.4b: Emit closure spawn wrapper functions (generated by lower_closure_spawn).
    for func in std::mem::take(&mut ctx.spawn.wrapper_fns) {
        module.functions.push(func);
    }
    *pass_times.entry("lower_closures_spawn_wrappers").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // P2.4c: Process deferred shared-async variants via GIR-to-GIR transform.
    // Source functions are already lowered into module.functions at this point.
    // Collect inner spawn rewrites for processing after all variants are generated.
    let mut all_inner_rewrites = Vec::new();
    for pending in std::mem::take(&mut ctx.shared.pending_variants) {
        let source_fn = module.functions.iter()
            .find(|f| f.name == pending.source_fn_name)
            .unwrap_or_else(|| panic!(
                "shared-async variant '{}' references source function '{}' which was not lowered",
                pending.variant_name, pending.source_fn_name
            ))
            .clone();
        let result = crate::ir::transforms::shared_async::inject_shared_token_management(
            &source_fn,
            &pending.variant_name,
            &pending.shared_args,
            &mut ctx.type_registry,
            &ctx.yield_point_fns,
        );
        module.functions.push(result.func);
        all_inner_rewrites.extend(result.inner_rewrites);
    }

    // P2.4d: Generate wrapper functions for inner shared spawns.
    // These are callees spawned from within a shared-async variant that need
    // their own shared wrappers to receive the Shared[Mutex[T]] parameter.
    for rewrite in &all_inner_rewrites {
        // Skip if wrapper already exists
        if module.functions.iter().any(|f| f.name == rewrite.wrapper_name) {
            continue;
        }

        let source_fn = module.functions.iter()
            .find(|f| f.name == rewrite.callee_name)
            .cloned();

        if let Some(source_fn) = source_fn {
            if rewrite.callee_has_awaits {
                // Async inner callee — use the full GIR transform
                let inner_result = crate::ir::transforms::shared_async::inject_shared_token_management(
                    &source_fn,
                    &rewrite.wrapper_name,
                    &rewrite.shared_args,
                    &mut ctx.type_registry,
                    &ctx.yield_point_fns,
                );
                module.functions.push(inner_result.func);
                // Note: we don't recurse further for simplicity.
                // Deep nesting (3+ levels) would need iteration.
            } else {
                // Sync inner callee — build a token wrapper
                use crate::ir::lowering::exprs::spawn::SharedSpawnArg;
                use crate::ir::lowering::context::SharedLocalKind;
                let spawn_args: Vec<SharedSpawnArg> = rewrite.shared_args.iter().map(|sa| {
                    SharedSpawnArg {
                        arg_index: sa.arg_index,
                        kind: SharedLocalKind::Mutex,
                        inner_type: sa.inner_type,
                        wrapper_type: sa.wrapper_type,
                        is_mutable: sa.is_mutable,
                        decl_order: 0,
                    }
                }).collect();
                let wrapper_fn = crate::ir::lowering::exprs::spawn::build_shared_token_wrapper(
                    &mut ctx,
                    &rewrite.wrapper_name,
                    &rewrite.callee_name,
                    &rewrite.callee_param_types,
                    &spawn_args,
                    rewrite.callee_return_type,
                );
                module.functions.push(wrapper_fn);
            }
        }

        // Register the wrapper in spawn infrastructure so the C backend
        // generates spawn/await helpers for it.
        let wrapper_param_types: Vec<TypeId> = rewrite.callee_param_types.iter().enumerate()
            .map(|(i, &t)| {
                rewrite.shared_args.iter()
                    .find(|sa| sa.arg_index == i)
                    .map(|sa| sa.wrapper_type)
                    .unwrap_or(t)
            })
            .collect();
        ctx.fn_sigs.insert(rewrite.wrapper_name.clone(), (wrapper_param_types, rewrite.callee_return_type));
        ctx.spawn.fn_names.insert(rewrite.wrapper_name.clone(), true);
        let param_names: Vec<String> = (0..rewrite.callee_param_types.len())
            .map(|i| format!("__p{i}"))
            .collect();
        ctx.fn_param_names.insert(rewrite.wrapper_name.clone(), param_names);

        // Register task type for the wrapper
        let ret_c = ctx.type_name_for_id(rewrite.callee_return_type)
            .unwrap_or("int64_t")
            .to_string();
        let task_name = if rewrite.callee_return_type == UNIT_TYPE {
            "Task__void".to_string()
        } else {
            format!("Task__{ret_c}")
        };
        if let Some(task_type) = ctx.type_mapper.lookup_named(&task_name) {
            ctx.spawn.register_task_type_fn(task_type, rewrite.wrapper_name.clone());
        }
    }

    *pass_times.entry("shared_async_transform").or_default() += __pass_t.elapsed();

    let __pass_t = Instant::now();
    // Move type_registry back to module for validation
    module.type_registry = std::mem::take(&mut ctx.type_registry);

    // Note: upgrade_types_from_fields runs at module start for types registered during
    // types for the LIR backend (drop/clone function generation). The GIR functions
    // don't need fixup because ensure_option/result_type_registered now upgrades
    // types at registration time (see context.rs).

    // Auto-register all CallExtern targets as externs if not already known.
    // This handles runtime functions (gorget_throw, gorget_array_new, etc.)
    // without needing to enumerate each one manually.
    auto_register_externs(&mut module);
    *pass_times.entry("auto_register_externs").or_default() += __pass_t.elapsed();

    // Tier 1c — TypeDef metadata coherence at registration. Promoted
    // to fatal 2026-05-11 after the carve-out removal landed and the
    // Cluster 1 / Snag #24 burn-down brought all 6 migration sites
    // to coherent state. Any TypeDef registered with metadata less
    // restrictive than its fields/variants demand halts the build.
    //
    // See `docs/devbook/25-structural-guards.md` §Tier 1c.
    time_pass!(pass_times, "validate_type_metadata_coherence", {
        let warnings = crate::ir::validate::validate_type_metadata_coherence(&module);
        if !warnings.is_empty() {
            eprintln!("[type-metadata-coherence] {} violation(s):", warnings.len());
            for w in warnings.iter().take(20) {
                eprintln!("  {}", w);
            }
            panic!(
                "Tier 1c type-metadata coherence violation: {} TypeDef(s) registered with \
                 (drop_strategy, copy_semantics) less restrictive than their fields demand. \
                 First: {}",
                warnings.len(),
                warnings.first().map(|w| w.to_string()).unwrap_or_default(),
            );
        }
    });

    let __pass_t = Instant::now();
    // Validate the resulting module
    let errors = crate::ir::validate::validate(&module);
    if !errors.is_empty() {
        // Enum field count mismatches from cross-module imports are non-fatal —
        // the TypeDef registration for imported enums can disagree with the
        // actual EnumInit operand counts when Vector[T] fields are involved.
        // The C codegen handles these correctly regardless.
        let (fatal, warnings): (Vec<_>, Vec<_>) = errors.into_iter().partition(|e| {
            !matches!(e.kind, crate::ir::validate::ValidationErrorKind::EnumFieldCountMismatch { .. })
        });
        if !warnings.is_empty() {
            eprintln!("GIR validation warnings ({} enum field-count mismatches suppressed)", warnings.len());
        }
        if !fatal.is_empty() {
            eprintln!("GIR validation errors:");
            for err in &fatal {
                eprintln!("  {}", err);
            }
            panic!("GIR module failed validation ({} errors)", fatal.len());
        }
    }
    *pass_times.entry("validate_module").or_default() += __pass_t.elapsed();

    // Phase C, Stage C4 + extensions: resource-site validators
    // (Assign / Call/CallExtern args / IndexLoad / EnumFieldLoad /
    // FieldLoad). All five share the unified `validate_read` rule and
    // produce `ResourceMoveWarning`; pre-D5 collapse they were five
    // back-to-back full-module walks (~30% of `gir_lower` on LW).
    //
    // The consolidated walker in `validate_resource_sites_all` performs
    // ONE walk per function and partitions warnings into class buckets
    // so the existing fatal diagnostics — including class-specific
    // labels and per-class violation counts — stay byte-identical with
    // the legacy split path. See `docs/devbook/25-structural-guards.md`
    // §Phase C / §6.4 for the contract.
    time_pass!(pass_times, "validate_resource_sites_all", {
        let findings = crate::ir::validate::validate_resource_sites_all(&module);
        // Each class is its own fatal gate (was its own `time_pass!`
        // block pre-collapse). Order matters for byte-identical
        // diagnostic output: emit in the original five-block order so
        // a panic message at any earlier class matches the legacy
        // behaviour exactly.
        if !findings.assign.is_empty() {
            eprintln!("[resource-moves] {} violation(s):", findings.assign.len());
            for w in &findings.assign {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed resource-move validation ({} violation(s))", findings.assign.len());
        }
        if !findings.call_args.is_empty() {
            eprintln!("[resource-call-args] {} violation(s):", findings.call_args.len());
            for w in &findings.call_args {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed resource call-arg validation ({} violation(s))", findings.call_args.len());
        }
        if !findings.index_reads.is_empty() {
            eprintln!("[resource-index-reads] {} violation(s):", findings.index_reads.len());
            for w in &findings.index_reads {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed resource index-read validation ({} violation(s))", findings.index_reads.len());
        }
        if !findings.enum_reads.is_empty() {
            eprintln!("[resource-enum-reads] {} violation(s):", findings.enum_reads.len());
            for w in &findings.enum_reads {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed resource enum-read validation ({} violation(s))", findings.enum_reads.len());
        }
        if !findings.field_reads.is_empty() {
            eprintln!("[resource-field-reads] {} violation(s):", findings.field_reads.len());
            for w in &findings.field_reads {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed resource field-read validation ({} violation(s))", findings.field_reads.len());
        }
    });

    // Phase C extension: opt-in residual logger.
    // All four read-site classes (Call/CallExtern args, IndexLoad,
    // EnumFieldLoad, FieldLoad) are now promoted to fatal above. This
    // GG_VALIDATE_RESOURCE_READS-gated path remains as diagnostic
    // scaffolding for any new read-site class added in the future.
    if let Ok(log_path) = std::env::var("GG_VALIDATE_RESOURCE_READS") {
        if !log_path.is_empty() {
            let reads = crate::ir::validate::validate_resource_reads(&module);
            if !reads.is_empty() {
                use crate::ir::validate::ResourceMoveWarningKind as K;
                use std::io::Write;
                let mut field_n = 0usize;
                let mut index_n = 0usize;
                let mut enum_n = 0usize;
                let mut arg_n = 0usize;
                for w in &reads {
                    match &w.kind {
                        K::ShallowCopyOfResourceField { .. } => field_n += 1,
                        K::ShallowReadOfResourceElement { .. } => index_n += 1,
                        K::ShallowCopyOfEnumPayload { .. } => enum_n += 1,
                        K::ShallowCopyOfResourceArg { .. } => arg_n += 1,
                        _ => {}
                    }
                }
                if let Ok(mut f) = std::fs::OpenOptions::new()
                    .create(true).append(true).open(&log_path)
                {
                    let _ = writeln!(f, "[resource-reads] module total={} field={} index={} enum={} arg={}",
                        reads.len(), field_n, index_n, enum_n, arg_n);
                    for w in &reads {
                        let _ = writeln!(f, "  {}", w);
                    }
                }
            }
        }
    }

    // Tier 1b — Move follow-through validator. See
    // `docs/devbook/25-structural-guards.md` §Tier 1b. Promoted from
    // env-gated diagnostic to unconditional fatal: the f-string interp
    // segment migration (commit `1d3ccd5b`) closed the dominant
    // violation class, and the c_emit_comparison full-fixture sweep
    // (1066 fixtures, every fixture built end-to-end via `gg build
    // --emit-c-lir`) reports 0 violations across 0 modules — the class
    // is provably closed. Promoting to fatal mirrors the Phase C
    // resource-moves block above and locks the invariant for any future
    // lowering site that emits Move-mode without follow-through.
    // Invariant: `Inst::Assign { mode: Move }` of a drop-registered
    // source must be followed by a `MoveZero` of the source in the same
    // basic block before any subsequent `Drop` / `DropIfAlive` of the
    // source — snag #19 / #23 lock-in.
    time_pass!(pass_times, "validate_move_follow_through", {
        let mft_warnings = crate::ir::validate::validate_move_follow_through(&module);
        if !mft_warnings.is_empty() {
            eprintln!("[move-follow-through] {} violation(s):", mft_warnings.len());
            for w in &mft_warnings {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed move-follow-through validation ({} violation(s))", mft_warnings.len());
        }
    });

    // Tier 2c — drop-tracking pre-rebind correctness (snag #23 class lock).
    // Every call to a heap-allocating consumer (currently `__gorget_box_alloc_<T>`)
    // with a Copy/Move source operand must be followed by a `MoveZero`
    // of the source in the same basic block before any subsequent
    // `Drop`/`DropIfAlive` of the source. Snag #23's bug shipped a fix
    // at `4ebefe44`; this validator locks in the rule so a future
    // shallow-copy heap-allocating consumer can't ship without the
    // paired `move_zero_and_mark`. See
    // `docs/devbook/25-structural-guards.md` Tier 2c.
    //
    // Recognition is typed: `Module::heap_alloc_consumer_externs` is
    // populated at the writer site (3 Box.new lowering paths) and read
    // by the validator. No `name.starts_with(...)` survives in the
    // validator's classifier — adding a new shallow-copy heap-allocating
    // consumer at any future writer site is a single
    // `module.heap_alloc_consumer_externs.insert(...)` call. Per
    // CLAUDE.md "No name matching".
    //
    // Set GG_VALIDATE_DROP_PRE_REBIND=/path/to/log to also write a
    // structured report (function counts, sample violations) without
    // suppressing the panic — mirrors `GG_VALIDATE_CONSUME_SITES` /
    // `GG_VALIDATE_MOVE_FOLLOW_THROUGH`.
    module.heap_alloc_consumer_externs = std::mem::take(&mut ctx.heap_alloc_consumer_externs);
    time_pass!(pass_times, "validate_drop_pre_rebind_and_null_to_opt", {
        let dpr_warnings = crate::ir::validate::validate_drop_pre_rebind(&module);
        if !dpr_warnings.is_empty() {
            // Optional structured log for investigation. Sweep-of-record
            // pattern: per-function counts + sample violations, mirroring
            // the Tier 2a / Tier 1b env-gate writers.
            if let Ok(log_path) = std::env::var("GG_VALIDATE_DROP_PRE_REBIND") {
                if !log_path.is_empty() {
                    use std::io::Write;
                    use rustc_hash::FxHashMap;
                    let mut by_function: FxHashMap<String, usize> = FxHashMap::default();
                    let mut by_callee: FxHashMap<String, usize> = FxHashMap::default();
                    for w in &dpr_warnings {
                        *by_function.entry(w.function.clone()).or_insert(0) += 1;
                        *by_callee.entry(w.callee.clone()).or_insert(0) += 1;
                    }
                    if let Ok(mut f) = std::fs::OpenOptions::new()
                        .create(true).append(true).open(&log_path)
                    {
                        let module_name = module.source_filename.as_deref().unwrap_or("<unknown>");
                        let _ = writeln!(f, "[drop-pre-rebind] module={} total={}",
                            module_name, dpr_warnings.len());
                        let mut bc: Vec<_> = by_callee.iter().collect();
                        bc.sort_by(|a, b| b.1.cmp(a.1));
                        for (callee, n) in &bc {
                            let _ = writeln!(f, "  callee {} = {}", callee, n);
                        }
                        let mut bf: Vec<_> = by_function.iter().collect();
                        bf.sort_by(|a, b| b.1.cmp(a.1));
                        for (fname, n) in bf.iter().take(20) {
                            let _ = writeln!(f, "  fn @{} = {}", fname, n);
                        }
                        let _ = writeln!(f, "  -- sample warnings (first 200) --");
                        for w in dpr_warnings.iter().take(200) {
                            let _ = writeln!(f, "  {}", w);
                        }
                    }
                }
            }
            eprintln!("[drop-pre-rebind] {} violation(s):", dpr_warnings.len());
            for w in &dpr_warnings {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed drop-pre-rebind validation ({} violation(s)). \
                    Run with GG_VALIDATE_DROP_PRE_REBIND=/tmp/dpr.log for full report.",
                   dpr_warnings.len());
        }

        // Snag #32 family — None-literal materialisation at writer boundaries.
        // Fatal: any `Inst::Assign { value: Constant::Null }` into an
        // Option/Result-typed slot is a writer-site bug. The IR-lowering
        // chokepoint (`coerce_null_to_option_none` in
        // `emit_field_store_with_cleanup`, plus
        // `materialise_none_for_expected_type` on the bare-NoneLiteral arm)
        // must rewrite `Null` to a tagged `enum_init … None` before
        // emission. See `validate_no_null_assign_to_option_slot`.
        let null_to_opt = crate::ir::validate::validate_no_null_assign_to_option_slot(&module);
        if !null_to_opt.is_empty() {
            eprintln!("[null-assign-to-option] {} violation(s):", null_to_opt.len());
            for w in &null_to_opt {
                eprintln!("  {}", w);
            }
            panic!("GIR module failed None-literal-materialisation validation ({} violation(s)). \
                    Snag #32 family — writer must route through `coerce_null_to_option_none`.",
                   null_to_opt.len());
        }
    });

    // Tier 2a Phase 2A — post-lowering ownership inference.
    //
    // Walks every function and tags `Owned` / `FreshOwned` on
    // producer destinations whose ownership is currently `Untracked`
    // (default) but whose IR shape unambiguously identifies a fresh
    // owned resource:
    //
    //   * `HeapAlloc` / `HeapAllocArray` → FreshOwned
    //   * `Call` / `CallExtern` to a typed clone-or-fresh callee
    //     (Phase 2E's `clone_fns` set + `RuntimeFn::returns_fresh`) →
    //     FreshOwned
    //   * `Call` (internal) returning a droppable non-pointer type →
    //     Owned (functions return owned by value; non-pointer return
    //     types don't alias into inputs)
    //   * `EnumFieldLoad` / `FieldLoad` followed in the same block by
    //     a `MoveZero` of the base → Owned (extracted slot owns the
    //     heap; the base is dead)
    //
    // This complements `LoweringContext::set_owned`'s per-call-site
    // setters: 200+ lowering paths call `builder.call*` or
    // `builder.enum_field_load_move` directly without going through the
    // `*_tracked` helpers — running structural inference once over the
    // module catches all of them at one site.
    //
    // The pass NEVER overwrites a non-Untracked tag, so Branch A/C/D's
    // borrow / shared-heap shapes from `lower_var_decl` (and the
    // CoW alias propagation) are preserved.
    //
    // See `src/ir/tag_ownership.rs` for the rules + the rationale.
    time_pass!(pass_times, "tag_ownership_infer_fresh_owned", {
        crate::ir::tag_ownership::infer_fresh_owned(&mut module);
    });

    // Populate Module::consume_externs. Two sources:
    //   (1) Direct registrations from writer sites (lower_dict_literal etc.)
    //       that emit mangled mutator extern calls — see ctx.consume_externs.
    //   (2) Derivation from `fn_param_ownerships` — any registered function
    //       with at least one `Ownership::Move` parameter is consume-shape
    //       by definition (covers Vector__push, Set__add, etc. that go
    //       through register_collection_method_sigs).
    //
    // This is the typed-metadata bridge between the IR-stage mangled name
    // (`Dict__K__V__put`) and the runtime contract that name-matched
    // allowlists miss. See `Module::consume_externs` doc for full rationale.
    // Must run BEFORE `validate_consume_sites` so the classifier sees the
    // registry.
    time_pass!(pass_times, "populate_consume_externs", {
        use crate::parser::ast::Ownership;
        module.consume_externs = std::mem::take(&mut ctx.consume_externs);
        for (name, ownerships) in &ctx.fn_param_ownerships {
            if ownerships.iter().any(|o| matches!(o, Ownership::Move)) {
                module.consume_externs.insert(name.clone());
            }
        }
    });

    // Tier 2a: consume-site discipline (CoW write-side) — FATAL.
    // Promoted from env-gated warning (Phase 1) to unconditional fatal
    // after Phase 2A/2B/2C drove the violation count to zero across all
    // 1068 integration fixtures (2026-05-08).
    //
    // Any EnumInit / StructInit / CollectionMutator / HeapAlloc field that
    // consumes a local with Borrowed / Untracked / OwnedLive ownership is
    // a CoW violation: the callee would own a non-owning alias or a still-live
    // owned value, producing either a double-free or a leak. The lowering
    // must clone or move before reaching a consume site.
    //
    // Set GG_VALIDATE_CONSUME_SITES=/path/to/log to also write a structured
    // report (class counts, sample violations) without suppressing the panic.
    //
    // See `docs/devbook/25-structural-guards.md` Tier 2a for the full spec.
    time_pass!(pass_times, "validate_consume_sites", {
        let warnings = crate::ir::validate::validate_consume_sites(&module);
        // Tier 2a Phase 3 promoted (2026-05-10): all classes — including
        // `AssignIntoOwnedSlot` (Snag #28's plain `Inst::Assign` shape) —
        // are now fatal after the residual 64 violations were driven to
        // zero across the 1078-fixture sweep. The split that previously
        // held AssignIntoOwnedSlot back as non-fatal pending migration
        // is gone; every consume class halts the build on first
        // violation.
        let fatal_warnings: Vec<_> = warnings.iter().cloned().collect();
        let assign_warnings: Vec<crate::ir::validate::ConsumeSiteWarning> = Vec::new();
        if !fatal_warnings.is_empty() || !assign_warnings.is_empty() {
            use std::io::Write;
            use rustc_hash::FxHashMap;
            let mut by_class: FxHashMap<String, usize> = FxHashMap::default();
            let mut by_class_violation: FxHashMap<(String, String), usize> = FxHashMap::default();
            let mut by_class_type: FxHashMap<(String, String), usize> = FxHashMap::default();
            let mut by_function: FxHashMap<String, usize> = FxHashMap::default();
            for w in fatal_warnings.iter().chain(assign_warnings.iter()) {
                let class_key = match &w.class {
                    crate::ir::validate::ConsumeSiteClass::CollectionMutator { .. }
                        => "CollectionMutator".to_string(),
                    crate::ir::validate::ConsumeSiteClass::EnumInit { .. }
                        => "EnumInit".to_string(),
                    crate::ir::validate::ConsumeSiteClass::StructInit { .. }
                        => "StructInit".to_string(),
                    crate::ir::validate::ConsumeSiteClass::BoxNew
                        => "BoxNew".to_string(),
                    crate::ir::validate::ConsumeSiteClass::CallByValueArg { .. }
                        => "CallByValueArg".to_string(),
                    crate::ir::validate::ConsumeSiteClass::CallExternByValueArg { .. }
                        => "CallExternByValueArg".to_string(),
                    crate::ir::validate::ConsumeSiteClass::AssignIntoOwnedSlot { .. }
                        => "AssignIntoOwnedSlot".to_string(),
                };
                *by_class.entry(class_key.clone()).or_insert(0) += 1;
                *by_class_violation
                    .entry((class_key.clone(), w.violation.to_string()))
                    .or_insert(0) += 1;
                *by_class_type
                    .entry((class_key.clone(), w.source_type_name.clone()))
                    .or_insert(0) += 1;
                *by_function.entry(w.function.clone()).or_insert(0) += 1;
            }
            // Optional structured log for investigation.
            if let Ok(log_path) = std::env::var("GG_VALIDATE_CONSUME_SITES") {
                if !log_path.is_empty() {
                    if let Ok(mut f) = std::fs::OpenOptions::new()
                        .create(true).append(true).open(&log_path)
                    {
                        let module_name = module.source_filename.as_deref().unwrap_or("<unknown>");
                        let _ = writeln!(f, "[consume-sites] module={} total={}",
                            module_name, warnings.len());
                        let mut classes: Vec<_> = by_class.iter().collect();
                        classes.sort_by(|a, b| a.0.cmp(b.0));
                        for (k, v) in &classes {
                            let _ = writeln!(f, "  class {}={}", k, v);
                        }
                        let mut cv: Vec<_> = by_class_violation.iter().collect();
                        cv.sort_by(|a, b| b.1.cmp(a.1));
                        for ((c, viol), n) in &cv {
                            let _ = writeln!(f, "  class+viol {} | {} = {}", c, viol, n);
                        }
                        let mut ct: Vec<_> = by_class_type.iter().collect();
                        ct.sort_by(|a, b| b.1.cmp(a.1));
                        for ((c, ty), n) in ct.iter().take(50) {
                            let _ = writeln!(f, "  class+type {} | {} = {}", c, ty, n);
                        }
                        let mut bf: Vec<_> = by_function.iter().collect();
                        bf.sort_by(|a, b| b.1.cmp(a.1));
                        for (fname, n) in bf.iter().take(20) {
                            let _ = writeln!(f, "  fn @{} = {}", fname, n);
                        }
                        let _ = writeln!(f, "  -- sample warnings (first 200) --");
                        for w in warnings.iter().take(200) {
                            let _ = writeln!(f, "  {}", w);
                        }
                    }
                }
            }
            // Fatal: panic on first violation. AssignIntoOwnedSlot
            // (Snag #28 class) is folded in here as of 2026-05-11 —
            // the Tier 1c Cluster 1 burn-down closed the last
            // untracked-source case (`try_lift_option_ref`'s merge
            // local, fixed by tagging it Owned at construction).
            if let Some(first) = fatal_warnings.first() {
                panic!(
                    "Tier 2a consume-site violation: {} violation(s) in module '{}'. \
                     First: fn @{} bb{} i{} — {} — {}. \
                     Run with GG_VALIDATE_CONSUME_SITES=/tmp/violations.log for full report.",
                    fatal_warnings.len(),
                    module.source_filename.as_deref().unwrap_or("<unknown>"),
                    first.function, first.block.0, first.inst_index,
                    first.class, first.violation,
                );
            }
        }
    });

    // G3: clone-reason census (the materialization-planner foundation).
    // Every compiler-emitted CLONE carries a typed `MaterializeReason` on its
    // `Instruction::Call.reason` — the house env-gate → burn-down → fatal
    // ratchet (mirrors GG_VALIDATE_CONSUME_SITES), now PROMOTED to always-on in
    // debug builds: the burn-down closed (untagged=0 across the fixture corpus
    // and every self-host driver), so a stray untagged clone is a regression the
    // debug build must catch, not an opt-in check. Modes:
    //   debug default                       → strict (panic on first untagged).
    //   release default (env unset)         → off (zero cost).
    //   "1" / "report" / a log path         → print the per-module census, no
    //                                          panic (opt-in even in release).
    //   "strict"                            → force strict (also in release).
    //   "off"                               → disable entirely (debug escape hatch).
    {
        let env = std::env::var("GG_VALIDATE_CLONE_REASONS").ok();
        let env_ref = env.as_deref();
        let strict = matches!(env_ref, Some("strict"))
            || (cfg!(debug_assertions) && env_ref != Some("off"));
        let report = matches!(env_ref, Some(m) if !m.is_empty() && m != "off" && m != "strict");
        if strict || report {
            let census = crate::ir::validate::validate_clone_reasons(&module);
            let module_name = module.source_filename.as_deref().unwrap_or("<unknown>");
            if report {
                eprintln!(
                    "[clone-reasons] module={} total={} tagged={} needs_classification={} untagged={}",
                    module_name, census.total_clones(), census.tagged,
                    census.needs_classification, census.untagged,
                );
                let mut by_reason: Vec<_> = census.by_reason.iter().collect();
                by_reason.sort_by(|a, b| b.1.cmp(a.1));
                for (reason, n) in &by_reason {
                    eprintln!("  reason {} = {}", reason, n);
                }
                for (f, b, i, callee) in census.untagged_sites.iter().take(40) {
                    eprintln!("  UNTAGGED @{} bb{} i{} → {}", f, b, i, callee);
                }
            }
            if strict {
                if let Some((f, b, i, callee)) = census.untagged_sites.first() {
                    panic!(
                        "G3 clone-reason violation: {} untagged clone(s) in module '{}'. \
                         First: fn @{} bb{} i{} → {}. Every compiler-emitted clone must \
                         carry a MaterializeReason (route through emit_clone / call_clone). \
                         Set GG_VALIDATE_CLONE_REASONS=off to bypass (debug), or =report for \
                         the full census.",
                        census.untagged, module_name, f, b, i, callee,
                    );
                }
            }
        }
    }


    let __pass_t = Instant::now();
    // Propagate directive flags to module
    module.runtime.scheduler_mode = ctx.spawn.scheduler_mode;

    // Trace: filename provided by options (derived from source path in main.rs)
    module.runtime.trace_filename = options.trace_filename.clone();

    // Hot-reload: scan for directive + find state type from init() + compute state hash
    setup_hot_reload(ast_module, &mut module, options);

    // Collect runtime metadata (channel/shared/mutex types, spawn info) for C backend
    collect_runtime_metadata(&ctx, &mut module);

    // Thread ParamABI data to the module for C backend consumption
    module.fn_param_abis = ctx.fn_param_abis.clone();

    // Thread extern ABI kinds to the module
    module.fn_extern_abi_kinds = ctx.fn_extern_abi_kinds.clone();
    module.yield_point_fns = ctx.yield_point_fns.clone();
    module.fn_return_abis = ctx.fn_return_abis.clone();
    module.fn_returns_borrowed = ctx.fn_returns_borrowed.clone();

    // Thread purity data to the module
    module.fn_purity = ctx.analysis.fn_purity.clone();

    // Transfer implicit clone warnings and move suggestions
    module.implicit_clone_warnings = ctx.implicit_clone_warnings;
    module.move_suggestions = ctx.move_suggestions;

    // Transfer runtime callees table for LIR backend
    module.runtime_callees = ctx.runtime_callees;
    *pass_times.entry("runtime_metadata_and_thread").or_default() += __pass_t.elapsed();

    // Fold per-function sub-pass timings accumulated inside `lower_function`
    // into the `pass_times` map so they surface in `gg profile` JSON. These
    // ride alongside (do NOT replace) the parent `lower_functions` entry,
    // which still represents the wall-clock cost of the lowering loop
    // including any monomorphized-function or equip-method work threaded
    // through other paths. The `lower_function::*` keys break down the
    // per-call cost of the non-generic top-level function lowering only
    // (called from the loop at line ~1242, accounted under `lower_functions`).
    for (k, v) in ctx.lower_fn_sub_times.drain() {
        *pass_times.entry(k).or_default() += v;
    }

    let lower_total = __lower_module_t0.elapsed();
    // The inner_sum computation must skip the `lower_function::*` sub-pass
    // entries — those are inside the existing `lower_functions` timing,
    // so counting both would double-attribute wall time. Only the top-level
    // pass entries contribute to inner_sum.
    let inner_sum: Duration = pass_times
        .iter()
        .filter(|(k, _)| !k.starts_with("lower_function::"))
        .map(|(_, v)| *v)
        .sum();
    // Residual = the bits between named phases that escape any time_pass!()
    // attribution. Lets the surfaced report sum to wall time without obscuring
    // which sub-pass dominates.
    *pass_times.entry("unaccounted").or_default() += lower_total.saturating_sub(inner_sum);
    module.gir_lower_pass_times = pass_times;

    module
}

/// Register runtime built-in method signatures for Str, uint8_t, and primitive static methods.
fn register_runtime_method_sigs(ctx: &mut LoweringContext) {
    let owned_string_type = ctx.type_mapper.owned_string_type;
    let array_type = ctx.type_mapper.named_types.get("GorgetArray").copied()
        .unwrap_or(UNIT_TYPE);

    // Str methods taking (self) returning various types
    let str_self = vec![owned_string_type];
    let str_str = vec![owned_string_type, owned_string_type];

    // Methods returning typed Vector
    // Ensure Vector__GorgetString is registered early so split() etc. return
    // the correct type. Phase A: protocol-derived metadata.
    let vec_str_type = ctx.ensure_collection_type("Vector__GorgetString");
    let vec_u8_type = ctx.type_mapper.named_types.get("Vector__uint8_t").copied()
        .unwrap_or(array_type);
    let vec_i64_type = ctx.type_mapper.named_types.get("Vector__int64_t").copied()
        .unwrap_or(array_type);
    ctx.fn_sigs.insert("GorgetString__bytes".to_string(), (str_self.clone(), vec_u8_type));
    ctx.fn_sigs.insert("GorgetString__codepoints".to_string(), (str_self.clone(), vec_i64_type));
    ctx.fn_sigs.insert("GorgetString__chars".to_string(), (str_self.clone(), vec_str_type));
    ctx.fn_sigs.insert("GorgetString__split".to_string(), (str_str.clone(), vec_str_type));
    ctx.fn_sigs.insert("GorgetString__splitn".to_string(), (vec![owned_string_type, owned_string_type, I64_TYPE], vec_str_type));
    ctx.fn_sigs.insert("GorgetString__lines".to_string(), (str_self.clone(), vec_str_type));
    // Methods returning Str
    for m in &["trim", "strip", "lstrip", "rstrip", "trim_left", "trim_right", "removeprefix", "removesuffix"] {
        ctx.fn_sigs.insert(format!("GorgetString__{m}"), (str_self.clone(), owned_string_type));
    }
    ctx.fn_sigs.insert("GorgetString__byte_slice".to_string(), (vec![owned_string_type, I64_TYPE, I64_TYPE], owned_string_type));
    ctx.fn_sigs.insert("GorgetString__byte_at".to_string(), (vec![owned_string_type, I64_TYPE], U8_TYPE));
    ctx.fn_sigs.insert("GorgetString__char_at".to_string(), (vec![owned_string_type, I64_TYPE], owned_string_type));
    // Methods returning GorgetString
    for m in &["to_upper", "to_lower"] {
        ctx.fn_sigs.insert(format!("GorgetString__{m}"), (str_self.clone(), owned_string_type));
    }
    ctx.fn_sigs.insert("GorgetString__replace".to_string(), (vec![owned_string_type, owned_string_type, owned_string_type], owned_string_type));
    ctx.fn_sigs.insert("GorgetString__replacen".to_string(), (vec![owned_string_type, owned_string_type, owned_string_type, I64_TYPE], owned_string_type));
    ctx.fn_sigs.insert("GorgetString__repeat".to_string(), (vec![owned_string_type, I64_TYPE], owned_string_type));
    ctx.fn_sigs.insert("GorgetString__pad_left".to_string(), (vec![owned_string_type, I64_TYPE, owned_string_type], owned_string_type));
    ctx.fn_sigs.insert("GorgetString__pad_right".to_string(), (vec![owned_string_type, I64_TYPE, owned_string_type], owned_string_type));
    // Methods returning int64_t
    for m in &["len", "byte_len", "index_of", "count", "find"] {
        let params = if *m == "len" || *m == "byte_len" {
            str_self.clone()
        } else {
            str_str.clone()
        };
        ctx.fn_sigs.insert(format!("GorgetString__{m}"), (params, I64_TYPE));
    }
    // find variants with extra params: find_from(pattern, from), find_ext(pattern, from, reverse)
    ctx.fn_sigs.insert("GorgetString__find_from".to_string(), (vec![owned_string_type, owned_string_type, I64_TYPE], I64_TYPE));
    ctx.fn_sigs.insert("GorgetString__find_ext".to_string(), (vec![owned_string_type, owned_string_type, I64_TYPE, BOOL_TYPE], I64_TYPE));
    // Methods returning bool
    for m in &["contains", "starts_with", "ends_with", "is_empty",
               "is_alpha", "is_digit", "is_alphanumeric", "is_whitespace",
               "is_upper", "is_lower", "is_ascii", "is_hex_digit"] {
        let params = if m.starts_with("is_") && *m != "is_empty" { str_self.clone() }
                     else if *m == "is_empty" { str_self.clone() }
                     else { str_str.clone() };
        ctx.fn_sigs.insert(format!("GorgetString__{m}"), (params, BOOL_TYPE));
    }
    ctx.fn_sigs.insert("GorgetString__eq".to_string(), (str_str.clone(), BOOL_TYPE));
    ctx.fn_sigs.insert("GorgetString__join".to_string(), (vec![owned_string_type, array_type], owned_string_type));

    // uint8_t (byte) method signatures
    for m in &["is_alpha", "is_digit", "is_alphanumeric", "is_whitespace",
               "is_upper", "is_lower", "is_ascii", "is_hex_digit"] {
        ctx.fn_sigs.insert(format!("uint8_t__{m}"), (vec![U8_TYPE], BOOL_TYPE));
    }
    ctx.fn_sigs.insert("uint8_t__to_upper".to_string(), (vec![U8_TYPE], U8_TYPE));
    ctx.fn_sigs.insert("uint8_t__to_lower".to_string(), (vec![U8_TYPE], U8_TYPE));

    // Primitive static method signatures (int.parse, int.default, etc.)
    // IMPORTANT: Use lookup_type_by_name first to find the canonical TypeId for
    // Option types.  Inserting a new GirType::Named into the registry creates a
    // *duplicate* entry that diverges from the TypeId used by collection method
    // return-type inference (e.g. Vector.get → Option[int]), causing type mismatches
    // in downstream codegen.
    let opt_int_type = ctx.type_mapper.named_types.get("Option__int64_t").copied()
        .or_else(|| ctx.lookup_type_by_name("Option__int64_t"))
        .unwrap_or(I64_TYPE);
    let opt_float_type = ctx.type_mapper.named_types.get("Option__double").copied()
        .or_else(|| ctx.lookup_type_by_name("Option__double"))
        .unwrap_or(F64_TYPE);
    let opt_bool_type = ctx.type_mapper.named_types.get("Option__bool").copied()
        .or_else(|| ctx.lookup_type_by_name("Option__bool"))
        .unwrap_or(BOOL_TYPE);
    ctx.fn_sigs.insert("int64_t__parse".to_string(), (vec![owned_string_type], opt_int_type));
    ctx.fn_sigs.insert("int64_t__default".to_string(), (vec![], I64_TYPE));
    ctx.fn_sigs.insert("int64_t__one".to_string(), (vec![], I64_TYPE));
    ctx.fn_sigs.insert("double__parse".to_string(), (vec![owned_string_type], opt_float_type));
    ctx.fn_sigs.insert("double__default".to_string(), (vec![], F64_TYPE));
    ctx.fn_sigs.insert("double__one".to_string(), (vec![], F64_TYPE));
    ctx.fn_sigs.insert("bool__parse".to_string(), (vec![owned_string_type], opt_bool_type));
    ctx.fn_sigs.insert("bool__default".to_string(), (vec![], BOOL_TYPE));
    // Sized integer __default and __one — match the C runtime's
    // `static inline TYPE TYPE__default(void)` / `__one(void)` decls
    // emitted in `emit_types.rs`. Without these registrations,
    // `T.default()` / `T.one()` calls in monomorphised generic
    // bodies (e.g. `Iterator[T]::sum` / `::product` defaults
    // specialised for `T = uint8`) bypass the static-method path
    // (fn_sig miss → I64 return-type fallback), producing a forward
    // decl `int64_t uint8_t__default(void)` that conflicts with the
    // C runtime's `uint8_t uint8_t__default(void)` definition.
    for (gg_name, c_name, type_id) in [
        ("int8",   "int8_t",   I8_TYPE),
        ("int16",  "int16_t",  I16_TYPE),
        ("int32",  "int32_t",  I32_TYPE),
        ("uint8",  "uint8_t",  U8_TYPE),
        ("uint16", "uint16_t", U16_TYPE),
        ("uint32", "uint32_t", U32_TYPE),
        ("uint64", "uint64_t", U64_TYPE),
        ("float32", "float",   F32_TYPE),
    ] {
        let _ = gg_name;
        ctx.fn_sigs.insert(format!("{c_name}__default"), (vec![], type_id));
        ctx.fn_sigs.insert(format!("{c_name}__one"), (vec![], type_id));
    }
}

/// Scan AST for hot-reload directives and configure module runtime metadata.
fn setup_hot_reload(
    ast_module: &ast::Module,
    module: &mut Module,
    options: &LoweringOptions,
) {
    let mut has_hot_reload_directive = false;
    for item in &ast_module.items {
        if let Item::Directive(d) = &item.node {
            if d.name == "hot-reload" { has_hot_reload_directive = true; }
        }
    }
    module.runtime.hot_reload = has_hot_reload_directive || options.hot_reload;
    if !module.runtime.hot_reload {
        return;
    }

    // Find state type from init() return type
    for item in &ast_module.items {
        if let Item::Function(f) = &item.node {
            if f.name.node == "init" {
                if let crate::parser::ast::Type::Named { name, .. } = &f.return_type.node {
                    module.runtime.hot_reload_state_type = Some(name.node.clone());
                }
                break;
            }
        }
    }
    // Compute state hash from the State struct field layout
    if let Some(ref state_type) = module.runtime.hot_reload_state_type.clone() {
        for item in &ast_module.items {
            if let Item::Struct(s) = &item.node {
                if &s.name.node == state_type {
                    let mut layout = String::new();
                    for field in &s.fields {
                        let field_type = format!("{:?}", field.node.type_.node);
                        let field_name = &field.node.name.node;
                        layout.push_str(&format!("{field_type} {field_name};"));
                    }
                    module.runtime.hot_reload_state_hash = fnv1a_hash(&layout);
                    break;
                }
            }
        }
    }
    // Check if reload() function exists
    module.runtime.hot_reload_has_reload_fn = ast_module.items.iter().any(|item| {
        if let Item::Function(f) = &item.node {
            f.name.node == "reload"
        } else { false }
    });
}

/// Collect runtime metadata from type registry and LoweringContext for C backend emission.
///
/// Scans for channel/shared/mutex/rwlock/thread element types, spawned function info,
/// and other runtime flags needed by the C backend's wrapper generation.
fn collect_runtime_metadata(ctx: &LoweringContext, module: &mut Module) {
    let is_template = |elem: &str| {
        elem.split("__").any(|part| !part.is_empty() && part.chars().all(|c| c.is_uppercase()))
    };

    for name in module.type_registry.all_type_def_names() {
        if let Some(elem) = name.strip_prefix("Channel__") {
            if !is_template(elem) && !module.runtime.channel_types.contains(&elem.to_string()) {
                module.runtime.channel_types.push(elem.to_string());
            }
        }
        if let Some(elem) = name.strip_prefix("Shared__") {
            if !is_template(elem) && !module.runtime.shared_types.contains(&elem.to_string()) {
                module.runtime.shared_types.push(elem.to_string());
            }
        }
        if let Some(elem) = name.strip_prefix("Weak__") {
            if !is_template(elem) && !module.runtime.weak_types.contains(&elem.to_string()) {
                module.runtime.weak_types.push(elem.to_string());
            }
        }
        if let Some(elem) = name.strip_prefix("Mutex__") {
            if !is_template(elem) && !module.runtime.mutex_types.contains(&elem.to_string()) {
                module.runtime.mutex_types.push(elem.to_string());
            }
        }
        if name == "TaskGroup" {
            module.runtime.has_task_group = true;
        }
        if let Some(elem) = name.strip_prefix("RWLock__") {
            if !elem.chars().all(|c| c.is_uppercase()) && !module.runtime.rwlock_types.contains(&elem.to_string()) {
                module.runtime.rwlock_types.push(elem.to_string());
            }
        }
        if matches!(name.as_str(), "AtomicInt" | "AtomicBool" | "Barrier" | "CondVar" | "WaitGroup" | "Semaphore" | "OnceFlag")
            || name.starts_with("RWLock__")
            || name.starts_with("ReadGuard__")
            || name.starts_with("WriteGuard__")
        {
            module.runtime.has_sync = true;
        }
        if let Some(elem) = name.strip_prefix("Thread__") {
            if !elem.chars().all(|c| c.is_uppercase()) {
                if !module.runtime.thread_types.contains(&elem.to_string()) {
                    module.runtime.thread_types.push(elem.to_string());
                }
                module.runtime.has_thread = true;
            }
        }
        if name == "Process" {
            module.runtime.has_process = true;
        }
    }

    // Thread-spawned function metadata
    for (fn_name, (ret_type, ret_name, stack_size)) in &ctx.spawn.thread_fns {
        if !module.runtime.thread_spawned_fns.iter().any(|(n, _, _, _)| n == fn_name) {
            module.runtime.thread_spawned_fns.push((fn_name.clone(), *ret_type, ret_name.clone(), *stack_size));
        }
    }

    // Spawned function metadata for spawn/await helper emission
    for (fn_name, _) in &ctx.spawn.fn_names {
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
            module.runtime.spawned_fns.push((fn_name.clone(), params, *ret_type));
        }
    }
    module.runtime.has_spawn = !ctx.spawn.fn_names.is_empty();
    module.runtime.blocking_fn_names = ctx.spawn.blocking_fn_names.clone();
    module.runtime.has_blocking_pool = !ctx.spawn.blocking_fn_names.is_empty();
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
    use crate::ir::{Global, GlobalInit, GlobalInitArg};

    // Use map_ast_type_mut so that generic types like Mutex[int] get registered
    // (map_ast_type returns UNIT_TYPE for unregistered generic types).
    let type_id = ctx.type_mapper.map_ast_type_mut(&decl.type_.node, &mut ctx.type_registry);
    let name = decl.name.node.clone();

    // Bug B: collection-literal static initializers (`static Vector[T] = [..]`,
    // `static Dict[K,V] = {..}`) cannot be encoded as a compile-time `GlobalInit`
    // — their elements are arbitrary expressions (e.g. `Item("alpha")`). They
    // would otherwise fall through to `GlobalInit::Zeroed`, silently dropping
    // every element. Instead, synthesize a zero-arg `__gg_static_init_<name>()`
    // function whose body materializes the RHS, register it as a runtime-init
    // (`Extern { name, args: [] }`), and seed it as a DCE root (see
    // `src/lir/optimize.rs`). The prologue emits `__lir_g<N> = name();`.
    // Chain C item 2: enum-typed statics (`static Option[String] G = None`,
    // `static Color C = Color.Blue()`) — `eval_static_init` has no arm for
    // NoneLiteral / enum-variant ctors, so EVERY enum-typed static fell
    // through to `GlobalInit::Zeroed` SILENTLY (Option's Some = tag 0, so
    // `Some(5)` read as Some with a zeroed payload; user enums read as
    // their first variant). Route them through the same synthetic-init-fn
    // path as collection literals. Widening is CALLER-side by design: the
    // `initializer_needs_synthetic_fn` predicate is type-blind (an
    // expression test), while enum-ness is typed registry metadata
    // available here. The `__gg_static_init_` prefix is already DCE-seeded
    // (`src/lir/optimize.rs`). NOTE: the bugB §3 init-ordering caveat now
    // also covers enum statics whose variant args read other statics.
    // Optional later: compile-time tag-only GlobalInit for payload-less
    // variants (see TODO).
    let is_enum_typed = matches!(
        ctx.type_registry.get(type_id),
        Some(GirType::Named(n))
            if matches!(
                ctx.type_registry.get_type_def(n).map(|d| &d.kind),
                Some(TypeDefKind::Enum(_))
            )
    );
    // Bug #2: a `static Struct = Ctor(non-literal args...)` (e.g.
    // `static Box2 B = Box2(Vector[int]())`) cannot be encoded as a compile-time
    // `GlobalInit` — `eval_static_init`'s catch-all falls to `GlobalInit::Zeroed`,
    // leaving resource fields (`B.items`) NULL so `push` no-ops. Route those
    // through the same synthetic-init-fn machinery. R1-BLOCKING: literal-arg
    // ctors (`Point(3,4)`/`Counter(0)`) are EXCLUDED by the predicate so they
    // stay on the compile-time `GlobalInit::Struct` path (no regression of
    // `static_global_method_call.gg`; output-neutral on every literal-arg static).
    // R34 Track A: intercept const-foldable global collections/aggregates and
    // emit them as STATIC C data (a `cap = 0` view over a file-scope compound
    // literal) instead of an imperative `__gg_static_init_<name>()` builder —
    // no runtime ctor, no startup allocation. GATED on two soundness checks:
    //   1. The static is NEVER directly mutated (mutation prescan, `DefId`
    //      membership) — a `cap = 0` view points at immutable rodata that a
    //      `push`/`&STATIC`/`STATIC[i]=x` would corrupt or segfault on.
    //   2. Every element/resource-field type is READ-SAFE (`try_const_view`'s
    //      internal `elem_type_read_safe` gate) — a `Custom`/`Trivial`-drop
    //      element with no clone path would be MOVE-ZEROED on IndexLoad,
    //      zeroing the rodata slot.
    // Any unhandled shape → `try_const_view` returns `None` → the existing
    // imperative path runs unchanged (predicate/emitter are fused, so they
    // never disagree).
    let static_def_id = ctx
        .analysis
        .scopes
        .lookup_def_by_span(&decl.name.node, decl.name.span);
    let directly_mutated = static_def_id
        .map(|d| ctx.mutated_static_defs.contains(&d))
        .unwrap_or(true); // unresolved def → be conservative, stay imperative
    if !directly_mutated {
        if let Some(view) = try_const_view(ctx, type_id, &decl.value.node) {
            module.globals.push(Global { name, type_id, init: view });
            return;
        }
    }

    let needs_struct_ctor_init = struct_ctor_needs_synthetic_fn(ctx, type_id, &decl.value.node);
    if initializer_needs_synthetic_fn(&decl.value.node) || is_enum_typed || needs_struct_ctor_init {
        let synth_name = format!("__gg_static_init_{}", name);
        synthesize_static_init_fn(ctx, &synth_name, decl);
        // Mirror `module.globals.push` below, changing ONLY `init`: the global
        // slot is left zeroed and the prologue assigns it from the synthetic fn.
        module.globals.push(Global {
            name,
            type_id,
            init: GlobalInit::Extern { name: synth_name, args: vec![] },
        });
        return;
    }

    let init = eval_static_init(&decl.type_.node, &decl.value.node, &ctx.extern_bindings);
    // Record `String FOO = "literal"` globals — these lower to a cap=0
    // rodata-view static initializer in the C/LLVM backends (no heap
    // allocation, no runtime ctor). The GIR clone-on-access path
    // (`clone_resource_global_ref`) checks this set and elides the
    // implicit clone for these globals — making module-level string
    // constants truly zero-cost to read.
    if let GlobalInit::Extern { name: ctor, args } = &init {
        if ctor == "gorget_str_from_literal"
            && matches!(args.as_slice(), [GlobalInitArg::StrLit(_), GlobalInitArg::Int(_)])
        {
            ctx.string_literal_view_globals.insert(name.clone());
        }
    }
    module.globals.push(Global { name, type_id, init });
}

/// Bug B: does this static-initializer RHS require a synthesized init function?
///
/// True for array/dict literals only (set literals `{a,b,c}` parse as
/// `Expr::ArrayLiteral`, so they're covered too). These have arbitrary-
/// expression elements that cannot be encoded as a compile-time `GlobalInit`,
/// and would otherwise hit the `GlobalInit::Zeroed` fallback in
/// `eval_static_init`, silently dropping every element. v1 deliberately does
/// NOT widen to general non-const RHS (e.g. `static Foo X = some_fn()`) — that
/// has an init-ordering interaction across statics that needs a spec first
/// (see `the static-collection-init writeup (git history)` §3). Widening is a one-line
/// change here plus an init-ordering decision.
fn initializer_needs_synthetic_fn(expr: &crate::parser::ast::Expr) -> bool {
    use crate::parser::ast::Expr;
    matches!(expr, Expr::ArrayLiteral(_) | Expr::DictLiteral(_))
}

/// Bug #2: does this static-initializer RHS construct a USER STRUCT whose ctor
/// args are NOT all compile-time literals? Such a static (`static Box2 B =
/// Box2(Vector[int]())`) cannot be encoded as a compile-time `GlobalInit::Struct`
/// — `eval_static_init`'s catch-all (`literal_to_global_init` per arg) falls to
/// `GlobalInit::Zeroed`, leaving `B.items` a NULL collection (`push` no-ops).
/// Route those through the synthetic-init-fn path instead.
///
/// ⚠ R1 BLOCKING: scope by LITERAL-NESS of the ctor args, NOT just "is a struct
/// ctor". Literal-arg ctors (`Point(3,4)`, `Counter(0)`) MUST stay on the
/// compile-time `GlobalInit::Struct` path (`eval_static_init` `_` arm) — diverting
/// them to the runtime synthetic fn regresses `static_global_method_call.gg` and is
/// non-neutral C for the scalar fixtures. The literal-ness test here MIRRORS that
/// catch-all exactly: `callee_name == type_name`, non-empty args, and at least one
/// arg that `literal_to_global_init` rejects. (Collection/sync ctors — `Vector(..)`,
/// `Mutex(..)`, etc. — are handled by their own `eval_static_init` arms and never
/// reach the catch-all; the `type_id`-is-Struct guard excludes them here too.)
fn struct_ctor_needs_synthetic_fn(
    ctx: &LoweringContext,
    type_id: TypeId,
    expr: &crate::parser::ast::Expr,
) -> bool {
    use crate::parser::ast::Expr;

    // The static's declared type must be a registered user struct. This excludes
    // builtin collections / sync primitives (handled by dedicated arms above) and
    // enums (routed via `is_enum_typed`).
    let Some(type_name) = ctx.type_name_for_id(type_id) else {
        return false;
    };
    let is_struct = matches!(
        ctx.type_registry.get_type_def(type_name).map(|d| &d.kind),
        Some(TypeDefKind::Struct(_))
    );
    if !is_struct {
        return false;
    }

    // Match the ctor shape `<type_name>(args...)` and test literal-ness the SAME
    // way `eval_static_init`'s catch-all does (`literal_to_global_init` per arg).
    // We only need the synthetic fn when there is at least one NON-LITERAL arg —
    // i.e. exactly the inputs that would otherwise hit `GlobalInit::Zeroed`.
    // All-literal ctors (and the no-arg case) stay on the compile-time path:
    //   - `Point(3,4)` / `Counter(0)` → catch-all builds `GlobalInit::Struct`,
    //   - `T()` (no args) → catch-all leaves `GlobalInit::Zeroed` (unchanged).
    let (callee_name, has_nonliteral_arg): (&str, bool) = match expr {
        Expr::StructLiteral { name, args, .. } => (
            name.node.as_str(),
            args.iter().any(|a| literal_to_global_init(&a.node).is_none()),
        ),
        Expr::Call { callee, args, .. } => {
            let cname = match &callee.node {
                Expr::Identifier(n) => n.as_str(),
                _ => return false,
            };
            (
                cname,
                args.iter()
                    .any(|a| literal_to_global_init(&a.node.value.node).is_none()),
            )
        }
        _ => return false,
    };

    // Only a ctor naming the static's own struct type, with ≥1 non-literal arg.
    callee_name == type_name && has_nonliteral_arg
}

/// Bug B: build a synthetic `<T> __gg_static_init_<name>(): <T> __r = RHS; return __r`
/// function and push it onto `ctx.synthetic_static_init_fns`. Lowered later in
/// the non-generic function loop via the normal `lower_function` path.
///
/// The body MUST be the `VarDecl`-with-declared-type then `return __r` shape,
/// NOT `return RHS`: a `VarDecl` sets `ctx.func_state.expected_type` to the
/// declared type before lowering the init (`stmts/mod.rs:478`), which the
/// array-literal lowering reads to size the buffer and propagate the element
/// type (`exprs/collections.rs:46-63,165-167`). A bare `return RHS` would leave
/// `expected_type` unset → wrong `elem_size` → quiet runtime corruption. Do not
/// "simplify" this to `return RHS`.
fn synthesize_static_init_fn(
    ctx: &mut LoweringContext,
    synth_name: &str,
    decl: &crate::parser::ast::StaticDecl,
) {
    use crate::parser::ast::{
        Block, Expr, FunctionBody, FunctionDef, FunctionQualifiers, Pattern, SharedKind, Stmt,
        ThrowsSpec, Visibility,
    };
    use crate::span::Spanned;

    let span = decl.span;

    let var_decl_stmt = Stmt::VarDecl {
        is_const: false,
        is_mutable: true,
        shared: SharedKind::None,
        type_: decl.type_.clone(),
        pattern: Spanned::new(Pattern::Binding("__r".into()), span),
        value: decl.value.clone(),
    };
    let return_stmt = Stmt::Return(Some(Spanned::new(Expr::Identifier("__r".into()), span)));

    let func = FunctionDef {
        attributes: vec![],
        visibility: Visibility::Private,
        qualifiers: FunctionQualifiers::default(),
        return_type: decl.type_.clone(),
        name: Spanned::new(synth_name.to_string(), span),
        generic_params: None,
        params: vec![],
        throws: ThrowsSpec::No,
        body: FunctionBody::Block(Block {
            stmts: vec![Spanned::new(var_decl_stmt, span), Spanned::new(return_stmt, span)],
            span,
        }),
        doc_comment: None,
        span,
        param_abis: vec![],
        extern_abi: None,
        returns_borrowed: false,
    };

    ctx.synthetic_static_init_fns.push(func);
}

/// Evaluate a static initializer expression into a GlobalInit value.
/// Supports constructor calls for sync primitives that require heap allocation.
fn eval_static_init(
    ty: &crate::parser::ast::Type,
    expr: &crate::parser::ast::Expr,
    extern_bindings: &rustc_hash::FxHashMap<String, String>,
) -> crate::ir::GlobalInit {
    use crate::ir::{GlobalInit, GlobalInitArg};
    use crate::parser::ast::Expr;

    // Handle primitive-type statics with literal initializers. These are
    // compile-time constants — encode as `Bytes` (8-byte LE for int / float;
    // 1-byte for bool). String literals need a runtime call into
    // `gorget_str_from_literal` to wrap raw text + length into a `Str`
    // struct, so they go through the `Extern` path.
    match expr {
        Expr::IntLiteral(n) => return GlobalInit::Bytes((*n as i64).to_le_bytes().to_vec()),
        Expr::FloatLiteral(f) => return GlobalInit::Bytes(f.to_le_bytes().to_vec()),
        Expr::BoolLiteral(b) => return GlobalInit::Bytes(vec![if *b { 1 } else { 0 }]),
        Expr::StringLiteral(s, _) => {
            // Str is `{ const char*, size_t }` — runtime wrapper turns
            // the text + length pair into the static struct.
            let text = s.as_plain_text();
            let len = text.len() as i64;
            return GlobalInit::Extern {
                name: "gorget_str_from_literal".to_string(),
                args: vec![GlobalInitArg::StrLit(text), GlobalInitArg::Int(len)],
            };
        }
        // Negative literals: parsed as UnaryOp { op: Neg, operand: IntLiteral/FloatLiteral }
        Expr::UnaryOp { op: crate::parser::ast::UnaryOp::Neg, operand } => {
            match &operand.node {
                Expr::IntLiteral(n) => return GlobalInit::Bytes((-(*n as i64)).to_le_bytes().to_vec()),
                Expr::FloatLiteral(f) => return GlobalInit::Bytes((-f).to_le_bytes().to_vec()),
                _ => {}
            }
        }
        _ => {} // Fall through to Named type handling below
    }

    // Primitive-typed static initialised by an extern call —
    // `public float INFINITY = _math_infinity()` in `lib/std/math.gg` and
    // its kin. The Gorget callee name resolves through the extern-bindings
    // registry to the runtime C symbol (`_math_infinity` →
    // `gorget_math_infinity`); the C backend emits the call at the
    // module-init prologue. Args must be literal-only — anything richer
    // belongs in `main()`'s init slot, not a global initialiser.
    if matches!(ty, crate::parser::ast::Type::Primitive(_)) {
        if let Expr::Call { callee, args, .. } = expr {
            if let Expr::Identifier(gname) = &callee.node {
                if let Some(c_symbol) = extern_bindings.get(gname.as_str()) {
                    let mut arg_inits = Vec::with_capacity(args.len());
                    let mut all_literal = true;
                    for a in args {
                        match literal_to_global_init_arg(&a.node.value.node) {
                            Some(arg) => arg_inits.push(arg),
                            None => { all_literal = false; break; }
                        }
                    }
                    if all_literal {
                        return GlobalInit::Extern {
                            name: c_symbol.clone(),
                            args: arg_inits,
                        };
                    }
                }
            }
        }
    }

    // Extract the type name (ignoring generic args) for dispatch
    let type_name = match ty {
        crate::parser::ast::Type::Named { name, .. } => name.node.as_str(),
        _ => return GlobalInit::Zeroed,
    };

    // Constructor syntax: TypeName(args...) is parsed as StructLiteral.
    // Fallback: plain function Call (e.g. from explicit call-style expressions).
    let (callee_name, arg_inits): (&str, Vec<GlobalInitArg>) = match expr {
        Expr::StructLiteral { name, args, .. } => {
            let inits = args.iter().filter_map(|a| literal_to_global_init_arg(&a.node)).collect();
            (name.node.as_str(), inits)
        }
        Expr::Call { callee, args, .. } => {
            let cname = match &callee.node {
                Expr::Identifier(n) => n.as_str(),
                _ => return GlobalInit::Zeroed,
            };
            let inits = args.iter().filter_map(|a| literal_to_global_init_arg(&a.node.value.node)).collect();
            (cname, inits)
        }
        _ => return GlobalInit::Zeroed,
    };

    // Dispatch by type/callee name. Each arm builds a typed `Extern { name, args }`
    // — backends consume the structure directly. New runtime ctors plug in by
    // adding a new arm; nothing here speaks C syntax.
    let extern_init = match type_name {
        "AtomicInt" if callee_name == "AtomicInt" => GlobalInit::Extern {
            name: "gorget_atomic_int_new".to_string(),
            args: vec![arg_inits.first().cloned().unwrap_or(GlobalInitArg::Int(0))],
        },
        "AtomicBool" if callee_name == "AtomicBool" => {
            // `gorget_atomic_bool_new(int)` — backends render Bool as the
            // matching 0/1 int literal.
            let b = match arg_inits.first() {
                Some(GlobalInitArg::Bool(true)) => GlobalInitArg::Int(1),
                Some(GlobalInitArg::Bool(false)) | None => GlobalInitArg::Int(0),
                Some(other) => other.clone(),
            };
            GlobalInit::Extern {
                name: "gorget_atomic_bool_new".to_string(),
                args: vec![b],
            }
        }
        "Barrier" if callee_name == "Barrier" => GlobalInit::Extern {
            name: "gorget_barrier_new".to_string(),
            args: vec![arg_inits.first().cloned().unwrap_or(GlobalInitArg::Int(1))],
        },
        "CondVar" if callee_name == "CondVar" => GlobalInit::Extern {
            name: "gorget_condvar_new".to_string(),
            args: vec![],
        },
        "Mutex" if callee_name == "Mutex" => {
            let elem_c = generic_elem_c_type(ty);
            GlobalInit::Extern {
                name: format!("Mutex__{elem_c}__new"),
                args: vec![arg_inits.first().cloned().unwrap_or(GlobalInitArg::Int(0))],
            }
        }
        "RWLock" if callee_name == "RWLock" => {
            let elem_c = generic_elem_c_type(ty);
            GlobalInit::Extern {
                name: format!("RWLock__{elem_c}__new"),
                args: vec![arg_inits.first().cloned().unwrap_or(GlobalInitArg::Int(0))],
            }
        }
        // Collections: Dict, HashMap, Vector need runtime heap allocation.
        "Dict" if callee_name == "Dict" => {
            // Collection-valued keys/values (`Dict[int, Vector[int]]`) spell
            // their element size as the runtime handle struct, not the surface
            // type name (which is not a declared C type). See
            // `collection_arg_sizeof_c_type`.
            let key_c = collection_arg_sizeof_c_type(ty, 0);
            let val_c = collection_arg_sizeof_c_type(ty, 1);
            if key_c == "GorgetString" {
                GlobalInit::Extern {
                    name: "gorget_dict_new_str".to_string(),
                    args: vec![GlobalInitArg::Sizeof(val_c)],
                }
            } else {
                GlobalInit::Extern {
                    name: "gorget_dict_new".to_string(),
                    args: vec![GlobalInitArg::Sizeof(key_c), GlobalInitArg::Sizeof(val_c)],
                }
            }
        }
        "HashMap" if callee_name == "HashMap" => {
            let key_c = collection_arg_sizeof_c_type(ty, 0);
            let val_c = collection_arg_sizeof_c_type(ty, 1);
            if key_c == "GorgetString" {
                GlobalInit::Extern {
                    name: "gorget_map_new_str".to_string(),
                    args: vec![GlobalInitArg::Sizeof(val_c)],
                }
            } else {
                GlobalInit::Extern {
                    name: "gorget_map_new".to_string(),
                    args: vec![GlobalInitArg::Sizeof(key_c), GlobalInitArg::Sizeof(val_c)],
                }
            }
        }
        "Vector" if callee_name == "Vector" => {
            let elem_c = collection_arg_sizeof_c_type(ty, 0);
            GlobalInit::Extern {
                name: "gorget_array_new".to_string(),
                args: vec![GlobalInitArg::Sizeof(elem_c)],
            }
        }
        // Set / HashSet: a `static Set[T] S = Set[T]()` previously fell
        // through to `GlobalInit::Zeroed` (no arm existed) → null header →
        // SEGFAULT on the first runtime `.add` on BOTH backends. Match the
        // local-ctor convention (`src/lir/lower/calls.rs`): `Set` → ordered
        // (`gorget_ordered_set_new`), `HashSet` → unordered
        // (`gorget_set_new`); the `_str` variants (zero-arg) for String
        // elements. The element-size operand uses the collection handle
        // struct name for collection-valued elements (same as Vector/Dict).
        "Set" if callee_name == "Set" => {
            let elem_c = collection_arg_sizeof_c_type(ty, 0);
            if elem_c == "GorgetString" {
                GlobalInit::Extern {
                    name: "gorget_ordered_set_new_str".to_string(),
                    args: vec![],
                }
            } else {
                GlobalInit::Extern {
                    name: "gorget_ordered_set_new".to_string(),
                    args: vec![GlobalInitArg::Sizeof(elem_c)],
                }
            }
        }
        "HashSet" if callee_name == "HashSet" => {
            let elem_c = collection_arg_sizeof_c_type(ty, 0);
            if elem_c == "GorgetString" {
                GlobalInit::Extern {
                    name: "gorget_set_new_str".to_string(),
                    args: vec![],
                }
            } else {
                GlobalInit::Extern {
                    name: "gorget_set_new".to_string(),
                    args: vec![GlobalInitArg::Sizeof(elem_c)],
                }
            }
        }
        // Standard-handle getters from std.io — zero-arg externs that the
        // backends call at main()'s prologue to give the `File` global a
        // live `GorgetFile` handle.
        "File" if callee_name == "_stdout_handle" => GlobalInit::Extern {
            name: "gorget_stdout_handle".to_string(), args: vec![],
        },
        "File" if callee_name == "_stderr_handle" => GlobalInit::Extern {
            name: "gorget_stderr_handle".to_string(), args: vec![],
        },
        "File" if callee_name == "_stdin_handle" => GlobalInit::Extern {
            name: "gorget_stdin_handle".to_string(), args: vec![],
        },
        _ => {
            // Generic struct constructor: if callee matches the type name and all
            // args are literals, emit a typed compile-time `Struct` initializer
            // (positional fields named `_0`, `_1`, …; the lowerer drops the
            // names). Both backends already render `Struct` to their target
            // syntax (`{f0, f1, …}` for C, `%T { i64 f0, … }` for LLVM) — no
            // runtime constructor needed. Replaces the prior
            // `RuntimeCall(format!("(T){{...}}"))` shape that forced backends
            // to parse C compound-literal syntax out of an opaque string.
            if callee_name == type_name {
                let arg_inits: Option<Vec<GlobalInit>> = match expr {
                    Expr::StructLiteral { args, .. } =>
                        args.iter().map(|a| literal_to_global_init(&a.node)).collect(),
                    Expr::Call { args, .. } =>
                        args.iter().map(|a| literal_to_global_init(&a.node.value.node)).collect(),
                    _ => None,
                };
                if let Some(inits) = arg_inits {
                    if !inits.is_empty() {
                        let fields = inits.into_iter().enumerate()
                            .map(|(i, init)| (format!("_{i}"), init))
                            .collect();
                        return GlobalInit::Struct {
                            type_name: type_name.to_string(),
                            fields,
                        };
                    }
                }
            }
            return GlobalInit::Zeroed;
        }
    };

    extern_init
}

// `eval_literal_arg` (string-based literal formatting) was retired
// alongside `RuntimeCall(String)` — `literal_to_global_init_arg` now
// produces typed `GlobalInitArg` values directly.

/// Lower a literal expression to a `GlobalInitArg` for use in a typed
/// `GlobalInit::Extern { name, args }`. Returns `None` for non-literal
/// exprs (the constructor falls back to `GlobalInit::Zeroed` upstream).
fn literal_to_global_init_arg(expr: &crate::parser::ast::Expr) -> Option<crate::ir::GlobalInitArg> {
    use crate::parser::ast::Expr;
    use crate::ir::GlobalInitArg;
    match expr {
        Expr::IntLiteral(n) => Some(GlobalInitArg::Int(*n as i64)),
        Expr::FloatLiteral(f) => Some(GlobalInitArg::Float(*f)),
        Expr::BoolLiteral(b) => Some(GlobalInitArg::Bool(*b)),
        Expr::StringLiteral(s, _) => Some(GlobalInitArg::StrLit(s.as_plain_text())),
        Expr::UnaryOp { op: crate::parser::ast::UnaryOp::Neg, operand } => match &operand.node {
            Expr::IntLiteral(n) => Some(GlobalInitArg::Int(-(*n as i64))),
            Expr::FloatLiteral(f) => Some(GlobalInitArg::Float(-f)),
            _ => None,
        },
        _ => None,
    }
}

/// Lower a compile-time literal expression to a `GlobalInit::Bytes`
/// payload. Integers / negated integers encode as 8-byte little-endian
/// i64 (the byte-length-driven C/LLVM backends pick the right width
/// from the consuming struct field's type). Floats encode as 8-byte
/// f64. Bools encode as a single byte. Returns `None` for non-literal
/// exprs.
fn literal_to_global_init(expr: &crate::parser::ast::Expr) -> Option<crate::ir::GlobalInit> {
    use crate::parser::ast::Expr;
    use crate::ir::GlobalInit;
    match expr {
        Expr::IntLiteral(n) => Some(GlobalInit::Bytes(n.to_le_bytes().to_vec())),
        Expr::FloatLiteral(f) => Some(GlobalInit::Bytes(f.to_le_bytes().to_vec())),
        Expr::BoolLiteral(b) => Some(GlobalInit::Bytes(vec![if *b { 1 } else { 0 }])),
        Expr::UnaryOp { op: crate::parser::ast::UnaryOp::Neg, operand } => match &operand.node {
            Expr::IntLiteral(n) => Some(GlobalInit::Bytes((-(*n as i64)).to_le_bytes().to_vec())),
            Expr::FloatLiteral(f) => Some(GlobalInit::Bytes((-f).to_le_bytes().to_vec())),
            _ => None,
        },
        _ => None,
    }
}

// ── R34 Track A: shape-driven const-value emitter ──────────────────────────
//
// `try_const_view` turns a const-foldable static initializer (a Vector-of-
// struct/enum/string/scalar literal, recursively) into a `GlobalInit` tree
// that the backends bake as STATIC C data — a `cap = 0` view over a file-scope
// compound literal — instead of an imperative `__gg_static_init_<name>()`
// builder. The predicate and the emitter are ONE fused function: any shape it
// cannot encode returns `None`, and `lower_static_decl` falls through to the
// unchanged imperative path (so predicate and emitter can never disagree).
//
// Shapes handled (covers all five self-host tables — RUNTIME_FNS, RESOURCES,
// the three BUILTIN_* — plus any user const global collection of the same
// shapes): scalar literals; `String` fields (→ cap=0 rodata view); struct
// literals of const args; FLAT enum variants (payload-less AND String/const-
// payload like `MkExact("…")`); bare `Option[T]` (`Some(constval)` / `None`);
// and nested `Vector` literals (→ nested `StaticArrayView`). Dict/Set are OUT
// (hash-layout replication is a separate, harder problem — session 2+).

/// Entry point: attempt to encode `expr` (typed `type_id`) as a const
/// `GlobalInit`. Dispatches on the resolved GIR type.
fn try_const_view(
    ctx: &LoweringContext,
    type_id: TypeId,
    expr: &crate::parser::ast::Expr,
) -> Option<crate::ir::GlobalInit> {
    let gir = ctx.type_registry.get(type_id)?.clone();
    match &gir {
        GirType::Bool
        | GirType::I8 | GirType::I16 | GirType::I32 | GirType::I64
        | GirType::U8 | GirType::U16 | GirType::U32 | GirType::U64
        | GirType::F32 | GirType::F64 => {
            const_scalar_bytes(&gir, expr).map(crate::ir::GlobalInit::Bytes)
        }
        GirType::Named(name) => try_const_named(ctx, name, expr),
        _ => None,
    }
}

/// Encode `expr` against a mangled type NAME (used for `StaticArrayView`
/// elements, where only the decoded element name is in hand — no `TypeId`).
fn try_const_view_by_name(
    ctx: &LoweringContext,
    name: &str,
    expr: &crate::parser::ast::Expr,
) -> Option<crate::ir::GlobalInit> {
    if let Some(gir) = mangled_scalar_gir(name) {
        return const_scalar_bytes(&gir, expr).map(crate::ir::GlobalInit::Bytes);
    }
    try_const_named(ctx, name, expr)
}

/// Encode `expr` against a Named GIR type: String view, collection, enum, or
/// struct. Non-const / unsupported shapes → `None`.
fn try_const_named(
    ctx: &LoweringContext,
    name: &str,
    expr: &crate::parser::ast::Expr,
) -> Option<crate::ir::GlobalInit> {
    use crate::parser::ast::Expr;
    use crate::ir::{GlobalInit, GlobalInitArg};

    // `String` field → a `cap = 0` view into the C `.rodata` section (mirror
    // the module-level string-literal path in `lower_static_decl`). The `len`
    // is the BYTE length — `gorget_str_from_literal`'s contract.
    if matches!(name, "GorgetString" | "Str" | "String") {
        if let Expr::StringLiteral(s, _) = expr {
            let text = s.as_plain_text();
            let len = text.len() as i64;
            return Some(GlobalInit::Extern {
                name: "gorget_str_from_literal".to_string(),
                args: vec![GlobalInitArg::StrLit(text), GlobalInitArg::Int(len)],
            });
        }
        return None;
    }

    // Snapshot what we need from the TypeDef, then release the borrow so the
    // recursive calls can re-borrow `ctx.type_registry` freely.
    let td = ctx.type_registry.get_type_def(name)?;
    if td.metadata.collection_kind == Some(CollectionKind::Array) {
        // Vector-family collection → nested static view. (Dict/Set are gated
        // out by requiring collection_kind == Array specifically.)
        return try_const_array(ctx, name, expr);
    }
    match &td.kind {
        TypeDefKind::Enum(edef) => try_const_enum(ctx, name, edef, expr),
        TypeDefKind::Struct(sdef) if !sdef.fields.is_empty() => try_const_struct(ctx, name, sdef, expr),
        // Zero-field structs / aliases: nothing const to encode.
        _ => None,
    }
}

/// `Vector[T]` literal → `StaticArrayView`. The element type name is recovered
/// by stripping the collection prefix from the mangled name (structural decode,
/// gated by the TYPED `collection_kind == Array` in the caller — the name is
/// never used to DECIDE collection-ness, only to recover the element type, the
/// same pattern `synthesize_struct_fields` uses for `Option__`/`Result__`).
fn try_const_array(
    ctx: &LoweringContext,
    coll_name: &str,
    expr: &crate::parser::ast::Expr,
) -> Option<crate::ir::GlobalInit> {
    use crate::parser::ast::Expr;
    let Expr::ArrayLiteral(elems) = expr else { return None; };
    let elem_name = coll_name.split_once("__").map(|(_, rest)| rest)?;

    // Drop-strategy soundness gate: the element type must NEVER be move-zeroed
    // by the C `IndexLoad` read path — else reading `STATIC[i]` would zero the
    // immutable rodata slot. Safe iff (recursively) drop ∈ {None, Recursive}
    // OR the type has a clone_fn (the read path clones instead of moving).
    let mut visited = rustc_hash::FxHashSet::default();
    if !elem_type_read_safe(ctx, elem_name, &mut visited) {
        return None;
    }

    let mut out = Vec::with_capacity(elems.len());
    for e in elems {
        out.push(try_const_view_by_name(ctx, elem_name, &e.node)?);
    }
    Some(crate::ir::GlobalInit::StaticArrayView {
        elem_type_name: elem_name.to_string(),
        elems: out,
    })
}

/// Struct literal of const args → `GlobalInit::Struct` (positional fields).
fn try_const_struct(
    ctx: &LoweringContext,
    name: &str,
    sdef: &crate::ir::types::StructDef,
    expr: &crate::parser::ast::Expr,
) -> Option<crate::ir::GlobalInit> {
    let args = ctor_positional_args(name, expr)?;
    if args.len() != sdef.fields.len() {
        return None;
    }
    let mut fields = Vec::with_capacity(sdef.fields.len());
    for (field, arg) in sdef.fields.iter().zip(args.iter()) {
        let fv = try_const_view(ctx, field.type_id, arg)?;
        fields.push((field.name.clone(), fv));
    }
    Some(crate::ir::GlobalInit::Struct { type_name: name.to_string(), fields })
}

/// FLAT enum variant (payload-less or const/String payload, incl. bare
/// `Option` `Some`/`None`) → a tagged `GlobalInit::Struct` matching the flat C
/// enum layout `{ i32 tag; <variant0 fields…>; <variant1 fields…>; … }`. The
/// active variant's payload slots recurse; every other slot is `Zeroed`.
/// Union-layout enums (flat field count > 4, the LIR `is_large_enum`
/// threshold) are rejected — the positional container only models the flat
/// layout.
fn try_const_enum(
    ctx: &LoweringContext,
    name: &str,
    edef: &crate::ir::types::EnumDef,
    expr: &crate::parser::ast::Expr,
) -> Option<crate::ir::GlobalInit> {
    let flat_field_count: usize = 1 + edef.variants.iter().map(|v| v.fields.len()).sum::<usize>();
    if flat_field_count > 4 {
        return None; // union layout — not modeled by the flat positional container
    }
    let (variant_name, payload_args) = enum_variant_and_args(expr)?;
    let idx = ctx.resolve_variant_tag(name, &variant_name)?; // declaration index == runtime tag
    let variant = edef.variants.get(idx as usize)?;
    if payload_args.len() != variant.fields.len() {
        return None;
    }
    let mut fields: Vec<(String, crate::ir::GlobalInit)> = Vec::with_capacity(flat_field_count);
    fields.push((
        "tag".to_string(),
        crate::ir::GlobalInit::Bytes((idx as i32).to_le_bytes().to_vec()),
    ));
    for (vi, v) in edef.variants.iter().enumerate() {
        for (fi, f) in v.fields.iter().enumerate() {
            let slot_name = format!("{}_{}", v.name, fi);
            if vi == idx as usize {
                let val = try_const_view(ctx, f.type_id, payload_args[fi])?;
                fields.push((slot_name, val));
            } else {
                fields.push((slot_name, crate::ir::GlobalInit::Zeroed));
            }
        }
    }
    Some(crate::ir::GlobalInit::Struct { type_name: name.to_string(), fields })
}

/// The IndexLoad move-zero soundness gate (recursive). Mirrors the C read-path
/// decision at `src/lir/lower/insts.rs`: a `cap = 0` view element is READ-SAFE
/// iff the read never memsets the source slot — i.e. the type has a `clone_fn`
/// (read clones), OR its drop strategy is `None`/`Recursive` (read Loads or
/// deep-clones, no zero). `Trivial`/`Custom` drop with no clone_fn → the read
/// move-zeroes the rodata → UNSAFE.
fn elem_type_read_safe(
    ctx: &LoweringContext,
    name: &str,
    visited: &mut rustc_hash::FxHashSet<String>,
) -> bool {
    if mangled_scalar_gir(name).is_some() {
        return true; // primitives are trivially copyable
    }
    if matches!(name, "GorgetString" | "Str" | "String") {
        return true; // string reads clone (gorget_string_clone_to_owned)
    }
    if !visited.insert(name.to_string()) {
        return true; // cycle guard
    }
    let (own_safe, is_array, child_type_ids): (bool, bool, Vec<TypeId>) = {
        let Some(td) = ctx.type_registry.get_type_def(name) else {
            return true; // no TypeDef → opaque/primitive-ish → nothing to move-zero here
        };
        let own = td.metadata.clone_fn.is_some()
            || matches!(
                td.metadata.drop_strategy,
                DropStrategy::None | DropStrategy::Recursive
            );
        let is_array = td.metadata.collection_kind == Some(CollectionKind::Array);
        let mut ids = Vec::new();
        match &td.kind {
            TypeDefKind::Struct(sd) => {
                for f in &sd.fields {
                    ids.push(f.type_id);
                }
            }
            TypeDefKind::Enum(ed) => {
                for v in &ed.variants {
                    for f in &v.fields {
                        ids.push(f.type_id);
                    }
                }
            }
            _ => {}
        }
        (own, is_array, ids)
    };
    if !own_safe {
        return false;
    }
    for tid in child_type_ids {
        if let Some(GirType::Named(child)) = ctx.type_registry.get(tid) {
            let child = child.clone();
            if !elem_type_read_safe(ctx, &child, visited) {
                return false;
            }
        }
    }
    if is_array {
        if let Some((_, elem)) = name.split_once("__") {
            let elem = elem.to_string();
            if !elem_type_read_safe(ctx, &elem, visited) {
                return false;
            }
        }
    }
    true
}

/// Extract positional ctor args from `Name(a, b, …)` (Call) or `Name{ … }`
/// (StructLiteral). Rejects named args and a callee that doesn't match `name`.
fn ctor_positional_args<'e>(
    type_name: &str,
    expr: &'e crate::parser::ast::Expr,
) -> Option<Vec<&'e crate::parser::ast::Expr>> {
    use crate::parser::ast::Expr;
    match expr {
        Expr::Call { callee, args, .. } => {
            let cname = match &callee.node {
                Expr::Identifier(n) => n.as_str(),
                _ => return None,
            };
            if cname != type_name {
                return None;
            }
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                if a.node.name.is_some() {
                    return None; // named arg — not the positional const shape
                }
                out.push(&a.node.value.node);
            }
            Some(out)
        }
        Expr::StructLiteral { name, args, .. } => {
            if name.node != type_name {
                return None;
            }
            Some(args.iter().map(|a| &a.node).collect())
        }
        _ => None,
    }
}

/// Extract `(variant_name, payload_arg_exprs)` from an enum-variant value.
/// Handles the bare (`MkExact("x")`, `TF64()`, `Some(v)`, `None`) and
/// qualified (`Enum.Variant(...)`) forms.
fn enum_variant_and_args<'e>(
    expr: &'e crate::parser::ast::Expr,
) -> Option<(String, Vec<&'e crate::parser::ast::Expr>)> {
    use crate::parser::ast::Expr;
    match expr {
        Expr::NoneLiteral => Some(("None".to_string(), vec![])),
        Expr::Identifier(n) => Some((n.clone(), vec![])),
        Expr::Path { segments } => segments.last().map(|s| (s.node.clone(), vec![])),
        Expr::Call { callee, args, .. } => {
            let vname = match &callee.node {
                Expr::Identifier(n) => n.clone(),
                Expr::Path { segments } => segments.last()?.node.clone(),
                _ => return None,
            };
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                if a.node.name.is_some() {
                    return None;
                }
                out.push(&a.node.value.node);
            }
            Some((vname, out))
        }
        // Resolver-rewritten qualified variant ctor `Enum.Variant(args)` — the
        // enum name is the receiver, the variant is the "method". The variant
        // name is validated by `resolve_variant_tag` in the caller (a genuine
        // method call on a non-enum receiver simply won't resolve to a tag →
        // `None` → imperative fallthrough), so we don't re-check the receiver.
        Expr::MethodCall { method, args, .. } => {
            let mut out = Vec::with_capacity(args.len());
            for a in args {
                if a.node.name.is_some() {
                    return None;
                }
                out.push(&a.node.value.node);
            }
            Some((method.node.clone(), out))
        }
        _ => None,
    }
}

/// Encode a scalar literal to width-correct little-endian bytes matching the
/// GIR primitive type. Returns `None` for non-literal / mismatched exprs.
fn const_scalar_bytes(gir: &GirType, expr: &crate::parser::ast::Expr) -> Option<Vec<u8>> {
    use crate::parser::ast::{Expr, UnaryOp};
    fn as_i(e: &Expr) -> Option<i64> {
        match e {
            Expr::IntLiteral(n) => Some(*n as i64),
            Expr::UnaryOp { op: UnaryOp::Neg, operand } => match &operand.node {
                Expr::IntLiteral(n) => Some(-(*n as i64)),
                _ => None,
            },
            _ => None,
        }
    }
    fn as_f(e: &Expr) -> Option<f64> {
        match e {
            Expr::FloatLiteral(f) => Some(*f),
            Expr::IntLiteral(n) => Some(*n as f64),
            Expr::UnaryOp { op: UnaryOp::Neg, operand } => match &operand.node {
                Expr::FloatLiteral(f) => Some(-f),
                Expr::IntLiteral(n) => Some(-(*n as f64)),
                _ => None,
            },
            _ => None,
        }
    }
    match gir {
        GirType::Bool => match expr {
            Expr::BoolLiteral(b) => Some(vec![if *b { 1 } else { 0 }]),
            _ => None,
        },
        GirType::I8 | GirType::U8 => as_i(expr).map(|n| (n as i8).to_le_bytes().to_vec()),
        GirType::I16 | GirType::U16 => as_i(expr).map(|n| (n as i16).to_le_bytes().to_vec()),
        GirType::I32 | GirType::U32 => as_i(expr).map(|n| (n as i32).to_le_bytes().to_vec()),
        GirType::I64 | GirType::U64 => as_i(expr).map(|n| n.to_le_bytes().to_vec()),
        GirType::F32 => as_f(expr).map(|f| (f as f32).to_le_bytes().to_vec()),
        GirType::F64 => as_f(expr).map(|f| f.to_le_bytes().to_vec()),
        _ => None,
    }
}

/// Map a mangled primitive type name (`int64_t`, `double`, `bool`, …) to its
/// GIR primitive — used where only the decoded element name is in hand.
fn mangled_scalar_gir(name: &str) -> Option<GirType> {
    Some(match name {
        "bool" => GirType::Bool,
        "int8_t" | "int8" => GirType::I8,
        "int16_t" | "int16" => GirType::I16,
        "int32_t" | "int32" => GirType::I32,
        "int64_t" | "int" => GirType::I64,
        "uint8_t" | "uint8" => GirType::U8,
        "uint16_t" | "uint16" => GirType::U16,
        "uint32_t" | "uint32" => GirType::U32,
        "uint64_t" | "uint" => GirType::U64,
        "float" | "float32" => GirType::F32,
        "double" | "float64" => GirType::F64,
        _ => return None,
    })
}

// ── R34 Track A: whole-program static-mutation prescan ─────────────────────
//
// Walks every function-like body and records the `DefId` of any module-level
// static that is DIRECTLY mutated. The const-view optimization is refused for
// those statics (a `cap = 0` rodata view can't be grown, written through, or
// move-consumed). Keyed by the original static `DefId`, which is sound across
// module boundaries: imports rebind the imported name to the ORIGINAL
// definition's `DefId` (`ScopeTable::rebind_alias` / `bind_wildcard` /
// `export_non_private`), so a mutation in any module and the decl in
// `lower_static_decl` resolve to the same `DefId`.

/// Compute the set of directly-mutated static `DefId`s across the whole program.
fn scan_mutated_statics(
    analysis: &AnalysisResult,
    items: &[crate::span::Spanned<Item>],
) -> rustc_hash::FxHashSet<crate::semantic::ids::DefId> {
    let mut scan = StaticMutationScan {
        analysis,
        mutated: rustc_hash::FxHashSet::default(),
    };
    scan.scan_items(items);
    scan.mutated
}

struct StaticMutationScan<'a> {
    analysis: &'a AnalysisResult,
    mutated: rustc_hash::FxHashSet<crate::semantic::ids::DefId>,
}

impl<'a> StaticMutationScan<'a> {
    /// Visit every function-like body reachable from the item list, including
    /// equip-block methods, trait default methods, and test / bench / suite
    /// hooks — a static can be mutated from any executable body.
    fn scan_items(&mut self, items: &[crate::span::Spanned<Item>]) {
        use crate::parser::visitor::ExprVisitor;
        for item in items {
            match &item.node {
                Item::Function(f) => self.scan_body(&f.body),
                Item::Equip(eq) => {
                    for m in &eq.items {
                        self.scan_body(&m.node.body);
                    }
                }
                Item::Trait(t) => {
                    for it in &t.items {
                        if let crate::parser::ast::TraitItem::Method(m) = &it.node {
                            self.scan_body(&m.body);
                        }
                    }
                }
                Item::Test(t) => self.visit_block(&t.body),
                Item::Bench(b) => self.visit_block(&b.body),
                Item::SuiteSetup(s) => self.visit_block(&s.body),
                Item::SuiteTeardown(s) => self.visit_block(&s.body),
                // `Item::Module` wrappers are already flattened away before this
                // pass (see `flat_module` in `lower_module`); recurse defensively
                // in case an unflattened list is ever passed.
                Item::Module { items, .. } => self.scan_items(items),
                _ => {}
            }
        }
    }

    fn scan_body(&mut self, body: &FunctionBody) {
        use crate::parser::visitor::ExprVisitor;
        match body {
            FunctionBody::Block(b) => self.visit_block(b),
            FunctionBody::Expression(e) => self.visit_expr(e),
            FunctionBody::Declaration | FunctionBody::Extern(_) => {}
        }
    }

    /// Peel index / field / tuple-field / deref projections to the base
    /// identifier and, if it resolves to a module-level static, return its
    /// `DefId`. `STATIC`, `STATIC[i]`, `STATIC.f`, `STATIC[i].f` all root at
    /// the same static.
    fn root_static(&self, expr: &crate::span::Spanned<crate::parser::ast::Expr>) -> Option<crate::semantic::ids::DefId> {
        use crate::parser::ast::Expr;
        let mut cur = expr;
        loop {
            match &cur.node {
                Expr::Index { object, .. }
                | Expr::FieldAccess { object, .. }
                | Expr::TupleFieldAccess { object, .. } => cur = object,
                Expr::Deref { expr } => cur = expr,
                Expr::Identifier(_) => {
                    let def_id = *self.analysis.resolution_map.get(&cur.span.start)?;
                    if self.analysis.scopes.get_def(def_id).kind
                        == crate::semantic::scope::DefKind::Static
                    {
                        return Some(def_id);
                    }
                    return None;
                }
                _ => return None,
            }
        }
    }

    fn mark(&mut self, expr: &crate::span::Spanned<crate::parser::ast::Expr>) {
        if let Some(d) = self.root_static(expr) {
            self.mutated.insert(d);
        }
    }

    /// `&`/`!` sigils on call args carry ownership separately from the
    /// `MutableBorrow`/`Move` wrapper exprs; check both.
    fn mark_owning_args(&mut self, args: &[crate::span::Spanned<crate::parser::ast::CallArg>]) {
        use crate::parser::ast::Ownership;
        for a in args {
            if matches!(a.node.ownership, Ownership::MutableBorrow | Ownership::Move) {
                self.mark(&a.node.value);
            }
        }
    }
}

impl<'a> crate::parser::visitor::ExprVisitor for StaticMutationScan<'a> {
    fn visit_expr(&mut self, expr: &crate::span::Spanned<crate::parser::ast::Expr>) {
        use crate::parser::ast::Expr;
        match &expr.node {
            // (c)/(d): `&STATIC` write-through borrow, `!STATIC` move/consume.
            Expr::MutableBorrow { expr: inner } | Expr::Move { expr: inner } => {
                self.mark(inner);
            }
            // (a): a method call on a static receiver. CONSERVATIVE: block on
            // ANY method (a sound over-approximation of the mutating-method
            // class — a precise builtin-`is_mutating` + user-`&self`/`!self`
            // classification is interprocedural and unsound to name-guess; over-
            // blocking only keeps a static imperative, never miscompiles). The
            // five target tables are `for`-iterated, never method-called, so
            // this costs them nothing.
            Expr::MethodCall { receiver, args, .. } => {
                self.mark(receiver);
                self.mark_owning_args(args);
            }
            Expr::Call { args, .. } => {
                self.mark_owning_args(args);
            }
            Expr::DotShorthand { args, .. } => {
                self.mark_owning_args(args);
            }
            // f-string interpolations: the default `walk_expr` only visits a
            // fake identifier per segment and DROPS the parsed interpolation
            // expressions — so a mutation inside `f"{STATIC.pop()}"` would be
            // missed. Walk the real parsed exprs for soundness.
            Expr::StringLiteral(_, parsed) => {
                for e in parsed {
                    self.visit_expr(e);
                }
            }
            _ => {}
        }
        crate::parser::visitor::walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &crate::span::Spanned<crate::parser::ast::Stmt>) {
        use crate::parser::ast::{Ownership, Stmt};
        match &stmt.node {
            // (b): assign / compound-assign whose target ROOT is a static
            // (incl. index/field projections `STATIC[i] = x`, `STATIC.f = x`).
            Stmt::Assign { target, .. } | Stmt::CompoundAssign { target, .. } => {
                self.mark(target);
            }
            // (c): `for x in &STATIC` / `for x in !STATIC` — `&` is write-through
            // exclusive access, `!` consumes. Plain `for x in STATIC` (shared
            // borrow) is a read and does NOT block.
            Stmt::For { ownership, iterable, .. } => {
                if matches!(ownership, Ownership::MutableBorrow | Ownership::Move) {
                    self.mark(iterable);
                }
            }
            _ => {}
        }
        crate::parser::visitor::walk_stmt(self, stmt);
    }
}

// `is_literal_arg` was the gate before the compound-literal path emitted
// a string `RuntimeCall(...)`. The new `literal_to_global_init`
// returns `None` directly for non-literals — the caller checks via
// `Option::collect` instead, which makes `is_literal_arg` dead code.

/// Spell the C type name for the Nth generic arg of `ty` when that arg is
/// used as the element-size operand of a runtime collection constructor
/// (`gorget_dict_new(sizeof(K), sizeof(V))`, `gorget_array_new(sizeof(E))`).
///
/// A nested collection value (`Dict[int, Vector[int]]`'s `Vector[int]`) is a
/// `Type::Named { name: "Vector", .. }`; `generic_nth_c_type` would return the
/// surface name `"Vector"` verbatim, which is not a declared C type — the
/// emitted `sizeof(Vector)` is a C compile error (and on LLVM a silent
/// size-8 truncation). The runtime stores every collection by its heap
/// handle struct, so the element size is the size of that struct.
///
/// The collection→runtime-struct mapping reads the TYPED
/// `BuiltinTypeProtocol.collection_kind` (the same axis
/// `register_collection_alias` uses, `src/ir/lowering/types.rs:772`) — NOT a
/// name-prefix test. Non-collection heads fall through to
/// `generic_nth_c_type`, which already handles primitives and plain named
/// types. (`generic_elem_c_type`'s Mutex/RWLock mangled-symbol path is a
/// DIFFERENT axis and is intentionally left untouched.)
fn collection_arg_sizeof_c_type(ty: &crate::parser::ast::Type, n: usize) -> String {
    use crate::parser::ast::Type;
    if let Type::Named { generic_args, .. } = ty {
        if let Some(arg) = generic_args.get(n) {
            if let Type::Named { name, .. } = &arg.node {
                if let Some(protocol) = builtins::lookup_protocol(name.node.as_str()) {
                    if let Some(kind) = protocol.collection_kind {
                        return match kind {
                            CollectionKind::Array => "GorgetArray",
                            CollectionKind::Map | CollectionKind::OrderedMap => "GorgetMap",
                            CollectionKind::Set | CollectionKind::OrderedSet => "GorgetSet",
                        }
                        .to_string();
                    }
                }
            }
        }
    }
    generic_nth_c_type(ty, n)
}

/// Extract the Nth generic argument's C type name (0-indexed).
fn generic_nth_c_type(ty: &crate::parser::ast::Type, n: usize) -> String {
    use crate::parser::ast::{PrimitiveType, Type};
    if let Type::Named { generic_args, .. } = ty {
        if let Some(arg) = generic_args.get(n) {
            return match &arg.node {
                Type::Primitive(p) => match p {
                    PrimitiveType::Int | PrimitiveType::Int64 => "int64_t".to_string(),
                    PrimitiveType::Float | PrimitiveType::Float64 => "double".to_string(),
                    PrimitiveType::Bool  => "bool".to_string(),
                    PrimitiveType::StringType => "GorgetString".to_string(),
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
                    PrimitiveType::StringType => "GorgetString".to_string(),
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
        // Name filter: skip if test name doesn't match.
        // Pipe-separated values (from --last-failed) use exact matching;
        // a single value uses substring matching.
        if let Some(ref filter) = options.test_name_filter {
            if filter.contains('|') {
                // Exact match against any of the pipe-separated names
                if !filter.split('|').any(|name| name == test_def.name.node) {
                    return false;
                }
            } else if !test_def.name.node.contains(filter.as_str()) {
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
            ctx.func_state.liveness = liveness::compute_function_liveness(&setup.body.stmts);
            stmts::lower_block(ctx, &mut builder, &setup.body);
            let last = builder.current_block.0 as usize;
            if builder.blocks[last].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }
            ctx.flush_ownership_to_locals(&mut builder);
            module.functions.push(builder.build());
            module.runtime.has_suite_setup = true;
        }
    }

    // Lower suite teardown as __suite_teardown() void function.
    for item in &ast_module.items {
        if let Item::SuiteTeardown(teardown) = &item.node {
            let mut builder = FunctionBuilder::new("__suite_teardown", UNIT_TYPE, &[]);
            ctx.clear_locals();
            ctx.drops.push_scope(drops::DropScopeKind::Function);
            ctx.func_state.liveness = liveness::compute_function_liveness(&teardown.body.stmts);
            stmts::lower_block(ctx, &mut builder, &teardown.body);
            let last = builder.current_block.0 as usize;
            if builder.blocks[last].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }
            ctx.flush_ownership_to_locals(&mut builder);
            module.functions.push(builder.build());
            module.runtime.has_suite_teardown = true;
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

            // Extract @skip metadata — skipped tests are reported but not executed
            let skipped = test_def.attributes.iter()
                .any(|a| a.node.name.node == "skip");
            let skip_reason: Option<String> = test_def.attributes.iter()
                .find(|a| a.node.name.node == "skip")
                .and_then(|a| a.node.args.first())
                .and_then(|arg| {
                    if let AttributeArg::StringLiteral(s) = arg { Some(s.clone()) } else { None }
                });

            // Extract @timeout metadata — per-test timeout in milliseconds
            let timeout_ms: Option<u64> = test_def.attributes.iter()
                .find(|a| a.node.name.node == "timeout")
                .and_then(|a| a.node.args.first())
                .and_then(|arg| match arg {
                    AttributeArg::StringLiteral(s) => s.parse::<u64>().ok(),
                    _ => None,
                })
                .or(options.default_timeout_ms);

            // Skipped tests: register metadata only, don't lower body
            if skipped {
                module.runtime.test_fns.push(TestFnInfo {
                    fn_name: String::new(),
                    display_name: test_name,
                    should_panic,
                    expected_panic_msg,
                    skipped: true,
                    skip_reason,
                    timeout_ms: None,
                });
                continue;
            }

            // Build the test function (no parameters — with-bindings use body-level `with` blocks).
            let mut builder = FunctionBuilder::new(&fn_name, UNIT_TYPE, &[]);
            ctx.clear_locals();
            ctx.drops.push_scope(drops::DropScopeKind::Function);
            // Match scrutinee staging consults `is_last_use_at` to decide
            // whether to transfer drop ownership from a named source to the
            // staged scrutinee. Without function-level liveness data (Snag
            // #25d), it stays conservative — the source's drop fires at
            // scope exit, double-freeing payloads an arm's `unwrap()`
            // already extracted via the aliased scrutinee. Same call as
            // the regular function lowering path in `functions.rs`.
            ctx.func_state.liveness = liveness::compute_function_liveness(&test_def.body.stmts);

            stmts::lower_block(ctx, &mut builder, &test_def.body);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }

            ctx.flush_ownership_to_locals(&mut builder);
            let mut test_fn = builder.build();
            test_fn.is_test_fn = true;
            module.functions.push(test_fn);
            module.runtime.test_fns.push(TestFnInfo {
                fn_name,
                display_name: test_name,
                should_panic,
                expected_panic_msg,
                skipped: false,
                skip_reason: None,
                timeout_ms,
            });
        }
    }

    // --failed-first: reorder so previously-failed tests run first
    if !options.failed_first_names.is_empty() {
        let failed_set: std::collections::HashSet<&str> = options.failed_first_names.iter().map(|s| s.as_str()).collect();
        module.runtime.test_fns.sort_by_key(|t| {
            if failed_set.contains(t.display_name.as_str()) { 0 } else { 1 }
        });
    }
}

/// Lower bench items into benchmark functions.
fn lower_bench_items(
    ctx: &mut LoweringContext,
    module: &mut Module,
    ast_module: &ast::Module,
    options: &LoweringOptions,
) {
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::BenchFnInfo;

    for (idx, item) in ast_module.items.iter().enumerate() {
        if let Item::Bench(bench_def) = &item.node {
            // Apply name filter if present
            if let Some(ref filter) = options.test_name_filter {
                if !bench_def.name.node.contains(filter.as_str()) {
                    continue;
                }
            }

            let fn_name = format!("__bench_{idx}");
            let bench_name = bench_def.name.node.clone();

            let mut builder = FunctionBuilder::new(&fn_name, UNIT_TYPE, &[]);
            ctx.clear_locals();
            ctx.drops.push_scope(drops::DropScopeKind::Function);
            ctx.func_state.liveness = liveness::compute_function_liveness(&bench_def.body.stmts);

            stmts::lower_block(ctx, &mut builder, &bench_def.body);

            let last_block_idx = builder.current_block.0 as usize;
            if builder.blocks[last_block_idx].terminator.is_none() {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
                builder.ret(FunctionBuilder::const_unit());
            } else {
                ctx.drops.pop_scope(&mut builder, &ctx.type_registry);
            }

            ctx.flush_ownership_to_locals(&mut builder);
            let mut bench_fn = builder.build();
            bench_fn.is_test_fn = true; // enable cleanup stack
            module.functions.push(bench_fn);
            module.runtime.bench_fns.push(BenchFnInfo {
                fn_name,
                display_name: bench_name,
            });
        }
    }
}

/// Scan types for droppable fields and upgrade to Resource+Recursive.
/// Called twice: once early (for types registered during type setup) and once
/// late (for types registered lazily during function lowering, e.g. Option[Vector[int]]).
fn upgrade_types_from_fields(module: &mut Module) {
    use crate::ir::types::*;

    // Fixed-point iteration: upgrading Option[String] to Recursive may cause
    // structs containing Option[String] to need upgrading too. Re-scan until
    // no new types are upgraded.
    loop {
        let mut upgraded_any = false;

        // Collect which type names currently need dropping (re-collected each iteration
        // so newly-upgraded types are visible to dependents).
        let droppable_names: Vec<String> = module.type_registry.all_type_def_names()
            .filter(|name| {
                if let Some(td) = module.type_registry.get_type_def(name) {
                    td.metadata.copy_semantics == CopySemantics::Resource
                        || td.metadata.drop_strategy != DropStrategy::None
                } else {
                    false
                }
            })
            .cloned()
            .collect();

        // Scan all types for fields whose type is droppable
        let all_names: Vec<String> = module.type_registry.all_type_def_names().cloned().collect();
        for name in &all_names {
            let needs_upgrade = {
                let td = match module.type_registry.get_type_def(name) {
                    Some(td) => td,
                    None => continue,
                };
                // Skip if already has a non-None drop strategy
                if td.metadata.drop_strategy != DropStrategy::None {
                    if matches!(td.metadata.drop_strategy, DropStrategy::Custom(_)) {
                        if let TypeDefKind::Struct(ref sdef) = td.kind {
                            sdef.fields.iter().any(|f| {
                                if let Some(GirType::Named(field_type_name)) = module.type_registry.get(f.type_id) {
                                    droppable_names.contains(field_type_name) || field_type_name == "GorgetString" || module.type_registry.is_collection_type_name(field_type_name)
                                } else { false }
                            })
                        } else { false }
                    } else {
                        continue; // Already Trivial or Recursive
                    }
                } else if let TypeDefKind::Struct(ref sdef) = td.kind {
                    sdef.fields.iter().any(|f| {
                        if let Some(GirType::Named(field_type_name)) = module.type_registry.get(f.type_id) {
                            droppable_names.contains(field_type_name) || field_type_name == "GorgetString" || module.type_registry.is_collection_type_name(field_type_name)
                        } else { false }
                    })
                } else if let TypeDefKind::Enum(ref edef) = td.kind {
                    // Enums (including Option/Result) with resource-type variant payloads
                    edef.variants.iter().any(|v| {
                        v.fields.iter().any(|f| {
                            if let Some(GirType::Named(field_type_name)) = module.type_registry.get(f.type_id) {
                                droppable_names.contains(field_type_name) || field_type_name == "GorgetString" || module.type_registry.is_collection_type_name(field_type_name)
                            } else { false }
                        })
                    })
                } else {
                    false
                }
            };

            if needs_upgrade {
                if let Some(td) = module.type_registry.get_type_def_mut(name) {
                    if td.metadata.drop_strategy == DropStrategy::None {
                        td.metadata.drop_strategy = DropStrategy::Recursive;
                        upgraded_any = true;
                    }
                    if td.metadata.copy_semantics != CopySemantics::Resource {
                        td.metadata.copy_semantics = CopySemantics::Resource;
                        upgraded_any = true;
                    }
                }
            }
        }

        if !upgraded_any { break; }
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
            param_abis: vec![],
            returns_borrowed: false,
        });
    }
}

/// Detect equip methods whose body is a single return of `self.field[idx]` or `self.field`.
/// These are candidates for clone elision — returning Ptr(T) instead of cloning the element.
fn is_trivial_getter_body(method: &ast::FunctionDef) -> bool {
    use crate::parser::ast::{Expr, Ownership, Stmt};

    // Must have non-consuming self parameter
    let has_borrow_self = method.params.first()
        .map(|p| p.node.name.node == "self" && !matches!(p.node.ownership, Ownership::Move))
        .unwrap_or(false);
    if !has_borrow_self {
        return false;
    }

    // Extract the return expression
    let ret_expr = match &method.body {
        FunctionBody::Block(block) => {
            if block.stmts.len() != 1 { return false; }
            match &block.stmts[0].node {
                Stmt::Return(Some(expr)) => &expr.node,
                _ => return false,
            }
        }
        FunctionBody::Expression(expr) => &expr.node,
        _ => return false,
    };

    // Match: self.field[idx]
    if let Expr::Index { object, .. } = ret_expr {
        if let Expr::FieldAccess { object: obj, .. } = &object.node {
            if matches!(&obj.node, Expr::SelfExpr) {
                return true;
            }
        }
    }

    // Match: self.field
    if let Expr::FieldAccess { object, .. } = ret_expr {
        if matches!(&object.node, Expr::SelfExpr) {
            return true;
        }
    }

    false
}

/// Pre-scan equip blocks for trivial getter methods.
/// Populates `ctx.trivial_getter_methods` with mangled names of methods
/// that can return Ptr(T) instead of cloning.
fn populate_trivial_getter_methods(
    ctx: &mut LoweringContext,
    ast_module: &ast::Module,
) {
    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            if equip.generic_params.is_some() || equip.trait_.is_some() {
                continue;
            }
            if let Some(type_name) = types::equip_target_name(&equip.type_.node) {
                for method in &equip.items {
                    if !is_trivial_getter_body(&method.node) { continue; }
                    let ret_type = ctx.type_mapper.map_ast_type(&method.node.return_type.node);
                    if ctx.type_registry.is_resource_type(ret_type) {
                        let mangled = format!("{}__{}", type_name, method.node.name.node);
                        ctx.trivial_getter_methods.insert(mangled);
                    }
                }
            }
        }
    }
}

/// Populate `gir_equip_methods` in a single pass over the AST.
///
/// A method is "GIR-lowered" if it has a Block or Expression body (not Extern or Declaration).
/// The set is keyed by the name the caller resolves to (`Type__method` for plain/non-vtable,
/// `Trait_for_Type__method` for vtable methods, and mangled generic names).
fn populate_gir_equip_methods(
    ctx: &mut LoweringContext,
    ast_module: &ast::Module,
    generic_collector: &GenericCollector,
) {
    let is_gir_body = |body: &FunctionBody| {
        matches!(body, FunctionBody::Block(_) | FunctionBody::Expression(_))
    };

    for item in &ast_module.items {
        if let Item::Equip(equip) = &item.node {
            if equip.generic_params.is_some() {
                continue; // generic equip handled below via GenericCollector
            }

            let type_name = match types::equip_target_name(&equip.type_.node) {
                Some(n) => n,
                None => continue, // generic — handled via monomorphization
            };

            for method in &equip.items {
                if !is_gir_body(&method.node.body) {
                    continue;
                }
                // Type__method — used for plain equip and non-vtable trait equip methods
                ctx.gir_equip_methods.insert(format!("{type_name}__{}", method.node.name.node));
            }

            // For trait equip blocks, also register Trait_for_Type__method names
            if let Some(trait_ref) = &equip.trait_ {
                let trait_name = traits::extract_trait_name(&trait_ref.trait_name.node);
                if !trait_name.is_empty() {
                    for method in &equip.items {
                        if !is_gir_body(&method.node.body) {
                            continue;
                        }
                        ctx.gir_equip_methods.insert(
                            format!("{trait_name}_for_{type_name}__{}", method.node.name.node)
                        );
                    }
                    // Default trait methods (not overridden) are always GIR-lowered
                    let implemented: Vec<&str> = equip.items.iter()
                        .map(|m| m.node.name.node.as_str())
                        .collect();
                    if let Some(trait_def) = ast_module.items.iter().find_map(|i| {
                        if let Item::Trait(td) = &i.node {
                            if td.name.node == trait_name { Some(td) } else { None }
                        } else { None }
                    }) {
                        for trait_item in &trait_def.items {
                            if let ast::TraitItem::Method(m) = &trait_item.node {
                                if !implemented.contains(&m.name.node.as_str()) && is_gir_body(&m.body) {
                                    ctx.gir_equip_methods.insert(
                                        format!("{trait_name}_for_{type_name}__{}", m.name.node)
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Generic equip methods
    for name in generic_collector.gir_equip_method_names() {
        ctx.gir_equip_methods.insert(name);
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
    let mut sigs_to_add: Vec<(String, Vec<TypeId>, TypeId, Vec<context::ParamABI>)> = Vec::new();

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

            use context::ParamABI::*;
            let self_param = vec![self_ptr];
            sigs_to_add.push((format!("{mangled_name}__unwrap"), self_param.clone(), inner_type, vec![ByPtr]));
            sigs_to_add.push((format!("{mangled_name}__expect"), vec![self_ptr, ctx.type_mapper.owned_string_type], inner_type, vec![ByPtr, ByValue]));
            sigs_to_add.push((format!("{mangled_name}__unwrap_or"), vec![self_ptr, inner_type], inner_type, vec![ByPtr, ByValue]));
            sigs_to_add.push((format!("{mangled_name}__is_some"), self_param.clone(), BOOL_TYPE, vec![ByPtr]));
            sigs_to_add.push((format!("{mangled_name}__is_none"), self_param, BOOL_TYPE, vec![ByPtr]));
        } else if base_name == "Result" {
            let ok_type = variants.iter()
                .find(|v| v.name == "Ok")
                .and_then(|v| v.fields.first())
                .map(|f| f.type_id)
                .unwrap_or(I64_TYPE);

            use context::ParamABI::*;
            let self_param = vec![self_ptr];
            sigs_to_add.push((format!("{mangled_name}__unwrap"), self_param.clone(), ok_type, vec![ByPtr]));
            sigs_to_add.push((format!("{mangled_name}__expect"), vec![self_ptr, ctx.type_mapper.owned_string_type], ok_type, vec![ByPtr, ByValue]));
            sigs_to_add.push((format!("{mangled_name}__unwrap_or"), vec![self_ptr, ok_type], ok_type, vec![ByPtr, ByValue]));
            sigs_to_add.push((format!("{mangled_name}__is_ok"), self_param.clone(), BOOL_TYPE, vec![ByPtr]));
            sigs_to_add.push((format!("{mangled_name}__is_err"), self_param, BOOL_TYPE, vec![ByPtr]));
        }
    }

    for (name, params, ret, abis) in sigs_to_add {
        ctx.fn_sigs.insert(name.clone(), (params, ret));
        ctx.fn_param_abis.insert(name, abis);
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

        // Helper: insert fn_sigs + fn_param_abis + fn_param_ownerships.
        // `move_indices` marks which non-self params take ownership (indices into params, 1-based).
        // User equip blocks take precedence — if a signature is already registered,
        // leave it alone (e.g. a user-defined `iter()` returning VectorIter[T]).
        let mut insert_collection_sig = |name: String, params: Vec<TypeId>, ret: TypeId, move_indices: &[usize]| {
            if ctx.fn_sigs.contains_key(&name) {
                return;
            }
            let abis: Vec<context::ParamABI> = params.iter().enumerate()
                .map(|(i, _)| if i == 0 { context::ParamABI::ByPtr } else { context::ParamABI::ByValue })
                .collect();
            let ownerships: Vec<ast::Ownership> = params.iter().enumerate()
                .map(|(i, _)| {
                    if i == 0 { ast::Ownership::MutableBorrow }
                    else if move_indices.contains(&i) { ast::Ownership::Move }
                    else { ast::Ownership::Borrow }
                })
                .collect();
            ctx.fn_sigs.insert(name.clone(), (params, ret));
            ctx.fn_param_abis.insert(name.clone(), abis);
            ctx.fn_param_ownerships.insert(name, ownerships);
        };

        match base_name {
            "Vector" => {
                // (name, params, return_type, move_indices)
                // move_indices: 1-based param indices that take ownership
                let sigs: Vec<(String, Vec<TypeId>, TypeId, &[usize])> = vec![
                    (format!("{mangled_name}__push"), vec![self_ptr, elem_type], UNIT_TYPE, &[1]),    // elem consumed
                    (format!("{mangled_name}__pop"), vec![self_ptr], elem_type, &[]),
                    (format!("{mangled_name}__get"), vec![self_ptr, I64_TYPE], elem_type, &[]),
                    (format!("{mangled_name}__set"), vec![self_ptr, I64_TYPE, elem_type], UNIT_TYPE, &[2]),  // elem consumed
                    (format!("{mangled_name}__len"), vec![self_ptr], I64_TYPE, &[]),
                    (format!("{mangled_name}__contains"), vec![self_ptr, elem_type], BOOL_TYPE, &[]),
                    (format!("{mangled_name}__remove"), vec![self_ptr, I64_TYPE], elem_type, &[]),
                    (format!("{mangled_name}__insert"), vec![self_ptr, I64_TYPE, elem_type], UNIT_TYPE, &[2]),  // elem consumed
                    (format!("{mangled_name}__clear"), vec![self_ptr], UNIT_TYPE, &[]),
                    (format!("{mangled_name}__clone"), vec![self_ptr], array_type, &[]),
                    (format!("{mangled_name}__iter"), vec![self_ptr], array_type, &[]),
                ];
                for (name, params, ret, moves) in sigs {
                    insert_collection_sig(name, params, ret, moves);
                }
            }
            "Dict" | "HashMap" => {
                // Dict/HashMap have key and value types
                // (GorgetDict/GorgetMap are opaque C types, not struct-based in GIR)
                let sigs: Vec<(String, Vec<TypeId>, TypeId, &[usize])> = vec![
                    (format!("{mangled_name}__len"), vec![self_ptr], I64_TYPE, &[]),
                    (format!("{mangled_name}__contains"), vec![self_ptr, I64_TYPE], BOOL_TYPE, &[]),
                    (format!("{mangled_name}__clear"), vec![self_ptr], UNIT_TYPE, &[]),
                ];
                for (name, params, ret, moves) in sigs {
                    insert_collection_sig(name, params, ret, moves);
                }
            }
            "Set" | "HashSet" => {
                let sigs: Vec<(String, Vec<TypeId>, TypeId, &[usize])> = vec![
                    (format!("{mangled_name}__add"), vec![self_ptr, elem_type], UNIT_TYPE, &[1]),    // elem consumed
                    (format!("{mangled_name}__contains"), vec![self_ptr, elem_type], BOOL_TYPE, &[]),
                    (format!("{mangled_name}__remove"), vec![self_ptr, elem_type], BOOL_TYPE, &[]),
                    (format!("{mangled_name}__len"), vec![self_ptr], I64_TYPE, &[]),
                    (format!("{mangled_name}__clear"), vec![self_ptr], UNIT_TYPE, &[]),
                ];
                for (name, params, ret, moves) in sigs {
                    insert_collection_sig(name, params, ret, moves);
                }
            }
            _ => {}
        }

        // Register sentinel-to-Option wrapping for find/index_of on collections
        if matches!(base_name, "Vector" | "Dict" | "HashMap" | "Set" | "HashSet") {
            ctx.sentinel_to_option_methods.insert(format!("{mangled_name}__find"));
            ctx.sentinel_to_option_methods.insert(format!("{mangled_name}__index_of"));
        }
    }

    // Register sentinel-to-Option for Str/Bytes/GorgetString/GorgetBytes/GorgetArray builtins
    for base in &["GorgetString", "Bytes", "GorgetBytes", "GorgetArray"] {
        ctx.sentinel_to_option_methods.insert(format!("{base}__find"));
        ctx.sentinel_to_option_methods.insert(format!("{base}__find_from"));
        ctx.sentinel_to_option_methods.insert(format!("{base}__find_ext"));
        ctx.sentinel_to_option_methods.insert(format!("{base}__index_of"));
    }
}

/// Evaluate a compile-time constant expression (for `const` and `meta` declarations).
pub(crate) fn eval_const_expr(
    expr: &ast::Expr,
    known: &rustc_hash::FxHashMap<String, crate::ir::instructions::Constant>,
) -> Option<crate::ir::instructions::Constant> {
    use crate::ir::instructions::Constant;
    use crate::parser::ast::{BinaryOp, Expr};

    match expr {
        Expr::IntLiteral(v) => Some(Constant::I64(*v)),
        Expr::FloatLiteral(v) => Some(Constant::F64(*v)),
        Expr::BoolLiteral(v) => Some(Constant::Bool(*v)),
        Expr::StringLiteral(lit, _) => {
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
                    BinaryOp::Rem if b != 0 => Some(Constant::I64(a % b)),
                    BinaryOp::Mod if b != 0 => {
                        let r = a % b;
                        Some(Constant::I64(if r != 0 && ((r ^ b) < 0) { r + b } else { r }))
                    }
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

    /// Generate C code from a GIR module via the LIR → BIR pipeline.
    fn gir_to_lir_c(gir: &crate::ir::Module) -> String {
        let mut lir_module = crate::lir::lower::lower_module(gir);
        // Tier E §8.2: critical-edge split before SSA construction.
        crate::lir::split_edges::split_critical_edges_module(&mut lir_module);
        // Tier E §8.3: per-pass invariant validation hooks, mirroring the
        // production pipeline in `src/main.rs`.
        crate::lir::validate::assert_module_valid(&lir_module, "lir-lowering");
        for func in &mut lir_module.functions {
            crate::lir::ssa::construct_ssa(func);
        }
        crate::lir::validate::assert_module_valid(&lir_module, "ssa-construction");
        crate::lir::types::wire_collection_bridges(&mut lir_module);
        crate::lir::validate::assert_module_valid(&lir_module, "wire-collection-bridges");
        crate::lir::runtime::promote_runtime_calls(&mut lir_module);
        crate::lir::validate::assert_module_valid(&lir_module, "promote-runtime-calls");
        crate::lir::types::compute_module_value_types(&mut lir_module);
        crate::lir::validate::assert_module_valid(&lir_module, "compute-value-types-pre-bir");
        let mut bir_module = crate::bir::BirModule::from_lir(lir_module)
            .expect("BIR lowering failed in gir_to_lir_c test helper");
        crate::lir::validate::assert_module_valid(bir_module.as_lir(), "bir-lowering");
        crate::lir::optimize::optimize_module(bir_module.as_lir_mut());
        crate::lir::validate::assert_module_valid(bir_module.as_lir(), "optimize");
        crate::lir::types::compute_module_value_types(bir_module.as_lir_mut());
        crate::lir::validate::assert_module_valid(bir_module.as_lir(), "compute-value-types-post-bir");
        let backend = crate::backend::c_lir::CLirBackend;
        crate::backend::Backend::generate(&backend, &bir_module).code
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
        let source = r#"int double(int x): x * 2

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
        let c_code = gir_to_lir_c(&gir);

        assert!(c_code.contains("Point"), "Should reference Point struct");
        assert!(c_code.contains("int64_t x;"), "Should have field x");
        assert!(c_code.contains("int64_t y;"), "Should have field y");
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
        let c_code = gir_to_lir_c(&gir);

        assert!(c_code.contains("Pair__int64_t__double"),
            "C output should contain monomorphized Pair type name. C code:\n{c_code}");
    }

    #[test]
    fn lower_generic_function_monomorphized() {
        let source = r#"T identity[T](T x): x

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

        // Verify VTable struct fields (2 method slots + the trailing __drop glue slot)
        let vtable_def = gir.type_registry.get_type_def("Shape_VTable").unwrap();
        if let TypeDefKind::Struct(ref s) = vtable_def.kind {
            assert_eq!(s.fields.len(), 3, "VTable should have 2 method slots + __drop");
            assert_eq!(s.fields[0].name, "area");
            assert_eq!(s.fields[1].name, "draw");
            assert_eq!(s.fields[2].name, traits::VTABLE_DROP_FIELD);
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
            assert_eq!(fields.len(), 3, "VTable should have 2 method entries + __drop");
            assert_eq!(fields[0].0, "area");
            assert_eq!(fields[1].0, "draw");
            assert_eq!(fields[2].0, traits::VTABLE_DROP_FIELD);
            // Verify FnRef entries point to correct functions
            if let crate::ir::GlobalInit::FnRef(fn_name) = &fields[0].1 {
                assert_eq!(fn_name, "Shape_for_Circle__area");
            } else {
                panic!("Expected FnRef for area slot");
            }
            // The drop slot carries the typed concrete inner name.
            if let crate::ir::GlobalInit::BoxDropRef(inner) = &fields[2].1 {
                assert_eq!(inner, "Circle");
            } else {
                panic!("Expected BoxDropRef for __drop slot");
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
        let c_code = gir_to_lir_c(&gir);

        // VTable type
        assert!(c_code.contains("Shape_VTable"),
            "Should have Shape_VTable. C code:\n{c_code}");
        // Function pointer field: `(*area)`
        assert!(c_code.contains("(*area)"),
            "Should have function pointer field 'area'. C code:\n{c_code}");

        // TraitObj type
        assert!(c_code.contains("Shape_TraitObj"),
            "Should have Shape_TraitObj. C code:\n{c_code}");

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
            destructure: None,
        })];

        let body = Spanned::dummy(ast::Expr::BinaryOp {
            left: Box::new(Spanned::dummy(ast::Expr::Identifier("x".to_string()))),
            op: ast::BinaryOp::Add,
            right: Box::new(Spanned::dummy(ast::Expr::Identifier("y".to_string()))),
        });

        let operand = closures.lower_closure(&mut ctx, &mut builder, &params, &body, false, crate::span::Span::new(0, 0));
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
