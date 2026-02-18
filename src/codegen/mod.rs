/// C code generation backend for the Gorget compiler.
pub mod c_emitter;
pub mod c_expr;
pub mod c_expr_call;
pub mod c_expr_closure;
pub mod c_expr_generic;
pub mod c_expr_methods;
pub mod c_expr_pattern;
pub mod c_expr_print;
pub mod c_item;
pub mod c_mangle;
pub mod c_runtime;
pub mod c_stmt;
pub mod c_types;

use std::collections::HashSet;

use rustc_hash::FxHashMap;

use crate::parser::ast::Module;
use crate::parser::ast::Type;
use crate::semantic::ids::{DefId, TypeId};
use crate::semantic::resolve::{EnumVariantInfo, FunctionInfo, ResolutionMap, StructFieldInfo};
use crate::semantic::scope::ScopeTable;
use crate::semantic::types::TypeTable;
use crate::semantic::traits::TraitRegistry;
use crate::semantic::AnalysisResult;
use crate::span::Span;

use crate::parser::ast::{EnumDef, EquipBlock, FunctionBody, FunctionDef, Item, StructDef};
use c_emitter::CEmitter;

/// A registered generic instantiation.
#[derive(Clone)]
pub struct GenericInstance {
    pub base_name: String,
    pub mangled_name: String,
    pub c_type_args: Vec<String>,
    pub kind: GenericInstanceKind,
}

/// The kind of generic definition being instantiated.
#[derive(Clone)]
pub enum GenericInstanceKind {
    Struct,
    Enum,
    Function,
    Map { ordered: bool },
}

/// The kind of drop scope, determining when cleanup runs.
#[derive(Clone, Copy, PartialEq)]
pub enum DropScopeKind {
    Function,
    Loop,
    Block,
}

/// A variable that needs cleanup when its scope exits.
#[derive(Clone)]
pub struct DropEntry {
    pub var_name: String,
    pub action: DropAction,
}

/// How to drop a variable.
#[derive(Clone)]
pub enum DropAction {
    /// free(var) for Box[T]
    BoxFree,
    /// free(var.data) for trait objects (Box[Trait]).
    /// If the trait's vtable has a `drop` slot (the trait extends Drop or
    /// includes a `drop` method), dispatch it before freeing.
    TraitObjFree { has_drop: bool },
    /// gorget_file_close(&var) for File
    FileClose,
    /// Drop_for_T__drop(&var) for user-defined Drop
    UserDrop { type_name: String },
    /// gorget_string_free(&var) for owned String
    StringFree,
    /// free(var.env) for heap-allocated closure environments
    ClosureEnvFree,
    /// Compiler-generated drop glue: user drop (if any) + per-field drops.
    StructDrop {
        type_name: String,
        has_user_drop: bool,
        field_drops: Vec<(String, DropAction)>,
    },
}

/// How a closure captures a variable from its enclosing scope.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CaptureMode {
    /// Copy the value into the closure environment.
    ByValue,
    /// Store a pointer to the outer variable so mutations propagate.
    ByMutRef,
}

/// A closure that has been lifted to a top-level function.
pub struct LiftedClosure {
    pub id: usize,
    /// The unique struct name for this closure (e.g., `__Closure_0`).
    pub struct_name: String,
    /// Captured variable names, their C types, and capture mode.
    pub captures: Vec<(String, String, CaptureMode)>,
    /// Parameter names and their C types.
    pub params: Vec<(String, String)>,
    /// The C return type.
    pub return_type: String,
    /// The C expression body.
    pub body: String,
}

/// The kind of callable trait (for vtable generation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableKind {
    Callable,    // Callable[sig] — immutable access
    MutCallable, // MutCallable[sig] — mutable access
    ConsumeCallable, // ConsumeCallable[sig] — consuming access
}

/// Info about a variable that holds a closure or callable trait object.
#[derive(Clone)]
pub enum ClosureVarInfo {
    /// Concrete per-closure struct type — use direct call dispatch.
    Concrete {
        struct_name: String,
        param_c_types: Vec<String>,
        return_c_type: String,
    },
    /// Callable trait object — use vtable dispatch.
    TraitObject {
        param_c_types: Vec<String>,
        return_c_type: String,
        kind: CallableKind,
    },
}

/// An extern function/method binding to a C symbol.
#[derive(Clone)]
pub struct ExternBinding {
    pub c_symbol: String,
    /// For each non-self parameter, whether to pass by reference (&addr_of).
    /// True for struct types (Vector, Dict, opaque types), false for scalars/pointers.
    pub params_need_ref: Vec<bool>,
}

/// Whether an AST type needs to be passed by reference (&) to the C runtime.
/// Primitive types (int, str, bool, float, char, uintN) are scalars/pointers — no ref.
/// Named types (Vector, Dict, Socket, BigNum, etc.) are C structs — need & wrapping.
fn ast_type_needs_ref(ty: &Type) -> bool {
    !matches!(ty, Type::Primitive(_))
}

/// Context threaded through all codegen functions.
pub struct CodegenContext<'a> {
    pub scopes: &'a ScopeTable,
    pub types: &'a TypeTable,
    pub resolution_map: &'a ResolutionMap,
    pub struct_fields: &'a FxHashMap<DefId, StructFieldInfo>,
    pub enum_variants: &'a FxHashMap<DefId, EnumVariantInfo>,
    pub function_info: &'a FxHashMap<DefId, FunctionInfo>,
    pub traits: &'a TraitRegistry,
    pub current_self_type: Option<String>,
    pub current_function_throws: bool,
    /// Closures collected during codegen, emitted in a later pass.
    pub lifted_closures: Vec<LiftedClosure>,
    pub closure_counter: usize,
    /// Generic instantiations registered during codegen.
    pub generic_instances: Vec<GenericInstance>,
    /// Generic struct templates stored for monomorphization.
    pub generic_struct_templates: FxHashMap<String, StructDef>,
    /// Generic enum templates stored for monomorphization.
    pub generic_enum_templates: FxHashMap<String, EnumDef>,
    /// Generic function templates stored for monomorphization.
    pub generic_fn_templates: FxHashMap<String, FunctionDef>,
    /// Equip blocks for generic types, stored for monomorphization.
    pub generic_equip_templates: FxHashMap<String, Vec<EquipBlock>>,
    /// Variables declared with GorgetClosure type (need fn_ptr dispatch on call).
    pub closure_vars: HashSet<String>,
    /// For Fn[sig]-typed variables: maps var name → (param_c_types, return_c_type).
    /// Used for correct fn_ptr cast in GorgetClosure dispatch.
    pub fn_type_signatures: FxHashMap<String, (Vec<String>, String)>,
    /// Per-closure-struct info: maps var name → ClosureVarInfo.
    /// Tracks whether a variable is a concrete closure struct or a trait object.
    pub closure_var_info: FxHashMap<String, ClosureVarInfo>,
    /// Unique Fn-family signatures encountered, for vtable/TraitObj generation.
    /// (kind, param_c_types, return_c_type)
    pub fn_trait_sigs: Vec<(CallableKind, Vec<String>, String)>,
    /// Variables whose auto-inferred array literal was promoted to GorgetArray (Vector).
    pub vector_vars: HashSet<String>,
    /// Variables whose closure environments should be heap-allocated (they escape their scope).
    pub escaping_closure_vars: HashSet<String>,
    /// Flag set during gen_expr for a VarDecl/return whose closure should heap-allocate its env.
    pub closure_heap_alloc: bool,
    /// Registered tuple typedefs: (mangled_name, field_c_types).
    pub tuple_typedefs: Vec<(String, Vec<String>)>,
    /// Hint for the declared type in a VarDecl, used to resolve unit variant
    /// constructors like `None()` to the correct monomorphized enum.
    pub decl_type_hint: Option<crate::parser::ast::Type>,
    /// Stack of drop scopes for RAII cleanup.
    pub drop_scopes: Vec<(DropScopeKind, Vec<DropEntry>)>,
    /// Hint for the C return type of the next closure generated by `gen_closure_expr`.
    /// Set by method codegen (e.g. `map`, `and_then`) before calling `gen_expr` on the
    /// closure argument, consumed via `.take()` inside `gen_closure_expr`.
    pub closure_return_type_hint: Option<String>,
    /// Active generic type substitutions (param_name → C type) during monomorphized
    /// function body codegen. Empty outside of `emit_monomorphized_function`.
    pub type_subs: Vec<(String, String)>,
    /// Active generic type substitutions (param_name → TypeId) during monomorphized
    /// function body codegen. Parallel to `type_subs` but preserves semantic info
    /// for rich formatting (Displayable dispatch, enum printing, etc.).
    pub type_id_subs: Vec<(String, TypeId)>,
    /// Substituted C types for function parameters during monomorphized body codegen.
    /// Maps parameter name → mangled C type (e.g., "p" → "Pair2__int64_t__const_char_ptr").
    /// Used as a fallback when TypeId-based resolution produces Error for generic params.
    pub monomorphized_param_c_types: Vec<(String, String)>,
    /// When true, `assert` statements are stripped from output (no code emitted).
    pub strip_asserts: bool,
    /// When true, integer overflow wraps silently instead of panicking.
    pub overflow_wrap: bool,
    /// When true, emit execution trace instrumentation (JSONL output).
    pub trace: bool,
    /// The Gorget name of the function currently being emitted (for trace return events).
    pub current_function_gorget_name: Option<String>,
    /// The binary stem name, used to construct the trace output filename.
    pub trace_filename: String,
    /// Map from expression span to inferred TypeId (for Result-based `?` codegen).
    pub expr_types: &'a FxHashMap<Span, TypeId>,
    /// C return type of the current function being emitted (for Result `?` early return).
    pub current_function_return_c_type: Option<String>,
    /// Counter for unique `__try_rN` temp variable names in Result `?` codegen.
    pub try_counter: usize,
    /// Counter for unique `__strtmp_N` temp variable names for GorgetString-to-str coercion.
    pub string_temp_counter: usize,
    /// Pending GorgetString temporaries: (temp_name, gorget_string_expr).
    /// Flushed before the containing statement to materialize + register for cleanup.
    pub string_temps: Vec<(String, String)>,
    /// Variables that were moved (`!var`) in the current statement.
    /// After the statement is emitted, `flush_move_zeros` emits `memset` to zero them,
    /// making the scope-exit drop a no-op (prevents double-free).
    pub pending_move_zeros: Vec<String>,
    /// Map from (struct_name, field_name) → AST Type, used to resolve field types
    /// in `infer_receiver_type` for FieldAccess expressions.
    pub field_type_names: FxHashMap<(String, String), Type>,
    /// Variables captured by mutable reference in the current closure body.
    /// When generating expressions inside a closure, identifiers in this set
    /// emit `(*__env->NAME)` instead of bare `NAME`.
    pub mutable_captures: HashSet<String>,
    /// Parameters declared with `Ownership::MutableBorrow` in the current function.
    /// These are emitted as `Type*` pointers and need `(*name)` for reads and
    /// `name->field` for field access.
    pub pointer_params: HashSet<String>,
    /// Maps (type_name, method_name) → extern binding for extern methods.
    /// Maps ("", fn_name) → extern binding for extern free functions.
    pub extern_symbols: FxHashMap<(String, String), ExternBinding>,
    /// Top-level function names (not main, not methods) — these get a `gg_` prefix
    /// in C to avoid collisions with C library symbols.
    pub function_names: HashSet<String>,
    /// Whether this module contains `test` blocks (drives test runner generation).
    pub is_test_module: bool,
    /// Tags to filter which tests run (empty = run all).
    pub test_tag_filter: Vec<String>,
    /// Tags to exclude from test runs (exclusion wins over inclusion).
    pub test_exclude_tags: Vec<String>,
    /// Substring filter for test names (only tests whose name contains this string run).
    pub test_name_filter: Option<String>,
    /// True while generating code inside a `test` block body (for cleanup registration).
    pub in_test_body: bool,
    /// Original Gorget source text, used to extract verbatim source lines for trace events.
    pub source_text: String,
    /// The scope of the function currently being emitted, for scope-aware variable lookup.
    pub current_function_scope: Option<crate::semantic::ids::ScopeId>,
    /// Maps (function_name, span_start) → body scope id (for ALL functions including equip methods).
    pub function_body_scopes: &'a FxHashMap<(String, usize), crate::semantic::ids::ScopeId>,
}

impl CodegenContext<'_> {
    /// Scope-aware variable lookup: if we know the current function scope, search
    /// within the function's scope tree (the scope itself, all descendants, and
    /// ancestors). Falls back to `lookup_by_name_anywhere` when no function scope
    /// is set.
    pub fn scoped_lookup(&self, name: &str) -> Option<DefId> {
        if let Some(scope_id) = self.current_function_scope {
            self.scopes.lookup_within_function(scope_id, name)
        } else {
            self.scopes.lookup_by_name_anywhere(name)
        }
    }

    /// Extract the first line of source at the given span, trimmed.
    pub fn source_line(&self, span: Span) -> String {
        if span == Span::dummy() || span.start >= self.source_text.len() {
            return String::new();
        }
        let end = span.end.min(self.source_text.len());
        let slice = &self.source_text[span.start..end];
        let line = slice.lines().next().unwrap_or("");
        line.trim().trim_end_matches(':').trim().to_string()
    }
}

/// Escape a string for embedding in a C string literal.
/// Note: does NOT double `%` — callers pass the result to `__gorget_trace_json_str`
/// (which uses `fputc`), not to `fprintf` format strings.
pub fn c_string_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

/// Configuration options for C code generation.
pub struct CodegenOptions {
    pub strip_asserts: bool,
    pub overflow_wrap: bool,
    pub trace: bool,
    pub trace_filename: String,
    pub test_mode: bool,
    pub test_tags: Vec<String>,
    pub test_exclude_tags: Vec<String>,
    pub test_name_filter: Option<String>,
    pub source_text: String,
    /// When true, produce split host/guest output for hot code reload.
    pub hot_reload: bool,
    /// Source file paths to watch for changes (main + imports).
    pub watch_paths: Vec<String>,
    /// The path where the guest .dylib will be compiled to.
    pub guest_lib_path: String,
    /// The `gg` compiler command to recompile the guest.
    pub recompile_cmd: String,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            strip_asserts: false,
            overflow_wrap: false,
            trace: false,
            trace_filename: String::new(),
            test_mode: false,
            test_tags: Vec::new(),
            test_exclude_tags: Vec::new(),
            test_name_filter: None,
            source_text: String::new(),
            hot_reload: false,
            watch_paths: Vec::new(),
            guest_lib_path: String::new(),
            recompile_cmd: String::new(),
        }
    }
}

/// Output from C code generation, including the C source and metadata.
pub struct CodegenOutput {
    pub c_code: String,
    pub needs_sdl: bool,
    pub needs_tls: bool,
    pub needs_crypto: bool,
    /// Host C source for hot-reload mode (contains main loop + file watcher).
    pub host_code: Option<String>,
    /// Guest C source for hot-reload mode (contains user functions as shared lib).
    pub guest_code: Option<String>,
}

/// Generate C source code from a parsed and analyzed Gorget module.
pub fn generate_c(module: &Module, analysis: &AnalysisResult, opts: CodegenOptions) -> CodegenOutput {
    let mut field_type_names = FxHashMap::default();
    for item in &module.items {
        if let Item::Struct(s) = &item.node {
            let struct_name = s.name.node.clone();
            for field in &s.fields {
                field_type_names.insert(
                    (struct_name.clone(), field.node.name.node.clone()),
                    field.node.type_.node.clone(),
                );
            }
        }
        // Newtypes have a single "value" field — register it for constructor coercion
        if let Item::Newtype(nt) = &item.node {
            field_type_names.insert(
                (nt.name.node.clone(), "value".to_string()),
                nt.inner_type.node.clone(),
            );
        }
    }

    // Collect all top-level function names (except main) so codegen can
    // prefix them with `gg_` to avoid C library symbol collisions.
    let mut function_names = HashSet::new();
    for item in &module.items {
        if let Item::Function(f) = &item.node {
            if f.name.node != "main" && f.span != crate::span::Span::dummy() {
                function_names.insert(f.name.node.clone());
            }
        }
    }

    // Build extern symbol table from extern function/method declarations.
    let mut extern_symbols: FxHashMap<(String, String), ExternBinding> = FxHashMap::default();
    for item in &module.items {
        match &item.node {
            Item::Function(f) => {
                if let FunctionBody::Extern(sym) = &f.body {
                    let params_need_ref: Vec<bool> = f.params.iter()
                        .map(|p| ast_type_needs_ref(&p.node.type_.node))
                        .collect();
                    extern_symbols.insert(
                        ("".to_string(), f.name.node.clone()),
                        ExternBinding { c_symbol: sym.clone(), params_need_ref },
                    );
                }
            }
            Item::Equip(equip) => {
                let type_name = match &equip.type_.node {
                    Type::Named { name, .. } => name.node.clone(),
                    _ => continue,
                };
                for method in &equip.items {
                    if let FunctionBody::Extern(sym) = &method.node.body {
                        // Skip self param (index 0) — compute ref info for remaining params
                        let params_need_ref: Vec<bool> = method.node.params.iter()
                            .filter(|p| p.node.name.node != "self")
                            .map(|p| ast_type_needs_ref(&p.node.type_.node))
                            .collect();
                        extern_symbols.insert(
                            (type_name.clone(), method.node.name.node.clone()),
                            ExternBinding { c_symbol: sym.clone(), params_need_ref },
                        );
                    }
                }
            }
            _ => {}
        }
    }

    let is_test_module = opts.test_mode;
    let hot_reload = opts.hot_reload;
    let hot_reload_opts = if hot_reload {
        Some((opts.watch_paths.clone(), opts.guest_lib_path.clone(), opts.recompile_cmd.clone()))
    } else {
        None
    };

    let mut ctx = CodegenContext {
        scopes: &analysis.scopes,
        types: &analysis.types,
        resolution_map: &analysis.resolution_map,
        struct_fields: &analysis.struct_fields,
        enum_variants: &analysis.enum_variants,
        function_info: &analysis.function_info,
        traits: &analysis.traits,
        current_self_type: None,
        current_function_throws: false,
        lifted_closures: Vec::new(),
        closure_counter: 0,
        generic_instances: Vec::new(),
        generic_struct_templates: FxHashMap::default(),
        generic_enum_templates: FxHashMap::default(),
        generic_fn_templates: FxHashMap::default(),
        generic_equip_templates: FxHashMap::default(),
        closure_vars: HashSet::new(),
        fn_type_signatures: FxHashMap::default(),
        closure_var_info: FxHashMap::default(),
        fn_trait_sigs: Vec::new(),
        vector_vars: HashSet::new(),
        escaping_closure_vars: HashSet::new(),
        closure_heap_alloc: false,
        tuple_typedefs: Vec::new(),
        decl_type_hint: None,
        drop_scopes: Vec::new(),
        closure_return_type_hint: None,
        type_subs: Vec::new(),
        type_id_subs: Vec::new(),
        monomorphized_param_c_types: Vec::new(),
        strip_asserts: opts.strip_asserts,
        overflow_wrap: opts.overflow_wrap,
        trace: opts.trace,
        current_function_gorget_name: None,
        trace_filename: opts.trace_filename,
        expr_types: &analysis.expr_types,
        current_function_return_c_type: None,
        try_counter: 0,
        string_temp_counter: 0,
        string_temps: Vec::new(),
        pending_move_zeros: Vec::new(),
        field_type_names,
        mutable_captures: HashSet::new(),
        pointer_params: HashSet::new(),
        extern_symbols,
        function_names,
        is_test_module,
        test_tag_filter: opts.test_tags,
        test_exclude_tags: opts.test_exclude_tags,
        test_name_filter: opts.test_name_filter,
        in_test_body: false,
        source_text: opts.source_text,
        current_function_scope: None,
        function_body_scopes: &analysis.function_body_scopes,
    };

    let mut emitter = CEmitter::new();

    ctx.collect_generic_templates(module);
    ctx.discover_generic_type_usages_from_semantic();
    ctx.discover_generic_function_usages(module);
    ctx.discover_tuple_types(module);

    // 1. Runtime preamble (includes, types, string helpers)
    emitter.emit(c_runtime::RUNTIME_PREAMBLE);

    // 1a. Panic handler (test mode uses setjmp recovery, normal uses exit)
    if is_test_module {
        emitter.emit(c_runtime::PANIC_TEST);
    } else {
        emitter.emit(c_runtime::PANIC_NORMAL);
    }

    // 1b. Runtime core (checked arithmetic, collections, etc.)
    emitter.emit(c_runtime::RUNTIME_CORE);

    // 1c. Process runtime (when std.process is imported)
    let has_process = module.items.iter().any(|i| {
        matches!(&i.node, Item::Struct(s) if s.name.node == "ExecResult" && s.span == crate::span::Span::dummy())
    });
    if has_process {
        emitter.emit(c_runtime::PROCESS_RUNTIME);
    }

    // 1d. TLS socket runtime (when std.net.tls is imported)
    let has_tls = module.items.iter().any(|i| {
        matches!(&i.node, Item::Struct(s) if s.name.node == "TlsSocket" && s.span == crate::span::Span::dummy())
    });
    if has_tls {
        emitter.emit(c_runtime::TLS_SOCKET_RUNTIME);
    }

    // 1f. Crypto runtime (when std.crypto is imported)
    let has_crypto = module.items.iter().any(|i| {
        matches!(&i.node, Item::Struct(s) if s.name.node == "CipherContext" && s.span == crate::span::Span::dummy())
    });
    if has_crypto {
        emitter.emit(c_runtime::CRYPTO_RUNTIME);
    }

    // 1g. Socket runtime (when std.net.socket is imported)
    let has_socket = module.items.iter().any(|i| {
        matches!(&i.node, Item::Struct(s) if s.name.node == "Socket" && s.span == crate::span::Span::dummy())
    });
    if has_socket {
        emitter.emit(c_runtime::SOCKET_RUNTIME);
    }

    // 1g. Bytes runtime (when std.bytes is imported)
    let has_bytes = module.items.iter().any(|i| {
        if let Item::Function(f) = &i.node {
            f.name.node == "bytes_from_str" && f.span == crate::span::Span::dummy()
        } else {
            false
        }
    });
    if has_bytes {
        emitter.emit(c_runtime::BYTES_RUNTIME);
    }

    // 1g. SDL2 runtime (when std.sdl is imported, directly or via std.gfx)
    let has_sdl = module.items.iter().any(|i| {
        matches!(&i.node, Item::Struct(s) if s.name.node == "SDLWindow" && s.span == crate::span::Span::dummy())
    });
    if has_sdl {
        emitter.emit(c_runtime::SDL_RUNTIME);
    }

    // 1e. Trace runtime (only when trace is enabled)
    if ctx.trace {
        emitter.emit(c_runtime::TRACE_RUNTIME);
    }

    // 2. Forward declarations
    ctx.emit_forward_declarations(module, &mut emitter);

    // 2b. Tuple typedefs (after forward declarations, before type definitions)
    ctx.emit_tuple_typedefs(&mut emitter);
    let tuple_count_before_codegen = ctx.tuple_typedefs.len();

    // 2c. Emit monomorphized generic type definitions (structs/enums only)
    //     Split into two phases:
    //     Phase 1: Collection types (Vector, Dict, etc.) that use pointers internally
    //              and only need forward declarations of their type arguments.
    //     Then regular type definitions (which may use these collections as fields).
    //     Phase 2: Wrapper types (Result, etc.) that contain type args by value
    //              and need their type arguments fully defined.
    ctx.emit_generic_type_definitions_phase1(&mut emitter);

    // 3. Type definitions (structs, enums, type aliases, newtypes)
    ctx.emit_type_definitions(module, &mut emitter);

    // 3b. Remaining generic type definitions (Result, etc. that need user types)
    ctx.emit_generic_type_definitions_phase2(&mut emitter);

    // 4. Function declarations (non-generic)
    ctx.emit_function_declarations(module, &mut emitter);

    // 4a. Generic method/function forward declarations
    ctx.emit_generic_method_declarations(module, &mut emitter);

    // 4b. Vtable instances (all functions are now declared)
    ctx.emit_vtable_instances(module, &mut emitter);

    // 5. Generic method/function definitions
    ctx.emit_generic_method_definitions(module, &mut emitter);

    // 5b. Function definitions (closures are collected during this pass)
    ctx.emit_function_definitions(module, &mut emitter);

    // 6. Emit lifted closures and late-registered tuple typedefs — these were
    //    collected during expression codegen in step 5. We insert them
    //    before main by re-emitting into a separate buffer and splicing.
    let has_late_tuples = ctx.tuple_typedefs.len() > tuple_count_before_codegen;
    if !ctx.lifted_closures.is_empty() || has_late_tuples {
        let mut splice_buf = CEmitter::new();
        // Emit any tuple typedefs registered during codegen (e.g. Dict.items())
        if has_late_tuples {
            splice_buf.emit_line("// ── Late Tuple Typedefs ──");
            for (name, field_types) in ctx.tuple_typedefs[tuple_count_before_codegen..].iter() {
                let fields: Vec<String> = field_types
                    .iter()
                    .enumerate()
                    .map(|(i, t)| format!("{t} _{i};"))
                    .collect();
                splice_buf.emit_line(&format!(
                    "typedef struct {{ {} }} {name};",
                    fields.join(" ")
                ));
            }
        }
        if !ctx.lifted_closures.is_empty() {
            splice_buf.emit_line("// ── Lifted Closures ──");
            ctx.emit_lifted_closures(&mut splice_buf);
        }
        splice_buf.blank_line();
        // Insert just before "// ── Function Definitions ──".
        let output = emitter.finish();
        let marker = "// ── Function Definitions ──";
        if let Some(pos) = output.find(marker) {
            let mut combined = String::with_capacity(output.len() + 512);
            combined.push_str(&output[..pos]);
            combined.push_str(&splice_buf.finish());
            combined.push_str(&output[pos..]);
            return CodegenOutput { c_code: combined, needs_sdl: has_sdl, needs_tls: has_tls, needs_crypto: has_crypto, host_code: None, guest_code: None };
        }
        return CodegenOutput { c_code: output + &splice_buf.finish(), needs_sdl: has_sdl, needs_tls: has_tls, needs_crypto: has_crypto, host_code: None, guest_code: None };
    }

    let mut output = CodegenOutput { c_code: emitter.finish(), needs_sdl: has_sdl, needs_tls: has_tls, needs_crypto: has_crypto, host_code: None, guest_code: None };

    // Hot-reload: generate split host/guest C sources
    if hot_reload {
        if let Some((watch_paths, guest_lib_path, recompile_cmd)) = hot_reload_opts {
            let hr_opts = CodegenOptions {
                hot_reload: true,
                watch_paths,
                guest_lib_path,
                recompile_cmd,
                ..CodegenOptions::default()
            };
            let (host, guest) = generate_hot_reload_split(module, &output.c_code, has_sdl, &hr_opts);
            output.host_code = Some(host);
            output.guest_code = Some(guest);
        }
    }

    output
}

/// Detect the State type from the `init()` function's return type in the module.
fn find_state_type_name(module: &Module) -> Option<String> {
    for item in &module.items {
        if let Item::Function(f) = &item.node {
            if f.name.node == "init" && f.span != Span::dummy() {
                if let Type::Named { name, .. } = &f.return_type.node {
                    return Some(name.node.clone());
                }
            }
        }
    }
    None
}

/// Compute a FNV-1a hash of a string (for state layout change detection).
fn fnv1a_hash(s: &str) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325;
    for b in s.bytes() {
        hash ^= b as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

/// Compute the state hash string from a struct's fields.
fn compute_state_hash(module: &Module, state_type_name: &str) -> u64 {
    for item in &module.items {
        if let Item::Struct(s) = &item.node {
            if s.name.node == state_type_name {
                let mut layout = String::new();
                for field in &s.fields {
                    let field_type = format!("{:?}", field.node.type_.node);
                    let field_name = &field.node.name.node;
                    layout.push_str(&format!("{field_type} {field_name};"));
                }
                return fnv1a_hash(&layout);
            }
        }
    }
    0
}

/// Collect init/tick/reload function signatures from the module.
struct HotReloadFunctions {
    /// init() extra params (beyond return type): list of (name, AST type)
    init_extra_params: Vec<(String, Type)>,
    /// tick() extra params (beyond State &state): list of (name, AST type)
    tick_extra_params: Vec<(String, Type)>,
    /// Whether reload() exists
    has_reload: bool,
    /// reload() extra params (beyond State &state)
    reload_extra_params: Vec<(String, Type)>,
}

fn collect_hot_reload_functions(module: &Module) -> HotReloadFunctions {
    let mut result = HotReloadFunctions {
        init_extra_params: Vec::new(),
        tick_extra_params: Vec::new(),
        has_reload: false,
        reload_extra_params: Vec::new(),
    };

    for item in &module.items {
        if let Item::Function(f) = &item.node {
            if f.span == Span::dummy() { continue; }
            match f.name.node.as_str() {
                "init" => {
                    for p in &f.params {
                        result.init_extra_params.push((
                            p.node.name.node.clone(),
                            p.node.type_.node.clone(),
                        ));
                    }
                }
                "tick" => {
                    for p in &f.params {
                        if p.node.name.node == "state" { continue; }
                        result.tick_extra_params.push((
                            p.node.name.node.clone(),
                            p.node.type_.node.clone(),
                        ));
                    }
                }
                "reload" => {
                    result.has_reload = true;
                    for p in &f.params {
                        if p.node.name.node == "state" { continue; }
                        result.reload_extra_params.push((
                            p.node.name.node.clone(),
                            p.node.type_.node.clone(),
                        ));
                    }
                }
                _ => {}
            }
        }
    }

    result
}

/// Generate split host and guest C sources for hot-reload mode.
fn generate_hot_reload_split(
    module: &Module,
    full_c_code: &str,
    has_sdl: bool,
    opts: &CodegenOptions,
) -> (String, String) {
    let state_type = find_state_type_name(module).unwrap_or_else(|| "State".to_string());
    let state_hash = compute_state_hash(module, &state_type);
    let hr_fns = collect_hot_reload_functions(module);

    // ── Guest code ──
    // The guest is the full program minus main(), plus exported wrappers and state hash.
    let guest_code = generate_guest_code(full_c_code, &state_type, state_hash, &hr_fns);

    // ── Host code ──
    let host_code = generate_host_code(
        full_c_code,
        &state_type,
        has_sdl,
        &hr_fns,
        opts,
    );

    (host_code, guest_code)
}

/// Generate the guest shared library C source.
fn generate_guest_code(
    full_c_code: &str,
    state_type: &str,
    state_hash: u64,
    hr_fns: &HotReloadFunctions,
) -> String {
    let mut guest = String::with_capacity(full_c_code.len() + 1024);

    // Remove main() from the full code — find "int main(" and strip to end of function
    // Strategy: emit everything, but replace the main function with exported guest wrappers.
    // We find "int main(int argc, char** argv) {" and remove through its matching close brace.
    let main_marker = "int main(int argc, char** argv) {";
    if let Some(main_pos) = full_c_code.find(main_marker) {
        // Emit everything before main
        guest.push_str(&full_c_code[..main_pos]);

        // Skip past main's closing brace. Count braces to find the matching close.
        let after_main = &full_c_code[main_pos..];
        let mut brace_depth = 0;
        let mut end_pos = 0;
        for (i, ch) in after_main.char_indices() {
            if ch == '{' { brace_depth += 1; }
            if ch == '}' {
                brace_depth -= 1;
                if brace_depth == 0 {
                    end_pos = i + 1;
                    break;
                }
            }
        }
        // Skip the trailing newline after the closing brace
        let remaining = &after_main[end_pos..];
        let remaining = remaining.strip_prefix('\n').unwrap_or(remaining);
        guest.push_str(remaining);
    } else {
        // No main() found — use as-is
        guest.push_str(full_c_code);
    }

    // Emit state hash constant
    guest.push_str(&format!(
        "\n// ── Hot Reload Guest Exports ──\n\
         const uint64_t GORGET_STATE_HASH = 0x{state_hash:016X}ULL;\n\n"
    ));

    // Emit exported wrapper for init()
    let init_c_params: Vec<String> = hr_fns.init_extra_params.iter()
        .map(|(name, _ty)| name.clone())
        .collect();
    let init_args = init_c_params.join(", ");
    guest.push_str(&format!(
        "__attribute__((visibility(\"default\")))\n\
         {state_type} gorget_guest_init({}) {{\n\
         \treturn gg_init({init_args});\n\
         }}\n\n",
        if hr_fns.init_extra_params.is_empty() {
            "void".to_string()
        } else {
            // For simplicity, use the same signature that codegen produced
            hr_fns.init_extra_params.iter()
                .map(|(name, _)| format!("void* {name}"))  // placeholder — refined below
                .collect::<Vec<_>>().join(", ")
        }
    ));

    // Emit exported wrapper for tick()
    guest.push_str(&format!(
        "__attribute__((visibility(\"default\")))\n\
         bool gorget_guest_tick({state_type}* state) {{\n\
         \treturn gg_tick(state);\n\
         }}\n\n"
    ));

    // Emit exported wrapper for reload() if present
    if hr_fns.has_reload {
        guest.push_str(&format!(
            "__attribute__((visibility(\"default\")))\n\
             void gorget_guest_reload({state_type}* state) {{\n\
             \tgg_reload(state);\n\
             }}\n\n"
        ));
    }

    guest
}

/// Generate the host executable C source.
fn generate_host_code(
    full_c_code: &str,
    state_type: &str,
    has_sdl: bool,
    _hr_fns: &HotReloadFunctions,
    opts: &CodegenOptions,
) -> String {
    let mut host = String::with_capacity(4096);

    // Emit the runtime preamble and type definitions from the full code
    // (everything up to "// ── Function Definitions ──" gives us types + runtime)
    let func_defs_marker = "// ── Function Definitions ──";
    if let Some(pos) = full_c_code.find(func_defs_marker) {
        host.push_str(&full_c_code[..pos]);
    } else {
        // Fallback: include whole runtime
        host.push_str(c_runtime::RUNTIME_PREAMBLE);
        host.push_str(c_runtime::PANIC_NORMAL);
        host.push_str(c_runtime::RUNTIME_CORE);
    }

    // Add hot-reload runtime
    host.push_str(c_runtime::HOT_RELOAD_RUNTIME);

    // Add function typedefs for guest entry points
    host.push_str(&format!(
        "// ── Hot Reload Host ──\n\
         typedef {state_type} (*gorget_init_fn_t)(void);\n\
         typedef bool (*gorget_tick_fn_t)({state_type}*);\n\
         typedef void (*gorget_reload_fn_t)({state_type}*);\n\n"
    ));

    // Generate main()
    let guest_lib_path = if opts.guest_lib_path.is_empty() {
        "guest".to_string()
    } else {
        opts.guest_lib_path.clone()
    };
    let recompile_cmd = if opts.recompile_cmd.is_empty() {
        "gg build --shared".to_string()
    } else {
        opts.recompile_cmd.clone()
    };

    // Build watch paths array
    let watch_paths_c: String = if opts.watch_paths.is_empty() {
        "    const char* __watch_paths[] = {\".\"};\n    int __watch_count = 1;\n".to_string()
    } else {
        let mut s = String::from("    const char* __watch_paths[] = {");
        for (i, p) in opts.watch_paths.iter().enumerate() {
            if i > 0 { s.push_str(", "); }
            s.push_str(&format!("\"{p}\""));
        }
        s.push_str("};\n");
        s.push_str(&format!("    int __watch_count = {};\n", opts.watch_paths.len()));
        s
    };

    let sdl_init = if has_sdl {
        "    // TODO: SDL resource initialization (persistent across reloads)\n"
    } else {
        ""
    };

    host.push_str(&format!(
        r#"int main(int argc, char** argv) {{
    gorget_init_args(argc, argv);
{sdl_init}
    // Watch source files for changes
{watch_paths_c}
    GorgetFileWatcher __watcher = gorget_hot_watch_init(__watch_paths, __watch_count);

    // Initial compile + load
    if (system("{recompile_cmd}") != 0) {{
        fprintf(stderr, "[hot-reload] Initial compilation failed\n");
        return 1;
    }}
    GorgetGuestModule __guest = gorget_hot_load("./{guest_lib_path}" GORGET_DYLIB_EXT);
    if (!__guest.handle) {{
        fprintf(stderr, "[hot-reload] Failed to load guest module\n");
        return 1;
    }}

    // Initialize state
    gorget_init_fn_t __init_fn = (gorget_init_fn_t)__guest.init;
    {state_type} __state = __init_fn();

    // Main loop
    while (1) {{
        if (gorget_hot_watch_check(&__watcher)) {{
            // Debounce: wait a moment for write to complete
            usleep(50000);  // 50ms

            fprintf(stderr, "[hot-reload] Recompiling...\n");
            if (system("{recompile_cmd}") == 0) {{
                GorgetGuestModule __new_guest = gorget_hot_load("./{guest_lib_path}" GORGET_DYLIB_EXT);
                if (__new_guest.handle) {{
                    if (__new_guest.state_hash != __guest.state_hash) {{
                        fprintf(stderr, "[hot-reload] State layout changed — reinitializing\n");
                        gorget_init_fn_t __new_init = (gorget_init_fn_t)__new_guest.init;
                        __state = __new_init();
                    }} else if (__new_guest.reload) {{
                        gorget_reload_fn_t __reload_fn = (gorget_reload_fn_t)__new_guest.reload;
                        __reload_fn(&__state);
                    }}
                    gorget_hot_unload(&__guest);
                    __guest = __new_guest;
                    fprintf(stderr, "[hot-reload] OK\n");
                }} else {{
                    fprintf(stderr, "[hot-reload] Load failed — keeping old code\n");
                }}
            }} else {{
                fprintf(stderr, "[hot-reload] Compile error — keeping old code\n");
            }}
        }}

        gorget_tick_fn_t __tick_fn = (gorget_tick_fn_t)__guest.tick;
        if (!__tick_fn(&__state)) break;
    }}

    gorget_hot_unload(&__guest);
    gorget_hot_watch_close(&__watcher);
    return 0;
}}
"#
    ));

    host
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::semantic;

    fn compile_to_c(source: &str) -> String {
        let mut parser = Parser::new(source);
        let mut module = parser.parse_module();
        assert!(
            parser.errors.is_empty(),
            "parse errors: {:?}",
            parser.errors
        );

        let result = semantic::analyze(&mut module);
        assert!(
            result.errors.is_empty(),
            "semantic errors: {:?}",
            result.errors
        );

        generate_c(&module, &result, CodegenOptions { source_text: source.to_string(), ..CodegenOptions::default() }).c_code
    }

    #[test]
    fn hello_world_generates_c() {
        let source = "void main():\n    print(\"Hello, World!\")\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int main(int argc, char** argv)"));
        assert!(c_code.contains("printf"));
        assert!(c_code.contains("Hello, World!"));
        assert!(c_code.contains("return 0;"));
    }

    #[test]
    fn function_with_return() {
        let source = "int add(int a, int b):\n    return a + b\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int64_t gg_add(int64_t a, int64_t b)"));
        assert!(c_code.contains("GORGET_CHECKED_ADD(a, b)"));
    }

    #[test]
    fn expression_body_function() {
        let source = "int double(int x) = x * 2\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int64_t gg_double(int64_t x)"));
        assert!(c_code.contains("GORGET_CHECKED_MUL(x, INT64_C(2))"));
    }

    #[test]
    fn struct_definition() {
        let source = "struct Point:\n    float x\n    float y\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef struct Point Point;"));
        assert!(c_code.contains("struct Point {"));
        assert!(c_code.contains("double x;"));
        assert!(c_code.contains("double y;"));
    }

    #[test]
    fn enum_definition() {
        let source = "enum Color:\n    Red\n    Green\n    Blue\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Color_TAG_Red"));
        assert!(c_code.contains("Color_TAG_Green"));
        assert!(c_code.contains("Color_TAG_Blue"));
        assert!(c_code.contains("Color_Tag"));
        assert!(c_code.contains("Color__Red"));
    }

    #[test]
    fn if_statement() {
        let source = "void main():\n    int x = 5\n    if x > 0:\n        print(\"positive\")\n    else:\n        print(\"negative\")\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("if ((x > INT64_C(0)))"));
        assert!(c_code.contains("} else {"));
    }

    #[test]
    fn for_loop_with_range() {
        let source = "void main():\n    for i in 0..10:\n        print(\"{i}\")\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("for (int64_t i = INT64_C(0); i < INT64_C(10); i++)"));
    }

    #[test]
    fn while_loop() {
        let source = "void main():\n    int x = 0\n    while x < 10:\n        x += 1\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("while ((x < INT64_C(10)))"));
        assert!(c_code.contains("GORGET_CHECKED_ADD_ASSIGN(x, INT64_C(1));"));
    }

    #[test]
    fn variable_declaration() {
        let source = "void main():\n    int x = 5\n    const int y = 10\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("int64_t x = INT64_C(5);"));
        assert!(c_code.contains("const int64_t y = INT64_C(10);"));
    }

    #[test]
    fn impl_block_methods() {
        let source = "\
struct Point:
    float x
    float y

equip Point:
    Point origin():
        return Point(0.0, 0.0)

    float magnitude(self):
        return 0.0
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Point Point__origin(void)"));
        assert!(c_code.contains("double Point__magnitude(const Point* self)"));
    }

    // ── Phase 5 tests ──────────────────────────────────────

    #[test]
    fn type_alias() {
        let source = "type Score = int\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef int64_t Score;"));
    }

    #[test]
    fn newtype_def() {
        let source = "newtype UserId(int)\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("struct UserId {"));
        assert!(c_code.contains("int64_t value;"));
    }

    #[test]
    fn match_with_binding() {
        let source = "\
void main():
    int x = 42
    match x:
        case 0:
            print(\"zero\")
        case n:
            print(\"other\")
";
        let c_code = compile_to_c(source);
        // Should use __typeof__ for the temp and binding
        assert!(c_code.contains("__typeof__"));
        // Binding pattern always matches (condition is 1)
        assert!(c_code.contains("if (") || c_code.contains("else if ("));
    }

    #[test]
    fn match_with_enum_destructuring() {
        let source = "\
enum Maybe:
    Just(int)
    Nothing

void main():
    auto val = Just(42)
    match val:
        case Just(x):
            print(\"has value\")
        case Nothing:
            print(\"empty\")
";
        let c_code = compile_to_c(source);
        // Should check the tag for enum variant patterns
        assert!(c_code.contains("Maybe_TAG_Just"));
        assert!(c_code.contains("Maybe_TAG_Nothing"));
    }

    #[test]
    fn throw_statement() {
        let source = "\
void fail() throws str:
    throw \"something went wrong\"
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GORGET_THROW("));
    }

    #[test]
    fn runtime_includes_setjmp() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("#include <setjmp.h>"));
        assert!(c_code.contains("GorgetError"));
        assert!(c_code.contains("GORGET_TRY"));
        assert!(c_code.contains("GorgetClosure"));
        assert!(c_code.contains("GorgetArray"));
        assert!(c_code.contains("GorgetString"));
    }

    #[test]
    fn list_comprehension() {
        let source = "\
void main():
    Vector[int] squares = [x * x for x in 0..10]
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("gorget_array_new"));
        assert!(c_code.contains("gorget_array_push"));
        assert!(c_code.contains("GorgetArray"));
    }

    #[test]
    fn for_in_over_array() {
        let source = "\
void main():
    int[3] nums = [1, 2, 3]
    for n in nums:
        print(\"{n}\")
";
        let c_code = compile_to_c(source);
        // Should use sizeof-based iteration
        assert!(c_code.contains("sizeof(nums)/sizeof(nums[0])"));
        assert!(c_code.contains("__typeof__"));
    }

    #[test]
    fn vector_type_maps_to_gorget_array() {
        let source = "\
void main():
    Vector[int] items = [1, 2, 3]
";
        let c_code = compile_to_c(source);
        // Vector[int] should be GorgetArray, not void*
        assert!(c_code.contains("GorgetArray"));
    }

    // ── Phase 6 tests ──────────────────────────────────────

    #[test]
    fn trait_definition_comment() {
        let source = "\
trait Drawable:
    void draw(self)
    str name(self)
";
        let c_code = compile_to_c(source);
        // Trait should be emitted as a descriptive comment
        assert!(c_code.contains("/* trait Drawable:"));
        assert!(c_code.contains("draw"));
        assert!(c_code.contains("name"));
        // Should also emit vtable and trait obj structs
        assert!(c_code.contains("Drawable_VTable"), "Should emit vtable struct");
        assert!(c_code.contains("Drawable_TraitObj"), "Should emit trait obj struct");
    }

    #[test]
    fn extern_block() {
        let source = "\
extern \"C\":
    int printf(str fmt)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("extern"));
        assert!(c_code.contains("printf"));
    }

    #[test]
    fn nil_coalescing_expr() {
        let source = "\
void main():
    auto a = None
    auto b = a ?? 42
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("!= NULL"));
    }

    #[test]
    fn await_stub() {
        let source = "\
async void main():
    auto x = await 42
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("/* await */"));
    }

    #[test]
    fn spawn_stub() {
        let source = "\
void main():
    auto x = spawn 42
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("/* spawn */"));
    }

    #[test]
    fn tuple_type_mapping() {
        // Test that tuple types in AST map to named typedefs
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Tuple(vec![
            crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            },
            crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Float),
                span: crate::span::Span::new(0, 0),
            },
        ]);
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "GorgetTuple_int64_t_double");
    }

    #[test]
    fn function_pointer_type_mapping() {
        // Test that function types in AST map to function pointers
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Function {
            return_type: Box::new(crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }),
            params: vec![
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
            ],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "int64_t (*)(int64_t, int64_t)");

        // c_declare should splice the name into (*)
        let decl = c_types::c_declare(&result, "add");
        assert_eq!(decl, "int64_t (*add)(int64_t, int64_t)");
    }

    #[test]
    fn slice_type_mapping() {
        // Test that slice types in AST map to struct { data, len }
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Slice {
            element: Box::new(crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }),
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert!(result.contains("struct"));
        assert!(result.contains("data"));
        assert!(result.contains("len"));
        assert!(result.contains("int64_t"));
    }

    // ── Phase 7 tests ──────────────────────────────────────

    #[test]
    fn runtime_includes_gorget_map() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GorgetMap"));
        assert!(c_code.contains("__gorget_fnv1a"));
        assert!(c_code.contains("__gorget_hash_str"));
        assert!(c_code.contains("gorget_map_new"));
        assert!(c_code.contains("gorget_map_put"));
        assert!(c_code.contains("gorget_map_get"));
        assert!(c_code.contains("gorget_map_contains"));
        assert!(c_code.contains("gorget_map_len"));
        assert!(c_code.contains("gorget_map_free"));
    }

    #[test]
    fn runtime_includes_gorget_set() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GorgetSet"));
        assert!(c_code.contains("gorget_set_new"));
        assert!(c_code.contains("gorget_set_add"));
        assert!(c_code.contains("gorget_set_contains"));
        assert!(c_code.contains("gorget_set_len"));
        assert!(c_code.contains("gorget_set_free"));
    }

    #[test]
    fn runtime_includes_string_format() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("gorget_string_format"));
        assert!(c_code.contains("gorget_string_cstr"));
    }

    #[test]
    fn runtime_includes_array_at_macro() {
        let source = "void main():\n    pass\n";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("GORGET_ARRAY_AT"));
    }

    #[test]
    fn set_type_maps_to_gorget_set() {
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Set".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "GorgetSet");
    }

    #[test]
    fn dict_type_maps_to_mangled_gorget_map() {
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Dict".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
            ],
        };
        let result = c_types::ast_type_to_c(&ty, &scopes);
        assert_eq!(result, "GorgetDict__int64_t__int64_t");
    }

    #[test]
    fn set_comprehension_generates_gorget_set() {
        let source = "\
void main():
    Set[int] evens = {x for x in 0..10 if x % 2 == 0}
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("/* TODO"));
        assert!(c_code.contains("gorget_set_new"));
        assert!(c_code.contains("gorget_set_add"));
    }

    #[test]
    fn dict_comprehension_generates_gorget_map() {
        let source = "\
void main():
    Dict[int, int] squares = {x: x * x for x in 0..10}
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("/* TODO"));
        assert!(c_code.contains("GorgetDict__int64_t__int64_t__new"));
        assert!(c_code.contains("GorgetDict__int64_t__int64_t__put"));
    }

    #[test]
    fn no_unsupported_expr_for_handled_variants() {
        // Ensure basic programs don't emit "unsupported expr"
        let source = "\
void main():
    int x = 5
    if x > 0:
        print(\"yes\")
    else:
        print(\"no\")
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("unsupported expr"));
    }

    // ── Phase 8 tests ──────────────────────────────────────

    #[test]
    fn generic_struct_skips_template() {
        // The bare generic struct (with type params) should NOT be emitted to C output
        let source = "\
struct Pair[A, B]:
    A first
    B second
";
        let c_code = compile_to_c(source);
        assert!(!c_code.contains("struct Pair {"), "Generic template should not be emitted directly");
        assert!(!c_code.contains("typedef struct Pair Pair;"), "Generic template forward decl should not be emitted");
    }

    #[test]
    fn generic_struct_emits_specialized() {
        let source = "\
struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](1, 2.0)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Pair__int64_t__double"), "Should contain mangled struct name");
        assert!(c_code.contains("int64_t first;"), "Specialized struct should have int64_t field");
        assert!(c_code.contains("double second;"), "Specialized struct should have double field");
    }

    #[test]
    fn generic_struct_forward_decl() {
        let source = "\
struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](1, 2.0)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("typedef struct Pair__int64_t__double Pair__int64_t__double;"),
            "Monomorphized struct should get typedef forward decl");
    }

    #[test]
    fn generic_type_maps_correctly() {
        // Set[int] → GorgetSet (built-in), Pair[int, str] → mangled name (user-defined)
        use crate::semantic::scope::ScopeTable;
        let scopes = ScopeTable::new();

        // Set[int] should still map to GorgetSet
        let ty = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Set".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![crate::span::Spanned {
                node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                span: crate::span::Span::new(0, 0),
            }],
        };
        assert_eq!(c_types::ast_type_to_c(&ty, &scopes), "GorgetSet");

        // Unknown generic type should produce mangled name
        let ty2 = crate::parser::ast::Type::Named {
            name: crate::span::Spanned {
                node: "Pair".to_string(),
                span: crate::span::Span::new(0, 0),
            },
            generic_args: vec![
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Int),
                    span: crate::span::Span::new(0, 0),
                },
                crate::span::Spanned {
                    node: crate::parser::ast::Type::Primitive(crate::parser::ast::PrimitiveType::Str),
                    span: crate::span::Span::new(0, 0),
                },
            ],
        };
        let result = c_types::ast_type_to_c(&ty2, &scopes);
        assert_eq!(result, "Pair__int64_t__const_char_ptr");
    }

    #[test]
    fn generic_call_mangles_name() {
        let source = "\
int max[T](T a, T b) = a

void main():
    int x = max[int](1, 2)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("max__int64_t("), "Generic call should use mangled name");
    }

    #[test]
    fn generic_function_emits_specialized() {
        let source = "\
int identity[T](T x) = x

void main():
    int y = identity[int](42)
";
        let c_code = compile_to_c(source);
        // The generic template should not be emitted
        assert!(!c_code.contains("int64_t identity("), "Generic template should not be emitted");
        // The specialized version should be emitted
        assert!(c_code.contains("identity__int64_t"), "Specialized function should be emitted");
    }

    // ── Phase 9 tests ──────────────────────────────────────

    #[test]
    fn trait_emits_vtable_struct() {
        let source = "\
trait Shape:
    float area(self)
    void draw(self)
";
        let c_code = compile_to_c(source);
        // Vtable struct with function pointers
        assert!(c_code.contains("Shape_VTable"), "Should emit vtable struct name");
        assert!(c_code.contains("double (*area)(const void*)"), "Should have area fn ptr");
        assert!(c_code.contains("void (*draw)(const void*)"), "Should have draw fn ptr");
    }

    #[test]
    fn trait_emits_trait_obj_struct() {
        let source = "\
trait Shape:
    float area(self)
";
        let c_code = compile_to_c(source);
        // Trait object struct with data + vtable
        assert!(c_code.contains("Shape_TraitObj"), "Should emit trait obj struct name");
        assert!(c_code.contains("void* data;"), "Trait obj should have data pointer");
        assert!(c_code.contains("const Shape_VTable* vtable;"), "Trait obj should have vtable pointer");
    }

    #[test]
    fn trait_impl_emits_vtable_instance() {
        let source = "\
trait Shape:
    float area(self)
    void draw(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return 3.14
    void draw(self):
        pass
";
        let c_code = compile_to_c(source);
        // Static vtable with function pointer assignments
        assert!(c_code.contains("static const Shape_VTable Shape_for_Circle_vtable"),
            "Should emit static vtable instance");
        assert!(c_code.contains(".area = "),
            "Should assign area in vtable");
        assert!(c_code.contains(".draw = "),
            "Should assign draw in vtable");
        assert!(c_code.contains("Shape_for_Circle__area"),
            "Should reference mangled impl function for area");
        assert!(c_code.contains("Shape_for_Circle__draw"),
            "Should reference mangled impl function for draw");
    }

    #[test]
    fn trait_vtable_forward_decl() {
        let source = "\
trait Drawable:
    void draw(self)
";
        let c_code = compile_to_c(source);
        // Forward declaration should appear in forward declarations section
        assert!(c_code.contains("typedef struct Drawable_VTable Drawable_VTable;"),
            "Should forward-declare vtable struct");
    }

    #[test]
    fn trait_method_with_params() {
        let source = "\
trait Renderer:
    void render(self, int width, int height)
";
        let c_code = compile_to_c(source);
        // Vtable fn ptr should include non-self params
        assert!(c_code.contains("void (*render)(const void*, int64_t, int64_t)"),
            "Vtable fn ptr should include non-self params");
    }

    #[test]
    fn equatable_eq_emits_trait_call() {
        let source = "\
struct Point:
    float x
    float y

equip Point with Equatable:
    bool eq(self, Point other):
        return self.x == other.x

void main():
    Point a = Point(1.0, 2.0)
    Point b = Point(1.0, 2.0)
    if a == b:
        print(\"equal\")
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Equatable_for_Point__eq"),
            "== on struct with Equatable should emit trait method call, got:\n{c_code}");
    }

    #[test]
    fn displayable_interpolation_emits_trait_call() {
        let source = "\
struct Point:
    float x
    float y

equip Point with Displayable:
    str display(self):
        return \"Point\"

void main():
    Point p = Point(1.0, 2.0)
    print(\"{p}\")
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("Displayable_for_Point__display"),
            "interpolation of Displayable struct should emit trait method call, got:\n{c_code}");
    }

    #[test]
    fn self_type_resolves_in_signature() {
        let source = "\
struct Foo:
    int x

equip Foo with Cloneable:
    Foo clone(self):
        return Foo(self.x)
";
        let c_code = compile_to_c(source);
        // Self in return type should resolve to Foo
        assert!(c_code.contains("Foo Cloneable_for_Foo__clone(const Foo* self)"),
            "Self return type should resolve to concrete type, got:\n{c_code}");
    }

    #[test]
    fn trait_default_method_emits_body() {
        let source = "\
trait Greeter:
    int value(self)
    int doubled(self):
        return self.value() * 2

struct Foo:
    int x

equip Foo with Greeter:
    int value(self):
        return self.x
";
        let c_code = compile_to_c(source);
        // Default method should be emitted with the trait_for_type mangling
        assert!(c_code.contains("Greeter_for_Foo__doubled"),
            "Default method should be emitted, got:\n{c_code}");
        // Vtable should reference the default method
        assert!(c_code.contains(".doubled = "),
            "Vtable should have default method slot, got:\n{c_code}");
        // Explicitly implemented method should still be present
        assert!(c_code.contains("Greeter_for_Foo__value"),
            "Explicit method should be emitted, got:\n{c_code}");
    }

    #[test]
    fn trait_default_method_override() {
        let source = "\
trait Greeter:
    int base(self):
        return 0

struct Foo:
    int x

equip Foo with Greeter:
    int base(self):
        return self.x
";
        let c_code = compile_to_c(source);
        // The override should be used, not the default
        assert!(c_code.contains("Greeter_for_Foo__base"),
            "Override should be emitted, got:\n{c_code}");
        assert!(c_code.contains("return self->x;"),
            "Override body should be used, got:\n{c_code}");
    }

    #[test]
    fn trait_inheritance_vtable_includes_parent() {
        let source = "\
trait Base:
    int value(self)

trait Child extends Base:
    int extra(self)

struct Foo:
    int x

equip Foo with Child:
    int value(self):
        return self.x
    int extra(self):
        return 99
";
        let c_code = compile_to_c(source);
        // Child vtable should have both parent and own method slots
        assert!(c_code.contains("Child_VTable"),
            "Should emit child vtable struct, got:\n{c_code}");
        // Should have parent method in vtable struct
        assert!(c_code.contains("(*value)"),
            "Vtable should include parent method slot, got:\n{c_code}");
        // Should have child method in vtable struct
        assert!(c_code.contains("(*extra)"),
            "Vtable should include own method slot, got:\n{c_code}");
        // Vtable instance should reference both methods
        assert!(c_code.contains(".value = "),
            "Vtable instance should assign parent method, got:\n{c_code}");
        assert!(c_code.contains(".extra = "),
            "Vtable instance should assign own method, got:\n{c_code}");
    }

    #[test]
    fn generic_function_block_body() {
        let source = "\
T first[T](T a, T b):
    T result = a
    return result

void main():
    int x = first[int](10, 20)
";
        let c_code = compile_to_c(source);
        assert!(c_code.contains("first__int64_t"), "Should emit monomorphized function");
        assert!(c_code.contains("int64_t result"), "Local var type T should be substituted");
    }
}
