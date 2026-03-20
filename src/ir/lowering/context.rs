use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::types::*;
use crate::parser::ast::{Expr, Ownership, PrimitiveType, Type};
use crate::semantic::AnalysisResult;
use crate::span::Spanned;

use super::closures::ClosureLowering;
use super::drops::DropElaborator;
use super::types::TypeMapper;

use crate::ir::types::BlockId;

/// How a function parameter is passed at the C ABI level.
/// Single source of truth — replaces scattered re-derivation in lower_call_arg
/// and format_args_with_coercion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamABI {
    /// Passed by value (copy types, small primitives).
    ByValue,
    /// Passed by const pointer (`const T*` in C). Bare resource-type params.
    ByPtr,
    /// Passed by mutable pointer (`T*` in C). `&` (MutableBorrow) or `!` (Move) on resource types.
    ByMutPtr,
}

/// Metadata for a shared variable's hidden local and wrapper.
#[derive(Debug, Clone, Copy)]
pub struct SharedLocalInfo {
    /// The hidden local that holds the actual wrapper (Mutex/Shared/Atomic/RwLock).
    pub hidden_local: LocalId,
    /// The inner value type (T in Mutex[T], Shared[T], etc.).
    pub inner_type: TypeId,
    /// The wrapper type (e.g., Mutex__int64_t, Shared__Str).
    pub wrapper_type: TypeId,
    /// Which locking/access protocol to use.
    pub kind: SharedLocalKind,
    /// The original `shared` annotation from the AST (`Auto`, `Atomic`, etc.).
    pub ast_shared: crate::parser::ast::SharedKind,
}

/// The kind of wrapper used for a shared variable's hidden local.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SharedLocalKind {
    /// Mutex[T] — lock-based read/write (single lock for all access)
    Mutex,
    /// Shared[T] — ARC-only, read via `.get()` (no locking)
    SharedArc,
    /// AtomicInt or AtomicBool — lock-free atomic ops
    Atomic,
    /// RWLock[T] — reader-writer lock (concurrent reads, exclusive writes)
    RwLock,
}

/// Information about a loop for break/continue targeting.
pub struct LoopInfo {
    pub header_bb: BlockId,  // target for continue
    pub exit_bb: BlockId,    // target for break
}

/// State for generic monomorphization within a function body.
#[derive(Default)]
pub struct GenericState {
    /// Type name substitutions for generic monomorphization.
    /// Maps template type names (e.g., "Container__T") to monomorphized names
    /// (e.g., "Container__int64_t") during generic function body lowering.
    pub type_name_subs: FxHashMap<String, String>,
    /// Raw generic param → concrete mangled fragment subs (e.g., "T" → "int64_t").
    /// Used by resolve_type_name for on-the-fly substitution of names not in the
    /// pre-computed type_name_subs map.
    pub generic_fragment_subs: Vec<(String, String)>,
    /// Generic type parameter → concrete TypeId substitutions.
    /// Maps bare type parameters (e.g., "T") to their concrete TypeIds (e.g., I64_TYPE)
    /// during generic function body lowering.
    pub generic_type_params: FxHashMap<String, TypeId>,
}

/// State for spawn/concurrency tracking during lowering.
#[derive(Default)]
pub struct SpawnState {
    /// Spawn wrapper functions accumulated during lowering; emitted into the module after
    /// all regular functions. Each wrapper reconstructs the closure struct from flat args
    /// and calls the corresponding __Closure_N__call function.
    pub wrapper_fns: Vec<crate::ir::Function>,
    /// Task variable LocalId → spawned fn_name (e.g., local for `t1` → "produce").
    /// Set by Spawn lowering, consumed by Await lowering.
    pub result_locals: FxHashMap<LocalId, String>,
    /// Set during Spawn lowering; consumed by lower_var_decl to register result_locals.
    pub pending_fn: Option<String>,
    /// Accumulated set of all spawned fn names (NOT cleared between functions).
    pub fn_names: FxHashMap<String, bool>,
    /// Subset of fn_names that should run on the blocking pool instead of M:N executor.
    pub blocking_fn_names: rustc_hash::FxHashSet<String>,
    /// Accumulated set of thread-spawned fn names: fn_name → return TypeId.
    /// NOT cleared between functions. Used to emit thread spawn/join helpers.
    pub thread_fns: FxHashMap<String, TypeId>,
    /// Task TypeId → spawned fn_name. Enables await dispatch for tasks stored
    /// in collections (where result_locals doesn't have an entry because the
    /// task local has projections like vector indexing).
    pub task_type_fns: FxHashMap<TypeId, Vec<String>>,
    /// Scheduler backend for `spawn`.
    pub scheduler_mode: crate::ir::SchedulerMode,
}

impl SpawnState {
    /// Register a Task TypeId → spawned fn_name mapping for await dispatch
    /// on tasks stored in collections (where result_locals doesn't apply).
    pub fn register_task_type_fn(&mut self, task_type: TypeId, fn_name: String) {
        self.task_type_fns.entry(task_type).or_default().push(fn_name);
    }
}

/// State for `shared` variable tracking during lowering.
#[derive(Default)]
pub struct SharedVarState {
    /// Shared variable facade locals → SharedLocalInfo.
    /// The facade local has the user-visible inner type T; the hidden local holds the
    /// actual Mutex[T], Shared[T], or AtomicInt/AtomicBool. The `kind` determines which
    /// ops to emit for transparent read/write access.
    pub locals: FxHashMap<LocalId, SharedLocalInfo>,
    /// When true, shared variable reads return the raw wrapper local instead of auto-locking/getting.
    /// Set during spawn arg lowering so shared vars are passed as Mutex/Shared pointers to spawned tasks.
    pub pass_raw: bool,
    /// Function AST bodies indexed by function name. Populated during pre-scan.
    /// Used by async shared token generation to re-lower function bodies with shared params.
    pub fn_ast_bodies: FxHashMap<String, crate::parser::ast::FunctionDef>,
    /// Deferred shared-async variant requests. Recorded at spawn sites, processed after
    /// all functions are lowered (so the source GIR function is available to transform).
    pub pending_variants: Vec<crate::ir::transforms::shared_async::PendingSharedVariant>,
}

/// Tracks lowering state within a function.
pub struct LoweringContext<'a> {
    pub analysis: &'a AnalysisResult,
    pub type_mapper: TypeMapper,
    /// Owned during lowering — taken from Module, returned after.
    pub type_registry: TypeRegistry,
    /// Closure lowering state.
    pub closures: ClosureLowering,
    /// Drop elaboration state.
    pub drops: DropElaborator,
    /// name → (LocalId, GIR TypeId) for variables in the current function.
    locals: FxHashMap<String, (LocalId, TypeId)>,
    /// Function signatures: name → (param GIR TypeIds, return GIR TypeId).
    pub fn_sigs: FxHashMap<String, (Vec<TypeId>, TypeId)>,
    /// Enum variant → (enum_type_name, variant_name) mapping.
    pub enum_variants: FxHashMap<String, (String, String)>,
    /// Struct field info: (type_name, field_name) → (field_index, field_type_id).
    pub struct_fields: FxHashMap<(String, String), (u32, TypeId)>,
    /// Closure info: struct_name → (call_fn_name, struct_type_id, by-value captures with field indices).
    /// Each capture entry is (name, type_id, struct_field_index).
    pub closure_info: FxHashMap<String, (String, TypeId, Vec<(String, TypeId, u32)>)>,
    /// Stack of active loops for break/continue targeting.
    loop_stack: Vec<LoopInfo>,
    /// Generic monomorphization state.
    pub generics: GenericState,
    /// Spawn/concurrency tracking.
    pub spawn: SpawnState,
    /// Shared variable tracking.
    pub shared: SharedVarState,
    /// Module-level constants: name → Constant value (for imports like PI, E, etc.)
    pub module_constants: FxHashMap<String, crate::ir::instructions::Constant>,
    /// Whether `directive strip-asserts` is active (asserts become no-ops).
    pub strip_asserts: bool,
    /// Whether snapshot capture mode is active (`--snapshot save`).
    pub snapshot_mode: bool,
    /// Whether `directive overflow wrap` is active (integer overflow wraps).
    pub overflow_wrap: bool,
    /// LocalIds that are mutable capture pointers (need deref on read/write in closure bodies).
    /// Tracks `&` (MutableBorrow) and `!` (Move) struct params, which are MutPtr in GIR.
    pub mut_capture_locals: FxHashMap<LocalId, TypeId>,
    /// LocalIds that are read-only reference params (bare struct Borrow params).
    /// These are Ptr (const pointer) in GIR and need auto-deref on read.
    pub ref_locals: FxHashMap<LocalId, TypeId>,
    /// LocalIds that are T & references from collection borrowing reads.
    /// The LIR lowering uses SlotLoad instead of SlotAddr for these locals.
    pub collection_ref_locals: FxHashSet<LocalId>,
    /// Extern binding: Gorget name → C symbol name (e.g., "llabs_wrapper" → "llabs").
    pub extern_bindings: FxHashMap<String, String>,
    /// Default parameter values: fn_name → Vec<(param_index, default_expr)>.
    pub fn_defaults: FxHashMap<String, Vec<(usize, crate::parser::ast::Expr)>>,
    /// Function parameter names: fn_name → Vec<param_name> (in declaration order).
    pub fn_param_names: FxHashMap<String, Vec<String>>,
    /// Function parameter ownerships: fn_name → Vec<Ownership> (in declaration order).
    /// Used by token wrapper generation to determine lock type per shared arg.
    pub fn_param_ownerships: FxHashMap<String, Vec<crate::parser::ast::Ownership>>,
    /// Unified parameter ABI: fn_name → Vec<ParamABI> (in declaration order).
    /// Single source of truth for how each parameter is passed at the C ABI level.
    pub fn_param_abis: FxHashMap<String, Vec<ParamABI>>,
    /// If current function uses `throws`, the Result TypeId for wrapping return/throw.
    pub current_throws_result_type: Option<TypeId>,
    /// Target type hint for the current expression being lowered.
    /// Set by VarDecl/Assign handlers so enum variant constructors (Some, None, Ok, Error)
    /// can pick the correctly-monomorphized type.
    pub expected_type: Option<TypeId>,
    /// Closure parameter type hints for higher-order collection methods.
    /// Set before lowering closure arguments to filter/map/fold/etc. so that
    /// untyped closure params get the correct element type instead of I64_TYPE.
    pub closure_param_type_hints: Vec<TypeId>,
    /// Callable parameter return types: LocalId → return TypeId.
    /// Populated during function setup for parameters with Callable/function types.
    pub callable_return_types: FxHashMap<LocalId, TypeId>,
    /// Module-level global variable names (from StaticDecl items).
    /// Used by Expr::Identifier lowering to emit Constant::GlobalRef instead of I64(0).
    pub global_names: rustc_hash::FxHashSet<String>,
    /// Module-level global variable type names: var_name → AST type name (e.g. "AtomicInt").
    /// Used by infer_type_name_from_operand_full to dispatch methods on globals.
    pub global_type_names: FxHashMap<String, String>,
    /// Set of equip method names that are GIR-lowered (not extern/C-runtime).
    /// Used by lower_method_call to decide whether to pass resource-type args by pointer.
    pub gir_equip_methods: rustc_hash::FxHashSet<String>,
    /// Active `with shared_var:` auto-refresh bindings.
    /// Maps the with-binding local → the shared facade local it mirrors.
    /// After each await, the shared var is re-read into the binding local.
    pub with_shared_refresh: Vec<(LocalId, LocalId)>,
    /// Accumulated `on error:` cleanup blocks. Emitted in LIFO order on error paths.
    pub on_error_blocks: Vec<crate::parser::ast::Block>,
    /// Accumulated `assert return` postcondition expressions.
    /// Checked at every `return` site before the value is returned.
    pub postconditions: Vec<(crate::span::Spanned<crate::parser::ast::Expr>, Option<crate::span::Spanned<crate::parser::ast::Expr>>)>,
    /// Parameters upgraded from Borrow to Move in generic functions that return them directly.
    /// The return path must zero the source through the pointer to prevent caller double-free.
    pub move_override_params: std::collections::HashSet<String>,
    /// Methods whose -1 sentinel return should be wrapped into Option[int].
    /// Populated during registration for stdlib collection/string `find`/`index_of` methods.
    /// User-defined methods default to NOT being in this set.
    pub sentinel_to_option_methods: rustc_hash::FxHashSet<String>,
    /// Maps temp locals from field_load → (source_field_place, field_type).
    /// Used by VarDecl/Assign to emit MoveZero after extracting resource-type fields.
    pub field_load_origins: FxHashMap<LocalId, (crate::ir::instructions::Place, TypeId)>,
}


impl<'a> LoweringContext<'a> {
    pub fn new(analysis: &'a AnalysisResult, type_mapper: TypeMapper, type_registry: TypeRegistry) -> Self {
        Self {
            analysis,
            type_mapper,
            type_registry,
            closures: ClosureLowering::new(),
            drops: DropElaborator::new(),
            locals: FxHashMap::default(),
            fn_sigs: FxHashMap::default(),
            enum_variants: FxHashMap::default(),
            struct_fields: FxHashMap::default(),
            closure_info: FxHashMap::default(),
            loop_stack: Vec::new(),
            generics: GenericState::default(),
            spawn: SpawnState::default(),
            shared: SharedVarState::default(),
            module_constants: FxHashMap::default(),
            strip_asserts: false,
            snapshot_mode: false,
            overflow_wrap: false,
            mut_capture_locals: FxHashMap::default(),
            ref_locals: FxHashMap::default(),
            collection_ref_locals: FxHashSet::default(),
            extern_bindings: FxHashMap::default(),
            fn_defaults: FxHashMap::default(),
            fn_param_names: FxHashMap::default(),
            fn_param_ownerships: FxHashMap::default(),
            fn_param_abis: FxHashMap::default(),
            current_throws_result_type: None,
            expected_type: None,
            closure_param_type_hints: Vec::new(),
            callable_return_types: FxHashMap::default(),
            global_names: rustc_hash::FxHashSet::default(),
            global_type_names: FxHashMap::default(),
            gir_equip_methods: rustc_hash::FxHashSet::default(),
            with_shared_refresh: Vec::new(),
            on_error_blocks: Vec::new(),
            postconditions: Vec::new(),
            move_override_params: std::collections::HashSet::new(),
            sentinel_to_option_methods: rustc_hash::FxHashSet::default(),
            field_load_origins: FxHashMap::default(),
        }
    }

    /// Resolve a type name, applying any active substitutions.
    pub fn resolve_type_name(&self, name: &str) -> String {
        if let Some(resolved) = self.generics.type_name_subs.get(name) {
            return resolved.clone();
        }
        // On-the-fly fragment substitution for names like "Vector__T" → "Vector__int64_t"
        // that weren't in the pre-computed map (because the template type was never registered).
        if !self.generics.generic_fragment_subs.is_empty() {
            let mut result = name.to_string();
            let mut changed = false;
            for (param, concrete) in &self.generics.generic_fragment_subs {
                let pattern_end = format!("__{param}");
                let pattern_mid = format!("__{param}__");
                if result.ends_with(&pattern_end) {
                    let prefix = &result[..result.len() - pattern_end.len()];
                    result = format!("{prefix}__{concrete}");
                    changed = true;
                } else if result.contains(&pattern_mid) {
                    result = result.replace(&pattern_mid, &format!("__{concrete}__"));
                    changed = true;
                }
            }
            if changed {
                return result;
            }
        }
        name.to_string()
    }

    /// Map an AST type to a GIR TypeId, applying any active type name substitutions.
    /// Use this instead of `type_mapper.map_ast_type()` when inside generic body lowering.
    pub fn map_type_with_subs(&self, ty: &crate::parser::ast::Type) -> TypeId {
        use crate::parser::ast::Type;
        // Check bare type parameter substitution (e.g., T → int64_t)
        if let Type::Named { name, generic_args } = ty {
            if generic_args.is_empty() {
                if let Some(&id) = self.generics.generic_type_params.get(name.node.as_str()) {
                    return id;
                }
            } else if !self.generics.type_name_subs.is_empty() || !self.generics.generic_fragment_subs.is_empty() {
                // For generic named types, check if the mangled name needs substitution.
                // resolve_type_name handles both type_name_subs (pre-computed) and
                // generic_fragment_subs (on-the-fly), e.g. "Vector__T" → "Vector__int64_t".
                let mangled = super::types::mangle_generic_name(&name.node, generic_args);
                let resolved = self.resolve_type_name(&mangled);
                if let Some(&id) = self.type_mapper.named_types.get(&resolved) {
                    return id;
                }
            }
        }
        self.type_mapper.map_ast_type(ty)
    }

    /// Register a variable in the current function scope.
    pub fn register_local(&mut self, name: &str, local_id: LocalId, type_id: TypeId) {
        self.locals.insert(name.to_string(), (local_id, type_id));
    }

    /// Look up a variable by name.
    pub fn lookup_local(&self, name: &str) -> Option<(LocalId, TypeId)> {
        self.locals.get(name).copied()
    }

    /// Reset locals for the next function.
    pub fn clear_locals(&mut self) {
        self.locals.clear();
        self.mut_capture_locals.clear();
        self.ref_locals.clear();
        self.collection_ref_locals.clear();
        self.spawn.result_locals.clear();
        self.spawn.pending_fn = None;
        self.shared.locals.clear();
        self.with_shared_refresh.clear();
        self.postconditions.clear();
        self.move_override_params.clear();
    }

    /// Take the locals map, leaving it empty. Used for save/restore during async variant generation.
    pub fn take_locals(&mut self) -> FxHashMap<String, (LocalId, TypeId)> {
        std::mem::take(&mut self.locals)
    }

    /// Restore a previously saved locals map.
    pub fn restore_locals(&mut self, locals: FxHashMap<String, (LocalId, TypeId)>) {
        self.locals = locals;
    }

    /// Iterate over all locals (for type inference).
    pub fn locals_iter(&self) -> impl Iterator<Item = (&String, &(LocalId, TypeId))> {
        self.locals.iter()
    }

    /// Resolve the GIR type for a variable declaration.
    /// Uses the explicit type if given, otherwise infers from the value expression for `auto`.
    pub fn resolve_var_type(
        &self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
    ) -> TypeId {
        match &type_.node {
            Type::Inferred => self.infer_type_from_expr(&value.node),
            other => self.map_type_with_subs(other),
        }
    }

    /// Adjust a GIR string type based on provenance analysis.
    /// After str→StringType parser unification, all string annotations produce
    /// `owned_string_type` in the IR. But provenance may have downgraded the
    /// semantic type_id to `string_id` (view). This method checks the semantic
    /// type_id and returns `str_type` if provenance determined the binding is a view.
    pub fn provenance_adjusted_string_type(
        &self,
        gir_type: TypeId,
        name: &str,
        span: crate::span::Span,
    ) -> TypeId {
        if gir_type != self.type_mapper.owned_string_type {
            return gir_type;
        }
        // Look up the provenance-adjusted type_id from semantic analysis
        if let Some(def_id) = self.analysis.scopes.lookup_def_by_span(name, span) {
            let def = self.analysis.scopes.get_def(def_id);
            if let Some(sem_tid) = def.type_id {
                if sem_tid == self.analysis.types.string_id {
                    return self.type_mapper.str_type;
                }
            }
        }
        gir_type
    }

    /// Extract the Ok type from a Result TypeId, if it is a Result type.
    pub fn unwrap_result_ok_type(&self, result_type: TypeId) -> Option<TypeId> {
        let name = self.type_registry.type_name(result_type)?;
        if !name.starts_with("Result__") { return None; }
        let td = self.type_registry.get_type_def(&name)?;
        if let TypeDefKind::Enum(e) = &td.kind {
            e.variants.iter().find(|v| v.name == "Ok")
                .and_then(|v| v.fields.first().map(|f| f.type_id))
        } else { None }
    }

    /// Infer a GIR type from a literal expression (for `auto` declarations).
    fn infer_type_from_expr(&self, expr: &Expr) -> TypeId {
        match expr {
            Expr::IntLiteral(_) => I64_TYPE,
            Expr::FloatLiteral(_) => F64_TYPE,
            Expr::BoolLiteral(_) => BOOL_TYPE,
            Expr::StringLiteral(_) => self.type_mapper.str_type,
            Expr::BinaryOp { left, op, .. } => {
                use crate::parser::ast::BinaryOp;
                match op {
                    BinaryOp::Eq | BinaryOp::Neq | BinaryOp::Lt | BinaryOp::Gt
                    | BinaryOp::LtEq | BinaryOp::GtEq | BinaryOp::And | BinaryOp::Or => {
                        BOOL_TYPE
                    }
                    _ => self.infer_type_from_expr(&left.node),
                }
            }
            Expr::UnaryOp { operand, .. } => self.infer_type_from_expr(&operand.node),
            Expr::Call { callee, .. } => {
                // Look up the function return type
                if let Expr::Identifier(name) = &callee.node {
                    if let Some((_, ret_ty)) = self.fn_sigs.get(name.as_str()) {
                        let ret_ty = *ret_ty;
                        // In a propagation context, auto-unwrap Result to its Ok type
                        if self.current_throws_result_type.is_some() {
                            if let Some(ok_ty) = self.unwrap_result_ok_type(ret_ty) {
                                return ok_ty;
                            }
                        }
                        return ret_ty;
                    }
                    // Check if it's an enum variant constructor
                    if let Some((enum_name, _)) = self.enum_variants.get(name.as_str()) {
                        if let Some(&type_id) = self.type_mapper.named_types.get(enum_name.as_str()) {
                            return type_id;
                        }
                    }
                }
                I64_TYPE // fallback
            }
            Expr::Identifier(name) => {
                if let Some((_, ty)) = self.lookup_local(name) {
                    return ty;
                }
                I64_TYPE // fallback
            }
            Expr::StructLiteral { name, .. } => {
                if let Some(&type_id) = self.type_mapper.named_types.get(name.node.as_str()) {
                    return type_id;
                }
                UNIT_TYPE
            }
            Expr::FieldAccess { object, .. } => {
                // Try to infer from the object type
                self.infer_type_from_expr(&object.node)
            }
            _ => I64_TYPE, // conservative default
        }
    }

    /// Resolve type for a const variable (same as regular var for Phase 1).
    pub fn resolve_const_type(
        &self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
    ) -> TypeId {
        match &type_.node {
            Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Int64) => I64_TYPE,
            _ => self.resolve_var_type(type_, value),
        }
    }

    /// Resolve an identifier to an enum variant: returns (enum_type_name, variant_name).
    pub fn resolve_enum_variant(&self, name: &str) -> Option<(String, String)> {
        self.enum_variants.get(name).cloned()
    }

    /// Register a pointer type and return its TypeId.
    pub fn register_ptr_type(&mut self, pointee: TypeId) -> TypeId {
        self.type_registry.insert(GirType::Ptr(pointee))
    }

    /// Register a mutable pointer type and return its TypeId.
    pub fn register_mut_ptr_type(&mut self, pointee: TypeId) -> TypeId {
        self.type_registry.insert(GirType::MutPtr(pointee))
    }

    /// Resolve a parameter's GIR type with explicit reference semantics.
    ///
    /// Move-type params are always passed by pointer:
    /// - Borrow (bare) → Ptr (const T*) — read-only, callee cannot mutate
    /// - MutableBorrow (&) → MutPtr (T*) — mutable
    /// - Move (!) → MutPtr (T*) — mutable, callee drops pointee
    ///
    /// Copy-type structs pass by value (natural immutability via copy).
    /// Primitives pass by value (except & which becomes MutPtr for out-params).
    pub fn resolve_param_type(&mut self, base_type: TypeId, ownership: Ownership) -> TypeId {
        let is_move = self.type_registry.is_resource_type(base_type);
        match ownership {
            Ownership::MutableBorrow => self.register_mut_ptr_type(base_type),
            Ownership::Move if is_move => self.register_mut_ptr_type(base_type),
            Ownership::Borrow if is_move => self.register_ptr_type(base_type),
            _ => base_type,
        }
    }

    /// Compute the ABI for a single parameter from its base type and ownership.
    pub fn compute_param_abi(&self, base_type: TypeId, ownership: Ownership) -> ParamABI {
        let is_move = self.type_registry.is_resource_type(base_type);
        match ownership {
            Ownership::MutableBorrow => ParamABI::ByMutPtr,
            Ownership::Move if is_move => ParamABI::ByMutPtr,
            Ownership::Borrow if is_move => ParamABI::ByPtr,
            _ => ParamABI::ByValue,
        }
    }

    /// Whether this is a bare Borrow param of a Move type (read-only const pointer).
    pub fn is_ref_param(&self, base_type: TypeId, ownership: Ownership) -> bool {
        matches!(ownership, Ownership::Borrow) && self.type_registry.is_resource_type(base_type)
    }

    /// Whether this param results in a MutPtr (mutable pointer).
    /// True for MutableBorrow (&) and Move (!) on Move types.
    pub fn is_mut_ref_param(&self, base_type: TypeId, ownership: Ownership) -> bool {
        matches!(ownership, Ownership::MutableBorrow)
            || (matches!(ownership, Ownership::Move) && self.type_registry.is_resource_type(base_type))
    }

    /// Whether this param ownership + type combination results in pass-by-pointer.
    /// Resource types and &-annotated params are passed by pointer.
    pub fn is_passed_by_ptr(&self, base_type: TypeId, ownership: Ownership) -> bool {
        self.is_ref_param(base_type, ownership) || self.is_mut_ref_param(base_type, ownership)
    }

    /// If the given local came from a resource-type field load, emit MoveZero for the
    /// source field to prevent double-free. Call this whenever a field_load temp is
    /// consumed (via assignment, function call, push, etc.).
    pub fn emit_field_origin_zero(
        &mut self,
        builder: &mut crate::ir::builder::FunctionBuilder,
        local: LocalId,
    ) {
        if let Some((field_place, _)) = self.field_load_origins.remove(&local) {
            builder.move_zero(field_place);
        }
    }

    /// Populate the struct_fields cache from the TypeRegistry.
    /// Call this after all types have been registered.
    pub fn populate_struct_fields(&mut self) {
        for type_def in self.type_registry.type_defs() {
            if let TypeDefKind::Struct(ref s) = type_def.kind {
                for (i, field) in s.fields.iter().enumerate() {
                    self.struct_fields.insert(
                        (type_def.name.clone(), field.name.clone()),
                        (i as u32, field.type_id),
                    );
                }
            }
        }
    }

    /// Look up field info from the cached struct_fields: returns (field_index, field_type_id).
    pub fn lookup_field(&self, type_name: &str, field_name: &str) -> Option<(u32, TypeId)> {
        self.struct_fields.get(&(type_name.to_string(), field_name.to_string())).copied()
    }

    /// Get the type name for a GIR TypeId from the named_types cache.
    pub fn type_name_for_id(&self, type_id: TypeId) -> Option<&str> {
        self.type_mapper.named_types.iter()
            .find_map(|(name, &id)| if id == type_id { Some(name.as_str()) } else { None })
    }

    /// Get the C type name for a GIR TypeId, including primitive types.
    /// Unlike `type_name_for_id`, this also handles int, float, bool, str, etc.
    /// Falls back to named_types cache for user-defined types.
    pub fn c_type_name_for_id(&self, type_id: TypeId) -> String {
        use crate::ir::types::*;
        match type_id {
            BOOL_TYPE => "bool".to_string(),
            I8_TYPE => "int8_t".to_string(),
            I16_TYPE => "int16_t".to_string(),
            I32_TYPE => "int32_t".to_string(),
            I64_TYPE => "int64_t".to_string(),
            U8_TYPE => "uint8_t".to_string(),
            U16_TYPE => "uint16_t".to_string(),
            U32_TYPE => "uint32_t".to_string(),
            U64_TYPE => "uint64_t".to_string(),
            F32_TYPE => "float".to_string(),
            F64_TYPE => "double".to_string(),
            _ => self.type_name_for_id(type_id)
                .unwrap_or("int64_t")
                .to_string(),
        }
    }

    /// Register closure info for call dispatch.
    pub fn register_closure_info(
        &mut self,
        struct_name: String,
        call_fn_name: String,
        struct_type_id: TypeId,
        captures: Vec<(String, TypeId, u32)>,
    ) {
        self.closure_info.insert(struct_name, (call_fn_name, struct_type_id, captures));
    }

    /// Look up closure info by struct name.
    pub fn lookup_closure_info(&self, struct_name: &str) -> Option<(&str, TypeId, &[(String, TypeId, u32)])> {
        self.closure_info.get(struct_name).map(|(name, tid, caps)| (name.as_str(), *tid, caps.as_slice()))
    }

    // ---- Loop stack for break/continue ----

    /// Push a loop onto the stack (called when entering a while/for/loop).
    pub fn push_loop(&mut self, header_bb: BlockId, exit_bb: BlockId) {
        self.loop_stack.push(LoopInfo { header_bb, exit_bb });
    }

    /// Pop the current loop off the stack.
    pub fn pop_loop(&mut self) {
        self.loop_stack.pop();
    }

    /// Get the current (innermost) loop info for break/continue.
    pub fn current_loop(&self) -> Option<&LoopInfo> {
        self.loop_stack.last()
    }

    // ---- Enum variant tag resolution ----

    /// Resolve the tag index for an enum variant.
    pub fn resolve_variant_tag(&self, type_name: &str, variant_name: &str) -> Option<i64> {
        if let Some(type_def) = self.type_registry.get_type_def(type_name) {
            if let TypeDefKind::Enum(ref e) = type_def.kind {
                for (i, v) in e.variants.iter().enumerate() {
                    if v.name == variant_name {
                        return Some(i as i64);
                    }
                }
            }
        }
        None
    }

    /// Resolve a pointer type to its pointee: Ptr(T) or MutPtr(T) → Some(T), else None.
    pub fn pointee_type(&self, type_id: TypeId) -> Option<TypeId> {
        match self.type_registry.get(type_id)? {
            GirType::Ptr(inner) | GirType::MutPtr(inner) => Some(*inner),
            _ => None,
        }
    }

    /// Resolve the inner type of a Box[T] (Named "Box__X" type) or a raw Ptr/MutPtr.
    /// Box types are stored as GirType::Named("Box__X") with a TypeDef having a single "_0" field.
    /// This is used for `*box_var` dereferencing where pointee_type() would return None.
    pub fn deref_inner_type(&self, type_id: TypeId) -> Option<TypeId> {
        // First try raw pointer types (Ptr/MutPtr)
        if let Some(inner) = self.pointee_type(type_id) {
            return Some(inner);
        }
        // Then try Named Box types: TypeDef with a single "_0" field
        if let Some(GirType::Named(name)) = self.type_registry.get(type_id) {
            if name.starts_with("Box__") {
                if let Some(type_def) = self.type_registry.get_type_def(name.as_str()) {
                    if let TypeDefKind::Struct(ref s) = type_def.kind {
                        if let Some(f) = s.fields.first() {
                            if f.name == "_0" {
                                return Some(f.type_id);
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Look up a TypeId for a named type in the registry.
    pub fn lookup_type_by_name(&self, name: &str) -> Option<TypeId> {
        for i in 0..self.type_registry.len() {
            let tid = TypeId(i as u32);
            if let Some(GirType::Named(n)) = self.type_registry.get(tid) {
                if n == name { return Some(tid); }
            }
        }
        None
    }

    /// Auto-register an Option[T] type if it doesn't exist yet.
    /// Used when Vector.get() is called and Option[T] wasn't pre-registered.
    pub fn ensure_option_type_registered(&mut self, option_name: &str, inner_type: TypeId) {
        use super::types::make_option_type_def;
        self.type_mapper.get_or_register(option_name, &mut self.type_registry, |n| {
            make_option_type_def(n, inner_type)
        });
    }
}

/// Known blocking call names at AST level — these are yield points where the
/// shared-variable token is released. Must stay in sync with
/// `src/ir/lowering/exprs/mod.rs::BLOCKING_CALL_NAMES`.
const BLOCKING_AST_CALLS: &[&str] = &[
    "sleep", "read_file", "write_file", "append_file",
    "readdir", "http_get", "http_post", "http_put", "http_delete",
];

/// Scan an AST expression for any yield points (`.await()` or blocking calls).
pub fn expr_has_await(expr: &crate::parser::ast::Expr) -> bool {
    use crate::parser::ast::Expr;
    match expr {
        Expr::Await { .. } => true,
        Expr::Call { callee, args, .. } => {
            // Check if callee is a known blocking function
            if let Expr::Identifier(name) = &callee.node {
                if BLOCKING_AST_CALLS.contains(&name.as_str()) {
                    return true;
                }
            }
            expr_has_await(&callee.node) || args.iter().any(|a| expr_has_await(&a.node.value.node))
        }
        Expr::MethodCall { receiver, args, .. } => {
            expr_has_await(&receiver.node) || args.iter().any(|a| expr_has_await(&a.node.value.node))
        }
        Expr::BinaryOp { left, right, .. } => {
            expr_has_await(&left.node) || expr_has_await(&right.node)
        }
        Expr::UnaryOp { operand, .. } => expr_has_await(&operand.node),
        Expr::Block(block) => block.stmts.iter().any(|s| stmt_has_await(&s.node)),
        Expr::If { condition, then_branch, else_branch, .. } => {
            expr_has_await(&condition.node)
            || expr_has_await(&then_branch.node)
            || else_branch.as_ref().map_or(false, |eb| expr_has_await(&eb.node))
        }
        Expr::Spawn { expr } => expr_has_await(&expr.node),
        Expr::TupleLiteral(elems) => elems.iter().any(|e| expr_has_await(&e.node)),
        Expr::Index { object, index, .. } => {
            expr_has_await(&object.node) || expr_has_await(&index.node)
        }
        Expr::FieldAccess { object, .. } => expr_has_await(&object.node),
        Expr::Closure { body, .. } => expr_has_await(&body.node),
        _ => false,
    }
}

/// Scan an AST statement for any `.await()` calls.
pub fn stmt_has_await(stmt: &crate::parser::ast::Stmt) -> bool {
    use crate::parser::ast::Stmt;
    match stmt {
        Stmt::Expr(e) => expr_has_await(&e.node),
        Stmt::VarDecl { value, .. } => expr_has_await(&value.node),
        Stmt::Assign { value, .. } => expr_has_await(&value.node),
        Stmt::CompoundAssign { value, .. } => expr_has_await(&value.node),
        Stmt::Return(Some(e)) => expr_has_await(&e.node),
        Stmt::If { condition, then_body, else_body, .. } => {
            expr_has_await(&condition.node)
            || then_body.stmts.iter().any(|s| stmt_has_await(&s.node))
            || else_body.as_ref().map_or(false, |eb| eb.stmts.iter().any(|s| stmt_has_await(&s.node)))
        }
        Stmt::While { condition, body, .. } => {
            expr_has_await(&condition.node)
            || body.stmts.iter().any(|s| stmt_has_await(&s.node))
        }
        Stmt::For { iterable, body, .. } => {
            expr_has_await(&iterable.node)
            || body.stmts.iter().any(|s| stmt_has_await(&s.node))
        }
        Stmt::Match { scrutinee, arms, .. } => {
            expr_has_await(&scrutinee.node)
            || arms.iter().any(|item| {
                if let Some(arm) = item.arm() {
                    expr_has_await(&arm.body.node)
                } else {
                    false
                }
            })
        }
        Stmt::With { body, .. } => {
            body.stmts.iter().any(|s| stmt_has_await(&s.node))
        }
        _ => false,
    }
}
