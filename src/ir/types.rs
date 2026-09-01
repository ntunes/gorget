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

/// Stable per-clone-site identifier. Allocated monotonically at GIR-lowering
/// time at every `LoweringContext::warn_implicit_clone` call. Stable within
/// a single build; deterministic in emission order. Becomes the join key
/// between the compile-time `--clones=sites` map and the (future) runtime
/// per-site counter table — `(site → span/type/reason/runtime_fn/size)`
/// joined to `(site → execution count)` answers the real perf question:
/// which clones are expensive (size × frequency)?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CloneId(pub u32);

impl fmt::Display for CloneId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "C{}", self.0)
    }
}

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
        /// Per-param `Ownership` sigil (`Borrow` / `MutableBorrow` / `Move`).
        /// Populated by every FnPtr writer so the read side at indirect calls
        /// (`calls.rs`'s non-identifier arm, callable-through-collection) can
        /// route each arg through `lower_call_arg` with the right sigil,
        /// mirroring the direct-call path. Empty vec on writers that don't
        /// know a signature (e.g. `Vector[Callable[...]]` element-type
        /// inferrer before the alias table lookup lands).
        param_ownerships: Vec<crate::parser::ast::Ownership>,
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
/// - `copy_semantics == Resource` → register for drop at scope exit
/// - `drop_strategy != None` → also register (even for Trivial types, e.g., ref-counted)
///
/// The C backend decides HOW to drop by looking up `drop_strategy` via
/// `lookup_drop_strategy()` when it encounters a `Drop`/`DropIfAlive` instruction.
///
/// # Valid combinations
///
/// | CopySemantics | DropStrategy    | Use case                               |
/// |---------------|-----------------|----------------------------------------|
/// | Trivial       | None            | Primitives, plain value structs        |
/// | Trivial       | Trivial(fn)     | Ref-counted types (Shared, Weak, Channel) — copyable at GIR level, decrement on drop |
/// | Resource      | None            | Ownership-tracked handles (Thread, Process) — no heap to free, resource semantics prevent duplication |
/// | Resource      | Trivial(fn)     | Standard owned types (String, Vector, Guard) — single free call |
/// | Resource      | Recursive       | Structs containing droppable fields — auto-upgraded by lowering |
/// | Resource      | Custom(fn)      | User-defined Drop::drop — runs custom cleanup then field drops |
///
/// **Suspicious** (flagged by validator): Trivial + Recursive, Trivial + Custom
/// Distinguishes Option/Result enums from user-defined enums.
/// Used for metadata-based dispatch instead of `starts_with("Option__")` matching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnumCategory {
    /// Option[T] — Some(T) | None.
    Option,
    /// Result[T, E] — Ok(T) | Error(E).
    Result,
}

/// What kind of builtin collection a type is (if any).
/// Used for metadata-based dispatch instead of string-prefix matching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionKind {
    /// Vector, Deque, GorgetArray — contiguous indexed storage.
    Array,
    /// Dict — ordered key-value map (preserves insertion order).
    OrderedMap,
    /// HashMap, GorgetMap — unordered key-value map.
    Map,
    /// Set — ordered unique elements.
    OrderedSet,
    /// HashSet, GorgetSet — unordered unique elements.
    Set,
}

impl CollectionKind {
    /// Does this collection's index-store path (`c[k] = v`) COPY its
    /// argument(s) into a self-owned heap slot?
    ///
    /// Map/Set stores lower to `gorget_map_put` → `__gorget_map_materialize_
    /// key/value` → `str_alloc_copy(…, __gorget_current_alloc)`: the owned
    /// key AND value are allocated from the CURRENT allocator. Under `with
    /// Arena` that is the arena, so a materialized non-Copy key/value into an
    /// OUTER-scoped map dangles at `gorget_arena_destroy` — the same escape
    /// class as the `put`/`insert`/`add` method forms.
    ///
    /// Array stores lower to `gorget_array_set`, which writes the value slot
    /// DIRECTLY (no `elem_materialize`) — no fresh allocation, nothing
    /// escapes. (That direct-view store into an owning element slot is a
    /// separate latent footgun, filed; it is not an arena escape.)
    ///
    /// Exhaustive by design (no `_` arm): a new `CollectionKind` MUST make an
    /// explicit materialize decision here, forcing its index-store position
    /// through the shared arena-escape Ingest/Bind classification — the
    /// completeness guard against the next missed store position.
    pub fn index_store_materializes(self) -> bool {
        match self {
            CollectionKind::Array => false,
            CollectionKind::OrderedMap
            | CollectionKind::Map
            | CollectionKind::OrderedSet
            | CollectionKind::Set => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeMetadata {
    pub size: Option<u64>,
    pub align: Option<u64>,
    pub drop_strategy: DropStrategy,
    pub copy_semantics: CopySemantics,
    /// Clone function name for deep-cloning this type (e.g., "gorget_array_clone").
    /// Set from BuiltinTypeProtocol during type registration.
    pub clone_fn: Option<String>,
    /// In-place clone function for collection element slots (`void(*)(void*)`).
    /// e.g., "gorget_array_clone_inplace", "gorget_string_clone_inplace".
    /// Replaces the parallel `elem_clone_fn_for_*` lookup tables.
    pub clone_inplace_fn: Option<String>,
    /// CoW materialize function (`void(*)(void*)`) — view → owned in place.
    /// e.g., "gorget_string_materialize_inplace". `None` if the type has no
    /// view/owner distinction. Replaces the `elem_materialize_fn_for_c_type`
    /// lookup table.
    pub materialize_fn: Option<String>,
    /// Borrow-as-view function (`T(*)(const T*)`) — shallow copy with the
    /// ownership discriminator forced to "view" (cap=0 for Str), drop-safe in
    /// a drop-tracked value slot. e.g., "gorget_string_borrow_view". The
    /// typed eligibility axis for the lazy loop-carried CoW bind
    /// (`emit_lazy_loopcarried_borrow`): `None` = the type's runtime cannot
    /// represent a drop-safe view, so element binds eager-clone. Phase 1:
    /// String only — collections need view-aware frees first
    /// (`gorget_array_free` runs `elem_drop` regardless of cap).
    /// Mirrors `BuiltinTypeProtocol::borrow_view_fn`.
    pub borrow_view_fn: Option<String>,
    /// Collection kind for metadata-based dispatch (replaces name-prefix matching).
    pub collection_kind: Option<CollectionKind>,
    /// Enum category for Option/Result detection (replaces starts_with("Option__") matching).
    pub enum_category: Option<EnumCategory>,
    /// C runtime struct name this Named type aliases to (e.g. `Callable__T_args`
    /// → `"GorgetClosure"`). When set, the C backend emits a typedef to this
    /// runtime struct instead of a fresh `__gg_X` struct definition. Read by
    /// the GIR type-mismatch corrective at `stmts/mod.rs` (skip overwrites
    /// when the inferred Named type is just an alias of a runtime struct).
    /// Mirrors `BuiltinTypeProtocol::c_runtime_alias`.
    pub c_runtime_alias: Option<String>,
    /// Set for `__Closure_N` struct types created by the closure-lowering pass.
    /// The closure env owns captured values via lifetime-tied aliasing — it holds
    /// bitwise copies of outer-scope locals whose lifetime exceeds the closure's.
    /// The consume-site validator skips StructInit fields when the destination
    /// is a closure-env type, because the outer scope's drop handles cleanup
    /// (the env is always freed before the outer scope exits). This is the
    /// typed-metadata form of the "closure alias" ownership pattern —
    /// contrast with user struct inits where the struct independently owns its fields.
    pub is_closure_env: bool,
    /// Set for `Box__T` types registered via `register_collection_alias`.
    /// Distinguishes a heap-allocated single-element wrapper (Box) from
    /// other 1-field newtype-shaped structs. Replaces downstream
    /// downstream name-prefix probes — readers that need to know
    /// "is this a Box wrapper?" check this flag instead of the name.
    pub is_box: bool,
}

impl TypeMetadata {
    /// Refcount-handle family {Shared, Weak, Channel}: set the by-VALUE incref
    /// `clone_fn = {mangled}__clone`. This is the SINGLE writer for the
    /// refcount-clone axis (Layering rule 3: one source of truth, resolve once
    /// / write through). EVERY def-mint path routes here — the annotated-type
    /// path (`map_ast_type_mut`'s Shared/Weak/Channel arm) and the ctor-path
    /// def-mint (`ensure_shared/weak/channel_type_def`) — so a handle minted
    /// either way carries byte-identical metadata and
    /// `TypeRegistry::is_refcount_clone_type` returns the same answer
    /// regardless of which path minted it.
    ///
    /// Guards are deliberately NOT routed here: they keep `Resource`
    /// copy_semantics (which excludes them from `is_refcount_clone_type`) and
    /// spell their own `{mangled}__clone` for the consume-site validator's
    /// producer recognition, a separate axis. `refcount_clone_arm_symmetry`
    /// (tests/lints.rs) locks every mint path to this writer.
    pub fn set_refcount_clone_fn(&mut self, mangled: &str) {
        self.clone_fn = Some(format!("{mangled}__clone"));
    }
}

impl Default for TypeMetadata {
    fn default() -> Self {
        Self {
            size: None,
            align: None,
            drop_strategy: DropStrategy::None,
            copy_semantics: CopySemantics::Trivial,
            clone_fn: None,
            clone_inplace_fn: None,
            materialize_fn: None,
            borrow_view_fn: None,
            collection_kind: None,
            enum_category: None,
            c_runtime_alias: None,
            is_closure_env: false,
            is_box: false,
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

/// Determines whether a value can be bitwise-copied or owns resources requiring cleanup.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CopySemantics {
    /// Trivial data — can be freely bitwise-copied (primitives, `str`, small value structs, ref-counted handles).
    Trivial,
    /// Owns a resource (heap allocation, file handle, lock guard) — requires ownership tracking, cannot be implicitly copied.
    Resource,
}

// Pre-allocated primitive type IDs (indices 0–11).
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

/// Number of pre-allocated primitive types in the registry (indices 0..PRIMITIVE_TYPE_COUNT).
/// Used to distinguish primitives from user-defined types without magic numbers.
pub const PRIMITIVE_TYPE_COUNT: u32 = 12;

/// True for every fixed-width integer primitive TypeId (int8..int64, uint8..uint64).
/// Shared predicate for sites that branch on "is this arg an integer?" (e.g. the
/// `String(n)` capacity-ctor routing in `exprs/mod.rs` + `exprs/calls.rs`) so all
/// int widths are covered consistently instead of hand-syncing per-site lists.
pub fn is_int_type_id(type_id: TypeId) -> bool {
    matches!(
        type_id,
        I8_TYPE | I16_TYPE | I32_TYPE | I64_TYPE | U8_TYPE | U16_TYPE | U32_TYPE | U64_TYPE
    )
}

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
        self.types.len() <= PRIMITIVE_TYPE_COUNT as usize
    }

    /// Whether a type is a primitive (index < PRIMITIVE_TYPE_COUNT).
    pub fn is_primitive(&self, type_id: TypeId) -> bool {
        type_id.0 < PRIMITIVE_TYPE_COUNT
    }

    /// Check whether a type needs dropping based on its metadata.
    /// Primitives never need dropping. Named types need dropping if they
    /// have Resource copy semantics or a non-None drop strategy. The type
    /// upgrade scan (upgrade_types_from_fields) sets DropStrategy::Recursive
    /// for structs/enums with resource-typed fields/variant payloads, so
    /// the drop_strategy check covers transitive cases.
    pub fn needs_drop(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; }
        // Callable values lower to either `GirType::FnPtr` (the bare type-level
        // form, e.g. a `Callable[int()] f` local) or `GirType::Named("Callable__…")`
        // (the mangled monomorphized form used inside collections and after
        // `clone_fn_for_ptr` materializes a value). The FnPtr shape — used
        // for local Callable bindings — carries a heap-alloc'd env at runtime
        // (via `__gorget_closure_env_alloc`); the Named form reads through
        // its TypeDef metadata (Phase A residual #1).
        if matches!(self.get(type_id), Some(GirType::FnPtr { .. })) {
            return true;
        }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                if type_def.metadata.copy_semantics == CopySemantics::Resource
                    || type_def.metadata.drop_strategy != DropStrategy::None
                {
                    return true;
                }
            }
        }
        false
    }

    /// Check if a type name is a collection type (Vector, Dict, Set, etc.).
    /// Reads `collection_kind` from the type's TypeDef — every collection
    /// type and runtime singleton has this set at registration via
    /// BuiltinTypeProtocol.
    pub fn is_collection_type_name(&self, name: &str) -> bool {
        self.get_type_def(name)
            .map(|td| td.metadata.collection_kind.is_some())
            .unwrap_or(false)
    }

    /// Tier 1c: predicate used by the construction helpers. Mirrors
    /// `upgrade_types_from_fields`' check: a field counts as droppable
    /// for transitive-drop purposes when its type is a `GirType::Named`
    /// whose TypeDef has Resource copy_semantics or a non-None
    /// drop_strategy.
    ///
    /// Deliberately NARROWER than `needs_drop`: bare `GirType::FnPtr`
    /// fields (vtable entries) are NOT counted, even though
    /// `needs_drop(FnPtr) == true` for local Callable bindings. The
    /// post-hoc `upgrade_types_from_fields` pass — the writer-side
    /// authority on transitive drop today — also excludes FnPtr fields;
    /// matching that semantic keeps the construction helper and the
    /// upgrade pass in lockstep. (See `closures.rs` for closure layout:
    /// captures use `MutPtr(T)` / value fields, never `FnPtr`-as-field.
    /// The `Callable[T()]` local case where FnPtr DOES own a heap env
    /// is a function-body local, not a struct field.)
    fn field_is_transitively_droppable(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT {
            return false;
        }
        let Some(GirType::Named(name)) = self.get(type_id) else { return false; };
        let Some(td) = self.get_type_def(name) else { return false; };
        td.metadata.copy_semantics == CopySemantics::Resource
            || td.metadata.drop_strategy != DropStrategy::None
    }

    /// Tier 1c: compute coherence-at-construction drop strategy + copy
    /// semantics for a struct's fields. Returns the inferred metadata
    /// upgrade — call this at every TypeDef construction site that doesn't
    /// already carry explicit drop metadata. The caller writes the result
    /// to the TypeDef's metadata before insertion.
    ///
    /// The contract is:
    /// - If ANY field is transitively droppable (the helper recurses
    ///   through registered Named TypeDefs), returns
    ///   `(DropStrategy::Recursive, CopySemantics::Resource)`.
    /// - Otherwise returns `(DropStrategy::None, CopySemantics::Trivial)`.
    ///
    /// This is the once-at-registration counterpart of
    /// `upgrade_types_from_fields`. Together with
    /// `compute_drop_strategy_for_enum`, it eliminates the timing class
    /// where a late-registered TypeDef carries stale metadata.
    ///
    /// **Read this AFTER setting the field types** but BEFORE inserting
    /// the TypeDef — the helper reads `field_is_transitively_droppable`
    /// on each field's TypeId from the live registry.
    pub fn compute_drop_strategy_for_struct(
        &self,
        fields: &[StructField],
    ) -> (DropStrategy, CopySemantics) {
        let needs_drop = fields.iter().any(|f| self.field_is_transitively_droppable(f.type_id));
        if needs_drop {
            (DropStrategy::Recursive, CopySemantics::Resource)
        } else {
            (DropStrategy::None, CopySemantics::Trivial)
        }
    }

    /// Tier 1c: compute coherence-at-construction drop strategy + copy
    /// semantics for an enum's variants. Walks every variant payload field
    /// and returns the same shape as `compute_drop_strategy_for_struct`.
    ///
    /// Use at Option/Result/user-enum registration sites. The wrapper
    /// auto-upgrades to Recursive whenever a variant payload is droppable.
    pub fn compute_drop_strategy_for_enum(
        &self,
        variants: &[EnumVariant],
    ) -> (DropStrategy, CopySemantics) {
        let needs_drop = variants.iter().any(|v| {
            v.fields.iter().any(|f| self.field_is_transitively_droppable(f.type_id))
        });
        if needs_drop {
            (DropStrategy::Recursive, CopySemantics::Resource)
        } else {
            (DropStrategy::None, CopySemantics::Trivial)
        }
    }

    /// Check whether a Copy-semantics type needs dropping when passed as a parameter.
    /// Only true for ref-counted types (Channel, Shared, Weak) that are Copy but
    /// still have a drop strategy. Move types are excluded (body-level drops handle them).
    pub fn needs_param_drop(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                return type_def.metadata.copy_semantics == CopySemantics::Trivial
                    && type_def.metadata.drop_strategy != DropStrategy::None
                    && type_def.metadata.clone_fn.is_some();
            }
        }
        false
    }

    /// Register a named type definition. Returns its index.
    pub fn add_type_def(&mut self, mut def: TypeDef) -> usize {
        // If a TypeDef with this name already exists and was upgraded
        // (e.g., by the type upgrade scan), preserve the metadata.
        if let Some(&existing_idx) = self.name_to_def.get(&def.name) {
            let existing = &self.type_defs[existing_idx];
            if existing.metadata.copy_semantics == CopySemantics::Resource
                || existing.metadata.drop_strategy != DropStrategy::None
            {
                def.metadata.copy_semantics = existing.metadata.copy_semantics.clone();
                if def.metadata.drop_strategy == DropStrategy::None {
                    def.metadata.drop_strategy = existing.metadata.drop_strategy.clone();
                }
                if def.metadata.clone_fn.is_none() {
                    def.metadata.clone_fn = existing.metadata.clone_fn.clone();
                }
            }
        }
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

    /// Wider counterpart of `is_resource_type`: returns true iff the type's
    /// drop is non-trivial (i.e. the type owns heap, OR transitively contains
    /// a type that does — `Option[String]`, `Result[Vector, _]`, struct/enum
    /// containing resource fields). Equivalent to `needs_drop(type_id)`;
    /// exposed as a separate accessor so migration sites read the intent
    /// ("this gate should fire for any non-trivial-drop type") rather than
    /// the mechanism ("does this need a drop registered?").
    ///
    /// **Phase 1 audit (2026-05-05).** The narrow `is_resource_type` and the
    /// wider `needs_drop` answer different questions:
    /// - `is_resource_type(t)`: "does `t` directly own heap?" → used for
    ///   Ptr-wrapping decisions, Move-vs-Copy assign mode, MoveZero
    ///   emission. Most callsites (~125 of ~134) want this narrow shape;
    ///   widening regresses 112 fixtures because clone-fn lookup, struct
    ///   field clone routing, and pattern Ptr-wrap depend on direct shape.
    /// - `needs_drop(t)` / `is_resource_or_contains_resource(t)`: "does
    ///   any drop logic fire for `t`?" → used for drop registration,
    ///   ownership-transfer correctness on enum wrappers, Phase C
    ///   validators that need to recognize Option/Result-with-resource
    ///   payloads. About 3-5 callsites should migrate to this predicate;
    ///   tracked in TODO.md "is_resource_type widening" Phase 2 plan.
    ///
    /// **Cluster 5 finding (2026-05-10).** The disjunction
    /// `is_resource_type(t) || needs_drop(t)` is **NOT redundant** at
    /// most sites and **must not be collapsed**. The two predicates read
    /// from different sources:
    /// - `needs_drop` is metadata-driven: checks `copy_semantics ==
    ///   Resource` and `drop_strategy != None` on the type's `TypeDef`.
    ///   Returns `true` only after `upgrade_types_from_fields` has
    ///   propagated `DropStrategy::Recursive` to structs/enums whose
    ///   fields transitively contain resources.
    /// - `is_resource_type` is structural-and-metadata: checks the same
    ///   metadata AND walks `is_resource_name`'s per-call transitive
    ///   struct-field scan. The scan does not depend on the upgrade pass
    ///   having run, so `is_resource_type(VectorIter[T])` returns true
    ///   immediately on registration (via its `Vector[T] source` field),
    ///   while `needs_drop(VectorIter[T])` returns false until the
    ///   upgrade scan completes.
    ///
    /// Lowering sites that emit `AssignMode` decisions during trait
    /// default body lowering, generic monomorphization templates, and
    /// other paths whose execution interleaves with type-def
    /// registration must keep the disjunction. The probe history is the
    /// 22-fixture regression on Cluster 5 (stdlib_iter / tensor /
    /// vector_userspace_hofs / test_deque / test_tuples /
    /// vector_each_userspace) when the disjunction was collapsed to
    /// just `needs_drop`.
    ///
    /// Implementation is a thin alias over `needs_drop`. Calling this
    /// vs. `needs_drop` is purely a readability choice for the call site:
    /// pick whichever name makes the intent clear.
    pub fn is_resource_or_contains_resource(&self, type_id: TypeId) -> bool {
        self.needs_drop(type_id)
    }

    /// Check whether a type has Resource copy semantics (owns heap-allocated buffers).
    /// This covers both types with explicit Resource metadata in their TypeDef,
    /// and collection types whose TypeDef may lack correct metadata due to
    /// early registration via `register_collection_alias`.
    pub fn is_resource_type(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; } // primitives
        if let Some(GirType::Named(name)) = self.get(type_id) {
            self.is_resource_name(name)
        } else {
            false
        }
    }

    /// Is this a REFCOUNT handle — a thin-pointer wrapper (Shared / Weak /
    /// Channel) whose `clone` is a by-VALUE incref rather than a deep copy?
    ///
    /// This is the ONE canonical accessor for refcount-family membership at
    /// **consuming positions** (ctor field-init, container literal, push/put/
    /// set/insert/send, return, capture). Every consuming-position gate that
    /// today keys off `is_resource_type` must ALSO admit these handles — they
    /// are NOT `is_resource_type` (thin-pointer, `copy_semantics == Trivial`)
    /// yet still need clone-if-live / move-if-dead so a live source is
    /// incref'd (`{Mangled}__clone`, passed BY VALUE) instead of shallow-
    /// aliased (the double-free / under-incref class).
    ///
    /// Discriminated by typed metadata, never a name: `copy_semantics ==
    /// Trivial` (deep-clone resources are `Resource`) AND a registered
    /// `clone_fn`. That is exactly {Shared, Weak, Channel}: the guards keep
    /// `Resource` copy_semantics (excluded), and Mutex/RWLock keep
    /// `clone_fn = None` (single-owner, excluded). The clone_fn on all mint
    /// paths is set through the single writer `TypeMetadata::set_refcount_clone_fn`.
    ///
    /// SIBLING to unify during the Track-2 sigil redesign: `needs_param_drop`
    /// carries a THIRD clause (`drop_strategy != None`) that excludes Channel
    /// (`DropStrategy::None`); the two predicates are NOT interchangeable and
    /// are deliberately kept separate until that redesign.
    pub fn is_refcount_clone_type(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            return self.is_refcount_clone_type_name(name);
        }
        false
    }

    /// Name-keyed form of [`Self::is_refcount_clone_type`], for the LIR-side
    /// readers that only ever hold a mangled element-type name (a collection's
    /// element type is carried as the `Vector__`/`Dict__K__` suffix, never as a
    /// `TypeId`). Shares the predicate body so the two cannot drift apart
    /// (layering rule 3 — one source of truth per axis).
    pub fn is_refcount_clone_type_name(&self, name: &str) -> bool {
        self.get_type_def(name).map_or(false, |td| {
            td.metadata.copy_semantics == CopySemantics::Trivial
                && td.metadata.clone_fn.is_some()
        })
    }

    /// Check if a type is a direct collection type (Vector, Dict, Set, etc.).
    /// Unlike `is_resource_type`, this does NOT include user structs with resource fields
    /// or types with custom Drop. Used for Ptr payload eligibility in collection reads —
    /// only direct collections can be borrowed as Ptr references.
    pub fn is_collection_type(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            self.is_collection_type_name(name)
        } else {
            false
        }
    }

    /// Check if a named type is a resource type (owns heap allocations).
    /// Reads `copy_semantics` and `collection_kind` from TypeDef metadata,
    /// with transitive struct-field check for user types containing
    /// resource-typed fields.
    ///
    /// **Phase D4 widening probe (2026-05-04, reverted):** adding a
    /// transitive enum-variant-payload check (so Option[String] /
    /// Result[String] return true) regressed 112 fixtures. Many
    /// consumers — pattern lowering, collection-element clone routing,
    /// drop accountant, ABI choice — depend on the current narrow
    /// semantics where Option/Result are NOT resources at the wrapper
    /// level (only their payloads are). Widening requires a coordinated
    /// migration of those consumers, not a one-line change here. See
    /// also `has_resource_fields` (which already checks enum variants
    /// for the .get()-borrow-decision use case) — that's the right
    /// shape, but not interchangeable with is_resource_type because
    /// downstream consumers branch on different axes for each.
    fn is_resource_name(&self, name: &str) -> bool {
        if let Some(type_def) = self.get_type_def(name) {
            if type_def.metadata.collection_kind.is_some() {
                return true;
            }
            if type_def.metadata.copy_semantics == CopySemantics::Resource {
                return true;
            }
            // Check if any struct field is a resource type (transitive)
            if let TypeDefKind::Struct(ref sdef) = type_def.kind {
                for f in &sdef.fields {
                    if let Some(GirType::Named(field_name)) = self.get(f.type_id) {
                        if field_name != name && self.is_resource_name(field_name) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Check if a type (struct or enum) contains resource-type fields.
    /// Used by collection .get() to determine if element reads need borrow semantics.
    /// Unlike is_resource_type, this checks enum variant fields too.
    pub fn has_resource_fields(&self, type_id: TypeId) -> bool {
        if self.is_resource_type(type_id) { return true; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                if let TypeDefKind::Enum(ref edef) = type_def.kind {
                    for v in &edef.variants {
                        for f in &v.fields {
                            if let Some(GirType::Named(field_name)) = self.get(f.type_id) {
                                if field_name != name && self.is_resource_name(field_name) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }

    /// Get the enum category (Option/Result) for a named type, if any.
    pub fn enum_category(&self, type_id: TypeId) -> Option<EnumCategory> {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return None; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                return type_def.metadata.enum_category;
            }
        }
        None
    }

    /// Get the collection kind (Array/Map/Set/OrderedMap/OrderedSet) for a
    /// named type, if any. Reads typed `metadata.collection_kind` set at
    /// builtin protocol registration. Both runtime singletons (GorgetArray,
    /// GorgetMap, GorgetSet) and monomorphized aliases (Vector__T,
    /// Dict__K__V, ...) carry the kind. Used to route collection-method
    /// dispatch without name-prefix matching.
    pub fn collection_kind(&self, type_id: TypeId) -> Option<CollectionKind> {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return None; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                return type_def.metadata.collection_kind;
            }
        }
        None
    }

    /// Get the collection kind for a named type by NAME (not TypeId). Useful
    /// for sites operating on mangled type names (e.g. LIR-level element
    /// extraction) where a `TypeId` isn't readily available. Reads the same
    /// `metadata.collection_kind` flag as the TypeId-keyed accessor.
    pub fn collection_kind_by_name(&self, name: &str) -> Option<CollectionKind> {
        self.get_type_def(name).and_then(|td| td.metadata.collection_kind)
    }

    /// Whether the named type is a `Box__T` heap-allocated wrapper.
    /// Reads the typed `metadata.is_box` flag set at registration in
    /// `register_collection_alias`. Replaces downstream
    /// downstream name-prefix probes — typed dispatch is correct
    /// even if a future user struct happens to share the `Box__` prefix.
    pub fn is_box(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(type_def) = self.get_type_def(name) {
                return type_def.metadata.is_box;
            }
        }
        false
    }

    /// Whether the named type is a `Box__T` heap-allocated wrapper, by name.
    /// Useful for sites that already have a name string (e.g. from
    /// extracting from a mangled function name).
    pub fn is_box_name(&self, name: &str) -> bool {
        self.get_type_def(name).map_or(false, |td| td.metadata.is_box)
    }

    /// Check if a named type is an Option or Result enum.
    pub fn is_option_or_result(&self, name: &str) -> bool {
        self.get_type_def(name)
            .and_then(|td| td.metadata.enum_category)
            .is_some()
    }

    /// Whether the type lowers to the 16-byte `GorgetClosure` runtime handle.
    /// Folds the four-line `match get(...) { Named(n) => get_type_def(n)... }`
    /// chain that appears at multiple sites in `lower_var_decl` / methods.rs.
    /// Layering: reads the typed `c_runtime_alias` flag, not a name prefix.
    /// One `FxHashMap` lookup per call (the get_type_def behind the alias).
    pub fn is_closure_runtime_type(&self, type_id: TypeId) -> bool {
        if type_id.0 < PRIMITIVE_TYPE_COUNT { return false; }
        if let Some(GirType::Named(name)) = self.get(type_id) {
            if let Some(td) = self.get_type_def(name) {
                return td.metadata.c_runtime_alias.as_deref() == Some("GorgetClosure");
            }
        }
        false
    }

    /// Resolve the deep-clone function name for a TypeDef, mirroring the
    /// logic in `LoweringContext::clone_fn_for_ptr`. This is the structural
    /// truth: for every named resource type, `T__clone` is generated when
    /// a clone is requested at lowering time, even though the generated
    /// function name isn't always written back into `metadata.clone_fn`
    /// (which is reserved for protocol-driven runtime fns).
    ///
    /// The two layers — `LoweringContext` (which decides emission) and
    /// `TypeRegistry` (which is shared by the validator) — agree by sharing
    /// this single resolver. Validators read it via `clone_fn_names_set`
    /// to recognise calls to user `T__clone` without name pattern matching.
    ///
    /// Returns `None` for trivial / non-droppable types.
    pub fn clone_fn_name_for_def(&self, td: &TypeDef) -> Option<String> {
        // Metadata-based: clone_fn populated at registration from
        // BuiltinTypeProtocol (covers every Vector/Deque/Dict/HashMap/
        // Set/HashSet instantiation, GorgetString, the runtime-named
        // collection types, and Callable / MutCallable / ConsumeCallable /
        // GorgetClosure via the c_runtime_alias-tagged TypeDef).
        if let Some(ref cf) = td.metadata.clone_fn {
            return Some(cf.clone());
        }
        // User structs with Recursive or Custom drop → generated
        // `{Name}__clone`. Mirrors `LoweringContext::clone_fn_for_ptr`.
        if matches!(
            td.metadata.drop_strategy,
            DropStrategy::Recursive | DropStrategy::Custom(_)
        ) {
            return Some(format!("{}__clone", td.name));
        }
        // Enums with cloneable variant payloads → generated
        // `{Name}__clone`. Includes Option/Result with resource payloads.
        if let TypeDefKind::Enum(ref edef) = td.kind {
            let has_cloneable_payload = edef.variants.iter().any(|v| {
                v.fields.iter().any(|f| self.is_resource_type(f.type_id))
            });
            if has_cloneable_payload {
                return Some(format!("{}__clone", td.name));
            }
        }
        None
    }

    /// Build the set of all clone fn names this module's types could call.
    /// Used by validators (`preceded_by_clone`) to recognise a producer
    /// instruction's callee as a clone fn via typed metadata, not by
    /// matching the `__clone` suffix.
    ///
    /// Iterates every registered TypeDef and collects
    /// `clone_fn_name_for_def(td)` results. Inserts both the protocol-set
    /// `metadata.clone_fn` value and the generated `{name}__clone` for
    /// user structs / cloneable enums — exactly the names that
    /// `LoweringContext::clone_fn_for_ptr` would emit.
    pub fn clone_fn_names_set(&self) -> rustc_hash::FxHashSet<String> {
        let mut set = rustc_hash::FxHashSet::default();
        for td in self.type_defs() {
            if let Some(cf) = self.clone_fn_name_for_def(td) {
                set.insert(cf);
            }
        }
        set
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
                copy_semantics: CopySemantics::Trivial,
                ..Default::default()
            },
        };
        reg.add_type_def(def);
        let retrieved = reg.get_type_def("Point").unwrap();
        assert_eq!(retrieved.name, "Point");
        assert!(matches!(retrieved.kind, TypeDefKind::Struct(_)));
        assert_eq!(retrieved.metadata.size, Some(16));
    }

    /// Phase A invariant: when a TypeDef has `collection_kind: Some(_)`, the
    /// consumers of `is_collection_type_name` / `is_resource_type` /
    /// `needs_drop` / clone-fn lookup all expect drop_strategy to be Trivial
    /// (a resource collection always frees through a runtime helper) and
    /// `clone_fn` to be Some. Locks the metadata shape at unit-test time so
    /// future drift trips here rather than producing a silent double-free.
    #[test]
    fn collection_typedef_metadata_invariant() {
        // Build a TypeRegistry with the canonical built-in collection-shaped
        // TypeDefs (mirroring src/ir/lowering/mod.rs's GorgetArray /
        // GorgetMap / GorgetSet registration). If we ever hand-construct a
        // TypeDef with collection_kind set, it MUST have drop_strategy and
        // clone_fn populated.
        let cases: &[(&str, CollectionKind, &str, &str)] = &[
            ("GorgetArray", CollectionKind::Array, "gorget_array_free", "gorget_array_clone"),
            ("GorgetMap", CollectionKind::Map, "gorget_map_free", "gorget_map_clone"),
            ("GorgetSet", CollectionKind::Set, "gorget_set_free", "gorget_set_clone"),
        ];
        for (name, kind, drop_fn, clone_fn) in cases {
            let td = TypeDef {
                name: name.to_string(),
                kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
                metadata: TypeMetadata {
                    drop_strategy: DropStrategy::Trivial(drop_fn.to_string()),
                    copy_semantics: CopySemantics::Resource,
                    clone_fn: Some(clone_fn.to_string()),
                    collection_kind: Some(*kind),
                    ..Default::default()
                },
            };
            assert!(td.metadata.collection_kind.is_some(), "{name} missing collection_kind");
            assert!(matches!(td.metadata.drop_strategy, DropStrategy::Trivial(_)),
                "{name} must have Trivial drop strategy");
            assert!(td.metadata.clone_fn.is_some(), "{name} missing clone_fn");
            assert_eq!(td.metadata.copy_semantics, CopySemantics::Resource,
                "{name} must be Resource");
        }
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

    /// Tier 1c: helper returns `(Recursive, Resource)` for a struct whose
    /// field is a registered Resource type.
    #[test]
    fn compute_drop_strategy_struct_with_resource_field() {
        let mut reg = TypeRegistry::new();
        reg.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = reg.insert(GirType::Named("OwnedBuf".into()));

        let fields = vec![
            StructField { name: "a".into(), type_id: I64_TYPE },
            StructField { name: "b".into(), type_id: buf_id },
        ];
        let (drop, copy) = reg.compute_drop_strategy_for_struct(&fields);
        assert_eq!(drop, DropStrategy::Recursive);
        assert_eq!(copy, CopySemantics::Resource);
    }

    /// Tier 1c: helper returns `(None, Trivial)` for an all-primitive struct.
    #[test]
    fn compute_drop_strategy_struct_primitives_only() {
        let reg = TypeRegistry::new();
        let fields = vec![
            StructField { name: "x".into(), type_id: F64_TYPE },
            StructField { name: "y".into(), type_id: F64_TYPE },
            StructField { name: "z".into(), type_id: I64_TYPE },
        ];
        let (drop, copy) = reg.compute_drop_strategy_for_struct(&fields);
        assert_eq!(drop, DropStrategy::None);
        assert_eq!(copy, CopySemantics::Trivial);
    }

    /// Tier 1c: helper returns `(Recursive, Resource)` for an enum whose
    /// variant payload is a registered Resource type.
    #[test]
    fn compute_drop_strategy_enum_with_resource_payload() {
        let mut reg = TypeRegistry::new();
        reg.add_type_def(TypeDef {
            name: "OwnedBuf".into(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                drop_strategy: DropStrategy::Trivial("buf_free".into()),
                copy_semantics: CopySemantics::Resource,
                ..Default::default()
            },
        });
        let buf_id = reg.insert(GirType::Named("OwnedBuf".into()));

        let variants = vec![
            EnumVariant {
                name: "Some".into(),
                fields: vec![StructField { name: "_0".into(), type_id: buf_id }],
            },
            EnumVariant { name: "None".into(), fields: vec![] },
        ];
        let (drop, copy) = reg.compute_drop_strategy_for_enum(&variants);
        assert_eq!(drop, DropStrategy::Recursive);
        assert_eq!(copy, CopySemantics::Resource);
    }

    /// Tier 1c: helper returns `(None, Trivial)` for an enum with only
    /// primitive payloads.
    #[test]
    fn compute_drop_strategy_enum_primitives_only() {
        let reg = TypeRegistry::new();
        let variants = vec![
            EnumVariant {
                name: "Ok".into(),
                fields: vec![StructField { name: "_0".into(), type_id: I64_TYPE }],
            },
            EnumVariant {
                name: "Error".into(),
                fields: vec![StructField { name: "_0".into(), type_id: I64_TYPE }],
            },
        ];
        let (drop, copy) = reg.compute_drop_strategy_for_enum(&variants);
        assert_eq!(drop, DropStrategy::None);
        assert_eq!(copy, CopySemantics::Trivial);
    }
}
