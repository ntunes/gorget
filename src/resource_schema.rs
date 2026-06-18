//! Rust mirror of `compiler/data/schema.gg`.
//!
//! Hand-written. Any field change here MUST bump `SCHEMA_VERSION` in
//! `compiler/data/resources.gg` and update the corresponding declaration
//! in `compiler/data/schema.gg`.
//!
//! This mirror retires when self-host replaces Rust as the canonical
//! compiler. Until then, the Gorget side is the source of truth — this
//! Rust file exists so that the loader at `src/ir/resources.rs` can
//! produce typed values for Rust consumers.

// ── Lookup-key strategy ─────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchKind {
    Exact(String),
    Prefix(String),
}

// ── Resource-metadata axes ──────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CopySemantics {
    Trivial,
    Resource,
    RefCounted,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionKind {
    NotCollection,
    Vector,
    Deque,
    Heap,
    Dict,
    /// `Set[T]` — ORDERED unique elements (insertion-order preserving).
    OrderedSet,
    /// `HashSet[T]` / `GorgetSet` — UNORDERED unique elements.
    HashSet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoxKind {
    NotBox,
    RegularBox,
    TraitBox,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LirType {
    StructBase,
    Ptr,
}

#[derive(Debug, Clone)]
pub struct ResourceMetadata {
    pub runtime_name: String,
    pub size_bytes: u32,
    pub lir_type: LirType,
    pub drop_fn: Option<String>,
    pub clone_fn: Option<String>,
    pub materialize_fn: Option<String>,
    pub copy_semantics: CopySemantics,
    pub collection_kind: CollectionKind,
    pub box_kind: BoxKind,
    pub opaque_handle: bool,
    pub method_prefix: Option<String>,
    /// C typedef target when it diverges from `runtime_name`; `None`
    /// means "use `runtime_name`". Used at the C-emit boundary in
    /// `emit_wrapper_typedef` for opaque-handle families whose C
    /// spelling is `GorgetMutex*` / `gorget_guard_t` / etc. — names the
    /// schema's `runtime_name` ("Mutex", "Guard") does not carry. See
    /// `docs/devbook/18-runtime-abi.md` (the resource table).
    pub c_typedef_name: Option<String>,
    pub is_typed_constructor: bool,
}

#[derive(Debug, Clone)]
pub struct ResourceEntry {
    pub match_on: Vec<MatchKind>,
    pub metadata: ResourceMetadata,
}

// ── Runtime-fn axes ─────────────────────────────────────────────────
//
// Mirrors `src/lir/runtime.rs`'s `CRuntimeType`/`AbiKind`/`SideEffects`/
// `RuntimeSig` deliberately. The schema is upstream of the LIR layer
// that consumes it; reusing types from `lir/runtime.rs` would invert
// layering (see schema.gg comment on the same point). The duplication
// retires when self-host replaces Rust.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CRuntimeType {
    Void, Bool,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Size, Ptr, VoidElem, CStr,
    Str, Array, Map, Set, Regex, Match,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AbiKind {
    Auto, Scalar, Ptr, Opaque,
    GorgetString, VoidElem, CStr, ByValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SideEffects {
    Pure, ReadOnly, Allocates,
    Mutates, Io, Aborts, Concurrent, Unknown,
}

#[derive(Debug, Clone)]
pub struct RuntimeParam {
    pub ty: CRuntimeType,
    pub abi: AbiKind,
}

#[derive(Debug, Clone)]
pub struct RuntimeFn {
    pub c_name: String,
    pub params: Vec<RuntimeParam>,
    pub ret: CRuntimeType,
    pub side_effects: SideEffects,
    pub returns_fresh: bool,
}

// ── Top-level table ─────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct ResourceTable {
    pub schema_version: u32,
    pub resources: Vec<ResourceEntry>,
    pub runtime_fns: Vec<RuntimeFn>,
}

impl ResourceTable {
    /// Look up resource metadata by mangled type name. Returns the first
    /// entry whose `match_on` list contains a matching key (Exact name
    /// equality OR Prefix `starts_with`). Match precedence is declaration
    /// order in `resources.gg` — exact names should precede prefix
    /// catch-alls when both could fire (per the spike's ordering).
    pub fn lookup(&self, name: &str) -> Option<&ResourceMetadata> {
        for entry in &self.resources {
            for key in &entry.match_on {
                let hit = match key {
                    MatchKind::Exact(s) => s == name,
                    MatchKind::Prefix(p) => name.starts_with(p),
                };
                if hit {
                    return Some(&entry.metadata);
                }
            }
        }
        None
    }

    /// Look up a runtime function by exact C symbol name.
    pub fn runtime_fn(&self, c_name: &str) -> Option<&RuntimeFn> {
        self.runtime_fns.iter().find(|f| f.c_name == c_name)
    }
}
