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

/// Which lock-guard wrapper a TypeId is, and whether writes through it are
/// permitted. Replaces the `guard_inner_suffix` name-prefix test
/// (`docs/devbook/24-layering-discipline.md` rule 2: facts cross boundaries as
/// typed fields, never as name prefixes).
///
/// POLARITY LIVES HERE, not on `ResourceMetadata`: all three prefixes share
/// `runtime_name = "Guard"` in the resource schema, so that channel cannot
/// distinguish read-only from writable. This enum can (plain enum, no schema
/// gate).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GuardKind {
    /// `Guard[T]` from `Mutex.lock()` — writable.
    Mutex,
    /// `ReadGuard[T]` from `RWLock.read()` — READ-ONLY; write places refuse.
    Read,
    /// `WriteGuard[T]` from `RWLock.write()` — writable.
    Write,
}

impl GuardKind {
    /// The ONE registration-time name match (`for_builtin_name` precedent).
    /// Returns the kind and the mangled inner C suffix.
    pub fn classify(name: &str) -> Option<GuardKind> {
        if name.starts_with("Guard__") {
            Some(GuardKind::Mutex)
        } else if name.starts_with("ReadGuard__") {
            Some(GuardKind::Read)
        } else if name.starts_with("WriteGuard__") {
            Some(GuardKind::Write)
        } else {
            None
        }
    }

    /// Writes through this guard are forbidden (`ReadGuard`).
    pub fn is_read_only(self) -> bool {
        matches!(self, GuardKind::Read)
    }

    /// The mangled inner C suffix of a guard's registered name.
    ///
    /// ⚠ RESIDUAL, deliberately NOT retired by this channel. The guarded
    /// value's TypeId is NOT available at `register_named` — the funnel with
    /// total coverage (see there) receives only `(name, type_id)`, and the two
    /// mint paths that DO know the inner type (`ensure_*_type_def`,
    /// `stmts/mod.rs`'s ArcMutex arm) are a strict subset. Resolving the suffix
    /// eagerly at registration would freeze a possibly-unregistered inner name
    /// to the `I64_TYPE` fallback, which is worse than resolving late.
    ///
    /// So the SEMANTIC axis (is-a-guard + polarity) is typed, and the inner
    /// TYPE stays a registration-time-name round-trip resolved at the read
    /// site — the same boundary and the same precedent as
    /// `emit_guard_get_ptr`'s `format!("{name}__get_ptr")`, where the mangled
    /// name IS the contract with the runtime.
    pub fn inner_suffix(name: &str) -> Option<&str> {
        name.strip_prefix("Guard__")
            .or_else(|| name.strip_prefix("ReadGuard__"))
            .or_else(|| name.strip_prefix("WriteGuard__"))
    }
}

/// Maps AST types to GIR TypeIds.
pub struct TypeMapper {
    /// `String` (owned) maps to Named("GorgetString") for string interpolation results.
    pub owned_string_type: TypeId,
    /// Cache of Named type → GIR TypeId.
    ///
    /// PRIVATE ON PURPOSE. `register_named` is the sole writer; `type_name_for_id`
    /// is this map's inverse. Keeping the field private is what makes
    /// `guard_types` provably as complete as the name test it replaces.
    named_types: FxHashMap<String, TypeId>,
    /// Typed guard channel: TypeId → `GuardKind`. Written at the one funnel
    /// (`register_named`), read via `LoweringContext::guard_of`.
    pub guard_types: FxHashMap<TypeId, GuardKind>,
    /// Builtin types registered during `map_ast_type_mut`, pending fn_sigs population.
    pub deferred_builtins: Vec<DeferredBuiltin>,
    /// Typed payload channel for `Thread[T]` handle types: the name-deduped
    /// `Thread__{T}` TypeId → T's TypeId (`UNIT_TYPE` for `Thread[void]`).
    /// Written at every Thread-handle MINT site — the protocol branch of
    /// `map_ast_type_mut` (annotations / params / fields, where `elem` is in
    /// hand) and the `thread_spawn` intrinsic (unannotated spawns,
    /// `exprs/calls.rs`). Read by the join/id method intercept
    /// (`exprs/methods.rs`) so the join result type comes from typed
    /// metadata, NOT from slicing the payload name out of the `Thread__`
    /// prefix and re-deriving a TypeId from it (layering rule 2, "typed
    /// metadata, never name-matching"). The receiver's type NAME is still
    /// used to spell the `Thread__{T}__join`/`__id` helper symbols — that is
    /// the symbol axis, where the name IS the contract with the emitted
    /// helpers.
    pub thread_payload_types: FxHashMap<TypeId, TypeId>,
    /// TRACK K: mangled `Callable__T_args` name → the original signature
    /// (params, param ownerships, return). Populated by
    /// `register_callable_inner_if_any` (via `register_callable_alias`) when
    /// the AST-level `Type::Function` inner is still in scope. Read by
    /// `infer_collection_element_type` so `Vector[Callable[T(P)]]` /
    /// `Dict[K, Callable[T(P)]]` element inference produces a
    /// full-signature FnPtr (not the empty-params placeholder that used
    /// to segfault at `arr[0](&a)` on `Callable[void(int &)]` elements).
    pub callable_alias_sigs: FxHashMap<String, (Vec<TypeId>, Vec<crate::parser::ast::Ownership>, TypeId)>,
}

impl TypeMapper {
    pub fn new(registry: &mut TypeRegistry) -> Self {
        // Register GorgetString with Move semantics + trivial drop (gorget_string_free).
        // Phase A: populate clone_fn / clone_inplace_fn / materialize_fn so consumers
        // read from metadata instead of name-matching against "GorgetString".
        registry.add_type_def(TypeDef {
            name: "GorgetString".to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: Some(32),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("gorget_string_free".to_string()),
                copy_semantics: CopySemantics::Resource,
                clone_fn: Some("gorget_string_clone_to_owned".to_string()),
                clone_inplace_fn: Some("gorget_string_clone_inplace".to_string()),
                materialize_fn: Some("gorget_string_materialize_inplace".to_string()),
                // String is the only Phase-1 type whose runtime supports
                // drop-safe cap=0 views — the lazy loop-carried CoW bind
                // eligibility axis (see `TypeMetadata::borrow_view_fn`).
                borrow_view_fn: Some("gorget_string_borrow_view".to_string()),
                ..Default::default()
            },
        });
        let owned_string_type = registry.insert(GirType::Named("GorgetString".to_string()));
        Self {
            owned_string_type,
            named_types: FxHashMap::default(),
            guard_types: FxHashMap::default(),
            deferred_builtins: Vec::new(),
            thread_payload_types: FxHashMap::default(),
            callable_alias_sigs: FxHashMap::default(),
        }
    }

    /// Map an AST `Type` to a GIR `TypeId`.
    ///
    /// Returns `UNIT_TYPE` for types not yet registered. Use `try_map_ast_type`
    /// when you need to distinguish "genuinely void" from "unknown type."
    ///
    /// **Local-form invariant (Phase A residual #1, sub-TODO 1b):** for the
    /// Callable family (`Callable[T(P)]`, `MutCallable`, `ConsumeCallable`),
    /// returns `UNIT_TYPE` (the void* __callable_N ABI). The `Function` and
    /// `Ref` variants follow the same pattern via `try_map_ast_type` — see
    /// the comment there. Callers that need a non-void TypeId at a local
    /// declaration must fall back to `map_ast_type_mut` (see VarDecl path
    /// in `stmts/mod.rs:240`).
    pub fn map_ast_type(&self, ty: &Type) -> TypeId {
        self.try_map_ast_type(ty).unwrap_or(UNIT_TYPE)
    }

    /// Try to map an AST `Type` to a GIR `TypeId`.
    ///
    /// Returns `None` when the type is not registered (not yet monomorphized,
    /// unresolved generic, function pointer, etc.). Unlike `map_ast_type`, this
    /// lets callers distinguish "genuinely void" from "unknown type."
    ///
    /// **Local-form invariant — types with stored-vs-local-form distinctions
    /// (Phase A residual #1, sub-TODO 1b audit, 2026-05-05):**
    ///
    /// - `Callable[T(P)]` / `MutCallable[T(P)]` / `ConsumeCallable[T(P)]`:
    ///   returns `None` so callers fall through to `map_ast_type_mut`, which
    ///   produces a fresh `GirType::FnPtr`. The Named form
    ///   `Callable__GorgetClosure` is reserved for in-collection positions
    ///   (where the runtime needs typed elem_size / elem_drop / elem_clone).
    ///
    /// - `Type::Function`: returns `None`. The resulting `UNIT_TYPE` activates
    ///   the `void* __callable_N` ABI, which is the intentional design (closure
    ///   values are passed as 16-byte `GorgetClosure` structs but spelled as
    ///   `void*` at the C ABI).
    ///
    ///   ⚠ THAT INCLUDES LOCAL POSITIONS — there is NO `FnPtr` fallback for the
    ///   bare function-type spelling. `lower_var_decl`'s `UNIT_TYPE` recovery
    ///   (`stmts/mod.rs`) has arms only for `ast::Type::Named` with non-empty
    ///   generic args and for a non-empty `ast::Type::Tuple`; an
    ///   `ast::Type::Function` declaration falls through the `_` arm and the
    ///   local KEEPS `UNIT_TYPE`. Verification command:
    ///   `gg build --emit-gir` on `int(int) f = dbl` prints `_1: unit`, while
    ///   the `Callable[int(int)]` spelling of the same binding prints
    ///   `_1: fn(i64) -> i64` (it takes the `Named` arm above).
    ///   So only the `Callable`/`MutCallable`/`ConsumeCallable` family reaches
    ///   `map_ast_type_mut` → `FnPtr`, and only at LOCAL positions: a
    ///   `Callable[..]` PARAMETER is `unit`, and a `Callable[..] &` PARAMETER is
    ///   `*mut unit` — which is why neither the `UNIT_TYPE` nor the `FnPtr`
    ///   dispatch arm in `exprs/calls.rs` fires for the latter, and the call
    ///   falls through to a direct call on the parameter's NAME (the filed
    ///   `Callable`-costume ICE; see TODO.md).
    ///
    /// - `Type::Ref` (ownership-suffix variant `T &` from generic args /
    ///   return positions; NOT `Param.ownership = MutableBorrow` which
    ///   doesn't go through `Type::Ref`): returns `None`. The mutable path
    ///   treats `Type::Ref` as transparent (same TypeId as inner). Today's
    ///   only consumer is type-arg positions like `Vector[T &]` (iterator-
    ///   intent marker); callers that need `Ptr(T)` at field/local positions
    ///   should write the AST as `Ref[T]` (which is `Type::Named` with
    ///   `name = "Ref"` and parses through `map_ast_type_mut`'s
    ///   `Ref`/`MutRef` branch into `GirType::Ptr`/`GirType::MutPtr`).
    ///
    /// **Audit conclusion (Phase A residual #2, 2026-05-05):** these are the
    /// ONLY three Type variants with a stored-vs-local-form distinction. No
    /// other type (Box, Shared, Weak, Mutex, Channel, Vector, Dict, Set, …)
    /// has this asymmetry — they all flow through `lookup_protocol(base)` →
    /// `get_or_register(&mangled, …)` which caches in `named_types`
    /// uniformly. `map_type_with_subs` (`context.rs`) honors the Callable
    /// invariant explicitly at its `matches!(base, "Callable" | …)` early
    /// return; `Type::Function` and `Type::Ref` inherit it via that function's
    /// trailing `self.type_mapper.map_ast_type(ty)` fallthrough. (Both were
    /// cited by line number and both had rotted; cite by symbol.)
    pub fn try_map_ast_type(&self, ty: &Type) -> Option<TypeId> {
        match ty {
            Type::Primitive(prim) => Some(self.map_primitive(prim)),
            Type::Inferred => panic!("BUG: Inferred type should be resolved before GIR lowering"),
            Type::Named { name, generic_args } => {
                if !generic_args.is_empty() {
                    // Ref[T] / MutRef[T] at field positions — can't insert from
                    // the immutable path, caller should use map_ast_type_mut.
                    let base = name.node.as_str();
                    if (base == "Ref" || base == "MutRef") && generic_args.len() == 1 {
                        return None;
                    }
                    // Callable/MutCallable/ConsumeCallable lower to FnPtr at locals
                    // and to Named("Callable__…") inside collections. The mangled
                    // name may have been cached as Named via resolve_inner_type
                    // (Option__Ref__Callable__… unwrap path). For local declarations
                    // we want a fresh FnPtr — return None to force the mut fallback.
                    if matches!(base, "Callable" | "MutCallable" | "ConsumeCallable") {
                        return None;
                    }
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
                    // Ref[T] / MutRef[T] at field positions — map to GirType::Ptr/MutPtr.
                    // Borrow-field feature (Phase 1). Does NOT cache in named_types:
                    // Ptr/MutPtr types are interchangeable at the GIR level — each
                    // call-site gets a fresh TypeId pointing to the same pointee.
                    let base = name.node.as_str();
                    if (base == "Ref" || base == "MutRef") && generic_args.len() == 1 {
                        let inner = self.map_ast_type_mut(&generic_args[0].node, registry);
                        let gir_ty = if base == "Ref" {
                            GirType::Ptr(inner)
                        } else {
                            GirType::MutPtr(inner)
                        };
                        return registry.insert(gir_ty);
                    }
                    let mangled = mangle_generic_name(&name.node, generic_args);
                    // Callable/MutCallable/ConsumeCallable bypass named_types caching:
                    // a Callable[T()] local declaration must always lower to GirType::FnPtr,
                    // even after `Callable__GorgetClosure` was registered as Named via
                    // resolve_inner_type for an Option__Ref__Callable__… unwrap path.
                    if !matches!(base, "Callable" | "MutCallable" | "ConsumeCallable") {
                        if let Some(&id) = self.named_types.get(&mangled) {
                            return id;
                        }
                    }
                    // Auto-register Option[T] and Result[T, E] types.
                    // Coherence-at-construction (Tier 1c, `docs/devbook/25-structural-guards.md`):
                    // make_option_type_def / make_result_type_def take the
                    // registry so they read the inner type's drop-strategy
                    // and propagate Recursive + Resource into the wrapper's
                    // metadata at registration time. Bypassing get_or_register
                    // here keeps its closure signature `FnOnce(&str)` for the
                    // 11 other callers that don't need registry access.
                    if base == "Option" && generic_args.len() == 1 {
                        let inner_type = self.map_ast_type_mut(&generic_args[0].node, registry);
                        if let Some(&id) = self.named_types.get(&mangled) { return id; }
                        let td = make_option_type_def(&mangled, inner_type, registry);
                        registry.add_type_def(td);
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.register_named(mangled, type_id);
                        return type_id;
                    }
                    if base == "Result" && generic_args.len() == 2 {
                        let ok_type = self.map_ast_type_mut(&generic_args[0].node, registry);
                        let err_type = self.map_ast_type_mut(&generic_args[1].node, registry);
                        if let Some(&id) = self.named_types.get(&mangled) { return id; }
                        let td = make_result_type_def(&mangled, ok_type, err_type, registry);
                        registry.add_type_def(td);
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.register_named(mangled, type_id);
                        return type_id;
                    }
                    // Box[T] auto-registration. Without this, Box[T] surfacing
                    // inside another wrapper (e.g. struct field `Option[Box[R]]`,
                    // collection element `Vector[Box[R]]`) falls through to
                    // UNIT_TYPE here, and the wrapper is registered with
                    // `Some._0: Unit`. A later remonomorphize pass fixes the
                    // field type but DOES NOT recompute the wrapper's metadata
                    // — so `Option__Box__R` ends up with `drop: None, copy:
                    // Trivial`, and the Tier 1a drop-completeness validator
                    // fires on any struct combining `Option[Box[T]]` with
                    // another droppable field (Snag #27, 2026-05-10).
                    // Coherence-at-construction: write Box's typed metadata
                    // (Resource + Trivial("free") + is_box) here so any
                    // wrapper computing its drop_strategy from inner.needs_drop
                    // sees the true answer at first registration. Mirrors the
                    // Box arm in `register_collection_alias`; the two paths
                    // exist because field-type pre-registration takes a
                    // different entry point than wrapper-arg recursion.
                    if base == "Box" && generic_args.len() == 1 {
                        let inner_type = self.map_ast_type_mut(&generic_args[0].node, registry);
                        if let Some(&id) = self.named_types.get(&mangled) { return id; }
                        let type_def = TypeDef {
                            name: mangled.clone(),
                            kind: TypeDefKind::Struct(StructDef {
                                fields: vec![StructField { name: "_0".to_string(), type_id: inner_type }],
                            }),
                            metadata: TypeMetadata {
                                copy_semantics: CopySemantics::Resource,
                                drop_strategy: DropStrategy::Trivial("free".to_string()),
                                is_box: true,
                                ..Default::default()
                            },
                        };
                        registry.add_type_def(type_def);
                        let type_id = registry.insert(GirType::Named(mangled.clone()));
                        self.register_named(mangled, type_id);
                        return type_id;
                    }
                    // Callable/MutCallable/ConsumeCallable generics: return a FnPtr TypeId
                    // so locals declared as Callable[T(P)] get GorgetClosure C type and
                    // use __gorget_closure_call_N dispatch.
                    // NOT cached in named_types so map_ast_type (immutable, used for
                    // function parameters) still returns UNIT_TYPE → void* __callable_N ABI.
                    // Must precede the protocol-table branch — Callable's protocol
                    // exists (so consumers can read c_runtime_alias / drop / clone
                    // for the Named form) but we don't want the protocol path's
                    // get_or_register to inject a Named TypeDef for the local form.
                    if matches!(base, "Callable" | "MutCallable" | "ConsumeCallable") {
                        return if generic_args.len() == 1 {
                            self.map_ast_type_mut(&generic_args[0].node, registry)
                        } else {
                            registry.insert(GirType::FnPtr { params: vec![], return_type: UNIT_TYPE, param_ownerships: vec![] })
                        };
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

                        // Phase A residual #1, sub-TODO 1b extension
                        // (2026-05-05): the Callable family at the LOCAL form
                        // returns FnPtr without inserting a Named TypeDef
                        // (lines 219-225 above). When a Callable surfaces as
                        // an inner type-arg of a smart pointer / collection
                        // (`Shared[Callable[int()]]`, `Box[Callable[T(P)]]`,
                        // `Vector[Callable[…]]`, …), the C backend later needs
                        // a typedef for `Callable__GorgetClosure` so that
                        // `Shared__Callable__GorgetClosure__new(Callable__GorgetClosure)`
                        // resolves. `register_collection_alias` walks args
                        // and calls `register_callable_inner_if_any` for the
                        // Vector/Dict/Box cases handled there; but `Shared`,
                        // `Weak`, `Mutex`, `Channel`, `RWLock`, etc. flow
                        // through the protocol-table branch directly and
                        // bypass that walk. Mirror the same eager-register
                        // here so every smart-pointer / opaque-handle path
                        // that wraps a Callable surfaces the inner Named
                        // TypeDef. Idempotent: `register_callable_alias`
                        // checks `named_types.contains` before inserting.
                        for arg in generic_args {
                            register_callable_inner_if_any(self, registry, &arg.node);
                        }

                        // Drop strategy: protocol provides the free function, but some
                        // types use per-monomorphization drop wrappers.
                        let drop_strat = match base {
                            "Guard" | "Shared" | "Weak" | "Channel"
                            | "ReadGuard" | "WriteGuard" | "Mutex" | "RWLock" =>
                                DropStrategy::Trivial(format!("{mangled}__drop")),
                            _ => match protocol.drop_fn {
                                Some(f) => DropStrategy::Trivial(f.to_string()),
                                None => DropStrategy::None,
                            },
                        };

                        let type_id = self.get_or_register(&mangled, registry, |n| {
                            make_opaque_type_def(n, protocol.copy_semantics, drop_strat)
                        });

                        // Thread[T]: record the typed payload channel at the
                        // handle's mint site (resolve once, write through —
                        // `elem` is the payload TypeId resolved above). The
                        // join/id intercept reads this instead of re-deriving
                        // the payload from the `Thread__{T}` name suffix.
                        // Idempotent: the mangled name dedupes to one TypeId,
                        // so a re-insert writes the same pair.
                        if base == "Thread" {
                            self.thread_payload_types.insert(type_id, elem);
                        }

                        // Set protocol-derived metadata on the TypeDef.
                        //
                        // Per-monomorphization protocols (Shared / Weak /
                        // Channel / Guard / ReadGuard / WriteGuard) keep
                        // `protocol.clone_fn = None` because the runtime
                        // symbol is per-instantiation (`{Mangled}__clone`,
                        // not a single static C symbol). The lowering
                        // unconditionally emits `Call("{Mangled}__clone")`
                        // for the receiver.clone() shape (`exprs/methods.rs`
                        // Shared/Weak arms) and the deferred fn-sig table
                        // (`mod.rs:2977`). Phase 2E (consume-site validator
                        // typed migration) reads `metadata.clone_fn` via
                        // `TypeRegistry::clone_fn_names_set()` and needs to
                        // see the per-mono name to recognise the producer.
                        // Write it here — same site that already writes the
                        // per-mono `drop_strategy` (`{Mangled}__drop`).
                        if let Some(td) = registry.get_type_def_mut(&mangled) {
                            match base {
                                // Refcount family {Shared, Weak, Channel}: the
                                // by-VALUE incref clone_fn, set through the ONE
                                // shared writer so this annotated-type path and
                                // the ctor-path def-mint (`ensure_*_type_def`)
                                // stay byte-identical (Layering rule 3).
                                "Shared" | "Weak" | "Channel" => {
                                    td.metadata.set_refcount_clone_fn(&mangled);
                                }
                                // Guards spell the same `{mangled}__clone` for the
                                // consume-site validator's producer recognition,
                                // but are NOT refcount handles — `Resource`
                                // copy_semantics keeps them out of
                                // `is_refcount_clone_type`. Kept separate on
                                // purpose (do NOT route through the refcount
                                // setter).
                                "Guard" | "ReadGuard" | "WriteGuard" => {
                                    td.metadata.clone_fn = Some(format!("{mangled}__clone"));
                                }
                                _ => td.metadata.clone_fn = protocol.clone_fn.map(String::from),
                            }
                            td.metadata.clone_inplace_fn = protocol.clone_inplace_fn.map(String::from);
                            td.metadata.materialize_fn = protocol.materialize_fn.map(String::from);
                            td.metadata.borrow_view_fn = protocol.borrow_view_fn.map(String::from);
                            td.metadata.collection_kind = protocol.collection_kind;
                            td.metadata.c_runtime_alias = protocol.c_runtime_alias.map(String::from);
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
                        self.register_named(mangled, type_id);
                        return type_id;
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
                // Tier 1c: coherence-at-construction. A tuple holding a
                // resource-typed element is itself a resource (its drop
                // must recurse). Mirrors the `monomorphize_struct` migration.
                let (drop_strategy, copy_semantics) = registry.compute_drop_strategy_for_struct(&fields);
                let type_def = TypeDef {
                    name: mangled.clone(),
                    kind: TypeDefKind::Struct(StructDef { fields }),
                    metadata: TypeMetadata {
                        drop_strategy,
                        copy_semantics,
                        ..Default::default()
                    },
                };
                registry.add_type_def(type_def);
                let type_id = registry.insert(GirType::Named(mangled.clone()));
                self.register_named(mangled, type_id);
                type_id
            }
            Type::Function { return_type, params, param_ownerships } => {
                let ret = self.map_ast_type_mut(&return_type.node, registry);
                let param_types: Vec<TypeId> = params.iter()
                    .map(|p| self.map_ast_type_mut(&p.node, registry))
                    .collect();
                // TRACK K: preserve param ownerships through the AST→GIR
                // FnPtr boundary so the indirect-call arg-loop (`calls.rs`
                // non-identifier arm) can route `&`-args through
                // `lower_call_arg`. Pre-fix this arm dropped the sigils,
                // and `arr[0](&a)` on a `Callable[void(int &)]` element
                // segfaulted on both backends because the arg loop
                // forwarded a VALUE bit-pattern to a callee expecting a
                // pointer.
                let owns: Vec<crate::parser::ast::Ownership> = params.iter().enumerate()
                    .map(|(i, _)| param_ownerships.get(i).copied().unwrap_or(crate::parser::ast::Ownership::Borrow))
                    .collect();
                registry.insert(GirType::FnPtr { params: param_types, return_type: ret, param_ownerships: owns })
            }
            Type::Ref(inner) => {
                // At a parameter position, `T &` means a mutable borrow (Ptr(T)).
                // At a type-argument position (e.g., `Vector[T &]`), it's an
                // iterator-intent marker and must NOT become Ptr(T) — the
                // storage is still T. The resolver distinguishes the two via
                // ownership on the FunctionParam, not via Type::Ref.
                // Here (type-arg path), treat as transparent.
                self.map_ast_type_mut(&inner.node, registry)
            }
            Type::Owned(inner) => {
                self.map_ast_type_mut(&inner.node, registry)
            }
            _ => self.map_ast_type(ty),
        }
    }

    /// Register a named type that has already been added to the TypeRegistry.
    ///
    /// THE SINGLE WRITE FUNNEL for `named_types`. The map is private precisely
    /// so this is the only door: `type_name_for_id` is this map's inverse, so a
    /// TypeId is name-resolvable IF AND ONLY IF it passed through here. Any
    /// TypeId→fact channel written here therefore has EXACTLY the coverage of
    /// the name test it replaces — a bijection argument, not an enumeration of
    /// mint sites.
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn register_named(&mut self, name: String, type_id: TypeId) {
        // Registration-time classification (the `for_builtin_name` precedent:
        // ONE name match, at registration, feeding typed metadata that every
        // consumer reads instead of re-deriving from the name).
        if let Some(kind) = GuardKind::classify(&name) {
            if std::env::var("GG_GUARD_CENSUS").is_ok() {
                let loc = std::panic::Location::caller();
                eprintln!("[guard-census] {}:{} | {name} -> {type_id:?} {kind:?}",
                          loc.file(), loc.line());
            }
            self.guard_types.insert(type_id, kind);
        }
        self.named_types.insert(name, type_id);
    }

    /// Typed guard lookup — the replacement for `guard_inner_suffix(name)`.
    /// Callers peel `Ptr`/`MutPtr` first (see `LoweringContext::guard_of`) so a
    /// `Guard[T]` reached through a `&`/`!` param answers the same as a bare
    /// `Guard[T]` local — the asymmetry that made three of the four read sites
    /// miss the guard branch.
    pub fn guard_kind(&self, type_id: TypeId) -> Option<GuardKind> {
        self.guard_types.get(&type_id).copied()
    }

    /// True iff `name` is in the guard family — the WRITE-SIDE assertion helper
    /// for the miss policy (see `LoweringContext::guard_of`).
    pub fn is_guard_name(name: &str) -> bool {
        GuardKind::classify(name).is_some()
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
        self.register_named(name.to_string(), type_id);
        type_id
    }

    /// Look up a named type's GIR TypeId.
    pub fn lookup_named(&self, name: &str) -> Option<TypeId> {
        self.named_types.get(name).copied()
    }

    /// Read accessors — the map is private, these are the only doors in.
    pub fn contains_named(&self, name: &str) -> bool {
        self.named_types.contains_key(name)
    }

    pub fn iter_named(&self) -> impl Iterator<Item = (&String, &TypeId)> {
        self.named_types.iter()
    }

    pub fn named_snapshot(&self) -> FxHashMap<String, TypeId> {
        self.named_types.clone()
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
            PrimitiveType::CStr => self.owned_string_type,
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
        } else if type_id == self.owned_string_type {
            "%s"
        } else {
            "%lld" // fallback
        }
    }

    /// Returns true if this type needs special printf handling (e.g., Str → two args).
    pub fn is_string_type(&self, type_id: TypeId) -> bool {
        type_id == self.owned_string_type
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
        mapper.register_named(name.clone(), placeholder_id);
    }

    // Pre-register any generic types used as field types (e.g., Option[Color])
    for f in &struct_def.fields {
        ensure_generic_field_type_registered(mapper, registry, &f.node.type_.node, generic_templates);
    }

    // Map fields. String fields keep owned_string_type (GorgetString) so the struct
    // OWNS its strings and recursive drop frees them. Field LOADS return str_type (Str
    // view) to prevent shallow-copy double-frees — this is handled in lower_field_access.
    // Uses `map_ast_type_mut` so user-written `Ref[T]` / `MutRef[T]` fields get
    // registered as `GirType::Ptr(T)` / `GirType::MutPtr(T)` instead of falling
    // back to UNIT_TYPE — the immutable path can't insert Ptr types.
    let fields: Vec<StructField> = struct_def.fields.iter()
        .map(|f| {
            let field_type = mapper.map_ast_type_mut(&f.node.type_.node, registry);
            StructField {
                name: f.node.name.node.clone(),
                type_id: field_type,
            }
        })
        .collect();

    // Tier 1c: compute coherence-at-construction drop metadata. The
    // post-hoc `upgrade_types_from_fields` pass already catches user
    // structs registered before function lowering; this helper closes
    // the timing window between registration and the upgrade pass.
    let (drop_strategy, copy_semantics) = registry.compute_drop_strategy_for_struct(&fields);
    let type_def = TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields }),
        metadata: TypeMetadata {
            drop_strategy,
            copy_semantics,
            ..Default::default()
        },
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
        mapper.register_named(name.clone(), placeholder_id);
    }

    let inner_type = mapper.map_ast_type(&nt.inner_type.node);
    let fields = vec![StructField {
        name: "_0".to_string(),
        type_id: inner_type,
    }];
    // Tier 1c: compute coherence-at-construction drop metadata.
    let (drop_strategy, copy_semantics) = registry.compute_drop_strategy_for_struct(&fields);
    let type_def = TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Struct(StructDef { fields }),
        metadata: TypeMetadata {
            drop_strategy,
            copy_semantics,
            ..Default::default()
        },
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
            "Vector" | "Deque" | "Dict" | "HashMap" | "Set" | "HashSet" | "Box" => {
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
///
/// Routes through `make_option_type_def` so payload drop-strategy propagates
/// into the wrapper's metadata at registration time (Tier 1c,
/// `docs/devbook/25-structural-guards.md`). Until 2026-05-10 this site inlined
/// the TypeDef literal with
/// `..Default::default()` metadata — a parallel registration path that
/// silently bypassed the `make_option_type_def` helper. The result was
/// `Option__Box__T` registered with `drop: None, copy: Trivial` even when
/// the inner type was Resource, and the drop-completeness validator firing
/// on any struct combining `Option[Box[T]]` with another droppable field.
fn register_builtin_option(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    type_args: &[crate::span::Spanned<ast::Type>],
    mangled_name: &str,
) {
    // Use map_ast_type_mut to ensure generic inner types (like Task[void])
    // get registered so the Option TypeDef references a valid TypeId.
    let inner_type = mapper.map_ast_type_mut(&type_args[0].node, registry);
    let type_def = make_option_type_def(mangled_name, inner_type, registry);
    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.register_named(mangled_name.to_string(), type_id);
}

/// Register a monomorphized Result[T, E] type (built-in: Ok(T) | Error(E)).
///
/// Routes through `make_result_type_def` for the same coherence-at-construction
/// reason as `register_builtin_option` above.
fn register_builtin_result(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    type_args: &[crate::span::Spanned<ast::Type>],
    mangled_name: &str,
) {
    let ok_type = mapper.map_ast_type_mut(&type_args[0].node, registry);
    let err_type = mapper.map_ast_type_mut(&type_args[1].node, registry);
    let type_def = make_result_type_def(mangled_name, ok_type, err_type, registry);
    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.register_named(mangled_name.to_string(), type_id);
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
    // The C backend handles collection_type_alias for the actual C type name.
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.register_named(mangled_name.to_string(), type_id);

    // Phase A: register a TypeDef with full metadata so consumers can read
    // drop_strategy / clone_fn / clone_inplace_fn from the protocol table
    // instead of falling back to name-prefix matching.
    //
    // Historical note: an earlier comment claimed registering TypeDefs here
    // would transitively upgrade containing structs (CliParser, HttpServer)
    // to Recursive drop and cause double-frees on shallow-copy returns. The
    // upgrade scan (`upgrade_types_from_fields` in lowering/mod.rs) already
    // detects collection fields via `is_collection_type_name(field_type_name)`
    // regardless of TypeDef presence — so the upgrade fires either way.
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
                is_box: true,
                ..Default::default()
            },
        };
        registry.add_type_def(type_def);
    } else if let Some(protocol) = builtins::lookup_protocol(base_name) {
        // Vector / Dict / HashMap / Set / HashSet — pull metadata from the
        // protocol so the same fields populate as in map_ast_type_mut.
        let drop_strat = match protocol.drop_fn {
            Some(f) => DropStrategy::Trivial(f.to_string()),
            None => DropStrategy::None,
        };
        let type_def = TypeDef {
            name: mangled_name.to_string(),
            kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
            metadata: TypeMetadata {
                size: None,
                align: None,
                copy_semantics: protocol.copy_semantics,
                drop_strategy: drop_strat,
                clone_fn: protocol.clone_fn.map(String::from),
                clone_inplace_fn: protocol.clone_inplace_fn.map(String::from),
                materialize_fn: protocol.materialize_fn.map(String::from),
                borrow_view_fn: protocol.borrow_view_fn.map(String::from),
                collection_kind: protocol.collection_kind,
                enum_category: None,
                c_runtime_alias: protocol.c_runtime_alias.map(String::from),
                is_closure_env: false,
                closure_call_fn: None,
                closure_captures: Vec::new(),
                is_box: false,
            },
        };
        registry.add_type_def(type_def);
    }

    // Phase A residual #1, sub-TODO 1b: eagerly register Callable / MutCallable /
    // ConsumeCallable inner-type aliases when surfaced as collection element /
    // dict value types. Without this, the only path that registers the inner
    // Callable's TypeDef is `resolve_inner_type` (via .get / .clone / etc.
    // method-call lowering). Fixtures like httpserver_methods.gg never invoke
    // those methods on the closure-valued cells, so the Callable name only
    // appears inside the mangled `Vector__Callable__GorgetClosure__new` runtime
    // helper — and consumer sites (clone_fn_for_collection_element, etc.) rely
    // on a TypeDef sidecar fallback through LIR StructDef.c_runtime_alias.
    for arg in _type_args {
        register_callable_inner_if_any(mapper, registry, &arg.node);
    }
}

/// Register Callable family Named TypeDefs that arise as inner type arguments
/// of a collection. The local-form `Callable[T(P)]` lowers to `GirType::FnPtr`
/// at parameters/locals; the Named form (`Callable__GorgetClosure`) is what
/// gets used inside collections via mangling. This helper materializes the
/// TypeDef so consumers can read drop / clone / `c_runtime_alias` uniformly.
fn register_callable_inner_if_any(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    ty: &Type,
) {
    if let Type::Named { name, generic_args } = ty {
        let base = name.node.as_str();
        if matches!(base, "Callable" | "MutCallable" | "ConsumeCallable") {
            let mangled = mangle_generic_name(base, generic_args);
            // TRACK K: extract the sig from the Function inner AND record it
            // in the side-table before the alias is registered. This is the
            // one write site where the AST-level params + ownerships are in
            // scope; downstream `infer_collection_element_type` reads it
            // when producing the FnPtr type for a `Vector[Callable[T(P)]]`
            // element (so the read side at `calls.rs`'s non-identifier arm
            // can route each arg through `lower_call_arg`).
            if let Some(func_arg) = generic_args.first() {
                if let Type::Function { return_type, params, param_ownerships } = &func_arg.node {
                    // Compute types + owns. Idempotent — if the entry already
                    // exists (this same mangled name registered twice), the
                    // insert overwrites with byte-identical data.
                    // NB: uses map_ast_type_mut so the arg types get properly
                    // interned in the registry too.
                    let ret = mapper.map_ast_type_mut(&return_type.node, registry);
                    let param_types: Vec<TypeId> = params.iter()
                        .map(|p| mapper.map_ast_type_mut(&p.node, registry))
                        .collect();
                    let owns: Vec<crate::parser::ast::Ownership> = params.iter().enumerate()
                        .map(|(i, _)| param_ownerships.get(i).copied().unwrap_or(crate::parser::ast::Ownership::Borrow))
                        .collect();
                    mapper.callable_alias_sigs.insert(mangled.clone(), (param_types, owns, ret));
                }
            }
            register_callable_alias(mapper, registry, &mangled);
        }
        // Recurse into generic args so `Vector[Vector[Callable[T(P)]]]` and
        // `Dict[K, Box[Callable[T(P)]]]` reach the inner Callable too.
        for arg in generic_args {
            register_callable_inner_if_any(mapper, registry, &arg.node);
        }
    }
}

/// Register a Callable / MutCallable / ConsumeCallable / GorgetClosure Named
/// type alias as a TypeDef carrying the protocol's metadata. The local-form
/// `Callable[T(args)]` lowers to `GirType::FnPtr` via `map_ast_type_mut`'s
/// special case (no Named insert). The Named form arises only when a Callable
/// shows up as a collection element / dict value / Option payload — at which
/// point `resolve_inner_type` (or this helper) inserts the Named GIR type and
/// we want a TypeDef behind it so consumers (clone_fn_for_ptr, infer_drop_strategy,
/// elem_drop_fn_for_type, …) can read drop / clone / `c_runtime_alias` uniformly
/// instead of name-prefix-matching.
pub(super) fn register_callable_alias(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    mangled_name: &str,
) -> TypeId {
    // Reuse an existing Named TypeId if one was already inserted (e.g., by a
    // direct `registry.insert(GirType::Named(...))` somewhere upstream); we
    // only want to attach the TypeDef if it isn't already there.
    let type_id = if let Some(&id) = mapper.named_types.get(mangled_name) {
        id
    } else {
        let id = registry.insert(GirType::Named(mangled_name.to_string()));
        mapper.register_named(mangled_name.to_string(), id);
        id
    };

    // Idempotent: skip if a TypeDef is already attached (e.g., from a previous
    // resolve pass or an explicit registration).
    if registry.get_type_def(mangled_name).is_some() {
        return type_id;
    }

    // Pick the protocol via the mangled-name recognizer. `GorgetClosure` (the
    // runtime singleton) and `Callable__…` / `MutCallable__…` /
    // `ConsumeCallable__…` monomorphizations all share the same closure ABI.
    // Filter to only the family this helper registers — protocols whose
    // `c_runtime_alias` resolves to `"GorgetClosure"`. Other protocols
    // (Vector, Dict, …) get registered via `register_collection_alias`, not
    // here.
    let protocol = match builtins::protocol_for_mangled_name(mangled_name) {
        Some(p) if p.c_runtime_alias == Some("GorgetClosure") => p,
        _ => return type_id,
    };
    let drop_strat = match protocol.drop_fn {
        Some(f) => DropStrategy::Trivial(f.to_string()),
        None => DropStrategy::None,
    };
    let type_def = TypeDef {
        name: mangled_name.to_string(),
        kind: TypeDefKind::Struct(StructDef { fields: vec![] }),
        metadata: TypeMetadata {
            size: None,
            align: None,
            copy_semantics: protocol.copy_semantics,
            drop_strategy: drop_strat,
            clone_fn: protocol.clone_fn.map(String::from),
            clone_inplace_fn: protocol.clone_inplace_fn.map(String::from),
            materialize_fn: protocol.materialize_fn.map(String::from),
            borrow_view_fn: protocol.borrow_view_fn.map(String::from),
            collection_kind: protocol.collection_kind,
            enum_category: None,
            c_runtime_alias: protocol.c_runtime_alias.map(String::from),
            is_closure_env: false,
            closure_call_fn: None,
            closure_captures: Vec::new(),
                is_box: false,
        },
    };
    registry.add_type_def(type_def);
    type_id
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
        mapper.register_named(name.clone(), placeholder_id);
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

    // Tier 1c: compute coherence-at-construction drop metadata.
    let (drop_strategy, copy_semantics) = registry.compute_drop_strategy_for_enum(&variants);
    let type_def = TypeDef {
        name: name.clone(),
        kind: TypeDefKind::Enum(EnumDef { variants }),
        metadata: TypeMetadata {
            drop_strategy,
            copy_semantics,
            ..Default::default()
        },
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

/// Extract the mangled prefix used for `equip`-block method names
/// (`{prefix}__{method}`). Returns `None` for generic targets (handled
/// separately via monomorphization) and for types that aren't equippable
/// (Void, Tuple, Function).
///
/// For `equip String:` the prefix is `"GorgetString"`, matching what the
/// call-site resolver uses when dispatching `s.method()`. For
/// `equip int:` it's `"int64_t"`. For `equip Point:` (user struct),
/// it's just `"Point"`. Gorget-doc-level `String` / `int` / `bool` etc.
/// all parse as `Type::Primitive(_)`; without this helper the
/// equip-lowering pipeline silently dropped them (the filter sites in
/// mod.rs only matched `Type::Named`). See
/// `docs/devbook/17-c-backend.md`.
pub fn equip_target_name(ty: &Type) -> Option<String> {
    match ty {
        Type::Named { name, generic_args } => {
            if generic_args.is_empty() {
                Some(name.node.clone())
            } else {
                None // generic — handled via monomorphization
            }
        }
        Type::Primitive(prim) => match prim {
            PrimitiveType::Void => None, // not equippable
            _ => Some(mangle_type_for_name(ty)),
        },
        _ => None, // Tuple / Function / Ref / etc. — not valid equip targets
    }
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
        // Sigils at type-arg positions (`T &`, `T !`) are iterator-intent markers,
        // not distinct storage types. They collapse to the bare type during
        // monomorphization so that `Vector[String &]` and `Vector[String]` share
        // the same layout and runtime code.
        Type::Ref(inner) => mangle_type_for_name(&inner.node),
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
/// Coherence-at-construction (Tier 1c, `docs/devbook/25-structural-guards.md`): if any payload
/// type already needs dropping, the wrapper enum's metadata reflects that
/// at registration time — no reliance on the post-hoc `upgrade_types_from_fields`
/// pass, which only runs once at module-start and misses lazily-registered
/// types (e.g., `Option__Box__T` populated when struct field types are
/// processed during AST→GIR lowering). The pass remains as defence-in-depth
/// for older registration paths but is no longer load-bearing here.
fn wrapper_metadata_for_payloads(
    registry: &TypeRegistry,
    payloads: &[TypeId],
    enum_category: EnumCategory,
) -> TypeMetadata {
    let any_payload_needs_drop = payloads.iter().any(|t| registry.needs_drop(*t));
    let (copy_semantics, drop_strategy) = if any_payload_needs_drop {
        (CopySemantics::Resource, DropStrategy::Recursive)
    } else {
        (CopySemantics::Trivial, DropStrategy::None)
    };
    TypeMetadata {
        enum_category: Some(enum_category),
        copy_semantics,
        drop_strategy,
        ..Default::default()
    }
}

pub fn make_option_type_def(name: &str, inner_type: TypeId, registry: &TypeRegistry) -> TypeDef {
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
        metadata: wrapper_metadata_for_payloads(registry, &[inner_type], EnumCategory::Option),
    }
}

/// Create a Result[T, E] enum TypeDef (Ok(_0: T) | Error(_0: E)).
pub fn make_result_type_def(name: &str, ok_type: TypeId, err_type: TypeId, registry: &TypeRegistry) -> TypeDef {
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
        metadata: wrapper_metadata_for_payloads(registry, &[ok_type, err_type], EnumCategory::Result),
    }
}

/// Synthesize (and cache) the `Result[T, E]` return type for a `throws` signature.
///
/// A `T foo() throws E` function/method lowers to a `Result[T, E]` return slot.
/// The mangled wrapper name is `Result__{ok_c}__{err_c}` (driven by
/// `mangle_type_for_name`), registered once via `make_result_type_def` so the
/// wrapper's `needs_drop`/copy-semantics metadata is read from the registry.
/// Idempotent: a second call with the same `(return_type, throws_type)` returns
/// the already-registered id.
///
/// **One source of truth (devbook-24 rule 3 / Core #4).** This is the SOLE place
/// the `Result__{ok}__{err}` throws-result synthesis is spelled. The three
/// throws-sig sites — the free-fn pre-scan (`mod.rs`), the equip-method pre-scan
/// (`mod.rs`), and the method-body lowering (`functions.rs`) — all route through
/// here so a fourth path cannot drift (which is exactly how the equip-method
/// pre-scan once silently registered bare `int` instead of `Result[int, E]`,
/// yielding ill-typed C at the call site). The `tests/lints.rs`
/// `throws_result_synthesis_single_source` ratchet pins this invariant.
pub fn synthesize_throws_result_type(
    type_mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    return_type: &Type,
    throws_type: &Type,
) -> TypeId {
    let ok_type = type_mapper.map_ast_type_mut(return_type, registry);
    let err_type = type_mapper.map_ast_type_mut(throws_type, registry);
    let ok_c = mangle_type_for_name(return_type);
    let err_c = mangle_type_for_name(throws_type);
    let result_name = format!("Result__{ok_c}__{err_c}");
    if let Some(&id) = type_mapper.named_types.get(&result_name) {
        return id;
    }
    // Tier 1c: route through `make_result_type_def` so the wrapper's metadata
    // reads `needs_drop` from the registry — registers as `(Recursive, Resource)`
    // when either variant payload is droppable. Replaces a direct
    // `TypeMetadata::default()` construction that silently recorded
    // `(None, Trivial)` for `Result[T, String]`.
    let type_def = make_result_type_def(&result_name, ok_type, err_type, registry);
    registry.add_type_def(type_def);
    let type_id = registry.insert(crate::ir::types::GirType::Named(result_name.clone()));
    type_mapper.register_named(result_name, type_id);
    type_id
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
        let str_id = mapper.map_primitive(&PrimitiveType::StringType);
        assert_eq!(str_id, mapper.owned_string_type);
        assert!(matches!(reg.get(str_id), Some(GirType::Named(name)) if name == "GorgetString"));
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
        assert_eq!(mapper.format_specifier(mapper.owned_string_type), "%s");
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
                spanned(Type::Primitive(PrimitiveType::StringType)),
            ],
        );
        assert_eq!(name, "Result__GorgetString__GorgetString");
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
