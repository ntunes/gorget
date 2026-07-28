use crate::parser::ast::{self, PrimitiveType};
use crate::span::Span;

use super::errors::{SemanticError, SemanticErrorKind};
use rustc_hash::{FxHashMap, FxHashSet};

use super::ids::{DefId, TypeId};
use super::scope::{DefKind, DerefWrapperKind, ScopeTable};

/// A resolved type, separate from the parser's AST Type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedType {
    /// Primitive type (int, float, bool, etc.)
    Primitive(PrimitiveType),

    /// A user-defined struct, enum, or newtype.
    Defined(DefId),

    /// Generic instantiation: Vector[int] → Generic(DefId_of_Vector, [int_TypeId])
    Generic(DefId, Vec<TypeId>),

    /// Tuple: (int, String)
    Tuple(Vec<TypeId>),

    /// Fixed-size array: int[5]
    Array(TypeId, usize),

    /// Slice: ref int[]
    Slice(TypeId),

    /// Function type: int(int, int) or int(&MyStruct, int)
    Function {
        params: Vec<TypeId>,
        param_ownerships: Vec<crate::parser::ast::Ownership>,
        return_type: TypeId,
    },

    /// Trait object: Box[Trait] → automatic vtable dispatch
    TraitObject(DefId),

    /// Callable trait type: Callable[int(int)] → wraps the inner Function type
    CallableTrait(TypeId),

    /// Mutable callable trait type: MutCallable[int(int)]
    MutCallableTrait(TypeId),

    /// Consuming callable trait type: ConsumeCallable[int(int)]
    ConsumeCallableTrait(TypeId),

    /// Boxed callable trait object: Box[Callable[int(int)]]
    BoxedCallable { kind: ClosureKind, inner: TypeId },

    /// Borrowed reference: `Type &`
    Ref(TypeId),

    /// Owned/moved value: `Type !`
    Owned(TypeId),

    /// Type variable for inference: ?T0, ?T1, ...
    Var(u32),

    /// Error sentinel — used to avoid cascading errors.
    Error,

    /// Void (no value).
    Void,

    /// Never type (for diverging expressions like return/throw).
    Never,
}

/// Classification of a closure's capture behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosureKind {
    /// Pure / immutable captures only.
    Callable,
    /// May mutate captured variables.
    MutCallable,
    /// Consumes captured variables (move closure).
    ConsumeCallable,
}

impl ClosureKind {
    /// Human-readable name for error messages.
    pub fn name(self) -> &'static str {
        match self {
            ClosureKind::Callable => "Callable",
            ClosureKind::MutCallable => "MutCallable",
            ClosureKind::ConsumeCallable => "ConsumeCallable",
        }
    }

    /// Returns true if `self` is compatible with `expected`.
    /// Hierarchy: Callable → MutCallable → ConsumeCallable (upward coercion OK).
    pub fn is_compatible_with(self, expected: Self) -> bool {
        match expected {
            ClosureKind::Callable => self == ClosureKind::Callable,
            ClosureKind::MutCallable => self != ClosureKind::ConsumeCallable,
            ClosureKind::ConsumeCallable => true,
        }
    }
}

/// Stores all resolved types, indexed by TypeId.
pub struct TypeTable {
    types: Vec<ResolvedType>,
    /// Cache for Defined(DefId) → TypeId deduplication.
    defined_cache: FxHashMap<DefId, TypeId>,
    /// Cache for Generic(DefId, Vec<TypeId>) → TypeId deduplication.
    generic_cache: FxHashMap<(DefId, Vec<TypeId>), TypeId>,
    // Pre-allocated IDs for common types
    pub void_id: TypeId,
    pub bool_id: TypeId,
    pub int_id: TypeId,
    pub float_id: TypeId,
    pub string_id: TypeId,
    pub owned_string_id: TypeId,
    pub cstr_id: TypeId,
    pub error_id: TypeId,
    pub never_id: TypeId,
    /// O(1) lookup table indexed by `PrimitiveType as usize`. Populated in
    /// `new()` so `primitive_id` never falls back to a linear scan, even for
    /// the rare sized numeric variants (Int8..Uint64, Float32/64).
    primitive_ids: [TypeId; PRIMITIVE_TYPE_COUNT],
}

/// Number of variants in `PrimitiveType`. Must equal `PrimitiveType` variant
/// count; the `primitive_id` lookup table is sized exactly to this.
const PRIMITIVE_TYPE_COUNT: usize = 17;

impl TypeTable {
    pub fn new() -> Self {
        let mut types = Vec::new();

        let void_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Void);

        let bool_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::Bool));

        let int_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::Int));

        let float_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::Float));

        let string_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::StringType));

        let owned_string_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::StringType));

        let cstr_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::CStr));

        let error_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Error);

        let never_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Never);

        // Pre-allocate the remaining sized-numeric primitives so
        // `primitive_id` is a direct array lookup on every variant.
        // Order doesn't matter — the lookup table is keyed by variant.
        let mut primitive_ids = [TypeId(0); PRIMITIVE_TYPE_COUNT];
        primitive_ids[PrimitiveType::Bool as usize] = bool_id;
        primitive_ids[PrimitiveType::Int as usize] = int_id;
        primitive_ids[PrimitiveType::Float as usize] = float_id;
        primitive_ids[PrimitiveType::CStr as usize] = cstr_id;
        primitive_ids[PrimitiveType::StringType as usize] = owned_string_id;
        primitive_ids[PrimitiveType::Void as usize] = void_id;
        for prim in [
            PrimitiveType::Int8,
            PrimitiveType::Int16,
            PrimitiveType::Int32,
            PrimitiveType::Int64,
            PrimitiveType::Uint,
            PrimitiveType::Uint8,
            PrimitiveType::Uint16,
            PrimitiveType::Uint32,
            PrimitiveType::Uint64,
            PrimitiveType::Float32,
            PrimitiveType::Float64,
        ] {
            let id = TypeId(types.len() as u32);
            types.push(ResolvedType::Primitive(prim));
            primitive_ids[prim as usize] = id;
        }

        Self {
            types,
            defined_cache: FxHashMap::default(),
            generic_cache: FxHashMap::default(),
            void_id,
            bool_id,
            int_id,
            float_id,
            string_id,
            owned_string_id,
            cstr_id,
            error_id,
            never_id,
            primitive_ids,
        }
    }

    /// Insert a type and return its ID.
    pub fn insert(&mut self, ty: ResolvedType) -> TypeId {
        let id = TypeId(self.types.len() as u32);
        self.types.push(ty);
        id
    }

    /// Get or create a TypeId for `ResolvedType::Defined(def_id)`.
    /// Ensures the same DefId always maps to the same TypeId.
    pub fn defined_id(&mut self, def_id: DefId) -> TypeId {
        if let Some(&tid) = self.defined_cache.get(&def_id) {
            return tid;
        }
        let tid = self.insert(ResolvedType::Defined(def_id));
        self.defined_cache.insert(def_id, tid);
        tid
    }

    pub fn get(&self, id: TypeId) -> &ResolvedType {
        &self.types[id.0 as usize]
    }

    /// Look up an existing TypeId for `Defined(def_id)` without creating one.
    pub fn try_defined_id(&self, def_id: DefId) -> Option<TypeId> {
        self.defined_cache.get(&def_id).copied()
    }

    /// Get or create a TypeId for `ResolvedType::Generic(def_id, args)`.
    /// Ensures identical generic instantiations share a single TypeId.
    pub fn intern_generic(&mut self, def_id: DefId, args: Vec<TypeId>) -> TypeId {
        let key = (def_id, args.clone());
        if let Some(&tid) = self.generic_cache.get(&key) {
            return tid;
        }
        let tid = self.insert(ResolvedType::Generic(def_id, args));
        self.generic_cache.insert(key, tid);
        tid
    }

    /// Get the TypeId for a primitive type. O(1) array lookup; every
    /// variant is pre-allocated in `new()`.
    pub fn primitive_id(&mut self, prim: PrimitiveType) -> TypeId {
        self.primitive_ids[prim as usize]
    }

    /// Create a fresh type variable for inference.
    pub fn fresh_var(&mut self, var_id: u32) -> TypeId {
        self.insert(ResolvedType::Var(var_id))
    }

    /// Collect all generic type instantiations for monomorphization.
    /// Returns (base_def_id, type_arg_ids) for each `ResolvedType::Generic` entry.
    pub fn collect_generic_instantiations(&self) -> Vec<(DefId, Vec<TypeId>)> {
        self.types
            .iter()
            .filter_map(|ty| {
                if let ResolvedType::Generic(def_id, args) = ty {
                    Some((*def_id, args.clone()))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Format a type as a human-readable string for error messages.
    pub fn display(&self, id: TypeId) -> String {
        match self.get(id) {
            ResolvedType::Primitive(p) => match p {
                PrimitiveType::StringType => "String".to_string(),
                PrimitiveType::CStr => "cstr".to_string(),
                _ => format!("{p:?}").to_lowercase(),
            },
            ResolvedType::Defined(_) => "<defined>".into(),
            ResolvedType::Generic(_, args) => {
                let arg_strs: Vec<_> = args.iter().map(|a| self.display(*a)).collect();
                format!("<generic>[{}]", arg_strs.join(", "))
            }
            ResolvedType::Tuple(elems) => {
                let parts: Vec<_> = elems.iter().map(|e| self.display(*e)).collect();
                format!("({})", parts.join(", "))
            }
            ResolvedType::Array(elem, size) => {
                format!("{}[{size}]", self.display(*elem))
            }
            ResolvedType::Slice(elem) => format!("{}[]", self.display(*elem)),
            ResolvedType::Function {
                params,
                return_type,
                param_ownerships,
            } => {
                let params: Vec<_> = params.iter().enumerate().map(|(i, p)| {
                    let prefix = match param_ownerships.get(i) {
                        Some(crate::parser::ast::Ownership::MutableBorrow) => "&",
                        Some(crate::parser::ast::Ownership::Move) => "!",
                        _ => "",
                    };
                    format!("{prefix}{}", self.display(*p))
                }).collect();
                format!("{}({})", self.display(*return_type), params.join(", "))
            }
            ResolvedType::TraitObject(_) => "<trait object>".into(),
            ResolvedType::CallableTrait(inner) => format!("Callable[{}]", self.display(*inner)),
            ResolvedType::MutCallableTrait(inner) => format!("MutCallable[{}]", self.display(*inner)),
            ResolvedType::ConsumeCallableTrait(inner) => format!("ConsumeCallable[{}]", self.display(*inner)),
            ResolvedType::BoxedCallable { kind, inner } => format!("Box[{}[{}]]", kind.name(), self.display(*inner)),
            ResolvedType::Ref(inner) => format!("{} &", self.display(*inner)),
            ResolvedType::Owned(inner) => format!("{} !", self.display(*inner)),
            ResolvedType::Var(n) => format!("?T{n}"),
            ResolvedType::Error => "<error>".into(),
            ResolvedType::Void => "void".into(),
            ResolvedType::Never => "never".into(),
        }
    }

    /// Resolve an AST type annotation to a TypeId.
    /// Handles primitives and named types. Returns None for unresolvable types.
    pub fn resolve_type(&self, ty: &crate::parser::ast::Type) -> Option<TypeId> {
        use crate::parser::ast::{Type, PrimitiveType as P};
        match ty {
            Type::Primitive(p) => {
                let tid = match p {
                    P::Int | P::Int64 => self.int_id,
                    P::Float | P::Float64 => self.float_id,
                    P::Bool => self.bool_id,
                    P::StringType => self.owned_string_id,
                    P::CStr => self.cstr_id,
                    P::Void => self.void_id,
                    _ => return None,
                };
                Some(tid)
            }
            _ => None, // Named/Generic types not resolvable without scope table
        }
    }
}

/// Returns true if a type is a reference type that needs lifetime tracking.
/// Reference types are views into data owned by something else — if the
/// owner is dropped/moved, outstanding references become dangling.
///
/// Includes: `str`, `Slice[T]`, and structs whose fields include reference types.
pub fn is_reference_type(type_id: TypeId, types: &TypeTable, ref_type_structs: &FxHashSet<DefId>) -> bool {
    match types.get(type_id) {
        ResolvedType::Ref(_) => true,
        ResolvedType::Slice(_) => true,
        ResolvedType::Defined(def_id) => ref_type_structs.contains(def_id),
        ResolvedType::Generic(def_id, _) => ref_type_structs.contains(def_id),
        _ => false,
    }
}

/// Check whether a TypeId is a callable type (function pointer or callable trait).
pub fn is_callable_type(type_id: TypeId, types: &TypeTable) -> bool {
    matches!(
        types.get(type_id),
        ResolvedType::Function { .. }
            | ResolvedType::CallableTrait(_)
            | ResolvedType::MutCallableTrait(_)
            | ResolvedType::ConsumeCallableTrait(_)
            | ResolvedType::BoxedCallable { .. }
    )
}

/// Convert an AST Type to a resolved TypeId.
/// If `inner_tid` resolves to a bare trait (same-file `DefKind::Trait`, or a
/// cross-module `DefKind::Import` placeholder whose name matches a Trait def in
/// any scope), return the trait name for diagnostics. Otherwise `None` — the
/// container's type-arg is a concrete type and NonDerefContainer-of-trait's
/// reject does not fire. Used by the Track P reject in `ast_type_to_resolved`.
pub(super) fn trait_name_of_inner(
    inner_tid: TypeId,
    scopes: &ScopeTable,
    types: &TypeTable,
) -> Option<String> {
    let inner_def_id = match types.get(inner_tid) {
        ResolvedType::Defined(d) => *d,
        _ => return None,
    };
    let inner_def = scopes.get_def(inner_def_id);
    match inner_def.kind {
        DefKind::Trait => Some(inner_def.name.clone()),
        DefKind::Import => {
            // Cross-module placeholder: consult the global name index for a
            // Trait def sharing this name (traits from nested Item::Module wrappers
            // land with their true DefKind::Trait — see traits.rs:501). If ANY
            // matching def is a Trait, the imported name is a trait.
            let name = &inner_def.name;
            for did in scopes.defs_named(name) {
                if scopes.get_def(did).kind == DefKind::Trait {
                    return Some(name.clone());
                }
            }
            None
        }
        _ => None,
    }
}

pub fn ast_type_to_resolved(
    ast_ty: &ast::Type,
    span: Span,
    scopes: &ScopeTable,
    types: &mut TypeTable,
) -> Result<TypeId, SemanticError> {
    match ast_ty {
        ast::Type::Primitive(prim) => Ok(types.primitive_id(*prim)),

        ast::Type::Named { name, generic_args } => {
            // Callable[sig] / MutCallable[sig] / ConsumeCallable[sig] — compiler-magic callable types
            if generic_args.len() == 1 {
                let variant = match name.node.as_str() {
                    "Callable" => Some(ResolvedType::CallableTrait as fn(TypeId) -> ResolvedType),
                    "MutCallable" => Some(ResolvedType::MutCallableTrait as fn(TypeId) -> ResolvedType),
                    "ConsumeCallable" => Some(ResolvedType::ConsumeCallableTrait as fn(TypeId) -> ResolvedType),
                    _ => None,
                };
                if let Some(constructor) = variant {
                    let inner = ast_type_to_resolved(&generic_args[0].node, generic_args[0].span, scopes, types)?;
                    if matches!(types.get(inner), ResolvedType::Function { .. }) {
                        return Ok(types.insert(constructor(inner)));
                    }
                    return Err(SemanticError {
                        kind: SemanticErrorKind::InvalidFnTraitArg,
                        span: generic_args[0].span,
                    });
                }
            }

            // Look up the name in the scope table
            match scopes.lookup(&name.node) {
                Some(def_id) => {
                    let def = scopes.get_def(def_id);
                    match def.kind {
                        DefKind::Struct
                        | DefKind::Enum
                        | DefKind::Trait
                        | DefKind::TypeAlias
                        | DefKind::Newtype
                        | DefKind::GenericParam
                        | DefKind::Import => {
                            if generic_args.is_empty() {
                                Ok(types.defined_id(def_id))
                            } else {
                                let mut resolved_args = Vec::new();
                                for arg in generic_args {
                                    resolved_args.push(ast_type_to_resolved(
                                        &arg.node, arg.span, scopes, types,
                                    )?);
                                }
                                // Track P (owner Q1 2026-07-28): NonDerefContainer[BareTrait]
                                // — Mutex/RWLock/Weak/Shared of a bare trait must be
                                // written as `Container[Box[Trait]]` explicitly. Costs
                                // stay visible (Box[T] is an ownership contract that
                                // changes storage layout — hiding it violates D31's
                                // spelling philosophy and CoW's no-user-visible-Ref[T]
                                // principle); a typo `Mutex[Trait]` (meaning
                                // `Mutex[Box[Trait]]`) is told clearly instead of
                                // silently magicked. The predicate reads typed
                                // metadata (`deref_wrapper_kind == NonDerefContainer`),
                                // not the container name — layering rule 2.
                                if def.deref_wrapper_kind
                                    == Some(DerefWrapperKind::NonDerefContainer)
                                    && resolved_args.len() == 1
                                {
                                    if let Some(trait_name) = trait_name_of_inner(
                                        resolved_args[0], scopes, types,
                                    ) {
                                        return Err(SemanticError {
                                            kind: SemanticErrorKind::NonDerefContainerBareTrait {
                                                container: name.node.clone(),
                                                trait_: trait_name,
                                            },
                                            span: name.span,
                                        });
                                    }
                                }
                                // Box[Trait] → TraitObject: automatic dispatch
                                if name.node == "Box" && resolved_args.len() == 1 {
                                    if let ResolvedType::Defined(inner_def_id) =
                                        *types.get(resolved_args[0])
                                    {
                                        if scopes.get_def(inner_def_id).kind == DefKind::Trait {
                                            return Ok(types.insert(
                                                ResolvedType::TraitObject(inner_def_id),
                                            ));
                                        }
                                    }
                                    // Box[Callable[sig]] / Box[MutCallable[sig]] / Box[ConsumeCallable[sig]]
                                    match types.get(resolved_args[0]).clone() {
                                        ResolvedType::CallableTrait(func_id) => {
                                            return Ok(types.insert(ResolvedType::BoxedCallable {
                                                kind: ClosureKind::Callable, inner: func_id,
                                            }));
                                        }
                                        ResolvedType::MutCallableTrait(func_id) => {
                                            return Ok(types.insert(ResolvedType::BoxedCallable {
                                                kind: ClosureKind::MutCallable, inner: func_id,
                                            }));
                                        }
                                        ResolvedType::ConsumeCallableTrait(func_id) => {
                                            return Ok(types.insert(ResolvedType::BoxedCallable {
                                                kind: ClosureKind::ConsumeCallable, inner: func_id,
                                            }));
                                        }
                                        _ => {}
                                    }
                                }
                                Ok(types
                                    .intern_generic(def_id, resolved_args))
                            }
                        }
                        _ => Err(SemanticError {
                            kind: SemanticErrorKind::NotAType {
                                name: name.node.clone(),
                            },
                            span: name.span,
                        }),
                    }
                }
                None => {
                    // Unknown type — could be from an unresolved import.
                    // Return error type to avoid cascading.
                    Ok(types.error_id)
                }
            }
        }

        ast::Type::Array { element, size } => {
            let elem_id = ast_type_to_resolved(&element.node, element.span, scopes, types)?;
            // Try to extract the array size from the expression
            let array_size = match &size.node {
                ast::Expr::IntLiteral(n) => *n as usize,
                _ => 0, // Can't evaluate at this stage
            };
            Ok(types.insert(ResolvedType::Array(elem_id, array_size)))
        }

        ast::Type::Slice { element } => {
            let elem_id = ast_type_to_resolved(&element.node, element.span, scopes, types)?;
            Ok(types.insert(ResolvedType::Slice(elem_id)))
        }

        ast::Type::Tuple(elements) => {
            let mut elem_ids = Vec::new();
            for elem in elements {
                elem_ids.push(ast_type_to_resolved(&elem.node, elem.span, scopes, types)?);
            }
            Ok(types.insert(ResolvedType::Tuple(elem_ids)))
        }

        ast::Type::Function {
            return_type,
            params,
            param_ownerships,
        } => {
            let ret_id =
                ast_type_to_resolved(&return_type.node, return_type.span, scopes, types)?;
            let mut param_ids = Vec::new();
            for param in params {
                param_ids.push(ast_type_to_resolved(&param.node, param.span, scopes, types)?);
            }
            Ok(types.insert(ResolvedType::Function {
                params: param_ids,
                param_ownerships: param_ownerships.clone(),
                return_type: ret_id,
            }))
        }

        ast::Type::SelfType => {
            // Self type — resolved based on enclosing impl block's self_type
            // For now, return error (resolved during type checking)
            Ok(types.error_id)
        }

        ast::Type::Ref(inner) => {
            let inner_id = ast_type_to_resolved(&inner.node, inner.span, scopes, types)?;
            Ok(types.insert(ResolvedType::Ref(inner_id)))
        }

        ast::Type::Owned(inner) => {
            let inner_id = ast_type_to_resolved(&inner.node, inner.span, scopes, types)?;
            Ok(types.insert(ResolvedType::Owned(inner_id)))
        }

        ast::Type::Pointer(inner) => {
            // T* in extern "C" — resolve as the inner type. Ptr ABI handled by AbiKind.
            ast_type_to_resolved(&inner.node, inner.span, scopes, types)
        }

        ast::Type::Inferred => {
            // auto — will be inferred during type checking
            Err(SemanticError {
                kind: SemanticErrorKind::CannotInferType,
                span,
            })
        }
    }
}

/// Map a Rust-style numeric shorthand (`u8`, `i32`, `f64`, …) to the Gorget
/// keyword it most likely meant (`uint8`, `int32`, `float64`). Returns `None`
/// for names that don't match the `[iuf]<bits>` shape. Used only to enrich the
/// "did you mean?" hint on an undefined type — it does NOT make these spellings
/// valid (that's an owner's language-design call).
fn numeric_shorthand_suggestion(name: &str) -> Option<String> {
    let (prefix, bits) = name.split_at(1);
    let canonical = match (prefix, bits) {
        ("i", "8") => "int8",
        ("i", "16") => "int16",
        ("i", "32") => "int32",
        ("i", "64") => "int64",
        ("u", "8") => "uint8",
        ("u", "16") => "uint16",
        ("u", "32") => "uint32",
        ("u", "64") => "uint64",
        ("f", "32") => "float32",
        ("f", "64") => "float64",
        _ => return None,
    };
    Some(canonical.to_string())
}

/// If `ast_ty` is a top-level `Type::Named` whose name is genuinely undefined
/// in scope, return that name node plus a "did you mean?" suggestion. Returns
/// `None` for any resolvable name, for the compiler-magic callable builtins
/// (`Callable`/`MutCallable`/`ConsumeCallable[sig]`), and for every non-Named
/// type shape.
///
/// This mirrors the exact lookup `ast_type_to_resolved` performs for
/// `Type::Named` — the only AST shape that can silently degrade to
/// `error_id` via the unknown-name (`None`) branch — so a caller can surface
/// `UndefinedName` instead of swallowing the unknown type. It is deliberately
/// scoped to the top-level name (not generic args / nested types): the
/// typecheck-pass VarDecl site is the only sound place to hard-error today
/// (see `docs/devbook/09-type-checking.md`, "Unknown type names").
///
/// `fn_scope` is the enclosing function's body scope (the typechecker's
/// `current_fn_scope`). It must be consulted IN ADDITION to plain
/// `scopes.lookup` because the typecheck pass reaches in-scope generic params
/// via two different scope trees depending on the enclosing construct (see the
/// two-root comment in the body). A name is unknown only when BOTH roots miss.
pub fn unknown_named_type<'a>(
    ast_ty: &'a ast::Type,
    scopes: &ScopeTable,
    fn_scope: Option<super::ids::ScopeId>,
) -> Option<(&'a crate::span::Spanned<String>, Option<String>)> {
    let ast::Type::Named { name, generic_args } = ast_ty else {
        return None;
    };

    // Compiler-magic callable builtins short-circuit before the scope lookup
    // in `ast_type_to_resolved`; they are never "unknown names".
    if generic_args.len() == 1
        && matches!(name.node.as_str(), "Callable" | "MutCallable" | "ConsumeCallable")
    {
        return None;
    }

    // Resolvable name → not unknown. Only a `None` lookup hits the
    // `Ok(error_id)` degrade branch we want to reject. We check the in-scope
    // lookup from TWO roots (the typecheck pass reaches in-scope generic params
    // via different trees depending on the enclosing construct):
    //   1. `scopes.current` (what plain `scopes.lookup` walks) — a free
    //      FUNCTION's own generics are NOT here (the pass never navigates into
    //      the fn body), but an EQUIP block's explicit `[T]` generic params ARE.
    //   2. `fn_scope` (the resolve-time function body scope) + its ancestor
    //      chain — this is where a free function's own generic params live.
    if scopes.lookup(&name.node).is_some() {
        return None;
    }
    if let Some(scope) = fn_scope {
        if scopes.lookup_from_scope(scope, &name.node).is_some() {
            return None;
        }
    }

    // Gate A: a name defined somewhere in the program but just not in the
    // current lexical scope — a yet-to-be-merged cross-module type, or a type
    // referenced before its (later) import is fully wired — is a REAL type that
    // degrades to `error_id` benignly today. The VarDecl site must NOT hard-error
    // on it (that would be a separate "type not in scope, forgot to import?"
    // diagnostic, out of scope here). `name_index`-backed, so it sees defs that
    // the lexical `lookup` misses.
    if scopes.name_defined_anywhere(&name.node) {
        return None;
    }

    // Gate B: runtime-provided type names that have NO semantic declaration at
    // all (no struct/import def, not in `name_index`) but ARE valid annotations
    // the runtime materializes — `GorgetClosure`, the un-imported `std.sync`
    // guards, etc. These legitimately resolve to `error_id` at the semantic
    // layer today; flagging them as undefined would regress real code. This is
    // the documented runtime-symbol-boundary exception to "no name matching":
    // the name IS the contract with the runtime, and the semantic layer has no
    // typed decl to consult (see `docs/devbook/24-layering-discipline.md`).
    if is_runtime_provided_type_name(&name.node) {
        return None;
    }

    let suggestion = numeric_shorthand_suggestion(&name.node)
        .or_else(|| scopes.suggest_name(&name.node));
    Some((name, suggestion))
}

/// Runtime-provided type names that the semantic layer does NOT register as
/// scope defs but the lowering/runtime materializes from the name itself. A
/// VarDecl annotated with one of these resolves to `error_id` at the semantic
/// layer today (a benign degrade the lowering fixes up by name), so the
/// unknown-type check must treat them as known, not as typos.
///
/// This is the runtime-symbol-boundary carve-out the layering rules permit:
/// these names have no typed semantic decl to consult, and the name itself is
/// the contract with the runtime. Sourced from the LIR resource aliases
/// (`src/lir/types.rs`) and the `std.sync` guard types (`lib/std/sync.gg`,
/// usable without an explicit import via the `shared`/RWLock magic).
fn is_runtime_provided_type_name(name: &str) -> bool {
    matches!(
        name,
        "GorgetClosure"
            | "GorgetArray"
            | "GorgetString"
            | "GorgetMap"
            | "GorgetSet"
            | "GorgetRange"
            | "GorgetFile"
            | "GorgetTlsSocket"
            | "ReadGuard"
            | "WriteGuard"
    )
}
