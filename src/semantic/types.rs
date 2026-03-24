use crate::parser::ast::{self, PrimitiveType};
use crate::span::Span;

use super::errors::{SemanticError, SemanticErrorKind};
use rustc_hash::{FxHashMap, FxHashSet};

use super::ids::{DefId, TypeId};
use super::scope::{DefKind, ScopeTable};

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
}

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
        types.push(ResolvedType::Primitive(PrimitiveType::StringView));

        let owned_string_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::StringType));

        let cstr_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::CStr));

        let error_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Error);

        let never_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Never);

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

    /// Get the TypeId for a primitive type.
    pub fn primitive_id(&mut self, prim: PrimitiveType) -> TypeId {
        match prim {
            PrimitiveType::Bool => self.bool_id,
            PrimitiveType::Int => self.int_id,
            PrimitiveType::Float => self.float_id,
            PrimitiveType::StringView => self.string_id,
            PrimitiveType::CStr => self.cstr_id,
            PrimitiveType::StringType => self.owned_string_id,
            PrimitiveType::Void => self.void_id,
            other => {
                // Other numeric variants - insert or find
                let ty = ResolvedType::Primitive(other);
                // Linear scan is fine for the small number of primitive types
                for (i, t) in self.types.iter().enumerate() {
                    if *t == ty {
                        return TypeId(i as u32);
                    }
                }
                self.insert(ty)
            }
        }
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
                PrimitiveType::StringView => "String".to_string(),
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
}

/// Returns true if a type is a reference type that needs lifetime tracking.
/// Reference types are views into data owned by something else — if the
/// owner is dropped/moved, outstanding references become dangling.
///
/// Includes: `str`, `Slice[T]`, and structs whose fields include reference types.
pub fn is_reference_type(type_id: TypeId, types: &TypeTable, ref_type_structs: &FxHashSet<DefId>) -> bool {
    match types.get(type_id) {
        ResolvedType::Primitive(PrimitiveType::StringView) => true,
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

        ast::Type::Inferred => {
            // auto — will be inferred during type checking
            Err(SemanticError {
                kind: SemanticErrorKind::CannotInferType,
                span,
            })
        }
    }
}
