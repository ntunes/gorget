use crate::parser::ast::{self, PrimitiveType};
use crate::span::Span;

use super::errors::{SemanticError, SemanticErrorKind};
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

    /// Function type: int(int, int)
    Function {
        params: Vec<TypeId>,
        return_type: TypeId,
    },

    /// Trait object: Box[Trait] → automatic vtable dispatch
    TraitObject(DefId),

    /// Type variable for inference: ?T0, ?T1, ...
    Var(u32),

    /// Error sentinel — used to avoid cascading errors.
    Error,

    /// Void (no value).
    Void,

    /// Never type (for diverging expressions like return/throw).
    Never,
}

/// Stores all resolved types, indexed by TypeId.
pub struct TypeTable {
    types: Vec<ResolvedType>,
    // Pre-allocated IDs for common types
    pub void_id: TypeId,
    pub bool_id: TypeId,
    pub int_id: TypeId,
    pub float_id: TypeId,
    pub char_id: TypeId,
    pub string_id: TypeId,
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

        let char_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::Char));

        let string_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Primitive(PrimitiveType::StringType));

        let error_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Error);

        let never_id = TypeId(types.len() as u32);
        types.push(ResolvedType::Never);

        Self {
            types,
            void_id,
            bool_id,
            int_id,
            float_id,
            char_id,
            string_id,
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

    pub fn get(&self, id: TypeId) -> &ResolvedType {
        &self.types[id.0 as usize]
    }

    /// Get the TypeId for a primitive type.
    pub fn primitive_id(&mut self, prim: PrimitiveType) -> TypeId {
        match prim {
            PrimitiveType::Bool => self.bool_id,
            PrimitiveType::Int => self.int_id,
            PrimitiveType::Float => self.float_id,
            PrimitiveType::Char => self.char_id,
            PrimitiveType::StringType => self.string_id,
            PrimitiveType::Void => self.void_id,
            PrimitiveType::Str => self.string_id, // treat str as String for now
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

    /// Format a type as a human-readable string for error messages.
    pub fn display(&self, id: TypeId) -> String {
        match self.get(id) {
            ResolvedType::Primitive(p) => format!("{p:?}").to_lowercase(),
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
            } => {
                let params: Vec<_> = params.iter().map(|p| self.display(*p)).collect();
                format!("{}({})", self.display(*return_type), params.join(", "))
            }
            ResolvedType::TraitObject(_) => "<trait object>".into(),
            ResolvedType::Var(n) => format!("?T{n}"),
            ResolvedType::Error => "<error>".into(),
            ResolvedType::Void => "void".into(),
            ResolvedType::Never => "never".into(),
        }
    }
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
                                if def.kind == DefKind::GenericParam {
                                    // Type parameter stays as Defined
                                    Ok(types.insert(ResolvedType::Defined(def_id)))
                                } else {
                                    Ok(types.insert(ResolvedType::Defined(def_id)))
                                }
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
                                }
                                Ok(types
                                    .insert(ResolvedType::Generic(def_id, resolved_args)))
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
        } => {
            let ret_id =
                ast_type_to_resolved(&return_type.node, return_type.span, scopes, types)?;
            let mut param_ids = Vec::new();
            for param in params {
                param_ids.push(ast_type_to_resolved(&param.node, param.span, scopes, types)?);
            }
            Ok(types.insert(ResolvedType::Function {
                params: param_ids,
                return_type: ret_id,
            }))
        }

        ast::Type::SelfType => {
            // Self type — resolved based on enclosing impl block's self_type
            // For now, return error (resolved during type checking)
            Ok(types.error_id)
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
