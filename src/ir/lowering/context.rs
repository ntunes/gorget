use rustc_hash::FxHashMap;

use crate::ir::types::*;
use crate::parser::ast::{Expr, PrimitiveType, Type};
use crate::semantic::AnalysisResult;
use crate::span::Spanned;

use super::closures::ClosureLowering;
use super::drops::DropElaborator;
use super::types::TypeMapper;

use crate::ir::types::BlockId;

/// Information about a loop for break/continue targeting.
pub struct LoopInfo {
    pub header_bb: BlockId,  // target for continue
    pub exit_bb: BlockId,    // target for break
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
    /// Closure info: struct_name → (call_fn_name, struct_type_id).
    pub closure_info: FxHashMap<String, (String, TypeId)>,
    /// Stack of active loops for break/continue targeting.
    loop_stack: Vec<LoopInfo>,
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
        }
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
            other => self.type_mapper.map_ast_type(other),
        }
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
                        return *ret_ty;
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

    /// Register closure info for call dispatch.
    pub fn register_closure_info(&mut self, struct_name: String, call_fn_name: String, struct_type_id: TypeId) {
        self.closure_info.insert(struct_name, (call_fn_name, struct_type_id));
    }

    /// Look up closure info by struct name.
    pub fn lookup_closure_info(&self, struct_name: &str) -> Option<(&str, TypeId)> {
        self.closure_info.get(struct_name).map(|(name, tid)| (name.as_str(), *tid))
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
}
