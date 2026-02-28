//! Generic monomorphization for GIR lowering.
//!
//! Walks the AST to discover all concrete generic instantiations (e.g., `Pair[int, str]`),
//! then creates monomorphized TypeDefs and Functions with substituted types.

use rustc_hash::FxHashMap;

use crate::ir::types::*;
use crate::parser::ast::{self, Expr, GenericParam, Item, Stmt, Type};
use crate::span::Spanned;

use super::types::{mangle_generic_name, TypeMapper};

/// The kind of generic template being instantiated.
#[derive(Debug, Clone, Copy)]
pub enum TemplateKind {
    Struct,
    Enum,
    Function,
}

/// Collects generic templates and discovered instantiations, then monomorphizes them.
pub struct GenericCollector {
    /// Generic struct templates: base_name → AST StructDef.
    struct_templates: FxHashMap<String, ast::StructDef>,
    /// Generic enum templates: base_name → AST EnumDef.
    enum_templates: FxHashMap<String, ast::EnumDef>,
    /// Generic function templates: base_name → AST FunctionDef.
    fn_templates: FxHashMap<String, ast::FunctionDef>,
    /// Generic equip templates: base_type_name → Vec<EquipBlock>.
    equip_templates: FxHashMap<String, Vec<ast::EquipBlock>>,
    /// Discovered instantiations: (base_name, concrete_type_args) → mangled_name.
    /// Uses stringified type args as key for deduplication.
    instances: Vec<(String, Vec<Spanned<Type>>, String, TemplateKind)>,
    /// Already-registered mangled names (for dedup).
    registered: FxHashMap<String, ()>,
}

impl GenericCollector {
    pub fn new() -> Self {
        Self {
            struct_templates: FxHashMap::default(),
            enum_templates: FxHashMap::default(),
            fn_templates: FxHashMap::default(),
            equip_templates: FxHashMap::default(),
            instances: Vec::new(),
            registered: FxHashMap::default(),
        }
    }

    /// Phase 1: Collect all generic templates from the AST module.
    pub fn collect_templates(&mut self, ast_module: &ast::Module) {
        for item in &ast_module.items {
            match &item.node {
                Item::Struct(s) if s.generic_params.is_some() => {
                    self.struct_templates.insert(s.name.node.clone(), s.clone());
                }
                Item::Enum(e) if e.generic_params.is_some() => {
                    self.enum_templates.insert(e.name.node.clone(), e.clone());
                }
                Item::Function(f) if f.generic_params.is_some() => {
                    self.fn_templates.insert(f.name.node.clone(), f.clone());
                }
                Item::Equip(equip) => {
                    if let Type::Named { name, generic_args } = &equip.type_.node {
                        if !generic_args.is_empty() || equip.generic_params.is_some() {
                            self.equip_templates
                                .entry(name.node.clone())
                                .or_default()
                                .push(equip.clone());
                        }
                    }
                }
                _ => {}
            }
        }

        // Inject built-in Option[T] and Result[T, E] if not already present
        inject_builtin_enums(&mut self.enum_templates);
    }

    /// Phase 2: Walk the AST to discover all concrete generic usages.
    pub fn discover_usages(&mut self, ast_module: &ast::Module) {
        for item in &ast_module.items {
            match &item.node {
                Item::Function(f) => {
                    self.scan_function(f);
                }
                Item::Struct(s) if s.generic_params.is_none() => {
                    // Non-generic struct fields may reference generic types
                    for field in &s.fields {
                        self.scan_type(&field.node.type_);
                    }
                }
                Item::Enum(e) if e.generic_params.is_none() => {
                    for variant in &e.variants {
                        match &variant.node.fields {
                            ast::VariantFields::Tuple(types) => {
                                for ty in types {
                                    self.scan_type(ty);
                                }
                            }
                            ast::VariantFields::Unit => {}
                        }
                    }
                }
                Item::Equip(equip) if equip.generic_params.is_none() => {
                    for method in &equip.items {
                        self.scan_function(&method.node);
                    }
                }
                _ => {}
            }
        }
    }

    /// Register a concrete generic instantiation.
    fn register_instance(
        &mut self,
        base_name: &str,
        type_args: &[Spanned<Type>],
        kind: TemplateKind,
    ) -> String {
        let mangled = mangle_generic_name(base_name, type_args);
        if self.registered.contains_key(&mangled) {
            return mangled;
        }
        self.registered.insert(mangled.clone(), ());
        self.instances
            .push((base_name.to_string(), type_args.to_vec(), mangled.clone(), kind));
        mangled
    }

    /// Scan a function definition for generic usages.
    fn scan_function(&mut self, func: &ast::FunctionDef) {
        // Return type
        self.scan_type(&func.return_type);
        // Params
        for p in &func.params {
            self.scan_type(&p.node.type_);
        }
        // Body
        match &func.body {
            ast::FunctionBody::Block(block) => self.scan_block(block),
            ast::FunctionBody::Expression(expr) => self.scan_expr(expr),
            _ => {}
        }
    }

    /// Scan an AST type for generic instantiations.
    fn scan_type(&mut self, ty: &Spanned<Type>) {
        match &ty.node {
            Type::Named { name, generic_args } if !generic_args.is_empty() => {
                // Recursively scan the type args themselves
                for arg in generic_args {
                    self.scan_type(arg);
                }
                // Register the instantiation
                let base = &name.node;
                if self.struct_templates.contains_key(base) {
                    self.register_instance(base, generic_args, TemplateKind::Struct);
                } else if self.enum_templates.contains_key(base) {
                    self.register_instance(base, generic_args, TemplateKind::Enum);
                } else if matches!(base.as_str(), "Vector" | "List" | "Array" | "Dict" | "HashMap" | "Set" | "HashSet") {
                    // Runtime collection types — register as Struct so the type name is
                    // available for method call mangling (no struct template to monomorphize)
                    self.register_instance(base, generic_args, TemplateKind::Struct);
                } else if matches!(base.as_str(), "Option" | "Result") {
                    // Option/Result generic types — register as Enum so the type definition
                    // is emitted in C output (auto-registered by map_ast_type_mut)
                    self.register_instance(base, generic_args, TemplateKind::Enum);
                }
            }
            Type::Tuple(elems) => {
                for elem in elems {
                    self.scan_type(elem);
                }
            }
            Type::Function { return_type, params, .. } => {
                self.scan_type(return_type);
                for p in params {
                    self.scan_type(p);
                }
            }
            Type::Array { element, .. } | Type::Slice { element } => {
                self.scan_type(element);
            }
            _ => {}
        }
    }

    /// Scan a block for generic usages.
    fn scan_block(&mut self, block: &ast::Block) {
        for stmt in &block.stmts {
            self.scan_stmt(stmt);
        }
    }

    /// Scan a statement for generic usages.
    fn scan_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl { type_, value, .. } => {
                self.scan_type(type_);
                self.scan_expr(value);
            }
            Stmt::Assign { target, value } => {
                self.scan_expr(target);
                self.scan_expr(value);
            }
            Stmt::CompoundAssign { target, value, .. } => {
                self.scan_expr(target);
                self.scan_expr(value);
            }
            Stmt::Return(Some(expr)) | Stmt::Expr(expr) | Stmt::Throw(expr) => {
                self.scan_expr(expr);
            }
            Stmt::If { condition, then_body, elif_branches, else_body } => {
                self.scan_expr(condition);
                self.scan_block(then_body);
                for (cond, body) in elif_branches {
                    self.scan_expr(cond);
                    self.scan_block(body);
                }
                if let Some(else_body) = else_body {
                    self.scan_block(else_body);
                }
            }
            Stmt::While { condition, body, .. } => {
                self.scan_expr(condition);
                self.scan_block(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.scan_expr(iterable);
                self.scan_block(body);
            }
            Stmt::Match { scrutinee, arms, else_arm } => {
                self.scan_expr(scrutinee);
                for arm in arms {
                    self.scan_expr(&arm.body);
                    if let Some(guard) = &arm.guard {
                        self.scan_expr(guard);
                    }
                }
                if let Some(else_body) = else_arm {
                    self.scan_block(else_body);
                }
            }
            _ => {}
        }
    }

    /// Scan an expression for generic usages.
    fn scan_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Call { callee, generic_args, args } => {
                self.scan_expr(callee);
                // Generic function call: identity[int](42)
                if let Some(type_args) = generic_args {
                    for arg in type_args {
                        self.scan_type(arg);
                    }
                    if let Expr::Identifier(name) = &callee.node {
                        if self.fn_templates.contains_key(name.as_str()) {
                            self.register_instance(name, type_args, TemplateKind::Function);
                        }
                    }
                }
                for arg in args {
                    self.scan_expr(&arg.node.value);
                }
            }
            Expr::MethodCall { receiver, generic_args, args, .. } => {
                self.scan_expr(receiver);
                if let Some(type_args) = generic_args {
                    for arg in type_args {
                        self.scan_type(arg);
                    }
                }
                for arg in args {
                    self.scan_expr(&arg.node.value);
                }
            }
            Expr::StructLiteral { generic_args, args, name, .. } => {
                if let Some(type_args) = generic_args {
                    for arg in type_args {
                        self.scan_type(arg);
                    }
                    if self.struct_templates.contains_key(name.node.as_str()) {
                        self.register_instance(&name.node, type_args, TemplateKind::Struct);
                    } else if self.enum_templates.contains_key(name.node.as_str()) {
                        self.register_instance(&name.node, type_args, TemplateKind::Enum);
                    }
                }
                for arg in args {
                    self.scan_expr(arg);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.scan_expr(left);
                self.scan_expr(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.scan_expr(operand);
            }
            Expr::FieldAccess { object, .. } => {
                self.scan_expr(object);
            }
            Expr::Index { object, index } => {
                self.scan_expr(object);
                self.scan_expr(index);
            }
            Expr::If { condition, then_branch, else_branch, .. } => {
                self.scan_expr(condition);
                self.scan_expr(then_branch);
                if let Some(eb) = else_branch {
                    self.scan_expr(eb);
                }
            }
            Expr::Move { expr: inner } | Expr::MutableBorrow { expr: inner } => {
                self.scan_expr(inner);
            }
            Expr::Range { start, end, .. } => {
                if let Some(s) = start { self.scan_expr(s); }
                if let Some(e) = end { self.scan_expr(e); }
            }
            Expr::Closure { body, .. } | Expr::ImplicitClosure { body } => {
                self.scan_expr(body);
            }
            Expr::TupleLiteral(elems) | Expr::ArrayLiteral(elems) => {
                for e in elems {
                    self.scan_expr(e);
                }
            }
            _ => {}
        }
    }

    /// Phase 3: Monomorphize all discovered instantiations.
    /// Creates TypeDefs for generic structs/enums and registers them in TypeMapper/TypeRegistry.
    pub fn monomorphize_types(
        &self,
        mapper: &mut TypeMapper,
        registry: &mut TypeRegistry,
    ) {
        for (base_name, type_args, mangled_name, kind) in &self.instances {
            match kind {
                TemplateKind::Struct => {
                    if let Some(template) = self.struct_templates.get(base_name) {
                        monomorphize_struct(template, type_args, mangled_name, mapper, registry);
                    }
                }
                TemplateKind::Enum => {
                    if let Some(template) = self.enum_templates.get(base_name) {
                        monomorphize_enum(template, type_args, mangled_name, mapper, registry);
                    }
                }
                TemplateKind::Function => {
                    // Function signatures are registered separately
                }
            }
        }
    }

    /// Register monomorphized function signatures in the fn_sigs map.
    pub fn register_fn_sigs(
        &self,
        mapper: &TypeMapper,
        fn_sigs: &mut FxHashMap<String, (Vec<TypeId>, TypeId)>,
    ) {
        for (base_name, type_args, mangled_name, kind) in &self.instances {
            if !matches!(kind, TemplateKind::Function) {
                continue;
            }
            if let Some(template) = self.fn_templates.get(base_name) {
                let subs = build_type_substitutions(template.generic_params.as_ref(), type_args);
                let ret_type = substitute_and_map(mapper, &template.return_type.node, &subs);
                let param_types: Vec<TypeId> = template.params.iter()
                    .map(|p| substitute_and_map(mapper, &p.node.type_.node, &subs))
                    .collect();
                fn_sigs.insert(mangled_name.clone(), (param_types, ret_type));
            }
        }
    }

    /// Register monomorphized equip method signatures.
    pub fn register_equip_sigs(
        &self,
        mapper: &TypeMapper,
        registry: &mut TypeRegistry,
        fn_sigs: &mut FxHashMap<String, (Vec<TypeId>, TypeId)>,
    ) {
        self.register_equip_sigs_with_defaults(mapper, registry, fn_sigs, None);
    }

    /// Register monomorphized equip method signatures, including default trait methods.
    pub fn register_equip_sigs_with_defaults(
        &self,
        mapper: &TypeMapper,
        registry: &mut TypeRegistry,
        fn_sigs: &mut FxHashMap<String, (Vec<TypeId>, TypeId)>,
        ast_module: Option<&crate::parser::ast::Module>,
    ) {
        use crate::parser::ast::{Item, TraitItem, FunctionBody};
        for (base_name, type_args, mangled_type_name, kind) in &self.instances {
            if !matches!(kind, TemplateKind::Struct | TemplateKind::Enum) {
                continue;
            }
            if let Some(equip_blocks) = self.equip_templates.get(base_name) {
                for equip in equip_blocks {
                    let subs = build_equip_type_substitutions(equip, type_args);
                    let mut implemented = Vec::new();
                    for method in &equip.items {
                        let method_mangled = format!("{mangled_type_name}__{}", method.node.name.node);
                        let ret_type = substitute_and_map(mapper, &method.node.return_type.node, &subs);
                        let self_type_id = mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
                        let self_ptr_type = registry.insert(GirType::Ptr(self_type_id));
                        let mut param_types = vec![self_ptr_type];
                        for p in &method.node.params {
                            if p.node.name.node == "self" {
                                continue;
                            }
                            param_types.push(substitute_and_map(mapper, &p.node.type_.node, &subs));
                        }
                        fn_sigs.insert(method_mangled, (param_types, ret_type));
                        implemented.push(method.node.name.node.clone());
                    }
                    // Also register signatures for default trait methods
                    if let (Some(ast_mod), Some(trait_ref)) = (ast_module, &equip.trait_) {
                        let trait_name = super::traits::extract_trait_name(&trait_ref.trait_name.node);
                        if !trait_name.is_empty() {
                            for item in &ast_mod.items {
                                if let Item::Trait(trait_def) = &item.node {
                                    if trait_def.name.node == trait_name {
                                        for trait_item in &trait_def.items {
                                            if let TraitItem::Method(dm) = &trait_item.node {
                                                if implemented.contains(&dm.name.node) { continue; }
                                                match &dm.body {
                                                    FunctionBody::Declaration | FunctionBody::Extern(_) => continue,
                                                    _ => {}
                                                }
                                                let m_mangled = format!("{mangled_type_name}__{}", dm.name.node);
                                                let ret_type = substitute_and_map(mapper, &dm.return_type.node, &subs);
                                                let self_type_id = mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
                                                let self_ptr_type = registry.insert(GirType::Ptr(self_type_id));
                                                let mut param_types = vec![self_ptr_type];
                                                for p in &dm.params {
                                                    if p.node.name.node == "self" { continue; }
                                                    param_types.push(substitute_and_map(mapper, &p.node.type_.node, &subs));
                                                }
                                                fn_sigs.insert(m_mangled, (param_types, ret_type));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Get all discovered function instantiations for lowering.
    pub fn function_instances(&self) -> Vec<(&str, &[Spanned<Type>], &str)> {
        self.instances.iter()
            .filter(|(_, _, _, kind)| matches!(kind, TemplateKind::Function))
            .map(|(base, args, mangled, _)| (base.as_str(), args.as_slice(), mangled.as_str()))
            .collect()
    }

    /// Get all struct/enum instantiations that have equip blocks.
    pub fn equip_instances(&self) -> Vec<(&str, &[Spanned<Type>], &str)> {
        self.instances.iter()
            .filter(|(base, _, _, kind)| {
                matches!(kind, TemplateKind::Struct | TemplateKind::Enum)
                    && self.equip_templates.contains_key(base.as_str())
            })
            .map(|(base, args, mangled, _)| (base.as_str(), args.as_slice(), mangled.as_str()))
            .collect()
    }

    /// Look up a generic function template.
    pub fn get_fn_template(&self, name: &str) -> Option<&ast::FunctionDef> {
        self.fn_templates.get(name)
    }

    /// Look up generic equip templates for a base type name.
    pub fn get_equip_templates(&self, base_name: &str) -> Option<&Vec<ast::EquipBlock>> {
        self.equip_templates.get(base_name)
    }

    /// Get raw access to all instances for iteration.
    pub fn instances_raw(&self) -> &[(String, Vec<Spanned<Type>>, String, TemplateKind)] {
        &self.instances
    }

    /// Get all type instances (struct + enum) with their mangled names.
    pub fn type_instances(&self) -> Vec<(&str, &str)> {
        self.instances.iter()
            .filter(|(_, _, _, kind)| matches!(kind, TemplateKind::Struct | TemplateKind::Enum))
            .map(|(_, _, mangled, _)| {
                let base = self.instances.iter()
                    .find(|(_, _, m, _)| m == mangled)
                    .map(|(b, _, _, _)| b.as_str())
                    .unwrap_or("");
                (base, mangled.as_str())
            })
            .collect()
    }
}

/// Public entry point for on-demand monomorphization of a generic type.
/// Used when struct fields reference generic types that haven't been registered yet.
pub fn monomorphize_generic_type(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    template_item: &ast::Item,
    type_args: &[Spanned<Type>],
    mangled_name: &str,
) {
    match template_item {
        ast::Item::Struct(struct_def) => {
            monomorphize_struct(struct_def, type_args, mangled_name, mapper, registry);
        }
        ast::Item::Enum(enum_def) => {
            monomorphize_enum(enum_def, type_args, mangled_name, mapper, registry);
        }
        _ => {}
    }
}

/// Monomorphize a generic struct: create a TypeDef with substituted field types.
fn monomorphize_struct(
    template: &ast::StructDef,
    type_args: &[Spanned<Type>],
    mangled_name: &str,
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
) {
    // Already registered?
    if mapper.named_types.contains_key(mangled_name) {
        return;
    }

    let subs = build_type_substitutions(template.generic_params.as_ref(), type_args);

    let mut fields: Vec<StructField> = Vec::new();
    for f in &template.fields {
        let field_type = substitute_and_map_mut(mapper, registry, &f.node.type_.node, &subs);
        fields.push(StructField {
            name: f.node.name.node.clone(),
            type_id: field_type,
        });
    }

    let type_def = TypeDef {
        name: mangled_name.to_string(),
        kind: TypeDefKind::Struct(StructDef { fields }),
        metadata: TypeMetadata::default(),
    };

    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.named_types.insert(mangled_name.to_string(), type_id);
}

/// Monomorphize a generic enum: create a TypeDef with substituted variant field types.
fn monomorphize_enum(
    template: &ast::EnumDef,
    type_args: &[Spanned<Type>],
    mangled_name: &str,
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
) {
    if mapper.named_types.contains_key(mangled_name) {
        return;
    }

    let subs = build_type_substitutions(template.generic_params.as_ref(), type_args);

    let mut variants: Vec<EnumVariant> = Vec::new();
    for v in &template.variants {
        let fields = match &v.node.fields {
            ast::VariantFields::Unit => vec![],
            ast::VariantFields::Tuple(types) => {
                let mut fs = Vec::new();
                for (i, t) in types.iter().enumerate() {
                    let field_type = substitute_and_map_mut(mapper, registry, &t.node, &subs);
                    fs.push(StructField {
                        name: format!("_{i}"),
                        type_id: field_type,
                    });
                }
                fs
            }
        };
        variants.push(EnumVariant {
            name: v.node.name.node.clone(),
            fields,
        });
    }

    let type_def = TypeDef {
        name: mangled_name.to_string(),
        kind: TypeDefKind::Enum(EnumDef { variants }),
        metadata: TypeMetadata::default(),
    };

    registry.add_type_def(type_def);
    let type_id = registry.insert(GirType::Named(mangled_name.to_string()));
    mapper.named_types.insert(mangled_name.to_string(), type_id);
}

/// Build a substitution map from generic param names to concrete AST types.
fn build_type_substitutions(
    generic_params: Option<&Spanned<ast::GenericParams>>,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    let mut subs = Vec::new();
    if let Some(params) = generic_params {
        for (param, arg) in params.node.params.iter().zip(type_args.iter()) {
            let name = match &param.node {
                GenericParam::Type(s) => s.node.clone(),
                GenericParam::Lifetime(s) => s.node.clone(),
                GenericParam::Const { name, .. } => name.node.clone(),
            };
            subs.push((name, arg.node.clone()));
        }
    }
    subs
}

/// Build substitutions for an equip block (generic params come from the equipped type).
fn build_equip_type_substitutions(
    equip: &ast::EquipBlock,
    type_args: &[Spanned<Type>],
) -> Vec<(String, Type)> {
    // Generic params may come from the equip block itself or the equipped type
    if let Some(ref gp) = equip.generic_params {
        return build_type_substitutions(Some(gp), type_args);
    }
    // Fall back: extract params from the equipped type's generic args
    if let Type::Named { generic_args, .. } = &equip.type_.node {
        let mut subs = Vec::new();
        for (param_type, arg) in generic_args.iter().zip(type_args.iter()) {
            if let Type::Named { name, generic_args: inner } = &param_type.node {
                if inner.is_empty() {
                    // This is a bare type parameter name like "T"
                    subs.push((name.node.clone(), arg.node.clone()));
                }
            }
        }
        return subs;
    }
    Vec::new()
}

/// Substitute type parameters in an AST type and map to GIR TypeId.
fn substitute_and_map(
    mapper: &TypeMapper,
    ty: &Type,
    subs: &[(String, Type)],
) -> TypeId {
    let substituted = substitute_type(ty, subs);
    mapper.map_ast_type(&substituted)
}

/// Like substitute_and_map, but auto-registers new collection/Option/Result types.
/// Used in monomorphize_struct/enum where field types may reference derived collections.
fn substitute_and_map_mut(
    mapper: &mut TypeMapper,
    registry: &mut TypeRegistry,
    ty: &Type,
    subs: &[(String, Type)],
) -> TypeId {
    let substituted = substitute_type(ty, subs);
    mapper.map_ast_type_mut(&substituted, registry)
}

/// Public entry point for type substitution (used by functions.rs).
pub fn substitute_type_pub(ty: &Type, subs: &[(String, Type)]) -> Type {
    substitute_type(ty, subs)
}

/// Recursively substitute type parameters in an AST type.
fn substitute_type(ty: &Type, subs: &[(String, Type)]) -> Type {
    match ty {
        Type::Named { name, generic_args } if generic_args.is_empty() => {
            // Check if this is a type parameter that should be substituted
            for (param_name, concrete) in subs {
                if name.node == *param_name {
                    return concrete.clone();
                }
            }
            ty.clone()
        }
        Type::Named { name, generic_args } => {
            // Recursively substitute within generic args
            let new_args: Vec<Spanned<Type>> = generic_args.iter()
                .map(|arg| Spanned::dummy(substitute_type(&arg.node, subs)))
                .collect();
            Type::Named {
                name: name.clone(),
                generic_args: new_args,
            }
        }
        Type::Tuple(elems) => {
            Type::Tuple(elems.iter()
                .map(|e| Spanned::dummy(substitute_type(&e.node, subs)))
                .collect())
        }
        Type::Function { return_type, params, param_ownerships } => {
            Type::Function {
                return_type: Box::new(Spanned::dummy(substitute_type(&return_type.node, subs))),
                params: params.iter()
                    .map(|p| Spanned::dummy(substitute_type(&p.node, subs)))
                    .collect(),
                param_ownerships: param_ownerships.clone(),
            }
        }
        Type::Array { element, size } => {
            Type::Array {
                element: Box::new(Spanned::dummy(substitute_type(&element.node, subs))),
                size: size.clone(),
            }
        }
        Type::Slice { element } => {
            Type::Slice {
                element: Box::new(Spanned::dummy(substitute_type(&element.node, subs))),
            }
        }
        // Primitives and other types pass through unchanged
        _ => ty.clone(),
    }
}

/// Inject built-in Option[T] and Result[T, E] enum templates if not present.
fn inject_builtin_enums(enum_templates: &mut FxHashMap<String, ast::EnumDef>) {
    use crate::parser::ast::*;

    if !enum_templates.contains_key("Option") {
        enum_templates.insert("Option".to_string(), ast::EnumDef {
            attributes: vec![],
            visibility: Visibility::Public,
            name: Spanned::dummy("Option".to_string()),
            generic_params: Some(Spanned::dummy(GenericParams {
                params: vec![Spanned::dummy(GenericParam::Type(Spanned::dummy("T".to_string())))],
            })),
            variants: vec![
                Spanned::dummy(Variant {
                    name: Spanned::dummy("Some".to_string()),
                    fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                        name: Spanned::dummy("T".to_string()),
                        generic_args: vec![],
                    })]),
                }),
                Spanned::dummy(Variant {
                    name: Spanned::dummy("None".to_string()),
                    fields: VariantFields::Unit,
                }),
            ],
            doc_comment: None,
            span: crate::span::Span::dummy(),
        });
    }

    if !enum_templates.contains_key("Result") {
        enum_templates.insert("Result".to_string(), ast::EnumDef {
            attributes: vec![],
            visibility: Visibility::Public,
            name: Spanned::dummy("Result".to_string()),
            generic_params: Some(Spanned::dummy(GenericParams {
                params: vec![
                    Spanned::dummy(GenericParam::Type(Spanned::dummy("T".to_string()))),
                    Spanned::dummy(GenericParam::Type(Spanned::dummy("E".to_string()))),
                ],
            })),
            variants: vec![
                Spanned::dummy(Variant {
                    name: Spanned::dummy("Ok".to_string()),
                    fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                        name: Spanned::dummy("T".to_string()),
                        generic_args: vec![],
                    })]),
                }),
                Spanned::dummy(Variant {
                    name: Spanned::dummy("Error".to_string()),
                    fields: VariantFields::Tuple(vec![Spanned::dummy(Type::Named {
                        name: Spanned::dummy("E".to_string()),
                        generic_args: vec![],
                    })]),
                }),
            ],
            doc_comment: None,
            span: crate::span::Span::dummy(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn collect_generic_struct_template() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    pass
"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "Parse errors: {:?}", parser.errors);

        let mut collector = GenericCollector::new();
        collector.collect_templates(&module);

        assert!(collector.struct_templates.contains_key("Pair"));
    }

    #[test]
    fn discover_struct_instantiation() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](10, 3.14)
    pass
"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "Parse errors: {:?}", parser.errors);

        let mut collector = GenericCollector::new();
        collector.collect_templates(&module);
        collector.discover_usages(&module);

        assert!(!collector.instances.is_empty(), "Should discover at least one instantiation");
        let has_pair = collector.instances.iter().any(|(_, _, mangled, _)| {
            mangled.contains("Pair")
        });
        assert!(has_pair, "Should find Pair instantiation");
    }

    #[test]
    fn monomorphize_generic_struct() {
        let source = r#"struct Pair[A, B]:
    A first
    B second

void main():
    Pair[int, float] p = Pair[int, float](10, 3.14)
    pass
"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty());

        let mut collector = GenericCollector::new();
        collector.collect_templates(&module);
        collector.discover_usages(&module);

        let mut registry = TypeRegistry::new();
        let mut mapper = TypeMapper::new(&mut registry);

        collector.monomorphize_types(&mut mapper, &mut registry);

        // Should have a mangled Pair type registered
        let mangled_name = &collector.instances.iter()
            .find(|(base, _, _, _)| base == "Pair")
            .expect("Should have Pair instance")
            .2;

        assert!(mapper.lookup_named(mangled_name).is_some(),
            "Mangled Pair type should be registered in mapper");

        let type_def = registry.get_type_def(mangled_name)
            .expect("Mangled Pair type should have TypeDef");
        if let TypeDefKind::Struct(ref s) = type_def.kind {
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
    fn discover_generic_function() {
        let source = r#"T identity[T](T x) = x

void main():
    int a = identity[int](42)
    pass
"#;
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "Parse errors: {:?}", parser.errors);

        let mut collector = GenericCollector::new();
        collector.collect_templates(&module);
        collector.discover_usages(&module);

        let fn_instances: Vec<_> = collector.instances.iter()
            .filter(|(_, _, _, k)| matches!(k, TemplateKind::Function))
            .collect();
        assert!(!fn_instances.is_empty(), "Should discover identity function instantiation");
    }

    #[test]
    fn builtin_option_result_injected() {
        let mut collector = GenericCollector::new();
        let source = "void main():\n    pass\n";
        let mut parser = Parser::new(source);
        let module = parser.parse_module();
        collector.collect_templates(&module);

        assert!(collector.enum_templates.contains_key("Option"), "Option should be injected");
        assert!(collector.enum_templates.contains_key("Result"), "Result should be injected");
    }

    #[test]
    fn substitute_type_simple() {
        let subs = vec![
            ("T".to_string(), Type::Primitive(ast::PrimitiveType::Int)),
        ];
        let input = Type::Named {
            name: Spanned::dummy("T".to_string()),
            generic_args: vec![],
        };
        let result = substitute_type(&input, &subs);
        assert!(matches!(result, Type::Primitive(ast::PrimitiveType::Int)));
    }

    #[test]
    fn substitute_type_nested() {
        let subs = vec![
            ("T".to_string(), Type::Primitive(ast::PrimitiveType::Int)),
        ];
        // Option[T] → Option[int]
        let input = Type::Named {
            name: Spanned::dummy("Option".to_string()),
            generic_args: vec![Spanned::dummy(Type::Named {
                name: Spanned::dummy("T".to_string()),
                generic_args: vec![],
            })],
        };
        let result = substitute_type(&input, &subs);
        if let Type::Named { generic_args, .. } = &result {
            assert_eq!(generic_args.len(), 1);
            assert!(matches!(generic_args[0].node, Type::Primitive(ast::PrimitiveType::Int)));
        } else {
            panic!("Expected Named type");
        }
    }
}
