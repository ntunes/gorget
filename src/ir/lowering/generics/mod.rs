//! Generic monomorphization for GIR lowering.
//!
//! Walks the AST to discover all concrete generic instantiations (e.g., `Pair[int, str]`),
//! then creates monomorphized TypeDefs and Functions with substituted types.

mod substitute;

pub use substitute::substitute_type_pub;
use substitute::{substitute_type, substitute_function_body, inject_builtin_enums};

use rustc_hash::FxHashMap;

use crate::ir::types::*;
use crate::lexer::token::StringSegment;
use crate::parser::ast::{self, Expr, GenericParam, Item, Stmt, Type};
use crate::span::Spanned;

use super::types::{mangle_generic_name, op_mangle_suffix, TypeMapper};

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
    /// Non-generic enum definitions: enum_name → AST EnumDef.
    /// Used for AST-level expansion of `variant_payloads(T)` inside `meta for` match arms.
    non_generic_enum_defs: FxHashMap<String, ast::EnumDef>,
    /// Generic function templates: base_name → AST FunctionDef.
    fn_templates: FxHashMap<String, ast::FunctionDef>,
    /// Generic equip templates: base_type_name → Vec<EquipBlock>.
    equip_templates: FxHashMap<String, Vec<ast::EquipBlock>>,
    /// Discovered instantiations: (base_name, concrete_type_args) → mangled_name.
    /// Uses stringified type args as key for deduplication.
    instances: Vec<(String, Vec<Spanned<Type>>, String, TemplateKind)>,
    /// Already-registered mangled names (for dedup).
    registered: FxHashMap<String, ()>,
    /// When scanning inside a generic function/equip body, the set of type parameter
    /// names (e.g., {"T", "U"}). Calls using these as type args are deferred until
    /// transitive discovery resolves them to concrete types.
    current_generic_params: Option<Vec<String>>,
    /// Meta op bindings for each mangled function name: mangled → [(param_name, BinaryOp)].
    /// Populated during scan when MetaOpToken args are detected.
    meta_op_bindings: FxHashMap<String, Vec<(String, ast::BinaryOp)>>,
}

impl GenericCollector {
    pub fn new() -> Self {
        Self {
            struct_templates: FxHashMap::default(),
            enum_templates: FxHashMap::default(),
            non_generic_enum_defs: FxHashMap::default(),
            fn_templates: FxHashMap::default(),
            equip_templates: FxHashMap::default(),
            instances: Vec::new(),
            registered: FxHashMap::default(),
            current_generic_params: None,
            meta_op_bindings: FxHashMap::default(),
        }
    }

    /// Return the set of GIR-lowered generic equip method names.
    /// These are methods with Block/Expression bodies (not Extern/Declaration).
    pub fn gir_equip_method_names(&self) -> Vec<String> {
        use crate::parser::ast::FunctionBody;
        let mut names = Vec::new();
        for (base_name, _type_args, mangled_type_name, kind) in &self.instances {
            if !matches!(kind, TemplateKind::Struct | TemplateKind::Enum) { continue; }
            if let Some(equip_blocks) = self.equip_templates.get(base_name) {
                for equip in equip_blocks {
                    for method in &equip.items {
                        if !matches!(method.node.body, FunctionBody::Extern(_) | FunctionBody::Declaration) {
                            names.push(format!("{mangled_type_name}__{}", method.node.name.node));
                        }
                    }
                }
            }
        }
        names
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
                Item::Enum(e) if e.generic_params.is_none() => {
                    // Store non-generic enums for AST-level variant_payloads expansion
                    // during MetaFor match arm scanning.
                    self.non_generic_enum_defs.insert(e.name.node.clone(), e.clone());
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

    /// Check if a type references any of the current generic params.
    fn type_has_generic_param(&self, ty: &Type) -> bool {
        if let Some(ref params) = self.current_generic_params {
            match ty {
                Type::Named { name, generic_args } if generic_args.is_empty() => {
                    params.iter().any(|p| p == &name.node)
                }
                Type::Named { generic_args, .. } => {
                    generic_args.iter().any(|a| self.type_has_generic_param(&a.node))
                }
                _ => false,
            }
        } else {
            false
        }
    }

    /// Check if any type in a list of type args contains an unresolved generic param.
    fn has_unresolved_type_args(&self, type_args: &[Spanned<Type>]) -> bool {
        type_args.iter().any(|a| self.type_has_generic_param(&a.node))
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

    /// Register a concrete generic function instantiation that has `meta op` bindings.
    /// The mangled name gets an additional `__<op_suffix>` per op binding.
    fn register_instance_with_ops(
        &mut self,
        base_name: &str,
        type_args: &[Spanned<Type>],
        op_bindings: Vec<(String, ast::BinaryOp)>,
    ) -> String {
        // Base mangled name from type args
        let mut mangled = mangle_generic_name(base_name, type_args);
        // Append one suffix per op binding
        for (_, op) in &op_bindings {
            mangled.push_str("__");
            mangled.push_str(op_mangle_suffix(*op));
        }
        if !self.registered.contains_key(&mangled) {
            self.registered.insert(mangled.clone(), ());
            self.instances.push((
                base_name.to_string(),
                type_args.to_vec(),
                mangled.clone(),
                TemplateKind::Function,
            ));
        }
        // Always store/update the bindings (idempotent for same mangled name)
        self.meta_op_bindings.entry(mangled.clone()).or_insert(op_bindings);
        mangled
    }

    /// Return the meta op bindings for a mangled function name (empty slice if none).
    pub fn meta_op_bindings_for(&self, mangled: &str) -> &[(String, ast::BinaryOp)] {
        self.meta_op_bindings
            .get(mangled)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    /// Scan a function definition for generic usages.
    fn scan_function(&mut self, func: &ast::FunctionDef) {
        // Track generic params so we can skip unresolved type-arg usages
        let prev = self.current_generic_params.take();
        if let Some(ref gp) = func.generic_params {
            let names: Vec<String> = gp.node.params.iter().map(|p| match &p.node {
                GenericParam::Type { name, .. } => name.node.clone(),
                GenericParam::Lifetime(s) | GenericParam::Const { name: s, .. } => s.node.clone(),
            }).collect();
            self.current_generic_params = Some(names);
        }

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

        self.current_generic_params = prev;
    }

    /// Scan an AST type for generic instantiations.
    fn scan_type(&mut self, ty: &Spanned<Type>) {
        match &ty.node {
            Type::Named { name, generic_args } if !generic_args.is_empty() => {
                // Skip if any type arg contains an unresolved generic param
                if self.has_unresolved_type_args(generic_args) {
                    return;
                }
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
                } else if matches!(base.as_str(), "Vector" | "Dict" | "HashMap" | "Set" | "HashSet" | "Box") {
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
            Type::Ref(inner) | Type::Owned(inner) => {
                self.scan_type(inner);
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
                for item in arms {
                    match item {
                        ast::MatchItem::Arm(arm) => {
                            self.scan_expr(&arm.body);
                            if let Some(guard) = &arm.guard {
                                self.scan_expr(guard);
                            }
                        }
                        ast::MatchItem::MetaFor { vars, range, arm_template, .. } => {
                            // Expand variant_payloads at scan time so generic calls inside
                            // arm templates (e.g. col_slice_inner[T]) get monomorphized.
                            self.scan_meta_for_match_arm(vars, range, arm_template);
                        }
                    }
                }
                if let Some(else_body) = else_arm {
                    self.scan_block(else_body);
                }
            }
            Stmt::With { bindings, body } => {
                for binding in bindings {
                    self.scan_expr(&binding.expr);
                }
                self.scan_block(body);
            }
            Stmt::Loop { body } | Stmt::Unsafe { body } | Stmt::NamedScope { body, .. } => {
                self.scan_block(body);
            }
            Stmt::Assert { condition, message } | Stmt::AssertReturn { condition, message } => {
                self.scan_expr(condition);
                if let Some(msg) = message {
                    self.scan_expr(msg);
                }
            }
            Stmt::Snapshot { value, .. } => {
                self.scan_expr(value);
            }
            // Delayed meta stmts: scan all branches (conservative — we can't
            // evaluate typename(T) yet; all branches may reference generic calls).
            Stmt::MetaIf { condition, then_body, elif_branches, else_body, .. } => {
                self.scan_expr(condition);
                self.scan_block(then_body);
                for (cond, body) in elif_branches {
                    self.scan_expr(cond);
                    self.scan_block(body);
                }
                if let Some(eb) = else_body {
                    self.scan_block(eb);
                }
            }
            Stmt::MetaFor { range, body, .. } => {
                self.scan_expr(range);
                self.scan_block(body);
            }
            Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
                self.scan_expr(scrutinee);
                for (case_expr, body) in arms {
                    self.scan_expr(case_expr);
                    self.scan_block(body);
                }
                if let Some(eb) = else_arm {
                    self.scan_block(eb);
                }
            }
            Stmt::MetaWhile { body, .. } => {
                self.scan_block(body);
            }
            Stmt::MetaConst { value, .. } => {
                self.scan_expr(value);
            }
            _ => {}
        }
    }

    /// Scan a `meta for vname, T in variant_payloads(EnumName):` match item.
    ///
    /// Expands the range by looking up the enum definition in the AST, then scans
    /// the arm template body once per variant with concrete variable substitutions.
    /// This ensures generic calls like `col_slice_inner[T]` are discovered for
    /// monomorphization even though the outer function is non-generic.
    fn scan_meta_for_match_arm(
        &mut self,
        vars: &[crate::span::Spanned<String>],
        range: &Spanned<Expr>,
        arm_template: &ast::MatchArm,
    ) {
        use crate::semantic::meta::{MetaValue, meta_str_to_type, substitute_match_arm};

        // Extract enum name from variant_payloads(EnumName) call
        let enum_name = match Self::extract_variant_payloads_enum_name(range) {
            Some(n) => n,
            None => {
                // Unknown range form — scan template conservatively with no substitution
                self.scan_expr(&arm_template.body);
                return;
            }
        };

        // Look up enum definition in the AST-level store
        let enum_def = match self.non_generic_enum_defs.get(&enum_name) {
            Some(e) => e.clone(),
            None => {
                self.scan_expr(&arm_template.body);
                return;
            }
        };

        // For each variant, build substitution env and scan the concrete arm body
        for variant in &enum_def.variants {
            let vname = variant.node.name.node.clone();

            // Extract inner type arg from the variant's first tuple field, if any
            let inner_type_str: String = match &variant.node.fields {
                ast::VariantFields::Tuple(types) if types.len() == 1 => {
                    // For a field like TypedColumn[int], take the first generic arg
                    match &types[0].node {
                        Type::Named { generic_args, .. } if !generic_args.is_empty() => {
                            crate::semantic::meta::type_to_canonical_name(&generic_args[0].node)
                        }
                        other => crate::semantic::meta::type_to_canonical_name(other),
                    }
                }
                _ => String::new(),
            };

            let mut env: rustc_hash::FxHashMap<String, MetaValue> =
                rustc_hash::FxHashMap::default();
            let mut type_env: rustc_hash::FxHashMap<String, crate::parser::ast::Type> =
                rustc_hash::FxHashMap::default();

            if !vars.is_empty() {
                env.insert(vars[0].node.clone(), MetaValue::Str(vname));
            }
            if vars.len() >= 2 && !inner_type_str.is_empty() {
                let key = vars[1].node.clone();
                env.insert(key.clone(), MetaValue::Str(inner_type_str.clone()));
                type_env.insert(key, meta_str_to_type(&inner_type_str));
            }

            let mut concrete_arm = arm_template.clone();
            substitute_match_arm(&mut concrete_arm, &env, &type_env);
            self.scan_expr(&concrete_arm.body);
        }
    }

    /// Extract the enum type name from a `variant_payloads(EnumName)` call expression.
    fn extract_variant_payloads_enum_name(range: &Spanned<Expr>) -> Option<String> {
        if let Expr::Call { callee, args, .. } = &range.node {
            if let Expr::Identifier(name) = &callee.node {
                if name == "variant_payloads" && !args.is_empty() {
                    if let Expr::Identifier(enum_name) = &args[0].node.value.node {
                        return Some(enum_name.clone());
                    }
                }
            }
        }
        None
    }

    /// Scan an expression for generic usages.
    fn scan_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Call { callee, generic_args, args } => {
                self.scan_expr(callee);
                // Generic function/struct call: identity[int](42), TypedColumn[int](d, m)
                if let Some(type_args) = generic_args {
                    // Skip registering if any type arg is an unresolved generic param
                    // (will be discovered transitively when the outer function is instantiated)
                    if !self.has_unresolved_type_args(type_args) {
                        for arg in type_args {
                            self.scan_type(arg);
                        }
                        if let Expr::Identifier(name) = &callee.node {
                            if self.fn_templates.contains_key(name.as_str()) {
                                // Collect meta op bindings from MetaOpToken args
                                let template = self.fn_templates.get(name.as_str()).unwrap();
                                let op_bindings: Vec<(String, ast::BinaryOp)> = template
                                    .params
                                    .iter()
                                    .zip(args.iter())
                                    .filter_map(|(param, arg)| {
                                        if param.node.is_meta_op {
                                            if let Expr::MetaOpToken(op) = &arg.node.value.node {
                                                return Some((param.node.name.node.clone(), *op));
                                            }
                                        }
                                        None
                                    })
                                    .collect();
                                if op_bindings.is_empty() {
                                    self.register_instance(name, type_args, TemplateKind::Function);
                                } else {
                                    self.register_instance_with_ops(name, type_args, op_bindings);
                                }
                            } else if self.struct_templates.contains_key(name.as_str()) {
                                // Struct constructor call with generic args: TypedColumn[int](...)
                                self.register_instance(name, type_args, TemplateKind::Struct);
                            }
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
                    if !self.has_unresolved_type_args(type_args) {
                        for arg in type_args {
                            self.scan_type(arg);
                        }
                        if self.struct_templates.contains_key(name.node.as_str()) {
                            self.register_instance(&name.node, type_args, TemplateKind::Struct);
                        } else if self.enum_templates.contains_key(name.node.as_str()) {
                            self.register_instance(&name.node, type_args, TemplateKind::Enum);
                        }
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
            Expr::StringLiteral(lit) => {
                // Scan interpolation segments for generic usages (e.g., "{add_values[int](3, 4)}")
                for seg in &lit.segments {
                    if let StringSegment::Interpolation(text, _) = seg {
                        if let Ok(parsed) = crate::parser::Parser::new(text).parse_expr() {
                            self.scan_expr(&parsed);
                        }
                    }
                }
            }
            // Block expressions: multi-statement match arm bodies, do-blocks, etc.
            Expr::Block(block) | Expr::Do { body: block } => {
                self.scan_block(block);
            }
            // Expression-level match (arm bodies may contain VarDecls with generic types)
            Expr::Match { scrutinee, arms, else_arm } => {
                self.scan_expr(scrutinee);
                for arm in arms {
                    self.scan_expr(&arm.body);
                }
                if let Some(eb) = else_arm {
                    self.scan_expr(eb);
                }
            }
            Expr::MetaOpInfix { left, right, .. } => {
                self.scan_expr(left);
                self.scan_expr(right);
            }
            // MetaOpToken is a leaf — no sub-expressions to scan.
            Expr::MetaOpToken(_) => {}
            _ => {}
        }
    }

    /// Phase 2b: Transitively discover generic usages inside monomorphized function bodies.
    /// When `tensor_neg[int]` calls `tensor_zeros[T]`, the initial scan skips it (T is abstract).
    /// This phase substitutes T→int in the template body and re-scans to discover `tensor_zeros[int]`.
    pub fn discover_transitive(&mut self) {
        let mut i = 0;
        while i < self.instances.len() {
            let (base_name, type_args, _, kind) = self.instances[i].clone();
            i += 1;
            if !matches!(kind, TemplateKind::Function) {
                continue;
            }
            let template = match self.fn_templates.get(&base_name) {
                Some(t) => t.clone(),
                None => continue,
            };
            let subs = build_type_substitutions(template.generic_params.as_ref(), &type_args);
            if subs.is_empty() {
                continue;
            }
            // Substitute types in the template body and re-scan (with no generic params context,
            // so all discovered usages are concrete).
            let prev = self.current_generic_params.take();
            let substituted = substitute_function_body(&template, &subs);
            self.scan_function(&substituted);
            self.current_generic_params = prev;
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
                    } else if matches!(base_name.as_str(), "Vector" | "Dict" | "HashMap" | "Set" | "HashSet" | "Box") {
                        // Runtime collection types — no template to monomorphize, register alias
                        if !mapper.named_types.contains_key(mangled_name) {
                            super::types::register_collection_alias(mapper, registry, base_name, type_args, mangled_name);
                        }
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
        registry: &mut TypeRegistry,
        fn_sigs: &mut FxHashMap<String, (Vec<TypeId>, TypeId)>,
        fn_param_ownerships: &mut FxHashMap<String, Vec<crate::parser::ast::Ownership>>,
        fn_param_abis: &mut FxHashMap<String, Vec<super::context::ParamABI>>,
    ) {
        use crate::parser::ast::{Ownership, FunctionBody};
        for (base_name, type_args, mangled_name, kind) in &self.instances {
            if !matches!(kind, TemplateKind::Function) {
                continue;
            }
            if let Some(template) = self.fn_templates.get(base_name) {
                let subs = build_type_substitutions(template.generic_params.as_ref(), type_args);
                let ret_type = substitute_and_map(mapper, &template.return_type.node, &subs);

                // Detect bare Move-type params that are directly returned.
                // Upgrade these from Borrow to Move to prevent double-free.
                let mut move_override_params: std::collections::HashSet<String> = std::collections::HashSet::new();
                if registry.is_resource_type(ret_type) {
                    let returned_param_name = match &template.body {
                        FunctionBody::Expression(expr) => {
                            if let Expr::Identifier(name) = &expr.node {
                                Some(name.as_str())
                            } else { None }
                        }
                        FunctionBody::Block(block) => {
                            if block.stmts.len() == 1 {
                                if let Stmt::Return(Some(expr)) = &block.stmts[0].node {
                                    if let Expr::Identifier(name) = &expr.node {
                                        Some(name.as_str())
                                    } else { None }
                                } else { None }
                            } else { None }
                        }
                        _ => None,
                    };
                    if let Some(name) = returned_param_name {
                        for p in &template.params {
                            if !p.node.is_meta_op
                                && p.node.name.node == name
                                && p.node.ownership == Ownership::Borrow
                            {
                                let base = substitute_and_map(mapper, &p.node.type_.node, &subs);
                                if registry.is_resource_type(base) {
                                    move_override_params.insert(name.to_string());
                                }
                            }
                        }
                    }
                }

                let param_types: Vec<TypeId> = template.params.iter()
                    .filter(|p| !p.node.is_meta_op) // meta op params have no runtime slot
                    .map(|p| {
                        let base = substitute_and_map(mapper, &p.node.type_.node, &subs);
                        // MutableBorrow params become MutPtr in the GIR
                        if matches!(p.node.ownership, Ownership::MutableBorrow) {
                            registry.insert(GirType::MutPtr(base))
                        } else {
                            base
                        }
                    })
                    .collect();

                // Register param ownerships so lower_call_arg uses correct semantics
                let param_ownerships: Vec<Ownership> = template.params.iter()
                    .filter(|p| !p.node.is_meta_op)
                    .map(|p| {
                        if move_override_params.contains(&p.node.name.node) {
                            Ownership::Move
                        } else {
                            p.node.ownership
                        }
                    })
                    .collect();

                // Compute ParamABI for monomorphized generic functions
                let abis: Vec<super::context::ParamABI> = template.params.iter()
                    .filter(|p| !p.node.is_meta_op)
                    .map(|p| {
                        let ownership = if move_override_params.contains(&p.node.name.node) {
                            Ownership::Move
                        } else {
                            p.node.ownership
                        };
                        let base = substitute_and_map(mapper, &p.node.type_.node, &subs);
                        let is_move = registry.is_resource_type(base);
                        match ownership {
                            Ownership::MutableBorrow => super::context::ParamABI::ByMutPtr,
                            Ownership::Move if is_move => super::context::ParamABI::ByMutPtr,
                            Ownership::Borrow if is_move => super::context::ParamABI::ByPtr,
                            _ => super::context::ParamABI::ByValue,
                        }
                    })
                    .collect();

                fn_sigs.insert(mangled_name.clone(), (param_types, ret_type));
                fn_param_ownerships.insert(mangled_name.clone(), param_ownerships);
                fn_param_abis.insert(mangled_name.clone(), abis);
            }
        }
    }

    /// Register monomorphized equip method signatures.
    pub fn register_equip_sigs(
        &self,
        mapper: &mut TypeMapper,
        registry: &mut TypeRegistry,
        fn_sigs: &mut FxHashMap<String, (Vec<TypeId>, TypeId)>,
        fn_param_abis: &mut FxHashMap<String, Vec<super::context::ParamABI>>,
    ) {
        self.register_equip_sigs_with_defaults(mapper, registry, fn_sigs, fn_param_abis, None);
    }

    /// Register monomorphized equip method signatures, including default trait methods.
    ///
    /// Uses the mutable mapper (`substitute_and_map_mut`) so that return types like
    /// `Option[double]` are auto-registered in the type registry if not already present.
    /// Without this, return types that haven't been discovered yet resolve to UNIT_TYPE,
    /// causing the IR to emit void calls and silently discard the result.
    pub fn register_equip_sigs_with_defaults(
        &self,
        mapper: &mut TypeMapper,
        registry: &mut TypeRegistry,
        fn_sigs: &mut FxHashMap<String, (Vec<TypeId>, TypeId)>,
        fn_param_abis: &mut FxHashMap<String, Vec<super::context::ParamABI>>,
        ast_module: Option<&crate::parser::ast::Module>,
    ) {
        use crate::parser::ast::{Item, Ownership, TraitItem, FunctionBody};
        for (base_name, type_args, mangled_type_name, kind) in &self.instances {
            if !matches!(kind, TemplateKind::Struct | TemplateKind::Enum) {
                continue;
            }
            if let Some(equip_blocks) = self.equip_templates.get(base_name) {
                let equip_blocks = equip_blocks.clone();
                for equip in &equip_blocks {
                    let subs = build_equip_type_substitutions(equip, type_args);
                    let mut implemented = Vec::new();
                    for method in &equip.items {
                        let method_mangled = format!("{mangled_type_name}__{}", method.node.name.node);
                        let ret_type = substitute_and_map_mut(mapper, registry, &method.node.return_type.node, &subs);
                        let has_self = method.node.params.first()
                            .map(|p| p.node.name.node == "self")
                            .unwrap_or(false);
                        let mut param_types = Vec::new();
                        let mut abis = Vec::new();
                        if has_self {
                            let self_type_id = mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
                            let self_ptr_type = registry.insert(GirType::Ptr(self_type_id));
                            param_types.push(self_ptr_type);
                            let self_is_mutable = method.node.params.first()
                                .map(|p| matches!(p.node.ownership, Ownership::MutableBorrow))
                                .unwrap_or(false);
                            abis.push(if self_is_mutable {
                                super::context::ParamABI::ByMutPtr
                            } else {
                                super::context::ParamABI::ByPtr
                            });
                        }
                        for p in &method.node.params {
                            if p.node.name.node == "self" {
                                continue;
                            }
                            let base = substitute_and_map_mut(mapper, registry, &p.node.type_.node, &subs);
                            param_types.push(base);
                            let is_move = registry.is_resource_type(base);
                            abis.push(match p.node.ownership {
                                Ownership::MutableBorrow => super::context::ParamABI::ByMutPtr,
                                Ownership::Move if is_move => super::context::ParamABI::ByMutPtr,
                                Ownership::Borrow if is_move => super::context::ParamABI::ByPtr,
                                _ => super::context::ParamABI::ByValue,
                            });
                        }
                        fn_sigs.insert(method_mangled.clone(), (param_types, ret_type));
                        fn_param_abis.insert(method_mangled, abis);
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
                                                let ret_type = substitute_and_map_mut(mapper, registry, &dm.return_type.node, &subs);
                                                let self_type_id = mapper.lookup_named(mangled_type_name).unwrap_or(UNIT_TYPE);
                                                let self_ptr_type = registry.insert(GirType::Ptr(self_type_id));
                                                let mut param_types = vec![self_ptr_type];
                                                let mut abis = vec![super::context::ParamABI::ByPtr]; // default methods: self by const ptr
                                                for p in &dm.params {
                                                    if p.node.name.node == "self" { continue; }
                                                    let base = substitute_and_map_mut(mapper, registry, &p.node.type_.node, &subs);
                                                    param_types.push(base);
                                                    let is_move = registry.is_resource_type(base);
                                                    abis.push(match p.node.ownership {
                                                        Ownership::MutableBorrow => super::context::ParamABI::ByMutPtr,
                                                        Ownership::Move if is_move => super::context::ParamABI::ByMutPtr,
                                                        Ownership::Borrow if is_move => super::context::ParamABI::ByPtr,
                                                        _ => super::context::ParamABI::ByValue,
                                                    });
                                                }
                                                fn_sigs.insert(m_mangled.clone(), (param_types, ret_type));
                                                fn_param_abis.insert(m_mangled, abis);
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

    // Box types need Move + Trivial("free") drop metadata for RAII.
    // ReadGuard[T] / WriteGuard[T] need Move + Trivial drop to release the pthread rwlock.
    // Collection types (Vector, Dict, etc.) get their own drop strategies.
    let metadata = if template.name.node == "Box" {
        TypeMetadata {
            size: None,
            align: None,
            copy_semantics: CopySemantics::Resource,
            drop_strategy: DropStrategy::Trivial("free".to_string()),
        }
    } else if matches!(template.name.node.as_str(), "ReadGuard" | "WriteGuard") {
        TypeMetadata {
            size: None,
            align: None,
            copy_semantics: CopySemantics::Resource,
            drop_strategy: DropStrategy::Trivial(format!("{mangled_name}__drop")),
        }
    } else {
        TypeMetadata::default()
    };

    let type_def = TypeDef {
        name: mangled_name.to_string(),
        kind: TypeDefKind::Struct(StructDef { fields }),
        metadata,
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
                GenericParam::Type { name: s, .. } => s.node.clone(),
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
        let source = r#"T identity[T](T x): x

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
