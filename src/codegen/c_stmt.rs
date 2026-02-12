/// Statement codegen: convert Gorget statements to C statements.
use crate::parser::ast::{BinaryOp, Block, Expr, Pattern, Stmt, Type};
use crate::span::Spanned;

use super::c_emitter::CEmitter;
use super::c_mangle;
use super::c_types;
use super::{CodegenContext, DropAction, DropEntry, DropScopeKind};

impl CodegenContext<'_> {
    /// Generate C code for a block of statements.
    pub fn gen_block(&mut self, block: &Block, emitter: &mut CEmitter) {
        for stmt in &block.stmts {
            self.gen_stmt(&stmt.node, emitter);
        }
    }

    // ─── Drop Scope Helpers ─────────────────────────────────

    /// Push a new drop scope onto the stack.
    pub fn push_drop_scope(&mut self, kind: DropScopeKind) {
        self.drop_scopes.push((kind, Vec::new()));
    }

    /// Pop the top drop scope and emit cleanup for its entries.
    pub fn pop_drop_scope(&mut self, emitter: &mut CEmitter) {
        if let Some((_kind, entries)) = self.drop_scopes.pop() {
            for entry in entries.iter().rev() {
                Self::emit_drop_entry(entry, emitter);
            }
        }
    }

    /// Emit cleanup for all scopes from innermost up to and including the
    /// first scope matching `kind`. Does NOT pop any scopes.
    pub fn emit_cleanup_to(&mut self, kind: DropScopeKind, emitter: &mut CEmitter) {
        for (scope_kind, entries) in self.drop_scopes.iter().rev() {
            for entry in entries.iter().rev() {
                Self::emit_drop_entry(entry, emitter);
            }
            if *scope_kind == kind {
                break;
            }
        }
    }

    /// Register a droppable variable in the current (topmost) scope.
    pub fn register_droppable(&mut self, var_name: &str, action: DropAction) {
        if let Some((_kind, entries)) = self.drop_scopes.last_mut() {
            entries.push(DropEntry {
                var_name: var_name.to_string(),
                action,
            });
        }
    }

    /// Check if any drop scope currently has droppable entries.
    fn has_droppable_entries(&mut self) -> bool {
        self.drop_scopes
            .iter()
            .any(|(_, entries)| !entries.is_empty())
    }

    /// Emit the C cleanup code for a single drop entry.
    fn emit_drop_entry(entry: &DropEntry, emitter: &mut CEmitter) {
        match &entry.action {
            DropAction::BoxFree => {
                emitter.emit_line(&format!("free({});", entry.var_name));
            }
            DropAction::TraitObjFree => {
                emitter.emit_line(&format!("free({}.data);", entry.var_name));
            }
            DropAction::FileClose => {
                emitter.emit_line(&format!("gorget_file_close(&{});", entry.var_name));
            }
            DropAction::UserDrop { type_name } => {
                let drop_fn = c_mangle::mangle_trait_method("Drop", type_name, "drop");
                emitter.emit_line(&format!("{drop_fn}(&{});", entry.var_name));
            }
        }
    }

    /// Check if a declared type needs drop and register it if so.
    fn maybe_register_droppable(&mut self, var_name: &str, ty: &Type) {
        match ty {
            // Box[T] — if T is a trait, register TraitObjFree (free .data field)
            Type::Named { name, generic_args }
                if name.node == "Box"
                    && !generic_args.is_empty() =>
            {
                let is_trait_obj = if let Some(Type::Named { name: inner, generic_args: inner_args }) =
                    generic_args.first().map(|a| &a.node)
                {
                    if inner_args.is_empty() {
                        self.scopes.lookup(&inner.node).map_or(false, |def_id| {
                            self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Trait
                        })
                    } else {
                        false
                    }
                } else {
                    false
                };
                if is_trait_obj {
                    self.register_droppable(var_name, DropAction::TraitObjFree);
                } else {
                    self.register_droppable(var_name, DropAction::BoxFree);
                }
            }
            // File → auto-close on scope exit
            Type::Named { name, generic_args } if name.node == "File" && generic_args.is_empty() => {
                self.register_droppable(var_name, DropAction::FileClose);
            }
            // User-defined struct with Drop impl
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                if self.traits.has_trait_impl_by_name(&name.node, "Drop") {
                    self.register_droppable(
                        var_name,
                        DropAction::UserDrop {
                            type_name: name.node.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }

    /// Generate C code for a single statement.
    pub fn gen_stmt(&mut self, stmt: &Stmt, emitter: &mut CEmitter) {
        match stmt {
            Stmt::VarDecl {
                is_const,
                type_,
                pattern,
                value,
                ..
            } => {
                self.gen_var_decl(*is_const, type_, pattern, value, emitter);
            }

            Stmt::Expr(expr) => {
                let e = self.gen_expr(expr);
                emitter.emit_line(&format!("{e};"));
            }

            Stmt::Assign { target, value } => {
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                emitter.emit_line(&format!("{t} = {v};"));
            }

            Stmt::CompoundAssign { target, op, value } => {
                // String +=: s = gorget_str_concat(s, rhs)
                if *op == BinaryOp::Add {
                    let target_type = self.infer_c_type_from_expr(&target.node);
                    if target_type == "const char*" {
                        let t = self.gen_expr(target);
                        let v = self.gen_expr(value);
                        emitter.emit_line(&format!("{t} = gorget_str_concat({t}, {v});"));
                        return;
                    }
                }
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul) && !self.overflow_wrap {
                    let macro_name = match op {
                        BinaryOp::Add => "GORGET_CHECKED_ADD_ASSIGN",
                        BinaryOp::Sub => "GORGET_CHECKED_SUB_ASSIGN",
                        BinaryOp::Mul => "GORGET_CHECKED_MUL_ASSIGN",
                        _ => unreachable!(),
                    };
                    emitter.emit_line(&format!("{macro_name}({t}, {v});"));
                } else {
                    let c_op = compound_op_to_c(*op);
                    emitter.emit_line(&format!("{t} {c_op} {v};"));
                }
            }

            Stmt::Return(expr) => {
                if self.has_droppable_entries() {
                    if let Some(expr) = expr {
                        let e = self.gen_expr(expr);
                        emitter.emit_line(&format!("__typeof__({e}) __ret_tmp = {e};"));
                        self.emit_cleanup_to(DropScopeKind::Function, emitter);
                        emitter.emit_line("return __ret_tmp;");
                    } else {
                        self.emit_cleanup_to(DropScopeKind::Function, emitter);
                        emitter.emit_line("return;");
                    }
                } else if let Some(expr) = expr {
                    let e = self.gen_expr(expr);
                    emitter.emit_line(&format!("return {e};"));
                } else {
                    emitter.emit_line("return;");
                }
            }

            Stmt::Break(_) => {
                if self.has_droppable_entries() {
                    self.emit_cleanup_to(DropScopeKind::Loop, emitter);
                }
                emitter.emit_line("break;");
            }

            Stmt::Continue => {
                if self.has_droppable_entries() {
                    self.emit_cleanup_to(DropScopeKind::Loop, emitter);
                }
                emitter.emit_line("continue;");
            }

            Stmt::Pass => {
                emitter.emit_line("(void)0;");
            }

            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                let cond = self.gen_expr(condition);
                emitter.emit_line(&format!("if ({cond}) {{"));
                emitter.indent();
                self.gen_block(then_body, emitter);
                emitter.dedent();

                for (elif_cond, elif_body) in elif_branches {
                    let ec = self.gen_expr(elif_cond);
                    emitter.emit_line(&format!("}} else if ({ec}) {{"));
                    emitter.indent();
                    self.gen_block(elif_body, emitter);
                    emitter.dedent();
                }

                if let Some(else_body) = else_body {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    self.gen_block(else_body, emitter);
                    emitter.dedent();
                }

                emitter.emit_line("}");
            }

            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                let cond = self.gen_expr(condition);
                if let Some(else_block) = else_body {
                    // while-else: flag tracks if we broke out
                    let flag = emitter.fresh_temp();
                    emitter.emit_line(&format!("bool {flag} = false;"));
                    emitter.emit_line(&format!("while ({cond}) {{"));
                    emitter.indent();
                    self.push_drop_scope(DropScopeKind::Loop);
                    self.gen_block_with_break_flag(body, &flag, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                    emitter.emit_line(&format!("if (!{flag}) {{"));
                    emitter.indent();
                    self.gen_block(else_block, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                } else {
                    emitter.emit_line(&format!("while ({cond}) {{"));
                    emitter.indent();
                    self.push_drop_scope(DropScopeKind::Loop);
                    self.gen_block(body, emitter);
                    self.pop_drop_scope(emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                }
            }

            Stmt::Loop { body } => {
                emitter.emit_line("while (1) {");
                emitter.indent();
                self.push_drop_scope(DropScopeKind::Loop);
                self.gen_block(body, emitter);
                self.pop_drop_scope(emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            Stmt::For {
                pattern,
                iterable,
                body,
                else_body,
                ..
            } => {
                self.gen_for_loop_with_else(pattern, iterable, body, else_body, emitter);
            }

            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.gen_match_stmt(scrutinee, arms, else_arm, emitter);
            }

            Stmt::Throw(expr) => {
                let e = self.gen_expr(expr);
                // str maps to const char* in all cases — pass directly
                emitter.emit_line(&format!("GORGET_THROW({e}, 1);"));
            }

            Stmt::With { bindings, body } => {
                emitter.emit_line("{");
                emitter.indent();
                for binding in bindings {
                    let e = self.gen_expr(&binding.expr);
                    let name = c_mangle::escape_keyword(&binding.name.node);
                    emitter.emit_line(&format!("/* with */ void* {name} = (void*)({e});"));
                }
                self.gen_block(body, emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            Stmt::Unsafe { body } => {
                emitter.emit_line("/* unsafe */ {");
                emitter.indent();
                self.gen_block(body, emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }

            Stmt::Assert { condition, message } => {
                if !self.strip_asserts {
                    let cond = self.gen_expr(condition);
                    let msg = match message {
                        Some(m) => self.gen_expr(m),
                        None => "\"assertion failed\"".to_string(),
                    };
                    emitter.emit_line(&format!("if (!({cond})) gorget_panic({msg});"));
                }
            }

            Stmt::Item(_) => {
                // Nested items handled during top-level pass
            }
        }
    }

    /// Generate a variable declaration.
    fn gen_var_decl(
        &mut self,
        is_const: bool,
        type_: &Spanned<Type>,
        pattern: &Spanned<Pattern>,
        value: &Spanned<Expr>,
        emitter: &mut CEmitter,
    ) {
        match &pattern.node {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                let const_prefix = if is_const { "const " } else { "" };

                // Special handling for trait object construction:
                // Box[dynamic Trait] x = Box.new(ConcreteType(...))
                if let Some((trait_name, concrete_type, inner_expr)) =
                    self.extract_trait_object_construction(type_, value)
                {
                    let trait_obj_type = c_mangle::mangle_trait_obj(&trait_name);
                    let vtable_instance = c_mangle::mangle_vtable_instance(&trait_name, &concrete_type);
                    let inner_val = self.gen_expr(inner_expr);

                    emitter.emit_line(&format!(
                        "{concrete_type}* __box_{escaped} = ({concrete_type}*)malloc(sizeof({concrete_type}));"
                    ));
                    emitter.emit_line(&format!("*__box_{escaped} = {inner_val};"));
                    emitter.emit_line(&format!(
                        "{const_prefix}{trait_obj_type} {escaped} = {{ .data = (void*)__box_{escaped}, .vtable = &{vtable_instance} }};"
                    ));
                    return;
                }

                // Special handling for array literals: emit as C array declarations.
                // But NOT when the declared type is a collection type like Vector[T].
                if let Expr::ArrayLiteral(elements) = &value.node {
                    let is_collection_type = matches!(&type_.node,
                        Type::Named { name, generic_args } if !generic_args.is_empty()
                            && matches!(name.node.as_str(), "Vector" | "List" | "Array" | "Set" | "Dict" | "HashMap" | "Map")
                    );
                    if !is_collection_type {
                        let elem_type = match &type_.node {
                            Type::Inferred => {
                                if let Some(first) = elements.first() {
                                    self.infer_c_type_from_expr(&first.node)
                                } else {
                                    "int64_t".to_string()
                                }
                            }
                            _ => self.type_to_c(&type_.node),
                        };
                        let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                        emitter.emit_line(&format!(
                            "{const_prefix}{elem_type} {escaped}[] = {{{}}};",
                            elems.join(", ")
                        ));
                        return;
                    }
                    // Collection type with array literal: create a GorgetArray and push elements
                    let elem_type = if let Type::Named { generic_args, .. } = &type_.node {
                        if let Some(first_arg) = generic_args.first() {
                            self.type_to_c(&first_arg.node)
                        } else {
                            "int64_t".to_string()
                        }
                    } else {
                        "int64_t".to_string()
                    };
                    emitter.emit_line(&format!(
                        "{const_prefix}GorgetArray {escaped} = gorget_array_new(sizeof({elem_type}));"
                    ));
                    for e in elements {
                        let val = self.gen_expr(e);
                        emitter.emit_line(&format!(
                            "{{ {elem_type} __tmp = {val}; gorget_array_push(&{escaped}, &__tmp); }}"
                        ));
                    }
                    return;
                }

                // Special handling for tuple literals: emit with named typedef
                if let Expr::TupleLiteral(elements) = &value.node {
                    let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                    let c_field_types: Vec<String> = elements
                        .iter()
                        .map(|elem| self.infer_c_type_from_expr(&elem.node))
                        .collect();
                    let tuple_name = self.register_tuple_typedef(&c_field_types);
                    emitter.emit_line(&format!(
                        "{const_prefix}{tuple_name} {escaped} = ({tuple_name}){{{}}};",
                        elems.join(", ")
                    ));
                    return;
                }

                let c_type = self.resolve_decl_type(type_, value, Some(name));
                if c_type == "GorgetClosure" {
                    self.closure_vars.insert(escaped.clone());
                }
                // Set type hint for unit variant constructors like None()
                let prev_hint = self.decl_type_hint.clone();
                self.decl_type_hint = Some(type_.node.clone());
                let val = self.gen_expr(value);
                self.decl_type_hint = prev_hint;
                let decl = c_types::c_declare(&c_type, &escaped);
                emitter.emit_line(&format!("{const_prefix}{decl} = {val};"));

                // Register droppable variable for RAII cleanup
                self.maybe_register_droppable(&escaped, &type_.node);
            }
            Pattern::Wildcard => {
                let val = self.gen_expr(value);
                emitter.emit_line(&format!("(void){val};"));
            }
            _ => {
                let c_type = self.resolve_decl_type(type_, value, None);
                let val = self.gen_expr(value);
                let decl = c_types::c_declare(&c_type, "__pat");
                emitter.emit_line(&format!("/* pattern decl */ {decl} = {val};"));
            }
        }
    }

    /// Resolve the C type for a declaration, handling `auto` (Inferred).
    /// Also registers generic instantiations when a user-defined generic type is used.
    fn resolve_decl_type(
        &mut self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
        var_name: Option<&str>,
    ) -> String {
        match &type_.node {
            Type::Inferred => {
                // Special-case capturing closures → GorgetClosure
                if let Expr::Closure { params, body, .. } = &value.node {
                    let bound: std::collections::HashSet<&str> =
                        params.iter().map(|p| p.node.name.node.as_str()).collect();
                    let captures = self.collect_free_vars(&body.node, &bound);
                    if !captures.is_empty() {
                        return "GorgetClosure".to_string();
                    }
                }
                // First, check if the semantic analysis stored a resolved type
                if let Some(name) = var_name {
                    if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                        let def = self.scopes.get_def(def_id);
                        if let Some(type_id) = def.type_id {
                            return c_types::type_id_to_c(type_id, self.types, self.scopes);
                        }
                    }
                }
                // Fall back to expression-based inference
                self.infer_c_type_from_expr(&value.node)
            }
            _ => self.type_to_c_with_registration(&type_.node),
        }
    }

    /// Best-effort C type inference from a value expression.
    pub(super) fn infer_c_type_from_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IntLiteral(_) => "int64_t".to_string(),
            Expr::FloatLiteral(_) => "double".to_string(),
            Expr::BoolLiteral(_) => "bool".to_string(),
            Expr::CharLiteral(_) => "char".to_string(),
            Expr::StringLiteral(_) => "const char*".to_string(),
            Expr::BinaryOp { op, left, .. } => {
                use crate::parser::ast::BinaryOp;
                match op {
                    BinaryOp::Eq
                    | BinaryOp::Neq
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::LtEq
                    | BinaryOp::GtEq
                    | BinaryOp::And
                    | BinaryOp::Or
                    | BinaryOp::In => "bool".to_string(),
                    _ => self.infer_c_type_from_expr(&left.node),
                }
            }
            Expr::UnaryOp { operand, .. } => self.infer_c_type_from_expr(&operand.node),
            Expr::Call { callee, .. } => {
                // Try to look up the return type of the function
                if let Expr::Identifier(name) = &callee.node {
                    if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                        if let Some(func_info) = self.function_info.get(&def_id) {
                            if let Some(ret_type_id) = func_info.return_type_id {
                                return c_types::type_id_to_c(ret_type_id, self.types, self.scopes);
                            }
                        }
                    }
                }
                "int64_t".to_string()
            }
            Expr::Identifier(name) => {
                if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                    let def = self.scopes.get_def(def_id);
                    if let Some(type_id) = def.type_id {
                        return c_types::type_id_to_c(type_id, self.types, self.scopes);
                    }
                }
                "int64_t".to_string()
            }
            Expr::FieldAccess { object, field } => {
                let obj_type = self.infer_receiver_type(object);
                if obj_type != "Unknown" {
                    let key = (obj_type, field.node.clone());
                    if let Some(field_type) = self.field_type_names.get(&key) {
                        return super::c_types::ast_type_to_c(field_type, self.scopes);
                    }
                }
                "int64_t".to_string()
            }
            Expr::StructLiteral { name, .. } => name.node.clone(),
            Expr::TupleLiteral(elements) => {
                let c_field_types: Vec<String> = elements
                    .iter()
                    .map(|elem| self.infer_c_type_from_expr(&elem.node))
                    .collect();
                super::c_mangle::mangle_tuple(&c_field_types)
            }
            Expr::MethodCall {
                receiver, method, ..
            } => {
                let recv_c_type = self.infer_receiver_c_type(receiver);
                if let Some(tid) = recv_c_type
                    .as_deref()
                    .and_then(|rt| self.builtin_method_return_type(rt, &method.node))
                {
                    return c_types::type_id_to_c(tid, self.types, self.scopes);
                }
                let type_name = self.infer_receiver_type(receiver);
                for impl_info in &self.traits.impls {
                    if impl_info.self_type_name == type_name {
                        if let Some((_def_id, sig)) = impl_info.methods.get(method.node.as_str())
                        {
                            return c_types::type_id_to_c(sig.return_type, self.types, self.scopes);
                        }
                    }
                }
                "int64_t".to_string()
            }
            _ => "int64_t".to_string(),
        }
    }

    /// Detect trait object construction pattern: `Box[Trait] x = Box.new(ConcreteValue)`.
    /// When the type annotation is `Box[T]` and `T` is a trait, this extracts the
    /// trait name, concrete type name, and inner expression for trait object codegen.
    fn extract_trait_object_construction<'b>(
        &mut self,
        type_: &Spanned<Type>,
        value: &'b Spanned<Expr>,
    ) -> Option<(String, String, &'b Spanned<Expr>)> {
        // Check if type is Box[TraitName] where TraitName is a trait
        let trait_name = match &type_.node {
            Type::Named { name, generic_args }
                if name.node == "Box" && generic_args.len() == 1 =>
            {
                if let Type::Named {
                    name: inner_name,
                    generic_args: inner_args,
                } = &generic_args[0].node
                {
                    if inner_args.is_empty() {
                        let def_id = self.scopes.lookup(&inner_name.node)?;
                        let def = self.scopes.get_def(def_id);
                        if def.kind == crate::semantic::scope::DefKind::Trait {
                            Some(inner_name.node.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }?;

        // Extract inner expression from Box.new(expr) or Box(expr)
        let inner_expr = match &value.node {
            Expr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => {
                if let Expr::Identifier(name) = &receiver.node {
                    if name == "Box" && method.node == "new" && args.len() == 1 {
                        Some(&args[0].node.value)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Expr::Call { callee, args, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    if name == "Box" && args.len() == 1 {
                        Some(&args[0].node.value)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }?;

        // Infer the concrete type from the inner expression (struct constructor call)
        let concrete_type = match &inner_expr.node {
            Expr::Call { callee, .. } => {
                if let Expr::Identifier(name) = &callee.node {
                    Some(name.clone())
                } else {
                    None
                }
            }
            _ => None,
        }?;

        Some((trait_name, concrete_type, inner_expr))
    }

    /// Check if an iterable expression resolves to a GorgetArray type.
    fn is_gorget_array_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            let c_type = c_types::type_id_to_c(tid, self.types, self.scopes);
            return c_type == "GorgetArray";
        }
        // Also check inferred type as fallback
        let c_type = self.infer_c_type_from_expr(&expr.node);
        c_type == "GorgetArray"
    }

    /// Check if an iterable expression resolves to a GorgetMap (Dict) type.
    fn is_gorget_map_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, _) = self.types.get(tid) {
                let def_name = &self.scopes.get_def(*def_id).name;
                return matches!(def_name.as_str(), "Dict" | "HashMap" | "Map");
            }
        }
        false
    }

    /// Check if an iterable expression resolves to a GorgetSet (Set) type.
    /// Must use Gorget-level type name since GorgetSet is typedef'd to GorgetMap at C level.
    fn is_gorget_set_expr(&mut self, expr: &Spanned<Expr>) -> bool {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, _) = self.types.get(tid) {
                let def_name = &self.scopes.get_def(*def_id).name;
                return matches!(def_name.as_str(), "Set" | "HashSet");
            }
        }
        false
    }

    /// Infer the element C type for a Set expression.
    fn infer_set_elem_type(&mut self, expr: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                if let Some(&elem_tid) = args.first() {
                    return super::c_types::type_id_to_c(elem_tid, self.types, self.scopes);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Infer the Gorget-level type name from an expression (e.g. "Counter").
    fn infer_type_name_from_expr(&mut self, expr: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(expr) {
            match self.types.get(tid) {
                crate::semantic::types::ResolvedType::Defined(def_id) => {
                    return self.scopes.get_def(*def_id).name.clone();
                }
                crate::semantic::types::ResolvedType::Generic(def_id, _) => {
                    return self.scopes.get_def(*def_id).name.clone();
                }
                _ => {}
            }
        }
        if let Expr::StructLiteral { name, .. } = &expr.node {
            return name.node.clone();
        }
        if let Expr::Call { callee, .. } = &expr.node {
            if let Expr::Identifier(name) = &callee.node {
                return name.clone();
            }
        }
        String::new()
    }

    /// Check if an iterable expression has an Iterator[T] trait implementation.
    fn has_iterator_impl(&mut self, expr: &Spanned<Expr>) -> bool {
        let type_name = self.infer_type_name_from_expr(expr);
        if type_name.is_empty() {
            return false;
        }
        self.traits.impls.iter().any(|i|
            i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterator")
        )
    }

    /// Get the element C type for an Iterator[T] implementation.
    pub(crate) fn get_iterator_elem_c_type(&mut self, expr: &Spanned<Expr>) -> String {
        let type_name = self.infer_type_name_from_expr(expr);
        for imp in &self.traits.impls {
            if imp.self_type_name == type_name && imp.trait_name.as_deref() == Some("Iterator") {
                if let Some(first_arg) = imp.trait_generic_args.first() {
                    return c_types::ast_type_to_c(first_arg, self.scopes);
                }
            }
        }
        "int64_t".to_string()
    }

    /// Generate a for-loop over a user-defined Iterator[T] type.
    fn gen_for_loop_iterator(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        emitter: &mut CEmitter,
    ) {
        let var_name = match &pattern.node {
            Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__gorget_i".to_string(),
        };
        let iter_expr = self.gen_expr(iterable);
        let type_name = self.infer_type_name_from_expr(iterable);
        let elem_c_type = self.get_iterator_elem_c_type(iterable);

        // Register Option[ElemType] so its typedef is emitted
        let option_mangled = self.register_generic("Option", &[elem_c_type.clone()], super::GenericInstanceKind::Enum);
        let tag_none = c_mangle::mangle_tag(&option_mangled, "None");
        let next_fn = c_mangle::mangle_trait_method("Iterator", &type_name, "next");
        let next_tmp = emitter.fresh_temp();

        emitter.emit_line("while (1) {");
        emitter.indent();
        self.push_drop_scope(DropScopeKind::Loop);
        emitter.emit_line(&format!("{option_mangled} {next_tmp} = {next_fn}(&{iter_expr});"));
        emitter.emit_line(&format!("if ({next_tmp}.tag == {tag_none}) break;"));
        emitter.emit_line(&format!("{elem_c_type} {var_name} = {next_tmp}.data.Some._0;"));
        self.gen_block(body, emitter);
        self.pop_drop_scope(emitter);
        emitter.dedent();
        emitter.emit_line("}");
    }

    /// Generate a for-loop over a user-defined Iterator[T] with an else clause.
    fn gen_for_loop_iterator_with_else(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        else_block: &Block,
        flag: &str,
        emitter: &mut CEmitter,
    ) {
        let var_name = match &pattern.node {
            Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__gorget_i".to_string(),
        };
        let iter_expr = self.gen_expr(iterable);
        let type_name = self.infer_type_name_from_expr(iterable);
        let elem_c_type = self.get_iterator_elem_c_type(iterable);

        let option_mangled = self.register_generic("Option", &[elem_c_type.clone()], super::GenericInstanceKind::Enum);
        let tag_none = c_mangle::mangle_tag(&option_mangled, "None");
        let next_fn = c_mangle::mangle_trait_method("Iterator", &type_name, "next");
        let next_tmp = emitter.fresh_temp();

        emitter.emit_line("while (1) {");
        emitter.indent();
        emitter.emit_line(&format!("{option_mangled} {next_tmp} = {next_fn}(&{iter_expr});"));
        emitter.emit_line(&format!("if ({next_tmp}.tag == {tag_none}) break;"));
        emitter.emit_line(&format!("{elem_c_type} {var_name} = {next_tmp}.data.Some._0;"));
        self.gen_block_with_break_flag(body, flag, emitter);
        emitter.dedent();
        emitter.emit_line("}");
        emitter.emit_line(&format!("if (!{flag}) {{"));
        emitter.indent();
        self.gen_block(else_block, emitter);
        emitter.dedent();
        emitter.emit_line("}");
    }

    /// Generate a for loop over a range or iterable.
    fn gen_for_loop(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        emitter: &mut CEmitter,
    ) {
        let var_name = match &pattern.node {
            Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__gorget_i".to_string(),
        };

        // Check if iterable is a Range expression
        if let Expr::Range {
            start,
            end,
            inclusive,
        } = &iterable.node
        {
            let start_expr = start
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let end_expr = end
                .as_ref()
                .map(|e| self.gen_expr(e))
                .unwrap_or_else(|| "0".to_string());
            let cmp = if *inclusive { "<=" } else { "<" };
            emitter.emit_line(&format!(
                "for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{"
            ));
            emitter.indent();
            self.push_drop_scope(DropScopeKind::Loop);
            self.gen_block(body, emitter);
            self.pop_drop_scope(emitter);
            emitter.dedent();
            emitter.emit_line("}");
        } else if self.is_string_expr(iterable) {
            // For-in over string (char by char)
            let iter = self.gen_expr(iterable);
            let len_var = emitter.fresh_temp();
            let idx = emitter.fresh_temp();
            emitter.emit_line(&format!("size_t {len_var} = strlen({iter});"));
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < {len_var}; {idx}++) {{"
            ));
            emitter.indent();
            self.push_drop_scope(DropScopeKind::Loop);
            emitter.emit_line(&format!("char {var_name} = {iter}[{idx}];"));
            self.gen_block(body, emitter);
            self.pop_drop_scope(emitter);
            emitter.dedent();
            emitter.emit_line("}");
        } else if self.is_gorget_array_expr(iterable) {
            // For-in over GorgetArray
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();
            let elem_type = self.infer_vector_elem_type(iterable);

            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < gorget_array_len(&{iter}); {idx}++) {{"
            ));
            emitter.indent();
            self.push_drop_scope(DropScopeKind::Loop);
            emitter.emit_line(&format!(
                "{elem_type} {var_name} = GORGET_ARRAY_AT({elem_type}, {iter}, {idx});"
            ));
            self.gen_block(body, emitter);
            self.pop_drop_scope(emitter);
            emitter.dedent();
            emitter.emit_line("}");
        } else if self.is_gorget_map_expr(iterable) {
            self.gen_for_loop_dict(pattern, iterable, body, emitter);
        } else if self.is_gorget_set_expr(iterable) {
            self.gen_for_loop_set(pattern, iterable, body, emitter);
        } else if self.has_iterator_impl(iterable) {
            self.gen_for_loop_iterator(pattern, iterable, body, emitter);
        } else {
            // For-in over C array
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();

            // Use sizeof-based iteration for C arrays
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < sizeof({iter})/sizeof({iter}[0]); {idx}++) {{"
            ));
            emitter.indent();
            self.push_drop_scope(DropScopeKind::Loop);
            emitter.emit_line(&format!(
                "__typeof__({iter}[0]) {var_name} = {iter}[{idx}];"
            ));
            self.gen_block(body, emitter);
            self.pop_drop_scope(emitter);
            emitter.dedent();
            emitter.emit_line("}");
        }
    }

    /// Generate a for-loop over a Dict (GorgetMap).
    /// Supports both key-only (`for k in dict`) and key-value (`for (k, v) in dict`) patterns.
    fn gen_for_loop_dict(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        emitter: &mut CEmitter,
    ) {
        let iter = self.gen_expr(iterable);
        let idx = emitter.fresh_temp();
        let (key_type, val_type) = self.infer_map_kv_types(iterable);

        emitter.emit_line(&format!(
            "for (size_t {idx} = 0; {idx} < {iter}.cap; {idx}++) {{"
        ));
        emitter.indent();
        emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
        self.push_drop_scope(DropScopeKind::Loop);

        if let Pattern::Tuple(elems) = &pattern.node {
            // (k, v) pattern
            let k_name = match &elems[0].node {
                Pattern::Binding(name) => c_mangle::escape_keyword(name),
                _ => "__gorget_k".to_string(),
            };
            let v_name = if elems.len() >= 2 {
                match &elems[1].node {
                    Pattern::Binding(name) => c_mangle::escape_keyword(name),
                    _ => "__gorget_v".to_string(),
                }
            } else {
                "__gorget_v".to_string()
            };
            emitter.emit_line(&format!(
                "{key_type} {k_name} = {iter}.keys[{idx}];"
            ));
            emitter.emit_line(&format!(
                "{val_type} {v_name} = {iter}.values[{idx}];"
            ));
        } else {
            // key-only pattern
            let var_name = match &pattern.node {
                Pattern::Binding(name) => c_mangle::escape_keyword(name),
                _ => "__gorget_i".to_string(),
            };
            emitter.emit_line(&format!(
                "{key_type} {var_name} = {iter}.keys[{idx}];"
            ));
        }

        self.gen_block(body, emitter);
        self.pop_drop_scope(emitter);
        emitter.dedent();
        emitter.emit_line("}");
    }

    /// Generate a for-loop over a Set (GorgetSet, which is typedef'd to GorgetMap).
    fn gen_for_loop_set(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        emitter: &mut CEmitter,
    ) {
        let iter = self.gen_expr(iterable);
        let idx = emitter.fresh_temp();
        let elem_type = self.infer_set_elem_type(iterable);
        let var_name = match &pattern.node {
            Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__gorget_i".to_string(),
        };

        emitter.emit_line(&format!(
            "for (size_t {idx} = 0; {idx} < {iter}.cap; {idx}++) {{"
        ));
        emitter.indent();
        emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
        self.push_drop_scope(DropScopeKind::Loop);
        emitter.emit_line(&format!(
            "{elem_type} {var_name} = *({elem_type}*)((char*){iter}.keys + {idx} * {iter}.key_size);"
        ));
        self.gen_block(body, emitter);
        self.pop_drop_scope(emitter);
        emitter.dedent();
        emitter.emit_line("}");
    }

    /// Generate a match statement.
    fn gen_match_stmt(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[crate::parser::ast::MatchArm],
        else_arm: &Option<Block>,
        emitter: &mut CEmitter,
    ) {
        let scrut_expr = self.gen_expr(scrutinee);

        // Always evaluate scrutinee into a temp to avoid double-evaluation
        let tmp = emitter.fresh_temp();
        emitter.emit_line(&format!(
            "__typeof__({scrut_expr}) {tmp} = {scrut_expr};"
        ));

        // Resolve the mangled enum type name for generic enums (e.g. Option__int64_t)
        let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

        // Use if-else chain for all patterns (simpler, handles all cases)
        let mut first = true;
        for arm in arms {
            let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &tmp, enum_c_type.as_deref());

            // Apply guard expression if present.
            // When a guard references pattern bindings (e.g. `case n if n > 0`),
            // we must declare them before the guard is evaluated.  We wrap bindings
            // + guard in a GCC statement expression so they live in the condition.
            let full_cond = if let Some(guard) = &arm.guard {
                let guard_expr = self.gen_expr(guard);
                let bindings = self.stmt_pattern_bindings_inline(&arm.pattern.node, &tmp);
                if bindings.is_empty() {
                    format!("({pattern_cond}) && ({guard_expr})")
                } else {
                    format!("({pattern_cond}) && ({{ {bindings}({guard_expr}); }})")
                }
            } else {
                pattern_cond
            };

            if first {
                emitter.emit_line(&format!("if ({full_cond}) {{"));
                first = false;
            } else {
                emitter.emit_line(&format!("}} else if ({full_cond}) {{"));
            }
            emitter.indent();

            // Emit pattern bindings inside the arm body
            self.emit_pattern_bindings(&arm.pattern.node, &tmp, emitter);

            let body = self.gen_expr(&arm.body);
            emitter.emit_line(&format!("{body};"));
            emitter.dedent();
        }
        if let Some(else_body) = else_arm {
            emitter.emit_line("} else {");
            emitter.indent();
            self.gen_block(else_body, emitter);
            emitter.dedent();
        }
        if !arms.is_empty() || else_arm.is_some() {
            emitter.emit_line("}");
        }
    }

    /// Convert a pattern to a C boolean condition (for if-else chain).
    /// `enum_c_type` overrides the enum name used for tag constants (needed for generic enums
    /// where the monomorphized name like `Option__int64_t` differs from the raw name `Option`).
    fn pattern_to_condition(&mut self, pattern: &Pattern, scrutinee: &str, enum_c_type: Option<&str>) -> String {
        match pattern {
            Pattern::Literal(lit) if matches!(lit.node, Expr::NoneLiteral) => {
                // None literal as pattern: generate tag check for the None variant
                if let Some((enum_name, _)) = self.find_enum_for_variant_by_name("None") {
                    let effective = enum_c_type.unwrap_or(&enum_name);
                    let tag = c_mangle::mangle_tag(effective, "None");
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    let val = self.gen_expr(lit);
                    format!("{scrutinee} == {val}")
                }
            }
            Pattern::Literal(lit) => {
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Wildcard => "1".to_string(),
            Pattern::Binding(name) => {
                // A bare identifier may be a unit enum variant (parser can't distinguish).
                if let Some((enum_name, _)) = self.find_enum_for_variant_by_name(name) {
                    let effective = enum_c_type.unwrap_or(&enum_name);
                    let tag = c_mangle::mangle_tag(effective, name);
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    "1".to_string() // genuine binding — always matches
                }
            }
            Pattern::Constructor { path, fields: _ } => {
                // Enum variant destructuring: check the tag
                if let Some((enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    let effective = enum_c_type.unwrap_or(&enum_name);
                    let tag = c_mangle::mangle_tag(effective, &variant_name);
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    "1".to_string()
                }
            }
            Pattern::Or(alternatives) => {
                let conds: Vec<String> = alternatives
                    .iter()
                    .map(|p| self.pattern_to_condition(&p.node, scrutinee, enum_c_type))
                    .collect();
                format!("({})", conds.join(" || "))
            }
            Pattern::Tuple(_) => "1".to_string(), // tuple always matches structurally
            Pattern::Rest => "1".to_string(),
        }
    }

    /// Emit variable bindings from a pattern into the current scope.
    fn emit_pattern_bindings(&mut self, pattern: &Pattern, scrutinee: &str, emitter: &mut CEmitter) {
        match pattern {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                emitter.emit_line(&format!(
                    "__typeof__({scrutinee}) {escaped} = {scrutinee};"
                ));
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access =
                            format!("{scrutinee}.data.{variant_name}._{i}");
                        self.emit_pattern_bindings(&field_pat.node, &field_access, emitter);
                    }
                }
            }
            Pattern::Tuple(elements) => {
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    self.emit_pattern_bindings(&elem.node, &field_access, emitter);
                }
            }
            Pattern::Or(alternatives) => {
                // For or-patterns, bind from the first alternative (they must all bind the same names)
                if let Some(first) = alternatives.first() {
                    self.emit_pattern_bindings(&first.node, scrutinee, emitter);
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => {
                // No bindings to emit
            }
        }
    }

    /// Return inline pattern bindings as a string (for use in GCC statement expressions).
    /// Uses statement-context enum variant lookup (vs expression-context in c_expr.rs).
    fn stmt_pattern_bindings_inline(&mut self, pattern: &Pattern, scrutinee: &str) -> String {
        match pattern {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                format!("__typeof__({scrutinee}) {escaped} = {scrutinee}; ")
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    let mut result = String::new();
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access =
                            format!("{scrutinee}.data.{variant_name}._{i}");
                        result.push_str(&self.stmt_pattern_bindings_inline(&field_pat.node, &field_access));
                    }
                    result
                } else {
                    String::new()
                }
            }
            Pattern::Tuple(elements) => {
                let mut result = String::new();
                for (i, elem) in elements.iter().enumerate() {
                    let field_access = format!("{scrutinee}._{i}");
                    result.push_str(&self.stmt_pattern_bindings_inline(&elem.node, &field_access));
                }
                result
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.stmt_pattern_bindings_inline(&first.node, scrutinee)
                } else {
                    String::new()
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Rest => String::new(),
        }
    }

    /// Generate a for loop with optional else clause.
    fn gen_for_loop_with_else(
        &mut self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        else_body: &Option<Block>,
        emitter: &mut CEmitter,
    ) {
        if let Some(else_block) = else_body {
            let flag = emitter.fresh_temp();
            emitter.emit_line(&format!("bool {flag} = false;"));
            // Emit the for loop header manually so we can use the break-flag body
            let var_name = match &pattern.node {
                Pattern::Binding(name) => c_mangle::escape_keyword(name),
                _ => "__gorget_i".to_string(),
            };

            if let Expr::Range {
                start,
                end,
                inclusive,
            } = &iterable.node
            {
                let start_expr = start
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                let end_expr = end
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                let cmp = if *inclusive { "<=" } else { "<" };
                emitter.emit_line(&format!(
                    "for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{"
                ));
            } else if self.is_string_expr(iterable) {
                let iter = self.gen_expr(iterable);
                let len_var = emitter.fresh_temp();
                let idx = emitter.fresh_temp();
                emitter.emit_line(&format!("size_t {len_var} = strlen({iter});"));
                emitter.emit_line(&format!(
                    "for (size_t {idx} = 0; {idx} < {len_var}; {idx}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!("char {var_name} = {iter}[{idx}];"));
                self.gen_block_with_break_flag(body, &flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line(&format!("if (!{flag}) {{"));
                emitter.indent();
                self.gen_block(else_block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                return;
            } else if self.is_gorget_array_expr(iterable) {
                let iter = self.gen_expr(iterable);
                let idx = emitter.fresh_temp();
                let elem_type = self.infer_vector_elem_type(iterable);
                emitter.emit_line(&format!(
                    "for (size_t {idx} = 0; {idx} < gorget_array_len(&{iter}); {idx}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!(
                    "{elem_type} {var_name} = GORGET_ARRAY_AT({elem_type}, {iter}, {idx});"
                ));
                self.gen_block_with_break_flag(body, &flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line(&format!("if (!{flag}) {{"));
                emitter.indent();
                self.gen_block(else_block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                return;
            } else if self.is_gorget_map_expr(iterable) {
                let iter = self.gen_expr(iterable);
                let idx = emitter.fresh_temp();
                let (key_type, val_type) = self.infer_map_kv_types(iterable);
                emitter.emit_line(&format!(
                    "for (size_t {idx} = 0; {idx} < {iter}.cap; {idx}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
                if let Pattern::Tuple(elems) = &pattern.node {
                    let k_name = match &elems[0].node {
                        Pattern::Binding(name) => c_mangle::escape_keyword(name),
                        _ => "__gorget_k".to_string(),
                    };
                    let v_name = if elems.len() >= 2 {
                        match &elems[1].node {
                            Pattern::Binding(name) => c_mangle::escape_keyword(name),
                            _ => "__gorget_v".to_string(),
                        }
                    } else {
                        "__gorget_v".to_string()
                    };
                    emitter.emit_line(&format!(
                        "{key_type} {k_name} = {iter}.keys[{idx}];"
                    ));
                    emitter.emit_line(&format!(
                        "{val_type} {v_name} = {iter}.values[{idx}];"
                    ));
                } else {
                    emitter.emit_line(&format!(
                        "{key_type} {var_name} = {iter}.keys[{idx}];"
                    ));
                }
                self.gen_block_with_break_flag(body, &flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line(&format!("if (!{flag}) {{"));
                emitter.indent();
                self.gen_block(else_block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                return;
            } else if self.is_gorget_set_expr(iterable) {
                let iter = self.gen_expr(iterable);
                let idx = emitter.fresh_temp();
                let elem_type = self.infer_set_elem_type(iterable);
                emitter.emit_line(&format!(
                    "for (size_t {idx} = 0; {idx} < {iter}.cap; {idx}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!("if ({iter}.states[{idx}] != 1) continue;"));
                emitter.emit_line(&format!(
                    "{elem_type} {var_name} = *({elem_type}*)((char*){iter}.keys + {idx} * {iter}.key_size);"
                ));
                self.gen_block_with_break_flag(body, &flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line(&format!("if (!{flag}) {{"));
                emitter.indent();
                self.gen_block(else_block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                return;
            } else if self.has_iterator_impl(iterable) {
                self.gen_for_loop_iterator_with_else(pattern, iterable, body, else_block, &flag, emitter);
                return;
            } else {
                let iter = self.gen_expr(iterable);
                let idx = emitter.fresh_temp();
                emitter.emit_line(&format!(
                    "for (size_t {idx} = 0; {idx} < sizeof({iter})/sizeof({iter}[0]); {idx}++) {{"
                ));
                emitter.indent();
                emitter.emit_line(&format!(
                    "__typeof__({iter}[0]) {var_name} = {iter}[{idx}];"
                ));
                self.gen_block_with_break_flag(body, &flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                emitter.emit_line(&format!("if (!{flag}) {{"));
                emitter.indent();
                self.gen_block(else_block, emitter);
                emitter.dedent();
                emitter.emit_line("}");
                return;
            }
            emitter.indent();
            self.gen_block_with_break_flag(body, &flag, emitter);
            emitter.dedent();
            emitter.emit_line("}");
            emitter.emit_line(&format!("if (!{flag}) {{"));
            emitter.indent();
            self.gen_block(else_block, emitter);
            emitter.dedent();
            emitter.emit_line("}");
        } else {
            self.gen_for_loop(pattern, iterable, body, emitter);
        }
    }

    /// Generate a block where `break` statements set a flag before breaking.
    /// Recurses into `if`, `match`, `unsafe`, and `with` bodies to find
    /// nested breaks, but stops at inner loops (`for`, `while`, `loop`)
    /// since those breaks target the inner loop.
    fn gen_block_with_break_flag(&mut self, block: &Block, flag: &str, emitter: &mut CEmitter) {
        for stmt in &block.stmts {
            self.gen_stmt_with_break_flag(&stmt.node, flag, emitter);
        }
    }

    /// Emit a single statement, instrumenting any `break` with the flag.
    fn gen_stmt_with_break_flag(&mut self, stmt: &Stmt, flag: &str, emitter: &mut CEmitter) {
        match stmt {
            Stmt::Break(_) => {
                emitter.emit_line(&format!("{flag} = true;"));
                emitter.emit_line("break;");
            }
            // Recurse into if/elif/else — breaks inside target the outer loop
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                let cond = self.gen_expr(condition);
                emitter.emit_line(&format!("if ({cond}) {{"));
                emitter.indent();
                self.gen_block_with_break_flag(then_body, flag, emitter);
                emitter.dedent();

                for (elif_cond, elif_body) in elif_branches {
                    let ec = self.gen_expr(elif_cond);
                    emitter.emit_line(&format!("}} else if ({ec}) {{"));
                    emitter.indent();
                    self.gen_block_with_break_flag(elif_body, flag, emitter);
                    emitter.dedent();
                }

                if let Some(else_body) = else_body {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    self.gen_block_with_break_flag(else_body, flag, emitter);
                    emitter.dedent();
                }

                emitter.emit_line("}");
            }
            // Recurse into match arms
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                let scrut_expr = self.gen_expr(scrutinee);
                let tmp = emitter.fresh_temp();
                emitter.emit_line(&format!(
                    "__typeof__({scrut_expr}) {tmp} = {scrut_expr};"
                ));
                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);
                let mut first = true;
                for arm in arms {
                    let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &tmp, enum_c_type.as_deref());
                    let full_cond = if let Some(guard) = &arm.guard {
                        let guard_expr = self.gen_expr(guard);
                        format!("({pattern_cond}) && ({guard_expr})")
                    } else {
                        pattern_cond
                    };
                    if first {
                        emitter.emit_line(&format!("if ({full_cond}) {{"));
                        first = false;
                    } else {
                        emitter.emit_line(&format!("}} else if ({full_cond}) {{"));
                    }
                    emitter.indent();
                    self.emit_pattern_bindings(&arm.pattern.node, &tmp, emitter);
                    // If arm body is a block, recurse into it for break flag handling
                    if let Expr::Block(block) = &arm.body.node {
                        self.gen_block_with_break_flag(block, flag, emitter);
                    } else {
                        let body = self.gen_expr(&arm.body);
                        emitter.emit_line(&format!("{body};"));
                    }
                    emitter.dedent();
                }
                if let Some(else_body) = else_arm {
                    emitter.emit_line("} else {");
                    emitter.indent();
                    self.gen_block_with_break_flag(else_body, flag, emitter);
                    emitter.dedent();
                }
                if !arms.is_empty() || else_arm.is_some() {
                    emitter.emit_line("}");
                }
            }
            // Recurse into unsafe/with blocks
            Stmt::Unsafe { body } => {
                emitter.emit_line("/* unsafe */ {");
                emitter.indent();
                self.gen_block_with_break_flag(body, flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }
            Stmt::With { bindings, body } => {
                emitter.emit_line("{");
                emitter.indent();
                for binding in bindings {
                    let e = self.gen_expr(&binding.expr);
                    let name = c_mangle::escape_keyword(&binding.name.node);
                    emitter.emit_line(&format!("/* with */ void* {name} = (void*)({e});"));
                }
                self.gen_block_with_break_flag(body, flag, emitter);
                emitter.dedent();
                emitter.emit_line("}");
            }
            // Do NOT recurse into inner loops — break inside them
            // targets the inner loop, not our outer for/else or while/else.
            _ => self.gen_stmt(stmt, emitter),
        }
    }

    /// Look up which enum owns a variant given a path (e.g., ["Some"] or ["Color", "Red"]).
    fn find_enum_for_variant_path(&mut self, path: &[Spanned<String>]) -> Option<(String, String)> {
        let variant_name = if path.len() == 1 {
            &path[0].node
        } else if path.len() == 2 {
            // Path is [EnumName, VariantName]
            return Some((path[0].node.clone(), path[1].node.clone()));
        } else {
            return None;
        };

        // Search enum_variants to find which enum has this variant
        for (enum_def_id, info) in self.enum_variants {
            for (vname, _) in &info.variants {
                if vname == variant_name {
                    let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                    return Some((enum_name, variant_name.clone()));
                }
            }
        }
        None
    }

    /// Look up which enum owns a variant given a bare variant name.
    fn find_enum_for_variant_by_name(&mut self, variant_name: &str) -> Option<(String, String)> {
        for (enum_def_id, info) in self.enum_variants {
            for (vname, _) in &info.variants {
                if vname == variant_name {
                    let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                    return Some((enum_name, variant_name.to_string()));
                }
            }
        }
        None
    }
}

/// Convert a compound assignment operator to its C form.
fn compound_op_to_c(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add | BinaryOp::AddWrap => "+=",
        BinaryOp::Sub | BinaryOp::SubWrap => "-=",
        BinaryOp::Mul | BinaryOp::MulWrap => "*=",
        BinaryOp::Div => "/=",
        BinaryOp::Mod => "%=",
        _ => "/* ?? */=",
    }
}
