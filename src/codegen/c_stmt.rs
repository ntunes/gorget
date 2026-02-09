/// Statement codegen: convert Gorget statements to C statements.
use crate::parser::ast::{BinaryOp, Block, Expr, Pattern, Stmt, Type};
use crate::span::Spanned;

use super::c_emitter::CEmitter;
use super::c_mangle;
use super::c_types;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate C code for a block of statements.
    pub fn gen_block(&self, block: &Block, emitter: &mut CEmitter) {
        for stmt in &block.stmts {
            self.gen_stmt(&stmt.node, emitter);
        }
    }

    /// Generate C code for a single statement.
    pub fn gen_stmt(&self, stmt: &Stmt, emitter: &mut CEmitter) {
        match stmt {
            Stmt::VarDecl {
                is_const,
                type_,
                pattern,
                value,
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
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                let c_op = compound_op_to_c(*op);
                emitter.emit_line(&format!("{t} {c_op} {v};"));
            }

            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let e = self.gen_expr(expr);
                    emitter.emit_line(&format!("return {e};"));
                } else {
                    emitter.emit_line("return;");
                }
            }

            Stmt::Break(_) => {
                emitter.emit_line("break;");
            }

            Stmt::Continue => {
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
                    self.gen_block_with_break_flag(body, &flag, emitter);
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
                    self.gen_block(body, emitter);
                    emitter.dedent();
                    emitter.emit_line("}");
                }
            }

            Stmt::Loop { body } => {
                emitter.emit_line("while (1) {");
                emitter.indent();
                self.gen_block(body, emitter);
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
                // If expr is a string literal, use it directly; otherwise stringify
                if matches!(&expr.node, Expr::StringLiteral(_)) {
                    emitter.emit_line(&format!("GORGET_THROW({e}, 1);"));
                } else {
                    emitter.emit_line(&format!("GORGET_THROW(\"{e}\", 1);"));
                }
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

            Stmt::Item(_) => {
                // Nested items handled during top-level pass
            }
        }
    }

    /// Generate a variable declaration.
    fn gen_var_decl(
        &self,
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

                // Special handling for array literals: emit as C array declarations
                if let Expr::ArrayLiteral(elements) = &value.node {
                    let elem_type = match &type_.node {
                        Type::Inferred => {
                            if let Some(first) = elements.first() {
                                self.infer_c_type_from_expr(&first.node)
                            } else {
                                "int64_t".to_string()
                            }
                        }
                        _ => c_types::ast_type_to_c(&type_.node, self.scopes),
                    };
                    let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                    emitter.emit_line(&format!(
                        "{const_prefix}{elem_type} {escaped}[] = {{{}}};",
                        elems.join(", ")
                    ));
                    return;
                }

                // Special handling for tuple literals: emit as struct
                if let Expr::TupleLiteral(elements) = &value.node {
                    let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                    // Use anonymous struct for tuple
                    let mut fields = Vec::new();
                    for (i, elem) in elements.iter().enumerate() {
                        let t = self.infer_c_type_from_expr(&elem.node);
                        fields.push(format!("{t} _{i}"));
                    }
                    emitter.emit_line(&format!(
                        "{const_prefix}struct {{ {}; }} {escaped} = {{{}}};",
                        fields.join("; "),
                        elems.join(", ")
                    ));
                    return;
                }

                let c_type = self.resolve_decl_type(type_, value, Some(name));
                let val = self.gen_expr(value);
                let decl = c_types::c_declare(&c_type, &escaped);
                emitter.emit_line(&format!("{const_prefix}{decl} = {val};"));
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
        &self,
        type_: &Spanned<Type>,
        value: &Spanned<Expr>,
        var_name: Option<&str>,
    ) -> String {
        match &type_.node {
            Type::Inferred => {
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
    pub(super) fn infer_c_type_from_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::IntLiteral(_) => "int64_t".to_string(),
            Expr::FloatLiteral(_) => "double".to_string(),
            Expr::BoolLiteral(_) => "bool".to_string(),
            Expr::CharLiteral(_) => "char".to_string(),
            Expr::StringLiteral(_) => "const char*".to_string(),
            Expr::BinaryOp { left, .. } => self.infer_c_type_from_expr(&left.node),
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
            Expr::StructLiteral { name, .. } => name.node.clone(),
            _ => "int64_t".to_string(),
        }
    }

    /// Detect trait object construction pattern:
    /// - Declared type is `dynamic Trait` or `Box[dynamic Trait]`
    /// - Value is `Box.new(ConcreteType(...))`
    /// Returns (trait_name, concrete_type, inner_expr) if matched.
    fn extract_trait_object_construction<'b>(
        &self,
        type_: &Spanned<Type>,
        value: &'b Spanned<Expr>,
    ) -> Option<(String, String, &'b Spanned<Expr>)> {
        // Check if declared type is dynamic Trait or Box[dynamic Trait]
        let trait_name = match &type_.node {
            Type::Dynamic { trait_ } => {
                if let Type::Named { name, .. } = &trait_.node {
                    Some(name.node.clone())
                } else {
                    None
                }
            }
            Type::Named { name, generic_args } if name.node == "Box" && generic_args.len() == 1 => {
                if let Type::Dynamic { trait_ } = &generic_args[0].node {
                    if let Type::Named { name: trait_name, .. } = &trait_.node {
                        Some(trait_name.node.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }?;

        // Check if value is Box.new(ConcreteType(...)) or Box.new(expr)
        if let Expr::Call { callee, args, .. } = &value.node {
            if let Expr::Path { segments } = &callee.node {
                if segments.len() == 2
                    && segments[0].node == "Box"
                    && segments[1].node == "new"
                {
                    if let Some(arg) = args.first() {
                        let inner_expr = &arg.node.value;
                        // Try to extract the concrete type from the inner expression
                        let concrete_type = match &inner_expr.node {
                            Expr::Call { callee: inner_callee, .. } => {
                                if let Expr::Identifier(name) = &inner_callee.node {
                                    name.clone()
                                } else {
                                    return None;
                                }
                            }
                            Expr::StructLiteral { name, .. } => name.node.clone(),
                            _ => return None,
                        };
                        return Some((trait_name, concrete_type, inner_expr));
                    }
                }
            }
        }
        None
    }

    /// Check if an iterable expression resolves to a GorgetArray type.
    fn is_gorget_array_expr(&self, expr: &Spanned<Expr>) -> bool {
        if let Expr::Identifier(name) = &expr.node {
            if let Some(def_id) = self.scopes.lookup(name) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    let c_type = c_types::type_id_to_c(type_id, self.types, self.scopes);
                    return c_type == "GorgetArray";
                }
            }
        }
        // Also check inferred type as fallback
        let c_type = self.infer_c_type_from_expr(&expr.node);
        c_type == "GorgetArray"
    }

    /// Infer the element C type for a GorgetArray expression.
    fn infer_array_elem_type(&self, _expr: &Spanned<Expr>) -> String {
        // Default to int64_t; more refined inference would need generic type args
        "int64_t".to_string()
    }

    /// Generate a for loop over a range or iterable.
    fn gen_for_loop(
        &self,
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
            self.gen_block(body, emitter);
            emitter.dedent();
            emitter.emit_line("}");
        } else if self.is_gorget_array_expr(iterable) {
            // For-in over GorgetArray
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();
            let elem_type = self.infer_array_elem_type(iterable);

            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < gorget_array_len(&{iter}); {idx}++) {{"
            ));
            emitter.indent();
            emitter.emit_line(&format!(
                "{elem_type} {var_name} = GORGET_ARRAY_AT({elem_type}, {iter}, {idx});"
            ));
            self.gen_block(body, emitter);
            emitter.dedent();
            emitter.emit_line("}");
        } else {
            // For-in over C array
            let iter = self.gen_expr(iterable);
            let idx = emitter.fresh_temp();

            // Use sizeof-based iteration for C arrays
            emitter.emit_line(&format!(
                "for (size_t {idx} = 0; {idx} < sizeof({iter})/sizeof({iter}[0]); {idx}++) {{"
            ));
            emitter.indent();
            emitter.emit_line(&format!(
                "__typeof__({iter}[0]) {var_name} = {iter}[{idx}];"
            ));
            self.gen_block(body, emitter);
            emitter.dedent();
            emitter.emit_line("}");
        }
    }

    /// Generate a match statement.
    fn gen_match_stmt(
        &self,
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

        // Use if-else chain for all patterns (simpler, handles all cases)
        let mut first = true;
        for arm in arms {
            let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &tmp);

            // Apply guard expression if present
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
    fn pattern_to_condition(&self, pattern: &Pattern, scrutinee: &str) -> String {
        match pattern {
            Pattern::Literal(lit) => {
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Wildcard => "1".to_string(),
            Pattern::Binding(_) => "1".to_string(), // always matches
            Pattern::Constructor { path, fields: _ } => {
                // Enum variant destructuring: check the tag
                if let Some((enum_name, variant_name)) = self.find_enum_for_variant_path(path) {
                    let tag = c_mangle::mangle_tag(&enum_name, &variant_name);
                    format!("{scrutinee}.tag == {tag}")
                } else {
                    "1".to_string()
                }
            }
            Pattern::Or(alternatives) => {
                let conds: Vec<String> = alternatives
                    .iter()
                    .map(|p| self.pattern_to_condition(&p.node, scrutinee))
                    .collect();
                format!("({})", conds.join(" || "))
            }
            Pattern::Tuple(_) => "1".to_string(), // tuple always matches structurally
            Pattern::Rest => "1".to_string(),
        }
    }

    /// Emit variable bindings from a pattern into the current scope.
    fn emit_pattern_bindings(&self, pattern: &Pattern, scrutinee: &str, emitter: &mut CEmitter) {
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

    /// Generate a for loop with optional else clause.
    fn gen_for_loop_with_else(
        &self,
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
            } else if self.is_gorget_array_expr(iterable) {
                let iter = self.gen_expr(iterable);
                let idx = emitter.fresh_temp();
                let elem_type = self.infer_array_elem_type(iterable);
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
    fn gen_block_with_break_flag(&self, block: &Block, flag: &str, emitter: &mut CEmitter) {
        for stmt in &block.stmts {
            self.gen_stmt_with_break_flag(&stmt.node, flag, emitter);
        }
    }

    /// Emit a single statement, instrumenting any `break` with the flag.
    fn gen_stmt_with_break_flag(&self, stmt: &Stmt, flag: &str, emitter: &mut CEmitter) {
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
                let mut first = true;
                for arm in arms {
                    let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &tmp);
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
    fn find_enum_for_variant_path(&self, path: &[Spanned<String>]) -> Option<(String, String)> {
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
}

/// Convert a compound assignment operator to its C form.
fn compound_op_to_c(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+=",
        BinaryOp::Sub => "-=",
        BinaryOp::Mul => "*=",
        BinaryOp::Div => "/=",
        BinaryOp::Mod => "%=",
        _ => "/* ?? */=",
    }
}
