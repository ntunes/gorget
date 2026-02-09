/// Statement codegen: convert Vyper statements to C statements.
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
                ..
            } => {
                let cond = self.gen_expr(condition);
                emitter.emit_line(&format!("while ({cond}) {{"));
                emitter.indent();
                self.gen_block(body, emitter);
                emitter.dedent();
                emitter.emit_line("}");
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
                ..
            } => {
                self.gen_for_loop(pattern, iterable, body, emitter);
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
                emitter.emit_line(&format!(
                    "fprintf(stderr, \"throw: %s\\n\", \"{e}\"); exit(1);"
                ));
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

                let c_type = self.resolve_decl_type(type_, value);
                let val = self.gen_expr(value);
                emitter.emit_line(&format!("{const_prefix}{c_type} {escaped} = {val};"));
            }
            Pattern::Wildcard => {
                let val = self.gen_expr(value);
                emitter.emit_line(&format!("(void){val};"));
            }
            _ => {
                let c_type = self.resolve_decl_type(type_, value);
                let val = self.gen_expr(value);
                emitter.emit_line(&format!("/* pattern decl */ {c_type} __pat = {val};"));
            }
        }
    }

    /// Resolve the C type for a declaration, handling `auto` (Inferred).
    fn resolve_decl_type(&self, type_: &Spanned<Type>, value: &Spanned<Expr>) -> String {
        match &type_.node {
            Type::Inferred => {
                // Infer type from value expression
                self.infer_c_type_from_expr(&value.node)
            }
            _ => c_types::ast_type_to_c(&type_.node, self.scopes),
        }
    }

    /// Best-effort C type inference from a value expression.
    fn infer_c_type_from_expr(&self, expr: &Expr) -> String {
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
                    if let Some(def_id) = self.scopes.lookup(name) {
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
                if let Some(def_id) = self.scopes.lookup(name) {
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

    /// Generate a for loop over a range.
    fn gen_for_loop(
        &self,
        pattern: &Spanned<Pattern>,
        iterable: &Spanned<Expr>,
        body: &Block,
        emitter: &mut CEmitter,
    ) {
        let var_name = match &pattern.node {
            Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__vyper_i".to_string(),
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
        } else {
            // Generic for-in (not fully supported, emit a TODO)
            let iter = self.gen_expr(iterable);
            emitter.emit_line(&format!(
                "/* for {var_name} in {iter} - not yet supported */ {{"
            ));
            emitter.indent();
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
        let scrut = self.gen_expr(scrutinee);

        // Check if all patterns are integer literals → use switch
        let all_int_literals = arms.iter().all(|arm| {
            matches!(
                &arm.pattern.node,
                Pattern::Literal(lit) if matches!(lit.node, Expr::IntLiteral(_))
            )
        });

        if all_int_literals {
            emitter.emit_line(&format!("switch ({scrut}) {{"));
            emitter.indent();
            for arm in arms {
                if let Pattern::Literal(lit) = &arm.pattern.node {
                    if let Expr::IntLiteral(n) = &lit.node {
                        emitter.emit_line(&format!("case {n}: {{"));
                        emitter.indent();
                        // arm.body is a Spanned<Expr> — emit as expression statement
                        let body = self.gen_expr(&arm.body);
                        emitter.emit_line(&format!("{body};"));
                        emitter.emit_line("break;");
                        emitter.dedent();
                        emitter.emit_line("}");
                    }
                }
            }
            if let Some(else_body) = else_arm {
                emitter.emit_line("default: {");
                emitter.indent();
                self.gen_block(else_body, emitter);
                emitter.emit_line("break;");
                emitter.dedent();
                emitter.emit_line("}");
            }
            emitter.dedent();
            emitter.emit_line("}");
        } else {
            // Use if-else chain
            let mut first = true;
            for arm in arms {
                let pattern_cond = self.pattern_to_condition(&arm.pattern.node, &scrut);
                if first {
                    emitter.emit_line(&format!("if ({pattern_cond}) {{"));
                    first = false;
                } else {
                    emitter.emit_line(&format!("}} else if ({pattern_cond}) {{"));
                }
                emitter.indent();
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
            _ => "1".to_string(),
        }
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
