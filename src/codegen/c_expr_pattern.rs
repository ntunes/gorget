/// Pattern matching, comprehension, and block expression codegen.
use crate::parser::ast::{BinaryOp, Expr};
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::c_expr::binary_op_to_c;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate a list comprehension as a GCC statement expression.
    pub(super) fn gen_list_comprehension(
        &mut self,
        expr: &Spanned<Expr>,
        variable: &Spanned<crate::parser::ast::Pattern>,
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let elem_expr = self.gen_expr(expr);
        let elem_type = self.infer_c_type_from_expr(&expr.node);

        let var_name = match &variable.node {
            crate::parser::ast::Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__gorget_v".to_string(),
        };

        // Check if iterable is a range
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

            let cond_guard = condition
                .map(|c| format!("if ({}) ", self.gen_expr(c)))
                .unwrap_or_default();

            return format!(
                "({{ GorgetArray __comp = gorget_array_new(sizeof({elem_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_array_push(&__comp, &__elem); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ GorgetArray __comp = gorget_array_new(sizeof({elem_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            {elem_type} {var_name} = {iter}[__i]; \
            {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_array_push(&__comp, &__elem); }} \
            }} __comp; }})"
        )
    }

    /// Generate a set comprehension as a GCC statement expression.
    pub(super) fn gen_set_comprehension(
        &mut self,
        expr: &Spanned<Expr>,
        variable: &Spanned<String>,
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let elem_expr = self.gen_expr(expr);
        let elem_type = self.infer_c_type_from_expr(&expr.node);
        let var_name = c_mangle::escape_keyword(&variable.node);

        // Check if iterable is a range
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

            let cond_guard = condition
                .map(|c| format!("if ({}) ", self.gen_expr(c)))
                .unwrap_or_default();

            return format!(
                "({{ GorgetSet __comp = gorget_set_new(sizeof({elem_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_set_add(&__comp, &__elem); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ GorgetSet __comp = gorget_set_new(sizeof({elem_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            {elem_type} {var_name} = {iter}[__i]; \
            {cond_guard}{{ {elem_type} __elem = {elem_expr}; gorget_set_add(&__comp, &__elem); }} \
            }} __comp; }})"
        )
    }

    /// Generate a dict comprehension as a GCC statement expression.
    pub(super) fn gen_dict_comprehension(
        &mut self,
        key: &Spanned<Expr>,
        value: &Spanned<Expr>,
        variables: &[Spanned<String>],
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let key_expr = self.gen_expr(key);
        let val_expr = self.gen_expr(value);
        let key_type = self.infer_c_type_from_expr(&key.node);
        let val_type = self.infer_c_type_from_expr(&value.node);
        let mangled = c_mangle::mangle_generic("GorgetDict", &[key_type.clone(), val_type.clone()]);
        self.register_generic("GorgetDict", &[key_type.clone(), val_type.clone()], super::GenericInstanceKind::Map { ordered: true });
        let var_name = variables
            .first()
            .map(|v| c_mangle::escape_keyword(&v.node))
            .unwrap_or_else(|| "__gorget_v".to_string());

        // Check if iterable is a range
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

            let cond_guard = condition
                .map(|c| format!("if ({}) ", self.gen_expr(c)))
                .unwrap_or_default();

            return format!(
                "({{ {mangled} __comp = {mangled}__new(); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
                {mangled}__put(&__comp, __k, __v); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ {mangled} __comp = {mangled}__new(); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            __typeof__({iter}[0]) {var_name} = {iter}[__i]; \
            {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
            {mangled}__put(&__comp, __k, __v); }} \
            }} __comp; }})"
        )
    }

    /// Convert a pattern to a C boolean condition for `is` expressions.
    /// `enum_c_type` overrides the enum name used for tag constants (needed for generic enums
    /// where the monomorphized name like `Option__int64_t` differs from the raw name `Option`).
    pub(super) fn pattern_to_condition_expr(&mut self, pattern: &crate::parser::ast::Pattern, scrutinee: &str, enum_c_type: Option<&str>) -> String {
        use crate::parser::ast::Pattern;
        match pattern {
            Pattern::Literal(lit) if matches!(lit.node, Expr::NoneLiteral) => {
                // None literal as pattern: generate tag check for the None variant
                for (enum_def_id, info) in self.enum_variants {
                    for (vname, _) in &info.variants {
                        if vname == "None" {
                            let effective = enum_c_type.unwrap_or(&self.scopes.get_def(*enum_def_id).name);
                            let tag = c_mangle::mangle_tag(effective, "None");
                            return format!("{scrutinee}.tag == {tag}");
                        }
                    }
                }
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Literal(lit) => {
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Wildcard => "1".to_string(),
            Pattern::Binding(name) => {
                // A bare identifier may be a unit enum variant (parser can't distinguish).
                // Check if the name matches a known variant; if so, generate a tag check.
                for (enum_def_id, info) in self.enum_variants {
                    for (vname, _) in &info.variants {
                        if vname == name {
                            let effective = enum_c_type.unwrap_or(&self.scopes.get_def(*enum_def_id).name);
                            let tag = c_mangle::mangle_tag(effective, name);
                            return format!("{scrutinee}.tag == {tag}");
                        }
                    }
                }
                "1".to_string()
            }
            Pattern::Constructor { path, .. } => {
                if path.len() == 2 {
                    // Explicit path like Shape.Circle — use enum_c_type override if available
                    let effective = enum_c_type.unwrap_or(&path[0].node);
                    let tag = c_mangle::mangle_tag(effective, &path[1].node);
                    format!("{scrutinee}.tag == {tag}")
                } else if path.len() == 1 {
                    // Try to find the enum for this variant
                    let variant_name = &path[0].node;
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, _) in &info.variants {
                            if vname == variant_name {
                                let effective = enum_c_type.unwrap_or(&self.scopes.get_def(*enum_def_id).name);
                                let tag = c_mangle::mangle_tag(effective, variant_name);
                                return format!("{scrutinee}.tag == {tag}");
                            }
                        }
                    }
                    "1".to_string()
                } else {
                    "1".to_string()
                }
            }
            Pattern::Or(alternatives) => {
                let conds: Vec<String> = alternatives
                    .iter()
                    .map(|p| self.pattern_to_condition_expr(&p.node, scrutinee, enum_c_type))
                    .collect();
                format!("({})", conds.join(" || "))
            }
            Pattern::Tuple(_) | Pattern::Rest => "1".to_string(),
        }
    }

    /// Generate a match expression as a GCC statement expression.
    pub(super) fn gen_match_expr(
        &mut self,
        scrutinee: &Spanned<Expr>,
        arms: &[crate::parser::ast::MatchArm],
        else_arm: Option<&Spanned<Expr>>,
    ) -> String {
        let scrut_expr = self.gen_expr(scrutinee);
        let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

        let mut parts = Vec::new();
        // When the scrutinee is `self` inside a method body, `self` is a pointer
        // (const T *self), so we must dereference it to get the struct value.
        let is_self_scrutinee = self.current_self_type.is_some() && matches!(scrutinee.node, Expr::SelfExpr);
        if is_self_scrutinee {
            parts.push(format!("__typeof__(*{scrut_expr}) __gorget_scrut = *{scrut_expr}"));
        } else {
            parts.push(format!("__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}"));
        }

        // Build if-else chain; each arm assigns to __gorget_match_result
        let mut arm_parts = Vec::new();
        let mut first_body: Option<String> = None;
        let mut first = true;
        for arm in arms {
            let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut", enum_c_type.as_deref());
            let full_cond = if let Some(guard) = &arm.guard {
                let guard_expr = self.gen_expr(guard);
                let guard_bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
                if guard_bindings.is_empty() {
                    format!("({cond}) && ({guard_expr})")
                } else {
                    format!("({cond}) && ({{ {guard_bindings}({guard_expr}); }})")
                }
            } else {
                cond
            };

            let bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
            let body = self.gen_expr(&arm.body);

            if first {
                first_body = Some(body.clone());
                arm_parts.push(format!("if ({full_cond}) {{ {bindings}__gorget_match_result = {body}; }}"));
                first = false;
            } else {
                arm_parts.push(format!("else if ({full_cond}) {{ {bindings}__gorget_match_result = {body}; }}"));
            }
        }

        if let Some(else_expr) = else_arm {
            let else_body = self.gen_expr(else_expr);
            arm_parts.push(format!("else {{ __gorget_match_result = {else_body}; }}"));
        }

        // Determine a result type from the first arm body (reuse already-generated body)
        let result_type = if let Some(body) = &first_body {
            format!("__typeof__(({}))", body)
        } else {
            "int64_t".to_string()
        };

        let chain = arm_parts.join(" ");
        format!(
            "({{ {result_type} __gorget_match_result; {} {chain} __gorget_match_result; }})",
            parts.join("; ") + ";"
        )
    }

    /// Generate inline variable bindings for a pattern (returns C statements as a string).
    fn pattern_bindings_inline(&mut self, pattern: &crate::parser::ast::Pattern, scrutinee: &str) -> String {
        use crate::parser::ast::Pattern;
        match pattern {
            Pattern::Binding(name) => {
                let escaped = c_mangle::escape_keyword(name);
                format!("__typeof__({scrutinee}) {escaped} = {scrutinee}; ")
            }
            Pattern::Constructor { path, fields } => {
                if let Some((_enum_name, variant_name)) = self.find_enum_for_variant_path_expr(path) {
                    let mut result = String::new();
                    for (i, field_pat) in fields.iter().enumerate() {
                        let field_access = format!("{scrutinee}.data.{variant_name}._{i}");
                        result.push_str(&self.pattern_bindings_inline(&field_pat.node, &field_access));
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
                    result.push_str(&self.pattern_bindings_inline(&elem.node, &field_access));
                }
                result
            }
            Pattern::Or(alternatives) => {
                if let Some(first) = alternatives.first() {
                    self.pattern_bindings_inline(&first.node, scrutinee)
                } else {
                    String::new()
                }
            }
            _ => String::new(),
        }
    }

    /// Look up which enum owns a variant given a path (for expression context).
    fn find_enum_for_variant_path_expr(&mut self, path: &[Spanned<String>]) -> Option<(String, String)> {
        if path.len() == 2 {
            return Some((path[0].node.clone(), path[1].node.clone()));
        }
        let variant_name = if path.len() == 1 {
            &path[0].node
        } else {
            return None;
        };
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

    /// Generate a block/do expression as a GCC statement expression.
    pub(super) fn gen_block_expr(&mut self, block: &crate::parser::ast::Block) -> String {
        if block.stmts.is_empty() {
            return "(void)0".to_string();
        }

        let mut parts = Vec::new();
        let last_idx = block.stmts.len() - 1;

        for (i, stmt) in block.stmts.iter().enumerate() {
            if i == last_idx {
                // Last statement: if it's an expression statement, use it as the result value
                if let crate::parser::ast::Stmt::Expr(expr) = &stmt.node {
                    let val = self.gen_expr(expr);
                    parts.push(format!("{val};"));
                } else if let Some(inline) = self.gen_value_producing_stmt(&stmt.node) {
                    // Stmt::If / Stmt::Match with value-producing branches
                    parts.push(inline);
                } else {
                    // Non-expression final statement: emit as statement, result is (void)0
                    parts.push(self.stmt_to_inline_string(&stmt.node));
                    parts.push("(void)0;".to_string());
                }
            } else {
                parts.push(self.stmt_to_inline_string(&stmt.node));
            }
        }

        format!("({{ {} }})", parts.join(" "))
    }

    /// Try to generate a statement as a value-producing GCC statement expression.
    /// Returns `Some(code)` for `Stmt::If` and `Stmt::Match` whose branches end
    /// with an expression statement, `None` otherwise.
    fn gen_value_producing_stmt(&mut self, stmt: &crate::parser::ast::Stmt) -> Option<String> {
        use crate::parser::ast::Stmt;
        match stmt {
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                // Need at least an else branch to produce a value in all paths
                let else_body = else_body.as_ref()?;
                // Each branch must end with an expression statement
                let then_val = self.block_tail_expr(then_body)?;
                let else_val = self.block_tail_expr(else_body)?;

                let cond = self.gen_expr(condition);
                let then_code = self.gen_expr(&then_val);
                let result_type = format!("__typeof__(({}))  ", then_code);

                let mut code = format!("{result_type} __gorget_do_result; ");
                code.push_str(&format!("if ({cond}) {{ "));
                code.push_str(&self.block_stmts_except_last_inline(then_body));
                code.push_str(&format!("__gorget_do_result = {then_code}; }} "));

                for (elif_cond, elif_body) in elif_branches {
                    let elif_val = self.block_tail_expr(elif_body)?;
                    let ec = self.gen_expr(elif_cond);
                    let elif_code = self.gen_expr(&elif_val);
                    code.push_str(&format!("else if ({ec}) {{ "));
                    code.push_str(&self.block_stmts_except_last_inline(elif_body));
                    code.push_str(&format!("__gorget_do_result = {elif_code}; }} "));
                }

                let else_code = self.gen_expr(&else_val);
                code.push_str("else { ");
                code.push_str(&self.block_stmts_except_last_inline(else_body));
                code.push_str(&format!("__gorget_do_result = {else_code}; }} "));
                code.push_str("__gorget_do_result;");

                Some(code)
            }
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                // Need at least one arm to produce a value
                let first_arm = arms.first()?;
                let first_body = self.gen_expr(&first_arm.body);
                let result_type = format!("__typeof__(({}))  ", first_body);

                let scrut_expr = self.gen_expr(scrutinee);
                let enum_c_type = self.resolve_enum_c_type_for_scrutinee(scrutinee);

                let mut code = format!(
                    "__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}; \
                     {result_type} __gorget_do_result; "
                );

                let mut first = true;
                for arm in arms {
                    let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut", enum_c_type.as_deref());
                    let full_cond = if let Some(guard) = &arm.guard {
                        let guard_expr = self.gen_expr(guard);
                        format!("({cond}) && ({guard_expr})")
                    } else {
                        cond
                    };
                    let bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
                    let body = self.gen_expr(&arm.body);

                    if first {
                        code.push_str(&format!("if ({full_cond}) {{ {bindings}__gorget_do_result = {body}; }} "));
                        first = false;
                    } else {
                        code.push_str(&format!("else if ({full_cond}) {{ {bindings}__gorget_do_result = {body}; }} "));
                    }
                }

                if let Some(else_body) = else_arm {
                    let else_val = self.block_tail_expr(else_body)?;
                    let else_code = self.gen_expr(else_val);
                    code.push_str("else { ");
                    code.push_str(&self.block_stmts_except_last_inline(else_body));
                    code.push_str(&format!("__gorget_do_result = {else_code}; }} "));
                }

                code.push_str("__gorget_do_result;");
                Some(code)
            }
            _ => None,
        }
    }

    /// Extract the tail expression from a block (last stmt must be Stmt::Expr).
    fn block_tail_expr<'b>(&mut self, block: &'b crate::parser::ast::Block) -> Option<&'b Spanned<Expr>> {
        if let Some(last) = block.stmts.last() {
            if let crate::parser::ast::Stmt::Expr(expr) = &last.node {
                return Some(expr);
            }
        }
        None
    }

    /// Generate all statements in a block except the last one, as inline C code.
    fn block_stmts_except_last_inline(&mut self, block: &crate::parser::ast::Block) -> String {
        if block.stmts.len() <= 1 {
            return String::new();
        }
        let mut result = String::new();
        for stmt in &block.stmts[..block.stmts.len() - 1] {
            result.push_str(&self.stmt_to_inline_string(&stmt.node));
            result.push(' ');
        }
        result
    }

    /// Generate inline C code for a statement (for use inside GCC statement expressions).
    pub(super) fn stmt_to_inline_string(&mut self, stmt: &crate::parser::ast::Stmt) -> String {
        use crate::parser::ast::Stmt;
        match stmt {
            Stmt::Expr(expr) => {
                let e = self.gen_expr(expr);
                format!("{e};")
            }
            Stmt::VarDecl {
                is_const,
                type_,
                pattern,
                value,
                ..
            } => {
                let const_prefix = if *is_const { "const " } else { "" };
                match &pattern.node {
                    crate::parser::ast::Pattern::Binding(name) => {
                        let escaped = c_mangle::escape_keyword(name);
                        let c_type = match &type_.node {
                            crate::parser::ast::Type::Inferred => self.infer_c_type_from_expr(&value.node),
                            _ => self.type_to_c(&type_.node),
                        };
                        let val = self.gen_expr(value);
                        let decl = c_types::c_declare(&c_type, &escaped);
                        format!("{const_prefix}{decl} = {val};")
                    }
                    _ => {
                        let val = self.gen_expr(value);
                        format!("/* pattern decl */ (void){val};")
                    }
                }
            }
            Stmt::Assign { target, value } => {
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                format!("{t} = {v};")
            }
            Stmt::CompoundAssign { target, op, value } => {
                let t = self.gen_expr(target);
                let v = self.gen_expr(value);
                if matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul) && !self.overflow_wrap {
                    let macro_name = match op {
                        BinaryOp::Add => "GORGET_CHECKED_ADD_ASSIGN",
                        BinaryOp::Sub => "GORGET_CHECKED_SUB_ASSIGN",
                        BinaryOp::Mul => "GORGET_CHECKED_MUL_ASSIGN",
                        _ => unreachable!(),
                    };
                    format!("{macro_name}({t}, {v});")
                } else {
                    let c_op = binary_op_to_c(*op);
                    format!("{t} {c_op}= {v};")
                }
            }
            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    format!("return {};", self.gen_expr(e))
                } else {
                    "return;".to_string()
                }
            }
            _ => "/* stmt */ (void)0;".to_string(),
        }
    }
}
