/// Expression codegen: convert Vyper expressions to C expression strings.
use crate::lexer::token::{StringLit, StringSegment};
use crate::parser::ast::{BinaryOp, Expr, PrimitiveType, UnaryOp};
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate a C expression string from a Vyper expression.
    pub fn gen_expr(&self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::IntLiteral(n) => format!("INT64_C({n})"),
            Expr::FloatLiteral(f) => format!("{f}"),
            Expr::BoolLiteral(b) => if *b { "true" } else { "false" }.to_string(),
            Expr::CharLiteral(c) => format!("'{}'", escape_char(*c)),
            Expr::StringLiteral(s) => self.gen_string_literal(s),
            Expr::NoneLiteral => "NULL".to_string(),

            Expr::Identifier(name) => c_mangle::escape_keyword(name),

            Expr::SelfExpr => "self".to_string(),

            Expr::Path { segments } => {
                // For now, join with __ (e.g., Color.Red → Color__Red)
                if segments.len() == 2 {
                    c_mangle::mangle_variant(&segments[0].node, &segments[1].node)
                } else {
                    segments
                        .iter()
                        .map(|s| s.node.as_str())
                        .collect::<Vec<_>>()
                        .join("__")
                }
            }

            Expr::UnaryOp { op, operand } => {
                let inner = self.gen_expr(operand);
                match op {
                    UnaryOp::Neg => format!("(-{inner})"),
                    UnaryOp::Not => format!("(!{inner})"),
                    UnaryOp::Deref => format!("(*{inner})"),
                }
            }

            Expr::BinaryOp { left, op, right } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let c_op = binary_op_to_c(*op);
                format!("({l} {c_op} {r})")
            }

            Expr::Call { callee, args, .. } => self.gen_call(callee, args),

            Expr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => self.gen_method_call(receiver, &method.node, args),

            Expr::FieldAccess { object, field } => {
                let obj = self.gen_expr(object);
                let field_name = c_mangle::escape_keyword(&field.node);
                if self.current_self_type.is_some() && matches!(object.node, Expr::SelfExpr) {
                    format!("self->{field_name}")
                } else {
                    format!("{obj}.{field_name}")
                }
            }

            Expr::TupleFieldAccess { object, index } => {
                let obj = self.gen_expr(object);
                format!("{obj}._{index}")
            }

            Expr::Index { object, index } => {
                let obj = self.gen_expr(object);
                let idx = self.gen_expr(index);
                format!("{obj}[{idx}]")
            }

            Expr::Range {
                start,
                end,
                inclusive,
            } => {
                // Ranges are handled specially in for-loop codegen; standalone range is not valid C.
                let s = start
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                let e = end
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                format!(
                    "/* range {s}..{}{e} */",
                    if *inclusive { "=" } else { "" }
                )
            }

            Expr::StructLiteral { name, args } => {
                let field_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(a)).collect();
                let fields_str = field_exprs.join(", ");
                let type_name = &name.node;
                format!("({type_name}){{{fields_str}}}")
            }

            Expr::ArrayLiteral(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                format!("{{{}}}", elems.join(", "))
            }

            Expr::TupleLiteral(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| self.gen_expr(e)).collect();
                format!("{{{}}}", elems.join(", "))
            }

            Expr::As { expr, type_ } => {
                let inner = self.gen_expr(expr);
                let c_type = c_types::ast_type_to_c(&type_.node, self.scopes);
                format!("(({c_type}){inner})")
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond = self.gen_expr(condition);
                let then_val = self.gen_expr(then_branch);
                let else_val = else_branch
                    .as_ref()
                    .map(|e| self.gen_expr(e))
                    .unwrap_or_else(|| "0".to_string());
                format!("({cond} ? {then_val} : {else_val})")
            }

            Expr::Move { expr } | Expr::MutableBorrow { expr } | Expr::Deref { expr } => {
                // In C output, ownership/borrow semantics are erased
                self.gen_expr(expr)
            }

            Expr::Closure {
                params,
                body,
                ..
            } => {
                self.gen_closure_expr(params, body)
            }

            Expr::ImplicitClosure { body } => {
                // Implicit closure with `it` parameter
                let param = crate::parser::ast::ClosureParam {
                    type_: None,
                    ownership: crate::parser::ast::Ownership::Borrow,
                    name: crate::span::Spanned {
                        node: "it".to_string(),
                        span: body.span,
                    },
                };
                let params = vec![crate::span::Spanned {
                    node: param,
                    span: body.span,
                }];
                self.gen_closure_expr(&params, body)
            }

            Expr::ListComprehension {
                expr: comp_expr,
                variable,
                iterable,
                condition,
                ..
            } => {
                self.gen_list_comprehension(comp_expr, variable, iterable, condition.as_deref())
            }

            Expr::SetComprehension {
                expr: comp_expr,
                variable,
                iterable,
                condition,
            } => {
                // Emit as VyperArray stub (full set type is Phase 6+)
                let var_name = c_mangle::escape_keyword(&variable.node);
                let elem = self.gen_expr(comp_expr);
                let iter = self.gen_expr(iterable);
                let cond_str = condition
                    .as_ref()
                    .map(|c| format!(" if ({})", self.gen_expr(c)))
                    .unwrap_or_default();
                format!(
                    "/* TODO: set comprehension [{elem} for {var_name} in {iter}{cond_str}] */ (VyperArray){{0}}"
                )
            }

            Expr::DictComprehension {
                key,
                value: dict_val,
                variables,
                iterable,
                condition,
            } => {
                // Emit as stub (full dict type is Phase 6+)
                let k = self.gen_expr(key);
                let v = self.gen_expr(dict_val);
                let iter = self.gen_expr(iterable);
                let vars: Vec<String> = variables.iter().map(|v| v.node.clone()).collect();
                let cond_str = condition
                    .as_ref()
                    .map(|c| format!(" if ({})", self.gen_expr(c)))
                    .unwrap_or_default();
                format!(
                    "/* TODO: dict comprehension {{{k}: {v} for {} in {iter}{cond_str}}} */ (VyperArray){{0}}",
                    vars.join(", ")
                )
            }

            Expr::Catch { expr: catch_expr } => {
                // GCC statement expression: try the expression, on error return default
                let inner = self.gen_expr(catch_expr);
                format!(
                    "({{ __typeof__({inner}) __catch_val; \
                    if (VYPER_TRY) {{ __catch_val = {inner}; VYPER_CATCH_END; }} \
                    else {{ VYPER_CATCH_END; memset(&__catch_val, 0, sizeof(__catch_val)); }} \
                    __catch_val; }})"
                )
            }

            Expr::Is {
                expr: is_expr,
                negated,
                pattern,
            } => {
                let val = self.gen_expr(is_expr);
                let cond = self.pattern_to_condition_expr(&pattern.node, &val);
                if *negated {
                    format!("(!({cond}))")
                } else {
                    format!("({cond})")
                }
            }

            Expr::It => "it".to_string(),

            _ => format!("/* unsupported expr */0"),
        }
    }

    /// Generate a plain C string literal (no interpolation).
    fn gen_string_literal(&self, s: &StringLit) -> String {
        let mut result = String::from("\"");
        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    result.push_str(&escape_string(text));
                }
                StringSegment::Interpolation(_) => {
                    // Interpolations inside string literals are only handled in print context
                    result.push_str("%s");
                }
            }
        }
        result.push('"');
        result
    }

    /// Generate a function/builtin call.
    fn gen_call(
        &self,
        callee: &Spanned<Expr>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        // Check for built-in print/println
        if let Expr::Identifier(name) = &callee.node {
            match name.as_str() {
                "print" | "println" => return self.gen_print_call(args, name == "println"),
                "len" => {
                    if let Some(arg) = args.first() {
                        let a = self.gen_expr(&arg.node.value);
                        return format!("(sizeof({a}) / sizeof({a}[0]))");
                    }
                }
                _ => {}
            }

            // Check if this is a struct constructor
            if let Some(def_id) = self.scopes.lookup(name) {
                let def = self.scopes.get_def(def_id);
                if def.kind == crate::semantic::scope::DefKind::Struct {
                    let field_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    let fields = field_exprs.join(", ");
                    return format!("({name}){{{fields}}}");
                }
                // Check for enum variant constructor
                if def.kind == crate::semantic::scope::DefKind::Variant {
                    // Find which enum this variant belongs to
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, vid) in &info.variants {
                            if *vid == def_id {
                                let enum_name = &self.scopes.get_def(*enum_def_id).name;
                                let field_exprs: Vec<String> =
                                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                                let fields = field_exprs.join(", ");
                                return format!(
                                    "{}({fields})",
                                    c_mangle::mangle_variant(enum_name, vname)
                                );
                            }
                        }
                    }
                }
            }
        }

        // Check if callee is a Path expression (static method call like Point.origin())
        if let Expr::Path { segments } = &callee.node {
            if segments.len() == 2 {
                let type_name = &segments[0].node;
                let method_name = &segments[1].node;
                let mangled = c_mangle::mangle_method(type_name, method_name);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{mangled}({})", arg_exprs.join(", "));
            }
        }

        // General function call
        let callee_str = self.gen_expr(callee);
        let callee_name = c_mangle::escape_keyword(&callee_str);
        let arg_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
        format!("{callee_name}({})", arg_exprs.join(", "))
    }

    /// Generate a method call: `receiver.method(args)` → `Type__method(&receiver, args)`
    fn gen_method_call(
        &self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let recv = self.gen_expr(receiver);
        // Try to figure out the receiver type for mangling
        let type_name = self.infer_receiver_type(receiver);
        let mangled = c_mangle::mangle_method(&type_name, method_name);
        let mut all_args = vec![format!("&{recv}")];
        for arg in args {
            all_args.push(self.gen_expr(&arg.node.value));
        }
        format!("{mangled}({})", all_args.join(", "))
    }

    /// Try to infer the type name of a receiver expression (best-effort).
    fn infer_receiver_type(&self, expr: &Spanned<Expr>) -> String {
        match &expr.node {
            Expr::Identifier(name) => {
                // Look up the variable's type
                if let Some(def_id) = self.resolution_map.get(&expr.span.start) {
                    let def = self.scopes.get_def(*def_id);
                    if let Some(type_id) = def.type_id {
                        if let crate::semantic::types::ResolvedType::Defined(tid) =
                            self.types.get(type_id)
                        {
                            return self.scopes.get_def(*tid).name.clone();
                        }
                    }
                }
                name.clone()
            }
            Expr::SelfExpr => self
                .current_self_type
                .clone()
                .unwrap_or_else(|| "Self".to_string()),
            _ => "Unknown".to_string(),
        }
    }

    /// Generate a print/println call, handling string interpolation.
    /// `print()` always appends a newline (like Python). `println()` is an alias.
    pub fn gen_print_call(
        &self,
        args: &[Spanned<crate::parser::ast::CallArg>],
        _is_println: bool,
    ) -> String {
        if args.is_empty() {
            return "printf(\"\\n\")".to_string();
        }

        let arg = &args[0].node.value;

        // Check if the argument is a string literal with interpolations
        if let Expr::StringLiteral(s) = &arg.node {
            // print() always adds a newline
            return self.gen_printf_from_string_lit(s, true);
        }

        // Non-string argument: try to print as the correct type
        let expr = self.gen_expr(arg);
        format!("printf(\"%lld\\n\", (long long){expr})")
    }

    /// Generate printf from a StringLit with possible interpolation segments.
    fn gen_printf_from_string_lit(&self, s: &StringLit, is_println: bool) -> String {
        let mut format_parts = Vec::new();
        let mut printf_args = Vec::new();

        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    format_parts.push(escape_string(text));
                }
                StringSegment::Interpolation(var_name) => {
                    // Determine the format specifier based on the variable's type
                    let (fmt, arg_expr) = self.interpolation_format(var_name);
                    format_parts.push(fmt);
                    printf_args.push(arg_expr);
                }
            }
        }

        if is_println {
            format_parts.push("\\n".to_string());
        }

        let format_str = format_parts.join("");
        if printf_args.is_empty() {
            format!("printf(\"{format_str}\")")
        } else {
            format!("printf(\"{format_str}\", {})", printf_args.join(", "))
        }
    }

    /// Determine printf format specifier and C expression for an interpolated variable.
    fn interpolation_format(&self, var_name: &str) -> (String, String) {
        // Try to look up the variable's type
        let escaped = c_mangle::escape_keyword(var_name);

        // Try to find the variable in the scope to determine its type
        if let Some(def_id) = self.scopes.lookup(var_name) {
            let def = self.scopes.get_def(def_id);
            if let Some(type_id) = def.type_id {
                return self.format_for_type_id(type_id, &escaped);
            }
        }

        // Default: assume int64_t
        ("%lld".to_string(), format!("(long long){escaped}"))
    }

    /// Get printf format + expression for a given TypeId.
    fn format_for_type_id(
        &self,
        type_id: crate::semantic::ids::TypeId,
        expr: &str,
    ) -> (String, String) {
        use crate::semantic::types::ResolvedType;
        match self.types.get(type_id) {
            ResolvedType::Primitive(prim) => {
                let fmt = c_types::printf_format_for_primitive(*prim);
                let arg = match prim {
                    PrimitiveType::Bool => format!("{expr} ? \"true\" : \"false\""),
                    PrimitiveType::Int | PrimitiveType::Int64 => {
                        format!("(long long){expr}")
                    }
                    PrimitiveType::Uint | PrimitiveType::Uint64 => {
                        format!("(unsigned long long){expr}")
                    }
                    _ => expr.to_string(),
                };
                (fmt.to_string(), arg)
            }
            ResolvedType::Void => ("%s".to_string(), "\"void\"".to_string()),
            _ => ("%lld".to_string(), format!("(long long){expr}")),
        }
    }

    /// Generate a closure expression via lambda lifting.
    fn gen_closure_expr(
        &self,
        params: &[Spanned<crate::parser::ast::ClosureParam>],
        body: &Spanned<Expr>,
    ) -> String {
        use super::LiftedClosure;

        let id = {
            let mut counter = self.closure_counter.borrow_mut();
            let id = *counter;
            *counter += 1;
            id
        };

        let fn_name = c_mangle::mangle_closure(id);
        let env_name = c_mangle::mangle_closure_env(id);

        // Build parameter list
        let closure_params: Vec<(String, String)> = params
            .iter()
            .map(|p| {
                let name = c_mangle::escape_keyword(&p.node.name.node);
                let ty = p
                    .node
                    .type_
                    .as_ref()
                    .map(|t| c_types::ast_type_to_c(&t.node, self.scopes))
                    .unwrap_or_else(|| "int64_t".to_string());
                (name, ty)
            })
            .collect();

        // Collect free variables (simple heuristic: identifiers referenced in body
        // that are not in params). For now, we collect none — full free variable
        // analysis would require a scope walk. This suffices for simple closures.
        let param_names: std::collections::HashSet<&str> =
            params.iter().map(|p| p.node.name.node.as_str()).collect();
        let captures = self.collect_free_vars(&body.node, &param_names);

        // Generate the body expression
        let body_expr = self.gen_expr(body);

        let lifted = LiftedClosure {
            id,
            captures: captures.clone(),
            params: closure_params,
            return_type: "int64_t".to_string(), // default; refined later
            body: body_expr,
        };

        self.lifted_closures.borrow_mut().push(lifted);

        // At the creation site: allocate env, populate, create VyperClosure
        if captures.is_empty() {
            format!("(VyperClosure){{.fn_ptr = (void*){fn_name}, .env = NULL}}")
        } else {
            // Use a GCC statement expression to allocate and populate the env
            let mut parts = Vec::new();
            parts.push(format!("{env_name}* __env = ({env_name}*)malloc(sizeof({env_name}))"));
            for (cap_name, _) in &captures {
                parts.push(format!("__env->{cap_name} = {cap_name}"));
            }
            let stmts = parts.join("; ");
            format!(
                "({{ {stmts}; (VyperClosure){{.fn_ptr = (void*){fn_name}, .env = (void*)__env}}; }})"
            )
        }
    }

    /// Collect free variable references from an expression (simple walk).
    fn collect_free_vars(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
    ) -> Vec<(String, String)> {
        let mut free = Vec::new();
        let mut seen = std::collections::HashSet::new();
        self.walk_free_vars(expr, bound, &mut seen, &mut free);
        free
    }

    fn walk_free_vars(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<&str>,
        seen: &mut std::collections::HashSet<String>,
        free: &mut Vec<(String, String)>,
    ) {
        match expr {
            Expr::Identifier(name) if !bound.contains(name.as_str()) && name != "self" => {
                if seen.insert(name.clone()) {
                    let ty = self.infer_c_type_from_expr(expr);
                    free.push((c_mangle::escape_keyword(name), ty));
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.walk_free_vars(&left.node, bound, seen, free);
                self.walk_free_vars(&right.node, bound, seen, free);
            }
            Expr::UnaryOp { operand, .. } => {
                self.walk_free_vars(&operand.node, bound, seen, free);
            }
            Expr::Call { callee, args, .. } => {
                self.walk_free_vars(&callee.node, bound, seen, free);
                for arg in args {
                    self.walk_free_vars(&arg.node.value.node, bound, seen, free);
                }
            }
            Expr::FieldAccess { object, .. } => {
                self.walk_free_vars(&object.node, bound, seen, free);
            }
            Expr::MethodCall {
                receiver, args, ..
            } => {
                self.walk_free_vars(&receiver.node, bound, seen, free);
                for arg in args {
                    self.walk_free_vars(&arg.node.value.node, bound, seen, free);
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_free_vars(&condition.node, bound, seen, free);
                self.walk_free_vars(&then_branch.node, bound, seen, free);
                if let Some(eb) = else_branch {
                    self.walk_free_vars(&eb.node, bound, seen, free);
                }
            }
            Expr::Index { object, index } => {
                self.walk_free_vars(&object.node, bound, seen, free);
                self.walk_free_vars(&index.node, bound, seen, free);
            }
            _ => {}
        }
    }

    /// Generate a list comprehension as a GCC statement expression.
    fn gen_list_comprehension(
        &self,
        expr: &Spanned<Expr>,
        variable: &Spanned<crate::parser::ast::Pattern>,
        iterable: &Spanned<Expr>,
        condition: Option<&Spanned<Expr>>,
    ) -> String {
        let elem_expr = self.gen_expr(expr);
        let elem_type = self.infer_c_type_from_expr(&expr.node);

        let var_name = match &variable.node {
            crate::parser::ast::Pattern::Binding(name) => c_mangle::escape_keyword(name),
            _ => "__vyper_v".to_string(),
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
                "({{ VyperArray __comp = vyper_array_new(sizeof({elem_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {elem_type} __elem = {elem_expr}; vyper_array_push(&__comp, &__elem); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ VyperArray __comp = vyper_array_new(sizeof({elem_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            {elem_type} {var_name} = {iter}[__i]; \
            {cond_guard}{{ {elem_type} __elem = {elem_expr}; vyper_array_push(&__comp, &__elem); }} \
            }} __comp; }})"
        )
    }

    /// Convert a pattern to a C boolean condition for `is` expressions.
    fn pattern_to_condition_expr(&self, pattern: &crate::parser::ast::Pattern, scrutinee: &str) -> String {
        use crate::parser::ast::Pattern;
        match pattern {
            Pattern::Literal(lit) => {
                let val = self.gen_expr(lit);
                format!("{scrutinee} == {val}")
            }
            Pattern::Wildcard => "1".to_string(),
            Pattern::Binding(_) => "1".to_string(),
            Pattern::Constructor { path, .. } => {
                if path.len() == 2 {
                    let tag = c_mangle::mangle_tag(&path[0].node, &path[1].node);
                    format!("{scrutinee}.tag == {tag}")
                } else if path.len() == 1 {
                    // Try to find the enum for this variant
                    let variant_name = &path[0].node;
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, _) in &info.variants {
                            if vname == variant_name {
                                let enum_name = &self.scopes.get_def(*enum_def_id).name;
                                let tag = c_mangle::mangle_tag(enum_name, variant_name);
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
                    .map(|p| self.pattern_to_condition_expr(&p.node, scrutinee))
                    .collect();
                format!("({})", conds.join(" || "))
            }
            Pattern::Tuple(_) | Pattern::Rest => "1".to_string(),
        }
    }
}

/// Convert a BinaryOp to its C operator string.
fn binary_op_to_c(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Mod => "%",
        BinaryOp::Eq => "==",
        BinaryOp::Neq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Gt => ">",
        BinaryOp::LtEq => "<=",
        BinaryOp::GtEq => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::In => "/* in */ ==", // `in` doesn't have a direct C equivalent
    }
}

/// Escape a character for a C char literal.
fn escape_char(c: char) -> String {
    match c {
        '\'' => "\\'".to_string(),
        '\\' => "\\\\".to_string(),
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\0' => "\\0".to_string(),
        c if c.is_ascii_graphic() || c == ' ' => c.to_string(),
        c => format!("\\x{:02x}", c as u32),
    }
}

/// Escape a string for a C string literal (without outer quotes).
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            '\0' => result.push_str("\\0"),
            '%' => result.push_str("%%"),
            c => result.push(c),
        }
    }
    result
}
