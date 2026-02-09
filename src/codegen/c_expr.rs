/// Expression codegen: convert Gorget expressions to C expression strings.
use crate::lexer::token::{StringLit, StringSegment};
use crate::parser::ast::{BinaryOp, Expr, PrimitiveType, UnaryOp};
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate a C expression string from a Gorget expression.
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

            Expr::Call { callee, generic_args, args } => {
                if let Some(type_args) = generic_args {
                    self.gen_generic_call(callee, type_args, args)
                } else {
                    self.gen_call(callee, args)
                }
            }

            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                if let Some(type_args) = generic_args {
                    self.gen_generic_method_call(receiver, &method.node, type_args, args)
                } else {
                    self.gen_method_call(receiver, &method.node, args)
                }
            }

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
                self.gen_set_comprehension(comp_expr, variable, iterable, condition.as_deref())
            }

            Expr::DictComprehension {
                key,
                value: dict_val,
                variables,
                iterable,
                condition,
            } => {
                self.gen_dict_comprehension(key, dict_val, variables, iterable, condition.as_deref())
            }

            Expr::TryCapture { expr: catch_expr } => {
                // GCC statement expression: try the expression, on error return default
                let inner = self.gen_expr(catch_expr);
                format!(
                    "({{ __typeof__({inner}) __catch_val; \
                    if (GORGET_TRY) {{ __catch_val = {inner}; GORGET_CATCH_END; }} \
                    else {{ GORGET_CATCH_END; memset(&__catch_val, 0, sizeof(__catch_val)); }} \
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

            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => self.gen_match_expr(scrutinee, arms, else_arm.as_deref()),

            Expr::Block(block) => self.gen_block_expr(block),

            Expr::Do { body } => self.gen_block_expr(body),

            Expr::OptionalChain { object, field } => {
                let obj = self.gen_expr(object);
                let field_name = c_mangle::escape_keyword(&field.node);
                format!("({obj} != NULL ? {obj}->{field_name} : NULL)")
            }

            Expr::NilCoalescing { lhs, rhs } => {
                let l = self.gen_expr(lhs);
                let r = self.gen_expr(rhs);
                format!("({l} != NULL ? {l} : {r})")
            }

            Expr::Try { expr: try_expr } => {
                let inner = self.gen_expr(try_expr);
                format!(
                    "({{ __typeof__({inner}) __try_val; \
                    if (GORGET_TRY) {{ __try_val = {inner}; GORGET_CATCH_END; }} \
                    else {{ GORGET_CATCH_END; memset(&__try_val, 0, sizeof(__try_val)); return __try_val; }} \
                    __try_val; }})"
                )
            }

            Expr::Await { expr: await_expr } => {
                let inner = self.gen_expr(await_expr);
                format!("/* await */ {inner}")
            }

            Expr::Spawn { expr: spawn_expr } => {
                let inner = self.gen_expr(spawn_expr);
                format!("/* spawn */ {inner}")
            }
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

                // Handle Box.new(value) → heap allocation
                if type_name == "Box" && method_name == "new" {
                    if let Some(arg) = args.first() {
                        let inner = self.gen_expr(&arg.node.value);
                        let inner_type = self.infer_c_type_from_expr(&arg.node.value.node);
                        return format!(
                            "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; (void*)__box_tmp; }})"
                        );
                    }
                }

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
        // Check if receiver is a trait object → vtable dispatch
        if let Some(_trait_name) = self.resolve_trait_object_type(receiver) {
            let recv = self.gen_expr(receiver);
            let mut all_args = vec![format!("{recv}.data")];
            for arg in args {
                all_args.push(self.gen_expr(&arg.node.value));
            }
            return format!("{recv}.vtable->{method_name}({})", all_args.join(", "));
        }

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

    /// Check if a receiver expression has a trait object type, returning the trait name if so.
    fn resolve_trait_object_type(&self, expr: &Spanned<Expr>) -> Option<String> {
        if let Expr::Identifier(name) = &expr.node {
            if let Some(def_id) = self.resolution_map.get(&expr.span.start) {
                let def = self.scopes.get_def(*def_id);
                if let Some(type_id) = def.type_id {
                    if let crate::semantic::types::ResolvedType::TraitObject(trait_def_id) =
                        self.types.get(type_id)
                    {
                        return Some(self.scopes.get_def(*trait_def_id).name.clone());
                    }
                }
            }
            // Fallback: search all scopes (codegen doesn't track current scope)
            if let Some(def_id) = self.scopes.lookup_by_name_anywhere(name) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    if let crate::semantic::types::ResolvedType::TraitObject(trait_def_id) =
                        self.types.get(type_id)
                    {
                        return Some(self.scopes.get_def(*trait_def_id).name.clone());
                    }
                }
            }
        }
        None
    }

    /// Generate a generic function call: `func[int](args)` → `func__int64_t(args)`
    fn gen_generic_call(
        &self,
        callee: &Spanned<Expr>,
        type_args: &[Spanned<crate::parser::ast::Type>],
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let c_type_args: Vec<String> = type_args
            .iter()
            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
            .collect();

        let base_name = match &callee.node {
            Expr::Identifier(name) => name.clone(),
            Expr::Path { segments } if segments.len() == 2 => {
                let type_name = &segments[0].node;
                let method_name = &segments[1].node;
                let mangled = c_mangle::mangle_method(type_name, method_name);
                let full_mangled = c_mangle::mangle_generic(&mangled, &c_type_args);
                self.register_generic(&mangled, &c_type_args, super::GenericInstanceKind::Function);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{full_mangled}({})", arg_exprs.join(", "));
            }
            _ => {
                let callee_str = self.gen_expr(callee);
                callee_str
            }
        };

        // Check if the callee is a generic struct constructor
        if self.generic_struct_templates.borrow().contains_key(&base_name) {
            let mangled = self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Struct);
            let field_exprs: Vec<String> =
                args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            return format!("({mangled}){{{}}}", field_exprs.join(", "));
        }

        // Check if the callee is a generic enum variant constructor
        if self.generic_enum_templates.borrow().contains_key(&base_name) {
            let mangled = self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Enum);
            let field_exprs: Vec<String> =
                args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
            return format!("({mangled}){{{}}}", field_exprs.join(", "));
        }

        let mangled = c_mangle::mangle_generic(&base_name, &c_type_args);
        self.register_generic(&base_name, &c_type_args, super::GenericInstanceKind::Function);
        let arg_exprs: Vec<String> = args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
        format!("{mangled}({})", arg_exprs.join(", "))
    }

    /// Generate a generic method call: `obj.method[T](args)` → `Type__method__T(&obj, args)`
    fn gen_generic_method_call(
        &self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        type_args: &[Spanned<crate::parser::ast::Type>],
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let recv = self.gen_expr(receiver);
        let type_name = self.infer_receiver_type(receiver);
        let c_type_args: Vec<String> = type_args
            .iter()
            .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
            .collect();
        let base_method = c_mangle::mangle_method(&type_name, method_name);
        let mangled = c_mangle::mangle_generic(&base_method, &c_type_args);
        self.register_generic(&base_method, &c_type_args, super::GenericInstanceKind::Function);
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

        // Search all scopes for the variable (codegen doesn't track current scope)
        if let Some(def_id) = self.scopes.lookup_by_name_anywhere(var_name) {
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

        // At the creation site: emit a bare function pointer (no captures)
        // or allocate env and create GorgetClosure (with captures).
        if captures.is_empty() {
            fn_name
        } else {
            // Use a GCC statement expression to allocate and populate the env
            let mut parts = Vec::new();
            parts.push(format!("{env_name}* __env = ({env_name}*)malloc(sizeof({env_name}))"));
            for (cap_name, _) in &captures {
                parts.push(format!("__env->{cap_name} = {cap_name}"));
            }
            let stmts = parts.join("; ");
            format!(
                "({{ {stmts}; (GorgetClosure){{.fn_ptr = (void*){fn_name}, .env = (void*)__env}}; }})"
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
    fn gen_set_comprehension(
        &self,
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
    fn gen_dict_comprehension(
        &self,
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
                "({{ GorgetMap __comp = gorget_map_new(sizeof({key_type}), sizeof({val_type})); \
                for (int64_t {var_name} = {start_expr}; {var_name} {cmp} {end_expr}; {var_name}++) {{ \
                {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
                gorget_map_put(&__comp, &__k, &__v); }} \
                }} __comp; }})"
            );
        }

        // Generic iterable (array)
        let iter = self.gen_expr(iterable);
        let cond_guard = condition
            .map(|c| format!("if ({}) ", self.gen_expr(c)))
            .unwrap_or_default();

        format!(
            "({{ GorgetMap __comp = gorget_map_new(sizeof({key_type}), sizeof({val_type})); \
            for (size_t __i = 0; __i < sizeof({iter})/sizeof({iter}[0]); __i++) {{ \
            __typeof__({iter}[0]) {var_name} = {iter}[__i]; \
            {cond_guard}{{ {key_type} __k = {key_expr}; {val_type} __v = {val_expr}; \
            gorget_map_put(&__comp, &__k, &__v); }} \
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

    /// Generate a match expression as a GCC statement expression.
    fn gen_match_expr(
        &self,
        scrutinee: &Spanned<Expr>,
        arms: &[crate::parser::ast::MatchArm],
        else_arm: Option<&Spanned<Expr>>,
    ) -> String {
        let scrut_expr = self.gen_expr(scrutinee);

        let mut parts = Vec::new();
        parts.push(format!("__typeof__({scrut_expr}) __gorget_scrut = {scrut_expr}"));

        // Build if-else chain; each arm assigns to __gorget_match_result
        let mut arm_parts = Vec::new();
        let mut first = true;
        for arm in arms {
            let cond = self.pattern_to_condition_expr(&arm.pattern.node, "__gorget_scrut");
            let full_cond = if let Some(guard) = &arm.guard {
                let guard_expr = self.gen_expr(guard);
                format!("({cond}) && ({guard_expr})")
            } else {
                cond
            };

            let bindings = self.pattern_bindings_inline(&arm.pattern.node, "__gorget_scrut");
            let body = self.gen_expr(&arm.body);

            if first {
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

        // Determine a result type from the first arm body
        let result_type = if let Some(arm) = arms.first() {
            format!("__typeof__(({}))", self.gen_expr(&arm.body))
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
    fn pattern_bindings_inline(&self, pattern: &crate::parser::ast::Pattern, scrutinee: &str) -> String {
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
    fn find_enum_for_variant_path_expr(&self, path: &[Spanned<String>]) -> Option<(String, String)> {
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
    fn gen_block_expr(&self, block: &crate::parser::ast::Block) -> String {
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

    /// Generate inline C code for a statement (for use inside GCC statement expressions).
    fn stmt_to_inline_string(&self, stmt: &crate::parser::ast::Stmt) -> String {
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
            } => {
                let const_prefix = if *is_const { "const " } else { "" };
                match &pattern.node {
                    crate::parser::ast::Pattern::Binding(name) => {
                        let escaped = c_mangle::escape_keyword(name);
                        let c_type = match &type_.node {
                            crate::parser::ast::Type::Inferred => self.infer_c_type_from_expr(&value.node),
                            _ => c_types::ast_type_to_c(&type_.node, self.scopes),
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
