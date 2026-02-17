/// Print, format, and string interpolation codegen.
///
/// Handles `print()`, `format()`, and the interpolation machinery that
/// maps Gorget types to printf format specifiers.
use crate::lexer::token::{StringLit, StringSegment};
use crate::parser::ast::{Expr, PrimitiveType, Type};
use crate::parser::Parser;
use crate::semantic::scope::DefKind;
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::c_expr::escape_string;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate a `print()` call with optional `file=` and `newline=` kwargs.
    ///
    /// - `print("hello")` → stdout with newline
    /// - `print("hello", file=stderr)` → stderr with newline
    /// - `print("hello", newline=false)` → stdout without newline
    pub(super) fn gen_print_call(
        &mut self,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let stream = self.extract_print_file_kwarg(args);
        let newline = self.extract_print_newline_kwarg(args);

        // Filter out kwargs to get content args
        let content_args: Vec<_> = args
            .iter()
            .filter(|a| {
                !matches!(&a.node.name, Some(n) if n.node == "file" || n.node == "newline")
            })
            .collect();

        if content_args.is_empty() {
            return if newline {
                match &stream {
                    Some(s) => format!("fprintf({s}, \"\\n\")"),
                    None => "printf(\"\\n\")".to_string(),
                }
            } else {
                // No content, no newline → no-op
                "((void)0)".to_string()
            };
        }

        let arg = &content_args[0].node.value;

        if let Expr::StringLiteral(s) = &arg.node {
            return self.gen_printf_from_string_lit(s, newline, stream.as_deref());
        }

        // Non-string argument: try to print as the correct type
        let expr = self.gen_expr(arg);
        let nl = if newline { "\\n" } else { "" };

        if let Some(type_id) = self.infer_interp_expr_type(arg) {
            let (fmt, arg_expr) = self.format_for_type_id(type_id, &expr);
            return match &stream {
                Some(s) => format!("fprintf({s}, \"{fmt}{nl}\", {arg_expr})"),
                None => format!("printf(\"{fmt}{nl}\", {arg_expr})"),
            };
        }

        match &stream {
            Some(s) => format!("fprintf({s}, \"%lld{nl}\", (long long){expr})"),
            None => format!("printf(\"%lld{nl}\", (long long){expr})"),
        }
    }

    /// Extract `file=` kwarg from print args → C stream name (e.g. "stderr").
    fn extract_print_file_kwarg(
        &mut self,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> Option<String> {
        for arg in args {
            if let Some(ref name) = arg.node.name {
                if name.node == "file" {
                    let val = self.gen_expr(&arg.node.value);
                    return Some(val);
                }
            }
        }
        None
    }

    /// Extract `newline=` kwarg from print args → bool (defaults to true).
    fn extract_print_newline_kwarg(
        &self,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> bool {
        for arg in args {
            if let Some(ref name) = arg.node.name {
                if name.node == "newline" {
                    if let Expr::BoolLiteral(b) = &arg.node.value.node {
                        return *b;
                    }
                }
            }
        }
        true
    }

    /// Generate a `gorget_format(...)` call that returns `const char*`.
    pub(super) fn gen_format_call(
        &mut self,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        if args.is_empty() {
            return "gorget_format(\"\")".to_string();
        }

        let arg = &args[0].node.value;

        // String literal with interpolations → reuse interpolation_format
        if let Expr::StringLiteral(s) = &arg.node {
            return self.gen_gorget_format_from_string_lit(s);
        }

        // Non-string argument: format a single value
        let expr = self.gen_expr(arg);

        if let Some(type_id) = self.infer_interp_expr_type(arg) {
            let (fmt, arg_expr) = self.format_for_type_id(type_id, &expr);
            return format!("gorget_format(\"{fmt}\", {arg_expr})");
        }

        format!("gorget_format(\"%lld\", (long long){expr})")
    }

    /// Generate `gorget_format("fmt", args...)` from a StringLit.
    fn gen_gorget_format_from_string_lit(&mut self, s: &StringLit) -> String {
        let mut format_parts = Vec::new();
        let mut format_args = Vec::new();

        for segment in &s.segments {
            match segment {
                StringSegment::Literal(text) => {
                    format_parts.push(escape_string(text));
                }
                StringSegment::Interpolation(var_name) => {
                    let (fmt, arg_expr) = self.interpolation_format(var_name);
                    format_parts.push(fmt);
                    format_args.push(arg_expr);
                }
            }
        }

        let format_str = format_parts.join("");
        if format_args.is_empty() {
            format!("gorget_format(\"{format_str}\")")
        } else {
            format!("gorget_format(\"{format_str}\", {})", format_args.join(", "))
        }
    }

    /// Generate printf/fprintf from a StringLit with possible interpolation segments.
    /// When `stream` is `None`, emits `printf(...)`. When `Some("stderr")`, emits `fprintf(stderr, ...)`.
    fn gen_printf_from_string_lit(&mut self, s: &StringLit, is_println: bool, stream: Option<&str>) -> String {
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
        let (func, prefix) = match stream {
            Some(s) => ("fprintf", format!("{s}, ")),
            None => ("printf", String::new()),
        };
        if printf_args.is_empty() {
            format!("{func}({prefix}\"{format_str}\")")
        } else {
            format!("{func}({prefix}\"{format_str}\", {})", printf_args.join(", "))
        }
    }

    /// Determine printf format specifier and C expression for an interpolated variable.
    fn interpolation_format(&mut self, var_name: &str) -> (String, String) {
        // Try to look up the variable's type
        let escaped = c_mangle::escape_keyword(var_name);

        // Handle dotted paths like "t._0" or "nested._1._0" by resolving
        // the base variable type, then following tuple field accesses.
        if var_name.contains('.') {
            let parts: Vec<&str> = var_name.splitn(2, '.').collect();
            let base = parts[0];
            let field_path = parts[1];
            // Use -> for pointer param field access
            let base_escaped = c_mangle::escape_keyword(base);
            let c_expr = if self.pointer_params.contains(&base_escaped) {
                format!("{base_escaped}->{}", c_mangle::escape_keyword(field_path))
            } else {
                escaped.clone()
            };
            if let Some(def_id) = self.scoped_lookup(base) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    if let Some(resolved_id) = self.resolve_field_type(type_id, field_path) {
                        return self.format_for_type_id(resolved_id, &c_expr);
                    }
                }
            }
        } else {
            // Dereference pointer params for interpolation
            let c_expr = if self.pointer_params.contains(&escaped) {
                format!("(*{escaped})")
            } else {
                escaped.clone()
            };
            // Search all scopes for the variable (codegen doesn't track current scope)
            if let Some(def_id) = self.scoped_lookup(var_name) {
                let def = self.scopes.get_def(def_id);
                if let Some(type_id) = def.type_id {
                    return self.format_for_type_id(type_id, &c_expr);
                }
            }
        }

        // Try parsing as a full expression (handles method calls, function calls, operators)
        if let Ok(parsed_expr) = Parser::new(var_name).parse_expr() {
            let c_expr = self.gen_expr(&parsed_expr);
            let type_id = self.infer_interp_expr_type(&parsed_expr);
            if let Some(tid) = type_id {
                return self.format_for_type_id(tid, &c_expr);
            }
            // Default: assume int for unknown expression types
            return ("%lld".to_string(), format!("(long long){c_expr}"));
        }

        // Default: assume int64_t
        let c_expr = if self.pointer_params.contains(&escaped) {
            format!("(*{escaped})")
        } else {
            escaped
        };
        ("%lld".to_string(), format!("(long long){c_expr}"))
    }

    /// Resolve a dotted field path against a type, returning the final TypeId.
    /// Handles tuple field access ("_0", "_1._0") and struct field access ("name", "msg.sender").
    fn resolve_field_type(
        &mut self,
        type_id: crate::semantic::ids::TypeId,
        field_path: &str,
    ) -> Option<crate::semantic::ids::TypeId> {
        use crate::semantic::types::ResolvedType;

        let (field, rest) = match field_path.split_once('.') {
            Some((f, r)) => (f, Some(r)),
            None => (field_path, None),
        };

        // Tuple field access: _0, _1, etc.
        if let Some(idx_str) = field.strip_prefix('_') {
            if let Ok(idx) = idx_str.parse::<usize>() {
                if let ResolvedType::Tuple(elems) = self.types.get(type_id) {
                    if let Some(&elem_type_id) = elems.get(idx) {
                        return match rest {
                            Some(remaining) => self.resolve_field_type(elem_type_id, remaining),
                            None => Some(elem_type_id),
                        };
                    }
                }
            }
        }

        // Struct field access
        if let ResolvedType::Defined(def_id) = self.types.get(type_id) {
            let struct_name = self.scopes.get_def(*def_id).name.clone();
            let key = (struct_name, field.to_string());
            if let Some(ast_type) = self.field_type_names.get(&key).cloned() {
                if let Some(field_tid) = self.ast_type_to_type_id(&ast_type) {
                    return match rest {
                        Some(remaining) => self.resolve_field_type(field_tid, remaining),
                        None => Some(field_tid),
                    };
                }
            }
        }

        None
    }

    /// Convert an AST `Type` to a semantic `TypeId`.
    pub(super) fn ast_type_to_type_id(&mut self, ty: &Type) -> Option<crate::semantic::ids::TypeId> {
        match ty {
            Type::Primitive(p) => match p {
                PrimitiveType::Int | PrimitiveType::Int64 => Some(self.types.int_id),
                PrimitiveType::Float | PrimitiveType::Float64 => Some(self.types.float_id),
                PrimitiveType::Bool => Some(self.types.bool_id),
                PrimitiveType::Char => Some(self.types.char_id),
                PrimitiveType::Str => Some(self.types.string_id),
                PrimitiveType::StringType => Some(self.types.owned_string_id),
                PrimitiveType::Void => Some(self.types.void_id),
                _ => None,
            },
            Type::Named { name, generic_args } if generic_args.is_empty() => {
                self.scoped_lookup(&name.node)
                    .and_then(|def_id| {
                        let def = self.scopes.get_def(def_id);
                        def.type_id
                    })
            }
            _ => None,
        }
    }

    /// Get printf format + expression for a given TypeId.
    pub(super) fn format_for_type_id(
        &mut self,
        type_id: crate::semantic::ids::TypeId,
        expr: &str,
    ) -> (String, String) {
        use crate::semantic::types::ResolvedType;

        // If the type is a generic param with an active substitution, resolve through TypeId
        // (preserves Displayable dispatch, enum/struct formatting, etc.).
        if let ResolvedType::Defined(def_id) = self.types.get(type_id) {
            let def = self.scopes.get_def(*def_id);
            if def.kind == DefKind::GenericParam {
                let param_name = def.name.clone();
                // Prefer type_id_subs (rich path) over type_subs (lossy C-string path)
                if let Some(tid) = self.type_id_subs.iter().find(|(n, _)| *n == param_name).map(|(_, v)| *v) {
                    return self.format_for_type_id(tid, expr);
                }
                // Fallback: if type_id_subs didn't have it (mangled generic), print type name
                if let Some(c_type) = self.type_subs.iter().find(|(n, _)| *n == param_name).map(|(_, v)| v.clone()) {
                    return ("%s".to_string(), format!("\"<{c_type}>\""));
                }
            }
        }

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
                    PrimitiveType::StringType => self.coerce_string_to_str(expr),
                    _ => expr.to_string(),
                };
                (fmt.to_string(), arg)
            }
            ResolvedType::Void => ("%s".to_string(), "\"void\"".to_string()),
            ResolvedType::Generic(def_id, args) if self.scopes.get_def(*def_id).name == "Box" && args.len() == 1 => {
                // Box[T]: auto-dereference and format the inner type
                let deref_expr = format!("(*{expr})");
                self.format_for_type_id(args[0], &deref_expr)
            }
            ResolvedType::Defined(def_id) | ResolvedType::Generic(def_id, _) => {
                let name = self.scopes.get_def(*def_id).name.clone();
                if self.traits.has_trait_impl_by_name(&name, "Displayable") {
                    // For generic types, use the mangled name (e.g., Pair__int64_t)
                    let mangled_name = if let ResolvedType::Generic(gdef_id, args) = self.types.get(type_id) {
                        let base = super::c_types::def_name_to_c(*gdef_id, self.scopes);
                        let c_args: Vec<String> = args.iter()
                            .map(|tid| super::c_types::type_id_to_c(*tid, self.types, self.scopes))
                            .collect();
                        super::c_mangle::mangle_generic(&base, &c_args)
                    } else {
                        name.clone()
                    };
                    let mangled = c_mangle::mangle_trait_method("Displayable", &mangled_name, "display");
                    // Use a GCC statement expression to handle non-lvalue exprs
                    let call = format!("({{ __typeof__({expr}) __tmp = {expr}; {mangled}(&__tmp); }})");
                    ("%s".to_string(), call)
                } else {
                    // Fallback: print the type name
                    ("%s".to_string(), format!("\"<{name}>\""))
                }
            }
            ResolvedType::Array(_, _)
            | ResolvedType::Tuple(_)
            | ResolvedType::Function { .. }
            | ResolvedType::TraitObject(_)
            | ResolvedType::CallableTrait(_)
            | ResolvedType::MutCallableTrait(_)
            | ResolvedType::MoveCallableTrait(_)
            | ResolvedType::BoxedCallable { .. }
            | ResolvedType::Slice(_) => {
                panic!("non-primitive type cannot be used in string interpolation")
            }
            ResolvedType::Error => {
                // In monomorphized contexts, Error often means an unresolved generic param.
                // Try to substitute using the active type_id_subs.
                if self.type_id_subs.len() == 1 {
                    return self.format_for_type_id(self.type_id_subs[0].1, expr);
                }
                // Fallback: assume integer (matches legacy behavior for inline method calls
                // like `print("{c.get()}")` where the return type can't be resolved).
                ("%lld".to_string(), format!("(long long){expr}"))
            }
            ResolvedType::Never | ResolvedType::Var(_) => {
                ("%lld".to_string(), format!("(long long){expr}"))
            }
        }
    }

    /// Convert a TypeId to a C type string, applying active type_subs for GenericParams.
    /// Recursively substitutes GenericParam args nested inside Generic containers.
    /// Falls back to the standard `type_id_to_c` when no substitution applies.
    pub(super) fn type_id_to_c_substituted(&self, tid: crate::semantic::ids::TypeId) -> String {
        if !self.type_subs.is_empty() {
            // Direct GenericParam → substitute
            if let crate::semantic::types::ResolvedType::Defined(def_id) = self.types.get(tid) {
                let def = self.scopes.get_def(*def_id);
                if def.kind == DefKind::GenericParam {
                    if let Some((_, c_type)) = self.type_subs.iter().find(|(n, _)| *n == def.name) {
                        return c_type.clone();
                    }
                }
            }

            // Generic with potentially-GenericParam args → recurse
            if let crate::semantic::types::ResolvedType::Generic(def_id, args) = self.types.get(tid) {
                let base = c_types::def_name_to_c(*def_id, self.scopes);
                let c_args: Vec<String> = args.iter()
                    .map(|a| self.type_id_to_c_substituted(*a))
                    .collect();
                return match base.as_str() {
                    "Vector" | "List" | "Array" => "GorgetArray".to_string(),
                    "Set" => "GorgetSet".to_string(),
                    "Dict" => c_mangle::mangle_generic("GorgetDict", &c_args),
                    "HashMap" => c_mangle::mangle_generic("GorgetMap", &c_args),
                    "Box" if args.len() == 1 => {
                        let inner_resolved = self.types.get(args[0]);
                        if let crate::semantic::types::ResolvedType::TraitObject(trait_def_id) = inner_resolved {
                            return c_mangle::mangle_trait_obj(&c_types::def_name_to_c(*trait_def_id, self.scopes));
                        }
                        format!("{}*", c_args[0])
                    }
                    _ => c_mangle::mangle_generic(&base, &c_args),
                };
            }

            // Error with single type param → best-effort substitution
            if matches!(self.types.get(tid), crate::semantic::types::ResolvedType::Error) && self.type_subs.len() == 1 {
                return self.type_subs[0].1.clone();
            }
        }
        c_types::type_id_to_c(tid, self.types, self.scopes)
    }

    /// Infer the result TypeId of a sub-parsed interpolation expression.
    pub(super) fn infer_interp_expr_type(&mut self, expr: &Spanned<Expr>) -> Option<crate::semantic::ids::TypeId> {
        self.resolve_expr_type_id(expr)
    }

    /// Map (receiver C type, method name) → return TypeId for known builtins.
    pub(super) fn builtin_method_return_type(
        &mut self,
        receiver_type: &str,
        method: &str,
    ) -> Option<crate::semantic::ids::TypeId> {
        // GorgetMap/GorgetSet use prefix matching because monomorphized names
        // are mangled (e.g. "GorgetMap__int64_t__int64_t").
        if receiver_type == "GorgetArray" {
            return match method {
                "len" | "get" | "pop" | "index_of" => Some(self.types.int_id),
                "contains" | "any" | "all" => Some(self.types.bool_id),
                "sort" | "reverse" | "insert" | "extend" => Some(self.types.void_id),
                _ => None,
            };
        }
        if receiver_type.starts_with("GorgetMap") {
            return match method {
                "len" | "get" => Some(self.types.int_id),
                "contains" => Some(self.types.bool_id),
                _ => None,
            };
        }
        if receiver_type.starts_with("GorgetSet") {
            return match method {
                "len" => Some(self.types.int_id),
                "contains" | "is_subset" | "is_superset" => Some(self.types.bool_id),
                _ => None,
            };
        }
        if receiver_type == "GorgetString" {
            return match method {
                "len" | "hash" | "index_of" | "count" | "capacity" => Some(self.types.int_id),
                "contains" | "starts_with" | "ends_with" | "is_empty" => Some(self.types.bool_id),
                "trim" | "strip" | "lstrip" | "rstrip" | "to_upper" | "to_lower" | "replace"
                | "substring" | "repeat" | "join" | "removeprefix" | "removesuffix"
                | "pad_left" | "pad_right" => Some(self.types.owned_string_id),
                "str" => Some(self.types.string_id),
                "char_at" => Some(self.types.char_id),
                "push" | "push_char" | "clear" => Some(self.types.void_id),
                _ => None,
            };
        }
        match (receiver_type, method) {
            ("const char*", "len" | "hash" | "index_of" | "count") => Some(self.types.int_id),
            ("const char*", "contains" | "starts_with" | "ends_with" | "is_empty") => {
                Some(self.types.bool_id)
            }
            ("const char*", "trim" | "strip" | "lstrip" | "rstrip" | "to_upper" | "to_lower" | "replace" | "substring" | "repeat" | "join" | "removeprefix" | "removesuffix" | "pad_left" | "pad_right") => {
                Some(self.types.owned_string_id)
            }
            ("const char*", "char_at") => Some(self.types.char_id),
            (
                "int64_t" | "int8_t" | "int16_t" | "int32_t" |
                "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t" |
                "double" | "float" | "bool" | "char32_t",
                "hash",
            ) => Some(self.types.int_id),
            ("char" | "char32_t", "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace" | "is_upper" | "is_lower") => {
                Some(self.types.bool_id)
            }
            ("char", "to_upper" | "to_lower") => {
                Some(self.types.char_id)
            }
            _ => None,
        }
    }
}
