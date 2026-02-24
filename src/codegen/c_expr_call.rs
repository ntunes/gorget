/// Call-related expression codegen: function calls, method calls, and `in` operator.
use crate::parser::ast::{Expr, Ownership};
use crate::semantic::scope::DefKind;
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::c_expr::{is_lvalue, addr_of};
use super::CodegenContext;

/// Built-in collection methods that consume arguments by value.
/// Returns indices of consuming args (excluding self), or None.
fn builtin_consuming_arg_indices(type_name: &str, method: &str) -> Option<&'static [usize]> {
    match (type_name, method) {
        ("Vector" | "List" | "Array", "push") => Some(&[0]),
        ("Vector" | "List" | "Array", "set") => Some(&[1]),
        ("Dict" | "HashMap", "put") => Some(&[1]),
        ("Set" | "HashSet", "add") => Some(&[0]),
        _ => None,
    }
}

impl CodegenContext<'_> {
    /// Queue move-zeroing for arguments consumed by a method call.
    ///
    /// Two-tier lookup:
    /// 1. Static table for built-in collection methods (push/set/put/add)
    /// 2. method_resolutions → FunctionInfo.param_ownerships for user-defined
    ///    methods with Ownership::Move parameters
    pub(super) fn queue_method_arg_move_zeros(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &Spanned<String>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) {
        // 1. Built-in table
        let type_name = self.infer_receiver_type(receiver);
        if let Some(indices) = builtin_consuming_arg_indices(&type_name, &method.node) {
            for &idx in indices {
                if let Some(a) = args.get(idx) {
                    if let Expr::Identifier(name) = &a.node.value.node {
                        self.queue_move_zero_if_droppable(name);
                    } else if let Expr::FieldAccess { object, field } = &a.node.value.node {
                        self.queue_field_move_zero(&object.node, &field.node);
                    }
                }
            }
            return;
        }

        // 2. User-defined methods with Move params
        if let Some(&def_id) = self.method_resolutions.get(&method.span.start) {
            if let Some(fi) = self.function_info.get(&def_id) {
                // param_ownerships[0] is self, user args start at index 1
                for (i, a) in args.iter().enumerate() {
                    if fi.param_ownerships.get(i + 1) == Some(&Ownership::Move) {
                        if let Expr::Identifier(name) = &a.node.value.node {
                            self.queue_move_zero_if_droppable(name);
                        } else if let Expr::FieldAccess { object, field } = &a.node.value.node {
                            self.queue_field_move_zero(&object.node, &field.node);
                        }
                    }
                }
            }
        }
    }

    /// Queue field-level move-zeroing for FieldAccess arguments in function calls.
    ///
    /// When a non-Copy struct field is passed by value to a function, the callee
    /// receives a shallow copy sharing the same buffer pointers. If the function
    /// stores the copy in a long-lived structure, the parent struct's field-drop
    /// at scope exit would double-free. Zeroing the field after the call prevents this.
    ///
    /// Guards (via `queue_field_move_zero` → `resolve_field_zero_target`):
    /// - Parent must have StructDrop with `has_user_drop: false`
    /// - Field type must be non-Copy (has a drop action)
    /// - Parent must not be a pointer parameter
    pub(super) fn queue_call_arg_field_move_zeros(
        &mut self,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) {
        for a in args {
            // Skip &-args — no copy made, no buffer aliasing
            if matches!(a.node.ownership, Ownership::MutableBorrow) {
                continue;
            }
            if let Expr::FieldAccess { object, field } = &a.node.value.node {
                self.queue_field_move_zero(&object.node, &field.node);
            }
        }
    }

    /// Queue move-zeroing for all identifier args in a constructor call (CallArg variant).
    /// All struct/variant/newtype fields consume their arguments by value.
    pub(super) fn queue_constructor_move_zeros_call_args(&mut self, args: &[Spanned<crate::parser::ast::CallArg>]) {
        for a in args {
            if let Expr::Identifier(name) = &a.node.value.node {
                self.queue_move_zero_if_droppable(name);
            } else if let Expr::FieldAccess { object, field } = &a.node.value.node {
                self.queue_field_move_zero(&object.node, &field.node);
            }
        }
    }

    /// Queue move-zeroing for all identifier args in a constructor call (Expr variant).
    /// All struct literal fields consume their arguments by value.
    pub(super) fn queue_constructor_move_zeros_exprs(&mut self, args: &[Spanned<Expr>]) {
        for a in args {
            if let Expr::Identifier(ref name) = a.node {
                self.queue_move_zero_if_droppable(name);
            } else if let Expr::FieldAccess { object, field } = &a.node {
                self.queue_field_move_zero(&object.node, &field.node);
            }
        }
    }

    /// Generate a C expression for an argument that expects `const char*` (str).
    /// If the argument is a String (GorgetString), coerces via `.data`.
    pub(super) fn gen_str_arg(&mut self, arg: &Spanned<Expr>) -> String {
        let expr = self.gen_expr(arg);
        if self.infer_c_type_from_expr(&arg.node) == "GorgetString" {
            self.coerce_string_to_str(&expr)
        } else {
            expr
        }
    }

    /// Extract the `alloc=` named arg from a call's args list.
    /// Returns the generated C expression for the allocator if found.
    pub(super) fn extract_alloc_arg(&mut self, args: &[Spanned<crate::parser::ast::CallArg>]) -> Option<String> {
        for arg in args {
            if arg.node.name.as_ref().map_or(false, |n| n.node == "alloc") {
                return Some(self.gen_expr(&arg.node.value));
            }
        }
        None
    }

    /// Filter out the `alloc=` named arg, returning only positional args.
    pub(super) fn filter_alloc_arg<'b>(args: &'b [Spanned<crate::parser::ast::CallArg>]) -> Vec<&'b Spanned<crate::parser::ast::CallArg>> {
        args.iter().filter(|a| !a.node.name.as_ref().map_or(false, |n| n.node == "alloc")).collect()
    }

    /// Wrap a constructor expression in alloc save/push/pop/restore.
    pub(super) fn wrap_with_alloc(&mut self, alloc_expr: &str, constructor_code: &str, result_c_type: &str) -> String {
        let n = self.alloc_tmp_counter;
        self.alloc_tmp_counter += 1;
        format!(
            "({{ GorgetAllocator* __saved_alloc_{n} = __gorget_current_alloc; \
             __gorget_current_alloc = &{alloc_expr}->__alloc; \
             {result_c_type} __alloc_tmp_{n} = {constructor_code}; \
             __gorget_current_alloc = __saved_alloc_{n}; \
             __alloc_tmp_{n}; }})"
        )
    }

    /// Generate a function/builtin call.
    pub(super) fn gen_call(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        // Handle None() as a variant constructor for Option[T]
        if matches!(&callee.node, Expr::NoneLiteral) && args.is_empty() {
            if let Some(mangled) = self.resolve_unit_variant_from_type_hint("Option", "None") {
                return format!("{}()", c_mangle::mangle_variant(&mangled, "None"));
            }
        }

        // Check for built-in and stdlib function calls
        if let Expr::Identifier(name) = &callee.node {
            // Compiler builtins — always dispatch (no stdlib guard needed)
            match name.as_str() {
                "print" => return self.gen_print_call(args),
                "format" => return self.gen_format_call(args),
                "String" => {
                    // String() → empty owned string, String(val) → owned copy
                    let alloc_expr = self.extract_alloc_arg(args);
                    let pos_args = Self::filter_alloc_arg(args);
                    let ctor = if pos_args.is_empty() {
                        "gorget_string_new(\"\")".to_string()
                    } else {
                        let arg = self.gen_expr(&pos_args[0].node.value);
                        let arg_type = self.infer_c_type_from_expr(&pos_args[0].node.value.node);
                        // String(1024) → capacity constructor
                        if matches!(arg_type.as_str(), "int64_t" | "int8_t" | "int16_t" | "int32_t"
                            | "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t") {
                            format!("gorget_string_with_capacity({arg})")
                        } else if arg_type == "GorgetString" {
                            format!("gorget_string_new({arg}.data)")
                        } else {
                            format!("gorget_string_new({arg})")
                        }
                    };
                    return if let Some(alloc) = alloc_expr {
                        self.wrap_with_alloc(&alloc, &ctor, "GorgetString")
                    } else {
                        ctor
                    };
                }
                "Arena" => {
                    let pos_args = Self::filter_alloc_arg(args);
                    let cap = if !pos_args.is_empty() {
                        self.gen_expr(&pos_args[0].node.value)
                    } else {
                        "4096".to_string()
                    };
                    return format!("gorget_arena_new((size_t)({cap}))");
                }
                "TrackingAllocator" => {
                    return "gorget_tracking_new()".to_string();
                }
                "len" => {
                    if let Some(arg) = args.first() {
                        let len_method = Spanned::dummy("len".to_string());
                        return self.gen_method_call(&arg.node.value, &len_method, &[]);
                    }
                }
                _ => {}
            }

            // Stdlib functions — only dispatch if resolved to a stdlib (dummy-span) def
            if self.is_stdlib_call(name) {
                match name.as_str() {
                    "read_file" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_read_file({path})");
                        }
                    }
                    "write_file" | "append_file" => {
                        let func = if name == "write_file" { "gorget_write_file" } else { "gorget_append_file" };
                        if args.len() >= 2 {
                            let path = self.gen_str_arg(&args[0].node.value);
                            let content = self.gen_str_arg(&args[1].node.value);
                            return format!("{func}({path}, {content})");
                        }
                    }
                    "file_exists" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_file_exists({path})");
                        }
                    }
                    "delete_file" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_delete_file({path})");
                        }
                    }
                    "mkdir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_mkdir({path})");
                        }
                    }
                    "rmdir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_rmdir({path})");
                        }
                    }
                    "rename" => {
                        if args.len() >= 2 {
                            let old = self.gen_str_arg(&args[0].node.value);
                            let new = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_rename({old}, {new})");
                        }
                    }
                    "copy_file" => {
                        if args.len() >= 2 {
                            let src = self.gen_str_arg(&args[0].node.value);
                            let dst = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_copy_file({src}, {dst})");
                        }
                    }
                    "file_size" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_file_size({path})");
                        }
                    }
                    "is_dir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_is_dir({path})");
                        }
                    }
                    "path_parent" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_path_parent({p}))");
                        }
                    }
                    "path_basename" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_path_basename({p}))");
                        }
                    }
                    "path_extension" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_path_extension({p}))");
                        }
                    }
                    "path_stem" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_path_stem({p}))");
                        }
                    }
                    "path_join" => {
                        if args.len() >= 2 {
                            let a = self.gen_str_arg(&args[0].node.value);
                            let b = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_string_adopt((char*)gorget_path_join({a}, {b}))");
                        }
                    }
                    "readdir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_readdir({path})");
                        }
                    }
                    "args" => {
                        return "gorget_args()".to_string();
                    }
                    "exec" => {
                        if let Some(arg) = args.first() {
                            let cmd = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_exec({cmd})");
                        }
                    }
                    "exit" => {
                        if let Some(arg) = args.first() {
                            let code = self.gen_expr(&arg.node.value);
                            return format!("exit((int)({code}))");
                        }
                        return "exit(0)".to_string();
                    }
                    "rand" => return "gorget_rand()".to_string(),
                    "rand_range" => {
                        if args.len() >= 2 {
                            let lo = self.gen_expr(&args[0].node.value);
                            let hi = self.gen_expr(&args[1].node.value);
                            return format!("gorget_rand_range({lo}, {hi})");
                        }
                    }
                    "time" => return "gorget_time()".to_string(),
                    "time_ms" => return "gorget_time_ms()".to_string(),
                    "format_time" => {
                        if args.len() >= 2 {
                            let epoch = self.gen_expr(&args[0].node.value);
                            let fmt = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_string_adopt((char*)gorget_format_time({epoch}, {fmt}))");
                        }
                    }
                    "parse_time" => {
                        if args.len() >= 2 {
                            let s = self.gen_str_arg(&args[0].node.value);
                            let fmt = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_parse_time({s}, {fmt})");
                        }
                    }
                    "getchar" => return "gorget_getchar()".to_string(),
                    "readline" => return "gorget_string_adopt((char*)gorget_readline())".to_string(),
                    "input" => {
                        if let Some(arg) = args.first() {
                            let prompt = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_input({prompt}))");
                        }
                    }
                    "stdin_eof" => return "((bool)feof(stdin))".to_string(),
                    "term_cols" => return "gorget_term_cols()".to_string(),
                    "term_rows" => return "gorget_term_rows()".to_string(),
                    "seed" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            return format!("gorget_seed({n})");
                        }
                    }
                    "sleep_ms" => {
                        if let Some(arg) = args.first() {
                            let ms = self.gen_expr(&arg.node.value);
                            return format!("gorget_sleep_ms({ms})");
                        }
                    }
                    "sleep" => {
                        if let Some(arg) = args.first() {
                            let seconds = self.gen_expr(&arg.node.value);
                            return format!("gorget_async_sleep({seconds})");
                        }
                    }
                    "ord" => {
                        if let Some(arg) = args.first() {
                            let c = self.gen_expr(&arg.node.value);
                            return format!("(int64_t)({c})");
                        }
                    }
                    "chr" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            return format!("(char)({n})");
                        }
                    }
                    "parse_int" => {
                        if let Some(arg) = args.first() {
                            let s = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_parse_int({s})");
                        }
                    }
                    "parse_float" => {
                        if let Some(arg) = args.first() {
                            let s = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_parse_float({s})");
                        }
                    }
                    "int_to_str" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_int_to_str({n}))");
                        }
                    }
                    "float_to_str" => {
                        if let Some(arg) = args.first() {
                            let x = self.gen_expr(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_float_to_str({x}))");
                        }
                    }
                    "bool_to_str" => {
                        if let Some(arg) = args.first() {
                            let b = self.gen_expr(&arg.node.value);
                            return format!("gorget_bool_to_str({b})");
                        }
                    }
                    "char_to_str" => {
                        if let Some(arg) = args.first() {
                            let c = self.gen_expr(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_char_to_str({c}))");
                        }
                    }
                    "codepoint_to_str" => {
                        if let Some(arg) = args.first() {
                            let cp = self.gen_expr(&arg.node.value);
                            return format!("gorget_string_adopt((char*)gorget_codepoint_to_utf8({cp}))");
                        }
                    }
                    "getenv" => {
                        if let Some(arg) = args.first() {
                            let name_expr = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_getenv({name_expr})");
                        }
                    }
                    "setenv" => {
                        if args.len() >= 2 {
                            let name_expr = self.gen_str_arg(&args[0].node.value);
                            let val_expr = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_setenv({name_expr}, {val_expr})");
                        }
                    }
                    "getcwd" => return "gorget_string_adopt((char*)gorget_getcwd())".to_string(),
                    "platform" => return "gorget_platform()".to_string(),
                    // std.math — abs/min/max dispatch to float or int variant
                    "abs" => {
                        if let Some(arg) = args.first() {
                            let x = self.gen_expr(&arg.node.value);
                            let is_float = self.resolve_expr_type_id(&arg.node.value)
                                .map_or(false, |t| t == self.types.float_id);
                            let func = if is_float { "gorget_fabs" } else { "gorget_abs" };
                            return format!("{func}({x})");
                        }
                    }
                    "min" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            let is_float = self.resolve_expr_type_id(&args[0].node.value)
                                .map_or(false, |t| t == self.types.float_id);
                            let func = if is_float { "gorget_fmin" } else { "gorget_min" };
                            return format!("{func}({a}, {b})");
                        }
                    }
                    "max" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            let is_float = self.resolve_expr_type_id(&args[0].node.value)
                                .map_or(false, |t| t == self.types.float_id);
                            let func = if is_float { "gorget_fmax" } else { "gorget_max" };
                            return format!("{func}({a}, {b})");
                        }
                    }
                    // std.math — float (1-arg)
                    "sqrt" | "floor" | "ceil" | "round" | "log" | "log2" | "log10"
                    | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" => {
                        if let Some(arg) = args.first() {
                            let x = self.gen_expr(&arg.node.value);
                            return format!("gorget_{name}({x})");
                        }
                    }
                    // std.math — float (2-arg)
                    "pow" | "atan2" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            return format!("gorget_{name}({a}, {b})");
                        }
                    }
                    // std.process
                    "exec_output" => {
                        if let Some(arg) = args.first() {
                            let cmd = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_exec_output({cmd})");
                        }
                    }
                    // std.sdl — lifecycle
                    "sdl_init" => {
                        let flags = if let Some(a) = args.first() { self.gen_expr(&a.node.value) } else { "0".to_string() };
                        return format!("gorget_sdl_init({flags})");
                    }
                    "sdl_quit" => return "gorget_sdl_quit()".to_string(),
                    // std.sdl — window
                    "sdl_create_window" => {
                        if args.len() >= 4 {
                            let title = self.gen_str_arg(&args[0].node.value);
                            let w = self.gen_expr(&args[1].node.value);
                            let h = self.gen_expr(&args[2].node.value);
                            let flags = self.gen_expr(&args[3].node.value);
                            return format!("gorget_sdl_create_window({title}, {w}, {h}, {flags})");
                        }
                    }
                    "sdl_destroy_window" => {
                        if let Some(a) = args.first() {
                            let win = self.gen_expr(&a.node.value);
                            return format!("gorget_sdl_destroy_window({win})");
                        }
                    }
                    "sdl_get_window_width" | "sdl_get_window_height" => {
                        if let Some(a) = args.first() {
                            let win = self.gen_expr(&a.node.value);
                            return format!("gorget_{name}({win})");
                        }
                    }
                    // std.sdl — renderer
                    "sdl_create_renderer" => {
                        if args.len() >= 2 {
                            let win = self.gen_expr(&args[0].node.value);
                            let flags = self.gen_expr(&args[1].node.value);
                            return format!("gorget_sdl_create_renderer({win}, {flags})");
                        }
                    }
                    "sdl_destroy_renderer" => {
                        if let Some(a) = args.first() {
                            let r = self.gen_expr(&a.node.value);
                            return format!("gorget_sdl_destroy_renderer({r})");
                        }
                    }
                    "sdl_set_draw_color" => {
                        if args.len() >= 5 {
                            let r = self.gen_expr(&args[0].node.value);
                            let red = self.gen_expr(&args[1].node.value);
                            let green = self.gen_expr(&args[2].node.value);
                            let blue = self.gen_expr(&args[3].node.value);
                            let alpha = self.gen_expr(&args[4].node.value);
                            return format!("gorget_sdl_set_draw_color({r}, {red}, {green}, {blue}, {alpha})");
                        }
                    }
                    "sdl_clear" | "sdl_present" => {
                        if let Some(a) = args.first() {
                            let r = self.gen_expr(&a.node.value);
                            return format!("gorget_{name}({r})");
                        }
                    }
                    "sdl_draw_rect" | "sdl_fill_rect" => {
                        if args.len() >= 5 {
                            let r = self.gen_expr(&args[0].node.value);
                            let x = self.gen_expr(&args[1].node.value);
                            let y = self.gen_expr(&args[2].node.value);
                            let w = self.gen_expr(&args[3].node.value);
                            let h = self.gen_expr(&args[4].node.value);
                            return format!("gorget_{name}({r}, {x}, {y}, {w}, {h})");
                        }
                    }
                    // std.sdl — drawing
                    "sdl_draw_line" => {
                        if args.len() >= 5 {
                            let r = self.gen_expr(&args[0].node.value);
                            let x1 = self.gen_expr(&args[1].node.value);
                            let y1 = self.gen_expr(&args[2].node.value);
                            let x2 = self.gen_expr(&args[3].node.value);
                            let y2 = self.gen_expr(&args[4].node.value);
                            return format!("gorget_sdl_draw_line({r}, {x1}, {y1}, {x2}, {y2})");
                        }
                    }
                    "sdl_draw_point" => {
                        if args.len() >= 3 {
                            let r = self.gen_expr(&args[0].node.value);
                            let x = self.gen_expr(&args[1].node.value);
                            let y = self.gen_expr(&args[2].node.value);
                            return format!("gorget_sdl_draw_point({r}, {x}, {y})");
                        }
                    }
                    "sdl_set_blend_mode" => {
                        if args.len() >= 2 {
                            let r = self.gen_expr(&args[0].node.value);
                            let mode = self.gen_expr(&args[1].node.value);
                            return format!("gorget_sdl_set_blend_mode({r}, {mode})");
                        }
                    }
                    // std.sdl — textures
                    "sdl_load_texture" => {
                        if args.len() >= 2 {
                            let r = self.gen_expr(&args[0].node.value);
                            let path = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_sdl_load_texture({r}, {path})");
                        }
                    }
                    "sdl_destroy_texture" => {
                        if let Some(a) = args.first() {
                            let t = self.gen_expr(&a.node.value);
                            return format!("gorget_sdl_destroy_texture({t})");
                        }
                    }
                    "sdl_render_texture" => {
                        if args.len() >= 4 {
                            let r = self.gen_expr(&args[0].node.value);
                            let t = self.gen_expr(&args[1].node.value);
                            let x = self.gen_expr(&args[2].node.value);
                            let y = self.gen_expr(&args[3].node.value);
                            return format!("gorget_sdl_render_texture({r}, {t}, {x}, {y})");
                        }
                    }
                    "sdl_render_texture_sized" => {
                        if args.len() >= 6 {
                            let r = self.gen_expr(&args[0].node.value);
                            let t = self.gen_expr(&args[1].node.value);
                            let x = self.gen_expr(&args[2].node.value);
                            let y = self.gen_expr(&args[3].node.value);
                            let w = self.gen_expr(&args[4].node.value);
                            let h = self.gen_expr(&args[5].node.value);
                            return format!("gorget_sdl_render_texture_sized({r}, {t}, {x}, {y}, {w}, {h})");
                        }
                    }
                    "sdl_texture_width" | "sdl_texture_height" => {
                        if let Some(a) = args.first() {
                            let t = self.gen_expr(&a.node.value);
                            return format!("gorget_{name}({t})");
                        }
                    }
                    "sdl_set_texture_alpha" => {
                        if args.len() >= 2 {
                            let t = self.gen_expr(&args[0].node.value);
                            let alpha = self.gen_expr(&args[1].node.value);
                            return format!("gorget_sdl_set_texture_alpha({t}, {alpha})");
                        }
                    }
                    // std.sdl — text
                    "sdl_load_font" => {
                        if args.len() >= 2 {
                            let path = self.gen_str_arg(&args[0].node.value);
                            let size = self.gen_expr(&args[1].node.value);
                            return format!("gorget_sdl_load_font({path}, {size})");
                        }
                    }
                    "sdl_close_font" => {
                        if let Some(a) = args.first() {
                            let f = self.gen_expr(&a.node.value);
                            return format!("gorget_sdl_close_font({f})");
                        }
                    }
                    "sdl_render_text" => {
                        if args.len() >= 6 {
                            let r = self.gen_expr(&args[0].node.value);
                            let f = self.gen_expr(&args[1].node.value);
                            let text = self.gen_str_arg(&args[2].node.value);
                            let red = self.gen_expr(&args[3].node.value);
                            let green = self.gen_expr(&args[4].node.value);
                            let blue = self.gen_expr(&args[5].node.value);
                            return format!("gorget_sdl_render_text({r}, {f}, {text}, {red}, {green}, {blue})");
                        }
                    }
                    "sdl_draw_text" => {
                        if args.len() >= 8 {
                            let r = self.gen_expr(&args[0].node.value);
                            let f = self.gen_expr(&args[1].node.value);
                            let text = self.gen_str_arg(&args[2].node.value);
                            let x = self.gen_expr(&args[3].node.value);
                            let y = self.gen_expr(&args[4].node.value);
                            let red = self.gen_expr(&args[5].node.value);
                            let green = self.gen_expr(&args[6].node.value);
                            let blue = self.gen_expr(&args[7].node.value);
                            return format!("gorget_sdl_draw_text({r}, {f}, {text}, {x}, {y}, {red}, {green}, {blue})");
                        }
                    }
                    "sdl_text_width" | "sdl_text_height" => {
                        if args.len() >= 2 {
                            let f = self.gen_expr(&args[0].node.value);
                            let text = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_{name}({f}, {text})");
                        }
                    }
                    // std.sdl — events
                    "sdl_poll_event" => return "gorget_sdl_poll_event()".to_string(),
                    "sdl_has_event" => return "gorget_sdl_has_event()".to_string(),
                    // std.sdl — timing
                    "sdl_delay" => {
                        if let Some(a) = args.first() {
                            let ms = self.gen_expr(&a.node.value);
                            return format!("gorget_sdl_delay({ms})");
                        }
                    }
                    "sdl_get_ticks" => return "gorget_sdl_get_ticks()".to_string(),
                    "sdl_get_performance_counter" => return "gorget_sdl_get_performance_counter()".to_string(),
                    // std.sdl — screen info
                    "sdl_get_display_width" => return "gorget_sdl_get_display_width()".to_string(),
                    "sdl_get_display_height" => return "gorget_sdl_get_display_height()".to_string(),
                    // std.crypto (Result-wrapping functions stay hardcoded)
                    "crypto_rsa_load_public" => {
                        if let Some(arg) = args.first() {
                            let kb = self.gen_expr(&arg.node.value);
                            let kb_addr = addr_of(&kb, &arg.node.value.node);
                            let result_type = c_mangle::mangle_generic("Result", &["GorgetRSAKey".into(), "const char*".into()]);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetRSAKey __rk = gorget_crypto_rsa_load_public({kb_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    "crypto_aes_ctr_new" => {
                        if args.len() >= 2 {
                            let key = self.gen_expr(&args[0].node.value);
                            let iv = self.gen_expr(&args[1].node.value);
                            let key_addr = addr_of(&key, &args[0].node.value.node);
                            let iv_addr = addr_of(&iv, &args[1].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetCipherContext".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetCipherContext __rk = gorget_crypto_aes_ctr_new({key_addr}, {iv_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    "crypto_hmac" => {
                        if args.len() >= 3 {
                            let algo = self.gen_str_arg(&args[0].node.value);
                            let key = self.gen_expr(&args[1].node.value);
                            let data = self.gen_expr(&args[2].node.value);
                            let key_addr = addr_of(&key, &args[1].node.value.node);
                            let data_addr = addr_of(&data, &args[2].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_hmac({algo}, {key_addr}, {data_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    "crypto_random_bytes" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_random_bytes({n}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    // std.crypto — Ed25519
                    "crypto_ed25519_keygen" => {
                        let result_type = self.register_generic("Result", &["GorgetEd25519KeyPair".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                        let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                        let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                        return format!(
                            "({{ GorgetEd25519KeyPair __rk = gorget_crypto_ed25519_keygen(); \
                            const char* __re = gorget_crypto_last_error(); \
                            __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                        );
                    }
                    "crypto_ed25519_sign" => {
                        if args.len() >= 2 {
                            let pk = self.gen_expr(&args[0].node.value);
                            let data = self.gen_expr(&args[1].node.value);
                            let pk_addr = addr_of(&pk, &args[0].node.value.node);
                            let data_addr = addr_of(&data, &args[1].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_ed25519_sign({pk_addr}, {data_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    // std.crypto — X25519
                    "crypto_x25519_keygen" => {
                        let result_type = self.register_generic("Result", &["GorgetX25519KeyPair".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                        let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                        let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                        return format!(
                            "({{ GorgetX25519KeyPair __rk = gorget_crypto_x25519_keygen(); \
                            const char* __re = gorget_crypto_last_error(); \
                            __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                        );
                    }
                    "crypto_x25519_shared_secret" => {
                        if args.len() >= 2 {
                            let pk = self.gen_expr(&args[0].node.value);
                            let peer_pub = self.gen_expr(&args[1].node.value);
                            let pk_addr = addr_of(&pk, &args[0].node.value.node);
                            let peer_pub_addr = addr_of(&peer_pub, &args[1].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_x25519_shared_secret({pk_addr}, {peer_pub_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    "crypto_x25519_dh" => {
                        if args.len() >= 2 {
                            let pk = self.gen_expr(&args[0].node.value);
                            let peer_pub = self.gen_expr(&args[1].node.value);
                            let pk_addr = addr_of(&pk, &args[0].node.value.node);
                            let peer_pub_addr = addr_of(&peer_pub, &args[1].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_x25519_dh({pk_addr}, {peer_pub_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    // std.crypto — HKDF-SHA256
                    "crypto_hkdf_sha256" => {
                        if args.len() >= 4 {
                            let salt = self.gen_expr(&args[0].node.value);
                            let ikm = self.gen_expr(&args[1].node.value);
                            let info = self.gen_expr(&args[2].node.value);
                            let length = self.gen_expr(&args[3].node.value);
                            let salt_addr = addr_of(&salt, &args[0].node.value.node);
                            let ikm_addr = addr_of(&ikm, &args[1].node.value.node);
                            let info_addr = addr_of(&info, &args[2].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_hkdf_sha256({salt_addr}, {ikm_addr}, {info_addr}, {length}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    // std.crypto — AES-256-GCM
                    "crypto_aes_gcm_encrypt" => {
                        if args.len() >= 3 {
                            let key = self.gen_expr(&args[0].node.value);
                            let nonce = self.gen_expr(&args[1].node.value);
                            let pt = self.gen_expr(&args[2].node.value);
                            let key_addr = addr_of(&key, &args[0].node.value.node);
                            let nonce_addr = addr_of(&nonce, &args[1].node.value.node);
                            let pt_addr = addr_of(&pt, &args[2].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_aes_gcm_encrypt({key_addr}, {nonce_addr}, {pt_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    "crypto_aes_gcm_decrypt" => {
                        if args.len() >= 2 {
                            let key = self.gen_expr(&args[0].node.value);
                            let ct = self.gen_expr(&args[1].node.value);
                            let key_addr = addr_of(&key, &args[0].node.value.node);
                            let ct_addr = addr_of(&ct, &args[1].node.value.node);
                            let result_type = self.register_generic("Result", &["GorgetArray".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetArray __rk = gorget_crypto_aes_gcm_decrypt({key_addr}, {ct_addr}); \
                                const char* __re = gorget_crypto_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rk); }})"
                            );
                        }
                    }
                    // std.net.udp
                    "udp_bind" => {
                        if args.len() >= 2 {
                            let addr = self.gen_str_arg(&args[0].node.value);
                            let port = self.gen_expr(&args[1].node.value);
                            let result_type = self.register_generic("Result", &["GorgetUdpSocket".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetUdpSocket __us = gorget_udp_bind({addr}, {port}); \
                                const char* __ue = gorget_udp_last_error(); \
                                __ue ? {err_ctor}(__ue) : {ok_ctor}(__us); }})"
                            );
                        }
                    }
                    // std.net.socket
                    "socket_connect" => {
                        if args.len() >= 2 {
                            let host = self.gen_str_arg(&args[0].node.value);
                            let port = self.gen_expr(&args[1].node.value);
                            let result_type = c_mangle::mangle_generic("Result", &["GorgetSocket".into(), "const char*".into()]);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetSocket __sk = gorget_socket_connect({host}, {port}); \
                                const char* __se = gorget_socket_last_error(); \
                                __se ? {err_ctor}(__se) : {ok_ctor}(__sk); }})"
                            );
                        }
                    }
                    // std.net.tls
                    "tls_connect" => {
                        if args.len() >= 2 {
                            let host = self.gen_str_arg(&args[0].node.value);
                            let port = self.gen_expr(&args[1].node.value);
                            let result_type = c_mangle::mangle_generic("Result", &["GorgetTlsSocket".into(), "const char*".into()]);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetTlsSocket __ts = gorget_tls_connect({host}, {port}); \
                                const char* __te = gorget_tls_last_error(); \
                                __te ? {err_ctor}(__te) : {ok_ctor}(__ts); }})"
                            );
                        }
                    }
                    // std.regex — free functions with Result/Option wrapping
                    "regex_compile" => {
                        if let Some(arg) = args.first() {
                            let pat = self.gen_str_arg(&arg.node.value);
                            let result_type = self.register_generic("Result", &["GorgetRegex".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetRegex __rx = gorget_regex_compile({pat}, NULL); \
                                const char* __re = gorget_regex_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rx); }})"
                            );
                        }
                    }
                    "regex_compile_with" => {
                        if args.len() >= 2 {
                            let pat = self.gen_str_arg(&args[0].node.value);
                            let flags = self.gen_str_arg(&args[1].node.value);
                            let result_type = self.register_generic("Result", &["GorgetRegex".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetRegex __rx = gorget_regex_compile({pat}, {flags}); \
                                const char* __re = gorget_regex_last_error(); \
                                __re ? {err_ctor}(__re) : {ok_ctor}(__rx); }})"
                            );
                        }
                    }
                    "regex_is_match" => {
                        if args.len() >= 2 {
                            let pat = self.gen_str_arg(&args[0].node.value);
                            let subj = self.gen_str_arg(&args[1].node.value);
                            return format!(
                                "({{ GorgetRegex __rx = gorget_regex_compile({pat}, NULL); \
                                bool __rm = gorget_regex_is_match(&__rx, {subj}); \
                                gorget_regex_free(&__rx); __rm; }})"
                            );
                        }
                    }
                    "regex_find" => {
                        if args.len() >= 2 {
                            let pat = self.gen_str_arg(&args[0].node.value);
                            let subj = self.gen_str_arg(&args[1].node.value);
                            let opt = self.register_generic("Option", &["GorgetRegexMatch".into()], super::GenericInstanceKind::Enum);
                            let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                            let ctor_none = c_mangle::mangle_variant(&opt, "None");
                            return format!(
                                "({{ GorgetRegex __rx = gorget_regex_compile({pat}, NULL); \
                                GorgetRegexMatch __rm = gorget_regex_find(&__rx, {subj}, 0); \
                                gorget_regex_free(&__rx); \
                                __rm.start >= 0 ? {ctor_some}(__rm) : {ctor_none}(); }})"
                            );
                        }
                    }
                    "regex_replace" => {
                        if args.len() >= 3 {
                            let pat = self.gen_str_arg(&args[0].node.value);
                            let subj = self.gen_str_arg(&args[1].node.value);
                            let repl = self.gen_str_arg(&args[2].node.value);
                            return format!(
                                "({{ GorgetRegex __rx = gorget_regex_compile({pat}, NULL); \
                                GorgetString __rs = gorget_regex_replace(&__rx, {subj}, {repl}); \
                                gorget_regex_free(&__rx); __rs; }})"
                            );
                        }
                    }
                    _ => {}
                }
            }

            // Handle Box(value) constructor → heap allocation
            if name == "Box" {
                if let Some(arg) = args.first() {
                    let inner = self.gen_expr(&arg.node.value);
                    let inner_type = self.box_inner_c_type(&arg.node.value);
                    return format!(
                        "({{ {inner_type}* __box_tmp = ({inner_type}*)GORGET_ALLOC(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                    );
                }
            }

            // Check if this is a struct/newtype constructor (struct calls are
            // normally rewritten to Expr::StructLiteral by the rewrite pass,
            // but newtypes still come through here).
            if let Some(def_id) = self.scopes.lookup(name) {
                let def = self.scopes.get_def(def_id);
                if def.kind == DefKind::Struct
                    || def.kind == DefKind::Newtype
                {
                    // Queue move-zeroing for consumed droppable args (fixes newtype bug)
                    self.queue_constructor_move_zeros_call_args(args);

                    let c_name = c_types::def_name_to_c(def_id, self.scopes);
                    let struct_name = def.name.clone();

                    // Hoist field names for per-field type hint + coercion.
                    // For structs, field order comes from struct_fields;
                    // for newtypes, single field named "value".
                    let field_names: Vec<String> = if def.kind == DefKind::Struct {
                        self.struct_fields.get(&def_id)
                            .map(|info| info.fields.iter().map(|(n, _)| n.clone()).collect())
                            .unwrap_or_default()
                    } else {
                        vec!["value".to_string()]
                    };

                    // Build per-field TypeIds for str↔String coercion.
                    let field_type_ids: Vec<Option<crate::semantic::ids::TypeId>> = {
                        field_names.iter().map(|fname| {
                            let key = (struct_name.clone(), fname.clone());
                            self.field_type_names.get(&key).and_then(|ast_type| {
                                use crate::parser::ast::{Type, PrimitiveType};
                                match ast_type {
                                    Type::Primitive(PrimitiveType::Str) => Some(self.types.string_id),
                                    Type::Primitive(PrimitiveType::StringType) => Some(self.types.owned_string_id),
                                    _ => None,
                                }
                            })
                        }).collect()
                    };

                    let saved_hint = self.decl_type_hint.clone();
                    let field_exprs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                        // Set per-field type hint so nested generic constructors
                        // (e.g. Some("hi") in an Option[str] field) resolve correctly
                        if let Some(fname) = field_names.get(i) {
                            let key = (struct_name.clone(), fname.clone());
                            if let Some(field_type) = self.field_type_names.get(&key) {
                                self.decl_type_hint = Some(field_type.clone());
                            }
                        }
                        let expr = self.gen_expr(&a.node.value);
                        let ptid = field_type_ids.get(i).copied().flatten();
                        self.coerce_arg_to_str(expr, &a.node.value, ptid)
                    }).collect();
                    self.decl_type_hint = saved_hint;
                    let fields = field_exprs.join(", ");
                    return format!("({c_name}){{{fields}}}");
                }
                // Check for enum variant constructor
                if def.kind == crate::semantic::scope::DefKind::Variant {
                    // Queue move-zeroing for consumed collection/droppable args
                    self.queue_constructor_move_zeros_call_args(args);
                    // Find which enum this variant belongs to
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, vid) in &info.variants {
                            if *vid == def_id {
                                let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                                let saved_hint = self.decl_type_hint.clone();

                                // Look up variant field types for per-field hint propagation.
                                // This ensures nested generic constructors (e.g. Some(Box(Num(1)))
                                // inside an Option[Box[Expr]] field) get the correct type hints.
                                let variant_field_types: Option<Vec<crate::parser::ast::Type>> =
                                    info.variant_field_types.iter()
                                        .find(|(vn, _)| vn == vname)
                                        .map(|(_, types)| types.iter().map(|t| t.node.clone()).collect());

                                let field_exprs: Vec<String> = args.iter().enumerate()
                                    .map(|(i, a)| {
                                        // Set per-field type hints when available
                                        if let Some(ref field_types) = variant_field_types {
                                            if let Some(field_type) = field_types.get(i) {
                                                self.decl_type_hint = Some(field_type.clone());
                                            }
                                        } else if i == 0 {
                                            // Fallback: peel one layer for single-type-param
                                            // generic enums (e.g. Some(Some(100)))
                                            if let Some(crate::parser::ast::Type::Named {
                                                name,
                                                generic_args,
                                            }) = saved_hint.as_ref()
                                            {
                                                if name.node == enum_name && generic_args.len() == 1 {
                                                    self.decl_type_hint =
                                                        Some(generic_args[0].node.clone());
                                                }
                                            }
                                        }
                                        self.gen_expr(&a.node.value)
                                    })
                                    .collect();
                                let fields = field_exprs.join(", ");
                                self.decl_type_hint = saved_hint;
                                // For generic enum templates, resolve the monomorphized name
                                if self.generic_enum_templates.contains_key(&enum_name) {
                                    // In a monomorphized method body, use the self type
                                    if let Some(self_type) = &self.current_self_type {
                                        let prefix = format!("{enum_name}__");
                                        if self_type.starts_with(&prefix) {
                                            return format!(
                                                "{}({fields})",
                                                c_mangle::mangle_variant(self_type, vname)
                                            );
                                        }
                                    }
                                    if let Some(mangled) = self.resolve_unit_variant_from_type_hint(&enum_name, vname) {
                                        return format!(
                                            "{}({fields})",
                                            c_mangle::mangle_variant(&mangled, vname)
                                        );
                                    }
                                }
                                return format!(
                                    "{}({fields})",
                                    c_mangle::mangle_variant(&enum_name, vname)
                                );
                            }
                        }
                    }
                }
            }

            // Check if this is a boxed callable trait object — dispatch through vtable
            let escaped_name = c_mangle::escape_keyword(name);
            if let Some(super::ClosureVarInfo::TraitObject { .. }) = self.closure_var_info.get(escaped_name.as_str()) {
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                let mut call_args = vec![format!("{escaped_name}.data")];
                call_args.extend(arg_exprs);
                return format!("{escaped_name}.vtable->call({})", call_args.join(", "));
            }

            // Check if this is a GorgetClosure variable — dispatch through .fn_ptr
            if self.closure_vars.contains(escaped_name.as_str()) {
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                let (cast, param_ownerships) = if let Some((param_types, ret, ownerships)) = self.fn_type_signatures.get(&escaped_name) {
                    // Fn[sig]-typed: use declared signature for precise cast
                    let mut cp = vec!["void*".to_string()];
                    cp.extend(param_types.clone());
                    (format!("{ret} (*)({})", cp.join(", ")), Some(ownerships.clone()))
                } else {
                    // Legacy GorgetClosure: infer from arguments
                    let arg_types: Vec<String> = args
                        .iter()
                        .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                        .collect();
                    let mut cp = vec!["void*".to_string()];
                    cp.extend(arg_types);
                    (format!("int64_t (*)({})", cp.join(", ")), None)
                };
                // Apply ownership-based argument wrapping for MutableBorrow params
                let wrapped_args: Vec<String> = arg_exprs.into_iter().enumerate().map(|(i, expr)| {
                    let ownership = param_ownerships.as_ref()
                        .and_then(|o| o.get(i).copied())
                        .unwrap_or(crate::parser::ast::Ownership::Borrow);
                    if matches!(ownership, crate::parser::ast::Ownership::MutableBorrow) {
                        super::c_expr::addr_of(&expr, &args[i].node.value.node)
                    } else {
                        expr
                    }
                }).collect();
                let mut call_args = vec![format!("{escaped_name}.env")];
                call_args.extend(wrapped_args);
                return format!("(({cast})({escaped_name}.fn_ptr))({})", call_args.join(", "));
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
                        let inner_type = self.box_inner_c_type(&arg.node.value);
                        return format!(
                            "({{ {inner_type}* __box_tmp = ({inner_type}*)GORGET_ALLOC(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                        );
                    }
                }

                // Handle File.open(path) and File.create(path)
                if type_name == "File" {
                    match method_name.as_str() {
                        "open" => {
                            if let Some(arg) = args.first() {
                                let path_arg = self.gen_str_arg(&arg.node.value);
                                return format!("gorget_file_open({path_arg}, \"r\")");
                            }
                        }
                        "create" => {
                            if let Some(arg) = args.first() {
                                let path_arg = self.gen_str_arg(&arg.node.value);
                                return format!("gorget_file_open({path_arg}, \"w\")");
                            }
                        }
                        _ => {}
                    }
                }

                let mangled = c_mangle::mangle_method(type_name, method_name);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{mangled}({})", arg_exprs.join(", "));
            }
        }

        // Check extern free function bindings
        if let Expr::Identifier(name) = &callee.node {
            if let Some(binding) = self.extern_symbols.get(&("".to_string(), name.clone())).cloned() {
                let arg_exprs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                    let expr = self.gen_expr(&a.node.value);
                    if binding.params_need_ref.get(i) == Some(&true) {
                        addr_of(&expr, &a.node.value.node)
                    } else {
                        expr
                    }
                }).collect();
                return format!("{}({})", binding.c_symbol, arg_exprs.join(", "));
            }
        }

        // General function call
        self.queue_call_arg_field_move_zeros(args);
        let callee_str = self.gen_expr(callee);
        let arg_exprs = self.resolve_call_args(callee, args);
        format!("{callee_str}({})", arg_exprs.join(", "))
    }

    /// Coerce between GorgetString and const char* at function call sites.
    pub(super) fn coerce_arg_to_str(
        &mut self,
        expr: String,
        arg_expr: &Spanned<Expr>,
        param_type_id: Option<crate::semantic::ids::TypeId>,
    ) -> String {
        let arg_type = self.infer_c_type_from_expr(&arg_expr.node);
        if let Some(ptid) = param_type_id {
            // String arg → str param: coerce via .data
            if arg_type == "GorgetString" && ptid == self.types.string_id {
                return self.coerce_string_to_str(&expr);
            }
            // str arg → String param: wrap with gorget_string_new
            if arg_type == "const char*" && ptid == self.types.owned_string_id {
                return format!("gorget_string_new({expr})");
            }
        }
        expr
    }

    /// Coerce a bare function pointer / function name to GorgetClosure when the
    /// parameter expects Fn[sig]. Already-GorgetClosure values pass through unchanged.
    /// Coerce a bare function pointer / function name to GorgetClosure when the
    /// parameter expects Fn[sig]. Already-GorgetClosure values pass through unchanged.
    pub(super) fn coerce_arg_to_fn(
        &mut self,
        expr: String,
        _arg_expr: &Spanned<Expr>,
        param_type_id: Option<crate::semantic::ids::TypeId>,
    ) -> String {
        if let Some(ptid) = param_type_id {
            if matches!(self.types.get(ptid),
                crate::semantic::types::ResolvedType::CallableTrait(_)
                | crate::semantic::types::ResolvedType::MutCallableTrait(_)
                | crate::semantic::types::ResolvedType::ConsumeCallableTrait(_)
            ) {
                // Already a GorgetClosure variable — pass through
                if self.closure_vars.contains(expr.as_str()) {
                    return expr;
                }
                // Already a GorgetClosure compound literal (e.g. capturing closure) — pass through
                if expr.starts_with("(GorgetClosure)") {
                    return expr;
                }
                // For non-capturing closure functions, use the _fn adapter
                // which has the void* env_ptr first param for ABI compatibility.
                let fn_ptr = if expr.starts_with("__gorget_closure_") {
                    format!("{expr}_fn")
                } else {
                    // Named function — generate an adapter closure that wraps it
                    // with the correct (void*, params...) ABI.
                    let inner = &self.types.get(ptid);
                    let inner_id = match inner {
                        crate::semantic::types::ResolvedType::CallableTrait(id)
                        | crate::semantic::types::ResolvedType::MutCallableTrait(id)
                        | crate::semantic::types::ResolvedType::ConsumeCallableTrait(id) => *id,
                        _ => unreachable!(),
                    };
                    if let crate::semantic::types::ResolvedType::Function { params, param_ownerships, return_type } =
                        self.types.get(inner_id).clone()
                    {
                        let id = self.closure_counter;
                        self.closure_counter += 1;
                        let struct_name = format!("__Closure_{id}");
                        let fn_name = super::c_mangle::mangle_closure(id);

                        let closure_params: Vec<(String, String)> = params.iter().enumerate()
                            .map(|(i, tid)| {
                                let base = super::c_types::type_id_to_c(*tid, self.types, self.scopes);
                                let ownership = param_ownerships.get(i).copied()
                                    .unwrap_or(crate::parser::ast::Ownership::Borrow);
                                let c_type = if matches!(ownership, crate::parser::ast::Ownership::MutableBorrow) {
                                    format!("{base}*")
                                } else {
                                    base
                                };
                                (format!("__p{i}"), c_type)
                            })
                            .collect();
                        let ret_type = super::c_types::type_id_to_c(return_type, self.types, self.scopes);
                        let arg_names: Vec<&str> = closure_params.iter().map(|(n, _)| n.as_str()).collect();
                        let call_expr = format!("{expr}({})", arg_names.join(", "));

                        self.lifted_closures.push(super::LiftedClosure {
                            id,
                            struct_name,
                            captures: vec![],
                            params: closure_params,
                            return_type: ret_type,
                            body: call_expr,
                        });

                        format!("{fn_name}_fn")
                    } else {
                        expr
                    }
                };
                // Wrap bare function pointer / function name into GorgetClosure
                return format!("(GorgetClosure){{.fn_ptr = (void*){fn_ptr}, .env = NULL}}");
            }
        }
        expr
    }

    /// Wrap a generated expression with `&` if the call arg has MutableBorrow ownership.
    /// Uses `addr_of` to handle rvalue expressions safely via temp vars.
    pub(super) fn wrap_borrow_arg(&self, expr: String, ast_expr: &Expr, ownership: crate::parser::ast::Ownership) -> String {
        if matches!(ownership, crate::parser::ast::Ownership::MutableBorrow) {
            // self in equip methods is already a pointer — don't double-address
            if matches!(ast_expr, Expr::SelfExpr) && self.current_self_type.is_some() {
                return expr;
            }
            addr_of(&expr, ast_expr)
        } else {
            expr
        }
    }

    /// Resolve call arguments: reorder named args to match param order and
    /// fill in default values for missing optional params.
    pub(super) fn resolve_call_args(
        &mut self,
        callee: &Spanned<Expr>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> Vec<String> {
        let has_named = args.iter().any(|a| a.node.name.is_some());
        let func_info = if let Expr::Identifier(cname) = &callee.node {
            self.resolution_map.get(&callee.span.start)
                .filter(|def_id| self.scopes.get_def(**def_id).name == *cname)
                .and_then(|def_id| self.function_info.get(def_id))
        } else {
            None
        };
        let has_defaults = func_info.map_or(false, |fi| fi.param_defaults.iter().any(|d| d.is_some()));

        if (!has_named && !has_defaults) || func_info.is_none() {
            // Simple positional — wrap with & for MutableBorrow args, coerce String→str
            let param_type_ids: Vec<Option<crate::semantic::ids::TypeId>> = func_info
                .map(|fi| fi.param_type_ids.clone())
                .unwrap_or_default();
            return args.iter().enumerate().map(|(i, a)| {
                let expr = self.gen_expr(&a.node.value);
                let ptid = param_type_ids.get(i).copied().flatten();
                let expr = self.coerce_arg_to_str(expr, &a.node.value, ptid);
                let expr = self.coerce_arg_to_fn(expr, &a.node.value, ptid);
                self.wrap_borrow_arg(expr, &a.node.value.node, a.node.ownership)
            }).collect();
        }

        let fi = func_info.unwrap();
        let param_names = &fi.param_names;
        let param_defaults = &fi.param_defaults;

        // Build a slot for each param
        let mut slots: Vec<Option<String>> = vec![None; param_names.len()];

        // Place positional args first, then named args
        let mut positional_idx = 0;
        for arg in args {
            if let Some(ref name) = arg.node.name {
                if let Some(pos) = param_names.iter().position(|pn| pn == &name.node) {
                    let expr = self.gen_expr(&arg.node.value);
                    slots[pos] = Some(self.wrap_borrow_arg(expr, &arg.node.value.node, arg.node.ownership));
                }
            } else {
                if positional_idx < slots.len() {
                    let expr = self.gen_expr(&arg.node.value);
                    slots[positional_idx] = Some(self.wrap_borrow_arg(expr, &arg.node.value.node, arg.node.ownership));
                }
                positional_idx += 1;
            }
        }

        // Fill missing slots with defaults
        for (i, slot) in slots.iter_mut().enumerate() {
            if slot.is_none() {
                if let Some(Some(default_expr)) = param_defaults.get(i) {
                    *slot = Some(self.gen_expr(default_expr));
                }
            }
        }

        slots.into_iter().map(|s| s.unwrap_or_else(|| "0".to_string())).collect()
    }

    /// Generate a method call: `receiver.method(args)` → `Type__method(&receiver, args)`
    pub(super) fn gen_method_call(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &Spanned<String>,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let method_name = method.node.as_str();

        // Centralized move-zeroing for consumed method arguments
        self.queue_method_arg_move_zeros(receiver, method, args);

        // Check if receiver is a trait object → vtable dispatch
        if let Some(_trait_name) = self.resolve_trait_object_type(receiver) {
            let recv = self.gen_expr(receiver);
            let mut all_args = vec![format!("{recv}.data")];
            for arg in args {
                all_args.push(self.gen_expr(&arg.node.value));
            }
            return format!("{recv}.vtable->{method_name}({})", all_args.join(", "));
        }

        // Check if receiver is a built-in collection or primitive type
        if let Some(builtin_code) = self.try_gen_builtin_method(receiver, method_name, args) {
            return builtin_code;
        }

        // Check if receiver is a type name (static method call like Point.origin())
        if let Expr::Identifier(name) = &receiver.node {
            // Handle File.open(path) and File.create(path) as static constructors
            if name == "File" {
                match method_name {
                    "open" => {
                        if let Some(arg) = args.first() {
                            let path_arg = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_file_open({path_arg}, \"r\")");
                        }
                    }
                    "create" => {
                        if let Some(arg) = args.first() {
                            let path_arg = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_file_open({path_arg}, \"w\")");
                        }
                    }
                    _ => {}
                }
            }

            let is_primitive_type = matches!(name.as_str(),
                "int" | "int8" | "int16" | "int32" | "int64"
                | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
                | "float" | "float32" | "float64"
                | "bool" | "char" | "str" | "String"
            );
            let is_type = is_primitive_type || self
                .resolution_map
                .get(&receiver.span.start)
                .filter(|did| self.scopes.get_def(**did).name == *name)
                .map(|def_id| self.scopes.get_def(*def_id))
                .or_else(|| {
                    self.scoped_lookup(name)
                        .map(|def_id| self.scopes.get_def(def_id))
                })
                .map_or(false, |def| {
                    matches!(def.kind, DefKind::Struct | DefKind::Enum)
                });

            if is_type {
                // Parseable trait: Type.parse(s) → fallible parse returning Option[T]
                if method_name == "parse" {
                    return self.gen_parse_call(name, args);
                }
                // Default trait: Type.default() → inline zero or trait function call
                if method_name == "default" {
                    return self.gen_default_call(name);
                }
                // From trait: Type.from(value) → From__argtype_for_Type__from(value)
                if method_name == "from" {
                    let trait_type_args: Vec<String> = args.first()
                        .map(|a| vec![self.infer_c_type_from_expr(&a.node.value.node)])
                        .unwrap_or_default();
                    let func = c_mangle::mangle_trait_method("From", name, "from", &trait_type_args);
                    let arg_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    return format!("{func}({})", arg_exprs.join(", "));
                }
                // TryFrom trait: Type.try_from(value) → TryFrom__argtype_for_Type__try_from(value)
                if method_name == "try_from" {
                    let trait_type_args: Vec<String> = args.first()
                        .map(|a| vec![self.infer_c_type_from_expr(&a.node.value.node)])
                        .unwrap_or_default();
                    let func = c_mangle::mangle_trait_method("TryFrom", name, "try_from", &trait_type_args);
                    let arg_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    return format!("{func}({})", arg_exprs.join(", "));
                }
                let mangled = c_mangle::mangle_method(name, method_name);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{mangled}({})", arg_exprs.join(", "));
            }
        }

        // Regex/Match method dispatch (Declaration methods needing Option/wrapping)
        {
            let type_name = self.infer_receiver_type(receiver);
            if type_name == "Regex" || type_name == "GorgetRegex" {
                let recv = self.gen_expr(receiver);
                let recv_ref = addr_of(&recv, &receiver.node);
                match method_name {
                    "find" => {
                        if let Some(arg) = args.first() {
                            let subj = self.gen_str_arg(&arg.node.value);
                            let opt = self.register_generic("Option", &["GorgetRegexMatch".into()], super::GenericInstanceKind::Enum);
                            let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                            let ctor_none = c_mangle::mangle_variant(&opt, "None");
                            return format!(
                                "({{ GorgetRegexMatch __rm = gorget_regex_find({recv_ref}, {subj}, 0); \
                                __rm.start >= 0 ? {ctor_some}(__rm) : {ctor_none}(); }})"
                            );
                        }
                    }
                    "find_at" => {
                        if args.len() >= 2 {
                            let subj = self.gen_str_arg(&args[0].node.value);
                            let pos = self.gen_expr(&args[1].node.value);
                            let opt = self.register_generic("Option", &["GorgetRegexMatch".into()], super::GenericInstanceKind::Enum);
                            let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                            let ctor_none = c_mangle::mangle_variant(&opt, "None");
                            return format!(
                                "({{ GorgetRegexMatch __rm = gorget_regex_find({recv_ref}, {subj}, {pos}); \
                                __rm.start >= 0 ? {ctor_some}(__rm) : {ctor_none}(); }})"
                            );
                        }
                    }
                    "fullmatch" => {
                        if let Some(arg) = args.first() {
                            let subj = self.gen_str_arg(&arg.node.value);
                            let opt = self.register_generic("Option", &["GorgetRegexMatch".into()], super::GenericInstanceKind::Enum);
                            let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                            let ctor_none = c_mangle::mangle_variant(&opt, "None");
                            return format!(
                                "({{ GorgetRegexMatch __rm = gorget_regex_fullmatch({recv_ref}, {subj}); \
                                __rm.start >= 0 ? {ctor_some}(__rm) : {ctor_none}(); }})"
                            );
                        }
                    }
                    "find_all" => {
                        if let Some(arg) = args.first() {
                            let subj = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_regex_find_all({recv_ref}, {subj})");
                        }
                    }
                    "replace" => {
                        if args.len() >= 2 {
                            let subj = self.gen_str_arg(&args[0].node.value);
                            let repl = self.gen_str_arg(&args[1].node.value);
                            return format!("gorget_regex_replace({recv_ref}, {subj}, {repl})");
                        }
                    }
                    "split" => {
                        if let Some(arg) = args.first() {
                            let subj = self.gen_str_arg(&arg.node.value);
                            return format!("gorget_regex_split({recv_ref}, {subj}, 0)");
                        }
                    }
                    "splitn" => {
                        if args.len() >= 2 {
                            let subj = self.gen_str_arg(&args[0].node.value);
                            let limit = self.gen_expr(&args[1].node.value);
                            return format!("gorget_regex_split({recv_ref}, {subj}, {limit})");
                        }
                    }
                    _ => {} // fall through to extern_symbols for is_match, replace_all, etc.
                }
            }
            if type_name == "Match" || type_name == "GorgetRegexMatch" {
                let recv = self.gen_expr(receiver);
                let recv_ref = addr_of(&recv, &receiver.node);
                match method_name {
                    "group" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            let opt = self.register_generic("Option", &["const char*".into()], super::GenericInstanceKind::Enum);
                            let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                            let ctor_none = c_mangle::mangle_variant(&opt, "None");
                            return format!(
                                "({{ const char* __mg = gorget_regex_match_group({recv_ref}, {n}); \
                                __mg ? {ctor_some}(__mg) : {ctor_none}(); }})"
                            );
                        }
                    }
                    "group_by_name" => {
                        if let Some(arg) = args.first() {
                            let name = self.gen_str_arg(&arg.node.value);
                            let opt = self.register_generic("Option", &["const char*".into()], super::GenericInstanceKind::Enum);
                            let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                            let ctor_none = c_mangle::mangle_variant(&opt, "None");
                            return format!(
                                "({{ const char* __mg = gorget_regex_match_group_by_name({recv_ref}, {name}); \
                                __mg ? {ctor_some}(__mg) : {ctor_none}(); }})"
                            );
                        }
                    }
                    _ => {} // fall through to extern_symbols for text, start, end_pos, etc.
                }
            }
        }

        // UdpSocket method dispatch (Declaration methods needing Result-wrapping)
        {
            let type_name = self.infer_receiver_type(receiver);
            if type_name == "UdpSocket" || type_name == "GorgetUdpSocket" {
                let recv = self.gen_expr(receiver);
                let recv_ref = addr_of(&recv, &receiver.node);
                match method_name {
                    "sendto" => {
                        if args.len() >= 3 {
                            let data = self.gen_expr(&args[0].node.value);
                            let data_addr = addr_of(&data, &args[0].node.value.node);
                            let host = self.gen_str_arg(&args[1].node.value);
                            let port = self.gen_expr(&args[2].node.value);
                            let result_type = self.register_generic("Result", &["int64_t".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ int64_t __us = gorget_udp_sendto({recv_ref}, {data_addr}, {host}, {port}); \
                                const char* __ue = gorget_udp_last_error(); \
                                __ue ? {err_ctor}(__ue) : {ok_ctor}(__us); }})"
                            );
                        }
                    }
                    "recvfrom" => {
                        if let Some(arg) = args.first() {
                            let max_bytes = self.gen_expr(&arg.node.value);
                            let result_type = self.register_generic("Result", &["GorgetUdpPacket".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ GorgetUdpPacket __up = gorget_udp_recvfrom({recv_ref}, {max_bytes}); \
                                const char* __ue = gorget_udp_last_error(); \
                                __ue ? {err_ctor}(__ue) : {ok_ctor}(__up); }})"
                            );
                        }
                    }
                    "join_multicast" => {
                        if let Some(arg) = args.first() {
                            let group = self.gen_str_arg(&arg.node.value);
                            let result_type = self.register_generic("Result", &["bool".into(), "const char*".into()], super::GenericInstanceKind::Enum);
                            let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                            let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                            return format!(
                                "({{ bool __uj = gorget_udp_join_multicast({recv_ref}, {group}); \
                                const char* __ue = gorget_udp_last_error(); \
                                __ue ? {err_ctor}(__ue) : {ok_ctor}(__uj); }})"
                            );
                        }
                    }
                    _ => {} // fall through to extern_symbols for poll, set_nonblocking, etc.
                }
            }
        }

        // Check if method has an extern binding (e.g. `extern int status(self) = "gorget_http_response_status"`)
        {
            let type_name = self.infer_receiver_type(receiver);
            if let Some(binding) = self.extern_symbols.get(&(type_name.clone(), method_name.to_string())).cloned() {
                let is_pointer_param = matches!(&receiver.node, Expr::Identifier(name) if self.pointer_params.contains(&c_mangle::escape_keyword(name)));
                let is_self_ptr = (self.current_self_type.is_some() && matches!(receiver.node, Expr::SelfExpr))
                    || is_pointer_param;
                let recv = if is_pointer_param {
                    if let Expr::Identifier(name) = &receiver.node {
                        c_mangle::escape_keyword(name)
                    } else { unreachable!() }
                } else {
                    self.gen_expr(receiver)
                };
                let c_symbol = &binding.c_symbol;
                let needs_temp = !is_lvalue(&receiver.node);
                if needs_temp {
                    let arg_exprs: Vec<String> = args.iter().enumerate().map(|(i, a)| {
                        let expr = self.gen_expr(&a.node.value);
                        if binding.params_need_ref.get(i) == Some(&true) {
                            addr_of(&expr, &a.node.value.node)
                        } else {
                            expr
                        }
                    }).collect();
                    let mut call_args = String::from("&__recv");
                    for a in &arg_exprs {
                        call_args.push_str(", ");
                        call_args.push_str(a);
                    }
                    return format!("({{ __typeof__({recv}) __recv = {recv}; {c_symbol}({call_args}); }})");
                } else {
                    let self_arg = if is_self_ptr { recv.clone() } else { format!("&{recv}") };
                    let mut all_args = vec![self_arg];
                    for (i, arg) in args.iter().enumerate() {
                        let expr = self.gen_expr(&arg.node.value);
                        if binding.params_need_ref.get(i) == Some(&true) {
                            all_args.push(addr_of(&expr, &arg.node.value.node));
                        } else {
                            all_args.push(expr);
                        }
                    }
                    return format!("{c_symbol}({})", all_args.join(", "));
                }
            }
        }

        // Check if the receiver is a pointer param (already a pointer, pass directly).
        let is_pointer_param = matches!(&receiver.node, Expr::Identifier(name) if self.pointer_params.contains(&c_mangle::escape_keyword(name)));
        let recv = if is_pointer_param {
            // Don't dereference — we need the raw pointer for method calls
            if let Expr::Identifier(name) = &receiver.node {
                c_mangle::escape_keyword(name)
            } else {
                unreachable!()
            }
        } else {
            self.gen_expr(receiver)
        };
        // Try to figure out the receiver type for mangling
        let type_name = self.infer_receiver_type(receiver);
        // Use mangled name for generic types (e.g., Pair[int] → Pair__int64_t)
        let mangled_type = self.infer_receiver_mangled_type(receiver);
        // Check if this method comes from a trait impl (not inherent)
        let mangled = if let Some((trait_name, trait_type_args)) = self.find_trait_for_method(&type_name, method_name) {
            c_mangle::mangle_trait_method(&trait_name, &mangled_type, method_name, &trait_type_args)
        } else {
            c_mangle::mangle_method(&mangled_type, method_name)
        };

        // For non-lvalue receivers (e.g. function calls), we can't take `&recv`
        // directly. Use a GCC statement expression to stash the result in a temp.
        let needs_temp = !is_lvalue(&receiver.node);
        // Inside a method body, `self` is already a pointer (const T* self),
        // so pass it directly instead of taking &self.
        let is_self_ptr = (self.current_self_type.is_some() && matches!(receiver.node, Expr::SelfExpr))
            || is_pointer_param;
        if needs_temp {
            let arg_exprs: Vec<String> = args.iter().map(|a| {
                let expr = self.gen_expr(&a.node.value);
                self.wrap_borrow_arg(expr, &a.node.value.node, a.node.ownership)
            }).collect();
            let mut call_args = format!("&__recv");
            for a in &arg_exprs {
                call_args.push_str(", ");
                call_args.push_str(a);
            }
            format!("({{ __typeof__({recv}) __recv = {recv}; {mangled}({call_args}); }})")
        } else {
            let self_arg = if is_self_ptr {
                recv.clone()
            } else {
                format!("&{recv}")
            };
            let mut all_args = vec![self_arg];
            for arg in args {
                let expr = self.gen_expr(&arg.node.value);
                all_args.push(self.wrap_borrow_arg(expr, &arg.node.value.node, arg.node.ownership));
            }
            format!("{mangled}({})", all_args.join(", "))
        }
    }

    /// Try to generate code for a built-in method call on a collection or primitive type.
    /// Returns `Some(code)` if the receiver is a known built-in type, `None` otherwise.
    pub(super) fn try_gen_builtin_method(
        &mut self,
        receiver: &Spanned<Expr>,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> Option<String> {
        let type_name = self.infer_receiver_type(receiver);

        // Also check the C-level type for cases where infer_receiver_type
        // returns the Gorget name vs the C type
        let c_type = self.infer_receiver_c_type(receiver);

        let is_vector = matches!(type_name.as_str(), "Vector" | "List" | "Array")
            || c_type.as_deref() == Some("GorgetArray")
            || self.is_vector_expr(receiver);
        let is_map = matches!(type_name.as_str(), "Dict" | "HashMap")
            || c_type.as_deref().map_or(false, |t| t.starts_with("GorgetMap__") || t.starts_with("GorgetDict__"));
        let is_set = matches!(type_name.as_str(), "Set" | "HashSet")
            || c_type.as_deref() == Some("GorgetSet");
        let is_string = matches!(type_name.as_str(), "str" | "String")
            || matches!(c_type.as_deref(), Some("const char*") | Some("GorgetString"));
        let is_option = type_name == "Option"
            || c_type.as_deref().map_or(false, |t| t.starts_with("Option__"));
        let is_result = type_name == "Result"
            || c_type.as_deref().map_or(false, |t| t.starts_with("Result__"));
        let is_box = type_name == "Box";
        let is_file = (type_name == "File" || c_type.as_deref() == Some("GorgetFile"))
            && matches!(method_name, "open" | "create");
        let is_iterator = !is_vector && !is_map && !is_set && !is_string
            && !is_option && !is_result && !is_box && !is_file
            && matches!(method_name, "collect" | "filter" | "map" | "fold")
            && self.traits.impls.iter().any(|i|
                i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterator")
            );

        let is_char = c_type.as_deref() == Some("char")
            && matches!(method_name, "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace" | "is_hex_digit" | "to_upper" | "to_lower" | "is_upper" | "is_lower");

        let is_channel = type_name == "Channel"
            || c_type.as_deref() == Some("GorgetChannel*");

        let is_arena = type_name == "Arena"
            || c_type.as_deref() == Some("GorgetArena*");

        let is_tracking_allocator = type_name == "TrackingAllocator"
            || c_type.as_deref() == Some("GorgetTrackingAllocator*");

        let is_primitive_hashable = !is_vector && !is_map && !is_set && !is_string
            && !is_option && !is_result && !is_box && !is_file && !is_iterator
            && !is_char && !is_channel && !is_arena && !is_tracking_allocator
            && method_name == "hash"
            && matches!(c_type.as_deref(), Some(
                "int64_t" | "int8_t" | "int16_t" | "int32_t" |
                "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t" |
                "double" | "float" |
                "bool" | "char32_t"
            ));

        if !is_vector && !is_map && !is_set && !is_string && !is_option && !is_result && !is_box && !is_file && !is_iterator && !is_primitive_hashable && !is_char && !is_channel && !is_arena && !is_tracking_allocator {
            return None;
        }

        let recv = self.gen_expr(receiver);
        let needs_temp = !is_lvalue(&receiver.node);

        if is_char {
            return match method_name {
                "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace" | "is_hex_digit" | "is_upper" | "is_lower" => {
                    let c_func = match method_name {
                        "is_alpha" => "isalpha",
                        "is_digit" => "isdigit",
                        "is_alphanumeric" => "isalnum",
                        "is_whitespace" => "isspace",
                        "is_hex_digit" => "isxdigit",
                        "is_upper" => "isupper",
                        "is_lower" => "islower",
                        _ => unreachable!(),
                    };
                    Some(format!("((bool){c_func}((int)({recv})))"))
                }
                "to_upper" => Some(format!("((char)toupper((int)({recv})))")),
                "to_lower" => Some(format!("((char)tolower((int)({recv})))")),
                _ => unreachable!(),
            };
        }
        if is_primitive_hashable {
            return Some(format!(
                "({{ __typeof__({recv}) __hv = {recv}; (int64_t)__gorget_fnv1a(&__hv, sizeof(__hv)); }})"
            ));
        }
        if is_iterator {
            return Some(self.gen_iterator_method(&recv, method_name, args, receiver, &type_name));
        }
        if is_vector {
            return Some(self.gen_vector_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_map {
            return Some(self.gen_map_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_set {
            return Some(self.gen_set_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_string {
            // Coerce GorgetString receiver to const char* for str methods
            let str_recv = if c_type.as_deref() == Some("GorgetString") {
                self.coerce_string_to_str(&recv)
            } else {
                recv.clone()
            };
            let is_owned = c_type.as_deref() == Some("GorgetString");
            return self.gen_string_method(&str_recv, method_name, args, needs_temp, is_owned);
        }
        if is_option {
            return Some(self.gen_option_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_result {
            return Some(self.gen_result_method(&recv, method_name, args, receiver, needs_temp));
        }
        if is_box {
            return Some(self.gen_box_method(&recv, method_name, args));
        }
        if is_file {
            return Some(self.gen_file_method(&recv, method_name, args, needs_temp));
        }
        if is_channel {
            return Some(self.gen_channel_method(&recv, method_name, args, receiver));
        }
        if is_arena {
            return Some(self.gen_arena_method(&recv, method_name));
        }
        if is_tracking_allocator {
            return Some(self.gen_tracking_method(&recv, method_name));
        }
        None
    }

    fn gen_channel_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
    ) -> String {
        match method_name {
            "send" => {
                let val = self.gen_expr(&args[0].node.value);
                format!("({{ __typeof__({val}) __ch_tmp = {val}; gorget_channel_send({recv}, &__ch_tmp); }})")
            }
            "recv" => {
                // Infer the element type from the receiver's generic type arg
                let elem_c = if let Some(tid) = self.resolve_expr_type_id(receiver) {
                    if let crate::semantic::types::ResolvedType::Generic(_, type_args) = self.types.get(tid) {
                        if let Some(&elem_tid) = type_args.first() {
                            crate::codegen::c_types::type_id_to_c(elem_tid, self.types, self.scopes)
                        } else {
                            "int64_t".to_string()
                        }
                    } else {
                        "int64_t".to_string()
                    }
                } else {
                    "int64_t".to_string()
                };
                format!("({{ {elem_c} __ch_tmp; gorget_channel_recv({recv}, &__ch_tmp); __ch_tmp; }})")
            }
            "close" => format!("gorget_channel_close({recv})"),
            _ => format!("/* unknown channel method: {method_name} */ 0"),
        }
    }

    fn gen_arena_method(&self, recv: &str, method_name: &str) -> String {
        match method_name {
            "bytes_used" => format!("gorget_arena_bytes_used({recv})"),
            "reset" => format!("gorget_arena_reset({recv})"),
            "destroy" => format!("gorget_arena_destroy({recv})"),
            _ => format!("/* unknown arena method: {method_name} */ 0"),
        }
    }

    fn gen_tracking_method(&self, recv: &str, method_name: &str) -> String {
        match method_name {
            "alloc_count" => format!("gorget_tracking_alloc_count({recv})"),
            "free_count" => format!("gorget_tracking_free_count({recv})"),
            "bytes_allocated" => format!("gorget_tracking_bytes_allocated({recv})"),
            "bytes_freed" => format!("gorget_tracking_bytes_freed({recv})"),
            "current_bytes" => format!("gorget_tracking_current_bytes({recv})"),
            "peak_bytes" => format!("gorget_tracking_peak_bytes({recv})"),
            "realloc_count" => format!("gorget_tracking_realloc_count({recv})"),
            "report" => format!("gorget_tracking_report({recv})"),
            "reset" => format!("gorget_tracking_reset({recv})"),
            "destroy" => format!("gorget_tracking_destroy({recv})"),
            _ => format!("/* unknown tracking allocator method: {method_name} */ 0"),
        }
    }

    /// Generate code for `needle in collection` expressions.
    /// Desugars to the appropriate `.contains()` call per collection type.
    pub(super) fn gen_in_operator(&mut self, needle: &Spanned<Expr>, collection: &Spanned<Expr>) -> String {
        let type_name = self.infer_receiver_type(collection);
        let c_type = self.infer_receiver_c_type(collection);

        let is_vector = matches!(type_name.as_str(), "Vector" | "List" | "Array")
            || c_type.as_deref() == Some("GorgetArray")
            || self.is_vector_expr(collection);
        let is_map = matches!(type_name.as_str(), "Dict" | "HashMap")
            || c_type.as_deref().map_or(false, |t| t.starts_with("GorgetMap__") || t.starts_with("GorgetDict__"));
        let is_set = matches!(type_name.as_str(), "Set" | "HashSet")
            || c_type.as_deref() == Some("GorgetSet");
        let is_string = matches!(type_name.as_str(), "str" | "String")
            || matches!(c_type.as_deref(), Some("const char*") | Some("GorgetString"));

        let coll = self.gen_expr(collection);
        let elem = self.gen_expr(needle);
        let needs_temp = !is_lvalue(&collection.node);

        let coll_ref = if needs_temp {
            format!("({{ __typeof__({coll}) __recv = {coll}; &__recv; }})")
        } else {
            format!("&{coll}")
        };

        if is_vector {
            let elem_type = self.infer_vector_elem_type(collection);
            format!("({{ {elem_type} __needle = {elem}; gorget_array_contains({coll_ref}, &__needle, sizeof({elem_type})); }})")
        } else if is_map {
            let (key_type, val_type) = self.infer_map_kv_types(collection);
            let base = if self.is_ordered_map_expr(collection) { "GorgetDict" } else { "GorgetMap" };
            let mangled = c_mangle::mangle_generic(base, &[key_type, val_type]);
            format!("{mangled}__contains({coll_ref}, {elem})")
        } else if is_set {
            let elem_type = self.infer_c_type_from_expr(&needle.node);
            format!("({{ {elem_type} __needle = {elem}; gorget_set_contains({coll_ref}, &__needle); }})")
        } else if is_string {
            // Coerce GorgetString to const char* for strstr
            let coll_str = if c_type.as_deref() == Some("GorgetString") {
                self.coerce_string_to_str(&coll)
            } else {
                coll.clone()
            };
            format!("(strstr({coll_str}, {elem}) != NULL)")
        } else {
            format!("/* unsupported `in` for type {type_name} */ false")
        }
    }

    /// Generate inline default value for `Type.default()` calls.
    fn gen_default_call(&mut self, type_name: &str) -> String {
        match type_name {
            "int" | "int64" => "((int64_t)0)".to_string(),
            "int8" => "((int8_t)0)".to_string(),
            "int16" => "((int16_t)0)".to_string(),
            "int32" => "((int32_t)0)".to_string(),
            "uint" | "uint64" => "((uint64_t)0)".to_string(),
            "uint8" => "((uint8_t)0)".to_string(),
            "uint16" => "((uint16_t)0)".to_string(),
            "uint32" => "((uint32_t)0)".to_string(),
            "float" | "float64" => "0.0".to_string(),
            "float32" => "0.0f".to_string(),
            "bool" => "false".to_string(),
            "char" => "((char)0)".to_string(),
            "str" => "\"\"".to_string(),
            "String" => "gorget_string_new(\"\")".to_string(),
            _ => {
                let func = c_mangle::mangle_trait_method("Default", type_name, "default", &[]);
                format!("{func}()")
            }
        }
    }

    /// Generate `Type.parse(s)` → fallible parse returning `Option[T]`.
    fn gen_parse_call(
        &mut self,
        type_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        let arg_str = args.first()
            .map(|a| self.gen_str_arg(&a.node.value))
            .unwrap_or_else(|| "\"\"".to_string());

        // Map Gorget type name → C type for Option registration
        let c_type = match type_name {
            "int" | "int64" => "int64_t",
            "int8" => "int8_t",
            "int16" => "int16_t",
            "int32" => "int32_t",
            "uint" | "uint64" => "uint64_t",
            "uint8" => "uint8_t",
            "uint16" => "uint16_t",
            "uint32" => "uint32_t",
            "float" | "float64" => "double",
            "float32" => "float",
            _ => {
                // User-defined types: call Parseable_for_Type__parse(s)
                let func = c_mangle::mangle_trait_method("Parseable", type_name, "parse", &[]);
                return format!("{func}({arg_str})");
            }
        };

        let opt = self.register_generic("Option", &[c_type.into()], super::GenericInstanceKind::Enum);
        let ctor_some = c_mangle::mangle_variant(&opt, "Some");
        let ctor_none = c_mangle::mangle_variant(&opt, "None");

        // Determine which runtime parser to call and the cast expression
        let is_int = matches!(type_name,
            "int" | "int8" | "int16" | "int32" | "int64"
            | "uint" | "uint8" | "uint16" | "uint32" | "uint64"
        );

        if is_int {
            format!(
                "({{ GorgetParseIntResult __pr = gorget_try_parse_int({arg_str}); \
                __pr.ok ? {ctor_some}(({c_type})__pr.value) : {ctor_none}(); }})"
            )
        } else {
            format!(
                "({{ GorgetParseFloatResult __pr = gorget_try_parse_float({arg_str}); \
                __pr.ok ? {ctor_some}(({c_type})__pr.value) : {ctor_none}(); }})"
            )
        }
    }
}
