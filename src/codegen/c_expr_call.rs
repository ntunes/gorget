/// Call-related expression codegen: function calls, method calls, and `in` operator.
use crate::parser::ast::Expr;
use crate::semantic::scope::DefKind;
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::c_expr::{is_lvalue, addr_of};
use super::CodegenContext;

impl CodegenContext<'_> {
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
                    if args.is_empty() {
                        return "gorget_string_new(\"\")".to_string();
                    }
                    let arg = self.gen_expr(&args[0].node.value);
                    let arg_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                    if arg_type == "GorgetString" {
                        return format!("gorget_string_new({arg}.data)");
                    }
                    return format!("gorget_string_new({arg})");
                }
                "len" => {
                    if let Some(arg) = args.first() {
                        let a = self.gen_expr(&arg.node.value);
                        return format!("(sizeof({a}) / sizeof({a}[0]))");
                    }
                }
                _ => {}
            }

            // Stdlib functions — only dispatch if resolved to a stdlib (dummy-span) def
            if self.is_stdlib_call(name) {
                match name.as_str() {
                    "read_file" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_read_file({path})");
                        }
                    }
                    "write_file" | "append_file" => {
                        let func = if name == "write_file" { "gorget_write_file" } else { "gorget_append_file" };
                        if args.len() >= 2 {
                            let path = self.gen_expr(&args[0].node.value);
                            let content = self.gen_expr(&args[1].node.value);
                            return format!("{func}({path}, {content})");
                        }
                    }
                    "file_exists" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_file_exists({path})");
                        }
                    }
                    "delete_file" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_delete_file({path})");
                        }
                    }
                    "mkdir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_mkdir({path})");
                        }
                    }
                    "rmdir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_rmdir({path})");
                        }
                    }
                    "rename" => {
                        if args.len() >= 2 {
                            let old = self.gen_expr(&args[0].node.value);
                            let new = self.gen_expr(&args[1].node.value);
                            return format!("gorget_rename({old}, {new})");
                        }
                    }
                    "copy_file" => {
                        if args.len() >= 2 {
                            let src = self.gen_expr(&args[0].node.value);
                            let dst = self.gen_expr(&args[1].node.value);
                            return format!("gorget_copy_file({src}, {dst})");
                        }
                    }
                    "file_size" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_file_size({path})");
                        }
                    }
                    "is_dir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_is_dir({path})");
                        }
                    }
                    "path_parent" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_expr(&arg.node.value);
                            return format!("gorget_path_parent({p})");
                        }
                    }
                    "path_basename" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_expr(&arg.node.value);
                            return format!("gorget_path_basename({p})");
                        }
                    }
                    "path_extension" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_expr(&arg.node.value);
                            return format!("gorget_path_extension({p})");
                        }
                    }
                    "path_stem" => {
                        if let Some(arg) = args.first() {
                            let p = self.gen_expr(&arg.node.value);
                            return format!("gorget_path_stem({p})");
                        }
                    }
                    "path_join" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            return format!("gorget_path_join({a}, {b})");
                        }
                    }
                    "readdir" => {
                        if let Some(arg) = args.first() {
                            let path = self.gen_expr(&arg.node.value);
                            return format!("gorget_readdir({path})");
                        }
                    }
                    "args" => {
                        return "gorget_args()".to_string();
                    }
                    "exec" => {
                        if let Some(arg) = args.first() {
                            let cmd = self.gen_expr(&arg.node.value);
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
                    "getchar" => return "gorget_getchar()".to_string(),
                    "readline" => return "gorget_readline()".to_string(),
                    "input" => {
                        if let Some(arg) = args.first() {
                            let prompt = self.gen_expr(&arg.node.value);
                            return format!("gorget_input({prompt})");
                        }
                    }
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
                            let s = self.gen_expr(&arg.node.value);
                            return format!("gorget_parse_int({s})");
                        }
                    }
                    "parse_float" => {
                        if let Some(arg) = args.first() {
                            let s = self.gen_expr(&arg.node.value);
                            return format!("gorget_parse_float({s})");
                        }
                    }
                    "int_to_str" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            return format!("gorget_string_new(gorget_int_to_str({n}))");
                        }
                    }
                    "float_to_str" => {
                        if let Some(arg) = args.first() {
                            let x = self.gen_expr(&arg.node.value);
                            return format!("gorget_string_new(gorget_float_to_str({x}))");
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
                            return format!("gorget_string_new(gorget_char_to_str({c}))");
                        }
                    }
                    "getenv" => {
                        if let Some(arg) = args.first() {
                            let name_expr = self.gen_expr(&arg.node.value);
                            return format!("gorget_getenv({name_expr})");
                        }
                    }
                    "setenv" => {
                        if args.len() >= 2 {
                            let name_expr = self.gen_expr(&args[0].node.value);
                            let val_expr = self.gen_expr(&args[1].node.value);
                            return format!("gorget_setenv({name_expr}, {val_expr})");
                        }
                    }
                    "getcwd" => return "gorget_getcwd()".to_string(),
                    "platform" => return "gorget_platform()".to_string(),
                    // std.math — integer
                    "abs" => {
                        if let Some(arg) = args.first() {
                            let x = self.gen_expr(&arg.node.value);
                            return format!("gorget_abs({x})");
                        }
                    }
                    "min" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            return format!("gorget_min({a}, {b})");
                        }
                    }
                    "max" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            return format!("gorget_max({a}, {b})");
                        }
                    }
                    // std.math — float (1-arg)
                    "sqrt" | "floor" | "ceil" | "round" | "log" | "log2" | "log10"
                    | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "fabs" => {
                        if let Some(arg) = args.first() {
                            let x = self.gen_expr(&arg.node.value);
                            return format!("gorget_{name}({x})");
                        }
                    }
                    // std.math — float (2-arg)
                    "pow" | "atan2" | "fmin" | "fmax" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let b = self.gen_expr(&args[1].node.value);
                            return format!("gorget_{name}({a}, {b})");
                        }
                    }
                    // std.process
                    "exec_output" => {
                        if let Some(arg) = args.first() {
                            let cmd = self.gen_expr(&arg.node.value);
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
                            let title = self.gen_expr(&args[0].node.value);
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
                            let path = self.gen_expr(&args[1].node.value);
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
                            let path = self.gen_expr(&args[0].node.value);
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
                            let text = self.gen_expr(&args[2].node.value);
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
                            let text = self.gen_expr(&args[2].node.value);
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
                            let text = self.gen_expr(&args[1].node.value);
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
                    // std.crypto
                    "crypto_sha256" => {
                        if let Some(arg) = args.first() {
                            let data = self.gen_expr(&arg.node.value);
                            let data_addr = addr_of(&data, &arg.node.value.node);
                            return format!("gorget_crypto_sha256({data_addr})");
                        }
                    }
                    "crypto_sha1" => {
                        if let Some(arg) = args.first() {
                            let data = self.gen_expr(&arg.node.value);
                            let data_addr = addr_of(&data, &arg.node.value.node);
                            return format!("gorget_crypto_sha1({data_addr})");
                        }
                    }
                    "crypto_hmac" => {
                        if args.len() >= 3 {
                            let algo = self.gen_expr(&args[0].node.value);
                            let key = self.gen_expr(&args[1].node.value);
                            let key_addr = addr_of(&key, &args[1].node.value.node);
                            let data = self.gen_expr(&args[2].node.value);
                            let data_addr = addr_of(&data, &args[2].node.value.node);
                            return format!("gorget_crypto_hmac({algo}, {key_addr}, {data_addr})");
                        }
                    }
                    "crypto_aes_ctr_new" => {
                        if args.len() >= 2 {
                            let key = self.gen_expr(&args[0].node.value);
                            let key_addr = addr_of(&key, &args[0].node.value.node);
                            let iv = self.gen_expr(&args[1].node.value);
                            let iv_addr = addr_of(&iv, &args[1].node.value.node);
                            return format!("gorget_crypto_aes_ctr_new({key_addr}, {iv_addr})");
                        }
                    }
                    "crypto_bn_from_bytes" => {
                        if let Some(arg) = args.first() {
                            let data = self.gen_expr(&arg.node.value);
                            let data_addr = addr_of(&data, &arg.node.value.node);
                            return format!("gorget_crypto_bn_from_bytes({data_addr})");
                        }
                    }
                    "crypto_bn_to_bytes" => {
                        if let Some(arg) = args.first() {
                            let bn = self.gen_expr(&arg.node.value);
                            let bn_addr = addr_of(&bn, &arg.node.value.node);
                            return format!("gorget_crypto_bn_to_bytes({bn_addr})");
                        }
                    }
                    "crypto_bn_mod_exp" => {
                        if args.len() >= 3 {
                            let base = self.gen_expr(&args[0].node.value);
                            let base_addr = addr_of(&base, &args[0].node.value.node);
                            let exp = self.gen_expr(&args[1].node.value);
                            let exp_addr = addr_of(&exp, &args[1].node.value.node);
                            let modulus = self.gen_expr(&args[2].node.value);
                            let mod_addr = addr_of(&modulus, &args[2].node.value.node);
                            return format!("gorget_crypto_bn_mod_exp({base_addr}, {exp_addr}, {mod_addr})");
                        }
                    }
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
                    "crypto_rsa_verify" => {
                        if args.len() >= 3 {
                            let key = self.gen_expr(&args[0].node.value);
                            let key_addr = addr_of(&key, &args[0].node.value.node);
                            let data = self.gen_expr(&args[1].node.value);
                            let data_addr = addr_of(&data, &args[1].node.value.node);
                            let sig = self.gen_expr(&args[2].node.value);
                            let sig_addr = addr_of(&sig, &args[2].node.value.node);
                            return format!("gorget_crypto_rsa_verify({key_addr}, {data_addr}, {sig_addr})");
                        }
                    }
                    "crypto_random_bytes" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            return format!("gorget_crypto_random_bytes({n})");
                        }
                    }
                    // std.net.socket
                    "socket_connect" => {
                        if args.len() >= 2 {
                            let host = self.gen_expr(&args[0].node.value);
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
                    // std.bytes
                    "bytes_from_str" => {
                        if let Some(arg) = args.first() {
                            let s = self.gen_expr(&arg.node.value);
                            return format!("gorget_bytes_from_str({s})");
                        }
                    }
                    "bytes_to_str" => {
                        if let Some(arg) = args.first() {
                            let b = self.gen_expr(&arg.node.value);
                            let b_addr = addr_of(&b, &arg.node.value.node);
                            return format!("gorget_bytes_to_str({b_addr})");
                        }
                    }
                    "bytes_from_hex" => {
                        if let Some(arg) = args.first() {
                            let s = self.gen_expr(&arg.node.value);
                            return format!("gorget_bytes_from_hex({s})");
                        }
                    }
                    "bytes_to_hex" => {
                        if let Some(arg) = args.first() {
                            let b = self.gen_expr(&arg.node.value);
                            let b_addr = addr_of(&b, &arg.node.value.node);
                            return format!("gorget_bytes_to_hex({b_addr})");
                        }
                    }
                    "bytes_write_u32_be" => {
                        if args.len() >= 3 {
                            let b = self.gen_expr(&args[0].node.value);
                            let b_addr = addr_of(&b, &args[0].node.value.node);
                            let offset = self.gen_expr(&args[1].node.value);
                            let value = self.gen_expr(&args[2].node.value);
                            return format!("gorget_bytes_write_u32_be({b_addr}, {offset}, {value})");
                        }
                    }
                    "bytes_read_u32_be" => {
                        if args.len() >= 2 {
                            let b = self.gen_expr(&args[0].node.value);
                            let b_addr = addr_of(&b, &args[0].node.value.node);
                            let offset = self.gen_expr(&args[1].node.value);
                            return format!("gorget_bytes_read_u32_be({b_addr}, {offset})");
                        }
                    }
                    "bytes_write_u16_be" => {
                        if args.len() >= 3 {
                            let b = self.gen_expr(&args[0].node.value);
                            let b_addr = addr_of(&b, &args[0].node.value.node);
                            let offset = self.gen_expr(&args[1].node.value);
                            let value = self.gen_expr(&args[2].node.value);
                            return format!("gorget_bytes_write_u16_be({b_addr}, {offset}, {value})");
                        }
                    }
                    "bytes_read_u16_be" => {
                        if args.len() >= 2 {
                            let b = self.gen_expr(&args[0].node.value);
                            let b_addr = addr_of(&b, &args[0].node.value.node);
                            let offset = self.gen_expr(&args[1].node.value);
                            return format!("gorget_bytes_read_u16_be({b_addr}, {offset})");
                        }
                    }
                    "bytes_concat" => {
                        if args.len() >= 2 {
                            let a = self.gen_expr(&args[0].node.value);
                            let a_addr = addr_of(&a, &args[0].node.value.node);
                            let b = self.gen_expr(&args[1].node.value);
                            let b_addr = addr_of(&b, &args[1].node.value.node);
                            return format!("gorget_bytes_concat({a_addr}, {b_addr})");
                        }
                    }
                    "bytes_slice" => {
                        if args.len() >= 3 {
                            let b = self.gen_expr(&args[0].node.value);
                            let b_addr = addr_of(&b, &args[0].node.value.node);
                            let start = self.gen_expr(&args[1].node.value);
                            let end = self.gen_expr(&args[2].node.value);
                            return format!("gorget_bytes_slice({b_addr}, {start}, {end})");
                        }
                    }
                    "random_bytes" => {
                        if let Some(arg) = args.first() {
                            let n = self.gen_expr(&arg.node.value);
                            return format!("gorget_random_bytes({n})");
                        }
                    }
                    // std.http.client
                    "get" | "post" | "put" | "delete" | "patch" | "head" => {
                        let method = name.as_str();
                        // Resolve named/default args: url, body, headers, timeout
                        let resolved = self.resolve_call_args(callee, args);
                        let url = resolved.first().cloned().unwrap_or_else(|| "\"\"".into());
                        let body = resolved.get(1).cloned().unwrap_or_else(|| "\"\"".into());
                        let headers_expr = resolved.get(2).cloned().unwrap_or_default();
                        // Check if headers is the default empty Dict (count==0) → pass NULL
                        let headers = if headers_expr.is_empty() || headers_expr.contains("__new()") {
                            "NULL".into()
                        } else {
                            format!("(const GorgetStringMapView*)&{headers_expr}")
                        };
                        let timeout = resolved.get(3).cloned().unwrap_or_else(|| "0".into());
                        let result_type = c_mangle::mangle_generic("Result", &["GorgetHttpResponse".into(), "const char*".into()]);
                        let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                        let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                        return format!(
                            "({{ GorgetHttpResponse __hr = gorget_http_{method}({url}, {body}, {headers}, {timeout}); \
                            const char* __he = gorget_http_last_error(); \
                            __he ? {err_ctor}(__he) : {ok_ctor}(__hr); }})"
                        );
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
                        "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                    );
                }
            }

            // Client() constructor → gorget_http_client_new()
            if name == "Client" && args.is_empty() {
                if let Some(did) = self.scopes.lookup("Client") {
                    let def = self.scopes.get_def(did);
                    if def.kind == DefKind::Struct && def.span == crate::span::Span::dummy() {
                        return "gorget_http_client_new()".to_string();
                    }
                }
            }

            // Check if this is a struct constructor
            if let Some(def_id) = self.scopes.lookup(name) {
                let def = self.scopes.get_def(def_id);
                if def.kind == crate::semantic::scope::DefKind::Struct
                    || def.kind == crate::semantic::scope::DefKind::Newtype
                {
                    let field_exprs: Vec<String> =
                        args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                    let fields = field_exprs.join(", ");
                    let c_name = c_types::def_name_to_c(def_id, self.scopes);
                    return format!("({c_name}){{{fields}}}");
                }
                // Check for enum variant constructor
                if def.kind == crate::semantic::scope::DefKind::Variant {
                    // Find which enum this variant belongs to
                    for (enum_def_id, info) in self.enum_variants {
                        for (vname, vid) in &info.variants {
                            if *vid == def_id {
                                let enum_name = self.scopes.get_def(*enum_def_id).name.clone();
                                let field_exprs: Vec<String> =
                                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                                let fields = field_exprs.join(", ");
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

            // Check if this is a GorgetClosure variable — dispatch through .fn_ptr
            let escaped_name = c_mangle::escape_keyword(name);
            if self.closure_vars.contains(escaped_name.as_str()) {
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                let arg_types: Vec<String> = args
                    .iter()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .collect();
                let mut cast_params = vec!["void*".to_string()];
                cast_params.extend(arg_types);
                let cast = format!("int64_t (*)({})", cast_params.join(", "));
                let mut call_args = vec![format!("{escaped_name}.env")];
                call_args.extend(arg_exprs);
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
                            "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                        );
                    }
                }

                // Handle File.open(path) and File.create(path)
                if type_name == "File" {
                    match method_name.as_str() {
                        "open" => {
                            if let Some(arg) = args.first() {
                                let path_arg = self.gen_expr(&arg.node.value);
                                return format!("gorget_file_open({path_arg}, \"r\")");
                            }
                        }
                        "create" => {
                            if let Some(arg) = args.first() {
                                let path_arg = self.gen_expr(&arg.node.value);
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

        // General function call
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
                return format!("{expr}.data");
            }
            // str arg → String param: wrap with gorget_string_new
            if arg_type == "const char*" && ptid == self.types.owned_string_id {
                return format!("gorget_string_new({expr})");
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
                let expr = self.coerce_arg_to_str(expr, &a.node.value, param_type_ids.get(i).copied().flatten());
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
                            let path_arg = self.gen_expr(&arg.node.value);
                            return format!("gorget_file_open({path_arg}, \"r\")");
                        }
                    }
                    "create" => {
                        if let Some(arg) = args.first() {
                            let path_arg = self.gen_expr(&arg.node.value);
                            return format!("gorget_file_open({path_arg}, \"w\")");
                        }
                    }
                    _ => {}
                }
            }

            let is_type = self
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
                let mangled = c_mangle::mangle_method(name, method_name);
                let arg_exprs: Vec<String> =
                    args.iter().map(|a| self.gen_expr(&a.node.value)).collect();
                return format!("{mangled}({})", arg_exprs.join(", "));
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
        let mangled = if let Some(trait_name) = self.find_trait_for_method(&type_name, method_name) {
            c_mangle::mangle_trait_method(&trait_name, &mangled_type, method_name)
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
        let is_option = type_name == "Option";
        let is_result = type_name == "Result";
        let is_box = type_name == "Box";
        let is_file = type_name == "File" || c_type.as_deref() == Some("GorgetFile");
        let is_http_response = type_name == "Response" || c_type.as_deref() == Some("GorgetHttpResponse");
        let is_http_client = type_name == "Client" || c_type.as_deref() == Some("GorgetHttpClient");
        let is_socket = type_name == "Socket" || c_type.as_deref() == Some("GorgetSocket");
        let is_cipher = type_name == "CipherContext" || c_type.as_deref() == Some("GorgetCipherContext");
        let is_iterator = !is_vector && !is_map && !is_set && !is_string
            && !is_option && !is_result && !is_box && !is_file
            && !is_http_response && !is_http_client && !is_socket && !is_cipher
            && matches!(method_name, "collect" | "filter" | "map" | "fold")
            && self.traits.impls.iter().any(|i|
                i.self_type_name == type_name && i.trait_name.as_deref() == Some("Iterator")
            );

        let is_char = c_type.as_deref() == Some("char")
            && matches!(method_name, "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace" | "to_upper" | "to_lower" | "is_upper" | "is_lower");

        let is_primitive_hashable = !is_vector && !is_map && !is_set && !is_string
            && !is_option && !is_result && !is_box && !is_file && !is_iterator
            && !is_char
            && method_name == "hash"
            && matches!(c_type.as_deref(), Some(
                "int64_t" | "int8_t" | "int16_t" | "int32_t" |
                "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t" |
                "double" | "float" |
                "bool" | "char32_t"
            ));

        if !is_vector && !is_map && !is_set && !is_string && !is_option && !is_result && !is_box && !is_file && !is_http_response && !is_http_client && !is_socket && !is_cipher && !is_iterator && !is_primitive_hashable && !is_char {
            return None;
        }

        let recv = self.gen_expr(receiver);
        let needs_temp = !is_lvalue(&receiver.node);

        if is_char {
            return match method_name {
                "is_alpha" | "is_digit" | "is_alphanumeric" | "is_whitespace" | "is_upper" | "is_lower" => {
                    let c_func = match method_name {
                        "is_alpha" => "isalpha",
                        "is_digit" => "isdigit",
                        "is_alphanumeric" => "isalnum",
                        "is_whitespace" => "isspace",
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
                format!("{recv}.data")
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
        if is_http_response {
            return Some(self.gen_http_response_method(&recv, method_name, args, needs_temp));
        }
        if is_http_client {
            return Some(self.gen_http_client_method(&recv, method_name, args, needs_temp));
        }
        if is_socket {
            return Some(self.gen_socket_method(&recv, method_name, args, needs_temp));
        }
        if is_cipher {
            return Some(self.gen_cipher_method(&recv, method_name, args, needs_temp));
        }

        None
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
                format!("{coll}.data")
            } else {
                coll.clone()
            };
            format!("(strstr({coll_str}, {elem}) != NULL)")
        } else {
            format!("/* unsupported `in` for type {type_name} */ false")
        }
    }
}
