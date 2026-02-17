/// Virtual stdlib module system.
///
/// When the loader encounters `std.*` imports, it generates synthetic
/// `Module` ASTs with `FunctionDef` / `FunctionBody::Declaration` nodes.
/// No filesystem files needed — the C runtime already has the implementations.
///
/// All synthetic defs use `Span::dummy()`, which distinguishes them from
/// user-defined code and enables the `is_stdlib_call()` guard in codegen.
use crate::lexer::token::{StringKind, StringLit};
use crate::parser::ast::*;
use crate::span::{Span, Spanned};

/// Check if an import path refers to a stdlib module.
pub fn is_stdlib_module(segments: &[String]) -> bool {
    if segments.first().map(|s| s.as_str()) != Some("std") {
        return false;
    }
    match segments.len() {
        2 => matches!(segments[1].as_str(), "fs" | "path" | "os" | "conv" | "io" | "random" | "time" | "collections" | "math" | "fmt" | "process" | "sdl" | "gfx" | "ecs" | "json" | "toml" | "xml" | "bytes" | "crypto" | "ssh"),
        3 => (segments[1] == "http" && segments[2] == "client")
            || (segments[1] == "net" && segments[2] == "socket"),
        _ => false,
    }
}

/// Generate a synthetic `Module` for a stdlib module.
pub fn generate_stdlib_module(segments: &[String]) -> Option<Module> {
    if segments.first().map(|s| s.as_str()) != Some("std") {
        return None;
    }
    match segments.len() {
        2 => match segments[1].as_str() {
            "fs" => Some(gen_fs_module()),
            "path" => Some(gen_path_module()),
            "os" => Some(gen_os_module()),
            "conv" => Some(gen_conv_module()),
            "io" => Some(gen_io_module()),
            "random" => Some(gen_random_module()),
            "time" => Some(gen_time_module()),
            "collections" => Some(gen_collections_module()),
            "math" => Some(gen_math_module()),
            "fmt" => Some(gen_fmt_module()),
            "process" => Some(gen_process_module()),
            "sdl" => Some(gen_sdl_module()),
            "json" => None, // file-based module — loaded via stdlib_module_source()
            "toml" => None, // file-based module — loaded via stdlib_module_source()
            "xml" => None,  // file-based module — loaded via stdlib_module_source()
            "bytes" => Some(gen_bytes_module()),
            "crypto" => Some(gen_crypto_module()),
            "gfx" => None, // file-based module — loaded via stdlib_module_source()
            "ecs" => None, // file-based module — loaded via stdlib_module_source()
            "ssh" => None, // file-based module — loaded via stdlib_module_source()
            _ => None,
        },
        3 if segments[1] == "http" && segments[2] == "client" => Some(gen_http_client_module()),
        3 if segments[1] == "net" && segments[2] == "socket" => Some(gen_socket_module()),
        _ => None,
    }
}

// ─── Module Generators ──────────────────────────────────────

fn gen_fs_module() -> Module {
    make_module(vec![
        decl_fn("read_file", &[("path", ty_str())], ty_str()),
        decl_fn("write_file", &[("path", ty_str()), ("content", ty_str())], ty_void()),
        decl_fn("append_file", &[("path", ty_str()), ("content", ty_str())], ty_void()),
        decl_fn("file_exists", &[("path", ty_str())], ty_bool()),
        decl_fn("delete_file", &[("path", ty_str())], ty_bool()),
        decl_fn("mkdir", &[("path", ty_str())], ty_bool()),
        decl_fn("rmdir", &[("path", ty_str())], ty_bool()),
        decl_fn("rename", &[("old_path", ty_str()), ("new_path", ty_str())], ty_bool()),
        decl_fn("copy_file", &[("src", ty_str()), ("dst", ty_str())], ty_bool()),
        decl_fn("file_size", &[("path", ty_str())], ty_int()),
        decl_fn("is_dir", &[("path", ty_str())], ty_bool()),
    ])
}

fn gen_path_module() -> Module {
    make_module(vec![
        decl_fn("path_join", &[("a", ty_str()), ("b", ty_str())], ty_str()),
        decl_fn("path_parent", &[("path", ty_str())], ty_str()),
        decl_fn("path_basename", &[("path", ty_str())], ty_str()),
        decl_fn("path_extension", &[("path", ty_str())], ty_str()),
        decl_fn("path_stem", &[("path", ty_str())], ty_str()),
    ])
}

fn gen_os_module() -> Module {
    make_module(vec![
        decl_fn("exit", &[("code", ty_int())], ty_void()),
        decl_fn("getenv", &[("name", ty_str())], ty_str()),
        decl_fn("setenv", &[("name", ty_str()), ("value", ty_str())], ty_void()),
        decl_fn("getcwd", &[], ty_str()),
        decl_fn("platform", &[], ty_str()),
        decl_fn("args", &[], ty_vector_str()),
        decl_fn("readdir", &[("path", ty_str())], ty_vector_str()),
    ])
}

fn gen_conv_module() -> Module {
    make_module(vec![
        decl_fn("ord", &[("c", ty_char())], ty_int()),
        decl_fn("chr", &[("n", ty_int())], ty_char()),
        decl_fn("parse_int", &[("s", ty_str())], ty_int()),
        decl_fn("parse_float", &[("s", ty_str())], ty_float()),
        decl_fn("int_to_str", &[("n", ty_int())], ty_string()),
        decl_fn("float_to_str", &[("x", ty_float())], ty_string()),
        decl_fn("bool_to_str", &[("b", ty_bool())], ty_str()),
        decl_fn("char_to_str", &[("c", ty_char())], ty_string()),
        decl_fn("codepoint_to_str", &[("cp", ty_int())], ty_string()),
    ])
}

fn gen_io_module() -> Module {
    let file_type = Type::Named {
        name: Spanned::dummy("File".to_string()),
        generic_args: vec![],
    };
    let mut items = vec![
        Spanned::dummy(Item::StaticDecl(StaticDecl {
            visibility: Visibility::Public,
            name: Spanned::dummy("stderr".to_string()),
            type_: Spanned::dummy(file_type.clone()),
            value: Spanned::dummy(Expr::IntLiteral(0)), // placeholder — codegen special-cases
            span: Span::dummy(),
        })),
        Spanned::dummy(Item::StaticDecl(StaticDecl {
            visibility: Visibility::Public,
            name: Spanned::dummy("stdout".to_string()),
            type_: Spanned::dummy(file_type),
            value: Spanned::dummy(Expr::IntLiteral(0)), // placeholder
            span: Span::dummy(),
        })),
    ];
    // Add functions
    for name in &["getchar", "term_cols", "term_rows"] {
        items.push(Spanned::dummy(Item::Function(
            decl_fn(name, &[], ty_int()),
        )));
    }
    items.push(Spanned::dummy(Item::Function(
        decl_fn("input", &[("prompt", ty_str())], ty_str()),
    )));
    items.push(Spanned::dummy(Item::Function(
        decl_fn("readline", &[], ty_str()),
    )));
    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_random_module() -> Module {
    make_module(vec![
        decl_fn("rand", &[], ty_int()),
        decl_fn("seed", &[("n", ty_int())], ty_void()),
        decl_fn("rand_range", &[("lo", ty_int()), ("hi", ty_int())], ty_int()),
    ])
}

fn gen_time_module() -> Module {
    make_module(vec![
        decl_fn("time", &[], ty_int()),
        decl_fn("time_ms", &[], ty_int()),
        decl_fn("sleep_ms", &[("ms", ty_int())], ty_void()),
    ])
}

fn gen_math_module() -> Module {
    make_module(vec![
        // Integer math
        decl_fn("abs", &[("x", ty_int())], ty_int()),
        decl_fn("min", &[("a", ty_int()), ("b", ty_int())], ty_int()),
        decl_fn("max", &[("a", ty_int()), ("b", ty_int())], ty_int()),
        // Float math
        decl_fn("sqrt", &[("x", ty_float())], ty_float()),
        decl_fn("pow", &[("base", ty_float()), ("exp", ty_float())], ty_float()),
        decl_fn("floor", &[("x", ty_float())], ty_float()),
        decl_fn("ceil", &[("x", ty_float())], ty_float()),
        decl_fn("round", &[("x", ty_float())], ty_float()),
        decl_fn("log", &[("x", ty_float())], ty_float()),
        decl_fn("log2", &[("x", ty_float())], ty_float()),
        decl_fn("log10", &[("x", ty_float())], ty_float()),
        decl_fn("sin", &[("x", ty_float())], ty_float()),
        decl_fn("cos", &[("x", ty_float())], ty_float()),
        decl_fn("tan", &[("x", ty_float())], ty_float()),
        decl_fn("asin", &[("x", ty_float())], ty_float()),
        decl_fn("acos", &[("x", ty_float())], ty_float()),
        decl_fn("atan", &[("x", ty_float())], ty_float()),
        decl_fn("atan2", &[("y", ty_float()), ("x", ty_float())], ty_float()),
        decl_fn("fabs", &[("x", ty_float())], ty_float()),
        decl_fn("fmin", &[("a", ty_float()), ("b", ty_float())], ty_float()),
        decl_fn("fmax", &[("a", ty_float()), ("b", ty_float())], ty_float()),
    ])
}

fn gen_fmt_module() -> Module {
    // Displayable trait and format() are already in the prelude/builtins.
    // This module exists so `from std.fmt import Displayable` doesn't error.
    make_module(vec![])
}

fn gen_process_module() -> Module {
    let exec_result_type = Type::Named {
        name: Spanned::dummy("ExecResult".to_string()),
        generic_args: vec![],
    };
    let struct_def = StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("ExecResult".to_string()),
        generic_params: None,
        fields: vec![
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("output".to_string()),
                type_: Spanned::dummy(ty_str()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("errors".to_string()),
                type_: Spanned::dummy(ty_str()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("exit_code".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
        ],
        doc_comment: None,
        span: Span::dummy(),
    };
    let items = vec![
        Spanned::dummy(Item::Struct(struct_def)),
        Spanned::dummy(Item::Function(decl_fn("exec", &[("cmd", ty_str())], ty_int()))),
        Spanned::dummy(Item::Function(decl_fn("exec_output", &[("cmd", ty_str())], exec_result_type))),
    ];
    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_collections_module() -> Module {
    let type_defs: Vec<(&str, usize)> = vec![
        ("Vector", 1), ("List", 1), ("Array", 1),   // [T]
        ("Dict", 2), ("HashMap", 2),                   // [K, V]
        ("Set", 1), ("HashSet", 1),                   // [T]
        ("Box", 1),                                    // [T]
        ("File", 0),                                   // no generics
    ];
    let mut items: Vec<Spanned<Item>> = type_defs
        .into_iter()
        .map(|(name, n_params)| {
            Spanned::dummy(Item::Struct(StructDef {
                attributes: vec![],
                visibility: Visibility::Public,
                name: Spanned::dummy(name.to_string()),
                generic_params: if n_params > 0 {
                    Some(Spanned::dummy(GenericParams {
                        params: (0..n_params)
                            .map(|i| {
                                let param_name = if n_params == 2 && i == 0 {
                                    "K"
                                } else if n_params == 2 && i == 1 {
                                    "V"
                                } else {
                                    "T"
                                };
                                Spanned::dummy(GenericParam::Type(Spanned::dummy(
                                    param_name.to_string(),
                                )))
                            })
                            .collect(),
                    }))
                } else {
                    None
                },
                fields: vec![],
                doc_comment: None,
                span: Span::dummy(),
            }))
        })
        .collect();

    // File instance methods — extern bindings (open/create stay hardcoded as static constructors)
    items.push(equip_block("File", vec![
        extern_method("read_all", Ownership::MutableBorrow, &[], ty_str(), "gorget_file_read_all"),
        extern_method("write", Ownership::MutableBorrow, &[("content", ty_str())], ty_void(), "gorget_file_write"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_file_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_sdl_module() -> Module {
    // Helper to create an opaque handle struct (zero fields, dummy span)
    let opaque_struct = |name: &str| -> Spanned<Item> {
        Spanned::dummy(Item::Struct(StructDef {
            attributes: vec![],
            visibility: Visibility::Public,
            name: Spanned::dummy(name.to_string()),
            generic_params: None,
            fields: vec![],
            doc_comment: None,
            span: Span::dummy(),
        }))
    };

    // SDLEvent has user-visible fields
    let sdl_event_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("SDLEvent".to_string()),
        generic_params: None,
        fields: vec![
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("event_type".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("key_code".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("mouse_x".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("mouse_y".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("mouse_button".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
        ],
        doc_comment: None,
        span: Span::dummy(),
    }));

    // Named types for opaque handles
    let ty_sdl_window = || Type::Named {
        name: Spanned::dummy("SDLWindow".to_string()),
        generic_args: vec![],
    };
    let ty_sdl_renderer = || Type::Named {
        name: Spanned::dummy("SDLRenderer".to_string()),
        generic_args: vec![],
    };
    let ty_sdl_texture = || Type::Named {
        name: Spanned::dummy("SDLTexture".to_string()),
        generic_args: vec![],
    };
    let ty_sdl_font = || Type::Named {
        name: Spanned::dummy("SDLFont".to_string()),
        generic_args: vec![],
    };
    let ty_sdl_event = || Type::Named {
        name: Spanned::dummy("SDLEvent".to_string()),
        generic_args: vec![],
    };

    // Helper to make a const declaration item
    let const_item = |name: &str, value: i64| -> Spanned<Item> {
        Spanned::dummy(Item::ConstDecl(ConstDecl {
            visibility: Visibility::Public,
            type_: Spanned::dummy(ty_int()),
            name: Spanned::dummy(name.to_string()),
            value: Spanned::dummy(Expr::IntLiteral(value)),
            span: Span::dummy(),
        }))
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // ── Opaque handle structs ────────────────────────────────
    items.push(opaque_struct("SDLWindow"));
    items.push(opaque_struct("SDLRenderer"));
    items.push(opaque_struct("SDLTexture"));
    items.push(opaque_struct("SDLFont"));
    items.push(sdl_event_struct);

    // ── Constants ────────────────────────────────────────────
    // Init flags
    items.push(const_item("SDL_INIT_VIDEO", 0x20));
    items.push(const_item("SDL_INIT_AUDIO", 0x10));
    items.push(const_item("SDL_INIT_EVERYTHING", 0x7231));

    // Event types
    items.push(const_item("SDL_QUIT", 256));
    items.push(const_item("SDL_KEYDOWN", 768));
    items.push(const_item("SDL_KEYUP", 769));
    items.push(const_item("SDL_MOUSEMOTION", 1024));
    items.push(const_item("SDL_MOUSEBUTTONDOWN", 1025));
    items.push(const_item("SDL_MOUSEBUTTONUP", 1026));

    // Key codes
    items.push(const_item("SDLK_ESCAPE", 27));
    items.push(const_item("SDLK_SPACE", 32));
    items.push(const_item("SDLK_RETURN", 13));
    items.push(const_item("SDLK_LEFT", 1073741904));
    items.push(const_item("SDLK_RIGHT", 1073741903));
    items.push(const_item("SDLK_UP", 1073741906));
    items.push(const_item("SDLK_DOWN", 1073741905));
    items.push(const_item("SDLK_a", 97));
    items.push(const_item("SDLK_b", 98));
    items.push(const_item("SDLK_c", 99));
    items.push(const_item("SDLK_d", 100));
    items.push(const_item("SDLK_e", 101));
    items.push(const_item("SDLK_f", 102));
    items.push(const_item("SDLK_g", 103));
    items.push(const_item("SDLK_h", 104));
    items.push(const_item("SDLK_i", 105));
    items.push(const_item("SDLK_j", 106));
    items.push(const_item("SDLK_k", 107));
    items.push(const_item("SDLK_l", 108));
    items.push(const_item("SDLK_m", 109));
    items.push(const_item("SDLK_n", 110));
    items.push(const_item("SDLK_o", 111));
    items.push(const_item("SDLK_p", 112));
    items.push(const_item("SDLK_q", 113));
    items.push(const_item("SDLK_r", 114));
    items.push(const_item("SDLK_s", 115));
    items.push(const_item("SDLK_t", 116));
    items.push(const_item("SDLK_u", 117));
    items.push(const_item("SDLK_v", 118));
    items.push(const_item("SDLK_w", 119));
    items.push(const_item("SDLK_x", 120));
    items.push(const_item("SDLK_y", 121));
    items.push(const_item("SDLK_z", 122));

    // Window flags
    items.push(const_item("SDL_WINDOW_SHOWN", 4));
    items.push(const_item("SDL_WINDOW_RESIZABLE", 32));
    items.push(const_item("SDL_WINDOW_FULLSCREEN", 1));

    // Renderer flags
    items.push(const_item("SDL_RENDERER_ACCELERATED", 2));
    items.push(const_item("SDL_RENDERER_PRESENTVSYNC", 4));

    // ── Function declarations ────────────────────────────────
    // Lifecycle
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_init", &[("flags", ty_int())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_quit", &[], ty_void()))));

    // Window
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_create_window", &[("title", ty_str()), ("w", ty_int()), ("h", ty_int()), ("flags", ty_int())], ty_sdl_window()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_destroy_window", &[("win", ty_sdl_window())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_window_width", &[("win", ty_sdl_window())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_window_height", &[("win", ty_sdl_window())], ty_int()))));

    // Renderer
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_create_renderer", &[("win", ty_sdl_window()), ("flags", ty_int())], ty_sdl_renderer()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_destroy_renderer", &[("r", ty_sdl_renderer())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_set_draw_color", &[("r", ty_sdl_renderer()), ("red", ty_int()), ("green", ty_int()), ("blue", ty_int()), ("alpha", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_clear", &[("r", ty_sdl_renderer())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_present", &[("r", ty_sdl_renderer())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_draw_rect", &[("r", ty_sdl_renderer()), ("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_fill_rect", &[("r", ty_sdl_renderer()), ("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void()))));

    // Drawing
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_draw_line", &[("r", ty_sdl_renderer()), ("x1", ty_int()), ("y1", ty_int()), ("x2", ty_int()), ("y2", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_draw_point", &[("r", ty_sdl_renderer()), ("x", ty_int()), ("y", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_set_blend_mode", &[("r", ty_sdl_renderer()), ("mode", ty_int())], ty_void()))));

    // Textures (SDL2_image)
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_load_texture", &[("r", ty_sdl_renderer()), ("path", ty_str())], ty_sdl_texture()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_destroy_texture", &[("t", ty_sdl_texture())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_render_texture", &[("r", ty_sdl_renderer()), ("t", ty_sdl_texture()), ("x", ty_int()), ("y", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_render_texture_sized", &[("r", ty_sdl_renderer()), ("t", ty_sdl_texture()), ("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_texture_width", &[("t", ty_sdl_texture())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_texture_height", &[("t", ty_sdl_texture())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_set_texture_alpha", &[("t", ty_sdl_texture()), ("alpha", ty_int())], ty_void()))));

    // Text (SDL2_ttf)
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_load_font", &[("path", ty_str()), ("size", ty_int())], ty_sdl_font()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_close_font", &[("f", ty_sdl_font())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_render_text", &[("r", ty_sdl_renderer()), ("f", ty_sdl_font()), ("text", ty_str()), ("red", ty_int()), ("green", ty_int()), ("blue", ty_int())], ty_sdl_texture()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_draw_text", &[("r", ty_sdl_renderer()), ("f", ty_sdl_font()), ("text", ty_str()), ("x", ty_int()), ("y", ty_int()), ("red", ty_int()), ("green", ty_int()), ("blue", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_text_width", &[("f", ty_sdl_font()), ("text", ty_str())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_text_height", &[("f", ty_sdl_font()), ("text", ty_str())], ty_int()))));

    // Events
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_poll_event", &[], ty_sdl_event()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_has_event", &[], ty_bool()))));

    // Timing
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_delay", &[("ms", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_ticks", &[], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_performance_counter", &[], ty_int()))));

    // Screen info
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_display_width", &[], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_display_height", &[], ty_int()))));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── File-based stdlib modules ──────────────────────────────

/// Get embedded source for file-based stdlib modules.
/// These are real `.gg` files compiled into the binary, parsed and loaded
/// by the module loader (including recursive import resolution).
pub fn stdlib_module_source(segments: &[String]) -> Option<&'static str> {
    if segments.first().map(|s| s.as_str()) != Some("std") {
        return None;
    }
    match segments.get(1).map(|s| s.as_str()) {
        Some("gfx") => Some(include_str!("../lib/std/gfx.gg")),
        Some("ecs") => Some(include_str!("../lib/std/ecs.gg")),
        Some("ssh") => Some(include_str!("../lib/std/ssh.gg")),
        Some("json") => Some(include_str!("../lib/std/json.gg")),
        Some("toml") => Some(include_str!("../lib/std/toml.gg")),
        Some("xml") => Some(include_str!("../lib/std/xml.gg")),
        _ => None,
    }
}

// ─── Helpers ────────────────────────────────────────────────

fn make_module(fns: Vec<FunctionDef>) -> Module {
    let items: Vec<Spanned<Item>> = fns
        .into_iter()
        .map(|f| Spanned::dummy(Item::Function(f)))
        .collect();
    Module {
        items,
        span: Span::dummy(),
    }
}

// All parameters in synthetic module declarations use Ownership::Borrow. This is
// intentional: these are FFI boundaries where the C runtime never takes ownership
// of the caller's data, so borrowing is always correct. Move semantics would force
// callers to clone before calling (e.g., losing a Vector after hashing it). Method
// receiver ownership (self vs &self) is set per-method via decl_method/extern_method.
fn decl_fn(name: &str, params: &[(&str, Type)], ret: Type) -> FunctionDef {
    FunctionDef {
        attributes: Vec::new(),
        visibility: Visibility::Public,
        qualifiers: FunctionQualifiers::default(),
        return_type: Spanned::dummy(ret),
        name: Spanned::dummy(name.to_string()),
        generic_params: None,
        params: params
            .iter()
            .map(|(pname, pty)| {
                Spanned::dummy(Param {
                    type_: Spanned::dummy(pty.clone()),
                    ownership: Ownership::Borrow,
                    name: Spanned::dummy(pname.to_string()),
                    default: None,
                    is_live: false,
                    live_group: None,
                })
            })
            .collect(),
        throws: None,
        where_clause: None,
        body: FunctionBody::Declaration,
        doc_comment: None,
        span: Span::dummy(),
    }
}

fn ty_str() -> Type {
    Type::Primitive(PrimitiveType::Str)
}

fn ty_string() -> Type {
    Type::Primitive(PrimitiveType::StringType)
}

fn ty_int() -> Type {
    Type::Primitive(PrimitiveType::Int)
}

fn ty_bool() -> Type {
    Type::Primitive(PrimitiveType::Bool)
}

fn ty_char() -> Type {
    Type::Primitive(PrimitiveType::Char)
}

fn ty_float() -> Type {
    Type::Primitive(PrimitiveType::Float)
}

fn ty_void() -> Type {
    Type::Primitive(PrimitiveType::Void)
}

fn ty_vector_str() -> Type {
    Type::Named {
        name: Spanned::dummy("Vector".to_string()),
        generic_args: vec![Spanned::dummy(ty_str())],
    }
}

fn ty_uint8() -> Type {
    Type::Primitive(PrimitiveType::Uint8)
}

fn ty_vector_uint8() -> Type {
    Type::Named {
        name: Spanned::dummy("Vector".to_string()),
        generic_args: vec![Spanned::dummy(ty_uint8())],
    }
}

fn ty_result(ok: Type, err: Type) -> Type {
    Type::Named {
        name: Spanned::dummy("Result".to_string()),
        generic_args: vec![Spanned::dummy(ok), Spanned::dummy(err)],
    }
}

fn ty_dict_str_str() -> Type {
    Type::Named {
        name: Spanned::dummy("Dict".to_string()),
        generic_args: vec![Spanned::dummy(ty_str()), Spanned::dummy(ty_str())],
    }
}

fn ty_response() -> Type {
    Type::Named {
        name: Spanned::dummy("Response".to_string()),
        generic_args: vec![],
    }
}


fn opaque_struct(name: &str) -> Spanned<Item> {
    Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy(name.to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }))
}

fn decl_fn_with_defaults(
    name: &str,
    params: &[(&str, Type, Option<Expr>)],
    ret: Type,
) -> FunctionDef {
    FunctionDef {
        attributes: Vec::new(),
        visibility: Visibility::Public,
        qualifiers: FunctionQualifiers::default(),
        return_type: Spanned::dummy(ret),
        name: Spanned::dummy(name.to_string()),
        generic_params: None,
        params: params
            .iter()
            .map(|(pname, pty, default)| {
                Spanned::dummy(Param {
                    type_: Spanned::dummy(pty.clone()),
                    ownership: Ownership::Borrow,
                    name: Spanned::dummy(pname.to_string()),
                    default: default.as_ref().map(|d| Spanned::dummy(d.clone())),
                    is_live: false,
                    live_group: None,
                })
            })
            .collect(),
        throws: None,
        where_clause: None,
        body: FunctionBody::Declaration,
        doc_comment: None,
        span: Span::dummy(),
    }
}

// ─── std.crypto ─────────────────────────────────────────────

fn gen_crypto_module() -> Module {
    let ty_cipher = || Type::Named {
        name: Spanned::dummy("CipherContext".to_string()),
        generic_args: vec![],
    };
    let ty_bignum = || Type::Named {
        name: Spanned::dummy("BigNum".to_string()),
        generic_args: vec![],
    };
    let ty_rsakey = || Type::Named {
        name: Spanned::dummy("RSAKey".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque structs
    items.push(opaque_struct("CipherContext"));
    items.push(opaque_struct("BigNum"));
    items.push(opaque_struct("RSAKey"));

    // Free functions — extern bindings (except Result-wrapping functions which stay as Declaration)
    let fns = vec![
        // Hashing
        extern_fn("crypto_sha256", &[("data", ty_vector_uint8())], ty_vector_uint8(), "gorget_crypto_sha256"),
        extern_fn("crypto_sha1", &[("data", ty_vector_uint8())], ty_vector_uint8(), "gorget_crypto_sha1"),
        // HMAC — Result wrapping in codegen
        decl_fn("crypto_hmac", &[("algo", ty_str()), ("key", ty_vector_uint8()), ("data", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str())),
        // AES-CTR — Result wrapping in codegen
        decl_fn("crypto_aes_ctr_new", &[("key", ty_vector_uint8()), ("iv", ty_vector_uint8())], ty_result(ty_cipher(), ty_str())),
        // BigNum
        extern_fn("crypto_bn_from_bytes", &[("data", ty_vector_uint8())], ty_bignum(), "gorget_crypto_bn_from_bytes"),
        extern_fn("crypto_bn_to_bytes", &[("bn", ty_bignum())], ty_vector_uint8(), "gorget_crypto_bn_to_bytes"),
        extern_fn("crypto_bn_mod_exp", &[("base", ty_bignum()), ("exp", ty_bignum()), ("modulus", ty_bignum())], ty_bignum(), "gorget_crypto_bn_mod_exp"),
        // RSA — crypto_rsa_load_public stays as Declaration (Result wrapping in codegen)
        decl_fn("crypto_rsa_load_public", &[("key_bytes", ty_vector_uint8())], ty_result(ty_rsakey(), ty_str())),
        extern_fn("crypto_rsa_verify", &[("key", ty_rsakey()), ("data", ty_vector_uint8()), ("sig", ty_vector_uint8())], ty_bool(), "gorget_crypto_rsa_verify"),
        // Random — Result wrapping in codegen
        decl_fn("crypto_random_bytes", &[("n", ty_int())], ty_result(ty_vector_uint8(), ty_str())),
    ];
    for f in fns {
        items.push(Spanned::dummy(Item::Function(f)));
    }

    // CipherContext methods — extern bindings
    items.push(equip_block("CipherContext", vec![
        extern_method("encrypt", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_vector_uint8(), "gorget_cipher_encrypt"),
        extern_method("decrypt", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_vector_uint8(), "gorget_cipher_decrypt"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── std.net.socket ─────────────────────────────────────────

fn gen_socket_module() -> Module {
    let ty_socket = || Type::Named {
        name: Spanned::dummy("Socket".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque struct: Socket
    items.push(opaque_struct("Socket"));

    // Free function: socket_connect(host, port) -> Result[Socket, str]
    items.push(Spanned::dummy(Item::Function(
        decl_fn("socket_connect", &[("host", ty_str()), ("port", ty_int())], ty_result(ty_socket(), ty_str())),
    )));

    // Socket methods — extern bindings
    items.push(equip_block("Socket", vec![
        extern_method("read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_read"),
        extern_method("read_exact", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_read_exact"),
        extern_method("write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_socket_write"),
        extern_method("write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_socket_write_str"),
        extern_method("read_line", Ownership::MutableBorrow, &[], ty_str(), "gorget_socket_read_line"),
        extern_method("set_timeout", Ownership::MutableBorrow, &[("ms", ty_int())], ty_void(), "gorget_socket_set_timeout"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_socket_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── std.bytes ──────────────────────────────────────────────

fn gen_bytes_module() -> Module {
    make_module(vec![
        extern_fn("bytes_from_str", &[("s", ty_str())], ty_vector_uint8(), "gorget_bytes_from_str"),
        extern_fn("bytes_to_str", &[("b", ty_vector_uint8())], ty_str(), "gorget_bytes_to_str"),
        extern_fn("bytes_from_hex", &[("hex", ty_str())], ty_vector_uint8(), "gorget_bytes_from_hex"),
        extern_fn("bytes_to_hex", &[("b", ty_vector_uint8())], ty_str(), "gorget_bytes_to_hex"),
        extern_fn("bytes_write_u32_be", &[("b", ty_vector_uint8()), ("offset", ty_int()), ("value", ty_int())], ty_void(), "gorget_bytes_write_u32_be"),
        extern_fn("bytes_read_u32_be", &[("b", ty_vector_uint8()), ("offset", ty_int())], ty_int(), "gorget_bytes_read_u32_be"),
        extern_fn("bytes_write_u16_be", &[("b", ty_vector_uint8()), ("offset", ty_int()), ("value", ty_int())], ty_void(), "gorget_bytes_write_u16_be"),
        extern_fn("bytes_read_u16_be", &[("b", ty_vector_uint8()), ("offset", ty_int())], ty_int(), "gorget_bytes_read_u16_be"),
        extern_fn("bytes_concat", &[("a", ty_vector_uint8()), ("b", ty_vector_uint8())], ty_vector_uint8(), "gorget_bytes_concat"),
        extern_fn("bytes_slice", &[("b", ty_vector_uint8()), ("start", ty_int()), ("end", ty_int())], ty_vector_uint8(), "gorget_bytes_slice"),
        extern_fn("random_bytes", &[("n", ty_int())], ty_vector_uint8(), "gorget_random_bytes"),
    ])
}

// ─── std.http.client ────────────────────────────────────────

/// Build an extern free function declaration.
/// The C symbol is called directly instead of going through hardcoded dispatch.
fn extern_fn(name: &str, params: &[(&str, Type)], ret: Type, c_symbol: &str) -> FunctionDef {
    let mut f = decl_fn(name, params, ret);
    f.body = FunctionBody::Extern(c_symbol.to_string());
    f
}

/// Build an extern method binding for an equip block (has `self` as first param).
/// The C symbol is called directly instead of going through hardcoded dispatch.
fn extern_method(
    name: &str,
    self_ownership: Ownership,
    extra_params: &[(&str, Type)],
    ret: Type,
    c_symbol: &str,
) -> FunctionDef {
    let mut f = decl_method(name, self_ownership, extra_params, ret);
    f.body = FunctionBody::Extern(c_symbol.to_string());
    f
}

/// Build a method declaration for an equip block (has `self` as first param).
fn decl_method(
    name: &str,
    self_ownership: Ownership,
    extra_params: &[(&str, Type)],
    ret: Type,
) -> FunctionDef {
    let mut params = vec![Spanned::dummy(Param {
        type_: Spanned::dummy(Type::SelfType),
        ownership: self_ownership,
        name: Spanned::dummy("self".to_string()),
        default: None,
        is_live: false,
        live_group: None,
    })];
    for (pname, pty) in extra_params {
        params.push(Spanned::dummy(Param {
            type_: Spanned::dummy(pty.clone()),
            ownership: Ownership::Borrow,
            name: Spanned::dummy(pname.to_string()),
            default: None,
            is_live: false,
            live_group: None,
        }));
    }
    FunctionDef {
        attributes: Vec::new(),
        visibility: Visibility::Public,
        qualifiers: FunctionQualifiers::default(),
        return_type: Spanned::dummy(ret),
        name: Spanned::dummy(name.to_string()),
        generic_params: None,
        params,
        throws: None,
        where_clause: None,
        body: FunctionBody::Declaration,
        doc_comment: None,
        span: Span::dummy(),
    }
}

/// Build an equip block (inherent, no trait).
fn equip_block(type_name: &str, methods: Vec<FunctionDef>) -> Spanned<Item> {
    Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy(type_name.to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: methods.into_iter().map(Spanned::dummy).collect(),
        span: Span::dummy(),
    }))
}

fn gen_http_client_module() -> Module {
    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque structs
    items.push(opaque_struct("Response"));
    items.push(opaque_struct("Client"));

    // Response methods: status(), body(), header(key) — extern bindings
    items.push(equip_block("Response", vec![
        extern_method("status", Ownership::Borrow, &[], ty_int(), "gorget_http_response_status"),
        extern_method("body", Ownership::Borrow, &[], ty_str(), "gorget_http_response_body"),
        extern_method("header", Ownership::Borrow, &[("key", ty_str())], ty_str(), "gorget_http_response_header"),
    ]));

    // Client methods: simple property setters use extern bindings,
    // HTTP verb methods (get/post/...) stay as Declaration (hardcoded Result wrapping in codegen)
    items.push(equip_block("Client", vec![
        extern_method("base_url", Ownership::MutableBorrow, &[("url", ty_str())], ty_void(), "gorget_http_client_set_base_url"),
        extern_method("header", Ownership::MutableBorrow, &[("key", ty_str()), ("value", ty_str())], ty_void(), "gorget_http_client_set_header"),
        extern_method("timeout", Ownership::MutableBorrow, &[("ms", ty_int())], ty_void(), "gorget_http_client_set_timeout"),
        decl_method("get", Ownership::Borrow, &[("path", ty_str())], ty_result(ty_response(), ty_str())),
        decl_method("post", Ownership::Borrow, &[("path", ty_str()), ("body", ty_str())], ty_result(ty_response(), ty_str())),
        decl_method("put", Ownership::Borrow, &[("path", ty_str()), ("body", ty_str())], ty_result(ty_response(), ty_str())),
        decl_method("delete", Ownership::Borrow, &[("path", ty_str())], ty_result(ty_response(), ty_str())),
        decl_method("patch", Ownership::Borrow, &[("path", ty_str()), ("body", ty_str())], ty_result(ty_response(), ty_str())),
        decl_method("head", Ownership::Borrow, &[("path", ty_str())], ty_result(ty_response(), ty_str())),
    ]));

    // HTTP verb functions with default params
    let http_verbs = ["get", "post", "put", "delete", "patch", "head"];
    for verb in &http_verbs {
        items.push(Spanned::dummy(Item::Function(decl_fn_with_defaults(
            verb,
            &[
                ("url", ty_str(), None),
                ("body", ty_str(), Some(Expr::StringLiteral(StringLit { kind: StringKind::Normal, segments: vec![] }))),
                ("headers", ty_dict_str_str(), Some(Expr::Call {
                    callee: Box::new(Spanned::dummy(Expr::Identifier("Dict".into()))),
                    generic_args: None,
                    args: vec![],
                })),
                ("timeout", ty_int(), Some(Expr::IntLiteral(0))),
            ],
            ty_result(ty_response(), ty_str()),
        ))));
    }

    Module {
        items,
        span: Span::dummy(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_stdlib() {
        assert!(is_stdlib_module(&["std".into(), "fs".into()]));
        assert!(is_stdlib_module(&["std".into(), "path".into()]));
        assert!(is_stdlib_module(&["std".into(), "os".into()]));
        assert!(is_stdlib_module(&["std".into(), "conv".into()]));
        assert!(is_stdlib_module(&["std".into(), "io".into()]));
        assert!(is_stdlib_module(&["std".into(), "random".into()]));
        assert!(is_stdlib_module(&["std".into(), "time".into()]));
        assert!(is_stdlib_module(&["std".into(), "math".into()]));
        assert!(is_stdlib_module(&["std".into(), "fmt".into()]));
        assert!(is_stdlib_module(&["std".into(), "process".into()]));
        assert!(is_stdlib_module(&["std".into(), "sdl".into()]));
        assert!(is_stdlib_module(&["std".into(), "ecs".into()]));
        assert!(is_stdlib_module(&["std".into(), "json".into()]));
        assert!(is_stdlib_module(&["std".into(), "bytes".into()]));
        assert!(is_stdlib_module(&["std".into(), "http".into(), "client".into()]));
        assert!(!is_stdlib_module(&["std".into(), "test".into(), "process".into()]));
        assert!(!is_stdlib_module(&["std".into(), "foo".into()]));
        assert!(!is_stdlib_module(&["foo".into(), "fs".into()]));
        assert!(!is_stdlib_module(&["std".into()]));
    }

    #[test]
    fn generate_fs() {
        let m = generate_stdlib_module(&["std".into(), "fs".into()]).unwrap();
        assert_eq!(m.items.len(), 11);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"read_file".to_string()));
        assert!(names.contains(&"write_file".to_string()));
        assert!(names.contains(&"append_file".to_string()));
        assert!(names.contains(&"file_exists".to_string()));
        assert!(names.contains(&"delete_file".to_string()));
        assert!(names.contains(&"mkdir".to_string()));
        assert!(names.contains(&"rmdir".to_string()));
        assert!(names.contains(&"rename".to_string()));
        assert!(names.contains(&"copy_file".to_string()));
        assert!(names.contains(&"file_size".to_string()));
        assert!(names.contains(&"is_dir".to_string()));
    }

    #[test]
    fn generate_conv() {
        let m = generate_stdlib_module(&["std".into(), "conv".into()]).unwrap();
        assert_eq!(m.items.len(), 9);
    }

    #[test]
    fn generate_io() {
        let m = generate_stdlib_module(&["std".into(), "io".into()]).unwrap();
        assert_eq!(m.items.len(), 7); // stderr, stdout, getchar, term_cols, term_rows, input, readline
    }

    #[test]
    fn generate_random() {
        let m = generate_stdlib_module(&["std".into(), "random".into()]).unwrap();
        assert_eq!(m.items.len(), 3);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"rand".to_string()));
        assert!(names.contains(&"seed".to_string()));
        assert!(names.contains(&"rand_range".to_string()));
    }

    #[test]
    fn generate_time() {
        let m = generate_stdlib_module(&["std".into(), "time".into()]).unwrap();
        assert_eq!(m.items.len(), 3);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"time".to_string()));
        assert!(names.contains(&"sleep_ms".to_string()));
        assert!(names.contains(&"time_ms".to_string()));
    }

    #[test]
    fn generate_math() {
        let m = generate_stdlib_module(&["std".into(), "math".into()]).unwrap();
        assert_eq!(m.items.len(), 21);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"abs".to_string()));
        assert!(names.contains(&"sqrt".to_string()));
        assert!(names.contains(&"sin".to_string()));
        assert!(names.contains(&"atan2".to_string()));
        assert!(names.contains(&"fmin".to_string()));
    }

    #[test]
    fn generate_collections() {
        let m = generate_stdlib_module(&["std".into(), "collections".into()]).unwrap();
        assert_eq!(m.items.len(), 10); // 9 structs + 1 File equip block
        let names: Vec<_> = m.items.iter().filter_map(|i| match &i.node {
            Item::Struct(s) => Some(s.name.node.clone()),
            _ => None,
        }).collect();
        assert!(names.contains(&"Vector".to_string()));
        assert!(names.contains(&"Dict".to_string()));
        assert!(names.contains(&"Set".to_string()));
        assert!(names.contains(&"Box".to_string()));
        assert!(names.contains(&"File".to_string()));
        // File equip block should have extern methods
        let equip_count = m.items.iter().filter(|i| matches!(&i.node, Item::Equip(_))).count();
        assert_eq!(equip_count, 1);
    }

    #[test]
    fn generate_unknown_returns_none() {
        assert!(generate_stdlib_module(&["std".into(), "foo".into()]).is_none());
    }

    #[test]
    fn is_stdlib_gfx() {
        assert!(is_stdlib_module(&["std".into(), "gfx".into()]));
    }

    #[test]
    fn generate_gfx_returns_none() {
        // std.gfx is file-based, not synthetic — generate returns None
        assert!(generate_stdlib_module(&["std".into(), "gfx".into()]).is_none());
    }

    #[test]
    fn gfx_module_source_exists() {
        let source = stdlib_module_source(&["std".into(), "gfx".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Canvas"));
        assert!(src.contains("struct Color"));
        assert!(src.contains("gfx_open"));
        assert!(src.contains("gfx_close"));
        assert!(src.contains("gfx_fill_circle"));
    }

    #[test]
    fn gfx_source_parses() {
        let source = stdlib_module_source(&["std".into(), "gfx".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "gfx.gg parse errors: {:?}", parser.errors);

        // Collect item names
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => {}
            }
        }

        assert!(struct_names.contains(&"Canvas".to_string()));
        assert!(struct_names.contains(&"Color".to_string()));
        assert!(fn_names.contains(&"gfx_open".to_string()));
        assert!(fn_names.contains(&"gfx_draw_circle".to_string()));
        assert!(fn_names.contains(&"gfx_fill_circle".to_string()));
    }

    #[test]
    fn is_stdlib_ecs() {
        assert!(is_stdlib_module(&["std".into(), "ecs".into()]));
    }

    #[test]
    fn generate_ecs_returns_none() {
        // std.ecs is file-based, not synthetic — generate returns None
        assert!(generate_stdlib_module(&["std".into(), "ecs".into()]).is_none());
    }

    #[test]
    fn ecs_module_source_exists() {
        let source = stdlib_module_source(&["std".into(), "ecs".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct EntityPool"));
        assert!(src.contains("struct SparseSet"));
        assert!(src.contains("equip EntityPool"));
        assert!(src.contains("equip SparseSet"));
    }

    #[test]
    fn ecs_source_parses() {
        let source = stdlib_module_source(&["std".into(), "ecs".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "ecs.gg parse errors: {:?}", parser.errors);

        let mut struct_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }

        assert!(struct_names.contains(&"EntityPool".to_string()));
        assert!(struct_names.contains(&"SparseSet".to_string()));
        assert_eq!(equip_count, 2);
    }

    #[test]
    fn is_stdlib_sdl() {
        assert!(is_stdlib_module(&["std".into(), "sdl".into()]));
    }

    #[test]
    fn generate_sdl() {
        let m = generate_stdlib_module(&["std".into(), "sdl".into()]).unwrap();

        // Collect item names by type
        let mut struct_names = vec![];
        let mut const_names = vec![];
        let mut fn_names = vec![];
        for item in &m.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::ConstDecl(c) => const_names.push(c.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => {}
            }
        }

        // 5 structs: SDLWindow, SDLRenderer, SDLTexture, SDLFont, SDLEvent
        assert!(struct_names.contains(&"SDLWindow".to_string()));
        assert!(struct_names.contains(&"SDLRenderer".to_string()));
        assert!(struct_names.contains(&"SDLTexture".to_string()));
        assert!(struct_names.contains(&"SDLFont".to_string()));
        assert!(struct_names.contains(&"SDLEvent".to_string()));

        // Key constants
        assert!(const_names.contains(&"SDL_INIT_VIDEO".to_string()));
        assert!(const_names.contains(&"SDL_QUIT".to_string()));
        assert!(const_names.contains(&"SDLK_ESCAPE".to_string()));
        assert!(const_names.contains(&"SDL_WINDOW_SHOWN".to_string()));
        assert!(const_names.contains(&"SDL_RENDERER_ACCELERATED".to_string()));

        // Key functions
        assert!(fn_names.contains(&"sdl_init".to_string()));
        assert!(fn_names.contains(&"sdl_quit".to_string()));
        assert!(fn_names.contains(&"sdl_create_window".to_string()));
        assert!(fn_names.contains(&"sdl_create_renderer".to_string()));
        assert!(fn_names.contains(&"sdl_poll_event".to_string()));
        assert!(fn_names.contains(&"sdl_delay".to_string()));
        assert!(fn_names.contains(&"sdl_load_font".to_string()));
        assert!(fn_names.contains(&"sdl_draw_text".to_string()));

        // SDLEvent struct has user-visible fields
        let event = m.items.iter().find(|i| matches!(&i.node, Item::Struct(s) if s.name.node == "SDLEvent")).unwrap();
        if let Item::Struct(s) = &event.node {
            assert_eq!(s.fields.len(), 5);
            let field_names: Vec<_> = s.fields.iter().map(|f| f.node.name.node.clone()).collect();
            assert!(field_names.contains(&"event_type".to_string()));
            assert!(field_names.contains(&"key_code".to_string()));
            assert!(field_names.contains(&"mouse_x".to_string()));
        }
    }

    #[test]
    fn is_stdlib_ssh() {
        assert!(is_stdlib_module(&["std".into(), "ssh".into()]));
    }

    #[test]
    fn generate_ssh_returns_none() {
        // std.ssh is file-based, not synthetic — generate returns None
        assert!(generate_stdlib_module(&["std".into(), "ssh".into()]).is_none());
    }

    #[test]
    fn ssh_module_source_exists() {
        let source = stdlib_module_source(&["std".into(), "ssh".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Session"));
        assert!(src.contains("struct CommandResult"));
        assert!(src.contains("ssh_connect"));
        assert!(src.contains("channel_exec"));
    }

    #[test]
    fn ssh_source_parses() {
        let source = stdlib_module_source(&["std".into(), "ssh".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "ssh.gg parse errors: {:?}", parser.errors);

        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }

        assert!(struct_names.contains(&"Session".to_string()));
        assert!(struct_names.contains(&"CommandResult".to_string()));
        assert!(fn_names.contains(&"ssh_connect".to_string()));
        assert!(fn_names.contains(&"send_packet".to_string()));
        assert!(fn_names.contains(&"read_packet".to_string()));
        assert_eq!(equip_count, 1);
    }

    #[test]
    fn is_stdlib_crypto() {
        assert!(is_stdlib_module(&["std".into(), "crypto".into()]));
    }

    #[test]
    fn generate_crypto() {
        let m = generate_stdlib_module(&["std".into(), "crypto".into()]).unwrap();
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        for item in &m.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => {}
            }
        }
        assert!(struct_names.contains(&"CipherContext".to_string()));
        assert!(struct_names.contains(&"BigNum".to_string()));
        assert!(struct_names.contains(&"RSAKey".to_string()));
        assert!(fn_names.contains(&"crypto_sha256".to_string()));
        assert!(fn_names.contains(&"crypto_sha1".to_string()));
        assert!(fn_names.contains(&"crypto_hmac".to_string()));
        assert!(fn_names.contains(&"crypto_aes_ctr_new".to_string()));
        assert!(fn_names.contains(&"crypto_bn_from_bytes".to_string()));
        assert!(fn_names.contains(&"crypto_bn_to_bytes".to_string()));
        assert!(fn_names.contains(&"crypto_bn_mod_exp".to_string()));
        assert!(fn_names.contains(&"crypto_rsa_load_public".to_string()));
        assert!(fn_names.contains(&"crypto_rsa_verify".to_string()));
        assert!(fn_names.contains(&"crypto_random_bytes".to_string()));
    }

    #[test]
    fn is_stdlib_net_socket() {
        assert!(is_stdlib_module(&["std".into(), "net".into(), "socket".into()]));
    }

    #[test]
    fn generate_socket() {
        let m = generate_stdlib_module(&["std".into(), "net".into(), "socket".into()]).unwrap();
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        let mut equip_method_names: Vec<String> = vec![];
        for item in &m.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(e) => {
                    equip_count += 1;
                    for method in &e.items {
                        equip_method_names.push(method.node.name.node.clone());
                    }
                }
                _ => {}
            }
        }
        assert!(struct_names.contains(&"Socket".to_string()));
        assert!(fn_names.contains(&"socket_connect".to_string()));
        assert_eq!(equip_count, 1);
        assert!(equip_method_names.contains(&"read".to_string()));
        assert!(equip_method_names.contains(&"write".to_string()));
        assert!(equip_method_names.contains(&"close".to_string()));
    }

    #[test]
    fn is_stdlib_bytes() {
        assert!(is_stdlib_module(&["std".into(), "bytes".into()]));
    }

    #[test]
    fn generate_bytes() {
        let m = generate_stdlib_module(&["std".into(), "bytes".into()]).unwrap();
        let mut fn_names = vec![];
        for item in &m.items {
            match &item.node {
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => {}
            }
        }
        assert!(fn_names.contains(&"bytes_from_str".to_string()));
        assert!(fn_names.contains(&"bytes_to_str".to_string()));
        assert!(fn_names.contains(&"bytes_from_hex".to_string()));
        assert!(fn_names.contains(&"bytes_to_hex".to_string()));
        assert!(fn_names.contains(&"bytes_write_u32_be".to_string()));
        assert!(fn_names.contains(&"bytes_read_u32_be".to_string()));
        assert!(fn_names.contains(&"bytes_concat".to_string()));
        assert!(fn_names.contains(&"bytes_slice".to_string()));
        assert!(fn_names.contains(&"random_bytes".to_string()));
    }

    #[test]
    fn is_stdlib_json() {
        assert!(is_stdlib_module(&["std".into(), "json".into()]));
    }

    #[test]
    fn generate_json_returns_none() {
        // std.json is file-based, not synthetic — generate returns None
        assert!(generate_stdlib_module(&["std".into(), "json".into()]).is_none());
    }

    #[test]
    fn json_module_source_exists() {
        let source = stdlib_module_source(&["std".into(), "json".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("enum Json"));
        assert!(src.contains("json_parse"));
        assert!(src.contains("json_stringify"));
        assert!(src.contains("json_pretty"));
        assert!(src.contains("equip Json"));
    }

    #[test]
    fn json_source_parses() {
        let source = stdlib_module_source(&["std".into(), "json".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "json.gg parse errors: {:?}", parser.errors);

        let mut enum_names = vec![];
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Enum(e) => enum_names.push(e.name.node.clone()),
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }

        assert!(enum_names.contains(&"Json".to_string()));
        assert!(struct_names.contains(&"JsonParser".to_string()));
        assert!(fn_names.contains(&"json_parse".to_string()));
        assert!(fn_names.contains(&"json_stringify".to_string()));
        assert!(fn_names.contains(&"json_pretty".to_string()));
        assert_eq!(equip_count, 2); // equip JsonParser + equip Json
    }

    #[test]
    fn is_stdlib_toml() {
        assert!(is_stdlib_module(&["std".into(), "toml".into()]));
    }

    #[test]
    fn generate_toml_returns_none() {
        assert!(generate_stdlib_module(&["std".into(), "toml".into()]).is_none());
    }

    #[test]
    fn toml_module_source_exists() {
        let source = stdlib_module_source(&["std".into(), "toml".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("enum TomlValue"));
        assert!(src.contains("toml_parse"));
        assert!(src.contains("toml_stringify"));
        assert!(src.contains("equip TomlValue"));
        assert!(src.contains("Arr(Vector[TomlValue])"));
        assert!(src.contains("Tbl(Dict[str, TomlValue])"));
    }

    #[test]
    fn toml_source_parses() {
        let source = stdlib_module_source(&["std".into(), "toml".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "toml.gg parse errors: {:?}", parser.errors);

        let mut enum_names = vec![];
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Enum(e) => enum_names.push(e.name.node.clone()),
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }

        assert!(enum_names.contains(&"TomlValue".to_string()));
        assert!(struct_names.contains(&"TomlParser".to_string()));
        assert!(fn_names.contains(&"toml_parse".to_string()));
        assert!(fn_names.contains(&"toml_stringify".to_string()));
        assert_eq!(equip_count, 2); // equip TomlParser + equip TomlValue
    }

    #[test]
    fn is_stdlib_xml() {
        assert!(is_stdlib_module(&["std".into(), "xml".into()]));
    }

    #[test]
    fn generate_xml_returns_none() {
        // std.xml is file-based, not synthetic — generate returns None
        assert!(generate_stdlib_module(&["std".into(), "xml".into()]).is_none());
    }

    #[test]
    fn xml_module_source_exists() {
        let source = stdlib_module_source(&["std".into(), "xml".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("enum XmlNode"));
        assert!(src.contains("xml_parse"));
        assert!(src.contains("xml_stringify"));
        assert!(src.contains("equip XmlNode"));
        assert!(src.contains("equip XmlParser"));
    }

    #[test]
    fn xml_source_parses() {
        let source = stdlib_module_source(&["std".into(), "xml".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "xml.gg parse errors: {:?}", parser.errors);

        let mut enum_names = vec![];
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Enum(e) => enum_names.push(e.name.node.clone()),
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }

        assert!(enum_names.contains(&"XmlNode".to_string()));
        assert!(struct_names.contains(&"XmlParser".to_string()));
        assert!(fn_names.contains(&"xml_parse".to_string()));
        assert!(fn_names.contains(&"xml_stringify".to_string()));
        assert_eq!(equip_count, 2); // equip XmlParser + equip XmlNode
    }

    #[test]
    fn is_stdlib_http_client() {
        assert!(is_stdlib_module(&["std".into(), "http".into(), "client".into()]));
    }

    #[test]
    fn generate_http_client() {
        let m = generate_stdlib_module(&["std".into(), "http".into(), "client".into()]).unwrap();
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        let mut equip_method_names: Vec<String> = vec![];
        for item in &m.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(e) => {
                    equip_count += 1;
                    for method in &e.items {
                        equip_method_names.push(method.node.name.node.clone());
                    }
                }
                _ => {}
            }
        }
        assert!(struct_names.contains(&"Response".to_string()));
        assert!(struct_names.contains(&"Client".to_string()));
        assert!(fn_names.contains(&"get".to_string()));
        assert!(fn_names.contains(&"post".to_string()));
        assert!(fn_names.contains(&"put".to_string()));
        assert!(fn_names.contains(&"delete".to_string()));
        assert!(fn_names.contains(&"patch".to_string()));
        assert!(fn_names.contains(&"head".to_string()));
        // equip blocks for Response and Client
        assert_eq!(equip_count, 2);
        // Response methods
        assert!(equip_method_names.contains(&"status".to_string()));
        assert!(equip_method_names.contains(&"body".to_string()));
        // Client methods
        assert!(equip_method_names.contains(&"base_url".to_string()));
        assert!(equip_method_names.contains(&"timeout".to_string()));
    }

    #[test]
    fn http_response_methods_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "http".into(), "client".into()]).unwrap();
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "Response" {
                        for method in &eq.items {
                            assert!(
                                matches!(method.node.body, FunctionBody::Extern(_)),
                                "Response.{} should be FunctionBody::Extern",
                                method.node.name.node
                            );
                        }
                        return;
                    }
                }
            }
        }
        panic!("Response equip block not found");
    }

    #[test]
    fn http_client_simple_methods_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "http".into(), "client".into()]).unwrap();
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "Client" {
                        for method in &eq.items {
                            match method.node.name.node.as_str() {
                                "base_url" | "header" | "timeout" => {
                                    assert!(
                                        matches!(method.node.body, FunctionBody::Extern(_)),
                                        "Client.{} should be FunctionBody::Extern",
                                        method.node.name.node
                                    );
                                }
                                "get" | "post" | "put" | "delete" | "patch" | "head" => {
                                    assert!(
                                        matches!(method.node.body, FunctionBody::Declaration),
                                        "Client.{} should be FunctionBody::Declaration",
                                        method.node.name.node
                                    );
                                }
                                _ => {}
                            }
                        }
                        return;
                    }
                }
            }
        }
        panic!("Client equip block not found");
    }

    #[test]
    fn socket_methods_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "net".into(), "socket".into()]).unwrap();
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "Socket" {
                        let names: Vec<&str> =
                            eq.items.iter().map(|m| m.node.name.node.as_str()).collect();
                        assert_eq!(
                            names,
                            vec![
                                "read",
                                "read_exact",
                                "write",
                                "write_str",
                                "read_line",
                                "set_timeout",
                                "close"
                            ]
                        );
                        for method in &eq.items {
                            assert!(
                                matches!(method.node.body, FunctionBody::Extern(_)),
                                "Socket.{} should be FunctionBody::Extern",
                                method.node.name.node
                            );
                        }
                        return;
                    }
                }
            }
        }
        panic!("Socket equip block not found");
    }

    #[test]
    fn cipher_methods_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "crypto".into()]).unwrap();
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "CipherContext" {
                        let names: Vec<&str> =
                            eq.items.iter().map(|m| m.node.name.node.as_str()).collect();
                        assert_eq!(names, vec!["encrypt", "decrypt"]);
                        for method in &eq.items {
                            assert!(
                                matches!(method.node.body, FunctionBody::Extern(_)),
                                "CipherContext.{} should be FunctionBody::Extern",
                                method.node.name.node
                            );
                        }
                        return;
                    }
                }
            }
        }
        panic!("CipherContext equip block not found");
    }

    #[test]
    fn file_methods_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "collections".into()]).unwrap();
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "File" {
                        let names: Vec<&str> =
                            eq.items.iter().map(|m| m.node.name.node.as_str()).collect();
                        assert_eq!(names, vec!["read_all", "write", "close"]);
                        for method in &eq.items {
                            assert!(
                                matches!(method.node.body, FunctionBody::Extern(_)),
                                "File.{} should be FunctionBody::Extern",
                                method.node.name.node
                            );
                        }
                        return;
                    }
                }
            }
        }
        panic!("File equip block not found");
    }

    #[test]
    fn crypto_free_functions_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "crypto".into()]).unwrap();
        let extern_expected = [
            "crypto_sha256",
            "crypto_sha1",
            "crypto_bn_from_bytes",
            "crypto_bn_to_bytes",
            "crypto_bn_mod_exp",
            "crypto_rsa_verify",
        ];
        let decl_expected = [
            "crypto_rsa_load_public",
            "crypto_hmac",
            "crypto_aes_ctr_new",
            "crypto_random_bytes",
        ];
        for item in &m.items {
            if let Item::Function(f) = &item.node {
                let name = f.name.node.as_str();
                if extern_expected.contains(&name) {
                    assert!(
                        matches!(f.body, FunctionBody::Extern(_)),
                        "{name} should be FunctionBody::Extern"
                    );
                } else if decl_expected.contains(&name) {
                    assert!(
                        matches!(f.body, FunctionBody::Declaration),
                        "{name} should be FunctionBody::Declaration (Result wrapping)"
                    );
                }
            }
        }
    }

    #[test]
    fn bytes_free_functions_are_extern() {
        let m = generate_stdlib_module(&["std".into(), "bytes".into()]).unwrap();
        let extern_expected = [
            "bytes_from_str",
            "bytes_to_str",
            "bytes_from_hex",
            "bytes_to_hex",
            "bytes_write_u32_be",
            "bytes_read_u32_be",
            "bytes_write_u16_be",
            "bytes_read_u16_be",
            "bytes_concat",
            "bytes_slice",
            "random_bytes",
        ];
        for item in &m.items {
            if let Item::Function(f) = &item.node {
                let name = f.name.node.as_str();
                if extern_expected.contains(&name) {
                    assert!(
                        matches!(f.body, FunctionBody::Extern(_)),
                        "{name} should be FunctionBody::Extern"
                    );
                }
            }
        }
    }
}
