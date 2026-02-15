/// Virtual stdlib module system.
///
/// When the loader encounters `std.*` imports, it generates synthetic
/// `Module` ASTs with `FunctionDef` / `FunctionBody::Declaration` nodes.
/// No filesystem files needed — the C runtime already has the implementations.
///
/// All synthetic defs use `Span::dummy()`, which distinguishes them from
/// user-defined code and enables the `is_stdlib_call()` guard in codegen.
use crate::parser::ast::*;
use crate::span::{Span, Spanned};

/// Check if an import path refers to a stdlib module.
pub fn is_stdlib_module(segments: &[String]) -> bool {
    if segments.first().map(|s| s.as_str()) != Some("std") {
        return false;
    }
    match segments.len() {
        2 => matches!(segments[1].as_str(), "fs" | "path" | "os" | "conv" | "io" | "random" | "time" | "collections" | "math" | "fmt" | "process" | "sdl" | "gfx" | "ecs"),
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
            "gfx" => None, // file-based module — loaded via stdlib_module_source()
            "ecs" => None, // file-based module — loaded via stdlib_module_source()
            _ => None,
        },
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
        decl_fn("int_to_str", &[("n", ty_int())], ty_str()),
        decl_fn("float_to_str", &[("x", ty_float())], ty_str()),
        decl_fn("bool_to_str", &[("b", ty_bool())], ty_str()),
        decl_fn("char_to_str", &[("c", ty_char())], ty_str()),
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
    let items = type_defs
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
        assert_eq!(m.items.len(), 8);
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
        assert_eq!(m.items.len(), 9);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Struct(s) => s.name.node.clone(),
            _ => panic!("expected struct"),
        }).collect();
        assert!(names.contains(&"Vector".to_string()));
        assert!(names.contains(&"Dict".to_string()));
        assert!(names.contains(&"Set".to_string()));
        assert!(names.contains(&"Box".to_string()));
        assert!(names.contains(&"File".to_string()));
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
}
