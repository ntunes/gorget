/// Built-in module system — covers both `std` (core) and `gg` (batteries).
///
/// The namespace split:
/// - `std.*` — lean, stable building blocks: collections, I/O, math, OS, net, …
/// - `gg.*`  — batteries-included ecosystem: JSON, TOML, XML, YAML, CSV,
///              crypto, regex, SDL, GFX, ECS, SSH, HTTP, P2P, …
///
/// Both namespaces use one of two module strategies:
///
/// ## Synthetic modules (`generate_builtin_module` returns `Some`)
///
/// The module AST is built in Rust via helper functions (`gen_fs_module`, etc.).
/// Function bodies are either `FunctionBody::Declaration` (codegen emits a
/// hardcoded C call, often with Result/Option wrapping) or `FunctionBody::Extern`
/// (codegen emits a direct call to the named C symbol). The C implementations
/// live in `c_runtime.rs`.
///
/// Use synthetic modules when the API is a thin wrapper over C functions that
/// already exist in the runtime — the Gorget source would just be boilerplate
/// `extern` declarations with no real logic.
///
/// Examples: `std.fs`, `std.os`, `std.conv`, `std.math`, `gg.crypto`,
/// `std.net.socket`, `gg.sdl`, `gg.regex`.
///
/// ## File-based modules (`builtin_module_source` returns `Some`)
///
/// The module is written in Gorget as a `.gg` file under `lib/std/` (for `std.*`)
/// or `lib/gg/` (for `gg.*`). The loader reads the source via `include_str!`,
/// parses it, recursively resolves its imports, and merges the resulting AST into
/// the main module. Semantic analysis (name resolution, type checking, borrow
/// checking) runs on the merged result — the file-based module code is fully
/// checked, not trusted.
///
/// Use file-based modules when the module contains substantial Gorget logic
/// (parsers, data structures, algorithms) that benefits from being written and
/// tested in the language itself. File-based modules can import other built-in
/// modules and use all language features.
///
/// Examples: `gg.json`, `gg.toml`, `gg.xml`, `gg.yaml`, `gg.csv`,
/// `std.bytes`, `std.encoding`, `gg.gfx`, `gg.ecs`, `gg.ssh`, `gg.http`.
///
/// ## Adding a new module
///
/// 1. Choose synthetic if it's pure C-runtime glue, file-based if it has logic.
/// 2. Add the module name to `is_builtin_module`.
/// 3. For synthetic: add a `gen_*_module()` function returning the AST.
///    For file-based in `std`: create `lib/std/<name>.gg`, add `None` to
///    `generate_builtin_module`, add `include_str!` to `builtin_module_source`.
///    For file-based in `gg`: create `lib/gg/<name>.gg` instead.
/// 4. Add unit tests (at minimum: is_builtin, generate returns correct variant,
///    source exists/parses for file-based modules).
///
/// All synthetic defs use `Span::dummy()`, which distinguishes them from
/// user-defined code and enables the `is_stdlib_call()` guard in codegen.
use crate::parser::ast::*;
use crate::span::{Span, Spanned};

/// Check if an import path refers to a built-in module (`std.*` or `gg.*`).
pub fn is_builtin_module(segments: &[String]) -> bool {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.len() {
            2 => matches!(segments[1].as_str(),
                "fs" | "path" | "os" | "conv" | "io" | "random" | "time"
                | "collections" | "math" | "fmt" | "process" | "bytes"
                | "encoding" | "channel" | "alloc" | "term" | "heap" | "datetime"
                | "sync" | "thread" | "async"),
            3 => segments[1] == "net" && matches!(segments[2].as_str(), "socket" | "tls" | "udp"),
            _ => false,
        },
        Some("gg") => segments.len() == 2 && matches!(segments[1].as_str(),
            "json" | "toml" | "xml" | "yaml" | "csv" | "crypto" | "regex"
            | "sdl" | "gfx" | "ecs" | "ssh" | "http" | "p2p"
            | "uuid" | "log" | "cli" | "tensor" | "dataframe"),
        _ => false,
    }
}

/// Generate a synthetic `Module` for a built-in module.
pub fn generate_builtin_module(segments: &[String]) -> Option<Module> {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.len() {
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
                "fmt" => None, // file-based module — loaded via builtin_module_source()
                "process" => Some(gen_process_module()),
                "bytes" => None, // file-based module — loaded via builtin_module_source()
                "encoding" => None, // file-based module — loaded via builtin_module_source()
                "channel" => Some(gen_channel_module()),
                "alloc" => Some(gen_alloc_module()),
                "sync" => Some(gen_sync_module()),
                "thread" => Some(gen_thread_module()),
                "async" => Some(gen_async_module()),
                "term" => None,     // file-based module — loaded via builtin_module_source()
                "heap" => None,     // file-based module — loaded via builtin_module_source()
                "datetime" => None, // file-based module — loaded via builtin_module_source()
                _ => None,
            },
            3 if segments[1] == "net" && segments[2] == "socket" => Some(gen_socket_module()),
            3 if segments[1] == "net" && segments[2] == "tls" => Some(gen_tls_socket_module()),
            3 if segments[1] == "net" && segments[2] == "udp" => Some(gen_udp_socket_module()),
            _ => None,
        },
        Some("gg") if segments.len() == 2 => match segments[1].as_str() {
            "sdl" => Some(gen_sdl_module()),
            "crypto" => Some(gen_crypto_module()),
            "regex" => Some(gen_regex_module()),
            "json" => None, // file-based module — loaded via builtin_module_source()
            "toml" => None, // file-based module — loaded via builtin_module_source()
            "xml" => None,  // file-based module — loaded via builtin_module_source()
            "yaml" => None, // file-based module — loaded via builtin_module_source()
            "csv" => None,  // file-based module — loaded via builtin_module_source()
            "gfx" => None,  // file-based module — loaded via builtin_module_source()
            "ecs" => None,  // file-based module — loaded via builtin_module_source()
            "ssh" => None,  // file-based module — loaded via builtin_module_source()
            "http" => None, // file-based module — loaded via builtin_module_source()
            "p2p" => None,  // file-based module — loaded via builtin_module_source()
            "uuid" => None, // file-based module — loaded via builtin_module_source()
            "log" => None,  // file-based module — loaded via builtin_module_source()
            "cli" => None,  // file-based module — loaded via builtin_module_source()
            "tensor" => None,    // file-based module — loaded via builtin_module_source()
            "dataframe" => None, // file-based module — loaded via builtin_module_source()
            _ => None,
        },
        _ => None,
    }
}

// ─── Module Generators ──────────────────────────────────────

fn gen_fs_module() -> Module {
    make_module(vec![
        decl_fn("read_file", &[("path", ty_str())], ty_string()),
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
        decl_fn("path_join", &[("a", ty_str()), ("b", ty_str())], ty_string()),
        decl_fn("path_parent", &[("path", ty_str())], ty_string()),
        decl_fn("path_basename", &[("path", ty_str())], ty_string()),
        decl_fn("path_extension", &[("path", ty_str())], ty_string()),
        decl_fn("path_stem", &[("path", ty_str())], ty_string()),
    ])
}

fn gen_os_module() -> Module {
    make_module(vec![
        decl_fn("exit", &[("code", ty_int())], ty_void()),
        decl_fn("getenv", &[("name", ty_str())], ty_str()),
        decl_fn("setenv", &[("name", ty_str()), ("value", ty_str())], ty_void()),
        decl_fn("getcwd", &[], ty_string()),
        decl_fn("platform", &[], ty_str()),
        decl_fn("args", &[], ty_vector_str()),
        decl_fn("readdir", &[("path", ty_str())], ty_vector_str()),
    ])
}

fn gen_conv_module() -> Module {
    make_module(vec![
        decl_fn("ord", &[("c", ty_char())], ty_int()),
        decl_fn("chr", &[("n", ty_int())], ty_char()),
        decl_fn("parse_int", &[("s", ty_str())], ty_result(ty_int(), ty_str())),
        decl_fn("parse_float", &[("s", ty_str())], ty_result(ty_float(), ty_str())),
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
        decl_fn("input", &[("prompt", ty_str())], ty_string()),
    )));
    items.push(Spanned::dummy(Item::Function(
        decl_fn("readline", &[], ty_string()),
    )));
    items.push(Spanned::dummy(Item::Function(
        decl_fn("stdin_eof", &[], ty_bool()),
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
        decl_async_fn("sleep", &[("seconds", ty_float())], ty_void()),
        decl_fn("format_time", &[("epoch", ty_int()), ("fmt", ty_str())], ty_string()),
        decl_fn("parse_time", &[("s", ty_str()), ("fmt", ty_str())], ty_int()),
    ])
}

/// std.async — non-blocking I/O helpers backed by the GorgetReactor (timerfd/kqueue).
/// async_sleep(ms: int) suspends the current task for `ms` milliseconds using the reactor.
fn gen_async_module() -> Module {
    make_module(vec![
        decl_fn("async_sleep", &[("ms", ty_int())], ty_void()),
    ])
}

fn gen_math_module() -> Module {
    let float_const = |name: &str, value: f64| -> Spanned<Item> {
        Spanned::dummy(Item::ConstDecl(ConstDecl {
            visibility: Visibility::Public,
            type_: Spanned::dummy(ty_float()),
            name: Spanned::dummy(name.to_string()),
            value: Spanned::dummy(Expr::FloatLiteral(value)),
            span: Span::dummy(),
        }))
    };
    let fn_item = |f: FunctionDef| -> Spanned<Item> {
        Spanned::dummy(Item::Function(f))
    };
    let items = vec![
        // Constants
        float_const("PI", std::f64::consts::PI),
        float_const("E", std::f64::consts::E),
        float_const("TAU", std::f64::consts::TAU),
        float_const("INFINITY", f64::INFINITY),
        float_const("NAN", f64::NAN),
        // Integer math
        fn_item(decl_fn("abs", &[("x", ty_int())], ty_int())),
        fn_item(decl_fn("min", &[("a", ty_int()), ("b", ty_int())], ty_int())),
        fn_item(decl_fn("max", &[("a", ty_int()), ("b", ty_int())], ty_int())),
        // Float math
        fn_item(decl_fn("sqrt", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("pow", &[("base", ty_float()), ("exp", ty_float())], ty_float())),
        fn_item(decl_fn("floor", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("ceil", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("round", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("log", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("log2", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("log10", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("sin", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("cos", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("tan", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("asin", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("acos", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("atan", &[("x", ty_float())], ty_float())),
        fn_item(decl_fn("atan2", &[("y", ty_float()), ("x", ty_float())], ty_float())),
    ];
    Module {
        items,
        span: Span::dummy(),
    }
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
    let ty_process = || Type::Named {
        name: Spanned::dummy("Process".to_string()),
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
    // Process is an opaque struct wrapping pid + pipe fds.
    let process_struct = opaque_struct("Process");
    let process_equip = equip_block("Process", vec![
        decl_method("wait", Ownership::Borrow, &[], ty_int()),
        decl_method("kill", Ownership::Borrow, &[], ty_void()),
        decl_method("pid", Ownership::Borrow, &[], ty_int()),
        decl_method("write_stdin", Ownership::Borrow, &[("data", ty_str())], ty_void()),
        decl_method("close_stdin", Ownership::Borrow, &[], ty_void()),
        decl_method("read_stdout", Ownership::Borrow, &[], ty_string()),
        decl_method("read_stderr", Ownership::Borrow, &[], ty_string()),
    ]);
    let items = vec![
        Spanned::dummy(Item::Struct(struct_def)),
        process_struct,
        process_equip,
        Spanned::dummy(Item::Function(decl_fn("exec", &[("cmd", ty_str())], ty_int()))),
        Spanned::dummy(Item::Function(decl_fn("exec_output", &[("cmd", ty_str())], exec_result_type))),
        Spanned::dummy(Item::Function(decl_fn(
            "process_spawn",
            &[("program", ty_str()), ("args", ty_vector_str())],
            ty_result(ty_process(), ty_str()),
        ))),
        Spanned::dummy(Item::Function(decl_fn("getpid", &[], ty_int()))),
    ];
    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_sync_module() -> Module {
    let ty_t = || Type::Named {
        name: Spanned::dummy("T".to_string()),
        generic_args: vec![],
    };
    let ty_read_guard = |t: Type| Type::Named {
        name: Spanned::dummy("ReadGuard".to_string()),
        generic_args: vec![Spanned::dummy(t)],
    };
    let ty_write_guard = |t: Type| Type::Named {
        name: Spanned::dummy("WriteGuard".to_string()),
        generic_args: vec![Spanned::dummy(t)],
    };

    // AtomicInt — non-generic opaque struct
    let atomic_int_struct = opaque_struct("AtomicInt");
    let atomic_int_equip = equip_block("AtomicInt", vec![
        decl_method("load", Ownership::Borrow, &[], ty_int()),
        decl_method("store", Ownership::Borrow, &[("val", ty_int())], ty_void()),
        decl_method("add", Ownership::Borrow, &[("val", ty_int())], ty_int()),
        decl_method("sub", Ownership::Borrow, &[("val", ty_int())], ty_int()),
        decl_method("compare_exchange", Ownership::Borrow, &[("expected", ty_int()), ("desired", ty_int())], ty_bool()),
    ]);

    // AtomicBool — non-generic opaque struct
    let atomic_bool_struct = opaque_struct("AtomicBool");
    let atomic_bool_equip = equip_block("AtomicBool", vec![
        decl_method("load", Ownership::Borrow, &[], ty_bool()),
        decl_method("store", Ownership::Borrow, &[("val", ty_bool())], ty_void()),
        decl_method("swap", Ownership::Borrow, &[("val", ty_bool())], ty_bool()),
        decl_method("compare_exchange", Ownership::Borrow, &[("expected", ty_bool()), ("desired", ty_bool())], ty_bool()),
    ]);

    // Barrier — non-generic opaque struct
    let barrier_struct = opaque_struct("Barrier");
    let barrier_equip = equip_block("Barrier", vec![
        decl_method("wait", Ownership::Borrow, &[], ty_void()),
    ]);

    // CondVar — non-generic opaque struct
    let condvar_struct = opaque_struct("CondVar");
    // CondVar.wait takes a Guard[T] argument; declared as a generic guard placeholder.
    let ty_guard_bool = Type::Named {
        name: Spanned::dummy("Guard".to_string()),
        generic_args: vec![Spanned::dummy(ty_bool())],
    };
    let condvar_equip = equip_block("CondVar", vec![
        decl_method("notify_one", Ownership::Borrow, &[], ty_void()),
        decl_method("notify_all", Ownership::Borrow, &[], ty_void()),
        decl_method("wait", Ownership::Borrow, &[("guard", ty_guard_bool)], ty_void()),
    ]);

    // RWLock[T] — generic, follows Mutex[T] pattern
    let rwlock_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("RWLock".to_string()),
        generic_params: Some(Spanned::dummy(GenericParams {
            params: vec![Spanned::dummy(GenericParam::Type {
                name: Spanned::dummy("T".to_string()),
                bounds: vec![],
            })],
        })),
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let rwlock_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("RWLock".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("read", Ownership::Borrow, &[], ty_read_guard(ty_t()))),
            Spanned::dummy(decl_method("write", Ownership::Borrow, &[], ty_write_guard(ty_t()))),
        ],
        span: Span::dummy(),
    }));

    // ReadGuard[T] — generic, Move + RAII unlock
    let read_guard_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("ReadGuard".to_string()),
        generic_params: Some(Spanned::dummy(GenericParams {
            params: vec![Spanned::dummy(GenericParam::Type {
                name: Spanned::dummy("T".to_string()),
                bounds: vec![],
            })],
        })),
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let read_guard_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("ReadGuard".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("get", Ownership::Borrow, &[], ty_t())),
        ],
        span: Span::dummy(),
    }));

    // WriteGuard[T] — generic, Move + RAII unlock
    let write_guard_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("WriteGuard".to_string()),
        generic_params: Some(Spanned::dummy(GenericParams {
            params: vec![Spanned::dummy(GenericParam::Type {
                name: Spanned::dummy("T".to_string()),
                bounds: vec![],
            })],
        })),
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let write_guard_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("WriteGuard".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("get", Ownership::Borrow, &[], ty_t())),
            Spanned::dummy(decl_method("set", Ownership::Borrow, &[("val", ty_t())], ty_void())),
        ],
        span: Span::dummy(),
    }));

    Module {
        items: vec![
            atomic_int_struct, atomic_int_equip,
            atomic_bool_struct, atomic_bool_equip,
            barrier_struct, barrier_equip,
            condvar_struct, condvar_equip,
            rwlock_struct, rwlock_equip,
            read_guard_struct, read_guard_equip,
            write_guard_struct, write_guard_equip,
        ],
        span: Span::dummy(),
    }
}

fn gen_thread_module() -> Module {
    let ty_t = || Type::Named {
        name: Spanned::dummy("T".to_string()),
        generic_args: vec![],
    };
    let ty_callable_t = || Type::Function {
        return_type: Box::new(Spanned::dummy(ty_t())),
        params: vec![],
        param_ownerships: vec![],
    };

    // Thread[T] — generic, Move semantics (must be joined or leaked)
    let thread_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("Thread".to_string()),
        generic_params: Some(Spanned::dummy(GenericParams {
            params: vec![Spanned::dummy(GenericParam::Type {
                name: Spanned::dummy("T".to_string()),
                bounds: vec![],
            })],
        })),
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let thread_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("Thread".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("join", Ownership::Move, &[], ty_t())),
            Spanned::dummy(decl_method("id", Ownership::Borrow, &[], ty_int())),
        ],
        span: Span::dummy(),
    }));

    Module {
        items: vec![
            thread_struct,
            thread_equip,
            // thread_spawn(fn) → Thread[T]
            Spanned::dummy(Item::Function(decl_fn(
                "thread_spawn",
                &[("fn", ty_callable_t())],
                Type::Named {
                    name: Spanned::dummy("Thread".to_string()),
                    generic_args: vec![Spanned::dummy(ty_t())],
                },
            ))),
            Spanned::dummy(Item::Function(decl_fn("current_thread_id", &[], ty_int()))),
        ],
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
                                Spanned::dummy(GenericParam::Type { name: Spanned::dummy(
                                    param_name.to_string(),
                                ), bounds: vec![] })
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
        extern_method("read_all", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_file_read_all"),
        extern_method("write", Ownership::MutableBorrow, &[("content", ty_str())], ty_void(), "gorget_file_write"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_file_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

fn gen_sdl_module() -> Module {
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
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_create_window_try", &[("title", ty_str()), ("w", ty_int()), ("h", ty_int()), ("flags", ty_int())], ty_sdl_window()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_window_is_null", &[("win", ty_sdl_window())], ty_bool()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_error", &[], ty_str()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_destroy_window", &[("win", ty_sdl_window())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_window_width", &[("win", ty_sdl_window())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_window_height", &[("win", ty_sdl_window())], ty_int()))));

    // Renderer
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_create_renderer", &[("win", ty_sdl_window()), ("flags", ty_int())], ty_sdl_renderer()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_create_renderer_try", &[("win", ty_sdl_window()), ("flags", ty_int())], ty_sdl_renderer()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_renderer_is_null", &[("ren", ty_sdl_renderer())], ty_bool()))));
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

// ─── File-based built-in modules ────────────────────────────

/// Get embedded source for file-based built-in modules (`std.*` or `gg.*`).
/// These are real `.gg` files compiled into the binary, parsed and loaded
/// by the module loader (including recursive import resolution).
pub fn builtin_module_source(segments: &[String]) -> Option<&'static str> {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.get(1).map(|s| s.as_str()) {
            Some("fmt") => Some(include_str!("../lib/std/fmt.gg")),
            Some("bytes") => Some(include_str!("../lib/std/bytes.gg")),
            Some("encoding") => Some(include_str!("../lib/std/encoding.gg")),
            Some("term") => Some(include_str!("../lib/std/term.gg")),
            Some("heap") => Some(include_str!("../lib/std/heap.gg")),
            Some("datetime") => Some(include_str!("../lib/std/datetime.gg")),
            _ => None,
        },
        Some("gg") => match segments.get(1).map(|s| s.as_str()) {
            Some("gfx") => Some(include_str!("../lib/gg/gfx.gg")),
            Some("ecs") => Some(include_str!("../lib/gg/ecs.gg")),
            Some("ssh") => Some(include_str!("../lib/gg/ssh.gg")),
            Some("http") => Some(include_str!("../lib/gg/http.gg")),
            Some("json") => Some(include_str!("../lib/gg/json.gg")),
            Some("toml") => Some(include_str!("../lib/gg/toml.gg")),
            Some("xml") => Some(include_str!("../lib/gg/xml.gg")),
            Some("yaml") => Some(include_str!("../lib/gg/yaml.gg")),
            Some("csv") => Some(include_str!("../lib/gg/csv.gg")),
            Some("p2p") => Some(include_str!("../lib/gg/p2p.gg")),
            Some("uuid") => Some(include_str!("../lib/gg/uuid.gg")),
            Some("log") => Some(include_str!("../lib/gg/log.gg")),
            Some("cli") => Some(include_str!("../lib/gg/cli.gg")),
            Some("tensor") => Some(include_str!("../lib/gg/tensor.gg")),
            Some("dataframe") => Some(include_str!("../lib/gg/dataframe.gg")),
            _ => None,
        },
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

fn decl_async_fn(name: &str, params: &[(&str, Type)], ret: Type) -> FunctionDef {
    let mut f = decl_fn(name, params, ret);
    f.qualifiers.is_async = true;
    f
}

fn ty_str() -> Type {
    Type::Primitive(PrimitiveType::Str)
}

fn ty_cstr() -> Type {
    Type::Primitive(PrimitiveType::CStr)
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

fn ty_option(inner: Type) -> Type {
    Type::Named {
        name: Spanned::dummy("Option".to_string()),
        generic_args: vec![Spanned::dummy(inner)],
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

// ─── gg.crypto ──────────────────────────────────────────────

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
    let ty_ed25519_keypair = || Type::Named {
        name: Spanned::dummy("Ed25519KeyPair".to_string()),
        generic_args: vec![],
    };
    let ty_x25519_keypair = || Type::Named {
        name: Spanned::dummy("X25519KeyPair".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque structs
    items.push(opaque_struct("CipherContext"));
    items.push(opaque_struct("BigNum"));
    items.push(opaque_struct("RSAKey"));
    items.push(opaque_struct("Ed25519KeyPair"));
    items.push(opaque_struct("X25519KeyPair"));

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
        // Ed25519 — keygen returns Result, sign returns Result, verify is direct
        decl_fn("crypto_ed25519_keygen", &[], ty_result(ty_ed25519_keypair(), ty_str())),
        decl_fn("crypto_ed25519_sign", &[("private_key", ty_vector_uint8()), ("data", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str())),
        extern_fn("crypto_ed25519_verify", &[("public_key", ty_vector_uint8()), ("data", ty_vector_uint8()), ("signature", ty_vector_uint8())], ty_bool(), "gorget_crypto_ed25519_verify"),
        // X25519 ECDH — keygen and shared_secret return Result
        decl_fn("crypto_x25519_keygen", &[], ty_result(ty_x25519_keypair(), ty_str())),
        decl_fn("crypto_x25519_shared_secret", &[("private_key", ty_x25519_keypair()), ("peer_public", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str())),
        decl_fn("crypto_x25519_dh", &[("private_key_bytes", ty_vector_uint8()), ("peer_public", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str())),
        // HKDF-SHA256 — returns Result
        decl_fn("crypto_hkdf_sha256", &[("salt", ty_vector_uint8()), ("ikm", ty_vector_uint8()), ("info", ty_vector_uint8()), ("length", ty_int())], ty_result(ty_vector_uint8(), ty_str())),
        // AES-256-GCM — encrypt/decrypt return Result
        decl_fn("crypto_aes_gcm_encrypt", &[("key", ty_vector_uint8()), ("nonce", ty_vector_uint8()), ("plaintext", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str())),
        decl_fn("crypto_aes_gcm_decrypt", &[("key", ty_vector_uint8()), ("ciphertext", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str())),
    ];
    for f in fns {
        items.push(Spanned::dummy(Item::Function(f)));
    }

    // CipherContext methods — extern bindings
    items.push(equip_block("CipherContext", vec![
        extern_method("encrypt", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_vector_uint8(), "gorget_cipher_encrypt"),
        extern_method("decrypt", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_vector_uint8(), "gorget_cipher_decrypt"),
    ]));

    // Ed25519KeyPair methods — extern bindings
    items.push(equip_block("Ed25519KeyPair", vec![
        extern_method("public_key", Ownership::Borrow, &[], ty_vector_uint8(), "gorget_ed25519_public_key"),
        extern_method("private_key", Ownership::Borrow, &[], ty_vector_uint8(), "gorget_ed25519_private_key"),
    ]));

    // X25519KeyPair methods — extern bindings
    items.push(equip_block("X25519KeyPair", vec![
        extern_method("public_key", Ownership::Borrow, &[], ty_vector_uint8(), "gorget_crypto_x25519_public"),
        extern_method("private_key", Ownership::Borrow, &[], ty_vector_uint8(), "gorget_crypto_x25519_private"),
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
        extern_method("read_line", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_socket_read_line"),
        extern_method("set_timeout", Ownership::MutableBorrow, &[("ms", ty_int())], ty_void(), "gorget_socket_set_timeout"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_socket_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── std.net.udp ────────────────────────────────────────────

fn gen_udp_socket_module() -> Module {
    let ty_udp_socket = || Type::Named {
        name: Spanned::dummy("UdpSocket".to_string()),
        generic_args: vec![],
    };
    let ty_udp_addr = || Type::Named {
        name: Spanned::dummy("UdpAddr".to_string()),
        generic_args: vec![],
    };
    let ty_udp_packet = || Type::Named {
        name: Spanned::dummy("UdpPacket".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque struct: UdpSocket
    items.push(opaque_struct("UdpSocket"));

    // Struct with visible fields: UdpAddr { host: str, port: int }
    items.push(Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("UdpAddr".to_string()),
        generic_params: None,
        fields: vec![
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("host".to_string()),
                type_: Spanned::dummy(ty_str()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("port".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
        ],
        doc_comment: None,
        span: Span::dummy(),
    })));

    // Struct with visible fields: UdpPacket { data: Vector[uint8], sender: UdpAddr }
    items.push(Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("UdpPacket".to_string()),
        generic_params: None,
        fields: vec![
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("data".to_string()),
                type_: Spanned::dummy(ty_vector_uint8()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("sender".to_string()),
                type_: Spanned::dummy(ty_udp_addr()),
            }),
        ],
        doc_comment: None,
        span: Span::dummy(),
    })));

    // Free function: udp_bind(addr, port) -> Result[UdpSocket, str]
    items.push(Spanned::dummy(Item::Function(
        decl_fn("udp_bind", &[("addr", ty_str()), ("port", ty_int())], ty_result(ty_udp_socket(), ty_str())),
    )));

    // UdpSocket methods
    items.push(equip_block("UdpSocket", vec![
        // sendto(&self, data, host, port) -> Result[int, str]
        decl_method("sendto", Ownership::MutableBorrow, &[("data", ty_vector_uint8()), ("host", ty_str()), ("port", ty_int())], ty_result(ty_int(), ty_str())),
        // recvfrom(&self, max_bytes) -> Result[UdpPacket, str]
        decl_method("recvfrom", Ownership::MutableBorrow, &[("max_bytes", ty_int())], ty_result(ty_udp_packet(), ty_str())),
        // poll(&self, timeout_ms) -> bool
        extern_method("poll", Ownership::MutableBorrow, &[("timeout_ms", ty_int())], ty_bool(), "gorget_udp_poll"),
        // set_nonblocking(&self, enabled)
        extern_method("set_nonblocking", Ownership::MutableBorrow, &[("enabled", ty_bool())], ty_void(), "gorget_udp_set_nonblocking"),
        // join_multicast(&self, group_addr) -> Result[bool, str]
        decl_method("join_multicast", Ownership::MutableBorrow, &[("group_addr", ty_str())], ty_result(ty_bool(), ty_str())),
        // leave_multicast(&self, group_addr)
        extern_method("leave_multicast", Ownership::MutableBorrow, &[("group_addr", ty_str())], ty_void(), "gorget_udp_leave_multicast"),
        // set_multicast_loopback(&self, enabled)
        extern_method("set_multicast_loopback", Ownership::MutableBorrow, &[("enabled", ty_bool())], ty_void(), "gorget_udp_set_multicast_loopback"),
        // local_addr(self) -> UdpAddr
        extern_method("local_addr", Ownership::Borrow, &[], ty_udp_addr(), "gorget_udp_local_addr"),
        // close(&self)
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_udp_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── std.net.tls ────────────────────────────────────────────

fn gen_tls_socket_module() -> Module {
    let ty_tls_socket = || Type::Named {
        name: Spanned::dummy("TlsSocket".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque struct: TlsSocket
    items.push(opaque_struct("TlsSocket"));

    // Free function: tls_connect(host, port) -> Result[TlsSocket, str]
    items.push(Spanned::dummy(Item::Function(
        decl_fn("tls_connect", &[("host", ty_str()), ("port", ty_int())], ty_result(ty_tls_socket(), ty_str())),
    )));

    // TlsSocket methods — extern bindings
    items.push(equip_block("TlsSocket", vec![
        extern_method("read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_tls_read"),
        extern_method("read_exact", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_tls_read_exact"),
        extern_method("write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_tls_write"),
        extern_method("write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_tls_write_str"),
        extern_method("read_line", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_tls_read_line"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_tls_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── gg.regex ───────────────────────────────────────────────

fn gen_regex_module() -> Module {
    let ty_regex = || Type::Named {
        name: Spanned::dummy("Regex".to_string()),
        generic_args: vec![],
    };
    let ty_match = || Type::Named {
        name: Spanned::dummy("Match".to_string()),
        generic_args: vec![],
    };
    let ty_vector_match = || Type::Named {
        name: Spanned::dummy("Vector".to_string()),
        generic_args: vec![Spanned::dummy(ty_match())],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque structs
    items.push(opaque_struct("Regex"));
    items.push(opaque_struct("Match"));

    // Free functions
    // regex_compile → Result[Regex, str] (hardcoded dispatch)
    items.push(Spanned::dummy(Item::Function(
        decl_fn("regex_compile", &[("pattern", ty_str())], ty_result(ty_regex(), ty_str())),
    )));
    // regex_compile_with → Result[Regex, str] (hardcoded dispatch)
    items.push(Spanned::dummy(Item::Function(
        decl_fn("regex_compile_with", &[("pattern", ty_str()), ("flags", ty_str())], ty_result(ty_regex(), ty_str())),
    )));
    // regex_escape → String (extern)
    items.push(Spanned::dummy(Item::Function(
        extern_fn("regex_escape", &[("s", ty_str())], ty_string(), "gorget_regex_escape"),
    )));
    // regex_is_match → bool (hardcoded — compile, match, free)
    items.push(Spanned::dummy(Item::Function(
        decl_fn("regex_is_match", &[("pattern", ty_str()), ("subject", ty_str())], ty_bool()),
    )));
    // regex_find → Option[Match] (hardcoded — compile, find, free pattern)
    items.push(Spanned::dummy(Item::Function(
        decl_fn("regex_find", &[("pattern", ty_str()), ("subject", ty_str())], ty_option(ty_match())),
    )));
    // regex_replace → String (hardcoded — compile, replace, free pattern)
    items.push(Spanned::dummy(Item::Function(
        decl_fn("regex_replace", &[("pattern", ty_str()), ("subject", ty_str()), ("repl", ty_str())], ty_string()),
    )));

    // Regex methods
    items.push(equip_block("Regex", vec![
        // Direct extern bindings (no Result/Option wrapping)
        extern_method("is_match", Ownership::Borrow, &[("subject", ty_str())], ty_bool(), "gorget_regex_is_match"),
        extern_method("replace_all", Ownership::Borrow, &[("subject", ty_str()), ("replacement", ty_str())], ty_string(), "gorget_regex_replace_all"),
        extern_method("capture_count", Ownership::Borrow, &[], ty_int(), "gorget_regex_capture_count"),
        extern_method("pattern_str", Ownership::Borrow, &[], ty_str(), "gorget_regex_pattern_str"),
        extern_method("group_names", Ownership::Borrow, &[], ty_vector_str(), "gorget_regex_group_names"),
        // Result/Option-wrapping methods (hardcoded dispatch)
        decl_method("find", Ownership::Borrow, &[("subject", ty_str())], ty_option(ty_match())),
        decl_method("find_at", Ownership::Borrow, &[("subject", ty_str()), ("pos", ty_int())], ty_option(ty_match())),
        decl_method("find_all", Ownership::Borrow, &[("subject", ty_str())], ty_vector_match()),
        decl_method("replace", Ownership::Borrow, &[("subject", ty_str()), ("replacement", ty_str())], ty_string()),
        decl_method("split", Ownership::Borrow, &[("subject", ty_str())], ty_vector_str()),
        decl_method("splitn", Ownership::Borrow, &[("subject", ty_str()), ("limit", ty_int())], ty_vector_str()),
        decl_method("fullmatch", Ownership::Borrow, &[("subject", ty_str())], ty_option(ty_match())),
    ]));

    // Match methods (all extern — simple accessors)
    items.push(equip_block("Match", vec![
        extern_method("text", Ownership::Borrow, &[], ty_str(), "gorget_regex_match_text"),
        extern_method("start", Ownership::Borrow, &[], ty_int(), "gorget_regex_match_start"),
        extern_method("end_pos", Ownership::Borrow, &[], ty_int(), "gorget_regex_match_end"),
        extern_method("group_count", Ownership::Borrow, &[], ty_int(), "gorget_regex_match_group_count"),
        extern_method("groups", Ownership::Borrow, &[], ty_vector_str(), "gorget_regex_match_groups"),
        // group(n) → Option[str], group_by_name(name) → Option[str] (hardcoded dispatch)
        decl_method("group", Ownership::Borrow, &[("n", ty_int())], ty_option(ty_str())),
        decl_method("group_by_name", Ownership::Borrow, &[("name", ty_str())], ty_option(ty_str())),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── Helpers ────────────────────────────────────────────────

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

fn gen_alloc_module() -> Module {
    let arena_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("Arena".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let arena_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("Arena".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("bytes_used", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("reset", Ownership::Borrow, &[], ty_void())),
            Spanned::dummy(decl_method("destroy", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    let tracking_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("TrackingAllocator".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let tracking_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("TrackingAllocator".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("alloc_count", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("free_count", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("bytes_allocated", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("bytes_freed", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("current_bytes", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("peak_bytes", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("realloc_count", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("report", Ownership::Borrow, &[], ty_void())),
            Spanned::dummy(decl_method("reset", Ownership::Borrow, &[], ty_void())),
            Spanned::dummy(decl_method("destroy", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    let pool_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("PoolAllocator".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let pool_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("PoolAllocator".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("used_blocks", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("free_blocks", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("total_blocks", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("block_size", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("reset", Ownership::Borrow, &[], ty_void())),
            Spanned::dummy(decl_method("destroy", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    let tlsf_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("TlsfAllocator".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let tlsf_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("TlsfAllocator".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("bytes_used", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("peak_bytes", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("pool_size", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("reset", Ownership::Borrow, &[], ty_void())),
            Spanned::dummy(decl_method("destroy", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    Module {
        items: vec![arena_struct, arena_equip, tracking_struct, tracking_equip, pool_struct, pool_equip, tlsf_struct, tlsf_equip],
        span: Span::dummy(),
    }
}

fn gen_channel_module() -> Module {
    let ty_t = || Type::Named {
        name: Spanned::dummy("T".to_string()),
        generic_args: vec![],
    };
    let channel_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("Channel".to_string()),
        generic_params: Some(Spanned::dummy(GenericParams {
            params: vec![Spanned::dummy(GenericParam::Type { name: Spanned::dummy("T".to_string()), bounds: vec![] })],
        })),
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let channel_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("Channel".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("send", Ownership::Borrow, &[("value", ty_t())], ty_void())),
            Spanned::dummy(decl_method("recv", Ownership::Borrow, &[], ty_t())),
            Spanned::dummy(decl_method("close", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    Module {
        items: vec![channel_struct, channel_equip],
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_builtin() {
        // std.* core modules
        assert!(is_builtin_module(&["std".into(), "fs".into()]));
        assert!(is_builtin_module(&["std".into(), "path".into()]));
        assert!(is_builtin_module(&["std".into(), "os".into()]));
        assert!(is_builtin_module(&["std".into(), "conv".into()]));
        assert!(is_builtin_module(&["std".into(), "io".into()]));
        assert!(is_builtin_module(&["std".into(), "random".into()]));
        assert!(is_builtin_module(&["std".into(), "time".into()]));
        assert!(is_builtin_module(&["std".into(), "math".into()]));
        assert!(is_builtin_module(&["std".into(), "fmt".into()]));
        assert!(is_builtin_module(&["std".into(), "process".into()]));
        assert!(is_builtin_module(&["std".into(), "bytes".into()]));
        assert!(is_builtin_module(&["std".into(), "encoding".into()]));
        assert!(is_builtin_module(&["std".into(), "channel".into()]));
        assert!(is_builtin_module(&["std".into(), "alloc".into()]));
        assert!(is_builtin_module(&["std".into(), "net".into(), "tls".into()]));
        // gg.* battery modules
        assert!(is_builtin_module(&["gg".into(), "sdl".into()]));
        assert!(is_builtin_module(&["gg".into(), "ecs".into()]));
        assert!(is_builtin_module(&["gg".into(), "json".into()]));
        assert!(is_builtin_module(&["gg".into(), "yaml".into()]));
        assert!(is_builtin_module(&["gg".into(), "http".into()]));
        assert!(is_builtin_module(&["gg".into(), "regex".into()]));
        assert!(is_builtin_module(&["gg".into(), "crypto".into()]));
        assert!(is_builtin_module(&["gg".into(), "toml".into()]));
        assert!(is_builtin_module(&["gg".into(), "xml".into()]));
        assert!(is_builtin_module(&["gg".into(), "csv".into()]));
        assert!(is_builtin_module(&["gg".into(), "gfx".into()]));
        assert!(is_builtin_module(&["gg".into(), "ssh".into()]));
        assert!(is_builtin_module(&["gg".into(), "p2p".into()]));
        // old std.* battery paths are NOT valid anymore
        assert!(!is_builtin_module(&["std".into(), "sdl".into()]));
        assert!(!is_builtin_module(&["std".into(), "json".into()]));
        assert!(!is_builtin_module(&["std".into(), "crypto".into()]));
        assert!(!is_builtin_module(&["gg".into(), "http".into(), "client".into()]));
        assert!(!is_builtin_module(&["std".into(), "test".into(), "process".into()]));
        assert!(!is_builtin_module(&["std".into(), "foo".into()]));
        assert!(!is_builtin_module(&["foo".into(), "fs".into()]));
        assert!(!is_builtin_module(&["std".into()]));
    }

    #[test]
    fn generate_fs() {
        let m = generate_builtin_module(&["std".into(), "fs".into()]).unwrap();
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
        let m = generate_builtin_module(&["std".into(), "conv".into()]).unwrap();
        assert_eq!(m.items.len(), 9);
    }

    #[test]
    fn generate_io() {
        let m = generate_builtin_module(&["std".into(), "io".into()]).unwrap();
        assert_eq!(m.items.len(), 8); // stderr, stdout, getchar, term_cols, term_rows, input, readline, stdin_eof
    }

    #[test]
    fn generate_random() {
        let m = generate_builtin_module(&["std".into(), "random".into()]).unwrap();
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
        let m = generate_builtin_module(&["std".into(), "time".into()]).unwrap();
        assert_eq!(m.items.len(), 6);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"time".to_string()));
        assert!(names.contains(&"sleep_ms".to_string()));
        assert!(names.contains(&"sleep".to_string()));
        assert!(names.contains(&"time_ms".to_string()));
        assert!(names.contains(&"format_time".to_string()));
        assert!(names.contains(&"parse_time".to_string()));
    }

    #[test]
    fn generate_math() {
        let m = generate_builtin_module(&["std".into(), "math".into()]).unwrap();
        assert_eq!(m.items.len(), 23); // 5 constants + 18 functions
        let mut const_names = vec![];
        let mut fn_names = vec![];
        for item in &m.items {
            match &item.node {
                Item::ConstDecl(c) => const_names.push(c.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => panic!("unexpected item type"),
            }
        }
        assert!(const_names.contains(&"PI".to_string()));
        assert!(const_names.contains(&"E".to_string()));
        assert!(const_names.contains(&"TAU".to_string()));
        assert!(const_names.contains(&"INFINITY".to_string()));
        assert!(const_names.contains(&"NAN".to_string()));
        assert!(fn_names.contains(&"abs".to_string()));
        assert!(fn_names.contains(&"sqrt".to_string()));
        assert!(fn_names.contains(&"sin".to_string()));
        assert!(fn_names.contains(&"atan2".to_string()));
        assert!(fn_names.contains(&"pow".to_string()));
    }

    #[test]
    fn generate_collections() {
        let m = generate_builtin_module(&["std".into(), "collections".into()]).unwrap();
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
        assert!(generate_builtin_module(&["std".into(), "foo".into()]).is_none());
    }

    #[test]
    fn is_builtin_gfx() {
        assert!(is_builtin_module(&["gg".into(), "gfx".into()]));
    }

    #[test]
    fn generate_gfx_returns_none() {
        // gg.gfx is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "gfx".into()]).is_none());
    }

    #[test]
    fn gfx_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "gfx".into()]);
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
        let source = builtin_module_source(&["gg".into(), "gfx".into()]).unwrap();
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
    fn is_builtin_ecs() {
        assert!(is_builtin_module(&["gg".into(), "ecs".into()]));
    }

    #[test]
    fn generate_ecs_returns_none() {
        // gg.ecs is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "ecs".into()]).is_none());
    }

    #[test]
    fn ecs_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "ecs".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct EntityPool"));
        assert!(src.contains("struct SparseSet"));
        assert!(src.contains("equip EntityPool"));
        assert!(src.contains("equip SparseSet"));
    }

    #[test]
    fn ecs_source_parses() {
        let source = builtin_module_source(&["gg".into(), "ecs".into()]).unwrap();
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

        assert!(struct_names.contains(&"Entity".to_string()));
        assert!(struct_names.contains(&"EntityPool".to_string()));
        assert!(struct_names.contains(&"SparseSet".to_string()));
        assert!(struct_names.contains(&"SparseSetIter".to_string()));
        assert_eq!(equip_count, 5);
    }

    #[test]
    fn is_builtin_sdl() {
        assert!(is_builtin_module(&["gg".into(), "sdl".into()]));
    }

    #[test]
    fn generate_sdl() {
        let m = generate_builtin_module(&["gg".into(), "sdl".into()]).unwrap();

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
    fn is_builtin_ssh() {
        assert!(is_builtin_module(&["gg".into(), "ssh".into()]));
    }

    #[test]
    fn generate_ssh_returns_none() {
        // gg.ssh is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "ssh".into()]).is_none());
    }

    #[test]
    fn ssh_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "ssh".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Session"));
        assert!(src.contains("struct CommandResult"));
        assert!(src.contains("ssh_connect"));
        assert!(src.contains("channel_exec"));
    }

    #[test]
    fn ssh_source_parses() {
        let source = builtin_module_source(&["gg".into(), "ssh".into()]).unwrap();
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
    fn is_builtin_crypto() {
        assert!(is_builtin_module(&["gg".into(), "crypto".into()]));
    }

    #[test]
    fn generate_crypto() {
        let m = generate_builtin_module(&["gg".into(), "crypto".into()]).unwrap();
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
    fn is_builtin_net_socket() {
        assert!(is_builtin_module(&["std".into(), "net".into(), "socket".into()]));
    }

    #[test]
    fn generate_socket() {
        let m = generate_builtin_module(&["std".into(), "net".into(), "socket".into()]).unwrap();
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
    fn is_builtin_bytes() {
        assert!(is_builtin_module(&["std".into(), "bytes".into()]));
    }

    #[test]
    fn generate_bytes_returns_none() {
        // std.bytes is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["std".into(), "bytes".into()]).is_none());
    }

    #[test]
    fn bytes_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "bytes".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("bytes_from_str"));
        assert!(src.contains("bytes_to_str"));
        assert!(src.contains("bytes_from_hex"));
        assert!(src.contains("bytes_to_hex"));
        assert!(src.contains("base64_encode"));
        assert!(src.contains("base64_decode"));
    }

    #[test]
    fn bytes_source_parses() {
        let source = builtin_module_source(&["std".into(), "bytes".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "bytes.gg parse errors: {:?}", parser.errors);

        let mut fn_names = vec![];
        for item in &module.items {
            match &item.node {
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => {}
            }
        }

        assert!(fn_names.contains(&"bytes_from_str".to_string()));
        assert!(fn_names.contains(&"bytes_to_str".to_string()));
        assert!(fn_names.contains(&"base64_encode".to_string()));
        assert!(fn_names.contains(&"base64_decode".to_string()));
        assert!(fn_names.contains(&"b64_char_value".to_string()));
    }

    #[test]
    fn is_builtin_encoding() {
        assert!(is_builtin_module(&["std".into(), "encoding".into()]));
    }

    #[test]
    fn generate_encoding_returns_none() {
        // std.encoding is file-based (std.*), not synthetic — generate returns None
        assert!(generate_builtin_module(&["std".into(), "encoding".into()]).is_none());
    }

    #[test]
    fn encoding_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "encoding".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("url_encode"));
        assert!(src.contains("url_decode"));
        assert!(src.contains("html_escape"));
        assert!(src.contains("html_unescape"));
        assert!(src.contains("utf8_len"));
        assert!(src.contains("utf8_codepoints"));
        assert!(src.contains("latin1_encode"));
        assert!(src.contains("latin1_decode"));
    }

    #[test]
    fn encoding_source_parses() {
        let source = builtin_module_source(&["std".into(), "encoding".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "encoding.gg parse errors: {:?}", parser.errors);

        let mut fn_names = vec![];
        for item in &module.items {
            match &item.node {
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                _ => {}
            }
        }

        assert!(fn_names.contains(&"url_encode".to_string()));
        assert!(fn_names.contains(&"url_decode".to_string()));
        assert!(fn_names.contains(&"form_encode".to_string()));
        assert!(fn_names.contains(&"form_decode".to_string()));
        assert!(fn_names.contains(&"html_escape".to_string()));
        assert!(fn_names.contains(&"html_unescape".to_string()));
        assert!(fn_names.contains(&"utf8_len".to_string()));
        assert!(fn_names.contains(&"utf8_codepoints".to_string()));
        assert!(fn_names.contains(&"utf8_is_valid".to_string()));
        assert!(fn_names.contains(&"utf8_char_at".to_string()));
        assert!(fn_names.contains(&"latin1_encode".to_string()));
        assert!(fn_names.contains(&"latin1_decode".to_string()));
    }

    #[test]
    fn is_builtin_json() {
        assert!(is_builtin_module(&["gg".into(), "json".into()]));
    }

    #[test]
    fn generate_json_returns_none() {
        // gg.json is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "json".into()]).is_none());
    }

    #[test]
    fn json_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "json".into()]);
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
        let source = builtin_module_source(&["gg".into(), "json".into()]).unwrap();
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
        assert!(struct_names.contains(&"JsonSerializer".to_string()));
        assert!(struct_names.contains(&"JsonDeserializer".to_string()));
        assert!(fn_names.contains(&"json_parse".to_string()));
        assert!(fn_names.contains(&"json_stringify".to_string()));
        assert!(fn_names.contains(&"json_pretty".to_string()));
        // equip JsonParser + equip Json + equip JsonSerializer with Serializer + equip JsonDeserializer with Deserializer
        assert_eq!(equip_count, 4);
    }

    #[test]
    fn is_builtin_toml() {
        assert!(is_builtin_module(&["gg".into(), "toml".into()]));
    }

    #[test]
    fn generate_toml_returns_none() {
        assert!(generate_builtin_module(&["gg".into(), "toml".into()]).is_none());
    }

    #[test]
    fn toml_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "toml".into()]);
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
        let source = builtin_module_source(&["gg".into(), "toml".into()]).unwrap();
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
    fn is_builtin_xml() {
        assert!(is_builtin_module(&["gg".into(), "xml".into()]));
    }

    #[test]
    fn generate_xml_returns_none() {
        // gg.xml is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "xml".into()]).is_none());
    }

    #[test]
    fn xml_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "xml".into()]);
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
        let source = builtin_module_source(&["gg".into(), "xml".into()]).unwrap();
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
    fn is_builtin_yaml() {
        assert!(is_builtin_module(&["gg".into(), "yaml".into()]));
    }

    #[test]
    fn generate_yaml_returns_none() {
        // gg.yaml is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "yaml".into()]).is_none());
    }

    #[test]
    fn yaml_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "yaml".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("enum Yaml"));
        assert!(src.contains("yaml_parse"));
        assert!(src.contains("yaml_stringify"));
        assert!(src.contains("yaml_pretty"));
        assert!(src.contains("equip Yaml"));
        assert!(src.contains("equip YamlParser"));
    }

    #[test]
    fn yaml_source_parses() {
        let source = builtin_module_source(&["gg".into(), "yaml".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "yaml.gg parse errors: {:?}", parser.errors);

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

        assert!(enum_names.contains(&"Yaml".to_string()));
        assert!(struct_names.contains(&"YamlParser".to_string()));
        assert!(fn_names.contains(&"yaml_parse".to_string()));
        assert!(fn_names.contains(&"yaml_stringify".to_string()));
        assert!(fn_names.contains(&"yaml_pretty".to_string()));
        assert_eq!(equip_count, 2); // equip YamlParser + equip Yaml
    }

    #[test]
    fn generate_tls_socket() {
        let m = generate_builtin_module(&["std".into(), "net".into(), "tls".into()]).unwrap();
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        for item in &m.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }
        assert!(struct_names.contains(&"TlsSocket".to_string()));
        assert!(fn_names.contains(&"tls_connect".to_string()));
        assert_eq!(equip_count, 1);
    }

    #[test]
    fn tls_socket_methods_are_extern() {
        let m = generate_builtin_module(&["std".into(), "net".into(), "tls".into()]).unwrap();
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "TlsSocket" {
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
                                "close"
                            ]
                        );
                        for method in &eq.items {
                            assert!(
                                matches!(method.node.body, FunctionBody::Extern(_)),
                                "TlsSocket.{} should be FunctionBody::Extern",
                                method.node.name.node
                            );
                        }
                        return;
                    }
                }
            }
        }
        panic!("TlsSocket equip block not found");
    }

    #[test]
    fn http_is_file_based_module() {
        // gg.http is file-based, so generate_builtin_module returns None
        assert!(generate_builtin_module(&["gg".into(), "http".into()]).is_none());
        // but builtin_module_source returns the source
        assert!(builtin_module_source(&["gg".into(), "http".into()]).is_some());
    }

    #[test]
    fn socket_methods_are_extern() {
        let m = generate_builtin_module(&["std".into(), "net".into(), "socket".into()]).unwrap();
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
        let m = generate_builtin_module(&["gg".into(), "crypto".into()]).unwrap();
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
        let m = generate_builtin_module(&["std".into(), "collections".into()]).unwrap();
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
        let m = generate_builtin_module(&["gg".into(), "crypto".into()]).unwrap();
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

    // ─── gg.csv ───────────────────────────────────────────────────

    #[test]
    fn is_builtin_csv() {
        assert!(is_builtin_module(&["gg".into(), "csv".into()]));
    }

    #[test]
    fn generate_csv_returns_none() {
        // gg.csv is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["gg".into(), "csv".into()]).is_none());
    }

    #[test]
    fn csv_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "csv".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct CsvParser"));
        assert!(src.contains("struct CsvTable"));
        assert!(src.contains("csv_parse"));
        assert!(src.contains("csv_stringify"));
        assert!(src.contains("equip CsvParser"));
        assert!(src.contains("equip CsvTable"));
    }

    #[test]
    fn csv_source_parses() {
        let source = builtin_module_source(&["gg".into(), "csv".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "csv.gg parse errors: {:?}", parser.errors);

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

        assert!(struct_names.contains(&"CsvParser".to_string()));
        assert!(struct_names.contains(&"CsvTable".to_string()));
        assert!(fn_names.contains(&"csv_parse".to_string()));
        assert!(fn_names.contains(&"csv_stringify".to_string()));
        assert!(fn_names.contains(&"csv_parse_table".to_string()));
        assert!(fn_names.contains(&"csv_stringify_table".to_string()));
        assert!(equip_count >= 2, "expected at least 2 equip blocks, got {equip_count}");
    }

    // ─── gg.tensor ────────────────────────────────────────────

    #[test]
    fn is_builtin_tensor() {
        assert!(is_builtin_module(&["gg".into(), "tensor".into()]));
    }

    #[test]
    fn generate_tensor_returns_none() {
        assert!(generate_builtin_module(&["gg".into(), "tensor".into()]).is_none());
    }

    #[test]
    fn tensor_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "tensor".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Tensor"));
        assert!(src.contains("tensor_arange"));
        assert!(src.contains("tensor_zeros_int"));
    }

    #[test]
    fn tensor_source_parses() {
        let source = builtin_module_source(&["gg".into(), "tensor".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let _module = parser.parse_module();
        assert!(parser.errors.is_empty(), "tensor.gg parse errors: {:?}", parser.errors);
    }

    // ─── gg.dataframe ─────────────────────────────────────────

    #[test]
    fn is_builtin_dataframe() {
        assert!(is_builtin_module(&["gg".into(), "dataframe".into()]));
    }

    #[test]
    fn generate_dataframe_returns_none() {
        assert!(generate_builtin_module(&["gg".into(), "dataframe".into()]).is_none());
    }

    #[test]
    fn dataframe_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "dataframe".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct DataFrame"));
        assert!(src.contains("df_from_columns"));
        assert!(src.contains("df_from_csv"));
    }

    #[test]
    fn dataframe_source_parses() {
        let source = builtin_module_source(&["gg".into(), "dataframe".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let _module = parser.parse_module();
        assert!(parser.errors.is_empty(), "dataframe.gg parse errors: {:?}", parser.errors);
    }

    // ─── gg.uuid ──────────────────────────────────────────────

    #[test]
    fn is_builtin_uuid() {
        assert!(is_builtin_module(&["gg".into(), "uuid".into()]));
    }

    #[test]
    fn generate_uuid_returns_none() {
        assert!(generate_builtin_module(&["gg".into(), "uuid".into()]).is_none());
    }

    #[test]
    fn uuid_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "uuid".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct UUID"));
        assert!(src.contains("v4()"));
        assert!(src.contains("to_string"));
    }

    #[test]
    fn uuid_source_parses() {
        let source = builtin_module_source(&["gg".into(), "uuid".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "uuid.gg parse errors: {:?}", parser.errors);
        let struct_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Struct(s) => Some(s.name.node.clone()),
            _ => None,
        }).collect();
        assert!(struct_names.contains(&"UUID".to_string()));
        let fn_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Function(f) => Some(f.name.node.clone()),
            _ => None,
        }).collect();
        assert!(fn_names.contains(&"uuid_hex_val".to_string()));
    }

    // ─── gg.log ───────────────────────────────────────────────

    #[test]
    fn is_builtin_log() {
        assert!(is_builtin_module(&["gg".into(), "log".into()]));
    }

    #[test]
    fn generate_log_returns_none() {
        assert!(generate_builtin_module(&["gg".into(), "log".into()]).is_none());
    }

    #[test]
    fn log_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "log".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Logger"));
        assert!(src.contains("enum LogLevel"));
        assert!(src.contains("log_info"));
    }

    #[test]
    fn log_source_parses() {
        let source = builtin_module_source(&["gg".into(), "log".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "log.gg parse errors: {:?}", parser.errors);
        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut enum_names = vec![];
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::Enum(e) => enum_names.push(e.name.node.clone()),
                _ => {}
            }
        }
        assert!(struct_names.contains(&"Logger".to_string()));
        assert!(enum_names.contains(&"LogLevel".to_string()));
        assert!(fn_names.contains(&"log_info".to_string()));
        assert!(fn_names.contains(&"log_debug".to_string()));
        assert!(fn_names.contains(&"log_warn".to_string()));
        assert!(fn_names.contains(&"log_error".to_string()));
    }

    // ─── std.term ─────────────────────────────────────────────

    #[test]
    fn is_builtin_term() {
        assert!(is_builtin_module(&["std".into(), "term".into()]));
    }

    #[test]
    fn generate_term_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "term".into()]).is_none());
    }

    #[test]
    fn term_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "term".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("is_tty"));
        assert!(src.contains("gorget_is_tty"));
        assert!(src.contains("red"));
        assert!(src.contains("green"));
        assert!(src.contains("strip_ansi"));
    }

    #[test]
    fn term_source_parses() {
        let source = builtin_module_source(&["std".into(), "term".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "term.gg parse errors: {:?}", parser.errors);
        let fn_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Function(f) => Some(f.name.node.clone()),
            _ => None,
        }).collect();
        assert!(fn_names.contains(&"is_tty".to_string()));
        assert!(fn_names.contains(&"red".to_string()));
        assert!(fn_names.contains(&"green".to_string()));
        assert!(fn_names.contains(&"bold".to_string()));
        assert!(fn_names.contains(&"strip_ansi".to_string()));
    }

    // ─── gg.cli ───────────────────────────────────────────────

    #[test]
    fn is_builtin_cli() {
        assert!(is_builtin_module(&["gg".into(), "cli".into()]));
    }

    #[test]
    fn generate_cli_returns_none() {
        assert!(generate_builtin_module(&["gg".into(), "cli".into()]).is_none());
    }

    #[test]
    fn cli_module_source_exists() {
        let source = builtin_module_source(&["gg".into(), "cli".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct CliParser"));
        assert!(src.contains("struct CliArg"));
        assert!(src.contains("add_flag"));
        assert!(src.contains("add_option"));
        assert!(src.contains("print_help"));
    }

    #[test]
    fn cli_source_parses() {
        let source = builtin_module_source(&["gg".into(), "cli".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "cli.gg parse errors: {:?}", parser.errors);
        let struct_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Struct(s) => Some(s.name.node.clone()),
            _ => None,
        }).collect();
        assert!(struct_names.contains(&"CliParser".to_string()));
        assert!(struct_names.contains(&"CliArg".to_string()));
    }

    // ─── std.heap ─────────────────────────────────────────────

    #[test]
    fn is_builtin_heap() {
        assert!(is_builtin_module(&["std".into(), "heap".into()]));
    }

    #[test]
    fn generate_heap_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "heap".into()]).is_none());
    }

    #[test]
    fn heap_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "heap".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Heap"));
        assert!(src.contains("push"));
        assert!(src.contains("pop"));
        assert!(src.contains("peek"));
    }

    #[test]
    fn heap_source_parses() {
        let source = builtin_module_source(&["std".into(), "heap".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "heap.gg parse errors: {:?}", parser.errors);
        let struct_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Struct(s) => Some(s.name.node.clone()),
            _ => None,
        }).collect();
        assert!(struct_names.contains(&"Heap".to_string()));
    }

    // ─── std.datetime ─────────────────────────────────────────

    #[test]
    fn is_builtin_datetime() {
        assert!(is_builtin_module(&["std".into(), "datetime".into()]));
    }

    #[test]
    fn generate_datetime_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "datetime".into()]).is_none());
    }

    #[test]
    fn datetime_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "datetime".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct DateTime"));
        assert!(src.contains("to_epoch"));
        assert!(src.contains("weekday"));
        assert!(src.contains("gorget_dt_decompose"));
    }

    #[test]
    fn datetime_source_parses() {
        let source = builtin_module_source(&["std".into(), "datetime".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "datetime.gg parse errors: {:?}", parser.errors);
        let struct_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Struct(s) => Some(s.name.node.clone()),
            _ => None,
        }).collect();
        assert!(struct_names.contains(&"DateTime".to_string()));
        let fn_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Function(f) => Some(f.name.node.clone()),
            _ => None,
        }).collect();
        assert!(fn_names.contains(&"dt_is_leap".to_string()));
        assert!(fn_names.contains(&"dt_days_from_epoch".to_string()));
    }

}
