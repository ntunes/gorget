/// Built-in module system — covers both `std` (core) and `gg` (batteries).
///
/// The namespace split:
/// - `std.*` — lean, stable building blocks: collections, I/O, math, OS, net, …
/// - `xtd.*`  — batteries-included ecosystem: JSON, TOML, XML, YAML, CSV,
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
/// Examples: `std.fs`, `std.os`, `std.conv`, `std.math`, `xtd.crypto`,
/// `std.net.socket`, `xtd.sdl`, `xtd.regex`.
///
/// ## File-based modules (`builtin_module_source` returns `Some`)
///
/// The module is written in Gorget as a `.gg` file under `lib/std/` (for `std.*`)
/// or `lib/xtd/` (for `xtd.*`). The loader reads the source via `include_str!`,
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
/// Examples: `xtd.json`, `xtd.toml`, `xtd.xml`, `xtd.yaml`, `xtd.csv`,
/// `std.bytes`, `std.encoding`, `xtd.gfx`, `xtd.ecs`, `xtd.ssh`, `xtd.http`.
///
/// ## Adding a new module
///
/// 1. Choose synthetic if it's pure C-runtime glue, file-based if it has logic.
/// 2. Add the module name to `is_builtin_module`.
/// 3. For synthetic: add a `gen_*_module()` function returning the AST.
///    For file-based in `std`: create `lib/std/<name>.gg`, add `None` to
///    `generate_builtin_module`, add `include_str!` to `builtin_module_source`.
///    For file-based in `gg`: create `lib/xtd/<name>.gg` instead.
/// 4. Add unit tests (at minimum: is_builtin, generate returns correct variant,
///    source exists/parses for file-based modules).
///
/// All synthetic defs use `Span::dummy()`, which distinguishes them from
/// user-defined code and enables the `is_stdlib_call()` guard in codegen.
use crate::parser::ast::*;
use crate::span::{Span, Spanned};

/// Check if an import path refers to a built-in module (`std.*` or `xtd.*`).
pub fn is_builtin_module(segments: &[String]) -> bool {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.len() {
            2 => matches!(segments[1].as_str(),
                "fs" | "path" | "os" | "conv" | "io" | "random" | "time"
                | "collections" | "math" | "fmt" | "process" | "bytes"
                | "encoding" | "channel" | "alloc" | "term" | "heap" | "datetime"
                | "sync" | "thread" | "async" | "signal"),
            3 => segments[1] == "net" && matches!(segments[2].as_str(), "socket" | "tls" | "udp"),
            _ => false,
        },
        Some("xtd") => segments.len() == 2 && matches!(segments[1].as_str(),
            "json" | "toml" | "xml" | "yaml" | "csv" | "crypto" | "regex"
            | "sdl" | "gfx" | "ecs" | "ssh" | "http" | "httpserver" | "p2p"
            | "uuid" | "log" | "cli" | "tensor" | "dataframe"
            | "db" | "sqlite" | "influx" | "jsonpath"
            | "math3d" | "gl" | "image" | "audio" | "compress"
            | "metal" | "gpu"),
        _ => false,
    }
}

/// Generate a synthetic `Module` for a built-in module.
pub fn generate_builtin_module(segments: &[String]) -> Option<Module> {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.len() {
            2 => match segments[1].as_str() {
                "fs" => None, // file-based — loaded via builtin_module_source()
                "path" => None, // file-based
                "os" => Some(gen_os_module()),
                "conv" => Some(gen_conv_module()),
                "io" => Some(gen_io_module()),
                "random" => None, // file-based
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
                "signal" => None, // file-based
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
        Some("xtd") if segments.len() == 2 => match segments[1].as_str() {
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
            "http" => None,       // file-based module — loaded via builtin_module_source()
            "httpserver" => None, // file-based module — loaded via builtin_module_source()
            "p2p" => None,        // file-based module — loaded via builtin_module_source()
            "uuid" => None, // file-based module — loaded via builtin_module_source()
            "log" => None,  // file-based module — loaded via builtin_module_source()
            "cli" => None,  // file-based module — loaded via builtin_module_source()
            "tensor" => None,    // file-based module — loaded via builtin_module_source()
            "dataframe" => None, // file-based module — loaded via builtin_module_source()
            "db" => None,        // file-based module — loaded via builtin_module_source()
            "sqlite" => None,    // file-based module — loaded via builtin_module_source()
            "influx" => None,    // file-based module — loaded via builtin_module_source()
            "jsonpath" => None,  // file-based module — loaded via builtin_module_source()
            "math3d" => None,    // file-based module — loaded via builtin_module_source()
            "gl" => Some(gen_gl_module()),
            "image" => Some(gen_image_module()),
            "audio" => Some(gen_audio_module()),
            "compress" => Some(gen_compress_module()),
            "metal" => Some(gen_metal_module()),
            "gpu" => None, // file-based module — loaded via builtin_module_source()
            _ => None,
        },
        _ => None,
    }
}

// ─── Module Generators ──────────────────────────────────────



fn gen_os_module() -> Module {
    use crate::ir::abi::AbiKind::CStr;
    make_module(vec![
        decl_fn("exit", &[("code", ty_int())], ty_void()),
        decl_fn_abi("getenv", &[("name", ty_str(), CStr)], ty_str()),
        decl_fn_abi("setenv", &[("name", ty_str(), CStr), ("value", ty_str(), CStr)], ty_void()),
        decl_fn("getcwd", &[], ty_string()),
        decl_fn("platform", &[], ty_str()),
        decl_fn("args", &[], ty_vector_str()),
        decl_fn_abi("readdir", &[("path", ty_str(), CStr)], ty_vector_str()),
        decl_fn("mem_allocated", &[], ty_int()),
        decl_fn("mem_freed", &[], ty_int()),
        decl_fn("mem_live", &[], ty_int()),
        decl_fn("mem_alloc_count", &[], ty_int()),
    ])
}

fn gen_conv_module() -> Module {
    use crate::ir::abi::AbiKind::CStr;
    make_module(vec![
        decl_fn_abi("ord", &[("s", ty_str(), CStr)], ty_int()),
        decl_fn("chr", &[("n", ty_int())], ty_str()),
        decl_fn_abi("parse_int", &[("s", ty_str(), CStr)], ty_result(ty_int(), ty_str())),
        decl_fn_abi("parse_float", &[("s", ty_str(), CStr)], ty_result(ty_float(), ty_str())),
        decl_fn("int_to_str", &[("n", ty_int())], ty_str()),
        decl_fn("float_to_str", &[("x", ty_float())], ty_str()),
        decl_fn("bool_to_str", &[("b", ty_bool())], ty_str()),
        decl_fn("codepoint_to_str", &[("cp", ty_int())], ty_str()),
        decl_fn("int_to_float", &[("n", ty_int())], ty_float()),
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
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::CStr;
        decl_fn_abi("input", &[("prompt", ty_str(), CStr)], ty_string())
    })));
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

fn gen_time_module() -> Module {
    use crate::ir::abi::AbiKind::{CStr, Scalar};
    make_module(vec![
        decl_fn("time", &[], ty_int()),
        decl_fn("time_ms", &[], ty_int()),
        decl_fn("sleep_ms", &[("ms", ty_int())], ty_void()),
        decl_async_fn("sleep", &[("seconds", ty_float())], ty_void()),
        decl_fn_abi("format_time", &[("epoch", ty_int(), Scalar), ("fmt", ty_str(), CStr)], ty_string()),
        decl_fn_abi("parse_time", &[("s", ty_str(), CStr), ("fmt", ty_str(), CStr)], ty_int()),
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
        decl_method("wait_timeout", Ownership::Borrow, &[("timeout_ms", ty_int())], ty_int()),
        decl_method("kill", Ownership::Borrow, &[], ty_void()),
        decl_method("pid", Ownership::Borrow, &[], ty_int()),
        decl_method("write_stdin", Ownership::Borrow, &[("data", ty_str())], ty_void()),
        decl_method("close_stdin", Ownership::Borrow, &[], ty_void()),
        decl_method("read_stdout", Ownership::Borrow, &[], ty_string()),
        decl_method("read_stderr", Ownership::Borrow, &[], ty_string()),
        decl_method("read_all", Ownership::Borrow, &[], exec_result_type.clone()),
        decl_method("read_all_timeout", Ownership::Borrow, &[("timeout_ms", ty_int())], exec_result_type.clone()),
    ]);
    let items = vec![
        Spanned::dummy(Item::Struct(struct_def)),
        process_struct,
        process_equip,
        Spanned::dummy(Item::Function({
            use crate::ir::abi::AbiKind::CStr;
            decl_fn_abi("exec", &[("cmd", ty_str(), CStr)], ty_int())
        })),
        Spanned::dummy(Item::Function({
            use crate::ir::abi::AbiKind::CStr;
            decl_fn_abi("exec_output", &[("cmd", ty_str(), CStr)], exec_result_type)
        })),
        Spanned::dummy(Item::Function({
            use crate::ir::abi::AbiKind::{CStr, Ptr};
            decl_fn_abi(
                "process_spawn",
                &[("program", ty_str(), CStr), ("args", ty_vector_str(), Ptr)],
                ty_result(ty_process(), ty_str()),
            )
        })),
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

    // WaitGroup — non-generic opaque struct
    let waitgroup_struct = opaque_struct("WaitGroup");
    let waitgroup_equip = equip_block("WaitGroup", vec![
        decl_method("add", Ownership::Borrow, &[("n", ty_int())], ty_void()),
        decl_method("done", Ownership::Borrow, &[], ty_void()),
        decl_method("wait", Ownership::Borrow, &[], ty_void()),
    ]);

    // Semaphore — non-generic opaque struct
    let semaphore_struct = opaque_struct("Semaphore");
    let semaphore_equip = equip_block("Semaphore", vec![
        decl_method("acquire", Ownership::Borrow, &[], ty_void()),
        decl_method("release", Ownership::Borrow, &[], ty_void()),
        decl_method("try_acquire", Ownership::Borrow, &[], ty_bool()),
    ]);

    // OnceFlag — exactly-once initialization primitive
    let onceflag_struct = opaque_struct("OnceFlag");
    let onceflag_equip = equip_block("OnceFlag", vec![
        decl_method("do_once", Ownership::Borrow, &[], ty_bool()),
        decl_method("is_done", Ownership::Borrow, &[], ty_bool()),
    ]);

    Module {
        items: vec![
            atomic_int_struct, atomic_int_equip,
            atomic_bool_struct, atomic_bool_equip,
            barrier_struct, barrier_equip,
            condvar_struct, condvar_equip,
            rwlock_struct, rwlock_equip,
            read_guard_struct, read_guard_equip,
            write_guard_struct, write_guard_equip,
            waitgroup_struct, waitgroup_equip,
            semaphore_struct, semaphore_equip,
            onceflag_struct, onceflag_equip,
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
        ("Vector", 1),   // [T]
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
    items.push(const_item("SDL_WINDOW_OPENGL", 2));
    items.push(const_item("SDL_WINDOW_BORDERLESS", 16));
    items.push(const_item("SDL_WINDOW_INPUT_GRABBED", 256));

    // Renderer flags
    items.push(const_item("SDL_RENDERER_ACCELERATED", 2));
    items.push(const_item("SDL_RENDERER_PRESENTVSYNC", 4));

    // ── Function declarations ────────────────────────────────
    // Lifecycle
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_init", &[("flags", ty_int())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_quit", &[], ty_void()))));

    // Window
    {
        use crate::ir::abi::AbiKind::*;
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_create_window", &[("title", ty_str(), CStr), ("w", ty_int(), Scalar), ("h", ty_int(), Scalar), ("flags", ty_int(), Scalar)], ty_sdl_window()))));
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_create_window_try", &[("title", ty_str(), CStr), ("w", ty_int(), Scalar), ("h", ty_int(), Scalar), ("flags", ty_int(), Scalar)], ty_sdl_window()))));
    }
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_window_is_null", &[("win", ty_sdl_window())], ty_bool()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_window_to_handle", &[("win", ty_sdl_window())], ty_int()))));
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
    {
        use crate::ir::abi::AbiKind::*;
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_load_texture", &[("r", ty_sdl_renderer(), Opaque), ("path", ty_str(), CStr)], ty_sdl_texture()))));
    }
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_destroy_texture", &[("t", ty_sdl_texture())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_render_texture", &[("r", ty_sdl_renderer()), ("t", ty_sdl_texture()), ("x", ty_int()), ("y", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_render_texture_sized", &[("r", ty_sdl_renderer()), ("t", ty_sdl_texture()), ("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_texture_width", &[("t", ty_sdl_texture())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_texture_height", &[("t", ty_sdl_texture())], ty_int()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_set_texture_alpha", &[("t", ty_sdl_texture()), ("alpha", ty_int())], ty_void()))));

    // Text (SDL2_ttf)
    {
        use crate::ir::abi::AbiKind::*;
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_load_font", &[("path", ty_str(), CStr), ("size", ty_int(), Scalar)], ty_sdl_font()))));
        items.push(Spanned::dummy(Item::Function(decl_fn("sdl_close_font", &[("f", ty_sdl_font())], ty_void()))));
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_render_text", &[("r", ty_sdl_renderer(), Opaque), ("f", ty_sdl_font(), Opaque), ("text", ty_str(), CStr), ("red", ty_int(), Scalar), ("green", ty_int(), Scalar), ("blue", ty_int(), Scalar)], ty_sdl_texture()))));
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_draw_text", &[("r", ty_sdl_renderer(), Opaque), ("f", ty_sdl_font(), Opaque), ("text", ty_str(), CStr), ("x", ty_int(), Scalar), ("y", ty_int(), Scalar), ("red", ty_int(), Scalar), ("green", ty_int(), Scalar), ("blue", ty_int(), Scalar)], ty_void()))));
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_text_width", &[("f", ty_sdl_font(), Opaque), ("text", ty_str(), CStr)], ty_int()))));
        items.push(Spanned::dummy(Item::Function(decl_fn_abi("sdl_text_height", &[("f", ty_sdl_font(), Opaque), ("text", ty_str(), CStr)], ty_int()))));
    }

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

    // Mouse capture and relative mode (for FPS controls)
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_set_relative_mouse_mode", &[("enabled", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_show_cursor", &[("toggle", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_relative_mouse_state", &[], ty_sdl_event()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_warp_mouse_in_window", &[("window", ty_sdl_window()), ("x", ty_int()), ("y", ty_int())], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_get_mouse_state", &[], ty_sdl_event()))));

    // Text input mode (for console / chat)
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_start_text_input", &[], ty_void()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("sdl_stop_text_input", &[], ty_void()))));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── File-based built-in modules ────────────────────────────

/// Get embedded source for file-based built-in modules (`std.*` or `xtd.*`).
/// These are real `.gg` files compiled into the binary, parsed and loaded
/// by the module loader (including recursive import resolution).
pub fn builtin_module_source(segments: &[String]) -> Option<&'static str> {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.get(1).map(|s| s.as_str()) {
            Some("fs") => Some(include_str!("../lib/std/fs.gg")),
            Some("path") => Some(include_str!("../lib/std/path.gg")),
            Some("random") => Some(include_str!("../lib/std/random.gg")),
            Some("signal") => Some(include_str!("../lib/std/signal.gg")),
            Some("fmt") => Some(include_str!("../lib/std/fmt.gg")),
            Some("bytes") => Some(include_str!("../lib/std/bytes.gg")),
            Some("encoding") => Some(include_str!("../lib/std/encoding.gg")),
            Some("term") => Some(include_str!("../lib/std/term.gg")),
            Some("heap") => Some(include_str!("../lib/std/heap.gg")),
            Some("datetime") => Some(include_str!("../lib/std/datetime.gg")),
            _ => None,
        },
        Some("xtd") => match segments.get(1).map(|s| s.as_str()) {
            Some("gfx") => Some(include_str!("../lib/xtd/gfx.gg")),
            Some("ecs") => Some(include_str!("../lib/xtd/ecs.gg")),
            Some("ssh") => Some(include_str!("../lib/xtd/ssh.gg")),
            Some("http") => Some(include_str!("../lib/xtd/http.gg")),
            Some("httpserver") => Some(include_str!("../lib/xtd/httpserver.gg")),
            Some("json") => Some(include_str!("../lib/xtd/json.gg")),
            Some("toml") => Some(include_str!("../lib/xtd/toml.gg")),
            Some("xml") => Some(include_str!("../lib/xtd/xml.gg")),
            Some("yaml") => Some(include_str!("../lib/xtd/yaml.gg")),
            Some("csv") => Some(include_str!("../lib/xtd/csv.gg")),
            Some("p2p") => Some(include_str!("../lib/xtd/p2p.gg")),
            Some("uuid") => Some(include_str!("../lib/xtd/uuid.gg")),
            Some("log") => Some(include_str!("../lib/xtd/log.gg")),
            Some("cli") => Some(include_str!("../lib/xtd/cli.gg")),
            Some("tensor") => Some(include_str!("../lib/xtd/tensor.gg")),
            Some("dataframe") => Some(include_str!("../lib/xtd/dataframe.gg")),
            Some("db") => Some(include_str!("../lib/xtd/db.gg")),
            Some("sqlite") => Some(include_str!("../lib/xtd/sqlite.gg")),
            Some("influx") => Some(include_str!("../lib/xtd/influx.gg")),
            Some("jsonpath") => Some(include_str!("../lib/xtd/jsonpath.gg")),
            Some("math3d") => Some(include_str!("../lib/xtd/math3d.gg")),
            Some("gpu") => Some(include_str!("../lib/xtd/gpu.gg")),
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
                    is_meta_op: false,
                })
            })
            .collect(),
        throws: None,
        where_clause: None,
        body: FunctionBody::Declaration,
        doc_comment: None,
        span: Span::dummy(),
        param_abis: vec![],
    }
}

/// Declare an extern function with explicit ABI annotations per parameter.
#[allow(dead_code)]
fn decl_fn_abi(name: &str, params: &[(&str, Type, crate::ir::abi::AbiKind)], ret: Type) -> FunctionDef {
    use crate::ir::abi::AbiKind;
    let param_abis: Vec<AbiKind> = params.iter().map(|(_, _, abi)| *abi).collect();
    let mut f = decl_fn(
        name,
        &params.iter().map(|(n, t, _)| (*n, t.clone())).collect::<Vec<_>>(),
        ret,
    );
    f.param_abis = param_abis;
    f
}

fn decl_async_fn(name: &str, params: &[(&str, Type)], ret: Type) -> FunctionDef {
    let mut f = decl_fn(name, params, ret);
    f.qualifiers.is_async = true;
    f
}

fn ty_str() -> Type {
    Type::Primitive(PrimitiveType::StringView)
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

// ─── xtd.crypto ──────────────────────────────────────────────

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
        {
            use crate::ir::abi::AbiKind::{CStr, Ptr};
            decl_fn_abi("crypto_hmac", &[("algo", ty_str(), CStr), ("key", ty_vector_uint8(), Ptr), ("data", ty_vector_uint8(), Ptr)], ty_result(ty_vector_uint8(), ty_str()))
        },
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
    let ty_server_socket = || Type::Named {
        name: Spanned::dummy("ServerSocket".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque struct: Socket
    items.push(opaque_struct("Socket"));

    // Free function: socket_connect(host, port) -> Result[Socket, str]
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::{CStr, Scalar};
        decl_fn_abi("socket_connect", &[("host", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_socket(), ty_str()))
    })));

    // NOTE: async_socket_connect deferred — connect is rarely the hot path in servers.
    // The important async ops are async_accept, async_read, async_write for server loops.

    // Socket methods — extern bindings
    items.push(equip_block("Socket", vec![
        extern_method("read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_read"),
        extern_method("read_exact", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_read_exact"),
        extern_method("write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_socket_write"),
        extern_method("write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_socket_write_str"),
        extern_method("read_line", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_socket_read_line"),
        extern_method("set_timeout", Ownership::MutableBorrow, &[("ms", ty_int())], ty_void(), "gorget_socket_set_timeout"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_socket_close"),
        // Non-blocking socket methods — for use in spawned/coroutine context
        extern_method("nb_read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_async_read"),
        extern_method("nb_write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_socket_async_write"),
        extern_method("nb_write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_socket_async_write_str"),
    ]));

    // Opaque struct: ServerSocket
    items.push(opaque_struct("ServerSocket"));

    // Free function: server_socket_bind(host, port) -> Result[ServerSocket, str]
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::{CStr, Scalar};
        decl_fn_abi("server_socket_bind", &[("host", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_server_socket(), ty_str()))
    })));

    // ServerSocket methods — extern bindings
    // accept() returns a Result[Socket, str]: the accepted client reuses all Socket methods.
    items.push(equip_block("ServerSocket", vec![
        extern_method("accept", Ownership::MutableBorrow, &[], ty_result(ty_socket(), ty_str()), "gorget_server_socket_accept"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_server_socket_close"),
        extern_method("local_port", Ownership::Borrow, &[], ty_int(), "gorget_server_socket_local_port"),
        // Non-blocking accept — for use in spawned/coroutine context
        extern_method("nb_accept", Ownership::MutableBorrow, &[], ty_result(ty_socket(), ty_str()), "gorget_socket_async_accept"),
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
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::{CStr, Scalar};
        decl_fn_abi("udp_bind", &[("addr", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_udp_socket(), ty_str()))
    })));

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
    let ty_tls_server_socket = || Type::Named {
        name: Spanned::dummy("TlsServerSocket".to_string()),
        generic_args: vec![],
    };

    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque structs
    items.push(opaque_struct("TlsSocket"));
    items.push(opaque_struct("TlsServerSocket"));

    // Free function: tls_connect(host, port) -> Result[TlsSocket, str]
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::{CStr, Scalar};
        decl_fn_abi("tls_connect", &[("host", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_tls_socket(), ty_str()))
    })));

    // Free function: tls_server_bind(host, port, cert_path, key_path) -> Result[TlsServerSocket, str]
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::{CStr, Scalar};
        decl_fn_abi("tls_server_bind", &[
            ("host", ty_str(), CStr),
            ("port", ty_int(), Scalar),
            ("cert_path", ty_str(), CStr),
            ("key_path", ty_str(), CStr),
        ], ty_result(ty_tls_server_socket(), ty_str()))
    })));

    // TlsSocket methods — extern bindings
    items.push(equip_block("TlsSocket", vec![
        extern_method("read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_tls_read"),
        extern_method("read_exact", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_tls_read_exact"),
        extern_method("write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_tls_write"),
        extern_method("write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_tls_write_str"),
        extern_method("read_line", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_tls_read_line"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_tls_close"),
        extern_method("set_timeout", Ownership::MutableBorrow, &[("ms", ty_int())], ty_void(), "gorget_tls_set_timeout"),
    ]));

    // TlsServerSocket methods — extern bindings
    items.push(equip_block("TlsServerSocket", vec![
        extern_method("accept", Ownership::MutableBorrow, &[], ty_result(ty_tls_socket(), ty_str()), "gorget_tls_server_accept"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_tls_server_close"),
    ]));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── xtd.regex ───────────────────────────────────────────────

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

    // Free functions (handled by takes_cstr_for_str_param whitelists — ABI tags
    // don't reach the C dispatch names like gorget_regex_is_match_pat)
    items.push(Spanned::dummy(Item::Function(decl_fn("regex_compile", &[("pattern", ty_str())], ty_result(ty_regex(), ty_str())))));
    items.push(Spanned::dummy(Item::Function(decl_fn("regex_compile_with", &[("pattern", ty_str()), ("flags", ty_str())], ty_result(ty_regex(), ty_str())))));
    items.push(Spanned::dummy(Item::Function(extern_fn("regex_escape", &[("s", ty_str())], ty_string(), "gorget_regex_escape"))));
    items.push(Spanned::dummy(Item::Function(decl_fn("regex_is_match", &[("pattern", ty_str()), ("subject", ty_str())], ty_bool()))));
    items.push(Spanned::dummy(Item::Function(decl_fn("regex_find", &[("pattern", ty_str()), ("subject", ty_str())], ty_option(ty_match())))));
    items.push(Spanned::dummy(Item::Function(decl_fn("regex_replace", &[("pattern", ty_str()), ("subject", ty_str()), ("repl", ty_str())], ty_string()))));

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

/// Build an extern free function declaration with ABI annotations.
#[allow(dead_code)]
fn extern_fn_abi(name: &str, params: &[(&str, Type, crate::ir::abi::AbiKind)], ret: Type, c_symbol: &str) -> FunctionDef {
    let mut f = decl_fn_abi(name, params, ret);
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
        is_meta_op: false,
    })];
    for (pname, pty) in extra_params {
        params.push(Spanned::dummy(Param {
            type_: Spanned::dummy(pty.clone()),
            ownership: Ownership::Borrow,
            name: Spanned::dummy(pname.to_string()),
            default: None,
            is_live: false,
            live_group: None,
            is_meta_op: false,
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
        param_abis: vec![],
    }
}

fn gen_alloc_module() -> Module {
    let ty_checkpoint = || Type::Named {
        name: Spanned::dummy("ArenaCheckpoint".to_string()),
        generic_args: vec![],
    };
    let arena_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("Arena".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let checkpoint_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("ArenaCheckpoint".to_string()),
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
            Spanned::dummy(decl_method("checkpoint", Ownership::Borrow, &[], ty_checkpoint())),
            Spanned::dummy(decl_method("restore", Ownership::Borrow, &[("cp", ty_checkpoint())], ty_void())),
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
    let fba_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("FixedBufferAllocator".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let fba_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("FixedBufferAllocator".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("bytes_used", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("capacity", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("reset", Ownership::Borrow, &[], ty_void())),
            Spanned::dummy(decl_method("destroy", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    let fallback_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("FallbackAllocator".to_string()),
        generic_params: None,
        fields: vec![],
        doc_comment: None,
        span: Span::dummy(),
    }));
    let fallback_equip = Spanned::dummy(Item::Equip(EquipBlock {
        generic_params: None,
        trait_: None,
        type_: Spanned::dummy(Type::Named {
            name: Spanned::dummy("FallbackAllocator".to_string()),
            generic_args: vec![],
        }),
        via_field: None,
        where_clause: None,
        items: vec![
            Spanned::dummy(decl_method("primary_count", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("fallback_count", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("destroy", Ownership::Borrow, &[], ty_void())),
        ],
        span: Span::dummy(),
    }));
    Module {
        items: vec![
            arena_struct, checkpoint_struct, arena_equip,
            tracking_struct, tracking_equip,
            pool_struct, pool_equip,
            tlsf_struct, tlsf_equip,
            fba_struct, fba_equip,
            fallback_struct, fallback_equip,
        ],
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
            Spanned::dummy(decl_method("len", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("capacity", Ownership::Borrow, &[], ty_int())),
            Spanned::dummy(decl_method("is_closed", Ownership::Borrow, &[], ty_bool())),
            Spanned::dummy(decl_method("recv_timeout", Ownership::Borrow, &[("ms", ty_int())], Type::Named {
                name: Spanned::dummy("Option".to_string()),
                generic_args: vec![Spanned::dummy(ty_t())],
            })),
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

// ─── xtd.gl — OpenGL 2.1 Bindings ──────────────────────────────

fn gen_gl_module() -> Module {
    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque handle types
    items.push(opaque_struct("GLContext"));

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

    let fn_item = |f: FunctionDef| -> Spanned<Item> {
        Spanned::dummy(Item::Function(f))
    };

    // ── GL Constants ────────────────────────────────────────
    // Primitive types
    items.push(const_item("GL_POINTS", 0x0000));
    items.push(const_item("GL_LINES", 0x0001));
    items.push(const_item("GL_LINE_STRIP", 0x0003));
    items.push(const_item("GL_TRIANGLES", 0x0004));
    items.push(const_item("GL_TRIANGLE_STRIP", 0x0005));
    items.push(const_item("GL_TRIANGLE_FAN", 0x0006));
    items.push(const_item("GL_QUADS", 0x0007));
    // Enable/Disable
    items.push(const_item("GL_DEPTH_TEST", 0x0B71));
    items.push(const_item("GL_BLEND", 0x0BE2));
    items.push(const_item("GL_CULL_FACE", 0x0B44));
    items.push(const_item("GL_SCISSOR_TEST", 0x0C11));
    items.push(const_item("GL_STENCIL_TEST", 0x0B90));
    items.push(const_item("GL_TEXTURE_2D", 0x0DE1));
    items.push(const_item("GL_ALPHA_TEST", 0x0BC0));
    items.push(const_item("GL_POLYGON_OFFSET_FILL", 0x8037));
    items.push(const_item("GL_MULTISAMPLE", 0x809D));
    // Blend funcs
    items.push(const_item("GL_ZERO", 0));
    items.push(const_item("GL_ONE", 1));
    items.push(const_item("GL_SRC_COLOR", 0x0300));
    items.push(const_item("GL_ONE_MINUS_SRC_COLOR", 0x0301));
    items.push(const_item("GL_SRC_ALPHA", 0x0302));
    items.push(const_item("GL_ONE_MINUS_SRC_ALPHA", 0x0303));
    items.push(const_item("GL_DST_ALPHA", 0x0304));
    items.push(const_item("GL_ONE_MINUS_DST_ALPHA", 0x0305));
    items.push(const_item("GL_DST_COLOR", 0x0306));
    items.push(const_item("GL_ONE_MINUS_DST_COLOR", 0x0307));
    items.push(const_item("GL_SRC_ALPHA_SATURATE", 0x0308));
    // Depth func
    items.push(const_item("GL_NEVER", 0x0200));
    items.push(const_item("GL_LESS", 0x0201));
    items.push(const_item("GL_EQUAL", 0x0202));
    items.push(const_item("GL_LEQUAL", 0x0203));
    items.push(const_item("GL_GREATER", 0x0204));
    items.push(const_item("GL_NOTEQUAL", 0x0205));
    items.push(const_item("GL_GEQUAL", 0x0206));
    items.push(const_item("GL_ALWAYS", 0x0207));
    // Cull face
    items.push(const_item("GL_FRONT", 0x0404));
    items.push(const_item("GL_BACK", 0x0405));
    items.push(const_item("GL_FRONT_AND_BACK", 0x0408));
    // Front face winding
    items.push(const_item("GL_CW", 0x0900));
    items.push(const_item("GL_CCW", 0x0901));
    // Clear bits
    items.push(const_item("GL_COLOR_BUFFER_BIT", 0x4000));
    items.push(const_item("GL_DEPTH_BUFFER_BIT", 0x0100));
    items.push(const_item("GL_STENCIL_BUFFER_BIT", 0x0400));
    // Texture parameters
    items.push(const_item("GL_TEXTURE_MIN_FILTER", 0x2801));
    items.push(const_item("GL_TEXTURE_MAG_FILTER", 0x2800));
    items.push(const_item("GL_TEXTURE_WRAP_S", 0x2802));
    items.push(const_item("GL_TEXTURE_WRAP_T", 0x2803));
    items.push(const_item("GL_NEAREST", 0x2600));
    items.push(const_item("GL_LINEAR", 0x2601));
    items.push(const_item("GL_NEAREST_MIPMAP_NEAREST", 0x2700));
    items.push(const_item("GL_LINEAR_MIPMAP_NEAREST", 0x2701));
    items.push(const_item("GL_NEAREST_MIPMAP_LINEAR", 0x2702));
    items.push(const_item("GL_LINEAR_MIPMAP_LINEAR", 0x2703));
    items.push(const_item("GL_REPEAT", 0x2901));
    items.push(const_item("GL_CLAMP_TO_EDGE", 0x812F));
    items.push(const_item("GL_MIRRORED_REPEAT", 0x8370));
    // Pixel formats
    items.push(const_item("GL_RGB", 0x1907));
    items.push(const_item("GL_RGBA", 0x1908));
    items.push(const_item("GL_LUMINANCE", 0x1909));
    items.push(const_item("GL_LUMINANCE_ALPHA", 0x190A));
    items.push(const_item("GL_ALPHA", 0x1906));
    // Data types
    items.push(const_item("GL_UNSIGNED_BYTE", 0x1401));
    items.push(const_item("GL_UNSIGNED_SHORT", 0x1403));
    items.push(const_item("GL_UNSIGNED_INT", 0x1405));
    items.push(const_item("GL_FLOAT", 0x1406));
    items.push(const_item("GL_BYTE", 0x1400));
    items.push(const_item("GL_SHORT", 0x1402));
    items.push(const_item("GL_INT", 0x1404));
    // Buffer targets
    items.push(const_item("GL_ARRAY_BUFFER", 0x8892));
    items.push(const_item("GL_ELEMENT_ARRAY_BUFFER", 0x8893));
    // Buffer usage
    items.push(const_item("GL_STATIC_DRAW", 0x88E4));
    items.push(const_item("GL_DYNAMIC_DRAW", 0x88E8));
    items.push(const_item("GL_STREAM_DRAW", 0x88E0));
    // Shader types
    items.push(const_item("GL_VERTEX_SHADER", 0x8B31));
    items.push(const_item("GL_FRAGMENT_SHADER", 0x8B30));
    // Matrix modes (fixed-function)
    items.push(const_item("GL_MODELVIEW", 0x1700));
    items.push(const_item("GL_PROJECTION", 0x1701));
    items.push(const_item("GL_TEXTURE", 0x1702));
    // Stencil ops
    items.push(const_item("GL_KEEP", 0x1E00));
    items.push(const_item("GL_REPLACE", 0x1E01));
    items.push(const_item("GL_INCR", 0x1E02));
    items.push(const_item("GL_DECR", 0x1E03));
    items.push(const_item("GL_INVERT", 0x150A));
    items.push(const_item("GL_INCR_WRAP", 0x8507));
    items.push(const_item("GL_DECR_WRAP", 0x8508));
    // GetString targets
    items.push(const_item("GL_VENDOR", 0x1F00));
    items.push(const_item("GL_RENDERER", 0x1F01));
    items.push(const_item("GL_VERSION", 0x1F02));
    items.push(const_item("GL_EXTENSIONS", 0x1F03));
    // Texture units
    items.push(const_item("GL_TEXTURE0", 0x84C0));
    items.push(const_item("GL_TEXTURE1", 0x84C1));
    items.push(const_item("GL_TEXTURE2", 0x84C2));
    items.push(const_item("GL_TEXTURE3", 0x84C3));
    items.push(const_item("GL_TEXTURE4", 0x84C4));
    items.push(const_item("GL_TEXTURE5", 0x84C5));
    items.push(const_item("GL_TEXTURE6", 0x84C6));
    items.push(const_item("GL_TEXTURE7", 0x84C7));
    items.push(const_item("GL_MAX_TEXTURE_IMAGE_UNITS", 0x8872));
    // SDL GL attrs (for use with sdl_gl_set_attribute)
    items.push(const_item("SDL_GL_RED_SIZE", 0));
    items.push(const_item("SDL_GL_GREEN_SIZE", 1));
    items.push(const_item("SDL_GL_BLUE_SIZE", 2));
    items.push(const_item("SDL_GL_ALPHA_SIZE", 3));
    items.push(const_item("SDL_GL_BUFFER_SIZE", 4));
    items.push(const_item("SDL_GL_DOUBLEBUFFER", 5));
    items.push(const_item("SDL_GL_DEPTH_SIZE", 6));
    items.push(const_item("SDL_GL_STENCIL_SIZE", 7));
    items.push(const_item("SDL_GL_MULTISAMPLEBUFFERS", 13));
    items.push(const_item("SDL_GL_MULTISAMPLESAMPLES", 14));
    items.push(const_item("SDL_GL_CONTEXT_MAJOR_VERSION", 17));
    items.push(const_item("SDL_GL_CONTEXT_MINOR_VERSION", 18));
    items.push(const_item("SDL_GL_CONTEXT_PROFILE_MASK", 21));
    items.push(const_item("SDL_GL_CONTEXT_PROFILE_CORE", 0x0001));
    items.push(const_item("SDL_GL_CONTEXT_PROFILE_COMPATIBILITY", 0x0002));

    // Fixed-function state
    items.push(const_item("GL_FILL", 0x1B02));
    items.push(const_item("GL_LINE", 0x1B01));
    items.push(const_item("GL_POINT", 0x1B00));
    items.push(const_item("GL_FLAT", 0x1D00));
    items.push(const_item("GL_SMOOTH", 0x1D01));
    items.push(const_item("GL_MODULATE", 0x2100));
    items.push(const_item("GL_DECAL", 0x2101));
    items.push(const_item("GL_ADD", 0x0104));
    items.push(const_item("GL_TEXTURE_ENV_MODE", 0x2200));
    items.push(const_item("GL_TEXTURE_ENV", 0x2300));
    // Client-side vertex arrays
    items.push(const_item("GL_VERTEX_ARRAY", 0x8074));
    items.push(const_item("GL_NORMAL_ARRAY", 0x8075));
    items.push(const_item("GL_COLOR_ARRAY", 0x8076));
    items.push(const_item("GL_TEXTURE_COORD_ARRAY", 0x8078));
    // Additional constants
    items.push(const_item("GL_NONE", 0x0000));
    items.push(const_item("GL_CLAMP", 0x2900));
    items.push(const_item("GL_MAX_TEXTURE_SIZE", 0x0D33));
    items.push(const_item("GL_MAX_TEXTURE_UNITS", 0x84E2));
    items.push(const_item("GL_TEXTURE_MAX_ANISOTROPY_EXT", 0x84FE));
    items.push(const_item("GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT", 0x84FF));
    // Shader introspection
    items.push(const_item("GL_COMPILE_STATUS", 0x8B81));
    items.push(const_item("GL_LINK_STATUS", 0x8B82));
    items.push(const_item("GL_VALIDATE_STATUS", 0x8B83));
    items.push(const_item("GL_INFO_LOG_LENGTH", 0x8B84));
    // FBO
    items.push(const_item("GL_FRAMEBUFFER", 0x8D40));
    items.push(const_item("GL_READ_FRAMEBUFFER", 0x8CA8));
    items.push(const_item("GL_DRAW_FRAMEBUFFER", 0x8CA9));
    items.push(const_item("GL_RENDERBUFFER", 0x8D41));
    items.push(const_item("GL_COLOR_ATTACHMENT0", 0x8CE0));
    items.push(const_item("GL_DEPTH_ATTACHMENT", 0x8D00));
    items.push(const_item("GL_STENCIL_ATTACHMENT", 0x8D20));
    items.push(const_item("GL_DEPTH_STENCIL_ATTACHMENT", 0x821A));
    items.push(const_item("GL_FRAMEBUFFER_COMPLETE", 0x8CD5));
    items.push(const_item("GL_DEPTH24_STENCIL8", 0x88F0));
    items.push(const_item("GL_DEPTH_COMPONENT16", 0x81A1));
    items.push(const_item("GL_DEPTH_COMPONENT24", 0x81A6));
    items.push(const_item("GL_DEPTH_COMPONENT32", 0x81A7));
    // Polygon modes
    items.push(const_item("GL_BACK_LEFT", 0x0402));
    // Clip planes
    items.push(const_item("GL_CLIP_PLANE0", 0x3000));

    // ── SDL-GL Context Functions ────────────────────────────
    let ty_glctx = || Type::Named {
        name: Spanned::dummy("GLContext".to_string()),
        generic_args: vec![],
    };
    items.push(fn_item(extern_fn("gl_create_context", &[("window", ty_int())], ty_glctx(), "gorget_gl_create_context")));
    items.push(fn_item(extern_fn("gl_destroy_context", &[("ctx", ty_glctx())], ty_void(), "gorget_gl_destroy_context")));
    items.push(fn_item(extern_fn("gl_make_current", &[("window", ty_int()), ("ctx", ty_glctx())], ty_void(), "gorget_gl_make_current")));
    items.push(fn_item(extern_fn("gl_swap_window", &[("window", ty_int())], ty_void(), "gorget_gl_swap_window")));
    items.push(fn_item(extern_fn("gl_set_swap_interval", &[("interval", ty_int())], ty_void(), "gorget_gl_set_swap_interval")));
    items.push(fn_item(extern_fn("gl_set_attribute", &[("attr", ty_int()), ("value", ty_int())], ty_void(), "gorget_gl_set_attribute")));

    // ── GL State ────────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_enable", &[("cap", ty_int())], ty_void(), "gorget_gl_enable")));
    items.push(fn_item(extern_fn("gl_disable", &[("cap", ty_int())], ty_void(), "gorget_gl_disable")));
    items.push(fn_item(extern_fn("gl_blend_func", &[("sfactor", ty_int()), ("dfactor", ty_int())], ty_void(), "gorget_gl_blend_func")));
    items.push(fn_item(extern_fn("gl_depth_func", &[("func", ty_int())], ty_void(), "gorget_gl_depth_func")));
    items.push(fn_item(extern_fn("gl_depth_mask", &[("flag", ty_int())], ty_void(), "gorget_gl_depth_mask")));
    items.push(fn_item(extern_fn("gl_cull_face", &[("mode", ty_int())], ty_void(), "gorget_gl_cull_face")));
    items.push(fn_item(extern_fn("gl_front_face", &[("mode", ty_int())], ty_void(), "gorget_gl_front_face")));
    items.push(fn_item(extern_fn("gl_viewport", &[("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void(), "gorget_gl_viewport")));
    items.push(fn_item(extern_fn("gl_scissor", &[("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void(), "gorget_gl_scissor")));
    items.push(fn_item(extern_fn("gl_clear", &[("mask", ty_int())], ty_void(), "gorget_gl_clear")));
    items.push(fn_item(extern_fn("gl_clear_color", &[("r", ty_float()), ("g", ty_float()), ("b", ty_float()), ("a", ty_float())], ty_void(), "gorget_gl_clear_color")));
    items.push(fn_item(extern_fn("gl_polygon_offset", &[("factor", ty_float()), ("units", ty_float())], ty_void(), "gorget_gl_polygon_offset")));
    items.push(fn_item(extern_fn("gl_color_mask", &[("r", ty_int()), ("g", ty_int()), ("b", ty_int()), ("a", ty_int())], ty_void(), "gorget_gl_color_mask")));
    items.push(fn_item(extern_fn("gl_stencil_func", &[("func", ty_int()), ("ref_val", ty_int()), ("mask", ty_int())], ty_void(), "gorget_gl_stencil_func")));
    items.push(fn_item(extern_fn("gl_stencil_op", &[("sfail", ty_int()), ("dpfail", ty_int()), ("dppass", ty_int())], ty_void(), "gorget_gl_stencil_op")));
    items.push(fn_item(extern_fn("gl_stencil_mask", &[("mask", ty_int())], ty_void(), "gorget_gl_stencil_mask")));

    // ── Textures ────────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_gen_texture", &[], ty_int(), "gorget_gl_gen_texture")));
    items.push(fn_item(extern_fn("gl_delete_texture", &[("tex", ty_int())], ty_void(), "gorget_gl_delete_texture")));
    items.push(fn_item(extern_fn("gl_bind_texture", &[("target", ty_int()), ("tex", ty_int())], ty_void(), "gorget_gl_bind_texture")));
    items.push(fn_item(extern_fn("gl_tex_image_2d", &[("target", ty_int()), ("level", ty_int()), ("internal_format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("format", ty_int()), ("type_", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_tex_image_2d")));
    items.push(fn_item(extern_fn("gl_tex_parameter_i", &[("target", ty_int()), ("pname", ty_int()), ("param", ty_int())], ty_void(), "gorget_gl_tex_parameter_i")));
    items.push(fn_item(extern_fn("gl_generate_mipmap", &[("target", ty_int())], ty_void(), "gorget_gl_generate_mipmap")));
    items.push(fn_item(extern_fn("gl_active_texture", &[("unit", ty_int())], ty_void(), "gorget_gl_active_texture")));

    // ── VBO/Vertex Arrays ───────────────────────────────────
    items.push(fn_item(extern_fn("gl_gen_buffer", &[], ty_int(), "gorget_gl_gen_buffer")));
    items.push(fn_item(extern_fn("gl_delete_buffer", &[("buf", ty_int())], ty_void(), "gorget_gl_delete_buffer")));
    items.push(fn_item(extern_fn("gl_bind_buffer", &[("target", ty_int()), ("buf", ty_int())], ty_void(), "gorget_gl_bind_buffer")));
    items.push(fn_item(extern_fn("gl_buffer_data", &[("target", ty_int()), ("data", ty_vector_uint8()), ("usage", ty_int())], ty_void(), "gorget_gl_buffer_data")));
    items.push(fn_item(extern_fn("gl_buffer_sub_data", &[("target", ty_int()), ("offset", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_buffer_sub_data")));
    items.push(fn_item(extern_fn("gl_draw_arrays", &[("mode", ty_int()), ("first", ty_int()), ("count", ty_int())], ty_void(), "gorget_gl_draw_arrays")));
    items.push(fn_item(extern_fn("gl_draw_elements", &[("mode", ty_int()), ("count", ty_int()), ("type_", ty_int()), ("offset", ty_int())], ty_void(), "gorget_gl_draw_elements")));
    items.push(fn_item(extern_fn("gl_vertex_attrib_pointer", &[("index", ty_int()), ("size", ty_int()), ("type_", ty_int()), ("normalized", ty_int()), ("stride", ty_int()), ("offset", ty_int())], ty_void(), "gorget_gl_vertex_attrib_pointer")));
    items.push(fn_item(extern_fn("gl_enable_vertex_attrib_array", &[("index", ty_int())], ty_void(), "gorget_gl_enable_vertex_attrib_array")));
    items.push(fn_item(extern_fn("gl_disable_vertex_attrib_array", &[("index", ty_int())], ty_void(), "gorget_gl_disable_vertex_attrib_array")));

    // ── Shaders ─────────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_create_shader", &[("type_", ty_int())], ty_int(), "gorget_gl_create_shader")));
    {
    use crate::ir::abi::AbiKind::{GorgetString as Str, Scalar as S};
    items.push(fn_item(extern_fn_abi("gl_shader_source", &[("shader", ty_int(), S), ("source", ty_str(), Str)], ty_void(), "gorget_gl_shader_source")));
    items.push(fn_item(extern_fn("gl_compile_shader", &[("shader", ty_int())], ty_void(), "gorget_gl_compile_shader")));
    items.push(fn_item(extern_fn("gl_create_program", &[], ty_int(), "gorget_gl_create_program")));
    items.push(fn_item(extern_fn("gl_attach_shader", &[("program", ty_int()), ("shader", ty_int())], ty_void(), "gorget_gl_attach_shader")));
    items.push(fn_item(extern_fn("gl_link_program", &[("program", ty_int())], ty_void(), "gorget_gl_link_program")));
    items.push(fn_item(extern_fn("gl_use_program", &[("program", ty_int())], ty_void(), "gorget_gl_use_program")));
    items.push(fn_item(extern_fn("gl_delete_shader", &[("shader", ty_int())], ty_void(), "gorget_gl_delete_shader")));
    items.push(fn_item(extern_fn("gl_delete_program", &[("program", ty_int())], ty_void(), "gorget_gl_delete_program")));
    items.push(fn_item(extern_fn("gl_get_shader_info_log", &[("shader", ty_int())], ty_str(), "gorget_gl_get_shader_info_log")));
    items.push(fn_item(extern_fn("gl_get_program_info_log", &[("program", ty_int())], ty_str(), "gorget_gl_get_program_info_log")));

    // ── Uniforms ────────────────────────────────────────────
    items.push(fn_item(extern_fn_abi("gl_get_uniform_location", &[("program", ty_int(), S), ("name", ty_str(), Str)], ty_int(), "gorget_gl_get_uniform_location")));
    items.push(fn_item(extern_fn("gl_uniform_1i", &[("location", ty_int()), ("v0", ty_int())], ty_void(), "gorget_gl_uniform_1i")));
    items.push(fn_item(extern_fn("gl_uniform_1f", &[("location", ty_int()), ("v0", ty_float())], ty_void(), "gorget_gl_uniform_1f")));
    items.push(fn_item(extern_fn("gl_uniform_2f", &[("location", ty_int()), ("v0", ty_float()), ("v1", ty_float())], ty_void(), "gorget_gl_uniform_2f")));
    items.push(fn_item(extern_fn("gl_uniform_3f", &[("location", ty_int()), ("v0", ty_float()), ("v1", ty_float()), ("v2", ty_float())], ty_void(), "gorget_gl_uniform_3f")));
    items.push(fn_item(extern_fn("gl_uniform_4f", &[("location", ty_int()), ("v0", ty_float()), ("v1", ty_float()), ("v2", ty_float()), ("v3", ty_float())], ty_void(), "gorget_gl_uniform_4f")));
    items.push(fn_item(extern_fn("gl_uniform_matrix4fv", &[("location", ty_int()), ("transpose", ty_int()), ("value", ty_vector_uint8())], ty_void(), "gorget_gl_uniform_matrix4fv")));

    // ── Fixed-function (Q3 compat) ──────────────────────────
    items.push(fn_item(extern_fn("gl_matrix_mode", &[("mode", ty_int())], ty_void(), "gorget_gl_matrix_mode")));
    items.push(fn_item(extern_fn("gl_load_identity", &[], ty_void(), "gorget_gl_load_identity")));
    items.push(fn_item(extern_fn("gl_load_matrix", &[("m", ty_vector_uint8())], ty_void(), "gorget_gl_load_matrix")));
    items.push(fn_item(extern_fn("gl_push_matrix", &[], ty_void(), "gorget_gl_push_matrix")));
    items.push(fn_item(extern_fn("gl_pop_matrix", &[], ty_void(), "gorget_gl_pop_matrix")));
    items.push(fn_item(extern_fn("gl_begin", &[("mode", ty_int())], ty_void(), "gorget_gl_begin")));
    items.push(fn_item(extern_fn("gl_end", &[], ty_void(), "gorget_gl_end")));
    items.push(fn_item(extern_fn("gl_vertex3f", &[("x", ty_float()), ("y", ty_float()), ("z", ty_float())], ty_void(), "gorget_gl_vertex3f")));
    items.push(fn_item(extern_fn("gl_tex_coord2f", &[("s", ty_float()), ("t", ty_float())], ty_void(), "gorget_gl_tex_coord2f")));
    items.push(fn_item(extern_fn("gl_normal3f", &[("x", ty_float()), ("y", ty_float()), ("z", ty_float())], ty_void(), "gorget_gl_normal3f")));
    items.push(fn_item(extern_fn("gl_color4f", &[("r", ty_float()), ("g", ty_float()), ("b", ty_float()), ("a", ty_float())], ty_void(), "gorget_gl_color4f")));
    items.push(fn_item(extern_fn("gl_color3f", &[("r", ty_float()), ("g", ty_float()), ("b", ty_float())], ty_void(), "gorget_gl_color3f")));

    // ── Client-side Vertex Arrays (Q3 renderergl1) ──────────
    items.push(fn_item(extern_fn("gl_vertex_pointer", &[("size", ty_int()), ("type_", ty_int()), ("stride", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_vertex_pointer")));
    items.push(fn_item(extern_fn("gl_tex_coord_pointer", &[("size", ty_int()), ("type_", ty_int()), ("stride", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_tex_coord_pointer")));
    items.push(fn_item(extern_fn("gl_color_pointer", &[("size", ty_int()), ("type_", ty_int()), ("stride", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_color_pointer")));
    items.push(fn_item(extern_fn("gl_normal_pointer", &[("type_", ty_int()), ("stride", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_normal_pointer")));
    items.push(fn_item(extern_fn("gl_enable_client_state", &[("cap", ty_int())], ty_void(), "gorget_gl_enable_client_state")));
    items.push(fn_item(extern_fn("gl_disable_client_state", &[("cap", ty_int())], ty_void(), "gorget_gl_disable_client_state")));

    // ── Fixed-function State (Q3) ───────────────────────────
    items.push(fn_item(extern_fn("gl_alpha_func", &[("func", ty_int()), ("ref_val", ty_float())], ty_void(), "gorget_gl_alpha_func")));
    items.push(fn_item(extern_fn("gl_shade_model", &[("mode", ty_int())], ty_void(), "gorget_gl_shade_model")));
    items.push(fn_item(extern_fn("gl_tex_env_f", &[("target", ty_int()), ("pname", ty_int()), ("param", ty_int())], ty_void(), "gorget_gl_tex_env_f")));
    items.push(fn_item(extern_fn("gl_depth_range", &[("near", ty_float()), ("far", ty_float())], ty_void(), "gorget_gl_depth_range")));
    items.push(fn_item(extern_fn("gl_polygon_mode", &[("face", ty_int()), ("mode", ty_int())], ty_void(), "gorget_gl_polygon_mode")));
    items.push(fn_item(extern_fn("gl_translate_f", &[("x", ty_float()), ("y", ty_float()), ("z", ty_float())], ty_void(), "gorget_gl_translate_f")));
    items.push(fn_item(extern_fn("gl_scale_f", &[("x", ty_float()), ("y", ty_float()), ("z", ty_float())], ty_void(), "gorget_gl_scale_f")));
    items.push(fn_item(extern_fn("gl_rotate_f", &[("angle", ty_float()), ("x", ty_float()), ("y", ty_float()), ("z", ty_float())], ty_void(), "gorget_gl_rotate_f")));

    // ── Projection / Clipping ───────────────────────────────
    items.push(fn_item(extern_fn("gl_ortho", &[("left", ty_float()), ("right", ty_float()), ("bottom", ty_float()), ("top", ty_float()), ("near", ty_float()), ("far", ty_float())], ty_void(), "gorget_gl_ortho")));
    items.push(fn_item(extern_fn("gl_frustum", &[("left", ty_float()), ("right", ty_float()), ("bottom", ty_float()), ("top", ty_float()), ("near", ty_float()), ("far", ty_float())], ty_void(), "gorget_gl_frustum")));
    items.push(fn_item(extern_fn("gl_clip_plane", &[("plane", ty_int()), ("equation", ty_vector_uint8())], ty_void(), "gorget_gl_clip_plane")));

    // ── Draw Control ────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_draw_buffer", &[("mode", ty_int())], ty_void(), "gorget_gl_draw_buffer")));
    items.push(fn_item(extern_fn("gl_read_pixels", &[("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int()), ("format", ty_int()), ("type_", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_read_pixels")));
    items.push(fn_item(extern_fn("gl_finish", &[], ty_void(), "gorget_gl_finish")));
    items.push(fn_item(extern_fn("gl_flush", &[], ty_void(), "gorget_gl_flush")));

    // ── Clear Control ───────────────────────────────────────
    items.push(fn_item(extern_fn("gl_clear_depth", &[("depth", ty_float())], ty_void(), "gorget_gl_clear_depth")));
    items.push(fn_item(extern_fn("gl_clear_stencil", &[("s", ty_int())], ty_void(), "gorget_gl_clear_stencil")));

    // ── Additional Texture ──────────────────────────────────
    items.push(fn_item(extern_fn("gl_tex_sub_image_2d", &[("target", ty_int()), ("level", ty_int()), ("xoff", ty_int()), ("yoff", ty_int()), ("w", ty_int()), ("h", ty_int()), ("format", ty_int()), ("type_", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_tex_sub_image_2d")));
    items.push(fn_item(extern_fn("gl_tex_parameter_f", &[("target", ty_int()), ("pname", ty_int()), ("param", ty_float())], ty_void(), "gorget_gl_tex_parameter_f")));
    items.push(fn_item(extern_fn("gl_copy_tex_sub_image_2d", &[("target", ty_int()), ("level", ty_int()), ("xoff", ty_int()), ("yoff", ty_int()), ("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void(), "gorget_gl_copy_tex_sub_image_2d")));

    // ── Debug ───────────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_line_width", &[("width", ty_float())], ty_void(), "gorget_gl_line_width")));

    // ── Multitexture ────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_client_active_texture", &[("texture", ty_int())], ty_void(), "gorget_gl_client_active_texture")));
    items.push(fn_item(extern_fn("gl_multi_tex_coord2f", &[("target", ty_int()), ("s", ty_float()), ("t", ty_float())], ty_void(), "gorget_gl_multi_tex_coord2f")));

    // ── Immediate Mode Variants ─────────────────────────────
    items.push(fn_item(extern_fn("gl_vertex2f", &[("x", ty_float()), ("y", ty_float())], ty_void(), "gorget_gl_vertex2f")));

    // ── Compiled Vertex Arrays (EXT) ────────────────────────
    items.push(fn_item(extern_fn("gl_lock_arrays_ext", &[("first", ty_int()), ("count", ty_int())], ty_void(), "gorget_gl_lock_arrays_ext")));
    items.push(fn_item(extern_fn("gl_unlock_arrays_ext", &[], ty_void(), "gorget_gl_unlock_arrays_ext")));

    // ── Shader Introspection ────────────────────────────────
    items.push(fn_item(extern_fn("gl_get_shader_iv", &[("shader", ty_int()), ("pname", ty_int())], ty_int(), "gorget_gl_get_shader_iv")));
    items.push(fn_item(extern_fn("gl_get_program_iv", &[("program", ty_int()), ("pname", ty_int())], ty_int(), "gorget_gl_get_program_iv")));
    items.push(fn_item(extern_fn("gl_validate_program", &[("program", ty_int())], ty_void(), "gorget_gl_validate_program")));
    items.push(fn_item(extern_fn("gl_detach_shader", &[("program", ty_int()), ("shader", ty_int())], ty_void(), "gorget_gl_detach_shader")));
    items.push(fn_item(extern_fn_abi("gl_bind_attrib_location", &[("program", ty_int(), S), ("index", ty_int(), S), ("name", ty_str(), Str)], ty_void(), "gorget_gl_bind_attrib_location")));
    items.push(fn_item(extern_fn_abi("gl_get_attrib_location", &[("program", ty_int(), S), ("name", ty_str(), Str)], ty_int(), "gorget_gl_get_attrib_location")));

    // ── Framebuffer Objects ─────────────────────────────────
    items.push(fn_item(extern_fn("gl_gen_framebuffer", &[], ty_int(), "gorget_gl_gen_framebuffer")));
    items.push(fn_item(extern_fn("gl_delete_framebuffer", &[("fb", ty_int())], ty_void(), "gorget_gl_delete_framebuffer")));
    items.push(fn_item(extern_fn("gl_bind_framebuffer", &[("target", ty_int()), ("fb", ty_int())], ty_void(), "gorget_gl_bind_framebuffer")));
    items.push(fn_item(extern_fn("gl_check_framebuffer_status", &[("target", ty_int())], ty_int(), "gorget_gl_check_framebuffer_status")));
    items.push(fn_item(extern_fn("gl_framebuffer_texture_2d", &[("target", ty_int()), ("attachment", ty_int()), ("tex_target", ty_int()), ("texture", ty_int()), ("level", ty_int())], ty_void(), "gorget_gl_framebuffer_texture_2d")));
    items.push(fn_item(extern_fn("gl_framebuffer_renderbuffer", &[("target", ty_int()), ("attachment", ty_int()), ("rb_target", ty_int()), ("renderbuffer", ty_int())], ty_void(), "gorget_gl_framebuffer_renderbuffer")));
    items.push(fn_item(extern_fn("gl_gen_renderbuffer", &[], ty_int(), "gorget_gl_gen_renderbuffer")));
    items.push(fn_item(extern_fn("gl_delete_renderbuffer", &[("rb", ty_int())], ty_void(), "gorget_gl_delete_renderbuffer")));
    items.push(fn_item(extern_fn("gl_bind_renderbuffer", &[("target", ty_int()), ("rb", ty_int())], ty_void(), "gorget_gl_bind_renderbuffer")));
    items.push(fn_item(extern_fn("gl_renderbuffer_storage", &[("target", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int())], ty_void(), "gorget_gl_renderbuffer_storage")));
    items.push(fn_item(extern_fn("gl_blit_framebuffer", &[("sx0", ty_int()), ("sy0", ty_int()), ("sx1", ty_int()), ("sy1", ty_int()), ("dx0", ty_int()), ("dy0", ty_int()), ("dx1", ty_int()), ("dy1", ty_int()), ("mask", ty_int()), ("filter", ty_int())], ty_void(), "gorget_gl_blit_framebuffer")));
    items.push(fn_item(extern_fn("gl_renderbuffer_storage_multisample", &[("target", ty_int()), ("samples", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int())], ty_void(), "gorget_gl_renderbuffer_storage_multisample")));

    // ── Query ───────────────────────────────────────────────
    items.push(fn_item(extern_fn("gl_get_error", &[], ty_int(), "gorget_gl_get_error")));
    items.push(fn_item(extern_fn("gl_get_string", &[("name", ty_int())], ty_str(), "gorget_gl_get_string")));
    items.push(fn_item(extern_fn("gl_get_integer", &[("pname", ty_int())], ty_int(), "gorget_gl_get_integer")));

    // ── VAO (Vertex Array Objects — GL 3.0+) ────────────────
    items.push(fn_item(extern_fn("gl_gen_vertex_array", &[], ty_int(), "gorget_gl_gen_vertex_array")));
    items.push(fn_item(extern_fn("gl_delete_vertex_array", &[("vao", ty_int())], ty_void(), "gorget_gl_delete_vertex_array")));
    items.push(fn_item(extern_fn("gl_bind_vertex_array", &[("vao", ty_int())], ty_void(), "gorget_gl_bind_vertex_array")));

    // ── Instanced Rendering (GL 3.1+) ───────────────────────
    items.push(fn_item(extern_fn("gl_draw_arrays_instanced", &[("mode", ty_int()), ("first", ty_int()), ("count", ty_int()), ("instance_count", ty_int())], ty_void(), "gorget_gl_draw_arrays_instanced")));
    items.push(fn_item(extern_fn("gl_draw_elements_instanced", &[("mode", ty_int()), ("count", ty_int()), ("type_", ty_int()), ("offset", ty_int()), ("instance_count", ty_int())], ty_void(), "gorget_gl_draw_elements_instanced")));
    items.push(fn_item(extern_fn("gl_vertex_attrib_divisor", &[("index", ty_int()), ("divisor", ty_int())], ty_void(), "gorget_gl_vertex_attrib_divisor")));

    // ── Occlusion Queries (GL 1.5+) ─────────────────────────
    items.push(fn_item(extern_fn("gl_gen_query", &[], ty_int(), "gorget_gl_gen_query")));
    items.push(fn_item(extern_fn("gl_delete_query", &[("query", ty_int())], ty_void(), "gorget_gl_delete_query")));
    items.push(fn_item(extern_fn("gl_begin_query", &[("target", ty_int()), ("query", ty_int())], ty_void(), "gorget_gl_begin_query")));
    items.push(fn_item(extern_fn("gl_end_query", &[("target", ty_int())], ty_void(), "gorget_gl_end_query")));
    items.push(fn_item(extern_fn("gl_get_query_result", &[("query", ty_int())], ty_int(), "gorget_gl_get_query_result")));
    items.push(fn_item(extern_fn("gl_get_query_result_available", &[("query", ty_int())], ty_int(), "gorget_gl_get_query_result_available")));

    // ── UBO (Uniform Buffer Objects — GL 3.1+) ──────────────
    items.push(fn_item(extern_fn("gl_bind_buffer_base", &[("target", ty_int()), ("index", ty_int()), ("buffer", ty_int())], ty_void(), "gorget_gl_bind_buffer_base")));
    items.push(fn_item(extern_fn("gl_bind_buffer_range", &[("target", ty_int()), ("index", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("size", ty_int())], ty_void(), "gorget_gl_bind_buffer_range")));
    items.push(fn_item(extern_fn_abi("gl_get_uniform_block_index", &[("program", ty_int(), S), ("name", ty_str(), Str)], ty_int(), "gorget_gl_get_uniform_block_index")));
    items.push(fn_item(extern_fn("gl_uniform_block_binding", &[("program", ty_int()), ("block_index", ty_int()), ("binding_point", ty_int())], ty_void(), "gorget_gl_uniform_block_binding")));

    // ── 3D Textures / Texture Arrays (GL 1.2+ / GL 3.0+) ────
    items.push(fn_item(extern_fn("gl_tex_image_3d", &[("target", ty_int()), ("level", ty_int()), ("internal_format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("depth", ty_int()), ("format", ty_int()), ("type_", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_tex_image_3d")));
    items.push(fn_item(extern_fn("gl_tex_sub_image_3d", &[("target", ty_int()), ("level", ty_int()), ("xoff", ty_int()), ("yoff", ty_int()), ("zoff", ty_int()), ("w", ty_int()), ("h", ty_int()), ("d", ty_int()), ("format", ty_int()), ("type_", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_tex_sub_image_3d")));

    // ── Geometry Shaders (GL 3.2+) ──────────────────────────
    items.push(fn_item(extern_fn("gl_program_parameter_i", &[("program", ty_int()), ("pname", ty_int()), ("value", ty_int())], ty_void(), "gorget_gl_program_parameter_i")));

    // ── Map/Unmap Buffers (GL 1.5+) ─────────────────────────
    items.push(fn_item(extern_fn("gl_map_buffer", &[("target", ty_int()), ("access", ty_int())], ty_int(), "gorget_gl_map_buffer")));
    items.push(fn_item(extern_fn("gl_unmap_buffer", &[("target", ty_int())], ty_int(), "gorget_gl_unmap_buffer")));
    items.push(fn_item(extern_fn("gl_map_buffer_range", &[("target", ty_int()), ("offset", ty_int()), ("length", ty_int()), ("access", ty_int())], ty_int(), "gorget_gl_map_buffer_range")));
    items.push(fn_item(extern_fn("gl_flush_mapped_buffer_range", &[("target", ty_int()), ("offset", ty_int()), ("length", ty_int())], ty_void(), "gorget_gl_flush_mapped_buffer_range")));

    // ── Additional Uniform Types ────────────────────────────
    items.push(fn_item(extern_fn("gl_uniform_3fv", &[("location", ty_int()), ("count", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_uniform_3fv")));
    items.push(fn_item(extern_fn("gl_uniform_4fv", &[("location", ty_int()), ("count", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_uniform_4fv")));
    items.push(fn_item(extern_fn("gl_uniform_matrix3fv", &[("location", ty_int()), ("transpose", ty_int()), ("value", ty_vector_uint8())], ty_void(), "gorget_gl_uniform_matrix3fv")));

    // ── Blend Separate (GL 1.4+) ────────────────────────────
    items.push(fn_item(extern_fn("gl_blend_func_separate", &[("src_rgb", ty_int()), ("dst_rgb", ty_int()), ("src_alpha", ty_int()), ("dst_alpha", ty_int())], ty_void(), "gorget_gl_blend_func_separate")));
    items.push(fn_item(extern_fn("gl_blend_equation", &[("mode", ty_int())], ty_void(), "gorget_gl_blend_equation")));
    items.push(fn_item(extern_fn("gl_blend_equation_separate", &[("mode_rgb", ty_int()), ("mode_alpha", ty_int())], ty_void(), "gorget_gl_blend_equation_separate")));

    // ── Point Sprites (GL 1.4+) ─────────────────────────────
    items.push(fn_item(extern_fn("gl_point_size", &[("size", ty_float())], ty_void(), "gorget_gl_point_size")));

    // ── Additional GL Constants (GL 3.0+) ────────────────────
    // VAO binding
    items.push(const_item("GL_VERTEX_ARRAY_BINDING", 0x85B5));
    // Query targets
    items.push(const_item("GL_SAMPLES_PASSED", 0x8914));
    items.push(const_item("GL_ANY_SAMPLES_PASSED", 0x8C2F));
    items.push(const_item("GL_PRIMITIVES_GENERATED", 0x8C87));
    items.push(const_item("GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN", 0x8C88));
    items.push(const_item("GL_QUERY_RESULT", 0x8866));
    items.push(const_item("GL_QUERY_RESULT_AVAILABLE", 0x8867));
    // UBO
    items.push(const_item("GL_UNIFORM_BUFFER", 0x8A11));
    items.push(const_item("GL_MAX_UNIFORM_BUFFER_BINDINGS", 0x8A2F));
    // Texture3D / Array
    items.push(const_item("GL_TEXTURE_3D", 0x806F));
    items.push(const_item("GL_TEXTURE_2D_ARRAY", 0x8C1A));
    items.push(const_item("GL_TEXTURE_WRAP_R", 0x8072));
    // Geometry shader
    items.push(const_item("GL_GEOMETRY_SHADER", 0x8DD9));
    items.push(const_item("GL_GEOMETRY_INPUT_TYPE", 0x8DDB));
    items.push(const_item("GL_GEOMETRY_OUTPUT_TYPE", 0x8DDC));
    items.push(const_item("GL_GEOMETRY_VERTICES_OUT", 0x8DDA));
    // sRGB
    items.push(const_item("GL_FRAMEBUFFER_SRGB", 0x8DB9));
    items.push(const_item("GL_SRGB", 0x8C40));
    items.push(const_item("GL_SRGB8", 0x8C41));
    items.push(const_item("GL_SRGB_ALPHA", 0x8C42));
    items.push(const_item("GL_SRGB8_ALPHA8", 0x8C43));
    // Buffer mapping
    items.push(const_item("GL_READ_ONLY", 0x88B8));
    items.push(const_item("GL_WRITE_ONLY", 0x88B9));
    items.push(const_item("GL_READ_WRITE", 0x88BA));
    items.push(const_item("GL_MAP_READ_BIT", 0x0001));
    items.push(const_item("GL_MAP_WRITE_BIT", 0x0002));
    items.push(const_item("GL_MAP_INVALIDATE_RANGE_BIT", 0x0004));
    items.push(const_item("GL_MAP_INVALIDATE_BUFFER_BIT", 0x0008));
    items.push(const_item("GL_MAP_FLUSH_EXPLICIT_BIT", 0x0010));
    items.push(const_item("GL_MAP_UNSYNCHRONIZED_BIT", 0x0020));
    // Blend equations
    items.push(const_item("GL_FUNC_ADD", 0x8006));
    items.push(const_item("GL_FUNC_SUBTRACT", 0x800A));
    items.push(const_item("GL_FUNC_REVERSE_SUBTRACT", 0x800B));
    items.push(const_item("GL_MIN", 0x8007));
    items.push(const_item("GL_MAX", 0x8008));
    // Color attachments 1-7 (for MRT)
    items.push(const_item("GL_COLOR_ATTACHMENT1", 0x8CE1));
    items.push(const_item("GL_COLOR_ATTACHMENT2", 0x8CE2));
    items.push(const_item("GL_COLOR_ATTACHMENT3", 0x8CE3));
    items.push(const_item("GL_COLOR_ATTACHMENT4", 0x8CE4));
    items.push(const_item("GL_COLOR_ATTACHMENT5", 0x8CE5));
    items.push(const_item("GL_COLOR_ATTACHMENT6", 0x8CE6));
    items.push(const_item("GL_COLOR_ATTACHMENT7", 0x8CE7));
    // Draw buffers (for MRT)
    items.push(const_item("GL_MAX_DRAW_BUFFERS", 0x8824));
    items.push(const_item("GL_MAX_COLOR_ATTACHMENTS", 0x8CDF));
    // Point sprites
    items.push(const_item("GL_POINT_SPRITE", 0x8861));
    items.push(const_item("GL_PROGRAM_POINT_SIZE", 0x8642));

    // ── MRT Draw Buffers (GL 2.0+) ──────────────────────────
    items.push(fn_item(extern_fn("gl_draw_buffers", &[("count", ty_int()), ("bufs", ty_vector_uint8())], ty_void(), "gorget_gl_draw_buffers")));

    // ══════════════════════════════════════════════════════════
    // GL Tier 3 — GL 3.3 through GL 4.6 advanced features
    // ══════════════════════════════════════════════════════════

    // ── Framebuffer/Renderbuffer already in Tier 2 above ─────

    // ── GL 3.2+ functions (not available on macOS GL headers) ──
    #[cfg(not(target_os = "macos"))]
    {
    // ── Sampler Objects (GL 3.3+) ───────────────────────────
    items.push(fn_item(extern_fn("gl_gen_sampler", &[], ty_int(), "gorget_gl_gen_sampler")));
    items.push(fn_item(extern_fn("gl_delete_sampler", &[("sampler", ty_int())], ty_void(), "gorget_gl_delete_sampler")));
    items.push(fn_item(extern_fn("gl_bind_sampler", &[("unit", ty_int()), ("sampler", ty_int())], ty_void(), "gorget_gl_bind_sampler")));
    items.push(fn_item(extern_fn("gl_sampler_parameter_i", &[("sampler", ty_int()), ("pname", ty_int()), ("param", ty_int())], ty_void(), "gorget_gl_sampler_parameter_i")));
    items.push(fn_item(extern_fn("gl_sampler_parameter_f", &[("sampler", ty_int()), ("pname", ty_int()), ("param", ty_float())], ty_void(), "gorget_gl_sampler_parameter_f")));

    // ── Compute Shaders (GL 4.3+) ───────────────────────────
    items.push(fn_item(extern_fn("gl_dispatch_compute", &[("num_groups_x", ty_int()), ("num_groups_y", ty_int()), ("num_groups_z", ty_int())], ty_void(), "gorget_gl_dispatch_compute")));
    items.push(fn_item(extern_fn("gl_dispatch_compute_indirect", &[("indirect_offset", ty_int())], ty_void(), "gorget_gl_dispatch_compute_indirect")));
    items.push(fn_item(extern_fn("gl_memory_barrier", &[("barriers", ty_int())], ty_void(), "gorget_gl_memory_barrier")));

    // ── Shader Storage Buffer Objects (GL 4.3+) ─────────────
    items.push(fn_item(extern_fn("gl_shader_storage_block_binding", &[("program", ty_int()), ("block_index", ty_int()), ("binding", ty_int())], ty_void(), "gorget_gl_shader_storage_block_binding")));
    items.push(fn_item(extern_fn_abi("gl_get_program_resource_index", &[("program", ty_int(), S), ("interface", ty_int(), S), ("name", ty_str(), Str)], ty_int(), "gorget_gl_get_program_resource_index")));

    // ── Immutable Texture Storage (GL 4.2+) ─────────────────
    items.push(fn_item(extern_fn("gl_tex_storage_2d", &[("target", ty_int()), ("levels", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int())], ty_void(), "gorget_gl_tex_storage_2d")));
    items.push(fn_item(extern_fn("gl_tex_storage_3d", &[("target", ty_int()), ("levels", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("depth", ty_int())], ty_void(), "gorget_gl_tex_storage_3d")));

    // ── Buffer Storage (GL 4.4+) ─────────────────────────────
    items.push(fn_item(extern_fn("gl_buffer_storage", &[("target", ty_int()), ("size", ty_int()), ("data", ty_vector_uint8()), ("flags", ty_int())], ty_void(), "gorget_gl_buffer_storage")));

    // ── Indirect Draw (GL 4.0+) ─────────────────────────────
    items.push(fn_item(extern_fn("gl_draw_arrays_indirect", &[("mode", ty_int()), ("indirect_offset", ty_int())], ty_void(), "gorget_gl_draw_arrays_indirect")));
    items.push(fn_item(extern_fn("gl_draw_elements_indirect", &[("mode", ty_int()), ("type_", ty_int()), ("indirect_offset", ty_int())], ty_void(), "gorget_gl_draw_elements_indirect")));
    items.push(fn_item(extern_fn("gl_multi_draw_arrays_indirect", &[("mode", ty_int()), ("indirect_offset", ty_int()), ("draw_count", ty_int()), ("stride", ty_int())], ty_void(), "gorget_gl_multi_draw_arrays_indirect")));
    items.push(fn_item(extern_fn("gl_multi_draw_elements_indirect", &[("mode", ty_int()), ("type_", ty_int()), ("indirect_offset", ty_int()), ("draw_count", ty_int()), ("stride", ty_int())], ty_void(), "gorget_gl_multi_draw_elements_indirect")));

    // ── Tessellation (GL 4.0+) ──────────────────────────────
    items.push(fn_item(extern_fn("gl_patch_parameter_i", &[("pname", ty_int()), ("value", ty_int())], ty_void(), "gorget_gl_patch_parameter_i")));
    items.push(fn_item(extern_fn("gl_patch_parameter_fv", &[("pname", ty_int()), ("values", ty_vector_uint8())], ty_void(), "gorget_gl_patch_parameter_fv")));

    // ── Transform Feedback (GL 3.0+) ────────────────────────
    items.push(fn_item(extern_fn("gl_gen_transform_feedback", &[], ty_int(), "gorget_gl_gen_transform_feedback")));
    items.push(fn_item(extern_fn("gl_delete_transform_feedback", &[("id", ty_int())], ty_void(), "gorget_gl_delete_transform_feedback")));
    items.push(fn_item(extern_fn("gl_bind_transform_feedback", &[("target", ty_int()), ("id", ty_int())], ty_void(), "gorget_gl_bind_transform_feedback")));
    items.push(fn_item(extern_fn("gl_begin_transform_feedback", &[("mode", ty_int())], ty_void(), "gorget_gl_begin_transform_feedback")));
    items.push(fn_item(extern_fn("gl_end_transform_feedback", &[], ty_void(), "gorget_gl_end_transform_feedback")));
    items.push(fn_item(extern_fn_abi("gl_transform_feedback_varyings", &[("program", ty_int(), S), ("count", ty_int(), S), ("varyings", ty_str(), Str), ("mode", ty_int(), S)], ty_void(), "gorget_gl_transform_feedback_varyings")));

    // ── Clip Control (GL 4.5+) ──────────────────────────────
    items.push(fn_item(extern_fn("gl_clip_control", &[("origin", ty_int()), ("depth", ty_int())], ty_void(), "gorget_gl_clip_control")));

    // ── Debug Output (GL 4.3+) ──────────────────────────────
    items.push(fn_item(extern_fn("gl_debug_message_control", &[("source", ty_int()), ("type_", ty_int()), ("severity", ty_int()), ("enabled", ty_int())], ty_void(), "gorget_gl_debug_message_control")));
    items.push(fn_item(extern_fn_abi("gl_object_label", &[("identifier", ty_int(), S), ("name", ty_int(), S), ("label", ty_str(), Str)], ty_void(), "gorget_gl_object_label")));
    } // close GL ABI block

    // ── Copy Image (GL 4.3+) ────────────────────────────────
    items.push(fn_item(extern_fn("gl_copy_image_sub_data", &[("src", ty_int()), ("src_target", ty_int()), ("src_level", ty_int()), ("src_x", ty_int()), ("src_y", ty_int()), ("src_z", ty_int()), ("dst", ty_int()), ("dst_target", ty_int()), ("dst_level", ty_int()), ("dst_x", ty_int()), ("dst_y", ty_int()), ("dst_z", ty_int()), ("width", ty_int()), ("height", ty_int()), ("depth", ty_int())], ty_void(), "gorget_gl_copy_image_sub_data")));

    // ── Texture Multisample (GL 3.2+) ───────────────────────
    items.push(fn_item(extern_fn("gl_tex_image_2d_multisample", &[("target", ty_int()), ("samples", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("fixed_locations", ty_int())], ty_void(), "gorget_gl_tex_image_2d_multisample")));
    // ── Sync Objects (GL 3.2+) ──────────────────────────────
    items.push(fn_item(extern_fn("gl_fence_sync", &[], ty_int(), "gorget_gl_fence_sync")));
    items.push(fn_item(extern_fn("gl_delete_sync", &[("sync", ty_int())], ty_void(), "gorget_gl_delete_sync")));
    items.push(fn_item(extern_fn("gl_client_wait_sync", &[("sync", ty_int()), ("flags", ty_int()), ("timeout_ns", ty_int())], ty_int(), "gorget_gl_client_wait_sync")));
    items.push(fn_item(extern_fn("gl_wait_sync", &[("sync", ty_int())], ty_void(), "gorget_gl_wait_sync")));

    // ── Program Binary (GL 4.1+) ────────────────────────────
    items.push(fn_item(extern_fn("gl_get_program_binary", &[("program", ty_int()), ("data", ty_vector_uint8())], ty_int(), "gorget_gl_get_program_binary")));
    items.push(fn_item(extern_fn("gl_program_binary", &[("program", ty_int()), ("format", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_program_binary")));

    // ── Timer Queries (GL 3.3+) ─────────────────────────────
    items.push(fn_item(extern_fn("gl_query_counter", &[("id", ty_int()), ("target", ty_int())], ty_void(), "gorget_gl_query_counter")));
    items.push(fn_item(extern_fn("gl_get_query_result_i64", &[("id", ty_int())], ty_int(), "gorget_gl_get_query_result_i64")));
    } // end #[cfg(not(target_os = "macos"))]

    // ── Pixel Store (GL 1.0+) ───────────────────────────────
    items.push(fn_item(extern_fn("gl_pixel_store_i", &[("pname", ty_int()), ("param", ty_int())], ty_void(), "gorget_gl_pixel_store_i")));

    // ── Compressed Textures ─────────────────────────────────
    items.push(fn_item(extern_fn("gl_compressed_tex_image_2d", &[("target", ty_int()), ("level", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_compressed_tex_image_2d")));
    items.push(fn_item(extern_fn("gl_copy_tex_image_2d", &[("target", ty_int()), ("level", ty_int()), ("format", ty_int()), ("x", ty_int()), ("y", ty_int()), ("width", ty_int()), ("height", ty_int())], ty_void(), "gorget_gl_copy_tex_image_2d")));

    // ── Texture Download ────────────────────────────────────
    items.push(fn_item(extern_fn("gl_get_tex_image", &[("target", ty_int()), ("level", ty_int()), ("format", ty_int()), ("type_", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_gl_get_tex_image")));

    // ── Typed State Getters ─────────────────────────────────
    items.push(fn_item(extern_fn("gl_get_float", &[("pname", ty_int())], ty_float(), "gorget_gl_get_float")));

    // ── Object Query Functions ──────────────────────────────
    items.push(fn_item(extern_fn("gl_is_enabled", &[("cap", ty_int())], ty_bool(), "gorget_gl_is_enabled")));
    items.push(fn_item(extern_fn("gl_is_texture", &[("texture", ty_int())], ty_bool(), "gorget_gl_is_texture")));
    items.push(fn_item(extern_fn("gl_is_buffer", &[("buffer", ty_int())], ty_bool(), "gorget_gl_is_buffer")));
    items.push(fn_item(extern_fn("gl_is_shader", &[("shader", ty_int())], ty_bool(), "gorget_gl_is_shader")));
    items.push(fn_item(extern_fn("gl_is_program", &[("program", ty_int())], ty_bool(), "gorget_gl_is_program")));
    items.push(fn_item(extern_fn("gl_is_framebuffer", &[("framebuffer", ty_int())], ty_bool(), "gorget_gl_is_framebuffer")));

    // ── GL Tier 3 Constants ─────────────────────────────────

    // Framebuffer/Renderbuffer constants already in Tier 2 above

    // Compute shader
    items.push(const_item("GL_COMPUTE_SHADER", 0x91B9));
    items.push(const_item("GL_MAX_COMPUTE_WORK_GROUP_COUNT", 0x91BE));
    items.push(const_item("GL_MAX_COMPUTE_WORK_GROUP_SIZE", 0x91BF));
    items.push(const_item("GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS", 0x90EB));
    // Memory barriers
    items.push(const_item("GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT", 0x00000001));
    items.push(const_item("GL_ELEMENT_ARRAY_BARRIER_BIT", 0x00000002));
    items.push(const_item("GL_UNIFORM_BARRIER_BIT", 0x00000004));
    items.push(const_item("GL_TEXTURE_FETCH_BARRIER_BIT", 0x00000008));
    items.push(const_item("GL_SHADER_IMAGE_ACCESS_BARRIER_BIT", 0x00000020));
    items.push(const_item("GL_COMMAND_BARRIER_BIT", 0x00000040));
    items.push(const_item("GL_PIXEL_BUFFER_BARRIER_BIT", 0x00000080));
    items.push(const_item("GL_TEXTURE_UPDATE_BARRIER_BIT", 0x00000100));
    items.push(const_item("GL_BUFFER_UPDATE_BARRIER_BIT", 0x00000200));
    items.push(const_item("GL_FRAMEBUFFER_BARRIER_BIT", 0x00000400));
    items.push(const_item("GL_TRANSFORM_FEEDBACK_BARRIER_BIT", 0x00000800));
    items.push(const_item("GL_ATOMIC_COUNTER_BARRIER_BIT", 0x00001000));
    items.push(const_item("GL_SHADER_STORAGE_BARRIER_BIT", 0x00002000));
    items.push(const_item("GL_ALL_BARRIER_BITS", 0xFFFFFFFF_u32 as i64));
    // SSBO
    items.push(const_item("GL_SHADER_STORAGE_BUFFER", 0x90D2));
    items.push(const_item("GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS", 0x90DD));
    items.push(const_item("GL_SHADER_STORAGE_BUFFER_BINDING", 0x90D3));
    // Immutable texture storage formats
    items.push(const_item("GL_R8", 0x8229));
    items.push(const_item("GL_RG8", 0x822B));
    items.push(const_item("GL_RGBA8", 0x8058));
    items.push(const_item("GL_R16F", 0x822D));
    items.push(const_item("GL_RG16F", 0x822F));
    items.push(const_item("GL_RGBA16F", 0x881A));
    items.push(const_item("GL_R32F", 0x822E));
    items.push(const_item("GL_RG32F", 0x8230));
    items.push(const_item("GL_RGBA32F", 0x8814));
    items.push(const_item("GL_R11F_G11F_B10F", 0x8C3A));
    items.push(const_item("GL_RGB10_A2", 0x8059));
    items.push(const_item("GL_R32UI", 0x8236));
    items.push(const_item("GL_RGBA32UI", 0x8238));
    // Buffer storage flags
    items.push(const_item("GL_DYNAMIC_STORAGE_BIT", 0x0100));
    items.push(const_item("GL_MAP_READ_BIT_STORAGE", 0x0001));
    items.push(const_item("GL_MAP_WRITE_BIT_STORAGE", 0x0002));
    items.push(const_item("GL_MAP_PERSISTENT_BIT", 0x0040));
    items.push(const_item("GL_MAP_COHERENT_BIT", 0x0080));
    items.push(const_item("GL_CLIENT_STORAGE_BIT", 0x0200));
    // Indirect draw
    items.push(const_item("GL_DRAW_INDIRECT_BUFFER", 0x8F3F));
    items.push(const_item("GL_DISPATCH_INDIRECT_BUFFER", 0x90EE));
    // Tessellation
    items.push(const_item("GL_TESS_CONTROL_SHADER", 0x8E88));
    items.push(const_item("GL_TESS_EVALUATION_SHADER", 0x8E87));
    items.push(const_item("GL_PATCHES", 0x000E));
    items.push(const_item("GL_PATCH_VERTICES", 0x8E72));
    items.push(const_item("GL_PATCH_DEFAULT_INNER_LEVEL", 0x8E73));
    items.push(const_item("GL_PATCH_DEFAULT_OUTER_LEVEL", 0x8E74));
    // Transform feedback
    items.push(const_item("GL_TRANSFORM_FEEDBACK", 0x8E22));
    items.push(const_item("GL_TRANSFORM_FEEDBACK_BUFFER", 0x8C8E));
    items.push(const_item("GL_INTERLEAVED_ATTRIBS", 0x8C8C));
    items.push(const_item("GL_SEPARATE_ATTRIBS", 0x8C8D));
    // Clip control
    items.push(const_item("GL_LOWER_LEFT", 0x8CA1));
    items.push(const_item("GL_UPPER_LEFT", 0x8CA2));
    items.push(const_item("GL_NEGATIVE_ONE_TO_ONE", 0x935E));
    items.push(const_item("GL_ZERO_TO_ONE", 0x935F));
    // Debug output
    items.push(const_item("GL_DEBUG_OUTPUT", 0x92E0));
    items.push(const_item("GL_DEBUG_OUTPUT_SYNCHRONOUS", 0x8242));
    items.push(const_item("GL_DEBUG_SOURCE_API", 0x8246));
    items.push(const_item("GL_DEBUG_SOURCE_WINDOW_SYSTEM", 0x8247));
    items.push(const_item("GL_DEBUG_SOURCE_SHADER_COMPILER", 0x8248));
    items.push(const_item("GL_DEBUG_TYPE_ERROR", 0x824C));
    items.push(const_item("GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR", 0x824D));
    items.push(const_item("GL_DEBUG_TYPE_PERFORMANCE", 0x8250));
    items.push(const_item("GL_DEBUG_SEVERITY_HIGH", 0x9146));
    items.push(const_item("GL_DEBUG_SEVERITY_MEDIUM", 0x9148));
    items.push(const_item("GL_DEBUG_SEVERITY_LOW", 0x9148));
    items.push(const_item("GL_DEBUG_SEVERITY_NOTIFICATION", 0x826B));
    // Multisampled texture
    items.push(const_item("GL_TEXTURE_2D_MULTISAMPLE", 0x9100));
    // SPIR-V
    items.push(const_item("GL_SHADER_BINARY_FORMAT_SPIR_V", 0x9551));
    // Anisotropic filtering
    items.push(const_item("GL_TEXTURE_MAX_ANISOTROPY", 0x84FE));
    items.push(const_item("GL_MAX_TEXTURE_MAX_ANISOTROPY", 0x84FF));
    // Program interface
    items.push(const_item("GL_SHADER_STORAGE_BLOCK", 0x92E6));
    // Pixel buffer objects
    items.push(const_item("GL_PIXEL_PACK_BUFFER", 0x88EB));
    items.push(const_item("GL_PIXEL_UNPACK_BUFFER", 0x88EC));
    // Sync objects
    items.push(const_item("GL_SYNC_GPU_COMMANDS_COMPLETE", 0x9117));
    items.push(const_item("GL_ALREADY_SIGNALED", 0x911A));
    items.push(const_item("GL_TIMEOUT_EXPIRED", 0x911B));
    items.push(const_item("GL_CONDITION_SATISFIED", 0x911C));
    items.push(const_item("GL_WAIT_FAILED", 0x911D));
    items.push(const_item("GL_SYNC_FLUSH_COMMANDS_BIT", 0x00000001));
    items.push(const_item("GL_TIMEOUT_IGNORED", -1));  // 0xFFFFFFFFFFFFFFFF
    // Pixel store parameters
    items.push(const_item("GL_PACK_ALIGNMENT", 0x0D05));
    items.push(const_item("GL_UNPACK_ALIGNMENT", 0x0CF5));
    items.push(const_item("GL_PACK_ROW_LENGTH", 0x0D02));
    items.push(const_item("GL_UNPACK_ROW_LENGTH", 0x0CF2));
    items.push(const_item("GL_PACK_SKIP_ROWS", 0x0D03));
    items.push(const_item("GL_PACK_SKIP_PIXELS", 0x0D04));
    items.push(const_item("GL_UNPACK_SKIP_ROWS", 0x0CF3));
    items.push(const_item("GL_UNPACK_SKIP_PIXELS", 0x0CF4));
    // Compressed texture formats
    items.push(const_item("GL_COMPRESSED_RGB_S3TC_DXT1", 0x83F0));
    items.push(const_item("GL_COMPRESSED_RGBA_S3TC_DXT1", 0x83F1));
    items.push(const_item("GL_COMPRESSED_RGBA_S3TC_DXT3", 0x83F2));
    items.push(const_item("GL_COMPRESSED_RGBA_S3TC_DXT5", 0x83F3));
    items.push(const_item("GL_COMPRESSED_RED_RGTC1", 0x8DBB));
    items.push(const_item("GL_COMPRESSED_RG_RGTC2", 0x8DBD));
    items.push(const_item("GL_COMPRESSED_RGBA_BPTC_UNORM", 0x8E8C));
    items.push(const_item("GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT", 0x8E8E));
    items.push(const_item("GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT", 0x8E8F));
    items.push(const_item("GL_COMPRESSED_RGB8_ETC2", 0x9274));
    items.push(const_item("GL_COMPRESSED_RGBA8_ETC2_EAC", 0x9278));
    items.push(const_item("GL_COMPRESSED_R11_EAC", 0x9270));
    items.push(const_item("GL_COMPRESSED_RG11_EAC", 0x9272));
    // Texture compression query
    items.push(const_item("GL_NUM_COMPRESSED_TEXTURE_FORMATS", 0x86A3));
    items.push(const_item("GL_COMPRESSED_TEXTURE_FORMATS", 0x86A4));
    // Program binary
    items.push(const_item("GL_PROGRAM_BINARY_LENGTH", 0x8741));
    items.push(const_item("GL_NUM_PROGRAM_BINARY_FORMATS", 0x87FE));
    items.push(const_item("GL_PROGRAM_BINARY_FORMATS", 0x87FF));
    items.push(const_item("GL_PROGRAM_BINARY_RETRIEVABLE_HINT", 0x8257));
    // Timer queries
    items.push(const_item("GL_TIMESTAMP", 0x8E28));
    items.push(const_item("GL_TIME_ELAPSED", 0x88BF));
    // Geometry shader (additional constants — base ones already in Tier 2 above)
    items.push(const_item("GL_MAX_GEOMETRY_OUTPUT_VERTICES", 0x8DE0));
    items.push(const_item("GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS", 0x8DE1));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── xtd.image — Image Loading (stb_image) ─────────────────────

fn gen_image_module() -> Module {
    // Image struct with user-visible fields
    let image_struct = Spanned::dummy(Item::Struct(StructDef {
        attributes: vec![],
        visibility: Visibility::Public,
        name: Spanned::dummy("Image".to_string()),
        generic_params: None,
        fields: vec![
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("width".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("height".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("channels".to_string()),
                type_: Spanned::dummy(ty_int()),
            }),
            Spanned::dummy(FieldDef {
                visibility: Visibility::Public,
                name: Spanned::dummy("data".to_string()),
                type_: Spanned::dummy(ty_vector_uint8()),
            }),
        ],
        doc_comment: None,
        span: Span::dummy(),
    }));

    let ty_image = || Type::Named {
        name: Spanned::dummy("Image".to_string()),
        generic_args: vec![],
    };

    let fn_item = |f: FunctionDef| -> Spanned<Item> {
        Spanned::dummy(Item::Function(f))
    };

    let mut items = vec![image_struct];
    {
        use crate::ir::abi::AbiKind::{CStr, Ptr, Scalar};
        items.push(fn_item(extern_fn_abi("image_load", &[("path", ty_str(), CStr)], ty_result(ty_image(), ty_str()), "gorget_image_load")));
        items.push(fn_item(extern_fn_abi("image_load_rgba", &[("path", ty_str(), CStr)], ty_result(ty_image(), ty_str()), "gorget_image_load_rgba")));
        items.push(fn_item(extern_fn("image_load_from_memory", &[("data", ty_vector_uint8())], ty_result(ty_image(), ty_str()), "gorget_image_load_from_memory")));
        items.push(fn_item(extern_fn("image_flip_vertically", &[("img", ty_image())], ty_image(), "gorget_image_flip_vertically")));

        // ── Enhanced Image Functions ─────────────────────────────
        // Query image info without full decode
        items.push(fn_item(extern_fn_abi("image_info", &[("path", ty_str(), CStr)], ty_result(ty_image(), ty_str()), "gorget_image_info")));
        items.push(fn_item(extern_fn("image_info_from_memory", &[("data", ty_vector_uint8())], ty_result(ty_image(), ty_str()), "gorget_image_info_from_memory")));
        // Load from memory with forced RGBA
        items.push(fn_item(extern_fn("image_load_rgba_from_memory", &[("data", ty_vector_uint8())], ty_result(ty_image(), ty_str()), "gorget_image_load_rgba_from_memory")));
        // Resize
        items.push(fn_item(extern_fn("image_resize", &[("img", ty_image()), ("new_width", ty_int()), ("new_height", ty_int())], ty_result(ty_image(), ty_str()), "gorget_image_resize")));
        // Write (stb_image_write — PNG and JPG)
        items.push(fn_item(extern_fn_abi("image_write_png", &[("path", ty_str(), CStr), ("img", ty_image(), Ptr)], ty_result(ty_int(), ty_str()), "gorget_image_write_png")));
        items.push(fn_item(extern_fn_abi("image_write_jpg", &[("path", ty_str(), CStr), ("img", ty_image(), Ptr), ("quality", ty_int(), Scalar)], ty_result(ty_int(), ty_str()), "gorget_image_write_jpg")));
        // Encode to memory
        items.push(fn_item(extern_fn("image_encode_png", &[("img", ty_image())], ty_result(ty_vector_uint8(), ty_str()), "gorget_image_encode_png")));
    }

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── gg.audio — Audio (SDL2_mixer) ────────────────────────────

fn gen_audio_module() -> Module {
    let mut items: Vec<Spanned<Item>> = Vec::new();

    // Opaque handle types
    items.push(opaque_struct("AudioChunk"));
    items.push(opaque_struct("AudioMusic"));

    let ty_chunk = || Type::Named {
        name: Spanned::dummy("AudioChunk".to_string()),
        generic_args: vec![],
    };
    let ty_music = || Type::Named {
        name: Spanned::dummy("AudioMusic".to_string()),
        generic_args: vec![],
    };

    let fn_item = |f: FunctionDef| -> Spanned<Item> {
        Spanned::dummy(Item::Function(f))
    };

    // Init/Quit
    items.push(fn_item(extern_fn("audio_init", &[("frequency", ty_int()), ("channels", ty_int()), ("chunk_size", ty_int())], ty_int(), "gorget_audio_init")));
    items.push(fn_item(extern_fn("audio_quit", &[], ty_void(), "gorget_audio_quit")));
    items.push(fn_item(extern_fn("audio_allocate_channels", &[("num_channels", ty_int())], ty_void(), "gorget_audio_allocate_channels")));

    // Sound effects
    items.push(fn_item({
        use crate::ir::abi::AbiKind::CStr;
        extern_fn_abi("audio_load_wav", &[("path", ty_str(), CStr)], ty_result(ty_chunk(), ty_str()), "gorget_audio_load_wav")
    }));
    items.push(fn_item(extern_fn("audio_free_chunk", &[("chunk", ty_chunk())], ty_void(), "gorget_audio_free_chunk")));
    items.push(fn_item(extern_fn("audio_play_channel", &[("channel", ty_int()), ("chunk", ty_chunk()), ("loops", ty_int())], ty_int(), "gorget_audio_play_channel")));
    items.push(fn_item(extern_fn("audio_halt_channel", &[("channel", ty_int())], ty_void(), "gorget_audio_halt_channel")));
    items.push(fn_item(extern_fn("audio_set_channel_volume", &[("channel", ty_int()), ("volume", ty_int())], ty_void(), "gorget_audio_set_channel_volume")));
    items.push(fn_item(extern_fn("audio_set_channel_position", &[("channel", ty_int()), ("angle", ty_int()), ("distance", ty_int())], ty_void(), "gorget_audio_set_channel_position")));
    items.push(fn_item(extern_fn("audio_set_channel_panning", &[("channel", ty_int()), ("left", ty_int()), ("right", ty_int())], ty_void(), "gorget_audio_set_channel_panning")));

    // Music
    items.push(fn_item({
        use crate::ir::abi::AbiKind::CStr;
        extern_fn_abi("audio_load_music", &[("path", ty_str(), CStr)], ty_result(ty_music(), ty_str()), "gorget_audio_load_music")
    }));
    items.push(fn_item(extern_fn("audio_free_music", &[("music", ty_music())], ty_void(), "gorget_audio_free_music")));
    items.push(fn_item(extern_fn("audio_play_music", &[("music", ty_music()), ("loops", ty_int())], ty_void(), "gorget_audio_play_music")));
    items.push(fn_item(extern_fn("audio_halt_music", &[], ty_void(), "gorget_audio_halt_music")));
    items.push(fn_item(extern_fn("audio_set_music_volume", &[("volume", ty_int())], ty_void(), "gorget_audio_set_music_volume")));
    items.push(fn_item(extern_fn("audio_pause_music", &[], ty_void(), "gorget_audio_pause_music")));
    items.push(fn_item(extern_fn("audio_resume_music", &[], ty_void(), "gorget_audio_resume_music")));

    // ── Enhanced Audio Functions ─────────────────────────────
    // Channel query
    items.push(fn_item(extern_fn("audio_channel_playing", &[("channel", ty_int())], ty_bool(), "gorget_audio_channel_playing")));
    items.push(fn_item(extern_fn("audio_channel_paused", &[("channel", ty_int())], ty_bool(), "gorget_audio_channel_paused")));
    items.push(fn_item(extern_fn("audio_pause_channel", &[("channel", ty_int())], ty_void(), "gorget_audio_pause_channel")));
    items.push(fn_item(extern_fn("audio_resume_channel", &[("channel", ty_int())], ty_void(), "gorget_audio_resume_channel")));
    items.push(fn_item(extern_fn("audio_playing_count", &[], ty_int(), "gorget_audio_playing_count")));
    items.push(fn_item(extern_fn("audio_paused_count", &[], ty_int(), "gorget_audio_paused_count")));
    // Fade in/out
    items.push(fn_item(extern_fn("audio_fade_in_channel", &[("channel", ty_int()), ("chunk", ty_chunk()), ("loops", ty_int()), ("ms", ty_int())], ty_int(), "gorget_audio_fade_in_channel")));
    items.push(fn_item(extern_fn("audio_fade_out_channel", &[("channel", ty_int()), ("ms", ty_int())], ty_void(), "gorget_audio_fade_out_channel")));
    items.push(fn_item(extern_fn("audio_fade_in_music", &[("music", ty_music()), ("loops", ty_int()), ("ms", ty_int())], ty_void(), "gorget_audio_fade_in_music")));
    items.push(fn_item(extern_fn("audio_fade_out_music", &[("ms", ty_int())], ty_void(), "gorget_audio_fade_out_music")));
    // Music state
    items.push(fn_item(extern_fn("audio_music_playing", &[], ty_bool(), "gorget_audio_music_playing")));
    items.push(fn_item(extern_fn("audio_music_paused", &[], ty_bool(), "gorget_audio_music_paused")));
    items.push(fn_item(extern_fn("audio_set_music_position", &[("position", ty_float())], ty_void(), "gorget_audio_set_music_position")));
    // Channel expiration
    items.push(fn_item(extern_fn("audio_expire_channel", &[("channel", ty_int()), ("ms", ty_int())], ty_void(), "gorget_audio_expire_channel")));
    // Master volume
    items.push(fn_item(extern_fn("audio_get_music_volume", &[], ty_int(), "gorget_audio_get_music_volume")));
    items.push(fn_item(extern_fn("audio_get_channel_volume", &[("channel", ty_int())], ty_int(), "gorget_audio_get_channel_volume")));
    // Channel distance (without angle)
    items.push(fn_item(extern_fn("audio_set_channel_distance", &[("channel", ty_int()), ("distance", ty_int())], ty_void(), "gorget_audio_set_channel_distance")));
    // Sound effects from memory
    items.push(fn_item(extern_fn("audio_load_wav_from_memory", &[("data", ty_vector_uint8())], ty_result(ty_chunk(), ty_str()), "gorget_audio_load_wav_from_memory")));

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── gg.compress — Zlib/Deflate (miniz) ──────────────────────

fn gen_compress_module() -> Module {
    let fn_item = |f: FunctionDef| -> Spanned<Item> {
        Spanned::dummy(Item::Function(f))
    };

    let items = vec![
        fn_item(extern_fn("zlib_decompress", &[("data", ty_vector_uint8()), ("uncompressed_size", ty_int())], ty_result(ty_vector_uint8(), ty_str()), "gorget_zlib_decompress")),
        fn_item(extern_fn("zlib_compress", &[("data", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str()), "gorget_zlib_compress")),
        // Compress with level (0=none, 1=fastest, 9=best)
        fn_item(extern_fn("zlib_compress_level", &[("data", ty_vector_uint8()), ("level", ty_int())], ty_result(ty_vector_uint8(), ty_str()), "gorget_zlib_compress_level")),
        // Raw deflate (no zlib header — for custom formats)
        fn_item(extern_fn("deflate_compress", &[("data", ty_vector_uint8())], ty_result(ty_vector_uint8(), ty_str()), "gorget_deflate_compress")),
        fn_item(extern_fn("deflate_decompress", &[("data", ty_vector_uint8()), ("uncompressed_size", ty_int())], ty_result(ty_vector_uint8(), ty_str()), "gorget_deflate_decompress")),
        // CRC32 (useful for ZIP/PK3 verification)
        fn_item(extern_fn("crc32_compute", &[("data", ty_vector_uint8())], ty_int(), "gorget_crc32_compute")),
    ];

    Module {
        items,
        span: Span::dummy(),
    }
}

// ─── xtd.metal — Apple Metal GPU API ────────────────────────────

fn gen_metal_module() -> Module {
    let mut items: Vec<Spanned<Item>> = Vec::new();

    let fn_item = |f: FunctionDef| -> Spanned<Item> {
        Spanned::dummy(Item::Function(f))
    };

    let const_item = |name: &str, value: i64| -> Spanned<Item> {
        Spanned::dummy(Item::ConstDecl(ConstDecl {
            visibility: Visibility::Public,
            type_: Spanned::dummy(ty_int()),
            name: Spanned::dummy(name.to_string()),
            value: Spanned::dummy(Expr::IntLiteral(value)),
            span: Span::dummy(),
        }))
    };

    // ── Pixel Formats ──────────────────────────────────────
    items.push(const_item("MTL_PIXEL_FORMAT_BGRA8_UNORM", 80));
    items.push(const_item("MTL_PIXEL_FORMAT_RGBA8_UNORM", 70));
    items.push(const_item("MTL_PIXEL_FORMAT_RGBA16_FLOAT", 115));
    items.push(const_item("MTL_PIXEL_FORMAT_R8_UNORM", 10));
    items.push(const_item("MTL_PIXEL_FORMAT_RG8_UNORM", 30));
    items.push(const_item("MTL_PIXEL_FORMAT_DEPTH32_FLOAT", 252));
    items.push(const_item("MTL_PIXEL_FORMAT_DEPTH32_FLOAT_STENCIL8", 260));
    items.push(const_item("MTL_PIXEL_FORMAT_STENCIL8", 253));

    // ── Primitive Types ────────────────────────────────────
    items.push(const_item("MTL_PRIMITIVE_POINT", 0));
    items.push(const_item("MTL_PRIMITIVE_LINE", 1));
    items.push(const_item("MTL_PRIMITIVE_LINE_STRIP", 2));
    items.push(const_item("MTL_PRIMITIVE_TRIANGLE", 3));
    items.push(const_item("MTL_PRIMITIVE_TRIANGLE_STRIP", 4));

    // ── Index Types ────────────────────────────────────────
    items.push(const_item("MTL_INDEX_UINT16", 0));
    items.push(const_item("MTL_INDEX_UINT32", 1));

    // ── Load/Store Actions ─────────────────────────────────
    items.push(const_item("MTL_LOAD_ACTION_DONT_CARE", 0));
    items.push(const_item("MTL_LOAD_ACTION_LOAD", 1));
    items.push(const_item("MTL_LOAD_ACTION_CLEAR", 2));
    items.push(const_item("MTL_STORE_ACTION_DONT_CARE", 0));
    items.push(const_item("MTL_STORE_ACTION_STORE", 1));

    // ── Compare Functions ──────────────────────────────────
    items.push(const_item("MTL_COMPARE_NEVER", 0));
    items.push(const_item("MTL_COMPARE_LESS", 1));
    items.push(const_item("MTL_COMPARE_EQUAL", 2));
    items.push(const_item("MTL_COMPARE_LESS_EQUAL", 3));
    items.push(const_item("MTL_COMPARE_GREATER", 4));
    items.push(const_item("MTL_COMPARE_NOT_EQUAL", 5));
    items.push(const_item("MTL_COMPARE_GREATER_EQUAL", 6));
    items.push(const_item("MTL_COMPARE_ALWAYS", 7));

    // ── Cull Modes ─────────────────────────────────────────
    items.push(const_item("MTL_CULL_NONE", 0));
    items.push(const_item("MTL_CULL_FRONT", 1));
    items.push(const_item("MTL_CULL_BACK", 2));

    // ── Winding Order ──────────────────────────────────────
    items.push(const_item("MTL_WINDING_CLOCKWISE", 0));
    items.push(const_item("MTL_WINDING_COUNTER_CLOCKWISE", 1));

    // ── Triangle Fill Mode ─────────────────────────────────
    items.push(const_item("MTL_FILL_MODE_FILL", 0));
    items.push(const_item("MTL_FILL_MODE_LINES", 1));

    // ── Blend Factors ──────────────────────────────────────
    items.push(const_item("MTL_BLEND_ZERO", 0));
    items.push(const_item("MTL_BLEND_ONE", 1));
    items.push(const_item("MTL_BLEND_SRC_COLOR", 2));
    items.push(const_item("MTL_BLEND_ONE_MINUS_SRC_COLOR", 3));
    items.push(const_item("MTL_BLEND_SRC_ALPHA", 4));
    items.push(const_item("MTL_BLEND_ONE_MINUS_SRC_ALPHA", 5));
    items.push(const_item("MTL_BLEND_DST_COLOR", 6));
    items.push(const_item("MTL_BLEND_ONE_MINUS_DST_COLOR", 7));
    items.push(const_item("MTL_BLEND_DST_ALPHA", 8));
    items.push(const_item("MTL_BLEND_ONE_MINUS_DST_ALPHA", 9));

    // ── Blend Operations ───────────────────────────────────
    items.push(const_item("MTL_BLEND_OP_ADD", 0));
    items.push(const_item("MTL_BLEND_OP_SUBTRACT", 1));
    items.push(const_item("MTL_BLEND_OP_REVERSE_SUBTRACT", 2));
    items.push(const_item("MTL_BLEND_OP_MIN", 3));
    items.push(const_item("MTL_BLEND_OP_MAX", 4));

    // ── Sampler Address Modes ──────────────────────────────
    items.push(const_item("MTL_ADDRESS_CLAMP_TO_EDGE", 0));
    items.push(const_item("MTL_ADDRESS_MIRROR_CLAMP_TO_EDGE", 1));
    items.push(const_item("MTL_ADDRESS_REPEAT", 2));
    items.push(const_item("MTL_ADDRESS_MIRROR_REPEAT", 3));
    items.push(const_item("MTL_ADDRESS_CLAMP_TO_ZERO", 4));

    // ── Sampler Min/Mag Filter ─────────────────────────────
    items.push(const_item("MTL_FILTER_NEAREST", 0));
    items.push(const_item("MTL_FILTER_LINEAR", 1));

    // ── Sampler Mip Filter ─────────────────────────────────
    items.push(const_item("MTL_MIP_FILTER_NOT_MIPMAPPED", 0));
    items.push(const_item("MTL_MIP_FILTER_NEAREST", 1));
    items.push(const_item("MTL_MIP_FILTER_LINEAR", 2));

    // ── Storage Mode ───────────────────────────────────────
    items.push(const_item("MTL_STORAGE_MODE_SHARED", 0));
    items.push(const_item("MTL_STORAGE_MODE_MANAGED", 1));
    items.push(const_item("MTL_STORAGE_MODE_PRIVATE", 2));

    // ── Texture Usage ──────────────────────────────────────
    items.push(const_item("MTL_TEXTURE_USAGE_SHADER_READ", 1));
    items.push(const_item("MTL_TEXTURE_USAGE_SHADER_WRITE", 2));
    items.push(const_item("MTL_TEXTURE_USAGE_RENDER_TARGET", 4));

    // ── Vertex Format ──────────────────────────────────────
    items.push(const_item("MTL_VERTEX_FORMAT_FLOAT", 28));
    items.push(const_item("MTL_VERTEX_FORMAT_FLOAT2", 29));
    items.push(const_item("MTL_VERTEX_FORMAT_FLOAT3", 30));
    items.push(const_item("MTL_VERTEX_FORMAT_FLOAT4", 31));
    items.push(const_item("MTL_VERTEX_FORMAT_UCHAR4", 3));
    items.push(const_item("MTL_VERTEX_FORMAT_SHORT2", 16));
    items.push(const_item("MTL_VERTEX_FORMAT_SHORT4", 18));

    // ── Vertex Step Function ───────────────────────────────
    items.push(const_item("MTL_STEP_PER_VERTEX", 1));
    items.push(const_item("MTL_STEP_PER_INSTANCE", 2));

    // ════════════════════════════════════════════════════════
    // Functions — all handles are opaque int (pointer-sized)
    // ════════════════════════════════════════════════════════

    // ── Device ─────────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_device", &[], ty_int(), "gorget_metal_create_device")));
    items.push(fn_item(extern_fn("metal_device_name", &[("device", ty_int())], ty_string(), "gorget_metal_device_name")));
    items.push(fn_item(extern_fn("metal_device_supports_family", &[("device", ty_int()), ("family", ty_int())], ty_bool(), "gorget_metal_device_supports_family")));

    // ── Command Queue ──────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_command_queue", &[("device", ty_int())], ty_int(), "gorget_metal_create_command_queue")));

    // ── Command Buffer ─────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_command_buffer", &[("queue", ty_int())], ty_int(), "gorget_metal_create_command_buffer")));
    items.push(fn_item(extern_fn("metal_command_buffer_present", &[("cmd_buf", ty_int()), ("drawable", ty_int())], ty_void(), "gorget_metal_command_buffer_present")));
    items.push(fn_item(extern_fn("metal_command_buffer_commit", &[("cmd_buf", ty_int())], ty_void(), "gorget_metal_command_buffer_commit")));
    items.push(fn_item(extern_fn("metal_command_buffer_wait", &[("cmd_buf", ty_int())], ty_void(), "gorget_metal_command_buffer_wait")));

    // ── Buffers ────────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_buffer", &[("device", ty_int()), ("length", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_create_buffer")));
    items.push(fn_item(extern_fn("metal_create_buffer_with_data", &[("device", ty_int()), ("data", ty_vector_uint8()), ("length", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_create_buffer_with_data")));
    items.push(fn_item(extern_fn("metal_buffer_contents", &[("buffer", ty_int())], ty_int(), "gorget_metal_buffer_contents")));
    items.push(fn_item(extern_fn("metal_buffer_length", &[("buffer", ty_int())], ty_int(), "gorget_metal_buffer_length")));
    items.push(fn_item(extern_fn("metal_buffer_did_modify_range", &[("buffer", ty_int()), ("offset", ty_int()), ("length", ty_int())], ty_void(), "gorget_metal_buffer_did_modify_range")));

    // ── Textures ───────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_texture_2d", &[("device", ty_int()), ("width", ty_int()), ("height", ty_int()), ("format", ty_int()), ("usage", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_create_texture_2d")));
    items.push(fn_item(extern_fn("metal_create_texture_2d_mipmapped", &[("device", ty_int()), ("width", ty_int()), ("height", ty_int()), ("format", ty_int()), ("mip_levels", ty_int()), ("usage", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_create_texture_2d_mipmapped")));
    items.push(fn_item(extern_fn("metal_texture_upload", &[("texture", ty_int()), ("x", ty_int()), ("y", ty_int()), ("width", ty_int()), ("height", ty_int()), ("data", ty_vector_uint8()), ("bytes_per_row", ty_int())], ty_void(), "gorget_metal_texture_upload")));
    items.push(fn_item(extern_fn("metal_texture_upload_mip", &[("texture", ty_int()), ("mip_level", ty_int()), ("x", ty_int()), ("y", ty_int()), ("width", ty_int()), ("height", ty_int()), ("data", ty_vector_uint8()), ("bytes_per_row", ty_int())], ty_void(), "gorget_metal_texture_upload_mip")));
    items.push(fn_item(extern_fn("metal_texture_width", &[("texture", ty_int())], ty_int(), "gorget_metal_texture_width")));
    items.push(fn_item(extern_fn("metal_texture_height", &[("texture", ty_int())], ty_int(), "gorget_metal_texture_height")));

    // ── Samplers ───────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_sampler", &[("device", ty_int()), ("min_filter", ty_int()), ("mag_filter", ty_int()), ("mip_filter", ty_int()), ("address_s", ty_int()), ("address_t", ty_int())], ty_int(), "gorget_metal_create_sampler")));

    // ── Shaders / Library ──────────────────────────────────
    {
        use crate::ir::abi::AbiKind::{CStr, Scalar as S};
        items.push(fn_item(extern_fn_abi("metal_create_library", &[("device", ty_int(), S), ("source", ty_str(), CStr)], ty_int(), "gorget_metal_create_library")));
        items.push(fn_item(extern_fn("metal_create_library_from_data", &[("device", ty_int()), ("data", ty_vector_uint8())], ty_int(), "gorget_metal_create_library_from_data")));
        items.push(fn_item(extern_fn_abi("metal_library_function", &[("library", ty_int(), S), ("name", ty_str(), CStr)], ty_int(), "gorget_metal_library_function")));
    }

    // ── Vertex Descriptor ──────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_vertex_descriptor", &[], ty_int(), "gorget_metal_create_vertex_descriptor")));
    items.push(fn_item(extern_fn("metal_vertex_desc_set_attribute", &[("desc", ty_int()), ("index", ty_int()), ("format", ty_int()), ("offset", ty_int()), ("buffer_index", ty_int())], ty_void(), "gorget_metal_vertex_desc_set_attribute")));
    items.push(fn_item(extern_fn("metal_vertex_desc_set_layout", &[("desc", ty_int()), ("index", ty_int()), ("stride", ty_int()), ("step_function", ty_int())], ty_void(), "gorget_metal_vertex_desc_set_layout")));

    // ── Render Pipeline ────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_render_pipeline", &[("device", ty_int()), ("vertex_fn", ty_int()), ("fragment_fn", ty_int()), ("vertex_desc", ty_int()), ("color_format", ty_int()), ("depth_format", ty_int())], ty_int(), "gorget_metal_create_render_pipeline")));
    items.push(fn_item(extern_fn("metal_create_render_pipeline_blended", &[("device", ty_int()), ("vertex_fn", ty_int()), ("fragment_fn", ty_int()), ("vertex_desc", ty_int()), ("color_format", ty_int()), ("depth_format", ty_int()), ("blend_src_rgb", ty_int()), ("blend_dst_rgb", ty_int()), ("blend_src_a", ty_int()), ("blend_dst_a", ty_int())], ty_int(), "gorget_metal_create_render_pipeline_blended")));

    // ── Depth/Stencil State ────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_depth_stencil", &[("device", ty_int()), ("compare_fn", ty_int()), ("depth_write", ty_int())], ty_int(), "gorget_metal_create_depth_stencil")));

    // ── Render Pass Descriptor ─────────────────────────────
    items.push(fn_item(extern_fn("metal_create_render_pass_desc", &[], ty_int(), "gorget_metal_create_render_pass_desc")));
    items.push(fn_item(extern_fn("metal_render_pass_set_color", &[("desc", ty_int()), ("index", ty_int()), ("texture", ty_int()), ("load_action", ty_int()), ("store_action", ty_int()), ("r", ty_float()), ("g", ty_float()), ("b", ty_float()), ("a", ty_float())], ty_void(), "gorget_metal_render_pass_set_color")));
    items.push(fn_item(extern_fn("metal_render_pass_set_depth", &[("desc", ty_int()), ("texture", ty_int()), ("load_action", ty_int()), ("store_action", ty_int()), ("clear_depth", ty_float())], ty_void(), "gorget_metal_render_pass_set_depth")));

    // ── Render Command Encoder ─────────────────────────────
    items.push(fn_item(extern_fn("metal_create_render_encoder", &[("cmd_buf", ty_int()), ("pass_desc", ty_int())], ty_int(), "gorget_metal_create_render_encoder")));
    items.push(fn_item(extern_fn("metal_encoder_set_pipeline", &[("encoder", ty_int()), ("pipeline", ty_int())], ty_void(), "gorget_metal_encoder_set_pipeline")));
    items.push(fn_item(extern_fn("metal_encoder_set_vertex_buffer", &[("encoder", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_vertex_buffer")));
    items.push(fn_item(extern_fn("metal_encoder_set_vertex_bytes", &[("encoder", ty_int()), ("data", ty_vector_uint8()), ("length", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_vertex_bytes")));
    items.push(fn_item(extern_fn("metal_encoder_set_fragment_buffer", &[("encoder", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_fragment_buffer")));
    items.push(fn_item(extern_fn("metal_encoder_set_fragment_bytes", &[("encoder", ty_int()), ("data", ty_vector_uint8()), ("length", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_fragment_bytes")));
    items.push(fn_item(extern_fn("metal_encoder_set_fragment_texture", &[("encoder", ty_int()), ("texture", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_fragment_texture")));
    items.push(fn_item(extern_fn("metal_encoder_set_fragment_sampler", &[("encoder", ty_int()), ("sampler", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_fragment_sampler")));
    items.push(fn_item(extern_fn("metal_encoder_set_depth_stencil", &[("encoder", ty_int()), ("state", ty_int())], ty_void(), "gorget_metal_encoder_set_depth_stencil")));
    items.push(fn_item(extern_fn("metal_encoder_set_cull_mode", &[("encoder", ty_int()), ("mode", ty_int())], ty_void(), "gorget_metal_encoder_set_cull_mode")));
    items.push(fn_item(extern_fn("metal_encoder_set_front_facing", &[("encoder", ty_int()), ("winding", ty_int())], ty_void(), "gorget_metal_encoder_set_front_facing")));
    items.push(fn_item(extern_fn("metal_encoder_set_fill_mode", &[("encoder", ty_int()), ("mode", ty_int())], ty_void(), "gorget_metal_encoder_set_fill_mode")));
    items.push(fn_item(extern_fn("metal_encoder_set_viewport", &[("encoder", ty_int()), ("x", ty_float()), ("y", ty_float()), ("w", ty_float()), ("h", ty_float()), ("near", ty_float()), ("far", ty_float())], ty_void(), "gorget_metal_encoder_set_viewport")));
    items.push(fn_item(extern_fn("metal_encoder_set_scissor", &[("encoder", ty_int()), ("x", ty_int()), ("y", ty_int()), ("w", ty_int()), ("h", ty_int())], ty_void(), "gorget_metal_encoder_set_scissor")));
    items.push(fn_item(extern_fn("metal_encoder_set_blend_color", &[("encoder", ty_int()), ("r", ty_float()), ("g", ty_float()), ("b", ty_float()), ("a", ty_float())], ty_void(), "gorget_metal_encoder_set_blend_color")));
    items.push(fn_item(extern_fn("metal_encoder_set_stencil_ref", &[("encoder", ty_int()), ("ref_val", ty_int())], ty_void(), "gorget_metal_encoder_set_stencil_ref")));
    items.push(fn_item(extern_fn("metal_encoder_draw_primitives", &[("encoder", ty_int()), ("primitive_type", ty_int()), ("start", ty_int()), ("count", ty_int())], ty_void(), "gorget_metal_encoder_draw_primitives")));
    items.push(fn_item(extern_fn("metal_encoder_draw_primitives_instanced", &[("encoder", ty_int()), ("primitive_type", ty_int()), ("start", ty_int()), ("count", ty_int()), ("instance_count", ty_int())], ty_void(), "gorget_metal_encoder_draw_primitives_instanced")));
    items.push(fn_item(extern_fn("metal_encoder_draw_indexed", &[("encoder", ty_int()), ("primitive_type", ty_int()), ("index_count", ty_int()), ("index_type", ty_int()), ("index_buffer", ty_int()), ("index_offset", ty_int())], ty_void(), "gorget_metal_encoder_draw_indexed")));
    items.push(fn_item(extern_fn("metal_encoder_draw_indexed_instanced", &[("encoder", ty_int()), ("primitive_type", ty_int()), ("index_count", ty_int()), ("index_type", ty_int()), ("index_buffer", ty_int()), ("index_offset", ty_int()), ("instance_count", ty_int())], ty_void(), "gorget_metal_encoder_draw_indexed_instanced")));
    items.push(fn_item(extern_fn("metal_encoder_end", &[("encoder", ty_int())], ty_void(), "gorget_metal_encoder_end")));

    // ── Blit Command Encoder (for mipmap generation, copies) ──
    items.push(fn_item(extern_fn("metal_create_blit_encoder", &[("cmd_buf", ty_int())], ty_int(), "gorget_metal_create_blit_encoder")));
    items.push(fn_item(extern_fn("metal_blit_generate_mipmaps", &[("encoder", ty_int()), ("texture", ty_int())], ty_void(), "gorget_metal_blit_generate_mipmaps")));
    items.push(fn_item(extern_fn("metal_blit_end", &[("encoder", ty_int())], ty_void(), "gorget_metal_blit_end")));
    // Alias for gpu.gg compatibility
    items.push(fn_item(extern_fn("metal_generate_mipmaps", &[("cmd_buf", ty_int()), ("texture", ty_int())], ty_void(), "gorget_metal_generate_mipmaps")));

    // ── SDL Metal Integration ──────────────────────────────
    items.push(fn_item(extern_fn("sdl_metal_create_view", &[("window", ty_int())], ty_int(), "gorget_sdl_metal_create_view")));
    items.push(fn_item(extern_fn("sdl_metal_get_layer", &[("view", ty_int())], ty_int(), "gorget_sdl_metal_get_layer")));
    items.push(fn_item(extern_fn("metal_layer_set_device", &[("layer", ty_int()), ("device", ty_int())], ty_void(), "gorget_metal_layer_set_device")));
    items.push(fn_item(extern_fn("metal_layer_set_pixel_format", &[("layer", ty_int()), ("format", ty_int())], ty_void(), "gorget_metal_layer_set_pixel_format")));
    items.push(fn_item(extern_fn("metal_layer_set_drawable_size", &[("layer", ty_int()), ("width", ty_int()), ("height", ty_int())], ty_void(), "gorget_metal_layer_set_drawable_size")));
    items.push(fn_item(extern_fn("metal_layer_next_drawable", &[("layer", ty_int())], ty_int(), "gorget_metal_layer_next_drawable")));
    items.push(fn_item(extern_fn("metal_drawable_texture", &[("drawable", ty_int())], ty_int(), "gorget_metal_drawable_texture")));

    // ── Release ────────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_release", &[("obj", ty_int())], ty_void(), "gorget_metal_release")));

    // ── Convenience: begin_frame wraps drawable+pass creation ──
    items.push(fn_item(extern_fn("metal_begin_frame", &[("layer", ty_int())], ty_int(), "gorget_metal_begin_frame")));

    // ── Autorelease Pool (per-frame memory management) ───
    items.push(fn_item(extern_fn("metal_autorelease_pool_push", &[], ty_int(), "gorget_metal_autorelease_pool_push")));
    items.push(fn_item(extern_fn("metal_autorelease_pool_pop", &[("pool", ty_int())], ty_void(), "gorget_metal_autorelease_pool_pop")));

    // ── Triple Buffering (dispatch semaphore) ────────────
    items.push(fn_item(extern_fn("metal_semaphore_create", &[("value", ty_int())], ty_int(), "gorget_metal_semaphore_create")));
    items.push(fn_item(extern_fn("metal_semaphore_wait", &[("sem", ty_int())], ty_void(), "gorget_metal_semaphore_wait")));
    items.push(fn_item(extern_fn("metal_semaphore_signal", &[("sem", ty_int())], ty_void(), "gorget_metal_semaphore_signal")));
    items.push(fn_item(extern_fn("metal_command_buffer_on_complete", &[("cmd_buf", ty_int()), ("sem", ty_int())], ty_void(), "gorget_metal_command_buffer_on_complete")));

    // ── Stencil Attachment ────────────────────────────────
    items.push(fn_item(extern_fn("metal_render_pass_set_stencil", &[("desc", ty_int()), ("texture", ty_int()), ("load_action", ty_int()), ("store_action", ty_int()), ("clear_stencil", ty_int())], ty_void(), "gorget_metal_render_pass_set_stencil")));
    items.push(fn_item(extern_fn("metal_create_render_pipeline_with_stencil", &[("device", ty_int()), ("vertex_fn", ty_int()), ("fragment_fn", ty_int()), ("vertex_desc", ty_int()), ("color_format", ty_int()), ("depth_format", ty_int()), ("stencil_format", ty_int())], ty_int(), "gorget_metal_create_render_pipeline_with_stencil")));

    // ── Depth Bias (shadow acne prevention) ──────────────
    items.push(fn_item(extern_fn("metal_encoder_set_depth_bias", &[("encoder", ty_int()), ("depth_bias", ty_float()), ("slope_scale", ty_float()), ("clamp", ty_float())], ty_void(), "gorget_metal_encoder_set_depth_bias")));

    // ── Vertex-stage Texture/Sampler ─────────────────────
    items.push(fn_item(extern_fn("metal_encoder_set_vertex_texture", &[("encoder", ty_int()), ("texture", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_vertex_texture")));
    items.push(fn_item(extern_fn("metal_encoder_set_vertex_sampler", &[("encoder", ty_int()), ("sampler", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_encoder_set_vertex_sampler")));

    // ── Command Buffer Status ────────────────────────────
    items.push(fn_item(extern_fn("metal_command_buffer_status", &[("cmd_buf", ty_int())], ty_int(), "gorget_metal_command_buffer_status")));
    items.push(fn_item(extern_fn("metal_command_buffer_error", &[("cmd_buf", ty_int())], ty_string(), "gorget_metal_command_buffer_error")));

    // ── MSAA Support ─────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_render_pipeline_msaa", &[("device", ty_int()), ("vertex_fn", ty_int()), ("fragment_fn", ty_int()), ("vertex_desc", ty_int()), ("color_format", ty_int()), ("depth_format", ty_int()), ("sample_count", ty_int())], ty_int(), "gorget_metal_create_render_pipeline_msaa")));
    items.push(fn_item(extern_fn("metal_create_texture_2d_msaa", &[("device", ty_int()), ("width", ty_int()), ("height", ty_int()), ("format", ty_int()), ("sample_count", ty_int()), ("usage", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_create_texture_2d_msaa")));

    // ── Compute Pipeline ────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_compute_pipeline", &[("device", ty_int()), ("function_handle", ty_int())], ty_int(), "gorget_metal_create_compute_pipeline")));
    items.push(fn_item(extern_fn("metal_create_compute_encoder", &[("cmd_buf", ty_int())], ty_int(), "gorget_metal_create_compute_encoder")));
    items.push(fn_item(extern_fn("metal_compute_set_pipeline", &[("encoder", ty_int()), ("pipeline", ty_int())], ty_void(), "gorget_metal_compute_set_pipeline")));
    items.push(fn_item(extern_fn("metal_compute_set_buffer", &[("encoder", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_compute_set_buffer")));
    items.push(fn_item(extern_fn("metal_compute_set_bytes", &[("encoder", ty_int()), ("data", ty_vector_uint8()), ("index", ty_int())], ty_void(), "gorget_metal_compute_set_bytes")));
    items.push(fn_item(extern_fn("metal_compute_set_texture", &[("encoder", ty_int()), ("texture", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_compute_set_texture")));
    items.push(fn_item(extern_fn("metal_compute_dispatch_threadgroups", &[("encoder", ty_int()), ("gx", ty_int()), ("gy", ty_int()), ("gz", ty_int()), ("tx", ty_int()), ("ty_", ty_int()), ("tz", ty_int())], ty_void(), "gorget_metal_compute_dispatch_threadgroups")));
    items.push(fn_item(extern_fn("metal_compute_dispatch_threads", &[("encoder", ty_int()), ("gx", ty_int()), ("gy", ty_int()), ("gz", ty_int()), ("tx", ty_int()), ("ty_", ty_int()), ("tz", ty_int())], ty_void(), "gorget_metal_compute_dispatch_threads")));
    items.push(fn_item(extern_fn("metal_compute_end", &[("encoder", ty_int())], ty_void(), "gorget_metal_compute_end")));
    items.push(fn_item(extern_fn("metal_compute_thread_execution_width", &[("pipeline", ty_int())], ty_int(), "gorget_metal_compute_thread_execution_width")));
    items.push(fn_item(extern_fn("metal_compute_max_threads_per_threadgroup", &[("pipeline", ty_int())], ty_int(), "gorget_metal_compute_max_threads_per_threadgroup")));
    items.push(fn_item(extern_fn("metal_compute_set_sampler", &[("encoder", ty_int()), ("sampler", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_compute_set_sampler")));

    // ── GPU Synchronization (Events + Fences) ───────────────
    items.push(fn_item(extern_fn("metal_create_event", &[("device", ty_int())], ty_int(), "gorget_metal_create_event")));
    items.push(fn_item(extern_fn("metal_command_buffer_encode_signal_event", &[("cmd_buf", ty_int()), ("event", ty_int()), ("value", ty_int())], ty_void(), "gorget_metal_command_buffer_encode_signal_event")));
    items.push(fn_item(extern_fn("metal_command_buffer_encode_wait_event", &[("cmd_buf", ty_int()), ("event", ty_int()), ("value", ty_int())], ty_void(), "gorget_metal_command_buffer_encode_wait_event")));
    items.push(fn_item(extern_fn("metal_create_shared_event", &[("device", ty_int())], ty_int(), "gorget_metal_create_shared_event")));
    items.push(fn_item(extern_fn("metal_shared_event_signaled_value", &[("event", ty_int())], ty_int(), "gorget_metal_shared_event_signaled_value")));
    items.push(fn_item(extern_fn("metal_create_fence", &[("device", ty_int())], ty_int(), "gorget_metal_create_fence")));
    items.push(fn_item(extern_fn("metal_encoder_wait_for_fence", &[("encoder", ty_int()), ("fence", ty_int())], ty_void(), "gorget_metal_encoder_wait_for_fence")));
    items.push(fn_item(extern_fn("metal_encoder_update_fence", &[("encoder", ty_int()), ("fence", ty_int())], ty_void(), "gorget_metal_encoder_update_fence")));
    items.push(fn_item(extern_fn("metal_encoder_memory_barrier", &[("encoder", ty_int()), ("scope", ty_int())], ty_void(), "gorget_metal_encoder_memory_barrier")));

    // ── Blit Encoder Operations ─────────────────────────────
    items.push(fn_item(extern_fn("metal_blit_copy_buffer_to_buffer", &[("encoder", ty_int()), ("src", ty_int()), ("src_offset", ty_int()), ("dst", ty_int()), ("dst_offset", ty_int()), ("size", ty_int())], ty_void(), "gorget_metal_blit_copy_buffer_to_buffer")));
    items.push(fn_item(extern_fn("metal_blit_copy_texture_to_texture", &[("encoder", ty_int()), ("src", ty_int()), ("src_slice", ty_int()), ("src_level", ty_int()), ("dst", ty_int()), ("dst_slice", ty_int()), ("dst_level", ty_int()), ("sx", ty_int()), ("sy", ty_int()), ("sw", ty_int()), ("sh", ty_int())], ty_void(), "gorget_metal_blit_copy_texture_to_texture")));
    items.push(fn_item(extern_fn("metal_blit_copy_buffer_to_texture", &[("encoder", ty_int()), ("buffer", ty_int()), ("buf_offset", ty_int()), ("bytes_per_row", ty_int()), ("bytes_per_image", ty_int()), ("texture", ty_int()), ("slice", ty_int()), ("level", ty_int()), ("w", ty_int()), ("h", ty_int()), ("d", ty_int())], ty_void(), "gorget_metal_blit_copy_buffer_to_texture")));
    items.push(fn_item(extern_fn("metal_blit_copy_texture_to_buffer", &[("encoder", ty_int()), ("texture", ty_int()), ("slice", ty_int()), ("level", ty_int()), ("buffer", ty_int()), ("buf_offset", ty_int()), ("bytes_per_row", ty_int()), ("bytes_per_image", ty_int()), ("w", ty_int()), ("h", ty_int()), ("d", ty_int())], ty_void(), "gorget_metal_blit_copy_texture_to_buffer")));
    items.push(fn_item(extern_fn("metal_blit_fill_buffer", &[("encoder", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("length", ty_int()), ("value", ty_int())], ty_void(), "gorget_metal_blit_fill_buffer")));
    items.push(fn_item(extern_fn("metal_blit_synchronize_resource", &[("encoder", ty_int()), ("resource", ty_int())], ty_void(), "gorget_metal_blit_synchronize_resource")));

    // ── Indirect Drawing ────────────────────────────────────
    items.push(fn_item(extern_fn("metal_encoder_draw_primitives_indirect", &[("encoder", ty_int()), ("prim_type", ty_int()), ("indirect_buffer", ty_int()), ("offset", ty_int())], ty_void(), "gorget_metal_encoder_draw_primitives_indirect")));
    items.push(fn_item(extern_fn("metal_encoder_draw_indexed_indirect", &[("encoder", ty_int()), ("prim_type", ty_int()), ("index_count", ty_int()), ("index_type", ty_int()), ("index_buffer", ty_int()), ("index_offset", ty_int()), ("indirect_buffer", ty_int()), ("indirect_offset", ty_int())], ty_void(), "gorget_metal_encoder_draw_indexed_indirect")));

    // ── Additional Texture Types ────────────────────────────
    items.push(fn_item(extern_fn("metal_create_texture_cube", &[("device", ty_int()), ("format", ty_int()), ("size", ty_int()), ("mipmaps", ty_int()), ("usage", ty_int())], ty_int(), "gorget_metal_create_texture_cube")));
    items.push(fn_item(extern_fn("metal_create_texture_2d_array", &[("device", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("array_length", ty_int()), ("mipmaps", ty_int()), ("usage", ty_int())], ty_int(), "gorget_metal_create_texture_2d_array")));
    items.push(fn_item(extern_fn("metal_create_texture_3d", &[("device", ty_int()), ("format", ty_int()), ("width", ty_int()), ("height", ty_int()), ("depth", ty_int()), ("mipmaps", ty_int()), ("usage", ty_int())], ty_int(), "gorget_metal_create_texture_3d")));
    items.push(fn_item(extern_fn("metal_create_texture_view", &[("texture", ty_int()), ("format", ty_int()), ("tex_type", ty_int()), ("mip_start", ty_int()), ("mip_count", ty_int()), ("slice_start", ty_int()), ("slice_count", ty_int())], ty_int(), "gorget_metal_create_texture_view")));

    // ── Resource Heaps ──────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_heap", &[("device", ty_int()), ("size", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_create_heap")));
    items.push(fn_item(extern_fn("metal_heap_create_buffer", &[("heap", ty_int()), ("length", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_heap_create_buffer")));
    items.push(fn_item(extern_fn("metal_heap_create_texture", &[("heap", ty_int()), ("width", ty_int()), ("height", ty_int()), ("format", ty_int()), ("usage", ty_int()), ("storage_mode", ty_int())], ty_int(), "gorget_metal_heap_create_texture")));
    items.push(fn_item(extern_fn("metal_heap_used_size", &[("heap", ty_int())], ty_int(), "gorget_metal_heap_used_size")));
    items.push(fn_item(extern_fn("metal_heap_current_allocated_size", &[("heap", ty_int())], ty_int(), "gorget_metal_heap_current_allocated_size")));
    items.push(fn_item(extern_fn("metal_heap_max_available_size", &[("heap", ty_int()), ("alignment", ty_int())], ty_int(), "gorget_metal_heap_max_available_size")));

    // ── Argument Buffers (Bindless Resources) ───────────────────
    items.push(fn_item(extern_fn("metal_create_argument_encoder", &[("function_handle", ty_int()), ("buffer_index", ty_int())], ty_int(), "gorget_metal_create_argument_encoder")));
    items.push(fn_item(extern_fn("metal_argument_encoder_encoded_length", &[("encoder", ty_int())], ty_int(), "gorget_metal_argument_encoder_encoded_length")));
    items.push(fn_item(extern_fn("metal_argument_encoder_set_argument_buffer", &[("encoder", ty_int()), ("buffer", ty_int()), ("offset", ty_int())], ty_void(), "gorget_metal_argument_encoder_set_argument_buffer")));
    items.push(fn_item(extern_fn("metal_argument_encoder_set_buffer", &[("encoder", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_argument_encoder_set_buffer")));
    items.push(fn_item(extern_fn("metal_argument_encoder_set_texture", &[("encoder", ty_int()), ("texture", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_argument_encoder_set_texture")));
    items.push(fn_item(extern_fn("metal_argument_encoder_set_sampler", &[("encoder", ty_int()), ("sampler", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_argument_encoder_set_sampler")));
    items.push(fn_item(extern_fn("metal_encoder_use_resource", &[("encoder", ty_int()), ("resource", ty_int()), ("usage", ty_int())], ty_void(), "gorget_metal_encoder_use_resource")));
    items.push(fn_item(extern_fn("metal_encoder_use_heap", &[("encoder", ty_int()), ("heap", ty_int())], ty_void(), "gorget_metal_encoder_use_heap")));

    // ── Parallel Render Command Encoder ──────────────────────────
    items.push(fn_item(extern_fn("metal_create_parallel_render_encoder", &[("cmd_buf", ty_int()), ("pass_desc", ty_int())], ty_int(), "gorget_metal_create_parallel_render_encoder")));
    items.push(fn_item(extern_fn("metal_parallel_encoder_make_render_encoder", &[("parallel_enc", ty_int())], ty_int(), "gorget_metal_parallel_encoder_make_render_encoder")));
    items.push(fn_item(extern_fn("metal_parallel_encoder_end", &[("parallel_enc", ty_int())], ty_void(), "gorget_metal_parallel_encoder_end")));

    // ── Visibility Result Buffer (Occlusion Queries) ────────────
    items.push(fn_item(extern_fn("metal_render_pass_set_visibility_result_buffer", &[("desc", ty_int()), ("buffer", ty_int())], ty_void(), "gorget_metal_render_pass_set_visibility_result_buffer")));
    items.push(fn_item(extern_fn("metal_encoder_set_visibility_result_mode", &[("encoder", ty_int()), ("mode", ty_int()), ("offset", ty_int())], ty_void(), "gorget_metal_encoder_set_visibility_result_mode")));

    // ── Full Stencil State (Separate front/back) ────────────────
    items.push(fn_item(extern_fn("metal_create_depth_stencil_full", &[("device", ty_int()), ("depth_compare", ty_int()), ("depth_write", ty_int()), ("front_stencil_compare", ty_int()), ("front_read_mask", ty_int()), ("front_write_mask", ty_int()), ("front_sfail", ty_int()), ("front_dfail", ty_int()), ("front_dspass", ty_int()), ("back_stencil_compare", ty_int()), ("back_read_mask", ty_int()), ("back_write_mask", ty_int()), ("back_sfail", ty_int()), ("back_dfail", ty_int()), ("back_dspass", ty_int())], ty_int(), "gorget_metal_create_depth_stencil_full")));

    // ── MRT Render Pipeline ─────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_render_pipeline_mrt", &[("device", ty_int()), ("vertex_fn", ty_int()), ("fragment_fn", ty_int()), ("vertex_desc", ty_int()), ("num_color_attachments", ty_int()), ("color_formats", ty_vector_uint8()), ("depth_format", ty_int())], ty_int(), "gorget_metal_create_render_pipeline_mrt")));
    items.push(fn_item(extern_fn("metal_render_pass_set_color_texture", &[("desc", ty_int()), ("index", ty_int()), ("texture", ty_int())], ty_void(), "gorget_metal_render_pass_set_color_texture")));

    // ── Indirect Command Buffers (GPU-Driven Rendering) ─────────
    items.push(fn_item(extern_fn("metal_create_indirect_command_buffer", &[("device", ty_int()), ("max_commands", ty_int()), ("inherit_pipeline", ty_int()), ("inherit_buffers", ty_int())], ty_int(), "gorget_metal_create_indirect_command_buffer")));
    items.push(fn_item(extern_fn("metal_icb_indirect_render_command", &[("icb", ty_int()), ("index", ty_int())], ty_int(), "gorget_metal_icb_indirect_render_command")));
    items.push(fn_item(extern_fn("metal_indirect_render_set_pipeline", &[("cmd", ty_int()), ("pipeline", ty_int())], ty_void(), "gorget_metal_indirect_render_set_pipeline")));
    items.push(fn_item(extern_fn("metal_indirect_render_set_vertex_buffer", &[("cmd", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_indirect_render_set_vertex_buffer")));
    items.push(fn_item(extern_fn("metal_indirect_render_set_fragment_buffer", &[("cmd", ty_int()), ("buffer", ty_int()), ("offset", ty_int()), ("index", ty_int())], ty_void(), "gorget_metal_indirect_render_set_fragment_buffer")));
    items.push(fn_item(extern_fn("metal_indirect_render_draw_primitives", &[("cmd", ty_int()), ("prim_type", ty_int()), ("start", ty_int()), ("count", ty_int()), ("instance_count", ty_int()), ("base_instance", ty_int())], ty_void(), "gorget_metal_indirect_render_draw_primitives")));
    items.push(fn_item(extern_fn("metal_indirect_render_draw_indexed", &[("cmd", ty_int()), ("prim_type", ty_int()), ("index_count", ty_int()), ("index_type", ty_int()), ("index_buffer", ty_int()), ("index_offset", ty_int()), ("instance_count", ty_int()), ("base_vertex", ty_int()), ("base_instance", ty_int())], ty_void(), "gorget_metal_indirect_render_draw_indexed")));
    items.push(fn_item(extern_fn("metal_indirect_render_reset", &[("cmd", ty_int())], ty_void(), "gorget_metal_indirect_render_reset")));
    items.push(fn_item(extern_fn("metal_encoder_execute_commands_in_buffer", &[("encoder", ty_int()), ("icb", ty_int()), ("start", ty_int()), ("count", ty_int())], ty_void(), "gorget_metal_encoder_execute_commands_in_buffer")));

    // ── Sampler with LOD ───────────────────────────────────────
    items.push(fn_item(extern_fn("metal_create_sampler_with_lod", &[("device", ty_int()), ("min_filter", ty_int()), ("mag_filter", ty_int()), ("mip_filter", ty_int()), ("address_s", ty_int()), ("address_t", ty_int()), ("lod_min", ty_float()), ("lod_max", ty_float()), ("max_anisotropy", ty_int())], ty_int(), "gorget_metal_create_sampler_with_lod")));
    // Sampler with compare function (for shadow mapping)
    items.push(fn_item(extern_fn("metal_create_sampler_with_compare", &[("device", ty_int()), ("min_filter", ty_int()), ("mag_filter", ty_int()), ("address_s", ty_int()), ("address_t", ty_int()), ("compare_fn", ty_int())], ty_int(), "gorget_metal_create_sampler_with_compare")));

    // ── Debug Groups ─────────────────────────────────────────────
    {
        use crate::ir::abi::AbiKind::{CStr, Scalar as S};
        items.push(fn_item(extern_fn_abi("metal_encoder_push_debug_group", &[("encoder", ty_int(), S), ("label", ty_str(), CStr)], ty_void(), "gorget_metal_encoder_push_debug_group")));
        items.push(fn_item(extern_fn("metal_encoder_pop_debug_group", &[("encoder", ty_int())], ty_void(), "gorget_metal_encoder_pop_debug_group")));
        items.push(fn_item(extern_fn_abi("metal_encoder_insert_debug_signpost", &[("encoder", ty_int(), S), ("label", ty_str(), CStr)], ty_void(), "gorget_metal_encoder_insert_debug_signpost")));
        items.push(fn_item(extern_fn_abi("metal_command_buffer_push_debug_group", &[("cmd_buf", ty_int(), S), ("label", ty_str(), CStr)], ty_void(), "gorget_metal_cmd_buf_push_debug_group")));
    }
    items.push(fn_item(extern_fn("metal_command_buffer_pop_debug_group", &[("cmd_buf", ty_int())], ty_void(), "gorget_metal_cmd_buf_pop_debug_group")));

    // ── MRT (Multiple Render Target) Pipeline ────────────────────
    items.push(fn_item(extern_fn("metal_render_pass_set_color_attachment", &[("desc", ty_int()), ("index", ty_int()), ("texture", ty_int()), ("load_action", ty_int()), ("store_action", ty_int()), ("clear_r", ty_float()), ("clear_g", ty_float()), ("clear_b", ty_float()), ("clear_a", ty_float())], ty_void(), "gorget_metal_render_pass_set_color_attachment")));
    items.push(fn_item(extern_fn("metal_pipeline_set_color_attachment_format", &[("desc", ty_int()), ("index", ty_int()), ("format", ty_int())], ty_void(), "gorget_metal_pipeline_set_color_attachment_format")));
    items.push(fn_item(extern_fn("metal_pipeline_set_color_attachment_blending", &[("desc", ty_int()), ("index", ty_int()), ("enabled", ty_int()), ("src_rgb", ty_int()), ("dst_rgb", ty_int()), ("src_alpha", ty_int()), ("dst_alpha", ty_int())], ty_void(), "gorget_metal_pipeline_set_color_attachment_blending")));

    // ── Device Feature Queries ────────────────────────────────────
    // metal_device_supports_family and metal_device_name already defined in Tier 1
    items.push(fn_item(extern_fn("metal_device_registry_id", &[("device", ty_int())], ty_int(), "gorget_metal_device_registry_id")));
    items.push(fn_item(extern_fn("metal_device_current_allocated_size", &[("device", ty_int())], ty_int(), "gorget_metal_device_current_allocated_size")));

    // ── Full Stencil Configuration ────────────────────────────────
    // metal_create_depth_stencil_full already defined in Tier 1
    // Set front and back stencil reference values separately
    items.push(fn_item(extern_fn("metal_encoder_set_stencil_front_back_ref", &[("encoder", ty_int()), ("front_ref", ty_int()), ("back_ref", ty_int())], ty_void(), "gorget_metal_encoder_set_stencil_front_back_ref")));

    // ── Texture Read-back ──────────────────────────────────────
    items.push(fn_item(extern_fn("metal_texture_get_bytes", &[("texture", ty_int()), ("x", ty_int()), ("y", ty_int()), ("width", ty_int()), ("height", ty_int()), ("bytes_per_row", ty_int())], ty_vector_uint8(), "gorget_metal_texture_get_bytes")));

    // ── Buffer Write from CPU ──────────────────────────────────
    items.push(fn_item(extern_fn("metal_buffer_write", &[("buffer", ty_int()), ("offset", ty_int()), ("data", ty_vector_uint8())], ty_void(), "gorget_metal_buffer_write")));

    // ── Device Queries ─────────────────────────────────────────
    items.push(fn_item(extern_fn("metal_device_max_buffer_length", &[("device", ty_int())], ty_int(), "gorget_metal_device_max_buffer_length")));
    items.push(fn_item(extern_fn("metal_device_max_threads_per_threadgroup", &[("device", ty_int())], ty_int(), "gorget_metal_device_max_threads_per_threadgroup")));
    items.push(fn_item(extern_fn("metal_device_has_unified_memory", &[("device", ty_int())], ty_bool(), "gorget_metal_device_has_unified_memory")));
    items.push(fn_item(extern_fn("metal_device_recommended_max_working_set_size", &[("device", ty_int())], ty_int(), "gorget_metal_device_recommended_max_working_set_size")));

    // ── CAMetalLayer Configuration ─────────────────────────────
    items.push(fn_item(extern_fn("metal_layer_set_vsync", &[("layer", ty_int()), ("enabled", ty_int())], ty_void(), "gorget_metal_layer_set_vsync")));
    items.push(fn_item(extern_fn("metal_layer_set_maximum_drawable_count", &[("layer", ty_int()), ("count", ty_int())], ty_void(), "gorget_metal_layer_set_maximum_drawable_count")));
    items.push(fn_item(extern_fn("metal_layer_set_framebuffer_only", &[("layer", ty_int()), ("flag", ty_int())], ty_void(), "gorget_metal_layer_set_framebuffer_only")));

    // ── Additional Metal Constants ──────────────────────────
    // Barrier scopes
    items.push(const_item("MTL_BARRIER_SCOPE_BUFFERS", 1));
    items.push(const_item("MTL_BARRIER_SCOPE_TEXTURES", 2));
    items.push(const_item("MTL_BARRIER_SCOPE_RENDER_TARGETS", 4));
    // Texture types
    items.push(const_item("MTL_TEXTURE_TYPE_1D", 0));
    items.push(const_item("MTL_TEXTURE_TYPE_2D", 2));
    items.push(const_item("MTL_TEXTURE_TYPE_2D_ARRAY", 4));
    items.push(const_item("MTL_TEXTURE_TYPE_CUBE", 5));
    items.push(const_item("MTL_TEXTURE_TYPE_3D", 7));
    // Additional pixel formats
    items.push(const_item("MTL_PIXEL_FORMAT_R32_FLOAT", 535));
    items.push(const_item("MTL_PIXEL_FORMAT_RG32_FLOAT", 543));
    items.push(const_item("MTL_PIXEL_FORMAT_RGBA32_FLOAT", 560));
    items.push(const_item("MTL_PIXEL_FORMAT_R16_FLOAT", 105));
    items.push(const_item("MTL_PIXEL_FORMAT_RG16_FLOAT", 111));
    items.push(const_item("MTL_PIXEL_FORMAT_RG11B10_FLOAT", 92));
    items.push(const_item("MTL_PIXEL_FORMAT_RGB10A2_UNORM", 90));
    items.push(const_item("MTL_PIXEL_FORMAT_BGRA8_SRGB", 81));
    items.push(const_item("MTL_PIXEL_FORMAT_RGBA8_SRGB", 71));
    items.push(const_item("MTL_PIXEL_FORMAT_R32_UINT", 533));
    items.push(const_item("MTL_PIXEL_FORMAT_R32_SINT", 534));
    items.push(const_item("MTL_PIXEL_FORMAT_DEPTH16_UNORM", 250));
    items.push(const_item("MTL_PIXEL_FORMAT_R16_UINT", 23));
    items.push(const_item("MTL_PIXEL_FORMAT_RG16_UINT", 33));
    items.push(const_item("MTL_PIXEL_FORMAT_RGBA16_UINT", 55));
    items.push(const_item("MTL_PIXEL_FORMAT_R8_UINT", 13));
    items.push(const_item("MTL_PIXEL_FORMAT_RGBA8_UINT", 43));
    // Storage modes
    items.push(const_item("MTL_STORAGE_MODE_MEMORYLESS", 3));
    // Resource usage (for argument buffers / use_resource)
    items.push(const_item("MTL_RESOURCE_USAGE_READ", 1));
    items.push(const_item("MTL_RESOURCE_USAGE_WRITE", 2));
    items.push(const_item("MTL_RESOURCE_USAGE_SAMPLE", 4));
    // Visibility result mode
    items.push(const_item("MTL_VISIBILITY_RESULT_DISABLED", 0));
    items.push(const_item("MTL_VISIBILITY_RESULT_BOOLEAN", 1));
    items.push(const_item("MTL_VISIBILITY_RESULT_COUNTING", 2));
    // Stencil operations
    items.push(const_item("MTL_STENCIL_OP_KEEP", 0));
    items.push(const_item("MTL_STENCIL_OP_ZERO", 1));
    items.push(const_item("MTL_STENCIL_OP_REPLACE", 2));
    items.push(const_item("MTL_STENCIL_OP_INCR_CLAMP", 3));
    items.push(const_item("MTL_STENCIL_OP_DECR_CLAMP", 4));
    items.push(const_item("MTL_STENCIL_OP_INVERT", 5));
    items.push(const_item("MTL_STENCIL_OP_INCR_WRAP", 6));
    items.push(const_item("MTL_STENCIL_OP_DECR_WRAP", 7));
    // Heap type
    items.push(const_item("MTL_HEAP_TYPE_AUTOMATIC", 0));
    items.push(const_item("MTL_HEAP_TYPE_PLACEMENT", 1));
    // GPU families (for feature queries)
    items.push(const_item("MTL_GPU_FAMILY_APPLE1", 1001));
    items.push(const_item("MTL_GPU_FAMILY_APPLE2", 1002));
    items.push(const_item("MTL_GPU_FAMILY_APPLE3", 1003));
    items.push(const_item("MTL_GPU_FAMILY_APPLE4", 1004));
    items.push(const_item("MTL_GPU_FAMILY_APPLE5", 1005));
    items.push(const_item("MTL_GPU_FAMILY_APPLE6", 1006));
    items.push(const_item("MTL_GPU_FAMILY_APPLE7", 1007));
    items.push(const_item("MTL_GPU_FAMILY_APPLE8", 1008));
    items.push(const_item("MTL_GPU_FAMILY_APPLE9", 1009));
    items.push(const_item("MTL_GPU_FAMILY_COMMON1", 3001));
    items.push(const_item("MTL_GPU_FAMILY_COMMON2", 3002));
    items.push(const_item("MTL_GPU_FAMILY_COMMON3", 3003));
    items.push(const_item("MTL_GPU_FAMILY_MAC2", 2002));
    items.push(const_item("MTL_GPU_FAMILY_METAL3", 5001));
    // Command buffer status
    items.push(const_item("MTL_COMMAND_BUFFER_STATUS_NOT_ENQUEUED", 0));
    items.push(const_item("MTL_COMMAND_BUFFER_STATUS_ENQUEUED", 1));
    items.push(const_item("MTL_COMMAND_BUFFER_STATUS_COMMITTED", 2));
    items.push(const_item("MTL_COMMAND_BUFFER_STATUS_SCHEDULED", 3));
    items.push(const_item("MTL_COMMAND_BUFFER_STATUS_COMPLETED", 4));
    items.push(const_item("MTL_COMMAND_BUFFER_STATUS_ERROR", 5));

    // ══════════════════════════════════════════════════════════
    // Metal Tier 3 — Advanced GPU features (Metal 3+)
    // ══════════════════════════════════════════════════════════

    // ── Ray Tracing (Metal 3, Apple7+) ──────────────────────
    // Acceleration structures
    items.push(fn_item(extern_fn("metal_create_acceleration_structure_descriptor", &[("type_", ty_int())], ty_int(), "gorget_metal_create_accel_struct_desc")));
    items.push(fn_item(extern_fn("metal_accel_desc_set_geometry", &[("desc", ty_int()), ("geometry", ty_int()), ("count", ty_int())], ty_void(), "gorget_metal_accel_desc_set_geometry")));
    items.push(fn_item(extern_fn("metal_create_triangle_geometry_descriptor", &[("vertex_buffer", ty_int()), ("vertex_offset", ty_int()), ("vertex_stride", ty_int()), ("triangle_count", ty_int()), ("index_buffer", ty_int()), ("index_type", ty_int())], ty_int(), "gorget_metal_create_triangle_geom_desc")));
    items.push(fn_item(extern_fn("metal_accel_structure_sizes", &[("device", ty_int()), ("desc", ty_int())], ty_int(), "gorget_metal_accel_structure_sizes")));
    items.push(fn_item(extern_fn("metal_create_acceleration_structure", &[("device", ty_int()), ("size", ty_int())], ty_int(), "gorget_metal_create_accel_structure")));
    items.push(fn_item(extern_fn("metal_build_acceleration_structure", &[("encoder", ty_int()), ("desc", ty_int()), ("accel", ty_int()), ("scratch_buffer", ty_int()), ("scratch_offset", ty_int())], ty_void(), "gorget_metal_build_accel_structure")));
    items.push(fn_item(extern_fn("metal_refit_acceleration_structure", &[("encoder", ty_int()), ("desc", ty_int()), ("source", ty_int()), ("dest", ty_int()), ("scratch_buffer", ty_int()), ("scratch_offset", ty_int())], ty_void(), "gorget_metal_refit_accel_structure")));
    // Instance acceleration structures
    items.push(fn_item(extern_fn("metal_create_instance_accel_desc", &[("instance_buffer", ty_int()), ("instance_count", ty_int())], ty_int(), "gorget_metal_create_instance_accel_desc")));
    // Intersection function tables
    items.push(fn_item(extern_fn("metal_create_intersection_function_table", &[("pipeline", ty_int()), ("count", ty_int())], ty_int(), "gorget_metal_create_intersection_fn_table")));
    items.push(fn_item({
        use crate::ir::abi::AbiKind::{CStr, Scalar as S};
        extern_fn_abi("metal_intersection_fn_table_set_function", &[("table", ty_int(), S), ("index", ty_int(), S), ("pipeline", ty_int(), S), ("function_name", ty_str(), CStr)], ty_void(), "gorget_metal_intersection_fn_table_set_fn")
    }));
    // Acceleration structure encoder
    items.push(fn_item(extern_fn("metal_command_buffer_acceleration_structure_encoder", &[("cmd_buf", ty_int())], ty_int(), "gorget_metal_cmd_buf_accel_encoder")));
    items.push(fn_item(extern_fn("metal_accel_encoder_end", &[("encoder", ty_int())], ty_void(), "gorget_metal_accel_encoder_end")));
    // Render encoder ray tracing support
    items.push(fn_item(extern_fn("metal_render_encoder_set_accel_structure", &[("encoder", ty_int()), ("index", ty_int()), ("accel", ty_int())], ty_void(), "gorget_metal_render_enc_set_accel_struct")));
    items.push(fn_item(extern_fn("metal_render_encoder_set_intersection_fn_table", &[("encoder", ty_int()), ("index", ty_int()), ("table", ty_int())], ty_void(), "gorget_metal_render_enc_set_intersection_fn_table")));
    items.push(fn_item(extern_fn("metal_compute_encoder_set_accel_structure", &[("encoder", ty_int()), ("index", ty_int()), ("accel", ty_int())], ty_void(), "gorget_metal_compute_enc_set_accel_struct")));

    // ── Mesh Shaders (Metal 3) ──────────────────────────────
    items.push(fn_item({
        use crate::ir::abi::AbiKind::{CStr, Scalar as S};
        extern_fn_abi("metal_create_mesh_render_pipeline", &[("device", ty_int(), S), ("object_fn", ty_str(), CStr), ("mesh_fn", ty_str(), CStr), ("frag_fn", ty_str(), CStr), ("library", ty_int(), S), ("pixel_format", ty_int(), S)], ty_int(), "gorget_metal_create_mesh_pipeline")
    }));
    items.push(fn_item(extern_fn("metal_render_encoder_draw_mesh_threadgroups", &[("encoder", ty_int()), ("mesh_tg_x", ty_int()), ("mesh_tg_y", ty_int()), ("mesh_tg_z", ty_int()), ("mesh_threads_x", ty_int()), ("mesh_threads_y", ty_int()), ("mesh_threads_z", ty_int()), ("object_threads_x", ty_int()), ("object_threads_y", ty_int()), ("object_threads_z", ty_int())], ty_void(), "gorget_metal_render_enc_draw_mesh_tg")));

    // ── Binary Archives (Metal 2.3+) ────────────────────────
    {
        use crate::ir::abi::AbiKind::{CStr, Scalar as S};
        items.push(fn_item(extern_fn_abi("metal_create_binary_archive", &[("device", ty_int(), S), ("url", ty_str(), CStr)], ty_int(), "gorget_metal_create_binary_archive")));
        items.push(fn_item(extern_fn("metal_binary_archive_add_pipeline", &[("archive", ty_int()), ("desc", ty_int())], ty_int(), "gorget_metal_binary_archive_add_pipeline")));
        items.push(fn_item(extern_fn_abi("metal_binary_archive_serialize", &[("archive", ty_int(), S), ("url", ty_str(), CStr)], ty_int(), "gorget_metal_binary_archive_serialize")));
    }

    // ── Visible Functions / Function Pointers (Metal 2.3+) ──
    items.push(fn_item(extern_fn("metal_create_visible_function_table", &[("pipeline", ty_int()), ("count", ty_int())], ty_int(), "gorget_metal_create_visible_fn_table")));
    items.push(fn_item({
        use crate::ir::abi::AbiKind::{CStr, Scalar as S};
        extern_fn_abi("metal_visible_fn_table_set_function", &[("table", ty_int(), S), ("index", ty_int(), S), ("pipeline", ty_int(), S), ("function_name", ty_str(), CStr)], ty_void(), "gorget_metal_visible_fn_table_set_fn")
    }));
    items.push(fn_item(extern_fn("metal_render_encoder_set_visible_fn_table", &[("encoder", ty_int()), ("index", ty_int()), ("table", ty_int())], ty_void(), "gorget_metal_render_enc_set_visible_fn_table")));
    items.push(fn_item(extern_fn("metal_compute_encoder_set_visible_fn_table", &[("encoder", ty_int()), ("index", ty_int()), ("table", ty_int())], ty_void(), "gorget_metal_compute_enc_set_visible_fn_table")));

    // ── Texture Views / Shared Resources (Metal 2+) ─────────
    items.push(fn_item(extern_fn("metal_texture_new_view", &[("texture", ty_int()), ("pixel_format", ty_int()), ("texture_type", ty_int()), ("levels", ty_int()), ("num_levels", ty_int()), ("slices", ty_int()), ("num_slices", ty_int())], ty_int(), "gorget_metal_texture_new_view")));
    items.push(fn_item(extern_fn("metal_make_aliasable", &[("resource", ty_int())], ty_void(), "gorget_metal_make_aliasable")));

    // ── Fence / Event Synchronization (Metal 2+) ────────────
    // metal_create_fence, metal_create_event, metal_create_shared_event,
    // metal_command_buffer_encode_signal_event, metal_command_buffer_encode_wait_event
    // already defined in Tier 1
    items.push(fn_item(extern_fn("metal_render_encoder_update_fence", &[("encoder", ty_int()), ("fence", ty_int()), ("stages", ty_int())], ty_void(), "gorget_metal_render_enc_update_fence")));
    items.push(fn_item(extern_fn("metal_render_encoder_wait_for_fence", &[("encoder", ty_int()), ("fence", ty_int()), ("stages", ty_int())], ty_void(), "gorget_metal_render_enc_wait_fence")));

    // ── Timestamps & Counters (Metal 2.3+) ──────────────────
    items.push(fn_item(extern_fn("metal_create_counter_sample_buffer", &[("device", ty_int()), ("count", ty_int())], ty_int(), "gorget_metal_create_counter_sample_buffer")));
    items.push(fn_item(extern_fn("metal_device_sample_timestamps", &[("device", ty_int())], ty_int(), "gorget_metal_device_sample_timestamps")));

    // ── Dynamic Libraries (Metal 2.3+) ──────────────────────
    items.push(fn_item(extern_fn("metal_create_dynamic_library", &[("device", ty_int()), ("library", ty_int())], ty_int(), "gorget_metal_create_dynamic_library")));

    // ── Residency Sets (Metal 3+) ───────────────────────────
    items.push(fn_item(extern_fn("metal_create_residency_set", &[("device", ty_int())], ty_int(), "gorget_metal_create_residency_set")));
    items.push(fn_item(extern_fn("metal_residency_set_add_allocation", &[("set", ty_int()), ("resource", ty_int())], ty_void(), "gorget_metal_residency_set_add")));
    items.push(fn_item(extern_fn("metal_residency_set_commit", &[("set", ty_int())], ty_void(), "gorget_metal_residency_set_commit")));

    // ── Metal Tier 3 Constants ──────────────────────────────
    // Acceleration structure types
    items.push(const_item("MTL_ACCEL_STRUCT_PRIMITIVE", 0));
    items.push(const_item("MTL_ACCEL_STRUCT_INSTANCE", 1));
    // Index types for ray tracing
    items.push(const_item("MTL_INDEX_TYPE_UINT16", 0));
    items.push(const_item("MTL_INDEX_TYPE_UINT32", 1));
    items.push(const_item("MTL_INDEX_TYPE_NONE", 2));
    // Render stages for fence/event
    items.push(const_item("MTL_RENDER_STAGE_VERTEX", 1));
    items.push(const_item("MTL_RENDER_STAGE_FRAGMENT", 2));
    items.push(const_item("MTL_RENDER_STAGE_OBJECT", 4));
    items.push(const_item("MTL_RENDER_STAGE_MESH", 8));
    items.push(const_item("MTL_RENDER_STAGE_TILE", 16));
    // Intersection function signature
    items.push(const_item("MTL_INTERSECTION_FN_SIGNATURE_NONE", 0));
    items.push(const_item("MTL_INTERSECTION_FN_SIGNATURE_INSTANCING", 1));
    items.push(const_item("MTL_INTERSECTION_FN_SIGNATURE_TRIANGLE_DATA", 2));
    items.push(const_item("MTL_INTERSECTION_FN_SIGNATURE_WORLD_SPACE_DATA", 4));
    // Acceleration structure usage
    items.push(const_item("MTL_ACCEL_STRUCT_USAGE_NONE", 0));
    items.push(const_item("MTL_ACCEL_STRUCT_USAGE_REFIT", 1));
    items.push(const_item("MTL_ACCEL_STRUCT_USAGE_PREFER_FAST_BUILD", 2));
    Module {
        items,
        span: Span::dummy(),
    }
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
        assert!(is_builtin_module(&["xtd".into(), "sdl".into()]));
        assert!(is_builtin_module(&["xtd".into(), "ecs".into()]));
        assert!(is_builtin_module(&["xtd".into(), "json".into()]));
        assert!(is_builtin_module(&["xtd".into(), "yaml".into()]));
        assert!(is_builtin_module(&["xtd".into(), "http".into()]));
        assert!(is_builtin_module(&["xtd".into(), "regex".into()]));
        assert!(is_builtin_module(&["xtd".into(), "crypto".into()]));
        assert!(is_builtin_module(&["xtd".into(), "toml".into()]));
        assert!(is_builtin_module(&["xtd".into(), "xml".into()]));
        assert!(is_builtin_module(&["xtd".into(), "csv".into()]));
        assert!(is_builtin_module(&["xtd".into(), "gfx".into()]));
        assert!(is_builtin_module(&["xtd".into(), "ssh".into()]));
        assert!(is_builtin_module(&["xtd".into(), "p2p".into()]));
        assert!(is_builtin_module(&["xtd".into(), "db".into()]));
        assert!(is_builtin_module(&["xtd".into(), "sqlite".into()]));
        assert!(is_builtin_module(&["xtd".into(), "influx".into()]));
        // old std.* battery paths are NOT valid anymore
        assert!(!is_builtin_module(&["std".into(), "sdl".into()]));
        assert!(!is_builtin_module(&["std".into(), "json".into()]));
        assert!(!is_builtin_module(&["std".into(), "crypto".into()]));
        assert!(!is_builtin_module(&["xtd".into(), "http".into(), "client".into()]));
        assert!(!is_builtin_module(&["std".into(), "test".into(), "process".into()]));
        assert!(!is_builtin_module(&["std".into(), "foo".into()]));
        assert!(!is_builtin_module(&["foo".into(), "fs".into()]));
        assert!(!is_builtin_module(&["std".into()]));
    }

    #[test]
    fn generate_fs() {
        let m = generate_builtin_module(&["std".into(), "fs".into()]).unwrap();
        assert_eq!(m.items.len(), 13);
        let names: Vec<_> = m.items.iter().map(|i| match &i.node {
            Item::Function(f) => f.name.node.clone(),
            _ => panic!("expected function"),
        }).collect();
        assert!(names.contains(&"read_file".to_string()));
        assert!(names.contains(&"read_file_bytes".to_string()));
        assert!(names.contains(&"write_file".to_string()));
        assert!(names.contains(&"write_file_bytes".to_string()));
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
        assert_eq!(m.items.len(), 9); // +1 for int_to_float
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
        assert_eq!(m.items.len(), 8); // 7 structs + 1 File equip block
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
        assert!(is_builtin_module(&["xtd".into(), "gfx".into()]));
    }

    #[test]
    fn generate_gfx_returns_none() {
        // xtd.gfx is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "gfx".into()]).is_none());
    }

    #[test]
    fn gfx_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "gfx".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Canvas"));
        assert!(src.contains("struct Color"));
        assert!(src.contains("Result[Canvas, String] open("));
        assert!(src.contains("void close("));
        assert!(src.contains("void fill_circle("));
    }

    #[test]
    fn gfx_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "gfx".into()]).unwrap();
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
        assert!(fn_names.contains(&"open".to_string()));
        assert!(fn_names.contains(&"draw_circle".to_string()));
        assert!(fn_names.contains(&"fill_circle".to_string()));
    }

    #[test]
    fn is_builtin_ecs() {
        assert!(is_builtin_module(&["xtd".into(), "ecs".into()]));
    }

    #[test]
    fn generate_ecs_returns_none() {
        // xtd.ecs is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "ecs".into()]).is_none());
    }

    #[test]
    fn ecs_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "ecs".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct EntityPool"));
        assert!(src.contains("struct SparseSet"));
        assert!(src.contains("equip EntityPool"));
        assert!(src.contains("equip SparseSet"));
    }

    #[test]
    fn ecs_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "ecs".into()]).unwrap();
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
        assert!(is_builtin_module(&["xtd".into(), "sdl".into()]));
    }

    #[test]
    fn generate_sdl() {
        let m = generate_builtin_module(&["xtd".into(), "sdl".into()]).unwrap();

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
        assert!(is_builtin_module(&["xtd".into(), "ssh".into()]));
    }

    #[test]
    fn generate_ssh_returns_none() {
        // xtd.ssh is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "ssh".into()]).is_none());
    }

    #[test]
    fn ssh_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "ssh".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Session"));
        assert!(src.contains("struct CommandResult"));
        assert!(src.contains("connect"));
        assert!(src.contains("channel_exec"));
    }

    #[test]
    fn ssh_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "ssh".into()]).unwrap();
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
        assert!(fn_names.contains(&"connect".to_string()));
        assert!(fn_names.contains(&"send_packet".to_string()));
        assert!(fn_names.contains(&"read_packet".to_string()));
        assert_eq!(equip_count, 1);
    }

    #[test]
    fn is_builtin_crypto() {
        assert!(is_builtin_module(&["xtd".into(), "crypto".into()]));
    }

    #[test]
    fn generate_crypto() {
        let m = generate_builtin_module(&["xtd".into(), "crypto".into()]).unwrap();
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
        assert!(struct_names.contains(&"ServerSocket".to_string()));
        assert!(fn_names.contains(&"socket_connect".to_string()));
        assert!(fn_names.contains(&"server_socket_bind".to_string()));
        assert_eq!(equip_count, 2); // Socket + ServerSocket
        assert!(equip_method_names.contains(&"read".to_string()));
        assert!(equip_method_names.contains(&"write".to_string()));
        assert!(equip_method_names.contains(&"close".to_string()));
        assert!(equip_method_names.contains(&"accept".to_string()));
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
        assert!(is_builtin_module(&["xtd".into(), "json".into()]));
    }

    #[test]
    fn generate_json_returns_none() {
        // xtd.json is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "json".into()]).is_none());
    }

    #[test]
    fn json_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "json".into()]);
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
        let source = builtin_module_source(&["xtd".into(), "json".into()]).unwrap();
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
        assert!(is_builtin_module(&["xtd".into(), "toml".into()]));
    }

    #[test]
    fn generate_toml_returns_none() {
        assert!(generate_builtin_module(&["xtd".into(), "toml".into()]).is_none());
    }

    #[test]
    fn toml_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "toml".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("enum TomlValue"));
        assert!(src.contains("parse"));
        assert!(src.contains("stringify"));
        assert!(src.contains("equip TomlValue"));
        assert!(src.contains("Arr(Vector[TomlValue])"));
        assert!(src.contains("Tbl(Dict[String, TomlValue])"));
    }

    #[test]
    fn toml_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "toml".into()]).unwrap();
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
        assert!(fn_names.contains(&"parse".to_string()));
        assert!(fn_names.contains(&"stringify".to_string()));
        assert_eq!(equip_count, 2); // equip TomlParser + equip TomlValue
    }

    #[test]
    fn is_builtin_xml() {
        assert!(is_builtin_module(&["xtd".into(), "xml".into()]));
    }

    #[test]
    fn generate_xml_returns_none() {
        // xtd.xml is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "xml".into()]).is_none());
    }

    #[test]
    fn xml_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "xml".into()]);
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
        let source = builtin_module_source(&["xtd".into(), "xml".into()]).unwrap();
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
        assert!(is_builtin_module(&["xtd".into(), "yaml".into()]));
    }

    #[test]
    fn generate_yaml_returns_none() {
        // xtd.yaml is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "yaml".into()]).is_none());
    }

    #[test]
    fn yaml_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "yaml".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("enum Yaml"));
        assert!(src.contains("parse"));
        assert!(src.contains("stringify"));
        assert!(src.contains("pretty"));
        assert!(src.contains("equip Yaml"));
        assert!(src.contains("equip YamlParser"));
    }

    #[test]
    fn yaml_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "yaml".into()]).unwrap();
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
        assert!(fn_names.contains(&"parse".to_string()));
        assert!(fn_names.contains(&"stringify".to_string()));
        assert!(fn_names.contains(&"pretty".to_string()));
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
        assert!(struct_names.contains(&"TlsServerSocket".to_string()));
        assert!(fn_names.contains(&"tls_connect".to_string()));
        assert!(fn_names.contains(&"tls_server_bind".to_string()));
        assert_eq!(equip_count, 2); // TlsSocket + TlsServerSocket
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
                                "close",
                                "set_timeout"
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
        // xtd.http is file-based, so generate_builtin_module returns None
        assert!(generate_builtin_module(&["xtd".into(), "http".into()]).is_none());
        // but builtin_module_source returns the source
        assert!(builtin_module_source(&["xtd".into(), "http".into()]).is_some());
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
                                "close",
                                "nb_read",
                                "nb_write",
                                "nb_write_str",
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
        let m = generate_builtin_module(&["xtd".into(), "crypto".into()]).unwrap();
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
        let m = generate_builtin_module(&["xtd".into(), "crypto".into()]).unwrap();
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

    // ─── xtd.csv ───────────────────────────────────────────────────

    #[test]
    fn is_builtin_csv() {
        assert!(is_builtin_module(&["xtd".into(), "csv".into()]));
    }

    #[test]
    fn generate_csv_returns_none() {
        // xtd.csv is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "csv".into()]).is_none());
    }

    #[test]
    fn csv_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "csv".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct CsvParser"));
        assert!(src.contains("struct CsvTable"));
        assert!(src.contains("Result[Vector[Vector[String]], String] parse("));
        assert!(src.contains("String stringify("));
        assert!(src.contains("equip CsvParser"));
        assert!(src.contains("equip CsvTable"));
    }

    #[test]
    fn csv_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "csv".into()]).unwrap();
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
        assert!(fn_names.contains(&"parse".to_string()));
        assert!(fn_names.contains(&"stringify".to_string()));
        assert!(fn_names.contains(&"parse_table".to_string()));
        assert!(fn_names.contains(&"stringify_table".to_string()));
        assert!(equip_count >= 2, "expected at least 2 equip blocks, got {equip_count}");
    }

    // ─── xtd.tensor ────────────────────────────────────────────

    #[test]
    fn is_builtin_tensor() {
        assert!(is_builtin_module(&["xtd".into(), "tensor".into()]));
    }

    #[test]
    fn generate_tensor_returns_none() {
        assert!(generate_builtin_module(&["xtd".into(), "tensor".into()]).is_none());
    }

    #[test]
    fn tensor_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "tensor".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Tensor"));
        assert!(src.contains("tensor_arange"));
        assert!(src.contains("tensor_zeros"));
        assert!(src.contains("tensor_bsub"));
        assert!(src.contains("tensor_le"));
        assert!(src.contains("tensor_ne"));
    }

    #[test]
    fn tensor_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "tensor".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let _module = parser.parse_module();
        assert!(parser.errors.is_empty(), "tensor.gg parse errors: {:?}", parser.errors);
    }

    // ─── xtd.dataframe ─────────────────────────────────────────

    #[test]
    fn is_builtin_dataframe() {
        assert!(is_builtin_module(&["xtd".into(), "dataframe".into()]));
    }

    #[test]
    fn generate_dataframe_returns_none() {
        assert!(generate_builtin_module(&["xtd".into(), "dataframe".into()]).is_none());
    }

    #[test]
    fn dataframe_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "dataframe".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct DataFrame"));
        assert!(src.contains("df_from_columns"));
        assert!(src.contains("df_from_csv"));
    }

    #[test]
    fn dataframe_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "dataframe".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let _module = parser.parse_module();
        assert!(parser.errors.is_empty(), "dataframe.gg parse errors: {:?}", parser.errors);
    }

    // ─── xtd.uuid ──────────────────────────────────────────────

    #[test]
    fn is_builtin_uuid() {
        assert!(is_builtin_module(&["xtd".into(), "uuid".into()]));
    }

    #[test]
    fn generate_uuid_returns_none() {
        assert!(generate_builtin_module(&["xtd".into(), "uuid".into()]).is_none());
    }

    #[test]
    fn uuid_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "uuid".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct UUID"));
        assert!(src.contains("v4()"));
        assert!(src.contains("to_string"));
    }

    #[test]
    fn uuid_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "uuid".into()]).unwrap();
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
        assert!(fn_names.contains(&"hex_val".to_string()));
    }

    // ─── xtd.log ───────────────────────────────────────────────

    #[test]
    fn is_builtin_log() {
        assert!(is_builtin_module(&["xtd".into(), "log".into()]));
    }

    #[test]
    fn generate_log_returns_none() {
        assert!(generate_builtin_module(&["xtd".into(), "log".into()]).is_none());
    }

    #[test]
    fn log_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "log".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Logger"));
        assert!(src.contains("enum LogLevel"));
        assert!(src.contains("log_info"));
    }

    #[test]
    fn log_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "log".into()]).unwrap();
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

    // ─── xtd.cli ───────────────────────────────────────────────

    #[test]
    fn is_builtin_cli() {
        assert!(is_builtin_module(&["xtd".into(), "cli".into()]));
    }

    #[test]
    fn generate_cli_returns_none() {
        assert!(generate_builtin_module(&["xtd".into(), "cli".into()]).is_none());
    }

    #[test]
    fn cli_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "cli".into()]);
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
        let source = builtin_module_source(&["xtd".into(), "cli".into()]).unwrap();
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

    // ─── xtd.db / xtd.sqlite / xtd.influx ──────────────────────

    #[test]
    fn db_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "db".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct Row"));
        assert!(src.contains("enum Param"));
        assert!(src.contains("trait FromRow"));
        assert!(src.contains("trait Queryable"));
        assert!(src.contains("trait DbConnection"));
    }

    #[test]
    fn db_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "db".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "db.gg parse errors: {:?}", parser.errors);
        let has_trait = module.items.iter().any(|i| match &i.node {
            Item::Trait(t) => t.name.node == "FromRow" || t.name.node == "Queryable",
            _ => false,
        });
        assert!(has_trait, "expected FromRow or Queryable trait in db.gg");
    }

    #[test]
    fn sqlite_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "sqlite".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct SqliteConn"));
        assert!(src.contains("Result[SqliteConn, String] open("));
        assert!(src.contains("DbConnection"));
    }

    #[test]
    fn sqlite_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "sqlite".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "sqlite.gg parse errors: {:?}", parser.errors);
        let fn_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Function(f) => Some(f.name.node.clone()),
            _ => None,
        }).collect();
        assert!(fn_names.contains(&"open".to_string()), "sqlite.gg missing open");
    }

    #[test]
    fn influx_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "influx".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct InfluxClient"));
        assert!(src.contains("influx_connect"));
        assert!(src.contains("DbConnection"));
    }

    #[test]
    fn influx_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "influx".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "influx.gg parse errors: {:?}", parser.errors);
        let fn_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Function(f) => Some(f.name.node.clone()),
            _ => None,
        }).collect();
        assert!(fn_names.contains(&"influx_connect".to_string()), "influx.gg missing influx_connect");
    }

    #[test]
    fn jsonpath_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "jsonpath".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "jsonpath.gg parse errors: {:?}", parser.errors);
        let fn_names: Vec<_> = module.items.iter().filter_map(|i| match &i.node {
            Item::Function(f) => Some(f.name.node.clone()),
            _ => None,
        }).collect();
        assert!(fn_names.contains(&"get".to_string()), "jsonpath.gg missing get");
        assert!(fn_names.contains(&"get_all".to_string()), "jsonpath.gg missing get_all");
        assert!(fn_names.contains(&"set".to_string()), "jsonpath.gg missing set");
        assert!(fn_names.contains(&"delete".to_string()), "jsonpath.gg missing delete");
    }

}
