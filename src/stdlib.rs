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
                "time" => None, // file-based
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
                "async" => None, // file-based
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
            "sdl" => None, // file-based module — loaded via builtin_module_source()
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
            "gl" => None, // file-based module — loaded via builtin_module_source()
            "image" => Some(gen_image_module()),
            "audio" => Some(gen_audio_module()),
            "compress" => Some(gen_compress_module()),
            "metal" => None, // file-based module — loaded via builtin_module_source()
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


/// std.async — non-blocking I/O helpers backed by the GorgetReactor (timerfd/kqueue).
/// async_sleep(ms: int) suspends the current task for `ms` milliseconds using the reactor.
// gen_async_module — migrated to lib/std/async.gg

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
            decl_blocking_fn_abi("exec", &[("cmd", ty_str(), CStr)], ty_int())
        })),
        Spanned::dummy(Item::Function({
            use crate::ir::abi::AbiKind::CStr;
            decl_blocking_fn_abi("exec_output", &[("cmd", ty_str(), CStr)], exec_result_type)
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

// gen_sdl_module() removed — now file-based at lib/xtd/sdl.gg
// Retained as comment for history. See git log for original synthetic module.

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
            Some("time") => Some(include_str!("../lib/std/time.gg")),
            Some("signal") => Some(include_str!("../lib/std/signal.gg")),
            Some("async") => Some(include_str!("../lib/std/async.gg")),
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
            Some("sdl") => Some(include_str!("../lib/xtd/sdl.gg")),
            Some("gl") => Some(include_str!("../lib/xtd/gl.gg")),
            Some("metal") => Some(include_str!("../lib/xtd/metal.gg")),
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

/// Declare a blocking extern function — yields shared variable locks during call.
fn decl_blocking_fn(name: &str, params: &[(&str, Type)], ret: Type) -> FunctionDef {
    let mut f = decl_fn(name, params, ret);
    f.qualifiers.is_blocking = true;
    f
}

fn decl_blocking_fn_abi(name: &str, params: &[(&str, Type, crate::ir::abi::AbiKind)], ret: Type) -> FunctionDef {
    let mut f = decl_fn_abi(name, params, ret);
    f.qualifiers.is_blocking = true;
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
        decl_blocking_fn_abi("socket_connect", &[("host", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_socket(), ty_str()))
    })));

    // NOTE: async_socket_connect deferred — connect is rarely the hot path in servers.
    // The important async ops are async_accept, async_read, async_write for server loops.

    // Socket methods — extern bindings
    items.push(equip_block("Socket", vec![
        extern_blocking_method("read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_read"),
        extern_blocking_method("read_exact", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_socket_read_exact"),
        extern_blocking_method("write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_socket_write"),
        extern_blocking_method("write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_socket_write_str"),
        extern_blocking_method("read_line", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_socket_read_line"),
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
        decl_blocking_fn_abi("server_socket_bind", &[("host", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_server_socket(), ty_str()))
    })));

    // ServerSocket methods — extern bindings
    // accept() returns a Result[Socket, str]: the accepted client reuses all Socket methods.
    items.push(equip_block("ServerSocket", vec![
        extern_blocking_method("accept", Ownership::MutableBorrow, &[], ty_result(ty_socket(), ty_str()), "gorget_server_socket_accept"),
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
        decl_blocking_fn_abi("tls_connect", &[("host", ty_str(), CStr), ("port", ty_int(), Scalar)], ty_result(ty_tls_socket(), ty_str()))
    })));

    // Free function: tls_server_bind(host, port, cert_path, key_path) -> Result[TlsServerSocket, str]
    items.push(Spanned::dummy(Item::Function({
        use crate::ir::abi::AbiKind::{CStr, Scalar};
        decl_blocking_fn_abi("tls_server_bind", &[
            ("host", ty_str(), CStr),
            ("port", ty_int(), Scalar),
            ("cert_path", ty_str(), CStr),
            ("key_path", ty_str(), CStr),
        ], ty_result(ty_tls_server_socket(), ty_str()))
    })));

    // TlsSocket methods — extern bindings
    items.push(equip_block("TlsSocket", vec![
        extern_blocking_method("read", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_tls_read"),
        extern_blocking_method("read_exact", Ownership::MutableBorrow, &[("n", ty_int())], ty_vector_uint8(), "gorget_tls_read_exact"),
        extern_blocking_method("write", Ownership::MutableBorrow, &[("data", ty_vector_uint8())], ty_int(), "gorget_tls_write"),
        extern_blocking_method("write_str", Ownership::MutableBorrow, &[("s", ty_str())], ty_int(), "gorget_tls_write_str"),
        extern_blocking_method("read_line", Ownership::MutableBorrow, &[], ty_result(ty_string(), ty_str()), "gorget_tls_read_line"),
        extern_method("close", Ownership::MutableBorrow, &[], ty_void(), "gorget_tls_close"),
        extern_method("set_timeout", Ownership::MutableBorrow, &[("ms", ty_int())], ty_void(), "gorget_tls_set_timeout"),
    ]));

    // TlsServerSocket methods — extern bindings
    items.push(equip_block("TlsServerSocket", vec![
        extern_blocking_method("accept", Ownership::MutableBorrow, &[], ty_result(ty_tls_socket(), ty_str()), "gorget_tls_server_accept"),
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

fn extern_blocking_method(
    name: &str,
    self_ownership: Ownership,
    extra_params: &[(&str, Type)],
    ret: Type,
    c_symbol: &str,
) -> FunctionDef {
    let mut f = extern_method(name, self_ownership, extra_params, ret, c_symbol);
    f.qualifiers.is_blocking = true;
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

// gen_gl_module() removed — now file-based at lib/xtd/gl.gg
// Retained as comment for history. See git log for original synthetic module.
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
    fn generate_sdl_returns_none() {
        // xtd.sdl is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "sdl".into()]).is_none());
    }

    #[test]
    fn sdl_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "sdl".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("struct SDLWindow"));
        assert!(src.contains("struct SDLRenderer"));
        assert!(src.contains("struct SDLTexture"));
        assert!(src.contains("struct SDLFont"));
        assert!(src.contains("struct SDLEvent"));
        assert!(src.contains("const int SDL_INIT_VIDEO"));
        assert!(src.contains("const int SDLK_ESCAPE"));
        assert!(src.contains("sdl_init"));
        assert!(src.contains("sdl_create_window"));
        assert!(src.contains("sdl_poll_event"));
    }

    #[test]
    fn sdl_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "sdl".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "sdl.gg parse errors: {:?}", parser.errors);

        // Collect item names
        let mut struct_names = vec![];
        let mut const_names = vec![];
        let mut fn_names = vec![];
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::ConstDecl(c) => const_names.push(c.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::ExternBlock(eb) => {
                    for f in &eb.items {
                        fn_names.push(f.node.name.node.clone());
                    }
                }
                _ => {}
            }
        }

        assert!(struct_names.contains(&"SDLWindow".to_string()));
        assert!(struct_names.contains(&"SDLEvent".to_string()));
        assert!(const_names.contains(&"SDL_INIT_VIDEO".to_string()));
        assert!(const_names.contains(&"SDL_QUIT".to_string()));
        assert!(const_names.contains(&"SDLK_ESCAPE".to_string()));
        assert!(fn_names.contains(&"sdl_init".to_string()));
        assert!(fn_names.contains(&"sdl_quit".to_string()));
        assert!(fn_names.contains(&"sdl_create_window".to_string()));
        assert!(fn_names.contains(&"sdl_create_renderer".to_string()));
        assert!(fn_names.contains(&"sdl_poll_event".to_string()));
        assert!(fn_names.contains(&"sdl_delay".to_string()));
        assert!(fn_names.contains(&"sdl_load_font".to_string()));
        assert!(fn_names.contains(&"sdl_draw_text".to_string()));
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
