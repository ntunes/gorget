/// Built-in module system — covers both `std` (core) and `xtd` (batteries).
///
/// The namespace split:
/// - `std.*` — lean, stable building blocks: collections, I/O, math, OS, net, …
/// - `xtd.*`  — batteries-included ecosystem: JSON, TOML, XML, YAML, CSV,
///              crypto, regex, SDL, GFX, ECS, SSH, HTTP, P2P, …
///
/// All built-in modules are file-based `.gg` files under `lib/std/` or `lib/xtd/`.
/// The loader reads source via `include_str!`, parses it, recursively resolves
/// imports, and merges the resulting AST into the main module. Semantic analysis
/// (name resolution, type checking, borrow checking) runs on the merged result.
///
/// Modules declare their C runtime bindings using `extern "C":` blocks with
/// explicit `= "c_symbol"` annotations. The `blocking` qualifier marks functions
/// that may block (for shared variable lock release). ABI marshalling (cstr, etc.)
/// is derived from the extern block's ABI string and explicit FFI types.
///
/// ## Adding a new module
///
/// 1. Create `lib/std/<name>.gg` (or `lib/xtd/<name>.gg`).
/// 2. Add the module name to `is_builtin_module`.
/// 3. Add `None` to `generate_builtin_module`.
/// 4. Add `include_str!` to `builtin_module_source`.
/// 5. Add unit tests (at minimum: is_builtin, source exists/parses).
use crate::parser::ast::*;

/// Check if an import path refers to a built-in module (`std.*` or `xtd.*`).
pub fn is_builtin_module(segments: &[String]) -> bool {
    match segments.first().map(|s| s.as_str()) {
        Some("std") => match segments.len() {
            2 => matches!(segments[1].as_str(),
                "fs" | "path" | "os" | "conv" | "io" | "iter" | "random" | "time"
                | "collections" | "math" | "fmt" | "process" | "bytes"
                | "encoding" | "channel" | "alloc" | "term" | "heap" | "datetime"
                | "hash" | "sync" | "thread" | "async" | "signal"),
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
        Some("gg") => segments.len() == 2 && matches!(segments[1].as_str(),
            "fb"),
        _ => false,
    }
}

/// Legacy entry point for synthetic module generation.
/// All modules are now file-based — this always returns `None`.
/// Kept for API compatibility with `loader.rs`; will be removed in a future cleanup.
pub fn generate_builtin_module(_segments: &[String]) -> Option<Module> {
    None
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
            Some("time") => Some(include_str!("../lib/std/time.gg")),
            Some("conv") => Some(include_str!("../lib/std/conv.gg")),
            Some("process") => Some(include_str!("../lib/std/process.gg")),
            Some("alloc") => Some(include_str!("../lib/std/alloc.gg")),
            Some("channel") => Some(include_str!("../lib/std/channel.gg")),
            Some("collections") => Some(include_str!("../lib/std/collections.gg")),
            Some("io") => Some(include_str!("../lib/std/io.gg")),
            Some("iter") => Some(include_str!("../lib/std/iter.gg")),
            Some("math") => Some(include_str!("../lib/std/math.gg")),
            Some("os") => Some(include_str!("../lib/std/os.gg")),
            Some("signal") => Some(include_str!("../lib/std/signal.gg")),
            Some("thread") => Some(include_str!("../lib/std/thread.gg")),
            Some("sync") => Some(include_str!("../lib/std/sync.gg")),
            Some("async") => Some(include_str!("../lib/std/async.gg")),
            Some("fmt") => Some(include_str!("../lib/std/fmt.gg")),
            Some("bytes") => Some(include_str!("../lib/std/bytes.gg")),
            Some("encoding") => Some(include_str!("../lib/std/encoding.gg")),
            Some("term") => Some(include_str!("../lib/std/term.gg")),
            Some("heap") => Some(include_str!("../lib/std/heap.gg")),
            Some("hash") => Some(include_str!("../lib/std/hash.gg")),
            Some("datetime") => Some(include_str!("../lib/std/datetime.gg")),
            Some("net") => match segments.get(2).map(|s| s.as_str()) {
                Some("socket") => Some(include_str!("../lib/std/socket.gg")),
                Some("tls") => Some(include_str!("../lib/std/tls.gg")),
                Some("udp") => Some(include_str!("../lib/std/udp.gg")),
                _ => None,
            },
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
            Some("audio") => Some(include_str!("../lib/xtd/audio.gg")),
            Some("image") => Some(include_str!("../lib/xtd/image.gg")),
            Some("compress") => Some(include_str!("../lib/xtd/compress.gg")),
            Some("crypto") => Some(include_str!("../lib/xtd/crypto.gg")),
            Some("regex") => Some(include_str!("../lib/xtd/regex.gg")),
            Some("sdl") => Some(include_str!("../lib/xtd/sdl.gg")),
            Some("gl") => Some(include_str!("../lib/xtd/gl.gg")),
            Some("metal") => Some(include_str!("../lib/xtd/metal.gg")),
            Some("gpu") => Some(include_str!("../lib/xtd/gpu.gg")),
            _ => None,
        },
        // gg.* platform modules
        Some("gg") => match segments.get(1).map(|s| s.as_str()) {
            Some("fb") => Some(include_str!("../lib/gg/fb.gg")),
            _ => None,
        },
        _ => None,
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
        assert!(is_builtin_module(&["std".into(), "hash".into()]));
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
    fn generate_fs_returns_none() {
        // std.fs is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["std".into(), "fs".into()]).is_none());
        assert!(builtin_module_source(&["std".into(), "fs".into()]).is_some());
    }

    #[test]
    fn generate_conv_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "conv".into()]).is_none());
        assert!(builtin_module_source(&["std".into(), "conv".into()]).is_some());
    }

    #[test]
    fn generate_io_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "io".into()]).is_none());
        assert!(builtin_module_source(&["std".into(), "io".into()]).is_some());
    }

    #[test]
    fn generate_random_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "random".into()]).is_none());
        assert!(builtin_module_source(&["std".into(), "random".into()]).is_some());
    }

    #[test]
    fn generate_time_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "time".into()]).is_none());
        assert!(builtin_module_source(&["std".into(), "time".into()]).is_some());
    }

    #[test]
    fn generate_math_returns_none() {
        assert!(generate_builtin_module(&["std".into(), "math".into()]).is_none());
        assert!(builtin_module_source(&["std".into(), "math".into()]).is_some());
    }

    #[test]
    fn generate_collections() {
        // std.collections is now file-based
        assert!(generate_builtin_module(&["std".into(), "collections".into()]).is_none());
        let source = builtin_module_source(&["std".into(), "collections".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let m = parser.parse_module();
        assert!(parser.errors.is_empty(), "collections.gg parse errors: {:?}", parser.errors);
        let names: Vec<_> = m.items.iter().filter_map(|i| match &i.node {
            Item::Struct(s) => Some(s.name.node.clone()),
            _ => None,
        }).collect();
        assert!(names.contains(&"Vector".to_string()));
        assert!(names.contains(&"Dict".to_string()));
        assert!(names.contains(&"Set".to_string()));
        assert!(names.contains(&"Box".to_string()));
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
    fn generate_crypto_returns_none() {
        // xtd.crypto is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "crypto".into()]).is_none());
    }

    #[test]
    fn crypto_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "crypto".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("crypto_sha256"));
        assert!(src.contains("crypto_sha1"));
        assert!(src.contains("equip CipherContext"));
    }

    #[test]
    fn is_builtin_net_socket() {
        assert!(is_builtin_module(&["std".into(), "net".into(), "socket".into()]));
    }

    #[test]
    fn generate_socket_returns_none() {
        // std.net.socket is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["std".into(), "net".into(), "socket".into()]).is_none());
    }

    #[test]
    fn socket_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "net".into(), "socket".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("socket_connect"));
        assert!(src.contains("server_socket_bind"));
        assert!(src.contains("equip Socket"));
        assert!(src.contains("equip ServerSocket"));
    }

    #[test]
    fn socket_source_parses() {
        let source = builtin_module_source(&["std".into(), "net".into(), "socket".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "socket.gg parse errors: {:?}", parser.errors);

        let mut struct_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }
        assert!(struct_names.contains(&"Socket".to_string()));
        assert!(struct_names.contains(&"ServerSocket".to_string()));
        assert_eq!(equip_count, 4); // Socket (intrinsic + Writer + Reader) + ServerSocket
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
    fn generate_tls_returns_none() {
        // std.net.tls is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["std".into(), "net".into(), "tls".into()]).is_none());
    }

    #[test]
    fn tls_module_source_exists() {
        let source = builtin_module_source(&["std".into(), "net".into(), "tls".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("tls_connect"));
        assert!(src.contains("tls_server_bind"));
        assert!(src.contains("equip TlsSocket"));
        assert!(src.contains("equip TlsServerSocket"));
    }

    #[test]
    fn tls_source_parses() {
        let source = builtin_module_source(&["std".into(), "net".into(), "tls".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "tls.gg parse errors: {:?}", parser.errors);

        let mut struct_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }
        assert!(struct_names.contains(&"TlsSocket".to_string()));
        assert!(struct_names.contains(&"TlsServerSocket".to_string()));
        assert_eq!(equip_count, 4); // TlsSocket (intrinsic + Writer + Reader) + TlsServerSocket
    }

    #[test]
    fn http_is_file_based_module() {
        // xtd.http is file-based, so generate_builtin_module returns None
        assert!(generate_builtin_module(&["xtd".into(), "http".into()]).is_none());
        // but builtin_module_source returns the source
        assert!(builtin_module_source(&["xtd".into(), "http".into()]).is_some());
    }

    #[test]
    fn generate_regex_returns_none() {
        // xtd.regex is file-based, not synthetic — generate returns None
        assert!(generate_builtin_module(&["xtd".into(), "regex".into()]).is_none());
    }

    #[test]
    fn regex_module_source_exists() {
        let source = builtin_module_source(&["xtd".into(), "regex".into()]);
        assert!(source.is_some());
        let src = source.unwrap();
        assert!(src.contains("regex_compile"));
        assert!(src.contains("regex_escape"));
        assert!(src.contains("regex_is_match"));
        assert!(src.contains("equip Regex"));
        assert!(src.contains("equip Match"));
    }

    #[test]
    fn regex_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "regex".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "regex.gg parse errors: {:?}", parser.errors);

        let mut struct_names = vec![];
        let mut fn_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Function(f) => fn_names.push(f.name.node.clone()),
                Item::ExternBlock(eb) => {
                    for f in &eb.items {
                        fn_names.push(f.node.name.node.clone());
                    }
                }
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }
        assert!(struct_names.contains(&"Regex".to_string()));
        assert!(struct_names.contains(&"Match".to_string()));
        assert!(fn_names.contains(&"regex_compile".to_string()));
        assert!(fn_names.contains(&"regex_compile_with".to_string()));
        assert!(fn_names.contains(&"regex_escape".to_string()));
        assert!(fn_names.contains(&"regex_is_match".to_string()));
        assert!(fn_names.contains(&"regex_find".to_string()));
        assert!(fn_names.contains(&"regex_replace".to_string()));
        assert_eq!(equip_count, 2); // Regex + Match
    }

    #[test]
    fn crypto_source_parses() {
        let source = builtin_module_source(&["xtd".into(), "crypto".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let module = parser.parse_module();
        assert!(parser.errors.is_empty(), "crypto.gg parse errors: {:?}", parser.errors);

        let mut struct_names = vec![];
        let mut equip_count = 0;
        for item in &module.items {
            match &item.node {
                Item::Struct(s) => struct_names.push(s.name.node.clone()),
                Item::Equip(_) => equip_count += 1,
                _ => {}
            }
        }
        assert!(struct_names.contains(&"CipherContext".to_string()));
        assert!(struct_names.contains(&"BigNum".to_string()));
        assert!(struct_names.contains(&"RSAKey".to_string()));
        assert!(equip_count >= 3); // CipherContext + Ed25519KeyPair + X25519KeyPair
    }

    #[test]
    fn file_methods_are_extern() {
        let source = builtin_module_source(&["std".into(), "io".into()]).unwrap();
        let mut parser = crate::parser::Parser::new(source);
        let m = parser.parse_module();
        assert!(parser.errors.is_empty());
        for item in &m.items {
            if let Item::Equip(eq) = &item.node {
                if let Type::Named { name, .. } = &eq.type_.node {
                    if name.node == "File" {
                        let names: Vec<&str> =
                            eq.items.iter().map(|m| m.node.name.node.as_str()).collect();
                        assert!(names.contains(&"read_all"));
                        assert!(names.contains(&"close"));
                        assert!(names.contains(&"_write_bytes_buf"));
                        assert!(names.contains(&"_read_bytes_buf"));
                        return;
                    }
                }
            }
        }
        panic!("File equip block not found");
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
