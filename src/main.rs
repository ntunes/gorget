use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

mod tui;

use gorget::errors::ErrorReporter;
use gorget::lexer::Lexer;
use gorget::loader::{self, ModuleLoader};
use gorget::manifest::{self, DepSpec, Manifest};
use gorget::parser::ast::{Item, Module};
use gorget::parser::Parser;
use gorget::resolver;

/// Load imported modules and merge them into a single module.
/// Returns `(merged_module, concatenated_source)` where the concatenated source
/// covers all modules with offsets matching the spans in the merged AST.
fn load_imports(filename: &str, source: &str, module: gorget::parser::ast::Module, dep_paths: HashMap<String, PathBuf>) -> (gorget::parser::ast::Module, String) {
    let input_path = Path::new(filename).canonicalize().unwrap_or_else(|e| {
        eprintln!("Error resolving path {filename}: {e}");
        process::exit(1);
    });

    let mut ml = if dep_paths.is_empty() {
        ModuleLoader::new()
    } else {
        ModuleLoader::with_dep_paths(dep_paths)
    };
    let modules = ml
        .load_all(&input_path, source.to_string(), module)
        .unwrap_or_else(|e| {
            match &e {
                loader::LoadError::Parse {
                    path,
                    errors,
                    source,
                } => {
                    let reporter = ErrorReporter::new(
                        path.display().to_string(),
                        source.clone(),
                    );
                    for err in errors {
                        reporter.report_parse_error(err);
                    }
                    eprintln!(
                        "\n{} parse error(s) in '{}'",
                        errors.len(),
                        path.display()
                    );
                }
                _ => eprintln!("Error: {e}"),
            }
            process::exit(1);
        });

    // Build concatenated source text matching the span offsets assigned during loading.
    // Each module's source is separated by "\n" (matching the +1 offset gaps in the loader).
    let concat_source = modules.iter()
        .map(|(_, src, _)| src.as_str())
        .collect::<Vec<_>>()
        .join("\n");

    (loader::merge_modules(modules), concat_source)
}

/// Resolve package dependencies for a source file, returning dep_paths.
/// Looks for gorget.toml by walking up from the source file's directory.
fn resolve_deps_for_file(filename: &str) -> HashMap<String, PathBuf> {
    let input_path = Path::new(filename);
    let start_dir = input_path.parent().unwrap_or(Path::new("."));

    if let Some(project_root) = manifest::find_project_root(start_dir) {
        let manifest_path = project_root.join("gorget.toml");
        if let Ok(manifest) = Manifest::from_path(&manifest_path) {
            if !manifest.dependencies.is_empty() {
                match resolver::resolve(&project_root, &manifest) {
                    Ok(lockfile) => {
                        return resolver::build_dep_paths(&lockfile, &project_root);
                    }
                    Err(e) => {
                        eprintln!("Warning: dependency resolution failed: {e}");
                    }
                }
            }
        }
    }

    HashMap::new()
}


/// Check if the source text uses hot-reload mode.
/// Used to fall back to old codegen (GIR doesn't support hot-reload yet).
fn source_has_hot_reload(source: &str) -> bool {
    source.contains("directive hot-reload")
}

/// Check if the source text uses the trace directive.
/// Used to fall back to old codegen (GIR doesn't emit trace instrumentation yet).
fn source_has_trace(source: &str) -> bool {
    source.contains("directive trace")
}

/// Directive flags extracted from the source module.
struct DirectiveFlags {
    strip_asserts: bool,
    overflow_wrap: bool,
    trace: bool,
    hot_reload: bool,
}

/// Extract directive flags from a parsed module.
fn extract_directives(module: &Module) -> DirectiveFlags {
    let mut flags = DirectiveFlags {
        strip_asserts: false,
        overflow_wrap: false,
        trace: false,
        hot_reload: false,
    };
    for item in &module.items {
        if let Item::Directive(d) = &item.node {
            match d.name.as_str() {
                "strip-asserts" => flags.strip_asserts = true,
                "overflow" if d.value.as_deref() == Some("wrap") => flags.overflow_wrap = true,
                "trace" => flags.trace = true,
                "hot-reload" => flags.hot_reload = true,
                _ => {}
            }
        }
    }
    flags
}

/// Add SDL2 linker flags to a cc command.
fn add_sdl_flags(cmd: &mut Command, needs_sdl: bool) {
    if !needs_sdl { return; }
    let pkg_ok = Command::new("pkg-config")
        .args(["--cflags", "--libs", "sdl2", "SDL2_image", "SDL2_ttf"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(String::from_utf8_lossy(&o.stdout).to_string())
            } else {
                None
            }
        });
    if let Some(flags) = pkg_ok {
        for flag in flags.split_whitespace() {
            cmd.arg(flag);
        }
    } else {
        cmd.args(["-lSDL2", "-lSDL2_image", "-lSDL2_ttf"]);
        #[cfg(target_os = "macos")]
        {
            cmd.arg("-I/opt/homebrew/include");
            cmd.arg("-L/opt/homebrew/lib");
            cmd.arg("-I/usr/local/include");
            cmd.arg("-L/usr/local/lib");
        }
    }
}

/// Add OpenSSL linker flags to a cc command (for std.net.tls or std.crypto).
fn add_tls_flags(cmd: &mut Command, needs_tls: bool) {
    if !needs_tls { return; }
    add_crypto_flags(cmd, true);
}

/// Add OpenSSL linker flags to a cc command.
fn add_crypto_flags(cmd: &mut Command, needs_crypto: bool) {
    if !needs_crypto { return; }
    let pkg_ok = Command::new("pkg-config")
        .args(["--cflags", "--libs", "openssl"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(String::from_utf8_lossy(&o.stdout).to_string())
            } else {
                None
            }
        });
    if let Some(flags) = pkg_ok {
        for flag in flags.split_whitespace() {
            cmd.arg(flag);
        }
    } else {
        cmd.arg("-lssl");
        cmd.arg("-lcrypto");
        #[cfg(target_os = "macos")]
        {
            cmd.arg("-I/opt/homebrew/include");
            cmd.arg("-L/opt/homebrew/lib");
            cmd.arg("-I/usr/local/include");
            cmd.arg("-L/usr/local/lib");
        }
    }
}

/// Add pthread linker flags for programs that use `spawn`.
/// On macOS, pthreads are part of libc — no extra flag needed.
fn add_thread_flags(_cmd: &mut Command, needs_threads: bool) {
    if !needs_threads { return; }
    #[cfg(not(target_os = "macos"))]
    _cmd.arg("-lpthread");
}

/// Add PCRE2 linker flags to a cc command (for std.regex).
fn add_regex_flags(cmd: &mut Command, needs_regex: bool) {
    if !needs_regex { return; }
    let pkg_ok = Command::new("pkg-config")
        .args(["--cflags", "--libs", "libpcre2-8"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(String::from_utf8_lossy(&o.stdout).to_string())
            } else {
                None
            }
        });
    if let Some(flags) = pkg_ok {
        for flag in flags.split_whitespace() {
            cmd.arg(flag);
        }
    } else {
        cmd.arg("-lpcre2-8");
        #[cfg(target_os = "macos")]
        {
            cmd.arg("-I/opt/homebrew/include");
            cmd.arg("-L/opt/homebrew/lib");
            cmd.arg("-I/usr/local/include");
            cmd.arg("-L/usr/local/lib");
        }
    }
}

/// Print inferred borrow analysis for all functions (--show-borrows diagnostic).
fn print_borrow_summary(result: &gorget::semantic::AnalysisResult) {
    // Collect and sort by function name for stable output
    let mut entries: Vec<(_, &gorget::semantic::resolve::FunctionInfo)> =
        result.function_info.iter().collect();
    entries.sort_by(|(_, a), (_, b)| {
        let name_a = &result.scopes.get_def(a.def_id).name;
        let name_b = &result.scopes.get_def(b.def_id).name;
        name_a.cmp(name_b)
    });

    let mut has_output = false;
    for (_, info) in &entries {
        let def = result.scopes.get_def(info.def_id);
        let func_name = &def.name;

        if info.return_borrows_from.is_empty() {
            continue;
        }

        if !has_output {
            eprintln!("Borrow analysis:");
            has_output = true;
        }

        let sources: Vec<String> = info.return_borrows_from.iter().map(|&idx| {
            if idx < info.param_names.len() {
                info.param_names[idx].clone()
            } else {
                format!("param[{idx}]")
            }
        }).collect();
        eprintln!("  {func_name}() -> borrows from: {}", sources.join(", "));
    }
    if !has_output {
        eprintln!("Borrow analysis: no functions return borrowed data");
    }
}

/// Extract all `--feature <name>` (and `--feature=<name>`) values from CLI args.
fn parse_features(args: &[String]) -> Vec<String> {
    let mut features = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--feature" && i + 1 < args.len() {
            features.push(args[i + 1].clone());
            i += 2;
        } else if let Some(val) = args[i].strip_prefix("--feature=") {
            features.push(val.to_string());
            i += 1;
        } else {
            i += 1;
        }
    }
    features
}

/// Build a .gg source file into a binary. Returns the path to the executable,
/// or an error string if compilation fails.
///
/// `shared_output`: if Some, build as a shared library (.dylib/.so) at this path.
/// `hot_reload_flag`: if true, force hot-reload mode even without `directive hot-reload`.
fn try_build(
    filename: &str,
    source: &str,
    strip_asserts: bool,
    no_strip_asserts: bool,
    overflow_wrap: bool,
    overflow_checked: bool,
    trace: bool,
    no_trace: bool,
    test_mode: bool,
    test_tags: &[String],
    test_exclude_tags: &[String],
    test_name_filter: Option<&str>,
    output_dir: Option<&Path>,
    dep_paths: HashMap<String, PathBuf>,
    shared_output: Option<&Path>,
    hot_reload_flag: bool,
    show_borrows: bool,
    features: &[String],
) -> Result<PathBuf, String> {
    let mut parser = Parser::new(source);
    let module = parser.parse_module();

    if !parser.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), source.to_string());
        for err in &parser.errors {
            reporter.report_parse_error(err);
        }
        return Err(format!("{} parse error(s) found", parser.errors.len()));
    }

    // Load imported modules recursively and merge
    let (mut module, concat_source) = load_imports(filename, source, module, dep_paths);

    // Merge source directives with CLI flags.
    let dir_flags = extract_directives(&module);
    let strip_asserts = if no_strip_asserts {
        false
    } else {
        strip_asserts || dir_flags.strip_asserts
    };
    let overflow_wrap = if overflow_checked {
        false
    } else {
        overflow_wrap || dir_flags.overflow_wrap
    };
    let trace = if no_trace {
        false
    } else {
        trace || dir_flags.trace
    };
    let hot_reload = dir_flags.hot_reload || hot_reload_flag;

    let result = gorget::semantic::analyze(&mut module, features);

    if show_borrows {
        print_borrow_summary(&result);
    }

    if !result.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), concat_source.clone());
        for err in &result.errors {
            reporter.report_semantic_error(err);
        }
        return Err(format!("{} semantic error(s) found", result.errors.len()));
    }

    // Determine output paths
    let input_path = Path::new(filename);
    let default_dir = input_path.parent().unwrap_or(Path::new("."));
    let dir = output_dir.unwrap_or(default_dir);
    let stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");

    // Build trace filename: always next to the source file (not in output_dir)
    let trace_filename = if trace {
        let trace_path = default_dir.join(format!("{stem}.trace.jsonl"));
        let trace_path = std::path::absolute(&trace_path).unwrap_or(trace_path);
        trace_path.display().to_string()
    } else {
        String::new()
    };

    // Determine guest library name for hot-reload mode
    let guest_lib_name = format!("{stem}_guest");
    #[cfg(target_os = "macos")]
    let dylib_ext = ".dylib";
    #[cfg(not(target_os = "macos"))]
    let dylib_ext = ".so";

    // Compute the recompile command for hot-reload
    let abs_filename = std::path::absolute(Path::new(filename))
        .unwrap_or_else(|_| PathBuf::from(filename));
    let guest_lib_file = format!("{guest_lib_name}{dylib_ext}");
    let recompile_cmd = format!(
        "{} build --shared {} -o {}",
        std::env::current_exe().unwrap_or_else(|_| PathBuf::from("gg")).display(),
        abs_filename.display(),
        dir.join(&guest_lib_file).display(),
    );

    // Generate C code
    let codegen_output = gorget::codegen::generate_c(&module, &result, gorget::codegen::CodegenOptions {
        strip_asserts,
        overflow_wrap,
        trace,
        trace_filename,
        test_mode,
        test_tags: test_tags.to_vec(),
        test_exclude_tags: test_exclude_tags.to_vec(),
        test_name_filter: test_name_filter.map(|s| s.to_string()),
        source_text: concat_source,
        hot_reload,
        watch_paths: vec![abs_filename.display().to_string()],
        guest_lib_path: guest_lib_name.clone(),
        recompile_cmd: recompile_cmd.clone(),
    });
    let c_code = codegen_output.c_code;
    let needs_sdl = codegen_output.needs_sdl;
    let needs_tls = codegen_output.needs_tls;
    let needs_crypto = codegen_output.needs_crypto;
    let needs_regex = codegen_output.needs_regex;
    let needs_threads = codegen_output.needs_threads;
    let c_path = dir.join(format!("{stem}.c"));
    // Canonicalize to an absolute path so Command::new() doesn't search $PATH.
    // For a bare filename like "hello.gg", dir is "." and exe_path would be "hello",
    // which Unix interprets as a $PATH lookup rather than ./hello.
    let exe_path = dir.join(stem);
    let exe_path = std::path::absolute(&exe_path).unwrap_or(exe_path);

    // ── --shared: build as shared library (highest priority) ──
    if let Some(shared_path) = shared_output {
        // For --shared, emit the guest code (if available) or the full code
        let shared_c_code = codegen_output.guest_code.as_deref().unwrap_or(&c_code);
        let shared_c_path = dir.join(format!("{stem}_guest.c"));
        if let Err(e) = fs::write(&shared_c_path, shared_c_code) {
            return Err(format!("Error writing {}: {e}", shared_c_path.display()));
        }

        let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
        let mut cc_cmd = Command::new(&cc);
        cc_cmd
            .arg("-std=c11")
            .arg("-shared")
            .arg("-fPIC")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Wno-unused-parameter")
            .arg("-Wno-unused-variable")
            .arg("-Wno-unused-function")
            .arg("-o")
            .arg(shared_path)
            .arg(&shared_c_path)
            .arg("-lm");
        if overflow_wrap { cc_cmd.arg("-fwrapv"); }
        add_sdl_flags(&mut cc_cmd, needs_sdl);
        add_tls_flags(&mut cc_cmd, needs_tls);
        add_crypto_flags(&mut cc_cmd, needs_crypto);
        add_regex_flags(&mut cc_cmd, needs_regex);
        add_thread_flags(&mut cc_cmd, needs_threads);

        let status = cc_cmd.status();
        return match status {
            Ok(s) if s.success() => Ok(shared_path.to_path_buf()),
            Ok(s) => Err(format!("Shared library compilation failed: {s}\nGenerated: {}", shared_c_path.display())),
            Err(e) => Err(format!("Failed to run C compiler '{cc}': {e}")),
        };
    }

    // ── Hot-reload two-phase build ──────────────────────────────
    if hot_reload {
        let host_code = codegen_output.host_code.as_deref().unwrap_or(&c_code);
        let guest_code = codegen_output.guest_code.as_deref().unwrap_or(&c_code);

        let host_c_path = dir.join(format!("{stem}_host.c"));
        let guest_c_path = dir.join(format!("{stem}_guest.c"));
        let guest_lib_path = dir.join(&guest_lib_file);

        // Write host and guest C files
        if let Err(e) = fs::write(&host_c_path, host_code) {
            return Err(format!("Error writing {}: {e}", host_c_path.display()));
        }
        if let Err(e) = fs::write(&guest_c_path, guest_code) {
            return Err(format!("Error writing {}: {e}", guest_c_path.display()));
        }

        let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());

        // Phase 1: Compile guest as shared library
        let mut guest_cmd = Command::new(&cc);
        guest_cmd
            .arg("-std=c11")
            .arg("-shared")
            .arg("-fPIC")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Wno-unused-parameter")
            .arg("-Wno-unused-variable")
            .arg("-Wno-unused-function")
            .arg("-o")
            .arg(&guest_lib_path)
            .arg(&guest_c_path)
            .arg("-lm");
        if overflow_wrap { guest_cmd.arg("-fwrapv"); }
        add_sdl_flags(&mut guest_cmd, needs_sdl);
        add_tls_flags(&mut guest_cmd, needs_tls);
        add_crypto_flags(&mut guest_cmd, needs_crypto);
        add_regex_flags(&mut guest_cmd, needs_regex);
        add_thread_flags(&mut guest_cmd, needs_threads);

        let guest_status = guest_cmd.status();
        match guest_status {
            Ok(s) if s.success() => {}
            Ok(s) => return Err(format!("Guest library compilation failed: {s}\nGenerated: {}", guest_c_path.display())),
            Err(e) => return Err(format!("Failed to run C compiler '{cc}': {e}")),
        }

        // Phase 2: Compile host as executable
        let mut host_cmd = Command::new(&cc);
        host_cmd
            .arg("-std=c11")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Wno-unused-parameter")
            .arg("-Wno-unused-variable")
            .arg("-Wno-unused-function")
            .arg("-o")
            .arg(&exe_path)
            .arg(&host_c_path)
            .arg("-lm");
        if overflow_wrap { host_cmd.arg("-fwrapv"); }
        add_sdl_flags(&mut host_cmd, needs_sdl);
        add_tls_flags(&mut host_cmd, needs_tls);
        add_crypto_flags(&mut host_cmd, needs_crypto);
        add_regex_flags(&mut host_cmd, needs_regex);
        add_thread_flags(&mut host_cmd, needs_threads);

        let host_status = host_cmd.status();
        return match host_status {
            Ok(s) if s.success() => Ok(exe_path),
            Ok(s) => Err(format!("Host compilation failed: {s}\nGenerated: {}", host_c_path.display())),
            Err(e) => Err(format!("Failed to run C compiler '{cc}': {e}")),
        };
    }

    // Write .c file (normal, non-hot-reload path)
    if let Err(e) = fs::write(&c_path, &c_code) {
        return Err(format!("Error writing {}: {e}", c_path.display()));
    }

    // Invoke C compiler
    let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let mut cc_cmd = Command::new(&cc);
    cc_cmd
        .arg("-std=c11")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-Wno-unused-function");
    if overflow_wrap {
        cc_cmd.arg("-fwrapv");
    }
    cc_cmd
        .arg("-o")
        .arg(&exe_path)
        .arg(&c_path)
        .arg("-lm");

    add_sdl_flags(&mut cc_cmd, needs_sdl);
    add_tls_flags(&mut cc_cmd, needs_tls);
    add_crypto_flags(&mut cc_cmd, needs_crypto);
    add_regex_flags(&mut cc_cmd, needs_regex);
    add_thread_flags(&mut cc_cmd, needs_threads);

    let status = cc_cmd.status();

    match status {
        Ok(s) if s.success() => Ok(exe_path),
        Ok(s) => {
            let mut msg = format!(
                "C compiler exited with: {s}\nGenerated C file: {}",
                c_path.display()
            );
            if needs_sdl {
                msg.push_str("\n\nHint: This program uses std.sdl which requires SDL2 development libraries.");
                msg.push_str("\nInstall them with:");
                msg.push_str("\n  macOS:   brew install sdl2 sdl2_image sdl2_ttf");
                msg.push_str("\n  Ubuntu:  apt install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev");
            }
            if needs_tls {
                msg.push_str("\n\nHint: This program uses std.net.tls which requires OpenSSL.");
                msg.push_str("\nInstall with:");
                msg.push_str("\n  macOS:   brew install openssl");
                msg.push_str("\n  Ubuntu:  apt install libssl-dev");
            }
            if needs_crypto {
                msg.push_str("\n\nHint: This program uses std.crypto which requires OpenSSL.");
                msg.push_str("\nInstall with:");
                msg.push_str("\n  macOS:   brew install openssl");
                msg.push_str("\n  Ubuntu:  apt install libssl-dev");
            }
            if needs_regex {
                msg.push_str("\n\nHint: This program uses std.regex which requires PCRE2.");
                msg.push_str("\nInstall with:");
                msg.push_str("\n  macOS:   brew install pcre2");
                msg.push_str("\n  Ubuntu:  apt install libpcre2-dev");
            }
            Err(msg)
        }
        Err(e) => Err(format!(
            "Failed to run C compiler '{cc}': {e}\nGenerated C file: {}",
            c_path.display()
        )),
    }
}

/// Build a .gg source file into a binary. Exits the process on error.
fn build(
    filename: &str,
    source: &str,
    strip_asserts: bool,
    no_strip_asserts: bool,
    overflow_wrap: bool,
    overflow_checked: bool,
    trace: bool,
    no_trace: bool,
    test_mode: bool,
    test_tags: &[String],
    test_exclude_tags: &[String],
    test_name_filter: Option<&str>,
    output_dir: Option<&Path>,
    dep_paths: HashMap<String, PathBuf>,
    features: &[String],
) -> PathBuf {
    try_build(filename, source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, test_mode, test_tags, test_exclude_tags, test_name_filter, output_dir, dep_paths, None, false, false, features)
        .unwrap_or_else(|e| {
            eprintln!("{e}");
            process::exit(1);
        })
}

/// Build a .gg source file using the GIR pipeline (--ir mode).
/// Parses, analyzes, lowers to GIR, generates C via the GIR backend, compiles to binary.
fn try_build_ir(
    filename: &str,
    source: &str,
    dep_paths: HashMap<String, PathBuf>,
    output_dir: Option<&Path>,
    output_exe: Option<&Path>,
    features: &[String],
    options: gorget::ir::lowering::LoweringOptions,
) -> Result<PathBuf, String> {
    let mut parser = Parser::new(source);
    let module = parser.parse_module();

    if !parser.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), source.to_string());
        for err in &parser.errors {
            reporter.report_parse_error(err);
        }
        return Err(format!("{} parse error(s) found", parser.errors.len()));
    }

    // Load imported modules recursively and merge
    let (mut module, concat_source) = load_imports(filename, source, module, dep_paths);

    let result = gorget::semantic::analyze(&mut module, features);

    if !result.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), concat_source);
        for err in &result.errors {
            reporter.report_semantic_error(err);
        }
        return Err(format!("{} semantic error(s) found", result.errors.len()));
    }

    // Lower AST to GIR
    let gir_module = gorget::ir::lowering::lower_module(&module, &result, &options);

    // Generate C from GIR
    let c_code = gorget::backend::c::generate_c(&gir_module);

    // Determine output paths
    let (c_path, exe_path) = if let Some(out) = output_exe {
        let out = std::path::absolute(out).unwrap_or(out.to_path_buf());
        let c_path = out.with_extension("c");
        (c_path, out)
    } else {
        let input_path = Path::new(filename);
        let default_dir = input_path.parent().unwrap_or(Path::new("."));
        let dir = output_dir.unwrap_or(default_dir);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");

        let c_path = dir.join(format!("{stem}.c"));
        let exe_path = dir.join(stem);
        let exe_path = std::path::absolute(&exe_path).unwrap_or(exe_path);
        (c_path, exe_path)
    };

    // Write .c file
    if let Err(e) = fs::write(&c_path, &c_code) {
        return Err(format!("Error writing {}: {e}", c_path.display()));
    }

    // Invoke C compiler
    let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let mut cc_cmd = Command::new(&cc);
    cc_cmd
        .arg("-std=c11")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-Wno-unused-function")
        .arg("-Wno-unused-label")
        .arg("-o")
        .arg(&exe_path)
        .arg(&c_path)
        .arg("-lm");

    // Overflow wrap: pass -fwrapv so C integer overflow wraps instead of UB.
    if options.overflow_wrap || gir_module.overflow_wrap {
        cc_cmd.arg("-fwrapv");
    }

    // Detect library dependencies from source imports
    let needs_crypto = source.contains("std.crypto") || source.contains("std.net.tls")
        || source.contains("std.p2p");
    add_crypto_flags(&mut cc_cmd, needs_crypto);

    let needs_regex = source.contains("std.regex");
    add_regex_flags(&mut cc_cmd, needs_regex);

    // Add pthread for async/p2p
    if source.contains("std.async") || source.contains("std.p2p") {
        cc_cmd.arg("-lpthread");
    }

    let status = cc_cmd.status();

    match status {
        Ok(s) if s.success() => Ok(exe_path),
        Ok(s) => Err(format!(
            "C compiler exited with: {s}\nGenerated C file: {}",
            c_path.display()
        )),
        Err(e) => Err(format!(
            "Failed to run C compiler '{cc}': {e}\nGenerated C file: {}",
            c_path.display()
        )),
    }
}

/// Returns true if a line starts a top-level definition (function, struct, enum, etc.)
fn is_definition_line(line: &str) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return false;
    }
    // Keywords that always start definitions
    let def_keywords = [
        "struct ", "enum ", "trait ", "equip ", "import ", "directive ", "fn ",
        "test ", "suite ",
    ];
    for kw in &def_keywords {
        if trimmed.starts_with(kw) {
            return true;
        }
    }
    // Function signature: `<type> <name>(` — at least two words before a paren
    // e.g. "int double(int x) = x * 2" or "void greet(str name):"
    if let Some(paren_pos) = trimmed.find('(') {
        let before_paren = &trimmed[..paren_pos];
        let words: Vec<&str> = before_paren.split_whitespace().collect();
        if words.len() >= 2 {
            // Check the second-to-last word looks like an identifier (the function name)
            let name = words[words.len() - 1];
            if name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                return true;
            }
        }
    }
    false
}

/// Returns true if a definition block defines `main()`.
fn is_main_def(def: &str) -> bool {
    let first_line = def.lines().next().unwrap_or("");
    let trimmed = first_line.trim();
    // Match "void main():" or "void main():" with any whitespace
    if let Some(paren_pos) = trimmed.find('(') {
        let before_paren = &trimmed[..paren_pos];
        let words: Vec<&str> = before_paren.split_whitespace().collect();
        if words.len() >= 2 && words[words.len() - 1] == "main" {
            return true;
        }
    }
    false
}

/// Generate .gg source from accumulated TUI state.
fn generate_tui_source(definitions: &[String], statements: &[String]) -> String {
    let has_user_main = definitions.iter().any(|d| is_main_def(d));

    let mut source = String::new();
    for d in definitions {
        source.push_str(d);
        source.push('\n');
    }

    if has_user_main {
        // User defined main() — don't generate a wrapper.
        // Loose statements can't be included (they'd be top-level).
        return source;
    }

    if !definitions.is_empty() {
        source.push('\n');
    }
    source.push_str("void main():\n");
    if statements.is_empty() {
        source.push_str("    pass\n");
    } else {
        for s in statements {
            for line in s.lines() {
                source.push_str("    ");
                source.push_str(line);
                source.push('\n');
            }
        }
    }
    source
}

/// Interactive TUI for Gorget.
fn run_tui() {
    let version = env!("CARGO_PKG_VERSION");
    println!("Gorget {version}");
    println!("Type code, then /run to execute. /help for commands.\n");

    let mut definitions: Vec<String> = Vec::new(); // top-level defs (functions, structs, etc.)
    let mut statements: Vec<String> = Vec::new();  // statements inside main()

    // Create temp directory
    let tmp_dir = env::temp_dir().join("gorget_tui");
    let _ = fs::create_dir_all(&tmp_dir);

    loop {
        let line = match tui::read_line(">>> ", true) {
            tui::ReadLineResult::Line(l) => l,
            tui::ReadLineResult::Eof => { println!(); break; }
        };

        let trimmed = line.trim();

        // Handle special commands
        if trimmed == "/quit" || trimmed == "/exit" {
            break;
        }
        if trimmed == "/reset" {
            definitions.clear();
            statements.clear();
            println!("State cleared.");
            continue;
        }
        if trimmed == "/show" {
            if !definitions.is_empty() {
                println!("--- definitions ---");
                for d in &definitions {
                    println!("{d}");
                }
            }
            if !statements.is_empty() {
                println!("--- statements ---");
                for s in &statements {
                    println!("{s}");
                }
            }
            if definitions.is_empty() && statements.is_empty() {
                println!("(empty)");
            }
            continue;
        }
        if trimmed == "/run" {
            if definitions.is_empty() && statements.is_empty() {
                println!("(nothing to run)");
                continue;
            }
            let source = generate_tui_source(&definitions, &statements);
            let gg_path = tmp_dir.join("tui.gg");
            if let Err(e) = fs::write(&gg_path, &source) {
                eprintln!("Error writing temp file: {e}");
                continue;
            }
            let gg_path_str = gg_path.display().to_string();
            match try_build(&gg_path_str, &source, false, false, false, false, false, false, false, &[], &[], None, Some(&tmp_dir), HashMap::new(), None, false, false, &[]) {
                Err(e) => {
                    eprintln!("{e}");
                }
                Ok(exe_path) => {
                    match Command::new(&exe_path).output() {
                        Ok(output) => {
                            let stdout = String::from_utf8_lossy(&output.stdout);
                            if !stdout.is_empty() {
                                print!("{stdout}");
                            }
                            let stderr = String::from_utf8_lossy(&output.stderr);
                            if !stderr.is_empty() {
                                eprint!("{stderr}");
                            }
                        }
                        Err(e) => {
                            eprintln!("Failed to execute: {e}");
                        }
                    }
                }
            }
            continue;
        }
        if trimmed == "/check" {
            if definitions.is_empty() && statements.is_empty() {
                println!("(nothing to check)");
                continue;
            }
            let source = generate_tui_source(&definitions, &statements);
            let gg_path = tmp_dir.join("tui.gg");
            if let Err(e) = fs::write(&gg_path, &source) {
                eprintln!("Error writing temp file: {e}");
                continue;
            }
            let gg_path_str = gg_path.display().to_string();
            match try_build(&gg_path_str, &source, false, false, false, false, false, false, false, &[], &[], None, Some(&tmp_dir), HashMap::new(), None, false, false, &[]) {
                Err(e) => {
                    eprintln!("{e}");
                }
                Ok(_) => {
                    println!("OK: no errors");
                }
            }
            continue;
        }
        if trimmed == "/help" {
            println!("/run    — compile and run the accumulated code");
            println!("/check  — compile and show any errors");
            println!("/show   — show accumulated code");
            println!("/reset  — clear accumulated code");
            println!("/quit   — exit (also Ctrl+D)");
            println!("/help   — show this help");
            println!();
            println!("Type definitions and statements, then /run to execute.");
            println!("Lines ending with ':' start indented blocks (blank line ends block).");
            continue;
        }

        if trimmed.is_empty() {
            continue;
        }

        // Collect the input block
        let mut block_lines: Vec<String> = vec![trimmed.to_string()];

        // If line ends with ':', read continuation lines
        if trimmed.ends_with(':') {
            loop {
                let cont_line = match tui::read_line("... ", false) {
                    tui::ReadLineResult::Line(l) => l,
                    tui::ReadLineResult::Eof => break,
                };
                let cont = cont_line.trim_end();
                // Blank line or unindented line ends the block
                if cont.is_empty() {
                    break;
                }
                if !cont.starts_with(' ') && !cont.starts_with('\t') {
                    block_lines.push(cont.to_string());
                    break;
                }
                block_lines.push(cont.to_string());
            }
        }

        let entry = block_lines.join("\n");

        // Classify and append: definition or statement
        if is_definition_line(&block_lines[0]) {
            definitions.push(entry);
        } else {
            statements.push(entry);
        }
    }

    // Cleanup temp directory (best-effort)
    let _ = fs::remove_dir_all(&tmp_dir);
}

// ══════════════════════════════════════════════════════════════
// Package management commands
// ══════════════════════════════════════════════════════════════

/// `gg init` — initialize a new Gorget project in the current directory.
fn cmd_init() {
    let cwd = env::current_dir().unwrap_or_else(|e| {
        eprintln!("Cannot determine current directory: {e}");
        process::exit(1);
    });

    let manifest_path = cwd.join("gorget.toml");
    if manifest_path.exists() {
        eprintln!("gorget.toml already exists");
        process::exit(1);
    }

    let name = cwd
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("my-project")
        .to_string();

    let manifest = Manifest::new(&name);
    manifest.save(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error writing gorget.toml: {e}");
        process::exit(1);
    });
    println!("Created gorget.toml");

    // Create main.gg if it doesn't exist
    let main_path = cwd.join("main.gg");
    if !main_path.exists() {
        fs::write(&main_path, "void main():\n    print(\"hello\")\n").unwrap_or_else(|e| {
            eprintln!("Error writing main.gg: {e}");
            process::exit(1);
        });
        println!("Created main.gg");
    }

    // Create .gitignore if it doesn't exist
    let gitignore_path = cwd.join(".gitignore");
    if !gitignore_path.exists() {
        fs::write(&gitignore_path, "# Build artifacts\n*.c\n*.o\n/target/\n").unwrap_or_else(|e| {
            eprintln!("Error writing .gitignore: {e}");
            process::exit(1);
        });
        println!("Created .gitignore");
    }
}

/// `gg new <name>` — create a new project directory and initialize it.
fn cmd_new(name: &str) {
    let project_dir = PathBuf::from(name);
    if project_dir.exists() {
        eprintln!("Directory '{name}' already exists");
        process::exit(1);
    }

    fs::create_dir_all(&project_dir).unwrap_or_else(|e| {
        eprintln!("Cannot create directory '{name}': {e}");
        process::exit(1);
    });

    // Change to the new directory and run init
    env::set_current_dir(&project_dir).unwrap_or_else(|e| {
        eprintln!("Cannot enter directory '{name}': {e}");
        process::exit(1);
    });

    cmd_init();
    println!("Created project '{name}'");
}

/// `gg add <name> --git <url> [--tag <tag>] [--branch <branch>]`
/// `gg add <name> --path <dir>`
fn cmd_add(args: &[String]) {
    let name = &args[0];
    let mut git_url: Option<String> = None;
    let mut tag: Option<String> = None;
    let mut branch: Option<String> = None;
    let mut dep_path: Option<PathBuf> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--git" if i + 1 < args.len() => {
                git_url = Some(args[i + 1].clone());
                i += 2;
            }
            "--tag" if i + 1 < args.len() => {
                tag = Some(args[i + 1].clone());
                i += 2;
            }
            "--branch" if i + 1 < args.len() => {
                branch = Some(args[i + 1].clone());
                i += 2;
            }
            "--path" if i + 1 < args.len() => {
                dep_path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            }
            other => {
                eprintln!("Unknown flag: {other}");
                process::exit(1);
            }
        }
    }

    let spec = if let Some(path) = dep_path {
        DepSpec::Path { path }
    } else if let Some(git) = git_url {
        DepSpec::Git {
            git,
            tag,
            branch,
            rev: None,
        }
    } else {
        eprintln!("Must specify --git <url> or --path <dir>");
        process::exit(1);
    };

    let cwd = env::current_dir().unwrap_or_else(|e| {
        eprintln!("Cannot determine current directory: {e}");
        process::exit(1);
    });

    let manifest_path = cwd.join("gorget.toml");
    let mut manifest = Manifest::from_path(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error reading gorget.toml: {e}");
        process::exit(1);
    });

    manifest.dependencies.insert(name.clone(), spec);
    manifest.save(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error writing gorget.toml: {e}");
        process::exit(1);
    });

    // Resolve to update lockfile and fetch
    match resolver::resolve(&cwd, &manifest) {
        Ok(_) => println!("Added '{name}'"),
        Err(e) => {
            eprintln!("Error resolving dependency '{name}': {e}");
            process::exit(1);
        }
    }
}

/// `gg remove <name>` — remove a dependency.
fn cmd_remove(name: &str) {
    let cwd = env::current_dir().unwrap_or_else(|e| {
        eprintln!("Cannot determine current directory: {e}");
        process::exit(1);
    });

    let manifest_path = cwd.join("gorget.toml");
    let mut manifest = Manifest::from_path(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error reading gorget.toml: {e}");
        process::exit(1);
    });

    if manifest.dependencies.remove(name).is_none() {
        eprintln!("Dependency '{name}' not found in gorget.toml");
        process::exit(1);
    }

    manifest.save(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error writing gorget.toml: {e}");
        process::exit(1);
    });

    // Re-resolve to update lockfile
    match resolver::resolve(&cwd, &manifest) {
        Ok(_) => println!("Removed '{name}'"),
        Err(e) => {
            eprintln!("Error updating lockfile: {e}");
            process::exit(1);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // No args → launch interactive TUI
    if args.len() < 2 {
        run_tui();
        return;
    }

    // `gg --version` / `gg -V`
    if args[1] == "--version" || args[1] == "-V" {
        println!("gg {}", env!("CARGO_PKG_VERSION"));
        return;
    }

    // `gg --help` / `gg -h`
    if args[1] == "--help" || args[1] == "-h" {
        println!("Usage: gg <file.gg>              Run a script");
        println!("       gg <command> <file.gg>     Run a compiler command");
        println!("       gg                         Interactive TUI");
        println!("       gg --version               Print version");
        println!();
        println!("Compiler commands: lex, parse, check, build, run, fmt, test, report");
        println!("Package commands:  init, new, add, remove");
        println!();
        println!("Build flags:");
        println!("  --hot-reload            Enable hot code reload (builds host + guest .dylib)");
        println!("  --shared [-o F]         Build as shared library (.dylib/.so)");
        println!("  --ir                    Alias for GIR pipeline (now the default)");
        return;
    }

    // `gg script.gg` shorthand → treat as `gg run script.gg`
    if args[1].ends_with(".gg") {
        let filename = &args[1];
        let source = match fs::read_to_string(filename) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {filename}: {e}");
                process::exit(1);
            }
        };
        let strip_asserts = args.iter().any(|a| a == "--strip-asserts");
        let no_strip_asserts = args.iter().any(|a| a == "--no-strip-asserts");
        let overflow_wrap = args.iter().any(|a| a == "--overflow=wrap");
        let overflow_checked = args.iter().any(|a| a == "--overflow=checked");
        let trace = args.iter().any(|a| a == "--trace");
        let no_trace = args.iter().any(|a| a == "--no-trace");
        let dep_paths = resolve_deps_for_file(filename);
        let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
            eprintln!("Failed to create temp directory: {e}");
            process::exit(1);
        });
        let features = parse_features(&args);
        let legacy = args.iter().any(|a| a == "--legacy-codegen");
        let ir = args.iter().any(|a| a == "--ir");
        let use_legacy = legacy
            || source_has_hot_reload(&source)
            || trace;
        let exe_path = if !ir && use_legacy {
            // Fall back to old codegen for unsupported features or explicit --legacy-codegen
            build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, false, &[], &[], None, Some(tmp_dir.path()), dep_paths, &features)
        } else {
            // Default: GIR pipeline
            let lowering_opts = gorget::ir::lowering::LoweringOptions {
                strip_asserts,
                no_strip_asserts,
                overflow_wrap,
                overflow_checked,
                ..Default::default()
            };
            try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, &features, lowering_opts)
                .unwrap_or_else(|e| {
                    eprintln!("{e}");
                    process::exit(1);
                })
        };
        let status = Command::new(&exe_path)
            .status()
            .unwrap_or_else(|e| {
                eprintln!("Failed to execute {}: {e}", exe_path.display());
                process::exit(1);
            });
        process::exit(status.code().unwrap_or(1));
    }

    // `gg report <file>.trace.jsonl` — generate HTML report from trace
    if args[1] == "report" {
        if args.len() < 3 {
            eprintln!("Usage: gg report <file.trace.jsonl> [--output <path>]");
            process::exit(1);
        }
        let trace_file = args.iter().skip(2).find(|a| !a.starts_with("--")).unwrap_or_else(|| {
            eprintln!("Usage: gg report <file.trace.jsonl> [--output <path>]");
            process::exit(1);
        });
        let trace_path = Path::new(trace_file);
        // Parse optional --output
        let output_path = {
            let mut out: Option<PathBuf> = None;
            let mut i = 2;
            while i < args.len() {
                if args[i] == "--output" && i + 1 < args.len() {
                    out = Some(PathBuf::from(&args[i + 1]));
                    i += 2;
                } else if args[i].starts_with("--output=") {
                    out = Some(PathBuf::from(&args[i]["--output=".len()..]));
                    i += 1;
                } else {
                    i += 1;
                }
            }
            out.unwrap_or_else(|| {
                // Default: <stem>.report.html in the same directory
                let stem = trace_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                // Strip .trace suffix if present (e.g. "test_basic.trace" -> "test_basic")
                let stem = stem.strip_suffix(".trace").unwrap_or(stem);
                let dir = trace_path.parent().unwrap_or(Path::new("."));
                dir.join(format!("{stem}.report.html"))
            })
        };
        gorget::report::generate_html_report(trace_path, &output_path)
            .unwrap_or_else(|e| {
                eprintln!("{e}");
                process::exit(1);
            });
        println!("Report: {}", output_path.display());
        return;
    }

    // ── Package management commands ────────────────────────────

    // `gg init`
    if args[1] == "init" {
        cmd_init();
        return;
    }

    // `gg new <name>`
    if args[1] == "new" {
        if args.len() < 3 {
            eprintln!("Usage: gg new <name>");
            process::exit(1);
        }
        cmd_new(&args[2]);
        return;
    }

    // `gg add <name> --git <url> [--tag <tag>] [--branch <branch>]`
    // `gg add <name> --path <dir>`
    if args[1] == "add" {
        if args.len() < 4 {
            eprintln!("Usage: gg add <name> --git <url> [--tag <tag>] [--branch <branch>]");
            eprintln!("       gg add <name> --path <dir>");
            process::exit(1);
        }
        cmd_add(&args[2..]);
        return;
    }

    // `gg remove <name>`
    if args[1] == "remove" {
        if args.len() < 3 {
            eprintln!("Usage: gg remove <name>");
            process::exit(1);
        }
        cmd_remove(&args[2]);
        return;
    }

    if args.len() < 3 {
        eprintln!("Usage: gg <file.gg>              Run a script");
        eprintln!("       gg <command> <file.gg>     Run a compiler command");
        eprintln!("       gg                         Interactive REPL");
        eprintln!("       gg --version               Print version");
        eprintln!("Compiler commands: lex, parse, check, build, run, fmt, test, report");
        eprintln!("Package commands:  init, new, add, remove");
        process::exit(1);
    }

    let command = &args[1];
    let strip_asserts = args.iter().any(|a| a == "--strip-asserts");
    let no_strip_asserts = args.iter().any(|a| a == "--no-strip-asserts");
    let overflow_wrap = args.iter().any(|a| a == "--overflow=wrap");
    let overflow_checked = args.iter().any(|a| a == "--overflow=checked");
    let trace = args.iter().any(|a| a == "--trace");
    let no_trace = args.iter().any(|a| a == "--no-trace");
    let hot_reload_flag = args.iter().any(|a| a == "--hot-reload");
    let shared_mode = args.iter().any(|a| a == "--shared");
    let show_borrows = args.iter().any(|a| a == "--show-borrows");
    let ir_mode = args.iter().any(|a| a == "--ir");
    // --legacy-codegen: bypass GIR, use old AST→C codegen directly.
    // GIR is now the default; this flag is provided for programs that GIR
    // doesn't yet support (async, hot-reload) or for debugging.
    let legacy_codegen = args.iter().any(|a| a == "--legacy-codegen");
    let features = parse_features(&args);
    // Parse -o <path> for shared output
    let shared_output_path: Option<PathBuf> = {
        let mut path = None;
        let mut i = 0;
        while i < args.len() {
            if args[i] == "-o" && i + 1 < args.len() {
                path = Some(PathBuf::from(&args[i + 1]));
                i += 2;
            } else {
                i += 1;
            }
        }
        path
    };
    // Find positional filename, skipping values of known flag pairs
    let filename = {
        let flags_with_values = ["--tag", "--exclude-tag", "--filter", "--report", "--output", "-o", "--feature"];
        let mut skip_next = false;
        let mut found = None;
        for arg in args.iter().skip(2) {
            if skip_next {
                skip_next = false;
                continue;
            }
            if flags_with_values.contains(&arg.as_str()) {
                skip_next = true;
                continue;
            }
            if arg.starts_with("--") {
                continue;
            }
            found = Some(arg);
            break;
        }
        found.unwrap_or_else(|| {
            eprintln!("Usage: gg <command> <file.gg>");
            process::exit(1);
        })
    };

    let source = match fs::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {filename}: {e}");
            process::exit(1);
        }
    };

    match command.as_str() {
        "lex" => {
            let lexer = Lexer::new(&source);
            for spanned_token in lexer {
                println!(
                    "  [{:>4}..{:<4}] {:?}",
                    spanned_token.span.start, spanned_token.span.end, spanned_token.node
                );
            }
        }
        "parse" => {
            let mut parser = Parser::new(&source);
            let module = parser.parse_module();

            if !parser.errors.is_empty() {
                let reporter = ErrorReporter::new(filename.clone(), source.clone());
                for err in &parser.errors {
                    reporter.report_parse_error(err);
                }
                eprintln!("\n{} error(s) found", parser.errors.len());
                process::exit(1);
            }

            println!("{module:#?}");
        }
        "check" => {
            let mut parser = Parser::new(&source);
            let module = parser.parse_module();

            if !parser.errors.is_empty() {
                let reporter = ErrorReporter::new(filename.clone(), source.clone());
                for err in &parser.errors {
                    reporter.report_parse_error(err);
                }
                eprintln!("\n{} parse error(s) found", parser.errors.len());
                process::exit(1);
            }

            // Load imported modules recursively and merge
            let dep_paths = resolve_deps_for_file(filename);
            let (mut module, concat_source) = load_imports(filename, &source, module, dep_paths);

            let result = gorget::semantic::analyze(&mut module, &features);

            if show_borrows {
                print_borrow_summary(&result);
            }

            if result.errors.is_empty() {
                println!("OK: no semantic errors");
            } else {
                let reporter = ErrorReporter::new(filename.clone(), concat_source);
                for err in &result.errors {
                    reporter.report_semantic_error(err);
                }
                eprintln!("\n{} error(s) found", result.errors.len());
                process::exit(1);
            }
        }
        "build" => {
            let dep_paths = resolve_deps_for_file(filename);
            if shared_mode {
                // --shared: build as shared library (old codegen; GIR doesn't split host/guest yet)
                let default_shared_path = {
                    let input_path = Path::new(filename);
                    let dir = input_path.parent().unwrap_or(Path::new("."));
                    let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                    #[cfg(target_os = "macos")]
                    let ext = ".dylib";
                    #[cfg(not(target_os = "macos"))]
                    let ext = ".so";
                    dir.join(format!("{stem}_guest{ext}"))
                };
                let shared_path = shared_output_path.as_deref().unwrap_or(&default_shared_path);
                let result = try_build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, false, &[], &[], None, None, dep_paths, Some(shared_path), false, show_borrows, &features);
                match result {
                    Ok(p) => println!("Built shared library: {}", p.display()),
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            } else if hot_reload_flag || source_has_hot_reload(&source) {
                // hot-reload: two-phase build (host + guest); old codegen (GIR deferred)
                let result = try_build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, false, &[], &[], None, None, dep_paths, None, true, show_borrows, &features);
                match result {
                    Ok(p) => {
                        // If -o was given, copy the host binary there so callers like test_ir.py
                        // find it at the expected path (GIR pipeline uses -o for its output).
                        if let Some(ref output_path) = shared_output_path {
                            if let Err(e) = fs::copy(&p, output_path) {
                                eprintln!("Warning: could not copy binary to {}: {e}", output_path.display());
                            }
                        }
                        println!("Built (hot-reload): {}", p.display());
                    }
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            } else if !ir_mode && (legacy_codegen || trace || source_has_trace(&source)) {
                // Legacy codegen: explicit request, or features GIR doesn't yet support (--trace)
                let exe_path = build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, false, &[], &[], None, None, dep_paths, &features);
                println!("Built: {}", exe_path.display());
            } else {
                // Default: GIR pipeline
                let lowering_opts = gorget::ir::lowering::LoweringOptions {
                    strip_asserts,
                    no_strip_asserts,
                    overflow_wrap,
                    overflow_checked,
                    ..Default::default()
                };
                let result = try_build_ir(filename, &source, dep_paths, None, shared_output_path.as_deref(), &features, lowering_opts);
                match result {
                    Ok(p) => println!("Built: {}", p.display()),
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            }
        }
        "run" => {
            let dep_paths = resolve_deps_for_file(filename);
            if hot_reload_flag || source_has_hot_reload(&source) {
                // hot-reload: build with hot-reload and run (old codegen; GIR deferred)
                let result = try_build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, false, &[], &[], None, None, dep_paths, None, true, show_borrows, &features);
                match result {
                    Ok(exe_path) => {
                        let status = Command::new(&exe_path)
                            .status()
                            .unwrap_or_else(|e| {
                                eprintln!("Failed to execute {}: {e}", exe_path.display());
                                process::exit(1);
                            });
                        process::exit(status.code().unwrap_or(1));
                    }
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            } else if !ir_mode && (legacy_codegen || trace || source_has_trace(&source)) {
                // Legacy codegen: explicit request, or features GIR doesn't yet support (--trace)
                let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
                    eprintln!("Failed to create temp directory: {e}");
                    process::exit(1);
                });
                let exe_path = build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked, trace, no_trace, false, &[], &[], None, Some(tmp_dir.path()), dep_paths, &features);
                let status = Command::new(&exe_path)
                    .status()
                    .unwrap_or_else(|e| {
                        eprintln!("Failed to execute {}: {e}", exe_path.display());
                        process::exit(1);
                    });
                process::exit(status.code().unwrap_or(1));
            } else {
                // Default: GIR pipeline
                let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
                    eprintln!("Failed to create temp directory: {e}");
                    process::exit(1);
                });
                let lowering_opts = gorget::ir::lowering::LoweringOptions {
                    strip_asserts,
                    no_strip_asserts,
                    overflow_wrap,
                    overflow_checked,
                    ..Default::default()
                };
                let result = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, &features, lowering_opts);
                match result {
                    Ok(exe_path) => {
                        let status = Command::new(&exe_path)
                            .status()
                            .unwrap_or_else(|e| {
                                eprintln!("Failed to execute {}: {e}", exe_path.display());
                                process::exit(1);
                            });
                        process::exit(status.code().unwrap_or(1));
                    }
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            }
        }
        "test" => {
            // Collect --tag, --exclude-tag, --filter, --report values
            let mut test_tags = Vec::new();
            let mut test_exclude_tags = Vec::new();
            let mut test_name_filter: Option<String> = None;
            let mut report_html = false;
            let mut i = 0;
            while i < args.len() {
                if args[i] == "--tag" && i + 1 < args.len() {
                    test_tags.push(args[i + 1].clone());
                    i += 2;
                } else if args[i].starts_with("--tag=") {
                    test_tags.push(args[i]["--tag=".len()..].to_string());
                    i += 1;
                } else if args[i] == "--exclude-tag" && i + 1 < args.len() {
                    test_exclude_tags.push(args[i + 1].clone());
                    i += 2;
                } else if args[i].starts_with("--exclude-tag=") {
                    test_exclude_tags.push(args[i]["--exclude-tag=".len()..].to_string());
                    i += 1;
                } else if args[i] == "--filter" && i + 1 < args.len() {
                    test_name_filter = Some(args[i + 1].clone());
                    i += 2;
                } else if args[i].starts_with("--filter=") {
                    test_name_filter = Some(args[i]["--filter=".len()..].to_string());
                    i += 1;
                } else if args[i] == "--report" && i + 1 < args.len() && args[i + 1] == "html" {
                    report_html = true;
                    i += 2;
                } else if args[i] == "--report=html" {
                    report_html = true;
                    i += 1;
                } else {
                    i += 1;
                }
            }
            // --report html implies --trace (unless --no-trace is explicit)
            let trace = if report_html && !no_trace { true } else { trace };
            let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
                eprintln!("Failed to create temp directory: {e}");
                process::exit(1);
            });
            let dep_paths = resolve_deps_for_file(filename);
            // Use GIR pipeline when trace is not needed; fall back to legacy for --trace/--report html.
            let exe_path = if !trace {
                let lowering_opts = gorget::ir::lowering::LoweringOptions {
                    test_mode: true,
                    test_tags: test_tags.clone(),
                    test_exclude_tags: test_exclude_tags.clone(),
                    test_name_filter: test_name_filter.clone(),
                    ..Default::default()
                };
                try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, &features, lowering_opts)
                    .unwrap_or_else(|e| {
                        eprintln!("{e}");
                        process::exit(1);
                    })
            } else {
                build(filename, &source, false, false, false, false, trace, no_trace, true, &test_tags, &test_exclude_tags, test_name_filter.as_deref(), Some(tmp_dir.path()), dep_paths, &features)
            };
            let status = Command::new(&exe_path)
                .status()
                .unwrap_or_else(|e| {
                    eprintln!("Failed to execute {}: {e}", exe_path.display());
                    process::exit(1);
                });
            // Generate HTML report if requested
            if report_html && trace {
                let input_path = Path::new(filename);
                let source_dir = input_path.parent().unwrap_or(Path::new("."));
                let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                let trace_path = source_dir.join(format!("{stem}.trace.jsonl"));
                let report_path = source_dir.join(format!("{stem}.report.html"));
                if trace_path.exists() {
                    gorget::report::generate_html_report(&trace_path, &report_path)
                        .unwrap_or_else(|e| {
                            eprintln!("Report generation failed: {e}");
                        });
                    println!("Report: {}", report_path.display());
                }
            }
            // tmp_dir is dropped here, cleaning up .c, binary, and trace
            process::exit(status.code().unwrap_or(1));
        }
        "fmt" => {
            let in_place = args.iter().any(|a| a == "--in-place" || a == "-i");
            let formatted = gorget::formatter::format_source(&source);
            if in_place {
                if let Err(e) = fs::write(filename, &formatted) {
                    eprintln!("Error writing {filename}: {e}");
                    process::exit(1);
                }
            } else {
                print!("{formatted}");
            }
        }
        _ => {
            eprintln!("Unknown command: {command}");
            eprintln!("Compiler commands: lex, parse, check, build, run, test, fmt, report");
            eprintln!("Package commands:  init, new, add, remove");
            process::exit(1);
        }
    }
}
