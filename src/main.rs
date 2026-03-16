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
use gorget::parser::Parser;
use gorget::resolver;

/// File info for multi-file error reporting: (display_name, source, base_offset).
type FileInfo = (String, String, usize);

/// Load imported modules and merge them into a single module.
/// Returns `(merged_module, file_infos)` where file_infos maps each module's
/// source to its filename and byte offset for accurate cross-file diagnostics.
fn load_imports(filename: &str, source: &str, module: gorget::parser::ast::Module, dep_paths: HashMap<String, PathBuf>) -> (gorget::parser::ast::Module, Vec<FileInfo>) {
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

    // Build file info for each module: (display_name, source, base_offset).
    // Offsets match the loader's assignment: entry at 0, each subsequent module
    // at previous_end + 1 (the +1 gap matches the loader's separator).
    let mut file_infos: Vec<FileInfo> = Vec::new();
    let mut offset = 0usize;
    for (path, _segments, src, _module) in &modules {
        let display_name = path.display().to_string();
        file_infos.push((display_name, src.clone(), offset));
        offset += src.len() + 1; // +1 for separator gap
    }

    (loader::merge_modules(modules), file_infos)
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
fn source_has_hot_reload(source: &str) -> bool {
    source.contains("directive hot-reload")
}

/// Check if the source text uses the trace directive.
fn source_has_trace(source: &str) -> bool {
    source.contains("directive trace")
}

/// Add SDL2 linker flags to a cc command.
fn add_sdl_flags(cmd: &mut Command, needs_sdl: bool, source: &str) {
    if !needs_sdl { return; }
    let needs_image = source.contains("GORGET_USE_SDL_IMAGE");
    let needs_ttf = source.contains("GORGET_USE_SDL_TTF");
    let mut pkg_args: Vec<&str> = vec!["--cflags", "--libs", "sdl2"];
    if needs_image { pkg_args.push("SDL2_image"); }
    if needs_ttf { pkg_args.push("SDL2_ttf"); }
    let pkg_ok = Command::new("pkg-config")
        .args(&pkg_args)
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
        cmd.arg("-lSDL2");
        if needs_image { cmd.arg("-lSDL2_image"); }
        if needs_ttf { cmd.arg("-lSDL2_ttf"); }
        #[cfg(target_os = "macos")]
        {
            cmd.arg("-I/opt/homebrew/include");
            cmd.arg("-L/opt/homebrew/lib");
            cmd.arg("-I/usr/local/include");
            cmd.arg("-L/usr/local/lib");
        }
    }
}

/// Add OpenSSL linker flags to a cc command (for std.net.tls or gg.crypto).
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

/// Add PCRE2 linker flags to a cc command (for gg.regex).
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

/// Add OpenGL linker flags to a cc command (for gg.gl).
fn add_gl_flags(cmd: &mut Command, needs_gl: bool) {
    if !needs_gl { return; }
    #[cfg(target_os = "macos")]
    {
        cmd.arg("-framework");
        cmd.arg("OpenGL");
    }
    #[cfg(not(target_os = "macos"))]
    cmd.arg("-lGL");
}

/// Add SDL2_mixer linker flags to a cc command (for gg.audio).
fn add_audio_flags(cmd: &mut Command, needs_audio: bool) {
    if !needs_audio { return; }
    let pkg_ok = Command::new("pkg-config")
        .args(["--cflags", "--libs", "SDL2_mixer"])
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
        cmd.arg("-lSDL2_mixer");
        #[cfg(target_os = "macos")]
        {
            cmd.arg("-I/opt/homebrew/include");
            cmd.arg("-L/opt/homebrew/lib");
            cmd.arg("-I/usr/local/include");
            cmd.arg("-L/usr/local/lib");
        }
    }
}

/// Add zlib linker flags to a cc command (for gg.compress).
fn add_compress_flags(cmd: &mut Command, needs_compress: bool) {
    if !needs_compress { return; }
    cmd.arg("-lz");
}

/// Add Metal framework linker flags to a cc command (for gg.metal).
fn add_metal_flags(cmd: &mut Command, needs_metal: bool) {
    if !needs_metal { return; }
    #[cfg(target_os = "macos")]
    {
        cmd.arg("-framework").arg("Metal");
        cmd.arg("-framework").arg("QuartzCore");
        cmd.arg("-framework").arg("Foundation");
    }
    #[cfg(not(target_os = "macos"))]
    let _ = cmd; // Metal is macOS-only; suppress unused warning
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

/// Extract `--scheduler=pool|thread|inline|single` from CLI args.
fn parse_scheduler(args: &[String]) -> Option<gorget::ir::SchedulerMode> {
    for a in args {
        if let Some(val) = a.strip_prefix("--scheduler=") {
            return match val {
                "pool" => Some(gorget::ir::SchedulerMode::Pool),
                "thread" => Some(gorget::ir::SchedulerMode::Thread),
                "inline" => Some(gorget::ir::SchedulerMode::Inline),
                "single" => Some(gorget::ir::SchedulerMode::Single),
                _ => {
                    eprintln!("Unknown scheduler mode: {val} (expected pool, thread, inline, single)");
                    None
                }
            };
        }
    }
    None
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

/// Build a .gg source file using the GIR pipeline (--ir mode).
/// Parses, analyzes, lowers to GIR, generates C via the GIR backend, compiles to binary.
fn try_build_ir(
    filename: &str,
    source: &str,
    dep_paths: HashMap<String, PathBuf>,
    output_dir: Option<&Path>,
    output_exe: Option<&Path>,
    shared_output: Option<&Path>,
    features: &[String],
    options: gorget::ir::lowering::LoweringOptions,
    emit_gir: bool,
    emit_lir: bool,
    emit_c_lir: bool,
    use_lir_backend: bool,
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

    // Emit parse-level warnings (e.g. deprecated syntax)
    if !parser.warnings.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), source.to_string());
        for warn in &parser.warnings {
            reporter.report_parse_warning(warn);
        }
    }

    // Load imported modules recursively and merge
    let (mut module, file_infos) = load_imports(filename, source, module, dep_paths);
    // Concatenated source for feature-flag detection (.contains() checks below).
    let concat_source: String = file_infos.iter().map(|(_, src, _)| src.as_str()).collect::<Vec<_>>().join("\n");

    let source_dir = std::path::Path::new(filename).parent().map(|p| p.to_path_buf());
    let result = gorget::semantic::analyze_with_source_dir(&mut module, features, source_dir, false);

    if !result.errors.is_empty() {
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        for err in &result.errors {
            reporter.report_semantic_error(err);
        }
        return Err(format!("{} semantic error(s) found", result.errors.len()));
    }

    // Display warnings (non-fatal)
    if !result.warnings.is_empty() {
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        for warn in &result.warnings {
            reporter.report_semantic_warning(warn);
        }
    }

    // Lower AST to GIR
    let mut gir_module = gorget::ir::lowering::lower_module(&module, &result, &options);

    // Run GIR optimization passes
    let opt_stats = gorget::ir::transforms::optimize::optimize_module(&mut gir_module);
    let _ = opt_stats; // available for --emit-gir stats or future --verbose

    // Dump GIR text if requested
    if emit_gir {
        print!("{}", gorget::ir::printer::print_module(&gir_module));
        if opt_stats.insts_eliminated() > 0 || opt_stats.blocks_eliminated() > 0 {
            eprintln!("; Optimization: {} blocks, {} instructions, {} locals eliminated",
                opt_stats.blocks_eliminated(), opt_stats.insts_eliminated(), opt_stats.locals_eliminated());
        }
        // Don't proceed to C codegen — just dump and exit
        let input_path = Path::new(filename);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        return Ok(PathBuf::from(stem));
    }

    // Dump LIR text if requested (pre-SSA if env var set)
    if emit_lir {
        let pre_ssa = std::env::var("LIR_PRE_SSA").is_ok();
        let mut lir_module = gorget::lir::lower::lower_module(&gir_module);
        if pre_ssa {
            print!("{}", gorget::lir::display::dump_module(&lir_module));
            let input_path = Path::new(filename);
            let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
            return Ok(PathBuf::from(stem));
        }
        let no_opt = std::env::var("LIR_NO_OPT").is_ok();
        for func in &mut lir_module.functions {
            gorget::lir::ssa::construct_ssa(func);
        }
        if !no_opt {
            let stats = gorget::lir::optimize::optimize_module(&mut lir_module);
            eprintln!("; LIR opt: {} dead fns, {} dead globals, {} dead insts, {} folded, {} copies prop'd",
                stats.dead_functions_eliminated, stats.dead_globals_eliminated,
                stats.dead_instructions_eliminated, stats.constants_folded,
                stats.copies_propagated);
        }
        print!("{}", gorget::lir::display::dump_module(&lir_module));
        let errors = gorget::lir::validate::validate_module(&lir_module);
        if !errors.is_empty() {
            eprintln!("; LIR validation: {} error(s)", errors.len());
            for e in &errors {
                eprintln!(";   {e}");
            }
        }
        let input_path = Path::new(filename);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        return Ok(PathBuf::from(stem));
    }

    // Dump C code generated from LIR if requested
    if emit_c_lir {
        let mut lir_module = gorget::lir::lower::lower_module(&gir_module);
        for func in &mut lir_module.functions {
            gorget::lir::ssa::construct_ssa(func);
        }
        gorget::lir::optimize::optimize_module(&mut lir_module);
        let c_code = gorget::backend::c_lir::generate_c(&lir_module);
        print!("{c_code}");
        let input_path = Path::new(filename);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        return Ok(PathBuf::from(stem));
    }

    // ── LIR backend: full build through LIR → C → binary ──────────
    if use_lir_backend {
        // Lower GIR → LIR → SSA → optimize → backend
        let mut lir_module = gorget::lir::lower::lower_module(&gir_module);
        for func in &mut lir_module.functions {
            gorget::lir::ssa::construct_ssa(func);
        }
        gorget::lir::optimize::optimize_module(&mut lir_module);

        let backend = gorget::backend::c_lir::CLirBackend;
        let output = gorget::backend::Backend::generate(&backend, &lir_module);
        let c_code = output.code;

        // Determine output paths
        let input_path = Path::new(filename);
        let default_dir = input_path.parent().unwrap_or(Path::new("."));
        let dir = output_dir.unwrap_or(default_dir);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        let (c_path, exe_path) = if let Some(out) = output_exe {
            let out = std::path::absolute(out).unwrap_or(out.to_path_buf());
            let c_path = out.with_extension("c");
            (c_path, out)
        } else {
            let c_path = dir.join(format!("{stem}.c"));
            let exe_path = dir.join(stem);
            let exe_path = std::path::absolute(&exe_path).unwrap_or(exe_path);
            (c_path, exe_path)
        };

        // ── --shared: build as shared library (used by hot-reload recompile) ──
        if let Some(shared_path) = shared_output {
            let shared_c_code = if lir_module.hot_reload {
                let state_type = lir_module.hot_reload_state_type.as_deref().unwrap_or("State");
                let (_, guest) = gorget::backend::generate_hot_reload_split(
                    &c_code, state_type, lir_module.hot_reload_state_hash,
                    lir_module.hot_reload_has_reload_fn, None,
                );
                guest
            } else {
                c_code.clone()
            };
            let shared_c_path = dir.join(format!("{stem}_guest.c"));
            if let Err(e) = fs::write(&shared_c_path, &shared_c_code) {
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
                .arg("-Wno-unused-but-set-variable")
                .arg("-o")
                .arg(shared_path)
                .arg(&shared_c_path)
                .arg("-lm");
            if options.overflow_wrap || gir_module.runtime.overflow_wrap { cc_cmd.arg("-fwrapv"); }
            if options.sanitize {
                cc_cmd.arg("-fsanitize=address,undefined");
                cc_cmd.arg("-fno-omit-frame-pointer");
                cc_cmd.arg("-g");
            }
            add_sdl_flags(&mut cc_cmd, concat_source.contains("gg.sdl") || concat_source.contains("gg.gfx"), &shared_c_code);
            add_tls_flags(&mut cc_cmd, concat_source.contains("std.net.tls") || concat_source.contains("gg.http"));
            add_crypto_flags(&mut cc_cmd, concat_source.contains("gg.crypto") || concat_source.contains("gg.p2p"));
            add_regex_flags(&mut cc_cmd, concat_source.contains("gg.regex"));
            add_thread_flags(&mut cc_cmd, concat_source.contains("std.async") || concat_source.contains("gg.p2p"));
            let status = cc_cmd.status();
            return match status {
                Ok(s) if s.success() => Ok(shared_path.to_path_buf()),
                Ok(s) => Err(format!("Shared library compilation failed: {s}\nGenerated: {}", shared_c_path.display())),
                Err(e) => Err(format!("Failed to run C compiler '{cc}': {e}")),
            };
        }

        // ── Hot-reload: two-phase build (host binary + guest shared library) ──
        if lir_module.hot_reload {
            let abs_filename = std::path::absolute(Path::new(filename))
                .unwrap_or_else(|_| PathBuf::from(filename));
            let guest_lib_name = format!("{stem}_guest");
            #[cfg(target_os = "macos")]
            let dylib_ext = ".dylib";
            #[cfg(not(target_os = "macos"))]
            let dylib_ext = ".so";
            let guest_lib_file = format!("{guest_lib_name}{dylib_ext}");
            let recompile_cmd = format!(
                "{} build --shared {} -o {}",
                std::env::current_exe().unwrap_or_else(|_| PathBuf::from("gg")).display(),
                abs_filename.display(),
                dir.join(&guest_lib_file).display(),
            );
            let state_type = lir_module.hot_reload_state_type.as_deref().unwrap_or("State");
            let hr_opts = gorget::backend::HotReloadOpts {
                watch_path: abs_filename.display().to_string(),
                guest_lib_name: guest_lib_name.clone(),
                recompile_cmd,
            };
            let (host_code, guest_code) = gorget::backend::generate_hot_reload_split(
                &c_code, state_type, lir_module.hot_reload_state_hash,
                lir_module.hot_reload_has_reload_fn, Some(&hr_opts),
            );

            let host_c_path = dir.join(format!("{stem}_host.c"));
            let guest_c_path = dir.join(format!("{stem}_guest.c"));
            let guest_lib_path = dir.join(format!("{}_guest{dylib_ext}", hr_opts.guest_lib_name.trim_end_matches("_guest")));

            if let Err(e) = fs::write(&host_c_path, &host_code) {
                return Err(format!("Error writing {}: {e}", host_c_path.display()));
            }
            if let Err(e) = fs::write(&guest_c_path, &guest_code) {
                return Err(format!("Error writing {}: {e}", guest_c_path.display()));
            }

            let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());

            // Compile guest shared library
            let mut guest_cmd = Command::new(&cc);
            guest_cmd
                .arg("-std=c11").arg("-shared").arg("-fPIC")
                .arg("-Wall").arg("-Wextra")
                .arg("-Wno-unused-parameter").arg("-Wno-unused-variable").arg("-Wno-unused-function")
                .arg("-Wno-unused-but-set-variable")
                .arg("-o").arg(&guest_lib_path)
                .arg(&guest_c_path).arg("-lm");
            if options.sanitize {
                guest_cmd.arg("-fsanitize=address,undefined");
                guest_cmd.arg("-fno-omit-frame-pointer");
                guest_cmd.arg("-g");
            }
            let guest_status = guest_cmd.status();
            match guest_status {
                Ok(s) if !s.success() => return Err(format!("Guest compilation failed: {s}\nGenerated: {}", guest_c_path.display())),
                Err(e) => return Err(format!("Failed to run '{cc}': {e}")),
                _ => {}
            }

            // Compile host binary
            let mut host_cmd = Command::new(&cc);
            host_cmd.arg("-std=c11")
                .arg("-Wall").arg("-Wextra")
                .arg("-Wno-unused-parameter").arg("-Wno-unused-variable").arg("-Wno-unused-function")
                .arg("-Wno-unused-but-set-variable")
                .arg("-o").arg(&exe_path)
                .arg(&host_c_path).arg("-lm").arg("-ldl");
            if options.overflow_wrap || gir_module.runtime.overflow_wrap { host_cmd.arg("-fwrapv"); }
            if options.sanitize {
                host_cmd.arg("-fsanitize=address,undefined");
                host_cmd.arg("-fno-omit-frame-pointer");
                host_cmd.arg("-g");
            }
            let host_status = host_cmd.status();
            return match host_status {
                Ok(s) if s.success() => Ok(exe_path),
                Ok(s) => Err(format!("Host compilation failed: {s}\nGenerated: {}", host_c_path.display())),
                Err(e) => Err(format!("Failed to run '{cc}': {e}")),
            };
        }

        // ── Normal LIR build ──
        if let Err(e) = fs::write(&c_path, &c_code) {
            return Err(format!("Error writing {}: {e}", c_path.display()));
        }

        let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
        let mut cc_cmd = Command::new(&cc);
        let needs_metal = concat_source.contains("gg.metal");
        cc_cmd
            .arg("-std=c11")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Wno-unused-parameter")
            .arg("-Wno-unused-variable")
            .arg("-Wno-unused-function")
            .arg("-Wno-unused-label")
            .arg("-Wno-unused-but-set-variable")
            // const qualifiers are now correctly emitted for borrow locals
            .arg("-Wno-sometimes-uninitialized")        // clang: conditional branch init
            .arg("-Wno-unknown-warning-option")         // suppress GCC/clang flag mismatch
            .arg("-ffunction-sections")
            .arg("-fdata-sections")
            .arg("-o")
            .arg(&exe_path);
        // Metal requires Objective-C compilation — must come before source file
        #[cfg(target_os = "macos")]
        if needs_metal {
            cc_cmd.arg("-x").arg("objective-c");
            cc_cmd.arg("-fno-objc-arc"); // Metal handles are cast to int64_t — ARC would release them
            cc_cmd.arg("-Wno-deprecated-declarations");
            cc_cmd.arg("-Wno-objc-method-access");
            cc_cmd.arg("-Wno-arc-bridge-casts-disallowed-in-nonarc");
            cc_cmd.arg("-Wno-incompatible-pointer-types-discards-qualifiers");
            cc_cmd.arg("-Wno-nonnull");
        }
        cc_cmd.arg(&c_path)
            .arg("-lm");

        #[cfg(not(target_os = "macos"))]
        cc_cmd.arg("-Wl,--gc-sections");
        #[cfg(target_os = "macos")]
        cc_cmd.arg("-Wl,-dead_strip");

        if options.overflow_wrap || gir_module.runtime.overflow_wrap {
            cc_cmd.arg("-fwrapv");
        }
        if options.sanitize {
            cc_cmd.arg("-fsanitize=address,undefined");
            cc_cmd.arg("-fno-omit-frame-pointer");
            cc_cmd.arg("-g");
        }

        // Library detection — use generated C for precise SDL sub-library detection
        add_sdl_flags(&mut cc_cmd, concat_source.contains("gg.sdl") || concat_source.contains("gg.gfx") || concat_source.contains("gg.gl") || needs_metal, &c_code);
        add_gl_flags(&mut cc_cmd, concat_source.contains("gg.gl"));
        add_audio_flags(&mut cc_cmd, concat_source.contains("gg.audio"));
        add_compress_flags(&mut cc_cmd, concat_source.contains("gg.compress"));
        add_metal_flags(&mut cc_cmd, needs_metal);
        add_tls_flags(&mut cc_cmd, concat_source.contains("std.net.tls") || concat_source.contains("gg.http"));
        add_crypto_flags(&mut cc_cmd, concat_source.contains("gg.crypto") || concat_source.contains("gg.p2p"));
        add_regex_flags(&mut cc_cmd, concat_source.contains("gg.regex"));
        add_thread_flags(&mut cc_cmd, concat_source.contains("std.async") || concat_source.contains("gg.p2p"));

        let status = cc_cmd.status();
        return match status {
            Ok(s) if s.success() => Ok(exe_path),
            Ok(s) => Err(format!(
                "C compiler exited with: {s}\nGenerated C file (LIR): {}",
                c_path.display()
            )),
            Err(e) => Err(format!(
                "Failed to run C compiler '{cc}': {e}\nGenerated C file (LIR): {}",
                c_path.display()
            )),
        };
    }

    // Determine output paths
    let input_path = Path::new(filename);
    let default_dir = input_path.parent().unwrap_or(Path::new("."));
    let dir = output_dir.unwrap_or(default_dir);
    let stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");
    let (c_path, exe_path) = if let Some(out) = output_exe {
        let out = std::path::absolute(out).unwrap_or(out.to_path_buf());
        let c_path = out.with_extension("c");
        (c_path, out)
    } else {
        let c_path = dir.join(format!("{stem}.c"));
        let exe_path = dir.join(stem);
        let exe_path = std::path::absolute(&exe_path).unwrap_or(exe_path);
        (c_path, exe_path)
    };

    // Build hot-reload options if needed
    let hr_opts = if gir_module.runtime.hot_reload {
        let abs_filename = std::path::absolute(Path::new(filename))
            .unwrap_or_else(|_| PathBuf::from(filename));
        let guest_lib_name = format!("{stem}_guest");
        #[cfg(target_os = "macos")]
        let dylib_ext = ".dylib";
        #[cfg(not(target_os = "macos"))]
        let dylib_ext = ".so";
        let guest_lib_file = format!("{guest_lib_name}{dylib_ext}");
        let recompile_cmd = format!(
            "{} build --shared {} -o {}",
            std::env::current_exe().unwrap_or_else(|_| PathBuf::from("gg")).display(),
            abs_filename.display(),
            dir.join(&guest_lib_file).display(),
        );
        Some(gorget::backend::HotReloadOpts {
            watch_path: abs_filename.display().to_string(),
            guest_lib_name: guest_lib_name.clone(),
            recompile_cmd,
        })
    } else {
        None
    };

    // Generate C from GIR
    let gir_output = gorget::backend::c::generate_c_with_opts(&gir_module, hr_opts.as_ref());

    // ── --shared: build as shared library (used by hot-reload recompile) ──
    if let Some(shared_path) = shared_output {
        let shared_c_code = gir_output.guest_code.as_deref().unwrap_or(&gir_output.c_code);
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
            .arg("-Wno-unused-but-set-variable")
            .arg("-o")
            .arg(shared_path)
            .arg(&shared_c_path)
            .arg("-lm");
        if options.overflow_wrap || gir_module.runtime.overflow_wrap { cc_cmd.arg("-fwrapv"); }
        if options.sanitize {
            cc_cmd.arg("-fsanitize=address,undefined");
            cc_cmd.arg("-fno-omit-frame-pointer");
            cc_cmd.arg("-g");
        }
        add_sdl_flags(&mut cc_cmd, concat_source.contains("gg.sdl") || concat_source.contains("gg.gfx"), shared_c_code);
        add_tls_flags(&mut cc_cmd, concat_source.contains("std.net.tls") || gir_output.needs_tls);
        add_crypto_flags(&mut cc_cmd, concat_source.contains("gg.crypto") || concat_source.contains("gg.p2p"));
        add_regex_flags(&mut cc_cmd, concat_source.contains("gg.regex"));
        add_thread_flags(&mut cc_cmd, concat_source.contains("std.async") || concat_source.contains("gg.p2p"));
        let status = cc_cmd.status();
        return match status {
            Ok(s) if s.success() => Ok(shared_path.to_path_buf()),
            Ok(s) => Err(format!("Shared library compilation failed: {s}\nGenerated: {}", shared_c_path.display())),
            Err(e) => Err(format!("Failed to run C compiler '{cc}': {e}")),
        };
    }

    // Hot-reload: two-phase build (host binary + guest shared library).
    if gir_module.runtime.hot_reload {
        let host_code = gir_output.host_code.as_deref().unwrap_or(&gir_output.c_code);
        let guest_code = gir_output.guest_code.as_deref().unwrap_or(&gir_output.c_code);

        let host_c_path = dir.join(format!("{stem}_host.c"));
        let guest_c_path = dir.join(format!("{stem}_guest.c"));
        let hr = hr_opts.as_ref().unwrap();
        #[cfg(target_os = "macos")]
        let dylib_ext = ".dylib";
        #[cfg(not(target_os = "macos"))]
        let dylib_ext = ".so";
        let guest_lib_path = dir.join(format!("{}_guest{dylib_ext}", hr.guest_lib_name.trim_end_matches("_guest")));

        if let Err(e) = fs::write(&host_c_path, host_code) {
            return Err(format!("Error writing {}: {e}", host_c_path.display()));
        }
        if let Err(e) = fs::write(&guest_c_path, guest_code) {
            return Err(format!("Error writing {}: {e}", guest_c_path.display()));
        }

        let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());

        // Compile guest shared library
        let mut guest_cmd = Command::new(&cc);
        guest_cmd
            .arg("-std=c11").arg("-shared").arg("-fPIC")
            .arg("-Wall").arg("-Wextra")
            .arg("-Wno-unused-parameter").arg("-Wno-unused-variable").arg("-Wno-unused-function")
            .arg("-Wno-unused-but-set-variable")
            .arg("-o").arg(&guest_lib_path)
            .arg(&guest_c_path).arg("-lm");
        if options.sanitize {
            guest_cmd.arg("-fsanitize=address,undefined");
            guest_cmd.arg("-fno-omit-frame-pointer");
            guest_cmd.arg("-g");
        }
        let guest_status = guest_cmd.status();
        match guest_status {
            Ok(s) if !s.success() => return Err(format!("Guest compilation failed: {s}\nGenerated: {}", guest_c_path.display())),
            Err(e) => return Err(format!("Failed to run '{cc}': {e}")),
            _ => {}
        }

        // Compile host binary
        let mut host_cmd = Command::new(&cc);
        host_cmd.arg("-std=c11")
            .arg("-Wall").arg("-Wextra")
            .arg("-Wno-unused-parameter").arg("-Wno-unused-variable").arg("-Wno-unused-function")
            .arg("-Wno-unused-but-set-variable")
            .arg("-o").arg(&exe_path)
            .arg(&host_c_path).arg("-lm").arg("-ldl");
        if options.overflow_wrap || gir_module.runtime.overflow_wrap { host_cmd.arg("-fwrapv"); }
        if options.sanitize {
            host_cmd.arg("-fsanitize=address,undefined");
            host_cmd.arg("-fno-omit-frame-pointer");
            host_cmd.arg("-g");
        }
        let host_status = host_cmd.status();
        return match host_status {
            Ok(s) if s.success() => Ok(exe_path),
            Ok(s) => Err(format!("Host compilation failed: {s}\nGenerated: {}", host_c_path.display())),
            Err(e) => Err(format!("Failed to run '{cc}': {e}")),
        };
    }

    // Normal single-binary compile.
    let c_code = &gir_output.c_code;
    if let Err(e) = fs::write(&c_path, c_code) {
        return Err(format!("Error writing {}: {e}", c_path.display()));
    }

    let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let mut cc_cmd = Command::new(&cc);
    let needs_metal = concat_source.contains("gg.metal");
    cc_cmd
        .arg("-std=c11")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-Wno-unused-function")
        .arg("-Wno-unused-label")
        .arg("-Wno-unused-but-set-variable")
        .arg("-Wno-discarded-qualifiers")          // GCC
        .arg("-Wno-ignored-qualifiers")             // clang
        .arg("-Wno-sometimes-uninitialized")        // clang: conditional branch init
        .arg("-Wno-unknown-warning-option")         // suppress GCC/clang flag mismatch
        .arg("-ffunction-sections")
        .arg("-fdata-sections")
        .arg("-o")
        .arg(&exe_path);
    // Metal requires Objective-C compilation — must come before source file
    #[cfg(target_os = "macos")]
    if needs_metal {
        cc_cmd.arg("-x").arg("objective-c");
        cc_cmd.arg("-Wno-deprecated-declarations");
        cc_cmd.arg("-Wno-objc-method-access");
        cc_cmd.arg("-Wno-arc-bridge-casts-disallowed-in-nonarc");
        cc_cmd.arg("-Wno-incompatible-pointer-types-discards-qualifiers");
        cc_cmd.arg("-Wno-nonnull");
    }
    cc_cmd.arg(&c_path)
        .arg("-lm");

    // Let the linker strip unused functions/data (dead code elimination).
    #[cfg(not(target_os = "macos"))]
    cc_cmd.arg("-Wl,--gc-sections");
    #[cfg(target_os = "macos")]
    cc_cmd.arg("-Wl,-dead_strip");

    if options.overflow_wrap || gir_module.runtime.overflow_wrap {
        cc_cmd.arg("-fwrapv");
    }

    if options.sanitize {
        cc_cmd.arg("-fsanitize=address,undefined");
        cc_cmd.arg("-fno-omit-frame-pointer");
        cc_cmd.arg("-g");
    }

    add_sdl_flags(&mut cc_cmd, concat_source.contains("gg.sdl") || concat_source.contains("gg.gfx") || concat_source.contains("gg.gl") || needs_metal, c_code);
    add_gl_flags(&mut cc_cmd, concat_source.contains("gg.gl"));
    add_audio_flags(&mut cc_cmd, concat_source.contains("gg.audio"));
    add_compress_flags(&mut cc_cmd, concat_source.contains("gg.compress"));
    add_metal_flags(&mut cc_cmd, needs_metal);
    add_tls_flags(&mut cc_cmd, concat_source.contains("std.net.tls") || gir_output.needs_tls);
    add_crypto_flags(&mut cc_cmd, concat_source.contains("gg.crypto") || concat_source.contains("gg.p2p"));
    add_regex_flags(&mut cc_cmd, concat_source.contains("gg.regex"));
    add_thread_flags(&mut cc_cmd, concat_source.contains("std.async") || concat_source.contains("gg.p2p"));

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
            match try_build_ir(&gg_path_str, &source, HashMap::new(), Some(&tmp_dir), None, None, &[], gorget::ir::lowering::LoweringOptions::default(), false, false, false, false) {
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
            match try_build_ir(&gg_path_str, &source, HashMap::new(), Some(&tmp_dir), None, None, &[], gorget::ir::lowering::LoweringOptions::default(), false, false, false, false) {
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
        println!("Compiler commands: lex, parse, check, build, run, sim, fmt, test, report");
        println!("Package commands:  init, new, add, remove");
        println!();
        println!("Build flags:");
        println!("  --hot-reload            Enable hot code reload (builds host + guest .dylib)");
        println!("  --shared [-o F]         Build as shared library (.dylib/.so)");
        println!("  --sanitize              Enable AddressSanitizer + UBSan for runtime bug detection");
        println!("  --emit-gir              Dump GIR (intermediate representation) to stdout instead of compiling");
        println!("  --emit-lir              Dump LIR (low-level SSA IR) to stdout instead of compiling");
        println!("  --emit-c-lir            Dump C code generated from LIR to stdout (for A/B testing)");
        println!("  --backend=gir           Build through legacy GIR→C backend instead of LIR→C");
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
        let sanitize = args.iter().any(|a| a == "--sanitize");
        let dep_paths = resolve_deps_for_file(filename);
        let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
            eprintln!("Failed to create temp directory: {e}");
            process::exit(1);
        });
        let features = parse_features(&args);
        let hot_reload_flag = args.iter().any(|a| a == "--hot-reload");
        let has_trace = !no_trace && (trace || source_has_trace(&source));
        let trace_filename = if has_trace {
            let trace_path = Path::new(filename).parent().unwrap_or(Path::new("."))
                .join(format!("{}.trace.jsonl", Path::new(filename).file_stem().and_then(|s| s.to_str()).unwrap_or("output")));
            let trace_path = std::path::absolute(&trace_path).unwrap_or(trace_path);
            Some(trace_path.display().to_string())
        } else {
            None
        };
        let lowering_opts = gorget::ir::lowering::LoweringOptions {
            strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked,
            trace_filename, hot_reload: hot_reload_flag || source_has_hot_reload(&source),
            sanitize, scheduler_mode: parse_scheduler(&args),
            ..Default::default()
        };
        let use_lir = !args.iter().any(|a| a == "--backend=gir");
        let exe_path = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, None, &features, lowering_opts, false, false, false, use_lir)
            .unwrap_or_else(|e| { eprintln!("{e}"); process::exit(1); });
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
    let scheduler_mode = parse_scheduler(&args);
    let sanitize = args.iter().any(|a| a == "--sanitize");
    let emit_gir = args.iter().any(|a| a == "--emit-gir");
    let emit_lir = args.iter().any(|a| a == "--emit-lir");
    let emit_c_lir = args.iter().any(|a| a == "--emit-c-lir");
    let shared_mode = args.iter().any(|a| a == "--shared");
    let show_borrows = args.iter().any(|a| a == "--show-borrows");
    let warn_const = args.iter().any(|a| a == "--warn-const");
    let use_lir_backend = !args.iter().any(|a| a == "--backend=gir");
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
    // Find positional filename, skipping values of known flag pairs.
    // For `gg sim test <file>`, "test" is a subcommand not the filename.
    let filename = {
        let flags_with_values = ["--tag", "--exclude-tag", "--filter", "--report", "--output", "-o", "--feature"];
        // Subcommand words that appear after the command and are NOT filenames.
        let sim_subcommands: &[&str] = if command == "sim" { &["test"] } else { &[] };
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
            if sim_subcommands.contains(&arg.as_str()) {
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

            if !parser.warnings.is_empty() {
                let reporter = ErrorReporter::new(filename.clone(), source.clone());
                for warn in &parser.warnings {
                    reporter.report_parse_warning(warn);
                }
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

            if !parser.warnings.is_empty() {
                let reporter = ErrorReporter::new(filename.clone(), source.clone());
                for warn in &parser.warnings {
                    reporter.report_parse_warning(warn);
                }
            }

            // Load imported modules recursively and merge
            let dep_paths = resolve_deps_for_file(filename);
            let (mut module, file_infos) = load_imports(filename, &source, module, dep_paths);

            let source_dir = std::path::Path::new(filename).parent().map(|p| p.to_path_buf());
            let result = gorget::semantic::analyze_with_source_dir(&mut module, &features, source_dir, warn_const);

            if show_borrows {
                print_borrow_summary(&result);
            }

            if result.errors.is_empty() {
                if !result.warnings.is_empty() {
                    let reporter = ErrorReporter::new_multi(file_infos);
                    for warn in &result.warnings {
                        reporter.report_semantic_warning(warn);
                    }
                }
                println!("OK: no semantic errors");
            } else {
                let reporter = ErrorReporter::new_multi(file_infos);
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
                let has_trace = !no_trace && (trace || source_has_trace(&source));
                let trace_filename = if has_trace {
                    let input_path = Path::new(filename);
                    let dir = input_path.parent().unwrap_or(Path::new("."));
                    let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                    let tp = dir.join(format!("{stem}.trace.jsonl"));
                    Some(std::path::absolute(&tp).unwrap_or(tp).display().to_string())
                } else {
                    None
                };
                let lowering_opts = gorget::ir::lowering::LoweringOptions {
                    strip_asserts,
                    no_strip_asserts,
                    overflow_wrap,
                    overflow_checked,
                    trace_filename,
                    hot_reload: hot_reload_flag || source_has_hot_reload(&source),
                    sanitize, scheduler_mode,
                    ..Default::default()
                };
                let result = try_build_ir(filename, &source, dep_paths, None, None, Some(shared_path), &features, lowering_opts, emit_gir, emit_lir, emit_c_lir, use_lir_backend);
                match result {
                    Ok(p) => if !emit_gir && !emit_lir && !emit_c_lir { println!("Built shared library: {}", p.display()); }
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            } else {
                // GIR pipeline (handles hot-reload, trace, and normal builds).
                let has_trace = !no_trace && (trace || source_has_trace(&source));
                let trace_filename = if has_trace {
                    let input_path = Path::new(filename);
                    let dir = input_path.parent().unwrap_or(Path::new("."));
                    let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                    let tp = dir.join(format!("{stem}.trace.jsonl"));
                    Some(std::path::absolute(&tp).unwrap_or(tp).display().to_string())
                } else {
                    None
                };
                let lowering_opts = gorget::ir::lowering::LoweringOptions {
                    strip_asserts,
                    no_strip_asserts,
                    overflow_wrap,
                    overflow_checked,
                    trace_filename,
                    hot_reload: hot_reload_flag || source_has_hot_reload(&source),
                    sanitize,
                    ..Default::default()
                };
                let result = try_build_ir(filename, &source, dep_paths, None, shared_output_path.as_deref(), None, &features, lowering_opts, emit_gir, emit_lir, emit_c_lir, use_lir_backend);
                match result {
                    Ok(p) => if !emit_gir && !emit_lir && !emit_c_lir { println!("Built: {}", p.display()); }
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            }
        }
        "run" => {
            let dep_paths = resolve_deps_for_file(filename);
            let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
                eprintln!("Failed to create temp directory: {e}");
                process::exit(1);
            });
            let has_trace = !no_trace && (trace || source_has_trace(&source));
            let trace_filename = if has_trace {
                let input_path = Path::new(filename);
                let dir = input_path.parent().unwrap_or(Path::new("."));
                let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                let tp = dir.join(format!("{stem}.trace.jsonl"));
                Some(std::path::absolute(&tp).unwrap_or(tp).display().to_string())
            } else {
                None
            };
            let lowering_opts = gorget::ir::lowering::LoweringOptions {
                strip_asserts,
                no_strip_asserts,
                overflow_wrap,
                overflow_checked,
                trace_filename,
                hot_reload: hot_reload_flag || source_has_hot_reload(&source),
                sanitize, scheduler_mode,
                ..Default::default()
            };
            let result = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, None, &features, lowering_opts, false, false, false, use_lir_backend);
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
        "test" => {
            // Collect --tag, --exclude-tag, --filter, --report, --bench, --timeout,
            // --parallel, --failed-only, --failed-first values
            let mut test_tags = Vec::new();
            let mut test_exclude_tags = Vec::new();
            let mut test_name_filter: Option<String> = None;
            let mut report_html = false;
            let mut bench_mode = false;
            let mut timeout_ms: Option<u64> = None;
            let mut parallel: Option<usize> = None;
            let mut failed_only = false;
            let mut failed_first = false;
            let mut snapshot_cmd: Option<String> = None;
            let mut snapshot_args: Vec<String> = Vec::new();
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
                } else if args[i] == "--bench" {
                    bench_mode = true;
                    i += 1;
                } else if args[i] == "--timeout" && i + 1 < args.len() {
                    timeout_ms = parse_timeout_value(&args[i + 1]);
                    i += 2;
                } else if args[i].starts_with("--timeout=") {
                    timeout_ms = parse_timeout_value(&args[i]["--timeout=".len()..]);
                    i += 1;
                } else if args[i] == "--parallel" && i + 1 < args.len() {
                    parallel = args[i + 1].parse::<usize>().ok();
                    i += 2;
                } else if args[i].starts_with("--parallel=") {
                    parallel = args[i]["--parallel=".len()..].parse::<usize>().ok();
                    i += 1;
                } else if args[i] == "--failed-only" {
                    failed_only = true;
                    i += 1;
                } else if args[i] == "--failed-first" {
                    failed_first = true;
                    i += 1;
                } else if args[i] == "--snapshot" && i + 1 < args.len() {
                    snapshot_cmd = Some(args[i + 1].clone());
                    // Collect remaining args after subcommand as snapshot args
                    i += 2;
                    while i < args.len() && !args[i].starts_with("--") {
                        snapshot_args.push(args[i].clone());
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }
            // Handle snapshot subcommands that don't require compilation
            let snapshot_dir = {
                let stem = Path::new(filename).file_stem().and_then(|s| s.to_str()).unwrap_or("test");
                Path::new(filename).parent().unwrap_or(Path::new(".")).join(".gorget").join("snapshots").join(stem)
            };
            if let Some(ref cmd) = snapshot_cmd {
                match cmd.as_str() {
                    "list" => {
                        if snapshot_dir.exists() {
                            let mut entries: Vec<_> = std::fs::read_dir(&snapshot_dir)
                                .unwrap_or_else(|_| { eprintln!("No snapshots found"); process::exit(0); })
                                .filter_map(|e| e.ok())
                                .filter(|e| e.path().extension().and_then(|x| x.to_str()) == Some("json"))
                                .collect();
                            entries.sort_by_key(|e| e.metadata().ok().and_then(|m| m.modified().ok()));
                            if entries.is_empty() {
                                println!("No snapshots saved.");
                            } else {
                                println!("Saved snapshots:");
                                for entry in &entries {
                                    let name = entry.path().file_stem().and_then(|s| s.to_str()).unwrap_or("?").to_string();
                                    println!("  {name}");
                                }
                            }
                        } else {
                            println!("No snapshots saved.");
                        }
                        process::exit(0);
                    }
                    "show" => {
                        let version = snapshot_args.first().unwrap_or_else(|| {
                            eprintln!("Usage: gg test <file> --snapshot show <version>");
                            process::exit(1);
                        });
                        let path = snapshot_dir.join(format!("{version}.json"));
                        match std::fs::read_to_string(&path) {
                            Ok(contents) => print!("{contents}"),
                            Err(_) => { eprintln!("Snapshot '{version}' not found"); process::exit(1); }
                        }
                        process::exit(0);
                    }
                    "delete" => {
                        let version = snapshot_args.first().unwrap_or_else(|| {
                            eprintln!("Usage: gg test <file> --snapshot delete <version>");
                            process::exit(1);
                        });
                        let path = snapshot_dir.join(format!("{version}.json"));
                        if path.exists() {
                            std::fs::remove_file(&path).unwrap_or_else(|e| {
                                eprintln!("Failed to delete snapshot: {e}");
                                process::exit(1);
                            });
                            println!("Deleted snapshot '{version}'");
                        } else {
                            eprintln!("Snapshot '{version}' not found");
                            process::exit(1);
                        }
                        process::exit(0);
                    }
                    "diff" => {
                        if snapshot_args.len() < 2 {
                            eprintln!("Usage: gg test <file> --snapshot diff <v1> <v2>");
                            process::exit(1);
                        }
                        let v1_path = snapshot_dir.join(format!("{}.json", snapshot_args[0]));
                        let v2_path = snapshot_dir.join(format!("{}.json", snapshot_args[1]));
                        let v1 = std::fs::read_to_string(&v1_path).unwrap_or_else(|_| {
                            eprintln!("Snapshot '{}' not found", snapshot_args[0]);
                            process::exit(1);
                        });
                        let v2 = std::fs::read_to_string(&v2_path).unwrap_or_else(|_| {
                            eprintln!("Snapshot '{}' not found", snapshot_args[1]);
                            process::exit(1);
                        });
                        let exit_code = snapshot_diff(&snapshot_args[0], &v1, &snapshot_args[1], &v2);
                        process::exit(exit_code);
                    }
                    "save" => { /* handled below — needs compilation */ }
                    other => {
                        eprintln!("Unknown snapshot subcommand: {other}");
                        eprintln!("Usage: --snapshot <save|diff|list|show|delete> [args...]");
                        process::exit(1);
                    }
                }
            }

            let snapshot_mode = matches!(snapshot_cmd.as_deref(), Some("save"));

            // --report html implies --trace (unless --no-trace is explicit)
            let trace = if report_html && !no_trace { true } else { trace };
            let tmp_dir = tempfile::tempdir().unwrap_or_else(|e| {
                eprintln!("Failed to create temp directory: {e}");
                process::exit(1);
            });
            let dep_paths = resolve_deps_for_file(filename);
            let has_trace = !no_trace && (trace || source_has_trace(&source));
            let trace_filename = if has_trace {
                let input_path = Path::new(filename);
                let dir = input_path.parent().unwrap_or(Path::new("."));
                let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
                let tp = dir.join(format!("{stem}.trace.jsonl"));
                Some(std::path::absolute(&tp).unwrap_or(tp).display().to_string())
            } else {
                None
            };

            // Results file for --failed-only / --failed-first persistence
            let results_dir = Path::new(filename).parent().unwrap_or(Path::new(".")).join(".gorget");
            let stem = Path::new(filename).file_stem().and_then(|s| s.to_str()).unwrap_or("test");
            let results_path = results_dir.join(format!("{stem}.test-results.json"));

            // Read previous results for --failed-only / --failed-first
            let prev_failed_names = if failed_only || failed_first {
                read_failed_test_names(&results_path)
            } else {
                Vec::new()
            };

            // --failed-only: filter to only previously failed tests
            let effective_filter = if failed_only && !prev_failed_names.is_empty() {
                // Use a special filter that matches any of the failed names
                // We'll pass them via test_name_filter as pipe-separated exact names
                Some(prev_failed_names.join("|"))
            } else {
                test_name_filter.clone()
            };

            let failed_first_names = if failed_first { prev_failed_names.clone() } else { Vec::new() };
            let lowering_opts = gorget::ir::lowering::LoweringOptions {
                test_mode: true,
                bench_mode,
                snapshot_mode,
                test_tags: test_tags.clone(),
                test_exclude_tags: test_exclude_tags.clone(),
                test_name_filter: effective_filter,
                default_timeout_ms: timeout_ms,
                failed_first_names,
                trace_filename,
                sanitize, scheduler_mode,
                ..Default::default()
            };
            let exe_path = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, None, &features, lowering_opts, false, false, false, false)
                .unwrap_or_else(|e| {
                    eprintln!("{e}");
                    process::exit(1);
                });

            // Ensure .gorget/ directory exists for results file
            let _ = std::fs::create_dir_all(&results_dir);

            if let Some(n) = parallel {
                // Parallel execution: spawn N worker processes
                let n = n.max(1);
                let mut children = Vec::new();
                for worker_id in 0..n {
                    let child = Command::new(&exe_path)
                        .env("GORGET_PARALLEL_ID", worker_id.to_string())
                        .env("GORGET_PARALLEL_TOTAL", n.to_string())
                        .env("GORGET_TEST_RESULTS", worker_results_path(&results_path, worker_id).display().to_string())
                        .spawn()
                        .unwrap_or_else(|e| {
                            eprintln!("Failed to spawn worker {worker_id}: {e}");
                            process::exit(1);
                        });
                    children.push(child);
                }
                let mut any_failed = false;
                for mut child in children {
                    let status = child.wait().unwrap_or_else(|e| {
                        eprintln!("Failed to wait for worker: {e}");
                        process::exit(1);
                    });
                    if !status.success() { any_failed = true; }
                }
                // Merge worker result files
                merge_parallel_results(&results_path, n);
                process::exit(if any_failed { 1 } else { 0 });
            }

            // Snapshot temp file for capture
            let snapshot_tmp = if snapshot_mode {
                Some(tmp_dir.path().join("snapshot_capture.json"))
            } else {
                None
            };

            // Sequential execution (default)
            let mut cmd = Command::new(&exe_path);
            cmd.env("GORGET_TEST_RESULTS", results_path.display().to_string());
            if let Some(ref snap_path) = snapshot_tmp {
                cmd.env("GORGET_SNAPSHOT_PATH", snap_path.display().to_string());
            }
            let status = cmd.status()
                .unwrap_or_else(|e| {
                    eprintln!("Failed to execute {}: {e}", exe_path.display());
                    process::exit(1);
                });

            // If snapshot save mode, restructure capture file into versioned JSON
            if let (Some(snap_path), Some(_cmd)) = (&snapshot_tmp, &snapshot_cmd) {
                let version = snapshot_args.first().unwrap_or_else(|| {
                    eprintln!("Usage: gg test <file> --snapshot save <version>");
                    process::exit(1);
                });
                let _ = std::fs::create_dir_all(&snapshot_dir);
                let dest = snapshot_dir.join(format!("{version}.json"));
                if snap_path.exists() {
                    let raw = std::fs::read_to_string(snap_path).unwrap_or_default();
                    let structured = restructure_snapshot_capture(&raw, version, filename);
                    std::fs::write(&dest, &structured).unwrap_or_else(|e| {
                        eprintln!("Failed to write snapshot: {e}");
                        process::exit(1);
                    });
                    println!("Snapshot '{version}' saved to {}", dest.display());
                } else {
                    // No snapshot statements executed — write empty
                    let structured = format!("{{\n  \"version\": \"{version}\",\n  \"file\": \"{filename}\",\n  \"tests\": {{}}\n}}\n");
                    std::fs::write(&dest, &structured).unwrap_or_else(|e| {
                        eprintln!("Failed to write snapshot: {e}");
                        process::exit(1);
                    });
                    println!("Snapshot '{version}' saved (no snapshot points captured)");
                }
            }
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
            let check = args.iter().any(|a| a == "--check" || a == "-c");
            let formatted = gorget::formatter::format_source(&source);
            if check {
                if formatted != source {
                    eprintln!("{filename}: not formatted");
                    process::exit(1);
                }
            } else if in_place {
                if let Err(e) = fs::write(filename, &formatted) {
                    eprintln!("Error writing {filename}: {e}");
                    process::exit(1);
                }
            } else {
                print!("{formatted}");
            }
        }
        "sim" => {
            // GIR interpreter: lex → parse → semantic analysis → GIR lowering → interpret
            // Sub-subcommand: `gg sim test <file>` activates test mode.
            let is_test_mode = args.iter().skip(2).any(|a| a == "test");
            let is_bench_mode = args.iter().any(|a| a == "--bench");

            let mut parser = gorget::parser::Parser::new(&source);
            let module = parser.parse_module();

            if !parser.errors.is_empty() {
                let reporter = gorget::errors::ErrorReporter::new(filename.clone(), source.clone());
                for err in &parser.errors {
                    reporter.report_parse_error(err);
                }
                eprintln!("\n{} parse error(s) found", parser.errors.len());
                process::exit(1);
            }

            let dep_paths = resolve_deps_for_file(filename);
            let (mut module, file_infos) = load_imports(filename, &source, module, dep_paths);
            let concat_source: String = file_infos.iter().map(|(_, src, _)| src.as_str()).collect::<Vec<_>>().join("\n");

            let source_dir = std::path::Path::new(&filename).parent().map(|p| p.to_path_buf());
            let result = gorget::semantic::analyze_with_source_dir(&mut module, &features, source_dir, false);

            if !result.errors.is_empty() {
                let reporter = gorget::errors::ErrorReporter::new_multi(file_infos);
                for err in &result.errors {
                    reporter.report_semantic_error(err);
                }
                eprintln!("\n{} semantic error(s) found", result.errors.len());
                process::exit(1);
            }

            if !result.warnings.is_empty() {
                let reporter = gorget::errors::ErrorReporter::new_multi(file_infos.clone());
                for warn in &result.warnings {
                    reporter.report_semantic_warning(warn);
                }
            }

            // Parse test-mode flags (--filter, --tag, --exclude-tag).
            let mut test_tags: Vec<String> = Vec::new();
            let mut test_exclude_tags: Vec<String> = Vec::new();
            let mut test_name_filter: Option<String> = None;
            if is_test_mode {
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
                    } else {
                        i += 1;
                    }
                }
            }

            let overflow_wrap = args.iter().any(|a| a == "--overflow=wrap");
            let overflow_checked = args.iter().any(|a| a == "--overflow=checked");
            let lowering_opts = gorget::ir::lowering::LoweringOptions {
                test_mode: is_test_mode,
                bench_mode: is_bench_mode,
                test_tags,
                test_exclude_tags,
                test_name_filter,
                overflow_wrap,
                overflow_checked,
                scheduler_mode: parse_scheduler(&args),
                ..Default::default()
            };

            let mut sim_config = gorget::sim::SimConfig::from_args(&args);
            // When running tests via `gg sim test`, enable UB checks by default
            // unless the user explicitly passed --ub-checks (already handled) or
            // --no-ub-checks (not yet a flag, so just enable unconditionally here).
            if is_test_mode {
                sim_config.ub_checks = true;
            }

            let mut gir_module = gorget::ir::lowering::lower_module(&module, &result, &lowering_opts);
            gir_module.source_filename = Some(filename.to_string());
            gir_module.source_code = Some(concat_source.clone());
            let exit_code = gorget::sim::interpret(&gir_module, filename, &sim_config);
            process::exit(exit_code);
        }
        _ => {
            eprintln!("Unknown command: {command}");
            eprintln!("Compiler commands: lex, parse, check, build, run, sim, test, fmt, report");
            eprintln!("Package commands:  init, new, add, remove");
            process::exit(1);
        }
    }
}

/// Parse a timeout value like "5s", "5000ms", or "5000" (bare number = milliseconds).
fn parse_timeout_value(s: &str) -> Option<u64> {
    let s = s.trim();
    if let Some(secs) = s.strip_suffix('s') {
        if let Some(ms_str) = secs.strip_suffix('m') {
            // "5000ms" -> strip "ms" suffix
            ms_str.trim().parse::<u64>().ok()
        } else {
            // "5s" -> seconds to milliseconds
            secs.trim().parse::<u64>().ok().map(|v| v * 1000)
        }
    } else {
        // bare number = milliseconds
        s.parse::<u64>().ok()
    }
}

/// Read previously failed test names from a JSON results file.
/// The file format is: `{"results":[{"name":"...","status":"pass"|"fail"|"skip"}, ...]}`
fn read_failed_test_names(path: &Path) -> Vec<String> {
    let contents = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };
    let mut names = Vec::new();
    // Find each {"name":"...","status":"fail"} entry
    // Simple approach: find all "status":"fail" entries and extract the preceding "name"
    let mut search_from = 0;
    while let Some(pos) = contents[search_from..].find("\"status\":\"fail\"") {
        let abs_pos = search_from + pos;
        // Look backwards for "name":"..." in the same object
        let obj_start = contents[..abs_pos].rfind('{').unwrap_or(0);
        let obj_slice = &contents[obj_start..abs_pos];
        if let Some(name_pos) = obj_slice.find("\"name\":\"") {
            let name_start = name_pos + "\"name\":\"".len();
            if let Some(name_end) = obj_slice[name_start..].find('"') {
                names.push(obj_slice[name_start..name_start + name_end].to_string());
            }
        }
        search_from = abs_pos + 1;
    }
    names
}

/// Merge per-worker result files into a single results file.
fn merge_parallel_results(results_path: &Path, n: usize) {
    let mut entries: Vec<(String, String)> = Vec::new();
    for worker_id in 0..n {
        let worker_path = worker_results_path(results_path, worker_id);
        if let Ok(contents) = std::fs::read_to_string(&worker_path) {
            // Parse each {"name":"...","status":"..."} entry
            let mut search_from = 0;
            while let Some(pos) = contents[search_from..].find("\"name\":\"") {
                let abs_pos = search_from + pos;
                let name_start = abs_pos + "\"name\":\"".len();
                if let Some(name_end) = contents[name_start..].find('"') {
                    let name = contents[name_start..name_start + name_end].to_string();
                    // Find the status for this entry
                    let rest = &contents[name_start + name_end..];
                    if let Some(status_pos) = rest.find("\"status\":\"") {
                        let s_start = status_pos + "\"status\":\"".len();
                        if let Some(s_end) = rest[s_start..].find('"') {
                            let status = rest[s_start..s_start + s_end].to_string();
                            entries.push((name, status));
                        }
                    }
                    search_from = name_start + name_end + 1;
                } else {
                    break;
                }
            }
            // Clean up worker file
            let _ = std::fs::remove_file(&worker_path);
        }
    }
    // Write merged results in the same array format
    if !entries.is_empty() {
        let items: Vec<String> = entries.iter()
            .map(|(name, status)| format!("  {{\"name\":\"{name}\",\"status\":\"{status}\"}}"))
            .collect();
        let json = format!("{{\"results\":[\n{}\n]}}\n", items.join(",\n"));
        let _ = std::fs::write(results_path, json);
    }
}

/// Get the per-worker results file path (sibling of the main results file).
fn worker_results_path(results_path: &Path, worker_id: usize) -> PathBuf {
    let stem = results_path.file_stem().and_then(|s| s.to_str()).unwrap_or("results");
    let ext = results_path.extension().and_then(|s| s.to_str()).unwrap_or("json");
    let parent = results_path.parent().unwrap_or(Path::new("."));
    parent.join(format!("{stem}.worker{worker_id}.{ext}"))
}

/// Restructure JSONL snapshot capture into versioned JSON.
/// Input format: `[{"test":"name","point":"name","value":...}, ...]`
/// Output format: `{"version":"v1","file":"f.gg","tests":{"test_name":{"point_name":value,...},...}}`
fn restructure_snapshot_capture(raw: &str, version: &str, filename: &str) -> String {
    // Parse the JSON array of capture entries
    // Simple approach: collect test→point→value triples
    let mut tests: std::collections::BTreeMap<String, std::collections::BTreeMap<String, String>> = std::collections::BTreeMap::new();

    let mut pos = 0;
    while let Some(test_pos) = raw[pos..].find("\"test\":\"") {
        let abs = pos + test_pos;
        let t_start = abs + "\"test\":\"".len();
        if let Some(t_end) = raw[t_start..].find('"') {
            let test_name = raw[t_start..t_start + t_end].to_string();
            let after_test = t_start + t_end;

            if let Some(p_pos) = raw[after_test..].find("\"point\":\"") {
                let p_start = after_test + p_pos + "\"point\":\"".len();
                if let Some(p_end) = raw[p_start..].find('"') {
                    let point_name = raw[p_start..p_start + p_end].to_string();
                    let after_point = p_start + p_end;

                    if let Some(v_pos) = raw[after_point..].find("\"value\":") {
                        let v_start = after_point + v_pos + "\"value\":".len();
                        // Find the end of the value — scan for the closing } of this entry
                        if let Some(entry_end) = raw[v_start..].find('}') {
                            let value = raw[v_start..v_start + entry_end].trim().to_string();
                            tests.entry(test_name).or_default().insert(point_name, value);
                            pos = v_start + entry_end + 1;
                            continue;
                        }
                    }
                }
            }
        }
        pos = abs + 1;
    }

    // Build structured JSON
    let mut out = String::new();
    out.push_str("{\n");
    out.push_str(&format!("  \"version\": \"{version}\",\n"));
    out.push_str(&format!("  \"file\": \"{filename}\",\n"));
    out.push_str("  \"tests\": {\n");
    let test_count = tests.len();
    for (ti, (test_name, points)) in tests.iter().enumerate() {
        out.push_str(&format!("    \"{test_name}\": {{\n"));
        let point_count = points.len();
        for (pi, (point_name, value)) in points.iter().enumerate() {
            let comma = if pi + 1 < point_count { "," } else { "" };
            out.push_str(&format!("      \"{point_name}\": {value}{comma}\n"));
        }
        let comma = if ti + 1 < test_count { "," } else { "" };
        out.push_str(&format!("    }}{comma}\n"));
    }
    out.push_str("  }\n}\n");
    out
}

/// Compare two snapshot JSON files and print a human-readable diff.
/// Returns 0 if identical, 1 if different.
fn snapshot_diff(name1: &str, json1: &str, name2: &str, json2: &str) -> i32 {
    // Parse both into test→point→value maps
    fn parse_snapshot(json: &str) -> std::collections::BTreeMap<String, std::collections::BTreeMap<String, String>> {
        let mut tests: std::collections::BTreeMap<String, std::collections::BTreeMap<String, String>> = std::collections::BTreeMap::new();
        // Find the "tests" object
        if let Some(tests_pos) = json.find("\"tests\"") {
            let rest = &json[tests_pos..];
            if let Some(brace) = rest.find('{') {
                let inner = &rest[brace + 1..];
                // Parse test entries — look for "test_name": { ... }
                let mut pos = 0;
                while let Some(key_start) = inner[pos..].find('"') {
                    let abs_ks = pos + key_start + 1;
                    if let Some(key_end) = inner[abs_ks..].find('"') {
                        let test_name = inner[abs_ks..abs_ks + key_end].to_string();
                        let after_key = abs_ks + key_end + 1;
                        // Find opening brace
                        if let Some(ob) = inner[after_key..].find('{') {
                            let obj_start = after_key + ob + 1;
                            // Find matching close brace (simple nesting)
                            let mut depth = 1;
                            let mut obj_end = obj_start;
                            for (i, c) in inner[obj_start..].char_indices() {
                                match c {
                                    '{' => depth += 1,
                                    '}' => { depth -= 1; if depth == 0 { obj_end = obj_start + i; break; } }
                                    _ => {}
                                }
                            }
                            let obj_body = &inner[obj_start..obj_end];
                            // Parse point→value pairs within this test
                            let mut points = std::collections::BTreeMap::new();
                            let mut pp = 0;
                            while let Some(pk_start) = obj_body[pp..].find('"') {
                                let apk = pp + pk_start + 1;
                                if let Some(pk_end) = obj_body[apk..].find('"') {
                                    let point_name = obj_body[apk..apk + pk_end].to_string();
                                    let after_pk = apk + pk_end + 1;
                                    if let Some(colon) = obj_body[after_pk..].find(':') {
                                        let val_start = after_pk + colon + 1;
                                        // Value ends at comma or end of object
                                        let val_rest = obj_body[val_start..].trim_start();
                                        let val_end = val_rest.find('\n')
                                            .or_else(|| val_rest.find(','))
                                            .unwrap_or(val_rest.len());
                                        let value = val_rest[..val_end].trim().trim_end_matches(',').to_string();
                                        points.insert(point_name, value);
                                        pp = val_start + val_end;
                                    } else {
                                        pp = after_pk;
                                    }
                                } else {
                                    break;
                                }
                            }
                            tests.insert(test_name, points);
                            pos = obj_end + 1;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        tests
    }

    let t1 = parse_snapshot(json1);
    let t2 = parse_snapshot(json2);

    println!("Snapshot diff: {name1} vs {name2}\n");

    let mut changed = 0;
    let mut unchanged = 0;

    // Collect all test names
    let all_tests: std::collections::BTreeSet<&String> = t1.keys().chain(t2.keys()).collect();

    for test_name in &all_tests {
        let p1 = t1.get(*test_name);
        let p2 = t2.get(*test_name);

        match (p1, p2) {
            (Some(_), None) => {
                println!("  test \"{test_name}\": REMOVED in {name2}");
                changed += 1;
            }
            (None, Some(_)) => {
                println!("  test \"{test_name}\": NEW in {name2}");
                changed += 1;
            }
            (Some(pts1), Some(pts2)) => {
                let all_points: std::collections::BTreeSet<&String> = pts1.keys().chain(pts2.keys()).collect();
                let mut test_changed = false;
                let mut diffs = Vec::new();
                for point in &all_points {
                    let v1 = pts1.get(*point);
                    let v2 = pts2.get(*point);
                    match (v1, v2) {
                        (Some(a), Some(b)) if a == b => {}
                        (Some(a), Some(b)) => {
                            diffs.push(format!("    \"{}\": {} -> {}", point, a, b));
                            test_changed = true;
                        }
                        (Some(a), None) => {
                            diffs.push(format!("    \"{}\": {} -> (removed)", point, a));
                            test_changed = true;
                        }
                        (None, Some(b)) => {
                            diffs.push(format!("    \"{}\": (new) -> {}", point, b));
                            test_changed = true;
                        }
                        (None, None) => {}
                    }
                }
                if test_changed {
                    println!("  test \"{test_name}\":");
                    for d in &diffs { println!("{d}"); }
                    changed += 1;
                } else {
                    unchanged += 1;
                }
            }
            (None, None) => {}
        }
    }

    if changed == 0 {
        println!("  (identical)");
    }
    println!("\n{changed} test(s) changed, {unchanged} test(s) unchanged");
    if changed > 0 { 1 } else { 0 }
}
