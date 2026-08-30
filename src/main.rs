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
use gorget::proc_guard;
use gorget::resolver;

/// Every legal `--backend` value. This is the ONE source of truth for the
/// accepted set: the parse-time check rejects anything not listed here, and the
/// dispatch in `build` must have an arm for each entry.
///
/// `c` and `c-lir` both select the C backend (`c-lir` is the default and the
/// name the pipeline uses internally; `c` is the shorter alias, also what the
/// hot-reload path substitutes when LLVM cannot serve a shared-library build).
///
/// Adding a backend means adding it here AND giving it a dispatch arm —
/// `backend_flag_set_matches_dispatch` (`tests/lints.rs`) fails if the two
/// disagree, because the failure mode is silent: an unmatched value falls
/// through to C and builds successfully.
const BACKENDS: &[&str] = &["c", "c-lir", "llvm"];

/// Propagate a child process's exit status to `gg`'s own exit, preserving
/// signal-death information the naive `status.code().unwrap_or(1)` pattern
/// erases. Follows the Bash/`cargo run`/`timeout(1)` convention:
/// - signal death → exit `128 + signo`, with a stderr diagnostic naming the
///   signal so an interactive user sees a real error message instead of the
///   silent exit-1 (memory-safety bugs used to be indistinguishable from
///   compile errors at the `gg run` UX — Round XXIV Track B).
/// - normal exit → propagate the child's exit code.
/// - unknown state → exit 1.
fn propagate_child_status(status: std::process::ExitStatus, exe_hint: &str) -> ! {
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signo) = status.signal() {
            let name = signal_name(signo);
            eprintln!("gg: {exe_hint} terminated by {name} (signal {signo})");
            process::exit(128 + signo);
        }
    }
    let _ = exe_hint; // silence unused warning on non-Unix
    process::exit(status.code().unwrap_or(1)); // LINT-CHOKEPOINT-FALLBACK: this IS the chokepoint (non-signal fallback), lint exempts this line
}

// Signal-number → name mapping is Linux-specific for numbers > ~6.
// macOS/BSDs disagree (signal 7 = SIGEMT vs Linux SIGBUS; signal 10 = SIGBUS
// vs Linux SIGUSR1). We bifurcate per target_os so the diagnostic names the
// right signal wherever gg runs. Exit CODE is 128+signo everywhere (that's
// the POSIX contract).
#[cfg(all(unix, target_os = "linux"))]
fn signal_name(signo: i32) -> &'static str {
    match signo {
        1 => "SIGHUP", 2 => "SIGINT", 3 => "SIGQUIT", 4 => "SIGILL",
        5 => "SIGTRAP", 6 => "SIGABRT", 7 => "SIGBUS", 8 => "SIGFPE",
        9 => "SIGKILL", 10 => "SIGUSR1", 11 => "SIGSEGV", 12 => "SIGUSR2",
        13 => "SIGPIPE", 14 => "SIGALRM", 15 => "SIGTERM",
        24 => "SIGXCPU", 25 => "SIGXFSZ",
        _ => "signal",
    }
}

#[cfg(all(unix, any(target_os = "macos", target_os = "freebsd",
                    target_os = "openbsd", target_os = "netbsd")))]
fn signal_name(signo: i32) -> &'static str {
    match signo {
        1 => "SIGHUP", 2 => "SIGINT", 3 => "SIGQUIT", 4 => "SIGILL",
        5 => "SIGTRAP", 6 => "SIGABRT", 7 => "SIGEMT", 8 => "SIGFPE",
        9 => "SIGKILL", 10 => "SIGBUS", 11 => "SIGSEGV", 12 => "SIGSYS",
        13 => "SIGPIPE", 14 => "SIGALRM", 15 => "SIGTERM",
        24 => "SIGXCPU", 25 => "SIGXFSZ",
        _ => "signal",
    }
}

#[cfg(all(unix, not(any(target_os = "linux", target_os = "macos",
                        target_os = "freebsd", target_os = "openbsd",
                        target_os = "netbsd"))))]
fn signal_name(_signo: i32) -> &'static str { "signal" }

/// File info for multi-file error reporting: (display_name, source, base_offset).
type FileInfo = (String, String, usize);

/// Convert main.rs's (filename, source, base_offset) tuples into the typed
/// `span::FileInfo` carried by `LirModule.file_infos`. Used at every LIR
/// construction site so the C backend can resolve panic-site spans to
/// `(file, line, col)` (stack-traces phase 2).
fn to_lir_file_infos(file_infos: &[FileInfo]) -> Vec<gorget::span::FileInfo> {
    file_infos
        .iter()
        .map(|(name, src, off)| gorget::span::FileInfo::new(name.clone(), src.clone(), *off))
        .collect()
}

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

    // Build file info for each module using the offsets the loader assigned.
    // Returning offsets directly (instead of reconstructing them by iterating
    // and bumping +1 per module) eliminates a class of drift bugs: synthetic
    // modules (gen_metal_module, gen_gl_module, …) push an entry into
    // `modules` without claiming a byte range, so the older reconstruction
    // path silently shifted every subsequent file's offset by +1 per
    // synthetic module. Drift accumulated past several synthetic modules
    // (Arena loads gg.metal + gg.gl + gg.sdl) misrouted spans across files
    // — diagnostic warnings about identifiers in `backend.gg` were landing
    // on `md3.gg`'s line numbers. Loader-supplied offsets are authoritative.
    //
    // Filter out empty-source modules: synthetic modules carry the same
    // base_offset as the next real module, so leaving them in `file_ranges`
    // would make the reporter's binary search non-deterministic at that
    // boundary.
    let file_infos: Vec<FileInfo> = modules.iter()
        .filter(|(_path, _seg, src, _mod, _off)| !src.is_empty())
        .map(|(path, _seg, src, _mod, offset)| (path.display().to_string(), src.clone(), *offset))
        .collect();

    (loader::merge_modules(modules), file_infos)
}

/// Resolve package dependencies for a source file, returning dep_paths.
/// Looks for the manifest by walking up from the source file's directory.
fn resolve_deps_for_file(filename: &str) -> HashMap<String, PathBuf> {
    let input_path = Path::new(filename);
    let start_dir = input_path.parent().unwrap_or(Path::new("."));

    if let Some(project_root) = manifest::find_project_root(start_dir) {
        let manifest_path = match manifest::find_manifest_in(&project_root) {
            Some(p) => p,
            None => return HashMap::new(),
        };
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

/// Add pthread linker flags. EVERY native C-backend binary links pthread
/// (both call sites pass `true`): the Task/async scheduler, spawn, and the
/// sync runtime (Mutex/Channel/WaitGroup/etc.) use pthreads, so the link is
/// unconditional even though `main` itself now runs on thread 0 (plain
/// `int main`). The parameter survives for explicitness at the call sites.
/// On macOS, pthreads are part of libc — no extra flag needed.
fn add_thread_flags(_cmd: &mut Command, needs_threads: bool) {
    if !needs_threads { return; }
    #[cfg(not(target_os = "macos"))]
    _cmd.arg("-lpthread");
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

/// Add the `--sanitize` flag set to a cc/link command.
///
/// THE SINGLE SOURCE OF TRUTH for what `--sanitize` spells. Every command that
/// compiles or links a translation unit destined for a user binary routes here
/// — the C backend's exe/shared/hot-reload-guest/hot-reload-host commands and
/// the LLVM backend's runtime `cc -c` and link commands. Before this helper
/// existed the flag set was hand-copied at four C sites and the LLVM pipeline
/// simply had no copy, so `--sanitize --backend=llvm` returned an
/// UNINSTRUMENTED binary while reporting success (t0723). Add a new command
/// that can carry user code and it goes through here, or it is the next hole.
///
/// ⚠ COVERAGE IS NOT UNIFORM ACROSS BACKENDS, and the flag set is not what
/// makes the difference — the compiler each lane hands its user code to is.
/// On the C backend the generated user code IS a C translation unit, so `cc`
/// instruments every user load/store. On the LLVM backend the user code is
/// LLVM IR that `llc` turns into an object file, and `llc` does not run ASan's
/// instrumentation passes — so on that lane only the runtime C blob is
/// shadow-instrumented. What still works there: LeakSanitizer (interceptor-
/// based, so complete), and any heap error whose faulting access is inside the
/// runtime. What does NOT: a use-after-free, overflow or stack error whose
/// faulting access sits in generated user code. See `todo/t0727` and
/// `docs/devbook/19-llvm-backend.md`.
fn add_sanitize_flags(cmd: &mut Command, sanitize: bool) {
    if !sanitize { return; }
    cmd.arg("-fsanitize=address,undefined");
    cmd.arg("-fno-omit-frame-pointer");
    cmd.arg("-g");
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

/// Bitset of selected `--resolvers[=MODE]` modes. Place-resolver fall-through
/// worklist only — **never a correctness gate (Core #13)**. `Some(wrong_root)`
/// counts as resolved; only instrument C (build-and-run) adjudicates landing.
/// Default-silent; bare `--resolvers` ⇒ `hist`.
#[derive(Debug, Clone, Default)]
struct ResolverDiagModes {
    /// Ranked histogram of unresolved shape chains after lower.
    hist: bool,
    /// Per-site log with span.
    sites: bool,
    /// Full histogram dump for scripts.
    hist_tsv: Option<PathBuf>,
}

/// Parse `--resolvers[=MODE[,MODE…]]`. Modes: `hist` (default), `sites`,
/// `hist-tsv=PATH`, `all` (= hist+sites). Mirrors [`parse_clone_modes`].
fn parse_resolver_modes(args: &[String]) -> Result<ResolverDiagModes, String> {
    let mut modes = ResolverDiagModes::default();
    for a in args {
        let body = if a == "--resolvers" {
            "hist"
        } else if let Some(v) = a.strip_prefix("--resolvers=") {
            v
        } else {
            continue;
        };
        for tok in body.split(',') {
            match tok.trim() {
                "" => continue,
                "hist" => modes.hist = true,
                "sites" => modes.sites = true,
                "all" => {
                    modes.hist = true;
                    modes.sites = true;
                }
                "hist-tsv" => {
                    return Err(
                        "--resolvers=hist-tsv requires a path: --resolvers=hist-tsv=PATH"
                            .to_string(),
                    )
                }
                tsv if tsv.starts_with("hist-tsv=") => {
                    let path = &tsv["hist-tsv=".len()..];
                    if path.is_empty() {
                        return Err(
                            "--resolvers=hist-tsv requires a path: --resolvers=hist-tsv=PATH"
                                .to_string(),
                        );
                    }
                    modes.hist = true; // hist is implied for the dump
                    modes.hist_tsv = Some(PathBuf::from(path));
                }
                other => {
                    return Err(format!(
                        "Unknown --resolvers mode '{other}'. Valid modes: hist, sites, hist-tsv=PATH, all. \
                         ⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13)."
                    ))
                }
            }
        }
    }
    Ok(modes)
}

/// Bitset of selected `--clones[=MODE]` modes. All clone diagnostics — both the
/// compile-time per-site report and the runtime `[clone-stats]` line — live
/// under this one flag and are default-silent. `all` is shorthand for
/// `verbose,stats`. The pre-unification spellings `--show-clones` and
/// `--clone-stats` are retired (see [`parse_clone_modes`]).
#[derive(Debug, Clone, Default)]
struct CloneDiagModes {
    /// Compact compile-time report: file:line:col  type  reason.
    sites: bool,
    /// Compile-time report with size_bytes + runtime_fn columns. Subsumes the
    /// historical `--trace-cow` plan (no separate flag was ever shipped).
    verbose: bool,
    /// Runtime instrumentation: the compiled binary carries per-CloneId
    /// counters (`__gorget_clone_site_hit` bumps emitted before each implicit
    /// clone) and an atexit handler emitting the aggregate `[clone-stats] …`
    /// line plus the per-site `[clone-sites]`/`[clone-site] #id=count` report.
    stats: bool,
    /// `--clones=sites-tsv=PATH`: dump EVERY CloneId (no span dedup —
    /// monomorphized siblings share a span but have distinct ids) as TSV
    /// (id, file, line, col, type, reason, size_bytes, runtime_fn) to PATH.
    /// This is the static half of the per-site attribution join: `join` the
    /// TSV's id column against the runtime `[clone-site] #id=count` lines a
    /// `--clones=stats` binary prints at exit.
    sites_tsv: Option<PathBuf>,
}

/// Parse `--clones[=MODE[,MODE…]]`. Recognised modes are `sites`, `verbose`,
/// `stats`, `all`; a bare `--clones` defaults to `sites`. Returns `Err(msg)` on
/// an unknown mode, or on the retired `--show-clones` / `--clone-stats`
/// spellings (with a message pointing at the replacement) — a clean error
/// rather than silently ignoring a flag the user expects to do something.
fn parse_clone_modes(args: &[String]) -> Result<CloneDiagModes, String> {
    let mut modes = CloneDiagModes::default();

    // Retired aliases — fail loudly with the migration path rather than
    // silently no-op'ing (the general arg loop below would otherwise skip them).
    if args.iter().any(|a| a == "--show-clones") {
        return Err("`--show-clones` was removed; use `--clones=sites` (or `--clones`).".to_string());
    }
    if args.iter().any(|a| a == "--clone-stats") {
        return Err("`--clone-stats` was removed; use `--clones=stats`.".to_string());
    }

    for a in args {
        let body = if a == "--clones" {
            // Bare flag → default to `sites`.
            "sites"
        } else if let Some(v) = a.strip_prefix("--clones=") {
            v
        } else {
            continue;
        };
        for tok in body.split(',') {
            match tok.trim() {
                "" => continue,
                "sites" => modes.sites = true,
                "verbose" => modes.verbose = true,
                "stats" => modes.stats = true,
                "all" => { modes.verbose = true; modes.stats = true; }
                "sites-tsv" => return Err(
                    "--clones=sites-tsv requires a path: --clones=sites-tsv=PATH".to_string()
                ),
                tsv if tsv.starts_with("sites-tsv=") => {
                    let path = &tsv["sites-tsv=".len()..];
                    if path.is_empty() {
                        return Err(
                            "--clones=sites-tsv requires a path: --clones=sites-tsv=PATH".to_string()
                        );
                    }
                    modes.sites_tsv = Some(PathBuf::from(path));
                }
                other => return Err(format!(
                    "Unknown --clones mode '{other}'. Valid modes: sites, verbose, stats, sites-tsv=PATH, all"
                )),
            }
        }
    }
    Ok(modes)
}

/// True if a semantic warning is a clone diagnostic that belongs under the
/// `--clones` umbrella rather than the always-on warning stream. Today that is
/// exactly `CowBorrowMutation` (the CoW system inserts an element clone when a
/// collection is mutated while an element borrow is live). It is informational,
/// not actionable — the clone is correct by design — so it is default-silent
/// like the rest of System A's clone report; `--clones=sites`/`verbose` surfaces
/// it. All other semantic warnings always display.
fn is_clone_diagnostic(warn: &gorget::semantic::errors::SemanticWarning) -> bool {
    matches!(
        warn.kind,
        gorget::semantic::errors::SemanticWarningKind::CowBorrowMutation { .. }
    )
}

/// Display the non-fatal semantic warnings, filtering out the clone diagnostics
/// (see [`is_clone_diagnostic`]) unless `show_clones` requested them. The clone
/// diagnostics are folded into the dedicated clone report on the build path; on
/// `check` (no lowering, no clone report) they are surfaced here through
/// the normal warning reporter when `show_clones` is set, and suppressed
/// otherwise. Keeps a plain `gg build`/`gg check` clone-silent.
fn report_semantic_warnings_filtered(
    reporter: &ErrorReporter,
    warnings: &[gorget::semantic::errors::SemanticWarning],
    show_clones: bool,
) {
    for warn in warnings {
        if is_clone_diagnostic(warn) && !show_clones {
            continue;
        }
        reporter.report_semantic_warning(warn);
    }
}

/// Build a .gg source file: parse → analyze → GIR → LIR → C → binary.
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
    clone_modes: &CloneDiagModes,
    resolver_modes: &ResolverDiagModes,
    backend_name: &str,
    target: &str,
) -> Result<PathBuf, String> {
    let show_clones = clone_modes.sites || clone_modes.verbose;
    let clones_verbose = clone_modes.verbose;
    let clone_stats = clone_modes.stats;
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

    // Display warnings (non-fatal). Clone diagnostics (CowBorrowMutation) are
    // filtered out here unconditionally on the build path — they are folded
    // into the dedicated Clone Report below (shown only under `--clones`), so a
    // plain build stays clone-silent.
    if !result.warnings.is_empty() {
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        for warn in &result.warnings {
            if is_clone_diagnostic(warn) {
                continue;
            }
            reporter.report_semantic_warning(warn);
        }
    }

    // Lower AST to GIR. `--clones=stats` also arms per-clone-site runtime
    // attribution in the lowering (see LoweringOptions::clone_stats).
    // `--resolvers` arms place-resolver fall-through bookkeeping (worklist only).
    let mut options = options;
    options.clone_stats = clone_stats;
    options.resolver_hist = resolver_modes.hist || resolver_modes.hist_tsv.is_some();
    options.resolver_sites = resolver_modes.sites;
    let mut gir_module = gorget::ir::lowering::lower_module(&module, &result, &options);

    // Place-resolver fall-through report (`--resolvers`). MUST run after lower
    // — `gg check` never lowers, so this instrument is build-path only.
    // ⚠ WORKLIST GENERATOR, never a correctness gate (Core #13).
    if resolver_modes.hist
        || resolver_modes.sites
        || resolver_modes.hist_tsv.is_some()
    {
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        let locate = |span_start: usize| {
            reporter.span_location(gorget::span::Span {
                start: span_start,
                end: span_start,
            })
        };
        gorget::ir::lowering::emit_resolver_report(
            &gir_module.resolver_miss_hist,
            &gir_module.resolver_miss_sites,
            resolver_modes.hist || resolver_modes.hist_tsv.is_some(),
            resolver_modes.sites,
            resolver_modes.hist_tsv.as_deref(),
            &locate,
        );
    }

    // `--clones=sites-tsv=PATH` (per-site attribution join table): dump EVERY
    // CloneId (no span dedup — monomorphized siblings share a span but have
    // distinct ids) as TSV: id, file, line, col, type, reason, size_bytes,
    // runtime_fn. Joined offline against the runtime `[clone-site] #id=count`
    // lines a `--clones=stats` binary emits at exit (join key: the id column).
    if let Some(tsv_path) = &clone_modes.sites_tsv {
        use std::io::Write as _;
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        let mut out = String::new();
        for warn in &gir_module.implicit_clone_warnings {
            let (file, line, col) = reporter.span_location(warn.span);
            out.push_str(&format!(
                "{}\t{}\t{}\t{}\t{}\t{:?}\t{}\t{}\n",
                warn.id.0, file, line, col, warn.type_name, warn.reason,
                warn.size_bytes, warn.runtime_fn,
            ));
        }
        if let Err(e) = std::fs::File::create(tsv_path).and_then(|mut f| f.write_all(out.as_bytes())) {
            eprintln!("warning: failed to write --clones=sites-tsv={}: {e}", tsv_path.display());
        }
    }

    // Display clone report when `--clones=sites` / `--clones=verbose` (or
    // legacy `--show-clones`) is passed. Both modes consume the same
    // ImplicitCloneWarning vector; `verbose` adds size_bytes + runtime_fn
    // columns. One source of truth, mode-selected rendering.
    if show_clones {
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        let mut shown = std::collections::HashSet::new();
        struct CloneEntry {
            file: String,
            line: usize,
            col: usize,
            /// `None` for clone sites detected by the semantic pass (CoW element
            /// mutation), which predate the lowering-time CloneId allocation.
            id: Option<u32>,
            type_name: String,
            reason: String,
            size_bytes: usize,
            runtime_fn: String,
        }
        let mut entries: Vec<CloneEntry> = Vec::new();
        for warn in &gir_module.implicit_clone_warnings {
            if !shown.insert(warn.span.start) {
                continue;
            }
            let (file, line, col) = reporter.span_location(warn.span);
            let reason = match &warn.reason {
                gorget::ir::ImplicitCloneReason::VarDeclFromBorrow => "variable declaration from borrow",
                gorget::ir::ImplicitCloneReason::NamedToNamed => "named-to-named assignment",
                gorget::ir::ImplicitCloneReason::ReturnFromBorrow => "return from borrow",
                gorget::ir::ImplicitCloneReason::MoveParamFromBorrow => "move param from borrow",
                gorget::ir::ImplicitCloneReason::StructFieldFromBorrow => "struct field from borrow",
                gorget::ir::ImplicitCloneReason::CoWMaterialization => "CoW materialization",
                gorget::ir::ImplicitCloneReason::ClosureCapture => "closure capture",
                gorget::ir::ImplicitCloneReason::PatternExtraction => "pattern extraction",
                gorget::ir::ImplicitCloneReason::ConsumingArg => "consuming argument",
                gorget::ir::ImplicitCloneReason::CallArg => "call argument",
                gorget::ir::ImplicitCloneReason::BorrowedExternReturn => "borrowed extern return",
                gorget::ir::ImplicitCloneReason::LoopPreHeaderMaterialize => "loop pre-header materialize",
                gorget::ir::ImplicitCloneReason::BranchPreHeaderMaterialize => "branch pre-header materialize",
                gorget::ir::ImplicitCloneReason::ExplicitUserClone => "explicit .clone()",
                gorget::ir::ImplicitCloneReason::NeedsClassification => "unclassified clone",
            };
            entries.push(CloneEntry {
                file, line, col,
                id: Some(warn.id.0),
                type_name: warn.type_name.clone(),
                reason: reason.to_string(),
                size_bytes: warn.size_bytes,
                runtime_fn: warn.runtime_fn.clone(),
            });
        }
        // Fold in the semantic-pass clone diagnostics (CoW element-mutation).
        // These live in the safety pass (they need borrow-source tracking that
        // lowering lacks) so they carry variable-name context instead of a
        // CloneId/runtime_fn; render them in the same table for one unified view.
        for warn in &result.warnings {
            if !is_clone_diagnostic(warn) {
                continue;
            }
            if !shown.insert(warn.span.start) {
                continue;
            }
            let (file, line, col) = reporter.span_location(warn.span);
            let reason = match &warn.kind {
                gorget::semantic::errors::SemanticWarningKind::CowBorrowMutation { source, borrow } => {
                    format!("CoW element mutation (`{source}` mutated while `{borrow}` held)")
                }
                _ => continue,
            };
            entries.push(CloneEntry {
                file, line, col,
                id: None,
                type_name: String::from("-"),
                reason,
                size_bytes: 0,
                runtime_fn: String::new(),
            });
        }
        entries.sort_by(|a, b| a.line.cmp(&b.line).then(a.col.cmp(&b.col)));
        let n = entries.len();
        let header = if clones_verbose { "Clone Report (verbose)" } else { "Clone Report" };
        eprintln!("\n=== {header} ({n} implicit clone{}) ===", if n == 1 { "" } else { "s" });
        if clones_verbose {
            // file:line:col  id   type             reason                            size  runtime_fn
            for e in &entries {
                let size_str = if e.size_bytes == 0 { String::from("-") } else { e.size_bytes.to_string() };
                let rt = if e.runtime_fn.is_empty() { "-" } else { e.runtime_fn.as_str() };
                let id_str = e.id.map_or_else(|| String::from("-"), |id| format!("#{id}"));
                eprintln!(
                    "  {file}:{line}:{col}  {id:<5} {type_name:<16} {reason:<48} {size:>4}  {rt}",
                    file = e.file, line = e.line, col = e.col,
                    id = id_str, type_name = e.type_name, reason = e.reason,
                    size = size_str, rt = rt,
                );
            }
        } else {
            for e in &entries {
                eprintln!("  {file}:{line}:{col}  {type_name:<16} {reason}",
                    file = e.file, line = e.line, col = e.col,
                    type_name = e.type_name, reason = e.reason);
            }
        }
        eprintln!();
    }

    // Run GIR optimization passes
    let opt_stats = gorget::ir::transforms::optimize::optimize_module(&mut gir_module);
    let _ = opt_stats; // available for --emit-gir stats or future --verbose

    // Dump GIR text if requested
    if emit_gir {
        print!("{}", gorget::ir::printer::print_module(&gir_module));
        if opt_stats.insts_eliminated() > 0 || opt_stats.blocks_eliminated() > 0 {
            eprintln!("; Optimization: {} blocks, {} instructions, {} locals eliminated",
                opt_stats.blocks_eliminated(), opt_stats.insts_eliminated(), opt_stats.locals_eliminated());
            for (name, stats) in &opt_stats.per_pass {
                if stats.insts_eliminated > 0 {
                    eprintln!(";   {name}: {} insts", stats.insts_eliminated);
                }
            }
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
        lir_module.file_infos = to_lir_file_infos(&file_infos);
        if pre_ssa {
            print!("{}", gorget::lir::display::dump_module(&lir_module));
            let input_path = Path::new(filename);
            let stem = input_path.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
            return Ok(PathBuf::from(stem));
        }
        let no_opt = std::env::var("LIR_NO_OPT").is_ok();
        // Tier E §8.2: split critical edges before SSA so Braun et al. SSA
        // construction (which assumes no critical edges) can run.
        gorget::lir::split_edges::split_critical_edges_module(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "lir-lowering");
        for func in &mut lir_module.functions {
            gorget::lir::ssa::construct_ssa(func);
        }
        gorget::lir::validate::assert_module_valid(&lir_module, "ssa-construction");
        if !no_opt {
            let stats = gorget::lir::optimize::optimize_module(&mut lir_module);
            eprintln!("; LIR opt: {} dead fns, {} dead globals, {} dead insts, {} folded, {} copies prop'd, {} drops elab'd, {} memsets rm'd, {} flags, {} moves",
                stats.dead_functions_eliminated, stats.dead_globals_eliminated,
                stats.dead_instructions_eliminated, stats.constants_folded,
                stats.copies_propagated, stats.drops_elaborated, stats.memsets_removed,
                stats.drop_flags_inserted, stats.move_slots_removed);
            gorget::lir::validate::assert_module_valid(&lir_module, "optimize");
        }
        gorget::lir::types::wire_collection_bridges(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "wire-collection-bridges");
        gorget::lir::runtime::promote_runtime_calls(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "promote-runtime-calls");
        // Order matters: pointee_types FIRST so value_types can fall back
        // through it for `Inst::Load { ty: Void }` (matches the C backend's
        // local single-pass behaviour, now consolidated upstream).
        gorget::lir::types::compute_module_pointee_types(&mut lir_module);
        gorget::lir::types::compute_module_value_types(&mut lir_module);
        gorget::lir::types::compute_module_value_origins(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "compute-types");
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
        lir_module.file_infos = to_lir_file_infos(&file_infos);
        // Tier E §8.2: critical-edge split before SSA construction.
        gorget::lir::split_edges::split_critical_edges_module(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "lir-lowering");
        for func in &mut lir_module.functions {
            gorget::lir::ssa::construct_ssa(func);
        }
        gorget::lir::validate::assert_module_valid(&lir_module, "ssa-construction");
        gorget::lir::types::wire_collection_bridges(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "wire-collection-bridges");
        gorget::lir::runtime::promote_runtime_calls(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "promote-runtime-calls");
        // Order matters: pointee_types FIRST so value_types can fall back
        // through it for `Inst::Load { ty: Void }` (matches the C backend's
        // local single-pass behaviour, now consolidated upstream).
        gorget::lir::types::compute_module_pointee_types(&mut lir_module);
        gorget::lir::types::compute_module_value_types(&mut lir_module);
        gorget::lir::types::compute_module_value_origins(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "compute-types-pre-bir");
        let mut bir_module = gorget::bir::BirModule::from_lir(lir_module)
            .map_err(|e| format!("BIR lowering failed: {e}"))?;
        gorget::lir::split_edges::split_critical_edges_module(bir_module.as_lir_mut());
        gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "bir-lowering");
        // Optimize runs post-BIR so synth fns (when present) get DCE/fold/CSE.
        gorget::lir::optimize::optimize_module(bir_module.as_lir_mut());
        gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "optimize");
        gorget::lir::types::compute_module_pointee_types(bir_module.as_lir_mut());
        gorget::lir::types::compute_module_value_types(bir_module.as_lir_mut());
        gorget::lir::types::compute_module_value_origins(bir_module.as_lir_mut());
        gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "compute-types-post-bir");
        let c_code = gorget::backend::c_lir::generate_c(bir_module.as_lir());
        print!("{c_code}");
        let input_path = Path::new(filename);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        return Ok(PathBuf::from(stem));
    }

    // ── LIR → BIR → target source → binary ──────────
    // Lower GIR → LIR → SSA → value_types → BIR synthesis → optimize → backend
        let mut lir_module = gorget::lir::lower::lower_module(&gir_module);
        lir_module.target = target.to_string();
        lir_module.clone_stats = clone_stats;
        // Per-site attribution: CloneIds are dense 0..N (one per warning), so
        // the warning count sizes the runtime counter table.
        lir_module.clone_site_count = gir_module.implicit_clone_warnings.len();
        lir_module.file_infos = to_lir_file_infos(&file_infos);
        // Tier E §8.2: critical-edge split before SSA construction.
        gorget::lir::split_edges::split_critical_edges_module(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "lir-lowering");
        for func in &mut lir_module.functions {
            gorget::lir::ssa::construct_ssa(func);
        }
        gorget::lir::validate::assert_module_valid(&lir_module, "ssa-construction");
        gorget::lir::types::wire_collection_bridges(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "wire-collection-bridges");
        gorget::lir::runtime::promote_runtime_calls(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "promote-runtime-calls");
        // Order matters: pointee_types FIRST so value_types can fall back
        // through it for `Inst::Load { ty: Void }` (matches the C backend's
        // local single-pass behaviour, now consolidated upstream).
        gorget::lir::types::compute_module_pointee_types(&mut lir_module);
        gorget::lir::types::compute_module_value_types(&mut lir_module);
        gorget::lir::types::compute_module_value_origins(&mut lir_module);
        gorget::lir::validate::assert_module_valid(&lir_module, "compute-types-pre-bir");

        // Save metadata we need after handing ownership to BirModule.
        let mut bir_module = gorget::bir::BirModule::from_lir(lir_module)
            .map_err(|e| format!("BIR lowering failed: {e}"))?;
        gorget::lir::split_edges::split_critical_edges_module(bir_module.as_lir_mut());
        gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "bir-lowering");
        // Optimize post-BIR so synth fns get DCE/fold/CSE, and so drop-elab
        // sees the expanded primitives from canonical ops (HofExpand, EnumInit,
        // etc.) rather than the opaque high-level shape.
        gorget::lir::optimize::optimize_module(bir_module.as_lir_mut());
        gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "optimize");
        gorget::lir::types::compute_module_pointee_types(bir_module.as_lir_mut());
        gorget::lir::types::compute_module_value_types(bir_module.as_lir_mut());
        gorget::lir::types::compute_module_value_origins(bir_module.as_lir_mut());
        gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "compute-types-post-bir");

        // Hot-reload's two-phase build (host binary + guest .dylib) leans on
        // C-level syntax — `generate_hot_reload_split` searches for `int main(`
        // and bracket-matches the body, then injects `__attribute__((visibility))`
        // wrappers and a dlopen/dlsym glue main. None of that ports to LLVM IR
        // without re-implementing the split at IR level. Fall back to the C
        // backend transparently when hot-reload is requested under
        // `--backend=llvm`, so the test fixture and the dev workflow keep
        // working — with the same binary semantics either way.
        let lir_meta = bir_module.as_lir();
        let needs_c_for_hot_reload = lir_meta.hot_reload && backend_name == "llvm";
        let effective_backend = if needs_c_for_hot_reload { "c" } else { backend_name };
        let backend: Box<dyn gorget::backend::Backend> = match effective_backend {
            "llvm" => Box::new(gorget::backend::llvm::LlvmBackend),
            _ => Box::new(gorget::backend::c_lir::CLirBackend),
        };
        let output = gorget::backend::Backend::generate(backend.as_ref(), &bir_module);
        let generated_code = output.code;
        let code_ext = output.extension;
        // Keep a LIR reference for metadata (hot_reload flags, target, etc.).
        let lir_module = bir_module.as_lir();

        // Determine output paths
        let input_path = Path::new(filename);
        let default_dir = input_path.parent().unwrap_or(Path::new("."));
        let dir = output_dir.unwrap_or(default_dir);
        let stem = input_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        let (src_path, exe_path) = if let Some(out) = output_exe {
            let out = std::path::absolute(out).unwrap_or(out.to_path_buf());
            let src_path = out.with_extension(code_ext);
            (src_path, out)
        } else {
            let src_path = dir.join(format!("{stem}.{code_ext}"));
            let exe_path = dir.join(stem);
            let exe_path = std::path::absolute(&exe_path).unwrap_or(exe_path);
            (src_path, exe_path)
        };

        // ── --shared: build as shared library (used by hot-reload recompile) ──
        if let Some(shared_path) = shared_output {
            let shared_c_code = if lir_module.hot_reload {
                let state_type = lir_module.hot_reload_state_type.as_deref().unwrap_or("State");
                let (_, guest) = gorget::backend::generate_hot_reload_split(
                    &generated_code, state_type, lir_module.hot_reload_state_hash,
                    lir_module.hot_reload_has_reload_fn, None,
                );
                guest
            } else {
                generated_code.clone()
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
                .arg("-Werror=implicit-function-declaration")
                .arg("-Wno-unused-parameter")
                .arg("-Wno-unused-variable")
                .arg("-Wno-unused-function")
                .arg("-Wno-unused-but-set-variable")
                .arg("-o")
                .arg(shared_path)
                .arg(&shared_c_path)
                .arg("-lm");
            add_sanitize_flags(&mut cc_cmd, options.sanitize);
            add_sdl_flags(&mut cc_cmd, concat_source.contains("xtd.sdl") || concat_source.contains("xtd.gfx"), &shared_c_code);
            add_tls_flags(&mut cc_cmd, concat_source.contains("std.net.tls") || concat_source.contains("xtd.http"));
            add_crypto_flags(&mut cc_cmd, concat_source.contains("xtd.crypto") || concat_source.contains("xtd.p2p"));
            // UNCONDITIONAL — the Task/async scheduler, spawn, and the sync
            // runtime use pthreads, so every C-backend binary links pthread,
            // not just std.async/p2p users (`main` itself runs on thread 0).
            // (The test harness passes -lpthread itself, which would MASK a
            // conditional-link break — keep this unconditional; the
            // freestanding path early-returns before reaching here.)
            add_thread_flags(&mut cc_cmd, true);
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
                std::env::current_exe()
                    .map(|p| {
                        // On Linux, /proc/self/exe can return "path (deleted)" when
                        // the binary was replaced. Strip the suffix to get the real path.
                        let s = p.display().to_string();
                        if let Some(stripped) = s.strip_suffix(" (deleted)") {
                            PathBuf::from(stripped)
                        } else {
                            p
                        }
                    })
                    .unwrap_or_else(|_| PathBuf::from("gg")).display(),
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
                &generated_code, state_type, lir_module.hot_reload_state_hash,
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
                .arg("-Werror=implicit-function-declaration")
                .arg("-Wno-unused-parameter").arg("-Wno-unused-variable").arg("-Wno-unused-function")
                .arg("-Wno-unused-but-set-variable")
                .arg("-o").arg(&guest_lib_path)
                .arg(&guest_c_path).arg("-lm");
            add_sanitize_flags(&mut guest_cmd, options.sanitize);
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
                .arg("-Werror=implicit-function-declaration")
                .arg("-Wno-unused-parameter").arg("-Wno-unused-variable").arg("-Wno-unused-function")
                .arg("-Wno-unused-but-set-variable")
                .arg("-o").arg(&exe_path)
                .arg(&host_c_path).arg("-lm").arg("-ldl");
            add_sanitize_flags(&mut host_cmd, options.sanitize);
            let host_status = host_cmd.status();
            return match host_status {
                Ok(s) if s.success() => Ok(exe_path),
                Ok(s) => Err(format!("Host compilation failed: {s}\nGenerated: {}", host_c_path.display())),
                Err(e) => Err(format!("Failed to run '{cc}': {e}")),
            };
        }

        // ── Normal LIR build ──
        if let Err(e) = fs::write(&src_path, &generated_code) {
            return Err(format!("Error writing {}: {e}", src_path.display()));
        }

        // ── LLVM backend: .ll → clang -c → link with runtime .o → binary ──
        if backend_name == "llvm" {
            return compile_llvm_pipeline(&src_path, &exe_path, &generated_code, &concat_source, &lir_module, options.release, options.sanitize);
        }

        // ── C backend: .c → cc → binary ──
        let is_freestanding = target.starts_with("freestanding");
        let cc = if is_freestanding {
            env::var("CC").unwrap_or_else(|_| "clang".to_string())
        } else {
            env::var("CC").unwrap_or_else(|_| "cc".to_string())
        };
        let mut cc_cmd = Command::new(&cc);
        let needs_metal = concat_source.contains("xtd.metal");

        if is_freestanding {
            // Freestanding: UEFI PE application, no libc, no stdlib.
            // --target freestanding          → host arch (aarch64 on Apple Silicon, x86_64 otherwise)
            // --target freestanding-x86_64   → cross-compile to x86_64
            // --target freestanding-aarch64  → cross-compile to aarch64
            let freestanding_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("lib/freestanding");
            let efi_path = exe_path.with_extension("efi");

            let is_aarch64 = target == "freestanding-aarch64"
                || (target == "freestanding" && cfg!(target_arch = "aarch64"));

            let (clang_target, boot_name) = if is_aarch64 {
                ("aarch64-unknown-windows", "BOOTAA64.EFI")
            } else {
                ("x86_64-unknown-windows", "BOOTX64.EFI")
            };
            let qemu_bin = if is_aarch64 { "qemu-system-aarch64" } else { "qemu-system-x86_64" };

            cc_cmd
                .arg("-std=c11")
                .arg("-target").arg(clang_target)
                .arg("-ffreestanding")
                .arg("-nostdlib")
                .arg("-fno-stack-protector");
            if !is_aarch64 {
                cc_cmd.arg("-mno-red-zone");
            }
            cc_cmd
                .arg("-Wall")
                // NOTE: no -Werror=implicit-function-declaration here. This is the
                // freestanding/UEFI build (-ffreestanding -nostdlib, no libc); it
                // intentionally lives in a different no-libc world where libc
                // prototypes are absent by design, so the flag is N/A.
                .arg("-Wno-unused-parameter")
                .arg("-Wno-unused-variable")
                .arg("-Wno-unused-function")
                .arg("-Wno-unused-label")
                .arg("-Wno-unused-but-set-variable")
                .arg("-Wno-sometimes-uninitialized")
                .arg("-Wno-unknown-warning-option")
                .arg(format!("-I{}", freestanding_dir.display()))
                .arg("-o")
                .arg(&efi_path)
                .arg(&src_path)
                .arg(freestanding_dir.join("uefi_stub.c"))
                .arg("-Wl,-subsystem:efi_application")
                .arg("-Wl,-entry:efi_main")
                .arg("-fuse-ld=lld");
            let status = cc_cmd.status();
            let esp_dir = efi_path.parent().unwrap_or(Path::new(".")).join("esp/EFI/BOOT");
            return match status {
                Ok(s) if s.success() => {
                    let _ = fs::create_dir_all(&esp_dir);
                    let boot_path = esp_dir.join(boot_name);
                    let _ = fs::copy(&efi_path, &boot_path);
                    let esp_root = esp_dir.parent().unwrap().parent().unwrap();
                    eprintln!("Built UEFI application: {}", efi_path.display());
                    eprintln!("ESP directory: {}", esp_root.display());
                    if is_aarch64 {
                        eprintln!("Run: {qemu_bin} -M virt -cpu cortex-a72 -bios AAVMF_CODE.fd -drive format=raw,file=fat:rw:{} -m 128M -device ramfb", esp_root.display());
                    } else {
                        eprintln!("Run: {qemu_bin} -bios OVMF.fd -drive format=raw,file=fat:rw:{} -m 128M -vga std", esp_root.display());
                    }
                    Ok(efi_path)
                }
                Ok(s) => Err(format!(
                    "C compiler exited with: {s}\nGenerated source file: {}\nNote: freestanding target requires clang with lld.\nInstall: brew install llvm (macOS) or apt install clang lld (Linux)",
                    src_path.display()
                )),
                Err(e) => Err(format!(
                    "Failed to run C compiler '{cc}': {e}\nNote: freestanding target requires clang.\nInstall: brew install llvm (macOS) or apt install clang lld (Linux)",
                )),
            };
        }

        cc_cmd
            .arg("-std=c11")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Werror=implicit-function-declaration") // hard-fail on calls to undeclared fns (caught the strptime bug -w was hiding)
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
        // --release: optimize the generated C at -O2. The default (no flag)
        // stays at the compiler's implicit -O0 for fast, debuggable builds.
        // -O2 changes only codegen, never observable program behavior.
        if options.release {
            cc_cmd.arg("-O2");
        }
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
        cc_cmd.arg(&src_path)
            .arg("-lm");

        #[cfg(not(target_os = "macos"))]
        cc_cmd.arg("-Wl,--gc-sections");
        #[cfg(target_os = "macos")]
        cc_cmd.arg("-Wl,-dead_strip");

        add_sanitize_flags(&mut cc_cmd, options.sanitize);

        // Library detection — use generated C for precise SDL sub-library detection
        add_sdl_flags(&mut cc_cmd, concat_source.contains("xtd.sdl") || concat_source.contains("xtd.gfx") || concat_source.contains("xtd.gl") || needs_metal, &generated_code);
        add_gl_flags(&mut cc_cmd, concat_source.contains("xtd.gl"));
        add_audio_flags(&mut cc_cmd, concat_source.contains("xtd.audio"));
        add_compress_flags(&mut cc_cmd, concat_source.contains("xtd.compress"));
        add_metal_flags(&mut cc_cmd, needs_metal);
        add_tls_flags(&mut cc_cmd, concat_source.contains("std.net.tls") || concat_source.contains("xtd.http"));
        add_crypto_flags(&mut cc_cmd, concat_source.contains("xtd.crypto") || concat_source.contains("xtd.p2p"));
        // UNCONDITIONAL — the Task/async scheduler, spawn, and the sync
        // runtime use pthreads, so every C-backend binary links pthread,
        // not just std.async/p2p users (`main` itself runs on thread 0).
        // (The test harness passes -lpthread itself, which would MASK a
        // conditional-link break — keep this unconditional; the
        // freestanding path early-returns before reaching here.)
        add_thread_flags(&mut cc_cmd, true);

        let status = cc_cmd.status();
        return match status {
            Ok(s) if s.success() => Ok(exe_path),
            Ok(s) => Err(format!(
                "C compiler exited with: {s}\nGenerated source file (LIR): {}",
                src_path.display()
            )),
            Err(e) => Err(format!(
                "Failed to run C compiler '{cc}': {e}\nGenerated source file (LIR): {}",
                src_path.display()
            )),
    }
}

/// True when the configured `llc` is LLVM <15, which still requires
/// `-opaque-pointers` to accept bare `ptr` types. Probed once via `--version`;
/// failure to parse falls open (we don't add the flag) so we don't break
/// LLVM 22+ where the flag was removed.
fn llc_needs_opaque_pointers_flag(llc: &str) -> bool {
    let out = match Command::new(llc).arg("--version").output() {
        Ok(o) => o,
        Err(_) => return false,
    };
    let text = String::from_utf8_lossy(&out.stdout);
    // Format: "LLVM version 14.0.6" / "Debian LLVM version 14.0.6"
    for line in text.lines() {
        if let Some(rest) = line.split("LLVM version").nth(1) {
            if let Some(major) = rest
                .trim()
                .split('.')
                .next()
                .and_then(|s| s.parse::<u32>().ok())
            {
                return major < 15;
            }
        }
    }
    false
}

/// LLVM backend compilation pipeline: .ll → clang -c → link with runtime .o → binary
fn compile_llvm_pipeline(
    ll_path: &Path,
    exe_path: &Path,
    _ll_code: &str,
    concat_source: &str,
    lir_module: &gorget::lir::LirModule,
    release: bool,
    sanitize: bool,
) -> Result<PathBuf, String> {
    let tmp_dir = ll_path.parent().unwrap_or(Path::new("."));

    // Per-fixture intermediate filenames keep parallel `gg build` invocations
    // from clobbering each other's runtime .c / .o (and the LLVM .o).
    // Integration tests run --test-threads up to N and would otherwise race
    // through the same `__gorget_runtime.c` path → undefined-reference link
    // failures because one process truncated the file mid-compile of another.
    let stem = ll_path.file_stem().and_then(|s| s.to_str()).unwrap_or("gg");

    // Step 1: Write the C runtime to a temporary file and compile it
    let runtime_c_path = tmp_dir.join(format!("__gorget_runtime_{stem}.c"));
    let mut runtime_src = String::with_capacity(256 * 1024);
    // Include all runtime modules needed for a basic program
    use gorget::backend::c::c_runtime;
    runtime_src.push_str(c_runtime::RUNTIME_PREAMBLE);
    // Non-test modules get the simple panic handler first (before string runtime).
    if !(lir_module.is_test_module || !lir_module.test_fns.is_empty() || !lir_module.bench_fns.is_empty()) {
        runtime_src.push_str(c_runtime::PANIC_NORMAL);
    }
    runtime_src.push_str(c_runtime::RUNTIME_CHECKED_ARITH);
    runtime_src.push_str(c_runtime::RUNTIME_STRING);
    // Test panic handler must come AFTER RUNTIME_STRING (uses Str type).
    if lir_module.is_test_module || !lir_module.test_fns.is_empty() || !lir_module.bench_fns.is_empty() {
        runtime_src.push_str(c_runtime::PANIC_TEST);
    }
    runtime_src.push_str(c_runtime::RUNTIME_STRING_EXTENDED);
    runtime_src.push_str(c_runtime::RUNTIME_STRING_BASE_OPS);
    runtime_src.push_str(c_runtime::RUNTIME_ARRAY);
    runtime_src.push_str(c_runtime::RUNTIME_STRING_ARRAY);
    runtime_src.push_str(c_runtime::RUNTIME_MAP);
    runtime_src.push_str(c_runtime::RUNTIME_SET);
    runtime_src.push_str(c_runtime::RUNTIME_ERROR);
    runtime_src.push_str(c_runtime::RUNTIME_FILE);
    runtime_src.push_str(c_runtime::RUNTIME_PATH);
    runtime_src.push_str(c_runtime::RUNTIME_ARGS);
    runtime_src.push_str(c_runtime::RUNTIME_PARSE);
    runtime_src.push_str(c_runtime::RUNTIME_TOSTR);
    runtime_src.push_str(c_runtime::RUNTIME_IO);
    runtime_src.push_str(c_runtime::RUNTIME_MATH);
    runtime_src.push_str(c_runtime::RUNTIME_SORT);
    runtime_src.push_str(c_runtime::RUNTIME_ENV);
    // Note: RUNTIME_ALLOC_REPORT deliberately excluded — it registers an atexit handler
    // that prints allocation stats, which the C backend only includes for test/bench mode.
    // Conditionally include heavier runtime modules
    let needs_async = concat_source.contains("std.async")
        || concat_source.contains("std.time")  // std.time exports gorget_reactor_sleep_seconds
        || !lir_module.spawned_fns.is_empty()
        || !lir_module.externs.iter().all(|e| !e.name.contains("gorget_channel") && !e.name.contains("gorget_task")
            && !e.name.contains("gorget_reactor")  // reactor sleep / async timer functions
            && !e.name.starts_with("Channel__"));
    if needs_async {
        runtime_src.push_str(c_runtime::ASYNC_RUNTIME);
        runtime_src.push_str(c_runtime::TASK_COMMON);
        runtime_src.push_str(c_runtime::EXECUTOR_RUNTIME);
        runtime_src.push_str(c_runtime::MAIN_WAKER_RUNTIME);
        runtime_src.push_str(c_runtime::CHANNEL_RUNTIME);
        runtime_src.push_str(c_runtime::BLOCKING_POOL_RUNTIME);
        // Scheduler runtime must come BEFORE task group runtime — TASK_GROUP_RUNTIME
        // uses the GORGET_SCHEDULER_WAIT macro defined in SCHEDULER_*_RUNTIME.
        runtime_src.push_str(match lir_module.scheduler_mode {
            gorget::ir::SchedulerMode::Pool => c_runtime::SCHEDULER_POOL_RUNTIME,
            gorget::ir::SchedulerMode::Thread => c_runtime::SCHEDULER_THREAD_RUNTIME,
            gorget::ir::SchedulerMode::Inline => c_runtime::SCHEDULER_INLINE_RUNTIME,
            gorget::ir::SchedulerMode::Single => c_runtime::SCHEDULER_SINGLE_RUNTIME,
        });
        runtime_src.push_str(c_runtime::TASK_GROUP_RUNTIME);
        runtime_src.push_str(c_runtime::REACTOR_RUNTIME);
    }
    let needs_sync = concat_source.contains("std.sync")
        || concat_source.contains("Shared")
        || concat_source.contains("Mutex")
        || concat_source.contains("Guard")
        || concat_source.contains("AtomicInt")
        || !lir_module.externs.iter().all(|e| !e.name.contains("mutex") && !e.name.contains("shared") && !e.name.contains("guard") && !e.name.contains("rwlock")
            && !e.name.starts_with("Shared__") && !e.name.starts_with("Mutex__") && !e.name.starts_with("Guard__")
            && !e.name.contains("gorget_atomic"));
    if needs_sync {
        // Mutex/Sync depend on async types (GorgetWaker) — include async basics if not already
        if !needs_async {
            runtime_src.push_str(c_runtime::ASYNC_RUNTIME);
            runtime_src.push_str(c_runtime::TASK_COMMON);
        }
        runtime_src.push_str(c_runtime::SHARED_RUNTIME);
        runtime_src.push_str(c_runtime::MUTEX_RUNTIME);
        runtime_src.push_str(c_runtime::SYNC_RUNTIME);
    }
    if concat_source.contains("std.thread") || !lir_module.thread_spawned_fns.is_empty() {
        runtime_src.push_str(c_runtime::THREAD_RUNTIME);
    }
    if concat_source.contains("std.alloc") {
        runtime_src.push_str(c_runtime::RUNTIME_ARENA_ALLOC);
        runtime_src.push_str(c_runtime::RUNTIME_TRACKING_ALLOC);
        runtime_src.push_str(c_runtime::RUNTIME_POOL_ALLOC);
        runtime_src.push_str(c_runtime::RUNTIME_TLSF_ALLOC);
        runtime_src.push_str(c_runtime::RUNTIME_FIXEDBUF_ALLOC);
        runtime_src.push_str(c_runtime::RUNTIME_FALLBACK_ALLOC);
    }
    if concat_source.contains("std.os")
        || !lir_module.externs.iter().all(|e| !e.name.contains("gorget_exec") && !e.name.contains("gorget_process")
            && !e.name.contains("gorget_getpid") && !e.name.contains("gorget_signal")) {
        runtime_src.push_str(c_runtime::PROCESS_RUNTIME);
        runtime_src.push_str(c_runtime::PROCESS_SPAWN_RUNTIME);
    }
    if concat_source.contains("xtd.crypto") {
        runtime_src.push_str(c_runtime::CRYPTO_RUNTIME);
    }
    if concat_source.contains("std.net") {
        runtime_src.push_str(c_runtime::SOCKET_RUNTIME);
        runtime_src.push_str(c_runtime::SERVER_SOCKET_RUNTIME);
    }
    if concat_source.contains("std.net.tls") {
        runtime_src.push_str(c_runtime::TLS_SOCKET_RUNTIME);
        runtime_src.push_str(c_runtime::TLS_SERVER_RUNTIME);
    }
    if concat_source.contains("std.net.udp") {
        runtime_src.push_str(c_runtime::UDP_SOCKET_RUNTIME);
    }
    if concat_source.contains("xtd.bytes")
        || lir_module.externs.iter().any(|e| e.name.contains("gorget_bytes")) {
        runtime_src.push_str(c_runtime::BYTES_RUNTIME);
    }
    // Trace runtime — needed when the LIR module carries a trace filename
    // (set by `directive trace` or the `--trace` CLI flag). The C backend
    // pulls this in through `emit_runtime_modules`; the LLVM build path
    // composes its runtime manually here, so mirror that conditional.
    if lir_module.trace_filename.is_some() {
        runtime_src.push_str(c_runtime::TRACE_RUNTIME);
    }
    let needs_sqlite = lir_module.externs.iter().any(|e| e.name.starts_with("gorget_sqlite_") || e.name == "sqlite_open");
    // Test/bench modules need the alloc report runtime for panic handler globals.
    if lir_module.is_test_module || !lir_module.test_fns.is_empty() || !lir_module.bench_fns.is_empty() {
        runtime_src.push_str(c_runtime::RUNTIME_ALLOC_REPORT);
    }
    // Append monomorphized wrappers (drops, clones, combinators, channel/shared/mutex
    // wrappers, spawn/await helpers, thread helpers, adapter functions, test runner).
    // These are C functions that call both runtime functions (already in runtime_src)
    // and user functions (defined in LLVM IR, resolved at link time).
    let wrapper_code = gorget::backend::c_lir::generate_llvm_wrappers(lir_module);
    runtime_src.push_str(&wrapper_code);

    // Make static functions non-static so they're visible for linking.
    // Keep _Thread_local statics (GCC requires TLS variables to be static at function scope).
    // Must happen AFTER wrapper append so wrapper functions are also de-staticified.
    let mut runtime_src = runtime_src
        .replace("static inline ", "")
        .replace("static _Thread_local", "_Thread_local_KEEP")
        .replace("static __thread", "__thread_KEEP")
        .replace("static ", "")
        .replace("_Thread_local_KEEP", "static _Thread_local")
        .replace("__thread_KEEP", "static __thread");

    // SQLite goes in AFTER the strip-static transform: the amalgamation
    // declares thousands of `static` functions and globals (mutex tables,
    // page-cache vtables, virtual filesystem registries) whose one-time-init
    // semantics rely on file-local linkage. Stripping `static` makes them
    // colide across translation units (when llc + cc share an exe) and, more
    // damagingly, leaves the mutex vtable NULL and crashes inside
    // sqlite3MutexInit on the first sqlite3_open(). The gorget wrappers
    // (gorget_sqlite_open / _exec / _bind_int / etc.) DO need their `static`
    // dropped so the LLVM-emitted .o can call them — strip just that block
    // before emitting it, then leave the amalgamation untouched.
    if needs_sqlite {
        runtime_src.push_str("\n#define SQLITE_MAX_MMAP_SIZE 0\n");
        runtime_src.push_str("#define HAVE_MREMAP 0\n");
        runtime_src.push_str("#pragma GCC diagnostic push\n");
        runtime_src.push_str("#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n");
        runtime_src.push_str("#pragma GCC diagnostic ignored \"-Wunused-variable\"\n");
        runtime_src.push_str("#pragma GCC diagnostic ignored \"-Wunused-function\"\n");
        runtime_src.push_str("#pragma GCC diagnostic ignored \"-Wimplicit-fallthrough\"\n");
        runtime_src.push_str("#pragma GCC diagnostic ignored \"-Wpedantic\"\n");
        runtime_src.push_str(c_runtime::SQLITE_AMALGAMATION);
        runtime_src.push_str("\n#pragma GCC diagnostic pop\n");
        // Wrappers: need globally visible names. The block uses
        // `static <ret> gorget_sqlite_*(...)` exclusively, no `static
        // inline`, no `static _Thread_local`, no `static __thread`.
        let wrappers = c_runtime::SQLITE_GORGET_WRAPPERS.replace("static ", "");
        runtime_src.push_str(&wrappers);
    }

    if let Err(e) = fs::write(&runtime_c_path, &runtime_src) {
        return Err(format!("Error writing runtime C: {e}"));
    }

    // Compile runtime C → .o
    let runtime_o_path = tmp_dir.join(format!("__gorget_runtime_{stem}.o"));
    let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let mut rt_cmd = Command::new(&cc);
    rt_cmd
        .arg("-c")
        .arg("-O2")
        .arg("-std=c11")
        // dropped blanket -w (it hid the strptime implicit-decl); keep the
        // implicit-decl class as a hard error without -Wall's benign flood.
        .arg("-Werror=implicit-function-declaration")
        .arg("-o").arg(&runtime_o_path)
        .arg(&runtime_c_path)
        .arg("-lm");
    // `--sanitize`, half 1 of 2: instrument the runtime translation unit.
    //
    // The runtime C blob carries essentially all of a Gorget program's heap
    // traffic — str_cat, array/map/set alloc, the generated drop/clone
    // wrappers — so instrumenting it here is what buys shadow-memory checking
    // on this lane. It is ALSO the half that distinguishes a genuinely
    // instrumented build from one that merely links libasan, which is why the
    // guard for it (`sanitizer_gate_is_real_on_both_backends`, tests/security.rs)
    // scans the FINAL LINKED ARTIFACT for `__asan_report_*` references: those
    // symbols are emitted by instrumented code, and with this call reverted the
    // executable's count drops to zero while `ldd` still shows libasan.
    //
    // ⚠ RESIDUAL GAP — this does NOT instrument generated user code. That code
    // reaches the binary as LLVM IR through `llc` (step 2 below), and `llc`
    // does not run ASan's instrumentation passes. So on this lane a UAF /
    // overflow / stack error whose FAULTING ACCESS is in user code is not
    // caught, while leaks (LeakSanitizer is interceptor-based) and faults
    // inside the runtime are. Closing it needs `sanitize_address` attribute
    // emission in `src/backend/llvm/` plus two measured IR/ASan
    // incompatibilities — filed as `todo/t0727` with a committed repro at
    // `tests/fixtures/known_gaps/llvm_sanitize_user_code_not_instrumented.gg`.
    add_sanitize_flags(&mut rt_cmd, sanitize);
    // Thread support
    #[cfg(not(target_os = "macos"))]
    rt_cmd.arg("-pthread");

    let status = rt_cmd.status();
    match status {
        Ok(s) if !s.success() => return Err(format!("Runtime compilation failed: {s}")),
        Err(e) => return Err(format!("Failed to run '{cc}' for runtime: {e}")),
        _ => {}
    }

    // Step 2: Compile LLVM IR → .o using llc
    let ll_o_path = tmp_dir.join(format!("__gorget_user_{stem}.o"));
    let llc = env::var("LLC").unwrap_or_else(|_| "llc".to_string());
    let mut ll_cmd = Command::new(&llc);
    // --release lifts the user-IR opt level to -O2 (matching the C backend);
    // the default is -O0 for fast, debuggable builds. The runtime .o above is
    // always -O2 regardless — release affects only generated user code.
    let user_opt = if release { "-O2" } else { "-O0" };
    ll_cmd
        .arg("-filetype=obj")
        .arg(user_opt)
        .arg("-relocation-model=pic")
        .arg("-o").arg(&ll_o_path)
        .arg(ll_path);
    // LLVM 14 (Debian oldstable) defaults to typed pointers and needs the
    // explicit `-opaque-pointers` opt-in for IR that uses bare `ptr`. LLVM 15
    // makes opaque pointers the default; LLVM 22 removed the flag entirely.
    // Probe the version once and add the flag only when it's < 15 *and*
    // recognized — that keeps both old local toolchains and CI happy.
    if llc_needs_opaque_pointers_flag(&llc) {
        ll_cmd.arg("-opaque-pointers");
    }

    let status = ll_cmd.status();
    match status {
        Ok(s) if !s.success() => return Err(format!(
            "llc compilation failed: {s}\nGenerated LLVM IR: {}",
            ll_path.display()
        )),
        Err(e) => return Err(format!("Failed to run '{llc}': {e}\nIs LLVM installed? (apt install llvm)")),
        _ => {}
    }

    // Step 3: Link .o files → binary
    let mut link_cmd = Command::new(&cc);
    link_cmd
        .arg("-o").arg(exe_path)
        .arg(&ll_o_path)
        .arg(&runtime_o_path)
        .arg("-lm");
    // `--sanitize`, half 2 of 2: link the sanitizer runtimes. LeakSanitizer is
    // interceptor-based, so this half alone gives COMPLETE leak coverage
    // regardless of what is instrumented; half 1 above adds the shadow checks.
    // Both halves are needed — half 1 without half 2 is undefined `__asan_*`
    // at link time, half 2 without half 1 is leak-only coverage that a
    // leak-based control cannot tell apart from the real thing.
    add_sanitize_flags(&mut link_cmd, sanitize);

    #[cfg(not(target_os = "macos"))]
    link_cmd.arg("-pthread");

    // Conditional external library flags
    let has_extern = |prefix: &str| lir_module.externs.iter().any(|e| e.name.contains(prefix));
    if concat_source.contains("xtd.crypto") || concat_source.contains("std.net.tls")
        || concat_source.contains("xtd.http")
        || has_extern("EVP_") || has_extern("SSL_") || has_extern("gorget_tls_") {
        add_crypto_flags(&mut link_cmd, true);
    }
    // (xtd.regex used to need -lpcre2-8 here; now pure Gorget — no link flag.)
    // gorget_sqlite_* now resolves through the embedded amalgamation
    // (compiled as __gorget_sqlite.o above). No -lsqlite3 needed.

    let status = link_cmd.status();
    match status {
        Ok(s) if s.success() => {
            // Clean up temp files unless GORGET_KEEP_RUNTIME=1 (debugging
            // runtime-side codegen — bridge wrappers, runtime fixups, etc.).
            if env::var("GORGET_KEEP_RUNTIME").ok().as_deref() != Some("1") {
                let _ = fs::remove_file(&runtime_c_path);
                let _ = fs::remove_file(&runtime_o_path);
                let _ = fs::remove_file(&ll_o_path);
            }
            Ok(exe_path.to_path_buf())
        }
        Ok(s) => Err(format!("Linking failed: {s}\nGenerated LLVM IR: {}", ll_path.display())),
        Err(e) => Err(format!("Failed to run '{cc}' for linking: {e}")),
    }
}

/// Profile the full compilation pipeline, timing each phase.
/// Outputs structured JSON to stdout.
fn try_profile(
    filename: &str,
    source: &str,
    dep_paths: HashMap<String, PathBuf>,
    features: &[String],
    options: gorget::ir::lowering::LoweringOptions,
) -> Result<(), String> {
    use std::time::Instant;

    let total_start = Instant::now();
    let source_lines = source.lines().count();

    // Phase 1: Parse
    let t = Instant::now();
    let mut parser = Parser::new(source);
    let module = parser.parse_module();
    let parse_ms = t.elapsed().as_secs_f64() * 1000.0;

    if !parser.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), source.to_string());
        for err in &parser.errors {
            reporter.report_parse_error(err);
        }
        return Err(format!("{} parse error(s) found", parser.errors.len()));
    }

    // Phase 2: Load imports
    let t = Instant::now();
    let (mut module, file_infos) = load_imports(filename, source, module, dep_paths);
    let load_imports_ms = t.elapsed().as_secs_f64() * 1000.0;

    // Phase 3: Semantic analysis
    let t = Instant::now();
    let source_dir = std::path::Path::new(filename).parent().map(|p| p.to_path_buf());
    let result = gorget::semantic::analyze_with_source_dir(&mut module, features, source_dir, false);
    let semantic_ms = t.elapsed().as_secs_f64() * 1000.0;

    if !result.errors.is_empty() {
        let reporter = ErrorReporter::new_multi(file_infos.clone());
        for err in &result.errors {
            reporter.report_semantic_error(err);
        }
        return Err(format!("{} semantic error(s) found", result.errors.len()));
    }

    // Phase 4: GIR lowering
    let t = Instant::now();
    let mut gir_module = gorget::ir::lowering::lower_module(&module, &result, &options);
    let gir_lower_ms = t.elapsed().as_secs_f64() * 1000.0;
    let gir_functions = gir_module.functions.len();

    // Phase 5: GIR optimization
    let t = Instant::now();
    let gir_opt_stats = gorget::ir::transforms::optimize::optimize_module(&mut gir_module);
    let gir_optimize_ms = t.elapsed().as_secs_f64() * 1000.0;

    // Phase 6: LIR lowering
    let t = Instant::now();
    let mut lir_module = gorget::lir::lower::lower_module(&gir_module);
    lir_module.file_infos = to_lir_file_infos(&file_infos);
    let lir_lower_ms = t.elapsed().as_secs_f64() * 1000.0;

    // Phase 7: SSA construction (with pre-pass critical-edge splitting; §8.2)
    let t = Instant::now();
    gorget::lir::split_edges::split_critical_edges_module(&mut lir_module);
    gorget::lir::validate::assert_module_valid(&lir_module, "lir-lowering");
    for func in &mut lir_module.functions {
        gorget::lir::ssa::construct_ssa(func);
    }
    let lir_ssa_ms = t.elapsed().as_secs_f64() * 1000.0;
    gorget::lir::validate::assert_module_valid(&lir_module, "ssa-construction");

    // Phase 8a: LIR value-types (pre-BIR) — optimize moves to post-BIR
    // so synth fns benefit from DCE/fold/CSE.
    gorget::lir::types::wire_collection_bridges(&mut lir_module);
    gorget::lir::validate::assert_module_valid(&lir_module, "wire-collection-bridges");
    gorget::lir::runtime::promote_runtime_calls(&mut lir_module);
    gorget::lir::validate::assert_module_valid(&lir_module, "promote-runtime-calls");
    gorget::lir::types::compute_module_pointee_types(&mut lir_module);
    gorget::lir::types::compute_module_value_types(&mut lir_module);
    gorget::lir::types::compute_module_value_origins(&mut lir_module);
    gorget::lir::validate::assert_module_valid(&lir_module, "compute-types-pre-bir");
    let lir_functions = lir_module.functions.len();
    let lir_instructions: usize = lir_module.functions.iter()
        .map(|f| f.blocks.iter().map(|b| b.insts.len()).sum::<usize>())
        .sum();

    // Phase 9: BIR lowering + optimize + C code generation
    let t = Instant::now();
    let mut bir_module = gorget::bir::BirModule::from_lir(lir_module)
        .map_err(|e| format!("BIR lowering failed: {e}"))?;
    // Tier E §8.2: BIR synthesis adds new control flow.
    gorget::lir::split_edges::split_critical_edges_module(bir_module.as_lir_mut());
    gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "bir-lowering");
    let lir_opt_stats = gorget::lir::optimize::optimize_module(bir_module.as_lir_mut());
    gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "optimize");
    gorget::lir::types::compute_module_pointee_types(bir_module.as_lir_mut());
    gorget::lir::types::compute_module_value_types(bir_module.as_lir_mut());
    gorget::lir::types::compute_module_value_origins(bir_module.as_lir_mut());
    gorget::lir::validate::assert_module_valid(bir_module.as_lir(), "compute-types-post-bir");
    let lir_optimize_ms = t.elapsed().as_secs_f64() * 1000.0;
    let t = Instant::now();
    let backend = gorget::backend::c_lir::CLirBackend;
    let output = gorget::backend::Backend::generate(&backend, &bir_module);
    let codegen_ms = t.elapsed().as_secs_f64() * 1000.0;
    let c_lines = output.code.lines().count();

    let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
    let frontend_ms = parse_ms + load_imports_ms + semantic_ms;
    let backend_ms = gir_lower_ms + gir_optimize_ms + lir_lower_ms + lir_ssa_ms + lir_optimize_ms + codegen_ms;

    // Peak RSS (Linux)
    let peak_rss_kb = read_peak_rss_kb();

    // Emit JSON
    println!("{{");
    println!("  \"file\": \"{}\",", filename.replace('\\', "\\\\").replace('"', "\\\""));
    println!("  \"source_lines\": {},", source_lines);
    println!("  \"compiler\": \"gg {}\",", env!("CARGO_PKG_VERSION"));
    println!("  \"timestamp\": \"{}\",", timestamp_now());
    println!("  \"phases\": {{");
    println!("    \"parse\": {{ \"duration_ms\": {:.3} }},", parse_ms);
    println!("    \"load_imports\": {{ \"duration_ms\": {:.3} }},", load_imports_ms);
    // Semantic phase: also surface per-sub-pass timing so hotspots are visible.
    print!("    \"semantic\": {{ \"duration_ms\": {:.3}", semantic_ms);
    if !result.pass_times.is_empty() {
        let mut entries: Vec<(&&'static str, &std::time::Duration)> =
            result.pass_times.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1));
        print!(", \"pass_times_ms\": {{ ");
        for (i, (name, dur)) in entries.iter().enumerate() {
            if i > 0 { print!(", "); }
            print!("\"{name}\": {:.3}", dur.as_secs_f64() * 1000.0);
        }
        print!(" }}");
    }
    println!(" }},");
    print!("    \"gir_lower\": {{ \"duration_ms\": {:.3}", gir_lower_ms);
    if !gir_module.gir_lower_pass_times.is_empty() {
        let mut entries: Vec<(&&'static str, &std::time::Duration)> =
            gir_module.gir_lower_pass_times.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1));
        print!(", \"pass_times_ms\": {{ ");
        for (i, (name, dur)) in entries.iter().enumerate() {
            if i > 0 { print!(", "); }
            print!("\"{name}\": {:.3}", dur.as_secs_f64() * 1000.0);
        }
        print!(" }}");
    }
    println!(" }},");
    println!("    \"gir_optimize\": {{ \"duration_ms\": {:.3} }},", gir_optimize_ms);
    println!("    \"lir_lower\": {{ \"duration_ms\": {:.3} }},", lir_lower_ms);
    println!("    \"lir_ssa\": {{ \"duration_ms\": {:.3} }},", lir_ssa_ms);
    println!("    \"lir_optimize\": {{ \"duration_ms\": {:.3} }},", lir_optimize_ms);
    println!("    \"codegen\": {{ \"duration_ms\": {:.3} }}", codegen_ms);
    println!("  }},");
    println!("  \"totals\": {{");
    println!("    \"total_ms\": {:.3},", total_ms);
    println!("    \"frontend_ms\": {:.3},", frontend_ms);
    println!("    \"backend_ms\": {:.3},", backend_ms);
    if let Some(rss) = peak_rss_kb {
        println!("    \"peak_rss_kb\": {}", rss);
    } else {
        println!("    \"peak_rss_kb\": null");
    }
    println!("  }},");
    println!("  \"stats\": {{");
    println!("    \"gir_functions\": {},", gir_functions);
    println!("    \"lir_functions\": {},", lir_functions);
    println!("    \"lir_instructions\": {},", lir_instructions);
    println!("    \"c_lines\": {},", c_lines);
    print!("    \"gir_opt\": {{ \"blocks_eliminated\": {}, \"insts_eliminated\": {}, \"locals_eliminated\": {}",
        gir_opt_stats.blocks_eliminated(), gir_opt_stats.insts_eliminated(), gir_opt_stats.locals_eliminated());
    if !gir_opt_stats.per_pass.is_empty() {
        print!(", \"per_pass\": {{ ");
        for (i, (name, stats)) in gir_opt_stats.per_pass.iter().enumerate() {
            if i > 0 { print!(", "); }
            print!("\"{name}\": {}", stats.insts_eliminated);
        }
        print!(" }}");
    }
    println!(" }},");
    print!("    \"lir_opt\": {{ \"dead_functions\": {}, \"dead_globals\": {}, \"dead_instructions\": {}, \"constants_folded\": {}, \"copies_propagated\": {}, \"algebraic_simplified\": {}, \"cse_eliminated\": {}",
        lir_opt_stats.dead_functions_eliminated, lir_opt_stats.dead_globals_eliminated,
        lir_opt_stats.dead_instructions_eliminated, lir_opt_stats.constants_folded,
        lir_opt_stats.copies_propagated, lir_opt_stats.algebraic_simplified,
        lir_opt_stats.cse_eliminated);
    if !lir_opt_stats.pass_times.is_empty() {
        // Sort descending by time so the dominant pass shows first.
        let mut entries: Vec<(&&'static str, &std::time::Duration)> =
            lir_opt_stats.pass_times.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1));
        print!(", \"pass_times_ms\": {{ ");
        for (i, (name, dur)) in entries.iter().enumerate() {
            if i > 0 { print!(", "); }
            print!("\"{name}\": {:.3}", dur.as_secs_f64() * 1000.0);
        }
        print!(" }}");
    }
    println!(" }}");
    println!("  }}");
    println!("}}");
    Ok(())
}

/// Read peak RSS from /proc/self/status (Linux) or getrusage (macOS).
fn read_peak_rss_kb() -> Option<u64> {
    #[cfg(target_os = "linux")]
    {
        if let Ok(status) = fs::read_to_string("/proc/self/status") {
            for line in status.lines() {
                if line.starts_with("VmHWM:") {
                    let parts: Vec<&str> = line.split_whitespace().collect();
                    if parts.len() >= 2 {
                        return parts[1].parse().ok();
                    }
                }
            }
        }
        None
    }
    #[cfg(not(target_os = "linux"))]
    {
        None
    }
}

/// ISO 8601 timestamp without external dependencies.
fn timestamp_now() -> String {
    use std::time::SystemTime;
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(d) => {
            let secs = d.as_secs();
            // Simple UTC timestamp: seconds since epoch → readable format
            let days = secs / 86400;
            let time_secs = secs % 86400;
            let hours = time_secs / 3600;
            let minutes = (time_secs % 3600) / 60;
            let seconds = time_secs % 60;
            // Calculate year/month/day from days since epoch (1970-01-01)
            let (year, month, day) = days_to_ymd(days);
            format!("{year:04}-{month:02}-{day:02}T{hours:02}:{minutes:02}:{seconds:02}Z")
        }
        Err(_) => "unknown".to_string(),
    }
}

fn days_to_ymd(days: u64) -> (u64, u64, u64) {
    // Civil calendar algorithm
    let z = days + 719468;
    let era = z / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

/// Compare two profile JSON files and output a diff.
fn compare_profiles(baseline_path: &str, current_path: &str) -> Result<(), String> {
    let baseline_str = fs::read_to_string(baseline_path)
        .map_err(|e| format!("Error reading {baseline_path}: {e}"))?;
    let current_str = fs::read_to_string(current_path)
        .map_err(|e| format!("Error reading {current_path}: {e}"))?;

    let baseline: serde_json::Value = serde_json::from_str(&baseline_str)
        .map_err(|e| format!("Error parsing {baseline_path}: {e}"))?;
    let current: serde_json::Value = serde_json::from_str(&current_str)
        .map_err(|e| format!("Error parsing {current_path}: {e}"))?;

    let phase_names = ["parse", "load_imports", "semantic", "gir_lower", "gir_optimize",
                       "lir_lower", "lir_ssa", "lir_optimize", "codegen"];

    // Header
    println!("{:<16} {:>12} {:>12} {:>10} {:>8}", "Phase", "Baseline", "Current", "Delta", "Factor");
    println!("{}", "-".repeat(62));

    for phase in &phase_names {
        let b = baseline["phases"][phase]["duration_ms"].as_f64().unwrap_or(0.0);
        let c = current["phases"][phase]["duration_ms"].as_f64().unwrap_or(0.0);
        let delta = c - b;
        let factor = if b > 0.001 { c / b } else { 0.0 };
        let sign = if delta >= 0.0 { "+" } else { "-" };
        let flag = if factor > 1.5 { " !!" } else if factor > 1.1 { " !" } else { "" };
        println!("{:<16} {:>10.3}ms {:>10.3}ms  {}{:.3}ms {:>6.2}x{}",
            phase, b, c, sign, delta.abs(), factor, flag);
    }

    println!("{}", "-".repeat(62));

    // Totals
    let bt = baseline["totals"]["total_ms"].as_f64().unwrap_or(0.0);
    let ct = current["totals"]["total_ms"].as_f64().unwrap_or(0.0);
    let dt = ct - bt;
    let ft = if bt > 0.001 { ct / bt } else { 0.0 };
    let sign = if dt >= 0.0 { "+" } else { "-" };
    println!("{:<16} {:>10.3}ms {:>10.3}ms  {}{:.3}ms {:>6.2}x",
        "TOTAL", bt, ct, sign, dt.abs(), ft);

    // Memory
    let bm = baseline["totals"]["peak_rss_kb"].as_u64();
    let cm = current["totals"]["peak_rss_kb"].as_u64();
    if let (Some(bm), Some(cm)) = (bm, cm) {
        let dm = cm as i64 - bm as i64;
        let sign = if dm >= 0 { "+" } else { "" };
        println!("{:<16} {:>10}KB {:>10}KB  {}{}KB",
            "Peak RSS", bm, cm, sign, dm.abs());
    }

    // Stats comparison
    let stat_keys = ["gir_functions", "lir_functions", "lir_instructions", "c_lines"];
    println!();
    println!("{:<20} {:>10} {:>10} {:>10}", "Stat", "Baseline", "Current", "Delta");
    println!("{}", "-".repeat(54));
    for key in &stat_keys {
        let b = baseline["stats"][key].as_u64().unwrap_or(0);
        let c = current["stats"][key].as_u64().unwrap_or(0);
        let d = c as i64 - b as i64;
        let sign = if d >= 0 { "+" } else { "-" };
        println!("{:<20} {:>10} {:>10}  {}{}", key, b, c, sign, d.unsigned_abs());
    }

    Ok(())
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
            match try_build_ir(&gg_path_str, &source, HashMap::new(), Some(&tmp_dir), None, None, &[], gorget::ir::lowering::LoweringOptions::default(), false, false, false, &CloneDiagModes::default(), &ResolverDiagModes::default(), "c-lir", "native") {
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
            match try_build_ir(&gg_path_str, &source, HashMap::new(), Some(&tmp_dir), None, None, &[], gorget::ir::lowering::LoweringOptions::default(), false, false, false, &CloneDiagModes::default(), &ResolverDiagModes::default(), "c-lir", "native") {
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

    let manifest_path = manifest::manifest_path_in(&cwd);
    if let Some(existing) = manifest::find_manifest_in(&cwd) {
        eprintln!("{} already exists", existing.display());
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

    let manifest_path = match manifest::find_manifest_in(&cwd) {
        Some(p) => p,
        None => {
            eprintln!("no {} found in '{}'", manifest::MANIFEST_NAME, cwd.display());
            process::exit(1);
        }
    };
    let mut manifest = Manifest::from_path(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error reading manifest: {e}");
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

    let manifest_path = match manifest::find_manifest_in(&cwd) {
        Some(p) => p,
        None => {
            eprintln!("no {} found in '{}'", manifest::MANIFEST_NAME, cwd.display());
            process::exit(1);
        }
    };
    let mut manifest = Manifest::from_path(&manifest_path).unwrap_or_else(|e| {
        eprintln!("Error reading manifest: {e}");
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
    // (A) #14: run the whole compiler on a sized stack (rustc's pattern — it runs
    // compilation on a `std::thread` with a configurable stack), so deep expression
    // lowering doesn't overflow the OS-default main-thread stack before the (B)
    // parse-time depth limit (128) can reject pathological input. The compiler is a
    // build tool (not a GUI app), so running off thread 0 is fine — unrelated to the
    // gorget-arena macOS fix (that was about *compiled user programs*' main thread).
    // env GG_MIN_STACK (bytes) overrides; default 512MB (mmap'd, lazily faulted → cheap).
    let stack_size = env::var("GG_MIN_STACK")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(512 * 1024 * 1024);
    let child = std::thread::Builder::new()
        .stack_size(stack_size)
        .name("gg-main".into())
        .spawn(real_main)
        .expect("failed to spawn gg main thread");
    child.join().expect("gg main thread panicked");
}

fn real_main() {
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
        println!("Compiler commands: lex, parse, check, build, run, fmt, test, report, profile");
        println!("Package commands:  init, new, add, remove");
        println!();
        println!("Build flags:");
        println!("  --release               Optimize the generated C at -O2 (default: -O0, fast/debuggable)");
        println!("  --hot-reload            Enable hot code reload (builds host + guest .dylib)");
        println!("  --shared [-o F]         Build as shared library (.dylib/.so)");
        println!("  --sanitize              Enable AddressSanitizer + UBSan for runtime bug detection");
        println!("  --emit-gir              Dump GIR (intermediate representation) to stdout instead of compiling");
        println!("  --emit-lir              Dump LIR (low-level SSA IR) to stdout instead of compiling");
        println!("  --emit-c-lir            Dump C code generated from LIR to stdout");
        println!("  --clones[=MODE,…]       Clone diagnostics (default: silent). Modes: sites (default), verbose, stats, sites-tsv=PATH, all");
        println!("                          sites:   compile-time report — file:line:col + type + reason");
        println!("                          verbose: sites + id + size_bytes + runtime_fn columns");
        println!("                          stats:   runtime atexit report — the aggregate `[clone-stats]` counter line");
        println!("                                   plus per-clone-site attribution: `[clone-site] #id=count` lines for");
        println!("                                   the hottest sites (top 50 by default; set GG_CLONE_SITES_TOP=N on");
        println!("                                   the compiled binary to widen, 0 = all nonzero sites)");
        println!("                          sites-tsv=PATH: dump the full static CloneId table (id, file, line, col,");
        println!("                                   type, reason, size_bytes, runtime_fn) as TSV to PATH — join its id");
        println!("                                   column against the runtime `[clone-site] #id=count` lines");
        println!("                          all:     alias for verbose,stats");
        println!("                          Without --clones, no clone diagnostics are printed.");
        println!("                          Note: stats is not supported with --backend=llvm yet.");
        println!("  --resolvers[=MODE,…]    Place-resolver fall-through worklist (default: silent). Modes: hist (default), sites, hist-tsv=PATH, all");
        println!("                          ⚠ WORKLIST GENERATOR only — not a correctness gate (Core #13). Some(wrong_root) counts as resolved.");
        println!("                          Requires the build/lower path (gg build --emit-gir --resolvers=hist); gg check never lowers.");
        println!("                          hist:    ranked shape histogram of None/Unresolved exits after lower");
        println!("                          sites:   per-site miss log (resolver + shape + span when available)");
        println!("                          hist-tsv=PATH: dump full hist rows as TSV to PATH");
        println!("                          all:     alias for hist,sites");
        println!("                          Without --resolvers, no fall-through report is printed.");
        println!();
        println!("Targets:");
        println!("  --target native                 Default — build for the host OS with full runtime");
        println!("  --target freestanding           Bare-metal UEFI application (auto-detects host arch)");
        println!("  --target freestanding-x86_64    UEFI application for x86_64 (BOOTX64.EFI)");
        println!("  --target freestanding-aarch64   UEFI application for aarch64 (BOOTAA64.EFI)");
        println!();
        println!("Profile:");
        println!("  gg profile <file.gg>                           Profile compilation phases (JSON to stdout)");
        println!("  gg profile --compare <base.json> <cur.json>    Compare two profiles");
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
            strip_asserts, no_strip_asserts,
            trace_filename, hot_reload: hot_reload_flag || source_has_hot_reload(&source),
            sanitize, scheduler_mode: parse_scheduler(&args),
            ..Default::default()
        };
        let clone_modes = parse_clone_modes(&args).unwrap_or_else(|e| {
            eprintln!("{e}");
            process::exit(1);
        });
        let resolver_modes = parse_resolver_modes(&args).unwrap_or_else(|e| {
            eprintln!("{e}");
            process::exit(1);
        });
        let exe_path = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, None, &features, lowering_opts, false, false, false, &clone_modes, &resolver_modes, "c-lir", "native")
            .unwrap_or_else(|e| { eprintln!("{e}"); process::exit(1); });
        let status = Command::new(&exe_path)
            .status()
            .unwrap_or_else(|e| {
                eprintln!("Failed to execute {}: {e}", exe_path.display());
                process::exit(1);
            });
        propagate_child_status(status, &exe_path.display().to_string());
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
        eprintln!("Compiler commands: lex, parse, check, build, run, fmt, test, report, profile");
        eprintln!("Package commands:  init, new, add, remove");
        process::exit(1);
    }

    let command = &args[1];
    let strip_asserts = args.iter().any(|a| a == "--strip-asserts");
    let no_strip_asserts = args.iter().any(|a| a == "--no-strip-asserts");
    let trace = args.iter().any(|a| a == "--trace");
    let no_trace = args.iter().any(|a| a == "--no-trace");
    let hot_reload_flag = args.iter().any(|a| a == "--hot-reload");
    let scheduler_mode = parse_scheduler(&args);
    let sanitize = args.iter().any(|a| a == "--sanitize");
    let release = args.iter().any(|a| a == "--release");
    let emit_gir = args.iter().any(|a| a == "--emit-gir");
    let emit_lir = args.iter().any(|a| a == "--emit-lir");
    let emit_c_lir = args.iter().any(|a| a == "--emit-c-lir");
    let backend_name = args.iter()
        .position(|a| a == "--backend")
        .and_then(|i| args.get(i + 1))
        .map(|s| s.as_str())
        .or_else(|| args.iter().find_map(|a| a.strip_prefix("--backend=")))
        .unwrap_or("c-lir");
    // Reject an unknown backend instead of silently falling through to C.
    // The dispatch below is `match { "llvm" => …, _ => CLir }`, so without this
    // check `--backend=wasm` (or any typo) built a C binary and reported
    // success — user input discarded rather than rejected.
    if !BACKENDS.contains(&backend_name) {
        eprintln!(
            "error: unknown backend `{backend_name}`\n  \
             expected one of: {}\n  \
             (the default is `c-lir`; `c` is an accepted alias for it)",
            BACKENDS.join(", ")
        );
        process::exit(1);
    }
    let shared_mode = args.iter().any(|a| a == "--shared");
    let show_borrows = args.iter().any(|a| a == "--show-borrows");
    let clone_modes = parse_clone_modes(&args).unwrap_or_else(|e| {
        eprintln!("{e}");
        process::exit(1);
    });
    let resolver_modes = parse_resolver_modes(&args).unwrap_or_else(|e| {
        eprintln!("{e}");
        process::exit(1);
    });
    let show_clones = clone_modes.sites || clone_modes.verbose;
    // `--clones=stats` instruments the binary through the C backend's runtime
    // blob (`emit_runtime_modules`, which the LLVM wrapper path never runs) —
    // under `--backend=llvm` the counters/atexit report would silently not
    // exist. Reject the combination honestly instead of no-op'ing.
    //
    // TODO(llvm-clone-stats): support `--clones=stats` under `--backend=llvm`.
    // The real path: the LLVM runtime C blob is hand-composed in
    // `compile_llvm_pipeline` (this file, `runtime_src` assembly) — it must
    // (a) append `RUNTIME_CLONE_STATS` +
    // `render_clone_sites_runtime(clone_site_count)` to that composition,
    // (b) declare `__gorget_clone_site_hit` in the emitted LLVM IR as an
    // external function, and (c) survive the strip-static pass there
    // (`.replace("static ", "")`), which would give the blob's
    // `__gorget_clone_site_counts[N]` table and hit fn EXTERNAL linkage —
    // in a shared-lib + exe build both TUs would then bind to ONE table
    // sized for the wrong module's CloneId range (silent misattribution,
    // the exact interposition hazard the C path avoids by keeping the hit
    // fn `static`).
    if clone_modes.stats && backend_name == "llvm" {
        eprintln!("--clones=stats is not supported with --backend=llvm yet; drop --clones=stats (compile-time modes like --clones=sites / --clones=verbose / --clones=sites-tsv=PATH still work) or use the default C backend.");
        process::exit(1);
    }
    let warn_const = args.iter().any(|a| a == "--warn-const");
    let target = args.iter()
        .position(|a| a == "--target")
        .and_then(|i| args.get(i + 1))
        .map(|s| s.as_str())
        .or_else(|| args.iter().find_map(|a| a.strip_prefix("--target=")))
        .unwrap_or("native");
    // ── Build-flag combinations that cannot be honoured: reject, never drop ──
    //
    // These sit with `--clones=stats` above and with the unknown-backend check,
    // as one lower-or-reject policy: every build flag the selected backend or
    // target cannot implement is refused by name, so `gg` never hands back an
    // artifact that is not what was asked for. They are deliberately placed
    // AFTER `--target` parsing (which is itself after the `--clones=stats`
    // check) because they read `target`.
    //
    // The sibling that is WIRED rather than rejected is `--sanitize` on the
    // LLVM backend (`add_sanitize_flags`): it was a member of this same class
    // and was cheap enough to implement. The discriminator across the class is
    // implementation cost, not principle.
    //
    // ⚠ THE SHAPE THAT KEEPS PRODUCING THESE. `try_build_ir` has FIVE build
    // sub-paths, and FOUR of them construct their OWN compiler invocation and
    // `return` from it before reaching the flag-application code at the bottom
    // of the fifth: `--shared`, the hot-reload split (guest + host), the LLVM
    // pipeline (`compile_llvm_pipeline`'s runtime `cc -c` + link), and
    // freestanding/UEFI. A flag applied only on the normal path is therefore
    // silently absent on all four unless it is threaded deliberately — which
    // is precisely how `--sanitize` came to be a no-op under `--backend=llvm`.
    //
    // ⚠ AND THERE IS A SECOND AXIS, which is the easier one to miss: the ENTRY
    // POINTS. `gg build`, `gg run`, `gg test` and the `gg script.gg` shorthand
    // each construct their OWN `LoweringOptions`, so a field one of them omits
    // is dropped for every sub-path beneath it — no early return involved.
    // `gg test`'s omits `release` (it carries `sanitize, scheduler_mode` and
    // simply not `release`), so `gg test --bench --release` benchmarks a `-O0`
    // build. Enumerating sub-paths alone missed that cell entirely, twice.
    //
    // `todo/t0641` is the family record. `--release` is still dropped on FIVE
    // shapes — `--shared`, hot-reload, freestanding, the `gg script.gg`
    // shorthand and `gg test`; it is correctly threaded on `gg build`'s and
    // `gg run`'s normal C path and through the LLVM pipeline. Measured, not
    // read — the driver honours `CC`/`LLC`, so
    // `CC=/bin/echo LLC=/bin/echo gg <sub> <flags> f.gg` prints the constructed
    // argv, and the same invocation WITHOUT the flag is the control (the LLVM
    // runtime carries an unconditional `-O2`, so presence alone proves nothing).
    // `build_flags_are_never_silently_dropped` (tests/integration.rs) runs that
    // census over both axes, so this enumeration is executable rather than prose.
    // ⚠ Its instrument is argv-only: a flag consumed during lowering rather than
    // passed to a compiler (`--scheduler`, `--strip-asserts`, `--trace`,
    // `--feature`, `--hot-reload`) is invisible to it, and at least one of those
    // is dropped today (`todo/t0627`).
    //
    // When adding a build flag, check every early-returning sub-path AND every
    // entry point's `LoweringOptions` — not just the one you tested.
    if target.starts_with("freestanding") && backend_name == "llvm" {
        // Silently built a hosted ELF and printed `Built:`. The freestanding
        // path is C-backend-only: `try_build_ir` returns into
        // `compile_llvm_pipeline` BEFORE it ever reads `target`, so the whole
        // UEFI branch (clang `-target …-windows`, `-ffreestanding -nostdlib`,
        // the `.efi` + ESP layout) was unreachable and the user got a Linux
        // binary named as if it were their bootloader.
        eprintln!(
            "error: unsupported --target={target} with --backend=llvm\n  \
             the freestanding/UEFI target is implemented on the C backend only \
             (it needs clang's `-target <arch>-unknown-windows -ffreestanding -nostdlib` \
             and lld, which the LLVM pipeline's llc+cc path does not drive)\n  \
             use the default C backend for freestanding targets, or drop --target"
        );
        process::exit(1);
    }
    if sanitize && target.starts_with("freestanding") {
        // Accepted and silently discarded on BOTH backends, with rc 0 and no
        // diagnostic — the same shape as `--sanitize --backend=llvm` before it
        // was wired, one axis over. The freestanding branch builds its own
        // `cc_cmd` and returns from it before reaching `add_sanitize_flags`,
        // so the flag never landed. Verified with `CC=/bin/echo`: the argv
        // carried no `-fsanitize` here while the ordinary C path did.
        //
        // REJECTED rather than wired, unlike the LLVM case: a UEFI PE image is
        // built `-ffreestanding -nostdlib`, and ASan/UBSan need a runtime
        // library and a libc to intercept. There is nothing to instrument
        // against, so this combination cannot be honoured at all.
        eprintln!(
            "error: unsupported --sanitize with --target={target}\n  \
             the sanitizers need a hosted environment (they link a runtime \
             library and intercept libc), and the freestanding/UEFI target is \
             built -ffreestanding -nostdlib with no libc to intercept\n  \
             drop --sanitize for freestanding builds, or build for the native \
             target to sanitize"
        );
        process::exit(1);
    }
    if shared_mode && backend_name == "llvm" {
        // Failed already, but incoherently: the `--shared` branch runs BEFORE
        // the backend dispatch, so it wrote LLVM IR into `<stem>_guest.c` and
        // handed it to `cc`, which reported ~290 lines of C syntax errors
        // about `target datalayout` and `%GorgetArray`. Loud, but it accused
        // the user's program of being broken C. Say what is actually wrong.
        eprintln!(
            "error: unsupported --shared with --backend=llvm\n  \
             shared-library output is implemented on the C backend only \
             (the split emits a C translation unit; the LLVM pipeline has no \
             shared-object path)\n  \
             use the default C backend for --shared builds"
        );
        process::exit(1);
    }
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
    let filename = {
        let flags_with_values = ["--tag", "--exclude-tag", "--filter", "--report", "--output", "-o", "--feature", "--backend", "--target"];
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

    // Directory mode for `gg test`: discover and run all test files.
    if command == "test" && Path::new(filename).is_dir() {
        let dir = Path::new(filename);
        let bench_mode = args.iter().any(|a| a == "--bench");
        let test_files = discover_test_files(dir, bench_mode);
        if test_files.is_empty() {
            eprintln!("No test files found in {}", dir.display());
            process::exit(1);
        }

        // Forward all remaining flags (everything after the directory arg) to each file run.
        let forward_flags: Vec<&String> = args.iter().skip(2)
            .filter(|a| a.as_str() != filename)
            .collect();

        let exe = env::current_exe().unwrap_or_else(|_| PathBuf::from(&args[0]));
        let mut total_passed = 0u64;
        let mut total_failed = 0u64;
        let mut total_skipped = 0u64;
        let mut any_failed = false;
        let mut files_with_tests = 0u64;
        #[cfg(unix)]
        let mut first_signal: Option<(PathBuf, i32)> = None;

        for file in &test_files {
            let rel = file.strip_prefix(dir).unwrap_or(file);
            let mut cmd = Command::new(&exe);
            cmd.arg("test");
            cmd.arg(file);
            for flag in &forward_flags {
                cmd.arg(flag);
            }
            // Capture output to parse summary line for aggregation.
            let output = cmd.output().unwrap_or_else(|e| {
                eprintln!("Failed to run tests for {}: {e}", file.display());
                process::exit(1);
            });
            #[cfg(unix)]
            {
                use std::os::unix::process::ExitStatusExt;
                if first_signal.is_none() {
                    if let Some(signo) = output.status.signal() {
                        first_signal = Some((file.clone(), signo));
                    }
                }
            }
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            // Parse the summary line: "N passed, N failed[, N skipped] (Nms)"
            let mut file_passed = 0u64;
            let mut file_failed = 0u64;
            let mut file_skipped = 0u64;
            for line in stdout.lines().rev() {
                let trimmed = line.trim();
                if trimmed.contains("passed,") && trimmed.contains("failed") {
                    let parts: Vec<&str> = trimmed.split_whitespace().collect();
                    if parts.len() >= 4 {
                        file_passed = parts[0].parse().unwrap_or(0);
                        file_failed = parts[2].trim_end_matches(',').parse().unwrap_or(0);
                        if parts.len() >= 6 {
                            file_skipped = parts[4].trim_end_matches(',').parse().unwrap_or(0);
                        }
                    }
                    break;
                }
            }

            let file_total = file_passed + file_failed + file_skipped;
            // Skip files where no tests ran (e.g. all filtered out).
            if file_total == 0 && output.status.success() {
                continue;
            }

            files_with_tests += 1;
            println!("--- {} ---", rel.display());

            // Compilation failure: no tests ran but exit code is non-zero.
            if file_total == 0 && !output.status.success() {
                // Show only the semantic/parse error lines, not C compiler noise.
                let stderr_str = stderr.to_string();
                let mut showed_error = false;
                for line in stderr_str.lines() {
                    let t = line.trim();
                    if t.contains("error(s) found") || t.contains("error:") || t.contains("error[") {
                        eprintln!("{line}");
                        showed_error = true;
                    }
                }
                if !showed_error {
                    eprintln!("  FAIL: compilation failed");
                }
                any_failed = true;
                total_failed += 1;
                continue;
            }

            // Print test lines (skip the "Running N tests..." and summary lines).
            for line in stdout.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with("Running ") && trimmed.ends_with(" tests...") {
                    continue;
                }
                if trimmed.contains("passed,") && trimmed.contains("failed") {
                    continue;
                }
                // Skip alloc-report lines and size-bucket lines.
                if trimmed.starts_with("[alloc-report]") || trimmed.ends_with(" allocs") {
                    continue;
                }
                if trimmed.is_empty() {
                    continue;
                }
                println!("{line}");
            }
            if !stderr.is_empty() {
                for line in stderr.lines() {
                    let t = line.trim();
                    if t.is_empty() {
                        continue;
                    }
                    // In bench mode, pass alloc-report lines through so snapshot
                    // scripts can capture allocation counts.
                    eprintln!("{line}");
                }
            }
            if !output.status.success() {
                any_failed = true;
            }
            total_passed += file_passed;
            total_failed += file_failed;
            total_skipped += file_skipped;
        }

        // Print aggregate summary.
        println!();
        if files_with_tests == 0 {
            println!("No matching tests found in {} file(s).", test_files.len());
            process::exit(0);
        }
        if total_skipped > 0 {
            println!("{total_passed} passed, {total_failed} failed, {total_skipped} skipped ({files_with_tests} file(s))");
        } else {
            println!("{total_passed} passed, {total_failed} failed ({files_with_tests} file(s))");
        }
        #[cfg(unix)]
        if let Some((who, signo)) = first_signal {
            eprintln!(
                "gg: test file {} terminated by {} (signal {signo})",
                who.display(),
                signal_name(signo)
            );
            process::exit(128 + signo);
        }
        process::exit(if any_failed { 1 } else { 0 });
    }

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
                    report_semantic_warnings_filtered(&reporter, &result.warnings, show_clones);
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
                    trace_filename,
                    hot_reload: hot_reload_flag || source_has_hot_reload(&source),
                    sanitize, scheduler_mode, release,
                    ..Default::default()
                };
                let result = try_build_ir(filename, &source, dep_paths, None, None, Some(shared_path), &features, lowering_opts, emit_gir, emit_lir, emit_c_lir, &clone_modes, &resolver_modes, backend_name, target);
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
                    trace_filename,
                    hot_reload: hot_reload_flag || source_has_hot_reload(&source),
                    sanitize, release,
                    ..Default::default()
                };
                let result = try_build_ir(filename, &source, dep_paths, None, shared_output_path.as_deref(), None, &features, lowering_opts, emit_gir, emit_lir, emit_c_lir, &clone_modes, &resolver_modes, backend_name, target);
                match result {
                    Ok(p) => if !emit_gir && !emit_lir && !emit_c_lir { println!("Built: {}", p.display()); }
                    Err(e) => {
                        eprintln!("{e}");
                        process::exit(1);
                    }
                }
            }
        }
        "profile" => {
            // `gg profile <file.gg>` — profile compilation phases
            // `gg profile --compare <baseline.json> <current.json>` — diff two profiles
            if args.iter().any(|a| a == "--compare") {
                // Find the two JSON file paths after --compare
                let json_files: Vec<&String> = args.iter().skip(2)
                    .filter(|a| !a.starts_with("--") && a.ends_with(".json"))
                    .collect();
                if json_files.len() != 2 {
                    eprintln!("Usage: gg profile --compare <baseline.json> <current.json>");
                    process::exit(1);
                }
                if let Err(e) = compare_profiles(json_files[0], json_files[1]) {
                    eprintln!("{e}");
                    process::exit(1);
                }
            } else {
                let dep_paths = resolve_deps_for_file(filename);
                let lowering_opts = gorget::ir::lowering::LoweringOptions {
                    strip_asserts,
                    no_strip_asserts,
                    ..Default::default()
                };
                if let Err(e) = try_profile(filename, &source, dep_paths, &features, lowering_opts) {
                    eprintln!("{e}");
                    process::exit(1);
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
                trace_filename,
                hot_reload: hot_reload_flag || source_has_hot_reload(&source),
                sanitize, scheduler_mode, release,
                ..Default::default()
            };
            let result = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, None, &features, lowering_opts, false, false, false, &clone_modes, &resolver_modes, "c-lir", "native");
            match result {
                Ok(exe_path) => {
                    // Forward positional args that appear AFTER the script filename
                    // to the running program. Anything starting with `--` and any
                    // flag-with-value pair is treated as belonging to gg itself.
                    let flags_with_values = ["--tag", "--exclude-tag", "--filter", "--report",
                        "--output", "-o", "--feature", "--backend", "--target"];
                    let mut script_args: Vec<&String> = Vec::new();
                    let mut seen_filename = false;
                    let mut skip_next = false;
                    for arg in args.iter().skip(2) {
                        if skip_next { skip_next = false; continue; }
                        if flags_with_values.contains(&arg.as_str()) { skip_next = true; continue; }
                        if arg.starts_with("--") { continue; }
                        if !seen_filename && arg == filename { seen_filename = true; continue; }
                        if seen_filename { script_args.push(arg); }
                    }
                    let status = Command::new(&exe_path)
                        .args(&script_args)
                        .status()
                        .unwrap_or_else(|e| {
                            eprintln!("Failed to execute {}: {e}", exe_path.display());
                            process::exit(1);
                        });
                    propagate_child_status(status, &exe_path.display().to_string());
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
            let mut nocapture = false;
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
                } else if args[i] == "--nocapture" {
                    nocapture = true;
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
            let exe_path = try_build_ir(filename, &source, dep_paths, Some(tmp_dir.path()), None, None, &features, lowering_opts, false, false, false, &CloneDiagModes::default(), &ResolverDiagModes::default(), "c-lir", "native")
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
                    let mut worker_cmd = Command::new(&exe_path);
                    worker_cmd.env("GORGET_PARALLEL_ID", worker_id.to_string())
                        .env("GORGET_PARALLEL_TOTAL", n.to_string())
                        .env("GORGET_TEST_RESULTS", worker_results_path(&results_path, worker_id).display().to_string());
                    if nocapture { worker_cmd.env("GORGET_TEST_NOCAPTURE", "1"); }
                    // ⚠ THE LIKELIER LEAK PATH, and it used to be the untouched
                    // one. If worker k fails to spawn, workers 0..k are already
                    // running and `process::exit(1)` abandons every one of them
                    // — and a spawn failure (fd exhaustion, ENOMEM) is exactly
                    // the situation in which you bail early, so this path fires
                    // when the box is ALREADY under pressure.
                    match worker_cmd.spawn() {
                        Ok(child) => children.push(child),
                        Err(e) => {
                            eprintln!("Failed to spawn worker {worker_id}: {e}");
                            for c in children.iter_mut() {
                                proc_guard::kill_process_tree(c);
                            }
                            process::exit(1);
                        }
                    }
                }
                let mut any_failed = false;
                #[cfg(unix)]
                let mut first_signal: Option<(usize, i32)> = None;
                let mut children: Vec<_> = children.into_iter().collect();
                // ⚠ Bailing out of this loop used to ORPHAN every worker that
                // had not been waited on yet: `process::exit(1)` inside the loop
                // skips their `wait`, and nothing else in the process ever
                // reaps them. They keep running the user's tests, writing to the
                // user's result files, after `gg` has reported failure and
                // exited. Same class as the test-harness orphan this release
                // closes -- in the shipped compiler.
                fn reap_remaining(rest: &mut Vec<std::process::Child>) {
                    for c in rest.iter_mut() {
                        proc_guard::kill_process_tree(c);
                    }
                    rest.clear();
                }
                let mut worker_id = 0usize;
                while !children.is_empty() {
                    let mut child = children.remove(0);
                    let status = match child.wait() {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("Failed to wait for worker: {e}");
                            proc_guard::kill_process_tree(&mut child);
                            reap_remaining(&mut children);
                            process::exit(1);
                        }
                    };
                    if !status.success() { any_failed = true; }
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;
                        if first_signal.is_none() {
                            if let Some(signo) = status.signal() {
                                first_signal = Some((worker_id, signo));
                            }
                        }
                    }
                    worker_id += 1;
                }
                // Merge worker result files
                merge_parallel_results(&results_path, n);
                #[cfg(unix)]
                if let Some((who, signo)) = first_signal {
                    eprintln!(
                        "gg: test worker {who} terminated by {} (signal {signo})",
                        signal_name(signo)
                    );
                    process::exit(128 + signo);
                }
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
            if nocapture {
                cmd.env("GORGET_TEST_NOCAPTURE", "1");
            }
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
            propagate_child_status(status, &exe_path.display().to_string());
        }
        "fmt" => {
            let in_place = args.iter().any(|a| a == "--in-place" || a == "-i");
            let check = args.iter().any(|a| a == "--check" || a == "-c");
            let formatted = match gorget::formatter::format_source_result(&source) {
                Ok(s) => s,
                Err(errs) => {
                    // Core #8: never silently drop unparseable lines from the
                    // formatted output. Render diagnostics and exit non-zero
                    // WITHOUT writing to disk or emitting a partial format.
                    let reporter = ErrorReporter::new(filename.clone(), source.clone());
                    for err in &errs {
                        reporter.report_parse_error(err);
                    }
                    eprintln!("\n{} parse error(s) found; refusing to format", errs.len());
                    process::exit(1);
                }
            };
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
        _ => {
            eprintln!("Unknown command: {command}");
            eprintln!("Compiler commands: lex, parse, check, build, run, test, fmt, report");
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

/// Recursively discover `.gg` files under `dir` that contain test blocks.
/// Skips hidden directories (starting with `.`) and non-`.gg` files.
/// If `bench_mode` is true, also includes files with only `bench` blocks.
/// Returns paths sorted alphabetically for deterministic ordering.
fn discover_test_files(dir: &Path, bench_mode: bool) -> Vec<PathBuf> {
    let mut files = Vec::new();
    discover_test_files_recursive(dir, bench_mode, &mut files);
    files.sort();
    files
}

fn discover_test_files_recursive(dir: &Path, bench_mode: bool, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    let mut dirs = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        // Skip hidden entries.
        if name_str.starts_with('.') {
            continue;
        }
        if path.is_dir() {
            dirs.push(path);
        } else if path.extension().and_then(|e| e.to_str()) == Some("gg") {
            if let Ok(contents) = fs::read_to_string(&path) {
                if file_has_test_blocks(&contents, bench_mode) {
                    out.push(path);
                }
            }
        }
    }
    for d in dirs {
        discover_test_files_recursive(&d, bench_mode, out);
    }
}

/// Fast heuristic: check if source contains `test "` at the start of a line.
/// In bench mode, `bench "` also qualifies.
fn file_has_test_blocks(source: &str, bench_mode: bool) -> bool {
    for line in source.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("test \"") {
            return true;
        }
        if bench_mode && trimmed.starts_with("bench \"") {
            return true;
        }
    }
    false
}
