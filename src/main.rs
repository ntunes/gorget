use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use gorget::errors::ErrorReporter;
use gorget::lexer::Lexer;
use gorget::loader::{self, ModuleLoader};
use gorget::parser::ast::{Item, Module};
use gorget::parser::Parser;

/// Load imported modules and merge them into a single module.
fn load_imports(filename: &str, source: &str, module: gorget::parser::ast::Module) -> gorget::parser::ast::Module {
    let input_path = Path::new(filename).canonicalize().unwrap_or_else(|e| {
        eprintln!("Error resolving path {filename}: {e}");
        process::exit(1);
    });

    let mut ml = ModuleLoader::new();
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

    loader::merge_modules(modules)
}

/// Extract directive flags from a parsed module.
fn extract_directives(module: &Module) -> (bool, bool) {
    let mut strip_asserts = false;
    let mut overflow_wrap = false;
    for item in &module.items {
        if let Item::Directive(d) = &item.node {
            match d.name.as_str() {
                "strip-asserts" => strip_asserts = true,
                "overflow" if d.value.as_deref() == Some("wrap") => overflow_wrap = true,
                _ => {}
            }
        }
    }
    (strip_asserts, overflow_wrap)
}

/// Build a .gg source file into a binary. Returns the path to the executable,
/// or an error string if compilation fails.
fn try_build(
    filename: &str,
    source: &str,
    strip_asserts: bool,
    no_strip_asserts: bool,
    overflow_wrap: bool,
    overflow_checked: bool,
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
    let module = load_imports(filename, source, module);

    // Merge source directives with CLI flags.
    let (dir_strip, dir_overflow) = extract_directives(&module);
    let strip_asserts = if no_strip_asserts {
        false
    } else {
        strip_asserts || dir_strip
    };
    let overflow_wrap = if overflow_checked {
        false
    } else {
        overflow_wrap || dir_overflow
    };

    let result = gorget::semantic::analyze(&module);

    if !result.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), source.to_string());
        for err in &result.errors {
            reporter.report_semantic_error(err);
        }
        return Err(format!("{} semantic error(s) found", result.errors.len()));
    }

    // Generate C code
    let c_code = gorget::codegen::generate_c(&module, &result, strip_asserts, overflow_wrap);

    // Determine output paths
    let input_path = Path::new(filename);
    let dir = input_path.parent().unwrap_or(Path::new("."));
    let stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");
    let c_path = dir.join(format!("{stem}.c"));
    let exe_path = dir.join(stem);

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
        .arg("-Wno-unused-function");
    if overflow_wrap {
        cc_cmd.arg("-fwrapv");
    }
    let status = cc_cmd
        .arg("-o")
        .arg(&exe_path)
        .arg(&c_path)
        .arg("-lm")
        .status();

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

/// Build a .gg source file into a binary. Exits the process on error.
fn build(
    filename: &str,
    source: &str,
    strip_asserts: bool,
    no_strip_asserts: bool,
    overflow_wrap: bool,
    overflow_checked: bool,
) -> PathBuf {
    try_build(filename, source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked)
        .unwrap_or_else(|e| {
            eprintln!("{e}");
            process::exit(1);
        })
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

/// Interactive REPL for Gorget.
fn run_repl() {
    let version = env!("CARGO_PKG_VERSION");
    println!("Gorget {version} REPL");
    println!("Type code and press Enter twice to execute. /help for commands.\n");

    let stdin = io::stdin();
    let mut reader = stdin.lock();

    let mut definitions: Vec<String> = Vec::new(); // top-level defs (functions, structs, etc.)
    let mut statements: Vec<String> = Vec::new();  // statements inside main()
    let mut counter: u64 = 0;

    // Create temp directory
    let tmp_dir = env::temp_dir().join("gorget_repl");
    let _ = fs::create_dir_all(&tmp_dir);

    loop {
        print!(">>> ");
        let _ = io::stdout().flush();

        let mut line = String::new();
        if reader.read_line(&mut line).unwrap_or(0) == 0 {
            // EOF (Ctrl+D)
            println!();
            break;
        }

        let trimmed = line.trim_end_matches('\n').trim_end_matches('\r');

        // Handle special commands
        if trimmed == "/quit" || trimmed == "/exit" {
            break;
        }
        if trimmed == "/reset" {
            definitions.clear();
            statements.clear();
            counter = 0;
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
        if trimmed == "/help" {
            println!("/reset  — clear accumulated code");
            println!("/show   — show accumulated code");
            println!("/quit   — exit REPL (also Ctrl+D)");
            println!("/help   — show this help");
            println!();
            println!("Type code and press Enter. If a line ends with ':',");
            println!("continue with indented lines, then a blank line to execute.");
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
                print!("... ");
                let _ = io::stdout().flush();

                let mut cont_line = String::new();
                if reader.read_line(&mut cont_line).unwrap_or(0) == 0 {
                    break;
                }
                let cont = cont_line.trim_end_matches('\n').trim_end_matches('\r');
                // Blank line or unindented line ends the block
                if cont.is_empty() {
                    break;
                }
                if !cont.starts_with(' ') && !cont.starts_with('\t') {
                    // Not indented — this is a new entry, not continuation
                    // Put it back? We can't easily unread, so just include it
                    block_lines.push(cont.to_string());
                    break;
                }
                block_lines.push(cont.to_string());
            }
        }

        let entry = block_lines.join("\n");

        // Classify: definition or statement?
        let is_def = is_definition_line(&block_lines[0]);

        // Build new candidate buffers
        let mut new_defs = definitions.clone();
        let mut new_stmts = statements.clone();
        if is_def {
            new_defs.push(entry.clone());
        } else {
            new_stmts.push(entry.clone());
        }

        // Generate .gg source
        counter += 1;
        let marker = format!("__GORGET_REPL_MARKER_{counter}__");

        let mut source = String::new();
        for d in &new_defs {
            source.push_str(d);
            source.push('\n');
        }
        if !new_defs.is_empty() {
            source.push('\n');
        }
        source.push_str("void main():\n");
        for s in &new_stmts[..new_stmts.len().saturating_sub(if is_def { 0 } else { 1 })] {
            for line in s.lines() {
                source.push_str("    ");
                source.push_str(line);
                source.push('\n');
            }
        }
        // Print marker, then new statement code
        source.push_str(&format!("    print(\"{marker}\")\n"));
        if !is_def {
            if let Some(newest) = new_stmts.last() {
                for line in newest.lines() {
                    source.push_str("    ");
                    source.push_str(line);
                    source.push('\n');
                }
            }
        }

        // Write temp file
        let gg_path = tmp_dir.join("repl.gg");
        if let Err(e) = fs::write(&gg_path, &source) {
            eprintln!("Error writing temp file: {e}");
            continue;
        }

        let gg_path_str = gg_path.display().to_string();

        // Try to build
        match try_build(&gg_path_str, &source, false, false, false, false) {
            Err(e) => {
                eprintln!("{e}");
                // Don't update buffers on error
            }
            Ok(exe_path) => {
                // Run and capture output
                match Command::new(&exe_path).output() {
                    Ok(output) => {
                        let stdout = String::from_utf8_lossy(&output.stdout);
                        // Print only text after the marker
                        if let Some(pos) = stdout.find(&marker) {
                            let after = &stdout[pos + marker.len()..];
                            // Skip the newline after the marker
                            let after = after.strip_prefix('\n').unwrap_or(after);
                            if !after.is_empty() {
                                print!("{after}");
                            }
                        } else {
                            // No marker found — print everything (shouldn't happen)
                            print!("{stdout}");
                        }
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        if !stderr.is_empty() {
                            eprint!("{stderr}");
                        }

                        // Success — update accumulated state
                        definitions = new_defs;
                        statements = new_stmts;
                    }
                    Err(e) => {
                        eprintln!("Failed to execute: {e}");
                    }
                }
            }
        }
    }

    // Cleanup temp directory (best-effort)
    let _ = fs::remove_dir_all(&tmp_dir);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // No args → launch interactive REPL
    if args.len() < 2 {
        run_repl();
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
        let exe_path = build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked);
        let status = Command::new(&exe_path)
            .status()
            .unwrap_or_else(|e| {
                eprintln!("Failed to execute {}: {e}", exe_path.display());
                process::exit(1);
            });
        process::exit(status.code().unwrap_or(1));
    }

    if args.len() < 3 {
        eprintln!("Usage: gg <file.gg>              Run a script");
        eprintln!("       gg <command> <file.gg>     Run a compiler command");
        eprintln!("       gg                         Interactive REPL");
        eprintln!("Commands: lex, parse, check, build, run, fmt");
        process::exit(1);
    }

    let command = &args[1];
    let strip_asserts = args.iter().any(|a| a == "--strip-asserts");
    let no_strip_asserts = args.iter().any(|a| a == "--no-strip-asserts");
    let overflow_wrap = args.iter().any(|a| a == "--overflow=wrap");
    let overflow_checked = args.iter().any(|a| a == "--overflow=checked");
    let filename = args.iter().skip(2).find(|a| !a.starts_with("--")).unwrap_or_else(|| {
        eprintln!("Usage: gg <command> <file.gg>");
        process::exit(1);
    });

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
            let module = load_imports(filename, &source, module);

            let result = gorget::semantic::analyze(&module);

            if result.errors.is_empty() {
                println!("OK: no semantic errors");
            } else {
                let reporter = ErrorReporter::new(filename.clone(), source.clone());
                for err in &result.errors {
                    reporter.report_semantic_error(err);
                }
                eprintln!("\n{} error(s) found", result.errors.len());
                process::exit(1);
            }
        }
        "build" => {
            let exe_path = build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked);
            println!("Built: {}", exe_path.display());
        }
        "run" => {
            let exe_path = build(filename, &source, strip_asserts, no_strip_asserts, overflow_wrap, overflow_checked);
            let status = Command::new(&exe_path)
                .status()
                .unwrap_or_else(|e| {
                    eprintln!("Failed to execute {}: {e}", exe_path.display());
                    process::exit(1);
                });
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
            eprintln!("Commands: lex, parse, check, build, run, fmt");
            process::exit(1);
        }
    }
}
