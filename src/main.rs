use std::env;
use std::fs;
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

/// Build a .gg source file into a binary. Returns the path to the executable.
fn build(
    filename: &str,
    source: &str,
    strip_asserts: bool,
    no_strip_asserts: bool,
    overflow_wrap: bool,
    overflow_checked: bool,
) -> PathBuf {
    let mut parser = Parser::new(source);
    let module = parser.parse_module();

    if !parser.errors.is_empty() {
        let reporter = ErrorReporter::new(filename.to_string(), source.to_string());
        for err in &parser.errors {
            reporter.report_parse_error(err);
        }
        eprintln!("\n{} parse error(s) found", parser.errors.len());
        process::exit(1);
    }

    // Load imported modules recursively and merge
    let module = load_imports(filename, source, module);

    // Merge source directives with CLI flags.
    // CLI explicit flags override directives: --no-strip-asserts beats
    // `directive strip-asserts`, --overflow=checked beats `directive overflow=wrap`.
    // Otherwise, source directives and CLI enable-flags are OR'd.
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
        eprintln!("\n{} semantic error(s) found", result.errors.len());
        process::exit(1);
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
        eprintln!("Error writing {}: {e}", c_path.display());
        process::exit(1);
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
        Ok(s) if s.success() => exe_path,
        Ok(s) => {
            eprintln!("C compiler exited with: {s}");
            eprintln!("Generated C file: {}", c_path.display());
            process::exit(1);
        }
        Err(e) => {
            eprintln!("Failed to run C compiler '{cc}': {e}");
            eprintln!("Generated C file: {}", c_path.display());
            process::exit(1);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: gg <command> <file>");
        eprintln!("Commands: lex, parse, check, build, run, fmt");
        process::exit(1);
    }

    let command = &args[1];
    let strip_asserts = args.iter().any(|a| a == "--strip-asserts");
    let no_strip_asserts = args.iter().any(|a| a == "--no-strip-asserts");
    let overflow_wrap = args.iter().any(|a| a == "--overflow=wrap");
    let overflow_checked = args.iter().any(|a| a == "--overflow=checked");
    let filename = args.iter().skip(2).find(|a| !a.starts_with("--")).unwrap_or_else(|| {
        eprintln!("Usage: gg <command> <file>");
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
