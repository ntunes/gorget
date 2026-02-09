use std::env;
use std::fs;
use std::path::Path;
use std::process::{self, Command};

use vyper::errors::ErrorReporter;
use vyper::lexer::Lexer;
use vyper::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: vyper <command> <file>");
        eprintln!("Commands: lex, parse, check, build");
        process::exit(1);
    }

    let command = &args[1];
    let filename = &args[2];

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

            let result = vyper::semantic::analyze(&module);

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

            let result = vyper::semantic::analyze(&module);

            if !result.errors.is_empty() {
                let reporter = ErrorReporter::new(filename.clone(), source.clone());
                for err in &result.errors {
                    reporter.report_semantic_error(err);
                }
                eprintln!("\n{} semantic error(s) found", result.errors.len());
                process::exit(1);
            }

            // Generate C code
            let c_code = vyper::codegen::generate_c(&module, &result);

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
            let status = Command::new(&cc)
                .arg("-std=c11")
                .arg("-Wall")
                .arg("-Wextra")
                .arg("-Wno-unused-parameter")
                .arg("-Wno-unused-variable")
                .arg("-Wno-unused-function")
                .arg("-o")
                .arg(&exe_path)
                .arg(&c_path)
                .arg("-lm")
                .status();

            match status {
                Ok(s) if s.success() => {
                    println!("Built: {}", exe_path.display());
                }
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
        _ => {
            eprintln!("Unknown command: {command}");
            eprintln!("Commands: lex, parse, check, build");
            process::exit(1);
        }
    }
}
