//! `ggdef` CLI (deliverable 6).
//!
//!   ggdef run   file.gg   — run; print stdout; exit code per outcome.
//!   ggdef trace file.gg   — run; print stdout, then `---trace---`, then the
//!                           provenance events as JSONL.
//!
//! Exit codes (provisional until the trap-normalization spec lands in B):
//!   0   Value          101 Trap          102 IllFormed      103 FuelExhausted
//!   2   usage / frontend (parse or elaboration) error — NOT an outcome.

use std::process::ExitCode;

use ggdef::{run_source, FrontendError, Outcome, DEFAULT_FUEL};

const EXIT_USAGE: u8 = 2;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    match (args.get(1).map(String::as_str), args.get(2)) {
        (Some("run"), Some(file)) => cmd(file, false),
        (Some("trace"), Some(file)) => cmd(file, true),
        _ => {
            eprintln!("usage: ggdef run|trace <file.gg>");
            ExitCode::from(EXIT_USAGE)
        }
    }
}

fn cmd(file: &str, emit_trace: bool) -> ExitCode {
    let source = match std::fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ggdef: cannot read `{file}`: {e}");
            return ExitCode::from(EXIT_USAGE);
        }
    };

    let run = match run_source(&source, DEFAULT_FUEL) {
        Ok(r) => r,
        Err(e @ FrontendError::Parse(_)) | Err(e @ FrontendError::Elaborate(_)) => {
            eprintln!("ggdef: {e}");
            return ExitCode::from(EXIT_USAGE);
        }
    };

    // The program's stdout is THE observable (RFC §2.1).
    print!("{}", run.stdout);

    if emit_trace {
        println!("---trace---");
        for ev in &run.trace {
            println!("{}", ev.to_json());
        }
    }

    // A non-Value outcome gets a short diagnostic on stderr; the machine-
    // readable signal is the exit code.
    match &run.outcome {
        Outcome::Value(_) => {}
        Outcome::Trap(f) => eprintln!("ggdef: trap: {}", f.message()),
        Outcome::IllFormed(m) => eprintln!("ggdef: ill-formed: {m}"),
        Outcome::FuelExhausted => eprintln!("ggdef: fuel exhausted (non-termination guard)"),
    }

    ExitCode::from(run.outcome.exit_code() as u8)
}
