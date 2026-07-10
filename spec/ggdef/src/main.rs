//! `ggdef` CLI (deliverable 6; `gen` added in Increment C).
//!
//!   ggdef run   file.gg   — run; print stdout; exit code per outcome.
//!   ggdef trace file.gg   — run; print stdout, then `---trace---`, then the
//!                           provenance events as JSONL.
//!   ggdef gen   file.gg   — run a `spectests/` fixture and write the observed
//!                           outcome into its frontmatter `expect:` block, in
//!                           place. Idempotent (RFC §4 "expectations flow FROM
//!                           the definition").
//!
//! Exit codes (the `T_`-code trap format + exit 101 are normative — D11 trap
//! normalization; see `spec/prose/trap-codes.md`):
//!   0   Value          101 Trap          102 IllFormed      103 FuelExhausted
//!   2   usage / frontend (parse or elaboration) error — NOT an outcome.

use std::path::Path;
use std::process::ExitCode;

use ggdef::{gen_frontmatter, migrate, run_source, FrontendError, Outcome, DEFAULT_FUEL};

const EXIT_USAGE: u8 = 2;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    match (args.get(1).map(String::as_str), args.get(2)) {
        (Some("run"), Some(file)) => cmd(file, false),
        (Some("trace"), Some(file)) => cmd(file, true),
        (Some("gen"), Some(file)) => cmd_gen(file),
        // `migrate` walks the corpus itself — it takes no file argument.
        (Some("migrate"), _) => cmd_migrate(),
        _ => {
            eprintln!("usage: ggdef run|trace|gen <file.gg> | migrate");
            ExitCode::from(EXIT_USAGE)
        }
    }
}

/// `ggdef migrate`: populate `spectests/run/` from the ggdef-adjudicated AGREE
/// set (D1). Expectations flow FROM the definition (RFC §4) via `gen`.
fn cmd_migrate() -> ExitCode {
    // The workspace root, relative to this crate's manifest (spec/ggdef).
    let ws_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..");
    match migrate(&ws_root) {
        Ok(report) => {
            println!("ggdef migrate: wrote {} fixture(s) into spectests/run/", report.migrated.len());
            for (verdict, n) in &report.skipped {
                println!("  skipped {verdict}: {n}");
            }
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("ggdef migrate: {e}");
            ExitCode::from(EXIT_USAGE)
        }
    }
}

/// `ggdef gen <file.gg>`: regenerate the fixture's frontmatter `expect:` block
/// from the observed outcome and write it back in place.
fn cmd_gen(file: &str) -> ExitCode {
    let source = match std::fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ggdef: cannot read `{file}`: {e}");
            return ExitCode::from(EXIT_USAGE);
        }
    };
    let updated = match gen_frontmatter(&source, DEFAULT_FUEL) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("ggdef: gen `{file}`: {e}");
            return ExitCode::from(EXIT_USAGE);
        }
    };
    if updated == source {
        // Idempotent no-op: leave the file (and its mtime) untouched.
        return ExitCode::SUCCESS;
    }
    if let Err(e) = std::fs::write(file, &updated) {
        eprintln!("ggdef: cannot write `{file}`: {e}");
        return ExitCode::from(EXIT_USAGE);
    }
    ExitCode::SUCCESS
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
        // Render the normative `trap[T_X]: detail` shape (§10.9 / trap-codes.md).
        // This is a human diagnostic (NOT conformance-compared — Q1), but the
        // definition's own tool should model the format it normativizes.
        Outcome::Trap(k) => eprintln!("trap[{}]: {}", k.code(), k.message()),
        Outcome::IllFormed(m) => eprintln!("ggdef: ill-formed: {m}"),
        Outcome::FuelExhausted => eprintln!("ggdef: fuel exhausted (non-termination guard)"),
    }

    ExitCode::from(run.outcome.exit_code() as u8)
}
