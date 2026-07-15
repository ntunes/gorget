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
//! Exit codes — the ratified TOOLCHAIN EXIT-CODE SCHEME (Option A), decisions.md
//! (the `T_`-code trap format + exit 101 are normative — D11 trap normalization;
//! see `spec/prose/trap-codes.md`):
//!
//!   0    success (Value).
//!   1    STATIC REJECTION — parse OR elaboration OR may-move `IllFormed`. ONE
//!        class: the program never ran (stdout empty; the `error[E_Code]: …`
//!        diagnostic goes to stderr), matching production `gg check`.
//!   2    usage / CLI error — bad args or an unreadable file. NOT an outcome.
//!   101  runtime trap (+ ICE) — the program RAN and died. Distinct from a
//!        static rejection so a crash can't masquerade as a correct reject.
//!   103  fuel exhausted — ggdef-ONLY, out of conformance (the interpreter's
//!        totality device, not a language outcome an implementation reproduces).

use std::path::Path;
use std::process::ExitCode;

use ggdef::{gen_frontmatter, migrate, run_source, FrontendError, Outcome, DEFAULT_FUEL};

/// Usage / CLI error (bad args, unreadable file) — NOT a program outcome.
const EXIT_USAGE: u8 = 2;
/// A static rejection at the frontend (parse / elaboration). The SAME class as a
/// may-move `IllFormed` (whose `Outcome::exit_code()` is also 1) — a source
/// error means the program never ran.
const EXIT_STATIC_REJECT: u8 = 1;

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
        // A parse OR elaboration error is a STATIC REJECTION, not a usage error
        // (the source is well-formed input the toolchain refuses) → exit 1, the
        // same class as a may-move `IllFormed`. stderr carries the diagnostic.
        Err(e @ FrontendError::Parse(_)) | Err(e @ FrontendError::Elaborate(_)) => {
            eprintln!("ggdef: {e}");
            return ExitCode::from(EXIT_STATIC_REJECT);
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
        // Render the normative `trap[T_X]: detail at file:line:col` shape
        // (§10.9 / trap-codes.md "Rendering"). This is a human diagnostic (NOT
        // conformance-compared — Q1), but the definition's own tool should
        // model the format it normativizes. The location resolves from the
        // run's trap provenance (statement-granular); a missing span renders
        // without the suffix rather than a bogus location.
        Outcome::Trap(k) => {
            let loc = run.trap_span.and_then(|sp| {
                let fi = gorget::span::FileInfo::new(file.to_string(), source.clone(), 0);
                gorget::span::offset_to_location(&[fi], sp.start)
                    .map(|(f, line, col)| format!(" at {f}:{line}:{col}"))
            });
            eprintln!("trap[{}]: {}{}", k.code(), k.message(), loc.unwrap_or_default());
        }
        // A static rejection renders the production-diagnostic-family shape
        // `error[E_Code]: <message> at file:line:col` (THE VERDICT TRIPLE, pin 1)
        // — stderr is ELABORATE's channel (why the program was rejected). The
        // location resolves via the SAME `offset_to_location` machinery as the
        // trap arm; a missing span (or an eval-internal `IllFormed` with no
        // ratified code) renders without the offending-code / location suffix
        // rather than a bogus one.
        Outcome::IllFormed(m) => {
            let loc = run.illformed_span.and_then(|sp| {
                let fi = gorget::span::FileInfo::new(file.to_string(), source.clone(), 0);
                gorget::span::offset_to_location(&[fi], sp.start)
                    .map(|(f, line, col)| format!(" at {f}:{line}:{col}"))
            });
            match run.reject_code {
                Some(code) => eprintln!("error[{code}]: {m}{}", loc.unwrap_or_default()),
                None => eprintln!("error: {m}{}", loc.unwrap_or_default()),
            }
        }
        Outcome::FuelExhausted => eprintln!("ggdef: fuel exhausted (non-termination guard)"),
    }

    ExitCode::from(run.outcome.exit_code() as u8)
}
