//! `ggdef` — the definitional interpreter for Gorget Core (GGC).
//!
//! `ggdef` IS the meaning of Gorget's dynamic semantics (RFC §1): a small
//! eager-value-semantics interpreter whose behaviour defines what a program
//! means. Production stays borrows + lazy CoW — an optimisation with an
//! observational-equivalence obligation (D1). This crate shares the production
//! **lexer + parser + AST only**; the fence against `ir`/`semantic`/`lir`/
//! `bir`/`backend` is the import ratchet in `tests/lints.rs`.
//!
//! Increment A is the walking skeleton: the elaborator + evaluator for the
//! honest subset the first ~20 `cow_*` fixtures use (int/bool/float scalars,
//! `String`, `Vector`, structs, tuples; bare/`&`/`!` modes; materialize-on-
//! write). See `docs/plans/define-gorget/phase0-brief.md`.

pub mod elaborate;
pub mod eval;
pub mod ggc;
pub mod trace;

pub use elaborate::{elaborate, ElabError};
pub use eval::{run, Fault, Outcome, Run, EXIT_FUEL, EXIT_ILLFORMED, EXIT_TRAP, EXIT_VALUE};

use gorget::parser::Parser;

/// The default fuel bound for CLI runs — generous; every phase-0 fixture
/// terminates in far fewer steps. Non-termination surfaces as `FuelExhausted`.
pub const DEFAULT_FUEL: u64 = 50_000_000;

/// A frontend (parse + elaborate) failure, distinct from an evaluation
/// outcome. Parse errors reclassify a program to the `parse-error` tier;
/// elaboration errors are the stop-and-report signal (a construct outside the
/// A subset, or a shape the elaborator will not fake).
#[derive(Debug)]
pub enum FrontendError {
    Parse(Vec<String>),
    Elaborate(ElabError),
}

impl std::fmt::Display for FrontendError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FrontendError::Parse(errs) => {
                writeln!(f, "{} parse error(s):", errs.len())?;
                for e in errs {
                    writeln!(f, "  {e}")?;
                }
                Ok(())
            }
            FrontendError::Elaborate(e) => {
                write!(f, "elaboration error @ {}..{}: {}", e.span.start, e.span.end, e.message)
            }
        }
    }
}

/// Parse + elaborate a source string into a GGC program.
pub fn frontend(source: &str) -> Result<ggc::Program, FrontendError> {
    let mut parser = Parser::new(source);
    let module = parser.parse_module();
    if !parser.errors.is_empty() {
        return Err(FrontendError::Parse(parser.errors.iter().map(|e| e.to_string()).collect()));
    }
    elaborate(&module).map_err(FrontendError::Elaborate)
}

/// Parse, elaborate, and run a source string, bounded by `fuel`.
pub fn run_source(source: &str, fuel: u64) -> Result<Run, FrontendError> {
    Ok(run(&frontend(source)?, fuel))
}

#[cfg(test)]
mod tests;
