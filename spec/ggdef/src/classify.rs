//! Fixture classification — the ONE predicate shared by both the
//! `converter_agreement` gate and the `migrate` tool (D1 FOLD-2 R1).
//!
//! Hoisted VERBATIM from `tests/converter_agreement.rs` (a PURE code-move — no
//! classification-logic change): the `run_gg`-needle extractor over
//! `tests/integration.rs`, the Rust-string-literal decoder, the decimal-number
//! float heuristic, the EXCLUDE list, and the fuel bound. `migrate` selects
//! EXACTLY the `Agree` bucket via the SAME `classify_fixture` the gate asserts
//! on, so the migration set and the agreement floor can never drift apart.
//!
//! ggdef's big-step evaluator recurses on the NATIVE stack (fuel bounds the
//! step count, not eval depth), so every caller that walks the fixture corpus
//! must run inside a large-stack thread (2 GiB — the scout-validated setup).

use std::path::Path;

use crate::Outcome;

/// The classification fuel bound (converter_agreement.rs:87). Bounds the STEP
/// count, not native recursion depth — run the walk in a big-stack thread.
pub const CLASSIFY_FUEL: u64 = 3_000_000;

/// Fixtures excluded from the classification walk. The designed
/// unbounded-recursion fixture blows ggdef's native stack (a filed
/// native-recursion finding — see `coverage_histogram.rs`).
pub const CLASSIFY_EXCLUDE: &[&str] = &["stack_guard_deep_recursion.gg"];

/// The verdict for one fixture: does ggdef reach a `Value` whose stdout agrees
/// with the committed `run_gg` expectation extracted from `integration.rs`?
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Classification {
    /// ggdef reached a `Value` AND its stdout (trimmed) equals the committed
    /// `run_gg` string (trimmed). THE P1-D migration set (`adjudicator: ggdef`).
    Agree,
    /// `Value`, but differs and either side prints a decimal number — the D8
    /// float rendering HOLD (not a bug here).
    Float,
    /// `Value`, but differs on NON-float output — a candidate ggdef coverage
    /// gap (invariant #8, must become a LOUD ElabError) OR a crude-extractor
    /// artifact. Carries both strings for the triage print.
    Other { ggdef_stdout: String, committed: String },
    /// ggdef ran but produced a non-`Value` outcome (Trap/IllFormed/Fuel).
    NotValue,
    /// ggdef reached a `Value` but no committed `run_gg("<name>", …)` pair
    /// exists (or its literal could not be parsed).
    NoPair,
    /// ggdef could not even run the program (parse / elaborate error) — outside
    /// the definitional surface. (The old `converter_agreement` walk simply
    /// `continue`d on this; kept a distinct verdict so callers can count it.)
    FrontendError,
}

/// The classifiable fixture universe: every top-level `tests/fixtures/*.gg`
/// that is NOT part of the `cow_*` / `deadwrite_*` phase-0 corpus (gated
/// separately by `corpus_b.rs`) and not in `CLASSIFY_EXCLUDE`. Sorted for
/// determinism. Both the agreement gate and `migrate` walk THIS set, so their
/// universes are identical by construction.
pub fn classifiable_fixture_names(fixtures_dir: &Path) -> Vec<String> {
    let mut names: Vec<String> = std::fs::read_dir(fixtures_dir)
        .expect("read tests/fixtures")
        .filter_map(|e| {
            let n = e.unwrap().file_name().into_string().unwrap();
            (n.ends_with(".gg")
                && !n.starts_with("cow_")
                && !n.starts_with("deadwrite_")
                && !CLASSIFY_EXCLUDE.contains(&n.as_str()))
            .then_some(n)
        })
        .collect();
    names.sort();
    names
}

/// Classify ONE fixture against its committed `run_gg` expectation. `integ` is
/// the full text of `tests/integration.rs` (read once by the caller and reused
/// across the walk). Runs ggdef at `CLASSIFY_FUEL`; the caller must provide a
/// large native stack (ggdef's big-step eval recurses natively).
pub fn classify_fixture(name: &str, src: &str, integ: &str) -> Classification {
    let run = match crate::run_source(src, CLASSIFY_FUEL) {
        Ok(r) => r,
        Err(_) => return Classification::FrontendError,
    };
    if !matches!(run.outcome, Outcome::Value(_)) {
        return Classification::NotValue;
    }
    let Some(exp) = extract_run_gg_expectation(integ, name) else {
        return Classification::NoPair;
    };
    if run.stdout.trim() == exp.trim() {
        Classification::Agree
    } else if has_decimal_number(&exp) || has_decimal_number(&run.stdout) {
        Classification::Float
    } else {
        Classification::Other { ggdef_stdout: run.stdout, committed: exp }
    }
}

/// Extract and decode the committed `run_gg("<name>", "<expected>")` stdout
/// literal from `tests/integration.rs`. Handles the two committed call layouts
/// (inline, and the `run_gg(\n        "name",` line-wrapped form). Returns
/// `None` when no pair is present or the literal cannot be parsed — the caller
/// treats that as `NoPair`.
pub fn extract_run_gg_expectation(integ: &str, name: &str) -> Option<String> {
    let needle = format!("run_gg(\"{name}\", ");
    let alt = format!("run_gg(\n        \"{name}\",");
    let p = integ
        .find(&needle)
        .map(|p| p + needle.len())
        .or_else(|| integ.find(&alt).map(|p| p + alt.len()))?;
    let q = integ[p..].find('"')?;
    parse_rust_str_lit(&integ[p + q..])
}

/// Parse a Rust double-quoted string literal starting at `s[0] == '"'`,
/// processing the escapes the harness expectations use — including the
/// `\`+newline string-continuation (drops the newline + next-line indent).
pub fn parse_rust_str_lit(s: &str) -> Option<String> {
    let b = s.as_bytes();
    if b.first() != Some(&b'"') {
        return None;
    }
    let mut i = 1;
    let mut out = String::new();
    while i < b.len() {
        match b[i] {
            b'"' => return Some(out),
            b'\\' => {
                i += 1;
                match b.get(i)? {
                    b'n' => out.push('\n'),
                    b't' => out.push('\t'),
                    b'r' => out.push('\r'),
                    b'\\' => out.push('\\'),
                    b'"' => out.push('"'),
                    b'0' => out.push('\0'),
                    b'\n' => {
                        i += 1;
                        while i < b.len() && (b[i] == b' ' || b[i] == b'\t') {
                            i += 1;
                        }
                        continue;
                    }
                    other => out.push(*other as char),
                }
                i += 1;
            }
            other => {
                out.push(other as char);
                i += 1;
            }
        }
    }
    None
}

/// Heuristic: does `s` contain a `<digit>.<digit>` decimal number? Used to route
/// a mismatch to the D8 float HOLD rather than treating it as a coverage gap.
pub fn has_decimal_number(s: &str) -> bool {
    let b = s.as_bytes();
    for i in 1..b.len().saturating_sub(1) {
        if b[i] == b'.' && b[i - 1].is_ascii_digit() && b[i + 1].is_ascii_digit() {
            return true;
        }
    }
    false
}
