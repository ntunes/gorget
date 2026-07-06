//! ggdef ↔ production OUTPUT-AGREEMENT gate (promoted from the scout probe
//! `scout_probe.rs::scout_converter_agreement`; brief pass-1 R1).
//!
//! For every non-`cow_*`/non-`deadwrite_*` fixture that ggdef elaborates to a
//! `Value`, compare ggdef's stdout to the committed `run_gg("fixture", "…")`
//! expectation extracted from `tests/integration.rs` (never retyped — the same
//! extraction discipline as `corpus_b.rs`). Three outcomes:
//!
//!   * **AGREE**          — ggdef stdout == the committed production output.
//!                          This is the P1-D converter's `adjudicator: ggdef`
//!                          set (modulo D8 float rendering, held separately).
//!   * **FLOAT-mismatch** — differs, and either side prints a decimal number.
//!                          D8 (shortest-round-trip) is HELD until the
//!                          production float fix lands; not a bug here.
//!   * **OTHER-mismatch** — differs on a NON-float output. A candidate: either
//!                          a real ggdef coverage gap producing a WRONG value
//!                          (invariant #8 — must become a LOUD ElabError, never
//!                          a silent wrong Value) OR a crude-extractor artifact
//!                          (a truncated committed string). Printed for triage.
//!
//! `AGREE` is the monotone CORRECTNESS floor: it only rises when ggdef gets
//! MORE fixtures byte-correct. The safety rule (silent-wrong → loud ElabError)
//! can only move a fixture OUT of OTHER-mismatch into a frontend error (not
//! counted) — it never lowers AGREE. So an AGREE drop is a real regression.

use std::fs;
use std::path::{Path, PathBuf};

fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

/// See `coverage_histogram.rs`: the designed unbounded-recursion fixture blows
/// ggdef's native stack. Excluded from the walk.
const EXCLUDE: &[&str] = &["stack_guard_deep_recursion.gg"];

// ── INLINE MONOTONE FLOOR (regenerate + bump in the landing commit) ──────────
//
// Regenerate with:
//   cargo test -p ggdef --test converter_agreement -- --nocapture
// and read the "AGREE" count. NEVER lower it without a cited reason.
const AGREE_FLOOR: usize = 181;

#[test]
fn converter_agreement() {
    // ggdef's recursive big-step eval is not stack-bounded by fuel; run the
    // walk in a 2 GiB-stack thread (the scout-validated setup).
    std::thread::Builder::new()
        .stack_size(2 * 1024 * 1024 * 1024)
        .name("converter_agreement".to_string())
        .spawn(converter_agreement_body)
        .unwrap()
        .join()
        .unwrap();
}

fn converter_agreement_body() {
    let root = ws_root();
    let dir = root.join("tests/fixtures");
    let integ = fs::read_to_string(root.join("tests/integration.rs")).unwrap();
    let mut names: Vec<String> = fs::read_dir(&dir)
        .unwrap()
        .filter_map(|e| {
            let n = e.unwrap().file_name().into_string().unwrap();
            if n.ends_with(".gg")
                && !n.starts_with("cow_")
                && !n.starts_with("deadwrite_")
                && !EXCLUDE.contains(&n.as_str())
            {
                Some(n)
            } else {
                None
            }
        })
        .collect();
    names.sort();

    let mut agree = 0usize;
    let mut float_mismatch = 0usize;
    let mut other_mismatch = 0usize;
    let mut no_pair = 0usize;
    let mut not_value = 0usize;
    let mut findings: Vec<String> = Vec::new();

    for name in &names {
        let src = fs::read_to_string(dir.join(name)).unwrap();
        let run = match ggdef::run_source(&src, 3_000_000) {
            Ok(r) => r,
            Err(_) => continue,
        };
        if !matches!(run.outcome, ggdef::Outcome::Value(_)) {
            not_value += 1;
            continue;
        }
        // Extract the committed run_gg expectation for this fixture.
        let needle = format!("run_gg(\"{name}\", ");
        let alt = format!("run_gg(\n        \"{name}\",");
        let pos = integ
            .find(&needle)
            .map(|p| p + needle.len())
            .or_else(|| integ.find(&alt).map(|p| p + alt.len()));
        let Some(p) = pos else {
            no_pair += 1;
            continue;
        };
        let Some(q) = integ[p..].find('"') else {
            no_pair += 1;
            continue;
        };
        let Some(exp) = parse_rust_str_lit(&integ[p + q..]) else {
            no_pair += 1;
            continue;
        };
        if run.stdout.trim() == exp.trim() {
            agree += 1;
        } else if has_decimal_number(&exp) || has_decimal_number(&run.stdout) {
            float_mismatch += 1;
        } else {
            other_mismatch += 1;
            if findings.len() < 25 {
                findings.push(format!(
                    "{name}: ggdef={:?} vs committed={:?}",
                    run.stdout.replace('\n', "\\n"),
                    exp.replace('\n', "\\n")
                ));
            }
        }
    }

    eprintln!("\n=== ggdef ↔ production OUTPUT-AGREEMENT (in-surface VALUE fixtures w/ run_gg pair) ===");
    eprintln!("  AGREE {agree}  [floor {AGREE_FLOOR}]  ·  FLOAT-mismatch {float_mismatch}  ·  OTHER-mismatch {other_mismatch}");
    eprintln!("  (not-value {not_value} · no-run_gg-pair {no_pair})");
    eprintln!("  OTHER-mismatch samples (candidate ggdef gaps [invariant #8] OR extractor artifacts):");
    for f in &findings {
        eprintln!("    {f}");
    }
    eprintln!();

    assert!(
        agree >= AGREE_FLOOR,
        "ggdef↔production AGREEMENT REGRESSED: AGREE {agree} < floor {AGREE_FLOOR}. \
         A fixture ggdef used to get byte-correct now diverges — a real regression \
         (NOT a silent-wrong safety narrowing, which lowers OTHER-mismatch, not AGREE).",
    );
}

/// Parse a Rust double-quoted string literal starting at `s[0] == '"'`,
/// processing the escapes the harness expectations use — including the
/// `\`+newline string-continuation (drops the newline + next-line indent).
fn parse_rust_str_lit(s: &str) -> Option<String> {
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

fn has_decimal_number(s: &str) -> bool {
    let b = s.as_bytes();
    for i in 1..b.len().saturating_sub(1) {
        if b[i] == b'.' && b[i - 1].is_ascii_digit() && b[i + 1].is_ascii_digit() {
            return true;
        }
    }
    false
}
