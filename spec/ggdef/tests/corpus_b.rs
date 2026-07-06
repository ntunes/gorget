//! Increment-B2 conformance gate — the ENTIRE cow_* / deadwrite_* corpus
//! (RFC §6(a)+(b)), the phase-0 acceptance bar.
//!
//! Runs `ggdef` over every `cow_*` / `deadwrite_*` fixture minus the standing
//! exclusions and compares its stdout to the committed expectation, extracted
//! from `tests/integration.rs` — never retyped (the brief's extraction
//! discipline, identical to `corpus_b1.rs`):
//!
//!   * **run_gg pair** (`run_gg("fixture.gg", "expected")`, incl. the multi-line
//!     `\`-continuation form) → MATCH-gated;
//!   * **self-host `assert_eq!(stdout, …)`** (the two EMove fixtures) →
//!     MATCH-gated;
//!   * **neither** (the `deadwrite_*` programs are wired via `check_gg_warns` on
//!     stderr; a handful of `cow_*` have no committed stdout pair) →
//!     **REPORT-ONLY**: the expectation is genuinely unextractable, so ggdef's
//!     output is recorded for ratification (see
//!     `reports/deadwrite_spec_expectations.md`), NOT guessed. Still gated to a
//!     clean `Value` outcome.
//!
//! Exclusions (RFC §6 + the brief): the three GENERIC-equip cow fixtures
//! (`cow_element_borrow_alias_mutate`, `cow_p3_alias_chain_mutate`,
//! `cow_p3_index_mutate` — generic-equip-on-builtin is optional in phase 0) and
//! `deadwrite_ok_atomic_add` (std.sync atomics are phase 3).
//!
//! Acceptance B2: every MATCH-gated fixture MATCHes. A mismatch is NEVER
//! silently "fixed" by patching `eval.rs`; it is triaged against the brief's
//! divergence table (D2 self-write-through / D1-EMove / the two smith bugs / the
//! 3 PRE-ADJUDICATED deadwrite deltas → EXPECTED; anything else → STOP-and-report).

use std::fs;
use std::path::{Path, PathBuf};

/// Standing exclusions (RFC §6): the 3 generic-equip cow fixtures +
/// `deadwrite_ok_atomic_add`. Unlike B1, equip fixtures are IN the B2 gate, so
/// these are excluded BY NAME (the generic-equip ones are the only equip
/// fixtures ggdef cannot elaborate — generic monomorph is phase 1).
const EXCLUDE: &[&str] = &[
    "deadwrite_ok_atomic_add.gg",
    "cow_element_borrow_alias_mutate.gg",
    "cow_p3_alias_chain_mutate.gg",
    "cow_p3_index_mutate.gg",
];

fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
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
                if i >= b.len() {
                    return None;
                }
                match b[i] {
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
                    other => out.push(other as char),
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

#[derive(PartialEq, Debug)]
enum Provenance {
    RunGg,
    SelfHost,
    ReportOnly,
}

/// Whether the `"fixture.gg"` string literal starting at `pos` is the first
/// argument of a `run_gg(...)` call (the previous non-whitespace text is
/// exactly `run_gg(`, allowing the multi-line form).
fn preceded_by_run_gg(integ: &str, pos: usize) -> bool {
    let start = pos.saturating_sub(64);
    integ[start..pos].trim_end().ends_with("run_gg(")
}

/// The committed expectation for `fixture` (e.g. `cow_x.gg`) and how it was
/// sourced. `RunGg` reads the 2nd string of the `run_gg(...)` call; `SelfHost`
/// reads the `assert_eq!(stdout, …)` inside the fixture's self-host test.
fn expected_for(integ: &str, fixture: &str) -> (Provenance, Option<String>) {
    let needle = format!("\"{fixture}\"");
    let mut search = 0;
    while let Some(rel) = integ[search..].find(&needle) {
        let pos = search + rel;
        if preceded_by_run_gg(integ, pos) {
            let after = pos + needle.len();
            if let Some(q) = integ[after..].find('"') {
                if let Some(exp) = parse_rust_str_lit(&integ[after + q..]) {
                    return (Provenance::RunGg, Some(exp));
                }
            }
        }
        search = pos + needle.len();
    }
    // Self-host fallback (fn-bounded, so a check_gg_warns test with no nearby
    // stdout assert never spuriously matches a distant one).
    let path_needle = format!("fixtures/{fixture}");
    if let Some(pos) = integ.find(&path_needle) {
        let rest = &integ[pos..];
        let bound = rest
            .find("\nfn ")
            .into_iter()
            .chain(rest.find("\n#[test]"))
            .min()
            .unwrap_or(rest.len());
        let seg = &rest[..bound];
        if let Some(a) = seg.find("assert_eq!(") {
            let seg2 = &seg[a..];
            if let Some(sp) = seg2.find("stdout") {
                if let Some(q) = seg2[sp..].find('"') {
                    if let Some(exp) = parse_rust_str_lit(&seg2[sp + q..]) {
                        return (Provenance::SelfHost, Some(exp));
                    }
                }
            }
        }
    }
    (Provenance::ReportOnly, None)
}

/// Discover the B2 gate set: every `cow_*` / `deadwrite_*` fixture minus the
/// standing exclusions (equip fixtures INCLUDED, unlike B1).
fn gate_fixtures(root: &Path) -> Vec<String> {
    let dir = root.join("tests/fixtures");
    let mut names: Vec<String> = fs::read_dir(&dir)
        .expect("read tests/fixtures")
        .filter_map(|e| {
            let n = e.unwrap().file_name().into_string().unwrap();
            let is_corpus = (n.starts_with("cow_") || n.starts_with("deadwrite_")) && n.ends_with(".gg");
            if !is_corpus || EXCLUDE.contains(&n.as_str()) {
                return None;
            }
            Some(n)
        })
        .collect();
    names.sort();
    names
}

#[test]
fn corpus_b_all_match() {
    let root = ws_root();
    let integ = fs::read_to_string(root.join("tests/integration.rs")).expect("read integration.rs");
    let fixtures = gate_fixtures(&root);

    let mut table = String::from("\n=== corpus_b (full phase-0 corpus) table ===\n");
    let mut matched = 0usize;
    let mut report_only: Vec<String> = Vec::new();
    let mut failures: Vec<String> = Vec::new();

    for fixture in &fixtures {
        let src = fs::read_to_string(root.join("tests/fixtures").join(fixture)).unwrap();
        let run = ggdef::run_source(&src, ggdef::DEFAULT_FUEL)
            .unwrap_or_else(|e| panic!("{fixture}: frontend error (STOP-and-report): {e}"));
        let got = run.stdout.clone();
        let (prov, expected) = expected_for(&integ, fixture);

        match expected {
            Some(exp) => {
                let ok = got.trim() == exp.trim();
                let tag = match prov {
                    Provenance::RunGg => "MATCH/run_gg",
                    Provenance::SelfHost => "MATCH/selfhost",
                    Provenance::ReportOnly => unreachable!(),
                };
                table.push_str(&format!("  {:<15} {}\n", if ok { tag } else { "MISMATCH" }, fixture));
                if ok {
                    matched += 1;
                } else {
                    failures.push(format!(
                        "  {fixture}: outcome={:?}\n    expected: {:?}\n    got:      {:?}",
                        run.outcome,
                        exp.trim(),
                        got.trim()
                    ));
                }
            }
            None => {
                // REPORT-ONLY: no committed expectation. Gate only that it ran
                // to a clean Value outcome; record the output for ratification.
                let is_value = matches!(run.outcome, ggdef::Outcome::Value(_));
                table.push_str(&format!(
                    "  {:<15} {:<48} => {:?}\n",
                    if is_value { "REPORT-ONLY" } else { "REPORT-ERR" },
                    fixture,
                    got.replace('\n', "\\n")
                ));
                report_only.push(fixture.clone());
                if !is_value {
                    failures.push(format!(
                        "  {fixture}: REPORT-ONLY fixture did not reach a Value outcome: {:?}",
                        run.outcome
                    ));
                }
            }
        }
    }

    table.push_str(&format!(
        "\n  total={} · MATCH-gated={} · REPORT-ONLY={}\n",
        fixtures.len(),
        matched,
        report_only.len()
    ));
    eprintln!("{table}");

    assert!(
        failures.is_empty(),
        "corpus_b: {} issue(s) — triage against the divergence table before touching eval.rs:\n{}",
        failures.len(),
        failures.join("\n")
    );

    // Guard the gate's shape: the full phase-0 corpus is 116 fixtures
    // (120 cow_*/deadwrite_* minus the 4 standing exclusions).
    assert_eq!(fixtures.len(), 116, "B2 gate set drifted from 116 fixtures");
}
