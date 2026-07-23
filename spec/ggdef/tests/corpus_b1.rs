//! Increment-B1 conformance gate (RFC §6(a)+(b) for the NON-equip surface).
//!
//! Runs `ggdef` over every `cow_*` / `deadwrite_*` fixture **without an `equip`
//! block** (minus the standing exclusions) and compares its stdout to the
//! committed expectation, extracted from `tests/integration.rs` — never
//! retyped (per the brief's extraction discipline):
//!
//!   * **run_gg pair** (`run_gg("fixture.gg", "expected")`, incl. the multi-line
//!     `\`-continuation form) → MATCH-gated;
//!   * **self-host `assert_eq!(stdout, ...)`** (the two EMove fixtures
//!     `cow_lazy_move_bind` / `cow_lazy_move_reassign`, wired via a self-host
//!     driver test rather than `run_gg`) → MATCH-gated;
//!   * **neither** (the `deadwrite_*` programs are wired via `check_gg_warns`
//!     on stderr, and 5 `cow_*` fixtures — `cow_closure_deferred_mutate` +
//!     four `cow_p3_*` — have no committed stdout pair) → **REPORT-ONLY**: the
//!     expectation is genuinely unextractable, so ggdef's output is recorded
//!     for orchestrator/owner ratification (Increment B2's
//!     `deadwrite_spec_expectations.md`), NOT guessed. These are still gated to
//!     produce a clean `Value` outcome (no elaboration STOP, no `IllFormed`).
//!
//! Acceptance B1: every MATCH-gated fixture MATCHes. A mismatch is NEVER
//! silently "fixed" by patching `eval.rs`; it is triaged against the brief's
//! divergence table (D2 self-write-through / D1-EMove / the two smith bugs →
//! EXPECTED; anything else → a STOP-and-report finding).

use std::fs;
use std::path::{Path, PathBuf};

/// The standing exclusions (RFC §6): the three generic-equip cow fixtures are
/// already excluded by the `equip`-block filter; `deadwrite_ok_atomic_add` uses
/// std.sync atomics (phase 3) and is excluded by name.
/// `cow_value_index_field_writethrough` mixes module statics (phase-1 —
/// `Item::StaticDecl` not elaborated) with locals; its LOCAL half was
/// ggdef-adjudicated at authoring (88/30/45, see the fixture comment).
/// `cow_dict_index_field_writethrough` is a Dict index write-place outside the
/// phase-0 subset (`navigate_write` eval.rs:923 has no Map arm → `IllFormed`);
/// its expected output is §3.1-prose-derived (99/41/99).
/// `cow_dict_index_field_single_eval` is the same Dict index write-place
/// (`make()[0].x`) — likewise out of subset; it pins eval-order, not ggdef.
///
/// Track 1A (+ its two remediations) adds ten cross-lane `cow_for_*` /
/// `cow_comprehension_*` fixtures. Eight are out of the phase-0 subset and get
/// EXCLUDE rows here (each row cites the ggdef-run-verified gate that fires):
///   - `cow_for_amp_vector_field_writethrough` / `cow_for_amp_vector_alias_root` /
///     `cow_for_amp_resource_elem_writethrough`: a `for x in &coll` iterable is
///     Increment B2 (`desugar_for` hits the `elaborate/mod.rs` "Increment B2"
///     error on the `&`-mode iterable), same as the standing for-`&` gap.
///   - the enumerate rows: gate depends on the SPELLING — statement-`&` forms
///     hit the :~967 B2 gate first; bare and receiver-wrap forms fall to
///     `binding_name` (:~969). Per-row comments below.
///   - `cow_comprehension_amp_source`: `elaborate_expr` has no comprehension arm,
///     so it hits the catch-all "expression outside the phase-0 subset" error —
///     a DIFFERENT site than the for-`&` B2 gate.
/// The two bare single-binding fixtures (`cow_for_bare_vector_control`,
/// `cow_for_bare_resource_elem_materialize`) ARE in the subset — ggdef
/// adjudicates them against their `run_gg` expectation (`1`).
const EXCLUDE: &[&str] = &[
    "deadwrite_ok_atomic_add.gg",
    "cow_value_index_field_writethrough.gg",
    "cow_dict_index_field_writethrough.gg",
    "cow_dict_index_field_single_eval.gg",
    // Dict get-chain write-place (`.get(k).unwrap().field`), the sibling of the
    // two rows above; landed with the Face-A fix (0f12b5cc) without refreshing
    // either gate. Two ggdef-run-verified phase-0 gates: the `{}` empty-Dict
    // literal has no `elaborate_expr` arm (catch-all, elaborate/mod.rs:1659), and
    // past that the get-chain write place is Vector-only by design ("a Dict
    // entry-by-key is not expressible as a `Proj`", eval.rs:805-812) so `as_index`
    // IllFormeds on the String key (eval.rs:818). See corpus_b.rs for the long form.
    "cow_getref_dict_writethrough.gg",
    "cow_for_amp_vector_field_writethrough.gg",
    "cow_for_amp_vector_alias_root.gg",
    "cow_for_amp_resource_elem_writethrough.gg",
    // bare `.enumerate()`: `binding_name(pattern)?` (elaborate/mod.rs:~969)
    // rejects the enumerate 2-tuple ("only simple bindings…"; ggdef-run-verified).
    "cow_for_enumerate_bare_resource_materialize.gg",
    // 1A remediation: statement-`&` enumerate (`for i, x in &a.enumerate()`)
    // hits the `for &` B2 gate FIRST (elaborate/mod.rs:~967 "`for &`/`for !`
    // iteration is Increment B2" — the ownership check precedes `binding_name`;
    // ggdef-run-verified). Expected output (`101`) is §3.1-prose-derived.
    "cow_for_enumerate_amp_writethrough.gg",
    // 1A remediation-2: the alias-root sibling — same statement-`&` shape,
    // same :~967 B2 gate (ggdef-run-verified). Expected `1`/`101` prose-derived.
    "cow_for_enumerate_amp_alias_root.gg",
    // 1A remediation-2: the RECEIVER-wrap spelling `(&a).enumerate()` carries
    // the `&` inside the expression, so statement ownership is Borrow and the
    // :~967 gate does NOT fire — it falls to `binding_name` (:~969, "only
    // simple bindings…"; ggdef-run-verified). Expected `101` prose-derived.
    "cow_for_enumerate_amp_recv_wrap.gg",
    "cow_comprehension_amp_source.gg",
    // CoW-2G loop fixtures (db25f0ef) outside the phase-0 subset — `for … else:`
    // (no else-body arm in `desugar_for`) and `.push_char()` (a B2 builtin
    // method); both `known_gaps`-flagged in their own frontmatter, ggdef cannot
    // elaborate them. The pin was not refreshed when they landed (leaving b1 red
    // on the STOP-and-report), so this exclusion accompanies the D31 slice.
    "cow_loop_bare_param_for_else.gg",
    "cow_loop_bare_param_push_char.gg",
    // NOTE: the 2T get-chain fixtures (`cow_taint_getchain_*`) all carry `equip R
    // with Drop`, so `gate_fixtures`'s `equip ` filter already routes them to B2
    // (corpus_b) — they are NOT in the B1 gate set and need no B1 exclusion.
];

fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

use ggdef::parse_rust_str_lit;


#[derive(PartialEq, Debug)]
enum Provenance {
    RunGg,
    SelfHost,
    /// A NEGATIVE expectation: `check_gg_fails(fixture, "error[E_…]")`. The
    /// fixture must be REJECTED at the frontend with that error code (D10(a):
    /// `cow_amp_bind_ref{,_field}`). Carries the code, not a stdout string.
    CheckFails,
    ReportOnly,
}

/// Whether the `"fixture.gg"` literal at `pos` is the first argument of a
/// `<fn>(...)` call whose name is exactly `needle_fn` (the previous
/// non-whitespace text ends with `<needle_fn>(`, allowing the multi-line form).
fn preceded_by_call(integ: &str, pos: usize, needle_fn: &str) -> bool {
    let start = pos.saturating_sub(64);
    integ[start..pos].trim_end().ends_with(&format!("{needle_fn}("))
}

/// The committed expectation for `fixture` (e.g. `cow_x.gg`) and how it was
/// sourced. `RunGg` reads the 2nd string of the `run_gg(...)` call; `CheckFails`
/// reads the error code of a `check_gg_fails(...)` rejection; `SelfHost` reads
/// the `assert_eq!(stdout, ...)` inside the fixture's self-host test.
fn expected_for(integ: &str, fixture: &str) -> (Provenance, Option<String>) {
    let needle = format!("\"{fixture}\"");
    let mut search = 0;
    while let Some(rel) = integ[search..].find(&needle) {
        let pos = search + rel;
        let prov = if preceded_by_call(integ, pos, "run_gg") {
            Some(Provenance::RunGg)
        } else if preceded_by_call(integ, pos, "check_gg_fails") {
            Some(Provenance::CheckFails)
        } else {
            None
        };
        if let Some(prov) = prov {
            let after = pos + needle.len();
            if let Some(q) = integ[after..].find('"') {
                if let Some(exp) = parse_rust_str_lit(&integ[after + q..]) {
                    return (prov, Some(exp));
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

/// Discover the B1 gate set: every `cow_*` / `deadwrite_*` fixture WITHOUT an
/// `equip` block, minus the standing exclusions.
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
            let src = fs::read_to_string(dir.join(&n)).unwrap();
            if src.contains("equip ") {
                return None; // equip fixtures are Increment B2
            }
            Some(n)
        })
        .collect();
    names.sort();
    names
}

#[test]
fn corpus_b1_all_match() {
    let root = ws_root();
    let integ = fs::read_to_string(root.join("tests/integration.rs")).expect("read integration.rs");
    let fixtures = gate_fixtures(&root);

    let mut table = String::from("\n=== corpus_b1 table ===\n");
    let mut matched = 0usize;
    let mut report_only: Vec<String> = Vec::new();
    let mut failures: Vec<String> = Vec::new();

    for fixture in &fixtures {
        let src = fs::read_to_string(root.join("tests/fixtures").join(fixture)).unwrap();
        let (prov, expected) = expected_for(&integ, fixture);
        let result = ggdef::run_source(&src, ggdef::DEFAULT_FUEL);

        // A NEGATIVE expectation (`check_gg_fails(..., "error[E_…]")`): the
        // fixture must be REJECTED at the frontend carrying that code — a run to
        // a value is a MISMATCH. Handled before the unwrap so the STOP-and-report
        // panic stays reserved for an UNEXPECTED frontend error.
        if prov == Provenance::CheckFails {
            let exp = expected.expect("CheckFails carries the error code");
            let exp = exp.trim();
            let (ok, detail) = match &result {
                Err(e) => {
                    let msg = e.to_string();
                    (msg.contains(exp), msg)
                }
                Ok(run) => (false, format!("ran to {:?}; stdout {:?}", run.outcome, run.stdout)),
            };
            table.push_str(&format!("  {:<15} {}\n", if ok { "MATCH/check_fails" } else { "MISMATCH" }, fixture));
            if ok {
                matched += 1;
            } else {
                failures.push(format!("  {fixture}: expected rejection {exp:?}, got: {detail}"));
            }
            continue;
        }

        let run = result
            .unwrap_or_else(|e| panic!("{fixture}: frontend error (STOP-and-report): {e}"));
        let got = run.stdout.clone();

        match expected {
            Some(exp) => {
                let ok = got.trim() == exp.trim();
                let tag = match prov {
                    Provenance::RunGg => "MATCH/run_gg",
                    Provenance::SelfHost => "MATCH/selfhost",
                    Provenance::CheckFails | Provenance::ReportOnly => unreachable!(),
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
                    "  {:<15} {:<44} => {:?}\n",
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
        "corpus_b1: {} issue(s) — triage against the divergence table before touching eval.rs:\n{}",
        failures.len(),
        failures.join("\n")
    );

    // Guard the gate's shape: the B1 non-equip surface. +2 (2026-07-21, SH-CoW
    // Face-A round): `cow_compound_getref_writethrough` + `cow_getref_writethrough_resource`
    // both landed top-level and match (in-subset), refreshing 122→124. History:
    // +15 net from the CoW-2G landings (2026-07-18) refreshed with the D31
    // exclusion additions; prior 107 counted the earlier surface.
    assert_eq!(fixtures.len(), 124, "B1 gate set drifted from 124 fixtures");
}
