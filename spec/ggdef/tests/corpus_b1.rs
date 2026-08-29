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
    // R45 memory-safety regression net — see the identical block in
    // corpus_b.rs. Both gates share the phase-0 subset, so an exclusion in one
    // without the other is a red in the sibling (Core #4: fix the class).
    // ggdef is structurally blind to memory-invalidation (Core #13); these
    // fixtures are adjudicated by C, LLVM and ASan.
    "cow_rescue_mutation_in_container_literal.gg",
    "cow_rescue_mutation_in_control_flow_expr.gg",
    "cow_rescue_mutation_in_operand_position.gg",
    "cow_rescue_mutation_through_getchain_receiver.gg",
    "cow_rescue_mutation_inside_assert.gg",
    "liveness_use_inside_loop.gg",
    "liveness_on_error_in_match_arm_do.gg",
    "liveness_on_error_inside_do_block.gg",
    "liveness_on_error_seed_kill_leak.gg",
    "liveness_on_error_nested_registration.gg",
    "liveness_for_zero_iteration_kill.gg",
    "liveness_loop_break_skips_kill.gg",
    "liveness_on_error_after_registration.gg",
    "liveness_loop_body_kill_leak.gg",
    "liveness_metafor_match_arm.gg",
    "liveness_on_error_assign_kill.gg",
    "liveness_on_error_vardecl_kill.gg",
    "liveness_use_inside_unsafe_scope.gg",
    "liveness_use_inside_with_block.gg",
    "liveness_use_inside_assert_return.gg",
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
    // Family-1 (`&<projection>` call args borrow THE PLACE, 2026-07-27) — the
    // same four rows EXCLUDEd from corpus_b, same phase-0 gates, each
    // ggdef-run-verified: `Box(...)` is an unresolved callee (Increment B2);
    // `if x is Some(v):` and a list comprehension both hit the `expression
    // \`unsupported\`` catch-all; `static Holder G` is "item kind static". The
    // ADJUDICABLE core of the same claim is
    // `cow_amp_projection_resource_value_split.gg`, written inside the subset on
    // purpose and IN this gate — see corpus_b.rs for the long form.
    "cow_amp_deref_box_projection.gg",
    "cow_amp_projection_type_axis.gg",
    "cow_amp_projection_base_shapes.gg",
    "cow_comprehension_amp_projection_source.gg",
    // Family-1 fix round — both pin ERROR-PROPAGATION semantics, which phase 0
    // does not model: `catch` is "expression `unsupported`", and an IIFE /
    // closure-variable call is "only named callees are supported in phase 0".
    // Both ggdef-run-verified; see corpus_b.rs for the long form.
    "cow_amp_projection_autoprop_arg.gg",
    "cow_amp_projection_indirect_call_arg.gg",
    // `Box` / `Mutex` / `Guard` objects + `catch` — "unresolved callee `Box`".
    "cow_amp_projection_autoprop_objects.gg",
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
    // R44 Track A GRADUATIONS of the CoW-2G comprehension cells — both are
    // COMPREHENSIONS, the same out-of-subset class as
    // `cow_comprehension_amp_source.gg` above (`elaborate_expr` has no
    // comprehension arm). Measured: b1 STOP-and-reported on the first of them
    // the moment it graduated into the top-level corpus. The subset gap is
    // already filed; do NOT re-file it.
    "cow_loop_bare_param_comprehension.gg",
    "cow_loop_bare_param_comprehension_matrix.gg",
    // NOTE: the 2T get-chain fixtures (`cow_taint_getchain_*`) all carry `equip R
    // with Drop`, so `gate_fixtures`'s `equip ` filter already routes them to B2
    // (corpus_b) — they are NOT in the B1 gate set and need no B1 exclusion.
    // R47 Track A1: the `.first()` producer-arm cell of the collection-identity
    // fix (`todo/t0703`). The builtin arm-picker has a `get` arm but none for
    // `first`/`last`, so this reaches the catch-all "method `.first()` is
    // outside the phase-0 subset". Excluded on that construct, not on the shape
    // under test — the spelling IS the thing under test, so reshaping it to
    // `.get(0)` would delete the cell. Same row and full rationale in
    // `corpus_b.rs`; both gates share the phase-0 subset. Subset gap filed as
    // `todo/t0753` — do NOT re-file it.
    "cow_alias_spelled_view_via_first_getter.gg",
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
    // +2 (2026-07-27, Family-1 round): `cow_amp_index_vs_getchain_differential`
    // + `cow_amp_projection_resource_value_split`, both top-level, both
    // in-subset, both MATCH — the two rows the oracle adjudicates for that
    // round (the other four are EXCLUDEd above with per-row citations).
    // +1 (same round): `cow_amp_ref_field_forward`, in-subset, MATCH (3/4/3) —
    // the already-a-pointer FIELD cell where the producer must decline.
    // +2 (2026-08-28, R45 Track G rework): `cow_assign_target_chain` +
    // `cow_assign_target_named_control` — the assignment-target sibling of the
    // mutation-path root peel, top-level, in-subset, both MATCH with no ggdef
    // change. The repro half was a live WRONG ANSWER on the self-host lane
    // before the fix, which is the class ggdef CAN adjudicate (value semantics),
    // unlike its realloc-UAF sibling.
    // +8 −1 (2026-08-29, R47 Track A1): the collection-identity fix
    // (`todo/t0703`) landed 8 top-level `cow_alias_*` / `cow_indexed_*` /
    // `cow_view_into_*` fixtures; 7 are in-subset and adjudicated here, the
    // 8th is EXCLUDEd above on `.first()` (`todo/t0753`) → 137.
    // ⚠ The 130 was never reached while this gate was RED on that elaboration
    // error, so it had not absorbed the 8; the arithmetic starts from 130.
    assert_eq!(fixtures.len(), 137, "B1 gate set drifted from 137 fixtures");
}
