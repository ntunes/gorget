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
    // R45 memory-safety regression net. These fixtures pin the CoW rescue
    // against realloc-induced use-after-free — the class ggdef is
    // STRUCTURALLY BLIND to (it adjudicates value semantics, not memory
    // validity; AGENTS.md Core #13). Their adjudicating lanes are C, LLVM and
    // ASan, all green. Two carry a DICT LITERAL with a call in key position
    // (`{grow(&v): 1}`), which hits the catch-all "expression `unsupported` is
    // outside the phase-0 subset"; excluded on that construct, not on the
    // shape under test.
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
    "cow_element_borrow_alias_mutate.gg",
    "cow_p3_alias_chain_mutate.gg",
    "cow_p3_index_mutate.gg",
    // Module statics are outside the phase-0 subset (`Item::StaticDecl` not
    // elaborated — "item kind static"): the CoW-1B fixture mixes static and
    // local shapes; its LOCAL half was ggdef-adjudicated at authoring (88/30/45,
    // see the fixture comment), the static half is prose-derived.
    "cow_value_index_field_writethrough.gg",
    // Dict index write-place outside the phase-0 subset — `navigate_write`
    // (eval.rs:923) has arms for Struct/Vector/Tuple/Enum only, so a Map value
    // + Index write-projection `IllFormed`s. The CoW-1C fixture's expected
    // output is §3.1-prose-derived (owned Dict place writes through: 99/41/99).
    "cow_dict_index_field_writethrough.gg",
    // Same Dict index write-place (a `make()[0].x = 99` through a side-effecting
    // producer) — the CoW-1C double-eval regression fixture is likewise outside
    // the phase-0 subset; it pins EVAL-ORDER on the real compilers, not ggdef.
    "cow_dict_index_field_single_eval.gg",
    // Dict get-chain write-place, the `.get(k).unwrap().field` sibling of the two
    // rows above (landed with the Face-A fix, 0f12b5cc). TWO phase-0 gates fire,
    // both ggdef-run-verified: (1) the `{}` empty-Dict literal is
    // `Expr::DictLiteral` (src/parser/expr.rs:1784) and `elaborate_expr` has no
    // arm for it → the catch-all "expression `unsupported`" (elaborate/mod.rs:1659);
    // (2) past that (rewriting `{}` to `Dict[String, Blk]()`), the get-chain write
    // place producer is Vector-only BY DESIGN — "a Dict entry-by-key is not
    // expressible as a `Proj`" (eval.rs:805-812) — so `as_index` on the String key
    // IllFormeds with "index must be int, got String" (eval.rs:818). The fixture's
    // expected output is the Rust/self-host lane's, not ggdef-adjudicated.
    "cow_getref_dict_writethrough.gg",
    // The three D2 plain-self/`&`-self trait-equip fixtures (landed 63cf298f +
    // 835f8875 on 2026-07-21, same round as the row above, likewise without a
    // gate refresh). `elaborate` has no `Item::Trait` arm → "item kind trait is
    // outside the phase-0 subset" (elaborate/mod.rs:126); trait dispatch is a
    // later increment, the same class as the `static` exclusion above. Their
    // expected outputs are the Rust + self-host lanes', not ggdef-adjudicated.
    "cow_trait_amp_self_field_writethrough.gg",
    "cow_trait_plain_self_collection_materialize.gg",
    "cow_trait_plain_self_field_materialize.gg",
    // Track 1A (+ remediations): the out-of-phase-0-subset for-element fixtures
    // (the same rows EXCLUDEd from corpus_b1 — both gates share the phase-0
    // elaboration; each citation is ggdef-run-verified).
    //   - for `x in &coll`: `desugar_for` returns the "`for &`/`for !` iteration
    //     is Increment B2" error (elaborate/mod.rs:~967).
    "cow_for_amp_vector_field_writethrough.gg",
    "cow_for_amp_vector_alias_root.gg",
    "cow_for_amp_resource_elem_writethrough.gg",
    //   - bare `.enumerate()`: `desugar_for`'s `binding_name(pattern)?`
    //     (elaborate/mod.rs:~969) rejects the enumerate 2-tuple pattern.
    "cow_for_enumerate_bare_resource_materialize.gg",
    //   - statement-`&` enumerate (`for i, x in &a.enumerate()` + the alias-root
    //     sibling): the :~967 `for &` B2 gate fires FIRST (the ownership check
    //     precedes `binding_name`). Expected outputs §3.1-prose-derived.
    "cow_for_enumerate_amp_writethrough.gg",
    "cow_for_enumerate_amp_alias_root.gg",
    //   - RECEIVER-wrap `(&a).enumerate()`: statement ownership is Borrow (the
    //     `&` sits inside the expression), so :~967 does NOT fire — it falls to
    //     `binding_name` (:~969, "only simple bindings…").
    "cow_for_enumerate_amp_recv_wrap.gg",
    // Family-1 (`&<projection>` call args borrow THE PLACE). Four of the round's
    // fixtures sit outside the phase-0 subset; each citation below is
    // ggdef-run-verified, and the ADJUDICABLE core of the same claim is the
    // fixture `cow_amp_projection_resource_value_split.gg`, which is deliberately
    // written inside the subset (no generics / `is`-binding / static / Box /
    // comprehension) and which ggdef DOES adjudicate: it prints
    // 110/a!/110/a!/110/a!, agreeing with production post-fix and disagreeing
    // with it pre-fix on exactly the three by-value cells. So this family is
    // oracle-covered, not merely excluded.
    //   - `Box(...)`: "unresolved callee `Box` (unknown function/struct/enum;
    //     may need Increment B2)".
    "cow_amp_deref_box_projection.gg",
    //   - `if h.oi is Some(v):` — the `is`-pattern binding elaborates to the
    //     catch-all "expression `unsupported` is outside the phase-0 subset".
    //     (The fixture also carries a generic struct field, phase 1.)
    "cow_amp_projection_type_axis.gg",
    //   - module-level `static Holder G` — "item kind static is outside the
    //     phase-0 subset", the same class as the `cow_value_index_field_*`
    //     exclusion above.
    "cow_amp_projection_base_shapes.gg",
    //   - list comprehension — "expression `unsupported` is outside the phase-0
    //     subset" (no comprehension arm in `elaborate_expr`), the same class as
    //     the comprehension row noted in the for-element block above.
    "cow_comprehension_amp_projection_source.gg",
    // Family-1 FIX ROUND (auto-propagate interaction with the chokepoint). Both
    // rows are ggdef-run-verified out-of-subset; both pin ERROR-PROPAGATION
    // semantics, which phase 0 does not model at all.
    //   - `expr! catch (e): …` — "expression `unsupported` is outside the
    //     phase-0 subset".
    "cow_amp_projection_autoprop_arg.gg",
    //   - an IIFE / closure-variable call — "only named callees are supported in
    //     phase 0".
    "cow_amp_projection_indirect_call_arg.gg",
    //   - `Box(...)` + `Mutex`/`Guard` objects, and `catch` — "unresolved callee
    //     `Box`". Pins the OBJECT domain of the auto-propagate pre-check, which
    //     is error-propagation semantics phase 0 does not model either.
    "cow_amp_projection_autoprop_objects.gg",
    //   - list comprehension: `elaborate_expr` has no comprehension arm, so the
    //     `[x*2 for x in &a]` expression is "outside the phase-0 subset".
    "cow_comprehension_amp_source.gg",
    // CoW-2G loop fixtures (added in db25f0ef) whose SHAPES are outside the
    // phase-0 subset — the expected outputs are §3.1-prose/Rust-lane-derived
    // (both are `known_gaps`-flagged in their own frontmatter), and ggdef cannot
    // elaborate them, so they belong with the other out-of-subset exclusions:
    //   - `for … else:` — `desugar_for` has no else-body arm ("`for ... else`
    //     is outside the phase-0 subset").
    "cow_loop_bare_param_for_else.gg",
    //   - `.push_char()` — outside the phase-0 builtin-method set (needs
    //     Increment B2, like the other B2-gated methods).
    "cow_loop_bare_param_push_char.gg",
    // R44 Track A GRADUATIONS of the CoW-2G comprehension cells. Both are
    // COMPREHENSIONS, and `elaborate_expr` has no comprehension arm, so ggdef
    // reports "expression `unsupported` is outside the phase-0 subset" — the
    // SAME class and the SAME citation as `cow_comprehension_amp_source.gg`
    // above. Measured, not assumed: `cargo test -p ggdef` STOP-and-reported on
    // `cow_loop_bare_param_comprehension.gg` the moment it graduated into the
    // top-level corpus. The subset gap is already filed under
    // ggdef / define-gorget ("Out of the ggdef phase-0 subset (no
    // comprehension arm)") — do NOT re-file it.
    //   - `[drain_one(&xs) for i in 0..2]` — the list x BODY channel cell.
    "cow_loop_bare_param_comprehension.gg",
    //   - the other six cells of the emitter x channel matrix (set / dict-key /
    //     dict-value body, plus the three filter cells).
    "cow_loop_bare_param_comprehension_matrix.gg",
    // 2T get-chain PRECISION guard: the write target is a USER-method-call
    // rvalue's field (`h.coll.get(0).name = v`). ggdef's frontend correctly
    // ACCEPTS it (no over-reject — the get-chain descent is Vector-kind-gated, so
    // a user `get` returning an owned temp is not descended; pinned by the unit
    // test `d4_position_6_user_get_chain_not_over_rejected`), but eval's
    // `navigate_write`/`ast_place` treats a MethodCall rvalue as "not a place" →
    // `IllFormed` — writing to a method-call temp's field is outside the phase-0
    // write-place subset (same class as the Dict/for-element exclusions). The
    // fixture's RUN role is the Rust-lane precision guard; ggdef precision is the
    // unit test above, not this run-diff.
    "cow_taint_getchain_user_get_ok.gg",
    // Round XIV — combinator-receiver ownership class-fix (74f566c6). The
    // `combinator_*` family joins the gate set (spec/ggdef/src/ggc.rs +
    // elaborate/mod.rs + eval.rs extended with the 7 Increment-B3 combinators:
    // Map/Filter/OrElse/AndThen/FlatMap/UnwrapOrElse/MapErr). The fixtures
    // below are OUTSIDE the phase-0 subset for reasons ggdef-run-verified:
    // each uses the `if x is Some(v):` binding construct — the `is`-pattern
    // is not in `elaborate_expr` and falls through to the catch-all
    // "expression `unsupported` is outside the phase-0 subset"
    // (elaborate/mod.rs:1665, same class as the row above at
    // `cow_amp_projection_type_axis.gg`). The class-fix's ADJUDICABLE
    // in-subset cells (unwrap_or_else on a resource payload, elaborable
    // without an `is`-binding readback) DO run through the extended eval
    // rules and match; those and the pre-existing `combinator_unwrap_or_else_str.gg`
    // are the +N ratchet contributions.
    "combinator_map_money_param_and_field.gg",
    "combinator_or_else_money_field.gg",
    "combinator_and_then_money_local.gg",
    "combinator_and_then_money_param.gg",
    "combinator_and_then_money_field.gg",
    "combinator_filter_money_param.gg",
    "combinator_filter_money_field.gg",
    "combinator_or_else_money_param.gg",
    "combinator_flat_map_money_local.gg",
    "combinator_flat_map_money_param.gg",
    "combinator_flat_map_money_field.gg",
    "combinator_map_err_money_local.gg",
    "combinator_map_err_money_param.gg",
    "combinator_result_ok_money_map.gg",
    "combinator_chain_map_filter.gg",
    "combinator_unwrap_or_else_money_field.gg",
    // The pre-existing `combinator_unwrap_or_else_str.gg` uses the top-level
    // `len(x)` free-function call — ggdef only knows `x.len()` methods,
    // producing "unresolved callee `len`" (elaborate/mod.rs:~680, same class
    // as an unknown callee). Not blocked by the class-fix, just outside the
    // phase-0 subset for that separate reason.
    "combinator_unwrap_or_else_str.gg",
    // Round XV Track B — string-coercion bail retired; new combinator_string*
    // / combinator_*_string_* axis fixtures. ggdef phase-0 has no String
    // methods / GorgetString surface for these programs (elaborates
    // "expression `unsupported` is outside the phase-0 subset"). Scout
    // confirmed out-of-subset; no ggdef flip. EXCLUDE with citation (Core #9
    // note: in-subset Money combinators remain the adjudicated surface).
    "combinator_map_string_to_int_param.gg",
    "combinator_map_string_to_int_field.gg",
    "combinator_map_string_to_int_local.gg",
    "combinator_map_string_to_string_param.gg",
    "combinator_and_then_string_to_int_param.gg",
    "combinator_flat_map_string_to_int_param.gg",
    "combinator_result_ok_string_map_to_int_param.gg",
    "combinator_result_ok_string_and_then_to_int_param.gg",
    // Round XXII Track γ — Result combinator field passthrough double-free
    // class-fix (4 arms): map/map_err/and_then/or_else on a heap-Money passthrough
    // field now clone via `combinator_owned_copy_stmt` at
    // `tests/fixtures/self_host_lowerer/lir_codegen.gg:5363/5374/5386/5396` (Core #4
    // chokepoint; SH-only fix — Rust C+LLVM already correct via scrut-CLONE +
    // field-MOVE). The 4 fixtures below read the passthrough branch via
    // `if mapped is Ok(m):` / `is Error(m):` — the `is`-pattern binding readback
    // is not in `elaborate_expr` (same class as the 13 `combinator_*_money_*.gg`
    // and `combinator_result_ok_money_map.gg` entries above), so ggdef falls
    // through to the catch-all "expression `unsupported` is outside the phase-0
    // subset". Class routing is via typed metadata (`combinator_field_lir_type` +
    // `field_drop_fn_for_lir_type` → `drop_to_clone_fn`), not name-matching.
    // MATCH ratchet in `self_host_runtime_diff` covers these; the corpus_b gate
    // stays shape-stable via EXCLUDE + the +0 net delta to the fixture count.
    "combinator_result_map_money_passthrough.gg",
    "combinator_result_map_err_money_passthrough.gg",
    "combinator_result_and_then_money_passthrough.gg",
    "combinator_result_or_else_money_passthrough.gg",
    // Round XXIII Track α positive-runtime fixture pinning the cross-type
    // or_else fix (was SBO pre-fix; now GREEN on C+LLVM). Out of phase-0
    // subset per elaborator ("expression `unsupported` is outside phase 0").
    // Same disposition as the Money-passthrough sibs above.
    "combinator_result_or_else_error_cross_type.gg",
    // Round XXIV Track C — SH-lane companion for cross-type or_else fix
    // (STRENGTHEN-B graduated from known_gaps/sound_sh_or_else_result_cross_type_sbo.gg
    // to `combinator_result_or_else_error_axis_sbo.gg`). Reads the Error branch
    // via `if r is Error(be):` — same `is`-pattern binding readback the
    // elaborator has no arm for ("expression `unsupported` is outside the
    // phase-0 subset"). Same disposition as the Round XXIII α sibs above.
    // The SH-lane oracle for this fixture is `self_host_runtime_diff` (MATCH
    // ratchet), not corpus_b.
    "combinator_result_or_else_error_axis_sbo.gg",
    // Round XXV Track B — one-sided-combinator class-fix (see the four
    // other `combinator_*_rejected.gg` sibling fixtures which corpus_b DOES
    // adjudicate as CheckFails). Result.flatten reaches the arm-picker's
    // `other =>` catch-all in ggdef ("method `.flatten()` is outside the
    // phase-0 subset") — a DIFFERENT reject path from the {flat_map, filter,
    // map_err, unwrap_error} siblings that hit the receiver-gate. The
    // catch-all's message will drift once Rust ships the paired class-fix
    // with different lane phrasing; the sibling fixtures adjudicate the
    // class. EXCLUDE with citation.
    "combinator_result_flatten_rejected.gg",
    // R47 Track A2, 2026-08-29: the `meta for`-GENERATED MATCH ARM cell of the
    // typed per-receiver CoW mutation classifier (`todo/t0699`). ggdef rejects
    // the SHAPE, not the program: `elaborate/mod.rs` raises "`meta for` match
    // arms are phase 2" for any match whose arm list holds a generator. The
    // fixture's own defect (a user `&self` mutator reached only through the
    // expansion; rc 139 on both backends pre-fix) is adjudicated on C, LLVM and
    // the self-host lowerer lane instead. Subset gap filed as `todo/t0762` —
    // and it is not exotic: `lib/xtd/dataframe.gg`'s `Column` methods are all
    // written in this shape.
    "cow_user_mutator_meta_generated_arm.gg",
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
/// the `assert_eq!(stdout, …)` inside the fixture's self-host test.
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

/// Discover the B2 gate set: every `cow_*` / `deadwrite_*` fixture minus the
/// standing exclusions (equip fixtures INCLUDED, unlike B1).
fn gate_fixtures(root: &Path) -> Vec<String> {
    let dir = root.join("tests/fixtures");
    let mut names: Vec<String> = fs::read_dir(&dir)
        .expect("read tests/fixtures")
        .filter_map(|e| {
            let n = e.unwrap().file_name().into_string().unwrap();
            let is_corpus = (n.starts_with("cow_")
                || n.starts_with("deadwrite_")
                || n.starts_with("combinator_"))
                && n.ends_with(".gg");
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

    // Guard the gate's shape (a silent-drift tripwire, not a match check). +2
    // (2026-07-21, SH-CoW Face-A round): `cow_compound_getref_writethrough`
    // (promoted known_gaps→top-level) + `cow_getref_writethrough_resource` both
    // landed as top-level cow_* fixtures and both MATCH ggdef (in phase-0 subset),
    // so the pin refreshes 150→152 to accompany them. History: +15 net from the
    // CoW-2G landings (2026-07-18) refreshed with the D31 exclusion additions.
    // +5 (2026-07-21, tainted-reject 2T get-chain round): the five NEG get-chain
    // fixtures `cow_taint_getchain_{vector,firstlast,compound,receiver,formation}`
    // all MATCH ggdef as `check_fails` rejections (the ggdef both-lane pin); the
    // POS `cow_taint_getchain_user_get_ok` is EXCLUDEd (out-of-subset write-place).
    // +2 (2026-07-27, Family-1 round — `&<projection>` call args borrow THE
    // PLACE): `cow_amp_index_vs_getchain_differential` and
    // `cow_amp_projection_resource_value_split` both land as top-level cow_*
    // fixtures INSIDE the phase-0 subset and both MATCH ggdef. They are the
    // adjudicated core of that round: pre-fix, production disagreed with the
    // oracle on every by-value cell (`10` where ggdef said `110`) while agreeing
    // on every resource cell — the accidental-correctness split that hid the
    // class. The round's four out-of-subset fixtures are EXCLUDEd above with a
    // per-row citation, so this pin refreshes 157→159 to accompany the two the
    // oracle actually covers.
    // +1 (same round): `cow_amp_ref_field_forward`, also in-subset and MATCHing
    // (ggdef prints 3/4/3). It pins the already-a-pointer FIELD cell — where the
    // shared producer must DECLINE and let the `is_already_ptr` fall-through
    // forward the stored pointer — a regression introduced and caught inside the
    // round itself.
    // +2 (Round XIII, 2026-07-29): Tracks V + X added two cow_* corpus fixtures
    // (`cow_value_index_bare_mut_recv_writethrough` + `cow_value_index_nested_mut_recv_writethrough`)
    // that ggdef adjudicates in-subset. Both are new-in-round MATCHes.
    // +2 (Round XIV, 2026-07-29): Increment B3 combinator subset extension —
    // ggdef gains 7 Option/Result combinator arms (Map/Filter/OrElse/AndThen/
    // FlatMap/UnwrapOrElse/MapErr) at ggc.rs + elaborate/mod.rs + eval.rs, and
    // the corpus_b filter accepts `combinator_*` fixtures. Two of Round XIV's
    // new fixtures — `combinator_unwrap_or_else_money_local.gg` and
    // `combinator_unwrap_or_else_money_param.gg` — are in-subset (no
    // `is`-pattern binding readback; unwrap_or_else's return IS the payload,
    // so `print(size_of(r))` alone reads it) and both MATCH ggdef post-fix.
    // The other 14 combinator_* fixtures land in EXCLUDE above with per-row
    // citations: 13 use `if x is Some(v):` (the `is`-pattern is not in
    // elaborate_expr) and the pre-existing `combinator_unwrap_or_else_str.gg`
    // uses a free-function `len(x)` call. So the ratchet refreshes 162→164
    // to accompany the two the oracle actually adjudicates.
    // Round XXIV Track C, 2026-08-01: SH-lane port of Rust XXIII α's cross-type
    // or_else fix graduated `known_gaps/sound_sh_or_else_result_cross_type_sbo.gg` to
    // top-level `combinator_result_or_else_error_axis_sbo.gg` (STRENGTHEN-B: pins the
    // Error-axis SBO cell — RED-verified `1` on SH pre-fix from a truncated
    // `.cents.len()`, `5` post-fix matching Rust). Landed EXCLUDEd above with citation
    // (`is Error(be):` binding readback is out of phase-0 subset), so the shape count
    // is UNCHANGED — the fixture is enumerated then filtered out by EXCLUDE.
    // Round XXIV Track D, 2026-08-01: ggdef mirror of XXIII α's
    // `unify_closure_ret_axis` landed — 3 NEG cross-type reject fixtures
    // (`combinator_result_or_else_ok_cross_type_reject.gg`,
    // `combinator_result_and_then_error_cross_type_reject.gg`,
    // `combinator_option_or_else_cross_type_reject.gg`) removed from EXCLUDE
    // above; they now REJECT with `error[E_TypeMismatch]` at the elaborator
    // and adjudicate via `Provenance::CheckFails` in corpus_b. Count +3 → 168.
    // Round XXV Track B, 2026-08-02: one-sided combinator receiver-gate
    // reject landed at `spec/ggdef/src/elaborate/mod.rs` — 5 new NEG
    // fixtures (`combinator_result_{flat_map,filter,flatten}_rejected.gg`
    // + `combinator_option_{map_err,unwrap_error}_rejected.gg`). Four of
    // them adjudicate via `CheckFails` (the receiver-gate emits
    // `error[E_NoMethodFound]:`); the fifth (`_flatten_rejected`) is
    // EXCLUDEd above (Flatten reaches the arm-picker catch-all — a
    // different reject path with a drift-prone message). Count +5 additions
    // − 1 EXCLUDE = +4 net → 172.
    //
    // Round XXVIII Track A + follow-up 2026-08-02: 4 tag-check NEG fixtures
    // added (`combinator_{result_is_some,result_is_none,option_is_ok,
    // option_is_error}_rejected.gg`). ggdef LAG closed in-round via the
    // string-based receiver-gate at `elaborate/mod.rs` (intercepts BEFORE
    // the arm-picker catch-all with `error[E_NoMethodFound]:` matching
    // Rust+SH lanes). All 4 adjudicate via CheckFails. Count +4 → 176.
    //
    // R45 Track G (rework), 2026-08-28: 2 CoW write-walker fixtures added
    // (`cow_assign_target_chain.gg` + `cow_assign_target_named_control.gg`),
    // the assignment-target sibling of the mutation-path root peel. Both
    // adjudicate MATCH via `run_gg` with NO ggdef change — the shape is
    // already in the phase-0 subset, and ggdef agrees with the fixed lanes.
    // Worth recording WHY that is a real result and not a formality
    // (Core #13): this defect was a WRONG ANSWER, not a memory-invalidation
    // one, so it sits squarely in what ggdef can adjudicate — unlike its
    // realloc-UAF sibling (`cow_rescue_mutation_through_getchain_receiver`),
    // which ggdef would accept cleanly. Count +2 → 178.
    //
    // R47 Track A2, 2026-08-29: 4 CoW fixtures added for the typed
    // per-receiver mutation classifier (`todo/t0699` — a user method's NAME
    // decided memory safety). THREE adjudicate MATCH via `run_gg` with no
    // ggdef change, and one of them is the round's most load-bearing ggdef
    // result rather than a formality (Core #13):
    // `cow_user_mutator_two_types_same_name` adjudicates `4 4 4 / 3 2 4`,
    // which is what the FIXED lanes print — the pre-fix compilers agreed on
    // `3 3` and were both wrong, so the definition, not their agreement, is
    // what says the shipped answer is right.
    // ⚠ The other two (`rename_invariance`, `fstring_interpolation`) are
    // memory-invalidation defects: ggdef adjudicates their VALUES and would
    // have accepted the pre-fix UAF cleanly, so their real adjudicator is
    // ASan (`tests/fixtures/security/sound_user_mutator_name_invariant_uaf.gg`).
    // The fourth, `cow_user_mutator_meta_generated_arm.gg`, is EXCLUDEd above
    // with citation (out of subset; `todo/t0762`). Count +4 additions
    // − 1 EXCLUDE = +3 net → 181.
    assert_eq!(fixtures.len(), 181, "B2 gate set drifted from 181 fixtures");
}
