//! Increment-A conformance evidence (deliverable 7).
//!
//! Runs `ggdef` over a hardcoded list of Increment-A-compatible `cow_*`
//! fixtures and compares its stdout to the **committed** expectation, extracted
//! from `tests/integration.rs`'s literal `run_gg(...)` pairs — parsed from the
//! Rust string literal (incl. the `\`-continuation multi-line form), never
//! retyped (per the brief).
//!
//! Acceptance A: every listed fixture MATCHes. A mismatch is NOT silently
//! "fixed" by patching `eval.rs`; it is triaged against the brief's divergence
//! decision table (D2 self-write-through / D1-EMove / the two smith bugs →
//! EXPECTED; anything else → a STOP-and-report finding).
//!
//! ## Why each fixture qualifies for Increment A
//!
//! Every fixture below uses ONLY the A subset: int/bool scalars, `String`,
//! `Vector`, structs, tuples; bare/`&`/`!` modes; `push`/`set`/`len`/index/
//! field access; list literals; `for`/`while`/`if`; `print`. NONE use
//! Option/`.unwrap()`/`.get()`, Dict/Set, `match`/enums, `equip`/`Drop`,
//! ranges/string-slices, closures, named-arg construction, `with`, or
//! float-printing (all Increment B/C). The A-clean pool was measured by
//! scanning every `cow_*` fixture for those B markers.
//!
//! The three RFC generic-equip cow exclusions (`cow_element_borrow_alias_mutate`,
//! `cow_p3_alias_chain_mutate`, `cow_p3_index_mutate`) are NOT in this list.

use std::fs;
use std::path::{Path, PathBuf};

/// `(fixture-basename, ratified-rule + why-A-clean)`. The rule tags reference
/// the `decisions.md` "RATIFIED-BY-FIXTURE" set (C1–C10).
const CORPUS_A: &[(&str, &str)] = &[
    // ── bare-assign copy + sever (C2) ──
    ("cow_struct_sever_on_mutation", "C2: bare-assign struct copy; first mutation severs the alias"),
    ("cow_struct_bare_assign", "C2: bare-assign of a non-Copy struct; bare/`!` params + returns"),
    ("cow_transitive_alias", "C2: x→y→z String/Vector alias chains; leaf mutation + source sever"),
    ("cow_flow_sensitive_alias", "C2: reassign-before-alias, then a bare alias (value-independent)"),
    ("cow_param_alias_reassign", "C2: bare Vector/String param alias, then alias mutated"),

    // ── `&` write-through of an owned/aliased root (C3) ──
    ("cow_amp_owned_writethrough", "C3: `&` of an owned root writes through (materialise is a no-op)"),
    ("cow_amp_bare_alias_arg", "C3+G2: `&` of a bare alias — source untouched, private copy grows"),
    ("cow_amp_bare_param_arg", "C3+G2: `&` inside a bare-param fn; caller untouched"),
    ("cow_amp_live_alias", "C3+G2: source read AFTER the `&b` mutation — both stay live"),
    ("cow_amp_bind_ref", "D10(a): standalone `auto r = &b` local `&`-bind — REJECTED (E_LocalBorrowBind)"),
    ("cow_amp_bind_ref_field", "D10(a): projected `auto r = &b.data` local `&`-bind — REJECTED (E_LocalBorrowBind)"),
    ("cow_amp_field_arg", "C3+G2: projected `&s.field` call-arg materialises the root"),
    ("cow_amp_index_side_effect_once", "C3: `&v[side()]` — index side effect runs exactly once"),

    // ── mutating method / assign through a bare param materialises (C4) ──
    ("cow_fieldpath_double_fire", "C4: field-path method — bare materialises once, `&` writes through"),
    ("cow_fieldpath_method_caller_untouched", "C4: `h.nums.push()` via bare param; caller untouched"),
    ("cow_fieldpath_mut_writethrough", "C4: `h.nums.push()` via `&` param; caller sees it"),
    ("cow_fieldpath_nested_assign_caller_untouched", "C4: nested field-path assign via bare param"),
    ("cow_index_proj_alias", "C4/G1a: two bare aliases; index-projected write privatises one"),
    ("cow_index_proj_rebind", "C4/G1a: bare-param index-proj write; in-fn read sees the copy"),
    ("cow_nested_field_mutation", "C4: element read from a struct's Vector field, then outer push"),

    // ── collection-element snapshots + nested index/field paths ──
    ("cow_collection_element_mutate", "C8: element snapshot preserved across a later push"),
    ("cow_multilevel_index_field_assign", "C4: 2-level nested index+field assign via bare param"),
    ("cow_rhs_same_coll_single", "C4: same-collection RHS read (`v[0]=v[1]`) then reallocating pushes"),

    // ── loops + for/while desugars ──
    ("cow_loop_borrow_propagation", "for-loop borrow propagation from bare params; read-only views"),
    ("cow_loop_body_local_move", "while + nested for; locals created in the loop body"),
    ("cow_field_of_for_element_read", "for over a Vector[struct]; read fields per element"),
];

/// Workspace root = two levels above this crate's manifest dir.
fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

/// The committed expected stdout for `fixture_file` (e.g. `cow_x.gg`), read
/// from the FIRST `run_gg(...)` pair that names it in `integration.rs`.
fn expected_for(integration_src: &str, fixture_file: &str) -> Option<String> {
    ggdef::extract_run_gg_expectation(integration_src, fixture_file)
        .or_else(|| {
            // Fallback: loose first-string-after-fixture-name scan (legacy).
            let needle = format!("\"{fixture_file}\"");
            let pos = integration_src.find(&needle)?;
            let after = pos + needle.len();
            let rest = &integration_src[after..];
            let open = rest.find('"')?;
            ggdef::parse_rust_str_lit(&rest[open..])
        })
}

#[test]
fn corpus_a_all_match() {
    let integ = fs::read_to_string(ws_root().join("tests/integration.rs"))
        .expect("read tests/integration.rs");

    let mut table = String::from("\n=== corpus_a MATCH table ===\n");
    let mut failures = Vec::new();

    for (name, why) in CORPUS_A {
        let fixture = format!("{name}.gg");
        let src = fs::read_to_string(ws_root().join("tests/fixtures").join(&fixture))
            .unwrap_or_else(|e| panic!("read fixture {fixture}: {e}"));
        let expected = expected_for(&integ, &fixture)
            .unwrap_or_else(|| panic!("no `run_gg`/`check_gg_fails` expectation for {fixture} in integration.rs"));
        let expected = expected.trim();

        let result = ggdef::run_source(&src, ggdef::DEFAULT_FUEL);

        // A NEGATIVE expectation (`check_gg_fails(..., "error[E_…]")`, D10(a)
        // for `cow_amp_bind_ref{,_field}`): the fixture must be REJECTED at the
        // frontend, and the diagnostic must carry the same `error[E_…]` code
        // production greps. A run to a value is a MISMATCH.
        let (matched, got) = if expected.starts_with("error[") {
            match &result {
                Err(e) => {
                    let msg = e.to_string();
                    (msg.contains(expected), msg)
                }
                Ok(run) => (
                    false,
                    format!("<ran to {:?}; stdout {:?}>", run.outcome, run.stdout),
                ),
            }
        } else {
            let run = result
                .as_ref()
                .unwrap_or_else(|e| panic!("{name}: frontend error: {e}"));
            (run.stdout.trim() == expected, run.stdout.trim().to_string())
        };

        table.push_str(&format!("  {:<9} {:<42} {}\n", if matched { "MATCH" } else { "MISMATCH" }, name, why));
        if !matched {
            failures.push(format!(
                "  {name}:\n    expected: {:?}\n    got:      {:?}",
                expected, got,
            ));
        }
    }

    table.push_str(&format!("  ({} fixtures)\n", CORPUS_A.len()));
    eprintln!("{table}");

    assert!(
        failures.is_empty(),
        "corpus_a: {} mismatch(es) — triage against the divergence table before touching eval.rs:\n{}",
        failures.len(),
        failures.join("\n"),
    );
}
