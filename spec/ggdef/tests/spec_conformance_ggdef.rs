//! The **ggdef conformance lane** (RFC §4) — the definitional interpreter
//! adjudicating the run-tier suite against itself.
//!
//! For every `spectests/run/*.gg` fixture: parse the frontmatter with the SHARED
//! reader (`ggdef::parse_frontmatter`), run the program through the definitional
//! interpreter (`ggdef::run_source`), and compare the observed `(exit, stdout)`
//! against the committed `expect:` block. Expectations flow FROM the definition
//! (they were written by `ggdef -- gen`), so this lane closes the loop: the
//! interpreter that WROTE the expectation must still REPRODUCE it.
//!
//! The per-fixture MATCH table is always printed (a diagnostic). Verdicts:
//!   * **MATCH**       — exit + stdout equal the committed `expect:`.
//!   * **MISMATCH**    — a Value/Trap/etc. outcome that disagrees. A committed
//!                       ggdef seed that ggdef itself no longer reproduces is a
//!                       genuine regression (or a stale, un-regenerated
//!                       `expect:` block) — never acceptable.
//!   * **GGDEF-SKIP**  — a `FrontendError` (Parse/Elaborate): the program is
//!                       outside the definitional interpreter's surface, so this
//!                       lane simply does not adjudicate it (other lanes do).
//!
//! Then an INLINE, monotone MATCH-count floor (the `c_emit_comparison` model,
//! but ggdef-local — the root `parity_floor_active` is unreachable from this
//! crate, and its linux/C-backend carve-outs do not apply to a pure-Rust
//! interpreter). The floor's own escape hatch is `GG_GGDEF_CONFORMANCE_FLOOR_OFF`.

use std::fs;
use std::path::{Path, PathBuf};

use ggdef::{parse_frontmatter, run_source, Outcome, DEFAULT_FUEL};

/// The monotone MATCH-count floor.
///
/// Seeded from a run regenerated IN THIS WORKTREE (never a dated number):
///   cargo test -p ggdef --test spec_conformance_ggdef -- --nocapture
///   → total=214 · MATCH=209 · MISMATCH=0 · GGDEF-SKIP=5
///
/// (5 original seeds + the 182-fixture P1-D "AGREE" migration + the 8 D11
/// trap-normalization fixtures + the may-move pair — `reject_use_after_move.gg`
/// (the `E_` reject code + exit 1 + empty stdout) and its accept complement
/// `reinit_accept.gg` (whole-local revive → Value "new") — plus the FIVE
/// migrated liveness/move rejects that the self-host now renders with the
/// ratified `error[E_<code>]` headline: `reject_double_move.gg` (E_DoubleMove),
/// `reject_move_in_loop.gg` (E_MoveInLoop), `reject_use_after_move_branch.gg`
/// (E_UseAfterMove), `reject_consuming_self_use_after_move.gg` (E_UseAfterMove),
/// and `reject_consume_callable_double.gg` (E_DoubleMove) — each rejected by the
/// interpreter's `liveness.rs` may-move / consume-call kill. Every fixture's
/// `expect:` is ggdef-generated, so on the ggdef lane total == MATCH by
/// construction.)
///
/// RV-F added 12 fixtures. SEVEN are `adjudicator: ggdef` and MATCH here (the
/// count rose 202 → 209): six ACCEPT seeds (`copy_field_borrow_ok`,
/// `copy_struct_field_borrow_ok` — the counterfactual-verified PIN on the #11
/// Copy-axis struct extension (Prim-only rejects its all-int struct-field read),
/// `loop_reassign_revive_move_ok`, `loop_body_local_move_ok`,
/// `callable_move_bind_return_ok`, `callable_param_rebind_ok`) plus the LIVENESS
/// reject `reject_for_var_move_in_loop.gg` (E_MoveInLoop, an eval-time
/// `IllFormed`). The other FIVE are `adjudicator: production-v1` ELABORATE-stage
/// rejections (E_BorrowConflict / E_MoveWithoutOperator) — the interpreter raises
/// them as a `FrontendError`, which this run-surface lane records as GGDEF-SKIP,
/// NOT MATCH (GGDEF-SKIP rose 0 → 5). The production lanes affirm those rejects at
/// build time (`tests/spec_conformance.rs`); the definition owns them at the
/// elaborate boundary, off this lane's run-surface.
///
/// D29 (visible error propagation) added SIX gate-8 seeds, ALL `adjudicator:
/// ggdef` and ALL MATCH here (the count rose 209 → 215): two ACCEPT+run
/// (`d29_unmarked_capture_accept`, `d29_tvariant_marked_match_accept`) and four
/// rejects the ggdef run-surface AFFIRMS on the CODE axis as a coded `IllFormed`
/// (`E_MissingFallibleMark`, TYPED metadata on `Program.d29_reject` surfaced by
/// `run` before eval) — NOT a `FrontendError` SKIP: `d29_bare_throws_discard_
/// reject`, `d29_kind2_bare_discard_reject`, `d29_mark_capture_reject`,
/// `d29_marked_match_result_arms_reject`. The auto-wrap retirement makes a bare
/// fallible call a codeful reject rather than an auto-propagate, so all four are
/// MATCH (not SKIP). GGDEF-SKIP stays 5.
///
/// Bump-on-improvement: when MATCH rises — a new run seed lands, or P1-A
/// coverage retires a GGDEF-SKIP — raise this in the SAME commit that lands the
/// gain, so the improvement is locked in.
const GGDEF_MATCH_FLOOR: usize = 216;

fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

#[test]
fn spec_conformance_ggdef() {
    let run_dir = ws_root().join("spectests/run");
    let mut fixtures: Vec<PathBuf> = fs::read_dir(&run_dir)
        .expect("read spectests/run")
        .filter_map(|e| {
            let p = e.unwrap().path();
            (p.extension().and_then(|x| x.to_str()) == Some("gg")).then_some(p)
        })
        .collect();
    fixtures.sort();

    // Guard the glob: an empty (or shrunken-below-floor) set must not make this
    // lane vacuously green.
    assert!(
        fixtures.len() >= GGDEF_MATCH_FLOOR,
        "expected >= {GGDEF_MATCH_FLOOR} run fixtures in spectests/run, found {}",
        fixtures.len()
    );

    let mut table =
        String::from("\n══ spec_conformance_ggdef ══ (definitional interpreter vs committed expect:)\n\n");
    let mut matched = 0usize;
    let mut mismatched = 0usize;
    let mut skipped = 0usize;
    let mut mismatches: Vec<String> = Vec::new();
    let mut frontmatter_errs: Vec<String> = Vec::new();

    for path in &fixtures {
        let name = path.file_name().unwrap().to_string_lossy().to_string();
        let src = fs::read_to_string(path).unwrap();

        let fm = match parse_frontmatter(&src) {
            Ok(f) => f,
            Err(e) => {
                table.push_str(&format!("  FRONTMATTER-ERR  {name}  ({e})\n"));
                frontmatter_errs.push(format!("  {name}: {e}"));
                continue;
            }
        };

        match run_source(&src, DEFAULT_FUEL) {
            Ok(run) => {
                let got_exit = run.outcome.exit_code();
                // On a Trap, also compare the `T_` code (the expectation was
                // GENERATED from ggdef, so this matches by construction — but the
                // check must actually verify the code, not just the exit).
                let got_trap: Option<&str> = match &run.outcome {
                    Outcome::Trap(kind) => Some(kind.code()),
                    _ => None,
                };
                // On a static rejection, compare the ratified `E_` reject code —
                // keyed on the outcome KIND (`IllFormed`) + the typed `reject_code`
                // on `Run`, NEVER re-parsed from the message and NEVER inferred
                // from exit alone (pin 3; the Value-exit-1-vs-reject-exit-1
                // disambiguation). Without this, a WRONG-code regression would
                // MATCH silently (empty stdout, exit 1) — the Core-#6/#8 hole this
                // axis closes.
                let got_reject: Option<&str> = match &run.outcome {
                    Outcome::IllFormed(_) => run.reject_code,
                    _ => None,
                };
                if got_exit == fm.expect.exit
                    && run.stdout == fm.expect.stdout
                    && got_trap == fm.expect.trap.as_deref()
                    && got_reject == fm.expect.reject.as_deref()
                {
                    matched += 1;
                    table.push_str(&format!(
                        "  MATCH        [{:<12}] {name}\n",
                        fm.adjudicator.as_deref().unwrap_or("-")
                    ));
                } else {
                    mismatched += 1;
                    table.push_str(&format!("  MISMATCH     {name}\n"));
                    mismatches.push(format!(
                        "  {name}: exit {got_exit} vs expect {} · trap {:?} vs expect {:?} · reject {:?} vs expect {:?} · stdout {:?} vs expect {:?}",
                        fm.expect.exit, got_trap, fm.expect.trap, got_reject, fm.expect.reject, run.stdout, fm.expect.stdout
                    ));
                }
            }
            // Parse/Elaborate FrontendError ⇒ outside the definitional surface.
            Err(e) => {
                skipped += 1;
                table.push_str(&format!("  GGDEF-SKIP   {name}  (out of surface: {e})\n"));
            }
        }
    }

    table.push_str(&format!(
        "\n  total={} · MATCH={matched} · MISMATCH={mismatched} · GGDEF-SKIP={skipped}\n",
        fixtures.len()
    ));
    eprintln!("{table}");

    // A malformed committed seed corrupts every verdict — always fatal,
    // independent of the floor escape hatch.
    assert!(
        frontmatter_errs.is_empty(),
        "spec_conformance_ggdef: {} committed seed(s) have malformed frontmatter — \
         the conformance reader cannot adjudicate them:\n{}",
        frontmatter_errs.len(),
        frontmatter_errs.join("\n")
    );

    let enforce = std::env::var_os("GG_GGDEF_CONFORMANCE_FLOOR_OFF").is_none();
    if enforce {
        assert!(
            mismatches.is_empty(),
            "spec_conformance_ggdef: {} committed seed(s) disagree with a fresh ggdef run:\n{}\n\n\
             The committed `expect:` block is stale, or a semantics change landed without \
             regenerating. Expectations flow FROM the definition — regenerate with:\n  \
             cargo run -p ggdef -- gen spectests/run/<fixture>.gg\n\
             then review the diff (never hand-edit `expect:`). If this is an intentional \
             semantics change, regenerate ALL affected seeds and commit the new expectations.\n\
             Emergency escape hatch (loud, temporary): GG_GGDEF_CONFORMANCE_FLOOR_OFF=1.",
            mismatches.len(),
            mismatches.join("\n")
        );
        assert!(
            matched >= GGDEF_MATCH_FLOOR,
            "spec_conformance_ggdef MATCH-count floor regression: MATCH {matched} < floor \
             {GGDEF_MATCH_FLOOR}.\n\n\
             A change dropped a run seed out of the definitional interpreter's surface \
             (GGDEF-SKIP) or otherwise lowered the MATCH count. The table above names the \
             fixtures — fix the regression rather than lowering the floor.\n\n\
             Regenerate the count with:\n  \
             cargo test -p ggdef --test spec_conformance_ggdef -- --nocapture\n\n\
             If MATCH went UP (a new seed landed, or P1-A coverage retired a GGDEF-SKIP), raise \
             GGDEF_MATCH_FLOOR in spec/ggdef/tests/spec_conformance_ggdef.rs in the SAME commit \
             to lock in the gain.\n\
             Emergency escape hatch (loud, temporary): GG_GGDEF_CONFORMANCE_FLOOR_OFF=1."
        );
    }
}
