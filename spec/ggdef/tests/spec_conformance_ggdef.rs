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

use ggdef::{parse_frontmatter, run_source, DEFAULT_FUEL};

/// The monotone MATCH-count floor.
///
/// Seeded from a run regenerated IN THIS WORKTREE (never a dated number):
///   cargo test -p ggdef --test spec_conformance_ggdef -- --nocapture
///   → total=187 · MATCH=187 · MISMATCH=0 · GGDEF-SKIP=0
///
/// (5 original seeds + the 182-fixture P1-D "AGREE" migration — every migrated
/// fixture is a ggdef-adjudicated run_gg pair, so total == MATCH by construction.)
///
/// Bump-on-improvement: when MATCH rises — a new run seed lands, or P1-A
/// coverage retires a GGDEF-SKIP — raise this in the SAME commit that lands the
/// gain, so the improvement is locked in.
const GGDEF_MATCH_FLOOR: usize = 187;

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
                if got_exit == fm.expect.exit && run.stdout == fm.expect.stdout {
                    matched += 1;
                    table.push_str(&format!(
                        "  MATCH        [{:<12}] {name}\n",
                        fm.adjudicator.as_deref().unwrap_or("-")
                    ));
                } else {
                    mismatched += 1;
                    table.push_str(&format!("  MISMATCH     {name}\n"));
                    mismatches.push(format!(
                        "  {name}: exit {got_exit} vs expect {} · stdout {:?} vs expect {:?}",
                        fm.expect.exit, run.stdout, fm.expect.stdout
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
