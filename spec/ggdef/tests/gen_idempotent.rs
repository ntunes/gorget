//! Increment-C gate: `ggdef -- gen` is idempotent (RFC §4 — expectations flow
//! FROM the definition, regenerable with no drift).
//!
//! For every committed `spectests/run/*.gg` seed **whose `adjudicator` is
//! `ggdef`** we assert `gen_frontmatter(committed) == committed`. That single
//! equality is stronger than a two-run diff: it proves BOTH (a) the committed
//! `expect:` block is up to date with what `ggdef` actually produces today, AND
//! (b) a second `gen` is a no-op (idempotence) — because `gen`'s output is the
//! fixed point the committed file must already sit at.
//!
//! **Adjudicator-aware (D1 FOLD-2 §3):** `gen` reflects the DEFINITIONAL
//! interpreter's outcome, so it only owns `adjudicator: ggdef` fixtures. A
//! future `adjudicator: production-v1` seed (D2) carries an expectation captured
//! from production, which `ggdef` may not reproduce (it ElabErrors out of
//! surface) — running `gen` on it would panic. Those are counted-and-reported,
//! not gen-checked. (Today ALL committed seeds are `ggdef`-adjudicated, so every
//! one is still checked.)
//!
//! If this fails after an intentional semantics change, the fix is to re-run
//! `cargo run -p ggdef -- gen spectests/run/<fixture>.gg` and review the diff —
//! never to hand-edit the `expect:` block.

use std::fs;
use std::path::{Path, PathBuf};

fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

#[test]
fn gen_is_idempotent_over_run_seeds() {
    let dir = ws_root().join("spectests/run");
    let mut seeds: Vec<PathBuf> = fs::read_dir(&dir)
        .expect("read spectests/run")
        .filter_map(|e| {
            let p = e.unwrap().path();
            (p.extension().and_then(|x| x.to_str()) == Some("gg")).then_some(p)
        })
        .collect();
    seeds.sort();

    // Guard the glob: an empty set would make this gate vacuously green.
    assert!(seeds.len() >= 3, "expected >=3 run seeds, found {}", seeds.len());

    let mut drift: Vec<String> = Vec::new();
    let mut checked = 0usize;
    let mut skipped_non_ggdef = 0usize;
    for seed in &seeds {
        let committed = fs::read_to_string(seed).unwrap();
        // The adjudicator decides ownership — only `gen` (= ggdef) owns
        // `adjudicator: ggdef` seeds; parse it via the SHARED reader.
        let fm = ggdef::parse_frontmatter(&committed)
            .unwrap_or_else(|e| panic!("parse {}: {e}", seed.display()));
        if fm.adjudicator.as_deref() != Some("ggdef") {
            skipped_non_ggdef += 1;
            continue;
        }
        checked += 1;
        let regenerated = ggdef::gen_frontmatter(&committed, ggdef::DEFAULT_FUEL).unwrap_or_else(|e| {
            panic!("gen {}: {e}", seed.display());
        });
        if regenerated != committed {
            drift.push(format!(
                "  {}: `gen` output differs from committed (stale expect: block or non-idempotent)",
                seed.file_name().unwrap().to_string_lossy()
            ));
        }
    }

    eprintln!(
        "gen_idempotent: {checked} ggdef-adjudicated seed(s) checked, \
         {skipped_non_ggdef} non-ggdef seed(s) reported-not-checked"
    );

    assert!(
        drift.is_empty(),
        "{} seed(s) drifted — re-run `ggdef -- gen` and review:\n{}",
        drift.len(),
        drift.join("\n")
    );
}
