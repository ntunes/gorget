//! ggdef COVERAGE HISTOGRAM — the Phase-1 coverage gate (promoted from the
//! scout probe `scout_probe.rs::scout_coverage_histogram`, RFC §6 Phase 1
//! "coverage completion"; brief pass-1 R1).
//!
//! Walks every non-`cow_*`/non-`deadwrite_*` top-level fixture (the `cow_*` /
//! `deadwrite_*` corpus is gated separately by `corpus_b.rs`) and classifies
//! ggdef's outcome. Prints an always-visible histogram + the top ElabError
//! buckets (what surface is still out of reach), then asserts an **inline
//! monotone floor** on the count of fixtures ggdef elaborates+runs and the
//! count that reach a clean `Value`. This is the c_emit_comparison precedent
//! (an inline dynamic floor per runner, RFC §4 amendment) — NOT a
//! `tests/lints.rs` static ratchet: a coverage count is DYNAMIC (requires
//! running), so it cannot be a grep-the-source arm-count.
//!
//! The floor is a RATCHET: climb it in the SAME commit that lands the coverage
//! that raised the number. It normally only goes UP; a drop is a regression —
//! with ONE sanctioned exception (below).
//!
//! Coverage-completion NEVER "climbs" by making a wrong answer silently pass:
//! the safety rule is that an unimplemented construct is a LOUD `ElabError`
//! (frontend failure), never a mis-evaluated `Value`. Converting a *silent-
//! wrong* path into a loud `ElabError` is a CORRECTNESS WIN that legitimately
//! LOWERS these counts (the fixture leaves `Value`/`elaborates` for `ELAB-ERR`).
//! That is the sanctioned floor-lowering; the real coverage-correctness signal
//! lives in the *agreement* gate (`converter_agreement.rs`, `AGREE` — monotone-
//! up), read together with this one. (The throws→Result landing did exactly
//! this: VALUE 277→276 as two silent-wrong `throws` fixtures became loud
//! errors, while `AGREE` rose +7.)

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

fn ws_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

/// `stack_guard_deep_recursion.gg` is a designed unbounded-recursion fixture
/// (RFC C11 implementation-defined resource exhaustion). ggdef's big-step eval
/// recurses on the native stack, so it SIGABRTs rather than reporting
/// `FuelExhausted` (a filed BONUS finding — fuel bounds STEP count, not eval
/// DEPTH). Excluded from the walk so the gate cannot crash; its disposition is
/// the native-recursion track (TODO HIGH), out of P1-A scope.
const EXCLUDE: &[&str] = &["stack_guard_deep_recursion.gg"];

// ── INLINE MONOTONE FLOORS (regenerate + bump in the landing commit) ──────────
//
// Regenerate with:
//   cargo test -p ggdef --test coverage_histogram -- --nocapture
// and read the "elaborates+runs" / "clean VALUE" lines. NEVER lower these.

/// Fixtures whose frontend (parse+elaborate) succeeds so eval produces an
/// outcome (Value/Trap/IllFormed/FuelExhausted). The honest "in reach" count.
const ELABORATES_FLOOR: usize = 340;
/// Fixtures reaching a clean `Value` outcome.
const VALUE_FLOOR: usize = 276;

fn fixture_names(dir: &Path) -> Vec<String> {
    let mut names: Vec<String> = fs::read_dir(dir)
        .expect("read fixtures")
        .filter_map(|e| {
            let n = e.unwrap().file_name().into_string().unwrap();
            if !n.ends_with(".gg") {
                return None;
            }
            if n.starts_with("cow_") || n.starts_with("deadwrite_") {
                return None;
            }
            if EXCLUDE.contains(&n.as_str()) {
                return None;
            }
            Some(n)
        })
        .collect();
    names.sort();
    names
}

#[test]
fn coverage_histogram() {
    // ggdef's recursive big-step eval is NOT stack-bounded by fuel, so a
    // moderately-recursive fixture can grow the native stack past the default.
    // Run the whole walk in a 2 GiB-stack thread (the scout-validated setup).
    std::thread::Builder::new()
        .stack_size(2 * 1024 * 1024 * 1024)
        .name("coverage_histogram".to_string())
        .spawn(coverage_histogram_body)
        .unwrap()
        .join()
        .unwrap();
}

fn coverage_histogram_body() {
    let root = ws_root();
    let dir = root.join("tests/fixtures");
    let names = fixture_names(&dir);
    let total = names.len();

    let mut buckets: BTreeMap<&str, usize> = BTreeMap::new();
    let mut elab_msgs: BTreeMap<String, usize> = BTreeMap::new();
    let mut value_count = 0usize;
    let started = std::time::Instant::now();

    for name in &names {
        let src = fs::read_to_string(dir.join(name)).unwrap();
        // Modest fuel bounds runtime; it bounds STEP count, not native
        // recursion depth (the 2 GiB stack covers depth).
        match ggdef::run_source(&src, 5_000_000) {
            Ok(run) => {
                let b = match run.outcome {
                    ggdef::Outcome::Value(_) => {
                        value_count += 1;
                        "VALUE"
                    }
                    ggdef::Outcome::Trap(_) => "TRAP",
                    ggdef::Outcome::IllFormed(_) => "ILLFORMED",
                    ggdef::Outcome::FuelExhausted => "FUEL",
                };
                *buckets.entry(b).or_default() += 1;
            }
            Err(ggdef::FrontendError::Parse(_)) => {
                *buckets.entry("PARSE-ERR").or_default() += 1;
            }
            Err(ggdef::FrontendError::Elaborate(e)) => {
                *buckets.entry("ELAB-ERR").or_default() += 1;
                // Bucket by the leading phrase (first 48 chars) to see WHAT is
                // still out of surface.
                let key: String = e.message.chars().take(48).collect();
                *elab_msgs.entry(key).or_default() += 1;
            }
        }
    }

    let elaborates = value_count
        + buckets.get("TRAP").copied().unwrap_or(0)
        + buckets.get("ILLFORMED").copied().unwrap_or(0)
        + buckets.get("FUEL").copied().unwrap_or(0);

    eprintln!("\n=== ggdef COVERAGE HISTOGRAM (non-cow/deadwrite fixtures) ===");
    eprintln!("pool: {total} fixtures   (elapsed {:?})", started.elapsed());
    for (k, v) in &buckets {
        eprintln!("  {k:<12} {v}");
    }
    eprintln!(
        "  --> elaborates+runs (any eval outcome): {elaborates} / {total} = {:.1}%  [floor {ELABORATES_FLOOR}]",
        100.0 * elaborates as f64 / total as f64
    );
    eprintln!(
        "  --> clean VALUE outcome:                {value_count} / {total} = {:.1}%  [floor {VALUE_FLOOR}]",
        100.0 * value_count as f64 / total as f64
    );

    eprintln!("\n=== top ElabError buckets (what's still out of surface) ===");
    let mut sorted: Vec<_> = elab_msgs.iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(a.1).then(a.0.cmp(b.0)));
    for (msg, n) in sorted.iter().take(30) {
        eprintln!("  {n:>3}  {msg}");
    }
    eprintln!();

    // ── inline monotone floors (bump in the landing commit; never lower) ──
    assert!(
        elaborates >= ELABORATES_FLOOR,
        "ggdef coverage REGRESSED: elaborates+runs {elaborates} < floor {ELABORATES_FLOOR}. \
         If this is an intentional narrowing (a silent-wrong path converted to a loud \
         ElabError), lower the floor WITH a cited reason; otherwise it is a bug.",
    );
    assert!(
        value_count >= VALUE_FLOOR,
        "ggdef coverage REGRESSED: clean VALUE {value_count} < floor {VALUE_FLOOR}. \
         See the ELABORATES_FLOOR note.",
    );
}
