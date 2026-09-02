// Declared ggdef corpus membership (R48 Track E-B3 / t0801 class).
//
// `corpus_b.rs` / `corpus_b1.rs` used to harvest `cow_*` / `deadwrite_*` /
// `combinator_*` minus a by-name EXCLUDE, so a new fixture was opted IN by
// default and reddened a lane its own track never ran. This lint pins a named
// census of every prefix-matching on-disk fixture against SOURCE-parsed
// prefixes and EXCLUDE. New `cow_*` / `deadwrite_*` / `combinator_*` without a
// row REDS; a stale row REDS; duplicate EXCLUDE literals RED; REPORT-ONLY is
// an exact-pin so a fixture with no `run_gg` / `check_gg_fails` / self-host
// pair cannot land green-and-unwitnessed.
//
// Prefixes are parsed from `starts_with(...)` inside `gate_fixtures`. EXCLUDE
// is parsed from comment-stripped SOURCE. Do not re-list the three strings.

use super::ggdef_corpus_membership::{
    GgdefCorpusDisp, GGDEF_CORPUS_B1_CENSUS, GGDEF_CORPUS_B1_REPORT_ONLY, GGDEF_CORPUS_B_CENSUS,
    GGDEF_CORPUS_B_INERT_LIVENESS, GGDEF_CORPUS_B_REPORT_ONLY,
};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Family-block uncited-EXCLUDE ratchet, target 0. EXCLUDE unique size may
/// grow (Core #9); only rows with no covering `//` run are pressured to 0.
const GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING: usize = 0;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Strip `//` and `/* */` comments, keeping string literals intact (unlike
/// `strip_rust_comments_and_strings`, which blanks strings).
fn strip_rust_comments_keep_strings(src: &str) -> String {
    let b: Vec<char> = src.chars().collect();
    let n = b.len();
    let mut out: Vec<char> = Vec::with_capacity(n);
    let mut i = 0usize;
    while i < n {
        if b[i] == '/' && i + 1 < n && b[i + 1] == '/' {
            while i < n && b[i] != '\n' {
                i += 1;
            }
        } else if b[i] == '/' && i + 1 < n && b[i + 1] == '*' {
            i += 2;
            while i + 1 < n && !(b[i] == '*' && b[i + 1] == '/') {
                if b[i] == '\n' {
                    out.push('\n');
                }
                i += 1;
            }
            i = i.saturating_add(2).min(n);
        } else if b[i] == '"' {
            out.push('"');
            i += 1;
            while i < n {
                out.push(b[i]);
                if b[i] == '\\' && i + 1 < n {
                    out.push(b[i + 1]);
                    i += 2;
                    continue;
                }
                if b[i] == '"' {
                    i += 1;
                    break;
                }
                i += 1;
            }
        } else {
            out.push(b[i]);
            i += 1;
        }
    }
    out.into_iter().collect()
}

fn rust_fn_body<'a>(src: &'a str, fn_name: &str) -> &'a str {
    let needle = format!("fn {fn_name}");
    let start = src
        .find(&needle)
        .unwrap_or_else(|| panic!("no `fn {fn_name}` in source"));
    let brace = start
        + src[start..]
            .find('{')
            .unwrap_or_else(|| panic!("`fn {fn_name}` has no body"));
    let bytes = src.as_bytes();
    let mut depth = 0i32;
    let mut i = brace;
    let mut in_str = false;
    while i < bytes.len() {
        let c = bytes[i] as char;
        if in_str {
            if c == '\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }
            if c == '"' {
                in_str = false;
            }
            i += 1;
            continue;
        }
        match c {
            '"' => in_str = true,
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return &src[brace..=i];
                }
            }
            _ => {}
        }
        i += 1;
    }
    panic!("unclosed `fn {fn_name}`");
}

fn exclude_array_body(src: &str) -> &str {
    const START: &str = "const EXCLUDE: &[&str] = &[";
    let start = src
        .find(START)
        .unwrap_or_else(|| panic!("no `const EXCLUDE` array"))
        + START.len();
    let rest = &src[start..];
    let end = rest
        .find("];")
        .unwrap_or_else(|| panic!("unclosed `const EXCLUDE` array"));
    &rest[..end]
}

fn string_lits(src: &str) -> Vec<String> {
    let re = regex::Regex::new(r#""([^"]+)""#).unwrap();
    re.captures_iter(src)
        .map(|c| c[1].to_string())
        .collect()
}

fn parse_starts_with_in_fn(src: &str, fn_name: &str) -> Vec<String> {
    let body = rust_fn_body(src, fn_name);
    let stripped = strip_rust_comments_keep_strings(body);
    let re = regex::Regex::new(r#"starts_with\(\s*"([^"]+)"\s*\)"#).unwrap();
    re.captures_iter(&stripped)
        .map(|c| c[1].to_string())
        .collect()
}

fn parse_contains_in_fn(src: &str, fn_name: &str) -> Vec<String> {
    let body = rust_fn_body(src, fn_name);
    let stripped = strip_rust_comments_keep_strings(body);
    let re = regex::Regex::new(r#"contains\(\s*"([^"]+)"\s*\)"#).unwrap();
    re.captures_iter(&stripped)
        .map(|c| c[1].to_string())
        .collect()
}

fn exclude_literals_comment_stripped(src: &str) -> Vec<String> {
    let stripped = strip_rust_comments_keep_strings(src);
    string_lits(exclude_array_body(&stripped))
}

fn exclude_duplicates(lits: &[String]) -> Vec<String> {
    let mut seen = BTreeSet::new();
    let mut dups = Vec::new();
    for n in lits {
        if !seen.insert(n.clone()) {
            dups.push(n.clone());
        }
    }
    dups
}

/// A `//` run covers following names until the next `//` run. Names that
/// appear before any covering run are uncited. Not per-literal-comment.
fn family_block_uncited(src: &str) -> Vec<String> {
    let body = exclude_array_body(src);
    let lines: Vec<&str> = body.lines().collect();
    let mut covering = false;
    let mut uncited = Vec::new();
    let mut i = 0usize;
    while i < lines.len() {
        let t = lines[i].trim();
        if t.is_empty() {
            i += 1;
            continue;
        }
        if t.starts_with("//") {
            while i < lines.len() {
                let u = lines[i].trim();
                if u.is_empty() || u.starts_with("//") {
                    i += 1;
                } else {
                    break;
                }
            }
            covering = true;
            continue;
        }
        for name in string_lits(lines[i]) {
            if !covering {
                uncited.push(name);
            }
        }
        if let Some((_, after)) = lines[i].rsplit_once('"') {
            if after.contains("//") {
                covering = true;
            }
        }
        i += 1;
    }
    uncited
}

fn disk_gg_matching(root: &Path, prefixes: &[String]) -> BTreeSet<String> {
    let dir = root.join("tests/fixtures");
    fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("read {}: {e}", dir.display()))
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .filter(|n| n.ends_with(".gg") && prefixes.iter().any(|p| n.starts_with(p)))
        .collect()
}

fn preceded_by_call(integ: &str, pos: usize, needle_fn: &str) -> bool {
    let start = pos.saturating_sub(64);
    integ[start..pos]
        .trim_end()
        .ends_with(&format!("{needle_fn}("))
}

/// Whether `tests/integration.rs` carries a `run_gg` / `check_gg_fails` /
/// self-host stdout pair for this fixture (the MATCH witness).
fn fixture_is_witnessed(integ: &str, fixture: &str) -> bool {
    let needle = format!("\"{fixture}\"");
    let mut search = 0;
    while let Some(rel) = integ[search..].find(&needle) {
        let pos = search + rel;
        if preceded_by_call(integ, pos, "run_gg") || preceded_by_call(integ, pos, "check_gg_fails")
        {
            return true;
        }
        search = pos + needle.len();
    }
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
                if seg2[sp..].contains('"') {
                    return true;
                }
            }
        }
    }
    false
}

fn census_map(rows: &[(&str, GgdefCorpusDisp)]) -> BTreeMap<String, GgdefCorpusDisp> {
    let mut m = BTreeMap::new();
    for (n, d) in rows {
        let prev = m.insert((*n).to_string(), *d);
        assert!(
            prev.is_none(),
            "census itself has a duplicate row `{n}` — the named list is the SET"
        );
    }
    m
}

fn sorted(set: &BTreeSet<String>) -> Vec<&String> {
    set.iter().collect()
}

fn assert_set_eq(label: &str, left: &BTreeSet<String>, right: &BTreeSet<String>) {
    if left != right {
        let only_left: BTreeSet<_> = left.difference(right).cloned().collect();
        let only_right: BTreeSet<_> = right.difference(left).cloned().collect();
        panic!(
            "{label} drifted.\n  only in left ({}): {:?}\n  only in right ({}): {:?}",
            only_left.len(),
            sorted(&only_left),
            only_right.len(),
            sorted(&only_right)
        );
    }
}

/// A THROWAWAY repo root holding `tests/fixtures/<name>` and nothing else.
///
/// The Core #13 self-probes used to write into the LIVE `tests/fixtures/`.
/// That directory is shared state: `..._b_membership_is_declared` and
/// `..._b1_membership_is_declared` run concurrently in one binary and both
/// glob `cow_*`, so each saw the other's probe and reddened `census names vs
/// disk` with the other's throwaway name — an intermittent red on a guard,
/// plus a stray `.gg` left in the repo whenever the process died inside the
/// window. What the self-probe actually claims is a property of
/// `disk_gg_matching` + set-difference over the SOURCE-parsed prefixes, so it
/// is exercised against a synthetic root and touches no shared state.
fn probe_root(name: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("probe tempdir");
    let fixtures = dir.path().join("tests/fixtures");
    fs::create_dir_all(&fixtures)
        .unwrap_or_else(|e| panic!("create {}: {e}", fixtures.display()));
    let path = fixtures.join(name);
    fs::write(&path, "void main():\n    print(1)\n")
        .unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
    dir
}

#[test]
fn ggdef_corpus_b_membership_is_declared() {
    let root = repo_root();
    let src = fs::read_to_string(root.join("spec/ggdef/tests/corpus_b.rs"))
        .expect("read spec/ggdef/tests/corpus_b.rs");
    let integ = fs::read_to_string(root.join("tests/integration.rs")).expect("read integration.rs");
    let prefixes = parse_starts_with_in_fn(&src, "gate_fixtures");
    assert!(
        !prefixes.is_empty(),
        "parsed no starts_with(...) inside corpus_b.rs::gate_fixtures — the \
         lint would go tautological"
    );

    let lits = exclude_literals_comment_stripped(&src);
    let dups = exclude_duplicates(&lits);
    assert!(
        dups.is_empty(),
        "corpus_b.rs EXCLUDE has duplicate literals (comment-stripped): {dups:?}. \
         Fix the duplicates; do not delete live exclusions."
    );

    let unique: BTreeSet<String> = lits.iter().cloned().collect();
    let prefix_reachable: BTreeSet<String> = unique
        .iter()
        .filter(|n| prefixes.iter().any(|p| n.starts_with(p)))
        .cloned()
        .collect();
    let inert: BTreeSet<String> = unique.difference(&prefix_reachable).cloned().collect();
    let pinned_inert: BTreeSet<String> = GGDEF_CORPUS_B_INERT_LIVENESS
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    assert_set_eq(
        "inert liveness_* EXCLUDE rows (outside the prefix glob; explain, do not delete)",
        &inert,
        &pinned_inert,
    );

    let disk = disk_gg_matching(&root, &prefixes);
    let census = census_map(GGDEF_CORPUS_B_CENSUS);
    let census_names: BTreeSet<String> = census.keys().cloned().collect();
    assert_set_eq(
        "corpus_b census names vs disk ∩ parsed prefixes",
        &census_names,
        &disk,
    );

    let census_exclude: BTreeSet<String> = census
        .iter()
        .filter(|(_, d)| **d == GgdefCorpusDisp::Exclude)
        .map(|(n, _)| n.clone())
        .collect();
    assert_set_eq(
        "corpus_b census EXCLUDE vs parsed unique prefix-reachable EXCLUDE",
        &census_exclude,
        &prefix_reachable,
    );

    assert!(
        census.values().all(|d| *d != GgdefCorpusDisp::Equip),
        "corpus_b has no equip filter; Equip rows belong on the b1 census"
    );

    let census_ro: BTreeSet<String> = census
        .iter()
        .filter(|(_, d)| **d == GgdefCorpusDisp::ReportOnly)
        .map(|(n, _)| n.clone())
        .collect();
    let pinned_ro: BTreeSet<String> = GGDEF_CORPUS_B_REPORT_ONLY
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    assert_set_eq(
        "corpus_b REPORT-ONLY named list vs census ReportOnly rows",
        &census_ro,
        &pinned_ro,
    );

    let mut unwitnessed_match = Vec::new();
    let mut witnessed_report = Vec::new();
    for (name, disp) in &census {
        match *disp {
            GgdefCorpusDisp::Match => {
                if !fixture_is_witnessed(&integ, name) {
                    unwitnessed_match.push(name.clone());
                }
            }
            GgdefCorpusDisp::ReportOnly => {
                if fixture_is_witnessed(&integ, name) {
                    witnessed_report.push(name.clone());
                }
            }
            GgdefCorpusDisp::Exclude | GgdefCorpusDisp::Equip => {}
        }
    }
    assert!(
        unwitnessed_match.is_empty(),
        "census MATCH rows with no run_gg / check_gg_fails / self-host pair \
         (they are REPORT-ONLY, not witnessed): {unwitnessed_match:?}"
    );
    assert!(
        witnessed_report.is_empty(),
        "census REPORT-ONLY rows that ARE witnessed in integration.rs \
         (move them to MATCH): {witnessed_report:?}"
    );

    // Core #13: the guard must be seen to fail.
    {
        let probe = probe_root("cow_eb3_membership_probe.gg");
        let disk2 = disk_gg_matching(probe.path(), &prefixes);
        assert!(
            disk2.difference(&census_names).next().is_some(),
            "throwaway cow_* without a census row did NOT redden membership"
        );
    }
    {
        let probe = probe_root("combinator_eb3_membership_probe.gg");
        let disk2 = disk_gg_matching(probe.path(), &prefixes);
        assert!(
            disk2.difference(&census_names).next().is_some(),
            "throwaway combinator_* without a census row did NOT redden membership \
             — combinator_ is in the parsed prefix set; if this fires, gate_fixtures \
             lost combinator_ and the t0801 class is back inside its own guard"
        );
    }
    {
        let mut stale = census_names.clone();
        stale.insert("cow_eb3_stale_row_deleted_name.gg".into());
        assert_ne!(
            stale, disk,
            "stale census row for a deleted name did NOT redden membership"
        );
    }
}

#[test]
fn ggdef_corpus_b_report_only_is_exact() {
    let pinned: Vec<&str> = {
        let mut v: Vec<&str> = GGDEF_CORPUS_B_REPORT_ONLY.to_vec();
        v.sort_unstable();
        v
    };
    let mut from_census: Vec<&str> = GGDEF_CORPUS_B_CENSUS
        .iter()
        .filter(|(_, d)| *d == GgdefCorpusDisp::ReportOnly)
        .map(|(n, _)| *n)
        .collect();
    from_census.sort_unstable();
    assert_eq!(
        pinned, from_census,
        "GGDEF_CORPUS_B_REPORT_ONLY is an exact-pin of the 26 (both-asserts). \
         A new fixture with no run_gg pair cannot land here green: grow this \
         list in the SAME commit, or give it a MATCH witness."
    );
    // Both-asserts against itself so a shrink of the named list without a
    // matching census edit is also red (the two lists are the two sides).
    assert_eq!(
        GGDEF_CORPUS_B_REPORT_ONLY.len(),
        from_census.len(),
        "REPORT-ONLY named-list length drifted from the census"
    );
}

#[test]
fn ggdef_corpus_b_exclude_citations_cover_every_row() {
    let root = repo_root();
    let src = fs::read_to_string(root.join("spec/ggdef/tests/corpus_b.rs"))
        .expect("read spec/ggdef/tests/corpus_b.rs");
    let uncited = family_block_uncited(&src);
    assert!(
        uncited.len() <= GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING,
        "uncited EXCLUDE rows GREW: {} > {GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING}.\n\
         {uncited:?}\nA `//` family-block run must cover following names until the \
         next `//` run. EXCLUDE unique size may grow (Core #9: a note + a filed \
         subset gap); an uncited add is undeclared justification.",
        uncited.len(),
    );
    assert!(
        uncited.len() >= GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING,
        "uncited EXCLUDE rows SHRANK: {} < {GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING} — \
         good. Lower GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING to {} in the SAME commit \
         (shrink-only on the uncited count; do not shrink unique EXCLUDE size).",
        uncited.len(),
        uncited.len(),
    );
}

#[test]
fn ggdef_corpus_b_inert_liveness_is_exact() {
    // The named list is the pin (explain, do not delete, do not silently
    // grow). Membership already set-equals it against parsed unique EXCLUDE
    // names that miss the prefix glob; this test pins the names exist on
    // disk and are the liveness_* family.
    let mut pinned: Vec<&str> = GGDEF_CORPUS_B_INERT_LIVENESS.to_vec();
    pinned.sort_unstable();
    assert!(
        !pinned.is_empty(),
        "inert liveness_* list vanished — those rows are outside the prefix \
         glob, so set-equality on the glob cannot see them"
    );
    for n in &pinned {
        assert!(
            n.starts_with("liveness_") && n.ends_with(".gg"),
            "inert list is the liveness_* EXCLUDE rows, got {n}"
        );
        let path = repo_root().join("tests/fixtures").join(n);
        assert!(
            path.is_file(),
            "inert EXCLUDE `{n}` is not on disk at {}",
            path.display()
        );
    }
    let mut uniq = pinned.clone();
    uniq.dedup();
    assert_eq!(uniq, pinned, "inert liveness_* named list has duplicates");
}

#[test]
fn ggdef_corpus_b1_membership_is_declared() {
    let root = repo_root();
    let src = fs::read_to_string(root.join("spec/ggdef/tests/corpus_b1.rs"))
        .expect("read spec/ggdef/tests/corpus_b1.rs");
    let integ = fs::read_to_string(root.join("tests/integration.rs")).expect("read integration.rs");
    let prefixes = parse_starts_with_in_fn(&src, "gate_fixtures");
    assert!(
        !prefixes.is_empty(),
        "parsed no starts_with(...) inside corpus_b1.rs::gate_fixtures"
    );
    assert!(
        prefixes.iter().all(|p| p != "combinator_"),
        "corpus_b1 must not grow a combinator_ prefix (G3); parsed {prefixes:?}"
    );
    let contains = parse_contains_in_fn(&src, "gate_fixtures");
    assert_eq!(
        contains.as_slice(),
        &["equip ".to_string()][..],
        "corpus_b1 gate_fixtures must parse contains(\"equip \") from SOURCE, \
         not a hardcoded drop-count; parsed {contains:?}"
    );
    let equip_needle = &contains[0];

    let lits = exclude_literals_comment_stripped(&src);
    let dups = exclude_duplicates(&lits);
    assert!(
        dups.is_empty(),
        "corpus_b1.rs EXCLUDE has duplicate literals (comment-stripped): {dups:?}"
    );

    let unique: BTreeSet<String> = lits.iter().cloned().collect();
    let prefix_reachable: BTreeSet<String> = unique
        .iter()
        .filter(|n| prefixes.iter().any(|p| n.starts_with(p)))
        .cloned()
        .collect();
    let inert: BTreeSet<String> = unique.difference(&prefix_reachable).cloned().collect();
    let pinned_inert: BTreeSet<String> = GGDEF_CORPUS_B_INERT_LIVENESS
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    assert_set_eq(
        "corpus_b1 inert liveness_* (same 15 as corpus_b)",
        &inert,
        &pinned_inert,
    );

    let disk = disk_gg_matching(&root, &prefixes);
    let census = census_map(GGDEF_CORPUS_B1_CENSUS);
    let census_names: BTreeSet<String> = census.keys().cloned().collect();
    assert_set_eq(
        "corpus_b1 census names vs disk ∩ parsed prefixes",
        &census_names,
        &disk,
    );

    let census_exclude: BTreeSet<String> = census
        .iter()
        .filter(|(_, d)| **d == GgdefCorpusDisp::Exclude)
        .map(|(n, _)| n.clone())
        .collect();
    assert_set_eq(
        "corpus_b1 census EXCLUDE vs parsed unique prefix-reachable EXCLUDE",
        &census_exclude,
        &prefix_reachable,
    );

    let fixtures = root.join("tests/fixtures");
    for (name, disp) in &census {
        let src_txt = fs::read_to_string(fixtures.join(name))
            .unwrap_or_else(|e| panic!("read {name}: {e}"));
        let has_equip = src_txt.contains(equip_needle.as_str());
        match *disp {
            GgdefCorpusDisp::Equip => assert!(
                has_equip,
                "census Equip `{name}` does not contain the parsed equip filter {equip_needle:?}"
            ),
            GgdefCorpusDisp::Match | GgdefCorpusDisp::ReportOnly => assert!(
                !has_equip,
                "census {disp:?} `{name}` contains {equip_needle:?} — that is Equip, \
                 not gated in b1"
            ),
            GgdefCorpusDisp::Exclude => {}
        }
    }

    let census_ro: BTreeSet<String> = census
        .iter()
        .filter(|(_, d)| **d == GgdefCorpusDisp::ReportOnly)
        .map(|(n, _)| n.clone())
        .collect();
    let pinned_ro: BTreeSet<String> = GGDEF_CORPUS_B1_REPORT_ONLY
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    assert_set_eq(
        "corpus_b1 REPORT-ONLY named list vs census ReportOnly rows",
        &census_ro,
        &pinned_ro,
    );

    let mut unwitnessed_match = Vec::new();
    let mut witnessed_report = Vec::new();
    for (name, disp) in &census {
        match *disp {
            GgdefCorpusDisp::Match => {
                if !fixture_is_witnessed(&integ, name) {
                    unwitnessed_match.push(name.clone());
                }
            }
            GgdefCorpusDisp::ReportOnly => {
                if fixture_is_witnessed(&integ, name) {
                    witnessed_report.push(name.clone());
                }
            }
            GgdefCorpusDisp::Exclude | GgdefCorpusDisp::Equip => {}
        }
    }
    assert!(
        unwitnessed_match.is_empty(),
        "b1 MATCH rows with no witness: {unwitnessed_match:?}"
    );
    assert!(
        witnessed_report.is_empty(),
        "b1 REPORT-ONLY rows that are witnessed: {witnessed_report:?}"
    );

    let uncited = family_block_uncited(&src);
    assert!(
        uncited.len() <= GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING,
        "b1 uncited EXCLUDE GREW: {} > {GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING}: {uncited:?}",
        uncited.len()
    );
    assert!(
        uncited.len() >= GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING,
        "b1 uncited EXCLUDE SHRANK: {} < {GGDEF_CORPUS_B_UNCITED_EXCLUDE_CEILING} — lower the \
         ceiling to {} in the SAME commit",
        uncited.len(),
        uncited.len()
    );

    {
        let probe = probe_root("cow_eb3_b1_membership_probe.gg");
        let disk2 = disk_gg_matching(probe.path(), &prefixes);
        assert!(
            disk2.difference(&census_names).next().is_some(),
            "throwaway cow_* without a b1 census row did NOT redden membership"
        );
    }
    {
        let mut stale = census_names.clone();
        stale.insert("cow_eb3_b1_stale_row_deleted_name.gg".into());
        assert_ne!(stale, disk, "stale b1 census row did NOT redden membership");
    }
}

#[test]
fn ggdef_corpus_b1_report_only_is_exact() {
    let pinned: Vec<&str> = {
        let mut v: Vec<&str> = GGDEF_CORPUS_B1_REPORT_ONLY.to_vec();
        v.sort_unstable();
        v
    };
    let mut from_census: Vec<&str> = GGDEF_CORPUS_B1_CENSUS
        .iter()
        .filter(|(_, d)| *d == GgdefCorpusDisp::ReportOnly)
        .map(|(n, _)| *n)
        .collect();
    from_census.sort_unstable();
    assert_eq!(
        pinned, from_census,
        "GGDEF_CORPUS_B1_REPORT_ONLY is an exact-pin (both-asserts)"
    );
}
