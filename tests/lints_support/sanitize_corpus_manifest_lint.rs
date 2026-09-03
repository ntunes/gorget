// The sanitize sweep's corpus is DECLARED, not an accident of walk depth
// (R48 Track T-a1).
//
// `scripts/sanitize_sweep.sh` used to enumerate its corpus with
// `find tests/fixtures -maxdepth 1 -name '*.gg'`. That depth limit was
// load-bearing — the gate's own positive controls live in
// `tests/fixtures/sanitize_selftest/`, one of them LEAKS BY DESIGN, and a naive
// recursive walk ingests them as findings and destroys the Core #13 self-test —
// but it was an ACCIDENT OF DEPTH, not a decision. Nothing recorded WHY any
// other directory was unwatched, and a new directory joined the unwatched set in
// silence. `tests/sanitize/CORPUS_MANIFEST.txt` is that decision written down:
// one `IN`/`OUT` row per first-level directory, each with its reason.
//
// This lint is the cheap half of the guard (the sweep itself refuses to report a
// verdict against an incomplete manifest, but it costs ~25 minutes). It parses
// the manifest PATH and the IN-selection predicate from the SWEEP'S SOURCE — so
// the two cannot drift into checking different files — then set-equals the
// manifest's directory census against disk.
//
// Do not re-list the directories here. The manifest file IS the named census;
// the reasons are literal text a reviewer reads, never derived from a predicate,
// so a directory cannot be auto-classified out of existence by (say) dropping a
// `main.gg` into it.

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// The manifest path the SWEEP uses, parsed out of its `MANIFEST="${MANIFEST:-…}"`
/// default. Parsed rather than repeated: a lint that checks a different file
/// from the one the gate reads is not a check.
fn manifest_path_from_sweep(sweep: &str) -> String {
    const KEY: &str = "MANIFEST=\"${MANIFEST:-";
    let start = sweep
        .find(KEY)
        .unwrap_or_else(|| panic!("no `{KEY}…` default in scripts/sanitize_sweep.sh"))
        + KEY.len();
    let rest = &sweep[start..];
    let end = rest
        .find("}\"")
        .unwrap_or_else(|| panic!("unterminated MANIFEST default in scripts/sanitize_sweep.sh"));
    rest[..end].to_string()
}

/// One manifest row.
struct ManifestRow {
    dir: String,
    disposition: String,
    reason: String,
}

fn parse_manifest(body: &str) -> Vec<ManifestRow> {
    body.lines()
        .filter(|l| !l.trim_start().starts_with('#') && !l.trim().is_empty())
        .map(|l| {
            let cols: Vec<&str> = l.split('\t').collect();
            assert!(
                cols.len() >= 3,
                "corpus manifest row is not <directory> TAB IN|OUT TAB <reason>: {l:?}"
            );
            ManifestRow {
                dir: cols[0].trim().to_string(),
                disposition: cols[1].trim().to_string(),
                reason: cols[2..].join("\t").trim().to_string(),
            }
        })
        .collect()
}

fn first_level_dirs(root: &Path) -> BTreeSet<String> {
    let dir = root.join("tests/fixtures");
    fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("read {}: {e}", dir.display()))
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .collect()
}

/// Every `todo/tNNNN` token appearing in a string. The manifest defers real
/// corpus candidates by citing the item that owns the work; a deferral pointing
/// at an item that does not exist is a dangling promise, which is the failure
/// mode `todo/t0875` catalogues (a textual guard's reach is the TOKEN, so make
/// the token mean something).
pub fn cited_todo_ids(text: &str) -> Vec<String> {
    let bytes = text.as_bytes();
    let mut out = Vec::new();
    let mut i = 0usize;
    while let Some(rel) = text[i..].find("todo/t") {
        let pos = i + rel + "todo/".len();
        let digits: String = bytes[pos + 1..]
            .iter()
            .take_while(|b| b.is_ascii_digit())
            .map(|b| *b as char)
            .collect();
        if digits.len() == 4 {
            out.push(format!("t{digits}"));
        }
        i = pos + 1;
    }
    out.sort();
    out.dedup();
    out
}

fn assert_set_eq(label: &str, left: &BTreeSet<String>, right: &BTreeSet<String>) {
    if left != right {
        let only_left: Vec<&String> = left.difference(right).collect();
        let only_right: Vec<&String> = right.difference(left).collect();
        panic!(
            "{label} drifted.\n  only in the manifest ({}): {:?}\n  only on disk ({}): {:?}",
            only_left.len(),
            only_left,
            only_right.len(),
            only_right
        );
    }
}

/// A THROWAWAY repo root holding `tests/fixtures/<name>/` and nothing else.
///
/// The self-probes below claim a property of `first_level_dirs` + set difference,
/// so they run against a synthetic root and touch no shared state. Writing a
/// probe directory into the LIVE `tests/fixtures/` would race every other test in
/// this binary and leave a stray directory behind whenever the process died
/// inside the window — the mistake `ggdef_corpus_membership_lint.rs` records.
fn probe_root(name: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("probe tempdir");
    let d = dir.path().join("tests/fixtures").join(name);
    fs::create_dir_all(&d).unwrap_or_else(|e| panic!("create {}: {e}", d.display()));
    fs::write(d.join("probe.gg"), "void main():\n    print(1)\n").expect("write probe fixture");
    dir
}

#[test]
fn sanitize_corpus_manifest_is_declared() {
    let root = repo_root();
    let sweep = fs::read_to_string(root.join("scripts/sanitize_sweep.sh"))
        .expect("read scripts/sanitize_sweep.sh");

    // (a) THE SWEEP ACTUALLY CONSULTS THE MANIFEST. Parsed from source, so a
    //     revert to a bare `find … -maxdepth 1` reds here rather than silently
    //     un-declaring the corpus. `$2 == "IN"` is the selection predicate; the
    //     completeness abort is what makes an undeclared directory fatal.
    let rel = manifest_path_from_sweep(&sweep);
    assert_eq!(
        rel, "tests/sanitize/CORPUS_MANIFEST.txt",
        "the sweep's MANIFEST default moved to {rel:?}; point this lint at the \
         same file or the two check different things"
    );
    for needle in [
        "corpus_paths()",
        "$2 == \"IN\"",
        "the swept corpus is not fully declared",
    ] {
        assert!(
            sweep.contains(needle),
            "scripts/sanitize_sweep.sh no longer contains {needle:?} — the corpus \
             is not being taken from the manifest any more. A depth-limited walk \
             is the RIGHT SET for the WRONG REASON: it cannot say why any \
             directory is unwatched, and a new one joins the unwatched set in \
             silence."
        );
    }

    let manifest_file = root.join(&rel);
    let body = fs::read_to_string(&manifest_file)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", manifest_file.display()));
    let rows = parse_manifest(&body);

    // (b) ONE ROW PER DIRECTORY, no duplicates — a second row for the same
    //     directory would let two dispositions disagree.
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for r in &rows {
        assert!(
            seen.insert(r.dir.clone()),
            "corpus manifest has two rows for {:?} — one row per directory",
            r.dir
        );
    }

    // (c) THE CENSUS SET-EQUALS DISK. A new directory with no row REDS; a row
    //     naming a deleted directory REDS.
    let disk = first_level_dirs(&root);
    assert_set_eq("corpus manifest directory census vs tests/fixtures/", &seen, &disk);

    // (d) EVERY ROW DECIDES, AND EVERY `OUT` SAYS WHY. A bare name is how a
    //     manifest turns into a parking lot (Core #14: an assertion with no
    //     enforcing guard is rot). 40 chars is the bar
    //     `CORRUPTION_ALLOWLIST.txt`'s justification column already sets.
    for r in &rows {
        assert!(
            r.disposition == "IN" || r.disposition == "OUT",
            "corpus manifest row {:?} has disposition {:?}; it must be IN or OUT",
            r.dir,
            r.disposition
        );
        if r.disposition == "OUT" {
            assert!(
                r.reason.len() > 40,
                "corpus manifest row {:?} is OUT with no real reason: {:?}\n\
                 Say what makes it unsweepable (no binary is produced, no \
                 standalone program, it holds the gate's own controls) or, if it \
                 is a real corpus candidate, cite the todo/ item that owns \
                 folding it in.",
                r.dir,
                r.reason
            );
        }
    }

    // (e) A DEFERRAL POINTS AT A LIVE ITEM. The manifest queues real corpus
    //     candidates on `todo/` items; a citation to an item that does not exist
    //     is a promise nothing can collect on.
    let mut dangling: Vec<String> = Vec::new();
    for r in &rows {
        for id in cited_todo_ids(&r.reason) {
            if !root.join("todo").join(format!("{id}.md")).is_file() {
                dangling.push(format!("{} -> todo/{id}.md", r.dir));
            }
        }
    }
    assert!(
        dangling.is_empty(),
        "corpus manifest rows cite todo/ items that do not exist: {dangling:?}\n\
         Either the item was closed (say so in the reason and re-decide the row) \
         or the citation is a typo."
    );

    // (f) THE ROW THAT MUST NEVER FLIP. `sanitize_selftest/` holds the gate's own
    //     positive controls: one leaks by design and one alternates by design, so
    //     sweeping them reports the gate's controls as findings and destroys the
    //     Core #13 self-test the sweep runs before any corpus verdict.
    let selftest = rows
        .iter()
        .find(|r| r.dir == "sanitize_selftest")
        .expect("no sanitize_selftest row in the corpus manifest");
    assert_eq!(
        selftest.disposition, "OUT",
        "sanitize_selftest/ is IN. Those four fixtures are the sweep's own \
         positive controls — one LEAKS BY DESIGN and one ALTERNATES BY DESIGN. \
         Ingesting them makes the gate report its own controls as findings and \
         removes the self-test that has to fire before any corpus verdict is \
         trustworthy (docs/devbook/25-structural-guards.md)."
    );

    // (g) CORE #13 — the guard, seen to fail, on a synthetic root.
    {
        let probe = probe_root("ta1_undeclared_probe");
        let disk2 = first_level_dirs(probe.path());
        assert!(
            disk2.difference(&seen).next().is_some(),
            "a directory with NO manifest row did not redden the census"
        );
    }
    {
        let mut stale = seen.clone();
        stale.insert("ta1_deleted_directory_row".into());
        assert_ne!(
            stale, disk,
            "a manifest row naming a directory that does not exist did not redden \
             the census"
        );
    }
    {
        let bogus = parse_manifest("ta1_probe\tMAYBE\tsome reason that is definitely long enough to clear the bar\n");
        assert_eq!(bogus.len(), 1);
        assert!(
            bogus[0].disposition != "IN" && bogus[0].disposition != "OUT",
            "the disposition check would accept a third value"
        );
    }
    {
        // The plausible EVASION, not merely the deletion: a reason that CITES an
        // item which does not exist. `todo/t0875` records that a textual guard's
        // reach is the TOKEN and never the CONCEPT, so the check must resolve the
        // token against the tree.
        let evasive = parse_manifest(
            "ta1_probe\tOUT\tdeferred, queued on todo/t0000 which is a plausible-looking id nobody filed\n",
        );
        let ids = cited_todo_ids(&evasive[0].reason);
        assert_eq!(ids, vec!["t0000".to_string()], "citation scan missed the token");
        assert!(
            !root.join("todo/t0000.md").is_file(),
            "todo/t0000.md now exists — pick another non-existent id for this probe"
        );
    }
}
