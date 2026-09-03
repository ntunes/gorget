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

/// The first-level directories under `tests/fixtures/` that GIT considers part of
/// this working tree, which is NOT the same set a disk walk returns.
///
/// ⚠ THIS LINT SHIPPED WITH A DISK WALK AND IT WAS WRONG, in the worst direction
/// a guard can be wrong: it was GREEN WHERE NOTHING HAD HAPPENED AND RED WHERE
/// WORK DID. `tests/fixtures/.gorget/` is a directory of `*.test-results.json`
/// files that running the fixture suite creates, so the census matched disk in a
/// fresh worktree and drifted on every machine — and every CI job — that had
/// actually run the tests.
///
/// ⚠ AND `git check-ignore` IS THE WRONG INSTRUMENT, measured rather than
/// assumed: `tests/fixtures/.gitignore` is deny-all plus an extension allowlist,
/// and its line 19 `!*/` UN-IGNORES every directory so the file rules can match
/// inside them. `git check-ignore -q tests/fixtures/.gorget` therefore exits 1 —
/// NOT ignored — while the file inside it exits 0. An ignore-based filter
/// excludes nothing here.
///
/// ⚠ AND TRACKED-NESS ALONE IS ALSO WRONG: a stray directory of real `.gg`
/// fixtures nobody has `git add`ed has no tracked files either, and that one MUST
/// still red — it is exactly the undeclared corpus this manifest exists to catch.
///
/// `--cached --others --exclude-standard` is the union that separates them: it
/// lists TRACKED files plus UNTRACKED-BUT-NOT-IGNORED ones, so a directory
/// appears iff it holds at least one file git would consider part of this tree.
/// `.gorget` contributes none and vanishes; the stray contributes its `.gg` and
/// stays. Verified both ways in the Core #13 probes at the end of this test.
///
/// ⚠ RESIDUAL, stated rather than papered over: a directory holding ONLY files
/// the fixtures allowlist drops — or no files at all — is invisible here. `.gg`
/// is allowlisted, so no real fixture directory can hide; but that IS the same
/// silent-skip hazard `tests/fixtures/.gitignore`'s own header warns about at
/// length, one level up.
fn first_level_dirs(root: &Path) -> BTreeSet<String> {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(root)
        .args(["ls-files", "--cached", "--others", "--exclude-standard", "tests/fixtures"])
        .output()
        .unwrap_or_else(|e| {
            panic!(
                "cannot run `git ls-files` in {}: {e}\n\
                 The corpus population is defined by git, not by a disk walk — a \
                 walk cannot tell a fixture directory from a build artefact like \
                 tests/fixtures/.gorget/.",
                root.display()
            )
        });
    assert!(
        out.status.success(),
        "`git ls-files` failed in {}: {}",
        root.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    let listing = String::from_utf8_lossy(&out.stdout);
    let dirs: BTreeSet<String> = listing
        .lines()
        .filter_map(|l| {
            // `tests/fixtures/<dir>/…` — anything shallower is a loose fixture
            // file at the top level, which the manifest does not enumerate.
            let rest = l.strip_prefix("tests/fixtures/")?;
            let (first, tail) = rest.split_once('/')?;
            (!tail.is_empty()).then(|| first.to_string())
        })
        .collect();
    assert!(
        !dirs.is_empty(),
        "`git ls-files` returned no directories under tests/fixtures/ in {} — \
         the census would be vacuously satisfied, so this is an instrument \
         failure, not an empty corpus.",
        root.display()
    );
    dirs
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

/// A THROWAWAY GIT REPO holding a real fixture directory AND a build-artefact
/// directory, so the probes can prove the census DISCRIMINATES them.
///
/// ⚠ IT IS A GIT REPO, NOT A BARE TEMPDIR, AND THAT IS THE POINT. The census is
/// now `git ls-files --cached --others --exclude-standard`, so a probe root with
/// no git in it cannot exercise the property at all — and the earlier bare-dir
/// probe is exactly why the `.gorget` defect shipped: it proved set-difference
/// arithmetic while the real question was WHICH DIRECTORIES ENTER THE SET.
///
/// The `.gitignore` written here mirrors the real one's shape — deny-all, `!*/`
/// to un-ignore directories, then an extension allowlist — because that shape is
/// what makes `git check-ignore` useless on the directory itself.
///
/// Synthetic root, never the LIVE `tests/fixtures/`: writing a probe directory
/// there would race every other test in this binary and leave a stray behind
/// whenever the process died inside the window (the mistake
/// `ggdef_corpus_membership_lint.rs` records).
fn probe_root(real: &str, artefact: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("probe tempdir");
    let fixtures = dir.path().join("tests/fixtures");
    fs::create_dir_all(&fixtures).expect("create probe fixtures dir");
    fs::write(
        fixtures.join(".gitignore"),
        "*\n!*/\n!**/*.gg\n!.gitignore\n",
    )
    .expect("write probe gitignore");

    let real_dir = fixtures.join(real);
    fs::create_dir_all(&real_dir).expect("create probe real dir");
    fs::write(real_dir.join("probe.gg"), "void main():\n    print(1)\n")
        .expect("write probe fixture");

    // The `.gorget` analogue: a directory whose every file the allowlist drops.
    let art_dir = fixtures.join(artefact);
    fs::create_dir_all(&art_dir).expect("create probe artefact dir");
    fs::write(art_dir.join("probe.test-results.json"), "{}\n")
        .expect("write probe artefact");

    let git = |args: &[&str]| {
        let st = std::process::Command::new("git")
            .arg("-C")
            .arg(dir.path())
            .args(args)
            .output()
            .expect("probe git");
        assert!(st.status.success(), "probe `git {args:?}` failed");
    };
    git(&["init", "-q"]);
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
        // ONE SOURCE OF TRUTH FOR THE POPULATION (Layering rule 3). The sweep and
        // this lint must enumerate the SAME directories, or one of them polices a
        // set the other does not have. Both ask git the same question; parsing the
        // sweep's spelling of it here is what stops them drifting back apart —
        // and a revert to a bare `find … -type d` reds right here, which is how
        // the `.gorget` defect would have been caught before it shipped.
        "git ls-files --cached --others --exclude-standard tests/fixtures",
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

    // (g) CORE #13 — the guard, seen to fail, on a synthetic root, AND seen NOT
    //     to fail on the artefact. Both halves, because the defect this probe
    //     replaces was a census that reddened on a build artefact: a guard that
    //     fires on the wrong thing is as broken as one that never fires, and it
    //     is worse, because it fires only where somebody has done work.
    {
        let probe = probe_root("ta1_undeclared_probe", ".ta1_artefact");
        let disk2 = first_level_dirs(probe.path());
        assert!(
            disk2.contains("ta1_undeclared_probe"),
            "a real fixture directory that nobody has `git add`ed vanished from \
             the census — tracked-ness is NOT the population, an untracked \
             directory of `.gg` files is precisely what must still red"
        );
        assert!(
            !disk2.contains(".ta1_artefact"),
            "a build-artefact directory (every file dropped by the fixtures \
             allowlist, like tests/fixtures/.gorget/) entered the census. That is \
             the defect this lint shipped with: green in a fresh checkout, red on \
             every machine that had actually run the tests. Got: {disk2:?}"
        );
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
