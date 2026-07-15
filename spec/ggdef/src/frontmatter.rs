//! The spectest frontmatter READER — the inverse of `gen`'s writer (`lib.rs`).
//!
//! `gen_frontmatter` (`lib.rs`) WRITES the `#!spectest … #!end` block; this is
//! the FIRST and ONLY reader that parses it back. All four run-tier conformance
//! lanes (ggdef, C, LLVM, self-host) share this ONE reader, so a verdict is
//! never computed against a differently-parsed expectation.
//!
//! The block lives entirely in leading `#` comments (invisible to the lexer, so
//! `run`/`gen` work unchanged). Shape:
//!
//! ```text
//! #!spectest
//! # mode: run
//! # adjudicator: ggdef
//! # since: spec-v0.1
//! # features: [cow, alias, sever]
//! # doc: |
//! #   Free prose. Consumed BY INDENTATION — a body line that happens to read
//! #   `mode: x` is prose, NEVER a key.
//! # expect:
//! #   exit: 0
//! #   stdout: "1\n99\n"
//! #!end
//! ```
//!
//! Design rules (P1-B brief):
//!   * `doc: |` is a block scalar — its body is consumed by indentation and is
//!     NEVER scanned for keys.
//!   * Unknown INLINE top-level keys (forward-compat: `args`, a scalar `stdin`…)
//!     are preserved-and-ignored, never an error. LIMIT (filed in TODO): an
//!     unknown key with a nested/`|`-block value currently errors LOUDLY
//!     (`MalformedKey`) — block-shaped future keys (`files:`, RFC §4) need the
//!     `other` arm to consume-and-discard indented children.
//!   * `expect:` is REQUIRED with nested `exit` (int) + `stdout` (a JSON-escaped
//!     string, the exact inverse of the writer's `json_escape`). Malformed
//!     frontmatter (missing/short `expect`, non-int `exit`, bad escape) is a
//!     LOUD, specific error — this reader feeds conformance verdicts, so a
//!     silent mis-parse would corrupt them.

use crate::{FENCE_CLOSE, FENCE_OPEN};

/// The nested `expect:` outcome a run-tier fixture is checked against.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expect {
    /// The expected process exit code (`Outcome::exit_code()`).
    pub exit: i32,
    /// The expected stdout, with the writer's JSON escapes already decoded.
    pub stdout: String,
    /// The expected trap `T_` code (`TrapKind::code()`), present IFF the outcome
    /// is a Trap (`exit == 101`). The reader enforces that biconditional.
    pub trap: Option<String>,
    /// The expected may-move rejection `E_` code (`MoveErrorKind::code()`),
    /// present IFF the outcome is a static rejection (`exit == 1`). The reader
    /// enforces that biconditional — symmetric with `trap`. This is the
    /// conformance-compared axis of a rejection (pin 3); prose + span stay
    /// impl-defined.
    pub reject: Option<String>,
}

/// A parsed `#!spectest` frontmatter block. A successful parse ALWAYS carries an
/// `expect` (the reader rejects a fixture without one), so downstream lanes can
/// read `fm.expect` unconditionally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frontmatter {
    /// `mode:` — the tier (`run`, later `static-error`/`parse-error`…). Optional
    /// on the struct; the run lanes are selected by the `spectests/run/`
    /// directory, not by re-checking this.
    pub mode: Option<String>,
    /// `adjudicator:` — who owns the expectation (`ggdef` / `production-v1`).
    pub adjudicator: Option<String>,
    /// `since:` — the spec version the fixture was introduced at.
    pub since: Option<String>,
    /// `features:` — the inline `[..]` feature tag list (may be empty).
    pub features: Vec<String>,
    /// `doc: |` — the free-prose block-scalar body (dedented), if present.
    pub doc: Option<String>,
    /// The required `expect:` block.
    pub expect: Expect,
    /// Unknown INLINE top-level keys, preserved verbatim `(key, raw_value)`
    /// for forward-compat (`args`, scalar `stdin`…). Block-shaped unknown keys
    /// currently error loudly — see the module-doc LIMIT note.
    pub unknown: Vec<(String, String)>,
}

/// A LOUD, specific frontmatter parse failure. Every variant names exactly what
/// is wrong so a corrupt seed cannot silently skew a conformance verdict.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrontmatterError {
    /// No `#!spectest` … `#!end` fence at all.
    NoFence,
    /// A fence that is present but broken (open without close, or close-before-open).
    MalformedFence(String),
    /// A non-blank body line that is not a `#` comment (real code inside the fence).
    NonCommentLine(String),
    /// A top-level line that is not `key: value` (or an unexpected indented line).
    MalformedKey(String),
    /// No `expect:` block — a run-tier fixture MUST declare its expectation.
    MissingExpect,
    /// `expect:` present but missing a required nested field (`exit` / `stdout`).
    MissingExpectField(&'static str),
    /// `expect.exit` was not an integer.
    BadExit(String),
    /// `expect.stdout` was not a well-formed JSON string literal (bad escape,
    /// unquoted, unterminated…).
    BadStdout(String),
    /// `features:` was not an inline `[..]` list.
    BadFeatures(String),
    /// A `trap:` code was present but `exit` was not 101 — a trap is EXACTLY the
    /// exit-101 outcome (`{code, exit}` field of the trap contract).
    TrapCodeNonTrapExit { code: String, exit: i32 },
    /// `exit: 101` (a trap) but no `trap:` code — an uncaught trap MUST carry its
    /// `T_` code so conformance can compare it.
    TrapExitWithoutCode,
    /// A `reject:` code was present but `exit` was not 1 — a static rejection is
    /// EXACTLY the exit-1 outcome (symmetric with the trap contract).
    RejectCodeNonRejectExit { code: String, exit: i32 },
    /// `exit: 1` (a static rejection) but no `reject:` code — a rejection MUST
    /// carry its `E_` code so conformance can compare it (pin 3).
    RejectExitWithoutCode,
}

impl std::fmt::Display for FrontmatterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FrontmatterError::NoFence => write!(
                f,
                "no `{FENCE_OPEN}` … `{FENCE_CLOSE}` frontmatter fence — not a spectest fixture"
            ),
            FrontmatterError::MalformedFence(why) => write!(f, "malformed frontmatter fence: {why}"),
            FrontmatterError::NonCommentLine(l) => {
                write!(f, "non-comment line inside the frontmatter fence: {l:?}")
            }
            FrontmatterError::MalformedKey(l) => write!(f, "malformed frontmatter line: {l}"),
            FrontmatterError::MissingExpect => {
                write!(f, "frontmatter has no `expect:` block (a run-tier fixture must declare one)")
            }
            FrontmatterError::MissingExpectField(k) => {
                write!(f, "`expect:` block is missing required field `{k}:`")
            }
            FrontmatterError::BadExit(v) => write!(f, "`expect.exit` is not an integer: {v}"),
            FrontmatterError::BadStdout(why) => write!(f, "`expect.stdout` is not a valid JSON string: {why}"),
            FrontmatterError::BadFeatures(v) => {
                write!(f, "`features:` must be an inline `[..]` list, got {v}")
            }
            FrontmatterError::TrapCodeNonTrapExit { code, exit } => write!(
                f,
                "`expect.trap: {code}` requires `exit: 101` (a trap is exactly the exit-101 \
                 outcome), but `exit` is {exit}"
            ),
            FrontmatterError::TrapExitWithoutCode => write!(
                f,
                "`expect.exit: 101` is a trap but no `expect.trap:` `T_` code is declared"
            ),
            FrontmatterError::RejectCodeNonRejectExit { code, exit } => write!(
                f,
                "`expect.reject: {code}` requires `exit: 1` (a static rejection is exactly the \
                 exit-1 outcome), but `exit` is {exit}"
            ),
            FrontmatterError::RejectExitWithoutCode => write!(
                f,
                "`expect.exit: 1` is a static rejection but no `expect.reject:` `E_` code is declared"
            ),
        }
    }
}

impl std::error::Error for FrontmatterError {}

/// Parse the `#!spectest` frontmatter of a spectest source. See the module docs
/// for the shape and the error contract.
pub fn parse_frontmatter(source: &str) -> Result<Frontmatter, FrontmatterError> {
    let body = fence_body(source)?;

    let mut mode = None;
    let mut adjudicator = None;
    let mut since = None;
    let mut features = Vec::new();
    let mut doc = None;
    let mut expect_exit = None;
    let mut expect_stdout = None;
    let mut expect_trap = None;
    let mut expect_reject = None;
    let mut saw_expect = false;
    let mut unknown = Vec::new();

    let mut i = 0;
    while i < body.len() {
        let line = &body[i];
        if line.trim().is_empty() {
            i += 1;
            continue;
        }
        if indent_of(line) != 0 {
            // A block key (`doc`/`expect`) consumes its own indented body; any
            // other indented line is orphaned → malformed.
            return Err(FrontmatterError::MalformedKey(format!(
                "unexpected indented line with no parent key: {:?}",
                line.trim()
            )));
        }
        let (key, rest) = split_key(line.trim())?;
        match key {
            "doc" => {
                let (block, next) = consume_block(&body, i + 1);
                doc = Some(if block.is_empty() && is_inline_doc(rest) {
                    rest.to_string()
                } else {
                    block
                });
                i = next;
                continue;
            }
            "expect" => {
                saw_expect = true;
                let (children, next) = consume_children(&body, i + 1);
                for child in &children {
                    let (ck, cv) = split_key(child.trim())?;
                    match ck {
                        "exit" => {
                            expect_exit = Some(
                                cv.parse::<i32>()
                                    .map_err(|_| FrontmatterError::BadExit(cv.to_string()))?,
                            );
                        }
                        "stdout" => expect_stdout = Some(parse_json_string(cv)?),
                        "trap" => expect_trap = Some(cv.to_string()),
                        "reject" => expect_reject = Some(cv.to_string()),
                        // Tolerate unknown nested keys (forward-compat).
                        _ => {}
                    }
                }
                i = next;
                continue;
            }
            "mode" => mode = Some(rest.to_string()),
            "adjudicator" => adjudicator = Some(rest.to_string()),
            "since" => since = Some(rest.to_string()),
            "features" => features = parse_features(rest)?,
            other => unknown.push((other.to_string(), rest.to_string())),
        }
        i += 1;
    }

    if !saw_expect {
        return Err(FrontmatterError::MissingExpect);
    }
    let exit = expect_exit.ok_or(FrontmatterError::MissingExpectField("exit"))?;
    let stdout = expect_stdout.ok_or(FrontmatterError::MissingExpectField("stdout"))?;

    // Enforce `trap: present ⟺ exit == 101` — a trap is exactly the exit-101
    // outcome. The one remaining non-trap nonzero exit (103 FuelExhausted)
    // legitimately carries no code, so the rule keys on `== 101`, not `!= 0`.
    match (&expect_trap, exit) {
        (Some(_), 101) => {}
        (None, 101) => return Err(FrontmatterError::TrapExitWithoutCode),
        (Some(code), _) => {
            return Err(FrontmatterError::TrapCodeNonTrapExit { code: code.clone(), exit });
        }
        (None, _) => {}
    }

    // Enforce `reject: present ⟺ exit == 1` — symmetric with the trap rule. A
    // static rejection is exactly the exit-1 outcome (a may-move `IllFormed`;
    // ggdef's `Value` is always exit 0, so exit 1 uniquely identifies a reject).
    // The `E_` code is the conformance-compared axis (pin 3), so a reject fixture
    // MUST declare it and a non-reject exit MUST NOT.
    match (&expect_reject, exit) {
        (Some(_), 1) => {}
        (None, 1) => return Err(FrontmatterError::RejectExitWithoutCode),
        (Some(code), _) => {
            return Err(FrontmatterError::RejectCodeNonRejectExit { code: code.clone(), exit });
        }
        (None, _) => {}
    }

    Ok(Frontmatter {
        mode,
        adjudicator,
        since,
        features,
        doc,
        expect: Expect { exit, stdout, trap: expect_trap, reject: expect_reject },
        unknown,
    })
}

/// Return the normalized (comment-stripped) content lines strictly between the
/// fences. Each returned line has its `#` marker and the ONE conventional space
/// after it removed; any further leading spaces are significant indentation.
fn fence_body(source: &str) -> Result<Vec<String>, FrontmatterError> {
    let lines: Vec<&str> = source.lines().collect();
    let open = lines.iter().position(|l| l.trim() == FENCE_OPEN);
    let close = lines.iter().position(|l| l.trim() == FENCE_CLOSE);
    let (open, close) = match (open, close) {
        (Some(o), Some(c)) if o < c => (o, c),
        (Some(_), Some(_)) => {
            return Err(FrontmatterError::MalformedFence(format!(
                "`{FENCE_CLOSE}` appears before `{FENCE_OPEN}`"
            )))
        }
        (Some(_), None) => {
            return Err(FrontmatterError::MalformedFence(format!(
                "`{FENCE_OPEN}` has no closing `{FENCE_CLOSE}`"
            )))
        }
        (None, _) => return Err(FrontmatterError::NoFence),
    };

    let mut out = Vec::with_capacity(close - open);
    for raw in &lines[open + 1..close] {
        if raw.trim().is_empty() {
            out.push(String::new());
            continue;
        }
        let after_hash = raw
            .trim_start()
            .strip_prefix('#')
            .ok_or_else(|| FrontmatterError::NonCommentLine(raw.trim().to_string()))?;
        // Strip the single conventional space of the "# " marker; keep the rest.
        out.push(after_hash.strip_prefix(' ').unwrap_or(after_hash).to_string());
    }
    Ok(out)
}

/// Count of significant leading spaces of a normalized content line.
fn indent_of(content: &str) -> usize {
    content.len() - content.trim_start().len()
}

/// Split a `key: value` line on its FIRST colon (so a colon inside a quoted
/// value survives in `value`). A line with no colon is malformed.
fn split_key(line: &str) -> Result<(&str, &str), FrontmatterError> {
    match line.split_once(':') {
        Some((k, v)) => Ok((k.trim(), v.trim())),
        None => Err(FrontmatterError::MalformedKey(format!("expected `key: value`, got {line:?}"))),
    }
}

/// Consume a block-scalar body starting at `start`: all following lines that are
/// blank OR indented past the (indent-0) parent key. Returns the dedented,
/// newline-joined text and the index of the first line NOT in the block. Purely
/// indentation-driven — the body is never inspected for keys.
fn consume_block(body: &[String], start: usize) -> (String, usize) {
    let mut j = start;
    let mut raw: Vec<&str> = Vec::new();
    while j < body.len() {
        let line = &body[j];
        if line.trim().is_empty() {
            raw.push("");
            j += 1;
        } else if indent_of(line) > 0 {
            raw.push(line);
            j += 1;
        } else {
            break; // dedent back to a top-level key ends the block
        }
    }
    // Drop trailing blank lines (leading/interior blanks are body content).
    while raw.last().is_some_and(|l| l.is_empty()) {
        raw.pop();
    }
    let min_indent =
        raw.iter().filter(|l| !l.is_empty()).map(|l| indent_of(l)).min().unwrap_or(0);
    let dedented: Vec<String> = raw
        .iter()
        .map(|l| if l.is_empty() { String::new() } else { l[min_indent..].to_string() })
        .collect();
    (dedented.join("\n"), j)
}

/// Consume the indented children of a block key (`expect:`) starting at `start`:
/// all following indented lines (blanks skipped). Returns them and the index of
/// the first dedented (top-level) line.
fn consume_children(body: &[String], start: usize) -> (Vec<String>, usize) {
    let mut j = start;
    let mut kids = Vec::new();
    while j < body.len() {
        let line = &body[j];
        if line.trim().is_empty() {
            j += 1;
            continue;
        }
        if indent_of(line) > 0 {
            kids.push(line.clone());
            j += 1;
        } else {
            break;
        }
    }
    (kids, j)
}

/// Whether a `doc:` inline value is real prose (not just a block-scalar
/// indicator like `|` / `>` / `|-`).
fn is_inline_doc(rest: &str) -> bool {
    !rest.is_empty() && !matches!(rest, "|" | ">" | "|-" | ">-" | "|+" | ">+")
}

/// Parse an inline `[a, b, c]` list into trimmed, non-empty items.
fn parse_features(rest: &str) -> Result<Vec<String>, FrontmatterError> {
    let inner = rest
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .ok_or_else(|| FrontmatterError::BadFeatures(format!("{rest:?}")))?;
    Ok(inner.split(',').map(str::trim).filter(|s| !s.is_empty()).map(str::to_string).collect())
}

/// Decode a JSON string literal — the EXACT inverse of the writer's
/// `json_escape` (`lib.rs`). Strict on purpose: the writer only emits
/// `\" \\ \n \r \t`, so any other escape (or an unquoted / unterminated value)
/// is a LOUD error rather than a lenient best-effort decode that could mask a
/// corrupt expectation.
pub(crate) fn parse_json_string(raw: &str) -> Result<String, FrontmatterError> {
    let bytes = raw.as_bytes();
    if bytes.len() < 2 || bytes[0] != b'"' || bytes[bytes.len() - 1] != b'"' {
        return Err(FrontmatterError::BadStdout(format!("expected a double-quoted string, got {raw}")));
    }
    let inner = &raw[1..raw.len() - 1];
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('"') => out.push('"'),
                Some('\\') => out.push('\\'),
                Some(other) => {
                    return Err(FrontmatterError::BadStdout(format!("unknown escape `\\{other}`")))
                }
                None => return Err(FrontmatterError::BadStdout("trailing backslash".to_string())),
            },
            '"' => {
                return Err(FrontmatterError::BadStdout(
                    "unescaped `\"` inside the string body".to_string(),
                ))
            }
            c => out.push(c),
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{run_source, DEFAULT_FUEL};
    use std::fs;
    use std::path::{Path, PathBuf};

    fn run_dir() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..").join("spectests/run")
    }

    fn run_seeds() -> Vec<PathBuf> {
        let mut v: Vec<PathBuf> = fs::read_dir(run_dir())
            .expect("read spectests/run")
            .filter_map(|e| {
                let p = e.unwrap().path();
                (p.extension().and_then(|x| x.to_str()) == Some("gg")).then_some(p)
            })
            .collect();
        v.sort();
        v
    }

    // ── all committed seeds parse cleanly ──────────────────────────────────
    #[test]
    fn all_committed_seeds_parse() {
        let seeds = run_seeds();
        assert!(seeds.len() >= 5, "expected >=5 committed run seeds, found {}", seeds.len());
        for seed in &seeds {
            let src = fs::read_to_string(seed).unwrap();
            let fm = parse_frontmatter(&src)
                .unwrap_or_else(|e| panic!("{}: {e}", seed.file_name().unwrap().to_string_lossy()));
            assert_eq!(fm.mode.as_deref(), Some("run"), "{seed:?}");
            assert_eq!(fm.adjudicator.as_deref(), Some("ggdef"), "{seed:?}");
        }
    }

    // ── writer ⇄ reader agreement: the parsed expect equals a fresh run ─────
    #[test]
    fn reader_agrees_with_writer_over_all_seeds() {
        for seed in &run_seeds() {
            let src = fs::read_to_string(seed).unwrap();
            let fm = parse_frontmatter(&src).unwrap();
            let run = run_source(&src, DEFAULT_FUEL).unwrap();
            let name = seed.file_name().unwrap().to_string_lossy();
            assert_eq!(
                fm.expect.exit,
                run.outcome.exit_code(),
                "{name}: parsed expect.exit disagrees with a fresh ggdef run",
            );
            assert_eq!(
                fm.expect.stdout, run.stdout,
                "{name}: parsed expect.stdout disagrees with a fresh ggdef run",
            );
        }
    }

    // ── a full hand-written block parses every field ───────────────────────
    #[test]
    fn parses_every_field() {
        let src = "#!spectest\n\
                   # mode: run\n\
                   # adjudicator: ggdef\n\
                   # since: spec-v0.1\n\
                   # features: [cow, alias, sever]\n\
                   # doc: |\n\
                   #   line one\n\
                   #   line two\n\
                   # expect:\n\
                   #   exit: 0\n\
                   #   stdout: \"1\\n99\\n\"\n\
                   #!end\n";
        let fm = parse_frontmatter(src).unwrap();
        assert_eq!(fm.mode.as_deref(), Some("run"));
        assert_eq!(fm.adjudicator.as_deref(), Some("ggdef"));
        assert_eq!(fm.since.as_deref(), Some("spec-v0.1"));
        assert_eq!(fm.features, vec!["cow", "alias", "sever"]);
        assert_eq!(fm.doc.as_deref(), Some("line one\nline two"));
        assert_eq!(
            fm.expect,
            Expect { exit: 0, stdout: "1\n99\n".to_string(), trap: None, reject: None }
        );
        assert!(fm.unknown.is_empty());
    }

    #[test]
    fn empty_and_absent_feature_list() {
        let src = "#!spectest\n# features: []\n# expect:\n#   exit: 0\n#   stdout: \"\"\n#!end\n";
        let fm = parse_frontmatter(src).unwrap();
        assert!(fm.features.is_empty());
        assert_eq!(fm.expect.stdout, "");
    }

    // ── forward-compat: unknown top-level AND nested keys are tolerated ─────
    #[test]
    fn tolerates_unknown_keys() {
        let src = "#!spectest\n\
                   # mode: run\n\
                   # args: [--verbose, 3]\n\
                   # stdin: \"payload\"\n\
                   # nondet: seeded\n\
                   # expect:\n\
                   #   exit: 0\n\
                   #   stdout: \"ok\\n\"\n\
                   #   duration_ms: 12\n\
                   #!end\n";
        let fm = parse_frontmatter(src).expect("unknown keys must be tolerated, not error");
        assert_eq!(
            fm.expect,
            Expect { exit: 0, stdout: "ok\n".to_string(), trap: None, reject: None }
        );
        // Unknown top-level keys are preserved, not dropped.
        let keys: Vec<&str> = fm.unknown.iter().map(|(k, _)| k.as_str()).collect();
        assert!(keys.contains(&"args"), "unknown keys: {keys:?}");
        assert!(keys.contains(&"stdin"), "unknown keys: {keys:?}");
        assert!(keys.contains(&"nondet"), "unknown keys: {keys:?}");
    }

    // ── block-scalar edge: a doc body line reading `mode:` is PROSE ─────────
    #[test]
    fn doc_block_body_is_never_scanned_for_keys() {
        let src = "#!spectest\n\
                   # mode: run\n\
                   # doc: |\n\
                   #   This mentions mode: SHOULD_NOT_WIN and adjudicator: nope\n\
                   #   and a second prose line.\n\
                   # expect:\n\
                   #   exit: 0\n\
                   #   stdout: \"x\\n\"\n\
                   #!end\n";
        let fm = parse_frontmatter(src).unwrap();
        // The doc body must NOT have overridden the real top-level keys.
        assert_eq!(fm.mode.as_deref(), Some("run"), "doc body leaked into `mode`");
        assert_eq!(fm.adjudicator, None, "doc body leaked into `adjudicator`");
        // The prose (including the literal `mode:` text) is captured as doc.
        let doc = fm.doc.expect("doc present");
        assert!(doc.contains("mode: SHOULD_NOT_WIN"), "doc body lost: {doc:?}");
    }

    // ── malformed input is LOUD and specific ───────────────────────────────
    #[test]
    fn no_fence_is_an_error() {
        assert_eq!(parse_frontmatter("void main():\n    print(1)\n"), Err(FrontmatterError::NoFence));
    }

    #[test]
    fn missing_expect_is_an_error() {
        let src = "#!spectest\n# mode: run\n#!end\n";
        assert_eq!(parse_frontmatter(src), Err(FrontmatterError::MissingExpect));
    }

    #[test]
    fn missing_expect_field_is_an_error() {
        let src = "#!spectest\n# expect:\n#   exit: 0\n#!end\n";
        assert_eq!(parse_frontmatter(src), Err(FrontmatterError::MissingExpectField("stdout")));
    }

    #[test]
    fn non_int_exit_is_an_error() {
        let src = "#!spectest\n# expect:\n#   exit: banana\n#   stdout: \"x\"\n#!end\n";
        assert_eq!(parse_frontmatter(src), Err(FrontmatterError::BadExit("banana".to_string())));
    }

    #[test]
    fn bad_stdout_escape_is_an_error() {
        let src = "#!spectest\n# expect:\n#   exit: 0\n#   stdout: \"bad\\q\"\n#!end\n";
        assert!(matches!(parse_frontmatter(src), Err(FrontmatterError::BadStdout(_))));
    }

    #[test]
    fn unquoted_stdout_is_an_error() {
        let src = "#!spectest\n# expect:\n#   exit: 0\n#   stdout: naked\n#!end\n";
        assert!(matches!(parse_frontmatter(src), Err(FrontmatterError::BadStdout(_))));
    }

    #[test]
    fn open_without_close_is_malformed() {
        let src = "#!spectest\n# mode: run\n";
        assert!(matches!(parse_frontmatter(src), Err(FrontmatterError::MalformedFence(_))));
    }

    // ── the `trap:` field: present IFF exit == 101 ─────────────────────────
    #[test]
    fn trap_bearing_expect_parses() {
        let src = "#!spectest\n# expect:\n#   exit: 101\n#   stdout: \"pre\\n\"\n#   trap: T_Overflow\n#!end\n";
        let fm = parse_frontmatter(src).expect("a trap fixture must parse");
        assert_eq!(
            fm.expect,
            Expect {
                exit: 101,
                stdout: "pre\n".to_string(),
                trap: Some("T_Overflow".to_string()),
                reject: None,
            }
        );
    }

    #[test]
    fn no_trap_fixture_has_none_trap() {
        // The exit-0 baseline shape: `trap` is absent → `None`, not an error.
        let src = "#!spectest\n# expect:\n#   exit: 0\n#   stdout: \"ok\\n\"\n#!end\n";
        assert_eq!(parse_frontmatter(src).unwrap().expect.trap, None);
    }

    #[test]
    fn trap_code_with_non_trap_exit_is_an_error() {
        // A `trap:` code but exit != 101 (here exit 0) — rejected.
        let src = "#!spectest\n# expect:\n#   exit: 0\n#   stdout: \"\"\n#   trap: T_Overflow\n#!end\n";
        assert_eq!(
            parse_frontmatter(src),
            Err(FrontmatterError::TrapCodeNonTrapExit { code: "T_Overflow".to_string(), exit: 0 })
        );
    }

    #[test]
    fn trap_exit_without_code_is_an_error() {
        // exit 101 but no `trap:` code — rejected.
        let src = "#!spectest\n# expect:\n#   exit: 101\n#   stdout: \"\"\n#!end\n";
        assert_eq!(parse_frontmatter(src), Err(FrontmatterError::TrapExitWithoutCode));
    }

    #[test]
    fn nonzero_non_trap_exit_needs_no_code() {
        // 103 (FuelExhausted) is the one remaining nonzero NON-trap NON-reject
        // outcome: it legitimately carries no `trap:` code (rule keys on == 101)
        // and no `reject:` code (rule keys on == 1). (102 is retired — a static
        // rejection now exits 1 per the ratified exit-code scheme.)
        let src = "#!spectest\n# expect:\n#   exit: 103\n#   stdout: \"\"\n#!end\n";
        let fm = parse_frontmatter(src).unwrap_or_else(|e| panic!("exit 103 must parse: {e}"));
        assert_eq!(fm.expect.trap, None);
        assert_eq!(fm.expect.reject, None);
    }

    // ── the `reject:` field: present IFF exit == 1 (symmetric with `trap`) ──
    #[test]
    fn reject_bearing_expect_parses() {
        let src = "#!spectest\n# expect:\n#   exit: 1\n#   stdout: \"\"\n#   reject: E_UseAfterMove\n#!end\n";
        let fm = parse_frontmatter(src).expect("a reject fixture must parse");
        assert_eq!(
            fm.expect,
            Expect {
                exit: 1,
                stdout: String::new(),
                trap: None,
                reject: Some("E_UseAfterMove".to_string()),
            }
        );
    }

    #[test]
    fn reject_code_with_non_reject_exit_is_an_error() {
        // A `reject:` code but exit != 1 (here exit 0) — rejected.
        let src = "#!spectest\n# expect:\n#   exit: 0\n#   stdout: \"\"\n#   reject: E_UseAfterMove\n#!end\n";
        assert_eq!(
            parse_frontmatter(src),
            Err(FrontmatterError::RejectCodeNonRejectExit {
                code: "E_UseAfterMove".to_string(),
                exit: 0
            })
        );
    }

    #[test]
    fn reject_exit_without_code_is_an_error() {
        // exit 1 but no `reject:` code — rejected (a rejection MUST name its code).
        let src = "#!spectest\n# expect:\n#   exit: 1\n#   stdout: \"\"\n#!end\n";
        assert_eq!(parse_frontmatter(src), Err(FrontmatterError::RejectExitWithoutCode));
    }

    // ── the JSON decode is a tight inverse of the writer ───────────────────
    #[test]
    fn json_decode_inverts_the_writer() {
        // Mirror lib.rs::json_escape's five escapes plus a passthrough char.
        assert_eq!(parse_json_string(r#""a\"b\\c\nd\te\r""#).unwrap(), "a\"b\\c\nd\te\r");
        assert_eq!(parse_json_string(r#""""#).unwrap(), "");
        assert_eq!(parse_json_string(r#""plain""#).unwrap(), "plain");
    }
}
