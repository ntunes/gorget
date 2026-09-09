// THE `#!parity-excluded` READER — ONE parser, TWO test binaries.
//
// `include!`d from `tests/integration.rs` (the three parity consumers) AND from
// `tests/lints.rs` (`parity_untriaged_exclusions_shrink_only`, which compares
// its own inline `strip_prefix` reading of the same declaration against this
// one). Keeping it here is what makes that comparison a WITNESS: two
// independently-written readings of one declaration, neither of them a copy of
// the other, asserted to agree on the live corpus.
//
// ⚠ THE WITNESS IS OVER *MEMBERSHIP*, AND IT IS A PAIR. A THIRD reading of
// this directive already exists and is deliberately NOT part of it:
// `tests/lints.rs::parity_declarations_are_well_formed` does its own
// `strip_prefix` + `split_once(':')` + kind-table + continuation walk. It
// answers a different question — is EVERY declaration in the tree well formed —
// and never builds a membership set, so it cannot be one of two readings that
// disagree about WHICH fixtures are untriaged.
//
// The consequence, not an unenforced rule: add a third reading of MEMBERSHIP
// and a disagreement stops naming which instrument is broken, because three
// readings have no majority worth trusting and no pair to attribute the fault
// to. Nothing in the tree can stop you doing it, which is exactly why the
// reason is written here rather than a bare prohibition (Core #14).
//
// The items are `pub` because both binaries include this inside a `mod`. Each
// uses a different subset, so both include sites carry `#[allow(dead_code)]`
// on the `mod` — an INNER `#![allow]` here does not compile, because inner
// attributes are not permitted out of an `include!` expansion.

use std::path::Path;

/// The declaration directive a fixture uses to take itself out of the
/// runtime-parity corpus. Lives in the fixture's LEADING comment block, in the
/// tree's established "`#!` fence inside leading `#` comments" convention (the
/// same one `#!spectest` uses — `spec/ggdef/src/frontmatter.rs`), so it is
/// invisible to the lexer and `gg run`/`gg build` are unaffected.
///
/// Shape, with the evidence free to run onto continuation lines so no line
/// blows the ratified 120-column budget (`fmt_no_new_over_budget_lines`):
///
/// ```text
/// #!parity-excluded untriaged: DEBT (todo/t0828) — no cause established.
/// #!  Excluded only because the retired filename heuristic matched
/// #!  `stem.starts_with("datetime_")`; its own header says "fully deterministic".
/// ```
///
/// A continuation is `#!` + WHITESPACE, which cannot collide with a fence
/// (`#!parity-excluded`, `#!spectest`, `#!end` are all `#!` + a word).
pub const PARITY_EXCLUDED_DECL: &str = "#!parity-excluded";

/// WHY a fixture's stdout is not a stable cross-lane function of its source.
///
/// TYPED, not a free string: `reason()` and `spelling()` are exhaustive
/// `match`es with NO `_` arm, so **rustc is the witness** that every kind the
/// reader can produce has both a wire spelling and a human reason. Adding a
/// variant without one does not compile.
///
/// ⛔ There is deliberately NO variant meaning "the self-host miscompiles it".
/// That fixture stays in the corpus as WRONG-OUTPUT / CC-FAIL and goes to the
/// TODO backlog — excluding it would be the forbidden parity-inflation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParityExclusionKind {
    /// The program reads the wall clock / current date into its own stdout.
    Clock,
    /// The program draws from an RNG.
    Random,
    /// The program binds/connects a socket or drives a peer stack.
    Network,
    /// A sleep, a timed wait, or a concurrent interleaving decides the output.
    Scheduling,
    /// The `bench` / `test` RUNNER prints elapsed wall-clock time into stdout —
    /// the program itself is deterministic, its harness output is not.
    Harness,
    /// The program reads runtime/allocator-internal state (`mem_live()`) into
    /// stdout, so the bytes differ per lane and per allocation history.
    Allocator,
    /// The output depends on the host environment beyond the program: an
    /// external binary, the host time zone, a device/windowing stack.
    Host,
    /// ⚠ DEBT — inherited from the retired filename-substring heuristic; NO
    /// cause has been established for this fixture. It is excluded only because
    /// of how it is SPELLED, which is exactly the defect this declaration
    /// retired. Each one is a candidate for reinstatement; see
    /// `parity_untriaged_exclusions_shrink_only` in `tests/lints.rs` for the
    /// shrink-only ratchet and `todo/` for the reinstatement work.
    Untriaged,
}

impl ParityExclusionKind {
    /// The wire spelling used in the fixture declaration. Exhaustive — rustc
    /// rejects a new variant that forgets its spelling.
    pub fn spelling(self) -> &'static str {
        match self {
            Self::Clock => "clock",
            Self::Random => "random",
            Self::Network => "network",
            Self::Scheduling => "scheduling",
            Self::Harness => "harness",
            Self::Allocator => "allocator",
            Self::Host => "host",
            Self::Untriaged => "untriaged",
        }
    }

    /// The reason text carried into the parity report. Exhaustive — rustc
    /// rejects a new variant that forgets its reason.
    pub fn reason(self) -> &'static str {
        match self {
            Self::Clock => "clock (the program reads the wall clock into stdout)",
            Self::Random => "random (the program draws from an RNG)",
            Self::Network => "network (binds/connects a socket or drives a peer stack)",
            Self::Scheduling => "scheduling (a sleep, timed wait or interleaving decides the output)",
            Self::Harness => "harness (the bench/test runner prints elapsed wall-clock time)",
            Self::Allocator => "allocator (reads mem_live()/runtime-internal state into stdout)",
            Self::Host => "host (depends on an external binary, the host time zone, or a device)",
            Self::Untriaged => "untriaged (DEBT — inherited from the retired filename heuristic)",
        }
    }

    /// Parse one wire spelling. An unrecognised kind is a LOUD error at the
    /// reader, never a silent "not declared" — a mis-parse would silently put a
    /// nondeterministic fixture back into a byte-compared corpus.
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "clock" => Some(Self::Clock),
            "random" => Some(Self::Random),
            "network" => Some(Self::Network),
            "scheduling" => Some(Self::Scheduling),
            "harness" => Some(Self::Harness),
            "allocator" => Some(Self::Allocator),
            "host" => Some(Self::Host),
            "untriaged" => Some(Self::Untriaged),
            _ => None,
        }
    }
}

/// One fixture's parity-exclusion declaration: the typed kind plus the
/// fixture-authored evidence sentence that justifies it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParityExclusion {
    pub kind: ParityExclusionKind,
    pub evidence: String,
}

impl ParityExclusion {
    /// The reason string the parity report prints for this fixture.
    pub fn reason(&self) -> String {
        format!("{} — {}", self.kind.reason(), self.evidence)
    }
}

/// Read a fixture's runtime-parity exclusion **from its SOURCE**.
///
/// ⛔ CORE #2 / LAYERING RULE 2 — this function is deliberately NOT given the
/// fixture's name. "Is this fixture's stdout a stable cross-lane function of
/// its source?" is a property of the PROGRAM, so the program declares it and
/// this accessor reads it. The predicate this replaced answered the question
/// with `stem.starts_with("httpserver_")` — which excluded `httpserver_router`
/// ("all core features exercised without TCP", per its own header) purely
/// because of how it is spelled, and left `test_process_timeout` in the corpus
/// for the same reason (`todo/t0820`). A name cannot decide meaning here even
/// by accident: there is no name in scope.
///
/// The declaration is only honoured inside the LEADING comment block — the
/// first non-comment, non-blank line ends the scan — matching `#!spectest`'s
/// "the block lives entirely in leading `#` comments" rule. A declaration
/// further down the file would be silently ignored, so
/// `parity_declarations_are_well_formed` (`tests/lints.rs`) fails on one.
///
/// Every malformed declaration is a LOUD `Err`, never a silent `None`: a
/// silent mis-parse would put a nondeterministic fixture back into a
/// byte-compared corpus, which is the exact failure class this reader exists
/// to make impossible.
pub fn fixture_parity_exclusion(src: &str) -> Result<Option<ParityExclusion>, String> {
    let mut found: Option<ParityExclusion> = None;
    let mut in_continuation = false;
    for line in src.lines() {
        let trimmed = line.trim_start();
        if trimmed.is_empty() {
            in_continuation = false;
            continue;
        }
        if !trimmed.starts_with('#') {
            break; // leading comment block is over
        }
        // `#!` + whitespace continues the evidence of the declaration above.
        if let Some(cont) = parity_continuation_text(trimmed) {
            if in_continuation {
                let decl = found.as_mut().expect("continuation implies a declaration");
                decl.evidence.push(' ');
                decl.evidence.push_str(cont);
                continue;
            }
            return Err(format!(
                "`#!` continuation line with no `{PARITY_EXCLUDED_DECL}` declaration \
                 directly above it: `{trimmed}`",
            ));
        }
        in_continuation = false;
        let Some(rest) = trimmed.strip_prefix(PARITY_EXCLUDED_DECL) else {
            continue;
        };
        let Some((kind_text, evidence)) = rest.split_once(':') else {
            return Err(format!(
                "malformed `{PARITY_EXCLUDED_DECL}` declaration: expected \
                 `{PARITY_EXCLUDED_DECL} <kind>: <evidence>`, got `{trimmed}`",
            ));
        };
        let kind_text = kind_text.trim();
        let Some(kind) = ParityExclusionKind::parse(kind_text) else {
            return Err(format!(
                "unknown parity-exclusion kind `{kind_text}` in `{trimmed}` — the kinds are \
                 clock / random / network / scheduling / harness / allocator / host / untriaged \
                 (`ParityExclusionKind` in tests/lints_support/parity_exclusion_reader.rs). There is \
                 deliberately no kind for \"the self-host miscompiles it\": that fixture \
                 stays in the corpus.",
            ));
        };
        let evidence = evidence.trim().to_string();
        if evidence.is_empty() {
            return Err(format!(
                "`{PARITY_EXCLUDED_DECL} {kind_text}:` carries no evidence — every exclusion \
                 states WHAT varies, in the fixture, so the next reader can check it or \
                 reinstate the fixture.",
            ));
        }
        if found.is_some() {
            return Err(format!(
                "two `{PARITY_EXCLUDED_DECL}` declarations in one fixture — exactly one \
                 declaration decides (layering rule 3: one source of truth per axis).",
            ));
        }
        found = Some(ParityExclusion { kind, evidence });
        in_continuation = true;
    }
    Ok(found)
}

/// The text of a `#!`-continuation line, or `None` if the line is a fence
/// (`#!parity-excluded`, `#!spectest`, `#!end` — all `#!` + a word character)
/// or an ordinary `#` comment.
pub fn parity_continuation_text(trimmed_line: &str) -> Option<&str> {
    let rest = trimmed_line.strip_prefix("#!")?;
    if !rest.starts_with(|c: char| c.is_whitespace()) {
        return None;
    }
    Some(rest.trim())
}

/// `fixture_parity_exclusion` for a fixture on disk. A malformed declaration or
/// an unreadable fixture PANICS — the three consumers must never proceed on a
/// guessed classification.
pub fn fixture_parity_exclusion_at(path: &Path) -> Option<ParityExclusion> {
    let src = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("parity: cannot read fixture {}: {e}", path.display()));
    fixture_parity_exclusion(&src)
        .unwrap_or_else(|e| panic!("parity: {} — {e}", path.display()))
}
