# BRIEF — Chain A: executable guard for the lazy-CoW view-producer enumeration

Status: v5 (pass-4 review folded 2026-06-10: Appendix-B heading range fixed
(i)-(vi)→(i)-(vii) — a v3-fold remnant that contradicted W1+gate-3 [p4-R1];
pass 4 also confirmed the "only gate" claim — both (v)/(vii) are
clean-tree-neutral on the current corpus — and reproduced all
inventories/budgets exact. v4 was pass-3 folded 2026-06-10: gate-3 gains the SIXTH simulation
— the only gate that proves the clean-tree-neutral adjustments (v)+(vii) were
actually applied [p3-R1]; pass 3 also re-proved (vii) both ways from the brief
text alone and verified the four-item residual + devbook/25 note. v3 was
pass-2 folded 2026-06-10: W1 adjustment (vii) — arm a detects
calls via `gorget_str_view_region\s*\(` regex, closing the RUN-PROVEN
spaced-paren bypass [p2-R1; arm b already paren-free, unaffected]; W2's
devbook/11 residual list corrected to FOUR items incl. backend-emit rewrites
[p2-R2, a v2 fold remnant]; note added to generalize devbook/25's count-stale
"three companion guards" sentence [p2-nit]. Pass 2 also proved adjustment (v)
both ways — the Allman-style silent mis-attribution hole is real without it,
loud with it — and re-verified budgets/inventories/gates at tip. v2 was
pass-1 folded 2026-06-10: `GORGET_SLIT` name fix [R1]; W1
adjustment (v) added — fn-def scanner resets attribution on unparsed col-0
signature starts so brace-on-next-line defs go loud-unattributed instead of
silently mis-attributing [R2, the real drift-route hole]; GIR-axis wording
14=12+2 [R3]; arm-b message history claim corrected [R4]; backend-emit callee
rewrites added to the honest-residual list [R5]; lint-3 lower-the-budget clause
[footnote b]; W1 (iv) resolved — `regex` is a regular dependency, lints.rs:72
already uses it. Pass 1 independently recounted ALL inventories (14/12/2×1/41/
7/14/34 — exact) and re-ran the prototype + all FIVE drift simulations green/
red as documented. v1 was the orchestrator draft from scout
`agent-ae71cddc8f9c3e894`; inventories regenerated at tip `f5cd6aad`.)

## Mission

Convert the #37 view-producer enumeration (devbook/11 §enumeration rule +
`docs/plans/brief_37_phase1_lazy_default.md` Appendix A) from PROSE into an
EXECUTABLE GUARD: three tests in `tests/lints.rs`, written as native siblings of
the existing ratchets, such that adding a new view producer on any route fails
loudly with an actionable message. CLAUDE.md core invariant 6 ("prose rots;
guards don't"); the #37 review history proved the need — three producer routes
were each missed by an earlier prose enumeration rule.

## Ground truth (scout-verified at `f5cd6aad`; the lint stores NO line numbers —
names + counts only, so cite drift is harmless; executor RE-DERIVES the
baselines at execution time)

- **Runtime-C producers:** 12 functions / 14 call lines of
  `gorget_str_view_region` across `runtime_string.c` (index :705, slice :730,
  byte_slice :758, char_at :766) + `runtime_string_extended.c` (trim :257,
  lstrip_ws :268, rstrip_ws :278, strip :306, lstrip :317, rstrip :327,
  removeprefix :332-333, removesuffix :338-339). Plus the forward decl (:688),
  the definition (:742), and 4 comment-only citations elsewhere — all excluded
  by the scanner.
- **Backend-synthetic producers:** `c_lir/emit_call_extern.rs:91` (the Phase-1
  brief's `:86` drifted +5 — proof the no-line-numbers design is right) and
  `c_lir/emit_types.rs:2409`; both emit the `gorget_str_codepoint_at` shim.
- **The grep-rule BYPASS (scout finding #2):** `gorget_string_borrow_view`
  (`runtime_string.c:236-239`) manufactures the cap=0 header via a DIRECT
  struct literal, not via `gorget_str_view_region` — invisible to the devbook
  grep. Benign direct literals: `GORGET_EMPTY_STR` :56, the `GORGET_SLIT`
  macro body :61 (defined :60 — NOT "GORGET_STR_LIT"; pass-1 verified),
  `replacen` locals (extended :556/:564), `find_from` local (:665) — total
  direct-manufacture baseline **7** (incl. the view_region definition itself
  and borrow_view). Lint 3 fences this route.
- **Registry axis (scout finding #1 — the brief's original premise REFUTED):**
  `{returns_fresh: false ∧ ret == T::Str}` in `src/lir/runtime.rs` is a
  **34-entry SUPERSET** of the 14 view producers (false is the conservative
  default; fresh-but-untagged examples: `gorget_string_clone`:356,
  `_clone_to_owned`:357, `_concat`:358, `str_from_int`:305, io fns). Only the
  SOUND direction is lintable: every view producer must be `sig(` and NEVER
  `sig_fresh(` (a view tagged fresh lets `is_fresh_string` elide needed clone
  guards → dangling alias). All 14 producers verified `sig(` today
  (runtime.rs:283-355).
- **GIR axis:** 14 `returns_view: true` decls TOTAL (`builtins.rs:720-744`):
  12 route to callees ∈ the producer set, 2 are `None` (the `str`/`as_str`
  identity header copy).
- **LIR-rewrite mention baseline: 41** (run-verified): 14 registry decl lines +
  6 `RuntimeFn::` variant refs in the arity-overload rewrite (runtime.rs:702-704,
  view→view) + 14 type-table mentions (`lir/types.rs:487,:498-506`) + 3
  GIR-name fixups (`lir/lower/calls.rs:397-399`) + 4 IndexLoad-rewrite mentions
  (`lir/lower/insts.rs:920/:947/:948/:952`).
- **Run-proof of the prototype (scout):** full `cargo test --test lints` =
  10/10 ok in 3.53s; FIVE simulated drift routes each fail with an actionable
  message (new runtime producer; new .rs emitter file; `sig(`→`sig_fresh(`
  flip; new LIR literal mention 42>41; new direct cap=0 literal 8>7);
  commented-out calls correctly ignored.
- **Honest residual (stays prose in devbook/11):** dynamically-constructed
  callee names; passes that move/duplicate an existing view call breaking hook
  dominance (semantic, not greppable); same-commit budget-slot reuse;
  **backend-emit-layer callee rewrites (`src/backend`)** — name-level view
  substitutions exist there today (`llvm/mod.rs:1595/:5653`,
  `c_lir/emit_call_extern.rs:164`, `backend/mod.rs:288`, all view→view) and a
  NEW backend rewrite targeting a view callee spells no `view_region` and is
  outside lint 2's `src/lir` root (extending the root would add ~10+ noisy
  LLVM arity-rewrite baseline mentions — deliberately not fenced; pass-1 R5).
  The ratchet converts "silent" into "loud" for the textual ~90%.

## The work

### W1 — the three lints in `tests/lints.rs`
Insert the scout's prototype (Appendix B below, verbatim modulo W1 adjustments)
before `cow_after_stmt_covers_block_bearing_variants`:
1. `str_view_producer_enumeration_is_closed` — exact-set, four arms:
   (a) runtime-C callers == the `RuntimeC` rows of `STR_VIEW_PRODUCERS`;
   (b) `.rs` files spelling `gorget_str_view_region` on non-comment lines ==
   `VIEW_REGION_RS_EMITTERS` (file+count); (c) every producer has a
   `src/lir/runtime.rs` entry declared `sig(`, never `sig_fresh(`;
   (d) every `returns_view: true` decl routes to a producer in the table.
2. `no_growth_in_lir_view_callee_rewrites` — BUDGET **41** ratchet over
   `src/lir/**/*.rs` (quoted producer names + `RuntimeFn::` variants,
   non-comment lines).
3. `no_growth_in_runtime_c_direct_view_manufacture` — BUDGET **7** ratchet over
   the single-line `{ .data = ..., .cap = 0 ... }` pattern in runtime `.c`.

Executor adjustments to the prototype (each small, each from the scout's risk
list): (i) RE-DERIVE both budgets at execution time and fix the constants if
the tree moved; (ii) tighten the cap=0 regex to be field-ORDER-INDEPENDENT
(two independent `contains`-style checks within one brace group — scout risk
#2); (iii) verify arm (d)'s `returns_view` decl regex against the REAL current
format of `builtins.rs:720-744` (the scout ran it green, but confirm the
capture groups match the live decl shape and fail closed on unparseable
lines); (iv) RESOLVED by pass 1 — `regex` is a regular `[dependencies]` entry
(Cargo.toml:19) and `lints.rs:72` already uses it, nothing to do; (v) [pass-1
R2 — the real drift-route hole] in `runtime_c_view_region_callers`, RESET
`current_fn = String::new()` on any column-0 alpha-start line that contains
`(` but fails the def regex and is not a `;`-terminated declaration:
brace-on-next-line / multi-line C signatures exist in the scanned corpus
(`tls_server_runtime.c:13-17`), and without the reset a NEW producer written
in that style directly after an existing producer would silently
mis-attribute its calls into the expected set; with the reset they go
`<unattributed>` → loud. Harmless on the current corpus (statements are
indented; the only col-0 alpha non-def lines are multi-line signature
starts). (vi) [pass-1 footnote b] add the "if the count went DOWN, lower
BUDGET" clause to lint 3's failure message for sibling consistency.
(vii) [pass-2 R1, RUN-PROVEN bypass] in `runtime_c_view_region_callers`,
detect calls with the regex `gorget_str_view_region\s*\(` instead of the
glued-paren `contains` — `return gorget_str_view_region ((const char*)…)`
(GNU spacing) currently passes ALL THREE lints silently; arm b is already
paren-free and needs no change.

### W2 — make the docs point at the guard
- `docs/devbook/11-copy-on-write.md` §view-producer enumeration rule: add that
  the rule is now EXECUTABLE (`tests/lints.rs` — the three test names), note
  the direct-manufacture bypass finding (the grep alone was incomplete:
  `gorget_string_borrow_view` builds its header directly — lint 3 fences this
  route), and keep the honest-residual list — FOUR items: dynamic names,
  dominance moves, slot reuse, backend-emit callee rewrites (`src/backend`,
  per the Ground-truth bullet) — as the remaining prose obligations.
- `docs/devbook/25-structural-guards.md`: add the guard family entry following
  the doc's existing format (class → guard → escalation state: these land
  FATAL from day one, no env-gate burn-down needed — the enumeration is closed
  today). While there, generalize the count-stale "The three companion
  meta-invariant guards live as Rust lint tests in tests/lints.rs" sentence
  (7 today, 10 after this chain) so it stops rotting [pass-2 nit].

### W3 — bookkeeping
- TODO.md: ADD one Low-priority entry for the scout's optional forcing
  function (pin the count of `sig(`-declared `T::Str` registry entries — today
  34 — so every new Str-returning runtime fn must be explicitly classified
  fresh-vs-conservative; owner's call, NOT implemented in this chain), citing
  scout finding #1 (the returns_fresh superset). Pending-phrased only.
- DONE.md: one dated entry (what the guard covers, the two scout findings, the
  honest residual, the commands to re-run).

## Gates (executor; this chain touches NO src/ code — no integration suite, no
fixed_point needed)

1. `cargo build` + `cargo test --lib` (compile sanity; expect 1072/0).
2. `cargo test --test lints` — **10/10** (7 existing + 3 new), < 10s.
3. Re-run ALL SIX simulated-drift demonstrations — the scout's five (new
   runtime producer / new .rs emitter / sig_fresh flip / LIR mention 42>41 /
   direct literal 8>7) PLUS the pass-3 sixth, which is the ONLY gate that
   proves adjustments (v)+(vii) were actually applied (both are
   clean-tree-neutral — an executor who skipped them would otherwise pass
   every gate with the proven bypass intact): append to
   `runtime_string_extended.c` a NEW function in Allman style with a
   GNU-spaced call —
   `static inline Str gorget_str_first_byte(Str s)` / `{` /
   `    return gorget_str_view_region ((const char*)s.data, 1);` / `}` —
   expected failure = arm a's "could not attribute…" assert ((v) proof; the
   spaced call is only SEEN at all because of (vii)). NOTE: placing the
   spaced call INSIDE an existing table producer correctly stays green
   (function-set granularity — that producer is already covered); the
   simulation must use a NEW function. Confirm each of the six fails with
   its message, REVERT each, confirm green again. Paste the six failure
   messages in the handoff (they are the guard's UX — reviewers approved
   their wording).
4. Commented-out-call check: a comment containing a verbatim
   `gorget_str_view_region(` call must NOT trip any arm.

## Constraints

- Executor: isolated worktree; open with `pwd` + `git rev-parse --show-toplevel`
  verification + `git merge --ff-only gorget-1`; never touch
  `/workspace/gorget-1` or `main`; `git add` explicit file names only
  (`tests/lints.rs`, the two devbook files, TODO.md, DONE.md).
- Chain B (self-host provenance port) runs in parallel touching
  `tests/fixtures/self_host_lowerer/*` — file-disjoint except TODO/DONE
  (parent merges). The lints scan `src/` + `src/backend/c/runtime` only, never
  `tests/fixtures/` (scout-confirmed).
- One commit is fine (test-only + docs); message cites this brief + the scout;
  Co-Authored-By trailer.
- The failure messages are part of the spec — keep the scout's wording unless
  a reviewer flags it; they must tell the author exactly what to do and why
  (the ASan-blindness warning included).

## Appendix B — the scout's run-proven prototype source (verbatim; executor
applies W1 adjustments (i)-(vii) on top; insert before
`cow_after_stmt_covers_block_bearing_variants` in `tests/lints.rs`)

```rust
// ─────────────────────────────────────────────────────────────────────────────
// #37 lazy-CoW view-producer enumeration guard
// ─────────────────────────────────────────────────────────────────────────────

/// Which route manufactures the view, deciding which detection arm must see it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ViewRoute {
    /// A runtime `.c` function whose body calls `gorget_str_view_region`.
    RuntimeC,
    /// A runtime `.c` function that manufactures the cap=0 header DIRECTLY
    /// (a blessed constructor; policed by the direct-manufacture ratchet).
    RuntimeCDirect,
    /// A SYNTHETIC callee: no `.c` body — backend `.rs` emitters write the
    /// `gorget_str_view_region` call into generated C.
    BackendSynthetic,
}

/// THE ENUMERATION. Every "view producer" — anything that manufactures a
/// cap=0 `Str` view aliasing another buffer — with its manufacture route and
/// the GIR-level mechanism that keeps the lazy-CoW default sound for it
/// (the four materialize hooks W3a/W3b/W3c/W3d, per
/// `docs/devbook/11-copy-on-write.md` §"View-producer enumeration rule" and
/// `docs/plans/brief_37_phase1_lazy_default.md` Appendix A).
///
/// **Adding a new view producer?** It is UNSOUND under the lazy-CoW default
/// unless a GIR materialize hook dominates every capture of its result while
/// the source is a lazy view. Cover it with one of the four hooks (or a new
/// sibling call site of `materialize_lazy_source_if_needed`), add the row
/// here AND to devbook/11's enumeration, and cite both in the PR.
const STR_VIEW_PRODUCERS: &[(&str, ViewRoute, &str)] = &[
    ("gorget_str_index",          ViewRoute::RuntimeC, "W3c index-base hook (lower_index_access)"),
    ("gorget_str_slice",          ViewRoute::RuntimeC, "W3c index-base hook + W3b receiver hook"),
    ("gorget_str_byte_slice",     ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_char_at",        ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_trim",           ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_lstrip_ws",      ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_rstrip_ws",      ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_strip",          ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_lstrip",         ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_rstrip",         ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_removeprefix",   ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_removesuffix",   ViewRoute::RuntimeC, "W3b receiver hook (returns_view dispatch)"),
    ("gorget_str_codepoint_at",   ViewRoute::BackendSynthetic, "W3d for-string hook (lower_for_string)"),
    ("gorget_string_borrow_view", ViewRoute::RuntimeCDirect, "W3a bind hook (the lazy bind producer itself)"),
];

/// `.rs` files allowed to spell `gorget_str_view_region` on a NON-comment
/// line, with the expected occurrence count. These are the backend emitters
/// of the synthetic `gorget_str_codepoint_at` shim (the C-emit boundary —
/// the name IS the contract with the runtime there). Comment-line mentions
/// (devbook citations in ir/lowering) are skipped, so docs stay free.
const VIEW_REGION_RS_EMITTERS: &[(&str, usize)] = &[
    ("src/backend/c_lir/emit_call_extern.rs", 1),
    ("src/backend/c_lir/emit_types.rs", 1),
];

/// Scan `src/backend/c/runtime/*.c` and return, for each non-comment call of
/// `gorget_str_view_region(`, the enclosing C function name (with file:line
/// for diagnostics). Function-definition and forward-declaration lines of
/// `gorget_str_view_region` itself are excluded — the definition is the
/// blessed constructor, not a producer.
fn runtime_c_view_region_callers() -> Vec<(String, String)> {
    let mut callers: Vec<(String, String)> = Vec::new();
    let fn_def = regex::Regex::new(
        // A C function definition at column 0: `static inline Str name(args) {`
        r"^[A-Za-z_][A-Za-z0-9_ \*]*?([A-Za-z_][A-Za-z0-9_]*)\s*\([^;{]*\)\s*\{\s*$"
    ).unwrap();
    visit("src/backend/c/runtime", &mut |path| {
        if path.extension().map_or(true, |e| e != "c") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        let mut current_fn = String::new();
        for (idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            // Skip comment lines: a commented-out call is documentation,
            // not a live producer (matches the comment-skip convention of
            // the sidecar / proxy-read lints above).
            if trimmed.starts_with("//") || trimmed.starts_with("*") || trimmed.starts_with("/*") {
                continue;
            }
            let is_def_line = if line.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
                if let Some(cap) = fn_def.captures(line) {
                    current_fn = cap[1].to_string();
                    true
                } else {
                    false
                }
            } else {
                false
            };
            if is_def_line {
                continue; // the def line of view_region itself is not a call
            }
            // Forward declarations (`static inline Str f(...);`) are not calls.
            if trimmed.starts_with("static") && trimmed.trim_end().ends_with(';') {
                continue;
            }
            if line.contains("gorget_str_view_region(") {
                if current_fn == "gorget_str_view_region" {
                    continue; // inside the blessed constructor's own body
                }
                let loc = format!("{}:{}", path.display(), idx + 1);
                if current_fn.is_empty() {
                    callers.push((format!("<unattributed at {loc}>"), loc));
                } else {
                    callers.push((current_fn.clone(), loc));
                }
            }
        }
    });
    callers
}

/// Per-file count of NON-comment `gorget_str_view_region` mentions in
/// `src/**/*.rs` — the backend-synthetic emitter arm.
fn rs_view_region_mentions() -> Vec<(String, usize)> {
    let mut counts: Vec<(String, usize)> = Vec::new();
    visit("src", &mut |path| {
        if path.extension().map_or(true, |e| e != "rs") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        let mut n = 0;
        for line in content.lines() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            n += line.matches("gorget_str_view_region").count();
        }
        if n > 0 {
            counts.push((path.to_string_lossy().replace('\\', "/"), n));
        }
    });
    counts.sort();
    counts
}

/// The enumeration guard: every view producer is detected on its manufacture
/// route AND maps to a covering mechanism in `STR_VIEW_PRODUCERS`.
///
/// **Arm a (runtime C):** the set of runtime `.c` functions whose bodies call
/// `gorget_str_view_region` must EXACTLY equal the `RuntimeC` rows.
/// **Arm b (backend synthetic):** the `.rs` files spelling
/// `gorget_str_view_region` on non-comment lines must EXACTLY match
/// `VIEW_REGION_RS_EMITTERS` (file + count).
/// **Arm c (typed-registry reconciliation):** every producer must be present
/// in `src/lir/runtime.rs` and declared via `sig(` — NEVER `sig_fresh(`. A
/// view tagged `returns_fresh: true` would let the CoW machinery elide the
/// clone guards that keep views from dangling (see `is_fresh_string`).
/// **Arm d (GIR axis reconciliation):** every `returns_view: true` method in
/// `src/ir/lowering/builtins.rs` must route to a producer in the table (or
/// `None` for the identity header copy `str`/`as_str`).
///
/// **If this fails**: you added (or re-routed) a view producer. Read the
/// table doc above — cover the producer with a GIR materialize hook
/// (W3a/W3b/W3c/W3d sibling), then add its row here and to
/// `docs/devbook/11-copy-on-write.md`'s enumeration. Do NOT just extend the
/// allowlist: an uncovered producer is a use-after-free generator under the
/// lazy-CoW default (and the class is proven ASan-blind — stdout fixtures
/// are the only net).
#[test]
fn str_view_producer_enumeration_is_closed() {
    // Arm a — runtime C producers.
    let callers = runtime_c_view_region_callers();
    let mut found: Vec<&str> = callers.iter().map(|(f, _)| f.as_str()).collect();
    found.sort();
    found.dedup();
    let mut expected: Vec<&str> = STR_VIEW_PRODUCERS
        .iter()
        .filter(|(_, route, _)| *route == ViewRoute::RuntimeC)
        .map(|(name, _, _)| *name)
        .collect();
    expected.sort();
    let unattributed: Vec<&(String, String)> =
        callers.iter().filter(|(f, _)| f.starts_with('<')).collect();
    assert!(
        unattributed.is_empty(),
        "could not attribute these gorget_str_view_region calls to an enclosing \
         C function (the def-line scanner in runtime_c_view_region_callers needs \
         updating for a new code shape): {unattributed:?}"
    );
    let missing: Vec<&&str> = expected.iter().filter(|e| !found.contains(*e)).collect();
    let new_producers: Vec<(&str, &str)> = callers
        .iter()
        .filter(|(f, _)| !expected.contains(&f.as_str()))
        .map(|(f, loc)| (f.as_str(), loc.as_str()))
        .collect();
    assert!(
        new_producers.is_empty() && missing.is_empty(),
        "View-producer enumeration drifted (runtime-C arm).\n\
         NEW producers (functions calling gorget_str_view_region, not in the table): {new_producers:?}\n\
         VANISHED producers (in the table, no longer calling it): {missing:?}\n\n\
         A function returning a cap=0 view aliasing another buffer is UNSOUND under \
         the lazy-CoW default unless a GIR materialize hook dominates every capture \
         of its result (docs/devbook/11-copy-on-write.md §\"View-producer enumeration \
         rule\"; docs/plans/brief_37_phase1_lazy_default.md Appendix A).\n\
         For a NEW producer: wire a hook (sibling call site of \
         materialize_lazy_source_if_needed — W3a bind / W3b receiver / W3c index \
         base / W3d for-string source), add a row to STR_VIEW_PRODUCERS in this \
         file naming the hook, and extend devbook/11's enumeration.\n\
         For a VANISHED producer: remove its row here and in devbook/11.",
    );

    // Arm b — backend-synthetic emitters.
    let rs_mentions = rs_view_region_mentions();
    let mut expected_rs: Vec<(String, usize)> = VIEW_REGION_RS_EMITTERS
        .iter()
        .map(|(f, n)| (f.to_string(), *n))
        .collect();
    expected_rs.sort();
    assert_eq!(
        rs_mentions, expected_rs,
        "View-producer enumeration drifted (backend-synthetic arm): the set of .rs \
         files spelling `gorget_str_view_region` on non-comment lines changed.\n\
         found:    {rs_mentions:?}\n\
         expected: {expected_rs:?}\n\n\
         A new emitter writes a view-manufacturing call into generated C — that is a \
         new view producer (the W3d `gorget_str_codepoint_at` class: synthetic callees \
         never appear in the runtime .c, which is exactly how the route was missed \
         pre-#37 — the enumeration rule needed two corrections in total). Cover \
         its GIR producer with a materialize hook, add the \
         producer row to STR_VIEW_PRODUCERS, and update VIEW_REGION_RS_EMITTERS + \
         devbook/11. Comment-line citations don't count — only live emit lines.",
    );

    // Arm c — LIR registry reconciliation.
    let registry = fs::read_to_string("src/lir/runtime.rs")
        .expect("src/lir/runtime.rs must exist (typed runtime registry)");
    for (name, _, mechanism) in STR_VIEW_PRODUCERS {
        let decl = registry
            .lines()
            .find(|l| l.contains(&format!("=> \"{name}\",")));
        let decl = decl.unwrap_or_else(|| {
            panic!(
                "view producer `{name}` ({mechanism}) has no entry in \
                 src/lir/runtime.rs — every producer must be a typed registry \
                 entry (devbook/24 rule 2: typed metadata, not name-matching)"
            )
        });
        assert!(
            decl.contains("sig(") && !decl.contains("sig_fresh("),
            "view producer `{name}` is declared `sig_fresh` in src/lir/runtime.rs:\n  {decl}\n\
             A cap=0 view MUST carry `returns_fresh: false` — `returns_fresh: true` \
             lets CoW elide the self-referential-reassignment clone guard and the \
             return-clone-elision check (`is_fresh_string`), turning the view into a \
             dangling alias. Change the declaration back to `sig(`.",
        );
    }

    // Arm d — GIR `returns_view` axis reconciliation.
    let builtins = fs::read_to_string("src/ir/lowering/builtins.rs")
        .expect("src/ir/lowering/builtins.rs must exist");
    let view_decl = regex::Regex::new(
        r#"name: "([A-Za-z_]+)", runtime_callee: (?:Some\("([a-z_]+)"\)|None)"#
    ).unwrap();
    let producer_names: Vec<&str> = STR_VIEW_PRODUCERS.iter().map(|(n, _, _)| *n).collect();
    for line in builtins.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("//") || !line.contains("returns_view: true") {
            continue;
        }
        let cap = view_decl.captures(line).unwrap_or_else(|| {
            panic!("unparseable returns_view decl line in builtins.rs: {line}")
        });
        if let Some(callee) = cap.get(2) {
            assert!(
                producer_names.contains(&callee.as_str()),
                "builtins.rs declares method `{}` with `returns_view: true` routing \
                 to runtime callee `{}`, which is NOT in STR_VIEW_PRODUCERS. Either \
                 the callee is a new view producer (cover it with a hook + add the \
                 row) or the `returns_view` tag is wrong.",
                &cap[1],
                callee.as_str(),
            );
        }
        // `None` callees (str/as_str identity header copy) are W3b-covered
        // through the same returns_view dispatch — nothing to reconcile.
    }
}

/// Ratchet (the LIR-rewrite fence — the partially-fenceable blind spot): the
/// count of view-producer callee MENTIONS in `src/lir/**/*.rs` must not grow.
///
/// The four GIR materialize hooks are keyed UPSTREAM (bind / receiver / index
/// base / for-string source). An LIR-level rewrite that changes a callee to a
/// view-returning one (the `IndexLoad → gorget_str_slice/str_index` precedent,
/// `src/lir/lower/insts.rs`) bypasses them unless the GIR shape it rewrites
/// was already hooked. This ratchet can't prove dominance, but it CAN make a
/// new mention of a view callee in the LIR layer fail loudly so the author
/// reconciles it against the enumeration before shipping.
///
/// Counted (non-comment lines): exact-quoted producer names
/// (`"gorget_str_slice"`, ...) and `RuntimeFn::` variant references
/// (`RuntimeFn::StrSlice`, ...) in src/lir/. Baseline 2026-06-10: 41 —
/// 14 registry decl lines (runtime.rs) + 6 variant refs in the arity-overload
/// rewrite (runtime.rs, strip→trim_ws family, view→view so W3b-covered) +
/// 14 return-type-table mentions (types.rs) + 3 GIR-name fixups (lower/calls.rs,
/// W3b-covered upstream) + 4 IndexLoad-rewrite mentions (lower/insts.rs,
/// W3c-covered upstream).
///
/// **If this fails (count went UP):** you added an LIR site naming a
/// view-returning callee. If it REWRITES some inst into a call of that
/// callee, verify a GIR materialize hook dominates every such rewritten
/// shape (or add the missing hook), reconcile against devbook/11's
/// enumeration, THEN bump with a justification comment. If it's registry /
/// type-table plumbing, bump with a one-liner.
#[test]
fn no_growth_in_lir_view_callee_rewrites() {
    const BUDGET: usize = 41;

    let names: Vec<&str> = STR_VIEW_PRODUCERS.iter().map(|(n, _, _)| *n).collect();
    let quoted = names
        .iter()
        .map(|n| format!(r#""{n}""#))
        .collect::<Vec<_>>()
        .join("|");
    // RuntimeFn variant spellings of the same producers (CamelCase of the
    // gorget_* names as declared in runtime.rs).
    let variants = [
        "StrIndex", "StrSlice", "StrByteSlice", "StrCharAt", "StrTrim",
        "StrLstripWs", "StrRstripWs", "StrStrip", "StrLstrip", "StrRstrip",
        "StrRemoveprefix", "StrRemovesuffix", "StrCodepointAt", "StringBorrowView",
    ];
    let variant_alt = variants
        .iter()
        .map(|v| format!(r"RuntimeFn::{v}\b"))
        .collect::<Vec<_>>()
        .join("|");
    let pattern = regex::Regex::new(&format!("{quoted}|{variant_alt}")).unwrap();

    let mut count = 0;
    visit("src/lir", &mut |path| {
        if path.extension().map_or(true, |e| e != "rs") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        for line in content.lines() {
            if line.trim_start().starts_with("//") {
                continue;
            }
            count += pattern.find_iter(line).count();
        }
    });
    assert!(
        count <= BUDGET,
        "View-producer callee mentions in src/lir/ grew beyond budget: {count} > {BUDGET}.\n\n\
         A new src/lir site names a view-returning callee (string literal or \
         RuntimeFn variant). The GIR materialize hooks (W3a-W3d) are keyed \
         upstream of LIR — a NEW rewrite targeting a view callee can bypass them \
         (the IndexLoad→str_slice/str_index precedent was exactly this shape and \
         needed its own hook, W3c).\n\n\
         Verify a GIR materialize hook dominates the rewritten shape (or add one — \
         sibling call site of materialize_lazy_source_if_needed), reconcile against \
         docs/devbook/11-copy-on-write.md's enumeration, then bump BUDGET with a \
         justification comment. If the count went DOWN, lower BUDGET.",
    );
}

/// Ratchet (the bypass fence): direct cap=0 `Str` header manufacture in the
/// runtime `.c` files must not grow. A view built with a raw struct literal
/// (`{ .data = ..., .cap = 0, ... }`) instead of `gorget_str_view_region`
/// is INVISIBLE to the enumeration guard's runtime-C arm — this ratchet is
/// what stops that bypass.
///
/// Baseline 2026-06-10: 7 —
///   runtime_string.c:56  GORGET_EMPTY_STR (static, .rodata, never freed)
///   runtime_string.c:61  GORGET_SLIT macro body (static literal views)
///   runtime_string.c:238 gorget_string_borrow_view (blessed producer, W3a)
///   runtime_string.c:744 gorget_str_view_region itself (THE blessed constructor)
///   runtime_string_extended.c:556/:564 replacen locals (ephemeral, bytes
///     copied into a fresh result before return)
///   runtime_string_extended.c:665 find_from local (ephemeral, search only)
///
/// **If this fails (count went UP):** a new direct cap=0 view literal was
/// added. If the view is RETURNED (or stored), route it through
/// `gorget_str_view_region` so the enumeration guard sees the producer, and
/// cover it per `str_view_producer_enumeration_is_closed`'s table. If it is
/// genuinely ephemeral (consumed before any caller-visible mutation), bump
/// with a justification comment naming the function.
/// NOTE: the pattern is single-line; a multi-line struct literal would evade
/// it. Keep view literals on one line (current style throughout).
#[test]
fn no_growth_in_runtime_c_direct_view_manufacture() {
    const BUDGET: usize = 7;

    let pattern = regex::Regex::new(
        r"\{[^{}]*\.data\s*=[^{}]*\.cap\s*=\s*0[^{}]*\}"
    ).unwrap();
    let mut count = 0;
    let mut sites: Vec<String> = Vec::new();
    visit("src/backend/c/runtime", &mut |path| {
        if path.extension().map_or(true, |e| e != "c") {
            return;
        }
        let content = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => return,
        };
        for (idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") || trimmed.starts_with("*") || trimmed.starts_with("/*") {
                continue;
            }
            let n = pattern.find_iter(line).count();
            if n > 0 {
                count += n;
                sites.push(format!("{}:{}", path.display(), idx + 1));
            }
        }
    });
    assert!(
        count <= BUDGET,
        "Direct cap=0 Str view manufacture in runtime .c grew beyond budget: \
         {count} > {BUDGET}.\nSites: {sites:?}\n\n\
         A raw `{{ .data = ..., .cap = 0 }}` struct literal manufactures a view \
         the enumeration guard cannot see (it only attributes \
         `gorget_str_view_region` callers). If the new view is returned or \
         stored, build it with `gorget_str_view_region` instead and cover the \
         producer per STR_VIEW_PRODUCERS. If it is ephemeral (consumed before \
         any caller-visible mutation, like the replacen/find_from locals), bump \
         BUDGET with a justification naming the function.",
    );
}
```
