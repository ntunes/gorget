# EXECUTOR BRIEF — D29 implementation: call-site `!` (visible error propagation), all lanes

**Status:** DRAFT v4 — in the ≥3-fresh-pass review gauntlet. Gauntlet history:
pass 1 (semantics faithful; capture machinery located — kind-1 capture EXISTS via
`decl_type_hint`/`dest_is_result`; 2 HIGH: checker/gates predated the amendment → kind-2
rewritten to bare-DISCARD-only + the three keep-the-Result sites' re-scoping + gates
reconciled; census re-measure; conformance/anchored gates; machinery named) → v2.
Pass 2 (1 HIGH: the auto-capture migration planned the WRONG edit — now a
rewrite-to-explicit-capture transform; gate-2 remnant synced; gate-8 accept/reject floor
split + ggdef IllFormed mechanics; auto clause scoped kind-1) → v3.
Pass 3 (caught the v3 fold's OWN defect: marked-match SPLITS BY ARM TYPE — T-variant arms
are LEGAL and keep their RUN net [snag48's class, the actual SIGSEGV site; its scout proof
STANDS], only Ok/Error arms are the check error; snag48's wrong NEG flip reverted to
mechanical-mark POS; +R2 `MIN_FIXTURES=214` EXISTS as the total-seed guard and bumps in
lockstep; +R3 the self-host lane given the explicit kind-1/kind-2 split) → this v4.
Do not execute until a clean pass. NOTE for reviewers: `src/semantic/safety/check_expr.rs`
gains an additive `Propagate` arm — RV-B's zone; the sequencing STOP covers it.
**Normative semantics:** `decisions.md` LOG — the D29 formal ratification + its six
follow-through pins + the **2026-07-17 CAPTURE AMENDMENT** (read all of them FIRST; where
any other document disagrees, the LOG wins).
**Scout evidence:** `scouts/scout-d29-impl.md` (all numbers regenerated 2026-07-16) +
prototype `/tmp/recover_d29_proto_live.patch` (the live-diff capture; the scout's blessed
755-line variant is `/tmp/d29_impl_proto.patch` — both in scratchpad backups) + the census
migrator `/tmp/d29_migrate.py`. The packet `scouts/scout-d29-packet.md` (currency 2026-07-17)
holds the grammar evidence and disposition table.

## The ratified semantics you are implementing (LOG-derived; flag ANY mismatch)

1. **Postfix `!` is MANDATORY on every fallible call whose error channel ACTIVATES** —
   callee declared `throws E` (kind-1) OR declared return `Result[T,E]` (kind-2) — in every
   D23 position. Nested calls each carry their own mark (`g(f()!)!`).
2. **Dispositions attach to the marked expression:** propagate (bare `f()!` inside a
   `throws E` fn; kind-2 peels to `T` and propagates); `f()! catch (e): …` (peels, recovers);
   `f()! rethrow (e): …` (peels, transforms, propagates). Marked call with no viable
   disposition in a non-throws fn → `E_UnhandledThrows` (message flips per the pinned draft).
3. **CAPTURE (the amendment):** an EXPLICITLY `Result[T,E]`-annotated destination — binding,
   param, return — captures the UNMARKED call: `Result[int, Error] r = f()`. For kind-1 the
   call types as `Result[T,E]` in exactly that position (the D23 addendum); kind-2 already
   does. **Mark + Result-annotated destination = error** with the remove-the-`!` fix-it.
   **KIND-1 ONLY (pass-2 R4):** inferred/`auto` destinations do NOT capture a throws call —
   it types as `T`, mark required. (A KIND-2 `auto` bind is an ordinary Result VALUE flow —
   legal unmarked, types as `Result[T,E]` as today; the LOG's "kind-2 calls stay
   Result-typed everywhere" governs.) Match scrutinee of a KIND-1 call stays `T`-typed
   (mark required; bind first to match Ok/Error — kind-2 `match g():` matching the Result
   value stays legal as today).
4. **Bare fallible call anywhere else → NEW code `E_MissingFallibleMark`** (message teaches
   all three exits: mark / handle / capture; never surfaces `Result[` per the D23 contract;
   never fix-its toward a signature `!`). **Bare-DISCARD (statement position, outcome
   dropped) is illegal for BOTH kinds.**
5. Signatures: `throws E` unchanged; bare `int f()!:` parses + teaching-rejects (A31
   reservation); `! E` does not exist. `!=` needs no lexer change (maximal munch); fmt
   inserts bang-space where a mark would fuse.
6. Combinators need NO special predicate — they are calls on Result VALUES; a combinator's
   own declared Result return in a capture/value position follows the same rules as any
   kind-2 call (verify this falls out; if the checker needs a carve-out anyway,
   STOP-AND-REPORT — that would contradict the amendment's derivation).

## The work (Core #9: Rust C+LLVM + self-host + ggdef-within-subset, one round)

### Rust gg (the reference implementation)
- **Parser:** postfix `!` at the scout's proven binding power (proto: bp 35), producing a
  typed `Expr::Propagate` node; the 12 scout parser tests + the `!=` corner tests
  (`a()!=b` comparison; `f()! == b`; `f()!= b`) land as committed tests.
- **Typed metadata, not the proto's shortcut:** the signature-side `throws` field uses a
  typed `ThrowsSpec::{No, Inferred, Explicit(TypeId)}`-shaped representation — the
  prototype's `"!inferred"` STRING SENTINEL violates layering rule 2 and must NOT ship.
- **Checker — TWO ARCHITECTURALLY DIFFERENT rules (pass-1 R1: do NOT apply kind-1's rule
  to kind-2):**
  **KIND-1 (throws callee).** Chokepoint = `resolve_throws_call_type` (`typecheck.rs:~5499`;
  all 4 throws-call sites funnel — verified). Enforcement: unmarked throws call →
  `E_MissingFallibleMark` UNLESS captured; marked → peel to `T` + require a disposition;
  mark+capture → the dedicated error with fix-it. **The capture rule already has its
  machinery (pass-1 R5 — do not build from scratch):** `decl_type_hint` (`typecheck.rs:~563`;
  set at bindings ~:1499, call-arg params ~:1923/:2099, returns ~:3343) already drives
  `dest_is_result` at the throws chokepoint (~:5511-5513) — kind-1 capture EXISTS today;
  the work is re-scoping it per the amendment:
  (a) explicit-Result `dest_is_result` — KEEP, now legal UNMARKED (and mark+capture = the
  new error); (b) the `auto`/inferred capture (~:4047) — **REMOVE**: `auto r = f()` now
  types as `T` and requires the mark; (c) the match-Ok/Error-arm suppress
  (`arms_match_result_or_option`, ~:3243) — **REMOVE for kind-1**: `match f():` scrutinee
  stays `T`-typed, `match f()!:` peels (so Ok/Error arms are a CHECK ERROR — bind to a
  Result first); the same helper KEEPS working for kind-2 (matching a Result VALUE as
  today).
  **KIND-2 (declared-Result callee).** No expected-type rule at all — Result VALUE flows
  (bind, match, pass, chain, receiver) are ALL legal unmarked (the amendment). Enforcement
  is **bare-DISCARD-only, a STATEMENT-position property, not a call-node one**: an
  expression-statement whose value is a kind-2 call's un-consumed `Result` →
  `E_MissingFallibleMark` (mark to propagate, or attach a handler). Plus the `!`-path:
  a MARKED kind-2 call peels to `T` and activates the channel (prop/catch/rethrow) — this
  needs the classification point ("is this callee kind-2") built at the call-typing layer
  (the free-fn `else` ~:2079 + method fallbacks, `typecheck.rs:~2061-2065` never peel
  today) with an ARM-COUNT LINT (`tests/lints.rs`) so the next call-shape is forced through
  it (mirror the throws lint). Verify combinator calls fall out correctly with NO carve-out
  (a combinator is a kind-2 call whose Result is consumed by the chain — legal unmarked);
  a needed carve-out = STOP-AND-REPORT.
  **Terminology (disambiguation):** "unmarked call" = no `!` (the E_MissingFallibleMark
  domain); "marked bare statement" = `f()!` as a statement with no attachment (LEGAL in a
  throws fn: discard Ok, propagate Error — the original pin). Do not conflate.
- **THE TWO-LAYER TRANSPARENCY FIX (mandatory-gate finding, AMENDMENT-CORRECTED per pass-1
  R2):** the `Propagate` node eats the `suppress_auto_prop` one-shot in BOTH the typechecker
  AND the IR lowerer — the scout APPLIED both fixes for the `catch`/`rethrow` attachments,
  and those remain correct and required (`f()! catch` must type and RUN). **MARKED-MATCH SPLITS BY ARM TYPE
  (pass-3 R1 — do not conflate the two cases):** `match f()!:` peels the scrutinee to `T`.
  With **T-VARIANT arms** (the snag48 shape — `case Tagged.StringV(s)…` inside a throws fn)
  that is LEGAL and MUST RUN: this is the direct-scrutinee `Propagate` lowering, the ACTUAL
  Finding-5 SIGSEGV site, and it KEEPS its RUN regression net (the scout proved snag48
  stdout-identical under mechanical marking — that proof STANDS). With **Ok/Error arms** it
  is a CHECK ERROR (you matched the peeled `T` against Result variants — bind to a Result
  first; the amendment pin). **Acceptance gates therefore: (a) `f()! catch (e): …` types
  and RUNS; (b) `match f()!:` with T-VARIANT arms RUNS — the SIGSEGV pin, snag48's class;
  (c) `match f()!:` with Ok/Error arms → CHECK ERROR (NEG); (d) `Result[T,E] r = f()` then
  `match r:` RUNS (the capture POS pin); (e) the kind-2 marked variants of (a).**
- **Diagnostics:** `E_MissingFallibleMark` registered (`errors.rs`; registry count 96→97;
  `spec/prose/diagnostic-codes.md` row); `E_UnhandledThrows` message flip per the pinned
  drafts; smith/D23 ratchets gain the new code.
- **`gg fmt`:** mechanical insertion for the throws-kind marks (census-driven — the checker
  IS the oracle) + bang-space at `=`-adjacency; capture sites get NO mark.

### The migration (same round, second commit for bisectability)
- Regenerate the census with the final checker **under the AMENDMENT's rules (pass-1 R3 —
  the scout's 267 is STALE):** the scout's handled=206 INCLUDED throws-call Result-capture
  sites measured via `dest_is_result`, which are now UNMARKED captures — the true
  throws-kind mark count is 61 + (206 − the capture subset) **< 267**; a correct smaller
  number is NOT a regression. Report the fresh split (prop / catch / rethrow / captures-now-
  unmarked). Kind-2 = ONLY the bare-discard sites — COUNT them first and report before
  migrating (an unexpectedly large count is a STOP-AND-REPORT). Beware the numeric
  coincidence: 206 is ALSO the kind-2 declaration count — two different quantities; do not
  conflate in the regen.
- Run the fmt insertion across the corpus; every migrated fixture must build+run
  STDOUT-IDENTICAL. **⚠ THE AUTO-CAPTURE CLASS NEEDS A DIFFERENT TRANSFORM (pass-2 R1 —
  the mechanical `!` insertion is WRONG for it):** ~12 fixtures use
  `auto r = throws_call()` then `match r:` with Ok/Error arms (e.g.
  `test_error_handling.gg:19-24`, `error_raw_nested.gg:7-14`). Post-amendment the
  auto-capture is removed, and inserting `!` would peel `r` to `T` and BREAK the match —
  the correct migration is the REWRITE `auto r = f()` → `Result[T,E] r = f()` (explicit
  capture, UNMARKED; derive the concrete T/E from the callee's signature). fmt cannot
  derive this from the missing-mark diagnostic — enumerate the class by grep + checker
  triage, rewrite by hand or a dedicated script, and verify each rewritten fixture
  build+runs stdout-identical. **CLASSIFY THE MATCH MIGRATIONS BY ARM TYPE (pass-3 R1):**
  `snag48_throws_match_scrutinee.gg` has T-VARIANT arms — it gets the MECHANICAL `!` on the
  scrutinee (`match stringv_throws()!:`) and STAYS a POS run fixture (the scout's
  stdout-identical proof for it STANDS); only auto-capture-then-Ok/Error-match fixtures get
  the rewrite-to-explicit-capture. Triage the whole direct-scrutinee-match class
  (snag41/43/46/49*, d23_*) by the same arm-type rule; the rewrite class = kind-1
  throws-call auto-binds ONLY (a kind-2 `auto r = parse_int(s); match r:` is a legal value
  flow — no rewrite). **The scout's
  "test_error_handling.gg 14 marks stdout-identical" proof is PRE-AMENDMENT STALE**
  (mark+capture was legal then; it is now an error) — do not cite it; re-prove on the
  amendment-correct transforms.
- The pinned HARDENING FIXTURE: a stdlib-shaped thin local `throws` wrapper exercising
  always-mark + ALL dispositions (prop / catch / rethrow / unmarked-capture / the
  mark+capture error / bare-discard error) end-to-end — expected outputs are
  §-prose-derived, committed as run_gg + check_gg_fails wiring.

### ggdef lane (within subset)
Shares the production parser+AST (gets `Expr::Propagate` for free); GGC already evaluates
Propagate. Needs: the elaboration arm (production `Expr::Propagate` → GGC) + reject-bare
within its subset with the SAME E_ code — **plumbed as typed `Outcome::IllFormed` +
`reject_code` so the conformance rejects count MATCH on the GGDEF lane (see gate 8; a
generic FrontendError records only GGDEF-SKIP)** — + the capture-position acceptance. ~19 inline
`throws` unit tests flip (scout-measured); the full `cargo test -p ggdef` suite is a gate
(the Batch-A lesson). The cow/deadwrite corpus is throws-free — no corpus_b churn expected;
verify, don't assume.

### Self-host lane
Port: `EPropagate` variant + the postfix arm in `parse_expr_bp` (`parser.gg:~1634-1693`;
the lexer already splits `TkBang`/`TkBangEq` — corner free); signature `!:`
parse+teaching-reject; the enforcement in `typecheck.gg` **with the SAME kind-1/kind-2
ARCHITECTURAL SPLIT as the Rust section (pass-3 R3 — "mirror the decision point" does NOT
mean one rule): kind-1 = the throws-chokepoint re-scope (unmarked→error unless
explicit-Result-captured; marked→peel+disposition; mark+capture→error; auto doesn't
capture; match-arm suppress removed for kind-1), kind-2 = the bare-DISCARD statement-position
check ONLY (value flows stay legal unmarked)** — a kind-1-only mirror yields the kind-2
bare-discard conformance fixture rejected by C/LLVM but accepted by the self-host = a
Core-#9 lane divergence; **the SAME two-layer transparency in its lowerer**. The driver
tests pin BOTH kinds' rejects + the unmarked-capture accept + the T-variant marked-match
run. Follow the A2-S port
pattern (extend the existing walker; prototype+revert measurement style). The self-host
sources themselves contain zero throws decls — the migration does not touch them; the
DRIVER tests pin the lane (reject-bare + accept-marked + capture cases through the driver).

## Gates (FOREGROUND; self-host commands `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`;
chunk >10min by test name; NEVER background a final gate)
1. `cargo build` + `cargo test --lib` + the full parser suite.
2. The disposition matrix (both backends): RUN tests — prop / catch / rethrow /
   unmarked-capture / **capture-then-match (`Result r = f(); match r:` — the real SIGSEGV
   coverage)** / nested `g(f()!)!` / `a()!=b` / the kind-2 variants; NEG tests —
   mark+capture-error / bare-discard-error / **kind-1 `match f()!:` with Ok/Error arms
   (CHECK ERROR per the amendment — the pass-2-corrected gate; no marked-match RUN test
   exists anymore by construction)**.
3. The migrated corpus: full C sweep is the PARENT's; you run the migrated-fixture filter
   (every touched fixture builds + runs stdout-identical) on C AND LLVM.
4. `cargo test -p ggdef` full suite.
5. Self-host: driver suite incl. the new lane tests; `self_host_bootstrap_fixed_point`
   (parser/typecheck.gg on the bootstrap path; budget 900s).
6. `type_comparison`/`check_comparison` — the migration touches fixtures those nets compile;
   report the fresh counts vs the DOCUMENTED baselines (regenerate, don't trust dated
   figures); deltas explained-or-STOP.
7. Registry lint: `grep -cE '=> "E_' src/semantic/errors.rs` == the new documented count.
8. **Four-lane conformance (pass-1 R4, pass-2-R3-corrected — D29 changes accept/reject on
   every lane):** new `spec_conformance` fixtures for the flips (at minimum:
   unmarked-throws-call reject; unmarked-capture accept; mark+capture reject;
   kind-1-marked-match reject; bare-discard reject both kinds). **Floor handling SPLITS by
   fixture kind:** ACCEPT fixtures bump C/LLVM/SELFHOST (run-MATCH, all three); REJECT
   fixtures bump C/LLVM/SELFHOST via the `error[E_]` marker mechanism
   (`tests/spec_conformance.rs:~88-92,~367-389`), but on the GGDEF lane a reject counts
   MATCH only if plumbed as a typed `Outcome::IllFormed` + `reject_code` — a generic
   FrontendError records GGDEF-SKIP (the E_BorrowConflict precedent,
   `spec_conformance_ggdef.rs:~58-60`); prefer the typed plumbing, else a documented SKIP.
   The GGDEF floor is a SEPARATE constant/value from the C/LLVM/SELFHOST 214s. There is no
   5th MATCH-floor lane, **but `MIN_FIXTURES = 214` EXISTS** (`tests/spec_conformance.rs:~152`,
   the total-seed-count guard documented to EQUAL the three MATCH floors) — every new seed
   (accept AND reject) increments the corpus, so bump `MIN_FIXTURES` in lockstep (the `>=`
   stays silently green if forgotten — that is documented-invariant staleness). Bump exactly
   the constants that exist, with regenerated counts, same commit.
9. **Emitted-shape / anchored-test grep (pass-1 R4):** before the gates, grep `tests/` for
   anchored landmarks a new parser node + error code can trip (snapshot tests, message
   asserts on the old E_UnhandledThrows wording, smith ratchet expectations); list what you
   regenerated.

## Sizing note (honest)
This is the largest track since the enforcement wave: parser+checker+fmt+three lanes+a
corpus migration. If mid-execution it proves too large for one executor run, STOP-AND-REPORT
with a proposed commit-split (machinery / migration / self-host port) rather than rushing —
the brief already mandates two commits; three sequenced commits on one branch is acceptable.

## Bookkeeping (final commit)
- TODO: the D29 IMPLEMENTATION-READY entry retires to DONE (datestamped, with the measured
  matrix + census counts); the readability-census C3 pointer stays; A31/A32 stay queued.
- decisions.md: NO new pins expected — if implementation forces ANY semantic choice the LOG
  doesn't cover, STOP-AND-REPORT (owner's call, not yours).
- Stage explicitly (enumerate; never `-a`). Trailers:

      Co-Authored-By: Claude Opus <noreply@anthropic.com>
      Claude-Session: https://claude.ai/code/session_01TYkkHveF8WhhTVX4DjbCTN

- Checkpoint `/tmp/d29_exec_progress.md` after every gate.

## Zones
Rust `src/lexer|parser|semantic|formatter` + `spec/ggdef/src` + `tests/**` + the self-host
parser/typechecker files. The RV-B executor is concurrently in `src/semantic/safety/**` +
the self-host `parser.gg`/`typecheck.gg`/lowerer arms — **if RV-B has not INTEGRATED when
you launch, STOP: this brief assumes RV-B's EDotShorthand CallArg shape is in your base**
(your parser.gg edits would collide). The orchestrator confirms sequencing at launch.

Final message: commit hash(es) + branch, the full disposition matrix results × lanes, the
census counts (throws-kind marks placed; kind-2 bare-discard count), every gate verbatim,
staged list, smells.
