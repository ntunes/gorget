# EXECUTOR BRIEF — D29 implementation: call-site `!` (visible error propagation), all lanes

**Status:** DRAFT — in the ≥3-fresh-pass review gauntlet. Do not execute until a clean pass.
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
   Inferred/`auto` destinations do NOT capture (type as `T`, mark required). Match scrutinee
   stays `T`-typed (mark required; bind first to match Ok/Error — kind-2 `match g():`
   matching the Result value stays legal as today).
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
- **Checker:** kind-1 chokepoint = `resolve_throws_call_type` (`typecheck.rs:~5499`; all 4
  throws-call sites funnel — verified). Kind-2 has NO chokepoint (`typecheck.rs:~2061-2065`
  never peels) — BUILD one: a single shared decision point for "this call is fallible"
  covering the free-fn `else` (~:2079) + the method fallbacks, with an ARM-COUNT LINT
  (`tests/lints.rs`) so the next call-shape is forced through it (mirror the throws lint).
  Enforcement: bare fallible call → `E_MissingFallibleMark` UNLESS the expected-type at an
  explicitly-annotated binding/param/return position is `Result[T,E]` with matching E
  (capture — no peel, no mark); marked call → peel to `T` + require a disposition;
  mark+capture → the dedicated error with fix-it.
- **THE TWO-LAYER TRANSPARENCY FIX (mandatory-gate finding):** the `Propagate` node eats
  the `suppress_auto_prop` one-shot in BOTH the typechecker AND the IR lowerer — the scout
  APPLIED both fixes; without them `f()! catch` mis-types and `match f()!:` passes `gg
  check` then SIGSEGVs. **A marked-match-scrutinee RUN test is an acceptance gate.**
- **Diagnostics:** `E_MissingFallibleMark` registered (`errors.rs`; registry count 96→97;
  `spec/prose/diagnostic-codes.md` row); `E_UnhandledThrows` message flip per the pinned
  drafts; smith/D23 ratchets gain the new code.
- **`gg fmt`:** mechanical insertion for the throws-kind marks (census-driven — the checker
  IS the oracle) + bang-space at `=`-adjacency; capture sites get NO mark.

### The migration (same round, second commit for bisectability)
- Regenerate the census with the final checker: throws-kind = 61 prop + 206 handled = 267
  marks expected (fixtures + spectests; lib has ZERO throws decls); kind-2 = ONLY the
  bare-discard sites (the amendment leaves bind/match/pass/chain sites untouched) — COUNT
  them first and report before migrating (an unexpectedly large count is a STOP-AND-REPORT).
- Run the fmt insertion across the corpus; every migrated fixture must build+run
  STDOUT-IDENTICAL (the scout proved the mechanics on 41 marks; now the full set).
- The pinned HARDENING FIXTURE: a stdlib-shaped thin local `throws` wrapper exercising
  always-mark + ALL dispositions (prop / catch / rethrow / unmarked-capture / the
  mark+capture error / bare-discard error) end-to-end — expected outputs are
  §-prose-derived, committed as run_gg + check_gg_fails wiring.

### ggdef lane (within subset)
Shares the production parser+AST (gets `Expr::Propagate` for free); GGC already evaluates
Propagate. Needs: the elaboration arm (production `Expr::Propagate` → GGC) + reject-bare
within its subset with the SAME E_ code + the capture-position acceptance. ~19 inline
`throws` unit tests flip (scout-measured); the full `cargo test -p ggdef` suite is a gate
(the Batch-A lesson). The cow/deadwrite corpus is throws-free — no corpus_b churn expected;
verify, don't assume.

### Self-host lane
Port: `EPropagate` variant + the postfix arm in `parse_expr_bp` (`parser.gg:~1634-1693`;
the lexer already splits `TkBang`/`TkBangEq` — corner free); signature `!:`
parse+teaching-reject; the mark/capture enforcement in `typecheck.gg` mirroring the Rust
decision point; **the SAME two-layer transparency in its lowerer**. Follow the A2-S port
pattern (extend the existing walker; prototype+revert measurement style). The self-host
sources themselves contain zero throws decls — the migration does not touch them; the
DRIVER tests pin the lane (reject-bare + accept-marked + capture cases through the driver).

## Gates (FOREGROUND; self-host commands `GG_BUILD_TIMEOUT_SECS=600 GG_TEST_TIMEOUT_SECS=600`;
chunk >10min by test name; NEVER background a final gate)
1. `cargo build` + `cargo test --lib` + the full parser suite.
2. The disposition matrix as RUN tests (both backends): prop / catch / rethrow /
   unmarked-capture / mark+capture-error / bare-discard-error / marked-match-scrutinee
   (the SIGSEGV pin) / nested `g(f()!)!` / `a()!=b` / kind-2 variants of each.
3. The migrated corpus: full C sweep is the PARENT's; you run the migrated-fixture filter
   (every touched fixture builds + runs stdout-identical) on C AND LLVM.
4. `cargo test -p ggdef` full suite.
5. Self-host: driver suite incl. the new lane tests; `self_host_bootstrap_fixed_point`
   (parser/typecheck.gg on the bootstrap path; budget 900s).
6. `type_comparison`/`check_comparison` — the migration touches fixtures those nets compile;
   report the fresh counts vs the 85/86 baselines; deltas explained-or-STOP.
7. Registry lint: `grep -cE '=> "E_' src/semantic/errors.rs` == the new documented count.

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
