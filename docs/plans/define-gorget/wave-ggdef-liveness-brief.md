# Executor Brief — ggdef liveness transition-table + the three-layer oracle boundary

**Track:** ggdef definition-integrity (owner-ruled HIGH, run now, ahead of Batch C). **Base:** main.
**Deliverable:** land the proven ggdef transition-table fix (revive + consume-call kill) AND document the
three-layer oracle boundary the audit surfaced AND wire the two operational riders that fall out of it —
so the fix closes the definition-integrity hole *and* makes the boundary self-enforcing. Contained to
`spec/ggdef/` + `spec/spectests` + `docs/` + fixtures. **NOT bootstrap-gated** (ggdef is a Rust crate; no
`gg`/self-host rebuild).

## 0. WORKTREE PREAMBLE (non-negotiable)
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm both inside your worktree. NEVER touch
`/workspace/gorget` (main) or `/workspace/gorget-1`. Worktree-RELATIVE paths only (an absolute
`/workspace/gorget/...` path writes into MAIN). Stage by explicit file name (NEVER `git add -a`/`commit -a`).
NEVER `git stash` — `git diff > /tmp/ggdefexec_<name>.patch`. Checkpoint to /tmp EARLY. Run FINAL gates
FOREGROUND with generous timeouts. On an Edit desync, re-Read + retry the Edit tool — never a shell heredoc
with an absolute path. After any non-Edit write, `git -C /workspace/gorget status` and STOP if it shows changes.

## 1. GROUND IN THE DOCS + THE PROVEN SCOUT
- Scout report `docs/plans/define-gorget/scouts/scout-ggdef-liveness.md` — the complete transition table,
  every-cell measurements, the branch-merge finding, the fix design, the migration plan. READ IT.
- Proven prototype `docs/plans/define-gorget/scouts/patches/ggdef-liveness-fix-proto.patch` — applies CLEAN;
  4 files under `spec/ggdef/src/`, +175/-8; ggdef suite 127/0. This is the CODE fix — apply it as your base.
- `docs/language-design.md` (ownership/move semantics); the reference sections you'll touch (below).

## 2. THE CODE FIX (apply the proven patch, verify)
`git apply docs/plans/define-gorget/scouts/patches/ggdef-liveness-fix-proto.patch`. It:
- adds typed `Ty::Callable{consuming}` resolved ONCE in `ty_of_type` (`ConsumeCallable`→true; `Callable`/
  `MutCallable`→false), read by `CallValue` — a consume-call kills the callee slot; a 2nd call is a
  double-move. (Rule 2/4: the surface type NAME is matched once at type-resolution into a typed field, read
  downstream — the CORRECT layering, not a call-site name-match. Confirm this is how it reads.)
- `resolve_write` Moved arm → `Revive` for a whole-local reassignment; stays IllFormed for a projected write.
- +6 tests (revive, revive-then-move-again, projected-write-stays-illformed, consume-double→IllFormed,
  consume-once-legal, plain-Callable-reusable).
Verify: `cargo build -p ggdef`; `ggdef run tests/fixtures/liveness/reinit_accept.gg` → exit 0 "new";
`ggdef run tests/fixtures/liveness/consume_callable_double_reject.gg` → exit 102; `cargo test -p ggdef` green.

## 3. WRITE-THROUGH PROSE (owner-ratified framing — get the wording exactly right)
**Two of the three normative rules ALREADY EXIST — verify + reference them, do NOT re-add:**
- Re-init revival: `docs/language-reference.md:1118` ("Reassigning a moved variable revives it"). The code
  fix brings ggdef INTO conformance with this — ggdef was lagging its own prose (an intra-definition
  disagreement; see the boundary note).
- May-move merge rule: `docs/language-reference.md:2390` ("if a variable is moved in any branch, it is
  treated as moved after the branch point"). THIS is the normative owner of the static conditional-move axis.

**(a) ADD the ConsumeCallable single-use sentence** — §4.2 Callable Trait Types (after the coercion-hierarchy
bullets, ~`:461`; the terse `# consuming: … (single use)` comment at ~`:455` is not a rule):
> A `ConsumeCallable` is **single-owner**: calling it consumes the callable (its captured environment is
> moved out), so it can be called **at most once**. A second call — or any use after the call — is a
> compile-time **use-after-move** (`error[E_DoubleMove]`), exactly as for any other moved value. `Callable`
> and `MutCallable` are reusable and carry no such restriction.

**(b) ADD the THREE-LAYER ORACLE BOUNDARY note** — this is the reference-grade closure of the audit and is
owner-SIGNED-OFF *with the ownership wording below*. Place it where the project's normative model lives:
a ratified entry in `docs/plans/define-gorget/decisions.md` (the ledger) AND a one-paragraph note in the
ggdef spec docs (find the ggdef README / spec doc under `spec/ggdef/` or `docs/`). The model has THREE
layers — **do not collapse to two, and do NOT write that implementations own any axis** (implementations
NEVER own normative semantics — ending implementation-defined semantics is the project's whole purpose):
> 1. **ggdef-eval is the sole oracle for DYNAMIC semantics.** A Value/trap/output verdict on a concrete
>    execution path is normative, full stop.
> 2. **ggdef-elaborate is the STATIC oracle for the position-based axes it models** — D10 place-overlap,
>    D12 positions, D10(a) binds, throws totality. This is how the enforcement wave gates; it is NOT demoted.
> 3. **Flow-sensitive static judgments ggdef structurally cannot model are normatively owned by PROSE +
>    SPECTESTS derived from it — never by an implementation.** This is an EXPLICIT, ENUMERATED list;
>    **currently it has exactly ONE member: conditional-move / may-move liveness** (the merge rule at
>    reference `:2390`). Production and the self-host CONFORM to that prose rule; their negative fixtures are
>    generated FROM it; they do not define it. **Adding an axis to this list is an OWNER DECISION** — so
>    "ggdef doesn't model it" can never become a lazy escape hatch for a future track.
> Why the list exists: ggdef is a fuel-bounded big-step interpreter (one concrete path per run) — its
> smallness/determinism/auditability is what makes it trustworthy. A conditional move like `if c: sink(!x)`
> then `use x` is memory-safe on the path where `c` is false, so ggdef-eval correctly returns `Value`; the
> static checkers reject it CONSERVATIVELY per the `:2390` rule. This is the inherent, one-directional
> static-vs-dynamic gap — NOT a ggdef bug, and NOT to be "fixed" with all-paths exploration (that would
> destroy the property that makes ggdef trustworthy).
Add one line noting the re-init bug's nature: *it was an INTRA-DEFINITION disagreement (the reference prose
said "revives"; ggdef-eval said IllFormed) — the definition's own twin-drift. The triple (prose, ggdef,
spectests) must agree; a disagreement is a defect in the DEFINITION regardless of which artifact is right.*

## 4. FIXTURE MIGRATION (close the KNOWN-ORACLE-BUG pins)
1. Strip the `KNOWN-ORACLE-BUG` headers from `tests/fixtures/liveness/reinit_accept.gg` (:3-5) and
   `consume_callable_double_reject.gg` (:4-6). Rewrite the comment block at `tests/integration.rs:~18855-18877`
   from "two axes go BEYOND ggdef… filed HIGH/MED" to "now AGREE with ggdef (fix landed)". KEEP the
   `self_host_driver_{accepts,rejects}_liveness` assertions (they pin the self-host lane).
2. Add TWO hand-authored `spectests/run/` seeds — `liveness_reinit_accept.gg` and
   `liveness_consume_callable_double.gg` — with `#!spectest` / `# mode: run` / `# adjudicator: ggdef`
   frontmatter; run `cargo run -p ggdef -- gen spectests/run/<f>.gg` to fill `expect:` (reinit → exit 0
   stdout "new\n"; consume-double → exit 102 / no trap). **The IllFormed reject seed MUST be hand-authored**
   — the automated `ggdef migrate` only takes the Value/Agree bucket (`classify.rs`), never an IllFormed seed.
3. Bump `GGDEF_MATCH_FLOOR` 195→197 in `spec_conformance_ggdef.rs` in the SAME commit.

## 5. RIDER 1 — the `static-only` frontmatter discriminator (decided NOW, per owner)
No current fixture is affected (rejects pin the condition so the move is on the sampled path — STATE this as
a **fixture-authoring rule**: when writing a liveness reject fixture, make the violating path the EXECUTED
path wherever possible, so the ggdef lane and the static lanes agree). BUT the first genuinely-conservative
fixture (maybe-moved, statically REJECTED, `Value` under ggdef by the `:2390` gap) will MISMATCH the ggdef
lane BY DESIGN. Add a frontmatter tag (`static-only:` or similar — match the existing frontmatter grammar) to
the spectest harness so a tagged case's **ggdef lane expects `Value`/clean while the production/self-host
static lanes expect the error**. Find the frontmatter parser + the ggdef-lane expectation logic (grep the
spectest harness / `spec_conformance` / the `#!spectest` parser). Implement the mechanism + ONE example/test
exercising it (a synthetic maybe-moved case). If the harness structure makes this materially different from
described, REPORT it — don't force a wrong shape.

## 6. RIDER 2 — convert the boundary's soundness direction into a guard (Core #6, per owner)
The gap is ONE-DIRECTIONAL: static may over-reject relative to a single path, but **a statically-ACCEPTED
program MUST run dynamically clean under ggdef on every input** — an accepted program that hits a liveness
`IllFormed` in ggdef is a static-checker SOUNDNESS bug, and that is mechanically checkable. The smith infra
already has a ggdef verdict lane (grep for **P1-E** / the smith ggdef adjudicator). Add a tier that runs
`gg check`-ACCEPTED move-shaped programs under ggdef-eval and screams **SPEC-DIVERGE** on any dynamic
liveness violation. This turns the prose boundary into an executable guard (Core #6) — it is what keeps the
"conservative static checkers" clause from ever drifting into "unconstrained static checkers." Find the smith
tier structure; add the tier + wire it into the smith run. If the smith infra can't express this cleanly,
REPORT the gap with what's there.

## 7. FILE (do NOT scope in) — a LOW idea
Append to `TODO.md` (Low): *a cheap audit pass that diffs the reference's normative move/ownership sentences
against ggdef-eval behavior would have caught the re-init intra-definition twin-drift — file as a LOW idea
for a future prose↔ggdef conformance auditor.* One entry; do not build it here.

## 8. GATES (agent, FOREGROUND)
- `cargo build -p ggdef` + `cargo test -p ggdef` (expect 127+/0 lib incl. your new tests + all 8 integration
  binaries green, esp. `spec_conformance_ggdef` at the bumped floor 197).
- The 2 migrated spectests pass; `ggdef run` on both fixtures gives the correct verdicts.
- Rider 1's example case + Rider 2's tier run green.
- `cargo test --lib` (Rust unit) stays green.
The PARENT runs the broader `cargo test` sweep if the change touches shared crates; ggdef is self-contained,
so the ggdef suite + spectests + smith tier are the load-bearing gates (NO self-host bootstrap needed).

## 9. FINAL REPORT
Commit hash(es) + summary; the ggdef suite result (counts) + conformance floor; the 2 fixtures' verdicts
(reinit→Value/"new", consume-double→IllFormed); confirmation the boundary note uses the THREE-layer +
prose/spectests-own-it wording (NOT implementations-own-it); Rider 1 mechanism + example; Rider 2 tier +
its result; the filed LOW idea; anything that didn't fit the described mechanism (riders) flagged LOUDLY;
confirm `git -C /workspace/gorget status` CLEAN.
