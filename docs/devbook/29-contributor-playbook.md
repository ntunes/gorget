# Working on the compiler: a contributor's playbook

*Verified against commit `7d3350a0` (branch `worktree-agent-a3fe15a62bbab46c4`).*

The rest of this book tells you how the compiler *is* built. This chapter tells
you how to *work on it without breaking it* — the hard-won rules in
`CLAUDE.md`/`AGENTS.md`, turned from a rules-list into the reasoning behind
them, with worked examples from real bugs in this tree. If you read one chapter
before touching the compiler, read this one and [Chapter 24
(layering discipline)](24-layering-discipline.md) together: 24 states the law,
this chapter is the how-to. The *process* side — rounds, the review gauntlet,
orchestration — has its own extended chapter, [Chapter 30 (the excellence
system)](30-excellence-system.md).

The single most useful instinct to internalize is this:

> **When the fix you are sketching is intrinsically complex, you are almost
> certainly fixing the wrong layer.**

Everything below is a corollary of that.

---

## The debugging heuristic: fix-complexity is a signal of the wrong layer

You have localized a bug. The fix you are about to write needs to save and
restore state around branches, insert a phi at a merge, thread a
scope-tracking name map, or repair SSA by hand. **Stop.** In a well-layered
compiler, real bugs are almost never multi-case rules at a *read* site — they
are one-line oversights at a *write* site one layer up (`AGENTS.md` →
"Debugging heuristic"). The complexity you are feeling is the symptom resisting
you, not the disease.

The procedure (`AGENTS.md` § "Debugging heuristic"):

1. **Trace the data the buggy site reads. Where was it last written?**
2. **Look at the writer. Did it respect every typed fact available to it?** Or
   did it default, hardcode, or collapse cases that an upstream pass had already
   distinguished?
3. **Writer was lossy → fix it at the source.** The "complex fix" you were
   sketching at the read site evaporates — the bad input never arrives.
4. **Writer was faithful → trace one more layer up.** Repeat.

The counterintuitive part is step 4's emotional content: *every layer hop that
doesn't find the bug should make you more suspicious of your diagnosis, not
less.* If you are three layers deep and still want to write a save/restore
patch, the diagnosis is wrong, not the architecture. Two real snags from this
tree show the pattern end to end.

### Worked example — Snag #17: a chained `substring` corrupting a later `parse_float`

**The symptom.** A program that did `text.substring(a, b)` and *later*
`parse_float(text)` got a corrupted `text` at the `parse_float`. It looked like
a control-flow problem: a string view rebound across a CF merge.

**The tempting fix.** The obvious suspect was `cow_materialize_alias` — the
machinery that materializes a string view when its source is mutated (see
[Chapter 11](11-copy-on-write.md)). Sketching a fix there meant tracking the
view's rebinding across the control-flow merge between the two statements: 50+
lines of save/restore-around-branches. That is precisely the "intrinsically
complex" smell.

**The real bug, one layer up.** `substring` is a builtin method, declared once
in the protocol table as a `BuiltinMethodDecl`:

```rust
BuiltinMethodDecl { name: "substring", runtime_callee: Some("gorget_str_slice"),
    self_conv: SelfConvention::Borrow, …, returns_view: true, … }
```

(`src/ir/lowering/builtins.rs:694`). It *borrows* `self` (`self_conv:
Borrow`) and returns a *view* (`returns_view: true`). The return-type resolver,
`resolve_builtin_method_return_type` (`src/ir/lowering/context.rs:658`), read
the declaration to type the result — but ignored the `self_conv` flag, so it
modeled the call as if it *consumed/mutated* `text`, triggering a bogus
materialization of `text`'s buffer. The materialization is what corrupted the
later `parse_float(text)`. The rebind across the CF merge was downstream
*fallout*, not the cause.

**The fix.** Five lines at the *writer* — teach the return-type resolver to
honor `self_conv` (a `Borrow`/`MutBorrow` self is received by pointer and not
consumed; the field that records this is `self_by_ptr`, set from `self_conv` at
`context.rs:647`). With the writer corrected, the bogus materialization never
fires, and the 50-line "rebind across merges" fix is **never-taken code** — it
was patching a symptom of a value that should never have been materialized
(`AGENTS.md` § "Debugging heuristic").

The lesson is the heuristic verbatim: the read site (`cow_materialize_alias`)
*looked* buggy because it was being handed input it should never have received.
The fix was at the writer (`resolve_builtin_method_return_type`), which had
dropped a typed fact (`self_conv`) that was sitting right there on the
declaration.

### Worked example — Snag #13: a Box-recursive enum linking to an undefined allocator

**The symptom.** A `Box[T]`-recursive enum (e.g. `Expr → Box[SpannedExpr] →
SpannedExpr → Expr`) compiled to C that referenced `__gorget_box_alloc_<inner>`
and a per-type `Box__<inner>__drop` wrapper that were *never defined* — a link
error.

**The tempting fix.** Make the emitter that generates those helpers scan the
recursive-drop tables for `Box__X__drop` entries and synthesize the missing
allocators. That is **name-matching** — pattern-matching on a runtime-symbol
string to recover a semantic fact — which is exactly what `CLAUDE.md` → "No
name matching" forbids, and exactly the smell the layering-discipline litmus
flags ([Chapter 24](24-layering-discipline.md)).

**The real bug, one layer up.** The `StructDef` for a regular `Box[T]` is
registered during LIR lowering, and at registration time the inner type `T` is
*known* — but the LIR `StructDef` had no field to carry it, so the inner type
was *lost at the layer boundary*. The C backend, needing to emit
`__gorget_box_alloc_<inner>`, had no typed source for `<inner>` and was reduced
to fishing it out of names.

**The fix.** Add a typed field — `box_inner_type: Option<String>` on the LIR
`StructDef` (`src/lir/mod.rs:1541`) — **set once at the writer**, the Box
registration site:

```rust
box_inner_type: Some(inner_name.to_string()), …
```

(`src/lir/lower/mod.rs:940`), and **read at the consumer**, the C box-helper
emitter:

```rust
for sd in &module.structs {
    if let Some(inner) = &sd.box_inner_type { … box_inners.push(inner.clone()); }
}
```

(`src/backend/c_lir/emit_types.rs:1403-1408`). The inner type now crosses the
GIR→LIR→backend boundary as a typed field instead of being reconstructed from a
`Box__`-prefixed name. This is "resolve once, write through"
([Chapter 24](24-layering-discipline.md), rule 4) applied at the runtime-symbol
boundary.

And then the fix is *locked*: a dedicated LIR invariant
`validate_box_inner_type` (`src/lir/validate.rs:788`) asserts that *every*
regular Box `StructDef` carries the metadata, and its inverse
`validate_box_inner_type_consistency` (`:855`) asserts no *non*-Box struct
carries stray Box metadata. The validators run after each LIR pass, so a future
registrar that forgets to set the field fails loudly at compile time rather than
producing a silent link error — see [Chapter 25 (structural guards)](25-structural-guards.md)
§1d. (The validator's own doc comment cites snag #13's commit, `c7a652f0`.)

### What the two snags have in common

Both bugs *looked* like they lived at the read site (a materialization rebind; a
helper emitter). Both were really a **typed fact dropped at the write site one
layer up** (`self_conv` ignored; `box_inner_type` not carried). Both
"obvious" fixes were the forbidden shape — complex save/restore for #17,
name-matching for #13 — and both evaporated once the writer was corrected. When
your fix is ugly, the ugliness is the diagnosis.

---

## Don't redesign around compiler gaps

Sometimes the bug is real and you *can't* fix it right now. The rule
(`AGENTS.md` → "Don't redesign around compiler gaps") allows exactly two
responses, and forbids a tempting third:

1. **Fix the gap.** The default — most gaps are a missing default-move or a
   dropped typed fact, i.e. the writer-fix above.
2. **Write a failing fixture that exposes the gap, plus a sharp `TODO.md`
   entry citing it.** Wire it `#[ignore]` if leaving it red would block other
   work — *but the fixture's expected output must encode what the language
   should do, not what it currently does.*

**Forbidden:** reshaping the surrounding code — tests, fixtures, examples, even
production code — to *avoid* the gap. Even a commented workaround does harm,
because the wired-in expected output (or the surviving workaround idiom) becomes
the load-bearing artifact, and a "passing" test then locks buggy behavior in as
canonical (`AGENTS.md` § "Don't redesign around compiler gaps").

This tree has been burned by exactly this. The `Dict.len()` workaround
(`scores.keys().len()`) outlived the bug it dodged by ~8 weeks, documented only
in a fixture comment, so the redesign quietly became the recommended idiom. A
`!`-param drop-at-exit leak got hidden for a day because a canonical fixture was
rewritten to use locals instead of `!` params — and when the bug was finally
fixed, three masked-leak tests needed their expected output updated
(`AGENTS.md` § "Don't redesign around compiler gaps"). The inverse failure mode is the same rule read
backwards: a stale `TODO.md` entry (the Phase A `collection_runtime_type`
migration) once described work that foundation commits had *already* completed —
and refusing to *manufacture* migration work to fit the stale premise is itself
an instance of this rule (verify the gap still exists before acting on it).

**The litmus test** (`AGENTS.md` § "Don't redesign around compiler gaps", litmus): if a fixture uses a more complex shape
than seems necessary, or a comment cites a bug as the reason for a workaround,
*ask why* — and **re-verify the bug still exists** before treating the
workaround as canonical. Which is the next rule.

---

## Self-host as the elegance showcase — and retiring fossils

The self-host frontend (`tests/fixtures/self_host_*`,
[Chapter 26](26-self-host-frontend.md)) is not just a stress test and a
regression net; it is the language's **reference-grade demonstration**. It must
read like the user manual — idiomatic Gorget, the way the language looks when
it's *working* — not the way it had to be written to dodge a compiler bug six
months ago (`AGENTS.md` → "Self-host as the elegance showcase").

Defensive code with a stale justification is **technical debt with a false
historical record.** The bug gets fixed; the workaround stays; the comment
explaining "why the parallel vector / extra clone / wrapper function" now lies
about the present. New contributors read the workaround as canonical style,
copy it, and the rot spreads. This tree has carried real fossils: a
`StructRegistry` with parallel `Vector[String]` + `Vector[int]` and an O(n)
scan, kept "because callers iterate in insertion order" long after the
Dict-ordering bug it dodged was fixed; a `type_info_keys_safe` wrapper whose
*entire purpose* was to dodge a state-loss bug that no longer exists; and
`# parallel storage to dodge Dict[String, _] state-loss` comments scattered
across `lower.gg`, each naming a Dict-ordering/state-loss bug long since fixed
(`AGENTS.md` § "Self-host as the elegance showcase").

The operating rules:

1. **No defensive code without a live, cited bug.** Find a "parallel because…"
   or "wrapper to avoid…" comment? Verify the bug still exists. If it doesn't,
   delete the workaround and write the idiomatic shape.
2. **Self-host reads like `docs/book/`.** If you wouldn't recommend the pattern
   there, don't write it in the self-host.
3. **A fix is incomplete until its dodge is retired.** When you fix a gap,
   `grep` for the workaround across *all* self-host directories before declaring
   the fix shipped. (Note the self-host dirs share files by symlink — e.g.
   `self_host_lowerer`'s `parser.gg`/`ast.gg` are symlinks into
   `self_host_typechecker` — so a single edit can land in several drivers, and a
   non-symlinked copy in another driver can silently diverge. `md5sum` to check.)

This rule is the mirror image of "don't redesign around gaps": that one is about
not *creating* new dodges; this one is about *retiring old ones*.

---

## Re-verify a premise against current source before acting

Diagnoses, plans, dated comparison scores, and `TODO.md`/`MEMORY.md` notes go
**stale the moment they are written** (`AGENTS.md` § "Solution Quality"). Before you act on a
load-bearing fact, confirm it still holds against *current* source and tests —
re-run the `*_comparison` test for a score, re-read the cited source for a
claimed "bug," check the actual code shape rather than a remembered one.

This tree has repeatedly burned cycles on stale premises: a "resolver at 57%"
that was actually 96% by the time someone acted on it; an "unshipped f-string
port" that had already shipped; a "live function-type bug" already fixed; a
"cleanup target" whose fossils were already retired (`AGENTS.md` § "Solution Quality"). Do not
trust a dated figure or an agent's unverified conclusion — cross-check first.
The book itself lives by this rule: every chapter's `file:line`s are re-derived
at authoring time and stamped "verified against `<commit>`"
([Chapter 0](00-how-to-read.md)), precisely because a transcribed figure is
presumed stale.

The cheapest re-verify is checking whether the work *already landed*. Two recent
saves: a Vector-`.slice()`/`.substr()` return-type fix was re-confirmed
present-in-current-source before being acted on as "open" — the commit was
already an ancestor of `HEAD` and its snapshot already existed (DONE.md `07b058c9`,
re-verified by scout); and an LLVM-backend batch (FIX A `85d9fecd`, FIX B
`2d720077`) was re-checked under `--backend=llvm` and found *passing* before
anyone re-implemented it (DONE.md / TODO.md "LLVM BACKEND → GREEN"). Re-running
the failing case for thirty seconds is always cheaper than re-deriving a fix that
already shipped. A third from the same round: a "static-literal initializers
miscompile under LLVM" lead turned out *already fixed* — the synthesized
`__gg_static_init_*` ABI guess had been replaced with the typed
`needs_sret(return_type)` predicate by commit `091faaef` ("LLVM static-init
ABI / TaskGroup opaque-handle sizeof"), so the bug the note described no longer
reproduced. The corollary: a "live bug" or "open task" inherited from a dated
note is a *hypothesis*, not a fact — reproduce it on the current tip first.

Re-verifying cuts the *other* way too: a "this is dead code, just delete it"
premise can be as stale as a "this is a live bug" one, and the cheap check is the
same — *try the delete and see what the suite says.* A scout was handed the
premise that `is_unmonomorphized_wrapper` was unreachable and could simply be
removed; a bare delete was **refuted** — it re-introduced 14 spurious
`ReadGuard__T` / `WriteGuard__T` typedefs, because the predicate is still live
through the opaque-`TypeDef` registration path that suppresses those generic
placeholder emissions (scout `ae8738a0`, which does not resolve in this worktree
— it was a read-only audit, not committed here). "Obviously dead, delete it" is a
claim to verify by deleting and rebuilding, not a fact to act on; a function with
no *direct* call can still be reached through a registration or dispatch table.

A corollary specific to *this* book and the self-host: **the comparison scores
are not facts you can quote.** The next section is why.

---

## The gates: comparison tests and the fixed-point loop

Two families of test guard correctness as you work; both are easy to misread.

### `*_comparison` is diagnostic-only — green says nothing

The self-host parity tests — `lexer_comparison`, `parser_comparison`,
`resolver_comparison`, `type_comparison`, `check_comparison`,
`lowerer_comparison`, `c_emit_comparison` — are **diagnostic-always-pass**. Each
ends with a `// Diagnostic test — always passes` comment and has *no `assert!`
on the matched count*; the only assertions are setup invariants like "fixtures
dir is non-empty" ([Chapter 27](27-comparison-bootstrap.md)). **A green
`cargo test` therefore tells you nothing about parity.** The signal is the
matched/mismatched count printed to stderr, and you only see it with
`--nocapture`:

```bash
cargo test --test integration <name>_comparison -- --nocapture 2>&1 | tee /tmp/cmp-$RANDOM.log
# then read the "=== <Name> Comparison Results ===" block
```

This is *by design*: the parity gap is the work, not a regression — turning it
red would make every routine `cargo test` fail and the suite useless as a signal
for everything else ([Chapter 27](27-comparison-bootstrap.md)). The practical
consequence for you: **never quote a parity figure from memory or a dated doc;
re-run and read the printed count.** (This is the previous section's
re-verify-a-premise rule, made concrete for the one number most often quoted
stale.)

### `self_host_bootstrap_fixed_point` is a milestone, not the finish line

The bootstrap fixed-point test proves the self-host can recompile *itself* to a
fixed point — stage-2 == stage-3 == stage-4, byte-identical
([Chapter 27](27-comparison-bootstrap.md)). When it is green, the self-host is a
*usable* compiler that reproduces itself. That is a closed loop and a real
milestone — but it is **not** parity with Rust. The north star is the
*comparison* counts climbing toward 100% (the self-host compiling every fixture
*the same way* Rust does), not a green fixed-point or a green suite
(`MEMORY.md` → "NORTH STAR"; [Chapter 27](27-comparison-bootstrap.md)). Do not
treat a green gate as the stopping condition.

### The layering ratchet

One more gate runs at lint time and enforces the "no name matching" rule
mechanically: `no_growth_in_name_prefix_routing` (`tests/lints.rs:166`) counts
the `starts_with("X__")`-style name-prefix routing sites in the compiler tree
and fails if the count *grows* (`count_name_prefix_sites`, `:47`). It is a
one-way ratchet: you can remove name-matching (by adding a typed field, as
Snag #13 did) but you cannot add it. If your change trips this lint, you have
introduced exactly the smell this chapter is about — go add the typed field
instead.

---

## Sibling-site drift: fix the class, not the instance

A family of this tree's worst time-sinks was *one* bug re-discovered at N sibling
sites that should have shared a single implementation. The fix kept landing at
site N, shipping, and site N+1 surfaced days later wearing a different hat.

Two canonical sagas:

- **`Result→T` auto-propagation.** Making a `throws` call type as `Result[T,E]`
  at the boundary exposed every consume site that hadn't been taught to unwrap
  it: call-arg (Snag #43) → constructor / enum-variant arg (#46) → match
  scrutinee (#48) → conditions / index / for-iter (#49). Each entry literally
  predicted the next ("a future audit should check push/put/insert/return") —
  and each prediction came true. The cycle broke only when the unwrap was
  **centralized at the producer** (the `lower_expr` exit, gated to
  Call/MethodCall with a one-shot suppress flag) instead of patched per consumer.
- **Tail-value-as-zero-init.** The "last statement of a block is the block's
  value" logic drifted across four dispatchers — statement-match (#8/#41), the
  `set_owned(result_local)` invariant (#31), expr-match (#50), and multi-statement
  closure bodies (#51). The original dispatcher had the discipline; each copy
  re-grew the same hole. The durable fix factored a shared
  `lower_stmt_as_tail_value` helper "so a future fourth dispatcher can't silently
  regress this family."

The rule (`AGENTS.md` → "Sibling-site drift"): when you fix a bug at one position
in an *enumerated set* — consume positions
(`push`/`put`/`set`/`insert`/`send`/ctor/return/capture), tail-value
dispatchers, container-literal arms, registration paths — **grep for the
siblings before you commit, prefer centralizing at the producer, and add an
arm-count lint** (`tests/lints.rs`, like `container_literal_arms_count`) so the
next sibling is forced through the shared path. The most elegant "centralize at
the producer" in this tree is one line: making `builder.set_terminator` a no-op
when the block is already terminated killed an entire class of "divergent
terminator silently overwritten" bugs (Snags #33/#39) by construction. When your
fix is "add the missing call to site N", ask "how many sites are there, and what
stops site N+1 from having the same hole?"

---

## Scout before you brief; review in sequential fresh passes

A delegated task is expensive: brief → reviews → execution → output-review →
integration. A wrong premise anywhere upstream burns the whole chain. Two cheap
habits front-load the risk.

**Scout first.** Before you write the brief — and before you commit to any
non-trivial plan — run a *scout*: a read-only probe (often a delegated
`Explore`/`general-purpose` agent) that verifies every load-bearing premise
against current source with `file:line`, confirms the bug still reproduces, and —
where the plan claims a yield — *prototypes it end-to-end and measures the real
result*. The scout's job is to **kill the plan cheaply if it's unsound**, and it
has earned its keep doing exactly that: refuting parity chains by running them
(the Box `__gg_new` chain scouted "DEEP / 0-yield"; a closure-call `void*`-cast
chain that SIGSEGV'd), and killing an RSS probe whose override fired zero times. A
killed plan after a one-agent scout is a *win*.

The non-negotiable scout rule: **yield estimates must be end-to-end-verified —
compile AND run AND diff whole output, never source-read.** Three separate scout
estimates in this tree were ~0 real because they were source-read. "It looks like
this fixes ~14 fixtures" is worthless until you have built the change and watched
14 fixtures flip. A scout that estimates a number without running the code has not
scouted.

The discipline catches over-claims in *both* directions — a yield estimated too
high, and an approach that *measures negative*. A higher-order-function port was
floated as "~9 → ~1 remaining"; the honest staged landing flipped one Vector-HOF
fixture and explicitly down-scoped the optimistic count (`vector_higher_order`,
self-host commit `4158530b`, see DONE.md).
And a "cheap composite-span-key" re-key proposed for the lazy-`Iterator[T]` chain
fix (TODO item #39) was *measured at **−4*** — it regressed four fixtures — so the
shipped fix used a per-link `span.end` oracle instead (DONE.md "TASK #39"). A
prototype that comes back negative is one of the most valuable scout outcomes
there is: it kills a plausible-looking plan before it costs a brief-and-execute
cycle.

Two scouts from the same session put numbers on exactly that collapse. The HOF
"~9" above came from a source read of the remaining `.map`/`.filter` corpus; the
scout that actually *built* each candidate and diffed whole output measured it
down to **~1 real** — the rest already matched, or failed for an unrelated reason
(scout `aadc8516`; it was a read-only audit and does not resolve in this
worktree). And an "O1 closure-ABI" lead projected, again from reading the
closure-call paths, that an ABI tweak would flip a batch of `~18` closure
fixtures; the end-to-end scout built it and measured **0 of 18** — the change
moved no output at all (scout `adc3d8c7`, likewise unresolved in-tree). In both,
the number that survived running the code was a fraction of the source-read
promise, and neither gap would have closed by reading harder — only by running.

A scout's premise can be wrong even when its conclusion is "go" — so verify the
*shape* of the claim, not just the number. A Track-A collection-HOF brief was
told to "start from the parked commit `4158530b`"; a brief-reviewer flagged it as
a retired branch, having **conflated it with `eb730d49`** — a *different* commit,
in the Rust C-backend (`src/backend/c_lir/emit_types.rs`), two months earlier,
that *deleted* inline-C HOF helpers in favor of `HofExpand`. `git` refuted the
conflation in seconds: `4158530b` is a *self-host* commit doing inline expansion
via `comp_make_acc` + `lower_for_vector` (the same direction as Rust's current
`HofExpand`), parked (not retired) — not a deletion of inline-C helpers like
`eb730d49`. Two commits that "sound like
the same change" can be on opposite sides of the compiler and months apart —
cross-check the hash, the files it touches, and the date before you let a
review-pass conclusion ("that's already retired") kill or redirect a plan.

**Then review in sequential fresh passes.** A fresh agent reviews the artifact;
you fold its findings; a *new* fresh agent reviews the corrected version; repeat
until a pass raises no reservations. The passes are **sequential, not parallel** —
fanning out N reviewers at v1 gives you N opinions on v1 and misses defects
introduced *by* a correction. And you **never stop on a pass that raised
reservations**: a fold can leave a stale remnant or introduce a fresh defect, and
only the next pass catches it. This tree has a brief whose fix to one section left
a *contradicting* second section (caught only on the next pass) and a plan that
needed **four** passes because passes 2 and 3 each surfaced a fresh remnant of the
same class. (The process that produced these very playbook additions is an
instance: a producer-side leak-validator design was scouted, then took four
sequential review passes — each catching a citation defect the prior fold left —
before a pass came clean.)

A later round put a sharper edge on this. The `index_of`/`find` → `Option[int]`
sentinel-wrap brief took **five** sequential fresh passes, each catching a distinct
defect a green gate would never surface: pass-2 caught a GIR→LIR **wrong-layer**
error (the draft emitted `IEnumInit` in GIR lowering, but it is LIR-only);
pass-3 caught a **backwards name-form** that would have NO-OPed the whole fix (the
predicate keyed on the unmapped method name, not the mapped `gorget_str_index_of`
that actually reaches the LIR dispatch), *plus* a missing collection-receiver
predicate arm that the receiver-blind type-flip itself regresses, *plus* a
`ptr-deref-an-int` segfault trap (the scalar sentinel must use the raw int
directly as the payload — no `ISlotLoad`/`ILoad`); pass-4 caught a missing
mandatory `record_enum_category` call (without it the type side never classifies
the result as `Option`, so ~14 internal `index_of().unwrap()` sites silently
miscompile and the bootstrap diverges); pass-5 signed off clean with an
end-to-end trace. Five defects across four passes — every one something a single
pass, or a parallel fan-out at v1, would have shipped; only the bootstrap
fixed-point would have caught the `record_enum_category` omission, and only after
a full execute cycle.

A three-track round (round-16: an await-splice port, a `Shared[T]`-wrapper
synthesis, a `??`-on-non-Option reject) sharpened three more edges, each a refinement
of an existing rule:
- **A clean pass can be wrong; the next fresh re-derivation catches it.** The
  wrapper brief's pass-2 returned a SIGN-OFF — on the wrong registrar (it confirmed
  the fix targeted a *field-only* helper, `:712`, when the fixtures' types flow
  through the *primary* `lower_type_defs` walk, `:962`). Pass-3, re-deriving from
  source rather than anchoring on pass-2's green, found it. This is why ≥3 passes is
  a floor, not "until the first clean one."
- **Verify a cited site is *live* (reached), not merely *present*.** The reject
  brief + all three of its reviews confirmed the self-host `infer.gg EDefaultOp` arm
  *exists* — but the parser lowers `a ?? b` to `EBinaryOp "??"`, so that arm is dead
  code, and typed-init checks bypass the inference path entirely. Only the executor,
  by *running* it, found the live site. A reviewer (and orchestrator) checking a
  cite must confirm the code path actually executes for the case at hand, not just
  that the symbol is there.
- **A completeness/reachability probe must exercise *nested* positions, not the
  happy-path instance.** The reject's first executor put the self-host check in a
  closure-finding walk that `else: pass`ed most parent shapes, so `??` nested in
  `EUnaryOp`/`EIndex`/`EArrayLiteral`/`EAs` escaped — yet its negative test (bare
  `int x = a ?? 5`) passed green. The output-review caught the one-sided reject only
  by probing nested positions; the fix's review probed *seven more* the executor
  never listed (tuple/dict/range/struct-arg/return/match-scrutinee/f-string). When a
  guard is meant to fire "everywhere," the test and the review must both prove
  "everywhere," not "here." (The robust fix was a single exhaustive walker over every
  AST variant, guarded by a lint that derives the variant set from `ast.gg` — Core #6.)
And a fourth, reaffirming "scout yield must be end-to-end": the wrapper scout's
"+4 floor" was a *link-resolution* estimate (the undefined symbols would now link),
not a compile-run-diff — the real flip was **+0**, because all eight fixtures were
gated behind pre-existing frontend bugs the wrappers merely made *reachable*. A
source-reasoned yield is not a measurement.

The reviewers are neither rubber-stamps nor nitpickers-for-sport: brief each to
verify load-bearing claims against source with `file:line` and return **SIGN OFF
or specific cited reservations**, and cross-check their claims yourself — a
reviewer can be wrong too. The lifecycle: **scout → brief → ≥3 fresh
brief-reviews → launch (worktree) → fresh output-review → integrate.** The
output-review includes the **breadcrumb-check**: no `LANDED`/`FIXED`/`DONE`/`✅`
status entries land in `TODO.md` — completed work belongs in `DONE.md`, and
`TODO.md` holds pending work only.

### Worked example — round-18: porting commit N of an N+k reference re-introduces the bug commit N+1 fixed

When a brief ports a *sequence* of reference commits, naming the wrong subset is a
silent-miscompile trap. The round-18 cross-frame-fault brief cited Rust commits
`a37143a7`+`04a8cf86` as "the CORE single-hop mechanism." Those two are
**Overflow-only** — their callee writes a fault into the slot and the caller takes a
single `slot != 0` branch. Brief-review **pass-1** read the *later* commit `d49e3cea`
(2.1c) and found that exact single-branch design is a **measured Core-#8 silent
miscompile**: with two fault categories live in one slot, `slot != 0` can't tell
Overflow from DivByZero, so it constructs the *wrong* `Fault` variant — a deep div0
printed 100 instead of 200, and "both backends agree on 100" is the wrong answer, not
a pass. The fix that *later* commit shipped — a per-category **tag-dispatch** — had to
be folded into the port's CORE, because even the "simple" `_divzero` callee `a/b` also
overflows on `INT_MIN/-1`, so two tags are live from the very first fixture. **The
lesson: when porting a multi-commit reference, enumerate the FULL commit chain for the
feature and read what each *later* commit fixed — a partial port silently re-introduces
the bug a subsequent commit already solved.** (The same round's pass-3 caught a second
class: the brief asserted `lower_function` was the *single* signature-build site, but
`lower_equip_block` inlines a hand-synced second copy — a partial edit would have left
method callees with a mismatched ABI. Both defects were invisible to a green suite; only
a fresh reviewer re-deriving from source caught them. And neither pass was the "obvious"
one — pass-2 was a clean SIGN OFF *between* them, which is exactly why you never stop on
a clean pass: the next fresh re-derivation is what found the segfault.)

### Worked example — round-20: a correct compile-fix unmasks the next-layer bug; fix it recursively, don't ship the unsafe MATCH

A correct fix that makes a previously-CC-FAIL fixture *compile* will expose whatever
bug sat one layer below the compile error. Round-20 typed the `String()` constructor +
registered a borrowed-string deref (both correct, reference-grade) — and the 3 `xml_*`
fixtures went from CC-FAIL to **correct stdout**. But ASan flagged a stack-buffer-overflow
in `gorget_array_push` of an `__gg_XmlNode` (256-byte stack slot vs a 288-byte array
elem_size), while the Rust oracle was ASan-clean — a self-host *divergence*, a pre-existing
struct-size bug (GorgetMap hardcoded at 184 across 9 sites; the real runtime is 152, and
Rust uses 152) that the CC-FAIL had simply kept unreachable. Two rules fall out:

1. **A stdout-MATCH that's memory-unsafe is NOT a clean parity win** (Core #8). The
   runtime-diff metric is stdout-only and ASan-blind, so it would happily count the xml
   fixtures as +3 while they stack-overflow. The right move is neither to ship that
   inflated unsafe MATCH nor to file-and-move-on — it's to **fix the unmasked bug
   recursively, in-round** (the output-review *proved* the root by changing 184→152 and
   re-running ASan-clean; the recursive Inc-C then landed it + collapsed the 9 duplicated
   constants to one source + a pinning lint). The xml +3 became genuinely clean.
2. **At a round's close, a `CC-FAIL → WRONG-OUTPUT` bucket-shift in runtime-diff is a
   REVEAL, not a regression** — but *verify* it: confirm each shifted fixture was CC-FAIL
   for the exact reason your change fixed (here `json_edge_cases`/`json_pretty` were
   CC-FAIL on `gorget_str_`, `bool_not_runtime_cmp_abi` on the `gorget_string_push_char`
   arg type — all three now compile and expose a separate, pre-existing wrong-output).
   Cross-check the MATCH count never *dropped* (no fixture regressed out of MATCH), then
   file the revealed bugs as next-round candidates. Don't hand-wave the shift; don't panic
   over it either.

### Worked example — round-29: port the INVARIANT, not the mechanism (Rust's `wrapping_shl` traps a checked-arithmetic self-host)

The self-host parser gives each f-string interpolation a synthetic span-offset window from
a per-`Parser` counter. Rust guarantees globally-disjoint windows — even for *nested*
f-strings — by seeding each interpolation's sub-parser at `(1<<40).wrapping_add(base << 20)`
(`src/parser/mod.rs:73-80`): the `base << 20` escalation puts every nesting level
astronomically above the parent's windows, so nesting can't collide *by construction*, and
the sub-parser is discarded with no write-back. The self-host had hardcoded the sub-parser's
counter to `1<<40` (dropping the escalation) → nested interps restarted at `1<<40` and
collided. The tempting fix is a faithful port of Rust's escalation. **It traps.** The
self-host uses *checked* arithmetic, and `1<<40 + base·(1<<20)` overflows i64 at nesting
depth (`base ≈ 1<<60` → `<<20` → `1<<80`); Rust only survives because `wrapping_shl`
*silently wraps*. Porting the mechanism ports a latent overflow that the reference's UB-via-
wraparound papers over.

The fix ports the **invariant** (windows are globally disjoint), not the mechanism: a single
monotonic counter threaded into the sub-parser and **written back** after it parses
(`self.next_interp_offset = sub.next_interp_offset`). Each interpolation — top-level or
nested — consumes exactly one `1<<20` stride, monotonically, so windows are disjoint and the
counter increments linearly (no escalation, no overflow). Two rules fall out:

1. **When the reference relies on wraparound/UB (`wrapping_*`, signed overflow, pointer
   provenance tricks), a checked/safe self-host CANNOT port it literally — port the property
   it achieves.** The disjointness invariant is the contract; `base << 20` is just Rust's way
   of getting it. A different, overflow-safe mechanism that yields the same invariant is
   *more* reference-grade, not less (document the divergence in-code and cite the invariant).
2. **A parity-neutral, stdout-unobservable correctness fix still earns its keep** — but be
   honest about the regression net. Under the round-28 scoped f-string guard this fix changes
   no fixture's stdout (the collided spans were never read), so no stdout fixture can guard
   its *behavior*; the guard fixture (`deep_nest_fstring`) instead pins the *failure mode* the
   naive port would hit (overflow/parse-trap on depth-3 nesting + a post-nested parent
   segment). Pair it with `self_host_bootstrap_fixed_point` (the self-host parses f-strings in
   its own source) and file the observable payoff (blanket interpolation inference) as the
   deferred follow-up. This is the sibling of round-18: there a *partial* commit-chain port
   re-introduced a fixed bug; here a *faithful* port re-introduces UB the reference hides.

### Worked example — round-39: the self-host driver is a build artifact — rebuild it before measuring emitted C

The round-39 T2 track wired real spawn/await into the self-host's `ESpawnBlocking` lowering
(previously a bare passthrough that ran the callee inline and dropped the `Task`/await). Its
output-review built the *emitted C* to check the claim and reported the opposite: Group A
(`spawn_blocking_basic`/`_multi`) was *still* sync-inline, and `spawn_blocking_basic` was leaking
21 bytes. Both were artifacts of a **stale driver.** `tests/fixtures/self_host_lowerer/driver{,.c}`
is a *build product* — an incremental `gg` build reuses the committed `driver.c`, so the review had
measured the *pre-fix* bare-passthrough behavior (a direct `blocking_read`, no spawn runtime), not
the code the fix emits. The executor cross-checked the reviewer's numbers against its own fixture
runs, could not reproduce them, and — per "a reviewer can be wrong too" — **refused the amendment
rather than book a phantom regression.** An independent tiebreaker settled it by forcing a clean
rebuild (`rm tests/fixtures/self_host_lowerer/driver{,.c}`, then re-running): Group A emits **real**
pthread-backed spawn/await, and the ASan matrix is **4/5 clean** (only `spawn_blocking_multi`'s
anonymous await-result temp leaks 14 B — an orthogonal, pre-existing drop-registration gap). The
lesson is mechanical: **before you measure the self-host's emitted C or run its ASan matrix,
`rm tests/fixtures/self_host_lowerer/driver{,.c}` so the driver is rebuilt from current sources** —
an incremental build serves a stale artifact and faithfully reproduces the behavior you just fixed.
This is the emitted-C sibling of "re-verify a premise against current source": the premise here was
*a number a reviewer measured*, and the cheap re-verify was a forced rebuild.

## Worktree discipline: agent worktrees nest under main

Agent worktrees live UNDER the main checkout (`/workspace/gorget/.claude/worktrees/agent-*`).
That nesting is a trap: an unqualified `/workspace/gorget/...` absolute path, or a
`python`/`sed`/heredoc fallback after an Edit-tool disk-desync, writes into MAIN
rather than the worktree. One f-string executor's heredoc fallback (after a
Read/Edit desync) wrote 20 files into `/workspace/gorget` — a pure duplicate of
work it had already committed to its own branch, caught only because the owner
noticed pending changes on `main`. The forensic patch was captured,
`git -C /workspace/gorget reset --hard` cleaned it (the branch never moved —
`reset --hard` with no commit arg only discards the working-tree contamination),
and every subsequent brief was hardened: worktree-relative paths only, no
absolute-repo-path heredoc fallback, and a post-write `git -C /workspace/gorget
status` contamination check. The one-line rule lives in `AGENTS.md` §
"Multi-agent orchestration" (rule 7); the lesson is that **worktree isolation is
necessary but not sufficient when the worktrees are children of the thing they
must not touch.**

### The stash race (rule 8) and the killed-agent recovery drill (rule 9)

Round 32 added two more entries to the same family. First, the **stash race**:
two concurrent scouts each ran `git stash push` around a baseline rebuild —
but the stash stack is repo-GLOBAL across every worktree, so scout B's `pop`
grabbed scout C's 14-file thread prototype, and scout C's own `pop` found
nothing (its entry had been consumed; the work survived only as a dangling
commit found via reflog). Both scouts noticed, captured the foreign diff to
`/tmp`, re-stored what they'd taken, and re-verified their own work from the
dangling commit — full recovery, but only because both agents were paranoid.
Rule 8: agents never stash; `git diff > /tmp/<name>.patch` + `git apply` has
identical save/restore semantics with per-agent namespacing for free.

Second, the **killed-agent drill**: a session limit killed five in-flight
agents mid-round. The one executor that had already committed lost nothing;
every other agent's work survived only as uncommitted worktree state that the
orchestrator captured with `git diff` before relaunching. The E2 scout lost 26
minutes of un-checkpointed prototype; its relaunch was briefed to update
`/tmp/recover_*.patch` after every meaningful step and later survived a second
kill with zero loss. Separately, three agents stalled indefinitely because
their last act was a *backgrounded* long run whose completion handoff got
lost. Rule 9 is both halves: checkpoint scout prototypes to `/tmp` early and
often, and run final validation gates as FOREGROUND commands with explicit
generous timeouts.

Third, the **nested-fork collision** (round 43). An output-reviewer — itself a
worktree-isolated subagent — spawned two of its *own* verification forks
WITHOUT `isolation: "worktree"`. The forks contended over the reviewer's
assigned worktree and reverted it mid-test: a freshly-built self-host `driver`
vanished between build and use, and the "fixed" tree reproduced the exact
*baseline* CC-FAILs — i.e. the fork was compiling fixed source against a
stale/baseline binary. The reviewer caught it (the tell: a fixed tree showing
baseline errors), traced it via reflog, aborted the forks, and redid all
empirical work in a dedicated isolated worktree. The orchestrator verified tree
integrity was intact (main, every track commit, and the stash were all clean —
the collision was confined to that one reviewer's environment) and then ran a
SOLO redo with an explicit anti-collision protocol: a dedicated
`CARGO_TARGET_DIR`, no sub-forks, and a **stability guard** that re-checks
`git rev-parse HEAD` and the built artifact's presence around every build,
aborting on any unexpected revert. Two lessons: (1) rule 1 (`isolation:
"worktree"`) applies to NESTED subagent-spawned forks too, not just top-level
`Agent` calls — brief any agent that may spawn its own helpers that THOSE must
be worktree-isolated (a shared worktree/target-dir under concurrent builds
silently reverts the tree); (2) when a self-host flip won't reproduce, re-check
HEAD and `rm` the `driver{,.c}` build artifacts before trusting the result — a
"fixed tree shows baseline behavior" reading is the signature of a reverted
tree or a stale driver, not a failed fix.

---

## The playbook in one paragraph

When a fix feels complex, you are at the wrong layer: trace the buggy read to
its write site and fix the dropped typed fact there (Snags #13 and #17). When
you hit a gap you can't fix now, expose it with a fixture + `TODO.md` entry —
never reshape code to dodge it. Keep the self-host idiomatic and retire
workarounds the moment their bug is fixed. Re-verify every dated premise against
current source before acting — *especially* parity scores, which the
diagnostic-only comparison tests will happily report green while wildly
mismatched. The gates (`*_comparison`, `bootstrap_fixed_point`, the name-prefix
ratchet) catch regressions, but only the comparison *counts* — read with
`--nocapture` — measure progress toward the actual finish line, which is parity
with Rust, not a green suite. And when you *delegate*: fix the whole sibling
class, not the instance; scout the premises end-to-end before briefing; and
review in sequential fresh passes — a wrong premise upstream burns the entire
brief → execute → validate chain.

---

*Playbook chapter. Verified against `7d3350a0`. The rules originate in
`AGENTS.md`/`CLAUDE.md`; the `file:line`s cite the source the worked examples
fixed (or the gates that enforce them) and are re-derived per the freshness rule
([Chapter 0](00-how-to-read.md)).*
