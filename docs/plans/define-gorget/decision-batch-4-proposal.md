# Decision Batch 4 — Proposal (2026-07-06)

> **STATUS: ✅ BATCH CLOSED 2026-07-06 — ALL of D10–D21 RATIFIED** (each section carries
> its owner-ruling block; ledger LOG has the entries; implementation tracks filed in
> TODO). Residual: D22 (colon-slice package) proposed in-conversation, one word out.
> Original header follows for the record: PROPOSED — awaiting owner ruling. Prepared from three probe-verified decision
> scouts (error model / aliasing & borrows / resources & ownership; probe corpora preserved at
> `/tmp/recover_scout_errmodel/`, `/tmp/recover_scout_aliasing/`, `/tmp/recover_scout_resources/`)
> plus the orchestrator's synthesis. Every current-behavior claim below was reproduced this
> session on both backends and cross-checked against ggdef; doc claims carry file:line in the
> scout reports. On ratification: each accepted item gets a D-number entry in `decisions.md`,
> and docs write-through (language-design / reference / book) is part of landing, per the
> standing directive.
>
> **Owner values applied throughout** (stated 2026-07-06): uniformity and simple mental models
> first; breaking currently-accepted code is acceptable when properly justified; never take an
> option that forecloses Gorget's future; simple, safe, fast, scalable, gorgeous.

---

## Part 0 — Questions that DISSOLVED under investigation (bugs or already settled — no ruling needed)

These were on the queue as "decisions"; probing showed they are defects with one defensible
answer, or already closed. All are filed in TODO.md. Listed so the queue can be pruned.

| Item | Finding | Disposition |
|---|---|---|
| **unwrap/expect on None/Error** | NO tag check emitted, both backends — silent garbage `0` or SIGSEGV (`insts.rs:4127`); reference §15.2 says "panics". ggdef traps correctly. | Filed HIGH 🔥. Pure bug; also the prerequisite for D-TRAP below. |
| **Nested-Result §10.3 garbage** | The well-formed nested shape WORKS and both impls agree; only the *nesting-level mismatch* miscompiles. | Not a semantics choice: §10.3 capture must type-unify against the destination like any bind → plain `E_TypeMismatch` + fix-it. Filed HIGH. |
| **Expr-body capture asymmetry** | Reference §5.1:619 already states expr-body ≡ block-body-with-`return`. The implementation violates the spec's own equivalence. | Enforce the spec: expr-body tails route through the same return-capture path. Widens acceptance; breaks nothing. |
| **A15 bare `return` in throws fn** | Already rejected (`E_TypeMismatch: expected int, found ()`) — and not throws-specific. | CLOSED. Residual: diagnostic polish (targeted message). Remove A15 from the queue. |
| **A16 "fast knob"** | Retired (`fb2e5037`): `+` always checks, `+%` wraps. The type never lies. | CLOSED. Add a pinning conformance fixture. |
| **A16 `Never` spelling** | `Never` = bottom type; `Fault` = the fault enum. Different kinds, no conflict (owner closed 2026-06-21). | CLOSED. |
| **A9 meta in `on error`** | `meta` is SILENTLY DROPPED there (missing visit in the meta pre-pass); fires everywhere else. | Fix as a bug under one rule: **`meta` runs wherever statements live** — no per-block carve-outs. (Rejecting would be an un-memorable exception.) |
| **`String(3)`** | Check-passes, prints empty string; reference :3224 says it IS a compile error. | Filed. Reject with the f-string fix-it (rides D-CANON below). |
| **A14 compound-assign resource ICE** | `v[i] += x` on a drop-tainted element ICEs the compiler (`mod.rs:1759`). | Filed HIGH 💥. Fix shape rides D-D4ENF below (move-the-dead-element). ICE removal is unconditional. |
| **LLVM `alloc=` divergence** | LLVM doesn't route `alloc=` into a bare Arena at all (no leak/UAF where C leaks/UAFs). | Filed. Parity bug regardless of D-ALLOC's ruling. |

---

## Part I — The decisions (recommendations, ranked by structural weight)

### D10 (RATIFIED 2026-07-06 — deletion rider SIGNED) — THE EXCLUSIVITY RULE: one place-overlap law at three sites  *(consolidates A29 a+b+c and A3)*

> **Owner ruling 2026-07-06: "Agreed, remove 'bind a borrow to a local variable'. This
> should not be legal and it also conflicts with only one exclusive writer."** The full
> package is ratified: (1) the place-overlap rule (readers XOR one writer/mover over
> overlapping places during the borrow's live range) as the normative exclusivity law;
> (2) duration = liveness-based normative, scope-based permitted as a stricter
> implementation (filed conformance gap, converging); (3) **local `&`-binds REJECTED in
> v1 — both `auto r = &b` and `T &a = b` forms — deleting the round-38 T-D write-through
> intercept** (owner reviewed the three live examples: the working write-through, the
> ASLR-garbage read-back twin, and the silent-copy LHS form); (4) same-call aliasing
> rejection normative, keyed on PLACE overlap across all sigil pairs. Frame-scoped
> borrows (`f(&b)`, `&`-params, `&self.field`, method auto-borrow, `for x in &b`) are
> untouched. A29 + A3 CLOSE. Implementation tracks filed.

**The reframe that makes this simple.** Exclusivity in Gorget is not Rust's
memory-safety-via-lifetimes. Under D1, ggdef cannot even *detect* an aliasing violation —
aliased writes just sequence to a defined value. Exclusivity is **production's license to use
lazy CoW**: a program that passes it is guaranteed lazy-CoW-refinable to the eager answer; a
program that violates it is *exactly* where lazy and eager diverge (measured: `auto r = &b;
b[1]=99; print(r[1])` → production prints an ASLR heap pointer as an int; ggdef prints 99).
Every violation is therefore a rejection, because accepting one un-defines the language.

**The rule (one sentence):** *for any two access paths in a call, a bind, or a live capture
whose **places overlap** (same root + compatible projection prefix), at most one may be a
writer (`&`) or a move (`!`) during the borrow's live range; violation is a compile error.*

Three site-specific consequences:

1. **Duration = liveness-based (NLL-style) as NORMATIVE; scope-based PERMITTED as a stricter
   implementation** (a filed conformance gap, converging over time). D7 already ratified
   liveness for captures; this extends it uniformly. The foreclosure argument lands on this
   side: scope→liveness is pure widening (rejected→accepted, breaks nobody), so the self-host
   can ship the trivial scope check first and relax later — Rust's lexical→NLL migration,
   done deliberately instead of painfully.
2. **Local `&`-binds: REJECTED in v1** — both `auto r = &b` (today: a half-landed live borrow
   whose read-back after source mutation is garbage) and `Vector[int] &a = b` (today: a
   *silent copy* — the same sigil meaning "borrow" on one side and "copy" on the other).
   Design-doc §3.5:583 already says the language has "no user-visible borrowed-view that can
   escape a function"; the book never teaches the idiom; the self-host uses it **zero** times;
   migration cost is 2 fixtures. **Breakage note: this deletes landed round-38 work
   (`cow_amp_bind_ref*`) — justified by the UB twin and the doc contradiction.** Forecloses
   nothing: full borrowed locals (option ii) remain a clean future *widening* governed by the
   same rule, if a real need ever appears.
3. **Same-call aliasing: normative rejection, keyed on PLACE overlap, across ALL `{bare,&,!}²`
   pairs where ≥1 side writes/moves.** Production's current check is 3-of-4 arms and
   name-exact: `f(v, !v)` is accepted-and-miscompiled (the smith lane's first three catches),
   and `f(b.data, &b)` slips through entirely. One class fix closes both (Core #4).
   "Defined evaluation order" is rejected: it would bless the divergence channel D1's
   refinement proof depends on being closed.

**Values scorecard:** maximal uniformity (one rule, three sites, one mental model: *"one
writer XOR readers, over overlapping places, while the borrow lives"*); safe (kills a live
garbage-read class); fast (it is what makes lazy CoW legal); forecloses nothing (every future
direction is a widening). Confidence: high.

---

### D11 (RATIFIED IN FULL 2026-07-06) — TRAP NORMALIZATION: the `T_` registry, one exit code, a `trap:` conformance field

> **Owner ruling 2026-07-06 (completing D11): the registry shape is APPROVED — "One
> registry is exactly the single source of truth we like and have written a rule about
> it on CLAUDE.md" — owner clarified the rule meant: the NO NAME-MATCHING / NO SIDECARS
> discipline (Layering rule 2 + its no-parallel-lists corollary). Apt: the current trap
> surface (three ad-hoc stderr formats) is a standing rule-2 violation — any tool
> classifying traps today must string-match message text; the typed registry with
> derived codes is the prescribed fix-shape from that rule applied to the runtime
> boundary.** The ratified shape: ONE closed `TrapKind` registry naming every
> trap class (initial: Overflow, DivByZero, Bounds, UnwrapNone, UnwrapError,
> UnwrapErrorOnOk, AssertFailed, Panic) with catch-all-free `code()` (T_ codes DERIVE
> from variant identity — the E_ convention, so `Fault.Bounds` renders `trap[T_Bounds]:
> … at file:line:col`, exit 101); **§10.9's `Fault` is RE-FOUNDED as the catchable
> SUBSET** (exactly the three, fault-`catch` semantics untouched — the scrutinee type
> only having catchable variants keeps uncatchable arms unwritable by construction) with
> a parity lint pinning the correspondence; **no-drops-on-uncaught-trap is normative
> v1** (matches ggdef + production; observable, so conformance needs it pinned);
> catchability of future codes = per-code annotation deferred to the deep-fault phase;
> stack-overflow SIGSEGV (C11, signal death) and OOM stay outside v1. Registry prose:
> spec/prose/trap-codes.md mirroring diagnostic-codes.md. Implementation track filed
> HIGH — it unblocks exact-code conformance, the P1-C dormant exit≠0 branch, and the
> panics-family (D2) migration.

> **Owner ruling 2026-07-06 (brainstorm session): the uncaught-trap exit code is `101`.**
> Reasoning recorded so it is never re-derived: (a) the MSB-set range (≥128) is REJECTED —
> `128+N` is the shell/CI/container convention for "killed by signal N" (129=SIGHUP,
> 130=SIGINT, 134=SIGABRT, 139=SIGSEGV), so an MSB trap code would be *ambiguous with signal
> deaths*, the opposite of the differentiation goal; (b) **WASI restricts exit codes to
> 0–125** (≥126 reserved) — anything ≥126 is unrepresentable on a plausible future Wasm
> target, and the assurance bar is Wasm-grade; (c) within the safe band (3–125 minus
> sysexits 64–78), 101 has no intrinsic meaning (Rust picked it semi-arbitrarily pre-1.0 and
> stability froze it) but enormous ACQUIRED meaning: Rust's panic is the exact semantic
> precedent for a Gorget trap, and the recognition (humans, CI tooling, LLM training data)
> comes free; (d) the principled alternative `70` (`EX_SOFTWARE`, sysexits) was considered
> and declined — semantically apt but the standard is moribund and recognition negligible.
> Scope notes ratified with it: only `0` and `101` are the LANGUAGE contract (ggdef's
> 102/103 stay tool-level verdicts production never emits); signal deaths (the by-design
> stack-guard SIGSEGV→139) remain OS-reported, outside the exit-code contract. The rest of
> D11 (the `T_` registry + stderr line + `trap:` frontmatter field) remains PROPOSED.

**Current reality (surveyed):** three incompatible stderr formats (`gorget: panic: …` with no
location / `file:line:col: …` / `<unknown>:0:0: …` for asserts) and three exit codes (1, 134
via stray `abort()`s, 139 via SIGSEGV) — and one trap class (unwrap-None) that production never
raises at all (the filed 🔥 bug). ggdef is uniform (101) but marked provisional pending exactly
this spec text (eval.rs:40; RFC §4 explicitly anticipates "trap output normalized … the
normalization rule is spec text").

**Recommendation — mirror the landed `E_` diagnostic registry, which is already the house
pattern:**

1. **stderr = one machine-readable line:** `trap[T_IndexOutOfBounds]: index 5, length 3 at
   file.gg:4:15` — codes from a **closed enum with a catch-all-free `code()` match** (rustc
   exhaustiveness = the ratchet, identical to `SemanticErrorKind::code()`). The `T_` registry
   subsumes §10.9's fault enum (`Overflow`/`DivByZero`/`Bounds`) plus the panic classes
   (`UnwrapNone`, `UnwrapError`, `AssertFailed`, user panic). Requires threading location into
   the assert + runtime-helper paths (today `<unknown>:0:0`).
2. **Exit code: ONE value for every uncaught trap — recommend `101`** (ggdef's current value,
   Rust's panic convention; folds the stray `abort()`→134 sites). The trap *class* rides the
   stderr line, not the exit code — POSIX truncates codes to 8 bits and Wasm doesn't model
   class-in-exit either. The by-design OS-guard stack-overflow SIGSEGV (ledger C11) stays
   impl-defined. *Owner pick available: plain `1` (POSIX-conventional) is defensible; the
   uniformity matters far more than the value. I lean 101 because it aligns production with
   the definition rather than the reverse, and distinguishes a trap from a program that
   deliberately `exit(1)`s.*
3. **Conformance:** add a `trap:` field to run-tier frontmatter (`expect: { exit: 101, trap:
   T_IndexOutOfBounds }`), with the trailing `at file:line:col` normalized out of comparison —
   host-independent fixtures, and the P1-C lanes' dormant exit≠0 branch (filed hazard) gets
   its contract.

**Sequencing:** fix the unwrap tag-check first (its trap must exist before its code can be
normative). Implementation is bounded: the reachable trap emit sites are few and centralized.
Values: uniformity (one format, one code, same pattern as `E_`), LLM-debuggability (parseable
class), Wasm-grade alignment, scalable (registry + ratchet). Confidence: high on the shape;
the 101-vs-1 pick is yours.

---

### D12 (RATIFIED 2026-07-06 — STRAIGHT TO ERROR) — D4 ENFORCEMENT LANDS IN PRODUCTION  *(the missing half of ratified D4)*

> **Owner ruling 2026-07-06: straight to error** (no warning period; the track's scout
> still MEASURES the corpus/self-host/gorget-arena blast radius first — if it shocks,
> that's a report back, not a silent downgrade). The compound-assign ruling rides along
> with no residual question: the element in `v[i] += x` is dead at the read, so
> move-out/apply/move-back is plain D4 move-at-last-use conformance — no implicit copy,
> no sigil owed; clone would be the D4 violation. The ICE dies with it.

The resources scout's highest-leverage finding: **ggdef already enforces D4 at the six
implicit-copy positions (`elaborate/mod.rs:566-596`); production does not.** Production
silently CoW-borrows tainted bare-assigns, silently *clones* tainted values in `get_or`
(an observable extra drop — precisely what D4 forbids), and ICEs on tainted compound-assign.
Three symptoms, one gap. The mandate:

1. Production implements `E_MoveWithoutOperator` for drop-tainted types at the six positions
   (bind / ctor-init / collection-put / return / capture / materialize-on-write), matching
   ggdef exactly. Negative fixtures per position.
2. **Compound-assign desugar ruling (A14):** `v[i] += x` reads an element that is *immediately
   overwritten* — the element is dead at the read, so the desugar **MOVES it out, applies
   `add`, moves the result back**. No clone, no double-drop, D4-clean, and `+=` "just works"
   on resource elements. (Interim fallback if move-liveness is hard at that site: clone,
   matching the already-working explicit form. The ICE closes either way.)
3. **Docs write-through debt:** D4 lives only in `decisions.md`. Reference :2266, design :460,
   and book 11-ownership.md:59 still enumerate a *closed* single-owner set (Box/Task/…) that
   omits custom-Drop types. Write it through.

Values: uniformity (one rule replaces three bespoke behaviors), safety, and it makes the
definition and production agree on the language's most distinctive rule. Confidence: high —
D4 is already ratified; this is execution.

---

### D13 (RATIFIED 2026-07-06 — TWO-STEP) — A10 allocators: REJECT bare allocator locals in v1; full RAII is the target state

> **Owner ruling 2026-07-06: two-step, as recommended** ("safety holes don't wait politely").
> Step 1 (now): bare allocator locals = compile error pointing at `with` / `alloc=`-inside-
> `with`; book §19 + reference `alloc=`/Fallback examples rewritten to the RAII form; phantom
> `checkpoint`/`restore` API removed from the book; negative fixtures. Step 2 (filed target):
> full RAII drop-registration with value→allocator ordering (the `borrow_deps` topo-sort is
> the primitive), `.destroy()` becomes reject-or-no-op under registration; bare locals then
> return as a widening. The LLVM `alloc=` divergence is an independent parity bug (filed).

Both filed behaviors confirmed: bare `Arena` leaks (4176B), and `.destroy()` while a backed
value lives is a **silent heap-UAF from safe code** (prints fine, exits 0; ASan sees it).
"Document the leak" is dead — it cannot fix the UAF.

**Recommendation — two steps:**
1. **Now (v1): bare allocator locals are a compile error** pointing at `with Arena(...) as a:`
   (or `alloc=` inside a `with`). Closes the UAF *and* the leak immediately, matches
   language-design §9.1 (which only ever shows `with`), and is the conservative pole —
   relaxable later without breaking anyone. **Breakage: the book's §19 bare-local example and
   the reference's own `alloc=`/Fallback examples must be rewritten** (they currently teach
   the leaking idiom, and book §19 additionally documents phantom `checkpoint`/`restore`
   methods that don't exist — filed).
2. **Target (filed track): full RAII** — drop-register allocator handles with a value→allocator
   ordering edge (the `borrow_deps` topo-sort in `drops.rs` already provides the ordering
   primitive), and manual `.destroy()` becomes reject-or-no-op under drop registration. Bare
   allocator locals then return as a *widening*, correctly this time.

Why not RAII-first (my earlier lean): the scout's feasibility read says the machinery exists,
but it is real compiler work across both backends + self-host, while the UAF is silently
shippable *today*. Step 1 is a one-week fence; step 2 is the destination. Confidence:
medium-high.

---

### D14 (RATIFIED 2026-07-06 — VIEW, re-confirmed after the write-through discussion) — A5 `get_or`/`get_or_put`: **"get_or is not special"** — reads return views, uniformly

> **Owner ruling 2026-07-06 (ratified twice — held once to interrogate the write-through
> story, re-confirmed after it): it's a view.** Ratified WITH the write-through story:
>
> **There is deliberately NO storable write-through variable in Gorget** (§3.5 + D10):
> names bind values; PLACES accept writes; borrows live only at call boundaries. The
> invariant this buys: *you can never mutate `d` through a name that isn't rooted at `d`*
> — every dict mutation is syntactically visible at the write site. The three write
> channels, all place-rooted: (1) place assignment `d[k] = v` / `d[k] += 1`;
> (2) mutating method on a place — `d[k].push(x)`, and the ensure-then-mutate one-liner
> `d.get_or_put(k, Vector()).push(x)` (receiver auto-borrow; ≡ Python's
> `setdefault(k, []).append(x)`); (3) `&`-arg of a place — `f(&d[k])`, frame-scoped.
>
> Family mapping: `d[k]` read = trap-on-missing · `.get(k)` = safe read (Option view) ·
> `.get_or(k, def)` / `.get_or_else(k, fn)` = read with (lazy) fallback, VIEW, READ-ONLY
> (mutating through it = compile error + "did you mean get_or_put?" — on miss its view
> aliases the CALLER'S default local, so write-through would be context-dependent) ·
> `.get_or_put(k, def)` = ensure-exists, THE write-through anchor.
>
> Multi-statement mutation idiom = read-modify-writeback: `Config c = d.get_or(k,
> Config())` (CoW alias, free until written) → mutate (severs: ONE clone) → `d[k] = c`
> (c dead → MOVE). Exactly one clone, elidable later via exclusivity-powered in-place
> proof — same optimization stance as D15 slices and the counter double-lookup
> (`d[k] = d.get_or(k, 0) + 1`).
>
> Consequences: retire the round-8 unconditional clone (wasteful plain / D4-violating
> observable double-drop tainted); temp-default rule (live place or consumed-within-
> expression, else reject — the TODO:143 view-of-temp class); D4 governs a tainted
> default at the put boundary. Book one-liner: **"`get_or` reads with a fallback;
> `get_or_put` makes the entry exist so you can write to it."**

The executing double-free is already fixed (round-8) — but the fix was an *unconditional
clone*, which is wasteful for plain types and a D4 violation for tainted ones (measured: the
hit path fires a side-effectful drop **twice**). The uniform answer needs no bespoke rule:

- `get_or_put(k, default)` always stores the default on miss → its result is **always a view
  of the map-resident value**. The store is an ordinary `put` — an ownership boundary where D4
  already governs the default (`!default` / `.clone()` for tainted).
- `get_or(k, default)` returns a **view**: of the map value (hit) or of the live default
  (miss) — exactly `.get()`'s semantics (reference :2371 "no implicit clone on read") with a
  fallback. Retire the round-8 unconditional clone.
- **One sharp edge to pin in the ledger:** a *temp* default (`d.get_or(k, Res("x"))`) whose
  result is stored past the statement is the pre-existing view-of-temp class (TODO:143).
  Rule: the default must be a live place, or the result must be consumed within the
  full expression; otherwise reject (fix-it: bind the default first). Same conservative pole.

Values: uniformity (reads are views everywhere — one table row, not three), fast (no clone),
safe (no owned copy exists → double-free unrepresentable). Owner alternative: if you want
`get_or` to yield an owned, freely-storable value, the D4-gated owned form is coherent —
but it keeps a clone in the hot path and a special case in the mental model. Confidence:
medium-high.

---

### D15 (RATIFIED 2026-07-06 — WITH FULL `int[]` REMOVAL) — A6 slices: **slices are owned values**; the fat pointer is an invisible optimization, never a surface type

> **Owner ruling 2026-07-06: remove `int[]`/`T[]` from the surface ENTIRELY** ("I believe
> it was left there for C-interop. but we can add later if required… let's simplify and
> uniformize for now") — superseding the filed reject-escape fix with the stronger
> uniform move: ONE sequence type (`Vector[T]`), slices are values, the vestigial
> fat-pointer type leaves the grammar/typechecker (pending the removal track's
> live-use scan; if C-interop ever needs a raw-view type it returns as a WIDENING,
> preferably as a dedicated FFI type in an interop phase, not as the general slice).
> Slicing syntax sugar (`v[a:b]`) remains an unopened future ergonomics question,
> orthogonal to these semantics. A6 CLOSES.

`.slice()` already returns owned copies — the language is already at the reference-grade
shape. The decisive discovery: under value semantics + CoW, an owned-copy slice and a lazy
refcounted **sub-range share** (Swift `ArraySlice`-style, offset+len over a shared backing,
sever-on-write) are *observationally identical* — so O(1) slices can land later as a pure
implementation optimization with zero semantic change. That is D1's eager-spec/lazy-impl split
applied to slices.

Ratify: (1) slices = owned value-semantics sub-sequences (spec text); (2) land the
already-filed reject of the vestigial `int[]` escape (today it silently miscompiles);
(3) **no user-visible fat-pointer slice type, ever-for-now** — it is the one option that
forecloses (it breaks §3.5's zero-lifetime-annotations promise permanently). Speed is not
foreclosed: it's deferred into the invisible-optimization slot, to be *measured into* if a
hot path demands it. Confidence: high.

---

### D16 (RATIFIED 2026-07-06) — A11 UFCS: remove GENERAL UFCS from the design targets; keep the curated trait-mediated dual spellings

> **Owner ruling 2026-07-06: abandon the stated design promise.** Docs write-through DONE
> same commit: `language-design.md:85`'s Design-Target cell rewritten to the curated
> reality (trait-exposed duals: `len`/`Measurable`, `map`/`filter`) with the abandonment
> rationale inline. The narrower future possibility (immutable-receiver-only free-fns-as-
> methods) remains un-foreclosed but is NOT a target. A11 closes.

The design doc is internally contradictory: :85 promises `arr.filter(f)` ≡ `filter(arr, f)`
universally, while :86 lists "multiple incompatible ways to do the same task" under AVOID.
Probing shows general UFCS is unimplemented in both directions; only curated trait forms ship
(`len` via Measurable, `map`/`filter` dual). And general UFCS has a fatal collision: a free
`f(String &s)` called as `x.f()` would mutate `x` with **no `&` at the call site**, gutting
the mutation-acknowledgment invariant that §3.5/§9 rest on (method receivers are the *one*
sanctioned exception, justified because signatures are the API contract — UFCS would extend
that exception to every free function).

Ratify: general UFCS is **removed** from the design targets; language-design :85 is rewritten
to describe the shipping reality (curated, trait-exposed dual spellings for a small set).
Anti-foreclosure note: a *narrower* future feature (free-fns-as-methods restricted to
immutable receivers) stays possible; it just isn't a target. Values: this is the uniformity
value applied to the design doc itself — UFCS's pitch is uniformity, its effect is two
spellings for every call. Confidence: high.

---

### D17 (RATIFIED 2026-07-06) — A12 `read_file` becomes FALLIBLE (`throws IoError`); `parse_int` book typo fixed

> **Owner ruling 2026-07-06: throws.** Owner context: "Initially it panic'ed, then we
> evolved to throw, but not all docs caught up. I am trying to avoid panics (keep the
> server running) and recover where possible." Correction of record: the probe shows the
> evolution ran the OTHER way — the DOCS evolved to throws (book/10, language-design
> §6.4), the IMPL never caught up (lib/std/fs.gg returns bare String; runtime_file.c
> exit(1)s). So the track is an implementation change + doc sweep. **The owner's
> stated philosophy is recorded as the STDLIB FALLIBILITY PRINCIPLE: environmental
> failures throw — panics are avoided wherever recovery is possible (keep the server
> running).** The track therefore sweeps the CLASS: every stdlib fn that panics on an
> environmental failure (not just read_file) converts or gets an explicit `_or_panic`
> opt-in variant. Forward note: this principle leans the future deep-fault catchability
> question toward recoverable-at-boundaries — record it as input when that phase opens.

The impl (`read_file` returns bare `String`, panics `exit(1)` on failure) violates the
design's own error model (§6.4: environmental failures — file I/O — MUST surface as Result;
error-model.md calls handling "mandatory"). Half the docs describe the intended contract
(throws), half describe the defect (panics). Core #8 says resolve toward the principled
behavior, not the majority text: **make `read_file` fallible**, keep an explicit
`read_file_or_panic` convenience for scripts, update book §19 + appendix. Python's `open`
throws — the fallible default *is* the Python-easy default. `parse_int`: impl and error
model already agree (`Result[int, ParseError]`); fix the one wrong book line (:166).
Breakage: every current `read_file` caller adds `throws`/handling — justified, it's the
error model's flagship contract. Confidence: high.

---

### D18 (RATIFIED 2026-07-06 — AS THE GENERAL RULE) — const-eval/meta integer overflow = COMPILE ERROR

> **Owner ruling 2026-07-06: ratify the general rule so it is never revisited
> per-operation.** THE RULE: **const-eval mirrors runtime semantics exactly, except
> runtime faults become compile errors** (there is no boundary to catch a compile-time
> fault; reject is the degenerate form of fault). Settled by it, now and forever:
> const `+` overflow → compile error · const `+%`/`-%`/`*%` → wraps (the explicit
> spelling means wrap, at compile time too) · const division-by-zero → compile error ·
> const float overflow → `inf` (IEEE — runtime doesn't fault, so const doesn't either) ·
> and every future const-eval question answers itself by the same mirror. One sentence,
> zero per-op relitigating.

`const int BIG = INT_MAX + 1` today: check OK, silently **wraps** — while the identical
expression at runtime **faults**. Compile-time inputs are determinate and there is no runtime
boundary to catch a compile-time fault, so the natural degenerate form of "fault" at
compile-time is *reject*. One rule: **overflow is always an error; at compile time that means
rejection** (`meta.rs:1278` drops its `wrapping_*` calls). Uniform with "the type never lies"
(`+%` remains the explicit wrap spelling, also in const contexts). Confidence: high.

---

### D19 (RATIFIED 2026-07-06) — A13: REMOVE `break <value>` / loop-as-expression from the v1 surface

> **Owner ruling 2026-07-06: "remove for now. no loop-as-expression in gorget as for
> now."** Removal track filed (grammar `break_stmt` loses its optional expr; the
> `Stmt::Break(Some(e))` arms leave the typecheck return-walks — typecheck.rs:7338/
> :7526/:7760 at filing; self-host already has no SBreak arm; reference §6.7:1188-1193
> rewritten; negative fixture: `break <expr>` = parse/check error with a message noting
> the removal). Re-adding later, properly (loop-typed collect targets, self-host arm,
> fixtures), is a pure widening. A13 closes as a decision; the residual is the removal
> track.

It's in the grammar, barely wired (loop-as-expression doesn't even parse in assignment
position), untested (zero fixtures), and its type inference is unsound-by-sharing (break
values walk the *function* return type). The most uniform outcome for a half-feature with no
demonstrated demand: **remove it** (grammar + the `Stmt::Break(Some(e))` walk arms + self-host
non-arm), file the removal fixtures. Re-adding it later, properly, is a pure widening.
Alternative if you *want* the feature: commit to it as a real track (parse loop-as-expr,
loop-typed collect targets, self-host arm, fixtures) — but that's a feature investment, not a
cleanup. Confidence: medium (owner taste call on the feature itself).

---

### D20 (RATIFIED 2026-07-06 — as recommended) — Canonical text-conversion: f-strings are THE way; reject the impostors with fix-its

> **Owner ruling 2026-07-06: "I agree with whatever you recommend."** Ratified as
> recommended: `f"{x}"` is the canonical surface; `.display()` remains as the trait
> method it desugars to (ONE concept, two positions — like `+` and `Add`; legitimate
> when a String value is needed programmatically — NOT a second way); `x.to_string()`
> on primitives stays rejected and gains a fix-it naming both forms; `String(x)` joins
> the rejection (today a silent empty-string miscompile contradicting reference :3224);
> UUID/DateTime keep type-specific `to_string` as ordinary API. Docs write-through
> includes the `language-design.md:80` falsehood (f-strings desugar via
> `Displayable`/`display`, not `.to_string()`). A7 CLOSES.

> Write-through note (found during D16's edit): `language-design.md:80` claims f-string
> interpolation "calls `.to_string()`" — stale/wrong (interpolation goes through
> `Displayable`/`display`, and `.to_string()` on primitives is rejected). Fix with D20's
> write-through.

The corpus already voted: ~3885 f-string uses vs 30 `.to_string()` (self-host: zero) vs 40
`.display()`. Ratify: `f"{x}"` (and `.display()` where a String is needed programmatically)
is canonical; `x.to_string()` on primitives stays rejected but gains a **fix-it**; `String(x)`
joins it (today a silent empty-string miscompile, already contradicting reference :3224).
Type-specific `to_string` on UUID/DateTime stays (that's API, not conversion). One obvious
way. Confidence: high.

---

### D21 (RATIFIED 2026-07-06 — GO) — A17: RETIRE `gg sim`

> **Owner ruling 2026-07-06** (delegated the excellence judgment: "happy to remove it if
> you think we can arrive at supreme excellence without it" — judgment: YES, remove).
> The rationale of record: Miri exists to police Rust's `unsafe` trapdoor, to run its
> aliasing-model research, and to stand in for the executable spec Rust lacks. Gorget
> has no unsafe surface (UB inexpressible by construction, four-outcomes contract), a
> one-sentence statically-enforced aliasing rule (D10), and an ACTUAL executable spec
> (ggdef) — Miri approximates a definition; ggdef IS one. The Miri role maps onto the
> existing triad: ggdef+lanes+smith (semantic oracle) · ASan/LSan/valgrind on emitted C
> (memory-UB) · 4-implementation differential (compiler-correctness). sim's GIR-consuming
> architecture was circular as an oracle regardless. **Deletion track: salvage scan
> FIRST (one agent, ~1hr: confirm nothing in src/sim's UB-detection/isolation/backtrace
> phases is worth porting), then delete src/sim + the `sim` command + sim-only tests +
> doc mentions; git history keeps everything. PHASE-3 NOTE (pinned): the one future gap
> is data-race detection for `shared`/Task — the right shape there is a ggdef
> INTERLEAVING extension + TSan on implementations, never a GIR interpreter.** A17
> CLOSES.

ggdef now occupies exactly the role sim was reaching for (an independent semantic oracle),
with the architecture sim never got (definitional, frontend-shared, IR-independent) — while
sim consumes GIR (disqualified as a definition by standing rule 3), duplicated backend effort
(the reason you deprioritized it), and was never architecturally designed. Recommendation:
remove `src/sim/` and the `sim` command from the build (own deletion track with review; code
survives in git history), port nothing. The one idea worth preserving — UB-detection as a
divergence-triage voice — is a future *ggdef extension* (trap provenance), not a reason to
keep a parallel backend. Confidence: high, given your stated context; the deletion itself
awaits your explicit go.

---

## Part II — Stays queued (no ruling sought now)

- **A18–A28** "ratify the rejection" batch — gated by your C12 phasing rule, unchanged.
- **Result-vs-Fault reconciliation + catchable-fault set** — rides the deep-fault phase; the
  D11 `T_` registry gives it names to attach to when it arrives.
- **OOM recoverability** — deferred (allocator rework).
- **P1-D/D2 migration questions** (production-v1 expect source, panics-family stderr field) —
  own scout, already sequenced; D11's `trap:` field is its missing prerequisite.

## Part III — Suggested ratification order

1. **D10 + D12** (exclusivity + D4 enforcement) — the structural pair; everything else gets
   simpler once the one-rule story is normative and production enforces it.
2. **D11** (traps) — unblocks conformance exactness + D2 panics migration; needs the 101-vs-1 pick.
3. **D13, D14, D15** (allocators, get_or, slices) — the safety/uniformity sweep.
4. **D16–D21** — each independent; any subset can land per round.
