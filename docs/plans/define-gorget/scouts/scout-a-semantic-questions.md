# Scout A — Open Semantic Questions in Gorget Ownership/CoW/Drop Semantics
# (working checkpoint — evidence: file:line citations, worktree-relative)
# Sources swept: docs/language-design.md, docs/language-reference.md (§9.6),
#   docs/devbook/10,11,13,15,28, docs/book/11,12,16, CLAUDE.md

================================================================================
FINAL NUMBERED LIST (most-fundamental-first)
================================================================================

--- TIER 1: THE LOAD-BEARING FORKS (everything else hangs off these) ---

Q1. PLAIN-`self` WRITE-THROUGH vs "self = immutable borrow"
 Q: When a bare-`self` method mutates the receiver (`void poke(self): self.log.push(1)`),
    the language should ___ (a) write through to the caller, (b) CoW-materialize a private
    copy, or (c) reject at compile time.
 Evidence: language-reference.md:639 ("`self` | Immutable borrow"); language-design.md:940
    ("immutable borrow for self"); §3.1 land.-design:317-332 (a write through ANY immutable
    binding — "a bare local, a bare parameter, a bare alias" — must materialize); reference
    §9.6:2327 (bare binding is read-only, mutation materializes). CONTRADICTED BY reality:
    TODO.md:948 (HIGH bug) — plain-`self` mutation WRITES THROUGH in BOTH Rust+self-host
    (probe printed 2 not 1); `self` is `LoOwned` so the CoW gate never fires. devbook/11
    §&self-mutation-inference (1049-1100) is about NAMED receivers, doesn't cover bare-self.
 Observable? YES — caller sees the mutation (output differs 2 vs 1); no clone so
    --clones=stats differs; a custom Drop on the field's old value differs.
 Options: (a) write-through = current impl, contradicts docs. (b) materialize = docs' uniform
    rule (§3.1). (c) reject. Docs imply (b); impl does (a). Owner call required.

Q2. LAZY-vs-EAGER CoW OBSERVABILITY (clone count / clone timing)
 Q: When a borrowed value's source is mutated on a path that may not execute, the language
    should ___ define observable clone COUNT/TIMING or leave it as implementation freedom.
 Evidence: devbook/11:471-535 (full lazy = 0 clones on untaken path, 1 on taken; "production
    default in both compilers"); devbook/11:646-656 (self-host is STRICTLY LAZIER: alias
    deadpath executes 0 clones self-host vs 1 Rust W3a) — the two compilers DISAGREE on clone
    count. devbook/11:917-963 (`--clones=stats` exposes executed clone counts per site).
    book/11:329-357 ("if that moment never arrives ... no copy ever happens"). §3.3:435-450.
 Observable? YES, THREE ways: (1) --clones=stats runtime counters; (2) allocator introspection
    `pool.bytes_used()` (§9.1:1667) — a deferred/elided clone changes arena consumption;
    (3) a resource element WITH A CUSTOM DROP: each clone is a value that later drops, so
    clone-count == Drop-side-effect-count (print in drop → observable in stdout).
 Options: (a) clone count is impl-freedom (docs' current stance: "identical observable
    behavior" claim, devbook/11:486 — but that only holds for String w/ view-aware free & no
    user Drop). (b) DEFINE lazy as the spec (0 on untaken, exactly 1 on taken) and make BOTH
    compilers match. Self-host(lazy-direct) vs Rust(hooks) already differ on alias shapes →
    must be decided. String-only today; user-Drop types make it sharply observable.

Q3. RUST EMove VALUE-BUG (move-bind/move-reassign then read lazy-bound string)
 Q: For `Vector[String] w = !v` (or `w = !v`) followed by a mutation through `w` and a read
    of a lazy-bound `String s = v.get(0).unwrap()`, `s` should read the ___ (pre-mutation /
    post-mutation) value.
 Evidence: devbook/11:724-733 ("⚠ Rust gg is VALUE-WRONG on both EMove shapes" — lazy
    read-through prints POST-mutation where EAGER prints PRE-mutation; open HIGH TODO).
    Self-host excludes via `cow_moved_names` → EAGER = reference behavior. TODO.md open.
 Observable? YES — stdout differs (pre vs post mutation value). Memory-safe but wrong output.
 Options: (a) eager/pre-mutation (self-host reference, the intended answer). (b) lazy/post
    (current Rust bug). Docs+self-host imply (a); Rust impl is (b) until fixed.

Q4. UNTRACKED ALIAS CHAINS still write through (the one unconverged CoW shape)
 Q: A `&`-mutation whose root is a view-returning method (`&x.slice()[i]`, a chain
    `resolve_projection_root_local` cannot name) should ___ materialize-at-the-immutable-
    binding or write through.
 Evidence: devbook/11:431-450 (Implementation status — "one remaining unconverged shape ...
    untracked alias chains ... still writes through"); TODO.md:22 ("Only unconverged shape
    left = untracked-alias-chains ... still writes through; when THAT lands delete the
    converging marker"). Design target (§3 uniform rule) = materialize.
 Observable? YES — source seen mutated where the model says it shouldn't be (output + clone
    count + drop count).
 Options: (a) materialize (spec/design target). (b) write-through (current impl gap). Docs
    imply (a); impl does (b) for this shape only.

--- TIER 2: WRITE-THROUGH / MUTATION-DIRECTION GAPS (all observable, all live bugs) ---

Q5. `for x in &coll` ELEMENT WRITE-THROUGH is LOST (even on an owned Vector)
 Q: `for c in &a: c.n += 100` on an owned `Vector[Cell]` should ___ write the mutation back
    into the collection.
 Evidence: land.-design.md:1178-1180, 411-418; book/11:411-432 ("for x in &coll ... points
    IS modified"); reference §6.11:1287 ("Mutable borrow (modify in-place)"). CONTRADICTED:
    TODO.md:23-33 (live bug — prints 1, SHOULD print 101; the `&`-bound loop element local is
    a COPY, not a write-through pointer; aliased-root variant leaves both a and b unchanged).
 Observable? YES — collection value after the loop (1 vs 101).
 Options: (a) write-through (docs). (b) silently dropped (impl). Docs demand (a); impl is (b).

Q6. VALUE-TYPE index-element field-store `v[i].field = x` (self-host mirror + Dict/Set)
 Q: `v[i].x = 99` on a value-struct element (and `d[k].field = x`) should write through to
    the element in the collection.
 Evidence: land.-design §24:2447-2476 ("direct place mutation ... writes through in place");
    reference:1647,1663. Rust LANDED for Vector value-elems (R39-T1, devbook/11:453-469).
    STILL BROKEN: self-host prints stale 10 (TODO.md:35); Dict/Set `d[k].field=x` drops the
    write on BOTH compilers (TODO.md:36, gated to CollectionKind::Array only).
 Observable? YES — element value after store (stale vs written).
 Options: (a) write-through (docs). (b) dropped write (impl for self-host Vector + all Dict/Set).

Q7. DEAD-BRANCH ALIAS BIND → SIGSEGV (memory-safety crash, both backends)
 Q: An alias bind inside a never-taken branch (`if false: Vector[int] v5 = v0`) followed by
    `v0[2]=9` should ___ be a no-op (never executed) — not corrupt the later mutation.
 Evidence: TODO.md:946 (HIGH — both-backend SIGSEGV in gorget_array_clone; mutation-site CoW
    materialize walks an uninitialized alias slot from a dead branch; self-host reference-
    correct prints 9). Sibling of tests/fixtures/cow_lazy_d1_alias_deadpath.gg.
 Observable? YES — crash (SIGSEGV) vs correct output.
 Options: (a) correct/no-op (self-host, intended). (b) crash (Rust impl bug). Must fix class.

Q8. `String !p` MOVE-PARAM + concat → check-accepts, backend build-breaks
 Q: `String f(String !p): return p + "log"` called `f(!s)` should ___ compile and run
    (self-host prints "ablog") — not emit invalid C/LLVM.
 Evidence: TODO.md:947 (HIGH — gg check accepts, C emits `(void*)a+(void*)b`, LLVM `invalid
    operand type`; self-host reference-correct). Front/back-end disagree on validity (inv #8).
 Observable? YES — build failure vs correct output.
 Options: (a) accept+run (self-host). (b) reject at check. Either way BOTH must agree; Rust
    backends currently miscompile a check-accepted program.

--- TIER 3: PROVENANCE / TYPE-CLASSIFICATION AMBIGUITIES ---

Q9. STRING: Trivial(Copy/view) or Resource(owned)? — the split-classification problem
 Q: Is `String` a Copy/Trivial type or a Resource type — i.e. does `String b = a` copy,
    borrow-with-CoW, or share-heap, and how many drops run?
 Evidence: land.-design §3.2:350-351 lists BOTH "String (view provenance)"=Trivial AND
    "String (owned provenance)"=Resource in the SAME table; §2.3:235 "32-byte value with CoW".
    book/11:28-31 lists String ONLY as Resource (not in Copy list). devbook/10:41-47 —
    TWO TypeIds: `string_id`=Copy view (`Str`), `owned_string_id`=non-Copy owned. devbook/13
    LocalOwnership::SharedHeap is "the sole source of truth for `String b = a`" (value-alias,
    shared heap, drop the tag only). So `String b = a` = SharedHeap (both drop-safe, heap
    shared) — but is that a copy, a borrow, or an owned-alias?
 Observable? PARTIALLY — cap==0 view vs cap>0 owned is a runtime discriminator; drop of a
    view no-ops. Under normal use unobservable; under allocator introspection / double-free
    scenarios it matters. Provenance edge cases (view escaping, view-into-mutated-source) are
    where UAFs live (devbook/11:658-744 view-producer enumeration).
 Options: (a) one type, provenance-inferred, view=Copy/owned=Resource (design intent). The
    ambiguity is WHICH representation a given `String` binding has and whether that is a
    stable observable contract. Needs a single stated rule per binding shape.

Q10. VIEW-PRODUCER ENUMERATION completeness (view-UAF class, ASan-silent)
 Q: Every cap==0 view producer aliasing another buffer must be covered by a materialize hook
    — the language should ___ guarantee this is a closed, enforced set.
 Evidence: devbook/11:658-744 (executable lint `str_view_producer_enumeration_is_closed`,
    but FOUR prose residuals the guard can't see: dynamically-constructed callee names, moved/
    duplicated view calls, budget-slot reuse, backend-emit-layer rewrites). devbook/11:736-744
    ("ASan is NOT the safety net" — D1 wrong-output + W3b/c/d view-UAF are ASan-SILENT).
 Observable? YES when it breaks — wrong output or heap UAF (garbage/crash), but NOT via ASan.
 Options: enforced-closed (goal) vs prose-obligation residuals (current). Self-host index/slice
    (W3c) is DEFERRED behind the F2 string-index miscompile (devbook/11:632-635, 808).

Q11. FieldPath and EIndex sources stay EAGER in Rust (lazy asymmetry)
 Q: `String s = self.data.get(0)` / `String s = v[i]` should be lazy-CoW like local-collection
    sources, or eager.
 Evidence: devbook/11:794-813 (Open items — FieldPath+EIndex sources stay eager in Rust;
    `cow_before_field_mutation` has no lazy routing; `String s = v[i]` never sets borrow-
    sources sidecar; `cow_lazy_fieldpath_excluded` locks the exclusion). Self-host index/slice
    join blocked on F2.
 Observable? YES via --clones=stats / allocator (1 clone where lazy is 0) + custom-Drop count.
 Options: (a) uniformly lazy (design target). (b) eager for these sources (current). Impl=(b).

--- TIER 4: DROP TIMING / ORDERING / COUNT ---

Q12. DOES COPY TIMING CHANGE DROP TIMING/COUNT when a custom Drop exists?
 Q: For a resource element with a custom `Drop` (observable side effect), the number and
    ORDER of drops the program runs should ___ be a defined function of source semantics,
    independent of lazy-vs-eager clone placement.
 Evidence: book/11:146-180 (custom Drop runs on scope exit, reverse declaration order).
    devbook/11 (lazy defers/elides clones); each clone is an independently-owned value that
    ALSO drops. So a lazy-elided clone on an untaken path = ONE FEWER Drop side-effect than
    eager. NO doc states drop COUNT is invariant under clone-placement. Interacts with Q2/Q3.
 Observable? YES — a `drop` that prints makes clone count directly visible in stdout+order.
 Options: (a) drop count/order is a defined observable (then lazy MUST be the spec and both
    compilers must match — contradicts current self-host-vs-Rust clone-count divergence Q2).
    (b) drop count is impl-freedom (then a printing Drop has nondeterministic-across-compilers
    output — violates Core invariant #8). UNRESOLVED; no doc addresses it.

Q13. `v[i] = x` DROP-OF-OLD-ELEMENT TIMING
 Q: `v[i] = val` drops the existing element and moves `!val` in — WHEN does the old element's
    Drop run relative to evaluating `val` and relative to a fault mid-expression?
 Evidence: land.-design §24:2476 + reference:1663 ("Subscript write drops the old element and
    moves the new value in") — states THAT it drops, not the ORDER vs RHS evaluation.
 Observable? YES with a printing Drop (old-drop-before-or-after new-value-eval is visible).
 Options: (a) eval RHS then drop-old then store; (b) drop-old then eval; undefined today.

Q14. PANIC/FAULT MID-EXPRESSION — do destructors run?
 Q: When an uncaught fault (overflow, bounds, div0) panics mid-expression, already-constructed
    resource temporaries / in-scope locals should ___ be dropped (unwind) or leaked (abort).
 Evidence: land.-design §6.4:1319-1366 ("panics by default ... aborting the process";
    "continuing with corrupted state is worse than stopping"). NO doc says whether panic runs
    drops. reference:980 `_Noreturn` (exit/abort/_Exit). No longjmp/unwind machinery found in
    docs. §6.4:1362-1366 `catch Fault.X` is local/lexical — recovers WITHOUT unwinding out of
    the expression. devbook/11:843-846 clone-temp MoveZero is about normal path only.
 Observable? YES with a printing Drop + allocator leak-check (pool.bytes_used after a caught
    fault) — does a `(f()) catch Fault.Overflow: fallback` leak temps built before the fault?
 Options: (a) abort, no drops run (leak; simplest, matches "abort the process"). (b) unwind &
    run drops. (c) for CAUGHT faults specifically — must the partial temps drop? Undecided.

Q15. INDEX-WRITE / MUTATION drop of the CoW-materialized temp
 Q: When a consuming push/store clones then the source is still live, the freshly-cloned temp
    is MoveZero'd post-call — but the ORDER of its registration/drop vs other scope drops.
 Evidence: devbook/11:843-846 (`pre_call_clone_temps` MoveZero'd right after the call);
    devbook/15 (drop elaboration is conservative DropIfAlive → guard). Order is impl-derived.
 Observable? YES with printing Drop.
 Options: impl-defined today; no spec statement.

--- TIER 5: CLOSURES, COMPREHENSIONS, FOR-LOOP ELEMENTS ---

Q16. CLOSURE CAPTURE: "immutable borrow capture" but "stored by value" — clone or alias?
 Q: A default (immutable-borrow) closure capture of a resource (`auto g = (): use(s)`) should
    ___ clone `s` into the closure struct, alias it, or share-heap — and does the outer `s`
    stay live?
 Evidence: land.-design §7.3:1480-1484 ("immutable-borrow and move captures are both stored
    BY VALUE in the closure struct; the difference is whether the outer binding survives";
    mutable-borrow stores a pointer). book/16:48-51 (`.clone()` on Callable = independent deep
    copy; captured mutable state NOT shared). devbook/11 lists closure capture as an
    `ensure_owned_at_boundary` site = CLONE. So an "immutable borrow capture" of a String
    actually CLONES it at capture (a boundary), despite the "borrow" name. CONTRADICTS the CoW
    bare-binding "borrow, zero cost" model for this position.
 Observable? YES — clone count (--clones), allocator bytes, custom-Drop count (the capture is
    an owned copy that drops with the closure).
 Options: (a) clone at capture (boundary rule, impl). (b) borrow/alias (the "immutable borrow"
    naming, zero-cost). Docs use "borrow" language but impl+boundary-list say clone. The word
    "borrow" for captures is a naming/semantics mismatch that must be reconciled.

Q17. COMPREHENSION OWNERSHIP: default `[p.name for p in people]`
 Q: The default (non-`!`, non-`.clone()`) comprehension element should ___ borrow-then-clone-
    at-collect, or move.
 Evidence: land.-design §18.4:2136-2151 ("Comprehensions produce owned collections. The
    iterator yields owned or cloned values"; default = "immutable borrow (people still valid)").
    reference §9.6:2359 boundary #13 "Comprehension into an owned collection" (materialize).
    "owned OR cloned" is ambiguous about the default cost.
 Observable? YES — people still valid after (borrow) vs consumed; clone count.
 Options: (a) borrow each element, clone at the collect boundary (matches boundary #13).
    (b) move. Default is (a) per "people still valid"; but "yields owned or cloned" is vague.

Q18. FOR-LOOP ELEMENT: dict/set loops EAGER while array loops BORROW (asymmetry)
 Q: `for k, v in dict` / `for x in set` element binding should ___ be a borrow alias (like
    array loops) or an eager per-iteration owned clone.
 Evidence: devbook/11:341-356 (per-loop-kind table: array string/struct/enum = borrow; dict/
    set = EAGER — out-param accessors hand back a drop-registered clone). Explicitly "the
    natural next step" but NOT done. Array tuple-destructure/direct-collection also stays eager.
 Observable? YES — clone count per iteration + custom-Drop count on a hot read-only loop.
 Options: (a) borrow uniformly (design direction). (b) eager for dict/set/tuple-destructure
    (current). Impl=(b); the asymmetry is a stated gap.

Q19. CONSUMING FOR-LOOP `for x in !coll` — per-iteration drop semantics
 Q: `for item in !coll: store(!item)` — what happens to elements NOT consumed in an iteration
    (early break), and when does the emptied collection drop?
 Evidence: land.-design §5.7:1182-1184 + reference §6.11:1288 ("Move (consumes collection)").
    NO doc detail on partial-consume/break drop behavior or element-vs-collection drop order.
 Observable? YES with printing Drop + break.
 Options: undefined; needs a rule (drop remaining elements on break? drop collection when?).

--- TIER 6: SINGLE-OWNER CARVE-OUT & MISC ---

Q20. SINGLE-OWNER CARVE-OUT BOUNDARY: which types require `!` at which positions
 Q: The set {Box, Task, TaskGroup, Guard, Owned, Callable/closure} requires `!`/.clone() at
    bare-assign AND ctor/field-init — but at plain call args they're "simply borrowed". Is
    Mutex/RWLock/Shared/Weak/Channel in or out, and is the rule uniform across positions?
 Evidence: CLAUDE.md §Ownership-at-Consuming:92 + devbook/10:222-232 (carve-out = Function/
    Callable(×3)/BoxedCallable/Owned + named Box/Task/TaskGroup/Guard; check_stmt.rs:1217).
    devbook/11:62-87. book/11:59-73 lists Box/Task/TaskGroup/Guard/Callable/Owned. NOTE:
    Mutex/RWLock/Shared/Weak/Channel are single-owner/refcounted (devbook/15:101-217) but are
    NOT in the MoveWithoutOperator carve-out — they're Trivial/Copy-semantics pointers. book/16
    never teaches Owned/Guard/TaskGroup (TODO.md:951 gap). Carve-out fires at bare-assign+ctor
    but NOT plain call (borrowed). Is that split intended & uniform?
 Observable? YES — compile error (E_MoveWithoutOperator) vs accepted.
 Options: (a) exactly this 6-type set, at assign+ctor+field+capture, borrow at call (current).
    Boundary is precise in impl but under-documented; Mutex/etc. inclusion is a real question.

Q21. `T[]` NON-STRING SLICE ESCAPE — parsed, typechecked, miscompiles if it escapes
 Q: A non-String `int[]` slice should ___ be rejected (no runtime repr) or supported.
 Evidence: land.-design §24:2396-2404 ("Slices are String-only today ... not lowered ... works
    only as a function-local binding and MISCOMPILES IF IT ESCAPES (e.g. returned)"). TODO.md
    "ESCAPING SLICE miscompiles" High entry (cross-ref'd at 942).
 Observable? YES — miscompile (garbage/UAF) on escape.
 Options: (a) reject at typecheck (reference-grade). (b) silently miscompile (current). Docs
    admit (b) is a gap; the fix is (a) per Core #8.

Q22. MutationWhileBorrowed — doc says compile ERROR, reality COMPILES+RUNS (latent UAF)
 Q: `auto e = v.get(0).unwrap(); v.push(42)` should ___ be rejected (borrow invalidation) or
    allowed (CoW materializes). And the in-place `p.score+=10` after a realloc-push — UAF?
 Evidence: book/12:213-235 ("cannot mutate v while entry borrows it" — claims ERROR).
    REFUTED: TODO.md:664 (compiles+runs today; the for-loop sub-case still errors; the in-place
    mutable-borrow shape keeps writing through after push = latent UAF if realloc). devbook/10
    §Mutation-while-borrowed:246-268 — it's a WARNING (CowBorrowMutation), not an error, for
    the CoW case; only explicit-ref borrows + for-loop iterators are hard errors.
 Observable? YES — accepted vs rejected; latent UAF (garbage) if buffer reallocs.
 Options: (a) error (book claim). (b) warning + CoW-materialize (impl for value binds).
    (c) error only for &-element borrows. UNRESOLVED design question (TODO.md:664 says decide
    intent before rewriting either the check or the chapter).

Q23. OVERFLOW-FAULT `catch` INTERPLAY WITH DROPS (partial-materialize under recovery)
 Q: `int r = (build_temps() * 2) catch Fault.Overflow: -1` — resource temps built in the
    wrapped expr before the fault should ___ drop cleanly on the recovery path.
 Evidence: §6.4:1350-1368 (local/lexical, covers only faultable ops in the expr's own BBs).
    No statement about drop of partially-built temps on the catch path. Overlaps Q14.
 Observable? YES — leak (pool.bytes_used) / double-drop on recovery.
 Options: undefined; needs a rule.

Q24. `.clone()` ORIGIN = Static (fresh, no provenance) — return-borrow interaction
 Q: `.clone()` yields an origin with NO provenance to the receiver (treated Static). Does a
    cloned value returned/stored always sever lifetime tracking correctly?
 Evidence: devbook/10:139-144 (".clone() produces Static because the clone is a fresh
    independent allocation"). Fine in principle; edge case when clone is of a view.
 Observable? Marginally (dangling-return acceptance).
 Options: mostly settled; note as a provenance edge to verify.

================================================================================
LOAD-BEARING FORKS (the N everything hangs off): Q1, Q2, Q12, Q4, Q16
  - Q1 (plain-self write-through) decides whether §3.1's "uniform bare-binding rule" is real.
  - Q2 (lazy clone-count observability) + Q12 (drop-count under clone placement) together
    decide whether clone placement is impl-freedom or spec — and whether the two compilers'
    KNOWN divergence (self-host 0 vs Rust 1) is a bug or allowed.
  - Q4 (untracked alias chains) is the last unconverged write-through shape = the boundary of
    "the spec is enforced everywhere."
  - Q16 (closure capture "borrow" that actually clones) decides whether "borrow" is uniform.
================================================================================
