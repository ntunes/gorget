# Scout B — Semantic Questions Forced by Open Bugs (Gorget "Define Gorget" spec-mining)
# Sources swept IN FULL: TODO.md (956 lines), tests/fixtures/known_gaps/*.gg,
# tests/security.rs security_known_unsafe class, cow_lazy_*/cow_* sneak fixtures,
# DONE.md rounds 40/41/42, docs/plans/error-model.md §9 Q17 (line 446-450).

################################################################################
# LIST A — GENUINE OPEN DECISIONS (the prize): each needs a "what SHOULD it do"
#          language-design call before it is fixable. Spec content.
################################################################################

A1. Unconstrained Ok(5)/Error(x) combinator chains — unbound E (resp. T).
    Q: What should `Ok(5).map(..).unwrap_or(0)` with NO Result in scope MEAN?
    Source: TODO.md:153 + error-model.md §9 Q17 (lines 446-450).
    Status: UNDECIDED, explicitly deferred to the error-model revisit. Fork:
      (a) reject+require annotation [RECOMMENDED, fits monomorphized Result[T,E]];
      (b) default to a universal error EXISTENTIAL (anyerror-style — a new feature);
      (c) infer E from the fn body (Zig error sets — a wholesale Result redesign).
    Both compilers currently MISCOMPILE it (Core #8, latent — no corpus hit).
    Blast radius: the whole error model — Never (=empty error set), From-rethrow
      widening, throws-sugar, existentials, monomorphization of Result.

A2. Plain-`self` mutation writes through to the caller.
    Q: Is `void poke(self): self.log.push(1)` (mutating caller's struct) intended,
       or must it be rejected / CoW-materialized?
    Source: TODO.md:948 (HIGH, STRATEGIC ASSESSMENT) merging :269 (round-35 MED).
    Status: CONTRADICTS-DOCS — language-design §4.5 says self is an immutable
      borrow; §3.1 says a bare-binding write must materialize. Impl does neither.
      Both Rust + self-host write through IDENTICALLY. Scout's FIRST job = the
      design question (owner call): fix BOTH (reject or materialize) OR declare
      write-through intended and fix DOCS (§4.5, book ch.11, MEMORY).
    Blast radius: the entire CoW bare-binding rule; whether `self` is a carve-out
      to "bare binding = borrow, mutation materializes"; the DeadBareParamWrite
      lint (self currently excluded until this resolves).

A3. Local mutable-borrow binding `auto a = &b` / `Vector[int] &a = b`.
    Q: When a local binds `&b`, is it a live exclusive borrow (b inaccessible while
       borrowed) or a rejected unsupported form?
    Source: TODO.md:745 (Low; owner-requested verification 2026-07-02) + book 12
      §MutationWhileBorrowed (:664).
    Status: CONTRADICTS-DOCS — spec (language-design:574-575, reference:2276/1747)
      makes `&` an EXCLUSIVE mutable borrow with source inaccessible; impl accepts
      + MISCOMPILES (`auto a=&b` → garbage; `Vector[int] &a=b` → silent copy).
      Owner+spec lean REJECT, but "live-exclusive-borrow vs reject-as-unsupported"
      is itself the open call.
    Blast radius: borrow-exclusivity model, aliasing rules, whether `&`-local-binds
      exist at all; interacts with the in-place mutable-element-borrow carve-out.

A4. In-place mutable-element borrow across a reallocating push (book 12 refuted).
    Q: Is `auto p = players.get(0).unwrap(); p.score+=10; players.push(..); p.score+=100`
       an allowed CoW-value carve-out, or a still-rejected in-place &-element borrow
       (latent UAF if the push reallocs)?
    Source: TODO.md:664 (docs/book/12-borrowing.md §MutationWhileBorrowed REFUTED-BY-RUN).
    Status: CONTRADICTS-DOCS + UNDECIDED — chapter claims it's a compile error; it
      compiles, runs, and keeps writing through after the push. "Decide intent
      FIRST, then fix EITHER the check or the chapter."
    Blast radius: CoW value-bind vs &-element-borrow distinction; borrow-checker
      decidability (the Site #4 lifetime-aware analysis, :632).

A5. Resource-valued Dict.get_or / get_or_put ownership.
    Q: Should a resource-valued get_or/get_or_put that can't be cheaply cloned be
       REJECTED, or should the clone semantics be DEFINED?
    Source: TODO.md:150 (ROUND-8 lead; scouted a0728051).
    Status: UNDECIDED language-design call (Core #8) — BOTH compilers double-free
      for non-String resource values today ("both backends agree on the wrong
      answer"). Ground in devbook/11 Materialization + devbook/13 ownership.
    Blast radius: every borrowing-wrapper builtin that returns an owned default;
      the clone-vs-move decision at wrapper ABIs.

A6. Escaping slice int[] (and Ref[T]/MutRef[T] lazy-iterator returns).
    Q: Reject any non-String slice that escapes at check-time, or build a real
       slice fat-pointer representation?
    Source: TODO.md:465 (+ :202/:220 Ref[T] read-side).
    Status: UNDECIDED fork — currently silent-miscompiles (returns 0, void*-from-int
      warning). "clean check-time error" is the near-term answer; "real slice
      fat-pointer" is the bigger alternative.
    Blast radius: slice representation across the whole language; lazy-iterator
      return ABI; Ref[T]/MutRef[T] drop-ownership (borrow field that neither
      double-frees NOR leaks = itself an open design question, :202).

A7. n.to_string() on a primitive: implement or reject?
    Q: `int n=3; String s=n.to_string()` check-passes then miscompiles — implement
       to_string for primitives (→gorget_int_to_str) OR reject at check pointing at
       .display()?
    Source: TODO.md:670 (Medium).
    Status: UNDECIDED fork. `.display()`/`f"{n}"` already work.
    Blast radius: the primitive method surface; Displayable vs to_string duplication.

A8. .unwrap()/.expect()/.unwrap_or() on a NON-Option/Result.
    Q: typecheck error or lint? (Design call; "verify no stdlib/self-host relies on
       the no-op first".)
    Source: TODO.md:702 (+ Method-resolution totality :500).
    Status: PARTIALLY DECIDED — Rust Phase-1 already made it UnwrapOnNonOptional
      error; the residual is the general silent-no-op `matches!` arm + the self-host
      (no reject mechanism). Fork = error vs lint.
    Blast radius: method-resolution totality (Phase 2 gate widening), the whole
      "typecheck deliberately non-enforcing" posture.

A9. Meta-stmt inside an `on error` block silently dropped in BOTH compilers.
    Q: Make on-error-body meta EVALUATE in both, or REJECT meta-in-on-error in both?
    Source: TODO.md:174 (Core #8 shared latent gap).
    Status: UNDECIDED fork; both compilers silently drop it today (a shared
      silent-miscompile class). Latent (no corpus fixture).
    Blast radius: delayed-meta evaluation ordering vs on_error_blocks cloning;
      meta semantics inside all deferred-lowering blocks.

A10. Bare allocator locals: destroy or document-the-leak?
    Q: drop-register allocator handles (+ extend escape tracking + fix destroy
       ordering) OR document the leak as the bare-idiom contract?
    Source: TODO.md:654 (Medium; ASan-verified UAF from safe code).
    Status: UNDECIDED — ESCALATED: not just a leak; manual `.destroy()` before a
      live RAII value's scope exit = heap-UAF from SAFE code. Documenting alone does
      NOT fix the UAF; only drop-registration-with-ordering does. The docs' own
      §15.3 `alloc=` example uses the leaking bare idiom.
    Blast radius: allocator lifetime model; with-block vs bare-local semantics;
      escape analysis at arena_depth 0.

A11. UFCS design-target with zero implementation.
    Q: decide+implement `arr.filter(f) == filter(arr,f)`, or remove it from the
       design targets? (Unresolved collision with the auto-borrow method-receiver
       exception: `v.push(x)` needs no `&` but `push(&v,x)` would.)
    Source: TODO.md:952 (STRATEGIC ASSESSMENT, language-design call for owner).
    Status: UNDECIDED — documented as a target, never built, and self-contradicts
      the §4.5 auto-borrow rule.
    Blast radius: the method-receiver auto-borrow exception; every free-fn-vs-method
      call-site.

A12. Book contract contradictions (which contract is REAL).
    Q(a): parse_int returns Result[int, ParseError] (book 10) or Result[int, String]
          (book 19)? Q(b): read_file throwing (book 10) or panic-on-failure (book
          19 + appendix-traits)?
    Source: TODO.md:951 (MEDIUM, book accuracy).
    Status: CONTRADICTS-DOCS internally — must decide the actual stdlib contract,
      then fix all sites. These are error-model surface decisions.
    Blast radius: the contract-error vs fault classification of stdlib IO/parse;
      the error-model §error-type-on-the-API-surface rule.

A13. Break(Some(e)) collect-target mis-bind.
    Q: align by DROPPING Break from Rust's typecheck pairing OR ADDING an SBreak arm
       to the self-host?
    Source: TODO.md:401 (③, low-pri, theoretical).
    Status: UNDECIDED reference-grade-resolution fork (divergent shapes; no corpus
      fixture). Self-host is the more conservative (no SBreak arm).
    Blast radius: break-as-loop-value vs break-binds-fn-return semantics.

A14. Op-overload compound-assign on a resource element.
    Q: support `Vector[Money] += ..` with a user `+` overload, or keep it rejected?
    Source: TODO.md:272 (LOW, pre-existing).
    Status: UNDECIDED — currently a fail-safe validator panic ("shallow copy of
      resource"); "resource-element compound-assign with a user operator overload
      is unsupported."
    Blast radius: operator-overload + resource-move interaction; compound-assign
      lowering.

A15. `gg check` accepts bare `return` inside `int throws E`.
    Q: is a bare `return` (→ silently Ok(0)) a missing-return-value error?
    Source: TODO.md:411 (Low).
    Status: UNDECIDED ("arguably" an error; lowering is well-defined as Ok(0)).
    Blast radius: throws-sugar return typing; definite-return checking.

A16. Error-model still-open §9 questions (each a spec decision).
    Q: (a) the fast knob (debug-checked vs release-wrap overflow); (b) meta-overflow
       three-way split (wraps silently today, meta.rs:1278); (c) `Never` spelling;
       (d) Result reconciliation with the Fault/contract split; (e) full fault set;
       (f) self-host parity story.
    Source: TODO.md:225 + error-model.md §9.
    Status: UNDECIDED (Phase-1 shipped; these are the Phase-2/knob residuals).
    Blast radius: the entire error model (overflow semantics, catch scope, faults).

A17. `gg sim` disposition (tooling, but forces a semantics-of-record decision).
    Q: delete / freeze-experimental / re-architect as a BIR-consuming Backend /
       replace with an instrumented native UB-checking build?
    Source: TODO.md:956 (MEDIUM — OWNER DECISION).
    Status: UNDECIDED — sim consumes GIR (forks above LIR/BIR) so it re-derives
      drop/CoW/materialization and now gives WRONG CoW answers (invariant-#8-flavor
      on a shipped, documented command). Immediate sub-item: warn-or-pull regardless.
    Blast radius: whether there is a second executable semantics-of-record; the
      "one source of truth per axis" principle applied to the interpreter.

--- Decisions ALREADY MADE IN ONE COMPILER ONLY (still a divergence to ratify) ---

A18. `type X = <scalar>; equip X with Trait` — Rust REJECTS, self-host ACCEPTS.
     Source: TODO.md:270/:277. Status: decided-in-one-compiler-only (Rust). Fixture
     primitive_trait_impl_alias_error.gg (#[ignore]) encodes reject. Blast radius:
     trait-registration-vs-alias-resolution ordering.

A19. f-string typecheck BLANKET-suppresses carrier-op/operator-on-wrong-type errors
     (Rust). `f"{int ?? 5}"` accepted+miscompiled by Rust; self-host (exhaustive
     check_carrier_ops) already REJECTS. Source: TODO.md:117. Status:
     decided-in-one-compiler (self-host is MORE correct). Blast radius: every
     operator/carrier-op inside an f-string interpolation.

A20. `?.` optional-chain on a non-Option — both accept then crash; should reject.
     Source: TODO.md:118. Status: undecided-but-obvious (reject); self-host fix is a
     natural sibling in check_carrier_ops, Rust needs the guard.

A21. Positional-after-named on METHOD calls — accepted by BOTH; free-fn form
     rejected. Should reject method form in both. Source: TODO.md:939. Status:
     ≥2-bug shared defect; reject-both is the reference-grade answer.

A22. `void ?? int` (?? on a void/unit LHS) — Rust accepts (cc warning) then runs;
     ill-formed. Both should REJECT. Source: TODO.md:854. Status: bounded,
     reject-in-both (opportunistic).

A23. Unknown method on Option[T]/Result[T] passes check, dies at LINK
     (`Option__int64_t__to_str` undefined ref). Method-existence check on builtin
     generic enums missing at typecheck. Should reject BOTH. Source: TODO.md:949.
     Status: reject-in-both (triage vs the TYPE-mismatch family).

A24. ReadGuard compound-write `rg.x += N` on a read-only lock guard silently
     dropped — should be REJECTED at typecheck. Source: TODO.md:392. Status:
     reject (carries a "type checker should reject in future" comment).

A25. Builtin collection value args not typechecked against V:
     `Dict[String,float].put("x", 10)` silently coerces int→0.0. Should reject like
     `float f = 10` does. Source: TODO.md:261. Status: reject (a typecheck-layer
     decision; direction = reject-not-coerce).

A26. Phantom static methods (`String.from("x")`, `int.try_parse(s)`) pass check,
     fail at build/link → should emit NoSuchMethod/UnknownStaticMethod. Source:
     TODO.md:701. Status: reject.

A27. `Ordinal` built-in trait never registered → wrong-signature `equip X with
     Ordinal:` compiles clean; should validate the signature (like Comparable
     does). Source: TODO.md:698. Status: validate (register Ordinal). Same class:
     the ill-typed-program-acceptance defects at :327 (int x = "s" / print(undef)
     BUILD clean on gg-selfhost).

A28. Collection-literal element type unchecked: `Vector[int] v = ["a","b"]` and
     `return ["a","b"]` from Vector[int] pass check. Should reject (element-type
     unify). Source: TODO.md:692. Sibling: collection-literal in RETURN position
     is REJECTED while VarDecl is ACCEPTED (:485) — an inconsistency to resolve
     toward ACCEPT. Static-decl initializers not type-checked (:596). Status:
     the consistent decision (accept declared-type-compatible, reject mismatched)
     is clear; the current state is inconsistent between positions.

################################################################################
# LIST B — NO DECISION NEEDED, JUST CONFORMANCE TESTS: intended semantics is
#          CLEAR/AGREED; the implementation is wrong. (Spec = write the fixture.)
################################################################################

B1. Dead-branch ALIAS BIND SIGSEGV (near-null in gorget_array_clone). Bind in a
    never-taken branch corrupts later source mutation; both C+LLVM crash. Intended
    output = 9 (self-host already correct → Rust-side fix). Missing sibling of
    cow_lazy_d1_alias_deadpath. Source: TODO.md:946 + known_gaps/cow_dead_branch_alias_bind.gg.
    Fix the CLASS (all dead-path alias-slot shapes) + the &-arg twist.

B2. `String !p` move-param + concat inside callee = both-backend build-break on a
    check-accepted program (C emits `(void*)a+(void*)b`, LLVM `invalid operand
    type`). Intended output = ablog (self-host correct). Source: TODO.md:947 +
    known_gaps/move_param_concat.gg. (Front-end/back-end disagreement, #8-adjacent —
    but the RIGHT answer is unambiguous.)

B3. `for x in &coll` element write-through LOST even on an OWNED vector (prints 1,
    should print 101). CoW model already DECIDES it write-throughs; for-loop
    lowering makes the &-bound element a copy. Source: TODO.md:23. (Semantics
    settled by the CoW model + cow_amp_owned_writethrough fixture.)

B4. `equip <primitive scalar>:` inherent methods (float/uint/int32/…) mis-lower:
    type_id_to_base_name collapses non-GtBool scalars to "int64_t" → phantom
    int64_t__m → undefined ref. Intended output for float/uint = 43.000000/false/42
    (Rust-faithful). Source: TODO.md:68 + known_gaps/equip_on_primitive_scalar_variants.gg.

B5. GENERIC-equip `&self` mutator via a named bare-value-param receiver still writes
    through (self-host); Rust materializes. Intended = caller untouched (Y then A).
    Source: TODO.md:244 + known_gaps/generic_equip_mutator_named_recv.gg.

B6. Shared[Vector[T]]/Shared[<resource>] leaks its inner payload in BOTH compilers
    (gorget_shared_drop never runs the inner drop fn on last-ref). Intended = deep-
    drop inner at strong==0 in the shared runtime. Source: TODO.md:114.

B7. `!`-param double-drop at exit (self-host emits drop inside consume(!r) AND at
    caller scope exit). Intended = drop exactly once (Rust correct). Source:
    TODO.md:293/:350 (test_option_resource_field quad-free).

B8. Static Vector[struct] index-load `STATIC_VEC[i].field` returns a zeroed struct
    (`.get(i).unwrap().field` works). Intended = the real field. Source: TODO.md:594.

B9. RWLock/Mutex/lock-object leaks — SEMANTICS ARE SETTLED (doc-settled: a lock IS
    a Resource, single-owner, single-drop; language-design:319, devbook/18,
    book/14+16, reference:3589). Only impl drift (MUTEX=Trivial/drop_fn:None) +
    a missing leak gate. Source: TODO.md:385-389.

B10. Un-awaited/error-path Task leaks (both compilers) — join-on-drop RAII is the
     documented INTENT (book/13-async); implement drop_fn_for_type→Task-join in
     both. Source: TODO.md:157.

B11. NON-void ambiguous Task[T] collection = silent garbage (task_type_fns.len()==1
     gate). Documented (book/13:149-163,385-405); fix = 2nd carried __await ptr on
     the Task ABI. Intended output = 48. Source: TODO.md:370.

B12. shared(<sync>) facade lowers as a plain value (self-host drops shared_kind) →
     async_rwlock HANGS/SIGSEGV. Rust half correct post-R2. Intended = the sync
     handle/guard alloc. Source: TODO.md:126/:853.

B13. Match-arm binding of a String/resource payload from an owned scrutinee CLONES
     but never DROPS → leak. Fix = bind payload as BORROW when scrutinee is a live
     owned local dropped whole. Source: TODO.md:447/:516.

B14. `while: v.push(x)` on a non-materialized value-param LOSES the pushes (memory-
     safe wrong-result; CoW loop-materialize-scoping). Intended = accumulate on the
     private copy. Source: TODO.md:742.

B15. Various leaks with a CLEAR "register the owned temp at the producer" fix (no
     semantic question): rethrow-String-transform leak (:312), unwrap()/unwrap_error
     Ok-side payload leak (:314), catch-bound error-payload leak (:455), print/await
     result-temp leaks (:79/:682/:683), Json/Dict accessor per-call clone (:46),
     match-arm-binding leak (:418), test_linked_list 192B iter leak (:864),
     Box[T:resource] drop-glue leak (:254), for-loop inline-iterable-literal drop
     leak (:260). All = "fix at the producer/write site"; intended (drop-once) clear.

B16. Arena borrow-escape holes (intended = REJECT the escape): closure-body carve-out
     escape (:408), nested-arena depth-blindness (:409), arena_escape_*_error
     fixtures compiled instead of rejected because the self-host lacks a borrow-check
     diagnostic pass (:265/:574/:936). Intended semantics = reject (Rust rejects);
     the gap is a MISSING DIAGNOSTIC PASS, not an undecided rule.

B17. Deref/UB accept-and-miscompile with a clear reject-intent (Rust already
     rejects): `int y = *x` explicit-VarDecl skips initializer inference (:341);
     the self-host silent no-op / has_side_effects DCE gap (:690); is_collection_
     assignment laxity (:692). Intended = reject / preserve side effects; clear.

B18. self-host `None()` (with parens) mistypes to Option[int] (:662); bare-None
     forward-callee silent (Option__T){0} (:432); `!None` at &/! param (:435) —
     intended = infer from consuming position (bare None already correct).

B19. Rust-side reference defects the self-host SURFACED, direction decided by "which
     is reference-grade": method default-arg-fill on equip methods rejected by Rust
     (:133, fix Rust), rwlock compound-assign self-deadlock in Rust (:75, self-host
     correct → fix Rust), Custom-drop struct-as-field leaks inner fields in Rust
     (:474, self-host correct → fix Rust), GIR-validator panic on loop-reassigned
     !-param (:251). Intended behavior is agreed; only the compiler-to-fix is named.

################################################################################
# LIST C — DECIDED-BY-FIXTURE / DECIDED-AD-HOC-IN-DONE spec points worth RECORDING
#          (these ARE spec content; already-made decisions, capture them as rules).
################################################################################

C1. cow_lazy_d1_alias_deadpath.gg — DECIDED: an alias bind of a lazy collection-
    member + a source mutation on a never-taken branch executes ZERO clones; output
    = Rust oracle. (Provenance-by-slot-aliasing; the guarded materialize on a dead
    branch never runs.) Spec rule for lazy CoW.

C2. cow_struct_sever_on_mutation.gg — DECIDED: after `Spanned b = a` the two share
    heap field data via a Ptr alias; the FIRST mutation through either name triggers
    cow_before_mutation, cloning for the OTHER alias; reading the un-mutated alias
    sees the pre-mutation value. (Bare-assign = borrow; mutation = sever.)

C3. cow_amp_owned_writethrough.gg — DECIDED: `&` of an OWNED (unique) root writes
    through; the materialize is a no-op. (Guards against over-materializing a value
    that legitimately owns its buffer.)

C4. cow_fieldpath_method_caller_untouched.gg — DECIDED: a mutating METHOD on
    `param.field` through a bare parameter materializes the immutable-in-context ROOT
    struct; the caller's resource field is UNTOUCHED.

C5. cow_lazy_staletag / cow_lazy_severorder — DECIDED: reassigning a lazy local then
    mutating the source emits no pointless guarded clone (stale-tag clearing);
    `v = w` routes through cow_sever_all_aliases_from BEFORE cow_before_mutation and
    that sever path must also materialize the lazy-tagged ref.

C6. Set[T] is insertion-ordered; HashSet[T] is unordered (per language-reference
    :3278/:3303). DECIDED in DONE (CkOrderedSet/CkHashSet split). Spec rule.

C7. Reading a RESOURCE-typed module static via a bare-identifier bind DEEP-CLONES
    (does not alias the global's heap buffer). DECIDED ad-hoc in DONE round-40 T-C
    ("self-host toward Rust"; Rust deep-clones the resource GlobalRef at the bind).

C8. A deferred-clone bind whose source is MUTATED-after-bind must fire the clone
    EAGERLY at the bind, before the clobber (DONE round-40 T-D EagerAggClone +
    pristine gate). Spec rule for aggregate field-read binds.

C9. User operator-overload dispatch = a bare `Type__method` symbol, no vtable
    (matching Rust). DONE round-42-A. Dispatch-convention spec rule.

C10. String.display() returns the RECEIVER (identity, faithful to Rust methods.rs:782)
     → a binding form `String d = s.display()` would ALIAS d and s (both droppable).
     DECIDED (do NOT add a clone — diverges from Rust + regresses zero-copy). Filed
     latent. DONE round-41 S-A.

C11. security_known_unsafe bucket currently holds exactly ONE fixture:
     attack_45_deep_recursion (KnownBug::SanitizerTrips). DECIDED BY-DESIGN: unbounded
     recursion hits the OS guard page → SIGSEGV, "matches Rust/C++/Zig; a per-call
     depth counter isn't free." The semantic hole it represents = no stack-depth
     guard (accepted, documented). tests/security.rs:794.

C12. Reference-grade REJECTION TRACK owner steer (2026-07-05): PHASE 1 (until 100%
     parity) = only bounded FP-enumerable single-node rejections, opportunistically;
     PHASE 2 (after parity) = the GENERAL type/arg/trait/const/borrow enforcement
     pass. This is itself a decided meta-rule about WHEN the List-A rejection
     decisions get built. TODO.md:924-936.
