# SCOUT BRIEF — Chain F: snag #11 (cross-error-type auto-propagation miscompile)

Status: v1 (orchestrator draft, 2026-06-11, on gorget-1 tip `ece4dcf4`
post-Chain-D-integration). This is a **read-mostly SCOUT** (verify premises +
prototype + MEASURE), not an executor. Deliver findings INLINE. No production
commit; a throwaway prototype branch in your worktree is fine.

## Worktree discipline (read FIRST, non-negotiable)
Run `pwd` and `git rev-parse --show-toplevel` FIRST; confirm both point inside
YOUR worktree. Then `git merge --ff-only gorget-1` so the worktree is at the
current gorget-1 tip (`ece4dcf4` or later — `git log -1` to confirm). NEVER
touch `/workspace/gorget-1` directly; no absolute `/workspace/gorget-1/...`
paths; do NOT `cd` there. If `pwd` reports `/workspace/gorget-1`, STOP and
report it. Stage nothing into gorget-1; this is a scout.

## The bug (one sentence)
A `throws A` callee called inside a `throws B` caller (A ≠ B, no `From[A]`
equipped on B, no `rethrow`) type-checks CLEAN and is miscompiled: the
auto-propagation re-wraps the callee's RAW error payload into the caller's
Error variant → backend `memcpy` of `sizeof(B)` bytes from a `sizeof(A)`
value = an out-of-bounds read / type confusion (also a drop-correctness hazard:
B's drop fn runs over reinterpreted A bits). Repro:
`docs/plans/snag11_cross_error_propagation.gg` (BigErr is larger than String;
the caught `e.a/b/c/d` print garbage; reverse direction silently truncates).

## Ground the design in the DOCS FIRST (read before touching code)
The intended semantics are EXPLICIT — design toward what the language SHOULD
do, not what the code currently does (CLAUDE.md "Don't redesign around compiler
gaps" / "Self-host as the elegance showcase"):
- **`docs/language-design.md` §36.3** ("File Processor with Error Handling"):
  cross-type auto-propagation is **From-MEDIATED**. `equip ProcessError with
  From[IoError]:` + `auto file = fs.File.open(path)` ⇒ the comment says
  "auto-propagates IoError → ProcessError". This is the reference shape.
- **`docs/language-design.md` §"trait From[T]"** (~line 3386): `From[T]` =
  `.from(T)` infallible conversion (static method); `TryFrom[T]` = fallible.
- **`docs/language-reference.md`** — the authoritative error-handling /
  `throws` / auto-propagation (`?`-equivalent) semantics. Find and cite the
  section that defines when a callee error propagates into a caller's error
  channel.
- **`docs/devbook/`** — the lowering/auto-propagation pipeline doc (and
  devbook/24 layering: the fix must WRITE-THROUGH a typed metadata axis, not
  reconstruct from names). Cite the section your design rests on.
Cite the doc sections your proposed design rests on in your deliverable.

## Premises to VERIFY against CURRENT source (file:line; I pre-confirmed these
## on `ece4dcf4` — re-confirm they still hold and trace the full data path)
1. **Typecheck discards the error axis.** `is_auto_propagation_compatible`
   (`src/semantic/typecheck.rs:4248`) destructures `(ok_type, _err_type)` and
   USES ONLY `ok_type` — `_err_type` (line ~4251) is literally discarded; it
   returns `true` whenever `self.current_function_throws` (~4273) regardless of
   callee-E vs caller-E. Also audit the centralized producer-peel /
   `current_fn_can_propagate` path (the Snag #35-followup inversion, ~4194 per
   the TODO) — it peels `Result[T,E] → T` regardless of E. ENUMERATE every call
   site of `is_auto_propagation_compatible` (grep shows ~10 in typecheck.rs:
   1478, 2188, 2995, 3053, 3104, 3237, 3264, 3273, …) and decide which are
   error-propagation positions that need the E check vs. unrelated bool-cond /
   assignment coercion uses that must NOT change behavior. This split is the
   crux of not over-rejecting.
2. **Lowering re-wraps the raw payload (the memcpy birth).**
   `emit_result_auto_propagate` (`src/ir/lowering/exprs/mod.rs:2784`), error
   path ~`2848-2869`: loads the callee's payload as `err_field_type` (line
   2848-2853, the CALLEE's error type) then `enum_init`s the CALLER's Result
   (`fn_res_type`, line 2867-2869) with it. Confirm `enum_init` lowers to a
   variant-sized store/memcpy (find the backend lowering) — that is the OOB
   read. `should_auto_propagate` (~2929) has no E check either.
3. **The self-host twin has the identical hole.** `maybe_auto_propagate`
   (`tests/fixtures/self_host_lowerer/lower_match.gg:1005-1067`, re-wrap at
   ~`1058-1061`) — ported faithfully from Rust. A complete fix fixes BOTH
   compilers (the self-host is the showcase + the bootstrap). NOTE: lower.gg /
   lower_match.gg may be SYMLINKED across self_host_* dirs — `md5sum` the
   copies and identify which dirs must change.
4. **`From` exists but is never consulted at the boundary.** Confirm the
   `From[T]` trait (`src/semantic/traits.rs:762` per the TODO) + its derive
   (`src/semantic/derive.rs:322`) + how an `equip B with From[A]:` impl is
   registered and looked up (trait registry). The fix needs to ASK "is
   `From[calleeE]` equipped on callerE?" at the propagation boundary and, if
   so, resolve the `B.from(A)` impl to a callable the lowering can emit.
First action in your report: **reproduce the bug** — `gg build` + run the
repro, show the garbage `e.a/b/c/d` (and ideally the `-Wstringop-overread`
or ASan over-read). If it does NOT reproduce on `ece4dcf4`, STOP and report
(premise contradicted).

## The fix to PROTOTYPE (design — adjust if the docs/source say otherwise)
Fix the CLASS at both layers, write-through typed metadata (devbook/24):
- **Typecheck (the gate + the conversion decision).** At each
  error-propagation position, resolve callee-E and caller-E (the throws/Result
  error type). Three cases:
  - **same type** → OK as today (no conversion).
  - **different + `From[calleeE]` equipped on callerE** → ACCEPT, and RECORD
    the resolved From impl as TYPED METADATA the lowering can read (a side-table
    keyed by the propagation expression / call site, holding the `B.from`
    callee — NOT a name-match downstream). This is the §36.3 path.
  - **different + NO From** → **teaching ERROR** (`gg check` rejects), message
    suggesting `equip CallerE with From[CalleeE]:` or an explicit `rethrow`,
    citing §36.3. Model the message on the existing `str(x)`-rejection / type
    mismatch precedent.
- **Lowering (both compilers).** At the re-wrap choke point
  (`emit_result_auto_propagate` err path; self-host `maybe_auto_propagate`
  re-wrap), when the typed metadata says "convert", emit `CallerE.from(e)`
  (call the recorded From impl on the callee payload) and `enum_init` the
  caller Error with the CONVERTED value. Same-type keeps today's path.
- **DESIGN NOTE (owner, 2026-06-11):** keep §36.3 semantics strictly —
  From-equip = implicit conversion; `rethrow` = per-site escape hatch; neither
  = teaching error. Do NOT synthesize conversions WITHOUT a From impl
  (payload-carrying errors make Zig-style implicit coercion unsound; the
  gorget-js wrong `throws String` annotation is the cautionary tale). If your
  source/doc reading suggests a different shape, FLAG it — don't silently
  diverge.

## END-TO-END prototype (compile AND run AND diff — NOT source-read; scout
## yield estimates in this tree have been ~0 when source-read only)
1. **Negative (check-reject):** the repro `snag11_cross_error_propagation.gg`
   must now FAIL `gg check` with the teaching error (was: compiled to OOB
   read). Show the exact diagnostic.
2. **Positive (From-mediated):** author a fixture where `B` equips
   `From[A]` and `inner() throws A` is called in `outer() throws B` — must
   compile, RUN, and produce CORRECT output (the converted error caught with
   intact fields). Show stdout. This proves the conversion path emits
   `B.from(e)` and the memcpy is gone (re-run under ASan / `-Wstringop-overread`
   to confirm the over-read is eliminated).
3. **Same-type unaffected:** an existing `throws E` … `throws E` fixture still
   auto-props with byte-identical emitted C (the gate must be a no-op when E
   matches). Pick one from the corpus and diff emitted C pre/post.

## MEASURE corpus fallout (THE BLOCKING QUESTION = bootstrap viability)
Run EVERY existing program through the NEW check and report what breaks:
- All `tests/fixtures/*.gg` (the integration corpus) — does any fixture
  currently rely on cross-type auto-propagation WITHOUT a `From` equip? List
  each by name with the offending call site.
- **The self-host sources** (`tests/fixtures/self_host_*/**.gg`) AND
  `lib/std/**` + prelude — this is the bootstrap-viability question. If the
  self-host or stdlib propagates A→B without From anywhere, the new check
  REJECTS the bootstrap and Chain F cannot land as a hard error without a
  migration (add the missing `From` equips or `rethrow`s first). QUANTIFY:
  how many sites, in which files, and is each a real bug (should be From/
  rethrow) or a case the design must permit?
- Decide + recommend: is the corpus clean enough to land the check as a
  hard error directly, or is a migration (add From/rethrow at N named sites)
  a prerequisite? If migration: enumerate the sites. This number drives the
  owner's go/no-go and the executor brief's scope.

## Deliverable (INLINE in your final message)
1. **Repro confirmation** (the garbage output + the over-read evidence).
2. **Premise audit** — each of the 4 premises: CONFIRMED/CONTRADICTED with
   file:line, plus the call-site split for premise 1 (which uses need the E
   check).
3. **Proposed design** — the typed-metadata axis (where it lives, how it's
   keyed, the accessor), the typecheck change, the both-compiler lowering
   change; grounded in the cited doc sections.
4. **End-to-end prototype results** — negative reject diagnostic, positive
   From-mediated stdout, same-type byte-identity; the throwaway diff INLINE
   (Rust `src/` + both self-host dirs).
5. **Corpus-fallout table** — every breakage with file:line; the
   bootstrap-viability verdict; land-direct vs migration-first recommendation
   with the site list.
6. **Go/no-go + open questions** for the owner.

STOP and report immediately if: the bug doesn't reproduce; the docs contradict
the From-mediated design; or the corpus fallout shows the bootstrap itself
relies on the unsound propagation (that reshapes the whole task).
