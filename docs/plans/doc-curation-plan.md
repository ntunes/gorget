# Documentation-Curation Scout — Deliverable (2026-07-16)
> **REVIEW PASS 1 FOLDS (2026-07-16 — AUTHORITATIVE overrides of the classification below; a
> confirming pass follows):**
> 1. `define-gorget/scouts/scout-rvc-compound-assign.md` → **LIVE** (was UNCLASSIFIED — created
>    the same day, after this scout's sweep). It is the IN-FLIGHT RV-C track's scout deliverable
>    AND the sole preserved repro for the filed ggdef compound-index double-eval bug. Keep until
>    RV-C lands. The body's claim that RV-* tracks "have no brief/scout files yet" is FALSE for
>    RV-C (and now also 1C: `cow-track-1c-scout.md`/`-brief.md` + `cow1c_proto.patch` are LIVE).
> 1b. EXPLICIT LIVE enumeration (pass-2: the fold prose alone did not amend the body's
>    bulk-delete instructions, and the link gate does NOT catch patch files cited only from
>    other plan files): `cow-track-1c-brief.md`, `cow-track-1c-scout.md`,
>    `scouts/patches/cow1c_proto.patch` — all three sit in the same enumerated keep-set as
>    the tuple-DefId carve-out until 1C lands. The body's line-81 and Slice-1 instructions
>    are amended in place to name BOTH patch exceptions.
> 2. `define-gorget/scouts/scout-wave-census.md` (the REPORT) → **LIVE** until Batch C closes —
>    cited BY PATH from TODO's ratified wave plan, HANDOVER, and the out-of-repo MEMORY.md; the
>    C3 sigil counts trace to it. The spent MANDATE file `wave-census-scout-mandate.md` stays
>    DELETE (different file).
> 3. `define-gorget/decision-batch-4-proposal.md` → **LIVE** until the D14 book write-through —
>    the body's "rulings live in decisions.md" rationale is WRONG for §D14: the owner-flagged
>    MUST-REACH-THE-BOOK copy/move/view derivation + what-chains table exists ONLY there
>    (decisions.md:711-717 points AT it). Alternative: absorb §D14 into the book via slice 2,
>    then delete.
> 4. Slice-1 link gate SPELLED OUT: before any `git rm`, grep every candidate path against
>    TODO.md + DONE.md + docs/plans/define-gorget/HANDOVER.md + decisions.md + docs/book +
>    docs/devbook + docs/language-reference.md + docs/language-design.md + spec/prose; AND
>    check the out-of-repo memory dir (`~/.claude/projects/.../memory/`) which an in-repo
>    git-grep cannot reach. Any hit = STOP on that file.



Scope: every file under `docs/plans/` (176 files) + a currency audit of the official docs
(book / devbook / language-reference / language-design / spec/prose) against landed reality.
Read-only scout; nothing deleted or edited. Evidence: DONE.md entries, commit hashes, TODO/HANDOVER references, source file:line.

Disposition tallies: **DELETE ≈ 156** (completed/superseded — all git-recoverable) · **LIVE = 17** (active tracks / kept-live spec files) · **ABSORB = 2 (+1 borderline)** (durable design prose not yet in official docs).

---

## (a) CLASSIFICATION MAP

### LIVE — keep (17)

Always-live spec files (standing rule):
- `define-gorget/HANDOVER.md`, `define-gorget/decisions.md`, `define-gorget/rfc-ggc-ggdef.md`

Active define-gorget tracks (pending in TODO):
- `define-gorget/scouts/scout-tupledefid.md` + `define-gorget/scouts/patches/tupledefid_proto.patch` — tuple/pattern `SVarDecl` DefId FP; TODO.md:270 "prototype PROVEN … Brief→gauntlet NEXT".
- `define-gorget/scouts/scout-cbackend-coalesce-bug.md` — repro source for TWO still-open bugs (TODO:248 field-access wrapper/tuple soundness holes; TODO:268 dormant `Dict.remove` coalesce).

Top-level LIVE (still referenced as pending/active design in TODO):
- `cast-via-construction.md` — owner-approved design, impl DEFERRED (TODO:179/220 "PAUSED; design SETTLED; §7+§8 authoritative"). Eventual ABSORB→language-reference once built; keep LIVE meanwhile.
- `error-model.md` — ~90% superseded by D23/D25/D26 but TODO:221 still points here for 4 unresolved §9 questions (fast-knob, meta-overflow split, `Never` spelling, Result reconciliation). Keep until those relocate.
- `method_resolution_totality_phase2.md` — TODO:584 "Phase 2 … BLOCKED by a measured superset failure. Plan `…method_resolution_totality_phase2.md`."
- `builtin_method_decl_port.md` — the "Phase 2a seed" for the above; not landed.
- `self_host_namematch_retirement.md` — companion analysis to the port plan; predicates un-retired. (Consolidatable INTO the port plan — owner call.)
- `gg_impl_endgame.md` — TODO:222 "GG_IMPL ENDGAME TRACK (owner-chosen, ACTIVE)".
- `devbook_honesty_audit.md` — TODO:675 "Work the honesty-audit catalog (A–F)"; still pending.
- `gorget-sheets-snag-report.md` — TODO:314+ snags #53–#58 filed OPEN with `#[ignore]` tests.
- `cow-writethrough-materialize-closed-set.md` — the LIVE CoW campaign plan (v3; waves 1C/1A/2/3 pending).
- `cow-wave0-measure.md` + `artifacts/cow_wave0_measure.log` — TODO:247 cited as live measurement evidence for open CoW bugs (`v[i].bump()` HIGH; struct-match mis-bind).

### ABSORB — move content to official docs before deleting (2 + 1 borderline)

| FILE | TARGET | CONTENT |
|---|---|---|
| `define-gorget/scouts/scout-a33-fault-model.md` | `spec/prose/` (§10.9 + §10.5 rewrite) + book/10 + language-design §6 | The D24–D28 *rulings* are in decisions.md, but the **prose** is not written anywhere: the D24 supervised-boundary spec, the fault-catch-removed (Swift) model, the fallible-operator taxonomy, and the "why not dynamic exceptions" appendix. decisions.md:596 explicitly says this prose "ships with [D24]" — i.e. in the not-yet-run Batch-C/C2 track. This scout is that track's source-of-truth. Keep until C2 absorbs it. |
| `define-gorget/scouts/scout-c-prior-art.md` | `rfc-ggc-ggdef.md` "Prior art & rationale" appendix (or a devbook methodology note) | The WASM/SML/CakeML/K-Framework/MiniRust comparison, the 9-failure-mode→structural-guard table, and the mechanization roadmap ("lazy CoW refines eager value semantics" as the first theorem; Aeneas/Charon/Lean4). The RFC operationalizes the guards but doesn't carry this rationale. Small, absorb-now-able. |
| `error-model-phase2-design.md` (borderline; else DELETE) | `docs/devbook/13-ownership-in-ir.md` or `15-drop-elaboration.md`, one paragraph | Durable, source-verified invariant from a *cancelled* feature: "Gorget's `throws`/`Result` path is already a deep, cross-frame, drop-correct **by-value** error channel (`emit_early_exit_drops` at each early exit; no unwind substrate)." Absorb that one paragraph if not already stated in the devbook, then DELETE. |

### DELETE — completed or superseded (≈156; every one git-recoverable)

**Top-level docs/plans (43 md + 5 non-md).** Full per-file evidence gathered; grouped by cluster:
- Case-B parity cluster (LANDED, parity now 800+/1083): `caseb-aclass-scout`, `caseb-aclosure-scout`, `caseb-aclosure-boxfix-scout`, `caseb-alpha-slice4{,b,c}-scout`, `caseb-beta-flip-{production,remeasure}-scout`, `caseb-inc1c-scout`, `caseb-pair-lowering-scout`, `caseb-track-beta-scout`, `ill-typed-case-b-scout` (commits 2026-06-22..24; e.g. "land Case-B pair …731→737/1069").
- Error-model fault-catch increments — CANCELLED by D25 (DONE 2026-07-11) or landed (Inc-2.1d Bounds): `error-model-inc21-scout`, `error-model-inc21c-scout`, `error-model-inc21d-bounds-scout`, `inc21b-llvm-fault-slot-scout`, `error-model-phase2-inc1-scout`, `error-model-phase2-A-vs-B`.
- Landed fix-briefs: `elemdrop-fix-brief` (DONE 2026-07-06), `matcluster-fix-brief`, `strmove-fix-brief`, `unwrap-fix-brief` (all "brief CLOSED" commits 2026-07-06), `fstring-format-spec-brief` ("round-3 +8 → 771/1083"), `b2-index-of-option-brief` ("index_of→Option → 781/1083"), `bugB_static_collection_init` (bug-B landed `3d22e234`).
- Round-scout / triage artifacts (curation-round "preserve … deliverable" commits): `parity-triage-scout-2026-06-24/25/25b`, `return-expr-body-scout`, `selfhost-return-expr-body-scout`, `throws-catch-resource-payload-scout`, `lints-case-none-ratchet-scout`, `compound-assign-guard-scout`, `option-none-static-init-scout`, `cast-via-construction-inc1-scout` (SUPERSEDED per TODO:179).
- Refuted/closed investigations (no live reference): `perf-embed-gate-scout` (REFUTED NO-GO), `sweep-walltime-bisect` (premise INVERTED), `loader-qualify-port-scout` (REFUTED +0), `regex-type-reg-scout` (findings filed as bugs), `meta-for-binding-scout` (REFUTES the hypothesis).
- Executed scaffold: `devbook_plan.md` (devbook/ chapters 00–27 now exist).
- Landed-slice brief: `cow-track-1b-brief.md` (1B LANDED `565392d8`, DONE 2026-07-16; its spillover HIGH lives on the LIVE campaign plan, not the brief).
- Non-md scratch: `artifacts/bugB_static_array_literal_repro.gg`, `artifacts/builtin_method_decl_step2_consumer.patch`, `artifacts/perf_phase_timer_driver.patch`, `chainE_artifacts/measurements.log`, `none_peel_fix.patch`.

**define-gorget briefs (~33) — every one a LANDED track's brief** (commit in DONE/HANDOVER): T1 `d412990a`, T2a-rust `82d50b0f`, T2a-selfhost `9bb33ec6`, T2b `c3962cd2`, T3a `9d9a6d83`, T3b `d70fefe1`, rr-a `6d12c5ad`, rr-b `6e51fd18`, rr-c `dd05ebb8`, rr-d `874b6371`, A1 `d59605fc`, A2-R1 `b72ef446`, A2-R2 `b4b6124a`, A2-S `3b741a8a`, A3 `414e652a`, B0 `40772fd4`, B1 `e49da630`, B2 `629ca465`, self-root `1eae75ca`, callarg-core `7dbb3f8d`, callarg-sigil `3681402c`, fieldaccess `f9a9da3d`, liveness `2928d9cb`, liveness-fp `567f053e`, ggdef-liveness/elab-eval `67ce92f8`, is-scrutinee `146c4830`, reject-diagnostic `cbb21f28`, coarsekind `c082ae96`, rvg-frontmatter `95b54cfb`, `phase0-brief` (CLOSED), `phase1-infra-brief` (COMPLETE), `decision-batch-4-proposal` (batch CLOSED, rulings live in decisions.md), plus the two spent process-mandates `a33-fault-model-scout-mandate` and `wave-census-scout-mandate`.

**define-gorget scouts (~26) — all for LANDED tracks:** `scout-trap-normalization`, `scout-t2a-production-emit`, `scout-t2a-selfhost-emit`, `scout-t2b-bounds-locations`, `scout-d23-throws-totality`, `scout-t3b-smith-throws-tier`, `scout-a2-r2`, `scout-a2-s`, `scout-batch-b`, `scout-b2`, `scout-selfroot`, `scout-callarg-normalization`, `scout-callarg-sigil`, `scout-coarsekind-split`, `scout-codegen-is-scrutinee`, `scout-ggdef-elaborate-move`, `scout-ggdef-liveness`, `scout-ggdef-verdict-triple`, `scout-liveness`, `scout-liveness-ext`, `scout-selfhost-reject-diagnostic`, `scout-rvg-frontmatter`, `scout-a-semantic-questions`, `scout-b-bug-questions`, `scout-wave-census`. (ABSORB carve-outs: `scout-a33-fault-model`, `scout-c-prior-art` above; LIVE carve-outs: `scout-tupledefid`, `scout-cbackend-coalesce-bug`.)

**define-gorget/scouts/patches/ (49) — entire tree is prototype scratch for LANDED tracks → DELETE**, filename↔landed-commit verified. **Sole exception kept LIVE: `tupledefid_proto.patch`.**

---

## (b) OFFICIAL-DOCS GAP LIST (ranked by impact)

**What is ALREADY current and well-documented** (no action): D11 trap format (reference §10.9:2666, §10.10:2685-2688; spec/prose/trap-codes.md) · D23 throws totality (reference §10.1:2503) · D10 exclusivity + D10(b) place-overlap + Copy-read exemption ADDENDUM (reference §9.4:2330-2388 — excellent; language-design §3.5; book/12:84-91 correctly frames `f(&x,&x)` as a compile error) · D12 drop-purity six positions (language-design:461; book/11:65; reference §9.1:2290; spec/prose/04-drop-purity.md:23) · exit-code scheme 0/1/2/101/103 (reference §10.10:2678-2688; spec/prose/trap-codes.md:10-14) · may-move revive + ConsumeCallable single-use (reference §9.1 rule 9:2286, §7:455-463, :1120) · D19 break-value removed (reference §6.7:1195) · D10 local `&`-binds illegal (reference §7:1757-1765). Book/devbook relative cross-references: 0 broken. **The reference is honest about shipped-vs-ratified: `**` power (D28) and `^` move sigil (D27) are NOT claimed early — good.**

Ranked gaps:

1. **[HIGH — contributor gap] The ggdef executable-definition subsystem has NO devbook home.** `grep ggdef docs/devbook/*` → nothing; it appears only in language-reference.md and the LIVE plan RFC. The landed, load-bearing mechanics — `verdict = check_liveness ∘ eval` (elaborate∘eval boundary, RFC §2.3.1), the dynamic (eager value-semantics) vs static (borrows+lazy-CoW) ownership note (RFC status/§2.2/D1), the conformance lanes + spectests-generated-from-ggdef (§4), the import ratchet — are contributor-facing and live only in `docs/plans/define-gorget/rfc-ggc-ggdef.md` (a plan). Recommend graduating a devbook chapter (e.g. `30-executable-definition.md`, or fold into README + `27-comparison-bootstrap`). Sourced from RFC §2.2/§2.3/§2.3.1/§2.7/§4 + `scout-a33`/`scout-c-prior-art` (the two ABSORB files). Mandate item #9.

2. **[MED — normative registry incomplete] Two ratified diagnostic codes missing from `spec/prose/diagnostic-codes.md`:** `E_LocalBorrowBind` (D10, in reference §7:1763) and `E_UnhandledThrows` (D23, in reference §10.1:2503) are NOT in the registry (registry has E_NoFieldFound, E_MoveWithoutOperator, E_MoveInLoop, E_DoubleMove, E_BorrowConflict, E_UseAfterMove). Add both rows. Small, mechanical.

3. **[MED — stale + war-story] `docs/devbook/11-copy-on-write.md:431-449` "Implementation status — converging to the uniform rule".** (a) Narrates session chronology ("landed `d1b1744a`, round-33"; "`2c7fbf04`, round-34") — violates the timeless-narrative rule (belongs in devbook/29 or DONE). (b) Its "one remaining unconverged shape = untracked alias chains" now UNDERCOUNTS: the CoW write-through campaign RE-OPENED (`cow-writethrough-materialize-closed-set.md` LIVE; 1B landed 2026-07-16; gaps B, method-receiver `v[i].bump()`, struct-match mis-bind still open). This marker is a docs-update target OWNED by the live CoW campaign (its Wave 3 "Spec lock + docs" lists "devbook/11 status"). Recommend: refresh to the campaign's real closed-set now, strip the round-33/34 chronology, and remove the marker when the campaign closes. Mandate items #2 (CoW §3.1 ↔ marker; §3.1 = language-design §3.1/D1, the canonical rule the campaign derives from) and #3.

4. **[MED — stale prose, but deletion pending] reference §10.5 "Fault catch" (2588-2600) contradicts shipped single-call-deep behavior.** Prose says "A fault raised inside a function the expression calls is not caught"; the a33 scout found single-call-DEEP catch IS shipped. BUT D25 (ratified) REMOVES fault-catch entirely (Batch-C/C2, not yet run). Recommendation: do NOT patch now — rewrite §10.5 + book/10 "Recovering from Faults" wholesale when C2 lands (tracked by decisions.md D25 "Docs write-through: §10.9 + book"). Flag so a reader knows the section is on death row.

5. **[LOW-MED — thin] reference §9.5 Branch Merging (2390-2402)** describes only the conservative "moved in any branch ⇒ moved after" rule and omits the landed may-move dataflow nuance (branch-join REPLACE-with-union-of-live-end-states → move-then-reinit-in-BOTH-arms stays live; production `merge_branch_states` + ggdef `union_all`). Not wrong (§9.1 rule 9 covers reassign-revive generally), just under-specified. One sentence.

6. **[LOW — discoverability] Batch-5 ratified-but-unimplemented forward-pointer.** decisions.md holds D24–D28 honestly ("spec-only now; impl = …"), and the reference correctly avoids claiming `**`/`^`/`+!` early. Optional nicety: a short "Planned (ratified, not yet implemented)" note in the reference pointing at the ledger, so a reader learns the future syntax exists.

7. **[LOW — state-doc currency, out of prune-scope but noted] `HANDOVER.md` "NEXT ORCHESTRATOR START HERE" (lines 394-401)** still lists the coarse-kind split as pending; it landed `c082ae96` (DONE 2026-07-16). The LIVE BATCH-A block is current; only the NEXT list lags. Refresh at next handover.

**Secondary curation candidate (outside the docs/plans mandate, but squarely inside the owner's "scattered docs" directive): `docs/internals/` (22 files, frozen 2026-06-06).** A legacy parallel tree largely superseded by devbook (copy-on-write→11, layering-discipline→24, structural-guards→25, safety-checker→10, ownership-ir/unified-resource-model→13, meta→06, lir-design→14, stdlib-design→23, self-host-resource-model→26) plus completed-plan cruft (`bir-module-synthesis-plan`, `lir-backend-lift-plan`, `llvm-backend-plan`, `handover-option-c-bir-synthesis`, `codegen-gap-spike`, `tier1c-cluster1-burn-down`, `clone-emission-at-calls`, `fstring-interp-as-expr`, `method-level-inference`). Recommend its own reviewed audit slice (DELETE-vs-ABSORB needs a per-file content-diff against the devbook counterpart — not folded into this plan).

---

## (c) PROPOSED EXECUTION SLICES (each = its own reviewed track)

**Slice 1 — Bulk delete (zero content loss).** `git rm` the ≈156 DELETE files (all top-level completed clusters + all define-gorget briefs/scouts for landed tracks + the entire `patches/` tree except `tupledefid_proto.patch` AND `cow1c_proto.patch` (both LIVE — queued/in-gauntlet tracks)). Reviewer verification: for each file, confirm (i) it is NOT in the LIVE or ABSORB lists, and (ii) its landing commit resolves in DONE.md/git OR it is a "preserve … deliverable"/REFUTED round artifact. Deletion is git-recoverable, so the bar is "no unabsorbed durable content" — which slices 2–3 handle first. Gate: `git grep` finds no LIVE doc still linking a deleted path.

**Slice 2 — Absorb before delete (content preservation).** (a) `scout-c-prior-art.md` → append a "Prior art & rationale" appendix to `rfc-ggc-ggdef.md` (do-now); then DELETE the scout. (b) `error-model-phase2-design.md`'s by-value-error-channel paragraph → `devbook/13` or `15` if not already stated; then DELETE. (c) `scout-a33-fault-model.md` — do NOT delete yet; it is the source prose for the unstarted C2 §10.9/§10.5 rewrite. Reviewer verification: diff the absorbed text against the target section; confirm the source file is then either deleted (a,b) or explicitly retained-until-C2 (c).

**Slice 3 — Official-docs currency fixes** (gaps 2, 5 now; 3 coordinated with the CoW campaign; 1 as a scoped ABSORB; 4 deferred to C2). Minimal set to land now: add `E_LocalBorrowBind` + `E_UnhandledThrows` rows to `spec/prose/diagnostic-codes.md`; one-sentence §9.5 may-move note. Reviewer verification: registry row count +2 and both codes cross-link to the right prose chapter; §9.5 sentence matches production `merge_branch_states` semantics (cite source).

**Slice 4 (bigger, own scout) — ggdef → devbook chapter** (gap 1). New `devbook/30-executable-definition.md` sourced from RFC §2.2/§2.3/§2.3.1/§2.7/§4 + the two ABSORB scouts, covering: the executable definition, verdict = check_liveness ∘ eval, dynamic-vs-static ownership, conformance lanes, the import ratchet. Reviewer verification: a contributor can find the verdict-triple/elaborate∘eval story in the devbook without opening a plan file; claims cross-checked against `spec/ggdef/` source.

**Slice 5 (deferred, own audit) — `docs/internals/` legacy tree.** Per-file DELETE-vs-ABSORB against the devbook counterparts. Not in this mandate; flagged for the owner.

---

### Cross-cutting notes
- Every DELETE is git-recoverable; the only *irreversible* risk is losing unabsorbed design prose — slices 2 and 4 close that before slice 1 runs.
- `cast-via-construction.md` and `error-model.md` are kept LIVE now but are future ABSORB→official-docs once their tracks build/resolve; not deletable today.
- The RV-A…RV-F review tracks and Batch C are LIVE but have no brief/scout files yet — nothing to prune or keep there.
