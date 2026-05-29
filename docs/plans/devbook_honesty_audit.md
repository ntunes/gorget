# devbook documentation-honesty audit — findings catalog

> Output of the cross-doc honesty triangulation run during devbook chapter generation (2026-05-29, workflow `wf_863ddf96-627`, verified against tip `ffd58b65`). The verify stage of each chapter compared source+tests ⟷ `language-reference`/`language-design` ⟷ `docs/book`, classifying disagreements. ~40 findings; the chapters themselves were corrected (107 edits) to follow source — the items below are the *external* artifacts (source comments, internals docs, spec, memory) that still need triage. Triage each: **implement** the missing behavior or **clean up** the doc.

## A. Genuine latent bugs / gaps (corroborated against source)

- **A1 — `Ordinal` trait is documented but never registered → never validated.** `docs/book/appendix-traits.md:92` + `docs/language-reference.md` (~:2705/:2783) document `Ordinal` with `int ordinal(self)`, but it is absent from the resolver's builtin-trait list (`grep Ordinal src/semantic/resolve.rs` → nothing). So `equip X with Ordinal:` is silently demoted to an *inherent* impl (trait_def_id=None) and its signature is **never** checked — `String ordinal(self)` compiles clean, whereas the same wrong signature on a registered builtin (`Comparable`) is correctly rejected. **Decide:** register `Ordinal` (resolve.rs builtin list) / declare `trait Ordinal` in a lib, **or** remove it from the docs.
- **A2 — Self-host lexer is NOT "green" (MEMORY stale).** The self-host lexer keywords the `Box/Rc/Arc/Weak/Cell/RefCell/Mutex/RwLock` family (`self_host_lexer/lexer.gg:34,266-267` → `KwMutex` etc.) while Rust treats them as plain identifiers (`src/lexer/token.rs:317`). Any fixture using those names mismatches in `lexer_comparison`. A verify agent reported ~601/1120 mismatches (also citing a missing `SkFormat` f-string kind at `lexer.gg:42`). **Decide:** fix the self-host lexer keyword set + f-string classification, **and** correct MEMORY's "lexer green" claim. (Exact count: run `cargo test --test integration lexer_comparison -- --nocapture`.) Note: `Cell`/`RefCell` are still keyworded here despite being rejected.

## B. Stale source doc-comments (IMPL-AHEAD-in-source — cleanup)

- `src/bir/mod.rs:20-26` + `src/bir/validate.rs:23-27` — "Step 0 / trivial passthrough / empty allowlist" but `validate.rs:54-108` rejects 11 canonical ops and `lower.rs` is ~4058 lines of expansions. *(Already TODO finding #1.)*
- `src/compiler_data.rs:5-11` — calls the resources loader "future"; it shipped at `src/ir/resources.rs:38-73` (`table()`→`load_table()`→`walk_module`).
- `src/ir/mod.rs:288-291` — `fn_returns_borrowed` "consumer not yet wired"; the call-site auto-clone IS active at `src/ir/lowering/exprs/calls.rs:1401-1422`.
- `src/lir/mod.rs:719` — `HofExpand` "not yet emitted"; it IS emitted (`src/lir/lower/insts.rs:2571,2739,3206`) and expanded in BIR. `insts.rs:3256` "still flow through the per-backend inline expanders" is also stale (C backend makes `HofExpand` unreachable, `mod.rs:2051-2055`).
- `src/lir/mod.rs:884-885` — `CallByRef.fref` "validate_module bound-checks this"; the validator (`src/lir/validate.rs:223-228`) only checks definedness, not the `FuncRef` type.
- `src/loader.rs:621-654` — comment says std.iter auto-load "call site is intentionally disabled"; the live code at `:655-661` performs it (shipped 2026-04-23 per stdlib-design.md:1159).
- `src/loader.rs:204-212` — `STD_ITER_NAMES` still lists retired free fns (`sum_iter/product_iter/...`) per `lib/std/iter.gg:772-773`.
- Self-host fossils (the "elegance showcase" rule): `self_host_lowerer/lower.gg:1486` `decide_operand_at_consuming_arg` "dead code, no caller" (called at `:2508`); `lower.gg:997,1004` "self-host doesn't have drop_elab" (it does — `drop_elab.gg:673`, wired at `driver.gg:95`); `lower.gg:1338-1342` "all four modes lower to same ISlotLoad" (OpClone now emits a real clone, `lir_lower.gg:2453-2521`).
- `tests/integration.rs:13837` — comment calls `self_host_bootstrap_fixed_point` "currently #[ignore]"; the test at `:13895` is live (no `#[ignore]`).
- `src/intern.rs:3` — doc-comment "all source identifiers pass through the interner at lex time"; production reach is lexer-only (sole `intern()` at `src/lexer/mod.rs:413`).

## C. Code-smell / no-name-matching debt

- `src/ir/validate.rs:2180-2207` — `validate_no_null_assign_to_option_slot` doc claims recognition is "structural, not name-pattern," but the body is a pure prefix match (`name.starts_with("Option__")` …). CLAUDE.md "no name matching" debt.
- `src/ir/lowering/context.rs:345` — `extern_body_fns` is **write-only** (inserts at `lowering/mod.rs:707,828,909,1018`, no read site). Wire the intended clone-suppression or delete the field.
- LIR pipeline-order inconsistency: `src/main.rs` runs `optimize_module` before `wire_collection_bridges`/`promote_runtime_calls` (601 then 609/611); `src/lir/integration.rs` runs them in the opposite order (44/46 then 48).

## D. Stale `language-reference` / `language-design` (spec ⟷ impl drift)

- `language-reference.md:5258` — `sizeof(String) == 16`; actual is **32** (`src/semantic/meta.rs:1246`). Likely pre-unified-String, or anticipating the unshipped thin-pointer-String.
- `language-reference.md:43` — indentation tabs "must not be mixed"; impl is **spaces-only** (`LexErrorKind::TabCharacter`, `src/lexer/mod.rs:165-171`).
- `language-reference.md:240` — lists `f"""` as a "Multi-line format" kind; no such `StringKind` variant (`src/lexer/token.rs:744-752`).
- `language-reference.md:4638,4670` — `gg fmt` CLI omits `--check`/`-c` (impl: `src/main.rs:3281-3287`; book documents it at `appendix-cli.md:146`).
- **Package manager (big DOC-AHEAD-implement):** `language-design.md` advertises semver solving (`:83`), `[dev-dependencies]` (`:2907`), `gg update` and bare-name `gg add http` (`:2982`) — **none implemented** (`src/resolver.rs:203-211` errors on divergent sources; `src/manifest.rs:13-17` has no dev-deps; no `cmd_update`; `cmd_add` requires `--git`/`--path`, `src/main.rs:2202-2205`). Decide: implement vs trim docs.
- `language-reference.md:831-835` — import grammar omits `from X import Y as Z` aliasing and module-level `from X import *` (both implemented: `src/parser/ast.rs:296-328`, `src/semantic/resolve.rs:204-239`).
- `borrowed` extern return qualifier — shipped (`src/parser/ast.rs:138-142`) but undocumented in `language-reference.md` (§extern :917-988) and `docs/book/21-interop.md`.
- `language-reference.md:4904` — trace `return` event documents a `value` field; `src/report.rs:12` `TraceEvent::Return { function, depth }` has none.
- `docs/book/appendix-cli.md:168,184` — `gg sim --max-steps` documented but non-existent (`src/sim/config.rs` has no such field); the sim flag table also omits the five real flags (`--seed`, `--many-seeds`, `--ignore-leaks`, `--disable-isolation`, `--backtrace`).
- `language-design.md §7.3:1408-1413` — three closure capture modes; impl models two (`CaptureMode::{ByValue,ByMutRef}`, `src/ir/lowering/closures.rs:22-27`).
- `language-design.md` Phase-4 pipeline omits the **BIR** stage (between LIR and backends).
- "sole production backend" (`CLAUDE.md:53`, `language-design.md:3540`) vs LLVM-shipped-at-parity wording — align the phrasing.
- `language-reference.md §9.6:2297-2305` — fixed seven-point "Materialization points" list; source has no numbered list (driven by `ensure_owned_at_boundary`, `context.rs:1756`). Same closed-enumeration staleness as the CoW 6-vs-7 count.

## E. Stale internals docs (handled by the fold; delete/refresh on fold)

These mislead via IMPL-AHEAD/roadmap framing; the chapters already describe shipped reality. Refresh-or-delete when the chapter folds them:
- `README.md:39` "Pass 4.5 provenance.rs" (no such pass/file).
- `fstring-interp-as-expr.md` — a redesign *plan* (`StringSegment::InterpolationSrc`) never implemented; current code uses synthetic `1<<40` spans + re-parse (`src/parser/expr.rs:302-332`). Decide: implement or mark plan not-done.
- `method-level-inference.md:3` "Not yet implemented" — shipped (`typecheck.rs:4262,1805,5536`).
- `ownership-ir.md:89-90` LoadRef/StoreRef "(future)" — LoadRef shipped (8 sites); StoreRef defined+LIR-handled, not yet emitted.
- `tier1c-cluster1-burn-down.md` "Status: Active" — shipped 2026-05-11 (DONE.md:1207).
- `extern-modules.md:114,124` "Future: borrowed" — shipped.
- `stdlib-design.md` — Writer/Reader as `equip` default methods (shipped as free fns, `io.gg:428`); Set/Dict drain "not shipped" (shipped, `iter.gg:972,993`).
- `self-host-resource-model.md:328` "Phase C IN PROGRESS 2026-05-10" — shipped+fatal (`self_host_lowerer/validate.gg:288-301`).
- `unified-resource-model.md §8.3` validator-framework future-tense (shipped; its own line-999 footnote already says so).
- `unified-resource-model.md §3.6` RUNTIME_DECLS/resources.toml/build.rs — genuinely unshipped roadmap (correctly classified; route to TODO when §3.6 folds into Ch.18).

## F. Stale MEMORY.md / CLAUDE.md

- MEMORY: self-host lexer "green" (see A2).
- CLAUDE.md Project Structure lists "provenance inference" as a `src/semantic/` pass — no such pass/file exists.

---

*Full machine-readable findings (with per-chapter attribution) were in the workflow result; this is the curated, source-corroborated catalog. Items in B/C are low-risk source-comment cleanups; D/E are doc cleanups (several gated on implement-vs-cleanup decisions); A1/A2 are real gaps.*
