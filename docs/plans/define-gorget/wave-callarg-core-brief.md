# Wave brief — CallArg{name, ownership, value} normalization (core: ECall/EMethodCall) + A2-S pos-2/3 re-enable

> **Track #14** (owner-ratified 2026-07-13: converge on Rust's typed `CallArg`).
> Converts the self-host `ECall`/`EMethodCall` argument model from
> `Vector[SpannedExpr] args` + a PARALLEL `Vector[String]` names-vector (with the
> `!`/`&` sigil DISCARDED) to a single `Vector[CallArg]` where `CallArg{name,
> ownership, value}` carries all per-arg metadata as TYPED FIELDS — value stays
> BARE. Then RE-ENABLES A2-S D12 pos-2 (ctor-call args) + pos-3 (collection-put) via
> `arg.ownership`, and unblocks Batch B B2.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-callarg-normalization.md` (PROVEN:
> proto applies, 0 semantic errors, sound repro `REPRO_OK_48`, value-bare → lowerer
> byte-unchanged). **Proto patch:** `scouts/patches/callarg-normalization-proto.patch`
> (16 files, 408/371). **The former "backend bug" blocker is RESOLVED** — it was the
> FieldAccess soundness hole (landed `f9a9da3d`); the proto's 7 `.value`-on-`Vector[CallArg]`
> accesses are now clean typecheck errors to fix (§3).
>
> **Status:** v1 — pass-1 (Opus, fresh) folded. Pass-1 applied the proto + the
> FieldAccess-fixed compiler, BUILT the driver twice, and CONFIRMED the two crux claims:
> value-stays-bare SAFETY (lower_call byte-unchanged, no wrapper → the driver builds
> 0-error + cc-clean after the 7 fixes; miscompile class cannot recur) + AST-transformer
> PRESERVATION (every meta/generics/lowering rebuild preserves name+ownership). Raised 3
> reservations, ALL FOLDED: **(1)** the 7 `.value` defects were MIS-LOCATED (all 7 in
> `lower_expr.gg`/`lower_expr_inner`: 3×`margs.value` + 3×`spawn_args.value` + 1×`sb_args.value`;
> `eval_meta_int_v2` has ZERO — the scout's stale C-function attribution) → §3.2 corrected.
> **(2)** my "disable EStructLiteral/EDotShorthand pos-2" was the WRONG direction
> (under-rejects `S{a:x}` → a self-host-accepts/Rust-rejects divergence) → §4 corrected to
> LEAVE the current unconditional reject UNTOUCHED (strictly safer, zero regression); the
> ownership-gating is purely the follow-up. **(3)** the 3 shared `*_comparison` gates:
> "no NEW divergences (count may improve)", not "identical". Awaiting pass 2 (fresh).

---

## 0. Scope (owner-decided)

**THIS landing = the PROVEN core (`ECall`/`EMethodCall`) + the A2-S pos-2/3 re-enable.**
FILED FOLLOW-UPS (NOT this track): (a) the two INDEPENDENT parser/resolver copies
`self_host_parser/` + `self_host_resolver/` (their own `ast.gg`/`parser.gg` — separate
drivers, for full sidecar-retirement consistency); (b) extend `CallArg` to
`EStructLiteral`/`EDotShorthand` (struct/enum-literal ctor positions — so `S{a:!x}` /
`.Red(!x)` moves are ownership-gated). Both toward TRUE 6/6 in a follow-up; here,
the struct/enum-literal pos-2 ownership-GATING stays staged (the unconditional reject
stays LIVE — §4).

## 1. The CallArg model (canonical `ast.gg`, symlinked into typechecker/check/lowerer)

```
struct CallArg:
    Option[String] name      # Some(kw) for `k = v`; None positional (retires the ""-sentinel)
    int ownership            # OWN_BORROW(0) / OWN_MUTABLE(1) / OWN_MOVE(2)
    SpannedExpr value        # BARE — no EMove/EMutableBorrow wrapper
```
- **Ownership convention** reuses `Param.ownership`'s (`parser.gg:177-179`):
  `&`→OWN_MUTABLE, `!`/`move`→OWN_MOVE, bare→OWN_BORROW.
- **AST arity change** (`ast.gg:62-63`): `ECall(Box[SpannedExpr], Vector[CallArg],
  Vector[SpannedType])` + `EMethodCall(Box[SpannedExpr], String, Vector[CallArg],
  Vector[SpannedType])` — the `Vector[String]` names field REMOVED (merged into CallArg).
- **Parser:** `parse_call_args` returns `Vector[CallArg]`; new `parse_arg_ownership()`
  (mirrors Rust `parse_ownership_modifier` `mod.rs:236` for the SIGILS — `&`→OWN_MUTABLE,
  `!`→OWN_MOVE, else OWN_BORROW; the `move`/`mutable` KEYWORD arms are inherited self-host
  behavior [from the old `skip_ownership_markers`], not a Rust mirror — behavior-preserving);
  RETIRE `Parser.last_arg_names` + `peek_arg_name` + every
  `call_names = self.last_arg_names` read. **KEEP `skip_ownership_markers`** (still used
  by function-type-param parsing / `parse_type` — do NOT delete it).
- **Design ground:** CLAUDE.md Layering rule 2 (typed-not-shape) + rule 3 (one source of
  truth); `decisions.md` LOG "SELF-HOST ARG MODEL" (RATIFIED); the Rust reference
  (`ast.rs:789` `CallArg{name, ownership, value}`, `expr.rs:1992`, the D12/D10(b) check
  reads `arg.node.ownership` at `helpers.rs:1129`).

## 2. Why this is SAFE (the value stays bare — no wrapper miscompile)

The reverted wrapper approach miscompiled because wrapping `&x`→`EMutableBorrow` made
the LOWERER's `EMutableBorrow` arm fire. CallArg keeps `value` a BARE expression:
- **`lower_call` signature + internals stay byte-identical** — it keeps taking
  `(Vector[SpannedExpr] args, Vector[String] arg_names)`; the ~4 call sites ADAPT via
  `callarg_values(args)` / `callarg_names(args)` (new `ast.gg` helpers). **The lowerer
  sees the identical bare `EIdentifier`/etc it saw before → the wrapper-miscompile class
  STRUCTURALLY CANNOT recur.** (Owner Q3 ruling: keep the boundary adapter; do NOT
  thread `CallArg` into `lower_call` — that's the follow-up "fuller shape".)
- **Standalone soundness proof:** `scouts/patches/callarg-backend-repro.gg` (the exact
  pattern) compiles to correct C + runs (`REPRO_OK_48`).

## 3. The work (apply the proto + fix the 7 defects + verify preservation)

1. **Apply `scouts/patches/callarg-normalization-proto.patch`** (16 files — canonical
   frontend [real in `self_host_typechecker/`] + lowerer-own files). If it doesn't apply
   cleanly (main drifted), re-derive from §1 (the AST+parser+consumer conversion). The
   compiler enumerates ALL sites in one build (atomic) — converge to 0 semantic errors.
2. **FIX the 7 `.value`-on-`Vector[CallArg]` proto defects** (now surfaced as
   `E_NoFieldFound` by the landed FieldAccess fix). **CORRECTED LOCATIONS (pass-1
   ground-truth build — the scout's "eval_meta_int_v2 ×4" was a stale C-function
   attribution; `eval_meta_int_v2` has ZERO bogus accesses):** all 7 are in
   `self_host_lowerer/lower_expr.gg` inside `lower_expr_inner` — 3× `margs.value`
   (`:3963/:3979/:3998`, Vector/Dict/Set HOF dispatch) + 3× `spawn_args.value`
   (`:5934/:5944/:5952`) + 1× `sb_args.value` (`:6005`, spawn arms). Each applies
   `.value` to a `Vector[CallArg]` (no such field) where it should build the values via
   `callarg_values(...)`. Fix each to `callarg_values(...)` (pass-1 VERIFIED this makes
   the driver build 0-error + `cc`-clean, 32MB driver.c + exe).
3. **⚠ VERIFY AST-TRANSFORMER PRESERVATION (the MEDIUM risk — reviewers hammer this):**
   every pass that REBUILDS an ECall/EMethodCall must PRESERVE `CallArg(name, ownership,
   value)` through the rewrite — a silent drop loses named-arg OR D12-ownership metadata.
   Sites: `meta.gg` (×6 subst/rename AST-transformer rebuilds), `lower_generics.gg`
   (`subst_mf` `:557/:1700/:1705`), `lower.gg` (`collect_rewrite` `:2881/:2984`),
   `lower_expr.gg` (`:2468` + the static/instance named-arg reorder blocks),
   `typecheck.gg` (×2 PositionalAfterNamed). Each must rebuild
   `CallArg(a.name, a.ownership, subst(a.value))` — verify NONE drops `name`/`ownership`.

## 4. A2-S pos-2/3 re-enable via `arg.ownership` (§4 of the scout)

The CLEAN replacement for the reverted wrapper's `expr_is_place`-skips-EMove trick — a
bare copy (`W(x)`) is rejected iff tainted; `W(!x)`/`W(&x)` are legal (ownership ≠
BORROW → skip), WITHOUT the lowerer ever seeing a wrapper:
- **pos-2 (ECall ctor arm, typecheck.gg ~:1103):** restore `bool call_is_ctor =
  is_ctor_callee(*callee, scopes, ctx)` (helper exists :691); `for a in args: if
  call_is_ctor and a.ownership == OWN_BORROW: reject_tainted_place(a.value, …)` (helper
  :673); then `check_carrier_ops_expr(a.value, …)`.
- **pos-3 (EMethodCall arm ~:1113):** restore `ingest = is_collection_ingest_method(…)
  and is_collection_receiver(…)`; `if ingest and a.ownership == OWN_BORROW:
  reject_tainted_place(a.value, …)`. Keep `reject_amp_self_mutator`.
- **lints.rs** `self_host_d12_reject_hook_count` 7→9 (`:896`).
- **integration.rs** restore the 3 reject fixtures (`pos2_ctor_init_reject`,
  `pos3_collection_put_reject`, `pos3_field_place_reject`) into
  `self_host_driver_rejects_d12_drop_purity`; ADD the `W(!x)` / `coll.push(!x)` ACCEPT
  guard into `self_host_driver_accepts_d12_legal` (the over-rejection hole that let the
  wrapper bug through — NON-NEGOTIABLE).
- **B2 unblock:** the D10(b) place-overlap mirror consumes `arg.ownership` DIRECTLY.

**⚠ EStructLiteral/EDotShorthand pos-2 — KEEP THE CURRENT UNCONDITIONAL REJECT UNCHANGED
(pass-1 CORRECTION — do NOT disable it).** These nodes keep `Vector[SpannedExpr]` and
their pos-2 arm (`typecheck.gg:1185/1191`) is CURRENTLY LIVE + green + unconditional. My
draft's "disable it" was the WRONG direction: disabling UNDER-rejects `S{a:x}` bare-tainted
→ a self-host-ACCEPTS / Rust-REJECTS D12 divergence (Rust enforces pos-2 on struct/enum-
literal init, `check_expr.rs:15-63`) — against the differential goal. The status-quo
unconditional reject is STRICTLY SAFER: it rejects bare-tainted (matches Rust) with ZERO
regression, and only over-rejects `S{a:!x}` (a move-into-literal that the executor should
confirm is even PARSEABLE in the self-host — the reviewer found no `EStructLiteral(`
construction site; if unparseable, there's no over-rejection at all). So the CallArg-core
landing LEAVES these two nodes UNTOUCHED; the ownership-gating (`S{a:!x}`/`.Red(!x)` →
accept) is PURELY the extension follow-up. No disable, no `#[ignore]` staging needed.

## 5. Gates — MANDATORY, FULL, the box QUIET (THE saga lesson: slice ≠ the gate)

Run FOREGROUND, CHUNKED. **NO slice-only validation** — the FULL sweep is the
over-rejection gate (Core #7; it caught both the pos-2/3 over-rejection AND `str.data`
where slices missed them). `self_host_runtime` is the correctness gate that caught the
wrapper miscompile — NEVER skip it.
1. `cargo build` + `cargo test --lib`.
2. Self-host driver build (chunked, ~2.5min) — MUST `cc` clean (the 7 defects fixed).
3. **`self_host_runtime` + `self_host_runtime_diff`** — THE correctness gate (byte-correct
   runtime output; would catch a value-model miscompile).
4. **`self_host_bootstrap_fixed_point`** (chunked) — the self-host compiles itself under
   the new arg model.
5. **ALL `*_comparison` count-diffs** (`--test-threads=1 --nocapture`, always-pass
   diagnostics — only counts matter): `lowerer_comparison`/`type_comparison`/
   `check_comparison` NO NEW divergences (count must NOT GROW — convergence on Rust's
   typed `.name` model may IMPROVE it; an improvement is fine, only a REGRESSION stops);
   `parser_comparison`/`resolver_comparison` STRICTLY UNCHANGED (the copies are NOT
   touched this landing). Any new divergence = STOP.
6. **FULL `cargo test --test integration -- --test-threads=4`** (`GG_BUILD_TIMEOUT_SECS=600
   GG_TEST_TIMEOUT_SECS=120`) — the whole suite, no over-rejection anywhere.
7. **`GG_BACKEND=llvm` FULL integration sweep** — the arg model feeds both backends.
8. `box_deref` self-host ASan + `cargo test --test lints`.
9. D12 lanes: `self_host_driver_rejects_d12_drop_purity` (pos-2/3 restored) +
   `self_host_driver_accepts_d12_legal` (incl. the new `!x` move-arg guard) +
   arm-count lint == 9.

## 6. Worktree + playbook preamble (non-negotiable)

Standard preamble (verify `pwd` + `git rev-parse --show-toplevel` inside the worktree;
NEVER touch `/workspace/gorget` or `/workspace/gorget-1`; no `/workspace/gorget/...`
absolute paths). `isolation: "worktree"`, `model: "opus"`; `git merge --ff-only gorget-1`
on entry; stage EXPLICITLY by file name (never `git add -a`/`.`/`commit -a`); **NEVER
`git stash`** (save with `git diff > /tmp/ca_<name>.patch`); checkpoint the durable patch
after the proto applies + after the re-enable; **run FINAL gates FOREGROUND with generous
timeouts and do NOT background-then-end** (the FieldAccess executor rule-9 stalled twice
this way — run the full sweep + LLVM in the FOREGROUND, chunked, and report only when
they've actually completed). On an Edit-tool desync, re-Read + retry.

## 7. Definition of done

- [ ] `CallArg{name, ownership, value}` in canonical `ast.gg`; `ECall`/`EMethodCall`
      carry `Vector[CallArg]`; the `Vector[String]` names field + `last_arg_names` +
      `peek_arg_name` RETIRED; `skip_ownership_markers` KEPT (type-param parsing).
- [ ] `parse_arg_ownership` (`&`/`!`→ownership per Rust; `move`/`mutable` inherited self-host); named-arg values
      preserved; the 7 `.value`-on-`Vector[CallArg]` defects fixed to `callarg_values(...)`.
- [ ] `lower_call` byte-unchanged (boundary adapter `callarg_values`/`callarg_names`);
      every AST-transformer rebuild PRESERVES `name`+`ownership` (§3.3 verified).
- [ ] A2-S pos-2 (ctor-call) + pos-3 re-enabled via `a.ownership == OWN_BORROW`; lint 9;
      3 reject fixtures restored + the `W(!x)`/`push(!x)` ACCEPT guard added; `W(!a)`/
      `v.push(!b)` ACCEPT, bare `W(a)`/`v.push(a)` REJECT on the self-host driver.
- [ ] EStructLiteral/EDotShorthand pos-2 arm LEFT UNTOUCHED (unconditional reject stays
      live + green — no disable, no under-rejection divergence); the ownership-gating is
      the filed extension follow-up.
- [ ] **`self_host_runtime`/`_diff` GREEN** + **`self_host_bootstrap_fixed_point` GREEN**
      + **FULL C sweep GREEN** + **FULL LLVM sweep GREEN** + the 5 `*_comparison` count-diffs (3 shared: no NEW divergence/may improve;
      2 copies: strictly unchanged) + `box_deref` ASan + lints. NO slice-only sign-off.
- [ ] Follow-ups filed: parser/resolver copies (sidecar consistency) + the
      EStructLiteral/EDotShorthand extension (true 6/6).

## 8. Non-goals

- **No parser/resolver COPY changes** (`self_host_parser`/`self_host_resolver`) — filed
  follow-up (their names sidecar stays this landing; their `*_comparison` must stay
  identical, confirming they're untouched).
- **No EStructLiteral/EDotShorthand CallArg** — staged (§4).
- **No threading CallArg into `lower_call`** (owner Q3 — keep the boundary adapter).
- Any NEW compiler gap the work hits → fixture + sharp TODO, never a reshape to dodge it.
