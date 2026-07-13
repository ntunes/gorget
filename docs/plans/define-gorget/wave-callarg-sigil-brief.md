# Wave brief — call-arg-sigil preservation (self-host) + A2-S pos-2/3 re-enable

> **Track #14** (owner-sequenced 2026-07-12: "pause Batch B, do the sigil fix
> first"). Preserves the per-arg ownership sigil the self-host parser discards,
> then RE-ENABLES A2-S D12 positions 2/3 (disabled in `5ea1c92b`), re-completing
> A2-S to 6/6 and unblocking Batch B B2.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-callarg-sigil.md` (shape (a)
> PROVEN clean end-to-end + PROVEN to fix the bug — prototype patches
> `scouts/patches/callarg-sigil-shapeA.patch` + `callarg-sigil-reenable-proof.patch`).
>
> **Status:** v1 — pass-1 (Opus, fresh) folded. Pass-1 CONFIRMED the gap + all 6
> parser/re-enable claims against source (the 3 real parser files, the
> lowerer-ignores-sigil de-risk, the named-arg no-double-wrap, the exact-inverse
> re-enable, the accept-guard shape) — and raised **reservations, ALL FOLDED:**
> **(R1)** the brief was uncommitted so the reviewer's worktree couldn't see it
> (now committed). **(R2)** DROP the `mutable`-prefix arm — Rust's
> `parse_ownership_modifier` accepts ONLY `&`/`!` (not `move`/`mutable`); adding it
> = a Core-#8 divergence (self-host accepts what Rust rejects); the minimal patch
> already makes `f(mutable x)` a parse error matching Rust, and NO `f(mutable x)`
> lock fixture. **(R3)** remove ONLY the 2 `parse_call_args` skip calls per file
> (6 sites) — `skip_ownership_markers` STAYS live for its type-context callers
> (`parse_function_type` :1473/1477, `parse_type_args`). **(R4)** parser/resolver
> comparison counts should be UNCHANGED-OR-IMPROVED (format round-trip fix), the
> other three IDENTICAL — count-diff all 5 with `--nocapture`. **(R5)**
> `is_collection_receiver` is :758 (was :766); the accept guard is a forward
> regression guard (teeth only in the final combined state).
>
> **v2 — pass-2 (Opus, fresh) folded.** Pass-2 verified ALL citations accurate
> against source (the 6 exact skip-sites + the surviving type-context callers; the
> re-enable = exact inverse of `5ea1c92b`; the accept-guard need; and CONFIRMED the
> `move`-keyword note is sound — Rust rejects bare `move x` `expr.rs:585-593`, the
> self-host divergence is pre-existing, file-not-chase) and raised **1 BLOCKING
> reservation, FOLDED:** the R2 `mutable`-arm drop had left STALE remnants in §0
> (:35), §5 gate 7 (:176), and §7 DoD (:194/:196) that still COMMANDED the arm —
> contradicting §3 and, via the DoD, would have driven the executor to ship the exact
> Core-#8 divergence R2 removed. All four scrubbed: §0 lists only `!`/`&`; gate 7 is
> now a `f(mutable x)` PARSE-ERROR negative; the DoD requires `mutable`→parse-error,
> no arm.
>
> **v3-FINAL — pass-3 (Opus, fresh, confirming): SIGN OFF.** Pass-3 confirmed the
> `mutable` scrub is COMPLETE (all 18 mentions legitimate — gap/Rust-ref/correction/
> negative-fixture, none command the arm), the `!`/`&`/`move`/`mutable` treatment is
> uniform end-to-end, and re-verified the 6 skip-sites + surviving type-context
> callers + the exact-inverse re-enable + the Core-#5 count-diff gates. Two
> zero-impact polish nits folded (the §0 "reuse the prefix arm" phrasing; the
> format.gg parser/resolver copy line is :375/:377). **The gauntlet is CLEAN — brief
> is EXECUTOR-READY.**

---

## 0. Decided: SHAPE (a), wrapper-based (not the `CallArg.ownership` field)

The scout measured both shapes. **Shape (a) — wrap the arg** (`!arg` → `EMove`,
`&arg` → `EMutableBorrow`, matching Rust's only two call-arg sigils — see §3 on the
`move`/`mutable` keywords) is the decision:
- PROVEN clean end-to-end (§ scout: `lowerer_comparison` byte-identical over 1524
  fixtures, `self_host_bootstrap_fixed_point` passes 439s, `box_deref` ASan 6/6),
  despite rewrapping 9059 `&ident` + 171 `!ident` call-arg sites.
- PROVEN to fix the bug (prototype re-enable: `W(!a)`/`v.push(!b)` ACCEPT, bare
  `W(a)`/`v.push(a)` REJECT).
- Minimal (delete 2 skip calls per file; REUSE the existing expression-context
  prefix parser already builds the wrappers), mirrors the language's own `!x`/`&x`
  parsing, and is Layering-rule-1 correct (ownership stops being dropped at parse).

Shape (b) (a typed `CallArg.ownership` field mirroring Rust gg) = a ~150-site AST
arity change for zero current benefit (the lowerer ignores the sigil — borrow_flags
+ liveness drive it; the D12 check reads place-ness via `expr_is_place`, which the
wrapper satisfies). It is DEFERRED as a possible future convergence, filed as a
non-blocking note — do NOT do it here.

## 1. Why (the gap + the measured lowering fact)

The self-host `parse_call_args` (`self_host_typechecker/parser.gg:2037`) calls
`skip_ownership_markers` (`:2069`) before each arg (`:2047`, `:2051`), which
ADVANCES PAST + DISCARDS all four spellings `!`/`&`/`move`/`mutable`. So `f(!x)` /
`coll.push(!x)` parse to bare `EIdentifier("x")` — the ownership invariant is dropped
at parse (rule-1 violation). This made A2-S's D12 pos-2 (ctor-arg) + pos-3
(collection-put) over-reject `!x` moves (they couldn't tell a move from a copy);
they were disabled in `5ea1c92b`. **Rust gg tracks `CallArg.ownership` and compiles
the same fixtures fine.**

**Critical de-risking fact (scout-measured):** the self-host lowerer decides
borrow-vs-copy from the callee's `borrow_flags` + liveness, NOT the sigil
(`lower_expr.gg:8626`, `:8590`; the `EMove` arm `:5604` is transparent). So
`push(!x)` and `push(x)` lower IDENTICALLY — wrapping changes ZERO lowering. The
sigil matters ONLY to the typechecker's D12 check. (The prototype's byte-identical
`lowerer_comparison` confirms it.) **This is why shape (a) is safe** — the nightmare
case (a call-arg change that passes the bootstrap but miscompiles) was hunted
directly and found absent.

## 2. Topology (3 real `parser.gg` files — the fix touches all 3)

- `self_host_typechecker/parser.gg` = REAL (canonical); `self_host_check/parser.gg`
  + `self_host_lowerer/parser.gg` are SYMLINKS to it — one edit covers all three
  stages (incl. the LOWERER, so the bootstrap sees it — the measured, cleared risk).
- `self_host_parser/parser.gg` + `self_host_resolver/parser.gg` are INDEPENDENT real
  copies — they need the same edit (they ALSO strip the sigil in FORMAT round-trip
  today, `f(!x)`→`f(x)`, a latent fidelity bug the same fix closes).
- The D12 disabled positions live ONLY in `self_host_typechecker/typecheck.gg`
  (:1103 pos-2 ECall arm, :1113 pos-3 EMethodCall arm — the `TEMPORARILY DISABLED
  (2026-07-12)` comments).

## 3. Milestone 1 — the parser fix (shape a)

In `parse_call_args` of the **3 real parser files**, REMOVE EXACTLY the two
`skip_ownership_markers()` calls that sit in `parse_call_args` (the first-arg site +
the per-comma site) — and NOTHING ELSE:
- `self_host_typechecker/parser.gg:2047` + `:2051` (symlink-covers check + lowerer)
- `self_host_parser/parser.gg:1943` + `:1947`
- `self_host_resolver/parser.gg:1900` + `:1904`

With them gone, `parse_call_arg` → `parse_expr` sees the leading sigil and the
EXISTING expression-context prefix parser builds the wrapper: `!x` → `EMove`
(`:2493/:2503`), `&x` → `EMutableBorrow` (`:2544/:2547`). **This matches Rust gg's
call-arg sigils EXACTLY** — Rust's `parse_ownership_modifier` (`src/parser/mod.rs`,
used by `parse_call_arg` at `expr.rs:1517/1576/1736`) accepts ONLY `&`→MutableBorrow
and `!`→Move (else Borrow); there is NO `move`/`mutable` keyword sigil.

**Do NOT add a `mutable`-prefix arm** (pass-1 CORRECTION — the scout's "add it for
completeness" was WRONG). `mutable` is a binding modifier, not a value-position sigil;
Rust rejects it at call args, ZERO corpus uses it, and the minimal patch already makes
`f(mutable x)` a parse error (matching Rust). Adding an arm would make the self-host
ACCEPT a program Rust REJECTS — a Core-#8 divergence — and do NOT author a
`f(mutable x)` "lock" fixture (it would bake in that divergence).

**`skip_ownership_markers` STAYS defined + live** (pass-1 precision hazard) — it has
OTHER callers in TYPE contexts that MUST NOT be touched: `parse_function_type`
(typechecker `:1473/:1477`, + parser/resolver equivalents) and `parse_type_args`
(parser/resolver). Remove ONLY the 6 `parse_call_args` sites above; a blind
grep-and-delete of every `skip_ownership_markers()` call would break function-type +
type-arg parsing.

**`move` keyword note:** the existing prefix parser DOES have a `move` arm
(`:2506/:2515` → `EMove`), so removing the skips exposes `f(move x)` → `EMove` at call
args while Rust's call-arg sigils are `&`/`!` only. `move` is unused as a positional
call-arg in the corpus; if `f(move x)` parses in the self-host but not Rust, that is a
PRE-EXISTING self-host prefix-parser behavior (NOT introduced by this fix — the skip
discarded `move` before, the prefix parser wraps it now) — verify against Rust at
execution and FILE it if divergent; do NOT expand this fix's scope to chase it.

**Named-arg values already preserve the sigil** (`f(k = !v)` routes through the
`IDENT =` branch `:2121` → `parse_expr("!v")` → `EMove`) — only POSITIONAL args were
broken; the removed skip was already a no-op for named args (the token is `k`, not a
sigil), so the fix wraps EXACTLY once (verify no double-wrap).

## 4. Milestone 2 — re-enable A2-S pos-2/3 (exact inverse of `5ea1c92b`)

Both gating helpers already exist (`is_ctor_callee` typecheck.gg:691;
`is_collection_ingest_method` :752 + `is_collection_receiver` :758). **Land M1
FIRST** — re-enabling without the parser fix re-opens the over-rejection.
1. **typecheck.gg ECall arm (pos-2)** — restore `bool call_is_ctor = is_ctor_callee(*callee, scopes, ctx)`
   + the `if call_is_ctor: reject_tainted_place(a, …)` inside the arg loop.
2. **typecheck.gg EMethodCall arm (pos-3)** — restore `bool ingest = is_collection_ingest_method(method_name) and is_collection_receiver(*receiver, …)`
   + the `if ingest: reject_tainted_place(a, …)` inside the arg loop. (Keep
   `reject_amp_self_mutator` where it is — it was untouched by the disable.)
3. **lints.rs `self_host_d12_reject_hook_count`** (`:902`): EXPECTED **7 → 9** (and
   restore the "8 consuming-position calls" wording).
4. **integration.rs `self_host_driver_rejects_d12_drop_purity`** (`:18457`): restore
   the 3 dropped entries — `pos2_ctor_init_reject`, `pos3_collection_put_reject`,
   `pos3_field_place_reject` (still on disk; Rust gg already asserts them at
   `:5699/:5704/:5709`). Remove the `TEMPORARILY OMITTED` comment block.
5. **NEW over-rejection ACCEPT guard** (the hole that let the bug through): author a
   `d12_drop_purity/` fixture with a `W(!x)` ctor-move AND a `coll.push(!x)`
   collection-move (both must ACCEPT), wired into `self_host_driver_accepts_d12_legal`
   (`:18533`). The scout's `accept_move_arg.gg` is a ready model. (Existing
   `legal_explicit_move` only covers a pos-1 BIND `!a`, NOT a ctor/collection MOVE
   arg — the exact untested case.) This guard is NON-NEGOTIABLE: it is the
   executable ratchet that stops a future sigil-drop from silently re-breaking pos-2/3.

## 5. Gates (bootstrap-gated + count-diffed + ASan — CHUNKED-FOREGROUND)

Run FOREGROUND, chunked (rule-9). The scout measured `lowerer_comparison` +
bootstrap + `box_deref` ASan directly; **regenerate ALL count-diffs at execution**
(re-verify-a-premise):
1. `cargo build` + `cargo test --lib`.
2. Self-host + parser + resolver driver builds (chunked, ~2.5min each).
3. **COUNT-DIFF (baseline UNCHANGED tree → after), `--test-threads=1 --nocapture`,
   always-pass diagnostics so ONLY the counts matter** (green pass/fail is NOT the
   signal — Core #5):
   - `lowerer_comparison`, `type_comparison`, `check_comparison` — counts MUST be
     IDENTICAL before/after (the scout measured `lowerer_comparison` byte-identical;
     confirm the other two).
   - `parser_comparison`, `resolver_comparison` — these frontends were NOT exercised
     by the scout, AND the fix CHANGES their format round-trip (`f(!x)` now formats
     back to `f(!x)` via `format.gg:275`, not `f(x)`), so their counts should be
     UNCHANGED-OR-IMPROVED (fewer mismatches — the latent round-trip fidelity bug is
     closed). Capture the baseline, confirm NO new mismatch/crash (an improvement is
     expected + fine; a REGRESSION = STOP).
   Any new mismatch/crash in ANY lane = STOP.
4. **`self_host_bootstrap_fixed_point` GREEN** (chunked, ~150-170s/stage — the
   self-host compiles itself with every arg rewrapped + pos-2/3 live).
5. **D12 lanes:** `self_host_driver_rejects_d12_drop_purity` (now 16 rejects incl.
   pos-2/3 restored) + `self_host_driver_accepts_d12_legal` (incl. the NEW `!x`
   move-arg accept guard) + `self_host_d12_reject_hook_count` lint (== 9).
6. **`box_deref` self-host ASan gates** (the `&*box` reroute the wrap touches): 6/6.
7. A tiny NEGATIVE fixture asserting `f(mutable x)` is a PARSE ERROR (matching Rust —
   `mutable` is not a call-arg sigil; the minimal patch already produces this, but
   pin it so a future stray `mutable` arm can't silently diverge from Rust).
8. Full integration sweep is the PARENT's job (`GG_BUILD_TIMEOUT_SECS=600
   GG_TEST_TIMEOUT_SECS=120`, quiet box — do NOT run review agents concurrently).

## 6. Worktree + playbook preamble (non-negotiable)

Standard preamble (verify `pwd` + `git rev-parse --show-toplevel` inside the
worktree; NEVER touch `/workspace/gorget` or `/workspace/gorget-1`; no
`/workspace/gorget/...` absolute paths). `isolation: "worktree"`, `model: "opus"`;
`git merge --ff-only gorget-1` on entry; stage EXPLICITLY by file name (never
`git add -a`/`.`/`commit -a`); NEVER `git stash`; checkpoint the durable patch to
`scouts/patches/callarg-sigil-final.patch` after M1 and after M2; run FINAL gates
FOREGROUND with generous timeouts. On an Edit-tool desync, re-Read + retry.

## 7. Definition of done

- [ ] The 3 real `parser.gg` files preserve the call-arg sigils Rust tracks —
      `!`→EMove, `&`→EMutableBorrow (via the existing prefix parser); `mutable` at a
      call arg is a PARSE ERROR (matching Rust); NO `mutable`-prefix arm added.
      `skip_ownership_markers`
      dead-code removed or left per its other callers.
- [ ] Named-arg values not double-wrapped; the `f(mutable x)` PARSE-ERROR negative
      fixture passes.
- [ ] A2-S pos-2/3 re-enabled; arm-count lint == 9; the 3 reject fixtures restored;
      the NEW `W(!x)`/`push(!x)` ACCEPT guard added + green.
- [ ] D12 lanes: `W(!a)`/`v.push(!b)` ACCEPT, bare `W(a)`/`v.push(a)` REJECT.
- [ ] **All 5 `*_comparison` count-diffs IDENTICAL** before/after (behavior-preserving)
      + **`self_host_bootstrap_fixed_point` GREEN** + `box_deref` ASan 6/6.
- [ ] Latent format round-trip bug (`f(!x)`→`f(x)`) closed in the parser/resolver
      copies (verified via `parser_comparison`/`resolver_comparison` count-diff).
- [ ] A2-S back to 6/6; the DONE/HANDOVER "4/6" corrected to "6/6 — sigil fix landed".
- [ ] Batch B B2 unblock noted (the per-arg sigil is now in the AST for the
      place-overlap mirror to consume).

## 8. Non-goals

- **No `CallArg.ownership` typed field** (shape b) — wrapper-based is decided;
  file the convergence as a future note only.
- **No Batch B work** (still paused per owner; this fix unblocks B2 for later).
- Any NEW compiler gap hit → fixture + sharp TODO, never a reshape to dodge it.
