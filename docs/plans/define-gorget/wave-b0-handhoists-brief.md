# Wave B0 brief — D10(b) in-repo hand-hoists (the behavior-preserving prerequisite)

> **Track B0** (first of Batch B's three slices: **B0 hand-hoists** → B1 Rust
> place-overlap check → B2 self-host mirror). B0 lands BEFORE the checks: it
> refactors the in-repo call sites that would otherwise trip the D10(b)
> place-overlap rejection, so that when B1/B2 land the check the bootstrap +
> `p2p_basic` stay green. **These are REAL refactors, not mechanical hoists** —
> D10(a) forbids `&`-binding a field into a local, so you cannot "hoist" the
> overlap away; you restructure the call.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-batch-b.md` (§3.3 is B0's spec;
> §1 the measured premises). **Owner ruling:** `decisions.md` LOG 2026-07-12 "D10(b)
> ADDENDUM" (the live-alias rule + the Copy-read exemption + movers rider).
>
> **Status:** v1 — pass-1 (Opus, fresh) folded. Pass-1 CONFIRMED all 5 load-bearing
> claims (the p2p double-writer is real + the selector fix behavior-preserving; the
> self-host families mutate only disjoint fields; the within-function-borrow risk is
> real + the fallback D10(a)-legal; the Copy-exempt + disjoint-sibling carve-outs
> correct) and the design as aligned with the D10(b) ADDENDUM — and raised **4
> brief-accuracy reservations, ALL FOLDED:** **(R1)** the trait-family call sites were
> mis-cited (`typecheck.gg:1133` is an `ERange` arm; real `resolve_method_full` calls
> = `infer.gg:276` + `typecheck.gg:779` + `:1656`; `infer.gg:303/:382` are the sibling
> calls) → **5 sites**, corrected §1b. **(R2)** the "8 call sites" undercounted —
> `resolved_to_gir_type` has 6 RECURSIVE calls too (`lower_types.gg:431/442/446/451/463/477`)
> → **~15 total**, corrected §1b + DoD. **(R3)** symlink structure stated (edit the
> real `self_host_typechecker/` path). **(R4)** gate missing the `type_comparison` /
> `check_comparison` / `lowerer_comparison` differential guards (they lack the
> `self_host` substring) → added §3 step 7.
>
> **v2 — pass-2 (Opus, fresh, read-only) folded.** Pass-2 independently RE-DERIVED
> and CONFIRMED R1/R2/R3 (all call-site anchors + the 5+10 counts + the symlink
> structure — whole-tree greps show no missing caller), the behavior-preservation
> premise, and the missed-family scan (CLEAN — only the p2p ×2 + trait ×5 + Copy-int
> reads; no missed non-Copy overlap). It raised **1 MATERIAL fold-introduced
> reservation, FOLDED:** the R4 gate I added wired the `*_comparison` tests as
> pass/fail, but they are ALWAYS-PASS diagnostics (`integration.rs:16557`) whose green
> status proves nothing AND whose count signal is suppressed without `--nocapture`
> (CLAUDE.md's exact warning). §3 step 7 + the DoD now BASELINE-capture the counts
> (`--nocapture`) and require them UNCHANGED before/after (the sensitive
> behavior-preservation guard the bootstrap fixed-point can miss).
>
> **v3 — pass-3 (Opus, fresh, confirming) folded.** Pass-3 independently re-derived
> ALL anchors (whole-tree greps confirm exactly 10 + 5 callers, no missing site; v2
> `*_comparison` always-pass fold correct + complete; symlink note, Copy-exempt +
> disjoint-sibling carve-outs, within-function-borrow fallback all verified against
> current source) and raised **1 stale-remnant reservation, FOLDED:** R2's recount
> left a stale "8 call sites" in the actionable Fix sentence (`:148`) contradicting
> the corrected ~15 in the header/families/DoD — now "~15". Optional polish folded:
> `lowerer_comparison`'s actual labels (`Matched/Error-only/Real mismatches/Crashes`)
> noted alongside the count-diff instruction. Awaiting pass 4 (fresh, confirming this
> final fold).

---

## 0. Why B0 exists + the standing rulings it rests on

D10(b) (ratified) rejects a call passing two args whose places overlap under
conflicting sigils (writer `&` / mover `!`). Landing that check will REJECT several
in-repo call sites that are field-disjoint-SAFE-but-coarsely-flagged (a function
reads a struct's sub-table while a `&whole` arg mutates *other* fields) AND two that
are genuine bugs (p2p double-writers). B0 refactors all of them FIRST, behavior-
preserving, so the check lands clean.

**Owner rulings that scope B0** (from the D10(b) ADDENDUM — do not re-litigate):
- **Copy-read exemption / live-alias rule:** the place-overlap check ranges over
  LIVE ALIASES only (`&` writers, `!`/`^` movers, **non-Copy** bare reads); a
  bare read of a **Copy-typed** place is a value snapshot and participates in NO
  overlap. **Consequence for B0:** the `add_local(&ctx, ctx.expected_type)` /
  `channel_write_data(&self, self.channel_id, …)` sites are Copy-int reads under a
  writer → EXEMPT → **B0 does NOT touch them.** (The TODO's "self-host 8 = the
  add_local family" is a MIS-CITE the scout corrected — those are the exempt
  outliers; the real refactor targets are the `tc_types`/`trait_registry` families.)
- B0 is **behavior-preserving**: it lands on the UNCHANGED checker; its gate is that
  the bootstrap fixed-point + `p2p_basic` stay green (the refactors change no
  observable behavior).

---

## 1. The six refactor sites (scout-measured — reviewers re-verify each anchor)

### 1a. Lib — 2 p2p double-writers (a REAL bug FIX, not a hoist)

`p2p_poll_socket(Node &node, UdpSocket &sock)` (def `lib/xtd/p2p.gg:1776`) is called:
- `p2p_poll_socket(&node, &node.disc_socket)` (`p2p.gg:2057`)
- `p2p_poll_socket(&node, &node.socket)` (`p2p.gg:2067`)

`sock` is ALWAYS a field of `node`. The body BOTH `sock.recvfrom(…)`-mutates the
socket AND `p2p_update_peer(&node,…)` / `p2p_send_raw(&node,…)`-mutates node (which
CONTAINS the socket) → **two live mutable borrows of overlapping places** — a real
double-writer. You CANNOT hoist a `&`-borrow of a field into a local (D10(a)).

**Fix (recommended): drop the `sock` param; pass a socket SELECTOR.** Add a
`bool use_disc` (or a small 2-variant enum if clearer) param; inside the body do
`node.disc_socket.recvfrom(…)` vs `node.socket.recvfrom(…)` on the selected field.
The `recvfrom` then reads `node.<socket>` and the later `&node` mutations are
field-disjoint *within one function* (sequential statements — gg permits it). Update
both call sites to pass the selector instead of the `&node.<socket>` arg.
- **Alt** (if the selector muddies the body): split into a call-site `recvfrom` +
  `p2p_handle_packet(&node, pkt)` — do the socket read at the call site (producing
  `pkt`), then pass only `&node` + `pkt` into the handler.
- **Gate:** `p2p_basic` must stay green (the prototype confirmed both sites errored
  under the check; the fix must both remove the overlap AND preserve behavior).

### 1b. Self-host — 4 function refactors / ~15 call sites (the `tc_types` / `trait_registry` families)

**⚠ Symlink structure (pass-1):** `traits.gg` / `infer.gg` / `typecheck.gg` (and the
other shared frontend files) in `self_host_check/` and `self_host_lowerer/` are
SYMLINKS to the REAL files in `self_host_typechecker/` — edit the
`self_host_typechecker/*.gg` path and all three stages update; `git add` the real
path, not a symlink. `lower_types.gg` / `lower_generics.gg` are real files ONLY in
`self_host_lowerer/`.


Both are the SAME structural shape — *a function reads a struct's sub-table while
registering into OTHER fields of the same struct*, spelled `f(struct.subtable, &struct)`.
The `&struct` genuinely mutates the struct, but only fields DISJOINT from the passed
sub-table — so the code is field-disjoint-SAFE; root+projection keying flags it only
because `&struct` (whole) coarsely overlaps `struct.subtable` (sub).

**Family 1 — `resolved_to_gir_type`** (def `lower_types.gg:383`), signature
`resolved_to_gir_type(int rtid, TypeTable types, ScopeTable scopes, GirModule &gmod)`.
`&gmod` mutates only gmod's GIR fields (`lookup_or_register_named`, `register_ptr`,
`record_enum_category` …) — never `tc_types`/`tc_scopes`. **Call sites (pass-1
CORRECTION — the "8" undercounted; dropping `types`/`scopes` from the signature
forces updating the RECURSIVE/internal calls too, which the build will fail until you
fix):**
- **Entry calls** (pass `gmod.tc_types, gmod.tc_scopes`): `lower_types.gg:561, 568,
  595` + `lower_generics.gg:105`.
- **Recursive/internal calls** (pass the forwarded `types, scopes`):
  `lower_types.gg:431, 442, 446, 451, 463, 477`.
= **10 `resolved_to_gir_type` call sites** (+ the def). Every caller forwards
`types == gmod.tc_types` / `scopes == gmod.tc_scopes` (verified), so reading them
internally observes the same table — behavior-preserving. The recursive calls already
pass `&gmod` and re-borrow works (sequential eval), so after the refactor they simply
drop `types, scopes` and pass `&gmod` alone.

**Family 2 — `resolve_method_full`** (def `traits.gg:658`), signature takes
`(TraitRegistry registry, …, TypeTable &types)`, + two siblings of the SAME shape:
`resolve_method_for_generic_receiver` (def `traits.gg:608`) and
`substitute_shape_return_generic_receiver` (def `traits.gg:952`). Each reads
`types.trait_registry` and mutates other `types` fields via `&types` (in
`resolve_method_full` the sole `&types` mutation is `substitute_default_return(…,
&types)` at `traits.gg:696`, whose other args are locals not `types.*` projections —
so the refactor removes the overlap without relocating it). **Call sites (pass-1
CORRECTION — the brief's earlier `infer.gg:303, 382` / `typecheck.gg:1133` grouping
was wrong: `typecheck.gg:1133` is an `ERange` arm, not a trait call; `infer.gg:303`
and `:382` are the SIBLING calls, not `resolve_method_full`):**
- `resolve_method_full` (3): `infer.gg:276`, `typecheck.gg:779`, `typecheck.gg:1656`.
- `resolve_method_for_generic_receiver` (1): `infer.gg:303`.
- `substitute_shape_return_generic_receiver` (1): `infer.gg:382`.
= **5 trait-family call sites** (each drops its `types.trait_registry` / `registry`
arg and passes only `&types`).

**Fix (recommended): change each signature to take ONLY `&gmod` / `&types` and read
`gmod.tc_types` / `types.trait_registry` INTERNALLY** — drop the redundant sub-table
params (they were the struct's own fields all along). This is ALSO more idiomatic
("self-host as elegance showcase" — passing a struct AND its own field is a smell).
Update all ~15 call sites (10 `resolved_to_gir_type` + 5 trait-family) to drop the
sub-table args.

**⚠ THE BIGGEST RISK IN BATCH B (scout §3.3):** must confirm gg's within-function
borrow checker ACCEPTS reading `gmod.tc_types` in the same body that mutates `gmod`
via `&gmod` on a disjoint field. Likely yes (different statements, field-disjoint),
but VERIFY on the branch with a targeted build. **Fallback if it trips:** thread the
sub-tables as separate top-level bindings destructured BEFORE the mutating loop (a
local `TypeTable sub = gmod.tc_types` read once up front, then the `&gmod` mutations
after) — but note a bare `TypeTable sub = gmod.tc_types` is a COPY/borrow bind, not a
`&`-bind, so it's D10(a)-legal. Confirm the fallback preserves behavior (the read
must observe the pre-mutation sub-table, which it does since the sub-table is never
mutated by `&gmod`).

---

## 2. What B0 must NOT touch (regression guards)

- **Copy-exempt sites** (owner ruling — EXEMPT, no refactor): `add_local(&ctx,
  ctx.expected_type)` (`lower_expr.gg:5002`, int), `add_local_inheriting(&ctx,
  ctx.locals.get(…).type_id, …)` (`lower_match.gg:743, 1015, 1212` — Copy int, also
  a method-chain non-place), `channel_write_data(&self, self.channel_id, …)`
  (`ssh.gg:633`, int). Leaving these is CORRECT per the live-alias rule; touching
  them would add ugly hoists on provably-safe Copy reads (against elegance-showcase).
- **Disjoint-sibling sites** (must KEEP passing once the check lands — B0 doesn't
  touch them, but be aware): `drop_fn_for_type(&gmod.resource_types,
  &m.type_runtime_map, …)` (`lir_lower.gg:1296, 1352, 1355, 4639, 4751, 4872, 5040,
  5500, 5563` — ~30 arg-pairs), lib `ParseError.InvalidSyntax(p.pos, !p.err)`
  (`json.gg:515`, `toml.gg:1522`, `xml.gg:358`, `yaml.gg:1269, 1276`). These are
  disjoint siblings (`x.a` vs `x.b`) that projection-prefix keying correctly
  ACCEPTS — B1/B2 add a POS fixture pinning that. B0 leaves them alone.

---

## 3. Gates (B0 lands on the UNCHANGED checker — behavior-preserving proof)

B0 adds NO check — it only refactors. So the ONLY proof it's correct is that nothing
observable changed. Run FOREGROUND, CHUNKED (rule-9):
1. `cargo build`
2. `cargo test --lib` (sanity — unchanged)
3. **`p2p_basic`** (+ any other p2p fixtures): `cargo test --test integration p2p -- --test-threads=4 2>&1 | tee /tmp/b0-p2p-$RANDOM.log` — the p2p refactor must preserve behavior.
4. Self-host driver build (chunked-foreground, ~2.5 min).
5. **`self_host_bootstrap_fixed_point`** (chunked-foreground, ~150-170s/stage) — THE
   proof the self-host refactors are behavior-preserving (the self-host compiles
   itself identically after the signature changes): `cargo test --test integration self_host_bootstrap_fixed_point 2>&1 | tee /tmp/b0-boot-$RANDOM.log`.
6. `cargo test --test integration self_host -- --test-threads=4` (the self-host lanes — no regression).
7. **The direct differential guards for the refactored self-host files — COUNT-DIFFED, not pass/fail** (pass-2 CORRECTION — `type_comparison` / `check_comparison` / `lowerer_comparison` are ALWAYS-PASS diagnostics: `integration.rs:16557` "Diagnostic test — always passes." A green result proves NOTHING, and because they pass, `cargo test` SUPPRESSES their `eprintln` counts unless `--nocapture`. CLAUDE.md: "the `*_comparison` tests are diagnostic-always-pass, so only the freshly-printed counts mean anything."). **So: BASELINE-capture the counts on the UNCHANGED tree, then require them UNCHANGED after the refactor.** On a clean checkout (or before touching the files): `cargo test --test integration type_comparison check_comparison lowerer_comparison -- --test-threads=1 --nocapture 2>&1 | tee /tmp/b0-cmp-baseline.log`; record each test's printed counts (`type_comparison`/`check_comparison` print `exact / superset / mismatched / crashed`; `lowerer_comparison` prints `Matched / Error-only / Real mismatches / Crashes` — the count-diff is label-agnostic, just capture whatever each prints). Re-run the SAME command after the refactor → `/tmp/b0-cmp-after.log`; the counts MUST be identical (behavior-preserving). Any new mismatch/crash = a behavior change = a bug (STOP, do not ship). The green pass/fail status is NOT the signal — the printed counts are.
8. `cargo test --test spec_conformance -- --test-threads=4` (self-host acceptance floor unchanged).
Do NOT run the full 15-20 min integration sweep — that's the PARENT's job. Run the
targeted lanes above. **If the within-function-borrow risk (§1b) trips the build,
apply the §1b fallback; if the fallback also trips, STOP and report — do NOT reshape
around it silently.**

---

## 4. Worktree + playbook preamble (non-negotiable — CLAUDE.md "Multi-agent")

Open the executor prompt with the standard preamble (verify `pwd` +
`git rev-parse --show-toplevel` inside the worktree; NEVER touch `/workspace/gorget`
or `/workspace/gorget-1`; no `/workspace/gorget/...` absolute paths). Plus:
`isolation: "worktree"`, `model: "opus"`; `git merge --ff-only gorget-1` on entry;
stage EXPLICITLY by file name (never `git add -a`/`.`/`commit -a`); NEVER `git stash`
(save with `git diff > /tmp/b0_<name>.patch`); checkpoint durable patch to
`docs/plans/define-gorget/scouts/patches/b0-<name>.patch` after each family; run FINAL
gates FOREGROUND with generous timeouts. On an Edit-tool desync, re-Read + retry —
never a shell heredoc with an absolute path.

---

## 5. Definition of done

- [ ] `p2p_poll_socket` refactored to a socket-selector (or the call-site-recvfrom
      alt); both call sites (`p2p.gg:2057, 2067`) updated; the double-writer gone;
      `p2p_basic` green + behavior-preserved.
- [ ] `resolved_to_gir_type` + `resolve_method_full` (+ its 2 siblings) refactored to
      take only `&gmod` / `&types`; **ALL call sites updated — ~15 total: 10 for
      `resolved_to_gir_type` (4 entry + 6 recursive) + 5 trait-family** (the param-drop
      makes the build fail until every caller is fixed, so completeness is
      compiler-forced — but expect ~15, not 8); sub-table params dropped; edits made to
      the real `self_host_typechecker/` path (symlink note §1b).
- [ ] The within-function-borrow question RESOLVED (either the direct read works, or
      the §1b fallback landed) — stated explicitly in the report with which path.
- [ ] **`self_host_bootstrap_fixed_point` GREEN** + `p2p_basic` green + self-host
      lanes + spec_conformance floor unchanged (behavior-preserving proof).
- [ ] **`type_comparison` / `check_comparison` / `lowerer_comparison` COUNT-DIFF: the
      `exact/superset/mismatched/crashed` counts (captured `--nocapture`) are IDENTICAL
      before vs after the refactor** (§3 step 7). Green pass/fail is NOT the gate — the
      counts are; any new mismatch/crash is a behavior change and blocks. (The bootstrap
      fixed-point proves self-compilation is identical but can miss a typechecker-output
      change that still self-compiles — the count-diff is the sensitive guard.)
- [ ] Copy-exempt sites + disjoint-sibling sites UNTOUCHED (regression guards).
- [ ] No self-host defensive fossil introduced; the refactors read like idiomatic
      Gorget (dropping a struct's own field from a param list is a cleanup, not a dodge).
- [ ] B0 is a standalone behavior-preserving landing; B1 (Rust check) gates on it.

---

## 6. Non-goals (do NOT expand scope)

- **No place-overlap CHECK in B0** — that's B1 (Rust) + B2 (self-host). B0 only
  refactors the sites the check would trip.
- **No Copy-exempt-site hoists** (owner ruled exempt).
- **No no-op-`&` value-position work** (TODO:248 — a separate later rider, not Batch B).
- **No borrow-provenance bit** (owner ruled (B) — defer to the value-position family).
- Any NEW compiler gap the refactor hits → fixture + sharp TODO citing it, never a
  reshape to dodge it.
