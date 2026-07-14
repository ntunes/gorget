# Wave B2 brief — the self-host mirror of the D10(b) place-overlap check

> **Batch B / B2** (last Batch-B slice: B0 hoists ✅ → B1 Rust+ggdef check ✅ → self-root ✅ →
> **B2 self-host mirror, this brief**). Mirror the D10(b) place-overlap rejection into the
> self-host typechecker so the gg-in-gg compiler rejects the same programs Rust production +
> ggdef reject. Bootstrap-gated.
>
> **Scout:** `docs/plans/define-gorget/scouts/scout-b2.md` (PROVEN end-to-end: 6/6 place-overlap
> cases match Rust; name-keyed root handles self for free; Copy exemption via
> `is_scalar_primitive_name`; ZERO over-rejection of the self-host's own source). Prototype:
> `scouts/patches/b2-place-overlap-proto.patch`. **Ruling:** `decisions.md` — the 2026-07-12
> "D10(b) ADDENDUM", the "⚠ RIDER 1 REVISED 2026-07-14" note, AND the "⚠ B2 SCOPE + LIVENESS-PASS
> + PASS-ORDER" ruling 2026-07-14 (the mover-mover-arm-IN / mover-Copy-EXEMPT decision — READ IT).
>
> **Status:** v0 — awaiting ≥3 sequential fresh brief-reviews.

---

## 0. What B2 is + the owner ruling that scopes it

Mirror B1's `check_call_aliasing` into the self-host `check_carrier_ops_expr` walker. **Honor
the CallArg model (owner directive, firm): read `arg.ownership` (OWN_BORROW=0/OWN_MUTABLE=1/
OWN_MOVE=2), NEVER shape-match `arg.value`.** Copy-ness reads the TYPED axis. Bootstrap-gated:
the self-host must still compile itself (`self_host_bootstrap_fixed_point`).

**⚠ THE CRITICAL SCOPE RULING (`decisions.md` "B2 SCOPE + LIVENESS-PASS" 2026-07-14) — B2 mirrors
the FULL D10 RULE, not production's exact code:**
- **The mover-mover arm is IN.** `f(!x, !x)` (and `f(!x, !x.field)`) is the MAXIMAL place-overlap
  ("at most one writer OR mover" — D10's ratified text). Production's `check_call_aliasing`
  EXCLUDES `(Move,Move)` only because `E_DoubleMove` preempts it one pass earlier; the self-host
  has NO upstream move-tracker, so the faithful mirror lets the arm FIRE — rejecting with the
  OVERLAP code. Reject-with-a-different-code strictly beats ACCEPT. **So B2 does NOT copy B1's
  `skip (Move,Move)` — it lets the general conflict rule flag same-root overlapping movers.**
- **The mover-Copy-read case stays EXEMPT — B2 must NOT catch `f(!x, x.copy_field)`.** The Copy
  read is a SNAPSHOT, not an alias (Rider 1 REVISED); catching it via the overlap rule would
  re-smuggle the phantom-alias mistake. It ACCEPTS until the liveness track lands. **This falls
  out automatically:** the Copy bare reader is dropped before the conflict test, so after the
  drop only `!x` remains → no pair → no conflict → accept. Do NOT add special handling.
- **Interim divergences DOCUMENTED + FILED:** self-host rejects `f(!x,!x)` with the overlap code
  while production/ggdef use `E_DoubleMove`; self-host accepts `f(!x,x.copy_field)` while
  production/ggdef reject via `E_UseAfterMove`. Both are honest consequences of the self-host's
  missing liveness axis (filed HIGH — the liveness track). **Pass-order rider (ratified): liveness
  precedes aliasing** — when the liveness track lands, it preempts the mover-mover arm (matching
  production). So the mover-mover fixture is a self-host-targeted REJECTION test, kept OUT of the
  cross-compiler exact-code conformance lane (D11) until liveness lands (it would then flip to the
  E_DoubleMove code).

---

## 1. The walker hook (scout-verified — the B2-scout corrected a stale anchor)

`check_carrier_ops_expr` (`tests/fixtures/self_host_typechecker/typecheck.gg:1076`) — the
EXHAUSTIVE D12 walker (A2-S's home). Hook the two arms:
- `case ECall(callee, args, _targs)` (`:1103`) — call `check_call_aliasing(args, …)` AFTER the
  existing ctor-arg loop.
- `case EMethodCall(receiver, method_name, args, _targs)` (`:1117`) — same AFTER the ingest loop.
- **Args only** (receiver NOT aliasing-checked against args — mirrors Rust `check_expr.rs:206,371`
  + ggdef `mod.rs:1343,1714`).

**NOTE:** the older "typecheck.gg:859/863" anchor (from `scout-batch-b.md`) is STALE — those are
`collect_idents_expr`. The live walker is 1076/1103/1117 (scout-b2 §1).

Emit via `ctx.diagnostics.push(Diagnostic.error(span, DkBorrowConflict(), msg))` — the A2-S
`DkLocalBorrowBind` pattern (`typecheck.gg:490`). Add `DkBorrowConflict` to `diagnostic.gg`'s
`enum DiagKind` + a `diag_kind_str` arm ("borrow-conflict") — those two are the only touch-points
(`diag_kind_str` is the sole exhaustive `DiagKind` match).

---

## 2. The mirror (the scout's proven prototype + the ONE mover-mover adjustment)

The prototype (`scouts/patches/b2-place-overlap-proto.patch`) is the recommended design. Its pieces
(all in `typecheck.gg`):
- `struct ArgPlace { String root; Vector[String] path; int ownership; bool is_copy; Span span }`
  (one struct, not parallel vectors — rule 3).
- `place_projection_path(Expr) → Option[Vector[String]]` — the extractor: EIdentifier/ESelfExpr →
  empty path; EFieldAccess(obj, field) → path(obj)++[field]; EIndex → collapse to root; range
  index `x[a..b]` → None (a slice is a fresh value). (No ETupleFieldAccess / EOptionalChain exist
  in the self-host AST; `t.0` is EFieldAccess(obj,"0") → flows naturally.)
- `paths_overlap_vec(Vector[String], Vector[String]) → bool` — zip prefix test.
- `arg_place_is_copy(SpannedExpr, …) → bool` — the TYPED Copy test: `infer_expr_type(arg) →
  RTPrimitive(name) ∧ is_scalar_primitive_name(name)` (`traits.gg:732`). Unknown → non-Copy
  (conservative). Mirrors ggdef's `Ty::Prim`. Reads the resolved type, NOT the value shape.
- `render_place_sig(int own, String root, Vector[String] path) → String` — diagnostic text
  (`&place` / `!place` / bare).
- `check_call_aliasing(Vector[CallArg] args, ScopeTable, TypeTable&, ResolveContext&)`:
  1. Collect ArgPlaces: **root = `place_root_name(arg.value)`** (NAME-keyed — the self-host does
     NOT resolve `self`, so name-keying handles self-root for free AND matches ggdef's
     `root: String`; within one call one name = one binding, so name-keying ≡ DefId-keying for
     same-call overlap). **Local-root filter:** `rname == "self"` OR `place_root_def_spanned`
     resolves to a `DkVariable` (params are `DkVariable`) — mirrors Rust's Variable filter + ggdef.
     Skip non-places (None path). Compute `is_copy`, sigil (`arg.ownership`), projection path.
  2. Pairwise (i<j): same root ∧ `paths_overlap_vec` ∧ **drop Copy bare readers** ∧
     **(≥1 writer/mover) ∧ ¬both-bare** → push `DkBorrowConflict` at the 2nd arg's span.
  3. **⚠ THE ADJUSTMENT vs the scout's prototype:** the scout's proto (mirroring B1) SKIPS
     `(Move,Move)`. **DELETE that skip** — per the owner ruling the mover-mover arm is IN, so the
     general "(≥1 writer/mover) ∧ ¬both-bare" rule flags same-root overlapping movers. VERIFY:
     `f(!x,!x)` → REJECT (overlap), `f(!x, x.copy_field)` → ACCEPT (Copy reader dropped → only `!x`
     left → no pair). Both MEASURED (§4).

---

## 3. Fixtures (self-host-driver lane — mirror `self_host_driver_rejects_d12_drop_purity`)

Mirror `tests/integration.rs:18551` (rejects) / `:18627` (accepts) pattern. Promote the scout's
probes (`/tmp/b2_work/probes/`) to `tests/fixtures/d10b_place_overlap/`:
- **REJECT** (non-zero exit + "their places overlap" + empty stdout): `f(&n,&n)` (p_writer_writer),
  `f(&n,&n.data)` (p_writer_subfield), `f(n,!n)` non-Copy (p_read_move), `f(!n, n.data)` non-Copy
  (p_move_noncopyread), **`f(!n,!n)` (p_double_move — the mover-mover arm; a self-host-targeted
  REJECTION test, NOT in the cross-compiler exact-code conformance lane — see §0)**.
- **ACCEPT** (exit 0 + emits C): `f(&m.a,&m.b)` (disjoint siblings), `f(&s,s.tag)` Copy int
  (writer-Copy-exempt — the over-rejection guard).
- **The mover-Copy divergence** (`f(!s, s.tag)` — accepts self-host-side while Rust rejects via
  E_UseAfterMove): do NOT wire as a self-host reject fixture (it legitimately accepts until
  liveness lands); note it in the liveness follow-up. Optionally a self-host ACCEPT fixture with a
  comment citing the interim divergence + the filed liveness entry (so a future reader knows it's
  a KNOWN pre-existing gap, not a blessed accept).

---

## 4. Gates (bootstrap is the over-rejection gate — scout MEASURED it, executor RE-MEASURES)

Executor runs FOREGROUND, CHUNKED (`GG_BUILD_TIMEOUT_SECS=900`, bootstrap ~150-170s/stage):
1. `cargo build` (Rust harness) + `cargo test --lib`.
2. Self-host driver build (`gg build self_host_lowerer/driver.gg`) — clean, the mirror type-checks.
3. **The over-rejection gate — stage 0→1 over the whole self-host source:** the scout MEASURED exit
   0, ~37 MB C, **stderr EMPTY** (zero borrow-conflict firings). RE-MEASURE — if the mover-mover
   adjustment newly-fires on the self-host source (a real `f(!x,!x)` in the self-host?), TRIAGE:
   real double-move → the self-host source has a latent bug, fix it + note; false positive → bug in
   the mirror. Do NOT weaken.
4. **`self_host_bootstrap_fixed_point` GREEN** — THE gate (the check is additive + emits nothing on
   the self-host source, so stage1==stage2 must hold; the scout left this UNMEASURED — the executor
   MUST measure it, per "regenerate every number").
5. The driver reject/accept fixtures (§3) — each rejects/accepts as specified.
6. `cargo test --test integration self_host -- --test-threads=4` (self-host lanes unregressed).
- **PARENT drives** the FULL C + FULL LLVM sweeps (the mirror is self-host-only, so the Rust
  compiler is unchanged — the full sweep mainly confirms the new fixtures + no harness breakage).

---

## 5. Worktree + playbook preamble (CLAUDE.md "Multi-agent")

Standard preamble (verify `pwd`/`git rev-parse --show-toplevel` inside the worktree; NEVER touch
`/workspace/gorget` or `/workspace/gorget-1`; no `/workspace/gorget/...` absolute paths).
`isolation: "worktree"`, `model: "opus"`; worktree branches from current main (has B0/B1/self-root
+ the scout + this brief). **⚠ Symlink note:** `typecheck.gg` in `self_host_check/` and
`self_host_lowerer/` are SYMLINKS to `self_host_typechecker/typecheck.gg` — edit the real
`self_host_typechecker/` path, `git add` the real path. Stage EXPLICITLY by file name; NEVER
`git add -a`/`commit -a`/`git stash` (save with `git diff > /tmp/b2_<name>.patch`); checkpoint a
durable patch to `scouts/patches/b2-fix.patch`. Run FINAL gates FOREGROUND. On Edit-tool desync,
re-Read + retry — never a heredoc with an absolute path.

---

## 6. Definition of done

- [ ] `check_call_aliasing` mirror in `typecheck.gg` (ECall + EMethodCall arms, args only),
      reading `arg.ownership` (NO shape-match), name-keyed root (self handled), projection-path
      overlap, Copy exemption via `is_scalar_primitive_name` (typed axis).
- [ ] **The mover-mover arm IS reachable** — `f(!x,!x)` REJECTS with the overlap code (the scout's
      `skip (Move,Move)` DELETED); `f(!x, x.copy_field)` ACCEPTS (Copy reader dropped) — both
      verified. Interim divergences documented at the arm + cited to the filed liveness entry.
- [ ] `DkBorrowConflict` added to `diagnostic.gg` (enum + `diag_kind_str`).
- [ ] Fixtures: reject set (incl. the mover-mover self-host-targeted rejection test, OUT of the
      exact-code conformance lane) + accept set (disjoint siblings + writer-Copy-read guard).
- [ ] **`self_host_bootstrap_fixed_point` GREEN** (MEASURED, not reasoned) + stage 0→1 stderr EMPTY
      (zero over-rejection of the self-host's own source) + self-host lanes unregressed.
- [ ] FULL C + FULL LLVM sweeps GREEN (parent — confirms fixtures + no harness breakage).
- [ ] No shape-match / name-list (Copy axis is `is_scalar_primitive_name` typed; root is
      name-keyed per ggdef). Reads like idiomatic Gorget (elegance showcase — one ArgPlace struct,
      not parallel vectors). The interim divergences are FILED (liveness HIGH), not blessed.

---

## 7. Non-goals
- **No liveness pass** (its own HIGH track — B2 is the place-overlap axis only; but B2 DOES let the
  mover-mover arm fire per the ruling).
- **No mover-Copy-read catching** (exempt — liveness's job).
- **No receiver-vs-args aliasing** (args only, mirror Rust/ggdef).
- **No unified-safety-module refactor** (that's the liveness track's structural job; B2 adds the
  place-overlap arm to the existing walker; the liveness track later unifies drop-purity +
  place-overlap + liveness into one module).
- Any NEW gap → triage + file, never a reshape to dodge.
