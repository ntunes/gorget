# Wave A3 brief — D10(a): local `&`-bind REJECTION (the whole form class)

> **Batch A, track 3** (ratified wave plan). **Zone (pass-1 completed the list):**
> `src/parser/stmt.rs` (decl-sigil parse error) + `src/semantic/` (the bind
> rejection) + `src/errors.rs` (the two new error kinds) +
> `tests/fixtures/self_host_lowerer/lower_stmt.gg` (the T-D intercept deletion) +
> `self_host_typechecker/{typecheck,diagnostic}.gg` (the self-host check) +
> `tests/security.rs` (attack_04 flip) + fixture flips + docs. ⚠ A1/A2-R touch
> the same semantic files with disjoint hunks — parent integrates sequentially.
> **Scout:** `/tmp/scout_wA3_report.md`; prototype DURABLE at
> `docs/plans/define-gorget/scouts/patches/scout_wA3_prototype.patch` (19 files,
> +298/−116; /tmp copy may rot), measured end-to-end incl. bootstrap fixed-point.
> **Status:** v2 — pass-1 reviewed (1 BLOCKING fold: the expr-position class-hole
> — `do:`-tail binds alias-and-write-through, match-expr binds garbage-link;
> `expr_is_borrow_bind` recursed `Expr::If` only — now extends to
> `Expr::Match`/`Expr::Do`/`Expr::Block` + 2 negative fixtures + re-sweep; plus
> the "bind reach dead" claim corrected, framing softened, zone completed,
> durable patch staged; D5/D6 discoveries filed). Awaiting pass 2.

## Verified premises — TWO CORRECTIONS over the wave plan's text

1. **The round-38 T-D intercept is in the SELF-HOST lowerer**
   (`tests/fixtures/self_host_lowerer/lower_stmt.gg:139-216`), per DONE.md's own
   T-D entry — NOT in the Rust lowering (pass-1: this DISAMBIGUATES the wave
   plan's compiler-unspecified wording; the census already said self-host).
   **The Rust side has NO bind-specific arm**: its behavior rides the SHARED
   `Expr::MutableBorrow` arm (`src/ir/lowering/exprs/mod.rs:338-424` — incl. the
   snag-#26 `&*box` path), serving every expression position — **it must NOT be
   deleted**. ⚠ Pass-1 CORRECTED the reach claim: the rejection makes the Rust
   arm's bind reach dead ONLY once the expr-position class-hole (premise 3
   below) is closed — `do:`-tails and match-arm initializers still reached it.
2. **Census +1**: `tests/fixtures/security/attack_04_cow_mutate_ref_borrow.gg:6`
   carries the decl-sigil form (subdirectory the census missed) — it passed as
   `security_safe` only because the parser SILENTLY SWALLOWS `&`/`!`/`move`
   decl-sigils (`src/parser/stmt.rs:678-688` — the sigil never reaches the AST).
   Flips to `security_rejected`, matching the fixture's own "must reject" comment.

## The form class (Core #4 — all 12 shapes probed and rejected in the prototype)

`error[E_LocalBorrowBind]` at: bare `auto r = &b` · projected `&b.field` ·
element `&v[i]` · scalar-field · parenthesized `(&a)` · typed `T r = &a` ·
assignment-RHS · if-expr-branch · module-static · in-closure. Plus a DEDICATED
PARSE ERROR for the decl-sigil forms (`T &r = a`, `T &r = &a`) — the parser
stops swallowing `&` there. **Bonus kills (reference-grade, both pre-existing):**
the typed form was an ICE (Tier-2a validator panic, `ir/lowering/mod.rs:2105`);
the element form was a SILENT WRONG-COPY WRITE — both now clean errors.
**Stays accepted (probed):** call args `f(&x)`, `!`-move binds, `match &x`,
`for x in &a`, ctor/literal `&` uses.

**3. THE EXPR-POSITION CLASS-HOLE (pass-1 BLOCKING fold — Core #4, in-track):**
`expr_is_borrow_bind` recursed `Expr::If` only. Pass-1 measured: `auto r = do: &a`
is ACCEPTED and ALIASES (writes through — prints `4/4`); `auto r = match n:
case 1: &a else: &b` typechecks then dies at link with a garbage
monomorphization error. Both pre-existing (byte-identical pristine A/B), but the
if-expr arm's own justification applies verbatim. EXTEND the helper with
`Expr::Match` (recurse `arms[].body` — `MatchArm.body: Spanned<Expr>`,
`ast.rs:833` — plus `else_arm`) and `Expr::Do`/`Expr::Block` (recurse the tail
expr-statement); ADD 2 negative fixtures (`amp_bind_matchexpr_error`,
`amp_bind_doexpr_error`); RE-RUN the zero-collateral sweep after. The self-host
`check_local_borrow_bind` dodges identically — same permissive-residual class as
its static/if-expr forms (note, don't chase; A2-S-era work).

## Scout-measured gates (executor re-runs; FOREGROUND, chunked)

build · lib 1105/0 · lints 53/0 · integration `cow_amp` 10/0, `amp_bind` 7/0,
`borrow` 45/0, `amp` 30/0, `cow` 91/0+1-ign (self-host driver rebuilds inside —
600s env) · security `sec_0*` 9/0. **Zero-collateral proof (re-run it):** a
patched `gg check` sweep over fixtures+security+lib+spec+smith (1520+165+66
files) — new rejections must be EXACTLY the 7 intended fixtures + attack_04
(attack_07/09 were already parse-rejected; D10's message is additive there).
**Bootstrap fixed-point GREEN — ⚠ TWO EXECUTOR TRAPS (keep):**
1. CHUNK the bootstrap per stage (~150-170s each) — the single-test form dies at
   the 600s cap (the scout hit this).
2. stage1≠stage2 by 12 lines (phantom `__gg_R`/`__gg_W` structs from
   `lib/std/io.gg` generic free fns) is PRE-EXISTING and test-tolerated
   (`integration.rs:17338` compares i≥1) — identical on the pristine baseline;
   NOT a regression. Do not chase it.

Self-host lane: the 4 main forms reject empirically (fresh driver, exit 1 +
rendered diagnostic; call-arg control exit 0); static + if-expr forms are
self-host-PERMISSIVE residuals (its checker has 42/47 diagnostic classes
unmigrated — the Rust gate covers the fixtures; note the residual, don't chase).

**Parity expectation at integration (parent):** the flipped fixtures become
RustRejected → runtime_diff MATCH −2 AND denominator −2; floors are min-counts,
safe — but regenerate on the pruned tree per the standard protocol.

## Docs

Reference borrow-section note: local `&`-binds are illegal (exclusivity — one
writer; mutation flows through places or `&`-args); frame-scoped `&` params
unchanged. Cite D10's ledger text.

## Executor protocol

Standard multi-agent rules in full. Apply `/tmp/scout_wA3_prototype.patch`,
re-derive hunk by hunk (you own it — especially: verify the Rust
`Expr::MutableBorrow` arm is UNTOUCHED and only its bind-position reach is dead).
Explicit-file staging. Commit: `feat(semantic): A3/D10a — reject local &-binds
(12-shape class + decl-sigil parse error) + retire the self-host T-D intercept`
+ standard trailers. Parent: full both-backend sweep + parity regen at
integration.
