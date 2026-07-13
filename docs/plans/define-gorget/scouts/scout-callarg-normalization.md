# Scout: self-host `CallArg{name, ownership, value}` normalization

Status: COMPLETE. Read-only on main (worktree `agent-a1b8d0e67545ae497`, synced to
gorget-1). Prototype APPLIED + MEASURED end-to-end in the worktree (NOT committed;
main clean — `git -C /workspace/gorget status` empty). Durable artifacts:
- `patches/callarg-normalization-proto.patch` — the 16-file lowerer-driver
  prototype (408 ins / 371 del).
- `patches/callarg-backend-repro.gg` — the standalone repro proving the CallArg
  mechanism is sound (prints `REPRO_OK_48`).

## TL;DR

- **The CallArg design is mechanically TRACTABLE and the Gorget-level refactor is
  COMPLETE for the lowerer driver: 0 semantic errors** — the whole
  frontend+lowerer typechecks AND lowers to C under the new arg model. Blast
  radius fully measured (compiler-enumerated).
- **The design is SOUND:** a standalone repro of the exact CallArg pattern
  (struct + `Option[String]` + nested `.get(i).unwrap().value.expr` + a
  `callarg_values` helper) compiles to correct C and runs (`REPRO_OK_48`). The
  value stays BARE → the lowerer's `lower_call` internals are byte-unchanged.
- **BLOCKER (the correctness proof is BLOCKED, and it's the decisive finding):**
  the emitted `driver.c` fails `cc` with **7 `incompatible types when assigning
  to type 'GorgetArray' from type 'int32_t'` errors** in the two LARGEST
  self-host functions — `lower_expr___lower_expr_inner` (3) and
  `lower_generics___eval_meta_int_v2` (4). The Gorget typechecker accepts the
  code (0 semantic errors); the BACKEND emits mis-typed C. **This is a latent
  main-gg C-backend coalescing / value-type-tracking bug that the change's SSA
  perturbation trips — NOT a CallArg defect** (baseline builds clean with the
  same binary; the small repro is clean; `eval_meta_int_v2` breaks with ONLY
  `.value` accesses added, no new locals). **It must be root-caused/fixed in the
  main gg backend before this feature can land**, because `self_host_runtime`
  cannot be green while the driver won't `cc`.
- The A2-S pos-2/3 re-enable via `arg.ownership == OWN_BORROW` is clean and
  confirmed (§4). B2 consumes `arg.ownership` directly.

---

## Section 1 — Topology, the CallArg struct, the ownership convention

### Rust reference model (the shape we converge on)
- `src/parser/ast.rs:789` `struct CallArg { name: Option<Spanned<String>>,
  ownership: Ownership, value: Spanned<Expr> }`.
- `src/parser/ast.rs:523/530` `Expr::Call{callee, generic_args, args:
  Vec<Spanned<CallArg>>}` / `Expr::MethodCall{…, args: Vec<Spanned<CallArg>>}`.
  **No parallel names vector — name is a field.**
- `src/parser/ast.rs:180` `enum Ownership { Borrow, MutableBorrow, Move }`.
- `src/parser/expr.rs:1992` `parse_call_arg`: `parse_ownership_modifier()`
  (`src/parser/mod.rs:236`: `&`→MutableBorrow, `!`→Move, else Borrow) → peek
  `IDENT =` name → `parse_expr()` value → `CallArg{name, ownership, value}`.
- `src/semantic/safety/helpers.rs:1129` reads `arg.node.ownership` directly
  (typed, no shape-walk) — the exact D10(b)/D12 consumption B2 mirrors.

### Self-host topology (symlinks — confirmed)
- `self_host_typechecker/` holds the REAL canonical frontend (ast, parser,
  typecheck, infer, resolve, format, meta, derive, traits, types, scope, ids,
  diagnostic, lexer, format_types).
- `self_host_check/` and `self_host_lowerer/` SYMLINK the whole frontend
  (`ast.gg`, `parser.gg`, `typecheck.gg`, … → `../self_host_typechecker/*`,
  confirmed via `readlink`). `self_host_lowerer/` adds its OWN real lowering
  files (lower_*.gg, lir_*.gg, gir.gg, drop_elab.gg, reachability.gg,
  validate.gg, loader.gg).
- `self_host_parser/` and `self_host_resolver/` are INDEPENDENT real copies
  (own ast.gg + parser.gg + format.gg + resolve.gg/etc).
- Consequence: one edit to `self_host_typechecker/{ast,parser}.gg` changes the
  TYPECHECKER, CHECK, and LOWERER drivers at once. The parser/resolver copies
  need the SAME change applied separately (out of this prototype's scope — §5).

### The CallArg struct (added to canonical `ast.gg`, prototype)
```
struct CallArg:
    Option[String] name      # Some(kw) for `k = v`; None for positional
    int ownership            # OWN_BORROW(0)/OWN_MUTABLE(1)/OWN_MOVE(2)
    SpannedExpr value        # BARE — no EMove/EMutableBorrow wrapper
```
- Ownership convention reuses `Param.ownership`'s (`parser.gg:177-179`
  `const int OWN_BORROW=0 / OWN_MUTABLE=1 / OWN_MOVE=2`): `&`→OWN_MUTABLE,
  `!`/`move`→OWN_MOVE, bare→OWN_BORROW.
- `name` uses `Option[String]` (mirrors Rust `Option<Spanned<String>>`);
  reference-grade over the retired `""`-sentinel sidecar, and already a
  supported field idiom (`Param.default_value` is `Option[SpannedExpr]`).
- AST enum arity change (`ast.gg:62-63` → prototype):
  `ECall(Box[SpannedExpr], Vector[CallArg], Vector[SpannedType])`
  `EMethodCall(Box[SpannedExpr], String, Vector[CallArg], Vector[SpannedType])`
  — the `Vector[String]` names field is REMOVED (merged into CallArg).
- The parallel `Parser.last_arg_names` field + `peek_arg_name` + all
  `call_names = self.last_arg_names` reads are RETIRED (parser.gg). Parser gains
  `parse_arg_ownership()` (mirrors Rust `parse_ownership_modifier`), and
  `parse_call_args` returns `Vector[CallArg]` directly. `skip_ownership_markers`
  is KEPT (still used by function-type-param parsing, `parse_type`).

**Design ground:** CLAUDE.md "Layering discipline" rule 2 (typed-not-shape) +
rule 3 (one source of truth); `decisions.md` LOG "SELF-HOST ARG MODEL"
(RATIFIED) + "D10(b) ADDENDUM"; the Rust reference above.

---

## Section 2 — FULL blast-radius measurement (compiler-enumerated)

The change has three coupled components; an AST-arity change is **ATOMIC** — the
whole driver compiles together, so the compiler enumerates EVERY site in one
build (confirmed: the main gg reported all 167 errors, not first-only).

**(A) ARITY change** (drop the names field). Every destructure + construction:
- `case ECall(` destructures **67**, `case EMethodCall(` **54** → **121**.
- `ECall(` constructions (non-case) **30**, `EMethodCall(` **19** → **49**.
- **≈170 arity sites** across all real self-host `.gg`.

**(B) ELEMENT-TYPE change** `Vector[SpannedExpr]` → `Vector[CallArg]`. Every arg
read as an expression gains `.value` (and `.span`→`.value.span`,
`.expr`→`.value.expr`). This is the component the prior sigil-scout's shape-(b)
estimate (~150) did NOT include. **Compiler-measured** on the FIRST build after
the AST+parser change (lowerer-driver compile set only):

| category | count | meaning |
|----------|-------|---------|
| `expected SpannedExpr, found CallArg` | 104 | `.value` access on an arg |
| `expected CallArg, found SpannedExpr` | 12 | synthetic-call ctor arg (needs `pos_arg` wrap) |
| `expected SpannedType, found String` | 9 | ctor still passing names positionally |
| `E_NoFieldFound` (`.span`/`.expr`/`.name`) | 23 | field access through the CallArg |
| `E_UndefinedName` (dropped `*_names` refs) | 15 | names-vector consumers |
| `E_WrongFieldCount` (Parser ctor / ECall) | 2 | struct-arity followthrough |
| `E_NoMethodFound` (`skip_ownership_markers`) | 2 | helper re-add |
| **TOTAL first-build errors** | **167** | (converged to 0 in ~5 iterations) |

**(C) NAMES-sidecar retirement.** ~15 `call_names = self.last_arg_names` reads +
the field, per parser copy. ~14 destructure sites bind `arg_names`/`carg_names`/
`marg_names` and USE them (meta.gg ×6 AST-transformer rebuilds; typecheck.gg ×2
PositionalAfterNamed; lower.gg 2881/2984; lower_expr.gg 2468 + the static/instance
named-arg reorder blocks; lower_generics.gg 557/1700/1705) — all now read
`arg.name` (or the `callarg_names` adapter).

### Prototype extent (the lowerer-driver compile set): 16 files
`git diff --stat`: **408 insertions / 371 deletions**. Heaviest: lower_generics.gg
(170 lines), parser.gg (139), lower_expr.gg (118), typecheck.gg (86), meta.gg (74).
This is the full canonical frontend (real in typechecker) + the lowerer-own files.

**NOT in this prototype (still to do for the full landing):** the two INDEPENDENT
copies `self_host_parser/{ast,parser,format}.gg` and
`self_host_resolver/{ast,parser,resolve,format,format_resolve}.gg` (≈12 more
destructure + their own parser construction + `last_arg_names` retirement). Their
blast radius is smaller (parser+format+resolve, no lowerer), but they are separate
drivers with their own `*_comparison`/round-trip gates.

Verdict: **materially bigger than the prior scout's ~150** — the full record adds
component (B) on top of (A). Real cost ≈ **170 arity + ~104 `.value` access + ~40
names/field/ctor sites for the lowerer driver**, plus the two parser/resolver copies.

---

## Section 3 — PROTOTYPE result

### What WORKS (Gorget level — the tractability + soundness proof)
- **`gg build self_host_lowerer/driver.gg` → 0 semantic errors.** The entire
  frontend+lowerer typechecks AND lowers under CallArg. Every one of the 167
  enumerated sites was resolved (compiler-guided; convergence 167→63→12→2→0).
- **The lowerer is byte-unchanged where it matters:** `lower_call` keeps its
  `(Vector[SpannedExpr] args, Vector[String] arg_names)` signature; callers adapt
  via `callarg_values(args)` / `callarg_names(args)` (ast.gg helpers). The arg
  VALUES the lowerer sees are the identical bare `EIdentifier`/etc it saw before
  — structurally the reverted-wrapper miscompile class (an `EMutableBorrow`
  wrapper firing the lowerer's dead arm) CANNOT recur, because no wrapper is
  introduced. AST transformers (meta.gg subst/rename, lower_generics subst_mf,
  lower.gg collect_rewrite) PRESERVE `CallArg(name, ownership, …)` through the
  rewrite.
- **Standalone soundness repro** (`patches/callarg-backend-repro.gg`): the exact
  pattern — `struct CallArg{Option[String] name; int ownership; SpannedExpr
  value}`, `Vector[CallArg]`, `args.get(i).unwrap().value.expr`,
  `args.get(i).unwrap().ownership`, and `callarg_values(args)` with
  `for a in args: out.push(a.value)` — **compiles to correct C and runs**
  (`gg run` → `REPRO_OK_48`, the correct 7+0+1+35+2+3). The CallArg mechanism is
  sound end-to-end.

### The BLOCKER (why `self_host_runtime` cannot be run green yet)
`gg build self_host_lowerer/driver.gg` reaches "Generated source file (LIR):
driver.c" then **`cc` FAILS** with 7 identical errors:
```
driver.c:…: error: incompatible types when assigning to type 'GorgetArray' from type 'int32_t'
  In function 'lower_expr___lower_expr_inner'   (3×: c-lines 538787/538814/538857)
  In function 'lower_generics___eval_meta_int_v2' (4×: 544120/544127/544136/544316)
```
Inside `lower_expr_inner`, `__coal49` is declared `__gg_SpannedExpr` and
`__coal75` `int32_t`; the failing statements store an `int32` SSA value into a
`GorgetArray`-typed destination. The Gorget typechecker is happy (0 semantic
errors) — this is a **main-gg C-backend codegen bug** (SSA value-type tracking /
`coalesce_assign_exact`, `src/backend/c_lir/mod.rs:2109`, which groups values by
`decl_ctype` string with a GLOBAL slot counter — an int32/GorgetArray collision
should be impossible by that grouping, so the bug is a decl_ctype or block-live
inconsistency that only manifests at scale).

**Proof it is a latent backend bug, not a CallArg defect:**
1. The BASELINE `self_host_lowerer/driver.gg` builds **clean** (0 cc errors, exe
   produced) with the SAME `gg` binary (`self_host_runtime` is a passing gate).
2. The standalone CallArg repro is **clean** (§ above).
3. `eval_meta_int_v2` breaks with **only `.value` field accesses added** (NO new
   locals, NO helper calls) — so the trigger is the SSA-value-graph perturbation
   of these two ENORMOUS functions (`lower_expr_inner` is the ~8k-line method-
   call lowering monster; `eval_meta_int_v2` the meta-int evaluator), not any
   code I wrote.

**Commands run (worktree, `--release`):**
`./target/release/gg build tests/fixtures/self_host_lowerer/driver.gg`
(baseline: 0 cc errors, exe OK; prototype: 0 semantic errors, 7 cc errors as
above). `./target/release/gg run patches/callarg-backend-repro.gg` → `REPRO_OK_48`.

**Consequence for the gates:** `self_host_runtime` / `self_host_runtime_diff`,
`self_host_bootstrap_fixed_point`, the `*_comparison` count-diffs, and `box_deref`
ASan all require a BUILDABLE driver — none can be run until the backend bug is
fixed. This is the honest state: the load-bearing runtime correctness proof is
**BLOCKED on a backend fix**, though the Gorget-level refactor and the CallArg
mechanism are both proven.

---

## Section 4 — A2-S pos-2/3 re-enable via `arg.ownership` (+ B2 note)

Confirmed the clean re-enable. The D12 pos-2 (ECall ctor) and pos-3 (EMethodCall
collection-put) arms are TEMPORARILY DISABLED at
`self_host_typechecker/typecheck.gg` (the `check_carrier_ops_expr` ECall arm
~:1103 and EMethodCall arm ~:1113, both citing "parse_call_args DISCARDS the
`!`/`&` sigil"). The prototype already converts their arg loops to `a.value`.

With `arg.ownership` available, the re-enable is (mirrors Rust
`src/semantic/safety/helpers.rs`):
```
case ECall(callee, args, _targs):
    check_carrier_ops_expr(*callee, …)
    bool call_is_ctor = is_ctor_callee(*callee, scopes, ctx)   # helper exists :691
    for a in args:
        if call_is_ctor and a.ownership == OWN_BORROW:
            reject_tainted_place(a.value, scopes, &types, &ctx) # helper exists :673
        check_carrier_ops_expr(a.value, scope_id, &scopes, &types, &ctx)
```
and the pos-3 EMethodCall arm gated by `is_collection_ingest_method(method_name)`
(`:752`) + `is_collection_receiver` similarly, `if ingest and a.ownership ==
OWN_BORROW: reject_tainted_place(a.value, …)`.

- The `a.ownership == OWN_BORROW` gate is the CLEAN replacement for the reverted
  wrapper's `expr_is_place`-skips-EMove trick: a bare copy (`W(x)`) is rejected
  iff tainted; `W(!x)` (OWN_MOVE) / `W(&x)` (OWN_MUTABLE) are legal and skip the
  reject WITHOUT changing what `reject_tainted_place` sees (still the bare place
  `x`), and WITHOUT the lowerer ever seeing a wrapper. This is exactly why the
  typed model is the only safe one.
- **Restore checklist** (unchanged from the prior sigil-scout §4): lints.rs
  `self_host_d12_reject_hook_count` **7→9** (`tests/lints.rs:896`); restore the 3
  reject fixtures (`pos2_ctor_init_reject`, `pos3_collection_put_reject`,
  `pos3_field_place_reject`) in `self_host_driver_rejects_d12_drop_purity`; ADD
  the `W(!x)` / `coll.push(!x)` ACCEPT guard (the over-rejection hole that let the
  wrapper bug through) to `self_host_driver_accepts_d12_legal`.
- **B2 (D10(b) place-overlap mirror):** consumes `arg.ownership` DIRECTLY (owner
  directive: honor the typed field, never shape-match). The CallArg record is the
  natural iteration shape — `for a in args: … a.ownership … a.value …`.

**Scope note (EStructLiteral / EDotShorthand):** these two nodes carry
`Vector[SpannedExpr]` (NOT the parallel-names pattern), and the prototype keeps
them so (the parser extracts `.value` at the EDotShorthand site). Their D12 pos-2
reject (typecheck.gg:1182/1188) is therefore UNCONDITIONAL — it cannot ownership-
gate `.Variant(!x)` / `S(!x)`. This matches BASELINE behavior (parse_call_args
already dropped their sigil), so no regression, but **full reference-grade would
extend CallArg to EStructLiteral/EDotShorthand too** (their args ARE constructor
positions). Owner decision needed (§5).

---

## Section 5 — slicing / size / risk / owner questions

**Atomicity:** the AST-arity change is ATOMIC per driver — all sites in a driver's
compile set must land together (confirmed: the compiler enumerates all in one
build; there is no partial-compile). So the natural slices are BY DRIVER:
1. Canonical frontend + lowerer (this prototype: 16 files) — but see BLOCKER.
2. `self_host_parser` copy (ast+parser+format).
3. `self_host_resolver` copy (ast+parser+resolve+format+format_resolve).
4. The A2-S pos-2/3 re-enable + lint + fixtures (additive on top of slice 1).

**Size:** LARGE but MECHANICAL. ~170 arity + ~104 `.value` + ~40 names/field
sites for the lowerer driver (16 files, ~780 changed lines), plus the two smaller
copies. ~90% is compiler-guided mechanical edits (a `.value`-insertion pass keyed
off `expected SpannedExpr found CallArg` errors handled 104 sites automatically);
the non-mechanical ~10% are the AST-transformer rebuilds (preserve `CallArg`
fields through subst/rename) and the named-arg-reorder adapters in
`lower_expr_inner`.

**Risk:**
- **HIGH / BLOCKING: the main-gg C-backend miscompile (§3).** This is the gating
  risk. It must be root-caused and fixed FIRST (a backend coalescing / LIR
  value-type-tracking bug in `src/backend/c_lir/mod.rs` around
  `coalesce_assign_exact`, exposed by SSA perturbation of `lower_expr_inner` /
  `eval_meta_int_v2`). Until then `self_host_runtime` cannot go green, so the
  ratified change cannot be validated to the mandatory gate. **Recommend: the
  executor's FIRST task is to reproduce + fix this backend bug (the prototype
  patch + `git build` reproduces it deterministically), independent of CallArg.**
- MEDIUM: the AST-transformer rebuilds (meta.gg, lower_generics, lower.gg) must
  preserve name+ownership — a silent drop there would lose named-arg / D12
  metadata. The prototype preserves them (`CallArg(a.name, a.ownership, subst(…))`);
  a reviewer must verify each.
- LOW: the mechanical `.value` pass (over-application caught 2 sites where `arg`
  was already a SpannedExpr — fixed; a reviewer should diff the auto-inserted
  `.value` against intent). The `EStructLiteral`/`EDotShorthand` scope carve-out
  (kept as SpannedExpr) is a deliberate boundary, not a bug.

**Owner questions:**
1. **The backend bug is the real gate.** Do you want the executor to fix the
   main-gg coalescing/type-tracking bug as the prerequisite step (recommended,
   since the ratified CallArg change cannot pass `self_host_runtime` without it),
   or investigate whether re-structuring the CallArg edits to MINIMIZE SSA
   perturbation of `lower_expr_inner`/`eval_meta_int_v2` dodges it (fragile,
   against the mechanical nature, and papers over a real backend bug — NOT
   recommended per "don't redesign around compiler gaps")?
2. **EStructLiteral / EDotShorthand scope:** extend CallArg to these two
   constructor-position nodes now (full reference-grade, ownership-gated pos-2
   reject for `.Variant(!x)` / `S(!x)`), or keep them on `Vector[SpannedExpr]`
   (matches baseline, smaller diff, but leaves an unconditional pos-2 reject)?
   The prototype takes the latter.
3. **`lower_call` boundary:** the prototype adapts CallArg→(values,names) at the
   4 `lower_call` call sites (keeping `lower_call`'s internals byte-identical — a
   deliberate choice that PROVES the lowerer is untouched). The fuller
   reference-grade shape threads `Vector[CallArg]` INTO `lower_call`. Keep the
   adapter (lower churn, proven-neutral) or thread the record (retires the
   internal parallel names too)?
