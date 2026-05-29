# Bug B — `public static Vector[T]/Dict[K,V]` literal initializers silently dropped

**Status:** design (no implementation). This doc is the spec; it will receive ≥3 fresh review passes before execution.

## 1. The bug (verified)

```gorget
struct Item:
    String name
public static Vector[Item] TABLE = [Item("alpha"), Item("beta")]
void main():
    print(f"len={TABLE.len()} expect 2")
```

Verified against `./target/release/gg run` in this worktree (commit-clean tree): prints `len=0`.
The two `Item` elements are silently discarded; the static is an empty `GorgetArray`.

**Root cause.** `eval_static_init` (`src/ir/lowering/mod.rs:2344`) handles scalar literals
(`mod.rs:2357-2380`), primitive extern-call statics (`:2389-2410`), and constructor-syntax
statics — `AtomicInt(…)`, `Dict(…)`/`Vector(…)`/`HashMap(…)` **zero-element** ctors, file-handle
getters, generic struct literals (`:2418-2557`). There is **no** `Expr::ArrayLiteral` /
`Expr::DictLiteral` arm. Array/dict *literals* fall through every dispatch to the catch-all
`_ => return GlobalInit::Zeroed` (`mod.rs:2433` for the literal shape, also `:2558`). `Zeroed`
emits a zeroed struct; nothing populates the elements.

The brief's stated lines (`~2344`, `~2433`) are accurate; the enum is at `src/ir/mod.rs:721-741`.

### Why this is not a one-line arm

`GlobalInit::Extern { name, args }` (`src/ir/mod.rs:737-740`) — the existing runtime-populated-static
mechanism — encodes exactly **one** container-`new` call with **literal** args
(`GlobalInitArg`: `Int`/`Float`/`Bool`/`Sizeof`/`StrLit`/`AddrOfInline`, `src/ir/mod.rs:748-772`).
The C prologue (`src/backend/c_lir/mod.rs:1761-1796`) emits one statement per `Extern` global:
`__lir_g<N> = name(arg, …);`. There is no representation for "then push these N elements", and
the elements are **arbitrary expressions** (`Item("alpha")` is a struct ctor, not a literal). So a
naïve `Vector` arm that emitted `gorget_array_new(sizeof(T))` would fix the *empty* case only and
still drop every element of the motivating non-empty `BUILTIN_METHODS`-style table. The fix must run
**per-element initialization** at module-init time.

## 2. Recommended approach — **Option B: synthesize a zero-arg init function**

For each static whose initializer is not compile-time-encodable (today: array/dict/set literals;
see scope decision §3), synthesize a function

```
<StaticType> __gg_static_init_<NAME>():
    <StaticType> __r = <the static's RHS expr>
    return __r
```

lower it through the **existing** function-lowering path (which reuses the existing array/dict-literal
lowering at `src/ir/lowering/exprs/collections.rs:13-179`), register the static as
`GlobalInit::Extern { name: "__gg_static_init_<NAME>", args: [] }`, and seed the synthetic fn as a
DCE root. The existing prologue then emits `__lir_g<N> = __gg_static_init_<NAME>();` with **zero**
backend changes (`src/backend/c_lir/mod.rs:1785-1793` already renders `name()` with empty args).

### Why the body is `<T> __r = RHS; return __r`, not `return RHS` — load-bearing detail

Verified empirically in this worktree:

- `Vector[Item] f(): [Item("a")]` and `Vector[int] f(): return [1,2,3]` **both fail type-checking**:
  `type mismatch: expected Vector[T], found <defined>[N] / int[N]`. An array literal in *return
  position* is treated as a fixed-array type and not coerced to `Vector[T]`.
- `Vector[Item] f(): Vector[Item] r = [Item("a"), Item("b")]; return r` **compiles and runs
  correctly** (`len=2`, element `name` = `"alpha"`).

The difference is `expected_type`: a `VarDecl` sets `ctx.func_state.expected_type` to the declared
type before lowering the init (`src/ir/lowering/stmts/mod.rs:478`, in `lower_var_decl` — NOT
`assigns.rs:109`, which is the `Stmt::Assign` reassignment path), which the array-literal lowering
reads to size the buffer and propagate the element type (`collections.rs:27,36,46-63`; the empty-array
`elem_size` fallback to I64=8 is at `collections.rs:165-167`). Return position does not set it. **So
the synthetic body MUST be the local-binding-then-return shape.**

Crucially, the synthetic function is built **during IR lowering**, *after* semantic analysis has run —
so it never faces the type checker, and the `return RHS` rejection above does not block us. (We choose
the `VarDecl` shape for the `expected_type` reason, not to satisfy the checker.)

### Why the other options lose

- **Option A (extend `GlobalInit` to carry lowered init *instructions*).** Requires a new `GlobalInit`
  variant holding a Vec of GIR/LIR insts, threading SSA/value-id namespaces into a context (the
  prologue) that has none, and teaching *both* backends to lower a free-floating inst sequence into
  `main`'s body. That is exactly the "intrinsically complex fix at the read site" the layering doc warns
  against — it reinvents function-body lowering inside the global-init representation. Option B reuses
  the function-body lowering that already exists and is already exercised by every expression.
- **Option C (one `__gg_module_init()` accumulating all inits).** Strictly more invasive than B for no
  benefit here: it still needs per-static body synthesis (same work as B), plus a new aggregation pass,
  plus a single new DCE root and a single new prologue call. B's per-static fn is the same machinery
  factored per-static; C only wins if we wanted ordering guarantees *between* statics, which Gorget does
  not currently specify and no fixture needs. C can be a later refactor on top of B (have the prologue
  call one wrapper) without changing B's per-static synthesis. Recommend B; note C as a possible future
  consolidation.
- **Self-host clincher for B.** The self-host static record `GirStaticInfo { init_expr: String, … }`
  (`gir.gg:275-277`) stores the init as a **raw C expression string**, and the prologue
  (`lir_codegen.gg:4386-4395`) emits `__lir_g<N> = <init_expr>;` verbatim. So on the self-host side,
  `init_expr = "__gg_static_init_<NAME>()"` slots in with **no** prologue change. A and C would force the
  self-host to grow a new init representation; B is a string substitution + one synthesized GirFunction.

## 3. Scope decision — **array/dict/set literals in v1, with a general hook**

`eval_static_init`'s `Zeroed` fallback silently breaks **any** non-const static initializer, not just
collection literals (e.g. `public static Foo X = some_fn()` where `some_fn` is an ordinary user fn, or
any RHS richer than a literal/ctor). Option B generalizes to all of these for free — the synthesized
`<T> __r = RHS; return __r` works for any RHS the function-body lowerer accepts.

**Decision: v1 fires the synthetic-init path for `Expr::ArrayLiteral` and `Expr::DictLiteral` only**
(the immediate, verified need + the motivating `BUILTIN_METHODS` case). NOTE: set literals `{a,b,c}` are
NOT a separate AST node — they parse as `Expr::ArrayLiteral` disambiguated by `expected_type`
(`collections.rs:18-35`; self-host `ast.gg:71`), so the `ArrayLiteral` arm already covers them; there is
no `ESetLiteral` to match. Reason:
the broad case has a sharp interaction — a static whose RHS calls a user fn could observe *other* statics
that haven't been initialized yet (the prologue runs init calls in global-id order at
`c_lir/mod.rs:1774` / `lir_codegen.gg:4387`). Restricting v1 to self-contained collection literals
sidesteps init-ordering semantics we haven't specified. The dispatch is written as a single predicate
`initializer_needs_synthetic_fn(expr)` so widening to the general case in a follow-up is a one-line
predicate change plus an init-ordering decision — not a re-architecture. Record the general case in
`TODO.md` as a follow-up gated on an init-ordering spec.

## 4. Exact insertion points

### 4.1 Rust compiler

| Concern | File:line | Change |
|---|---|---|
| Detect collection-literal init | `src/ir/lowering/mod.rs:2357` (top of `eval_static_init` match) **and/or** `lower_static_decl` `:2325` | Before `eval_static_init`, test `initializer_needs_synthetic_fn(&decl.value.node)` (true for `Expr::ArrayLiteral`/`Expr::DictLiteral`). If true, skip `eval_static_init` and take the synthetic path. |
| Synthesize + register | `lower_static_decl`, `src/ir/lowering/mod.rs:2313-2340` | Build a `FunctionDef` (name `__gg_static_init_<name>`, no params, `return_type = decl.type_.clone()`, body = `FunctionBody::Block(Block([VarDecl_stmt, Return(Some(EIdentifier("__r")))]))`). **The `VarDecl` stmt is the REAL `Stmt::VarDecl` shape (`src/parser/ast.rs:914-921`): `pattern: Pattern::Binding("__r")`, `type_: decl.type_.clone()` (the field is `Spanned<Type>`, NOT `Option`), `value: decl.value.clone()`, `is_const: false`, `is_mutable: true`, `shared: SharedKind::None` (the field is the `SharedKind` enum at `ast.rs:896-905`, NOT a bool)** — there is NO `name` field; `lower_var_decl` dispatches on `Pattern::Binding` (`stmts/mod.rs:380`). Push the fn to a new `ctx.synthetic_static_init_fns: Vec<FunctionDef>` accumulator (new field on `LoweringContext`). Push the `Global` with `init = GlobalInit::Extern { name: "__gg_static_init_<name>".into(), args: vec![] }`. (Clone `decl.value` into the synthetic body; the global slot itself is left zeroed + prologue-assigned — `decl.value` is lowered ONCE, inside the synthetic fn, never into the slot.) |
| Lower synthetic fns | `src/ir/lowering/mod.rs:1242-1257` (the non-generic function loop) | After the existing user-function loop, iterate `ctx.synthetic_static_init_fns` (cloned out to satisfy the borrow checker) and call `lower_function(&mut ctx, &mut module, &f, None)`. Placing it here (not inside the globals loop at `:1234`) guarantees the type registry + monomorph collection have run. |
| DCE root seeding | `src/lir/optimize.rs:206-220` (root loop in `find_live_functions`) | Add `|| func.name.starts_with("__gg_static_init_")` to the root predicate. **Required**: the prologue call is raw C text, invisible to the LIR call graph (`collect_global_func_refs`, `optimize.rs:358-368`, only handles `FuncAddr`/`Struct`, not `Extern{name}`). Without this seed the fn is DCE'd and the prologue call link-fails. |
| `GlobalInit` enum | `src/ir/mod.rs:721-741` | **No change** — reuse `Extern { name, args: [] }`. |
| Prologue emit | `src/backend/c_lir/mod.rs:1761-1796` | **No change** — already renders `name()` for empty-arg `Extern`. |

`literal_to_global_init_arg` / `literal_to_global_init` (`mod.rs:2572-2587`, `:2595+`) — **no change**.

### 4.2 Self-host (`tests/fixtures/self_host_lowerer/`, symlinked into `self_host_typechecker`)

| Concern | File:line | Change |
|---|---|---|
| `IStaticDecl` handler | `lower.gg:9246-9279` (the `case IStaticDecl(sd_ty, sd_name, sd_val)` cascade; the array/dict literal currently hits the final `else: pass` at `:9277-9278`) | Before the int/float/runtime-call cascade — or as a new branch after `try_register_static_runtime_call` fails — test `static_init_needs_synthetic_fn(sd_val.expr)` (true for `EArrayLiteral`/`EDictLiteral`/set-literal). If true: synthesize a `FunctionDef` (name `"__gg_static_init_" + sd_name`, empty params, `return_type = sd_ty`, body `[SVarDecl(sd_ty, "__r", sd_val, OWN_BORROW, 0, -1), SReturn(Some(EIdentifier("__r")))]`, `is_expr_body=false`) — the 4th arg is the ownership kind: use `OWN_BORROW` (`=0`, `parser.gg:177-179`); there is NO `OWN_DEFAULT` (a plain non-sigil binding is `OWN_BORROW`), `lower_function(synth_fd, &gmod)` (`lower.gg:7352`), `gmod.functions.push(synth_gfn)`, and `static_put(&gmod, sd_name, "__gg_static_init_" + sd_name + "()", map_ast_type(sd_ty,&gmod))` (`gir.gg:376`). |
| `try_register_static_runtime_call` | `lower.gg:3284-3301` | **No change** — leave it for the `_handle()` getters. The new branch is separate. |
| `static_put` / `GirStaticInfo` | `gir.gg:275-277,376-377` | **No change** — `init_expr` already carries an arbitrary C expr string; `"__gg_static_init_<name>()"` is exactly such a string. |
| `lir_lower.gg` global registration | `lir_lower.gg:3605-3610` | **No change** — already iterates `gmod.statics`, builds `LirGlobal(… GINIT_RUNTIME_CALL … sg_info.init_expr …)`. |
| Prologue | `lir_codegen.gg:4386-4395` | **No change** — emits `__lir_g<N> = <init_expr>;` verbatim. |
| Self-host DCE | `lir_codegen.gg:944-961` (root-seeding block in `compute_reachable_fns`) | Add an `elif name.starts_with("__gg_static_init_"): is_root = true` arm. **Required** for the same reason as the Rust seed: the prologue call is raw text (`init_expr`), invisible to the ICall/ICallExtern transitive walk (`lir_codegen.gg:1028-1101`). |

> **⚠ Pipeline-position caveat (pass-1 finding) — v1-safe, but the §3 widening MUST revisit it.**
> Rust lowers the synthetic fn in the non-generic function loop *after* monomorph collection
> (`mod.rs:1257`); the self-host §4.2 lowers it *inline in the `IStaticDecl` pass* (`lower.gg:~9246`),
> which runs *before* generic-template collection (`~9285`) and the main function loop (`~9541`). For
> **v1's restricted scope (collection literals with CONCRETE element ctors, non-generic)** the two
> positions are EQUIVALENT — the synthetic body needs no monomorph instantiation, so early vs late
> lowering produces byte-identical results, and the `lowerer_comparison` fn-count guard confirms parity.
> But if the §3 widening ever lets an element expression trigger a generic instantiation, the
> self-host's early-lowered body would be INVISIBLE to the monomorph collector while Rust's late-lowered
> one is collected → internal-body byte-divergence the fn-count guard would NOT catch. **The §3
> widening follow-up must defer the self-host synthetic-fn lowering to the same relative position as
> Rust (post-monomorph), or keep the concrete-element restriction.** Stated here so the v1 executor
> does not "tidy up" by widening the predicate without revisiting placement.

### 4.3 CLASS-1 DCE overlap (note, do not entangle)

Agent a3a14f53 found the self-host `compute_reachable_fns` lacks the global-init / `FuncAddr`
seeding that Rust's `find_live_functions` has (`optimize.rs:262-268` walks `module.globals` via
`collect_global_func_refs`). The bug-B seed is **name-prefix**-based (`__gg_static_init_`), not
`FuncAddr`-in-global-based, so it is **orthogonal**: a future CLASS-1 fix that adds general
global-`FuncAddr` seeding to the self-host will *not* cover bug-B's fns (they're referenced by raw
text, never as a `FuncAddr` LIR global). Keep the two seeds as separate, additive root arms so neither
fix regresses the other. Do **not** try to fold bug-B's seeding into a generic global-walk — the
prologue-text reference is precisely the case a global-walk can't see.

> Layering note: the `name.starts_with("__gg_static_init_")` root predicate is **not** a "name
> matching" violation per CLAUDE.md rule 2. It is a DCE *root seed* (a reachability hint for a
> compiler-synthesized symbol whose only caller is compiler-emitted text), the same category as the
> existing `__test` / `__suite_` / `__call` / `__bench_` root prefixes already in both root sets
> (`optimize.rs:208-214`, `lir_codegen.gg:948-957`). It makes no *semantic* decision about user code.

## 5. Output-neutrality

The change adds behavior **only** on the previously-`Zeroed` array/dict-literal path. Every other
static is byte-unchanged:

- Scalar literals (`Bytes`), string literals (`Extern gorget_str_from_literal`), primitive extern-call
  statics, ctor-syntax statics (`AtomicInt`/`Dict()`/`Vector()`/`Mutex`/`File`-handles/generic struct
  literals), and `Option[T] = None` — all still hit their existing arms in `eval_static_init` /
  `try_register_static_runtime_call`; the synthetic predicate returns false for them.
- The empty `Vector[T] = []` / `Dict[K,V] = {}` case: **decide explicitly.** Today `[]`/`{}` as an
  array/dict-literal initializer also hits `Zeroed` (it's `Expr::ArrayLiteral`/`DictLiteral` with no
  elements) — so it is *also* currently broken (an empty static collection that is `Zeroed` has no
  `elem_size`, so a later `.push` mis-sizes). Routing empty literals through the synthetic fn fixes
  them too and is consistent. **Recommendation: include empty literals in the predicate** (the
  synthetic body lowers `Vector[T] __r = []` correctly with the declared `elem_size`). This is a
  behavior *change* for empty-literal statics — call it out in the PR and cover it with a fixture
  assertion (a `static Vector[int] E = []` that is pushed to in `main` and read back).

## 6. Both-compilers-agree & sequencing

The self-host compiles itself; the Rust fix and the self-host mirror must emit equivalent C. Sequence so
neither half breaks the bootstrap alone:

1. **Land the Rust fix first** (Stage 1). The self-host `.gg` sources are unchanged, so
   `self_host_bootstrap_fixed_point` still builds the self-host from the *fixed* Rust gg — but the
   self-host sources contain **no** `static Vector[...] = [...]` initializers today (verify with the
   grep in Stage 0), so the Rust fix alone cannot change the self-host's emitted C. Fixed-point stays
   green.
2. **Land the self-host mirror second** (Stage 2). Now both compilers agree. Because the self-host
   sources still have no collection-literal statics, the mirror is *dormant* in the bootstrap (it only
   fires on user fixtures that use the feature), so it cannot perturb the fixed point either.

If a future self-host source introduces a `static Vector[...] = [...]`, both halves must already be
landed — hence Stage 1 before Stage 2, and the mirror must produce a byte-identical `__gg_static_init_`
fn + prologue call to what Rust gg produces for the same source. The `lowerer_comparison` test (fn-count
parity) is the guard that the self-host emits the same synthetic fn Rust does.

## 7. Staged plan with validation gates

**Stage 0 — baselines (no code).**
- Capture the repro (`len=0`).
- `grep -rn "static .*\[\|static .*= {" tests/fixtures/self_host_*/` and `lib/` to confirm no
  collection-literal statics exist in self-host/stdlib sources (premise of §6). Record findings.
- Record current `lowerer_comparison` fn-count and `self_host_bootstrap_fixed_point` green.

**Stage 1 — Rust fix.** Implement §4.1. Gate:
- Repro now prints `len=2` and element `name`s are correct.
- New fixture `tests/fixtures/static_vec_literal.gg` (deterministic stdout: len + a couple element
  values for a `Vector[struct]` static, a `Dict[String,int]` static, and an empty-then-pushed static),
  wired into `tests/integration.rs` with `run_gg(...)` (pattern at `integration.rs:295-297`).
- `cargo build` + `cargo test --lib` green.
- `cargo test --test integration -- --test-threads=4 …` targeted: the new fixture + a sampling of
  existing static-using fixtures (atomics, file handles, string statics) unchanged.
- `self_host_bootstrap_fixed_point` still byte-identical (the heavy gate — parent runs it).

**Stage 2 — self-host mirror.** Implement §4.2. Gate:
- `lowerer_comparison --nocapture`: fn-count parity holds *and* (manual spot-check) the self-host emits
  a `__gg_static_init_<name>` fn for the new fixture matching Rust gg's.
- `self_host_bootstrap_fixed_point` still green.
- Re-run the Stage 1 fixture through the self-host driver (or via the comparison harness) to confirm
  identical C / identical runtime stdout.

**Stage 3 — cleanup/record.** Add `TODO.md` entry for the general non-const-static case (§3) gated on an
init-ordering spec. Note the C-consolidation (Option C) as optional future work.

## 8. Honest complexity / risk assessment

- **Lowest-risk part:** the prologue and `GlobalInit` enum need **no** changes on either compiler —
  the existing empty-arg `Extern` rendering is exactly what we emit. This is why B is cheap.
- **Riskiest part: DCE seeding, on BOTH compilers.** The synthetic fn is referenced *only* by raw C
  text in the prologue, which neither DCE walker can see. Miss the seed → the fn is pruned → link error
  (Rust) / missing-symbol (self-host C). It is a two-line additive root arm on each side, but it is the
  single point where "looks done, links broken" hides. The self-host overlap with the CLASS-1 gap
  (§4.3) makes it doubly worth an explicit reviewer check that the two seeds stay separate and additive.
- **Second risk: the synthetic body shape.** If anyone "simplifies" it to `return RHS`, it silently
  reverts to the type-checker-rejected shape *for hand-written code* — but since the synthetic fn skips
  the checker, the failure would instead be a *mis-sized array* (no `expected_type` → wrong
  `elem_size`), a quiet runtime corruption, not a compile error. The `VarDecl`-with-declared-type shape
  is load-bearing; comment it at both synthesis sites citing `stmts/mod.rs:478` (the VarDecl
  `expected_type` set) / `collections.rs:46-63,165-167` (the size+elem-type read).
- **Third risk: self-host fn-count parity.** Adding a synthetic fn changes `user_fn_count`; the mirror
  must add the *same* fn so `lowerer_comparison` stays at parity. Sequencing (Rust first) means the
  comparison is briefly skewed by exactly the synthetic fns Rust emits for fixtures that use the
  feature — but the self-host sources themselves have none, so the *bootstrap* fn-count is unchanged;
  only user-fixture comparisons shift, and Stage 2 restores them.
- **Not a risk:** monomorphization. The synthetic fn is non-generic (concrete `Vector[Item]` etc.) and
  lowered in the non-generic function loop after monomorph collection, so `Item`/`String` element
  ctors resolve normally.
