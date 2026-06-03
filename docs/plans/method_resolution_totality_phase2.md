# Plan — Method-Resolution Totality, Phase 2 (Option B: one source of truth for builtin methods)

**Status:** REVIEWED & SIGNED OFF — 3 sequential fresh-agent review passes (R1: 4 reservations folded;
R2: 6 incl. a fold-introduced §2↔§6 contradiction; R3: clean), informed by 3 parallel source scouts.
EXECUTION-READY for a future agent. Builds on Phase 1 (the `unwrap`/`expect`/`unwrap_or` checker gate,
landed) and on `docs/plans/builtin_method_decl_port.md` (the "Phase 2a" seed; land it first).

> **All `file:line` citations below are as-of the planning tip and WILL drift.** The executor must
> RE-PIN every cited site by content on a fresh checkout before editing (the codebase moves; e.g. the
> §8 `error_id` defaults already moved ~25 lines between Phase-1 and now). Treat the line numbers as
> "find the construct near here," not contracts.

## 1. Goal
Today the compiler has **two parallel, hand-maintained builtin-method registries** that have drifted:
- the type checker's `builtin_method_type` (`src/semantic/typecheck.rs:4864-5237`, ~374-line name-keyed match → semantic `ResolvedType` return types), and
- the IR lowering's `ALL_PROTOCOLS`/`BuiltinMethodDecl` (`src/ir/lowering/builtins.rs`, 30 protocols / ~180 method decls → GIR `TypeId` via `fn(&BuiltinTypeArgs,&LookupCtx)->TypeId` thunks + runtime/CoW facts).

Collapse them into **ONE declarative registry** living in the existing `resources` infrastructure
(`compiler/data/{schema,resources}.gg`), consumed by BOTH the semantic checker and the IR lowering
(and the self-host, which already imports `RESOURCES`). This makes the checker's method knowledge a
**superset of the emittable surface by construction**, which is the precondition for the *final*
step: flipping the resolution gate so an unresolved method is a clean `gg check` error
(`NoMethodFound`) instead of a silent `error_id` default + backend name-mangle-and-hope.

This is the "be like Rust" end-state (the impls/registry ARE the single member source); Phase-1's
`unwrap` gate was the bounded down-payment.

## 2. Scope — what the registry OWNS vs. LEAVES ALONE
The registry owns ONLY the **builtin collection/handle method surface** (`push`/`get`/`len`/`keys`/
`lock`/`recv`/`load`/string-view methods/…). It must NOT subsume — these stay in their own resolvers:
- **Static `Type.method`** (`int.parse`, `str.parse`, `int.default`) → `resolve_static_method_type` (`typecheck.rs:4742-4814`).
- **Enum-variant constructors** (`Color.Red()`) → `infer_variant_constructor`.
- **User `equip` / trait-impl / trait-default / `via`** → the trait registry `resolve_method` (`traits.rs:157-303`).
- **Closure-typed HOFs** (`Vector.map/filter/fold`, `Option.map/and_then`, `Dict/Set.filter`) → `infer_closure_method_type` (`typecheck.rs:4413+`), which fires BEFORE the registry (`~:1911`) so the SEMANTIC interpreter never sees them. **HARD EXCLUSION for the semantic side only** (see §6): the tag is `Infer` *semantically* — BUT the IR interpreter must KEEP the rows' `ret_self`/`ret_int` fallback (they are "signature-load-bearing" per `builtins.rs:314-320`, read via `methods.rs:2158`). The tag is interpreter-specific, NOT "no signature anywhere".
- **Universal `.clone()`** + **derivable/auto-trait methods** → the auto-trait subsystem (`register_builtin_traits` `traits.rs:505-590`, `is_auto_derivable` carve-out `typecheck.rs:1978-1981`). ⚠ The carve-out is EXACTLY `clone | debug | display | hash` — `eq`/`compare` are NOT exempt (don't assume they are at the §8 gate-flip). (Per-type collection `clone` rows already exist in the protocol as `ret_self`/`returns_fresh` and MAY stay registry-owned, but universal-clone-on-any-type does not.)

**Decisions the executor must make explicitly (see §7):** whether to fold **primitive intrinsics**
(`int.mod`, `uint8` char-class) and **stdlib runtime types** (`Box`, `File`, `Socket`, `Arena`,
`ArenaCheckpoint`, `Tlsf*`/`FixedBuffer*`) — these are checker-only today (no IR protocol entry), so
folding them means *adding* protocol coverage, not just unifying.

## 3. The foundation we build on (`resources`)
- Source of truth: hand-written `compiler/data/schema.gg` (type defs) + `compiler/data/resources.gg`
  (data: `SCHEMA_VERSION`, `RESOURCES` 31 rows, `RUNTIME_FNS` 299 rows). `include_str!`'d via
  `src/compiler_data.rs:12-13`; parsed at runtime by `src/ir/resources.rs` (`table()` `OnceLock`,
  `walk_module` positional AST walker `:87-117`, `SCHEMA_VERSION` guard PANICS on mismatch `:65-70`);
  Rust mirror types in `src/ir/resource_schema.rs` (zero `crate::` imports — pure/layer-neutral).
  Dev override: `GORGET_RESOURCES_PATH`.
- **Dual-consumer**: self-host already does `from compiler.data.resources import RESOURCES`
  (`tests/fixtures/self_host_lowerer/lir_lower.gg:38`). Any schema change is a coordinated atomic edit
  across `schema.gg` + `resources.gg` + `resource_schema.rs` + the version bump + the self-host import.
- **Extension playbook (proven)**: add a table = define struct(s) in `schema.gg`, mirror in
  `resource_schema.rs`, add a `build_*` walker arm + a `walk_module` case, add an accessor, bump
  `SCHEMA_VERSION`. Precedent: the `c_typedef_name` additive field (2026-05-20). **NO TOML/codegen/
  build.rs** — that approach is explicitly rejected/dead (`devbook/18-runtime-abi.md:255-261`).
- **`builtin_method_decl_port.md` is Phase 2a (down-payment), not a competitor.** It already designs a
  `BUILTIN_METHODS` table + `BuiltinRetKind` enum (Void/Int/Bool/F64/U8/String/Array/Self/Elem/
  OptionElem/OptionRefElem/Infer) + dispatcher, scoped to retiring 3 self-host name-lists
  (`is_string_view_method`, `is_owning_mutator_arg`, the `infer_method_return_type` String rows).
  **Recommended staging: land `builtin_method_decl_port.md` FIRST** (establishes the `BUILTIN_METHODS`
  table + `BuiltinRetKind` in `resources`), then Option B extends that same table.

## 4. Layering
Hoist `resources` + `resource_schema` from `src/ir/` to a top-level `crate::resources` (a pure move —
neither file depends on `ir` internals). Both `src/semantic/` and `src/ir/lowering/` then read the
unified table with no inversion. (Even left in place it compiles — semantic→ir already exists 15× —
but the hoist is the layering-clean home and worth doing as step 0.)

## 5. Schema design — the `BUILTIN_METHODS` table
Per-(family, method) declaration carrying the **UNION** of both registries' facts as TYPED fields
(never name-string conventions — per "No name matching"):
- `family: &str` (base_name key: "Vector"/"Dict"/"String"/…), `name: &str` (method).
- **Runtime/lowering facts** (from Reg2): `runtime_callee: Option<&str>`, `self_conv: SelfConvention`,
  `is_mutating: bool`, `returns_view: bool`, `returns_fresh: bool`, `arity`/`owning_arg_positions`.
- **Signature shape-DSL** (the crux, §6): `params: Vec<TypeShape>`, `ret: TypeShape` — a layer-neutral
  enum, NOT IR `TypeId` and NOT semantic `ResolvedType`.
- `TypeShape` must express (union of what BOTH layers need, from the scout inventory):
  scalars `Void/Bool/Int/U8/F64` (+ sized-int family the semantic side distinguishes);
  projections `SelfTy/Elem/Key/Val`; **owned-vs-view String as TWO distinct tags** `OwnedString`
  vs `StringView` (load-bearing for CoW + the checker's `string_id` vs `owned_string_id`);
  **Option markers** `Opt(shape)` and `OptRef(proj)`/`OptOwned(proj)` where `proj ∈ {Elem, Val}` — the
  axis matters: `Vector.get → Option[Ref[T]]` (Elem) vs `Dict.get → Option[Ref[V]]` / `Dict.remove →
  Option[Owned[V]]` (Val); the IR side already splits these as `ret_option_ref_or_val_elem` vs
  `ret_option_ref_or_val_val`/`ret_option_val` (`builtins.rs:182/200/188`). This borrow-provenance (the
  checker encodes as `ResolvedType::Ref/Owned`, the IR as the `Option__Ref__T` mangled name) must become
  ONE typed tag carrying the axis; constructed generics `VecOf(shape)/VecOfTuple(Key,Val)/
  VecOfVec(Elem)/Weak(Elem)/Guard(Elem)/{Read,Write}Guard(Elem)/Shared(Elem)/OptShared(Elem)`;
  structural tags for non-projection returns in registry-OWNED families — `FlattenOpt` (`Option.flatten`) and `ElemOfElem` (`Shared[Vector[T]].at`) (see §6); and an explicit `ResolvedElsewhere`/`Infer` tag for the HARD-EXCLUSION rows (closure-HOFs). NOTE: the Reg1-only File/Socket/Arena families also need `ResultOf(ok,err)` (`File.read_all → Result[String, str]`) and a bare named-type (`Arena.checkpoint → ArenaCheckpoint`) — DEFERRABLE under §7/§11 (no IR-protocol entry today), so the DSL needs them only if/when those families are folded.

## 6. Per-layer resolvers (one DATA table, two interpreters)
**Not** "one pure function lowers to both" — the IR side mutates state. Build TWO interpreters of
`TypeShape`:
- **Semantic interpreter** (replaces `builtin_method_type`): `TypeShape` + receiver `type_args` →
  `ResolvedType`/`TypeId`. `Elem`→`type_args[0]`, `OptRef(Elem)`→`intern_generic(Option,[Ref(elem)])`,
  `StringView`→`string_id`, `OwnedString`→`owned_string_id`, etc.
- **IR interpreter** (replaces the `params`/`return_type` thunks): a `&mut Context` method
  `shape_to_typeid(shape, &BuiltinTypeArgs, &LookupCtx) -> TypeId` that reproduces today's behavior —
  incl. mangled-name lookup (`Option__Ref__<elem_name>`, `ensure_option`) and the `Ptr`/`MutPtr`
  self-param insertion driven by `self_conv`. The existing `resolve_builtin_method_return_type`
  (`context.rs:654-771`) + `register_builtin_method_sigs` (`:515`) become consumers of the table +
  this interpreter.
- **Self-host interpreter**: the `builtin_method_decl_port.md` `BuiltinRetKind` dispatcher, extended to
  the fuller `TypeShape` set. (Self-host can't store closures — the enum-tag DSL is exactly why this
  representation is mandatory, not optional.)

### HARD CASES (must be in the plan; each is a scout-confirmed sharp edge)
- **Closure-HOFs** (`map`/`filter`/`fold`/`reduce`/`flat_map`/`zip`/Option/Dict/Set HOFs): the `Infer`
  tag is **interpreter-specific, NOT "no signature anywhere"** (a uniform drop would regress the IR
  layer). SEMANTIC interpreter: defer to `infer_closure_method_type` (which fires *before* the registry
  at `typecheck.rs:~1911`, so the registry is never consulted for these on the semantic side). IR
  interpreter: KEEP today's `ret_self`/`ret_int` fallback for these rows — they are explicitly
  "signature-load-bearing" (`builtins.rs:314-320`) and read via `resolve_builtin_method_return_type`
  (`methods.rs:2158`, `.unwrap_or(UNIT_TYPE)`) when a user-space HOF wrapper's mono sig isn't yet
  registered. So the row carries `Infer` + the IR-side fallback shape; the semantic interpreter ignores it.
- **Structural Option/elem returns the projection tags don't cover** (registry-OWNED families — must be
  handled, not silently dropped): `Option.flatten` strips ONE Option level (`typecheck.rs:5090-5100` +
  `builtins.rs:746-754`); `Shared[Vector[T]].at → T` is elem-of-elem (`typecheck.rs:5131-5139`). Add
  structural `TypeShape` tags (`FlattenOpt`, `ElemOfElem`) or tag them `Infer`/handled-elsewhere with an
  explicit note — do NOT assume `Elem`/`OptRef` cover them.
- **The `&mut Context` coupling**: the IR interpreter writes `fn_sigs`/`runtime_callees`/`type_registry`.
  Keep it impure; only the *spec* is shared data.
- **`ensure_option` fragility**: the IR thunk returns `I64_TYPE` if the Option mangled type isn't
  registered yet (`context.rs:713-722`). Preserve this exact behavior; do not "fix" it here.
- **Load-bearing rows — DO NOT regress**: the collection getter `Option[Ref[V]]`/`Option[Owned[V]]`
  rows (`get`/`pop`/`first`/`last`/`remove`, the `ret_option_*` helpers
  — `ret_option_ref_or_val_elem`/`_val`, `ret_option_elem`, `ret_option_val` — and the
  `typecheck.rs` get/pop arms) feed the Tier-1 `EK_OPTION` / `try_lift_option_ref` lift. They must stay
  byte-equivalent through the migration (`bootstrap_fixed_point` + `self_host_runtime` are the canaries).
- **`returns_fresh` ↔ `RuntimeSig.returns_fresh` sync**: the unit test
  `method_returns_fresh_matches_runtime_returns_fresh` (`builtins.rs:1126`) locks these; keep it green.

## 7. Divergence reconciliation (per-method decisions)
The two tables are NOT congruent (scout-confirmed). For EACH divergent method, the executor decides:
intentional (inline-codegen-only / user-space equip / primitive-arm) vs. latent bug to fix.
- Reg2-only families (no checker arm): `Deque, Channel, RWLock, Read/WriteGuard, Thread, Heap,
  AtomicInt/Bool, Barrier, WaitGroup, Semaphore, OnceFlag, Callable*`.
- Reg1-only families (no protocol): `Box, uint8, File, Socket, Arena, ArenaCheckpoint, Tlsf*/FixedBuffer*`.
- Per-method drift: Vector ±13 (`clone/count/fill/find/flat_map/fold/map/reduce/swap/swap_remove/zip`),
  String ±37, Dict/Set ±6-9. The unified table becomes the reconciled superset; document every
  intentional asymmetry as a typed field, not a silent omission.

## 8. Final step — flip the totality gate (only after §3-7 land + prove superset)
- Widen the `has_inherent_only_impls && !is_auto_derivable` gate (`typecheck.rs:~1978-1983`) so an
  unresolved instance/static method emits `NoMethodFound` instead of defaulting to `error_id` (the two
  default sites, `~:2015`/`~:2018` as-of now — re-pin by content per the staleness note at top).
- **b1 guard**: receiver type `error_id`/`Var(_)` → stay silent (upstream inference already failed;
  don't pile on a spurious "no method").
- **`via`**: either resolve `via`-forwarded methods first, or keep them exempt (the safe interim).
- **Arity** (separable, can ship independently): give the decls arity and convert the
  `lir/validate.rs` arity panic (message `:253`, surfaced `:137`) into a clean `check`-time
  `WrongArgCount` (already wired for resolved methods, `typecheck.rs:~1846`).

## 9. Staging (each stage independently green + canary-gated)
0. Hoist `resources`/`resource_schema` to `crate::resources` (pure move).
1. Land `builtin_method_decl_port.md` (the self-host `BUILTIN_METHODS` table + `BuiltinRetKind`).
2. Extend the table's schema to the full `TypeShape` DSL + runtime/CoW fields (§5); bump `SCHEMA_VERSION`.
3. Rewire ALL IR-side consumers to read the table via the IR interpreter, then retire
   `ALL_PROTOCOLS`/`BuiltinMethodDecl`. **Map the consumer set with `grep -rn 'builtins::' src/`** (NOT
   `lowering::builtins::` — that misses the `super::builtins::` form). ⚠ The grep does NOT surface the
   `ctx.`-METHOD indirections — trace those through `Context`'s own method defs
   (`context.rs:515/633/654/774`). Full external surface (re-verify on the executor's checkout):
   `context.rs:515` (`register_builtin_method_sigs`), `:633` (`register_builtin_runtime_callees`),
   `:654` + `closures.rs:1159` + `methods.rs:2158` (`resolve_builtin_method_return_type`),
   `:774` + `methods.rs:2644` (`builtin_returns_view`), `context.rs:3233` (`ensure_collection_type`,
   fn at `:3216`, reads protocol metadata via `super::builtins::lookup_protocol`); the TypeDef-metadata
   writers `types.rs:279-356/806/910` + `mod.rs:394-457`; the dispatch readers `methods.rs:1623/1627/1738/1778`
   (`is_mutating`/`is_mut_borrow`/`is_by_value`); `lir/lower/types.rs:222` (`c_runtime_alias_for_mangled_name`);
   and the borrow checker `safety/check_expr.rs:322` (`is_mutating_builtin_method` — a SEMANTIC-layer Reg2
   consumer the unified table naturally serves). ALSO retire the `pub protocol: &'static
   builtins::BuiltinTypeProtocol` field coupling on the `DeferredBuiltin` struct (`types.rs:14`). (NOTE:
   `lir/lower/operands.rs:255` already reads `resources::table()`/`CollectionKind` — it's on the resources
   model already, NOT a Reg2 consumer to rewire.)
4. Rewire the semantic side: replace `builtin_method_type` (`typecheck.rs:4864`) with the semantic
   interpreter over the same table. Reconcile divergence (§7).
5. b1 guard + (optional) `via` resolution + (optional/separable) arity.
6. Flip the gate → `NoMethodFound` totality. Add fixtures for the headline negatives
   (`int.frobnicate()`, `String.from`, `"x".no_such()`, wrong arity) erroring at `gg check`.

## 10. Validation gates / canaries (every stage)
`cargo test --lib`; whole-corpus `gg check` A/B diff (ZERO new failures until the gate-flip stage,
then exactly the intended negatives); the self-host canaries `self_host_bootstrap_fixed_point`,
`self_host_runtime`, `lexer/parser/type/check/lowerer/c_emit _comparison` (read printed counts);
`method_returns_fresh_matches_runtime_returns_fresh`. The self-host comparison/runtime tests are the
true superset-failure detector (a missed method → `String.clone`-class red).

## 11. Risks & open questions
- **Size**: ~30 families × methods; the divergence reconciliation (§7) is the bulk. Multi-session.
- **Closure-HOF return typing stays out of the registry** — confirm `infer_closure_method_type` fully
  covers every HOF before tagging them `ResolvedElsewhere`.
- **Open**: do primitives + stdlib-runtime-types (Box/File/Socket/Arena) get folded (adds protocol
  coverage) or stay checker-special-cased? (Recommend: fold incrementally, last, behind the gate-flip.)
- **Open**: exact `TypeShape` variant set — derive from the union inventory; the `builtin_method_decl_port.md`
  `BuiltinRetKind` is the seed but lacks `Key/Val/StringView/OptOwned/VecOfTuple/Weak/Guard/Shared`.

## 12. Key files
`compiler/data/{schema,resources}.gg`; `src/compiler_data.rs:12`; `src/ir/resources.rs` (`table`/walker/
version guard); `src/ir/resource_schema.rs`; `src/semantic/typecheck.rs:4742-5237` (`resolve_static_method_type`,
`builtin_method_type`, `infer_closure_method_type@4413`, the dispatch fork `1732-2010`, the gate `~1978-1983`,
defaults `~2015/2018`); `src/ir/lowering/builtins.rs` (protocols + thunks + `ALL_PROTOCOLS@977`);
`src/ir/lowering/context.rs:515,633,654,774` (the IR consumers); `src/ir/lowering/types.rs:279-356,806,910`
+ `mod.rs:394-457` (metadata writers); `src/ir/lowering/exprs/methods.rs:1623-1778,2120-2160` (dispatch);
`src/semantic/safety/check_expr.rs:322` (borrow checker, already a Reg2 consumer); `src/lir/runtime.rs:172`
(`RuntimeSig.returns_fresh` sync); `src/semantic/traits.rs:157-303,505-590` (resolve_method/auto-traits/via);
`docs/plans/builtin_method_decl_port.md` (Phase 2a seed); `docs/devbook/18-runtime-abi.md:202-274`.
