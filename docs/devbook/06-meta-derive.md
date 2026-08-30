# Chapter 06 — Meta & derive (Pass 0 / 0.5)

This chapter documents the two earliest semantic passes: **meta evaluation**
(compile-time `meta` constants, type aliases, conditional compilation, and the
`field_value`/`field_set` rewrites) and **`@derive` expansion** (auto-generated
trait `equip` blocks). Both run at the very top of `analyze_with_source_dir` in
`src/semantic/mod.rs:107-115`, before name resolution and type checking — meta
first, then derive. The implementations live in `src/semantic/meta.rs` (~4900
lines) and `src/semantic/derive.rs` (~1790 lines).

A third piece — **delayed meta evaluation** — runs much later, during GIR
monomorphization, because it needs concrete type-parameter bindings. It shares
the same evaluator and `MetaValue` representation but is dispatched from IR
lowering (`src/ir/lowering/functions.rs`, `src/ir/lowering/traits.rs`). It is
covered in [§Delayed meta](#delayed-meta-monomorphization-time).

The design philosophy ("one keyword, types are first-class compile-time values,
complements generics rather than replacing them") is folded into this chapter;
the user-facing surface is in
`docs/language-reference.md`. This chapter is about *how the passes work*.

---

## What the meta pass guarantees downstream

After `evaluate_meta_consts` returns, the rest of the compiler sees **no
top-level meta constructs at all**. The module-level pass erases every
`MetaConst`, `MetaAssert`, `MetaLog`, `MetaType`, `MetaTypeFunc`, `MetaIf`, and
`TypeAlias` item (`remove_meta_and_alias_items`,
`src/semantic/meta.rs:548-562`), having first substituted their effects into the
surviving AST. The only meta constructs that survive into later passes are the
*in-body* delayed forms (`Stmt::MetaIf`, `Stmt::MetaFor`, `Stmt::MetaMatch`,
`Stmt::MetaWhile`, `Stmt::MetaConst`, `MatchItem::MetaFor`) inside generic
function/method bodies — those are deliberately left for monomorphization.

## MetaValue — the compile-time value domain

Compile-time evaluation produces a `MetaValue`
(`src/semantic/meta.rs:29-38`):

```rust
pub enum MetaValue {
    Int(i64), Float(f64), Bool(bool), Str(String),
    List(Vec<MetaValue>),
    Op(crate::parser::ast::BinaryOp),   // compile-time operator token (meta op)
}
```

There is no heap-collection, struct, or closure value — the evaluator is a
deliberately small interpreter. `List` exists only to carry reflection results
(`field_names(T)`, `variant_payloads(T)`, …) into `meta for` iteration; it is
**not** representable as a single AST expression, so substituting a `List` into
expression position panics (`meta_value_to_expr`,
`src/semantic/meta.rs:2569-2573`). `Op` is consumed by `MetaOpInfix`
substitution and likewise never stands alone (`src/semantic/meta.rs:2574-2577`).

Two hard limits bound the interpreter against runaway compile-time programs:
`MAX_META_RECURSION = 256` and `MAX_META_ITERATIONS = 100_000`
(`src/semantic/meta.rs:40-41`), checked in `eval_meta_fn_call` and every loop
form.

## Pass 0 phase structure

`evaluate_meta_consts_impl` (`src/semantic/meta.rs:460-543`) runs a fixed
sequence of phases over `module.items`. The phase numbering in the code is
non-contiguous (1, 1.5, 1.75, 2, 2.5, 3) because phases were inserted between
existing ones over time; the order is what matters:

1. **Phase 1 — evaluate declarations.** Walk `module.items` top-to-bottom
   (`process_meta_item`, `src/semantic/meta.rs:597-666`). Each `MetaConst` is
   evaluated to a `MetaValue` and type-checked against its declared type
   (`validate_type`, `src/semantic/meta.rs:1827`), then bound in `env`. Each
   `MetaType` resolves its RHS into `type_env`; each `MetaTypeFunc` is stored in
   `type_func_env`; `MetaAssert` is checked inline (failure → a
   `MetaEvalError`); `MetaLog` prints to stderr. Then `collect_type_aliases`
   (`src/semantic/meta.rs:568-594`) folds plain `type X = …` aliases into
   `type_env` and generic ones (`type X[T] = …`) into `generic_aliases`.

2. **Phase 1.5 — flatten `meta if`.** `flatten_meta_ifs`
   (`src/semantic/meta.rs:865-903`) evaluates each `MetaIf` condition, splices
   the winning branch's items in place, and processes any meta declarations the
   winning branch introduced. It loops until no `MetaIf` remains (nested
   meta-ifs in expanded branches) and recurses into `Item::Module` so imported
   modules' meta-ifs are flattened too. **Dead branches are discarded entirely**
   — they never reach resolve or type-check, so a pruned branch may reference
   unimported names or unsupported platform APIs without error.

3. **Phase 1.75 — constructor fixups.** For aliases whose underlying type is a
   `Type::Named` (e.g. `type IntList = Vector[int]`), rewrite *call*
   expressions: `IntList()` → `Vector[int]()`
   (`fixup_constructor_calls_in_item`, `src/semantic/meta.rs:102-261`). This
   runs before Phase 2 because plain identifier substitution cannot add generic
   args. A non-generic struct alias (`type Handle = SlotKey`) is rewritten to
   `SlotKey(...)` with `generic_args = None` so the result is byte-identical to a
   direct constructor call (`src/semantic/meta.rs:184-201`).

4. **Phase 2 — substitute.** `substitute_item` (`src/semantic/meta.rs:1907`)
   walks the whole AST replacing meta-const identifier references with their
   literal value (`meta_value_to_expr`), meta-type-alias names in every type
   annotation (`substitute_type`, `src/semantic/meta.rs:1867`), and meta-string
   variables inside string-interpolation segments. This phase is also where the
   `field_value`/`field_set`/`make_variant` rewrites fire (see below).

5. **Phase 2.5 — expand generic aliases.** `expand_generic_aliases_in_item`
   (`src/semantic/meta.rs:264-378`) substitutes parameterised aliases
   (`type StringMap[V] = Dict[String, V]`) by binding the alias's type params to
   the use-site args (`substitute_alias_params`, `src/semantic/meta.rs:381`),
   recursing in case substitution exposes further aliases.

6. **Phase 3 — remove.** Strip all meta items and `TypeAlias` items, recursing
   one level into imported `Item::Module` wrappers
   (`remove_meta_and_alias_items`, `src/semantic/meta.rs:548`).

### Why Phase 1.5 / 3 recurse into `Item::Module`

`loader::merge_modules` wraps each imported module's items in one
`Item::Module` node. The recursion in `collect_type_aliases`,
`flatten_meta_ifs`, and `remove_meta_and_alias_items` exists so that a `type
Entity = SlotKey` declared in an *imported* module is collected, its uses
rewritten, and its declaration erased. Without the Phase-3 recursion the
declaration would survive into resolve as an opaque `DefKind::TypeAlias` with no
struct body — the bug the in-code comments call "Bug B"
(`src/semantic/meta.rs:535-540`).

## The expression evaluator

`eval_expr` (`src/semantic/meta.rs:960-1152`) is the core interpreter. It
handles literals, identifier lookup against `env`, unary/binary ops
(`eval_binary_op`, `src/semantic/meta.rs:1270`), expression-position `if`
(ternary-style), and a fixed set of **built-in functions** dispatched by callee
name (`src/semantic/meta.rs:1012-1124`):

| Built-in | Result | Notes |
|---|---|---|
| `platform()` | `Str` | `cfg!(target_os=…)` → `"macos"`/`"linux"`/`"windows"` |
| `arch()` | `Str` | `cfg!(target_arch=…)` |
| `arch_word_bits()` | `Int` | `size_of::<usize>() * 8` |
| `feature(s)` | `Bool` | membership test against `ctx.features` (`--feature` CLI args) |
| `debug()` | `Bool` | shorthand for `feature("debug")` |
| `sizeof(T)` / `alignof(T)` | `Int` | primitive/built-in types only (`meta_type_byte_size`/`meta_type_align_bytes`, `src/semantic/meta.rs:1235`/`1254`) |
| `typename(T)` | `Str` | normalizes `"str"` → `"String"` |
| `embed_file(path)` | `Str` | reads file relative to `ctx.source_dir` |

Anything else falls through to **M7 user-function evaluation**
(`src/semantic/meta.rs:1107-1119`): `lookup_meta_function`
(`src/semantic/meta.rs:1410`) scans `ctx.items` for a non-generic, non-async,
function whose params and return are meta-compatible primitives
(`is_meta_compatible_type`, `src/semantic/meta.rs:1384`), and
`eval_meta_fn_call` (`src/semantic/meta.rs:1467`) interprets its body. Functions
are *not* marked `meta`; any pure primitive-typed function is callable at
compile time. The body interpreter (`eval_meta_stmt`,
`src/semantic/meta.rs:1555`) supports `if/elif/else`, `while`, `loop`, integer
`for`, local bindings, assignment, compound assignment, `assert`, and `return`,
propagating control flow through a small `MetaControlFlow` enum
(`src/semantic/meta.rs:75-84`). Unsupported statements (match, throw, with,
select, …) are rejected with a `MetaEvalError`.

`sizeof`/`alignof` return primitive sizes for Gorget's single 64-bit target;
the unified `String` is 32 bytes (`{*u8, u64, u64, *Alloc}`) and `cstr` is 8
(`src/semantic/meta.rs:1245-1248`). These tables are *not* the authoritative
layout — they exist because meta runs before layout computation, so they cover
only primitives.

## Meta type functions (`meta type f(...)`)

`meta type` aliases come in three RHS shapes (`MetaTypeRhs`,
`src/parser/ast.rs:1176`): `Plain(ty)`, `Conditional { then, cond, else }`, and
`Call { callee, args }`. `resolve_meta_type_rhs`
(`src/semantic/meta.rs:673-701`) dispatches: a conditional evaluates its
`bool` condition and picks a branch; a call looks the function up in
`type_func_env` and interprets it. `eval_meta_type_body`
(`src/semantic/meta.rs:754-810`) is a *type-returning* interpreter — it walks
`if/elif/else` and `return <type>` statements, mapping the returned expression
back to a `Type` via `resolve_expr_as_type` (`src/semantic/meta.rs:814-857`,
which recognises primitive keywords and falls back to a named user type). This
is how `meta type Word = sized_int(arch_word_bits())` resolves to a concrete
integer type at compile time.

### Why type application keeps its own brackets

Because `meta type` makes types first-class compile-time values, it is tempting
to conclude that type arguments and value arguments should share one set of
parentheses. They deliberately do not. Collapsing them makes the boundary
unreadable at exactly the sites where it matters most:

```
# Hypothetical — types in parens alongside values
Vector(int) items = Vector(int)()   # two paren groups, neither self-evident
Pair(int, String, 10, "hello")      # where do the types stop?
max(int, 3, 5)                      # is `int` a type, or a variable named int?
```

Parentheses already mean "constructor or call arguments". Square brackets carry
the separate job of marking type application, and every language that keeps the
distinction — Scala's `[]`, the `<>` of C++/Kotlin/TypeScript — keeps it for
readability, not tradition. So `[]` stays: the `meta type` system **complements**
generics rather than replacing them, and a `meta type` alias resolves to an
ordinary type that is then applied with the ordinary bracket syntax.

## The `field_value` / `field_set` / `make_variant` rewrites

These are **compile-time AST rewrites**, not runtime functions. They let
reflection-driven code (typically generated inside a `meta for` over
`field_names(T)`) read and write struct fields and construct enum variants by
*name string*. After substitution has turned the name argument into a plain
string literal, the meta pass rewrites the call into a direct field
access / assignment / path:

- **`field_value(obj, "f")` → `obj.f`** — an expression rewrite in
  `substitute_expr` (`src/semantic/meta.rs:2503-2525`). Guard: the second arg
  must be a non-interpolated string literal with a non-empty name.
- **`field_set(obj, "f", v)` → `obj.f = v`** — a *statement* rewrite in
  `substitute_stmt` for `Stmt::Expr` (`src/semantic/meta.rs:2146-2172`); it
  replaces the whole expression statement with a `Stmt::Assign`.
- **`make_variant(T, "Variant")` → `Expr::Path ["T", "Variant"]`** — an
  expression rewrite (`src/semantic/meta.rs:2527-2553`) building a qualified
  enum-constructor path.

Because the rewrite only fires once the name argument is a literal, these
calls are *errors* if reached in a pure compile-time `meta const` position —
the delayed evaluator returns explicit "use it in a runtime statement"
diagnostics for `field_value`/`field_set`/`make_variant`
(`src/semantic/meta.rs:3188-3210`). They are runtime operations whose *target
name* is fixed at compile time.

> **Layering note.** These rewrites are name-matched on the literal callee
> string (`if cname == "field_value"`), which the project's "no name matching"
> rule normally forbids. They are tolerated because the name *is* the meta
> builtin's spelling at this stage — there is no earlier typed declaration to
> hang a flag on (the call is synthesised by user `meta for` code), and the
> rewrite is purely syntactic. Treat this as the meta-builtin analogue of the
> C-emit runtime-symbol exception, not a license to name-match elsewhere.

## Delayed meta (monomorphization time)

`meta if`/`meta for`/`meta match`/`meta while`/`meta const` written **inside a
function or method body** cannot be evaluated at Pass 0 because they typically
depend on a generic type parameter (`meta if T is numeric:`). They are parsed
as `Stmt::Meta*` nodes (`src/parser/ast.rs:1047-1095`) and left untouched by
Pass 0 — `eval_meta_stmt` explicitly errors if it sees one
(`src/semantic/meta.rs:1814-1819`).

These are expanded by `evaluate_delayed_meta_block`
(`src/semantic/meta.rs:3296`), called from GIR lowering once per monomorphized
instantiation with the concrete type bindings in a `DelayedMetaContext`
(`src/semantic/meta.rs:2648-2661`):

```rust
pub struct DelayedMetaContext<'a> {
    pub type_subs: &'a [(String, Type)],   // e.g. [("T", int)]
    pub features: &'a [String],
    pub meta_env: &'a FxHashMap<String, MetaValue>,  // Phase-0 consts
    pub items: &'a [Spanned<Item>],
    pub trait_registry: &'a TraitRegistry,
    pub type_registry: &'a TypeRegistry,
}
```

Call sites (the `evaluate_delayed_meta_block` invocations): non-generic fn
bodies and generic instantiations in
`src/ir/lowering/functions.rs:767,1068,1173,1736` and equip-method bodies in
`src/ir/lowering/traits.rs:1375,1605`. Not every site guards the same way. The
three primary fn-body sites
(`functions.rs:754→767`, `1055→1068`, `1723→1736`) first call
`block_has_delayed_meta` (`src/semantic/meta.rs:3237`) — a read-only,
early-exit AST scan — to skip the upfront `block.clone()` when there is no meta
work to do. The generic-template site (`functions.rs:1173`) instead gates on
`subs.is_empty() && meta_env_map.is_empty()` (`functions.rs:1159`): a template
with no type substitutions and no meta-op bindings is reused as-is. The
static-trait sites (`traits.rs:1375`, `1605`) clone and evaluate
unconditionally.

`evaluate_delayed_meta_block` modifies `block.stmts` in place: it evaluates each
`MetaIf` condition and splices the winning branch; unrolls `MetaFor` over an
integer range *or* a reflection `List` (binding loop vars and substituting them
into the cloned body each iteration, `src/semantic/meta.rs:3399-3477`);
evaluates `MetaMatch` by value equality (`meta_values_eq`,
`src/semantic/meta.rs:2706`); unrolls `MetaWhile`; binds `MetaConst` and
substitutes it into the remaining statements; and recurses into the sub-blocks of
nested control-flow and container statements — `if`/`elif`/`else`, `while`/`for`
(incl. their `else`), `loop`, `with`, `named scope`, `match` (arm bodies
+ `else`, after expanding any `MetaFor` arms), `select` (arm bodies + `else`), and
nested `meta match`/`meta while` (`recurse_delayed_meta_in_stmt`,
`src/semantic/meta.rs:3651`). It deliberately does **not** recurse into `on error`
blocks: the on-error body is cloned into `on_error_blocks` at lower-time, *after*
the delayed-meta pass runs, so a `meta if` inside an `on error` block is dropped by
both Rust and the self-host (a known shared latent gap — the gate
`block_has_delayed_meta` over-reports it via an `OnError` arm, but the pass leaves
it unevaluated). It splices
replacements without advancing the cursor so freshly-inserted statements are
re-processed (handling nesting).

`eval_delayed_expr` (`src/semantic/meta.rs:2725`) extends `eval_expr` with the
**type-aware reflection builtins** that need `type_subs` and the registries:
`typename`, `typeof`, `sizeof`, `bitwidth`, `min_val`, `max_val`,
`implements(T, "Trait")`, and the struct/enum reflection family —
`field_names`, `field_count`, `has_field`, `field_type`, `fields`,
`variant_names`, `variant_count`, `variant_payloads`, `enum_ordinal`,
`enum_from_ordinal` (`src/semantic/meta.rs:2766-3211`). It also evaluates
`T is Category` predicates (`eval_type_is_check`,
`src/semantic/meta.rs:1197`) where categories like `numeric`, `signed`,
`unsigned`, `Enum`, `Struct` match families of types and everything else is an
exact canonical-name match. `MatchItem::MetaFor` arms in a runtime `match` are
unrolled into concrete arms by `expand_match_meta_for`
(`src/semantic/meta.rs:2073`).

## `@derive` expansion (Pass 0.5)

`expand_derives` (`src/semantic/derive.rs:54`) runs immediately after meta. Its
strategy is **source generation, not AST surgery**: for each derivable trait
named in a struct's or enum's `@derive(...)` attribute, it generates a Gorget
source string for the trait's `equip` block, parses that string into items, and
appends them to the module. The recursion handles `Item::Module` wrappers so
derives in imported modules expand too (`src/semantic/derive.rs:60-83`).

The generators (`generate_struct_derive`, `src/semantic/derive.rs:307`;
`generate_enum_derive`, `src/semantic/derive.rs:865`) format trait method
bodies as text. For example `generate_struct_equatable`
(`src/semantic/derive.rs:329`) emits:

```
equip {gp}{type_name}{gs} with Equatable:
    bool eq(self, {type_name}{gs} other):
        return self.a == other.a and self.b == other.b
```

The derivable sets are fixed lists (`src/semantic/derive.rs:85-88`):
`DERIVABLE_STRUCT_TRAITS` = Equatable, Displayable, Debuggable, Cloneable,
Hashable, Serializable, Default, Deserializable, From, TryFrom, FromRow;
`DERIVABLE_ENUM_TRAITS` = Equatable, Displayable, Debuggable, Cloneable,
Hashable, Serializable, Deserializable, Ordinal. A non-derivable trait emits
`UnderivableTrait`; `From`/`TryFrom` require a single field
(`src/semantic/derive.rs:111-119`).

Almost every derive produces an `equip` block — including `From` and `TryFrom`,
whose methods return `Self` (`generate_struct_from` emits `equip … with
From[U]:` with a `{type_name} from(U value)` method, `src/semantic/derive.rs:674-682`;
`generate_struct_try_from` emits `equip … with TryFrom[U]:`,
`src/semantic/derive.rs:684-692`). The lone exception is `@derive(Deserializable)`,
which produces a free **function** `Result[T, String] deserialize_<T>(Box[Deserializer] de)`
(`generate_struct_deserializable`, `src/semantic/derive.rs:735-754`;
`generate_enum_deserializable`, `src/semantic/derive.rs:778-850`) — there is no
trait method to hang it on. So `parse_and_collect_derived_items`
(`src/semantic/derive.rs:1210-1232`) accepts both `Item::Equip` and
`Item::Function`, the latter specifically for the Deserializable case.

### Generic derives stay generic

`@derive` on a generic type emits a generic `equip` — the prefix logic in
`equip_generic_prefix` (`src/semantic/derive.rs:297`) produces
`equip [T] Box[T] with …`. Hashable is the sharp case: the derived method keeps
the `Hasher` type parameter rather than concretising it —
`void hash[Hasher H](self, H &h):` (`src/semantic/derive.rs:413-415`), so field
hashes route through trait dispatch on `H`. (Concretising it to a specific
hasher was a self-host parity bug, since fixed.)

### Span offsets and the leak

Two atomics keep generated code well-behaved. `DERIVE_SPAN_OFFSET`
(`src/semantic/derive.rs:27`, base `10_000_000`, stride `100_000`) gives each
generated source a unique parser base offset so synthetic spans never collide —
`method_resolutions` is keyed by `span.start`, and colliding derive blocks would
alias each other (`src/semantic/derive.rs:1211-1216`). `DERIVE_VAR_COUNTER`
(`src/semantic/derive.rs:496`, via `next_var`) generates fresh loop-variable
names inside serialization codegen. Field type names are `Box::leak`ed to obtain
`'static &str`s (`src/semantic/derive.rs:127-130`) — deliberate, because derive
expansion happens once at startup. If a generated source fails to parse,
`parse_and_collect_derived_items` *panics* (`src/semantic/derive.rs:1219-1225`):
that can only mean buggy codegen, not bad user input.

### Post-Pass-3 field-trait validation

`@derive` can name a trait the field types don't satisfy (`@derive(Hashable)`
on a struct with a `float` field — floats aren't hashable). Detecting that needs
the trait registry, which isn't populated until after resolution. So
`expand_derives` records a `DeriveRecord` per derive
(`src/semantic/derive.rs:17-22,135-140`) and `validate_derive_field_traits`
(`src/semantic/derive.rs:171`) runs *after Pass 3*
(`src/semantic/mod.rs:276-277`), checking each field type of a
`FIELD_REQUIRING_TRAITS` derive (Hashable, Equatable, Cloneable,
`src/semantic/derive.rs:148`) against `primitive_satisfies`, the trait registry,
and other derive records (for cross-module derives whose `equip` blocks aren't
yet registered). A miss emits `FieldMissingDerivedTrait`. This is a clean
example of the "resolve once, write through" discipline: the derive pass can't
answer the question yet, so it leaves a typed record for a later pass instead of
guessing.

## In the self-host

The self-host type checker reimplements both passes:
`tests/fixtures/self_host_typechecker/meta.gg` (~350 lines) and
`derive.gg` (~736 lines). Both files declare in their headers that they mirror
the Rust `meta.rs` / `derive.rs`.

- **meta.gg** covers the Pass-0 alias machinery the type checker needs: a
  constant-expression evaluator (`eval_int`/`eval_bool`,
  `meta.gg:21-65`), type-name extraction, and meta type-alias resolution
  (including the `MtrPlain`/`MtrConditional`/`MtrCall` RHS shapes that mirror
  Rust's `MetaTypeRhs`). It is scoped to what type-checking requires — it does
  not reimplement the full M7 user-function interpreter or the delayed-meta
  monomorphization machinery.
- **derive.gg** mirrors the source-generate-then-parse strategy exactly: it
  formats `equip` blocks as text via `parse_source` (`derive.gg:6`) and appends
  the parsed items. The struct/enum generators and the generic-suffix /
  equip-prefix helpers (`derive.gg:40-55`) parallel the Rust ones.

**Parity is a procedure, not a number.** The `*_comparison` integration tests
are diagnostic-always-pass (no assertions — a green `cargo test` says nothing).
To read current parity for the area this chapter covers, run the typechecker
comparison and read the printed matched-count:

```bash
cargo test --test integration type_comparison -- --nocapture
```

There is no dedicated `meta_comparison` or `derive_comparison` test; meta-alias
and derive behaviour is exercised through the type-checker driver — `type_comparison`
(`tests/integration.rs:12996`) builds the whole `self_host_typechecker` directory,
including its `derive.gg`, into the comparison driver — and through end-to-end
fixtures such as `meta_numeric_meta.gg` and `test_struct_derive.gg`. (Note the
standalone `derive()` test at `tests/integration.rs:2287` is a separate
end-to-end run of the top-level `tests/fixtures/derive.gg`, unrelated to the
self-host `derive.gg`.) The self-host typechecker's parser/AST are symlinked
into `self_host_lowerer`, so changes to the meta/derive AST primitives must be
mirrored across the relevant driver directories.
