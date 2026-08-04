# Appendix B — Glossary

This appendix defines the load-bearing terms used throughout the book — the IR
layer names, the ownership vocabulary, the runtime-ABI conventions, and the
self-host test apparatus. Each entry is one crisp sentence plus a `path:line`
anchor you can verify, and a cross-link to the chapter that treats it in depth.
Definitions are owned here in summary form only; the cited chapter and source
remain authoritative for the live facts (per [Chapter 0](00-how-to-read.md)).

Entries are grouped by subsystem, then alphabetical within a group.

---

## The IR layers

The compile path is `.gg → tokens → AST → GIR → LIR → BIR → backend`. Each layer
*resolves* abstractions and *adds* information without dropping a semantic
invariant ([Chapter 24](24-layering-discipline.md)).

### AST

The recursive-descent parser's output: a syntax tree that still carries source
spans and unresolved names. It is the input to semantic analysis; the lowering
passes consume the *resolved + type-checked* AST. See
[Chapter 4](04-parser-ast.md).

### GIR

**Gorget IR** — the mid-level, control-flow-explicit IR that sits between the
type-checked AST and LIR; a `Module` of `Function`s, each a CFG of basic blocks
over a flat `Vec<Local>` slot array, built by `lower_module`
([`src/ir/lowering/mod.rs:77`](../../src/ir/lowering/mod.rs)). GIR *decides
semantics*: it does [monomorphization](#monomorphization), resolves
methods/traits to free functions, lifts closures, and decides **where** drops go
and **whether** an assignment is a borrow/clone/move
([Chapter 12](12-gir-lowering.md), [Chapter 13](13-ownership-in-ir.md)).

### LIR

**Low-Level Intermediate Representation** — the [SSA](#ssa)-form IR between GIR
and the backends, where everything the old GIR→C backend did implicitly (drop
glue, vtable dispatch, closure dispatch, coercions, collection-method inlining,
printf formatting) is made *explicit* as an instruction so backends are thin 1:1
translators; the
data structures (`LirType`/`Inst`/`Term`) live in
[`src/lir/mod.rs`](../../src/lir/mod.rs). LIR *decides mechanics* and is
**ownership-unaware** — borrow checking and drop placement are already done by
the time it exists ([Chapter 14](14-lir-ssa.md)).

### BIR

**Backend IR** — the final layer before machine-code emission, a *newtype
wrapper* over `LirModule` (`BirModule(LirModule)`,
[`src/bir/mod.rs:72`](../../src/bir/mod.rs)) whose construction guarantees every
[canonical op](#canonical-op) has been expanded into primitives; the `Backend`
trait's emit entry point takes `&BirModule`
([`src/backend/mod.rs:382`](../../src/backend/mod.rs)), so the Rust type system
makes it impossible to hand a backend unlowered LIR
([Chapter 16](16-bir.md)).

### SSA

**Static Single Assignment** — the form LIR is promoted into: each scalar slot
becomes a value assigned exactly once, with block parameters at control-flow
merges — the Cranelift model, not LLVM-style φ-nodes ([Chapter 14](14-lir-ssa.md)
calls this out as a load-bearing implementation fact). The
construction is a simplified Braun et al. 2013 algorithm
([`src/lir/ssa.rs:4`](../../src/lir/ssa.rs)), run per function and validated for
dominance ([Chapter 14](14-lir-ssa.md)).

### Canonical op

A high-level `Inst` variant LIR is allowed to carry but **no backend** is allowed
to see (`SizeOf`, `EnumInit`, …); the single source of truth for "what is
canonical" is the BIR validator's rejection list
([`src/bir/validate.rs`](../../src/bir/validate.rs), surveyed at
[`16-bir.md`](16-bir.md)). BIR lowering expands each into primitives, and
`assert_primitives_only` halts the build if any survive.

### Monomorphization

The GIR-lowering step that collects generic templates, discovers the concrete
type-argument instantiations actually used, and generates a distinct
non-generic `TypeDef`/`Function` per instantiation — looping to a fixed point so
that generics instantiated *by* other generics are caught
([`12-gir-lowering.md:87`](12-gir-lowering.md)). After it runs, no generics
remain in the IR; this is the "abstractions evaporate" half of layering
discipline.

### Drop elaboration

The tail LIR pass that turns conservatively-emitted, always-conditional resource
drops into the cheapest correct form — *deleting* a drop a forward init/uninit
dataflow proves dead, *stripping the guard* off one it proves live, and falling
back to a stack `bool` drop-flag only when initialization is genuinely
path-dependent ([`src/lir/drop_elab.rs`](../../src/lir/drop_elab.rs), scheduled
by `optimize_module` at
[`src/lir/optimize.rs:78`](../../src/lir/optimize.rs)). It is a *removal* pass:
GIR emits every scope-exit resource drop defensively as a runtime-checked
guard, and this pass eliminates the check wherever dataflow can
([Chapter 15](15-drop-elaboration.md)).

### Structural guard

A *writer-side validator* that turns a soundness invariant into a build-halting
assertion: a pure structural walk over an IR/LIR module after lowering that
reports nothing when the invariant holds and (once the class is migrated) fails
the build on the first violation — one validator file per IR layer
(`src/ir/validate.rs`, `src/lir/validate.rs`, `src/bir/validate.rs`)
([Chapter 25](25-structural-guards.md)). The bar: *every soundness invariant is
a writer-side static guard; every bug fixed leaves a permanent counterexample
and a validator that locks the class shut.*

### RuntimeFn

The typed enum naming every C runtime function, declared lockstep with its
signature registry by the `runtime_table!` macro so the `as usize` ordinal
matches the registry index ([`src/lir/runtime.rs:208,218`](../../src/lir/runtime.rs));
it is the *typed* replacement for name-matching on runtime symbols — `Inst::CallRuntime`
carries a `RuntimeFn`, and `from_c_name` is the one canonical string→enum path
([`src/lir/runtime.rs:5-8`](../../src/lir/runtime.rs), [Chapter 18](18-runtime-abi.md)).

---

## Ownership & copy-on-write

These terms describe how Gorget gets Rust-like single-ownership *without* a
borrow lattice in the IR ([Chapter 11](11-copy-on-write.md),
[Chapter 13](13-ownership-in-ir.md)).

### CoW (copy-on-write)

Gorget's ownership model: every assignment, argument pass, and collection read
defaults to a zero-cost **borrow** (a `Ptr` alias), and a **clone** is inserted
only at the handful of points where a borrowed value crosses into something that
must *own* it — *everything is a reference until ownership is demanded*. All
decisions are made at compile time during lowering; there is no reference
counting and no `provenance.rs` ([Chapter 11](11-copy-on-write.md), and the
"Ownership at Consuming Positions" contract in `CLAUDE.md`).

### The consuming position

A site that *demands ownership* — collection `push`/`put`/`set`/`insert`/`send`
and index-assign sugar `v[i] = x` — where the destination must own, so the
compiler picks per-argument move-vs-clone via the canonical decision tree in
`ensure_owned_at_consuming_arg`
([`src/ir/lowering/context.rs:1927`](../../src/ir/lowering/context.rs),
[`13-ownership-in-ir.md:345`](13-ownership-in-ir.md)). Note bare call args are
deliberately *not* routed through this helper — they stay borrows
([`13-ownership-in-ir.md:339`](13-ownership-in-ir.md)).

### MoveZero

The GIR instruction (`MoveZero { place }`,
[`src/ir/instructions.rs:306`](../../src/ir/instructions.rs)) that marks a
source slot logically dead after a valid move; the backend zeros the slot only
when drop-tracking would otherwise re-drop the value, and elides the zero when
liveness proves it unobservable — the zero is a backend optimization for drop
correctness, not part of the move semantics ([Chapter 13](13-ownership-in-ir.md)).

### Provenance

The recorded *origin* of a borrow alias, used so a later mutation of the source
can sever the alias (and so the safety pass can flag returns of local
references). There is **no provenance pass** and no `src/semantic/provenance.rs`
([`00-how-to-read.md`](00-how-to-read.md)); the machinery lives in the GIR
lowering context and, separately, in the diagnostic [BorrowOrigin](#borroworigin)
tracker.

### BorrowOrigin

The borrow-origin tracker — and a *trap*: two unrelated types share the name.
The **safety** one (`src/semantic/safety/mod.rs:68`) is purely diagnostic,
tracking origins for use-after-move / return-of-local-reference /
borrow-across-await errors; the **IR** one (`src/ir/mod.rs:582`) lives inside
`LocalOwnership` and feeds the CoW alias machinery
([`11-copy-on-write.md:38`](11-copy-on-write.md)).

### View (cap == 0)

The non-owning state of a resource struct, signalled by a `cap` field of zero at
the shared offset +8: `cap == 0` ⟺ the struct's `data` *borrows* a buffer it
does not own (a `.rodata` literal or a slice into another buffer) and its drop
is a no-op; `cap > 0` ⟺ owned, and drop frees `data`
([`src/backend/c/runtime/runtime_string.c:7`](../../src/backend/c/runtime/runtime_string.c),
[`18-runtime-abi.md:54`](18-runtime-abi.md)). This is the runtime's only
participation in CoW — a cheap discriminator check, with no reference counting in
the CoW path. (The single-owner concurrency types — `Channel`/`Shared`/`Weak` —
do use atomic refcounting in the runtime, but they sit *outside* CoW.)

### Narrow waist

Two related uses. (1) The **runtime-ABI** narrow waist: only a handful of named
structs (`Str`, `Array`, `Map`, `Set` — the `CRuntimeType` enum,
[`src/lir/runtime.rs:42`](../../src/lir/runtime.rs)) cross the runtime boundary;
the rest of the `LirType` lattice never reaches the runtime ABI
([`18-runtime-abi.md:40`](18-runtime-abi.md)). (2) The **stdlib** narrow waist:
a small set of `Iterator`/`Writer`/`Reader`-style primitive traits that broader
convenience APIs are built on top of ([Chapter 23](23-stdlib.md)).

---

## The self-host & its tests

The "north star" is self-host feature parity with the Rust `gg` (see
`MEMORY.md`). [Chapter 26](26-self-host-frontend.md) and
[Chapter 27](27-comparison-bootstrap.md) give the system-level treatment;
per-chapter "In the self-host" sections give the area-level mirror.

### The self-host

The Gorget compiler frontend re-implemented *in Gorget*, living under
`tests/fixtures/self_host_*` (`self_host_lexer`, `self_host_parser`,
`self_host_resolver`, `self_host_typechecker`, `self_host_check`,
`self_host_lowerer`). It serves three roles at once — a compiler stress test, a
regression net, and the language's elegance showcase (it must be idiomatic
Gorget, not workaround-shaped; see `CLAUDE.md` "Self-host as the elegance
showcase"). It reaches C emission too: `self_host_lowerer` lowers GIR → LIR and
emits C via `lir_codegen.gg` (`--lir-c`), measured by `c_emit_comparison` and
exercised end-to-end by [bootstrap fixed-point](#bootstrap-fixed-point). Only the
LLVM backend has no self-host coverage.

### Comparison test

A `*_comparison` integration test that runs both the Rust `gg` and the self-host
on every fixture and prints the matched/mismatched counts — the parity meter.
The drivers are `lexer_comparison`, `parser_comparison`, `resolver_comparison`,
`type_comparison`, `check_comparison`, `lowerer_comparison`, and
`c_emit_comparison`
([`tests/integration.rs:9346,12406,12683,12997,13193,13390,13549`](../../tests/integration.rs)).
**Crucial:** these are *diagnostic-always-pass* — the counts are `eprintln!`'d,
not asserted — so a green `cargo test` says nothing about parity. State parity
as a **procedure** (`cargo test --test integration <name>_comparison --
--nocapture` and read the count), never as a frozen number
([`00-how-to-read.md`](00-how-to-read.md)).

### Bootstrap fixed-point

The `self_host_bootstrap_fixed_point` test
([`tests/integration.rs:13897`](../../tests/integration.rs)), which proves the
self-host reproduces *itself* — stage-0 → stage-1 C output, then chained
stages, byte-identical at the fixed point. It is a closed-loop *milestone* (the
self-host compiling the self-host), **not** parity with Rust `gg`; parity is the
comparison-test counts climbing toward 100% (see `MEMORY.md` north star).

---

*Appendix. Synthesized from the chapters above and re-derived from source at
authoring time; verify every `path:line` against current source per
[Chapter 0](00-how-to-read.md).*
