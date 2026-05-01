# Compiler Internals

Contributor-facing documentation for the Gorget compiler implementation.

## Documents

| Document | Covers | Key Files |
|----------|--------|-----------|
| [Layering Discipline](layering-discipline.md) | Cross-layer rules: lossless on invariants, typed metadata, single source of truth | All IR layers |
| [Unified Resource Model](unified-resource-model.md) | Phases A/B/C/D: type-axis + local-axis ownership consolidation | `src/ir/`, `src/lir/`, runtime |
| [Safety Checker](safety-checker.md) | Ownership, lifetimes, concurrency checks (Pass 5) | `src/semantic/safety/` |
| [Ownership IR](ownership-ir.md) | GIR ownership semantics: AssignMode, FieldLoadMode, ArgOwnership | `src/ir/instructions.rs`, `src/ir/lowering/` |
| [LIR Design](lir-design.md) | SSA-based low-level IR, optimization, backend trait | `src/lir/`, `src/backend/c_lir/` |
| [Shared Keyword](shared-keyword-design.md) | `shared` keyword, token semantics, CFA | `src/semantic/safety/helpers.rs` |
| [Stdlib Design](stdlib-design.md) | Narrow waist architecture, trait layering, API consolidation | `src/semantic/typecheck.rs`, `docs/` |

## Pipeline Overview

```
.gg source
  |
  v
Lexer          src/lexer/           Logos tokenizer + indentation tracking
  |
  v
Parser         src/parser/          Recursive descent + Pratt expressions
  |
  v
Semantic       src/semantic/
  Pass 0         meta.rs            Compile-time evaluation (meta if/for/const)
  Pass 0.5       derive.rs          @derive attribute expansion
  Pass 1         resolve.rs         Name resolution, scope building
  Pass 2         resolve.rs         Body resolution, resolution_map
  Pass 2.5       rewrite.rs         Struct call -> struct literal rewrite
  Pass 3         traits.rs          Trait registry, impl validation
  Pass 4         typecheck.rs       Type inference, exhaustiveness, method resolution
  Pass 4.5       provenance.rs      String provenance inference (view vs owned)
  Pass 5         safety/            Ownership, lifetimes, concurrency  <-- documented
  |
  v
IR Lowering    src/ir/              GIR: monomorphization, drop insertion, closures
  |
  v
LIR            src/lir/             SSA lowering, optimization           <-- documented
  |
  v
Backend        src/backend/c_lir/   LIR -> C code generation
  |
  v
cc -> binary
```
