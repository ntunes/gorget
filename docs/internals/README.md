# Compiler Internals

Contributor-facing documentation for the Gorget compiler implementation.

## Documents

| Document | Covers | Key Files |
|----------|--------|-----------|
| [Safety Checker](safety-checker.md) | Ownership, lifetimes, concurrency checks (Pass 5) | `src/semantic/safety/` |
| [LIR Design](lir-design.md) | SSA-based low-level IR, optimization, backend trait | `src/lir/`, `src/backend/c_lir/` |
| [Shared Keyword](shared-keyword-design.md) | `shared` keyword, token semantics, CFA | `src/semantic/safety/helpers.rs` |

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
