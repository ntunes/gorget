# The Gorget Compiler — Internals Book ("devbook")

A reference-grade guide to **how the Gorget compiler is implemented**, for the people who work on it.

This book is for *compiler developers*, not application programmers. It assumes you can read Rust and that you've met a compiler before (you know what an AST, a pass, and an IR are). It does **not** assume you know Gorget's internals — that's what it teaches, frontend to backend. For the *language* itself, see [The Gorget Book](../book/README.md) (how to use it), [`language-reference.md`](../language-reference.md) (the normative spec), and [`language-design.md`](../language-design.md) (the why).

---

## How to read this book

**One authoritative source.** This book owns the *design narrative* — the architecture and the "why." For *live facts* (which function does what, exact ABI offsets, comparison scores, numeric budgets, `file:line`), the **code and tests are authoritative** and the book *cites* them. If a number or a line reference here disagrees with the source, the source wins and the book is stale — tell us.

**Chapters are subsystem-ordered, not strictly pass-ordered.** The pipeline order is given in [Chapter 1](01-pipeline-and-driver.md); the chapters then walk the compiler by subsystem.

**"In the self-host" sections.** Most phase chapters carry a section on how the Gorget *self-host* compiler (`tests/fixtures/self_host_*`) implements the same area, where it diverges from the Rust `gg`, and the current parity. Parity numbers are obtained by running the relevant `*_comparison` test, not quoted as fixed facts — those tests are diagnostic-only.

**Freshness.** Chapters describing fast-moving internals carry a "verified against `<commit>`" stamp. Treat an un-stamped fast-moving claim as suspect and re-verify against source.

> This book was grown by folding the former `internals/` deep-dives into chapters and repointing their source citations here; the former `internals/` tree has since been deleted.

---

## Table of Contents

### Part 0 — Orientation
0. [How to read this book](00-how-to-read.md)
1. [The pipeline & the `gg` driver](01-pipeline-and-driver.md)
2. [Foundations: spans, interning & diagnostics](02-foundations.md)

### Part I — Frontend
3. [The lexer & indentation](03-lexer.md)
4. [The parser & the AST](04-parser-ast.md)
5. [The formatter (`gg fmt`)](05-formatter.md)

### Part II — Semantic analysis
6. [Meta & derive (Pass 0 / 0.5)](06-meta-derive.md)
7. [Name resolution & scopes (Pass 1–2)](07-name-resolution.md)
8. [Traits & the impl registry (Pass 3)](08-traits.md)
9. [Type inference & checking (Pass 4)](09-type-checking.md)
10. [Ownership: moves & borrows — the safety checker (Pass 5)](10-ownership-safety.md)

### Part III — Ownership, copy-on-write & resources
11. [Copy-on-write & view provenance](11-copy-on-write.md)

### Part IV — IR lowering (GIR)
12. [GIR & lowering: monomorphization, drops, closures](12-gir-lowering.md)
13. [Ownership in the IR](13-ownership-in-ir.md)

### Part V — LIR
14. [LIR & SSA](14-lir-ssa.md)
15. [Drop elaboration & optimization](15-drop-elaboration.md)

### Part VI — BIR & backends
16. [BIR: backend-agnostic synthesis & validation](16-bir.md)
17. [The C backend](17-c-backend.md)
18. [The runtime & the backend ABI contract](18-runtime-abi.md)
19. [The LLVM backend](19-llvm-backend.md)
20. [Extern, interop & GPU backends](20-extern-gpu.md)

### Part VII — Other consumers & tooling
22. [Modules, loading & package management](22-modules-packages.md)
23. [The standard library narrow waist](23-stdlib.md)

### Part VIII — Cross-cutting laws
24. [Layering discipline](24-layering-discipline.md)
25. [Structural guards](25-structural-guards.md)

### Part IX — The self-host
26. [The self-host frontend](26-self-host-frontend.md)
27. [Comparison, bootstrap & report generation](27-comparison-bootstrap.md)

### Appendices
- [A — Subsystem → file map](appendix-a-file-map.md)
- [B — Glossary](appendix-b-glossary.md)

### Part X — Walkthroughs
- [Follow the value: a construct from source to machine code](28-follow-the-value.md) — one collection literal traced through every pipeline stage, with the handler at each hop
- [Working on the compiler: a contributor's playbook](29-contributor-playbook.md) — the debugging heuristic, the layering litmus, and the comparison/fixed-point gates, via worked snags
- [The excellence system: how delegated rounds run](30-excellence-system.md) — rounds, the review gauntlet, orchestration, and the measured incidents behind the AGENTS.md process rules

---

*Chapters were folded in from the former `internals/` deep-dives as they reached reference quality; that tree has since been deleted.*
