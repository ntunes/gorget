# Design notes — ruled but not yet built

**This is not the contributor book.** The contributor book is
[`docs/devbook/`](../devbook/README.md): it describes what the compiler does
*today*, in present tense, verifiable against HEAD.

This directory holds the other thing — **designed-but-unbuilt** material that
the devbook's genre cannot carry: owner rulings on work not yet in the tree,
rejected-alternative records, and deferred designs. A file here describes what
has been *decided*, not what the compiler *does*.

Normative language rulings live in
[`docs/define-gorget/decisions.md`](../define-gorget/decisions.md). A design
note may elaborate a ruling; it never *is* the ruling.

## Status vocabulary

Every file carries one status in its header. There is deliberately no `SHIPPED`
status: once the work lands, the content belongs in a devbook chapter and the
design note is deleted.

| status | meaning |
|---|---|
| `RATIFIED-UNBUILT` | an owner has ruled; no code exists yet |
| `PROPOSED` | a design under discussion; not ruled |
| `IN-PROGRESS` | partially landed; the note tracks the remainder |
| `SUPERSEDED → <target>` | folded into `<target>`; awaiting deletion |

## Contents

Dispositions below are the result of a full per-file audit against HEAD. A row
marked `SUPERSEDED` is **not** a source of truth — read the target chapter
instead; the file survives only until its pending fold/repoint work lands.

### Live design notes

| file | status | covers |
|---|---|---|
| [cow-transient-view-model.md](cow-transient-view-model.md) | `RATIFIED-UNBUILT` | The *legality* axis of CoW: place-gate, typed builtin views, transient/unstorable views, closures as the user mutate-through path. Elaborates the no-stored-borrows ruling. |
| [cow-cost-contract.md](cow-cost-contract.md) | `RATIFIED-UNBUILT` | The *cost* axis of CoW: the per-signature ownership summary, arg- and return-boundary elision, the guaranteed-elision set, and the `implicit_clones` knob. |

### Superseded — read the devbook chapter instead

| file | current truth lives in |
|---|---|
| [bir-module-synthesis-plan.md](bir-module-synthesis-plan.md) | [devbook/16 — BIR](../devbook/16-bir.md) |
| [handover-option-c-bir-synthesis.md](handover-option-c-bir-synthesis.md) | [devbook/16 — BIR](../devbook/16-bir.md) |
| [clone-emission-at-calls.md](clone-emission-at-calls.md) | [devbook/13 — Ownership in the IR](../devbook/13-ownership-in-ir.md) |
| [ownership-ir.md](ownership-ir.md) | [devbook/13 — Ownership in the IR](../devbook/13-ownership-in-ir.md) |
| [copy-on-write.md](copy-on-write.md) | [devbook/11 — Copy-on-write](../devbook/11-copy-on-write.md) |
| [codegen-gap-spike.md](codegen-gap-spike.md) | [devbook/17 — The C backend](../devbook/17-c-backend.md) |
| [tier1c-cluster1-burn-down.md](tier1c-cluster1-burn-down.md) | [devbook/17 — The C backend](../devbook/17-c-backend.md) |
| [extern-modules.md](extern-modules.md) | [devbook/20 — Extern & GPU](../devbook/20-extern-gpu.md) |
| [fstring-interp-as-expr.md](fstring-interp-as-expr.md) | [devbook/04 — Parser & AST](../devbook/04-parser-ast.md) |
| [safety-checker.md](safety-checker.md) | [devbook/10 — Ownership & safety](../devbook/10-ownership-safety.md) |
| [structural-guards.md](structural-guards.md) | [devbook/25 — Structural guards](../devbook/25-structural-guards.md) |
| [self-host-resource-model.md](self-host-resource-model.md) | [devbook/26 — The self-host frontend](../devbook/26-self-host-frontend.md) |
| [layering-discipline.md](layering-discipline.md) | [devbook/24 — Layering discipline](../devbook/24-layering-discipline.md) |
| [shared-keyword-design.md](shared-keyword-design.md) | [devbook/10 — Ownership & safety](../devbook/10-ownership-safety.md) |
| [method-level-inference.md](method-level-inference.md) | [devbook/09 — Type checking](../devbook/09-type-checking.md) |

### Mixed — partly superseded, partly unbuilt

These carry live design alongside folded material. Each is pending a split: the
shipped half folds into the named chapter, the unbuilt half stays here.

| file | shipped half → | unbuilt half retained |
|---|---|---|
| [unified-resource-model.md](unified-resource-model.md) | devbook [12](../devbook/12-gir-lowering.md) / [13](../devbook/13-ownership-in-ir.md) / [14](../devbook/14-lir-ssa.md) / [15](../devbook/15-drop-elaboration.md) / [18](../devbook/18-runtime-abi.md) / [25](../devbook/25-structural-guards.md) | Phase B (deferred view/owner discrimination); the `SlotProvenance` unified-enum ruling; contract-evolution discipline; four open design questions |
| [lir-design.md](lir-design.md) | [devbook/14 — LIR & SSA](../devbook/14-lir-ssa.md) | The WASM backend design (Relooper/Stackifier, slots→locals vs linear memory); the IR research base (why not Sea of Nodes / MLIR dialects) |
| [llvm-backend-plan.md](llvm-backend-plan.md) | [devbook/19 — The LLVM backend](../devbook/19-llvm-backend.md) | WASM via `--target=wasm32` — a second, distinct route from `lir-design.md`'s |
| [lir-backend-lift-plan.md](lir-backend-lift-plan.md) | [devbook/16 — BIR](../devbook/16-bir.md) | Why-not records (MLIR dialects, Sea of Nodes, stack bytecode); the "why BIR" naming rationale |
| [stdlib-design.md](stdlib-design.md) | [devbook/23 — The stdlib](../devbook/23-stdlib.md) | The six iterator algebraic laws (property-test targets); the `spawn unchecked` discipline rules |
| [meta.md](meta.md) | [devbook/06 — Meta & derive](../devbook/06-meta-derive.md) | Why `[]` type application over parens; diamond inference |

## Conventions

- **A design note is not a plan.** Round-scoped scouts, briefs, and censuses are
  `/tmp`-only and are never committed (`docs/plans/` is retired and lint-guarded).
  What lives here is durable design, not the paperwork of a round.
- **No numbers.** A measured figure in a design note is a stale premise with a
  fuse on it. Record the command that regenerates it instead.
- **Cite source, don't transcribe it.** A `file:line` citation is checkable; a
  copied code block silently rots.

The pipeline overview that used to live here is superseded by
[devbook/01 — Pipeline & driver](../devbook/01-pipeline-and-driver.md) and
[appendix A — file map](../devbook/appendix-a-file-map.md), which are derived
from `src/semantic/mod.rs` rather than transcribed.
