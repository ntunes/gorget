# Working on the compiler: a contributor's playbook

> **Status: STUB** — deepened after reference coverage is complete (per [the plan](../plans/devbook_plan.md), reference-first).

This chapter will turn the project's hard-won working rules into narrative with worked examples. It draws on `CLAUDE.md` and the internals docs, but as *story*, not rules-list. Planned material:

- **The debugging heuristic: fix-complexity is a signal of the wrong layer.** When a fix needs save/restore around branches, phi insertion, or manual SSA repair, you're patching a symptom — trace the data to its *write* site. Worked examples: Snag #17 (`self_conv` flag ignored) and Snag #13 (`Box[T]` inner-type not exposed to the C backend).
- **Don't redesign around compiler gaps.** Fix the gap, or write a failing fixture + a sharp TODO — never reshape tests/fixtures/self-host to dodge it.
- **Self-host as the elegance showcase.** Defensive code with a stale justification is debt; retire workarounds when their bug is fixed.
- **Re-verify a premise against current source before acting.** Dated scores, diagnoses, and TODO notes go stale.
- **Layering discipline in practice** — see [Chapter 24](24-layering-discipline.md); this section is the how-to companion.
