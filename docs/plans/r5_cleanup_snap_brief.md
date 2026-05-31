# Executor Brief — R5 CLEANUP: retire the 62 `.snap` files (Chain-1 follow-up)

**Status:** DRAFT — under fresh-review discipline before launch. Scope re-verified against source 2026-05-31.
**Risk:** LOW (output-neutral, mechanical). **Files (DISJOINT from all other chains):**
`src/backend/c/runtime_snapshot/*.snap` (delete) + `src/backend/c/runtime_extract_test.rs` (lighten).

## 0. Worktree discipline
Run `pwd` + `git rev-parse --show-toplevel` FIRST; confirm inside YOUR worktree. NEVER touch
`/workspace/gorget-1`; no `cd` there; no `/workspace/gorget-1/...` paths. `git add <specific files>` only —
never `-a`/`.`/`commit -a`. Commit in your worktree. Run `cargo build` + `cargo test --lib` only (NOT the
full integration sweep — parent's job).

## 1. The change
Chain 1 extracted the C runtime into `src/backend/c/runtime/*.c`, each wired via
`pub const X: &str = include_str!("runtime/x.c")` in `c_runtime.rs`. The 62 `src/backend/c/runtime_snapshot/
*.snap` files are a **byte-identical frozen copy** of those `.c` files (VERIFIED: 62 files;
`runtime_array.snap` == `runtime_array.c` byte-for-byte). `src/backend/c/runtime_extract_test.rs` (87 lines)
asserts, via its `assert_snap!` macro (`:12-21`), that each `RUNTIME_*` constant `==
include_str!("runtime_snapshot/<name>.snap")`. Since the constants ARE `include_str!` of the SAME `.c`
files, this is now a tautological byte-identity check whose only effect is a stale-on-future-runtime-edit
maintenance burden.

**Do:**
1. `git rm -r src/backend/c/runtime_snapshot/` (all 62 `.snap`).
2. Lighten `runtime_extract_test.rs` to a NON-duplicating smoke check — keep a real but cheap assertion so
   an accidental truncation/empty-file is still caught: e.g. assert each `RUNTIME_*`/`*_RUNTIME` constant
   `!is_empty()` (and maybe a couple of `.contains("...")` sentinel checks on the big ones), OR a single
   combined-length/hash assert over the concatenated constants. Do NOT reintroduce a per-const frozen copy.
   Keep the test compiling + meaningful (it should fail if a runtime `.c` goes missing/empty — which would
   be an `include_str!` compile error anyway, so the test is belt-and-suspenders). If you judge the test
   adds zero value after delinking the snapshots, you MAY delete the file entirely — but PREFER keeping a
   minimal non-empty smoke check (cheaper to keep than to argue).
3. Grep for any other reference to `runtime_snapshot` (build scripts, mod declarations, `.gitignore`,
   docs) and clean up dangling references. Confirm `runtime_extract_test` is still declared as a test module
   wherever it's registered (or remove that registration if you delete the file).

## 2. Gates
- `cargo build` clean; `cargo test --lib` green (the lightened/removed test included).
- No other test references `runtime_snapshot` (grep clean).

## 3. Report back
The diff (files deleted + the test's new form), the grep result for `runtime_snapshot` references, and
`cargo test --lib` status. Note if you deleted vs lightened the test + why.

## 4. Don't-dodge rule
If deleting the snapshots surfaces that something REAL depended on them (a build step, a doc, another
test), STOP and report it — don't silently work around. (Scout verified they're pure duplicates with the one
test as sole consumer, but confirm.)
