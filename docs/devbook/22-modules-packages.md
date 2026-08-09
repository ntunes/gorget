# 22 — Modules, loading & package management

This chapter covers how a multi-file Gorget program becomes a single AST, and how external dependencies are fetched and pinned. Four Rust modules own this subsystem:

- `src/loader.rs` — the **module loader**: turns `import` statements into a filesystem walk, parses each `.gg` file, detects cycles, and merges everything into one `Module` for semantic analysis.
- `src/manifest.rs` — `gorget.toml` (package metadata + declared dependencies).
- `src/lockfile.rs` — `gorget.lock` (pinned, fully-resolved dependency graph).
- `src/resolver.rs` — the **package/dependency resolver**: walks declared deps, fetches git/path sources, detects dependency cycles and version conflicts, writes the lockfile.

> **Naming hazard — two unrelated "resolvers".** `src/resolver.rs` (this chapter) resolves *packages and versions* — which directory on disk a dependency name maps to. It is **completely distinct** from `src/semantic/resolve.rs` (Chapter 7), which does *name resolution* — binding identifiers to definitions inside the merged AST. They share a word and nothing else. When this chapter says "the resolver", it means `src/resolver.rs`. The name resolver enters the story only at the very end, after merging (`§ Where the merged module goes`).

The driver wiring that calls all four lives in `src/main.rs` (`load_imports` at `src/main.rs:33`, `resolve_deps_for_file` at `src/main.rs:96`).

---

## The end-to-end flow

For `gg build foo.gg`, the driver does, in order:

1. **Resolve dependencies** (`resolve_deps_for_file`, `src/main.rs:96`): walk up from the source file looking for `gorget.toml`; if found and it declares deps, call `resolver::resolve` to produce/validate the lockfile, then `resolver::build_dep_paths` to get a `package-name → source-dir` map.
2. **Load & merge** (`load_imports`, `src/main.rs:33`): construct a `ModuleLoader` (with the dep paths from step 1), call `load_all`, then `merge_modules`.
3. Hand the merged `Module` to semantic analysis.

Steps 1 and 2 are independent: a single-file program with no `gorget.toml` skips straight to step 2 with an empty dep map (`resolve_deps_for_file` returns `HashMap::new()` at `src/main.rs:116`).

---

## Import syntax → loader input

The parser produces three import shapes (`ImportStmt` at `src/parser/ast.rs:296`):

- `Simple { path }` — `import std.io`
- `Grouped { path, names }` — `import std.sync.{Arc, Mutex}`
- `From { path, names, glob_types, wildcard }` — `from std.fmt import Displayable, format`, including `from X import *` (`wildcard`), `from xtd.log import LogLevel.*` (`glob_types`), and `from std.math import sin as msin` (the `ImportName.alias`, `src/parser/ast.rs:328`).

The loader cares about **only the `path`** — the dotted module path. The `names`/`glob_types`/`wildcard`/`alias` parts are purely a *name-resolution* concern (Chapter 7) and are ignored during loading: `extract_imports` (`src/loader.rs:108`) pulls just `(segments, span)` from every import shape, collapsing all three into a bare list of dotted paths. A `from std.io import read_file` loads exactly the same file as `import std.io`; whether `read_file` is in scope is decided later.

### Imports hidden inside `meta if`

`extract_imports` recurses into `meta if` blocks (`extract_imports_from_meta_if`, `src/loader.rs:442`). For platform gates it **evaluates the condition at load time** so a Linux build doesn't try to load a macOS-only module: `eval_platform_condition` (`src/loader.rs:483`) recognises `platform() == "..."` / `!= "..."` against the host triple and, when it matches, extracts imports from only the live branch. Any condition it can't evaluate is handled conservatively — imports from *all* branches are extracted (the `None` arm at `src/loader.rs:468`), because a missed import is a hard error but an extra one is merely dead weight that semantic analysis can drop.

---

## The loader: `ModuleLoader::load_all`

`ModuleLoader` (`src/loader.rs:61`) holds five pieces of state:

- `loaded: HashSet<PathBuf>` — canonicalized paths already merged (dedup).
- `load_stack: Vec<PathBuf>` — the active recursion path (cycle detection).
- `dep_paths: HashMap<String, PathBuf>` — package-name → source-dir, from `build_dep_paths`.
- `next_offset: usize` — the cumulative byte offset assigned to the next module (see *Global span offsets* below).
- `entry_base_dir: Option<PathBuf>` — the entry file's directory, the cross-directory fallback root.

`load_all` (`src/loader.rs:568`) takes the already-parsed entry module (the driver parses the entry file itself), seeds `next_offset = entry_source.len() + 1` (`src/loader.rs:586`), records the entry at offset 0, then recurses through its imports via `load_recursive`. It returns a `Vec<(canonical_path, logical_segments, source, Module, base_offset)>` — one quint per loaded file, **entry first**.

### `load_recursive` and the resolution fallback chain

`load_recursive` (`src/loader.rs:672`) maps a dotted path to a file by trying these sources in order; the **first existing file wins**:

1. **Built-in modules** (`std.*`, `xtd.*`, `gg.*`) are intercepted *before* any filesystem lookup (`src/loader.rs:679`). `crate::stdlib::is_builtin_module` (`src/stdlib.rs:28`) gates this; the source is pulled from `crate::stdlib::builtin_module_source` (`src/stdlib.rs:64`), which `include_str!`s the real `.gg` file embedded in the compiler binary (e.g. `lib/std/fs.gg`). The module gets a synthetic path `"<std.fs>"` so it can't collide with anything on disk. (A legacy `generate_builtin_module` hook for compiler-synthesised modules still exists at `src/loader.rs:689` / `src/stdlib.rs:55` but always returns `None` now — every built-in is file-based.)
2. **Local directory** — relative to the *importing file's* directory: `resolve_import_path(base_dir, segments)` (`src/loader.rs:725`), which joins each segment and appends `.gg` (`src/loader.rs:76`). `["util", "greet"]` → `<base>/util/greet.gg`.
3. **Entry base-dir fallback** (`src/loader.rs:730`) — if the local file doesn't exist, retry relative to the *entry* file's directory. This is what lets `renderer/backend.gg` import a module that physically lives under `client/` in a multi-directory project.
4. **Project-root namespaces** (`src/loader.rs:752`) — if the first segment is a known "root namespace" (today only `compiler`; see `is_project_root_namespace`, `src/loader.rs:88`), walk up from the entry dir to the nearest ancestor that contains a directory of that name (`find_project_root_for`, `src/loader.rs:95`) and resolve from there. This makes `from compiler.data.schema import …` resolve to `<project_root>/compiler/data/schema.gg` from anywhere in the tree.
5. **Package dependencies** (`src/loader.rs:765`) — the first segment is treated as a package name and looked up in `dep_paths`. `import mylib` → `<dep_dir>/mylib.gg`; `from mylib.sub import X` → resolve `sub` *within* the dep dir (`src/loader.rs:777`). This is the seam between the loader and the package resolver: `dep_paths` is exactly what `resolver::build_dep_paths` produced.

If none of these yields an existing file, `file_path.canonicalize()` fails and the loader returns `LoadError::Io` (`src/loader.rs:786`).

### Dedup, cycles, and post-order

Each resolved file is canonicalized (`src/loader.rs:786`) so two import paths reaching the same physical file dedup correctly. After resolution:

- **Dedup**: if `canonical` is in `loaded`, return early (`src/loader.rs:792`).
- **Cycle detection**: if `canonical` is on the active `load_stack`, return `LoadError::Cycle` with the chain (`src/loader.rs:797`). Note this is an *import* cycle, separate from the *dependency* cycle the package resolver detects.
- **Post-order recursion**: the file is parsed, then its own imports are loaded **first** (recursively), and only then is the file itself pushed to `results` (`src/loader.rs:826`–`834`). This ordering matters downstream: dependency structs land in the merged AST before the structs that reference them, which the C backend's topological struct emission relies on.

Parse errors during loading surface as `LoadError::Parse` (`src/loader.rs:815`), carrying the offending file's path, errors, and source so the driver can report against the right file.

### Auto-loads (heuristic imports the loader injects)

`load_all` injects two imports the user never wrote:

- **`std.hash`** when the entry module uses `@derive(Hashable)` (`module_uses_hashable_derive`, `src/loader.rs:117`; injected at `src/loader.rs:616`). The derived `equip T with Hashable` references `FxHasher` and its methods, which live in `std.hash`; without the auto-load the user hits link-time undefined-symbol errors. It is **skipped for hot-reload modules** (`src/loader.rs:608`) because the host binary's pruned function set would dangle the Hasher vtable.
- **`std.iter`** — **live, shipped behaviour**. The call site at `src/loader.rs:655`–`661` fires whenever `module_should_auto_load_std_iter` (`src/loader.rs:153`) returns true for the entry module: `let auto_load_iter = …` then `self.load_recursive(&base_dir, &iter_segments, …)`. So `v.iter().map(f).filter(p).collect()` in a scratch file compiles and runs with **no** `from std.iter import …` boilerplate. The heuristic keys off iterator/adapter names (`STD_ITER_NAMES`, `src/loader.rs:204`) and `.iter()` calls, subject to shadowing and existing-import checks via the supporting AST-walkers (`ast_mentions_std_iter_need` et al.). The eager `try_lower_iterator_adapter` shortcut (which materialises `.map()`/`.filter()`/`.collect()` straight into a `GorgetArray`) now **coexists as a fallback** for when the lazy `Iterator[T]` trait default isn't in scope — auto-load shipped 2026-04-23 once the two paths were reconciled (the design rationale, formerly in `stdlib-design.md`, is folded into this chapter and Chapter 23).
  > **Source fossil warning.** The long comment block at `src/loader.rs:621`–`654` still claims "the call site is intentionally disabled" and lists a now-resolved blocker. That comment is **stale and contradicts the live code immediately below it** (621-654 vs 655-661). It is a documentation fossil scheduled for cleanup; trust the code.

---

## Global span offsets

Spans in Gorget are global byte offsets across the *entire merged program*, not per-file. The loader assigns each module a non-overlapping byte range by carrying `next_offset` forward: the entry module owns `[0, len)`, the next module starts at `len + 1` (the `+1` is a separator so end-of-module spans don't butt against the next module's start), and each file is parsed via `Parser::new_with_offset` (`src/loader.rs:812`) so its spans are pre-shifted into its slice. The assigned `base_offset` is returned per module so the diagnostic reporter can binary-search a span back to its originating file without re-deriving offsets — which silently drifted past synthetic/empty-source modules in the older reconstruction path (the cautionary tale is documented at `src/main.rs:71`–`89`). Synthetic/built-in modules with no source text carry a `base_offset` equal to the current `next_offset` but claim no range; the reporter filters them out by `source.is_empty()` (`src/main.rs:87`).

---

## `merge_modules` and variant qualification

`merge_modules` (`src/loader.rs:1348`) flattens the loaded quints into one `Module`:

- The **entry file's items stay at the top level** (including its import statements, which the name resolver needs).
- Each **non-entry module is wrapped in `Item::Module { path, items }`** (`src/loader.rs:1403`) to preserve module identity through the pipeline — this is what lets the name resolver enforce per-module scoping and `private` visibility. The wrapped module's *own* import statements are stripped (`src/loader.rs:1394`) so they don't pollute the entry's import-resolution pass.

The subtle part is **bare enum-variant qualification**. Gorget requires `Color.Red()` not `Red()`, but variants of a *user* enum may be written bare and must be rewritten to qualified form before semantic analysis sees them. `merge_modules` builds a `variant_name → enum_name` map and rewrites every expression/pattern that references a bare variant (`qualify_module_with_map`, `src/loader.rs:956`; the per-node rewriters `qualify_expr`/`qualify_pattern` follow).

Two map-builders exist for two scopes:

- `build_variant_map_from_module` (`src/loader.rs:863`) — a single module's own enums.
- `build_variant_map_from_all` (`src/loader.rs:919`) — unambiguous sightings across *all* modules.

**Ambiguity is deliberately left unresolved.** If a variant name (`Arr`) is defined on more than one enum across the merged set (e.g. both `Json` and `TomlValue`), it is **excluded from the map** (`src/loader.rs:884`, `src/loader.rs:943`). The loader has no type context, so picking either enum would be a [layering-discipline] rule-2 violation (resolving an abstraction by identifier-string lookup). The bare reference stays unqualified and the downstream pattern-lowering / type-checking passes — which *do* know the scrutinee type — resolve it. The merge layers per-module qualification on top of the global unambiguous map (`src/loader.rs:1377`–`1382`) so each module's own `case Arr(x)` always binds to *its* local enum, even when another loaded module ships the same variant name. The pre-fix "first-writer-wins" behaviour (silently routing to whichever module loaded first) is exactly the bug this two-tier scheme fixes.

`PRELUDE_VARIANTS` (`Ok`, `Error`, `Some`, `None`; `src/loader.rs:856`) are never auto-qualified — they stay bare by language design. Generic enums are skipped entirely (`src/loader.rs:867`).

### Where the merged module goes

The single `Module` from `merge_modules` is what every later pass consumes. The *name* resolver (`src/semantic/resolve.rs`, Chapter 7) is the first to read the import metadata the loader discarded: it processes glob imports (`src/semantic/resolve.rs:179`), aliased imports — rebinding `Z → Y` and recording `import_aliases` so the backend emits the real C symbol, not the local alias (`src/semantic/resolve.rs:204`–`213`) — and wildcard imports (`src/semantic/resolve.rs:227`). That is the division of labour: **the loader decides which files to read; the name resolver decides which names are visible.**

---

## Manifest — `gorget.toml`

The filename and format are ruled by **D44**: the manifest is `gorget.toml`, and it is declarative TOML rather than Gorget source, because its readers are not all `gg` — registries, CI, editors and auditors read it too, and dependency resolution must be able to obtain metadata without executing anything.

The name lives at one source of truth: `manifest::MANIFEST_NAME`, reached through `manifest_path_in` and `find_manifest_in`. No call site spells the string, so changing the filename is a one-constant edit rather than a sweep across the driver, the resolver and the manifest module.

Visibility does **not** key off this file. The privacy unit is the directory (**D43**); the manifest governs dependencies and versioning only. That separation is deliberate — it keeps a mislocated manifest a packaging error rather than something that silently rescopes declarations.

`Manifest` (`src/manifest.rs:13`) is a thin serde-over-TOML struct: a `[package]` table (`name`, `version`; `PackageInfo` at `src/manifest.rs:20`) and a `[dependencies]` table mapping name → `DepSpec`. `dependencies` is a `BTreeMap` (`src/manifest.rs:16`) — sorted by name, which makes serialization deterministic.

`DepSpec` (`src/manifest.rs:27`) is an **untagged** enum with two shapes, distinguished by which keys are present:

- `Git { git, tag?, branch?, rev? }` — `http-router = { git = "…", tag = "v1.2.0" }`
- `Path { path }` — `utils = { path = "../shared/utils" }`

`find_project_root` (`src/manifest.rs:104`) walks up from a starting path until it finds a directory containing `gorget.toml` — this is how the driver locates the manifest from an arbitrary source file. `Manifest::new` (`src/manifest.rs:91`) seeds version `"0.1.0"`; `gg init` writes it via `cmd_init` (`src/main.rs:2089`), which also scaffolds `main.gg` and `.gitignore`.

---

## Lockfile — `gorget.lock`

`Lockfile` (`src/lockfile.rs:12`) is `Vec<LockedPackage>` serialized as `[[package]]` TOML arrays. Each `LockedPackage` (`src/lockfile.rs:18`) records `name`, `source`, `version`, and its direct `dependencies` (names only). `save` (`src/lockfile.rs:69`) prepends a `# auto-generated, do not edit` header.

The `source` field is a tagged string with two forms:

- `path+<dir>` — a path dependency.
- `git+<url>#<commit>` — a git dependency pinned to an exact commit.

`LockedPackage::source_dir` (`src/lockfile.rs:91`) decodes `source` back into a filesystem directory: `path+` resolves relative to the project root (absolute paths pass through); `git+url#commit` maps to `<cache>/git/<sha256(url)>/<commit>`. The "sha256" here is **not cryptographic** — `sha256_hex` (`src/lockfile.rs:117`) is a doubled `DefaultHasher` used purely to derive a stable cache-directory name, deliberately avoiding a crypto dependency.

---

## Package resolver — `src/resolver.rs`

`resolver::resolve` (`src/resolver.rs:75`) is the entry point. It is **lockfile-first**: if `gorget.lock` exists and `is_lockfile_current` (`src/resolver.rs:112`) holds — every manifest dep name appears in the lock — the existing lockfile is returned unchanged (`src/resolver.rs:84`). Note this check is name-coverage only; it does **not** verify versions or sources match, so editing a same-named dep's *source* without changing the set of names won't trigger a re-resolve. `gg add`/`gg remove` (`cmd_add`, `src/main.rs:2218`–`2231`; `cmd_remove`, `src/main.rs:2252`–`2264`) mutate the manifest and then call this *same* lockfile-first `resolver::resolve` — neither deletes the lockfile nor bypasses `is_lockfile_current`. So what re-resolves is a **name-set change** (adding a brand-new dep name, or removing one); a same-name source edit does **not** re-resolve through either path.

Otherwise it resolves from scratch via `resolve_deps` (`src/resolver.rs:130`), a recursive DFS over the dependency graph that maintains:

- `resolved: BTreeMap<String, LockedPackage>` — the accumulating result (dedup: already-resolved deps are skipped, `src/resolver.rs:139`).
- `resolving: Vec<String>` — the active path, for **dependency cycle detection** (`src/resolver.rs:144`, returns `ResolveError::CycleDetected`).

For each dep:

- **Path deps** (`src/resolver.rs:153`): canonicalize the path (relative to the importing manifest's dir), read the dep's own `gorget.toml` for its version (defaulting to `"0.0.0"` if absent), and record `source = "path+<rel>"`.
- **Git deps** (`src/resolver.rs:181`): `resolve_git_ref` (`src/resolver.rs:246`) turns the requested ref into a commit hash. An explicit `rev` is used verbatim; otherwise `git ls-remote` resolves `refs/tags/<tag>`, `refs/heads/<branch>`, or `HEAD`. `fetch_git_dep` (`src/resolver.rs:299`) then shallow-clones into `<cache>/git/<url_hash>/<commit>` (skipping the clone if already cached, `src/resolver.rs:319`) and checks out the commit. Source is recorded as `git+<url>#<commit>`.

After resolving a dep, the resolver reads the dep's *transitive* dependencies from its manifest (`src/resolver.rs:214`) and **recurses** (`src/resolver.rs:235`), so the lockfile captures the full transitive graph.

**Version-conflict detection** (`src/resolver.rs:203`) is intentionally crude: if the same package name is reached from two *different source strings*, it errors with `ResolveError::VersionConflict`. There is **no semver solving** — Gorget pins by exact source/commit and treats divergent sources as a hard conflict rather than trying to pick a compatible version. This is a deliberate simplicity trade-off, not an oversight.

The global cache lives at `~/.gorget/cache/` (`cache_dir`, `src/resolver.rs:64`, falling back to `USERPROFILE` then `.`). `build_dep_paths` (`src/resolver.rs:381`) is the bridge back to the loader: it maps each locked package through `source_dir` into the `package-name → directory` map the loader consults in fallback step 5.

The CLI surface — `gg init` (`src/main.rs:2089`), `gg new` (`src/main.rs:2136`), `gg add` (`src/main.rs:2160`), `gg remove` (`src/main.rs:2235`) — mutates the manifest and then calls `resolver::resolve` to update the lockfile and fetch.

---

## In the self-host

The self-host frontend has a Gorget reimplementation of the **module loader** at `tests/fixtures/self_host_lowerer/loader.gg` (`load_imports`, `loader.gg:527`). It does *not* reimplement the package resolver, the manifest, or the lockfile — there is **no self-host coverage of `gorget.toml`/`gorget.lock`/git fetching**; the self-host always resolves modules from a local dir or an embedded `lib` dir. It is wired into the self-host pipeline by `tests/fixtures/self_host_lowerer/driver.gg` (`from loader import load_imports`, `driver.gg:10`; called at `driver.gg:52`). A second copy lives at `tests/fixtures/self_host_check/loader.gg` (the two copies have diverged in size).

Architectural parity and the notable divergences:

- **Same fallback intent, fewer layers.** `resolve_module_path` (`loader.gg:16`) maps a dotted path to a `.gg` file, trying the local dir then the `lib` dir (`loader.gg:587`–`589`), plus the project-root-namespace fallback for `compiler.*` (`loader.gg:593`–`600`, mirroring `is_project_root_namespace`/`find_project_root_for` at `loader.gg:43`/`loader.gg:54`). It hardcodes the `std.net.{socket,tls,udp}` → `std.{socket,tls,udp}` flattening (`loader.gg:20`) that the Rust side gets from `builtin_module_source`.
- **Same global-offset scheme.** `next_offset = entry_source_len + 1` (`loader.gg:554`) and per-module `parse_source_with_offset` (`loader.gg:605`) mirror the Rust `next_offset` discipline so cross-file spans stay unique — the comment at `loader.gg:529`–`542` documents a real bug (the resolver's span-keyed `Dict[int,int]` last-write-wins on a collision) that this offset scheme, plus a hash-function fix, resolved.
- **Mangling instead of `Item::Module` wrapping.** Rather than wrapping imported modules in `Item::Module` and relying on a separate name resolver, the self-host loader **renames imported functions** with a module prefix (`mangle_prefix`, `loader.gg:521`: `std.conv` → `std__conv___`) and returns a `call_redirects` map (`loader.gg:832`) so the lowerer can rewrite bare call names. It also filters out extern-stub bodies and merges only concrete (non-generic) imported structs/enums/consts/statics tagged `__imported_type__` (`loader.gg:619`–`827`).
- **`std.iter` auto-import.** Enabled here (`should_auto_load_std_iter` at `loader.gg:512`, invoked at `loader.gg:573`) — same as Rust `gg`, which also fires its auto-load (`src/loader.rs:655`–`661`). The supporting AST walkers (`expr_mentions_iter`, `type_mentions_iter`, …) are direct ports. The only divergence is **Self-host snag #5** (comment block at `loader.gg:85`–`92`): the auto-load check cannot be factored exactly as in Rust because a top-level `match` on `Vector[Item]` triggers a Rust-stage codegen bug, so the free helpers avoid top-level-matching on `Item` rather than calling out to a shared top-level helper.

**Checking current parity (procedure, not a fixed number).** The self-host loader is exercised by two diagnostic comparison tests; both are *always-pass* (they print counts, they don't assert), so read the printed matched-count rather than the green/red status:

```bash
cargo test --test integration lowerer_comparison -- --nocapture
cargo test --test integration c_emit_comparison -- --nocapture
```

`c_emit_comparison` (`tests/integration.rs:13549`) builds the `self_host_lowerer` driver — which runs `loader.gg` end-to-end across every fixture — and compares emitted-C user-function counts against Rust `gg`. The `check_comparison` test (`tests/integration.rs:13193`) similarly runs the `self_host_check` loader+typecheck pipeline. Per the project north star, parity (matched-counts climbing toward Rust's) is the target — a green run alone says nothing.
