# TODO

## High

- **Stdlib narrow waist — Phase 1.5 remainder**: 5/6 done 2026-04-17 (Vector.capacity, sort_by_key/sorted_by_key, windows/chunks, Dict/Set Hashable+Equatable bounds, Debuggable trait + @derive(Debuggable)). REMAINING: (c) Deprecate `String.join(vec)` — blocked on Phase 2 `iter().join(sep)` existing as the replacement. See `docs/internals/stdlib-design.md` §10. [revised: 2026-04-17]

- **Stdlib narrow waist — Phase 2a (type-system prerequisites)**: SHIPPED 2026-04-17: (1) Grammar accepts `&`/`!` in type-args via `parse_type_with_ownership`. (2) Sigils collapse to bare T at monomorphization — `Vector[T &]`, `Vector[T !]`, and `Vector[T]` now share the same runtime layout (fixed `mangle_type_for_name` + GIR `map_ast_type_mut`). Non-Copy iteration works (`Vector[String &]` iterates like `Vector[String]`). (3) `Owned(T)` transparently resolved. (4) Associated-type bounds parse+format: `type Iter: Iterator[T]`. REMAINING (deferred to Phase 2b): (a) Sigil propagation through HOF closure signatures — when `filter[U](U(T) f)` is called with `Iterator[String &]`, does closure signature become `U(String &)`? Unverified. (b) Associated-type bound *enforcement* (parsing works, semantics no-op). (c) Distinct Iterator types — the sigil collapse means `Iterator[T &]` and `Iterator[T !]` are currently indistinguishable. When Iterator trait lands it needs a different mechanism (e.g., tier encoded in the trait's self_ownership). See design doc §4.2, §4.3. [revised: 2026-04-17]

- **Stdlib narrow waist — Phase 2b (concrete iterator returns)**: Depends on 2a. Compiler support for concrete iterator state-machine structs (one per adapter: Filter, Map, Take, Chain). Monomorphized adapter chains fuse. Optional: `impl Iterator[T]` return-type sugar. NO TRAIT OBJECTS — trait-object iteration is virtual-dispatch per element and makes lazy fusion structurally impossible. See design doc §3 "Concrete Return" and §4.6. [added: 2026-04-17]

- **Stdlib narrow waist — Phase 2c (Iterator/Iterable/IntoIterable, LAZY FROM DAY ONE)**: Depends on 2a+2b. Define `Iterator[T]`, `Iterable[T]`, `IntoIterable[T]` per design doc §3. Implement all equip methods from §4.4 — lazy, no eager Vector-intermediate. Vector/Dict/Set convenience wrappers (`v.map(f)` ≡ `v.iter().map(f).collect()`). Add `swap_remove`, `retain`, `fill`, `swap` on Vector. Single inferred `collect()` (drop `to_set()`/`to_dict()`). **Critical**: do NOT ship an eager interim — it trains users onto the old Vector API and lazy Iterator stays unused. See `docs/internals/stdlib-design.md`. [revised: 2026-04-17]

- **Cloneable trait + runtime clone counters**: `--show-clones` is comprehensive (all 22 implicit clone sites report with span, type, and reason; output sorted by source location). Remaining: `Cloneable` trait for generic bounds (`T: Cloneable`). Runtime clone counters (`gg run --clone-stats`) via existing alloc-report infrastructure. [updated: 2026-04-10]

- **`borrowed` qualifier for extern return types**: All extern function results are currently assumed owned. If we wrap a C library function returning a borrowed pointer (e.g., SDL_GetError's internal buffer), we need `extern borrowed String sdl_get_error()` to tell the compiler to auto-clone at the boundary. Currently these cases are handled by making the C wrapper return Str (copying internally). [added: 2026-04-03]

- **C backend: migrate val_types to func.value_types**: Shared `compute_module_value_types()` runs after LIR optimization and populates `func.value_types`. The LLVM backend already reads from it. The C backend still uses its own single-pass `infer_inst_type` because its multi-phase fixups (guard accessor inference, CallExtern→SlotStore mismatch, cross-type map combinator) depend on `ptr_pointee` context computed in the same pass. Next step: seed the C backend's val_types from `func.value_types` and reduce the fixup phases. [updated: 2026-04-14]

- **LIR value origin metadata — enable Store/SlotStore/Call lifts**: The C backend maintains 5 origin bitmaps (`str_lit_vals`, `null_vals`, `cstr_vals`, `ptr_pointee`, `func_addr_targets`) beyond type info. These track value provenance needed for ~37 emit-decision sites. The type metadata (`func.value_types`) is now shared; origin metadata remains backend-local. Fix: attach origin tags to LIR values (e.g. `StrLit` → string-literal flag, `NullPtr` → null flag, `FuncAddr` → FuncId). Unblocks lifting Store routing (~50 lines), SlotStore string/cstr coercion (~22 lines), and Call/CallPtr ABI coercion (~100 lines). [updated: 2026-04-14]

- **Decompose emit_call_extern.rs (~1,850 lines)**: Tier 1-3 lifts complete — ~490 lines of inline expansion removed. Remaining: HOF inlining (map/filter/each/fold ~590 lines), printf rewriting (~130 lines), out-parameter adaptation (~178 lines), collection drop/clone injection (~70 lines). These are genuinely backend-specific patterns. [updated: 2026-04-15]

## Medium

- **Stdlib narrow waist — Phase 3 (Writer/Reader, byte-shaped + typed IoError)**: SHIPPED 2026-04-17: `std.io` with full `IoError` + Displayable + Debuggable + Error, `Error` trait (`extends Displayable & Debuggable` + `Option[String] source(&self)` — coexists with `Result.Error` variant via type/value namespace separation), `Writer` / `Reader` traits (Vector[byte]), helpers `write_all[W]` / `write_str[W]` / `write_display[W, Displayable D]` / `reader_drain[R]` / `read_exact[R]`, all generic (no trait-object dispatch). **`equip String with Writer:` works end-to-end**. `ParseError` lives in `std.conv` (parsing is semantic, not I/O) with Displayable + Debuggable + Error; `parse_int`/`parse_float` now return `Result[T, ParseError]`. REMAINING: (a) implement Writer on File / Socket / TlsSocket / stdout / stderr — needs runtime `gorget_file_write_bytes_handle`-style function taking File + byte slice (the existing gorget_file_write takes path+content). (b) Migrate remaining stdlib I/O signatures from `Result[T, String]` to `Result[T, IoError]` (json.parse, toml.parse, yaml.parse → their own JsonError / TomlError / YamlError or ParseError; file.read_*, tls.*, socket.* → IoError) and `print()` → `stdout.write_display` + newline — depends on (a). See `docs/internals/stdlib-design.md` §6, §9. [revised: 2026-04-17]

- **Stdlib narrow waist — Phase 4 (concurrency enforcement + Hashable migration)**: (1) Type-checker pass: reject `&` captures crossing `spawn` boundaries — must be `shared T` or `spawn unchecked`. (2) Parser/syntax for `spawn unchecked` (per-spawn opt-out, grep-able). (3) Audit fixtures + xtd libraries for patterns relying on `&` escaping spawns; migrate. (4) Hashable migration: trait switches from `int hash(self)` to `void hash(self, Hasher &h)` — state-based hashing composes. Update `@derive(Hashable)` generator; reimplement Dict/Set internals against `Hasher`. Hand-written impls need rewrite (High impact). See design doc §8 (concurrency) and §3 (Hasher). [revised: 2026-04-17]

- **Stdlib narrow waist — Phase 5 (Documentation updates)**: Update language-design.md §4.4.1 (Writer/Reader/Hasher/Debuggable -er/-able examples), language-reference.md §15.2 (method signatures), book/05-collections.md (new methods, Iterator examples), book/appendix-traits.md (add Writer, Reader, Debuggable, Error, Hasher; naming convention), book/19-stdlib.md (std/xtd layering + concurrency model). New: book/XX-concurrency.md — `&`/`shared`/`unchecked` with worked examples. See `docs/internals/stdlib-design.md` §10 Phase 5. [revised: 2026-04-17]

- **ensure_owned_at_boundary migration — remaining specialized sites**: Core migration done. 5 remaining sites each have specialized logic beyond pure boundary-clones (fresh-string elision, last-use move, MutPtr wrapping, pattern extraction, field_access checks). Struct init was already covered. Enum variant init fixed (was missing `clone_multi_use_resource_args` at the `methods.rs` and `calls.rs` call sites — caused double-free on resource-typed fields in loops). [updated: 2026-04-16]


- **dict[key].push() index-mutate**: Prototype works for MutPtr in-place mutation. Needs `is_storing_method` flag on BuiltinMethodDecl. [updated: 2026-03-28]

- **Box.new should enforce `!` at borrow checker level**: Currently Box.new implicitly MoveZeros the source. [added: 2026-03-26]

- **Name-based dispatch: remaining migration**: ~96 `starts_with` sites in IR lowering, ~87 in LIR backend. Blocked on `register_collection_alias` TypeDef timing. [added: 2026-03-26]


- **Hardcoded type size database — blocks self-host lowerer**: `c_sizeof_with_structs()` still has string-match fallbacks for `Vector__*`, `Dict__*`, `Set__*`, `Callable__*`, `Task__*`, `Tuple__*`, `Option__*`. These hit before the struct lookup. Fix: register monomorphized collection/option/tuple types with correct `computed_c_size` during type lowering so the match arms can be removed. [updated: 2026-04-06]


- **`@[no_alloc]` function annotation**: Compiler error on allocating operations. [added: 2026-03-21]

- **Spawn captures don't check stale shared-derived**: Spawned closures can capture stale pre-await data. [added: 2026-03-18]

- **Replace auto-borrow with explicit reference semantics**: Phase 1 done (const_params). Phase 2 (const propagation) not started. [updated: 2026-03-20]

- **Collection Resource semantics: remaining call-site ownership gaps**: Borrow checker doesn't cover field assignment or method-call ownership transfer. [updated: 2026-03-22]

- **Drop elaboration — remaining cleanup**: (1) 24 Memsets across 17 fixtures remain: IndexLoad element zeroing (inside collection data arrays) and projected Deref/Field MoveZero (field-level ownership through pointers). Genuinely necessary — could be eliminated with element drop flags or `MoveField` instruction. (2) GIR still emits MoveZero for borrow-wrapped call args (field loads, MutPtr params), but these are zero-cost at runtime (V6 converts to MoveSlot). Removing the GIR emissions is code cleanliness, not a perf concern. [updated: 2026-04-14]

- **LLVM backend test results (2026-04-16, post-session)**:
  - **738 PASS / 814 (90.7%)**, 29 FAIL, 38 CRASH, 9 BUILD_FAIL (after elem_drop re-enable). Up from 710 PASS baseline — **+28 net PASS, -10 FAIL, +3 CRASH**.
  - Fixes: (1) Option/Result combinator inline handlers, (2) CStr null-termination, (3) **LIR elem_drop/elem_clone stores re-enabled** + LLVM SlotStore String CoW clone + NamedFuncAddr declaration generation.
  - **elem_drop root cause (resolved)**: LLVM's SlotStore did plain memcpy for all aggregate stores regardless of `is_move`. C backend emits `gorget_string_copy_cow` on non-move Ptr→String stores (src/backend/c_lir/mod.rs:1629). Fix: mirror that CoW clone in LLVM backend src/backend/llvm/mod.rs SlotStore handler + declare `T__clone`/`T__drop` for NamedFuncAddr user-type references.
  - **Remaining 4 dataframe_* CRASH**: Still double-free somewhere in xtd.dataframe with elem_drop active — deferred (likely nested Vector[Vector[Column]] or Union-typed payload issue).
  - Remaining BUILD_FAIL (9): 4x LLC forward-ref type mismatch, conv_stdlib, shared_iterator_invalidation, print_trait_object, string_enum_variants, sqlite

## Low

- **Clone reduction — 3 deferrable sites (low ROI)**: (1) context.rs:905 Ptr(resource) init → scope escape check, (2) stmts/mod.rs:374 Ptr binding auto-clone → defer to mutation, (3) patterns.rs:522 string field extraction → check arm escape. Audit of all 952 fixtures found max 5 implicit clones per fixture, all at necessary ownership boundaries. These 3 sites add complexity for marginal gain. [demoted from High: 2026-04-09]

- **Self-host LIR backend**: ~6,200 lines across 4 files. 687/936 fixtures compile (was 462 baseline; net +225 over two sessions). 0 crashes. Key fixes across sessions: (1) SlotStore type-mismatch coercion — scalar→aggregate and aggregate→aggregate both emit `{0}` zero-init; (2) runtime fn return types — gorget_args/env_vars/cwd/str_to_upper/lower/char_at/byte_slice/int_to_str/float_to_str/bool_to_str all correctly typed; (3) runtime_arg_is_str table coerces pointers/scalars at Str parameter positions (str_cat/eq/cmp etc.); (4) ICmp narrowed to GorgetString plus memcmp fallback for struct==struct; (5) generic placeholder + enum variant filtering in type_defs; (6) bare opaque/prelude type constructors (TaskGroup, AtomicInt, Box, Shared, …); (7) is_type_constructor excludes primitive coercions; (8) post-gmod fn_sigs pass covers functions + equip methods; (9) extern time/time_ms/format_time/parse_time mappings; (10) Option/Result combinator takes address of aggregate src; (11) drop/clone forward declarations prevent static-after-implicit conflicts; (12) enum_variant_parent routes bare variant constructors to parent enum type; (13) Str/String/GorgetString identity coercion (Str("x") → x); (14) imported IEnum merged with __imported_type__ marker (skips drop/clone regen); (15) TFunction param ABI is Ptr(FnPtr) instead of unit — closure params now get pointer passing; (16) static method calls on type identifiers (Point.default(), int.parse(s)); (17) operator overload (+/-/*/div/rem/neg/==/!=/<=/>=) dispatches to TypeName__method for user structs, including monomorphized instances; (18) gorget_str_strip arity padding. Remaining ~249 failures: Str-as-int casts in JSON/XML/TOML parsers (b64_char_value), imported-struct field access (needs IStruct loader merge without drop conflicts — tried, regresses), DataFrame col_slice with Column placeholder types, Vector[T](alloc=…) keyword args, throws/Result auto-wrapping, SSA phi gaps (unassigned block params). [updated: 2026-04-17]

- **`meta is_pure(fn_name)` builtin**: Chicken-and-egg with pass ordering. [added: 2026-03-14]

- **If-expression `elif` branches**: Parser limitation. Workaround: nested `else: if`. [updated: 2026-03-11]

- **`shared_stress_yield` flaky deadlock**: Timing-dependent contention. [added: 2026-03-11]

- **Inline `None()` without typed variable**: Produces garbage. Workaround: bind to typed `Option[T]` first. [added: 2026-03-11]

- **`shared static` support**: Thread-safe module-level statics. Workaround: explicit `Mutex[int]`. [added: 2026-03-10]

- **C backend: `compute_type_overrides` should use TypeIds**: Fragile string-matching. [added: 2026-03-14]

- **C backend: uninitialized return variable**: `_0` used uninitialized in some functions. [added: 2026-03-13]

- **Metal runtime: ObjC method signature issue**: drawIndexedPrimitives with indirect buffer. [added: 2026-03-13]

- **Metal runtime: deprecated APIs**: sampleCount, useResource, useHeap. [added: 2026-03-13]

- **User DictIter[K, V] / DictKeyIter[K] hit Ptr-ABI codegen panic**: Constructing a generic struct with a `Vector[K]` field from a `Dict.keys()` source panics in `emit_types.rs:1645` with "Ptr ABI received scalar value" when the user-defined iterator is subsequently stored and read back through a self pointer. `VectorIter[T]` avoids this because the Vector is passed by value into a Vector-typed field. Reproducer: `tests/fixtures/iterator_direct.gg`-style iterator but on Dict-derived keys. [added: 2026-04-17]
