# DONE

- [2026-02-10] Option[T] / Result[T,E] methods: unwrap, unwrap_or, is_some/is_none, is_ok/is_err (built-in type registration, typecheck, codegen, monomorphization)
- [2026-02-10] Collections: Vector[T], HashMap[K,V], HashSet[T] with real impls (set/remove/clear/is_empty methods, generic return types, C runtime support)
- [2026-02-10] Option[T]/Result[T,E]: .map(), .and_then(), .or_else() with closure return type inference and non-capturing closure codegen
- [2026-02-10] Add `moving` and `mutable` keywords as ownership operator alternatives (both forms coexist with `!`/`&`)
- [2026-02-10] Iterator protocol: for-loop iteration over Dict and Set collections (key-only, key-value tuple, for-else support)
- [2026-02-10] Core traits: Displayable, Equatable, Cloneable, Hashable — built-in trait registration, auto-hooks for ==/!= (Equatable) and string interpolation (Displayable), Self type resolution in equip blocks
- [2026-02-10] Box[T]: heap allocation wrapper (Box(v) and Box.new(v) constructors, .get()/.set() methods, *b deref, string interpolation, type inference)
- [2026-02-10] Implicit `it` in closures: parser auto-wraps call args containing `it` in ImplicitClosure, typecheck assigns fresh type var to `it`, codegen delegates to existing closure machinery
- [2026-02-10] Drop trait / RAII (destructors): built-in Drop trait, scope-based cleanup at function/loop exit, Box[T] auto-free, user-defined Drop via equip, cleanup before return/break/continue
- [2026-02-10] Default trait method resolution: codegen emits default method bodies when equip block doesn't override, vtable wiring, trait method call resolution for trait-provided methods
- [2026-02-10] Trait inheritance (extends) in vtables: child vtable structs include parent method slots, equip blocks validate and emit parent methods, semantic validation checks inherited required methods
- [2026-02-10] `gg fmt` code formatter: lexer emits Comment tokens, parser side-tables them, AST-walking formatter with comment interleaving, --in-place flag, idempotency tested on all fixtures
- [2026-02-10] File I/O: built-in File type with GorgetFile C runtime, free functions (read_file, write_file, append_file, file_exists), File struct (File.open, File.create, .read_all, .write, .close), Drop auto-close on scope exit
- [2026-02-10] Generic function monomorphization: type substitutions now propagate into block bodies (local vars, casts, nested generic calls), added type_subs field to CodegenContext, type_to_c() convenience method
- [2026-02-10] Trait bounds enforcement: `where T is Trait` clauses now checked at generic call sites in semantic analysis, UnsatisfiedTraitBound error emitted when concrete type lacks required trait impl
- [2026-02-10] Runtime safety: bounds checking on Vector get/set/remove/pop (gorget_array_get, gorget_array_set, gorget_array_remove), division by zero guard on `/` and `%` operators — panics with clear error messages instead of UB
- [2026-02-10] Match exhaustiveness checking: semantic analysis rejects non-exhaustive enum matches with `NonExhaustiveMatch` error listing missing variants; handles constructors, or-patterns, wildcards, catch-all bindings, guards; skips non-enum types
