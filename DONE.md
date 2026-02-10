# DONE

- [2026-02-10] Option[T] / Result[T,E] methods: unwrap, unwrap_or, is_some/is_none, is_ok/is_err (built-in type registration, typecheck, codegen, monomorphization)
- [2026-02-10] Collections: Vector[T], HashMap[K,V], HashSet[T] with real impls (set/remove/clear/is_empty methods, generic return types, C runtime support)
- [2026-02-10] Option[T]/Result[T,E]: .map(), .and_then(), .or_else() with closure return type inference and non-capturing closure codegen
- [2026-02-10] Add `moving` and `mutable` keywords as ownership operator alternatives (both forms coexist with `!`/`&`)
- [2026-02-10] Iterator protocol: for-loop iteration over Dict and Set collections (key-only, key-value tuple, for-else support)
- [2026-02-10] Core traits: Displayable, Equatable, Cloneable, Hashable — built-in trait registration, auto-hooks for ==/!= (Equatable) and string interpolation (Displayable), Self type resolution in equip blocks
