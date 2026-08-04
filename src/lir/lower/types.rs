//! Type mapping, size calculation, and enum helpers for GIR → LIR lowering.

use super::*;
use std::collections::HashMap;

/// Alias map type: monomorphized name (e.g. `Vector__int64_t`) → backing
/// runtime StructDef id (e.g. GorgetArray's StructId). Populated at LIR
/// lowering on `LirModule::struct_aliases`. Used by size lookups so the
/// canonical typed `StructDef.computed_c_size` replaces name-prefix routing
/// in `opaque_runtime_size`.
pub(super) type AliasMap = HashMap<String, StructId>;

/// Extract the element sizeof from a monomorphized collection constructor name.
/// E.g., `Vector__int64_t__new` → sizeof(int64_t) = 8.
/// Returns the size in bytes, or None if the name doesn't match.
pub(super) fn elem_size_from_monomorphized(name: &str, structs: &[StructDef], aliases: &AliasMap) -> Option<usize> {
    // Extract the type portion between the collection prefix and the method name.
    let type_str = if let Some(rest) = name.strip_prefix("Vector__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Deque__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Set__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("HashSet__") {
        rest.strip_suffix("__new")?
    } else if let Some(rest) = name.strip_prefix("Heap__") {
        rest.strip_suffix("__new")?
    } else {
        // Dict/HashMap constructors are handled by dict_elem_sizes_from_monomorphized.
        return None;
    };
    Some(c_sizeof_with_structs(type_str, structs, aliases))
}

/// Extract the inner-type sizeof from a monomorphized concurrency constructor or
/// guard set call. Works for Mutex__T__new, Shared__T__new, RWLock__T__new,
/// Channel__T__new, Guard__T__set, WriteGuard__T__set.
pub(super) fn concurrency_elem_size(name: &str, structs: &[StructDef], aliases: &AliasMap) -> Option<usize> {
    // Try each prefix; the type sits between the prefix and the __method suffix.
    for prefix in &["Mutex__", "Shared__", "RWLock__", "Channel__", "Guard__", "WriteGuard__"] {
        if let Some(rest) = name.strip_prefix(prefix) {
            // Find the last `__method` segment.
            if let Some(idx) = rest.rfind("__") {
                let type_str = &rest[..idx];
                return Some(c_sizeof_with_structs(type_str, structs, aliases));
            }
        }
    }
    None
}

/// Extract key and value sizeof from a monomorphized Dict constructor name.
/// E.g., `Dict__Str__int64_t__new` → (sizeof(Str), sizeof(int64_t)) = (16, 8).
pub(super) fn dict_elem_sizes_from_monomorphized(name: &str, structs: &[StructDef], aliases: &AliasMap) -> (usize, usize) {
    // Dict__K__V__new or HashMap__K__V__new
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .and_then(|r| r.strip_suffix("__new"));
    if let Some(types) = rest {
        // Split on `__` to find key and value type names.
        // For simple types: "Str__int64_t" → key=Str, val=int64_t
        // For complex types: "int64_t__Str" → key=int64_t, val=Str
        // Heuristic: try splitting at each `__` boundary and pick the first valid split.
        if let Some(idx) = types.find("__") {
            let key = &types[..idx];
            let val = &types[idx + 2..];
            return (c_sizeof_with_structs(key, structs, aliases), c_sizeof_with_structs(val, structs, aliases));
        }
    }
    (8, 8) // fallback
}

/// Extract the key type name from a monomorphized Dict/HashMap name.
/// E.g., `Dict__GorgetString__int64_t__new` → Some("GorgetString").
pub(super) fn dict_key_type_from_monomorphized(name: &str) -> Option<String> {
    let rest = name.strip_prefix("Dict__")
        .or_else(|| name.strip_prefix("HashMap__"))
        .and_then(|r| r.strip_suffix("__new"))?;
    let idx = rest.find("__")?;
    Some(rest[..idx].to_string())
}

/// Extract the element type from a monomorphized Set/HashSet name.
/// E.g., `Set__GorgetString__new` → Some("GorgetString").
pub(super) fn set_elem_type_from_monomorphized(name: &str) -> Option<String> {
    let rest = name.strip_prefix("Set__")
        .or_else(|| name.strip_prefix("HashSet__"))
        .and_then(|r| r.strip_suffix("__new"))?;
    Some(rest.to_string())
}

/// Determine the elem_drop function name for a collection element type.
///
/// Reads `metadata.drop_strategy` from the type's TypeDef — every collection
/// type and runtime singleton carries this set at registration via
/// BuiltinTypeProtocol. Box[T] needs an override (the registered metadata
/// is `Trivial("free")` historical convention; the per-type
/// `Box__T__drop` wrapper is what actually frees inner resources).
/// Callable types now register via `c_runtime_alias = "GorgetClosure"` (Phase
/// A residual #1, sub-TODOs 1a + 1b closed 2026-05-05); the LIR-side
/// `c_runtime_alias` lookup remains as defensive backup for any path the
/// eager registration hasn't reached.
///
/// Returns None for trivially-droppable types (int, float, bool, ptr).
pub(super) fn elem_drop_fn_for_type(elem_type: &str, gir_types: &TypeRegistry) -> Option<String> {
    use crate::ir::types::DropStrategy;

    // Box[T] override: route per-element drop through the per-type wrapper
    // (emitted by `emit_box_drop_wrappers` in c_lir) so boxed allocs get
    // freed AND inner T resources recursively dropped. Trait boxes
    // (`{Trait}_TraitObj` registered) use a 16-byte `{data, vtable}`
    // layout and aren't currently supported as collection elements.
    if let Some(inner) = elem_type.strip_prefix("Box__") {
        let trait_obj = format!("{inner}_TraitObj");
        if gir_types.get_type_def(&trait_obj).is_none() {
            return Some(format!("Box__{inner}__drop"));
        }
        return None;
    }

    if let Some(td) = gir_types.get_type_def(elem_type) {
        if let DropStrategy::Trivial(ref f) = td.metadata.drop_strategy {
            return Some(f.clone());
        }
    }

    None
}

/// Determine the elem_clone function name for a collection element type.
///
/// Reads `metadata.clone_inplace_fn` — every collection type and runtime
/// singleton carries this set at registration via BuiltinTypeProtocol,
/// including Callable / MutCallable / ConsumeCallable / GorgetClosure
/// (Phase A residual #1).
pub(super) fn elem_clone_fn_for_type(elem_type: &str, gir_types: &TypeRegistry) -> Option<String> {
    if let Some(td) = gir_types.get_type_def(elem_type) {
        if let Some(ref f) = td.metadata.clone_inplace_fn {
            return Some(f.clone());
        }
    }

    None
}

/// Return the sizeof of an LIR type in bytes (best-effort for 64-bit targets).
pub(super) fn lir_type_sizeof(ty: &LirType) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 | LirType::F32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 => 8,
        LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8,
        LirType::Struct(_) => 8, // conservative; struct sizeof varies
        // Item 7e (Phase 1): resource sizes are fixed by C runtime ABI.
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::GorgetString => 32,
            crate::lir::ResourceKind::GorgetArray => 64,
            crate::lir::ResourceKind::GorgetMap | crate::lir::ResourceKind::GorgetSet => 192,
            crate::lir::ResourceKind::GorgetClosure => 16,
            crate::lir::ResourceKind::RefCounted => 8,
        },
        LirType::Void => 0,
    }
}

/// Map a GIR C type name to its sizeof in bytes.
/// `structs` is used to compute sizes of user-defined struct types.
/// `aliases` resolves monomorphized names (e.g. `Vector__int64_t`) to their
/// backing runtime StructDef so the typed `computed_c_size` is the source
/// of truth for collection-shape types (replaces prefix-match arms in
/// `opaque_runtime_size`).
///
/// Lookup order:
///   1. Primitive C types (int64_t, double, ...) — fixed-width by definition.
///   2. Registered `StructDef` — read `computed_c_size` (typed) or recurse via
///      `c_sizeof_struct_def`. Honors `c_runtime_alias` for closure-aliased
///      structs (Callable/MutCallable → GorgetClosure size).
///   3. `BuiltinTypeProtocol` `c_runtime_alias` lookup — for monomorphized
///      types whose LIR `StructDef` hasn't been built yet.
///   4. `struct_aliases` — typed name → StructId map for monomorphized
///      collection / Box / Task / Guard aliases (replaces prefix-match arms
///      that previously lived in `opaque_runtime_size`). One source of truth
///      per axis (layering-discipline §Rule 3).
///   5. `opaque_runtime_size` — canonical singleton-name table for runtime
///      handles whose monomorphized form has no registered StructDef
///      (Mutex__/Shared__/RWLock__/Channel__/Weak__/Thread__ → 8). Concrete
///      singleton entries (Socket, Match, …) live here too.
///   6. `Tuple__`/`Option__` — recursive structural sizes (compute, don't look up).
///   7. Pointer/opaque default — 8 bytes.
pub(super) fn c_sizeof_with_structs(type_name: &str, structs: &[StructDef], aliases: &AliasMap) -> usize {
    match type_name {
        "int64_t" | "uint64_t" | "double" => 8,
        "int32_t" | "uint32_t" | "float" => 4,
        "int16_t" | "uint16_t" => 2,
        "int8_t" | "uint8_t" | "bool" => 1,
        // GorgetString is a 32-byte fat struct { data, cap, len, alloc }
        "GorgetString" | "String" | "Str" => 32,
        _ => {
            // 2. Direct lookup on a registered StructDef.
            if let Some(sd) = structs.iter().find(|s| s.name == type_name) {
                // Phase A residual #1: Callable monomorphizations register
                // with `c_runtime_alias = "GorgetClosure"` — read the alias
                // size (typed) instead of falling through to a name-prefix
                // path. Resolves cross-module / pre-registration cases where
                // the GIR TypeDef registration didn't fire.
                if let Some(ref alias) = sd.c_runtime_alias {
                    if let Some(alias_sd) = structs.iter().find(|s| s.name == *alias) {
                        if let Some(sz) = alias_sd.computed_c_size {
                            return sz;
                        }
                    }
                    if let Some(sz) = opaque_runtime_size(alias) {
                        return sz;
                    }
                }
                return sd.computed_c_size.unwrap_or_else(|| c_sizeof_struct_def(sd, structs));
            }

            // 3. BuiltinTypeProtocol-tagged alias (Callable family →
            //    GorgetClosure). Pre-registration fallback for parsers that
            //    derive a type name from a generated function spelling (e.g.
            //    `Vector__Callable__GorgetClosure__new`).
            if let Some(alias) = crate::ir::lowering::builtins::c_runtime_alias_for_mangled_name(type_name) {
                if let Some(sd) = structs.iter().find(|s| s.name == alias) {
                    if let Some(sz) = sd.computed_c_size {
                        return sz;
                    }
                }
                if let Some(sz) = opaque_runtime_size(alias) {
                    return sz;
                }
            }

            // 4. `struct_aliases`: monomorphized collection / Box / Task /
            //    Guard names resolve to their backing runtime StructDef.
            //    Reads typed `computed_c_size` from the alias target —
            //    layering-discipline §Rule 3 (one source of truth per axis).
            //    Replaces former `name.starts_with("Vector__"|"Dict__"|…)`
            //    arms in `opaque_runtime_size`.
            if let Some(sid) = aliases.get(type_name) {
                if let Some(sd) = structs.get(sid.0 as usize) {
                    if let Some(sz) = sd.computed_c_size {
                        return sz;
                    }
                    return c_sizeof_struct_def(sd, structs);
                }
            }

            // 5. Canonical singleton-name size table — concrete runtime types
            //    (Socket, Match, ExecResult, …) and the genuinely opaque-pointer
            //    monomorphized families (Mutex__/Shared__/RWLock__/Channel__/
            //    Weak__/Thread__) which never get a registered StructDef
            //    because they're typedef'd to `void*` in C.
            if let Some(sz) = opaque_runtime_size(type_name) {
                return sz;
            }

            // 6. Recursive structural sizes (compute, don't look up).
            if let Some(rest) = type_name.strip_prefix("Tuple__") {
                return c_sizeof_tuple_fields(rest, structs, aliases);
            }
            if let Some(inner) = type_name.strip_prefix("Option__") {
                let payload = c_sizeof_with_structs(inner, structs, aliases);
                // struct { int32_t tag; <pad to 8>; T payload; }
                return 8 + std::cmp::max(payload, 8);
            }

            // 7. Pointer/opaque default.
            8
        }
    }
}

/// Compute the size of a struct from its LIR StructDef.
/// For union-layout enums (`is_union_layout == true`), uses union layout:
///   sizeof = align8(tag) + max(variant_size), aligned to 8
/// For regular structs, sums fields sequentially with alignment.
pub fn c_sizeof_struct_def(sd: &StructDef, structs: &[StructDef]) -> usize {
    if sd.is_union_layout && sd.fields.len() > 1 {
        // Union layout: tag (field 0) + union of variant groups.
        // tag is always I32 = 4 bytes, padded to 8 for union alignment.
        let tag_size = 8usize;

        // Group remaining fields by variant prefix (split on last '_').
        let mut max_variant_size = 0usize;
        let mut current_prefix = String::new();
        let mut current_variant_size = 0usize;

        for (name, ty) in &sd.fields[1..] {
            let prefix = name.rsplitn(2, '_').nth(1).unwrap_or(name).to_string();
            if prefix != current_prefix {
                // Finish previous variant group.
                if !current_prefix.is_empty() {
                    let aligned = (current_variant_size + 7) / 8 * 8;
                    max_variant_size = std::cmp::max(max_variant_size, aligned);
                }
                current_prefix = prefix;
                current_variant_size = 0;
            }
            let field_sz = c_sizeof_lir_type(ty, structs);
            let align = std::cmp::min(field_sz, 8);
            if align > 0 {
                current_variant_size = (current_variant_size + align - 1) / align * align;
            }
            current_variant_size += field_sz;
        }
        // Finish last variant group.
        if !current_prefix.is_empty() {
            let aligned = (current_variant_size + 7) / 8 * 8;
            max_variant_size = std::cmp::max(max_variant_size, aligned);
        }

        let total = tag_size + max_variant_size;
        // Align total to 8 bytes.
        (total + 7) / 8 * 8
    } else {
        // Regular struct: sum fields sequentially.
        let mut total = 0usize;
        let mut max_align = 1usize;
        for (_name, ty) in &sd.fields {
            let field_sz = c_sizeof_lir_type(ty, structs);
            let align = std::cmp::min(field_sz, 8).max(1);
            max_align = std::cmp::max(max_align, align);
            total = (total + align - 1) / align * align;
            total += field_sz;
        }
        // Align total to the struct's max field alignment.
        if max_align > 0 {
            total = (total + max_align - 1) / max_align * max_align;
        }
        total
    }
}

/// Known sizes for opaque runtime C structs that have no LIR fields
/// (declared as `struct X: pass` in Gorget, backed by a C typedef).
///
/// Shared by size-of calculations, LLVM struct emission, and aggregate-return
/// ABI decisions. Keeping the table in one place means both backends reach
/// the same layout; adding a new runtime struct requires one edit here.
///
/// The `*__T` monomorphized arms that previously lived here for
/// Vector/Deque/Dict/HashMap/Set/HashSet/Heap/Box/Task/Guard/ReadGuard/
/// WriteGuard have been retired: each now resolves through
/// `LirModule::struct_aliases` (Vector__/Dict__/Set__/Heap__) or a directly
/// registered StructDef (Box__/Task__/Guard__) where the typed
/// `computed_c_size` field is the source of truth. Callers reach the typed
/// path via `c_sizeof_with_structs(name, structs, aliases)` and via direct
/// `StructDef.computed_c_size` reads at sites that already hold the
/// StructDef.
///
/// What remains here are concrete singleton names plus the genuinely
/// opaque-pointer monomorphized families (Mutex__/Shared__/RWLock__/
/// Channel__/Weak__/Thread__) which never get a registered StructDef
/// because they typedef to `void*` in C — there is no struct to read a
/// `computed_c_size` from. Those last six prefix arms are the typed-
/// metadata floor: removing them requires registering a stub StructDef
/// for each (architectural change beyond TODO scope).
pub fn opaque_runtime_size(name: &str) -> Option<usize> {
    let sz = match name {
        // Core collections (layouts match c_runtime.rs typedefs).
        "GorgetString" | "Str" => 32,
        "GorgetArray" => 64,
        "GorgetMap" | "GorgetSet" => 192,
        "GorgetClosure" => 16,
        "GorgetRange" => 24,
        // Concurrency opaque handles — pointer-sized.
        "AtomicInt" | "AtomicBool" | "Mutex" | "Shared" | "RWLock" | "Barrier"
        | "CondVar" | "WaitGroup" | "Semaphore" | "OnceFlag" | "TaskGroup"
        | "Weak" | "Executor" | "BlockingPool" | "FileWatcher" | "Reactor"
        | "GuestModule" | "Thread" => 8,
        // Channel[T] is a pointer to GorgetChannel.
        "Channel" => 8,
        // Per-type concurrency typedefs alias to opaque pointers (`void*`)
        // — these are deliberately NOT registered as StructDefs, so there
        // is no `computed_c_size` to read. The prefix-match here is the
        // typed-metadata floor: every other mangled prefix has been
        // retired into `struct_aliases` / direct StructDef reads.
        _ if name.starts_with("Mutex__") || name.starts_with("Shared__")
            || name.starts_with("RWLock__") || name.starts_with("Channel__")
            || name.starts_with("Weak__") || name.starts_with("Thread__") => 8,
        // Sockets / files / process — runtime uses fixed layouts.
        "Socket" | "ServerSocket" | "UdpSocket" => 8,
        "TlsSocket" => 24,        // {int64_t fd; SSL_CTX* ctx; SSL* ssl}
        "TlsServerSocket" => 16,  // {int64_t fd; SSL_CTX* ctx}
        "UdpAddr" => 40,   // {Str host, int64_t port} = 32 + 8
        "UdpPacket" => 104,// {GorgetArray data, UdpAddr sender} = 64 + 40
        "File" | "GorgetFile" => 16,
        "Process" => 48,   // {pid, stdin/out/err fds, status, owned}
        // ExecResult mirrors c_runtime.rs `{ Str output, Str errors, int64_t exit_code }`
        // and the Gorget `struct ExecResult { String output; String errors; int exit_code }`.
        // 32 + 32 + 8 = 72 bytes. Previous 48 here truncated the memcpy back from
        // process_read_all's sret buffer, leaving exit_code uninitialized.
        "ExecResult" => 72,
        // Regex/Match are now pure-Gorget structs (lib/xtd/regex.gg) — no
        // runtime-layout entry; the normal user-struct size path handles them.
        // Allocators — each has its own fixed layout.
        "Arena" => 64,       // fields vary; treat as 64-byte allocator handle
        "ArenaCheckpoint" => 16,
        "PoolAllocator" | "TlsfAllocator" | "FixedBufferAllocator"
        | "FallbackAllocator" | "TrackingAllocator" => 8,
        // Crypto.
        "CipherContext" => 8,
        "BigNum" | "RSAKey" => 8,
        // GorgetEd25519KeyPair / GorgetX25519KeyPair are each `{ EVP_PKEY* pkey }`
        // in c_runtime.rs — single pointer, 8 bytes. The previous 64-byte size
        // here misled is_small_aggregate into thinking they were big enough to
        // require sret return ABI; the actual runtime returns them in x0
        // (≤16-byte pass-through). Mismatch caused SEGV on x25519_keygen and
        // friends under --backend=llvm.
        "Ed25519KeyPair" | "X25519KeyPair" => 8,
        // SDL.
        "SDLWindow" | "SDLRenderer" | "SDLTexture" | "SDLFont" | "SDLEvent" => 8,
        // Audio. GorgetAudioChunk is `{Mix_Chunk*}` = 8B, passed by value to
        // gorget_audio_play_channel; a 16B size would let `max()` activate a
        // latent 8B over-read past the slot.
        "AudioChunk" => 8,
        _ => return None,
    };
    Some(sz)
}

/// Field layout for opaque runtime structs that the backends need to *return
/// or pass by value at ABI boundaries*. When a struct is just memcpy'd around
/// as a blob the opaque `[N x i8]` layout works, but as soon as it crosses a
/// C calling-convention boundary (return value, by-value param) the LLVM
/// backend has to declare it with the actual field types — otherwise AArch64's
/// `[N x i8]` HFA-disqualification path takes over and the function gets
/// returned via sret/memory instead of register pairs, mismatching the C
/// runtime's actual ABI.
///
/// Returns `None` for structs that are only ever passed by pointer (most of
/// them) — those keep their `[N x i8]` opaque layout. Returns `Some(fields)`
/// for structs that are returned/passed by value (e.g. Regex from
/// `gorget_regex_compile`).
///
/// Keep this in sync with the runtime's actual C struct definitions.
pub fn opaque_runtime_layout(name: &str) -> Option<Vec<LirType>> {
    match name {
        // No runtime opaque structs need explicit field layout right now.
        // (Regex/Match were here when xtd.regex was a PCRE2 wrapper.)
        _ => None,
    }
}

/// Returns true if a struct type is small enough to be returned / passed
/// in registers (≤16 bytes on aarch64, ≤8 bytes on x86-64). Used by both
/// backends to decide sret vs direct-return and, via the `ByValue` ABI
/// tag on closure calls, whether to `load` the struct before the call.
///
/// Kept in sync with the `is_small_aggregate` shim in `src/backend/llvm/mod.rs`,
/// which is a thin re-export.
pub fn is_small_aggregate(ty: &LirType, structs: &[StructDef]) -> bool {
    if let LirType::Struct(sid) = ty {
        let sdef = match structs.get(sid.0 as usize) {
            Some(s) => s,
            None => return false,
        };
        // Typed read: `computed_c_size` is the canonical size set at
        // registration (runtime singletons) or via `compute_struct_sizes()`.
        // Falls back to the runtime singleton-name table for pre-compute
        // calls and structurally-sized recursion otherwise. Replaces a
        // former unconditional `opaque_runtime_size(&sdef.name)` short-
        // circuit that fired for every monomorphized prefix-matched name.
        if let Some(cs) = sdef.computed_c_size {
            return cs <= 16;
        }
        if let Some(sz) = opaque_runtime_size(&sdef.name) {
            return sz <= 16;
        }
        c_sizeof_lir_type(ty, structs) <= 16
    } else {
        false
    }
}

/// Compute sizeof for an LirType.
pub fn c_sizeof_lir_type(ty: &LirType, structs: &[StructDef]) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8,
        LirType::F32 => 4,
        LirType::Struct(sid) => {
            if let Some(sd) = structs.get(sid.0 as usize) {
                // Typed read — `computed_c_size` is set at registration for
                // runtime singletons (GorgetArray = 64, GorgetMap = 192, …)
                // and populated by `compute_struct_sizes()` for everyone
                // else. Falls back to the singleton-name table for
                // pre-compute calls (during `compute_struct_sizes` itself)
                // and structurally-sized recursion otherwise.
                if let Some(cs) = sd.computed_c_size {
                    return cs;
                }
                if let Some(sz) = opaque_runtime_size(&sd.name) {
                    return sz;
                }
                c_sizeof_struct_def(sd, structs)
            } else {
                8
            }
        }
        // Item 7e (Phase 1): resource sizes are fixed by C runtime ABI.
        LirType::Resource { kind, .. } => match kind {
            crate::lir::ResourceKind::GorgetString => 32,
            crate::lir::ResourceKind::GorgetArray => 64,
            crate::lir::ResourceKind::GorgetMap | crate::lir::ResourceKind::GorgetSet => 192,
            crate::lir::ResourceKind::GorgetClosure => 16,
            crate::lir::ResourceKind::RefCounted => 8,
        },
        LirType::Void => 0,
    }
}

/// Compute the C alignment of an LirType (in bytes).
/// Scalars align to their natural size (capped at 8). Aggregates align to
/// the max of their field alignments. Used by the LLVM backend to insert
/// inter-field padding matching the C ABI.
pub fn c_alignof_lir_type(ty: &LirType, structs: &[StructDef]) -> usize {
    match ty {
        LirType::I8 | LirType::U8 | LirType::Bool => 1,
        LirType::I16 | LirType::U16 => 2,
        LirType::I32 | LirType::U32 | LirType::F32 => 4,
        LirType::I64 | LirType::U64 | LirType::F64 | LirType::Ptr | LirType::PtrTo(_) | LirType::FuncRef => 8,
        LirType::Struct(sid) => {
            if let Some(sd) = structs.get(sid.0 as usize) {
                if let Some(a) = sd.computed_c_align {
                    return a;
                }
                // Union-layout enums always align to 8 (payload contains i64/ptr)
                if sd.is_union_layout { return 8; }
                // Recursive: max of field alignments
                sd.fields.iter()
                    .map(|(_, fty)| c_alignof_lir_type(fty, structs))
                    .max()
                    .unwrap_or(1)
                    .min(8)
            } else {
                8
            }
        }
        // Item 7e (Phase 1): resource alignments match the C runtime ABI
        // (pointer-aligned to 8 for every resource).
        LirType::Resource { .. } => 8,
        LirType::Void => 1,
    }
}

/// Compute the size of a tuple from its mangled field types.
/// `Tuple__int64_t__Str` → fields are [int64_t, Str] → 8 + 32 = 40.
/// Fields are split on `__` but multi-word types like `int64_t` contain `_`
/// (not `__`), so we split on `__` and rejoin single-underscore segments.
pub(super) fn c_sizeof_tuple_fields(fields_str: &str, structs: &[StructDef], aliases: &AliasMap) -> usize {
    let mut total = 0usize;
    // Split on __ delimiter.  Type names use single _ (int64_t, uint8_t).
    for part in fields_str.split("__") {
        if part.is_empty() { continue; }
        let field_sz = c_sizeof_with_structs(part, structs, aliases);
        // Align each field to its natural alignment (max 8).
        let align = std::cmp::min(field_sz, 8);
        if align > 0 {
            total = (total + align - 1) / align * align;
        }
        total += field_sz;
    }
    // Align total to 8 bytes (struct padding).
    let align = 8;
    total = (total + align - 1) / align * align;
    total
}

pub(super) fn lower_global_init(init: &ir::GlobalInit, func_index: &std::collections::HashMap<String, FuncId>, target_ty: &LirType, struct_reg: &StructRegistry, gir_types: &crate::ir::types::TypeRegistry) -> LirGlobalInit {
    match init {
        ir::GlobalInit::Zeroed => LirGlobalInit::Zeroed,
        ir::GlobalInit::Bytes(b) => LirGlobalInit::Bytes(b.clone()),
        ir::GlobalInit::FnRef(name) => {
            if let Some(fid) = func_index.get(name) {
                LirGlobalInit::FuncAddr(*fid)
            } else {
                LirGlobalInit::Zeroed
            }
        }
        // Trait-object vtable drop slot: forward the typed inner-type name.
        // NOT routed through `FnRef` — the `Box__<inner>__drop` wrapper is
        // backend-synthesized (no GIR/LIR function), so the FnRef arm would
        // silently lower it to `Zeroed` (a NULL drop slot).
        ir::GlobalInit::BoxDropRef(inner) => LirGlobalInit::BoxDropAddr(inner.clone()),
        ir::GlobalInit::Struct { type_name, fields } => {
            // Resolve struct_id from the GIR type_name. Falls back to the
            // target type's struct if the registry doesn't have it yet,
            // then StructId(0) if neither is available (trivial structs).
            let struct_id = struct_reg.lookup(type_name)
                .or_else(|| match target_ty {
                    LirType::Struct(sid) | LirType::PtrTo(sid) => Some(*sid),
                    _ => None,
                })
                .unwrap_or(StructId(0));
            // `type_name` is the primary key for `struct_id` (see the
            // `GlobalInit::Struct` arm above); the const-value emitter reads it
            // straight off the GIR `Named` type so it always resolves. Field
            // recursion keeps the existing I64 target — the C/LLVM `Struct`
            // emit reads field types from `structs[struct_id]`, not this target.
            LirGlobalInit::Struct {
                struct_id,
                fields: fields.iter().map(|(_, f)| lower_global_init(f, func_index, &LirType::I64, struct_reg, gir_types)).collect(),
            }
        }
        ir::GlobalInit::StaticArrayView { elem_type_name, elems } => {
            // Resolve the element type name → element LirType via the same
            // helper the cross-module Result/Option field resolver uses; the
            // backends spell the C / LLVM element type from this typed handle.
            let elem_ty = super::component_to_lir_type(elem_type_name, struct_reg, gir_types);
            LirGlobalInit::StaticArrayView {
                elem_ty: elem_ty.clone(),
                elems: elems.iter()
                    .map(|e| lower_global_init(e, func_index, &elem_ty, struct_reg, gir_types))
                    .collect(),
            }
        }
        ir::GlobalInit::Extern { name, args } => {
            let lir_args: Vec<LirGlobalInitArg> = args.iter()
                .map(lower_global_init_arg)
                .collect();
            // Concurrency-ctor remap: `Mutex__T__new(v)` → `gorget_mutex_new(sizeof(T), &(T){v})`.
            // Same shape for `Shared__T__new` and `RWLock__T__new`. The runtime
            // takes (size, ptr-to-initial-value); the AddrOfInline arg lets the
            // backend allocate the temporary inline.
            if let Some(mapped) = map_monomorphized_to_runtime(name) {
                if matches!(mapped.as_str(), "gorget_mutex_new" | "gorget_shared_new" | "gorget_rwlock_new") {
                    let elem_type = name
                        .strip_prefix("Mutex__").or_else(|| name.strip_prefix("Shared__"))
                        .or_else(|| name.strip_prefix("RWLock__"))
                        .and_then(|r| r.rsplit_once("__").map(|(t, _)| t))
                        .unwrap_or("int64_t");
                    let initial_value = lir_args.into_iter().next()
                        .unwrap_or(LirGlobalInitArg::Int(0));
                    return LirGlobalInit::Extern {
                        name: mapped,
                        args: vec![
                            LirGlobalInitArg::Sizeof(elem_type.to_string()),
                            LirGlobalInitArg::AddrOfInline {
                                c_type: elem_type.to_string(),
                                value: Box::new(initial_value),
                            },
                        ],
                    };
                }
                return LirGlobalInit::Extern { name: mapped, args: lir_args };
            }
            // Type-targeted shortcut: a runtime ctor whose result is a primitive
            // value (int / float) is constant-foldable when the only arg is a
            // matching literal. Skips the runtime call entirely. Mainly useful
            // for `static int x = some_const_extern()` patterns once they exist.
            if target_ty.is_float() {
                if let Some(LirGlobalInitArg::Float(f)) = lir_args.first() {
                    return LirGlobalInit::Bytes(f.to_le_bytes().to_vec());
                }
            }
            LirGlobalInit::Extern { name: name.clone(), args: lir_args }
        }
    }
}

fn lower_global_init_arg(arg: &ir::GlobalInitArg) -> LirGlobalInitArg {
    match arg {
        ir::GlobalInitArg::Int(n) => LirGlobalInitArg::Int(*n),
        ir::GlobalInitArg::Float(f) => LirGlobalInitArg::Float(*f),
        ir::GlobalInitArg::Bool(b) => LirGlobalInitArg::Bool(*b),
        ir::GlobalInitArg::Sizeof(t) => LirGlobalInitArg::Sizeof(t.clone()),
        ir::GlobalInitArg::StrLit(s) => LirGlobalInitArg::StrLit(s.clone()),
        ir::GlobalInitArg::AddrOfInline { c_type, value } => LirGlobalInitArg::AddrOfInline {
            c_type: c_type.clone(),
            value: Box::new(lower_global_init_arg(value)),
        },
    }
}

/// Top-level entry point: lower a GIR module to LIR.
pub fn lower_module(gir: &ir::Module) -> LirModule {
    let ctx = LoweringContext::new(gir);
    let module = ctx.lower();

    // Tier 1d structural guard (unconditional; release + debug + tests):
    // every regular `Box[T]` `StructDef` must carry typed `box_inner_type`
    // metadata so the C backend's `emit_box_drop_wrappers` /
    // `emit_runtime_helpers` passes can emit the per-type
    // `Box__<inner>__drop` wrapper and `__gorget_box_alloc_<inner>` /
    // `__gorget_box_free_<inner>` helpers without name-prefix scanning.
    // Snag #13's family — see commit `c7a652f0` for the original fix and
    // `docs/devbook/25-structural-guards.md` Tier 1d for the rationale.
    //
    // The same validator also runs per-pass under `assert_module_valid` via
    // the `VALIDATORS` registry, but that path is debug-only / env-gated
    // (`GG_VALIDATE_PASSES`); the unconditional check here ensures release
    // CI catches a regressing registration site at the LIR exit boundary.
    let box_errors = super::super::validate::validate_box_inner_type(&module);
    if !box_errors.is_empty() {
        eprintln!("[box-inner-type] {} violation(s):", box_errors.len());
        for e in &box_errors {
            eprintln!("  {e}");
        }
        panic!(
            "LIR module failed Box-inner-type validation ({} violation(s))",
            box_errors.len()
        );
    }

    #[cfg(debug_assertions)]
    {
        for func in &module.functions {
            for block in &func.blocks {
                debug_assert_eq!(
                    block.span_map.len(),
                    block.insts.len(),
                    "LIR block {:?} in fn '{}' violates span_map.len() == insts.len() invariant",
                    block.id,
                    func.name,
                );
            }
        }
    }

    module
}

/// Convert a GIR TypeId to its C type name (for spawn metadata).
/// Note: returns "GorgetString" for GorgetString so Task type names match the
/// mangling used by Task[str] user annotations (Task__GorgetString, not Task__GorgetString).
pub(super) fn gir_type_to_c(type_id: gir_types::TypeId, registry: &TypeRegistry) -> String {
    use gir_types::*;
    if type_id == BOOL_TYPE { return "bool".into(); }
    if type_id == I8_TYPE { return "int8_t".into(); }
    if type_id == I16_TYPE { return "int16_t".into(); }
    if type_id == I32_TYPE { return "int32_t".into(); }
    if type_id == I64_TYPE { return "int64_t".into(); }
    if type_id == U8_TYPE { return "uint8_t".into(); }
    if type_id == U16_TYPE { return "uint16_t".into(); }
    if type_id == U32_TYPE { return "uint32_t".into(); }
    if type_id == U64_TYPE { return "uint64_t".into(); }
    if type_id == F32_TYPE { return "float".into(); }
    if type_id == F64_TYPE { return "double".into(); }
    if type_id == UNIT_TYPE { return "void".into(); }
    if let Some(gir_type) = registry.get(type_id) {
        match gir_type {
            GirType::Ptr(inner) if *inner == U8_TYPE => "const char*".into(),
            GirType::Ptr(inner) => format!("const {}*", gir_type_to_c(*inner, registry)),
            GirType::MutPtr(inner) => format!("{}*", gir_type_to_c(*inner, registry)),
            GirType::Named(name) => {
                // Map collection instantiations to runtime struct names.
                if let Some(rt) = collection_runtime_type(name, registry) {
                    rt.into()
                } else if is_opaque_pointer_type(name) {
                    // Opaque types are lowered to Ptr (void*) in LIR.
                    "void*".into()
                } else if let Some(rt) = opaque_runtime_type_name(name) {
                    rt.into()
                } else {
                    name.clone()
                }
            }
            GirType::FnPtr { .. } => "void*".into(),
            _ => format!("int64_t"), // fallback
        }
    } else {
        "int64_t".into()
    }
}

/// Convert a GIR TypeId to C type for spawn context fields.
/// Callable params (FnPtr) become void*; void becomes void*.
pub(super) fn spawn_param_c_type(type_id: gir_types::TypeId, registry: &TypeRegistry) -> String {
    if matches!(registry.get(type_id), Some(GirType::FnPtr { .. })) {
        return "void*".into();
    }
    let c = gir_type_to_c(type_id, registry);
    if c == "void" { "void*".into() } else { c }
}
