//! Call and extern dispatch helpers for GIR → LIR lowering.

use super::*;

/// Printf argument type — used to select the correct format specifier.
#[derive(Clone, Copy, PartialEq)]
pub enum PrintfArgKind {
    Int,
    Float,
    Str,
    Bool,
}

/// Rewrite printf format specifiers to match actual argument types.
/// `arg_kinds` is indexed from arg[1] onward (arg[0] is the format string).
///
/// Rewrites at each `%lld` position:
///   Str   → `%.*s`  (string expanded to len+data pair)
///   Float → `%f`    (C variadic promotes float to double)
///   Bool  → `%s`    (bool converted to "true"/"false" via gorget_bool_to_str)
///   Int   → `%lld`  (unchanged)
pub fn fix_printf_format(fmt: &str, arg_kinds: &[PrintfArgKind]) -> String {
    let mut result = String::with_capacity(fmt.len() + 8);
    let mut arg_idx = 0usize;
    let bytes = fmt.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 1 < bytes.len() {
            if bytes[i + 1] == b'%' {
                result.push_str("%%");
                i += 2;
                continue;
            }
            // Check if this is %lld
            if i + 4 <= bytes.len() && &bytes[i..i+4] == b"%lld" {
                let kind = arg_kinds.get(arg_idx).copied().unwrap_or(PrintfArgKind::Int);
                match kind {
                    PrintfArgKind::Str => result.push_str("%.*s"),
                    PrintfArgKind::Float => result.push_str("%f"),
                    PrintfArgKind::Bool => result.push_str("%.*s"),
                    PrintfArgKind::Int => result.push_str("%lld"),
                }
                arg_idx += 1;
                i += 4;
                continue;
            }
            // Check if this is %s that needs rewriting for Bool or Str (expand to %.*s)
            if i + 2 <= bytes.len() && &bytes[i..i+2] == b"%s" {
                let kind = arg_kinds.get(arg_idx).copied().unwrap_or(PrintfArgKind::Int);
                if matches!(kind, PrintfArgKind::Bool | PrintfArgKind::Str) {
                    // Bool: gorget_bool_to_str produces a (len, data) pair.
                    // Str:  32-byte struct decomposed to (int)str.len, (const char*)str.data.
                    result.push_str("%.*s");
                } else {
                    result.push_str("%s");
                }
                arg_idx += 1;
                i += 2;
                continue;
            }
            // Other format specifiers: scan past them
            let start = i;
            i += 1;
            while i < bytes.len() && !bytes[i].is_ascii_alphabetic() && bytes[i] != b'%' {
                i += 1;
            }
            if i < bytes.len() && bytes[i].is_ascii_alphabetic() {
                i += 1;
            }
            result.push_str(&fmt[start..i]);
            arg_idx += 1;
        } else {
            let ch = fmt[i..].chars().next().unwrap();
            result.push(ch);
            i += ch.len_utf8();
        }
    }
    result
}

pub(super) fn lower_binop(dst: ValueId, op: GirBinOp, lhs: ValueId, rhs: ValueId, ty: LirType) -> Inst {
    // Plain `+`/`-`/`*` always check overflow (panic, or `catch Fault.Overflow`
    // recovers). The wrapping `+%`/`-%`/`*%` ops below emit `Overflow::Wrap`
    // explicitly. There is no global "wrap" mode.
    let default_overflow = Overflow::Trap;
    match op {
        GirBinOp::Add => Inst::Add { dst, ty, lhs, rhs, overflow: default_overflow },
        GirBinOp::Sub => Inst::Sub { dst, ty, lhs, rhs, overflow: default_overflow },
        GirBinOp::Mul => Inst::Mul { dst, ty, lhs, rhs, overflow: default_overflow },
        GirBinOp::Div => Inst::Div { dst, ty, lhs, rhs },
        GirBinOp::Rem => Inst::Rem { dst, ty, lhs, rhs },
        GirBinOp::Mod => Inst::Mod { dst, ty, lhs, rhs },
        GirBinOp::Pow => {
            // Pow doesn't have a direct LIR instruction. Emit as CallExtern to pow().
            // For now, emit as Mul (placeholder).
            Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Trap }
        }
        GirBinOp::BitAnd => Inst::BitAnd { dst, ty, lhs, rhs },
        GirBinOp::BitOr => Inst::BitOr { dst, ty, lhs, rhs },
        GirBinOp::BitXor => Inst::BitXor { dst, ty, lhs, rhs },
        GirBinOp::Shl => Inst::Shl { dst, ty, lhs, rhs },
        GirBinOp::Shr => Inst::Shr { dst, ty, lhs, rhs },
        GirBinOp::AddWrap => Inst::Add { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
        GirBinOp::SubWrap => Inst::Sub { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
        GirBinOp::MulWrap => Inst::Mul { dst, ty, lhs, rhs, overflow: Overflow::Wrap },
    }
}

pub(super) fn lower_unop(dst: ValueId, op: GirUnOp, operand: ValueId, ty: LirType) -> Inst {
    match op {
        GirUnOp::Neg => Inst::Neg { dst, ty, operand },
        GirUnOp::Not => Inst::Not { dst, operand },
        GirUnOp::BitNot => Inst::BitNot { dst, ty, operand },
    }
}

pub(super) fn map_cmp_op(op: GirCmpOp) -> CmpOp {
    match op {
        GirCmpOp::Eq => CmpOp::Eq,
        GirCmpOp::Ne => CmpOp::Ne,
        GirCmpOp::Lt => CmpOp::Lt,
        GirCmpOp::Le => CmpOp::Le,
        GirCmpOp::Gt => CmpOp::Gt,
        GirCmpOp::Ge => CmpOp::Ge,
    }
}


/// Returns the clone function name for a collection element type that is
/// itself a collection or string. These types support deep cloning via
/// runtime functions and should be cloned on IndexLoad rather than moved,
/// so the collection retains the original element.
///
/// Other resource types (Task, user structs, etc.) are still moved+zeroed
/// since they may not support cloning or may be intentionally consumed.
pub(super) fn clone_fn_for_collection_element(
    elem_type_name: &str,
    gir_types: &crate::ir::types::TypeRegistry,
    structs: &[crate::lir::StructDef],
) -> Option<String> {
    // Reads `metadata.clone_fn` from the type's TypeDef — every collection
    // type and runtime singleton carries this set at registration via
    // BuiltinTypeProtocol (see src/ir/lowering/types.rs and mod.rs),
    // including Callable / MutCallable / ConsumeCallable / GorgetClosure
    // (Phase A residual #1).
    if let Some(td) = gir_types.get_type_def(elem_type_name) {
        if let Some(ref f) = td.metadata.clone_fn {
            return Some(f.clone());
        }
    }

    // Phase A residual #1 fallback: typed read from the LIR StructDef.
    // GIR TypeDef may be missing for Callable element types when the
    // collection arose via mangling-only paths (no `map_ast_type_mut` for
    // the inner Callable). The LIR StructDef carries `c_runtime_alias =
    // "GorgetClosure"` in those cases — read it as the authoritative
    // source-of-truth for the runtime layout. Without this, `Vector[
    // Callable]`-via-method-monomorphization paths drop the deep-clone on
    // collection read and SEGV on the next iteration.
    if let Some(sd) = structs.iter().find(|s| s.name == elem_type_name) {
        if sd.c_runtime_alias.as_deref() == Some("GorgetClosure") {
            return Some("gorget_closure_clone_to_owned".to_string());
        }
    }

    None
}

/// Map monomorphized GIR function names to their C runtime equivalents.
/// E.g., `Vector__GorgetString__push` → `gorget_array_push`,
///       `Dict__GorgetString__int64_t__put` → `gorget_map_put`,
///       `GorgetString__to_upper` → `gorget_str_to_upper`.
/// Returns true if `s` is a known C type name (indicating the "method" part of a
/// monomorphized name is actually a type parameter, not a method name).
/// Map a monomorphized element-type name to a qsort comparator suffix.
/// Matches the typed stub families emitted by `emit_types.rs` — int
/// uses a value-wise i64 compare, float uses value-wise double,
/// Str uses lexical, anything else falls back to memcmp on `elem_size`.
pub(super) fn cmp_suffix_for_elem(elem: &str) -> &'static str {
    match elem {
        "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t" => "int",
        "double" | "float" => "float",
        "Str" | "GorgetString" => "str",
        _ => "generic",
    }
}

/// Map a typed LirType element to a qsort comparator suffix.
///
/// Item 7e-r2: the typed counterpart to `cmp_suffix_for_elem`. Reads the
/// element type from an operand's `LirType::Resource { params }` so the
/// sort/sorted/unique dispatch no longer needs to strip the `Vector__`
/// prefix off the monomorphized callee name (per layering-discipline
/// rule 2 — no name matching at the read site).
///
/// Returns `None` when the operand isn't a `Resource { kind: GorgetArray }`
/// (e.g., the writer at `map_gir_type_with_structs` hasn't been migrated
/// to emit Resource for this operand yet — that's 7e-r1's job). The
/// caller falls back to the legacy name-stripping path in that case.
pub(super) fn cmp_suffix_from_lir_type(ty: &LirType) -> &'static str {
    match ty {
        LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64
        | LirType::U8 | LirType::U16 | LirType::U32 | LirType::U64 => "int",
        LirType::F32 | LirType::F64 => "float",
        LirType::Resource { kind: crate::lir::ResourceKind::GorgetString, .. } => "str",
        _ => "generic",
    }
}

pub(super) fn is_type_name(s: &str) -> bool {
    matches!(s, "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "GorgetString"
        | "GorgetArray" | "GorgetMap" | "GorgetSet" | "void"
        | "T" | "U" | "V")
}

pub(super) fn map_monomorphized_to_runtime_with_table(
    name: &str,
    table: &rustc_hash::FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
) -> Option<String> {
    // Check the protocol-populated table first (covers all builtins).
    if let Some(info) = table.get(name) {
        return Some(info.name.clone());
    }
    // Fall through to legacy name-based mapping for types not in the table.
    map_monomorphized_to_runtime(name)
}

/// Item 7e-r2: typed overload that consults operand `LirType`s for the
/// sort/sorted/unique within-arm dispatch instead of stripping `Vector__`
/// off the monomorphized callee name.
///
/// `operand_types[0]` is the receiver (the `Vector` self). When it
/// arrives as `LirType::Resource { kind: GorgetArray, params: [elem] }`,
/// the qsort comparator suffix is derived from `elem`'s `LirType` — no
/// name parsing.
///
/// Falls back to `map_monomorphized_to_runtime_with_table` for every
/// other case (the legacy name-strip path still inside
/// `map_monomorphized_to_runtime` handles operands that haven't been
/// migrated to typed Resource — that's 7e-r1's job).
pub(super) fn map_monomorphized_to_runtime_with_operand_types(
    name: &str,
    operand_types: &[Option<LirType>],
    table: &rustc_hash::FxHashMap<String, crate::ir::RuntimeCalleeInfo>,
) -> Option<String> {
    // Family-route via the resources table — same shape as the legacy
    // path (commits e129746e + 7bb75bf4) so the typed branch only
    // activates for genuine gorget_array sort/sorted/unique calls.
    let family = crate::resources::table().lookup(name)
        .and_then(|m| m.method_prefix.as_deref());
    if family == Some("gorget_array") {
        let method = name.rsplit("__").next();
        if matches!(method, Some("sort") | Some("sorted") | Some("unique")) {
            // Typed fast path: receiver's LirType carries the element.
            if let Some(Some(LirType::Resource { kind: crate::lir::ResourceKind::GorgetArray, params })) = operand_types.first() {
                if let Some(elem_ty) = params.first() {
                    let suffix = cmp_suffix_from_lir_type(elem_ty);
                    let m = method.expect("method matched above");
                    return Some(format!("gorget_array_{m}_{suffix}"));
                }
            }
            // Operand wasn't typed Resource — fall through to the legacy
            // name-strip path. Pre-7e-r1, most receivers still arrive as
            // `LirType::Struct(sid)`, so this is the common case today.
        }
    }
    map_monomorphized_to_runtime_with_table(name, table)
}

pub(super) fn map_monomorphized_to_runtime(name: &str) -> Option<String> {
    // Family classification reads from `compiler/data/resources.gg`'s typed
    // `method_prefix` field (per layering-discipline rule 2 — no string-prefix
    // dispatch at the read site). The within-arm method-name dispatch
    // (constructor detection, sort element-type variants, higher-order
    // inline returns, etc.) stays as-is — only the "which family is this?"
    // head was the name-matched part.
    //
    // See docs/devbook/18-runtime-abi.md (the resource table / runtime
    // declaration table) for the SSoT design.
    let family = crate::resources::table().lookup(name)
        .and_then(|m| m.method_prefix.as_deref());

    // Vector__T__method → gorget_array_method
    // GorgetArray__method → gorget_array_method  (non-generic array calls)
    // Higher-order methods (filter, map, fold, any, all, each, reduce, flat_map, find, find_index)
    // are NOT runtime functions — they are generated inline by the c_lir backend.
    // Keep them as their original monomorphized names so the backend can detect and generate them.
    if family == Some("gorget_array") {
        let method = name.rsplit("__").next()?;
        // Guard: if the "method" is actually a type name (int64_t, double, etc.),
        // this is a constructor call like Vector__int64_t(cap), not a method call.
        // Keep the original name — the c_lir backend handles these constructors specially.
        if is_type_name(method) {
            return None;
        }
        match method {
            "filter" | "map" | "flat_map" | "fold" | "reduce" | "any" | "all"
            | "each" | "find" | "find_index" | "sorted_by" | "sort_by"
            | "sorted_by_key" | "sort_by_key"
            | "count"
            // `iter` and `drain` are user-space (Iterable / Drainable
            // trait equips in lib/std/iter.gg) — thin wrappers that
            // construct an iterator state machine; no runtime fn.
            | "iter" | "drain" => return None,
            // `windows` / `chunks` route to generic runtime stubs that
            // use the source array's `elem_size` field — one stub covers
            // every element type, no per-type variants needed.
            "windows" | "chunks" => {
                return Some(format!("gorget_array_{method}"));
            }
            // sort/sorted/unique dispatch to typed stubs emitted by
            // emit_types.rs, keyed by element type so qsort uses the
            // right comparator:
            //   Vector__int64_t__sort → gorget_array_sort_int
            //   Vector__double__sort  → gorget_array_sort_float
            //   Vector__Str__sort     → gorget_array_sort_str
            //   Vector__Foo__sort     → gorget_array_sort_generic
            // Deque shares Vector's underlying gorget_array runtime, so
            // `Deque__T__sort` MUST route to the same typed comparator
            // (Round XXVII Track B — Core #4 sibling arm-add; pre-fix the
            // Deque__-prefixed name fell to `generic` (memcmp) and produced
            // wrong-order output on int (negatives sort after positives)
            // and String (heap-address ordering)).
            "sort" | "sorted" | "unique" => {
                let elem = name
                    .strip_prefix("Vector__")
                    .or_else(|| name.strip_prefix("Deque__"))
                    .and_then(|rest| {
                        rest.strip_suffix(&format!("__{method}"))
                    });
                let suffix = elem
                    .map(cmp_suffix_for_elem)
                    .unwrap_or("generic");
                return Some(format!("gorget_array_{method}_{suffix}"));
            }
            // Vector.get() returns Option[T] — use safe (non-panicking) get.
            "get" => return Some("gorget_array_safe_get".into()),
            // Vector.pop() returns Option[T] — use safe (non-panicking) pop.
            "pop" => return Some("gorget_array_safe_pop".into()),
            // Vector.remove() returns Option[T] — use the opt variant (returns void*).
            "remove" => return Some("gorget_array_remove_opt".into()),
            _ => return Some(format!("gorget_array_{method}")),
        }
    }
    // Dict__K__V__method → gorget_dict_new for "new", gorget_map_* for all others
    // HashMap__K__V__method / GorgetMap__method → gorget_map_method (unordered)
    // Higher-order methods (filter, fold, each, any, all, map) and non-runtime methods
    // (update, get_or, get_or_put) keep their monomorphized names for inline codegen.
    if family == Some("gorget_map") {
        let method = name.rsplit("__").next()?;
        match method {
            "filter" | "fold" | "each" | "any" | "all" | "map"
            | "get_or" | "get_or_put" => return None,
            // `update(other)` routes to a single generic runtime stub
            // (`gorget_map_update`) — the iteration is type-independent
            // since src's struct carries key_size/val_size at runtime.
            "update" => return Some("gorget_map_update".into()),
            // Dict.new() needs gorget_dict_new (ordered); all other methods use gorget_map_*
            "new" if name.starts_with("Dict__") => return Some("gorget_dict_new".into()),
            "set" => return Some("gorget_map_put".into()),
            "has" | "has_key" | "contains_key" => return Some("gorget_map_contains".into()),
            // Dict.remove(key) returns Option[V !] — use the opt variant (returns void*).
            "remove" => return Some("gorget_map_remove_opt".into()),
            // Dict.swap_remove(key) returns Option[V !] — same TLS-buffer opt
            // shape as remove, but O(1) via swap-out (order-destroying).
            "swap_remove" => return Some("gorget_map_swap_remove_opt".into()),
            _ => return Some(format!("gorget_map_{method}")),
        }
    }
    // Set__T__method → gorget_set_method
    // GorgetSet__method → gorget_set_method
    // Higher-order methods and non-runtime set operations keep monomorphized names.
    if family == Some("gorget_set") {
        let method = name.rsplit("__").next()?;
        match method {
            "filter" | "fold" | "each" | "any" | "all" | "map" => return None,
            // Set-ops route to generic runtime stubs that walk the
            // sets' own cap/states/key_size fields and construct a
            // fresh result via `gorget_set_new_like`. Type-independent:
            // one stub per op covers every element type.
            "union" | "intersection" | "difference" | "symmetric_difference" => {
                return Some(format!("gorget_set_{method}"));
            }
            // Read-only set predicates route to generic runtime stubs
            // that walk the sets' own cap/states/key_size fields —
            // type-independent, one stub per op covers every T.
            "is_subset" | "is_superset" | "is_disjoint" => {
                return Some(format!("gorget_set_{method}"));
            }
            "insert" => return Some("gorget_set_add".into()),
            "has" => return Some("gorget_set_contains".into()),
            // Set.new() needs gorget_ordered_set_new (ordered); HashSet uses unordered.
            "new" if name.starts_with("Set__") => return Some("gorget_ordered_set_new".into()),
            "new_str" if name.starts_with("Set__") => return Some("gorget_ordered_set_new_str".into()),
            _ => return Some(format!("gorget_set_{method}")),
        }
    }
    // GorgetString__method → gorget_str_method (for string methods).
    // Bare "GorgetString" / "String" / "Str" hits family but lacks the
    // `__method` suffix — strip_prefix returns None → `?` propagates,
    // matching the previous starts_with-guard behaviour.
    if family == Some("gorget_str") {
        let method = name.strip_prefix("GorgetString__")?;
        // Belt-and-suspenders (round-31): `.str()`/`.as_str()` were removed as
        // redundant deep-copy self-view accessors (bare `String v = sb` is a
        // zero-cost CoW borrow). The typecheck primitive-method reject
        // (semantic/typecheck.rs, #1) is the real gate — no `GorgetString__str`
        // / `GorgetString__as_str` name should ever reach LIR. If one does, a
        // typecheck-bypass path silently reintroduced the method; refuse to
        // invent the runtime symbol (`gorget_str_str` doesn't exist → link
        // error or silent miscompile) and fail loudly instead.
        assert!(
            method != "str" && method != "as_str",
            "LIR: GorgetString__{method} reached runtime-symbol mapping — \
             `.str()`/`.as_str()` were removed in round-31 and must be \
             rejected at typecheck; this signals a typecheck-bypass bug",
        );
        let mapped = format!("gorget_str_{method}");
        // Fixup: these GIR method names don't match runtime function names.
        return Some(match mapped.as_str() {
            "gorget_str_substring" => "gorget_str_slice".into(),
            "gorget_str_trim_left" => "gorget_str_lstrip".into(),
            "gorget_str_trim_right" => "gorget_str_rstrip".into(),
            _ => mapped,
        });
    }
    // Option/Result helpers are handled inline by the c_lir backend — don't map them.
    // Heap__T__method → gorget_heap_method
    if family == Some("gorget_heap") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_heap_{method}"));
    }
    // Mutex__T__method → gorget_mutex_method  (new/lock/free)
    // Guard__T__method → gorget_guard_method  (get/set/drop/get_ptr/release)
    if family == Some("gorget_mutex") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_mutex_{method}"));
    }
    if family == Some("gorget_guard") {
        let method = name.rsplit("__").next()?;
        // Guard__T__drop → gorget_guard_release (RAII drop = release the mutex)
        if method == "drop" {
            return Some("gorget_guard_release".into());
        }
        return Some(format!("gorget_guard_{method}"));
    }
    // Shared__T and Weak__T methods are NOT mapped — they have different calling
    // conventions (monomorphized wrappers pass/return typed values, runtime uses void*).
    // Inline wrappers are emitted by the c_lir backend.
    if family == Some("gorget_shared") || family == Some("gorget_weak") {
        return None;
    }
    // Channel__T methods are NOT mapped — they have different calling conventions
    // (monomorphized wrappers pass values, runtime uses void*). Inline wrappers
    // are emitted by the c_lir backend.
    if family == Some("gorget_channel") {
        return None;
    }
    // RWLock__T__method → gorget_rwlock_method  (new/read/write/free)
    if family == Some("gorget_rwlock") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_rwlock_{method}"));
    }
    // ReadGuard__T__method → gorget_read_guard_method  (get/get_ptr/drop)
    if family == Some("gorget_read_guard") {
        let method = name.rsplit("__").next()?;
        if method == "drop" {
            return Some("gorget_read_guard_release".into());
        }
        return Some(format!("gorget_read_guard_{method}"));
    }
    // WriteGuard__T__method → gorget_write_guard_method  (get/set/get_ptr/drop)
    if family == Some("gorget_write_guard") {
        let method = name.rsplit("__").next()?;
        if method == "drop" {
            return Some("gorget_write_guard_release".into());
        }
        return Some(format!("gorget_write_guard_{method}"));
    }
    // uint8_t__method → gorget_uint8_method (byte equip methods)
    // Exclude parse/default — these go through the generic int parse path in the C backend.
    if name.starts_with("uint8_t__") {
        let method = name.strip_prefix("uint8_t__")?;
        if method != "parse" && method != "default" {
            return Some(format!("gorget_uint8_{method}"));
        }
    }
    // Bare stdlib helpers → gorget_ prefixed runtime functions.
    // Delegates to the shared map_stdlib_name() in crate::backend.
    if let Some(mapped) = crate::backend::map_stdlib_name(name) {
        return Some(mapped.to_string());
    }
    // SDL wildcard fallback for any sdl_ function not explicitly listed.
    if name.starts_with("sdl_") {
        return Some(format!("gorget_{name}"));
    }
    None
}
