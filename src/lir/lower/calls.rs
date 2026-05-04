//! Call and extern dispatch helpers for GIR → LIR lowering.

use super::*;
use crate::ir::abi::AbiKind;

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

pub(super) fn lower_binop(dst: ValueId, op: GirBinOp, lhs: ValueId, rhs: ValueId, ty: LirType, overflow_wrap: bool) -> Inst {
    let default_overflow = if overflow_wrap { Overflow::Wrap } else { Overflow::Trap };
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

/// Canonical signature for a known Gorget runtime function.
pub(super) struct RuntimeSig {
    pub params: Vec<LirType>,
    pub ret: LirType,
    pub param_abis: Vec<AbiKind>,
}

/// Return canonical signature + ABI tags for known Gorget runtime functions.
/// This prevents call-site inference from producing wrong parameter types
/// (e.g. GorgetString instead of Str for gorget_str_* functions).
pub(super) fn runtime_extern_sig(name: &str, sr: &StructRegistry) -> Option<RuntimeSig> {
    use AbiKind::*;
    let str_ty = || sr.lookup("GorgetString").map(LirType::Struct).unwrap_or(LirType::Ptr);
    let arr_ty = || sr.lookup("GorgetArray").map(LirType::Struct).unwrap_or(LirType::Ptr);
    let regex_ty = || sr.lookup("Regex").map(LirType::Struct).unwrap_or(LirType::Ptr);
    let match_ty = || sr.lookup("Match").or_else(|| sr.lookup("RegexMatch")).map(LirType::Struct).unwrap_or(LirType::Ptr);
    let s = str_ty;
    let g = str_ty;

    // Shorthand: signature with explicit ABI tags.
    let sig = |params: Vec<LirType>, ret: LirType, abis: Vec<AbiKind>| -> Option<RuntimeSig> {
        debug_assert_eq!(
            params.len(), abis.len(),
            "runtime_extern_sig: param count ({}) != ABI tag count ({})",
            params.len(), abis.len(),
        );
        Some(RuntimeSig { params, ret, param_abis: abis })
    };

    // Shorthands for common ABI patterns: S = GorgetString by value, SS = two Str by value.
    let _s1 = || vec![GorgetString];
    let _s2 = || vec![GorgetString, GorgetString];
    let _s3 = || vec![GorgetString, GorgetString, GorgetString];
    let _ss = |n: usize| vec![GorgetString; n]; // N Str args
    let _si = || vec![GorgetString, Scalar]; // Str + int
    let _ssi = || vec![GorgetString, Scalar, GorgetString]; // Str + int + Str

    match name {
        // ── gorget_str_* — take Str by value ─────────────────────────
        "gorget_str_cat" => sig(vec![s(), s()], g(), _s2()),
        "gorget_str_eq" => sig(vec![s(), s()], LirType::Bool, _s2()),
        "gorget_str_cmp" => sig(vec![s(), s()], LirType::I64, _s2()),
        // gorget_str_from_cstr(const char*) — arg is already const char* in LIR
        "gorget_str_from_cstr" => sig(vec![LirType::Ptr], s(), vec![Opaque]),
        "gorget_str_to_cstr" => sig(vec![s()], LirType::Ptr, _s1()),
        "gorget_str_empty" => sig(vec![], s(), vec![]),
        "gorget_str_index" => sig(vec![s(), LirType::I64], s(), _si()),
        "gorget_str_slice" => sig(vec![s(), LirType::I64, LirType::I64], s(), vec![GorgetString, Scalar, Scalar]),
        "gorget_str_byte_slice" => sig(vec![s(), LirType::I64, LirType::I64], s(), vec![GorgetString, Scalar, Scalar]),
        "gorget_str_char_at" => sig(vec![s(), LirType::I64], s(), _si()),
        "gorget_str_codepoint_at" => sig(vec![s(), LirType::I64], s(), _si()),
        "gorget_utf8_codepoint_len_at" => sig(vec![s(), LirType::I64], LirType::I64, _si()),
        "gorget_str_byte_at" => sig(vec![s(), LirType::I64], LirType::U8, _si()),
        "gorget_str_byte_len" => sig(vec![s()], LirType::I64, _s1()),
        "gorget_str_codepoint_count" => sig(vec![s()], LirType::I64, _s1()),
        "gorget_str_is_empty" => sig(vec![s()], LirType::Bool, _s1()),
        "gorget_str_contains" => sig(vec![s(), s()], LirType::Bool, _s2()),
        "gorget_str_starts_with" => sig(vec![s(), s()], LirType::Bool, _s2()),
        "gorget_str_ends_with" => sig(vec![s(), s()], LirType::Bool, _s2()),
        // gorget_str_find: restored as the unified search primitive (same as index_of for 1-arg)
        "gorget_str_find" => sig(vec![s(), s()], LirType::I64, _s2()),
        "gorget_str_find_from" => sig(vec![s(), s(), LirType::I64], LirType::I64, vec![GorgetString, GorgetString, Scalar]),
        "gorget_str_find_ext" => sig(vec![s(), s(), LirType::I64, LirType::Bool], LirType::I64, vec![GorgetString, GorgetString, Scalar, Scalar]),
        "gorget_str_index_of" => sig(vec![s(), s()], LirType::I64, _s2()),
        "gorget_str_count" => sig(vec![s(), s()], LirType::I64, _s2()),
        "gorget_str_trim" | "gorget_str_lstrip_ws" | "gorget_str_rstrip_ws" => sig(vec![s()], s(), _s1()),
        "gorget_str_strip" | "gorget_str_lstrip" | "gorget_str_rstrip" => sig(vec![s(), s()], s(), _s2()),
        "gorget_str_removeprefix" | "gorget_str_removesuffix" => sig(vec![s(), s()], s(), _s2()),
        "gorget_str_to_upper" | "gorget_str_to_lower" => sig(vec![s()], g(), _s1()),
        "gorget_str_replace" => sig(vec![s(), s(), s()], g(), _s3()),
        "gorget_str_replacen" => sig(vec![s(), s(), s(), LirType::I64], g(), vec![GorgetString, GorgetString, GorgetString, Scalar]),
        "gorget_str_repeat" => sig(vec![s(), LirType::I64], g(), _si()),
        "gorget_str_pad_left" | "gorget_str_pad_right" => sig(vec![s(), LirType::I64, s()], g(), _ssi()),
        "gorget_str_is_alpha" | "gorget_str_is_digit" | "gorget_str_is_alphanumeric"
        | "gorget_str_is_whitespace" | "gorget_str_is_upper" | "gorget_str_is_lower"
        | "gorget_str_is_hex_digit" | "gorget_str_is_ascii" | "gorget_str_has_null" => {
            sig(vec![s()], LirType::Bool, _s1())
        }
        "gorget_str_split" => sig(vec![s(), s()], arr_ty(), _s2()),
        "gorget_str_splitn" => sig(vec![s(), s(), LirType::I64], arr_ty(), vec![GorgetString, GorgetString, Scalar]),
        "gorget_str_lines" => sig(vec![s()], arr_ty(), _s1()),
        "gorget_str_join" => sig(vec![s(), arr_ty()], g(), vec![GorgetString, ByValue]),
        "gorget_str_bytes" | "gorget_str_codepoints" | "gorget_str_chars" => {
            sig(vec![s()], arr_ty(), _s1())
        }
        // ── StringBuilder thunks: shim wrappers around gorget_string_* ──
        // The C runtime defines these as `(GorgetString* s, Str chunk) → void`
        // (see emit_types.rs line 2316). Without a sig entry the call-site
        // type inference reads the 2nd arg as Ptr (from the strlit's ptr
        // operand) → ABI tag = default → LLVM x86_64 emits bare `ptr` instead
        // of `ptr byval(%GorgetString) align 8`, so the runtime reads the
        // 32-byte struct from the wrong location and prints garbage / crashes.
        "gorget_str_push" | "gorget_str_push_char" | "gorget_str_push_line" => {
            sig(vec![LirType::Ptr, s()], LirType::Void, vec![Ptr, GorgetString])
        }
        "gorget_str_clear" => sig(vec![LirType::Ptr], LirType::Void, vec![Ptr]),
        "gorget_str_capacity" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        // ── gorget_string_* — take GorgetString* by pointer ──────────
        // gorget_string_new(const char*) — arg is already const char* in LIR
        "gorget_string_new" => sig(vec![LirType::Ptr], g(), vec![Opaque]),
        "gorget_string_from_str" => sig(vec![s()], g(), _s1()),
        "gorget_string_clone" => sig(vec![LirType::Ptr], g(), vec![Ptr]),
        "gorget_string_clone_to_owned" => sig(vec![LirType::Ptr], g(), vec![Ptr]),
        "gorget_string_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Ptr]),
        "gorget_string_eq" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, Ptr]),
        "gorget_string_cstr" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Ptr]),
        "gorget_string_concat" => sig(vec![LirType::Ptr, LirType::Ptr], g(), vec![Ptr, Ptr]),
        "gorget_string_append" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, Ptr]),
        // gorget_string_push_line(GorgetString* dst, const char* line)
        "gorget_string_push_line" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, CStr]),
        // gorget_str_str(GorgetString*) — single ptr arg, returns Str view of the string.
        "gorget_str_str" => sig(vec![LirType::Ptr], s(), vec![Ptr]),
        // gorget_str_from_literal(const char* raw, size_t len) — arg is already const char*
        "gorget_str_from_literal" => sig(vec![LirType::Ptr, LirType::I64], s(), vec![Opaque, Scalar]),
        "gorget_str_from_int" | "gorget_str_from_float" | "gorget_str_from_bool" => {
            sig(vec![LirType::I64], s(), vec![Scalar])
        }

        // Collection methods
        "gorget_array_new" => sig(vec![LirType::I64], arr_ty(), vec![Scalar]),
        "gorget_array_with_capacity" => sig(vec![LirType::I64, LirType::I64], arr_ty(), vec![Scalar, Scalar]),
        // ── Collection functions — explicit ABI tags ──
        "gorget_array_push" => {
            // void gorget_array_push(GorgetArray* arr, const void* elem)
            sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem])
        }
        "gorget_array_set" | "gorget_array_insert" => {
            // void gorget_array_set(GorgetArray* arr, size_t idx, const void* val)
            sig(vec![LirType::Ptr, LirType::I64, LirType::Ptr], LirType::Void, vec![Ptr, Scalar, VoidElem])
        }
        "gorget_array_get" | "gorget_array_pop" | "gorget_array_safe_get" => {
            sig(vec![LirType::Ptr, LirType::I64], LirType::Ptr, vec![Ptr, Scalar])
        }
        "gorget_array_first" | "gorget_array_last" => {
            sig(vec![LirType::Ptr], LirType::Ptr, vec![Ptr])
        }
        "gorget_array_safe_pop" => {
            sig(vec![LirType::Ptr], LirType::Ptr, vec![Ptr])
        }
        "gorget_array_remove" => sig(vec![LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, Scalar]),
        "gorget_array_remove_opt" => sig(vec![LirType::Ptr, LirType::I64], LirType::Ptr, vec![Ptr, Scalar]),
        // void gorget_array_fill(GorgetArray*, size_t n, const void* val_src)
        // The third arg is a pointer to one element (the runtime memcpy's
        // `arr->elem_size` bytes from there). Without this entry, the call
        // site infers `i64` from the user's `int` literal in `v.fill(n, 7)`
        // and the runtime tries to memcpy from address `7` → SEGV.
        "gorget_array_fill" => sig(vec![LirType::Ptr, LirType::I64, LirType::Ptr], LirType::Void, vec![Ptr, Scalar, VoidElem]),
        "gorget_array_swap" => sig(vec![LirType::Ptr, LirType::I64, LirType::I64], LirType::Void, vec![Ptr, Scalar, Scalar]),
        "gorget_array_swap_remove" => sig(vec![LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, Scalar]),
        "gorget_array_len" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        "gorget_array_capacity" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        // gorget_array_contains(arr*, elem*, elem_size)
        "gorget_array_contains" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Bool, vec![Ptr, VoidElem, Scalar]),
        "gorget_array_is_empty" => sig(vec![LirType::Ptr], LirType::Bool, vec![Ptr]),
        "gorget_array_index_of" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::I64, vec![Ptr, VoidElem]),
        "gorget_array_binary_search" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::I64, vec![Ptr, VoidElem]),
        "gorget_array_clear" | "gorget_array_free" | "gorget_array_reverse"
        | "gorget_array_dedup" => {
            sig(vec![LirType::Ptr], LirType::Void, vec![Ptr])
        }
        "gorget_array_extend" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, Ptr]),
        "gorget_array_reserve" => sig(vec![LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, Scalar]),
        "gorget_array_clone" => sig(vec![LirType::Ptr], arr_ty(), vec![Ptr]),
        "gorget_array_slice" => sig(vec![LirType::Ptr, LirType::I64, LirType::I64], arr_ty(), vec![Ptr, Scalar, Scalar]),
        // Map methods (unordered)
        "gorget_map_new" => sig(vec![LirType::I64, LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar, Scalar]),
        "gorget_map_new_str" => sig(vec![LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar]),
        // Dict methods (ordered — only new differs; put/get/etc. use gorget_map_*)
        "gorget_dict_new" => sig(vec![LirType::I64, LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar, Scalar]),
        "gorget_dict_new_str" => sig(vec![LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar]),
        // gorget_map_put(map*, key*, val*)
        "gorget_map_put" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem, VoidElem]),
        // gorget_map_put_cloned(map*, key*, val*) — like put but deep-
        // clones key/val via the map's key_clone/val_clone hooks. Used
        // by HOF BIR expansions (`filter`, `map`, `union`, …) when
        // lifting elements from another map/set into a fresh result.
        "gorget_map_put_cloned" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem, VoidElem]),
        // gorget_map_get(map*, key*) → void*
        "gorget_map_get" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Ptr, vec![Ptr, VoidElem]),
        "gorget_map_remove" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, VoidElem]),
        "gorget_map_remove_opt" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Ptr, vec![Ptr, VoidElem]),
        "gorget_map_contains" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, VoidElem]),
        "gorget_map_len" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        "gorget_map_is_empty" => sig(vec![LirType::Ptr], LirType::Bool, vec![Ptr]),
        "gorget_map_clear" | "gorget_map_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Ptr]),
        "gorget_map_reserve" | "gorget_set_reserve" => sig(vec![LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, Scalar]),
        "gorget_map_clone" => sig(vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Ptr]),
        // gorget_map_new_like(const GorgetMap*) — fresh empty map that
        // mirrors the source's hash/eq/drop/clone/materialize config.
        // Used by the BIR expansion of `Dict.filter` (and, later,
        // `Dict.map`) so the result inherits the right per-type wiring.
        "gorget_map_new_like" => sig(vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Ptr]),
        "gorget_map_keys" | "gorget_map_values" | "gorget_map_items" => sig(vec![LirType::Ptr], arr_ty(), vec![Ptr]),
        // Set methods
        "gorget_set_new" | "gorget_ordered_set_new" => sig(vec![LirType::I64], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))), vec![Scalar]),
        "gorget_set_new_str" | "gorget_ordered_set_new_str" => sig(vec![], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))), vec![]),
        "gorget_set_add" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem]),
        "gorget_set_contains" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, VoidElem]),
        "gorget_set_remove" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, VoidElem]),
        "gorget_set_len" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        "gorget_set_is_empty" => sig(vec![LirType::Ptr], LirType::Bool, vec![Ptr]),
        "gorget_set_clear" | "gorget_set_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Ptr]),
        "gorget_set_clone" => sig(vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))), vec![Ptr]),
        // gorget_set_new_like(const GorgetSet*) — fresh empty set that
        // mirrors the source's hash/eq/drop/clone/materialize config.
        // Used by the BIR expansion of `Set.filter` / `Set.map` so the
        // result inherits the correct per-element-type wiring.
        "gorget_set_new_like" => sig(vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetSet").unwrap_or(StructId(0))), vec![Ptr]),
        "gorget_set_to_array" => sig(vec![LirType::Ptr], arr_ty(), vec![Ptr]),
        // Heap methods
        "gorget_heap_new" => sig(vec![LirType::I64], LirType::Ptr, vec![Scalar]),
        "gorget_heap_push" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem]),
        "gorget_heap_pop" | "gorget_heap_peek" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Ptr]),
        "gorget_heap_len" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        "gorget_heap_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Ptr]),

        // Mutex / Guard methods
        // gorget_mutex_new(size, void* initial_val)
        "gorget_mutex_new" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, VoidElem]),
        "gorget_mutex_lock" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_mutex_lock_to" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Opaque, Opaque]),
        "gorget_mutex_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_guard_release" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_guard_get" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        // gorget_guard_set(guard*, void* val, size_t size)
        "gorget_guard_set" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, VoidElem, Scalar]),
        "gorget_guard_get_ptr" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),

        // Shared methods
        // gorget_shared_new(size, void* initial_val)
        "gorget_shared_new" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, VoidElem]),
        "gorget_shared_clone" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_shared_drop" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_shared_get" | "gorget_shared_get_ptr" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_shared_strong_count" => sig(vec![LirType::Ptr], LirType::I64, vec![Opaque]),
        "gorget_shared_downgrade" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),

        // Weak methods
        "gorget_weak_clone" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_weak_drop" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_weak_upgrade" => sig(vec![LirType::Ptr], LirType::I64, vec![Opaque]),

        // Channel methods
        "gorget_channel_new" => sig(vec![LirType::I64, LirType::I64], LirType::Ptr, vec![Scalar, Scalar]),
        // gorget_channel_send(ch*, void* elem)
        "gorget_channel_send" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem]),
        "gorget_channel_recv" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Opaque, Opaque]),
        "gorget_channel_close" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_channel_len" | "gorget_channel_capacity" => sig(vec![LirType::Ptr], LirType::I64, vec![Opaque]),
        "gorget_channel_is_closed" => sig(vec![LirType::Ptr], LirType::Bool, vec![Opaque]),
        "gorget_channel_retain" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_channel_release" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_channel_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),

        // RWLock / ReadGuard / WriteGuard methods
        // gorget_rwlock_new(size, void* initial_val)
        "gorget_rwlock_new" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, VoidElem]),
        "gorget_rwlock_read" | "gorget_rwlock_write" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_rwlock_read_to" | "gorget_rwlock_write_to" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Opaque, Opaque]),
        "gorget_rwlock_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_read_guard_get" | "gorget_read_guard_get_ptr" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "gorget_read_guard_release" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "gorget_write_guard_get" | "gorget_write_guard_get_ptr" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        // gorget_write_guard_set(guard*, void* val, size_t size)
        "gorget_write_guard_set" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, VoidElem, Scalar]),
        "gorget_write_guard_release" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),

        // Allocator push/pop stubs
        "__gorget_push_allocator" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "__gorget_pop_allocator" => sig(vec![], LirType::Void, vec![]),

        // chr/ord
        "gorget_char_chr" => sig(vec![LirType::I64], s(), vec![Scalar]),
        "gorget_str_ord" => sig(vec![s()], LirType::I64, vec![GorgetString]),
        // Conversion helpers
        "gorget_int_to_str" => sig(vec![LirType::I64], s(), vec![Scalar]),
        "gorget_float_to_str" => sig(vec![LirType::F64], s(), vec![Scalar]),
        "gorget_bool_to_str" => sig(vec![LirType::Bool], s(), vec![Scalar]),
        "gorget_string_debug" => sig(vec![s()], s(), vec![GorgetString]),
        "gorget_codepoint_to_utf8" => sig(vec![LirType::I64], s(), vec![Scalar]),
        "gorget_int_to_float" => sig(vec![LirType::I64], LirType::F64, vec![Scalar]),
        // I/O
        // I/O — these take const char* but may receive Str structs from some code paths
        "gorget_read_file" => sig(vec![LirType::Ptr], g(), vec![Auto]),
        "gorget_write_file" | "gorget_append_file" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Auto, Auto]),
        "gorget_file_exists" | "gorget_is_dir" => sig(vec![LirType::Ptr], LirType::Bool, vec![Auto]),
        // Math (integer)
        "gorget_abs" => sig(vec![LirType::I64], LirType::I64, vec![Scalar]),
        "gorget_min" | "gorget_max" => sig(vec![LirType::I64, LirType::I64], LirType::I64, vec![Scalar, Scalar]),
        // Math (float)
        "gorget_fabs" => sig(vec![LirType::F64], LirType::F64, vec![Scalar]),
        "gorget_fmin" | "gorget_fmax" => sig(vec![LirType::F64, LirType::F64], LirType::F64, vec![Scalar, Scalar]),
        "gorget_sqrt" | "gorget_floor" | "gorget_ceil" | "gorget_round"
        | "gorget_log" | "gorget_log2" | "gorget_log10"
        | "gorget_sin" | "gorget_cos" | "gorget_tan"
        | "gorget_asin" | "gorget_acos" | "gorget_atan" => {
            sig(vec![LirType::F64], LirType::F64, vec![Scalar])
        }
        "gorget_pow" | "gorget_atan2" => sig(vec![LirType::F64, LirType::F64], LirType::F64, vec![Scalar, Scalar]),
        // Random
        "gorget_rand" => sig(vec![], LirType::I64, vec![]),
        "gorget_rand_range" => sig(vec![LirType::I64, LirType::I64], LirType::I64, vec![Scalar, Scalar]),
        "gorget_seed" => sig(vec![LirType::I64], LirType::Void, vec![Scalar]),
        // Time
        "gorget_time" | "gorget_time_ms" => sig(vec![], LirType::I64, vec![]),
        "gorget_sleep_ms" | "gorget_reactor_sleep_ms" => sig(vec![LirType::I64], LirType::Void, vec![Scalar]),
        "gorget_format_time" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, CStr]),
        "gorget_parse_time" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::I64, vec![CStr, CStr]),

        // Barrier
        "gorget_barrier_new" => sig(vec![LirType::I64], LirType::Ptr, vec![Scalar]),
        "gorget_barrier_wait" | "gorget_barrier_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        // CondVar
        "gorget_condvar_new" => sig(vec![], LirType::Ptr, vec![]),
        "gorget_condvar_notify_one" | "gorget_condvar_notify_all" | "gorget_condvar_free" => {
            sig(vec![LirType::Ptr], LirType::Void, vec![Opaque])
        }
        "gorget_condvar_wait_guard" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Opaque, Opaque]),
        // AtomicInt
        "gorget_atomic_int_new" => sig(vec![LirType::I64], LirType::Ptr, vec![Scalar]),
        "gorget_atomic_int_load" => sig(vec![LirType::Ptr], LirType::I64, vec![Opaque]),
        "gorget_atomic_int_store" => sig(vec![LirType::Ptr, LirType::I64], LirType::Void, vec![Opaque, Scalar]),
        "gorget_atomic_int_add" | "gorget_atomic_int_sub" => sig(vec![LirType::Ptr, LirType::I64], LirType::I64, vec![Opaque, Scalar]),
        "gorget_atomic_int_compare_exchange" => sig(vec![LirType::Ptr, LirType::I64, LirType::I64], LirType::Bool, vec![Opaque, Scalar, Scalar]),
        "gorget_atomic_int_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        // AtomicBool
        "gorget_atomic_bool_new" => sig(vec![LirType::Bool], LirType::Ptr, vec![Scalar]),
        "gorget_atomic_bool_load" => sig(vec![LirType::Ptr], LirType::Bool, vec![Opaque]),
        "gorget_atomic_bool_store" => sig(vec![LirType::Ptr, LirType::Bool], LirType::Void, vec![Opaque, Scalar]),
        "gorget_atomic_bool_swap" => sig(vec![LirType::Ptr, LirType::Bool], LirType::Bool, vec![Opaque, Scalar]),
        "gorget_atomic_bool_compare_exchange" => sig(vec![LirType::Ptr, LirType::Bool, LirType::Bool], LirType::Bool, vec![Opaque, Scalar, Scalar]),
        "gorget_atomic_bool_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        // Process
        "gorget_process_spawn" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Ptr, vec![CStr, Ptr]),
        "gorget_process_wait" | "gorget_process_pid" => sig(vec![LirType::Ptr], LirType::I64, vec![Opaque]),
        "gorget_process_kill" | "gorget_process_close_stdin" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        // gorget_process_write_stdin(proc*, const char*) — Str param needs CStr extraction
        "gorget_process_write_stdin" => sig(vec![LirType::Ptr, s()], LirType::Void, vec![Opaque, CStr]),
        "gorget_process_read_stdout" | "gorget_process_read_stderr" => sig(vec![LirType::Ptr], g(), vec![Opaque]),

        // Panic / abort functions (void return)
        // gorget_panic(const char*) — may receive Str struct from assert paths
        "gorget_panic" => sig(vec![LirType::Ptr], LirType::Void, vec![CStr]),
        "gorget_assert_fail" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void, vec![CStr, CStr, Scalar]),
        // gorget_assert_fail_values(const char* op, Str left, Str right)
        "gorget_assert_fail_values" => sig(vec![LirType::Ptr, s(), s()], LirType::Void, vec![CStr, GorgetString, GorgetString]),
        "gorget_overflow_add" | "gorget_overflow_sub" | "gorget_overflow_mul" => {
            sig(vec![], LirType::Void, vec![])
        }

        // Parse int/float — take null-terminated C strings
        "gorget_parse_int" => sig(vec![LirType::Ptr], LirType::I64, vec![CStr]),
        "gorget_parse_float" => sig(vec![LirType::Ptr], LirType::F64, vec![CStr]),

        // Bytes (Vector[uint8]) operations
        // Regex — several functions take const char* subject/pattern strings
        // gorget_regex_compile returns GorgetRegex by value (16-byte struct: pcre2_code* + const char*).
        // Declaring the return as `Ptr` reads only the first 8 bytes of the return register pair → SEGV
        // when downstream code tries to extract the second pointer field. Use the Regex struct type so
        // both backends generate the correct two-register return ABI (AArch64) / sret (large ABI).
        "gorget_regex_compile" => sig(vec![LirType::Ptr, LirType::Ptr], regex_ty(), vec![CStr, CStr]),
        // gorget_regex_find / find_at / fullmatch return GorgetRegexMatch (56-byte struct) by value.
        // Declaring return as `Ptr` reads only x0 (likely garbage) — must use the actual struct
        // type so the LLVM caller emits sret (>16 bytes → memory return ABI on AArch64).
        "gorget_regex_find" | "gorget_regex_find_at" => {
            sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], match_ty(), vec![Ptr, CStr, Scalar])
        }
        "gorget_regex_is_match" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, CStr]),
        "gorget_regex_find_all" => sig(vec![LirType::Ptr, LirType::Ptr], arr_ty(), vec![Ptr, CStr]),
        "gorget_regex_replace" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::Ptr], s(), vec![Ptr, CStr, CStr]),
        "gorget_regex_split" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], arr_ty(), vec![Ptr, CStr, Scalar]),
        "gorget_regex_fullmatch" => sig(vec![LirType::Ptr, LirType::Ptr], match_ty(), vec![Ptr, CStr]),

        // gorget_bytes_from_str/hex(const char*) — may receive Str reference
        "gorget_bytes_from_str" | "gorget_bytes_from_hex" => sig(vec![LirType::Ptr], arr_ty(), vec![CStr]),
        "gorget_bytes_to_str" | "gorget_bytes_to_hex" => sig(vec![LirType::Ptr], s(), vec![Ptr]),
        "gorget_bytes_utf8_valid" => sig(vec![LirType::Ptr], LirType::Bool, vec![Ptr]),
        "gorget_bytes_concat" => sig(vec![LirType::Ptr, LirType::Ptr], arr_ty(), vec![Ptr, Ptr]),
        "gorget_bytes_slice" => sig(vec![LirType::Ptr, LirType::I64, LirType::I64], arr_ty(), vec![Ptr, Scalar, Scalar]),
        // gorget_bytes_write_* / read_* — self by ptr + offset + value
        "gorget_bytes_write_u32_be" | "gorget_bytes_write_u16_be"
        | "gorget_bytes_write_u32_le" | "gorget_bytes_write_u16_le"
        | "gorget_bytes_write_i32_le" | "gorget_bytes_write_i64_le" => {
            sig(vec![LirType::Ptr, LirType::I64, LirType::I64], LirType::Void, vec![Ptr, Scalar, Scalar])
        }
        "gorget_bytes_write_f32_le" | "gorget_bytes_write_f64_le" => {
            sig(vec![LirType::Ptr, LirType::I64, LirType::F64], LirType::Void, vec![Ptr, Scalar, Scalar])
        }
        "gorget_bytes_read_u32_be" | "gorget_bytes_read_u16_be"
        | "gorget_bytes_read_u32_le" | "gorget_bytes_read_u16_le"
        | "gorget_bytes_read_i32_le" | "gorget_bytes_read_i64_le" => {
            sig(vec![LirType::Ptr, LirType::I64], LirType::I64, vec![Ptr, Scalar])
        }
        "gorget_bytes_read_f32_le" | "gorget_bytes_read_f64_le" => {
            sig(vec![LirType::Ptr, LirType::I64], LirType::F64, vec![Ptr, Scalar])
        }

        // Memory allocation / deallocation (used by arena, pool, closure heap-alloc)
        "malloc" => sig(vec![LirType::I64], LirType::Ptr, vec![Scalar]),
        "free" => sig(vec![LirType::Ptr], LirType::Void, vec![Opaque]),
        "memcmp" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::I32, vec![Opaque, Opaque, Scalar]),
        "memset" => sig(vec![LirType::Ptr, LirType::I32, LirType::I64], LirType::Ptr, vec![Opaque, Scalar, Scalar]),
        "__gorget_alloc" => sig(vec![LirType::Ptr], LirType::Ptr, vec![Opaque]),
        "__gorget_alloc_array" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, Opaque]),
        "__gorget_dealloc" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Opaque, Opaque]),

        _ => None,
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
) -> Option<String> {
    // Reads `metadata.clone_fn` from the type's TypeDef — every collection
    // type and runtime singleton carries this set at registration via
    // BuiltinTypeProtocol (see src/ir/lowering/types.rs and mod.rs).
    if let Some(td) = gir_types.get_type_def(elem_type_name) {
        if let Some(ref f) = td.metadata.clone_fn {
            return Some(f.clone());
        }
    }

    // Callable elements: deep-clone on read so the source slot stays
    // intact. Without this, the IndexLoad path treats the Callable's
    // `Trivial("gorget_closure_free")` drop strategy as a Move, memsets
    // the slot to zero after the read, and the next iteration of (e.g.)
    // a middleware loop reads `fn_ptr=NULL` → SEGV calling the closure.
    // Callable types still don't have TypeDef registration today.
    if elem_type_name == "GorgetClosure"
        || elem_type_name.starts_with("Callable__")
        || elem_type_name.starts_with("MutCallable__")
        || elem_type_name.starts_with("ConsumeCallable__")
    {
        return Some("gorget_closure_clone_to_owned".to_string());
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

pub(super) fn is_type_name(s: &str) -> bool {
    matches!(s, "int64_t" | "int32_t" | "int16_t" | "int8_t"
        | "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t"
        | "double" | "float" | "bool" | "GorgetString"
        | "GorgetArray" | "GorgetMap" | "GorgetSet" | "void"
        | "T" | "U" | "V")
}

/// Returns true if the GIR function name refers to a collection or concurrency
/// method whose first argument (self) should be passed by pointer (GlobalAddr)
/// rather than by value (GlobalAddr+Load). These are mutating methods on
/// Vector, Dict, Set, HashMap, HashSet, Heap, Mutex, RWLock, etc.
pub(super) fn is_self_by_ptr_method(name: &str) -> bool {
    // Collections and guards store their data inline (as struct values), so passing
    // by pointer (GlobalAddr without Load) gives a pointer to the struct — correct
    // for mutating methods.
    //
    // Mutex and RWLock are already POINTER types (GorgetMutex*, GorgetRWLock*),
    // so the global holds a pointer value. Passing by value (GlobalAddr+Load) gives
    // the pointer itself, which is what the runtime functions expect. Do NOT include
    // Mutex__ or RWLock__ here — they should be passed by value.
    //
    // Guard/ReadGuard/WriteGuard ARE structs (gorget_guard_t etc.), so they need
    // by-pointer passing for their mutating methods (get, set, drop/release).
    name.starts_with("Vector__")
        || name.starts_with("GorgetArray__")
        || name.starts_with("Dict__")
        || name.starts_with("HashMap__")
        || name.starts_with("GorgetMap__")
        || name.starts_with("Set__")
        || name.starts_with("HashSet__")
        || name.starts_with("GorgetSet__")
        || name.starts_with("Heap__")
        || name.starts_with("Guard__")
        || name.starts_with("ReadGuard__")
        || name.starts_with("WriteGuard__")
        || name.starts_with("GorgetString__")
        || name.starts_with("Deque__")
}

pub(super) fn map_monomorphized_to_runtime_with_table(
    name: &str,
    table: &rustc_hash::FxHashMap<String, String>,
) -> Option<String> {
    // Check the protocol-populated table first (covers all builtins).
    if let Some(callee) = table.get(name) {
        return Some(callee.clone());
    }
    // Fall through to legacy name-based mapping for types not in the table.
    map_monomorphized_to_runtime(name)
}

pub(super) fn map_monomorphized_to_runtime(name: &str) -> Option<String> {
    // Vector__T__method → gorget_array_method
    // GorgetArray__method → gorget_array_method  (non-generic array calls)
    // Higher-order methods (filter, map, fold, any, all, each, reduce, flat_map, find, find_index)
    // are NOT runtime functions — they are generated inline by the c_lir backend.
    // Keep them as their original monomorphized names so the backend can detect and generate them.
    if name.starts_with("Vector__") || name.starts_with("GorgetArray__") {
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
            "sort" | "sorted" | "unique" => {
                let elem = name
                    .strip_prefix("Vector__")
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
    if name.starts_with("Dict__") || name.starts_with("HashMap__") || name.starts_with("GorgetMap__") {
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
            _ => return Some(format!("gorget_map_{method}")),
        }
    }
    // Set__T__method → gorget_set_method
    // GorgetSet__method → gorget_set_method
    // Higher-order methods and non-runtime set operations keep monomorphized names.
    if name.starts_with("Set__") || name.starts_with("HashSet__") || name.starts_with("GorgetSet__") {
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
    // GorgetString__method → gorget_str_method (for string methods)
    if name.starts_with("GorgetString__") {
        let method = name.strip_prefix("GorgetString__")?;
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
    if name.starts_with("Heap__") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_heap_{method}"));
    }
    // Mutex__T__method → gorget_mutex_method  (new/lock/free)
    // Guard__T__method → gorget_guard_method  (get/set/drop/get_ptr/release)
    if name.starts_with("Mutex__") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_mutex_{method}"));
    }
    if name.starts_with("Guard__") {
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
    if name.starts_with("Shared__") || name.starts_with("Weak__") {
        return None;
    }
    // Channel__T methods are NOT mapped — they have different calling conventions
    // (monomorphized wrappers pass values, runtime uses void*). Inline wrappers
    // are emitted by the c_lir backend.
    if name.starts_with("Channel__") {
        return None;
    }
    // RWLock__T__method → gorget_rwlock_method  (new/read/write/free)
    if name.starts_with("RWLock__") {
        let method = name.rsplit("__").next()?;
        return Some(format!("gorget_rwlock_{method}"));
    }
    // ReadGuard__T__method → gorget_read_guard_method  (get/get_ptr/drop)
    if name.starts_with("ReadGuard__") {
        let method = name.rsplit("__").next()?;
        if method == "drop" {
            return Some("gorget_read_guard_release".into());
        }
        return Some(format!("gorget_read_guard_{method}"));
    }
    // WriteGuard__T__method → gorget_write_guard_method  (get/set/get_ptr/drop)
    if name.starts_with("WriteGuard__") {
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
