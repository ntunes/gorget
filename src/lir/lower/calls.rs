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
            result.push(bytes[i] as char);
            i += 1;
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
    let s = str_ty;
    let g = str_ty;

    // Shorthand: plain signature with all-Auto ABI tags (migration shim for non-collection fns).
    let auto = |params: Vec<LirType>, ret: LirType| -> Option<RuntimeSig> {
        let n = params.len();
        Some(RuntimeSig { params, ret, param_abis: vec![Auto; n] })
    };
    // Shorthand: signature with explicit ABI tags.
    let sig = |params: Vec<LirType>, ret: LirType, abis: Vec<AbiKind>| -> Option<RuntimeSig> {
        Some(RuntimeSig { params, ret, param_abis: abis })
    };

    match name {
        // String concatenation and conversion
        "gorget_str_cat" => auto(vec![s(), s()], g()),
        "gorget_str_eq" => auto(vec![s(), s()], LirType::Bool),
        "gorget_str_cmp" => auto(vec![s(), s()], LirType::I64),
        "gorget_str_from_cstr" => auto(vec![LirType::Ptr], s()),
        "gorget_str_to_cstr" => auto(vec![s()], LirType::Ptr),
        "gorget_str_empty" => auto(vec![], s()),
        "gorget_str_index" => auto(vec![s(), LirType::I64], s()),
        "gorget_str_slice" => auto(vec![s(), LirType::I64, LirType::I64], s()),
        "gorget_str_byte_slice" => auto(vec![s(), LirType::I64, LirType::I64], s()),
        "gorget_str_char_at" => auto(vec![s(), LirType::I64], s()),
        "gorget_str_codepoint_at" => auto(vec![s(), LirType::I64], s()),
        "gorget_utf8_codepoint_len_at" => auto(vec![s(), LirType::I64], LirType::I64),
        "gorget_str_byte_at" => auto(vec![s(), LirType::I64], LirType::U8),
        "gorget_str_byte_len" => auto(vec![s()], LirType::I64),
        "gorget_str_codepoint_count" => auto(vec![s()], LirType::I64),
        "gorget_str_is_empty" => auto(vec![s()], LirType::Bool),
        "gorget_str_contains" => auto(vec![s(), s()], LirType::Bool),
        "gorget_str_starts_with" => auto(vec![s(), s()], LirType::Bool),
        "gorget_str_ends_with" => auto(vec![s(), s()], LirType::Bool),
        "gorget_str_find" => auto(vec![s(), s()], LirType::I64),
        "gorget_str_index_of" => auto(vec![s(), s()], LirType::I64),
        "gorget_str_count" => auto(vec![s(), s()], LirType::I64),
        "gorget_str_trim" | "gorget_str_lstrip_ws" | "gorget_str_rstrip_ws" => auto(vec![s()], s()),
        "gorget_str_strip" | "gorget_str_lstrip" | "gorget_str_rstrip" => auto(vec![s(), s()], s()),
        "gorget_str_removeprefix" | "gorget_str_removesuffix" => auto(vec![s(), s()], s()),
        "gorget_str_to_upper" | "gorget_str_to_lower" => auto(vec![s()], g()),
        "gorget_str_replace" => auto(vec![s(), s(), s()], g()),
        "gorget_str_repeat" => auto(vec![s(), LirType::I64], g()),
        "gorget_str_pad_left" | "gorget_str_pad_right" => auto(vec![s(), LirType::I64, s()], g()),
        "gorget_str_is_alpha" | "gorget_str_is_digit" | "gorget_str_is_alphanumeric"
        | "gorget_str_is_whitespace" | "gorget_str_is_upper" | "gorget_str_is_lower"
        | "gorget_str_is_hex_digit" | "gorget_str_is_ascii" | "gorget_str_has_null" => {
            auto(vec![s()], LirType::Bool)
        }
        "gorget_str_split" => auto(vec![s(), s()], arr_ty()),
        "gorget_str_join" => auto(vec![s(), arr_ty()], g()),
        "gorget_str_bytes" | "gorget_str_codepoints" | "gorget_str_chars" => {
            auto(vec![s()], arr_ty())
        }
        // GorgetString methods
        "gorget_string_new" => auto(vec![LirType::Ptr], g()),
        "gorget_string_from_str" => auto(vec![s()], g()),
        "gorget_string_clone" => auto(vec![LirType::Ptr], g()),
        "gorget_string_clone_to_owned" => auto(vec![LirType::Ptr], g()),
        "gorget_string_free" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_string_eq" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Bool),
        "gorget_string_cstr" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_string_concat" => auto(vec![LirType::Ptr, LirType::Ptr], g()),
        "gorget_string_append" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Void),
        "gorget_str_str" => auto(vec![s(), s()], s()),
        // (gorget_str_slice handled above, from Str__substring → gorget_str_slice mapping)
        "gorget_str_from_literal" => auto(vec![LirType::Ptr, LirType::I64], s()),
        "gorget_str_from_int" | "gorget_str_from_float" | "gorget_str_from_bool" => {
            auto(vec![LirType::I64], s())
        }

        // Collection methods
        "gorget_array_new" => auto(vec![LirType::I64], arr_ty()),
        "gorget_array_with_capacity" => auto(vec![LirType::I64, LirType::I64], arr_ty()),
        // ── Collection functions — explicit ABI tags ──
        "gorget_array_push" => {
            // void gorget_array_push(GorgetArray* arr, const void* elem)
            sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem])
        }
        "gorget_array_set" | "gorget_array_insert" => {
            // void gorget_array_set(GorgetArray* arr, size_t idx, const void* val)
            sig(vec![LirType::Ptr, LirType::I64, LirType::Ptr], LirType::Void, vec![Ptr, Scalar, VoidElem])
        }
        "gorget_array_get" | "gorget_array_pop" | "gorget_array_first" | "gorget_array_last"
        | "gorget_array_safe_get" => {
            sig(vec![LirType::Ptr, LirType::I64], LirType::Ptr, vec![Ptr, Scalar])
        }
        "gorget_array_safe_pop" => {
            sig(vec![LirType::Ptr], LirType::Ptr, vec![Ptr])
        }
        "gorget_array_remove" => sig(vec![LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, Scalar]),
        "gorget_array_remove_opt" => sig(vec![LirType::Ptr, LirType::I64], LirType::Ptr, vec![Ptr, Scalar]),
        "gorget_array_len" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        // gorget_array_contains(arr*, elem*, elem_size)
        "gorget_array_contains" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Bool, vec![Ptr, VoidElem, Scalar]),
        "gorget_array_is_empty" => sig(vec![LirType::Ptr], LirType::Bool, vec![Ptr]),
        "gorget_array_index_of" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::I64, vec![Ptr, VoidElem]),
        "gorget_array_binary_search" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::I64, vec![Ptr, VoidElem]),
        "gorget_array_clear" | "gorget_array_free" | "gorget_array_reverse"
        | "gorget_array_dedup" | "gorget_array_extend" | "gorget_array_reserve" => {
            sig(vec![LirType::Ptr], LirType::Void, vec![Ptr])
        }
        "gorget_array_clone" | "gorget_array_slice" => sig(vec![LirType::Ptr], arr_ty(), vec![Ptr]),
        // Map methods (unordered)
        "gorget_map_new" => sig(vec![LirType::I64, LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar, Scalar]),
        "gorget_map_new_str" => sig(vec![LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar]),
        // Dict methods (ordered — only new differs; put/get/etc. use gorget_map_*)
        "gorget_dict_new" => sig(vec![LirType::I64, LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar, Scalar]),
        "gorget_dict_new_str" => sig(vec![LirType::I64], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Scalar]),
        // gorget_map_put(map*, key*, val*)
        "gorget_map_put" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem, VoidElem]),
        // gorget_map_get(map*, key*) → void*
        "gorget_map_get" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Ptr, vec![Ptr, VoidElem]),
        "gorget_map_remove" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, VoidElem]),
        "gorget_map_contains" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Bool, vec![Ptr, VoidElem]),
        "gorget_map_len" => sig(vec![LirType::Ptr], LirType::I64, vec![Ptr]),
        "gorget_map_is_empty" => sig(vec![LirType::Ptr], LirType::Bool, vec![Ptr]),
        "gorget_map_clear" | "gorget_map_free" => sig(vec![LirType::Ptr], LirType::Void, vec![Ptr]),
        "gorget_map_clone" => sig(vec![LirType::Ptr], LirType::Struct(sr.lookup("GorgetMap").unwrap_or(StructId(0))), vec![Ptr]),
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
        "gorget_mutex_lock" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_mutex_lock_to" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Void),
        "gorget_mutex_free" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_guard_release" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_guard_get" => auto(vec![LirType::Ptr], LirType::Ptr),
        // gorget_guard_set(guard*, void* val, size_t size)
        "gorget_guard_set" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, VoidElem, Scalar]),
        "gorget_guard_get_ptr" => auto(vec![LirType::Ptr], LirType::Ptr),

        // Shared methods
        // gorget_shared_new(size, void* initial_val)
        "gorget_shared_new" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, VoidElem]),
        "gorget_shared_clone" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_shared_drop" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_shared_get" | "gorget_shared_get_ptr" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_shared_strong_count" => auto(vec![LirType::Ptr], LirType::I64),
        "gorget_shared_downgrade" => auto(vec![LirType::Ptr], LirType::Ptr),

        // Weak methods
        "gorget_weak_clone" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_weak_drop" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_weak_upgrade" => auto(vec![LirType::Ptr], LirType::I64),

        // Channel methods
        "gorget_channel_new" => auto(vec![LirType::I64, LirType::I64], LirType::Ptr),
        // gorget_channel_send(ch*, void* elem)
        "gorget_channel_send" => sig(vec![LirType::Ptr, LirType::Ptr], LirType::Void, vec![Ptr, VoidElem]),
        "gorget_channel_recv" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Void),
        "gorget_channel_close" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_channel_len" | "gorget_channel_capacity" => auto(vec![LirType::Ptr], LirType::I64),
        "gorget_channel_is_closed" => auto(vec![LirType::Ptr], LirType::Bool),
        "gorget_channel_retain" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_channel_release" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_channel_free" => auto(vec![LirType::Ptr], LirType::Void),

        // RWLock / ReadGuard / WriteGuard methods
        // gorget_rwlock_new(size, void* initial_val)
        "gorget_rwlock_new" => sig(vec![LirType::I64, LirType::Ptr], LirType::Ptr, vec![Scalar, VoidElem]),
        "gorget_rwlock_read" | "gorget_rwlock_write" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_rwlock_read_to" | "gorget_rwlock_write_to" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Void),
        "gorget_rwlock_free" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_read_guard_get" | "gorget_read_guard_get_ptr" => auto(vec![LirType::Ptr], LirType::Ptr),
        "gorget_read_guard_release" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_write_guard_get" | "gorget_write_guard_get_ptr" => auto(vec![LirType::Ptr], LirType::Ptr),
        // gorget_write_guard_set(guard*, void* val, size_t size)
        "gorget_write_guard_set" => sig(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void, vec![Ptr, VoidElem, Scalar]),
        "gorget_write_guard_release" => auto(vec![LirType::Ptr], LirType::Void),

        // Allocator push/pop stubs
        "__gorget_push_allocator" => auto(vec![LirType::Ptr], LirType::Void),
        "__gorget_pop_allocator" => auto(vec![], LirType::Void),

        // chr/ord
        "gorget_char_chr" => auto(vec![LirType::I64], s()),
        "gorget_str_ord" => auto(vec![s()], LirType::I64),
        // Conversion helpers
        "gorget_int_to_str" => auto(vec![LirType::I64], s()),
        "gorget_float_to_str" => auto(vec![LirType::F64], s()),
        "gorget_bool_to_str" => auto(vec![LirType::Bool], s()),
        "gorget_codepoint_to_utf8" => auto(vec![LirType::I64], s()),
        "gorget_int_to_float" => auto(vec![LirType::I64], LirType::F64),
        // I/O
        "gorget_read_file" => auto(vec![LirType::Ptr], g()),
        "gorget_write_file" | "gorget_append_file" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Void),
        "gorget_file_exists" | "gorget_is_dir" => auto(vec![LirType::Ptr], LirType::Bool),
        // Math (integer)
        "gorget_abs" => auto(vec![LirType::I64], LirType::I64),
        "gorget_min" | "gorget_max" => auto(vec![LirType::I64, LirType::I64], LirType::I64),
        // Math (float)
        "gorget_fabs" => auto(vec![LirType::F64], LirType::F64),
        "gorget_fmin" | "gorget_fmax" => auto(vec![LirType::F64, LirType::F64], LirType::F64),
        "gorget_sqrt" | "gorget_floor" | "gorget_ceil" | "gorget_round"
        | "gorget_log" | "gorget_log2" | "gorget_log10"
        | "gorget_sin" | "gorget_cos" | "gorget_tan"
        | "gorget_asin" | "gorget_acos" | "gorget_atan" => {
            auto(vec![LirType::F64], LirType::F64)
        }
        "gorget_pow" | "gorget_atan2" => auto(vec![LirType::F64, LirType::F64], LirType::F64),
        // Random
        "gorget_rand" => auto(vec![], LirType::I64),
        "gorget_rand_range" => auto(vec![LirType::I64, LirType::I64], LirType::I64),
        "gorget_seed" => auto(vec![LirType::I64], LirType::Void),
        // Time
        "gorget_time" | "gorget_time_ms" => auto(vec![], LirType::I64),
        "gorget_sleep_ms" | "gorget_reactor_sleep_ms" => auto(vec![LirType::I64], LirType::Void),
        "gorget_format_time" => auto(vec![LirType::I64, LirType::Ptr], LirType::Ptr),
        "gorget_parse_time" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::I64),

        // Barrier
        "gorget_barrier_new" => auto(vec![LirType::I64], LirType::Ptr),
        "gorget_barrier_wait" | "gorget_barrier_free" => auto(vec![LirType::Ptr], LirType::Void),
        // CondVar
        "gorget_condvar_new" => auto(vec![], LirType::Ptr),
        "gorget_condvar_notify_one" | "gorget_condvar_notify_all" | "gorget_condvar_free" => {
            auto(vec![LirType::Ptr], LirType::Void)
        }
        "gorget_condvar_wait_guard" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Void),
        // AtomicInt
        "gorget_atomic_int_new" => auto(vec![LirType::I64], LirType::Ptr),
        "gorget_atomic_int_load" => auto(vec![LirType::Ptr], LirType::I64),
        "gorget_atomic_int_store" => auto(vec![LirType::Ptr, LirType::I64], LirType::Void),
        "gorget_atomic_int_add" | "gorget_atomic_int_sub" => auto(vec![LirType::Ptr, LirType::I64], LirType::I64),
        "gorget_atomic_int_compare_exchange" => auto(vec![LirType::Ptr, LirType::I64, LirType::I64], LirType::Bool),
        "gorget_atomic_int_free" => auto(vec![LirType::Ptr], LirType::Void),
        // AtomicBool
        "gorget_atomic_bool_new" => auto(vec![LirType::Bool], LirType::Ptr),
        "gorget_atomic_bool_load" => auto(vec![LirType::Ptr], LirType::Bool),
        "gorget_atomic_bool_store" => auto(vec![LirType::Ptr, LirType::Bool], LirType::Void),
        "gorget_atomic_bool_swap" => auto(vec![LirType::Ptr, LirType::Bool], LirType::Bool),
        "gorget_atomic_bool_compare_exchange" => auto(vec![LirType::Ptr, LirType::Bool, LirType::Bool], LirType::Bool),
        "gorget_atomic_bool_free" => auto(vec![LirType::Ptr], LirType::Void),
        // Process
        "gorget_process_spawn" => auto(vec![LirType::Ptr, LirType::Ptr], LirType::Ptr),
        "gorget_process_wait" | "gorget_process_pid" => auto(vec![LirType::Ptr], LirType::I64),
        "gorget_process_kill" | "gorget_process_close_stdin" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_process_write_stdin" => auto(vec![LirType::Ptr, s()], LirType::Void),
        "gorget_process_read_stdout" | "gorget_process_read_stderr" => auto(vec![LirType::Ptr], g()),

        // Panic / abort functions (void return)
        "gorget_panic" => auto(vec![LirType::Ptr], LirType::Void),
        "gorget_assert_fail" => auto(vec![LirType::Ptr, LirType::Ptr, LirType::I64], LirType::Void),
        "gorget_overflow_add" | "gorget_overflow_sub" | "gorget_overflow_mul" => {
            auto(vec![], LirType::Void)
        }
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
pub(super) fn clone_fn_for_collection_element(elem_type_name: &str) -> Option<&'static str> {
    if elem_type_name.starts_with("Vector__")
        || elem_type_name.starts_with("Deque__")
        || elem_type_name == "GorgetArray"
    {
        Some("gorget_array_clone")
    } else if elem_type_name.starts_with("Dict__")
        || elem_type_name.starts_with("HashMap__")
        || elem_type_name == "GorgetMap"
    {
        Some("gorget_map_clone")
    } else if elem_type_name.starts_with("Set__")
        || elem_type_name.starts_with("HashSet__")
        || elem_type_name == "GorgetSet"
    {
        Some("gorget_set_clone")
    } else if elem_type_name == "GorgetString" {
        Some("gorget_string_clone_to_owned")
    } else {
        None
    }
}

/// Map monomorphized GIR function names to their C runtime equivalents.
/// E.g., `Vector__GorgetString__push` → `gorget_array_push`,
///       `Dict__GorgetString__int64_t__put` → `gorget_map_put`,
///       `GorgetString__to_upper` → `gorget_str_to_upper`.
/// Returns true if `s` is a known C type name (indicating the "method" part of a
/// monomorphized name is actually a type parameter, not a method name).
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
            | "each" | "find" | "find_index" | "sorted" | "sort" | "unique" | "count" => return None,
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
            | "update" | "get_or" | "get_or_put" => return None,
            // Dict.new() needs gorget_dict_new (ordered); all other methods use gorget_map_*
            "new" if name.starts_with("Dict__") => return Some("gorget_dict_new".into()),
            "has" => return Some("gorget_map_contains".into()),
            "set" => return Some("gorget_map_put".into()),
            _ => return Some(format!("gorget_map_{method}")),
        }
    }
    // Set__T__method → gorget_set_method
    // GorgetSet__method → gorget_set_method
    // Higher-order methods and non-runtime set operations keep monomorphized names.
    if name.starts_with("Set__") || name.starts_with("HashSet__") || name.starts_with("GorgetSet__") {
        let method = name.rsplit("__").next()?;
        match method {
            "filter" | "fold" | "each" | "any" | "all" | "map"
            | "is_subset" | "is_superset"
            | "union" | "intersection" | "difference" | "symmetric_difference" => return None,
            "has" => return Some("gorget_set_contains".into()),
            "insert" => return Some("gorget_set_add".into()),
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
