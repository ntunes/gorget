//! Printf format-string rewriting and argument decomposition.
//!
//! Lifted from `emit_call_extern.rs` (TODO line 273, decomposition entry)
//! following the same pattern as `emit_hof.rs` (2026-05-16 extraction of the
//! Option/Result combinator inline expansion). Pure relocation — no logic
//! changes; the generated C is byte-identical before and after.
//!
//! The cluster handles three printf-specific concerns at the C-emit
//! boundary:
//!
//!   1. `need_fmt_fix` — detects whether the printf call has any Float /
//!      Str / GorgetString / PtrTo(Str) args while its format string is a
//!      `StrLit`, in which case we'll need to rewrite `%lld → %f / %.*s`
//!      and decompose Str args into `(len, data)` pairs.
//!
//!   2. `try_emit_single_arg_printf` — fast path for `print("hello")` /
//!      `print(str_var)` (no format string). macOS clang rejects bare
//!      `printf(str_arg)` under `-Wformat-security`, and under 32-byte Str
//!      the arg is a struct rather than a `const char*`, so we emit the
//!      decomposed `printf("%.*s", (int)len, data)` form directly.
//!
//!   3. `try_emit_printf_arg` — per-arg handler in the call's main arg
//!      loop. Rewrites the format-string arg (Str-aware `%lld → %.*s`),
//!      decomposes 32-byte Str / GorgetString structs into
//!      `(int)len, data`, derefs `PtrTo(Str)` to read the struct fields,
//!      and casts pre-split data pointers to `(const char*)`.
//!
//! The C-emit boundary is the documented carve-out to CLAUDE.md's
//! "no name matching" rule: the runtime symbol *is* the contract with
//! libc. The `is_printf` detection itself stays at the call site (it
//! routes `emit_name` selection too); this module is only the rewrite
//! machinery downstream of that flag.

use super::*;

/// Compute whether the printf call needs format-string rewriting.
///
/// Returns `true` when `is_printf` is set, the first arg is a `StrLit`
/// (so we can read the format string at compile time), and at least one
/// non-format arg is a Float / Str-by-value / PtrTo(Str) / GorgetString
/// struct — i.e. an arg that doesn't match the default GIR-generated
/// `%lld` placeholder.
pub(super) fn need_fmt_fix(
    is_printf: bool,
    emit_args: &[ValueId],
    ctx: &super::EmitContext,
) -> bool {
    let module = ctx.module;
    let val_types = ctx.val_types;
    let ptr_pointee = ctx.ptr_pointee;
    let fmt_arg_id = if is_printf && !emit_args.is_empty() {
        emit_args.first()
    } else { None };
    is_printf && fmt_arg_id.map_or(false, |fid| {
        ctx.is_str_lit(*fid)
    }) && emit_args.iter().skip(1).any(|a| {
        let ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
        let ptr_to_str = is_str_ptr_opt(ty, module)
            || (matches!(ty, Some(LirType::Ptr)) && ptr_pointee.get(a.0 as usize)
                .and_then(|p| p.as_ref())
                .map_or(false, |p| is_str_struct(p, module)));
        matches!(ty, Some(LirType::F32 | LirType::F64))
        || matches!(ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"))
        || ptr_to_str
    })
}

/// Try to emit the single-arg printf fast path: `print("hello")` /
/// `print(str_var)` with no format string.
///
/// macOS clang rejects bare `printf(str_arg)` under `-Wformat-security`,
/// and under 32-byte Str the arg is a struct rather than a `const char*`,
/// so we emit `printf("%.*s", (int)len, data)` directly. For
/// `print(..., file=stderr)`, the `fprintf(stderr, ...)` prefix is folded
/// in via `stderr_prefix` — the FILE* itself is not in `emit_args` (it
/// was the Null placeholder stripped by the caller).
///
/// Returns `true` if the fast path was emitted (caller should `return`).
/// Returns `false` if the arg shape doesn't match (Str struct or
/// PtrTo(Str)), and the caller should fall through to the raw call.
pub(super) fn try_emit_single_arg_printf(
    out: &mut String,
    is_printf: bool,
    is_stderr_print: bool,
    emit_name: &str,
    emit_args: &[ValueId],
    ctx: &super::EmitContext,
) -> bool {
    let module = ctx.module;
    let val_types = ctx.val_types;
    let ptr_pointee = ctx.ptr_pointee;
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };
    let printf_needs_fmt_guard = is_printf && emit_args.len() == 1;
    if !printf_needs_fmt_guard {
        return false;
    }
    let a = emit_args[0];
    let arg_ty = val_types.get(a.0 as usize).and_then(|t| t.as_ref());
    let is_gs_struct = matches!(arg_ty, Some(LirType::Struct(sid))
        if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
    let stderr_prefix = if is_stderr_print { "stderr, " } else { "" };
    if is_gs_struct {
        // `print(str)` → `__gorget_printf("%.*s", (int)str.len, (const char*)str.data);`
        writeln!(out, "{}({}\"%.*s\", (int){vv}.len, (const char*){vv}.data);",
            emit_name, stderr_prefix, vv = v(a)).unwrap();
        return true;
    }
    // Ptr(Str) fallback: deref the pointer and use struct fields.
    let pointee_is_str = matches!(arg_ty, Some(LirType::Ptr))
        && ptr_pointee.get(a.0 as usize)
            .and_then(|p| p.as_ref())
            .map_or(false, |p| is_str_struct(p, module));
    if pointee_is_str || is_str_ptr_opt(arg_ty, module) {
        writeln!(out, "{}({}\"%.*s\", (int)((Str*){vv})->len, (const char*)((Str*){vv})->data);",
            emit_name, stderr_prefix, vv = v(a)).unwrap();
        return true;
    }
    // Rare non-Str path (shouldn't normally happen) — caller falls through to raw call.
    false
}

/// Try to emit a printf-specific rewrite for the i-th arg of an
/// in-progress printf call. Called from the main arg-emit loop after the
/// ABI-marshalling fall-throughs have been attempted.
///
/// Handles four cases (in order):
///
///   1. i == 0 (the format-string arg) when it's a `StrLit`: scan the
///      remaining args' types, build the `PrintfArgKind` vector, and
///      rewrite `%lld → %f / %.*s` via
///      `crate::lir::lower::calls::fix_printf_format`.
///   2. i > 0 and the arg is a `Struct(GorgetString)`: decompose to
///      `(int)v.len, (const char*)v.data` for `%.*s`.
///   3. i > 0 and the arg is `PtrTo(Str)` / a pointer with `Str` pointee:
///      deref to `(int)((Str*)v)->len, (const char*)((Str*)v)->data`.
///   4. i > 1 and the arg is a raw `Ptr` preceded by an `I32` arg: this
///      is the data half of a pre-decomposed `(len, data)` pair from
///      LIR lowering — cast to `(const char*)`.
///
/// Returns `true` if a rewrite was emitted (caller should `continue`).
/// Returns `false` if no printf-specific handling applies and the caller
/// should fall through to the general per-arg dispatch.
pub(super) fn try_emit_printf_arg(
    out: &mut String,
    i: usize,
    a: ValueId,
    arg_ty: Option<&LirType>,
    is_str_lit: bool,
    need_fmt_fix: bool,
    is_printf: bool,
    emit_args: &[ValueId],
    ctx: &super::EmitContext,
) -> bool {
    let func = ctx.func;
    let module = ctx.module;
    let val_types = ctx.val_types;
    let ptr_pointee = ctx.ptr_pointee;
    let v = |id: ValueId| -> String { format!("__v{}", id.0) };

    // Printf format rewriting for float/bool is handled by LIR lowering.
    // Str decomposition for CallExtern @printf with PtrTo(Str) args is still
    // needed here because the LIR lowering can't detect Str type for all values
    // (e.g., gorget_array_get returns generic void*).
    if need_fmt_fix && i == 0 && is_str_lit {
        // Rewrite format string: %lld → %.*s for Str args at emit time.
        let fmt_val = a;
        let mut fmt_text: Option<&str> = None;
        'find_fmt: for blk in &func.blocks {
            for inst2 in &blk.insts {
                if let Inst::StrLit { dst, value } = inst2 {
                    if *dst == fmt_val {
                        fmt_text = Some(value.as_str());
                        break 'find_fmt;
                    }
                }
            }
        }
        if let Some(fmt) = fmt_text {
            // Fix Str and Float args (bool already handled by LIR lowering)
            use crate::lir::lower::calls::PrintfArgKind;
            let arg_kinds: Vec<PrintfArgKind> = emit_args[1..].iter()
                .map(|ea| {
                    let ty = val_types.get(ea.0 as usize).and_then(|t| t.as_ref());
                    let ea_str_ptr = is_str_ptr_opt(ty, module)
                        || (matches!(ty, Some(LirType::Ptr)) && ptr_pointee.get(ea.0 as usize)
                            .and_then(|p| p.as_ref())
                            .map_or(false, |p| is_str_struct(p, module)))
                        || matches!(ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"));
                    if matches!(ty, Some(LirType::F32 | LirType::F64)) {
                        PrintfArgKind::Float
                    } else if ea_str_ptr {
                        PrintfArgKind::Str
                    } else {
                        PrintfArgKind::Int
                    }
                })
                .collect();
            if arg_kinds.iter().any(|k| *k != PrintfArgKind::Int) {
                let fixed = crate::lir::lower::calls::fix_printf_format(fmt, &arg_kinds);
                let escaped = escape_c_string(&fixed);
                write!(out, "\"{}\"", escaped).unwrap();
            } else {
                write!(out, "{}", v(a)).unwrap();
            }
        } else {
            write!(out, "{}", v(a)).unwrap();
        }
        return true;
    }
    // Decompose 32-byte Str / GorgetString args into (int)len, data for %.*s format.
    // Struct is a value type; empty strings are {"", 0, 0, NULL} so len/data read safely.
    if need_fmt_fix && is_printf && i > 0
        && matches!(arg_ty, Some(LirType::Struct(sid)) if module.structs.get(sid.0 as usize).map_or(false, |s| s.name == "GorgetString"))
    {
        write!(out, "(int){v}.len, (const char*){v}.data", v = v(a)).unwrap();
        return true;
    }
    // PtrTo(Str) in printf — deref the pointer, then read struct fields.
    if need_fmt_fix && is_printf && i > 0 && (is_str_ptr_opt(arg_ty, module)
        || (matches!(arg_ty, Some(LirType::Ptr)) && ptr_pointee.get(a.0 as usize)
            .and_then(|p| p.as_ref())
            .map_or(false, |p| is_str_struct(p, module)))) {
        write!(out, "(int)((Str*){v})->len, (const char*)((Str*){v})->data", v = v(a)).unwrap();
        return true;
    }
    // Pre-decomposed %.*s data arg: the LIR lowering splits Str into
    // (int32)len + (Ptr)data as separate args. The len was already emitted;
    // the data arg is a raw void* from a FieldPtr→Load on Str.data.
    // Cast to (const char*) to satisfy printf's %s expectation.
    if need_fmt_fix && is_printf && i > 1
        && matches!(arg_ty, Some(LirType::Ptr))
    {
        // Check if the preceding arg was i32 (the len half of a %.*s pair)
        let prev_ty = val_types.get(emit_args[i-1].0 as usize).and_then(|t| t.as_ref());
        if matches!(prev_ty, Some(LirType::I32)) {
            write!(out, "(const char*){}", v(a)).unwrap();
            return true;
        }
    }
    false
}
