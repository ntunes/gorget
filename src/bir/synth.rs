//! BIR module-level synthesis.
//!
//! Synthesizes new `LirFunction` definitions from canonical high-level ops
//! (today: sort_by family; future: any op that needs a dedicated specialized
//! body the backends shouldn't reinvent per-type).
//!
//! See `docs/devbook/16-bir.md` for the design.
//!
//! ## Opaque-closure invariant
//!
//! Synthesized bodies dispatch closures exclusively via `Inst::CallClosure`.
//! They never inspect env bytes, never read fields on the closure Ptr,
//! never depend on closure struct layout. This is what makes the dedup
//! key `(op, element_ty, closure_call_fn_name)` safe: two captures with
//! the same closure type but different env values share one synth fn and
//! pass distinct closure pointers at runtime; since the body only talks
//! to the closure through CallClosure, env layout is irrelevant.
//!
//! ## Naming
//!
//! All synthesized functions use the reserved prefix `__gg_synth_`.
//! Monomorphization never produces names starting with this prefix;
//! `lower_lir_to_bir` asserts on entry that the invariant holds.

use crate::ir::abi::AbiKind;
use crate::lir::lower::types::c_sizeof_lir_type;
use crate::lir::{
    BlockId, CmpOp, ClosureDispatchKind, FuncId, HofOp, Inst, LirFunction, LirModule, LirType,
    Overflow, StructDef, StructId, Term, ValueId,
};
use rustc_hash::FxHashMap;

/// Reserved prefix for all synthesized function names.
pub const SYNTH_PREFIX: &str = "__gg_synth_";

/// De-duplication key for synthesized functions.
///
/// Two call sites with the same `SynthKey` share one synthesized body.
/// Keyed by the info actually used inside the body: element type,
/// closure arg ABIs, and closure return type. The closure's `__call`
/// function name is NOT part of the key because the body dispatches
/// through the closure pointer via `CallClosure`, never by direct name
/// — two different closures with the same ABI shape share one body.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct SynthKey {
    /// Which HOF this specializes.
    pub op: HofOp,
    /// Mangled element type.
    pub element_ty: String,
    /// Mangled closure arg-ABI list.
    pub closure_arg_abis: String,
    /// Mangled closure return type.
    pub closure_ret_ty: String,
}

/// Closure signature snapshot — params (env stripped for `__Closure_N__call`)
/// plus return type. Test-only helper used by the synth unit tests to
/// build synthetic modules; production synth paths derive signatures
/// directly from the `LirFunction` rather than going through this struct.
#[cfg(test)]
#[derive(Debug, Clone)]
pub(crate) struct ClosureSig {
    pub param_tys: Vec<LirType>,
    pub ret_ty: LirType,
}

/// Look up the call signature of a closure's `__call` function (or a bare
/// FuncRef target) from the module's current function list. Test-only.
#[cfg(test)]
pub(crate) fn closure_call_sig_of(module: &LirModule, name: &str) -> Option<ClosureSig> {
    let f = module.functions.iter().find(|f| f.name == name)?;
    let skip = if name.starts_with("__Closure_") && name.ends_with("__call") {
        1
    } else {
        0
    };
    Some(ClosureSig {
        param_tys: f.params.iter().skip(skip).cloned().collect(),
        ret_ty: f.return_type.clone(),
    })
}

/// De-duplication key for synthesized trait-helper functions.
///
/// One helper per unique `(trait, method, signature)` shape. The
/// signature mangle folds return type and user-param types together so
/// two calls of the same method with different type substitutions (if
/// any ever arise) don't collide. In normal usage a single
/// `(trait, method)` pair has one signature and this collapses to the
/// one-helper-per-pair guarantee the plan promises.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct TraitHelperKey {
    pub trait_obj_struct: StructId,
    pub method_idx: u32,
    /// Same-trait-method instances may differ in concrete user-param /
    /// return types after monomorphization, so the signature mangle
    /// stays in the key.
    pub sig_mangle: String,
}

/// Pool of synthesized functions for one BIR lowering pass.
pub(crate) struct SynthPool {
    cache: FxHashMap<SynthKey, FuncId>,
    trait_cache: FxHashMap<TraitHelperKey, FuncId>,
    new_fns: Vec<LirFunction>,
    base_func_count: u32,
}

impl SynthPool {
    pub fn new(base_func_count: u32) -> Self {
        Self {
            cache: FxHashMap::default(),
            trait_cache: FxHashMap::default(),
            new_fns: Vec::new(),
            base_func_count,
        }
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.new_fns.len()
    }

    /// Request (and if necessary emit) the sort_by / sorted_by synth fn for
    /// `(element_ty, closure_arg_abis, closure_ret_ty)`. SortBy and
    /// SortedBy share the same impl — the caller wraps the Sorted
    /// variant with clone+return at the call site.
    pub fn get_or_emit_sort_impl(
        &mut self,
        structs: &[StructDef],
        op: HofOp,
        element_ty: &LirType,
        closure_arg_abis: &[AbiKind],
        closure_ret_ty: &LirType,
    ) -> FuncId {
        debug_assert!(
            matches!(op, HofOp::SortBy | HofOp::SortedBy),
            "get_or_emit_sort_impl: unexpected HofOp {op:?}",
        );
        self.emit_or_reuse_sort(
            structs,
            HofOp::SortBy,
            element_ty,
            closure_arg_abis,
            closure_ret_ty,
            "sort_impl",
            /* is_key_variant = */ false,
        )
    }

    /// Request the sort_by_key / sorted_by_key synth fn. Closure is `K(T)`:
    /// takes a single element, returns a key of type K. The body extracts
    /// keys from both elements per compare and branches on `K`'s natural
    /// ordering.
    ///
    /// Handles scalar AND aggregate keys. Comparison is chosen by `K`: a
    /// direct `Cmp Le` for scalars, `gorget_str_cmp` for `Str`/`GorgetString`,
    /// and `memcmp` over `sizeof(K)` for any other struct key.
    pub fn get_or_emit_sort_by_key_impl(
        &mut self,
        structs: &[StructDef],
        op: HofOp,
        element_ty: &LirType,
        closure_arg_abis: &[AbiKind],
        closure_ret_ty: &LirType,
    ) -> FuncId {
        debug_assert!(
            matches!(op, HofOp::SortByKey | HofOp::SortedByKey),
            "get_or_emit_sort_by_key_impl: unexpected HofOp {op:?}",
        );
        self.emit_or_reuse_sort(
            structs,
            HofOp::SortByKey,
            element_ty,
            closure_arg_abis,
            closure_ret_ty,
            "sort_impl_key",
            /* is_key_variant = */ true,
        )
    }

    fn emit_or_reuse_sort(
        &mut self,
        structs: &[StructDef],
        canonical_op: HofOp,
        element_ty: &LirType,
        closure_arg_abis: &[AbiKind],
        closure_ret_ty: &LirType,
        name_prefix: &str,
        is_key_variant: bool,
    ) -> FuncId {
        let element_ty_mangle = mangle_lir_type(element_ty);
        let abis_mangle = mangle_abi_list(closure_arg_abis);
        let ret_mangle = mangle_lir_type(closure_ret_ty);
        let key = SynthKey {
            op: canonical_op,
            element_ty: element_ty_mangle.clone(),
            closure_arg_abis: abis_mangle.clone(),
            closure_ret_ty: ret_mangle.clone(),
        };
        if let Some(&fid) = self.cache.get(&key) {
            return fid;
        }
        let fname = format!(
            "{SYNTH_PREFIX}{name_prefix}_{element_ty_mangle}_{abis_mangle}_{ret_mangle}"
        );
        let mut func =
            LirFunction::new(fname, vec![LirType::Ptr, LirType::Ptr], LirType::Void);
        func.param_names = vec![Some("arr".into()), Some("cl".into())];
        func.const_params = vec![false, false];

        emit_sort_impl_body(
            &mut func,
            structs,
            element_ty,
            closure_arg_abis,
            if is_key_variant {
                Some(closure_ret_ty.clone())
            } else {
                None
            },
        );

        let fid = FuncId(self.base_func_count + self.new_fns.len() as u32);
        self.cache.insert(key, fid);
        self.new_fns.push(func);
        fid
    }

    /// Request (and if necessary emit) a trait-dispatch helper for the
    /// method at `method_idx` of the given trait. Callers use this to
    /// replace inline vtable dispatch with a single `Call` to a dedup'd
    /// helper; the helper's body contains the `FieldPtr+Load×3 + CallPtr`
    /// chain (same shape as the legacy inline expansion in `bir::lower`).
    ///
    /// `trait_obj_struct` points to `{Trait}_TraitObj`; `method_idx` is
    /// the method's slot in `{Trait}_VTable`. The cache is keyed on the
    /// resolved IDs (plus the user signature mangle), so the dedup is
    /// invariant under the trait's name.
    ///
    /// Signature: `fn(self: Ptr, ...user_params) -> ret_ty`. The `self`
    /// param is the `{Trait}_TraitObj*`; user params are the method's
    /// user parameters (the VTable FnPtr's `params[1..]`, skipping the
    /// synthetic `void* data`).
    pub fn get_or_emit_trait_helper(
        &mut self,
        structs: &[StructDef],
        trait_obj_struct: StructId,
        method_idx: u32,
        user_param_tys: &[LirType],
        ret_ty: &LirType,
    ) -> FuncId {
        let sig_mangle = std::iter::once(mangle_lir_type(ret_ty))
            .chain(user_param_tys.iter().map(mangle_lir_type))
            .collect::<Vec<_>>()
            .join("_");
        let key = TraitHelperKey {
            trait_obj_struct,
            method_idx,
            sig_mangle,
        };
        if let Some(&fid) = self.trait_cache.get(&key) {
            return fid;
        }

        // Recover trait + method names from the LIR for diagnostic-quality
        // helper-fn naming. The `_TraitObj` suffix is invariant (registered
        // by GIR lowering); the matching VTable carries the method slot.
        let trait_obj_name = &structs[trait_obj_struct.0 as usize].name;
        let trait_name = trait_obj_name
            .strip_suffix("_TraitObj")
            .expect("TraitCall trait_obj_struct must point to a `*_TraitObj` struct");
        let vtable_name = format!("{trait_name}_VTable");
        let vtable_sid = structs.iter().position(|s| s.name == vtable_name)
            .unwrap_or_else(|| panic!(
                "trait helper: missing `{vtable_name}` struct (paired with `{trait_obj_name}`)"
            ));
        let method_name = &structs[vtable_sid].fields[method_idx as usize].0;

        let fname = format!("{SYNTH_PREFIX}trait_{trait_name}_{method_name}");
        let mut params = Vec::with_capacity(1 + user_param_tys.len());
        params.push(LirType::Ptr);
        params.extend(user_param_tys.iter().cloned());
        let mut func = LirFunction::new(fname, params, ret_ty.clone());
        func.param_names = std::iter::once(Some("self".to_string()))
            .chain((0..user_param_tys.len()).map(|i| Some(format!("arg{i}"))))
            .collect();
        func.const_params = vec![false; func.params.len()];

        emit_trait_helper_body(&mut func, structs, trait_name, method_name, user_param_tys, ret_ty);

        let fid = FuncId(self.base_func_count + self.new_fns.len() as u32);
        self.trait_cache.insert(key, fid);
        self.new_fns.push(func);
        fid
    }

    pub fn finish(self) -> Vec<LirFunction> {
        self.new_fns
    }
}

/// Debug guardrail: assert no function outside the pool already carries
/// the reserved `__gg_synth_` prefix. Called at pass entry.
pub(crate) fn assert_no_synth_prefix(module: &LirModule) {
    if let Some(f) = module
        .functions
        .iter()
        .find(|f| f.name.starts_with(SYNTH_PREFIX))
    {
        panic!(
            "BIR synthesis: function `{}` already carries reserved prefix \
             `{SYNTH_PREFIX}`. The prefix is reserved for synthesis output; \
             rename the offending function.",
            f.name
        );
    }
}

/// Mangle an `AbiKind` list into a string suitable for function-name suffixing.
fn mangle_abi_list(abis: &[AbiKind]) -> String {
    abis.iter()
        .map(|a| match a {
            AbiKind::Auto => "a",
            AbiKind::CStr => "c",
            AbiKind::BytePtr => "b",
            AbiKind::GorgetString => "g",
            AbiKind::Scalar => "s",
            AbiKind::Ptr => "p",
            AbiKind::Opaque => "o",
            AbiKind::ByValue => "v",
            AbiKind::VoidElem => "e",
            AbiKind::OutPtr => "u",
        })
        .collect::<Vec<_>>()
        .join("")
}

/// Mangle a `LirType` into a string suitable for function-name suffixing.
fn mangle_lir_type(ty: &LirType) -> String {
    match ty {
        LirType::I8 => "i8".into(),
        LirType::I16 => "i16".into(),
        LirType::I32 => "i32".into(),
        LirType::I64 => "i64".into(),
        LirType::U8 => "u8".into(),
        LirType::U16 => "u16".into(),
        LirType::U32 => "u32".into(),
        LirType::U64 => "u64".into(),
        LirType::F32 => "f32".into(),
        LirType::F64 => "f64".into(),
        LirType::Bool => "bool".into(),
        LirType::Void => "void".into(),
        LirType::Ptr => "ptr".into(),
        LirType::PtrTo(sid) => format!("ptrTo{}", sid.0),
        LirType::FuncRef => "funcref".into(),
        LirType::Struct(sid) => format!("s{}", sid.0),
        // Item 7e (Phase 1): mangle resource as kind + nested params so
        // synthesized helper names disambiguate Vector[int] from Vector[Foo].
        LirType::Resource { kind, params } => {
            let mut out = format!("res{kind:?}");
            for p in params {
                out.push('_');
                out.push_str(&mangle_lir_type(p));
            }
            out
        }
    }
}

/// Look up a struct ID by name.
fn lookup_struct_id(structs: &[StructDef], name: &str) -> Option<StructId> {
    structs
        .iter()
        .position(|s| s.name == name)
        .map(|i| StructId(i as u32))
}

fn alloc_value(next: &mut u32) -> ValueId {
    let v = ValueId(*next);
    *next += 1;
    v
}

/// Emit the full body of a `sort_impl(arr: Ptr, cl: Ptr) -> Void` function
/// implementing iterative bottom-up stable mergesort.
///
/// Aux buffer is raw bytes allocated via `CallExtern "malloc"`; elements
/// are moved around via `Memcpy` so the body is uniform for scalar and
/// aggregate element types. The closure is dispatched via `CallClosure`
/// with the caller-provided `closure_arg_abis` — for scalar-ABI closures
/// we `Load` element values before passing; for pointer-ABI closures we
/// pass the `ElemPtr` directly.
///
/// Block layout (high level):
/// ```text
///   entry:         n = arr.len; if n<2 → done_nofree; else alloc aux
///   pass_loop:     width = 1, 2, 4, ... while < n
///     run_loop:    left = 0, 2*width, ... while < n
///       merge:     merge arr[left..mid] + arr[mid..right] into aux
///       copy_back: memcpy aux[left..right] back into arr[left..right]
///   free:          free(aux); return
///   done_nofree:   return (n<2 path)
/// ```
/// If `key_ty` is `Some(K)`, the closure is `K(T)`: extract a key from each
/// element before comparing. Compare dispatches on `K`'s shape — numeric/bool
/// keys use direct `Cmp Le`, aggregate keys are rejected upstream in
/// `try_emit_vector_each_hof` (no aggregate-key support in this commit).
///
/// If `key_ty` is `None`, the closure is `int(T, T)`: classic comparator;
/// compare its result against zero.
fn emit_sort_impl_body(
    func: &mut LirFunction,
    structs: &[StructDef],
    element_ty: &LirType,
    closure_arg_abis: &[AbiKind],
    key_ty: Option<LirType>,
) {
    let elem_size = c_sizeof_lir_type(element_ty, structs) as u32;
    let array_sid = lookup_struct_id(structs, "GorgetArray").unwrap_or(StructId(0));
    let mut next: u32 = 0;

    // ── Block allocation ──
    let entry_bb = func.add_block();
    let alloc_bb = func.add_block();
    let pass_check_bb = func.add_block();
    let run_check_bb = func.add_block();
    let compute_mid_bb = func.add_block();
    let skip_run_bb = func.add_block();
    let compute_right_bb = func.add_block();
    let merge_loop_bb = func.add_block();
    let check_j_bb = func.add_block();
    let compare_bb = func.add_block();
    let take_i_bb = func.add_block();
    let take_j_bb = func.add_block();
    let drain_left_bb = func.add_block();
    let drain_right_bb = func.add_block();
    let copy_back_bb = func.add_block();
    let next_width_bb = func.add_block();
    let free_bb = func.add_block();
    let done_bb = func.add_block();

    // Param refs.
    let arr = alloc_value(&mut next);
    let cl = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::ParamRef {
        dst: arr,
        index: 0,
        ty: LirType::Ptr,
    });
    func.block_mut(entry_bb).insts.push(Inst::ParamRef {
        dst: cl,
        index: 1,
        ty: LirType::Ptr,
    });

    // ── entry: load n = arr.len, short-circuit if n < 2 ──
    let lenp = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::FieldPtr {
        dst: lenp,
        base: arr,
        struct_id: array_sid,
        field: 2, // GorgetArray.len
    });
    let n = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::Load {
        dst: n,
        ptr: lenp,
        ty: LirType::I64,
    });
    let two = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::IConst {
        dst: two,
        ty: LirType::I64,
        value: 2,
    });
    let lt2 = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::Cmp {
        dst: lt2,
        op: CmpOp::Lt,
        lhs: n,
        rhs: two,
    });
    func.block_mut(entry_bb).terminator = Term::Branch {
        cond: lt2,
        then_block: done_bb,
        then_args: vec![],
        else_block: alloc_bb,
        else_args: vec![],
    };

    // ── alloc: bytes = n * elem_size; aux = malloc(bytes); width = 1 ──
    let elsz = alloc_value(&mut next);
    func.block_mut(alloc_bb).insts.push(Inst::IConst {
        dst: elsz,
        ty: LirType::I64,
        value: elem_size as i64,
    });
    let bytes = alloc_value(&mut next);
    func.block_mut(alloc_bb).insts.push(Inst::Mul {
        dst: bytes,
        ty: LirType::I64,
        lhs: n,
        rhs: elsz,
        overflow: Overflow::Wrap,
    });
    let aux = alloc_value(&mut next);
    func.block_mut(alloc_bb).insts.push(Inst::CallExtern {
        dst: Some(aux),
        name: "malloc".to_string(),
        args: vec![bytes],
        arg_abis: vec![AbiKind::Scalar],
    });
    let one = alloc_value(&mut next);
    func.block_mut(alloc_bb).insts.push(Inst::IConst {
        dst: one,
        ty: LirType::I64,
        value: 1,
    });
    func.block_mut(alloc_bb).terminator = Term::Jump(pass_check_bb, vec![one]);

    // ── pass_check(width): if width < n, enter the pass; else free & return ──
    let width = alloc_value(&mut next);
    func.block_mut(pass_check_bb)
        .params
        .push((width, LirType::I64));
    let width_lt_n = alloc_value(&mut next);
    func.block_mut(pass_check_bb).insts.push(Inst::Cmp {
        dst: width_lt_n,
        op: CmpOp::Lt,
        lhs: width,
        rhs: n,
    });
    let zero = alloc_value(&mut next);
    func.block_mut(pass_check_bb).insts.push(Inst::IConst {
        dst: zero,
        ty: LirType::I64,
        value: 0,
    });
    func.block_mut(pass_check_bb).terminator = Term::Branch {
        cond: width_lt_n,
        then_block: run_check_bb,
        then_args: vec![zero, width],
        else_block: free_bb,
        else_args: vec![],
    };

    // ── run_check(left, width): if left < n, enter merge; else next_width ──
    let left = alloc_value(&mut next);
    let w_rc = alloc_value(&mut next);
    func.block_mut(run_check_bb).params.push((left, LirType::I64));
    func.block_mut(run_check_bb).params.push((w_rc, LirType::I64));
    let left_lt_n = alloc_value(&mut next);
    func.block_mut(run_check_bb).insts.push(Inst::Cmp {
        dst: left_lt_n,
        op: CmpOp::Lt,
        lhs: left,
        rhs: n,
    });
    func.block_mut(run_check_bb).terminator = Term::Branch {
        cond: left_lt_n,
        then_block: compute_mid_bb,
        then_args: vec![left, w_rc],
        else_block: next_width_bb,
        else_args: vec![w_rc],
    };

    // ── compute_mid(left, width): mid = left + width; if mid>=n skip run ──
    let l_cm = alloc_value(&mut next);
    let w_cm = alloc_value(&mut next);
    func.block_mut(compute_mid_bb)
        .params
        .push((l_cm, LirType::I64));
    func.block_mut(compute_mid_bb)
        .params
        .push((w_cm, LirType::I64));
    let mid_raw = alloc_value(&mut next);
    func.block_mut(compute_mid_bb).insts.push(Inst::Add {
        dst: mid_raw,
        ty: LirType::I64,
        lhs: l_cm,
        rhs: w_cm,
        overflow: Overflow::Wrap,
    });
    let mid_ge_n = alloc_value(&mut next);
    func.block_mut(compute_mid_bb).insts.push(Inst::Cmp {
        dst: mid_ge_n,
        op: CmpOp::Ge,
        lhs: mid_raw,
        rhs: n,
    });
    func.block_mut(compute_mid_bb).terminator = Term::Branch {
        cond: mid_ge_n,
        then_block: skip_run_bb,
        then_args: vec![l_cm, w_cm],
        else_block: compute_right_bb,
        else_args: vec![l_cm, w_cm, mid_raw],
    };

    // ── skip_run(left, width): advance left by 2*width, back to run_check ──
    let l_sr = alloc_value(&mut next);
    let w_sr = alloc_value(&mut next);
    func.block_mut(skip_run_bb).params.push((l_sr, LirType::I64));
    func.block_mut(skip_run_bb).params.push((w_sr, LirType::I64));
    let ww_sr = alloc_value(&mut next);
    func.block_mut(skip_run_bb).insts.push(Inst::Add {
        dst: ww_sr,
        ty: LirType::I64,
        lhs: w_sr,
        rhs: w_sr,
        overflow: Overflow::Wrap,
    });
    let new_l_sr = alloc_value(&mut next);
    func.block_mut(skip_run_bb).insts.push(Inst::Add {
        dst: new_l_sr,
        ty: LirType::I64,
        lhs: l_sr,
        rhs: ww_sr,
        overflow: Overflow::Wrap,
    });
    func.block_mut(skip_run_bb).terminator = Term::Jump(run_check_bb, vec![new_l_sr, w_sr]);

    // ── compute_right(left, width, mid): right = min(left + 2*width, n);
    //                                     start merge ──
    let l_cr = alloc_value(&mut next);
    let w_cr = alloc_value(&mut next);
    let mid_cr = alloc_value(&mut next);
    func.block_mut(compute_right_bb)
        .params
        .push((l_cr, LirType::I64));
    func.block_mut(compute_right_bb)
        .params
        .push((w_cr, LirType::I64));
    func.block_mut(compute_right_bb)
        .params
        .push((mid_cr, LirType::I64));
    let ww_cr = alloc_value(&mut next);
    func.block_mut(compute_right_bb).insts.push(Inst::Add {
        dst: ww_cr,
        ty: LirType::I64,
        lhs: w_cr,
        rhs: w_cr,
        overflow: Overflow::Wrap,
    });
    let right_raw = alloc_value(&mut next);
    func.block_mut(compute_right_bb).insts.push(Inst::Add {
        dst: right_raw,
        ty: LirType::I64,
        lhs: l_cr,
        rhs: ww_cr,
        overflow: Overflow::Wrap,
    });
    // right = right_raw < n ? right_raw : n — via a small branch is painful;
    // emit as an unconditional min via Sub-compare pattern would need Select,
    // which we don't use. Use two Jumps to merge_loop with the right value.
    // Simpler: have two mini-blocks? That's more blocks. Cheapest: compute
    // right = right_raw - ((right_raw > n) ? (right_raw - n) : 0) … still
    // needs conditional. So: use a tiny helper block that clamps.
    //
    // But we can simulate Select with just arithmetic for this specific
    // bounded case: since 0 ≤ right_raw and right_raw ≤ 2n (roughly), we
    // can compute right = n < right_raw ? n : right_raw via:
    //   ge = (right_raw >= n)   // bool, 0 or 1 — but LIR Cmp produces bool
    //   diff = right_raw - n
    // Still needs branching to materialize the result conditionally on bool.
    //
    // Cleanest path: emit two Jump targets inline. Add a conditional Branch
    // into merge_loop_bb with either `right_raw` or `n` passed.
    let rr_lt_n = alloc_value(&mut next);
    func.block_mut(compute_right_bb).insts.push(Inst::Cmp {
        dst: rr_lt_n,
        op: CmpOp::Lt,
        lhs: right_raw,
        rhs: n,
    });
    // Two-target Branch passing the appropriate clamped value.
    func.block_mut(compute_right_bb).terminator = Term::Branch {
        cond: rr_lt_n,
        then_block: merge_loop_bb,
        then_args: vec![l_cr, w_cr, mid_cr, right_raw, l_cr, mid_cr, l_cr],
        else_block: merge_loop_bb,
        else_args: vec![l_cr, w_cr, mid_cr, n, l_cr, mid_cr, l_cr],
    };

    // ── merge_loop(left, width, mid, right, i, j, k): main merge ──
    let l_ml = alloc_value(&mut next);
    let w_ml = alloc_value(&mut next);
    let mid_ml = alloc_value(&mut next);
    let right_ml = alloc_value(&mut next);
    let i_ml = alloc_value(&mut next);
    let j_ml = alloc_value(&mut next);
    let k_ml = alloc_value(&mut next);
    func.block_mut(merge_loop_bb)
        .params
        .push((l_ml, LirType::I64));
    func.block_mut(merge_loop_bb)
        .params
        .push((w_ml, LirType::I64));
    func.block_mut(merge_loop_bb)
        .params
        .push((mid_ml, LirType::I64));
    func.block_mut(merge_loop_bb)
        .params
        .push((right_ml, LirType::I64));
    func.block_mut(merge_loop_bb)
        .params
        .push((i_ml, LirType::I64));
    func.block_mut(merge_loop_bb)
        .params
        .push((j_ml, LirType::I64));
    func.block_mut(merge_loop_bb)
        .params
        .push((k_ml, LirType::I64));
    let i_lt_mid = alloc_value(&mut next);
    func.block_mut(merge_loop_bb).insts.push(Inst::Cmp {
        dst: i_lt_mid,
        op: CmpOp::Lt,
        lhs: i_ml,
        rhs: mid_ml,
    });
    func.block_mut(merge_loop_bb).terminator = Term::Branch {
        cond: i_lt_mid,
        then_block: check_j_bb,
        then_args: vec![l_ml, w_ml, mid_ml, right_ml, i_ml, j_ml, k_ml],
        else_block: drain_right_bb,
        else_args: vec![l_ml, w_ml, mid_ml, right_ml, i_ml, j_ml, k_ml],
    };

    // ── check_j: if j < right, compare; else drain left ──
    let l_cj = alloc_value(&mut next);
    let w_cj = alloc_value(&mut next);
    let mid_cj = alloc_value(&mut next);
    let right_cj = alloc_value(&mut next);
    let i_cj = alloc_value(&mut next);
    let j_cj = alloc_value(&mut next);
    let k_cj = alloc_value(&mut next);
    func.block_mut(check_j_bb).params.push((l_cj, LirType::I64));
    func.block_mut(check_j_bb).params.push((w_cj, LirType::I64));
    func.block_mut(check_j_bb).params.push((mid_cj, LirType::I64));
    func.block_mut(check_j_bb).params.push((right_cj, LirType::I64));
    func.block_mut(check_j_bb).params.push((i_cj, LirType::I64));
    func.block_mut(check_j_bb).params.push((j_cj, LirType::I64));
    func.block_mut(check_j_bb).params.push((k_cj, LirType::I64));
    let j_lt_right = alloc_value(&mut next);
    func.block_mut(check_j_bb).insts.push(Inst::Cmp {
        dst: j_lt_right,
        op: CmpOp::Lt,
        lhs: j_cj,
        rhs: right_cj,
    });
    func.block_mut(check_j_bb).terminator = Term::Branch {
        cond: j_lt_right,
        then_block: compare_bb,
        then_args: vec![l_cj, w_cj, mid_cj, right_cj, i_cj, j_cj, k_cj],
        else_block: drain_left_bb,
        else_args: vec![l_cj, w_cj, mid_cj, right_cj, i_cj, j_cj, k_cj],
    };

    // ── compare: CallClosure(cl, arr[i], arr[j]); if ≤0 take i, else j ──
    let l_cp = alloc_value(&mut next);
    let w_cp = alloc_value(&mut next);
    let mid_cp = alloc_value(&mut next);
    let right_cp = alloc_value(&mut next);
    let i_cp = alloc_value(&mut next);
    let j_cp = alloc_value(&mut next);
    let k_cp = alloc_value(&mut next);
    func.block_mut(compare_bb).params.push((l_cp, LirType::I64));
    func.block_mut(compare_bb).params.push((w_cp, LirType::I64));
    func.block_mut(compare_bb).params.push((mid_cp, LirType::I64));
    func.block_mut(compare_bb).params.push((right_cp, LirType::I64));
    func.block_mut(compare_bb).params.push((i_cp, LirType::I64));
    func.block_mut(compare_bb).params.push((j_cp, LirType::I64));
    func.block_mut(compare_bb).params.push((k_cp, LirType::I64));
    // data = arr.data
    let datap_cp = alloc_value(&mut next);
    func.block_mut(compare_bb).insts.push(Inst::FieldPtr {
        dst: datap_cp,
        base: arr,
        struct_id: array_sid,
        field: 0,
    });
    let data_cp = alloc_value(&mut next);
    func.block_mut(compare_bb).insts.push(Inst::Load {
        dst: data_cp,
        ptr: datap_cp,
        ty: LirType::Ptr,
    });
    // ip = &data[i]; jp = &data[j]
    let ip_cp = alloc_value(&mut next);
    func.block_mut(compare_bb).insts.push(Inst::ElemPtr {
        dst: ip_cp,
        base: data_cp,
        index: i_cp,
        elem_size,
    });
    let jp_cp = alloc_value(&mut next);
    func.block_mut(compare_bb).insts.push(Inst::ElemPtr {
        dst: jp_cp,
        base: data_cp,
        index: j_cp,
        elem_size,
    });
    // Compare: closure signature dictates whether we Load or pass pointer
    // for each element arg.
    let (ivarg, jvarg, arg_abi) = match closure_arg_abis.first().copied() {
        Some(AbiKind::Ptr) => (ip_cp, jp_cp, AbiKind::Ptr),
        _ => {
            let iv = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::Load {
                dst: iv,
                ptr: ip_cp,
                ty: element_ty.clone(),
            });
            let jv = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::Load {
                dst: jv,
                ptr: jp_cp,
                ty: element_ty.clone(),
            });
            (iv, jv, AbiKind::Scalar)
        }
    };
    let le0 = match &key_ty {
        None => {
            // Classic comparator: cl(iv, jv) -> int. Branch on result ≤ 0
            // (stable: ties take the left element).
            let cmpr = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::CallClosure {
                dst: Some(cmpr),
                kind: ClosureDispatchKind::EscapedClosure,
                closure: cl,
                args: vec![ivarg, jvarg],
                arg_abis: vec![arg_abi, arg_abi],
                ret_ty: LirType::I64,
            });
            let zero_cp = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::IConst {
                dst: zero_cp,
                ty: LirType::I64,
                value: 0,
            });
            let out = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::Cmp {
                dst: out,
                op: CmpOp::Le,
                lhs: cmpr,
                rhs: zero_cp,
            });
            out
        }
        Some(k_ty) => {
            // Key variant: cl(iv) -> K; cl(jv) -> K; compare keys per K's
            // natural ordering.
            //
            //   Scalar K (int/bool/float/...): direct `Cmp Le`.
            //   Struct("GorgetString"|"Str"): `gorget_str_cmp(ki, kj) ≤ 0`.
            //   Other struct K: `memcmp(&ki, &kj, sizeof(K)) ≤ 0` — matches
            //     the previous TLS trampoline's fallback for unknown K.
            let ki = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::CallClosure {
                dst: Some(ki),
                kind: ClosureDispatchKind::EscapedClosure,
                closure: cl,
                args: vec![ivarg],
                arg_abis: vec![arg_abi],
                ret_ty: k_ty.clone(),
            });
            let kj = alloc_value(&mut next);
            func.block_mut(compare_bb).insts.push(Inst::CallClosure {
                dst: Some(kj),
                kind: ClosureDispatchKind::EscapedClosure,
                closure: cl,
                args: vec![jvarg],
                arg_abis: vec![arg_abi],
                ret_ty: k_ty.clone(),
            });
            // Pick compare strategy by K's concrete shape.
            let k_struct_name = match &k_ty {
                LirType::Struct(sid) => structs
                    .get(sid.0 as usize)
                    .map(|s| s.name.as_str())
                    .unwrap_or(""),
                _ => "",
            };
            if !k_ty.is_aggregate() {
                // Scalar: direct Cmp Le.
                let out = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::Cmp {
                    dst: out,
                    op: CmpOp::Le,
                    lhs: ki,
                    rhs: kj,
                });
                out
            } else if matches!(k_struct_name, "GorgetString" | "Str") {
                // Str key: gorget_str_cmp takes Str by value; pass the SSA
                // struct values directly. Backend marshals per arg_abi.
                let cmp_int = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::CallExtern {
                    dst: Some(cmp_int),
                    name: "gorget_str_cmp".to_string(),
                    args: vec![ki, kj],
                    arg_abis: vec![AbiKind::GorgetString, AbiKind::GorgetString],
                });
                let zero_cp = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::IConst {
                    dst: zero_cp,
                    ty: LirType::I64,
                    value: 0,
                });
                let out = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::Cmp {
                    dst: out,
                    op: CmpOp::Le,
                    lhs: cmp_int,
                    rhs: zero_cp,
                });
                out
            } else {
                // Generic struct K: memcmp(&ki, &kj, sizeof(K)).
                // Spill SSA values to slots to take addresses.
                let ki_slot = func.add_slot(k_ty.clone(), None);
                func.block_mut(compare_bb).insts.push(Inst::SlotStore {
                    slot: ki_slot,
                    value: ki,
                    is_move: true,
                });
                let ki_ptr = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::SlotAddr {
                    dst: ki_ptr,
                    slot: ki_slot,
                });
                let kj_slot = func.add_slot(k_ty.clone(), None);
                func.block_mut(compare_bb).insts.push(Inst::SlotStore {
                    slot: kj_slot,
                    value: kj,
                    is_move: true,
                });
                let kj_ptr = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::SlotAddr {
                    dst: kj_ptr,
                    slot: kj_slot,
                });
                let k_size = c_sizeof_lir_type(&k_ty, structs) as i64;
                let size_v = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::IConst {
                    dst: size_v,
                    ty: LirType::I64,
                    value: k_size,
                });
                let cmp_int = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::CallExtern {
                    dst: Some(cmp_int),
                    name: "memcmp".to_string(),
                    args: vec![ki_ptr, kj_ptr, size_v],
                    arg_abis: vec![AbiKind::Opaque, AbiKind::Opaque, AbiKind::Scalar],
                });
                let zero_cp = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::IConst {
                    dst: zero_cp,
                    ty: LirType::I64,
                    value: 0,
                });
                let out = alloc_value(&mut next);
                func.block_mut(compare_bb).insts.push(Inst::Cmp {
                    dst: out,
                    op: CmpOp::Le,
                    lhs: cmp_int,
                    rhs: zero_cp,
                });
                out
            }
        }
    };
    func.block_mut(compare_bb).terminator = Term::Branch {
        cond: le0,
        then_block: take_i_bb,
        then_args: vec![l_cp, w_cp, mid_cp, right_cp, i_cp, j_cp, k_cp],
        else_block: take_j_bb,
        else_args: vec![l_cp, w_cp, mid_cp, right_cp, i_cp, j_cp, k_cp],
    };

    // ── take_i: Memcpy(aux[k], data[i], sz); i++; k++ ──
    emit_take_block(
        func,
        &mut next,
        take_i_bb,
        merge_loop_bb,
        arr,
        aux,
        array_sid,
        elsz,
        elem_size,
        /* take_i = */ true,
    );

    // ── take_j: Memcpy(aux[k], data[j], sz); j++; k++ ──
    emit_take_block(
        func,
        &mut next,
        take_j_bb,
        merge_loop_bb,
        arr,
        aux,
        array_sid,
        elsz,
        elem_size,
        /* take_i = */ false,
    );

    // ── drain_left(l,w,mid,right,i,j,k): if i<mid copy i→k, i++,k++; else copy_back ──
    let l_dl = alloc_value(&mut next);
    let w_dl = alloc_value(&mut next);
    let mid_dl = alloc_value(&mut next);
    let right_dl = alloc_value(&mut next);
    let i_dl = alloc_value(&mut next);
    let j_dl = alloc_value(&mut next);
    let k_dl = alloc_value(&mut next);
    func.block_mut(drain_left_bb)
        .params
        .push((l_dl, LirType::I64));
    func.block_mut(drain_left_bb)
        .params
        .push((w_dl, LirType::I64));
    func.block_mut(drain_left_bb)
        .params
        .push((mid_dl, LirType::I64));
    func.block_mut(drain_left_bb)
        .params
        .push((right_dl, LirType::I64));
    func.block_mut(drain_left_bb)
        .params
        .push((i_dl, LirType::I64));
    func.block_mut(drain_left_bb)
        .params
        .push((j_dl, LirType::I64));
    func.block_mut(drain_left_bb)
        .params
        .push((k_dl, LirType::I64));
    emit_drain_block(
        func,
        &mut next,
        drain_left_bb,
        copy_back_bb,
        arr,
        aux,
        array_sid,
        elsz,
        elem_size,
        /* drain_i = */ true,
        l_dl,
        w_dl,
        mid_dl,
        right_dl,
        i_dl,
        j_dl,
        k_dl,
    );

    // ── drain_right: same shape, advancing j ──
    let l_dr = alloc_value(&mut next);
    let w_dr = alloc_value(&mut next);
    let mid_dr = alloc_value(&mut next);
    let right_dr = alloc_value(&mut next);
    let i_dr = alloc_value(&mut next);
    let j_dr = alloc_value(&mut next);
    let k_dr = alloc_value(&mut next);
    func.block_mut(drain_right_bb)
        .params
        .push((l_dr, LirType::I64));
    func.block_mut(drain_right_bb)
        .params
        .push((w_dr, LirType::I64));
    func.block_mut(drain_right_bb)
        .params
        .push((mid_dr, LirType::I64));
    func.block_mut(drain_right_bb)
        .params
        .push((right_dr, LirType::I64));
    func.block_mut(drain_right_bb)
        .params
        .push((i_dr, LirType::I64));
    func.block_mut(drain_right_bb)
        .params
        .push((j_dr, LirType::I64));
    func.block_mut(drain_right_bb)
        .params
        .push((k_dr, LirType::I64));
    emit_drain_block(
        func,
        &mut next,
        drain_right_bb,
        copy_back_bb,
        arr,
        aux,
        array_sid,
        elsz,
        elem_size,
        /* drain_i = */ false,
        l_dr,
        w_dr,
        mid_dr,
        right_dr,
        i_dr,
        j_dr,
        k_dr,
    );

    // ── copy_back(l, w, right): memcpy aux[l..right] back into data[l..right] ──
    let l_cb = alloc_value(&mut next);
    let w_cb = alloc_value(&mut next);
    let right_cb = alloc_value(&mut next);
    func.block_mut(copy_back_bb)
        .params
        .push((l_cb, LirType::I64));
    func.block_mut(copy_back_bb)
        .params
        .push((w_cb, LirType::I64));
    func.block_mut(copy_back_bb)
        .params
        .push((right_cb, LirType::I64));
    let span = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::Sub {
        dst: span,
        ty: LirType::I64,
        lhs: right_cb,
        rhs: l_cb,
        overflow: Overflow::Wrap,
    });
    let span_bytes = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::Mul {
        dst: span_bytes,
        ty: LirType::I64,
        lhs: span,
        rhs: elsz,
        overflow: Overflow::Wrap,
    });
    let datap_cb = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::FieldPtr {
        dst: datap_cb,
        base: arr,
        struct_id: array_sid,
        field: 0,
    });
    let data_cb = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::Load {
        dst: data_cb,
        ptr: datap_cb,
        ty: LirType::Ptr,
    });
    let dstp = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::ElemPtr {
        dst: dstp,
        base: data_cb,
        index: l_cb,
        elem_size,
    });
    let srcp = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::ElemPtr {
        dst: srcp,
        base: aux,
        index: l_cb,
        elem_size,
    });
    func.block_mut(copy_back_bb).insts.push(Inst::Memcpy {
        dst_ptr: dstp,
        src_ptr: srcp,
        size: span_bytes,
    });
    // new_left = l + 2*width
    let ww_cb = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::Add {
        dst: ww_cb,
        ty: LirType::I64,
        lhs: w_cb,
        rhs: w_cb,
        overflow: Overflow::Wrap,
    });
    let new_l_cb = alloc_value(&mut next);
    func.block_mut(copy_back_bb).insts.push(Inst::Add {
        dst: new_l_cb,
        ty: LirType::I64,
        lhs: l_cb,
        rhs: ww_cb,
        overflow: Overflow::Wrap,
    });
    func.block_mut(copy_back_bb).terminator = Term::Jump(run_check_bb, vec![new_l_cb, w_cb]);

    // ── next_width(width): width *= 2, back to pass_check ──
    let w_nw = alloc_value(&mut next);
    func.block_mut(next_width_bb)
        .params
        .push((w_nw, LirType::I64));
    let new_w = alloc_value(&mut next);
    func.block_mut(next_width_bb).insts.push(Inst::Add {
        dst: new_w,
        ty: LirType::I64,
        lhs: w_nw,
        rhs: w_nw,
        overflow: Overflow::Wrap,
    });
    func.block_mut(next_width_bb).terminator = Term::Jump(pass_check_bb, vec![new_w]);

    // ── free: free(aux); jump done ──
    func.block_mut(free_bb).insts.push(Inst::CallExtern {
        dst: None,
        name: "free".to_string(),
        args: vec![aux],
        arg_abis: vec![AbiKind::Opaque],
    });
    func.block_mut(free_bb).terminator = Term::Jump(done_bb, vec![]);

    // ── done: return void ──
    func.block_mut(done_bb).terminator = Term::RetVoid;

    // Block id sanity: `BlockId` assignment order matters only for display —
    // we use the IDs returned by `add_block` consistently above. Rebinding
    // wasn't needed; BlockId values are stable.
    let _ = (
        entry_bb,
        alloc_bb,
        pass_check_bb,
        run_check_bb,
        compute_mid_bb,
        skip_run_bb,
        compute_right_bb,
        merge_loop_bb,
        check_j_bb,
        compare_bb,
        take_i_bb,
        take_j_bb,
        drain_left_bb,
        drain_right_bb,
        copy_back_bb,
        next_width_bb,
        free_bb,
        done_bb,
    );

    func.set_next_value_raw(next);
}

/// Emit the body of a trait-dispatch helper:
///
/// ```text
///   entry:
///     self      = ParamRef 0, Ptr
///     argN      = ParamRef N, <user_param_tys[N-1]>       (one per user arg)
///     vtbl_ptr  = FieldPtr self, {Trait}_TraitObj.vtable    (field 1)
///     vtbl      = Load vtbl_ptr, Ptr
///     fnp_ptr   = FieldPtr vtbl, {Trait}_VTable.method      (field method_idx)
///     fnp       = Load fnp_ptr, Ptr
///     data_ptr  = FieldPtr self, {Trait}_TraitObj.data      (field 0)
///     data      = Load data_ptr, Ptr
///     result?   = CallPtr fnp, [data, args...], ret_ty
///     Ret result | RetVoid
/// ```
///
/// Field indices 1=vtable, 0=data are invariant — see
/// `src/ir/lowering/traits.rs::register_trait_sigs`, which always
/// registers `{Trait}_TraitObj` with `data` at index 0 and `vtable` at
/// index 1. The VTable's method index is resolved by name-lookup on
/// the VTable struct's fields.
fn emit_trait_helper_body(
    func: &mut LirFunction,
    structs: &[StructDef],
    trait_name: &str,
    method: &str,
    user_param_tys: &[LirType],
    ret_ty: &LirType,
) {
    let trait_obj_sid = lookup_struct_id(structs, &format!("{trait_name}_TraitObj"))
        .expect("trait helper: TraitObj struct must exist");
    let vtable_sid = lookup_struct_id(structs, &format!("{trait_name}_VTable"))
        .expect("trait helper: VTable struct must exist");
    let method_idx: u32 = structs[vtable_sid.0 as usize]
        .fields
        .iter()
        .position(|(n, _)| n == method)
        .map(|i| i as u32)
        .expect("trait helper: method must be a VTable field");

    let mut next: u32 = 0;
    let entry_bb = func.add_block();

    // Param refs: self (0), then one per user param.
    let self_v = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::ParamRef {
        dst: self_v,
        index: 0,
        ty: LirType::Ptr,
    });
    let user_param_vs: Vec<ValueId> = user_param_tys
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            let v = alloc_value(&mut next);
            func.block_mut(entry_bb).insts.push(Inst::ParamRef {
                dst: v,
                index: (i + 1) as u32,
                ty: ty.clone(),
            });
            v
        })
        .collect();

    // vtbl_ptr = &self->vtable ; vtbl = *vtbl_ptr
    let vtbl_ptr = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::FieldPtr {
        dst: vtbl_ptr,
        base: self_v,
        struct_id: trait_obj_sid,
        field: 1, // TraitObj.vtable
    });
    let vtbl = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::Load {
        dst: vtbl,
        ptr: vtbl_ptr,
        ty: LirType::Ptr,
    });
    // fnp_ptr = &vtbl->method ; fnp = *fnp_ptr
    let fnp_ptr = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::FieldPtr {
        dst: fnp_ptr,
        base: vtbl,
        struct_id: vtable_sid,
        field: method_idx,
    });
    let fnp = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::Load {
        dst: fnp,
        ptr: fnp_ptr,
        ty: LirType::Ptr,
    });
    // data_ptr = &self->data ; data = *data_ptr
    let data_ptr = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::FieldPtr {
        dst: data_ptr,
        base: self_v,
        struct_id: trait_obj_sid,
        field: 0, // TraitObj.data
    });
    let data = alloc_value(&mut next);
    func.block_mut(entry_bb).insts.push(Inst::Load {
        dst: data,
        ptr: data_ptr,
        ty: LirType::Ptr,
    });

    // fnp(data, user_params...)
    let mut call_args = Vec::with_capacity(1 + user_param_vs.len());
    call_args.push(data);
    call_args.extend(user_param_vs);

    if matches!(ret_ty, LirType::Void) {
        func.block_mut(entry_bb).insts.push(Inst::CallPtr {
            dst: None,
            callee: fnp,
            args: call_args,
            ret_ty: ret_ty.clone(),
        });
        func.block_mut(entry_bb).terminator = Term::RetVoid;
    } else {
        let result = alloc_value(&mut next);
        func.block_mut(entry_bb).insts.push(Inst::CallPtr {
            dst: Some(result),
            callee: fnp,
            args: call_args,
            ret_ty: ret_ty.clone(),
        });
        func.block_mut(entry_bb).terminator = Term::Ret(result);
    }

    func.set_next_value_raw(next);
}

/// Emit the body of `take_i` or `take_j`:
/// - Memcpy aux[k] ← data[i or j]
/// - advance i or j by 1, always advance k by 1
/// - jump merge_loop with updated (i,j,k)
#[allow(clippy::too_many_arguments)]
fn emit_take_block(
    func: &mut LirFunction,
    next: &mut u32,
    take_bb: BlockId,
    merge_loop_bb: BlockId,
    arr: ValueId,
    aux: ValueId,
    array_sid: StructId,
    elsz: ValueId,
    elem_size: u32,
    take_i: bool,
) {
    let l = alloc_value(next);
    let w = alloc_value(next);
    let mid = alloc_value(next);
    let right = alloc_value(next);
    let i = alloc_value(next);
    let j = alloc_value(next);
    let k = alloc_value(next);
    func.block_mut(take_bb).params.push((l, LirType::I64));
    func.block_mut(take_bb).params.push((w, LirType::I64));
    func.block_mut(take_bb).params.push((mid, LirType::I64));
    func.block_mut(take_bb).params.push((right, LirType::I64));
    func.block_mut(take_bb).params.push((i, LirType::I64));
    func.block_mut(take_bb).params.push((j, LirType::I64));
    func.block_mut(take_bb).params.push((k, LirType::I64));

    // data = arr.data
    let datap = alloc_value(next);
    func.block_mut(take_bb).insts.push(Inst::FieldPtr {
        dst: datap,
        base: arr,
        struct_id: array_sid,
        field: 0,
    });
    let data = alloc_value(next);
    func.block_mut(take_bb).insts.push(Inst::Load {
        dst: data,
        ptr: datap,
        ty: LirType::Ptr,
    });
    // src ptr in arr
    let src_idx = if take_i { i } else { j };
    let srcp = alloc_value(next);
    func.block_mut(take_bb).insts.push(Inst::ElemPtr {
        dst: srcp,
        base: data,
        index: src_idx,
        elem_size,
    });
    // dst ptr in aux
    let dstp = alloc_value(next);
    func.block_mut(take_bb).insts.push(Inst::ElemPtr {
        dst: dstp,
        base: aux,
        index: k,
        elem_size,
    });
    func.block_mut(take_bb).insts.push(Inst::Memcpy {
        dst_ptr: dstp,
        src_ptr: srcp,
        size: elsz,
    });
    let one = alloc_value(next);
    func.block_mut(take_bb).insts.push(Inst::IConst {
        dst: one,
        ty: LirType::I64,
        value: 1,
    });
    let new_i;
    let new_j;
    if take_i {
        let ni = alloc_value(next);
        func.block_mut(take_bb).insts.push(Inst::Add {
            dst: ni,
            ty: LirType::I64,
            lhs: i,
            rhs: one,
            overflow: Overflow::Wrap,
        });
        new_i = ni;
        new_j = j;
    } else {
        let nj = alloc_value(next);
        func.block_mut(take_bb).insts.push(Inst::Add {
            dst: nj,
            ty: LirType::I64,
            lhs: j,
            rhs: one,
            overflow: Overflow::Wrap,
        });
        new_i = i;
        new_j = nj;
    }
    let new_k = alloc_value(next);
    func.block_mut(take_bb).insts.push(Inst::Add {
        dst: new_k,
        ty: LirType::I64,
        lhs: k,
        rhs: one,
        overflow: Overflow::Wrap,
    });
    func.block_mut(take_bb).terminator =
        Term::Jump(merge_loop_bb, vec![l, w, mid, right, new_i, new_j, new_k]);
}

/// Emit the body of drain_left / drain_right (iterative tail copy).
/// `drain_i = true` means copy remaining from left side (index i < mid);
/// `drain_i = false` means copy remaining from right side (index j < right).
///
/// Uses the already-populated block params (caller set them).
#[allow(clippy::too_many_arguments)]
fn emit_drain_block(
    func: &mut LirFunction,
    next: &mut u32,
    drain_bb: BlockId,
    copy_back_bb: BlockId,
    arr: ValueId,
    aux: ValueId,
    array_sid: StructId,
    elsz: ValueId,
    elem_size: u32,
    drain_i: bool,
    l: ValueId,
    w: ValueId,
    mid: ValueId,
    right: ValueId,
    i: ValueId,
    j: ValueId,
    k: ValueId,
) {
    // Loop cond: (drain_i? i < mid : j < right)
    let cur = if drain_i { i } else { j };
    let lim = if drain_i { mid } else { right };
    let cond = alloc_value(next);
    func.block_mut(drain_bb).insts.push(Inst::Cmp {
        dst: cond,
        op: CmpOp::Lt,
        lhs: cur,
        rhs: lim,
    });
    // Branch: if cond, do the copy inline and jump back; else copy_back.
    // To keep the body a straight-line extension of drain_bb, we insert a
    // nested helper block for the body and jump back to drain_bb itself
    // with updated state.
    let body_bb = func.add_block();
    func.block_mut(drain_bb).terminator = Term::Branch {
        cond,
        then_block: body_bb,
        then_args: vec![l, w, mid, right, i, j, k],
        else_block: copy_back_bb,
        else_args: vec![l, w, right],
    };

    // body_bb params mirror drain_bb's.
    let l2 = alloc_value(next);
    let w2 = alloc_value(next);
    let mid2 = alloc_value(next);
    let right2 = alloc_value(next);
    let i2 = alloc_value(next);
    let j2 = alloc_value(next);
    let k2 = alloc_value(next);
    func.block_mut(body_bb).params.push((l2, LirType::I64));
    func.block_mut(body_bb).params.push((w2, LirType::I64));
    func.block_mut(body_bb).params.push((mid2, LirType::I64));
    func.block_mut(body_bb).params.push((right2, LirType::I64));
    func.block_mut(body_bb).params.push((i2, LirType::I64));
    func.block_mut(body_bb).params.push((j2, LirType::I64));
    func.block_mut(body_bb).params.push((k2, LirType::I64));

    // Memcpy the current element from source (arr.data[cur]) to aux[k].
    let datap = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::FieldPtr {
        dst: datap,
        base: arr,
        struct_id: array_sid,
        field: 0,
    });
    let data = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::Load {
        dst: data,
        ptr: datap,
        ty: LirType::Ptr,
    });
    let src_idx = if drain_i { i2 } else { j2 };
    let srcp = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::ElemPtr {
        dst: srcp,
        base: data,
        index: src_idx,
        elem_size,
    });
    let dstp = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::ElemPtr {
        dst: dstp,
        base: aux,
        index: k2,
        elem_size,
    });
    func.block_mut(body_bb).insts.push(Inst::Memcpy {
        dst_ptr: dstp,
        src_ptr: srcp,
        size: elsz,
    });
    let one = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::IConst {
        dst: one,
        ty: LirType::I64,
        value: 1,
    });
    let new_i;
    let new_j;
    if drain_i {
        let ni = alloc_value(next);
        func.block_mut(body_bb).insts.push(Inst::Add {
            dst: ni,
            ty: LirType::I64,
            lhs: i2,
            rhs: one,
            overflow: Overflow::Wrap,
        });
        new_i = ni;
        new_j = j2;
    } else {
        let nj = alloc_value(next);
        func.block_mut(body_bb).insts.push(Inst::Add {
            dst: nj,
            ty: LirType::I64,
            lhs: j2,
            rhs: one,
            overflow: Overflow::Wrap,
        });
        new_i = i2;
        new_j = nj;
    }
    let new_k = alloc_value(next);
    func.block_mut(body_bb).insts.push(Inst::Add {
        dst: new_k,
        ty: LirType::I64,
        lhs: k2,
        rhs: one,
        overflow: Overflow::Wrap,
    });
    func.block_mut(body_bb).terminator =
        Term::Jump(drain_bb, vec![l2, w2, mid2, right2, new_i, new_j, new_k]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lir::{LirFunction, LirType};

    #[test]
    fn pool_starts_empty() {
        let pool = SynthPool::new(0);
        assert_eq!(pool.len(), 0);
        assert!(pool.finish().is_empty());
    }

    #[test]
    fn closure_call_sig_strips_env_for_closure_n() {
        let mut module = LirModule::new();
        module.functions.push(LirFunction::new(
            "__Closure_3__call".into(),
            vec![LirType::Ptr, LirType::I64, LirType::I64],
            LirType::I64,
        ));
        let sig = closure_call_sig_of(&module, "__Closure_3__call").unwrap();
        assert_eq!(sig.param_tys, vec![LirType::I64, LirType::I64]);
        assert_eq!(sig.ret_ty, LirType::I64);
    }

    #[test]
    fn closure_call_sig_keeps_all_for_funcref() {
        let mut module = LirModule::new();
        module.functions.push(LirFunction::new(
            "user_cmp".into(),
            vec![LirType::I64, LirType::I64],
            LirType::I64,
        ));
        let sig = closure_call_sig_of(&module, "user_cmp").unwrap();
        assert_eq!(sig.param_tys, vec![LirType::I64, LirType::I64]);
    }

    #[test]
    #[should_panic(expected = "reserved prefix")]
    fn assert_no_synth_prefix_catches_collisions() {
        let mut module = LirModule::new();
        module
            .functions
            .push(LirFunction::new("__gg_synth_bogus".into(), vec![], LirType::Void));
        assert_no_synth_prefix(&module);
    }

    /// End-to-end test of the sort-family synthesis pipeline:
    ///
    /// 1. Construct a module with one user function containing a single
    ///    `HofExpand { SortBy, element_ty=I64 }` instruction.
    /// 2. Run it through `BirModule::from_lir` (which runs synthesis then
    ///    validates).
    /// 3. Assert:
    ///    - The caller's `HofExpand` was rewritten to a `Call` targeting a
    ///      `__gg_synth_*` function.
    ///    - No `HofExpand` remains anywhere in the module.
    ///    - **Opaque-closure invariant**: in the synthesized body, the
    ///      closure parameter is never the base of a `FieldPtr`. The body
    ///      must only use the closure through `CallClosure`.
    ///    - BirModule::from_lir returns Ok (validator passes).
    #[test]
    fn sort_by_synthesis_end_to_end_and_opaque_closure_invariant() {
        use crate::bir::BirModule;
        use crate::ir::abi::AbiKind;
        use crate::lir::{
            Block, BlockId, ClosureDispatchKind, HofOp, Inst, LirModule, StructDef, Term,
            ValueId,
        };

        // Minimal module: GorgetArray struct (synth body FieldPtrs on it) +
        // one user function `caller(arr: Ptr, cl: Ptr)` with a SortBy HofExpand.
        let mut module = LirModule::new();
        // Push the builtin GorgetArray shape so lookup_struct_id finds it.
        let gorget_array_sid = module.add_struct(StructDef {
            name: "GorgetArray".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("cap".into(), LirType::I64),
                ("len".into(), LirType::I64),
                ("elem_size".into(), LirType::I64),
            ],
            enum_kind: crate::lir::EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(64),
            computed_c_align: Some(8), elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        let _ = gorget_array_sid;

        // Build caller function. Param layout: (arr: Ptr, cl: Ptr).
        let mut caller =
            LirFunction::new("caller".into(), vec![LirType::Ptr, LirType::Ptr], LirType::Void);
        let arr = caller.next_value();
        let cl = caller.next_value();
        let bb0 = caller.add_block();
        caller.block_mut(bb0).insts.push(Inst::ParamRef {
            dst: arr,
            index: 0,
            ty: LirType::Ptr,
        });
        caller.block_mut(bb0).insts.push(Inst::ParamRef {
            dst: cl,
            index: 1,
            ty: LirType::Ptr,
        });
        caller.block_mut(bb0).insts.push(Inst::HofExpand {
            coll: arr,
            hof_op: HofOp::SortBy,
            element_ty: LirType::I64,
            value_ty: None,
            closure: cl,
            closure_kind: ClosureDispatchKind::EscapedClosure,
            closure_ret_ty: LirType::I64,
            closure_arg_abis: vec![AbiKind::Scalar, AbiKind::Scalar],
            dst: None,
            init: None,
        });
        caller.block_mut(bb0).terminator = Term::RetVoid;
        module.add_function(caller);

        // compute_module_value_types before BIR (matches real pipeline).
        crate::lir::types::compute_module_value_types(&mut module);

        // Lower to BIR. This runs the synth pool and validates primitives-only.
        let bir = BirModule::from_lir(module).expect("BIR lowering should succeed");
        let module = bir.as_lir();

        // (a) Caller has been rewritten: no HofExpand, one Call to __gg_synth_*.
        let caller = module
            .functions
            .iter()
            .find(|f| f.name == "caller")
            .expect("caller function present");
        let caller_has_hof = caller
            .blocks
            .iter()
            .flat_map(|b: &Block| b.insts.iter())
            .any(|i| matches!(i, Inst::HofExpand { .. }));
        assert!(
            !caller_has_hof,
            "caller still contains HofExpand after synthesis",
        );
        let called_synth_fn: Option<&str> = caller
            .blocks
            .iter()
            .flat_map(|b| b.insts.iter())
            .find_map(|i| match i {
                Inst::Call { func: fid, .. } => {
                    let name = &module.functions[fid.0 as usize].name;
                    name.starts_with(SYNTH_PREFIX).then(|| name.as_str())
                }
                _ => None,
            });
        assert!(
            called_synth_fn.is_some(),
            "caller must contain a Call to a __gg_synth_* function",
        );

        // (b) No HofExpand remains anywhere in the module (synth or user).
        for f in &module.functions {
            for b in &f.blocks {
                for inst in &b.insts {
                    assert!(
                        !matches!(inst, Inst::HofExpand { .. }),
                        "unlowered HofExpand in fn `{}` bb{}",
                        f.name,
                        b.id.0,
                    );
                }
            }
        }

        // (c) Opaque-closure invariant: find the synthesized fn and walk its
        // body. Locate the ValueId for the closure param (ParamRef index=1);
        // assert no FieldPtr uses it as base.
        let synth_fn_name = called_synth_fn.unwrap().to_string();
        let synth = module
            .functions
            .iter()
            .find(|f| f.name == synth_fn_name)
            .expect("synth fn exists in module");
        let closure_param_vid: Option<ValueId> = synth
            .blocks
            .iter()
            .flat_map(|b| b.insts.iter())
            .find_map(|i| match i {
                Inst::ParamRef { dst, index: 1, .. } => Some(*dst),
                _ => None,
            });
        let closure_vid =
            closure_param_vid.expect("synth fn must materialize its closure param");
        for b in &synth.blocks {
            for inst in &b.insts {
                if let Inst::FieldPtr { base, .. } = inst {
                    assert_ne!(
                        *base, closure_vid,
                        "opaque-closure invariant violated: FieldPtr on closure \
                         param in synth fn `{}`",
                        synth.name,
                    );
                }
            }
        }

        // Suppress unused warning on BlockId import when the BlockId type
        // only shows up inside the flat_map closures.
        let _ = std::marker::PhantomData::<BlockId>;
    }

    /// End-to-end test of trait-helper synthesis:
    ///
    /// 1. Build a module with TraitObj + VTable structs and a caller
    ///    function carrying one `Inst::TraitCall` for `Greeter::greet`.
    /// 2. Run it through `BirModule::from_lir`.
    /// 3. Assert:
    ///    - No TraitCall survives in any function.
    ///    - The caller now contains a `Call` to `__gg_synth_trait_Greeter_greet`.
    ///    - The synth fn body has the expected primitive shape: ParamRef
    ///      self + three (FieldPtr, Load) pairs on TraitObj.vtable /
    ///      VTable.greet / TraitObj.data + CallPtr + Ret.
    ///    - Dedup: a second TraitCall for the same (trait, method, sig)
    ///      reuses the same FuncId.
    #[test]
    fn trait_call_synthesis_end_to_end() {
        use crate::bir::BirModule;
        use crate::ir::abi::AbiKind;
        use crate::lir::{Inst, LirModule, StructDef, Term};

        let mut module = LirModule::new();

        // Greeter_TraitObj { data: Ptr, vtable: Ptr } — field 0=data, field 1=vtable.
        let trait_obj_sid = module.add_struct(StructDef {
            name: "Greeter_TraitObj".into(),
            fields: vec![
                ("data".into(), LirType::Ptr),
                ("vtable".into(), LirType::Ptr),
            ],
            enum_kind: crate::lir::EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(16),
            computed_c_align: Some(8), elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        // Greeter_VTable { greet: Ptr } — single method slot at index 0.
        let vtable_sid = module.add_struct(StructDef {
            name: "Greeter_VTable".into(),
            fields: vec![("greet".into(), LirType::Ptr)],
            enum_kind: crate::lir::EnumKind::NotEnum,
            is_union_layout: false,
            computed_c_size: Some(8),
            computed_c_align: Some(8), elem_drop_fn: None, elem_clone_fn: None, materialize_fn: None, c_runtime_alias: None, box_inner_type: None, is_trait_box: false, expects_drop_fn: false,
        });
        let _ = vtable_sid;

        // caller(obj: Ptr) { TraitCall dst, obj, "Greeter"::"greet"(), I64; Ret dst }
        let mut caller =
            LirFunction::new("caller".into(), vec![LirType::Ptr], LirType::I64);
        let obj = caller.next_value();
        let result = caller.next_value();
        let bb0 = caller.add_block();
        caller.block_mut(bb0).insts.push(Inst::ParamRef {
            dst: obj,
            index: 0,
            ty: LirType::Ptr,
        });
        caller.block_mut(bb0).insts.push(Inst::TraitCall {
            dst: Some(result),
            object: obj,
            trait_obj_struct: trait_obj_sid,
            method_idx: 0, // Greeter_VTable.greet at index 0
            args: vec![],
            arg_abis: vec![AbiKind::Ptr],
            param_tys: vec![],
            ret_ty: LirType::I64,
        });
        // Second TraitCall (identical shape) for dedup check.
        let result2 = caller.next_value();
        caller.block_mut(bb0).insts.push(Inst::TraitCall {
            dst: Some(result2),
            object: obj,
            trait_obj_struct: trait_obj_sid,
            method_idx: 0,
            args: vec![],
            arg_abis: vec![AbiKind::Ptr],
            param_tys: vec![],
            ret_ty: LirType::I64,
        });
        caller.block_mut(bb0).terminator = Term::Ret(result);
        module.add_function(caller);

        crate::lir::types::compute_module_value_types(&mut module);
        let bir = BirModule::from_lir(module).expect("BIR lowering should succeed");
        let module = bir.as_lir();

        // No TraitCall anywhere.
        for f in &module.functions {
            for b in &f.blocks {
                for inst in &b.insts {
                    assert!(
                        !matches!(inst, Inst::TraitCall { .. }),
                        "unlowered TraitCall in fn `{}` bb{}",
                        f.name,
                        b.id.0,
                    );
                }
            }
        }

        // Caller has two Calls to the same synth fn.
        let caller = module
            .functions
            .iter()
            .find(|f| f.name == "caller")
            .expect("caller present");
        let synth_fids: Vec<FuncId> = caller
            .blocks
            .iter()
            .flat_map(|b| b.insts.iter())
            .filter_map(|i| match i {
                Inst::Call { func, .. } => Some(*func),
                _ => None,
            })
            .collect();
        assert_eq!(synth_fids.len(), 2, "caller should have two synth Calls");
        assert_eq!(
            synth_fids[0], synth_fids[1],
            "two TraitCalls with identical (trait, method, sig) must dedup to the same helper",
        );
        let synth_fn = &module.functions[synth_fids[0].0 as usize];
        assert_eq!(synth_fn.name, "__gg_synth_trait_Greeter_greet");
        assert_eq!(synth_fn.params, vec![LirType::Ptr]);
        assert_eq!(synth_fn.return_type, LirType::I64);

        // Body shape: ParamRef self (index 0) + 3×(FieldPtr, Load) +
        // CallPtr + Term::Ret.
        let insts: Vec<&Inst> = synth_fn.blocks[0].insts.iter().collect();
        assert!(
            matches!(insts[0], Inst::ParamRef { index: 0, .. }),
            "first inst must be ParamRef self at index 0, got {:?}",
            insts[0],
        );
        // Count each primitive kind.
        let n_fieldptr = insts.iter().filter(|i| matches!(i, Inst::FieldPtr { .. })).count();
        let n_load = insts.iter().filter(|i| matches!(i, Inst::Load { .. })).count();
        let n_callptr = insts.iter().filter(|i| matches!(i, Inst::CallPtr { .. })).count();
        assert_eq!(n_fieldptr, 3, "body should have 3 FieldPtrs (vtable, method, data)");
        assert_eq!(n_load, 3, "body should have 3 Loads (vtable, fnp, data)");
        assert_eq!(n_callptr, 1, "body should have exactly one CallPtr");
        assert!(
            matches!(synth_fn.blocks[0].terminator, Term::Ret(_)),
            "body terminator must be Ret (I64 return)",
        );
    }
}
