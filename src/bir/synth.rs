//! BIR module-level synthesis.
//!
//! Synthesizes new `LirFunction` definitions from canonical high-level ops
//! (today: sort_by family; future: any op that needs a dedicated specialized
//! body the backends shouldn't reinvent per-type).
//!
//! See `docs/internals/bir-module-synthesis-plan.md` for the design.
//!
//! ## Current state (Commit 1 — infrastructure)
//!
//! `SynthPool` is constructed in `lower_lir_to_bir` but no caller emits
//! synthesized functions yet. `get_or_emit_sort_impl` panics; the first
//! caller lands in Commit 2 alongside the SortBy expansion.
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
//! A post-synthesis test (`assert_opaque_closures`) walks every emitted
//! body and asserts no `FieldPtr { base: closure_param, .. }` appears.
//! Kept as a debug guardrail.
//!
//! ## Naming
//!
//! All synthesized functions use the reserved prefix `__gg_synth_`.
//! Monomorphization never produces names starting with this prefix;
//! `lower_lir_to_bir` asserts on entry that the invariant holds.

use crate::lir::{FuncId, HofOp, LirFunction, LirModule, LirType};
use rustc_hash::FxHashMap;

/// Reserved prefix for all synthesized function names.
pub const SYNTH_PREFIX: &str = "__gg_synth_";

/// De-duplication key for synthesized functions.
///
/// Two call sites with the same `SynthKey` share one synthesized body.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct SynthKey {
    /// Which HOF this specializes.
    pub op: HofOp,
    /// Mangled element type (via `LirType`'s stable Debug format today;
    /// swap to `display::mangle_lir_type` when that helper exists).
    pub element_ty: String,
    /// Name of the closure's `__call` function (e.g. `__Closure_7__call`),
    /// or a FuncRef target name for bare function references.
    pub closure_call: String,
}

/// Closure signature snapshot — params (env stripped for `__Closure_N__call`)
/// plus return type. Synthesis needs both to pick per-arg ABIs inside
/// synthesized `CallClosure`s.
#[derive(Debug, Clone)]
#[allow(dead_code)] // consumed in Commit 2 when body emitters wire up
pub(crate) struct ClosureSig {
    pub param_tys: Vec<LirType>,
    pub ret_ty: LirType,
}

/// Look up the call signature of a closure's `__call` function (or a bare
/// FuncRef target) from the module's current function list.
///
/// Mirrors `LoweringContext::closure_call_sigs` but re-derived at BIR time
/// from `LirModule.functions` — no new module-level state. The `__Closure_N`
/// env param is skipped to align with what the emitter treats as "user
/// params."
#[allow(dead_code)] // consumed in Commit 2 when body emitters wire up
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

/// Pool of synthesized functions for one BIR lowering pass.
///
/// Owned by `lower_lir_to_bir` for the duration of the pass. Call
/// `get_or_emit_*` during block walks; at the end, call `finish()` and
/// splice the returned `Vec<LirFunction>` onto `module.functions`.
pub(crate) struct SynthPool {
    /// (key → FuncId) — once assigned, reused for every call site with
    /// the same key. The `FuncId` is computed from
    /// `base_func_count + new_fns.len()` at insertion time, so it's
    /// stable across the splice.
    cache: FxHashMap<SynthKey, FuncId>,
    /// New functions to append. Fresh `LirFunction` objects with SSA-form
    /// bodies — no SlotStore/SlotLoad of values that cross blocks.
    new_fns: Vec<LirFunction>,
    /// `FuncId.0` baseline — equals `module.functions.len()` at pass entry.
    /// A synthesized fn's FuncId is `base_func_count + new_fns.len()` at
    /// the moment it's pushed, matching the slot it'll occupy after
    /// splicing.
    #[allow(dead_code)] // consumed in Commit 2 when body emitters wire up
    base_func_count: u32,
}

impl SynthPool {
    /// Create an empty pool. `base_func_count` should be
    /// `module.functions.len()` at pass entry.
    pub fn new(base_func_count: u32) -> Self {
        Self {
            cache: FxHashMap::default(),
            new_fns: Vec::new(),
            base_func_count,
        }
    }

    /// Number of synthesized functions queued so far. Useful for tests and
    /// for verifying dedup is working.
    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.new_fns.len()
    }

    /// Request (and if necessary emit) the synth fn for `(op, element_ty,
    /// closure_call)`. Returns the `FuncId` of the synthesized function
    /// post-splice.
    ///
    /// Commit 1 stub: panics unconditionally. Commit 2 adds the SortBy
    /// body; later commits add SortByKey.
    #[allow(dead_code, unused_variables)]
    pub fn get_or_emit_sort_impl(
        &mut self,
        module: &LirModule,
        op: HofOp,
        element_ty: &LirType,
        closure_call: &str,
    ) -> FuncId {
        let key = SynthKey {
            op,
            element_ty: format!("{element_ty:?}"),
            closure_call: closure_call.to_string(),
        };
        if let Some(&fid) = self.cache.get(&key) {
            return fid;
        }
        // Placeholder — Commit 2 plugs in `emit_sort_impl_body` here.
        panic!(
            "SynthPool::get_or_emit_sort_impl: body emitter not yet wired \
             (op={op:?}, element_ty={element_ty:?}, closure_call={closure_call}). \
             Landing in Commit 2 of the BIR module-synthesis plan."
        );
    }

    /// Consume the pool and return the synthesized functions for the
    /// caller to splice into `module.functions`.
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
        // Env param dropped; two user params remain.
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
        // No env to strip — both params kept.
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
}
