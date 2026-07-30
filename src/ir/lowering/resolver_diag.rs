//! Place-resolver fall-through diagnostics (`--resolvers[=hist|sites|hist-tsv=PATH]`).
//!
//! ⚠ **WORKLIST GENERATOR, NEVER A CORRECTNESS GATE (Core #13).**
//! `Some(wrong_root)` counts as resolved. Only instrument C (build-and-run cell
//! matrix, later round) adjudicates landing. Rising arm counts / falling
//! histograms measure dispatch totality, not write-through soundness.

use rustc_hash::FxHashMap;

use crate::parser::ast::Expr;
use crate::span::Span;

/// Which place resolver declined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolverId {
    RootLocal,
    TryPlace,
    FieldPlace,
    TuplePlace,
    PtrField,
}

impl ResolverId {
    pub fn as_str(self) -> &'static str {
        match self {
            ResolverId::RootLocal => "root_local",
            ResolverId::TryPlace => "try_place",
            ResolverId::FieldPlace => "field_place",
            ResolverId::TuplePlace => "tuple_place",
            ResolverId::PtrField => "ptr_field",
        }
    }
}

/// Why a resolver returned None / Unresolved / ReadGuardSkip.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MissReason {
    NoArm,
    LookupMiss,
    ReadGuard,
    PostPtrFilter,
    NonPlaceOperand,
    MethodNotDescend,
    Unresolved,
}

impl MissReason {
    pub fn as_str(self) -> &'static str {
        match self {
            MissReason::NoArm => "NoArm",
            MissReason::LookupMiss => "LookupMiss",
            MissReason::ReadGuard => "ReadGuard",
            MissReason::PostPtrFilter => "PostPtrFilter",
            MissReason::NonPlaceOperand => "NonPlaceOperand",
            MissReason::MethodNotDescend => "MethodNotDescend",
            MissReason::Unresolved => "Unresolved",
        }
    }
}

/// One fall-through observation (module-wide, transfered off the context).
#[derive(Debug, Clone)]
pub struct ResolverMissRecord {
    pub resolver: String,
    pub shape: String,
    pub reason: String,
    /// Byte offset into the primary source (for `--resolvers=sites` location).
    pub span_start: usize,
}

/// Compile-time bookkeeping for `--resolvers`. Zero-cost when disabled:
/// every public entry checks `enabled` before string work.
#[derive(Debug, Default)]
pub struct ResolverDiag {
    pub enabled: bool,
    pub sites: bool,
    /// Aggregated counts keyed by (resolver, shape, reason).
    pub hist: FxHashMap<(String, String, String), u64>,
    /// Optional per-site log (only when `sites`).
    pub site_log: Vec<ResolverMissRecord>,
}

impl ResolverDiag {
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// Record a fall-through. No-op when the instrument is off.
    pub fn bump(
        &mut self,
        id: ResolverId,
        shape: String,
        reason: MissReason,
        span: Option<Span>,
    ) {
        if !self.enabled {
            return;
        }
        let resolver = id.as_str().to_string();
        let reason_s = reason.as_str().to_string();
        *self
            .hist
            .entry((resolver.clone(), shape.clone(), reason_s.clone()))
            .or_insert(0) += 1;
        if self.sites {
            self.site_log.push(ResolverMissRecord {
                resolver,
                shape,
                reason: reason_s,
                span_start: span.map(|s| s.start).unwrap_or(0),
            });
        }
    }

    /// Compact hist map for reporting without exploding into N identical rows
    /// when sites mode is off. Prefer this for hist / hist-tsv.
    pub fn hist_entries(&self) -> Vec<(String, String, String, u64)> {
        let mut entries: Vec<_> = self
            .hist
            .iter()
            .map(|((r, s, reason), c)| (r.clone(), s.clone(), reason.clone(), *c))
            .collect();
        // Rank by count desc, then resolver, shape for stability.
        entries.sort_by(|a, b| {
            b.3.cmp(&a.3)
                .then_with(|| a.0.cmp(&b.0))
                .then_with(|| a.1.cmp(&b.1))
                .then_with(|| a.2.cmp(&b.2))
        });
        entries
    }
}

/// Depth-capped AST discriminant chain for worklist keys.
/// e.g. `FieldAccess(MethodCall:get(Identifier))` for `v.get(i).unwrap().fd`.
/// Method names are included; field names and idents are not (cardinality).
pub fn expr_shape_chain(expr: &Expr, max_depth: usize) -> String {
    expr_shape_chain_inner(expr, max_depth)
}

fn expr_shape_chain_inner(expr: &Expr, depth: usize) -> String {
    if depth == 0 {
        return "…".to_string();
    }
    match expr {
        Expr::Identifier(_) => "Identifier".to_string(),
        Expr::SelfExpr => "SelfExpr".to_string(),
        Expr::FieldAccess { object, .. } => {
            format!(
                "FieldAccess({})",
                expr_shape_chain_inner(&object.node, depth - 1)
            )
        }
        Expr::TupleFieldAccess { object, .. } => {
            format!(
                "TupleFieldAccess({})",
                expr_shape_chain_inner(&object.node, depth - 1)
            )
        }
        Expr::Index { object, .. } => {
            format!(
                "Index({})",
                expr_shape_chain_inner(&object.node, depth - 1)
            )
        }
        Expr::Deref { expr: inner } => {
            format!(
                "Deref({})",
                expr_shape_chain_inner(&inner.node, depth - 1)
            )
        }
        Expr::MethodCall {
            receiver, method, ..
        } => {
            format!(
                "MethodCall:{}({})",
                method.node,
                expr_shape_chain_inner(&receiver.node, depth - 1)
            )
        }
        Expr::Call { callee, .. } => {
            format!(
                "Call({})",
                expr_shape_chain_inner(&callee.node, depth - 1)
            )
        }
        Expr::Path { .. } => "Path".to_string(),
        Expr::IntLiteral(_) => "IntLiteral".to_string(),
        Expr::FloatLiteral(_) => "FloatLiteral".to_string(),
        Expr::BoolLiteral(_) => "BoolLiteral".to_string(),
        Expr::StringLiteral(_, _) => "StringLiteral".to_string(),
        Expr::NoneLiteral => "NoneLiteral".to_string(),
        Expr::UnaryOp { .. } => "UnaryOp".to_string(),
        Expr::BinaryOp { .. } => "BinaryOp".to_string(),
        Expr::Range { .. } => "Range".to_string(),
        Expr::OptionalChain { .. } => "OptionalChain".to_string(),
        other => {
            // Fallback: debug-variant-ish via type_name of match arm coverage.
            // Keep stable short labels for remaining heads without pulling Debug.
            let _ = other;
            "Other".to_string()
        }
    }
}

/// Default max depth for shape chains (covers deep get/unwrap field chains).
pub const SHAPE_MAX_DEPTH: usize = 6;

/// Emit the ranked histogram / sites report to stderr (and optional TSV).
///
/// Banner always restates THE TRAP so no consumer mistakes emptiness for soundness.
pub fn emit_resolver_report(
    hist: &[(String, String, String, u64)],
    sites: &[ResolverMissRecord],
    show_hist: bool,
    show_sites: bool,
    hist_tsv: Option<&std::path::Path>,
    locate: &dyn Fn(usize) -> (String, usize, usize),
) {
    if show_hist {
        eprintln!(
            "=== Resolver Fall-through Report (worklist only; NOT a correctness gate) ==="
        );
        eprintln!(
            "⚠ A and B are WORKLIST GENERATORS, never correctness gates (Core #13)."
        );
        eprintln!(
            "  Some(wrong_root) counts as resolved. Only instrument C adjudicates landing."
        );
        eprintln!("{:<18} {:>8}  {:<14}  shape", "resolver", "count", "reason");
        let top_n = std::env::var("GG_RESOLVER_HIST_TOP")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(40usize);
        let mut total = 0u64;
        let mut shapes = 0usize;
        for (i, (resolver, shape, reason, count)) in hist.iter().enumerate() {
            total += count;
            shapes += 1;
            if i < top_n {
                eprintln!("{resolver:<18} {count:>8}  {reason:<14}  {shape}");
            }
        }
        let resolvers_touched: std::collections::BTreeSet<_> =
            hist.iter().map(|(r, _, _, _)| r.as_str()).collect();
        eprintln!(
            "[resolver-hist] total_misses={total} resolvers_touched={} shapes={shapes}",
            resolvers_touched.len()
        );
    }

    if show_sites {
        eprintln!("=== Resolver Fall-through Sites (worklist only) ===");
        for rec in sites {
            let (file, line, col) = locate(rec.span_start);
            eprintln!(
                "{file}:{line}:{col}  {}  {}  {}",
                rec.resolver, rec.shape, rec.reason
            );
        }
    }

    if let Some(path) = hist_tsv {
        use std::io::Write as _;
        let mut out = String::from("resolver\tshape\treason\tcount\n");
        for (resolver, shape, reason, count) in hist {
            out.push_str(&format!("{resolver}\t{shape}\t{reason}\t{count}\n"));
        }
        if let Err(e) = std::fs::File::create(path).and_then(|mut f| f.write_all(out.as_bytes())) {
            eprintln!(
                "warning: failed to write --resolvers=hist-tsv={}: {e}",
                path.display()
            );
        }
    }
}
