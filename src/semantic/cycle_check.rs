//! Detect unbounded recursive types.
//!
//! A user-defined struct/enum cycles "by value" when every loop through its
//! field/variant graph stores its members inline (no heap indirection). Such
//! a type would have infinite size — codegen recurses unboundedly trying to
//! lay it out and stack-overflows. Catch it at semantic time and emit a clear
//! error pointing at the cycle.
//!
//! A cycle is *broken* when at least one edge passes through a heap-indirect
//! wrapper:
//!
//! - `Box[T]` — single heap pointer.
//! - `Vector[T]`, `Deque[T]`, `Dict[K, V]`, `HashMap[K, V]`, `Set[T]`,
//!   `HashSet[T]` — heap-allocated handle structs.
//! - `Rc[T]`, `Arc[T]`, `Shared[T]`, `Weak[T]` — refcounted heap pointers.
//! - `Channel[T]`, `Mutex[T]`, `RWLock[T]`, `Future[T]`, `Task[T]`,
//!   `Guard[T]`, `ReadGuard[T]`, `WriteGuard[T]`, `TaskGroup`, `Heap`,
//!   `Thread` — opaque pointer handles.
//! - `String` (`GorgetString`) — heap-allocated UTF-8 buffer.
//!
//! `Option[T]` and `Result[T, E]` *do not* break a cycle — their payloads are
//! stored inline as variant data, so `Option[Spanned]` is the same size as
//! `Spanned` plus a tag. The fix in those cases is `Option[Box[Spanned]]`
//! (or one of the other indirections).
//!
//! ## On the heap-indirect list
//!
//! The list lives as a `const &[&str]` here rather than as a typed flag on
//! each type's metadata. The cleanest "no name matching" home would be a
//! `heap_indirect: bool` on `BuiltinTypeProtocol` (and the `TypeMetadata`
//! that lives with each TypeDef in the IR layer). Today `TypeMetadata` is
//! built during IR lowering — too late for this semantic-time check —
//! and the protocol-level flag would still leave `Box` (which is declared
//! in `lib/std/collections.gg` as a bare `struct Box[T]: pass`, not a
//! `BuiltinTypeProtocol` entry) outside the typed channel. Lifting the
//! flag up to the semantic layer is a worthwhile follow-up; until then,
//! this list is the single source of truth and is referenced from one
//! call site (`is_heap_indirect_wrapper`).

use crate::parser::ast::Item;
use crate::semantic::errors::{SemanticError, SemanticErrorKind};
use crate::semantic::ids::{DefId, TypeId};
use crate::semantic::scope::{DefKind, ScopeTable};
use crate::semantic::types::{ResolvedType, TypeTable};
use crate::span::{Span, Spanned};
use rustc_hash::FxHashSet;

const HEAP_INDIRECT_TYPES: &[&str] = &[
    "Box",
    "Vector",
    "Deque",
    "Dict",
    "HashMap",
    "Set",
    "HashSet",
    "Rc",
    "Arc",
    "Shared",
    "Weak",
    "Channel",
    "Mutex",
    "RWLock",
    "Guard",
    "ReadGuard",
    "WriteGuard",
    "Future",
    "Task",
    "TaskGroup",
    "GorgetString",
    "Heap",
    "Thread",
    "AtomicInt",
    "AtomicBool",
    "Barrier",
    "WaitGroup",
    "Semaphore",
    "OnceFlag",
    "Arena",
    "TrackingAllocator",
    "PoolAllocator",
    "TlsfAllocator",
    "FixedBufferAllocator",
    "FallbackAllocator",
];

fn is_heap_indirect_wrapper(name: &str) -> bool {
    HEAP_INDIRECT_TYPES.contains(&name)
}

/// Walk all user-defined struct/enum/newtype definitions reachable from the
/// module and report unbounded type cycles.
pub fn check_recursive_type_cycles(
    module: &crate::parser::ast::Module,
    scopes: &ScopeTable,
    types: &TypeTable,
    errors: &mut Vec<SemanticError>,
) {
    let user_defs = collect_user_type_defs(&module.items, scopes);

    // Each type whose def_id appears in any reported cycle — we want one
    // error per cycle, not one per node, so suppress further reports for
    // types that already participated in a reported cycle.
    let mut already_reported: FxHashSet<DefId> = FxHashSet::default();

    for (def_id, name, span) in &user_defs {
        if already_reported.contains(def_id) {
            continue;
        }
        let mut on_stack: Vec<(DefId, String)> = Vec::new();
        let mut visited: FxHashSet<DefId> = FxHashSet::default();
        if let Some(cycle) = find_unbounded_cycle(*def_id, &mut on_stack, &mut visited, scopes, types) {
            // Mark every type in the reported cycle so we don't surface a
            // second redundant error for the same loop seen from a
            // different entry point.
            for cdef in &cycle {
                if let Some(def_id) = scopes.lookup(cdef) {
                    already_reported.insert(def_id);
                }
            }
            errors.push(SemanticError {
                kind: SemanticErrorKind::RecursiveTypeNeedsBox {
                    name: name.clone(),
                    cycle,
                },
                span: *span,
            });
        }
    }
}

fn collect_user_type_defs(
    items: &[Spanned<Item>],
    scopes: &ScopeTable,
) -> Vec<(DefId, String, Span)> {
    let mut out = Vec::new();
    for item in items {
        match &item.node {
            Item::Struct(s) => {
                if let Some(def_id) = scopes.lookup(&s.name.node) {
                    out.push((def_id, s.name.node.clone(), s.name.span));
                }
            }
            Item::Enum(e) => {
                if let Some(def_id) = scopes.lookup(&e.name.node) {
                    out.push((def_id, e.name.node.clone(), e.name.span));
                }
            }
            Item::Newtype(n) => {
                if let Some(def_id) = scopes.lookup(&n.name.node) {
                    out.push((def_id, n.name.node.clone(), n.name.span));
                }
            }
            Item::Module { items: inner, .. } => {
                out.extend(collect_user_type_defs(inner, scopes));
            }
            _ => {}
        }
    }
    out
}

/// DFS from `start` through by-value field/variant edges. Returns the cycle
/// path (`[A, B, C, A]`) if `start` is reachable from itself; `None` otherwise.
fn find_unbounded_cycle(
    start: DefId,
    on_stack: &mut Vec<(DefId, String)>,
    visited: &mut FxHashSet<DefId>,
    scopes: &ScopeTable,
    types: &TypeTable,
) -> Option<Vec<String>> {
    // Already on the active DFS path → cycle detected. The path from where
    // we first hit `start` back to the current node is the cycle.
    if let Some(idx) = on_stack.iter().position(|(d, _)| *d == start) {
        let mut cycle: Vec<String> =
            on_stack[idx..].iter().map(|(_, n)| n.clone()).collect();
        cycle.push(scopes.get_def(start).name.clone());
        return Some(cycle);
    }
    // Already finished exploring (no cycle reachable from here) — skip.
    if visited.contains(&start) {
        return None;
    }

    on_stack.push((start, scopes.get_def(start).name.clone()));

    let def = scopes.get_def(start);
    let by_value_field_types: Vec<TypeId> = match def.kind {
        DefKind::Struct | DefKind::Newtype => {
            def.field_types.clone().unwrap_or_default()
        }
        DefKind::Enum => def
            .variant_field_types
            .iter()
            .flatten()
            .flat_map(|v| v.iter().copied())
            .collect(),
        _ => Vec::new(),
    };

    let mut found: Option<Vec<String>> = None;
    for field_ty in by_value_field_types {
        if let Some(c) = walk_type_for_cycle(field_ty, on_stack, visited, scopes, types) {
            found = Some(c);
            break;
        }
    }

    on_stack.pop();
    if found.is_none() {
        visited.insert(start);
    }
    found
}

fn walk_type_for_cycle(
    ty: TypeId,
    on_stack: &mut Vec<(DefId, String)>,
    visited: &mut FxHashSet<DefId>,
    scopes: &ScopeTable,
    types: &TypeTable,
) -> Option<Vec<String>> {
    let resolved = types.get(ty);
    match resolved {
        ResolvedType::Defined(def_id) => {
            find_unbounded_cycle(*def_id, on_stack, visited, scopes, types)
        }
        ResolvedType::Generic(def_id, args) => {
            let name = &scopes.get_def(*def_id).name;
            if is_heap_indirect_wrapper(name) {
                // The wrapper stores its args behind a pointer/handle —
                // doesn't propagate the cycle. (The args still exist as
                // independent types and will be visited via their own
                // top-level entry; their internal cycles are caught
                // there if any.)
                return None;
            }
            // Inline-arg generic (Option[T], Result[T, E], user generics):
            // recurse into args AND, if it's a user-defined generic
            // struct/enum, also into its fields/variants.
            let args = args.clone();
            for arg in args {
                if let Some(c) = walk_type_for_cycle(arg, on_stack, visited, scopes, types) {
                    return Some(c);
                }
            }
            if matches!(
                scopes.get_def(*def_id).kind,
                DefKind::Struct | DefKind::Enum | DefKind::Newtype
            ) {
                if let Some(c) = find_unbounded_cycle(*def_id, on_stack, visited, scopes, types) {
                    return Some(c);
                }
            }
            None
        }
        ResolvedType::Tuple(elems) => {
            let elems = elems.clone();
            for e in elems {
                if let Some(c) = walk_type_for_cycle(e, on_stack, visited, scopes, types) {
                    return Some(c);
                }
            }
            None
        }
        ResolvedType::Array(elem, _) => {
            let elem = *elem;
            walk_type_for_cycle(elem, on_stack, visited, scopes, types)
        }
        // Slice, Ref, Owned — borrow / pointer / annotation: by definition
        // they don't store the pointee inline, so they break the cycle.
        // Function/CallableTrait/MutCallableTrait/ConsumeCallableTrait/
        // BoxedCallable — function pointers / closure handles, fixed-size.
        // TraitObject — boxed dyn dispatch, fixed-size.
        // Primitive / Var / Error / Void / Never — no contained user type.
        _ => None,
    }
}
