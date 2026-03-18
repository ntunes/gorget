//! P2.6: Drop & Move Elaboration
//!
//! Inserts explicit `Drop` and `MoveZero` instructions based on type
//! ownership metadata. Tracks drop scopes (function, loop, block) and
//! emits cleanup code at scope exits and early returns.

use crate::ir::builder::FunctionBuilder;
use crate::ir::instructions::Place;
use crate::ir::types::*;

/// Tracks owned locals and emits drop instructions at scope boundaries.
///
/// **Contract with the backend**: This elaborator decides WHEN to drop by
/// emitting `Drop { place }` / `DropIfAlive { place }` instructions.
/// The backend decides HOW to drop by looking up the type's `DropStrategy`
/// from the `TypeRegistry`. See `TypeMetadata` docs for valid combinations.
///
/// Registration rules:
/// - `register_local`: registers Move-type locals + any type with non-None drop
/// - `register_param`: registers Copy-type params with non-None drop (ref-counted)
/// - Move-type params are NOT registered here (handled by body-level mechanisms)
pub struct DropElaborator {
    /// Stack of drop scopes, innermost last.
    scopes: Vec<DropScope>,
}

/// A drop scope corresponds to a language construct that owns locals.
struct DropScope {
    kind: DropScopeKind,
    /// Locals registered in this scope, in declaration order.
    /// Drops are emitted in reverse (LIFO) order.
    entries: Vec<DropEntry>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DropScopeKind {
    Function,
    Loop,
    Block,
}

/// An entry tracking a local that needs dropping at scope exit.
#[derive(Clone)]
struct DropEntry {
    local: LocalId,
    type_id: TypeId,
    /// If true, the value may have been moved — use DropIfAlive instead of Drop.
    maybe_moved: bool,
}

impl DropElaborator {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
        }
    }

    /// Push a new drop scope.
    pub fn push_scope(&mut self, kind: DropScopeKind) {
        self.scopes.push(DropScope {
            kind,
            entries: Vec::new(),
        });
    }

    /// Pop the current drop scope and emit drops for all registered locals.
    pub fn pop_scope(&mut self, builder: &mut FunctionBuilder, registry: &TypeRegistry) {
        if let Some(scope) = self.scopes.pop() {
            emit_scope_drops(builder, registry, &scope.entries);
        }
    }

    /// Pop the current drop scope WITHOUT emitting drops.
    /// Used when drops were already emitted via emit_early_exit_drops (e.g., explicit return).
    pub fn pop_scope_no_emit(&mut self) {
        self.scopes.pop();
    }

    /// Register an owned (Move-type) local in the current scope.
    ///
    /// Only registers locals whose type has Move semantics — Copy types
    /// don't need dropping.
    pub fn register_local(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_drop(type_id, registry) {
            return;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
            });
        }
    }

    /// Register a Move-type local at the outermost (Function) scope, even if the current
    /// scope is a nested Block or Loop. Used for GorgetString temps that back Str views —
    /// they must survive until the function exits, not just the current block scope.
    /// Skips if the local is already registered in any scope (prevents double-registration).
    pub fn register_local_at_function_scope(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_drop(type_id, registry) {
            return;
        }
        // Check if already registered in any scope
        for scope in &self.scopes {
            if scope.entries.iter().any(|e| e.local == local) {
                return;
            }
        }
        // Find the outermost Function scope
        for scope in self.scopes.iter_mut() {
            if scope.kind == DropScopeKind::Function {
                scope.entries.push(DropEntry {
                    local,
                    type_id,
                    maybe_moved: false,
                });
                return;
            }
        }
        // Fallback to current scope if no Function scope exists
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
            });
        }
    }

    /// Register a function parameter for drop at scope exit — **only** for Copy-semantics
    /// types that have a non-None drop strategy (e.g., Channel, Shared, Weak).
    ///
    /// Move-semantics parameters are already tracked via VarDecl/body mechanisms;
    /// registering them again would cause double-free.
    pub fn register_param(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        if !needs_param_drop(type_id, registry) {
            return;
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.entries.push(DropEntry {
                local,
                type_id,
                maybe_moved: false,
            });
        }
    }

    /// Completely remove a local from drop tracking. Used when a GorgetString temp
    /// is consumed by a str view assignment — the view may escape the scope, so the
    /// GorgetString must NOT be freed (it will leak, same as pre-drop-registration).
    pub fn unregister(&mut self, local: LocalId) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(pos) = scope.entries.iter().position(|e| e.local == local) {
                scope.entries.remove(pos);
                return;
            }
        }
    }

    /// Mark a local as "maybe moved" — future drops will use DropIfAlive.
    pub fn mark_moved(&mut self, local: LocalId) {
        for scope in self.scopes.iter_mut().rev() {
            for entry in &mut scope.entries {
                if entry.local == local {
                    entry.maybe_moved = true;
                    return;
                }
            }
        }
    }

    /// Update the recorded type for a local after type re-inference.
    ///
    /// Called in VarDecl when `auto` or closure types are re-inferred after lowering
    /// the RHS: the initial registration used the pre-inference type (often I64_TYPE),
    /// but now we know the real type. If the local wasn't yet registered (because the
    /// original type had no drop semantics), this registers it fresh.
    pub fn update_or_register_type(&mut self, local: LocalId, type_id: TypeId, registry: &TypeRegistry) {
        // Try to find and update an existing entry
        for scope in self.scopes.iter_mut().rev() {
            for entry in &mut scope.entries {
                if entry.local == local {
                    entry.type_id = type_id;
                    return;
                }
            }
        }
        // Not yet registered — register it now (only if the new type needs dropping)
        self.register_local(local, type_id, registry);
    }

    /// Emit cleanup drops for an early exit (return, break, continue).
    ///
    /// Drops all locals from the current scope back to (and excluding)
    /// the target scope kind. For `return`, drops everything up to Function.
    /// For `break`/`continue`, drops up to the enclosing Loop.
    ///
    /// `exclude` optionally skips a local (e.g., the return value being moved out).
    pub fn emit_early_exit_drops(
        &self,
        builder: &mut FunctionBuilder,
        registry: &TypeRegistry,
        target: DropScopeKind,
        exclude: Option<LocalId>,
    ) {
        // Walk scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            emit_scope_drops_excluding(builder, registry, &scope.entries, exclude);
            if scope.kind == target {
                break;
            }
        }
    }

    /// Check if there are any active scopes.
    pub fn has_scopes(&self) -> bool {
        !self.scopes.is_empty()
    }
}

/// Check whether a Copy-semantics type needs dropping when passed as a function parameter.
/// Delegates to `TypeRegistry::needs_param_drop()`.
fn needs_param_drop(type_id: TypeId, registry: &TypeRegistry) -> bool {
    registry.needs_param_drop(type_id)
}

/// Check whether a type needs dropping based on its metadata.
/// Delegates to `TypeRegistry::needs_drop()`.
fn needs_drop(type_id: TypeId, registry: &TypeRegistry) -> bool {
    registry.needs_drop(type_id)
}

/// Emit Drop/DropIfAlive for a list of entries in LIFO order.
fn emit_scope_drops(
    builder: &mut FunctionBuilder,
    registry: &TypeRegistry,
    entries: &[DropEntry],
) {
    emit_scope_drops_excluding(builder, registry, entries, None);
}

/// Emit Drop/DropIfAlive for a list of entries in LIFO order,
/// optionally excluding a specific local (e.g., the return value being moved out).
fn emit_scope_drops_excluding(
    builder: &mut FunctionBuilder,
    registry: &TypeRegistry,
    entries: &[DropEntry],
    exclude: Option<LocalId>,
) {
    for entry in entries.iter().rev() {
        // Skip excluded local (return value being moved out)
        if let Some(excl) = exclude {
            if entry.local == excl {
                continue;
            }
        }
        // Verify the type still needs dropping (defense-in-depth)
        if !needs_drop(entry.type_id, registry) {
            continue;
        }

        let place = Place::local(entry.local);
        if entry.maybe_moved {
            builder.drop_if_alive(place);
        } else {
            builder.drop(place);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::FunctionBuilder;
    use crate::ir::instructions::Instruction;

    fn make_move_registry() -> TypeRegistry {
        let mut reg = TypeRegistry::new();
        // Register a Move type with Trivial drop
        reg.add_type_def(TypeDef {
            name: "OwnedString".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "data".into(), type_id: U8_TYPE },
                    StructField { name: "len".into(), type_id: U64_TYPE },
                ],
            }),
            metadata: TypeMetadata {
                size: Some(16),
                align: Some(8),
                drop_strategy: DropStrategy::Trivial("gorget_string_free".into()),
                copy_semantics: CopySemantics::Resource,
            },
        });
        reg.insert(GirType::Named("OwnedString".into()));
        reg
    }

    #[test]
    fn needs_drop_primitives() {
        let reg = TypeRegistry::new();
        assert!(!needs_drop(I64_TYPE, &reg));
        assert!(!needs_drop(BOOL_TYPE, &reg));
        assert!(!needs_drop(F64_TYPE, &reg));
        assert!(!needs_drop(UNIT_TYPE, &reg));
    }

    #[test]
    fn needs_drop_move_type() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12); // first inserted after primitives
        assert!(needs_drop(owned_string_id, &reg));
    }

    #[test]
    fn drop_elaborator_scope_lifecycle() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // Function scope
        elab.push_scope(DropScopeKind::Function);
        elab.register_local(LocalId(1), I64_TYPE, &reg); // Copy — not registered
        let s_local = builder.add_local(owned_string_id, Some("s"));
        elab.register_local(s_local, owned_string_id, &reg); // Move — registered

        // Pop scope should emit Drop for owned_string
        elab.pop_scope(&mut builder, &reg);

        let block = &builder.blocks[0];
        assert!(
            block.instructions.iter().any(|inst| matches!(inst, Instruction::Drop { .. })),
            "Should emit Drop instruction for Move-type local"
        );
    }

    #[test]
    fn drop_elaborator_maybe_moved() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        let s_local = builder.add_local(owned_string_id, Some("s"));
        elab.register_local(s_local, owned_string_id, &reg);

        // Mark as moved
        elab.mark_moved(s_local);

        // Pop scope should emit DropIfAlive instead of Drop
        elab.pop_scope(&mut builder, &reg);

        let block = &builder.blocks[0];
        assert!(
            block.instructions.iter().any(|inst| matches!(inst, Instruction::DropIfAlive { .. })),
            "Should emit DropIfAlive for maybe-moved local"
        );
        assert!(
            !block.instructions.iter().any(|inst| matches!(inst, Instruction::Drop { .. })),
            "Should NOT emit plain Drop for maybe-moved local"
        );
    }

    #[test]
    fn drop_elaborator_nested_scopes() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        // Function scope with one owned local
        elab.push_scope(DropScopeKind::Function);
        let s1 = builder.add_local(owned_string_id, Some("s1"));
        elab.register_local(s1, owned_string_id, &reg);

        // Inner block scope with another owned local
        elab.push_scope(DropScopeKind::Block);
        let s2 = builder.add_local(owned_string_id, Some("s2"));
        elab.register_local(s2, owned_string_id, &reg);

        // Pop inner scope — should drop s2
        elab.pop_scope(&mut builder, &reg);

        let drop_count_1 = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::Drop { .. }))
            .count();
        assert_eq!(drop_count_1, 1, "Should drop s2 from inner scope");

        // Pop outer scope — should drop s1
        elab.pop_scope(&mut builder, &reg);

        let drop_count_2 = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::Drop { .. }))
            .count();
        assert_eq!(drop_count_2, 2, "Should drop both s1 and s2 total");
    }

    #[test]
    fn drop_elaborator_early_return() {
        let reg = make_move_registry();
        let owned_string_id = TypeId(12);

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        let s1 = builder.add_local(owned_string_id, Some("s1"));
        elab.register_local(s1, owned_string_id, &reg);

        elab.push_scope(DropScopeKind::Block);
        let s2 = builder.add_local(owned_string_id, Some("s2"));
        elab.register_local(s2, owned_string_id, &reg);

        // Early return — should drop s2 (Block) + s1 (Function)
        elab.emit_early_exit_drops(&mut builder, &reg, DropScopeKind::Function, None);

        let drop_count = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::Drop { .. }))
            .count();
        assert_eq!(drop_count, 2, "Early return should drop both s2 and s1");
    }

    #[test]
    fn copy_types_not_registered() {
        let mut reg = TypeRegistry::new();
        // Register a Copy type (no drop needed)
        reg.add_type_def(TypeDef {
            name: "Point".into(),
            kind: TypeDefKind::Struct(StructDef {
                fields: vec![
                    StructField { name: "x".into(), type_id: F64_TYPE },
                    StructField { name: "y".into(), type_id: F64_TYPE },
                ],
            }),
            metadata: TypeMetadata::default(), // Copy, DropStrategy::None
        });
        let point_id = reg.insert(GirType::Named("Point".into()));

        let mut elab = DropElaborator::new();
        let mut builder = FunctionBuilder::new("test", UNIT_TYPE, &[]);

        elab.push_scope(DropScopeKind::Function);
        let p = builder.add_local(point_id, Some("p"));
        elab.register_local(p, point_id, &reg);

        // Pop scope — should NOT emit any drops for Copy type
        elab.pop_scope(&mut builder, &reg);

        let drop_count = builder.blocks[0].instructions.iter()
            .filter(|inst| matches!(inst, Instruction::Drop { .. } | Instruction::DropIfAlive { .. }))
            .count();
        assert_eq!(drop_count, 0, "Should not drop Copy types");
    }
}
